//
//  Copyright (C) 2022  Nick Gasson
//
//  This program is free software: you can redistribute it and/or modify
//  it under the terms of the GNU General Public License as published by
//  the Free Software Foundation, either version 3 of the License, or
//  (at your option) any later version.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.
//
//  You should have received a copy of the GNU General Public License
//  along with this program.  If not, see <http://www.gnu.org/licenses/>.
//

#include "util.h"
#include "array.h"
#include "common.h"
#include "debug.h"
#include "diag.h"
#include "hash.h"
#include "lib.h"
#include "jit/jit-priv.h"
#include "opt.h"
#include "rt/model.h"
#include "rt/mspace.h"
#include "rt/structs.h"
#include "thread.h"
#include "type.h"
#include "vcode.h"

#include <assert.h>
#include <errno.h>
#include <inttypes.h>
#include <setjmp.h>
#include <signal.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>

#ifdef __MINGW32__
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#else
#include <dlfcn.h>
#endif

#ifdef __MINGW32__
typedef HMODULE module_t;
#else
typedef void *module_t;
#endif

typedef A(jit_func_t *) func_array_t;
typedef A(module_t) module_array_t;

typedef struct _jit_tier {
   jit_tier_t    *next;
   int            threshold;
   jit_plugin_t   plugin;
   void          *context;
} jit_tier_t;

typedef struct _jit {
   func_array_t    funcs;
   hash_t         *index;
   mspace_t       *mspace;
   jit_lower_fn_t  lower_fn;
   void           *lower_ctx;
   hash_t         *layouts;
   bool            silent;
   bool            runtime;
   unsigned        backedge;
   module_array_t  modules;
   int             exit_status;
   jit_tier_t     *tiers;
} jit_t;

typedef enum {
   JIT_IDLE,
   JIT_NATIVE,
   JIT_INTERP
} jit_state_t;

typedef struct {
   jit_t                 *jit;
   jit_state_t            state;
   jmp_buf                abort_env;
   volatile sig_atomic_t  jmp_buf_valid;
} jit_thread_local_t;

static void jit_oom_cb(mspace_t *m, size_t size)
{
   diag_t *d = diag_new(DIAG_FATAL, NULL);
   diag_printf(d, "out of memory attempting to allocate %zu byte object", size);

   const int heapsize = opt_get_int(OPT_HEAP_SIZE);
   diag_hint(d, NULL, "the current heap size is %u bytes which you can "
             "increase with the $bold$-H$$ option, for example $bold$-H %um$$",
             heapsize, MAX(1, (heapsize * 2) / 1024 / 1024));

   diag_emit(d);
   jit_abort(EXIT_FAILURE);
}

static jit_thread_local_t *jit_thread_local(void)
{
   static __thread jit_thread_local_t *local = NULL;

   if (local == NULL) {
      local = xcalloc(sizeof(jit_thread_local_t));
      local->state = JIT_IDLE;
   }

   return local;
}

jit_t *jit_new(void)
{
   jit_t *j = xcalloc(sizeof(jit_t));
   j->index = hash_new(256);
   j->mspace = mspace_new(opt_get_int(OPT_HEAP_SIZE));

   mspace_set_oom_handler(j->mspace, jit_oom_cb);

   return j;
}

static void jit_free_func(jit_func_t *f)
{
   jit_free_cfg(f);
   mptr_free(f->jit->mspace, &(f->privdata));
   free(f->irbuf);
   free(f->varoff);
   free(f->cpool);
   free(f);
}

void jit_free(jit_t *j)
{
#ifdef __MINGW32__
   for (int i = 0; i < j->modules.count; i++)
      FreeLibrary(j->modules.items[i]);
#endif
   ACLEAR(j->modules);

   for (int i = 0; i < j->funcs.count; i++)
      jit_free_func(j->funcs.items[i]);
   ACLEAR(j->funcs);

   if (j->layouts != NULL) {
      hash_iter_t it = HASH_BEGIN;
      const void *key;
      void *value;
      while (hash_iter(j->layouts, &it, &key, &value))
         free(value);
      hash_free(j->layouts);
   }

   for (jit_tier_t *it = j->tiers, *tmp; it; it = tmp) {
      tmp = it->next;
      (*it->plugin.cleanup)(it->context);
      free(it);
   }

   mspace_destroy(j->mspace);
   hash_free(j->index);
   free(j);
}

mspace_t *jit_get_mspace(jit_t *j)
{
   return j->mspace;
}

void *jit_find_symbol(jit_t *j, ident_t name)
{
   LOCAL_TEXT_BUF tb = safe_symbol(name);
   const char *symbol_name = tb_get(tb);

   for (size_t i = 0; i < j->modules.count; i++) {
      module_t handle = j->modules.items[i];
#ifdef __MINGW32__
      void *ptr = (void *)(uintptr_t)GetProcAddress(handle, symbol_name);
#else
      void *ptr = dlsym(handle, symbol_name);
#endif
      if (ptr != NULL)
         return ptr;
   }

   return NULL;
}

jit_handle_t jit_lazy_compile(jit_t *j, ident_t name)
{
   jit_func_t *f = hash_get(j->index, name);
   if (f != NULL)
      return f->handle;

   void *symbol = jit_find_symbol(j, name);

   vcode_unit_t vu = vcode_find_unit(name);

   if (vu == NULL && symbol == NULL) {
      if (opt_get_verbose(OPT_JIT_VERBOSE, NULL))
         debugf("loading vcode for %s", istr(name));

      tree_t unit = lib_get_qualified(name);
      if (unit != NULL && tree_kind(unit) == T_PACKAGE)
         (void)body_of(unit);  // Make sure body is loaded

      vu = vcode_find_unit(name);
   }

   ident_t alias = NULL;
   if (vu == NULL && j->lower_fn != NULL) {
      vcode_state_t state;
      vcode_state_save(&state);

      if ((vu = (*j->lower_fn)(name, j->lower_ctx))) {
         vcode_select_unit(vu);
         alias = vcode_unit_name();   // May have $thunk appended
      }

      vcode_state_restore(&state);
   }

   if (vu == NULL && symbol == NULL)
      return JIT_HANDLE_INVALID;

   assert(vu == NULL || hash_get(j->index, vu) == NULL);

   f = xcalloc(sizeof(jit_func_t));

   f->name      = alias ?: name;
   f->unit      = vu;
   f->symbol    = symbol;
   f->jit       = j;
   f->handle    = j->funcs.count;
   f->next_tier = j->tiers;
   f->hotness   = f->next_tier ? f->next_tier->threshold : 0;
   f->entry     = jit_interp;

   if (vu) hash_put(j->index, vu, f);
   hash_put(j->index, name, f);

   if (alias != NULL && alias != name)
      hash_put(j->index, alias, f);

   APUSH(j->funcs, f);
   return f->handle;
}

jit_func_t *jit_get_func(jit_t *j, jit_handle_t handle)
{
   return AGET(j->funcs, handle);
}

jit_handle_t jit_compile(jit_t *j, ident_t name)
{
   jit_handle_t handle = jit_lazy_compile(j, name);
   if (handle == JIT_HANDLE_INVALID)
      return handle;

   jit_func_t *f = jit_get_func(j, handle);
   if (f->irbuf == NULL && f->symbol == NULL)
      jit_irgen(f);

   return handle;
}

void *jit_link(jit_t *j, jit_handle_t handle)
{
   if (handle == JIT_HANDLE_INVALID)
      return NULL;

   jit_func_t *f = jit_get_func(j, handle);
   if (f->privdata != MPTR_INVALID)
      return mptr_get(j->mspace, f->privdata);

   f->privdata = mptr_new(j->mspace, "privdata");

   vcode_state_t state;
   vcode_state_save(&state);

   vcode_select_unit(f->unit);
   const vunit_kind_t kind = vcode_unit_kind();
   if (kind != VCODE_UNIT_PACKAGE && kind != VCODE_UNIT_INSTANCE)
      fatal_trace("cannot link unit %s", istr(f->name));

   const loc_t *loc = vcode_unit_loc();

   jit_scalar_t args[JIT_MAX_ARGS] = { { .pointer = NULL } };
   if (!jit_interp(f, args)) {
      error_at(loc, "failed to initialise %s", istr(f->name));
      args[0].pointer = NULL;
   }
   else if (args[0].pointer == NULL)
      fatal_trace("link %s returned NULL", istr(f->name));

   vcode_state_restore(&state);

   // Initialisation should save the context pointer
   assert(args[0].pointer == mptr_get(j->mspace, f->privdata));

   return args[0].pointer;
}

void *jit_get_privdata(jit_t *j, jit_func_t *f)
{
   if (f->privdata == MPTR_INVALID)
      f->privdata = mptr_new(j->mspace, "privdata");

   return mptr_get(j->mspace, f->privdata);
}

void jit_put_privdata(jit_t *j, jit_func_t *f, void *ptr)
{
   assert(f->privdata != MPTR_INVALID);
   mptr_put(j->mspace, f->privdata, ptr);
}

void *jit_get_frame_var(jit_t *j, jit_handle_t handle, uint32_t var)
{
   jit_func_t *f = jit_get_func(j, handle);
   if (f->privdata == MPTR_INVALID)
      fatal_trace("%s not linked", istr(f->name));

   assert(var < f->nvars);
   return (char *)mptr_get(j->mspace, f->privdata) + f->varoff[var];
}

static void jit_native_trace(diag_t *d)
{
   assert(jit_thread_local()->state == JIT_NATIVE);

   debug_info_t *di = debug_capture();

   const int nframes = debug_count_frames(di);
   for (int i = 0; i < nframes; i++) {
      const debug_frame_t *f = debug_get_frame(di, i);
      if (f->kind != FRAME_VHDL || f->vhdl_unit == NULL || f->symbol == NULL)
         continue;

      for (debug_inline_t *inl = f->inlined; inl != NULL; inl = inl->next) {
         tree_t enclosing = find_enclosing_decl(inl->vhdl_unit, inl->symbol);
         if (enclosing == NULL)
            continue;

         // Processes should never be inlined
         assert(tree_kind(enclosing) != T_PROCESS);

         loc_file_ref_t file_ref = loc_file_ref(inl->srcfile, NULL);
         loc_t loc = get_loc(inl->lineno, inl->colno, inl->lineno,
                             inl->colno, file_ref);

         jit_emit_trace(d, &loc, enclosing, inl->symbol);
      }

      tree_t enclosing = find_enclosing_decl(f->vhdl_unit, f->symbol);
      if (enclosing == NULL)
         continue;

      loc_t loc;
      if (f->lineno == 0) {
         // Exact DWARF debug info not available
         loc = *tree_loc(enclosing);
      }
      else {
         loc_file_ref_t file_ref = loc_file_ref(f->srcfile, NULL);
         loc = get_loc(f->lineno, f->colno, f->lineno, f->colno, file_ref);
      }

      jit_emit_trace(d, &loc, enclosing, f->symbol);
   }

   debug_free(di);
}

static void jit_diag_cb(diag_t *d, void *arg)
{
   jit_t *j = arg;

   if (j->silent) {
      diag_suppress(d, true);
      return;
   }

   switch (jit_thread_local()->state) {
   case JIT_NATIVE:
      jit_native_trace(d);
      break;
   case JIT_INTERP:
      jit_interp_trace(d);
      break;
   case JIT_IDLE:
      fatal_trace("JIT diag callback called when idle");
   }
}

static void jit_transition(jit_t *j, jit_state_t from, jit_state_t to)
{
   jit_thread_local_t *thread = jit_thread_local();

#ifdef DEBUG
   if (thread->state != from)
      fatal_trace("expected thread state %d but have %d", from, thread->state);
#endif

   thread->state = to;

   switch (to) {
   case JIT_NATIVE:
   case JIT_INTERP:
      diag_add_hint_fn(jit_diag_cb, j);
      thread->jit = j;
      break;
   case JIT_IDLE:
      diag_remove_hint_fn(jit_diag_cb);
      thread->jit = NULL;
      break;
   }
}

static bool jit_try_vcall(jit_t *j, ident_t func, bool pcall, void *state,
                          void *context, jit_scalar_t *result,
                          const char *fmt, va_list ap)
{
   jit_scalar_t args[JIT_MAX_ARGS];

   int nargs = 0;
   if (pcall) args[nargs++].pointer = state;
   args[nargs++].pointer = context;

   for (; *fmt; fmt++) {
      if (nargs == JIT_MAX_ARGS)
         fatal_trace("too many arguments to function %s", istr(func));

      switch (*fmt) {
      case 'I':
         args[nargs++].integer = va_arg(ap, int64_t);
         break;
      case 'i':
         args[nargs++].integer = va_arg(ap, int32_t);
         break;
      case 'R':
         args[nargs++].real = va_arg(ap, double);
         break;
      case 'u':
         args[nargs++].pointer = va_arg(ap, void *);
         args[nargs++].integer = va_arg(ap, int32_t);  // Left
         args[nargs++].integer = va_arg(ap, int32_t);  // Length
         break;
      case 'p':
         args[nargs++].integer = (intptr_t)va_arg(ap, void *);
         break;
      default:
         fatal_trace("invalid character '%c' in jit_call format", *fmt);
      }
   }

   jit_handle_t handle = jit_compile(j, func);
   if (handle == JIT_HANDLE_INVALID)
      fatal_trace("invalid handle for %s", istr(func));

   jit_transition(j, JIT_IDLE, JIT_INTERP);

   jit_func_t *f = jit_get_func(j, handle);
   bool ok = (*f->entry)(f, args);

   jit_transition(j, JIT_INTERP, JIT_IDLE);

   *result = args[0];
   return ok;
}

bool jit_try_call(jit_t *j, ident_t func, void *context, jit_scalar_t *result,
                  const char *fmt, ...)
{
   va_list ap;
   va_start(ap, fmt);
   bool ok = jit_try_vcall(j, func, false, NULL, context, result, fmt, ap);
   va_end(ap);
   return ok;
}

bool jit_try_pcall(jit_t *j, ident_t func, void *state, void *context,
                   const char *fmt, ...)
{
   va_list ap;
   va_start(ap, fmt);
   jit_scalar_t result;
   bool ok = jit_try_vcall(j, func, true, state, context, &result, fmt, ap);
   va_end(ap);
   return ok;
}

jit_scalar_t jit_call(jit_t *j, ident_t func, void *context,
                      const char *fmt, ...)
{
   va_list ap;
   va_start(ap, fmt);

   jit_scalar_t result;
   if (!jit_try_vcall(j, func, false, NULL, context, &result, fmt, ap))
      fatal_trace("call to %s failed", istr(func));

   va_end(ap);
   return result;
}

jit_scalar_t jit_pcall(jit_t *j, ident_t func, void *state, void *context,
                       const char *fmt, ...)
{
   va_list ap;
   va_start(ap, fmt);

   jit_scalar_t result;
   if (!jit_try_vcall(j, func, true, state, context, &result, fmt, ap))
      fatal_trace("call to %s failed", istr(func));

   va_end(ap);
   return result;
}

bool jit_call_thunk(jit_t *j, vcode_unit_t unit, jit_scalar_t *result)
{
   vcode_select_unit(unit);
   assert(vcode_unit_kind() == VCODE_UNIT_THUNK);

   jit_func_t *f = xcalloc(sizeof(jit_func_t));
   f->unit   = unit;
   f->jit    = j;
   f->handle = JIT_HANDLE_INVALID;
   f->entry  = jit_interp;

   jit_irgen(f);

   jit_transition(j, JIT_IDLE, JIT_INTERP);

   jit_scalar_t args[JIT_MAX_ARGS];
   bool ok = jit_interp(f, args);

   jit_transition(j, JIT_INTERP, JIT_IDLE);

   jit_free_func(f);

   *result = args[0];
   return ok;
}

bool jit_fastcall(jit_t *j, jit_handle_t handle, jit_scalar_t *result,
                  jit_scalar_t p1, jit_scalar_t p2)
{
   // TODO: this interface and jit_call should be rethought

   jit_func_t *f = jit_get_func(j, handle);
   jit_thread_local_t *thread = jit_thread_local();

   if (f->symbol) {
      assert(!thread->jmp_buf_valid);
      const int rc = setjmp(thread->abort_env);
      if (rc == 0) {
         thread->jmp_buf_valid = 1;
         jit_transition(j, JIT_IDLE, JIT_NATIVE);
         void *(*fn)(void *, void *) = f->symbol;
         result->pointer = (*fn)(p1.pointer, p2.pointer);
         jit_transition(j, JIT_NATIVE, JIT_IDLE);
         thread->jmp_buf_valid = 0;
         return true;
      }
      else {
         jit_transition(j, JIT_NATIVE, JIT_IDLE);
         thread->jmp_buf_valid = 0;
         jit_set_exit_status(j, rc - 1);
         return false;
      }
   }
   else {
      jit_transition(j, JIT_IDLE, JIT_INTERP);
      jit_scalar_t args[JIT_MAX_ARGS] = { p1, p2 };
      bool ok = jit_interp(f, args);
      *result = args[0];
      jit_transition(j, JIT_INTERP, JIT_IDLE);
      return ok;
   }
}

static bool ffi_is_integral(ffi_type_t type)
{
   return type == FFI_INT8 || type == FFI_INT16 || type == FFI_INT32
      || type == FFI_INT64;
}

static int64_t ffi_widen_int(ffi_type_t type, const void *input, size_t insz)
{
   switch (type) {
   case FFI_INT8: return *((int8_t *)input);
   case FFI_INT16: return *((int16_t *)input);
   case FFI_INT32: return *((int32_t *)input);
   case FFI_INT64: return *((int32_t *)input);
   default:
      fatal_trace("invalid integer type in ffi_widen_int");
   }
}

static void ffi_store_int(ffi_type_t type, uint64_t value,
                          void *output, size_t outsz)
{
   assert(outsz <= sizeof(int64_t));
   switch (type) {
   case FFI_INT8: *(uint8_t *)output = (uint8_t)value; break;
   case FFI_INT16: *(uint16_t *)output = (uint16_t)value; break;
   case FFI_INT32: *(uint32_t *)output = (uint32_t)value; break;
   case FFI_INT64: *(uint64_t *)output = value; break;
   default:
      fatal_trace("invalid integer type in ffi_store_int");
   }
}

static void jit_ffi_wrapper(jit_func_t *f, ffi_closure_t *c, const void *input,
                            size_t insz, void *output, size_t outsz)
{
   if (f->symbol)
      ffi_call(c, f->symbol, input, insz, output, outsz);
   else {
      jit_scalar_t args[JIT_MAX_ARGS];
      args[0].pointer = c->context;

      if (ffi_is_integral(c->spec.atype)) {
         args[1].integer = ffi_widen_int(c->spec.atype, input, insz);
         (*f->entry)(f, args);

         if (ffi_is_integral(c->spec.rtype))
            ffi_store_int(c->spec.rtype, args[0].integer, output, outsz);
         else if (c->spec.rtype == FFI_FLOAT) {
            assert(outsz == sizeof(double));
            *(double *)output = args[0].real;
         }
         else if (c->spec.rtype == FFI_POINTER) {
            memcpy(output, args[0].pointer, outsz);
         }
         else
            fatal_trace("unhandled FFI function argument combination");
      }
      else if (c->spec.atype == FFI_FLOAT && ffi_is_integral(c->spec.rtype)) {
         assert(insz == sizeof(double));
         args[1].real = *(double *)input;
         (*f->entry)(f, args);
         ffi_store_int(c->spec.rtype, args[0].integer, output, outsz);
      }
      else if (c->spec.atype == FFI_FLOAT && c->spec.rtype == FFI_FLOAT) {
         assert(insz == sizeof(double));
         assert(outsz == sizeof(double));
         args[1].real = *(double *)input;
         (*f->entry)(f, args);
         *(double *)output = args[0].real;
      }
      else if (c->spec.atype == FFI_POINTER && ffi_is_integral(c->spec.rtype)) {
         args[1].pointer = (void *)input;
         (*f->entry)(f, args);
         ffi_store_int(c->spec.rtype, args[0].integer, output, outsz);
      }
      else if (c->spec.atype == FFI_VOID && ffi_is_integral(c->spec.rtype)) {
         (*f->entry)(f, args);
         ffi_store_int(c->spec.rtype, args[0].integer, output, outsz);
      }
      else if (c->spec.atype == FFI_UARRAY && ffi_is_integral(c->spec.rtype)) {
         assert(insz == sizeof(ffi_uarray_t));
         const ffi_uarray_t *u = input;
         args[1].pointer = u->ptr;
         args[2].integer = u->dims[0].left;
         args[3].integer = u->dims[0].length;
         (*f->entry)(f, args);
         ffi_store_int(c->spec.rtype, args[0].integer, output, outsz);
      }
      else if (c->spec.atype == FFI_UARRAY && c->spec.rtype == FFI_FLOAT) {
         assert(insz == sizeof(ffi_uarray_t));
         const ffi_uarray_t *u = input;
         args[1].pointer = u->ptr;
         args[2].integer = u->dims[0].left;
         args[3].integer = u->dims[0].length;
         (*f->entry)(f, args);
         *(double *)output = args[0].real;
      }
      else if (c->spec.atype == FFI_UARRAY && c->spec.rtype == FFI_POINTER) {
         assert(insz == sizeof(ffi_uarray_t));
         assert(outsz == sizeof(void *));
         const ffi_uarray_t *u = input;
         args[1].pointer = u->ptr;
         args[2].integer = u->dims[0].left;
         args[3].integer = u->dims[0].length;
         (*f->entry)(f, args);
         *(void **)output = args[0].pointer;
      }
      else
         fatal_trace("unhandled FFI function argument combination");
   }
}

void jit_ffi_call(jit_t *j, ffi_closure_t *c, const void *input, size_t insz,
                  void *output, size_t outsz, bool catch)
{
   // TODO: temporary wrapper around legacy ffi_call

   jit_thread_local_t *thread = jit_thread_local();
   jit_func_t *f = jit_get_func(j, c->handle);

   if (thread->state == JIT_IDLE) {
      assert(!thread->jmp_buf_valid);
      const int rc = setjmp(thread->abort_env);
      if (rc == 0) {
         thread->jmp_buf_valid = 1;
         jit_transition(j, JIT_IDLE, JIT_NATIVE);
         jit_ffi_wrapper(f, c, input, insz, output, outsz);
      }
      else
         jit_set_exit_status(j, rc - 1);

      jit_transition(j, JIT_NATIVE, JIT_IDLE);
      thread->jmp_buf_valid = 0;
   }
   else if (catch) {
      const jit_state_t oldstate = thread->state;
      const int rc = setjmp(thread->abort_env);
      if (rc == 0) {
         thread->jmp_buf_valid = 1;
         thread->state = JIT_NATIVE;
         jit_ffi_wrapper(f, c, input, insz, output, outsz);
      }
      else
         jit_set_exit_status(j, rc - 1);

      thread->state = oldstate;
      thread->jmp_buf_valid = 0;
   }
   else
      jit_ffi_wrapper(f, c, input, insz, output, outsz);
}

void jit_set_lower_fn(jit_t *j, jit_lower_fn_t fn, void *ctx)
{
   j->lower_fn = fn;
   j->lower_ctx = ctx;
}

const jit_layout_t *jit_layout(jit_t *j, type_t type)
{
   if (j->layouts == NULL)
      j->layouts = hash_new(256);

   jit_layout_t *l = hash_get(j->layouts, type);
   if (l != NULL)
      return l;

   if (type_is_integer(type) || type_is_physical(type) || type_is_enum(type)) {
      l = xcalloc_flex(sizeof(jit_layout_t), 1, sizeof(layout_part_t));
      l->nparts = 1;

      tree_t r = type_dim(type, 0);
      int64_t low, high;
      if (!folded_bounds(r, &low, &high))
         fatal_trace("type %s has unknown bounds", type_pp(type));

      const int bits = bits_for_range(low, high);
      l->parts[0].offset = 0;
      l->parts[0].size   = l->size = ALIGN_UP(bits, 8) / 8;
      l->parts[0].repeat = 1;
      l->parts[0].align  = l->align = l->parts[0].size;
   }
   else if (type_is_real(type)) {
      l = xcalloc_flex(sizeof(jit_layout_t), 1, sizeof(layout_part_t));
      l->nparts = 1;

      l->parts[0].offset = 0;
      l->parts[0].size   = l->size = sizeof(double);
      l->parts[0].repeat = 1;
      l->parts[0].align  = l->align = l->parts[0].size;
   }
   else if (type_is_array(type)) {
      const int ndims = dimension_of(type);

      if (type_is_unconstrained(type)) {
         l = xcalloc_flex(sizeof(jit_layout_t), 2, sizeof(layout_part_t));
         l->nparts = 2;
         l->size   = sizeof(void *) + ndims * 2 * sizeof(int32_t);
         l->align  = sizeof(void *);

         l->parts[0].offset = 0;
         l->parts[0].size   = sizeof(void *);
         l->parts[0].repeat = 1;
         l->parts[0].align  = l->parts[0].size;

         l->parts[1].offset = sizeof(void *);
         l->parts[1].size   = sizeof(int32_t);
         l->parts[1].repeat = ndims * 2;
         l->parts[1].align  = l->parts[1].size;
      }
      else {
         int length = 1;
         for (int i = 0; i < ndims; i++) {
            tree_t r = range_of(type, i);

            int64_t dlen;
            if (!folded_length(r, &dlen))
               fatal_at(tree_loc(r), "dimension %d of type %s is not static",
                        i, type_pp(type));

            length *= dlen;
         }

         const jit_layout_t *el = jit_layout(j, type_elem(type));
         assert(!type_is_array(type_elem(type)));

         l = xcalloc_flex(sizeof(jit_layout_t), 1, sizeof(layout_part_t));
         l->nparts = 1;
         l->size   = length * el->size;
         l->align  = el->align;

         l->parts[0].offset = 0;
         l->parts[0].size   = el->size;
         l->parts[0].repeat = length;
         l->parts[0].align  = el->align;
      }
   }
   else if (type_is_record(type)) {
      const int nfields = type_fields(type);

      l = xcalloc_flex(sizeof(jit_layout_t), nfields, sizeof(layout_part_t));
      l->nparts = nfields;

      int offset = 0;
      for (int i = 0; i < nfields; i++) {
         type_t ftype = tree_type(type_field(type, i));
         const jit_layout_t *fl = jit_layout(j, ftype);

         offset = ALIGN_UP(offset, fl->align);

         l->parts[i].offset = offset;
         l->parts[i].size   = fl->size;
         l->parts[i].repeat = 1;
         l->parts[i].align  = fl->align;

         offset += fl->size;
      }

      l->size  = offset;
      l->align = sizeof(void *);  // Matches irgen_align_of
   }
   else
      fatal_trace("cannot get layout for %s", type_pp(type));

   hash_put(j->layouts, type, l);
   return l;
}

void jit_limit_backedges(jit_t *j, int limit)
{
   j->backedge = limit;
}

void jit_set_silent(jit_t *j, bool silent)
{
   j->silent = silent;
}

void jit_enable_runtime(jit_t *j, bool enable)
{
   j->runtime = enable;
}

bool jit_has_runtime(jit_t *j)
{
   return j->runtime;
}

int jit_backedge_limit(jit_t *j)
{
   return j->backedge;
}

void jit_load_dll(jit_t *j, ident_t name)
{
   lib_t lib = lib_require(ident_until(name, '.'));

   LOCAL_TEXT_BUF tb = tb_new();
   tb_printf(tb, "_%s", istr(name));
   if (opt_get_int(OPT_NO_SAVE))
      tb_printf(tb, ".%d", getpid());
   tb_cat(tb, "." DLL_EXT);

   char so_path[PATH_MAX];
   lib_realpath(lib, tb_get(tb), so_path, sizeof(so_path));

   if (access(so_path, F_OK) != 0)
      return;

   if (opt_get_verbose(OPT_JIT_VERBOSE, NULL))
      debugf("loading shared library %s", so_path);

   uint32_t abi_version = 0;

   // Loading the DLL can execute constructors
   jit_transition(j, JIT_IDLE, JIT_NATIVE);

#ifdef __MINGW32__
   HMODULE handle = LoadLibrary(so_path);
   if (handle == NULL)
      fatal("failed to load %s", so_path);

   FARPROC p = GetProcAddress(handle, "__nvc_abi_version");
   if (p == NULL)
      warnf("%s: cannot find symbol __nvc_abi_version", so_path);
   else
      abi_version = *(uint32_t *)(uintptr_t)p;
#else
   void *handle = dlopen(so_path, RTLD_LAZY);
   if (handle == NULL)
      fatal("%s", dlerror());

   uint32_t *p = dlsym(handle, "__nvc_abi_version");
   if (p == NULL)
      warnf("%s", dlerror());
   else
      abi_version = *p;
#endif

   jit_transition(j, JIT_NATIVE, JIT_IDLE);

   if (abi_version != RT_ABI_VERSION)
      fatal("%s: ABI version %d does not match current version %d",
            so_path, abi_version, RT_ABI_VERSION);

   APUSH(j->modules, handle);
}

void jit_emit_trace(diag_t *d, const loc_t *loc, tree_t enclosing,
                    const char *symbol)
{
   switch (tree_kind(enclosing)) {
   case T_PROCESS:
      {
         rt_proc_t *proc = get_active_proc();
         const char *name = istr(proc ? proc->name : tree_ident(enclosing));
         diag_trace(d, loc, "Process$$ %s", name);
      }
      break;
   case T_FUNC_BODY:
   case T_FUNC_DECL:
      diag_trace(d, loc, "Function$$ %s", type_pp(tree_type(enclosing)));
      break;
   case T_PROC_BODY:
   case T_PROC_DECL:
      diag_trace(d, loc, "Procedure$$ %s", type_pp(tree_type(enclosing)));
      break;
   case T_TYPE_DECL:
      if (strstr(symbol, "$value"))
         diag_trace(d, loc, "Attribute$$ %s'VALUE",
                    istr(tree_ident(enclosing)));
      else
         diag_trace(d, loc, "Type$$ %s", istr(tree_ident(enclosing)));
      break;
   case T_BLOCK:
      diag_trace(d, loc, "Process$$ (init)");
      break;
   default:
      diag_trace(d, loc, "$$%s", istr(tree_ident(enclosing)));
      break;
   }
}

__attribute__((format(printf, 3, 4)))
void jit_msg(const loc_t *where, diag_level_t level, const char *fmt, ...)
{
   diag_t *d = diag_new(level, where);

   va_list ap;
   va_start(ap, fmt);
   diag_vprintf(d, fmt, ap);
   va_end(ap);

   diag_emit(d);

   if (level == DIAG_FATAL)
      jit_abort(EXIT_FAILURE);
}

void jit_abort(int code)
{
   jit_thread_local_t *thread = jit_thread_local();

   switch (thread->state) {
   case JIT_IDLE:
      fatal_exit(code);
      break;
   case JIT_NATIVE:
      assert(code >= 0);
      if (thread->jmp_buf_valid)
         longjmp(thread->abort_env, code + 1);
      else
         fatal_exit(code);
      break;
   case JIT_INTERP:
      jit_interp_abort(code);
      break;
   }
}

void jit_set_exit_status(jit_t *j, int code)
{
   // Only allow one thread to set exit status
   atomic_cas(&(j->exit_status), 0, code);
}

void jit_reset_exit_status(jit_t *j)
{
   atomic_store(&(j->exit_status), 0);
}

int jit_exit_status(jit_t *j)
{
   return j->exit_status;
}

void jit_tier_up(jit_func_t *f)
{
   assert(f->hotness <= 0);
   assert(f->next_tier != NULL);

   (*f->next_tier->plugin.cgen)(f->jit, f->handle, f->next_tier->context);

   f->hotness   = 0;
   f->next_tier = NULL;
}

void jit_add_tier(jit_t *j, int threshold, const jit_plugin_t *plugin)
{
   jit_tier_t *t = xcalloc(sizeof(jit_tier_t));
   t->next      = j->tiers;
   t->threshold = threshold;
   t->plugin    = *plugin;
   t->context   = (*plugin->init)();

   j->tiers = t;
}

jit_t *jit_for_thread(void)
{
   jit_thread_local_t *thread = jit_thread_local();
   assert(thread->jit != NULL);
   return thread->jit;
}

ident_t jit_get_name(jit_t *j, jit_handle_t handle)
{
   return jit_get_func(j, handle)->name;
}
