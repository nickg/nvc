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
#include "jit/jit-priv.h"
#include "jit/jit.h"
#include "lib.h"
#include "object.h"
#include "opt.h"
#include "rt/model.h"
#include "rt/mspace.h"
#include "rt/structs.h"
#include "thread.h"
#include "tree.h"
#include "type.h"
#include "vcode.h"

#include <assert.h>
#include <ctype.h>
#include <errno.h>
#include <inttypes.h>
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

#define FUNC_HASH_SZ  1024
#define FUNC_LIST_SZ  512

typedef struct _jit_tier {
   jit_tier_t    *next;
   int            threshold;
   jit_plugin_t   plugin;
   void          *context;
} jit_tier_t;

typedef struct {
   size_t      length;
   jit_func_t *items[0];
} func_array_t;

typedef struct _jit {
   chash_t        *index;
   mspace_t       *mspace;
   jit_lower_fn_t  lower_fn;
   void           *lower_ctx;
   hash_t         *layouts;
   bool            silent;
   bool            runtime;
   unsigned        backedge;
   int             exit_status;
   jit_tier_t     *tiers;
   jit_dll_t      *aotlib;
   func_array_t   *funcs;
   unsigned        next_handle;
   nvc_lock_t      lock;
} jit_t;

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

jit_thread_local_t *jit_thread_local(void)
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
   j->index = chash_new(FUNC_HASH_SZ);
   j->mspace = mspace_new(opt_get_int(OPT_HEAP_SIZE));

   j->funcs = xcalloc_flex(sizeof(func_array_t),
                           FUNC_LIST_SZ, sizeof(jit_func_t *));
   j->funcs->length = FUNC_LIST_SZ;

   mspace_set_oom_handler(j->mspace, jit_oom_cb);

   // Ensure we can resolve symbols from the executable
   ffi_load_dll(NULL);

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
   if (j->aotlib != NULL)
      ffi_unload_dll(j->aotlib);

   for (int i = 0; i < j->next_handle; i++)
      jit_free_func(j->funcs->items[i]);
   free(j->funcs);

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
   chash_free(j->index);
   free(j);
}

mspace_t *jit_get_mspace(jit_t *j)
{
   return j->mspace;
}

static void jit_install(jit_t *j, jit_func_t *f)
{
   assert_lock_held(&(j->lock));

   if (f->handle >= j->funcs->length) {
      const size_t newlen = MAX(f->handle + 1, j->funcs->length * 2);
      func_array_t *new = xcalloc_flex(sizeof(func_array_t),
                                       newlen, sizeof(jit_func_t *));
      new->length = newlen;
      for (size_t i = 0; i < j->funcs->length; i++)
         new->items[i] = j->funcs->items[i];

      // Synchronises with load_acquire in jit_get_func
      store_release(&(j->funcs), new);

      // XXX: the old list leaks here: it should be cleaned up when
      // no more threads can reference it
   }

   // Synchronises with load_acquire in jit_get_func
   store_release(&(j->funcs->items[f->handle]), f);

   chash_put(j->index, f->name, f);
   if (f->unit) chash_put(j->index, f->unit, f);
}

jit_handle_t jit_lazy_compile(jit_t *j, ident_t name)
{
   jit_func_t *f = chash_get(j->index, name);
   if (f != NULL)
      return f->handle;

   SCOPED_LOCK(j->lock);

   // Second check with lock held
   if ((f = chash_get(j->index, name)))
      return f->handle;

   void *symbol = NULL;
   if (j->aotlib != NULL) {
      LOCAL_TEXT_BUF tb = safe_symbol(name);
      symbol = ffi_find_symbol(j->aotlib, tb_get(tb));
   }

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

   assert(vu == NULL || chash_get(j->index, vu) == NULL);

   f = xcalloc(sizeof(jit_func_t));

   f->name      = alias ?: name;
   f->state     = symbol ? JIT_FUNC_READY : JIT_FUNC_PLACEHOLDER;
   f->unit      = vu;
   f->symbol    = symbol;
   f->jit       = j;
   f->handle    = j->next_handle++;
   f->next_tier = j->tiers;
   f->hotness   = f->next_tier ? f->next_tier->threshold : 0;
   f->entry     = jit_interp;
   f->object    = vu ? vcode_unit_object(vu) : NULL;

   jit_install(j, f);

   if (alias != NULL && alias != name)
      chash_put(j->index, name, f);

   return f->handle;
}

jit_func_t *jit_get_func(jit_t *j, jit_handle_t handle)
{
   assert(handle != JIT_HANDLE_INVALID);

   // Synchronises with store_release in jit_install
   func_array_t *list = load_acquire(&(j->funcs));
   assert(handle < list->length);

   // Synchronises with store_release in jit_install
   jit_func_t *f = load_acquire(&(list->items[handle]));
   if (f == NULL) printf("get func %d is null\n", handle);
   assert(f != NULL);
   return f;
}

jit_handle_t jit_compile(jit_t *j, ident_t name)
{
   jit_handle_t handle = jit_lazy_compile(j, name);
   if (handle == JIT_HANDLE_INVALID)
      return handle;

   jit_func_t *f = jit_get_func(j, handle);
   if (load_acquire(&(f->state)) != JIT_FUNC_READY)
      jit_irgen(f);

   return handle;
}

void jit_register(jit_t *j, ident_t name, jit_entry_fn_t fn,
                  const uint8_t *debug, size_t bufsz, object_t *obj,
                  ffi_spec_t spec)
{
   jit_func_t *f = chash_get(j->index, name);
   if (f != NULL)
      fatal_trace("attempt to register existing function %s", istr(name));

   SCOPED_LOCK(j->lock);

   f = xcalloc(sizeof(jit_func_t));

   f->name      = name;
   f->state     = JIT_FUNC_READY;
   f->unit      = vcode_find_unit(f->name);
   f->jit       = j;
   f->handle    = j->next_handle++;
   f->next_tier = j->tiers;
   f->hotness   = f->next_tier ? f->next_tier->threshold : 0;
   f->entry     = fn;
   f->irbuf     = xcalloc_array(bufsz, sizeof(jit_ir_t));
   f->nirs      = bufsz;
   f->object    = obj;
   f->spec      = spec;

   for (int i = 0; i < bufsz; i++) {
      jit_ir_t *ir = &(f->irbuf[i]);
      ir->op        = J_TRAP;
      ir->size      = JIT_SZ_UNSPEC;
      ir->result    = JIT_REG_INVALID;
      ir->arg1.kind = JIT_VALUE_INVALID;
      ir->arg2.kind = JIT_VALUE_INVALID;
   }

   char *file LOCAL = NULL;
   loc_file_ref_t file_ref = FILE_INVALID;
   int pos = 0, lineno;
   for (const uint8_t *cmd = debug; *cmd >> 4 != DC_STOP; cmd++) {
      jit_ir_t *ir = &(f->irbuf[pos]);

      switch (*cmd >> 4) {
      case DC_TRAP:
         pos += *cmd & 0xf;
         break;
      case DC_LONG_TRAP:
         pos += *(cmd + 1) | *(cmd + 2) << 8;
         cmd += 2;
         break;
      case DC_TARGET:
         ir->target = 1;
         break;
      case DC_LOCINFO:
         lineno = *cmd & 0xf;
         ir->op = J_DEBUG;
         ir->arg1.kind = JIT_VALUE_LOC;
         ir->arg1.loc = get_loc(lineno, 0, lineno, 0, file_ref);
         pos++;
         break;
      case DC_LONG_LOCINFO:
         lineno = *(cmd + 1) | *(cmd + 2) << 8;
         ir->op = J_DEBUG;
         ir->arg1.kind = JIT_VALUE_LOC;
         ir->arg1.loc = get_loc(lineno, 0, lineno, 0, file_ref);
         pos++;
         cmd += 2;
         break;
      case DC_FILE:
         {
            char *p = file = xmalloc(1 << (*cmd & 0xf));
            do { *p++ = *++cmd; } while (*cmd);
            file_ref = loc_file_ref(file, NULL);
         }
         break;
      default:
         fatal_trace("unhandled debug command %x", *cmd);
      }
   }
   assert(pos == bufsz);

   jit_install(j, f);
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

   jit_scalar_t p1 = { .pointer = NULL }, p2 = p1, result;
   if (!jit_fastcall(j, f->handle, &result, p1, p2)) {
      error_at(&(f->object->loc), "failed to initialise %s", istr(f->name));
      result.pointer = NULL;
   }
   else if (result.pointer == NULL)
      fatal_trace("link %s returned NULL", istr(f->name));

   vcode_state_restore(&state);

   // Initialisation should save the context pointer
   assert(result.pointer == mptr_get(j->mspace, f->privdata));

   return result.pointer;
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

static void jit_emit_trace(diag_t *d, const loc_t *loc, tree_t enclosing,
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

static void jit_interp_trace(diag_t *d)
{
   for (jit_anchor_t *a = jit_thread_local()->anchor; a; a = a->caller) {
      // Scan backwards to find the last debug info
      assert(a->irpos < a->func->nirs);
      const loc_t *loc = NULL;
      for (jit_ir_t *ir = &(a->func->irbuf[a->irpos]);
           ir >= a->func->irbuf; ir--) {
         if (ir->op == J_DEBUG) {
            loc = &ir->arg1.loc;
            break;
         }
         else if (ir->target)
            break;
      }

      tree_t enclosing = tree_from_object(a->func->object);
      assert(enclosing != NULL);

      const char *symbol = istr(a->func->name);
      jit_emit_trace(d, loc ?: tree_loc(enclosing), enclosing, symbol);
   }
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
      if (from == JIT_IDLE) {
         diag_add_hint_fn(jit_diag_cb, j);
         thread->jit = j;
      }
      else
         assert(thread->jit == j);
      break;
   case JIT_IDLE:
      diag_remove_hint_fn(jit_diag_cb);
      thread->jit = NULL;
      break;
   }
}

bool jit_fastcall(jit_t *j, jit_handle_t handle, jit_scalar_t *result,
                  jit_scalar_t p1, jit_scalar_t p2)
{
   jit_func_t *f = jit_get_func(j, handle);
   jit_thread_local_t *thread = jit_thread_local();

   assert(!thread->jmp_buf_valid);
   const int rc = sigsetjmp(thread->abort_env, 0);
   if (rc == 0) {
      thread->jmp_buf_valid = 1;

      if (f->symbol) {
         jit_transition(j, JIT_IDLE, JIT_NATIVE);
         void *(*fn)(void *, void *) = f->symbol;
         result->pointer = (*fn)(p1.pointer, p2.pointer);
         jit_transition(j, JIT_NATIVE, JIT_IDLE);
      }
      else {
         jit_transition(j, JIT_IDLE, JIT_INTERP);
         jit_scalar_t args[JIT_MAX_ARGS];
         args[0] = p1;
         args[1] = p2;
         (*f->entry)(f, NULL, args);
         *result = args[0];
         jit_transition(j, JIT_INTERP, JIT_IDLE);
      }

      thread->jmp_buf_valid = 0;
      thread->anchor = NULL;
      return true;
   }
   else {
      jit_transition(j, f->symbol ? JIT_NATIVE : JIT_INTERP, JIT_IDLE);
      thread->jmp_buf_valid = 0;
      thread->anchor = NULL;
      atomic_cas(&(j->exit_status), 0, rc - 1);
      return false;
   }
}

static bool jit_try_vcall(jit_t *j, jit_func_t *f, jit_scalar_t *result,
                          jit_scalar_t *args)
{
   jit_thread_local_t *thread = jit_thread_local();

   bool failed = false;
   const jit_state_t oldstate = thread->state;
   const jit_state_t newstate = f->symbol ? JIT_NATIVE : JIT_INTERP;
   const int rc = sigsetjmp(thread->abort_env, 0);
   if (rc == 0) {
      thread->jmp_buf_valid = 1;
      jit_transition(j, oldstate, newstate);

      if (f->symbol != NULL) {
         jit_foreign_t *ff = jit_ffi_get(f->name);
         if (ff == NULL)
            ff = jit_ffi_bind(f->name, f->spec, f->symbol);

         jit_ffi_call(ff, args);
      }
      else
         (*f->entry)(f, NULL, args);

      *result = args[0];
   }
   else {
      atomic_cas(&(j->exit_status), 0, rc - 1);
      failed = true;
   }

   jit_transition(j, newstate, oldstate);
   thread->jmp_buf_valid = 0;
   thread->anchor = NULL;

   return !failed;
}

static void jit_unpack_args(jit_func_t *f, jit_scalar_t *args, va_list ap)
{
   if (load_acquire(&(f->state)) != JIT_FUNC_READY)
      jit_irgen(f);   // Ensure FFI spec is set

   const int nargs = ffi_count_args(f->spec);
   assert(nargs <= JIT_MAX_ARGS);
   int wptr = 0;
   for (ffi_spec_t spec = f->spec >> 4; wptr < nargs; spec >>= 4) {
      switch (spec & 0xf) {
      case FFI_POINTER:
         args[wptr++].pointer = va_arg(ap, void *);
         break;
      case FFI_FLOAT:
         args[wptr++].real = va_arg(ap, double);
         break;
      case FFI_UARRAY:
         args[wptr++].pointer = va_arg(ap, void *);
         args[wptr++].integer = va_arg(ap, int32_t);
         args[wptr++].integer = va_arg(ap, int32_t);
         break;
      case FFI_INT8:
      case FFI_INT16:
      case FFI_INT32:
         args[wptr++].integer = va_arg(ap, int32_t);
         break;
      default:
         args[wptr++].integer = va_arg(ap, int64_t);
         break;
      }
   }
   assert(wptr == nargs);
}

bool jit_try_call(jit_t *j, jit_handle_t handle, jit_scalar_t *result, ...)
{
   va_list ap;
   va_start(ap, result);

   jit_func_t *f = jit_get_func(j, handle);

   jit_scalar_t args[JIT_MAX_ARGS];
   jit_unpack_args(f, args, ap);

   va_end(ap);

   return jit_try_vcall(j, f, result, args);
}

jit_scalar_t jit_call(jit_t *j, jit_handle_t handle, ...)
{
   va_list ap;
   va_start(ap, handle);

   jit_func_t *f = jit_get_func(j, handle);

   jit_scalar_t args[JIT_MAX_ARGS];
   jit_unpack_args(f, args, ap);

   jit_scalar_t result;
   if (!jit_try_vcall(j, f, &result, args))
      fatal_trace("call to %s failed", istr(jit_get_func(j, handle)->name));

   va_end(ap);
   return result;
}

bool jit_try_call_packed(jit_t *j, jit_handle_t handle, jit_scalar_t context,
                         void *input, size_t insz, void *output, size_t outsz)
{
   jit_func_t *f = jit_get_func(j, handle);

   if (load_acquire(&(f->state)) != JIT_FUNC_READY)
      jit_irgen(f);   // Ensure FFI spec is set

   assert(f->spec != 0);

   ffi_type_t atype = (f->spec >> 8) & 0xf;
   ffi_type_t rtype = f->spec & 0xf;

   jit_scalar_t args[JIT_MAX_ARGS];
   args[0] = context;

   if (ffi_is_integral(atype))
      args[1].integer = ffi_widen_int(atype, input, insz);
   else if (atype == FFI_FLOAT) {
      assert(insz == sizeof(double));
      args[1].real = *(double *)input;
   }
   else if (atype == FFI_POINTER)
      args[1].pointer = input;
   else
      fatal_trace("unhandled FFI argument type %x", atype);

   jit_scalar_t result;
   if (!jit_try_vcall(j, f, &result, args))
      return false;

   if (ffi_is_integral(rtype))
      ffi_store_int(rtype, result.integer, output, outsz);
   else if (rtype == FFI_FLOAT) {
      assert(outsz == sizeof(double));
      *(double *)output = result.real;
   }
   else if (rtype == FFI_POINTER)
      memcpy(output, result.pointer, outsz);
   else
      fatal_trace("unhandled FFI result type %x", rtype);

   return true;
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
   f->object = vcode_unit_object(unit);

   jit_irgen(f);

   jit_scalar_t args[JIT_MAX_ARGS];
   bool ok = jit_try_vcall(j, f, result, args);

   jit_free_func(f);
   return ok;
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

   uint32_t abi_version = 0;

   // Loading the DLL can execute constructors
   jit_transition(j, JIT_IDLE, JIT_NATIVE);

   if (j->aotlib != NULL)
      fatal_trace("AOT library alreadt loaded");

   j->aotlib = ffi_load_dll(so_path);

   uint32_t *p = ffi_find_symbol(j->aotlib, "__nvc_abi_version");
   if (p == NULL)
      warnf("%s: cannot find symbol __nvc_abi_version", so_path);
   else
      abi_version = *p;

   jit_transition(j, JIT_NATIVE, JIT_IDLE);

   if (abi_version != RT_ABI_VERSION)
      fatal("%s: ABI version %d does not match current version %d",
            so_path, abi_version, RT_ABI_VERSION);
}

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
   case JIT_INTERP:
      assert(code >= 0);
      if (thread->jmp_buf_valid)
         siglongjmp(thread->abort_env, code + 1);
      else
         fatal_exit(code);
      break;
   }

   __builtin_unreachable();
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
   assert(threshold > 0);

   jit_tier_t *t = xcalloc(sizeof(jit_tier_t));
   t->next      = j->tiers;
   t->threshold = threshold;
   t->plugin    = *plugin;
   t->context   = (*plugin->init)();

   j->tiers = t;
}

ident_t jit_get_name(jit_t *j, jit_handle_t handle)
{
   return jit_get_func(j, handle)->name;
}

jit_handle_t jit_assemble(jit_t *j, ident_t name, const char *text)
{
   jit_func_t *f = chash_get(j->index, name);
   if (f != NULL)
      return f->handle;

   SCOPED_LOCK(j->lock);

   f = xcalloc(sizeof(jit_func_t));

   f->name      = name;
   f->state     = JIT_FUNC_READY;
   f->jit       = j;
   f->handle    = j->next_handle++;
   f->next_tier = j->tiers;
   f->hotness   = f->next_tier ? f->next_tier->threshold : 0;
   f->entry     = jit_interp;

   jit_install(j, f);

   enum { LABEL, INS, CCSIZE, RESULT, ARG1, ARG2, NEWLINE } state = LABEL;

   static const struct {
      const char *name;
      jit_op_t    op;
      int         nresult;
      int         nargs;
   } optab[] = {
      { "MOV",   J_MOV,      1, 1 },
      { "ADD",   J_ADD,      1, 2 },
      { "SUB",   J_SUB,      1, 2 },
      { "MUL",   J_MUL,      1, 2 },
      { "RECV",  J_RECV,     1, 1 },
      { "SEND",  J_SEND,     0, 2 },
      { "RET",   J_RET,      0, 0 },
      { "CMP",   J_CMP,      0, 2 },
      { "JUMP",  J_JUMP,     0, 1 },
      { "$COPY", MACRO_COPY, 1, 2 },
   };

   static const struct {
      const char *name;
      jit_cc_t    cc;
   } cctab[] = {
      { "T",  JIT_CC_T },
      { "F",  JIT_CC_F },
      { "EQ", JIT_CC_EQ },
      { "O",  JIT_CC_O },
      { "C",  JIT_CC_C },
   };

   const unsigned bufsz = 128;
   f->irbuf = xcalloc_array(bufsz, sizeof(jit_ir_t));

   SCOPED_A(int) lpatch = AINIT;
   int *labels LOCAL = NULL, maxlabel = 0;
   jit_ir_t *ir = NULL;
   int nresult = 0, nargs = 0;
   char *copy LOCAL = xstrdup(text);
   for (char *tok = strtok(copy, " \r\t"); tok; tok = strtok(NULL, " \r\t")) {
   again:
      switch (state) {
      case LABEL:
         assert(f->nirs < bufsz);
         ir = &(f->irbuf[f->nirs++]);

         if (tok[0] == 'L' && isdigit((int)tok[1])) {
            char *eptr = NULL;
            const int lab = strtol(tok + 1, &eptr, 10);
            if (*eptr != ':')
               fatal_trace("cannot parse label '%s'", tok);

            if (lab > maxlabel) {
               maxlabel = next_power_of_2(lab);
               labels = xrealloc_array(labels, maxlabel + 1, sizeof(int));
            }

            ir->target = 1;
            labels[lab] = ir - f->irbuf;
            state = INS;
         }
         else {
            state = INS;
            goto again;
         }
         break;

      case INS:
         {
            char *dot = strchr(tok, '.');
            if (dot != NULL)
               *dot++ = '\0';

            int ipos = 0;
            for (; ipos < ARRAY_LEN(optab)
                    && strcmp(optab[ipos].name, tok); ipos++)
               ;

            if (ipos == ARRAY_LEN(optab))
               fatal_trace("illegal instruction %s", tok);

            nargs = optab[ipos].nargs;
            nresult = optab[ipos].nresult;

            ir->op = optab[ipos].op;
            ir->size = JIT_SZ_UNSPEC;
            ir->result = JIT_REG_INVALID;

            if (dot != NULL) {
               tok = dot;
               state = CCSIZE;
               goto again;
            }

            state = nresult > 0 ? RESULT : (nargs > 0 ? ARG1 : NEWLINE);
         }
         break;

      case CCSIZE:
         {
            if (isdigit((int)tok[0])) {
               switch (atoi(tok)) {
               case 8: ir->size = JIT_SZ_8; break;
               case 16: ir->size = JIT_SZ_16; break;
               case 32: ir->size = JIT_SZ_32; break;
               case 64: ir->size = JIT_SZ_64; break;
               default:
                  fatal_trace("illegal operation size %s", tok);
               }
            }
            else {
               int cpos = 0;
               for (; cpos < ARRAY_LEN(cctab)
                       && strcmp(cctab[cpos].name, tok); cpos++)
                  ;

               if (cpos == ARRAY_LEN(cctab))
                  fatal_trace("illegal condition code %s", tok);

               ir->cc = cctab[cpos].cc;
            }

            state = nresult > 0 ? RESULT : (nargs > 0 ? ARG1 : NEWLINE);
         }
         break;

      case RESULT:
         if (tok[0] != 'R')
            fatal_trace("expected register name but got '%s'", tok);

         ir->result = atoi(tok + 1);
         f->nregs = MAX(ir->result + 1, f->nregs);
         state = ARG1;
         break;

      case ARG1:
      case ARG2:
         {
            jit_value_t arg;
            if (tok[0] == 'R') {
               arg.kind = JIT_VALUE_REG;
               arg.reg  = atoi(tok + 1);
               f->nregs = MAX(arg.reg + 1, f->nregs);
            }
            else if (tok[0] == '#') {
               arg.kind  = JIT_VALUE_INT64;
               arg.int64 = strtoll(tok + 1, NULL, 0);
            }
            else if (tok[0] == 'L') {
               APUSH(lpatch, ir - f->irbuf);
               arg.kind = JIT_VALUE_LABEL;
               arg.label = atoi(tok + 1);
            }
            else if (tok[0] == '[' && tok[1] == 'R') {
               arg.kind = JIT_ADDR_REG;
               arg.reg  = atoi(tok + 2);
               arg.disp = 0;
               f->nregs = MAX(arg.reg + 1, f->nregs);
            }
            else if (tok[0] == '\n')
               fatal_trace("got newline, expecting argument");
            else
               fatal_trace("cannot parse argument '%s'", tok);

            if (state == ARG1)
               ir->arg1 = arg;
            else
               ir->arg2 = arg;

            state = state == ARG1 && nargs > 1 ? ARG2 : NEWLINE;
         }
         break;

      case NEWLINE:
         if (*tok != '\n')
            fatal_trace("expected newline, got '%s'", tok);
         state = LABEL;
         if (*++tok != '\0')
            goto again;
         break;
      }
   }

   for (int i = 0; i < lpatch.count; i++) {
      jit_ir_t *ir = &(f->irbuf[lpatch.items[i]]);
      ir->arg1.label = labels[ir->arg1.label];
   }

   return f->handle;
}
