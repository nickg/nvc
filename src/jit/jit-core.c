//
//  Copyright (C) 2022-2023  Nick Gasson
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
#include "option.h"
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

#define FUNC_HASH_SZ    1024
#define FUNC_LIST_SZ    512
#define COMPILE_TIMEOUT 100000

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

typedef struct _aot_dll {
   jit_dll_t  *dll;
   jit_pack_t *pack;
} aot_dll_t;

typedef struct {
   reloc_kind_t  kind;
   union {
      uintptr_t  off;
      void      *ptr;
   };
} aot_reloc_t;

// The code generator knows the layout of this struct
typedef struct {
   jit_entry_fn_t  entry;
   const char     *strtab;
   const uint8_t  *debug;
   const uint8_t  *cpool;
   aot_reloc_t     relocs[0];
} aot_descr_t;

typedef struct _jit {
   chash_t        *index;
   mspace_t       *mspace;
   void           *lower_ctx;
   hash_t         *layouts;
   bool            silent;
   bool            runtime;
   bool            shutdown;
   unsigned        backedge;
   int             exit_status;
   jit_tier_t     *tiers;
   aot_dll_t      *aotlib;
   aot_dll_t      *preloadlib;
   func_array_t   *funcs;
   unsigned        next_handle;
   nvc_lock_t      lock;
   int32_t        *cover_mem[4];
   jit_irq_fn_t    interrupt;
   void           *interrupt_ctx;
} jit_t;

static void jit_oom_cb(mspace_t *m, size_t size)
{
   diag_t *d = diag_new(DIAG_FATAL, NULL);
   diag_printf(d, "out of memory attempting to allocate %zu byte object", size);

   const size_t heapsize = opt_get_size(OPT_HEAP_SIZE);
   diag_hint(d, NULL, "the current heap size is %zu bytes which you can "
             "increase with the $bold$-H$$ option, for example $bold$-H %zum$$",
             heapsize, MAX(1, (heapsize * 2) / 1024 / 1024));

   diag_emit(d);
   jit_abort(EXIT_FAILURE);
}

jit_thread_local_t *jit_thread_local(void)
{
   static __thread jit_thread_local_t *local = NULL;

   if (unlikely(local == NULL)) {
      local = xcalloc(sizeof(jit_thread_local_t));
      local->state = JIT_IDLE;
   }

   return local;
}

jit_t *jit_new(void)
{
   jit_t *j = xcalloc(sizeof(jit_t));
   j->index = chash_new(FUNC_HASH_SZ);
   j->mspace = mspace_new(opt_get_size(OPT_HEAP_SIZE));

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
   free(f->linktab);
   if (f->owns_cpool) free(f->cpool);
   free(f);
}

void jit_free(jit_t *j)
{
   store_release(&j->shutdown, true);
   async_barrier();

   aot_dll_t *libs[] = { j->aotlib, j->preloadlib };
   for (int i = 0; i < ARRAY_LEN(libs); i++) {
      if (libs[i] != NULL) {
         ffi_unload_dll(libs[i]->dll);
         jit_pack_free(libs[i]->pack);
         free(libs[i]);
      }
   }

   for (int i = 0; i < j->next_handle; i++)
      jit_free_func(j->funcs->items[i]);
   free(j->funcs);

   for (int i = 0; i < ARRAY_LEN(j->cover_mem); i++)
      free(j->cover_mem[i]);

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

void *jit_mspace_alloc(size_t size)
{
   jit_thread_local_t *thread = jit_thread_local();
   assert(thread->state == JIT_RUNNING);
   return mspace_alloc(thread->jit->mspace, size);
}

static void jit_install(jit_t *j, jit_func_t *f)
{
   assert_lock_held(&(j->lock));

   func_array_t *list = j->funcs;
   if (f->handle >= list->length) {
      const size_t newlen = MAX(f->handle + 1, list->length * 2);
      func_array_t *new = xcalloc_flex(sizeof(func_array_t),
                                       newlen, sizeof(jit_func_t *));
      new->length = newlen;
      for (size_t i = 0; i < list->length; i++)
         new->items[i] = list->items[i];

      // Synchronises with load_acquire in jit_get_func
      store_release(&(j->funcs), new);

      async_free(list);
      list = new;
   }

   // Synchronises with load_acquire in jit_get_func
   store_release(&(list->items[f->handle]), f);

   chash_put(j->index, f->name, f);
   if (f->unit) chash_put(j->index, f->unit, f);
}

static jit_handle_t jit_lazy_compile_locked(jit_t *j, ident_t name)
{
   assert_lock_held(&j->lock);

   jit_func_t *f = chash_get(j->index, name);
   if (f != NULL)
      return f->handle;

   aot_descr_t *descr = NULL;
   if (j->aotlib != NULL || j->preloadlib != NULL) {
      LOCAL_TEXT_BUF tb = safe_symbol(name);
      tb_cat(tb, ".descr");

      aot_dll_t *try[] = { j->aotlib, j->preloadlib };
      for (int i = 0; i < ARRAY_LEN(try); i++) {
         if (try[i] == NULL)
            continue;
         else if ((descr = ffi_find_symbol(try[i]->dll, tb_get(tb)))) {
            jit_pack_put(try[i]->pack, name, descr->cpool,
                         descr->strtab, descr->debug);
            break;
         }
      }
   }

   vcode_unit_t vu = vcode_find_unit(name);
   assert(vu == NULL || chash_get(j->index, vu) == NULL);

   f = xcalloc(sizeof(jit_func_t));

   f->name      = name;
   f->state     = descr ? JIT_FUNC_COMPILING : JIT_FUNC_PLACEHOLDER;
   f->unit      = vu;
   f->jit       = j;
   f->handle    = j->next_handle++;
   f->next_tier = j->tiers;
   f->hotness   = f->next_tier ? f->next_tier->threshold : 0;
   f->entry     = descr ? descr->entry : jit_interp;
   f->object    = vu ? vcode_unit_object(vu) : NULL;

   // Install now to allow circular references in relocations
   jit_install(j, f);

   if (descr != NULL) {
      for (aot_reloc_t *r = descr->relocs; r->kind != RELOC_NULL; r++) {
         const char *str = descr->strtab + r->off;
         if (r->kind == RELOC_FOREIGN) {
            const char *eptr = strchr(str, '\b');
            if (eptr == NULL)
               fatal_trace("invalid foreign reloc '%s'", str);

            ffi_spec_t spec = ffi_spec_new(str, eptr - str);

            ident_t id = ident_new(eptr + 1);
            r->ptr = jit_ffi_get(id) ?: jit_ffi_bind(id, spec, NULL);
         }
         else if (r->kind == RELOC_COVER) {
            // TODO: get rid of the double indirection here by
            //       allocating coverage memory earlier
            if (strcmp(str, "stmt") == 0)
               r->ptr = &(j->cover_mem[JIT_COVER_STMT]);
            else if (strcmp(str, "branch") == 0)
               r->ptr = &(j->cover_mem[JIT_COVER_BRANCH]);
            else if (strcmp(str, "expr") == 0)
               r->ptr = &(j->cover_mem[JIT_COVER_EXPRESSION]);
            else if (strcmp(str, "toggle") == 0)
               r->ptr = &(j->cover_mem[JIT_COVER_TOGGLE]);
            else
               fatal_trace("relocation against invalid coverage kind %s", str);
         }
         else {
            jit_handle_t h = jit_lazy_compile_locked(j, ident_new(str));
            if (h == JIT_HANDLE_INVALID)
               fatal_trace("relocation against invalid function %s", str);

            switch (r->kind) {
            case RELOC_FUNC:
               r->ptr = jit_get_func(j, h);
               break;
            case RELOC_HANDLE:
               r->ptr = (void *)(uintptr_t)h;
               break;
            case RELOC_PRIVDATA:
               r->ptr = jit_get_privdata_ptr(j, jit_get_func(j, h));
               break;
            default:
               fatal_trace("unhandled relocation kind %d", r->kind);
            }
         }
      }

      store_release(&f->state, JIT_FUNC_READY);
   }

   return f->handle;
}

jit_handle_t jit_lazy_compile(jit_t *j, ident_t name)
{
   jit_func_t *f = chash_get(j->index, name);
   if (f != NULL)
      return f->handle;

   SCOPED_LOCK(j->lock);
   return jit_lazy_compile_locked(j, name);
}

jit_func_t *jit_get_func(jit_t *j, jit_handle_t handle)
{
   assert(handle != JIT_HANDLE_INVALID);

   // Synchronises with store_release in jit_install
   func_array_t *list = load_acquire(&(j->funcs));
   assert(handle < list->length);

   // Synchronises with store_release in jit_install
   jit_func_t *f = load_acquire(&(list->items[handle]));
   assert(f != NULL);
   return f;
}

static inline bool jit_fill_from_aot(jit_func_t *f, aot_dll_t *lib)
{
   return lib != NULL && jit_pack_fill(lib->pack, f->jit, f);
}

void jit_fill_irbuf(jit_func_t *f)
{
   const func_state_t state = load_acquire(&(f->state));
   switch (state) {
   case JIT_FUNC_READY:
      if (f->irbuf != NULL)
         return;
      // Fall-through
   case JIT_FUNC_PLACEHOLDER:
      if (atomic_cas(&(f->state), state, JIT_FUNC_COMPILING))
         break;
      // Fall-through
   case JIT_FUNC_COMPILING:
      // Another thread is compiling this function
      for (int timeout = 0; load_acquire(&(f->state)) != JIT_FUNC_READY; ) {
         if (++timeout % COMPILE_TIMEOUT == 0)
            warnf("waiting for %s to finish compiling", istr(f->name));
         thread_sleep(100);
      }
      return;
   default:
      fatal_trace("illegal function state for %s", istr(f->name));
   }

   assert(f->irbuf == NULL);

   if (jit_fill_from_aot(f, f->jit->aotlib))
      return;
   else if (jit_fill_from_aot(f, f->jit->preloadlib))
      return;
   else if (f->unit || (f->unit = vcode_find_unit(f->name)))
      jit_irgen(f);
   else {
      if (opt_get_int(OPT_JIT_LOG))
         debugf("loading vcode for %s", istr(f->name));

      tree_t unit = lib_get_qualified(f->name);
      if (unit != NULL && tree_kind(unit) == T_PACKAGE)
         (void)body_of(unit);  // Make sure body is loaded

      if ((f->unit = vcode_find_unit(f->name)) == NULL)
         fatal_trace("cannot generate JIT IR for %s", istr(f->name));

      jit_irgen(f);
   }
}

jit_handle_t jit_compile(jit_t *j, ident_t name)
{
   jit_handle_t handle = jit_lazy_compile(j, name);
   if (handle == JIT_HANDLE_INVALID)
      return handle;

   jit_func_t *f = jit_get_func(j, handle);
   jit_fill_irbuf(f);

   return handle;
}

void *jit_link(jit_t *j, jit_handle_t handle)
{
   if (handle == JIT_HANDLE_INVALID)
      return NULL;

   jit_func_t *f = jit_get_func(j, handle);
   if (f->privdata != MPTR_INVALID)
      return *mptr_get(f->privdata);

   f->privdata = mptr_new(j->mspace, "privdata");

   vcode_state_t state;
   vcode_state_save(&state);

   vcode_select_unit(f->unit);
   const vunit_kind_t kind = vcode_unit_kind();
   if (kind != VCODE_UNIT_PACKAGE && kind != VCODE_UNIT_INSTANCE)
      fatal_trace("cannot link unit %s", istr(f->name));

   tlab_t tlab = jit_null_tlab(j);
   jit_scalar_t p1 = { .pointer = NULL }, p2 = p1, result;
   if (!jit_fastcall(j, f->handle, &result, p1, p2, &tlab)) {
      error_at(&(f->object->loc), "failed to initialise %s", istr(f->name));
      result.pointer = NULL;
   }
   else if (result.pointer == NULL)
      fatal_trace("link %s returned NULL", istr(f->name));

   vcode_state_restore(&state);

   // Initialisation should save the context pointer
   assert(result.pointer == *mptr_get(f->privdata));

   return result.pointer;
}

void **jit_get_privdata_ptr(jit_t *j, jit_func_t *f)
{
   if (f->privdata == MPTR_INVALID)
      f->privdata = mptr_new(j->mspace, "privdata");

   return mptr_get(f->privdata);
}

void *jit_get_frame_var(jit_t *j, jit_handle_t handle, uint32_t var)
{
   jit_func_t *f = jit_get_func(j, handle);
   if (f->privdata == MPTR_INVALID)
      fatal_trace("%s not linked", istr(f->name));

   assert(var < f->nvars);
   return *mptr_get(f->privdata) + f->linktab[var].offset;
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
   case T_PACKAGE:
   case T_PACK_BODY:
   case T_PACK_INST:
      diag_trace(d, loc, "Package$$ %s", istr(tree_ident(enclosing)));
      break;
   default:
      diag_trace(d, loc, "$$%s", istr(tree_ident(enclosing)));
      break;
   }
}

jit_stack_trace_t *jit_stack_trace(void)
{
   jit_thread_local_t *thread = jit_thread_local();

   int count = 0;
   for (jit_anchor_t *a = thread->anchor; a; a = a->caller)
      count++;

   jit_stack_trace_t *stack =
      xmalloc_flex(sizeof(jit_stack_trace_t), count, sizeof(jit_frame_t));
   stack->count = count;

   jit_frame_t *frame = stack->frames;
   for (jit_anchor_t *a = thread->anchor; a; a = a->caller, frame++) {
      jit_fill_irbuf(a->func);

      frame->decl = NULL;
      if (a->func->object != NULL)
         frame->decl = tree_from_object(a->func->object);

      // Scan backwards to find the last debug info
      assert(a->irpos < a->func->nirs);
      frame->loc = frame->decl ? *tree_loc(frame->decl) : LOC_INVALID;
      for (jit_ir_t *ir = &(a->func->irbuf[a->irpos]);
           ir >= a->func->irbuf; ir--) {
         if (ir->op == J_DEBUG) {
            frame->loc = ir->arg1.loc;
            break;
         }
         else if (ir->target)
            break;
      }

      frame->symbol = a->func->name;
   }

   return stack;
}

static void jit_diag_cb(diag_t *d, void *arg)
{
   jit_t *j = arg;

   if (j->silent) {
      diag_suppress(d, true);
      return;
   }
   else if (unlikely(jit_thread_local()->state == JIT_IDLE))
      fatal_trace("JIT diag callback called when idle");

   jit_stack_trace_t *stack LOCAL = jit_stack_trace();

   for (int i = 0; i < stack->count; i++) {
      jit_frame_t *frame = &(stack->frames[i]);

      if (frame->decl != NULL)
         jit_emit_trace(d, &(frame->loc), frame->decl, istr(frame->symbol));
      else
         diag_trace(d, &(frame->loc), "%s", istr(frame->symbol));
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
   case JIT_RUNNING:
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
                  jit_scalar_t p1, jit_scalar_t p2, tlab_t *tlab)
{
   jit_func_t *f = jit_get_func(j, handle);
   jit_thread_local_t *thread = jit_thread_local();

   assert(!thread->jmp_buf_valid);
   const int rc = sigsetjmp(thread->abort_env, 0);
   if (rc == 0) {
      thread->jmp_buf_valid = 1;

      jit_transition(j, JIT_IDLE, JIT_RUNNING);
      jit_scalar_t args[JIT_MAX_ARGS];
      args[0] = p1;
      args[1] = p2;
      jit_entry_fn_t entry = load_acquire(&f->entry);
      (*entry)(f, NULL, args, tlab);
      *result = args[0];
      jit_transition(j, JIT_RUNNING, JIT_IDLE);

      thread->jmp_buf_valid = 0;
      thread->anchor = NULL;
      return true;
   }
   else {
      jit_transition(j, JIT_RUNNING, JIT_IDLE);
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
   const int rc = sigsetjmp(thread->abort_env, 0);
   if (rc == 0) {
      thread->jmp_buf_valid = 1;
      jit_transition(j, oldstate, JIT_RUNNING);

      tlab_t tlab = jit_null_tlab(j);
      (*f->entry)(f, NULL, args, &tlab);

      *result = args[0];
   }
   else {
      atomic_cas(&(j->exit_status), 0, rc - 1);
      failed = true;
   }

   jit_transition(j, JIT_RUNNING, oldstate);
   thread->jmp_buf_valid = 0;
   thread->anchor = NULL;

   return !failed;
}

static void jit_unpack_args(jit_func_t *f, jit_scalar_t *args, va_list ap)
{
   jit_fill_irbuf(f);  // Ensure FFI spec is set

   int wptr = 0;
   for (int i = 1; ffi_spec_has(f->spec, i); i++) {
      switch (ffi_spec_get(f->spec, i)) {
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

   jit_fill_irbuf(f);   // Ensure FFI spec is set
   assert(ffi_spec_valid(f->spec));

   ffi_type_t atype = ffi_spec_get(f->spec, 2);
   ffi_type_t rtype = ffi_spec_get(f->spec, 0);

   jit_scalar_t args[JIT_MAX_ARGS];
   args[0] = context;

   if (ffi_is_integral(atype))
      args[1].integer = ffi_widen_int(atype, input);
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
      ffi_store_int(rtype, result.integer, output);
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

bool jit_call_thunk(jit_t *j, vcode_unit_t unit, jit_scalar_t *result,
                    void *context)
{
   vcode_select_unit(unit);
   assert(vcode_unit_kind() == VCODE_UNIT_THUNK);

   jit_func_t *f = xcalloc(sizeof(jit_func_t));
   f->state  = JIT_FUNC_COMPILING;
   f->unit   = unit;
   f->jit    = j;
   f->handle = JIT_HANDLE_INVALID;
   f->entry  = jit_interp;
   f->object = vcode_unit_object(unit);

   jit_irgen(f);

   jit_scalar_t args[JIT_MAX_ARGS];
   args[0].pointer = context;

   bool ok = jit_try_vcall(j, f, result, args);

   jit_free_func(f);
   return ok;
}

tlab_t jit_null_tlab(jit_t *j)
{
   tlab_t t = { .mspace = j->mspace };
   return t;
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

static aot_dll_t *load_dll_internal(jit_t *j, const char *path)
{
   uint32_t abi_version = 0;

   aot_dll_t *lib = xcalloc(sizeof(aot_dll_t));
   lib->pack = jit_pack_new();
   lib->dll  = ffi_load_dll(path);

   uint32_t *p = ffi_find_symbol(lib->dll, "__nvc_abi_version");
   if (p == NULL)
      warnf("%s: cannot find symbol __nvc_abi_version", path);
   else
      abi_version = *p;

   if (abi_version != RT_ABI_VERSION)
      fatal("%s: ABI version %d does not match current version %d",
            path, abi_version, RT_ABI_VERSION);

   if (opt_get_int(OPT_JIT_LOG))
      debugf("loaded AOT library from %s", path);

   return lib;
}

void jit_preload(jit_t *j)
{
#ifdef HAVE_LLVM
   if (j->preloadlib != NULL)
      fatal_trace("Preload library already loaded");

   lib_t std = lib_find(well_known(W_STD));
   if (std == NULL)
      fatal("cannot find STD library");

   LOCAL_TEXT_BUF tb = tb_new();
   tb_cat(tb, lib_path(std));
   tb_cat(tb, DIR_SEP "..");

   const char *preload_vers[] = { "93", "93", "93", "93", "08", "19" };
   tb_printf(tb, DIR_SEP "preload%s." DLL_EXT, preload_vers[standard()]);

   const char *path = tb_get(tb);
   struct stat st;
   if (stat(path, &st) != 0 || !S_ISREG(st.st_mode))
      fatal("missing preload library at %s", path);

   j->preloadlib = load_dll_internal(j, path);
#endif  // HAVE_LLVM
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

   if (j->aotlib != NULL)
      fatal_trace("AOT library already loaded");

   j->aotlib = load_dll_internal(j, so_path);
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
   case JIT_RUNNING:
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

static void jit_async_cgen(void *context, void *arg)
{
   jit_func_t *f = context;
   jit_tier_t *tier = arg;

   if (!load_acquire(&f->jit->shutdown))
      (*tier->plugin.cgen)(f->jit, f->handle, tier->context);
}

void jit_tier_up(jit_func_t *f)
{
   assert(f->hotness <= 0);
   assert(f->next_tier != NULL);

   if (opt_get_int(OPT_JIT_ASYNC))
      async_do(jit_async_cgen, f, f->next_tier);
   else
      (f->next_tier->plugin.cgen)(f->jit, f->handle, f->next_tier->context);

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
   t->context   = (*plugin->init)(j);

   j->tiers = t;
}

ident_t jit_get_name(jit_t *j, jit_handle_t handle)
{
   return jit_get_func(j, handle)->name;
}

bool jit_writes_flags(jit_ir_t *ir)
{
   return ir->op == J_CMP || ir->op == J_FCMP
      || ir->op == J_CCMP || ir->op == J_FCCMP
      || (ir->op == J_ADD && ir->cc != JIT_CC_NONE)
      || (ir->op == J_SUB && ir->cc != JIT_CC_NONE)
      || (ir->op == J_MUL && ir->cc != JIT_CC_NONE)
      || (ir->op == MACRO_EXP && ir->cc != JIT_CC_NONE);
}

bool jit_reads_flags(jit_ir_t *ir)
{
   return (ir->op == J_JUMP && ir->cc != JIT_CC_NONE)
      || ir->op == J_CSET || ir->op == J_CSEL || ir->op == J_CCMP
      || ir->op == J_FCCMP;
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

   enum { LABEL, INS, CCSIZE, RESULT, ARG1, ARG2, NEWLINE, CP } state = LABEL;

   static const struct {
      const char *name;
      jit_op_t    op;
      int         nresult;
      int         nargs;
   } optab[] = {
      { "MOV",     J_MOV,        1, 1 },
      { "ADD",     J_ADD,        1, 2 },
      { "SUB",     J_SUB,        1, 2 },
      { "MUL",     J_MUL,        1, 2 },
      { "DIV",     J_DIV,        1, 2 },
      { "REM",     J_REM,        1, 2 },
      { "RECV",    J_RECV,       1, 1 },
      { "SEND",    J_SEND,       0, 2 },
      { "RET",     J_RET,        0, 0 },
      { "CMP",     J_CMP,        0, 2 },
      { "CCMP",    J_CCMP,       0, 2 },
      { "JUMP",    J_JUMP,       0, 1 },
      { "CSEL",    J_CSEL,       1, 2 },
      { "CSET",    J_CSET,       1, 0 },
      { "NOP",     J_NOP,        0, 0 },
      { "CLAMP",   J_CLAMP,      1, 1 },
      { "CALL",    J_CALL,       0, 1 },
      { "STORE",   J_STORE,      0, 2 },
      { "LOAD",    J_LOAD,       1, 1 },
      { "ULOAD",   J_ULOAD,      1, 1 },
      { "SHL",     J_SHL,        1, 2 },
      { "ASR",     J_ASR,        1, 2 },
      { "LEA",     J_LEA,        1, 1 },
      { "AND",     J_AND,        1, 2 },
      { "OR",      J_OR,         1, 2 },
      { "XOR",     J_XOR,        1, 2 },
      { "NOT",     J_NOT,        1, 1 },
      { "NEG",     J_NEG,        1, 1 },
      { "FADD",    J_FADD,       1, 2 },
      { "FSUB",    J_FSUB,       1, 2 },
      { "FMUL",    J_FMUL,       1, 2 },
      { "FDIV",    J_FDIV,       1, 2 },
      { "FNEG",    J_FNEG,       1, 1 },
      { "FCMP",    J_FCMP,       0, 2 },
      { "FCVTNS",  J_FCVTNS,     1, 1 },
      { "SCVTF",   J_SCVTF,      1, 1 },
      { "$EXIT",   MACRO_EXIT,   0, 1 },
      { "$COPY",   MACRO_COPY,   1, 2 },
      { "$MOVE",   MACRO_MOVE,   1, 2 },
      { "$CASE",   MACRO_CASE,   1, 2 },
      { "$SALLOC", MACRO_SALLOC, 1, 2 },
      { "$LALLOC", MACRO_LALLOC, 1, 1 },
      { "$BZERO",  MACRO_BZERO,  1, 1 },
      { "$MEMSET", MACRO_MEMSET, 1, 2 },
      { "$EXP",    MACRO_EXP,    1, 2 },
      { "$FEXP",   MACRO_FEXP,   1, 2 },
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
      { "LT", JIT_CC_LT },
      { "LE", JIT_CC_LE },
      { "GT", JIT_CC_GT },
      { "GE", JIT_CC_GE },
   };

   const unsigned bufsz = 128;
   f->irbuf = xcalloc_array(bufsz, sizeof(jit_ir_t));

   SCOPED_A(int) lpatch = AINIT;
   int *labels LOCAL = NULL, maxlabel = 0;
   jit_ir_t *ir = NULL;
   int nresult = 0, nargs = 0, cpoolptr = 0;
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
         else if (isdigit((int)tok[1])) {
            state = CP;
            goto again;
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
            char *dot = strchr(tok, '.');
            if (dot != NULL)
               *dot++ = '\0';

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

            if (dot != NULL) {
               tok = dot;
               state = CCSIZE;
               goto again;
            }

            state = nresult > 0 ? RESULT : (nargs > 0 ? ARG1 : NEWLINE);
         }
         break;

      case RESULT:
         if (tok[0] != 'R')
            fatal_trace("expected register name but got '%s'", tok);

         ir->result = atoi(tok + 1);
         f->nregs = MAX(ir->result + 1, f->nregs);
         state = nargs > 0 ? ARG1 : NEWLINE;
         break;

      case ARG1:
      case ARG2:
         {
            jit_value_t arg;
            size_t toklen = strlen(tok);
            if (tok[0] == 'R') {
               arg.kind = JIT_VALUE_REG;
               arg.reg  = atoi(tok + 1);
               f->nregs = MAX(arg.reg + 1, f->nregs);
            }
            else if (tok[0] == '#') {
               arg.kind  = JIT_VALUE_INT64;
               arg.int64 = strtoll(tok + 1, NULL, 0);
            }
            else if (tok[0] == '%') {
               arg.kind  = JIT_VALUE_DOUBLE;
               arg.dval = strtod(tok + 1, NULL);
            }
            else if (tok[0] == 'L') {
               APUSH(lpatch, ir - f->irbuf);
               arg.kind = JIT_VALUE_LABEL;
               arg.label = atoi(tok + 1);
            }
            else if (tok[0] == '[' && tok[1] == 'R') {
               char *eptr = NULL;
               arg.kind = JIT_ADDR_REG;
               arg.reg = strtol(tok + 2, &eptr, 0);
               if (*eptr == '+' || *eptr == '-')
                  arg.disp = strtol(eptr, &eptr, 0);
               else
                  arg.disp = 0;
               if (*eptr != ']')
                  fatal_trace("invalid address %s", tok);
               f->nregs = MAX(arg.reg + 1, f->nregs);
            }
            else if (tok[0] == '[' && tok[1] == 'C' && tok[2] == 'P') {
               char *eptr = NULL;
               arg.int64 = strtol(tok + 4, &eptr, 0);
               arg.kind = JIT_ADDR_CPOOL;
               if (tok[3] != '+' || *eptr != ']')
                  fatal_trace("invalid address %s", tok);
            }
            else if (tok[0] == '<' && tok[toklen - 1] == '>') {
               tok[toklen - 1] = '\0';
               arg.kind = JIT_VALUE_HANDLE;
               arg.handle = jit_lazy_compile(j, ident_new(tok + 1));
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
         if (ir->op == MACRO_SALLOC)
            f->framesz += ALIGN_UP(ir->arg2.int64, 8);
         state = LABEL;
         if (*++tok != '\0')
            goto again;
         break;

      case CP:
         {
            if (*tok == '\n' && *++tok == '\0')
               continue;

            char *eptr = NULL;
            unsigned long byte = strtoul(tok, &eptr, 16);
            if (*eptr != '\0' || byte >= 256)
               fatal_trace("invalid constant pool value '%s'", tok);

            if (f->cpool == NULL)
               f->cpool = xmalloc((f->cpoolsz = 64));
            else if (cpoolptr == f->cpoolsz)
               fatal_trace("constant pool overflow");

            f->cpool[cpoolptr++] = byte;
         }
         break;
      }
   }

   f->cpoolsz = cpoolptr;

   for (int i = 0; i < lpatch.count; i++) {
      jit_ir_t *ir = &(f->irbuf[lpatch.items[i]]);
      if (ir->arg1.kind == JIT_VALUE_LABEL)
         ir->arg1.label = labels[ir->arg1.label];
      else
         ir->arg2.label = labels[ir->arg2.label];
   }

   return f->handle;
}

bool jit_will_abort(jit_ir_t *ir)
{
   if (ir->op == MACRO_EXIT) {
      return ir->arg1.exit == JIT_EXIT_INDEX_FAIL
         || ir->arg1.exit == JIT_EXIT_OVERFLOW
         || ir->arg1.exit == JIT_EXIT_NULL_DEREF
         || ir->arg1.exit == JIT_EXIT_UNREACHABLE
         || ir->arg1.exit == JIT_EXIT_LENGTH_FAIL
         || ir->arg1.exit == JIT_EXIT_DIV_ZERO
         || ir->arg1.exit == JIT_EXIT_EXPONENT_FAIL
         || ir->arg1.exit == JIT_EXIT_RANGE_FAIL
         || ir->arg1.exit == JIT_EXIT_ELAB_ORDER_FAIL;
   }
   else
      return ir->op == J_TRAP;
}

void jit_alloc_cover_mem(jit_t *j, int n_stmts, int n_branches, int n_toggles,
                         int n_expressions)
{
   const int counts[4] = { n_stmts, n_branches, n_toggles, n_expressions };
   for (int i = 0; i < 4; i++) {
      if (counts[i] == 0)
         continue;
      else if (j->cover_mem[i] == NULL)
         j->cover_mem[i] = xcalloc_array(counts[i], sizeof(int32_t));
      else
         j->cover_mem[i] = xrealloc_array(j->cover_mem[i],
                                          counts[i], sizeof(int32_t));
   }
}

int32_t *jit_get_cover_mem(jit_t *j, jit_cover_mem_t kind)
{
   assert(kind < ARRAY_LEN(j->cover_mem));
   return j->cover_mem[kind];
}

int32_t *jit_get_cover_ptr(jit_t *j, jit_value_t addr)
{
   assert(addr.kind == JIT_ADDR_COVER);
   int32_t *base = jit_get_cover_mem(j, addr.int64 & 3);
   assert(base != NULL);
   return base + (addr.int64 >> 2);
}

object_t *jit_get_locus(jit_value_t value)
{
   assert(value.kind == JIT_VALUE_LOCUS);
   return object_from_locus(value.ident, value.disp, lib_load_handler);
}

void jit_interrupt(jit_t *j, jit_irq_fn_t fn, void *ctx)
{
   relaxed_store(&j->interrupt_ctx, ctx);
   if (!atomic_cas(&j->interrupt, NULL, fn))
      fatal_exit(1);
}

__attribute__((cold, noinline))
static void jit_handle_interrupt(jit_t *j)
{
   jit_irq_fn_t fn = load_acquire(&j->interrupt);
   if (!atomic_cas(&j->interrupt, fn, NULL))
      return;

   (*fn)(j, relaxed_load(&j->interrupt_ctx));
}

void jit_check_interrupt(jit_t *j)
{
   if (unlikely(relaxed_load(&j->interrupt) != NULL))
      jit_handle_interrupt(j);
}

void jit_reset(jit_t *j)
{
   SCOPED_LOCK(j->lock);

   for (int i = 0; i < j->next_handle; i++) {
      jit_func_t *f = j->funcs->items[i];
      if (f->privdata != MPTR_INVALID)
         *mptr_get(f->privdata) = NULL;
   }
}
