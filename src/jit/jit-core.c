//
//  Copyright (C) 2022-2025  Nick Gasson
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
#include "cov/cov-api.h"
#include "debug.h"
#include "diag.h"
#include "hash.h"
#include "jit/jit-priv.h"
#include "jit/jit.h"
#include "lib.h"
#include "lower.h"
#include "mir/mir-unit.h"
#include "object.h"
#include "option.h"
#include "rt/model.h"
#include "rt/mspace.h"
#include "rt/structs.h"
#include "thread.h"
#include "tree.h"
#include "type.h"
#include "vcode.h"
#include "vlog/vlog-node.h"

#include <assert.h>
#include <ctype.h>
#include <inttypes.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#define FUNC_HASH_SZ    1024
#define FUNC_LIST_SZ    512
#define COMPILE_TIMEOUT 10000

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
   chash_t          *index;
   mspace_t         *mspace;
   void             *lower_ctx;
   bool              silent;
   bool              shutdown;
   int               exit_status;
   jit_tier_t       *tiers;
   jit_pack_t       *pack;
   func_array_t     *funcs;
   unsigned          next_handle;
   nvc_lock_t        lock;
   jit_irq_fn_t      interrupt;
   void             *interrupt_ctx;
   unit_registry_t  *registry;
   mir_context_t    *mir;
   cover_data_t     *cover;
} jit_t;

static void jit_transition(jit_t *j, jit_state_t from, jit_state_t to);

static void jit_oom_cb(mspace_t *m, size_t size)
{
   diag_t *d = diag_new(DIAG_FATAL, NULL);
   diag_printf(d, "out of memory attempting to allocate %zu byte object", size);

   const size_t heapsize = opt_get_size(OPT_HEAP_SIZE);
   diag_hint(d, NULL, "the current heap size is %zu bytes which you can "
             "increase with the $bold$-H$$ option, for example $bold$-H %zum$$",
             heapsize, MAX(1, (heapsize * 2) / 1024 / 1024));

   diag_emit(d);
   jit_abort_with_status(EXIT_FAILURE);
}

static inline jit_thread_local_t **jit_thread_local_ptr(void)
{
#ifdef USE_EMUTLS
   static jit_thread_local_t *local = NULL;
   assert(thread_id() == 0);
#else
   static __thread jit_thread_local_t *local = NULL;
#endif

   return &local;
}

jit_thread_local_t *jit_thread_local(void)
{
   jit_thread_local_t **ptr = jit_thread_local_ptr();

   if (unlikely(*ptr == NULL)) {
      *ptr = xcalloc(sizeof(jit_thread_local_t));
      (*ptr)->state = JIT_IDLE;
   }

   return *ptr;
}

jit_t *jit_new(unit_registry_t *ur, mir_context_t *mc, cover_data_t *db)
{
   jit_t *j = xcalloc(sizeof(jit_t));
   j->registry    = ur;
   j->index       = chash_new(FUNC_HASH_SZ);
   j->mspace      = mspace_new(opt_get_size(OPT_HEAP_SIZE));
   j->exit_status = INT_MIN;
   j->mir         = mc;
   j->cover       = db;

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

   if (j->pack != NULL)
      jit_pack_free(j->pack);

   for (int i = 0; i < j->next_handle; i++)
      jit_free_func(j->funcs->items[i]);
   free(j->funcs);

   for (jit_tier_t *it = j->tiers, *tmp; it; it = tmp) {
      tmp = it->next;
      (*it->plugin.cleanup)(it->context);
      free(it);
   }

   mspace_destroy(j->mspace);
   chash_free(j->index);
   free(j);
}

bool jit_is_shutdown(jit_t *j)
{
   return load_acquire(&j->shutdown);
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
}

static jit_handle_t jit_lazy_compile_locked(jit_t *j, ident_t name)
{
   assert_lock_held(&j->lock);

   jit_func_t *f = chash_get(j->index, name);
   if (f != NULL)
      return f->handle;

   jit_entry_fn_t entry = jit_bind_intrinsic(name) ?: jit_interp;

   f = xcalloc(sizeof(jit_func_t));
   f->name      = name;
   f->state     = JIT_FUNC_PLACEHOLDER;
   f->jit       = j;
   f->handle    = j->next_handle++;
   f->next_tier = j->tiers;
   f->hotness   = f->next_tier ? f->next_tier->threshold : 0;
   f->entry     = entry;

   // Install now to allow circular references in relocations
   jit_install(j, f);

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

__attribute__((noreturn))
static void jit_missing_unit(jit_func_t *f)
{
   tree_t unit = lib_get_qualified(f->name);
   if (unit != NULL && tree_kind(unit) == T_PACKAGE)
      jit_msg(tree_loc(unit), DIAG_FATAL, "missing body for package %s",
              istr(f->name));
   else
      jit_msg(NULL, DIAG_FATAL, "missing body for %s", istr(f->name));

   __builtin_unreachable();
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
   case JIT_FUNC_ERROR:
      jit_missing_unit(f);
      break;
   default:
      fatal_trace("illegal function state for %s", istr(f->name));
   }

   assert(f->irbuf == NULL);

#ifndef USE_EMUTLS
   const jit_state_t oldstate = jit_thread_local()->state;
   jit_transition(f->jit, oldstate, JIT_COMPILING);
#endif

   if (f->jit->pack != NULL && jit_pack_fill(f->jit->pack, f->jit, f)) {
      // TODO: this should happen before the store_release to f->state
      f->counters = cover_get_counters(f->jit->cover, f->name);
      goto done;
   }

   mir_unit_t *mu = mir_get_unit(f->jit->mir, f->name);

   if (mu == NULL && f->jit->registry != NULL) {
      // Unit registry and MIR import is not thread-safe
      SCOPED_LOCK(f->jit->lock);
      (void)unit_registry_get(f->jit->registry, f->name);
      mu = mir_get_unit(f->jit->mir, f->name);
   }

   if (mu == NULL) {
      store_release(&(f->state), JIT_FUNC_ERROR);
      jit_missing_unit(f);
   }

   f->counters = cover_get_counters(f->jit->cover, f->name);

   jit_irgen(f, mu);

 done:
#ifndef USE_EMUTLS
   jit_transition(f->jit, JIT_COMPILING, oldstate);
#endif
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

   jit_fill_irbuf(f);

   tlab_t tlab = jit_null_tlab(j);
   jit_scalar_t p1 = { .pointer = NULL }, p2 = p1, result;
   if (!jit_fastcall(j, f->handle, &result, p1, p2, &tlab)) {
      error_at(&(f->object->loc), "failed to initialise %s", istr(f->name));
      result.pointer = NULL;
   }
   else if (result.pointer == NULL)
      fatal_trace("link %s returned NULL", istr(f->name));
   else if (f->privdata == MPTR_INVALID)
      fatal_trace("cannot link unit %s", istr(f->name));

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

void *jit_get_frame_var(jit_t *j, jit_handle_t handle, ident_t name)
{
   jit_func_t *f = jit_get_func(j, handle);
   if (f->privdata == MPTR_INVALID)
      fatal_trace("%s not linked", istr(f->name));

   jit_fill_irbuf(f);

   for (int i = 0; i < f->nvars; i++) {
      if (f->linktab[i].name == name)
         return *mptr_get(f->privdata) + f->linktab[i].offset;
   }

   fatal_trace("%s has no variable %s", istr(f->name), istr(name));
}

static void jit_emit_trace(diag_t *d, const loc_t *loc, object_t *enclosing,
                           const char *symbol)
{
   tree_t tree = tree_from_object(enclosing);
   if (tree != NULL) {
      switch (tree_kind(tree)) {
      case T_PROCESS:
         {
            rt_proc_t *proc = get_active_proc();
            const char *name = istr(proc ? proc->name : tree_ident(tree));
            diag_trace(d, loc, "Process$$ %s", name);
         }
         break;
      case T_ATTR_SPEC:
         assert(ident_casecmp(tree_ident(tree), well_known(W_FOREIGN)));
         diag_trace(d, loc, "Subprogram$$ %s",
                    type_pp(tree_type(tree_ref(tree))));
         break;
      case T_FUNC_BODY:
      case T_FUNC_DECL:
         diag_trace(d, loc, "Function$$ %s", type_pp(tree_type(tree)));
         break;
      case T_PROC_BODY:
      case T_PROC_DECL:
         diag_trace(d, loc, "Procedure$$ %s", type_pp(tree_type(tree)));
         break;
      case T_TYPE_DECL:
         if (strstr(symbol, "$value"))
            diag_trace(d, loc, "Attribute$$ %s'VALUE", istr(tree_ident(tree)));
         else
            diag_trace(d, loc, "Type$$ %s", istr(tree_ident(tree)));
         break;
      case T_BLOCK:
         diag_trace(d, loc, "Process$$ (init)");
         break;
      case T_PACKAGE:
      case T_PACK_BODY:
      case T_PACK_INST:
         diag_trace(d, loc, "Package$$ %s", istr(tree_ident(tree)));
         break;
      case T_INERTIAL:
         diag_trace(d, loc, "Equivalent process");
         break;
      case T_TYPE_CONV:
         diag_trace(d, loc, "Type conversion %s", type_pp(tree_type(tree)));
         break;
      case T_PARAM_DECL:
         diag_trace(d, loc, "Parameter %s", istr(tree_ident(tree)));
         break;
      case T_PARAM:
         diag_trace(d, loc, "Port conversion");
         break;
      default:
         {
            const char *class = class_str(class_of(tree));
            diag_trace(d, loc, "%c%s %s", toupper_iso88591(class[0]),
                       class + 1, istr(tree_ident(tree)));
         }
         break;
      }
   }

   vlog_node_t v = vlog_from_object(enclosing);
   if (v != NULL) {
      switch (vlog_kind(v)) {
      case V_INITIAL:
         diag_trace(d, loc, "Initial procedure %s", istr(vlog_ident(v)));
         break;
      case V_ALWAYS:
         diag_trace(d, loc, "Always procedure %s", istr(vlog_ident(v)));
         break;
      default:
         diag_trace(d, loc, "%s", istr(vlog_ident(v)));
      }
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
      frame->loc    = LOC_INVALID;
      frame->symbol = a->func->name;
      frame->object = NULL;

#ifdef USE_EMUTLS
      if (load_acquire(&a->func->state) == JIT_FUNC_COMPILING)
         continue;   // Cannot use jit_transition in jit_fill_irbuf
#endif

      jit_fill_irbuf(a->func);

      frame->object = a->func->object;

      // Scan backwards to find the last debug info
      assert(a->irpos < a->func->nirs);
      frame->loc = frame->object ? frame->object->loc : LOC_INVALID;
      for (jit_ir_t *ir = &(a->func->irbuf[a->irpos]);
           ir >= a->func->irbuf; ir--) {
         if (ir->op == J_DEBUG) {
            frame->loc = ir->arg1.loc;
            break;
         }
         else if (ir->target)
            break;
      }
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
   else if (unlikely(jit_thread_local()->state != JIT_RUNNING))
      fatal_trace("JIT diag callback called when not running");

   jit_stack_trace_t *stack LOCAL = jit_stack_trace();

   for (int i = 0; i < stack->count; i++) {
      jit_frame_t *frame = &(stack->frames[i]);

      if (frame->object != NULL)
         jit_emit_trace(d, &(frame->loc), frame->object, istr(frame->symbol));
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
      if (from != JIT_RUNNING) {
         diag_add_hint_fn(jit_diag_cb, j);
         thread->jit = j;
      }
      else
         assert(thread->jit == j);
      break;
   case JIT_IDLE:
      if (from == JIT_RUNNING) {
         diag_remove_hint_fn(jit_diag_cb);
         thread->jit = NULL;
      }
      break;
   case JIT_COMPILING:
      if (from == JIT_RUNNING) {
         diag_remove_hint_fn(jit_diag_cb);
         thread->jit = NULL;
      }
      break;
   }
}

static bool jit_try_vcall(jit_t *j, jit_func_t *f, jit_scalar_t *args,
                          tlab_t *tlab)
{
   jit_thread_local_t *volatile thread = jit_thread_local();
   volatile const jit_state_t oldstate = thread->state;

   const int rc = jit_setjmp(thread->abort_env);
   if (rc == 0) {
      thread->jmp_buf_valid = 1;
      jit_transition(j, oldstate, JIT_RUNNING);

      jit_entry_fn_t entry = load_acquire(&f->entry);
      (*entry)(f, NULL, args, tlab);

      jit_transition(j, JIT_RUNNING, oldstate);
      thread->jmp_buf_valid = 0;
      thread->anchor = NULL;
      return true;
   }
   else {
      jit_transition(j, JIT_RUNNING, oldstate);
      thread->jmp_buf_valid = 0;
      thread->anchor = NULL;
      return false;
   }
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
      case FFI_UINT8:
      case FFI_UINT16:
      case FFI_UINT32:
         args[wptr++].integer = va_arg(ap, uint32_t);
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

   tlab_t tlab = jit_null_tlab(j);
   if (!jit_try_vcall(j, f, args, &tlab))
      return false;

   *result = args[0];
   return true;
}

jit_scalar_t jit_call(jit_t *j, jit_handle_t handle, ...)
{
   va_list ap;
   va_start(ap, handle);

   jit_func_t *f = jit_get_func(j, handle);

   jit_scalar_t args[JIT_MAX_ARGS];
   jit_unpack_args(f, args, ap);

   va_end(ap);

   tlab_t tlab = jit_null_tlab(j);

   if (!jit_try_vcall(j, f, args, &tlab))
      fatal_trace("call to %s failed", istr(jit_get_func(j, handle)->name));

   return args[0];
}

bool jit_fastcall(jit_t *j, jit_handle_t handle, jit_scalar_t *result,
                  jit_scalar_t p1, jit_scalar_t p2, tlab_t *tlab)
{
   jit_func_t *f = jit_get_func(j, handle);

   jit_scalar_t args[JIT_MAX_ARGS];
   args[0] = p1;
   args[1] = p2;

   if (!jit_try_vcall(j, f, args, tlab))
      return false;

   *result = args[0];
   return true;
}

bool jit_vfastcall(jit_t *j, jit_handle_t handle, const jit_scalar_t *inargs,
                   unsigned nargs, jit_scalar_t *results, unsigned nresults,
                   tlab_t *tlab)
{
   jit_func_t *f = jit_get_func(j, handle);

   jit_scalar_t args[JIT_MAX_ARGS];
   memcpy(args, inargs, nargs * sizeof(jit_scalar_t));

   if (!jit_try_vcall(j, f, args, tlab))
      return false;

   memcpy(results, args, nresults * sizeof(jit_scalar_t));
   return true;
}

void *jit_call_thunk(jit_t *j, vcode_unit_t unit, void *context,
                     thunk_result_fn_t fn, void *arg)
{
   assert(vcode_unit_kind(unit) == VCODE_UNIT_THUNK);

   jit_func_t *f = xcalloc(sizeof(jit_func_t));
   f->state  = JIT_FUNC_COMPILING;
   f->jit    = j;
   f->handle = JIT_HANDLE_INVALID;
   f->entry  = jit_interp;
   f->object = vcode_unit_object(unit);

   mir_unit_t *mu = mir_import(f->jit->mir, unit);
   jit_irgen(f, mu);
   mir_unit_free(mu);

   jit_scalar_t args[JIT_MAX_ARGS];
   args[0].pointer = context;

   tlab_t tlab = jit_null_tlab(j);

   void *user = NULL;
   if (jit_try_vcall(j, f, args, &tlab))
      user = (*fn)(args, arg);

   jit_free_func(f);
   return user;
}

void *jit_call_thunk2(jit_t *j, mir_unit_t *mu, void *context,
                      thunk_result_fn_t fn, void *arg)
{
   assert(mir_get_kind(mu) == MIR_UNIT_THUNK);

   jit_func_t *f = xcalloc(sizeof(jit_func_t));
   f->state  = JIT_FUNC_COMPILING;
   f->jit    = j;
   f->handle = JIT_HANDLE_INVALID;
   f->entry  = jit_interp;
   f->object = mir_get_object(mu);

   jit_irgen(f, mu);

   jit_scalar_t args[JIT_MAX_ARGS];
   args[0].pointer = context;

   tlab_t tlab = jit_null_tlab(j);

   void *user = NULL;
   if (jit_try_vcall(j, f, args, &tlab) && fn != NULL)
      user = (*fn)(args, arg);

   jit_free_func(f);
   return user;
}

tlab_t jit_null_tlab(jit_t *j)
{
   tlab_t t = { .mspace = j->mspace };
   return t;
}

void jit_set_silent(jit_t *j, bool silent)
{
   j->silent = silent;
}

void jit_load_pack(jit_t *j, FILE *f)
{
   assert(j->pack == NULL);
   j->pack = jit_read_pack(f);
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
      jit_abort_with_status(EXIT_FAILURE);
}

void jit_abort(void)
{
   jit_thread_local_t *thread = jit_thread_local();

   switch (thread->state) {
   case JIT_IDLE:
   case JIT_COMPILING:
      fatal_exit(1);
      break;
   case JIT_RUNNING:
      if (thread->jmp_buf_valid)
         jit_longjmp(thread->abort_env, 1);
      else {
         const int code = atomic_load(&thread->jit->exit_status);
         fatal_exit(code);
      }
      break;
   }

   __builtin_unreachable();
}

void jit_abort_with_status(int code)
{
   assert(code != INT_MIN);

   jit_thread_local_t *thread = jit_thread_local();
   if (thread->jit != NULL)
      atomic_store(&(thread->jit->exit_status), code);

   jit_abort();
}

void jit_reset_exit_status(jit_t *j)
{
   atomic_store(&(j->exit_status), INT_MIN);
}

bool jit_exit_status(jit_t *j, int *status)
{
   if (j->exit_status != INT_MIN) {
      *status = j->exit_status;
      return true;
   }
   else
      return false;
}

static void jit_async_cgen(void *context, void *arg)
{
   jit_func_t *f = context;
   jit_tier_t *tier = arg;

   if (!jit_is_shutdown(f->jit))
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

object_t *jit_get_object(jit_t *j, jit_handle_t handle)
{
   jit_func_t *f = jit_get_func(j, handle);
   jit_fill_irbuf(f);
   return f->object;
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

   {
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
   }

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
      { "SHR",     J_SHR,        1, 2 },
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
                    && strcmp(optab[ipos].name, tok); ipos++);

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
         || ir->arg1.exit == JIT_EXIT_RANGE_FAIL;
   }
   else
      return ir->op == J_TRAP;
}

int32_t *jit_get_cover_ptr(jit_func_t *f, jit_value_t addr)
{
   assert(addr.kind == JIT_ADDR_COVER);
   assert(f->counters != NULL);
   assert(addr.int64 >= 0 && addr.int64 <= UINT_MAX);
   return f->counters + addr.int64;
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

   jit_reset_exit_status(j);
}

jit_t *jit_for_thread(void)
{
   jit_thread_local_t *thread = jit_thread_local();

   if (unlikely(thread->jit == NULL))
      fatal_trace("thread not attached to JIT");

   return thread->jit;
}

jit_thread_local_t *jit_attach_thread(jit_anchor_t *anchor)
{
   jit_thread_local_t *thread = *jit_thread_local_ptr();
   assert(thread != NULL);

   thread->anchor = anchor;

   jit_check_interrupt(thread->jit);

   return thread;
}
