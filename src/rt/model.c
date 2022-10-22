//
//  Copyright (C) 2011-2022  Nick Gasson
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
#include "cover.h"
#include "hash.h"
#include "jit/jit.h"
#include "lib.h"
#include "opt.h"
#include "rt/alloc.h"
#include "rt/heap.h"
#include "rt/model.h"
#include "rt/structs.h"
#include "thread.h"
#include "tree.h"
#include "type.h"

#include <assert.h>
#include <inttypes.h>
#include <stdlib.h>
#include <string.h>

typedef struct _callback callback_t;
typedef struct _memblock memblock_t;

typedef struct _callback {
   rt_event_fn_t  fn;
   void          *user;
   callback_t    *next;
} callback_t;

#define MEMBLOCK_LINE_SZ 64
#define MEMBLOCK_PAGE_SZ 0x200000

typedef struct _memblock {
   memblock_t *chain;
   unsigned    free;
   unsigned    pagesz;
   char       *ptr;
} memblock_t;

typedef struct _rt_model {
   tree_t             top;
   hash_t            *scopes;
   rt_scope_t        *root;
   mspace_t          *mspace;
   jit_t             *jit;
   rt_nexus_t        *nexuses;
   rt_nexus_t       **nexus_tail;
   unsigned           stop_delta;
   int                iteration;
   uint64_t           now;
   bool               can_create_delta;
   bool               next_is_delta;
   bool               force_stop;
   unsigned           n_signals;
   heap_t            *eventq_heap;
   ihash_t           *res_memo;
   rt_alloc_stack_t   event_stack;
   rt_alloc_stack_t   sens_list_stack;
   rt_alloc_stack_t   watch_stack;
   rt_alloc_stack_t   callback_stack;
   rt_watch_t        *watches;
   workq_t           *procq;
   workq_t           *delta_procq;
   workq_t           *driverq;
   workq_t           *delta_driverq;
   workq_t           *effq;
   workq_t           *postponedq;
   workq_t           *implicitq;
   callback_t        *global_cbs[RT_LAST_EVENT];
   cover_tagging_t   *cover;
   nvc_rusage_t       ready_rusage;
   memblock_t        *memblocks;
} rt_model_t;

#define FMT_VALUES_SZ   128
#define NEXUS_INDEX_MIN 8
#define TRACE_SIGNALS   1

#define TRACE(...) do {                                 \
      if (unlikely(__trace_on))                         \
         __model_trace(get_model(), __VA_ARGS__);       \
   } while (0)

#define FOR_ALL_SIZES(size, macro) do {                 \
      switch (size) {                                   \
      case 1:                                           \
         macro(uint8_t); break;                         \
      case 2:                                           \
         macro(uint16_t); break;                        \
      case 4:                                           \
         macro(uint32_t); break;                        \
      case 8:                                           \
         macro(uint64_t); break;                        \
      }                                                 \
   } while (0)

#define MODEL_ENTRY(m)                                                  \
   rt_model_t *__save __attribute__((unused, cleanup(__model_exit)));   \
   __model_entry(m, &__save);                                           \

static __thread rt_proc_t    *active_proc = NULL;
static __thread rt_scope_t   *active_scope = NULL;
static __thread rt_signal_t **signals_tail = NULL;
static __thread rt_scope_t  **scopes_tail = NULL;
static __thread rt_model_t   *__model = NULL;
static __thread tlab_t        spare_tlab = {};
static __thread waveform_t   *free_waveforms = NULL;

DLLEXPORT tlab_t __nvc_tlab = {};   // TODO: this should be thread-local

static bool __trace_on = false;

static void *driving_value(rt_nexus_t *nexus);
static void *source_value(rt_nexus_t *nexus, rt_source_t *src);
static void free_value(rt_nexus_t *n, rt_value_t v);
static rt_nexus_t *clone_nexus(rt_model_t *m, rt_nexus_t *old, int offset,
                               rt_net_t *net);
static void update_implicit_signal(rt_model_t *m, rt_implicit_t *imp);
static void async_run_process(void *context, void *arg);
static void async_update_driver(void *context, void *arg);
static void async_update_driving(void *context, void *arg);
static void async_disconnect(void *context, void *arg);

static int fmt_now(rt_model_t *m, char *buf, size_t len)
{
   if (m->iteration < 0)
      return checked_sprintf(buf, len, "(init)");
   else {
      char *p = buf;
      p += fmt_time_r(p, buf + len - p, m->now);
      p += checked_sprintf(p, buf + len - p, "+%d", m->iteration);
      return p - buf;
   }
}

__attribute__((format(printf, 2, 3)))
static void __model_trace(rt_model_t *m, const char *fmt, ...)
{
   va_list ap;
   va_start(ap, fmt);

   char buf[64];
   fmt_now(m, buf, sizeof(buf));

   static nvc_lock_t lock = 0;
   {
      SCOPED_LOCK(lock);

      fprintf(stderr, "TRACE %s: ", buf);
      vfprintf(stderr, fmt, ap);
      fprintf(stderr, "\n");
      fflush(stderr);
   }

   va_end(ap);
}

static void model_diag_cb(diag_t *d, void *arg)
{
   rt_model_t *m = arg;

   char tmbuf[64];
   fmt_now(m, tmbuf, sizeof(tmbuf));

   diag_printf(d, "%s: ", tmbuf);
}

static void __model_entry(rt_model_t *m, rt_model_t **save)
{
   if (__model == NULL)
      diag_add_hint_fn(model_diag_cb, m);

   *save = __model;
   __model = m;
}

static void __model_exit(rt_model_t **save)
{
   __model = *save;
   *save = NULL;

   if (__model == NULL)
      diag_remove_hint_fn(model_diag_cb);
}

static char *fmt_values_r(const void *values, size_t len, char *buf, size_t max)
{
   char *p = buf;
   const uint8_t *vptr = values;

   for (unsigned i = 0; i < len; i++) {
      if (buf + max - p <= 5) {
         checked_sprintf(p, buf + max - p, "...");
         break;
      }
      else
         p += checked_sprintf(p, buf + max - p, "%02x", *vptr++);
   }

   return buf;
}

static const char *fmt_nexus(rt_nexus_t *n, const void *values)
{
   static char buf[FMT_VALUES_SZ*2 + 2];
   return fmt_values_r(values, n->size * n->width, buf, sizeof(buf));
}

static const char *fmt_values(const void *values, uint32_t len)
{
   static char buf[FMT_VALUES_SZ*2 + 2];
   return fmt_values_r(values, len, buf, sizeof(buf));
}

static void *static_alloc(rt_model_t *m, size_t size)
{
   const int nlines = ALIGN_UP(size, MEMBLOCK_LINE_SZ) / MEMBLOCK_LINE_SZ;

   memblock_t *mb = m->memblocks;
   if (mb == NULL || mb->free < nlines) {
      mb = xmalloc(sizeof(memblock_t));
      mb->pagesz = MAX(MEMBLOCK_PAGE_SZ, nlines * MEMBLOCK_LINE_SZ);
      mb->chain  = m->memblocks;
      mb->free   = mb->pagesz / MEMBLOCK_LINE_SZ;
      mb->ptr    = nvc_memalign(MEMBLOCK_LINE_SZ, mb->pagesz);

      m->memblocks = mb;
   }

   assert(nlines <= mb->free);

   void *ptr = mb->ptr + mb->pagesz - mb->free * MEMBLOCK_LINE_SZ;
   mb->free -= nlines;
   return ptr;
}

static void global_event(rt_model_t *m, rt_event_t kind)
{
   callback_t *list = m->global_cbs[kind];
   m->global_cbs[kind] = NULL;

   for (callback_t *it = list, *tmp; it; it = tmp) {
      tmp = it->next;
      (*it->fn)(m, it->user);
      rt_free(m->callback_stack, it);
   }
}

static void scope_deps_cb(ident_t unit_name, void *__ctx)
{
   // TODO: this should be redundant now we have the package init op

   rt_model_t *m = __ctx;

   tree_t unit = lib_get_qualified(unit_name);
   if (unit == NULL) {
      warnf("missing dependency %s", istr(unit_name));
      return;
   }

   if (hash_get(m->scopes, unit) != NULL)
      return;

   const tree_kind_t kind = tree_kind(unit);
   if ((kind != T_PACKAGE && kind != T_PACK_INST)
       || is_uninstantiated_package(unit)) {
      tree_walk_deps(unit, scope_deps_cb, m);
      return;
   }

   rt_scope_t *s = xcalloc(sizeof(rt_scope_t));
   s->where    = unit;
   s->name     = tree_ident(unit);
   s->kind     = SCOPE_PACKAGE;
   s->privdata = mptr_new(m->mspace, "package privdata");

   hash_put(m->scopes, unit, s);

   tree_walk_deps(unit, scope_deps_cb, m);

   if (kind == T_PACKAGE) {
      tree_t body = body_of(unit);
      if (body != NULL)
         tree_walk_deps(body, scope_deps_cb, m);
   }

   *scopes_tail = s;
   scopes_tail = &(s->chain);
}

static rt_scope_t *scope_for_block(rt_model_t *m, tree_t block, ident_t prefix)
{
   rt_scope_t *s = xcalloc(sizeof(rt_scope_t));
   s->where    = block;
   s->name     = ident_prefix(prefix, tree_ident(block), '.');
   s->kind     = SCOPE_INSTANCE;
   s->privdata = mptr_new(m->mspace, "block privdata");

   hash_put(m->scopes, block, s);

   rt_scope_t **childp = &(s->child);
   rt_proc_t **procp = &(s->procs);

   tree_t hier = tree_decl(block, 0);
   assert(tree_kind(hier) == T_HIER);

   ident_t path = tree_ident(hier);

   const int ndecls = tree_decls(block);
   for (int i = 0; i < ndecls; i++) {
      tree_t d = tree_decl(block, i);
      if (tree_kind(d) == T_PACK_INST) {
         rt_scope_t *p = xcalloc(sizeof(rt_scope_t));
         p->where    = d;
         p->name     = ident_prefix(s->name, tree_ident(d), '.');
         p->kind     = SCOPE_PACKAGE;
         p->privdata = mptr_new(m->mspace, "pack inst privdata");

         hash_put(m->scopes, d, p);

         *childp = p;
         childp = &(p->chain);
      }
   }

   const int nstmts = tree_stmts(block);
   for (int i = 0; i < nstmts; i++) {
      tree_t t = tree_stmt(block, i);
      switch (tree_kind(t)) {
      case T_BLOCK:
         {
            rt_scope_t *c = scope_for_block(m, t, s->name);
            c->parent = s;

            *childp = c;
            childp = &(c->chain);
         }
         break;

      case T_PROCESS:
         {
            ident_t name = tree_ident(t);
            ident_t sym = ident_prefix(s->name, name, '.');

            rt_proc_t *p = xcalloc(sizeof(rt_proc_t));
            p->where     = t;
            p->name      = ident_prefix(path, ident_downcase(name), ':');
            p->handle    = jit_lazy_compile(m->jit, sym);
            p->scope     = s;
            p->privdata  = mptr_new(m->mspace, "process privdata");

            p->wakeable.kind       = W_PROC;
            p->wakeable.wakeup_gen = 0;
            p->wakeable.pending    = false;
            p->wakeable.postponed  = !!(tree_flags(t) & TREE_F_POSTPONED);

            *procp = p;
            procp = &(p->chain);
         }
         break;

      default:
         break;
      }
   }

   return s;
}

rt_model_t *model_new(tree_t top, jit_t *jit)
{
   rt_model_t *m = xcalloc(sizeof(rt_model_t));
   m->top         = top;
   m->scopes      = hash_new(256);
   m->mspace      = jit_get_mspace(jit);
   m->jit         = jit;
   m->nexus_tail  = &(m->nexuses);
   m->iteration   = -1;
   m->stop_delta  = opt_get_int(OPT_STOP_DELTA);
   m->eventq_heap = heap_new(512);
   m->res_memo    = ihash_new(128);

   m->can_create_delta = true;

   m->event_stack     = rt_alloc_stack_new(sizeof(event_t), "event");
   m->sens_list_stack = rt_alloc_stack_new(sizeof(sens_list_t), "sens_list");
   m->watch_stack     = rt_alloc_stack_new(sizeof(rt_watch_t), "watch");
   m->callback_stack  = rt_alloc_stack_new(sizeof(callback_t), "callback");

   m->root = xcalloc(sizeof(rt_scope_t));
   m->root->kind     = SCOPE_ROOT;
   m->root->where    = top;
   m->root->privdata = mptr_new(m->mspace, "root privdata");

   m->procq         = workq_new(m);
   m->delta_procq   = workq_new(m);
   m->postponedq    = workq_new(m);
   m->driverq       = workq_new(m);
   m->delta_driverq = workq_new(m);
   m->effq          = workq_new(m);

   scopes_tail = &(m->root->child);
   tree_walk_deps(top, scope_deps_cb, m);

   *scopes_tail = scope_for_block(m, tree_stmt(top, 0), lib_name(lib_work()));

   __trace_on = opt_get_int(OPT_RT_TRACE);

   nvc_rusage(&m->ready_rusage);

   return m;
}

rt_model_t *get_model(void)
{
   assert(__model != NULL);
   return __model;
}

rt_model_t *get_model_or_null(void)
{
   return __model;
}

rt_proc_t *get_active_proc(void)
{
   return active_proc;
}

static void free_waveform(waveform_t *w)
{
   w->next = free_waveforms;
   free_waveforms = w;
}

static void free_sens_list(rt_model_t *m, sens_list_t **l)
{
   for (sens_list_t *it = *l, *tmp; it; it = tmp) {
      tmp = it->next;
      rt_free(m->sens_list_stack, it);
   }
   *l = NULL;
}

static void cleanup_nexus(rt_model_t *m, rt_nexus_t *n)
{
   for (rt_source_t *s = &(n->sources), *tmp; s; s = tmp) {
      tmp = s->chain_input;

      switch (s->tag) {
      case SOURCE_DRIVER:
         for (waveform_t *it = s->u.driver.waveforms.next, *next;
              it; it = next) {
            next = it->next;
            free_waveform(it);
         }
         break;

      case SOURCE_PORT:
         if (s->u.port.conv_func != NULL) {
            assert(s->u.port.conv_func->refcnt > 0);
            if (--(s->u.port.conv_func->refcnt) == 0)
               free(s->u.port.conv_func);
         }
         break;
      }
   }

   if (n->net != NULL)
      free_sens_list(m, &(n->net->pending));
}

static void cleanup_signal(rt_model_t *m, rt_signal_t *s)
{
   rt_nexus_t *n = &(s->nexus), *tmp;
   for (int i = 0; i < s->n_nexus; i++, n = tmp) {
      tmp = n->chain;
      cleanup_nexus(m, n);
   }

   free(s->index);
}

static void cleanup_scope(rt_model_t *m, rt_scope_t *scope)
{
   for (rt_proc_t *it = scope->procs, *tmp; it; it = tmp) {
      tmp = it->chain;
      mptr_free(m->mspace, &(it->privdata));
      tlab_release(&(it->tlab));
      free(it);
   }

   for (rt_signal_t *it = scope->signals, *tmp; it; it = tmp) {
      tmp = it->chain;
      cleanup_signal(m, it);
   }

   for (rt_alias_t *it = scope->aliases, *tmp; it; it = tmp) {
      tmp = it->chain;
      free(it);
   }

   for (rt_scope_t *it = scope->child, *tmp; it; it = tmp) {
      tmp = it->chain;
      cleanup_scope(m, it);
   }

   mptr_free(m->mspace, &(scope->privdata));
   free(scope);
}

void model_free(rt_model_t *m)
{
   if (opt_get_int(OPT_RT_STATS)) {
      nvc_rusage_t ru;
      nvc_rusage(&ru);

      unsigned mem = 0;
      for (memblock_t *mb = m->memblocks; mb; mb = mb->chain)
         mem += mb->pagesz - (MEMBLOCK_LINE_SZ * mb->free);

      notef("setup:%ums run:%ums maxrss:%ukB static:%ukB",
            m->ready_rusage.ms, ru.ms, ru.rss, mem / 1024);
   }

   while (heap_size(m->eventq_heap) > 0)
      rt_free(m->event_stack, heap_extract_min(m->eventq_heap));

   tlab_release(&__nvc_tlab);

   cleanup_scope(m, m->root);

   workq_free(m->procq);
   workq_free(m->delta_procq);
   workq_free(m->postponedq);
   workq_free(m->driverq);
   workq_free(m->delta_driverq);
   workq_free(m->effq);

   if (m->implicitq != NULL)
      workq_free(m->implicitq);

   for (rt_watch_t *it = m->watches, *tmp; it; it = tmp) {
      tmp = it->chain_all;
      rt_free(m->watch_stack, it);
   }

   for (int i = 0; i < RT_LAST_EVENT; i++) {
      for (callback_t *it = m->global_cbs[i], *tmp; it; it = tmp) {
         tmp = it->next;
         rt_free(m->callback_stack, it);
      }
   }

   rt_alloc_stack_destroy(m->event_stack);
   rt_alloc_stack_destroy(m->sens_list_stack);
   rt_alloc_stack_destroy(m->watch_stack);
   rt_alloc_stack_destroy(m->callback_stack);

   for (memblock_t *mb = m->memblocks, *tmp; mb; mb = tmp) {
      tmp = mb->chain;
      nvc_munmap(mb->ptr, MEMBLOCK_PAGE_SZ);
      free(mb);
   }

   heap_free(m->eventq_heap);
   hash_free(m->scopes);
   ihash_free(m->res_memo);
   free(m);
}

rt_signal_t *find_signal(rt_scope_t *scope, tree_t decl)
{
   for (rt_signal_t *s = scope->signals; s; s = s->chain) {
      if (s->where == decl)
         return s;
   }

   for (rt_alias_t *a = scope->aliases; a; a = a->chain) {
      if (a->where == decl)
         return a->signal;
   }

   return NULL;
}

rt_scope_t *find_scope(rt_model_t *m, tree_t container)
{
   return hash_get(m->scopes, container);
}

rt_scope_t *child_scope(rt_scope_t *scope, tree_t decl)
{
   for (rt_scope_t *s = scope->child; s != NULL; s = s->chain) {
      if (s->where == decl)
         return s;
   }

   return NULL;
}

const void *signal_value(rt_signal_t *s)
{
   return s->shared.data;
}

size_t signal_expand(rt_signal_t *s, int offset, uint64_t *buf, size_t max)
{
   rt_nexus_t *n = &(s->nexus);
   for (; offset > 0; n = n->chain)
      offset -= n->width;
   assert(offset == 0);

   for (; n != NULL && offset < max; n = n->chain) {
#define SIGNAL_READ_EXPAND_U64(type) do {                               \
         const type *sp = (type *)n->resolved;                          \
         for (int j = 0; (j < n->width) && (offset + j < max); j++)     \
            buf[offset + j] = sp[j];                                    \
      } while (0)

      FOR_ALL_SIZES(n->size, SIGNAL_READ_EXPAND_U64);

      offset += n->width;
   }

   return offset;
}

size_t signal_string(rt_signal_t *s, const char *map, char *buf, size_t max)
{
   char *endp = buf + max;
   int offset = 0;
   rt_nexus_t *n = &(s->nexus);
   for (unsigned i = 0; i < s->n_nexus; i++, n = n->chain) {
      const char *vals = n->resolved;
      if (likely(map != NULL)) {
         for (int j = 0; j < n->width; j++) {
            if (buf + 1 < endp)
               *buf++ = map[(int)vals[j]];
         }
      }
      else {
         for (int j = 0; j < n->width; j++) {
            if (buf + 1 < endp)
               *buf++ = vals[j];
         }
      }

      if (buf < endp)
         *buf = '\0';

      offset += n->width;
   }

   return offset + 1;
}

static void deltaq_insert_proc(rt_model_t *m, uint64_t delta, rt_proc_t *wake)
{
   if (delta == 0) {
      assert(!wake->wakeable.pending);
      wake->wakeable.pending = true;
      ++(wake->wakeable.wakeup_gen);

      workq_do(m->delta_procq, async_run_process, wake);

      m->next_is_delta = true;
   }
   else {
      event_t *e = rt_alloc(m->event_stack);
      e->when            = m->now + delta;
      e->kind            = EVENT_PROCESS;
      e->proc.wakeup_gen = wake->wakeable.wakeup_gen;
      e->proc.proc       = wake;

      heap_insert(m->eventq_heap, e->when, e);
   }
}

static void deltaq_insert_driver(rt_model_t *m, uint64_t delta,
                                 rt_nexus_t *nexus, rt_source_t *source)
{
   if (delta == 0) {
      workq_do(m->delta_driverq, async_update_driver, source);
      m->next_is_delta = true;
   }
   else {
      event_t *e = rt_alloc(m->event_stack);
      e->when          = m->now + delta;
      e->kind          = EVENT_DRIVER;
      e->driver.nexus  = nexus;
      e->driver.source = source;

      heap_insert(m->eventq_heap, e->when, e);
   }
}

static void deltaq_insert_force_release(rt_model_t *m, uint64_t delta,
                                        rt_nexus_t *nexus)
{
   if (delta == 0) {
      workq_do(m->delta_driverq, async_update_driving, nexus);
      m->next_is_delta = true;
   }
   else {
      event_t *e = rt_alloc(m->event_stack);
      e->when          = m->now + delta;
      e->kind          = EVENT_DRIVER;
      e->driver.nexus  = nexus;
      e->driver.source = NULL;

      heap_insert(m->eventq_heap, e->when, e);
   }
}

static void deltaq_insert_disconnect(rt_model_t *m, uint64_t delta,
                                     rt_source_t *source)
{
   if (delta == 0) {
      workq_do(m->delta_driverq, async_disconnect, source);
      m->next_is_delta = true;
   }
   else {
      event_t *e = rt_alloc(m->event_stack);
      e->when          = m->now + delta;
      e->kind          = EVENT_DISCONNECT;
      e->driver.nexus  = source->u.driver.nexus;
      e->driver.source = source;

      heap_insert(m->eventq_heap, e->when, e);
   }
}

static void reset_process(rt_model_t *m, rt_proc_t *proc)
{
   TRACE("reset process %s", istr(proc->name));

   assert(!tlab_valid(proc->tlab));
   assert(!tlab_valid(__nvc_tlab));   // Not used during reset

   active_proc = proc;
   active_scope = proc->scope;

   jit_scalar_t context = {
      .pointer = mptr_get(m->mspace, proc->scope->privdata)
   };
   jit_scalar_t state = { .pointer = NULL };
   jit_scalar_t result;

   if (jit_fastcall(m->jit, proc->handle, &result, state, context))
      mptr_put(m->mspace, proc->privdata, result.pointer);
   else
      m->force_stop = true;
}

static void run_process(rt_model_t *m, rt_proc_t *proc)
{
   TRACE("run %sprocess %s", proc->privdata ? "" :  "stateless ",
         istr(proc->name));

   assert(!tlab_valid(spare_tlab));

   if (tlab_valid(proc->tlab)) {
      TRACE("using private TLAB at %p (%zu used)", proc->tlab.base,
            proc->tlab.alloc - proc->tlab.base);
      tlab_move(__nvc_tlab, spare_tlab);
      tlab_move(proc->tlab, __nvc_tlab);
   }
   else if (!tlab_valid(__nvc_tlab))
      tlab_acquire(m->mspace, &__nvc_tlab);

   active_proc = proc;
   active_scope = proc->scope;

   // Stateless processes have NULL privdata so pass a dummy pointer
   // value in so it can be distinguished from a reset
   jit_scalar_t state = {
      .pointer = mptr_get(m->mspace, proc->privdata) ?: (void *)-1
   };

   jit_scalar_t result;
   jit_scalar_t context = {
      .pointer = mptr_get(m->mspace, proc->scope->privdata)
   };

   if (!jit_fastcall(m->jit, proc->handle, &result, state, context))
      m->force_stop = true;

   active_proc = NULL;

   if (tlab_valid(__nvc_tlab)) {
      // The TLAB is still valid which means the process finished
      // instead of suspending at a wait statement and none of the data
      // inside it can be live anymore
      assert(!tlab_valid(proc->tlab));
      tlab_reset(__nvc_tlab);

      if (tlab_valid(spare_tlab))   // Surplus TLAB
         tlab_release(&spare_tlab);
   }
   else {
      // Process must have claimed TLAB or otherwise it would be lost
      assert(tlab_valid(proc->tlab));
      if (tlab_valid(spare_tlab))
         tlab_move(spare_tlab, __nvc_tlab);
   }
}

static void reset_scope(rt_model_t *m, rt_scope_t *s)
{
   if (s->kind == SCOPE_INSTANCE || s->kind == SCOPE_PACKAGE) {
      TRACE("reset scope %s", istr(s->name));

      active_scope = s;
      signals_tail = &(s->signals);

      jit_handle_t handle = jit_compile(m->jit, s->name);
      if (handle == JIT_HANDLE_INVALID)
         fatal_trace("failed to compile %s", istr(s->name));

      jit_scalar_t result, context = { .pointer = NULL };
      jit_scalar_t p2 = { .integer = 0 };

      if (s->parent != NULL)
         context.pointer = mptr_get(m->mspace, s->parent->privdata);

      if (jit_fastcall(m->jit, handle, &result, context, p2))
         mptr_put(m->mspace, s->privdata, result.pointer);
      else {
         m->force_stop = true;
         return;
      }

      active_scope = NULL;
      signals_tail = NULL;
   }

   for (rt_scope_t *c = s->child; c != NULL; c = c->chain)
      reset_scope(m, c);

   for (rt_proc_t *p = s->procs; p != NULL; p = p->chain)
      reset_process(m, p);
}

static res_memo_t *memo_resolution_fn(rt_model_t *m, rt_signal_t *signal,
                                      rt_resolution_t *resolution)
{
   // Optimise some common resolution functions by memoising them

   res_memo_t *memo = ihash_get(m->res_memo, resolution->closure.handle);
   if (memo != NULL)
      return memo;

   memo = static_alloc(m, sizeof(res_memo_t));
   memo->closure = resolution->closure;
   memo->flags   = resolution->flags;
   memo->ileft   = resolution->ileft;

   ihash_put(m->res_memo, memo->closure.handle, memo);

   if (resolution->nlits == 0 || resolution->nlits > 16)
      return memo;

   const vhdl_severity_t old_severity = get_exit_severity();
   set_exit_severity(SEVERITY_NOTE);

   jit_set_silent(m->jit, true);

   // Memoise the function for all two value cases

   for (int i = 0; i < resolution->nlits; i++) {
      for (int j = 0; j < resolution->nlits; j++) {
         int8_t args[2] = { i, j };
         jit_scalar_t result;
         if (jit_try_call(m->jit, memo->closure.handle, &result,
                          memo->closure.context, args, memo->ileft, 2)) {
            assert(result.integer < resolution->nlits && result.integer >= 0);
            memo->tab2[i][j] = result.integer;
         }
      }
   }

   // Memoise the function for all single value cases and determine if the
   // function behaves like the identity function

   bool identity = true;
   for (int i = 0; i < resolution->nlits; i++) {
      int8_t args[1] = { i };
      jit_scalar_t result;
      if (jit_try_call(m->jit, memo->closure.handle, &result,
                       memo->closure.context, args, memo->ileft, 1)) {
         memo->tab1[i] = result.integer;
         identity = identity && (memo->tab1[i] == i);
      }
   }

   if (jit_exit_status(m->jit) == 0) {
      memo->flags |= R_MEMO;
      if (identity)
         memo->flags |= R_IDENT;
   }

   TRACE("memoised resolution function %s for type %s",
         istr(jit_get_name(m->jit, resolution->closure.handle)),
         type_pp(tree_type(signal->where)));

   jit_set_silent(m->jit, false);
   jit_reset_exit_status(m->jit);

   set_exit_severity(old_severity);

   return memo;
}

static rt_value_t alloc_value(rt_model_t *m, rt_nexus_t *n)
{
   rt_value_t result = {};

   const size_t valuesz = n->size * n->width;
   if (valuesz > sizeof(rt_value_t)) {
      if (n->free_value != NULL) {
         result.ext = n->free_value;
         n->free_value = *(void **)result.ext;
      }
      else
         result.ext = static_alloc(m, valuesz);
   }

   return result;
}

static void free_value(rt_nexus_t *n, rt_value_t v)
{
   const size_t valuesz = n->width * n->size;
   if (valuesz > sizeof(rt_value_t)) {
      *(void **)v.ext = n->free_value;
      n->free_value = v.ext;
   }
}

static void *local_alloc(size_t size)
{
   if (tlab_valid(__nvc_tlab))
      return tlab_alloc(&__nvc_tlab, size);
   else
      return mspace_alloc(get_model()->mspace, size);
}

static inline uint8_t *value_ptr(rt_nexus_t *n, rt_value_t *v)
{
   const size_t valuesz = n->width * n->size;
   if (valuesz <= sizeof(rt_value_t))
      return v->bytes;
   else
      return v->ext;
}

static void copy_value_ptr(rt_nexus_t *n, rt_value_t *v, const void *p)
{
   const size_t valuesz = n->width * n->size;
   if (valuesz <= sizeof(rt_value_t))
      v->qword = *(uint64_t *)p;
   else
      memcpy(v->ext, p, valuesz);
}

static inline bool cmp_values(rt_nexus_t *n, rt_value_t a, rt_value_t b)
{
   const size_t valuesz = n->width * n->size;
   if (valuesz <= sizeof(rt_value_t))
      return a.qword == b.qword;
   else
      return memcmp(a.ext, b.ext, valuesz) == 0;
}

static rt_source_t *add_source(rt_model_t *m, rt_nexus_t *n, source_kind_t kind)
{
   rt_source_t *src = NULL;
   if (n->n_sources == 0)
      src = &(n->sources);
   else if (n->signal->resolution == NULL
            && (n->sources.tag == SOURCE_DRIVER
                || n->sources.u.port.conv_func == NULL))
      jit_msg(tree_loc(n->signal->where), DIAG_FATAL,
              "unresolved signal %s has multiple sources",
              istr(tree_ident(n->signal->where)));
   else {
      rt_source_t **p;
      for (p = &(n->sources.chain_input); *p; p = &((*p)->chain_input))
         ;
      *p = src = static_alloc(m, sizeof(rt_source_t));
   }

   // The only interesting values of n_sources are 0, 1, and 2
   if (n->n_sources < UINT8_MAX)
      n->n_sources++;

   src->chain_input  = NULL;
   src->chain_output = NULL;
   src->tag          = kind;
   src->disconnected = 0;

   switch (kind) {
   case SOURCE_DRIVER:
      {
         src->u.driver.proc  = NULL;
         src->u.driver.nexus = n;

         waveform_t *w0 = &(src->u.driver.waveforms);
         w0->when  = 0;
         w0->next  = NULL;
      }
      break;

   case SOURCE_PORT:
      src->u.port.conv_func = NULL;
      src->u.port.input     = NULL;
      src->u.port.output    = n;
      break;
   }

   return src;
}

static rt_net_t *get_net(rt_model_t *m, rt_nexus_t *nexus)
{
   if (likely(nexus->net != NULL))
      return nexus->net;
   else {
      rt_net_t *net = static_alloc(m, sizeof(rt_net_t));
      net->pending      = NULL;
      net->last_active  = TIME_HIGH;
      net->last_event   = TIME_HIGH;
      net->active_delta = -1;
      net->event_delta  = -1;

      static uint32_t next_net_id = 1;
      net->net_id = next_net_id++;

      return (nexus->net = net);
   }
}

static inline int map_index(rt_index_t *index, unsigned offset)
{
   if (likely(index->how >= 0))
      return offset >> index->how;
   else
      return offset / -(index->how);
}

static inline int unmap_index(rt_index_t *index, unsigned key)
{
   if (likely(index->how >= 0))
      return key << index->how;
   else
      return key * -(index->how);
}

static inline bool index_valid(rt_index_t *index, unsigned offset)
{
   if (likely(index->how >= 0))
      return (offset >> index->how) << index->how == offset;
   else
      return offset % -(index->how) == 0;
}

static void build_index(rt_signal_t *signal)
{
   const unsigned signal_w = signal->shared.size / signal->nexus.size;

   int shift = INT_MAX, gcd = 0;
   rt_nexus_t *n = &(signal->nexus);
   for (int i = 0, offset = 0; i < signal->n_nexus;
        i++, offset += n->width, n = n->chain) {
      const int tzc = __builtin_ctz(offset);
      shift = MIN(shift, tzc);

      // Compute greatest common divisor
      for (int b = offset; b > 0;) {
         int temp = b;
         b = gcd % b;
         gcd = temp;
      }
   }

   const int how = gcd > 1 && gcd > (1 << shift) && gcd > 1 ? -gcd : shift;
   const int count =
      how < 0 ? (signal_w - how - 1) / -how : (signal_w >> shift) + 1;

   TRACE("create index for signal %s how=%d count=%d",
         istr(tree_ident(signal->where)), how, count);

   rt_index_t *index = xcalloc_flex(sizeof(rt_index_t), count,
                                    sizeof(rt_nexus_t *));
   index->how = how;

   n = &(signal->nexus);
   for (int i = 0, offset = 0; i < signal->n_nexus;
        i++, offset += n->width, n = n->chain) {
      index->nexus[map_index(index, offset)] = n;
   }

   free(signal->index);
   signal->index = index;
}

static void update_index(rt_signal_t *s, rt_nexus_t *n)
{
   const unsigned offset = (n->resolved - (void *)s->shared.data) / n->size;

   if (!index_valid(s->index, offset)) {
      TRACE("rebuild index for %s offset=%d how=%d",
            istr(tree_ident(s->where)), offset, s->index->how);
      build_index(s);
      assert(s->index->nexus[map_index(s->index, offset)] == n);
   }
   else {
      const int elt = map_index(s->index, offset);
      assert(s->index->nexus[elt] == NULL);
      s->index->nexus[elt] = n;
   }
}

static rt_nexus_t *lookup_index(rt_signal_t *s, int *offset)
{
   if (likely(offset == 0 || s->index == NULL))
      return &(s->nexus);
   else if (!index_valid(s->index, *offset)) {
      TRACE("invalid index for %s offset=%d how=%d", istr(tree_ident(s->where)),
            *offset, s->index->how);
      free(s->index);
      s->index = NULL;
      return &(s->nexus);
   }
   else {
      const int key = map_index(s->index, *offset);
      for (int k = key; k >= 0; k--) {
         rt_nexus_t *n = s->index->nexus[k];
         if (n != NULL) {
            *offset = unmap_index(s->index, key - k);
            return n;
         }
      }
      return &(s->nexus);
   }
}

static waveform_t *alloc_waveform(void)
{
   if (free_waveforms == NULL) {
      // Ensure waveforms are always within one cache line
      STATIC_ASSERT(sizeof(waveform_t) <= 32);
      const int chunksz = 1024;
      char *mem = nvc_memalign(64, chunksz * 32);
      for (int i = 1; i < chunksz; i++)
         free_waveform((waveform_t *)(mem + i*32));

      return (waveform_t *)mem;
   }
   else {
      waveform_t *w = free_waveforms;
      free_waveforms = w->next;
      __builtin_prefetch(w->next, 1, 1);
      w->next = NULL;
      return w;
   }
}

static void clone_waveform(rt_nexus_t *nexus, waveform_t *w_new,
                           waveform_t *w_old, int offset)
{
   w_new->when = w_old->when;
   w_new->next = NULL;

   const int split = offset * nexus->size;
   const int oldsz = (offset + nexus->width) * nexus->size;
   const int newsz = nexus->width * nexus->size;

   if (split > sizeof(rt_value_t) && newsz > sizeof(rt_value_t)) {
      // Split the external memory with no copying
      w_new->value.ext = (char *)w_old->value.ext + split;
   }
   else if (newsz > sizeof(rt_value_t)) {
      // Wasting up to eight bytes at the start of the the old waveform
      char *ext = w_old->value.ext;
      w_old->value.qword = *(uint64_t *)ext;
      w_new->value.ext = ext + split;
   }
   else if (split > sizeof(rt_value_t)) {
      // Wasting up to eight bytes at the end of the the old waveform
      memcpy(w_new->value.bytes, w_old->value.ext + split, newsz);
   }
   else if (oldsz > sizeof(rt_value_t)) {
      // The memory backing this waveform is lost now but this can only
      // happen a bounded number of times as nexuses only ever shrink
      char *ext = w_old->value.ext;
      memcpy(w_new->value.bytes, ext + split, newsz);
      w_old->value.qword = *(uint64_t *)ext;
   }
   else {
      // This trick with shifting probably only works on little-endian
      // systems
      w_new->value.qword = w_old->value.qword >> (split * 8);
   }
}

static void clone_source(rt_model_t *m, rt_nexus_t *nexus, rt_source_t *old,
                         int offset, rt_net_t *net)
{
   rt_source_t *new = add_source(m, nexus, old->tag);

   switch (old->tag) {
   case SOURCE_PORT:
      {
         new->u.port.input = old->u.port.input;

         if (old->u.port.conv_func != NULL) {
            new->u.port.conv_func = old->u.port.conv_func;
            new->u.port.conv_func->refcnt++;
         }
         else {
            if (old->u.port.input->width == offset)
               new->u.port.input = old->u.port.input->chain;  // Cycle breaking
            else {
               rt_nexus_t *n = clone_nexus(m, old->u.port.input, offset, net);
               new->u.port.input = n;
            }
            assert(new->u.port.input->width == nexus->width);
         }
      }
      break;

   case SOURCE_DRIVER:
      {
         new->u.driver.proc = old->u.driver.proc;

         // Current transaction
         waveform_t *w_new = &(new->u.driver.waveforms);
         waveform_t *w_old = &(old->u.driver.waveforms);
         w_new->when = w_old->when;

         clone_waveform(nexus, w_new, w_old, offset);

         // Future transactions
         for (w_old = w_old->next; w_old; w_old = w_old->next) {
            w_new = (w_new->next = alloc_waveform());
            clone_waveform(nexus, w_new, w_old, offset);

            assert(w_old->when >= m->now);
            deltaq_insert_driver(m, w_new->when - m->now, nexus, new);
         }
      }
      break;
   }
}

static rt_nexus_t *clone_nexus(rt_model_t *m, rt_nexus_t *old, int offset,
                               rt_net_t *net)
{
   assert(offset < old->width);

   rt_signal_t *signal = old->signal;
   signal->n_nexus++;

   rt_nexus_t *new = static_alloc(m, sizeof(rt_nexus_t));
   new->width    = old->width - offset;
   new->size     = old->size;
   new->signal   = signal;
   new->resolved = (uint8_t *)old->resolved + offset * old->size;
   new->chain    = old->chain;
   new->flags    = old->flags;

   old->chain = new;
   old->width = offset;

   if (old->net != NULL) {
      if (net == NULL) {
         rt_net_t *new_net = get_net(m, new);
         rt_net_t *old_net = get_net(m, old);

         new_net->last_active  = old_net->last_active;
         new_net->last_event   = old_net->last_event;
         new_net->active_delta = old_net->active_delta;
         new_net->event_delta  = old_net->event_delta;

         for (sens_list_t *l = old_net->pending; l; l = l->next) {
            sens_list_t *lnew = rt_alloc(m->sens_list_stack);
            lnew->wake       = l->wake;
            lnew->wakeup_gen = l->wakeup_gen;
            lnew->recur      = l->recur;
            lnew->next       = new_net->pending;

            new_net->pending = lnew;
         }

         new->net = net = new_net;
      }
      else
         new->net = net;
   }

   if (new->chain == NULL)
      m->nexus_tail = &(new->chain);

   if (old->n_sources > 0) {
      for (rt_source_t *it = &(old->sources); it; it = it->chain_input)
         clone_source(m, new, it, offset, net);
   }

   int nth = 0;
   for (rt_source_t *old_o = old->outputs; old_o;
        old_o = old_o->chain_output, nth++) {

      assert(old_o->tag != SOURCE_DRIVER);

      if (old_o->u.port.conv_func != NULL)
         new->outputs = old_o;
      else {
         rt_nexus_t *out_n;
         if (old_o->u.port.output->width == offset)
            out_n = old_o->u.port.output->chain;   // Cycle breaking
         else
            out_n = clone_nexus(m, old_o->u.port.output, offset, net);

         for (rt_source_t *s = &(out_n->sources); s; s = s->chain_input) {
            if (s->tag == SOURCE_DRIVER)
               continue;
            else if (s->u.port.input == new || s->u.port.input == old) {
               s->u.port.input = new;
               s->chain_output = new->outputs;
               new->outputs = s;
               break;
            }
         }
      }
   }

   if (signal->index == NULL && signal->n_nexus >= NEXUS_INDEX_MIN)
      build_index(signal);
   else if (signal->index != NULL)
      update_index(signal, new);

   return new;
}

static rt_nexus_t *split_nexus(rt_model_t *m, rt_signal_t *s,
                               int offset, int count)
{
   rt_nexus_t *n0 = &(s->nexus);
   if (likely(offset == 0 && n0->width == count))
      return n0;
   else if (offset == 0 && count == s->shared.size / n0->size)
      return n0;

   rt_nexus_t *result = NULL;
   for (rt_nexus_t *it = lookup_index(s, &offset); count > 0; it = it->chain) {
      if (offset >= it->width) {
         offset -= it->width;
         continue;
      }
      else if (offset > 0) {
         clone_nexus(m, it, offset, NULL);
         offset = 0;
         continue;
      }
      else {
         if (it->width > count)
            clone_nexus(m, it, count, NULL);

         count -= it->width;

         if (result == NULL)
            result = it;
      }
   }

   return result;
}

static void setup_signal(rt_model_t *m, rt_signal_t *s, tree_t where,
                         unsigned count, unsigned size, net_flags_t flags,
                         unsigned offset)
{
   s->where         = where;
   s->n_nexus       = 1;
   s->shared.size   = count * size;
   s->shared.offset = offset;
   s->flags         = flags | NET_F_LAST_VALUE;
   s->parent        = active_scope;

   *signals_tail = s;
   signals_tail = &(s->chain);

   s->nexus.width      = count;
   s->nexus.size       = size;
   s->nexus.n_sources  = 0;
   s->nexus.resolved   = s->shared.data;
   s->nexus.flags      = flags | NET_F_LAST_VALUE;
   s->nexus.signal     = s;
   s->nexus.net        = NULL;

   *m->nexus_tail = &(s->nexus);
   m->nexus_tail = &(s->nexus.chain);

   m->n_signals++;
}

static void copy_sub_signals(rt_scope_t *scope, void *buf, value_fn_t fn)
{
   assert(scope->kind == SCOPE_SIGNAL);

   for (rt_signal_t *s = scope->signals; s != NULL; s = s->chain) {
      rt_nexus_t *n = &(s->nexus);
      for (unsigned i = 0; i < s->n_nexus; i++, n = n->chain) {
         ptrdiff_t o = (uint8_t *)n->resolved - s->shared.data;
         memcpy(buf + s->shared.offset + o, (*fn)(n), n->size * n->width);
      }
   }

   for (rt_scope_t *s = scope->child; s != NULL; s = s->chain)
      copy_sub_signals(s, buf, fn);
}

static void copy_sub_signal_sources(rt_scope_t *scope, void *buf, int stride)
{
   assert(scope->kind == SCOPE_SIGNAL);

   for (rt_signal_t *s = scope->signals; s != NULL; s = s->chain) {
      rt_nexus_t *n = &(s->nexus);
      for (unsigned i = 0; i < s->n_nexus; i++) {
         unsigned o = 0;
         for (rt_source_t *src = &(n->sources); src; src = src->chain_input) {
            const void *data = source_value(n, src);
            if (data == NULL)
               continue;

            memcpy(buf + s->shared.offset + (o++ * stride),
                   data, n->size * n->width);
         }
      }
   }

   for (rt_scope_t *s = scope->child; s != NULL; s = s->chain)
      copy_sub_signal_sources(s, buf, stride);
}

static void *composite_signal(rt_signal_t *signal, size_t *psz, value_fn_t fn)
{
   assert(signal->parent->kind == SCOPE_SIGNAL);

   rt_scope_t *root = signal->parent;
   while (root->parent->kind == SCOPE_SIGNAL)
      root = root->parent;

   *psz = root->size;

   char *buf = xmalloc(root->size);
   copy_sub_signals(root, buf, fn);
   return buf;
}

static void *call_conversion(rt_port_t *port, value_fn_t fn)
{
   rt_signal_t *i0 = port->input->signal;
   rt_conv_func_t *cf = port->conv_func;

   bool incopy = false;
   void *indata;
   size_t insz;
   if (i0->parent->kind == SCOPE_SIGNAL) {
      indata = composite_signal(i0, &insz, fn);
      incopy = true;
   }
   else if (i0->n_nexus == 1) {
      insz   = i0->shared.size;
      indata = (*fn)(&(i0->nexus));
   }
   else {
      insz   = i0->shared.size;
      indata = xmalloc(insz);
      incopy = true;

      rt_nexus_t *n = &(i0->nexus);
      for (unsigned i = 0; i < i0->n_nexus; i++, n = n->chain) {
         ptrdiff_t o = (uint8_t *)n->resolved - i0->shared.data;
         memcpy(indata + i0->shared.offset + o, (*fn)(n), n->size * n->width);
      }
   }

   rt_model_t *m = get_model();

   TRACE("call conversion function %s insz=%zu outsz=%zu",
         istr(jit_get_name(m->jit, cf->closure.handle)), insz, cf->bufsz);

   jit_scalar_t context = { .pointer = cf->closure.context };
   if (!jit_try_call_packed(m->jit, cf->closure.handle, context,
                            indata, insz, cf->buffer, cf->bufsz))
      m->force_stop = true;

   if (incopy) free(indata);

   return cf->buffer + port->output->signal->shared.offset;
}

static void *source_value(rt_nexus_t *nexus, rt_source_t *src)
{
   switch (src->tag) {
   case SOURCE_DRIVER:
      if (unlikely(src->disconnected))
         return NULL;
      else
         return value_ptr(nexus, &(src->u.driver.waveforms.value));

   case SOURCE_PORT:
      if (likely(src->u.port.conv_func == NULL))
         return driving_value(src->u.port.input);
      else
         return call_conversion(&(src->u.port), driving_value);
   }

   return NULL;
}

static void *call_resolution(rt_nexus_t *nexus, res_memo_t *r, int nonnull)
{
   // Find the first non-null source
   char *p0 = NULL;
   rt_source_t *s0 = &(nexus->sources);
   for (; s0 && (p0 = source_value(nexus, s0)) == NULL; s0 = s0->chain_input)
      ;

   if ((nexus->flags & NET_F_R_IDENT) && nonnull == 1) {
      // Resolution function behaves like identity for a single driver
      return p0;
   }
   else if ((r->flags & R_MEMO) && nonnull == 1) {
      // Resolution function has been memoised so do a table lookup

      void *resolved = local_alloc(nexus->width * nexus->size);

      for (int j = 0; j < nexus->width; j++) {
         const int index = ((uint8_t *)p0)[j];
         ((int8_t *)resolved)[j] = r->tab1[index];
      }

      return resolved;
   }
   else if ((r->flags & R_MEMO) && nonnull == 2) {
      // Resolution function has been memoised so do a table lookup

      void *resolved = local_alloc(nexus->width * nexus->size);

      char *p1 = NULL;
      for (rt_source_t *s1 = s0->chain_input;
           s1 && (p1 = source_value(nexus, s1)) == NULL;
           s1 = s1->chain_input)
         ;

      for (int j = 0; j < nexus->width; j++)
         ((int8_t *)resolved)[j] = r->tab2[(int)p0[j]][(int)p1[j]];

      return resolved;
   }
   else if (r->flags & R_COMPOSITE) {
      // Call resolution function of composite type

      rt_scope_t *scope = nexus->signal->parent, *rscope = scope;
      while (scope->parent->kind == SCOPE_SIGNAL) {
         scope = scope->parent;
         if (scope->flags & SCOPE_F_RESOLVED)
            rscope = scope;
      }

      TRACE("resolved composite signal needs %d bytes", scope->size);

      uint8_t *inputs = rt_tlab_alloc(nonnull * scope->size);
      copy_sub_signal_sources(scope, inputs, scope->size);

      rt_model_t *m = get_model();
      jit_scalar_t result;
      if (!jit_try_call(m->jit, r->closure.handle, &result,
                        r->closure.context, inputs, r->ileft, nonnull))
         m->force_stop = true;

      void *resolved = result.pointer;
      const ptrdiff_t noff =
         nexus->resolved - (void *)nexus->signal->shared.data;
      return resolved + nexus->signal->shared.offset + noff - rscope->offset;
   }
   else {
      void *resolved = local_alloc(nexus->width * nexus->size);
      rt_model_t *m = get_model();

      for (int j = 0; j < nexus->width; j++) {
#define CALL_RESOLUTION_FN(type) do {                                   \
            type vals[nonnull];                                         \
            unsigned o = 0;                                             \
            for (rt_source_t *s = s0; s; s = s->chain_input) {          \
               const void *data = source_value(nexus, s);               \
               if (data != NULL)                                        \
                  vals[o++] = ((const type *)data)[j];                  \
            }                                                           \
            assert(o == nonnull);                                       \
            type *p = (type *)resolved;                                 \
            jit_scalar_t result;                                        \
            if (!jit_try_call(m->jit, r->closure.handle, &result,       \
                              r->closure.context, vals, r->ileft,       \
                              nonnull))                                 \
               m->force_stop = true;                                    \
            p[j] = result.integer;                                      \
         } while (0)

         FOR_ALL_SIZES(nexus->size, CALL_RESOLUTION_FN);
      }

      return resolved;
   }
}

static void *driving_value(rt_nexus_t *nexus)
{
   // Algorithm for driving values is in LRM 08 section 14.7.7.2

   // If S is driving-value forced, the driving value of S is unchanged
   // from its previous value; no further steps are required.
   if (unlikely(nexus->flags & NET_F_FORCED))
      return value_ptr(nexus, &(nexus->forcing));

   // If S has no source, then the driving value of S is given by the
   // default value associated with S
   if (nexus->n_sources == 0)
      return nexus->resolved + 2*nexus->signal->shared.size;

   res_memo_t *r = nexus->signal->resolution;

   if (r == NULL) {
      rt_source_t *s = &(nexus->sources);

      if (s->tag == SOURCE_DRIVER) {
         // If S has one source that is a driver and S is not a resolved
         // signal, then the driving value of S is the current value of
         // that driver.
         assert(!s->disconnected);
         return value_ptr(nexus, &(s->u.driver.waveforms.value));
      }
      else {
         // If S has one source that is a port and S is not a resolved
         // signal, then the driving value of S is the driving value of
         // the formal part of the association element that associates S
         // with that port
         if (likely(s->u.port.conv_func == NULL))
            return driving_value(s->u.port.input);
         else
            return call_conversion(&(s->u.port), driving_value);
      }
   }
   else {
      // If S is a resolved signal and has one or more sources, then the
      // driving values of the sources of S are examined.

      int nonnull = 0;
      for (rt_source_t *s = &(nexus->sources); s; s = s->chain_input) {
         if (!s->disconnected)
            nonnull++;
      }

      // If S is of signal kind register and all the sources of S have
      // values determined by the null transaction, then the driving
      // value of S is unchanged from its previous value.
      if (nonnull == 0 && (nexus->flags & NET_F_REGISTER))
         return nexus->resolved;

      // Otherwise, the driving value of S is obtained by executing the
      // resolution function associated with S
      return call_resolution(nexus, r, nonnull);
   }
}

static const void *effective_value(rt_nexus_t *nexus)
{
   // Algorithm for effective values is in LRM 08 section 14.7.7.3

   // If S is a connected port of mode in or inout, then the effective
   // value of S is the same as the effective value of the actual part
   // of the association element that associates an actual with S
   if (nexus->flags & NET_F_INOUT) {
      for (rt_source_t *s = nexus->outputs; s; s = s->chain_output) {
         if (s->tag == SOURCE_PORT) {
            if (likely(s->u.port.conv_func == NULL))
               return effective_value(s->u.port.output);
            else
               return nexus->resolved;
         }
      }
   }

   // If S is a signal declared by a signal declaration, a port of mode
   // out or buffer, or an unconnected port of mode inout, then the
   // effective value of S is the same as the driving value of S.
   //
   // If S is an unconnected port of mode in, the effective value of S
   // is given by the default value associated with S.
   if (nexus->flags & NET_F_EFFECTIVE)
      return nexus->resolved + 2 * nexus->signal->shared.size;
   else
      return nexus->resolved;
}

static void propagate_nexus(rt_nexus_t *nexus, const void *resolved)
{
   const size_t valuesz = nexus->size * nexus->width;

   // LAST_VALUE is the same as the initial value when there have
   // been no events on the signal otherwise only update it when
   // there is an event
   void *last_value = nexus->resolved + nexus->signal->shared.size;
   memcpy(last_value, nexus->resolved, valuesz);

   if (nexus->resolved != resolved)   // Can occur during startup
      memcpy(nexus->resolved, resolved, valuesz);
}

static int nexus_rank(rt_nexus_t *nexus)
{
   if (nexus->n_sources > 0) {
      int rank = 0;
      for (rt_source_t *s = &(nexus->sources); s; s = s->chain_input) {
         if (s->tag == SOURCE_PORT)
            rank = MAX(rank, nexus_rank(s->u.port.input) + 1);
      }
      return rank;
   }
   else
      return 0;
}

static void reset_coverage(rt_model_t *m)
{
   assert(m->cover == NULL);

   if ((m->cover = cover_read_tags(m->top)) == NULL)
      return;

   int32_t n_stmts, n_conds;
   cover_count_tags(m->cover, &n_stmts, &n_conds);

   int32_t *cover_stmts = ffi_find_symbol(NULL, "cover_stmts");
   if (cover_stmts != NULL)
      memset(cover_stmts, '\0', sizeof(int32_t) * n_stmts);

   int32_t *cover_conds = ffi_find_symbol(NULL, "cover_conds");
   if (cover_conds != NULL)
      memset(cover_conds, '\0', sizeof(int32_t) * n_conds);
}

static void emit_coverage(rt_model_t *m)
{
   if (m->cover != NULL) {
      int32_t *cover_stmts = ffi_find_symbol(NULL, "cover_stmts");
      int32_t *cover_conds = ffi_find_symbol(NULL, "cover_conds");
      if (cover_stmts != NULL)
         cover_report(m->top, m->cover, cover_stmts, cover_conds);
   }
}

static void dump_one_signal(rt_model_t *m, rt_scope_t *scope, rt_signal_t *s,
                            tree_t alias)
{
   rt_nexus_t *n = &(s->nexus);

   LOCAL_TEXT_BUF tb = tb_new();
   if (scope->kind == SCOPE_SIGNAL)
      tb_printf(tb, "%s.", istr(scope->name));
   tb_cat(tb, istr(tree_ident(alias ?: s->where)));
   if (alias != NULL)
      tb_append(tb, '*');

   for (int nth = 0; nth < s->n_nexus; nth++, n = n->chain) {
      int n_outputs = 0;
      for (rt_source_t *s = n->outputs; s != NULL; s = s->chain_output)
         n_outputs++;

      void *driving = NULL;
      if (n->flags & NET_F_EFFECTIVE)
         driving = n->resolved + 2 * n->signal->shared.size;

      fprintf(stderr, "%-20s %-5d %-4d %-7d %-7d %-4d ",
              nth == 0 ? tb_get(tb) : "+",
              n->width, n->size, n->n_sources, n_outputs,
              n->net != NULL ? n->net->net_id : 0);

      if (n->net != NULL) {
         void *last_value = n->resolved + n->signal->shared.size;
         if (n->net->event_delta == m->iteration
             && n->net->last_event == m->now)
            fprintf(stderr, "%s -> ", fmt_nexus(n, last_value));
      }

      fputs(fmt_nexus(n, n->resolved), stderr);

      if (driving != NULL)
         fprintf(stderr, " (%s)", fmt_nexus(n, driving));

      fputs("\n", stderr);
   }
}

static void dump_signals(rt_model_t *m, rt_scope_t *scope)
{
   if (scope->signals == NULL && scope->child == NULL)
      return;

   if (scope->kind != SCOPE_SIGNAL && scope->kind != SCOPE_ROOT) {
      const char *sname = istr(scope->name);
      fprintf(stderr, "== %s ", sname);
      for (int pad = 74 - strlen(sname); pad > 0; pad--)
         fputc('=', stderr);
      fputc('\n', stderr);

      fprintf(stderr, "%-20s %5s %4s %7s %7s %-4s %s\n",
              "Signal", "Width", "Size", "Sources", "Outputs",
              "Net", "Value");
   }

   for (rt_signal_t *s = scope->signals; s != NULL; s = s->chain)
      dump_one_signal(m, scope, s, NULL);

   for (rt_alias_t *a = scope->aliases; a != NULL; a = a->chain)
      dump_one_signal(m, scope, a->signal, a->where);

   for (rt_scope_t *c = scope->child; c != NULL; c = c->chain)
      dump_signals(m, c);
}

void model_reset(rt_model_t *m)
{
   MODEL_ENTRY(m);

   // Initialisation is described in LRM 93 section 12.6.4

   reset_coverage(m);
   reset_scope(m, m->root);

   if (m->force_stop)
      return;   // Error in intialisation

#if TRACE_SIGNALS > 0
   if (__trace_on)
      dump_signals(m, m->root);
#endif

   TRACE("calculate initial signal values");

   // The signals in the model are updated as follows in an order such
   // that if a given signal R depends upon the current value of another
   // signal S, then the current value of S is updated prior to the
   // updating of the current value of R.

   heap_t *q = heap_new(MAX(m->n_signals + 1, 128));

   for (rt_nexus_t *n = m->nexuses; n != NULL; n = n->chain) {
      // The initial value of each driver is the default value of the signal
      if (n->n_sources > 0) {
         for (rt_source_t *s = &(n->sources); s; s = s->chain_input) {
            if (s->tag == SOURCE_DRIVER)
               copy_value_ptr(n, &(s->u.driver.waveforms.value), n->resolved);
         }
      }

      heap_insert(q, nexus_rank(n), n);
   }

   SCOPED_A(rt_nexus_t *) effq = AINIT;

   while (heap_size(q) > 0) {
      rt_nexus_t *n = heap_extract_min(q);

      if (n->flags & NET_F_EFFECTIVE) {
         // Driving and effective values must be calculated separately
         void *driving = n->resolved + 2*n->signal->shared.size;
         memcpy(driving, driving_value(n), n->width * n->size);

         APUSH(effq, n);

         TRACE("%s initial driving value %s",
               istr(tree_ident(n->signal->where)), fmt_nexus(n, driving));
      }
      else {
         // Effective value is always the same as the driving value
         const void *initial = n->resolved;
         if (n->n_sources > 0)
            initial = driving_value(n);

         propagate_nexus(n, initial);

         TRACE("%s initial value %s", istr(tree_ident(n->signal->where)),
               fmt_nexus(n, initial));
      }
   }

   heap_free(q);

   // Update effective values after all initial driving values calculated
   for (int i = 0; i < effq.count; i++) {
      rt_nexus_t *n = effq.items[i];

      const void *initial = effective_value(n);
      propagate_nexus(n, initial);

      TRACE("%s initial effective value %s", istr(tree_ident(n->signal->where)),
            fmt_nexus(n, initial));
   }
}

static bool is_stale_event(event_t *e)
{
   return (e->kind == EVENT_PROCESS)
      && (e->proc.wakeup_gen != e->proc.proc->wakeable.wakeup_gen);
}

static void sched_event(rt_model_t *m, sens_list_t **list,
                        rt_wakeable_t *obj, bool recur)
{
   // See if there is already a stale entry in the pending list for this
   // object
   sens_list_t *it = *list;
   int count = 0;
   for (; it != NULL; it = it->next, ++count) {
      if ((it->wake == obj) && (it->wakeup_gen != obj->wakeup_gen))
         break;
   }

   if (it == NULL) {
      sens_list_t *node = rt_alloc(m->sens_list_stack);
      node->wake       = obj;
      node->wakeup_gen = obj->wakeup_gen;
      node->next       = *list;
      node->recur      = recur;

      *list = node;
   }
   else {
      // Reuse the stale entry
      it->wakeup_gen = obj->wakeup_gen;
   }
}

static rt_source_t *find_driver(rt_nexus_t *nexus)
{
   // Try to find this process in the list of existing drivers
   for (rt_source_t *d = &(nexus->sources); d; d = d->chain_input) {
      if (d->tag == SOURCE_DRIVER && d->u.driver.proc == active_proc)
         return d;
   }

   return NULL;
}

static inline bool insert_transaction(rt_model_t *m, rt_nexus_t *nexus,
                                      rt_source_t *source, waveform_t *w,
                                      uint64_t when, uint64_t reject)
{
   waveform_t *last = &(source->u.driver.waveforms);
   waveform_t *it   = last->next;
   while (it != NULL && it->when < when) {
      // If the current transaction is within the pulse rejection interval
      // and the value is different to that of the new transaction then
      // delete the current transaction
      assert(it->when >= m->now);
      if (it->when >= when - reject
          && (w == NULL || !cmp_values(nexus, it->value, w->value))) {
         waveform_t *next = it->next;
         last->next = next;
         free_value(nexus, it->value);
         free_waveform(it);
         it = next;
      }
      else {
         last = it;
         it = it->next;
      }
   }
   last->next = w;

   // Delete all transactions later than this
   // We could remove this transaction from the deltaq as well but the
   // overhead of doing so is probably higher than the cost of waking
   // up for the empty event
   bool already_scheduled = false;
   for (waveform_t *next; it != NULL; it = next) {
      next = it->next;
      already_scheduled |= (it->when == when);
      free_value(nexus, it->value);
      free_waveform(it);
   }

   return already_scheduled;
}

static void sched_driver(rt_model_t *m, rt_nexus_t *nexus, uint64_t after,
                         uint64_t reject, rt_value_t value)
{
   rt_source_t *d = find_driver(nexus);
   assert(d != NULL);

   if (unlikely(reject > after))
      jit_msg(NULL, DIAG_FATAL, "signal %s pulse reject limit %s is greater "
              "than delay %s", istr(tree_ident(nexus->signal->where)),
              fmt_time(reject), fmt_time(after));

   waveform_t *w = alloc_waveform();
   w->when  = m->now + after;
   w->next  = NULL;
   w->value = value;

   if (!insert_transaction(m, nexus, d, w, w->when, reject))
      deltaq_insert_driver(m, after, nexus, d);
}

static void sched_disconnect(rt_model_t *m, rt_nexus_t *nexus, uint64_t after,
                             uint64_t reject)
{
   rt_source_t *d = find_driver(nexus);
   assert(d != NULL);

   const uint64_t when = m->now + after;

   insert_transaction(m, nexus, d, NULL, when, reject);
   deltaq_insert_disconnect(m, after, d);
}

static void async_watch_callback(void *context, void *arg)
{
   rt_model_t *m = context;
   rt_watch_t *w = arg;

   assert(w->wakeable.pending);
   w->wakeable.pending = false;

   MODEL_ENTRY(m);
   (*w->fn)(m->now, w->signal, w, w->user_data);
}

static void async_timeout_callback(void *context, void *arg)
{
   rt_model_t *m = context;
   event_t *e = arg;

   MODEL_ENTRY(m);
   (*e->timeout.fn)(m->now, e->timeout.user);
   rt_free(m->event_stack, e);
}

static void async_update_implicit_signal(void *context, void *arg)
{
   rt_model_t *m = context;
   rt_implicit_t *imp = arg;

   assert(imp->wakeable.pending);
   imp->wakeable.pending = false;

   MODEL_ENTRY(m);
   update_implicit_signal(m, imp);
}

static void async_run_process(void *context, void *arg)
{
   rt_model_t *m = context;
   rt_proc_t *proc = arg;

   assert(proc->wakeable.pending);
   proc->wakeable.pending = false;

   MODEL_ENTRY(m);
   run_process(m, proc);
}

static void notify_event(rt_model_t *m, rt_net_t *net)
{
   net->last_event = net->last_active = m->now;
   net->event_delta = net->active_delta = m->iteration;

   // Wake up everything on the pending list
   for (sens_list_t *it = net->pending, *next, **reenq = &(net->pending);
        it; it = next) {
      next = it->next;

      // To avoid having each process keep a list of the signals it is
      // sensitive to, each process has a "wakeup generation" number
      // which is incremented after each wait statement and stored in
      // the signal sensitivity list. We then ignore any sensitivity
      // list elements where the generation doesn't match the current
      // process wakeup generation: these correspond to stale "wait on"
      // statements that have already resumed.
      const bool stale = !it->recur && it->wakeup_gen != it->wake->wakeup_gen;

      if (!stale && !it->wake->pending) {
         workq_t *wq = it->wake->postponed ? m->postponedq : m->procq;

         switch (it->wake->kind) {
         case W_PROC:
            {
               rt_proc_t *proc = container_of(it->wake, rt_proc_t, wakeable);
               TRACE("wakeup %sprocess %s",
                     it->wake->postponed ? "postponed " : "", istr(proc->name));
               workq_do(wq, async_run_process, proc);
            }
            break;

         case W_IMPLICIT:
            {
               rt_implicit_t *imp =
                  container_of(it->wake, rt_implicit_t, wakeable);
               TRACE("wakeup %svalue change callback %s",
                     it->wake->postponed ? "postponed " : "",
                     istr(jit_get_name(m->jit, imp->closure.handle)));
               workq_do(m->implicitq, async_update_implicit_signal, imp);
            }
            break;

         case W_WATCH:
            {
               rt_watch_t *w = container_of(it->wake, rt_watch_t, wakeable);
               TRACE("wakeup implicit signal %s",
                     istr(tree_ident(w->signal->where)));
               workq_do(wq, async_watch_callback, w);
            }
            break;
         }

         ++(it->wake->wakeup_gen);
         it->wake->pending = true;
      }

      if (it->recur) {
         *reenq = it;
         reenq = &(it->next);
      }
      else {
         rt_free(m->sens_list_stack, it);
         *reenq = NULL;
      }
   }
}

static void notify_active(rt_model_t *m, rt_net_t *net)
{
   net->last_active = m->now;
   net->active_delta = m->iteration;
}

static void update_effective(rt_model_t *m, rt_nexus_t *nexus)
{
   const void *value = effective_value(nexus);

   TRACE("update %s effective value %s", istr(tree_ident(nexus->signal->where)),
         fmt_nexus(nexus, value));

   rt_net_t *net = get_net(m, nexus);

   if (memcmp(nexus->resolved, value, nexus->size * nexus->width) != 0) {
      propagate_nexus(nexus, value);
      notify_event(m, net);
   }
   else
      notify_active(m, net);
}

static void async_update_effective(void *context, void *arg)
{
   rt_model_t *m = context;
   rt_nexus_t *nexus = arg;

   MODEL_ENTRY(m);
   update_effective(m, nexus);
}

static void enqueue_effective(rt_model_t *m, rt_nexus_t *nexus)
{
   workq_do(m->effq, async_update_effective, nexus);

   if (nexus->n_sources > 0) {
      for (rt_source_t *s = &(nexus->sources); s; s = s->chain_input) {
         if (s->tag == SOURCE_PORT && (s->u.port.input->flags & NET_F_INOUT))
            enqueue_effective(m, s->u.port.input);
      }
   }
}

static void update_driving(rt_model_t *m, rt_nexus_t *nexus)
{
   const void *value = driving_value(nexus);
   const size_t valuesz = nexus->size * nexus->width;

   TRACE("update %s driving value %s", istr(tree_ident(nexus->signal->where)),
         fmt_nexus(nexus, value));

   rt_net_t *net = get_net(m, nexus);
   bool update_outputs = false;

   if (nexus->flags & NET_F_EFFECTIVE) {
      // The active and event flags will be set when we update the
      // effective value later
      update_outputs = true;

      void *driving = nexus->resolved + 2*nexus->signal->shared.size;
      memcpy(driving, value, valuesz);

      enqueue_effective(m, nexus);
   }
   else if (memcmp(nexus->resolved, value, valuesz) != 0) {
      propagate_nexus(nexus, value);
      notify_event(m, net);
      update_outputs = true;
   }
   else
      notify_active(m, net);

   if (update_outputs) {
      for (rt_source_t *o = nexus->outputs; o; o = o->chain_output) {
         assert(o->tag == SOURCE_PORT);
         update_driving(m, o->u.port.output);
      }
   }
}

static void update_driver(rt_model_t *m, rt_nexus_t *nexus, rt_source_t *source)
{
   // Updating drivers may involve calling resolution functions
   if (!tlab_valid(__nvc_tlab))
      tlab_acquire(m->mspace, &__nvc_tlab);

   if (likely(source != NULL)) {
      waveform_t *w_now  = &(source->u.driver.waveforms);
      waveform_t *w_next = w_now->next;

      if (likely((w_next != NULL) && (w_next->when == m->now))) {
         free_value(nexus, w_now->value);
         *w_now = *w_next;
         free_waveform(w_next);
         source->disconnected = 0;
         update_driving(m, nexus);
      }
      else
         assert(w_now != NULL);
   }
   else  // Update due to force/release
      update_driving(m, nexus);

   tlab_reset(__nvc_tlab);   // No allocations can be live past here
}

static void async_update_driver(void *context, void *arg)
{
   rt_model_t *m = context;
   rt_source_t *src = arg;

   MODEL_ENTRY(m);
   update_driver(m, src->u.driver.nexus, src);
}

static void async_disconnect(void *context, void *arg)
{
   rt_model_t *m = context;
   rt_source_t *src = arg;

   MODEL_ENTRY(m);
   src->disconnected = 1;
   update_driver(m, src->u.driver.nexus, NULL);
}

static void async_update_driving(void *context, void *arg)
{
   rt_model_t *m = context;
   rt_nexus_t *nexus = arg;

   MODEL_ENTRY(m);
   update_driver(m, nexus, NULL);
}

static void update_implicit_signal(rt_model_t *m, rt_implicit_t *imp)
{
   jit_scalar_t result;
   if (!jit_try_call(m->jit, imp->closure.handle, &result,
                     imp->closure.context))
      m->force_stop = true;

   TRACE("implicit signal %s guard expression %"PRIi64,
         istr(tree_ident(imp->signal.where)), result.integer);

   assert(imp->signal.n_nexus == 1);
   rt_nexus_t *n0 = &(imp->signal.nexus);

   rt_net_t *net = get_net(m, n0);

   if (*(int8_t *)n0->resolved != result.integer) {
      propagate_nexus(n0, &result.integer);
      notify_event(m, net);
   }
   else
      notify_active(m, net);
}

static void iteration_limit_proc_cb(void *context, void *arg, void *extra)
{
   rt_proc_t *proc = arg;
   diag_t *d = extra;

   const loc_t *l = tree_loc(proc->where);
   diag_hint(d, l, "process %s is active", istr(proc->name));
}

static void iteration_limit_driver_cb(void *context, void *arg, void *extra)
{
   rt_source_t *src = arg;
   diag_t *d = extra;

   if (src->tag == SOURCE_DRIVER) {
      tree_t decl = src->u.driver.nexus->signal->where;
      diag_hint(d, tree_loc(decl), "driver for %s %s is active",
                tree_kind(decl) == T_PORT_DECL ? "port" : "signal",
                istr(tree_ident(decl)));
   }
}

static void reached_iteration_limit(rt_model_t *m)
{
   diag_t *d = diag_new(DIAG_FATAL, NULL);

   diag_printf(d, "limit of %d delta cycles reached", m->stop_delta);

   workq_scan(m->delta_procq, iteration_limit_proc_cb, d);
   workq_scan(m->delta_driverq, iteration_limit_driver_cb, d);

   diag_hint(d, NULL, "you can increase this limit with $bold$--stop-delta$$");
   diag_emit(d);

   jit_abort(EXIT_FAILURE);
}

static void swap_workq(workq_t **a, workq_t **b)
{
   workq_t *tmp = *a;
   *a = *b;
   *b = tmp;
}

static void model_cycle(rt_model_t *m)
{
   // Simulation cycle is described in LRM 93 section 12.6.4

   const bool is_delta_cycle = m->next_is_delta;
   m->next_is_delta = false;

   if (is_delta_cycle)
      m->iteration = m->iteration + 1;
   else {
      event_t *peek = heap_min(m->eventq_heap);
      while (unlikely(is_stale_event(peek))) {
         // Discard stale events
         rt_free(m->event_stack, heap_extract_min(m->eventq_heap));
         if (heap_size(m->eventq_heap) == 0)
            return;
         else
            peek = heap_min(m->eventq_heap);
      }
      m->now = peek->when;
      m->iteration = 0;
   }

   TRACE("begin cycle");

#if TRACE_DELTAQ > 0
   if (__trace_on)
      deltaq_dump(m);
#endif

   swap_workq(&m->procq, &m->delta_procq);
   swap_workq(&m->driverq, &m->delta_driverq);

   if (!is_delta_cycle) {
      global_event(m, RT_NEXT_TIME_STEP);

      for (;;) {
         event_t *e = heap_extract_min(m->eventq_heap);
         switch (e->kind) {
         case EVENT_PROCESS:
            if (!is_stale_event(e)) {
               assert(!e->proc.proc->wakeable.pending);
               workq_do(m->procq, async_run_process, e->proc.proc);
               e->proc.proc->wakeable.pending = true;
               ++(e->proc.proc->wakeable.wakeup_gen);
            }
            rt_free(m->event_stack, e);
            break;
         case EVENT_DRIVER:
            workq_do(m->driverq, async_update_driver, e->driver.source);
            rt_free(m->event_stack, e);
            break;
         case EVENT_TIMEOUT:
            workq_do(m->driverq, async_timeout_callback, e);
            // Event freed in callback
            break;
         case EVENT_DISCONNECT:
            workq_do(m->driverq, async_disconnect, e->driver.source);
            rt_free(m->event_stack, e);
            break;
         }

         if (heap_size(m->eventq_heap) == 0)
            break;

         event_t *peek = heap_min(m->eventq_heap);
         if (peek->when > m->now)
            break;
      }
   }

   workq_start(m->driverq);
   workq_drain(m->driverq);

   workq_start(m->effq);
   workq_drain(m->effq);

   // Update implicit signals
   if (m->implicitq != NULL) {
      workq_start(m->implicitq);
      workq_drain(m->implicitq);
   }

#if TRACE_SIGNALS > 0
   if (__trace_on)
      dump_signals(m, m->root);
#endif

   // Run all non-postponed processes and event callbacks
   workq_start(m->procq);
   workq_drain(m->procq);

   global_event(m, RT_END_OF_PROCESSES);

   if (!m->next_is_delta) {
      m->can_create_delta = false;
      global_event(m, RT_LAST_KNOWN_DELTA_CYCLE);

      // Run all postponed processes and event callbacks
      workq_start(m->postponedq);
      workq_drain(m->postponedq);

      m->can_create_delta = true;
   }
   else if (m->stop_delta > 0 && m->iteration == m->stop_delta)
      reached_iteration_limit(m);
}

static bool should_stop_now(rt_model_t *m, uint64_t stop_time)
{
   if (m->force_stop)
      return true;
   else if (m->next_is_delta)
      return false;
   else if (heap_size(m->eventq_heap) == 0)
      return true;
   else {
      event_t *peek = heap_min(m->eventq_heap);
      return peek->when > stop_time;
   }
}

void model_run(rt_model_t *m, uint64_t stop_time)
{
   MODEL_ENTRY(m);

   if (m->force_stop)
      return;   // Was error during intialisation

   stop_workers();   // Runtime is not thread-safe

   global_event(m, RT_START_OF_SIMULATION);

   while (!should_stop_now(m, stop_time))
      model_cycle(m);

   global_event(m, RT_END_OF_SIMULATION);

   emit_coverage(m);
}

static inline void check_postponed(int64_t after)
{
   if (unlikely(active_proc->wakeable.postponed && (after == 0)))
      fatal("postponed process %s cannot cause a delta cycle",
            istr(active_proc->name));
}

bool force_signal(rt_signal_t *s, const uint64_t *buf, size_t count)
{
   TRACE("force signal %s to %"PRIu64"%s",
         istr(tree_ident(s->where)), buf[0], count > 1 ? "..." : "");

   rt_model_t *m = get_model();
   assert(m->can_create_delta);

   int offset = 0;
   rt_nexus_t *n = split_nexus(m, s, offset, count);
   for (; count > 0; n = n->chain) {
      if (n->flags & NET_F_FORCED)
         free_value(n, n->forcing);

      n->flags |= NET_F_FORCED;
      n->forcing = alloc_value(m, n);

#define SIGNAL_FORCE_EXPAND_U64(type) do {                              \
         type *dp = (type *)value_ptr(n, &(n->forcing));                \
         for (int i = 0; (i < n->width) && (offset + i < count); i++)   \
            dp[i] = buf[offset + i];                                    \
      } while (0)

      FOR_ALL_SIZES(n->size, SIGNAL_FORCE_EXPAND_U64);

      deltaq_insert_force_release(m, 0, n);

      offset += n->width;
      count -= n->width;
   }

   return (offset == count);
}

bool model_can_create_delta(rt_model_t *m)
{
   return m->can_create_delta;
}

int64_t model_now(rt_model_t *m, unsigned *deltas)
{
   if (deltas != NULL)
      *deltas = MAX(m->iteration, 0);

   return m->now;
}

void model_stop(rt_model_t *m)
{
   m->force_stop = true;
}

void model_set_global_cb(rt_model_t *m, rt_event_t event, rt_event_fn_t fn,
                         void *user)
{
   assert(event < RT_LAST_EVENT);

   callback_t *cb = rt_alloc(m->callback_stack);
   cb->next = m->global_cbs[event];
   cb->fn   = fn;
   cb->user = user;

   m->global_cbs[event] = cb;
}

void model_set_timeout_cb(rt_model_t *m, uint64_t when, timeout_fn_t fn,
                          void *user)
{
   event_t *e = rt_alloc(m->event_stack);
   e->when         = m->now + when;
   e->kind         = EVENT_TIMEOUT;
   e->timeout.fn   = fn;
   e->timeout.user = user;

   assert(when > m->now);   // TODO: delta timeouts?
   heap_insert(m->eventq_heap, e->when, e);
}

rt_watch_t *model_set_event_cb(rt_model_t *m, rt_signal_t *s, sig_event_fn_t fn,
                               void *user, bool postponed)
{
   if (fn == NULL) {
      // Find the first entry in the watch list and disable it
      for (rt_watch_t *it = m->watches; it != NULL; it = it->chain_all) {
         if ((it->signal == s) && (it->user_data == user)) {
            it->wakeable.pending = true;   // TODO: not a good way of doing this
            break;
         }
      }

      return NULL;
   }
   else {
      rt_watch_t *w = rt_alloc(m->watch_stack);
      w->signal    = s;
      w->fn        = fn;
      w->chain_all = m->watches;
      w->user_data = user;

      w->wakeable.kind       = W_WATCH;
      w->wakeable.postponed  = postponed;
      w->wakeable.pending    = false;
      w->wakeable.wakeup_gen = 0;

      m->watches = w;

      rt_nexus_t *n = &(w->signal->nexus);
      for (int i = 0; i < s->n_nexus; i++, n = n->chain)
         sched_event(m, &(get_net(m, n)->pending), &(w->wakeable), true);

      return w;
   }
}

void model_interrupt(rt_model_t *m)
{
   model_stop(m);

   if (active_proc != NULL)
      jit_msg(NULL, DIAG_FATAL,
              "interrupted in process %s at %s+%d",
              istr(active_proc->name), fmt_time(m->now), m->iteration);
   else {
      diag_t *d = diag_new(DIAG_FATAL, NULL);
      diag_printf(d, "interrupted at %s+%d", fmt_time(m->now), m->iteration);
      diag_emit(d);

      jit_set_exit_status(m->jit, EXIT_FAILURE);
   }
}

// TODO: this interface should be removed eventually
void *rt_tlab_alloc(size_t size)
{
   if (tlab_valid(__nvc_tlab))
      return tlab_alloc(&__nvc_tlab, size);
   else
      return mspace_alloc(get_model()->mspace, size);
}

////////////////////////////////////////////////////////////////////////////////
// Entry points from compiled code

sig_shared_t *x_init_signal(int count, int size, const uint8_t *values,
                            net_flags_t flags, tree_t where, int offset)
{
   TRACE("init signal %s count=%d size=%d values=%s flags=%x offset=%d",
         istr(tree_ident(where)), count, size,
         fmt_values(values, size * count), flags, offset);

   rt_model_t *m = get_model();

   const size_t datasz = MAX(3 * count * size, 8);
   rt_signal_t *s = static_alloc(m, sizeof(rt_signal_t) + datasz);
   setup_signal(m, s, where, count, size, flags, offset);

   memcpy(s->shared.data, values, s->shared.size);

   // The driving value area is also used to save the default value
   void *driving = s->shared.data + 2*s->shared.size;
   memcpy(driving, values, s->shared.size);

   return &(s->shared);
}

void x_drive_signal(sig_shared_t *ss, uint32_t offset, int32_t count)
{
   rt_signal_t *s = container_of(ss, rt_signal_t, shared);

   TRACE("drive signal %s+%d count=%d", istr(tree_ident(s->where)),
         offset, count);

   rt_model_t *m = get_model();
   rt_nexus_t *n = split_nexus(m, s, offset, count);
   for (; count > 0; n = n->chain) {
      rt_source_t *s;
      for (s = &(n->sources); s; s = s->chain_input) {
         if (s->tag == SOURCE_DRIVER && s->u.driver.proc == active_proc)
            break;
      }

      if (s == NULL) {
         s = add_source(m, n, SOURCE_DRIVER);
         s->u.driver.waveforms.value = alloc_value(m, n);
         s->u.driver.proc = active_proc;
      }

      count -= n->width;
      assert(count >= 0);
   }
}

int64_t x_now(void)
{
   return __model ? __model->now : 0;
}

int x_current_delta(void)
{
   return __model ? __model->iteration : 0;
}

void x_sched_process(int64_t delay)
{
   TRACE("_sched_process delay=%s", fmt_time(delay));
   deltaq_insert_proc(get_model(), delay, active_proc);
}

void x_sched_waveform_s(sig_shared_t *ss, uint32_t offset, uint64_t scalar,
                        int64_t after, int64_t reject)
{
   rt_signal_t *s = container_of(ss, rt_signal_t, shared);

   TRACE("_sched_waveform_s %s+%d value=%"PRIi64" after=%s reject=%s",
         istr(tree_ident(s->where)), offset, scalar, fmt_time(after),
         fmt_time(reject));

   check_postponed(after);

   rt_model_t *m = get_model();
   rt_nexus_t *n = split_nexus(m, s, offset, 1);

   rt_value_t value = alloc_value(m, n);
   value.qword = scalar;

   sched_driver(m, n, after, reject, value);
}

void x_sched_waveform(sig_shared_t *ss, uint32_t offset, void *values,
                      int32_t count, int64_t after, int64_t reject)
{
   rt_signal_t *s = container_of(ss, rt_signal_t, shared);

   TRACE("_sched_waveform %s+%d value=%s count=%d after=%s reject=%s",
         istr(tree_ident(s->where)), offset, fmt_values(values, count),
         count, fmt_time(after), fmt_time(reject));

   check_postponed(after);

   rt_model_t *m = get_model();
   rt_nexus_t *n = split_nexus(m, s, offset, count);
   char *vptr = values;
   for (; count > 0; n = n->chain) {
      count -= n->width;
      assert(count >= 0);

      const size_t valuesz = n->width * n->size;
      rt_value_t value = alloc_value(m, n);
      copy_value_ptr(n, &value, vptr);
      vptr += valuesz;

      sched_driver(m, n, after, reject, value);
   }
}

int32_t x_test_net_event(sig_shared_t *ss, uint32_t offset, int32_t count)
{
   rt_signal_t *s = container_of(ss, rt_signal_t, shared);

   TRACE("_test_net_event %s offset=%d count=%d",
         istr(tree_ident(s->where)), offset, count);

   rt_model_t *m = get_model();
   rt_nexus_t *n = split_nexus(m, s, offset, count);
   for (; count > 0; n = n->chain) {
      rt_net_t *net = get_net(m, n);
      if (net->last_event == m->now && net->event_delta == m->iteration)
         return 1;

      count -= n->width;
      assert(count >= 0);
   }

   return 0;
}

int32_t x_test_net_active(sig_shared_t *ss, uint32_t offset, int32_t count)
{
   rt_signal_t *s = container_of(ss, rt_signal_t, shared);

   TRACE("_test_net_active %s offset=%d count=%d",
         istr(tree_ident(s->where)), offset, count);

   rt_model_t *m = get_model();
   rt_nexus_t *n = split_nexus(m, s, offset, count);
   for (; count > 0; n = n->chain) {
      rt_net_t *net = get_net(m, n);
      if (net->last_active == m->now && net->active_delta == m->iteration)
         return 1;

      count -= n->width;
      assert(count >= 0);
   }

   return 0;
}

void x_sched_event(sig_shared_t *ss, uint32_t offset, int32_t count, bool recur,
                   sig_shared_t *wake_ss)
{
   rt_signal_t *s = container_of(ss, rt_signal_t, shared);

   TRACE("_sched_event %s+%d count=%d recur=%d proc %s",
         istr(tree_ident(s->where)), offset, count, recur,
         wake_ss ? "(implicit)" : istr(active_proc->name));

   rt_wakeable_t *wake;
   if (wake_ss != NULL)
      wake = &(container_of(wake_ss, rt_implicit_t, signal.shared)->wakeable);
   else
      wake = &(active_proc->wakeable);

   rt_model_t *m = get_model();
   rt_nexus_t *n = split_nexus(m, s, offset, count);
   for (; count > 0; n = n->chain) {
      sched_event(m, &(get_net(m, n)->pending), wake, recur);

      count -= n->width;
      assert(count >= 0);
   }
}

void x_alias_signal(sig_shared_t *ss, tree_t where)
{
   rt_signal_t *s = container_of(ss, rt_signal_t, shared);

   TRACE("alias signal %s to %s", istr(tree_ident(s->where)),
         istr(tree_ident(where)));

   rt_alias_t *a = xcalloc(sizeof(rt_alias_t));
   a->where  = where;
   a->signal = s;
   a->chain  = active_scope->aliases;

   active_scope->aliases = a;
}

void x_claim_tlab(void)
{
   TRACE("claiming TLAB for private use (used %zu/%d)",
         __nvc_tlab.alloc - __nvc_tlab.base, TLAB_SIZE);

   assert(tlab_valid(__nvc_tlab));
   assert(__nvc_tlab.alloc > __nvc_tlab.base);

   tlab_move(__nvc_tlab, active_proc->tlab);
}

int64_t x_last_event(sig_shared_t *ss, uint32_t offset, int32_t count)
{
   rt_signal_t *s = container_of(ss, rt_signal_t, shared);

   TRACE("_last_event %s offset=%d count=%d",
         istr(tree_ident(s->where)), offset, count);

   int64_t last = TIME_HIGH;

   rt_model_t *m = get_model();
   rt_nexus_t *n = split_nexus(m, s, offset, count);
   for (; count > 0; n = n->chain) {
      rt_net_t *net = get_net(m, n);
      if (net->last_event <= m->now)
         last = MIN(last, m->now - net->last_event);

      count -= n->width;
      assert(count >= 0);
   }

   return last;
}

int64_t x_last_active(sig_shared_t *ss, uint32_t offset, int32_t count)
{
   rt_signal_t *s = container_of(ss, rt_signal_t, shared);

   TRACE("_last_active %s offset=%d count=%d",
         istr(tree_ident(s->where)), offset, count);

   int64_t last = TIME_HIGH;

   rt_model_t *m = get_model();
   rt_nexus_t *n = split_nexus(m, s, offset, count);
   for (; count > 0; n = n->chain) {
      rt_net_t *net = get_net(m, n);
      if (net->last_active <= m->now)
         last = MIN(last, m->now - net->last_active);

      count -= n->width;
      assert(count >= 0);
   }

   return last;
}

void x_map_signal(sig_shared_t *src_ss, uint32_t src_offset,
                  sig_shared_t *dst_ss, uint32_t dst_offset,
                  uint32_t src_count, uint32_t dst_count,
                  ffi_closure_t *closure)
{
   rt_signal_t *src_s = container_of(src_ss, rt_signal_t, shared);
   rt_signal_t *dst_s = container_of(dst_ss, rt_signal_t, shared);

   TRACE("map signal %s+%d to %s+%d count %d/%d%s",
         istr(tree_ident(src_s->where)), src_offset,
         istr(tree_ident(dst_s->where)), dst_offset,
         src_count, dst_count, closure ? " converted" : "");

   assert(src_count == dst_count || closure != NULL);

   rt_conv_func_t *conv_func = NULL;
   if (closure != NULL) {
      size_t bufsz = dst_s->shared.size;
      if (dst_s->parent->kind == SCOPE_SIGNAL) {
         rt_scope_t *root = dst_s->parent;
         while (root->parent->kind == SCOPE_SIGNAL)
            root = root->parent;
         bufsz = root->size;
      }

      TRACE("need %zu bytes for conversion function buffer", bufsz);

      conv_func = xmalloc_flex(sizeof(rt_conv_func_t), 1, bufsz);
      conv_func->closure = *closure;
      conv_func->refcnt  = 0;
      conv_func->bufsz   = bufsz;
   }

   rt_model_t *m = get_model();

   rt_nexus_t *src_n = split_nexus(m, src_s, src_offset, src_count);
   rt_nexus_t *dst_n = split_nexus(m, dst_s, dst_offset, dst_count);

   while (src_count > 0 && dst_count > 0) {
      if (src_n->width > dst_n->width && closure == NULL)
         clone_nexus(m, src_n, dst_n->width, NULL);
      else if (src_n->width < dst_n->width && closure == NULL)
         clone_nexus(m, dst_n, src_n->width, NULL);

      assert(src_n->width == dst_n->width || closure != NULL);
      assert(src_n->size == dst_n->size || closure != NULL);

      // For inout ports and ports with conversion functions the driving
      // value and the effective value may be different so 'EVENT and
      // 'ACTIVE are not necessarily equal for all signals attached to
      // the same net
      if (((dst_n->flags | src_n->flags) & NET_F_EFFECTIVE) == 0) {
         if (src_n->net == NULL)
            src_n->net = get_net(m, dst_n);
         else {
            assert(dst_n->net == NULL);
            dst_n->net = get_net(m, src_n);
         }
      }
      else {
         src_n->flags |= NET_F_EFFECTIVE;
         dst_n->flags |= NET_F_EFFECTIVE;
      }

      rt_source_t *port = add_source(m, dst_n, SOURCE_PORT);
      port->u.port.input = src_n;

      if (conv_func != NULL) {
         port->u.port.conv_func = conv_func;
         conv_func->refcnt++;
         src_n->flags |= NET_F_EFFECTIVE;
         dst_n->flags |= NET_F_EFFECTIVE;
      }

      port->chain_output = src_n->outputs;
      src_n->outputs = port;

      src_count -= src_n->width;
      dst_count -= dst_n->width;
      assert(src_count >= 0);
      assert(dst_count >= 0);

      src_n = src_n->chain;
      dst_n = dst_n->chain;
   }
}

void x_map_const(sig_shared_t *ss, uint32_t offset,
                 const uint8_t *values, uint32_t count)
{
   rt_signal_t *s = container_of(ss, rt_signal_t, shared);

   TRACE("map const %s to %s+%d count %d", fmt_values(values, count),
         istr(tree_ident(s->where)), offset, count);

   rt_model_t *m = get_model();
   rt_nexus_t *n = split_nexus(m, s, offset, count);
   for (; count > 0; n = n->chain) {
      void *driving = n->resolved + 2*n->signal->shared.size;
      memcpy(driving, values, n->width * n->size);

      memcpy(n->resolved, values, n->width * n->size);
      values += n->width * n->size;

      count -= n->width;
      assert(count >= 0);
   }
}

void x_push_scope(tree_t where, int32_t size)
{
   TRACE("push scope %s size=%d", istr(tree_ident(where)), size);

   ident_t name = tree_ident(where);
   if (active_scope->kind == SCOPE_SIGNAL)
      name = ident_prefix(active_scope->name, name, '.');

   rt_scope_t *s = xcalloc(sizeof(rt_scope_t));
   s->where    = where;
   s->name     = name;
   s->kind     = SCOPE_SIGNAL;
   s->parent   = active_scope;
   s->chain    = active_scope->child;
   s->size     = size;
   s->privdata = mptr_new(get_model()->mspace, "push scope privdata");

   type_t type = tree_type(where);
   if (type_kind(type) == T_SUBTYPE && type_has_resolution(type))
      s->flags |= SCOPE_F_RESOLVED;

   active_scope->child = s;
   active_scope = s;

   signals_tail = &(s->signals);
}

void x_pop_scope(void)
{
   TRACE("pop scope %s", istr(tree_ident(active_scope->where)));

   if (unlikely(active_scope->kind != SCOPE_SIGNAL))
      fatal_trace("cannot pop non-signal scope");

   int offset = INT_MAX;
   for (rt_scope_t *s = active_scope->child; s; s = s->chain)
      offset = MIN(offset, s->offset);
   for (rt_signal_t *s = active_scope->signals; s; s = s->chain)
      offset = MIN(offset, s->shared.offset);
   active_scope->offset = offset;

   active_scope = active_scope->parent;

   for (signals_tail = &(active_scope->signals);
        *signals_tail != NULL;
        signals_tail = &((*signals_tail)->chain))
      ;
}

bool x_driving(sig_shared_t *ss, uint32_t offset, int32_t count)
{
   rt_signal_t *s = container_of(ss, rt_signal_t, shared);

   TRACE("_driving %s offset=%d count=%d",
         istr(tree_ident(s->where)), offset, count);

   int ntotal = 0, ndriving = 0;
   bool found = false;
   rt_model_t *m = get_model();
   rt_nexus_t *n = split_nexus(m, s, offset, count);
   for (; count > 0; n = n->chain) {
      if (n->n_sources > 0) {
         rt_source_t *src = find_driver(n);
         if (src != NULL) {
            if (!src->disconnected) ndriving++;
            found = true;
         }
      }

      ntotal++;
      count -= n->width;
      assert(count >= 0);
   }

   if (!found)
      jit_msg(NULL, DIAG_FATAL, "process %s does not contain a driver for %s",
              istr(active_proc->name), istr(tree_ident(s->where)));

   return ntotal == ndriving;
}

void *x_driving_value(sig_shared_t *ss, uint32_t offset, int32_t count)
{
   rt_signal_t *s = container_of(ss, rt_signal_t, shared);

   TRACE("_driving_value %s offset=%d count=%d",
         istr(tree_ident(s->where)), offset, count);

   void *result = local_alloc(s->shared.size);

   uint8_t *p = result;
   rt_model_t *m = get_model();
   rt_nexus_t *n = split_nexus(m, s, offset, count);
   for (; count > 0; n = n->chain) {
      rt_source_t *src = find_driver(n);
      if (src == NULL)
         jit_msg(NULL, DIAG_FATAL, "process %s does not contain a driver "
                 "for %s", istr(active_proc->name), istr(tree_ident(s->where)));

      memcpy(p, value_ptr(n, &(src->u.driver.waveforms.value)),
             n->width * n->size);
      p += n->width * n->size;

      count -= n->width;
      assert(count >= 0);
   }

   return result;
}

sig_shared_t *x_implicit_signal(uint32_t count, uint32_t size, tree_t where,
                                uint32_t kind, ffi_closure_t *closure)
{
   TRACE("_implicit_signal %s count=%d size=%d kind=%d",
         istr(tree_ident(where)), count, size, kind);

   rt_model_t *m = get_model();

   if (m->implicitq == NULL)
      m->implicitq = workq_new(m);

   const size_t datasz = MAX(2 * count * size, 8);
   rt_implicit_t *imp = static_alloc(m, sizeof(rt_implicit_t) + datasz);
   setup_signal(m, &(imp->signal), where, count, size, NET_F_IMPLICIT, 0);

   imp->closure = *closure;
   imp->wakeable.kind = W_IMPLICIT;

   jit_scalar_t result;
   if (!jit_try_call(m->jit, imp->closure.handle, &result,
                     imp->closure.context))
      m->force_stop = true;

   assert(size * count == 1);
   memcpy(imp->signal.shared.data, &result.integer, imp->signal.shared.size);

   return &(imp->signal.shared);
}

void x_disconnect(sig_shared_t *ss, uint32_t offset, int32_t count,
                  int64_t after, int64_t reject)
{
   rt_signal_t *s = container_of(ss, rt_signal_t, shared);

   TRACE("_disconnect %s+%d len=%d after=%s reject=%s",
         istr(tree_ident(s->where)), offset, count, fmt_time(after),
         fmt_time(reject));

   check_postponed(after);

   rt_model_t *m = get_model();
   rt_nexus_t *n = split_nexus(m, s, offset, count);
   for (; count > 0; n = n->chain) {
      count -= n->width;
      assert(count >= 0);

      sched_disconnect(m, n, after, reject);
   }
}

void x_force(sig_shared_t *ss, uint32_t offset, int32_t count, void *values)
{
   rt_signal_t *s = container_of(ss, rt_signal_t, shared);

   TRACE("force signal %s+%d value=%s count=%d", istr(tree_ident(s->where)),
         offset, fmt_values(values, count), count);

   check_postponed(0);

   rt_model_t *m = get_model();
   rt_nexus_t *n = split_nexus(m, s, offset, count);
   char *vptr = values;
   for (; count > 0; n = n->chain) {
      count -= n->width;
      assert(count >= 0);

      if (!(n->flags & NET_F_FORCED)) {
         n->flags |= NET_F_FORCED;
         n->forcing = alloc_value(m, n);
      }

      copy_value_ptr(n, &(n->forcing), vptr);
      vptr += n->width * n->size;

      deltaq_insert_force_release(m, 0, n);
   }
}

void x_release(sig_shared_t *ss, uint32_t offset, int32_t count)
{
   rt_signal_t *s = container_of(ss, rt_signal_t, shared);

   TRACE("release signal %s+%d count=%d", istr(tree_ident(s->where)),
         offset, count);

   check_postponed(0);

   rt_model_t *m = get_model();
   rt_nexus_t *n = split_nexus(m, s, offset, count);
   for (; count > 0; n = n->chain) {
      count -= n->width;
      assert(count >= 0);

      if (n->flags & NET_F_FORCED) {
         n->flags &= ~NET_F_FORCED;
         free_value(n, n->forcing);
         n->forcing.qword = 0;
      }

      deltaq_insert_force_release(m, 0, n);
   }
}

void x_resolve_signal(sig_shared_t *ss, rt_resolution_t *resolution)
{
   rt_signal_t *s = container_of(ss, rt_signal_t, shared);

   TRACE("resolve signal %s", istr(tree_ident(s->where)));

   s->resolution = memo_resolution_fn(get_model(), s, resolution);

   // Copy R_IDENT into the nexus flags to avoid rt_resolve_nexus_fast
   // having to dereference the resolution pointer in the common case
   if (s->resolution->flags & R_IDENT) {
      s->flags |= NET_F_R_IDENT;

      rt_nexus_t *n = &(s->nexus);
      for (int i = 0; i < s->n_nexus; i++, n = n->chain)
         n->flags |= NET_F_R_IDENT;
   }
}

void x_resolve_signal2(sig_shared_t *ss, jit_handle_t handle, void *context,
                       int32_t ileft, int32_t nlits)
{
   rt_resolution_t resolution = {
      .closure = {
         .handle  = handle,
         .context = context,
      },
      .ileft = ileft,
      .nlits = nlits,
   };

   x_resolve_signal(ss, &resolution);
}
