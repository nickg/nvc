//
//  Copyright (C) 2011-2023  Nick Gasson
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
#include "debug.h"
#include "eval.h"
#include "hash.h"
#include "jit/jit-exits.h"
#include "jit/jit.h"
#include "lib.h"
#include "option.h"
#include "rt/heap.h"
#include "rt/model.h"
#include "rt/structs.h"
#include "thread.h"
#include "tree.h"
#include "type.h"
#include "vlog/vlog-node.h"

#include <assert.h>
#include <inttypes.h>
#include <stdlib.h>
#include <string.h>

typedef struct _rt_callback rt_callback_t;
typedef struct _memblock memblock_t;

typedef struct _rt_callback {
   rt_event_fn_t  fn;
   void          *user;
   rt_callback_t *next;
} rt_callback_t;

typedef enum {
   EVENT_TIMEOUT,
   EVENT_DRIVER,
   EVENT_PROCESS,
   EVENT_DISCONNECT,
} event_kind_t;

#define MEMBLOCK_LINE_SZ 64
#define MEMBLOCK_PAGE_SZ 0x800000

typedef struct _memblock {
   memblock_t *chain;
   unsigned    free;
   unsigned    pagesz;
   char       *ptr;
} memblock_t;

typedef struct {
   rt_nexus_t *nexus;
   uint8_t     data[0];
} rt_deposit_t;

typedef struct {
   waveform_t    *free_waveforms;
   tlab_t         tlab;
   tlab_t         spare_tlab;
   rt_wakeable_t *active_obj;
   rt_scope_t    *active_scope;
} __attribute__((aligned(64))) model_thread_t;

typedef struct _rt_model {
   tree_t             top;
   hash_t            *scopes;
   rt_scope_t        *root;
   mspace_t          *mspace;
   jit_t             *jit;
   rt_nexus_t        *nexuses;
   rt_nexus_t       **nexus_tail;
   delta_cycle_t      stop_delta;
   int                iteration;
   uint64_t           now;
   bool               can_create_delta;
   bool               next_is_delta;
   bool               force_stop;
   unsigned           n_signals;
   heap_t            *eventq_heap;
   ihash_t           *res_memo;
   rt_watch_t        *watches;
   workq_t           *procq;
   workq_t           *delta_procq;
   workq_t           *driverq;
   workq_t           *delta_driverq;
   workq_t           *effq;
   workq_t           *postponedq;
   workq_t           *implicitq;
   rt_callback_t     *global_cbs[RT_LAST_EVENT];
   cover_tagging_t   *cover;
   nvc_rusage_t       ready_rusage;
   nvc_lock_t         memlock;
   memblock_t        *memblocks;
   model_thread_t    *threads[MAX_THREADS];
   ptr_list_t         eventsigs;
} rt_model_t;

#define FMT_VALUES_SZ   128
#define NEXUS_INDEX_MIN 8
#define TRACE_SIGNALS   1
#define WAVEFORM_CHUNK  256
#define PENDING_MIN     4

#define TRACE(...) do {                                 \
      if (unlikely(__trace_on))                         \
         __model_trace(get_model(), __VA_ARGS__);       \
   } while (0)

#define MODEL_ENTRY(m)                                                  \
   rt_model_t *__save __attribute__((unused, cleanup(__model_exit)));   \
   __model_entry(m, &__save);                                           \

static __thread rt_model_t *__model = NULL;

static bool __trace_on = false;

static void *driving_value(rt_nexus_t *nexus);
static void *source_value(rt_nexus_t *nexus, rt_source_t *src);
static void free_value(rt_nexus_t *n, rt_value_t v);
static rt_nexus_t *clone_nexus(rt_model_t *m, rt_nexus_t *old, int offset);
static void update_implicit_signal(rt_model_t *m, rt_implicit_t *imp);
static void async_run_process(void *context, void *arg);
static void async_update_property(void *context, void *arg);
static void async_update_driver(void *context, void *arg);
static void async_fast_driver(void *context, void *arg);
static void async_fast_all_drivers(void *context, void *arg);
static void async_update_driving(void *context, void *arg);
static void async_disconnect(void *context, void *arg);
static void async_deposit(void *context, void *arg);

static int fmt_time_r(char *buf, size_t len, int64_t t, const char *sep)
{
   static const struct {
      int64_t time;
      const char *unit;
   } units[] = {
      { INT64_C(1), "fs" },
      { INT64_C(1000), "ps" },
      { INT64_C(1000000), "ns" },
      { INT64_C(1000000000), "us" },
      { INT64_C(1000000000000), "ms" },
      { 0, NULL }
   };

   int u = 0;
   while (units[u + 1].unit && (t % units[u + 1].time == 0))
      ++u;

   return checked_sprintf(buf, len, "%"PRIi64"%s%s",
                          t / units[u].time, sep, units[u].unit);
}

__attribute__((format(printf, 2, 3)))
static void __model_trace(rt_model_t *m, const char *fmt, ...)
{
   va_list ap;
   va_start(ap, fmt);

   static nvc_lock_t lock = 0;
   {
      SCOPED_LOCK(lock);

      if (m->iteration < 0)
         fprintf(stderr, "TRACE (init): ");
      else {
         char buf[64];
         fmt_time_r(buf, sizeof(buf), m->now, "");
         fprintf(stderr, "TRACE %s+%d: ", buf, m->iteration);
      }
      vfprintf(stderr, fmt, ap);
      fprintf(stderr, "\n");
      fflush(stderr);
   }

   va_end(ap);
}

static const char *trace_time(uint64_t value)
{
   static __thread char buf[2][32];
   static __thread int which = 0;

   which ^= 1;
   fmt_time_r(buf[which], 32, value, "");
   return buf[which];
}

static const char *trace_states(bit_mask_t *mask)
{
   static __thread text_buf_t *tb = NULL;

   if (tb == NULL)
      tb = tb_new();

   tb_rewind(tb);
   tb_append(tb, '{');

   int bit = -1;
   while (mask_iter(mask, &bit))
      tb_printf(tb, "%s%d", tb_len(tb) > 1 ? "," : "", bit);

   tb_append(tb, '}');

   return tb_get(tb);
}

static void model_diag_cb(diag_t *d, void *arg)
{
   rt_model_t *m = arg;

   if (m->iteration < 0)
      diag_printf(d, "(init): ");
   else  {
      char tmbuf[64];
      fmt_time_r(tmbuf, sizeof(tmbuf), m->now, "");

      diag_printf(d, "%s+%d: ", tmbuf, m->iteration);
   }
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

static model_thread_t *model_thread(rt_model_t *m)
{
   const int my_id = thread_id();

   if (unlikely(m->threads[my_id] == NULL))
      return (m->threads[my_id] = xcalloc(sizeof(model_thread_t)));

   return m->threads[my_id];
}

static void *static_alloc(rt_model_t *m, size_t size)
{
   const int nlines = ALIGN_UP(size, MEMBLOCK_LINE_SZ) / MEMBLOCK_LINE_SZ;

   RT_LOCK(m->memlock);

   memblock_t *mb = m->memblocks;
   if (mb == NULL || mb->free < nlines) {
      mb = xmalloc(sizeof(memblock_t));
      mb->pagesz = MAX(MEMBLOCK_PAGE_SZ, nlines * MEMBLOCK_LINE_SZ);
      mb->chain  = m->memblocks;
      mb->free   = mb->pagesz / MEMBLOCK_LINE_SZ;
      mb->ptr    = map_huge_pages(MEMBLOCK_LINE_SZ, mb->pagesz);

      m->memblocks = mb;
   }

   assert(nlines <= mb->free);

   void *ptr = mb->ptr + mb->pagesz - mb->free * MEMBLOCK_LINE_SZ;
   mb->free -= nlines;
   return ptr;
}

static void global_event(rt_model_t *m, rt_event_t kind)
{
   rt_callback_t *list = m->global_cbs[kind];
   m->global_cbs[kind] = NULL;

   for (rt_callback_t *it = list, *tmp; it; it = tmp) {
      tmp = it->next;
      (*it->fn)(m, it->user);
      free(it);
   }
}

static void scope_deps_cb(ident_t unit_name, void *__ctx)
{
   // TODO: this should be redundant now we have the package init op

   rt_model_t *m = __ctx;

   object_t *obj = lib_load_handler(unit_name);
   if (obj == NULL) {
      warnf("missing dependency %s", istr(unit_name));
      return;
   }

   tree_t unit = tree_from_object(obj);
   if (unit == NULL)
      return;

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

   list_add(&m->root->children, s);

   hash_put(m->scopes, unit, s);

   tree_walk_deps(unit, scope_deps_cb, m);

   if (kind == T_PACKAGE) {
      tree_t body = body_of(unit);
      if (body != NULL)
         tree_walk_deps(body, scope_deps_cb, m);
   }
}

static rt_scope_t *scope_for_verilog(rt_model_t *m, vlog_node_t scope,
                                     ident_t prefix)
{
   rt_scope_t *s = xcalloc(sizeof(rt_scope_t));
   s->where    = NULL;
   s->name     = ident_prefix(prefix, vlog_ident(scope), '.');
   s->kind     = SCOPE_INSTANCE;
   s->privdata = mptr_new(m->mspace, "block privdata");

   hash_put(m->scopes, scope, s);

   const int nstmts = vlog_stmts(scope);
   for (int i = 0; i < nstmts; i++) {
      vlog_node_t v = vlog_stmt(scope, i);
      switch (vlog_kind(v)) {
      case V_ALWAYS:
      case V_INITIAL:
         {
            ident_t name = vlog_ident(v);
            ident_t sym = ident_prefix(s->name, name, '.');

            rt_proc_t *p = xcalloc(sizeof(rt_proc_t));
            p->where     = NULL;
            p->name      = ident_prefix(NULL, ident_downcase(name), ':');
            p->handle    = jit_lazy_compile(m->jit, sym);
            p->scope     = s;
            p->privdata  = mptr_new(m->mspace, "process privdata");

            p->wakeable.kind      = W_PROC;
            p->wakeable.pending   = false;
            p->wakeable.postponed = false;
            p->wakeable.delayed   = false;

            list_add(&s->procs, p);
         }
         break;

      default:
         break;
      }
   }

   return s;
}

static rt_scope_t *scope_for_block(rt_model_t *m, tree_t block, ident_t prefix)
{
   rt_scope_t *s = xcalloc(sizeof(rt_scope_t));
   s->where    = block;
   s->name     = ident_prefix(prefix, tree_ident(block), '.');
   s->kind     = SCOPE_INSTANCE;
   s->privdata = mptr_new(m->mspace, "block privdata");

   hash_put(m->scopes, block, s);

   tree_t hier = tree_decl(block, 0);
   assert(tree_kind(hier) == T_HIER);

   ident_t path = tree_ident(hier);

   const int ndecls = tree_decls(block);
   for (int i = 0; i < ndecls; i++) {
      tree_t d = tree_decl(block, i);
      switch (tree_kind(d)) {
      case T_PACK_INST:
         {
            rt_scope_t *p = xcalloc(sizeof(rt_scope_t));
            p->where    = d;
            p->name     = ident_prefix(s->name, tree_ident(d), '.');
            p->kind     = SCOPE_PACKAGE;
            p->privdata = mptr_new(m->mspace, "pack inst privdata");

            hash_put(m->scopes, d, p);

            list_add(&s->children, p);
         }

      default:
         break;
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
            list_add(&s->children, c);
         }
         break;

      case T_VERILOG:
         {
            rt_scope_t *c = scope_for_verilog(m, tree_vlog(t), s->name);
            c->parent = s;
            list_add(&s->children, c);
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

            p->wakeable.kind      = W_PROC;
            p->wakeable.pending   = false;
            p->wakeable.postponed = !!(tree_flags(t) & TREE_F_POSTPONED);
            p->wakeable.delayed   = false;

            list_add(&s->procs, p);
         }
         break;

      case T_PSL:
         {
            ident_t name = tree_ident(t);
            ident_t sym = ident_prefix(s->name, name, '.');

            rt_prop_t *p = xcalloc(sizeof(rt_prop_t));
            p->where  = tree_psl(t);
            p->handle = jit_lazy_compile(m->jit, sym);
            p->scope  = s;
            p->name   = sym;

            p->wakeable.kind      = W_PROPERTY;
            p->wakeable.pending   = false;
            p->wakeable.postponed = false;
            p->wakeable.delayed   = false;

            list_add(&s->properties, p);
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

#if !RT_MULTITHREADED
   workq_not_thread_safe(m->procq);
   workq_not_thread_safe(m->delta_procq);
#endif
   workq_not_thread_safe(m->postponedq);
   workq_not_thread_safe(m->driverq);
   workq_not_thread_safe(m->delta_driverq);
   workq_not_thread_safe(m->effq);

   tree_walk_deps(top, scope_deps_cb, m);

   rt_scope_t *s = NULL;
   tree_t s0 = tree_stmt(top, 0);
   if (tree_kind(s0) == T_VERILOG)
      s = scope_for_verilog(m, tree_vlog(s0), lib_name(lib_work()));
   else
      s = scope_for_block(m, s0, lib_name(lib_work()));

   list_add(&m->root->children, s);

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

static rt_wakeable_t *get_active_wakeable(void)
{
   return __model ? model_thread(__model)->active_obj : NULL;
}

rt_proc_t *get_active_proc(void)
{
   rt_wakeable_t *obj = get_active_wakeable();
   if (obj == NULL)
      return NULL;

   assert(obj->kind == W_PROC);
   return container_of(obj, rt_proc_t, wakeable);
}

static void free_waveform(rt_model_t *m, waveform_t *w)
{
   model_thread_t *thread = model_thread(m);
   w->next = thread->free_waveforms;
   thread->free_waveforms = w;
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
            free_waveform(m, it);
         }
         break;

      case SOURCE_PORT:
         if (s->u.port.conv_func != NULL) {
            assert(s->u.port.conv_func->refcnt > 0);
            if (--(s->u.port.conv_func->refcnt) == 0)
               free(s->u.port.conv_func);
         }
         break;

      case SOURCE_FORCING:
         break;
      }
   }


   if (n->pending != NULL && pointer_tag(n->pending) == 0)
      free(n->pending);
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
   list_foreach(rt_proc_t *, it, scope->procs) {
      mptr_free(m->mspace, &(it->privdata));
      tlab_release(&(it->tlab));
      free(it);
   }
   list_free(&scope->procs);

   list_foreach(rt_signal_t *, it, scope->signals) {
      cleanup_signal(m, it);
   }
   list_free(&scope->signals);

   list_foreach(rt_alias_t *, it, scope->aliases) {
      free(it);
   }
   list_free(&scope->aliases);

   list_foreach(rt_prop_t *, it, scope->properties) {
      mask_free(&it->state);
      mask_free(&it->newstate);
      free(it);
   }
   list_free(&scope->properties);

   list_foreach(rt_scope_t *, it, scope->children) {
      cleanup_scope(m, it);
   }
   list_free(&scope->children);

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

      notef("setup:%ums run:%ums user:%ums sys:%ums maxrss:%ukB static:%ukB",
            m->ready_rusage.ms, ru.ms, ru.user, ru.sys, ru.rss, mem / 1024);
   }

   while (heap_size(m->eventq_heap) > 0) {
      void *e = heap_extract_min(m->eventq_heap);
      if (pointer_tag(e) == EVENT_TIMEOUT)
         free(untag_pointer(e, rt_callback_t));
   }

   cleanup_scope(m, m->root);

   for (int i = 0; i < MAX_THREADS; i++) {
      model_thread_t *thread = m->threads[i];
      if (thread != NULL) {
         tlab_release(&thread->tlab);
         free(thread);
      }
   }

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
      free(it);
   }

   for (int i = 0; i < RT_LAST_EVENT; i++) {
      for (rt_callback_t *it = m->global_cbs[i], *tmp; it; it = tmp) {
         tmp = it->next;
         free(it);
      }
   }

   for (memblock_t *mb = m->memblocks, *tmp; mb; mb = tmp) {
      tmp = mb->chain;
      nvc_munmap(mb->ptr, MEMBLOCK_PAGE_SZ);
      free(mb);
   }

   heap_free(m->eventq_heap);
   hash_free(m->scopes);
   ihash_free(m->res_memo);
   list_free(&m->eventsigs);
   free(m);
}

rt_signal_t *find_signal(rt_scope_t *scope, tree_t decl)
{
   list_foreach(rt_signal_t *, s, scope->signals) {
      if (s->where == decl)
         return s;
   }

   list_foreach(rt_alias_t *, a, scope->aliases) {
      if (a->where == decl)
         return a->signal;
   }

   return NULL;
}

rt_proc_t *find_proc(rt_scope_t *scope, tree_t proc)
{
   list_foreach(rt_proc_t *, p, scope->procs) {
      if (p->where == proc)
         return p;
   }

   return NULL;
}

rt_watch_t *find_watch(rt_nexus_t *n, sig_event_fn_t fn)
{
   if (n->pending == NULL)
      return NULL;
   else if (pointer_tag(n->pending) == 1) {
      rt_wakeable_t *obj = untag_pointer(n->pending, rt_wakeable_t);
      if (obj->kind == W_WATCH) {
         rt_watch_t *w = container_of(obj, rt_watch_t, wakeable);
         if (w->fn == fn)
            return w;
      }

      return NULL;
   }
   else {
      rt_pending_t *p = untag_pointer(n->pending, rt_pending_t);

      for (int i = 0; i < p->count; i++) {
         rt_wakeable_t *obj = untag_pointer(p->wake[i], rt_wakeable_t);
         if (obj->kind == W_WATCH) {
            rt_watch_t *w = container_of(obj, rt_watch_t, wakeable);
            if (w->fn == fn)
               return w;
         }
      }

      return NULL;
   }
}

rt_scope_t *find_scope(rt_model_t *m, tree_t container)
{
   return hash_get(m->scopes, container);
}

rt_scope_t *child_scope(rt_scope_t *scope, tree_t decl)
{
   list_foreach(rt_scope_t *, s, scope->children) {
      if (s->where == decl)
         return s;
   }

   return NULL;
}

rt_scope_t *child_scope_at(rt_scope_t *scope, int index)
{
   return (rt_scope_t *)scope->children->items[index];
}

const void *signal_value(rt_signal_t *s)
{
   return s->shared.data;
}

const void *signal_last_value(rt_signal_t *s)
{
   return s->shared.data + s->shared.size;
}

uint8_t signal_size(rt_signal_t *s)
{
   return s->nexus.size;
}

uint32_t signal_width(rt_signal_t *s)
{
   return s->shared.size / s->nexus.size;
}

size_t signal_expand(rt_signal_t *s, uint64_t *buf, size_t max)
{
   const size_t total = s->shared.size / s->nexus.size;

#define SIGNAL_READ_EXPAND_U64(type) do {                               \
      const type *sp = (type *)s->shared.data;                          \
      for (int i = 0; i < max && i < total; i++)                        \
         buf[i] = sp[i];                                                \
   } while (0)

   FOR_ALL_SIZES(s->nexus.size, SIGNAL_READ_EXPAND_U64);

   return total;
}

static inline void set_pending(rt_wakeable_t *wake)
{
   assert(!wake->pending);
   assert(!wake->delayed);
   wake->pending = true;
}

static void deltaq_insert_proc(rt_model_t *m, uint64_t delta, rt_proc_t *proc)
{
   if (delta == 0) {
      set_pending(&proc->wakeable);
      workq_do(m->delta_procq, async_run_process, proc);
      m->next_is_delta = true;
   }
   else {
      assert(!proc->wakeable.delayed);
      proc->wakeable.delayed = true;

      void *e = tag_pointer(proc, EVENT_PROCESS);
      heap_insert(m->eventq_heap, m->now + delta, e);
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
      void *e = tag_pointer(source, EVENT_DRIVER);
      heap_insert(m->eventq_heap, m->now + delta, e);
   }
}

static void deltaq_insert_force_release(rt_model_t *m, rt_nexus_t *nexus)
{
   workq_do(m->delta_driverq, async_update_driving, nexus);
   m->next_is_delta = true;
}

static void deltaq_insert_deposit(rt_model_t *m, rt_deposit_t *deposit)
{
   workq_do(m->delta_driverq, async_deposit, deposit);
   m->next_is_delta = true;
}

static void deltaq_insert_disconnect(rt_model_t *m, uint64_t delta,
                                     rt_source_t *source)
{
   if (delta == 0) {
      workq_do(m->delta_driverq, async_disconnect, source);
      m->next_is_delta = true;
   }
   else {
      void *e = tag_pointer(source, EVENT_DISCONNECT);
      heap_insert(m->eventq_heap, m->now + delta, e);
   }
}

static void reset_process(rt_model_t *m, rt_proc_t *proc)
{
   TRACE("reset process %s", istr(proc->name));

   assert(!tlab_valid(proc->tlab));
   assert(!tlab_valid(model_thread(m)->tlab));   // Not used during reset

   model_thread_t *thread = model_thread(m);
   thread->active_obj = &(proc->wakeable);
   thread->active_scope = proc->scope;

   jit_scalar_t context = {
      .pointer = *mptr_get(proc->scope->privdata)
   };
   jit_scalar_t state = { .pointer = NULL };
   jit_scalar_t result;

   tlab_t tlab = jit_null_tlab(m->jit);

   if (jit_fastcall(m->jit, proc->handle, &result, state, context, &tlab))
      *mptr_get(proc->privdata) = result.pointer;
   else
      m->force_stop = true;

   thread->active_obj = NULL;
   thread->active_scope = NULL;
}

static void reset_property(rt_model_t *m, rt_prop_t *prop)
{
   TRACE("reset property %s", istr(prop->name));

   assert(!tlab_valid(model_thread(m)->tlab));   // Not used during reset

   model_thread_t *thread = model_thread(m);
   thread->active_obj = &(prop->wakeable);
   thread->active_scope = prop->scope;

   jit_scalar_t context = {
      .pointer = *mptr_get(prop->scope->privdata)
   };
   jit_scalar_t state = { .integer = -1 };
   jit_scalar_t result;

   tlab_t tlab = jit_null_tlab(m->jit);

   if (!jit_fastcall(m->jit, prop->handle, &result, context, state, &tlab))
      m->force_stop = true;

   TRACE("needs %"PRIi64" state bits", result.integer);

   mask_init(&prop->state, result.integer);
   mask_init(&prop->newstate, result.integer);

   mask_set(&prop->state, 0);

   thread->active_obj = NULL;
   thread->active_scope = NULL;
}

static void run_process(rt_model_t *m, rt_proc_t *proc)
{
   TRACE("run %sprocess %s", *mptr_get(proc->privdata) ? "" :  "stateless ",
         istr(proc->name));

   model_thread_t *thread = model_thread(m);

   assert(!tlab_valid(thread->spare_tlab));

   if (tlab_valid(proc->tlab)) {
      TRACE("using private TLAB at %p (%u used)", proc->tlab.base,
            proc->tlab.alloc);
      tlab_move(thread->tlab, thread->spare_tlab);
      tlab_move(proc->tlab, thread->tlab);
   }
   else if (!tlab_valid(thread->tlab))
      tlab_acquire(m->mspace, &thread->tlab);

   tlab_t *tlab = &thread->tlab;

   thread->active_obj = &(proc->wakeable);
   thread->active_scope = proc->scope;

   // Stateless processes have NULL privdata so pass a dummy pointer
   // value in so it can be distinguished from a reset
   jit_scalar_t state = {
      .pointer = *mptr_get(proc->privdata) ?: (void *)-1
   };

   jit_scalar_t result;
   jit_scalar_t context = {
      .pointer = *mptr_get(proc->scope->privdata)
   };

   if (!jit_fastcall(m->jit, proc->handle, &result, state, context, tlab))
      m->force_stop = true;

   thread->active_obj = NULL;
   thread->active_scope = NULL;

   if (tlab_valid(thread->tlab)) {
      // The TLAB is still valid which means the process finished
      // instead of suspending at a wait statement and none of the data
      // inside it can be live anymore
      assert(!tlab_valid(proc->tlab));
      tlab_reset(thread->tlab);

      if (tlab_valid(thread->spare_tlab))   // Surplus TLAB
         tlab_release(&thread->spare_tlab);
   }
   else {
      // Process must have claimed TLAB or otherwise it would be lost
      assert(tlab_valid(proc->tlab));
      if (tlab_valid(thread->spare_tlab))
         tlab_move(thread->spare_tlab, thread->tlab);
   }
}

static void reset_scope(rt_model_t *m, rt_scope_t *s)
{
   if (s->kind == SCOPE_INSTANCE || s->kind == SCOPE_PACKAGE) {
      TRACE("reset scope %s", istr(s->name));

      model_thread_t *thread = model_thread(m);
      thread->active_scope = s;

      jit_handle_t handle = jit_lazy_compile(m->jit, s->name);
      if (handle == JIT_HANDLE_INVALID)
         fatal_trace("failed to compile %s", istr(s->name));

      jit_scalar_t result, context = { .pointer = NULL };
      jit_scalar_t p2 = { .integer = 0 };

      if (s->parent != NULL)
         context.pointer = *mptr_get(s->parent->privdata);

      tlab_t tlab = jit_null_tlab(m->jit);

      if (jit_fastcall(m->jit, handle, &result, context, p2, &tlab))
         *mptr_get(s->privdata) = result.pointer;
      else {
         m->force_stop = true;
         return;
      }

      thread->active_scope = NULL;
   }

   list_foreach(rt_scope_t *, c, s->children)
      reset_scope(m, c);

   list_foreach(rt_proc_t *, p, s->procs)
      reset_process(m, p);

   list_foreach(rt_prop_t *, p, s->properties)
      reset_property(m, p);
}

static res_memo_t *memo_resolution_fn(rt_model_t *m, rt_signal_t *signal,
                                      ffi_closure_t closure, int64_t ileft,
                                      int32_t nlits, res_flags_t flags)
{
   // Optimise some common resolution functions by memoising them

   res_memo_t *memo = ihash_get(m->res_memo, closure.handle);
   if (memo != NULL)
      return memo;

   memo = static_alloc(m, sizeof(res_memo_t));
   memo->closure = closure;
   memo->flags   = flags;
   memo->ileft   = ileft;

   ihash_put(m->res_memo, memo->closure.handle, memo);

   if (nlits == 0 || nlits > 16)
      return memo;

   const vhdl_severity_t old_severity = set_exit_severity(SEVERITY_NOTE);

   jit_set_silent(m->jit, true);

   // Memoise the function for all two value cases

   for (int i = 0; i < nlits; i++) {
      for (int j = 0; j < nlits; j++) {
         int8_t args[2] = { i, j };
         jit_scalar_t result;
         if (jit_try_call(m->jit, memo->closure.handle, &result,
                          memo->closure.context, args, memo->ileft, 2)) {
            assert(result.integer < nlits && result.integer >= 0);
            memo->tab2[i][j] = result.integer;
         }
      }
   }

   // Memoise the function for all single value cases and determine if the
   // function behaves like the identity function

   bool identity = true;
   for (int i = 0; i < nlits; i++) {
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
         istr(jit_get_name(m->jit, closure.handle)),
         type_pp(tree_type(signal->where)));

   jit_set_silent(m->jit, false);
   jit_reset_exit_status(m->jit, 0);

   set_exit_severity(old_severity);

   return memo;
}

static inline void *nexus_effective(rt_nexus_t *n)
{
   return n->signal->shared.data + n->offset;
}

static inline void *nexus_last_value(rt_nexus_t *n)
{
   return n->signal->shared.data + n->offset + n->signal->shared.size;
}

static inline void *nexus_driving(rt_nexus_t *n)
{
   return n->signal->shared.data + n->offset + 2*n->signal->shared.size;
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
   rt_model_t *m = get_model();
   model_thread_t *thread = model_thread(m);

   if (tlab_valid(thread->tlab))
      return tlab_alloc(&thread->tlab, size);
   else
      return mspace_alloc(m->mspace, size);
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
   if (valuesz <= sizeof(rt_value_t)) {
#if __SANITIZE_ADDRESS__
      memcpy(v->bytes, p, valuesz);
#else
      v->qword = *(const uint64_t *)p;
#endif
   }
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
   else if (n->signal->resolution == NULL && kind != SOURCE_FORCING
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

   if (n->n_sources > 1)
      n->flags &= ~NET_F_FAST_DRIVER;

   src->chain_input  = NULL;
   src->chain_output = NULL;
   src->tag          = kind;
   src->disconnected = 0;
   src->fastqueued   = 0;
   src->sigqueued    = 0;

   switch (kind) {
   case SOURCE_DRIVER:
      {
         src->u.driver.proc  = NULL;
         src->u.driver.nexus = n;

         waveform_t *w0 = &(src->u.driver.waveforms);
         w0->when  = TIME_HIGH;
         w0->next  = NULL;
      }
      break;

   case SOURCE_PORT:
      src->u.port.conv_func = NULL;
      src->u.port.input     = NULL;
      src->u.port.output    = n;
      break;

   case SOURCE_FORCING:
      src->u.forcing = alloc_value(m, n);
      break;
   }

   return src;
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
      if (offset > 0) {
         const int tzc = __builtin_ctz(offset);
         shift = MIN(shift, tzc);
      }

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
   const unsigned offset = n->offset / n->size;

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

static waveform_t *alloc_waveform(rt_model_t *m)
{
   model_thread_t *thread = model_thread(m);

   if (thread->free_waveforms == NULL) {
      // Ensure waveforms are always within one cache line
      STATIC_ASSERT(sizeof(waveform_t) <= 32);
      char *mem = static_alloc(m, WAVEFORM_CHUNK * 32);
      for (int i = 1; i < WAVEFORM_CHUNK; i++)
         free_waveform(m, (waveform_t *)(mem + i*32));

      return (waveform_t *)mem;
   }
   else {
      waveform_t *w = thread->free_waveforms;
      thread->free_waveforms = w->next;
      __builtin_prefetch(w->next, 1, 1);
      w->next = NULL;
      return w;
   }
}

static void split_value(rt_nexus_t *nexus, rt_value_t *v_new,
                        rt_value_t *v_old, int offset)
{
   const int split = offset * nexus->size;
   const int oldsz = (offset + nexus->width) * nexus->size;
   const int newsz = nexus->width * nexus->size;

   if (split > sizeof(rt_value_t) && newsz > sizeof(rt_value_t)) {
      // Split the external memory with no copying
      v_new->ext = (char *)v_old->ext + split;
   }
   else if (newsz > sizeof(rt_value_t)) {
      // Wasting up to eight bytes at the start of the the old waveform
      char *ext = v_old->ext;
      v_old->qword = *(uint64_t *)ext;
      v_new->ext = ext + split;
   }
   else if (split > sizeof(rt_value_t)) {
      // Wasting up to eight bytes at the end of the the old waveform
      memcpy(v_new->bytes, v_old->ext + split, newsz);
   }
   else if (oldsz > sizeof(rt_value_t)) {
      // The memory backing this waveform is lost now but this can only
      // happen a bounded number of times as nexuses only ever shrink
      char *ext = v_old->ext;
      memcpy(v_new->bytes, ext + split, newsz);
      v_old->qword = *(uint64_t *)ext;
   }
   else {
      // This trick with shifting probably only works on little-endian
      // systems
      v_new->qword = v_old->qword >> (split * 8);
   }
}

static void clone_source(rt_model_t *m, rt_nexus_t *nexus,
                         rt_source_t *old, int offset)
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
               RT_LOCK(old->u.port.input->signal->lock);
               rt_nexus_t *n = clone_nexus(m, old->u.port.input, offset);
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
         w_new->next = NULL;

         split_value(nexus, &w_new->value, &w_old->value, offset);

         // Pending fast driver update
         if ((nexus->flags & NET_F_FAST_DRIVER) && old->fastqueued) {
            rt_nexus_t *n0 = &(nexus->signal->nexus);
            if (!n0->sources.sigqueued)
               workq_do(m->delta_driverq, async_fast_driver, new);
            new->fastqueued = 1;
         }

         // Future transactions
         for (w_old = w_old->next; w_old; w_old = w_old->next) {
            w_new = (w_new->next = alloc_waveform(m));
            w_new->when = w_old->when;
            w_new->next = NULL;

            split_value(nexus, &w_new->value, &w_old->value, offset);

            assert(w_old->when >= m->now);
            deltaq_insert_driver(m, w_new->when - m->now, nexus, new);
         }
      }
      break;

   case SOURCE_FORCING:
      split_value(nexus, &(new->u.forcing), &(old->u.forcing), offset);
      break;
   }
}

static rt_nexus_t *clone_nexus(rt_model_t *m, rt_nexus_t *old, int offset)
{
   assert(offset < old->width);

   rt_signal_t *signal = old->signal;
   MULTITHREADED_ONLY(assert_lock_held(&signal->lock));
   signal->n_nexus++;

   if (signal->n_nexus == 2 && (old->flags & NET_F_FAST_DRIVER))
      signal->shared.flags |= NET_F_FAST_DRIVER;

   rt_nexus_t *new = static_alloc(m, sizeof(rt_nexus_t));
   new->width        = old->width - offset;
   new->size         = old->size;
   new->signal       = signal;
   new->offset       = old->offset + offset * old->size;
   new->chain        = old->chain;
   new->flags        = old->flags;
   new->active_delta = old->active_delta;
   new->event_delta  = old->event_delta;
   new->last_event   = old->last_event;

   old->chain = new;
   old->width = offset;

   if (old->pending == NULL)
      new->pending = NULL;
   else if (pointer_tag(old->pending) == 1)
      new->pending = old->pending;
   else {
      rt_pending_t *old_p = untag_pointer(old->pending, rt_pending_t);
      rt_pending_t *new_p = xmalloc_flex(sizeof(rt_pending_t), old_p->count,
                                         sizeof(rt_wakeable_t *));

      new_p->count = new_p->max = old_p->count;

      for (int i = 0; i < old_p->count; i++)
         new_p->wake[i] = old_p->wake[i];

      new->pending = tag_pointer(new_p, 0);
   }

   if (new->chain == NULL)
      m->nexus_tail = &(new->chain);

   if (old->n_sources > 0) {
      for (rt_source_t *it = &(old->sources); it; it = it->chain_input)
         clone_source(m, new, it, offset);
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
         else {
            RT_LOCK(old_o->u.port.output->signal->lock);
            out_n = clone_nexus(m, old_o->u.port.output, offset);
         }

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
   MULTITHREADED_ONLY(assert_lock_held(&s->lock));

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
         clone_nexus(m, it, offset);
         offset = 0;
         continue;
      }
      else {
         if (it->width > count)
            clone_nexus(m, it, count);

         count -= it->width;

         if (result == NULL)
            result = it;
      }
   }

   return result;
}

static void setup_signal(rt_model_t *m, rt_signal_t *s, tree_t where,
                         unsigned count, unsigned size, sig_flags_t flags,
                         unsigned offset)
{
   rt_scope_t *parent = model_thread(m)->active_scope;

   s->where   = where;
   s->n_nexus = 1;
   s->offset  = offset;
   s->parent  = parent;

   s->shared.flags = flags;
   s->shared.size  = count * size;

   list_add(&parent->signals, s);

   s->nexus.width        = count;
   s->nexus.size         = size;
   s->nexus.n_sources    = 0;
   s->nexus.offset       = 0;
   s->nexus.flags        = flags | NET_F_FAST_DRIVER;
   s->nexus.signal       = s;
   s->nexus.pending      = NULL;
   s->nexus.active_delta = DELTA_CYCLE_MAX;
   s->nexus.event_delta  = DELTA_CYCLE_MAX;
   s->nexus.last_event   = TIME_HIGH;

   *m->nexus_tail = &(s->nexus);
   m->nexus_tail = &(s->nexus.chain);

   m->n_signals++;
}

static void copy_sub_signals(rt_scope_t *scope, void *buf, value_fn_t fn)
{
   assert(scope->kind == SCOPE_SIGNAL);

   list_foreach(rt_signal_t *, s, scope->signals) {
      rt_nexus_t *n = &(s->nexus);
      for (unsigned i = 0; i < s->n_nexus; i++, n = n->chain)
         memcpy(buf + s->offset + n->offset, (*fn)(n), n->size * n->width);
   }

   list_foreach(rt_scope_t *, s, scope->children)
      copy_sub_signals(s, buf, fn);
}

static void copy_sub_signal_sources(rt_scope_t *scope, void *buf, int stride)
{
   assert(scope->kind == SCOPE_SIGNAL);

   list_foreach(rt_signal_t *, s, scope->signals) {
      rt_nexus_t *n = &(s->nexus);
      for (unsigned i = 0; i < s->n_nexus; i++) {
         unsigned o = 0;
         for (rt_source_t *src = &(n->sources); src; src = src->chain_input) {
            const void *data = source_value(n, src);
            if (data == NULL)
               continue;

            memcpy(buf + s->offset + (o++ * stride), data, n->size * n->width);
         }
      }
   }

   list_foreach(rt_scope_t *, s, scope->children)
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
      for (unsigned i = 0; i < i0->n_nexus; i++, n = n->chain)
         memcpy(indata + i0->offset + n->offset, (*fn)(n), n->size * n->width);
   }

   rt_model_t *m = get_model();

   TRACE("call conversion function %s insz=%zu outsz=%zu",
         istr(jit_get_name(m->jit, cf->closure.handle)), insz, cf->bufsz);

   jit_scalar_t context = { .pointer = cf->closure.context };
   if (!jit_try_call_packed(m->jit, cf->closure.handle, context,
                            indata, insz, cf->buffer, cf->bufsz))
      m->force_stop = true;

   if (incopy) free(indata);

   return cf->buffer + port->output->signal->offset;
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

   case SOURCE_FORCING:
      assert(src->disconnected);
      return NULL;
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

      return result.pointer + nexus->signal->offset
         + nexus->offset - rscope->offset;
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

static rt_source_t *get_forcing_source(rt_model_t *m, rt_nexus_t *n)
{
   if (n->n_sources > 0) {
      for (rt_source_t *s = &(n->sources); s; s = s->chain_input) {
         if (s->tag == SOURCE_FORCING)
            return s;
      }
   }

   return add_source(m, n, SOURCE_FORCING);
}

static void *driving_value(rt_nexus_t *nexus)
{
   // Algorithm for driving values is in LRM 08 section 14.7.7.2

   // If S is driving-value forced, the driving value of S is unchanged
   // from its previous value; no further steps are required.
   if (unlikely(nexus->flags & NET_F_FORCED)) {
      rt_source_t *src = get_forcing_source(get_model(), nexus);
      return value_ptr(nexus, &(src->u.forcing));
   }

   // If S has no source, then the driving value of S is given by the
   // default value associated with S
   if (nexus->n_sources == 0)
      return nexus_driving(nexus);

   res_memo_t *r = nexus->signal->resolution;

   if (r == NULL) {
      rt_source_t *s = &(nexus->sources);
      switch (s->tag) {
      case SOURCE_DRIVER:
         // If S has one source that is a driver and S is not a resolved
         // signal, then the driving value of S is the current value of
         // that driver.
         assert(!s->disconnected);
         return value_ptr(nexus, &(s->u.driver.waveforms.value));

      case SOURCE_PORT:
         // If S has one source that is a port and S is not a resolved
         // signal, then the driving value of S is the driving value of
         // the formal part of the association element that associates S
         // with that port
         if (likely(s->u.port.conv_func == NULL))
            return driving_value(s->u.port.input);
         else
            return call_conversion(&(s->u.port), driving_value);

      case SOURCE_FORCING:
         // An undriven signal that was previously forced
         assert(s->disconnected);
         return nexus_driving(nexus);
      }

      return NULL;
   }
   else {
      // If S is a resolved signal and has one or more sources, then the
      // driving values of the sources of S are examined.

      int nonnull = 0, released = 0;
      for (rt_source_t *s = &(nexus->sources); s; s = s->chain_input) {
         if (!s->disconnected)
            nonnull++;
         else if (s->tag == SOURCE_FORCING)
            released++;
      }

      // If S is of signal kind register and all the sources of S have
      // values determined by the null transaction, then the driving
      // value of S is unchanged from its previous value.
      if (nonnull == 0 && (nexus->flags & NET_F_REGISTER))
         return nexus_effective(nexus);
      else if (nonnull == 0 && released == nexus->n_sources)
         return nexus_driving(nexus);

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
               return nexus_effective(nexus);
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
      return nexus_driving(nexus);
   else
      return nexus_effective(nexus);
}

static void propagate_nexus(rt_nexus_t *nexus, const void *resolved)
{
   const size_t valuesz = nexus->size * nexus->width;

   // LAST_VALUE is the same as the initial value when there have
   // been no events on the signal otherwise only update it when
   // there is an event
   void *eff = nexus_effective(nexus);
   memcpy(nexus_last_value(nexus), eff, valuesz);
   memcpy(eff, resolved, valuesz);
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

   fbuf_t *f = cover_open_lib_file(m->top, FBUF_IN, false);
   if (f == NULL)
      return;

   m->cover = cover_read_tags(f, 0);

   int32_t n_tags;
   cover_count_tags(m->cover, &n_tags);

   jit_alloc_cover_mem(m->jit, n_tags);

   fbuf_close(f, NULL);
}

static void emit_coverage(rt_model_t *m)
{
   if (m->cover != NULL) {
      const int32_t *counts = jit_get_cover_mem(m->jit);
      fbuf_t *covdb =  cover_open_lib_file(m->top, FBUF_OUT, true);
      cover_dump_tags(m->cover, covdb, COV_DUMP_RUNTIME, counts);
      fbuf_close(covdb, NULL);
   }
}

cover_tagging_t *get_coverage(rt_model_t *m)
{
   return m->cover;
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

      const void *driving = NULL;
      if (n->flags & NET_F_EFFECTIVE)
         driving = nexus_driving(n);

      fprintf(stderr, "%-20s %-5d %-4d %-7d %-7d ",
              nth == 0 ? tb_get(tb) : "+",
              n->width, n->size, n->n_sources, n_outputs);

      if (n->event_delta == m->iteration && n->last_event == m->now)
         fprintf(stderr, "%s -> ", fmt_nexus(n, nexus_last_value(n)));

      fputs(fmt_nexus(n, nexus_effective(n)), stderr);

      if (driving != NULL)
         fprintf(stderr, " (%s)", fmt_nexus(n, driving));

      fputs("\n", stderr);
   }
}

static void dump_signals(rt_model_t *m, rt_scope_t *scope)
{
   if (scope->signals == NULL && list_size(scope->children) == 0)
      return;

   if (scope->kind != SCOPE_SIGNAL && scope->kind != SCOPE_ROOT) {
      const char *sname = istr(scope->name);
      fprintf(stderr, "== %s ", sname);
      for (int pad = 74 - strlen(sname); pad > 0; pad--)
         fputc('=', stderr);
      fputc('\n', stderr);

      fprintf(stderr, "%-20s %5s %4s %7s %7s %s\n",
              "Signal", "Width", "Size", "Sources", "Outputs", "Value");
   }

   for (list_iter(rt_signal_t *, s, scope->signals))
      dump_one_signal(m, scope, s, NULL);

   for (list_iter(rt_alias_t *, a, scope->aliases))
      dump_one_signal(m, scope, a->signal, a->where);

   for (list_iter(rt_scope_t *, c, scope->children)) {
      if (c->kind == SCOPE_SIGNAL)
         dump_signals(m, c);
   }

   for (list_iter(rt_scope_t *, c, scope->children)) {
      if (c->kind != SCOPE_SIGNAL)
         dump_signals(m, c);
   }
}

static text_buf_t *signal_full_name(rt_signal_t *s)
{
   text_buf_t *tb = tb_new();
   if (s->parent->kind == SCOPE_SIGNAL)
      tb_printf(tb, "%s.", istr(s->parent->name));
   tb_cat(tb, istr(tree_ident(s->where)));
   return tb;
}

static void check_undriven_std_logic(rt_nexus_t *n)
{
   // Print a warning if any STD_LOGIC signal has multiple sources one
   // of which is an undriven port with initial value 'U'. The resolved
   // value will then always be 'U' which often confuses users.

   if (n->n_sources < 2 || !(n->signal->shared.flags & SIG_F_STD_LOGIC))
      return;

   rt_signal_t *undriven = NULL;
   for (rt_source_t *s = &(n->sources); s; s = s->chain_input) {
      if (s->tag == SOURCE_PORT) {
         rt_nexus_t *input = s->u.port.input;
         if (input->n_sources == 0) {
            const unsigned char *init = nexus_effective(input), *p = init;
            for (; *p == 0 && p < init + input->width; p++);

            if (p == init + input->width)
               undriven = s->u.port.input->signal;
         }
      }
   }

   if (undriven == NULL)
      return;

   LOCAL_TEXT_BUF sig_name = signal_full_name(n->signal);
   LOCAL_TEXT_BUF port_name = signal_full_name(undriven);

   const loc_t *sig_loc = tree_loc(n->signal->where);
   rt_scope_t *sig_scope = n->signal->parent;
   for (; sig_scope->kind == SCOPE_SIGNAL; sig_scope = sig_scope->parent)
      sig_loc = tree_loc(sig_scope->where);

   const loc_t *port_loc = tree_loc(undriven->where);
   rt_scope_t *port_scope = undriven->parent;
   for (; port_scope->kind == SCOPE_SIGNAL; port_scope = port_scope->parent)
      port_loc = tree_loc(port_scope->where);

   diag_t *d = diag_new(DIAG_WARN, sig_loc);
   diag_printf(d, "%ssignal %s has %d sources including port %s which has "
               "initial value 'U' and no driver in instance %s",
               n->signal->n_nexus > 1 ? "sub-element of " : "",
               tb_get(sig_name), n->n_sources,
               tb_get(port_name), istr(tree_ident(port_scope->where)));
   diag_hint(d, sig_loc, "signal %s declared here", tb_get(sig_name));
   diag_hint(d, port_loc, "sourced by port %s which always contributes 'U'",
             tb_get(port_name));
   diag_hint(d, NULL, "the resolved value will always be 'U' which was almost "
             "certainly not intended");
   diag_emit(d);

   // Prevent multiple warnings for the same signal
   n->signal->shared.flags &= ~SIG_F_STD_LOGIC;
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
               copy_value_ptr(n, &(s->u.driver.waveforms.value),
                              nexus_effective(n));
         }
      }

      heap_insert(q, nexus_rank(n), n);
   }

   SCOPED_A(rt_nexus_t *) effq = AINIT;

   while (heap_size(q) > 0) {
      rt_nexus_t *n = heap_extract_min(q);

      if (n->flags & NET_F_EFFECTIVE) {
         // Driving and effective values must be calculated separately
         void *driving = nexus_driving(n);
         memcpy(driving, driving_value(n), n->width * n->size);

         APUSH(effq, n);

         TRACE("%s initial driving value %s",
               istr(tree_ident(n->signal->where)), fmt_nexus(n, driving));
      }
      else {
         // Effective value is always the same as the driving value
         const void *initial = nexus_effective(n);
         if (n->n_sources > 0)
            initial = driving_value(n);

         const size_t valuesz = n->size * n->width;

         memcpy(nexus_last_value(n), initial, valuesz);
         memcpy(nexus_effective(n), initial, valuesz);

         TRACE("%s initial value %s", istr(tree_ident(n->signal->where)),
               fmt_nexus(n, initial));
      }

      check_undriven_std_logic(n);
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

static void update_property(rt_model_t *m, rt_prop_t *prop)
{
   TRACE("update property %s state %s", istr(prop->name),
         trace_states(&prop->state));

   model_thread_t *thread = model_thread(m);

   if (!tlab_valid(thread->tlab))
      tlab_acquire(m->mspace, &thread->tlab);

   tlab_t *tlab = &thread->tlab;

   thread->active_obj = &(prop->wakeable);
   thread->active_scope = prop->scope;

   jit_scalar_t context = {
      .pointer = *mptr_get(prop->scope->privdata)
   };

   mask_clearall(&prop->newstate);

   int bit = -1;
   while (mask_iter(&prop->state, &bit)) {
      jit_scalar_t state = { .integer = bit }, result;
      if (!jit_fastcall(m->jit, prop->handle, &result, context, state, tlab))
         m->force_stop = true;
   }

   thread->active_obj = NULL;
   thread->active_scope = NULL;

   TRACE("new state %s", trace_states(&prop->newstate));

   mask_copy(&prop->state, &prop->newstate);
}

static void sched_event(rt_model_t *m, rt_nexus_t *n, rt_wakeable_t *obj)
{
   if (n->pending == NULL)
      n->pending = tag_pointer(obj, 1);
   else if (pointer_tag(n->pending) == 1) {
      rt_pending_t *p = xmalloc_flex(sizeof(rt_pending_t), PENDING_MIN,
                                     sizeof(rt_wakeable_t *));
      p->max = PENDING_MIN;
      p->count = 2;
      p->wake[0] = untag_pointer(n->pending, rt_wakeable_t);
      p->wake[1] = obj;

      n->pending = tag_pointer(p, 0);
   }
   else {
      rt_pending_t *p = untag_pointer(n->pending, rt_pending_t);

      for (int i = 0; i < p->count; i++) {
         if (p->wake[i] == NULL || p->wake[i] == obj) {
            p->wake[i] = obj;
            return;
         }
      }

      if (p->count == p->max) {
         p->max = MAX(PENDING_MIN, p->max * 2);
         p = xrealloc_flex(p, sizeof(rt_pending_t), p->max,
                           sizeof(rt_wakeable_t *));
         n->pending = tag_pointer(p, 0);
      }

      p->wake[p->count++] = obj;
   }
}

static void clear_event(rt_model_t *m, rt_nexus_t *n, rt_wakeable_t *obj)
{
   if (pointer_tag(n->pending) == 1) {
      rt_wakeable_t *wake = untag_pointer(n->pending, rt_wakeable_t);
      if (wake == obj)
         n->pending = NULL;
   }
   else if (n->pending != NULL) {
      rt_pending_t *p = untag_pointer(n->pending, rt_pending_t);
      for (int i = 0; i < p->count; i++) {
         if (p->wake[i] == obj) {
            p->wake[i] = NULL;
            return;
         }
      }
   }
}

static rt_source_t *find_driver(rt_nexus_t *nexus, rt_proc_t *proc)
{
   // Try to find this process in the list of existing drivers
   for (rt_source_t *d = &(nexus->sources); d; d = d->chain_input) {
      if (d->tag == SOURCE_DRIVER && d->u.driver.proc == proc)
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
         free_waveform(m, it);
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
      free_waveform(m, it);
   }

   return already_scheduled;
}

static void sched_driver(rt_model_t *m, rt_nexus_t *nexus, uint64_t after,
                         uint64_t reject, const void *value, rt_proc_t *proc)
{
   if (after == 0 && (nexus->flags & NET_F_FAST_DRIVER)) {
      rt_source_t *d = &(nexus->sources);
      assert(nexus->n_sources == 1);

      waveform_t *w = &d->u.driver.waveforms;
      w->when = m->now;
      assert(w->next == NULL);

      rt_signal_t *signal = nexus->signal;
      rt_source_t *d0 = &(signal->nexus.sources);

      if (d->fastqueued)
         assert(m->next_is_delta);
      else if ((signal->shared.flags & NET_F_FAST_DRIVER) && d0->sigqueued) {
         assert(m->next_is_delta);
         d->fastqueued = 1;
      }
      else if (signal->shared.flags & NET_F_FAST_DRIVER) {
         workq_do(m->delta_driverq, async_fast_all_drivers, signal);
         m->next_is_delta = true;
         d0->sigqueued = 1;
         d->fastqueued = 1;
      }
      else {
         workq_do(m->delta_driverq, async_fast_driver, d);
         m->next_is_delta = true;
         d->fastqueued = 1;
      }

      copy_value_ptr(nexus, &w->value, value);
   }
   else {
      rt_source_t *d = find_driver(nexus, proc);
      assert(d != NULL);

      if ((nexus->flags & NET_F_FAST_DRIVER) && d->fastqueued) {
         // A fast update to this driver is already scheduled
         waveform_t *w0 = alloc_waveform(m);
         w0->when  = m->now;
         w0->next  = NULL;
         w0->value = alloc_value(m, nexus);

         const uint8_t *prev = value_ptr(nexus, &(d->u.driver.waveforms.value));
         copy_value_ptr(nexus, &w0->value, prev);

         assert(d->u.driver.waveforms.next == NULL);
         d->u.driver.waveforms.next = w0;
      }

      nexus->flags &= ~NET_F_FAST_DRIVER;

      waveform_t *w = alloc_waveform(m);
      w->when  = m->now + after;
      w->next  = NULL;
      w->value = alloc_value(m, nexus);

      copy_value_ptr(nexus, &w->value, value);

      if (!insert_transaction(m, nexus, d, w, w->when, reject))
         deltaq_insert_driver(m, after, nexus, d);
   }
}

static void sched_disconnect(rt_model_t *m, rt_nexus_t *nexus, uint64_t after,
                             uint64_t reject, rt_proc_t *proc)
{
   rt_source_t *d = find_driver(nexus, proc);
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
   bool free_later = w->wakeable.free_later;

   MODEL_ENTRY(m);
   (*w->fn)(m->now, w->signal, w, w->user_data);

   if (free_later)
      free(w);
}

static void async_timeout_callback(void *context, void *arg)
{
   rt_model_t *m = context;
   rt_callback_t *cb = arg;

   MODEL_ENTRY(m);
   (*cb->fn)(m, cb->user);
   free(cb);
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

static void async_update_property(void *context, void *arg)
{
   rt_model_t *m = context;
   rt_prop_t *prop = arg;

   assert(prop->wakeable.pending);
   prop->wakeable.pending = false;

   MODEL_ENTRY(m);
   update_property(m, prop);
}

static bool heap_delete_proc_cb(uint64_t key, void *value, void *search)
{
   if (pointer_tag(value) != EVENT_PROCESS)
      return false;

   return untag_pointer(value, rt_proc_t) == search;
}

static void wakeup_one(rt_model_t *m, rt_wakeable_t *obj)
{
   if (obj->pending)
      return;   // Already scheduled

   workq_t *wq = obj->postponed ? m->postponedq : m->procq;

   switch (obj->kind) {
   case W_PROC:
      {
         rt_proc_t *proc = container_of(obj, rt_proc_t, wakeable);
         TRACE("wakeup %sprocess %s", obj->postponed ? "postponed " : "",
               istr(proc->name));
         workq_do(wq, async_run_process, proc);

         if (proc->wakeable.delayed) {
            // This process was already scheduled to run at a later
            // time so we need to delete it from the simulation queue
            heap_delete(m->eventq_heap, heap_delete_proc_cb, proc);
            proc->wakeable.delayed = false;
         }
      }
      break;

   case W_PROPERTY:
      {
         rt_prop_t *prop = container_of(obj, rt_prop_t, wakeable);
         TRACE("wakeup property %s", istr(prop->name));
         workq_do(wq, async_update_property, prop);
      }
      break;

   case W_IMPLICIT:
      {
         rt_implicit_t *imp = container_of(obj, rt_implicit_t, wakeable);
         TRACE("wakeup implicit signal %s closure %s",
               istr(tree_ident(imp->signal.where)),
               istr(jit_get_name(m->jit, imp->closure.handle)));
         workq_do(m->implicitq, async_update_implicit_signal, imp);
      }
      break;

   case W_WATCH:
      {
         rt_watch_t *w = container_of(obj, rt_watch_t, wakeable);
         TRACE("wakeup %svalue change callback %s",
               obj->postponed ? "postponed " : "", debug_symbol_name(w->fn));
         workq_do(wq, async_watch_callback, w);
      }
      break;
   }

   set_pending(obj);
}

static void notify_event(rt_model_t *m, rt_nexus_t *nexus)
{
   nexus->last_event = m->now;
   nexus->event_delta = m->iteration;

   if (pointer_tag(nexus->pending) == 1) {
      rt_wakeable_t *wake = untag_pointer(nexus->pending, rt_wakeable_t);
      wakeup_one(m, wake);
   }
   else if (nexus->pending != NULL) {
      rt_pending_t *p = untag_pointer(nexus->pending, rt_pending_t);
      for (int i = 0; i < p->count; i++) {
         if (p->wake[i] != NULL)
            wakeup_one(m, p->wake[i]);
      }
   }
}

static void update_effective(rt_model_t *m, rt_nexus_t *nexus)
{
   const void *value = effective_value(nexus);

   TRACE("update %s effective value %s", istr(tree_ident(nexus->signal->where)),
         fmt_nexus(nexus, value));

   nexus->active_delta = m->iteration;

   if (memcmp(nexus_effective(nexus), value, nexus->size * nexus->width) != 0) {
      propagate_nexus(nexus, value);
      notify_event(m, nexus);
   }
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

static void update_driving(rt_model_t *m, rt_nexus_t *nexus, const void *value)
{
   const size_t valuesz = nexus->size * nexus->width;

   TRACE("update %s driving value %s", istr(tree_ident(nexus->signal->where)),
         fmt_nexus(nexus, value));

   nexus->active_delta = m->iteration;

   bool update_outputs = false;
   if (nexus->flags & NET_F_EFFECTIVE) {
      // The active and event flags will be set when we update the
      // effective value later
      update_outputs = true;

      memcpy(nexus_driving(nexus), value, valuesz);

      enqueue_effective(m, nexus);
   }
   else if (memcmp(nexus_effective(nexus), value, valuesz) != 0) {
      propagate_nexus(nexus, value);
      notify_event(m, nexus);
      update_outputs = true;
   }

   if (update_outputs) {
      for (rt_source_t *o = nexus->outputs; o; o = o->chain_output) {
         assert(o->tag == SOURCE_PORT);
         update_driving(m, o->u.port.output, driving_value(o->u.port.output));
      }
   }
}

static void update_driver(rt_model_t *m, rt_nexus_t *nexus, rt_source_t *source)
{
   model_thread_t *thread = model_thread(m);

   // Updating drivers may involve calling resolution functions
   if (!tlab_valid(thread->tlab))
      tlab_acquire(m->mspace, &thread->tlab);

   if (likely(source != NULL)) {
      waveform_t *w_now  = &(source->u.driver.waveforms);
      waveform_t *w_next = w_now->next;

      if (likely((w_next != NULL) && (w_next->when == m->now))) {
         free_value(nexus, w_now->value);
         *w_now = *w_next;
         free_waveform(m, w_next);
         source->disconnected = 0;
         update_driving(m, nexus, driving_value(nexus));
      }
      else
         assert(w_now != NULL);
   }
   else  // Update due to force/release
      update_driving(m, nexus, driving_value(nexus));

   tlab_reset(thread->tlab);   // No allocations can be live past here
}

static void fast_update_driver(rt_model_t *m, rt_nexus_t *nexus)
{
   rt_source_t *src = &(nexus->sources);

   if (likely(nexus->flags & NET_F_FAST_DRIVER)) {
      model_thread_t *thread = model_thread(m);

      // Updating drivers may involve calling resolution functions
      if (!tlab_valid(thread->tlab))
         tlab_acquire(m->mspace, &thread->tlab);

      // Preconditions for fast driver updates
      assert(nexus->n_sources == 1);
      assert(src->tag == SOURCE_DRIVER);
      assert(src->u.driver.waveforms.next == NULL);

      update_driving(m, nexus, driving_value(nexus));

      tlab_reset(thread->tlab);   // No allocations can be live past here
   }
   else
      update_driver(m, nexus, src);

   assert(src->fastqueued);
   src->fastqueued = 0;
}

static void fast_update_all_drivers(rt_model_t *m, rt_signal_t *signal)
{
   assert(signal->shared.flags & NET_F_FAST_DRIVER);

   rt_nexus_t *n = &(signal->nexus);
   assert(n->sources.sigqueued);
   n->sources.sigqueued = 0;

   int count = 0;
   for (int i = 0; i < signal->n_nexus; i++, n = n->chain) {
      if (n->sources.fastqueued) {
         fast_update_driver(m, n);
         count++;
      }
   }

   if (count < signal->n_nexus >> 1) {
      // Unlikely to be worth the iteration cost
      signal->shared.flags &= ~NET_F_FAST_DRIVER;
   }
}

static void async_update_driver(void *context, void *arg)
{
   rt_model_t *m = context;
   rt_source_t *src = arg;

   MODEL_ENTRY(m);
   update_driver(m, src->u.driver.nexus, src);
}

static void async_fast_driver(void *context, void *arg)
{
   rt_model_t *m = context;
   rt_source_t *src = arg;

   MODEL_ENTRY(m);
   fast_update_driver(m, src->u.driver.nexus);
}

static void async_fast_all_drivers(void *context, void *arg)
{
   rt_model_t *m = context;
   rt_signal_t *signal = arg;

   MODEL_ENTRY(m);
   fast_update_all_drivers(m, signal);
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

static void async_deposit(void *context, void *arg)
{
   rt_model_t *m = context;
   rt_deposit_t *deposit = arg;

   MODEL_ENTRY(m);

   // Force takes priority over deposit
   if (!(deposit->nexus->flags & NET_F_FORCED))
      update_driving(m, deposit->nexus, deposit->data);

   free(deposit);
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

   n0->active_delta = m->iteration;

   if (*(int8_t *)nexus_effective(n0) != result.integer) {
      propagate_nexus(n0, &result.integer);
      notify_event(m, n0);
   }
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

   jit_reset_exit_status(m->jit, EXIT_FAILURE);
   m->force_stop = true;
}

static void sync_event_cache(rt_model_t *m)
{
   list_foreach(rt_signal_t *, s, m->eventsigs) {
      assert(s->shared.flags & SIG_F_CACHE_EVENT);

      const bool event = s->nexus.last_event == m->now
         && s->nexus.event_delta == m->iteration;

      TRACE("sync event flag %d for %s", event, istr(tree_ident(s->where)));

      if (event)
         s->shared.flags |= SIG_F_EVENT_FLAG;
      else
         s->shared.flags &= ~SIG_F_EVENT_FLAG;
   }
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
      m->now = heap_min_key(m->eventq_heap);
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
         void *e = heap_extract_min(m->eventq_heap);
         switch (pointer_tag(e)) {
         case EVENT_PROCESS:
            {
               rt_proc_t *proc = untag_pointer(e, rt_proc_t);
               assert(proc->wakeable.delayed);
               proc->wakeable.delayed = false;
               set_pending(&proc->wakeable);
               workq_do(m->procq, async_run_process, proc);
            }
            break;
         case EVENT_DRIVER:
            {
               rt_source_t *source = untag_pointer(e, rt_source_t);
               workq_do(m->driverq, async_update_driver, source);
            }
            break;
         case EVENT_TIMEOUT:
            {
               rt_callback_t *cb = untag_pointer(e, rt_callback_t);
               workq_do(m->driverq, async_timeout_callback, cb);
            }
            break;
         case EVENT_DISCONNECT:
            {
               rt_source_t *source = untag_pointer(e, rt_source_t);
               workq_do(m->driverq, async_disconnect, source);
            }
            break;
         }

         if (heap_size(m->eventq_heap) == 0)
            break;
         else if (heap_min_key(m->eventq_heap) > m->now)
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

   sync_event_cache(m);

#if TRACE_SIGNALS > 0
   if (__trace_on)
      dump_signals(m, m->root);
#endif

   // Run all non-postponed processes and event callbacks
   workq_start(m->procq);
   workq_drain(m->procq);

   global_event(m, RT_END_OF_PROCESSES);

   if (!m->next_is_delta)
      global_event(m, RT_LAST_KNOWN_DELTA_CYCLE);

   if (!m->next_is_delta) {
      m->can_create_delta = false;

      // Run all postponed processes and event callbacks
      workq_start(m->postponedq);
      workq_drain(m->postponedq);

      global_event(m, RT_END_TIME_STEP);

      m->can_create_delta = true;
   }
   else if (m->stop_delta > 0 && m->iteration == m->stop_delta)
      reached_iteration_limit(m);
}

static bool should_stop_now(rt_model_t *m, uint64_t stop_time)
{
   if (m->force_stop) {
      // Make sure we print the interrupted message if this was the
      // result of an interrupt
      jit_check_interrupt(m->jit);
      return true;
   }
   else if (m->next_is_delta)
      return false;
   else if (heap_size(m->eventq_heap) == 0)
      return true;
   else
      return heap_min_key(m->eventq_heap) > stop_time;
}

void model_run(rt_model_t *m, uint64_t stop_time)
{
   MODEL_ENTRY(m);

   if (m->force_stop)
      return;   // Was error during intialisation

   global_event(m, RT_START_OF_SIMULATION);

   while (!should_stop_now(m, stop_time))
      model_cycle(m);

   global_event(m, RT_END_OF_SIMULATION);

   emit_coverage(m);
}

bool model_step(rt_model_t *m)
{
   MODEL_ENTRY(m);

   if (!m->force_stop)
      model_cycle(m);

   return should_stop_now(m, TIME_HIGH);
}

static inline void check_postponed(int64_t after, rt_proc_t *proc)
{
   if (unlikely(proc->wakeable.postponed && (after == 0)))
      fatal("postponed process %s cannot cause a delta cycle",
            istr(proc->name));
}

static inline void check_reject_limit(rt_signal_t *s, uint64_t after,
                                      uint64_t reject)
{
   if (unlikely(reject > after))
      jit_msg(NULL, DIAG_FATAL, "signal %s pulse reject limit %s is greater "
              "than delay %s", istr(tree_ident(s->where)),
              trace_time(reject), trace_time(after));
}

static inline void check_delay(int64_t delay)
{
   if (unlikely(delay < 0)) {
      char buf[32];
      fmt_time_r(buf, sizeof(buf), delay, " ");
      jit_msg(NULL, DIAG_FATAL, "illegal negative delay %s", buf);
   }
}

void force_signal(rt_signal_t *s, const void *values, int offset, size_t count)
{
   RT_LOCK(s->lock);

   TRACE("force signal %s (offset %d) to %s", istr(tree_ident(s->where)), offset,
         fmt_values(values, count));

   rt_model_t *m = get_model();
   assert(m->can_create_delta);

   rt_nexus_t *n = split_nexus(m, s, offset, count);
   const char *vptr = values;
   for (; count > 0; n = n->chain) {
      count -= n->width;
      assert(count >= 0);

      n->flags |= NET_F_FORCED;

      rt_source_t *src = get_forcing_source(m, n);
      copy_value_ptr(n, &(src->u.forcing), vptr);

      deltaq_insert_force_release(m, n);
      vptr += n->width * n->size;
   }
}

void deposit_signal(rt_signal_t *s, const void *values, int offset, size_t count)
{
   RT_LOCK(s->lock);

   TRACE("deposit signal %s (offset %d) to %s", istr(tree_ident(s->where)),
         offset, fmt_values(values, count));

   rt_model_t *m = get_model();
   assert(m->can_create_delta);

   rt_nexus_t *n = split_nexus(m, s, offset, count);
   const char *vptr = values;
   for (; count > 0; n = n->chain) {
      count -= n->width;
      assert(count >= 0);

      rt_deposit_t *d = xmalloc_flex(sizeof(rt_deposit_t), n->width, n->size);
      d->nexus = n;
      memcpy(d->data, vptr, n->width * n->size);

      deltaq_insert_deposit(m, d);
      vptr += n->width * n->size;
   }
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
   relaxed_store(&m->force_stop, true);
}

void model_set_global_cb(rt_model_t *m, rt_event_t event, rt_event_fn_t fn,
                         void *user)
{
   assert(event < RT_LAST_EVENT);

   rt_callback_t *cb = xcalloc(sizeof(rt_callback_t));
   cb->next = m->global_cbs[event];
   cb->fn   = fn;
   cb->user = user;

   m->global_cbs[event] = cb;
}

void model_set_timeout_cb(rt_model_t *m, uint64_t when, rt_event_fn_t fn,
                          void *user)
{
   rt_callback_t *cb = xcalloc(sizeof(rt_callback_t));
   cb->next = NULL;
   cb->fn   = fn;
   cb->user = user;

   assert(when > m->now);   // TODO: delta timeouts?

   void *e = tag_pointer(cb, EVENT_TIMEOUT);
   heap_insert(m->eventq_heap, when, e);
}

rt_watch_t *model_set_event_cb(rt_model_t *m, rt_signal_t *s, sig_event_fn_t fn,
                               void *user, bool postponed)
{
   rt_watch_t *w = xcalloc(sizeof(rt_watch_t));
   w->signal    = s;
   w->fn        = fn;
   w->chain_all = m->watches;
   w->user_data = user;

   w->wakeable.kind      = W_WATCH;
   w->wakeable.postponed = postponed;
   w->wakeable.pending   = false;
   w->wakeable.delayed   = false;

   m->watches = w;

   rt_nexus_t *n = &(w->signal->nexus);
   for (int i = 0; i < s->n_nexus; i++, n = n->chain)
      sched_event(m, n, &(w->wakeable));

   return w;
}

bool model_clear_global_cb(rt_model_t *m, rt_event_t event, rt_event_fn_t fn,
                           void *user)
{
   assert(event < RT_LAST_EVENT);

   rt_callback_t **last = &m->global_cbs[event];
   if (!*last)
      return false;

   for (rt_callback_t *it = *last; it; last = &(it->next), it = it->next) {
      if (it->fn == fn && it->user == user) {
         *last = it->next;
         free(it);
         return true;
      }
   }

   return false;
}

typedef struct {
   uint64_t       when;
   rt_event_fn_t  fn;
   void          *user;
} timeout_params_t;

static bool eventq_delete_fn(uint64_t key, void *e, void *context)
{
   timeout_params_t *params = context;

   if (key != params->when)
      return false;

   if (pointer_tag(e) != EVENT_TIMEOUT)
      return false;

   rt_callback_t *cb = untag_pointer(e, rt_callback_t);
   if (cb->fn == params->fn && cb->user == params->user) {
      free(cb);
      return true;
   }

   return false;
}

bool model_clear_timeout_cb(rt_model_t *m, uint64_t when, rt_event_fn_t fn,
                            void *user)
{
   timeout_params_t params = {
      .when = when,
      .fn = fn,
      .user = user,
   };

   return heap_delete(m->eventq_heap, eventq_delete_fn, &params);
}

bool model_clear_event_cb(rt_model_t *m, rt_watch_t *w)
{
   rt_nexus_t *n = &(w->signal->nexus);
   for (int i = 0; i < w->signal->n_nexus; i++, n = n->chain)
      clear_event(m, n, &(w->wakeable));

   rt_watch_t **last = &m->watches;
   for (rt_watch_t *it = *last; it; last = &(it->chain_all), it = it->chain_all) {
      if (it == w) {
         *last = it->chain_all;
         break;
      }
   }

   if (w->wakeable.pending) {
      w->wakeable.free_later = true;
      return false;
   }

   free(w);
   return true;
}

static void handle_interrupt_cb(jit_t *j, void *ctx)
{
   rt_proc_t *proc = get_active_proc();

   if (proc != NULL)
      jit_msg(NULL, DIAG_FATAL, "interrupted in process %s", istr(proc->name));
   else {
      diag_t *d = diag_new(DIAG_FATAL, NULL);
      diag_printf(d, "interrupted");
      diag_emit(d);
   }
}

void model_interrupt(rt_model_t *m)
{
   model_stop(m);
   jit_interrupt(m->jit, handle_interrupt_cb, m);
}

// TODO: this interface should be removed eventually
void *rt_tlab_alloc(size_t size)
{
   rt_model_t *m = get_model();
   model_thread_t *thread = model_thread(m);

   if (tlab_valid(thread->tlab))
      return tlab_alloc(&thread->tlab, size);
   else
      return mspace_alloc(get_model()->mspace, size);
}

static bool nexus_active(rt_model_t *m, rt_nexus_t *nexus)
{
   if (nexus->n_sources > 0) {
      for (rt_source_t *s = &(nexus->sources); s; s = s->chain_input) {
         if (s->tag == SOURCE_PORT) {
            RT_LOCK(s->u.port.input->signal->lock);
            if (nexus_active(m, s->u.port.input))
               return true;
         }
         else if (s->tag == SOURCE_DRIVER && nexus->active_delta == m->iteration
                  && s->u.driver.waveforms.when == m->now)
            return true;
      }
   }

   return false;
}

static uint64_t nexus_last_active(rt_model_t *m, rt_nexus_t *nexus)
{
   int64_t last = TIME_HIGH;

   if (nexus->n_sources > 0) {
      for (rt_source_t *s = &(nexus->sources); s; s = s->chain_input) {
         if (s->tag == SOURCE_PORT) {
            RT_LOCK(s->u.port.input->signal->lock);
            last = MIN(last, nexus_last_active(m, s->u.port.input));
         }
         else if (s->tag == SOURCE_DRIVER
                  && s->u.driver.waveforms.when <= m->now)
            last = MIN(last, m->now - s->u.driver.waveforms.when);
      }
   }

   return last;
}

int64_t get_static_expr(rt_model_t *m, tree_t expr)
{
   switch (tree_kind(expr)) {
   case T_LITERAL:
      switch (tree_subkind(expr)) {
      case L_INT:
      case L_PHYSICAL:
         return tree_ival(expr);
      }
      break;

   case T_REF:
      {
         tree_t decl = tree_ref(expr);
         if (tree_kind(decl) == T_CONST_DECL && tree_has_value(decl))
            return get_static_expr(m, tree_value(decl));
      }
      break;

   case T_FCALL:
      // There is a corner case where globally static expressions
      // containing deferred constants will not be folded if the
      // expression appears in a package as the value of the constant is
      // not known at analysis time and it is not revisited during
      // elaboration
      assert(tree_flags(expr) & TREE_F_GLOBALLY_STATIC);
      break;

   default:
      break;
   }

   return eval_static_expr(m->jit, expr);
}

////////////////////////////////////////////////////////////////////////////////
// Entry points from compiled code

sig_shared_t *x_init_signal(uint32_t count, uint32_t size,
                            const uint8_t *values, sig_flags_t flags,
                            tree_t where, int32_t offset)
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

sig_shared_t *x_init_signal_s(uint32_t count, uint32_t size, uint64_t value,
                              sig_flags_t flags, tree_t where, int32_t offset)
{
   TRACE("init signal %s count=%d size=%d value=%"PRIx64" flags=%x offset=%d",
         istr(tree_ident(where)), count, size, value, flags, offset);

   rt_model_t *m = get_model();

   const size_t datasz = MAX(3 * count * size, 8);
   rt_signal_t *s = static_alloc(m, sizeof(rt_signal_t) + datasz);
   setup_signal(m, s, where, count, size, flags, offset);

   // The driving value area is also used to save the default value
   void *driving = s->shared.data + 2*s->shared.size;

#define COPY_SCALAR(type) do {                  \
      type *pi = (type *)s->shared.data;        \
      type *pd = (type *)driving;               \
      for (int i = 0; i < count; i++)           \
         pi[i] = pd[i] = value;                 \
   } while (0)

   FOR_ALL_SIZES(size, COPY_SCALAR);

   return &(s->shared);
}

void x_drive_signal(sig_shared_t *ss, uint32_t offset, int32_t count)
{
   rt_signal_t *s = container_of(ss, rt_signal_t, shared);
   RT_LOCK(s->lock);

   TRACE("drive signal %s+%d count=%d", istr(tree_ident(s->where)),
         offset, count);

   rt_model_t *m = get_model();
   rt_proc_t *proc = get_active_proc();
   rt_nexus_t *n = split_nexus(m, s, offset, count);
   for (; count > 0; n = n->chain) {
      rt_source_t *s;
      for (s = &(n->sources); s; s = s->chain_input) {
         if (s->tag == SOURCE_DRIVER && s->u.driver.proc == proc)
            break;
      }

      if (s == NULL) {
         s = add_source(m, n, SOURCE_DRIVER);
         s->u.driver.waveforms.value = alloc_value(m, n);
         s->u.driver.proc = proc;
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
   rt_proc_t *proc = get_active_proc();

   TRACE("schedule process %s delay=%s", istr(proc->name), trace_time(delay));

   check_delay(delay);
   deltaq_insert_proc(get_model(), delay, proc);
}

void x_sched_waveform_s(sig_shared_t *ss, uint32_t offset, uint64_t scalar,
                        int64_t after, int64_t reject)
{
   rt_signal_t *s = container_of(ss, rt_signal_t, shared);
   RT_LOCK(s->lock);

   TRACE("_sched_waveform_s %s+%d value=%"PRIi64" after=%s reject=%s",
         istr(tree_ident(s->where)), offset, scalar, trace_time(after),
         trace_time(reject));

   rt_proc_t *proc = get_active_proc();

   check_delay(after);
   check_postponed(after, proc);
   check_reject_limit(s, after, reject);

   rt_model_t *m = get_model();
   rt_nexus_t *n = split_nexus(m, s, offset, 1);

   sched_driver(m, n, after, reject, &scalar, proc);
}

void x_sched_waveform(sig_shared_t *ss, uint32_t offset, void *values,
                      int32_t count, int64_t after, int64_t reject)
{
   rt_signal_t *s = container_of(ss, rt_signal_t, shared);
   RT_LOCK(s->lock);

   TRACE("_sched_waveform %s+%d value=%s count=%d after=%s reject=%s",
         istr(tree_ident(s->where)), offset, fmt_values(values, count),
         count, trace_time(after), trace_time(reject));

   rt_proc_t *proc = get_active_proc();

   check_delay(after);
   check_postponed(after, proc);
   check_reject_limit(s, after, reject);

   rt_model_t *m = get_model();
   rt_nexus_t *n = split_nexus(m, s, offset, count);
   char *vptr = values;
   for (; count > 0; n = n->chain) {
      count -= n->width;
      assert(count >= 0);

      sched_driver(m, n, after, reject, vptr, proc);
      vptr += n->width * n->size;
   }
}

int32_t x_test_net_event(sig_shared_t *ss, uint32_t offset, int32_t count)
{
   rt_signal_t *s = container_of(ss, rt_signal_t, shared);
   RT_LOCK(s->lock);

   TRACE("_test_net_event %s offset=%d count=%d",
         istr(tree_ident(s->where)), offset, count);

   int32_t result = 0;
   rt_model_t *m = get_model();
   rt_nexus_t *n = split_nexus(m, s, offset, count);
   for (; count > 0; n = n->chain) {
      if (n->last_event == m->now && n->event_delta == m->iteration) {
         result = 1;
         break;
      }

      count -= n->width;
      assert(count >= 0);
   }

   if (ss->size == 1) {
      assert(!(ss->flags & SIG_F_CACHE_EVENT));   // Should have taken fast-path
      ss->flags |= SIG_F_CACHE_EVENT | (result ? SIG_F_EVENT_FLAG : 0);
      list_add(&m->eventsigs, s);
   }

   return result;
}

int32_t x_test_net_active(sig_shared_t *ss, uint32_t offset, int32_t count)
{
   rt_signal_t *s = container_of(ss, rt_signal_t, shared);
   RT_LOCK(s->lock);

   TRACE("_test_net_active %s offset=%d count=%d",
         istr(tree_ident(s->where)), offset, count);

   rt_model_t *m = get_model();
   rt_nexus_t *n = split_nexus(m, s, offset, count);
   for (; count > 0; n = n->chain) {
      if (nexus_active(m, n))
         return 1;

      count -= n->width;
      assert(count >= 0);
   }

   return 0;
}

void x_sched_event(sig_shared_t *ss, uint32_t offset, int32_t count)
{
   rt_signal_t *s = container_of(ss, rt_signal_t, shared);
   RT_LOCK(s->lock);

   TRACE("_sched_event %s+%d count=%d", istr(tree_ident(s->where)),
         offset, count);

   rt_wakeable_t *obj = get_active_wakeable();

   rt_model_t *m = get_model();
   rt_nexus_t *n = split_nexus(m, s, offset, count);
   for (; count > 0; n = n->chain) {
      sched_event(m, n, obj);

      count -= n->width;
      assert(count >= 0);
   }
}

void x_implicit_event(sig_shared_t *ss, uint32_t offset, int32_t count,
                      sig_shared_t *wake_ss)
{
   rt_signal_t *s = container_of(ss, rt_signal_t, shared);
   RT_LOCK(s->lock);

   rt_implicit_t *wake_s = container_of(wake_ss, rt_implicit_t, signal.shared);
   RT_LOCK(wake_s->signal.lock);

   TRACE("implicit event %s+%d count=%d wake %s",
         istr(tree_ident(s->where)), offset, count,
         istr(tree_ident(wake_s->signal.where)));

   rt_model_t *m = get_model();
   rt_nexus_t *n = split_nexus(m, s, offset, count);
   for (; count > 0; n = n->chain) {
      sched_event(m, n, &(wake_s->wakeable));

      count -= n->width;
      assert(count >= 0);
   }
}

void x_clear_event(sig_shared_t *ss, uint32_t offset, int32_t count)
{
   rt_signal_t *s = container_of(ss, rt_signal_t, shared);
   RT_LOCK(s->lock);

   TRACE("clear event %s+%d count=%d",
         istr(tree_ident(s->where)), offset, count);

   rt_model_t *m = get_model();
   rt_proc_t *proc = get_active_proc();
   rt_nexus_t *n = split_nexus(m, s, offset, count);
   for (; count > 0; n = n->chain) {
      clear_event(m, n, &(proc->wakeable));

      count -= n->width;
      assert(count >= 0);
   }
}

void x_enter_state(int32_t state)
{
   rt_wakeable_t *obj = get_active_wakeable();
   assert(obj->kind == W_PROPERTY);

   rt_prop_t *prop = container_of(obj, rt_prop_t, wakeable);
   mask_set(&prop->newstate, state);
}

void x_alias_signal(sig_shared_t *ss, tree_t where)
{
   rt_signal_t *s = container_of(ss, rt_signal_t, shared);
   RT_LOCK(s->lock);

   TRACE("alias signal %s to %s", istr(tree_ident(s->where)),
         istr(tree_ident(where)));

   rt_alias_t *a = xcalloc(sizeof(rt_alias_t));
   a->where  = where;
   a->signal = s;

   model_thread_t *thread = model_thread(get_model());
   list_add(&thread->active_scope->aliases, a);
}

void x_claim_tlab(tlab_t *tlab)
{
   TRACE("claiming TLAB for private use (used %u/%u)",
         tlab->alloc, tlab->limit);

   if (tlab_valid(*tlab)) {
      assert(tlab->alloc <= tlab->limit);
      tlab_move(*tlab, get_active_proc()->tlab);
   }
}

int64_t x_last_event(sig_shared_t *ss, uint32_t offset, int32_t count)
{
   rt_signal_t *s = container_of(ss, rt_signal_t, shared);
   RT_LOCK(s->lock);

   TRACE("_last_event %s offset=%d count=%d",
         istr(tree_ident(s->where)), offset, count);

   int64_t last = TIME_HIGH;

   rt_model_t *m = get_model();
   rt_nexus_t *n = split_nexus(m, s, offset, count);
   for (; count > 0; n = n->chain) {
      if (n->last_event <= m->now)
         last = MIN(last, m->now - n->last_event);

      count -= n->width;
      assert(count >= 0);
   }

   return last;
}

int64_t x_last_active(sig_shared_t *ss, uint32_t offset, int32_t count)
{
   rt_signal_t *s = container_of(ss, rt_signal_t, shared);
   RT_LOCK(s->lock);

   TRACE("_last_active %s offset=%d count=%d",
         istr(tree_ident(s->where)), offset, count);

   int64_t last = TIME_HIGH;

   rt_model_t *m = get_model();
   rt_nexus_t *n = split_nexus(m, s, offset, count);
   for (; count > 0; n = n->chain) {
      last = MIN(last, nexus_last_active(m, n));

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
   RT_LOCK(src_s->lock);

   rt_signal_t *dst_s = container_of(dst_ss, rt_signal_t, shared);
   RT_LOCK(dst_s->lock);

   TRACE("map signal %s+%d to %s+%d count %d/%d%s",
         istr(tree_ident(src_s->where)), src_offset,
         istr(tree_ident(dst_s->where)), dst_offset,
         src_count, dst_count, closure ? " converted" : "");

   assert(src_count == dst_count || closure != NULL);
   assert(src_s != dst_s);

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
         clone_nexus(m, src_n, dst_n->width);
      else if (src_n->width < dst_n->width && closure == NULL)
         clone_nexus(m, dst_n, src_n->width);

      assert(src_n->width == dst_n->width || closure != NULL);
      assert(src_n->size == dst_n->size || closure != NULL);

      // Effective value updates must propagate through ports
      src_n->flags |= (dst_n->flags & NET_F_EFFECTIVE);
      dst_n->flags |= (src_n->flags & NET_F_EFFECTIVE);

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
   RT_LOCK(s->lock);

   TRACE("map const %s to %s+%d count %d", fmt_values(values, count),
         istr(tree_ident(s->where)), offset, count);

   rt_model_t *m = get_model();
   rt_nexus_t *n = split_nexus(m, s, offset, count);
   for (; count > 0; n = n->chain) {
      memcpy(nexus_driving(n), values, n->width * n->size);
      memcpy(nexus_effective(n), values, n->width * n->size);
      values += n->width * n->size;

      count -= n->width;
      assert(count >= 0);
   }
}

void x_push_scope(tree_t where, int32_t size)
{
   TRACE("push scope %s size=%d", istr(tree_ident(where)), size);

   rt_model_t *m = get_model();
   model_thread_t *thread = model_thread(m);

   ident_t name = tree_ident(where);
   if (thread->active_scope->kind == SCOPE_SIGNAL)
      name = ident_prefix(thread->active_scope->name, name, '.');

   rt_scope_t *s = xcalloc(sizeof(rt_scope_t));
   s->where    = where;
   s->name     = name;
   s->kind     = SCOPE_SIGNAL;
   s->parent   = thread->active_scope;
   s->size     = size;
   s->privdata = mptr_new(m->mspace, "push scope privdata");

   type_t type = tree_type(where);
   if (type_kind(type) == T_SUBTYPE && type_has_resolution(type))
      s->flags |= SCOPE_F_RESOLVED;

   list_add(&thread->active_scope->children, s);
   thread->active_scope = s;
}

void x_pop_scope(void)
{
   rt_model_t *m = get_model();
   model_thread_t *thread = model_thread(m);

   TRACE("pop scope %s", istr(tree_ident(thread->active_scope->where)));

   if (unlikely(thread->active_scope->kind != SCOPE_SIGNAL))
      fatal_trace("cannot pop non-signal scope");

   int offset = INT_MAX;
   list_foreach(rt_scope_t *, s, thread->active_scope->children)
      offset = MIN(offset, s->offset);
   list_foreach(rt_signal_t *, s, thread->active_scope->signals)
      offset = MIN(offset, s->offset);
   thread->active_scope->offset = offset;

   thread->active_scope = thread->active_scope->parent;
}

bool x_driving(sig_shared_t *ss, uint32_t offset, int32_t count)
{
   rt_signal_t *s = container_of(ss, rt_signal_t, shared);
   RT_LOCK(s->lock);

   TRACE("_driving %s offset=%d count=%d",
         istr(tree_ident(s->where)), offset, count);

   int ntotal = 0, ndriving = 0;
   bool found = false;
   rt_model_t *m = get_model();
   rt_proc_t *proc = get_active_proc();
   rt_nexus_t *n = split_nexus(m, s, offset, count);
   for (; count > 0; n = n->chain) {
      if (n->n_sources > 0) {
         rt_source_t *src = find_driver(n, proc);
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
              istr(proc->name), istr(tree_ident(s->where)));

   return ntotal == ndriving;
}

void *x_driving_value(sig_shared_t *ss, uint32_t offset, int32_t count)
{
   rt_signal_t *s = container_of(ss, rt_signal_t, shared);
   RT_LOCK(s->lock);

   TRACE("_driving_value %s offset=%d count=%d",
         istr(tree_ident(s->where)), offset, count);

   void *result = local_alloc(s->shared.size);

   uint8_t *p = result;
   rt_model_t *m = get_model();
   rt_proc_t *proc = get_active_proc();
   rt_nexus_t *n = split_nexus(m, s, offset, count);
   for (; count > 0; n = n->chain) {
      rt_source_t *src = find_driver(n, proc);
      if (src == NULL)
         jit_msg(NULL, DIAG_FATAL, "process %s does not contain a driver "
                 "for %s", istr(proc->name), istr(tree_ident(s->where)));

      const uint8_t *driving;
      if (n->flags & NET_F_FAST_DRIVER)
         driving = nexus_effective(n);
      else
         driving = value_ptr(n, &(src->u.driver.waveforms.value));

      memcpy(p, driving, n->width * n->size);
      p += n->width * n->size;

      count -= n->width;
      assert(count >= 0);
   }

   return result;
}

sig_shared_t *x_implicit_signal(uint32_t count, uint32_t size, tree_t where,
                                implicit_kind_t kind, ffi_closure_t *closure)
{
   TRACE("_implicit_signal %s count=%d size=%d kind=%d",
         istr(tree_ident(where)), count, size, kind);

   rt_model_t *m = get_model();

   if (m->implicitq == NULL) {
      m->implicitq = workq_new(m);
      workq_not_thread_safe(m->implicitq);
   }

   const size_t datasz = MAX(2 * count * size, 8);
   rt_implicit_t *imp = static_alloc(m, sizeof(rt_implicit_t) + datasz);
   setup_signal(m, &(imp->signal), where, count, size, SIG_F_IMPLICIT, 0);

   imp->closure = *closure;
   imp->wakeable.kind = W_IMPLICIT;

   switch (kind) {
   case IMPLICIT_GUARD:
      {
         jit_scalar_t result;
         if (!jit_try_call(m->jit, imp->closure.handle, &result,
                           imp->closure.context))
            m->force_stop = true;

         assert(size * count == 1);
         memcpy(imp->signal.shared.data, &result.integer,
                imp->signal.shared.size);
      }
      break;

   case IMPLICIT_TRANSACTION:
      assert(size * count == 1);
      imp->signal.shared.data[0] = 0;
      break;

   default:
      fatal_trace("invalid implicit signal kind %d", kind);
   }

   return &(imp->signal.shared);
}

void x_disconnect(sig_shared_t *ss, uint32_t offset, int32_t count,
                  int64_t after, int64_t reject)
{
   rt_signal_t *s = container_of(ss, rt_signal_t, shared);

   TRACE("_disconnect %s+%d len=%d after=%s reject=%s",
         istr(tree_ident(s->where)), offset, count, trace_time(after),
         trace_time(reject));

   rt_proc_t *proc = get_active_proc();

   check_postponed(after, proc);
   check_reject_limit(s, after, reject);

   rt_model_t *m = get_model();
   rt_nexus_t *n = split_nexus(m, s, offset, count);
   for (; count > 0; n = n->chain) {
      count -= n->width;
      assert(count >= 0);

      sched_disconnect(m, n, after, reject, proc);
   }
}

void x_force(sig_shared_t *ss, uint32_t offset, int32_t count, void *values)
{
   rt_signal_t *s = container_of(ss, rt_signal_t, shared);
   RT_LOCK(s->lock);

   TRACE("force signal %s+%d value=%s count=%d", istr(tree_ident(s->where)),
         offset, fmt_values(values, count), count);

   rt_proc_t *proc = get_active_proc();

   check_postponed(0, proc);

   rt_model_t *m = get_model();
   rt_nexus_t *n = split_nexus(m, s, offset, count);
   char *vptr = values;
   for (; count > 0; n = n->chain) {
      count -= n->width;
      assert(count >= 0);

      n->flags |= NET_F_FORCED;

      rt_source_t *src = get_forcing_source(m, n);
      copy_value_ptr(n, &(src->u.forcing), vptr);
      vptr += n->width * n->size;

      deltaq_insert_force_release(m, n);
   }
}

void x_release(sig_shared_t *ss, uint32_t offset, int32_t count)
{
   rt_signal_t *s = container_of(ss, rt_signal_t, shared);

   TRACE("release signal %s+%d count=%d", istr(tree_ident(s->where)),
         offset, count);

   rt_proc_t *proc = get_active_proc();

   check_postponed(0, proc);

   rt_model_t *m = get_model();
   rt_nexus_t *n = split_nexus(m, s, offset, count);
   for (; count > 0; n = n->chain) {
      count -= n->width;
      assert(count >= 0);

      if (n->flags & NET_F_FORCED)
         n->flags &= ~NET_F_FORCED;

      rt_source_t *src = get_forcing_source(m, n);
      src->disconnected = 1;

      deltaq_insert_force_release(m, n);
   }
}

void x_resolve_signal(sig_shared_t *ss, jit_handle_t handle, void *context,
                      int64_t ileft, int32_t nlits, int32_t flags)
{
   rt_signal_t *s = container_of(ss, rt_signal_t, shared);

   TRACE("resolve signal %s", istr(tree_ident(s->where)));

   ffi_closure_t closure = {
      .handle = handle,
      .context = context
   };

   rt_model_t *m = get_model();
   s->resolution = memo_resolution_fn(m, s, closure, ileft, nlits, flags);

   // Copy R_IDENT into the nexus flags to avoid rt_resolve_nexus_fast
   // having to dereference the resolution pointer in the common case
   if (s->resolution->flags & R_IDENT) {
      s->shared.flags |= NET_F_R_IDENT;

      rt_nexus_t *n = &(s->nexus);
      for (int i = 0; i < s->n_nexus; i++, n = n->chain)
         n->flags |= NET_F_R_IDENT;
   }
}

void x_process_init(jit_handle_t handle, tree_t where)
{
   rt_model_t *m = get_model();
   ident_t name = jit_get_name(m->jit, handle);

   TRACE("init process %s", istr(name));

   rt_scope_t *s = model_thread(m)->active_scope;
   assert(s != NULL);
   assert(s->kind == SCOPE_INSTANCE);

   rt_proc_t *p = xcalloc(sizeof(rt_proc_t));
   p->where     = where;
   p->name      = name;
   p->handle    = handle;
   p->scope     = s;
   p->privdata  = mptr_new(m->mspace, "process privdata");

   p->wakeable.kind      = W_PROC;
   p->wakeable.pending   = false;
   p->wakeable.postponed = false;
   p->wakeable.delayed   = false;

   list_add(&s->procs, p);
}
