//
//  Copyright (C) 2011  Nick Gasson
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

#include "rt.h"
#include "tree.h"
#include "lib.h"
#include "util.h"
#include "signal.h"
#include "slave.h"
#include "alloc.h"
#include "heap.h"

#include <assert.h>
#include <stdint.h>
#include <stdarg.h>
#include <inttypes.h>
#include <stdlib.h>
#include <string.h>

#define TRACE_DELTAQ 1

typedef void (*proc_fn_t)(int32_t reset);

struct rt_proc {
   tree_t    source;
   proc_fn_t proc_fn;
   unsigned  wakeup_gen;
   void      *tmp_buf;
   size_t    tmp_buf_sz;
};

typedef enum { E_DRIVER, E_PROCESS } event_kind_t;

struct event {
   uint64_t       when;
   int            iteration;
   event_kind_t   kind;
   union {
      struct rt_proc *wake_proc;
      struct {
         struct signal *signal;
         unsigned      source;
      };
   };
};

struct waveform {
   uint64_t        value;
   uint64_t        when;
   struct waveform *next;
};

struct sens_list {
   struct rt_proc   *proc;
   unsigned         wakeup_gen;
   struct sens_list *next;
};

struct signal {
   uint64_t         resolved;
   uint64_t         last_value;
   tree_t           decl;
   int32_t          flags;
   int32_t          n_sources;
   struct waveform  **sources;
   struct sens_list *sensitive;
   sig_event_fn_t   event_cb;
};

static struct rt_proc   *procs = NULL;
static struct rt_proc   *active_proc = NULL;
static struct sens_list *resume = NULL;

static heap_t   eventq_heap = NULL;
static size_t   n_procs = 0;
static uint64_t now = 0;
static int      iteration = -1;
static bool     trace_on = false;

static rt_alloc_stack_t event_stack = NULL;
static rt_alloc_stack_t waveform_stack = NULL;
static rt_alloc_stack_t sens_list_stack = NULL;

#define MAX_ACTIVE_SIGS 128
static struct signal *active_signals[MAX_ACTIVE_SIGS];
static unsigned      n_active_signals = 0;

static void deltaq_insert(uint64_t delta, struct rt_proc *wake,
                          struct signal *signal, unsigned source);
static void *rt_tmp_alloc(size_t sz);
static void _tracef(const char *fmt, ...);
static ident_t i_signal = NULL;

#define TRACE(...) if (trace_on) _tracef(__VA_ARGS__)

////////////////////////////////////////////////////////////////////////////////
// Utilities

static const char *fmt_time_r(char *buf, size_t len, uint64_t t)
{
   struct {
      uint64_t time;
      const char *unit;
   } units[] = {
      { 1ull, "fs" },
      { 1000ull, "ps" },
      { 1000000ull, "ns" },
      { 1000000000ull, "us" },
      { 1000000000000ull, "ms" },
      { 0, NULL }
   };

   int u = 0;
   while (units[u + 1].unit && (t % units[u + 1].time == 0))
      ++u;

   snprintf(buf, len, "%"PRIu64"%s",
            t / units[u].time, units[u].unit);

   return buf;
}

static const char *fmt_time(uint64_t t)
{
   static char buf[64];
   return fmt_time_r(buf, sizeof(buf), t);
}

static const char *fmt_sig(struct signal *sig)
{
   static char buf[256];
   char *p = buf;
   const char *end = buf + sizeof(buf);
   p += snprintf(buf, end - p, "%s", istr(tree_ident(sig->decl)));
   if (type_kind(tree_type(sig->decl)) == T_CARRAY) {
      struct signal *first = tree_attr_ptr(sig->decl, i_signal);
      p += snprintf(p, end - p, "[%zd]", sig - first);
   }
   return buf;
}

static inline uint64_t heap_key(uint64_t when, event_kind_t kind)
{
   // Use the bottom bit of the key to indicate the kind
   // The highest priority should have the lowest enumeration value
   return (when << 1) | (kind & 1);
}

////////////////////////////////////////////////////////////////////////////////
// Runtime support functions

void _sched_process(int64_t delay)
{
   TRACE("_sched_process delay=%s", fmt_time(delay));
   deltaq_insert(delay, active_proc, NULL, 0);
}

void _sched_waveform(void *_sig, int32_t source, int64_t value, int64_t after)
{
   struct signal *sig = _sig;

   TRACE("_sched_waveform %s source=%d value=%"PRIx64" after=%s",
         fmt_sig(sig), source, value, fmt_time(after));

   // Allocate memory for drivers on demand
   const size_t ptr_sz = sizeof(struct waveform *);
   if (sig->sources == NULL) {
      sig->n_sources = source + 1;
      sig->sources = xmalloc(sig->n_sources * ptr_sz);
      memset(sig->sources, '\0', sig->n_sources * ptr_sz);
   }
   else if (source >= sig->n_sources) {
      // TODO: scale size more aggressively here?
      sig->sources = xrealloc(sig->sources, (source + 1) * ptr_sz);
      memset(&sig->sources[sig->n_sources], '\0',
             (source + 1 - sig->n_sources) * ptr_sz);
      sig->n_sources = source + 1;
   }

   struct waveform *w = rt_alloc(waveform_stack);
   w->value = value;
   w->when  = now + after;
   w->next  = NULL;

   // TODO: transport vs. inertial

   struct waveform *it = sig->sources[source], *last = NULL;
   while (it != NULL && it->when <= w->when) {
      last = it;
      it = it->next;
   }

   w->next = it;
   if (last == NULL) {
      // Assigning the initial value of a driver
      // Generate a dummy transaction so the real one will be propagated
      // at time zero (since the first element on the transaction queue
      // is assumed to have already occured)
      assert(now == 0);
      assert(after == 0);

      struct waveform *dummy = rt_alloc(waveform_stack);
      dummy->value = value;
      dummy->when  = 0;
      dummy->next  = w;

      sig->sources[source] = dummy;
   }
   else
      last->next = w;

   deltaq_insert(after, NULL, sig, source);
}

void _sched_event(void *_sig, int32_t n)
{
   struct signal *sig = _sig;

   TRACE("_sched_event %s n=%d proc %s", fmt_sig(sig), n,
         istr(tree_ident(active_proc->source)));

   for (int i = 0; i < n; i++) {
      // See if there is already a stale entry in the sensitvity
      // list for this process
      struct sens_list *it = sig[i].sensitive;
      for (; it != NULL && it->proc != active_proc; it = it->next)
         ;

      if (it == NULL) {
         struct sens_list *node = rt_alloc(sens_list_stack);
         node->proc       = active_proc;
         node->wakeup_gen = active_proc->wakeup_gen + 1;
         node->next       = sig[i].sensitive;

         sig[i].sensitive = node;
      }
      else {
         // Reuse the stale entry
         it->wakeup_gen = active_proc->wakeup_gen + 1;
      }
   }
}

void _assert_fail(int8_t report, const uint8_t *msg,
                  int32_t msg_len, int8_t severity)
{
   // LRM 93 section 8.2
   // The error message consists of at least
   // a) An indication that this message is from an assertion
   // b) The value of the severity level
   // c) The value of the message string
   // d) The name of the design unit containing the assertion

   assert(severity < 4);

   const char *levels[] = {
      "Note", "Warning", "Error", "Failure"
   };

   fprintf(stderr, "%s+%d: %s %s: ",
           fmt_time(now), iteration,
           report ? "Report" : "Assertion",
           levels[severity]);
   if (msg_len >= 0)
      fwrite(msg, 1, msg_len, stderr);
   else
      fputs((const char *)msg, stderr);
   fprintf(stderr, "\n");

   if (severity >= 2)
      exit(EXIT_FAILURE);
}

uint64_t _std_standard_now(void)
{
   return now;
}

void _array_copy(void *dst, const void *src, int32_t off, int32_t n, int32_t sz)
{
   TRACE("_array_copy dst=%p+%d src=%p %dx%d", dst, off, src, n, sz);
   memcpy((char *)dst + (off * sz) , src, n * sz);
}

int8_t _array_eq(const void *lhs, const void *rhs,
                 int32_t n, int32_t sz, int32_t op)
{
   TRACE("_array_eq lhs=%p rhs=%p %dx%d op=%d", lhs, rhs, n, sz, op);
   if (op) {
      const uint8_t *pl = lhs;
      const uint8_t *pr = (const uint8_t *)rhs + ((n - 1) * sz);
      while (n--) {
         for (int i = 0; i < sz; i++) {
            if (*(pl + i) != *(pr + i))
               return 0;
         }
         pl += sz;
         pr -= sz;
      }
      return 1;
   }
   else
      return memcmp(lhs, rhs, n * sz) == 0;
}

int8_t *_image(int64_t val)
{
   char *buf = rt_tmp_alloc(32);
   snprintf(buf, 32, "%"PRIi64, val);
   return (int8_t*)buf;
}

////////////////////////////////////////////////////////////////////////////////
// Simulation kernel

static void _tracef(const char *fmt, ...)
{
   va_list ap;
   va_start(ap, fmt);

   char buf[64];
   if (iteration < 0)
      fprintf(stderr, "TRACE (init): ");
   else
      fprintf(stderr, "TRACE %s+%d: ",
              fmt_time_r(buf, sizeof(buf), now), iteration);
   vfprintf(stderr, fmt, ap);
   fprintf(stderr, "\n");

   va_end(ap);
}

static void deltaq_insert(uint64_t delta, struct rt_proc *wake,
                          struct signal *signal, unsigned source)
{
   assert(!(wake != NULL && signal != NULL));

   struct event *e = rt_alloc(event_stack);
   e->iteration = (delta == 0 ? iteration + 1 : 0);
   e->when      = now + delta;

   if (wake != NULL) {
      e->kind      = E_PROCESS;
      e->wake_proc = wake;
   }
   else {
      e->kind   = E_DRIVER;
      e->signal = signal;
      e->source = source;
   }

   heap_insert(eventq_heap, heap_key(e->when, e->kind), e);
}

#if TRACE_DELTAQ > 0
static void deltaq_walk(uint64_t key, void *user, void *context)
{
   struct event *e = user;

   printf("%s\t", fmt_time(e->when));
   if (e->kind == E_DRIVER)
      printf("driver\t %s\n", fmt_sig(e->signal));
   else
      printf("process\t %s\n", istr(tree_ident(e->wake_proc->source)));
}

static void deltaq_dump(void)
{
   heap_walk(eventq_heap, deltaq_walk, NULL);
#if 0
   struct deltaq *it;
   for (it = eventq; it != NULL; it = it->next) {
      printf("%s\t", fmt_time(it->delta));
      if (it->kind == E_DRIVER)
         printf("driver\t %s\n", fmt_sig(it->signal));
      else
         printf("process\t %s\n", istr(tree_ident(it->wake_proc->source)));
   }
#endif
}
#endif

static void *rt_tmp_alloc(size_t sz)
{
   // Allocate sz bytes that will be freed when the process suspends

   size_t base = active_proc->tmp_buf_sz;
   if (active_proc->tmp_buf != NULL)
      active_proc->tmp_buf = xrealloc(active_proc->tmp_buf, base + sz);
   else
      active_proc->tmp_buf = xmalloc(sz);
   active_proc->tmp_buf_sz = base + sz;
   return (char*)active_proc->tmp_buf + base;
}

static void rt_reset_signal(struct signal *s, tree_t decl)
{
   if (s->sources != NULL) {
      for (int i = 0; i < s->n_sources; i++) {
         struct waveform *w, *wnext;
         w = s->sources[i];
         do {
            wnext = w->next;
            rt_free(waveform_stack, w);
         } while ((w = wnext) != NULL);
      }

      free(s->sources);
   }

   s->decl      = decl;
   s->sensitive = NULL;
   s->sources   = NULL;
   s->event_cb  = NULL;
}

static void rt_setup(tree_t top)
{
   now = 0;
   iteration = -1;

   assert(eventq_heap == NULL);
   assert(resume == NULL);

   eventq_heap = heap_new(512);

   if (procs == NULL) {
      n_procs = tree_stmts(top);
      procs   = xmalloc(sizeof(struct rt_proc) * n_procs);
   }

   for (unsigned i = 0; i < tree_decls(top); i++) {
      tree_t d = tree_decl(top, i);
      if (tree_kind(d) != T_SIGNAL_DECL)
         continue;

      struct signal *s = jit_var_ptr(istr(tree_ident(d)));

      type_t type = tree_type(d);
      if (type_kind(type) == T_CARRAY) {
         int64_t low, high;
         range_bounds(type_dim(type, 0), &low, &high);

         for (unsigned i = 0; i < high - low + 1; i++) {
            TRACE("signal %s[%d] at %p", istr(tree_ident(d)), i, &s[i]);
            rt_reset_signal(&s[i], d);
         }
      }
      else {
         TRACE("signal %s at %p", istr(tree_ident(d)), s);
         rt_reset_signal(s, d);
      }

      tree_add_attr_ptr(d, i_signal, s);
   }

   for (unsigned i = 0; i < tree_stmts(top); i++) {
      tree_t p = tree_stmt(top, i);
      assert(tree_kind(p) == T_PROCESS);

      procs[i].source     = p;
      procs[i].proc_fn    = jit_fun_ptr(istr(tree_ident(p)));
      procs[i].wakeup_gen = 0;
      procs[i].tmp_buf    = NULL;
      procs[i].tmp_buf_sz = 0;

      TRACE("process %s at %p", istr(tree_ident(p)), procs[i].proc_fn);
   }
}

static void rt_run(struct rt_proc *proc, bool reset)
{
   TRACE("%s process %s", reset ? "reset" : "run",
         istr(tree_ident(proc->source)));

   active_proc = proc;
   (*proc->proc_fn)(reset ? 1 : 0);
   ++(proc->wakeup_gen);

   // Free any temporary memory allocated by the process
   if (proc->tmp_buf) {
      free(proc->tmp_buf);
      proc->tmp_buf    = NULL;
      proc->tmp_buf_sz = 0;
   }
}

static void rt_initial(void)
{
   // Initialisation is described in LRM 93 section 12.6.4

   for (size_t i = 0; i < n_procs; i++)
      rt_run(&procs[i], true /* reset */);
}

static void rt_wakeup(struct sens_list *sl)
{
   // To avoid having each process keep a list of the signals it is
   // sensitive to, each process has a "wakeup generation" number which
   // is incremented after each wait statement and stored in the signal
   // sensitivity list. We then ignore any sensitivity list elements
   // where the generation doesn't match the current process wakeup
   // generation: these correspond to stale "wait on" statements that
   // have already resumed.

   if (sl->wakeup_gen == sl->proc->wakeup_gen) {
      TRACE("wakeup process %s", istr(tree_ident(sl->proc->source)));
      sl->next = resume;
      resume = sl;
   }
   else
      rt_free(sens_list_stack, sl);
}

static void rt_update_driver(struct signal *s, unsigned source)
{
   struct waveform *w_now  = s->sources[source];
   struct waveform *w_next = w_now->next;

   if (w_next != NULL && w_next->when == now) {
      TRACE("update signal %s value %"PRIx64,
            fmt_sig(s), w_next->value);

      int32_t new_flags = 0;
      const bool first_cycle = (iteration == 0 && now == 0);
      if (!first_cycle) {
         new_flags = SIGNAL_F_ACTIVE;
         if (s->resolved != w_next->value) {
            new_flags |= SIGNAL_F_EVENT;

            struct sens_list *it, *next;
            for (it = s->sensitive; it != NULL; it = next) {
               next = it->next;
               rt_wakeup(it);
            }
            s->sensitive = NULL;

            if (s->event_cb)
               (*s->event_cb)(s->decl);
         }

         assert(n_active_signals < MAX_ACTIVE_SIGS);
         active_signals[n_active_signals++] = s;
      }
      else {
         s->resolved = w_next->value;
      }

      // LAST_VALUE is the same as the initial value when
      // there have been no events on the signal otherwise
      // only update it when there is an event
      if (first_cycle)
         s->last_value = w_next->value;
      else if (new_flags & SIGNAL_F_EVENT)
         s->last_value = s->resolved;

      s->resolved        = w_next->value;
      s->flags          |= new_flags;
      s->sources[source] = w_next;

      rt_free(waveform_stack, w_now);
   }
   else
      assert(w_now != NULL);
}

static void rt_cycle(void)
{
   // Simulation cycle is described in LRM 93 section 12.6.4

   struct event *peek = heap_min(eventq_heap);

   if (peek->when > now) {
      now = peek->when;
      assert(peek->iteration == 0);
      iteration = 0;
   }
   else
      iteration = peek->iteration;

   TRACE("begin cycle");

#if TRACE_DELTAQ > 0
   if (trace_on)
      deltaq_dump();
#endif

   for (;;) {
      struct event *event = heap_extract_min(eventq_heap);

      switch (event->kind) {
      case E_PROCESS:
         rt_run(event->wake_proc, false /* reset */);
         break;
      case E_DRIVER:
         rt_update_driver(event->signal, event->source);
         break;
      }

      rt_free(event_stack, event);

      if (heap_size(eventq_heap) == 0)
         break;

      peek = heap_min(eventq_heap);
      if (peek->when > now || peek->iteration != iteration)
         break;
   }

   // Run all processes that resumed because of signal events
   while (resume != NULL) {
      struct sens_list *next = resume->next;
      rt_run(resume->proc, false /* reset */);
      rt_free(sens_list_stack, resume);
      resume = next;
   }

   for (unsigned i = 0; i < n_active_signals; i++)
      active_signals[i]->flags &= ~(SIGNAL_F_ACTIVE | SIGNAL_F_EVENT);
   n_active_signals = 0;
}

static void rt_one_time_init(void)
{
   i_signal = ident_new("signal");

   jit_bind_fn("STD.STANDARD.NOW", _std_standard_now);

   event_stack     = rt_alloc_stack_new(sizeof(struct event));
   waveform_stack  = rt_alloc_stack_new(sizeof(struct waveform));
   sens_list_stack = rt_alloc_stack_new(sizeof(struct sens_list));
}

static void rt_cleanup_signal(struct signal *sig)
{
   for (int j = 0; j < sig->n_sources; j++) {
      while (sig->sources[j] != NULL) {
         struct waveform *next = sig->sources[j]->next;
         rt_free(waveform_stack, sig->sources[j]);
         sig->sources[j] = next;
      }
   }
   free(sig->sources);

   while (sig->sensitive) {
      struct sens_list *next = sig->sensitive->next;
      rt_free(sens_list_stack, sig->sensitive);
      sig->sensitive = next;
   }
}

static void rt_cleanup(tree_t top)
{
   assert(resume == NULL);

   while (heap_size(eventq_heap) > 0)
      rt_free(event_stack, heap_extract_min(eventq_heap));

   heap_free(eventq_heap);
   eventq_heap = NULL;

   for (unsigned i = 0; i < tree_decls(top); i++) {
      tree_t d = tree_decl(top, i);
      if (tree_kind(d) != T_SIGNAL_DECL)
         continue;

      struct signal *sig = tree_attr_ptr(d, i_signal);

      type_t type = tree_type(d);
      if (type_kind(type) == T_CARRAY) {
         int64_t low, high;
         range_bounds(type_dim(type, 0), &low, &high);

         for (unsigned i = 0; i < high - low + 1; i++)
            rt_cleanup_signal(&sig[i]);
      }
      else
         rt_cleanup_signal(sig);
   }

   rt_alloc_stack_destroy(event_stack);
   rt_alloc_stack_destroy(waveform_stack);
   rt_alloc_stack_destroy(sens_list_stack);
}

void rt_trace_en(bool en)
{
   trace_on = en;
}

static bool rt_stop_now(uint64_t stop_time)
{
   struct event *peek = heap_min(eventq_heap);
   return peek->when > stop_time;
}

void rt_batch_exec(tree_t e, uint64_t stop_time)
{
   jit_init(tree_ident(e));

   rt_one_time_init();
   rt_setup(e);
   rt_initial();
   vcd_restart(e);
   while (heap_size(eventq_heap) > 0 && !rt_stop_now(stop_time))
      rt_cycle();
   rt_cleanup(e);

   jit_shutdown();
}

static void rt_slave_run(slave_run_msg_t *msg)
{
   const uint64_t end = now + msg->time;
   while (heap_size(eventq_heap) > 0 && !rt_stop_now(end));
      rt_cycle();
}

static void rt_slave_read_signal(slave_read_signal_msg_t *msg,
                                 tree_rd_ctx_t ctx)
{
   tree_t t = tree_read_recall(ctx, msg->index);
   assert(tree_kind(t) == T_SIGNAL_DECL);

   type_t type = tree_type(t);

   int64_t low = 0, high = 0;
   if (type_kind(type) == T_CARRAY)
      range_bounds(type_dim(type, 0), &low, &high);

   assert(msg->len <= high - low + 1);

   struct signal *sig = tree_attr_ptr(t, i_signal);

   const size_t rsz =
      sizeof(reply_read_signal_msg_t) + (msg->len * sizeof(uint64_t));
   reply_read_signal_msg_t *reply = xmalloc(rsz);

   reply->len = msg->len;
   for (unsigned i = 0; i < msg->len; i++)
      reply->values[i] = sig[i].resolved;

   slave_post_msg(REPLY_READ_SIGNAL, reply, rsz);

   free(reply);
}

void rt_slave_exec(tree_t e, tree_rd_ctx_t ctx)
{
   jit_init(tree_ident(e));
   rt_one_time_init();

   for (;;) {
      char buf[256];
      slave_msg_t msg;
      size_t len = sizeof(buf);
      slave_get_msg(&msg, buf, &len);

      switch (msg) {
      case SLAVE_QUIT:
         jit_shutdown();
         return;

      case SLAVE_RESTART:
         rt_setup(e);
         rt_initial();
         vcd_restart(e);
         break;

      case SLAVE_RUN:
         rt_slave_run((slave_run_msg_t*)buf);
         break;

      case SLAVE_READ_SIGNAL:
         rt_slave_read_signal((slave_read_signal_msg_t*)buf, ctx);
         break;

      default:
         assert(false);
      }
   }

   rt_cleanup(e);
   jit_shutdown();
}

void rt_set_event_cb(tree_t s, sig_event_fn_t fn)
{
   assert(tree_kind(s) == T_SIGNAL_DECL);

   struct signal *sig = tree_attr_ptr(s, i_signal);
   assert(sig != NULL);

   sig->event_cb = fn;
}

size_t rt_signal_value(tree_t decl, uint64_t *buf, size_t max)
{
   assert(tree_kind(decl) == T_SIGNAL_DECL);

   type_t type = tree_type(decl);

   int64_t low = 0, high = 0;
   if (type_kind(type) == T_CARRAY) {
      range_bounds(type_dim(type, 0), &low, &high);
   }
   struct signal *base = tree_attr_ptr(decl, i_signal);

   size_t n_vals = high - low + 1;
   if (n_vals > max)
      n_vals = max;

   for (unsigned i = 0; i < n_vals; i++)
      buf[i] = base[i].resolved;

   return n_vals;
}
