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
};

typedef enum { E_PROCESS, E_DRIVER } event_kind_t;

struct deltaq {
   uint64_t       delta;
   int            iteration;
   event_kind_t   kind;
   union {
      struct rt_proc *wake_proc;
      struct {
         struct signal *signal;
         unsigned      source;
      };
   };
   struct deltaq  *next;
};

union sigvalue {
   uint64_t val;
   void     *ptr;
};

struct waveform {
   union sigvalue  value;
   uint64_t        when;
   struct waveform *next;
};

struct sens_list {
   struct rt_proc   *proc;
   struct sens_list *next;
};

struct signal {
   union sigvalue   resolved;
   tree_t           decl;
   int32_t          flags;
   int32_t          n_sources;
   struct waveform  **sources;
   struct sens_list *sensitive;
};

static struct rt_proc *procs = NULL;
static struct rt_proc *active_proc = NULL;
static struct deltaq  *eventq = NULL;

static size_t   n_procs = 0;
static uint64_t now = 0;
static int      iteration = -1;
static bool     trace_on = false;

#define MAX_ACTIVE_SIGS 128
static struct signal *active_signals[MAX_ACTIVE_SIGS];
static unsigned      n_active_signals = 0;

static void deltaq_insert(uint64_t delta, struct rt_proc *wake,
                          struct signal *signal, unsigned source);
static void _tracef(const char *fmt, ...);

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
      struct signal *first = tree_attr_ptr(sig->decl, ident_new("signal"));
      p += snprintf(p, end - p, "[%zd]", sig - first);
   }
   return buf;
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

   struct waveform *w = xmalloc(sizeof(struct waveform));
   w->value.val = value;
   w->when      = now + after;
   w->next      = NULL;

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

      struct waveform *dummy = xmalloc(sizeof(struct waveform));
      dummy->value.val = value;
      dummy->when      = 0;
      dummy->next      = w;

      sig->sources[source] = dummy;
   }
   else
      last->next = w;

   deltaq_insert(after, NULL, sig, source);
}

void _sched_event(void *_sig)
{
   struct signal *sig = _sig;

   TRACE("_sched_event %s proc %s", fmt_sig(sig),
         istr(tree_ident(active_proc->source)));

   struct sens_list *node = xmalloc(sizeof(struct sens_list));
   node->proc = active_proc;
   node->next = sig->sensitive;

   sig->sensitive = node;
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
   fwrite(msg, 1, msg_len, stderr);
   fprintf(stderr, "\n");

   if (severity >= 2)
      exit(EXIT_FAILURE);
}

uint64_t _std_standard_now(void)
{
   return now;
}

void _array_copy(void *dst, const void *src, int32_t n, int32_t sz)
{
   TRACE("_array_copy dst=%p src=%p %dx%d", dst, src, n, sz);
   memcpy(dst, src, n * sz);
}

int8_t _array_eq(const void *lhs, const void *rhs, int32_t n, int32_t sz)
{
   TRACE("_array_eq lhs=%p rhs=%p %dx%d", lhs, rhs, n, sz);
   return memcmp(lhs, rhs, n * sz) == 0;
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

   struct deltaq *q = xmalloc(sizeof(struct deltaq));
   q->next      = NULL;
   q->iteration = (delta == 0 ? iteration + 1 : 0);

   if (wake != NULL) {
      q->kind      = E_PROCESS;
      q->wake_proc = wake;
   }
   else {
      q->kind   = E_DRIVER;
      q->signal = signal;
      q->source = source;
   }

   if (eventq == NULL)
      eventq = q;
   else {
      struct deltaq *it = eventq, *last = NULL;
      uint64_t sum = 0;
      while (it != NULL && sum + it->delta <= delta) {
         sum += it->delta;
         delta -= it->delta;

         last = it;
         it = it->next;
      }

      if (it != NULL) {
         q->next = it;
         if (last != NULL)
            last->next = q;
         else
            eventq = q;

         it->delta -= delta;
      }
      else
         last->next = q;
   }

   q->delta = delta;
}

static void deltaq_pop(void)
{
   struct deltaq *next = eventq->next;
   free(eventq);
   eventq = next;
}

#if TRACE_DELTAQ > 0
static void deltaq_dump(void)
{
   struct deltaq *it;
   for (it = eventq; it != NULL; it = it->next) {
      printf("%s\t", fmt_time(it->delta));
      if (it->kind == E_DRIVER)
         printf("driver\t %s\n", fmt_sig(it->signal));
      else
         printf("process\t %s\n", istr(tree_ident(it->wake_proc->source)));
   }
}
#endif

static void rt_setup(tree_t top)
{
   n_procs = tree_stmts(top);
   procs   = xmalloc(sizeof(struct rt_proc) * n_procs);

   jit_bind_fn("STD.STANDARD.NOW", _std_standard_now);

   for (unsigned i = 0; i < tree_decls(top); i++) {
      tree_t d = tree_decl(top, i);
      assert(tree_kind(d) == T_SIGNAL_DECL);

      struct signal *s = jit_var_ptr(istr(tree_ident(d)));

      type_t type = tree_type(d);
      if (type_kind(type) == T_CARRAY) {
         int64_t low, high;
         range_bounds(type_dim(type, 0), &low, &high);

         for (unsigned i = 0; i < high - low + 1; i++) {
            TRACE("signal %s[%d] at %p", istr(tree_ident(d)), i, &s[i]);
            s[i].decl      = d;
            s[i].sensitive = NULL;
         }
      }
      else {
         TRACE("signal %s at %p", istr(tree_ident(d)), s);
         s->decl      = d;
         s->sensitive = NULL;
      }

      tree_add_attr_ptr(d, ident_new("signal"), s);
   }

   for (unsigned i = 0; i < tree_stmts(top); i++) {
      tree_t p = tree_stmt(top, i);
      assert(tree_kind(p) == T_PROCESS);

      procs[i].source  = p;
      procs[i].proc_fn = jit_fun_ptr(istr(tree_ident(p)));

      TRACE("process %s at %p", istr(tree_ident(p)), procs[i].proc_fn);
   }
}

static void rt_run(struct rt_proc *proc, bool reset)
{
   TRACE("%s process %s", reset ? "reset" : "run",
         istr(tree_ident(proc->source)));

   active_proc = proc;
   (*proc->proc_fn)(reset ? 1 : 0);
}

static void rt_initial(void)
{
   // Initialisation is described in LRM 93 section 12.6.4

   now = 0;

   for (size_t i = 0; i < n_procs; i++)
      rt_run(&procs[i], true /* reset */);
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
         if (s->resolved.val != w_next->value.val) {
            new_flags |= SIGNAL_F_EVENT;

            struct sens_list *it, *next;
            for (it = s->sensitive; it != NULL; it = next) {
               TRACE("wakeup process %s", istr(tree_ident(it->proc->source)));
               next = it->next;
               deltaq_insert(0, it->proc, NULL, 0);
               free(it);
            }
         }

         assert(n_active_signals < MAX_ACTIVE_SIGS);
         active_signals[n_active_signals++] = s;
      }

      s->resolved        = w_next->value;
      s->flags          |= new_flags;
      s->sources[source] = w_next;

      free(w_now);
   }
   else
      assert(w_now != NULL);
}

static void rt_cycle(void)
{
   // Simulation cycle is described in LRM 93 section 12.6.4

   if (eventq->delta > 0) {
      now += eventq->delta;
      eventq->delta = 0;
      assert(eventq->iteration == 0);
      iteration = 0;
   }
   else
      iteration = eventq->iteration;

   TRACE("begin cycle");

#if TRACE_DELTAQ > 0
   if (trace_on)
      deltaq_dump();
#endif

   do {
      switch (eventq->kind) {
      case E_PROCESS:
         rt_run(eventq->wake_proc, false /* reset */);
         break;
      case E_DRIVER:
         rt_update_driver(eventq->signal, eventq->source);
         break;
      }

      deltaq_pop();
   } while (eventq != NULL
            && (eventq->delta == 0 && eventq->iteration == iteration));

   for (unsigned i = 0; i < n_active_signals; i++)
      active_signals[i]->flags &= ~(SIGNAL_F_ACTIVE | SIGNAL_F_EVENT);
   n_active_signals = 0;
}

void rt_trace_en(bool en)
{
   trace_on = en;
}

void rt_batch_exec(tree_t e)
{
   jit_init(tree_ident(e));

   rt_setup(e);
   rt_initial();
   while (eventq != NULL)
      rt_cycle();

   jit_shutdown();
}

static void rt_slave_run(slave_run_msg_t *msg)
{
   const uint64_t end = (msg->time == 0 ? ~0 : now + msg->time);
   while (now < end && eventq != NULL)
      rt_cycle();
}

void rt_slave_exec(tree_t e)
{
   jit_init(tree_ident(e));

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
         break;

      case SLAVE_RUN:
         rt_slave_run((slave_run_msg_t*)buf);
         break;
      }
   }
}
