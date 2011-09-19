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

#include <assert.h>
#include <stdint.h>
#include <stdarg.h>
#include <inttypes.h>
#include <stdlib.h>

typedef void (*simple_proc_fn_t)(void);

struct rt_proc {
   tree_t           source;
   simple_proc_fn_t simple_proc_fn;
};

struct deltaq {
   uint64_t       delta;
   struct rt_proc *wake_proc;
   struct deltaq  *next;
};

static struct rt_proc *procs = NULL;
static struct rt_proc *active_proc = NULL;
static struct deltaq  *eventq = NULL;
static size_t         n_procs = 0;
static uint64_t       now = 0;
static bool           trace_on = false;

static void deltaq_insert(uint64_t delta, struct rt_proc *wake);
static void _tracef(const char *fmt, ...);

#define TRACE(...) if (trace_on) _tracef(__VA_ARGS__)

////////////////////////////////////////////////////////////////////////////////
// Runtime support functions

void _sched_process(uint64_t delay)
{
   TRACE("_sched_process delay=%u!!", (unsigned)delay);
   deltaq_insert(delay, active_proc);
}

void _assert_fail(int8_t report, const uint8_t *msg, int32_t msg_len)
{
   // LRM 93 section 8.2
   // The error message consists of at least
   // a) An indication that this message is from an assertion
   // b) The value of the severity level
   // c) The value of the message string
   // d) The name of the design unit containing the assertion

   fprintf(stderr, "Assertion %s: ", "Error");
   fwrite(msg, 1, msg_len, stderr);
   fprintf(stderr, "\n");
}

uint64_t _std_standard_now(void)
{
   return now;
}

////////////////////////////////////////////////////////////////////////////////
// Simulation kernel

static void _tracef(const char *fmt, ...)
{
   va_list ap;
   va_start(ap, fmt);

   fprintf(stderr, "TRACE %llufs: ", (unsigned long long)now);
   vfprintf(stderr, fmt, ap);
   fprintf(stderr, "\n");

   va_end(ap);
}

static void deltaq_insert(uint64_t delta, struct rt_proc *wake)
{
   struct deltaq *q = xmalloc(sizeof(struct deltaq));
   q->wake_proc = wake;
   q->next      = NULL;

   if (eventq == NULL)
      eventq = q;
   else {
      struct deltaq *it = eventq;
      uint64_t sum = 0;
      while (sum + it->delta < delta && it->next != NULL) {
         sum += it->delta;
         delta -= it->delta;
      }

      q->next = it->next;   // XXX: need to change it->next delta
      it->next = q;
   }

   q->delta = delta;
}

static void deltaq_pop(void)
{
   struct deltaq *next = eventq->next;
   free(eventq);
   eventq = next;
}

static void rt_setup(tree_t top)
{
   n_procs = tree_stmts(top);
   procs   = xmalloc(sizeof(struct rt_proc) * n_procs);

   jit_bind_fn("STD.STANDARD.NOW", _std_standard_now);

   for (unsigned i = 0; i < tree_stmts(top); i++) {
      tree_t p = tree_stmt(top, i);
      assert(tree_kind(p) == T_PROCESS);

      procs[i].source         = p;
      procs[i].simple_proc_fn = jit_fun_ptr(istr(tree_ident(p)));

      TRACE("%s fun at %p", istr(tree_ident(p)), procs[i].simple_proc_fn);
   }
}

static void rt_run(struct rt_proc *proc)
{
   TRACE("run process %s", istr(tree_ident(proc->source)));

   active_proc = proc;
   (*proc->simple_proc_fn)();
}

static void rt_initial(void)
{
   // Initialisation is described in LRM 93 section 12.6.4

   now = 0;

   for (size_t i = 0; i < n_procs; i++)
      rt_run(&procs[i]);
}

static void rt_cycle(void)
{
   // Simulation cycle is described in LRM 93 section 12.6.4

   now += eventq->delta;

   rt_run(eventq->wake_proc);

   deltaq_pop();
}

void rt_trace_en(bool en)
{
   trace_on = en;
}

void rt_exec(ident_t top)
{
   ident_t ename = ident_prefix(top, ident_new("elab"));
   tree_t e = lib_get(lib_work(), ename);
   if (e == NULL)
      fatal("%s not elaborated", istr(top));
   else if (tree_kind(e) != T_ELAB)
      fatal("%s not suitable top level", istr(top));

   jit_init(ename);

   rt_setup(e);
   rt_initial();
   while (eventq != NULL)
      rt_cycle();

   jit_shutdown();
}
