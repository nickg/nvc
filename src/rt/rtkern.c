//
//  Copyright (C) 2011-2013  Nick Gasson
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
#include "common.h"

#include <assert.h>
#include <stdint.h>
#include <stdarg.h>
#include <inttypes.h>
#include <stdlib.h>
#include <string.h>
#include <setjmp.h>
#include <math.h>
#include <errno.h>
#include <sys/time.h>
#include <sys/resource.h>

#define TRACE_DELTAQ  1
#define EXIT_SEVERITY 2

typedef void (*proc_fn_t)(int32_t reset);
typedef uint64_t (*resolution_fn_t)(uint64_t *vals, int32_t n);

struct tmp_chunk_hdr {
   struct tmp_chunk *next;
   size_t alloced;
   char   *external;
};

#define TMP_BUF_SZ (1024 - sizeof(struct tmp_chunk_hdr))

struct tmp_chunk {
   struct tmp_chunk_hdr hdr;
   char buf[TMP_BUF_SZ];
};

struct rt_proc {
   tree_t           source;
   proc_fn_t        proc_fn;
   unsigned         wakeup_gen;
   struct tmp_chunk *tmp_chunks;
};

typedef enum { E_DRIVER, E_PROCESS } event_kind_t;

struct event {
   uint64_t        when;
   int             iteration;
   event_kind_t    kind;
   struct rt_proc *proc;
   struct signal  *signal;
   int             length;
};

struct waveform {
   uint64_t        when;
   struct waveform *next;
   uint64_t        value;
};

struct sens_list {
   struct rt_proc   *proc;
   unsigned         wakeup_gen;
   struct sens_list *next;
};

struct driver {
   struct rt_proc  *proc;
   struct waveform *waveforms;
};

struct signal {
   uint64_t          resolved;
   uint64_t          last_value;
   tree_t            decl;
   uint8_t           flags;
   uint8_t           n_drivers;
   uint16_t          offset;
   struct driver    *drivers;
   struct sens_list *sensitive;
   sig_event_fn_t    event_cb;
   resolution_fn_t   resolution;
   uint64_t          last_event;
};

struct uarray {
   void    *ptr;
   struct {
      int32_t left;
      int32_t right;
      int8_t  dir;
   } dims[1];
};

struct loaded {
   const char    *name;
   tree_rd_ctx_t read_ctx;
   struct loaded *next;
};

struct run_queue {
   struct event **queue;
   size_t       wr, rd;
   size_t       alloc;
};

static struct rt_proc   *procs = NULL;
static struct rt_proc   *active_proc = NULL;
static struct sens_list *resume = NULL;
static struct loaded    *loaded = NULL;
static struct run_queue run_queue;

static heap_t        eventq_heap = NULL;
static size_t        n_procs = 0;
static uint64_t      now = 0;
static int           iteration = -1;
static bool          trace_on = false;
static ident_t       i_signal = NULL;
static tree_rd_ctx_t tree_rd_ctx = NULL;
static struct rusage ready_rusage;
static jmp_buf       fatal_jmp;
static bool          aborted = false;

static rt_alloc_stack_t event_stack = NULL;
static rt_alloc_stack_t waveform_stack = NULL;
static rt_alloc_stack_t sens_list_stack = NULL;
static rt_alloc_stack_t tmp_chunk_stack = NULL;

#define MAX_ACTIVE_SIGS 128
static struct signal *active_signals[MAX_ACTIVE_SIGS];
static unsigned      n_active_signals = 0;

static void deltaq_insert_proc(uint64_t delta, struct rt_proc *wake);
static void deltaq_insert_driver(uint64_t delta, struct signal *signal,
                                 int length, struct rt_proc *driver);
static void rt_alloc_driver(struct signal *sig, uint64_t after,
                            uint64_t reject, uint64_t value);
static void *rt_tmp_alloc(size_t sz);
static tree_t rt_recall_tree(const char *unit, int32_t where);
static void _tracef(const char *fmt, ...);

#define TRACE(...) if (unlikely(trace_on)) _tracef(__VA_ARGS__)

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

////////////////////////////////////////////////////////////////////////////////
// Utilities

static int array_size(type_t type)
{
   if (type_is_array(type)) {
      int64_t low, high;
      range_bounds(type_dim(type, 0), &low, &high);
      return (high - low + 1) * array_size(type_elem(type));
   }
   else
      return 1;
}

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
   static const int BUF_SZ = 64;
   return fmt_time_r(get_fmt_buf(BUF_SZ), BUF_SZ, t);
}

static const char *fmt_sig(struct signal *sig)
{
   static const int BUF_SZ = 256;
   char *buf = get_fmt_buf(BUF_SZ);
   char *p = buf;
   const char *end = buf + BUF_SZ;
   p += snprintf(p, end - p, "%s", istr(tree_ident(sig->decl)));

   struct signal *first = tree_attr_ptr(sig->decl, i_signal);
   ptrdiff_t offset = sig - first;

   type_t type = tree_type(sig->decl);
   while (type_is_array(type)) {
      int stride = array_size(type_elem(type));
      p += snprintf(p, end - p, "[%zd]", offset / stride);
      offset %= stride;

      type = type_elem(type);
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
   deltaq_insert_proc(delay, active_proc);
}

void _sched_waveform_vec(void *_sig, void *values, int32_t n, int32_t size,
                         int64_t after, int64_t reject, int32_t reverse)
{
   struct signal *sig = _sig;

   TRACE("_sched_waveform_vec %p values=%p n=%d size=%d after=%s "
         "reject=%s reverse=%d", sig, values, n, size, fmt_time(after),
         fmt_time(reject), reverse);

   const int v_start = reverse ? (n - 1) : 0;
   const int v_inc   = reverse ? -1 : 1;

#define SCHED_WAVEFORM_VEC(type) do {                           \
      const type *vp  = (type *)values + v_start;               \
      for (int i = 0; i < n; i++, vp += v_inc)                  \
         rt_alloc_driver(&sig[i], after, reject, *vp);          \
   } while (0)

   FOR_ALL_SIZES(size, SCHED_WAVEFORM_VEC);

   deltaq_insert_driver(after, sig, n, active_proc);
}

void _sched_waveform(void *_sig, int64_t value, int64_t after, int64_t reject)
{
   struct signal *sig = _sig;

   TRACE("_sched_waveform %s value=%"PRIx64" after=%s reject=%s",
         fmt_sig(sig), value, fmt_time(after), fmt_time(reject));

   rt_alloc_driver(sig, after, reject, value);

   deltaq_insert_driver(after, sig, 1, active_proc);
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
         node->wakeup_gen = active_proc->wakeup_gen;
         node->next       = sig[i].sensitive;

         sig[i].sensitive = node;
      }
      else {
         // Reuse the stale entry
         it->wakeup_gen = active_proc->wakeup_gen;
      }
   }
}

void _set_initial_vec(void *_sig, void *values, int32_t n, int32_t size)
{
   struct signal *sig = _sig;

   TRACE("_set_initial_vec %p values=%p n=%d size=%d", sig, values, n, size);

#define SET_INITIAL_VEC(type) do {                      \
      const type *vp = values;                          \
      for (int i = 0; i < n; i++)                       \
         sig[i].resolved = sig[i].last_value = vp[i];   \
   } while (0)

   FOR_ALL_SIZES(size, SET_INITIAL_VEC);
}

void _set_initial(void *_sig, int64_t value)
{
   struct signal *sig = _sig;

   TRACE("_set_initial %s value=%"PRIx64, fmt_sig(sig), value);

   sig->resolved = sig->last_value = value;
}

void _assert_fail(const uint8_t *msg, int32_t msg_len, int8_t severity,
                  int32_t where, const char *module)
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

   tree_t t = rt_recall_tree(module, where);
   const loc_t *loc = tree_loc(t);
   bool is_report = tree_attr_int(t, ident_new("is_report"), 0);

   char *copy = NULL;
   if (msg_len >= 0) {
      copy = xmalloc(msg_len + 1);
      memcpy(copy, msg, msg_len);
      copy[msg_len] = '\0';
   }

   void (*fn)(const loc_t *loc, const char *fmt, ...);

   switch (severity) {
   case 0: fn = note_at; break;
   case 1: fn = warn_at; break;
   case 2:
   case 3: fn = error_at; break;
   default:
      assert(false);
   }

   if (severity >= EXIT_SEVERITY)
      fn = fatal_at;

   (*fn)(loc, "%s+%d: %s %s: %s",
         fmt_time(now), iteration,
         (is_report ? "Report" : "Assertion"),
         levels[severity],
         (copy != NULL ? copy : (const char *)msg));

   if (copy != NULL)
      free(copy);
}

void _bounds_fail(int32_t where, const char *module, int32_t value,
                  int32_t min, int32_t max, int32_t kind)
{
   tree_t t = rt_recall_tree(module, where);
   const loc_t *loc = tree_loc(t);

   switch ((bounds_kind_t)kind) {
   case BOUNDS_ARRAY_TO:
      fatal_at(loc, "array index %d outside bounds %d to %d",
               value, min, max);
      break;
   case BOUNDS_ARRAY_DOWNTO:
      fatal_at(loc, "array index %d outside bounds %d downto %d",
               value, max, min);
      break;

   case BOUNDS_ENUM:
      fatal_at(loc, "value %d outside %s bounds %d to %d",
               value, type_pp(tree_type(t)), min, max);
      break;

   case BOUNDS_TYPE_TO:
      fatal_at(loc, "value %d outside bounds %d to %d", value, min, max);
      break;

   case BOUNDS_TYPE_DOWNTO:
      fatal_at(loc, "value %d outside bounds %d downto %d", value, max, min);
      break;
   }
}

uint64_t _std_standard_now(void)
{
   return now;
}

char *_tmp_alloc(int32_t n, int32_t sz)
{
   TRACE("_tmp_alloc %dx%d bytes", n, sz);
   return rt_tmp_alloc(n * sz);
}

void _array_reverse(void *restrict dst, const void *restrict src,
                    int32_t off, int32_t n, int32_t sz)
{
   //TRACE("_array_reverse dst=%p src=%p off=%d n=%d sz=%d",
   //      dst, src, off, n, sz);

#define ARRAY_REVERSE(type) do {                \
      const type *restrict sp = src;            \
      type *restrict dp = dst;                  \
      for (int i = n - 1; i >= 0; i--)          \
         *(dp + off + i) = *sp++;               \
   } while (0)

   FOR_ALL_SIZES(sz, ARRAY_REVERSE);
}

void _vec_load(void *_sig, void *where, int32_t size, int32_t low,
               int32_t high, int32_t last)
{
   struct signal *sig = _sig;

   //TRACE("_vec_load %s where=%p size=%d low=%d high=%d last=%d",
   //      fmt_sig(sig), where, size, low, high, last);

#define VEC_LOAD(type) do {                                          \
      type *p = where;                                               \
      if (unlikely(last))                                            \
         for (int i = low; i <= high; i++)                           \
            *p++ = sig[i].last_value;                                \
      else                                                           \
         for (int i = low; i <= high; i++)                           \
            *p++ = sig[i].resolved;                                  \
   } while (0)

   FOR_ALL_SIZES(size, VEC_LOAD);
}

void _image(int64_t val, int32_t where, const char *module, struct uarray *u)
{
   tree_t t = rt_recall_tree(module, where);

   type_t type = tree_type(t);
   while (type_kind(type) == T_SUBTYPE)
      type = type_base(type);

   const size_t max = 32;
   char *buf = rt_tmp_alloc(max);
   size_t len = 0;

   switch (type_kind(type)) {
   case T_INTEGER:
      len = snprintf(buf, max, "%"PRIi64, val);
      break;

   case T_ENUM:
      len = snprintf(buf, max, "%s",
                     istr(tree_ident(type_enum_literal(type, val))));
      break;

   case T_REAL:
      {
         union {
            double  d;
            int64_t i;
         } u = { .i = val };
         len = snprintf(buf, max, "%lf", u.d);
      }
      break;

   default:
      fatal_at(tree_loc(t), "cannot use 'IMAGE with this type");
   }

   u->ptr = buf;
   u->dims[0].left  = 0;
   u->dims[0].right = len - 1;
   u->dims[0].dir   = RANGE_TO;
}

void _bit_shift(int32_t kind, const uint8_t *data, int32_t len,
                int8_t dir, int32_t shift, struct uarray *u)
{
   if (dir == RANGE_DOWNTO)
      kind = kind ^ 1;

   if (shift < 0) {
      kind  = kind ^ 1;
      shift = -shift;
   }

   uint8_t *buf = rt_tmp_alloc(len);

   for (int i = 0; i < len; i++) {
      switch (kind) {
      case BIT_SHIFT_SLL:
         buf[i] = (i < len - shift) ? data[i + shift] : 0;
         break;
      case BIT_SHIFT_SRL:
         buf[i] = (i >= shift) ? data[i - shift] : 0;
         break;
      case BIT_SHIFT_SLA:
         buf[i] = (i < len - shift) ? data[i + shift] : data[len - 1];
         break;
      case BIT_SHIFT_SRA:
         buf[i] = (i >= shift) ? data[i - shift] : data[0];
         break;
      case BIT_SHIFT_ROL:
         buf[i] = (i < len - shift) ? data[i + shift] : data[(i + shift) % len];
         break;
      case BIT_SHIFT_ROR:
         buf[i] = (i >= shift) ? data[i - shift] : data[(i - shift) % len];
         break;
      }
   }

   u->ptr = buf;
   u->dims[0].left  = 0;
   u->dims[0].right = len - 1;
   u->dims[0].dir   = dir;
}

void _debug_out(int32_t val)
{
   printf("DEBUG: val=%"PRIx32"\n", val);
}

void _debug_dump(const uint8_t *ptr, int32_t len)
{
   printf("---- %p ----\n", ptr);

   for (int i = 0; i < len; i++)
      printf("%02x%c", ptr[i], (i % 8 == 7) ? '\n' : ' ');
   if (len % 8 != 0)
      printf("\n");
}

void _name_attr(void *_sig, int which, struct uarray *u)
{
   struct signal *sig = _sig;
   ident_t inst_name_i = ident_new("INSTANCE_NAME");

   const char *str;
   switch (which) {
   case 0:   // PATH_NAME
      str = istr(tree_ident(sig->decl));
      break;
   case 1:   // INSTANCE_NAME
      str = istr(tree_attr_str(sig->decl, inst_name_i));
      break;
   default:
      assert(false);
   }

   size_t len = strlen(str) + 1;
   char *buf = rt_tmp_alloc(len);
   strncpy(buf, str, len);

   u->ptr = buf;
   u->dims[0].left  = 0;
   u->dims[0].right = len - 1;
   u->dims[0].dir   = RANGE_TO;
}

void _file_open(int8_t *status, void **_fp, uint8_t *name_bytes,
                int32_t name_len, int8_t mode)
{
   FILE **fp = (FILE **)_fp;
   if (*fp != NULL) {
      if (status != NULL) {
         *status = 1;   // STATUS_ERROR
         return;
      }
      else
         // This is to support closing a file implicitly when the
         // design is reset
         fclose(*fp);
   }

   char *fname = xmalloc(name_len + 1);
   memcpy(fname, name_bytes, name_len);
   fname[name_len] = '\0';

   TRACE("_file_open %s fp=%p mode=%d", fname, fp, mode);

   const char *mode_str[] = {
      "r", "w", "w+"
   };
   assert(mode < ARRAY_LEN(mode_str));

   if (status != NULL)
      *status = 0;   // OPEN_OK

   *fp = fopen(fname, mode_str[mode]);

   if (*fp == NULL) {
      if (status == NULL)
         fatal_errno("failed to open %s", fname);
      else {
         switch (errno) {
         case ENOENT:
            *status = 2;   // NAME_ERROR
            break;
         case EPERM:
            *status = 3;   // MODE_ERROR
            break;
         default:
            fatal_errno("%s", fname);
         }
      }
   }

   free(fname);
}

void _file_write(void **_fp, uint8_t *data, int32_t len)
{
   FILE **fp = (FILE **)_fp;

   TRACE("_file_write fp=%p data=%p len=%d", fp, data, len);

   if (*fp == NULL)
      fatal("write to closed file");

   fwrite(data, 1, len, *fp);
}

void _file_read(void **_fp, uint8_t *data, int32_t len, int32_t *out)
{
   FILE **fp = (FILE **)_fp;

   TRACE("_file_read fp=%p data=%p len=%d", fp, data, len);

   if (*fp == NULL)
      fatal("read from closed file");

   size_t n = fread(data, 1, len, *fp);
   if (out != NULL)
      *out = n;
}

void _file_close(void **_fp)
{
   FILE **fp = (FILE **)_fp;

   TRACE("_file_close fp=%p", fp);

   if (*fp == NULL)
      fatal("attempt to close already closed file");

   fclose(*fp);
   *fp = NULL;
}

int8_t _endfile(void *_f)
{
   FILE *f = _f;

   if (f == NULL)
      fatal("ENDFILE called on closed file");

   int c = fgetc(f);
   if (c == EOF)
      return 1;
   else {
      ungetc(c, f);
      return 0;
   }
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

static void deltaq_insert_proc(uint64_t delta, struct rt_proc *wake)
{
   struct event *e = rt_alloc(event_stack);
   e->iteration = (delta == 0 ? iteration + 1 : 0);
   e->when      = now + delta;
   e->kind      = E_PROCESS;
   e->proc      = wake;

   heap_insert(eventq_heap, heap_key(e->when, e->kind), e);
}

static void deltaq_insert_driver(uint64_t delta, struct signal *signal,
                                 int length, struct rt_proc *driver)
{
   struct event *e = rt_alloc(event_stack);
   e->iteration = (delta == 0 ? iteration + 1 : 0);
   e->when      = now + delta;
   e->kind      = E_DRIVER;
   e->signal    = signal;
   e->proc      = driver;
   e->length    = length;

   heap_insert(eventq_heap, heap_key(e->when, e->kind), e);
}

#if TRACE_DELTAQ > 0
static void deltaq_walk(uint64_t key, void *user, void *context)
{
   struct event *e = user;

   fprintf(stderr, "%s\t", fmt_time(e->when));
   if (e->kind == E_DRIVER) {
      fprintf(stderr, "driver\t %s", fmt_sig(e->signal));
      if (e->length > 1)
         fprintf(stderr, "+%d", e->length - 1);
      fprintf(stderr, "\n");
   }
   else
      fprintf(stderr, "process\t %s\n", istr(tree_ident(e->proc->source)));
}

static void deltaq_dump(void)
{
   heap_walk(eventq_heap, deltaq_walk, NULL);
}
#endif

static void *rt_tmp_alloc(size_t sz)
{
   // Allocate sz bytes that will be freed when the process suspends

   if (unlikely(active_proc == NULL)) {
      // It is possible for the module reset functions to call _tmp_alloc
      // without an active process
      // XXX: this can leak memory when result is not used for a global
      return xmalloc(sz);
   }

   struct tmp_chunk *c = active_proc->tmp_chunks;

   bool reuse =
      (c != NULL)
      && (TMP_BUF_SZ - c->hdr.alloced >= sz)
      && (c->hdr.external == NULL);

   if (likely(reuse)) {
      char *ptr = c->buf + c->hdr.alloced;
      c->hdr.alloced += sz;
      return ptr;
   }
   else {
      c = rt_alloc(tmp_chunk_stack);
      c->hdr.alloced  = sz;
      c->hdr.external = NULL;
      c->hdr.next     = active_proc->tmp_chunks;

      active_proc->tmp_chunks = c;

      if (likely(sz <= TMP_BUF_SZ))
         return c->buf;
      else
         return (c->hdr.external = xmalloc(sz));
   }
}

static void rt_reset_signal(struct signal *s, tree_t decl, int offset)
{
   if (s->drivers != NULL) {
      for (int i = 0; i < s->n_drivers; i++) {
         struct waveform *w, *wnext;
         w = s->drivers[i].waveforms;
         do {
            wnext = w->next;
            rt_free(waveform_stack, w);
         } while ((w = wnext) != NULL);
      }

      free(s->drivers);
   }

   s->decl      = decl;
   s->sensitive = NULL;
   s->drivers   = NULL;
   s->n_drivers = 0;
   s->event_cb  = NULL;
   s->offset    = offset;
}

static void rt_setup(tree_t top)
{
   now = 0;
   iteration = -1;

   assert(resume == NULL);

   if (eventq_heap != NULL)
      heap_free(eventq_heap);
   eventq_heap = heap_new(512);

   if (procs == NULL) {
      n_procs = tree_stmts(top);
      procs   = xmalloc(sizeof(struct rt_proc) * n_procs);
   }

   for (unsigned i = 0; i < tree_decls(top); i++) {
      tree_t d = tree_decl(top, i);
      if (tree_kind(d) != T_SIGNAL_DECL)
         continue;

      struct signal *s = jit_var_ptr(istr(tree_ident(d)), true);
      tree_add_attr_ptr(d, i_signal, s);

      type_t type = tree_type(d);
      if (type_is_array(type)) {
         int total = array_size(type);
         for (unsigned j = 0; j < total; j++) {
            rt_reset_signal(&s[j], d, j);
            TRACE("signal %s at %p", fmt_sig(&s[j]), &s[j]);
         }
      }
      else {
         rt_reset_signal(s, d, 0);
         TRACE("signal %s at %p", fmt_sig(s), s);
      }
   }

   for (unsigned i = 0; i < tree_stmts(top); i++) {
      tree_t p = tree_stmt(top, i);
      assert(tree_kind(p) == T_PROCESS);

      procs[i].source     = p;
      procs[i].proc_fn    = jit_fun_ptr(istr(tree_ident(p)), true);
      procs[i].wakeup_gen = 0;
      procs[i].tmp_chunks = NULL;

      TRACE("process %s at %p", istr(tree_ident(p)), procs[i].proc_fn);
   }
}

static void rt_run(struct rt_proc *proc, bool reset)
{
   TRACE("%s process %s", reset ? "reset" : "run",
         istr(tree_ident(proc->source)));

   active_proc = proc;
   (*proc->proc_fn)(reset ? 1 : 0);

   // Free any temporary memory allocated by the process
   while (proc->tmp_chunks) {
      struct tmp_chunk *n = proc->tmp_chunks;
      proc->tmp_chunks = n->hdr.next;
      if (n->hdr.external != NULL)
         free(n->hdr.external);
      rt_free(tmp_chunk_stack, n);
   }
}

static void rt_call_module_reset(ident_t name)
{
   char buf[128];
   snprintf(buf, sizeof(buf), "%s_reset", istr(name));

   void (*reset_fn)(void) = jit_fun_ptr(buf, false);
   if (reset_fn != NULL)
      (*reset_fn)();
}

static void rt_initial(tree_t top)
{
   // Initialisation is described in LRM 93 section 12.6.4

   const int ncontext = tree_contexts(top);
   for (int i = 0; i < ncontext; i++) {
      context_t c = tree_context(top, i);
      rt_call_module_reset(c.name);

      ident_t body = ident_prefix(c.name, ident_new("body"), '-');
      rt_call_module_reset(body);
   }

   rt_call_module_reset(tree_ident(top));

   for (size_t i = 0; i < n_procs; i++)
      rt_run(&procs[i], true /* reset */);
}

static bool rt_wakeup(struct sens_list *sl)
{
   // To avoid having each process keep a list of the signals it is
   // sensitive to, each process has a "wakeup generation" number which
   // is incremented after each wait statement and stored in the signal
   // sensitivity list. We then ignore any sensitivity list elements
   // where the generation doesn't match the current process wakeup
   // generation: these correspond to stale "wait on" statements that
   // have already resumed.

   const char *pstr = NULL;
   if (unlikely(trace_on))
      pstr = istr(tree_ident(sl->proc->source));

   if (sl->wakeup_gen == sl->proc->wakeup_gen) {
      TRACE("wakeup process %s", pstr);
      ++(sl->proc->wakeup_gen);
      sl->next = resume;
      resume = sl;
      return true;
   }
   else {
      rt_free(sens_list_stack, sl);
      return true;
   }
}

static void rt_alloc_driver(struct signal *sig, uint64_t after,
                            uint64_t reject, uint64_t value)
{
   if (unlikely(reject > after))
      fatal("signal %s pulse reject limit %s is greater than "
            "delay %s", fmt_sig(sig), fmt_time(reject), fmt_time(after));

   // Try to find this process is the list of existing drivers
   int driver;
   for (driver = 0; driver < sig->n_drivers; driver++) {
      if (likely(sig->drivers[driver].proc == active_proc))
         break;
   }

   // Allocate memory for drivers on demand
   if (unlikely(driver == sig->n_drivers)) {
      const size_t driver_sz = sizeof(struct driver);
      sig->drivers = xrealloc(sig->drivers, (driver + 1) * driver_sz);
      memset(&sig->drivers[sig->n_drivers], '\0',
             (driver + 1 - sig->n_drivers) * driver_sz);
      sig->n_drivers = driver + 1;
   }

   struct driver *d = &(sig->drivers[driver]);

   if (unlikely(d->waveforms == NULL)) {
      // Assigning the initial value of a driver
      struct waveform *dummy = rt_alloc(waveform_stack);
      dummy->when  = 0;
      dummy->next  = NULL;
      dummy->value = value;

      d->waveforms = dummy;
      d->proc      = active_proc;
   }

   struct waveform *w = rt_alloc(waveform_stack);
   w->when  = now + after;
   w->next  = NULL;
   w->value = value;

   struct waveform *last = d->waveforms;
   struct waveform *it   = last->next;
   while ((it != NULL) && (it->when < w->when)) {
      // If the current transaction is within the pulse rejection interval
      // and the value is different to that of the new transaction then
      // delete the current transaction
      if ((it->when >= w->when - reject) && (it->value != w->value)) {
         struct waveform *next = it->next;
         last->next = next;
         rt_free(waveform_stack, it);
         it = next;
      }
      else {
         last = it;
         it = it->next;
      }
   }
   w->next = NULL;
   last->next = w;

   // Delete all transactions later than this
   // We could remove this transaction from the deltaq as well but the
   // overhead of doing so is probably higher than the cost of waking
   // up for the empty event
   while (it != NULL) {
      rt_free(waveform_stack, it);
      it = it->next;
   }
}

static void rt_update_signal(struct signal *s, int driver, uint64_t value)
{
   TRACE("update signal %s value=%"PRIx64" driver=%d",
         fmt_sig(s), value, driver);

   uint64_t resolved;
   if (unlikely(s->n_drivers > 1)) {
      // If there is more than one driver call the resolution function

      if (unlikely(s->resolution == NULL))
         fatal_at(tree_loc(s->decl), "signal %s has multiple drivers but "
                  "no resolution function", istr(tree_ident(s->decl)));

      uint64_t vals[s->n_drivers];
      for (int i = 0; i < s->n_drivers; i++)
         vals[i] = s->drivers[i].waveforms->value;
      vals[driver] = value;

      resolved = (*s->resolution)(vals, s->n_drivers);
   }
   else
      resolved = value;

   int32_t new_flags = SIGNAL_F_ACTIVE;
   if (s->resolved != resolved)
      new_flags |= SIGNAL_F_EVENT;

   assert(n_active_signals < MAX_ACTIVE_SIGS);
   active_signals[n_active_signals++] = s;

   // Set the update flag on the first element of the vector which
   // will cause the event callback to be executed at the end of
   // the cycle
   struct signal *base = s - s->offset;
   base->flags |= SIGNAL_F_UPDATE;

   // LAST_VALUE is the same as the initial value when
   // there have been no events on the signal otherwise
   // only update it when there is an event
   if (new_flags & SIGNAL_F_EVENT) {
      s->last_value = s->resolved;
      s->last_event = now;
   }

   s->resolved  = resolved;
   s->flags    |= new_flags;

   // Wake up any processes sensitive to this signal
   if (new_flags & SIGNAL_F_EVENT) {
      struct sens_list *it, *next, *save = NULL;
      for (it = s->sensitive; it != NULL; it = next) {
         next = it->next;
         if (unlikely(!rt_wakeup(it))) {
            it->next = save;
            save = it;
         }
      }
      s->sensitive = save;
   }
}

static void rt_update_driver(struct signal *s, struct rt_proc *proc)
{
   // Find the driver owned by proc
   int driver;
   for (driver = 0; driver < s->n_drivers; driver++) {
      if (likely(s->drivers[driver].proc == proc))
         break;
   }
   assert(driver != s->n_drivers);

   struct waveform *w_now  = s->drivers[driver].waveforms;
   struct waveform *w_next = w_now->next;

   if (w_next != NULL && w_next->when == now) {
      rt_update_signal(s, driver, w_next->value);
      s->drivers[driver].waveforms = w_next;
      rt_free(waveform_stack, w_now);
   }
   else
      assert(w_now != NULL);
}

static void rt_push_run_queue(struct event *e)
{
   if (unlikely(run_queue.wr == run_queue.alloc)) {
      if (run_queue.alloc == 0) {
         run_queue.alloc = 128;
         run_queue.queue = xmalloc(sizeof(struct event *) * run_queue.alloc);
      }
      else {
         run_queue.alloc *= 2;
         run_queue.queue = realloc(run_queue.queue,
                                   sizeof(struct event *) * run_queue.alloc);
      }
   }

   run_queue.queue[(run_queue.wr)++] = e;
}

static struct event *rt_pop_run_queue(void)
{
   if (run_queue.wr == run_queue.rd) {
      run_queue.wr = 0;
      run_queue.rd = 0;
      return NULL;
   }
   else
      return run_queue.queue[(run_queue.rd)++];
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
      rt_push_run_queue(heap_extract_min(eventq_heap));

      if (heap_size(eventq_heap) == 0)
         break;

      peek = heap_min(eventq_heap);
      if (peek->when > now || peek->iteration != iteration)
         break;
   }

   struct event *event;
   while ((event = rt_pop_run_queue())) {
      switch (event->kind) {
      case E_PROCESS:
         rt_run(event->proc, false /* reset */);
         break;
      case E_DRIVER:
         for (int i = 0; i < event->length; i++)
            rt_update_driver(&event->signal[i], event->proc);
         break;
      }

      rt_free(event_stack, event);
   }

   if (unlikely(now == 0 && iteration == 0))
      vcd_restart();

   // Run all processes that resumed because of signal events
   while (resume != NULL) {
      struct sens_list *next = resume->next;
      rt_run(resume->proc, false /* reset */);
      rt_free(sens_list_stack, resume);
      resume = next;
   }

   for (unsigned i = 0; i < n_active_signals; i++) {
      struct signal *s = active_signals[i];
      struct signal *base = s - s->offset;
      s->flags &= ~(SIGNAL_F_ACTIVE | SIGNAL_F_EVENT);
      if (unlikely((base->event_cb != NULL)
                   && (base->flags & SIGNAL_F_UPDATE))) {
         (*base->event_cb)(now, s->decl);
         base->flags &= ~SIGNAL_F_UPDATE;
      }
   }
   n_active_signals = 0;
}

static void rt_load_unit(const char *name)
{
   char *tmp = strdup(name);
   const char *lib_name  = strtok(tmp, ".");

   lib_t lib = lib_find(lib_name, true, true);
   if (lib == NULL)
      fatal("cannot continue");

   tree_rd_ctx_t ctx = NULL;
   if (lib_get_ctx(lib, ident_new(name), &ctx) == NULL)
      fatal("cannot find unit %s", name);

   struct loaded *l = xmalloc(sizeof(struct loaded));
   l->next     = NULL;
   l->name     = name;
   l->read_ctx = ctx;

   if (loaded == NULL)
      loaded = l;
   else {
      struct loaded *it;
      for (it = loaded; it->next != NULL; it = it->next)
         ;
      it->next = l;
   }

   free(tmp);
}

static tree_t rt_recall_tree(const char *unit, int32_t where)
{
   struct loaded *it;
   for (it = loaded; it != NULL; it = it->next) {
      if (it->name == unit)
         return tree_read_recall(it->read_ctx, where);
   }

   rt_load_unit(unit);
   return rt_recall_tree(unit, where);
}

static void rt_one_time_init(void)
{
   i_signal = ident_new("signal");

   jit_bind_fn("_std_standard_now", _std_standard_now);
   jit_bind_fn("_sched_process", _sched_process);
   jit_bind_fn("_sched_waveform_vec", _sched_waveform_vec);
   jit_bind_fn("_sched_waveform", _sched_waveform);
   jit_bind_fn("_sched_event", _sched_event);
   jit_bind_fn("_assert_fail", _assert_fail);
   jit_bind_fn("_tmp_alloc", _tmp_alloc);
   jit_bind_fn("_array_reverse", _array_reverse);
   jit_bind_fn("_vec_load", _vec_load);
   jit_bind_fn("_image", _image);
   jit_bind_fn("_debug_out", _debug_out);
   jit_bind_fn("_name_attr", _name_attr);
   jit_bind_fn("_set_initial", _set_initial);
   jit_bind_fn("_set_initial_vec", _set_initial_vec);
   jit_bind_fn("_file_open", _file_open);
   jit_bind_fn("_file_close", _file_close);
   jit_bind_fn("_file_write", _file_write);
   jit_bind_fn("_file_read", _file_read);
   jit_bind_fn("_endfile", _endfile);
   jit_bind_fn("_bounds_fail", _bounds_fail);
   jit_bind_fn("_bit_shift", _bit_shift);

   trace_on = opt_get_int("rt_trace_en");

   event_stack     = rt_alloc_stack_new(sizeof(struct event));
   waveform_stack  = rt_alloc_stack_new(sizeof(struct waveform));
   sens_list_stack = rt_alloc_stack_new(sizeof(struct sens_list));
   tmp_chunk_stack = rt_alloc_stack_new(sizeof(struct tmp_chunk));
}

static void rt_cleanup_signal(struct signal *sig)
{
   for (int j = 0; j < sig->n_drivers; j++) {
      while (sig->drivers[j].waveforms != NULL) {
         struct waveform *next = sig->drivers[j].waveforms->next;
         rt_free(waveform_stack, sig->drivers[j].waveforms);
         sig->drivers[j].waveforms = next;
      }
   }
   free(sig->drivers);

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
      if (type_is_array(type)) {
         int size = array_size(type);
         for (unsigned i = 0; i < size; i++)
            rt_cleanup_signal(&sig[i]);
      }
      else
         rt_cleanup_signal(sig);
   }

   rt_alloc_stack_destroy(event_stack);
   rt_alloc_stack_destroy(waveform_stack);
   rt_alloc_stack_destroy(sens_list_stack);
}

static bool rt_stop_now(uint64_t stop_time)
{
   struct event *peek = heap_min(eventq_heap);
   return peek->when > stop_time;
}

static void rt_stats_ready(void)
{
   if (getrusage(RUSAGE_SELF, &ready_rusage) < 0)
      fatal_errno("getrusage");
}

static unsigned rt_tv2ms(struct timeval *tv)
{
   return (tv->tv_sec * 1000) + (tv->tv_usec / 1000);
}

static void rt_stats_print(void)
{
   struct rusage final_rusage;
   if (getrusage(RUSAGE_SELF, &final_rusage) < 0)
      fatal_errno("getrusage");

   unsigned ready_u = rt_tv2ms(&ready_rusage.ru_utime);
   unsigned ready_s = rt_tv2ms(&ready_rusage.ru_stime);

   unsigned final_u = rt_tv2ms(&final_rusage.ru_utime);
   unsigned final_s = rt_tv2ms(&final_rusage.ru_stime);

   notef("setup:%ums run:%ums maxrss:%ldkB",
         ready_u + ready_s,
         final_u + final_s - ready_u - ready_s,
         final_rusage.ru_maxrss);
}

void rt_batch_exec(tree_t e, uint64_t stop_time, tree_rd_ctx_t ctx)
{
   tree_rd_ctx = ctx;

   jit_init(tree_ident(e));

   rt_one_time_init();
   rt_setup(e);
   rt_stats_ready();
   rt_initial(e);
   while (heap_size(eventq_heap) > 0 && !rt_stop_now(stop_time))
      rt_cycle();
   rt_cleanup(e);

   jit_shutdown();

   if (opt_get_int("rt-stats"))
      rt_stats_print();
}

static void rt_slave_fatal(void)
{
   aborted = true;
   longjmp(fatal_jmp, 1);
}

static void rt_slave_run(slave_run_msg_t *msg)
{
   if (aborted)
      errorf("simulation has aborted and must be restarted");
   else if (heap_size(eventq_heap) == 0)
      warnf("no future simulation events");
   else {
      set_fatal_fn(rt_slave_fatal);

      if (setjmp(fatal_jmp) == 0) {
         const uint64_t end = now + msg->time;
         while (heap_size(eventq_heap) > 0 && !rt_stop_now(end))
            rt_cycle();
      }

      set_fatal_fn(NULL);
   }

   slave_post_msg(EVENT_STOP, NULL, 0);
}

static void rt_slave_read_signal(slave_read_signal_msg_t *msg)
{
   tree_t t = tree_read_recall(tree_rd_ctx, msg->index);
   assert(tree_kind(t) == T_SIGNAL_DECL);

   type_t type = tree_type(t);
   size_t len = 1;
   while (type_is_array(type)) {
      int64_t low = 0, high = 0;
      range_bounds(type_dim(type, 0), &low, &high);
      len *= (high - low + 1);

      type = type_elem(type);
   }
   assert(len <= msg->len);

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

static void rt_slave_now(void)
{
   reply_now_msg_t reply = {
      .now = now
   };
   fmt_time_r(reply.text, sizeof(reply.text), now);
   slave_post_msg(REPLY_NOW, &reply, sizeof(reply));
}

static void rt_slave_watch_cb(uint64_t now, tree_t decl)
{
   struct signal *base = tree_attr_ptr(decl, i_signal);

   event_watch_msg_t event = {
      .index = tree_index(decl),
      .now   = now,
      .value = base[0].resolved,
      .last  = base[0].last_value
   };
   fmt_time_r(event.now_text, sizeof(event.now_text), now);
   slave_post_msg(EVENT_WATCH, &event, sizeof(event));
}

static void rt_slave_watch(slave_watch_msg_t *msg)
{
   tree_t t = tree_read_recall(tree_rd_ctx, msg->index);
   assert(tree_kind(t) == T_SIGNAL_DECL);

   rt_set_event_cb(t, rt_slave_watch_cb);
}

static void rt_slave_unwatch(slave_unwatch_msg_t *msg)
{
   tree_t t = tree_read_recall(tree_rd_ctx, msg->index);
   assert(tree_kind(t) == T_SIGNAL_DECL);

   rt_set_event_cb(t, NULL);
}

void rt_slave_exec(tree_t e, tree_rd_ctx_t ctx)
{
   tree_rd_ctx = ctx;

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
         rt_initial(e);
         aborted = false;
         break;

      case SLAVE_RUN:
         rt_slave_run((slave_run_msg_t *)buf);
         break;

      case SLAVE_READ_SIGNAL:
         rt_slave_read_signal((slave_read_signal_msg_t *)buf);
         break;

      case SLAVE_NOW:
         rt_slave_now();
         break;

      case SLAVE_WATCH:
         rt_slave_watch((slave_watch_msg_t *)buf);
         break;

      case SLAVE_UNWATCH:
         rt_slave_unwatch((slave_unwatch_msg_t *)buf);
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
