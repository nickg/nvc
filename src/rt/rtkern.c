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
#include "netdb.h"
#include "cover.h"

#include <assert.h>
#include <stdint.h>
#include <stdarg.h>
#include <inttypes.h>
#include <stdlib.h>
#include <string.h>
#include <setjmp.h>
#include <math.h>
#include <errno.h>
#include <alloca.h>
#include <sys/time.h>
#include <sys/resource.h>

#define TRACE_DELTAQ  1
#define TRACE_PENDING 0
#define EXIT_SEVERITY 2

typedef void (*proc_fn_t)(int32_t reset);
typedef uint64_t (*resolution_fn_t)(uint64_t *vals, int32_t n);

typedef struct netgroup   netgroup_t;
typedef struct driver     driver_t;
typedef struct rt_proc    rt_proc_t;
typedef struct event      event_t;
typedef struct waveform   waveform_t;
typedef struct sens_list  sens_list_t;
typedef struct value      value_t;
typedef struct watch_list watch_list_t;

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
   tree_t            source;
   proc_fn_t         proc_fn;
   struct tmp_chunk *tmp_chunks;
   uint32_t          wakeup_gen;
};

typedef enum { E_DRIVER, E_PROCESS } event_kind_t;

struct event {
   uint64_t      when;
   int           iteration;
   event_kind_t  kind;
   rt_proc_t    *proc;
   netgroup_t   *group;
};

struct waveform {
   uint64_t    when;
   waveform_t *next;
   value_t    *values;
};

struct sens_list {
   rt_proc_t   *proc;
   sens_list_t *next;
   uint32_t     wakeup_gen;
   netid_t      first;
   netid_t      last;
};

struct driver {
   rt_proc_t  *proc;
   waveform_t *waveforms;
};

struct value {
   value_t *next;
   char     data[0];
};

struct netgroup {
   netid_t         first;
   uint32_t        length;
   net_flags_t     flags;
   value_t        *resolved;
   value_t        *last_value;
   uint16_t        size;
   uint16_t        n_drivers;
   driver_t       *drivers;
   resolution_fn_t resolution;
   uint64_t        last_event;
   tree_t          sig_decl;
   value_t        *free_values;
   sens_list_t    *pending;
   watch_list_t   *watching;
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
   event_t **queue;
   size_t    wr, rd;
   size_t    alloc;
};

struct watch {
   tree_t         signal;
   sig_event_fn_t fn;
   bool           pending;
   watch_t       *chain_all;
   watch_t       *chain_pending;
   netgroup_t   **groups;
   int            n_groups;
   void          *user_data;
   range_kind_t   dir;
};

struct watch_list {
   watch_t      *watch;
   watch_list_t *next;
};

static struct rt_proc   *procs = NULL;
static struct rt_proc   *active_proc = NULL;
static struct loaded    *loaded = NULL;
static struct run_queue  run_queue;

static heap_t        eventq_heap = NULL;
static size_t        n_procs = 0;
static uint64_t      now = 0;
static int           iteration = -1;
static bool          trace_on = false;
static tree_rd_ctx_t tree_rd_ctx = NULL;
static struct rusage ready_rusage;
static jmp_buf       fatal_jmp;
static bool          aborted = false;
static netdb_t      *netdb = NULL;
static netgroup_t   *groups = NULL;
static sens_list_t  *pending = NULL;
static sens_list_t  *resume = NULL;
static watch_t      *watches = NULL;
static watch_t      *callbacks = NULL;

static rt_alloc_stack_t event_stack = NULL;
static rt_alloc_stack_t waveform_stack = NULL;
static rt_alloc_stack_t sens_list_stack = NULL;
static rt_alloc_stack_t tmp_chunk_stack = NULL;
static rt_alloc_stack_t watch_stack = NULL;

static netgroup_t **active_groups;
static unsigned     n_active_groups = 0;
static unsigned     n_active_alloc = 0;

static void deltaq_insert_proc(uint64_t delta, rt_proc_t *wake);
static void deltaq_insert_driver(uint64_t delta, netgroup_t *group,
                                 rt_proc_t *driver);
static void rt_alloc_driver(netgroup_t *group, uint64_t after,
                            uint64_t reject, value_t *values);
static void rt_sched_event(sens_list_t **list, netid_t first, netid_t last,
                           rt_proc_t *proc);
static void *rt_tmp_alloc(size_t sz);
static value_t *rt_alloc_value(netgroup_t *g);
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

static const char *fmt_group(const netgroup_t *g)
{
   static const int BUF_SZ = 256;
   char *buf = get_fmt_buf(BUF_SZ);
   char *p = buf;
   const char *end = buf + BUF_SZ;
   p += snprintf(p, end - p, "%s", istr(tree_ident(g->sig_decl)));

   groupid_t sig_group0 = netdb_lookup(netdb, tree_net(g->sig_decl, 0));
   netid_t sig_net0 = groups[sig_group0].first;
   int offset = g->first - sig_net0;

   type_t type = tree_type(g->sig_decl);
   while (type_is_array(type)) {
      const int stride = array_size(type_elem(type));
      const int index = offset / stride;
      p += snprintf(p, end - p, "[%d", index);
      if (g->length > 1)
         p += snprintf(p, end - p, "..%d", index + g->length - 1);
      p += snprintf(p, end -p, "]");
      offset %= stride;

      type = type_elem(type);
   }

   return buf;
}

static const char *fmt_net(netid_t nid)
{
   return fmt_group(&(groups[netdb_lookup(netdb, nid)]));
}

static const char *fmt_values(const void *values, int length)
{
   const size_t sz = (length * 2) + 1;
   char *buf = get_fmt_buf(sz);
   static_printf_begin(buf, sz);

   for (int i = 0; i < length; i++)
      static_printf(buf, "%02x", ((uint8_t *)values)[i]);

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

void _sched_waveform(void *_nids, void *values, int32_t n, int32_t size,
                     int64_t after, int64_t reject, int32_t reverse)
{
   const int32_t *nids = _nids;

   TRACE("_sched_waveform %s values=%s n=%d size=%d after=%s "
         "reject=%s reverse=%d", fmt_net(nids[0]),
         fmt_values(values, n * size), n, size, fmt_time(after),
         fmt_time(reject), reverse);

   int offset = 0;
   while (offset < n) {
      netgroup_t *g = &(groups[netdb_lookup(netdb, nids[offset])]);

      value_t *values_copy = rt_alloc_value(g);

      if (unlikely(reverse)) {
#define COPY_VALUES(type) do {                                  \
            const type *vp = (type *)values + (n - offset - 1); \
            type *vc = (type *)values_copy->data;               \
            for (int i = 0; i < g->length; i++, vp--)           \
               vc[i] = *vp;                                     \
         } while (0)

         FOR_ALL_SIZES(size, COPY_VALUES);
      }
      else
         memcpy(values_copy->data, (uint8_t *)values + (offset * size),
                size * g->length);

      rt_alloc_driver(g, after, reject, values_copy);
      deltaq_insert_driver(after, g, active_proc);

      offset += g->length;
   }
}

void _sched_event(void *_nids, int32_t n, int32_t seq)
{
   const int32_t *nids = _nids;

   TRACE("_sched_event %s n=%d seq=%d proc %s", fmt_net(nids[0]), n,
         seq, istr(tree_ident(active_proc->source)));

   if (n == 1) {
      netgroup_t *g = &(groups[netdb_lookup(netdb, nids[0])]);
      rt_sched_event(&(g->pending), NETID_INVALID, NETID_INVALID, active_proc);
   }
   else {
      int offset = 0;
      while (offset < n) {
         netgroup_t *g = &(groups[netdb_lookup(netdb, nids[offset])]);
         offset += g->length;

         if (seq && (offset < n)) {
            // Place on the global pending list
            rt_sched_event(&pending, nids[0], nids[n - 1], active_proc);
            break;
         }
         else {
            // Place on the net group's pending list
            rt_sched_event(&(g->pending), NETID_INVALID,
                           NETID_INVALID, active_proc);
         }
      }
   }
}

void _set_initial(int32_t nid, void *values, int32_t n, int32_t size,
                  void *resolution, int32_t index, const char *module)
{
   //TRACE("_set_initial net=%d values=%s n=%d size=%d index=%d",
   //      nid, fmt_values(values, n * size), n, size, index);

   tree_t decl = rt_recall_tree(module, index);
   assert(tree_kind(decl) == T_SIGNAL_DECL);

   int offset = 0;
   while (offset < n) {
      groupid_t gid = netdb_lookup(netdb, nid + offset);
      netgroup_t *g = &(groups[gid]);

      g->sig_decl   = decl;
      g->resolution = resolution;
      g->size       = size;
      g->resolved   = rt_alloc_value(g);
      g->last_value = rt_alloc_value(g);

      const void *src = (uint8_t *)values + (offset * size);
      memcpy(g->resolved->data, src, g->length * size);
      memcpy(g->last_value->data, src, g->length * size);

      offset += g->length;
   }
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

   case BOUNDS_ARRAY_SIZE:
      fatal_at(loc, "length of target %d does not match length of value %d",
               min, max);
      break;

   case BOUNDS_INDEX_TO:
      fatal_at(loc, "index %d violates constraint bounds %d to %d",
               value, min, max);
      break;

   case BOUNDS_INDEX_DOWNTO:
      fatal_at(loc, "index %d violates constraint bounds %d downto %d",
               value, max, min);
      break;
   }
}

void _div_zero(int32_t where, const char *module)
{
   tree_t t = rt_recall_tree(module, where);
   fatal_at(tree_loc(t), "division by zero");
}

void _null_deref(int32_t where, const char *module)
{
   tree_t t = rt_recall_tree(module, where);
   fatal_at(tree_loc(t), "null access dereference");
}

int64_t _std_standard_now(void)
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

void *_vec_load(const int32_t *nids, void *where, int32_t size, int32_t low,
                int32_t high, int32_t last)
{
   //TRACE("_vec_load %s where=%p size=%d low=%d high=%d last=%d",
   //      fmt_net(nids[0]), where, size, low, high, last);

   int offset = low;
   while (offset <= high) {
      groupid_t gid = netdb_lookup(netdb, nids[offset]);
      netgroup_t *g = &(groups[gid]);

      const int skip = nids[offset] - g->first;
      const int to_copy = MIN(high - offset + 1, g->length - skip);

      if ((offset == low) && (offset + g->length - skip > high)) {
         // If the signal data is already contiguous return a pointer to
         // that rather than copying into the user buffer
         void *r = unlikely(last) ? g->last_value->data : g->resolved->data;
         return (uint8_t *)r + (skip * size);
      }

      void *p = (uint8_t *)where + ((offset - low) * size);
      if (unlikely(last))
         memcpy(p, (uint8_t *)g->last_value->data + (skip * size),
                to_copy * size);
      else
         memcpy(p, (uint8_t *)g->resolved->data + (skip * size),
                to_copy * size);

      offset += g->length - skip;
   }

   // Signal data was non-contiguous so return the user buffer
   return where;
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

   case T_PHYSICAL:
      {
         tree_t unit = type_unit(type, 0);
         len = snprintf(buf, max, "%"PRIi64" %s", val, istr(tree_ident(unit)));
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

   shift %= len;

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
         buf[i] = (i >= shift) ? data[i - shift] : data[len + i - shift];
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

int64_t _last_event(const int32_t *nids, int32_t n)
{
   //TRACE("_last_event %s n=%d", fmt_net(&(nets[nids[0]])), n);

   int64_t last = INT64_MAX;
   int offset = 0;
   while (offset < n) {
      netgroup_t *g = &(groups[netdb_lookup(netdb, nids[offset])]);
      if (g->last_event < now)
         last = MIN(last, now - g->last_event);

      offset += g->length;
   }

   return last;
}

int32_t _test_net_flag(const int32_t *nids, int32_t n, int32_t flag)
{
   //TRACE("_test_net_flag %s n=%d flag=%d", fmt_net(&(nets[nids[0]])),
   //      n, flag);

   int offset = 0;
   while (offset < n) {
      netgroup_t *g = &(groups[netdb_lookup(netdb, nids[offset])]);

      if (g->flags & flag)
         return 1;

      offset += g->length;
   }

   return 0;
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

   if (strcmp(fname, "STD_INPUT") == 0)
      *fp = stdin;
   else if (strcmp(fname, "STD_OUTPUT") == 0)
      *fp = stdout;
   else
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

static void deltaq_insert_driver(uint64_t delta, netgroup_t *group,
                                 rt_proc_t *driver)
{
   struct event *e = rt_alloc(event_stack);
   e->iteration = (delta == 0 ? iteration + 1 : 0);
   e->when      = now + delta;
   e->kind      = E_DRIVER;
   e->group     = group;
   e->proc      = driver;

   heap_insert(eventq_heap, heap_key(e->when, e->kind), e);
}

#if TRACE_DELTAQ > 0
static void deltaq_walk(uint64_t key, void *user, void *context)
{
   struct event *e = user;

   fprintf(stderr, "%s\t", fmt_time(e->when));
   if (e->kind == E_DRIVER)
      fprintf(stderr, "driver\t %s\n", fmt_group(e->group));
   else
      fprintf(stderr, "process\t %s\n", istr(tree_ident(e->proc->source)));
}

static void deltaq_dump(void)
{
   heap_walk(eventq_heap, deltaq_walk, NULL);
}
#endif

static value_t *rt_alloc_value(netgroup_t *g)
{
   if (g->free_values == NULL) {
      value_t *v = xmalloc(sizeof(struct value) + (g->size * g->length));
      v->next = NULL;
      return v;
   }
   else {
      value_t *v = g->free_values;
      g->free_values = v->next;
      v->next = NULL;
      return v;
   }
}

static void rt_free_value(netgroup_t *g, value_t *v)
{
   assert(v->next == NULL);
   v->next = g->free_values;
   g->free_values = v;
}

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

static void rt_sched_event(sens_list_t **list, netid_t first, netid_t last,
                           rt_proc_t *proc)
{
   // See if there is already a stale entry in the pending
   // list for this process
   sens_list_t *it = *list;
   int count = 0;
   for (; it != NULL; it = it->next, ++count) {
      if ((it->proc == proc)
          && (it->wakeup_gen != proc->wakeup_gen))
         break;
   }

   if (it == NULL) {
      sens_list_t *node = rt_alloc(sens_list_stack);
      node->proc       = proc;
      node->wakeup_gen = proc->wakeup_gen;
      node->next       = *list;
      node->first      = first;
      node->last       = last;

      *list = node;
   }
   else {
      // Reuse the stale entry
      it->wakeup_gen = proc->wakeup_gen;
      it->first      = first;
      it->last       = last;
   }
}

#if TRACE_PENDING
static void rt_dump_pending(void)
{
   for (struct sens_list *it = pending; it != NULL; it = it->next) {
      printf("%d..%d\t%s%s\n", it->first, it->last,
             istr(tree_ident(it->proc->source)),
             (it->wakeup_gen == it->proc->wakeup_gen) ? "" : "(stale)");
   }
}
#endif  // TRACE_PENDING

static void rt_reset_group(groupid_t gid, netid_t first, unsigned length)
{
   netgroup_t *g = &(groups[gid]);
   g->first       = first;
   g->length      = length;
   g->resolved    = NULL;
   g->last_value  = NULL;
   g->size        = 0;
   g->flags       = 0;
   g->n_drivers   = 0;
   g->drivers     = NULL;
   g->resolution  = NULL;
   g->last_event  = INT64_MAX;
   g->sig_decl    = NULL;
   g->free_values = NULL;
   g->pending     = NULL;
   g->watching    = NULL;
}

static void rt_setup(tree_t top)
{
   now = 0;
   iteration = -1;

   assert(resume == NULL);

   if (eventq_heap != NULL)
      heap_free(eventq_heap);
   eventq_heap = heap_new(512);

   if (netdb == NULL) {
      netdb = netdb_open(top);
      groups = xmalloc(sizeof(struct netgroup) * netdb_size(netdb));
   }

   if (procs == NULL) {
      n_procs = tree_stmts(top);
      procs   = xmalloc(sizeof(struct rt_proc) * n_procs);
   }

   netdb_walk(netdb, rt_reset_group);

   const int nstmts = tree_stmts(top);
   for (int i = 0; i < nstmts; i++) {
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
      tree_t c = tree_context(top, i);
      ident_t unit_name = tree_ident(c);
      rt_call_module_reset(unit_name);

      ident_t body = ident_prefix(unit_name, ident_new("body"), '-');
      rt_call_module_reset(body);
   }

   rt_call_module_reset(tree_ident(top));

   for (size_t i = 0; i < n_procs; i++)
      rt_run(&procs[i], true /* reset */);
}

static void rt_watch_signal(watch_t *w)
{
   const int nnets = tree_nets(w->signal);
   int offset = 0;
   while (offset < nnets) {
      netid_t nid = tree_net(w->signal, offset);
      netgroup_t *g = &(groups[netdb_lookup(netdb, nid)]);

      watch_list_t *link = xmalloc(sizeof(watch_list_t));
      link->next  = g->watching;
      link->watch = w;

      g->watching = link;

      offset += g->length;
      (w->n_groups)++;
   }

   w->groups = xmalloc(sizeof(netgroup_t *) * w->n_groups);

   int ptr = 0;
   offset = 0;
   while (offset < nnets) {
      netid_t nid = tree_net(w->signal, offset);
      netgroup_t *g = &(groups[netdb_lookup(netdb, nid)]);
      w->groups[ptr++] = g;
      offset += g->length;
   }
}

static void rt_wakeup(sens_list_t *sl)
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
      ++(sl->proc->wakeup_gen);

      sl->next = resume;
      resume = sl;
   }
   else
      rt_free(sens_list_stack, sl);
}

static void rt_alloc_driver(netgroup_t *group, uint64_t after,
                            uint64_t reject, value_t *values)
{
   if (unlikely(reject > after))
      fatal("signal %s pulse reject limit %s is greater than "
            "delay %s", fmt_group(group), fmt_time(reject), fmt_time(after));

   // Try to find this process in the list of existing drivers
   int driver;
   for (driver = 0; driver < group->n_drivers; driver++) {
      if (likely(group->drivers[driver].proc == active_proc))
         break;
   }

   // Allocate memory for drivers on demand
   if (unlikely(driver == group->n_drivers)) {
      const size_t driver_sz = sizeof(struct driver);
      group->drivers = xrealloc(group->drivers, (driver + 1) * driver_sz);
      memset(&group->drivers[group->n_drivers], '\0',
             (driver + 1 - group->n_drivers) * driver_sz);
      group->n_drivers = driver + 1;

      TRACE("allocate driver %s %d %s", fmt_group(group), driver,
            istr(tree_ident(active_proc->source)));
   }

   driver_t *d = &(group->drivers[driver]);

   const size_t valuesz = group->size * group->length;

   if (unlikely(d->waveforms == NULL)) {
      // Assigning the initial value of a driver
      waveform_t *dummy = rt_alloc(waveform_stack);
      dummy->when   = 0;
      dummy->next   = NULL;
      dummy->values = rt_alloc_value(group);
      memcpy(dummy->values->data, values, valuesz);

      d->waveforms = dummy;
      d->proc      = active_proc;
   }

   waveform_t *w = rt_alloc(waveform_stack);
   w->when   = now + after;
   w->next   = NULL;
   w->values = values;

   waveform_t *last = d->waveforms;
   waveform_t *it   = last->next;
   while ((it != NULL) && (it->when < w->when)) {
      // If the current transaction is within the pulse rejection interval
      // and the value is different to that of the new transaction then
      // delete the current transaction
      if ((it->when >= w->when - reject)
          && (memcmp(it->values->data, w->values->data, valuesz) != 0)) {
         waveform_t *next = it->next;
         last->next = next;
         rt_free_value(group, it->values);
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
      rt_free_value(group, it->values);
      rt_free(waveform_stack, it);
      it = it->next;
   }
}

static void rt_update_group(netgroup_t *group, int driver, void *values)
{
   const size_t valuesz = group->size * group->length;

   TRACE("update group %s values=%s driver=%d",
         fmt_group(group), fmt_values(values, valuesz), driver);

   void *resolved = values;
   if (unlikely(group->n_drivers > 1)) {
      // If there is more than one driver call the resolution function

      if (unlikely(group->resolution == NULL))
         fatal_at(tree_loc(group->sig_decl), "group %s has multiple drivers "
                  "but no resolution function", fmt_group(group));

      resolved = alloca(valuesz);

      for (int j = 0; j < group->length; j++) {
         uint64_t vals[group->n_drivers];

#define CALL_RESOLUTION_FN(type) do {                                   \
            for (int i = 0; i < group->n_drivers; i++) {                \
               const value_t *v = group->drivers[i].waveforms->values;  \
               vals[i] = ((const type *)v->data)[j];                    \
            }                                                           \
            vals[driver] = ((const type *)values)[j];                   \
            type *r = (type *)resolved;                                 \
            r[j] = (*group->resolution)(vals, group->n_drivers);        \
         } while (0)

         FOR_ALL_SIZES(group->size, CALL_RESOLUTION_FN);
      }
   }

   int32_t new_flags = NET_F_ACTIVE;
   if (memcmp(group->resolved->data, resolved, valuesz) != 0)
      new_flags |= NET_F_EVENT;

   if (unlikely(n_active_groups == n_active_alloc)) {
      n_active_alloc *= 2;
      const size_t newsz = n_active_alloc * sizeof(struct net *);
      active_groups = xrealloc(active_groups, newsz);
   }
   active_groups[n_active_groups++] = group;

   // LAST_VALUE is the same as the initial value when
   // there have been no events on the signal otherwise
   // only update it when there is an event
   if (new_flags & NET_F_EVENT) {
      // Swap last with current value to avoid a memcpy
      value_t *tmp = group->last_value;
      group->last_value = group->resolved;
      group->resolved = tmp;

      group->last_event = now;
   }

   memcpy(group->resolved->data, resolved, valuesz);
   group->flags |= new_flags;

   // Wake up any processes sensitive to this group
   if (new_flags & NET_F_EVENT) {
      sens_list_t *it, *last = NULL, *next = NULL;

      // First wakeup everything on the group specific pending list
      for (it = group->pending; it != NULL; it = next) {
         next = it->next;
         rt_wakeup(it);
         group->pending = next;
      }

      // Now check the global pending list
      for (it = pending; it != NULL; it = next) {
         next = it->next;

         const netid_t x = group->first;
         const netid_t y = group->first + group->length - 1;
         const netid_t a = it->first;
         const netid_t b = it->last;

         const bool hit = (x <= b) && (a <= y);

         if (hit) {
            rt_wakeup(it);
            if (last == NULL)
               pending = next;
            else
               last->next = next;
         }
         else
            last = it;
      }

      // Schedule any callbacks to run
      for (watch_list_t *wl = group->watching; wl != NULL; wl = wl->next) {
         if (!wl->watch->pending) {
            wl->watch->chain_pending = callbacks;
            wl->watch->pending = true;
            callbacks = wl->watch;
         }
      }
   }
}

static void rt_update_driver(netgroup_t *group, rt_proc_t *proc)
{
   // Find the driver owned by proc
   int driver;
   for (driver = 0; driver < group->n_drivers; driver++) {
      if (likely(group->drivers[driver].proc == proc))
         break;
   }
   assert(driver != group->n_drivers);

   waveform_t *w_now  = group->drivers[driver].waveforms;
   waveform_t *w_next = w_now->next;

   if (w_next != NULL && w_next->when == now) {
      rt_update_group(group, driver, w_next->values->data);
      group->drivers[driver].waveforms = w_next;
      rt_free_value(group, w_now->values);
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

static void rt_iteration_limit(void)
{
   char buf[2048];
   static_printf_begin(buf, sizeof(buf));

   static_printf(buf, "Iteration limit of %d delta cycles reached. "
                 "The following processes are active:\n",
                 opt_get_int("stop-delta"));

   for (sens_list_t *it = resume; it != NULL; it = it->next) {
      tree_t p = it->proc->source;
      const loc_t *l = tree_loc(p);
      static_printf(buf, "  %-30s %s line %d\n", istr(tree_ident(p)),
                    l->file, l->first_line);
   }

   static_printf(buf, "You can increase this limit with --stop-delta");

   fatal("%s", buf);
}

static void rt_process_callbacks(void)
{
   while (callbacks != NULL) {
      (*callbacks->fn)(now, callbacks->signal, callbacks, callbacks->user_data);
      callbacks->pending = false;

      watch_t *next = callbacks->chain_pending;
      callbacks->chain_pending = NULL;
      callbacks = next;
   }
}

static void rt_cycle(int stop_delta)
{
   // Simulation cycle is described in LRM 93 section 12.6.4

   event_t *peek = heap_min(eventq_heap);

   if (peek->when > now) {
      rt_process_callbacks();
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
#if TRACE_PENDING > 0
   if (trace_on)
      rt_dump_pending();
#endif

   for (;;) {
      rt_push_run_queue(heap_extract_min(eventq_heap));

      if (heap_size(eventq_heap) == 0)
         break;

      peek = heap_min(eventq_heap);
      if (peek->when > now || peek->iteration != iteration)
         break;
   }

   event_t *event;
   while ((event = rt_pop_run_queue())) {
      switch (event->kind) {
      case E_PROCESS:
         rt_run(event->proc, false /* reset */);
         break;
      case E_DRIVER:
         rt_update_driver(event->group, event->proc);
         break;
      }

      rt_free(event_stack, event);
   }

   if (unlikely(now == 0 && iteration == 0)) {
      vcd_restart();
      lxt_restart();
      fst_restart();
   }
   else if (unlikely((stop_delta > 0) && (iteration == stop_delta)))
      rt_iteration_limit();

   // Run all processes that resumed because of signal events
   while (resume != NULL) {
      rt_run(resume->proc, false /* reset */);

      sens_list_t *next = resume->next;
      rt_free(sens_list_stack, resume);
      resume = next;
   }

   for (unsigned i = 0; i < n_active_groups; i++) {
      netgroup_t *g = active_groups[i];
      g->flags &= ~(NET_F_ACTIVE | NET_F_EVENT);
   }
   n_active_groups = 0;
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
   jit_bind_fn("_std_standard_now", _std_standard_now);
   jit_bind_fn("_sched_process", _sched_process);
   jit_bind_fn("_sched_waveform", _sched_waveform);
   jit_bind_fn("_sched_event", _sched_event);
   jit_bind_fn("_assert_fail", _assert_fail);
   jit_bind_fn("_tmp_alloc", _tmp_alloc);
   jit_bind_fn("_array_reverse", _array_reverse);
   jit_bind_fn("_vec_load", _vec_load);
   jit_bind_fn("_image", _image);
   jit_bind_fn("_debug_out", _debug_out);
   jit_bind_fn("_set_initial", _set_initial);
   jit_bind_fn("_file_open", _file_open);
   jit_bind_fn("_file_close", _file_close);
   jit_bind_fn("_file_write", _file_write);
   jit_bind_fn("_file_read", _file_read);
   jit_bind_fn("_endfile", _endfile);
   jit_bind_fn("_bounds_fail", _bounds_fail);
   jit_bind_fn("_bit_shift", _bit_shift);
   jit_bind_fn("_test_net_flag", _test_net_flag);
   jit_bind_fn("_last_event", _last_event);
   jit_bind_fn("_div_zero", _div_zero);
   jit_bind_fn("_null_deref", _null_deref);

   trace_on = opt_get_int("rt_trace_en");

   event_stack     = rt_alloc_stack_new(sizeof(event_t));
   waveform_stack  = rt_alloc_stack_new(sizeof(waveform_t));
   sens_list_stack = rt_alloc_stack_new(sizeof(sens_list_t));
   tmp_chunk_stack = rt_alloc_stack_new(sizeof(struct tmp_chunk));
   watch_stack     = rt_alloc_stack_new(sizeof(watch_t));

   n_active_alloc = 128;
   active_groups = xmalloc(n_active_alloc * sizeof(struct net *));
}

static void rt_cleanup_group(groupid_t gid, netid_t first, unsigned length)
{
   netgroup_t *g = &(groups[gid]);

   assert(g->first == first);
   assert(g->length == length);

   free(g->resolved);
   free(g->last_value);

   for (int j = 0; j < g->n_drivers; j++) {
      while (g->drivers[j].waveforms != NULL) {
         waveform_t *next = g->drivers[j].waveforms->next;
         rt_free_value(g, g->drivers[j].waveforms->values);
         rt_free(waveform_stack, g->drivers[j].waveforms);
         g->drivers[j].waveforms = next;
      }
   }
   free(g->drivers);

   while (g->free_values != NULL) {
      value_t *next = g->free_values->next;
      free(g->free_values);
      g->free_values = next;
   }

   while (g->pending != NULL) {
      sens_list_t *next = g->pending->next;
      rt_free(sens_list_stack, g->pending);
      g->pending = next;
   }

   while (g->watching != NULL) {
      watch_list_t *next = g->watching->next;
      free(g->watching);
      g->watching = next;
   }
}

static void rt_cleanup(tree_t top)
{
   assert(resume == NULL);

   while (heap_size(eventq_heap) > 0)
      rt_free(event_stack, heap_extract_min(eventq_heap));

   heap_free(eventq_heap);
   eventq_heap = NULL;

   netdb_walk(netdb, rt_cleanup_group);
   netdb_close(netdb);

   while (watches != NULL) {
      watch_t *next = watches->chain_all;
      rt_free(watch_stack, watches);
      free(watches->groups);
      watches = next;
   }

   while (pending != NULL) {
      sens_list_t *next = pending->next;
      rt_free(sens_list_stack, pending);
      pending = next;
   }

   rt_alloc_stack_destroy(event_stack);
   rt_alloc_stack_destroy(waveform_stack);
   rt_alloc_stack_destroy(sens_list_stack);
   rt_alloc_stack_destroy(watch_stack);
}

static bool rt_stop_now(uint64_t stop_time)
{
   event_t *peek = heap_min(eventq_heap);
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

#ifdef __APPLE__
   const int rss_units = 1024;
#else
   const int rss_units = 1;
#endif

   notef("setup:%ums run:%ums maxrss:%ldkB",
         ready_u + ready_s,
         final_u + final_s - ready_u - ready_s,
         final_rusage.ru_maxrss / rss_units);
}

static void rt_emit_coverage(tree_t e)
{
   const int32_t *cover_stmts = jit_var_ptr("cover_stmts", false);
   if (cover_stmts != NULL)
      cover_report(e, cover_stmts);
}

void rt_batch_exec(tree_t e, uint64_t stop_time, tree_rd_ctx_t ctx)
{
   tree_rd_ctx = ctx;

   jit_init(tree_ident(e));

   const int stop_delta = opt_get_int("stop-delta");

   rt_one_time_init();
   rt_setup(e);
   rt_stats_ready();
   rt_initial(e);
   while (heap_size(eventq_heap) > 0 && !rt_stop_now(stop_time))
      rt_cycle(stop_delta);
   rt_process_callbacks();
   rt_cleanup(e);
   rt_emit_coverage(e);

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

      const int stop_delta = opt_get_int("stop-delta");

      if (setjmp(fatal_jmp) == 0) {
         const uint64_t end = now + msg->time;
         while (heap_size(eventq_heap) > 0 && !rt_stop_now(end))
            rt_cycle(stop_delta);
         rt_process_callbacks();
      }

      set_fatal_fn(NULL);
   }

   slave_post_msg(EVENT_STOP, NULL, 0);
}

static void rt_slave_read_signal(slave_read_signal_msg_t *msg)
{
   tree_t t = tree_read_recall(tree_rd_ctx, msg->index);
   assert(tree_kind(t) == T_SIGNAL_DECL);

   const size_t rsz =
      sizeof(reply_read_signal_msg_t) + (msg->len * sizeof(uint64_t));
   reply_read_signal_msg_t *reply = xmalloc(rsz);

   const int nnets = tree_nets(t);
   int offset = 0;
   for (int i = 0; (i < nnets) && (offset < msg->len); i++) {
      netid_t nid = tree_net(t, offset);
      netgroup_t *g = &(groups[netdb_lookup(netdb, nid)]);

#define SIGNAL_READ_EXPAND_U64(type) do {                                 \
         const type *sp = (type *)g->resolved->data;                      \
         for (int i = 0; (i < g->length) && (offset + i < msg->len); i++) \
            reply->values[offset + i] = sp[i];                            \
      } while (0)

      FOR_ALL_SIZES(g->size, SIGNAL_READ_EXPAND_U64);

      offset += g->length;
   }

   reply->len = offset;

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

static void rt_slave_watch_cb(uint64_t now, tree_t decl, watch_t *w, void *user)
{
   uint64_t value[1];
   rt_signal_value(w, value, 1, false);

   uint64_t last[1];
   rt_signal_value(w, last, 1, true);

   event_watch_msg_t event = {
      .index = tree_index(decl),
      .now   = now,
      .value = value[0],
      .last  = last[0]
   };
   fmt_time_r(event.now_text, sizeof(event.now_text), now);
   slave_post_msg(EVENT_WATCH, &event, sizeof(event));
}

static void rt_slave_watch(slave_watch_msg_t *msg)
{
   tree_t t = tree_read_recall(tree_rd_ctx, msg->index);
   assert(tree_kind(t) == T_SIGNAL_DECL);

   rt_set_event_cb(t, rt_slave_watch_cb, NULL);
}

static void rt_slave_unwatch(slave_unwatch_msg_t *msg)
{
   tree_t t = tree_read_recall(tree_rd_ctx, msg->index);
   assert(tree_kind(t) == T_SIGNAL_DECL);

   rt_set_event_cb(t, NULL, NULL);
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

watch_t *rt_set_event_cb(tree_t s, sig_event_fn_t fn, void *user)
{
   assert(tree_kind(s) == T_SIGNAL_DECL);

   if (fn == NULL) {
      // Find the first entry in the watch list and disable it
      for (watch_t *it = watches; it != NULL; it = it->chain_all) {
         if (it->signal == s) {
            it->pending = true;   // TODO: not a good way of doing this
            break;
         }
      }

      return NULL;
   }
   else {
      watch_t *w = rt_alloc(watch_stack);
      assert(w != NULL);
      w->signal        = s;
      w->fn            = fn;
      w->chain_all     = watches;
      w->chain_pending = NULL;
      w->pending       = false;
      w->groups        = NULL;
      w->n_groups      = 0;
      w->user_data     = user;

      type_t type = tree_type(s);
      if (type_is_array(type))
         w->dir = type_dim(type, 0).kind;
      else
         w->dir = RANGE_TO;

      watches = w;

      rt_watch_signal(w);
      return w;
   }
}

size_t rt_signal_value(watch_t *w, uint64_t *buf, size_t max, bool last)
{
   int offset = 0;
   for (int i = 0; (i < w->n_groups) && (offset < max); i++) {
      netgroup_t *g = w->groups[i];

#define SIGNAL_VALUE_EXPAND_U64(type) do {                              \
         const value_t *v = (last ? g->last_value : g->resolved);       \
         const type *sp = (type *)v->data;                              \
         for (int i = 0; (i < g->length) && (offset + i < max); i++)    \
            buf[offset + i] = sp[i];                                    \
      } while (0)

      FOR_ALL_SIZES(g->size, SIGNAL_VALUE_EXPAND_U64);

      offset += g->length;
   }

   return offset;
}

size_t rt_string_value(watch_t *w, const char *map, char *buf, size_t max)
{
   size_t offset = 0;
   for (int i = 0; (i < w->n_groups) && (offset < max - 1); i++) {
      netgroup_t *g = w->groups[i];
      const char *vals = g->resolved->data;
      const bool to = (w->dir == RANGE_TO);

      if (likely(map != NULL)) {
         for (int i = 0; (i < g->length) && (offset + i < max - 1); i++) {
            const int ptr = to ? i : g->length - i - 1;
            buf[offset + i] = map[(int)vals[ptr]];
         }
      }
      else {
         for (int i = 0; (i < g->length) && (offset + i < max - 1); i++) {
            const int ptr = to ? i : g->length - i - 1;
            buf[offset + i] = vals[ptr];
         }
      }

      offset += g->length;
   }

   buf[offset++] = '\0';
   return offset;
}

uint64_t rt_now(void)
{
   return now;
}
