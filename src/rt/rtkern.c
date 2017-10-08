//
//  Copyright (C) 2011-2017  Nick Gasson
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
#include "alloc.h"
#include "heap.h"
#include "common.h"
#include "netdb.h"
#include "cover.h"
#include "hash.h"

#include <assert.h>
#include <stdint.h>
#include <stdarg.h>
#include <inttypes.h>
#include <stdlib.h>
#include <string.h>
#include <setjmp.h>
#include <math.h>
#include <errno.h>
#include <signal.h>
#include <sys/time.h>
#include <float.h>
#include <ctype.h>

#ifdef HAVE_ALLOCA_H
#include <alloca.h>
#endif

#ifdef __MINGW32__
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#undef SEVERITY_ERROR
#endif

#define TRACE_DELTAQ  1
#define TRACE_PENDING 0

typedef void (*proc_fn_t)(int32_t reset);
typedef uint64_t (*resolution_fn_t)(void *vals, int32_t n);

typedef struct netgroup   netgroup_t;
typedef struct driver     driver_t;
typedef struct rt_proc    rt_proc_t;
typedef struct event      event_t;
typedef struct waveform   waveform_t;
typedef struct sens_list  sens_list_t;
typedef struct value      value_t;
typedef struct watch_list watch_list_t;
typedef struct res_memo   res_memo_t;
typedef struct callback   callback_t;
typedef struct image_map  image_map_t;
typedef struct rt_loc     rt_loc_t;

struct rt_proc {
   tree_t    source;
   proc_fn_t proc_fn;
   uint32_t  wakeup_gen;
   void     *tmp_stack;
   uint32_t  tmp_alloc;
   bool      postponed;
   bool      pending;
};

typedef enum {
   E_TIMEOUT,
   E_DRIVER,
   E_PROCESS
} event_kind_t;

struct event {
   uint64_t      when;
   event_kind_t  kind;
   uint32_t      wakeup_gen;
   event_t      *delta_chain;
   rt_proc_t    *proc;
   netgroup_t   *group;
   timeout_fn_t  timeout_fn;
   void         *timeout_user;
};

struct waveform {
   uint64_t    when;
   waveform_t *next;
   value_t    *values;
};

struct sens_list {
   rt_proc_t    *proc;
   sens_list_t  *next;
   sens_list_t **reenq;
   uint32_t      wakeup_gen;
   netid_t       first;
   netid_t       last;
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
   netid_t       first;
   uint32_t      length;
   net_flags_t   flags;
   void         *resolved;
   void         *last_value;
   value_t      *forcing;
   uint16_t      size;
   uint16_t      n_drivers;
   driver_t     *drivers;
   res_memo_t   *resolution;
   uint64_t      last_event;
   tree_t        sig_decl;
   value_t      *free_values;
   sens_list_t  *pending;
   watch_list_t *watching;
};

struct uarray {
   void    *ptr;
   struct {
      int32_t left;
      int32_t right;
      int8_t  dir;
   } dims[1];
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
   size_t         length;
   bool           postponed;
};

struct watch_list {
   watch_t      *watch;
   watch_list_t *next;
};

typedef enum {
   R_MEMO  = (1 << 0),
   R_IDENT = (1 << 1),
} res_flags_t;

struct res_memo {
   resolution_fn_t fn;
   res_flags_t     flags;
   int8_t          tab2[16][16];
   int8_t          tab1[16];
};

typedef enum {
   SIDE_EFFECT_ALLOW,
   SIDE_EFFECT_DISALLOW,
   SIDE_EFFECT_OCCURRED
} side_effect_t;

struct callback {
   rt_event_fn_t  fn;
   void          *user;
   callback_t    *next;
};

struct image_map {
   int32_t        kind;
   int32_t        stride;
   const char    *elems;
   const int64_t *values;
   int32_t        count;
};

struct rt_loc {
   int32_t     first_line;
   int32_t     last_line;
   int16_t     first_column;
   int16_t     last_column;
   const char *file;
};

static struct rt_proc   *procs = NULL;
static struct rt_proc   *active_proc = NULL;
static struct run_queue  run_queue;

static heap_t        eventq_heap = NULL;
static size_t        n_procs = 0;
static uint64_t      now = 0;
static int           iteration = -1;
static bool          trace_on = false;
static nvc_rusage_t  ready_rusage;
static jmp_buf       fatal_jmp;
static bool          aborted = false;
static netdb_t      *netdb = NULL;
static netgroup_t   *groups = NULL;
static sens_list_t  *pending = NULL;
static sens_list_t  *resume = NULL;
static sens_list_t  *postponed = NULL;
static watch_t      *watches = NULL;
static watch_t      *callbacks = NULL;
static event_t      *delta_proc = NULL;
static event_t      *delta_driver = NULL;
static void         *global_tmp_stack = NULL;
static void         *proc_tmp_stack = NULL;
static uint32_t      global_tmp_alloc;
static hash_t       *res_memo_hash = NULL;
static side_effect_t init_side_effect = SIDE_EFFECT_ALLOW;
static bool          force_stop;
static bool          can_create_delta;
static callback_t   *global_cbs[RT_LAST_EVENT];
static rt_severity_t exit_severity = SEVERITY_ERROR;
static hash_t       *decl_hash = NULL;

static rt_alloc_stack_t event_stack = NULL;
static rt_alloc_stack_t waveform_stack = NULL;
static rt_alloc_stack_t sens_list_stack = NULL;
static rt_alloc_stack_t watch_stack = NULL;
static rt_alloc_stack_t callback_stack = NULL;

static netgroup_t **active_groups;
static unsigned     n_active_groups = 0;
static unsigned     n_active_alloc = 0;

static void deltaq_insert_proc(uint64_t delta, rt_proc_t *wake);
static void deltaq_insert_driver(uint64_t delta, netgroup_t *group,
                                 rt_proc_t *driver);
static bool rt_sched_driver(netgroup_t *group, uint64_t after,
                            uint64_t reject, value_t *values);
static void rt_sched_event(sens_list_t **list, netid_t first, netid_t last,
                           rt_proc_t *proc, bool is_static);
static void *rt_tmp_alloc(size_t sz);
static value_t *rt_alloc_value(netgroup_t *g);
static tree_t rt_recall_decl(const char *name);
static res_memo_t *rt_memo_resolution_fn(type_t type, resolution_fn_t fn);
static void _tracef(const char *fmt, ...);

#define GLOBAL_TMP_STACK_SZ (1024 * 1024)
#define PROC_TMP_STACK_SZ   (64 * 1024)

#define TRACE(...) do {                                 \
      if (unlikely(trace_on)) _tracef(__VA_ARGS__);     \
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

////////////////////////////////////////////////////////////////////////////////
// Utilities

static const char *fmt_group(const netgroup_t *g)
{
   static const size_t BUF_LEN = 512;
   char *buf = get_fmt_buf(BUF_LEN);

   const char *eptr = buf + BUF_LEN;
   char *p = buf;

   p += checked_sprintf(p, eptr - p, "%s", istr(tree_ident(g->sig_decl)));

   groupid_t sig_group0 = netdb_lookup(netdb, tree_net(g->sig_decl, 0));
   netid_t sig_net0 = groups[sig_group0].first;
   int offset = g->first - sig_net0;

   const int length = g->length;
   type_t type = tree_type(g->sig_decl);
   while (type_is_array(type)) {
      const int stride = type_width(type_elem(type));
      const int ndims = type_dims(type);

      p += checked_sprintf(p, eptr - p, "[");
      for (int i = 0; i < ndims; i++) {
         int stride2 = stride;
         for (int j = i + 1; j < ndims; j++) {
            range_t r = type_dim(type, j);

            int64_t low, high;
            range_bounds(r, &low, &high);

            stride2 *= (high - low) + 1;
         }

         const int index = offset / stride2;
         p += checked_sprintf(p, eptr - p, "%s%d", (i > 0) ? "," : "", index);
         if ((length / stride2) > 1)
            p += checked_sprintf(p, eptr - p, "..%d",
                                 index + (length / stride2) - 1);
         offset %= stride2;
      }
      p += checked_sprintf(p, eptr - p, "]");

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
   const size_t len = (length * 2) + 1;
   char *vbuf = get_fmt_buf(len);

   char *p = vbuf;
   for (int i = 0; i < length; i++)
      p += checked_sprintf(p, vbuf + len - p, "%02x",
                           ((uint8_t *)values)[i]);

   return vbuf;
}

static inline uint64_t heap_key(uint64_t when, event_kind_t kind)
{
   // Use the bottom bit of the key to indicate the kind
   // The highest priority should have the lowest enumeration value
   return (when << 2) | (kind & 3);
}

static void from_rt_loc(const rt_loc_t *rt, loc_t *loc)
{
   // This function can be expensive: only call it when loc_t is required
   loc->file = ident_new(rt->file);
   loc->first_line = rt->first_line;
   loc->last_line = rt->last_line;
   loc->first_column = rt->first_column;
   loc->last_column = rt->last_column;
   loc->linebuf = NULL;
}

static void rt_show_trace(void)
{
   jit_trace_t *trace;
   size_t count;
   jit_trace(&trace, &count);

   for (size_t i = 0; i < count; i++)
      note_at(&(trace[i].loc), "in subprogram %s",
              istr(tree_ident(trace[i].tree)));

   free(trace);
}

////////////////////////////////////////////////////////////////////////////////
// Runtime support functions

DLLEXPORT void     *_tmp_stack;
DLLEXPORT uint32_t  _tmp_alloc;

DLLEXPORT
void _sched_process(int64_t delay)
{
   TRACE("_sched_process delay=%s", fmt_time(delay));
   deltaq_insert_proc(delay, active_proc);
}

DLLEXPORT
void _sched_waveform(void *_nids, void *values, int32_t n,
                     int64_t after, int64_t reject)
{
   const int32_t *nids = _nids;

   TRACE("_sched_waveform %s values=%s n=%d after=%s reject=%s",
         fmt_net(nids[0]),
         fmt_values(values, n * groups[netdb_lookup(netdb, nids[0])].size),
         n, fmt_time(after), fmt_time(reject));

   if (unlikely(active_proc->postponed && (after == 0)))
      fatal("postponed process %s cannot cause a delta cycle",
            istr(tree_ident(active_proc->source)));

   const uint8_t *vp = values;
   int offset = 0;
   while (offset < n) {
      const netid_t nid = nids[offset];
      if (likely(nid != NETID_INVALID)) {
         netgroup_t *g = &(groups[netdb_lookup(netdb, nid)]);

         value_t *values_copy = rt_alloc_value(g);
         memcpy(values_copy->data, vp, g->size * g->length);

         if (!rt_sched_driver(g, after, reject, values_copy))
            deltaq_insert_driver(after, g, active_proc);

         vp += g->size * g->length;
         offset += g->length;
      }
      else
         offset++;
   }

   assert(offset == n);
}

DLLEXPORT
void _sched_event(void *_nids, int32_t n, int32_t flags)
{
   const int32_t *nids = _nids;

   TRACE("_sched_event %s n=%d flags=%d proc %s", fmt_net(nids[0]), n,
         flags, istr(tree_ident(active_proc->source)));

   netgroup_t *g0 = &(groups[netdb_lookup(netdb, nids[0])]);

   if (g0->length == n) {
      rt_sched_event(&(g0->pending), NETID_INVALID, NETID_INVALID,
                     active_proc, flags & SCHED_STATIC);
   }
   else {
      const bool global = !!(flags & SCHED_SEQUENTIAL);
      if (global) {
         // Place on the global pending list
         rt_sched_event(&pending, nids[0], nids[n - 1], active_proc,
                        flags & SCHED_STATIC);
      }

      int offset = 0;
      netgroup_t *g = g0;
      for (;;) {
         if (global)
            g->flags |= NET_F_GLOBAL;
         else {
            // Place on the net group's pending list
            rt_sched_event(&(g->pending), NETID_INVALID, NETID_INVALID,
                           active_proc, flags & SCHED_STATIC);
         }

         offset += g->length;
         if (offset < n)
            g = &(groups[netdb_lookup(netdb, nids[offset])]);
         else
            break;
      }
   }
}

DLLEXPORT
void _alloc_driver(const int32_t *all_nets, int32_t all_length,
                   const int32_t *driven_nets, int32_t driven_length,
                   const void *init)
{
   TRACE("_alloc_driver all=%s+%d driven=%s+%d", fmt_net(all_nets[0]),
         all_length, fmt_net(driven_nets[0]), driven_length);

   const char *initp = init;

   int offset = 0;
   while (offset < driven_length) {
      netgroup_t *g = &(groups[netdb_lookup(netdb, driven_nets[offset])]);
      offset += g->length;

      // Try to find this process in the list of existing drivers
      int driver;
      for (driver = 0; driver < g->n_drivers; driver++) {
         if (likely(g->drivers[driver].proc == active_proc))
            break;
      }

      // Allocate memory for drivers on demand
      if (driver == g->n_drivers) {
         if ((g->n_drivers == 1) && (g->resolution == NULL))
            fatal_at(tree_loc(g->sig_decl), "group %s has multiple drivers "
                     "but no resolution function", fmt_group(g));

         const size_t driver_sz = sizeof(struct driver);
         g->drivers = xrealloc(g->drivers, (driver + 1) * driver_sz);
         memset(&g->drivers[g->n_drivers], '\0',
                (driver + 1 - g->n_drivers) * driver_sz);
         g->n_drivers = driver + 1;

         TRACE("allocate driver %s %d %s", fmt_group(g), driver,
               istr(tree_ident(active_proc->source)));

         driver_t *d = &(g->drivers[driver]);
         d->proc = active_proc;

         const void *src = (init == NULL) ? g->resolved : initp;

         // Assign the initial value of the driver
         waveform_t *dummy = rt_alloc(waveform_stack);
         dummy->when   = 0;
         dummy->next   = NULL;
         dummy->values = rt_alloc_value(g);
         memcpy(dummy->values->data, src, g->length * g->size);

         d->waveforms = dummy;
      }

      initp += g->length * g->size;
   }
}

DLLEXPORT
void _private_stack(void)
{
   TRACE("_private_stack %p %d %d", active_proc->tmp_stack,
         active_proc->tmp_alloc, _tmp_alloc);

   if (active_proc->tmp_stack == NULL && _tmp_alloc > 0) {
      active_proc->tmp_stack = _tmp_stack;

      proc_tmp_stack = mmap_guarded(PROC_TMP_STACK_SZ,
                                    istr(tree_ident(active_proc->source)));
   }

   active_proc->tmp_alloc = _tmp_alloc;
}

DLLEXPORT
void *_resolved_address(int32_t nid)
{
   groupid_t gid = netdb_lookup(netdb, nid);
   netgroup_t *g = &(groups[gid]);
   TRACE("_resolved_address %d %p", nid, g->resolved);
   return g->resolved;
}

DLLEXPORT
void _needs_last_value(const int32_t *nids, int32_t n)
{
   TRACE("_needs_last_value %s n=%d", fmt_net(nids[0]), n);

   int offset = 0;
   while (offset < n) {
      netgroup_t *g = &(groups[netdb_lookup(netdb, nids[offset])]);
      g->flags |= NET_F_LAST_VALUE;

      offset += g->length;
   }
}

DLLEXPORT
void _set_initial(int32_t nid, const uint8_t *values, const int32_t *size_list,
                  int32_t nparts, void *resolution, const char *name)
{
   tree_t decl = rt_recall_decl(name);
   assert(tree_kind(decl) == T_SIGNAL_DECL);

   TRACE("_set_initial %s values=%s nparts=%d", name,
         fmt_values(values, size_list[0] * size_list[1]), nparts);

   res_memo_t *memo = NULL;
   if (resolution != NULL)
      memo = rt_memo_resolution_fn(tree_type(decl), resolution);

   int total_size = 0;
   for (int i = 0; i < nparts; i++)
      total_size += size_list[i * 2] * size_list[(i * 2) + 1];

   uint8_t *res_mem  = xmalloc(total_size * 2);
   uint8_t *last_mem = res_mem + total_size;

   const uint8_t *src = values;
   int offset = 0, part = 0, remain = size_list[1];
   while (part < nparts) {
      groupid_t gid = netdb_lookup(netdb, nid + offset);
      netgroup_t *g = &(groups[gid]);

      const int size = size_list[part * 2];

      assert(g->sig_decl == NULL);
      assert(remain >= g->length);

      g->sig_decl   = decl;
      g->resolution = memo;
      g->size       = size;
      g->resolved   = res_mem;
      g->last_value = last_mem;

      if (offset == 0)
         g->flags |= NET_F_OWNS_MEM;

      const int nbytes = g->length * size;

      res_mem += nbytes;
      last_mem += nbytes;

      memcpy(g->resolved, src, nbytes);
      memcpy(g->last_value, src, nbytes);

      offset += g->length;
      src    += nbytes;
      remain -= g->length;

      if (remain == 0) {
         part++;
         remain = size_list[(part * 2) + 1];
      }
   }
}

DLLEXPORT
void _assert_fail(const uint8_t *msg, int32_t msg_len, int8_t severity,
                  int8_t is_report, const rt_loc_t *where)
{
   // LRM 93 section 8.2
   // The error message consists of at least
   // a) An indication that this message is from an assertion
   // b) The value of the severity level
   // c) The value of the message string
   // d) The name of the design unit containing the assertion

   assert(severity <= SEVERITY_FAILURE);

   const char *levels[] = {
      "Note", "Warning", "Error", "Failure"
   };

   if (init_side_effect != SIDE_EFFECT_ALLOW) {
      init_side_effect = SIDE_EFFECT_OCCURRED;
      return;
   }

   rt_show_trace();

   loc_t loc;
   from_rt_loc(where, &loc);

   void (*fn)(const loc_t *loc, const char *fmt, ...) = fatal_at;

   switch (severity) {
   case SEVERITY_NOTE:    fn = note_at; break;
   case SEVERITY_WARNING: fn = warn_at; break;
   case SEVERITY_ERROR:
   case SEVERITY_FAILURE: fn = error_at; break;
   }

   if (severity >= exit_severity)
      fn = fatal_at;

   (*fn)(&loc, "%s+%d: %s %s: %.*s\r\tProcess %s",
         fmt_time(now), iteration,
         (is_report ? "Report" : "Assertion"),
         levels[severity],
         msg_len, msg,
         ((active_proc == NULL) ? "(init)"
          : istr(tree_ident(active_proc->source))));
}

DLLEXPORT
void _bounds_fail(int32_t value, int32_t min, int32_t max, int32_t kind,
                  rt_loc_t *where, const char *hint)
{
   rt_show_trace();

   loc_t loc;
   from_rt_loc(where, &loc);

   char *copy LOCAL = xstrdup(hint ?: "");
   const char *prefix = copy, *suffix = copy;
   char *sep = strchr(copy, '|');
   if (sep != NULL) {
      suffix = sep + 1;
      *sep = '\0';
   }

   const char *spacer = hint ? " " : "";

   switch ((bounds_kind_t)kind) {
   case BOUNDS_ARRAY_TO:
      fatal_at(&loc, "array index %d outside bounds %d to %d%s%s",
               value, min, max, spacer, suffix);
      break;
   case BOUNDS_ARRAY_DOWNTO:
      fatal_at(&loc, "array index %d outside bounds %d downto %d%s%s",
               value, max, min, spacer, suffix);
      break;

   case BOUNDS_ENUM:
      fatal_at(&loc, "value %d outside %s bounds %d to %d%s%s",
               value, prefix, min, max, spacer, suffix);
      break;

   case BOUNDS_TYPE_TO:
      fatal_at(&loc, "value %d outside bounds %d to %d%s%s",
               value, min, max, spacer, suffix);
      break;

   case BOUNDS_TYPE_DOWNTO:
      fatal_at(&loc, "value %d outside bounds %d downto %d%s%s",
               value, max, min, spacer, suffix);
      break;

   case BOUNDS_ARRAY_SIZE:
      fatal_at(&loc, "length of target %d does not match length of value "
               "%d%s%s", min, max, spacer, suffix);
      break;

   case BOUNDS_INDEX_TO:
      fatal_at(&loc, "index %d violates constraint bounds %d to %d",
               value, min, max);
      break;

   case BOUNDS_INDEX_DOWNTO:
      fatal_at(&loc, "index %d violates constraint bounds %d downto %d",
               value, max, min);
      break;
   }
}

DLLEXPORT
int64_t _value_attr(const uint8_t *raw_str, int32_t str_len,
                    image_map_t *map, const rt_loc_t *where)
{
   const char *p = (const char *)raw_str;
   const char *endp = p + str_len;

   while (p < endp && isspace((int)*p))
      ++p;

   loc_t loc = LOC_INVALID;
   int64_t value = INT64_MIN;

   switch (map->kind) {
   case IMAGE_INTEGER:
      {
         bool is_negative = p < endp && *p == '-';
         int num_digits = 0;

         if (is_negative) {
            ++p;
         }
         while (p < endp && (isdigit((int)*p) || *p == '_')) {
            if (*p != '_') {
               value *= 10;
               value += (*p - '0');
               num_digits++;
            }
            ++p;
         }
         if (is_negative) {
            value = -value;
         }

         if (num_digits == 0) {
            from_rt_loc(where, &loc);
            fatal_at(&loc, "invalid integer value "
                     "\"%.*s\"", str_len, (const char *)raw_str);
         }
      }
      break;

   case IMAGE_REAL:
      from_rt_loc(where, &loc);
      fatal_at(&loc, "real values not yet supported in 'VALUE");
      break;

   case IMAGE_PHYSICAL:
      from_rt_loc(where, &loc);
      fatal_at(&loc, "physical values not yet supported in 'VALUE");
      break;

   case IMAGE_ENUM:
      for (int i = 0; value < 0 && i < map->count; i++) {
         const char *elem = map->elems + (i * map->stride);
         bool match_case = false;
         for (int j = 0; j < map->stride && p + j < endp; j++) {
            if (elem[j] != p[j]
                && (match_case || tolower((int)elem[j]) != tolower((int)p[j])))
               break;
            else if (elem[j + 1] == '\0') {
               value = i;
               p += j + 1;
               break;
            }
            else if (elem[j] == '\'')
               match_case = !match_case;
         }
      }

      if (value < 0) {
         from_rt_loc(where, &loc);
         fatal_at(&loc, "\"%.*s\" is not a valid enumeration value",
                  str_len, (const char *)raw_str);
      }
      break;
   }

   while (p < endp && *p != '\0') {
      if (!isspace((int)*p)) {
         from_rt_loc(where, &loc);
         fatal_at(&loc, "found invalid characters \"%.*s\" after value "
                  "\"%.*s\"", (int)(endp - p), p, str_len,
                  (const char *)raw_str);
      }
      p++;
   }

   return value;
}

DLLEXPORT
void _div_zero(const rt_loc_t *where)
{
   loc_t loc;
   from_rt_loc(where, &loc);
   fatal_at(&loc, "division by zero");
}

DLLEXPORT
void _null_deref(const rt_loc_t *where)
{
   loc_t loc;
   from_rt_loc(where, &loc);
   fatal_at(&loc, "null access dereference");
}

DLLEXPORT
int64_t _std_standard_now(void)
{
   return now;
}

DLLEXPORT
void _nvc_env_stop(int32_t finish, int32_t have_status, int32_t status)
{
   if (have_status)
      notef("%s called with status %d", finish ? "FINISH" : "STOP", status);
   else
      notef("%s called", finish ? "FINISH" : "STOP");

   exit(status);
}

DLLEXPORT
void *_vec_load(const int32_t *nids, void *where,
                int32_t low, int32_t high, int32_t last)
{
   TRACE("_vec_load %s where=%p low=%d high=%d last=%d",
         fmt_net(nids[0]), where, low, high, last);

   int offset = low;

   groupid_t gid = netdb_lookup(netdb, nids[offset]);
   netgroup_t *g = &(groups[gid]);
   int skip = nids[offset] - g->first;

   assert((g->flags & NET_F_LAST_VALUE) || !last);

   if (offset + g->length - skip > high) {
      // If the signal data is already contiguous return a pointer to
      // that rather than copying into the user buffer
      void *r = unlikely(last) ? g->last_value : g->resolved;
      return (uint8_t *)r + (skip * g->size);
   }

   uint8_t *p = where;
   for (;;) {
      const int to_copy = MIN(high - offset + 1, g->length - skip);
      const int bytes   = to_copy * g->size;

      const void *src = unlikely(last) ? g->last_value : g->resolved;

      memcpy(p, (uint8_t *)src + (skip * g->size), bytes);

      offset += g->length - skip;
      p += bytes;

      if (offset > high)
         break;

      gid = netdb_lookup(netdb, nids[offset]);
      g = &(groups[gid]);
      skip = nids[offset] - g->first;
   }

   // Signal data was non-contiguous so return the user buffer
   return where;
}

DLLEXPORT
void _image(int64_t val, image_map_t *map, struct uarray *u)
{
   char *buf = NULL;
   size_t len = 0;

   switch (map->kind) {
   case IMAGE_INTEGER:
      buf = rt_tmp_alloc(16);
      len = checked_sprintf(buf, 16, "%"PRIi64, val);
      break;

   case IMAGE_ENUM:
      buf = rt_tmp_alloc(map->stride);
      strncpy(buf, map->elems + (val * map->stride), map->stride);
      len = strlen(buf);
      break;

   case IMAGE_REAL:
      {
         union {
            double  d;
            int64_t i;
         } u = { .i = val };
         buf = rt_tmp_alloc(32);
         len = checked_sprintf(buf, 32, "%.*g", 15 + 3, u.d);
      }
      break;

   case IMAGE_PHYSICAL:
      buf = rt_tmp_alloc(16 + map->stride);
      len = checked_sprintf(buf, 16 + map->stride, "%"PRIi64" %s",
                            val, map->elems + (0 * map->stride));
      break;
   }

   u->ptr = buf;
   u->dims[0].left  = 1;
   u->dims[0].right = len;
   u->dims[0].dir   = RANGE_TO;
}

DLLEXPORT
void _bit_shift(int32_t kind, const uint8_t *data, int32_t len,
                int8_t dir, int32_t shift, struct uarray *u)
{
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
   u->dims[0].left  = (dir == RANGE_TO) ? 0 : len - 1;
   u->dims[0].right = (dir == RANGE_TO) ? len - 1 : 0;
   u->dims[0].dir   = dir;
}

DLLEXPORT
void _bit_vec_op(int32_t kind, const uint8_t *left, int32_t left_len,
                 int8_t left_dir, const uint8_t *right, int32_t right_len,
                 int8_t right_dir, struct uarray *u)
{
   if ((kind != BIT_VEC_NOT) && (left_len != right_len))
      fatal("arguments to bit vector operation are not the same length");

   uint8_t *buf = rt_tmp_alloc(left_len);

   switch (kind) {
   case BIT_VEC_NOT:
      for (int i = 0; i < left_len; i++)
         buf[i] = !left[i];
      break;

   case BIT_VEC_AND:
      for (int i = 0; i < left_len; i++)
         buf[i] = left[i] && right[i];
      break;

   case BIT_VEC_OR:
      for (int i = 0; i < left_len; i++)
         buf[i] = left[i] || right[i];
      break;

   case BIT_VEC_XOR:
      for (int i = 0; i < left_len; i++)
         buf[i] = left[i] ^ right[i];
      break;

   case BIT_VEC_XNOR:
      for (int i = 0; i < left_len; i++)
         buf[i] = !(left[i] ^ right[i]);
      break;

   case BIT_VEC_NAND:
      for (int i = 0; i < left_len; i++)
         buf[i] = !(left[i] && right[i]);
      break;

   case BIT_VEC_NOR:
      for (int i = 0; i < left_len; i++)
         buf[i] = !(left[i] || right[i]);
      break;
   }

   u->ptr = buf;
   u->dims[0].left  = (left_dir == RANGE_TO) ? 0 : left_len - 1;
   u->dims[0].right = (left_dir == RANGE_TO) ? left_len - 1 : 0;
   u->dims[0].dir   = left_dir;
}

DLLEXPORT
void _debug_out(int32_t val, int32_t reg)
{
   printf("DEBUG: r%d val=%"PRIx32"\n", reg, val);
}

DLLEXPORT
void _debug_dump(const uint8_t *ptr, int32_t len)
{
   printf("---- %p ----\n", ptr);

   if (ptr != NULL) {
      for (int i = 0; i < len; i++)
         printf("%02x%c", ptr[i], (i % 8 == 7) ? '\n' : ' ');
      if (len % 8 != 0)
         printf("\n");
   }
}

DLLEXPORT
int64_t _last_event(const int32_t *nids, int32_t n)
{
   //TRACE("_last_event %s n=%d %d", fmt_net(nids[0]), n);

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

DLLEXPORT
int32_t _test_net_flag(const int32_t *nids, int32_t n, int32_t flag)
{
   //TRACE("_test_net_flag %s n=%d flag=%d", fmt_net(nids[0]), n, flag);

   int offset = 0;
   while (offset < n) {
      netgroup_t *g = &(groups[netdb_lookup(netdb, nids[offset])]);

      if (g->flags & flag)
         return 1;

      offset += g->length;
   }

   return 0;
}

DLLEXPORT
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

DLLEXPORT
void _file_write(void **_fp, uint8_t *data, int32_t len)
{
   FILE **fp = (FILE **)_fp;

   TRACE("_file_write fp=%p data=%p len=%d", fp, data, len);

   if (*fp == NULL)
      fatal("write to closed file");

   fwrite(data, 1, len, *fp);
}

DLLEXPORT
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

DLLEXPORT
void _file_close(void **_fp)
{
   FILE **fp = (FILE **)_fp;

   TRACE("_file_close fp=%p", fp);

   if (*fp == NULL)
      fatal("attempt to close already closed file");

   fclose(*fp);
   *fp = NULL;
}

DLLEXPORT
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
   fflush(stderr);

   va_end(ap);
}

static void deltaq_insert(event_t *e)
{
   if (e->when == now) {
      event_t **chain = (e->kind == E_DRIVER) ? &delta_driver : &delta_proc;
      e->delta_chain = *chain;
      *chain = e;
   }
   else {
      e->delta_chain = NULL;
      heap_insert(eventq_heap, heap_key(e->when, e->kind), e);
   }
}

static void deltaq_insert_proc(uint64_t delta, rt_proc_t *wake)
{
   event_t *e = rt_alloc(event_stack);
   e->when       = now + delta;
   e->kind       = E_PROCESS;
   e->proc       = wake;
   e->wakeup_gen = wake->wakeup_gen;

   deltaq_insert(e);
}

static void deltaq_insert_driver(uint64_t delta, netgroup_t *group,
                                 rt_proc_t *driver)
{
   event_t *e = rt_alloc(event_stack);
   e->when       = now + delta;
   e->kind       = E_DRIVER;
   e->group      = group;
   e->proc       = driver;
   e->wakeup_gen = UINT32_MAX;

   deltaq_insert(e);
}

#if TRACE_DELTAQ > 0
static void deltaq_walk(uint64_t key, void *user, void *context)
{
   event_t *e = user;

   fprintf(stderr, "%s\t", fmt_time(e->when));
   switch (e->kind) {
   case E_DRIVER:
      fprintf(stderr, "driver\t %s\n", fmt_group(e->group));
      break;
   case E_PROCESS:
      fprintf(stderr, "process\t %s%s\n", istr(tree_ident(e->proc->source)),
              (e->wakeup_gen == e->proc->wakeup_gen) ? "" : " (stale)");
      break;
   case E_TIMEOUT:
      fprintf(stderr, "timeout\t %p %p\n", e->timeout_fn, e->timeout_user);
      break;
   }
}

static void deltaq_dump(void)
{
   for (event_t *e = delta_driver; e != NULL; e = e->delta_chain)
      fprintf(stderr, "delta\tdriver\t %s\n", fmt_group(e->group));

   for (event_t *e = delta_proc; e != NULL; e = e->delta_chain)
      fprintf(stderr, "delta\tprocess\t %s%s\n",
              istr(tree_ident(e->proc->source)),
              (e->wakeup_gen == e->proc->wakeup_gen) ? "" : " (stale)");

   heap_walk(eventq_heap, deltaq_walk, NULL);
}
#endif

static res_memo_t *rt_memo_resolution_fn(type_t type, resolution_fn_t fn)
{
   // Optimise some common resolution functions by memoising them

   res_memo_t *memo = hash_get(res_memo_hash, fn);
   if (memo != NULL)
      return memo;

   if (type_is_array(type))
      type = type_elem(type);

   memo = xmalloc(sizeof(res_memo_t));
   memo->fn    = fn;
   memo->flags = 0;

   hash_put(res_memo_hash, fn, memo);

   if (type_kind(type_base_recur(type)) != T_ENUM)
      return memo;

   int nlits;
   switch (type_kind(type)) {
   case T_ENUM:
      nlits = type_enum_literals(type);
      break;
   case T_SUBTYPE:
      {
         int64_t low, high;
         range_bounds(type_dim(type, 0), &low, &high);
         nlits = high - low + 1;
      }
      break;
   default:
      return memo;
   }

   if (nlits > 16)
      return memo;

   init_side_effect = SIDE_EFFECT_DISALLOW;

   // Memoise the function for all two value cases

   for (int i = 0; i < nlits; i++) {
      for (int j = 0; j < nlits; j++) {
         int8_t args[2] = { i, j };
         memo->tab2[i][j] = (*fn)(args, 2);
      }
   }

   // Memoise the function for all single value cases and determine if the
   // function behaves like the identity function

   bool identity = true;
   for (int i = 0; i < nlits; i++) {
      int8_t args[1] = { i };
      memo->tab1[i] = (*fn)(args, 1);
      identity = identity && (memo->tab1[i] == i);
   }

   if (init_side_effect != SIDE_EFFECT_OCCURRED) {
      memo->flags |= R_MEMO;
      if (identity)
         memo->flags |= R_IDENT;
   }

   return memo;
}

static void rt_global_event(rt_event_t kind)
{
   callback_t *it = global_cbs[kind];
   if (unlikely(it != NULL)) {
      while (it != NULL) {
         callback_t *tmp = it->next;
         (*it->fn)(it->user);
         rt_free(callback_stack, it);
         it = tmp;
      }

      global_cbs[kind] = NULL;
   }
}

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
   // Allocate sz bytes that will be freed by the active process

   uint8_t *ptr = (uint8_t *)_tmp_stack + _tmp_alloc;
   _tmp_alloc += sz;
   return ptr;
}

static void rt_sched_event(sens_list_t **list, netid_t first, netid_t last,
                           rt_proc_t *proc, bool is_static)
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
      node->reenq      = (is_static ? list : NULL);

      *list = node;
   }
   else {
      // Reuse the stale entry
      assert(!is_static);
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
             (it->wakeup_gen == it->proc->wakeup_gen) ? "" : " (stale)");
   }
}
#endif  // TRACE_PENDING

static void rt_reset_group(groupid_t gid, netid_t first, unsigned length)
{
   netgroup_t *g = &(groups[gid]);
   memset(g, '\0', sizeof(netgroup_t));
   g->first       = first;
   g->length      = length;
   g->last_event  = INT64_MAX;
}

static void rt_free_delta_events(event_t *e)
{
   while (e != NULL) {
      event_t *tmp = e->delta_chain;
      rt_free(event_stack, e);
      e = tmp;
   }
}

static void rt_setup(tree_t top)
{
   now = 0;
   iteration = -1;
   active_proc = NULL;
   force_stop = false;
   can_create_delta = true;

   assert(resume == NULL);

   rt_free_delta_events(delta_proc);
   rt_free_delta_events(delta_driver);

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

   const int ndecls = tree_decls(top);
   decl_hash = hash_new(next_power_of_2(ndecls * 2), true);
   for (int i = 0; i < ndecls; i++) {
      tree_t d = tree_decl(top, i);
      hash_put(decl_hash, tree_ident(d), d);
   }

   res_memo_hash = hash_new(128, true);

   netdb_walk(netdb, rt_reset_group);

   const int nstmts = tree_stmts(top);
   for (int i = 0; i < nstmts; i++) {
      tree_t p = tree_stmt(top, i);
      assert(tree_kind(p) == T_PROCESS);

      procs[i].source     = p;
      procs[i].proc_fn    = jit_fun_ptr(istr(tree_ident(p)), true);
      procs[i].wakeup_gen = 0;
      procs[i].postponed  = !!(tree_flags(p) & TREE_F_POSTPONED);
      procs[i].tmp_stack  = NULL;
      procs[i].tmp_alloc  = 0;
      procs[i].pending    = false;
   }
}

static void rt_run(struct rt_proc *proc, bool reset)
{
   TRACE("%s process %s", reset ? "reset" : "run",
         istr(tree_ident(proc->source)));

   if (reset) {
      _tmp_stack = global_tmp_stack;
      _tmp_alloc = global_tmp_alloc;
   }
   else if (proc->tmp_stack != NULL) {
      TRACE("using private stack at %p %d", proc->tmp_stack, proc->tmp_alloc);
      _tmp_stack = proc->tmp_stack;
      _tmp_alloc = proc->tmp_alloc;

      // Will be updated by _private_stack if suspending in procedure otherwise
      // clear stack when process suspends
      proc->tmp_alloc = 0;
   }
   else {
      _tmp_stack = proc_tmp_stack;
      _tmp_alloc = 0;
   }

   active_proc = proc;
   (*proc->proc_fn)(reset ? 1 : 0);

   if (reset)
      global_tmp_alloc = _tmp_alloc;
}

static void rt_call_module_reset(ident_t name)
{
   char *buf LOCAL = xasprintf("%s_reset", istr(name));

   _tmp_stack = global_tmp_stack;
   _tmp_alloc = global_tmp_alloc;

   void (*reset_fn)(void) = jit_fun_ptr(buf, false);
   if (reset_fn != NULL) {
      TRACE("reset module %s", istr(name));
      (*reset_fn)();
   }

   global_tmp_alloc = _tmp_alloc;
}

static int32_t rt_resolve_group(netgroup_t *group, int driver, void *values)
{
   // Set driver to -1 for initial call to resolution function

   const size_t valuesz = group->size * group->length;

   void *resolved = NULL;
   if (unlikely(group->flags & NET_F_FORCED)) {
      resolved = group->forcing->data;
   }
   else if (group->resolution == NULL) {
      resolved = values;
   }
   else if ((group->resolution->flags & R_IDENT) && (group->n_drivers == 1)) {
      // Resolution function behaves like identity for a single driver
      resolved = values;
   }
   else if ((group->resolution->flags & R_MEMO) && (group->n_drivers == 1)) {
      // Resolution function has been memoised so do a table lookup

      resolved = alloca(valuesz);

      for (int j = 0; j < group->length; j++) {
         const int index = { ((const char *)values)[j] };
         const int8_t r = group->resolution->tab1[index];
         ((int8_t *)resolved)[j] = r;
      }
   }
   else if ((group->resolution->flags & R_MEMO) && (group->n_drivers == 2)) {
      // Resolution function has been memoised so do a table lookup

      resolved = alloca(valuesz);

      const char *p0 = group->drivers[0].waveforms->values->data;
      const char *p1 = group->drivers[1].waveforms->values->data;

      for (int j = 0; j < group->length; j++) {
         int driving[2] = { p0[j], p1[j] };
         if (likely(driver >= 0))
            driving[driver] = ((const char *)values)[j];

         const int8_t r = group->resolution->tab2[driving[0]][driving[1]];
         ((int8_t *)resolved)[j] = r;
      }
  }
   else {
      // Must actually call resolution function in general case

      resolved = alloca(valuesz);

      for (int j = 0; j < group->length; j++) {
#define CALL_RESOLUTION_FN(type) do {                                   \
            type vals[group->n_drivers];                                \
            for (int i = 0; i < group->n_drivers; i++) {                \
               const value_t *v = group->drivers[i].waveforms->values;  \
               vals[i] = ((const type *)v->data)[j];                    \
            }                                                           \
            if (likely(driver >= 0))                                    \
               vals[driver] = ((const type *)values)[j];                \
            type *r = (type *)resolved;                                 \
            r[j] = (*group->resolution->fn)(vals, group->n_drivers);    \
         } while (0)

         FOR_ALL_SIZES(group->size, CALL_RESOLUTION_FN);
      }
   }

   int32_t new_flags = NET_F_ACTIVE;
   if (memcmp(group->resolved, resolved, valuesz) != 0)
      new_flags |= NET_F_EVENT;

   // LAST_VALUE is the same as the initial value when
   // there have been no events on the signal otherwise
   // only update it when there is an event
   if (new_flags & NET_F_EVENT) {
      if (group->flags & NET_F_LAST_VALUE)
         memcpy(group->last_value, group->resolved, valuesz);
      memcpy(group->resolved, resolved, valuesz);

      group->last_event = now;
   }

   return new_flags;
}

static void rt_group_inital(groupid_t gid, netid_t first, unsigned length)
{
   netgroup_t *g = &(groups[gid]);
   if ((g->n_drivers == 1) && (g->resolution == NULL))
      rt_resolve_group(g, -1, g->drivers[0].waveforms->values->data);
   else if (g->n_drivers > 0)
      rt_resolve_group(g, -1, g->resolved);
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

   TRACE("calculate initial driver values");

   init_side_effect = SIDE_EFFECT_ALLOW;
   netdb_walk(netdb, rt_group_inital);

   TRACE("used %d bytes of global temporary stack", global_tmp_alloc);
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
      w->length += g->length;
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

   if (sl->wakeup_gen == sl->proc->wakeup_gen || sl->reenq != NULL) {
      TRACE("wakeup process %s%s", istr(tree_ident(sl->proc->source)),
            sl->proc->postponed ? " [postponed]" : "");
      ++(sl->proc->wakeup_gen);

      if (unlikely(sl->proc->postponed)) {
         sl->next  = postponed;
         postponed = sl;
      }
      else {
         sl->next = resume;
         resume = sl;
      }

      sl->proc->pending = true;
   }
   else
      rt_free(sens_list_stack, sl);
}

static bool rt_sched_driver(netgroup_t *group, uint64_t after,
                            uint64_t reject, value_t *values)
{
   if (unlikely(reject > after))
      fatal("signal %s pulse reject limit %s is greater than "
            "delay %s", fmt_group(group), fmt_time(reject), fmt_time(after));

   int driver = 0;
   if (unlikely(group->n_drivers != 1)) {
      // Try to find this process in the list of existing drivers
      for (driver = 0; driver < group->n_drivers; driver++) {
         if (likely(group->drivers[driver].proc == active_proc))
            break;
      }

      assert(driver != group->n_drivers);
   }

   driver_t *d = &(group->drivers[driver]);

   const size_t valuesz = group->size * group->length;

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
   bool already_scheduled = false;
   while (it != NULL) {
      rt_free_value(group, it->values);

      if (it->when == w->when)
         already_scheduled = true;

      waveform_t *next = it->next;
      rt_free(waveform_stack, it);
      it = next;
   }

   return already_scheduled;
}

static void rt_update_group(netgroup_t *group, int driver, void *values)
{
   const size_t valuesz = group->size * group->length;

   TRACE("update group %s values=%s driver=%d",
         fmt_group(group), fmt_values(values, valuesz), driver);

   const int32_t new_flags = rt_resolve_group(group, driver, values);
   group->flags |= new_flags;

   if (unlikely(n_active_groups == n_active_alloc)) {
      n_active_alloc *= 2;
      const size_t newsz = n_active_alloc * sizeof(struct netgroup *);
      active_groups = xrealloc(active_groups, newsz);
   }
   active_groups[n_active_groups++] = group;

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
      if (group->flags & NET_F_GLOBAL) {
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
   if (likely(proc != NULL)) {
      // Find the driver owned by proc
      int driver;
      for (driver = 0; driver < group->n_drivers; driver++) {
         if (likely(group->drivers[driver].proc == proc))
            break;
      }
      assert(driver != group->n_drivers);

      waveform_t *w_now  = group->drivers[driver].waveforms;
      waveform_t *w_next = w_now->next;

      if (likely((w_next != NULL) && (w_next->when == now))) {
         rt_update_group(group, driver, w_next->values->data);
         group->drivers[driver].waveforms = w_next;
         rt_free_value(group, w_now->values);
         rt_free(waveform_stack, w_now);
      }
      else
         assert(w_now != NULL);
   }
   else if (group->flags & NET_F_FORCED)
      rt_update_group(group, -1, group->forcing->data);
}

static bool rt_stale_event(event_t *e)
{
   return (e->kind == E_PROCESS) && (e->wakeup_gen != e->proc->wakeup_gen);
}

static void rt_push_run_queue(event_t *e)
{
   if (unlikely(run_queue.wr == run_queue.alloc)) {
      if (run_queue.alloc == 0) {
         run_queue.alloc = 128;
         run_queue.queue = xmalloc(sizeof(event_t *) * run_queue.alloc);
      }
      else {
         run_queue.alloc *= 2;
         run_queue.queue = realloc(run_queue.queue,
                                   sizeof(event_t *) * run_queue.alloc);
      }
   }

   if (unlikely(rt_stale_event(e)))
      rt_free(event_stack, e);
   else {
      run_queue.queue[(run_queue.wr)++] = e;
      if (e->kind == E_PROCESS)
         ++(e->proc->wakeup_gen);
   }
}

static event_t *rt_pop_run_queue(void)
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
   text_buf_t *buf = tb_new();
   tb_printf(buf, "Iteration limit of %d delta cycles reached. "
             "The following processes are active:\n",
             opt_get_int("stop-delta"));

   for (sens_list_t *it = resume; it != NULL; it = it->next) {
      tree_t p = it->proc->source;
      const loc_t *l = tree_loc(p);
      tb_printf(buf, "  %-30s %s line %d\n", istr(tree_ident(p)),
                istr(l->file), l->first_line);
   }

   tb_printf(buf, "You can increase this limit with --stop-delta");

   fatal("%s", tb_get(buf));
}

static void rt_resume_processes(sens_list_t **list)
{
   sens_list_t *it = *list;
   while (it != NULL) {
      if (it->proc->pending) {
         rt_run(it->proc, false /* reset */);
         it->proc->pending = false;
      }

      sens_list_t *next = it->next;

      if (it->reenq == NULL)
         rt_free(sens_list_stack, it);
      else {
         it->next = *(it->reenq);
         *(it->reenq) = it;
      }

      it = next;
   }

   *list = NULL;
}

static void rt_event_callback(bool postponed)
{
   watch_t **last = &callbacks;
   watch_t *next = NULL, *it;
   for (it = callbacks; it != NULL; it = next) {
      next = it->chain_pending;
      if (it->postponed == postponed) {
         (*it->fn)(now, it->signal, it, it->user_data);
         it->pending = false;

         *last = it->chain_pending;
         it->chain_pending = NULL;
      }
      else
         last = &(it->chain_pending);
   }
}

static inline bool rt_next_cycle_is_delta(void)
{
   return (delta_driver != NULL) || (delta_proc != NULL);
}

static void rt_cycle(int stop_delta)
{
   // Simulation cycle is described in LRM 93 section 12.6.4

   const bool is_delta_cycle = (delta_driver != NULL) || (delta_proc != NULL);

   if (is_delta_cycle)
      iteration = iteration + 1;
   else {
      event_t *peek = heap_min(eventq_heap);
      while (unlikely(rt_stale_event(peek))) {
         // Discard stale events
         rt_free(event_stack, heap_extract_min(eventq_heap));
         if (heap_size(eventq_heap) == 0)
            return;
         else
            peek = heap_min(eventq_heap);
      }
      now = peek->when;
      iteration = 0;
   }

   TRACE("begin cycle");

#if TRACE_DELTAQ > 0
   if (trace_on)
      deltaq_dump();
#endif
#if TRACE_PENDING > 0
   if (trace_on)
      rt_dump_pending();
#endif

   if (is_delta_cycle) {
      for (event_t *e = delta_driver; e != NULL; e = e->delta_chain)
         rt_push_run_queue(e);

      for (event_t *e = delta_proc; e != NULL; e = e->delta_chain)
         rt_push_run_queue(e);

      delta_driver = NULL;
      delta_proc = NULL;
   }
   else {
      rt_global_event(RT_NEXT_TIME_STEP);

      for (;;) {
         rt_push_run_queue(heap_extract_min(eventq_heap));

         if (heap_size(eventq_heap) == 0)
            break;

         event_t *peek = heap_min(eventq_heap);
         if (peek->when > now)
            break;
      }
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
      case E_TIMEOUT:
         (*event->timeout_fn)(now, event->timeout_user);
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

   // Run all non-postponed event callbacks
   rt_event_callback(false);

   // Run all processes that resumed because of signal events
   rt_resume_processes(&resume);
   rt_global_event(RT_END_OF_PROCESSES);

   for (unsigned i = 0; i < n_active_groups; i++) {
      netgroup_t *g = active_groups[i];
      g->flags &= ~(NET_F_ACTIVE | NET_F_EVENT);
   }
   n_active_groups = 0;

   if (!rt_next_cycle_is_delta()) {
      can_create_delta = false;
      rt_global_event(RT_LAST_KNOWN_DELTA_CYCLE);

      // Run any postponed processes
      rt_resume_processes(&postponed);

      // Execute all postponed event callbacks
      rt_event_callback(true);

      can_create_delta = true;
   }
}

static tree_t rt_recall_decl(const char *name)
{
   tree_t decl = hash_get(decl_hash, ident_new(name));
   if (decl != NULL)
      return decl;
   else
      fatal("cannot find name %s in elaborated design", name);
}

static void rt_cleanup_group(groupid_t gid, netid_t first, unsigned length)
{
   netgroup_t *g = &(groups[gid]);

   assert(g->first == first);
   assert(g->length == length);

   if (g->flags & NET_F_OWNS_MEM)
      free(g->resolved);

   free(g->forcing);

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

   rt_free_delta_events(delta_proc);
   rt_free_delta_events(delta_driver);

   heap_free(eventq_heap);
   eventq_heap = NULL;

   netdb_walk(netdb, rt_cleanup_group);
   netdb_close(netdb);

   hash_free(decl_hash);
   decl_hash = NULL;

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

   for (int i = 0; i < RT_LAST_EVENT; i++) {
      while (global_cbs[i] != NULL) {
         callback_t *tmp = global_cbs[i]->next;
         rt_free(callback_stack, global_cbs[i]);
         global_cbs[i] = tmp;
      }
   }

   rt_alloc_stack_destroy(event_stack);
   rt_alloc_stack_destroy(waveform_stack);
   rt_alloc_stack_destroy(sens_list_stack);
   rt_alloc_stack_destroy(watch_stack);
   rt_alloc_stack_destroy(callback_stack);

   hash_free(res_memo_hash);
}

static bool rt_stop_now(uint64_t stop_time)
{
   if ((delta_driver != NULL) || (delta_proc != NULL))
      return false;
   else if (heap_size(eventq_heap) == 0)
      return true;
   else if (force_stop)
      return true;
   else if (stop_time == UINT64_MAX)
      return false;
   else {
      event_t *peek = heap_min(eventq_heap);
      return peek->when > stop_time;
   }
}

static void rt_stats_print(void)
{
   nvc_rusage_t ru;
   nvc_rusage(&ru);

   notef("setup:%ums run:%ums maxrss:%ukB", ready_rusage.ms, ru.ms, ru.rss);
}

static void rt_reset_coverage(tree_t top)
{
   int32_t *cover_stmts = jit_var_ptr("cover_stmts", false);
   if (cover_stmts != NULL) {
      const int ntags = tree_attr_int(top, ident_new("stmt_tags"), 0);
      memset(cover_stmts, '\0', sizeof(int32_t) * ntags);
   }

   int32_t *cover_conds = jit_var_ptr("cover_conds", false);
   if (cover_conds != NULL) {
      const int ntags = tree_attr_int(top, ident_new("cond_tags"), 0);
      memset(cover_conds, '\0', sizeof(int32_t) * ntags);
   }
}

static void rt_emit_coverage(tree_t top)
{
   const int32_t *cover_stmts = jit_var_ptr("cover_stmts", false);
   const int32_t *cover_conds = jit_var_ptr("cover_conds", false);
   if (cover_stmts != NULL)
      cover_report(top, cover_stmts, cover_conds);
}

static void rt_interrupt(void)
{
   if (active_proc != NULL)
      fatal_at(tree_loc(active_proc->source),
               "interrupted in process %s at %s+%d",
               istr(tree_ident(active_proc->source)), fmt_time(now), iteration);
   else
      fatal("interrupted");
}

#ifdef __MINGW32__
static BOOL rt_win_ctrl_handler(DWORD fdwCtrlType)
{
   switch (fdwCtrlType) {
   case CTRL_C_EVENT:
      rt_interrupt();
      return TRUE;

   default:
      return FALSE;
   }
}
#endif

void rt_start_of_tool(tree_t top)
{
   jit_init(top);

#ifndef __MINGW32__
   struct sigaction sa;
   sa.sa_sigaction = (void*)rt_interrupt;
   sigemptyset(&sa.sa_mask);
   sa.sa_flags = SA_RESTART | SA_SIGINFO;

   sigaction(SIGINT, &sa, NULL);
#else
   if (!SetConsoleCtrlHandler(rt_win_ctrl_handler, TRUE))
      fatal_trace("SetConsoleCtrlHandler");
#endif

   jit_bind_fn("_std_standard_now", _std_standard_now);
   jit_bind_fn("_sched_process", _sched_process);
   jit_bind_fn("_sched_waveform", _sched_waveform);
   jit_bind_fn("_sched_event", _sched_event);
   jit_bind_fn("_assert_fail", _assert_fail);
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
   jit_bind_fn("_bit_vec_op", _bit_vec_op);
   jit_bind_fn("_test_net_flag", _test_net_flag);
   jit_bind_fn("_last_event", _last_event);
   jit_bind_fn("_div_zero", _div_zero);
   jit_bind_fn("_null_deref", _null_deref);

   trace_on = opt_get_int("rt_trace_en");

   event_stack     = rt_alloc_stack_new(sizeof(event_t), "event");
   waveform_stack  = rt_alloc_stack_new(sizeof(waveform_t), "waveform");
   sens_list_stack = rt_alloc_stack_new(sizeof(sens_list_t), "sens_list");
   watch_stack     = rt_alloc_stack_new(sizeof(watch_t), "watch");
   callback_stack  = rt_alloc_stack_new(sizeof(callback_t), "callback");

   n_active_alloc = 128;
   active_groups = xmalloc(n_active_alloc * sizeof(struct netgroup *));

   global_tmp_stack = mmap_guarded(GLOBAL_TMP_STACK_SZ, "global temp stack");
   proc_tmp_stack   = mmap_guarded(PROC_TMP_STACK_SZ, "process temp stack");

   global_tmp_alloc = 0;

   rt_reset_coverage(top);

   nvc_rusage(&ready_rusage);
}

void rt_end_of_tool(tree_t top)
{
   rt_cleanup(top);
   rt_emit_coverage(top);

   jit_shutdown();

   if (opt_get_int("rt-stats"))
      rt_stats_print();
}

void rt_run_sim(uint64_t stop_time)
{
   const int stop_delta = opt_get_int("stop-delta");

   rt_global_event(RT_START_OF_SIMULATION);
   while (!rt_stop_now(stop_time))
      rt_cycle(stop_delta);
   rt_global_event(RT_END_OF_SIMULATION);
}

static void rt_interactive_fatal(void)
{
   aborted = true;
   longjmp(fatal_jmp, 1);
}

void rt_run_interactive(uint64_t stop_time)
{
   if (aborted)
      errorf("simulation has aborted and must be restarted");
   else if ((heap_size(eventq_heap) == 0) && (delta_proc == NULL))
      warnf("no future simulation events");
   else {
      set_fatal_fn(rt_interactive_fatal);

      if (setjmp(fatal_jmp) == 0)
         rt_run_sim(stop_time);

      set_fatal_fn(NULL);
   }
}

void rt_restart(tree_t top)
{
   rt_setup(top);
   rt_initial(top);
   aborted = false;
}

void rt_set_timeout_cb(uint64_t when, timeout_fn_t fn, void *user)
{
   event_t *e = rt_alloc(event_stack);
   e->when         = now + when;
   e->kind         = E_TIMEOUT;
   e->group        = NULL;
   e->proc         = NULL;
   e->timeout_fn   = fn;
   e->timeout_user = user;
   e->wakeup_gen   = UINT32_MAX;

   deltaq_insert(e);
}

watch_t *rt_set_event_cb(tree_t s, sig_event_fn_t fn, void *user,
                         bool postponed)
{
   assert(tree_kind(s) == T_SIGNAL_DECL);

   if (fn == NULL) {
      // Find the first entry in the watch list and disable it
      for (watch_t *it = watches; it != NULL; it = it->chain_all) {
         if ((it->signal == s) && (it->user_data == user)) {
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
      w->length        = 0;
      w->postponed     = postponed;

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

void rt_set_global_cb(rt_event_t event, rt_event_fn_t fn, void *user)
{
   assert(event < RT_LAST_EVENT);

   callback_t *cb = rt_alloc(callback_stack);
   cb->next = global_cbs[event];
   cb->fn   = fn;
   cb->user = user;

   global_cbs[event] = cb;
}

size_t rt_watch_value(watch_t *w, uint64_t *buf, size_t max, bool last)
{
   int offset = 0;
   for (int i = 0; (i < w->n_groups) && (offset < max); i++) {
      netgroup_t *g = w->groups[i];

#define SIGNAL_VALUE_EXPAND_U64(type) do {                              \
         const type *sp = (type *)(last ? g->last_value : g->resolved); \
         for (int j = 0; (j < g->length) && (offset + j < max); j++)    \
            buf[offset + j] = sp[j];                                    \
      } while (0)

      FOR_ALL_SIZES(g->size, SIGNAL_VALUE_EXPAND_U64);

      offset += g->length;
   }

   return offset;
}

static size_t rt_group_string(netgroup_t *group, const char *map,
                              char *buf, const char *end1)
{
   char *bp = buf;
   const char *vals = group->resolved;

   if (likely(map != NULL)) {
      for (int j = 0; j < group->length; j++) {
         if (bp + 1 < end1)
            *bp++ = map[(int)vals[j]];
      }
   }
   else {
      for (int j = 0; j < group->length; j++) {
         if (bp + 1 < end1)
            *bp++ = vals[j];
      }
   }

   if (bp < end1)
      *bp = '\0';

   return bp - buf;
}

size_t rt_watch_string(watch_t *w, const char *map, char *buf, size_t max)
{
   char *bp = buf;
   size_t offset = 0;
   for (int i = 0; i < w->n_groups; i++) {
      netgroup_t *g = w->groups[i];
      bp += rt_group_string(g, map, bp, buf + max);
      offset += g->length;
   }

   return offset + 1;
}

size_t rt_signal_string(tree_t s, const char *map, char *buf, size_t max)
{
   char *bp = buf;
   const int nnets = tree_nets(s);
   int offset = 0;
   while (offset < nnets) {
      netid_t nid = tree_net(s, offset);
      netgroup_t *g = &(groups[netdb_lookup(netdb, nid)]);
      bp += rt_group_string(g, map, bp, buf + max);
      offset += g->length;
   }

   return offset + 1;
}

size_t rt_signal_value(tree_t s, uint64_t *buf, size_t max)
{
   const int nnets = tree_nets(s);
   int offset = 0;
   while (offset < nnets) {
      netid_t nid = tree_net(s, offset);
      netgroup_t *g = &(groups[netdb_lookup(netdb, nid)]);

#define SIGNAL_READ_EXPAND_U64(type) do {                               \
         const type *sp = (type *)g->resolved;                          \
         for (int i = 0; (i < g->length) && (offset + i < max); i++)    \
            buf[offset + i] = sp[i];                                    \
      } while (0)

      FOR_ALL_SIZES(g->size, SIGNAL_READ_EXPAND_U64);

      offset += g->length;
   }

   return offset;
}

bool rt_force_signal(tree_t s, const uint64_t *buf, size_t count,
                     bool propagate)
{
   TRACE("force signal %s to %s propagate=%d", istr(tree_ident(s)),
         fmt_values(buf, count * sizeof(uint64_t)), propagate);

   assert(!propagate || can_create_delta);

   const int nnets = tree_nets(s);
   int offset = 0;
   while (offset < nnets) {
      netid_t nid = tree_net(s, offset);
      netgroup_t *g = &(groups[netdb_lookup(netdb, nid)]);

      g->flags |= NET_F_FORCED;

      if (g->forcing == NULL)
         g->forcing = rt_alloc_value(g);

#define SIGNAL_FORCE_EXPAND_U64(type) do {                              \
         type *dp = (type *)g->forcing->data;                           \
         for (int i = 0; (i < g->length) && (offset + i < count); i++)  \
            dp[i] = buf[offset + i];                                    \
      } while (0)

      FOR_ALL_SIZES(g->size, SIGNAL_FORCE_EXPAND_U64);

      if (propagate)
         deltaq_insert_driver(0, g, NULL);

      offset += g->length;
   }

   return (offset == count);
}

bool rt_can_create_delta(void)
{
   return can_create_delta;
}

uint64_t rt_now(unsigned *deltas)
{
   if (deltas != NULL)
      *deltas = MAX(iteration, 0);
   return now;
}

void rt_stop(void)
{
   force_stop = true;
}

void rt_set_exit_severity(rt_severity_t severity)
{
   exit_severity = severity;
}
