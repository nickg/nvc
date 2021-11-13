//
//  Copyright (C) 2011-2021  Nick Gasson
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
#include "cover.h"
#include "hash.h"
#include "debug.h"
#include "enode.h"

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
#include <time.h>

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
#define RT_DEBUG      0

typedef struct event       event_t;
typedef struct waveform    waveform_t;
typedef struct sens_list   sens_list_t;
typedef struct value       value_t;
typedef struct watch_list  watch_list_t;
typedef struct callback    callback_t;
typedef struct image_map   image_map_t;
typedef struct rt_loc      rt_loc_t;
typedef struct rt_nexus_s  rt_nexus_t;
typedef struct rt_scope_s  rt_scope_t;
typedef struct uarray      uarray_t;
typedef struct rt_source_s rt_source_t;

typedef void *(*proc_fn_t)(void *, rt_scope_t *);
typedef uint64_t (*resolution_fn_t)(void *, void *, int32_t, int32_t);

typedef struct {
   e_node_t    source;
   proc_fn_t   proc_fn;
   uint32_t    wakeup_gen;
   void       *tmp_stack;
   uint32_t    tmp_alloc;
   bool        postponed;
   bool        pending;
   uint64_t    usage;
   rt_scope_t *scope;
   void       *privdata;
} rt_proc_t;

typedef enum {
   EVENT_TIMEOUT,
   EVENT_DRIVER,
   EVENT_PROCESS
} event_kind_t;

struct event {
   uint64_t      when;
   event_kind_t  kind;
   uint32_t      wakeup_gen;
   event_t      *delta_chain;
   rt_proc_t    *proc;
   rt_nexus_t   *nexus;
   rt_source_t  *source;
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
};

// The code generator knows the layout of this struct
typedef struct {
   void          *fn;
   void          *context;
   rt_ffi_spec_t  spec;
   uint32_t       refcnt;
} rt_closure_t;

STATIC_ASSERT(sizeof(rt_closure_t) == 24);

typedef struct rt_source_s {
   rt_proc_t    *proc;
   rt_nexus_t   *input;
   rt_nexus_t   *output;
   waveform_t   *waveforms;
   rt_closure_t *conv_func;
} rt_source_t;

struct value {
   value_t *next;
   union {
      char     data[0];
      uint64_t qwords[0];
   };
} __attribute__((aligned(8)));

// The code generator knows the layout of this struct
typedef struct {
   resolution_fn_t  fn;
   void            *context;
   uint32_t         flags;
   int32_t          ileft;
   int32_t          nlits;
} rt_resolution_t;

typedef struct {
   resolution_fn_t  fn;
   void            *context;
   res_flags_t      flags;
   int32_t          ileft;
   int8_t           tab2[16][16];
   int8_t           tab1[16];
} res_memo_t;

typedef enum {
   NET_F_FORCED     = (1 << 0),
   NET_F_OWNS_MEM   = (1 << 1),
   NET_F_LAST_VALUE = (1 << 2),
   NET_F_PENDING    = (1 << 3),
} net_flags_t;

typedef struct rt_nexus_s {
   e_node_t      enode;
   uint32_t      width;
   uint32_t      size;
   value_t      *free_values;
   uint64_t      last_event;
   uint64_t      last_active;
   int32_t       event_delta;
   int32_t       active_delta;
   sens_list_t  *pending;
   watch_list_t *watching;
   value_t      *forcing;
   res_memo_t   *resolution;
   net_flags_t   flags;
   unsigned      rank;
   unsigned      n_sources;
   unsigned      n_signals;
   unsigned      n_outputs;
   rt_source_t  *sources;
   rt_signal_t **signals;
   rt_source_t **outputs;
   void         *resolved;
   void         *last_value;
   unsigned     *offsets;
} rt_nexus_t;

// The code generator knows the layout of this struct
typedef struct {
   uint32_t  id;
   uint32_t  __pad;
   void     *resolved;
   void     *last_value;
} sig_shared_t;

STATIC_ASSERT(sizeof(sig_shared_t) == 24);

typedef enum {
   NEXUS_MAP_SEARCH,
   NEXUS_MAP_DIVIDE,
   NEXUS_MAP_DIRECT
} rt_nexus_map_t;

typedef struct rt_signal_s {
   sig_shared_t     shared;
   e_node_t         enode;
   uint32_t         width;
   uint32_t         size;
   rt_nexus_map_t   nmap_kind;
   uint32_t         nmap_param;
   net_flags_t      flags;
   uint32_t         n_nexus;
   rt_nexus_t     **nexus;      // TODO: flatten this
} rt_signal_t;

typedef struct rt_scope_s {
   rt_signal_t *signals;
   unsigned     n_signals;
   rt_proc_t   *procs;
   unsigned     n_procs;
   e_node_t     enode;
   void        *privdata;
   rt_scope_t  *parent;
} rt_scope_t;

// The code generator knows the layout of this struct
typedef struct uarray {
   void *ptr;
   struct {
      int32_t left;
      int32_t length;
   } dims[1];
} uarray_t;

typedef struct {
   event_t **queue;
   size_t    wr, rd;
   size_t    alloc;
} rt_run_queue_t;

struct watch {
   rt_signal_t    *signal;
   sig_event_fn_t  fn;
   bool            pending;
   watch_t        *chain_all;
   watch_t        *chain_pending;
   void           *user_data;
   size_t          length;
   bool            postponed;
};

struct watch_list {
   watch_t      *watch;
   watch_list_t *next;
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

typedef struct {
   uint32_t n_signals;
   uint32_t n_contig;
   uint32_t n_procs;
   uint32_t runq_min;
   uint32_t runq_max;
   uint32_t n_simple;
   uint32_t nmap_direct;
   uint32_t nmap_search;
   uint32_t nmap_divide;
   double   runq_mean;
   uint64_t deltas;
} rt_profile_t;

static rt_proc_t       *active_proc = NULL;
static rt_scope_t      *active_scope = NULL;
static rt_scope_t      *scopes = NULL;
static rt_run_queue_t   timeoutq;
static rt_run_queue_t   driverq;
static rt_run_queue_t   procq;
static heap_t          *eventq_heap = NULL;
static heap_t          *rankn_heap = NULL;
static unsigned         n_scopes = 0;
static unsigned         n_nexuses = 0;
static uint64_t         now = 0;
static int              iteration = -1;
static bool             trace_on = false;
static nvc_rusage_t     ready_rusage;
static jmp_buf          fatal_jmp;
static bool             aborted = false;
static sens_list_t     *resume = NULL;
static sens_list_t     *postponed = NULL;
static watch_t         *watches = NULL;
static watch_t         *callbacks = NULL;
static event_t         *delta_proc = NULL;
static event_t         *delta_driver = NULL;
static void            *global_tmp_stack = NULL;
static void            *proc_tmp_stack = NULL;
static uint32_t         global_tmp_alloc;
static hash_t          *res_memo_hash = NULL;
static side_effect_t    init_side_effect = SIDE_EFFECT_ALLOW;
static bool             force_stop;
static bool             can_create_delta;
static callback_t      *global_cbs[RT_LAST_EVENT];
static rt_severity_t    exit_severity = SEVERITY_ERROR;
static bool             profiling = false;
static rt_profile_t     profile;
static rt_nexus_t      *nexuses = NULL;
static cover_tagging_t *cover = NULL;
static unsigned         highest_rank;

static rt_alloc_stack_t event_stack = NULL;
static rt_alloc_stack_t waveform_stack = NULL;
static rt_alloc_stack_t sens_list_stack = NULL;
static rt_alloc_stack_t watch_stack = NULL;
static rt_alloc_stack_t callback_stack = NULL;

static void deltaq_insert_proc(uint64_t delta, rt_proc_t *wake);
static void deltaq_insert_driver(uint64_t delta, rt_nexus_t *nexus,
                                 rt_source_t *source);
static void rt_sched_driver(rt_nexus_t *nexus, uint64_t after,
                            uint64_t reject, value_t *values);
static void rt_sched_event(sens_list_t **list, rt_proc_t *proc, bool is_static);
static void *rt_tmp_alloc(size_t sz);
static value_t *rt_alloc_value(rt_nexus_t *n);
static res_memo_t *rt_memo_resolution_fn(rt_signal_t *signal,
                                         rt_resolution_t *resolution);
static inline unsigned rt_signal_nexus_index(rt_signal_t *s, unsigned offset);
static void _tracef(const char *fmt, ...);

#define GLOBAL_TMP_STACK_SZ (1024 * 1024)
#define PROC_TMP_STACK_SZ   (64 * 1024)
#define FMT_VALUES_SZ       128

#if RT_DEBUG
#define RT_ASSERT(x) assert((x))
#else
#define RT_ASSERT(x)
#endif

// Macro to generate the correct calling convention for LLVM by-value
// uarray aggregates
#define EXPLODED_UARRAY(name) \
   void *name##_ptr, int32_t name##_left, int32_t name##_length

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

static char *fmt_nexus_r(rt_nexus_t *n, const void *values,
                         char *buf, size_t max)
{
   char *p = buf;
   const uint8_t *vptr = values;

   for (unsigned i = 0; i < n->size * n->width; i++) {
      if (buf + max - p <= 5)
         return p + checked_sprintf(p, buf + max - p, "...");
      else
         p += checked_sprintf(p, buf + max - p, "%02x", *vptr++);
   }

   return p;
}

static const char *fmt_nexus(rt_nexus_t *n, const void *values)
{
   static char buf[FMT_VALUES_SZ*2 + 2];
   fmt_nexus_r(n, values, buf, sizeof(buf));
   return buf;
}

static const char *fmt_values(rt_signal_t *s, const void *values,
                              unsigned offset, uint32_t len)
{
   static char buf[FMT_VALUES_SZ*2 + 2];

   char *p = buf;
   const uint8_t *vptr = values;
   unsigned index = rt_signal_nexus_index(s, offset);
   while (len > 0 && buf + sizeof(buf) - p > 5)  {
      RT_ASSERT(index < s->n_nexus);
      rt_nexus_t *n = s->nexus[index++];
      len -= n->width;
      RT_ASSERT(len >= 0);

      if (p > buf) p += checked_sprintf(p, buf + sizeof(buf) - p, ",");

      p = fmt_nexus_r(n, vptr, p, buf + sizeof(buf) - p);
      vptr += n->size * n->width;
   }

   return buf;
}

static tree_t rt_find_enclosing_decl(ident_t unit_name, const char *symbol)
{
   tree_t unit = lib_get_qualified(unit_name);
   if (unit == NULL)
      return NULL;

   if (tree_kind(unit) == T_PACKAGE) {
      ident_t body_name = ident_prefix(unit_name, ident_new("body"), '-');
      tree_t body = lib_get_qualified(body_name);
      if (body != NULL)
         unit = body;
   }

   static hash_t *cache = NULL;
   if (cache == NULL)
      cache = hash_new(256, true);

   ident_t key = ident_new(symbol);
   tree_t enclosing = hash_get(cache, key);
   if (enclosing == NULL) {
      if ((enclosing = find_mangled_decl(unit, key)))
         hash_put(cache, key, enclosing);
   }

   return enclosing;
}

static text_buf_t *rt_fmt_trace(const rt_loc_t *fixed)
{
   debug_info_t *di = debug_capture();
   text_buf_t *tb = tb_new();

   bool found_fixed = false;
   const int nframes = debug_count_frames(di);
   for (int i = 0; i < nframes; i++) {
      const debug_frame_t *f = debug_get_frame(di, i);
      if (f->kind != FRAME_VHDL || f->vhdl_unit == NULL || f->symbol == NULL)
         continue;

      for (debug_inline_t *inl = f->inlined; inl != NULL; inl = inl->next) {
         tree_t enclosing = rt_find_enclosing_decl(inl->vhdl_unit, inl->symbol);
         if (enclosing == NULL)
            continue;

         found_fixed = true;  // DWARF data should be most accurate

         // Processes should never be inlined
         assert(tree_kind(enclosing) != T_PROCESS);

         type_t type = tree_type(enclosing);
         tb_printf(tb, "\r\tInlined %s %s",
                   type_kind(type) == T_FUNC ? "function" : "procedure",
                   type_pp(type));
         tb_printf(tb, "\r\t    File %s, Line %u", inl->srcfile, inl->lineno);
      }

      tree_t enclosing = rt_find_enclosing_decl(f->vhdl_unit, f->symbol);
      if (enclosing == NULL)
         continue;

      unsigned lineno = f->lineno;
      const char *srcfile = f->srcfile;
      if (fixed != NULL && !found_fixed) {
         lineno = fixed->first_line;
         srcfile = fixed->file;
         found_fixed = true;
      }
      else if (f->lineno == 0) {
         // Exact DWARF debug info not available
         const loc_t *loc = tree_loc(enclosing);
         lineno = loc->first_line;
         srcfile = loc_file_str(loc);
      }

      if (tree_kind(enclosing) == T_PROCESS)
         tb_printf(tb, "\r\tProcess %s", istr(e_path(active_proc->source)));
      else if (is_subprogram(enclosing)) {
         type_t type = tree_type(enclosing);
         tb_printf(tb, "\r\t%s %s",
                   type_kind(type) == T_FUNC ? "Function" : "Procedure",
                   type_pp(type));
      }

      tb_printf(tb, "\r\t    File %s, Line %u", srcfile, lineno);
   }

   if (fixed != NULL && (nframes == 0 || !found_fixed)) {
      const char *pname = active_proc == NULL
         ? "(init)" : istr(e_path(active_proc->source));
      tb_printf(tb, "\r\tProcess %s", pname);
      tb_printf(tb, "\r\t    File %s, Line %u", fixed->file, fixed->first_line);
   }

   debug_free(di);
   return tb;
}

typedef void (*rt_msg_fn_t)(const char *, ...);

__attribute__((format(printf, 3, 4)))
static void rt_msg(const rt_loc_t *where, rt_msg_fn_t fn, const char *fmt, ...)
{
   va_list ap;
   va_start(ap, fmt);

   char *LOCAL buf = xvasprintf(fmt, ap);
   LOCAL_TEXT_BUF trace = rt_fmt_trace(where);

   va_end(ap);

   (*fn)("%s%s", buf, tb_get(trace));
}

static size_t uarray_len(const uarray_t *u)
{
   return abs(u->dims[0].length);
}

static uarray_t wrap_str(char *buf, size_t len)
{
   uarray_t u = {
      .ptr = buf,
      .dims = { [0] = { .left = 1, .length = len } }
   };
   return u;
}

static uarray_t bit_vec_to_string(const uarray_t *vec, int log_base)
{
   const size_t vec_len = uarray_len(vec);
   const size_t result_len = (vec_len + log_base - 1) / log_base;
   const int left_pad = (log_base - (vec_len % log_base)) % log_base;
   char *buf = rt_tmp_alloc(result_len);

   for (int i = 0; i < result_len; i++) {
      unsigned nibble = 0;
      for (int j = 0; j < log_base; j++) {
         if (i > 0 || j >= left_pad) {
            nibble <<= 1;
            nibble |= !!(((uint8_t *)vec->ptr)[i*log_base + j - left_pad]);
         }
      }

      static const char map[16] = "0123456789ABCDEF";
      buf[i] = map[nibble];
   }

   return wrap_str(buf, result_len);
}

static unsigned rt_signal_nexus_index(rt_signal_t *s, unsigned offset)
{
   unsigned nid = 0;

   switch (s->nmap_kind) {
   case NEXUS_MAP_SEARCH:
      while (offset > 0) {
         RT_ASSERT(nid < s->n_nexus);
         rt_nexus_t *n = s->nexus[nid++];
         offset -= n->width * n->size;
      }
      RT_ASSERT(offset == 0);
      break;

   case NEXUS_MAP_DIVIDE:
      nid = offset / s->nmap_param;
      break;

   case NEXUS_MAP_DIRECT:
      nid = offset;
      break;
   }

   RT_ASSERT(nid < s->n_nexus);
   return nid;
}

static int rt_fmt_now(char *buf, size_t len)
{
   if (iteration < 0)
      return checked_sprintf(buf, len, "(init)");
   else {
      char *p = buf;
      p += fmt_time_r(p, buf + len - p, now);
      p += checked_sprintf(p, buf + len - p, "+%d", iteration);
      return p - buf;
   }
}

static void rt_unref_closure(rt_closure_t *closure)
{
   assert(closure->refcnt > 0);
   if (--(closure->refcnt) == 0)
      free(closure);
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
void _sched_waveform_s(sig_shared_t *ss, uint32_t offset, uint64_t scalar,
                       int64_t after, int64_t reject)
{
   rt_signal_t *s = container_of(ss, rt_signal_t, shared);

   TRACE("_sched_waveform_s %s+%d value=%"PRIi64" after=%s reject=%s",
         istr(e_path(s->enode)), offset, scalar, fmt_time(after),
         fmt_time(reject));

   if (unlikely(active_proc->postponed && (after == 0)))
      fatal("postponed process %s cannot cause a delta cycle",
            istr(e_path(active_proc->source)));

   rt_nexus_t *n = s->nexus[rt_signal_nexus_index(s, offset)];

   value_t *values_copy = rt_alloc_value(n);
   values_copy->qwords[0] = scalar;

   rt_sched_driver(n, after, reject, values_copy);
}

DLLEXPORT
void _sched_waveform(sig_shared_t *ss, uint32_t offset, void *values,
                     int32_t len, int64_t after, int64_t reject)
{
   rt_signal_t *s = container_of(ss, rt_signal_t, shared);

   TRACE("_sched_waveform %s+%d value=%s len=%d after=%s reject=%s",
         istr(e_path(s->enode)), offset, fmt_values(s, values, offset, len),
         len, fmt_time(after), fmt_time(reject));

   if (unlikely(active_proc->postponed && (after == 0)))
      fatal("postponed process %s cannot cause a delta cycle",
            istr(e_path(active_proc->source)));

   char *vptr = values;
   unsigned index = rt_signal_nexus_index(s, offset);
   while (len > 0) {
      RT_ASSERT(index < s->n_nexus);
      rt_nexus_t *n = s->nexus[index++];
      len -= n->width;
      RT_ASSERT(len >= 0);

      const size_t valuesz = n->width * n->size;
      value_t *values_copy = rt_alloc_value(n);
      memcpy(values_copy->data, vptr, valuesz);
      vptr += valuesz;

      rt_sched_driver(n, after, reject, values_copy);
   }
}

DLLEXPORT
void _sched_event(sig_shared_t *ss, uint32_t offset, int32_t count,
                  int32_t flags)
{
   rt_signal_t *s = container_of(ss, rt_signal_t, shared);

   TRACE("_sched_event %s+%d count=%d flags=%d proc %s", istr(e_path(s->enode)),
         offset, count, flags, istr(e_path(active_proc->source)));

   unsigned index = rt_signal_nexus_index(s, offset);
   while (count > 0) {
      RT_ASSERT(index < s->n_nexus);
      rt_nexus_t *n = s->nexus[index++];
      rt_sched_event(&(n->pending), active_proc, !!(flags & SCHED_STATIC));

      count -= n->width;
      RT_ASSERT(count >= 0);
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
                                    istr(e_path(active_proc->source)));
   }

   active_proc->tmp_alloc = _tmp_alloc;
}

DLLEXPORT
sig_shared_t *_link_signal(const char *name)
{
   ident_t id = ident_new(name);
   rt_scope_t *search_scope = active_scope;

   const bool is_path = name[0] == ':';

   if (is_path) {
      ident_t scope_path = ident_runtil(id, ':');
      for (unsigned i = 0; i < n_scopes; i++) {
         if (e_path(scopes[i].enode) == scope_path) {
            search_scope = &(scopes[i]);
            break;
         }
      }
   }

   for (unsigned i = 0; i < search_scope->n_signals; i++) {
      rt_signal_t *signal = &(search_scope->signals[i]);
      if (e_ident(signal->enode) == id)
         return &(signal->shared);
      else if (is_path && e_path(signal->enode) == id)
         return &(signal->shared);
   }

   fatal("failed to link signal %s in scope %s", name,
         istr(e_instance(search_scope->enode)));
}

DLLEXPORT
void _init_signal(sig_shared_t *ss, uint32_t offset, uint32_t count,
                  uint32_t size, const uint8_t *values,
                  rt_resolution_t *resolution)
{
   rt_signal_t *s = container_of(ss, rt_signal_t, shared);

   TRACE("_init_signal %s+%d values=%s count=%d%s",
         istr(e_path(s->enode)), offset, fmt_values(s, values, offset, count),
         count, resolution ? " resolved" : "");

   res_memo_t *memo = NULL;
   if (resolution != NULL)
      memo = rt_memo_resolution_fn(s, resolution);

   unsigned index = rt_signal_nexus_index(s, offset);
   while (count > 0) {
      RT_ASSERT(index < s->n_nexus);
      rt_nexus_t *n = s->nexus[index++];
      RT_ASSERT(n->size == size);

      if (s == n->signals[0]) {
         RT_ASSERT(n->resolution == NULL || n->resolution == memo);
         n->resolution = memo;

         memcpy(n->resolved, values, n->size * n->width);
         if (n->flags & NET_F_LAST_VALUE)
            memcpy(n->last_value, values, n->size * n->width);
      }

      count -= n->width;
      values += n->width * n->size;
      RT_ASSERT(count >= 0);
   }
}

DLLEXPORT
void _convert_signal(sig_shared_t *ss, uint32_t offset, uint32_t count,
                     rt_closure_t *closure)
{
   rt_signal_t *s = container_of(ss, rt_signal_t, shared);

   TRACE("_convert_signal %s+%d count=%d fn=%p context=%p",
         istr(e_path(s->enode)), offset, count, closure->fn, closure->context);

   rt_closure_t *copy = xmalloc(sizeof(rt_closure_t));
   *copy = *closure;
   copy->refcnt = 1;

   unsigned index = rt_signal_nexus_index(s, offset);
   while (count > 0) {
      RT_ASSERT(index < s->n_nexus);
      rt_nexus_t *n = s->nexus[index++];

      for (unsigned i = 0; i < n->n_sources; i++) {
         if (n->sources[i].proc == NULL) {  // Is a port source
            (copy->refcnt)++;
            n->sources[i].conv_func = copy;
         }
      }

      count -= n->width;
      RT_ASSERT(count >= 0);
   }

   rt_unref_closure(copy);
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

   RT_ASSERT(severity <= SEVERITY_FAILURE);

   const char *levels[] = {
      "Note", "Warning", "Error", "Failure"
   };

   if (init_side_effect != SIDE_EFFECT_ALLOW) {
      init_side_effect = SIDE_EFFECT_OCCURRED;
      return;
   }

   void (*fn)(const char *fmt, ...) = fatal;

   switch (severity) {
   case SEVERITY_NOTE:    fn = notef; break;
   case SEVERITY_WARNING: fn = warnf; break;
   case SEVERITY_ERROR:
   case SEVERITY_FAILURE: fn = errorf; break;
   }

   if (severity >= exit_severity)
      fn = fatal;

   char tmbuf[64];
   rt_fmt_now(tmbuf, sizeof(tmbuf));

   rt_msg(where, fn, "%s: %s %s: %.*s",
          tmbuf, (is_report ? "Report" : "Assertion"),
          levels[severity], msg_len, msg);
}

DLLEXPORT
void _bounds_fail(int32_t value, int32_t min, int32_t max, int32_t kind,
                  rt_loc_t *where, const char *hint)
{
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
      rt_msg(where, fatal, "array index %d outside bounds %d to %d%s%s",
             value, min, max, spacer, suffix);
      break;
   case BOUNDS_ARRAY_DOWNTO:
      rt_msg(where, fatal, "array index %d outside bounds %d downto %d%s%s",
             value, max, min, spacer, suffix);
      break;

   case BOUNDS_ENUM:
      rt_msg(where, fatal, "value %d outside %s bounds %d to %d%s%s",
             value, prefix, min, max, spacer, suffix);
      break;

   case BOUNDS_TYPE_TO:
      rt_msg(where, fatal, "value %d outside bounds %d to %d%s%s",
             value, min, max, spacer, suffix);
      break;

   case BOUNDS_TYPE_DOWNTO:
      rt_msg(where, fatal, "value %d outside bounds %d downto %d%s%s",
             value, max, min, spacer, suffix);
      break;

   case BOUNDS_ARRAY_SIZE:
      rt_msg(where, fatal, "length of target %d%s does not match length of "
             "value %d%s%s", min, prefix, max, spacer, suffix);
      break;

   case BOUNDS_PARAM_SIZE:
      rt_msg(where, fatal, "actual length %d%s does not match formal length "
             "%d%s%s", max, prefix, min, spacer, suffix);
      break;

   case BOUNDS_INDEX_TO:
      rt_msg(where, fatal, "index %d violates constraint bounds %d to %d",
             value, min, max);
      break;

   case BOUNDS_INDEX_DOWNTO:
      rt_msg(where, fatal, "index %d violates constraint bounds %d downto %d",
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

         if (num_digits == 0)
            rt_msg(where, fatal, "invalid integer value "
                   "\"%.*s\"", str_len, (const char *)raw_str);
      }
      break;

   case IMAGE_REAL:
      rt_msg(where, fatal, "real values not yet supported in 'VALUE");
      break;

   case IMAGE_PHYSICAL:
      rt_msg(where, fatal, "physical values not yet supported in 'VALUE");
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

      if (value < 0)
         rt_msg(where, fatal, "\"%.*s\" is not a valid enumeration value",
                str_len, (const char *)raw_str);
      break;
   }

   while (p < endp && *p != '\0') {
      if (!isspace((int)*p)) {
         rt_msg(where, fatal, "found invalid characters \"%.*s\" after value "
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
   rt_msg(where, fatal, "division by zero");
}

DLLEXPORT
void _null_deref(const rt_loc_t *where)
{
   rt_msg(where, fatal, "null access dereference");
}

DLLEXPORT
bool _nvc_ieee_warnings(void)
{
   return opt_get_int("ieee-warnings");
}

DLLEXPORT
int64_t _std_standard_now(void)
{
   return now;
}

DLLEXPORT
void _std_to_string_time(int64_t value, int64_t unit, uarray_t *u)
{
   const char *unit_str = "";
   switch (unit) {
   case 1ll: unit_str = "fs"; break;
   case 1000ll: unit_str = "ps"; break;
   case 1000000ll: unit_str = "ns"; break;
   case 1000000000ll: unit_str = "us"; break;
   case 1000000000000ll: unit_str = "ms"; break;
   case 1000000000000000ll: unit_str = "sec"; break;
   case 60000000000000000ll: unit_str = "min"; break;
   case 3600000000000000000ll: unit_str = "hr"; break;
   default:
      rt_msg(NULL, fatal, "invalid UNIT argument %"PRIi64" in TO_STRING", unit);
   }

   size_t max_len = 16 + strlen(unit_str) + 1;
   char *buf = rt_tmp_alloc(max_len);
   size_t len = checked_sprintf(buf, max_len, "%"PRIi64" %s",
                                value / unit, unit_str);

   *u = wrap_str(buf, len);
}

DLLEXPORT
void _std_to_string_real_digits(double value, int32_t digits, uarray_t *u)
{
   size_t max_len = 32;
   char *buf = rt_tmp_alloc(max_len);

   size_t len;
   if (digits == 0)
      len = checked_sprintf(buf, max_len, "%.17g", value);
   else
      len = checked_sprintf(buf, max_len, "%.*f", digits, value);

   *u = wrap_str(buf, len);
}

DLLEXPORT
void _std_to_string_real_format(double value, EXPLODED_UARRAY(fmt), uarray_t *u)
{
   char *LOCAL fmt_cstr = xmalloc(fmt_length + 1);
   memcpy(fmt_cstr, fmt_ptr, fmt_length);
   fmt_cstr[fmt_length] = '\0';

   if (fmt_cstr[0] != '%')
      rt_msg(NULL, fatal, "conversion specification must start with '%%'");

   for (const char *p = fmt_cstr + 1; *p; p++) {
      switch (*p) {
      case 'e': case 'E': case 'f': case 'F': case 'g': case 'G':
      case 'a': case 'A':
         continue;
      case '0'...'9':
         continue;
      case '.': case '-':
         continue;
      default:
         rt_msg(NULL, fatal, "illegal character '%c' in format \"%s\"",
                *p, fmt_cstr + 1);
      }
   }

   size_t max_len = 64;
   char *buf = rt_tmp_alloc(max_len);
   size_t len = checked_sprintf(buf, max_len, fmt_cstr, value);
   *u = wrap_str(buf, len);
}

DLLEXPORT
void _std_to_hstring_bit_vec(EXPLODED_UARRAY(vec), uarray_t *u)
{
   const uarray_t vec = { vec_ptr, { { vec_left, vec_length } } };
   *u = bit_vec_to_string(&vec, 4);
}

DLLEXPORT
void _std_to_ostring_bit_vec(EXPLODED_UARRAY(vec), uarray_t *u)
{
   const uarray_t vec = { vec_ptr, { { vec_left, vec_length } } };
   *u = bit_vec_to_string(&vec, 3);
}

DLLEXPORT
void _std_env_stop(int32_t finish, int32_t have_status, int32_t status)
{
   if (have_status)
      notef("%s called with status %d", finish ? "FINISH" : "STOP", status);
   else
      notef("%s called", finish ? "FINISH" : "STOP");

   exit(status);
}

DLLEXPORT
void _image(int64_t val, image_map_t *map, uarray_t *u)
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
         len = checked_sprintf(buf, 32, "%.*g", 17, u.d);
      }
      break;

   case IMAGE_PHYSICAL:
      buf = rt_tmp_alloc(20 + map->stride);
      len = checked_sprintf(buf, 20 + map->stride, "%"PRIi64" %s",
                            val, map->elems + (0 * map->stride));
      break;
   }

   *u = wrap_str(buf, len);
}

DLLEXPORT
void _bit_shift(int32_t kind, const uint8_t *data, int32_t len,
                int8_t dir, int32_t shift, uarray_t *u)
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
   u->dims[0].left = (dir == RANGE_TO) ? 0 : len - 1;
   u->dims[0].length = (dir == RANGE_TO) ? len : -len;
}

DLLEXPORT
void _bit_vec_op(int32_t kind, const uint8_t *left, int32_t left_len,
                 int8_t left_dir, const uint8_t *right, int32_t right_len,
                 int8_t right_dir, uarray_t *u)
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
   u->dims[0].left = (left_dir == RANGE_TO) ? 0 : left_len - 1;
   u->dims[0].length = (left_dir == RANGE_TO) ? left_len : -left_len;
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
int64_t _last_event(sig_shared_t *ss, uint32_t offset, int32_t count)
{
   rt_signal_t *s = container_of(ss, rt_signal_t, shared);

   TRACE("_last_event %s offset=%d count=%d",
         istr(e_path(s->enode)), offset, count);

   int64_t last = INT64_MAX;

   unsigned index = rt_signal_nexus_index(s, offset);
   while (count > 0) {
      RT_ASSERT(index < s->n_nexus);
      rt_nexus_t *n = s->nexus[index++];

      if (n->last_event < now)
         last = MIN(last, now - n->last_event);

      count -= n->width;
      RT_ASSERT(count >= 0);
   }

   return last;
}

DLLEXPORT
int64_t _last_active(sig_shared_t *ss, uint32_t offset, int32_t count)
{
   rt_signal_t *s = container_of(ss, rt_signal_t, shared);

   TRACE("_last_active %s offset=%d count=%d",
         istr(e_path(s->enode)), offset, count);

   int64_t last = INT64_MAX;

   unsigned index = rt_signal_nexus_index(s, offset);
   while (count > 0) {
      RT_ASSERT(index < s->n_nexus);
      rt_nexus_t *n = s->nexus[index++];

      if (n->last_active < now)
         last = MIN(last, now - n->last_active);

      count -= n->width;
      RT_ASSERT(count >= 0);
   }

   return last;
}

DLLEXPORT
bool _driving(sig_shared_t *ss, uint32_t offset, int32_t count)
{
   rt_signal_t *s = container_of(ss, rt_signal_t, shared);

   TRACE("_driving %s offset=%d count=%d",
         istr(e_path(s->enode)), offset, count);

   int ntotal = 0, ndriving = 0;
   unsigned index = rt_signal_nexus_index(s, offset);
   while (count > 0) {
      RT_ASSERT(index < s->n_nexus);
      rt_nexus_t *n = s->nexus[index++];

      int driver;
      for (driver = 0; driver < n->n_sources; driver++) {
         if (likely(n->sources[driver].proc == active_proc))
            break;
      }

      if (driver != n->n_sources) ndriving++;
      ntotal++;

      count -= n->width;
      RT_ASSERT(count >= 0);
   }

   if (ndriving == 0)
      rt_msg(NULL, fatal, "process %s does not contain a driver for %s",
             istr(e_path(active_proc->source)), istr(e_ident(s->enode)));

   return ntotal == ndriving;
}

DLLEXPORT
void *_driving_value(sig_shared_t *ss, uint32_t offset, int32_t count)
{
   rt_signal_t *s = container_of(ss, rt_signal_t, shared);

   TRACE("_driving_value %s offset=%d count=%d",
         istr(e_path(s->enode)), offset, count);

   void *result = rt_tmp_alloc(s->size);

   uint8_t *p = result;
   unsigned index = rt_signal_nexus_index(s, offset);
   while (count > 0) {
      RT_ASSERT(index < s->n_nexus);
      rt_nexus_t *n = s->nexus[index++];

      int driver;
      for (driver = 0; driver < n->n_sources; driver++) {
         if (likely(n->sources[driver].proc == active_proc))
            break;
      }

      if (driver == n->n_sources)
         rt_msg(NULL, fatal, "process %s does not contain a driver for %s",
                istr(e_path(active_proc->source)), istr(e_ident(s->enode)));

      memcpy(p, n->sources[driver].waveforms->values->data, n->width * n->size);
      p += n->width * n->size;

      count -= n->width;
      RT_ASSERT(count >= 0);
   }

   return result;
}

DLLEXPORT
int32_t _test_net_active(sig_shared_t *ss, uint32_t offset, int32_t count)
{
   rt_signal_t *s = container_of(ss, rt_signal_t, shared);

   TRACE("_test_net_active %s offset=%d count=%d",
         istr(e_path(s->enode)), offset, count);

   unsigned index = rt_signal_nexus_index(s, offset);
   while (count > 0) {
      RT_ASSERT(index < s->n_nexus);
      rt_nexus_t *n = s->nexus[index++];

      if (n->last_active == now && n->active_delta == iteration)
         return 1;

      count -= n->width;
      RT_ASSERT(count >= 0);
   }

   return 0;
}

DLLEXPORT
int32_t _test_net_event(sig_shared_t *ss, uint32_t offset, int32_t count)
{
   rt_signal_t *s = container_of(ss, rt_signal_t, shared);

   TRACE("_test_net_event %s offset=%d count=%d",
         istr(e_path(s->enode)), offset, count);

   unsigned index = rt_signal_nexus_index(s, offset);
   while (count > 0) {
      RT_ASSERT(index < s->n_nexus);
      rt_nexus_t *n = s->nexus[index++];

      if (n->last_event == now && n->event_delta == iteration)
         return 1;

      count -= n->width;
      RT_ASSERT(count >= 0);
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
      "rb", "wb", "w+b"
   };
   RT_ASSERT(mode < ARRAY_LEN(mode_str));

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

   if (*fp == NULL)
      fatal("write to closed file");

   fwrite(data, 1, len, *fp);
}

DLLEXPORT
void _file_read(void **_fp, uint8_t *data, int32_t len, int32_t *out)
{
   FILE **fp = (FILE **)_fp;

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

__attribute__((format(printf, 1, 2)))
static void _tracef(const char *fmt, ...)
{
   va_list ap;
   va_start(ap, fmt);

   char buf[64];
   rt_fmt_now(buf, sizeof(buf));

   fprintf(stderr, "TRACE %s: ", buf);
   vfprintf(stderr, fmt, ap);
   fprintf(stderr, "\n");
   fflush(stderr);

   va_end(ap);
}

static void deltaq_insert(event_t *e)
{
   if (e->when == now) {
      event_t **chain = (e->kind == EVENT_DRIVER) ? &delta_driver : &delta_proc;
      e->delta_chain = *chain;
      *chain = e;
   }
   else {
      e->delta_chain = NULL;
      heap_insert(eventq_heap, e->when, e);
   }
}

static void deltaq_insert_proc(uint64_t delta, rt_proc_t *wake)
{
   event_t *e = rt_alloc(event_stack);
   e->when       = now + delta;
   e->kind       = EVENT_PROCESS;
   e->proc       = wake;
   e->wakeup_gen = wake->wakeup_gen;

   deltaq_insert(e);
}

static void deltaq_insert_driver(uint64_t delta, rt_nexus_t *nexus,
                                 rt_source_t *source)
{
   event_t *e = rt_alloc(event_stack);
   e->when       = now + delta;
   e->kind       = EVENT_DRIVER;
   e->nexus      = nexus;
   e->source     = source;
   e->wakeup_gen = UINT32_MAX;

   deltaq_insert(e);
}

#if TRACE_DELTAQ > 0
static void deltaq_walk(uint64_t key, void *user, void *context)
{
   event_t *e = user;

   fprintf(stderr, "%s\t", fmt_time(e->when));
   switch (e->kind) {
   case EVENT_DRIVER:
      fprintf(stderr, "driver\t %s\n", istr(e_ident(e->nexus->enode)));
      break;
   case EVENT_PROCESS:
      fprintf(stderr, "process\t %s%s\n", istr(e_path(e->proc->source)),
              (e->wakeup_gen == e->proc->wakeup_gen) ? "" : " (stale)");
      break;
   case EVENT_TIMEOUT:
      fprintf(stderr, "timeout\t %p %p\n", e->timeout_fn, e->timeout_user);
      break;
   }
}

static void deltaq_dump(void)
{
   for (event_t *e = delta_driver; e != NULL; e = e->delta_chain)
      fprintf(stderr, "delta\tdriver\t %s\n", istr(e_ident(e->nexus->enode)));

   for (event_t *e = delta_proc; e != NULL; e = e->delta_chain)
      fprintf(stderr, "delta\tprocess\t %s%s\n",
              istr(e_path(e->proc->source)),
              (e->wakeup_gen == e->proc->wakeup_gen) ? "" : " (stale)");

   heap_walk(eventq_heap, deltaq_walk, NULL);
}
#endif

static res_memo_t *rt_memo_resolution_fn(rt_signal_t *signal,
                                         rt_resolution_t *resolution)
{
   // Optimise some common resolution functions by memoising them

   res_memo_t *memo = hash_get(res_memo_hash, resolution->fn);
   if (memo != NULL)
      return memo;

   memo = xmalloc(sizeof(res_memo_t));
   memo->fn      = resolution->fn;
   memo->context = resolution->context;
   memo->flags   = resolution->flags;
   memo->ileft   = resolution->ileft;

   hash_put(res_memo_hash, memo->fn, memo);

   if (resolution->nlits == 0 || resolution->nlits > 16)
      return memo;

   init_side_effect = SIDE_EFFECT_DISALLOW;

   // Memoise the function for all two value cases

   for (int i = 0; i < resolution->nlits; i++) {
      for (int j = 0; j < resolution->nlits; j++) {
         int8_t args[2] = { i, j };
         memo->tab2[i][j] = (*memo->fn)(memo->context, args, memo->ileft, 2);
         RT_ASSERT(memo->tab2[i][j] < resolution->nlits);
      }
   }

   // Memoise the function for all single value cases and determine if the
   // function behaves like the identity function

   bool identity = true;
   for (int i = 0; i < resolution->nlits; i++) {
      int8_t args[1] = { i };
      memo->tab1[i] = (*memo->fn)(memo->context, args, memo->ileft, 1);
      identity = identity && (memo->tab1[i] == i);
   }

   if (init_side_effect != SIDE_EFFECT_OCCURRED) {
      memo->flags |= R_MEMO;
      if (identity)
         memo->flags |= R_IDENT;
   }

   TRACE("memoised resolution function %p for type %s",
         resolution->fn, type_pp(e_type(signal->enode)));

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

static value_t *rt_alloc_value(rt_nexus_t *n)
{
   if (n->free_values == NULL) {
      const size_t size = MAX(sizeof(uint64_t), n->size * n->width);
      value_t *v = xmalloc(sizeof(struct value) + size);
      v->next = NULL;
      return v;
   }
   else {
      value_t *v = n->free_values;
      n->free_values = v->next;
      v->next = NULL;
      return v;
   }
}

static void rt_free_value(rt_nexus_t *n, value_t *v)
{
   RT_ASSERT(v->next == NULL);
   v->next = n->free_values;
   n->free_values = v;
}

static void *rt_tmp_alloc(size_t sz)
{
   // Allocate sz bytes that will be freed by the active process

   uint8_t *ptr = (uint8_t *)_tmp_stack + _tmp_alloc;
   _tmp_alloc += sz;
   return ptr;
}

static void rt_sched_event(sens_list_t **list, rt_proc_t *proc, bool is_static)
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
      node->reenq      = (is_static ? list : NULL);

      *list = node;
   }
   else {
      // Reuse the stale entry
      RT_ASSERT(!is_static);
      it->wakeup_gen = proc->wakeup_gen;
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

static void rt_free_delta_events(event_t *e)
{
   while (e != NULL) {
      event_t *tmp = e->delta_chain;
      rt_free(event_stack, e);
      e = tmp;
   }
}

static unsigned rt_count_scopes(e_node_t e)
{
   unsigned sum = 0;

   if (e_kind(e) == E_SCOPE)
      sum++;

   const int sub_scopes = e_scopes(e);
   for (int i = 0; i < sub_scopes; i++)
      sum += rt_count_scopes(e_scope(e, i));

   return sum;
}

static void rt_setup_signal(e_node_t e, rt_signal_t *s, unsigned *total_mem)
{
   s->enode   = e;
   s->width   = e_width(e);
   s->n_nexus = e_nexuses(e);
   s->nexus   = xmalloc_array(s->n_nexus, sizeof(rt_nexus_t*));
   s->flags   = 0;

   *total_mem += s->n_nexus * sizeof(rt_nexus_t*);

   const e_flags_t flags = e_flags(e);

   unsigned offset = 0, nmdivide = 0;
   for (int j = 0; j < s->n_nexus; j++) {
      rt_nexus_t *n = &(nexuses[e_pos(e_nexus(e, j))]);
      s->nexus[j] = n;

      unsigned o;
      for (o = 0; o < n->n_signals; o++) {
         if (e_signal(n->enode, o) == e)
            break;
      }

      if (o == n->n_signals)
         fatal_trace("signal %s missing in nexus %s", istr(e_path(e)),
                     istr(e_ident(n->enode)));

      assert(n->signals[o] == NULL);
      n->signals[o] = s;
      n->offsets[o] = offset;

      const unsigned bytes = n->width * n->size;

      if (j == 0)
         nmdivide = bytes;
      else if (nmdivide != bytes)
         nmdivide = 0;

      offset += bytes;

      if (flags & E_F_LAST_VALUE) {
         n->flags |= NET_F_LAST_VALUE;

         if (n->last_value == NULL)
            n->last_value = xcalloc_array(n->width, n->size);
      }
   }

   if (s->n_nexus == 1 || nmdivide == 1) {
      s->nmap_kind = NEXUS_MAP_DIRECT;
      profile.nmap_direct++;
   }
   else if (nmdivide == 0) {
      s->nmap_kind = NEXUS_MAP_SEARCH;
      profile.nmap_search++;
   }
   else {
      s->nmap_kind = NEXUS_MAP_DIVIDE;
      s->nmap_param = nmdivide;
      profile.nmap_divide++;
   }

   s->size = offset;

   profile.n_signals++;

   if (flags & E_F_CONTIGUOUS) {
      s->shared.resolved = s->nexus[0]->resolved;
      profile.n_contig++;
   }
   else {
      s->shared.resolved = xcalloc(s->size);
      s->flags |= NET_F_OWNS_MEM;
   }

   if (flags & E_F_LAST_VALUE) {
      s->shared.last_value = xcalloc(s->size);
      s->flags |= NET_F_LAST_VALUE;
   }
}

static void rt_setup_scopes_recur(e_node_t e, rt_scope_t *parent,
                                  unsigned *next_scope,
                                  unsigned *total_mem)
{
   rt_scope_t *scope = NULL;

   if (e_kind(e) == E_SCOPE) {
      const int nsignals = e_signals(e);
      const int nprocs = e_procs(e);

      scope = &(scopes[(*next_scope)++]);
      scope->enode = e;
      scope->parent = parent;

      scope->n_procs = nprocs;
      scope->procs = xcalloc_array(nprocs, sizeof(rt_proc_t));

      scope->n_signals = nsignals;
      scope->signals = xcalloc_array(nsignals, sizeof(rt_signal_t));

      *total_mem += nsignals * sizeof(rt_signal_t) + nprocs * sizeof(rt_proc_t);

      for (int i = 0; i < nprocs; i++) {
         e_node_t p = e_proc(e, i);

         rt_proc_t *r = &(scope->procs[i]);
         r->source     = p;
         r->proc_fn    = jit_find_symbol(istr(e_vcode(p)), true);
         r->wakeup_gen = 0;
         r->postponed  = !!(e_flags(p) & E_F_POSTPONED);
         r->tmp_stack  = NULL;
         r->tmp_alloc  = 0;
         r->pending    = false;
         r->usage      = 0;
         r->scope      = scope;

         const int nnexus = e_nexuses(p);
         for (int j = 0; j < nnexus; j++) {
            rt_nexus_t *n = &(nexuses[e_pos(e_nexus(p, j))]);
            for (unsigned k = 0; k < n->n_sources; k++) {
               if (e_source(n->enode, k) == p)
                  n->sources[k].proc = r;
            }
         }

         profile.n_procs++;
      }

      for (int i = 0; i < nsignals; i++)
         rt_setup_signal(e_signal(e, i), &(scope->signals[i]), total_mem);
   }

   const int nscopes = e_scopes(e);
   for (int i = 0; i < nscopes; i++)
      rt_setup_scopes_recur(e_scope(e, i), scope, next_scope, total_mem);
}

static void rt_setup_scopes(e_node_t e)
{
   n_scopes = rt_count_scopes(e);
   scopes  = xcalloc_array(n_scopes, sizeof(rt_scope_t));

   unsigned total_mem = n_scopes * sizeof(rt_scope_t);

   unsigned next_scope = 0;
   rt_setup_scopes_recur(e, NULL, &next_scope, &total_mem);
   assert(next_scope == n_scopes);

   TRACE("allocated %u bytes for %d scopes", total_mem, n_scopes);
}

static void rt_setup_nexus(e_node_t top)
{
   assert(nexuses == NULL);

   // TODO: how to optimise this for cache locality?

   n_nexuses = e_nexuses(top);
   nexuses = xcalloc_array(n_nexuses, sizeof(rt_nexus_t));
   unsigned total_mem = n_nexuses * sizeof(rt_nexus_t);

   size_t resolved_size = 0;
   for (int i = 0; i < n_nexuses; i++) {
      rt_nexus_t *n = &(nexuses[i]);
      e_node_t e = e_nexus(top, i);
      n->enode     = e;
      n->width     = e_width(e);
      n->size      = e_size(e);
      n->n_sources = e_sources(e);
      n->n_outputs = e_outputs(e);
      n->n_signals = e_signals(e);

      if (n->n_sources > 0) {
         n->sources = xcalloc_array(n->n_sources, sizeof(rt_source_t));
         total_mem += n->n_sources * sizeof(rt_source_t);
      }

      if (n->n_outputs > 0) {
         n->outputs = xcalloc_array(n->n_outputs, sizeof(rt_source_t *));
         total_mem += n->n_sources * sizeof(rt_source_t *);
      }

      for (unsigned i = 0; i < n->n_sources; i++) {
         waveform_t *w = rt_alloc(waveform_stack);
         w->when   = 0;
         w->next   = NULL;
         w->values = rt_alloc_value(n);

         n->sources[i].waveforms = w;
         n->sources[i].output = n;
      }

      if (n->n_signals > 0) {
         n->signals = xcalloc_array(n->n_signals, sizeof(rt_signal_t *));
         n->offsets = xcalloc_array(n->n_signals, sizeof(unsigned));

         total_mem += n->n_signals * (sizeof(rt_signal_t *) + sizeof(unsigned));
      }

      resolved_size += n->width * n->size;
      profile.n_simple += n->width;
   }

   // Allocate memory for all nexuses as one contiguous blob. This is
   // important so that the common case of signals consisting only of
   // contiguous nexuses do not need a private copy of the resolved
   // value.
   uint8_t *resolved_mem = NULL;
   if (resolved_size > 0) resolved_mem = xcalloc(resolved_size);
   total_mem += resolved_size;

   highest_rank = 0;

   uint8_t *nextp = resolved_mem;
   for (int i = 0; i < n_nexuses; i++) {
      rt_nexus_t *n = &(nexuses[i]);
      if (i == 0) n->flags |= NET_F_OWNS_MEM;
      n->resolved = nextp;
      nextp += n->width * n->size;

      // Attach port outputs to sources
      for (int j = 0; j < n->n_outputs; j++) {
         e_node_t p = e_output(n->enode, j);
         assert(e_nexus(p, 0) == n->enode);
         rt_nexus_t *to = &(nexuses[e_pos(e_nexus(p, 1))]);

         int to_src_id = 0;
         for (; to_src_id < to->n_sources; to_src_id++) {
            if (e_source(to->enode, to_src_id) == p)
               break;
         }
         assert(to_src_id != to->n_sources);

         n->outputs[j] = &(to->sources[to_src_id]);
         n->outputs[j]->input = n;

         if (n->outputs[j]->output->rank <= n->rank) {
            n->outputs[j]->output->rank = n->rank + 1;
            highest_rank = MAX(n->rank + 1, highest_rank);
         }
      }
   }

   // Calculate the rank of each nexus so signals can be updated in the
   // correct order
   if (highest_rank > 0) {
      bool made_changes;
      do {
         made_changes = false;
         for (int i = 0; i < n_nexuses; i++) {
            rt_nexus_t *n = &(nexuses[i]);
            for (int j = 0; j < n->n_outputs; j++) {
               if (n->outputs[j]->output->rank <= n->rank) {
                  n->outputs[j]->output->rank = n->rank + 1;
                  highest_rank = MAX(n->rank + 1, highest_rank);
                  made_changes = true;
               }
            }
         }
      } while (made_changes);

      TRACE("highest rank is %u", highest_rank);
   }

   TRACE("allocated %u bytes for %d nexuses", total_mem, n_nexuses);
}

static void rt_setup(e_node_t top)
{
   now = 0;
   iteration = -1;
   active_proc = NULL;
   active_scope = NULL;
   force_stop = false;
   can_create_delta = true;

   RT_ASSERT(resume == NULL);

   rt_free_delta_events(delta_proc);
   rt_free_delta_events(delta_driver);

   eventq_heap = heap_new(512);
   rankn_heap = heap_new(128);

   rt_setup_nexus(top);
   rt_setup_scopes(top);

   res_memo_hash = hash_new(128, true);
}

static void rt_run(rt_proc_t *proc)
{
   TRACE("%s process %s", proc->privdata ? "run" : "reset",
         istr(e_path(proc->source)));

   const bool reset = (proc->privdata == NULL);

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
   active_scope = proc->scope;

   if (unlikely(reset)) {
      proc->privdata = (*proc->proc_fn)(NULL, proc->scope->privdata);
      global_tmp_alloc = _tmp_alloc;
      RT_ASSERT(proc->privdata);
   }
   else
      (*proc->proc_fn)(proc->privdata, proc->scope->privdata);
}

static void *rt_call_module_reset(ident_t name, void *arg)
{
   char *buf LOCAL = xasprintf("%s_reset", istr(name));

   _tmp_stack = global_tmp_stack;
   _tmp_alloc = global_tmp_alloc;

   void *result = NULL;
   void *(*reset_fn)(void *) = jit_find_symbol(buf, false);
   if (reset_fn != NULL)
      result = (*reset_fn)(arg);
   else
      warnf("symbol %s not found", buf);

   global_tmp_alloc = _tmp_alloc;
   return result;
}

static inline void *rt_resolution_buffer(size_t required)
{
   static void *rbuf = NULL;
   static size_t size = 0;

   if (likely(size >= required))
      return rbuf;

   size = MAX(required, 16);
   return (rbuf = xrealloc(rbuf, size));
}

static void *rt_call_resolution_fn(rt_nexus_t *nexus)
{
   void *resolved = NULL;
   if (unlikely(nexus->flags & NET_F_FORCED)) {
      resolved = nexus->forcing->data;
   }
   else if (nexus->resolution == NULL && nexus->n_sources == 0) {
      // Always maintains initial driver value
      resolved = nexus->resolved;
   }
   else if (nexus->resolution == NULL) {
      resolved = nexus->sources[0].waveforms->values->data;
   }
   else if ((nexus->resolution->flags & R_IDENT) && (nexus->n_sources == 1)) {
      // Resolution function behaves like identity for a single driver
      resolved = nexus->sources[0].waveforms->values->data;
   }
   else if ((nexus->resolution->flags & R_MEMO) && (nexus->n_sources == 1)) {
      // Resolution function has been memoised so do a table lookup

      resolved = rt_resolution_buffer(nexus->width * nexus->size);

      for (int j = 0; j < nexus->width; j++) {
         const int index = nexus->sources[0].waveforms->values->data[j];
         const int8_t r = nexus->resolution->tab1[index];
         ((int8_t *)resolved)[j] = r;
      }
   }
   else if ((nexus->resolution->flags & R_MEMO) && (nexus->n_sources == 2)) {
      // Resolution function has been memoised so do a table lookup

      resolved = rt_resolution_buffer(nexus->width * nexus->size);

      const char *p0 = nexus->sources[0].waveforms->values->data;
      const char *p1 = nexus->sources[1].waveforms->values->data;

      for (int j = 0; j < nexus->width; j++) {
         const int driving[2] = { p0[j], p1[j] };
         const int8_t r = nexus->resolution->tab2[driving[0]][driving[1]];
         ((int8_t *)resolved)[j] = r;
      }
   }
   else if (nexus->resolution->flags & R_COMPOSITE) {
      // Call resolution function of composite type

      rt_signal_t *s0 = nexus->signals[0];
      uint8_t *inputs = rt_resolution_buffer(nexus->n_sources * s0->size);

      size_t offset = 0, result_offset = 0;
      for (unsigned i = 0; i < s0->n_nexus; i++) {
         rt_nexus_t *n = s0->nexus[i];

         for (unsigned j = 0; j < nexus->n_sources; j++) {
            const void *src = NULL;
            if (n == nexus) {
               result_offset = offset;
               src = n->sources[j].waveforms->values->data;
            }
            else
               src = n->resolved;

            memcpy(inputs + offset + (j * s0->size), src, n->size * n->width);
         }

         offset += n->size * n->width;
      }

      const int32_t left = nexus->resolution->ileft;
      const int32_t len  = nexus->n_sources;
      void *priv = nexus->resolution->context;
      uint8_t *result =
         (uint8_t *)(*nexus->resolution->fn)(priv, inputs, left, len);
      resolved = result + result_offset;
   }
   else {
      // Must actually call resolution function in general case

      resolved = rt_resolution_buffer(nexus->width * nexus->size);

      for (int j = 0; j < nexus->width; j++) {
#define CALL_RESOLUTION_FN(type) do {                                   \
            type vals[nexus->n_sources];                                \
            for (int i = 0; i < nexus->n_sources; i++) {                \
               const value_t *v = nexus->sources[i].waveforms->values;  \
               vals[i] = ((const type *)v->data)[j];                    \
            }                                                           \
            type *r = (type *)resolved;                                 \
            const int32_t left = nexus->resolution->ileft;              \
            const int32_t len  = nexus->n_sources;                      \
            void *priv = nexus->resolution->context;                    \
            r[j] = (*nexus->resolution->fn)(priv, vals, left, len);     \
         } while (0)

         FOR_ALL_SIZES(nexus->size, CALL_RESOLUTION_FN);
      }
   }

   return resolved;
}

static void rt_call_conversion_func(rt_source_t *source, const void *input)
{
   TRACE("call conversion function %p", source->conv_func->fn);

   rt_closure_t *c = source->conv_func;
   const size_t outsz = source->output->size * source->output->width;

   // This implicitly assumes little-endian representation

   if (c->spec.atype == RT_FFI_INT && c->spec.rtype == RT_FFI_INT) {
      int64_t (*fn)(void *, uint64_t) = c->fn;
      const int64_t r = (*fn)(c->context, *(uint64_t *)input);
      TRACE("integer result is %"PRIi64, r);
      RT_ASSERT(outsz <= sizeof(int64_t));
      memcpy(source->waveforms->values->data, &r, outsz);
   }
   else if (c->spec.atype == RT_FFI_FLOAT && c->spec.rtype == RT_FFI_INT) {
      int64_t (*fn)(void *, double) = c->fn;
      const int64_t r = (*fn)(c->context, *(double *)input);
      TRACE("integer result is %"PRIi64, r);
      RT_ASSERT(outsz <= sizeof(int64_t));
      memcpy(source->waveforms->values->data, &r, outsz);
   }
   else if (c->spec.atype == RT_FFI_INT && c->spec.rtype == RT_FFI_FLOAT) {
      double (*fn)(void *, uint64_t) = c->fn;
      const double r = (*fn)(c->context, *(uint64_t *)input);
      TRACE("float result is %f", r);
      RT_ASSERT(outsz == sizeof(double));
      memcpy(source->waveforms->values->data, &r, outsz);
   }
   else if (c->spec.atype == RT_FFI_FLOAT && c->spec.rtype == RT_FFI_FLOAT) {
      double (*fn)(void *, double) = c->fn;
      const double r = (*fn)(c->context, *(double *)input);
      TRACE("float result is %f", r);
      RT_ASSERT(outsz == sizeof(double));
      memcpy(source->waveforms->values->data, &r, outsz);
   }
   else if (c->spec.atype == RT_FFI_POINTER && c->spec.rtype == RT_FFI_INT) {
      rt_signal_t *s0 = source->input->signals[0];
      int64_t (*fn)(void *, void *) = c->fn;
      const int64_t r = (*fn)(c->context, s0->shared.resolved);
      TRACE("integer result is %"PRIi64, r);
      RT_ASSERT(outsz <= sizeof(int64_t));
      memcpy(source->waveforms->values->data, &r, outsz);
   }
   else if (c->spec.atype == RT_FFI_POINTER && c->spec.rtype == RT_FFI_INT) {
      rt_signal_t *s0 = source->input->signals[0];
      int64_t (*fn)(void *, void *) = c->fn;
      const int64_t r = (*fn)(c->context, s0->shared.resolved);
      TRACE("integer result is %"PRIi64, r);
      RT_ASSERT(outsz <= sizeof(int64_t));
      memcpy(source->waveforms->values->data, &r, outsz);
   }
   else if (c->spec.atype == RT_FFI_INT && c->spec.rtype == RT_FFI_POINTER) {
      void *(*fn)(void *, uint64_t) = c->fn;
      void *r = (*fn)(c->context, *(uint64_t *)input);
      memcpy(source->waveforms->values->data, r, outsz);
   }
   else
      fatal_trace("unhandled conversion function argument combination");
}

static void rt_propagate_nexus(rt_nexus_t *nexus, const void *resolved)
{
   const size_t valuesz = nexus->size * nexus->width;

   // LAST_VALUE is the same as the initial value when there have
   // been no events on the signal otherwise only update it when
   // there is an event
   if (nexus->flags & NET_F_LAST_VALUE)
      memcpy(nexus->last_value, nexus->resolved, valuesz);
   if (nexus->resolved != resolved)   // Can occur during startup
      memcpy(nexus->resolved, resolved, valuesz);

   for (unsigned i = 0; i < nexus->n_signals; i++) {
      rt_signal_t *s = nexus->signals[i];
      if (s->flags & NET_F_LAST_VALUE)
         memcpy(s->shared.last_value + nexus->offsets[i],
                nexus->last_value, valuesz);
      if (s->flags & NET_F_OWNS_MEM)
         memcpy(s->shared.resolved + nexus->offsets[i],
                nexus->resolved, valuesz);
   }
}

static void rt_update_inputs(rt_nexus_t *nexus)
{
   for (unsigned i = 0; i < nexus->n_sources; i++) {
      rt_source_t *s = &(nexus->sources[i]);
      if (s->proc != NULL)
         continue;
      else if (likely(s->conv_func == NULL)) {
         const size_t valuesz = s->input->size * s->input->width;
         memcpy(s->waveforms->values->data, s->input->resolved, valuesz);
      }
      else
         rt_call_conversion_func(s, s->input->resolved);
   }
}

static void rt_reset_scopes(e_node_t top)
{
   for (unsigned i = 0; i < n_scopes; i++) {
      rt_scope_t *s = &(scopes[i]);
      TRACE("reset scope %s", istr(e_path(s->enode)));

      void *privdata = s->parent ? s->parent->privdata : NULL;
      active_scope = s;

      s->privdata = rt_call_module_reset(e_vcode(s->enode), privdata);

   }
   active_scope = NULL;
}

static void rt_driver_initial(rt_nexus_t *nexus)
{
   const size_t valuesz = nexus->size * nexus->width;

   // Assign the initial value of the drivers
   for (unsigned i = 0; i < nexus->n_sources; i++) {
      rt_source_t *s = &(nexus->sources[i]);
      if (s->proc != NULL)  // Driver not port source
         memcpy(s->waveforms->values->data, nexus->resolved, valuesz);
   }

   rt_update_inputs(nexus);

   void *resolved;
   if (nexus->n_sources > 0) {
      resolved = rt_call_resolution_fn(nexus);
      nexus->event_delta = nexus->active_delta = -1;
      nexus->last_event = nexus->last_active = now;
   }
   else {
      resolved = nexus->resolved;
      nexus->event_delta = nexus->active_delta = -1;
      nexus->last_event = nexus->last_active = INT64_MAX;    // TIME'HIGH
   }

   TRACE("%s initial value %s", istr(e_ident(nexus->enode)),
         fmt_nexus(nexus, resolved));

   rt_propagate_nexus(nexus, resolved);
}

static void rt_initial(e_node_t top)
{
   // Initialisation is described in LRM 93 section 12.6.4

   rt_reset_scopes(top);

   for (unsigned i = 0; i < n_scopes; i++) {
      for (unsigned j = 0; j < scopes[i].n_procs; j++)
         rt_run(&(scopes[i].procs[j]));
   }

   TRACE("calculate initial driver values");

   init_side_effect = SIDE_EFFECT_ALLOW;

   for (int rank = 0; rank <= highest_rank; rank++) {
      for (unsigned i = 0; i < n_nexuses; i++) {
         if (nexuses[i].rank == rank)
            rt_driver_initial(&(nexuses[i]));
      }
   }

   TRACE("used %d bytes of global temporary stack", global_tmp_alloc);
}

static void rt_watch_signal(watch_t *w)
{
   for (unsigned i = 0; i < w->signal->n_nexus; i++) {
      rt_nexus_t *n = w->signal->nexus[i];

      watch_list_t *link = xmalloc(sizeof(watch_list_t));
      link->next  = n->watching;
      link->watch = w;

      n->watching = link;
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
      TRACE("wakeup process %s%s", istr(e_path(sl->proc->source)),
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

static void rt_sched_driver(rt_nexus_t *nexus, uint64_t after,
                            uint64_t reject, value_t *values)
{
   if (unlikely(reject > after))
      fatal("signal %s pulse reject limit %s is greater than "
            "delay %s", istr(e_path(nexus->signals[0]->enode)),
            fmt_time(reject), fmt_time(after));

   int driver = 0;
   if (unlikely(nexus->n_sources != 1)) {
      // Try to find this process in the list of existing drivers
      for (driver = 0; driver < nexus->n_sources; driver++) {
         if (likely(nexus->sources[driver].proc == active_proc))
            break;
      }

      RT_ASSERT(driver != nexus->n_sources);
   }

   rt_source_t *d = &(nexus->sources[driver]);

   const size_t valuesz = nexus->size * nexus->width;

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
         rt_free_value(nexus, it->values);
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
      rt_free_value(nexus, it->values);

      if (it->when == w->when)
         already_scheduled = true;

      waveform_t *next = it->next;
      rt_free(waveform_stack, it);
      it = next;
   }

   if (!already_scheduled)
      deltaq_insert_driver(after, nexus, d);
}

static void rt_notify_event(rt_nexus_t *nexus)
{
   sens_list_t *it = NULL, *next = NULL;

   nexus->last_event = nexus->last_active = now;
   nexus->event_delta = nexus->active_delta = iteration;

   // First wakeup everything on the nexus specific pending list
   for (it = nexus->pending; it != NULL; it = next) {
      next = it->next;
      rt_wakeup(it);
      nexus->pending = next;
   }

   for (unsigned i = 0; i < nexus->n_sources; i++) {
      rt_source_t *o = &(nexus->sources[i]);
      if (o->proc == NULL)
         rt_notify_event(o->input);
   }

   // Schedule any callbacks to run
   for (watch_list_t *wl = nexus->watching; wl != NULL; wl = wl->next) {
      if (!wl->watch->pending) {
         wl->watch->chain_pending = callbacks;
         wl->watch->pending = true;
         callbacks = wl->watch;
      }
   }
}

static void rt_notify_active(rt_nexus_t *nexus)
{
   nexus->last_active = now;
   nexus->active_delta = iteration;

   for (unsigned i = 0; i < nexus->n_sources; i++) {
      rt_source_t *o = &(nexus->sources[i]);
      if (o->proc == NULL)
         rt_notify_active(o->input);
   }
}

static void rt_update_nexus(rt_nexus_t *nexus)
{
   void *resolved = rt_call_resolution_fn(nexus);
   const size_t valuesz = nexus->size * nexus->width;

   nexus->last_active = now;
   nexus->active_delta = iteration;

   RT_ASSERT(nexus->flags & NET_F_PENDING);
   nexus->flags &= ~NET_F_PENDING;

   TRACE("update nexus %s resolved=%s", istr(e_ident(nexus->enode)),
         fmt_nexus(nexus, resolved));

   if (memcmp(nexus->resolved, resolved, valuesz) != 0) {
      rt_propagate_nexus(nexus, resolved);
      rt_notify_event(nexus);
   }
   else
      rt_notify_active(nexus);
}

static void rt_push_active_nexus(rt_nexus_t *nexus)
{
   if (nexus->flags & NET_F_PENDING)
      return;   // Already scheduled

   nexus->flags |= NET_F_PENDING;

   if (nexus->rank == 0 && nexus->n_sources == 1) {
      // This nexus does not depend on the values of any inputs or other
      // drivers so we can eagerly update its value now
      rt_update_nexus(nexus);
   }
   else
      heap_insert(rankn_heap, nexus->rank, nexus);

   for (unsigned i = 0; i < nexus->n_outputs; i++) {
      rt_source_t *o = nexus->outputs[i];
      TRACE("active nexus %s sources nexus %s", istr(e_ident(nexus->enode)),
            istr(e_ident(o->output->enode)));
      RT_ASSERT(nexus->rank < o->output->rank);
      rt_push_active_nexus(o->output);
   }
}

static void rt_update_driver(rt_nexus_t *nexus, rt_source_t *source)
{
   if (likely(source != NULL)) {
      waveform_t *w_now  = source->waveforms;
      waveform_t *w_next = w_now->next;

      if (likely((w_next != NULL) && (w_next->when == now))) {
         source->waveforms = w_next;
         rt_free_value(nexus, w_now->values);
         rt_free(waveform_stack, w_now);
         rt_push_active_nexus(nexus);
      }
      else
         RT_ASSERT(w_now != NULL);
   }
   else if (nexus->flags & NET_F_FORCED)
      rt_push_active_nexus(nexus);
}

static bool rt_stale_event(event_t *e)
{
   return (e->kind == EVENT_PROCESS) && (e->wakeup_gen != e->proc->wakeup_gen);
}

static void rt_push_run_queue(rt_run_queue_t *q, event_t *e)
{
   if (unlikely(q->wr == q->alloc)) {
      if (q->alloc == 0) {
         q->alloc = 128;
         q->queue = xmalloc_array(q->alloc, sizeof(event_t *));
      }
      else {
         q->alloc *= 2;
         q->queue = xrealloc_array(q->queue, q->alloc, sizeof(event_t *));
      }
   }

   if (unlikely(rt_stale_event(e)))
      rt_free(event_stack, e);
   else {
      q->queue[(q->wr)++] = e;
      if (e->kind == EVENT_PROCESS)
         ++(e->proc->wakeup_gen);
   }
}

static event_t *rt_pop_run_queue(rt_run_queue_t *q)
{
   if (q->wr == q->rd) {
      q->wr = 0;
      q->rd = 0;
      return NULL;
   }
   else
      return q->queue[(q->rd)++];
}

static void rt_iteration_limit(void)
{
   text_buf_t *buf = tb_new();
   tb_printf(buf, "Iteration limit of %d delta cycles reached. "
             "The following processes are active:\n",
             opt_get_int("stop-delta"));

   for (sens_list_t *it = resume; it != NULL; it = it->next) {
      const loc_t *l = e_loc(it->proc->source);
      tb_printf(buf, "  %-30s %s line %d\n", istr(e_path(it->proc->source)),
                loc_file_str(l), l->first_line);
   }

   tb_printf(buf, "You can increase this limit with --stop-delta");

   fatal("%s", tb_get(buf));
}

static void rt_resume_processes(sens_list_t **list)
{
   sens_list_t *it = *list;
   while (it != NULL) {
      if (it->proc->pending) {
         rt_run(it->proc);
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
         rt_push_run_queue(&driverq, e);

      for (event_t *e = delta_proc; e != NULL; e = e->delta_chain)
         rt_push_run_queue(&procq, e);

      delta_driver = NULL;
      delta_proc = NULL;
   }
   else {
      rt_global_event(RT_NEXT_TIME_STEP);

      for (;;) {
         event_t *e = heap_extract_min(eventq_heap);
         switch (e->kind) {
         case EVENT_PROCESS: rt_push_run_queue(&procq, e); break;
         case EVENT_DRIVER:  rt_push_run_queue(&driverq, e); break;
         case EVENT_TIMEOUT: rt_push_run_queue(&timeoutq, e); break;
         }

         if (heap_size(eventq_heap) == 0)
            break;

         event_t *peek = heap_min(eventq_heap);
         if (peek->when > now)
            break;
      }
   }

   if (profiling) {
      const uint32_t nevents = procq.wr + driverq.wr + timeoutq.wr;

      profile.deltas++;
      profile.runq_min = MIN(profile.runq_min, nevents);
      profile.runq_max = MAX(profile.runq_max, nevents);
      profile.runq_mean += (nevents - profile.runq_mean) / profile.deltas;
   }

   event_t *event;

   while ((event = rt_pop_run_queue(&timeoutq))) {
      (*event->timeout_fn)(now, event->timeout_user);
      rt_free(event_stack, event);
   }

   while ((event = rt_pop_run_queue(&driverq))) {
      rt_update_driver(event->nexus, event->source);
      rt_free(event_stack, event);
   }

   while (heap_size(rankn_heap) > 0) {
      rt_nexus_t *n = heap_extract_min(rankn_heap);
      rt_update_inputs(n);
      rt_update_nexus(n);
   }

   while ((event = rt_pop_run_queue(&procq))) {
      rt_run(event->proc);
      rt_free(event_stack, event);
   }

   if (unlikely((stop_delta > 0) && (iteration == stop_delta)))
      rt_iteration_limit();

   // Run all non-postponed event callbacks
   rt_event_callback(false);

   // Run all processes that resumed because of signal events
   rt_resume_processes(&resume);
   rt_global_event(RT_END_OF_PROCESSES);

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

static void rt_cleanup_nexus(rt_nexus_t *n)
{
   if (n->flags & NET_F_OWNS_MEM)
      free(n->resolved);
   if (n->flags & NET_F_LAST_VALUE)
      free(n->last_value);

   free(n->forcing);

   for (int j = 0; j < n->n_sources; j++) {
      while (n->sources[j].waveforms != NULL) {
         waveform_t *next = n->sources[j].waveforms->next;
         rt_free_value(n, n->sources[j].waveforms->values);
         rt_free(waveform_stack, n->sources[j].waveforms);
         n->sources[j].waveforms = next;
      }

      if (n->sources[j].conv_func != NULL)
         rt_unref_closure(n->sources[j].conv_func);
   }
   free(n->sources);

   free(n->outputs);
   free(n->signals);
   free(n->offsets);

   while (n->free_values != NULL) {
      value_t *next = n->free_values->next;
      free(n->free_values);
      n->free_values = next;
   }

   while (n->pending != NULL) {
      sens_list_t *next = n->pending->next;
      rt_free(sens_list_stack, n->pending);
      n->pending = next;
   }

   while (n->watching != NULL) {
      watch_list_t *next = n->watching->next;
      free(n->watching);
      n->watching = next;
   }
}

static void rt_cleanup_signal(rt_signal_t *s)
{
   if (s->flags & NET_F_OWNS_MEM)
      free(s->shared.resolved);

   if (s->flags & NET_F_LAST_VALUE)
      free(s->shared.last_value);

   free(s->nexus);
}

static void rt_cleanup_scope(rt_scope_t *scope)
{
   for (unsigned i = 0; i < scope->n_procs; i++)
      free(scope->procs[i].privdata);

   for (unsigned i = 0; i < scope->n_signals; i++)
      rt_cleanup_signal(&(scope->signals[i]));

   free(scope->privdata);
   free(scope->procs);
   free(scope->signals);
}

static void rt_cleanup(e_node_t top)
{
   RT_ASSERT(resume == NULL);

   while (heap_size(eventq_heap) > 0)
      rt_free(event_stack, heap_extract_min(eventq_heap));

   rt_free_delta_events(delta_proc);
   rt_free_delta_events(delta_driver);

   heap_free(eventq_heap);
   eventq_heap = NULL;

   heap_free(rankn_heap);
   rankn_heap = NULL;

   for (unsigned i = 0; i < n_nexuses; i++)
      rt_cleanup_nexus(&(nexuses[i]));

   free(nexuses);
   nexuses = NULL;

   for (unsigned i = 0; i < n_scopes; i++)
      rt_cleanup_scope(&(scopes[i]));

   free(scopes);
   scopes = NULL;

   while (watches != NULL) {
      watch_t *next = watches->chain_all;
      rt_free(watch_stack, watches);
      watches = next;
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

   if (profiling) {
      LOCAL_TEXT_BUF tb = tb_new();
      tb_printf(tb, "Signals: %d  (%.1f%% contiguous)\n", profile.n_signals,
                100.0f * ((float)profile.n_contig / profile.n_signals));
      tb_printf(tb, "Nexuses: %-5d      Simple signals: %d (1:%.1f)\n",
                n_nexuses, profile.n_simple,
                (double)profile.n_simple / n_nexuses);
      tb_printf(tb, "Mapping:  direct:%d search:%d divide:%d\n",
                profile.nmap_direct, profile.nmap_search, profile.nmap_divide);
      tb_printf(tb, "Processes: %-5d    Scopes: %d\n",
                profile.n_procs, n_scopes);
      tb_printf(tb, "Cycles: %"PRIu64"\n", profile.deltas);
      tb_printf(tb, "Run queue:   min:%d max:%d avg:%.2f\n",
                profile.runq_min, profile.runq_max, profile.runq_mean);

      notef("Simulation profile data:%s", tb_get(tb));
   }

   notef("setup:%ums run:%ums maxrss:%ukB", ready_rusage.ms, ru.ms, ru.rss);
}

static void rt_reset_coverage(tree_t top)
{
   assert(cover == NULL);

   if ((cover = cover_read_tags(top)) == NULL)
      return;

   int32_t n_stmts, n_conds;
   cover_count_tags(cover, &n_stmts, &n_conds);

   int32_t *cover_stmts = jit_find_symbol("cover_stmts", false);
   if (cover_stmts != NULL)
      memset(cover_stmts, '\0', sizeof(int32_t) * n_stmts);

   int32_t *cover_conds = jit_find_symbol("cover_conds", false);
   if (cover_conds != NULL)
      memset(cover_conds, '\0', sizeof(int32_t) * n_conds);
}

static void rt_emit_coverage(tree_t top)
{
   if (cover != NULL) {
      const int32_t *cover_stmts = jit_find_symbol("cover_stmts", false);
      const int32_t *cover_conds = jit_find_symbol("cover_conds", false);
      if (cover_stmts != NULL)
         cover_report(top, cover, cover_stmts, cover_conds);
   }
}

static void rt_interrupt(void)
{
   if (active_proc != NULL)
      rt_msg(NULL, fatal,
             "interrupted in process %s at %s+%d",
             istr(e_path(active_proc->source)), fmt_time(now), iteration);
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

void rt_start_of_tool(tree_t top, e_node_t e)
{
   jit_init(e);

#if RT_DEBUG
   warnf("runtime debug checks enabled");
#endif

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

   trace_on = opt_get_int("rt_trace_en");
   profiling = opt_get_int("rt_profile");

   if (profiling) {
      memset(&profile, '\0', sizeof(profile));
      profile.runq_min = ~0;
   }

   event_stack     = rt_alloc_stack_new(sizeof(event_t), "event");
   waveform_stack  = rt_alloc_stack_new(sizeof(waveform_t), "waveform");
   sens_list_stack = rt_alloc_stack_new(sizeof(sens_list_t), "sens_list");
   watch_stack     = rt_alloc_stack_new(sizeof(watch_t), "watch");
   callback_stack  = rt_alloc_stack_new(sizeof(callback_t), "callback");

   global_tmp_stack = mmap_guarded(GLOBAL_TMP_STACK_SZ, "global temp stack");
   proc_tmp_stack   = mmap_guarded(PROC_TMP_STACK_SZ, "process temp stack");

   global_tmp_alloc = 0;

   rt_reset_coverage(top);

   nvc_rusage(&ready_rusage);
}

void rt_end_of_tool(tree_t top, e_node_t e)
{
   rt_cleanup(e);
   rt_emit_coverage(top);

   jit_shutdown();

   if (opt_get_int("rt-stats") || profiling)
      rt_stats_print();
}

void rt_run_sim(uint64_t stop_time)
{
   const int stop_delta = opt_get_int("stop-delta");

   fst_restart();

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

void rt_restart(e_node_t top)
{
   rt_setup(top);
   rt_initial(top);
   aborted = false;
}

void rt_set_timeout_cb(uint64_t when, timeout_fn_t fn, void *user)
{
   event_t *e = rt_alloc(event_stack);
   e->when         = now + when;
   e->kind         = EVENT_TIMEOUT;
   e->nexus        = NULL;
   e->proc         = NULL;
   e->timeout_fn   = fn;
   e->timeout_user = user;
   e->wakeup_gen   = UINT32_MAX;

   deltaq_insert(e);
}

watch_t *rt_set_event_cb(rt_signal_t *s, sig_event_fn_t fn, void *user,
                         bool postponed)
{
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
      RT_ASSERT(w != NULL);
      w->signal        = s;
      w->fn            = fn;
      w->chain_all     = watches;
      w->chain_pending = NULL;
      w->pending       = false;
      w->user_data     = user;
      w->length        = 0;
      w->postponed     = postponed;

      watches = w;

      rt_watch_signal(w);
      return w;
   }
}

void rt_set_global_cb(rt_event_t event, rt_event_fn_t fn, void *user)
{
   RT_ASSERT(event < RT_LAST_EVENT);

   callback_t *cb = rt_alloc(callback_stack);
   cb->next = global_cbs[event];
   cb->fn   = fn;
   cb->user = user;

   global_cbs[event] = cb;
}

size_t rt_watch_value(watch_t *w, uint64_t *buf, size_t max)
{
   return rt_signal_value(w->signal, buf, max);
}

size_t rt_watch_string(watch_t *w, const char *map, char *buf, size_t max)
{
   return rt_signal_string(w->signal, map, buf, max);
}

size_t rt_signal_string(rt_signal_t *s, const char *map, char *buf, size_t max)
{
   char *endp = buf + max;
   int offset = 0;
   for (unsigned i = 0; i < s->n_nexus; i++) {
      rt_nexus_t *n = s->nexus[i];

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

size_t rt_signal_value(rt_signal_t *s, uint64_t *buf, size_t max)
{
   int offset = 0;
   for (unsigned i = 0; i < s->n_nexus && offset < max; i++) {
      rt_nexus_t *n = s->nexus[i];

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

rt_signal_t *rt_find_signal(e_node_t esignal)
{
   assert(e_kind(esignal) == E_SIGNAL);

   for (unsigned i = 0; i < n_scopes; i++) {
      for (unsigned j = 0; j < scopes[i].n_signals; j++) {
         if (scopes[i].signals[j].enode == esignal)
            return &(scopes[i].signals[j]);
      }
   }

   return NULL;
}

bool rt_force_signal(rt_signal_t *s, const uint64_t *buf, size_t count,
                     bool propagate)
{
   TRACE("force signal %s to %"PRIu64"%s propagate=%d", istr(e_path(s->enode)),
         buf[0], count > 1 ? "..." : "", propagate);

   RT_ASSERT(!propagate || can_create_delta);

   int offset = 0, index = 0;
   while (count > 0) {
      RT_ASSERT(index < s->n_nexus);
      rt_nexus_t *n = s->nexus[index++];

      n->flags |= NET_F_FORCED;

      if (n->forcing == NULL)
         n->forcing = rt_alloc_value(n);

#define SIGNAL_FORCE_EXPAND_U64(type) do {                              \
         type *dp = (type *)n->forcing->data;                           \
         for (int i = 0; (i < n->width) && (offset + i < count); i++)   \
            dp[i] = buf[offset + i];                                    \
      } while (0)

      FOR_ALL_SIZES(n->size, SIGNAL_FORCE_EXPAND_U64);

      if (propagate)   // XXX: this is wrong, sensitive process can run twice
                       //      see vhpi1
         deltaq_insert_driver(0, n, NULL);

      offset += n->width;
      count -= n->width;
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
