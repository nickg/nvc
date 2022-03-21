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
#include "alloc.h"
#include "common.h"
#include "cover.h"
#include "debug.h"
#include "ffi.h"
#include "hash.h"
#include "heap.h"
#include "lib.h"
#include "opt.h"
#include "rt.h"
#include "tree.h"
#include "type.h"

#include <assert.h>
#include <stdint.h>
#include <stdarg.h>
#include <inttypes.h>
#include <stdlib.h>
#include <string.h>
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
#define TRACE_SIGNALS 1
#define RT_DEBUG      1

typedef struct event         event_t;
typedef struct waveform      waveform_t;
typedef struct sens_list     sens_list_t;
typedef struct callback      callback_t;
typedef struct rt_nexus_s    rt_nexus_t;
typedef struct rt_source_s   rt_source_t;
typedef struct rt_implicit_s rt_implicit_t;
typedef struct rt_proc_s     rt_proc_t;

typedef void *(*proc_fn_t)(void *, rt_scope_t *);

typedef enum {
   W_PROC, W_WATCH, W_IMPLICIT
} wakeable_kind_t;

typedef struct {
   uint32_t        wakeup_gen;
   wakeable_kind_t kind : 8;
   bool            pending;
   bool            postponed;
} rt_wakeable_t;

typedef struct rt_proc_s {
   rt_wakeable_t  wakeable;
   tree_t         where;
   ident_t        name;
   proc_fn_t      proc_fn;
   void          *tmp_stack;
   uint32_t       tmp_alloc;
   rt_scope_t    *scope;
   rt_proc_t     *chain;
   void          *privdata;
} rt_proc_t;

typedef enum {
   EVENT_TIMEOUT,
   EVENT_DRIVER,
   EVENT_PROCESS
} event_kind_t;

typedef struct {
   timeout_fn_t  fn;
   void         *user;
} event_timeout_t;

typedef struct {
   rt_nexus_t   *nexus;
   rt_source_t  *source;
} event_driver_t;

typedef struct {
   rt_proc_t *proc;
   uint32_t   wakeup_gen;
} event_proc_t;

struct event {
   uint64_t            when;
   event_kind_t        kind;
   event_t            *delta_chain;
   union {
      event_timeout_t  timeout;
      event_driver_t   driver;
      event_proc_t     proc;
   };
};

typedef union {
   uint8_t   bytes[8];
   uint64_t  qword;
   void     *ext;
} rt_value_t;

STATIC_ASSERT(sizeof(rt_value_t) == 8);

struct waveform {
   uint64_t    when : 63;
   unsigned    null : 1;
   waveform_t *next;
   rt_value_t  value;
};

STATIC_ASSERT(sizeof(waveform_t) == 24);

struct sens_list {
   rt_wakeable_t *wake;
   sens_list_t   *next;
   sens_list_t  **reenq;
   uint32_t       wakeup_gen;
};

typedef enum {
   SOURCE_DRIVER,
   SOURCE_PORT,
} source_kind_t;

typedef struct {
   rt_proc_t  *proc;
   waveform_t  waveforms;
} rt_driver_t;

typedef struct {
   ffi_closure_t closure;
   unsigned      refcnt;
   size_t        bufsz;
   uint8_t       buffer[0];
} rt_conv_func_t;

typedef struct {
   rt_nexus_t     *input;
   rt_nexus_t     *output;
   rt_conv_func_t *conv_func;
} rt_port_t;

typedef struct rt_source_s {
   rt_source_t    *chain_input;
   rt_source_t    *chain_output;
   source_kind_t   tag;
   union {
      rt_port_t   port;
      rt_driver_t driver;
   } u;
} rt_source_t;

// The code generator knows the layout of this struct
typedef struct {
   ffi_closure_t    closure;
   uint32_t         flags;
   int32_t          ileft;
   int32_t          nlits;
} rt_resolution_t;

typedef struct {
   ffi_closure_t closure;
   res_flags_t   flags;
   int32_t       ileft;
   int8_t        tab2[16][16];
   int8_t        tab1[16];
} res_memo_t;

typedef enum {
   NET_F_FORCED       = (1 << 0),
   NET_F_OWNS_MEM     = (1 << 1),
   NET_F_LAST_VALUE   = (1 << 2),
   NET_F_IMPLICIT     = (1 << 4),
   NET_F_REGISTER     = (1 << 5),
   NET_F_DISCONNECTED = (1 << 6),
} net_flags_t;

typedef struct rt_nexus_s {
   uint32_t      width;
   uint32_t      size;
   rt_nexus_t   *chain;
   void         *free_value;
   uint64_t      last_event;
   uint64_t      last_active;
   int32_t       event_delta;
   int32_t       active_delta;
   sens_list_t  *pending;
   rt_value_t    forcing;
   res_memo_t   *resolution;
   net_flags_t   flags;
   unsigned      rank;
   unsigned      n_sources;
   rt_source_t   sources;
   rt_signal_t  *signal;
   rt_source_t  *outputs;
   void         *resolved;
   void         *last_value;
} rt_nexus_t;

// The code generator knows the layout of this struct
typedef struct {
   uint32_t size;
   uint32_t offset;
   uint8_t  data[0];
} sig_shared_t;

typedef struct rt_signal_s {
   tree_t          where;
   rt_signal_t    *chain;
   rt_scope_t     *parent;
   ihash_t        *index;
   uint32_t        width;
   net_flags_t     flags;
   uint32_t        n_nexus;
   rt_nexus_t      nexus;
   sig_shared_t    shared;
} rt_signal_t;

typedef struct rt_implicit_s {
   rt_wakeable_t  wakeable;
   ffi_closure_t *closure;
   rt_signal_t    signal;   // Has a flexible member
} rt_implicit_t;

typedef enum {
   SCOPE_ROOT,
   SCOPE_INSTANCE,
   SCOPE_PACKAGE,
   SCOPE_SIGNAL,
} rt_scope_kind_t;

typedef struct rt_scope_s {
   rt_signal_t     *signals;
   rt_proc_t       *procs;
   rt_scope_kind_t  kind;
   unsigned         size;   // For signal scopes
   ident_t          name;
   tree_t           where;
   void            *privdata;
   rt_scope_t      *parent;
   rt_scope_t      *child;
   rt_scope_t      *chain;
} rt_scope_t;

typedef struct {
   event_t **queue;
   size_t    wr, rd;
   size_t    alloc;
} rt_run_queue_t;

typedef struct rt_watch_s {
   rt_wakeable_t   wakeable;
   rt_signal_t    *signal;
   sig_event_fn_t  fn;
   rt_watch_t     *chain_all;
   void           *user_data;
} rt_watch_t;

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
static rt_scope_t      *root = NULL;
static hash_t          *scopes = NULL;
static rt_run_queue_t   timeoutq;
static rt_run_queue_t   driverq;
static rt_run_queue_t   procq;
static heap_t          *eventq_heap = NULL;
static uint64_t         now = 0;
static int              iteration = -1;
static bool             trace_on = false;
static nvc_rusage_t     ready_rusage;
static bool             aborted = false;
static sens_list_t     *resume = NULL;
static sens_list_t     *postponed = NULL;
static sens_list_t     *resume_watch = NULL;
static sens_list_t     *postponed_watch = NULL;
static sens_list_t     *implicit = NULL;
static rt_watch_t      *watches = NULL;
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
static rt_nexus_t     **nexus_tail = NULL;
static cover_tagging_t *cover = NULL;
static rt_signal_t    **signals_tail = NULL;

static rt_alloc_stack_t event_stack = NULL;
static rt_alloc_stack_t waveform_stack = NULL;
static rt_alloc_stack_t sens_list_stack = NULL;
static rt_alloc_stack_t watch_stack = NULL;
static rt_alloc_stack_t callback_stack = NULL;

static void deltaq_insert_proc(uint64_t delta, rt_proc_t *wake);
static void deltaq_insert_driver(uint64_t delta, rt_nexus_t *nexus,
                                 rt_source_t *source);
static void rt_sched_driver(rt_nexus_t *nexus, uint64_t after,
                            uint64_t reject, rt_value_t value, bool null);
static void rt_sched_event(sens_list_t **list, rt_wakeable_t *proc, bool recur);
static void *rt_tmp_alloc(size_t sz);
static rt_value_t rt_alloc_value(rt_nexus_t *n);
static void rt_copy_value_ptr(rt_nexus_t *n, rt_value_t *v, const void *p);
static inline const uint8_t *rt_value_ptr(rt_nexus_t *n, rt_value_t *v);
static rt_nexus_t *rt_clone_nexus(rt_nexus_t *old, unsigned offset);
static res_memo_t *rt_memo_resolution_fn(rt_signal_t *signal,
                                         rt_resolution_t *resolution);
static const void *rt_source_data(rt_nexus_t *nexus, rt_source_t *src);
static void _tracef(const char *fmt, ...);

#define FMT_VALUES_SZ   128
#define NEXUS_INDEX_MIN 32

#if RT_DEBUG && !defined NDEBUG
#define RT_ASSERT(x) assert((x))
#else
#define RT_ASSERT(x)
#endif

// Helper macro for passing debug loci from LLVM
#define DEBUG_LOCUS(name) \
   const char *name##_unit, uint32_t name##_offset

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

static char *fmt_values_r(const void *values, size_t len, char *buf, size_t max)
{
   char *p = buf;
   const uint8_t *vptr = values;

   for (unsigned i = 0; i < len; i++) {
      if (buf + max - p <= 5)
         return p + checked_sprintf(p, buf + max - p, "...");
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

   static shash_t *cache = NULL;
   if (cache == NULL)
      cache = shash_new(256);

   tree_t enclosing = shash_get(cache, symbol);
   if (enclosing == NULL) {
      if ((enclosing = find_mangled_decl(unit, ident_new(symbol))))
         shash_put(cache, symbol, enclosing);
   }

   return enclosing;
}

static void rt_fmt_enclosing(text_buf_t *tb, tree_t enclosing,
                             const char *symbol, const char *prefix)
{
   switch (tree_kind(enclosing)) {
   case T_PROCESS:
      tb_printf(tb, "\r\t%sProcess %s", prefix, istr(active_proc->name));
      break;
   case T_FUNC_BODY:
   case T_FUNC_DECL:
      tb_printf(tb, "\r\t%sFunction %s", prefix, type_pp(tree_type(enclosing)));
      break;
   case T_PROC_BODY:
   case T_PROC_DECL:
      tb_printf(tb, "\r\t%sProcedure %s", prefix,
                type_pp(tree_type(enclosing)));
      break;
   case T_TYPE_DECL:
      if (strstr(symbol, "$value"))
         tb_printf(tb, "\r\t%sAttribute %s'VALUE",
                   prefix, istr(tree_ident(enclosing)));
      else
         tb_printf(tb, "\r\t%sType %s", prefix, istr(tree_ident(enclosing)));
      break;
   case T_BLOCK:
      tb_printf(tb, "\r\t%sProcess (init)", prefix);
      break;
   default:
      tb_printf(tb, "\r\t%s%s", prefix, istr(tree_ident(enclosing)));
      break;
   }
}

static text_buf_t *rt_fmt_trace(const loc_t *fixed)
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

         rt_fmt_enclosing(tb, enclosing, inl->symbol, "Inlined ");
         tb_printf(tb, "\r\t    File %s, Line %u", inl->srcfile, inl->lineno);
      }

      tree_t enclosing = rt_find_enclosing_decl(f->vhdl_unit, f->symbol);
      if (enclosing == NULL)
         continue;

      unsigned lineno = f->lineno;
      const char *srcfile = f->srcfile;
      if (fixed != NULL && !found_fixed) {
         lineno = fixed->first_line;
         srcfile = loc_file_str(fixed);
         found_fixed = true;
      }
      else if (f->lineno == 0) {
         // Exact DWARF debug info not available
         const loc_t *loc = tree_loc(enclosing);
         lineno = loc->first_line;
         srcfile = loc_file_str(loc);
      }

      rt_fmt_enclosing(tb, enclosing, f->symbol, "");
      tb_printf(tb, "\r\t    File %s, Line %u", srcfile, lineno);
   }

   if (fixed != NULL && (nframes == 0 || !found_fixed)) {
      const char *pname = active_proc == NULL
         ? "(init)" : istr(active_proc->name);
      tb_printf(tb, "\r\tProcess %s", pname);
      tb_printf(tb, "\r\t    File %s, Line %u", loc_file_str(fixed),
                fixed->first_line);
   }

   debug_free(di);
   return tb;
}

typedef void (*rt_msg_fn_t)(const char *, ...);

__attribute__((format(printf, 3, 4)))
static void rt_msg(const loc_t *where, rt_msg_fn_t fn, const char *fmt, ...)
{
   va_list ap;
   va_start(ap, fmt);

   char *LOCAL buf = xvasprintf(fmt, ap);
   LOCAL_TEXT_BUF trace = rt_fmt_trace(where);

   va_end(ap);

   (*fn)("%s%s", buf, tb_get(trace));
}

static size_t uarray_len(const ffi_uarray_t *u)
{
   return abs(u->dims[0].length);
}

static ffi_uarray_t wrap_str(char *buf, size_t len)
{
   ffi_uarray_t u = {
      .ptr = buf,
      .dims = { [0] = { .left = 1, .length = len } }
   };
   return u;
}

static ffi_uarray_t bit_vec_to_string(const ffi_uarray_t *vec, int log_base)
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

static inline void rt_check_postponed(int64_t after)
{
   if (unlikely(active_proc->wakeable.postponed && (after == 0)))
      fatal("postponed process %s cannot cause a delta cycle",
            istr(active_proc->name));
}

static inline tree_t rt_locus_to_tree(const char *unit, unsigned offset)
{
   return tree_from_locus(ident_new(unit), offset, lib_get_qualified);
}

static void rt_secondary_stack_fault(void *addr, void *__ctx)
{
   opt_name_t which = (uintptr_t)__ctx;

   const unsigned curr = opt_get_int(which);
   const char flag = which == OPT_GLOBAL_STACK ? 'G' : 'P';
   hint_at(NULL, "the current limit is %u bytes which you can increase with "
           "the $bold$-%c$$ option, for example $bold$-%c %uk$$",
           curr, flag, flag, (curr * 2) / 1024);

   LOCAL_TEXT_BUF trace = rt_fmt_trace(NULL);
   fatal("%s secondary stack exhausted%s",
         which == OPT_GLOBAL_STACK ? "global" : "process", tb_get(trace));
}

static void *rt_map_secondary_stack(opt_name_t which)
{
   return mmap_guarded(opt_get_int(which), rt_secondary_stack_fault,
                       (void *)(uintptr_t)which);
}

static void rt_dump_signals(rt_scope_t *scope)
{
   if (scope->signals == NULL && scope->child == NULL)
      return;

   if (scope->kind != SCOPE_SIGNAL && scope->kind != SCOPE_ROOT) {
      const char *sname = istr(scope->name);
      fprintf(stderr, "== %s ", sname);
      for (int pad = 74 - strlen(sname); pad > 0; pad--)
         fputc('=', stderr);
      fputc('\n', stderr);

      fprintf(stderr, "%-16s %5s %4s %7s %7s %7s %4s %s\n",
              "Signal", "Width", "Size", "Sources", "Outputs",
              "Pending", "Rank", "Value");
   }

   for (rt_signal_t *s = scope->signals; s != NULL; s = s->chain) {
      rt_nexus_t *n = &(s->nexus);

      LOCAL_TEXT_BUF tb = tb_new();
      if (scope->kind == SCOPE_SIGNAL)
         tb_printf(tb, "%s.", istr(scope->name));
      tb_cat(tb, istr(tree_ident(s->where)));

      for (int nth = 0; nth < s->n_nexus; nth++, n = n->chain) {
         int pending = 0;
         for (sens_list_t *p = n->pending; p != NULL; p = p->next)
            pending++;

         int n_outputs = 0;
         for (rt_source_t *s = n->outputs; s != NULL; s = s->chain_output)
            n_outputs++;

         fprintf(stderr, "%-16s %-5d %-4d %-7d %-7d %-7d %-4d %s\n",
                 nth == 0 ? tb_get(tb) : "+",
                 n->width, n->size, n->n_sources, n_outputs, pending,
                 n->rank,
                 fmt_nexus(n, n->resolved));
      }
   }

   for (rt_scope_t *c = scope->child; c != NULL; c = c->chain)
      rt_dump_signals(c);
}

static rt_source_t *rt_add_source(rt_nexus_t *n, source_kind_t kind)
{
   rt_source_t *src = NULL;
   if (n->n_sources++ == 0)
      src = &(n->sources);
   else if (n->resolution == NULL
            && (n->sources.tag != SOURCE_PORT
                || n->sources.u.port.conv_func == NULL))
      rt_msg(tree_loc(n->signal->where), fatal,
             "unresolved signal %s has multiple sources",
             istr(tree_ident(n->signal->where)));
   else {
      rt_source_t **p;
      for (p = &(n->sources.chain_input); *p; p = &((*p)->chain_input))
         ;
      *p = src = xmalloc(sizeof(rt_source_t));
   }

   src->chain_input  = NULL;
   src->chain_output = NULL;
   src->tag          = kind;

   switch (kind) {
   case SOURCE_DRIVER:
      {
         src->u.driver.proc = NULL;

         waveform_t *w0 = &(src->u.driver.waveforms);
         w0->when  = 0;
         w0->null  = 0;
         w0->next  = NULL;
         w0->value = rt_alloc_value(n);
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

static void rt_clone_source(rt_nexus_t *nexus, rt_source_t *old, int offset)
{
   rt_source_t *new = rt_add_source(nexus, old->tag);

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
            else
               new->u.port.input = rt_clone_nexus(old->u.port.input, offset);
            RT_ASSERT(new->u.port.input->width == nexus->width);
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
         w_new->null = w_old->null;

         const int split = offset * nexus->size;
         rt_copy_value_ptr(nexus, &(w_new->value),
                           rt_value_ptr(nexus, &(w_old->value)) + split);

         // Future transactions
         for (w_old = w_old->next; w_old; w_old = w_old->next) {
            w_new = (w_new->next = rt_alloc(waveform_stack));
            w_new->value = rt_alloc_value(nexus);
            w_new->next  = NULL;
            w_new->when  = w_old->when;
            w_new->null  = w_old->null;

            rt_copy_value_ptr(nexus, &(w_new->value),
                              rt_value_ptr(nexus, &(w_old->value)) + split);

            RT_ASSERT(w_old->when >= now);
            deltaq_insert_driver(w_new->when - now, nexus, new);
         }
      }
      break;
   }
}

static rt_nexus_t *rt_clone_nexus(rt_nexus_t *old, unsigned offset)
{
   RT_ASSERT(offset < old->width);

   rt_signal_t *signal = old->signal;
   signal->n_nexus++;

   const size_t oldsz = old->width * old->size;

   rt_nexus_t *new = xcalloc(sizeof(rt_nexus_t));
   new->width        = old->width - offset;
   new->size         = old->size;
   new->resolution   = old->resolution;
   new->signal       = signal;
   new->resolved     = (uint8_t *)old->resolved + offset * old->size;
   new->last_value   = (uint8_t *)old->last_value + offset * old->size;
   new->chain        = old->chain;
   new->flags        = old->flags;
   new->rank         = old->rank;
   new->last_active  = old->last_active;
   new->last_event   = old->last_event;
   new->active_delta = old->active_delta;
   new->event_delta  = old->event_delta;

   old->chain = new;
   old->width = offset;

   // Old nexus may be holding large amounts of memory
   free(old->free_value);
   old->free_value = NULL;

   if (new->chain == NULL)
      nexus_tail = &(new->chain);

   for (sens_list_t *l = old->pending; l; l = l->next) {
      sens_list_t *lnew = rt_alloc(sens_list_stack);
      lnew->wake       = l->wake;
      lnew->wakeup_gen = l->wakeup_gen;
      lnew->next       = new->pending;
      lnew->reenq      = l->reenq ? &(new->pending) : NULL;

      new->pending = lnew;
   }

   if (old->n_sources > 0) {
      for (rt_source_t *it = &(old->sources); it; it = it->chain_input) {
         rt_clone_source(new, it, offset);

         // Free up memory from old driver
         if (it->tag == SOURCE_DRIVER && oldsz > sizeof(rt_value_t)) {
            waveform_t *w_old = &(it->u.driver.waveforms);
            rt_value_t v_old = rt_alloc_value(old);
            rt_copy_value_ptr(old, &v_old, w_old->value.ext);
            free(w_old->value.ext);
            w_old->value = v_old;
         }
      }
   }

   int nth = 0;
   for (rt_source_t *old_o = old->outputs; old_o;
        old_o = old_o->chain_output, nth++) {

      RT_ASSERT(old_o->tag == SOURCE_PORT);

      if (old_o->u.port.conv_func != NULL)
         new->outputs = old_o;
      else {
         rt_nexus_t *out_n;
         if (old_o->u.port.output->width == offset)
            out_n = old_o->u.port.output->chain;   // Cycle breaking
         else
            out_n = rt_clone_nexus(old_o->u.port.output, offset);

         for (rt_source_t *s = &(out_n->sources); s; s = s->chain_input) {
            if (s->tag != SOURCE_PORT)
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

   if (signal->index == NULL && signal->n_nexus >= NEXUS_INDEX_MIN) {
      const unsigned w = MIN(old->width, new->width);
      TRACE("create index for signal %s", istr(tree_ident(signal->where)));
      signal->index = ihash_new(MIN(MAX((signal->width / w) * 2, 16), 1024));
   }

   if (signal->index != NULL) {
      const unsigned key =
         (new->resolved - (void *)signal->shared.data) / new->size;
      ihash_put(signal->index, key, new);
   }

   return new;
}

static rt_nexus_t *rt_split_nexus(rt_signal_t *s, int offset, int count)
{
   rt_nexus_t *n0 = &(s->nexus);
   if (likely(offset == 0 && n0->width == count))
      return n0;

   rt_nexus_t *map = NULL;
   if (s->index != NULL && (map = ihash_get(s->index, offset))) {
      if (likely(map->width == count))
         return map;
      offset = 0;
   }

   rt_nexus_t *result = NULL;
   for (rt_nexus_t *it = map ?: &(s->nexus); count > 0; it = it->chain) {
      if (offset >= it->width) {
         offset -= it->width;
         continue;
      }
      else if (offset > 0) {
         rt_clone_nexus(it, offset);
         offset = 0;
         continue;
      }
      else {
         if (it->width > count)
            rt_clone_nexus(it, count);

         count -= it->width;

         if (result == NULL)
            result = it;
      }
   }

   return result;
}

static void rt_set_rank(rt_nexus_t *n, int rank)
{
   if (n->rank < rank) {
      n->rank = rank;

      for (rt_source_t *o = n->outputs; o; o = o->chain_output)
         rt_set_rank(o->u.port.output, rank + 1);
   }
}

static void rt_setup_signal(rt_signal_t *s, tree_t where, unsigned count,
                            unsigned size, unsigned offset)
{
   s->where         = where;
   s->width         = count;
   s->n_nexus       = 1;
   s->shared.size   = count * size;
   s->shared.offset = offset;
   s->flags         = NET_F_OWNS_MEM | NET_F_LAST_VALUE;
   s->parent        = active_scope;

   *signals_tail = s;
   signals_tail = &(s->chain);

   s->nexus.width      = count;
   s->nexus.size       = size;
   s->nexus.n_sources  = 0;
   s->nexus.resolved   = s->shared.data;
   s->nexus.last_value = s->shared.data + s->shared.size;
   s->nexus.flags      = NET_F_LAST_VALUE;
   s->nexus.signal     = s;

   *nexus_tail = &(s->nexus);
   nexus_tail = &(s->nexus.chain);

   profile.n_signals++;
}

static void rt_copy_sub_signals(rt_scope_t *scope, void *buf)
{
   assert(scope->kind == SCOPE_SIGNAL);

   for (rt_signal_t *s = scope->signals; s != NULL; s = s->chain)
      memcpy(buf + s->shared.offset, s->shared.data, s->shared.size);

   for (rt_scope_t *s = scope->child; s != NULL; s = s->chain)
      rt_copy_sub_signals(s, buf);
}

static void rt_copy_sub_signal_sources(rt_scope_t *scope, void *buf)
{
   assert(scope->kind == SCOPE_SIGNAL);

   for (rt_signal_t *s = scope->signals; s != NULL; s = s->chain) {
      rt_nexus_t *n = &(s->nexus);
      for (unsigned i = 0; i < s->n_nexus; i++) {
         unsigned o = 0;
         for (rt_source_t *src = &(n->sources); src; src = src->chain_input) {
            const void *data = rt_source_data(n, src);
            if (data == NULL)
               continue;

            memcpy(buf + s->shared.offset + (o++ * scope->size),
                   data, n->size * n->width);
         }
      }
   }

   for (rt_scope_t *s = scope->child; s != NULL; s = s->chain)
      rt_copy_sub_signal_sources(s, buf);
}

static void *rt_composite_signal(rt_signal_t *signal, size_t *psz)
{
   assert(signal->parent->kind == SCOPE_SIGNAL);

   rt_scope_t *root = signal->parent;
   while (root->parent->kind == SCOPE_SIGNAL)
      root = root->parent;

   *psz = root->size;

   char *buf = xmalloc(root->size);
   rt_copy_sub_signals(root, buf);
   return buf;
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
         istr(tree_ident(s->where)), offset, scalar, fmt_time(after),
         fmt_time(reject));

   rt_check_postponed(after);

   rt_nexus_t *n = rt_split_nexus(s, offset, 1);

   rt_value_t value = rt_alloc_value(n);
   value.qword = scalar;

   rt_sched_driver(n, after, reject, value, false);
}

DLLEXPORT
void _sched_waveform(sig_shared_t *ss, uint32_t offset, void *values,
                     int32_t count, int64_t after, int64_t reject)
{
   rt_signal_t *s = container_of(ss, rt_signal_t, shared);

   TRACE("_sched_waveform %s+%d value=%s count=%d after=%s reject=%s",
         istr(tree_ident(s->where)), offset, fmt_values(values, count),
         count, fmt_time(after), fmt_time(reject));

   rt_check_postponed(after);

   rt_nexus_t *n = rt_split_nexus(s, offset, count);
   char *vptr = values;
   for (; count > 0; n = n->chain) {
      count -= n->width;
      RT_ASSERT(count >= 0);

      const size_t valuesz = n->width * n->size;
      rt_value_t value = rt_alloc_value(n);
      rt_copy_value_ptr(n, &value, vptr);
      vptr += valuesz;

      rt_sched_driver(n, after, reject, value, false);
   }
}

DLLEXPORT
void _disconnect(sig_shared_t *ss, uint32_t offset, int32_t count,
                 int64_t after, int64_t reject)
{
   rt_signal_t *s = container_of(ss, rt_signal_t, shared);

   TRACE("_disconnect %s+%d len=%d after=%s reject=%s",
         istr(tree_ident(s->where)), offset, count, fmt_time(after),
         fmt_time(reject));

   rt_check_postponed(after);

   rt_nexus_t *n = rt_split_nexus(s, offset, count);
   for (; count > 0; n = n->chain) {
      count -= n->width;
      RT_ASSERT(count >= 0);

      rt_value_t null = {};
      rt_sched_driver(n, after, reject, null, true);
      n->flags |= NET_F_DISCONNECTED;
   }
}

DLLEXPORT
void _sched_event(sig_shared_t *ss, uint32_t offset, int32_t count, bool recur,
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

   rt_nexus_t *n = rt_split_nexus(s, offset, count);
   for (; count > 0; n = n->chain) {
      rt_sched_event(&(n->pending), wake, recur);

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
      proc_tmp_stack = rt_map_secondary_stack(OPT_PROC_STACK);
   }

   active_proc->tmp_alloc = _tmp_alloc;
}

DLLEXPORT
void __nvc_drive_signal(sig_shared_t *ss, uint32_t offset, int32_t count)
{
   rt_signal_t *s = container_of(ss, rt_signal_t, shared);

   TRACE("drive signal %s+%d count=%d", istr(tree_ident(s->where)),
         offset, count);

   rt_nexus_t *n = rt_split_nexus(s, offset, count);
   for (; count > 0; n = n->chain) {
      rt_source_t *s;
      for (s = &(n->sources); s; s = s->chain_input) {
         if (s->tag == SOURCE_DRIVER && s->u.driver.proc == active_proc)
            break;
      }

      if (s == NULL) {
         s = rt_add_source(n, SOURCE_DRIVER);
         s->u.driver.proc = active_proc;
      }

      count -= n->width;
      RT_ASSERT(count >= 0);
   }
}

DLLEXPORT
void __nvc_set_signal_kind(sig_shared_t *ss, int32_t kind)
{
   rt_signal_t *s = container_of(ss, rt_signal_t, shared);

   TRACE("set signal kind %s kind=%d", istr(tree_ident(s->where)), kind);

   rt_nexus_t *n = &(s->nexus);
   for (int i = 0; i < s->n_nexus; i++, n = n->chain) {
      if (kind == SIGNAL_REGISTER)
         n->flags |= NET_F_REGISTER;
      else
         n->flags &= ~NET_F_REGISTER;
   }
}

DLLEXPORT
void __nvc_resolve_signal(sig_shared_t *ss, rt_resolution_t *resolution)
{
   rt_signal_t *s = container_of(ss, rt_signal_t, shared);

   TRACE("resolve signal %s", istr(tree_ident(s->where)));

   res_memo_t *memo = rt_memo_resolution_fn(s, resolution);

   rt_nexus_t *n = &(s->nexus);
   for (int i = 0; i < s->n_nexus; i++, n = n->chain)
      n->resolution = memo;
}

DLLEXPORT
void __nvc_push_scope(DEBUG_LOCUS(locus), int32_t size)
{
   tree_t where = rt_locus_to_tree(locus_unit, locus_offset);

   TRACE("push scope %s size=%d", istr(tree_ident(where)), size);

   ident_t name = tree_ident(where);
   if (active_scope->kind == SCOPE_SIGNAL)
      name = ident_prefix(active_scope->name, name, '.');

   rt_scope_t *s = xcalloc(sizeof(rt_scope_t));
   s->where  = where;
   s->name   = name;
   s->kind   = SCOPE_SIGNAL;
   s->parent = active_scope;
   s->chain  = active_scope->child;
   s->size   = size;

   active_scope->child = s;
   active_scope = s;

   signals_tail = &(s->signals);
}

DLLEXPORT
void __nvc_pop_scope(void)
{
   TRACE("pop scope");

   if (unlikely(active_scope->kind != SCOPE_SIGNAL))
      fatal_trace("cannot pop non-signal scope");

   active_scope = active_scope->parent;

   for (signals_tail = &(active_scope->signals);
        *signals_tail != NULL;
        signals_tail = &((*signals_tail)->chain))
      ;
}

DLLEXPORT
sig_shared_t *_init_signal(uint32_t count, uint32_t size, const uint8_t *values,
                           DEBUG_LOCUS(locus), int32_t offset)
{
   tree_t where = rt_locus_to_tree(locus_unit, locus_offset);

   TRACE("_init_signal %s count=%d size=%d values=%p offset=%d",
         istr(tree_ident(where)), count, size, values, offset);

   const size_t datasz = MAX(2 * count * size, 8);
   rt_signal_t *s = xcalloc_flex(sizeof(rt_signal_t), 1, datasz);
   rt_setup_signal(s, where, count, size, offset);

   memcpy(s->shared.data, values, s->shared.size);

   return &(s->shared);
}

DLLEXPORT
sig_shared_t *_implicit_signal(uint32_t count, uint32_t size,
                               DEBUG_LOCUS(locus), uint32_t kind,
                               ffi_closure_t *closure)
{
   tree_t where = rt_locus_to_tree(locus_unit, locus_offset);

   TRACE("_init_signal %s count=%d size=%d kind=%d",
         istr(tree_ident(where)), count, size, kind);

   const size_t datasz = MAX(2 * count * size, 8);
   rt_implicit_t *imp = xcalloc_flex(sizeof(rt_implicit_t), 1, datasz);
   rt_setup_signal(&(imp->signal), where, count, size, 0);

   ffi_closure_t *copy = xmalloc(sizeof(ffi_closure_t));
   *copy = *closure;
   copy->refcnt = 1;

   imp->signal.flags |= NET_F_IMPLICIT;
   imp->closure = copy;
   imp->wakeable.kind = W_IMPLICIT;

   int8_t r;
   ffi_call(imp->closure, NULL, 0, &r, sizeof(r));

   RT_ASSERT(size * count == 1);
   memcpy(imp->signal.shared.data, &r, imp->signal.shared.size);

   return &(imp->signal.shared);
}

DLLEXPORT
void __nvc_map_signal(sig_shared_t *src_ss, uint32_t src_offset,
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

   rt_nexus_t *src_n = rt_split_nexus(src_s, src_offset, src_count);
   rt_nexus_t *dst_n = rt_split_nexus(dst_s, dst_offset, dst_count);

   while (src_count > 0 && dst_count > 0) {
      if (src_n->width > dst_n->width && closure == NULL)
         rt_clone_nexus(src_n, dst_n->width);
      else if (src_n->width < dst_n->width && closure == NULL)
         rt_clone_nexus(dst_n, src_n->width);

      assert(src_n->width == dst_n->width || closure != NULL);
      assert(src_n->size == dst_n->size || closure != NULL);

      rt_source_t *port = rt_add_source(dst_n, SOURCE_PORT);
      port->u.port.input = src_n;

      if (conv_func != NULL) {
         port->u.port.conv_func = conv_func;
         conv_func->refcnt++;
      }

      rt_set_rank(dst_n, src_n->rank + 1);

      port->chain_output = src_n->outputs;
      src_n->outputs = port;

      src_count -= src_n->width;
      dst_count -= dst_n->width;
      RT_ASSERT(src_count >= 0);
      RT_ASSERT(dst_count >= 0);

      src_n = src_n->chain;
      dst_n = dst_n->chain;
   }
}

DLLEXPORT
void __nvc_map_const(sig_shared_t *ss, uint32_t offset,
                     const uint8_t *values, uint32_t count)
{
   rt_signal_t *s = container_of(ss, rt_signal_t, shared);

   TRACE("map const %s to %s+%d count %d", fmt_values(values, count),
         istr(tree_ident(s->where)), offset, count);

   rt_nexus_t *n = rt_split_nexus(s, offset, count);
   for (; count > 0; n = n->chain) {
      memcpy(n->resolved, values, n->width * n->size);
      values += n->width * n->size;

      count -= n->width;
      RT_ASSERT(count >= 0);
   }
}

DLLEXPORT
void __nvc_assert_fail(const uint8_t *msg, int32_t msg_len, int8_t severity,
                       int64_t hint_left, int64_t hint_right, int8_t hint_valid,
                       DEBUG_LOCUS(locus))
{
   // LRM 93 section 8.2
   // The error message consists of at least
   // a) An indication that this message is from an assertion
   // b) The value of the severity level
   // c) The value of the message string
   // d) The name of the design unit containing the assertion

   RT_ASSERT(severity <= SEVERITY_FAILURE);

   static const char *levels[] = {
      "Note", "Warning", "Error", "Failure"
   };

   static const uint8_t def_str[] = "Assertion violation.";

   if (msg == NULL) {
      msg = def_str;
      msg_len = sizeof(def_str) - 1;
   }

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

   tree_t where = rt_locus_to_tree(locus_unit, locus_offset);

   if (hint_valid) {
      assert(tree_kind(where) == T_FCALL);
      type_t p0_type = tree_type(tree_value(tree_param(where, 0)));
      type_t p1_type = tree_type(tree_value(tree_param(where, 1)));

      LOCAL_TEXT_BUF tb = tb_new();
      tb_cat(tb, "condition ");
      to_string(tb, p0_type, hint_left);
      switch (tree_subkind(tree_ref(where))) {
      case S_SCALAR_EQ:  tb_cat(tb, " = "); break;
      case S_SCALAR_NEQ: tb_cat(tb, " /= "); break;
      case S_SCALAR_LT:  tb_cat(tb, " < "); break;
      case S_SCALAR_GT:  tb_cat(tb, " > "); break;
      case S_SCALAR_LE:  tb_cat(tb, " <= "); break;
      case S_SCALAR_GE:  tb_cat(tb, " >= "); break;
      default: tb_cat(tb, " <?> "); break;
      }
      to_string(tb, p1_type, hint_right);
      tb_cat(tb, " is false");

      hint_at(tree_loc(where), "%s", tb_get(tb));
   }

   char tmbuf[64];
   rt_fmt_now(tmbuf, sizeof(tmbuf));

   rt_msg(tree_loc(where), fn, "%s: Assertion %s: %.*s",
          tmbuf, levels[severity], msg_len, msg);
}

DLLEXPORT
void __nvc_report(const uint8_t *msg, int32_t msg_len, int8_t severity,
                  DEBUG_LOCUS(locus))
{
   RT_ASSERT(severity <= SEVERITY_FAILURE);

   static const char *levels[] = {
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

   tree_t where = rt_locus_to_tree(locus_unit, locus_offset);

   rt_msg(tree_loc(where), fn, "%s: Report %s: %.*s",
          tmbuf, levels[severity], msg_len, msg);
}

DLLEXPORT
void __nvc_index_fail(int32_t value, int32_t left, int32_t right, int8_t dir,
                      DEBUG_LOCUS(locus))
{
   tree_t where = rt_locus_to_tree(locus_unit, locus_offset);
   type_t type = tree_type(where);

   LOCAL_TEXT_BUF tb = tb_new();
   tb_cat(tb, "index ");
   to_string(tb, type, value);
   tb_printf(tb, " outside of %s range ", type_pp(type));
   to_string(tb, type, left);
   tb_cat(tb, dir == RANGE_TO ? " to " : " downto ");
   to_string(tb, type, right);

   rt_msg(tree_loc(where), fatal, "%s", tb_get(tb));
}

DLLEXPORT
void __nvc_range_fail(int64_t value, int64_t left, int64_t right, int8_t dir,
                      DEBUG_LOCUS(locus), DEBUG_LOCUS(hint))
{
   tree_t where = rt_locus_to_tree(locus_unit, locus_offset);
   tree_t hint = rt_locus_to_tree(hint_unit, hint_offset);

   type_t type = tree_type(hint);

   LOCAL_TEXT_BUF tb = tb_new();
   tb_cat(tb, "value ");
   to_string(tb, type, value);
   tb_printf(tb, " outside of %s range ", type_pp(type));
   to_string(tb, type, left);
   tb_cat(tb, dir == RANGE_TO ? " to " : " downto ");
   to_string(tb, type, right);

   switch (tree_kind(hint)) {
   case T_SIGNAL_DECL:
   case T_CONST_DECL:
   case T_VAR_DECL:
   case T_REF:
      tb_printf(tb, " for %s %s", class_str(class_of(hint)),
                istr(tree_ident(hint)));
      break;
   case T_PORT_DECL:
      tb_printf(tb, " for port %s", istr(tree_ident(hint)));
      break;
   case T_PARAM_DECL:
      tb_printf(tb, " for parameter %s", istr(tree_ident(hint)));
      break;
   case T_GENERIC_DECL:
      tb_printf(tb, " for generic %s", istr(tree_ident(hint)));
      break;
   case T_ATTR_REF:
      tb_printf(tb, " for attribute '%s", istr(tree_ident(hint)));
      break;
   default:
      break;
   }

   rt_msg(tree_loc(where), fatal, "%s", tb_get(tb));
}

DLLEXPORT
void __nvc_length_fail(int32_t left, int32_t right, int32_t dim,
                       DEBUG_LOCUS(locus))
{
   tree_t where = rt_locus_to_tree(locus_unit, locus_offset);
   const tree_kind_t kind = tree_kind(where);

   LOCAL_TEXT_BUF tb = tb_new();
   if (kind == T_PORT_DECL || kind == T_GENERIC_DECL || kind == T_PARAM_DECL)
      tb_cat(tb, "actual");
   else
      tb_cat(tb, "value");
   tb_printf(tb, " length %d", right);
   if (dim > 0)
      tb_printf(tb, " for dimension %d", dim);
   tb_cat(tb, " does not match ");

   switch (kind) {
   case T_PORT_DECL:
      tb_printf(tb, "port %s", istr(tree_ident(where)));
      break;
   case T_PARAM_DECL:
      tb_printf(tb, "parameter %s", istr(tree_ident(where)));
      break;
   case T_GENERIC_DECL:
      tb_printf(tb, "generic %s", istr(tree_ident(where)));
      break;
   case T_VAR_DECL:
      tb_printf(tb, "variable %s", istr(tree_ident(where)));
      break;
   case T_SIGNAL_DECL:
      tb_printf(tb, "signal %s", istr(tree_ident(where)));
      break;
   case T_REF:
      tb_printf(tb, "%s %s", class_str(class_of(where)),
                istr(tree_ident(where)));
      break;
   default:
      tb_cat(tb, "target");
      break;
   }

   tb_printf(tb, " length %d", left);

   rt_msg(tree_loc(where), fatal, "%s", tb_get(tb));
}

DLLEXPORT
void __nvc_exponent_fail(int32_t value, DEBUG_LOCUS(locus))
{
   tree_t where = rt_locus_to_tree(locus_unit, locus_offset);

   rt_msg(tree_loc(where), fatal, "negative exponent %d only "
          "allowed for floating-point types", value);
}

DLLEXPORT
void _canon_value(const uint8_t *raw_str, int32_t str_len, ffi_uarray_t *u)
{
   char *buf = rt_tmp_alloc(str_len), *p = buf;
   int pos = 0;

   for (; pos < str_len && isspace((int)raw_str[pos]); pos++)
      ;

   bool upcase = true;
   for (; pos < str_len && !isspace((int)raw_str[pos]); pos++) {
      if (raw_str[pos] == '\'')
         upcase = !upcase;

      *p++ = upcase ? toupper((int)raw_str[pos]) : raw_str[pos];
   }

   for (; pos < str_len; pos++) {
      if (!isspace((int)raw_str[pos])) {
         rt_msg(NULL, fatal, "found invalid characters \"%.*s\" after value "
                "\"%.*s\"", (int)(str_len - pos), raw_str + pos, str_len,
                (const char *)raw_str);
      }
   }

   *u = wrap_str(buf, p - buf);
}

DLLEXPORT
void _int_to_string(int64_t value, ffi_uarray_t *u)
{
   char *buf = rt_tmp_alloc(20);
   size_t len = checked_sprintf(buf, 20, "%"PRIi64, value);

   *u = wrap_str(buf, len);
}

DLLEXPORT
void _real_to_string(double value, ffi_uarray_t *u)
{
   char *buf = rt_tmp_alloc(32);
   size_t len = checked_sprintf(buf, 32, "%.*g", 17, value);

   *u = wrap_str(buf, len);
}

DLLEXPORT
int64_t _string_to_int(const uint8_t *raw_str, int32_t str_len, int32_t *used)
{
   const char *p = (const char *)raw_str;
   const char *endp = p + str_len;

   for (; p < endp && isspace((int)*p); p++)
      ;

   const bool is_negative = p < endp && *p == '-';
   if (is_negative) p++;

   int64_t value = INT64_MIN;
   int num_digits = 0;
   while (p < endp && (isdigit((int)*p) || *p == '_')) {
      if (*p != '_') {
         value *= 10;
         value += (*p - '0');
         num_digits++;
      }
      ++p;
   }

   if (is_negative) value = -value;

   if (num_digits == 0)
      rt_msg(NULL, fatal, "invalid integer value "
             "\"%.*s\"", str_len, (const char *)raw_str);

   if (used != NULL)
      *used = p - (const char *)raw_str;
   else {
      for (; p < endp && *p != '\0'; p++) {
         if (!isspace((int)*p)) {
            rt_msg(NULL, fatal, "found invalid characters \"%.*s\" after value "
                   "\"%.*s\"", (int)(endp - p), p, str_len,
                   (const char *)raw_str);
         }
      }
   }

   return value;
}

DLLEXPORT
double _string_to_real(const uint8_t *raw_str, int32_t str_len, uint8_t **tail)
{
   char *null LOCAL = xmalloc(str_len + 1);
   memcpy(null, raw_str, str_len);
   null[str_len] = '\0';

   char *p = null;
   for (; p < p + str_len && isspace((int)*p); p++)
      ;

   double value = strtod(p, &p);

   if (*p != '\0' && !isspace((int)*p))
      rt_msg(NULL, fatal, "invalid real value "
             "\"%.*s\"", str_len, (const char *)raw_str);

   if (tail != NULL)
      *tail = (uint8_t *)p;
   else {
      for (; p < null + str_len && *p != '\0'; p++) {
         if (!isspace((int)*p)) {
            rt_msg(NULL, fatal, "found invalid characters \"%.*s\" after value "
                   "\"%.*s\"", (int)(null + str_len - p), p, str_len,
                   (const char *)raw_str);
         }
      }
   }

   return value;
}

DLLEXPORT
void __nvc_div_zero(DEBUG_LOCUS(locus))
{
   tree_t where = rt_locus_to_tree(locus_unit, locus_offset);
   rt_msg(tree_loc(where), fatal, "division by zero");
}

DLLEXPORT
void __nvc_null_deref(DEBUG_LOCUS(locus))
{
   tree_t where = rt_locus_to_tree(locus_unit, locus_offset);
   rt_msg(tree_loc(where), fatal, "null access dereference");
}

DLLEXPORT
bool _nvc_ieee_warnings(void)
{
   return opt_get_int(OPT_IEEE_WARNINGS);
}

DLLEXPORT
int64_t _std_standard_now(void)
{
   return now;
}

DLLEXPORT
void _std_to_string_time(int64_t value, int64_t unit, ffi_uarray_t *u)
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

   size_t len;
   if (value % unit == 0)
      len = checked_sprintf(buf, max_len, "%"PRIi64" %s",
                            value / unit, unit_str);
   else
      len = checked_sprintf(buf, max_len, "%g %s",
                            (double)value / (double)unit, unit_str);

   TRACE("result=%s", buf);
   *u = wrap_str(buf, len);
}

DLLEXPORT
void _std_to_string_real_digits(double value, int32_t digits, ffi_uarray_t *u)
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
void _std_to_string_real_format(double value, EXPLODED_UARRAY(fmt),
                                ffi_uarray_t *u)
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
void _std_to_hstring_bit_vec(EXPLODED_UARRAY(vec), ffi_uarray_t *u)
{
   const ffi_uarray_t vec = { vec_ptr, { { vec_left, vec_length } } };
   *u = bit_vec_to_string(&vec, 4);
}

DLLEXPORT
void _std_to_ostring_bit_vec(EXPLODED_UARRAY(vec), ffi_uarray_t *u)
{
   const ffi_uarray_t vec = { vec_ptr, { { vec_left, vec_length } } };
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
void _debug_out(intptr_t val, int32_t reg)
{
   printf("DEBUG: r%d val=%"PRIxPTR"\n", reg, val);
   fflush(stdout);
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

   fflush(stdout);
}

DLLEXPORT
int64_t _last_event(sig_shared_t *ss, uint32_t offset, int32_t count)
{
   rt_signal_t *s = container_of(ss, rt_signal_t, shared);

   TRACE("_last_event %s offset=%d count=%d",
         istr(tree_ident(s->where)), offset, count);

   int64_t last = TIME_HIGH;

   rt_nexus_t *n = rt_split_nexus(s, offset, count);
   for (; count > 0; n = n->chain) {
      if (n->last_event <= now)
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
         istr(tree_ident(s->where)), offset, count);

   int64_t last = TIME_HIGH;

   rt_nexus_t *n = rt_split_nexus(s, offset, count);
   for (; count > 0; n = n->chain) {
      if (n->last_active <= now)
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
         istr(tree_ident(s->where)), offset, count);

   int ntotal = 0, ndriving = 0;
   bool found = false;
   rt_nexus_t *n = rt_split_nexus(s, offset, count);
   for (; count > 0; n = n->chain) {
      if (n->n_sources > 0) {
         for (rt_source_t *src = &(n->sources); src; src = src->chain_input) {
            if (src->tag != SOURCE_DRIVER)
               continue;
            else if (src->u.driver.proc == active_proc) {
               if (!src->u.driver.waveforms.null) ndriving++;
               found = true;
               break;
            }
         }
      }

      ntotal++;
      count -= n->width;
      RT_ASSERT(count >= 0);
   }

   if (!found)
      rt_msg(NULL, fatal, "process %s does not contain a driver for %s",
             istr(active_proc->name), istr(tree_ident(s->where)));

   return ntotal == ndriving;
}

DLLEXPORT
void *_driving_value(sig_shared_t *ss, uint32_t offset, int32_t count)
{
   rt_signal_t *s = container_of(ss, rt_signal_t, shared);

   TRACE("_driving_value %s offset=%d count=%d",
         istr(tree_ident(s->where)), offset, count);

   void *result = rt_tmp_alloc(s->shared.size);

   uint8_t *p = result;
   rt_nexus_t *n = rt_split_nexus(s, offset, count);
   for (; count > 0; n = n->chain) {
      rt_source_t *src = NULL;
      if (n->n_sources > 0) {
         for (src = &(n->sources); src; src = src->chain_input) {
            if (src->tag == SOURCE_DRIVER && src->u.driver.proc == active_proc)
               break;
         }
      }

      if (src == NULL)
         rt_msg(NULL, fatal, "process %s does not contain a driver for %s",
                istr(active_proc->name), istr(tree_ident(s->where)));

      memcpy(p, rt_value_ptr(n, &(src->u.driver.waveforms.value)),
             n->width * n->size);
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
         istr(tree_ident(s->where)), offset, count);

   rt_nexus_t *n = rt_split_nexus(s, offset, count);
   for (; count > 0; n = n->chain) {
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
         istr(tree_ident(s->where)), offset, count);

   rt_nexus_t *n = rt_split_nexus(s, offset, count);
   for (; count > 0; n = n->chain) {
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
      else {
         // This is to support closing a file implicitly when the
         // design is reset
         fclose(*fp);
      }
   }

   char *fname LOCAL = xmalloc(name_len + 1);
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
         rt_msg(NULL, fatal, "failed to open %s: %s", fname, last_os_error());
      else {
         switch (errno) {
         case ENOENT:
            *status = 2;   // NAME_ERROR
            break;
         case EPERM:
            *status = 3;   // MODE_ERROR
            break;
         default:
            rt_msg(NULL, fatal, "%s: %s", fname, last_os_error());
         }
      }
   }
}

DLLEXPORT
void _file_write(void **_fp, uint8_t *data, int32_t len)
{
   FILE **fp = (FILE **)_fp;

   if (*fp == NULL)
      rt_msg(NULL, fatal, "write to closed file");

   fwrite(data, 1, len, *fp);
}

DLLEXPORT
void _file_read(void **_fp, uint8_t *data, int32_t size, int32_t count,
                int32_t *out)
{
   FILE **fp = (FILE **)_fp;

   if (*fp == NULL)
      rt_msg(NULL, fatal, "read from closed file");

   size_t n = fread(data, size, count, *fp);
   if (out != NULL)
      *out = n;
}

DLLEXPORT
void _file_close(void **_fp)
{
   FILE **fp = (FILE **)_fp;

   TRACE("_file_close fp=%p", fp);

   if (*fp == NULL)
      rt_msg(NULL, fatal, "attempt to close already closed file");

   fclose(*fp);
   *fp = NULL;
}

DLLEXPORT
int8_t _endfile(void *_f)
{
   FILE *f = _f;

   if (f == NULL)
      rt_msg(NULL, fatal, "ENDFILE called on closed file");

   int c = fgetc(f);
   if (c == EOF)
      return 1;
   else {
      ungetc(c, f);
      return 0;
   }
}

DLLEXPORT
void __nvc_flush(FILE *f)
{
   if (f == NULL)
      rt_msg(NULL, fatal, "FLUSH called on closed file");

   fflush(f);
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
   e->when            = now + delta;
   e->kind            = EVENT_PROCESS;
   e->proc.wakeup_gen = wake->wakeable.wakeup_gen;
   e->proc.proc       = wake;

   deltaq_insert(e);
}

static void deltaq_insert_driver(uint64_t delta, rt_nexus_t *nexus,
                                 rt_source_t *source)
{
   event_t *e = rt_alloc(event_stack);
   e->when          = now + delta;
   e->kind          = EVENT_DRIVER;
   e->driver.nexus  = nexus;
   e->driver.source = source;

   deltaq_insert(e);
}

#if TRACE_DELTAQ > 0
static void deltaq_walk(uint64_t key, void *user, void *context)
{
   event_t *e = user;

   fprintf(stderr, "%s\t", fmt_time(e->when));
   switch (e->kind) {
   case EVENT_DRIVER:
      fprintf(stderr, "driver\t %s\n",
              istr(tree_ident(e->driver.nexus->signal->where)));
      break;
   case EVENT_PROCESS:
      fprintf(stderr, "process\t %s%s\n", istr(e->proc.proc->name),
              (e->proc.wakeup_gen == e->proc.proc->wakeable.wakeup_gen)
              ? "" : " (stale)");
      break;
   case EVENT_TIMEOUT:
      fprintf(stderr, "timeout\t %p %p\n", e->timeout.fn, e->timeout.user);
      break;
   }
}

static void deltaq_dump(void)
{
   for (event_t *e = delta_driver; e != NULL; e = e->delta_chain)
      fprintf(stderr, "delta\tdriver\t %s\n",
              istr(tree_ident(e->driver.nexus->signal->where)));

   for (event_t *e = delta_proc; e != NULL; e = e->delta_chain)
      fprintf(stderr, "delta\tprocess\t %s%s\n",
              istr(tree_ident(e->proc.proc->where)),
              (e->proc.wakeup_gen == e->proc.proc->wakeable.wakeup_gen)
              ? "" : " (stale)");

   heap_walk(eventq_heap, deltaq_walk, NULL);
}
#endif

static res_memo_t *rt_memo_resolution_fn(rt_signal_t *signal,
                                         rt_resolution_t *resolution)
{
   // Optimise some common resolution functions by memoising them

   res_memo_t *memo = hash_get(res_memo_hash, resolution->closure.fn);
   if (memo != NULL)
      return memo;

   memo = xmalloc(sizeof(res_memo_t));
   memo->closure = resolution->closure;
   memo->flags   = resolution->flags;
   memo->ileft   = resolution->ileft;

   hash_put(res_memo_hash, memo->closure.fn, memo);

   if (resolution->nlits == 0 || resolution->nlits > 16)
      return memo;

   init_side_effect = SIDE_EFFECT_DISALLOW;

   // Memoise the function for all two value cases

   for (int i = 0; i < resolution->nlits; i++) {
      for (int j = 0; j < resolution->nlits; j++) {
         int8_t args[2] = { i, j };
         ffi_uarray_t u = { args, { { memo->ileft, 2 } } };
         ffi_call(&(memo->closure), &u, sizeof(u), &(memo->tab2[i][j]), 1);
         RT_ASSERT(memo->tab2[i][j] < resolution->nlits);
      }
   }

   // Memoise the function for all single value cases and determine if the
   // function behaves like the identity function

   bool identity = true;
   for (int i = 0; i < resolution->nlits; i++) {
      int8_t args[1] = { i };
      ffi_uarray_t u = { args, { { memo->ileft, 1 } } };
      ffi_call(&(memo->closure), &u, sizeof(u), &(memo->tab1[i]), 1);
      identity = identity && (memo->tab1[i] == i);
   }

   if (init_side_effect != SIDE_EFFECT_OCCURRED) {
      memo->flags |= R_MEMO;
      if (identity)
         memo->flags |= R_IDENT;
   }

   TRACE("memoised resolution function %p for type %s",
         resolution->closure.fn, type_pp(tree_type(signal->where)));

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

static rt_value_t rt_alloc_value(rt_nexus_t *n)
{
   rt_value_t result = {};

   const size_t valuesz = n->size * n->width;
   if (valuesz > sizeof(rt_value_t)) {
      if (n->free_value != NULL) {
         result.ext = n->free_value;
         n->free_value = NULL;
      }
      else
         result.ext = xmalloc(valuesz);
   }

   return result;
}

static inline const uint8_t *rt_value_ptr(rt_nexus_t *n, rt_value_t *v)
{
   const size_t valuesz = n->width * n->size;
   if (valuesz <= sizeof(rt_value_t))
      return v->bytes;
   else
      return v->ext;
}

static void rt_copy_value_ptr(rt_nexus_t *n, rt_value_t *v, const void *p)
{
   const size_t valuesz = n->width * n->size;
   if (valuesz <= sizeof(rt_value_t))
      v->qword = *(uint64_t *)p;
   else
      memcpy(v->ext, p, valuesz);
}

static inline bool rt_cmp_values(rt_nexus_t *n, rt_value_t a, rt_value_t b)
{
   const size_t valuesz = n->width * n->size;
   if (valuesz <= sizeof(rt_value_t))
      return a.qword == b.qword;
   else
      return memcmp(a.ext, b.ext, valuesz) == 0;
}

static void rt_free_value(rt_nexus_t *n, rt_value_t v)
{
#if 0
   if (v != NULL) {
      RT_ASSERT(v->next == NULL);
      v->next = n->free_values;
      n->free_values = v;
   }
#else
   const size_t valuesz = n->width * n->size;
   if (valuesz > sizeof(rt_value_t)) {
      if (n->free_value == NULL)
         n->free_value = v.ext;
      else {
         //printf("free a %zu byte value!\n", valuesz);
         free(v.ext);
      }
   }
#endif
}

static void *rt_tmp_alloc(size_t sz)
{
   // Allocate sz bytes that will be freed by the active process

   uint8_t *ptr = (uint8_t *)_tmp_stack + _tmp_alloc;
   _tmp_alloc += sz;
   return ptr;
}

static void rt_sched_event(sens_list_t **list, rt_wakeable_t *obj, bool recur)
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
      sens_list_t *node = rt_alloc(sens_list_stack);
      node->wake       = obj;
      node->wakeup_gen = obj->wakeup_gen;
      node->next       = *list;
      node->reenq      = (recur ? list : NULL);

      *list = node;
   }
   else {
      // Reuse the stale entry
      it->wakeup_gen = obj->wakeup_gen;
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

static void rt_scope_deps_cb(ident_t unit_name, void *__ctx)
{
   rt_scope_t ***tailp = __ctx;

   tree_t unit = lib_get_qualified(unit_name);
   if (unit == NULL) {
      warnf("missing dependency %s", istr(unit_name));
      return;
   }

   if (hash_get(scopes, unit) != NULL)
      return;

   const tree_kind_t kind = tree_kind(unit);
   if (kind != T_PACKAGE && kind != T_PACK_INST) {
      tree_walk_deps(unit, rt_scope_deps_cb, tailp);
      return;
   }

   rt_scope_t *s = xcalloc(sizeof(rt_scope_t));
   s->where = unit;
   s->name  = tree_ident(unit);
   s->kind  = SCOPE_PACKAGE;

   hash_put(scopes, unit, s);

   tree_walk_deps(unit, rt_scope_deps_cb, tailp);

   if (kind == T_PACKAGE) {
      ident_t body_i = ident_prefix(unit_name, ident_new("body"), '-');
      tree_t body = lib_get_qualified(body_i);
      if (body != NULL)
         tree_walk_deps(body, rt_scope_deps_cb, tailp);
   }

   **tailp = s;
   *tailp = &(s->chain);
}

static rt_scope_t *rt_scope_for_block(tree_t block, ident_t prefix)
{
   rt_scope_t *s = xcalloc(sizeof(rt_scope_t));
   s->where = block;
   s->name  = ident_prefix(prefix, tree_ident(block), '.');
   s->kind  = SCOPE_INSTANCE;

   hash_put(scopes, block, s);

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
         p->where = d;
         p->name  = ident_prefix(s->name, tree_ident(d), '.');
         p->kind  = SCOPE_PACKAGE;

         hash_put(scopes, d, p);

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
            rt_scope_t *c = rt_scope_for_block(t, s->name);
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
            p->proc_fn   = jit_find_symbol(istr(sym), true);
            p->tmp_stack = NULL;
            p->tmp_alloc = 0;
            p->scope     = s;

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

static void rt_setup(tree_t top)
{
   now = 0;
   iteration = -1;
   active_proc = NULL;
   active_scope = NULL;
   force_stop = false;
   can_create_delta = true;
   nexus_tail = &nexuses;

   RT_ASSERT(resume == NULL);

   rt_free_delta_events(delta_proc);
   rt_free_delta_events(delta_driver);

   eventq_heap = heap_new(512);

   scopes = hash_new(256, true);

   root = xcalloc(sizeof(rt_scope_t));
   root->kind  = SCOPE_ROOT;
   root->where = top;

   rt_scope_t **tailp = &(root->child);
   tree_walk_deps(top, rt_scope_deps_cb, &tailp);

   *tailp = rt_scope_for_block(tree_stmt(top, 0), lib_name(lib_work()));

   res_memo_hash = hash_new(128, true);
}

static void rt_reset(rt_proc_t *proc)
{
   TRACE("reset process %s", istr(proc->name));

   assert(proc->tmp_stack == NULL);

   _tmp_stack = global_tmp_stack;
   _tmp_alloc = global_tmp_alloc;

   active_proc = proc;
   active_scope = proc->scope;

   proc->privdata = (*proc->proc_fn)(NULL, proc->scope->privdata);
   global_tmp_alloc = _tmp_alloc;
}

static void rt_run(rt_proc_t *proc)
{
   TRACE("run %sprocess %s", proc->privdata ? "" :  "stateless ",
         istr(proc->name));

   if (proc->tmp_stack != NULL) {
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

   // Stateless processes have NULL privdata so pass a dummy pointer
   // value in so it can be distinguished from a reset
   void *state = proc->privdata ?: (void *)-1;

   (*proc->proc_fn)(state, proc->scope->privdata);
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

static const void *rt_source_data(rt_nexus_t *nexus, rt_source_t *src)
{
   switch (src->tag) {
   case SOURCE_DRIVER:
      if (unlikely(src->u.driver.waveforms.null))
         return NULL;
      else
         return rt_value_ptr(nexus, &(src->u.driver.waveforms.value));

   case SOURCE_PORT:
      if (likely(src->u.port.conv_func == NULL))
         return src->u.port.input->resolved;
      else {
         rt_signal_t *i0 = src->u.port.input->signal;
         rt_conv_func_t *cf = src->u.port.conv_func;

         bool incopy = false;
         void *indata;
         size_t insz;
         if (i0->parent->kind == SCOPE_SIGNAL) {
            indata = rt_composite_signal(i0, &insz);
            incopy = true;
         }
         else {
            indata = i0->shared.data;
            insz   = i0->shared.size;
         }

         TRACE("call conversion function %p insz=%zu outsz=%zu",
               cf->closure.fn, insz, cf->bufsz);

         ffi_call(&(cf->closure), indata, insz, cf->buffer, cf->bufsz);

         if (incopy) free(indata);

         const unsigned offset = src->u.port.output->signal->shared.offset;
         return src->u.port.conv_func->buffer + offset;
      }
   }

   return NULL;
}

static void *rt_resolve_nexus_slow(rt_nexus_t *nexus)
{
   int nonnull = 0;
   for (rt_source_t *s = &(nexus->sources); s; s = s->chain_input) {
      if (s->tag == SOURCE_PORT || !s->u.driver.waveforms.null)
         nonnull++;
   }

   if (nonnull == 0 && (nexus->flags & NET_F_REGISTER)) {
      return nexus->resolved;
   }
   else if (nexus->resolution->flags & R_COMPOSITE) {
      // Call resolution function of composite type

      rt_scope_t *scope = nexus->signal->parent;
      while (scope->parent->kind == SCOPE_SIGNAL)
         scope = scope->parent;

      TRACE("resolved composite signal needs %d bytes", scope->size);

      uint8_t *inputs LOCAL = xmalloc(nonnull * scope->size);
      void *resolved = rt_resolution_buffer(scope->size);

      rt_copy_sub_signal_sources(scope, inputs);

      const int32_t left = nexus->resolution->ileft;
      ffi_uarray_t u = { inputs, { { left, nonnull } } };
      ffi_call(&(nexus->resolution->closure), &u, sizeof(u),
               resolved, scope->size);

      const ptrdiff_t noff =
         nexus->resolved - (void *)nexus->signal->shared.data;
      return resolved + nexus->signal->shared.offset + noff;
   }
   else {
      void *resolved = rt_resolution_buffer(nexus->width * nexus->size);

      for (int j = 0; j < nexus->width; j++) {
#define CALL_RESOLUTION_FN(type) do {                                   \
            type vals[nonnull];                                         \
            unsigned o = 0;                                             \
            for (rt_source_t *s = &(nexus->sources); s;                 \
                 s = s->chain_input) {                                  \
               const void *data = rt_source_data(nexus, s);             \
               if (data != NULL)                                        \
                  vals[o++] = ((const type *)data)[j];                  \
            }                                                           \
            type *r = (type *)resolved;                                 \
            const int32_t left = nexus->resolution->ileft;              \
            ffi_uarray_t u = { vals, { { left, nonnull } } };           \
            ffi_call(&(nexus->resolution->closure), &u, sizeof(u),      \
                     &(r[j]), sizeof(r[j]));                            \
         } while (0)

         FOR_ALL_SIZES(nexus->size, CALL_RESOLUTION_FN);
      }

      return resolved;
   }
}

static const void *rt_resolve_nexus_fast(rt_nexus_t *nexus)
{
   if (unlikely(nexus->flags & NET_F_FORCED)) {
      return rt_value_ptr(nexus, &(nexus->forcing));
   }
   else if (unlikely(nexus->flags & NET_F_DISCONNECTED)) {
      // Some drivers may have null transactions
      return rt_resolve_nexus_slow(nexus);
   }
   else if (nexus->resolution == NULL && nexus->n_sources == 0) {
      // Always maintains initial driver value
      return nexus->resolved;
   }
   else if (nexus->resolution == NULL) {
      return rt_source_data(nexus, &(nexus->sources));
   }
   else if ((nexus->resolution->flags & R_IDENT) && (nexus->n_sources == 1)) {
      // Resolution function behaves like identity for a single driver
      return rt_source_data(nexus, &(nexus->sources));
   }
   else if ((nexus->resolution->flags & R_MEMO) && (nexus->n_sources == 1)) {
      // Resolution function has been memoised so do a table lookup

      void *resolved = rt_resolution_buffer(nexus->width * nexus->size);
      const void *data = rt_source_data(nexus, &(nexus->sources));

      for (int j = 0; j < nexus->width; j++) {
         const int index = ((uint8_t *)data)[j];
         const int8_t r = nexus->resolution->tab1[index];
         ((int8_t *)resolved)[j] = r;
      }

      return resolved;
   }
   else if ((nexus->resolution->flags & R_MEMO) && (nexus->n_sources == 2)) {
      // Resolution function has been memoised so do a table lookup

      void *resolved = rt_resolution_buffer(nexus->width * nexus->size);

      const char *p0 = rt_source_data(nexus, &(nexus->sources));
      const char *p1 = rt_source_data(nexus, nexus->sources.chain_input);

      for (int j = 0; j < nexus->width; j++) {
         const int driving[2] = { p0[j], p1[j] };
         const int8_t r = nexus->resolution->tab2[driving[0]][driving[1]];
         ((int8_t *)resolved)[j] = r;
      }

      return resolved;
   }
   else {
      // Must actually call resolution function in general case
      return rt_resolve_nexus_slow(nexus);
   }
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
}

static void rt_reset_scope(rt_scope_t *s)
{
   if (s->kind == SCOPE_INSTANCE || s->kind == SCOPE_PACKAGE) {
      TRACE("reset scope %s", istr(s->name));

      void *privdata = s->parent ? s->parent->privdata : NULL;
      active_scope = s;
      signals_tail = &(s->signals);

      s->privdata = rt_call_module_reset(s->name, privdata);

      active_scope = NULL;
      signals_tail = NULL;
   }

   for (rt_scope_t *c = s->child; c != NULL; c = c->chain)
      rt_reset_scope(c);

   for (rt_proc_t *p = s->procs; p != NULL; p = p->chain)
      rt_reset(p);
}

static void rt_driver_initial(rt_nexus_t *nexus)
{
   // Assign the initial value of the drivers
   if (nexus->n_sources > 0) {
      for (rt_source_t *s = &(nexus->sources); s; s = s->chain_input) {
         if (s->tag == SOURCE_DRIVER)
            rt_copy_value_ptr(nexus, &(s->u.driver.waveforms.value),
                              nexus->resolved);
      }
   }

   const void *resolved;
   if (nexus->n_sources > 0)
      resolved = rt_resolve_nexus_fast(nexus);
   else
      resolved = nexus->resolved;

   nexus->event_delta = nexus->active_delta = -1;
   nexus->last_event = nexus->last_active = TIME_HIGH;

   TRACE("%s initial value %s", istr(tree_ident(nexus->signal->where)),
         fmt_nexus(nexus, resolved));

   rt_propagate_nexus(nexus, resolved);
}

static void rt_initial(tree_t top)
{
   // Initialisation is described in LRM 93 section 12.6.4

   rt_reset_scope(root);

#if TRACE_SIGNALS > 0
   if (trace_on)
      rt_dump_signals(root);
#endif

   TRACE("calculate initial driver values");

   heap_t *q = heap_new(MAX(profile.n_signals + 1, 128));

   for (rt_nexus_t *n = nexuses; n != NULL; n = n->chain)
      heap_insert(q, n->rank, n);

   init_side_effect = SIDE_EFFECT_ALLOW;

   while (heap_size(q) > 0) {
      rt_nexus_t *n = heap_extract_min(q);
      rt_driver_initial(n);
   }

   heap_free(q);

   TRACE("used %d bytes of global temporary stack", global_tmp_alloc);
}

static void rt_trace_wakeup(rt_wakeable_t *obj)
{
   if (unlikely(trace_on)) {
      switch (obj->kind) {
      case W_PROC:
         TRACE("wakeup %sprocess %s", obj->postponed ? "postponed " : "",
               istr(container_of(obj, rt_proc_t, wakeable)->name));
         break;

      case W_WATCH:
         TRACE("wakeup %svalue change callback %p",
               obj->postponed ? "postponed " : "",
               container_of(obj, rt_watch_t, wakeable)->fn);
         break;

      case W_IMPLICIT:
         TRACE("wakeup implicit signal %s",
               istr(tree_ident(container_of(obj, rt_implicit_t, wakeable)
                               ->signal.where)));
      }
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

   if (sl->wakeup_gen == sl->wake->wakeup_gen || sl->reenq != NULL) {
      rt_trace_wakeup(sl->wake);

      sens_list_t **enq = NULL;
      if (sl->wake->postponed) {
         switch (sl->wake->kind) {
         case W_PROC: enq = &postponed; break;
         case W_WATCH: enq = &postponed_watch; break;
         case W_IMPLICIT: assert(false); break;
         }
      }
      else {
         switch (sl->wake->kind) {
         case W_PROC: enq = &resume; break;
         case W_WATCH: enq = &resume_watch; break;
         case W_IMPLICIT: enq = &implicit; break;
         }
      }

      sl->next = *enq;
      *enq = sl;

      ++(sl->wake->wakeup_gen);
      sl->wake->pending = true;
   }
   else
      rt_free(sens_list_stack, sl);
}

static void rt_sched_driver(rt_nexus_t *nexus, uint64_t after,
                            uint64_t reject, rt_value_t value, bool null)
{
   if (unlikely(reject > after))
      fatal("signal %s pulse reject limit %s is greater than "
            "delay %s", istr(tree_ident(nexus->signal->where)),
            fmt_time(reject), fmt_time(after));

   // Try to find this process in the list of existing drivers
   rt_source_t *d;
   for (d = &(nexus->sources); d; d = d->chain_input) {
      if (d->tag == SOURCE_DRIVER && d->u.driver.proc == active_proc)
         break;
   }
   RT_ASSERT(d != NULL);

   waveform_t *w = rt_alloc(waveform_stack);
   w->when  = now + after;
   w->next  = NULL;
   w->value = value;
   w->null  = null;

   waveform_t *last = &(d->u.driver.waveforms);
   waveform_t *it   = last->next;
   while (it != NULL && it->when < w->when) {
      // If the current transaction is within the pulse rejection interval
      // and the value is different to that of the new transaction then
      // delete the current transaction
      RT_ASSERT(it->when >= now);
      if ((it->when >= w->when - reject)
          && !rt_cmp_values(nexus, it->value, w->value)) {
         waveform_t *next = it->next;
         last->next = next;
         rt_free_value(nexus, it->value);
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
      rt_free_value(nexus, it->value);

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
   nexus->last_event = nexus->last_active = now;
   nexus->event_delta = nexus->active_delta = iteration;

   // Wake up everything on the pending list
   for (sens_list_t *it = nexus->pending, *next; it != NULL; it = next) {
      next = it->next;
      rt_wakeup(it);
      nexus->pending = next;
   }

   if (nexus->n_sources > 0) {
      for (rt_source_t *s = &(nexus->sources); s; s = s->chain_input) {
         if (s->tag == SOURCE_PORT)
            rt_notify_event(s->u.port.input);
      }
   }
}

static void rt_notify_active(rt_nexus_t *nexus)
{
   nexus->last_active = now;
   nexus->active_delta = iteration;

   if (nexus->n_sources > 0) {
      for (rt_source_t *s = &(nexus->sources); s; s = s->chain_input) {
         if (s->tag == SOURCE_PORT)
            rt_notify_active(s->u.port.input);
      }
   }
}

static void rt_update_nexus(rt_nexus_t *nexus)
{
   const void *resolved = rt_resolve_nexus_fast(nexus);
   const size_t valuesz = nexus->size * nexus->width;

   TRACE("update nexus %s resolved=%s",
         istr(tree_ident(nexus->signal->where)),
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
   rt_update_nexus(nexus);

   for (rt_source_t *o = nexus->outputs; o; o = o->chain_output) {
      RT_ASSERT(o->tag == SOURCE_PORT);
      TRACE("active nexus %s sources nexus %s",
            istr(tree_ident(nexus->signal->where)),
            istr(tree_ident(o->u.port.output->signal->where)));
      RT_ASSERT(nexus->rank < o->u.port.output->rank);
      rt_push_active_nexus(o->u.port.output);
   }
}

static void rt_update_driver(rt_nexus_t *nexus, rt_source_t *source)
{
   if (likely(source != NULL)) {
      RT_ASSERT(source->tag == SOURCE_DRIVER);
      waveform_t *w_now  = &(source->u.driver.waveforms);
      waveform_t *w_next = w_now->next;

      if (likely((w_next != NULL) && (w_next->when == now))) {
         rt_free_value(nexus, w_now->value);
         *w_now = *w_next;
         rt_free(waveform_stack, w_next);
         rt_push_active_nexus(nexus);
      }
      else
         RT_ASSERT(w_now != NULL);
   }
   else if (nexus->flags & NET_F_FORCED)
      rt_push_active_nexus(nexus);
}

static void rt_update_implicit_signal(rt_implicit_t *imp)
{
   int8_t r;
   ffi_call(imp->closure, NULL, 0, &r, sizeof(r));

   TRACE("implicit signal %s guard expression %d",
         istr(tree_ident(imp->signal.where)), r);

   RT_ASSERT(imp->signal.n_nexus == 1);
   rt_nexus_t *n0 = &(imp->signal.nexus);

   if (*(int8_t *)n0->resolved != r) {
      rt_propagate_nexus(n0, &r);
      rt_notify_event(n0);
   }
   else
      rt_notify_active(n0);
}

static bool rt_stale_event(event_t *e)
{
   return (e->kind == EVENT_PROCESS)
      && (e->proc.wakeup_gen != e->proc.proc->wakeable.wakeup_gen);
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
         ++(e->proc.proc->wakeable.wakeup_gen);
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
             opt_get_int(OPT_STOP_DELTA));

   for (sens_list_t *it = resume; it != NULL; it = it->next) {
      if (it->wake->kind == W_PROC) {
         rt_proc_t *proc = container_of(it->wake, rt_proc_t, wakeable);
         const loc_t *l = tree_loc(proc->where);
         tb_printf(buf, "  %-30s %s line %d\n", istr(proc->name),
                   loc_file_str(l), l->first_line);
      }
   }

   tb_printf(buf, "You can increase this limit with --stop-delta");

   fatal("%s", tb_get(buf));
}

static void rt_resume(sens_list_t **list)
{
   sens_list_t *it = *list;
   while (it != NULL) {
      if (it->wake->pending) {
         switch (it->wake->kind) {
         case W_PROC:
            {
               rt_proc_t *proc = container_of(it->wake, rt_proc_t, wakeable);
               rt_run(proc);
            }
            break;
         case W_WATCH:
            {
               rt_watch_t *w = container_of(it->wake, rt_watch_t, wakeable);
               (*w->fn)(now, w->signal, w, w->user_data);
            }
            break;
         case W_IMPLICIT:
            {
               rt_implicit_t *imp =
                  container_of(it->wake, rt_implicit_t, wakeable);
               rt_update_implicit_signal(imp);
            }
         }
         it->wake->pending = false;
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

#if TRACE_SIGNALS > 0
   if (trace_on)
      rt_dump_signals(root);
#endif
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
      (*event->timeout.fn)(now, event->timeout.user);
      rt_free(event_stack, event);
   }

   while ((event = rt_pop_run_queue(&driverq))) {
      rt_update_driver(event->driver.nexus, event->driver.source);
      rt_free(event_stack, event);
   }

   rt_resume(&implicit);

   while ((event = rt_pop_run_queue(&procq))) {
      rt_run(event->proc.proc);
      rt_free(event_stack, event);
   }

   if (unlikely((stop_delta > 0) && (iteration == stop_delta)))
      rt_iteration_limit();

   // Run all non-postponed event callbacks
   rt_resume(&resume_watch);

   // Run all processes that resumed because of signal events
   rt_resume(&resume);
   rt_global_event(RT_END_OF_PROCESSES);

   if (!rt_next_cycle_is_delta()) {
      can_create_delta = false;
      rt_global_event(RT_LAST_KNOWN_DELTA_CYCLE);

      // Run any postponed processes
      rt_resume(&postponed);

      // Execute all postponed event callbacks
      rt_resume(&postponed_watch);

      can_create_delta = true;
   }
}

static void rt_cleanup_nexus(rt_nexus_t *n)
{
   rt_free_value(n, n->forcing);

   bool must_free = false;
   for (rt_source_t *s = &(n->sources), *tmp; s; s = tmp, must_free = true) {
      tmp = s->chain_input;

      switch (s->tag) {
      case SOURCE_DRIVER:
         rt_free_value(n, s->u.driver.waveforms.value);

         for (waveform_t *it = s->u.driver.waveforms.next, *next;
              it; it = next) {
            rt_free_value(n, it->value);
            next = it->next;
            rt_free(waveform_stack, it);
         }
         break;

      case SOURCE_PORT:
         if (s->u.port.conv_func != NULL) {
            RT_ASSERT(s->u.port.conv_func->refcnt > 0);
            if (--(s->u.port.conv_func->refcnt) == 0)
               free(s->u.port.conv_func);
         }
         break;
      }

      if (must_free) free(s);
   }

   free(n->free_value);

   while (n->pending != NULL) {
      sens_list_t *next = n->pending->next;
      rt_free(sens_list_stack, n->pending);
      n->pending = next;
   }
}

static void rt_cleanup_signal(rt_signal_t *s)
{
   rt_nexus_t *n = &(s->nexus), *tmp;
   for (int i = 0; i < s->n_nexus; i++, n = tmp) {
      tmp = n->chain;
      rt_cleanup_nexus(n);
      if (i > 0) free(n);
   }

   if (s->index != NULL)
      ihash_free(s->index);

   if (s->flags & NET_F_IMPLICIT) {
      rt_implicit_t *imp = container_of(s, rt_implicit_t, signal);
      ffi_unref_closure(imp->closure);
      free(imp);
   }
   else
      free(s);
}

static void rt_cleanup_scope(rt_scope_t *scope)
{
   for (rt_proc_t *it = scope->procs, *tmp; it; it = tmp) {
      tmp = it->chain;
      free(it->privdata);
      free(it);
   }

   for (rt_signal_t *it = scope->signals, *tmp; it; it = tmp) {
      tmp = it->chain;
      rt_cleanup_signal(it);
   }

   for (rt_scope_t *it = scope->child, *tmp; it; it = tmp) {
      tmp = it->chain;
      rt_cleanup_scope(it);
   }

   free(scope->privdata);
   free(scope);
}

static void rt_cleanup(void)
{
   RT_ASSERT(resume == NULL);

   while (heap_size(eventq_heap) > 0)
      rt_free(event_stack, heap_extract_min(eventq_heap));

   rt_free_delta_events(delta_proc);
   rt_free_delta_events(delta_driver);

   heap_free(eventq_heap);
   eventq_heap = NULL;

   hash_iter_t it = HASH_BEGIN;
   const void *key;
   void *value;
   while (hash_iter(res_memo_hash, &it, &key, &value))
      free(value);

   hash_free(res_memo_hash);
   res_memo_hash = NULL;

   rt_cleanup_scope(root);
   root = NULL;

   nexuses = NULL;
   nexus_tail = NULL;

   while (watches != NULL) {
      rt_watch_t *next = watches->chain_all;
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
}

static bool rt_stop_now(uint64_t stop_time)
{
   if ((delta_driver != NULL) || (delta_proc != NULL))
      return false;
   else if (heap_size(eventq_heap) == 0)
      return true;
   else if (force_stop)
      return true;
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
      //tb_printf(tb, "Nexuses: %-5d      Simple signals: %d (1:%.1f)\n",
      //          nnexus, profile.n_simple, (double)profile.n_simple / nnexus);
      tb_printf(tb, "Mapping:  direct:%d search:%d divide:%d\n",
                profile.nmap_direct, profile.nmap_search, profile.nmap_divide);
      //tb_printf(tb, "Processes: %-5d    Scopes: %d\n",
      // profile.n_procs, n_scopes);
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
#ifdef __SANITIZE_THREAD__
   _Exit(1);
#else
   if (active_proc != NULL)
      rt_msg(NULL, fatal,
             "interrupted in process %s at %s+%d",
             istr(active_proc->name), fmt_time(now), iteration);
   else
      fatal("interrupted");
#endif
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

#if RT_DEBUG && !defined NDEBUG
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

   trace_on = opt_get_int(OPT_RT_TRACE);
   profiling = opt_get_int(OPT_RT_PROFILE);

   if (profiling) {
      memset(&profile, '\0', sizeof(profile));
      profile.runq_min = ~0;
   }

   event_stack     = rt_alloc_stack_new(sizeof(event_t), "event");
   waveform_stack  = rt_alloc_stack_new(sizeof(waveform_t), "waveform");
   sens_list_stack = rt_alloc_stack_new(sizeof(sens_list_t), "sens_list");
   watch_stack     = rt_alloc_stack_new(sizeof(rt_watch_t), "watch");
   callback_stack  = rt_alloc_stack_new(sizeof(callback_t), "callback");

   global_tmp_stack = rt_map_secondary_stack(OPT_GLOBAL_STACK);
   proc_tmp_stack   = rt_map_secondary_stack(OPT_PROC_STACK);

   global_tmp_alloc = 0;

   rt_reset_coverage(top);

   nvc_rusage(&ready_rusage);
}

void rt_end_of_tool(tree_t top)
{
   rt_cleanup();
   rt_emit_coverage(top);

   jit_shutdown();

   if (opt_get_int(OPT_RT_STATS) || profiling)
      rt_stats_print();
}

void rt_run_sim(uint64_t stop_time)
{
   const int stop_delta = opt_get_int(OPT_STOP_DELTA);

   wave_restart();

   rt_global_event(RT_START_OF_SIMULATION);
   while (!rt_stop_now(stop_time))
      rt_cycle(stop_delta);
   rt_global_event(RT_END_OF_SIMULATION);
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
   e->kind         = EVENT_TIMEOUT;
   e->timeout.fn   = fn;
   e->timeout.user = user;

   deltaq_insert(e);
}

rt_watch_t *rt_set_event_cb(rt_signal_t *s, sig_event_fn_t fn, void *user,
                            bool postponed)
{
   if (fn == NULL) {
      // Find the first entry in the watch list and disable it
      for (rt_watch_t *it = watches; it != NULL; it = it->chain_all) {
         if ((it->signal == s) && (it->user_data == user)) {
            it->wakeable.pending = true;   // TODO: not a good way of doing this
            break;
         }
      }

      return NULL;
   }
   else {
      rt_watch_t *w = rt_alloc(watch_stack);
      RT_ASSERT(w != NULL);
      w->signal        = s;
      w->fn            = fn;
      w->chain_all     = watches;
      w->user_data     = user;

      w->wakeable.kind       = W_WATCH;
      w->wakeable.postponed  = postponed;
      w->wakeable.pending    = false;
      w->wakeable.wakeup_gen = 0;

      watches = w;

      rt_nexus_t *n = &(w->signal->nexus);
      for (int i = 0; i < s->n_nexus; i++, n = n->chain)
         rt_sched_event(&(n->pending), &(w->wakeable), true);

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

size_t rt_signal_string(rt_signal_t *s, const char *map, char *buf, size_t max)
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

size_t rt_signal_expand(rt_signal_t *s, int offset, uint64_t *buf, size_t max)
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

const void *rt_signal_value(rt_signal_t *s, int offset)
{
   const uint8_t *ptr = s->shared.data;
   for (rt_nexus_t *n = &(s->nexus); offset > 0; n = n->chain) {
      ptr += n->width * n->size;
      offset -= n->width;
   }
   assert(offset == 0);

   return ptr;
}

rt_signal_t *rt_find_signal(rt_scope_t *scope, tree_t decl)
{
   for (rt_signal_t *s = scope->signals; s; s = s->chain) {
      if (s->where == decl)
         return s;
   }

   return NULL;
}

rt_scope_t *rt_find_scope(tree_t container)
{
   if (scopes == NULL)
      return NULL;
   else
      return hash_get(scopes, container);
}

rt_scope_t *rt_child_scope(rt_scope_t *scope, tree_t decl)
{
   for (rt_scope_t *s = scope->child; s != NULL; s = s->chain) {
      if (s->where == decl)
         return s;
   }

   return NULL;
}

bool rt_force_signal(rt_signal_t *s, const uint64_t *buf, size_t count,
                     bool propagate)
{
   TRACE("force signal %s to %"PRIu64"%s propagate=%d",
         istr(tree_ident(s->where)), buf[0], count > 1 ? "..." : "", propagate);

   RT_ASSERT(!propagate || can_create_delta);

   int offset = 0;
   rt_nexus_t *n = rt_split_nexus(s, offset, count);
   for (; count > 0; n = n->chain) {
      if (n->flags & NET_F_FORCED)
         rt_free_value(n, n->forcing);

      n->flags |= NET_F_FORCED;
      n->forcing = rt_alloc_value(n);

#define SIGNAL_FORCE_EXPAND_U64(type) do {                              \
         type *dp = (type *)rt_value_ptr(n, &(n->forcing));             \
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
