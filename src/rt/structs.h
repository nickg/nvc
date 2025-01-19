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

#ifndef _RT_STRUCTS_H
#define _RT_STRUCTS_H

#include "prim.h"
#include "array.h"
#include "mask.h"
#include "jit/jit.h"
#include "jit/jit-ffi.h"
#include "rt/mspace.h"
#include "rt/rt.h"
#include "thread.h"

typedef void *(*value_fn_t)(rt_nexus_t *);

typedef A(rt_scope_t *) scope_list_t;
typedef A(rt_prop_t *) prop_list_t;
typedef A(rt_signal_t *) signal_list_t;
typedef A(rt_proc_t *) proc_list_t;
typedef A(rt_alias_t *) alias_list_t;

typedef enum {
   W_PROC, W_WATCH, W_IMPLICIT, W_PROPERTY, W_TRANSFER,
} wakeable_kind_t;

typedef uint32_t wakeup_gen_t;

typedef enum {
   FUNC_TRIGGER, OR_TRIGGER, CMP_TRIGGER
} trigger_kind_t;

typedef struct _rt_trigger {
   jit_handle_t    handle;
   unsigned        nargs;
   uint64_t        when;
   unsigned        iteration;
   trigger_kind_t  kind;
   rt_trigger_t   *chain;
   jit_scalar_t    result;
   jit_scalar_t    args[0];
} rt_trigger_t;

typedef struct {
   wakeable_kind_t kind : 8;
   unsigned        pending : 1;
   unsigned        postponed : 1;
   unsigned        delayed : 1;
   unsigned        zombie : 1;
   rt_trigger_t   *trigger;
} rt_wakeable_t;

typedef struct _rt_proc {
   rt_wakeable_t  wakeable;
   tree_t         where;
   ident_t        name;
   jit_handle_t   handle;
   tlab_t        *tlab;
   rt_scope_t    *scope;
   mptr_t         privdata;
} rt_proc_t;

STATIC_ASSERT(sizeof(rt_proc_t) <= 128);

typedef struct _rt_prop {
   rt_wakeable_t  wakeable;
   psl_node_t     where;
   ident_t        name;
   jit_handle_t   handle;
   rt_scope_t    *scope;
   bit_mask_t     state;
   bit_mask_t     newstate;
   mptr_t         privdata;
   bool           strong;
} rt_prop_t;

typedef union {
   uint8_t   bytes[8];
   uint64_t  qword;
   void     *ext;
} rt_value_t;

STATIC_ASSERT(sizeof(rt_value_t) == 8);

struct waveform {
   uint64_t    when;
   waveform_t *next;
   rt_value_t  value;
};

STATIC_ASSERT(sizeof(waveform_t) <= 24);

typedef struct {
   unsigned       count;
   unsigned       max;
   rt_wakeable_t *wake[];
} rt_pending_t;

typedef enum {
   SOURCE_DRIVER,
   SOURCE_PORT,
   SOURCE_FORCING,
   SOURCE_DEPOSIT,
   SOURCE_IMPLICIT,
} source_kind_t;

typedef struct {
   rt_proc_t  *proc;
   rt_nexus_t *nexus;
   waveform_t  waveforms;
} rt_driver_t;

typedef struct {
   rt_nexus_t *nexus;
   rt_value_t  result;
} conv_input_t;

typedef struct _rt_conv_func {
   ffi_closure_t  driving;
   ffi_closure_t  effective;
   unsigned       ninputs;
   unsigned       maxinputs;
   conv_input_t  *inputs;
   rt_source_t   *outputs;
   uint64_t       when;
   unsigned       iteration;
   conv_input_t   tail[];
} rt_conv_func_t;

typedef struct {
   rt_nexus_t     *output;
   rt_nexus_t     *input;
   rt_conv_func_t *conv_func;
   rt_value_t      conv_result;
} rt_port_t;

typedef struct {
   rt_nexus_t *nexus;
   rt_value_t  value;
} rt_pseudo_t;

typedef struct _rt_source {
   rt_source_t    *chain_input;
   rt_source_t    *chain_output;
   source_kind_t   tag;
   unsigned        disconnected : 1;
   unsigned        fastqueued : 1;
   unsigned        sigqueued : 1;
   unsigned        pseudoqueued : 1;
   unsigned        was_active : 1;
   union {
      rt_port_t   port;
      rt_driver_t driver;
      rt_pseudo_t pseudo;
   } u;
} rt_source_t;

STATIC_ASSERT(sizeof(rt_source_t) <= 64);

typedef struct {
   ffi_closure_t closure;
   res_flags_t   flags;
   int64_t       ileft;
   int8_t        tab2[16][16];
   int8_t        tab1[16];
} res_memo_t;

typedef struct _rt_nexus {
   rt_nexus_t    *chain;
   rt_signal_t   *signal;
   uint32_t       offset;
   delta_cycle_t  active_delta;
   delta_cycle_t  event_delta;
   uint32_t       width;
   net_flags_t    flags;
   uint8_t        size;
   uint8_t        n_sources;
   uint8_t        rank;
   uint64_t       last_event;
   void          *pending;
   rt_source_t   *outputs;
   void          *free_value;
   rt_source_t    sources;
} rt_nexus_t;

STATIC_ASSERT(sizeof(rt_nexus_t) <= 128);

// The code generator knows the layout of this struct
typedef struct _sig_shared {
   uint32_t    size;
   sig_flags_t flags;
   uint8_t     data[0];
} sig_shared_t;

typedef struct {
   int         how;
   rt_nexus_t *nexus[0];
} rt_index_t;

typedef struct _rt_signal {
   tree_t        where;
   rt_scope_t   *parent;
   rt_index_t   *index;
   res_memo_t   *resolution;
   nvc_lock_t    lock;
   uint32_t      offset;
   uint32_t      n_nexus;
   rt_nexus_t    nexus;
   sig_shared_t  shared;
} rt_signal_t;

STATIC_ASSERT(sizeof(rt_signal_t) + 8 <= 192);

typedef struct _rt_implicit {
   rt_wakeable_t wakeable;
   ffi_closure_t closure;
   uint64_t      delay;
   rt_signal_t   signal;   // Has a flexible member
} rt_implicit_t;

typedef struct {
   rt_wakeable_t  wakeable;
   rt_proc_t     *proc;
   rt_nexus_t    *target;
   rt_nexus_t    *source;
   int64_t        after;
   int64_t        reject;
   unsigned       count;
} rt_transfer_t;

typedef struct _rt_alias {
   rt_alias_t  *chain;
   tree_t       where;
   rt_signal_t *signal;
} rt_alias_t;

typedef enum {
   SCOPE_F_RESOLVED = (1 << 0)
} rt_scope_flags_t;

typedef struct _rt_scope {
   signal_list_t    signals;
   proc_list_t      procs;
   alias_list_t     aliases;
   prop_list_t      properties;
   rt_scope_kind_t  kind;
   rt_scope_flags_t flags;
   unsigned         size;   // For signal scopes
   unsigned         offset;
   ident_t          name;
   tree_t           where;
   mptr_t           privdata;
   rt_scope_t      *parent;
   scope_list_t     children;
} rt_scope_t;

typedef struct _rt_watch {
   rt_wakeable_t   wakeable;
   sig_event_fn_t  fn;
   rt_watch_t     *chain_all;
   void           *user_data;
   unsigned        num_slots;
   unsigned        next_slot;
   rt_signal_t    *signals[];
} rt_watch_t;

#endif  // _RT_STRUCTS_H
