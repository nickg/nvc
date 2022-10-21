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

#ifndef _RT_STRUCTS_H
#define _RT_STRUCTS_H

#include "prim.h"
#include "jit/jit.h"
#include "jit/jit-ffi.h"
#include "rt/mspace.h"
#include "rt/rt.h"

typedef void *(*value_fn_t)(rt_nexus_t *);

typedef enum {
   W_PROC, W_WATCH, W_IMPLICIT
} wakeable_kind_t;

typedef uint32_t generation_t;

typedef struct {
   generation_t    wakeup_gen;
   wakeable_kind_t kind : 8;
   bool            pending;
   bool            postponed;
} rt_wakeable_t;

typedef struct _rt_proc {
   rt_wakeable_t  wakeable;
   tree_t         where;
   ident_t        name;
   jit_handle_t   handle;
   tlab_t         tlab;
   rt_scope_t    *scope;
   rt_proc_t     *chain;
   mptr_t         privdata;
} rt_proc_t;

typedef enum {
   EVENT_TIMEOUT,
   EVENT_DRIVER,
   EVENT_PROCESS,
   EVENT_DISCONNECT,
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
   rt_proc_t    *proc;
   generation_t  wakeup_gen;
} event_proc_t;

struct event {
   uint64_t            when;
   event_kind_t        kind;
   union {
      event_timeout_t  timeout;
      event_driver_t   driver;
      event_proc_t     proc;
      rt_nexus_t      *effective;
   };
};

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

STATIC_ASSERT(sizeof(rt_value_t) <= 24);

struct sens_list {
   rt_wakeable_t *wake;
   sens_list_t   *next;
   uint32_t       wakeup_gen;
   bool           recur;
};

typedef struct {
   sens_list_t *pending;
   uint64_t     last_event;
   uint64_t     last_active;
   int32_t      event_delta;
   int32_t      active_delta;
   uint32_t     net_id;
} rt_net_t;

STATIC_ASSERT(sizeof(rt_net_t) <= 64);

typedef enum {
   SOURCE_DRIVER,
   SOURCE_PORT,
} source_kind_t;

typedef struct {
   rt_proc_t  *proc;
   rt_nexus_t *nexus;
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

typedef struct _rt_source {
   rt_source_t    *chain_input;
   rt_source_t    *chain_output;
   source_kind_t   tag;
   unsigned        disconnected : 1;
   union {
      rt_port_t    port;
      rt_driver_t  driver;
   } u;
} rt_source_t;

STATIC_ASSERT(sizeof(rt_source_t) <= 64);

// The code generator knows the layout of this struct
typedef struct _rt_resolution {
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

typedef struct _rt_nexus {
   rt_nexus_t   *chain;
   void         *free_value;
   rt_net_t     *net;
   rt_value_t    forcing;
   uint32_t      width;
   net_flags_t   flags : 8;
   unsigned      size : 8;
   unsigned      n_sources : 8;
   rt_source_t   sources;
   rt_signal_t  *signal;
   rt_source_t  *outputs;
   void         *resolved;
} rt_nexus_t;

STATIC_ASSERT(sizeof(rt_nexus_t) <= 128);

// The code generator knows the layout of this struct
typedef struct _sig_shared {
   uint32_t size;
   uint32_t offset;
   uint8_t  data[0];
} sig_shared_t;

typedef struct {
   int         how;
   rt_nexus_t *nexus[0];
} rt_index_t;

typedef struct _rt_signal {
   tree_t          where;
   rt_signal_t    *chain;
   rt_scope_t     *parent;
   rt_index_t     *index;
   res_memo_t     *resolution;
   net_flags_t     flags;
   uint32_t        n_nexus;
   rt_nexus_t      nexus;
   sig_shared_t    shared;
} rt_signal_t;

STATIC_ASSERT(sizeof(rt_signal_t) + 8 <= 192);

typedef enum {
   SCOPE_ROOT,
   SCOPE_INSTANCE,
   SCOPE_PACKAGE,
   SCOPE_SIGNAL,
} rt_scope_kind_t;

typedef struct _rt_implicit {
   rt_wakeable_t wakeable;
   ffi_closure_t closure;
   rt_signal_t   signal;   // Has a flexible member
} rt_implicit_t;

typedef struct _rt_alias {
   rt_alias_t  *chain;
   tree_t       where;
   rt_signal_t *signal;
} rt_alias_t;

typedef enum {
   SCOPE_F_RESOLVED = (1 << 0)
} rt_scope_flags_t;

typedef struct _rt_scope {
   rt_signal_t     *signals;
   rt_proc_t       *procs;
   rt_alias_t      *aliases;
   rt_scope_kind_t  kind;
   rt_scope_flags_t flags;
   unsigned         size;   // For signal scopes
   unsigned         offset;
   ident_t          name;
   tree_t           where;
   mptr_t           privdata;
   rt_scope_t      *parent;
   rt_scope_t      *child;
   rt_scope_t      *chain;
} rt_scope_t;

typedef struct _rt_watch {
   rt_wakeable_t   wakeable;
   rt_signal_t    *signal;
   sig_event_fn_t  fn;
   rt_watch_t     *chain_all;
   void           *user_data;
} rt_watch_t;


#endif  // _RT_STRUCTS_H
