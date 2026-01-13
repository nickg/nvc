//
//  Copyright (C) 2024-2025  Nick Gasson
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

#ifndef _MIR_STRUCTS_H
#define _MIR_STRUCTS_H

#include "prim.h"
#include "diag.h"
#include "mir/mir-unit.h"
#include "mir/mir-node.h"
#include "thread.h"

#define MIR_INLINE_ARGS 4

typedef struct {
   mir_op_t    op;
   mir_type_t  type;
   mir_stamp_t stamp;
   unsigned    nargs;
   loc_t       loc;
   union {
      mir_value_t args[MIR_INLINE_ARGS];
      size_t      spilloff;
      object_t   *locus;
      int64_t     iconst;
      double      dconst;
      uint64_t    bits[2];
   };
} node_data_t;

STATIC_ASSERT(sizeof(node_data_t) == 40);

typedef uint32_t node_id_t;

typedef struct {
   unsigned   num_nodes;
   unsigned   max_nodes;
   node_id_t *nodes;
   loc_t      last_loc;
   int        gap_pos;
} block_data_t;

typedef struct {
   ident_t     name;
   mir_type_t  type;
   mir_stamp_t stamp;
} param_data_t;

typedef struct {
   ident_t         name;
   mir_type_t      type;
   mir_stamp_t     stamp;
   mir_type_t      pointer;
   mir_var_flags_t flags;
} var_data_t;

typedef struct {
   int64_t low, high;
} mir_intg_type_t;

typedef struct {
   double low, high;
} mir_real_type_t;

typedef struct {
   unsigned   size;
   mir_type_t elem;
   mir_type_t pointer;
} mir_carray_type_t;

typedef struct {
   unsigned   dims;
   mir_type_t elem;
   mir_type_t pointer;
} mir_uarray_type_t;

typedef struct {
   mir_type_t base;
   mir_type_t pointer;
} mir_signal_type_t;

typedef struct {
   mir_type_t to;
   mir_type_t pointer;
} mir_access_type_t;

typedef struct {
   ident_t     name;
   mir_type_t *fields;
   unsigned    count;
} mir_record_type_t;

typedef struct {
   mir_type_t rtype;
} mir_closure_type_t;

typedef struct {
   unsigned size;
   bool     issigned;
} mir_vec_type_t;

typedef struct {
   mir_class_t class : 16;
   mir_repr_t  repr : 16;
   uint32_t    hash;
   union {
      mir_intg_type_t    intg;
      mir_real_type_t    real;
      mir_type_t         pointer;
      mir_carray_type_t  carray;
      mir_uarray_type_t  uarray;
      mir_signal_type_t  signal;
      mir_access_type_t  access;
      mir_record_type_t  record;
      mir_closure_type_t closure;
      mir_type_t         base;
      mir_vec_type_t     vec;
      ident_t            context;
   } u;
} type_data_t;

STATIC_ASSERT(sizeof(type_data_t) <= 32);

typedef enum {
   _MIR_INVALID_STAMP,
   MIR_STAMP_INT,
   MIR_STAMP_REAL,
   MIR_STAMP_POINTER,
} mir_stamp_kind_t;

typedef struct {
   mir_stamp_t elem;
   mir_mem_t   memory;
} mir_ptr_stamp_t;

typedef struct {
   mir_stamp_kind_t kind;
   uint32_t         hash;
   union {
      mir_intg_type_t intg;
      mir_real_type_t real;
      mir_ptr_stamp_t pointer;
   } u;
} stamp_data_t;

typedef struct {
   mir_block_t block;
   unsigned    pos;
   loc_t       loc;
} mir_cursor_t;

typedef struct {
   mir_type_t offset_type;
   mir_type_t bool_type;
   mir_type_t time_type;
   mir_type_t logic_type;
   mir_type_t double_type;
   mir_type_t char_type;
   mir_type_t locus_type;
   mir_type_t conversion_type;
   mir_type_t trigger_type;
   mir_type_t self_type;
   mir_type_t string_type;
} known_types_t;

typedef struct {
   unsigned      count;
   unsigned      limit;
   block_data_t *items;
} block_tab_t;

typedef struct {
   unsigned      count;
   unsigned      limit;
   param_data_t *items;
} param_tab_t;

typedef struct {
   unsigned    count;
   unsigned    limit;
   var_data_t *items;
} var_tab_t;

typedef struct {
   unsigned      count;
   unsigned      limit;
   stamp_data_t *items;
} stamp_tab_t;

typedef struct {
   unsigned  count;
   unsigned  limit;
   ident_t  *items;
} link_tab_t;

typedef struct _mir_unit {
   mir_context_t   *context;
   ident_t          name;
   object_t        *object;
   mir_unit_kind_t  kind;
   mir_shape_t     *parent;
   mir_shape_t     *shape;
   mem_pool_t      *pool;
   hash_t          *objmap;
   hash_t          *privmap;
   mir_type_t       result;
   known_types_t    types;
   mir_cursor_t     cursor;
   block_tab_t      blocks;
   param_tab_t      params;
   var_tab_t        vars;
   stamp_tab_t      stamps;
   link_tab_t       linkage;
   link_tab_t       extvars;
   unsigned         num_nodes;
   unsigned         max_nodes;
   node_data_t     *nodes;
   mir_value_t     *argspill;
   unsigned         num_argspill;
   unsigned         max_argspill;
   unsigned         num_vregs;
   mir_vreg_t      *vregs;
#ifdef DEBUG
   text_buf_t      *comments;
#endif
} mir_unit_t;

typedef enum {
   FREE_MARKER,
   BUSY_MARKER,
   INUSE_MARKER,
   MOVED_MARKER,
} mir_marker_t;

typedef union {
   struct {
      uint32_t marker : 2;
      uint32_t id : 30;
   };
   uint32_t bits;
} type_slot_t;

STATIC_ASSERT(sizeof(type_slot_t) == 4);

typedef struct {
   uint32_t     max_types;
   uint32_t     next_id;
   type_slot_t *hashtab;
   type_data_t  types[0];
} type_tab_t;

typedef struct _mir_context {
   type_tab_t *typetab;
   type_tab_t *resizing;
   chash_t    *map;
   mem_pool_t *pool;
   nvc_lock_t  pool_mtx;
} mir_context_t;

typedef struct {
   ident_t    name;
   mir_type_t type;
   mir_type_t pointer;
} shape_slot_t;

typedef struct _mir_shape {
   ident_t         name;
   mir_shape_t    *parent;
   hash_t         *objmap;
   mir_type_t      type;
   mir_unit_kind_t kind;
   unsigned        num_slots;
   shape_slot_t    slots[];
} mir_shape_t;

typedef struct {
   ident_t          name;
   ident_t          parent;
   mir_lower_fn_t   fn;
   object_t        *object;
   mir_unit_kind_t  kind;
} deferred_unit_t;

#endif   // _MIR_STRUCTS_H
