//
//  Copyright (C) 2013-2023  Nick Gasson
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

#ifndef _COV_DATA_H
#define _COV_DATA_H

#include "prim.h"
#include "array.h"
#include "cov/cov-api.h"
#include "diag.h"

typedef struct _cover_exclude_ctx   cover_exclude_ctx_t;
typedef struct _cover_rpt_buf       cover_rpt_buf_t;
typedef struct _cover_spec          cover_spec_t;

typedef A(cover_obj_t) cover_array_t;

typedef struct _cover_excl_cmd {
   ident_t              hier;
   loc_t                loc;
   bool                 found;
} cover_excl_cmd_t;

typedef struct _cover_fold_cmd {
   ident_t              target;
   ident_t              source;
   loc_t                loc;
   bool                 found_target;
   bool                 found_source;
} cover_fold_cmd_t;

typedef struct _cover_ef {
   cover_excl_cmd_t     *excl;
   cover_fold_cmd_t     *fold;
   int                   n_excl_cmds;
   int                   n_fold_cmds;
   int                   alloc_excl_cmds;
   int                   alloc_fold_cmds;
} cover_ef_t;

typedef enum {
   CSCOPE_NONE,
   CSCOPE_INSTANCE,
   CSCOPE_SUBPROG,
   CSCOPE_PACKAGE,
   CSCOPE_PROCESS,
   CSCOPE_USER,
   CSCOPE_PROPERTY,
} cscope_kind_t;

typedef struct _cover_scope {
   ident_t        name;
   ident_t        hier;
   ident_t        block_name;
   loc_t          loc;
   cscope_kind_t  kind;
   int            branch_label;
   int            stmt_label;
   int            expression_label;
   cover_obj_t    parent;
   cover_block_t *block;
   cover_array_t  children;
   cover_array_t  items;
   int            sig_pos;
   bool           emit;
} cover_scope_t;

typedef struct {
   ident_t     hier;
   cover_obj_t first_range;
   int32_t     tag;
   int32_t     data;
   uint32_t    flags;
   uint32_t    n_ranges;
   uint32_t    field_idx;
} cover_bin_t;

typedef struct {
   int64_t min;
   int64_t max;
} cover_range_t;

typedef struct {
   // Type of coverage
   cover_item_kind_t kind;

   // Location of the item in the source file
   loc_t             loc;

   // Locations of LHS/RHS operands
   loc_t             loc_rhs;
   loc_t             loc_lhs;

   // Additional name for cover item:
   //    COV_ITEM_EXPRESSION     Name of expression (e.g. OR, AND, XOR)
   //    COV_ITEM_STATE          Name of FSM state
   //    COV_ITEM_FUNCTIONAL     Name of user-defined functional over point
   ident_t           func_name;

   // Type of source statement or expression
   cover_src_t       source;

   // Threshold for being covered
   int               atleast;

   // Secondary numeric data:
   //    COV_ITEM_TOGGLE - Start position of signal name
   //    COV_ITEM_STATE  - Value of low-index of enum sub-type
   int64_t           metadata;

   uint32_t    nbins;
   cover_obj_t first_bin;
} cover_item_t;

typedef struct {
   unsigned      count;
   unsigned      limit;
   cover_item_t *items;
} item_tab_t;

typedef struct {
   unsigned     count;
   unsigned     limit;
   cover_bin_t *items;
} bin_tab_t;

typedef struct {
   unsigned       count;
   unsigned       limit;
   cover_range_t *items;
} range_tab_t;

typedef struct {
   unsigned       count;
   unsigned       limit;
   cover_scope_t *items;
} scope_tab_t;

struct _cover_data {
   cover_mask_t     mask;
   int              array_limit;
   int              threshold;
   cover_rpt_buf_t *rpt_buf;
   cover_spec_t    *spec;
   cover_ef_t      *ef;
   cover_obj_t      root_scope;
   hash_t          *blocks;
   mem_pool_t      *pool;
   item_tab_t       items;
   bin_tab_t        bins;
   range_tab_t      ranges;
   scope_tab_t      scopes;
};

typedef struct {
   file_ref_t file_ref;
   int        start;
   int        end;
} ignore_range_t;

typedef A(ignore_range_t) ignore_array_t;

//
// Internal API
//

const char *cover_item_kind_str(cover_item_kind_t kind);
void cover_merge_bin(cover_data_t *db, cover_obj_t obj, int32_t data);

#endif   // _COV_DATA_H
