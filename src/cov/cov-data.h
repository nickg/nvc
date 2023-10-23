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

typedef struct _cover_report_ctx    cover_report_ctx_t;
typedef struct _cover_file          cover_file_t;
typedef struct _cover_scope         cover_scope_t;
typedef struct _cover_exclude_ctx   cover_exclude_ctx_t;
typedef struct _cover_rpt_buf       cover_rpt_buf_t;
typedef struct _cover_spec          cover_spec_t;

typedef A(char*) char_array_t;

struct _cover_spec {
   char_array_t hier_include;
   char_array_t hier_exclude;
   char_array_t block_include;
   char_array_t block_exclude;
   char_array_t fsm_type_include;
   char_array_t fsm_type_exclude;
};

struct _cover_data {
   int               next_tag;
   cover_mask_t      mask;
   int               array_limit;
   int               array_depth;
   int               report_item_limit;
   cover_rpt_buf_t  *rpt_buf;
   cover_spec_t     *spec;
   cover_scope_t    *top_scope;
   cover_scope_t    *root_scope;
};

typedef enum {
   CSCOPE_UNKNOWN,
   CSCOPE_INSTANCE,
} scope_type_t;

typedef struct {
   int start;
   int end;
} line_range_t;

typedef A(line_range_t) range_array_t;
typedef A(cover_item_t) cov_item_array_t;

typedef struct _cover_scope {
   scope_type_t      type;
   ident_t           name;
   ident_t           hier;
   loc_t             loc;
   int               branch_label;
   int               stmt_label;
   int               expression_label;
   cover_scope_t    *parent;
   ptr_list_t        children;
   cov_item_array_t  items;
   range_array_t     ignore_lines;
   ident_t           block_name;
   int               sig_pos;
   bool              emit;
} cover_scope_t;

//
// Internal API
//

void cover_bmask_to_bin_list(uint32_t bmask, text_buf_t *tb);
uint32_t cover_bin_str_to_bmask(const char *bin);
const char *cover_item_kind_str(cover_item_kind_t kind);

#endif   // _COV_DATA_H
