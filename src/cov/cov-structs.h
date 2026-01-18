//
//  Copyright (C) 2025  Nick Gasson
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

#ifndef _COV_STRUCTS_H
#define _COV_STRUCTS_H

#include "prim.h"
#include "array.h"
#include "cov/cov-api.h"
#include "cov/cov-data.h"

#define SHA_HEX_LEN (20 * 2 + 1)

typedef struct _cover_block {
   ident_t        name;
   unsigned       next_tag;
   cover_scope_t *self;
   int32_t       *data;
} cover_block_t;

typedef struct {
   char   *text;
   size_t  len;
} rpt_line_t;

typedef struct {
   unsigned total[NUM_COVER_KINDS];
   unsigned hit[NUM_COVER_KINDS];
} rpt_stats_t;

typedef struct {
   const rpt_line_t   *line;
   unsigned            count;
   const cover_item_t *items[];
} rpt_table_t;

typedef A(rpt_table_t *) table_array_t;

typedef struct {
   table_array_t hits[NUM_COVER_KINDS];
   table_array_t miss[NUM_COVER_KINDS];
   table_array_t excl[NUM_COVER_KINDS];
   unsigned      total;
} rpt_detail_t;

typedef struct {
   const char        *path;
   char               path_hash[SHA_HEX_LEN];
   rpt_stats_t        stats;
   cov_item_array_t   items;
   rpt_detail_t       detail;
   rpt_line_t        *lines;
   unsigned           n_lines;
   bool               valid;
} rpt_file_t;

typedef struct {
   char              name_hash[SHA_HEX_LEN];
   rpt_stats_t       flat_stats;
   rpt_stats_t       nested_stats;
   rpt_detail_t      detail;
} rpt_hier_t;

#endif   // _COV_STRUCTS_H
