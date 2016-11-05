//
//  Copyright (C) 2013-2016  Nick Gasson
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

#ifndef _PRIM_H
#define _PRIM_H

#include <stdint.h>
#include <stddef.h>

typedef struct loc {
   unsigned    first_line : 20;
   unsigned    first_column : 12;
   unsigned    last_line : 20;
   unsigned    last_column : 12;
   const char *file;
   const char *linebuf;
} loc_t;

#define LINE_INVALID   0xfffff
#define COLUMN_INVALID 0xfff

static const loc_t LOC_INVALID = {
   LINE_INVALID,
   COLUMN_INVALID,
   LINE_INVALID,
   COLUMN_INVALID,
   NULL,
   NULL
};

typedef struct tree *tree_t;
typedef struct type *type_t;

typedef enum {
   RANGE_TO,
   RANGE_DOWNTO,
   RANGE_EXPR,
   RANGE_DYN,
   RANGE_RDYN
} range_kind_t;

typedef struct range {
   tree_t       left;
   tree_t       right;
   range_kind_t kind;
} range_t;

typedef struct tree_wr_ctx *tree_wr_ctx_t;
typedef struct tree_rd_ctx *tree_rd_ctx_t;

typedef struct trie *ident_t;

typedef struct vcode_unit *vcode_unit_t;

typedef uint32_t netid_t;

#define NETID_INVALID UINT32_MAX

#endif  // _PRIM_H
