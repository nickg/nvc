//
//  Copyright (C) 2011  Nick Gasson
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

#ifndef _TYPE_H
#define _TYPE_H

#include <stdbool.h>

#include "ident.h"

typedef struct type *type_t;

typedef struct type_wr_ctx *type_wr_ctx_t;
typedef struct type_rd_ctx *type_rd_ctx_t;

struct tree_wr_ctx;
struct tree_rd_ctx;

typedef enum type_kind {
   T_UNRESOLVED,
   T_SUBTYPE,
   T_INTEGER,
   T_REAL,
   T_ENUM,
   T_PHYSICAL,
   T_CARRAY,
   T_UARRAY,
   T_RECORD,
   T_FILE,
   T_ACCESS,
   T_FUNC
} type_kind_t;

typedef struct range {
   struct tree *left;
   struct tree *right;
   enum { RANGE_TO, RANGE_DOWNTO } kind;
} range_t;

typedef struct unit {
   struct tree *multiplier;
   ident_t     name;
} unit_t;

type_t type_new(type_kind_t kind);

type_kind_t type_kind(type_t t);
bool type_eq(type_t a, type_t b);

// T_UNRESOLVED, T_FUNC
ident_t type_ident(type_t t);
void type_set_ident(type_t t, ident_t id);

// T_SUBTYPE, T_UARRAY, T_CARRAY
type_t type_base(type_t t);
void type_set_base(type_t t, type_t b);

// T_INTEGER, T_SUBTYPE, T_PHYSICAL, T_CARRAY
unsigned type_dims(type_t t);
range_t type_dim(type_t t, unsigned n);
void type_add_dim(type_t t, range_t r);

// T_ENUM
unsigned type_enum_literals(type_t t);
struct tree *type_enum_literal(type_t t, unsigned n);
void type_enum_add_literal(type_t t, struct tree *lit);

// T_PHYSICAL
unsigned type_units(type_t t);
unit_t type_unit(type_t t, unsigned n);
void type_add_unit(type_t t, unit_t u);

// T_FUNC
unsigned type_params(type_t t);
type_t type_param(type_t t, unsigned n);
void type_add_param(type_t t, type_t p);

// T_FUNC
type_t type_result(type_t t);
void type_set_result(type_t t, type_t r);

// T_UARRAY
unsigned type_index_constrs(type_t t);
void type_add_index_constr(type_t t, type_t c);
type_t type_index_constr(type_t t, unsigned n);

type_wr_ctx_t type_write_begin(struct tree_wr_ctx *tree_ctx);
void type_write(type_t t, type_wr_ctx_t ctx);
void type_write_end(type_wr_ctx_t ctx);

type_rd_ctx_t type_read_begin(struct tree_rd_ctx *tree_ctx);
type_t type_read(type_rd_ctx_t ctx);
void type_read_end(type_rd_ctx_t ctx);

// Pretty printing
const char *type_pp(type_t t);

// Reference counting
void type_ref(type_t t);
void type_unref(type_t t);

// Predefined types
type_t type_universal_int(void);

#endif  // _TYPE_H
