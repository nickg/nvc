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

#ifndef _TYPE_H
#define _TYPE_H

#include <stdbool.h>

#include "ident.h"
#include "prim.h"

typedef enum {
   T_SUBTYPE,
   T_INTEGER,
   T_REAL,
   T_ENUM,
   T_PHYSICAL,
   T_ARRAY,
   T_RECORD,
   T_FILE,
   T_ACCESS,
   T_FUNC,
   T_INCOMPLETE,
   T_PROC,
   T_NONE,
   T_PROTECTED,
   T_GENERIC,
   T_VIEW,

   T_LAST_TYPE_KIND
} type_kind_t;

typedef enum {
   GTYPE_PRIVATE,
   GTYPE_SCALAR,
   GTYPE_DISCRETE,
   GTYPE_INTEGER,
   GTYPE_PHYSICAL,
   GTYPE_FLOATING,
   GTYPE_ARRAY,
   GTYPE_ACCESS,
   GTYPE_FILE,
} gtype_class_t;

type_t type_new(type_kind_t kind);

type_kind_t type_kind(type_t t);
const char *type_kind_str(type_kind_t t);

bool type_eq(type_t a, type_t b);
bool type_strict_eq(type_t a, type_t b);
bool type_eq_map(type_t a, type_t b, hash_t *map);

// See `has_map' in type.c for definition of which fields each type
// kind contains

ident_t type_ident(type_t t);
void type_set_ident(type_t t, ident_t id);
bool type_has_ident(type_t t);

type_t type_base(type_t t);
void type_set_base(type_t t, type_t b);

type_t type_elem(type_t t);
void type_set_elem(type_t t, type_t e);
bool type_has_elem(type_t t);

unsigned type_subkind(type_t t);
void type_set_subkind(type_t t, unsigned subkind);

unsigned type_dims(type_t t);
tree_t type_dim(type_t t, unsigned n);
void type_add_dim(type_t t, tree_t r);

unsigned type_enum_literals(type_t t);
tree_t type_enum_literal(type_t t, unsigned n);
void type_enum_add_literal(type_t t, tree_t lit);

unsigned type_units(type_t t);
tree_t type_unit(type_t t, unsigned n);
void type_add_unit(type_t t, tree_t u);

unsigned type_params(type_t t);
type_t type_param(type_t t, unsigned n);
void type_add_param(type_t t, type_t p);

type_t type_result(type_t t);
void type_set_result(type_t t, type_t r);

type_t type_designated(type_t t);
void type_set_designated(type_t t, type_t a);

unsigned type_indexes(type_t t);
void type_add_index(type_t t, type_t sub);
type_t type_index(type_t t, unsigned n);

unsigned type_constraints(type_t t);
void type_add_constraint(type_t t, tree_t c);
tree_t type_constraint(type_t t, unsigned n);

unsigned type_fields(type_t t);
tree_t type_field(type_t t, unsigned n);
void type_add_field(type_t t, tree_t e);

void type_set_resolution(type_t t, tree_t r);
bool type_has_resolution(type_t t);
tree_t type_resolution(type_t t);

// Pretty printing
const char *type_pp(type_t t);
const char *type_pp2(type_t t, type_t other);
void type_signature(type_t t, text_buf_t *tb);

// Type predicates that recurse to base of subtypes
bool type_is_array(type_t t);
bool type_is_record(type_t t);
bool type_is_unconstrained(type_t t);
bool type_is_enum(type_t t);
bool type_is_integer(type_t t);
bool type_is_real(type_t t);
bool type_is_generic(type_t t);
bool type_is_scalar(type_t t);
bool type_is_representable(type_t t);
bool type_is_file(type_t t);
bool type_is_protected(type_t t);
bool type_is_access(type_t t);
bool type_is_incomplete(type_t t);
bool type_is_none(type_t t);
bool type_is_valid(type_t t);
bool type_is_physical(type_t t);
bool type_is_discrete(type_t t);
bool type_is_subprogram(type_t t);
bool type_is_universal(type_t t);
bool type_is_composite(type_t t);
bool type_is_homogeneous(type_t t);
bool type_is_resolved(type_t t);
bool type_is_character_array(type_t t);
bool type_is_numeric(type_t t);
bool type_has_error(type_t t);

bool type_const_bounds(type_t t);

bool type_matches_class(type_t t, gtype_class_t class);

// Helper to find ultimate base type
type_t type_base_recur(type_t t);
type_kind_t type_base_kind(type_t t);

type_t type_elem_recur(type_t t);

// Helper function to find number of sub-elemets
unsigned type_width(type_t type);

// Helpers for element constraints
tree_t type_constraint_for_field(type_t t, tree_t f);

int type_bit_width(type_t type);
int type_byte_width(type_t type);

bool type_frozen(type_t t);
tree_t type_container(type_t t);

object_t *type_to_object(type_t t);
type_t type_from_object(object_t *obj);

#endif  // _TYPE_H
