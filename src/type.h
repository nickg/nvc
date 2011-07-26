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

typedef enum type_kind {
   T_UNRESOLVED,
   T_SUBTYPE,
   T_INTEGER,
   T_FLOAT,
   T_ENUM,
   T_PHYSICAL,
   T_ARRAY,
   T_RECORD,
   T_FILE,
   T_ACCESS
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

// T_UNRESOLVED
ident_t type_ident(type_t t);
void type_set_ident(type_t t, ident_t id);

// T_SUBTYPE
type_t type_base(type_t t);
void type_set_base(type_t t, type_t b);

// T_INTEGER, T_SUBTYPE, T_PHYSICAL
unsigned type_dims(type_t t);
range_t type_dim(type_t t, unsigned n);
void type_add_dim(type_t t, range_t r);

// T_ENUM
unsigned type_enum_literals(type_t t);
ident_t type_enum_literal(type_t t, unsigned n);
void type_enum_add_literal(type_t t, ident_t lit);

// T_PHYSICAL
unsigned type_units(type_t t);
unit_t type_unit(type_t t, unsigned n);
void type_add_unit(type_t t, unit_t u);

// Predefined types
type_t type_universal_int(void);

#endif  // _TYPE_H
