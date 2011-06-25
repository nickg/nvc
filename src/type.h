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
   T_INTEGER,
   T_FLOAT,
   T_ENUM,
   T_PHYSICAL,
   T_ARRAY,
   T_RECORD,
   T_FILE,
   T_ACCESS
} type_kind_t;

typedef struct enum_lit {
   union {
      ident_t id;
      char    ch;
   } u;
   enum { ENUM_IDENT, ENUM_CHAR } kind;
} enum_lit_t;

typedef struct range {
   int low;
   int high;
} range_t;

/**
 * Allocate a new type node.
 */
type_t type_new(type_kind_t kind);

/**
 * Return the kind of the type node.
 */
type_kind_t type_kind(type_t t);

/**
 * Compare two types for equality.
 *
 * TODO: describe rules
 */
bool type_eq(type_t a, type_t b);

/**
 * Get the identifier of an unresovled type.
 *
 * \param t Must be T_UNRESOLVED.
 */
ident_t type_ident(type_t t);

/**
 * Set the identifier for an unresolved type.
 *
 * Should only be called by the parser.
 * \param t Must be T_UNRESOLVED.
 */
void type_set_ident(type_t t, ident_t id);

unsigned type_dims(type_t t);

range_t type_dim(type_t t, unsigned n);

void type_add_dim(type_t t, range_t r);

/**
 * Count the number of literals in an enumeration.
 *
 * \param t Must be T_ENUM.
 */
unsigned type_enum_literals(type_t t);

/**
 * Get the Nth literal in an enumeration.
 *
 * \param t Must be T_ENUM.
 */
enum_lit_t type_enum_literal(type_t t, unsigned n);

/**
 * Add a literal to an enumeration.
 *
 * \param t Must be T_ENUM.
 */
void type_enum_add_literal(type_t t, enum_lit_t lit);

#endif  // _TYPE_H
