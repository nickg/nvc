//
//  Copyright (C) 2022-2023  Nick Gasson
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

#ifndef _PSL_NODE_H
#define _PSL_NODE_H

#include "prim.h"

typedef enum {
   P_ASSERT,
   P_ALWAYS,
   P_HDL_EXPR,
   P_CLOCK_DECL,
   P_NEXT,
   P_NEVER,
   P_EVENTUALLY,
   P_NEXT_A,
   P_NEXT_E,
   P_NEXT_EVENT,
   P_SERE,
   P_IMPLICATION,

   P_LAST_PSL_KIND
} psl_kind_t;

typedef enum {
   PSL_IMPL_IF,
   PSL_IMPL_IFF,
} psl_impl_kind_t;

typedef enum {
   PSL_TYPE_BIT,
   PSL_TYPE_BOOLEAN,
   PSL_TYPE_BITVECTOR,
   PSL_TYPE_NUMERIC,
   PSL_TYPE_STRING,
} psl_type_t;

typedef enum {
   PSL_WEAK,
   PSL_STRONG,
} psl_strength_t;

psl_node_t psl_new(psl_kind_t kind);
psl_kind_t psl_kind(psl_node_t p);
const char *psl_kind_str(psl_kind_t kind);

const loc_t *psl_loc(psl_node_t p);
void psl_set_loc(psl_node_t p, const loc_t *loc);

unsigned psl_subkind(psl_node_t p);
void psl_set_subkind(psl_node_t p, unsigned sub);

psl_type_t psl_type(psl_node_t p);
void psl_set_type(psl_node_t p, psl_type_t type);

tree_t psl_tree(psl_node_t p);
void psl_set_tree(psl_node_t p, tree_t t);

psl_node_t psl_value(psl_node_t p);
void psl_set_value(psl_node_t p, psl_node_t v);

unsigned psl_operands(psl_node_t p);
psl_node_t psl_operand(psl_node_t p, unsigned n);
void psl_add_operand(psl_node_t p, psl_node_t o);

psl_node_t psl_clock(psl_node_t p);
bool psl_has_clock(psl_node_t p);
void psl_set_clock(psl_node_t p, psl_node_t clk);

object_t *psl_to_object(psl_node_t p);
psl_node_t psl_from_object(object_t *obj);

#endif  // _PSL_NODE_H
