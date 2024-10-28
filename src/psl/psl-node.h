//
//  Copyright (C) 2022-2024  Nick Gasson
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
   P_ASSUME,
   P_RESTRICT,
   P_FAIRNESS,
   P_COVER,
   P_ALWAYS,
   P_HDL_EXPR,
   P_PROPERTY_DECL,
   P_SEQUENCE_DECL,
   P_CLOCK_DECL,
   P_NEXT,
   P_NEVER,
   P_EVENTUALLY,
   P_NEXT_A,
   P_NEXT_E,
   P_NEXT_EVENT,
   P_SERE,
   P_IMPLICATION,
   P_REPEAT,
   P_PROPERTY_INST,
   P_SEQUENCE_INST,
   P_UNION,
   P_BUILTIN_FUNC,
   P_VALUE_SET,
   P_PARAM,
   P_UNTIL,
   P_ABORT,

   P_LAST_PSL_KIND
} psl_kind_t;

typedef enum {
   PSL_IMPL_IF,
   PSL_IMPL_IFF,
} psl_impl_kind_t;

typedef enum {
   PSL_TYPE_HDLTYPE,
   PSL_TYPE_BOOLEAN,
   PSL_TYPE_BIT,
   PSL_TYPE_BITVECTOR,
   PSL_TYPE_NUMERIC,
   PSL_TYPE_STRING,
   PSL_TYPE_SEQUENCE,
   PSL_TYPE_PROPERTY
} psl_type_t;

typedef enum {
   PSL_GUARANTEE,
   PSL_NO_GUARANTEE,
} psl_guarantee_t;

typedef enum {
   PSL_PLUS_REPEAT,
   PSL_TIMES_REPEAT,
   PSL_GOTO_REPEAT,
   PSL_ARROW_REPEAT
} psl_repeat_t;

typedef enum {
   PSL_SERE_CONCAT,
   PSL_SERE_FUSION,
   PSL_SERE_OR,
   PSL_SERE_EQU_AND,
   PSL_SERE_NEQ_AND,
   PSL_SERE_WITHIN,
   PSL_SERE_PARAM_NEQ_AND,
   PSL_SERE_PARAM_EQU_AND,
   PSL_SERE_PARAM_OR
} psl_sere_kind_t;

typedef enum {
   PSL_BUILTIN_NEXT,
   PSL_BUILTIN_PREV,
   PSL_BUILTIN_STABLE,
   PSL_BUILTIN_ROSE,
   PSL_BUILTIN_FELL,
   PSL_BUILTIN_ENDED,
   PSL_BUILTIN_NONDET,
   PSL_BUILTIN_NONDET_VECTOR
} psl_builtin_kind_t;

typedef enum {
   PSL_VALUE_SET_BOOLEAN,
   PSL_VALUE_SET_EXPLICIT
} psl_value_set_kind_t;

typedef enum {
   PSL_ABORT_ASYNC,
   PSL_ABORT_SYNC,
} psl_abort_t;

typedef enum {
   PSL_F_STRONG    = (1 << 0),
   PSL_F_INCLUSIVE = (1 << 1),
} psl_flags_t;

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
bool psl_has_tree(psl_node_t p);
void psl_set_tree(psl_node_t p, tree_t t);

psl_node_t psl_value(psl_node_t p);
void psl_set_value(psl_node_t p, psl_node_t v);

tree_t psl_delay(psl_node_t p);
void psl_set_delay(psl_node_t p, tree_t d);
bool psl_has_delay(psl_node_t p);

unsigned psl_operands(psl_node_t p);
psl_node_t psl_operand(psl_node_t p, unsigned n);
void psl_add_operand(psl_node_t p, psl_node_t o);

psl_node_t psl_clock(psl_node_t p);
bool psl_has_clock(psl_node_t p);
void psl_set_clock(psl_node_t p, psl_node_t clk);

tree_t psl_message(psl_node_t p);
bool psl_has_message(psl_node_t p);
void psl_set_message(psl_node_t, tree_t m);

unsigned psl_ports(psl_node_t p);
tree_t psl_port(psl_node_t p, unsigned n);
void psl_add_port(psl_node_t p, tree_t o);

ident_t psl_ident(psl_node_t p);
bool psl_has_ident(psl_node_t p);
void psl_set_ident(psl_node_t p, ident_t i);

unsigned psl_decls(psl_node_t p);
void psl_add_decl(psl_node_t p, tree_t r);
tree_t psl_decl(psl_node_t p, unsigned n);

void psl_set_ref(psl_node_t p, psl_node_t r);
psl_node_t psl_ref(psl_node_t p);

void psl_set_repeat(psl_node_t p, psl_node_t r);
psl_node_t psl_repeat(psl_node_t p);
bool psl_has_repeat(psl_node_t p);

psl_flags_t psl_flags(psl_node_t p);
void psl_set_flag(psl_node_t p, psl_flags_t mask);

void psl_locus(psl_node_t p, ident_t *unit, ptrdiff_t *offset);

object_t *psl_to_object(psl_node_t p);
psl_node_t psl_from_object(object_t *obj);

#endif  // _PSL_NODE_H
