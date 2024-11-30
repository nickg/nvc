//
//  Copyright (C) 2023  Nick Gasson
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

#ifndef _PSL_FSM_H
#define _PSL_FSM_H

#include "prim.h"

typedef struct _fsm_edge fsm_edge_t;
typedef struct _fsm_state fsm_state_t;

typedef enum {
   EDGE_NEXT, EDGE_EPSILON
} edge_kind_t;

typedef enum {
   GUARD_EXPR = 0b00,
   GUARD_BINOP = 0b01,
   GUARD_NOT = 0b10,
   GUARD_FALSE = 0b11,
} guard_kind_t;

typedef void *psl_guard_t;

typedef enum {
   BINOP_AND,
   BINOP_OR,
} binop_kind_t;

typedef struct {
   binop_kind_t kind;
   psl_guard_t  left;
   psl_guard_t  right;
} guard_binop_t;

typedef struct _fsm_edge {
   fsm_edge_t  *next;
   fsm_state_t *dest;
   edge_kind_t  kind;
   psl_guard_t  guard;
} fsm_edge_t;

typedef struct _fsm_state {
   unsigned     id;
   fsm_state_t *next;
   fsm_edge_t  *edges;
   psl_node_t   where;
   psl_guard_t  guard;
   bool         initial;
   bool         accept;
   bool         strong;
} fsm_state_t;

typedef enum {
   FSM_BARE, FSM_ALWAYS, FSM_NEVER, FSM_COVER
} fsm_kind_t;

typedef struct {
   fsm_state_t  *states;
   fsm_state_t **tail;
   mem_pool_t   *pool;
   ident_t       label;
   psl_node_t    src;
   unsigned      next_id;
   fsm_kind_t    kind;
} psl_fsm_t;

psl_fsm_t *psl_fsm_new(psl_node_t p, ident_t label);
void psl_fsm_free(psl_fsm_t *fsm);
void psl_fsm_dump(psl_fsm_t *fsm, const char *tag);
bool psl_fsm_repeating(psl_fsm_t *fsm);

guard_kind_t psl_guard_kind(psl_guard_t g);
const guard_binop_t *psl_guard_binop(psl_guard_t g);
psl_node_t psl_guard_expr(psl_guard_t g);

#endif  // _PSL_FSM_H
