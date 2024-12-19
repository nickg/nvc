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

#ifndef _VLOG_NODE_H
#define _VLOG_NODE_H

#include "prim.h"

typedef enum {
   V_EVENT_LEVEL,
   V_EVENT_POSEDGE,
   V_EVENT_NEGEDGE,
} v_event_kind_t;

typedef enum {
   V_PORT_INPUT,
   V_PORT_OUTPUT,
   V_PORT_INOUT,
} v_port_kind_t;

typedef enum {
   V_NET_WIRE,
   V_NET_SUPPLY0,
   V_NET_SUPPLY1,
} vlog_net_kind_t;

typedef enum {
   V_UDP_COMB,
   V_UDP_SEQ,
} vlog_udp_kind_t;

typedef enum {
   V_MODULE,
   V_PORT_DECL,
   V_REF,
   V_ALWAYS,
   V_TIMING,
   V_NBASSIGN,
   V_EVENT,
   V_INITIAL,
   V_SEQ_BLOCK,
   V_SYS_TCALL,
   V_STRING,
   V_NUMBER,
   V_NET_DECL,
   V_ASSIGN,
   V_DIMENSION,
   V_IF,
   V_COND,
   V_VAR_DECL,
   V_DELAY_CONTROL,
   V_BINARY,
   V_BASSIGN,
   V_UNARY,
   V_GATE_INST,
   V_STRENGTH,
   V_MOD_INST,
   V_BIT_SELECT,
   V_SYS_FCALL,
   V_FOREVER,
   V_SPECIFY,
   V_PRIMITIVE,
   V_UDP_TABLE,
   V_UDP_ENTRY,
   V_DATA_TYPE,
   V_TYPE_DECL,
   V_ENUM_DECL,
   V_ENUM_NAME,
   V_UNION_DECL,
   V_STRUCT_DECL,
   V_EVENT_CONTROL,
   V_EMPTY,
   V_REPEAT,
   V_WHILE,
   V_DO_WHILE,
   V_TASK_DECL,
   V_FUNC_DECL,
   V_WAIT,
   V_PARAM_DECL,
   V_COND_EXPR,
   V_REAL,

   V_LAST_NODE_KIND
} vlog_kind_t;

typedef enum {
   DT_LOGIC,
   DT_INTEGER,
   DT_REAL,
   DT_STRUCT,
   DT_BYTE,
   DT_SHORTINT,
   DT_INT,
   DT_LONGINT,
   DT_TIME,
   DT_BIT,
   DT_SHORTREAL,
   DT_REALTIME,
} data_type_t;

typedef enum {
   V_DIM_PACKED,
   V_DIM_UNPACKED
} vlog_dimension_kind_t;

typedef enum {
   V_BINARY_OR,
   V_BINARY_AND,
   V_BINARY_PLUS,
   V_BINARY_MINUS,
   V_BINARY_TIMES,
   V_BINARY_DIVIDE,
   V_BINARY_MOD,
   V_BINARY_LOG_EQ,
   V_BINARY_LOG_NEQ,
   V_BINARY_CASE_EQ,
   V_BINARY_CASE_NEQ,
   V_BINARY_LOG_OR,
   V_BINARY_LOG_AND,
   V_BINARY_SHIFT_LL,
   V_BINARY_SHIFT_RL,
   V_BINARY_SHIFT_LA,
   V_BINARY_SHIFT_RA,
   V_BINARY_LT,
   V_BINARY_GT,
   V_BINARY_LEQ,
   V_BINARY_GEQ,
} vlog_binary_t;

typedef enum {
   V_UNARY_BITNEG,
   V_UNARY_NOT,
   V_UNARY_NEG,
   V_UNARY_IDENTITY,
} vlog_unary_t;

typedef enum {
   V_ASSIGN_EQUALS,
} vlog_assign_t;

typedef enum {
   V_GATE_PULLDOWN,
   V_GATE_PULLUP,
   V_GATE_AND,
   V_GATE_NAND,
   V_GATE_OR,
   V_GATE_NOR,
   V_GATE_XOR,
   V_GATE_XNOR,
   V_GATE_NOT,
   V_GATE_BUF,
} vlog_gate_kind_t;

typedef enum {
   V_STRENGTH_HIGHZ,
   V_STRENGTH_SMALL,
   V_STRENGTH_MEDIUM,
   V_STRENGTH_WEAK,
   V_STRENGTH_LARGE,
   V_STRENGTH_PULL,
   V_STRENGTH_STRONG,
   V_STRENGTH_SUPPLY,
} vlog_strength_t;

#define STRENGTH1(st) (((st) >> 5) & 7)
#define STRENGTH0(st) (((st) >> 2) & 7)

#define MAKE_STRENGTH(s0, s1) ((s1) << 5 | (s0) << 2)

#define ST_SUPPLY MAKE_STRENGTH(V_STRENGTH_SUPPLY, V_STRENGTH_SUPPLY)
#define ST_STRONG MAKE_STRENGTH(V_STRENGTH_STRONG, V_STRENGTH_STRONG)
#define ST_PULLUP MAKE_STRENGTH(V_STRENGTH_PULL, V_STRENGTH_PULL)

vlog_node_t vlog_new(vlog_kind_t kind);
vlog_kind_t vlog_kind(vlog_node_t v);
const char *vlog_kind_str(vlog_kind_t kind);

void make_new_vlog_arena(void);

const loc_t *vlog_loc(vlog_node_t v);
void vlog_set_loc(vlog_node_t v, const loc_t *loc);

ident_t vlog_ident(vlog_node_t v);
void vlog_set_ident(vlog_node_t v, ident_t i);
bool vlog_has_ident(vlog_node_t v);

ident_t vlog_ident2(vlog_node_t v);
void vlog_set_ident2(vlog_node_t v, ident_t i);

vlog_node_t vlog_ref(vlog_node_t v);
void vlog_set_ref(vlog_node_t v, vlog_node_t d);
bool vlog_has_ref(vlog_node_t v);

unsigned vlog_stmts(vlog_node_t v);
vlog_node_t vlog_stmt(vlog_node_t v, unsigned n);
void vlog_add_stmt(vlog_node_t v, vlog_node_t s);

unsigned vlog_ports(vlog_node_t v);
vlog_node_t vlog_port(vlog_node_t v, unsigned n);
void vlog_add_port(vlog_node_t v, vlog_node_t p);

unsigned vlog_params(vlog_node_t v);
vlog_node_t vlog_param(vlog_node_t v, unsigned n);
void vlog_add_param(vlog_node_t v, vlog_node_t p);

unsigned vlog_ranges(vlog_node_t v);
vlog_node_t vlog_range(vlog_node_t v, unsigned n);
void vlog_add_range(vlog_node_t v, vlog_node_t r);

unsigned vlog_decls(vlog_node_t v);
vlog_node_t vlog_decl(vlog_node_t v, unsigned n);
void vlog_add_decl(vlog_node_t v, vlog_node_t d);

unsigned vlog_conds(vlog_node_t v);
vlog_node_t vlog_cond(vlog_node_t v, unsigned n);
void vlog_add_cond(vlog_node_t v, vlog_node_t c);

unsigned vlog_subkind(vlog_node_t v);
void vlog_set_subkind(vlog_node_t v, unsigned sub);

vlog_node_t vlog_value(vlog_node_t v);
bool vlog_has_value(vlog_node_t v);
void vlog_set_value(vlog_node_t v, vlog_node_t e);

vlog_node_t vlog_target(vlog_node_t v);
void vlog_set_target(vlog_node_t v, vlog_node_t e);

vlog_node_t vlog_delay(vlog_node_t v);
bool vlog_has_delay(vlog_node_t v);
void vlog_set_delay(vlog_node_t v, vlog_node_t d);

vlog_node_t vlog_type(vlog_node_t v);
bool vlog_has_type(vlog_node_t v);
void vlog_set_type(vlog_node_t v, vlog_node_t t);

vlog_node_t vlog_left(vlog_node_t v);
void vlog_set_left(vlog_node_t v, vlog_node_t e);

vlog_node_t vlog_right(vlog_node_t v);
void vlog_set_right(vlog_node_t v, vlog_node_t e);

const char *vlog_text(vlog_node_t v);
void vlog_set_text(vlog_node_t v, const char *text);

number_t vlog_number(vlog_node_t v);
void vlog_set_number(vlog_node_t v, number_t n);

data_type_t vlog_datatype(vlog_node_t v);
void vlog_set_datatype(vlog_node_t v, data_type_t dt);

double vlog_dval(vlog_node_t v);
void vlog_set_dval(vlog_node_t v, double d);

typedef void (*vlog_visit_fn_t)(vlog_node_t v, void *context);

void vlog_visit(vlog_node_t v, vlog_visit_fn_t fn, void *context);
void vlog_visit_only(vlog_node_t v, vlog_visit_fn_t fn, void *context,
                     vlog_kind_t kind);

typedef vlog_node_t (*vlog_rewrite_fn_t)(vlog_node_t t, void *context);

vlog_node_t vlog_rewrite(vlog_node_t v, vlog_rewrite_fn_t fn, void *context);

void vlog_locus(vlog_node_t v, ident_t *unit, ptrdiff_t *offset);

object_t *vlog_to_object(vlog_node_t v);
vlog_node_t vlog_from_object(object_t *obj);

#endif  // _VLOG_NODE_H
