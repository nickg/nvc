//
//  Copyright (C) 2022  Nick Gasson
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
   V_PORT_OUTPUT_REG,
   V_PORT_INOUT,
} v_port_kind_t;

typedef enum {
   V_SYS_DISPLAY,
   V_SYS_WRITE,
   V_SYS_FINISH,
} v_systask_kind_t;

typedef enum {
   V_NET_WIRE,
} vlog_net_kind_t;

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
   V_SYSTASK_ENABLE,
   V_STRING,
   V_NUMBER,
   V_NET_DECL,
   V_ASSIGN,
   V_ROOT,

   V_LAST_NODE_KIND
} vlog_kind_t;

vlog_node_t vlog_new(vlog_kind_t kind);
vlog_kind_t vlog_kind(vlog_node_t v);
const char *vlog_kind_str(vlog_kind_t kind);

void make_new_vlog_arena(void);

const loc_t *vlog_loc(vlog_node_t v);
void vlog_set_loc(vlog_node_t v, const loc_t *loc);

ident_t vlog_ident(vlog_node_t v);
void vlog_set_ident(vlog_node_t v, ident_t i);

ident_t vlog_ident2(vlog_node_t v);
void vlog_set_ident2(vlog_node_t v, ident_t i);

vlog_node_t vlog_ref(vlog_node_t v);
void vlog_set_ref(vlog_node_t v, vlog_node_t d);

unsigned vlog_stmts(vlog_node_t v);
vlog_node_t vlog_stmt(vlog_node_t v, unsigned n);
void vlog_add_stmt(vlog_node_t v, vlog_node_t s);

unsigned vlog_ports(vlog_node_t v);
vlog_node_t vlog_port(vlog_node_t v, unsigned n);
void vlog_add_port(vlog_node_t v, vlog_node_t p);

unsigned vlog_params(vlog_node_t v);
vlog_node_t vlog_param(vlog_node_t v, unsigned n);
void vlog_add_param(vlog_node_t v, vlog_node_t p);

unsigned vlog_decls(vlog_node_t v);
vlog_node_t vlog_decl(vlog_node_t v, unsigned n);
void vlog_add_decl(vlog_node_t v, vlog_node_t d);

unsigned vlog_subkind(vlog_node_t v);
void vlog_set_subkind(vlog_node_t v, unsigned sub);

vlog_node_t vlog_value(vlog_node_t v);
void vlog_set_value(vlog_node_t v, vlog_node_t e);

vlog_node_t vlog_target(vlog_node_t v);
void vlog_set_target(vlog_node_t v, vlog_node_t e);

typedef void (*vlog_visit_fn_t)(vlog_node_t v, void *context);

void vlog_visit(vlog_node_t v, vlog_visit_fn_t fn, void *context);
void vlog_visit_only(vlog_node_t v, vlog_visit_fn_t fn, void *context,
                     vlog_kind_t kind);

void vlog_locus(vlog_node_t v, ident_t *unit, ptrdiff_t *offset);

object_t *vlog_to_object(vlog_node_t v);
vlog_node_t vlog_from_object(object_t *obj);

#endif  // _VLOG_NODE_H
