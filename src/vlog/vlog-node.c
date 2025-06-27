//
//  Copyright (C) 2022-2025  Nick Gasson
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

#include "util.h"
#include "vlog/vlog-node.h"
#include "vlog/vlog-number.h"
#include "object.h"

#include <string.h>
#include <inttypes.h>
#include <stdlib.h>

static const imask_t has_map[V_LAST_NODE_KIND] = {
   // V_MODULE
   (I_IDENT | I_PORTS | I_STMTS | I_DECLS | I_IDENT2),

   // V_PORT_DECL
   (I_IDENT | I_SUBKIND | I_IDENT2 | I_REF | I_RANGES | I_TYPE | I_VALUE),

   // V_REF
   (I_IDENT | I_REF),

   // V_ALWAYS
   (I_IDENT | I_SUBKIND | I_STMTS),

   // V_TIMING
   (I_VALUE | I_STMTS),

   // V_NBASSIGN
   (I_TARGET | I_VALUE | I_DELAY),

   // V_EVENT
   (I_SUBKIND | I_VALUE),

   // V_INITIAL
   (I_IDENT | I_STMTS),

   // V_BLOCK
   (I_IDENT | I_DECLS | I_STMTS),

   // V_SYS_TCALL
   (I_IDENT | I_PARAMS),

   // V_STRING
   (I_NUMBER),

   // V_NUMBER
   (I_NUMBER),

   // V_NET_DECL
   (I_IDENT | I_SUBKIND | I_TYPE | I_RANGES | I_VALUE),

   // V_ASSIGN
   (I_TARGET | I_VALUE | I_IDENT | I_DELAY),

   // V_DIMENSION
   (I_SUBKIND | I_LEFT | I_RIGHT),

   // V_IF
   (I_CONDS),

   // V_COND
   (I_VALUE | I_STMTS),

   // V_VAR_DECL
   (I_IDENT | I_TYPE | I_RANGES | I_VALUE),

   // V_DELAY_CONTROL
   (I_VALUE),

   // V_BINARY
   (I_LEFT | I_RIGHT | I_SUBKIND),

   // V_BASSIGN
   (I_TARGET | I_VALUE | I_SUBKIND | I_DELAY),

   // V_UNARY
   (I_VALUE | I_SUBKIND),

   // V_GATE_INST
   (I_SUBKIND | I_PARAMS | I_IDENT | I_REF | I_TARGET),

   // V_STRENGTH
   (I_SUBKIND),

   // V_MOD_INST
   (I_IDENT | I_PARAMS),

   // V_BIT_SELECT
   (I_IDENT | I_REF | I_PARAMS),

   // V_SYS_FCALL
   (I_IDENT | I_PARAMS),

   // V_FOREVER
   (I_STMTS),

   // V_SPECIFY
   (0),

   // V_PRIMITIVE
   (I_IDENT | I_IDENT2 | I_PORTS | I_DECLS | I_STMTS),

   // V_UDP_TABLE
   (I_IDENT | I_PARAMS | I_SUBKIND | I_STMTS),

   // V_UDP_ENTRY
   (I_PARAMS),

   // V_DATA_TYPE
   (I_SUBKIND | I_RANGES),

   // V_TYPE_DECL
   (I_IDENT | I_TYPE),

   // V_ENUM_DECL
   (I_IDENT | I_TYPE | I_RANGES | I_DECLS),

   // V_ENUM_NAME
   (I_IDENT | I_TYPE),

   // V_UNION_DECL
   (I_IDENT | I_DECLS),

   // V_STRUCT_DECL
   (I_IDENT | I_DECLS),

   // V_EVENT_CONTROL
   (I_PARAMS),

   // V_EMPTY
   (0),

   // V_REPEAT
   (I_VALUE | I_STMTS),

   // V_WHILE
   (I_VALUE | I_STMTS),

   // V_DO_WHILE
   (I_VALUE | I_STMTS),

   // V_TASK_DECL
   (I_IDENT | I_STMTS | I_DECLS),

   // V_FUNC_DECL
   (I_IDENT | I_STMTS | I_DECLS | I_TYPE),

   // V_WAIT
   (I_VALUE | I_STMTS),

   // V_PARAM_DECL
   (I_IDENT | I_VALUE | I_TYPE),

   // V_COND_EXPR
   (I_VALUE | I_LEFT | I_RIGHT),

   // V_REAL
   (I_DVAL),

   // V_CONCAT
   (I_PARAMS | I_VALUE),

   // V_FOR_LOOP
   (I_LEFT | I_VALUE | I_RIGHT | I_STMTS),

   // V_FOR_INIT
   (I_DECLS | I_STMTS),

   // V_FOR_STEP
   (I_STMTS),

   // V_PREFIX
   (I_TARGET | I_SUBKIND),

   // V_POSTFIX
   (I_TARGET | I_SUBKIND),

   // V_LOCALPARAM
   (I_IDENT | I_VALUE | I_TYPE),

   // V_CASE
   (I_SUBKIND | I_VALUE | I_STMTS),

   // V_CASE_ITEM
   (I_PARAMS | I_STMTS),

   // V_INST_LIST
   (I_PARAMS | I_STMTS | I_IDENT),

   // V_PARAM_ASSIGN
   (I_IDENT | I_VALUE),

   // V_INST_BODY
   (I_IDENT | I_IDENT2 | I_PORTS | I_STMTS | I_DECLS),

   // V_PORT_CONN
   (I_IDENT | I_VALUE),

   // V_PART_SELECT
   (I_SUBKIND | I_VALUE | I_LEFT | I_RIGHT),

   // V_IF_GENERATE
   (I_CONDS),

   // V_EVENT_TRIGGER
   (I_IDENT),

   // V_USER_FCALL
   (I_IDENT | I_PARAMS),

   // V_UDP_LEVEL
   (I_SUBKIND | I_IVAL),

   // V_UDP_EDGE
   (I_SUBKIND | I_LEFT | I_RIGHT),
};

static const char *kind_text_map[V_LAST_NODE_KIND] = {
   "V_MODULE",        "V_PORT_DECL",   "V_REF",           "V_ALWAYS",
   "V_TIMING",        "V_NBASSIGN",    "V_EVENT",         "V_INITIAL",
   "V_BLOCK",         "V_SYS_TCALL",   "V_STRING",        "V_NUMBER",
   "V_NET_DECL",      "V_ASSIGN",      "V_DIMENSION",     "V_IF",
   "V_COND",          "V_VAR_DECL",    "V_DELAY_CONTROL", "V_BINARY",
   "V_BASSIGN",       "V_UNARY",       "V_GATE_INST",     "V_STRENGTH",
   "V_MOD_INST",      "V_BIT_SELECT",  "V_SYS_FCALL",     "V_FOREVER",
   "V_SPECIFY",       "V_PRIMITIVE",   "V_UDP_TABLE",     "V_UDP_ENTRY",
   "V_DATA_TYPE",     "V_TYPE_DECL",   "V_ENUM_DECL",     "V_ENUM_NAME",
   "V_UNION_DECL",    "V_STRUCT_DECL", "V_EVENT_CONTROL", "V_EMPTY",
   "V_REPEAT",        "V_WHILE",       "V_DO_WHILE",      "V_TASK_DECL",
   "V_FUNC_DECL",     "V_WAIT",        "V_PARAM_DECL",    "V_COND_EXPR",
   "V_REAL",          "V_CONCAT",      "V_FOR_LOOP",      "V_FOR_INIT",
   "V_FOR_STEP",      "V_PREFIX",      "V_POSTFIX",       "V_LOCALPARAM",
   "V_CASE",          "V_CASE_ITEM",   "V_INST_LIST",     "V_PARAM_ASSIGN",
   "V_INST_BODY",     "V_PORT_CONN",   "V_PART_SELECT",   "V_IF_GENERATE",
   "V_EVENT_TRIGGER", "V_USER_FCALL",  "V_UDP_LEVEL",     "V_UDP_EDGE",
};

static const change_allowed_t change_allowed[] = {
   { -1, -1 }
};

object_class_t vlog_object = {
   .name           = "vlog",
   .change_allowed = change_allowed,
   .has_map        = has_map,
   .kind_text_map  = kind_text_map,
   .tag            = OBJECT_TAG_VLOG,
   .last_kind      = V_LAST_NODE_KIND,
   .has_loc        = true,
   .gc_roots       = { V_MODULE, V_PRIMITIVE },
   .gc_num_roots   = 2
};

struct _vlog_node {
   object_t object;
};

static inline vlog_node_t vlog_array_nth(item_t *item, unsigned n)
{
   object_t *o = obj_array_nth(item->obj_array, n);
   return container_of(o, struct _vlog_node, object);
}

static inline void vlog_array_add(item_t *item, vlog_node_t v)
{
   obj_array_add(&(item->obj_array), &(v->object));
}

vlog_node_t vlog_new(vlog_kind_t kind)
{
   object_t *o = object_new(NULL, &vlog_object, kind);
   return container_of(o, struct _vlog_node, object);
}

vlog_kind_t vlog_kind(vlog_node_t v)
{
   return v->object.kind;
}

const char *vlog_kind_str(vlog_kind_t kind)
{
   return kind_text_map[kind];
}

ident_t vlog_ident(vlog_node_t v)
{
   item_t *item = lookup_item(&vlog_object, v, I_IDENT);
   assert(item->ident != NULL);
   return item->ident;
}

void vlog_set_ident(vlog_node_t v, ident_t i)
{
   lookup_item(&vlog_object, v, I_IDENT)->ident = i;
}

bool vlog_has_ident(vlog_node_t v)
{
   return lookup_item(&vlog_object, v, I_IDENT)->ident != NULL;
}

ident_t vlog_ident2(vlog_node_t v)
{
   item_t *item = lookup_item(&vlog_object, v, I_IDENT2);
   assert(item->ident != NULL);
   return item->ident;
}

void vlog_set_ident2(vlog_node_t v, ident_t i)
{
   lookup_item(&vlog_object, v, I_IDENT2)->ident = i;
}

const loc_t *vlog_loc(vlog_node_t v)
{
   assert(v != NULL);
   return &(v->object.loc);
}

void vlog_set_loc(vlog_node_t v, const loc_t *loc)
{
   assert(v != NULL);
   assert(loc != NULL);
   v->object.loc = *loc;
}

vlog_node_t vlog_ref(vlog_node_t v)
{
   item_t *item = lookup_item(&vlog_object, v, I_REF);
   assert(item->object != NULL);
   return container_of(item->object, struct _vlog_node, object);
}

bool vlog_has_ref(vlog_node_t v)
{
   return lookup_item(&vlog_object, v, I_REF)->object != NULL;
}

void vlog_set_ref(vlog_node_t v, vlog_node_t d)
{
   lookup_item(&vlog_object, v, I_REF)->object = &(d->object);
   object_write_barrier(&(v->object), &(d->object));
}

unsigned vlog_stmts(vlog_node_t v)
{
   item_t *item = lookup_item(&vlog_object, v, I_STMTS);
   return obj_array_count(item->obj_array);
}

vlog_node_t vlog_stmt(vlog_node_t v, unsigned n)
{
   item_t *item = lookup_item(&vlog_object, v, I_STMTS);
   return vlog_array_nth(item, n);
}

void vlog_add_stmt(vlog_node_t v, vlog_node_t s)
{
   assert(s != NULL);
   vlog_array_add(lookup_item(&vlog_object, v, I_STMTS), s);
   object_write_barrier(&(v->object), &(s->object));
}

unsigned vlog_ports(vlog_node_t v)
{
   item_t *item = lookup_item(&vlog_object, v, I_PORTS);
   return obj_array_count(item->obj_array);
}

vlog_node_t vlog_port(vlog_node_t v, unsigned n)
{
   item_t *item = lookup_item(&vlog_object, v, I_PORTS);
   return vlog_array_nth(item, n);
}

void vlog_add_port(vlog_node_t v, vlog_node_t p)
{
   assert(p != NULL);
   assert(p->object.kind == V_REF);
   vlog_array_add(lookup_item(&vlog_object, v, I_PORTS), p);
   object_write_barrier(&(v->object), &(p->object));
}

unsigned vlog_params(vlog_node_t v)
{
   item_t *item = lookup_item(&vlog_object, v, I_PARAMS);
   return obj_array_count(item->obj_array);
}

vlog_node_t vlog_param(vlog_node_t v, unsigned n)
{
   item_t *item = lookup_item(&vlog_object, v, I_PARAMS);
   return vlog_array_nth(item, n);
}

void vlog_add_param(vlog_node_t v, vlog_node_t p)
{
   assert(p != NULL);
   vlog_array_add(lookup_item(&vlog_object, v, I_PARAMS), p);
   object_write_barrier(&(v->object), &(p->object));
}

unsigned vlog_ranges(vlog_node_t v)
{
   item_t *item = lookup_item(&vlog_object, v, I_RANGES);
   return obj_array_count(item->obj_array);
}

vlog_node_t vlog_range(vlog_node_t v, unsigned n)
{
   item_t *item = lookup_item(&vlog_object, v, I_RANGES);
   return vlog_array_nth(item, n);
}

void vlog_add_range(vlog_node_t v, vlog_node_t r)
{
   assert(r != NULL);
   assert(r->object.kind == V_DIMENSION);
   vlog_array_add(lookup_item(&vlog_object, v, I_RANGES), r);
   object_write_barrier(&(v->object), &(r->object));
}

unsigned vlog_decls(vlog_node_t v)
{
   item_t *item = lookup_item(&vlog_object, v, I_DECLS);
   return obj_array_count(item->obj_array);
}

vlog_node_t vlog_decl(vlog_node_t v, unsigned n)
{
   item_t *item = lookup_item(&vlog_object, v, I_DECLS);
   return vlog_array_nth(item, n);
}

void vlog_add_decl(vlog_node_t v, vlog_node_t d)
{
   assert(d != NULL);
   vlog_array_add(lookup_item(&vlog_object, v, I_DECLS), d);
   object_write_barrier(&(v->object), &(d->object));
}

unsigned vlog_conds(vlog_node_t v)
{
   item_t *item = lookup_item(&vlog_object, v, I_CONDS);
   return obj_array_count(item->obj_array);
}

vlog_node_t vlog_cond(vlog_node_t v, unsigned n)
{
   item_t *item = lookup_item(&vlog_object, v, I_CONDS);
   return vlog_array_nth(item, n);
}

void vlog_add_cond(vlog_node_t v, vlog_node_t c)
{
   assert(c != NULL && c->object.kind == V_COND);
   vlog_array_add(lookup_item(&vlog_object, v, I_CONDS), c);
   object_write_barrier(&(v->object), &(c->object));
}

unsigned vlog_subkind(vlog_node_t v)
{
   return lookup_item(&vlog_object, v, I_SUBKIND)->ival;
}

void vlog_set_subkind(vlog_node_t v, unsigned sub)
{
   lookup_item(&vlog_object, v, I_SUBKIND)->ival = sub;
}

vlog_node_t vlog_value(vlog_node_t v)
{
   item_t *item = lookup_item(&vlog_object, v, I_VALUE);
   assert(item->object != NULL);
   return container_of(item->object, struct _vlog_node, object);
}

bool vlog_has_value(vlog_node_t v)
{
   return lookup_item(&vlog_object, v, I_VALUE)->object != NULL;
}

void vlog_set_value(vlog_node_t v, vlog_node_t e)
{
   lookup_item(&vlog_object, v, I_VALUE)->object = &(e->object);
   object_write_barrier(&(v->object), &(e->object));
}

vlog_node_t vlog_target(vlog_node_t v)
{
   item_t *item = lookup_item(&vlog_object, v, I_TARGET);
   assert(item->object != NULL);
   return container_of(item->object, struct _vlog_node, object);
}

void vlog_set_target(vlog_node_t v, vlog_node_t e)
{
   lookup_item(&vlog_object, v, I_TARGET)->object = &(e->object);
   object_write_barrier(&(v->object), &(e->object));
}

vlog_node_t vlog_delay(vlog_node_t v)
{
   item_t *item = lookup_item(&vlog_object, v, I_DELAY);
   assert(item->object != NULL);
   return container_of(item->object, struct _vlog_node, object);
}

bool vlog_has_delay(vlog_node_t v)
{
   return lookup_item(&vlog_object, v, I_DELAY)->object != NULL;
}

void vlog_set_delay(vlog_node_t v, vlog_node_t d)
{
   lookup_item(&vlog_object, v, I_DELAY)->object = &(d->object);
   object_write_barrier(&(v->object), &(d->object));
}

vlog_node_t vlog_type(vlog_node_t v)
{
   item_t *item = lookup_item(&vlog_object, v, I_TYPE);
   assert(item->object != NULL);
   return container_of(item->object, struct _vlog_node, object);
}

bool vlog_has_type(vlog_node_t v)
{
   return lookup_item(&vlog_object, v, I_TYPE)->object != NULL;
}

void vlog_set_type(vlog_node_t v, vlog_node_t t)
{
   lookup_item(&vlog_object, v, I_TYPE)->object = &(t->object);
   object_write_barrier(&(v->object), &(t->object));
}

number_t vlog_number(vlog_node_t v)
{
   return lookup_item(&vlog_object, v, I_NUMBER)->number;
}

void vlog_set_number(vlog_node_t v, number_t n)
{
   lookup_item(&vlog_object, v, I_NUMBER)->number = n;
}

vlog_node_t vlog_left(vlog_node_t v)
{
   item_t *item = lookup_item(&vlog_object, v, I_LEFT);
   assert(item->object != NULL);
   return container_of(item->object, struct _vlog_node, object);
}

void vlog_set_left(vlog_node_t v, vlog_node_t e)
{
   lookup_item(&vlog_object, v, I_LEFT)->object = &(e->object);
   object_write_barrier(&(v->object), &(e->object));
}

vlog_node_t vlog_right(vlog_node_t v)
{
   item_t *item = lookup_item(&vlog_object, v, I_RIGHT);
   assert(item->object != NULL);
   return container_of(item->object, struct _vlog_node, object);
}

void vlog_set_right(vlog_node_t v, vlog_node_t e)
{
   lookup_item(&vlog_object, v, I_RIGHT)->object = &(e->object);
   object_write_barrier(&(v->object), &(e->object));
}

double vlog_dval(vlog_node_t v)
{
   return lookup_item(&vlog_object, v, I_DVAL)->dval;
}

void vlog_set_dval(vlog_node_t v, double d)
{
   lookup_item(&vlog_object, v, I_DVAL)->dval = d;
}

int64_t vlog_ival(vlog_node_t v)
{
   return lookup_item(&vlog_object, v, I_IVAL)->ival;
}

void vlog_set_ival(vlog_node_t v, int64_t i)
{
   lookup_item(&vlog_object, v, I_IVAL)->ival = i;
}

void vlog_visit(vlog_node_t v, vlog_visit_fn_t fn, void *context)
{
   vlog_visit_only(v, fn, context, V_LAST_NODE_KIND);
}

void vlog_visit_only(vlog_node_t v, vlog_visit_fn_t fn, void *context,
                     vlog_kind_t kind)
{
   assert(v != NULL);

   object_visit_ctx_t ctx = {
      .count      = 0,
      .postorder  = (object_visit_fn_t)fn,
      .preorder   = NULL,
      .context    = context,
      .kind       = kind,
      .tag        = OBJECT_TAG_VLOG,
      .generation = object_next_generation(),
      .deep       = false
   };

   object_visit(&(v->object), &ctx);
}

vlog_node_t vlog_rewrite(vlog_node_t v, vlog_rewrite_fn_t fn, void *context)
{
   object_arena_t *arena = object_arena(&(v->object));
   if (arena_frozen(arena))
      return v;

   object_rewrite_ctx_t ctx = {
      .generation = object_next_generation(),
      .context    = context,
      .arena      = arena,
   };

   ctx.post_fn[OBJECT_TAG_VLOG] = (object_rewrite_post_fn_t)fn;

   object_t *result = object_rewrite(&(v->object), &ctx);
   free(ctx.cache);
   return container_of(result, struct _vlog_node, object);
}

vlog_node_t vlog_copy(vlog_node_t v, vlog_copy_pred_t pred, void *ctx)
{
   object_copy_ctx_t *copy LOCAL = xcalloc_flex(sizeof(object_copy_ctx_t),
                                                1, sizeof(object_t *));
   copy->generation   = object_next_generation();
   copy->pred_context = ctx;
   copy->nroots       = 1;
   copy->roots[0]     = &(v->object);

   copy->should_copy[OBJECT_TAG_VLOG] = (object_copy_pred_t)pred;

   object_copy(copy);

   return container_of(copy->roots[0], struct _vlog_node, object);
}

object_t *vlog_to_object(vlog_node_t v)
{
   return &(v->object);
}

vlog_node_t vlog_from_object(object_t *obj)
{
   if (obj != NULL && obj->tag == OBJECT_TAG_VLOG)
      return container_of(obj, struct _vlog_node, object);
   else
      return NULL;
}
