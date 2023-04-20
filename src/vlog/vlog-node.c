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

#include "vlog/vlog-node.h"
#include "common.h"
#include "object.h"

#include <string.h>
#include <inttypes.h>
#include <stdlib.h>

static const imask_t has_map[V_LAST_NODE_KIND] = {
   // V_MODULE
   (I_IDENT | I_PORTS | I_STMTS | I_DECLS | I_IDENT2),

   // V_PORT_DECL
   (I_IDENT | I_SUBKIND | I_IDENT2),

   // V_REF
   (I_IDENT | I_REF),

   // V_ALWAYS
   (I_STMTS),

   // V_TIMING
   (I_VALUE | I_STMTS),

   // V_NBASSIGN
   (I_TARGET | I_VALUE),

   // V_EVENT
   (I_SUBKIND | I_VALUE),

   // V_INITIAL
   (I_STMTS),

   // V_SEQ_BLOCK
   (I_STMTS),

   // V_SYSTASK_ENABLE
   (I_IDENT | I_SUBKIND | I_PARAMS),

   // V_STRING
   (0),

   // V_NUMBER
   (0),

   // V_NET_DECL
   (I_IDENT | I_SUBKIND),

   // V_ASSIGN
   (I_TARGET | I_VALUE),

   // V_ROOT
   (I_IDENT | I_STMTS | I_DECLS),
};

static const char *kind_text_map[V_LAST_NODE_KIND] = {
   "V_MODULE",    "V_PORT_DECL",      "V_REF",    "V_ALWAYS",
   "V_TIMING",    "V_NBASSIGN",       "V_EVENT",  "V_INITIAL",
   "V_SEQ_BLOCK", "V_SYSTASK_ENABLE", "V_STRING", "V_NUMBER",
   "V_NET_DECL",  "V_ASSIGN",         "V_ROOT",
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
   .gc_roots       = { V_MODULE },
   .gc_num_roots   = 1
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

void vlog_locus(vlog_node_t v, ident_t *unit, ptrdiff_t *offset)
{
   assert(v != NULL);
   object_locus(&(v->object), unit, offset);
}
