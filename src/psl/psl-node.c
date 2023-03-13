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

#include "util.h"
#include "object.h"
#include "psl/psl-node.h"

static const imask_t has_map[P_LAST_PSL_KIND] = {
   // P_ASSERT
   (I_VALUE | I_MESSAGE),

   // P_ASSUME
   (I_VALUE | I_MESSAGE),

   // P_COVER
   (I_VALUE | I_MESSAGE),

   // P_ALWAYS
   (I_VALUE | I_CLOCK),

   // P_HDL_EXPR
   (I_FOREIGN | I_CLASS | I_CLOCK),

   // P_CLOCK_DECL
   (I_FOREIGN),

   // P_NEXT
   (I_SUBKIND | I_VALUE),

   // P_NEVER
   (I_VALUE | I_CLOCK),

   // P_EVENTUALLY
   (I_VALUE | I_CLOCK),

   // P_NEXT_A
   (I_SUBKIND | I_VALUE),

   // P_NEXT_E
   (I_SUBKIND | I_VALUE),

   // P_NEXT_EVENT
   (I_SUBKIND | I_VALUE),

   // P_SERE
   (I_PARAMS | I_CLOCK),

   // P_IMPLICATION
   (I_SUBKIND | I_PARAMS | I_CLOCK),
};

static const char *kind_text_map[P_LAST_PSL_KIND] = {
   "P_ASSERT", "P_ALWAYS", "P_HDL_EXPR", "P_CLOCK_DECL", "P_NEXT",
   "P_NEVER", "P_EVENTUALLY", "P_NEXT_A", "P_NEXT_E", "P_NEXT_EVENT",
   "P_SERE", "P_IMPLICATION",
};

static const change_allowed_t change_allowed[] = {
   { -1, -1 }
};

struct _psl_node {
   object_t object;
};

struct _tree {
   object_t object;
};

object_class_t psl_object = {
   .name           = "psl",
   .change_allowed = change_allowed,
   .has_map        = has_map,
   .kind_text_map  = kind_text_map,
   .tag            = OBJECT_TAG_PSL,
   .last_kind      = P_LAST_PSL_KIND
};

static inline psl_node_t psl_array_nth(item_t *item, unsigned n)
{
   object_t *o = obj_array_nth(item->obj_array, n);
   return container_of(o, struct _psl_node, object);
}

static inline void psl_array_add(item_t *item, psl_node_t p)
{
   obj_array_add(&(item->obj_array), &(p->object));
}

psl_node_t psl_new(psl_kind_t kind)
{
   object_t *o = object_new(NULL, &psl_object, kind);
   return container_of(o, struct _psl_node, object);
}

psl_kind_t psl_kind(psl_node_t p)
{
   return p->object.kind;
}

const char *psl_kind_str(psl_kind_t kind)
{
   return kind_text_map[kind];
}

const loc_t *psl_loc(psl_node_t p)
{
   assert(p != NULL);
   return &(p->object.loc);
}

void psl_set_loc(psl_node_t p, const loc_t *loc)
{
   assert(p != NULL);
   assert(loc != NULL);
   p->object.loc = *loc;
}

unsigned psl_subkind(psl_node_t p)
{
   return lookup_item(&psl_object, p, I_SUBKIND)->ival;
}

void psl_set_subkind(psl_node_t p, unsigned sub)
{
   lookup_item(&psl_object, p, I_SUBKIND)->ival = sub;
}

psl_type_t psl_type(psl_node_t p)
{
   return lookup_item(&psl_object, p, I_CLASS)->ival;
}

void psl_set_type(psl_node_t p, psl_type_t type)
{
   lookup_item(&psl_object, p, I_CLASS)->ival = type;
}

tree_t psl_tree(psl_node_t p)
{
   item_t *item = lookup_item(&psl_object, p, I_FOREIGN);
   assert(item->object != NULL);
   return container_of(item->object, struct _tree, object);
}

void psl_set_tree(psl_node_t p, tree_t t)
{
   assert(p != NULL);
   lookup_item(&psl_object, p, I_FOREIGN)->object = &(t->object);
   object_write_barrier(&(p->object), &(t->object));
}

psl_node_t psl_value(psl_node_t p)
{
   item_t *item = lookup_item(&psl_object, p, I_VALUE);
   assert(item->object != NULL);
   return container_of(item->object, struct _psl_node, object);
}

void psl_set_value(psl_node_t p, psl_node_t v)
{
   lookup_item(&psl_object, p, I_VALUE)->object = &(v->object);
   object_write_barrier(&(p->object), &(v->object));
}

unsigned psl_operands(psl_node_t p)
{
   item_t *item = lookup_item(&psl_object, p, I_PARAMS);
   return obj_array_count(item->obj_array);
}

psl_node_t psl_operand(psl_node_t p, unsigned n)
{
   item_t *item = lookup_item(&psl_object, p, I_PARAMS);
   return psl_array_nth(item, n);
}

void psl_add_operand(psl_node_t p, psl_node_t o)
{
   assert(o != NULL);
   psl_array_add(lookup_item(&psl_object, p, I_PARAMS), o);
   object_write_barrier(&(p->object), &(o->object));
}

psl_node_t psl_clock(psl_node_t p)
{
   item_t *item = lookup_item(&psl_object, p, I_CLOCK);
   assert(item->object != NULL);
   return container_of(item->object, struct _psl_node, object);
}

bool psl_has_clock(psl_node_t p)
{
   return lookup_item(&psl_object, p, I_CLOCK)->object != NULL;
}

void psl_set_clock(psl_node_t p, psl_node_t clk)
{
   assert(clk == NULL || clk->object.kind == P_CLOCK_DECL);
   lookup_item(&psl_object, p, I_CLOCK)->object = &(clk->object);
   object_write_barrier(&(p->object), &(clk->object));
}

tree_t psl_message(psl_node_t p)
{
   item_t *item = lookup_item(&psl_object, p, I_MESSAGE);
   assert(item->object != NULL);
   return container_of(item->object, struct _tree, object);
}

bool psl_has_message(psl_node_t p)
{
   return lookup_item(&psl_object, p, I_MESSAGE)->object != NULL;
}

void psl_set_message(psl_node_t p, tree_t m)
{
   assert(m == NULL || m->object.kind == T_FCALL);
   lookup_item(&psl_object, p, I_MESSAGE)->object = &(m->object);
   object_write_barrier(&(p->object), &(m->object));
}

object_t *psl_to_object(psl_node_t p)
{
   return &(p->object);
}

psl_node_t psl_from_object(object_t *obj)
{
   assert(obj != NULL);
   if (obj->tag == OBJECT_TAG_PSL)
      return container_of(obj, struct _psl_node, object);
   else
      return NULL;
}
