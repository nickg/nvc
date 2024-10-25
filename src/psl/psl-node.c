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
   (I_SUBKIND | I_VALUE | I_MESSAGE),

   // P_RESTRICT
   (I_SUBKIND | I_VALUE | I_MESSAGE),

   // P_FAIRNESS
   (I_PARAMS | I_MESSAGE | I_FLAGS),

   // P_COVER
   (I_VALUE | I_MESSAGE),

   // P_ALWAYS
   (I_VALUE | I_CLOCK),

   // P_HDL_EXPR
   (I_FOREIGN | I_CLASS | I_CLOCK),

   // P_PROPERTY_DECL
   (I_VALUE | I_IDENT | I_PORTS),

   // P_SEQUENCE_DECL
   (I_VALUE | I_IDENT | I_PORTS),

   // P_CLOCK_DECL
   (I_FOREIGN),

   // P_NEXT
   (I_VALUE | I_DELAY | I_FLAGS),

   // P_NEVER
   (I_VALUE | I_CLOCK),

   // P_EVENTUALLY
   (I_VALUE | I_CLOCK),

   // P_NEXT_A
   (I_VALUE | I_DELAY | I_FLAGS),

   // P_NEXT_E
   (I_VALUE | I_DELAY | I_FLAGS),

   // P_NEXT_EVENT
   (I_VALUE | I_DELAY | I_FLAGS),

   // P_SERE
   (I_SUBKIND | I_PARAMS | I_CLOCK | I_REPEAT | I_DECLS),

   // P_IMPLICATION
   (I_SUBKIND | I_PARAMS | I_CLOCK),

   // P_REPEAT,
   (I_SUBKIND | I_FOREIGN),

   // P_PROPERTY_INST
   (I_REF | I_PARAMS),

   // P_SEQUENCE_INST
   (I_REF | I_PARAMS | I_CLOCK | I_REPEAT),

   // P_UNION
   (I_PARAMS),

   // P_BUILTIN_FUNC
   (I_SUBKIND | I_PARAMS | I_CLOCK),

   // P_VALUE_SET
   (I_SUBKIND | I_PARAMS),

   // P_PARAM
   (I_FOREIGN | I_VALUE),

   // P_UNTIL
   (I_PARAMS | I_CLOCK | I_FLAGS),

   // P_SEQ_IMPLICATION
   (I_SUBKIND | I_PARAMS | I_CLOCK)
};

static const char *kind_text_map[P_LAST_PSL_KIND] = {
   "P_ASSERT", "P_ASSUME", "P_RESTRICT", "P_FAIRNESS", "P_COVER", "P_ALWAYS",
   "P_HDL_EXPR", "P_PROPERTY_DECL", "P_SEQUENCE_DECL", "P_CLOCK_DECL", "P_NEXT",
   "P_NEVER", "P_EVENTUALLY", "P_NEXT_A", "P_NEXT_E", "P_NEXT_EVENT",
   "P_SERE", "P_IMPLICATION", "P_REPEAT", "P_PROPERTY_INST", "P_SEQUENCE_INST",
   "P_UNION", "P_BUILTIN_FUNC", "P_VALUE_SET", "P_PARAM", "P_UNTIL",
   "P_SEQ_IMPLICATION"
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

struct _type {
   object_t object;
};

object_class_t psl_object = {
   .name           = "psl",
   .change_allowed = change_allowed,
   .has_map        = has_map,
   .kind_text_map  = kind_text_map,
   .has_loc        = true,
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

static inline tree_t tree_array_nth(item_t *item, unsigned n)
{
   object_t *o = obj_array_nth(item->obj_array, n);
   return container_of(o, struct _tree, object);
}

static inline void tree_array_add(item_t *item, tree_t t)
{
   obj_array_add(&(item->obj_array), &(t->object));
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

bool psl_has_tree(psl_node_t p)
{
   return lookup_item(&psl_object, p, I_FOREIGN)->object != NULL;
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

tree_t psl_delay(psl_node_t p)
{
   item_t *item = lookup_item(&psl_object, p, I_DELAY);
   assert(item->object != NULL);
   return container_of(item->object, struct _tree, object);
}

void psl_set_delay(psl_node_t p, tree_t d)
{
   lookup_item(&psl_object, p, I_DELAY)->object = &(d->object);
   object_write_barrier(&(p->object), &(d->object));
}

bool psl_has_delay(psl_node_t p)
{
   return lookup_item(&psl_object, p, I_DELAY)->object != NULL;
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
   lookup_item(&psl_object, p, I_MESSAGE)->object = &(m->object);
   object_write_barrier(&(p->object), &(m->object));
}

unsigned psl_ports(psl_node_t p)
{
   item_t *item = lookup_item(&psl_object, p, I_PORTS);
   return obj_array_count(item->obj_array);
}

tree_t psl_port(psl_node_t p, unsigned n)
{
   item_t *item = lookup_item(&psl_object, p, I_PORTS);
   return tree_array_nth(item, n);
}

void psl_add_port(psl_node_t p, tree_t o)
{
   assert(o != NULL);
   tree_array_add(lookup_item(&psl_object, p, I_PORTS), o);
   object_write_barrier(&(p->object), &(o->object));
}

ident_t psl_ident(psl_node_t p)
{
   item_t *item = lookup_item(&psl_object, p, I_IDENT);
   assert(item->ident != NULL);
   return item->ident;
}

bool psl_has_ident(psl_node_t p)
{
   return lookup_item(&psl_object, p, I_IDENT)->ident != NULL;
}

void psl_set_ident(psl_node_t p, ident_t i)
{
   lookup_item(&psl_object, p, I_IDENT)->ident = i;
}

unsigned psl_decls(psl_node_t p)
{
   item_t *item = lookup_item(&psl_object, p, I_DECLS);
   return obj_array_count(item->obj_array);
}

void psl_add_decl(psl_node_t p, tree_t r)
{
   assert(r != NULL);
   tree_array_add(lookup_item(&psl_object, p, I_DECLS), r);
   object_write_barrier(&(p->object), &(r->object));
}

tree_t psl_decl(psl_node_t p, unsigned n)
{
   item_t *item = lookup_item(&psl_object, p, I_DECLS);
   return tree_array_nth(item, n);
}

void psl_set_ref(psl_node_t p, psl_node_t r)
{
   lookup_item(&psl_object, p, I_REF)->object = &(r->object);
   object_write_barrier(&(p->object), &(r->object));
}

psl_node_t psl_ref(psl_node_t p)
{
   item_t *item = lookup_item(&psl_object, p, I_REF);
   assert(item->object != NULL);
   return container_of(item->object, struct _psl_node, object);
}

void psl_set_repeat(psl_node_t p, psl_node_t r)
{
   lookup_item(&psl_object, p, I_REPEAT)->object = &(r->object);
   object_write_barrier(&(p->object), &(r->object));
}

psl_node_t psl_repeat(psl_node_t p)
{
   item_t *item = lookup_item(&psl_object, p, I_REPEAT);
   assert(item->object != NULL);
   return container_of(item->object, struct _psl_node, object);
}

bool psl_has_repeat(psl_node_t p)
{
   return lookup_item(&psl_object, p, I_REPEAT)->object != NULL;
}

psl_flags_t psl_flags(psl_node_t p)
{
   return lookup_item(&psl_object, p, I_FLAGS)->ival;
}

void psl_set_flag(psl_node_t p, psl_flags_t mask)
{
   lookup_item(&psl_object, p, I_FLAGS)->ival |= mask;
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

void psl_locus(psl_node_t p, ident_t *unit, ptrdiff_t *offset)
{
   assert(p != NULL);
   object_locus(&(p->object), unit, offset);
}
