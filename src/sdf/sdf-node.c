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

#include "util.h"
#include "prim.h"
#include "sdf/sdf-node.h"
#include "common.h"
#include "object.h"

static const imask_t has_map[S_LAST_NODE_KIND] = {
   // S_DELAY_FILE
   (I_IDENT | I_SUBKIND | I_DECLS | I_STMTS),

   // S_HEADER_ITEM
   (I_SUBKIND | I_NUMBER | I_IDENT),

   // S_CELL
   (I_IDENT | I_IDENT2 | I_WAVES | I_TRIGGERS | I_PARAMS | I_CONSTR),

   // S_DELAY
   (I_SUBKIND | I_PORTS | I_LITERALS | I_FLAGS | I_CONDS),

   // S_TIMING_CHECK
   (I_SUBKIND | I_PORTS | I_LITERALS | I_CONDS),

   // S_LABEL
   (I_IDENT | I_LITERALS | I_FLAGS),

   // S_COND
   (I_SUBKIND | I_IDENT | I_VALUE),

   // S_SIGNAL
   (I_SUBKIND | I_IDENT | I_DIMS | I_FLAGS | I_VALUE | I_CONDS),

   // S_DELVAL
   (I_LITERALS),

   // S_VALUE
   (I_NUMBER),

   // S_TRIPPLE
   (I_LEFT | I_BASE | I_RIGHT),

   // S_NUMBER
   (I_SUBKIND | I_DVAL | I_IVAL),

   // S_TIMING_ENV
   (I_SUBKIND | I_IDENT | I_PORTS | I_LITERALS | I_CONTEXT),

   // S_EXCEPTION
   (I_IDENT),

   // S_CONSTR_PATH
   (I_PORTS),

   // S_TIMING_ENV
   (I_SUBKIND | I_PORTS | I_LITERALS),

   // S_UNARY
   (I_SUBKIND | I_LITERALS),

   // S_BINARY
   (I_SUBKIND | I_LITERALS),

   // S_COMPLEX
   (I_SUBKIND | I_LITERALS),

   // S_EDGE
   (I_LITERALS | I_FLAGS),

   // S_RETAIN
   (I_LITERALS)
};

static const char *kind_text_map[S_LAST_NODE_KIND] = {
   "S_DELAY_FILE",   "S_HEADER_ITEM",  "S_CELL",         "S_DELAY",
   "S_TIMING_CHECK", "S_LABEL",        "S_COND",         "S_SIGNAL",
   "S_DELVAL",       "S_VALUE",        "S_TRIPPLE",      "S_NUMBER",
   "S_TIMING_ENV",   "S_EXCEPTION",    "S_CONSTR_PATH",  "S_TIMING_ENV",
   "S_UNARY",        "S_BINARY",       "S_COMPLEX",      "S_EDGE",
   "S_RETAIN"
};

static const change_allowed_t change_allowed[] = {
   { -1, -1 }
};

object_class_t sdf_object = {
   .name           = "sdf",
   .change_allowed = change_allowed,
   .has_map        = has_map,
   .kind_text_map  = kind_text_map,
   .tag            = OBJECT_TAG_SDF,
   .last_kind      = S_LAST_NODE_KIND,
   .has_loc        = false,
   .gc_roots       = { S_DELAY_FILE },
   .gc_num_roots   = 1
};

struct _sdf_node {
   object_t object;
};

static inline sdf_node_t sdf_array_nth(item_t *item, unsigned n)
{
   object_t *o = obj_array_nth(item->obj_array, n);
   return container_of(o, struct _sdf_node, object);
}

static inline void sdf_array_add(item_t *item, sdf_node_t s)
{
   obj_array_add(&(item->obj_array), &(s->object));
}

sdf_node_t sdf_new(sdf_kind_t kind)
{
   object_t *o = object_new(NULL, &sdf_object, kind);
   return container_of(o, struct _sdf_node, object);
}

sdf_kind_t sdf_kind(sdf_node_t s)
{
   return s->object.kind;
}

const char *sdf_kind_str(sdf_kind_t kind)
{
   return kind_text_map[kind];
}

const loc_t *sdf_loc(sdf_node_t s)
{
   assert(s != NULL);
   return &(s->object.loc);
}

void sdf_set_loc(sdf_node_t s, const loc_t *loc)
{
   assert(s != NULL);
   assert(loc != NULL);
   s->object.loc = *loc;
}

void sdf_add_decl(sdf_node_t s, sdf_node_t decl)
{
   assert(decl != NULL);
   sdf_array_add(lookup_item(&sdf_object, s, I_DECLS), decl);
   object_write_barrier(&(s->object), &(decl->object));
}

unsigned sdf_decls(sdf_node_t s)
{
   item_t *item = lookup_item(&sdf_object, s, I_DECLS);
   return obj_array_count(item->obj_array);
}

sdf_node_t sdf_decl(sdf_node_t s, unsigned int n)
{
   item_t *item = lookup_item(&sdf_object, s, I_DECLS);
   return sdf_array_nth(item, n);
}

void sdf_add_cell(sdf_node_t s, sdf_node_t cell)
{
   assert(cell != NULL);
   assert(sdf_kind(s) == S_DELAY_FILE);

   sdf_array_add(lookup_item(&sdf_object, s, I_STMTS), cell);
   object_write_barrier(&(s->object), &(cell->object));
}

unsigned sdf_cells(sdf_node_t s)
{
   assert(sdf_kind(s) == S_DELAY_FILE);

   item_t *item = lookup_item(&sdf_object, s, I_STMTS);
   return obj_array_count(item->obj_array);
}

sdf_node_t sdf_cell(sdf_node_t s, unsigned int n)
{
   assert(sdf_kind(s) == S_DELAY_FILE);

   item_t *item = lookup_item(&sdf_object, s, I_STMTS);
   return sdf_array_nth(item, n);
}

void sdf_add_signal(sdf_node_t s, sdf_node_t port)
{
   sdf_array_add(lookup_item(&sdf_object, s, I_PORTS), port);
   object_write_barrier(&(s->object), &(port->object));
}

unsigned sdf_signals(sdf_node_t s)
{
   item_t *item = lookup_item(&sdf_object, s, I_PORTS);
   return obj_array_count(item->obj_array);
}

sdf_node_t sdf_signal(sdf_node_t s, unsigned int n)
{
   item_t *item = lookup_item(&sdf_object, s, I_PORTS);
   return sdf_array_nth(item, n);
}

void sdf_add_value(sdf_node_t s, sdf_node_t literal)
{
   assert(literal != NULL);
   sdf_array_add(lookup_item(&sdf_object, s, I_LITERALS), literal);
   object_write_barrier(&(s->object), &(literal->object));
}

unsigned sdf_values(sdf_node_t s)
{
   item_t *item = lookup_item(&sdf_object, s, I_LITERALS);
   return obj_array_count(item->obj_array);
}

sdf_node_t sdf_value(sdf_node_t s, unsigned int n)
{
   item_t *item = lookup_item(&sdf_object, s, I_LITERALS);
   return sdf_array_nth(item, n);
}

void sdf_set_subkind(sdf_node_t s, unsigned int sub)
{
   lookup_item(&sdf_object, s, I_SUBKIND)->ival = sub;
}

unsigned sdf_subkind(sdf_node_t s)
{
   return lookup_item(&sdf_object, s, I_SUBKIND)->ival;
}

void sdf_set_flag(sdf_node_t s, sdf_flags_t mask)
{
   lookup_item(&sdf_object, s, I_FLAGS)->ival |= mask;
}

sdf_flags_t sdf_flags(sdf_node_t s)
{
   return lookup_item(&sdf_object, s, I_FLAGS)->ival;
}

ident_t sdf_ident(sdf_node_t s)
{
   item_t *item = lookup_item(&sdf_object, s, I_IDENT);
   assert(item->ident != NULL);
   return item->ident;
}

void sdf_set_ident(sdf_node_t s, ident_t i)
{
   lookup_item(&sdf_object, s, I_IDENT)->ident = i;
}

bool sdf_has_ident(sdf_node_t s)
{
   return lookup_item(&sdf_object, s, I_IDENT)->ident != NULL;
}

ident_t sdf_ident2(sdf_node_t s)
{
   item_t *item = lookup_item(&sdf_object, s, I_IDENT2);
   assert(item->ident != NULL);
   return item->ident;
}

void sdf_set_ident2(sdf_node_t s, ident_t i)
{
   lookup_item(&sdf_object, s, I_IDENT2)->ident = i;
}

bool sdf_has_ident2(sdf_node_t s)
{
   return lookup_item(&sdf_object, s, I_IDENT2)->ident != NULL;
}

void sdf_set_number(sdf_node_t s, sdf_node_t v)
{
   lookup_item(&sdf_object, s, I_NUMBER)->object = &(v->object);
   object_write_barrier(&(s->object), &(v->object));
}

sdf_node_t sdf_number(sdf_node_t s)
{
   item_t *item = lookup_item(&sdf_object, s, I_NUMBER);
   assert(item->object != NULL);
   return container_of(item->object, struct _sdf_node, object);
}

bool sdf_has_number(sdf_node_t s)
{
   return lookup_item(&sdf_object, s, I_NUMBER)->object != NULL;
}

void sdf_set_ival(sdf_node_t s, int64_t i)
{
   lookup_item(&sdf_object, s, I_IVAL)->ival = i;
}

int64_t sdf_ival(sdf_node_t s)
{
   return lookup_item(&sdf_object, s, I_IVAL)->ival;
}

bool sdf_has_ival(sdf_node_t s)
{
   return lookup_item(&sdf_object, s, I_IVAL)->object != NULL;
}

void sdf_set_dval(sdf_node_t s, double d)
{
   lookup_item(&sdf_object, s, I_DVAL)->dval = d;
}

double sdf_dval(sdf_node_t s)
{
   return lookup_item(&sdf_object, s, I_DVAL)->dval;
}

void sdf_set_min(sdf_node_t s, sdf_node_t min)
{
   lookup_item(&sdf_object, s, I_LEFT)->object = &(min->object);
   object_write_barrier(&(s->object), &(min->object));
}

void sdf_set_typ(sdf_node_t s, sdf_node_t typ)
{
   lookup_item(&sdf_object, s, I_BASE)->object = &(typ->object);
   object_write_barrier(&(s->object), &(typ->object));
}

void sdf_set_max(sdf_node_t s, sdf_node_t max)
{
   lookup_item(&sdf_object, s, I_RIGHT)->object = &(max->object);
   object_write_barrier(&(s->object), &(max->object));
}

sdf_node_t sdf_min(sdf_node_t s)
{
   item_t *item = lookup_item(&sdf_object, s, I_LEFT);
   assert(item->object != NULL);
   return container_of(item->object, struct _sdf_node, object);
}

sdf_node_t sdf_typ(sdf_node_t s)
{
   item_t *item = lookup_item(&sdf_object, s, I_BASE);
   assert(item->object != NULL);
   return container_of(item->object, struct _sdf_node, object);
}

sdf_node_t sdf_max(sdf_node_t s)
{
   item_t *item = lookup_item(&sdf_object, s, I_RIGHT);
   assert(item->object != NULL);
   return container_of(item->object, struct _sdf_node, object);
}

bool sdf_has_min(sdf_node_t s)
{
   return lookup_item(&sdf_object, s, I_LEFT)->object != NULL;
}

bool sdf_has_typ(sdf_node_t s)
{
   return lookup_item(&sdf_object, s, I_BASE)->object != NULL;
}

bool sdf_has_max(sdf_node_t s)
{
   return lookup_item(&sdf_object, s, I_RIGHT)->object != NULL;
}

sdf_node_t sdf_unit(sdf_node_t s)
{
   item_t *item = lookup_item(&sdf_object, s, I_UNITS);
   assert(item->object != NULL);
   return container_of(item->object, struct _sdf_node, object);
}

void sdf_set_unit(sdf_node_t s, sdf_node_t u)
{
   lookup_item(&sdf_object, s, I_UNITS)->object = &(u->object);
   object_write_barrier(&(s->object), &(u->object));
}

void sdf_add_delay(sdf_node_t s, sdf_node_t d)
{
   assert(d != NULL);
   assert(sdf_kind(s) == S_CELL);

   sdf_array_add(lookup_item(&sdf_object, s, I_WAVES), d);
   object_write_barrier(&(s->object), &(d->object));
}

unsigned sdf_delays(sdf_node_t s)
{
   assert(sdf_kind(s) == S_CELL);

   item_t *item = lookup_item(&sdf_object, s, I_WAVES);
   return obj_array_count(item->obj_array);
}

sdf_node_t sdf_delay(sdf_node_t s, unsigned int n)
{
   assert(sdf_kind(s) == S_CELL);

   item_t *item = lookup_item(&sdf_object, s, I_WAVES);
   return sdf_array_nth(item, n);
}

void sdf_add_tcheck(sdf_node_t s, sdf_node_t tcheck)
{
   assert(tcheck != NULL);
   assert(sdf_kind(s) == S_CELL);

   sdf_array_add(lookup_item(&sdf_object, s, I_TRIGGERS), tcheck);
   object_write_barrier(&(s->object), &(tcheck->object));
}

unsigned sdf_tchecks(sdf_node_t s)
{
   assert(sdf_kind(s) == S_CELL);

   item_t *item = lookup_item(&sdf_object, s, I_TRIGGERS);
   return obj_array_count(item->obj_array);
}

sdf_node_t sdf_tcheck(sdf_node_t s, unsigned int n)
{
   assert(sdf_kind(s) == S_CELL);

   item_t *item = lookup_item(&sdf_object, s, I_TRIGGERS);
   return sdf_array_nth(item, n);
}

void sdf_add_label(sdf_node_t s, sdf_node_t label)
{
   assert(label != NULL);
   assert(sdf_kind(s) == S_CELL);

   sdf_array_add(lookup_item(&sdf_object, s, I_PARAMS), label);
   object_write_barrier(&(s->object), &(label->object));
}

unsigned sdf_labels(sdf_node_t s)
{
   assert(sdf_kind(s) == S_CELL);

   item_t *item = lookup_item(&sdf_object, s, I_PARAMS);
   return obj_array_count(item->obj_array);
}

sdf_node_t sdf_label(sdf_node_t s, unsigned int n)
{
   assert(sdf_kind(s) == S_CELL);

   item_t *item = lookup_item(&sdf_object, s, I_PARAMS);
   return sdf_array_nth(item, n);
}

void sdf_add_dim(sdf_node_t s, sdf_node_t dim)
{
   assert(sdf_kind(s) == S_SIGNAL);

   sdf_array_add(lookup_item(&sdf_object, s, I_DIMS), dim);
   object_write_barrier(&(s->object), &(dim->object));
}

unsigned sdf_dims(sdf_node_t s)
{
   item_t *item = lookup_item(&sdf_object, s, I_DIMS);
   return obj_array_count(item->obj_array);
}

sdf_node_t sdf_dim(sdf_node_t s, unsigned n)
{
   item_t *item = lookup_item(&sdf_object, s, I_DIMS);
   return sdf_array_nth(item, n);
}

void sdf_add_cond(sdf_node_t s, sdf_node_t c)
{
   assert(sdf_kind(c) == S_COND);

   sdf_array_add(lookup_item(&sdf_object, s, I_CONDS), c);
   object_write_barrier(&(s->object), &(c->object));
}

unsigned sdf_conds(sdf_node_t s)
{
   item_t *item = lookup_item(&sdf_object, s, I_CONDS);
   return obj_array_count(item->obj_array);
}

sdf_node_t sdf_cond(sdf_node_t s, unsigned int n)
{
   item_t *item = lookup_item(&sdf_object, s, I_CONDS);
   return sdf_array_nth(item, n);
}

void sdf_set_expr(sdf_node_t s, sdf_node_t e)
{
   lookup_item(&sdf_object, s, I_VALUE)->object = &(e->object);
   object_write_barrier(&(s->object), &(e->object));
}

bool sdf_has_expr(sdf_node_t s)
{
   return lookup_item(&sdf_object, s, I_VALUE)->object != NULL;
}

sdf_node_t sdf_expr(sdf_node_t s)
{
   item_t *item = lookup_item(&sdf_object, s, I_VALUE);
   assert(item->object != NULL);
   return container_of(item->object, struct _sdf_node, object);
}

void sdf_add_tenv(sdf_node_t s, sdf_node_t c)
{
   assert(sdf_kind(s) == S_CELL);
   assert(sdf_kind(c) == S_CONSTRAINT || sdf_kind(c) == S_TIMING_ENV);

   sdf_array_add(lookup_item(&sdf_object, s, I_CONSTR), c);
   object_write_barrier(&(s->object), &(c->object));
}

unsigned sdf_tenvs(sdf_node_t s)
{
   item_t *item = lookup_item(&sdf_object, s, I_CONSTR);
   return obj_array_count(item->obj_array);
}

sdf_node_t sdf_tenv(sdf_node_t s, unsigned int n)
{
   item_t *item = lookup_item(&sdf_object, s, I_CONSTR);
   return sdf_array_nth(item, n);
}

void sdf_add_exception(sdf_node_t s, sdf_node_t e)
{
   assert(sdf_kind(s) == S_CONSTRAINT);
   assert(sdf_kind(e) == S_EXCEPTION);

   sdf_array_add(lookup_item(&sdf_object, s, I_CONTEXT), e);
   object_write_barrier(&(s->object), &(e->object));
}

sdf_node_t sdf_exception(sdf_node_t s, unsigned int n)
{
   item_t *item = lookup_item(&sdf_object, s, I_CONTEXT);
   return sdf_array_nth(item, n);
}

unsigned sdf_exceptions(sdf_node_t s)
{
   item_t *item = lookup_item(&sdf_object, s, I_CONTEXT);
   return obj_array_count(item->obj_array);
}

object_t *sdf_to_object(sdf_node_t s)
{
   return &(s->object);
}

sdf_node_t sdf_from_object(object_t *obj)
{
   if (obj != NULL && obj->tag == OBJECT_TAG_SDF)
      return container_of(obj, struct _sdf_node, object);
   else
      return NULL;
}