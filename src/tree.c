//
//  Copyright (C) 2011  Nick Gasson
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

#include "tree.h"
#include "util.h"

#include <assert.h>

#define MAX_CONTEXTS 16

struct tree_array {
   size_t count;
   size_t max;
   tree_t *items;
};

struct tree {
   tree_kind_t       kind;
   loc_t             loc;
   ident_t           ident;
   ident_t           ident2;
   struct tree_array ports;
   struct tree_array generics;
   struct tree_array params;
   struct tree_array decls;
   struct tree_array stmts;
   port_mode_t       port_mode;
   type_t            type;
   literal_t         literal;
   tree_t            value;
   tree_t            delay;
   tree_t            target;
   tree_t            ref;
   ident_t           *context;
   unsigned          n_contexts;

   // Serialisation bookkeeping
   unsigned          generation;
   unsigned          index;
};

#define IS(t, k) ((t)->kind == (k))
#define IS_DECL(t) \
   (IS(t, T_PORT_DECL) || IS(t, T_SIGNAL_DECL) || IS(t, T_VAR_DECL) \
    || IS(t, T_TYPE_DECL))
#define IS_EXPR(t) \
   (IS(t, T_FCALL) || IS(t, T_LITERAL) || IS(t, T_REF) || IS(t, T_QUALIFIED))
#define IS_STMT(t) \
   (IS(t, T_PROCESS) || IS(t, T_WAIT) || IS(t, T_VAR_ASSIGN) \
    || IS(t, T_SIGNAL_ASSIGN))
#define HAS_IDENT(t) \
   (IS(t, T_ENTITY) || IS(t, T_PORT_DECL) || IS(t, T_FCALL) || IS(t, T_ARCH) \
    || IS(t, T_SIGNAL_DECL) || IS(t, T_PROCESS) || IS(t, T_VAR_DECL)    \
    || IS(t, T_REF) || IS(t, T_TYPE_DECL) || IS(t, T_PACKAGE)           \
    || IS(t, T_QUALIFIED) || IS(t, T_ENUM_LIT))
#define HAS_IDENT2(t) (IS(t, T_ARCH))
#define HAS_PORTS(t) (IS(t, T_ENTITY))
#define HAS_GENERICS(t) (IS(t, T_ENTITY))
#define HAS_TYPE(t) \
   (IS(t, T_PORT_DECL) || IS(t, T_SIGNAL_DECL) || IS(t, T_VAR_DECL) \
    || IS(t, T_TYPE_DECL) || IS_EXPR(t) || IS(t, T_ENUM_LIT))
#define HAS_PARAMS(t) (IS(t, T_FCALL))
#define HAS_DECLS(t) (IS(t, T_ARCH) || IS(t, T_PROCESS) || IS(t, T_PACKAGE))
#define HAS_STMTS(t) (IS(t, T_ARCH) || IS(t, T_PROCESS))
#define HAS_DELAY(t) (IS(t, T_WAIT))
#define HAS_TARGET(t) (IS(t, T_VAR_ASSIGN) || IS(t, T_SIGNAL_ASSIGN))
#define HAS_VALUE(t) \
   (IS_DECL(t) || IS(t, T_VAR_ASSIGN) || IS(t, T_SIGNAL_ASSIGN) \
    || IS(t, T_QUALIFIED))
#define HAS_CONTEXT(t) (IS(t, T_ARCH) || IS(t, T_ENTITY) || IS(t, T_PACKAGE))

#define TREE_ARRAY_BASE_SZ  16

static void tree_number(tree_t t, unsigned *n_tree,
                        unsigned *n_type, unsigned generation);

static void tree_array_init(struct tree_array *a)
{
   a->count = 0;
   a->max   = 0;
   a->items = NULL;
}

static void tree_array_add(struct tree_array *a, tree_t t)
{
   if (a->max == 0) {
      a->items = xmalloc(sizeof(tree_t) * TREE_ARRAY_BASE_SZ);
      a->max   = TREE_ARRAY_BASE_SZ;
   }
   else if (a->count == a->max) {
      a->max *= 2;
      a->items = xrealloc(a->items, sizeof(tree_t) * a->max);
   }

   a->items[a->count++] = t;
}

static inline tree_t tree_array_nth(struct tree_array *a, unsigned n)
{
   assert(n < a->count);
   return a->items[n];
}

tree_t tree_new(tree_kind_t kind)
{
   tree_t t = xmalloc(sizeof(struct tree));
   t->kind       = kind;
   t->ident      = NULL;
   t->ident2     = NULL;
   t->port_mode  = PORT_INVALID;
   t->value      = NULL;
   t->target     = NULL;
   t->loc        = LOC_INVALID;
   t->ref        = NULL;
   t->context    = NULL;
   t->n_contexts = 0;
   t->generation = 0;
   t->index      = 0;
   
   tree_array_init(&t->ports);
   tree_array_init(&t->generics);
   tree_array_init(&t->params);
   tree_array_init(&t->decls);
   tree_array_init(&t->stmts);
   
   t->literal.kind = L_INT;
   t->literal.i    = 0;
   
   return t;
}

const loc_t *tree_loc(tree_t t)
{
   assert(t != NULL);

   return &t->loc;
}

void tree_set_loc(tree_t t, const loc_t *loc)
{
   assert(t != NULL);
   assert(loc != NULL);

   t->loc = *loc;
}

ident_t tree_ident(tree_t t)
{
   assert(t != NULL);
   assert(HAS_IDENT(t));
   assert(t->ident != NULL);

   return t->ident;
}

void tree_set_ident(tree_t t, ident_t i)
{
   assert(t != NULL);
   assert(i != NULL);
   assert(HAS_IDENT(t));

   t->ident = i;
}

ident_t tree_ident2(tree_t t)
{
   assert(t != NULL);
   assert(HAS_IDENT2(t));
   assert(t->ident2 != NULL);

   return t->ident2;
}

void tree_set_ident2(tree_t t, ident_t i)
{
   assert(t != NULL);
   assert(i != NULL);
   assert(HAS_IDENT2(t));

   t->ident2 = i;
}

tree_kind_t tree_kind(tree_t t)
{
   assert(t != NULL);
   return t->kind;
}

unsigned tree_ports(tree_t t)
{
   assert(t != NULL);
   assert(HAS_PORTS(t));

   return t->ports.count;
}

tree_t tree_port(tree_t t, unsigned n)
{
   assert(t != NULL);
   assert(HAS_PORTS(t));

   return tree_array_nth(&t->ports, n);
}

void tree_add_port(tree_t t, tree_t d)
{
   assert(t != NULL);
   assert(d != NULL);
   assert(HAS_PORTS(t));
   assert(IS_DECL(d));

   tree_array_add(&t->ports, d);
}

port_mode_t tree_port_mode(tree_t t)
{
   assert(t != NULL);
   assert(IS(t, T_PORT_DECL));
   assert(t->port_mode != PORT_INVALID);

   return t->port_mode;
}

void tree_set_port_mode(tree_t t, port_mode_t mode)
{
   assert(t != NULL);
   assert(IS(t, T_PORT_DECL));

   t->port_mode = mode;
}

unsigned tree_generics(tree_t t)
{
   assert(t != NULL);
   assert(HAS_GENERICS(t));

   return t->generics.count;
}

tree_t tree_generic(tree_t t, unsigned n)
{
   assert(t != NULL);
   assert(HAS_GENERICS(t));

   return tree_array_nth(&t->generics, n);   
}

void tree_add_generic(tree_t t, tree_t d)
{
   assert(t != NULL);
   assert(d != NULL);
   assert(HAS_GENERICS(t));
   assert(IS_DECL(d));

   tree_array_add(&t->generics, d);
}

type_t tree_type(tree_t t)
{
   assert(t != NULL);
   assert(HAS_TYPE(t));
   assert(t->type != NULL);

   return t->type;
}

void tree_set_type(tree_t t, type_t ty)
{
   assert(t != NULL);
   assert(HAS_TYPE(t));

   t->type = ty;
}

unsigned tree_params(tree_t t)
{
   assert(t != NULL);
   assert(HAS_PARAMS(t));

   return t->params.count;
}

tree_t tree_param(tree_t t, unsigned n)
{
   assert(t != NULL);
   assert(HAS_PARAMS(t));
   
   return tree_array_nth(&t->params, n);
}

void tree_add_param(tree_t t, tree_t e)
{
   assert(t != NULL);
   assert(e != NULL);
   assert(HAS_PARAMS(t));
   assert(IS_EXPR(e));

   tree_array_add(&t->params, e);
}

void tree_set_literal(tree_t t, literal_t lit)
{
   assert(t != NULL);
   assert(IS(t, T_LITERAL));

   t->literal = lit;
}

literal_t tree_literal(tree_t t)
{
   assert(t != NULL);
   assert(IS(t, T_LITERAL));

   return t->literal;
}

bool tree_has_value(tree_t t)
{
   assert(t != NULL);
   assert(HAS_VALUE(t));

   return t->value != NULL;
}

tree_t tree_value(tree_t t)
{
   assert(t != NULL);
   assert(HAS_VALUE(t));
   assert(t->value != NULL);

   return t->value;
}

void tree_set_value(tree_t t, tree_t v)
{
   assert(t != NULL);
   assert(HAS_VALUE(t));
   assert(v == NULL || IS_EXPR(v));

   t->value = v;
}

unsigned tree_decls(tree_t t)
{
   assert(t != NULL);
   assert(HAS_DECLS(t));

   return t->decls.count;
}

tree_t tree_decl(tree_t t, unsigned n)
{
   assert(t != NULL);
   assert(HAS_DECLS(t));

   return tree_array_nth(&t->decls, n);   
}

void tree_add_decl(tree_t t, tree_t d)
{
   assert(t != NULL);
   assert(d != NULL);
   assert(HAS_DECLS(t));
   assert(IS_DECL(d));
   
   tree_array_add(&t->decls, d);
}

unsigned tree_stmts(tree_t t)
{
   assert(t != NULL);
   assert(HAS_STMTS(t));

   return t->stmts.count;
}

tree_t tree_stmt(tree_t t, unsigned n)
{
   assert(t != NULL);
   assert(HAS_STMTS(t));

   return tree_array_nth(&t->stmts, n);
}

void tree_add_stmt(tree_t t, tree_t s)
{
   assert(t != NULL);
   assert(s != NULL);
   assert(HAS_STMTS(t));
   assert(IS_STMT(s));
   
   tree_array_add(&t->stmts, s);
}

bool tree_has_delay(tree_t t)
{
   assert(t != NULL);
   assert(HAS_DELAY(t));

   return t->delay != NULL;
}

tree_t tree_delay(tree_t t)
{
   assert(t != NULL);
   assert(HAS_DELAY(t));
   assert(t->delay != NULL);

   return t->delay;
}

void tree_set_delay(tree_t t, tree_t d)
{
   assert(t != NULL);
   assert(d != NULL);
   assert(HAS_DELAY(t));
   assert(IS_EXPR(d));

   t->delay = d;
}

tree_t tree_target(tree_t t)
{
   assert(t != NULL);
   assert(HAS_TARGET(t));
   assert(t->target != NULL);

   return t->target;
}

void tree_set_target(tree_t t, tree_t lhs)
{
   assert(t != NULL);
   assert(HAS_TARGET(t));

   t->target = lhs;
}

tree_t tree_ref(tree_t t)
{
   assert(t != NULL);
   assert(IS(t, T_REF));
   assert(t->ref != NULL);

   return t->ref;
}

void tree_set_ref(tree_t t, tree_t decl)
{
   assert(t != NULL);
   assert(IS(t, T_REF));
   assert(IS_DECL(decl) || IS(decl, T_ENUM_LIT));

   t->ref = decl;
}

unsigned tree_contexts(tree_t t)
{
   assert(t != NULL);
   assert(HAS_CONTEXT(t));

   return t->n_contexts;
}

ident_t tree_context(tree_t t, unsigned n)
{
   assert(t != NULL);
   assert(HAS_CONTEXT(t));
   assert(n < t->n_contexts);

   return t->context[n];
}

void tree_add_context(tree_t t, ident_t ctx)
{
   assert(t != NULL);
   assert(HAS_CONTEXT(t));
   assert(t->n_contexts < MAX_CONTEXTS);

   if (t->n_contexts == 0)
      t->context = xmalloc(sizeof(ident_t) * MAX_CONTEXTS);

   t->context[t->n_contexts++] = ctx;
}

static void write_loc(loc_t *l, FILE *f)
{
   write_s(l->first_line, f);
   write_s(l->first_column, f);
   write_s(l->last_line, f);
   write_s(l->last_column, f);
}

static loc_t read_loc(FILE *f)
{
   loc_t l = { .file = "none", .linebuf = NULL };
   l.first_line   = read_s(f);
   l.first_column = read_s(f);
   l.last_line    = read_s(f);
   l.last_column  = read_s(f);
   return l;
}

static void write_a(struct tree_array *a, FILE *f)
{
   write_u(a->count, f);
   for (unsigned i = 0; i < a->count; i++)
      tree_write(a->items[i], f);
}

static void read_a(struct tree_array *a, FILE *f)
{
   a->count = a->max = read_u(f);
   a->items = xmalloc(a->count * sizeof(tree_t));
   for (unsigned i = 0; i < a->count; i++)
      a->items[i] = tree_read(f);
}

static void array_number(struct tree_array *a, unsigned *n_tree,
                         unsigned *n_type, unsigned generation)
{
   for (unsigned n = 0; n < a->count; n++)
      tree_number(a->items[n], n_tree, n_type, generation);
}

static void tree_number(tree_t t, unsigned *n_tree, unsigned *n_type,
                        unsigned generation)
{
   if (t == NULL)
      return;
   
   if (t->generation == generation) {
      printf("already visited %p\n", t);
      return;    // Already visited this tree
   }

   t->generation = generation;
   t->index      = (*n_tree)++;

   if (HAS_PORTS(t))
      array_number(&t->ports, n_tree, n_type, generation);
   if (HAS_GENERICS(t))
      array_number(&t->generics, n_tree, n_type, generation);
   if (HAS_PARAMS(t))
      array_number(&t->params, n_tree, n_type, generation);
   if (HAS_DECLS(t))
      array_number(&t->decls, n_tree, n_type, generation);
   if (HAS_STMTS(t))
      array_number(&t->stmts, n_tree, n_type, generation);
   if (HAS_VALUE(t))
      tree_number(t->value, n_tree, n_type, generation);
   if (HAS_DELAY(t))
      tree_number(t->delay, n_tree, n_type, generation);
   if (HAS_TARGET(t))
      tree_number(t->target, n_tree, n_type, generation);
   if (IS(t, T_REF))
      tree_number(t->ref, n_tree, n_type, generation);
}

void tree_write(tree_t t, FILE *f)
{
   static unsigned next_generation = 1;
   unsigned n_trees = 0;
   unsigned n_types = 0;
   tree_number(t, &n_trees, &n_types, next_generation++);

   printf("%d trees\n", n_trees);
   
#if 0
   assert(t != NULL);

   write_s(t->kind, f);
   write_loc(&t->loc, f);
   if (HAS_IDENT(t))
      ident_write(t->ident, f);
   if (HAS_IDENT2(t))
      ident_write(t->ident2, f);
   if (HAS_PORTS(t))
      write_a(&t->ports, f);
   if (HAS_GENERICS(t))
      write_a(&t->generics, f);
   if (HAS_PARAMS(t))
      write_a(&t->params, f);
   if (HAS_DECLS(t))
      write_a(&t->decls, f);
   if (HAS_STMTS(t))
      write_a(&t->stmts, f);
   if (IS(t, T_PORT_DECL))
      write_s(t->port_mode, f);
   if (HAS_TYPE(t) && !IS(t, T_ENUM_LIT))
      type_write(t->type, f);
   if (IS(t, T_LITERAL))
      fwrite(&t->literal, sizeof(literal_t), 1, f);
   if (HAS_VALUE(t) && t->value != NULL)
      tree_write(t->value, f);
   if (HAS_DELAY(t))
      tree_write(t->delay, f);
   if (HAS_TARGET(t))
      tree_write(t->target, f);
   if (IS(t, T_REF))
      tree_write(t->ref, f);
   if (HAS_CONTEXT(t)) {
      write_s(t->n_contexts, f);
      for (unsigned i = 0; i < t->n_contexts; i++)
         ident_write(t->context[i], f);
   }
#endif
}

tree_t tree_read(FILE *f)
{
   tree_t t = tree_new(read_s(f));
   t->loc = read_loc(f);
   if (HAS_IDENT(t))
      tree_set_ident(t, ident_read(f));
   if (HAS_IDENT2(t))
      tree_set_ident2(t, ident_read(f));
   if (HAS_PORTS(t))
      read_a(&t->ports, f);
   if (HAS_GENERICS(t))
      read_a(&t->generics, f);
   if (HAS_PARAMS(t))
      read_a(&t->params, f);
   if (HAS_DECLS(t))
      read_a(&t->decls, f);
   if (HAS_STMTS(t))
      read_a(&t->stmts, f);
   if (IS(t, T_PORT_DECL))
      t->port_mode = read_s(f);
   
   return t;
}
