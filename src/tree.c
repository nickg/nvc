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
#include <stdlib.h>

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
   unsigned short    n_contexts;

   // Serialisation and GC bookkeeping
   unsigned short    generation;
   unsigned          index;
};

struct tree_wr_ctx {
   FILE          *file;
   type_wr_ctx_t type_ctx;
   unsigned      generation;
   unsigned      n_trees;
};

struct tree_rd_ctx {
   FILE          *file;
   type_rd_ctx_t type_ctx;
   unsigned      n_trees;
   tree_t        *store;
   unsigned      store_sz;
};

#define IS(t, k) ((t)->kind == (k))
#define IS_TOP_LEVEL(t) (IS(t, T_ARCH) || IS(t, T_ENTITY) || IS(t, T_PACKAGE))
#define IS_DECL(t) \
   (IS(t, T_PORT_DECL) || IS(t, T_SIGNAL_DECL) || IS(t, T_VAR_DECL) \
    || IS(t, T_TYPE_DECL) || IS(t, T_CONST_DECL) || IS(t, T_FUNC_DECL))
#define IS_EXPR(t) \
   (IS(t, T_FCALL) || IS(t, T_LITERAL) || IS(t, T_REF) || IS(t, T_QUALIFIED))
#define IS_STMT(t) \
   (IS(t, T_PROCESS) || IS(t, T_WAIT) || IS(t, T_VAR_ASSIGN) \
    || IS(t, T_SIGNAL_ASSIGN))
#define HAS_IDENT(t) \
   (IS(t, T_ENTITY) || IS(t, T_PORT_DECL) || IS(t, T_FCALL) || IS(t, T_ARCH) \
    || IS(t, T_SIGNAL_DECL) || IS(t, T_PROCESS) || IS(t, T_VAR_DECL)    \
    || IS(t, T_REF) || IS(t, T_TYPE_DECL) || IS(t, T_PACKAGE)           \
    || IS(t, T_QUALIFIED) || IS(t, T_ENUM_LIT) || IS(t, T_CONST_DECL)   \
    || IS(t, T_FUNC_DECL))
#define HAS_IDENT2(t) (IS(t, T_ARCH))
#define HAS_PORTS(t) (IS(t, T_ENTITY) || IS(t, T_FUNC_DECL))
#define HAS_GENERICS(t) (IS(t, T_ENTITY))
#define HAS_TYPE(t) \
   (IS(t, T_PORT_DECL) || IS(t, T_SIGNAL_DECL) || IS(t, T_VAR_DECL) \
    || IS(t, T_TYPE_DECL) || IS_EXPR(t) || IS(t, T_ENUM_LIT) \
    || IS(t, T_CONST_DECL) || IS(t, T_FUNC_DECL))
#define HAS_PARAMS(t) (IS(t, T_FCALL))
#define HAS_DECLS(t) (IS(t, T_ARCH) || IS(t, T_PROCESS) || IS(t, T_PACKAGE))
#define HAS_STMTS(t) (IS(t, T_ARCH) || IS(t, T_PROCESS))
#define HAS_DELAY(t) (IS(t, T_WAIT))
#define HAS_TARGET(t) (IS(t, T_VAR_ASSIGN) || IS(t, T_SIGNAL_ASSIGN))
#define HAS_VALUE(t) \
   (IS_DECL(t) || IS(t, T_VAR_ASSIGN) || IS(t, T_SIGNAL_ASSIGN) \
    || IS(t, T_QUALIFIED) || IS(t, T_CONST_DECL))
#define HAS_CONTEXT(t) (IS(t, T_ARCH) || IS(t, T_ENTITY) || IS(t, T_PACKAGE))

#define TREE_ARRAY_BASE_SZ  16

// Garbage collection
static tree_t   *all_trees = NULL;
static size_t   max_trees = 128;   // Grows at runtime
static size_t   n_trees_alloc = 0;
static unsigned next_generation = 1;

static unsigned tree_visit_aux(tree_t t, tree_visit_fn_t fn, void *context,
                               unsigned generation);

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
   t->type       = NULL;
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

   if (all_trees == NULL)
      all_trees = xmalloc(sizeof(tree_t) * max_trees);
   else if (n_trees_alloc == max_trees) {
      max_trees *= 2;
      all_trees = xrealloc(all_trees, sizeof(tree_t) * max_trees);
   }
   all_trees[n_trees_alloc++] = t;      
   
   return t;
}

void tree_gc(void)
{
   // Generation will be updated by tree_visit
   const unsigned base_gen = next_generation;

   // Mark
   for (unsigned i = 0; i < n_trees_alloc; i++) {
      assert(all_trees[i] != NULL);
      
      if (IS_TOP_LEVEL(all_trees[i]))
         tree_visit(all_trees[i], NULL, NULL);
   }

   // Sweep
   for (unsigned i = 0; i < n_trees_alloc; i++) {
      tree_t t = all_trees[i];
      if (t->generation < base_gen) {
         if (HAS_TYPE(t) && t->type != NULL)
            type_unref(t->type);

         if (HAS_PORTS(t) && t->ports.items != NULL)
            free(t->ports.items);
         if (HAS_GENERICS(t) && t->generics.items != NULL)
            free(t->generics.items);
         if (HAS_PARAMS(t) && t->params.items != NULL)
            free(t->params.items);
         if (HAS_DECLS(t) && t->decls.items != NULL)
            free(t->decls.items);
         if (HAS_STMTS(t) && t->stmts.items != NULL)
            free(t->stmts.items);

         if (HAS_CONTEXT(t) && t->context != NULL)
            free(t->context);
         
         free(t);

         all_trees[i] = NULL;
      }
   }

   // Compact
   size_t p = 0;
   for (unsigned i = 0; i < n_trees_alloc; i++) {
      if (all_trees[i] != NULL)
         all_trees[p++] = all_trees[i];
   }

   printf("[gc: freed %lu trees; %lu allocated]\n",
          n_trees_alloc - p, p);

   n_trees_alloc = p;
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

   type_ref(ty);
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

static unsigned tree_visit_a(struct tree_array *a,
                             tree_visit_fn_t fn, void *context,
                             unsigned generation)
{
   unsigned n = 0;
   for (unsigned i = 0; i < a->count; i++)
      n += tree_visit_aux(a->items[i], fn, context, generation);
   
   return n;
}

static unsigned tree_visit_type(type_t type,
                                tree_visit_fn_t fn, void *context,
                                unsigned generation)
{
   if (type == NULL)
      return 0;
   
   unsigned n = 0;

   switch (type_kind(type)) {
   case T_SUBTYPE:
   case T_INTEGER:
   case T_PHYSICAL:
      for (unsigned i = 0; i < type_dims(type); i++) {
         range_t r = type_dim(type, i);
         n += tree_visit_aux(r.left, fn, context, generation);
         n += tree_visit_aux(r.right, fn, context, generation);
      }
      break;
      
   default:
      break;
   }
      
   switch (type_kind(type)) {
   case T_UNRESOLVED:
      break;

   case T_SUBTYPE:
      n += tree_visit_type(type_base(type), fn, context, generation);
      break;
      
   case T_PHYSICAL:
      for (unsigned i = 0; i < type_units(type); i++)
         n += tree_visit_aux(type_unit(type, i).multiplier, fn, context,
                             generation);
      break;

   case T_FUNC:
      for (unsigned i = 0; i < type_params(type); i++)
         n += tree_visit_type(type_param(type, i), fn, context, generation);
      tree_visit_type(type_result(type), fn, context, generation);
      break;

   case T_ENUM:
      for (unsigned i = 0; i < type_enum_literals(type); i++)
         n += tree_visit_aux(type_enum_literal(type, i), fn, context,
                             generation);
      break;

   default:
      break;
   }

   return n;
}

static unsigned tree_visit_aux(tree_t t, tree_visit_fn_t fn, void *context,
                               unsigned generation)
{
   if (t == NULL || t->generation == generation)
      return 0;

   t->generation = generation;
   
   unsigned n = 1;

   if (HAS_PORTS(t))
      n += tree_visit_a(&t->ports, fn, context, generation);
   if (HAS_GENERICS(t))
      n += tree_visit_a(&t->generics, fn, context, generation);
   if (HAS_PARAMS(t))
      n += tree_visit_a(&t->params, fn, context, generation);
   if (HAS_DECLS(t))
      n += tree_visit_a(&t->decls, fn, context, generation);
   if (HAS_STMTS(t))
      n += tree_visit_a(&t->stmts, fn, context, generation);
   if (HAS_VALUE(t))
      n += tree_visit_aux(t->value, fn, context, generation);
   if (HAS_DELAY(t))
      n += tree_visit_aux(t->delay, fn, context, generation);
   if (HAS_TARGET(t))
      n += tree_visit_aux(t->target, fn, context, generation);
   if (IS(t, T_REF))
      n += tree_visit_aux(t->ref, fn, context, generation);
   if (HAS_TYPE(t))
      n += tree_visit_type(t->type, fn, context, generation);

   if (fn)
      (*fn)(t, context);
   
   return n;
}

unsigned tree_visit(tree_t t, tree_visit_fn_t fn, void *context)
{
   assert(t != NULL);

   return tree_visit_aux(t, fn, context, next_generation++);   
}

static void write_loc(loc_t *l, tree_wr_ctx_t ctx)
{
   write_s(l->first_line, ctx->file);
   write_s(l->first_column, ctx->file);
   write_s(l->last_line, ctx->file);
   write_s(l->last_column, ctx->file);
}

static loc_t read_loc(tree_rd_ctx_t ctx)
{
   loc_t l = { .file = "none", .linebuf = NULL };
   l.first_line   = read_s(ctx->file);
   l.first_column = read_s(ctx->file);
   l.last_line    = read_s(ctx->file);
   l.last_column  = read_s(ctx->file);
   return l;
}

static void write_a(struct tree_array *a, tree_wr_ctx_t ctx)
{
   write_u(a->count, ctx->file);
   for (unsigned i = 0; i < a->count; i++)
      tree_write(a->items[i], ctx);
}

static void read_a(struct tree_array *a, tree_rd_ctx_t ctx)
{
   a->count = a->max = read_u(ctx->file);
   a->items = xmalloc(a->count * sizeof(tree_t));
   for (unsigned i = 0; i < a->count; i++)
      a->items[i] = tree_read(ctx);
}

tree_wr_ctx_t tree_write_begin(FILE *f)
{
   struct tree_wr_ctx *ctx = xmalloc(sizeof(struct tree_wr_ctx));
   ctx->file       = f;
   ctx->generation = next_generation++;
   ctx->n_trees    = 0;
   ctx->type_ctx   = type_write_begin(ctx);
   
   return ctx;
}

void tree_write_end(tree_wr_ctx_t ctx)
{
   type_write_end(ctx->type_ctx);
   free(ctx);
}

FILE *tree_write_file(tree_wr_ctx_t ctx)
{
   return ctx->file;
}

void tree_write(tree_t t, tree_wr_ctx_t ctx)
{
   if (t == NULL) {
      write_s(0xffff, ctx->file);  // Null marker
      return;
   }
   
   if (t->generation == ctx->generation) {
      // Already visited this tree
      write_s(0xfffe, ctx->file);   // Back reference marker
      write_u(t->index, ctx->file);
      return;
   }

   t->generation = ctx->generation;
   t->index      = (ctx->n_trees)++;

   write_s(t->kind, ctx->file);
   write_loc(&t->loc, ctx);
   if (HAS_IDENT(t))
      ident_write(t->ident, ctx->file);
   if (HAS_IDENT2(t))
      ident_write(t->ident2, ctx->file);
   if (HAS_PORTS(t))
      write_a(&t->ports, ctx);
   if (HAS_GENERICS(t))
      write_a(&t->generics, ctx);
   if (HAS_PARAMS(t))
      write_a(&t->params, ctx);
   if (HAS_DECLS(t))
      write_a(&t->decls, ctx);
   if (HAS_STMTS(t))
      write_a(&t->stmts, ctx);
   if (IS(t, T_PORT_DECL))
      write_s(t->port_mode, ctx->file);
   if (HAS_TYPE(t))
      type_write(t->type, ctx->type_ctx);
   if (HAS_VALUE(t))
      tree_write(t->value, ctx);
   if (HAS_DELAY(t))
      tree_write(t->delay, ctx);
   if (HAS_TARGET(t))
      tree_write(t->target, ctx);
   if (IS(t, T_REF))
      tree_write(t->ref, ctx);
   if (HAS_CONTEXT(t)) {
      write_s(t->n_contexts, ctx->file);
      for (unsigned i = 0; i < t->n_contexts; i++)
         ident_write(t->context[i], ctx->file);
   }
}

tree_t tree_read(tree_rd_ctx_t ctx)
{
   unsigned short marker = read_s(ctx->file);
   if (marker == 0xffff)
      return NULL;    // Null marker
   else if (marker == 0xfffe) {
      // Back reference marker
      unsigned index = read_u(ctx->file);
      assert(index < ctx->n_trees);
      return ctx->store[index];
   }

   tree_t t = tree_new((tree_kind_t)marker);
   t->loc = read_loc(ctx);

   // Stash pointer for later back references
   // This must be done early as a child node of this type may
   // reference upwards
   if (ctx->n_trees == ctx->store_sz) {
      ctx->store_sz *= 2;
      ctx->store = xrealloc(ctx->store, ctx->store_sz * sizeof(tree_t));
   }
   ctx->store[ctx->n_trees++] = t;
   
   if (HAS_IDENT(t))
      tree_set_ident(t, ident_read(ctx->file));
   if (HAS_IDENT2(t))
      tree_set_ident2(t, ident_read(ctx->file));
   if (HAS_PORTS(t))
      read_a(&t->ports, ctx);
   if (HAS_GENERICS(t))
      read_a(&t->generics, ctx);
   if (HAS_PARAMS(t))
      read_a(&t->params, ctx);
   if (HAS_DECLS(t))
      read_a(&t->decls, ctx);
   if (HAS_STMTS(t))
      read_a(&t->stmts, ctx);
   if (IS(t, T_PORT_DECL))
      t->port_mode = read_s(ctx->file);
   if (HAS_TYPE(t)) {
      if ((t->type = type_read(ctx->type_ctx)))
         type_ref(t->type);
   }
   if (HAS_VALUE(t))
      t->value = tree_read(ctx);
   if (HAS_DELAY(t))
      t->delay = tree_read(ctx);
   if (HAS_TARGET(t))
      t->target = tree_read(ctx);
   if (IS(t, T_REF))
      t->ref = tree_read(ctx);
   if (HAS_CONTEXT(t)) {
      t->n_contexts = read_s(ctx->file);
      t->context    = xmalloc(sizeof(ident_t) * MAX_CONTEXTS);

      for (unsigned i = 0; i < t->n_contexts; i++)
         t->context[i] = ident_read(ctx->file);
   }
   
   return t;
}

tree_rd_ctx_t tree_read_begin(FILE *f)
{
   struct tree_rd_ctx *ctx = xmalloc(sizeof(struct tree_rd_ctx));
   ctx->file     = f;
   ctx->type_ctx = type_read_begin(ctx);
   ctx->store_sz = 128;
   ctx->store    = xmalloc(ctx->store_sz * sizeof(tree_t));
   ctx->n_trees  = 0;
   
   return ctx;
}

void tree_read_end(tree_rd_ctx_t ctx)
{
   type_read_end(ctx->type_ctx);
   free(ctx->store);
   free(ctx);
}

FILE *tree_read_file(tree_rd_ctx_t ctx)
{
   return ctx->file;
}
