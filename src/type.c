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

#include "type.h"
#include "tree.h"
#include "util.h"

#include <assert.h>
#include <limits.h>
#include <stdlib.h>

#define MAX_DIMS     4
#define MAX_UNITS    16

struct type {
   type_kind_t kind;
   ident_t     ident;
   range_t     dims[MAX_DIMS];
   unsigned    n_dims;
   type_t      base;
   unit_t      *units;
   unsigned    n_units;
   tree_t      *literals;
   unsigned    n_literals;
   size_t      lit_alloc;
   type_t      *params;
   unsigned    n_params;
   size_t      params_alloc;
   type_t      result;

   // Serialisation accounting
   unsigned    generation;
   unsigned    index;
};

struct type_wr_ctx {
   tree_wr_ctx_t tree_ctx;
   unsigned      generation;
   unsigned      n_types;
};

struct type_rd_ctx {
   tree_rd_ctx_t tree_ctx;
   unsigned      n_types;
   type_t        *store;
   unsigned      store_sz;
};

#define IS(t, k) ((t)->kind == (k))
#define HAS_DIMS(t) \
   (IS(t, T_INTEGER) || IS(t, T_SUBTYPE) || IS(t, T_PHYSICAL))

type_t type_new(type_kind_t kind)
{
   struct type *t = xmalloc(sizeof(struct type));
   t->kind       = kind;
   t->ident      = NULL;
   t->n_dims     = 0;
   t->base       = NULL;
   t->units      = NULL;
   t->n_units    = 0;
   t->literals   = NULL;
   t->n_literals = 0;
   t->lit_alloc  = 0;
   t->generation = 0;
   t->params     = NULL;
   t->n_params   = 0;
   t->result     = NULL;
   
   return t;
}

type_kind_t type_kind(type_t t)
{
   assert(t != NULL);

   return t->kind;
}

bool type_eq(type_t a, type_t b)
{
   assert(a != NULL);
   assert(b != NULL);
   
   assert(type_kind(a) != T_UNRESOLVED);
   assert(type_kind(b) != T_UNRESOLVED);
   
   if (a == b)
      return true;

   // Subtypes are convertible to the base type
   while (type_kind(a) == T_SUBTYPE)
      a = type_base(a);
   while (type_kind(b) == T_SUBTYPE)
      b = type_base(b);

   if (type_kind(a) != type_kind(b))
      return false;

   // Universal integer type is equal to any other integer type
   type_t universal_int = type_universal_int();
   if (type_kind(a) == T_INTEGER
       && (a == universal_int || b == universal_int))
      return true;

   // XXX: this is not quite right as structurally equivalent types
   // may be declared in different scopes with the same name but
   // shouldn't compare equal

   if (type_ident(a) != type_ident(b))
      return false;

   if (HAS_DIMS(a) && type_dims(a) != type_dims(b))
      return false;

   // TODO: compare dimensions
   
   return true;
}

ident_t type_ident(type_t t)
{
   assert(t != NULL);

   if (t->ident == NULL && t->kind == T_SUBTYPE) {      
      char buf[128];
      snprintf(buf, sizeof(buf), "anonymous subtype of %s",
               istr(type_ident(type_base(t))));
      return ident_new(buf);
   }
   else {
      assert(t->ident != NULL);
      return t->ident;
   }
}

void type_set_ident(type_t t, ident_t id)
{
   assert(t != NULL);

   t->ident = id;
}

unsigned type_dims(type_t t)
{
   assert(t != NULL);
   assert(HAS_DIMS(t));

   return t->n_dims;
}

range_t type_dim(type_t t, unsigned n)
{
   assert(t != NULL);
   assert(HAS_DIMS(t));
   assert(n < t->n_dims);

   // TODO: need to check if this is a subtype and apply
   // constraints up the chain

   return t->dims[n];
}

void type_add_dim(type_t t, range_t r)
{
   assert(t != NULL);
   assert(HAS_DIMS(t));
   assert(t->n_dims < MAX_DIMS);

   t->dims[t->n_dims++] = r;
}

type_t type_base(type_t t)
{
   assert(t != NULL);
   assert(IS(t, T_SUBTYPE));
   assert(t->base != NULL);

   return t->base;
}

void type_set_base(type_t t, type_t b)
{
   assert(t != NULL);
   assert(IS(t, T_SUBTYPE));
   assert(b != NULL);

   t->base = b;
}

type_t type_universal_int(void)
{
   static type_t t = NULL;

   if (t == NULL) {
      t = type_new(T_INTEGER);
      type_set_ident(t, ident_new("universal integer"));

      tree_t left = tree_new(T_LITERAL);
      literal_t l_min = { { .i = INT_MIN }, .kind = L_INT };
      tree_set_literal(left, l_min);

      tree_t right = tree_new(T_LITERAL);      
      literal_t l_max = { { .i = INT_MAX }, .kind = L_INT };
      tree_set_literal(right, l_max);
      
      range_t r = { .kind  = RANGE_TO,
                    .left  = left,
                    .right = right };
      type_add_dim(t, r);
   }

   return t;
}

unsigned type_units(type_t t)
{
   assert(t != NULL);
   assert(IS(t, T_PHYSICAL));

   return t->n_units;
}

unit_t type_unit(type_t t, unsigned n)
{
   assert(t != NULL);
   assert(IS(t, T_PHYSICAL));
   assert(n < t->n_units);

   return t->units[n];
}

void type_add_unit(type_t t, unit_t u)
{
   assert(t != NULL);
   assert(IS(t, T_PHYSICAL));
   assert(t->n_units < MAX_UNITS);

   if (t->n_units == 0)
      t->units = xmalloc(MAX_UNITS * sizeof(unit_t));

   t->units[t->n_units++] = u;
}

unsigned type_enum_literals(type_t t)
{
   assert(t != NULL);
   assert(IS(t, T_ENUM));

   return t->n_literals;
}

tree_t type_enum_literal(type_t t, unsigned n)
{
   assert(t != NULL);
   assert(IS(t, T_ENUM));
   assert(n < t->n_literals);

   return t->literals[n];
}

void type_enum_add_literal(type_t t, tree_t lit)
{
   assert(t != NULL);
   assert(IS(t, T_ENUM));
   assert(tree_kind(lit) == T_ENUM_LIT);

   if (t->n_literals == 0) {
      t->lit_alloc = 16;
      t->literals = xmalloc(t->lit_alloc * sizeof(tree_t));
   }
   else if (t->n_literals == t->lit_alloc) {
      t->lit_alloc *= 2;
      t->literals = xrealloc(t->literals, t->lit_alloc * sizeof(tree_t));
   }

   t->literals[t->n_literals++] = lit;
}

unsigned type_params(type_t t)
{
   assert(t != NULL);
   assert(IS(t, T_FUNC));

   return t->n_params;
}

type_t type_param(type_t t, unsigned n)
{
   assert(t != NULL);
   assert(IS(t, T_FUNC));
   assert(n < t->n_params);

   return t->params[n];
}

void type_add_param(type_t t, type_t p)
{
   assert(t != NULL);
   assert(p != NULL);
   assert(IS(t, T_FUNC));

   if (t->params == NULL) {
      t->params_alloc = 4;
      t->params = xmalloc(t->params_alloc * sizeof(type_t));
   }
   else if (t->n_params == t->params_alloc) {
      t->params_alloc *= 2;
      t->params = xrealloc(t->params, t->params_alloc * sizeof(type_t));
   }

   t->params[t->n_params++] = p;
}

type_t type_result(type_t t)
{
   assert(t != NULL);
   assert(IS(t, T_FUNC));
   assert(t->result != NULL);

   return t->result;
}

void type_set_result(type_t t, type_t r)
{
   assert(t != NULL);
   assert(IS(t, T_FUNC));

   t->result = r;
}

void type_write(type_t t, type_wr_ctx_t ctx)
{
   FILE *f = tree_write_file(ctx->tree_ctx);
   
   if (t == NULL) {
      write_s(0xffff, f);   // Null marker
      return;
   }

   if (t->generation == ctx->generation) {
      // Already visited this type
      write_s(0xfffe, f);   // Back reference marker
      write_u(t->index, f);
      return;
   }

   t->generation = ctx->generation;
   t->index      = (ctx->n_types)++;

   write_s(t->kind, f);
   ident_write(t->ident, f);
   if (HAS_DIMS(t)) {
      write_s(t->n_dims, f);
      for (unsigned i = 0; i < t->n_dims; i++) {
         write_s(t->dims[i].kind, f);
         tree_write(t->dims[i].left, ctx->tree_ctx);
         tree_write(t->dims[i].right, ctx->tree_ctx);
      }
   }
   if (write_b(t->base != NULL, f))
      type_write(t->base, ctx);
   
   if (IS(t, T_PHYSICAL)) {
      write_s(t->n_units, f);
      for (unsigned i = 0; i < t->n_units; i++) {
         tree_write(t->units[i].multiplier, ctx->tree_ctx);
         ident_write(t->units[i].name, f);
      }
   }
   else if (IS(t, T_ENUM)) {
      write_s(t->n_literals, f);
      for (unsigned i = 0; i < t->n_literals; i++)
         tree_write(t->literals[i], ctx->tree_ctx);
   }
   else if (IS(t, T_FUNC)) {
      write_s(t->n_params, f);
      for (unsigned i = 0; i < t->n_params; i++)
         type_write(t->params[i], ctx);
      type_write(t->result, ctx);
   }
}

type_t type_read(type_rd_ctx_t ctx)
{
   FILE *f = tree_read_file(ctx->tree_ctx);

   unsigned short marker = read_s(f);
   if (marker == 0xffff)
      return NULL;   // Null marker
   else if (marker == 0xfffe) {
      // Back reference marker
      unsigned index = read_u(f);
      assert(index < ctx->n_types);
      return ctx->store[index];
   }
   
   type_t t = type_new((type_kind_t)marker);
   t->ident = ident_read(f);
   
   // Stash pointer for later back references
   // This must be done early as a child node of this type may
   // reference upwards
   if (ctx->n_types == ctx->store_sz) {
      ctx->store_sz *= 2;
      ctx->store = xrealloc(ctx->store, ctx->store_sz * sizeof(type_t));
   }
   ctx->store[ctx->n_types++] = t;
   
   if (HAS_DIMS(t)) {
      unsigned short ndims = read_s(f);
      assert(ndims < MAX_DIMS);

      for (unsigned i = 0; i < ndims; i++) {
         t->dims[i].kind  = read_s(f);
         t->dims[i].left  = tree_read(ctx->tree_ctx);
         t->dims[i].right = tree_read(ctx->tree_ctx);
      }
      t->n_dims = ndims;
   }
   if (read_b(f))
      t->base = type_read(ctx);

   if (IS(t, T_PHYSICAL)) {
      unsigned short nunits = read_s(f);
      assert(nunits < MAX_UNITS);

      t->units = xmalloc(MAX_UNITS * sizeof(unit_t));

      for (unsigned i = 0; i < nunits; i++) {
         t->units[i].multiplier = tree_read(ctx->tree_ctx);
         t->units[i].name = ident_read(f);
      }
      t->n_units = nunits;
   }
   else if (IS(t, T_ENUM)) {
      unsigned short nlits = read_s(f);

      t->literals = xmalloc(nlits * sizeof(tree_t));
      t->lit_alloc = nlits;

      for (unsigned i = 0; i < nlits; i++) {
         t->literals[i] = tree_read(ctx->tree_ctx);
      }
      t->n_literals = nlits;
   }
   else if (IS(t, T_FUNC)) {
      unsigned short nparams = read_s(f);

      t->params = xmalloc(nparams * sizeof(type_t));
      t->params_alloc = nparams;

      for (unsigned i = 0; i < nparams; i++) {
         t->params[i] = type_read(ctx);
      }
      t->n_params = nparams;

      t->result = type_read(ctx);
   }

   return t;
}

type_wr_ctx_t type_write_begin(tree_wr_ctx_t tree_ctx)
{
   static unsigned next_generation = 1;

   struct type_wr_ctx *ctx = xmalloc(sizeof(struct type_wr_ctx));
   ctx->tree_ctx   = tree_ctx;
   ctx->generation = next_generation++;
   ctx->n_types    = 0;

   return ctx;
}

void type_write_end(type_wr_ctx_t ctx)
{
   free(ctx);
}

type_rd_ctx_t type_read_begin(tree_rd_ctx_t tree_ctx)
{
   struct type_rd_ctx *ctx = xmalloc(sizeof(struct type_rd_ctx));
   ctx->tree_ctx = tree_ctx;
   ctx->store_sz = 32;
   ctx->store    = xmalloc(ctx->store_sz * sizeof(type_t));
   ctx->n_types  = 0;

   return ctx;
}

void type_read_end(type_rd_ctx_t ctx)
{
   free(ctx->store);
   free(ctx);
}
