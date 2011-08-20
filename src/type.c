#include "type.h"
#include "tree.h"
#include "util.h"

#include <assert.h>
#include <limits.h>

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

   if (type_dims(a) != type_dims(b))
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

void type_write(type_t t, tree_wr_ctx_t ctx)
{
   FILE *f = tree_write_file(ctx);
   
   if (t == NULL) {
      write_s(0xffff, f);   // Null marker
      return;
   }

   write_s(t->kind, f);
   ident_write(t->ident, f);
   if (HAS_DIMS(t)) {
      write_s(t->n_dims, f);
      for (unsigned i = 0; i < t->n_dims; i++) {
         write_s(t->dims[i].kind, f);
         tree_write(t->dims[i].left, ctx);
         tree_write(t->dims[i].right, ctx);
      }
   }
   if (write_b(t->base != NULL, f))
      type_write(t->base, ctx);
   if (IS(t, T_PHYSICAL)) {
      write_s(t->n_units, f);
      for (unsigned i = 0; i < t->n_units; i++) {
         tree_write(t->units[i].multiplier, ctx);
         ident_write(t->units[i].name, f);
      }
   }
   if (IS(t, T_ENUM)) {
      write_s(t->n_literals, f);
      for (unsigned i = 0; i < t->n_literals; i++)
         tree_write(t->literals[i], ctx);
   }
}

type_t type_read(tree_rd_ctx_t ctx)
{
   FILE *f = tree_read_file(ctx);

   unsigned short marker = read_s(f);
   if (marker == 0xffff)
      return NULL;   // Null marker
   
   type_t t = type_new((type_kind_t)marker);
   t->ident = ident_read(f);
   if (HAS_DIMS(t)) {
      unsigned short ndims = read_s(f);
      assert(ndims < MAX_DIMS);

      for (unsigned i = 0; i < ndims; i++) {
         t->dims[i].kind  = read_s(f);
         t->dims[i].left  = tree_read(ctx);
         t->dims[i].right = tree_read(ctx);
      }
      t->n_dims = ndims;
   }
   if (read_b(f))
      t->base = type_read(ctx);
   if (IS(t, T_PHYSICAL)) {
      unsigned short nunits = read_s(f);
      assert(nunits < MAX_UNITS);

      for (unsigned i = 0; i < nunits; i++) {
         t->units[i].multiplier = tree_read(ctx);
         t->units[i].name = ident_read(f);
      }
      t->n_units = nunits;
   }
   if (IS(t, T_ENUM)) {
      unsigned short nlits = read_s(f);

      t->literals = xmalloc(nlits * sizeof(tree_t));
      t->lit_alloc = nlits;

      for (unsigned i = 0; i < nlits; i++) {
         t->literals[i] = tree_read(ctx);
      }
      t->n_literals = nlits;      
   }

   return t;
}
