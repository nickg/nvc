//
//  Copyright (C) 2011-2012  Nick Gasson
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
#include <string.h>
#include <ctype.h>
#include <float.h>

#define MAX_DIMS      4
#define MAX_UNITS     16
#define MAX_ITEMS     4
#define ARRAY_BASE_SZ 8

typedef struct {
   uint32_t  count;
   uint32_t  max;
   type_t   *items;
} type_array_t;

enum {
   I_PARAMS       = (1 << 0),
   I_INDEX_CONSTR = (1 << 1),
   I_BASE         = (1 << 2),
   I_ELEM         = (1 << 3),
};

typedef union {
   type_array_t type_array;
   type_t       type;
} item_t;

typedef uint32_t imask_t;

static const imask_t has_map[T_LAST_TYPE_KIND] = {
   // T_UNRESOLVED
   (0),

   // T_SUBTYPE
   (I_BASE),

   // T_INTEGER
   (0),

   // T_REAL
   (0),

   // T_ENUM
   (0),

   // T_PHYSICAL
   (0),

   // T_CARRAY
   (I_ELEM),

   // T_UARRAY
   (I_INDEX_CONSTR | I_ELEM),

   // T_RECORD
   (0),

   // T_FILE
   (0),

   // T_ACCESS
   (0),

   // T_FUNC
   (I_PARAMS),

   // T_INCOMPLETE
   (0),

   // T_PROC
   (I_PARAMS),
};

#define ITEM_TYPE_ARRAY (I_PARAMS | I_INDEX_CONSTR)
#define ITEM_TYPE       (I_BASE | I_ELEM)

static const char *kind_text_map[T_LAST_TYPE_KIND] = {
   "T_UNRESOLVED", "T_SUBTYPE",  "T_INTEGER", "T_REAL",
   "T_ENUM",       "T_PHYSICAL", "T_CARRAY",  "T_UARRAY",
   "T_RECORD",     "T_FILE",     "T_ACCESS",  "T_FUNC",
   "T_INCOMPLETE", "T_PROC",
};

static const char *item_text_map[] = {
   "I_PARAMS", "I_INDEX_CONSTR", "I_BASE", "I_ELEM",
};

struct type {
   type_kind_t kind;
   ident_t     ident;
   range_t     *dims;
   unsigned    n_dims;
   item_t      items[MAX_ITEMS];

   union {
      type_t access;        // T_ACCESS
      type_t file;          // T_FILE
   };
   union {
      type_t result;     // T_FUNC
      tree_t resolution; // T_SUBTYPE
   };
   union {
      struct {   // T_ENUM
         tree_t   *literals;
         unsigned n_literals;
         size_t   lit_alloc;
      };
      struct {   // T_PHYSICAL
         unit_t   *units;
         unsigned n_units;
      };
   };

   // Serialisation accounting
   uint32_t generation;
   uint32_t index;
};

struct type_wr_ctx {
   tree_wr_ctx_t  tree_ctx;
   ident_wr_ctx_t ident_ctx;
   unsigned       generation;
   unsigned       n_types;
};

struct type_rd_ctx {
   tree_rd_ctx_t  tree_ctx;
   ident_rd_ctx_t ident_ctx;
   unsigned       n_types;
   type_t         *store;
   unsigned       store_sz;
};

#define IS(t, k) ((t)->kind == (k))
#define HAS_DIMS(t) \
   (IS(t, T_INTEGER) || IS(t, T_SUBTYPE) || IS(t, T_PHYSICAL)   \
    || IS(t, T_CARRAY) || IS(t, T_REAL))
#define HAS_RESOLUTION(t) \
   (IS(t, T_SUBTYPE) || IS(t, T_UNRESOLVED))

// Garbage collection
static type_t *all_types = NULL;
static size_t max_types = 128;   // Grows at runtime
static size_t n_types_alloc = 0;

static uint32_t format_digest;
static int item_lookup[T_LAST_TREE_KIND][32];

static void type_one_time_init(void)
{
   static bool done = false;
   if (likely(done))
      return;

   for (int i = 0; i < T_LAST_TYPE_KIND; i++) {
      const int nitems = __builtin_popcount(has_map[i]);
      assert(nitems <= MAX_ITEMS);

      // Knuth's multiplicative hash
      format_digest += has_map[i] * 2654435761u;

      int n = 0;
      for (int j = 0; j < 32; j++) {
         if (has_map[i] & (1 << j))
            item_lookup[i][j] = n++;
         else
            item_lookup[i][j] = -1;
      }
   }

   done = true;
}

static item_t *lookup_item(type_t t, imask_t mask)
{
   assert(t != NULL);
   assert((mask & (mask - 1)) == 0);
   const imask_t has = has_map[t->kind];

   if (unlikely((has & mask) == 0)) {
      int item;
      for (item = 0; (mask & (1 << item)) == 0; item++)
         ;

      assert(item < ARRAY_LEN(item_text_map));
      assert(false);
      fatal("tree kind %s does not have item %s",
            kind_text_map[t->kind], item_text_map[item]);
   }

   const int tzc = __builtin_ctz(mask);
   const int n   = item_lookup[t->kind][tzc];

   return &(t->items[n]);
}

static void item_without_type(imask_t mask)
{
   int item;
   for (item = 0; (mask & (1 << item)) == 0; item++)
      ;

   assert(item < ARRAY_LEN(item_text_map));
   fatal("tree item %s does not have a type", item_text_map[item]);
}

static void type_array_add(type_array_t *a, type_t t)
{
   if (a->max == 0) {
      a->items = xmalloc(sizeof(type_t) * ARRAY_BASE_SZ);
      a->max   = ARRAY_BASE_SZ;
   }
   else if (a->count == a->max) {
      a->max *= 2;
      a->items = xrealloc(a->items, sizeof(type_t) * a->max);
   }

   a->items[a->count++] = t;
}

static inline type_t type_array_nth(type_array_t *a, unsigned n)
{
   assert(n < a->count);
   return a->items[n];
}

type_t type_new(type_kind_t kind)
{
   type_one_time_init();

   struct type *t = xmalloc(sizeof(struct type));
   memset(t, '\0', sizeof(struct type));
   t->kind = kind;

   if (all_types == NULL)
      all_types = xmalloc(sizeof(tree_t) * max_types);
   else if (n_types_alloc == max_types) {
      max_types *= 2;
      all_types = xrealloc(all_types, sizeof(tree_t) * max_types);
   }
   all_types[n_types_alloc++] = t;

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

   const bool compare_c_u_arrays =
      (type_kind(a) == T_CARRAY && type_kind(b) == T_UARRAY)
      || (type_kind(a) == T_UARRAY && type_kind(b) == T_CARRAY);

   if ((type_kind(a) != type_kind(b)) && !compare_c_u_arrays)
      return false;

   // Universal integer type is equal to any other integer type
   type_t universal_int = type_universal_int();
   ident_t uint_i = type_ident(universal_int);
   if (type_kind(a) == T_INTEGER
       && (type_ident(a) == uint_i || type_ident(b) == uint_i))
      return true;

   // Universal real type is equal to any other real type
   type_t universal_real = type_universal_real();
   ident_t ureal_i = type_ident(universal_real);
   if (type_kind(a) == T_REAL
       && (type_ident(a) == ureal_i || type_ident(b) == ureal_i))
      return true;

   // XXX: this is not quite right as structurally equivalent types
   // may be declared in different scopes with the same name but
   // shouldn't compare equal

   if (type_ident(a) != type_ident(b))
      return false;

   if (compare_c_u_arrays)
      return type_eq(type_elem(a), type_elem(b));

   if (HAS_DIMS(a) && type_dims(a) != type_dims(b))
      return false;

   if (type_kind(a) == T_FUNC) {
      if (!type_eq(type_result(a), type_result(b)))
         return false;
   }

   if (has_map[a->kind] & I_PARAMS) {
      if (type_params(a) != type_params(b))
         return false;

      for (unsigned i = 0; i < type_params(a); i++) {
         if (!type_eq(type_param(a, i), type_param(b, i)))
             return false;
      }
   }

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

   if (t->dims == NULL)
      t->dims = xmalloc(MAX_DIMS * sizeof(range_t));

   t->dims[t->n_dims++] = r;
}

void type_change_dim(type_t t, unsigned n, range_t r)
{
   assert(t != NULL);
   assert(HAS_DIMS(t));
   assert(n < t->n_dims);

   t->dims[n] = r;
}

type_t type_base(type_t t)
{
   item_t *item = lookup_item(t, I_BASE);
   assert(item->type != NULL);
   return item->type;
}

void type_set_base(type_t t, type_t b)
{
   lookup_item(t, I_BASE)->type = b;
}

type_t type_elem(type_t t)
{
   assert(t != NULL);

   if (IS(t, T_SUBTYPE))
      return type_elem(type_base(t));
   else {
      item_t *item = lookup_item(t, I_ELEM);
      assert(item->type != NULL);
      return item->type;
   }
}

void type_set_elem(type_t t, type_t e)
{
   lookup_item(t, I_ELEM)->type = e;
}

static type_t type_make_universal(type_kind_t kind, const char *name,
                                  literal_t min, literal_t max)
{
   type_t t = type_new(kind);
   type_set_ident(t, ident_new(name));

   tree_t left = tree_new(T_LITERAL);
   tree_set_literal(left, min);
   tree_set_type(left, t);

   tree_t right = tree_new(T_LITERAL);
   tree_set_literal(right, max);
   tree_set_type(right, t);

   range_t r = { .kind  = RANGE_TO,
                 .left  = left,
                 .right = right };
   type_add_dim(t, r);

   return t;
}

type_t type_universal_int(void)
{
   static type_t t = NULL;

   if (t == NULL) {
      literal_t min = { { .i = INT_MIN }, .kind = L_INT };
      literal_t max = { { .i = INT_MAX }, .kind = L_INT };
      t = type_make_universal(T_INTEGER, "universal integer", min, max);
   }

   return t;
}

type_t type_universal_real(void)
{
   static type_t t = NULL;

   if (t == NULL) {
      literal_t min = { { .r = -DBL_MAX }, .kind = L_REAL };
      literal_t max = { { .r = DBL_MAX },  .kind = L_REAL };
      t = type_make_universal(T_REAL, "universal real", min, max);
   }

   return t;
}

bool type_is_universal(type_t t)
{
   assert(t != NULL);

   switch (t->kind) {
   case T_INTEGER:
      return t->ident == type_universal_int()->ident;
   case T_REAL:
      return t->ident == type_universal_real()->ident;
   default:
      return false;
   }
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
   return lookup_item(t, I_PARAMS)->type_array.count;
}

type_t type_param(type_t t, unsigned n)
{
   return type_array_nth(&(lookup_item(t, I_PARAMS)->type_array), n);
}

void type_add_param(type_t t, type_t p)
{
   type_array_add(&(lookup_item(t, I_PARAMS)->type_array), p);
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

void type_replace(type_t t, type_t a)
{
   assert(t != NULL);
   assert(IS(t, T_INCOMPLETE));

   t->kind  = a->kind;
   t->ident = a->ident;

   if (HAS_DIMS(a)) {
      for (unsigned i = 0; i < a->n_dims; i++)
         type_add_dim(t, type_dim(a, i));
   }

   switch (a->kind) {
   case T_UARRAY:
      for (unsigned i = 0; i < type_index_constrs(a); i++)
         type_add_index_constr(t, type_index_constr(a, i));

      // Fall-through
   case T_CARRAY:
      type_set_elem(t, type_elem(a));
      break;

   case T_SUBTYPE:
      type_set_base(t, type_base(a));
      break;

   case T_FUNC:
      type_set_result(t, type_result(a));
      break;

   case T_INTEGER:
   case T_REAL:
      break;

   case T_ENUM:
      for (unsigned i = 0; i < type_enum_literals(a); i++)
         type_enum_add_literal(t, type_enum_literal(a, i));
      break;

   default:
      assert(false);
   }
}

unsigned type_index_constrs(type_t t)
{
   return lookup_item(t, I_INDEX_CONSTR)->type_array.count;
}

void type_add_index_constr(type_t t, type_t c)
{
   type_array_add(&(lookup_item(t, I_INDEX_CONSTR)->type_array), c);
}

void type_change_index_constr(type_t t, unsigned n, type_t c)
{
   type_array_t *a = &(lookup_item(t, I_INDEX_CONSTR)->type_array);
   assert(n < a->count);
   a->items[n] = c;
}

type_t type_index_constr(type_t t, unsigned n)
{
   return type_array_nth(&(lookup_item(t, I_INDEX_CONSTR)->type_array), n);
}

void type_set_resolution(type_t t, tree_t r)
{
   assert(t != NULL);
   assert(HAS_RESOLUTION(t));

   t->resolution = r;
}

bool type_has_resolution(type_t t)
{
   assert(t != NULL);
   assert(HAS_RESOLUTION(t));

   return t->resolution != NULL;
}

tree_t type_resolution(type_t t)
{
   assert(t != NULL);
   assert(HAS_RESOLUTION(t));
   assert(t->resolution != NULL);

   return t->resolution;
}

type_t type_access(type_t t)
{
   assert(t != NULL);
   assert(IS(t, T_ACCESS));

   return t->access;
}

void type_set_access(type_t t, type_t a)
{
   assert(t != NULL);
   assert(a != NULL);
   assert(IS(t, T_ACCESS));

   t->access = a;
}

type_t type_file(type_t t)
{
   assert(t != NULL);
   assert(IS(t, T_FILE));

   return t->file;
}

void type_set_file(type_t t, type_t f)
{
   assert(t != NULL);
   assert(f != NULL);
   assert(IS(t, T_FILE));

   t->file = f;
}

void type_write(type_t t, type_wr_ctx_t ctx)
{
   FILE *f = tree_write_file(ctx->tree_ctx);

   if (t == NULL) {
      write_u16(0xffff, f);   // Null marker
      return;
   }

   if (t->generation == ctx->generation) {
      // Already visited this type
      write_u16(0xfffe, f);   // Back reference marker
      write_u32(t->index, f);
      return;
   }

   t->generation = ctx->generation;
   t->index      = (ctx->n_types)++;

   write_u16(t->kind, f);

   // Call type_ident here to generate an arbitrary name if needed
   ident_write(type_ident(t), ctx->ident_ctx);

   const uint32_t has = has_map[t->kind];
   const int nitems = __builtin_popcount(has);
   uint32_t mask = 1;
   for (int n = 0; n < nitems; mask <<= 1) {
      if (has & mask) {
         if (ITEM_TYPE_ARRAY & mask) {
            type_array_t *a = &(t->items[n].type_array);
            write_u16(a->count, f);
            for (unsigned i = 0; i < a->count; i++)
               type_write(a->items[i], ctx);
         }
         else if (ITEM_TYPE & mask)
            type_write(t->items[n].type, ctx);
         else
            item_without_type(mask);
         n++;
      }
   }

   if (HAS_DIMS(t)) {
      write_u16(t->n_dims, f);
      for (unsigned i = 0; i < t->n_dims; i++) {
         write_u16(t->dims[i].kind, f);
         tree_write(t->dims[i].left, ctx->tree_ctx);
         tree_write(t->dims[i].right, ctx->tree_ctx);
      }
   }
   if (HAS_RESOLUTION(t))
      tree_write(t->resolution, ctx->tree_ctx);

   if (IS(t, T_PHYSICAL)) {
      write_u16(t->n_units, f);
      for (unsigned i = 0; i < t->n_units; i++) {
         tree_write(t->units[i].multiplier, ctx->tree_ctx);
         ident_write(t->units[i].name, ctx->ident_ctx);
      }
   }
   else if (IS(t, T_ENUM)) {
      write_u16(t->n_literals, f);
      for (unsigned i = 0; i < t->n_literals; i++)
         tree_write(t->literals[i], ctx->tree_ctx);
   }
   else if (IS(t, T_FUNC))
      type_write(t->result, ctx);
   else if (IS(t, T_ACCESS))
      type_write(t->access, ctx);
   else if (IS(t, T_FILE))
      type_write(t->file, ctx);
}

type_t type_read(type_rd_ctx_t ctx)
{
   FILE *f = tree_read_file(ctx->tree_ctx);

   unsigned short marker = read_u16(f);
   if (marker == 0xffff)
      return NULL;   // Null marker
   else if (marker == 0xfffe) {
      // Back reference marker
      unsigned index = read_u32(f);
      assert(index < ctx->n_types);
      return ctx->store[index];
   }

   assert(marker < T_LAST_TYPE_KIND);

   type_t t = type_new((type_kind_t)marker);
   t->ident = ident_read(ctx->ident_ctx);

   // Stash pointer for later back references
   // This must be done early as a child node of this type may
   // reference upwards
   if (ctx->n_types == ctx->store_sz) {
      ctx->store_sz *= 2;
      ctx->store = xrealloc(ctx->store, ctx->store_sz * sizeof(type_t));
   }
   ctx->store[ctx->n_types++] = t;

   const uint32_t has = has_map[t->kind];
   const int nitems = __builtin_popcount(has);
   uint32_t mask = 1;
   for (int n = 0; n < nitems; mask <<= 1) {
      if (has & mask) {
         if (ITEM_TYPE_ARRAY & mask) {
            type_array_t *a = &(t->items[n].type_array);

            a->count = a->max = read_u16(f);
            a->items = xmalloc(a->count * sizeof(type_t));

            for (unsigned i = 0; i < a->count; i++)
               a->items[i] = type_read(ctx);
         }
         else if (ITEM_TYPE & mask)
            t->items[n].type = type_read(ctx);
         else
            item_without_type(mask);
         n++;
      }
   }

   if (HAS_DIMS(t)) {
      unsigned short ndims = read_u16(f);
      assert(ndims < MAX_DIMS);
      t->dims = xmalloc(sizeof(range_t) * MAX_DIMS);

      for (unsigned i = 0; i < ndims; i++) {
         t->dims[i].kind  = read_u16(f);
         t->dims[i].left  = tree_read(ctx->tree_ctx);
         t->dims[i].right = tree_read(ctx->tree_ctx);
      }
      t->n_dims = ndims;
   }
   if (HAS_RESOLUTION(t))
      t->resolution = tree_read(ctx->tree_ctx);

   if (IS(t, T_PHYSICAL)) {
      unsigned short nunits = read_u16(f);
      assert(nunits < MAX_UNITS);

      t->units = xmalloc(MAX_UNITS * sizeof(unit_t));

      for (unsigned i = 0; i < nunits; i++) {
         t->units[i].multiplier = tree_read(ctx->tree_ctx);
         t->units[i].name = ident_read(ctx->ident_ctx);
      }
      t->n_units = nunits;
   }
   else if (IS(t, T_ENUM)) {
      unsigned short nlits = read_u16(f);

      t->literals = xmalloc(nlits * sizeof(tree_t));
      t->lit_alloc = nlits;

      for (unsigned i = 0; i < nlits; i++) {
         t->literals[i] = tree_read(ctx->tree_ctx);
      }
      t->n_literals = nlits;
   }
   else if (IS(t, T_FUNC))
      t->result = type_read(ctx);
   else if (IS(t, T_ACCESS))
      t->access = type_read(ctx);
   else if (IS(t, T_FILE))
      t->file = type_read(ctx);

   return t;
}

type_wr_ctx_t type_write_begin(tree_wr_ctx_t tree_ctx, ident_wr_ctx_t ident_ctx)
{
   extern unsigned next_generation;

   struct type_wr_ctx *ctx = xmalloc(sizeof(struct type_wr_ctx));
   ctx->tree_ctx   = tree_ctx;
   ctx->ident_ctx  = ident_ctx;
   ctx->generation = next_generation++;
   ctx->n_types    = 0;

   return ctx;
}

void type_write_end(type_wr_ctx_t ctx)
{
   free(ctx);
}

type_rd_ctx_t type_read_begin(tree_rd_ctx_t tree_ctx, ident_rd_ctx_t ident_ctx)
{
   type_one_time_init();

   struct type_rd_ctx *ctx = xmalloc(sizeof(struct type_rd_ctx));
   ctx->tree_ctx  = tree_ctx;
   ctx->ident_ctx = ident_ctx;
   ctx->store_sz  = 32;
   ctx->store     = xmalloc(ctx->store_sz * sizeof(type_t));
   ctx->n_types   = 0;

   return ctx;
}

void type_read_end(type_rd_ctx_t ctx)
{
   free(ctx->store);
   free(ctx);
}

const char *type_pp(type_t t)
{
   assert(t != NULL);

   switch (type_kind(t)) {
   case T_FUNC:
   case T_PROC:
      {
         static char fn[256];
         char *p = fn;
         const char *end = fn + sizeof(fn);
         const char *fname = istr(type_ident(t));

         p += snprintf(p, end - p, "%s(", fname);
         for (unsigned i = 0; i < type_params(t); i++)
            p += snprintf(p, end - p, "%s%s",
                          (i == 0 ? "" : ", "),
                          istr(type_ident(type_param(t, i))));
         p += snprintf(p, end - p, ")");
         if (type_kind(t) == T_FUNC)
            p += snprintf(p, end - p, " return %s",
                          istr(type_ident(type_result(t))));

         return fn;
      }

   default:
      return istr(type_ident(t));
   }
}

bool type_update_generation(type_t t, unsigned generation)
{
   if (t->generation == generation)
      return false;
   else {
      t->generation = generation;
      return true;
   }
}

void type_sweep(unsigned generation)
{
   for (unsigned i = 0; i < n_types_alloc; i++) {
      type_t t = all_types[i];
      if (t->generation < generation) {

         const uint32_t has = has_map[t->kind];
         const int nitems = __builtin_popcount(has);
         uint32_t mask = 1;
         for (int n = 0; n < nitems; mask <<= 1) {
            if (has & mask) {
               if (ITEM_TYPE_ARRAY & mask)
                  free(t->items[n].type_array.items);
               else if (ITEM_TYPE & mask)
                  ;
               else
                  item_without_type(mask);
               n++;
            }
         }

         if (IS(t, T_PHYSICAL) && t->units != NULL)
            free(t->units);
         else if (IS(t, T_ENUM) && t->literals != NULL)
            free(t->literals);

         if (HAS_DIMS(t) && t->dims != NULL)
            free(t->dims);

         free(t);

         all_types[i] = NULL;
      }
   }

   // Compact
   size_t p = 0;
   for (unsigned i = 0; i < n_types_alloc; i++) {
      if (all_types[i] != NULL)
         all_types[p++] = all_types[i];
   }

   if (getenv("NVC_GC_VERBOSE") != NULL)
      printf("[gc: freed %zu types; %zu allocated]\n",
             n_types_alloc - p, p);

   n_types_alloc = p;
}

bool type_is_array(type_t t)
{
   if (t->kind == T_SUBTYPE)
      return type_is_array(type_base(t));
   else
      return (t->kind == T_CARRAY || t->kind == T_UARRAY);
}

type_t type_base_recur(type_t t)
{
   while (t->kind == T_SUBTYPE)
      t = type_base(t);
   return t;
}

uint32_t type_format_digest(void)
{
   type_one_time_init();
   return format_digest;
}
