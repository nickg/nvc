//
//  Copyright (C) 2011-2013  Nick Gasson
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
#include "array.h"

#include <assert.h>
#include <limits.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <float.h>

#define MAX_ITEMS 3

DEFINE_ARRAY(type);
DEFINE_ARRAY(tree);
DEFINE_ARRAY(range);

enum {
   I_PARAMS       = (1 << 0),
   I_INDEX_CONSTR = (1 << 1),
   I_BASE         = (1 << 2),
   I_ELEM         = (1 << 3),
   I_FILE         = (1 << 4),
   I_ACCESS       = (1 << 5),
   I_RESOLUTION   = (1 << 6),
   I_RESULT       = (1 << 7),
   I_UNITS        = (1 << 8),
   I_LITERALS     = (1 << 9),
   I_DIMS         = (1 << 10),
   I_FIELDS       = (1 << 11),
};

typedef union {
   type_array_t  type_array;
   type_t        type;
   tree_t        tree;
   tree_array_t  tree_array;
   range_array_t range_array;
} item_t;

typedef uint32_t imask_t;

static const imask_t has_map[T_LAST_TYPE_KIND] = {
   // T_UNRESOLVED
   (I_RESOLUTION),

   // T_SUBTYPE
   (I_BASE | I_RESOLUTION | I_DIMS),

   // T_INTEGER
   (I_DIMS),

   // T_REAL
   (I_DIMS),

   // T_ENUM
   (I_LITERALS),

   // T_PHYSICAL
   (I_UNITS | I_DIMS),

   // T_CARRAY
   (I_ELEM | I_DIMS),

   // T_UARRAY
   (I_INDEX_CONSTR | I_ELEM),

   // T_RECORD
   (I_FIELDS),

   // T_FILE
   (I_FILE),

   // T_ACCESS
   (I_ACCESS),

   // T_FUNC
   (I_PARAMS | I_RESULT),

   // T_INCOMPLETE
   (0),

   // T_PROC
   (I_PARAMS),
};

#define ITEM_TYPE_ARRAY  (I_PARAMS | I_INDEX_CONSTR)
#define ITEM_TYPE        (I_BASE | I_ELEM | I_ACCESS | I_RESULT | I_FILE)
#define ITEM_TREE        (I_RESOLUTION)
#define ITEM_TREE_ARRAY  (I_LITERALS | I_FIELDS | I_UNITS)
#define ITEM_RANGE_ARRAY (I_DIMS)

static const char *kind_text_map[T_LAST_TYPE_KIND] = {
   "T_UNRESOLVED", "T_SUBTYPE",  "T_INTEGER", "T_REAL",
   "T_ENUM",       "T_PHYSICAL", "T_CARRAY",  "T_UARRAY",
   "T_RECORD",     "T_FILE",     "T_ACCESS",  "T_FUNC",
   "T_INCOMPLETE", "T_PROC",
};

static const char *item_text_map[] = {
   "I_PARAMS", "I_INDEX_CONSTR", "I_BASE",       "I_ELEM",
   "I_FILE",   "I_ACCESS",       "I_RESOLUTION", "I_RESULT",
   "I_UNITS",  "I_LITERALS",     "I_DIMS",       "I_FIELDS",
};

struct type {
   type_kind_t kind;
   ident_t     ident;
   item_t      items[MAX_ITEMS];

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
      fatal_trace("type kind %s does not have item %s",
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
   fatal_trace("type item %s does not have a type", item_text_map[item]);
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

   type_kind_t kind_a = type_kind(a);
   type_kind_t kind_b = type_kind(b);

   if ((kind_a == T_UNRESOLVED) || (kind_b == T_UNRESOLVED))
      return false;

   if (a == b)
      return true;

   // Subtypes are convertible to the base type
   while ((kind_a = type_kind(a)) == T_SUBTYPE)
      a = type_base(a);
   while ((kind_b = type_kind(b)) == T_SUBTYPE)
      b = type_base(b);

   const bool compare_c_u_arrays =
      (kind_a == T_CARRAY && kind_b == T_UARRAY)
      || (kind_a == T_UARRAY && kind_b == T_CARRAY);

   if ((kind_a != kind_b) && !compare_c_u_arrays)
      return false;

   // Universal integer type is equal to any other integer type
   type_t universal_int = type_universal_int();
   ident_t uint_i = type_ident(universal_int);
   if (kind_a == T_INTEGER
       && (type_ident(a) == uint_i || type_ident(b) == uint_i))
      return true;

   // Universal real type is equal to any other real type
   type_t universal_real = type_universal_real();
   ident_t ureal_i = type_ident(universal_real);
   if (kind_a == T_REAL
       && (type_ident(a) == ureal_i || type_ident(b) == ureal_i))
      return true;

   // XXX: this is not quite right as structurally equivalent types
   // may be declared in different scopes with the same name but
   // shouldn't compare equal

   if (type_ident(a) != type_ident(b))
      return false;

   // Access types are equal if the pointed to type is the same
   if (kind_a == T_ACCESS)
      return type_eq(type_access(a), type_access(b));

   if (compare_c_u_arrays)
      return type_eq(type_elem(a), type_elem(b));

   const imask_t has = has_map[a->kind];

   if ((has & I_DIMS) && (type_dims(a) != type_dims(b)))
      return false;

   if (type_kind(a) == T_FUNC) {
      if (!type_eq(type_result(a), type_result(b)))
         return false;
   }

   if (has & I_PARAMS) {
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

   if (t->ident == NULL) {
      char buf[128];
      switch (t->kind) {
      case T_SUBTYPE:
         snprintf(buf, sizeof(buf), "anonymous subtype of %s",
                  istr(type_ident(type_base(t))));
         break;

      case T_ACCESS:
         snprintf(buf, sizeof(buf), "access to %s",
                  istr(type_ident(type_access(t))));
         break;

      default:
         assert(false);
      }

      return ident_new(buf);
   }
   else
      return t->ident;
}

void type_set_ident(type_t t, ident_t id)
{
   assert(t != NULL);

   t->ident = id;
}

unsigned type_dims(type_t t)
{
   return lookup_item(t, I_DIMS)->range_array.count;
}

range_t type_dim(type_t t, unsigned n)
{
   return range_array_nth(&(lookup_item(t, I_DIMS)->range_array), n);
}

void type_add_dim(type_t t, range_t r)
{
   range_array_add(&(lookup_item(t, I_DIMS)->range_array), r);
}

void type_change_dim(type_t t, unsigned n, range_t r)
{
   item_t *item = lookup_item(t, I_DIMS);
   assert(n < item->range_array.count);
   item->range_array.items[n] = r;
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
   return lookup_item(t, I_UNITS)->tree_array.count;
}

tree_t type_unit(type_t t, unsigned n)
{
   return tree_array_nth(&(lookup_item(t, I_UNITS)->tree_array), n);
}

void type_add_unit(type_t t, tree_t u)
{
   tree_array_add(&(lookup_item(t, I_UNITS)->tree_array), u);
}

unsigned type_enum_literals(type_t t)
{
   return lookup_item(t, I_LITERALS)->tree_array.count;
}

tree_t type_enum_literal(type_t t, unsigned n)
{
   return tree_array_nth(&(lookup_item(t, I_LITERALS)->tree_array), n);
}

void type_enum_add_literal(type_t t, tree_t lit)
{
   assert(tree_kind(lit) == T_ENUM_LIT);
   tree_array_add(&(lookup_item(t, I_LITERALS)->tree_array), lit);
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

unsigned type_fields(type_t t)
{
   return lookup_item(t, I_FIELDS)->tree_array.count;
}

tree_t type_field(type_t t, unsigned n)
{
   return tree_array_nth(&(lookup_item(t, I_FIELDS)->tree_array), n);
}

void type_add_field(type_t t, tree_t p)
{
   assert(tree_kind(p) == T_FIELD_DECL);
   tree_array_add(&(lookup_item(t, I_FIELDS)->tree_array), p);
}

type_t type_result(type_t t)
{
   item_t *item = lookup_item(t, I_RESULT);
   assert(item->type != NULL);
   return item->type;
}

void type_set_result(type_t t, type_t r)
{
   lookup_item(t, I_RESULT)->type = r;
}

void type_replace(type_t t, type_t a)
{
   assert(t != NULL);
   assert(IS(t, T_INCOMPLETE));

   t->kind  = a->kind;
   t->ident = a->ident;

   const imask_t has = has_map[t->kind];

   if (has & I_DIMS) {
      const int ndims = type_dims(a);
      for (int i = 0; i < ndims; i++)
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

   case T_RECORD:
      for (unsigned i = 0; i < type_fields(a); i++)
         type_add_field(t, type_field(a, i));
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
   lookup_item(t, I_RESOLUTION)->tree = r;
}

bool type_has_resolution(type_t t)
{
   return lookup_item(t, I_RESOLUTION)->tree != NULL;
}

tree_t type_resolution(type_t t)
{
   item_t *item = lookup_item(t, I_RESOLUTION);
   assert(item->tree != NULL);
   return item->tree;
}

type_t type_access(type_t t)
{
   return lookup_item(t, I_ACCESS)->type;
}

void type_set_access(type_t t, type_t a)
{
   lookup_item(t, I_ACCESS)->type = a;
}

type_t type_file(type_t t)
{
   return lookup_item(t, I_FILE)->type;
}

void type_set_file(type_t t, type_t f)
{
   lookup_item(t, I_FILE)->type = f;
}

void type_write(type_t t, type_wr_ctx_t ctx)
{
   fbuf_t *f = tree_write_file(ctx->tree_ctx);

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
         else if (ITEM_TREE & mask)
            tree_write(t->items[n].tree, ctx->tree_ctx);
         else if (ITEM_TREE_ARRAY & mask) {
            tree_array_t *a = &(t->items[n].tree_array);
            write_u16(a->count, f);
            for (unsigned i = 0; i < a->count; i++)
               tree_write(a->items[i], ctx->tree_ctx);
         }
         else if (ITEM_RANGE_ARRAY & mask) {
            range_array_t *a = &(t->items[n].range_array);
            write_u16(a->count, f);
            for (unsigned i = 0; i < a->count; i++) {
               write_u8(a->items[i].kind, f);
               tree_write(a->items[i].left, ctx->tree_ctx);
               tree_write(a->items[i].right, ctx->tree_ctx);
            }
         }
         else
            item_without_type(mask);
         n++;
      }
   }
}

type_t type_read(type_rd_ctx_t ctx)
{
   fbuf_t *f = tree_read_file(ctx->tree_ctx);

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
            type_array_resize(a, read_u16(f), NULL);

            for (unsigned i = 0; i < a->count; i++)
               a->items[i] = type_read(ctx);
         }
         else if (ITEM_TYPE & mask)
            t->items[n].type = type_read(ctx);
         else if (ITEM_TREE & mask)
            t->items[n].tree = tree_read(ctx->tree_ctx);
         else if (ITEM_TREE_ARRAY & mask) {
            tree_array_t *a = &(t->items[n].tree_array);
            tree_array_resize(a, read_u16(f), NULL);

            for (unsigned i = 0; i < a->count; i++)
               a->items[i] = tree_read(ctx->tree_ctx);
         }
         else if (ITEM_RANGE_ARRAY & mask) {
            range_array_t *a = &(t->items[n].range_array);
            range_t dummy = { NULL, NULL, 0 };
            range_array_resize(a, read_u16(f), dummy);

            for (unsigned i = 0; i < a->count; i++) {
               a->items[i].kind  = read_u8(f);
               a->items[i].left  = tree_read(ctx->tree_ctx);
               a->items[i].right = tree_read(ctx->tree_ctx);
            }
         }
         else
            item_without_type(mask);
         n++;
      }
   }

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

const char *type_pp_minify(type_t t, minify_fn_t fn)
{
   assert(t != NULL);

   switch (type_kind(t)) {
   case T_FUNC:
   case T_PROC:
      {
         char *buf = get_fmt_buf(256);
         static_printf_begin(buf, 256);

         const char *fname = (*fn)(istr(type_ident(t)));

         static_printf(buf, "%s(", fname);
         const int nparams = type_params(t);
         for (int i = 0; i < nparams; i++)
            static_printf(buf, "%s%s",
                          (i == 0 ? "" : ", "),
                          (*fn)(istr(type_ident(type_param(t, i)))));
         static_printf(buf, ")");
         if (type_kind(t) == T_FUNC)
            static_printf(buf, " return %s",
                          (*fn)(istr(type_ident(type_result(t)))));

         return buf;
      }

   default:
      return (*fn)(istr(type_ident(t)));
   }
}

static const char *type_minify_identity(const char *s)
{
   return s;
}

const char *type_pp(type_t t)
{
   return type_pp_minify(t, type_minify_identity);
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
               else if (ITEM_TREE & mask)
                  ;
               else if (ITEM_TREE_ARRAY & mask)
                  free(t->items[n].tree_array.items);
               else if (ITEM_RANGE_ARRAY & mask)
                  free(t->items[n].range_array.items);
               else
                  item_without_type(mask);
               n++;
            }
         }

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
   assert(t != NULL);
   if (t->kind == T_SUBTYPE)
      return type_is_array(type_base(t));
   else
      return (t->kind == T_CARRAY || t->kind == T_UARRAY);
}

type_t type_base_recur(type_t t)
{
   assert(t != NULL);
   while (t->kind == T_SUBTYPE)
      t = type_base(t);
   return t;
}

uint32_t type_format_digest(void)
{
   type_one_time_init();
   return format_digest;
}

void type_visit_trees(type_t t, unsigned generation,
                      tree_visit_fn_t fn, void *context)
{
   if (t == NULL)
      return;

   if (t->generation == generation)
      return;
   else
      t->generation = generation;

   const uint32_t has = has_map[t->kind];
   const int nitems = __builtin_popcount(has);
   uint32_t mask = 1;
   for (int n = 0; n < nitems; mask <<= 1) {
      if (has & mask) {
         if (ITEM_TYPE_ARRAY & mask) {
            type_array_t *a = &(t->items[n].type_array);
            for (unsigned i = 0; i < a->count; i++)
               type_visit_trees(a->items[i], generation, fn, context);
         }
         else if (ITEM_TYPE & mask)
            type_visit_trees(t->items[n].type, generation, fn, context);
         else if (ITEM_TREE & mask)
            (*fn)(t->items[n].tree, context);
         else if (ITEM_TREE_ARRAY & mask) {
            tree_array_t *a = &(t->items[n].tree_array);
            for (unsigned i = 0; i < a->count; i++)
               (*fn)(a->items[i], context);
         }
         else if (ITEM_RANGE_ARRAY & mask) {
            range_array_t *a = &(t->items[n].range_array);
            for (unsigned i = 0; i < a->count; i++) {
               (*fn)(a->items[i].left, context);
               (*fn)(a->items[i].right, context);
            }
         }
         else
            item_without_type(mask);
         n++;
      }
   }
}

void type_rewrite_trees(type_t t, unsigned generation,
                        tree_rewrite_fn_t fn, void *context)
{
   if (t == NULL)
      return;

   if (t->generation == generation)
      return;
   else
      t->generation = generation;

   const uint32_t has = has_map[t->kind];
   const int nitems = __builtin_popcount(has);
   uint32_t mask = 1;
   for (int n = 0; n < nitems; mask <<= 1) {
      if (has & mask) {
         if (ITEM_TYPE_ARRAY & mask) {
            type_array_t *a = &(t->items[n].type_array);
            for (unsigned i = 0; i < a->count; i++)
               type_rewrite_trees(a->items[i], generation, fn, context);
         }
         else if (ITEM_TYPE & mask)
            type_rewrite_trees(t->items[n].type, generation, fn, context);
         else if (ITEM_TREE & mask)
            t->items[n].tree = (*fn)(t->items[n].tree, context);
         else if (ITEM_TREE_ARRAY & mask) {
            tree_array_t *a = &(t->items[n].tree_array);
            for (unsigned i = 0; i < a->count; i++)
               a->items[i] = (*fn)(a->items[i], context);
         }
         else if (ITEM_RANGE_ARRAY & mask) {
            range_array_t *a = &(t->items[n].range_array);
            for (unsigned i = 0; i < a->count; i++) {
               a->items[i].left  = (*fn)(a->items[i].left, context);
               a->items[i].right = (*fn)(a->items[i].right, context);
               assert(a->items[i].left);
               assert(a->items[i].right);
            }
         }
         else
            item_without_type(mask);
         n++;
      }
   }
}

const char *type_kind_str(type_kind_t t)
{
   return kind_text_map[t];
}
