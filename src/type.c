//
//  Copyright (C) 2011-2021  Nick Gasson
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
#include "common.h"
#include "object.h"
#include "hash.h"

#include <assert.h>
#include <limits.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <float.h>

static const imask_t has_map[T_LAST_TYPE_KIND] = {
   // T_SUBTYPE
   (I_IDENT | I_BASE | I_RESOLUTION | I_CONSTR),

   // T_INTEGER
   (I_IDENT | I_DIMS),

   // T_REAL
   (I_IDENT | I_DIMS),

   // T_ENUM
   (I_IDENT | I_LITERALS | I_DIMS),

   // T_PHYSICAL
   (I_IDENT | I_UNITS | I_DIMS),

   // T_CARRAY
   (I_IDENT | I_ELEM | I_DIMS),

   // T_UARRAY
   (I_IDENT | I_INDEXCON | I_ELEM),

   // T_RECORD
   (I_IDENT | I_FIELDS),

   // T_FILE
   (I_IDENT | I_FILE),

   // T_ACCESS
   (I_IDENT | I_ACCESS),

   // T_FUNC
   (I_IDENT | I_PTYPES | I_RESULT),

   // T_INCOMPLETE
   (I_IDENT),

   // T_PROC
   (I_IDENT | I_PTYPES),

   // T_NONE
   (I_IDENT),

   // T_PROTECTED
   (I_IDENT | I_DECLS | I_REF)
};

static const char *kind_text_map[T_LAST_TYPE_KIND] = {
   "T_SUBTYPE",    "T_INTEGER",  "T_REAL",     "T_ENUM",
   "T_PHYSICAL",   "T_CARRAY",   "T_UARRAY",   "T_RECORD",
   "T_FILE",       "T_ACCESS",   "T_FUNC",     "T_INCOMPLETE",
   "T_PROC",       "T_NONE",     "T_PROTECTED"
};

static const change_allowed_t change_allowed[] = {
   { -1, -1 }
};

struct _type {
   object_t object;
};

struct _tree {
   object_t object;
};

struct type_rd_ctx {
   tree_rd_ctx_t  tree_ctx;
   ident_rd_ctx_t ident_ctx;
   unsigned       n_types;
   type_t         *store;
   unsigned       store_sz;
};

object_class_t type_object = {
   .name           = "type",
   .change_allowed = change_allowed,
   .has_map        = has_map,
   .kind_text_map  = kind_text_map,
   .tag            = OBJECT_TAG_TYPE,
   .last_kind      = T_LAST_TYPE_KIND
};

static inline tree_t tree_array_nth(item_t *item, unsigned n)
{
   return container_of(AGET(item->obj_array, n), struct _tree, object);
}

static inline void tree_array_add(item_t *item, tree_t t)
{
   APUSH(item->obj_array, &(t->object));
}

static inline type_t type_array_nth(item_t *item, unsigned n)
{
   return container_of(AGET(item->obj_array, n), struct _type, object);
}

static inline void type_array_add(item_t *item, type_t t)
{
   APUSH(item->obj_array, &(t->object));
}

type_t type_new(type_kind_t kind)
{
   return (type_t)object_new(&type_object, kind);
}

type_kind_t type_kind(type_t t)
{
   assert(t != NULL);
   return t->object.kind;
}

static bool _type_eq(type_t a, type_t b, bool strict)
{
   assert(a != NULL);
   assert(b != NULL);

   if (a == b)
      return true;

   type_kind_t kind_a = a->object.kind;
   type_kind_t kind_b = b->object.kind;

   if (!strict) {
      // Subtypes are convertible to the base type
      while ((kind_a = a->object.kind) == T_SUBTYPE)
         a = type_base(a);
      while ((kind_b = b->object.kind) == T_SUBTYPE)
         b = type_base(b);

      if (a == b)
         return true;
   }

   ident_t ai = lookup_item(&type_object, a, I_IDENT)->ident;
   ident_t bi = lookup_item(&type_object, b, I_IDENT)->ident;

   if (ai != bi)
      return false;

   if (kind_a == T_INCOMPLETE || kind_b == T_INCOMPLETE)
      return true;

   const imask_t has = has_map[a->object.kind];

   const bool compare_c_u_arrays =
      (kind_a == T_CARRAY && kind_b == T_UARRAY)
      || (kind_a == T_UARRAY && kind_b == T_CARRAY);

   if (kind_a != kind_b && (!compare_c_u_arrays || strict))
      return false;

   if (has & I_ELEM)
      return _type_eq(type_elem(a), type_elem(b), strict);

   if (kind_a == T_ACCESS)
      return _type_eq(type_access(a), type_access(b), strict);

   if ((has & I_DIMS) && (type_dims(a) != type_dims(b)))
      return false;

   if (type_kind(a) == T_FUNC) {
      if (!_type_eq(type_result(a), type_result(b), strict))
         return false;
   }

   if (has & I_PTYPES) {
      const int nparams = type_params(a);

      if (type_params(b) != nparams)
         return false;

      for (int i = 0; i < nparams; i++) {
         if (!_type_eq(type_param(a, i), type_param(b, i), strict))
             return false;
      }
   }

   return true;
}

bool type_strict_eq(type_t a, type_t b)
{
   return _type_eq(a, b, true);
}

bool type_eq(type_t a, type_t b)
{
   return _type_eq(a, b, false);
}

ident_t type_ident(type_t t)
{
   assert(t != NULL);

   item_t *item = lookup_item(&type_object, t, I_IDENT);
   if (item->ident == NULL) {
      switch (t->object.kind) {
      case T_SUBTYPE:
         return type_ident(type_base(t));

      case T_NONE:
         return ident_new("none");

      default:
         fatal_trace("type kind %s has no ident",
                     type_kind_str(t->object.kind));
      }
   }
   else
      return item->ident;
}

bool type_has_ident(type_t t)
{
   assert(t != NULL);
   return (lookup_item(&type_object, t, I_IDENT)->ident != NULL);
}

void type_set_ident(type_t t, ident_t id)
{
   assert(t != NULL);
   lookup_item(&type_object, t, I_IDENT)->ident = id;
}

unsigned type_dims(type_t t)
{
   return lookup_item(&type_object, t, I_DIMS)->obj_array.count;
}

tree_t type_dim(type_t t, unsigned n)
{
   item_t *item = lookup_item(&type_object, t, I_DIMS);
   return tree_array_nth(item, n);
}

void type_add_dim(type_t t, tree_t r)
{
   tree_array_add(lookup_item(&type_object, t, I_DIMS), r);
}

type_t type_base(type_t t)
{
   item_t *item = lookup_item(&type_object, t, I_BASE);
   assert(item->type != NULL);
   return item->type;
}

void type_set_base(type_t t, type_t b)
{
   lookup_item(&type_object, t, I_BASE)->type = b;
}

type_t type_elem(type_t t)
{
   assert(t != NULL);

   if (t->object.kind == T_SUBTYPE)
      return type_elem(type_base(t));
   else if (t->object.kind == T_NONE)
      return t;
   else {
      item_t *item = lookup_item(&type_object, t, I_ELEM);
      assert(item->type != NULL);
      return item->type;
   }
}

void type_set_elem(type_t t, type_t e)
{
   lookup_item(&type_object, t, I_ELEM)->type = e;
}

static type_t type_make_universal(type_kind_t kind, const char *name,
                                  tree_t min, tree_t max)
{
   type_t t = type_new(kind);
   type_set_ident(t, ident_new(name));

   tree_t r = tree_new(T_RANGE);
   tree_set_subkind(r, RANGE_TO);
   tree_set_left(r, min);
   tree_set_right(r, max);
   tree_set_type(r, t);

   type_add_dim(t, r);

   tree_set_type(min, t);
   tree_set_type(max, t);

   return t;
}

type_t type_universal_int(void)
{
   static type_t t = NULL;

   if (t == NULL) {
      tree_t min = tree_new(T_LITERAL);
      tree_set_subkind(min, L_INT);
      tree_set_ival(min, INT64_MIN);

      tree_t max = tree_new(T_LITERAL);
      tree_set_subkind(max, L_INT);
      tree_set_ival(max, INT64_MAX);

      t = type_make_universal(T_INTEGER, "universal_integer", min, max);
   }

   return t;
}

type_t type_universal_real(void)
{
   static type_t t = NULL;

   if (t == NULL) {
      tree_t min = tree_new(T_LITERAL);
      tree_set_subkind(min, L_REAL);
      tree_set_dval(min, DBL_MIN);

      tree_t max = tree_new(T_LITERAL);
      tree_set_subkind(max, L_REAL);
      tree_set_dval(max, DBL_MAX);

      t = type_make_universal(T_REAL, "universal_real", min, max);
   }

   return t;
}

bool type_is_universal(type_t t)
{
   assert(t != NULL);

   item_t *item = lookup_item(&type_object, t, I_IDENT);
   switch (t->object.kind) {
   case T_INTEGER:
      return item->ident == type_ident(type_universal_int());
   case T_REAL:
      return item->ident == type_ident(type_universal_real());
   default:
      return false;
   }
}

unsigned type_units(type_t t)
{
   return lookup_item(&type_object, t, I_UNITS)->obj_array.count;
}

tree_t type_unit(type_t t, unsigned n)
{
   item_t *item = lookup_item(&type_object, t, I_UNITS);
   return tree_array_nth(item, n);
}

void type_add_unit(type_t t, tree_t u)
{
   tree_array_add(lookup_item(&type_object, t, I_UNITS), u);
}

unsigned type_enum_literals(type_t t)
{
   return lookup_item(&type_object, t, I_LITERALS)->obj_array.count;
}

tree_t type_enum_literal(type_t t, unsigned n)
{
   item_t *item = lookup_item(&type_object, t, I_LITERALS);
   return tree_array_nth(item, n);
}

void type_enum_add_literal(type_t t, tree_t lit)
{
   assert(tree_kind(lit) == T_ENUM_LIT);
   tree_array_add(lookup_item(&type_object, t, I_LITERALS), lit);
}

unsigned type_params(type_t t)
{
   return lookup_item(&type_object, t, I_PTYPES)->obj_array.count;
}

type_t type_param(type_t t, unsigned n)
{
   item_t *item = lookup_item(&type_object, t, I_PTYPES);
   return type_array_nth(item, n);
}

void type_add_param(type_t t, type_t p)
{
   type_array_add(lookup_item(&type_object, t, I_PTYPES), p);
}

unsigned type_fields(type_t t)
{
   if (t->object.kind == T_SUBTYPE)
      return type_fields(type_base(t));
   else
      return lookup_item(&type_object, t, I_FIELDS)->obj_array.count;
}

tree_t type_field(type_t t, unsigned n)
{
   if (t->object.kind == T_SUBTYPE)
      return type_field(type_base(t), n);
   else {
      item_t *item = lookup_item(&type_object, t, I_FIELDS);
      return tree_array_nth(item, n);
   }
}

void type_add_field(type_t t, tree_t p)
{
   assert(p->object.kind == T_FIELD_DECL);
   tree_array_add(lookup_item(&type_object, t, I_FIELDS), p);
}

unsigned type_decls(type_t t)
{
   return lookup_item(&type_object, t, I_DECLS)->obj_array.count;
}

tree_t type_decl(type_t t, unsigned n)
{
   item_t *item = lookup_item(&type_object, t, I_DECLS);
   return tree_array_nth(item, n);
}

void type_add_decl(type_t t, tree_t p)
{
   tree_array_add(lookup_item(&type_object, t, I_DECLS), p);
}

type_t type_result(type_t t)
{
   item_t *item = lookup_item(&type_object, t, I_RESULT);
   assert(item->type != NULL);
   return item->type;
}

void type_set_result(type_t t, type_t r)
{
   lookup_item(&type_object, t, I_RESULT)->type = r;
}

unsigned type_index_constrs(type_t t)
{
   return lookup_item(&type_object, t, I_INDEXCON)->obj_array.count;
}

void type_add_index_constr(type_t t, type_t c)
{
   type_array_add(lookup_item(&type_object, t, I_INDEXCON), c);
}

type_t type_index_constr(type_t t, unsigned n)
{
   item_t *item = lookup_item(&type_object, t, I_INDEXCON);
   return type_array_nth(item, n);
}

void type_set_constraint(type_t t, tree_t c)
{
   lookup_item(&type_object, t, I_CONSTR)->object = &(c->object);
}

bool type_has_constraint(type_t t)
{
   return lookup_item(&type_object, t, I_CONSTR)->object != NULL;
}

tree_t type_constraint(type_t t)
{
   item_t *item = lookup_item(&type_object, t, I_CONSTR);
   assert(item->object != NULL);
   return container_of(item->object, struct _tree, object);
}

void type_set_resolution(type_t t, tree_t r)
{
   lookup_item(&type_object, t, I_RESOLUTION)->object = &(r->object);
}

bool type_has_resolution(type_t t)
{
   return lookup_item(&type_object, t, I_RESOLUTION)->object != NULL;
}

tree_t type_resolution(type_t t)
{
   item_t *item = lookup_item(&type_object, t, I_RESOLUTION);
   assert(item->object != NULL);
   return container_of(item->object, struct _tree, object);
}

type_t type_access(type_t t)
{
   if (t->object.kind == T_SUBTYPE)
      return type_access(type_base(t));
   else
      return lookup_item(&type_object, t, I_ACCESS)->type;
}

void type_set_access(type_t t, type_t a)
{
   lookup_item(&type_object, t, I_ACCESS)->type = a;
}

type_t type_file(type_t t)
{
   return lookup_item(&type_object, t, I_FILE)->type;
}

void type_set_file(type_t t, type_t f)
{
   lookup_item(&type_object, t, I_FILE)->type = f;
}

tree_t type_body(type_t t)
{
   assert(t->object.kind == T_PROTECTED);
   item_t *item = lookup_item(&type_object, t, I_REF);
   assert(item->object);
   return container_of(item->object, struct _tree, object);
}

void type_set_body(type_t t, tree_t b)
{
   assert(t->object.kind == T_PROTECTED);
   item_t *item = lookup_item(&type_object, t, I_REF);
   item->object = &(b->object);
}

bool type_has_body(type_t t)
{
   assert(t->object.kind == T_PROTECTED);
   item_t *item = lookup_item(&type_object, t, I_REF);
   return (item->object != NULL);
}

const char *type_pp2(type_t t, type_t other)
{
   assert(t != NULL);

   switch (type_kind(t)) {
   case T_FUNC:
   case T_PROC:
      {
         static hash_t *cache = NULL;
         if (cache == NULL)
            cache = hash_new(64, true, HASH_PTR);

         text_buf_t *tb = hash_get(cache, t);
         if (tb == NULL) {
            tb = tb_new();
            hash_put(cache, t, tb);

            if (type_has_ident(t)) {
               const char *fname = istr(type_ident(t));
               tb_printf(tb, "%s ", fname);
            }
            tb_printf(tb, "[");
            const int nparams = type_params(t);
            for (int i = 0; i < nparams; i++)
               tb_printf(tb, "%s%s", (i == 0 ? "" : ", "),
                         type_pp(type_param(t, i)));
            if (type_kind(t) == T_FUNC)
               tb_printf(tb, "%sreturn %s", nparams > 0 ? " " : "",
                         type_pp(type_result(t)));
            tb_printf(tb, "]");
         }

         return tb_get(tb);
      }

   default:
      {
         const char *full1 = istr(type_ident(t));
         const char *dot1  = strrchr(full1, '.');
         const char *tail1 = dot1 ? dot1 + 1 : full1;

         if (other != NULL) {
            const char *full2 = istr(type_ident(other));
            const char *dot2  = strrchr(full2, '.');
            const char *tail2 = dot2 ? dot2 + 1 : full2;

            return strcmp(tail1, tail2) ? tail1 : full1;
         }
         else
            return tail1;
      }
   }
}

const char *type_pp(type_t t)
{
   return type_pp2(t, NULL);
}

type_kind_t type_base_kind(type_t t)
{
   assert(t != NULL);
   if (t->object.kind == T_SUBTYPE)
      return type_base_kind(type_base(t));
   else
      return t->object.kind;
}

bool type_is_array(type_t t)
{
   const type_kind_t base = type_base_kind(t);
   return base == T_CARRAY || base == T_UARRAY;
}

bool type_is_record(type_t t)
{
   return type_base_kind(t) == T_RECORD;
}

bool type_is_protected(type_t t)
{
   return type_base_kind(t) == T_PROTECTED;
}

bool type_is_file(type_t t)
{
   return type_base_kind(t) == T_FILE;
}

bool type_is_access(type_t t)
{
   return type_base_kind(t) == T_ACCESS;
}

bool type_is_incomplete(type_t t)
{
   return type_base_kind(t) == T_INCOMPLETE;
}

bool type_is_none(type_t t)
{
   return type_base_kind(t) == T_NONE;
}

bool type_is_unconstrained(type_t t)
{
   assert(t != NULL);
   if (t->object.kind == T_SUBTYPE) {
      if (!type_has_constraint(t))
         return type_is_unconstrained(type_base(t));
      else
         return false;
   }
   else
      return (t->object.kind == T_UARRAY);
}

bool type_is_enum(type_t t)
{
   return type_base_kind(t) == T_ENUM;
}

bool type_is_discrete(type_t t)
{
   const type_kind_t base = type_base_kind(t);
   return base == T_INTEGER || base == T_ENUM;
}

bool type_is_subprogram(type_t t)
{
   return t->object.kind == T_FUNC || t->object.kind == T_PROC;
}

bool type_is_physical(type_t t)
{
   return type_base_kind(t) == T_PHYSICAL;
}

bool type_is_integer(type_t t)
{
   return type_base_kind(t) == T_INTEGER;
}

bool type_is_real(type_t t)
{
   return type_base_kind(t) == T_REAL;
}

bool type_is_scalar(type_t t)
{
   const type_kind_t base = type_base_kind(t);
   return base == T_INTEGER || base == T_REAL
      || base == T_ENUM || base == T_PHYSICAL || base == T_NONE;
}

type_t type_base_recur(type_t t)
{
   assert(t != NULL);
   while (t->object.kind == T_SUBTYPE)
      t = type_base(t);
   return t;
}

const char *type_kind_str(type_kind_t t)
{
   assert(t < T_LAST_TYPE_KIND);
   return kind_text_map[t];
}

bool type_known_width(type_t type)
{
   if (!type_is_array(type))
      return true;

   if (type_is_unconstrained(type))
      return false;

   if (!type_known_width(type_elem(type)))
      return false;

   const int ndims = dimension_of(type);
   for (int i = 0; i < ndims; i++) {
      int64_t low, high;
      if (!folded_bounds(range_of(type, i), &low, &high))
         return false;
   }

   return true;
}

unsigned type_width(type_t type)
{
   if (type_is_array(type)) {
      const unsigned elem_w = type_width(type_elem(type));
      unsigned w = 1;
      const int ndims = dimension_of(type);
      for (int i = 0; i < ndims; i++) {
         int64_t low, high;
         range_bounds(range_of(type, i), &low, &high);
         w *= MAX(high - low + 1, 0);
      }
      return w * elem_w;
   }
   else if (type_is_record(type)) {
      type_t base = type_base_recur(type);
      unsigned w = 0;
      const int nfields = type_fields(base);
      for (int i = 0; i < nfields; i++)
         w += type_width(tree_type(type_field(base, i)));
      return w;
   }
   else
      return 1;
}

bool type_is_convertible(type_t from, type_t to)
{
   // LRM 08 section 9.3.6 final paragraph lists rules for implicit
   // conversion from universal operands to other integer/real types.

   type_kind_t fromk = type_base_kind(from);
   type_kind_t tok   = type_base_kind(to);

   if (fromk == T_NONE)
      return true;  // Suppress cascading errors
   else if (!type_is_universal(from))
      return false;
   else if (type_is_universal(to))
      return false;
   else if (fromk == T_INTEGER && tok == T_INTEGER)
      return true;
   else if (fromk == T_REAL && tok == T_REAL)
      return true;
   else
      return false;
}

bool type_is_composite(type_t t)
{
   const type_kind_t base = type_base_kind(t);
   return base == T_CARRAY || base == T_UARRAY || base == T_RECORD;
}
