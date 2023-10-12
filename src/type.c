//
//  Copyright (C) 2011-2022  Nick Gasson
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
   (I_IDENT | I_BASE | I_RESOLUTION | I_CONSTR | I_ELEM),

   // T_INTEGER
   (I_IDENT | I_DIMS),

   // T_REAL
   (I_IDENT | I_DIMS),

   // T_ENUM
   (I_IDENT | I_LITERALS | I_DIMS),

   // T_PHYSICAL
   (I_IDENT | I_UNITS | I_DIMS),

   // T_ARRAY
   (I_IDENT | I_INDEXES | I_ELEM),

   // T_RECORD
   (I_IDENT | I_FIELDS),

   // T_FILE
   (I_IDENT | I_DESIGNATED),

   // T_ACCESS
   (I_IDENT | I_DESIGNATED),

   // T_FUNC
   (I_IDENT | I_PARAMS | I_RESULT),

   // T_INCOMPLETE
   (I_IDENT),

   // T_PROC
   (I_IDENT | I_PARAMS),

   // T_NONE
   (I_IDENT),

   // T_PROTECTED
   (I_IDENT | I_FIELDS),

   // T_GENERIC
   (I_IDENT | I_SUBKIND | I_DESIGNATED | I_INDEXES | I_ELEM),

   // T_VIEW
   (I_IDENT | I_DESIGNATED | I_FIELDS),
};

static const char *kind_text_map[T_LAST_TYPE_KIND] = {
   "T_SUBTYPE",    "T_INTEGER",   "T_REAL",       "T_ENUM",
   "T_PHYSICAL",   "T_ARRAY",     "T_RECORD",     "T_FILE",
   "T_ACCESS",     "T_FUNC",      "T_INCOMPLETE", "T_PROC",
   "T_NONE",       "T_PROTECTED", "T_GENERIC",    "T_VIEW",
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

object_class_t type_object = {
   .name           = "type",
   .change_allowed = change_allowed,
   .has_map        = has_map,
   .kind_text_map  = kind_text_map,
   .tag            = OBJECT_TAG_TYPE,
   .last_kind      = T_LAST_TYPE_KIND
};

extern object_arena_t *global_arena;

static inline tree_t tree_array_nth(item_t *item, unsigned n)
{
   object_t *o = obj_array_nth(item->obj_array, n);
   return container_of(o, struct _tree, object);
}

static inline void tree_array_add(item_t *item, tree_t t)
{
   obj_array_add(&(item->obj_array), &(t->object));
}

static inline type_t type_array_nth(item_t *item, unsigned n)
{
   object_t *o = obj_array_nth(item->obj_array, n);
   return container_of(o, struct _type, object);
}

static inline void type_array_add(item_t *item, type_t t)
{
   obj_array_add(&(item->obj_array), &(t->object));
}

type_t type_new(type_kind_t kind)
{
   object_t *o = object_new(NULL, &type_object, kind);
   return container_of(o, struct _type, object);
}

type_kind_t type_kind(type_t t)
{
   assert(t != NULL);
   return t->object.kind;
}

static inline type_t type_base_map(type_t t, hash_t *map)
{
   assert(t->object.kind == T_SUBTYPE);
   type_t base = type_base(t);
   if (map != NULL)
      return hash_get(map, base) ?: base;
   else
      return base;
}

static bool _type_eq(type_t a, type_t b, bool strict, hash_t *map)
{
   assert(a != NULL);
   assert(b != NULL);

   if (a == b)
      return true;

   type_kind_t kind_a = a->object.kind;
   type_kind_t kind_b = b->object.kind;

   if (map != NULL) {
      if (kind_a == T_GENERIC) {
         a = hash_get(map, a) ?: a;
         kind_a = a->object.kind;
      }

      if (kind_b == T_GENERIC) {
         b = hash_get(map, b) ?: b;
         kind_b = b->object.kind;
      }

      if (a == b)
         return true;
   }

   if (!strict) {
      // Subtypes are convertible to the base type
      while ((kind_a = a->object.kind) == T_SUBTYPE)
         a = type_base_map(a, map);
      while ((kind_b = b->object.kind) == T_SUBTYPE)
         b = type_base_map(b, map);

      if (a == b)
         return true;
   }

   const imask_t has = has_map[a->object.kind];

   if (!(has & I_PARAMS)) {
      ident_t ai = lookup_item(&type_object, a, I_IDENT)->ident;
      ident_t bi = lookup_item(&type_object, b, I_IDENT)->ident;

      if (ai != bi)
         return false;
   }

   if (kind_a == T_INCOMPLETE || kind_b == T_INCOMPLETE)
      return true;

   if (kind_a != kind_b)
      return false;

   if (kind_a == T_ARRAY)
      return _type_eq(type_elem(a), type_elem(b), strict, map);

   if (kind_a == T_ACCESS)
      return _type_eq(type_designated(a), type_designated(b), strict, map);

   if ((has & I_DIMS) && (type_dims(a) != type_dims(b)))
      return false;

   if (kind_a == T_FUNC) {
      if (!_type_eq(type_result(a), type_result(b), strict, map))
         return false;
   }

   if (has & I_PARAMS) {
      item_t *ap = lookup_item(&type_object, a, I_PARAMS);
      item_t *bp = lookup_item(&type_object, b, I_PARAMS);

      const int acount = obj_array_count(ap->obj_array);
      const int bcount = obj_array_count(bp->obj_array);

      if (acount != bcount)
         return false;

      for (int i = 0; i < acount; i++) {
         type_t ai = type_array_nth(ap, i);
         type_t bi = type_array_nth(bp, i);
         if (ai != bi && !_type_eq(ai, bi, strict, map))
            return false;
      }
   }

   return true;
}

bool type_strict_eq(type_t a, type_t b)
{
   return _type_eq(a, b, true, NULL);
}

bool type_eq(type_t a, type_t b)
{
   return _type_eq(a, b, false, NULL);
}

bool type_eq_map(type_t a, type_t b, hash_t *map)
{
   return _type_eq(a, b, false, map);
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
      case T_GENERIC:
         return ident_new("anonymous");
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
   item_t *item = lookup_item(&type_object, t, I_DIMS);
   return obj_array_count(item->obj_array);
}

tree_t type_dim(type_t t, unsigned n)
{
   item_t *item = lookup_item(&type_object, t, I_DIMS);
   return tree_array_nth(item, n);
}

void type_add_dim(type_t t, tree_t r)
{
   tree_array_add(lookup_item(&type_object, t, I_DIMS), r);
   object_write_barrier(&(t->object), &(r->object));
}

type_t type_base(type_t t)
{
   item_t *item = lookup_item(&type_object, t, I_BASE);
   assert(item->object != NULL);
   return container_of(item->object, struct _type, object);
}

void type_set_base(type_t t, type_t b)
{
   lookup_item(&type_object, t, I_BASE)->object = &(b->object);
   object_write_barrier(&(t->object), &(b->object));
}

type_t type_elem(type_t t)
{
   assert(t != NULL);

   if (t->object.kind == T_NONE)
      return t;
   else {
      item_t *item = lookup_item(&type_object, t, I_ELEM);
      if (t->object.kind == T_SUBTYPE && item->object == NULL)
         return type_elem(type_base(t));
      else {
         assert(item->object != NULL);
         return container_of(item->object, struct _type, object);
      }
   }
}

void type_set_elem(type_t t, type_t e)
{
   lookup_item(&type_object, t, I_ELEM)->object = &(e->object);
   object_write_barrier(&(t->object), &(e->object));
}

bool type_has_elem(type_t t)
{
   return lookup_item(&type_object, t, I_ELEM)->object != NULL;
}

unsigned type_subkind(type_t t)
{
   item_t *item = lookup_item(&type_object, t, I_SUBKIND);
   return item->ival;
}

void type_set_subkind(type_t t, unsigned sub)
{
   lookup_item(&type_object, t, I_SUBKIND)->ival = sub;
}

bool type_is_universal(type_t t)
{
   assert(t != NULL);

   switch (t->object.kind) {
   case T_INTEGER:
      return t == std_type(NULL, STD_UNIVERSAL_INTEGER);
   case T_REAL:
      return t == std_type(NULL, STD_UNIVERSAL_REAL);
   default:
      return false;
   }
}

unsigned type_units(type_t t)
{
   item_t *item = lookup_item(&type_object, t, I_UNITS);
   return obj_array_count(item->obj_array);
}

tree_t type_unit(type_t t, unsigned n)
{
   item_t *item = lookup_item(&type_object, t, I_UNITS);
   return tree_array_nth(item, n);
}

void type_add_unit(type_t t, tree_t u)
{
   tree_array_add(lookup_item(&type_object, t, I_UNITS), u);
   object_write_barrier(&(t->object), &(u->object));
}

unsigned type_enum_literals(type_t t)
{
   item_t *item = lookup_item(&type_object, t, I_LITERALS);
   return obj_array_count(item->obj_array);
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
   object_write_barrier(&(t->object), &(lit->object));
}

unsigned type_params(type_t t)
{
   item_t *item = lookup_item(&type_object, t, I_PARAMS);
   return obj_array_count(item->obj_array);
}

type_t type_param(type_t t, unsigned n)
{
   item_t *item = lookup_item(&type_object, t, I_PARAMS);
   return type_array_nth(item, n);
}

void type_add_param(type_t t, type_t p)
{
   type_array_add(lookup_item(&type_object, t, I_PARAMS), p);
   object_write_barrier(&(t->object), &(p->object));
}

unsigned type_fields(type_t t)
{
   if (t->object.kind == T_SUBTYPE)
      return type_fields(type_base(t));
   else {
      item_t *item = lookup_item(&type_object, t, I_FIELDS);
      return obj_array_count(item->obj_array);
   }
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
   tree_array_add(lookup_item(&type_object, t, I_FIELDS), p);
   object_write_barrier(&(t->object), &(p->object));
}

type_t type_result(type_t t)
{
   item_t *item = lookup_item(&type_object, t, I_RESULT);
   assert(item->object != NULL);
   return container_of(item->object, struct _type, object);
}

void type_set_result(type_t t, type_t r)
{
   lookup_item(&type_object, t, I_RESULT)->object = &(r->object);
   object_write_barrier(&(t->object), &(r->object));
}

unsigned type_indexes(type_t t)
{
   item_t *item = lookup_item(&type_object, t, I_INDEXES);
   return obj_array_count(item->obj_array);
}

void type_add_index(type_t t, type_t sub)
{
   type_array_add(lookup_item(&type_object, t, I_INDEXES), sub);
   object_write_barrier(&(t->object), &(sub->object));
}

type_t type_index(type_t t, unsigned n)
{
   item_t *item = lookup_item(&type_object, t, I_INDEXES);
   return type_array_nth(item, n);
}

unsigned type_constraints(type_t t)
{
   item_t *item = lookup_item(&type_object, t, I_CONSTR);
   return obj_array_count(item->obj_array);
}

void type_add_constraint(type_t t, tree_t c)
{
   assert(c->object.kind == T_CONSTRAINT);
   tree_array_add(lookup_item(&type_object, t, I_CONSTR), c);
   object_write_barrier(&(t->object), &(c->object));
}

tree_t type_constraint(type_t t, unsigned n)
{
   assert(n == 0);    // TODO: this list is largely redundant now
   item_t *item = lookup_item(&type_object, t, I_CONSTR);
   return tree_array_nth(item, n);
}

void type_set_resolution(type_t t, tree_t r)
{
   lookup_item(&type_object, t, I_RESOLUTION)->object = &(r->object);
   object_write_barrier(&(t->object), &(r->object));
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

type_t type_designated(type_t t)
{
   if (t->object.kind == T_SUBTYPE)
      return type_designated(type_base(t));
   else {
      item_t *item = lookup_item(&type_object, t, I_DESIGNATED);
      assert(item->object != NULL);
      return container_of(item->object, struct _type, object);
   }
}

void type_set_designated(type_t t, type_t d)
{
   lookup_item(&type_object, t, I_DESIGNATED)->object = &(d->object);
   object_write_barrier(&(t->object), &(d->object));
}

void type_signature(type_t t, text_buf_t *tb)
{
   assert(t->object.kind == T_FUNC || t->object.kind == T_PROC);

   tb_printf(tb, "[");
   const int nparams = type_params(t);
   for (int i = 0; i < nparams; i++)
      tb_printf(tb, "%s%s", (i == 0 ? "" : ", "),
                type_pp(type_param(t, i)));
   if (t->object.kind == T_FUNC)
      tb_printf(tb, "%sreturn %s", nparams > 0 ? " " : "",
                type_pp(type_result(t)));
   tb_printf(tb, "]");
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
            cache = hash_new(64);

         text_buf_t *tb = hash_get(cache, t);
         if (tb == NULL) {
            tb = tb_new();
            hash_put(cache, t, tb);

            if (type_has_ident(t)) {
               tb_istr(tb, type_ident(t));
               tb_append(tb, ' ');
            }
            type_signature(t, tb);
         }

         return tb_get(tb);
      }

   case T_GENERIC:
      if (!type_has_ident(t))
         return "(an anonymous type)";
      // Fall-through

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
   if (base == T_GENERIC)
      return type_subkind(type_base_recur(t)) == GTYPE_ARRAY;
   else
      return base == T_ARRAY;
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
   const type_kind_t base = type_base_kind(t);
   if (base == T_GENERIC)
      return type_subkind(type_base_recur(t)) == GTYPE_FILE;
   else
      return base == T_FILE;
}

bool type_is_access(type_t t)
{
   const type_kind_t base = type_base_kind(t);
   if (base == T_GENERIC)
      return type_subkind(type_base_recur(t)) == GTYPE_ACCESS;
   else
      return base == T_ACCESS;
}

bool type_is_incomplete(type_t t)
{
   return type_base_kind(t) == T_INCOMPLETE;
}

bool type_is_none(type_t t)
{
   return type_base_kind(t) == T_NONE;
}

bool type_is_valid(type_t t)
{
   return type_base_kind(t) != T_NONE;
}

tree_t type_constraint_for_field(type_t t, tree_t f)
{
   if (t->object.kind == T_SUBTYPE) {
      const int ncon = type_constraints(t);
      if (ncon > 0) {
         tree_t c = type_constraint(t, ncon - 1);

         if (tree_subkind(c) != C_RECORD)
            return NULL;

         const int nelem = tree_ranges(c);
         for (int i = 0; i < nelem; i++) {
            tree_t ei = tree_range(c, i);
            assert(tree_kind(ei) == T_ELEM_CONSTRAINT);

            if (tree_has_ref(ei) && tree_ref(ei) == f)
               return ei;
         }
      }

      return type_constraint_for_field(type_base(t), f);
   }
   else
      return NULL;
}

bool type_is_unconstrained(type_t t)
{
   assert(t != NULL);

   if (t->object.kind == T_SUBTYPE) {
      if (type_is_record(t)) {
         if (standard() >= STD_08) {
            const int nfields = type_fields(t);
            for (int i = 0; i < nfields; i++) {
               tree_t f = type_field(t, i);
               if (type_is_unconstrained(tree_type(f))
                   && type_constraint_for_field(t, f) == NULL)
                  return true;
            }
         }

         return false;
      }
      else if (type_is_array(t)) {
         if (standard() >= STD_08) {
            if (type_is_array(t)) {
               type_t elem = type_elem(t);
               if (type_is_unconstrained(elem))
                  return true;
            }
         }

         for (; t->object.kind == T_SUBTYPE; t = type_base(t)) {
            if (type_constraints(t) > 0) {
               tree_t c = type_constraint(t, 0);
               if (tree_subkind(c) == C_INDEX)
                  return false;
            }
         }

         assert(t->object.kind == T_ARRAY);
         return true;
      }
      else
         return false;
   }
   else if (t->object.kind == T_ARRAY)
      return true;
   else if (t->object.kind == T_GENERIC && type_subkind(t) == GTYPE_ARRAY)
      return true;
   else if (t->object.kind == T_RECORD && standard() >= STD_08) {
      const int nfields = type_fields(t);
      for (int i = 0; i < nfields; i++) {
         if (type_is_unconstrained(tree_type(type_field(t, i))))
            return true;
      }
      return false;
   }
   else
      return false;
}

bool type_is_enum(type_t t)
{
   return type_base_kind(t) == T_ENUM;
}

bool type_is_discrete(type_t t)
{
   const type_kind_t base = type_base_kind(t);
   if (base == T_GENERIC) {
      const gtype_class_t class = type_subkind(type_base_recur(t));
      return class == GTYPE_INTEGER || class == GTYPE_DISCRETE;
   }
   else
      return base == T_INTEGER || base == T_ENUM;
}

bool type_is_subprogram(type_t t)
{
   return t->object.kind == T_FUNC || t->object.kind == T_PROC;
}

bool type_is_physical(type_t t)
{
   const type_kind_t base = type_base_kind(t);
   if (base == T_GENERIC)
      return type_subkind(type_base_recur(t)) == GTYPE_PHYSICAL;
   else
      return base == T_PHYSICAL;
}

bool type_is_integer(type_t t)
{
   const type_kind_t base = type_base_kind(t);
   if (base == T_GENERIC)
      return type_subkind(type_base_recur(t)) == GTYPE_INTEGER;
   else
      return base == T_INTEGER;
}

bool type_is_real(type_t t)
{
   const type_kind_t base = type_base_kind(t);
   if (base == T_GENERIC)
      return type_subkind(type_base_recur(t)) == GTYPE_FLOATING;
   else
      return base == T_REAL;
}

bool type_is_generic(type_t t)
{
   return type_base_kind(t) == T_GENERIC;
}

bool type_is_scalar(type_t t)
{
   const type_kind_t base = type_base_kind(t);
   if (base == T_GENERIC) {
      const gtype_class_t class = type_subkind(type_base_recur(t));
      return class == GTYPE_SCALAR || class == GTYPE_DISCRETE
         || class == GTYPE_FLOATING || class == GTYPE_INTEGER;
   }
   else
      return base == T_INTEGER || base == T_REAL
         || base == T_ENUM || base == T_PHYSICAL || base == T_NONE;
}

bool type_is_representable(type_t t)
{
   if (type_is_scalar(t))
      return true;
   else if (standard() < STD_19)
      return false;
   else if (type_is_record(t)) {
      const int nfields = type_fields(t);
      for (int i = 0; i < nfields; i++) {
         if (!type_is_representable(tree_type(type_field(t, i))))
            return false;
      }

      return true;
   }
   else if (type_is_array(t))
      return type_is_representable(type_elem(t));
   else
      return false;
}

bool type_const_bounds(type_t t)
{
   if (type_is_unconstrained(t))
      return false;
   else if (type_is_record(t)) {
      const int nfields = type_fields(t);
      for (int i = 0; i < nfields; i++) {
         type_t ftype = tree_type(type_field(t, i));
         if (!type_const_bounds(ftype))
            return false;
      }

      return true;
   }
   else if (type_is_array(t)) {
      const int ndims = dimension_of(t);
      for (int i = 0; i < ndims; i++) {
         int64_t low, high;
         if (!folded_bounds(range_of(t, i), &low, &high))
            return false;
      }

      return type_const_bounds(type_elem(t));
   }
   else
      return true;
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

bool type_is_convertible_map(type_t from, type_t to, hash_t *map)
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
   else if (tok == T_GENERIC && map != NULL) {
      type_t to_map = hash_get(map, to);
      return to_map ? type_is_convertible_map(from, to_map, map) : false;
   }
   else if (tok == T_GENERIC) {
      // Handle VHDL-2019 anonymous type classes
      return (fromk == T_INTEGER && type_subkind(to) == GTYPE_INTEGER)
         || (fromk == T_REAL && type_subkind(to) == GTYPE_FLOATING);
   }
   else
      return false;
}

bool type_is_convertible(type_t from, type_t to)
{
   return type_is_convertible_map(from, to, NULL);
}

bool type_is_composite(type_t t)
{
   const type_kind_t base = type_base_kind(t);
   return base == T_ARRAY || base == T_RECORD;
}

bool type_is_homogeneous(type_t t)
{
   if (type_is_scalar(t))
      return true;
   else if (type_is_array(t))
      return type_is_homogeneous(type_elem(t));
   else
      return false;
}

bool type_is_resolved(type_t t)
{
   if (t->object.kind == T_SUBTYPE)
      return type_has_resolution(t) || type_is_resolved(type_base(t));
   else
      return false;
}

bool type_frozen(type_t t)
{
   return arena_frozen(object_arena(&(t->object)));
}

tree_t type_container(type_t t)
{
   object_t *o = arena_root(object_arena(&(t->object)));
   assert(o->tag == OBJECT_TAG_TREE);
   return container_of(o, struct _tree, object);
}

object_t *type_to_object(type_t t)
{
   return t ? &(t->object) : NULL;
}

type_t type_from_object(object_t *obj)
{
   assert(obj->tag == OBJECT_TAG_TYPE);
   return container_of(obj, struct _type, object);
}

int type_bit_width(type_t type)
{
   switch (type_kind(type)) {
   case T_INTEGER:
   case T_PHYSICAL:
      {
         tree_t r = range_of(type, 0);
         return bits_for_range(assume_int(tree_left(r)),
                               assume_int(tree_right(r)));
      }

   case T_REAL:
       // All real types are doubles at the moment
       return 64;

   case T_SUBTYPE:
      return type_bit_width(type_base(type));

   case T_ENUM:
      return bits_for_range(0, type_enum_literals(type) - 1);

   case T_ARRAY:
      return type_bit_width(type_elem(type));

   default:
      fatal_trace("unhandled type %s in type_bit_width", type_pp(type));
   }
}

int type_byte_width(type_t type)
{
   return (type_bit_width(type) + 7) / 8;
}

bool type_is_character_array(type_t t)
{
   // LRM 93 section 3.1.1 an enumeration type is a character type if at
   // least one of its enumeration literals is a character literal

   if (!type_is_array(t))
      return false;

   if (dimension_of(t) != 1)
      return false;

   type_t elem = type_base_recur(type_elem(t));

   if (!type_is_enum(elem))
      return false;

   const int nlits = type_enum_literals(elem);
   for (int i = 0; i < nlits; i++) {
      tree_t lit = type_enum_literal(elem, i);
      if (ident_char(tree_ident(lit), 0) == '\'')
         return true;
   }

   return false;
}

bool type_matches_class(type_t t, gtype_class_t class)
{
   switch (class) {
   case GTYPE_PRIVATE:
      return true;
   case GTYPE_SCALAR:
      return type_is_scalar(t);
   case GTYPE_DISCRETE:
      return type_is_discrete(t);
   case GTYPE_INTEGER:
      return type_is_integer(t);
   case GTYPE_FLOATING:
      return type_is_real(t);
   case GTYPE_PHYSICAL:
      return type_is_physical(t);
   case GTYPE_ACCESS:
      return type_is_access(t);
   case GTYPE_ARRAY:
      return type_is_array(t);
   case GTYPE_FILE:
      return type_is_file(t);
   default:
      return false;
   }
}
