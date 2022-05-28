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

#include "phase.h"
#include "util.h"
#include "common.h"
#include "type.h"

#include <assert.h>
#include <string.h>
#include <stdarg.h>
#include <stdlib.h>
#include <inttypes.h>

static void bounds_check_assignment(tree_t target, tree_t value);

typedef struct interval interval_t;

struct interval {
   int64_t     low;
   int64_t     high;
   interval_t *next;
};

#define bounds_error(t, ...) \
   do { error_at(tree_loc(t), __VA_ARGS__); } while (0)

static void bounds_check_string_literal(tree_t t)
{
   type_t type = tree_type(t);
   if (type_is_unconstrained(type)) {
      // Construct a new array type: the direction and bounds are the same
      // as those for a positional array aggregate

      type_t tmp = type_new(T_SUBTYPE);
      type_set_ident(tmp, type_ident(type));
      type_set_base(tmp, type);

      type_t index_type = index_type_of(type, 0);
      const bool is_enum = type_is_enum(index_type);

      // The direction is determined by the index type
      range_kind_t dir = direction_of(index_type, 0);

      // The left bound is the left of the index type and the right bound
      // is determined by the number of elements

      tree_t left = NULL, right = NULL;

      if (is_enum)
         left = make_ref(type_enum_literal(index_type, 0));
      else
         left = tree_left(range_of(index_type, 0));

      int64_t iright;
      if (dir == RANGE_DOWNTO)
         iright = assume_int(left) - tree_chars(t) + 1;
      else
         iright = assume_int(left) + tree_chars(t) - 1;

      if (is_enum)
         right = get_enum_lit(t, index_type, iright);
      else
         right = get_int_lit(t, index_type, iright);

      tree_t r = tree_new(T_RANGE);
      tree_set_subkind(r, dir);
      tree_set_left(r, left);
      tree_set_right(r, right);
      tree_set_loc(r, tree_loc(t));

      tree_t c = tree_new(T_CONSTRAINT);
      tree_set_subkind(c, C_INDEX);
      tree_add_range(c, r);
      tree_set_loc(c, tree_loc(t));

      type_add_constraint(tmp, c);

      tree_set_type(t, tmp);
   }
   else {
      int64_t expect;
      if (folded_length(range_of(type, 0), &expect) && expect != tree_chars(t))
         bounds_error(t, "expected %"PRIi64" elements in string literal but "
                      "have %d", expect, tree_chars(t));
   }
}

static void bounds_check_literal(tree_t t)
{
   switch (tree_subkind(t)) {
   case L_STRING:
      bounds_check_string_literal(t);
      break;
   default:
      break;
   }
}

static void bounds_fmt_type_range(text_buf_t *tb, type_t type, range_kind_t dir,
                                  int64_t low, int64_t high)
{
   if (dir == RANGE_DOWNTO) {
      to_string(tb, type, high);
      tb_cat(tb, " downto ");
      to_string(tb, type, low);
   }
   else {
      to_string(tb, type, low);
      tb_cat(tb, " to ");
      to_string(tb, type, high);
   }
}

static void bounds_check_scalar(tree_t value, type_t type, tree_t hint)
{
   tree_t r = range_of(type, 0);

   bool error = false;
   int64_t low, high, folded;
   double rlow, rhigh;
   if (folded_bounds(r, &low, &high)) {
      unsigned folded_u;
      if (folded_int(value, &folded))
         error = (folded < low || folded > high);
      else if (folded_enum(value, &folded_u)) {
         error  = (folded_u < low || folded_u > high);
         folded = folded_u;
      }
   }
   else if (folded_bounds_real(r, &rlow, &rhigh)) {
      double folded_f;
      if (folded_real(value, &folded_f)) {
         error  = (folded_f < rlow || folded_f > rhigh);
         folded = FLOAT_BITS(folded_f);
         low    = FLOAT_BITS(rlow);
         high   = FLOAT_BITS(rhigh);
      }
   }

   if (error) {
      LOCAL_TEXT_BUF tb = tb_new();
      tb_cat(tb, "value ");
      to_string(tb, type, folded);
      tb_printf(tb, " outside of %s range ", type_pp(type));
      bounds_fmt_type_range(tb, type, tree_subkind(r), low, high);

      if (hint != NULL) {
         const char *what = "";
         switch (tree_kind(hint)) {
         case T_VAR_DECL:
         case T_SIGNAL_DECL:
         case T_CONST_DECL:
         case T_REF:          what = class_str(class_of(hint)); break;
         case T_PARAM_DECL:   what = "parameter"; break;
         case T_PORT_DECL:    what = "port"; break;
         case T_GENERIC_DECL: what = "generic"; break;
         default: break;
         }

         tb_printf(tb, " for %s %s", what, istr(tree_ident(hint)));
      }

      bounds_error(value, "%s", tb_get(tb));
   }
}

static const char *bounds_dimension_str(int ndims, int dim)
{
   static char dimstr[32];
   if (ndims > 1)
      checked_sprintf(dimstr, sizeof(dimstr), " for dimension %d", dim + 1);
   else
      dimstr[0] = '\0';
   return dimstr;
}

static tree_t bounds_check_call_args(tree_t t)
{
   tree_t decl = tree_ref(t);

   const int nparams = tree_params(t);
   const int nports  = tree_ports(decl);

   for (int i = 0; (i < nparams) && (i < nports); i++) {
      tree_t param = tree_param(t, i);
      assert(tree_subkind(param) == P_POS);

      tree_t value = tree_value(param);
      tree_t port  = tree_port(decl, tree_pos(param));

      type_t ftype = tree_type(port);
      type_t atype = tree_type(tree_value(param));

      if (type_is_array(ftype)) {
         // Check bounds of constrained array parameters

         if ((type_is_unconstrained(atype)) || (type_is_unconstrained(ftype)))
            continue;

         const int ndims = dimension_of(ftype);

         for (int j = 0; j < ndims; j++) {
            tree_t formal_r = range_of(ftype, j);
            tree_t actual_r = range_of(atype, j);

            int64_t f_len, a_len;

            const bool folded =
               folded_length(formal_r, &f_len)
               && folded_length(actual_r, &a_len);

            if (!folded)
               continue;

            if (f_len != a_len)
               bounds_error(param, "actual length %"PRIi64"%s does not match "
                            "formal length %"PRIi64" for parameter %s",
                            a_len, bounds_dimension_str(ndims, j),
                            f_len, istr(tree_ident(port)));
         }
      }
      else if (type_is_scalar(ftype))
         bounds_check_scalar(value, ftype, port);
   }

   return t;
}

static bool bounds_check_index(tree_t index, tree_t ref, type_t type,
                               range_kind_t kind, const char *what,
                               int64_t low, int64_t high)
{
   int64_t folded;
   unsigned folded_u;
   if (folded_int(index, &folded)) {
      if (folded < low || folded > high) {
         LOCAL_TEXT_BUF tb = tb_new();
         tb_cat(tb, what);
         if (ref != NULL && tree_kind(ref) == T_REF)
            tb_printf(tb, " %s", istr(tree_ident(ref)));
         tb_printf(tb, " index %"PRIi64" outside of %s range ",
                   folded, type_pp(type));
         bounds_fmt_type_range(tb, type, kind, low, high);

         bounds_error(index, "%s", tb_get(tb));
         return false;
      }
   }
   else if (folded_enum(index, &folded_u)) {
      if (folded_u < low || folded_u > high) {
         type_t base = type_base_recur(tree_type(index));
         tree_t value_lit = type_enum_literal(base, folded_u);

         LOCAL_TEXT_BUF tb = tb_new();
         tb_cat(tb, what);
         if (ref != NULL && tree_kind(ref) == T_REF)
            tb_printf(tb, " %s", istr(tree_ident(ref)));
         tb_printf(tb, " index %s outside of %s range ",
                   istr(tree_ident(value_lit)), type_pp(type));
         bounds_fmt_type_range(tb, type, kind, low, high);

         bounds_error(index, "%s", tb_get(tb));
         return false;
      }
   }

   return true;
}

static void bounds_check_array_ref(tree_t t)
{
   tree_t value = tree_value(t);

   if (!tree_has_type(value))
      return;
   else if (tree_flags(t) & TREE_F_ELIDE_BOUNDS)
      return;

   type_t value_type = tree_type(value);

   const bool unconstrained = type_is_unconstrained(value_type);
   const bool value_is_ref = tree_kind(value) == T_REF;

   int nstatic = 0;
   const int nparams = tree_params(t);
   for (int i = 0; i < nparams; i++) {
      tree_t p = tree_param(t, i);
      tree_t pvalue = tree_value(p);
      bool checked = false;

      if (!unconstrained) {
         tree_t r = range_of(value_type, i);
         type_t index_type = index_type_of(value_type, i);
         const range_kind_t dir = tree_subkind(r);

         int64_t ivalue;
         checked = folded_int(pvalue, &ivalue);

         int64_t low, high;
         if (folded_bounds(r, &low, &high))
            checked &= bounds_check_index(pvalue, value, index_type, dir,
                                          "array", low, high);
      }

      if (value_is_ref && nparams == 1 && tree_kind(pvalue) == T_REF) {
         // Automatically elide bounds check in cases like
         //
         //   for i in x'range loop
         //     y := a(x);  -- Always in bounds

         type_t ptype = tree_type(pvalue);
         if (type_kind(ptype) == T_SUBTYPE && type_constraints(ptype) == 1) {
            tree_t c = type_constraint(ptype, 0);
            if (tree_subkind(c) == C_RANGE) {
               tree_t r = tree_range(c, 0);
               if (tree_subkind(r) == RANGE_EXPR) {
                  tree_t r_attr = tree_value(r);
                  assert(tree_kind(r_attr) == T_ATTR_REF);
                  tree_t r_base = tree_name(r_attr);
                  assert(tree_kind(r_base) == T_REF);

                  if (tree_ref(r_base) == tree_ref(value))
                     checked = true;
               }
            }
         }
      }

      if (checked)
         nstatic++;
   }

   if (nstatic == nparams)
      tree_set_flag(t, TREE_F_ELIDE_BOUNDS);
}

static void bounds_check_array_slice(tree_t t)
{
   tree_t value = tree_value(t);

   if (!tree_has_type(value))
      return;

   type_t value_type = tree_type(value);

   if (type_is_unconstrained(value_type))
      return;

   tree_t b = range_of(value_type, 0);
   tree_t r = tree_range(t, 0);

   int64_t blow, bhigh;
   if (!folded_bounds(b, &blow, &bhigh))
      return;

   int64_t rlow, rhigh;
   if (!folded_bounds(r, &rlow, &rhigh))
      return;

   if (rlow > rhigh)
      return;  // Null range

   type_t index_type = index_type_of(value_type, 0);
   const range_kind_t dir = tree_subkind(b);

   bounds_check_index(tree_left(r), value, index_type, dir, "array", blow, bhigh);
   bounds_check_index(tree_right(r), value, index_type, dir, "array", blow, bhigh);
}

static void bounds_cover_choice(interval_t **isp, tree_t t, type_t type,
                                int64_t low, int64_t high)
{
   interval_t *it, *prev;
   for (it = *isp, prev = NULL;
        (it != NULL) && (it->low <= high);
        prev = it, it = it->next) {

      if ((low <= it->high) && (it->low <= high)) {
         const int64_t rlow  = MAX(low, it->low);
         const int64_t rhigh = MIN(high, it->high);
         if (type_is_integer(type)) {
            if (rlow == rhigh)
               bounds_error(t, "value %"PRIi64" is already covered", rlow);
            else
               bounds_error(t, "range %"PRIi64" to %"PRIi64" is already covered",
                            rlow, rhigh);
         }
         else if (type_is_enum(type)) {
            type_t base = type_base_recur(type);
            if (rlow == rhigh)
               bounds_error(t, "duplicate choice for %s",
                            istr(tree_ident(type_enum_literal(base, rlow))));
            else
               bounds_error(t, "duplicate choices for range %s to %s",
                            istr(tree_ident(type_enum_literal(base, rlow))),
                            istr(tree_ident(type_enum_literal(base, rhigh))));
         }
         it->low = MIN(low, it->low);
         it->high = MAX(high, it->high);
         return;
      }
      else if (high == it->low - 1) {
         it->low = low;
         return;
      }
      else if (low == it->high + 1) {
         it->high = high;
         return;
      }
   }

   interval_t *new = xmalloc(sizeof(interval_t));
   new->low  = low;
   new->high = high;

   if ((*isp == NULL) || (prev == NULL)) {
      new->next = *isp;
      *isp = new;
   }
   else {
      new->next = prev->next;
      prev->next = new;
   }
}

static void bounds_fmt_interval(text_buf_t *tb, type_t type, range_kind_t dir,
                                int64_t low, int64_t high)
{
   if (low == high) {
      if (type_is_integer(type))
         tb_printf(tb, "%"PRIi64, low);
      else if (type_is_enum(type)) {
         type_t base = type_base_recur(type);
         tb_cat(tb, istr(tree_ident(type_enum_literal(base, low))));
      }
   }
   else
      bounds_fmt_type_range(tb, type, dir, low, high);
}

static void bounds_check_missing_choices(tree_t t, type_t type,
                                         type_t index_type, range_kind_t dir,
                                         int64_t tlow, int64_t thigh,
                                         interval_t *covered)
{
   int missing = 0;
   interval_t *it;
   int64_t walk;

   for (it = covered, walk = tlow; it != NULL; it = it->next) {
      if (it->low != walk)
         missing += it->low - walk;
      walk = it->high + 1;
   }

   if (walk != thigh + 1)
      missing += (thigh + 1 - walk);

   if (missing == 0)
      return;

   LOCAL_TEXT_BUF tb = tb_new();
   tb_printf(tb, "missing choice%s for element%s ",
             missing > 1 ? "s" : "", missing > 1 ? "s" : "");

   int printed = 0;
   for (it = covered, walk = tlow; it != NULL; it = it->next) {
      if (it->low != walk) {
         if (printed++) tb_cat(tb, ", ");
         bounds_fmt_interval(tb, index_type ?: type, dir, walk, it->low - 1);
      }

      walk = it->high + 1;
   }

   if (walk != thigh + 1) {
      if (printed++) tb_cat(tb, ", ");
      bounds_fmt_interval(tb, index_type ?: type, dir, walk, thigh);
   }

   if (index_type == NULL)
      tb_printf(tb, " of type %s", type_pp(type));
   else
      tb_printf(tb, " of %s with index type %s", type_pp(type),
                type_pp(index_type));

   type_t base = index_type ?: type;
   while (type_kind(base) == T_SUBTYPE && !type_has_ident(base))
      base = type_base(base);

   int64_t rlow, rhigh;
   if (!folded_bounds(range_of(index_type ?: type, 0), &rlow, &rhigh))
      return;

   if (rlow != tlow || rhigh != thigh || !type_has_ident(index_type ?: type)) {
      tb_cat(tb, " range ");
      bounds_fmt_interval(tb, index_type ?: type, dir, tlow, thigh);
   }

   bounds_error(t, "%s", tb_get(tb));
}

static void bounds_free_intervals(interval_t **list)
{
   for (interval_t *it = *list, *tmp; it != NULL; it = tmp) {
      tmp = it->next;
      free(it);
   }
   *list = NULL;
}

static void bounds_check_aggregate(tree_t t)
{
   if (!tree_has_type(t))
      return;   // VHDL-2008 aggregate resolution in subtype declaration

   type_t type = tree_type(t);
   if (!type_is_array(type))
      return;

   // Find the tightest bounds for the index

   int64_t low, high, clow = 0, chigh = 0;
   type_t index_type = index_type_of(type, 0);
   range_kind_t dir;

   const bool unconstrained = type_is_unconstrained(type);

   if (unconstrained) {
      // Aggregate of unconstrained array type
      tree_t base_r = range_of(index_type, 0);
      if (!folded_bounds(base_r, &low, &high))
         return;

      clow = high, chigh = low;  // Actual bounds computed below
      dir = tree_subkind(base_r);
   }
   else {
      tree_t type_r = range_of(type, 0);
      if (!folded_bounds(type_r, &low, &high))
         return;

      clow = low, chigh = high;
      dir = tree_subkind(type_r);
   }

   const int ndims = dimension_of(type);

   interval_t *covered = NULL;
   bool known_elem_count = true;
   int next_pos = 0;
   const int nassocs = tree_assocs(t);
   for (int i = 0; i < nassocs; i++) {
      tree_t a = tree_assoc(t, i);
      int64_t ilow = 0, ihigh = 0, count = 1;
      unsigned uval;

      if (ndims == 1 && standard() >= STD_08) {
         // Element type may also have the same type as the aggregate
         type_t value_type = tree_type(tree_value(a));
         if (type_eq(value_type, type)) {
            if (type_is_unconstrained(value_type))
               known_elem_count = false;
            else if (!folded_length(range_of(value_type, 0), &count))
               known_elem_count = false;
         }
      }

      switch (tree_subkind(a)) {
      case A_NAMED:
         {
            tree_t name = tree_name(a);
            if (!bounds_check_index(name, NULL, index_type, dir,
                                    "aggregate choice", low, high))
               known_elem_count = false;
            if (folded_int(name, &ilow))
               ihigh = ilow;
            else if (folded_enum(name, &uval))
               ihigh = ilow = uval;
            else
               known_elem_count = false;
         }
         break;

      case A_RANGE:
         {
            tree_t r = tree_range(a, 0);
            const range_kind_t rkind = tree_subkind(r);
            if (rkind == RANGE_TO || rkind == RANGE_DOWNTO) {
               tree_t left = tree_left(r), right = tree_right(r);

               if (!bounds_check_index(left, NULL, index_type, rkind,
                                       "aggregate choice", low, high))
                  known_elem_count = false;
               if (!bounds_check_index(right, NULL, index_type, rkind,
                                       "aggregate choice", low, high))
                  known_elem_count = false;

               int64_t ileft, iright;
               unsigned pleft, pright;
               if (folded_int(left, &ileft) && folded_int(right, &iright)) {
                  ilow = (rkind == RANGE_TO ? ileft : iright);
                  ihigh = (rkind == RANGE_TO ? iright : ileft);
               }
               else if (folded_enum(left, &pleft)
                        && folded_enum(right, &pright)) {
                  ilow = (rkind == RANGE_TO ? pleft : pright);
                  ihigh = (rkind == RANGE_TO ? pright : pleft);
               }
               else
                  known_elem_count = false;

               if (count > 1 && known_elem_count && ihigh - ilow + 1 != count)
                  bounds_error(a, "discrete range has %"PRIi64" elements but "
                               "length of expression is %"PRIi64,
                               ihigh - ilow + 1, count);
            }
            else
               known_elem_count = false;
         }
         break;

      case A_OTHERS:
         known_elem_count = false;
         break;

      case A_POS:
         if (dir == RANGE_TO) {
            ilow = low + next_pos;
            ihigh = ilow + count - 1;
         }
         else {
            ihigh = high - next_pos;
            ilow = ihigh - count + 1;
         }

         next_pos += count;

         if ((ilow < low || ihigh > high) && known_elem_count) {
            LOCAL_TEXT_BUF tb = tb_new();
            tb_printf(tb, "expected at most %"PRIi64" positional "
                      "associations in %s aggregate with index type %s "
                      "range ", MAX(0, high - low + 1), type_pp(type),
                      type_pp(index_type));
            bounds_fmt_type_range(tb, index_type, dir, low, high);

            bounds_error(t, "%s", tb_get(tb));
            known_elem_count = false;
         }
         break;
      }

      if (unconstrained) {
         clow = MIN(clow, ilow);
         chigh = MAX(chigh, ihigh);
      }

      if (known_elem_count)
         bounds_cover_choice(&covered, a, index_type, ilow, ihigh);
   }

   if (known_elem_count)
      bounds_check_missing_choices(t, type, index_type, dir, clow, chigh,
                                   covered);

   bounds_free_intervals(&covered);

   // Check each sub-aggregate has the same length for an unconstrained
   // array aggregate

   if (ndims > 1 && unconstrained) {
      int64_t length = -1;
      for (int i = 0; i < nassocs; i++) {
         tree_t a = tree_assoc(t, i);
         type_t value_type = tree_type(tree_value(a));

         int64_t this_length;
         if (!folded_length(range_of(value_type, 0), &this_length))
             break;

         if (length == -1)
            length = this_length;
         else if (length != this_length)
            bounds_error(a, "length of sub-aggregate %"PRIi64" does not match "
                         "expected length %"PRIi64,
                         this_length, length);
      }
   }

   if (unconstrained && (known_elem_count || nassocs == 1)) {
      // Construct a new array subtype using the rules in LRM 93 7.3.2.2

      type_t tmp = type_new(T_SUBTYPE);
      type_set_base(tmp, type);

      tree_t constraint = tree_new(T_CONSTRAINT);
      tree_set_subkind(constraint, C_INDEX);

      tree_t left = NULL, right = NULL, r = NULL;

      if (known_elem_count) {
         const int64_t ileft = dir == RANGE_TO ? clow : chigh;
         const int64_t iright = dir == RANGE_TO ? chigh : clow;

         if (type_is_enum(index_type)) {
            left = get_enum_lit(t, index_type, ileft);
            right = get_enum_lit(t, index_type, iright);
         }
         else if (type_is_integer(index_type)) {
            left = get_int_lit(t, index_type, ileft);
            right = get_int_lit(t, index_type, iright);
         }
         else
            fatal_trace("cannot handle aggregate index type %s",
                        type_pp(index_type));
      }
      else {
         // Must have a single association
         assert(nassocs == 1);
         tree_t a0 = tree_assoc(t, 0);
         switch (tree_subkind(a0)) {
         case A_NAMED:
            left = right = tree_name(a0);
            break;
         case A_RANGE:
            {
               tree_t a0r = tree_range(a0, 0);
               if (tree_subkind(a0r) == RANGE_EXPR)
                  r = a0r;
               else {
                  left = tree_left(a0r);
                  right = tree_right(a0r);
                  dir = tree_subkind(a0r);
               }
            }
            break;
         default:
            fatal_trace("unexpected association kind %d in unconstrained "
                        "aggregate", tree_subkind(a0));
         }
      }

      if (r == NULL) {
         r = tree_new(T_RANGE);
         tree_set_subkind(r, dir);
         tree_set_left(r, left);
         tree_set_right(r, right);
         tree_set_loc(r, tree_loc(t));
         tree_set_type(r, tree_type(left));
      }

      tree_add_range(constraint, r);

      for (int i = 1; i < ndims; i++) {
         tree_t dim = range_of(tree_type(tree_value(tree_assoc(t, 0))), i - 1);
         tree_add_range(constraint, dim);
      }

      type_add_constraint(tmp, constraint);
      tree_set_type(t, tmp);
   }
}

static void bounds_check_decl(tree_t t)
{
   if (!tree_has_type(t))
      return;   // Alias declaration without subtype indication

   type_t type = tree_type(t);

   if (tree_has_value(t))
      bounds_check_assignment(t, tree_value(t));

   const bool is_constrained_array_subtype =
      type_is_array(type)
      && !type_is_unconstrained(type)
      && type_kind(type) == T_SUBTYPE;

   if (is_constrained_array_subtype) {
      // Check folded range does not violate index constraints of base type

      type_t base = type_base(type);

      const int ndims = dimension_of(base);
      for (int i = 0; i < ndims; i++) {
         tree_t dim = range_of(type, i);

         type_t cons = index_type_of(base, i);
         type_t cons_base  = type_base_recur(cons);

         const bool is_enum = (type_kind(cons_base) == T_ENUM);

         tree_t bounds = range_of(cons, 0);

         // Only check here if range can be determined to be non-null

         int64_t dim_low, bounds_low;
         int64_t dim_high, bounds_high;

         const bool is_static =
            folded_bounds(dim, &dim_low, &dim_high)
            && folded_bounds(bounds, &bounds_low, &bounds_high);

         if (!is_static)
            continue;

         const bool is_null =
            dim_low > dim_high || bounds_low > bounds_high;

         if (is_null)
            continue;

         const range_kind_t dim_kind = tree_subkind(dim);
         tree_t dim_left = tree_left(dim);
         tree_t dim_right = tree_right(dim);

         if (dim_low < bounds_low) {
            if (is_enum) {
               tree_t lit = type_enum_literal(cons_base, (unsigned)dim_low);
               bounds_error(dim_kind == RANGE_TO ? dim_left : dim_right,
                            "%s index %s violates constraint %s",
                            dim_kind == RANGE_TO ? "left" : "right",
                            istr(tree_ident(lit)), type_pp(cons));
            }
            else
               bounds_error(dim_kind == RANGE_TO ? dim_left : dim_right,
                            "%s index %"PRIi64" violates constraint %s",
                            dim_kind == RANGE_TO ? "left" : "right",
                            dim_low, type_pp(cons));

         }

         if (dim_high > bounds_high) {
            if (is_enum) {
               tree_t lit = type_enum_literal(cons_base, (unsigned)dim_high);
               bounds_error(dim_kind == RANGE_TO ? dim_right : dim_left,
                            "%s index %s violates constraint %s",
                            dim_kind == RANGE_TO ? "right" : "left",
                            istr(tree_ident(lit)), type_pp(cons));
            }
            else
               bounds_error(dim_kind == RANGE_TO ? dim_right : dim_left,
                            "%s index %"PRIi64" violates constraint %s",
                            dim_kind == RANGE_TO ? "right" : "left",
                            dim_high, type_pp(cons));
         }
      }
   }
}

static char *bounds_get_hint_str(tree_t where)
{
   switch (tree_kind(where)) {
   case T_PORT_DECL:
      return xasprintf(" for port %s", istr(tree_ident(where)));
   case T_PARAM_DECL:
      return xasprintf(" for parameter %s", istr(tree_ident(where)));
   case T_VAR_DECL:
      return xasprintf(" for variable %s", istr(tree_ident(where)));
   case T_GENERIC_DECL:
      return xasprintf(" for generic %s", istr(tree_ident(where)));
   case T_SIGNAL_DECL:
      return xasprintf(" for signal %s", istr(tree_ident(where)));
   case T_ALIAS:
      return xasprintf(" for alias %s", istr(tree_ident(where)));
   case T_REF:
      return bounds_get_hint_str(tree_ref(where));
   default:
      return NULL;
   }
}

static void bounds_check_assignment(tree_t target, tree_t value)
{
   type_t target_type = tree_type(target);
   type_t value_type  = tree_type(value);

   const bool check_array_length =
      type_is_array(target_type)
      && !type_is_unconstrained(target_type)
      && !type_is_unconstrained(value_type);

   if (check_array_length) {
      const int ndims = dimension_of(target_type);
      for (int i = 0; i < ndims; i++) {
         int64_t target_w, value_w;
         if (folded_length(range_of(target_type, i), &target_w)
             && folded_length(range_of(value_type, i), &value_w)) {
            if (target_w != value_w) {
               char *hint LOCAL = bounds_get_hint_str(target);
               if (i > 0)
                  bounds_error(value, "length of dimension %d of value %"PRIi64
                               " does not match length of target %"PRIi64"%s",
                               i + 1, value_w, target_w, hint ?: "");
               else
                  bounds_error(value, "length of value %"PRIi64" does not "
                               "match length of target %"PRIi64"%s",
                               value_w, target_w, hint ?: "");
            }
         }
      }
   }

   if (type_is_scalar(target_type))
      bounds_check_scalar(value, target_type, target);
}

static void bounds_check_signal_assign(tree_t t)
{
   int64_t last_delay = 0;
   tree_t target = tree_target(t);

   const int nwaves = tree_waveforms(t);
   for (int i = 0; i < nwaves; i++) {
      tree_t w = tree_waveform(t, i);

      if (tree_has_value(w))
         bounds_check_assignment(target, tree_value(w));

      int64_t delay = 0;
      bool delay_is_known = true;
      const bool has_delay = tree_has_delay(w);

      if (has_delay) {
         delay_is_known = folded_int(tree_delay(w), &delay);
         if (delay_is_known && delay < 0)
            bounds_error(tree_delay(w), "assignment delay may not be "
                         "negative");
      }

      if (i == 0) {
         int64_t rlimit;
         if (tree_has_reject(t) && folded_int(tree_reject(t), &rlimit)) {
            if ((rlimit < 0) && (delay >= 0))
               bounds_error(tree_reject(t), "rejection limit may not be "
                            "negative");

            if (rlimit > delay)
               bounds_error(tree_reject(t), "rejection limit may not be "
                            "greater than first assignment delay");
         }
      } else if (delay_is_known && delay <= last_delay)
         bounds_error(has_delay ? tree_delay(w) : w, "assignment delays "
                      "must be in ascending time order");

      // even if the delay isn't known, it has to be at least zero
      last_delay = delay;
   }
}

static void bounds_check_var_assign(tree_t t)
{
   bounds_check_assignment(tree_target(t), tree_value(t));
}

static void bounds_check_case(tree_t t)
{
   type_t type = tree_type(tree_value(t));

   if (type_is_scalar(type)) {
      // Check that the full range of the type is covered

      tree_t type_r = range_of(type, 0);

      int64_t tlow, thigh;
      if (!folded_bounds(type_r, &tlow, &thigh))
         return;

      const range_kind_t tdir = tree_subkind(type_r);

      bool have_others = false;
      interval_t *covered = NULL;

      const int nassocs = tree_assocs(t);
      for (int i = 0; i < nassocs; i++) {
         tree_t a = tree_assoc(t, i);

         int64_t low = INT64_MIN, high = INT64_MAX;
         switch (tree_subkind(a)) {
         case A_OTHERS:
            have_others = true;
            continue;

         case A_NAMED:
            {
               tree_t name = tree_name(a);
               if (!bounds_check_index(name, NULL, type, tdir, "case choice",
                                       tlow, thigh))
                  have_others = true;
               else
                  low = high = assume_int(tree_name(a));
            }
            break;

         case A_RANGE:
            {
               tree_t r = tree_range(a, 0);
               const range_kind_t dir = tree_subkind(r);

               if (dir == RANGE_EXPR)
                  fatal_at(tree_loc(r), "locally static range not folded");

               tree_t left = tree_left(r);
               tree_t right = tree_right(r);

               if (!bounds_check_index(left, NULL, type, dir, "case choice",
                                       tlow, thigh))
                  have_others = true;
               if (!bounds_check_index(right, NULL, type, dir, "case choice",
                                       tlow, thigh))
                  have_others = true;

               low = assume_int(dir == RANGE_TO ? left : right);
               high = assume_int(dir == RANGE_TO ? right : left);
            }
            break;
         }

         if (!have_others)
            bounds_cover_choice(&covered, a, type, low, high);
      }

      if (!have_others)
         bounds_check_missing_choices(t, type, NULL, direction_of(type, 0),
                                      tlow, thigh, covered);

      bounds_free_intervals(&covered);
   }
   else if (type_is_array(type) && !type_is_unconstrained(type)) {
      // Calculate how many values each element has
      type_t elem = type_elem(type);
      assert(type_is_enum(elem));

      int64_t elemsz;
      if (!folded_length(range_of(elem, 0), &elemsz))
         return;

      int64_t length;
      if (!folded_length(range_of(type, 0), &length))
          return;

      const int64_t expect = ipow(elemsz, length);

      int64_t have = 0;
      const int nassocs = tree_assocs(t);
      for (int i = 0; i < nassocs; i++) {
         tree_t a = tree_assoc(t, i);

         switch (tree_subkind(a)) {
         case A_OTHERS:
            have = expect;
            continue;

         case A_NAMED:
            have++;
            break;

         case A_RANGE:
            assert(false);
         }
      }

      if (have != expect)
         bounds_error(t, "choices cover only %"PRIi64" of %"PRIi64
                      " possible values", have, expect);
   }
}

static void bounds_check_type_conv(tree_t t)
{
   tree_t value = tree_value(t);

   type_t from = tree_type(value);
   type_t to   = tree_type(t);

   if (type_is_integer(to)) {
      int64_t ival = 0;
      double rval = 0.0;
      bool folded = false;
      if (type_is_real(from) && (folded = folded_real(value, &rval)))
         ival = (int64_t)rval;
      else if (type_is_integer(from) && (folded = folded_int(value, &ival)))
         ;

      if (folded) {
         int64_t b_low, b_high;
         folded_bounds(range_of(to, 0), &b_low, &b_high);
         if (folded_bounds(range_of(to, 0), &b_low, &b_high)
             && (ival < b_low || ival > b_high)) {
            char *argstr LOCAL = type_is_real(from)
               ? xasprintf("%lg", rval) : xasprintf("%"PRIi64, ival);
            bounds_error(value, "type conversion argument %s out of "
                         "bounds %"PRIi64" to %"PRIi64, argstr, b_low, b_high);
         }
      }
   }
}

static void bounds_check_attr_ref(tree_t t)
{
   const attr_kind_t predef = tree_subkind(t);
   switch (predef) {
   case ATTR_LENGTH:
   case ATTR_LOW:
   case ATTR_HIGH:
   case ATTR_LEFT:
   case ATTR_RIGHT:
      if (tree_params(t) > 0) {
         type_t type = tree_type(tree_name(t));
         if (type_is_array(type) && !type_is_unconstrained(type)) {
            tree_t dim_tree = tree_value(tree_param(t, 0));

            int64_t dim;
            const bool f = folded_int(dim_tree, &dim);
            (void)f;
            assert(f);

            if (dim < 1 || dim > dimension_of(type))
               bounds_error(dim_tree, "invalid dimension %"PRIi64" for type %s",
                            dim, type_pp(type));
         }
      }
      break;

   default:
      break;
   }
}

static void bounds_check_wait(tree_t t)
{
   int64_t delay = 0;
   if (tree_has_delay(t) && folded_int(tree_delay(t), &delay))
      if (delay < 0)
         bounds_error(tree_delay(t), "wait timeout may not be negative");
}

static void bounds_check_block(tree_t t)
{
   const int nparams = tree_params(t);
   for (int i = 0; i < nparams; i++) {
      tree_t m = tree_param(t, i);

      bool is_subelement = false;
      type_t ftype = NULL;
      tree_t port = NULL;
      if (tree_subkind(m) == P_POS) {
         port  = tree_port(t, tree_pos(m));
         ftype = tree_type(port);
      }
      else {
         tree_t name = tree_name(m);

         // Skip output conversions
         const tree_kind_t kind = tree_kind(name);
         if (kind == T_TYPE_CONV || kind == T_CONV_FUNC) return;

         port = tree_ref(name_to_ref(name));
         ftype = tree_type(name);
         is_subelement = tree_kind(name) != T_REF;
      }

      if (!type_is_array(ftype) || type_is_unconstrained(ftype))
         continue;

      tree_t value = tree_value(m);
      for (tree_kind_t kind = tree_kind(value);
           kind == T_TYPE_CONV || kind == T_QUALIFIED;
           value = tree_value(value), kind = tree_kind(value))
         ;

      type_t atype = tree_type(value);

      if (type_is_unconstrained(atype))
         continue;

      const int ndims = dimension_of(ftype);
      for (int j = 0; j < ndims; j++) {
         tree_t formal_r = range_of(ftype, j);
         tree_t actual_r = range_of(atype, j);

         int64_t f_len, a_len;
         const bool folded =
            folded_length(formal_r, &f_len) && folded_length(actual_r, &a_len);

         if (!folded) continue;

         if (f_len != a_len)
            bounds_error(value, "actual length %"PRIi64"%s does not match "
                         "formal length %"PRIi64" for%s port %s",
                         a_len, bounds_dimension_str(ndims, j), f_len,
                         is_subelement ? " subelement of" : "",
                         istr(tree_ident(port)));
      }
   }
}

static tree_t bounds_visit_fn(tree_t t, void *context)
{
   switch (tree_kind(t)) {
   case T_PCALL:
   case T_FCALL:
      bounds_check_call_args(t);
      break;
   case T_ARRAY_REF:
      bounds_check_array_ref(t);
      break;
   case T_ARRAY_SLICE:
      bounds_check_array_slice(t);
      break;
   case T_AGGREGATE:
      bounds_check_aggregate(t);
      break;
   case T_SIGNAL_DECL:
   case T_CONST_DECL:
   case T_VAR_DECL:
   case T_PORT_DECL:
   case T_PARAM_DECL:
   case T_ALIAS:
      bounds_check_decl(t);
      break;
   case T_GENERIC_DECL:
      if (tree_class(t) == C_CONSTANT)
         bounds_check_decl(t);
      break;
   case T_SIGNAL_ASSIGN:
      bounds_check_signal_assign(t);
      break;
   case T_VAR_ASSIGN:
      bounds_check_var_assign(t);
      break;
   case T_CASE:
      bounds_check_case(t);
      break;
   case T_LITERAL:
      bounds_check_literal(t);
      break;
   case T_TYPE_CONV:
      bounds_check_type_conv(t);
      break;
   case T_ATTR_REF:
      bounds_check_attr_ref(t);
      break;
   case T_WAIT:
      bounds_check_wait(t);
      break;
   case T_BLOCK:
      bounds_check_block(t);
      break;
   default:
      break;
   }

   return t;
}

void bounds_check(tree_t top)
{
   tree_rewrite(top, NULL, bounds_visit_fn, NULL, NULL);
}
