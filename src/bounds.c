//
//  Copyright (C) 2011-2024  Nick Gasson
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
#include "common.h"
#include "diag.h"
#include "phase.h"
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
   assert(!type_is_unconstrained(type));

   int64_t expect;
   if (folded_length(range_of(type, 0), &expect) && expect != tree_chars(t))
      bounds_error(t, "expected %"PRIi64" elements in string literal but "
                   "have %d", expect, tree_chars(t));
}

static void format_type_range(diag_t *d, type_t type, range_kind_t dir,
                              int64_t low, int64_t high)
{
   text_buf_t *tb = diag_text_buf(d);

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

static void add_hint_string(diag_t *d, tree_t where)
{
   const char *what = "";
   switch (tree_kind(where)) {
   case T_VAR_DECL:
   case T_SIGNAL_DECL:
   case T_CONST_DECL:
   case T_REF:          what = class_str(class_of(where)); break;
   case T_PARAM_DECL:   what = "parameter"; break;
   case T_PORT_DECL:    what = "port"; break;
   case T_GENERIC_DECL: what = "generic"; break;
   case T_ALIAS:        what = "alias"; break;
   default: break;
   }

   diag_printf(d, " for %s %s", what, istr(tree_ident(where)));
}

static void bounds_check_scalar(tree_t value, type_t type, tree_t hint)
{
   tree_t r = range_of(type, 0);

   bool error = false;
   int64_t low, high, folded;
   double rlow, rhigh;
   if (folded_bounds(r, &low, &high)) {
      if (folded_int(value, &folded))
         error = (folded < low || folded > high);
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
      diag_t *d = diag_new(DIAG_ERROR, tree_loc(value));
      diag_printf(d, "value ");
      to_string(diag_text_buf(d), type, folded);
      diag_printf(d, " outside of %s range ", type_pp(type));
      format_type_range(d, type, tree_subkind(r), low, high);

      if (hint != NULL)
         add_hint_string(d, hint);

      diag_emit(d);
   }
}

static void bounds_check_array(tree_t value, type_t type, tree_t hint)
{
   if (type_is_unconstrained(type))
      return;

   type_t value_type = tree_type(value);
   if (type_is_unconstrained(value_type))
      return;

   const int ndims = dimension_of(type);
   for (int i = 0; i < ndims; i++) {
      int64_t target_w, value_w;
      if (!folded_length(range_of(type, i), &target_w))
         continue;
      else if (!folded_length(range_of(value_type, i), &value_w))
         continue;
      else if (target_w != value_w) {
         diag_t *d = diag_new(DIAG_ERROR, tree_loc(value));
         diag_printf(d, "length of ");
         if (i > 0)
            diag_printf(d, "dimension %d of ", i + 1);
         diag_printf(d, "value %"PRIi64" does not match length of target %"
                     PRIi64, value_w, target_w);
         if (hint != NULL)
            add_hint_string(d, hint);

         diag_emit(d);
      }
   }
}

static tree_t bounds_check_call_args(tree_t t)
{
   tree_t decl = tree_ref(t);

   const int nparams = tree_params(t);
   const int nports  = tree_ports(decl);

   bool known_arg_length = true;

   for (int i = 0; (i < nparams) && (i < nports); i++) {
      tree_t param = tree_param(t, i);
      assert(tree_subkind(param) == P_POS);

      tree_t value = tree_value(param);
      tree_t port  = tree_port(decl, tree_pos(param));

      type_t ftype = tree_type(port);
      type_t atype = tree_type(tree_value(param));

      if (type_is_array(ftype)) {
         // Check bounds of constrained array parameters

         if (type_is_unconstrained(atype)) {
            known_arg_length = false;
            continue;
         }
         else if (type_is_unconstrained(ftype))
            continue;

         const int ndims = dimension_of(ftype);

         for (int j = 0; j < ndims; j++) {
            tree_t formal_r = range_of(ftype, j);
            tree_t actual_r = range_of(atype, j);

            int64_t f_len, a_len;
            if (!folded_length(formal_r, &f_len))
               continue;
            else if (!folded_length(actual_r, &a_len)) {
               known_arg_length = false;
               continue;
            }

            if (f_len != a_len) {
               diag_t *d = diag_new(DIAG_ERROR, tree_loc(param));
               diag_printf(d, "actual length %"PRIi64, a_len);
               if (ndims > 1)
                  diag_printf(d, " for dimension %d", j + 1);
               diag_printf(d, " does not match formal length %"PRIi64
                           " for parameter %s", f_len, istr(tree_ident(port)));
               diag_emit(d);
            }
         }
      }
      else if (type_is_scalar(ftype))
         bounds_check_scalar(value, ftype, port);
   }

   if (tree_kind(decl) == T_GENERIC_DECL)
      return t;

   const subprogram_kind_t kind = tree_subkind(decl);
   if (known_arg_length && (kind == S_ARRAY_EQ || kind == S_ARRAY_NEQ)) {
      // Warn if calling the predefined array equality operators and the
      // left and right hand sides have different lengths as this always
      // returns FALSE

      type_t ltype = tree_type(tree_value(tree_param(t, 0)));
      type_t rtype = tree_type(tree_value(tree_param(t, 1)));

      const int ndims = dimension_of(ltype);
      for (int i = 0; i < ndims; i++) {
         tree_t left_r = range_of(ltype, i);
         tree_t right_r = range_of(rtype, i);

         int64_t left_len;
         if (!folded_length(left_r, &left_len))
            continue;

         int64_t right_len;
         if (!folded_length(right_r, &right_len))
            continue;

         if (left_len != right_len) {
            diag_t *d = diag_new(DIAG_WARN, tree_loc(t));
            diag_printf(d, "call to predefined operator %s always returns "
                        "FALSE", istr(tree_ident(t)));
            diag_hint(d, tree_loc(t), "left length is %"PRIi64" but right "
                      "length is %"PRIi64, left_len, right_len);
            diag_emit(d);
            break;
         }
      }
   }

   return t;
}

static bool index_in_range(tree_t index, int64_t low, int64_t high,
                           int64_t *value)
{
   int64_t folded;
   if (folded_int(index, &folded)) {
      *value = folded;
      return folded >= low && folded <= high;
   }

   return true;
}

static bool bounds_check_index(tree_t index, type_t type, range_kind_t kind,
                               const char *what, int64_t low, int64_t high)
{
   int64_t folded;
   if (!index_in_range(index, low, high, &folded) && low <= high) {
      diag_t *d = diag_new(DIAG_ERROR, tree_loc(index));
      diag_printf(d, "%s index ", what);
      to_string(diag_text_buf(d), type, folded);
      diag_printf(d, " outside of %s range ", type_pp(type));
      format_type_range(d, type, kind, low, high);
      diag_emit(d);

      return false;
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

         int64_t ivalue, low, high;
         if (folded_int(pvalue, &ivalue) && folded_bounds(r, &low, &high)) {
            checked = true;

            if (ivalue < low || ivalue > high) {
               diag_t *d = diag_new(DIAG_ERROR, tree_loc(pvalue));
               diag_printf(d, "array");
               if (tree_kind(value) == T_REF)
                  diag_printf(d, " %s", istr(tree_ident(value)));
               diag_printf(d, " index ");
               to_string(diag_text_buf(d), index_type, ivalue);
               diag_printf(d, " outside of %s range ", type_pp(index_type));
               format_type_range(d, index_type, dir, low, high);

               diag_emit(d);
            }
         }
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
                  checked |= (tree_kind(r_base) == T_REF &&
                              tree_ref(r_base) == tree_ref(value));
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

   int64_t folded;
   const char *error = NULL;
   if (!index_in_range(tree_left(r), blow, bhigh, &folded))
      error = "left";
   else if (!index_in_range(tree_right(r), blow, bhigh, &folded))
      error = "right";

   if (error) {
      diag_t *d = diag_new(DIAG_ERROR, tree_loc(r));
      diag_printf(d, "array");
      if (tree_kind(value) == T_REF)
         diag_printf(d, " %s", istr(tree_ident(value)));
      diag_printf(d, " slice %s index ", error);
      to_string(diag_text_buf(d), index_type, folded);
      diag_printf(d, " outside of %s range ", type_pp(index_type));
      format_type_range(d, index_type, dir, blow, bhigh);

      diag_emit(d);
   }
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

static void report_interval(diag_t *d, type_t type, range_kind_t dir,
                            int64_t low, int64_t high)
{
   if (low == high) {
      if (type_is_integer(type))
         diag_printf(d, "%"PRIi64, low);
      else if (type_is_enum(type)) {
         type_t base = type_base_recur(type);
         diag_printf(d, "%s", istr(tree_ident(type_enum_literal(base, low))));
      }
   }
   else
      format_type_range(d, type, dir, low, high);
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

   // Allow downgrade to warning for case statement but not aggregates
   diag_t *d;
   if (index_type == NULL) {
      if ((d = pedantic_diag(tree_loc(t))) == NULL)
         return;
   }
   else
      d = diag_new(DIAG_ERROR, tree_loc(t));

   diag_printf(d, "missing choice%s for element%s ",
               missing > 1 ? "s" : "", missing > 1 ? "s" : "");

   int printed = 0;
   for (it = covered, walk = tlow; it != NULL; it = it->next) {
      if (it->low != walk) {
         if (printed++) diag_printf(d, ", ");
         report_interval(d, index_type ?: type, dir, walk, it->low - 1);
      }

      walk = it->high + 1;
   }

   if (walk != thigh + 1) {
      if (printed++) diag_printf(d, ", ");
      report_interval(d, index_type ?: type, dir, walk, thigh);
   }

   if (index_type == NULL)
      diag_printf(d, " of type %s", type_pp(type));
   else
      diag_printf(d, " of %s with index type %s", type_pp(type),
                  type_pp(index_type));

   type_t base = index_type ?: type;
   while (is_anonymous_subtype(base))
      base = type_base(base);

   int64_t rlow, rhigh;
   if (!folded_bounds(range_of(index_type ?: type, 0), &rlow, &rhigh))
      return;

   if (rlow != tlow || rhigh != thigh || !type_has_ident(index_type ?: type)) {
      diag_printf(d, " range ");
      report_interval(d, index_type ?: type, dir, tlow, thigh);
   }

   diag_emit(d);
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
   type_t elem = type_elem(type);

   interval_t *covered = NULL;
   bool known_elem_count = true;
   int next_pos = 0;
   const int nassocs = tree_assocs(t);
   for (int i = 0; i < nassocs; i++) {
      tree_t a = tree_assoc(t, i), value = tree_value(a);
      int64_t ilow = 0, ihigh = 0, count = 1;
      const assoc_kind_t akind = tree_subkind(a);

      if (akind == A_SLICE || akind == A_CONCAT) {
         type_t value_type = tree_type(value);
         if (type_is_unconstrained(value_type))
            known_elem_count = false;
         else if (!folded_length(range_of(value_type, 0), &count))
            known_elem_count = false;
      }

      switch (akind) {
      case A_NAMED:
         {
            tree_t name = tree_name(a);
            if (!bounds_check_index(name, index_type, dir,
                                    "aggregate choice", low, high))
               known_elem_count = false;
            if (folded_int(name, &ilow))
               ihigh = ilow;
            else
               known_elem_count = false;
         }
         break;

      case A_RANGE:
      case A_SLICE:
         {
            tree_t r = tree_range(a, 0);
            const range_kind_t rkind = tree_subkind(r);
            if (rkind == RANGE_TO || rkind == RANGE_DOWNTO) {
               tree_t left = tree_left(r), right = tree_right(r);

               int64_t ileft, iright;
               if (folded_int(left, &ileft) && folded_int(right, &iright)) {
                  ilow = (rkind == RANGE_TO ? ileft : iright);
                  ihigh = (rkind == RANGE_TO ? iright : ileft);

                  // Cannot check either index unless both are known
                  // since the range may be null
                  if (ilow <= ihigh) {
                     if (!bounds_check_index(left, index_type, rkind,
                                             "aggregate choice", low, high))
                        known_elem_count = false;
                     if (!bounds_check_index(right, index_type, rkind,
                                             "aggregate choice", low, high))
                        known_elem_count = false;
                  }
                  else
                     known_elem_count = false;
               }
               else
                  known_elem_count = false;

               if (count > 1 && known_elem_count && ihigh - ilow + 1 != count)
                  bounds_error(a, "discrete range has %"PRIi64" elements but "
                               "length of expression is %"PRIi64,
                               ihigh - ilow + 1, count);
               else if (unconstrained && akind == A_SLICE && count > 1) {
                  // VHDL-2008 range association determines index
                  // direction for unconstrained aggregate
                  assert(standard() >= STD_08);
                  dir = rkind;
               }
            }
            else
               known_elem_count = false;
         }
         break;

      case A_OTHERS:
         known_elem_count = false;
         break;

      case A_POS:
      case A_CONCAT:
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
            diag_t *d = diag_new(DIAG_ERROR, tree_loc(t));
            diag_printf(d, "expected at most %"PRIi64" positional "
                        "associations in %s aggregate with index type %s "
                        "range ", MAX(0, high - low + 1), type_pp(type),
                        type_pp(index_type));
            format_type_range(d, index_type, dir, low, high);
            diag_emit(d);

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

      if (type_is_scalar(elem))
         bounds_check_scalar(value, elem, NULL);
      else if (ndims == 1 && type_is_array(elem))
         bounds_check_array(value, elem, NULL);
   }

   if (known_elem_count)
      bounds_check_missing_choices(t, type, index_type, dir, clow, chigh,
                                   covered);

   bounds_free_intervals(&covered);

   // Check each sub-aggregate has the same length for an unconstrained
   // array aggregate

   if (ndims > 1) {
      int64_t length = -1;
      for (int i = 0; i < nassocs; i++) {
         tree_t a = tree_assoc(t, i);
         type_t value_type = tree_type(tree_value(a));

         if (type_is_unconstrained(value_type))
            continue;

         int64_t this_length;
         if (!folded_length(range_of(value_type, 0), &this_length))
            continue;

         if (length == -1)
            length = this_length;
         else if (length != this_length)
            bounds_error(a, "length of sub-aggregate %"PRIi64" does not match "
                         "expected length %"PRIi64, this_length, length);
      }
   }
}

static void bounds_check_index_contraints(type_t type)
{
   if (type_kind(type) != T_SUBTYPE || type_constraints(type) == 0)
      return;

   tree_t c0 = type_constraint(type, 0);

   const constraint_kind_t kind = tree_subkind(c0);
   if (kind != C_INDEX && kind != C_RANGE)
      return;

   // Check folded range does not violate index constraints of base type

   const int ndims = dimension_of(type);
   for (int i = 0; i < ndims; i++) {
      tree_t dim = tree_range(c0, i);

      const range_kind_t dir = tree_subkind(dim);
      if (dir != RANGE_TO && dir != RANGE_DOWNTO)
         continue;

      type_t cons = kind == C_INDEX ? index_type_of(type, i) : type_base(type);
      if (type_kind(cons) == T_GENERIC)
         continue;   // Cannot check

      tree_t bounds = range_of(cons, 0);

      // Only check here if range can be determined to be non-null

      if (kind == C_RANGE && type_is_real(type)) {
         double dim_low, bounds_low;
         double dim_high, bounds_high;

         const bool is_static =
            folded_bounds_real(dim, &dim_low, &dim_high)
            && folded_bounds_real(bounds, &bounds_low, &bounds_high);

         if (!is_static)
            continue;

         const bool is_null =
            dim_low > dim_high || bounds_low > bounds_high;

         if (is_null)
            continue;

         if (dim_low < bounds_low)
            bounds_error(dir == RANGE_TO ? tree_left(dim) : tree_right(dim),
                         "%s index %g violates constraint %s",
                         dir == RANGE_TO ? "left" : "right",
                         dim_low, type_pp(cons));

         if (dim_high > bounds_high)
            bounds_error(dir == RANGE_TO ? tree_right(dim) : tree_left(dim),
                         "%s index %g violates constraint %s",
                         dir == RANGE_TO ? "right" : "left",
                         dim_high, type_pp(cons));

      }
      else {
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

         if (type_is_enum(cons)) {
            if (dim_low < bounds_low) {
               type_t cons_base = type_base_recur(cons);
               tree_t lit = type_enum_literal(cons_base, (unsigned)dim_low);
               bounds_error(dir == RANGE_TO ? tree_left(dim) : tree_right(dim),
                            "%s index %s violates constraint %s",
                            dir == RANGE_TO ? "left" : "right",
                            istr(tree_ident(lit)), type_pp(cons));
            }

            if (dim_high > bounds_high) {
               type_t cons_base = type_base_recur(cons);
               tree_t lit = type_enum_literal(cons_base, (unsigned)dim_high);
               bounds_error(dir == RANGE_TO ? tree_right(dim) : tree_left(dim),
                            "%s index %s violates constraint %s",
                            dir == RANGE_TO ? "right" : "left",
                            istr(tree_ident(lit)), type_pp(cons));
            }
         }
         else if (dim_high > bounds_high)
            bounds_error(dir == RANGE_TO ? tree_right(dim) : tree_left(dim),
                         "%s index %"PRIi64" violates constraint %s",
                         dir == RANGE_TO ? "right" : "left",
                         dim_high, type_pp(cons));
         else if (dim_low < bounds_low)
            bounds_error(dir == RANGE_TO ? tree_left(dim) : tree_right(dim),
                         "%s index %"PRIi64" violates constraint %s",
                         dir == RANGE_TO ? "left" : "right",
                         dim_low, type_pp(cons));
      }
   }
}

static void bounds_check_assignment(tree_t target, tree_t value)
{
   type_t target_type = tree_type(target);

   if (type_is_scalar(target_type))
      bounds_check_scalar(value, target_type, target);
   else if (type_is_array(target_type))
      bounds_check_array(value, target_type, target);
}

static void bounds_check_object_decl(tree_t t)
{
   if (tree_has_value(t))
      bounds_check_assignment(t, tree_value(t));

   type_t type = tree_type(t);
   if (is_anonymous_subtype(type))
      bounds_check_index_contraints(type);
}

static void bounds_check_alias_decl(tree_t t)
{
   if (!tree_has_type(t))
      return;   // Alias declaration without subtype indication

   if (tree_has_value(t))
      bounds_check_assignment(t, tree_value(t));

   type_t type = tree_type(t);
   if (is_anonymous_subtype(type))
      bounds_check_index_contraints(type);
}

static void bounds_check_elem_constraint(tree_t t)
{
   type_t type = tree_type(t);
   if (is_anonymous_subtype(type))
      bounds_check_index_contraints(type);
}

static void bounds_check_type_decl(tree_t t)
{
   type_t type = tree_type(t);

   if (type_is_record(type)) {
      const int nfields = type_fields(type);
      for (int i = 0; i < nfields; i++) {
         type_t ft = tree_type(type_field(type, i));
         if (is_anonymous_subtype(ft))
            bounds_check_index_contraints(ft);
      }
   }
   else if (type_is_array(type)) {
      bounds_check_index_contraints(type);

      type_t elem = type_elem(type);
      if (is_anonymous_subtype(elem))
         bounds_check_index_contraints(elem);
   }
   else if (type_is_scalar(type)) {
      int64_t low, high;
      if (folded_bounds(range_of(type, 0), &low, &high) && low > high)
         warn_at(tree_loc(t), "type %s has null range", type_pp(type));
   }
}

static void bounds_check_subtype_decl(tree_t t)
{
   bounds_check_index_contraints(tree_type(t));
}

static void bounds_check_interface_decl(tree_t t)
{
   const class_t class = tree_class(t);
   if (class == C_FUNCTION || class == C_PROCEDURE || class == C_PACKAGE)
      return;

   const port_mode_t mode = tree_subkind(t);
   if (mode != PORT_ARRAY_VIEW && mode != PORT_RECORD_VIEW && tree_has_value(t))
      bounds_check_assignment(t, tree_value(t));

   type_t type = tree_type(t);
   if (is_anonymous_subtype(type))
      bounds_check_index_contraints(type);
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

static void bounds_check_scalar_case(tree_t t, type_t type)
{
   // Check that the full range of the type is covered

   tree_t type_r = range_of(type, 0);

   int64_t tlow = INT64_MIN, thigh = INT64_MAX;
   while (!folded_bounds(type_r, &tlow, &thigh)
          && type_kind(type) == T_SUBTYPE) {
      // LRM 08 section 10.9: if the case expression does not have a
      // locally static subtype then the choices must cover every value
      // of the base type
      type = type_base(type);
      type_r = range_of(type, 0);
   }

   const range_kind_t tdir = tree_subkind(type_r);

   bool have_others = false;
   interval_t *covered = NULL;

   const int nstmts = tree_stmts(t);
   for (int i = 0; i < nstmts; i++) {
      tree_t alt = tree_stmt(t, i);

      const int nassocs = tree_assocs(alt);
      for (int j = 0; j < nassocs; j++) {
         tree_t a = tree_assoc(alt, j);

         int64_t low = INT64_MIN, high = INT64_MAX;
         switch (tree_subkind(a)) {
         case A_OTHERS:
            have_others = true;
            continue;

         case A_NAMED:
            {
               tree_t name = tree_name(a);
               if (!bounds_check_index(name, type, tdir, "case choice",
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

               if (!bounds_check_index(left, type, dir, "case choice",
                                       tlow, thigh))
                  have_others = true;
               if (!bounds_check_index(right, type, dir, "case choice",
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
   }

   if (!have_others)
      bounds_check_missing_choices(t, type, NULL, direction_of(type, 0),
                                   tlow, thigh, covered);

   bounds_free_intervals(&covered);
}

static void bounds_check_duplicate_choice(tree_t old, tree_t new, int length)
{
   for (int i = 0; i < length; i++) {
      if (get_case_choice_char(old, i) != get_case_choice_char(new, i))
         return;
   }

   diag_t *d = diag_new(DIAG_ERROR, tree_loc(new));
   diag_printf(d, "duplicate choice in case statement");
   diag_hint(d, tree_loc(new), "repeated here");
   diag_hint(d, tree_loc(old), "previous choice for this value");
   diag_hint(d, NULL, "each value of the subtype of the expression must be "
             "represented once and only once in the set of choices");
   diag_lrm(d, STD_93, "8.8");
   diag_emit(d);
}

static void bounds_check_array_case(tree_t t, type_t type)
{
   type_t elem = type_elem(type);
   assert(type_is_enum(elem));

   // Calculate how many values each element has
   int64_t elemsz = INT64_MAX;
   while (!folded_length(range_of(elem, 0), &elemsz)
          && type_kind(elem) == T_SUBTYPE)
      elem = type_base(elem);

   int64_t expect = -1, length = -1;

   const bool known_length =
      !type_is_unconstrained(type) && folded_length(range_of(type, 0), &length);

   if (known_length && !ipow_safe(elemsz, length, &expect))
      expect = INT64_MAX;   // Overflow

   int nchoices = 0;
   const int nstmts = tree_stmts(t);
   for (int i = 0; i < nstmts; i++)
      nchoices += tree_assocs(tree_stmt(t, i));

   int64_t have = 0;
   int64_t *hashes LOCAL = xmalloc_array(nchoices, sizeof(int64_t));
   tree_t *choices LOCAL = xmalloc_array(nchoices, sizeof(tree_t));
   int hptr = 0;

   for (int i = 0; i < nstmts; i++) {
      tree_t alt = tree_stmt(t, i);

      const int nassocs = tree_assocs(alt);
      for (int j = 0; j < nassocs; j++, hptr++) {
         tree_t a = tree_assoc(alt, j);
         hashes[hptr] = INT64_MAX;
         choices[hptr] = NULL;

         if (tree_subkind(a) == A_OTHERS) {
            have = expect;
            continue;
         }

         assert(tree_subkind(a) == A_NAMED);
         have++;

         tree_t name = choices[hptr] = tree_name(a);
         type_t choice_type = tree_type(name);
         if (type_is_unconstrained(choice_type))
            continue;

         int64_t choice_length;
         if (folded_length(range_of(choice_type, 0), &choice_length)) {
            if (length == -1) {
               length = choice_length;
               if (!ipow_safe(elemsz, length, &expect))
                  expect = INT64_MAX;
            }
            else if (choice_length != length) {
               diag_t *d = diag_new(DIAG_ERROR, tree_loc(name));
               diag_printf(d, "expected case choice to have length %"PRIi64
                           " but is %"PRIi64, length, choice_length);
               diag_hint(d, NULL, "the values of all choices for a case "
                         "expression with one-dimensional character array "
                         "type must have the same length");
               diag_lrm(d, STD_08, "10.9");
               diag_emit(d);
               return;
            }

            hashes[hptr] = encode_case_choice(name, choice_length, 0);

            for (int k = 0; k < hptr; k++) {
               if (hashes[k] == INT64_MAX)
                  continue;
               else if (hashes[hptr] == hashes[k]) {
                  tree_t old = choices[k];
                  bounds_check_duplicate_choice(old, name, choice_length);
               }
            }
         }
      }
   }

   if (have != expect && expect != -1) {
      const loc_t *loc = tree_loc(tree_value(t));
      diag_t *d = pedantic_diag(loc);
      if (d != NULL) {
         diag_printf(d, "choices cover only %"PRIi64" of ", have);
         if (expect > 100000)
            diag_printf(d, "%"PRIi64" ** %"PRIi64, elemsz, length);
         else
            diag_printf(d, "%"PRIi64, expect);
         diag_printf(d, " possible values");

         diag_hint(d, loc, "expression has %"PRIi64" elements of type %s, "
                   "each of which has %"PRIi64" possible values",
                   length, type_pp(type_elem(type)), elemsz);

         if (!known_length)
            diag_hint(d, NULL, "the case expression subtype is not locally "
                      "static so the length was derived from the choices");

         diag_emit(d);
      }
   }
}

static void bounds_check_case(tree_t t)
{
   type_t type = tree_type(tree_value(t));

   if (type_is_scalar(type))
      bounds_check_scalar_case(t, type);
   else if (type_is_array(type))
      bounds_check_array_case(t, type);
}

static void bounds_check_conv_integer(tree_t value, type_t from, type_t to)
{
   int64_t ival = 0;
   double rval = 0.0;
   bool folded = false;
   if (type_is_real(from)) {
      folded = folded_real(value, &rval);
      ival = (int64_t)(rval + 0.5);
   }
   else if (type_is_integer(from))
      folded = folded_int(value, &ival);

   if (folded) {
      int64_t b_low, b_high;
      if (folded_bounds(range_of(to, 0), &b_low, &b_high)
          && (ival < b_low || ival > b_high)) {
         char *argstr LOCAL = type_is_real(from)
            ? xasprintf("%lg", rval) : xasprintf("%"PRIi64, ival);
         bounds_error(value, "type conversion argument %s out of "
                      "bounds %"PRIi64" to %"PRIi64, argstr, b_low, b_high);
      }
   }
}

static void bounds_check_conv_array(tree_t value, type_t from, type_t to)
{
   const int ndims = dimension_of(to);
   assert(ndims == dimension_of(from));

   const bool to_constrained = !type_is_unconstrained(to);
   const bool from_constrained = !type_is_unconstrained(from);

   if (to_constrained && from_constrained) {
      for (int i = 0; i < ndims; i++) {
         tree_t r_to = range_of(to, i);
         tree_t r_from = range_of(from, i);

         int64_t to_length;
         if (!folded_length(r_to, &to_length))
            continue;

         int64_t from_length;
         if (!folded_length(r_from, &from_length))
            continue;

         if (to_length != from_length)
            bounds_error(value, "length of type conversion argument %"PRIi64
                         " does not match expected length %"PRIi64
                         " for constrained array subtype %s",
                         from_length, to_length, type_pp(to));
      }
   }
   else if (!to_constrained && from_constrained) {
      for (int i = 0; i < ndims; i++) {
         type_t index_type = index_type_of(to, i);
         tree_t r_to = range_of(index_type, i);
         tree_t r_from = range_of(from, i);

         if (tree_subkind(r_from) == RANGE_EXPR)
            continue;

         int64_t low, high;
         if (!folded_bounds(r_to, &low, &high))
            continue;

         int64_t f_low, f_high;
         if (!folded_bounds(r_from, &f_low, &f_high))
            continue;
         else if (f_low > f_high)
            continue;  // Null range

         int64_t folded = 0;
         const char *error = NULL;
         if (!index_in_range(tree_left(r_from), low, high, &folded))
            error = "left";
         else if (!index_in_range(tree_right(r_from), low, high, &folded))
            error = "right";

         if (error) {
            diag_t *d = diag_new(DIAG_ERROR, tree_loc(value));
            diag_printf(d, "array ");
            if (tree_kind(value) == T_REF)
               diag_printf(d, "%s ", istr(tree_ident(value)));
            diag_printf(d, "%s bound ", error);
            to_string(diag_text_buf(d), index_type, folded);
            diag_printf(d, " violates %s index constraint ", type_pp(to));
            format_type_range(d, index_type, tree_subkind(r_to), low, high);
            diag_emit(d);
         }
      }
   }
}

static void bounds_check_type_conv(tree_t t)
{
   tree_t value = tree_value(t);

   type_t from = tree_type(value);
   type_t to   = tree_type(t);

   if (type_is_integer(to))
      bounds_check_conv_integer(value, from, to);
   else if (type_is_array(to))
      bounds_check_conv_array(value, from, to);
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
   case ATTR_RANGE:
   case ATTR_REVERSE_RANGE:
      if (tree_params(t) > 0) {
         type_t type = tree_type(tree_name(t));
         if (type_is_array(type)) {
            tree_t dim_tree = tree_value(tree_param(t, 0));

            int64_t dim;
            if (!folded_int(dim_tree, &dim))
               bounds_error(dim_tree, "dimension is not constant");
            else if (dim < 1 || dim > dimension_of(type))
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
   if (tree_has_delay(t) && folded_int(tree_delay(t), &delay) && delay < 0)
      bounds_error(tree_delay(t), "wait timeout may not be negative");
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
      bounds_check_object_decl(t);
      break;
   case T_PORT_DECL:
   case T_PARAM_DECL:
   case T_GENERIC_DECL:
      bounds_check_interface_decl(t);
      break;
   case T_ALIAS:
      bounds_check_alias_decl(t);
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
   case T_STRING:
      bounds_check_string_literal(t);
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
   case T_ELEM_CONSTRAINT:
      bounds_check_elem_constraint(t);
      break;
   case T_TYPE_DECL:
      bounds_check_type_decl(t);
      break;
   case T_SUBTYPE_DECL:
      bounds_check_subtype_decl(t);
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
