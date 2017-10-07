//
//  Copyright (C) 2011-2015  Nick Gasson
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

static int errors = 0;

#define bounds_error(t, ...) \
   do { errors++; error_at(tree_loc(t), __VA_ARGS__); } while (0)

static void bounds_check_string_literal(tree_t t)
{
   type_t type = tree_type(t);
   if (type_is_unconstrained(type))
      return;

   int64_t expect;
   if (folded_length(type_dim(type, 0), &expect) && expect != tree_chars(t))
      bounds_error(t, "expected %"PRIi64" elements in string literal but "
                   "have %d", expect, tree_chars(t));
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

static bool is_out_of_range(tree_t value, range_t range, bool *checked)
{
   type_kind_t base_kind = type_base_kind(tree_type(value));

   switch (base_kind) {
   case T_INTEGER:
   case T_PHYSICAL:
      {
         int64_t ival, low, high;
         *checked = folded_int(value, &ival) && folded_bounds(range, &low, &high);
         if (*checked && (ival < low || ival > high))
            return true;

         return false;
      }
      break;
   case T_REAL:
      {
         double rval, low, high;
         *checked = folded_real(value, &rval) && folded_bounds_real(range, &low, &high);
         if (*checked && (rval < low || rval > high))
            return true;

         return false;
      }
      break;
   case T_ENUM:
      {
         unsigned uval;
         int64_t low, high;
         *checked = folded_enum(value, &uval) && folded_bounds(range, &low, &high);
         if (*checked && (((int64_t) uval < low) || ((int64_t) uval > high)))
            return true;

         return false;
      }
      break;
   default:
      return false;
   }
}

static const char *value_str(tree_t value)
{
   static const int BUF_SZ = 64;
   const type_t vtype = tree_type(value);
   const type_kind_t base_kind = type_base_kind(vtype);
   char *buf = get_fmt_buf(BUF_SZ);

   buf[0] = '\0';

   switch (base_kind) {
   case T_INTEGER:
      {
         int64_t ival;
         const bool is_folded = folded_int(value, &ival);
         assert(is_folded);
         snprintf(buf, BUF_SZ, "%"PRIi64, ival);
      }
      break;
   case T_PHYSICAL:
      {
         int64_t ival, max_unit_value = 0;
         const bool is_folded = folded_int(value, &ival);
         assert(is_folded);
         const unsigned  nunits = type_units(vtype);
         tree_t max_unit = NULL;

         // Find the largest unit that evenly divides the given value
         for (unsigned u = 0; u < nunits; u++)
         {
            tree_t unit = type_unit(vtype, u);
            int64_t unit_value;
            const bool is_folded = folded_int(tree_value(unit), &unit_value);
            assert(is_folded);
            if ((ival % unit_value == 0) && (unit_value > max_unit_value))
            {
               max_unit = unit;
               max_unit_value = unit_value;
            }
         }
         assert(max_unit);
         ival = ival / max_unit_value;
         snprintf(buf, BUF_SZ, "%"PRIi64" %s", ival, istr(tree_ident(max_unit)));
      }
      break;
   case T_REAL:
      {
         double rval;
         const bool is_folded = folded_real(value, &rval);
         assert(is_folded);
         snprintf(buf, BUF_SZ, "%lf", rval);
      }
      break;
   case T_ENUM:
      {
         unsigned uval;
         const bool is_folded = folded_enum(value, &uval);
         assert(is_folded);
         type_t base = type_base_recur(vtype);
         return istr(tree_ident(type_enum_literal(base, uval)));
      }
      break;
   default:
      break;
   }

   return (const char *) buf;
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

         const int ndims = type_dims(ftype);

         for (int j = 0; j < ndims; j++) {
            range_t formal_r = type_dim(ftype, j);
            range_t actual_r = type_dim(atype, j);

            int64_t f_len, a_len;

            const bool folded =
               folded_length(formal_r, &f_len)
               && folded_length(actual_r, &a_len);

            if (!folded)
               continue;

            if (f_len != a_len) {
               if (ndims > 1)
                  bounds_error(param, "actual length %"PRIi64" for dimension %d does "
                               "not match formal length %"PRIi64,
                               a_len, j + 1, f_len);
               else
                  bounds_error(param, "actual length %"PRIi64" does not match formal "
                               "length %"PRIi64, a_len, f_len);
            }
         }
      }
      else if (type_is_scalar(ftype)) {
         range_t r = type_dim(ftype, 0);
         bool checked;

         if (is_out_of_range(value, r, &checked))
            bounds_error(value, "value %s out of bounds %s %s %s for "
                         "parameter %s", value_str(value),
                         value_str(r.left),
                         (r.kind == RANGE_TO) ? "to" : "downto",
                         value_str(r.right), istr(tree_ident(port)));
      }
   }

   return t;
}

static void bounds_check_array_ref(tree_t t)
{
   tree_t value = tree_value(t);

   if (!tree_has_type(value))
      return;

   type_t value_type = tree_type(value);

   if (type_is_unconstrained(value_type))
      return;

   int nstatic = 0;
   const int nparams = tree_params(t);
   for (int i = 0; i < nparams; i++) {
      tree_t p = tree_param(t, i);
      tree_t pvalue = tree_value(p);
      range_t r = type_dim(value_type, i);
      bool checked;
      if (is_out_of_range(pvalue, r, &checked)) {
         const char *name = (tree_kind(value) == T_REF)
            ? istr(tree_ident(value)) : NULL;
         bounds_error(t, "array %s%sindex %s out of bounds %s %s %s",
                      name ? name : "", name ? " " : "",
                      value_str(pvalue), value_str(r.left),
                      (r.kind == RANGE_TO) ? "to" : "downto",
                      value_str(r.right));
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

   range_t b = type_dim(value_type, 0);
   range_t r = tree_range(t);

   if ((b.kind != RANGE_TO) && (b.kind != RANGE_DOWNTO))
      return;
   else if ((r.kind != RANGE_TO) && (r.kind != RANGE_DOWNTO))
      return;

   int64_t b_left, r_left;
   bool left_error = false;
   if (folded_int(b.left, &b_left) && folded_int(r.left, &r_left))
      left_error = ((b.kind == RANGE_TO) && (r_left < b_left))
         || ((b.kind == RANGE_DOWNTO) && (r_left > b_left));

   int64_t b_right, r_right;
   bool right_error = false;
   if (folded_int(b.right, &b_right) && folded_int(r.right, &r_right))
      right_error = ((b.kind == RANGE_TO) && (r_right > b_right))
         || ((b.kind == RANGE_DOWNTO) && (r_right < b_right));

   if (left_error || right_error) {
      const char *name = (tree_kind(value) == T_REF)
         ? istr(tree_ident(value)) : NULL;
      bounds_error(t, "%s%sslice %s index %"PRIi64" out of bounds "
                   "%"PRIi64" %s %"PRIi64,
                   name ? name : "", name ? " " : "",
                   left_error ? "left" : "right",
                   left_error ? r_left : r_right, b_left,
                   (b.kind == RANGE_TO) ? "to" : "downto", b_right);
   }
}

static void bounds_within(tree_t i, range_kind_t kind, const char *what,
                          int64_t low, int64_t high)
{
   int64_t folded;
   unsigned folded_u;
   if (folded_int(i, &folded)) {
      if (folded < low || folded > high)
         bounds_error(i, "%s index %"PRIi64" out of bounds %"PRIi64
                      " %s %"PRIi64, what, folded,
                      (kind == RANGE_TO) ? low : high,
                      (kind == RANGE_TO) ? "to" : "downto",
                      (kind == RANGE_TO) ? high : low);
   }
   else if (folded_enum(i, &folded_u)) {
      if (folded_u < low || folded_u > high) {
         type_t base = type_base_recur(tree_type(i));
         tree_t value_lit = type_enum_literal(base, folded_u);
         tree_t left_lit  = type_enum_literal(base, (kind == RANGE_TO) ? low : high);
         tree_t right_lit = type_enum_literal(base, (kind == RANGE_TO) ? high : low);

         bounds_error(i, "%s index %s out of bounds %s %s %s", what,
                      istr(tree_ident(value_lit)),
                      istr(tree_ident(left_lit)),
                      (kind == RANGE_TO) ? "to" : "downto",
                      istr(tree_ident(right_lit)));
      }
   }
}

static void bounds_check_aggregate(tree_t t)
{
   type_t type = tree_type(t);
   if (!type_is_array(type))
      return;

   assert(type_kind(type) != T_UARRAY);

   // Find the tightest bounds for the index

   int64_t low, high;
   bool have_bounds = false;

   range_t type_r = type_dim(type, 0);

   const bool unconstrained = tree_flags(t) & TREE_F_UNCONSTRAINED;

   if (unconstrained) {
      // Aggregate of unconstrained array type
      type_t base = type_base_recur(type);
      assert(type_kind(base) == T_UARRAY);

      type_t index = type_index_constr(base, 0);

      range_t base_r = type_dim(index, 0);

      have_bounds = folded_bounds(base_r, &low, &high);
   }
   else
      have_bounds = folded_bounds(type_r, &low, &high);

   if (!have_bounds)
      return;

   // Check for out of bounds indexes

   bool known_elem_count = true;
   int nelems = 0;
   const int nassocs = tree_assocs(t);
   for (int i = 0; i < nassocs; i++) {
      tree_t a = tree_assoc(t, i);

      switch (tree_subkind(a)) {
      case A_NAMED:
         bounds_within(tree_name(a), type_r.kind, "aggregate", low, high);
         nelems++;
         break;

      case A_RANGE:
         {
            range_t r = tree_range(a);
            bounds_within(r.left, r.kind, "aggregate", low, high);
            bounds_within(r.right, r.kind, "aggregate", low, high);

            int64_t length;
            if (folded_length(r, &length))
               nelems += length;
            else
               known_elem_count = false;
         }
         break;

      case A_OTHERS:
         known_elem_count = false;
         break;

      default:
         nelems++;
         break;
      }
   }

   // Check the actual against the expected element count

   const int ndims = type_dims(type);

   if (known_elem_count) {
      int64_t expect;
      if (folded_length(type_dim(type, 0), &expect) && expect != nelems)
         bounds_error(t, "expected %"PRIi64" elements in aggregate but have %d",
                      expect, nelems);
   }

   // Check each sub-aggregate has the same length for an unconstrained
   // array aggregate

   if ((ndims > 1) && unconstrained) {
      int64_t length = -1;
      for (int i = 0; i < nassocs; i++) {
         tree_t a = tree_assoc(t, i);
         type_t value_type = tree_type(tree_value(a));

         int64_t this_length;
         if (!folded_length(type_dim(value_type, 0), &this_length))
             break;

         if (length == -1)
            length = this_length;
         else if (length != this_length)
            bounds_error(a, "length of sub-aggregate %"PRIi64" does not match "
                         "expected length %"PRIi64,
                         this_length, length);
      }
   }
}

static void bounds_check_decl(tree_t t)
{
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

      const int ndims = array_dimension(base);
      for (int i = 0; i < ndims; i++) {
         range_t dim = type_dim(type, i);

         type_t cons = index_type_of(base, i);
         type_t cons_base  = type_base_recur(cons);

         const bool is_enum = (type_kind(cons_base) == T_ENUM);

         range_t bounds = type_dim(cons, 0);

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

         if (dim_low < bounds_low) {
            if (is_enum) {
               tree_t lit = type_enum_literal(cons_base, (unsigned) dim_low);
               bounds_error((dim.kind == RANGE_TO) ? dim.left : dim.right,
                            "%s index %s violates constraint %s",
                            (dim.kind == RANGE_TO) ? "left" : "right",
                            istr(tree_ident(lit)), type_pp(cons));
            }
            else
               bounds_error((dim.kind == RANGE_TO) ? dim.left : dim.right,
                            "%s index %"PRIi64" violates constraint %s",
                            (dim.kind == RANGE_TO) ? "left" : "right",
                            dim_low, type_pp(cons));

         }

         if (dim_high > bounds_high) {
            if (is_enum) {
               tree_t lit = type_enum_literal(cons_base, (unsigned) dim_high);
               bounds_error((dim.kind == RANGE_TO) ? dim.right : dim.left,
                            "%s index %s violates constraint %s",
                            (dim.kind == RANGE_TO) ? "right" : "left",
                            istr(tree_ident(lit)), type_pp(cons));
            }
            else
               bounds_error((dim.kind == RANGE_TO) ? dim.right : dim.left,
                            "%s index %"PRIi64" violates constraint %s",
                            (dim.kind == RANGE_TO) ? "right" : "left",
                            dim_high, type_pp(cons));
         }
      }
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
      const int ndims = type_dims(target_type);
      for (int i = 0; i < ndims; i++) {
         int64_t target_w, value_w;
         if (folded_length(type_dim(target_type, i), &target_w)
             && folded_length(type_dim(value_type, i), &value_w)) {
            if (target_w != value_w) {
               if (i > 0)
                  bounds_error(value, "length of dimension %d of value %"PRIi64
                               " does not match length of target %"PRIi64,
                               i + 1, value_w, target_w);
               else
                  bounds_error(value, "length of value %"PRIi64" does not "
                               "match length of target %"PRIi64,
                               value_w, target_w);
            }
         }
      }
   }

   if (type_is_scalar(target_type)) {
      range_t r = type_dim(target_type, 0);
      bool checked;

      if (is_out_of_range(value, r, &checked)) {
         bounds_error(value, "value %s out of target bounds %s %s %s",
                      value_str(value), value_str(r.left),
                      (r.kind == RANGE_TO) ? "to" : "downto",
                      value_str(r.right));
      }
   }
}

static void bounds_check_signal_assign(tree_t t)
{
   int64_t last_delay = 0;
   tree_t target = tree_target(t);

   const int nwaves = tree_waveforms(t);
   for (int i = 0; i < nwaves; i++) {
      tree_t w = tree_waveform(t, i);

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

static void bounds_case_cover(interval_t **isp, tree_t t,
                              int64_t low, int64_t high)
{
   interval_t *it, *prev;
   for (it = *isp, prev = NULL;
        (it != NULL) && (it->low <= high);
        prev = it, it = it->next) {

      if ((low <= it->high) && (it->low <= high)) {
         const int64_t rlow  = MAX(low, it->low);
         const int64_t rhigh = MIN(high, it->high);
         if (rlow == rhigh)
            bounds_error(t, "value %"PRIi64" is already covered", rlow);
         else
            bounds_error(t, "range %"PRIi64" to %"PRIi64" is already covered",
                         rlow, rhigh);
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

static void bounds_fmt_case_missing(text_buf_t *tb, int64_t low, int64_t high)
{
   if (low == high)
      tb_printf(tb, "\n    %"PRIi64, low);
   else
      tb_printf(tb, "\n    %"PRIi64" to %"PRIi64, low, high);
}

static void bounds_check_case(tree_t t)
{
   type_t type = tree_type(tree_value(t));

   if (type_is_enum(type)) {
      // Check the choices cover all elements of an enumerated type

      unsigned nlits, low, high;
      if (type_kind(type) == T_SUBTYPE) {
         assert(type_dims(type) == 1);

         range_t r = type_dim(type, 0);
         assert(r.kind == RANGE_TO);

         if ((tree_kind(r.left) != T_REF)
             || ((tree_kind(r.right) != T_REF)))
            return;

         tree_t ldecl = tree_ref(r.left);
         tree_t rdecl = tree_ref(r.right);

         assert(tree_kind(ldecl) == T_ENUM_LIT);
         assert(tree_kind(rdecl) == T_ENUM_LIT);

         low   = tree_pos(ldecl);
         high  = tree_pos(rdecl);
         nlits = high - low + 1;
      }
      else {
         nlits = type_enum_literals(type);
         low   = 0;
         high  = nlits - 1;
      }

      bool have[nlits];
      for (unsigned i = 0; i < nlits; i++)
         have[i] = false;

      type_t base = type_base_recur(type);

      bool have_others = false;

      const int nassocs = tree_assocs(t);
      for (unsigned i = 0; i < nassocs; i++) {
         tree_t a = tree_assoc(t, i);

         if (tree_subkind(a) == A_OTHERS) {
            have_others = true;
            continue;
         }

         ident_t name = tree_ident(tree_name(a));
         for (unsigned j = low; j <= high; j++) {
            if (tree_ident(type_enum_literal(base, j)) == name) {
               if (have[j - low])
                  bounds_error(tree_name(a), "choice %s appears multiple times "
                               "in case statement", istr(name));
               else
                  have[j - low] = true;
            }
         }
      }

      bool have_all = true;
      for (unsigned i = low; i <= high; i++) {
         if (!have[i - low] && !have_others)
            bounds_error(t, "missing choice %s in case statement",
                         istr(tree_ident(type_enum_literal(base, i))));
         have_all = have_all && have[i - low];
      }
   }
   else if (type_is_integer(type)) {
      // Check that the full range of the type is covered

      int64_t tlow, thigh;
      if (!folded_bounds(type_dim(type, 0), &tlow, &thigh))
         return;

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
               low = high = assume_int(tree_name(a));
            }
            break;

         case A_RANGE:
            {
               range_t r = tree_range(a);
               assert(r.kind == RANGE_TO);
               low  = assume_int(r.left);
               high = assume_int(r.right);
            }
            break;
         }

         if ((low < tlow) || (high > thigh))
            bounds_error(a, "value %"PRIi64" outside %s bounds %"PRIi64
                         " to %"PRIi64, (low < tlow) ? low : high,
                         type_pp(type), tlow, thigh);
         else
            bounds_case_cover(&covered, a, low, high);
      }

      if (!have_others) {
         LOCAL_TEXT_BUF tb = tb_new();
         tb_printf(tb, "case choices do not cover the following "
                   "values of %s:", type_pp(type));

         bool missing = false;
         int64_t walk = tlow;
         interval_t *it, *tmp;
         for (it = covered, tmp = NULL; it != NULL; it = tmp) {
            if (it->low != walk) {
               bounds_fmt_case_missing(tb, walk, it->low - 1);
               missing = true;
            }

            walk = it->high + 1;

            tmp = it->next;
            free(it);
         }

         if (walk != thigh + 1) {
            bounds_fmt_case_missing(tb, walk, thigh);
            missing = true;
         }

         if (missing)
            bounds_error(t, "%s", tb_get(tb));
      }
   }
   else if (type_is_array(type)) {
      // Calculate how many values each element has
      type_t elem = type_elem(type);
      assert(type_is_enum(elem));

      int64_t elemsz;
      if (!folded_length(type_dim(elem, 0), &elemsz))
         return;

      int64_t length;
      if (!folded_length(type_dim(type, 0), &length))
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
   tree_t value = tree_value(tree_param(t, 0));

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
         folded_bounds(type_dim(to, 0), &b_low, &b_high);
         if (folded_bounds(type_dim(to, 0), &b_low, &b_high)
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
   const predef_attr_t predef = tree_attr_int(t, builtin_i, -1);
   switch (predef) {
   case ATTR_LENGTH:
   case ATTR_LOW:
   case ATTR_HIGH:
   case ATTR_LEFT:
   case ATTR_RIGHT:
      if (tree_params(t) > 0)
      {
         type_t type = tree_type(tree_name(t));
         if (type_is_array(type) && !type_is_unconstrained(type)) {
            tree_t dim_tree = tree_value(tree_param(t, 0));

            int64_t dim;
            const bool f = folded_int(dim_tree, &dim);
            assert(f);

            if (dim < 1 || dim > type_dims(type))
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

static void bounds_visit_fn(tree_t t, void *context)
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
   default:
      break;
   }
}

void bounds_check(tree_t top)
{
   tree_visit(top, bounds_visit_fn, NULL);
}

int bounds_errors(void)
{
   return errors;
}

void reset_bounds_errors(void)
{
   errors = 0;
}
