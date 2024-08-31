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
#include "hash.h"
#include "lib.h"
#include "mask.h"
#include "names.h"
#include "option.h"
#include "phase.h"
#include "type.h"

#include <assert.h>
#include <stdlib.h>
#include <string.h>

typedef bool (*static_fn_t)(tree_t t);

typedef struct {
   tree_t decl;
   bool   have;
   bool   partial;
} formal_map_t;

static bool sem_check_array_ref(tree_t t, nametab_t *tab);
static bool sem_locally_static(tree_t t);
static bool sem_globally_static(tree_t t);
static tree_t sem_check_lvalue(tree_t t);
static bool sem_check_same_type(tree_t left, tree_t right);
static bool sem_check_type(tree_t t, type_t expect, nametab_t *tab);
static bool sem_static_name(tree_t t, static_fn_t check_fn);
static bool sem_static_subtype(type_t type, static_fn_t fn);
static bool sem_check_attr_ref(tree_t t, bool allow_range, nametab_t *tab);
static bool sem_check_generic_map(tree_t t, tree_t unit, nametab_t *tab);
static bool sem_check_port_map(tree_t t, tree_t unit, nametab_t *tab);
static bool sem_check_subtype(tree_t decl, type_t type, nametab_t *tab);
static bool sem_check_incomplete(tree_t t, type_t type);

#define sem_error(t, ...) do {                        \
      error_at(t ? tree_loc(t) : NULL , __VA_ARGS__); \
      return false;                                   \
   } while (0)

static bool sem_check_resolution(type_t type, tree_t res)
{
   // Resolution functions are described in LRM 93 section 2.4

   if (tree_kind(res) == T_ELEM_RESOLUTION) {
      // VHDL-2008 element resolution
      assert(standard() >= STD_08);

      if (type_is_array(type)) {
         tree_t sub = tree_value(tree_assoc(res, 0));
         return sem_check_resolution(type_elem(type), sub);
      }
      else if (type_is_record(type))
         sem_error(res, "sorry, record element resolution functions are not"
                   "supported yet");
      else {
         // Should have been caught during name resolution
         assert(error_count() > 0);
         return false;
      }
   }

   assert(tree_kind(res) == T_REF);

   if (!tree_has_ref(res))
      return false;

   tree_t fdecl = tree_ref(res);
   assert(is_subprogram(fdecl));

   type_t ftype = tree_type(fdecl);

   if (type_kind(ftype) != T_SIGNATURE || !type_has_result(ftype))
      sem_error(res, "resolution function name %s is not a function",
                istr(tree_ident(res)));

   // Must take a single parameter of array of base type

   if (type_params(ftype) != 1)
      sem_error(res, "resolution function must have a single argument");

   type_t param = type_param(ftype, 0);
   if (type_kind(param) != T_ARRAY)
      sem_error(res, "parameter of resolution function must be "
                "an unconstrained array type");

   if (!type_eq(type_elem(param), type))
      sem_error(res, "parameter of resolution function must be "
                "an array of %s but found %s", type_pp(type),
                type_pp(type_elem(param)));

   // Return type must be the resolved type

   if (!type_eq(type_result(ftype), type))
      sem_error(res, "result of resolution function must be %s but have %s",
                type_pp(type), type_pp(type_result(ftype)));

   return true;
}

static bool sem_check_range(tree_t r, type_t expect, nametab_t *tab)
{
   if (expect != NULL && type_is_none(expect))
      return false;   // Prevent cascading errors

   switch (tree_subkind(r)) {
   case RANGE_EXPR:
      {
         tree_t expr = tree_value(r);

         type_t type = tree_type(expr);
         if (type_is_none(type))
            return false;   // Was earlier error

         if (tree_kind(expr) != T_ATTR_REF)
            sem_error(expr, "invalid expression in range constraint");

         if (!sem_check_attr_ref(expr, true, tab))
            return false;

         if (expect && !sem_check_type(expr, expect, tab))
            sem_error(expr, "expected type of range bound to be %s but is %s",
                      type_pp(expect), type_pp(type));
      }
      break;

   case RANGE_TO:
   case RANGE_DOWNTO:
      {
         tree_t left = tree_left(r);
         if (!sem_check(left, tab))
            return false;

         tree_t right = tree_right(r);
         if (!sem_check(right, tab))
            return false;

         if (expect != NULL) {
            if (!sem_check_same_type(left, right))
               sem_error(right, "type mismatch in range: left is %s,"
                         " right is %s", type_pp(tree_type(left)),
                         type_pp(tree_type(right)));

            if (!sem_check_type(left, expect, tab))
               sem_error(r, "expected type of range bounds to be %s but"
                         " have %s", type_pp(expect), type_pp(tree_type(left)));

            // This cannot fail because we know left and right have the
            // same type and left is equal to expect, but we still need
            // to call sem_check_type for the implicit conversion
            sem_check_type(right, expect, tab);
            sem_check_type(r, expect, tab);
         }
      }
      break;
   }

   return true;
}

static bool sem_check_discrete_range(tree_t r, type_t expect, nametab_t *tab)
{
   if (!sem_check_range(r, expect ?: tree_type(r), tab))
      return false;

   const range_kind_t kind = tree_subkind(r);
   type_t type = tree_type(r);
   if (type_is_none(type) || kind == RANGE_ERROR)
      return false;

   if (!type_is_discrete(type))
      sem_error(r, "type of range bounds %s is not discrete", type_pp(type));

   // See LRM 93 section 3.2.1.1: universal integer bound must be a
   // numeric literal or attribute. Later LRMs relax the wording here.
   if (standard() < STD_00 && !relaxed_rules() && kind != RANGE_EXPR) {
      tree_t left  = tree_left(r);
      tree_t right = tree_right(r);

      type_t left_type  = tree_type(left);
      type_t right_type = tree_type(right);

      if (type_is_universal(left_type) && type_is_universal(right_type)) {
         tree_kind_t lkind = tree_kind(left);
         tree_kind_t rkind = tree_kind(right);

         const bool invalid =
            lkind != T_LITERAL && lkind != T_ATTR_REF
            && rkind != T_LITERAL && rkind != T_ATTR_REF;

         if (invalid)
            sem_error(r, "universal integer bound must be numeric"
                      " literal or attribute");
      }
   }

   return true;
}

static bool sem_check_constraint(tree_t constraint, type_t base, nametab_t *tab)
{
   const constraint_kind_t consk = tree_subkind(constraint);
   switch (consk) {
   case C_RANGE:
      if (!type_is_scalar(base))
         sem_error(constraint, "range constraint cannot be used with "
                   "non-scalar type %s", type_pp(base));
      break;

   case C_INDEX:
      if (!type_is_array(base))
         sem_error(constraint, "index constraint cannot be used with "
                   "non-array type %s", type_pp(base));
      break;

   case C_OPEN:
      if (!type_is_array(base))
         sem_error(constraint, "array constraint cannot be used with "
                   "non-array type %s", type_pp(base));
      return true;

   case C_RECORD:
      {
         if (!type_is_record(base))
            sem_error(constraint, "record element constraint cannot be used "
                      "with non-record type %s", type_pp(base));

         // Range list is overloaded to hold record element constraints
         const int nelem = tree_ranges(constraint);
         for (int i = 0; i < nelem; i++) {
            tree_t ei = tree_range(constraint, i);
            assert(tree_kind(ei) == T_ELEM_CONSTRAINT);

            if (!tree_has_ref(ei))
               return false;   // Was parse error

            tree_t decl = tree_ref(ei);
            assert(tree_kind(decl) == T_FIELD_DECL);  // Checked by parser

            type_t ftype = tree_type(decl);
            if (!type_is_unconstrained(ftype))
               sem_error(constraint, "field %s in record element constraint is "
                         "already constrained", istr(tree_ident(decl)));

            type_t sub = tree_type(ei);
            if (!sem_check_subtype(decl, sub, tab))
               return false;

            // Check for duplicate element constraints
            tree_t fi = tree_ref(ei);
            for (int j = 0; j < i; j++) {
               tree_t ej = tree_range(constraint, j);
               if (tree_pos(tree_ref(ej)) == tree_pos(fi))
                  sem_error(ei, "duplicate record element constraint for "
                            "field %s", istr(tree_ident(fi)));
            }

            if (type_kind(base) == T_SUBTYPE) {
               tree_t dup = type_constraint_for_field(base, fi);
               if (dup != NULL && !type_is_unconstrained(tree_type(dup))) {
                  diag_t *d = diag_new(DIAG_ERROR, tree_loc(ei));
                  diag_printf(d, "duplicate record element constraint for "
                              "field %s", istr(tree_ident(fi)));
                  diag_hint(d, tree_loc(dup), "constraint in subtype %s",
                            type_pp(base));
                  diag_hint(d, tree_loc(ei), "duplicate constraint here");
                  diag_emit(d);
                  return false;
               }
            }
         }

         // Code belows handles index and range constraints
         return true;
      }
   }

   if (type_is_array(base)) {
      if (type_kind(base) == T_SUBTYPE && !type_is_unconstrained(base))
         sem_error(constraint, "cannot change constraints of constrained "
                   "array type %s", type_pp(base));
   }
   else if (type_is_record(base) && standard() < STD_08)
      sem_error(constraint, "record subtype may not have constraints "
                "in VHDL-%s", standard_text(standard()));

   const int ndims_base = type_is_array(base) ? dimension_of(base) : 1;
   const int ndims = tree_ranges(constraint);

   if (ndims != ndims_base)
      sem_error(constraint, "expected %d constraints for type %s but found %d",
                ndims_base, type_pp(base), ndims);

   for (int i = 0; i < ndims; i++) {
      tree_t r = tree_range(constraint, i);
      type_t index = index_type_of(base, i);

      switch (consk) {
      case C_INDEX:
         if (!sem_check_discrete_range(r, index, tab))
            return false;
         break;

      case C_RANGE:
         if (!sem_check_range(r, index, tab))
            return false;
         break;

      default:
         break;
      }
   }

   return true;
}

static bool sem_check_subtype_helper(tree_t decl, type_t type, nametab_t *tab)
{
   // Shared code for checking subtype declarations and implicit subtypes

   type_t base = type_base(type);
   if (type_is_none(base))
      return false;
   else if (type_is_access(base))
      base = type_designated(base);

   if (type_is_protected(base))
      sem_error(decl, "subtypes may not have protected base types");
   else if (!sem_check_incomplete(decl, type))
      return false;

   type_t elem = base;
   const int ncon = type_constraints(type);
   for (int i = 0; i < ncon; i++) {
      tree_t cons = type_constraint(type, i);
      if (!sem_check_constraint(cons, elem, tab))
         return false;

      const constraint_kind_t consk = tree_subkind(cons);
      if (i + 1 < ncon && (consk == C_INDEX || consk == C_OPEN))
         elem = type_elem(elem);
   }

   if (type_is_array(type) && type_has_elem(type)) {
      type_t elem = type_elem(type);
      if (type_kind(elem) == T_SUBTYPE && !type_has_ident(elem)) {
         // Anonymous subtype created for array element constraint
         assert(standard() >= STD_08);

         if (!sem_check_subtype_helper(decl, elem, tab))
            return false;
      }
   }

   if (type_has_resolution(type)) {
      if (!sem_check_resolution(type_base(type), type_resolution(type)))
         return false;
   }

   return true;
}

static bool sem_check_subtype(tree_t decl, type_t type, nametab_t *tab)
{
   // Check an anonymous subtype at the point of use

   if (type_kind(type) != T_SUBTYPE)
      return true;
   else if (type_has_ident(type))
      return true;   // Explicitly declared subtype

   return sem_check_subtype_helper(decl, type, tab);
}

static bool sem_check_use_clause(tree_t c, nametab_t *tab)
{
   if (standard() >= STD_08) {
      tree_t unit = find_enclosing(tab, S_DESIGN_UNIT);

      if (unit != NULL && tree_kind(unit) == T_CONTEXT) {
         // LRM 08 section 13.3
         ident_t prefix = ident_until(tree_ident(c), '.');
         if (prefix == well_known(W_WORK))
            sem_error(c, "selected name in context declaration use clause "
                      "may not have WORK as a prefix");
      }
   }

   return true;
}

static bool sem_check_library_clause(tree_t t, nametab_t *tab)
{
   if (standard() >= STD_08) {
      ident_t name = tree_ident(t);
      tree_t unit = find_enclosing(tab, S_DESIGN_UNIT);

      if (unit != NULL && tree_kind(unit) == T_CONTEXT) {
         // LRM 08 section 13.3
         if (name == well_known(W_WORK))
            sem_error(t, "library clause in a context declaration may not have "
                      "logical library name WORK");
      }
   }

   return true;
}

static bool sem_check_context_clause(tree_t t, nametab_t *tab)
{
   // Ignore the implicit WORK and STD with context declarations
   const int ignore = tree_kind(t) == T_CONTEXT ? 2 : 0;

   bool ok = true;
   const int ncontexts = tree_contexts(t);
   for (int n = ignore; n < ncontexts; n++)
      ok = sem_check(tree_context(t, n), tab) && ok;

   return ok;
}

static bool sem_check_readable(tree_t t)
{
   switch (tree_kind(t)) {
   case T_REF:
      {
         if (tree_flags(t) & TREE_F_FORMAL_NAME)
            return true;   // Name appearing in formal

         tree_t decl = tree_ref(t);
         switch (tree_kind(decl)) {
         case T_PORT_DECL:
            {
               const port_mode_t mode = tree_subkind(decl);
               if (mode == PORT_OUT && standard() < STD_08) {
                  diag_t *d = diag_new(DIAG_ERROR, tree_loc(t));
                  diag_printf(d, "cannot read output port %s",
                              istr(tree_ident(t)));
                  diag_hint(d, tree_loc(decl), "%s declared with mode OUT",
                            istr(tree_ident(decl)));
                  diag_hint(d, tree_loc(t), "read here");
                  diag_hint(d, NULL, "outputs can be read with "
                            "$bold$--std=2008$$");
                  diag_emit(d);
                  return false;
               }
               else if (mode == PORT_LINKAGE)
                  sem_error(t, "linkage port %s may not be read except as "
                            "an actual corresponding to an interface of mode "
                            "linkage", istr(tree_ident(t)));
            }
            break;

         case T_PARAM_DECL:
            if (tree_subkind(decl) == PORT_OUT && standard() < STD_08)
               sem_error(t, "cannot read OUT parameter %s",
                         istr(tree_ident(t)));
            break;

         default:
            break;
         }

         return true;
      }

   case T_ARRAY_REF:
   case T_ARRAY_SLICE:
      return sem_check_readable(tree_value(t));

   default:
      return true;
   }
}

static bool sem_check_array_dims(type_t type, type_t constraint, nametab_t *tab)
{
   const int ndims = dimension_of(type);
   for (int i = 0; i < ndims; i++) {
      tree_t r = range_of(type, i);

      type_t index_type = NULL;
      if (constraint != NULL && i < dimension_of(constraint))
         index_type = index_type_of(constraint, i);

      if (!sem_check_discrete_range(r, index_type, tab))
         return false;

      if (index_type == NULL)
         index_type = tree_type(r);

      if (!sem_check_type(r, index_type, tab))
         sem_error(r, "type of bound %s does not match type of index %s",
                   type_pp(tree_type(r)),
                   type_pp(index_type));
   }

   return true;
}

static bool sem_check_type(tree_t t, type_t expect, nametab_t *tab)
{
   type_t actual = tree_type(t);

   if (type_eq_map(actual, expect, get_generic_map(tab)))
      return true;

   // Supress cascading errors
   if (type_is_none(actual) || type_is_none(expect))
      return true;

   return false;
}

static bool sem_check_same_type(tree_t left, tree_t right)
{
   type_t left_type  = tree_type(left);
   type_t right_type = tree_type(right);

   if (type_eq(left_type, right_type))
      return true;

   // Supress cascading errors
   if (type_is_none(left_type) || type_is_none(right_type))
      return true;

   return false;
}

static bool sem_has_access(type_t t)
{
   // returns true if the type is an access type or is a composite
   // type that contains a subelement of an access type
   type_t base = type_base_recur(t);

   if (type_is_access(base))
      return true;

   if (type_is_array(base))
      return sem_has_access(type_elem(base));

   if (type_is_record(base)) {
      const int nfields = type_fields(base);
      for (int i = 0; i < nfields; i++) {
         if (sem_has_access(tree_type(type_field(base, i))))
            return true;
      }
   }

   return false;
}

static bool sem_check_type_decl(tree_t t, nametab_t *tab)
{
   type_t type = tree_type(t);

   // Nothing more to do for incomplete types
   if (type_kind(type) == T_INCOMPLETE)
      return true;

   type_kind_t kind = type_kind(type);

   if (kind == T_SUBTYPE) {
      // Implicitly created subtype for a constrained array defintion
      if (!sem_check_subtype_helper(t, type, tab)) {
         // Prevent cascading errors
         // TODO: can we do this check in the parser and set T_NONE earlier?
         type_set_base(type, type_new(T_NONE));
         return false;
      }

      type = type_base(type);
      kind = type_kind(type);
      assert(kind == T_ARRAY);
   }

   switch (kind) {
   case T_ARRAY:
      {
         type_t elem_type = type_elem(type);
         if (!sem_check_subtype(t, elem_type, tab))
            return false;

         if (standard() < STD_08 && type_is_unconstrained(elem_type)) {
            diag_t *d = diag_new(DIAG_ERROR, tree_loc(t));
            diag_printf(d, "array %s cannot have unconstrained element type",
                        istr(tree_ident(t)));
            diag_hint(d, NULL, "this would be allowed with $bold$--std=2008$$");
            diag_emit(d);
            return false;
         }

         if (type_is_file(elem_type))
            sem_error(t, "array %s cannot have element of file type",
                      istr(tree_ident(t)));
         else if (type_is_protected(elem_type))
            sem_error(t, "array %s cannot have element of protected type",
                      istr(tree_ident(t)));
         else if (!sem_check_incomplete(t, elem_type))
            return false;

         const int nindex = type_indexes(type);
         for (int i = 0; i < nindex; i++) {
            type_t index_type = type_index(type, i);
            if (type_is_none(index_type))
               return false;
            else if (!type_is_discrete(index_type))
               sem_error(t, "index type %s is not discrete",
                         type_pp(index_type));
         }

         return true;
      }

   case T_ENUM:
      return true;

   case T_PHYSICAL:
      {
         const int nunits = type_units(type);
         for (int i = 0; i < nunits; i++) {
            tree_t u = type_unit(type, i);
            tree_set_type(u, type);
            if (!sem_check(u, tab))
               return false;

            tree_t value = tree_value(u);

            // LRM 08 section 5.2.4.1: the abstract literal portion
            // shall be an integer literal
            if (tree_ival(value) == 0 && tree_dval(value) != 0)
               sem_error(value, "the abstract literal portion of a secondary "
                         "unit declaration must be an integer literal");

            if (i > 0 && !sem_check_type(value, type, tab))
               sem_error(value, "secondary unit %s must have type %s",
                         istr(tree_ident(u)), type_pp(type));
         }
      }

      // Fall-through
   case T_INTEGER:
      {
         tree_t r = type_dim(type, 0);

         if (!sem_check_range(r, NULL, tab))
            return false;

         switch (tree_subkind(r)) {
         case RANGE_TO:
         case RANGE_DOWNTO:
            {
               // See LRM 93 section 3.1.2: Each bound of a range
               // constraint that must be of some integer type, but the
               // two bounds need not have the same integer type.
               tree_t left = tree_left(r), right = tree_right(r);
               if (!type_is_integer(tree_type(left)))
                  sem_error(left, "type of left bound must be of some integer "
                            "type but have %s", type_pp(tree_type(left)));
               else if (!type_is_integer(tree_type(right)))
                  sem_error(right, "type of right bound must be of some "
                            "integer type but have %s",
                            type_pp(tree_type(right)));
            }
            break;

         case RANGE_EXPR:
            if (!type_is_integer(tree_type(r)))
               sem_error(r, "type of range bounds must be of some integer "
                         "type but have %s", type_pp(tree_type(r)));
            break;
         }

         if (!sem_locally_static(r))
            sem_error(r, "range constraint of type %s must be locally static",
                      type_pp(type));

         tree_set_type(r, type);
         return true;
      }

   case T_REAL:
      {
         tree_t r = type_dim(type, 0);

         if (!sem_check_range(r, NULL, tab))
            return false;

         switch (tree_subkind(r)) {
         case RANGE_TO:
         case RANGE_DOWNTO:
            {
               // See LRM 93 section 3.1.4: Each bound of a range
               // constraint that must be of some floating-point type, but
               // the two bounds need not have the same floating-point type.
               tree_t left = tree_left(r), right = tree_right(r);
               if (!type_is_real(tree_type(left)))
                  sem_error(left, "type of left bound must be of some "
                            "floating-point type but have %s",
                            type_pp(tree_type(left)));
               else if (!type_is_real(tree_type(right)))
                  sem_error(right, "type of right bound must be of some "
                            "floating-point type but have %s",
                            type_pp(tree_type(right)));
            }
            break;

         case RANGE_EXPR:
            assert(type_is_real(tree_type(r)));   // Unreachable
            break;
         }

         if (!sem_locally_static(r))
            sem_error(r, "range constraint of type %s must be locally static",
                      type_pp(type));

         tree_set_type(r, type);
         return true;
      }

   case T_RECORD:
      {
         const int nfields = type_fields(type);
         for (int i = 0; i < nfields; i++) {
            tree_t f = type_field(type, i);

            if (!sem_check(f, tab))
               return false;

            // Each field name must be distinct
            ident_t f_name = tree_ident(f);
            for (int j = 0; j < i; j++) {
               tree_t fj = type_field(type, j);
               if (f_name == tree_ident(fj)) {
                  diag_t *d = diag_new(DIAG_ERROR, tree_loc(f));
                  diag_printf(d, "duplicate field name %s", istr(f_name));
                  diag_hint(d, tree_loc(fj), "previously declared here");
                  diag_hint(d, tree_loc(f), "declared again here");
                  diag_emit(d);
                  return false;
               }
            }

            type_t f_type = tree_type(f);

            if (!sem_check_subtype(f, f_type, tab))
               return false;

            // Recursive record types are not allowed
            if (type_eq(type, f_type))
               sem_error(f, "recursive record types are not allowed");

            // Element types may not be unconstrained before VHDL-2008
            if (standard() < STD_08 && type_is_unconstrained(f_type)) {
               diag_t *d = diag_new(DIAG_ERROR, tree_loc(f));
               diag_printf(d, "record field %s cannot have unconstrained "
                           "array type in VHDL-%s", istr(f_name),
                           standard_text(standard()));
               diag_hint(d, NULL, "pass $bold$--std=2008$$ to enable this "
                         "feature");
               diag_emit(d);
               return false;
            }
            else if (type_is_file(f_type))
               sem_error(f, "record field %s cannot be of file type",
                         istr(f_name));
            else if (type_is_protected(f_type))
               sem_error(f, "record field %s cannot be of protected type",
                         istr(f_name));
         }

         return true;
      }

   case T_FILE:
      // Rules for file types are in LRM 93 section 3.4
      {
         type_t f = type_designated(type);

         switch (type_kind(f)) {
         case T_ACCESS:
            sem_error(t, "files may not be of access type");
            break;
         case T_FILE:
            sem_error(t, "files may not be of file type");
            break;
         case T_PROTECTED:
            sem_error(t, "files may not be of protected type");
            break;
         default:
            break;
         }

         if (sem_has_access(f))
            sem_error(t, "type %s has a subelement with an access type",
                      type_pp(f));

         type_t base_f = type_base_recur(f);
         if (type_is_array(base_f) && dimension_of(base_f) > 1)
            sem_error(t, "array type for file type must be one-dimensional");

         return true;
      }

   case T_ACCESS:
      // Rules for access types are in LRM 93 section 3.3
      {
         type_t designated = type_designated(type);

         if (!sem_check_subtype(t, designated, tab))
            return false;

         if (standard() < STD_19) {
            if (type_is_file(designated))
               sem_error(t, "access type %s cannot designate file type",
                         istr(tree_ident(t)));

            if (type_is_protected(designated))
               sem_error(t, "access type %s cannot designate protected type",
                         istr(tree_ident(t)));
         }

         return true;
      }

   case T_PROTECTED:
   default:
      return true;
   }
}

static bool sem_check_subtype_decl(tree_t t, nametab_t *tab)
{
   type_t type = tree_type(t);
   assert(type_kind(type) == T_SUBTYPE);
   assert(type_has_ident(type));

   return sem_check_subtype_helper(t, type, tab);
}

static bool sem_check_incomplete(tree_t t, type_t type)
{
   if (type_is_incomplete(type)) {
      diag_t *d = diag_new(DIAG_ERROR, tree_loc(t));
      diag_printf(d, "invalid use of incomplete type %s", type_pp(type));
      diag_hint(d, NULL, "prior to the end of the corresponding full type "
                "declaration, an incomplete type may only be used as the "
                "designated type of an access type declaration");
      diag_lrm(d, STD_08, "5.4.2");
      diag_emit(d);

      return false;
   }

   return true;
}

static bool sem_no_access_file_or_protected(tree_t t, type_t type, const char *what)
{
   // constants, signals, attributes, generics, ports
   // may not be of an access, file, or protected type, or
   // of a composite type with a subelement of an access type

   if (type_is_access(type))
      sem_error(t, "%s may not have access type", what);

   if (sem_has_access(type))
      sem_error(t, "%s may not have a type with a subelement of access type", what);

   if (type_is_protected(type))
      sem_error(t, "%s may not have protected type", what);

   if (type_is_file(type))
      sem_error(t, "%s may not have file type", what);

   return true;
}

static void sem_unconstrained_decl_hint(diag_t *d, type_t type)
{
   if (!type_is_record(type))
      return;

   // Tell the user which field is unconstrained

   type_t base = type_base_recur(type);
   const int nfields = type_fields(base);
   for (int i = 0; i < nfields; i++) {
      tree_t f = type_field(base, i);
      if (!type_is_unconstrained(tree_type(f)))
         continue;
      else if (type_constraint_for_field(type, f) == NULL)
         diag_hint(d, NULL, "missing record element constraint for field %s",
                   istr(tree_ident(f)));
   }
}

static void sem_propagate_constraints(tree_t decl, tree_t value)
{
   // Propagate the constraints from an initial value to an object
   // declaration with unconstrained type but only if the object subtype
   // has no constraints of its own

   if (standard() < STD_19 && tree_kind(decl) != T_CONST_DECL)
      return;

   type_t type = tree_type(decl);
   if (!type_is_unconstrained(type))
      return;

   for (type_t iter = type; type_kind(iter) == T_SUBTYPE;
        iter = type_base(iter)) {
      if (type_constraints(iter) > 0)
         return;
   }

   tree_set_type(decl, tree_type(value));
}

static bool sem_check_const_decl(tree_t t, nametab_t *tab)
{
   type_t type = tree_type(t);

   if (!sem_check_subtype(t, type, tab))
      return false;
   else if (type_is_none(type))
      return false;
   else if (!sem_check_incomplete(t, type))
      return false;

   if (!sem_no_access_file_or_protected(t, type, "constants"))
      return false;

   tree_t fwd = find_forward_decl(tab, t);

   if (tree_has_value(t)) {
      tree_t value = tree_value(t);
      if (!sem_check(value, tab))
         return false;

      if (!sem_check_type(value, type, tab))
         sem_error(value, "type of initial value %s does not match type "
                   "of declaration %s", type_pp2(tree_type(value), type),
                   type_pp2(type, tree_type(value)));

      if (fwd == NULL)
         sem_propagate_constraints(t, value);
   }
   else if (tree_kind(find_enclosing(tab, S_DESIGN_UNIT)) != T_PACKAGE)
      sem_error(t, "deferred constant declarations are only permitted "
                "in packages");

   if (fwd != NULL && !type_strict_eq(tree_type(fwd), type)) {
      diag_t *d = diag_new(DIAG_ERROR, tree_loc(t));
      diag_printf(d, "expected type %s for deferred constant %s but "
                  "found %s", type_pp2(tree_type(fwd), type),
                  istr(tree_ident(t)), type_pp2(type, tree_type(fwd)));
      diag_hint(d, tree_loc(fwd), "originally declared with type %s",
                type_pp2(tree_type(fwd), type));
      diag_hint(d, tree_loc(t), "type here is %s",
                type_pp2(type, tree_type(fwd)));
      diag_emit(d);
      return false;
   }

   return true;
}

static bool sem_check_signal_decl(tree_t t, nametab_t *tab)
{
   type_t type = tree_type(t);

   if (!sem_check_subtype(t, type, tab))
      return false;
   else if (type_is_none(type))
      return false;

   if (type_is_unconstrained(type)) {
      if (standard() < STD_19) {
         diag_t *d = diag_new(DIAG_ERROR, tree_loc(t));
         diag_printf(d, "declaration of signal %s cannot have unconstrained "
                     "type %s", istr(tree_ident(t)), type_pp(type));
         sem_unconstrained_decl_hint(d, type);
         diag_emit(d);
         return false;
      }
      else if (!tree_has_value(t)) {
         diag_t *d = diag_new(DIAG_ERROR, tree_loc(t));
         diag_printf(d, "declaration of signal %s without an initial value "
                     "cannot have unconstrained type %s",
                     istr(tree_ident(t)), type_pp(type));
         sem_unconstrained_decl_hint(d, type);
         diag_emit(d);
         return false;
      }
   }
   else if (!sem_check_incomplete(t, type))
      return false;

   if (!sem_no_access_file_or_protected(t, type, "signals"))
      return false;

   if (is_guarded_signal(t) && !type_is_resolved(type))
      sem_error(t, "guarded signal must have resolved subtype");

   if (tree_has_value(t)) {
      tree_t value = tree_value(t);
      if (!sem_check(value, tab))
         return false;

      if (!sem_check_type(value, type, tab))
         sem_error(value, "type of initial value %s does not match type "
                   "of declaration %s", type_pp2(tree_type(value), type),
                   type_pp2(type, tree_type(value)));

      if (standard() >= STD_19 && type_is_unconstrained(type))
         tree_set_type(t, tree_type(value));
   }

   return true;
}

static bool sem_check_var_decl(tree_t t, nametab_t *tab)
{
   type_t type = tree_type(t);

   if (!sem_check_subtype(t, type, tab))
      return false;
   else if (type_is_none(type))
      return false;

   if (type_is_unconstrained(type)) {
      if (standard() < STD_19) {
         diag_t *d = diag_new(DIAG_ERROR, tree_loc(t));
         diag_printf(d, "declaration of variable %s cannot have unconstrained "
                     "type %s", istr(tree_ident(t)), type_pp(type));
         sem_unconstrained_decl_hint(d, type);
         diag_emit(d);
         return false;
      }
      else if (!tree_has_value(t)) {
         diag_t *d = diag_new(DIAG_ERROR, tree_loc(t));
         diag_printf(d, "declaration of variable %s without an initial value "
                     "cannot have unconstrained type %s", istr(tree_ident(t)),
                     type_pp(type));
         sem_unconstrained_decl_hint(d, type);
         diag_emit(d);
         return false;
      }
   }
   else if (!sem_check_incomplete(t, type))
      return false;

   if (tree_has_value(t)) {
      if (type_kind(type) == T_PROTECTED)
         sem_error(t, "variable %s with protected type may not have an "
                   "initial value", istr(tree_ident(t)));

      tree_t value = tree_value(t);
      if (!sem_check(value, tab))
         return false;

      if (!sem_check_type(value, type, tab))
         sem_error(value, "type of initial value %s does not match type "
                   "of declaration %s", type_pp2(tree_type(value), type),
                   type_pp2(type, tree_type(value)));

      sem_propagate_constraints(t, value);
   }

   // From VHDL-2000 onwards shared variables must be protected types
   if (standard() >= STD_00) {
      if ((tree_flags(t) & TREE_F_SHARED) && !type_is_protected(type)) {
         diag_t *d = pedantic_diag(tree_loc(t));
         if (d != NULL) {
            diag_printf(d, "shared variable %s must have protected type",
                        istr(tree_ident(t)));
            diag_emit(d);
         }
      }
   }

   if (type_is_protected(type)) {
      ident_t typeid = ident_rfrom(type_ident(type), '.');
      tree_t pt = get_local_decl(tab, NULL, typeid, 0);
      if (pt != NULL && tree_kind(pt) == T_PROT_DECL) {
         diag_t *d = diag_new(DIAG_ERROR, tree_loc(t));
         diag_printf(d, "cannot declare instance of protected type %s "
                     "before its body has been elaborated", type_pp(type));
         diag_hint(d, tree_loc(pt), "%s declared here",
                   type_pp(type));
         diag_lrm(d, STD_08, "14.4.2");
         diag_emit(d);
         return false;
      }

      // The 2019 standard needs access to the instance and path name at
      // the point of declaration
      if (standard() >= STD_19)
         tree_set_global_flags(t, TREE_GF_INSTANCE_NAME | TREE_GF_PATH_NAME);
   }

   return true;
}

static bool sem_check_param_decl(tree_t t, nametab_t *tab)
{
   type_t type = tree_type(t);

   if (!sem_check_subtype(t, type, tab))
      return false;
   else if (!sem_check_incomplete(t, type))
      return false;

   // See LRM 93 section 3.3 for restrictions

   const type_kind_t kind = type_base_kind(type);
   const class_t class = tree_class(t);
   const port_mode_t mode = tree_subkind(t);

   switch (mode) {
   case PORT_BUFFER:
      sem_error(t, "subprogram formal parameters cannot have mode BUFFER");
      break;
   case PORT_LINKAGE:
      sem_error(t, "subprogram formal parameters cannot have mode LINKAGE");
      break;
   default:
      break;
   }

   if (kind == T_FILE && class != C_FILE)
      sem_error(t, "formal parameter %s with file type must have class FILE",
                istr(tree_ident(t)));

   if (kind != T_FILE && class == C_FILE)
      sem_error(t, "formal parameter %s with class FILE must have file type",
                istr(tree_ident(t)));

   if ((kind == T_ACCESS || kind == T_PROTECTED) && class != C_VARIABLE)
      sem_error(t, "formal parameter %s with %s type must have class VARIABLE",
                istr(tree_ident(t)),
                kind == T_ACCESS ? "access" : "protected");

   if (sem_has_access(type) && class != C_VARIABLE)
      sem_error(t, "formal parameter %s with type containing an access type "
                "must have class VARIABLE", istr(tree_ident(t)));

   if (class == C_CONSTANT && mode != PORT_IN)
      sem_error(t, "parameter of class CONSTANT must have mode IN");

   // LRM 08 section 4.2.2.3
   if (class == C_SIGNAL && tree_flags(t) & TREE_F_BUS)
      sem_error(t, "formal signal parameter declaration may "
                "not include the reserved word BUS");

   if (mode == PORT_RECORD_VIEW || mode == PORT_ARRAY_VIEW) {
      tree_t name = tree_value(t);
      type_t view_type = tree_type(name);

      if (type_is_none(view_type))
         return false;

      if (type_kind(view_type) != T_VIEW)
         sem_error(name, "name in mode view indication of parameter %s does "
                   "not denote a mode view", istr(tree_ident(t)));

      type_t elem_type = type;
      if (mode == PORT_ARRAY_VIEW) {
         if (!type_is_array(type))
            sem_error(t, "parameter %s with array mode view indication has "
                      "non-array type %s", istr(tree_ident(t)), type_pp(type));

         elem_type = type_elem(type);
      }

      if (!type_eq(elem_type, type_designated(view_type)))
         sem_error(t, "subtype %s is not compatible with mode "
                   "view %s", type_pp(elem_type), type_pp(view_type));
   }
   else if (tree_has_value(t)) {
      tree_t value = tree_value(t);
      if (!sem_check(value, tab))
         return false;

      if (!sem_check_type(value, type, tab))
         sem_error(value, "type of default value %s does not match type "
                   "of declaration %s", type_pp(tree_type(value)),
                   type_pp(type));

      switch (class) {
      case C_SIGNAL:
         sem_error(t, "parameter of class SIGNAL cannot have a "
                   "default value");
         break;

      case C_VARIABLE:
         if (mode == PORT_OUT || mode == PORT_INOUT)
            sem_error(t, "parameter of class VARIABLE with mode OUT or "
                      "INOUT cannot have a default value");
         break;

      default:
         break;
      }

      if (!sem_globally_static(value)) {
         diag_t *d = pedantic_diag(tree_loc(value));
         if (d != NULL) {
            diag_printf(d, "default value must be a static expression");
            diag_emit(d);
         }
      }

      if (kind == T_PROTECTED)
         sem_error(t, "parameter with protected type cannot have "
                   "a default value");
   }

   return true;
}

static bool sem_check_port_decl(tree_t t, nametab_t *tab)
{
   type_t type = tree_type(t);

   if (type_is_none(type))
      return false;
   else if (!sem_check_subtype(t, type, tab))
      return false;
   else if (!sem_check_incomplete(t, type))
      return false;

   const class_t class = tree_class(t);
   const port_mode_t mode = tree_subkind(t);

   if (class == C_VARIABLE) {
      if (standard() < STD_19) {
         diag_t *d = diag_new(DIAG_ERROR, tree_loc(t));
         diag_printf(d, "ports may not have variable class in VHDL-%s",
                     standard_text(standard()));
         diag_hint(d, NULL, "pass $bold$--std=2019$$ to enable this "
                   "feature");
         diag_emit(d);
         return false;
      }

      if (mode != PORT_INOUT)
         sem_error(t, "formal variable port %s must have mode INOUT",
                   istr(tree_ident(t)));

      if (!type_is_protected(type))
         sem_error(t, "formal variable port %s must have protected type",
                   istr(tree_ident(t)));
   }
   else if (class != C_SIGNAL)
      sem_error(t, "invalid object class %s for port %s",
                class_str(class), istr(tree_ident(t)));

   if (type_is_access(type))
      sem_error(t, "port %s cannot be declared with access type %s",
                istr(tree_ident(t)), type_pp(type));

   if (sem_has_access(type))
      sem_error(t, "port %s cannot be declared with type %s which has a "
                "subelement of access type", istr(tree_ident(t)),
                type_pp(type));

   if (class != C_VARIABLE && type_is_protected(type)) {
      diag_t *d = diag_new(DIAG_ERROR, tree_loc(t));
      diag_printf(d, "port %s with class %s cannot be declared with "
                  "protected type %s", istr(tree_ident(t)),
                  class_str(class), type_pp(type));
      diag_hint(d, NULL, "ports with variable class can be of protected "
                "type in VHDL-2019");
      diag_hint(d, NULL, "pass $bold$--std=2019$$ to enable this "
                "feature");
      diag_emit(d);
      return false;
   }

   if (type_is_file(type))
      sem_error(t, "port %s cannot be declared with file type %s",
                istr(tree_ident(t)), type_pp(type));

   if (mode == PORT_RECORD_VIEW || mode == PORT_ARRAY_VIEW) {
      tree_t name = tree_value(t);
      type_t view_type = tree_type(name);

      if (type_is_none(view_type))
         return false;

      if (type_kind(view_type) != T_VIEW)
         sem_error(name, "name in mode view indication of port %s does not "
                   "denote a mode view", istr(tree_ident(t)));

      type_t elem_type = type;
      if (mode == PORT_ARRAY_VIEW) {
         if (!type_is_array(type))
            sem_error(t, "port %s with array mode view indication has "
                      "non-array type %s", istr(tree_ident(t)), type_pp(type));

         elem_type = type_elem(type);
      }

      if (!type_eq(elem_type, type_designated(view_type)))
         sem_error(t, "subtype %s is not compatible with mode "
                   "view %s", type_pp(elem_type), type_pp(view_type));
   }
   else if (tree_has_value(t)) {
      tree_t value = tree_value(t);
      if (!sem_check(value, tab))
         return false;

      if (mode == PORT_LINKAGE)
         sem_error(t, "port with mode LINKAGE cannot have a default value");

      if (!sem_check_type(value, type, tab))
         sem_error(value, "type of default value %s does not match type "
                   "of declaration %s", type_pp(tree_type(value)),
                   type_pp(type));
   }

   return true;
}

static bool sem_check_generic_decl(tree_t t, nametab_t *tab)
{
   const class_t class = tree_class(t);
   switch (class) {
   case C_CONSTANT:
   case C_TYPE:
   case C_FUNCTION:
   case C_PROCEDURE:
      break;

   case C_PACKAGE:
      {
         tree_t map = tree_value(t);
         if (!tree_has_ref(map))
            return false;   // Was earlier error

         assert(tree_kind(map) == T_PACKAGE_MAP);

         tree_t pack = tree_ref(map);
         assert(is_uninstantiated_package(pack));

         switch (tree_subkind(map)) {
         case PACKAGE_MAP_DEFAULT:
            {
               // Check each generic in the uninstantiated package has a
               // default value
               const int ngenerics = tree_generics(pack);
               for (int i = 0; i < ngenerics; i++) {
                  tree_t g = tree_generic(pack, i);
                  if (!tree_has_value(g)) {
                     diag_t *d = diag_new(DIAG_ERROR, tree_loc(map));
                     diag_printf(d, "generic %s in package %s does not have a "
                                 "default value", istr(tree_ident(g)),
                                 istr(tree_ident(pack)));
                     diag_hint(d, tree_loc(g), "%s declared here",
                               istr(tree_ident(g)));
                     diag_lrm(d, STD_08, "6.5.5");

                     diag_emit(d);
                     return false;
                  }
               }
            }
            break;

         case PACKAGE_MAP_MATCHING:
            sem_check_generic_map(map, pack, tab);
            break;

         case PACKAGE_MAP_BOX:
            break;
         }

         return true;
      }
   default:
      sem_error(t, "invalid object class %s for generic %s",
                class_str(tree_class(t)), istr(tree_ident(t)));
   }

   type_t type = tree_type(t);

   if (!sem_check_subtype(t, type, tab))
      return false;
   else if (type_is_none(type))
      return false;
   else if (!sem_check_incomplete(t, type))
      return false;

   if (class != C_TYPE) {
      if (type_is_access(type))
         sem_error(t, "generic %s may not have access type",
                   istr(tree_ident(t)));
      else if (sem_has_access(type))
         sem_error(t, "generic %s may not have a type with a subelement of "
                   "access type", istr(tree_ident(t)));
      else if (type_is_protected(type))
         sem_error(t, "generic %s may not have protected type",
                   istr(tree_ident(t)));
      else if (type_is_file(type))
         sem_error(t, "generic %s may not have file type", istr(tree_ident(t)));
   }

   if (class == C_CONSTANT && tree_subkind(t) != PORT_IN)
      sem_error(t, "generic %s with class CONSTANT must have mode IN",
                istr(tree_ident(t)));

   if (tree_has_value(t)) {
      tree_t value = tree_value(t);
      if (!sem_check(value, tab))
         return false;

      if (!sem_check_type(value, type, tab))
         sem_error(value, "type of default value %s does not match type "
                   "of declaration %s", type_pp(tree_type(value)),
                   type_pp(type));
   }

   return true;
}

static bool sem_check_field_decl(tree_t t)
{
   type_t type = tree_type(t);

   if (!sem_check_incomplete(t, type))
      return false;

   return true;
}

static bool sem_check_unit_decl(tree_t t)
{
   return true;
}

static bool sem_check_alias(tree_t t, nametab_t *tab)
{
   // Rules for aliases are given in LRM 93 section 4.3.3

   tree_t value = tree_value(t);
   type_t type = get_type_or_null(t);

   const tree_kind_t value_kind = tree_kind(value);

   if (type != NULL && type_is_subprogram(type)) {
      // Alias of subprogram or enumeration literal
      // Rules for matching signatures are in LRM 93 section 2.3.2
      assert(tree_kind(value) == T_REF || tree_kind(value) == T_PROT_REF);
      return true;
   }
   else if (tree_flags(t) & TREE_F_NONOBJECT_ALIAS) {
      // TODO: cannot be label
      return true;
   }
   else if (value_kind == T_REF && tree_has_ref(value)) {
      tree_t decl = tree_ref(value);
      if (aliased_type_decl(decl) != NULL)
         return true;   // Alias of type
      else if (tree_kind(decl) == T_VIEW_DECL)
         return true;   // Alias of view declaration
   }
   else if (value_kind == T_ATTR_REF && is_type_attribute(tree_subkind(value)))
      return true;   // Alias of type

   // Alias of object
   if (!sem_check(value, tab))
      return false;

   if (value_kind == T_ATTR_REF && tree_subkind(value) == ATTR_CONVERSE) {
      // Special case handling for
      //   https://gitlab.com/IEEE-P1076/VHDL-Issues/-/issues/293
      return true;
   }
   else if (!sem_static_name(value, sem_globally_static)) {
      diag_t *d = diag_new(DIAG_ERROR, tree_loc(value));
      diag_printf(d, "aliased name is not static");
      diag_lrm(d, STD_93, "6.1");
      diag_emit(d);
      return false;
   }

   if (type != NULL) {
      // Alias declaration had optional subtype indication

      if (!sem_check_subtype(t, type, tab))
         return false;

      if (!sem_check_type(value, type, tab))
         sem_error(t, "type of aliased object %s does not match expected "
                   "type %s", type_pp2(tree_type(value), type),
                   type_pp2(type, tree_type(value)));

      if (opt_get_int(OPT_RELAXED) && type_is_unconstrained(type)) {
         // If the type of the aliased object is unconstrained then
         // use its subtype instead of the subtype declared by the
         // alias.  This is required for some UVVM sources.
         type_t obj_type = tree_type(value);
         if (!type_is_unconstrained(obj_type))
            tree_set_type(t, obj_type);
      }
   }
   else
      type = tree_type(value);

   if (standard() < STD_08 && type_is_array(type) && dimension_of(type) > 1) {
      diag_t *d = diag_new(DIAG_ERROR, tree_loc(t));
      diag_printf(d, "object alias may not have multidimensional array type "
                  "in VHDL-%s", standard_text(standard()));
      diag_lrm(d, STD_93, "4.3.3.1");
      diag_emit(d);
      return false;
   }

   return true;
}

static void sem_missing_body_cb(tree_t t, tree_t parent, nametab_t *tab)
{
   if (parent == NULL || !opt_get_int(OPT_MISSING_BODY))
      return;

   const tree_kind_t kind = tree_kind(parent);
   if (kind == T_PACKAGE || kind == T_PROT_DECL)
      return;   // Should be in body

   type_t type = tree_type(t);
   tree_t body = NULL;
   int nth = 0;
   do {
      body = get_local_decl(tab, NULL, tree_ident(t), nth++);
   } while (body != NULL && !type_eq(tree_type(body), type));

   if (body == NULL || !is_body(body)) {
      diag_t *d = diag_new(DIAG_ERROR, tree_loc(t));
      diag_printf(d, "missing body for ");
      switch (tree_kind(t)) {
      case T_PROT_DECL: diag_printf(d, "protected type "); break;
      case T_PROC_DECL: diag_printf(d, "procedure "); break;
      case T_FUNC_DECL: diag_printf(d, "function "); break;
      default: break;
      }

      diag_printf(d, "%s", type_pp(type));
      diag_suppress(d, type_has_error(type));

      diag_hint(d, tree_loc(parent), "body not found in %s",
                istr(tree_ident(parent)));

      diag_hint(d, tree_loc(t), "%s declared here", type_pp(type));

      diag_emit(d);
   }
}

static bool sem_check_func_ports(tree_t t, nametab_t *tab)
{
   const vhdl_standard_t std = standard();

   ident_t id = tree_ident(t);
   if (is_operator_symbol(id)) {
      const int nports = tree_ports(t);
      const well_known_t wk = is_well_known(id);
      if (wk >= W_OP_AND && wk <= W_OP_XNOR) {
         if (std >= STD_08 && (nports < 1 || nports > 2))
            sem_error(t, "logical operator must have either one or two "
                      "operands");
         else if (std < STD_08 && nports != 2)
            sem_error(t, "logical operator must have two operands");
      }
      else if (wk >= W_OP_ABS && wk <= W_OP_CCONV && nports != 1)
         sem_error(t, "unary operator must have one operand");
      else if ((wk >= W_OP_EQUAL && wk <= W_OP_EXPONENT && nports != 2)
               || (wk >= W_OP_MATCH_EQUAL && wk <= W_OP_MATCH_GREATER_EQUAL
                   && nports != 2)
               || (wk >= W_OP_ADD && wk <= W_OP_MINUS
                   && (nports < 1 || nports > 2)))
         sem_error(t, "binary operator must have two operands");
   }

   const bool pure = !(tree_flags(t) & TREE_F_IMPURE);

   if (!pure && std >= STD_19)
      return true;   // LCS2016_002 relaxed rules for impure functions

   const int nports = tree_ports(t);
   for (int i = 0; i < nports; i++) {
      tree_t p = tree_port(t, i);
      if (tree_subkind(p) != PORT_IN) {
         diag_t *d = diag_new(DIAG_ERROR, tree_loc(p));
         diag_printf(d, "%sfunction parameters must have mode IN",
                     std < STD_19 ? "" : "pure ");
         diag_hint(d, tree_loc(p), "parameter %s has mode %s",
                   istr(tree_ident(p)), port_mode_str(tree_subkind(p)));
         diag_lrm(d, STD_08, "4.2.2.1");
         diag_emit(d);
      }
      else if (tree_class(p) == C_VARIABLE) {
         diag_t *d = diag_new(DIAG_ERROR, tree_loc(p));
         diag_printf(d, "class of %sfunction parameters must be CONSTANT, "
                     "SIGNAL, or FILE", std < STD_19 ? "" : "pure ");
         diag_hint(d, tree_loc(p), "parameter %s has class %s",
                   istr(tree_ident(p)), class_str(tree_class(p)));
         diag_lrm(d, STD_08, "4.2.2.1");
         diag_emit(d);
      }
   }

   return true;
}

static bool sem_check_protected_method(tree_t t, nametab_t *tab)
{
   if (standard() >= STD_19)
      return true;   // Relaxed in LCS2016_04

   const int nports = tree_ports(t);
   for (int i = 0; i < nports; i++) {
      tree_t p = tree_port(t, i);
      type_t type = tree_type(p);

      if (sem_has_access(type)) {
         diag_t *d = pedantic_diag(tree_loc(p));
         if (d != NULL) {
            diag_printf(d, "parameters of protected type methods cannot be of "
                        "an access type or a composite type containing an "
                        "access type");
            if (type_is_access(type))
               diag_hint(d, tree_loc(p), "type of %s is %s which is an "
                         "access type", istr(tree_ident(p)), type_pp(type));
            else
               diag_hint(d, tree_loc(p), "type of %s is %s which has an "
                         "access type as a subelement",
                         istr(tree_ident(p)), type_pp(type));
            diag_lrm(d, STD_08, "5.6.2");
            diag_emit(d);
         }
      }
      else if (type_is_file(type)) {
         diag_t *d = pedantic_diag(tree_loc(p));
         if (d != NULL) {
            diag_printf(d, "parameters of protected type methods cannot be of "
                        "a file type");
            diag_hint(d, tree_loc(p), "type of %s is %s which is a file type",
                      istr(tree_ident(p)), type_pp(type));
            diag_lrm(d, STD_08, "5.6.2");
            diag_emit(d);
         }
      }
   }

   if (tree_kind(t) == T_FUNC_DECL) {
      type_t result = type_result(tree_type(t));
      if (sem_has_access(result) || type_is_file(result)) {
         diag_t *d = pedantic_diag(tree_loc(t));
         if (d != NULL) {
            diag_printf(d, "return type of a protected type method cannot be "
                        "of a file type, access type, or a composite type with "
                        "a subelement that is an access type");
            diag_lrm(d, STD_08, "5.6.2");
            diag_emit(d);
         }
      }
   }

   return true;
}

static bool sem_check_func_result(tree_t t)
{
   type_t result = type_result(tree_type(t));

   if (type_is_protected(result))
      sem_error(t, "function result subtype may not denote a protected type");
   else if (type_is_file(result))
      sem_error(t, "function result subtype may not denote a file type");
   else if (!sem_check_incomplete(t, result))
      return false;

   return true;
}

static bool sem_check_func_decl(tree_t t, nametab_t *tab)
{
   const tree_flags_t flags = tree_flags(t);
   if (flags & TREE_F_PREDEFINED)
      return true;

   if (!sem_check_func_ports(t, tab))
      return false;

   if (!sem_check_func_result(t))
      return false;

   if ((flags & TREE_F_PROTECTED) && !sem_check_protected_method(t, tab))
      return false;

   defer_check(tab, sem_missing_body_cb, t);
   return true;
}

static bool sem_compare_interfaces(tree_t decl, tree_t body, int nth,
                                   tree_t (*getfn)(tree_t, unsigned),
                                   const char *what)
{
   tree_t dport = (*getfn)(decl, nth);
   tree_t bport = (*getfn)(body, nth);

   tree_flags_t dflags = tree_flags(dport);
   tree_flags_t bflags = tree_flags(bport);

   ident_t dname = tree_ident(dport);
   ident_t bname = tree_ident(bport);

   if (dname != bname) {
      diag_t *d = diag_new(DIAG_ERROR, tree_loc(bport));
      diag_printf(d, "%s name %s in subprogram %s body does not match "
                  "name %s in declaration", what,
                  istr(bname), istr(tree_ident(body)), istr(dname));
      diag_hint(d, tree_loc(dport), "%s %s has name %s in specification",
                ordinal_str(nth + 1), what, istr(dname));
      diag_hint(d, tree_loc(bport), "%s %s has name %s in body",
                ordinal_str(nth + 1), what, istr(bname));
      diag_emit(d);
      return false;
   }

   const bool dcont = !!(tree_flags(dport) & TREE_F_CONTINUATION);
   const bool bcont = !!(tree_flags(bport) & TREE_F_CONTINUATION);

   if (dcont != bcont) {
      diag_t *d = pedantic_diag(tree_loc(bport));
      if (d != NULL) {
         diag_printf(d, "declaration of %s %s in subprogram body does not "
                     "match specification", what, istr(bname));

         int bseq = 1;
         for (tree_t it = bport; tree_flags(it) & TREE_F_CONTINUATION;
              it = (*getfn)(body, nth - bseq++));

         diag_hint(d, tree_loc(bport), "%s appears %s in identifier list",
                   istr(bname), ordinal_str(bseq));

         int dseq = 1;
         for (tree_t it = dport; tree_flags(it) & TREE_F_CONTINUATION;
              it = (*getfn)(decl, nth - dseq++));

         diag_hint(d, tree_loc(dport), "%s appears %s in identifier list",
                   istr(dname), ordinal_str(dseq));

         diag_lrm(d, STD_08, "4.10");
         diag_emit(d);
         return false;
      }
   }
   else if (dcont && bcont)
      return true;   // Already checked first declaration in list

   type_t dtype = tree_type(dport);
   type_t btype = tree_type(bport);

   // Do not use type_eq here as subtype must exactly match
   if (!type_strict_eq(btype, dtype)) {
     diag_t *d = diag_new(DIAG_ERROR, tree_loc(bport));
     diag_printf(d, "subtype of %s %s does not match type %s in "
                 "specification", what, istr(bname), type_pp(dtype));
     diag_hint(d, tree_loc(dport), "%s %s declared with type %s",
               what, istr(dname), type_pp(dtype));
     diag_hint(d, tree_loc(bport), "%s %s declared with type %s ",
               what, istr(bname), type_pp(btype));
     diag_emit(d);
     return false;
   }

   const port_mode_t dmode = tree_subkind(dport);
   const port_mode_t bmode = tree_subkind(bport);

   if (dmode != bmode) {
     diag_t *d = diag_new(DIAG_ERROR, tree_loc(bport));
     diag_printf(d, "%s %s of subprogram body %s with mode %s does not "
                 "match mode %s in specification", what, istr(dname),
                 istr(tree_ident(body)), port_mode_str(bmode),
                 port_mode_str(dmode));
     diag_hint(d, tree_loc(dport), "%s %s declared with mode %s",
               what, istr(dname), port_mode_str(dmode));
     diag_hint(d, tree_loc(bport), "%s %s declared with mode %s",
               what, istr(bname), port_mode_str(bmode));
     diag_emit(d);
     return false;
   }

   const bool bmode_explicit = !!(bflags & TREE_F_EXPLICIT_MODE);
   const bool dmode_explicit = !!(dflags & TREE_F_EXPLICIT_MODE);

   if (bmode_explicit != dmode_explicit) {
      diag_t *d = pedantic_diag(tree_loc(bport));
      if (d != NULL) {
         const char *dmode_str =
            dmode_explicit ? port_mode_str(dmode) : "default";
         const char *bmode_str =
            bmode_explicit ? port_mode_str(bmode) : "default";

         diag_printf(d, "mode indication of subprogram %s %s %s was %s in "
                     "specification but %s in body", istr(tree_ident(body)),
                     what, istr(dname), dmode_str, bmode_str);
         diag_hint(d, tree_loc(dport), "%s %s declared with %s mode in "
                   "specification", what, istr(dname), dmode_str);
         diag_hint(d, tree_loc(bport), "%s %s declared with %s mode in body",
                   what, istr(bname), bmode_str);
         diag_lrm(d, STD_08, "4.10");
         diag_emit(d);
         return false;
      }
   }

   const class_t dclass = tree_class(dport);
   const class_t bclass = tree_class(bport);

   if (dclass != bclass) {
     diag_t *d = diag_new(DIAG_ERROR, tree_loc(bport));
     diag_printf(d, "class %s of subprogram body %s %s %s does not "
                 "match class %s in specification", class_str(bclass),
                 istr(tree_ident(body)), what, istr(dname), class_str(dclass));
     diag_hint(d, tree_loc(dport), "%s %s declared with class %s in "
               "subprogram specification", what, istr(dname),
               class_str(dclass));
     diag_hint(d, tree_loc(bport), "%s %s declared with class %s in "
               "subprogram body", what, istr(bname), class_str(bclass));
     diag_emit(d);
     return false;
   }

   const bool bclass_explicit = !!(bflags & TREE_F_EXPLICIT_CLASS);
   const bool dclass_explicit = !!(dflags & TREE_F_EXPLICIT_CLASS);

   if (bclass_explicit != dclass_explicit) {
      diag_t *d = pedantic_diag(tree_loc(bport));
      if (d != NULL) {
         const char *dclass_str =
            dclass_explicit ? class_str(dclass) : "default";
         const char *bclass_str =
            bclass_explicit ? class_str(bclass) : "default";

         diag_printf(d, "class of subprogram %s %s %s was %s in specification "
                     "but %s in body", istr(tree_ident(body)),
                     what, istr(dname), dclass_str, bclass_str);
         diag_hint(d, tree_loc(dport), "%s %s declared with %s class in "
                   "specification", what, istr(dname), dclass_str);
         diag_hint(d, tree_loc(bport), "%s %s declared with %s class in body",
                   what, istr(bname), bclass_str);
         diag_lrm(d, STD_08, "4.10");
         diag_emit(d);
         return false;
      }
   }

   tree_t bdef = tree_has_value(bport) ? tree_value(bport) : NULL;
   tree_t ddef = tree_has_value(dport) ? tree_value(dport) : NULL;

   if (bdef == NULL && ddef == NULL)
     return true;

   const tree_kind_t bkind = bdef ? tree_kind(bdef) : T_LAST_TREE_KIND;
   const tree_kind_t dkind = ddef ? tree_kind(ddef) : T_LAST_TREE_KIND;

   // Work around some mismatches caused by folding
   if (bdef != NULL && ddef != NULL && bkind != dkind)
     return true;

   if (dkind == bkind) {
     // This only covers a few simple cases
     switch (dkind) {
     case T_LITERAL: {
       const literal_kind_t dsub = tree_subkind(ddef);
       const literal_kind_t bsub = tree_subkind(bdef);
       if (dsub == bsub) {
         switch (dsub) {
         case L_INT:
           if (tree_ival(ddef) == tree_ival(bdef))
             return true;
           break;
         case L_REAL:
           if (tree_dval(ddef) == tree_dval(bdef))
             return true;
           break;
         default:
           return true;
         }
       }
     } break;

     case T_REF:
     case T_FCALL:
       if (!tree_has_ref(bdef) || !tree_has_ref(ddef))
         return true; // Was parse error, ignore it

       tree_t bref = tree_ref(bdef);
       tree_t dref = tree_ref(ddef);

       if (bref == dref)
         return true;

       // Work around mismatch introduced by folding
       const tree_kind_t brefkind = tree_kind(bref);
       if (brefkind == T_CONST_DECL || brefkind == T_GENERIC_DECL)
         return true;

       break;

     default:
       return true;
     }
   }

   diag_t *d = diag_new(DIAG_ERROR, tree_loc(bport));
   diag_printf(d, "default value of %s %s in subprogram body %s does not "
               "match declaration", what, istr(dname), istr(tree_ident(body)));
   diag_hint(d, tree_loc(dport), "parameter was originally declared here");
   diag_hint(d, tree_loc(bport), "body has different default value");
   diag_emit(d);

   return false;
}

static bool sem_check_conforming(tree_t decl, tree_t body)
{
   // Conformance rules are in LRM 08 section 4.10
   // Note we don't implement strict lexical conformance here

   bool ok = true;

   const bool dimpure = !!(tree_flags(decl) & TREE_F_IMPURE);
   const bool bimpure = !!(tree_flags(body) & TREE_F_IMPURE);

   if (dimpure != bimpure) {
      diag_t *d = diag_new(DIAG_ERROR, tree_loc(body));
      diag_printf(d, "function %s declaration was %s but body is %s",
                  istr(tree_ident(body)), dimpure ? "impure" : "pure",
                  bimpure ? "impure" : "pure");
      diag_hint(d, tree_loc(decl), "declaration was %s",
                dimpure ? "impure" : "pure");
      diag_hint(d, tree_loc(body), "expecting keyword %s to match declaration",
                bimpure ? "IMPURE" : "PURE");
      diag_emit(d);
      ok = false;
   }

   // This must be true or they would be considered different overloads
   assert(tree_ports(decl) == tree_ports(body));

   const int nports = tree_ports(decl);
   for (int i = 0; i < nports; i++)
      ok &= sem_compare_interfaces(decl, body, i, tree_port, "parameter");

   const int ngenerics = tree_generics(decl);
   if (ngenerics != tree_generics(body)) {
      diag_t *d = diag_new(DIAG_ERROR, tree_loc(body));
      diag_printf(d, "subprogram %s declaration has %d generic%s but body "
                  "has %d", istr(tree_ident(body)), ngenerics,
                  ngenerics > 1 ? "s" : "", tree_generics(body));
      diag_hint(d, tree_loc(decl), "declaration with %d generics", ngenerics);
      diag_hint(d, tree_loc(body), "body has %d generics", tree_generics(body));
      diag_emit(d);
      ok = false;
   }
   else {
      for (int i = 0; i < ngenerics; i++)
         ok &= sem_compare_interfaces(decl, body, i, tree_generic, "generic");
   }

   return ok;
}

static bool sem_check_func_body(tree_t t, nametab_t *tab)
{
   if (!sem_check_func_ports(t, tab))
      return false;

   if (!sem_check_func_result(t))
      return false;

   tree_t fwd = find_forward_decl(tab, t);
   if (fwd != NULL && !sem_check_conforming(fwd, t))
      return false;

   return true;
}

static bool sem_check_proc_decl(tree_t t, nametab_t *tab)
{
   if (is_operator_symbol(tree_ident(t)))
      sem_error(t, "procedure name must be an identifier");

   const tree_flags_t flags = tree_flags(t);
   if (flags & TREE_F_PREDEFINED)
      return true;
   else if ((flags & TREE_F_PROTECTED) && !sem_check_protected_method(t, tab))
      return false;

   defer_check(tab, sem_missing_body_cb, t);
   return true;
}

static bool sem_check_proc_body(tree_t t, nametab_t *tab)
{
   tree_t fwd = find_forward_decl(tab, t);
   if (fwd != NULL && !sem_check_conforming(fwd, t))
      return false;

   if (fwd == NULL && is_operator_symbol(tree_ident(t)))
      sem_error(t, "procedure name must be an identifier");

   // Cleared by wait statement or pcall
   tree_set_flag(t, TREE_F_NEVER_WAITS);

   return true;
}

static bool sem_check_subprogram_inst(tree_t t, nametab_t *tab)
{
   if (tree_generics(t) == 0)
      return false;   // Was a parse error

   if (!sem_check_generic_map(t, t, tab))
      return false;

   // Other declarations were checked on the uninstantiated subprogram

   return true;
}

static bool sem_check_sensitivity(tree_t t, nametab_t *tab)
{
   const int ntriggers = tree_triggers(t);
   for (int i = 0; i < ntriggers; i++) {
      tree_t r = tree_trigger(t, i);
      if (tree_kind(r) == T_ALL)
         continue;
      else if (!sem_check(r, tab) || !sem_check_readable(r))
         return false;

      if (!sem_static_name(r, sem_globally_static))
         sem_error(r, "name in sensitivity list is not a static signal name");

      if (class_of(r) != C_SIGNAL) {
         tree_t ref = name_to_ref(r);
         diag_t *d = diag_new(DIAG_ERROR, tree_loc(r));
         if (ref != NULL) {
            tree_t decl = tree_ref(ref);
            diag_printf(d, "name %s in sensitivity list is not a signal",
                        istr(tree_ident(decl)));
            diag_hint(d, tree_loc(r), "%s is a %s", istr(tree_ident(decl)),
                      class_str(class_of(decl)));
         }
         else
            diag_printf(d, "name in sensitivity list is not a signal");
         diag_emit(d);
         return false;
      }
   }

   return true;
}

static void sem_check_static_elab(tree_t t)
{
   // LRM 93 12.3 forbirds references to signals before the design has
   // been elaborated: "The value of any object denoted by a primary in
   // such an expression must be defined at the time the primary is read"

   const tree_kind_t kind = tree_kind(t);

   switch (kind) {
   case T_REF:
   case T_EXTERNAL_NAME:
      {
         if (class_of(t) != C_SIGNAL)
            return;

         ident_t id;
         diag_t *d;
         if (kind == T_EXTERNAL_NAME) {
            id = tree_ident(tree_part(t, tree_parts(t) - 1));
            d = diag_new(DIAG_ERROR, tree_loc(t));
         }
         else if (!tree_has_ref(t))
            return;   // Was earlier error
         else {
            tree_t decl = tree_ref(t);
            id = tree_ident(decl);
            if (tree_has_value(decl) || !type_is_unconstrained(tree_type(decl)))
               d = pedantic_diag(tree_loc(t));
            else {
               d = diag_new(DIAG_ERROR, tree_loc(t));
               diag_hint(d, NULL, "the $bold$--relaxed$$ option would "
                         "downgrade this to a warning if %s had an initial "
                         "value or its type was fully constrained", istr(id));
            }
         }

         if (d != NULL) {
            diag_printf(d, "cannot reference signal %s during static "
                        "elaboration", istr(id));
            diag_hint(d, NULL, "the value of a signal is not defined "
                      "until after the design hierarchy is elaborated");
            diag_lrm(d, STD_93, "12.3");
            diag_emit(d);
         }
      }
      break;

   case T_SIGNAL_DECL:
   case T_VAR_DECL:
   case T_CONST_DECL:
      if (tree_has_value(t))
         sem_check_static_elab(tree_value(t));
      // Fall-through
   case T_TYPE_DECL:
   case T_SUBTYPE_DECL:
      {
         type_t type = tree_type(t);
         if (type_is_generic(type))
            ;  // Cannot check further
         else if (type_is_scalar(type) && !type_is_none(type))
            sem_check_static_elab(range_of(type, 0));
         else if (type_is_array(type) && !type_is_unconstrained(type)) {
            const int ndims = dimension_of(type);
            for (int i = 0; i < ndims; i++)
               sem_check_static_elab(range_of(type, i));
         }
      }
      break;

   case T_ARRAY_REF:
      {
         sem_check_static_elab(tree_value(t));

         const int nparams = tree_params(t);
         for (int i = 0; i < nparams; i++)
            sem_check_static_elab(tree_value(tree_param(t, i)));
      }
      break;

   case T_FCALL:
      if (!(tree_flags(t) & TREE_F_GLOBALLY_STATIC)) {
         const int nparams = tree_params(t);
         for (int i = 0; i < nparams; i++)
            sem_check_static_elab(tree_value(tree_param(t, i)));
      }
      break;

   case T_ARRAY_SLICE:
      sem_check_static_elab(tree_value(t));
      sem_check_static_elab(tree_range(t, 0));
      break;

   case T_RECORD_REF:
   case T_TYPE_CONV:
      sem_check_static_elab(tree_value(t));
      break;

   case T_RANGE:
      switch (tree_subkind(t)) {
      case RANGE_TO:
      case RANGE_DOWNTO:
         sem_check_static_elab(tree_left(t));
         sem_check_static_elab(tree_right(t));
         break;
      case RANGE_EXPR:
         sem_check_static_elab(tree_value(t));
         break;
      }
      break;

   case T_ATTR_REF:
      {
         // Same list of predefined attributes as sem_globally_static
         const attr_kind_t predef = tree_subkind(t);
         const bool non_static = predef == ATTR_EVENT || predef == ATTR_ACTIVE
            || predef == ATTR_LAST_EVENT || predef == ATTR_LAST_ACTIVE
            || predef == ATTR_LAST_VALUE || predef == ATTR_DRIVING
            || predef == ATTR_DRIVING_VALUE;

         if (non_static)
            sem_check_static_elab(tree_name(t));
      }
      break;

   default:
      break;
   }
}

static bool sem_check_process(tree_t t, nametab_t *tab)
{
   if (!sem_check_sensitivity(t, tab))
      return false;

   return true;
}

static bool sem_check_package(tree_t t, nametab_t *tab)
{
   if (!sem_check_context_clause(t, tab))
      return false;

   if (tree_genmaps(t) > 0 && !sem_check_generic_map(t, t, tab))
      return false;

   // Subprogram bodies are not allowed in package specification
   const int ndecls = tree_decls(t);
   for (int i = 0; i < ndecls; i++) {
     tree_t d = tree_decl(t, i);
     tree_kind_t kind = tree_kind(d);
     if ((kind == T_FUNC_BODY) || (kind == T_PROC_BODY))
       sem_error(d, "subprogram body is not allowed in package specification");
   }

   return true;
}

static bool sem_check_pack_inst(tree_t t, nametab_t *tab)
{
   if (tree_generics(t) == 0)
      return false;   // Was a parse error

   if (!sem_check_generic_map(t, t, tab))
      return false;

   // Other declarations were checked on the uninstantiated package

   return true;
}

static bool sem_check_missing_bodies(tree_t secondary, nametab_t *tab)
{
   // Check for any missing subprogram or protected type bodies, and
   // deferred constants which were not given values and
   tree_t primary = tree_primary(secondary);
   const int ndecls = tree_decls(primary);
   for (int i = 0; i < ndecls; i++) {
      tree_t d = tree_decl(primary, i);
      switch (tree_kind(d)) {
      case T_FUNC_DECL:
      case T_PROC_DECL:
         if (tree_flags(d) & TREE_F_PREDEFINED)
            break;
         // Fall-through
      case T_PROT_DECL:
         sem_missing_body_cb(d, secondary, tab);
         break;
      case T_CONST_DECL:
         if (!tree_has_value(d)) {
            tree_t d2 = get_local_decl(tab, NULL, tree_ident(d), 0);
            if (d2 == NULL || !tree_has_value(d2))
               sem_error(d, "deferred constant %s was not given a value in the "
                         "package body", istr(tree_ident(d)));
         }
      default:
         break;
      }
   }

   return true;
}

static bool sem_check_pack_body(tree_t t, nametab_t *tab)
{
   if (!tree_has_primary(t))
      return false;

   if (!sem_check_context_clause(t, tab))
      return false;

   if (!sem_check_missing_bodies(t, tab))
      return false;

   return true;
}

static bool sem_check_component(tree_t t, nametab_t *tab)
{
   return true;
}

static void sem_passive_cb(tree_t t, void *context)
{
   tree_t s = context;

   diag_t *d = diag_new(DIAG_ERROR, tree_loc(t));
   diag_printf(d, "signal assignment statement not allowed inside passive "
               "process");
   diag_hint(d, tree_loc(s), "process in entity statement part must "
             "be passive");
   diag_hint(d, tree_loc(t), "signal assignment statement");
   diag_lrm(d, STD_93, "1.1.3");
   diag_lrm(d, STD_93, "9.2");

   diag_emit(d);
}

static bool sem_check_entity(tree_t t, nametab_t *tab)
{
   if (!sem_check_context_clause(t, tab))
      return false;

   // All processes in entity statement part must be passive
   const int nstmts = tree_stmts(t);
   for (int i = 0; i < nstmts; i++) {
      tree_t s = tree_stmt(t, i);
      tree_visit_only(s, sem_passive_cb, s, T_SIGNAL_ASSIGN);
   }

   return true;
}

static bool sem_check_arch(tree_t t, nametab_t *tab)
{
   if (!tree_has_primary(t))
      return false;

   if (!sem_check_context_clause(t, tab))
      return false;

   const int ndecls = tree_decls(t);
   for (int n = 0; n < ndecls; n++) {
      tree_t d = tree_decl(t, n);
      sem_check_static_elab(d);
   }

   return true;
}

static tree_t sem_check_lvalue(tree_t t)
{
   switch (tree_kind(t)) {
   case T_REF:
      return sem_check_lvalue(tree_ref(t));
   case T_ARRAY_SLICE:
   case T_ARRAY_REF:
   case T_ALIAS:
   case T_RECORD_REF:
   case T_ALL:
      return sem_check_lvalue(tree_value(t));
   case T_VAR_DECL:
   case T_SIGNAL_DECL:
   case T_PORT_DECL:
   case T_CONST_DECL:
   case T_IMPLICIT_SIGNAL:
   case T_PARAM_DECL:
   case T_EXTERNAL_NAME:
      return t;
   default:
      return NULL;
   }
}

static bool sem_check_aggregate_target_element(tree_t target, int nth)
{
   tree_t a = tree_assoc(target, nth);
   tree_t value = tree_value(a);

   if (tree_kind(value) != T_AGGREGATE) {
      if (!sem_static_name(value, sem_locally_static))
         sem_error(value, "aggregate element must be locally static name");
   }

   const assoc_kind_t kind = tree_subkind(a);
   switch (kind) {
   case A_RANGE:
      if (standard() >= STD_08) {
         // LRM 08 section 10.6.2.1: it is an error if the element
         // association contains a choice that is a discrete range and
         // an expression of a type other than the aggregate type.
         diag_t *d = diag_new(DIAG_ERROR, tree_loc(value));
         diag_printf(d, "range choice expression must have same type "
                     "as aggregate");
         diag_hint(d, tree_loc(value), "expression type is %s but "
                   "aggregate is %s", type_pp(tree_type(value)),
                   type_pp(tree_type(target)));
         diag_lrm(d, STD_08, "10.6.2");
         diag_emit(d);
         return false;
      }
      // Fall-through
   case A_OTHERS:
      sem_error(a, "%s association not allowed in aggregate target",
                assoc_kind_str(kind));
   case A_NAMED:
   case A_POS:
   case A_SLICE:
   case A_CONCAT:
      break;
   }

   const int nassocs = tree_assocs(target);
   for (int j = nth + 1; j < nassocs; j++) {
      tree_t cmp = tree_value(tree_assoc(target, j));
      if (same_tree(value, cmp)) {
         diag_t *d = diag_new(DIAG_ERROR, tree_loc(cmp));
         diag_printf(d, "%s %s is identifed more than once in "
                     "aggregate target", class_str(class_of(cmp)),
                     tree_kind(cmp) == T_REF ?
                     istr(tree_ident(cmp)) : "subelement");
         diag_hint(d, tree_loc(value), "first seen here");
         diag_lrm(d, STD_08, "10.5.2");
         diag_emit(d);
         return false;
      }
   }

   return true;
}

static bool sem_check_variable_target(tree_t target)
{
   if (tree_kind(target) == T_AGGREGATE) {
      // Rules for aggregate variable targets in LRM 93 section 8.5

      type_t type = tree_type(target);
      if (!type_is_composite(type))
         sem_error(target, "aggregate target of variable assignment has "
                   "non-composite type %s", type_pp(tree_type(target)));

      const int nassocs = tree_assocs(target);
      for (int i = 0; i < nassocs; i++) {
         tree_t a = tree_assoc(target, i);
         tree_t value = tree_value(a);

         if (!sem_check_variable_target(value))
            return false;

         if (!sem_check_aggregate_target_element(target, i))
            return false;
      }
   }
   else {
      tree_t decl = sem_check_lvalue(target);
      const tree_kind_t kind = decl ? tree_kind(decl) : T_LAST_TREE_KIND;

      const bool suitable = kind == T_VAR_DECL
         || (kind == T_PARAM_DECL && tree_class(decl) == C_VARIABLE)
         || (kind == T_EXTERNAL_NAME && tree_class(decl) == C_VARIABLE);

      if (!suitable) {
         diag_t *d = diag_new(DIAG_ERROR, tree_loc(target));
         diag_printf(d, "target of variable assignment must be a variable "
                     "name or aggregate");

         tree_t ref = name_to_ref(target);
         if (ref != NULL && tree_has_ref(ref))
            diag_hint(d, tree_loc(target), "%s is a %s", istr(tree_ident(ref)),
                      class_str(class_of(tree_ref(ref))));

         diag_emit(d);
         return false;
      }
      else if (kind == T_PARAM_DECL && tree_subkind(decl) == PORT_IN) {
         diag_t *d = diag_new(DIAG_ERROR, tree_loc(target));
         diag_printf(d, "cannot assign to parameter %s with mode IN",
                     istr(tree_ident(decl)));
         diag_hint(d, tree_loc(decl), "%s declared with mode IN",
                   istr(tree_ident(decl)));
         diag_hint(d, tree_loc(target), "target of variable assignment");
         diag_emit(d);
         return false;
      }
      else if (type_is_protected(tree_type(target)))
         sem_error(target, "may not assign to variable of a protected type");
   }

   return true;
}

static bool sem_check_var_assign(tree_t t, nametab_t *tab)
{
   tree_t target = tree_target(t);
   tree_t value = tree_value(t);

   if (!sem_check(target, tab))
      return false;

   if (!sem_check(value, tab))
      return false;

   if (!sem_check_readable(value))
      return false;

   if (!sem_check_variable_target(target))
      return false;

   if (!sem_check_same_type(value, target)) {
      type_t target_type = tree_type(target);
      type_t value_type  = tree_type(value);
      sem_error(t, "type of value %s does not match type of target %s",
                type_pp2(value_type, target_type),
                type_pp2(target_type, value_type));
   }

   return true;
}

static bool sem_check_waveforms(tree_t t, tree_t target, nametab_t *tab)
{
   type_t std_time = std_type(NULL, STD_TIME);
   type_t expect = tree_type(target);

   const int nwaves = tree_waveforms(t);
   for (int i = 0; i < nwaves; i++) {
      tree_t waveform = tree_waveform(t, i);

      if (tree_has_value(waveform)) {
         tree_t value = tree_value(waveform);

         if (!sem_check(value, tab))
            return false;

         if (!sem_check_readable(value))
            return false;

         if (!sem_check_type(value, expect, tab))
            sem_error(t, "type of value %s does not match type of target %s",
                      type_pp2(tree_type(value), expect),
                      type_pp2(expect, tree_type(value)));
      }
      else {
         tree_t decl = sem_check_lvalue(target);
         if (decl != NULL && !is_guarded_signal(decl))
            sem_error(waveform, "a null waveform element is only valid when "
                      "the target is a guarded signal");
      }

      if (tree_has_delay(waveform)) {
         tree_t delay = tree_delay(waveform);
         if (!sem_check(delay, tab))
            return false;

         if (!sem_check_type(delay, std_time, tab))
            sem_error(delay, "type of delay must be %s but have %s",
                      type_pp(std_time), type_pp(tree_type(delay)));
      }
   }

   return true;
}

static tree_t sem_check_view_target(tree_t target)
{
   switch (tree_kind(target)) {
   case T_REF:
      {
         tree_t decl = tree_ref(target);
         if (tree_kind(decl) == T_PORT_DECL) {
            const port_mode_t mode = tree_subkind(decl);
            if (mode == PORT_ARRAY_VIEW || mode == PORT_RECORD_VIEW)
               return tree_value(decl);
         }

         return NULL;
      }

   case T_ARRAY_REF:
   case T_ARRAY_SLICE:
      return sem_check_view_target(tree_value(target));

   case T_RECORD_REF:
      {
         tree_t view = sem_check_view_target(tree_value(target));
         if (view == NULL)
            return NULL;

         bool converse = false;
         tree_t f = tree_ref(target);
         tree_t e = find_element_mode_indication(view, f, &converse);
         if (e == NULL)
            return NULL;

         if (converse_mode(e, converse) == PORT_IN) {
            tree_t port = tree_ref(name_to_ref(target));
            diag_t *d = diag_new(DIAG_ERROR, tree_loc(target));
            diag_printf(d, "cannot assign to element %s of port %s which has "
                        "mode IN from mode view indication",
                        istr(tree_ident(e)), istr(tree_ident(port)));
            diag_hint(d, tree_loc(target), "target of signal assignment");
            diag_hint(d, tree_loc(port), "sub-element %s of %s declared with "
                      "mode IN due to mode view indication",
                      istr(tree_ident(e)), istr(tree_ident(port)));
            diag_emit(d);
            return NULL;
         }

         return NULL;
      }

   default:
      return NULL;
   }
}

static bool sem_check_signal_target(tree_t target, nametab_t *tab, bool guarded)
{
   if (tree_kind(target) == T_AGGREGATE) {
      // Rules for aggregate signal targets in LRM 93 section 8.4

      type_t type = tree_type(target);
      if (!type_is_composite(type))
         sem_error(target, "aggregate target of signal assignment has "
                   "non-composite type %s", type_pp(type));

      bool has_guarded = false, has_unguarded = false;
      const int nassocs = tree_assocs(target);
      for (int i = 0; i < nassocs; i++) {
         tree_t a = tree_assoc(target, i);
         tree_t value = tree_value(a);

         if (!sem_check_signal_target(value, tab, guarded))
            return false;

         if (!sem_check_aggregate_target_element(target, i))
            return false;

         tree_t ref = name_to_ref(value);
         if (ref != NULL && is_guarded_signal(tree_ref(ref)))
            has_guarded = true;
         else
            has_unguarded = true;
      }

      if (has_guarded && has_unguarded) {
         diag_t *d = diag_new(DIAG_ERROR, tree_loc(target));
         diag_printf(d, "aggregate target of signal assignment contains both "
                     "guarded and unguarded signals");
         diag_lrm(d, STD_08, "11.6");
         diag_emit(d);
         return false;
      }

      return true;
   }
   else {
      tree_t decl = sem_check_lvalue(target);
      if (decl == NULL)
         sem_error(target, "target of signal assignment must be a signal "
                   "name or aggregate");

      if (is_guarded_signal(decl) && !guarded) {
         diag_t *d = diag_new(DIAG_ERROR, tree_loc(target));
         diag_printf(d, "guarded signal %s cannot be the target of an "
                     "unguarded assignment", istr(tree_ident(decl)));
         diag_hint(d, tree_loc(decl), "%s declared here",
                   istr(tree_ident(decl)));
         diag_lrm(d, STD_08, "11.6");
         diag_emit(d);
         return false;
      }

      switch (tree_kind(decl)) {
      case T_SIGNAL_DECL:
         {
            tree_t sub = find_enclosing(tab, S_SUBPROGRAM);
            if (sub != NULL && find_enclosing(tab, S_PROCESS) == NULL) {
               // LRM 08 section 10.5.2.2: if a signal assignment appears
               // in a procedure not contained within a process then the
               // target must be a formal parameter
               sem_error(target, "signal %s is not a formal parameter and "
                         "subprogram %s is not contained within a process "
                         "statement", istr(tree_ident(decl)),
                         type_pp(tree_type(sub)));
            }
         }
         break;

      case T_IMPLICIT_SIGNAL:
         sem_error(target, "implicit signal may not be assigned");

      case T_PORT_DECL:
      case T_PARAM_DECL:
         {
            const port_mode_t mode = tree_subkind(decl);
            if (mode == PORT_IN) {
               diag_t *d = diag_new(DIAG_ERROR, tree_loc(target));
               diag_printf(d, "cannot assign to input %s %s",
                           tree_kind(decl) == T_PORT_DECL
                           ? "port" : "parameter",
                           istr(tree_ident(decl)));
               diag_hint(d, tree_loc(target), "target of signal assignment");
               diag_hint(d, tree_loc(decl), "%s declared with mode IN",
                         istr(tree_ident(decl)));
               diag_emit(d);
               return false;
            }
            else if (mode == PORT_ARRAY_VIEW || mode == PORT_RECORD_VIEW) {
               tree_t view = sem_check_view_target(target);
               if (view != NULL) {
                  tree_t inport = NULL;
                  type_t view_type = tree_type(view);
                  const int nelems = type_fields(view_type);
                  for (int i = 0; i < nelems; i++) {
                     tree_t e = type_field(view_type, i);
                     const port_mode_t mode = tree_subkind(e);
                     if (mode == PORT_IN || mode == PORT_ARRAY_VIEW
                         || mode == PORT_RECORD_VIEW) {
                        // This is not correct for nested mode view
                        // indications but seems like a very obscure
                        // corner case
                        inport = e;
                        break;
                     }
                  }

                  if (inport != NULL) {
                     diag_t *d = diag_new(DIAG_ERROR, tree_loc(target));
                     diag_printf(d, "cannot assign to port %s with mode view "
                                 "indication as one or more sub-elements have "
                                 "mode IN", istr(tree_ident(decl)));
                     diag_hint(d, tree_loc(target),
                               "target of signal assignment");
                     diag_hint(d, tree_loc(inport),
                               "element %s declared with mode IN",
                               istr(tree_ident(inport)));
                     diag_emit(d);
                     return false;
                  }
               }

               return true;
            }
            else if (mode == PORT_LINKAGE)
               sem_error(target, "linkage port %s may not be updated except as "
                         "an actual corresponding to an interface of mode "
                         "linkage", istr(tree_ident(decl)));
            else if (tree_class(decl) != C_SIGNAL) {
               diag_t *d = diag_new(DIAG_ERROR, tree_loc(target));
               diag_printf(d, "%s is not a valid target of signal assignment",
                           istr(tree_ident(decl)));
               diag_hint(d, tree_loc(target), "target of signal assignment");
               diag_hint(d, tree_loc(decl), "declared with class %s",
                         class_str(tree_class(decl)));
               diag_emit(d);
               return false;
            }
         }
         break;

      case T_EXTERNAL_NAME:
         {
            if (tree_class(decl) != C_SIGNAL) {
               tree_t tail = tree_part(decl, tree_parts(decl) - 1);
               diag_t *d = diag_new(DIAG_ERROR, tree_loc(target));
               diag_printf(d, "external name %s is not a valid target of "
                           "signal assignment", istr(tree_ident(tail)));
               diag_hint(d, tree_loc(target), "target of signal assignment");
               diag_hint(d, tree_loc(decl), "declared with class %s",
                         class_str(tree_class(decl)));
               diag_emit(d);
               return false;
            }

            tree_t sub = find_enclosing(tab, S_SUBPROGRAM);
            if (sub != NULL && find_enclosing(tab, S_PROCESS) == NULL)
               sem_error(target, "cannot create driver for external name as "
                         "subprogram %s is not contained within a process "
                         "statement", type_pp(tree_type(sub)));
         }
         break;

      case T_VAR_DECL:
      case T_CONST_DECL:
         {
            diag_t *d = diag_new(DIAG_ERROR, tree_loc(target));
            diag_printf(d, "%s %s is not a valid target of signal assignment",
                        class_str(class_of(decl)), istr(tree_ident(decl)));
            diag_hint(d, tree_loc(target), "target of signal assignment");
            diag_hint(d, tree_loc(decl), "declared as %s",
                      class_str(class_of(decl)));
            diag_emit(d);
            return false;
         }

      default:
         sem_error(target, "invalid target of signal assignment");
      }

      return true;
   }
}

static bool sem_check_reject(tree_t t, nametab_t *tab)
{
   if (!sem_check(t, tab))
      return false;

   if (!type_eq(tree_type(t), std_type(NULL, STD_TIME)))
      sem_error(t, "reject interval must have type TIME but have %s",
                type_pp(tree_type(t)));

   return true;
}

static bool sem_check_signal_assign(tree_t t, nametab_t *tab)
{
   tree_t target = tree_target(t);

   if (!sem_check(target, tab))
      return false;

   if (!sem_check_signal_target(target, tab, true))
      return false;

   if (!sem_check_waveforms(t, target, tab))
      return false;

   if (tree_has_reject(t) && !sem_check_reject(tree_reject(t), tab))
      return false;

   return true;
}

static bool sem_check_guard(tree_t t, nametab_t *tab)
{
   assert(tree_kind(t) == T_GUARD);

   if (!sem_check_type(t, std_type(NULL, STD_BOOLEAN), tab))
      sem_error(t, "guard signal must have BOOLEAN type but found %s",
                type_pp(tree_type(t)));

   tree_t decl = tree_ref(t);
   switch (tree_kind(decl)) {
   case T_SIGNAL_DECL:
   case T_IMPLICIT_SIGNAL:
      break;
   case T_PORT_DECL:
      if (tree_class(decl) == C_SIGNAL)
         break;
      // Fall-through
   default:
      {
         diag_t *d = diag_new(DIAG_ERROR, tree_loc(t));
         diag_printf(d, "assignment guard must be a signal");
         diag_hint(d, tree_loc(decl), "%s is a %s", istr(tree_ident(decl)),
                   class_str(class_of(decl)));
         diag_hint(d, tree_loc(t), "guarded statement");
         diag_emit(d);
         return false;
      }
   }

   return true;
}

static bool sem_check_cond_assign(tree_t t, nametab_t *tab)
{
   tree_t target = tree_target(t);

   if (!sem_check(target, tab))
      return false;

   const bool has_guard = tree_has_guard(t);

   if (!sem_check_signal_target(target, tab, has_guard))
      return false;

   if (has_guard && !sem_check_guard(tree_guard(t), tab))
      return false;

   type_t std_bool = std_type(NULL, STD_BOOLEAN);

   const int nconds = tree_conds(t);
   for (int i = 0; i < nconds; i++) {
      tree_t c = tree_cond(t, i);

      if (tree_has_value(c)) {
         tree_t test = tree_value(c);

         if (!sem_check(test, tab))
            return false;

         if (!type_eq(tree_type(test), std_bool))
            sem_error(test, "type of condition must be BOOLEAN");
      }

      assert(tree_stmts(c) == 1);
      tree_t a = tree_stmt(c, 0);

      assert(tree_kind(a) == T_SIGNAL_ASSIGN);
      assert(tree_target(a) == target);

      if (tree_has_reject(a) && !sem_check_reject(tree_reject(a), tab))
         return false;

      if (!sem_check_waveforms(a, target, tab))
         return false;
   }

   return true;
}

static bool sem_check_closely_related(type_t from, type_t to, tree_t where)
{
   if (type_eq(to, from))
      return true;

   // Conversions are allowed between any abstract numeric types
   if (type_is_numeric(from) && type_is_numeric(to))
      return true;

   // Suppress cascading errors
   if (type_is_none(from) || type_is_none(to))
      return true;

   char *reason = NULL;

   if (type_is_array(from) && type_is_array(to)) {
      const int from_dims = dimension_of(from);
      const int to_dims = dimension_of(to);

      // Types must have same dimensionality
      if (from_dims != to_dims) {
         reason = xasprintf("%s has %d dimension%s but %s has %d",
                            type_pp2(from, to), from_dims,
                            from_dims == 1 ? "" : "s",
                            type_pp2(to, from), to_dims);
         goto not_closely_related;
      }

      // Index types the same or closely related
      for (int i = 0; i < from_dims; i++) {
         type_t from_index = index_type_of(from, i);
         type_t to_index = index_type_of(to, i);

         if (!sem_check_closely_related(from_index, to_index, NULL)) {
            reason = xasprintf("%s index type of %s is %s which is not closely "
                               "related to the %s index type of %s",
                               ordinal_str(i + 1), type_pp2(from, to),
                               type_pp(from_index), ordinal_str(i + 1),
                               type_pp2(to, from));
            goto not_closely_related;
         }
      }

      type_t from_e = type_elem(from);
      type_t to_e = type_elem(to);

      if (standard() >= STD_08) {
         // Element types must be closely related
         if (!sem_check_closely_related(from_e, to_e, NULL)) {
            reason = xasprintf("element type %s is not closely related to %s",
                               type_pp2(from_e, to_e), type_pp2(to_e, from_e));
            goto not_closely_related;
         }
      }
      else {
         // Element types must be the same
         if (!type_eq(from_e, to_e)) {
            reason = xasprintf("element type %s does not match %s",
                               type_pp2(from_e, to_e), type_pp2(to_e, from_e));
            goto not_closely_related;
         }
      }

      return true;
   }

   if (type_is_record(from) && type_is_record(to) && standard() >= STD_19) {
      // Each element of the target type must have a matching element in
      // the from type
      const int from_nf = type_fields(from);
      const int to_nf = type_fields(to);

      for (int i = 0; i < to_nf; i++) {
         tree_t to_f = type_field(to, i), from_f = NULL;
         type_t to_ftype = tree_type(to_f);
         ident_t name = tree_ident(to_f);

         for (int j = 0; j < from_nf && from_f == NULL; j++) {
            tree_t f = type_field(from, j);
            if (tree_ident(f) != name)
               continue;

            type_t from_ftype = tree_type(f);
            if (!sem_check_closely_related(from_ftype, to_ftype, NULL))
               break;

            from_f = f;
         }

         if (from_f != NULL)
            continue;

         reason = xasprintf("field %s in record type %s has no matching "
                            "element in type %s", istr(tree_ident(to_f)),
                            type_pp2(to, from), type_pp2(from, to));
         goto not_closely_related;
      }

      return true;
   }

 not_closely_related:
   if (where != NULL) {
      const loc_t *loc = tree_loc(where);
      diag_t *d = diag_new(DIAG_ERROR, loc);
      diag_printf(d, "conversion only allowed between closely related types");
      if (reason != NULL)
         diag_hint(d, loc, "%s", reason);
      else
         diag_hint(d, loc, "%s and %s are not closely related",
                   type_pp2(from, to), type_pp2(to, from));
      diag_lrm(d, STD_93, "7.3.5");
      diag_emit(d);

      free(reason);
   }

   return false;
}

static bool sem_check_conversion(tree_t t, nametab_t *tab)
{
   // Type conversions are described in LRM 93 section 7.3.5

   tree_t value = tree_value(t);
   if (!sem_check(value, tab))
      return false;

   return sem_check_closely_related(tree_type(value), tree_type(t), t);
}

static bool sem_check_compatible_view(tree_t formal, tree_t actual)
{
   type_t type = tree_type(formal);

   type_t elem_type = type;
   if (tree_subkind(formal) == PORT_ARRAY_VIEW)
      elem_type = type_elem(type);

   tree_t formal_view = tree_value(formal);

   tree_t actual_view = sem_check_view_target(actual);
   if (actual_view != NULL) {
      // Associating an interface with another interface: check the
      // mode of each element is compatible
      const int nfields = type_fields(elem_type);
      for (int i = 0; i < nfields; i++) {
         tree_t f = type_field(elem_type, i);

         bool formal_converse = false;
         tree_t formal_elem = find_element_mode_indication(formal_view, f,
                                                           &formal_converse);

         bool actual_converse = false;
         tree_t actual_elem = find_element_mode_indication(actual_view, f,
                                                           &actual_converse);

         const port_mode_t formal_mode =
            converse_mode(formal_elem, formal_converse);

         const port_mode_t actual_mode =
            converse_mode(actual_elem, actual_converse);

         if (formal_mode != actual_mode) {
            diag_t *d = diag_new(DIAG_ERROR, tree_loc(actual));
            diag_printf(d, "mode view indication of formal %s %s "
                        "element %s is not compatible with actual",
                        tree_kind(formal) == T_PORT_DECL ? "port" : "parameter",
                        istr(tree_ident(formal)), istr(tree_ident(f)));
            diag_hint(d, tree_loc(actual_view), "actual has mode %s from "
                      "mode view indication on port %s",
                      port_mode_str(actual_mode),
                      istr(tree_ident(name_to_ref(actual))));
            diag_hint(d, tree_loc(formal_view), "formal has mode %s",
                      port_mode_str(formal_mode));
            diag_emit(d);
            return false;
         }
      }
   }

   return true;
}

static bool sem_check_call_args(tree_t t, tree_t decl, nametab_t *tab)
{
   const int nparams = tree_params(t);
   const int nports  = tree_ports(decl);

   if (is_uninstantiated_subprogram(decl)) {
      // Allow recursive calls to the same subprogram
      tree_t sub = find_enclosing(tab, S_SUBPROGRAM);
      if (sub != decl)
         sem_error(t, "cannot call uninstantiated %s %s",
                   class_str(class_of(decl)), istr(tree_ident(decl)));
   }

   tree_t *map LOCAL = xcalloc_array(nports, sizeof(tree_t));

   bool have_named = false;
   for (int i = 0; i < nparams; i++) {
      tree_t param = tree_param(t, i), port = NULL;
      type_t port_type = NULL;
      bool partial = false;
      int index = -1;
      switch (tree_subkind(param)) {
      case P_POS:
         if (have_named)
            sem_error(param, "positional parameters must precede named "
                      "parameters");
         else if ((index = tree_pos(param)) >= nports) {
            diag_t *d = diag_new(DIAG_ERROR, tree_loc(param));
            diag_printf(d, "too many positional parameters for subprogram %s",
                        type_pp(tree_type(decl)));
            diag_hint(d, tree_loc(param), "%s positional parameter",
                      ordinal_str(index + 1));
            diag_hint(d, tree_loc(decl), "%s %s has %d formal parameter%s",
                      class_str(class_of(decl)), istr(tree_ident(decl)),
                      nports, nports > 1 ? "s" : "");
            diag_emit(d);
            return false;
         }
         else {
            port = tree_port(decl, index);
            port_type = tree_type(port);
         }
         break;

      case P_NAMED:
         {
            have_named = true;

            tree_t name = tree_name(param);
            if (tree_kind(name) == T_FCALL)
               sem_error(name, "sorry, conversion functions are not yet "
                         "supported here");

            tree_t ref = name_to_ref(name);
            assert(ref != NULL);

            if ((partial = (ref != name))) {
               tree_t value = tree_value(name);
               if (tree_kind(value) != T_REF)
                  sem_error(name, "sorry, this form of named parameter is "
                            "not supported");
            }

            ident_t id = tree_ident(ref);
            for (int j = 0; j < nports; j++) {
               tree_t p = tree_port(decl, j);
               if (tree_ident(p) == id) {
                  index = j;
                  port = p;
                  break;
               }
            }

            if (index == -1 || !tree_has_ref(ref)) {
               // Should have generated an error during overload
               // resolution
               assert(error_count() > 0);
               return false;
            }

            // Set the ref again here because solve_types may have set it
            // to the wrong overload
            if (tree_ref(ref) != port)
               tree_set_name(param, (name = change_ref(name, port)));

            port_type = tree_type(name);
         }
      }

      class_t class    = tree_class(port);
      port_mode_t mode = tree_subkind(port);

      if (map[index] != NULL && (!partial || tree_kind(map[index]) == T_REF))
         sem_error(param, "formal parameter %s already has an associated "
                   "actual", istr(tree_ident(port)));

      map[index] = param;

      tree_t value = tree_value(param);
      if (!sem_check(value, tab))
         return false;

      if (!sem_check_type(value, port_type, tab))
         sem_error(value, "type of actual %s does not match formal %s type %s",
                   type_pp2(tree_type(value), port_type),
                   istr(tree_ident(port)),
                   type_pp2(port_type, tree_type(value)));

      // LRM 08 sections 4.2.2.2 and 4.2.2.3
      if (class == C_VARIABLE || class == C_SIGNAL) {
         tree_t decl = sem_check_lvalue(value);
         if (decl == NULL || class_of(value) != class) {
            diag_t *d = diag_new(DIAG_ERROR, tree_loc(value));
            diag_printf(d, "actual for formal %s with class %s must be "
                        "a name denoting a %s", istr(tree_ident(port)),
                        class == C_VARIABLE ? "VARIABLE" : "SIGNAL",
                        class_str(class));
            if (decl == NULL)
               diag_hint(d, tree_loc(value), "actual designator is not a name");
            else if (tree_kind(decl) == T_EXTERNAL_NAME)
               diag_hint(d, tree_loc(value), "external name has class %s",
                         class_str(tree_class(decl)));
            else
               diag_hint(d, tree_loc(value), "object %s has class %s",
                         istr(tree_ident(decl)), class_str(class_of(decl)));
            diag_lrm(d, STD_08, class == C_SIGNAL ? "4.2.2.3" : "4.2.2.2");
            diag_emit(d);
            return false;
         }

         // Check OUT and INOUT parameters can be assigned to
         if (mode == PORT_OUT || mode == PORT_INOUT) {
            const tree_kind_t decl_kind = tree_kind(decl);
            if ((decl_kind == T_PARAM_DECL || decl_kind == T_PORT_DECL)
                && tree_subkind(decl) == PORT_IN) {
               const char *what =
                  decl_kind == T_PARAM_DECL ? "parameter" : "port";
               diag_t *d = diag_new(DIAG_ERROR, tree_loc(value));
               diag_printf(d, "cannot associate %s %s of mode IN with "
                           "formal %s of mode %s", what,
                           istr(tree_ident(decl)), istr(tree_ident(port)),
                           port_mode_str(mode));
               diag_hint(d, tree_loc(decl), "%s declared with mode %s",
                         istr(tree_ident(decl)),
                         port_mode_str(tree_subkind(decl)));
               diag_hint(d, tree_loc(value), "associated with %s %s %s here",
                         port_mode_str(mode), what, istr(tree_ident(port)));
               diag_emit(d);
               return false;
            }
         }
         else if ((mode == PORT_ARRAY_VIEW || mode == PORT_RECORD_VIEW)
                  && !sem_check_compatible_view(port, value))
            return false;
      }

      if (class == C_SIGNAL && !sem_static_name(value, sem_globally_static)) {
         diag_t *d = pedantic_diag(tree_loc(value));
         if (d != NULL) {
            diag_printf(d, "actual associated with signal parameter %s must be "
                        "denoted by a static signal name",
                        istr(tree_ident(port)));
            diag_hint(d, tree_loc(value), "not a static signal name");
            diag_lrm(d, STD_08, "4.2.2.3");
            diag_lrm(d, STD_08, "8.1");
            diag_emit(d);
            return false;
         }
      }

      // Check IN and INOUT parameters can be read
      if (tree_kind(t) != T_ATTR_REF) {
         const port_mode_t mode = tree_subkind(port);
         if (mode == PORT_IN || mode == PORT_INOUT) {
            if (!sem_check_readable(value))
               return false;
         }
      }
   }

   for (int i = 0; i < nports; i++) {
      if (map[i] == NULL) {
         tree_t port = tree_port(decl, i);
         if (!tree_has_value(port))
            sem_error(t, "missing actual for formal parameter %s without "
                      "default value", istr(tree_ident(port)));
         else {
            const port_mode_t mode = tree_subkind(port);
            if (mode == PORT_ARRAY_VIEW || mode == PORT_RECORD_VIEW)
               sem_error(t, "missing actual for formal parameter %s with "
                         "mode view indication %s", istr(tree_ident(port)),
                         type_pp(tree_type(tree_value(port))));
         }
      }
   }

   return true;
}

static bool sem_check_fcall(tree_t t, nametab_t *tab)
{
   if (!tree_has_ref(t))
      return false;

   if (tree_kind(t) == T_PROT_FCALL && tree_has_name(t)) {
      tree_t name = tree_name(t);
      if (!sem_check(name, tab))
         return false;
   }

   tree_t decl = tree_ref(t), sub;
   const tree_flags_t flags = tree_flags(decl);

   if ((flags & TREE_F_IMPURE) && (sub = find_enclosing(tab, S_SUBPROGRAM))) {
      // Pure function may not call an impure function
      if (tree_kind(sub) == T_FUNC_BODY && !(tree_flags(sub) & TREE_F_IMPURE)) {
         diag_t *d = pedantic_diag(tree_loc(t));
         if (d != NULL) {
            diag_printf(d, "pure function %s cannot call impure function %s",
                        istr(tree_ident(sub)), istr(tree_ident(decl)));
            diag_emit(d);
         }
      }

      // Propagate impurity flags
      tree_set_flag(sub, flags & (TREE_F_IMPURE_FILE | TREE_F_IMPURE_SHARED));
   }
   else if (tree_kind(decl) == T_FUNC_DECL && !(flags & TREE_F_PREDEFINED)
            && is_same_region(tab, decl) && opt_get_int(OPT_MISSING_BODY)) {
      diag_t *d = diag_new(DIAG_ERROR, tree_loc(t));
      diag_printf(d, "subprogram %s called before its body has been "
                  "elaborated", type_pp(tree_type(decl)));
      diag_hint(d, tree_loc(decl), "%s declared here",
                type_pp(tree_type(decl)));
      diag_lrm(d, STD_08, "14.4.2");
      diag_emit(d);
      return false;
   }

   if (!sem_check_call_args(t, decl, tab))
      return false;

   if (sem_locally_static(t))
      tree_set_flag(t, TREE_F_LOCALLY_STATIC | TREE_F_GLOBALLY_STATIC);
   else if (sem_globally_static(t))
      tree_set_flag(t, TREE_F_GLOBALLY_STATIC);

   return true;
}

static bool sem_check_pcall(tree_t t, nametab_t *tab)
{
   if (!tree_has_ref(t))
      return false;

   const bool is_protected = (tree_kind(t) == T_PROT_PCALL);
   if (is_protected && tree_has_name(t)
       && !sem_check(tree_name(t), tab))
      return false;

   tree_t decl = tree_ref(t);

   switch (class_of(decl)) {
   case C_PROCEDURE:
      break;
   case C_FUNCTION:
      sem_error(t, "function %s cannot be called as a procedure",
                type_pp(tree_type(decl)));
   default:
      // All other errors should be caught at parsing stage
      assert(error_count() > 0);
      return false;
   }

   if (!sem_check_call_args(t, decl, tab))
      return false;

   const tree_flags_t flags = tree_flags(decl);

   const bool never_waits = is_protected || !!(flags & TREE_F_NEVER_WAITS);
   const bool has_wait = !is_protected && !!(flags & TREE_F_HAS_WAIT);

   assert(!never_waits || !has_wait);

   tree_t sub = find_enclosing(tab, S_SUBPROGRAM);
   if (sub != NULL) {
      if (!never_waits)
         tree_clear_flag(sub, TREE_F_NEVER_WAITS);

      if (has_wait)
         tree_set_flag(sub, TREE_F_HAS_WAIT);

      if (flags & TREE_F_IMPURE_FILE)
         tree_set_flag(sub, TREE_F_IMPURE_FILE);

      if (flags & TREE_F_IMPURE_SHARED)
         tree_set_flag(sub, TREE_F_IMPURE_SHARED);

      const bool in_func = tree_kind(sub) == T_FUNC_BODY;
      const bool in_pure_func = in_func && !(tree_flags(sub) & TREE_F_IMPURE);

      if (has_wait && in_func)
         sem_error(t, "function %s cannot call procedure %s which contains "
                   "a wait statement", istr(tree_ident(sub)),
                   istr(tree_ident(decl)));
      else if ((flags & TREE_F_IMPURE_FILE) && in_pure_func) {
         diag_t *d = pedantic_diag(tree_loc(t));
         if (d != NULL) {
            diag_printf(d, "pure function %s cannot call procedure %s which "
                        "references a file object", istr(tree_ident(sub)),
                        istr(tree_ident(decl)));
            diag_emit(d);
         }
      }
      else if ((flags & TREE_F_IMPURE_SHARED) && in_pure_func) {
         diag_t *d = pedantic_diag(tree_loc(t));
         if (d != NULL) {
            diag_printf(d, "pure function %s cannot call procedure %s which "
                        "references a shared variable", istr(tree_ident(sub)),
                        istr(tree_ident(decl)));
            diag_emit(d);
         }
      }
   }

   return true;
}

static bool sem_check_wait(tree_t t, nametab_t *tab)
{
   if (tree_has_delay(t)) {
      type_t std_time = std_type(NULL, STD_TIME);
      tree_t delay = tree_delay(t);

      if (!sem_check(delay, tab))
         return false;

      if (!sem_check_type(delay, std_time, tab))
         sem_error(delay, "type of delay must be %s but have %s",
                   type_pp(std_time), type_pp(tree_type(delay)));
   }

   if (tree_has_value(t)) {
      type_t std_bool = std_type(NULL, STD_BOOLEAN);
      tree_t value = tree_value(t);

      if (!sem_check(value, tab))
         return false;

      if (!sem_check_type(value, std_bool, tab))
         sem_error(value, "type of condition must be BOOLEAN but have %s",
                   type_pp(tree_type(value)));
   }

   if (find_enclosing(tab, S_PROTECTED))
      sem_error(t, "wait statement not allowed in protected subprogram body");

   tree_t sub = find_enclosing(tab, S_SUBPROGRAM);
   if (sub != NULL) {
      if (tree_kind(sub) == T_FUNC_BODY)
         sem_error(t, "wait statement not allowed in function body");

      tree_clear_flag(sub, TREE_F_NEVER_WAITS);
      tree_set_flag(sub, TREE_F_HAS_WAIT);
   }

   tree_t proc = find_enclosing(tab, S_PROCESS);
   if (proc != NULL && tree_triggers(proc) > 0) {
      // No wait statements allowed in process with sensitivity list
      sem_error(t, "wait statement not allowed in process with "
                "sensitvity list");
   }

   return sem_check_sensitivity(t, tab);
}

static bool sem_check_assert(tree_t t, nametab_t *tab)
{
   // Rules for asserion statements are in LRM 93 section 8.2

   tree_t value = tree_value(t);
   if (!sem_check(value, tab))
      return false;

   type_t std_bool = std_type(NULL, STD_BOOLEAN);
   if (!sem_check_type(value, std_bool, tab))
      sem_error(value, "type of assertion expression must be %s but "
                "is %s", type_pp(std_bool), type_pp(tree_type(value)));

   if (tree_has_message(t)) {
      tree_t message = tree_message(t);
      if (!sem_check(message, tab))
         return false;

      type_t std_string = std_type(NULL, STD_STRING);
      if (!sem_check_type(message, std_string, tab))
         sem_error(message, "type of message be %s but is %s",
                   type_pp(std_string), type_pp(tree_type(message)));
   }

   if (tree_has_severity(t)) {
      tree_t severity = tree_severity(t);
      if (!sem_check(severity, tab))
         return false;

      type_t std_severity = std_type(NULL, STD_SEVERITY_LEVEL);
      if (!sem_check_type(severity, std_severity, tab))
         sem_error(severity, "type of severity must be %s but is %s",
                   type_pp(std_severity), type_pp(tree_type(severity)));
   }

   return true;
}

static bool sem_check_report(tree_t t, nametab_t *tab)
{
   tree_t message = tree_message(t);
   if (!sem_check(message, tab))
      return false;

   type_t std_string = std_type(NULL, STD_STRING);
   if (!sem_check_type(message, std_string, tab))
      sem_error(message, "type of message be %s but is %s",
                type_pp(std_string), type_pp(tree_type(message)));

   if (tree_has_severity(t)) {
      tree_t severity = tree_severity(t);
      if (!sem_check(severity, tab))
         return false;

      type_t std_severity = std_type(NULL, STD_SEVERITY_LEVEL);
      if (!sem_check_type(severity, std_severity, tab))
         sem_error(severity, "type of severity must be %s but is %s",
                   type_pp(std_severity), type_pp(tree_type(severity)));
   }

   return true;
}

static bool sem_check_string_literal(tree_t t)
{
   // String literals are in LRM 93 section 7.3.1

   type_t type = tree_type(t);
   type_t elem = type_base_recur(type_elem(type));

   if (type_is_none(elem))
      return false;

   const int nlits = type_enum_literals(elem);
   const int nchars = tree_chars(t);
   for (int i = 0; i < nchars; i++) {
      tree_t ch = tree_char(t, i);

      ident_t ch_i = tree_ident(ch);
      bool valid = false;
      for (int j = 0; !valid && (j < nlits); j++) {
         tree_t lit = type_enum_literal(elem, j);
         if (ch_i == tree_ident(lit))
            valid = true;
      }

      if (!valid)
         sem_error(t, "invalid character %s in string literal of type %s",
                   istr(ch_i), type_pp(type));
   }

   return true;
}

static bool sem_check_literal(tree_t t)
{
   type_t type = tree_type(t);
   if (type_is_none(type))
      return false;

   return true;
}

static bool sem_check_array_aggregate(tree_t t, nametab_t *tab)
{
   type_t composite_type = tree_type(t);
   type_t base_type = type_base_recur(composite_type);

   const bool unconstrained = type_is_unconstrained(composite_type);

   type_t elem_type = NULL;
   const int ndims = dimension_of(composite_type);
   if (ndims == 1)
      elem_type = type_elem(base_type);
   else {
      // Higher dimensions must be specified with a sub-aggregate or
      // string literal
      tree_t a0 = tree_value(tree_assoc(t, 0));
      const tree_kind_t a0_kind = tree_kind(a0);
      if (a0_kind != T_AGGREGATE && a0_kind != T_STRING)
         sem_error(a0, "second dimension of %d dimensional array type %s must "
                   "be specified by a sub-aggregate, string, or bit-string "
                   "literal", ndims, type_pp(composite_type));

      // The parser will have constructed a type with ndims - 1
      // dimensions.
      elem_type = tree_type(tree_value(tree_assoc(t, 0)));

      if (!type_is_unconstrained(elem_type)) {
         if (!sem_check_array_dims(elem_type, NULL, tab))
            return false;
      }
   }

   type_t index_type = index_type_of(composite_type, 0);

   bool have_named = false;
   bool have_pos = false;

   const int nassocs = tree_assocs(t);
   for (int i = 0; i < nassocs; i++) {
      tree_t a = tree_assoc(t, i);

      const assoc_kind_t akind = tree_subkind(a);
      switch (akind) {
      case A_RANGE:
      case A_SLICE:
         {
            tree_t r = tree_range(a, 0);
            if (!sem_check_discrete_range(r, index_type, tab))
               return false;

            have_named = true;
         }
         break;

      case A_NAMED:
         {
            tree_t name = tree_name(a);

            if (!sem_check(name, tab))
               return false;

            if (!sem_check_type(name, index_type, tab))
               sem_error(name, "type of array aggregate choice %s does not "
                         "match %s index type %s", type_pp(tree_type(name)),
                         type_pp(composite_type), type_pp(index_type));

            have_named = true;
         }
         break;

      case A_POS:
      case A_CONCAT:
         have_pos = true;
         break;

      case A_OTHERS:
         if (unconstrained)
            sem_error(a, "index range of array aggregate with others choice "
                      "cannot be determined from the context");
         break;
      }

      tree_t value = tree_value(a);

      if (!sem_check(value, tab))
         return false;

      if (!sem_check_type(value, elem_type, tab)) {
         // LRM 08 section 9.3.3.3 allows the association to be of the
         // base aggregate type as well
         const bool allow_slice =
            (akind == A_CONCAT || akind == A_SLICE)
            || (ndims == 1 && standard() >= STD_08
                && (akind == A_POS || akind == A_RANGE));

         if (allow_slice && !sem_check_type(value, composite_type, tab))
            sem_error(value, "type of %s association %s does not match "
                      "aggregate element type %s or the aggregate type "
                      "itself %s", assoc_kind_str(akind),
                      type_pp(tree_type(value)), type_pp(elem_type),
                      type_pp(composite_type));
         else if (!allow_slice)
            sem_error(value, "type of %s association %s does not match "
                      "aggregate element type %s", assoc_kind_str(akind),
                      type_pp(tree_type(value)), type_pp(elem_type));
         else
            assert(akind == A_CONCAT || akind == A_SLICE);
      }
   }

   // Named and positional associations cannot be mixed in array
   // aggregates

   if (have_named && have_pos)
      sem_error(t, "named and positional associations cannot be "
                "mixed in array aggregates");

   // If a choice is not locally static then it must be the only element

   if (have_named && nassocs > 1) {
      for (int i = 0; i < nassocs; i++) {
         tree_t a = tree_assoc(t, i);
         tree_t choice = NULL;
         switch (tree_subkind(a)) {
         case A_NAMED: choice = tree_name(a); break;
         case A_SLICE:
         case A_RANGE: choice = tree_range(a, 0); break;
         }

         if (choice && !sem_locally_static(choice))
            sem_error(choice, "a choice that is not locally static is allowed"
                      " only if the array aggregate contains a single element"
                      " association");
      }
   }

   return true;
}

static bool sem_check_record_aggregate(tree_t t, nametab_t *tab)
{
   // Checks for record aggregates are given in LRM 93 section 7.3.2.1

   type_t composite_type = tree_type(t);
   type_t base_type = type_base_recur(composite_type);

   const int nfields = type_fields(base_type);
   int pos = 0;

   LOCAL_BIT_MASK have;
   mask_init(&have, nfields);

   const int nassocs = tree_assocs(t);
   for (int i = 0; i < nassocs; i++) {
      tree_t a = tree_assoc(t, i);
      int f = -1;

      switch (tree_subkind(a)) {
      case A_NAMED:
         {
            tree_t name = tree_name(a);
            if (tree_kind(name) != T_REF)
               sem_error(name, "association choice must be a field name");
            else if (!tree_has_ref(name))
               return false;   // Was parse error

            tree_t fdecl = tree_ref(name);
            if (tree_kind(fdecl) != T_FIELD_DECL)
               return false;   // Was parse error

            f = tree_pos(fdecl);
         }
         break;

      case A_POS:
         {
            if (pos >= nfields)
               sem_error(a, "%d positional associations given but record type"
                         " %s only has %d fields", pos + 1,
                         type_pp(composite_type), nfields);

            f = pos++;
         }
         break;

      case A_OTHERS:
         f = -1;
         break;

      case A_RANGE:
         sem_error(a, "range association invalid in record aggregate");

      case A_SLICE:
      case A_CONCAT:
         fatal_trace("illegal association type in record aggregate");
      }

      int nmatched = 0;
      for (int j = 0; j < nfields; j++) {
         if ((f != -1) && (f != j))
            continue;

         tree_t field = type_field(base_type, j);
         type_t field_type = tree_type(field);

         if (mask_test(&have, j)) {
            if (f == -1)
               continue;

            tree_t ak = NULL;
            for (int k = 0; k < i; k++) {
               ak = tree_assoc(t, k);
               if (tree_subkind(ak) == A_POS && tree_pos(ak) == j)
                  break;
               else if (tree_pos(tree_ref(tree_name(ak))) == j)
                  break;
            }
            assert(ak != NULL);

            diag_t *d = diag_new(DIAG_ERROR, tree_loc(a));
            diag_printf(d, "field %s was already given a value by earlier "
                        "%s choice", istr(tree_ident(field)),
                        assoc_kind_str(tree_subkind(ak)));
            diag_hint(d, tree_loc(ak), "first choice associated with field %s",
                      istr(tree_ident(field)));
            diag_hint(d, tree_loc(a), "duplicate choice here");
            diag_emit(d);
            return false;
         }

         tree_t value = tree_value(a);

         if (!sem_check(value, tab))
            return false;

         if (!sem_check_type(value, field_type, tab))
            sem_error(value, "type of value %s does not match type %s"
                      " of field %s",
                      type_pp2(tree_type(value), field_type),
                      type_pp2(field_type, tree_type(value)),
                      istr(tree_ident(field)));

         mask_set(&have, j);
         nmatched++;
      }

      if (f == -1 && nmatched == 0)
         sem_error(a, "others association must represent at least one element");
   }

   for (int i = 0; i < nfields; i++) {
      if (!mask_test(&have, i)) {
         tree_t field = type_field(base_type, i);
         sem_error(t, "field %s does not have a value",
                   istr(tree_ident(field)));
      }
   }

   return true;
}

static bool sem_check_aggregate(tree_t t, nametab_t *tab)
{
   // Rules for aggregates are in LRM 93 section 7.3.2

   type_t composite_type = tree_type(t);

   if (type_is_none(composite_type))
      return false;
   assert(type_is_composite(composite_type));

   // All positional associations must appear before named associations
   // and those must appear before any others association

   enum { POS, NAMED, OTHERS } state = POS;

   const int nassocs = tree_assocs(t);
   for (int i = 0; i < nassocs; i++) {
      tree_t a = tree_assoc(t, i);

      switch (tree_subkind(a)) {
      case A_POS:
      case A_CONCAT:
         if (state > POS)
            sem_error(a, "positional associations must appear "
                      "first in aggregate");
         break;

      case A_NAMED:
      case A_RANGE:
      case A_SLICE:
         if (state > NAMED)
            sem_error(a, "named association must not follow "
                      "others association in aggregate");
         state = NAMED;
         break;

      case A_OTHERS:
         if (state == OTHERS)
            sem_error(a, "only a single others association "
                      "allowed in aggregate");
         state = OTHERS;
         break;
      }
   }

   if (type_is_array(composite_type))
      return sem_check_array_aggregate(t, tab);
   else
      return sem_check_record_aggregate(t, tab);
}

static bool sem_check_ref(tree_t t, nametab_t *tab)
{
   if (!tree_has_ref(t))
      return false;

   type_t type = get_type_or_null(t);
   if (type != NULL && type_is_none(type))
      return false;

   tree_t decl = tree_ref(t);
   const tree_kind_t kind = tree_kind(decl);

   switch (kind) {
   case T_PORT_DECL:
   case T_VAR_DECL:
   case T_SIGNAL_DECL:
   case T_FILE_DECL:
   case T_ENUM_LIT:
   case T_UNIT_DECL:
   case T_FUNC_DECL:
   case T_FUNC_BODY:
   case T_FUNC_INST:
   case T_PROC_DECL:
   case T_PROC_BODY:
   case T_PROC_INST:
   case T_IMPLICIT_SIGNAL:
   case T_PARAM_DECL:
      break;

   case T_CONST_DECL:
      if (!tree_has_value(decl) && is_same_region(tab, decl)) {
         diag_t *d = diag_new(DIAG_ERROR, tree_loc(t));
         diag_printf(d, "cannot reference deferred constant %s before the "
                     "elaboration of the corresponding full declaration",
                     istr(tree_ident(decl)));
         diag_hint(d, tree_loc(decl), "%s declared here",
                   istr(tree_ident(decl)));
         diag_lrm(d, STD_08, "4.8");
         diag_emit(d);
         return false;
      }
      break;

   case T_ALIAS:
      {
         switch (class_of(decl)) {
         case C_VARIABLE:
         case C_SIGNAL:
         case C_CONSTANT:
         case C_LITERAL:
            break;

         case C_DEFAULT:
            return false;   // Must have been an earlier parse error

         default:
            sem_error(t, "invalid use of alias %s", istr(tree_ident(decl)));
         }
      }
      break;

   case T_GENERIC_DECL:
      if (tree_class(decl) == C_CONSTANT)
         break;
      // Fall-through

   default:
      {
         diag_t *d = diag_new(DIAG_ERROR, tree_loc(t));
         diag_printf(d, "invalid use of %s %s", class_str(class_of(decl)),
                     istr(tree_ident(t)));
         diag_hint(d, tree_loc(decl), "%s declared here",
                   istr(tree_ident(decl)));
         diag_emit(d);
         return false;
      }
   }

   tree_t sub = find_enclosing(tab, S_SUBPROGRAM);
   if (sub != NULL) {
      if (kind == T_FILE_DECL)
         tree_set_flag(sub, TREE_F_IMPURE_FILE);
      else if (kind == T_VAR_DECL && (tree_flags(decl) & TREE_F_SHARED))
         tree_set_flag(sub, TREE_F_IMPURE_SHARED);
   }

   return true;
}

static bool sem_check_name_prefix(tree_t t, nametab_t *tab, const char *what)
{
   tree_t value = tree_value(t);
   if (!sem_check(value, tab))
      return false;

   // The prefix of a name may only be function call or another name
   switch (tree_kind(value)) {
   case T_FCALL:
   case T_PROT_FCALL:
   case T_REF:
   case T_ATTR_REF:
   case T_ALL:
   case T_ARRAY_REF:
   case T_ARRAY_SLICE:
   case T_RECORD_REF:
   case T_EXTERNAL_NAME:
      break;

   default:
      {
         diag_t *d = diag_new(DIAG_ERROR, tree_loc(t));
         diag_printf(d, "the prefix of %s must be a name or a "
                     "function call", what);
         diag_lrm(d, STD_08, "8.1");
         diag_emit(d);
         return false;
      }
   }

   return true;
}

static bool sem_check_record_ref(tree_t t, nametab_t *tab)
{
   if (!sem_check_name_prefix(t, tab, "a selected name"))
      return false;

   tree_t value = tree_value(t);
   type_t value_type = tree_type(value);

   if (type_is_none(value_type))
      return false;
   else if (!type_is_record(value_type))
      sem_error(value, "expected record type but found %s",
                type_pp(value_type));

   return true;
}

static bool sem_check_array_ref(tree_t t, nametab_t *tab)
{
   if (!sem_check_name_prefix(t, tab, "an indexed name"))
      return false;

   type_t type = tree_type(tree_value(t));

   if (!type_is_array(type))
      return false;  // Checked earlier

   const int nindex  = dimension_of(type);
   const int nparams = tree_params(t);

   if (nparams != nindex)
      sem_error(t, "prefix of indexed name has %d dimensions but %d "
                "indices given", nindex, nparams);

   bool ok = true;
   for (int i = 0; i < nparams; i++) {
      tree_t p = tree_param(t, i);
      assert(tree_subkind(p) == P_POS);

      type_t expect = index_type_of(type, i);
      tree_t value = tree_value(p);

      ok = sem_check(value, tab) && ok;

      if (ok && !sem_check_type(value, expect, tab))
         sem_error(value, "type of index %s does not match type of "
                   "array dimension %s",
                   type_pp(tree_type(value)),
                   type_pp(expect));
   }

   return ok;
}

static bool sem_check_array_slice(tree_t t, nametab_t *tab)
{
   if (!sem_check_name_prefix(t, tab, "a slice name"))
      return false;

   type_t array_type = tree_type(tree_value(t));

   if (type_is_none(array_type))
      return false;
   else if (!sem_check_incomplete(t, array_type))
      return false;
   else if (!type_is_array(array_type))
      sem_error(t, "type of slice prefix %s is not an array",
                type_pp(array_type));

   tree_t r = tree_range(t, 0);
   if (!sem_check_discrete_range(r, index_type_of(array_type, 0), tab))
      return false;

   const bool unconstrained = type_is_unconstrained(array_type);
   const range_kind_t prefix_dir =
      unconstrained ? RANGE_EXPR : direction_of(array_type, 0);

   const range_kind_t rkind = tree_subkind(r);
   const bool wrong_dir =
      !unconstrained
      && rkind != prefix_dir
      && (rkind == RANGE_TO || rkind == RANGE_DOWNTO)
      && (prefix_dir == RANGE_TO || prefix_dir == RANGE_DOWNTO);

   if (wrong_dir) {
      const char *text[] = { "TO", "DOWNTO", "?", "??", "???" };
      sem_error(t, "range direction of slice %s does not match prefix %s",
                text[rkind], text[prefix_dir]);
   }

   return true;
}

static bool sem_check_signal_attr(tree_t t)
{
   tree_t name = tree_name(t);

   if (tree_kind(name) == T_ATTR_REF)
      return sem_check_signal_attr(name);

   tree_t ref = name_to_ref(name);
   if (ref != NULL && class_of(ref) == C_SIGNAL)
      return true;

   sem_error(t, "prefix of attribute %s must denote a signal",
             istr(tree_ident(t)));
}

static bool sem_check_driving(tree_t t)
{
   // See LRM 08 section 16.2.4 for special rules about 'DRIVING and
   // 'DRIVING_VALUE

   if (!sem_check_signal_attr(t))
      return false;

   tree_t ref = name_to_ref(tree_name(t));
   if (ref == NULL || !tree_has_ref(ref))
      return false;

   tree_t decl = tree_ref(ref);
   if (tree_kind(decl) == T_PORT_DECL) {
      const port_mode_t mode = tree_subkind(decl);
      if (mode != PORT_OUT && mode != PORT_INOUT && mode != PORT_BUFFER)
         sem_error(t, "prefix of attribute %s must denote a signal or a port "
                   "with mode IN, INOUT, or BUFFER", istr(tree_ident(t)));
   }

   // TODO: check within a process

   return true;
}

static bool sem_check_attr_param(tree_t t, type_t expect, int min, int max,
                                 nametab_t *tab)
{
   const int nparams = tree_params(t);
   if (nparams == 0 && min > 0)
      sem_error(t, "attribute %s requires a parameter", istr(tree_ident(t)));
   else if (nparams > max)
      sem_error(t, "too many parameters for attribute %s", istr(tree_ident(t)));
   else if (nparams == 1) {
      tree_t dim = tree_value(tree_param(t, 0));
      if (!sem_check(dim, tab))
         return false;

      tree_t value = tree_value(tree_param(t, 0));
      if (!sem_check_type(value, expect, tab))
         sem_error(t, "expected type %s for attribute %s parameter but "
                   "have %s", type_pp(expect), istr(tree_ident(t)),
                   type_pp(tree_type(value)));
   }

   return true;
}

static bool sem_check_dimension_attr(tree_t t, nametab_t *tab)
{
   const int nparams = tree_params(t);
   if (nparams == 0)
      return true;

   assert(nparams == 1);   // Enforced by parser

   tree_t dim = tree_value(tree_param(t, 0));
   if (!sem_check(dim, tab))
      return false;

   // The parameter must be a locally static expression of type
   // universal_integer

   type_t uint = std_type(NULL, STD_UNIVERSAL_INTEGER);
   type_t dimtype = tree_type(dim);
   if (!type_eq(dimtype, uint)) {
      diag_t *d;
      if (type_is_integer(dimtype))
         d = pedantic_diag(tree_loc(dim));
      else
         d = diag_new(DIAG_ERROR, tree_loc(dim));

      if (d != NULL) {
         diag_printf(d, "dimension parameter of attribute %s must be a locally "
                     "static expression of type universal_integer",
                     istr(tree_ident(t)));
         diag_hint(d, tree_loc(dim), "expression has type %s",
                   type_pp(tree_type(dim)));
         diag_emit(d);
         return false;
      }
   }

   if (!sem_locally_static(dim))
      sem_error(dim, "dimension parameter of attribute %s must be a locally "
                "static expression", istr(tree_ident(t)));

   if (!type_is_array(tree_type(tree_name(t))))
      sem_error(t, "prefix of attribute %s with dimension is not an array",
                istr(tree_ident(t)));

   return true;
}

static bool sem_is_named_entity(tree_t t)
{
   const tree_kind_t kind = tree_kind(t);
   if (kind != T_REF)
      return false;

   tree_t decl = tree_ref(t);

   switch (tree_kind(decl)) {
   case T_SIGNAL_DECL:  case T_VAR_DECL:     case T_PORT_DECL:
   case T_ALIAS:        case T_ENTITY:       case T_ARCH:
   case T_PACKAGE:      case T_PACK_BODY:    case T_BLOCK:
   case T_FILE_DECL:    case T_CONST_DECL:   case T_FUNC_DECL:
   case T_FUNC_BODY:    case T_PROC_DECL:    case T_PROC_BODY:
   case T_PROCESS:      case T_GENERIC_DECL: case T_PARAM_DECL:
   case T_INSTANCE:     case T_PROT_DECL:    case T_PROT_BODY:
      return true;
   case T_IMPLICIT_SIGNAL:
      return tree_subkind(decl) == IMPLICIT_GUARD;   // See LRM 93 section 4.3
   default:
      return false;
   }
}

static bool sem_check_attr_ref(tree_t t, bool allow_range, nametab_t *tab)
{
   // Attribute names are in LRM 93 section 6.6

   tree_t name = tree_name(t), decl = NULL, type_decl = NULL;
   type_t named_type = NULL;

   ident_t attr = tree_ident(t);
   const attr_kind_t predef = tree_subkind(t);

   switch (tree_kind(name)) {
   case T_REF:
      {
         if (!tree_has_ref(name))
            return false;

         decl = tree_ref(name);

         if ((type_decl = aliased_type_decl(decl)) != NULL)
            named_type = tree_type(type_decl);
      }
      break;

   case T_ATTR_REF:
      if (is_type_attribute(tree_subkind(name)))
         named_type = tree_type(name);
      else {
         const bool prefix_can_be_range =
            predef == ATTR_LOW || predef == ATTR_HIGH || predef == ATTR_LEFT
            || predef == ATTR_RIGHT || predef == ATTR_ASCENDING;

         if (!sem_check_attr_ref(name, prefix_can_be_range, tab))
            return false;
      }
      break;

   default:
      if (!sem_check(name, tab))
         return false;
   }

   if (predef == ATTR_INSTANCE_NAME)
      tree_set_global_flags(t, TREE_GF_INSTANCE_NAME);
   else if (predef == ATTR_PATH_NAME)
      tree_set_global_flags(t, TREE_GF_PATH_NAME);

   switch (predef) {
   case ATTR_RANGE:
   case ATTR_REVERSE_RANGE:
      {
         if (!allow_range)
            sem_error(t, "range expression not allowed here");

         type_t name_type = tree_has_type(name) ? tree_type(name) : NULL;
         const bool is_type = type_decl != NULL;
         const bool is_discrete =
            name_type != NULL && type_is_discrete(name_type);
         const bool invalid =
            name_type == NULL
            || (!(is_discrete && is_type) && !type_is_array(name_type));

         if (invalid) {
            if (name_type != NULL && type_is_none(name_type))
               return false;
            else if (decl != NULL && class_has_type(class_of(decl))) {
               if (is_type)
                  sem_error(t, "type %s does not have a range",
                            type_pp(tree_type(decl)));
               else
                  sem_error(t, "object %s does not have a range",
                            istr(tree_ident(decl)));
            }
            else {
               assert(error_count() > 0);  // Checked in parser
               return false;
            }
         }

         if (is_type && type_is_unconstrained(name_type))
            sem_error(t, "cannot use attribute %s with unconstrained array "
                      "type %s", istr(attr), type_pp(name_type));

         if (!sem_check_dimension_attr(t, tab))
            return false;

         return true;
      }

   case ATTR_LENGTH:
      {
         type_t type = get_type_or_null(name);
         if (type == NULL)
            sem_error(name, "prefix does not have LENGTH attribute");
         else if (type_is_none(type))
            return false;
         else if (!sem_check_incomplete(t, type))
            return false;
         else if (!type_is_array(type)
                  && !(standard() >= STD_19 && type_is_discrete(type)))
            sem_error(name, "prefix of attribute LENGTH must be an array%s "
                      "but have type %s",
                      standard() >= STD_19 ? " or a discrete type" : "",
                      type_pp(type));

         if (!sem_check_dimension_attr(t, tab))
            return false;

         return true;
      }

   case ATTR_LEFT:
   case ATTR_RIGHT:
   case ATTR_LOW:
   case ATTR_HIGH:
   case ATTR_ASCENDING:
      {
         type_t type = tree_type(name);

         if (type_is_none(type))
            return false;
         else if (!sem_check_incomplete(t, type))
            return false;
         else if (!sem_check_dimension_attr(t, tab))
            return false;

         if (!type_is_array(type) && !type_is_scalar(type))
            sem_error(t, "prefix does not have attribute %s", istr(attr));

         return true;
      }

   case ATTR_LAST_EVENT:
   case ATTR_LAST_ACTIVE:
      if (!sem_check_readable(name))
         return false;
      else if (!sem_check_attr_param(t, NULL, 0, 0, tab))
         return false;
      else if (!sem_check_signal_attr(t))
         return false;

      return true;

   case ATTR_EVENT:
   case ATTR_ACTIVE:
   case ATTR_LAST_VALUE:
      if (!sem_check_readable(name))
         return false;
      else if (!sem_check_signal_attr(t))
         return false;

      return true;

   case ATTR_PATH_NAME:
   case ATTR_INSTANCE_NAME:
   case ATTR_SIMPLE_NAME:
      if (!sem_is_named_entity(name))
         sem_error(t, "prefix of %s attribute must be a named entity",
                   istr(attr));

      tree_set_flag(name, TREE_F_ATTR_PREFIX);
      return true;

   case ATTR_QUIET:
   case ATTR_STABLE:
   case ATTR_DELAYED:
      {
         if (!sem_check_readable(name))
            return false;
         else if (!sem_check_signal_attr(t))
            return false;

         if (tree_params(t) > 0) {
            tree_t value = tree_value(tree_param(t, 0));

            if (!sem_check(value, tab))
               return false;

            type_t std_time = std_type(NULL, STD_TIME);
            if (!sem_check_type(value, std_time, tab))
               sem_error(value, "attribute %s parameter must have type %s",
                         istr(attr), type_pp(std_time));
         }

         return true;
      }

   case ATTR_TRANSACTION:
      if (!sem_check_readable(name))
         return false;
      else if (!sem_check_signal_attr(t))
         return false;

      return true;

   case ATTR_DRIVING_VALUE:
   case ATTR_DRIVING:
      return sem_check_driving(t);

   case ATTR_IMAGE:
   case ATTR_VALUE:
      {
         const bool std_2019 = standard() >= STD_19;

         if (named_type == NULL && std_2019 && tree_params(t) == 0) {
            // LCS2016-18 allows attribute with object prefix
            named_type = get_type_or_null(name);
            add_param(t, name, P_POS, NULL);
         }

         if (named_type == NULL)
            sem_error(t, "prefix of attribute %s must be a type", istr(attr));
         if (!std_2019 && !type_is_scalar(named_type))
            sem_error(t, "cannot use attribute %s with non-scalar type %s",
                      istr(attr), type_pp(named_type));
         else if (std_2019 && !type_is_representable(named_type))
            sem_error(t, "cannot use attribute %s with non-representable "
                      "type %s", istr(attr), type_pp(named_type));

         type_t std_string = std_type(NULL, STD_STRING);
         type_t arg_type = predef == ATTR_IMAGE ? named_type : std_string;
         if (!sem_check_attr_param(t, arg_type, 1, 1, tab))
            return false;

         return true;
      }

   case ATTR_LEFTOF:
   case ATTR_RIGHTOF:
   case ATTR_PRED:
   case ATTR_SUCC:
   case ATTR_POS:
   case ATTR_VAL:
      {
         if (named_type == NULL && standard() >= STD_19)
            named_type = get_type_or_null(name);   // LCS2016-08 relaxation

         if (named_type == NULL)
            sem_error(t, "prefix of attribute %s must be a type", istr(attr));

         type_t name_type = tree_type(name);

         if (!type_is_discrete(name_type) && !type_is_physical(name_type))
            sem_error(t, "prefix of attribute %s must be a discrete or "
                      "physical type", istr(attr));

         if (predef == ATTR_VAL) {
            // Parameter may be any integer type
            if (tree_params(t) != 1)
               sem_error(t, "attribute VAL requires a parameter");

            type_t ptype = tree_type(tree_value(tree_param(t, 0)));
            if (!type_is_integer(ptype))
               sem_error(t, "parameter of attribute VAL must have an integer "
                         "type but found %s", type_pp(ptype));
         }
         else if (!sem_check_attr_param(t, name_type, 1, 1, tab))
            return false;

         return true;
      }

   case ATTR_BASE:
      sem_error(t, "BASE attribute is allowed only as the prefix of the name "
                "of another attribute");

   case ATTR_ELEMENT:
   case ATTR_SUBTYPE:
   case ATTR_INDEX:
      sem_error(t, "%s attribute is only allowed in a type mark", istr(attr));

   case ATTR_CONVERSE:
      if (type_kind(tree_type(name)) != T_VIEW)
         sem_error(t, "prefix of 'CONVERSE attribute must be a named mode "
                   "view or alias thereof");

      return true;

   case ATTR_REFLECT:
      if (get_type_or_null(name) == NULL)
         sem_error(t, "prefix of attribute REFLECT is not a type mark or "
                   "an object with a type");
      else if (named_type != NULL && type_is_unconstrained(named_type))
         sem_error(t, "prefix of 'REFLECT attribute must be a fully "
                   "constrained subtype");

      return true;

   case ATTR_USER:
      if (!tree_has_value(t))
         return false;

      if (!sem_static_name(name, sem_globally_static)) {
         if (tree_kind(name) == T_REF)
            sem_error(name, "%s is not a static name", istr(tree_ident(name)));
         else
            sem_error(name, "invalid attribute reference");
      }

      return true;

   default:
      fatal_trace("unhandled attribute kind %d", predef);
   }
}

static bool sem_check_qualified(tree_t t, nametab_t *tab)
{
   if (tree_has_value(t)) {
      tree_t value = tree_value(t);

      if (!sem_check(value, tab))
         return false;

      // LRM 08 section 9.3.5 qualified expressions: the operand shall have
      // the same type as the base type of the type mark
      type_t base = type_base_recur(tree_type(t));
      if (!sem_check_type(value, base, tab)) {
         diag_t *d = diag_new(DIAG_ERROR, tree_loc(value));
         diag_printf(d, "operand of qualified expression must have type %s",
                     type_pp(base));
         diag_hint(d, tree_loc(value), "operand has type %s",
                   type_pp(tree_type(value)));
         diag_lrm(d, STD_08, "9.3.5");
         diag_emit(d);
         return false;
      }
   }

   return true;
}

static bool sem_static_signal_name(tree_t t)
{
   if (!sem_static_name(t, sem_globally_static))
      return false;

   tree_t ref = name_to_ref(t);
   if (ref != NULL && !tree_has_ref(ref))
      return true;  // Suppress cascading error

   return ref != NULL && class_of(tree_ref(ref)) == C_SIGNAL;
}

static bool sem_check_port_actual(formal_map_t *formals, int nformals,
                                  tree_t param, tree_t unit, nametab_t *tab)
{
   tree_t value = tree_value(param), name = NULL, out_conv = NULL;
   tree_t decl = NULL;
   type_t type = NULL;

   switch (tree_subkind(param)) {
   case P_POS:
      {
         const int pos = tree_pos(param);
         if (pos >= nformals)
            sem_error(value, "found at least %d positional actuals but %s "
                      "has only %d port%s", pos + 1, istr(tree_ident(unit)),
                      nformals, nformals == 1 ? "" : "s");
         if (formals[pos].have)
            sem_error(value, "formal port %s already has an actual",
                      istr(tree_ident(formals[pos].decl)));
         formals[pos].have = true;
         decl = formals[pos].decl;
         type = tree_type(decl);
      }
      break;

   case P_NAMED:
      {
         tree_t ref = (name = tree_name(param));

         switch (tree_kind(name)) {
         case T_FCALL:
            if (tree_params(name) != 1)
               sem_error(name, "output conversion function must have "
                         "exactly one parameter");

            // The parser would have replaced any other valid conversion
            // function with T_CONV_FUNC
            sem_error(name, "invalid output conversion %s",
                      istr(tree_ident(name)));
            break;

         case T_CONV_FUNC:
         case T_TYPE_CONV:
            out_conv = name;
            name = ref = tree_value(name);
            break;

         default:
            break;
         }

         ref = name_to_ref(ref);
         assert(ref != NULL && tree_kind(ref) == T_REF);

         for (int i = 0; i < nformals; i++) {
            if (tree_ident(formals[i].decl) == tree_ident(ref)) {
               if (formals[i].have && !formals[i].partial)
                  sem_error(value, "formal port %s already has an actual",
                            istr(tree_ident(formals[i].decl)));
               formals[i].have    = true;
               formals[i].partial = (tree_kind(name) != T_REF);
               decl = formals[i].decl;
               tree_set_flag(ref, TREE_F_FORMAL_NAME);
               break;
            }
         }

         if (decl == NULL)
            sem_error(value, "%s has no port named %s",
                      istr(tree_ident(unit)), istr(tree_ident(ref)));

         if (!sem_static_name(name, sem_locally_static))
            sem_error(name, "formal name must be locally static");

         if (out_conv != NULL) {
            port_mode_t mode = tree_subkind(decl);

            type = tree_type((mode == PORT_INOUT) ? name : out_conv);

            if (mode == PORT_IN)
               sem_error(name, "output conversion not allowed for formal "
                         "%s with mode IN", istr(tree_ident(decl)));

            if (tree_kind(value) == T_OPEN)
               sem_error(name, "output conversion for formal %s must not "
                         "have OPEN actual", istr(tree_ident(decl)));
         }
         else
            type = tree_type(name);

         break;
      }
   }

   assert(type != NULL);

   if (!sem_check(value, tab))
      return false;

   const tree_kind_t kind = tree_kind(value);
   tree_t expr = kind == T_INERTIAL ? tree_value(value) : value;

   type_t value_type = tree_type(expr), atype;

   if (!sem_check_type(expr, type, tab)) {
      diag_t *d = diag_new(DIAG_ERROR, tree_loc(value));
      diag_printf(d, "type of actual %s does not match type %s of formal "
                  "port %s", type_pp2(value_type, type),
                  type_pp2(type, value_type), istr(tree_ident(decl)));

      if (type_is_generic(type)) {
         hash_t *map = get_generic_map(tab);
         if (map != NULL && (atype = hash_get(map, type)))
            diag_hint(d, NULL, "generic type %s is mapped to %s",
                      type_pp(type), type_pp(atype));
      }

      diag_emit(d);
      return false;
   }

   const port_mode_t mode = tree_subkind(decl);

   if (tree_kind(value) == T_OPEN) {
      if ((mode == PORT_IN) && !tree_has_value(decl))
         sem_error(value, "unconnected port %s with mode IN must have a "
                   "default value", istr(tree_ident(decl)));

      if ((mode != PORT_IN) && type_is_unconstrained(tree_type(decl)))
         sem_error(value, "port %s of unconstrained type %s cannot "
                   "be unconnected", istr(tree_ident(decl)), type_pp(type));
   }

   // Check for type conversions and conversion functions
   // These only apply if the class of the formal is not constant

   tree_t actual;
   if (kind == T_TYPE_CONV || kind == T_CONV_FUNC) {
      // Conversion functions are in LRM 93 section 4.3.2.2
      actual = tree_value(value);

      // LRM 93 section 3.2.1.1 result of a type conversion in an
      // association list cannot be an unconstrained array type
      if (type_is_unconstrained(value_type)
          && type_is_unconstrained(type))
         sem_error(value, "result of conversion for unconstrained formal "
                   "%s must be a constrained array type",
                   istr(tree_ident(decl)));

      if (mode == PORT_OUT)
         sem_error(value, "conversion not allowed for formal %s with "
                   "mode OUT", istr(tree_ident(decl)));
      else if (mode == PORT_INOUT && out_conv == NULL)
         sem_error(value, "INOUT port %s has output conversion but no "
                   "corresponding input conversion", istr(tree_ident(decl)));
   }
   else
      actual = value;    // No conversion

   tree_t ref = name_to_ref(actual);

   if (mode == PORT_IN && kind != T_INERTIAL) {
      bool is_static = true;
      if (ref != NULL && class_of(ref) == C_SIGNAL)
         is_static = sem_static_name(actual, sem_globally_static);
      else
         is_static = sem_globally_static(actual);

      // LRM 08 section 6.5.6.3 the actual is converted to a concurrent
      // signal assignment to an anonymous signal that is then
      // associated with the formal
      if (!is_static && standard() >= STD_08) {
         // The rules listed for unconstrained ports in 6.5.6.3 should
         // be covered by the check for a globally static subtype in
         // addition to the checks above
         if (type_is_unconstrained(type)
             && !sem_static_subtype(value_type, sem_globally_static)) {
            diag_t *d = diag_new(DIAG_ERROR, tree_loc(value));
            diag_printf(d, "expression associated with unconstrained formal "
                        "port %s must have a globally static subtype",
                        istr(tree_ident(decl)));
            diag_lrm(d, STD_08, "6.5.6.3");
            diag_emit(d);
            return false;
         }

         tree_t w = tree_new(T_INERTIAL);
         tree_set_loc(w, tree_loc(value));
         tree_set_value(w, value);
         tree_set_target(w, name ?: make_ref(decl));

         tree_set_value(param, w);
      }
      else if (!is_static)
         sem_error(value, "actual associated with port %s of mode IN must be "
                   "a globally static expression or static signal name",
                   istr(tree_ident(decl)));
   }
   else if (mode == PORT_INOUT && tree_class(decl) == C_VARIABLE) {
      // VHDL-2019 additions for shared variable ports
      if (ref == NULL || class_of(ref) != C_VARIABLE)
         sem_error(value, "actual associated with formal variable port %s "
                   "must either be a shared variable or a formal variable port "
                   "of another design entity", istr(tree_ident(decl)));
   }
   else if (mode == PORT_ARRAY_VIEW || mode == PORT_RECORD_VIEW) {
      if (!sem_static_signal_name(actual))
         sem_error(value, "actual associated with port %s with mode view "
                   "indication must be a static signal name",
                   istr(tree_ident(decl)));

      if (!sem_check_compatible_view(decl, actual))
         return false;
   }
   else if (mode != PORT_IN && tree_kind(actual) != T_OPEN
            && !sem_static_signal_name(actual)) {
      sem_error(value, "actual associated with port %s of mode %s must be "
                "a static signal name or OPEN",
                istr(tree_ident(decl)), port_mode_str(tree_subkind(decl)));
   }
   else if (kind == T_INERTIAL)
      tree_set_target(value, name ?: make_ref(decl));

   // Check connections between ports
   if (ref != NULL && tree_has_ref(ref)) {
      tree_t odecl = tree_ref(ref);
      if (tree_kind(odecl) == T_PORT_DECL) {
         const port_mode_t omode = tree_subkind(odecl);

         const bool error =
            (omode == PORT_BUFFER && (mode == PORT_OUT || mode == PORT_INOUT))
            || (omode != PORT_BUFFER && mode == PORT_BUFFER)
            || (omode == PORT_IN && (mode == PORT_OUT || mode == PORT_INOUT))
            || (omode == PORT_OUT && mode == PORT_IN && standard() < STD_08)
            || (omode == PORT_OUT && mode == PORT_INOUT)
            || (omode == PORT_LINKAGE && mode != PORT_LINKAGE);

         if (error) {
            diag_t *d = diag_new(DIAG_ERROR, tree_loc(value));
            diag_printf(d, "port %s with mode %s cannot be associated "
                        "with formal port %s with mode %s",
                        istr(tree_ident(odecl)), port_mode_str(omode),
                        istr(tree_ident(decl)), port_mode_str(mode));
            diag_hint(d, tree_loc(decl), "formal port %s declared here",
                      istr(tree_ident(decl)));
            diag_hint(d, tree_loc(odecl), "port %s declared here",
                      istr(tree_ident(odecl)));
            diag_emit(d);
            return false;
         }
      }
   }

   return true;
}

static bool sem_check_port_map(tree_t t, tree_t unit, nametab_t *tab)
{
   // Check there is an actual for each formal port generic
   // Rules for maps are described in LRM 93 section 5.2.1.2

   const int nformals = tree_ports(unit);
   const int nactuals = tree_params(t);

   bool ok = true;

   formal_map_t *formals LOCAL = xmalloc_array(nformals, sizeof(formal_map_t));

   for (int i = 0; i < nformals; i++) {
      formals[i].decl    = tree_port(unit, i);
      formals[i].have    = false;
      formals[i].partial = false;
   }

   for (int i = 0; i < nactuals; i++) {
      tree_t p = tree_param(t, i);
      if (tree_subkind(p) != P_NAMED)
         continue;

      tree_t name = tree_name(p);

      ok &= sem_check(name, tab);

      const tree_kind_t name_kind = tree_kind(name);
      if ((name_kind == T_ARRAY_REF || name_kind == T_ARRAY_SLICE)
          && tree_kind(tree_value(p)) == T_OPEN && standard() < STD_19) {
         diag_t *d = pedantic_diag(tree_loc(p));
         if (d != NULL) {
            diag_printf(d, "sub-elements of composite port cannot be "
                        "associated with OPEN");
            diag_emit(d);
         }
      }
   }

   if (!ok)
      return false;

   for (int i = 0; i < nactuals; i++) {
      tree_t actual = tree_param(t, i);
      ok &= sem_check_port_actual(formals, nformals, actual, unit, tab);

      if (!ok && tree_subkind(actual) == P_POS && i >= nformals)
         break;   // Prevent useless repeated errors
   }

   if (tree_kind(t) == T_BINDING)
      return ok;

   for (int i = 0; i < nformals; i++) {
      if (!formals[i].have) {
         port_mode_t mode = tree_subkind(formals[i].decl);

         if (mode == PORT_IN && !tree_has_value(formals[i].decl)) {
            error_at(tree_loc(t), "missing actual for port %s of "
                     "mode IN without a default expression",
                     istr(tree_ident(formals[i].decl)));
         }
         else if (mode == PORT_ARRAY_VIEW || mode == PORT_RECORD_VIEW)
            error_at(tree_loc(t), "missing actual for port %s with "
                     "mode view indication %s",
                     istr(tree_ident(formals[i].decl)),
                     type_pp(tree_type(tree_value(formals[i].decl))));

         type_t ftype = tree_type(formals[i].decl);
         if (mode != PORT_IN && type_is_unconstrained(ftype)) {
            error_at(tree_loc(t), "missing actual for port %s with "
                     "unconstrained array type",
                     istr(tree_ident(formals[i].decl)));
         }
      }
   }

   return ok;
}

static bool sem_check_generic_actual(formal_map_t *formals, int nformals,
                                     tree_t param, tree_t unit, nametab_t *tab)
{
   tree_t value = tree_value(param), decl = NULL;
   type_t type = NULL;

   switch (tree_subkind(param)) {
   case P_POS:
      {
         const int pos = tree_pos(param);
         if (pos >= nformals)
            sem_error(value, "found at least %d positional actuals but %s "
                      "has only %d generic%s", pos + 1, istr(tree_ident(unit)),
                      nformals, nformals == 1 ? "" : "s");
         else if (formals[pos].have)
            sem_error(value, "formal generic %s already has an actual",
                      istr(tree_ident(formals[pos].decl)));
         else if (tree_flags(formals[pos].decl) & TREE_F_PREDEFINED) {
            diag_t *d = diag_new(DIAG_WARN, tree_loc(param));
            diag_printf(d, "positional generic actual is associated with "
                        "implicit generic subprogram %s for type %s",
                        istr(tree_ident(formals[pos].decl)),
                        type_pp(tree_type(tree_port(formals[pos].decl, 0))));
            diag_hint(d, NULL, "use a named association if this was intended");
            diag_emit(d);
         }

         formals[pos].have = true;
         decl = formals[pos].decl;
         type = get_type_or_null(decl);
      }
      break;

   case P_NAMED:
      {
         tree_t name = tree_name(param);
         tree_t ref = name_to_ref(name);

         if (ref == NULL)
            sem_error(name, "invalid name in generic map");
         else if (!tree_has_ref(ref))
            return false;

         tree_t d = tree_ref(ref);
         for (int i = 0; i < nformals; i++) {
            if (formals[i].decl == d) {
               if (formals[i].have && !formals[i].partial)
                  sem_error(value, "generic %s already has an actual",
                            istr(tree_ident(formals[i].decl)));
               formals[i].have    = true;
               formals[i].partial = (tree_kind(name) != T_REF);
               decl = d;
               tree_set_flag(ref, TREE_F_FORMAL_NAME);
               break;
            }
         }

         if (decl == NULL)
            sem_error(name, "%s is not a formal generic of %s",
                      istr(tree_ident(ref)), istr(tree_ident(unit)));

         if (tree_class(decl) == C_CONSTANT || tree_kind(name) != T_REF) {
            // Do not check package or type for names here as that will
            // throw an error
            if (!sem_check(name, tab))
               return false;

            if (!sem_static_name(name, sem_locally_static))
               sem_error(name, "formal generic name must be a locally "
                         "static name");
         }

         type = get_type_or_null(name);
         break;
      }
   }

   switch (tree_class(decl)) {
   case C_TYPE:
      // The parser already called map_generic_type
      assert(tree_kind(value) == T_TYPE_REF);
      assert(type_kind(type) == T_GENERIC);

      type_t map = tree_type(value);
      if (type_is_none(map))
         return false;
      else if (!sem_check_incomplete(param, map))
         return false;

      static const char *class_strings[] = {
         [GTYPE_SCALAR] = "a scalar",
         [GTYPE_DISCRETE] = "a discrete",
         [GTYPE_INTEGER] = "an integer",
         [GTYPE_FLOATING] = "a floating-point",
         [GTYPE_PHYSICAL] = "a physical",
         [GTYPE_ACCESS] = "an access",
         [GTYPE_ARRAY] = "an array",
         [GTYPE_FILE] = "a file",
      };

      const gtype_class_t class = type_subkind(type);
      if (!type_matches_class(map, class))
         sem_error(param, "cannot map type %s to generic interface type %s "
                   "which requires %s type", type_pp(map),
                   istr(tree_ident(decl)), class_strings[class]);
      else if (class == GTYPE_ACCESS || class == GTYPE_FILE) {
         type_t expect = type_designated(type);
         type_t actual = type_designated(map);

         if (type_is_generic(expect)) {
            const gtype_class_t expect_class = type_subkind(expect);
            if (!type_matches_class(actual, expect_class))
               sem_error(param, "cannot map type %s to generic interface type "
                         "%s as the designated type %s is not %s type",
                         type_pp(map), istr(tree_ident(decl)),
                         type_pp(actual), class_strings[expect_class]);
         }
         else if (!type_eq(actual, expect))
            sem_error(param, "cannot map type %s to generic interface type "
                      "%s as the designated type %s is not %s",
                      type_pp(map), istr(tree_ident(decl)),
                      type_pp(actual), type_pp(expect));
      }
      else if (class == GTYPE_ARRAY) {
         const int nindex = type_indexes(type);
         if (nindex != dimension_of(map))
            sem_error(param, "cannot map type %s to generic interface "
                      "type %s as it has %d dimensions but the incomplete "
                      "type definition has %d", type_pp(map),
                      istr(tree_ident(decl)), dimension_of(map), nindex);

         for (int i = 0; i < nindex; i++) {
            type_t itype = type_index(type, i);
            type_t imap = index_type_of(map, i);

            if (type_is_generic(itype)) {
               const gtype_class_t expect_class = type_subkind(itype);
               if (!type_matches_class(imap, expect_class))
                  sem_error(param, "cannot map type %s to generic interface "
                            "type %s as the index type %s of the %s dimension "
                            "is not %s type", type_pp(map),
                            istr(tree_ident(decl)), type_pp(imap),
                            ordinal_str(i + 1), class_strings[expect_class]);
            }
            else if (!type_eq(imap, itype))
               sem_error(param, "cannot map type %s to generic interface type "
                         "%s as the index type %s of the %s dimension is not "
                         "%s", type_pp(map), istr(tree_ident(decl)),
                         type_pp(imap), ordinal_str(i + 1), type_pp(itype));
         }

         type_t expect = type_elem(type);
         type_t actual = type_elem(map);

          if (type_is_generic(expect)) {
            const gtype_class_t expect_class = type_subkind(expect);
            if (!type_matches_class(actual, expect_class))
               sem_error(param, "cannot map type %s to generic interface type "
                         "%s as the element type %s is not %s type",
                         type_pp(map), istr(tree_ident(decl)),
                         type_pp(actual), class_strings[expect_class]);
         }
         else if (!type_eq(actual, expect))
            sem_error(param, "cannot map type %s to generic interface type "
                      "%s as the element type %s is not %s",
                      type_pp(map), istr(tree_ident(decl)),
                      type_pp(actual), type_pp(expect));
      }

      break;

   case C_PACKAGE:
      {
         tree_t pack = NULL;
         if (tree_kind(value) == T_REF && tree_has_ref(value))
            pack = tree_ref(value);

         if (pack == NULL || tree_kind(pack) != T_PACK_INST)
            sem_error(value, "actual for generic %s is not an "
                      "instantiated package name", istr(tree_ident(decl)));
         else if (!tree_has_ref(pack))
            return false;   // Was parse error

         tree_t map = tree_value(decl);
         if (!tree_has_ref(map))
            return false;   // Was earlier error

         assert(tree_kind(map) == T_PACKAGE_MAP);

         tree_t base = tree_ref(pack);
         tree_t expect = tree_ref(map);

         if (tree_ident(base) != tree_ident(expect))
            sem_error(value, "expected an instance of package %s but have "
                      "instance of %s for generic %s", istr(tree_ident(expect)),
                      istr(tree_ident(base)), istr(tree_ident(decl)));

         map_generic_package(tab, expect, pack);
      }
      break;

   case C_FUNCTION:
   case C_PROCEDURE:
      if (!sem_check(value, tab))
         return false;

      if (!type_eq_map(tree_type(value), type, get_generic_map(tab)))
         sem_error(value, "type of actual %s does not match type %s of formal "
                   "generic %s", type_pp(tree_type(value)), type_pp(type),
                   istr(tree_ident(decl)));

      assert(tree_kind(value) == T_REF);

      if (!tree_has_ref(value))
         return false;

      tree_t sub = tree_ref(value);
      assert(is_subprogram(sub));

      const bool pure_formal = !(tree_flags(decl) & TREE_F_IMPURE);
      const bool pure_actual = !(tree_flags(sub) & TREE_F_IMPURE);

      if (pure_formal && !pure_actual)
         sem_error(value, "cannot associate impure function %s with pure "
                   "generic subprogram %s", type_pp(tree_type(value)),
                   istr(tree_ident(decl)));

      map_generic_subprogram(tab, decl, sub);
      break;

   case C_CONSTANT:
      if (!sem_check(value, tab))
         return false;

      if (!sem_check_type(value, type, tab))
         sem_error(value, "type of actual %s does not match type %s of formal "
                   "generic %s", type_pp(tree_type(value)), type_pp(type),
                   istr(tree_ident(decl)));

      sem_check_static_elab(value);
      break;

   default:
      // Was an earlier error
      break;
   }

   return true;
}

static bool sem_check_generic_map(tree_t t, tree_t unit, nametab_t *tab)
{
   // Check there is an actual for each formal generic
   // Rules for maps are described in LRM 93 section 5.2.1.2

   const int nformals = tree_generics(unit);
   const int nactuals = tree_genmaps(t);

   formal_map_t *formals LOCAL = xmalloc_array(nformals, sizeof(formal_map_t));

   for (int i = 0; i < nformals; i++) {
      formals[i].decl    = tree_generic(unit, i);
      formals[i].have    = false;
      formals[i].partial = false;
   }

   bool ok = true;

   for (int i = 0; i < nactuals; i++) {
      tree_t actual = tree_genmap(t, i);
      ok &= sem_check_generic_actual(formals, nformals, actual, unit, tab);

      if (!ok && tree_subkind(actual) == P_POS && i >= nformals)
         break;   // Prevent useless repeated errors
   }

   for (int i = 0; i < nformals; i++) {
      if (formals[i].have)
         continue;

      const class_t class = tree_class(formals[i].decl);
      if (class == C_PACKAGE)
         error_at(tree_loc(t), "missing actual for generic package %s",
                  istr(tree_ident(formals[i].decl)));
      else if (class == C_TYPE) {
         error_at(tree_loc(t), "missing actual for generic type %s",
                  istr(tree_ident(formals[i].decl)));
         map_generic_type(tab, tree_type(formals[i].decl), type_new(T_NONE));
      }
      else if (tree_has_value(formals[i].decl)) {
         tree_t value = tree_value(formals[i].decl);
         if (tree_kind(value) == T_BOX) {
            // Need to look up the matching subprogram now while we still
            // have the symbol table
            map_generic_box(tab, t, formals[i].decl, i);
         }
      }
      else {
         diag_t *d = diag_new(DIAG_ERROR, tree_loc(t));
         diag_printf(d, "missing actual for generic %s without a "
                     "default expression", istr(tree_ident(formals[i].decl)));
         diag_hint(d, tree_loc(formals[i].decl), "generic %s declared here",
                   istr(tree_ident(formals[i].decl)));
         diag_emit(d);
      }
   }

   return ok;
}

static bool sem_check_instance(tree_t t, nametab_t *tab)
{
   if (!tree_has_ref(t))
      return false;

   tree_t unit = primary_unit_of(tree_ref(t));

   if (tree_has_spec(t)) {
      tree_t spec = tree_spec(t);

      if (tree_class(t) != C_COMPONENT) {
         diag_t *d = diag_new(DIAG_ERROR, tree_loc(spec));
         diag_printf(d, "specification may only be used with component"
                     " instances");
         diag_hint(d, tree_loc(spec), "specification for %s",
                   istr(tree_ident(t)));
         diag_hint(d, tree_loc(t), "%s instance", class_str(tree_class(t)));
         diag_emit(d);
         return false;
      }

      assert(tree_kind(unit) == T_COMPONENT);   // Checked by parser

      if (tree_has_ref(spec) && tree_ref(spec) != unit) {
         diag_t *d = diag_new(DIAG_ERROR, tree_loc(spec));
         diag_printf(d, "component mismatch for instance %s: expected %s "
                     "but specification has %s", istr(tree_ident(t)),
                     istr(tree_ident(unit)), istr(tree_ident(tree_ref(spec))));
         diag_hint(d, tree_loc(spec), "specification has component %s",
                   istr(tree_ident(tree_ref(spec))));
         diag_hint(d, tree_loc(t), "instance of component %s",
                   istr(tree_ident(unit)));
         diag_emit(d);
         return false;
      }
   }

   if (!sem_check_generic_map(t, unit, tab))
      return false;

   if (!sem_check_port_map(t, unit, tab))
      return false;

   return true;
}

static bool sem_check_cond(tree_t t, nametab_t *tab)
{
   if (tree_has_value(t)) {
      type_t std_bool = std_type(NULL, STD_BOOLEAN);

      tree_t value = tree_value(t);
      if (!sem_check(value, tab))
         return false;

      if (!sem_check_type(value, std_bool, tab))
         sem_error(value, "type of condition must be %s but have %s",
                   type_pp(std_bool), type_pp(tree_type(value)));

      if (!sem_check_readable(value))
         return false;
   }

   return true;
}

static bool sem_check_if(tree_t t, nametab_t *tab)
{
   bool ok = true;
   const int nconds = tree_conds(t);
   for (int i = 0; i < nconds; i++)
      ok &= sem_check_cond(tree_cond(t, i), tab);

   return ok;
}

static bool sem_static_subtype(type_t type, static_fn_t fn)
{
   // Rules for locally static subtypes are in LRM 93 7.4.1

   if (type_is_unconstrained(type))
      return false;

   if (type_is_scalar(type))
      return true;

   if (type_is_record(type)) {
      const int nfields = type_fields(type);
      for (int i = 0; i < nfields; i++) {
         if (!sem_static_subtype(tree_type(type_field(type, i)), fn))
            return false;
      }

      return true;
   }

   switch (type_kind(type)) {
   case T_SUBTYPE:
      {
         const int ndims = dimension_of(type);
         for (int i = 0; i < ndims; i++) {
            if (!(*fn)(range_of(type, i)))
               return false;
         }

         return true;
      }
   default:
      return true;
   }
}

static bool sem_ieee_locally_static(tree_t decl)
{
   // Subprograms definined in certain IEEE packages are treated the
   // same as builtin operators in VHDL-2008

   if (standard() < STD_08)
      return false;

   ident_t unit_name = tree_ident(tree_container(decl));

   switch (is_well_known(unit_name)) {
   case W_NUMERIC_STD:
   case W_NUMERIC_BIT:
   case W_IEEE_1164:
   case W_NUMERIC_BIT_UNSIGNED:
   case W_NUMERIC_STD_UNSIGNED:
      return true;
   default:
      return false;
   }
}

static bool sem_locally_static(tree_t t)
{
   // Rules for locally static expressions are in LRM 93 7.4.1

   type_t type = tree_type(t);
   tree_kind_t kind = tree_kind(t);

   if (type_is_none(type))
      return true;   // Prevents further cascading errors

   // Any literal other than of type time
   if (kind == T_LITERAL) {
      if (tree_subkind(t) == L_PHYSICAL)
         return !type_eq(type, std_type(NULL, STD_TIME));
      else
         return true;
   }
   else if (kind == T_STRING)
      return true;
   else if ((kind == T_REF) && (tree_kind(tree_ref(t)) == T_ENUM_LIT))
      return true;
   else if (kind == T_OPEN)
      return true;

   if (kind == T_REF) {
      tree_t decl = tree_ref(t);
      const tree_kind_t dkind = tree_kind(decl);

      // A constant reference (other than a deferred constant) with a
      // locally static value
      if (dkind == T_CONST_DECL) {
         if (tree_has_value(decl))
            return sem_locally_static(tree_value(decl));
         else
            return false;
      }

      // An alias of a locally static name
      if (dkind == T_ALIAS)
         return sem_locally_static(tree_value(decl));

      // [2008] A generic reference with a locally static subtype
      if (dkind == T_GENERIC_DECL && (standard() >= STD_08 || relaxed_rules()))
         return sem_static_subtype(tree_type(decl), sem_locally_static);
   }

   // A locally static range
   if (kind == T_RANGE) {
      switch (tree_subkind(t)) {
      case RANGE_TO:
      case RANGE_DOWNTO:
         return sem_locally_static(tree_left(t))
            && sem_locally_static(tree_right(t));

      case RANGE_EXPR:
         return sem_locally_static(tree_value(t));

      default:
         return false;
      }
   }

   // A function call of an implicit operator or [2008] an operation
   // defined in one of the packages STD_LOGIC_1164, NUMERIC_BIT,
   // NUMERIC_STD, NUMERIC_BIT_UNSIGNED, or NUMERIC_STD_UNSIGNED in
   // library IEEE whose actuals are locally static expressions.
   if (kind == T_FCALL) {
      if (!tree_has_ref(t))
         return true;  // Suppress further errors
      else if (tree_flags(t) & TREE_F_LOCALLY_STATIC)
         return true;

      tree_t decl = tree_ref(t);
      if (tree_kind(decl) == T_GENERIC_DECL)
         return false;   // Not known at this point
      else if (tree_subkind(decl) == S_USER && !sem_ieee_locally_static(decl))
         return false;

      const int nparams = tree_params(t);
      for (int i = 0; i < nparams; i++) {
         if (!sem_locally_static(tree_value(tree_param(t, i))))
            return false;
      }

      return true;
   }

   if (kind == T_ATTR_REF) {
      // A predefined attribute other than those listed below whose prefix
      // prefix is either a locally static subtype or is an object that is
      // of a locally static subtype
      const attr_kind_t predef = tree_subkind(t);
      if (predef == ATTR_EVENT || predef == ATTR_ACTIVE
          || predef == ATTR_LAST_EVENT || predef == ATTR_LAST_ACTIVE
          || predef == ATTR_LAST_VALUE || predef == ATTR_DRIVING
          || predef == ATTR_DRIVING_VALUE || predef == ATTR_PATH_NAME
          || predef == ATTR_INSTANCE_NAME || predef == ATTR_SIMPLE_NAME)
         return false;
      else if (!tree_has_value(t)) {
         type_t type = tree_type(tree_name(t));
         return sem_static_subtype(type, sem_locally_static);
      }

      // Whose actual parameter (if any) is a locally static expression
      const int nparams = tree_params(t);
      for (int i = 0; i < nparams; i++) {
         if (!sem_locally_static(tree_value(tree_param(t, i))))
            return false;
      }

      // A user-defined attribute whose value is a locally static expression
      assert(tree_has_value(t));
      return sem_locally_static(tree_value(t));
   }

   // A qualified expression whose operand is locally static
   if (kind == T_QUALIFIED)
      return sem_locally_static(tree_value(t));

   // A type conversion whose expression is locally static
   if (kind == T_TYPE_CONV)
      return sem_locally_static(tree_value(t));

   // Aggregates must have locally static range and all elements
   // must have locally static values
   if (kind == T_AGGREGATE) {
      if (type_is_unconstrained(type))
         return false;

      if (type_is_array(type)) {
         if (!sem_locally_static(range_of(type, 0)))
            return false;
      }

      const int nassocs = tree_assocs(t);
      for (int i = 0; i < nassocs; i++) {
         tree_t a = tree_assoc(t, i);
         if ((tree_subkind(a) == A_NAMED) && !sem_locally_static(tree_name(a)))
            return false;

         if (!sem_locally_static(tree_value(a)))
            return false;
      }

      return true;
   }

   // A record field name
   if ((kind == T_REF) && (tree_kind(tree_ref(t)) == T_FIELD_DECL))
      return true;

   const bool std08_rules = standard() >= STD_08 || relaxed_rules();

   // [2008] An indexed name whose prefix and index expressions are
   // locally static
   if (std08_rules && kind == T_ARRAY_REF) {
      const int nparams = tree_params(t);
      for (int i = 0; i < nparams; i++) {
         if (!sem_locally_static(tree_value(tree_param(t, i))))
            return false;
      }

      return sem_locally_static(tree_value(t));
   }

   // [2008] A slice name whose prefix and range is locally static
   if (std08_rules && kind == T_ARRAY_SLICE) {
      if (!sem_locally_static(tree_range(t, 0)))
         return false;

      return sem_locally_static(tree_value(t));
   }

   // [2008] A selected name whose prefix is locally static
   if (std08_rules && kind == T_RECORD_REF)
      return sem_locally_static(tree_value(t));

   return false;
}

static bool sem_static_name(tree_t t, static_fn_t check_fn)
{
   // Rules for static names are in LRM 93 6.1

   switch (tree_kind(t)) {
   case T_REF:
      {
         tree_t decl = tree_ref(t);
         switch (tree_kind(decl)) {
         case T_SIGNAL_DECL:
         case T_VAR_DECL:
         case T_CONST_DECL:
         case T_PORT_DECL:
         case T_TYPE_DECL:
         case T_SUBTYPE_DECL:
         case T_ENTITY:
         case T_ARCH:
         case T_PACK_BODY:
         case T_PACKAGE:
         case T_FUNC_BODY:
         case T_PROC_BODY:
         case T_FUNC_DECL:
         case T_PROC_DECL:
         case T_PROCESS:
         case T_BLOCK:
         case T_ENUM_LIT:
         case T_IMPLICIT_SIGNAL:
         case T_GENERIC_DECL:
         case T_PARAM_DECL:
         case T_CONCURRENT:
         case T_VIEW_DECL:
            return true;
         case T_ALIAS:
            return sem_static_name(tree_value(decl), check_fn);
         default:
            return false;
         }
      }

   case T_EXTERNAL_NAME:
      return true;

   case T_RECORD_REF:
      return sem_static_name(tree_value(t), check_fn);

   case T_ARRAY_REF:
      {
         if (!sem_static_name(tree_value(t), check_fn))
            return false;

         const int nparams = tree_params(t);
         for (int i = 0; i < nparams; i++) {
            if (!(*check_fn)(tree_value(tree_param(t, i))))
               return false;
         }

         return true;
      }

   case T_ARRAY_SLICE:
      {
         if (!sem_static_name(tree_value(t), check_fn))
            return false;

         return (*check_fn)(tree_range(t, 0));
      }

   case T_ATTR_REF:
      {
         switch (tree_subkind(t)) {
         case ATTR_DELAYED:
         case ATTR_STABLE:
         case ATTR_QUIET:
         case ATTR_TRANSACTION:
            return sem_static_name(tree_name(t), check_fn);
         default:
            return false;
         }
      }

   default:
      return false;
   }
}

static bool sem_globally_static(tree_t t)
{
   // Rules for globally static expressions are in LRM 93 7.4.2

   type_t type = tree_type(t);
   tree_kind_t kind = tree_kind(t);

   if (type_is_none(type))
      return true;   // Prevents further cascading errors

   // A literal of type TIME

   if (type_eq(type, std_type(NULL, STD_TIME))) {
      if (kind == T_REF && tree_kind(tree_ref(t)) == T_UNIT_DECL)
         return true;
      else if (kind == T_LITERAL && tree_subkind(t) == L_PHYSICAL)
         return true;
   }

   // A locally static primary

   if (sem_locally_static(t))
      return true;

   // A generic constant, generate parameter, or constant

   if (kind == T_REF) {
      tree_t decl = tree_ref(t);
      const tree_kind_t decl_kind = tree_kind(decl);
      return decl_kind == T_GENERIC_DECL || decl_kind == T_CONST_DECL;
   }
   else if (kind == T_EXTERNAL_NAME)
      return tree_class(t) == C_CONSTANT;

   // An alias whose aliased name is globally static

   if (kind == T_ALIAS)
      return sem_globally_static(tree_value(t));

   if (kind == T_RANGE) {
      if (tree_subkind(t) == RANGE_EXPR)
         return sem_globally_static(tree_value(t));

      if (!sem_globally_static(tree_left(t)))
         return false;

      if (!sem_globally_static(tree_right(t)))
         return false;

      return true;
   }

   // Aggregates must have globally static range and all elements
   // must have globally static values
   if (kind == T_AGGREGATE) {
      if (type_is_array(type)
          && !type_is_unconstrained(type)
          && !sem_globally_static(range_of(type, 0)))
         return false;

      const int nassocs = tree_assocs(t);
      for (int i = 0; i < nassocs; i++) {
         tree_t a = tree_assoc(t, i);
         if ((tree_subkind(a) == A_NAMED) && !sem_globally_static(tree_name(a)))
            return false;

         if (!sem_globally_static(tree_value(a)))
            return false;
      }

      return true;
   }

   // A function call of a pure function with globally static actuals
   if (kind == T_FCALL) {
      tree_t decl = tree_ref(t);
      if (tree_flags(decl) & TREE_F_IMPURE)
         return false;

      bool all_static = true;
      const int nparams = tree_params(t);
      for (int i = 0; i < nparams; i++) {
         tree_t p = tree_param(t, i);
         all_static = all_static && sem_globally_static(tree_value(p));
      }
      return all_static;
   }

   if (kind == T_ATTR_REF) {
      const attr_kind_t predef = tree_subkind(t);

      // A predefined attribute that is one of 'SIMPLE_NAME,
      // 'INSTANCE_NAME, or 'PATH_NAME
      if (predef == ATTR_SIMPLE_NAME || predef == ATTR_INSTANCE_NAME
          || predef == ATTR_PATH_NAME)
         return true;   // Clause j

      // A predefined attribute other than those listed below whose
      // prefix is either a globally static subtype or is an object or
      // function call that is of a globally static subtype, or in 2008,
      // a prefix which is a appropriate for a globally static attribute
      if (predef == ATTR_EVENT || predef == ATTR_ACTIVE
          || predef == ATTR_LAST_EVENT || predef == ATTR_LAST_ACTIVE
          || predef == ATTR_LAST_VALUE || predef == ATTR_DRIVING
          || predef == ATTR_DRIVING_VALUE)
         return false;   // Clause k
      else if (predef == ATTR_USER) {
         // A user-defined attribute whose value is a globally static
         // expression
         return sem_globally_static(tree_value(t));
      }

      tree_t name = tree_name(t);

      if (standard() >= STD_08 || relaxed_rules()) {
         // LRM 08 section 9.4.3: A prefix is appropriate for a globally
         // static attribute if it denotes a signal, a constant, a type
         // or subtype, a globally static function call, a variable that
         // is not of an access type, or a variable of an access type
         // whose designated subtype is fully constrained.

         if (tree_kind(name) == T_FCALL)
            return sem_globally_static(name);

         tree_t ref = name_to_ref(name);
         if (ref == NULL)
            return false;

         tree_t decl = tree_ref(ref);
         const tree_kind_t dkind = tree_kind(decl);
         if (dkind == T_VAR_DECL && type_is_access(tree_type(name)))
            return false;

         return dkind == T_CONST_DECL || dkind == T_SIGNAL_DECL
            || dkind == T_TYPE_DECL || dkind == T_VAR_DECL
            || dkind == T_SUBTYPE_DECL || dkind == T_PORT_DECL
            || dkind == T_GENERIC_DECL;
      }

      type_t type = get_type_or_null(name);
      if (type == NULL)
         return false;

      return sem_static_subtype(type, sem_globally_static);
   }

   // A qualified expression whose operand is globally static

   if (kind == T_QUALIFIED)
      return sem_globally_static(tree_value(t));

   // A type conversion whose operand is globally static

   if (kind == T_TYPE_CONV)
      return sem_globally_static(tree_value(t));

   // TODO: clauses o, p

   // A sub-element or slice where indexes are globally static

   if (kind == T_ARRAY_REF) {
      if (!sem_globally_static(tree_value(t)))
         return false;

      const int nparams = tree_params(t);
      for (int i = 0; i < nparams; i++) {
         if (!sem_globally_static(tree_value(tree_param(t, i))))
            return false;
      }

      return true;
   }
   else if (kind == T_ARRAY_SLICE) {
      if (!sem_globally_static(tree_value(t)))
         return false;

      if (!sem_globally_static(tree_range(t, 0)))
         return false;

      return true;
   }
   else if (kind == T_RECORD_REF)
      return sem_globally_static(tree_value(t));

   return false;
}

static bool sem_check_case(tree_t t, nametab_t *tab)
{
   tree_t test = tree_value(t);
   if (!sem_check(test, tab))
      return false;

   type_t type = tree_type(test);

   // LRM 93 8.8 if the type of the expression is an array then it must be
   // a one dimensional character array type

   const bool is_1d_character_array = type_is_character_array(type);
   const bool valid = is_1d_character_array || type_is_discrete(type);

   if (!valid) {
      diag_t *d = diag_new(DIAG_ERROR, tree_loc(test));
      diag_printf(d, "case expression must have a discrete type or one "
                  "dimensional character array type");
      if (type_is_array(type) && dimension_of(type) != 1)
         diag_hint(d, tree_loc(test), "array has %d dimensions",
                   dimension_of(type));
      else if (type_is_array(type))
         diag_hint(d, tree_loc(test), "type %s is not a character array",
                   type_pp(type));
      else
         diag_hint(d, tree_loc(test), "type is %s", type_pp(type));
      diag_lrm(d, STD_08, "10.9");
      diag_emit(d);
      return false;
   }

   if (is_1d_character_array && standard() < STD_08) {
      // VHDL-93 requires a locally static subtype, relaxed in later
      // revisions
      if (!sem_static_subtype(type, sem_locally_static))
         sem_error(test, "case expression must have locally static subtype");
   }

   static_fn_t static_fn = sem_locally_static;
   const char *static_str = "locally";

   if (tree_kind(t) == T_CASE_GENERATE) {
      static_fn = sem_globally_static;
      static_str = "globally";
   }

   const int nstmts = tree_stmts(t);
   for (int i = 0; i < nstmts; i++) {
      tree_t alt = tree_stmt(t, i);

      const int nassocs = tree_assocs(alt);
      for (int j = 0; j < nassocs; j++) {
         tree_t a = tree_assoc(alt, j);
         switch (tree_subkind(a)) {
         case A_OTHERS:
            if (j != nassocs - 1 || i != nstmts - 1) {
               diag_t *d = diag_new(DIAG_ERROR, tree_loc(a));
               diag_printf(d, "others choice must appear last");
               diag_hint(d, tree_loc(a), "others choice");

               tree_t more = j + 1 < nassocs
                  ? tree_assoc(alt, j + 1) : tree_assoc(tree_stmt(t, i + 1), 0);
               diag_hint(d, tree_loc(more), "further choices follow this");

               diag_emit(d);
               return false;
            }
            break;

         case A_NAMED:
            {
               tree_t name = tree_name(a);
               if (!sem_check(name, tab))
                  return false;

               if (!sem_check_type(name, type, tab))
                  sem_error(name, "case choice must have type %s but found %s",
                            type_pp(type), type_pp(tree_type(name)));
               else if (!(*static_fn)(name))
                  sem_error(name, "case choice must be %s static", static_str);
            }
            break;

         case A_RANGE:
            {
               tree_t r = tree_range(a, 0);
               if (!sem_check_discrete_range(r, type, tab))
                  return false;

               switch (tree_subkind(r)) {
               case RANGE_TO:
               case RANGE_DOWNTO:
                  if (!(*static_fn)(tree_left(r)))
                     sem_error(tree_left(r), "left index of case choice "
                               "range is not %s static", static_str);
                  else if (!(*static_fn)(tree_right(r)))
                     sem_error(tree_right(r), "right index of case choice "
                               "range is not %s static", static_str);
                  break;

               case RANGE_EXPR:
                  if (!(*static_fn)(tree_value(r)))
                     sem_error(tree_value(r), "range expression is not %s "
                               "static", static_str);
                  break;

               default:
                  return false;
               }
            }
            break;
         }
      }
   }

   return true;
}

static bool sem_check_match_case(tree_t t, nametab_t *tab)
{
   // Matching case statement is in LRM 08 section 10.9

   if (!sem_check_case(t, tab))
      return false;

   tree_t value = tree_value(t);
   type_t type = tree_type(value);

   type_t std_bit = std_type(NULL, STD_BIT);
   type_t std_logic = ieee_type(IEEE_STD_ULOGIC);

   type_t elem = type;
   if (type_is_array(type) && dimension_of(type) == 1)
      elem = type_elem(type);

   if (!type_eq(elem, std_bit) && !type_eq(elem, std_logic)) {
      diag_t *d = diag_new(DIAG_ERROR, tree_loc(value));
      diag_printf(d, "type of expression in a matching case statement must be "
                  "BIT, STD_ULOGIC, or a one-dimensional array of these types");
      diag_hint(d, tree_loc(value), "type is %s", type_pp(type));
      diag_lrm(d, STD_08, "10.9");
      diag_emit(d);
      return false;
   }

   return true;
}

static bool sem_check_return(tree_t t, nametab_t *tab)
{
   tree_t sub = find_enclosing(tab, S_SUBPROGRAM);
   if (sub == NULL)
      sem_error(t, "return statement not allowed outside subprogram");

   if (tree_has_value(t)) {
      if (tree_kind(sub) == T_PROC_BODY)
         sem_error(t, "cannot return a value from a procedure");

      type_t expect = type_result(tree_type(sub));

      if (!sem_check(tree_value(t), tab))
         return false;

      if (!sem_check_type(tree_value(t), expect, tab))
         sem_error(t, "expected return type %s but have %s",
                   type_pp(expect), type_pp(tree_type(tree_value(t))));
   }
   else if (tree_kind(sub) == T_FUNC_BODY)
      sem_error(t, "return in function must have an expression");

   return true;
}

static bool sem_check_cond_return(tree_t t, nametab_t *tab)
{
   tree_t sub = find_enclosing(tab, S_SUBPROGRAM);
   if (sub == NULL)
      sem_error(t, "return statement not allowed outside subprogram");

   if (tree_kind(sub) != T_PROC_BODY)
      sem_error(t, "conditional return statement without value is only "
                "valid inside a procedure");

   tree_t value = tree_value(t);
   if (!sem_check(value, tab))
      return false;

   type_t std_bool = std_type(NULL, STD_BOOLEAN);

   if (!sem_check_type(value, std_bool, tab))
      sem_error(value, "type of condition must be %s but have %s",
                type_pp(std_bool), type_pp(tree_type(value)));

   return true;
}

static bool sem_check_while(tree_t t, nametab_t *tab)
{
   type_t std_bool = std_type(NULL, STD_BOOLEAN);

   tree_t value = tree_value(t);
   if (!sem_check(value, tab))
      return false;

   if (!sem_check_type(value, std_bool, tab))
      sem_error(value, "type of loop condition must be %s but is %s",
                type_pp(std_bool), type_pp(tree_type(value)));

   return true;
}

static bool sem_check_for(tree_t t, nametab_t *tab)
{
   if (!sem_check_discrete_range(tree_range(t, 0), NULL, tab))
      return false;

   tree_t idecl = tree_decl(t, 0);

   if (!sem_check_subtype(idecl, tree_type(idecl), tab))
      return false;

   return true;
}

static bool sem_check_block(tree_t t, nametab_t *tab)
{
   if (!sem_check_generic_map(t, t, tab))
      return false;

   if (!sem_check_port_map(t, t, tab))
      return false;

   const int ndecls = tree_decls(t);
   for (int i = 0; i < ndecls; i++) {
      tree_t d = tree_decl(t, i);
      sem_check_static_elab(d);
   }

   return true;
}

static bool sem_check_loop_control(tree_t t, nametab_t *tab)
{
   if (tree_has_value(t)) {
      tree_t value = tree_value(t);
      if (!sem_check(value, tab))
         return false;

      type_t std_bool = std_type(NULL, STD_BOOLEAN);
      if (!type_eq(tree_type(value), std_bool))
         sem_error(value, "type of %s condition must be %s but is %s",
                   (tree_kind(t) == T_EXIT) ? "exit" : "next",
                   type_pp(std_bool), type_pp(tree_type(value)));
   }

   return true;
}

static bool sem_check_attr_decl(tree_t t)
{
   if (!sem_no_access_file_or_protected(t, tree_type(t), "attributes"))
      return false;

   return true;
}

static bool sem_check_attr_spec(tree_t t, nametab_t *tab)
{
   tree_t value = tree_value(t);
   if (!sem_check(value, tab))
      return false;

   type_t type = tree_type(t);
   if (!sem_check_type(value, type, tab))
      sem_error(t, "expected attribute specification for %s to have type %s "
                "but found %s", istr(tree_ident(t)), type_pp(type),
                type_pp(tree_type(value)));

   if (!tree_has_ref(t))
      return false;

   tree_t decl = tree_ref(t);
   const class_t class = tree_class(t);

   if (class_of(decl) != class)
      sem_error(t, "class of object %s is %s not %s",
                istr(tree_ident(decl)), class_str(class_of(decl)),
                class_str(class));

   switch (is_well_known(tree_ident(t))) {
   case W_NEVER_WAITS:
      {
         bool flag;
         if (!folded_bool(value, &flag))
            sem_error(value, "expression must be a BOOLEAN literal");
         else if (class != C_PROCEDURE)
            sem_error(t, "NEVER_WAITS attribute can only be applied to "
                      "procedures");
         else if (flag && !tree_frozen(decl))
            tree_set_flag(decl, TREE_F_NEVER_WAITS);
      }
      break;

   case W_FOREIGN:
      if (!tree_frozen(decl))
         tree_set_flag(decl, TREE_F_NEVER_WAITS);
      break;

   default:
      break;
   }

   return true;
}

static bool sem_check_if_generate(tree_t t, nametab_t *tab)
{
   const int nconds = tree_conds(t);
   for (int i = 0; i < nconds; i++) {
      tree_t cond = tree_cond(t, i);

      if (!sem_check_cond(cond, tab))
         return false;

      if (tree_has_value(cond)) {
         tree_t value = tree_value(cond);
         if (!sem_globally_static(value))
            sem_error(value, "condition of generate statement must be static");
      }
   }

   return true;
}

static bool sem_check_for_generate(tree_t t, nametab_t *tab)
{
   tree_t r = tree_range(t, 0);
   if (!sem_check_discrete_range(r, NULL, tab))
      return false;

   if (!sem_globally_static(r))
      sem_error(r, "range of generate statement must be static");

   tree_t idecl = tree_decl(t, 0);
   assert(tree_kind(idecl) == T_GENERIC_DECL);

   if (!sem_check_subtype(idecl, tree_type(idecl), tab))
      return false;

   return true;
}

static bool sem_check_open(tree_t t)
{
   return true;
}

static bool sem_check_file_decl(tree_t t, nametab_t *tab)
{
   // Rules for file declarations are in LRM 93 section 4.3.1.4

   if (!type_is_file(tree_type(t)))
      sem_error(t, "file declarations must have file type");

   if (tree_has_value(t)) {
      tree_t value = tree_value(t);
      if (!sem_check(value, tab))
         return false;

      if (!sem_check_type(value, std_type(NULL, STD_STRING), tab))
         sem_error(value, "file name must have type STRING");

      tree_t mode = tree_file_mode(t);
      if (!sem_check(mode, tab))
         return false;

      if (!sem_check_type(mode, std_type(NULL, STD_FILE_OPEN_KIND), tab))
         sem_error(mode, "open mode must have type FILE_OPEN_KIND");
   }

   tree_t sub = find_enclosing(tab, S_SUBPROGRAM);
   const bool in_pure_func =
      sub != NULL && tree_kind(sub) == T_FUNC_BODY
      && !(tree_flags(sub) & TREE_F_IMPURE);

   if (in_pure_func) {
      diag_t *d = pedantic_diag(tree_loc(t));
      if (d != NULL) {
         diag_printf(d, "cannot declare a file object in a pure function");
         diag_emit(d);
      }
   }

   return true;
}

static bool sem_check_new(tree_t t, nametab_t *tab)
{
   // Rules for allocators are in LRM 93 section 7.3.6

   tree_t value = tree_value(t);
   type_t access_type = tree_type(t);

   if (type_is_none(access_type))
      return false;

   assert(type_is_access(access_type));
   assert(tree_kind(value) == T_QUALIFIED);

   if (!sem_check(value, tab))
      return false;

   type_t type = tree_type(value);

   if (type_is_none(type))
      return false;

   if (!sem_check_subtype(value, type, tab))
      return false;

   const bool has_initial = tree_has_value(value);

   if (!has_initial && type_is_unconstrained(type))
      sem_error(t, "unconstrained array type %s not allowed in allocator "
                "expression", type_pp(type));
   else if (!sem_check_incomplete(t, type))
      return false;
   else if (type_is_protected(type)) {
      if (has_initial)
         sem_error(t, "protected type %s cannot have initial value",
                   type_pp(type));
      else if (standard() >= STD_19)
         tree_set_global_flags(t, TREE_GF_INSTANCE_NAME | TREE_GF_PATH_NAME);
   }

   type_t designated = type_designated(access_type);

   if (!type_eq(type, designated))
      sem_error(value, "type of allocator expresion %s does not match "
                "access type %s", type_pp(type), type_pp(designated));

   return true;
}

static bool sem_check_all(tree_t t, nametab_t *tab)
{
   if (!sem_check_name_prefix(t, tab, "a selected name"))
      return false;

   tree_t value = tree_value(t);
   type_t value_type = tree_type(value);

   if (type_is_none(value_type))
      return false;

   if (!sem_check_readable(value))
      return false;

   if (!type_is_access(value_type)) {
      diag_t *d = diag_new(DIAG_ERROR, tree_loc(value));
      diag_printf(d, "prefix of a selected name with suffix ALL must "
                  "have access type");
      diag_hint(d, tree_loc(value), "prefix has type %s", type_pp(value_type));
      diag_lrm(d, STD_08, "8.3");
      diag_emit(d);
      return false;
   }

   return true;
}

static bool sem_check_binding(tree_t t, nametab_t *tab)
{
   if (!tree_has_ref(t))
      return false;

   tree_t unit = primary_unit_of(tree_ref(t));
   if (tree_kind(unit) == T_ENTITY) {
      if (tree_genmaps(t) > 0 && !sem_check_generic_map(t, unit, tab))
         return false;

      if (tree_params(t) > 0 && !sem_check_port_map(t, unit, tab))
         return false;
   }

   return true;
}

static bool sem_check_block_config(tree_t t, nametab_t *tab)
{
   return true;
}

static bool sem_check_spec(tree_t t, nametab_t *tab)
{
   if (!tree_has_ref(t))
      return false;

   tree_t comp = tree_ref(t);
   assert(tree_kind(comp) == T_COMPONENT);

   if (!tree_has_value(t))
      return true;

   tree_t bind = tree_value(t);
   assert(tree_kind(bind) == T_BINDING);

   if (!sem_check(bind, tab))
      return false;

   tree_t unit = tree_ref(bind);
   tree_t entity = primary_unit_of(unit);
   assert(tree_kind(entity) == T_ENTITY);

   bool ok = true;

   if (tree_genmaps(bind) == 0) {
      const int c_ngenerics = tree_generics(comp);
      const int e_ngenerics = tree_generics(entity);

      bit_mask_t have;
      mask_init(&have, e_ngenerics);

      bool have_named = false;
      for (int i = 0; i < c_ngenerics; i++) {
         tree_t cg = tree_generic(comp, i);

         int epos = 0;
         tree_t match = NULL;
         for (; epos < e_ngenerics; epos++) {
            tree_t eg = tree_generic(entity, epos);
            if (tree_ident(eg) == tree_ident(cg)) {
               match = eg;
               mask_set(&have, epos);
               break;
            }
         }

         if (match == NULL) {
            diag_t *d = diag_new(DIAG_ERROR, tree_loc(t));
            diag_printf(d, "generic %s in component %s has no corresponding "
                        "generic in entity %s", istr(tree_ident(cg)),
                        istr(tree_ident(comp)), istr(tree_ident(entity)));
            diag_hint(d, tree_loc(cg), "generic %s declared here",
                      istr(tree_ident(cg)));
            diag_emit(d);

            ok = false;
            continue;
         }

         tree_t value;
         if (tree_class(cg) == C_PACKAGE) {
            tree_t pmap = tree_value(cg);
            assert(tree_kind(pmap) == T_PACKAGE_MAP);

            tree_t expect = tree_value(match);
            assert(tree_kind(expect) == T_PACKAGE_MAP);

            if (tree_ref(pmap) != tree_ref(expect)) {
               diag_t *d = diag_new(DIAG_ERROR, tree_loc(t));
               diag_printf(d, "generic package %s in component %s does not "
                           "match entity %s", istr(tree_ident(cg)),
                           istr(tree_ident(comp)), istr(tree_ident(entity)));
               diag_hint(d, tree_loc(cg), "declaration of generic %s in "
                         "component as instance of %s", istr(tree_ident(cg)),
                         istr(tree_ident(pmap)));
               diag_hint(d, tree_loc(match), "declaration of generic %s in "
                         "entity as instance of %s", istr(tree_ident(match)),
                         istr(tree_ident(expect)));
               diag_emit(d);

               ok = false;
               continue;
            }

            value = tree_new(T_REF);
            tree_set_ident(value, tree_ident(cg));
            tree_set_ref(value, cg);
         }
         else {
            type_t ctype = tree_type(cg);
            type_t etype = tree_type(match);
            if (!type_eq(ctype, etype)) {
               diag_t *d = diag_new(DIAG_ERROR, tree_loc(t));
               diag_printf(d, "generic %s in component %s has type %s which "
                           "is incompatible with type %s in entity %s",
                           istr(tree_ident(cg)), istr(tree_ident(comp)),
                           type_pp2(ctype, etype), type_pp2(etype, ctype),
                           istr(tree_ident(entity)));
               diag_hint(d, tree_loc(cg), "declaration of generic %s in "
                         "component", istr(tree_ident(cg)));
               diag_hint(d, tree_loc(match), "declaration of generic %s in "
                         "entity", istr(tree_ident(match)));
               diag_emit(d);

               ok = false;
               continue;
            }

            value = make_ref(cg);
         }

         tree_t map = tree_new(T_PARAM);
         tree_set_loc(map, tree_loc(t));
         tree_set_value(map, value);

         if (!have_named && epos == i) {
            tree_set_subkind(map, P_POS);
            tree_set_pos(map, epos);
         }
         else {
            tree_set_subkind(map, P_NAMED);
            tree_set_name(map, make_ref(match));
            have_named = true;
         }

         tree_add_genmap(bind, map);
      }

      for (int i = 0; i < e_ngenerics; i++) {
         tree_t eg = tree_generic(entity, i);
         if (!mask_test(&have, i) && !tree_has_value(eg)) {
            diag_t *d = diag_new(DIAG_ERROR, tree_loc(t));
            diag_printf(d, "generic %s in entity %s without a default value "
                        "has no corresponding generic in component %s",
                        istr(tree_ident(eg)),  istr(tree_ident(entity)),
                        istr(tree_ident(comp)));
            diag_hint(d, tree_loc(eg), "generic %s declared here",
                      istr(tree_ident(eg)));
            diag_emit(d);

            ok = false;
            continue;
         }
      }

      mask_free(&have);
   }

   if (tree_params(bind) == 0) {
      const int c_nports = tree_ports(comp);
      const int e_nports = tree_ports(entity);

      bit_mask_t have;
      mask_init(&have, e_nports);

      bool have_named = false;
      for (int i = 0; i < c_nports; i++) {
         tree_t cp = tree_port(comp, i);

         int epos = 0;
         tree_t match = NULL;
         for (; match == NULL && epos < e_nports; epos++) {
            tree_t ep = tree_port(entity, epos);
            if (tree_ident(ep) == tree_ident(cp)) {
               match = ep;
               mask_set(&have, epos);
               break;
            }
         }

         if (match == NULL) {
            diag_t *d = diag_new(DIAG_ERROR, tree_loc(t));
            diag_printf(d, "port %s in component %s has no corresponding port "
                        "in entity %s", istr(tree_ident(cp)),
                        istr(tree_ident(comp)), istr(tree_ident(entity)));
            diag_hint(d, tree_loc(cp), "port %s declared here",
                      istr(tree_ident(cp)));
            diag_emit(d);

            ok = false;
            continue;
         }

         type_t ctype = tree_type(cp);
         type_t etype = tree_type(match);
         if (!type_eq(ctype, etype)) {
            diag_t *d = diag_new(DIAG_ERROR, tree_loc(t));
            diag_printf(d, "port %s in component %s has type %s which is "
                        "incompatible with type %s in entity %s",
                        istr(tree_ident(cp)), istr(tree_ident(comp)),
                        type_pp2(ctype, etype), type_pp2(etype, ctype),
                        istr(tree_ident(entity)));
            diag_hint(d, tree_loc(cp), "declaration of port %s in component",
                      istr(tree_ident(cp)));
            diag_hint(d, tree_loc(match), "declaration of port %s in entity",
                      istr(tree_ident(match)));
            diag_emit(d);

            ok = false;
            continue;
         }

         if (!have_named && epos == i)
            add_param(bind, make_ref(cp), P_POS, NULL);
         else {
            add_param(bind, make_ref(cp), P_NAMED, make_ref(match));
            have_named = true;
         }
      }

      for (int i = 0; i < e_nports; i++) {
         if (mask_test(&have, i))
            continue;

         tree_t ep = tree_port(entity, i);

         const bool open_ok =
            tree_has_value(ep)
            || (tree_subkind(ep) == PORT_OUT
                && !type_is_unconstrained(tree_type(ep)));

         if (!open_ok) {
            diag_t *d = diag_new(DIAG_ERROR, tree_loc(t));
            diag_printf(d, "port %s in entity %s without a default value "
                        "has no corresponding port in component %s",
                        istr(tree_ident(ep)), istr(tree_ident(entity)),
                        istr(tree_ident(comp)));
            diag_hint(d, tree_loc(ep), "port %s declared here",
                      istr(tree_ident(ep)));
            diag_emit(d);

            ok = false;
            continue;
         }
      }
   }

   return ok;
}

static bool sem_check_configuration(tree_t t, nametab_t *tab)
{
   if (!tree_has_primary(t))
      return false;   // Was parse error

   tree_t of = tree_primary(t);

   // LRM 08 section 3.4.1: for a configuration of a given design
   // entity, both the configuration declaration and the corresponding
   // entity declaration shall reside in the same library
   ident_t elib = ident_until(tree_ident(of), '.');
   if (standard() < STD_19 && elib != lib_name(lib_work())) {
      diag_t *d = pedantic_diag(tree_loc(t));
      if (d != NULL) {
         ident_t ename = ident_rfrom(tree_ident(of), '.');
         diag_printf(d, "configuration declaration %s must reside in the "
                     "same library as entity %s", istr(tree_ident(t)),
                     istr(ename));
         diag_hint(d, NULL, "entity %s is in library %s which is not the same "
                   "as the current working library %s",
                   istr(ename), istr(elib), istr(lib_name(lib_work())));
         diag_lrm(d, STD_08, "3.4");
         diag_emit(d);
         return false;
      }
   }

   return true;
}

static bool sem_check_prot_body(tree_t t, nametab_t *tab)
{
   // Rules for protected type bodies are in LRM 00 section 3.5.2

   type_t type = tree_type(t);
   if (type_is_none(type))
      return false;

   if (!sem_check_missing_bodies(t, tab))
      return false;

   return true;
}

static bool sem_check_implicit_signal(tree_t t, nametab_t *tab)
{
   tree_t value = tree_value(t);
   type_t type  = tree_type(t);

   if (!sem_check(value, tab))
      return false;

   switch (tree_subkind(t)) {
   case IMPLICIT_GUARD:
      if (!sem_check_type(value, type, tab))
         sem_error(value, "guard expression must have type %s but "
                   "found %s", type_pp2(type, tree_type(value)),
                   type_pp2(tree_type(value), type));
      break;
   }

   return true;
}

static bool sem_check_context_decl(tree_t t, nametab_t *tab)
{
   // Context declarations are in LRM 08 section 13.3

   if (!sem_check_context_clause(t, tab))
      return false;

   return true;
}

static bool sem_check_context_ref(tree_t t, nametab_t *tab)
{
   if (standard() >= STD_08) {
      tree_t unit = find_enclosing(tab, S_DESIGN_UNIT);

      if (unit != NULL && tree_kind(unit) == T_CONTEXT) {
         // LRM 08 section 13.3
         ident_t prefix = ident_until(tree_ident(t), '.');
         if (prefix == well_known(W_WORK))
            sem_error(t, "selected name in context declaration context "
                      "reference may not have WORK as a prefix");
      }
   }

   return true;
}

static bool sem_check_disconnect(tree_t t, nametab_t *tab)
{
   if (!tree_has_ref(t))
      return false;

   tree_t decl = tree_ref(t);
   if (class_of(decl) != C_SIGNAL || !is_guarded_signal(decl))
      sem_error(t, "signal name %s in disconnection specification must denote "
                "a guarded signal", istr(tree_ident(t)));

   type_t type = tree_type(t);
   if (!type_eq(tree_type(decl), type))
      sem_error(t, "type of declared signal %s does not match type %s in "
                "disconnection specification", type_pp(tree_type(decl)),
                type_pp(type));

   tree_t delay = tree_delay(t);
   type_t std_time = std_type(NULL, STD_TIME);
   if (!sem_check_type(delay, std_time, tab))
      sem_error(delay, "time expression in disconnection specification must "
                "have type %s but found %s", type_pp(std_time),
                type_pp(tree_type(delay)));

   if (!sem_globally_static(delay))
      sem_error(delay, "time expression in disconnection specificiation "
                "must be static");

   return true;
}

static bool sem_check_conv_func(tree_t t, nametab_t *tab)
{
   if (type_is_none(tree_type(t)))
      return false;
   else if (!tree_has_ref(t))
      return false;

   tree_t value = tree_value(t);
   if (!sem_check(value, tab))
      return false;

   tree_t decl = tree_ref(t);
   assert(tree_ports(decl) == 1);

   tree_t formal = tree_port(decl, 0);
   type_t ftype = tree_type(formal);
   if (!sem_check_type(value, ftype, tab))
      sem_error(value, "type of conversion function actual %s does not match "
                "formal %s type %s", type_pp2(tree_type(value), ftype),
                istr(tree_ident(formal)), type_pp2(ftype, tree_type(value)));

   return true;
}

static bool sem_check_concurrent(tree_t t, nametab_t *tab)
{
   if (tree_stmts(t) == 0)
      return false;   // Was parse error

   return sem_check(tree_stmt(t, 0), tab);
}

static bool sem_check_external_name(tree_t t, nametab_t *tab)
{
   const int nparts = tree_parts(t);
   for (int i = 0; i < nparts; i++) {
      tree_t pe = tree_part(t, i);
      switch (tree_subkind(pe)) {
      case PE_GENERATE:
         {
            tree_t value = tree_value(pe);
            if (!sem_globally_static(value))
               sem_error(value, "generate index must be a static expression");
         }
         break;
      case PE_RELATIVE:
         // Relative pathnames must have enclosing concurrent region
         if (find_enclosing(tab, S_CONCURRENT_BLOCK) == NULL) {
            diag_t *d = diag_new(DIAG_ERROR, tree_loc(t));
            diag_printf(d, "relative pathname has no enclosing "
                        "concurrent region");
            diag_lrm(d, STD_08, "8.7");
            diag_emit(d);
            return false;
         }
      }
   }

   // Cannot do any more checking until elaboration
   return true;
}

static port_mode_t sem_default_force_mode(tree_t target)
{
   // Rules for default force mode in LRM 08 section 10.5.2.1

   tree_t ref = name_to_ref(target);
   if (ref == NULL || !tree_has_ref(ref))
      return PORT_IN;

   tree_t decl = tree_ref(ref);
   const tree_kind_t dkind = tree_kind(decl);
   if (dkind == T_PORT_DECL || dkind == T_PARAM_DECL) {
      switch (tree_subkind(decl)) {
      case PORT_OUT:
      case PORT_INOUT:
      case PORT_BUFFER:
         return PORT_OUT;
      default:
         return PORT_IN;
      }
   }

   return PORT_IN;
}

static bool sem_check_force_target(tree_t target, port_mode_t mode,
                                   const char *what)
{
   tree_t decl = sem_check_lvalue(target);
   if (decl == NULL)
      sem_error(target, "target of simple %s assignment must be a "
                "signal name", what);

   switch (tree_kind(decl)) {
   case T_SIGNAL_DECL:
      break;

   case T_PORT_DECL:
   case T_PARAM_DECL:
      if (tree_class(decl) != C_SIGNAL) {
         diag_t *d = diag_new(DIAG_ERROR, tree_loc(target));
         diag_printf(d, "%s is not a valid target of simple %s assignment",
                     what, istr(tree_ident(decl)));
         diag_hint(d, tree_loc(target), "target of simple %s assignment", what);
         diag_hint(d, tree_loc(decl), "declared with class %s",
                   class_str(tree_class(decl)));
         diag_emit(d);
         return false;
      }
      else if (mode == PORT_OUT && tree_subkind(decl) == PORT_IN)
         sem_error(target, "force mode OUT may not be used with target "
                   "of mode IN");
      break;

      case T_EXTERNAL_NAME:
         if (tree_class(decl) != C_SIGNAL) {
            tree_t tail = tree_part(decl, tree_parts(decl) - 1);
            diag_t *d = diag_new(DIAG_ERROR, tree_loc(target));
            diag_printf(d, "external name %s is not a valid target of "
                        "simple %s assignment", istr(tree_ident(tail)), what);
            diag_hint(d, tree_loc(target), "target of signal assignment");
            diag_hint(d, tree_loc(decl), "declared with class %s",
                      class_str(tree_class(decl)));
            diag_emit(d);
            return false;
         }
         break;

   case T_VAR_DECL:
   case T_CONST_DECL:
      {
         diag_t *d = diag_new(DIAG_ERROR, tree_loc(target));
         diag_printf(d, "%s %s is not a valid target of simple %s assignment",
                     class_str(class_of(decl)), istr(tree_ident(decl)), what);
         diag_hint(d, tree_loc(target), "target of simple %s assignment", what);
         diag_hint(d, tree_loc(decl), "declared as %s",
                   class_str(class_of(decl)));
         diag_emit(d);
         return false;
      }

   default:
      sem_error(target, "invalid target of simple %s assignment", what);
   }

   return true;
}

static bool sem_check_force(tree_t t, nametab_t *tab)
{
   tree_t target = tree_target(t);

   if (!sem_check(target, tab))
      return false;

   port_mode_t mode = tree_subkind(t);
   if (mode == PORT_INVALID)
      tree_set_subkind(t, (mode = sem_default_force_mode(target)));

   if (!sem_check_force_target(target, mode, "force"))
      return false;

   tree_t value = tree_value(t);
   if (!sem_check(value, tab))
      return false;

   type_t expect = tree_type(target);

   if (!sem_check_type(value, expect, tab))
      sem_error(t, "type of force expression %s does not match type of "
                "target %s", type_pp2(tree_type(value), expect),
                type_pp2(expect, tree_type(value)));

   return true;
}

static bool sem_check_release(tree_t t, nametab_t *tab)
{
   tree_t target = tree_target(t);

   if (!sem_check(target, tab))
      return false;

   port_mode_t mode = tree_subkind(t);
   if (mode == PORT_INVALID)
      tree_set_subkind(t, (mode = sem_default_force_mode(target)));

   if (!sem_check_force_target(target, mode, "release"))
      return false;

   return true;
}

static bool sem_check_prot_ref(tree_t t, nametab_t *tab)
{
   if (standard() >= STD_19)
      return true;   // Alias of private variable method

   // There are no legal ways this can appear here and should always
   // have been converted to a call
   assert(error_count() > 0);

   return false;
}

static bool sem_check_view_decl(tree_t t, nametab_t *tab)
{
   type_t type = tree_type(t);
   if (type_is_none(type))
      return false;

   assert(type_kind(type) == T_VIEW);

   type_t rtype = type_designated(type);
   if (!type_is_record(rtype)) {
      assert(error_count() > 0);   // Checked by parser
      return false;
   }
   else if (type_is_resolved(rtype))
      sem_error(t, "subtype indication of a mode view declaration "
                "must denote an unresolved record type");

   const int nfields = type_fields(rtype);

   LOCAL_BIT_MASK have;
   mask_init(&have, nfields);

   const int nelems = type_fields(type);
   for (int i = 0; i < nelems; i++) {
      tree_t e = type_field(type, i);
      assert(tree_kind(e) == T_VIEW_ELEMENT);

      if (!tree_has_ref(e))
         return false;

      tree_t f = tree_ref(e);
      assert(tree_kind(f) == T_FIELD_DECL);

      const int pos = tree_pos(f);
      if (mask_test(&have, pos))
         sem_error(e, "duplicate mode view element definition for field %s",
                   istr(tree_ident(e)));

      mask_set(&have, pos);

      switch (tree_subkind(e)) {
      case PORT_LINKAGE:
         sem_error(e, "element mode indication cannot have mode LINKAGE");

      case PORT_RECORD_VIEW:
      case PORT_ARRAY_VIEW:
         {
            tree_t name = tree_value(e);
            type_t type = tree_type(e);
            type_t view_type = tree_type(name);

            if (type_is_none(view_type))
               return false;

            if (type_kind(view_type) != T_VIEW)
               sem_error(name, "name in element mode view indication of field "
                         "%s does not denote a mode view", istr(tree_ident(f)));

            type_t elem_type = type;
            if (tree_subkind(e) == PORT_ARRAY_VIEW) {
               if (!type_is_array(type))
                  sem_error(e, "field %s with array mode view indication has "
                            "non-array type %s", istr(tree_ident(f)),
                            type_pp(type));

               elem_type = type_elem(type);
            }

            if (!type_eq(elem_type, type_designated(view_type)))
               sem_error(e, "field %s subtype %s is not compatible with mode "
                         "view %s", istr(tree_ident(f)), type_pp(elem_type),
                         type_pp(view_type));
         }
         break;
      }
   }

   if (mask_popcount(&have) != nfields) {
      LOCAL_TEXT_BUF tb = tb_new();
      for (int i = 0, missing = 0; i < nfields; i++) {
         if (!mask_test(&have, i))
            tb_printf(tb, "%s%s", missing++ > 0 ? ", " : "",
                      istr(tree_ident(type_field(rtype, i))));
      }

      sem_error(t, "missing mode view element defintion for %s", tb_get(tb));
   }

   return true;
}

static bool sem_check_cond_value(tree_t t, nametab_t *tab)
{
   type_t type = tree_type(t);
   if (type_is_none(type))
      return false;

   type_t std_bool = std_type(NULL, STD_BOOLEAN);

   const int nconds = tree_conds(t);
   for (int i = 0; i < nconds; i++) {
      tree_t cond = tree_cond(t, i);
      assert(tree_kind(cond) == T_COND_EXPR);

      if (tree_has_value(cond)) {
         tree_t value = tree_value(cond);
         if (!sem_check(value, tab))
            return false;

         if (!sem_check_type(value, std_bool, tab))
            sem_error(value, "type of condition must be %s but have %s",
                      type_pp(std_bool), type_pp(tree_type(value)));
      }
      else
         assert(i == nconds - 1);

      if (tree_has_result(cond)) {
         tree_t result = tree_result(cond);
         if (!sem_check(result, tab))
            return false;

         if (!sem_check_type(result, type, tab))
            sem_error(result, "expected type of conditional expression to be "
                      "%s but is %s", type_pp(type),
                      type_pp(tree_type(result)));
      }
   }

   return true;
}

static bool sem_check_sequence(tree_t t, nametab_t *tab)
{
   const int ndecls = tree_decls(t);
   for (int i = 0; i < ndecls; i++) {
      // Mark all constant declarations as they need to be treated
      // specially when calculating longest static prefix
      tree_t d = tree_decl(t, i);
      if (tree_kind(d) == T_CONST_DECL)
         tree_set_flag(d, TREE_F_SEQ_BLOCK);
   }

   return true;
}

static bool sem_check_prot_decl(tree_t t, nametab_t *tab)
{
   const int ndecls = tree_decls(t);
   for (int i = 0; i < ndecls; i++) {
      tree_t d = tree_decl(t, i);
      if (tree_kind(d) == T_ALIAS) {
         // LRM 19 section 5.6.2: it is an error if an alias declared
         // within a protected type declaration denotes anything other
         // than a method of a protected type
         tree_t value = tree_value(d);
         if (tree_kind(value) != T_PROT_REF)
            sem_error(d, "an alias declared within a protected type "
                      "declaration must denote a protected type method");
      }
   }

   defer_check(tab, sem_missing_body_cb, t);
   return true;
}

static bool sem_check_inertial(tree_t t, nametab_t *tab)
{
   if (!sem_check(tree_value(t), tab))
      return false;

   return true;
}

bool sem_check(tree_t t, nametab_t *tab)
{
   switch (tree_kind(t)) {
   case T_ARCH:
      return sem_check_arch(t, tab);
   case T_PACKAGE:
      return sem_check_package(t, tab);
   case T_ENTITY:
      return sem_check_entity(t, tab);
   case T_TYPE_DECL:
      return sem_check_type_decl(t, tab);
   case T_SUBTYPE_DECL:
      return sem_check_subtype_decl(t, tab);
   case T_PORT_DECL:
      return sem_check_port_decl(t, tab);
   case T_PARAM_DECL:
      return sem_check_param_decl(t, tab);
   case T_GENERIC_DECL:
      return sem_check_generic_decl(t, tab);
   case T_SIGNAL_DECL:
      return sem_check_signal_decl(t, tab);
   case T_VAR_DECL:
      return sem_check_var_decl(t, tab);
   case T_CONST_DECL:
      return sem_check_const_decl(t, tab);
   case T_PROCESS:
      return sem_check_process(t, tab);
   case T_VAR_ASSIGN:
      return sem_check_var_assign(t, tab);
   case T_SIGNAL_ASSIGN:
      return sem_check_signal_assign(t, tab);
   case T_FCALL:
   case T_PROT_FCALL:
      return sem_check_fcall(t, tab);
   case T_LITERAL:
      return sem_check_literal(t);
   case T_STRING:
      return sem_check_string_literal(t);
   case T_REF:
      return sem_check_ref(t, tab);
   case T_WAIT:
      return sem_check_wait(t, tab);
   case T_ASSERT:
      return sem_check_assert(t, tab);
   case T_REPORT:
      return sem_check_report(t, tab);
   case T_QUALIFIED:
      return sem_check_qualified(t, tab);
   case T_FUNC_DECL:
      return sem_check_func_decl(t, tab);
   case T_AGGREGATE:
      return sem_check_aggregate(t, tab);
   case T_ATTR_REF:
      return sem_check_attr_ref(t, false, tab);
   case T_ARRAY_REF:
      return sem_check_array_ref(t, tab);
   case T_ARRAY_SLICE:
      return sem_check_array_slice(t, tab);
   case T_INSTANCE:
      return sem_check_instance(t, tab);
   case T_IF:
      return sem_check_if(t, tab);
   case T_NULL:
      return true;
   case T_PACK_BODY:
      return sem_check_pack_body(t, tab);
   case T_FUNC_BODY:
      return sem_check_func_body(t, tab);
   case T_RETURN:
      return sem_check_return(t, tab);
   case T_COND_RETURN:
      return sem_check_cond_return(t, tab);
   case T_COND_ASSIGN:
      return sem_check_cond_assign(t, tab);
   case T_WHILE:
      return sem_check_while(t, tab);
   case T_ALIAS:
      return sem_check_alias(t, tab);
   case T_FOR:
      return sem_check_for(t, tab);
   case T_PROC_DECL:
      return sem_check_proc_decl(t, tab);
   case T_PROC_BODY:
      return sem_check_proc_body(t, tab);
   case T_BLOCK:
      return sem_check_block(t, tab);
   case T_CASE:
   case T_SELECT:
      return sem_check_case(t, tab);
   case T_EXIT:
   case T_NEXT:
      return sem_check_loop_control(t, tab);
   case T_PCALL:
   case T_PROT_PCALL:
      return sem_check_pcall(t, tab);
   case T_ATTR_SPEC:
      return sem_check_attr_spec(t, tab);
   case T_ATTR_DECL:
      return sem_check_attr_decl(t);
   case T_COMPONENT:
      return sem_check_component(t, tab);
   case T_IF_GENERATE:
      return sem_check_if_generate(t, tab);
   case T_FOR_GENERATE:
      return sem_check_for_generate(t, tab);
   case T_CASE_GENERATE:
      return sem_check_case(t, tab);
   case T_OPEN:
      return sem_check_open(t);
   case T_FIELD_DECL:
      return sem_check_field_decl(t);
   case T_FILE_DECL:
      return sem_check_file_decl(t, tab);
   case T_NEW:
      return sem_check_new(t, tab);
   case T_ALL:
      return sem_check_all(t, tab);
   case T_RECORD_REF:
      return sem_check_record_ref(t, tab);
   case T_UNIT_DECL:
      return sem_check_unit_decl(t);
   case T_USE:
      return sem_check_use_clause(t, tab);
   case T_TYPE_CONV:
      return sem_check_conversion(t, tab);
   case T_SPEC:
      return sem_check_spec(t, tab);
   case T_BINDING:
      return sem_check_binding(t, tab);
   case T_LIBRARY:
      return sem_check_library_clause(t, tab);
   case T_CONFIGURATION:
      return sem_check_configuration(t, tab);
   case T_PROT_BODY:
      return sem_check_prot_body(t, tab);
   case T_CONTEXT:
      return sem_check_context_decl(t, tab);
   case T_CONTEXT_REF:
      return sem_check_context_ref(t, tab);
   case T_BLOCK_CONFIG:
      return sem_check_block_config(t, tab);
   case T_IMPLICIT_SIGNAL:
      return sem_check_implicit_signal(t, tab);
   case T_DISCONNECT:
      return sem_check_disconnect(t, tab);
   case T_GROUP:
   case T_GROUP_TEMPLATE:
   case T_BOX:
   case T_PSL:
   case T_LOOP:
      return true;
   case T_CONV_FUNC:
      return sem_check_conv_func(t, tab);
   case T_CONCURRENT:
      return sem_check_concurrent(t, tab);
   case T_PACK_INST:
      return sem_check_pack_inst(t, tab);
   case T_EXTERNAL_NAME:
      return sem_check_external_name(t, tab);
   case T_FORCE:
      return sem_check_force(t, tab);
   case T_RELEASE:
      return sem_check_release(t, tab);
   case T_PROT_REF:
      return sem_check_prot_ref(t, tab);
   case T_MATCH_CASE:
   case T_MATCH_SELECT:
      return sem_check_match_case(t, tab);
   case T_FUNC_INST:
   case T_PROC_INST:
      return sem_check_subprogram_inst(t, tab);
   case T_VIEW_DECL:
      return sem_check_view_decl(t, tab);
   case T_COND_VALUE:
      return sem_check_cond_value(t, tab);
   case T_SEQUENCE:
      return sem_check_sequence(t, tab);
   case T_PROT_DECL:
      return sem_check_prot_decl(t, tab);
   case T_INERTIAL:
      return sem_check_inertial(t, tab);
   default:
      sem_error(t, "cannot check %s", tree_kind_str(tree_kind(t)));
   }
}
