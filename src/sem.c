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

#include "phase.h"
#include "util.h"
#include "common.h"
#include "hash.h"
#include "loc.h"

#include <assert.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <ctype.h>
#include <errno.h>
#include <sys/stat.h>
#include <sys/types.h>

typedef struct scope       scope_t;
typedef struct loop_stack  loop_stack_t;

typedef bool (*static_fn_t)(tree_t t);

typedef enum {
   SCOPE_PACKAGE   = (1 << 0),
   SCOPE_FORMAL    = (1 << 1),
   SCOPE_PROTECTED = (1 << 2),
   SCOPE_CONTEXT   = (1 << 3)
} scope_flags_t;

struct scope {
   scope_t       *down;

   tree_t         subprog;
   wait_level_t   wait_level;
   impure_io_t    impure_io;
   tree_t         unit;

   // For design unit scopes
   ident_t        prefix;
   scope_flags_t  flags;
};

struct loop_stack {
   loop_stack_t *up;
   ident_t       name;
};

typedef struct {
   tree_t decl;
   bool   have;
   bool   partial;
} formal_map_t;

typedef tree_t (*get_fn_t)(tree_t);
typedef void (*set_fn_t)(tree_t, tree_t);
typedef tree_t (*get_nth_fn_t)(tree_t, unsigned);

static bool sem_check_array_ref(tree_t t);
static bool sem_locally_static(tree_t t);
static bool sem_globally_static(tree_t t);
static tree_t sem_check_lvalue(tree_t t);
static bool sem_check_same_type(tree_t left, tree_t right);
static bool sem_check_type(tree_t t, type_t expect);
static bool sem_static_name(tree_t t, static_fn_t check_fn);
static bool sem_check_range(tree_t r, type_t context);
static bool sem_check_attr_ref(tree_t t, bool allow_range);
static bool sem_check_array_dims(type_t type, type_t constraint);

static scope_t      *top_scope = NULL;
static loop_stack_t *loop_stack = NULL;

#define sem_error(t, ...) do {                        \
      error_at(t ? tree_loc(t) : NULL , __VA_ARGS__); \
      return false;                                   \
   } while (0)

static void scope_push(ident_t prefix)
{
   scope_t *s = xmalloc(sizeof(scope_t));
   s->prefix     = prefix;
   s->down       = top_scope;
   s->subprog    = (top_scope ? top_scope->subprog : NULL) ;
   s->flags      = (top_scope ? top_scope->flags : 0);
   s->unit       = (top_scope ? top_scope->unit : NULL);
   s->wait_level = WAITS_NO;
   s->impure_io  = 0;

   top_scope = s;
}

static void scope_pop(void)
{
   assert(top_scope != NULL);

   scope_t *s = top_scope;
   if (s->down != NULL && s->down->subprog == s->subprog) {
      s->down->wait_level |= s->wait_level;
      s->down->impure_io  |= s->down->impure_io;
   }

   top_scope = s->down;
   free(s);
}

static void loop_push(ident_t name)
{
   loop_stack_t *ls = xmalloc(sizeof(loop_stack_t));
   ls->up   = loop_stack;
   ls->name = name;

   loop_stack = ls;
}

static void loop_pop(void)
{
   loop_stack_t *tmp = loop_stack->up;
   free(loop_stack);
   loop_stack = tmp;
}

static tree_t sem_int_lit(type_t type, int64_t i)
{
   tree_t f = tree_new(T_LITERAL);
   tree_set_subkind(f, L_INT);
   tree_set_ival(f, i);
   tree_set_type(f, type);

   return f;
}

static bool sem_check_resolution(type_t type, tree_t res)
{
   // Resolution functions are described in LRM 93 section 2.4

   if (tree_kind(res) == T_AGGREGATE) {
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

   if (type_kind(ftype) != T_FUNC)
      sem_error(res, "resolution function name %s is not a function",
                istr(tree_ident(res)));

   // Must take a single parameter of array of base type

   if (type_params(ftype) != 1)
      sem_error(res, "resolution function must have a single argument");

   type_t param = type_param(ftype, 0);
   if (type_kind(param) != T_UARRAY)
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

static bool sem_check_subtype(tree_t decl, type_t type)
{
   if (type_kind(type) != T_SUBTYPE)
      return true;

   type_t base = type_base(type);
   if (type_is_none(base))
      return false;

   if (type_is_protected(base))
      sem_error(decl, "subtypes may not have protected base types");

   if (type_has_constraint(type)) {
      tree_t constraint = type_constraint(type);

      if (type_is_access(base))
         base = type_access(base);

      const constraint_kind_t consk = tree_subkind(constraint);
      if (consk == C_RANGE && !type_is_scalar(base))
         sem_error(constraint, "range constraint cannot be used with "
                   "non-scalar type %s", type_pp(base));
      else if (consk == C_INDEX && !type_is_array(base))
         sem_error(constraint, "index constraint cannot be used with "
                   "non-array type %s", type_pp(base));

      if (type_kind(base) == T_CARRAY)
         sem_error(decl, "may not change constraints of a constrained array");

      if (type_is_record(base))
         sem_error(decl, "record subtype may not have constraints");

      const int ndims_base = type_is_array(base) ? dimension_of(base) : 1;
      const int ndims = tree_ranges(constraint);

      if (ndims != ndims_base)
         sem_error(decl, "expected %d constraints for type %s but found %d",
                   ndims_base, type_pp(base), ndims);

      for (int i = 0; i < ndims; i++) {
         tree_t r = tree_range(constraint, i);
         if (!sem_check_range(r, index_type_of(base, i)))
            return false;
      }
   }

   if (type_has_resolution(type)) {
      if (!sem_check_resolution(type_base(type), type_resolution(type)))
         return false;
   }

   return true;
}

static bool sem_check_range(tree_t r, type_t expect)
{
   switch (tree_subkind(r)) {
   case RANGE_EXPR:
      {
         tree_t expr = tree_value(r);
         assert(tree_kind(expr) == T_ATTR_REF);

         if (!sem_check_attr_ref(expr, true))
            return false;

         if (expect && !sem_check_type(expr, expect))
            sem_error(expr, "expected type of range bound to be %s but is %s",
                      type_pp(expect), type_pp(tree_type(expr)));
      }
      break;

   case RANGE_TO:
   case RANGE_DOWNTO:
      {
         tree_t left = tree_left(r);
         if (!sem_check(left))
            return false;

         tree_t right = tree_right(r);
         if (!sem_check(right))
            return false;

         if (!sem_check_same_type(left, right))
            sem_error(right, "type mismatch in range: left is %s, right is %s",
                      type_pp(tree_type(left)), type_pp(tree_type(right)));

         if (expect != NULL) {
            if (!sem_check_type(left, expect)) {
               sem_error(left, "expected type of left bound to be %s but is %s",
                         type_pp(expect), type_pp(tree_type(left)));
            }
            else if (!sem_check_type(right, expect)) {
               sem_error(right, "expected type of right bound to be %s but is "
                         "%s", type_pp(expect), type_pp(tree_type(right)));
            }
         }
      }
      break;
   }

   return true;
}

static bool sem_check_discrete_range(tree_t r, type_t expect)
{
   if (!sem_check_range(r, expect))
      return false;

   type_t type = tree_type(r);
   if (type_is_none(type))
      return false;

   if (!type_is_discrete(type))
      sem_error(r, "type of range bounds %s is not discrete", type_pp(type));

   const range_kind_t rkind = tree_subkind(r);
   if (rkind == RANGE_ERROR)
      return false;

   if (expect == NULL && rkind != RANGE_EXPR) {
      tree_t left  = tree_left(r);
      tree_t right = tree_right(r);

      type_t left_type  = tree_type(left);
      type_t right_type = tree_type(right);

      if (type_is_universal(left_type) && type_is_universal(right_type)) {
         tree_kind_t lkind = tree_kind(left);
         tree_kind_t rkind = tree_kind(right);

         // See LRM 93 section 3.2.1.1
         // Later LRMs relax the wording here
         const bool invalid =
            standard() < STD_00
            && !(relax_rules() & RELAX_UNIVERSAL_BOUND)
            && lkind != T_LITERAL && lkind != T_ATTR_REF
            && rkind != T_LITERAL && rkind != T_ATTR_REF;

         if (invalid)
            sem_error(left, "universal integer bound must be "
                      "numeric literal or attribute");

         type_t std_int = std_type(NULL, "INTEGER");
         if (!sem_check_type(left, std_int))
            sem_error(left, "universal bound not convertible to INTEGER");
         if (!sem_check_type(right, std_int))
            sem_error(right, "universal bound not convertible to INTEGER");
      }
   }

   return true;
}

static bool sem_check_use_clause(tree_t c)
{
   if (top_scope->flags & SCOPE_CONTEXT) {
      // LRM 08 section 13.3
      ident_t prefix = ident_until(tree_ident(c), '.');
      if (prefix == work_i)
         sem_error(c, "selected name in context declaration use clause may not "
                   "have WORK as a prefix");
   }

   return true;
}

static bool sem_check_library_clause(tree_t t)
{
   ident_t name = tree_ident(t);
   if (name == work_i && (top_scope->flags & SCOPE_CONTEXT)) {
      // LRM 08 section 13.3
      sem_error(t, "library clause in a context declaration may not have "
                "logical library name WORK");
   }

   if (lib_find(name, false) == NULL)
      return false;

   return true;
}

static bool sem_check_context_clause(tree_t t)
{
   // Ignore the implicit WORK and STD with context declarations
   const int ignore = tree_kind(t) == T_CONTEXT ? 2 : 0;

   bool ok = true;
   const int ncontexts = tree_contexts(t);
   for (int n = ignore; n < ncontexts; n++)
      ok = sem_check(tree_context(t, n)) && ok;

   return ok;
}

static bool sem_readable(tree_t t)
{
   // Outputs may be read in LRM 08
   if (standard() >= STD_08)
      return true;

   switch (tree_kind(t)) {
   case T_REF:
      {
         tree_t decl = tree_ref(t);
         if ((tree_kind(decl) == T_PORT_DECL)
             && (tree_subkind(decl) == PORT_OUT)
             && !(top_scope->flags & SCOPE_FORMAL))
            sem_error(t, "cannot read output port %s",
                      istr(tree_ident(t)));

         return true;
      }

   case T_ARRAY_REF:
   case T_ARRAY_SLICE:
      return sem_readable(tree_value(t));

   default:
      return true;
   }
}

static bool sem_check_array_dims(type_t type, type_t constraint)
{
   const int ndims = dimension_of(type);
   for (int i = 0; i < ndims; i++) {
      tree_t r = range_of(type, i);

      type_t index_type = NULL;
      if (constraint != NULL && i < dimension_of(constraint))
         index_type = index_type_of(constraint, i);

      if (!sem_check_discrete_range(r, index_type))
         return false;

      if (index_type == NULL)
         index_type = tree_type(r);

      if (!sem_check_type(r, index_type))
         sem_error(r, "type of bound %s does not match type of index %s",
                   type_pp(tree_type(r)),
                   type_pp(index_type));
   }

   return true;
}

static bool sem_check_type(tree_t t, type_t expect)
{
   type_t actual = tree_type(t);

   if (type_eq(actual, expect))
      return true;

   // LRM 08 section 9.3.6 rules for implicit conversion
   if (type_is_convertible(actual, expect)) {
      tree_set_type(t, expect);
      return true;
   }

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

   if (type_is_convertible(left_type, right_type)) {
      tree_set_type(left, right_type);
      return true;
   }

   if (type_is_convertible(right_type, left_type)) {
      tree_set_type(right, left_type);
      return true;
   }

   // Supress cascading errors
   if (type_kind(left_type) == T_NONE || type_kind(right_type) == T_NONE)
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

static bool sem_check_type_decl(tree_t t)
{
   type_t type = tree_type(t);

   // Nothing more to do for incomplete types
   if (type_kind(type) == T_INCOMPLETE)
      return true;

   type_kind_t kind = type_kind(type);

   switch (kind) {
   case T_CARRAY:
   case T_UARRAY:
      {
         type_t elem_type = type_elem(type);

         if (!sem_check_subtype(t, elem_type))
            return false;

         if (type_is_unconstrained(elem_type))
            sem_error(t, "array %s cannot have unconstrained element type",
                      istr(tree_ident(t)));

         if (type_is_file(elem_type))
            sem_error(t, "array %s cannot have element of file type",
                      istr(tree_ident(t)));

         if (type_is_protected(elem_type))
            sem_error(t, "array %s cannot have element of protected type",
                      istr(tree_ident(t)));
      }
      break;

   default:
      break;
   }

   switch (kind) {
   case T_CARRAY:
      {
         if (!sem_check_array_dims(type, NULL))
            return false;

         return true;
      }

   case T_UARRAY:
      {
         const int nindex = type_index_constrs(type);
         for (int i = 0; i < nindex; i++) {
            type_t index_type = type_index_constr(type, i);
            if (!type_is_discrete(index_type))
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
            if (!sem_check(u))
               return false;

            tree_t value = tree_value(u);

            // LRM 08 section 5.2.4.1: the abstract literal portion
            // shall be an integer literal
            if (tree_ival(value) == 0 && tree_dval(value) != 0)
               sem_error(value, "the abstract literal portion of a secondary "
                         "unit declaration must be an integer literal");

            if (i > 0 && !sem_check_type(value, type))
               sem_error(value, "secondary unit %s must have type %s",
                         istr(tree_ident(u)), type_pp(type));
         }
      }

      // Fall-through
   case T_INTEGER:
   case T_REAL:
      {
         tree_t r = type_dim(type, 0);

         if (!sem_check_range(r, NULL))
            return false;

         // Standard specifies type of 'LEFT and 'RIGHT are same
         // as the declared type
         switch (tree_subkind(r)) {
         case RANGE_TO:
         case RANGE_DOWNTO:
            tree_set_type(tree_left(r), type);
            tree_set_type(tree_right(r), type);
            break;
         case RANGE_EXPR:
            tree_set_type(tree_value(r), type);
            break;
         }

         tree_set_type(r, type);
         return true;
      }

   case T_SUBTYPE:
      return sem_check_subtype(t, type);

   case T_RECORD:
      {
         const int nfields = type_fields(type);
         for (int i = 0; i < nfields; i++) {
            tree_t f = type_field(type, i);

            if (!sem_check(f))
               return false;

            // Each field name must be distinct
            ident_t f_name = tree_ident(f);
            for (int j = 0; j < i; j++) {
               if (f_name == tree_ident(type_field(type, j)))
                  sem_error(f, "duplicate field name %s", istr(f_name));
            }

            type_t f_type = tree_type(f);

            if (!sem_check_subtype(f, f_type))
               return false;

            // Recursive record types are not allowed
            if (type_eq(type, f_type))
               sem_error(f, "recursive record types are not allowed");

            // Element types may not be unconstrained
            if (type_is_unconstrained(f_type))
               sem_error(f, "field %s with unconstrained array type "
                         "is not allowed", istr(f_name));

             if (type_is_file(f_type))
                sem_error(f, "record field %s cannot be of file type",
                          istr(f_name));

             if (type_is_protected(f_type))
                sem_error(f, "record field %s cannot be of protected type",
                          istr(f_name));
         }

         return true;
      }

   case T_FILE:
      // Rules for file types are in LRM 93 section 3.4
      {
         type_t f = type_file(type);

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
         return true;
      }

   case T_PROTECTED:
      // Rules for protected types are in LRM 02 section 3.5
      {
         scope_push(tree_ident(t));
         top_scope->flags |= SCOPE_PROTECTED;

         bool ok = true;
         const int ndecls = type_decls(type);
         for (int i = 0; i < ndecls; i++) {
            tree_t d = type_decl(type, i);
            ok = sem_check(d) && ok;
         }

         scope_pop();
         return ok;
      }

   default:
      return true;
   }
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

static bool sem_check_decl(tree_t t)
{
   type_t type = tree_type(t);

   if (!sem_check_subtype(t, type))
      return false;

   tree_kind_t kind = tree_kind(t);

   if (!tree_has_value(t) && kind == T_CONST_DECL
       && !(top_scope->flags & SCOPE_PACKAGE))
      sem_error(t, "deferred constant declarations are only permitted "
                "in packages");

   if (type_is_unconstrained(type) && (kind != T_CONST_DECL))
      sem_error(t, "type %s is unconstrained", type_pp(type));
   else if (type_is_incomplete(type))
      sem_error(t, "type %s is incomplete", type_pp(type));

   switch (kind) {
   case T_CONST_DECL:
      if (!sem_no_access_file_or_protected(t, type, "constants"))
         return false;
      break;
   case T_SIGNAL_DECL:
      if (!sem_no_access_file_or_protected(t, type, "signals"))
         return false;
      break;
   default:
      break;
   }

   const bool needs_default_value =
      !tree_has_value(t) && (kind != T_PORT_DECL) && (kind != T_CONST_DECL)
      && (type_kind(type) != T_PROTECTED);

   if (needs_default_value)
      tree_set_value(t, make_default_value(type, tree_loc(t)));
   else if (tree_has_value(t)) {
      if (type_kind(type) == T_PROTECTED)
         sem_error(t, "variable %s with protected type may not have an "
                   "initial value", istr(tree_ident(t)));

      tree_t value = tree_value(t);
      if (!sem_check(value))
         return false;

      if (!sem_check_type(value, type))
         sem_error(value, "type of initial value %s does not match type "
                   "of declaration %s", type_pp2(tree_type(value), type),
                   type_pp2(type, tree_type(value)));
   }

   if (kind == T_PORT_DECL && tree_class(t) == C_DEFAULT)
      tree_set_class(t, C_SIGNAL);

   // From VHDL-2000 onwards shared variables must be protected types
   if (standard() >= STD_00 && (tree_flags(t) & TREE_F_SHARED)) {
      if (type_kind(type) != T_PROTECTED)
         sem_error(t, "shared variable %s must have protected type",
                   istr(tree_ident(t)));
   }

   // Cannot optimise out LAST_VALUE for package signals
   if (kind == T_SIGNAL_DECL && (top_scope->flags & SCOPE_PACKAGE))
      tree_set_flag(t, TREE_F_LAST_VALUE);

   return true;
}

static bool sem_check_port_decl(tree_t t)
{
   type_t type = tree_type(t);

   if (!sem_check_subtype(t, type))
      return false;

   if (tree_has_value(t)) {
      tree_t value = tree_value(t);
      if (!sem_check(value))
         return false;

      port_mode_t mode = tree_subkind(t);
      if (mode == PORT_LINKAGE)
         sem_error(t, "port with mode LINKAGE can not have a default value");

      if (!sem_check_type(value, type))
         sem_error(value, "type of default value %s does not match type "
                   "of declaration %s", type_pp(tree_type(value)),
                   type_pp(type));
   }

   return true;
}

static bool sem_check_field_decl(tree_t t)
{
   return true;
}

static bool sem_check_unit_decl(tree_t t)
{
   return true;
}

static bool sem_check_alias(tree_t t)
{
   // Rules for aliases are given in LRM 93 section 4.3.3

   tree_t value = tree_value(t);
   type_t type = tree_type(t);

   if (type_is_none(type))
      return false;
   else if (type_is_subprogram(type)) {
      // Alias of subprogram or enumeration literal
      // Rules for matching signatures are in LRM 93 section 2.3.2
      assert(tree_kind(value) == T_REF);
   }
   else if (tree_kind(value) == T_REF
            && tree_kind(tree_ref(value)) == T_TYPE_DECL) {
      // Alias of type
   }
   else {
      // Alias of object
      if (!sem_check(value))
         return false;

      if (!sem_static_name(value, sem_globally_static))
         sem_error(value, "aliased name is not static");

      if (!sem_check_subtype(t, type))
         return false;

      if (!sem_check_type(value, type))
         sem_error(t, "type of aliased object %s does not match expected "
                   "type %s", type_pp2(tree_type(value), type),
                   type_pp2(type, tree_type(value)));
   }

   return true;
}

static bool sem_check_interface_class(tree_t port)
{
   // See LRM 93 section 3.3 for restrictions

   const type_t type = tree_type(port);
   const type_kind_t kind = type_base_kind(type);
   const class_t class = tree_class(port);
   const port_mode_t mode = tree_subkind(port);

   if (tree_has_value(port)) {
       if (class == C_SIGNAL)
          sem_error(port, "parameter of class SIGNAL can not have a default value");

       if (class == C_VARIABLE) {
          if (mode == PORT_OUT || mode == PORT_INOUT)
             sem_error(port, "parameter of class VARIABLE with mode OUT or INOUT can not have a default value");
       }

       tree_t value = tree_value(port);
       if (!sem_globally_static(value))
          sem_error(value, "default value must be a static expression");

       if (kind == T_PROTECTED)
          sem_error(port, "parameter with protected type can not have a default value");
   }

   if (kind == T_FILE && class != C_FILE)
      sem_error(port, "object %s with file type must have class FILE",
                istr(tree_ident(port)));

   if (kind != T_FILE && class == C_FILE)
      sem_error(port, "object %s with class FILE must have file type",
                istr(tree_ident(port)));

   if ((kind == T_ACCESS || kind == T_PROTECTED) && class != C_VARIABLE)
      sem_error(port, "object %s with %s type must have class VARIABLE",
                istr(tree_ident(port)),
                kind == T_ACCESS ? "access" : "protected");

   if (sem_has_access(type) && class != C_VARIABLE)
      sem_error(port, "object %s with type containing an access type must have class VARIABLE",
                istr(tree_ident(port)));

   if (class == C_CONSTANT && mode != PORT_IN)
      sem_error(port, "parameter of class CONSTANT must have mode IN");

   return true;
}

static bool sem_check_func_ports(tree_t t)
{
   const int nports = tree_ports(t);
   for (int i = 0; i < nports; i++) {
      tree_t p = tree_port(t, i);
      if (tree_subkind(p) != PORT_IN)
         sem_error(p, "function arguments must have mode IN");

      // See LRM 93 section 2.1.1 for default class
      if (tree_class(p) == C_VARIABLE)
         sem_error(p, "function arguments may not have VARIABLE class");

      if (!sem_check(p))
         return false;

      if (!sem_check_interface_class(p))
         return false;
   }

   return true;
}

static bool sem_check_stmts(tree_t t, tree_t (*get_stmt)(tree_t, unsigned),
                            int nstmts)
{
   bool ok = true;
   for (int i = 0; i < nstmts; i++)
      ok = sem_check(get_stmt(t, i)) && ok;

   return ok;
}

static bool sem_check_func_decl(tree_t t)
{
   if (tree_flags(t) & TREE_F_PREDEFINED)
      return true;

   if (!sem_check_func_ports(t))
      return false;

   return true;
}

static bool sem_check_func_body(tree_t t)
{
   if (!sem_check_func_ports(t))
      return false;

   scope_push(NULL);

   scope_push(NULL);
   top_scope->subprog = t;

   bool ok = true;
   const int ndecls = tree_decls(t);
   for (int i = 0; i < ndecls; i++) {
      tree_t d = tree_decl(t, i);
      ok = sem_check(d) && ok;

      if (tree_kind(d) == T_USE)
         tree_add_context(top_scope->unit, d);
   }

   ok = ok && sem_check_stmts(t, tree_stmt, tree_stmts(t));

   scope_pop();
   scope_pop();

   unsigned nret = tree_visit_only(t, NULL, NULL, T_RETURN);
   if (nret == 0)
      sem_error(t, "function must contain a return statement");

   return ok;
}

static bool sem_check_proc_ports(tree_t t)
{
   const int nports = tree_ports(t);
   for (unsigned i = 0; i < nports; i++) {
      tree_t p = tree_port(t, i);

      switch (tree_subkind(p)) {
      case PORT_BUFFER:
         sem_error(p, "procedure arguments may not have mode BUFFER");
         break;
      case PORT_LINKAGE:
         sem_error(p, "procedure arguments may not have mode LINKAGE");
         break;
      default:
         break;
      }

      if (!sem_check(p))
         return false;

      if (!sem_check_interface_class(p))
         return false;
   }

   return true;
}

static bool sem_check_proc_decl(tree_t t)
{
   if (tree_flags(t) & TREE_F_PREDEFINED)
      return true;

   if (!sem_check_proc_ports(t))
      return false;

   if (tree_flags(t) & TREE_F_FOREIGN)
      tree_add_attr_int(t, wait_level_i, WAITS_NO);

   return true;
}

static bool sem_check_proc_body(tree_t t)
{
   if (!sem_check_proc_ports(t))
      return false;

   scope_push(NULL);

   scope_push(NULL);
   top_scope->subprog = t;

   bool ok = true;
   const int ndecls = tree_decls(t);
   for (int i = 0; i < ndecls; i++) {
      tree_t d = tree_decl(t, i);
      ok = sem_check(d) && ok;

      if (tree_kind(d) == T_USE)
         tree_add_context(top_scope->unit, d);
   }

   ok = ok && sem_check_stmts(t, tree_stmt, tree_stmts(t));

   tree_add_attr_int(t, wait_level_i, top_scope->wait_level);
   if (top_scope->impure_io)
      tree_add_attr_int(t, impure_io_i, top_scope->impure_io);

#if 0
   tree_t proto = sem_check_duplicate(t, T_PROC_DECL);
   if (proto != NULL) {
      tree_add_attr_int(proto, wait_level_i, top_scope->wait_level);
      if (top_scope->impure_io)
         tree_add_attr_int(t, impure_io_i, top_scope->impure_io);
   }
#endif

   scope_pop();
   scope_pop();
   return ok;
}

static bool sem_check_sensitivity(tree_t t)
{
   const int ntriggers = tree_triggers(t);
   for (int i = 0; i < ntriggers; i++) {
      tree_t r = tree_trigger(t, i);
      if (tree_kind(r) == T_ALL)
         continue;
      else if (!sem_check(r) || !sem_readable(r))
         return false;

      // Can only reference signals in sensitivity list
      tree_t decl = sem_check_lvalue(r);
      if (decl == NULL)
         sem_error(r, "not a sutiable l-value");

      switch (tree_kind(decl)) {
      case T_SIGNAL_DECL:
      case T_PORT_DECL:
         break;
      default:
         sem_error(r, "name %s in sensitivity list is not a signal",
                   istr(tree_ident(decl)));
      }

      if (!sem_static_name(r, sem_globally_static))
         sem_error(r, "name in sensitivity list is not static");
   }

   return true;
}

static void sem_check_static_elab_fn(tree_t t, void *context)
{
   tree_t decl = tree_ref(t);
   if (tree_kind(decl) == T_SIGNAL_DECL)
      error_at(tree_loc(t), "cannot reference signal %s during static "
               "elaboration", istr(tree_ident(decl)));
}

static void sem_check_static_elab(tree_t t)
{
   // LRM 93 12.3 forbirds references to signals before the design has been
   // elaborated

   switch (tree_kind(t)) {
   case T_VAR_DECL:
   case T_CONST_DECL:
   case T_SIGNAL_DECL:
      {
         type_t type = tree_type(t);
         if (type_is_scalar(type) && !type_is_none(type))
            sem_check_static_elab(range_of(type, 0));
         else if (type_is_array(type) && !type_is_unconstrained(type)) {
            const int ndims = dimension_of(type);
            for (int i = 0; i < ndims; i++)
               sem_check_static_elab(range_of(type, i));
         }

         if (tree_has_value(t))
            sem_check_static_elab(tree_value(t));
      }
      break;

   case T_REF:
   case T_ARRAY_REF:
   case T_ARRAY_SLICE:
   case T_RECORD_REF:
   case T_ATTR_REF:
   case T_FCALL:
      if (!sem_globally_static(t))
         tree_visit_only(t, sem_check_static_elab_fn, NULL, T_REF);
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

   default:
      break;
   }
}

static bool sem_check_process(tree_t t)
{
   const bool synthetic_name = !!(tree_flags(t) & TREE_F_SYNTHETIC_NAME);
   scope_push(synthetic_name ? NULL : tree_ident(t));

   bool ok = sem_check_sensitivity(t);

   const int ndecls = tree_decls(t);
   for (int n = 0; n < ndecls; n++) {
      tree_t d = tree_decl(t, n);

      if ((ok = sem_check(d) && ok))
         sem_check_static_elab(d);

      if (tree_kind(d) == T_USE)
         tree_add_context(top_scope->unit, d);
   }

   ok = ok && sem_check_stmts(t, tree_stmt, tree_stmts(t));

   scope_pop();

   if (tree_triggers(t) > 0) {
      // No wait statements allowed in process with sensitivity list
      if (tree_visit_only(t, NULL, NULL, T_WAIT) > 0)
         sem_error(t, "wait statement not allowed in process "
                   "with sensitvity list");
   }

   return ok;
}

static bool sem_check_package(tree_t t)
{
   assert(top_scope == NULL);
   scope_push(NULL);
   top_scope->unit = t;

   const int ndecls = tree_decls(t);

   bool ok = sem_check_context_clause(t);
   if (ok) {
      scope_push(NULL);

      // Allow constant declarations without initial values
      top_scope->flags |= SCOPE_PACKAGE;

      for (int n = 0; n < ndecls; n++) {
         tree_t decl = tree_decl(t, n);
         if (!sem_check(decl))
            ok = false;
      }
      scope_pop();
   }

   scope_pop();

   // Subprogram bodies are not allowed in package specification
   for (int i = 0; i < ndecls; i++) {
     tree_t d = tree_decl(t, i);
     tree_kind_t kind = tree_kind(d);
     if ((kind == T_FUNC_BODY) || (kind == T_PROC_BODY))
       sem_error(d, "subprogram body is not allowed in package specification");
   }

   return ok;
}

static bool sem_check_missing_body(tree_t body, tree_t spec)
{
   // Check for any subprogram declarations or protected types without bodies
   bool ok = true;
   const int ndecls = tree_decls(spec);
   for (int i = 0; i < ndecls; i++) {
      tree_t d = tree_decl(spec, i);
      tree_kind_t dkind = tree_kind(d);

      if (dkind == T_FUNC_DECL || dkind == T_PROC_DECL
          || (dkind == T_TYPE_DECL && type_is_protected(tree_type(d)))) {
         type_t dtype = tree_type(d);

         bool found = false;
         const int nbody_decls = tree_decls(body);
         const int start = (body == spec ? i + 1 : 0);
         for (int j = start; !found && (j < nbody_decls); j++) {
            tree_t b = tree_decl(body, j);
            tree_kind_t bkind = tree_kind(b);
            if (bkind == T_FUNC_BODY || bkind == T_PROC_BODY
                || bkind == T_PROT_BODY) {
               if (type_eq(dtype, tree_type(b)))
                  found = true;
            }
         }

         if (!found && !(dkind != T_TYPE_DECL
                         && ((tree_flags(d) & TREE_F_PREDEFINED)
                             || (tree_flags(d) & TREE_F_FOREIGN)))
             && opt_get_int("missing-body")) {
            warn_at(tree_loc(d), "missing body for %s %s",
                    (dkind == T_TYPE_DECL) ? "protected type"
                    : (dkind == T_PROC_DECL ? "procedure" : "function"),
                    type_pp(dtype));
         }
      }
   }

   if (body != spec)
      ok = sem_check_missing_body(body, body) && ok;

   return ok;
}

static bool sem_check_pack_body(tree_t t)
{
   // Look up package declaration
   ident_t pname = ident_until(tree_ident(t), '-');
   tree_t pack = lib_get_check_stale(lib_work(), pname);
   if (pack == NULL)
      sem_error(t, "missing declaration for package %s",
                istr(ident_rfrom(pname, '.')));

   if (tree_kind(pack) != T_PACKAGE)
      sem_error(t, "unit %s is not a package", istr(tree_ident(pack)));

   assert(top_scope == NULL);
   scope_push(NULL);
   top_scope->unit = t;

   bool ok = sem_check_context_clause(pack) && sem_check_context_clause(t);

   scope_push(NULL);

   if (ok) {
      const int ndecls = tree_decls(t);
      for (int n = 0; n < ndecls; n++) {
         tree_t decl = tree_decl(t, n);
         ok = sem_check(decl) && ok;
      }
   }

   ok = ok && sem_check_missing_body(t, pack) && sem_check_missing_body(t, t);

   scope_pop();
   scope_pop();

   // Check for any deferred constants which were not given values
   const int ndecls = tree_decls(pack);
   for (int i = 0; i < ndecls; i++) {
      tree_t d = tree_decl(pack, i);
      if ((tree_kind(d) == T_CONST_DECL) && !tree_has_value(d)) {
         tree_t d2 = search_decls(t, tree_ident(d), 0);
         if (d2 == NULL || !tree_has_value(d2))
            sem_error(d, "deferred constant %s was not given a value in the "
                      "package body", istr(tree_ident(d)));
      }
   }

   if (!ok)
      return false;

   // Mark the package declaration as dirty in the library so any changes
   // to declaration attributes are saved
   lib_put(lib_work(), pack);

   return ok;
}

static bool sem_check_generics(tree_t t)
{
   bool ok = true;

   const int ngenerics = tree_generics(t);
   for (int n = 0; n < ngenerics; n++) {
      tree_t g = tree_generic(t, n);

      switch (tree_class(g)) {
      case C_DEFAULT:
         tree_set_class(g, C_CONSTANT);
         break;
      case C_CONSTANT:
         break;
      default:
         sem_error(g, "invalid object class for generic");
      }

      tree_add_attr_int(g, elab_copy_i, 1);

      ok = sem_check(g) && ok;

      ok = sem_no_access_file_or_protected(g, tree_type(g), "generics") && ok;
   }

   return ok;
}

static bool sem_check_ports(tree_t t)
{
   bool ok = true;

   const int nports = tree_ports(t);
   for (int n = 0; n < nports; n++) {
      tree_t p = tree_port(t, n);

      switch (tree_class(p)) {
      case C_DEFAULT:
         tree_set_class(p, C_SIGNAL);
         break;
      case C_SIGNAL:
         break;
      default:
         sem_error(p, "invalid object class for port");
      }

      tree_add_attr_int(p, elab_copy_i, 1);

      ok = sem_check(p) && ok;

      ok = sem_no_access_file_or_protected(p, tree_type(p), "ports") && ok;
   }

   return ok;
}

static bool sem_check_component(tree_t t)
{
   scope_push(NULL);

   bool ok = sem_check_generics(t) && sem_check_ports(t);

   scope_pop();

   return ok;
}

static bool sem_check_entity(tree_t t)
{
   assert(top_scope == NULL);
   scope_push(NULL);
   top_scope->unit = t;

   bool ok = sem_check_context_clause(t);

   scope_push(NULL);

   ok = ok && sem_check_generics(t) && sem_check_ports(t);

   if (ok) {
      const int ndecls = tree_decls(t);
      const int nstmts = tree_stmts(t);

      for (int n = 0; n < ndecls; n++) {
         tree_t d = tree_decl(t, n);
         ok = sem_check(d) && ok;

         if (tree_kind(d) == T_USE)
            tree_add_context(top_scope->unit, d);
      }

      ok = ok && sem_check_stmts(t, tree_stmt, nstmts);
   }

   scope_pop();

   scope_pop();

   return ok;
}

static bool sem_check_arch(tree_t t)
{
   // Find the corresponding entity
   tree_t e = lib_get_check_stale(lib_work(),
                                  ident_prefix(lib_name(lib_work()),
                                               tree_ident2(t), '.'));
   if (e == NULL)
      sem_error(t, "missing declaration for entity %s",
                istr(tree_ident2(t)));

   if (tree_kind(e) != T_ENTITY)
      sem_error(t, "unit %s is not an entity", istr(tree_ident(e)));

   tree_set_ref(t, e);

   assert(top_scope == NULL);
   scope_push(NULL);
   top_scope->unit = t;

   // Make all port and generic declarations available in this scope

   bool ok = sem_check_context_clause(e) && sem_check_context_clause(t);

   scope_push(NULL);

   scope_push(NULL);

   // Now check the architecture itself

   if (ok) {
      const int ndecls = tree_decls(t);
      for (int n = 0; n < ndecls; n++) {
         tree_t d = tree_decl(t, n);
         if ((ok = sem_check(d) && ok))
            sem_check_static_elab(d);

         if (tree_kind(d) == T_USE)
            tree_add_context(t, d);
      }
   }

   ok = ok && sem_check_missing_body(t, t);

   ok = ok && sem_check_stmts(t, tree_stmt, tree_stmts(t));

   scope_pop();
   scope_pop();
   scope_pop();

   lib_put(lib_work(), t);

   return ok;
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
      return t;
   default:
      return NULL;
   }
}

static bool sem_check_variable_target(tree_t target)
{
   if (tree_kind(target) == T_AGGREGATE) {
      // Rules for aggregate variable targets in LRM 93 section 8.5

      if (!type_is_composite(tree_type(target)))
         sem_error(target, "aggregate target of variable assignment has "
                   "non-composite type %s", type_pp(tree_type(target)));

      const int nassocs = tree_assocs(target);
      for (int i = 0; i < nassocs; i++) {
         tree_t a = tree_assoc(target, i);
         tree_t value = tree_value(a);

         if (!sem_check_variable_target(value))
            return false;

         if (!sem_static_name(value, sem_locally_static))
            sem_error(value, "aggregate element must be locally static name");

         assoc_kind_t kind = tree_subkind(a);
         switch (kind) {
         case A_OTHERS:
            sem_error(a, "others association not allowed in aggregate "
                      "variable target");
         case A_RANGE:
            sem_error(a, "range association not allowed in aggregate "
                      "variable target");
         case A_NAMED:
            sem_error(a, "sorry, named associations are not yet "
                      "supported here");
         case A_POS:
            break;
         }
      }
   }
   else {
      tree_t decl = sem_check_lvalue(target);

      bool suitable = false;
      if (decl != NULL) {
         const tree_kind_t kind = tree_kind(decl);
         suitable = kind == T_VAR_DECL
            || (kind == T_PORT_DECL && tree_class(decl) == C_VARIABLE);
      }

      if (!suitable)
         sem_error(target, "target of variable assignment must be a variable "
                   "name or aggregate");
   }

   return true;
}

static bool sem_check_var_assign(tree_t t)
{
   tree_t target = tree_target(t);
   tree_t value = tree_value(t);

   if (!sem_check(target))
      return false;

   if (!sem_check(value))
      return false;

   if (!sem_readable(value))
      return false;

   if (!sem_check_variable_target(target))
      return false;

   type_t target_type = tree_type(target);
   type_t value_type  = tree_type(value);

   if (type_is_protected(target_type))
      sem_error(t, "may not assign to variable of a protected type");

   if (!sem_check_same_type(value, target))
      sem_error(t, "type of value %s does not match type of target %s",
                type_pp2(value_type, target_type),
                type_pp2(target_type, value_type));

   return true;
}

static bool sem_check_waveforms(tree_t t, type_t expect)
{
   type_t std_time = std_type(NULL, "TIME");

   for (unsigned i = 0; i < tree_waveforms(t); i++) {
      tree_t waveform = tree_waveform(t, i);
      tree_t value = tree_value(waveform);

      if (!sem_check(value))
         return false;

      if (!sem_readable(value))
         return false;

      type_t value_type = tree_type(value);

      if (!sem_check_type(value, expect))
         sem_error(t, "type of value %s does not match type of target %s",
                   type_pp2(value_type, expect),
                   type_pp2(expect, value_type));

      if (tree_has_delay(waveform)) {
         tree_t delay = tree_delay(waveform);
         if (!sem_check(delay))
            return false;

         if (!sem_check_type(delay, std_time))
            sem_error(delay, "type of delay must be %s but have %s",
                      type_pp(std_time), type_pp(tree_type(delay)));
      }
   }

   return true;
}

static bool sem_check_signal_target(tree_t target)
{
   if (tree_kind(target) == T_AGGREGATE) {
      // Rules for aggregate signal targets in LRM 93 section 8.4

      if (!type_is_composite(tree_type(target)))
         sem_error(target, "aggregate target of signal assignment has "
                   "non-composite type %s", type_pp(tree_type(target)));

      const int nassocs = tree_assocs(target);
      for (int i = 0; i < nassocs; i++) {
         tree_t a = tree_assoc(target, i);
         tree_t value = tree_value(a);

         if (!sem_check_signal_target(value))
            return false;

         if (!sem_static_name(value, sem_locally_static))
            sem_error(value, "aggregate element must be locally static name");

         assoc_kind_t kind = tree_subkind(a);
         switch (kind) {
         case A_OTHERS:
            sem_error(a, "others association not allowed in aggregate "
                      "signal target");
         case A_RANGE:
            sem_error(a, "range association not allowed in aggregate "
                      "signal target");
         case A_NAMED:
            sem_error(a, "sorry, named associations are not yet "
                      "supported here");
         case A_POS:
            break;
         }
      }

      return true;
   }
   else {
      tree_t decl = sem_check_lvalue(target);
      if (decl == NULL)
         sem_error(target, "target of signal assignment must be a signal "
                   "name or aggregate");

      switch (tree_kind(decl)) {
      case T_SIGNAL_DECL:
         break;

      case T_PORT_DECL:
         if (tree_subkind(decl) == PORT_IN)
            sem_error(target, "cannot assign to input port %s",
                      istr(tree_ident(decl)));
         else if (tree_class(decl) != C_SIGNAL)
            sem_error(target, "target of signal assignment is not a signal");
         break;

      case T_VAR_DECL:
      case T_CONST_DECL:
         sem_error(target, "%s %s is not a valid target of signal assignment",
                   class_str(class_of(decl)), istr(tree_ident(decl)));

      default:
         sem_error(target, "invalid target of signal assignment");
      }

      return true;
   }
}

static bool sem_check_reject(tree_t t)
{
   if (!sem_check(t))
      return false;

   type_t std_time = std_type(NULL, "TIME");
   if (!type_eq(tree_type(t), std_time))
      sem_error(t, "reject interval must have type TIME");

   return true;
}

static bool sem_check_signal_assign(tree_t t)
{
   tree_t target = tree_target(t);

   if (!sem_check(target))
      return false;

   if (!sem_check_signal_target(target))
      return false;

   if (!sem_check_waveforms(t, tree_type(target)))
      return false;

   if (tree_has_reject(t) && !sem_check_reject(tree_reject(t)))
      return false;

   return true;
}

static bool sem_check_cassign(tree_t t)
{
   tree_t target = tree_target(t);

   if (!sem_check(target))
      return false;

   if (!sem_check_signal_target(target))
      return false;

   type_t std_bool = std_type(NULL, "BOOLEAN");

   const int nconds = tree_conds(t);
   for (int i = 0; i < nconds; i++) {
      tree_t c = tree_cond(t, i);

      if (tree_has_value(c)) {
         tree_t test = tree_value(c);

         if (!sem_check(test))
            return false;

         if (!type_eq(tree_type(test), std_bool))
            sem_error(test, "type of condition must be BOOLEAN");
      }

      if (tree_has_reject(c) && !sem_check_reject(tree_reject(c)))
         return false;

      if (!sem_check_waveforms(c, tree_type(target)))
         return false;
   }

   return true;
}

static bool sem_check_conversion(tree_t t)
{
   // Type conversions are described in LRM 93 section 7.3.5

   // Really we should push the set of types that are closely related
   // to the one being converted to
   tree_t value = tree_value(t);
   if (!sem_check(value))
      return false;

   type_t from = tree_type(value);
   type_t to   = tree_type(t);

   if (type_eq(to, from))
      return true;

   // Resolve both types to their base types
   from = type_base_recur(from);
   to   = type_base_recur(to);

   type_kind_t from_k = type_kind(from);
   type_kind_t to_k   = type_kind(to);

   const bool from_num = (from_k == T_INTEGER) || (from_k == T_REAL);
   const bool to_num   = (to_k == T_INTEGER) || (to_k == T_REAL);

   // Conversions are allowed between any abstract numeric types
   if (from_num && to_num)
      return true;

   const bool from_array = (from_k == T_CARRAY || from_k == T_UARRAY);
   const bool to_array   = (to_k == T_CARRAY || to_k == T_UARRAY);

   if (from_array && to_array) {
      // Types must have same dimensionality
      bool same_dim = (dimension_of(from) == dimension_of(to));

      // TODO: index types the same or closely related

      // Element types must be the same
      bool same_elem = type_eq(type_elem(from), type_elem(to));

      if (same_dim && same_elem)
         return true;
   }

   sem_error(t, "conversion only allowed between closely related types");
}

static bool sem_copy_default_args(tree_t call, tree_t decl)
{
   const int nparams = tree_params(call);
   const int nports  = tree_ports(decl);

   // Copy the default values for any unspecified arguments
   for (int i = 0; i < nports; i++) {
      tree_t port  = tree_port(decl, i);
      ident_t name = tree_ident(port);

      tree_t found = NULL;
      for (int j = 0; (j < nparams) && (found == NULL); j++) {
         tree_t p = tree_param(call, j);
         switch (tree_subkind(p)) {
         case P_POS:
            if (tree_pos(p) == i)
               found = p;
            break;
         case P_NAMED:
            {
               tree_t ref = tree_name(p);
               if (tree_ident(ref) == name) {
                  found = p;
                  tree_set_ref(ref, port);
               }
            }
            break;
         default:
            assert(false);
         }
      }

      if (found == NULL) {
         if (tree_has_value(port))
            found = add_param(call, tree_value(port), P_NAMED, make_ref(port));
         else
            sem_error(call, "missing actual for formal %s without "
                      "default value", istr(name));
      }

      // Constrain the type of any universal arguments
      tree_t value = tree_value(found);
      if (tree_has_type(value) && type_is_universal(tree_type(value)))
         tree_set_type(value, tree_type(port));
   }

   return true;
}

static bool sem_check_params(tree_t t)
{
   bool have_named = false;
   const int nparams = tree_params(t);
   for (int i = 0; i < nparams; i++) {
      tree_t p = tree_param(t, i);

      switch (tree_subkind(p)) {
      case P_POS:
         if (have_named)
            sem_error(p, "positional parameters must precede named "
                      "parameters");
         break;

      case P_NAMED:
         {
            tree_t ref = tree_name(p);
            if (tree_kind(ref) != T_REF)
               sem_error(ref, "sorry, this form of parameter name "
                        "is not yet supported");

            ident_t name = tree_ident(ref);
            for (int j = 0; j < i; j++) {
               tree_t q = tree_param(t, j);
               if ((tree_subkind(q) == P_NAMED)
                   && (tree_ident(tree_name(q)) == name))
                  sem_error(p, "duplicate parameter name %s", istr(name));
            }

            have_named = true;
         }
         break;
      }

      if (!sem_check(tree_value(p)))
         return false;
   }

   return true;
}

static bool sem_check_call_args(tree_t t, tree_t decl)
{
   const int nparams = tree_params(t);
   const int nports  = tree_ports(decl);

   if (nparams > nports)
      sem_error(t, "expected %d argument%s for subprogram %s but have %d",
                nports, nports > 1 ? "s" : "",
                type_pp(tree_type(decl)), nparams);

   for (int i = 0; i < nparams; i++) {
      tree_t param = tree_param(t, i);

      int index = -1;
      if (tree_subkind(param) == P_POS)
         index = tree_pos(param);
      else {
         assert(tree_subkind(param) == P_NAMED);

         tree_t ref = tree_name(param);
         assert(tree_kind(ref) == T_REF);

         ident_t name = tree_ident(ref);
         for (int j = 0; (j < nports) && (index == -1); j++) {
            if (tree_ident(tree_port(decl, j)) == name)
               index = j;
         }
         assert(index != -1);

         // Set the ref again here because solve_types may have set it
         // to the wrong overload
         tree_set_ref(ref, tree_port(decl, index));
      }

      tree_t  port     = tree_port(decl, index);
      class_t class    = tree_class(port);
      port_mode_t mode = tree_subkind(port);

      tree_t value = tree_value(param);
      tree_kind_t kind = tree_kind(value);

      type_t port_type = tree_type(port);

      if (!sem_check_type(value, port_type))
         sem_error(value, "type of actual %s does not match formal %s type %s",
                   type_pp2(tree_type(value), port_type),
                   istr(tree_ident(port)),
                   type_pp2(port_type, tree_type(value)));

      while (kind == T_ARRAY_REF || kind == T_ARRAY_SLICE
             || kind == T_ALL || kind == T_RECORD_REF
             || (kind == T_REF && tree_kind(tree_ref(value)) == T_ALIAS)) {
         if (kind == T_REF)
            value = tree_value(tree_ref(value));
         else
            value = tree_value(value);
         kind = tree_kind(value);
      }

      if (class == C_SIGNAL) {
         if (tree_kind(value) == T_OPEN)
            sem_error(value, "actual for formal %s with class SIGNAL must "
                      "not be OPEN", istr(tree_ident(port)));

         if (kind != T_REF)
            sem_error(value, "actual for formal %s with class SIGNAL must be a "
                      "name denoting a signal", istr(tree_ident(port)));

         // The 'LAST_VALUE attribute may be accessed in the body so cannot
         // optimise this out
         tree_set_flag(tree_ref(value), TREE_F_LAST_VALUE);
      }

      if (class == C_VARIABLE) {
         if (kind != T_REF)
            sem_error(value, "actual for formal %s with class VARIABLE must be "
                      "a name denoting a variable", istr(tree_ident(port)));

         tree_t decl = tree_ref(value);
         tree_kind_t decl_kind = tree_kind(decl);

         if (decl_kind == T_SIGNAL_DECL)
            sem_error(value, "cannot associate signal %s with parameter "
                      "class VARIABLE", istr(tree_ident(decl)));
         else if (decl_kind == T_FILE_DECL)
            sem_error(value, "cannot associate file %s with parameter "
                      "class VARIABLE", istr(tree_ident(decl)));
         else if (decl_kind == T_PORT_DECL) {
            const class_t class = tree_class(decl);
            if (mode == PORT_OUT && tree_subkind(decl) == PORT_IN)
               sem_error(value, "cannot read parameter %s with mode IN",
                         istr(tree_ident(decl)));
            else if ((mode == PORT_OUT || mode == PORT_INOUT)
                     && (class == C_CONSTANT || class == C_DEFAULT))
               sem_error(value, "object %s has class CONSTANT and "
                         "cannot be associated with OUT or INOUT parameters",
                         istr(tree_ident(decl)));
         }
         else if ((decl_kind != T_VAR_DECL) && (decl_kind != T_ALIAS))
            sem_error(value, "invalid use of name %s", istr(tree_ident(decl)));
      }

      // Check IN and INOUT parameters can be read
      if (tree_kind(t) != T_ATTR_REF) {
         port_mode_t mode = tree_subkind(port);
         if (mode == PORT_IN || mode == PORT_INOUT) {
            if (!sem_readable(value))
               return false;
         }
      }
   }

   return true;
}

static bool sem_check_fcall(tree_t t)
{
   if (!sem_check_params(t))
      return false;

   if (tree_kind(t) == T_PROT_FCALL && tree_has_name(t)
       && !sem_check(tree_name(t)))
      return false;

   if (!tree_has_ref(t))
      return false;

   tree_t decl = tree_ref(t);

   // Pure function may not call an impure function
   tree_t sub = top_scope->subprog;
   if ((sub != NULL) && (tree_kind(sub) == T_FUNC_BODY)) {
      if (!(tree_flags(sub) & TREE_F_IMPURE)
          && (tree_flags(decl) & TREE_F_IMPURE)
          && !(relax_rules() & RELAX_IMPURE))
         sem_error(t, "pure function %s cannot call impure function %s",
                   istr(tree_ident(sub)), istr(tree_ident(decl)));
   }

   if (!sem_check_call_args(t, decl))
      return false;

   if (!sem_copy_default_args(t, decl))
      return false;

   return true;
}

static bool sem_check_pcall(tree_t t)
{
   if (!sem_check_params(t))
      return false;

   if (tree_kind(t) == T_PROT_PCALL && tree_has_name(t)
       && !sem_check(tree_name(t)))
      return false;

   if (!tree_has_ref(t))
      return false;

   tree_t decl = tree_ref(t);

   if (!sem_check_call_args(t, decl))
      return false;

   if (!sem_copy_default_args(t, decl))
      return false;

   const wait_level_t waits = tree_attr_int(decl, wait_level_i, WAITS_MAYBE);
   if (waits == WAITS_YES)
      top_scope->wait_level = WAITS_YES;
   else if (waits == WAITS_MAYBE && top_scope->wait_level == WAITS_NO)
      top_scope->wait_level = WAITS_MAYBE;

   const impure_io_t impure_io = tree_attr_int(decl, impure_io_i, 0);
   top_scope->impure_io |= impure_io;

   const bool in_func = top_scope->subprog != NULL
      && tree_kind(top_scope->subprog) == T_FUNC_BODY;

   const bool in_pure_func = in_func && !(tree_flags(top_scope->subprog) & TREE_F_IMPURE);

   if (waits == WAITS_YES && in_func)
      sem_error(t, "function %s cannot call procedure %s which contains "
                "a wait statement", istr(tree_ident(top_scope->subprog)),
                istr(tree_ident(decl)));
   else if ((impure_io & IMPURE_FILE) && in_pure_func)
      sem_error(t, "pure function %s cannot call procedure %s which references "
                "a file object", istr(tree_ident(top_scope->subprog)),
                istr(tree_ident(decl)));
   else if ((impure_io & IMPURE_SHARED) && in_pure_func)
      sem_error(t, "pure function %s cannot call procedure %s which references "
                "a shared variable", istr(tree_ident(top_scope->subprog)),
                istr(tree_ident(decl)));

   return true;
}

static bool sem_check_wait(tree_t t)
{
   if (tree_has_delay(t)) {
      type_t std_time = std_type(NULL, "TIME");
      tree_t delay = tree_delay(t);

      if (!sem_check(delay))
         return false;

      if (!sem_check_type(delay, std_time))
         sem_error(delay, "type of delay must be %s but have %s",
                   type_pp(std_time), type_pp(tree_type(delay)));
   }

   if (tree_has_value(t)) {
      type_t std_bool = std_type(NULL, "BOOLEAN");
      tree_t value = tree_value(t);

      if (!sem_check(value))
         return false;

      if (!sem_check_type(value, std_bool))
         sem_error(value, "type of condition must be BOOLEAN but have %s",
                   type_pp(tree_type(value)));
   }

   if (top_scope->flags & SCOPE_PROTECTED)
      sem_error(t, "wait statement not allowed in protected subprogram body");
   else if (top_scope->subprog != NULL
            && tree_kind(top_scope->subprog) == T_FUNC_BODY)
      sem_error(t, "wait statement not allowed in function body");

   top_scope->wait_level = WAITS_YES;

   return sem_check_sensitivity(t);
}

static bool sem_check_assert(tree_t t)
{
   // Rules for asserion statements are in LRM 93 section 8.2

   type_t std_bool     = std_type(NULL, "BOOLEAN");
   type_t std_string   = std_type(NULL, "STRING");
   type_t std_severity = std_type(NULL, "SEVERITY_LEVEL");

   tree_t value    = tree_has_value(t) ? tree_value(t) : NULL;
   tree_t severity = tree_severity(t);
   tree_t message  = tree_has_message(t) ? tree_message(t) : NULL;

   if (value != NULL && !sem_check(value))
      return false;

   if (!sem_check(severity))
      return false;

   if (message != NULL && !sem_check(message))
      return false;

   if (value != NULL && !sem_check_type(value, std_bool))
      sem_error(value, "type of assertion expression must "
                "be %s but is %s", type_pp(std_bool),
                type_pp(tree_type(value)));

   if (!sem_check_type(severity, std_severity))
      sem_error(severity, "type of severity must be %s but is %s",
                type_pp(std_severity),
                type_pp(tree_type(severity)));

   if (message != NULL && !sem_check_type(message, std_string))
      sem_error(message, "type of message be %s but is %s",
                type_pp(std_string),
                type_pp(tree_type(message)));

   return true;
}

static bool sem_is_character_array(type_t t)
{
   // According LRM 93 section 3.1.1 an enumeration type is a character
   // type if at least one of its enumeration literals is a character
   // literal

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

   if (type_is_unconstrained(type)) {
      // Construct a new array type: the direction and bounds are the same
      // as those for a positional array aggregate

      type_t tmp = type_new(T_SUBTYPE);
      type_set_ident(tmp, type_ident(type));
      type_set_base(tmp, type);

      type_t index_type = index_type_of(type, 0);
      const type_kind_t indexk = type_kind(index_type);

      // The direction is determined by the index type
      range_kind_t dir = direction_of(index_type, 0);

      // The left bound is the left of the index type and the right bound
      // is determined by the number of elements

      tree_t left = NULL, right = NULL;
      type_t std_int = std_type(NULL, "INTEGER");

      if (indexk == T_ENUM)
         left = make_ref(type_enum_literal(index_type, 0));
      else
         left = tree_left(range_of(index_type, 0));

      right = call_builtin(S_ADD, index_type,
                           sem_int_lit(std_int, nchars - 1),
                           left, NULL);

      // TODO: should this happen in type solver?
      tree_t r = tree_new(T_RANGE);
      tree_set_subkind(r, dir);
      tree_set_left(r, left);
      tree_set_right(r, right);
      tree_set_loc(r, tree_loc(t));

      tree_t c = tree_new(T_CONSTRAINT);
      tree_set_subkind(c, C_COMPUTED);
      tree_add_range(c, r);
      tree_set_loc(c, tree_loc(t));

      type_set_constraint(tmp, c);

      tree_set_type(t, tmp);
      tree_set_flag(t, TREE_F_UNCONSTRAINED);
   }
   else
      tree_set_type(t, type);

   return true;
}

static bool sem_check_literal(tree_t t)
{
   switch (tree_subkind(t)) {
   case L_INT:
   case L_REAL:
   case L_PHYSICAL:
      break;

   case L_NULL:
      if (!type_is_access(tree_type(t)))
         sem_error(t, "null expression must have access type");
      break;

   case L_STRING:
      return sem_check_string_literal(t);

   default:
      assert(false);
   }

   return true;
}

static bool sem_check_aggregate(tree_t t)
{
   // Rules for aggregates are in LRM 93 section 7.3.2

   type_t composite_type = tree_type(t);

   if (type_is_none(composite_type))
      return false;
   assert(type_is_composite(composite_type));

   type_t base_type = type_base_recur(composite_type);

   const bool unconstrained = type_is_unconstrained(composite_type);
   const bool array = type_is_array(composite_type);

   // All positional associations must appear before named associations
   // and those must appear before any others association

   enum { POS, NAMED, OTHERS } state = POS;
   bool have_named = false;
   bool have_pos = false;
   bool have_others = false;

   const int nassocs = tree_assocs(t);

   for (int i = 0; i < nassocs; i++) {
      tree_t a = tree_assoc(t, i);

      switch (tree_subkind(a)) {
      case A_POS:
         if (state > POS)
            sem_error(a, "positional associations must appear "
                      "first in aggregate");
         have_pos = true;
         break;

      case A_NAMED:
      case A_RANGE:
         if (state > NAMED)
            sem_error(a, "named association must not follow "
                      "others association in aggregate");
         state = NAMED;
         have_named = true;
         break;

      case A_OTHERS:
         if (state == OTHERS)
            sem_error(a, "only a single others association "
                      "allowed in aggregate");
         if (unconstrained)
            sem_error(a, "others choice not allowed in this context");
         state = OTHERS;
         have_others = true;
         break;
      }
   }

   // Named and positional associations cannot be mixed in array
   // aggregates

   if (array && have_named && have_pos)
      sem_error(t, "named and positional associations cannot be "
                "mixed in array aggregates");

   if (array) {
      type_t elem_type = NULL;
      const int ndims = dimension_of(composite_type);
      if (ndims == 1)
         elem_type = type_elem(base_type);
      else {
         // The parser will have constructed a type with ndims - 1
         // dimensions.
         elem_type = tree_type(tree_value(tree_assoc(t, 0)));

         if (!type_is_unconstrained(elem_type)) {
            if (!sem_check_array_dims(elem_type, NULL))
               return false;
         }
      }

      type_t index_type = index_type_of(composite_type, 0);

      for (int i = 0; i < nassocs; i++) {
         tree_t a = tree_assoc(t, i);

         switch (tree_subkind(a)) {
         case A_RANGE:
            {
               tree_t r = tree_range(a, 0);
               if (!sem_check_discrete_range(r, index_type))
                  return false;
            }
            break;

         case A_NAMED:
            if (!sem_check(tree_name(a)))
               return false;
            break;

         default:
            break;
         }

         tree_t value = tree_value(a);

         if (!sem_check(value))
            return false;

         if (!sem_check_type(value, elem_type))
            sem_error(value, "type of element %s does not match base "
                      "type of aggregate %s",
                      type_pp(tree_type(value)),
                      type_pp(elem_type));
      }
   }

   // Checks for record aggregates are given in LRM 93 section 7.3.2.1

   if (type_is_record(base_type)) {
      const int nfields = type_fields(base_type);
      bool have[nfields];
      int pos = 0;
      for (int i = 0; i < nfields; i++)
         have[i] = false;

      for (int i = 0; i < nassocs; i++) {
         tree_t a = tree_assoc(t, i);
         int f = -1;

         switch (tree_subkind(a)) {
         case A_NAMED:
            {
               tree_t name = tree_name(a);
               if (tree_kind(name) != T_REF)
                  sem_error(name, "association name must be a field "
                            "identifier");

               ident_t name_i = tree_ident(name);
               for (f = 0; f < nfields; f++) {
                  if (tree_ident(type_field(base_type, f)) == name_i)
                     break;
               }

               if (f == nfields) {
                  // Should have been checked in parser
                  assert(error_count() > 0);
                  return false;
               }
            }
            break;

         case A_POS:
            {
               if (pos >= nfields)
                  sem_error(t, "too many positional associations");

               f = pos++;
            }
            break;

         case A_OTHERS:
            f = -1;
            break;

         case A_RANGE:
            sem_error(a, "range is not allowed here");
         }

         for (int j = 0; j < nfields; j++) {
            if ((f != -1) && (f != j))
               continue;

            tree_t field = type_field(base_type, j);
            type_t field_type = tree_type(field);

            if (have[j]) {
               if (f == -1)
                  continue;
               else
                  sem_error(a, "field %s already has a value",
                            istr(tree_ident(field)));
            }

            tree_t value = tree_value(a);

            if (!sem_check(value))
               return false;

            if (!sem_check_type(value, field_type))
               sem_error(value, "type of value %s does not match type %s"
                         " of field %s",
                         type_pp2(tree_type(value), field_type),
                         type_pp2(field_type, tree_type(value)),
                         istr(tree_ident(field)));

            have[j] = true;
         }
      }

      for (int i = 0; i < nfields; i++) {
         if (!have[i]) {
            tree_t field = type_field(base_type, i);
            sem_error(t, "field %s does not have a value",
                      istr(tree_ident(field)));
         }
      }
   }

   // If a named choice is not locally static then it must be the
   // only element

   for (int i = 0; i < nassocs; i++) {
      tree_t a = tree_assoc(t, i);
      if (tree_subkind(a) == A_NAMED && !sem_locally_static(tree_name(a))) {
         if (tree_assocs(t) != 1)
            sem_error(tree_name(a), "non-locally static choice must be "
                      "only choice");
      }
   }

   // If there is no others choice or the base type is unconstrained then
   // construct a new array subtype using the rules in LRM 93 7.3.2.2

   if (array && (have_named || unconstrained) && !have_others) {
      type_t tmp = type_new(T_SUBTYPE);
      type_set_ident(tmp, type_ident(base_type));
      type_set_base(tmp, base_type);

      const int ndims = dimension_of(composite_type);

      type_t index_type = index_type_of(composite_type, 0);

      range_kind_t dir;
      if (unconstrained) {
         // The direction is determined by the index type
         dir = direction_of(index_type, 0);
      }
      else {
         // The direction is determined by the context
         dir = direction_of(composite_type, 0);
      }

      assert(dir == RANGE_TO || dir == RANGE_DOWNTO);

      tree_t left = NULL, right = NULL;

      if (have_pos || !have_named) {
         // The left bound is the left of the index type and the right bound
         // is determined by the number of elements

         assert(unconstrained);

         if (type_kind(index_type) == T_ENUM) {
            const unsigned nlits = type_enum_literals(index_type);

            if (nassocs > nlits) {
               sem_error(t, "too many elements in array");
            }
            left = make_ref(type_enum_literal(index_type, 0));
            right = make_ref(type_enum_literal(index_type, nassocs - 1));
         }
         else {
            type_t std_int = std_type(NULL, "INTEGER");
            left = tree_left(range_of(index_type, 0));
            right = call_builtin(S_ADD, index_type,
                                 sem_int_lit(std_int, nassocs - 1),
                                 left, NULL);
         }
      }
      else {
         // The left and right bounds are determined by the smallest and
         // largest choices

         tree_t low  = call_builtin(S_MINIMUM, index_type, NULL);
         tree_t high = call_builtin(S_MAXIMUM, index_type, NULL);

         tree_set_loc(low, tree_loc(t));
         tree_set_loc(high, tree_loc(t));

         for (int i = 0; i < nassocs; i++) {
            tree_t a = tree_assoc(t, i);
            switch (tree_subkind(a)) {
            case A_NAMED:
               {
                  tree_t name = tree_name(a);
                  add_param(low, name, P_POS, NULL);
                  add_param(high, name, P_POS, NULL);
               }
               break;

            case A_RANGE:
               {
                  tree_t r = tree_range(a, 0);
                  switch (tree_subkind(r)) {
                  case RANGE_TO:
                     add_param(low, tree_left(r), P_POS, NULL);
                     add_param(high, tree_right(r), P_POS, NULL);
                     break;

                  case RANGE_DOWNTO:
                     add_param(low, tree_right(r), P_POS, NULL);
                     add_param(high, tree_left(r), P_POS, NULL);
                     break;

                  case RANGE_EXPR:
                     {
                        tree_t name = tree_name(tree_value(r));

                        tree_t rlow = tree_new(T_ATTR_REF);
                        tree_set_ident(rlow, ident_new("LOW"));
                        tree_set_name(rlow, name);
                        tree_set_subkind(rlow, ATTR_LOW);
                        tree_set_type(rlow, tree_type(r));

                        tree_t rhigh = tree_new(T_ATTR_REF);
                        tree_set_ident(rhigh, ident_new("HIGH"));
                        tree_set_name(rhigh, name);
                        tree_set_subkind(rhigh, ATTR_HIGH);
                        tree_set_type(rhigh, tree_type(r));

                        add_param(low, rlow, P_POS, NULL);
                        add_param(high, rhigh, P_POS, NULL);
                     }
                     break;
                  }
               }
               break;
            }
         }

         left  = (dir == RANGE_TO ? low : high);
         right = (dir == RANGE_TO ? high : low);
      }

      tree_t constraint = tree_new(T_CONSTRAINT);
      tree_set_subkind(constraint, C_INDEX);

      tree_t r = tree_new(T_RANGE);
      tree_set_subkind(r, dir);
      tree_set_left(r, left);
      tree_set_right(r, right);
      tree_set_loc(r, tree_loc(t));
      tree_set_type(r, tree_type(left));

      tree_add_range(constraint, r);

      for (int i = 1; i < ndims; i++) {
         tree_t dim;
         if (unconstrained)
            dim = range_of(tree_type(tree_value(tree_assoc(t, 0))), i - 1);
         else
            dim = range_of(composite_type, i);
         tree_add_range(constraint, dim);
      }

      type_set_constraint(tmp, constraint);
      tree_set_type(t, tmp);
   }

   if (unconstrained)
      tree_set_flag(t, TREE_F_UNCONSTRAINED);

   return true;
}

static bool sem_check_ref(tree_t t)
{
   if (!tree_has_ref(t))
      return false;

   tree_t decl = tree_ref(t);
   class_t class = class_of(decl);

   const tree_kind_t kind = tree_kind(decl);
   switch (kind) {
   case T_PORT_DECL:
   case T_VAR_DECL:
   case T_SIGNAL_DECL:
   case T_FILE_DECL:
   case T_CONST_DECL:
   case T_ENUM_LIT:
   case T_ALIAS:
   case T_UNIT_DECL:
   case T_GENVAR:
   case T_FUNC_DECL:
   case T_FUNC_BODY:
      break;

   default:
      sem_error(t, "invalid use of %s %s", class_str(class),
                istr(tree_ident(t)));
   }

   if (top_scope->subprog != NULL) {
      if (kind == T_FILE_DECL)
         top_scope->impure_io |= IMPURE_FILE;
      else if (kind == T_VAR_DECL && (tree_flags(decl) & TREE_F_SHARED))
         top_scope->impure_io |= IMPURE_SHARED;
   }

   return true;
}

static bool sem_check_record_ref(tree_t t)
{
   tree_t value = tree_value(t);
   if (!sem_check(value))
      return false;

   type_t value_type = tree_type(value);

   if (!type_is_record(value_type))
      sem_error(value, "expected record type but found %s%s",
                type_is_incomplete(value_type) ? "incomplete type " : "",
                type_pp(value_type));

   return true;
}

static bool sem_check_array_ref(tree_t t)
{
   tree_t value = tree_value(t);
   if (!sem_check(value))
      return false;

   type_t type = tree_type(tree_value(t));

   if (!type_is_array(type))
      sem_error(t, "cannot index non-array type %s", type_pp(type));

   const int nindex  = dimension_of(type);
   const int nparams = tree_params(t);

   if (nparams != nindex)
      sem_error(t, "array %s has %d dimensions but %d indices given",
                istr(tree_ident(value)), nindex, nparams);

   bool ok = true;
   for (int i = 0; i < nparams; i++) {
      tree_t p = tree_param(t, i);
      if (tree_subkind(p) != P_POS)
         sem_error(t, "only scalar references supported");

      type_t expect = index_type_of(type, i);
      tree_t value = tree_value(p);

      ok = sem_check(value) && ok;

      if (ok && !sem_check_type(value, expect))
         sem_error(value, "type of index %s does not match type of "
                   "array dimension %s",
                   type_pp(tree_type(value)),
                   type_pp(expect));
   }

   return ok;
}

static bool sem_check_array_slice(tree_t t)
{
   if (!sem_check(tree_value(t)))
      return false;

   type_t array_type = tree_type(tree_value(t));

   if (!type_is_array(array_type))
      sem_error(t, "type of slice prefix is not an array");

   tree_t r = tree_range(t, 0);
   if (!sem_check_discrete_range(r, index_type_of(array_type, 0)))
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

static bool sem_check_valid_implicit_signal(tree_t t)
{
   // Certain attributes are illegal inside a subprogram according to LRM
   // 93 section 2.1.1.2

   if (top_scope->subprog != NULL)
      sem_error(t, "implicit signal %s cannot be used in a "
                "subprogram body", istr(tree_ident(t)));

   return true;
}

static bool sem_check_signal_attr(tree_t t)
{
   if (class_of(tree_name(t)) != C_SIGNAL)
      sem_error(t, "prefix of attribute %s must denote a signal",
                istr(tree_ident(t)));

   return true;
}

static bool sem_check_attr_param(tree_t t, type_t expect, int min, int max)
{
   const int nparams = tree_params(t);
   if (nparams == 0 && min > 0)
      sem_error(t, "attribute %s requires a parameter", istr(tree_ident(t)));
   else if (nparams > max)
      sem_error(t, "too many parameters for attribute %s", istr(tree_ident(t)));
   else if (nparams == 1) {
      tree_t dim = tree_value(tree_param(t, 0));
      if (!sem_check(dim))
         return false;

      tree_t value = tree_value(tree_param(t, 0));
      if (!sem_check_type(value, expect))
         sem_error(t, "expected type %s for attribute %s parameter but "
                   "have %s", type_pp(expect), istr(tree_ident(t)),
                   type_pp(tree_type(value)));
   }

   return true;
}

static bool sem_check_dimension_attr(tree_t t)
{
   if (!sem_check_attr_param(t, std_type(NULL, "INTEGER"), 0, 1))
      return false;

   if (tree_params(t) > 0) {
      if (!type_is_array(tree_type(tree_name(t))))
         sem_error(t, "prefix of attribute %s with dimension is not an array",
                   istr(tree_ident(t)));

      tree_t dim = tree_value(tree_param(t, 0));
      if (!sem_locally_static(dim))
         sem_error(dim, "dimension of attribute %s must be locally "
                   "static", istr(tree_ident(t)));
   }

   return true;
}

static bool sem_check_attr_ref(tree_t t, bool allow_range)
{
   // Attribute names are in LRM 93 section 6.6

   tree_t name = tree_name(t), decl = NULL;
   type_t named_type = NULL;
   bool has_type = true;
   switch (tree_kind(name)) {
   case T_REF:
      if (!tree_has_ref(name))
         return false;

      decl = tree_ref(name);
      has_type = class_has_type(class_of(decl));

      if (tree_kind(decl) == T_TYPE_DECL)
         named_type = tree_type(decl);

      break;

   case T_ATTR_REF:
      if (tree_subkind(name) == ATTR_BASE) {
         tree_t base = tree_name(name);

         if (tree_kind(base) == T_REF) {
            if (!tree_has_ref(base))
               return false;

            if (tree_kind(tree_ref(base)) == T_TYPE_DECL) {
               named_type = tree_type(base);
               break;
            }
         }

         sem_error(base, "prefix of BASE attribute must be a type or "
                   "subtype declaration");
      }
      // Fall-through

   default:
      if (!sem_check(name))
         return false;
   }

   ident_t attr = tree_ident(t);
   const attr_kind_t predef = tree_subkind(t);

   switch (predef) {
   case ATTR_RANGE:
   case ATTR_REVERSE_RANGE:
      {
         // TODO: can this check move to the parser?
         if (!allow_range)
            sem_error(t, "range expression not allowed here");

         type_t name_type = tree_has_type(name) ? tree_type(name) : NULL;
         const bool is_type_decl = decl != NULL && tree_kind(decl) == T_TYPE_DECL;
         const bool is_discrete =
            name_type != NULL && type_is_discrete(name_type);
         const bool invalid =
            name_type == NULL
            || (!(is_discrete && is_type_decl) && !type_is_array(name_type));

         if (invalid) {
            if (decl != NULL && class_has_type(class_of(decl))) {
               if (is_type_decl)
                  sem_error(t, "type %s does not have a range",
                            type_pp(tree_type(decl)));
               else
                  sem_error(t, "object %s does not have a range",
                            istr(tree_ident(decl)));
            }
            else
               sem_error(t, "prefix does not have a range");
         }

         if (decl != NULL && tree_kind(decl) == T_TYPE_DECL
             && type_is_unconstrained(name_type))
            sem_error(t, "cannot use attribute %s with unconstrained array "
                      "type %s", istr(attr), type_pp(name_type));

         return true;
      }

   case ATTR_LENGTH:
      {
         if (!has_type)
            sem_error(name, "prefix does not have LENGTH attribute");
         else if (!type_is_array(tree_type(name)))
            sem_error(name, "prefix of attribute LENGTH must be an array but "
                      "have type %s", type_pp(tree_type(name)));

         if (!sem_check_dimension_attr(t))
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

         if (!sem_check_dimension_attr(t))
            return false;

         if (!type_is_array(type) && !type_is_scalar(type))
            sem_error(t, "prefix does not have attribute %s", istr(attr));

         return true;
      }

   case ATTR_LAST_EVENT:
      if (!sem_check_attr_param(t, NULL, 0, 0))
         return false;

      if (!sem_check_signal_attr(t))
         return false;

      return true;

   case ATTR_EVENT:
   case ATTR_ACTIVE:
      if (!sem_check_signal_attr(t))
         return false;

      return true;

   case ATTR_LAST_VALUE:
      if (!sem_check_signal_attr(t))
         return false;

      tree_set_flag(tree_ref(name), TREE_F_LAST_VALUE);
      return true;

   case ATTR_PATH_NAME:
   case ATTR_INSTANCE_NAME:
      {
         const class_t class = class_of(name);
         if (class != C_SIGNAL && class != C_ARCHITECTURE && class != C_ENTITY
             && class != C_FUNCTION && class != C_PROCEDURE && class != C_LABEL
             && class != C_PACKAGE)
            sem_error(t, "prefix does not have attribute %s", istr(attr));

         return true;
      }

   case ATTR_DELAYED:
   case ATTR_STABLE:
   case ATTR_QUIET:
      {
         if (!sem_check_signal_attr(t))
            return false;

         if (!sem_check_valid_implicit_signal(t))
            return false;

         type_t std_time = std_type(NULL, "TIME");
         if (tree_params(t) > 0) {
            tree_t value = tree_value(tree_param(t, 0));

            if (!sem_check(value))
               return false;

            if (!sem_check_type(value, std_time))
               sem_error(value, "attribute %s parameter must have type %s",
                         istr(attr), type_pp(std_time));
         }
         else
            add_param(t, sem_int_lit(std_time, 0), P_POS, NULL);

         return true;
      }
   case ATTR_TRANSACTION:
      if (!sem_check_signal_attr(t))
         return false;

      if (!sem_check_valid_implicit_signal(t))
         return false;

      return true;

   case ATTR_IMAGE:
   case ATTR_VALUE:
      {
         if (named_type == NULL)
            sem_error(t, "prefix of attribute %s must be a type", istr(attr));

         type_t name_type = tree_type(name);
         if (!type_is_scalar(name_type))
            sem_error(t, "cannot use attribute %s with non-scalar type %s",
                      type_pp(name_type), istr(attr));

         type_t std_string = std_type(NULL, "STRING");
         type_t arg_type = predef == ATTR_IMAGE ? name_type : std_string;
         if (!sem_check_attr_param(t, arg_type, 1, 1))
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
         if (named_type == NULL)
            sem_error(t, "prefix of attribute %s must be a type", istr(attr));

         type_t name_type = tree_type(name);

         if (!type_is_discrete(name_type) && !type_is_physical(name_type))
            sem_error(t, "prefix of attribute %s must be a discrete or "
                      "physical type", istr(attr));

         type_t std_int = std_type(NULL, "INTEGER");
         type_t arg_type = predef == ATTR_VAL ? std_int : name_type;

         if (!sem_check_attr_param(t, arg_type, 1, 1))
            return false;

         return true;
      }

   case ATTR_BASE:
      sem_error(t, "BASE attribute is allowed only as the prefix of the name "
                "of another attribute");

   case ATTR_DRIVING_VALUE:
   case ATTR_LAST_ACTIVE:
   case ATTR_DRIVING:
      fatal_at(tree_loc(t), "sorry, attribute %s not implemented", istr(attr));

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

static bool sem_check_qualified(tree_t t)
{
   if (tree_has_value(t)) {
      tree_t value = tree_value(t);

      if (!sem_check(value))
         return false;

      // LRM 08 section 9.3.5 qualified expressions: the operand shall have
      // the same type as the base type of the type mark
      type_t base = type_base_recur(tree_type(t));
      if (!sem_check_type(value, base))
         sem_error(value, "operand of qualified expression must have type %s",
                   type_pp(base));
   }

   return true;
}

static bool sem_check_actual(formal_map_t *formals, int nformals,
                             tree_t param, tree_t unit)
{
   tree_t value = tree_value(param);
   tree_t decl = NULL;
   type_t type = NULL;

   switch (tree_subkind(param)) {
   case P_POS:
      {
         const int pos = tree_pos(param);
         if (pos >= nformals)
            sem_error(value, "too many positional actuals");
         if (formals[pos].have)
            sem_error(value, "formal %s already has an actual",
                      istr(tree_ident(formals[pos].decl)));
         formals[pos].have = true;
         decl = formals[pos].decl;
         type = tree_type(decl);
      }
      break;

   case P_NAMED:
      {
         tree_t name = tree_name(param);
         tree_kind_t kind = tree_kind(name);
         tree_t ref = name;
         tree_t conv = NULL;

         if (kind == T_FCALL) {
            if (tree_params(name) != 1)
               sem_error(name, "output conversion function must have "
                         "exactly one parameter");

            conv = name;
            name = ref = tree_value(tree_param(name, 0));
            kind = tree_kind(ref);
         }
         else if (kind == T_TYPE_CONV) {
            conv = name;
            name = ref = tree_value(name);
            kind = tree_kind(ref);
         }

         while ((kind == T_ARRAY_REF) || (kind == T_ARRAY_SLICE)
             || (kind == T_RECORD_REF)) {
            ref  = tree_value(ref);
            kind = tree_kind(ref);
         }

         assert(tree_kind(ref) == T_REF);

         for (int i = 0; i < nformals; i++) {
            if (tree_ident(formals[i].decl) == tree_ident(ref)) {
               if (formals[i].have && !formals[i].partial)
                  sem_error(value, "formal %s already has an actual",
                            istr(tree_ident(formals[i].decl)));
               formals[i].have    = true;
               formals[i].partial = (tree_kind(name) != T_REF);
               decl = formals[i].decl;
               tree_set_ref(ref, decl);
               tree_add_attr_int(ref, formal_i, 1);
               break;
            }
         }

         if (decl == NULL)
            sem_error(value, "%s has no formal %s",
                      istr(tree_ident(unit)), istr(tree_ident(ref)));

         if (!sem_static_name(name, sem_locally_static))
            sem_error(name, "formal name must be locally static");

         if (conv != NULL) {
            port_mode_t mode = tree_subkind(decl);

            type = tree_type((mode == PORT_INOUT) ? name : conv);

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

   if (!sem_check(value))
      return false;

   type_t value_type = tree_type(value);

   if (!sem_check_type(value, type))
      sem_error(value, "type of actual %s does not match type %s of formal "
                "port %s", type_pp(value_type),
                type_pp(type), istr(tree_ident(decl)));

   if (tree_kind(value) == T_OPEN) {
      port_mode_t mode = tree_subkind(decl);

      if ((mode == PORT_IN) && !tree_has_value(decl))
         sem_error(value, "unconnected port %s with mode IN must have a "
                   "default value", istr(tree_ident(decl)));

      if ((mode != PORT_IN) && type_is_unconstrained(tree_type(decl)))
         sem_error(value, "port %s of unconstrained type %s cannot "
                   "be unconnected", istr(tree_ident(decl)),
                   type_pp(type));
   }

   // Check for type conversions and conversion functions
   // These only apply if the class of the formal is not constant

   tree_t actual = NULL;

   if (tree_class(decl) != C_CONSTANT) {
      if (tree_kind(value) == T_TYPE_CONV)
         actual = tree_value(value);
      else if (tree_kind(value) == T_FCALL) {
         // Conversion functions are in LRM 93 section 4.3.2.2

         tree_t func = tree_ref(value);
         if (tree_ports(func) == 1 && tree_params(value) == 1
             && (tree_flags(value) & TREE_F_CONVERSION))
            actual = tree_value(tree_param(value, 0));
      }
   }

   if (actual == NULL)
      actual = value;    // No conversion
   else {
      // LRM 93 section 3.2.1.1 result of a type conversion in an
      // association list cannot be an unconstrained array type
      if (type_is_unconstrained(value_type)
          && type_is_unconstrained(type))
         sem_error(value, "result of conversion for unconstrained formal "
                   "%s must be a constrained array type",
                   istr(tree_ident(decl)));

      if (tree_subkind(decl) == PORT_OUT)
         sem_error(value, "conversion not allowed for formal %s with "
                   "mode OUT", istr(tree_ident(decl)));
   }

   if (!sem_globally_static(actual)
       && !sem_static_name(actual, sem_globally_static))
      sem_error(value, "actual must be globally static expression "
                "or locally static name");

   return true;
}

static bool sem_check_map(tree_t t, tree_t unit,
                          tree_formals_t tree_Fs, tree_formal_t tree_F,
                          tree_actuals_t tree_As, tree_actual_t tree_A)
{
   // Check there is an actual for each formal port or generic
   // Rules for maps are described in LRM 93 section 5.2.1.2

   const int nformals = tree_Fs(unit);
   const int nactuals = tree_As(t);

   bool ok = true;

   formal_map_t formals[nformals];

   for (int i = 0; i < nformals; i++) {
      formals[i].decl    = tree_F(unit, i);
      formals[i].have    = false;
      formals[i].partial = false;
   }

   bool has_named = false;

   for (int i = 0; i < nactuals; i++) {
      tree_t p = tree_A(t, i);
      if (tree_subkind(p) != P_NAMED)
         continue;

      if (!has_named) {
         scope_push(NULL);
         top_scope->flags |= SCOPE_FORMAL;

         has_named = true;
      }

      tree_t name = tree_name(p);

      ok = sem_check(name) && ok;

      const tree_kind_t name_kind = tree_kind(name);
      if ((name_kind == T_ARRAY_REF || name_kind == T_ARRAY_SLICE)
          && tree_kind(tree_value(p)) == T_OPEN) {
         error_at(tree_loc(p), "sub-elements of composite port cannot be "
                  "associated with OPEN");
      }
   }

   if (has_named)
      scope_pop();

   if (!ok)
      return false;

   for (int i = 0; i < nactuals; i++)
      ok = sem_check_actual(formals, nformals, tree_A(t, i), unit) && ok;

   const tree_kind_t kind = tree_kind(unit);
   if (kind == T_ENTITY || kind == T_BLOCK) {
      // Component and configuration instantiations must be checked at
      // elaboration time

      for (int i = 0; i < nformals; i++) {
         if (!formals[i].have) {
            port_mode_t mode = tree_subkind(formals[i].decl);

            if ((mode == PORT_IN) && !tree_has_value(formals[i].decl)) {
               error_at(tree_loc(t), "missing actual for formal %s of "
                        "mode IN without a default expression",
                        istr(tree_ident(formals[i].decl)));
            }

            if ((mode != PORT_IN)
                && type_is_unconstrained(tree_type(formals[i].decl))) {
               error_at(tree_loc(t), "missing actual for formal %s with "
                        "unconstrained array type",
                        istr(tree_ident(formals[i].decl)));
            }
         }
      }
   }

   return ok;
}

static bool sem_find_unit(tree_t t, ident_t name, tree_t *unit)
{
   ident_t lname = ident_until(name, '.');
   lib_t lib = lib_loaded(lname);
   if (lib != NULL) {
      if ((*unit = lib_get_check_stale(lib, name)) == NULL)
         sem_error(t, "cannot find unit %s", istr(name));

      return true;
   }
   else
      sem_error(t, "missing library clause for %s", istr(lname));
}

static bool sem_check_instance(tree_t t)
{
   if (!tree_has_ref(t))
      return false;

   ident_t name = tree_ident2(t);
   tree_t unit = tree_ref(t);

   switch (tree_class(t)) {
   case C_ENTITY:
      if (tree_kind(unit) != T_ENTITY)
         sem_error(t, "unit %s is not an entity", istr(name));
      break;

   case C_COMPONENT:
      if (tree_kind(unit) != T_COMPONENT)
         sem_error(t, "object %s is not a component declaration", istr(name));
      break;

   default:
      sem_error(t, "sorry, this form of instance is not supported yet");
   }


   if (!sem_check_map(t, unit, tree_ports, tree_port, tree_params, tree_param))
      return false;

   if (!sem_check_map(t, unit, tree_generics, tree_generic,
                      tree_genmaps, tree_genmap))
      return false;

   return true;
}

static bool sem_check_if(tree_t t)
{
   type_t std_bool = std_type(NULL, "BOOLEAN");

   tree_t value = tree_value(t);
   if (!sem_check(value))
      return false;

   if (!sem_check_type(value, std_bool))
      sem_error(value, "type of condition must be %s but is %s",
                type_pp(std_bool), type_pp(tree_type(value)));

   if (!sem_readable(value))
      return false;

   return sem_check_stmts(t, tree_stmt, tree_stmts(t))
      && sem_check_stmts(t, tree_else_stmt, tree_else_stmts(t));
}

static bool sem_subtype_locally_static(type_t type)
{
   // Rules for locally static subtypes are in LRM 93 7.4.1

   if (type_is_unconstrained(type))
      return false;

   if (type_is_scalar(type))
      return true;

   if (type_is_record(type)) {
      const int nfields = type_fields(type);
      for (int i = 0; i < nfields; i++) {
         if (!sem_subtype_locally_static(tree_type(type_field(type, i))))
            return false;
      }

      return true;
   }

   switch (type_kind(type)) {
   case T_CARRAY:
   case T_SUBTYPE:
      {
         const int ndims = dimension_of(type);
         for (int i = 0; i < ndims; i++) {
            if (!sem_locally_static(range_of(type, i)))
               return false;
         }

         return true;
      }
   default:
      return true;
   }
}

static bool sem_unconstrained_value(tree_t expr)
{
   switch (tree_kind(expr)) {
   case T_AGGREGATE:
   case T_LITERAL:
      return !!(tree_flags(expr) & TREE_F_UNCONSTRAINED);

   default:
      return false;
   }
}

static bool sem_locally_static(tree_t t)
{
   // Rules for locally static expressions are in LRM 93 7.4.1

   type_t type = tree_type(t);
   tree_kind_t kind = tree_kind(t);

   // Any literal other than of type time
   if (kind == T_LITERAL) {
      if (tree_subkind(t) == L_PHYSICAL) {
         type_t std_time = std_type(NULL, "TIME");
         return !type_eq(type, std_time);
      }
      else
         return true;
   }
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
         if (!tree_has_value(decl))
            return false;

         tree_t value = tree_value(decl);
         return sem_subtype_locally_static(tree_type(decl))
            && sem_locally_static(value)
            && !sem_unconstrained_value(value);
      }
      else if ((standard() >= STD_08 || (relax_rules() & RELAX_LOCALLY_STATIC))
               && dkind == T_PORT_DECL) {
         // [2008] A generic reference with a locally static subtype
         return tree_class(decl) == C_CONSTANT
            && sem_subtype_locally_static(tree_type(decl));
      }
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

   // An alias of a locally static name
   if (kind == T_ALIAS)
      return sem_locally_static(tree_value(t));

   // A function call of an implicit operator with locally static actuals
   if (kind == T_FCALL) {
      if (!is_builtin(tree_subkind(tree_ref(t))))
         return false;

      bool all_static = true;
      const int nparams = tree_params(t);
      for (int i = 0; i < nparams; i++) {
         tree_t p = tree_param(t, i);
         all_static = all_static && sem_locally_static(tree_value(p));
      }
      return all_static;
   }

   if (kind == T_ATTR_REF) {
      // A predefined attribute other than those listed below whose prefix
      // prefix is either a locally static subtype or is an object that is
      // of a locally static subtype
      const attr_kind_t predef = tree_subkind(t);
      if (predef == ATTR_EVENT || predef == ATTR_ACTIVE
          || predef == ATTR_LAST_EVENT || predef == ATTR_LAST_ACTIVE
          || predef == ATTR_LAST_VALUE || predef == ATTR_DRIVING
          || predef == ATTR_DRIVING_VALUE || predef == ATTR_PATH_NAME)
         return false;
      else if (!tree_has_value(t)) {
         type_t type = tree_type(tree_name(t));
         return sem_subtype_locally_static(type);
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
      if (sem_unconstrained_value(t))
         return false;

      if (type_is_array(type) && !sem_locally_static(range_of(type, 0)))
         return false;

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

   // [2008] A slice name whose prefix and range is locally static
   if (kind == T_ARRAY_SLICE &&
       (standard() >= STD_08 || (relax_rules() & RELAX_LOCALLY_STATIC))) {

      if (!sem_locally_static(tree_range(t, 0)))
         return false;

      return sem_locally_static(tree_value(t));
   }

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
            return true;
         case T_ALIAS:
            return sem_static_name(tree_value(decl), check_fn);
         default:
            return false;
         }
      }

   case T_RECORD_REF:
   case T_ALL:
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

   default:
      return false;
   }
}

static bool sem_globally_static(tree_t t)
{
   // Rules for globally static expressions are in LRM 93 7.4.2

   type_t type = tree_type(t);
   tree_kind_t kind = tree_kind(t);

   // A literal of type TIME

   if ((kind == T_REF && tree_kind(tree_ref(t)) == T_UNIT_DECL)
       || (kind == T_LITERAL && tree_subkind(t) == L_PHYSICAL)) {
      type_t std_time = std_type(NULL, "TIME");
      if (type_eq(type, std_time))
         return true;
   }

   // A locally static primary

   if (sem_locally_static(t))
      return true;

   // A generic constant, generate parameter, or constant

   if (kind == T_REF) {
      tree_t decl = tree_ref(t);
      tree_kind_t decl_kind = tree_kind(decl);
      if ((decl_kind == T_PORT_DECL) && (tree_class(decl) == C_CONSTANT))
         return true;
      else if (decl_kind == T_GENVAR)
         return true;
      else if (decl_kind == T_CONST_DECL)
         return true;
   }

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
      if (type_is_array(type) && !sem_globally_static(range_of(type, 0)))
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

   // TODO: clause h

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
      // A predefined attribute other than those listed below whose prefix
      // is appropriate for a globally static attribute
      const attr_kind_t predef = tree_subkind(t);
      if (predef == ATTR_EVENT || predef == ATTR_ACTIVE
          || predef == ATTR_LAST_EVENT || predef == ATTR_LAST_ACTIVE
          || predef == ATTR_LAST_VALUE || predef == ATTR_DRIVING
          || predef == ATTR_DRIVING_VALUE)
         return false;   // Clause k
      else if (predef != ATTR_USER) {
         tree_t name = tree_name(t);
         switch (tree_kind(name)) {
         case T_REF:
            {
               tree_t decl = tree_ref(name);
               const tree_kind_t dkind = tree_kind(decl);
               if (dkind == T_VAR_DECL && type_is_access(tree_type(name)))
                  return false;
               else if (dkind == T_PORT_DECL)
                  return tree_class(decl) == C_CONSTANT
                     || !type_is_unconstrained(tree_type(decl));
               else
                  return dkind == T_CONST_DECL || dkind == T_SIGNAL_DECL
                     || dkind == T_TYPE_DECL || dkind == T_VAR_DECL;
            }
         case T_FCALL:
            return sem_globally_static(name);
         default:
            return false;
         }
      }

      // A user-defined attribute whose value is a globally static expression
      assert(tree_has_value(t));
      return sem_globally_static(tree_value(t));
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

static bool sem_check_case(tree_t t)
{
   tree_t test = tree_value(t);
   if (!sem_check(test))
      return false;

   type_t type = tree_type(test);

   // LRM 93 8.8 if the type of the expression is an array then it must be
   // a one dimensional character array type

   const bool is_1d_character_array =
      type_is_array(type)
      && sem_is_character_array(type)
      && dimension_of(type) == 1;
   const bool valid = is_1d_character_array || type_is_discrete(type);

   if (!valid)
      sem_error(test, "case expression must have a discrete type or one "
                "dimensional character array type");

   if (is_1d_character_array && !sem_subtype_locally_static(type))
      sem_error(test, "case expression must have locally static subtype");

   tree_t last = NULL;
   bool ok = true;
   const int nassocs = tree_assocs(t);
   for (int i = 0; i < nassocs; i++) {
      tree_t a = tree_assoc(t, i);
      switch (tree_subkind(a)) {
      case A_OTHERS:
         if (i != tree_assocs(t) - 1)
            sem_error(t, "others choice must appear last");
         break;

      case A_NAMED:
         {
            tree_t name = tree_name(a);
            if ((ok = sem_check(name) && ok)) {
               if (!sem_check_type(name, type))
                  sem_error(name, "case choice must have type %s but found %s",
                            type_pp(type), type_pp(tree_type(name)));
               else if (!sem_locally_static(name))
                  sem_error(name, "case choice must be locally static");
            }
         }
         break;

      case A_RANGE:
         {
            tree_t r = tree_range(a, 0);
            if ((ok = sem_check_discrete_range(r, type) && ok)) {
               if (!sem_locally_static(tree_left(r)))
                  sem_error(tree_left(r), "left index of case choice range is "
                            "not locally static");
               else if (!sem_locally_static(tree_right(r)))
                  sem_error(tree_right(r), "right index of case choice range "
                            "is not locally static");
            }
         }
         break;

      default:
         sem_error(a, "sorry, this form of choice is not supported");
      }

      tree_t stmt = tree_value(a);
      if (stmt != last)
         ok = sem_check(stmt) && ok;
      last = stmt;
   }

   return ok;
}

static bool sem_check_return(tree_t t)
{
   if (top_scope->subprog == NULL)
      sem_error(t, "return statement not allowed outside subprogram");

   if (tree_has_value(t)) {
      if (tree_kind(top_scope->subprog) == T_PROC_BODY)
         sem_error(t, "cannot return a value from a procedure");

      type_t expect = type_result(tree_type(top_scope->subprog));

      if (!sem_check(tree_value(t)))
         return false;

      if (!sem_check_type(tree_value(t), expect))
         sem_error(t, "expected return type %s but have %s",
                   type_pp(expect), type_pp(tree_type(tree_value(t))));
   }

   return true;
}

static bool sem_check_while(tree_t t)
{
   type_t std_bool = std_type(NULL, "BOOLEAN");

   tree_t value = tree_value(t);
   if (!sem_check(value))
      return false;

   if (!sem_check_type(value, std_bool))
      sem_error(value, "type of loop condition must be %s but is %s",
                type_pp(std_bool), type_pp(tree_type(value)));

   loop_push(tree_ident(t));

   const bool ok = sem_check_stmts(t, tree_stmt, tree_stmts(t));

   loop_pop();

   return ok;
}

static bool sem_check_for(tree_t t)
{
   tree_t r = tree_range(t, 0);
   const bool is_range_expr = (tree_subkind(r) == RANGE_EXPR);
   if (!sem_check_discrete_range(r, NULL))
      return false;

   tree_t idecl = tree_decl(t, 0);

   if (!sem_check_subtype(idecl, tree_type(idecl)))
      return false;

   tree_t name = NULL;
   if (is_range_expr && tree_kind((name = tree_name(tree_value(r)))) == T_REF) {
      // Find the variable X in X'RANGE
      tree_t range_var = tree_ref(name);
      tree_add_attr_tree(idecl, range_var_i, range_var);
   }

   scope_push(tree_ident(t));
   loop_push(tree_ident(t));

   const bool ok = sem_check_stmts(t, tree_stmt, tree_stmts(t));

   loop_pop();
   scope_pop();
   return ok;
}

static bool sem_check_block(tree_t t)
{
   scope_push(tree_ident(t));

   if (!sem_check_map(t, t, tree_ports, tree_port, tree_params, tree_param))
      return false;

   if (!sem_check_map(t, t, tree_generics, tree_generic,
                      tree_genmaps, tree_genmap))
      return false;

   bool ok = true;

   const int ndecls = tree_decls(t);
   for (int i = 0; i < ndecls; i++) {
      tree_t d = tree_decl(t, i);

      if ((ok = sem_check(d) && ok))
         sem_check_static_elab(d);

      if (tree_kind(d) == T_USE)
         tree_add_context(top_scope->unit, d);
   }

   ok = ok && sem_check_stmts(t, tree_stmt, tree_stmts(t));

   scope_pop();
   return ok;
}

static bool sem_check_loop_control(tree_t t)
{
   if (loop_stack == NULL)
      sem_error(t, "cannot use %s outside loop",
                (tree_kind(t) == T_EXIT) ? "exit" : "next");

   if (tree_has_ident2(t)) {
      ident_t label = tree_ident2(t);
      loop_stack_t *it;
      for (it = loop_stack; (it != NULL) && (it->name != label); it = it->up)
         ;

      if (it == NULL)
         sem_error(t, "no nested loop with label %s", istr(label));
   }
   else
      tree_set_ident2(t, loop_stack->name);

   if (tree_has_value(t)) {
      tree_t value = tree_value(t);
      if (!sem_check(value))
         return false;

      type_t std_bool = std_type(NULL, "BOOLEAN");
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

static bool sem_check_attr_spec(tree_t t)
{
   tree_t attr_decl = tree_ref(t);
   type_t type = tree_type(attr_decl);

   tree_t value = tree_value(t);
   if (!sem_check(value))
      return false;

   if (!sem_check_type(value, type))
      sem_error(t, "expected attribute type %s", type_pp(type));

   return true;
}

static bool sem_check_if_generate(tree_t t)
{
   type_t std_bool = std_type(NULL, "BOOLEAN");
   tree_t value = tree_value(t);

   if (!sem_check(value))
      return false;

   if (!type_eq(tree_type(value), std_bool))
      sem_error(value, "condition of generate statement must be BOOLEAN");

   if (!sem_globally_static(value))
      sem_error(value, "condition of generate statement must be static");

   scope_push(NULL);

   bool ok = true;

   const int ndecls = tree_decls(t);
   for (int i = 0; i < ndecls; i++) {
      tree_t d = tree_decl(t, i);
      ok = sem_check(d) && ok;

      if (tree_kind(d) == T_USE)
         tree_add_context(top_scope->unit, d);
   }

   ok = ok && sem_check_stmts(t, tree_stmt, tree_stmts(t));

   scope_pop();
   return ok;
}

static bool sem_check_for_generate(tree_t t)
{
   tree_t r = tree_range(t, 0);
   if (!sem_check_discrete_range(r, NULL))
      return false;

   if (!sem_globally_static(r))
      sem_error(r, "range of generate statement must be static");

   tree_t idecl = tree_decl(t, 0);
   assert(tree_kind(idecl) == T_GENVAR);

   if (!sem_check_subtype(idecl, tree_type(idecl)))
      return false;

   scope_push(NULL);

   bool ok = true;

   const int ndecls = tree_decls(t);
   for (int i = 1; i < ndecls; i++) {
      tree_t d = tree_decl(t, i);
      ok = sem_check(d) && ok;

      if (tree_kind(d) == T_USE)
         tree_add_context(top_scope->unit, d);
   }

   ok = ok && sem_check_stmts(t, tree_stmt, tree_stmts(t));

   scope_pop();
   return ok;
}

static bool sem_check_open(tree_t t)
{
   return true;
}

static bool sem_check_file_decl(tree_t t)
{
   // Rules for file declarations are in LRM 93 section 4.3.1.4

   type_t type = tree_type(t);

   if (type_kind(type) != T_FILE)
      sem_error(t, "file declarations must have file type");

   if (tree_has_value(t)) {
      type_t string = std_type(NULL, "STRING");
      tree_t value = tree_value(t);
      if (!sem_check(value))
         return false;

      if (!sem_check_type(value, string))
         sem_error(value, "file name must have type STRING");

      type_t open_kind = std_type(NULL, "FILE_OPEN_KIND");
      tree_t mode = tree_file_mode(t);
      if (!sem_check(mode))
         return false;

      if (!sem_check_type(mode, open_kind))
         sem_error(mode, "open mode must have type FILE_OPEN_KIND");
   }

   const bool is_pure_func_body =
      top_scope->subprog != NULL
      && tree_kind(top_scope->subprog) == T_FUNC_BODY
      && !(tree_flags(top_scope->subprog) & TREE_F_IMPURE);

   if (is_pure_func_body & !(relax_rules() & RELAX_PURE_FILES))
      sem_error(t, "cannot declare a file object in a pure function");

   return true;
}

static bool sem_check_new(tree_t t)
{
   // Rules for allocators are in LRM 93 section 7.3.6

   tree_t value = tree_value(t);
   type_t access_type = tree_type(t);

   if (type_is_none(access_type))
      return false;

   assert(type_is_access(access_type));
   assert(tree_kind(value) == T_QUALIFIED);

   if (!sem_check(value))
      return false;

   type_t type = tree_type(value);

   if (type_is_none(type))
      return false;

   if (!sem_check_subtype(value, type))
      return false;

   if (!tree_has_value(value) && type_is_unconstrained(type))
      sem_error(t, "unconstrained array type %s not allowed in allocator "
                "expression", type_pp(type));

   if (!type_eq(type, type_access(access_type)))
      sem_error(value, "type of allocator expresion %s does not match "
                "access type %s", type_pp(type),
                type_pp(type_access(access_type)));

   return true;
}

static bool sem_check_all(tree_t t)
{
   tree_t value = tree_value(t);
   if (!sem_check(value))
      return false;

   type_t value_type = tree_type(value);

   if (type_is_none(value_type))
      return false;

   if (!type_is_access(value_type))
      sem_error(value, "expression type %s is not access", type_pp(value_type));

   return true;
}

bool sem_bind(tree_t spec, tree_t inst, tree_t comp)
{
   ident_t cname = tree_ident2(spec);

   if (tree_kind(comp) != T_COMPONENT)
      sem_error(spec, "object %s is not a component declaration", istr(cname));

   if (tree_class(inst) != C_COMPONENT)
      sem_error(spec, "specification may only be used with component "
                "instances");

   if (!tree_has_ref(inst))
      return false;

   if (tree_ref(inst) != comp)
      sem_error(spec, "component mismatch for instance %s: expected %s but "
                "specification has %s", istr(tree_ident(spec)),
                istr(tree_ident(tree_ref(inst))), istr(cname));

   if (tree_has_spec(inst)) {
      tree_t exist = tree_spec(inst);
      if (tree_has_ident(exist))  // Not an OTHERS specification
         sem_error(spec, "instance %s is already bound by a specification",
                   istr(tree_ident(inst)));
   }

   tree_set_spec(inst, spec);
   return true;
}

static bool sem_check_binding(tree_t t)
{
   if (tree_params(t) > 0)
      sem_error(t, "sorry, bindings with port maps are not yet supported");

   if (tree_genmaps(t) > 0)
      sem_error(t, "sorry, bindings with generic maps are not yet supported");

   tree_t unit;
   if (!sem_find_unit(t, tree_ident(t), &unit))
      return false;

   switch (tree_class(t)) {
   case C_ENTITY:
      if (tree_kind(unit) != T_ENTITY)
         sem_error(t, "unit %s is not an entity", istr(tree_ident(t)));
      break;

   case C_CONFIGURATION:
      if (tree_kind(unit) != T_CONFIGURATION)
         sem_error(t, "unit %s is not a configuration", istr(tree_ident(t)));
      break;

   default:
      assert(false);
   }

   return true;
}

static bool sem_check_block_config(tree_t t)
{
   scope_push(tree_ident(t));

   bool ok = true;
   const int ndecls = tree_decls(t);
   for (int i = 0; i < ndecls; i++)
      ok = sem_check(tree_decl(t, i)) && ok;

   scope_pop();
   return ok;
}

static bool sem_copy_instances(tree_t t, void *context)
{
   switch (tree_kind(t)) {
   case T_INSTANCE:
   case T_ARCH:
      return true;
   default:
      return false;
   }
}

static bool sem_check_configuration(tree_t t)
{
   const int ndecls = tree_decls(t);
   tree_t block_config = tree_decl(t, ndecls - 1);
   assert(tree_kind(block_config) == T_BLOCK_CONFIG);

   lib_t work = lib_work();

   ident_t name_qual =
      ident_prefix(ident_prefix(lib_name(work), tree_ident2(t), '.'),
                   tree_ident(block_config), '-');
   tree_t arch = lib_get(work, name_qual);
   if (arch == NULL || tree_kind(arch) != T_ARCH)
      sem_error(t, "architecture %s of entity %s not found in library %s",
                istr(ident_rfrom(name_qual, '-')),
                istr(ident_until(name_qual, '-')),
                istr(lib_name(work)));

   tree_t copy = tree_copy(arch, sem_copy_instances, NULL);
   tree_set_ident(copy, ident_prefix(name_qual, tree_ident(t), '-'));
   lib_put(lib_work(), copy);

   scope_push(NULL);
   top_scope->unit = t;

   scope_push(NULL);

   bool ok = true;
   for (int i = 0; i < ndecls; i++)
      ok = sem_check(tree_decl(t, i)) && ok;

   scope_pop();
   scope_pop();

   return ok;
}

static bool sem_check_prot_body(tree_t t)
{
   // Rules for protected type bodies are in LRM 00 section 3.5.2

   ident_t name = tree_ident(t);
   type_t type = tree_type(t);

   if (type_is_none(type))
      return false;

   if (type_has_body(type))
      sem_error(t, "protected type %s already has body", istr(name));

   type_set_body(type, t);

   scope_push(ident_prefix(top_scope->prefix, name, '.'));
   top_scope->flags |= SCOPE_PROTECTED;

   bool ok = true;
   const int ndecls = tree_decls(t);
   for (int i = 0; i < ndecls; i++) {
      tree_t d = tree_decl(t, i);
      ok = sem_check(d) && ok;
   }

   scope_pop();
   return ok;
}

static bool sem_check_context_decl(tree_t t)
{
   // Context declarations are in LRM 08 section 13.3

   assert(top_scope == NULL);
   scope_push(NULL);

   top_scope->flags |= SCOPE_CONTEXT;

   const bool ok = sem_check_context_clause(t);

   scope_pop();
   return ok;
}

static bool sem_check_context_ref(tree_t t)
{
   if (top_scope->flags & SCOPE_CONTEXT) {
      // LRM 08 section 13.3
      ident_t prefix = ident_until(tree_ident(t), '.');
      if (prefix == work_i)
         sem_error(t, "selected name in context declaration context reference "
                   "may not have WORK as a prefix");
   }

   return true;
}

bool sem_check(tree_t t)
{
   switch (tree_kind(t)) {
   case T_ARCH:
      return sem_check_arch(t);
   case T_PACKAGE:
      return sem_check_package(t);
   case T_ENTITY:
      return sem_check_entity(t);
   case T_TYPE_DECL:
      return sem_check_type_decl(t);
   case T_PORT_DECL:
      return sem_check_port_decl(t);
   case T_SIGNAL_DECL:
   case T_VAR_DECL:
   case T_CONST_DECL:
      return sem_check_decl(t);
   case T_PROCESS:
      return sem_check_process(t);
   case T_VAR_ASSIGN:
      return sem_check_var_assign(t);
   case T_SIGNAL_ASSIGN:
      return sem_check_signal_assign(t);
   case T_FCALL:
   case T_PROT_FCALL:
      return sem_check_fcall(t);
   case T_LITERAL:
      return sem_check_literal(t);
   case T_REF:
      return sem_check_ref(t);
   case T_WAIT:
      return sem_check_wait(t);
   case T_ASSERT:
   case T_CASSERT:
      return sem_check_assert(t);
   case T_QUALIFIED:
      return sem_check_qualified(t);
   case T_FUNC_DECL:
      return sem_check_func_decl(t);
   case T_AGGREGATE:
      return sem_check_aggregate(t);
   case T_ATTR_REF:
      return sem_check_attr_ref(t, false);
   case T_ARRAY_REF:
      return sem_check_array_ref(t);
   case T_ARRAY_SLICE:
      return sem_check_array_slice(t);
   case T_INSTANCE:
      return sem_check_instance(t);
   case T_IF:
      return sem_check_if(t);
   case T_NULL:
      return true;
   case T_PACK_BODY:
      return sem_check_pack_body(t);
   case T_FUNC_BODY:
      return sem_check_func_body(t);
   case T_RETURN:
      return sem_check_return(t);
   case T_CASSIGN:
      return sem_check_cassign(t);
   case T_WHILE:
      return sem_check_while(t);
   case T_ALIAS:
      return sem_check_alias(t);
   case T_FOR:
      return sem_check_for(t);
   case T_PROC_DECL:
      return sem_check_proc_decl(t);
   case T_PROC_BODY:
      return sem_check_proc_body(t);
   case T_BLOCK:
      return sem_check_block(t);
   case T_CASE:
   case T_SELECT:
      return sem_check_case(t);
   case T_EXIT:
   case T_NEXT:
      return sem_check_loop_control(t);
   case T_PCALL:
   case T_CPCALL:
   case T_PROT_PCALL:
      return sem_check_pcall(t);
   case T_ATTR_SPEC:
      return sem_check_attr_spec(t);
   case T_ATTR_DECL:
      return sem_check_attr_decl(t);
   case T_COMPONENT:
      return sem_check_component(t);
   case T_IF_GENERATE:
      return sem_check_if_generate(t);
   case T_FOR_GENERATE:
      return sem_check_for_generate(t);
   case T_OPEN:
      return sem_check_open(t);
   case T_FIELD_DECL:
      return sem_check_field_decl(t);
   case T_FILE_DECL:
      return sem_check_file_decl(t);
   case T_NEW:
      return sem_check_new(t);
   case T_ALL:
      return sem_check_all(t);
   case T_RECORD_REF:
      return sem_check_record_ref(t);
   case T_UNIT_DECL:
      return sem_check_unit_decl(t);
   case T_USE:
      return sem_check_use_clause(t);
   case T_TYPE_CONV:
      return sem_check_conversion(t);
   case T_SPEC:
      return true;
   case T_BINDING:
      return sem_check_binding(t);
   case T_LIBRARY:
      return sem_check_library_clause(t);
   case T_CONFIGURATION:
      return sem_check_configuration(t);
   case T_PROT_BODY:
      return sem_check_prot_body(t);
   case T_CONTEXT:
      return sem_check_context_decl(t);
   case T_CTXREF:
      return sem_check_context_ref(t);
   case T_BLOCK_CONFIG:
      return sem_check_block_config(t);
   default:
      sem_error(t, "cannot check %s", tree_kind_str(tree_kind(t)));
   }
}
