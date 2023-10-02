//
//  Copyright (C) 2013-2022  Nick Gasson
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
#include "eval.h"
#include "hash.h"
#include "ident.h"
#include "jit/jit.h"
#include "lib.h"
#include "lower.h"
#include "option.h"
#include "phase.h"
#include "rt/cover.h"
#include "rt/mspace.h"
#include "tree.h"
#include "type.h"
#include "vcode.h"

#include <assert.h>
#include <stdint.h>
#include <stdlib.h>
#include <stdarg.h>
#include <inttypes.h>
#include <string.h>
#include <math.h>

typedef enum {
   EVAL_WARN    = (1 << 2),
   EVAL_VERBOSE = (1 << 3),
} eval_flags_t;

struct _eval {
   jit_t        *jit;
};

#define ITER_LIMIT 10000

static const char *eval_expr_name(tree_t expr)
{
   const tree_kind_t kind = tree_kind(expr);
   if (kind == T_FCALL)
      return istr(tree_ident(expr));
   else
      return tree_kind_str(kind);
}

static tree_t eval_value_to_tree(jit_scalar_t value, type_t type,
                                 const loc_t *loc)
{
   tree_t tree = NULL;

   if (type_is_enum(type)) {
      type_t base = type_base_recur(type);
      if ((unsigned)value.integer >= type_enum_literals(base))
         fatal_at(loc, "enum position %"PRIi64" out of range for type %s",
                  value.integer, type_pp(base));

      tree_t lit = type_enum_literal(base, value.integer);

      tree = tree_new(T_REF);
      tree_set_ref(tree, lit);
      tree_set_ident(tree, tree_ident(lit));
   }
   else if (type_is_integer(type)) {
      tree = tree_new(T_LITERAL);
      tree_set_subkind(tree, L_INT);
      tree_set_ival(tree, value.integer);
   }
   else if (type_is_real(type)) {
      tree = tree_new(T_LITERAL);
      tree_set_subkind(tree, L_REAL);
      tree_set_dval(tree, value.real);
   }
   else if (type_is_physical(type)) {
      tree = tree_new(T_LITERAL);
      tree_set_subkind(tree, L_PHYSICAL);
      tree_set_ival(tree, value.integer);
   }
   else
      fatal_trace("cannot convert 0x%"PRIx64" to %s", value.integer,
                  type_pp(type));

   tree_set_type(tree, type);
   tree_set_loc(tree, loc);
   return tree;
}

static tree_t eval_do_fold(jit_t *jit, tree_t expr, lower_unit_t *parent,
                           void *context)
{
   vcode_unit_t thunk = lower_thunk(parent, expr);
   if (thunk == NULL)
      return expr;

   const bool verbose = opt_get_verbose(OPT_EVAL_VERBOSE, NULL);

   jit_scalar_t result;
   const bool finished = jit_call_thunk(jit, thunk, &result, context);

   vcode_unit_unref(thunk);
   thunk = NULL;

   if (finished) {
      if (verbose) {
         diag_t *d = diag_new(DIAG_DEBUG, tree_loc(expr));
         diag_printf(d, "evaluating %s returned ", eval_expr_name(expr));
         if (llabs(result.integer) < 1024)
            diag_printf(d, "%"PRIi64, result.integer);
         else
            diag_printf(d, "0x%"PRIx64, result.integer);
         diag_emit(d);
      }

      return eval_value_to_tree(result, tree_type(expr), tree_loc(expr));
   }
   else if (verbose) {
      diag_t *d = diag_new(DIAG_DEBUG, tree_loc(expr));
      diag_printf(d, "failed to evaluate %s", eval_expr_name(expr));
      diag_emit(d);
   }

   return expr;
}

tree_t eval_try_fold(jit_t *jit, tree_t expr, lower_unit_t *parent,
                     void *context)
{
   const bool verbose = opt_get_verbose(OPT_EVAL_VERBOSE, NULL);
   jit_set_silent(jit, !verbose);

   tree_t result = eval_do_fold(jit, expr, parent, context);

   jit_set_silent(jit, false);

   return result;
}

tree_t eval_must_fold(jit_t *jit, tree_t expr, lower_unit_t *parent,
                      void *context)
{
   return eval_do_fold(jit, expr, parent, context);
}

static bool eval_not_possible(tree_t t, const char *why)
{
   if (opt_get_verbose(OPT_EVAL_VERBOSE, NULL))
      warn_at(tree_loc(t), "%s prevents constant folding", why);

   return false;
}

bool eval_possible(tree_t t, unit_registry_t *ur)
{
   switch (tree_kind(t)) {
   case T_FCALL:
      {
         const tree_flags_t flags = tree_flags(t);
         if (!(flags & (TREE_F_LOCALLY_STATIC | TREE_F_GLOBALLY_STATIC)))
            return eval_not_possible(t, "non-static expression");

         tree_t decl = tree_ref(t);
         const subprogram_kind_t kind = tree_subkind(decl);
         if (kind == S_FOREIGN)
            return eval_not_possible(t, "call to foreign function");
         else if (tree_flags(decl) & TREE_F_IMPURE)
            return eval_not_possible(t, "call to impure function");
         else if (kind != S_USER && !is_open_coded_builtin(kind)
                  && unit_registry_get(ur, tree_ident2(decl)) == NULL)
            return eval_not_possible(t, "not yet lowered predef");
         else if (kind == S_USER && !is_package(tree_container(decl)))
            return eval_not_possible(t, "subprogram not in package");

         const int nparams = tree_params(t);
         for (int i = 0; i < nparams; i++) {
            tree_t p = tree_value(tree_param(t, i));
            if (!eval_possible(p, ur))
               return false;
            else if (tree_kind(p) == T_FCALL && type_is_scalar(tree_type(p)))
               return false;  // Would have been folded already if possible
         }

         return true;
      }

   case T_LITERAL:
   case T_STRING:
      return true;

   case T_TYPE_CONV:
      return eval_possible(tree_value(t), ur);

   case T_QUALIFIED:
      return eval_possible(tree_value(t), ur);

   case T_REF:
      {
         tree_t decl = tree_ref(t);
         switch (tree_kind(decl)) {
         case T_UNIT_DECL:
         case T_ENUM_LIT:
            return true;

         case T_CONST_DECL:
            if (tree_has_value(decl))
               return eval_possible(tree_value(decl), ur);
            else
               return false;

         default:
            return eval_not_possible(t, "reference");
         }
      }

   case T_RECORD_REF:
      return eval_possible(tree_value(t), ur);

   case T_ARRAY_REF:
      {
         const int nparams = tree_params(t);
         for (int i = 0; i < nparams; i++) {
            if (!eval_possible(tree_value(tree_param(t, i)), ur))
               return false;
         }

         return eval_possible(tree_value(t), ur);
      }

   case T_AGGREGATE:
      {
         const int nassocs = tree_assocs(t);
         for (int i = 0; i < nassocs; i++) {
            if (!eval_possible(tree_value(tree_assoc(t, i)), ur))
               return false;
         }

         return true;
      }

   case T_ATTR_REF:
      {
         if (tree_subkind(t) == ATTR_USER)
            return eval_not_possible(t, "user defined attribute");

         if (!eval_possible(tree_name(t), ur))
            return false;

         const int nparams = tree_params(t);
         for (int i = 0; i < nparams; i++) {
            if (!eval_possible(tree_value(tree_param(t, i)), ur))
               return false;
         }

         return true;
      }

   default:
      return eval_not_possible(t, tree_kind_str(tree_kind(t)));
   }
}

tree_t eval_case(jit_t *jit, tree_t stmt, lower_unit_t *parent, void *context)
{
   assert(tree_kind(stmt) == T_CASE_GENERATE);

   vcode_unit_t thunk = lower_case_generate_thunk(parent, stmt);

   jit_scalar_t result = { .integer = -1 };
   if (!jit_call_thunk(jit, thunk, &result, context))
      error_at(tree_loc(tree_value(stmt)), "generate expression is not static");

   vcode_unit_unref(thunk);

   if (result.integer == -1)
      return NULL;
   else
      return tree_stmt(stmt, result.integer);
}

void *eval_instance(jit_t *jit, ident_t name, void *context)
{
   jit_handle_t h = jit_lazy_compile(jit, name);
   if (h == JIT_HANDLE_INVALID)
      fatal_trace("failed to compile instance %s", istr(name));

   jit_scalar_t result;
   if (!jit_try_call(jit, h, &result, context, context))
      return NULL;

   return result.pointer;
}
