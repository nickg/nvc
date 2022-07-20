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
#include "opt.h"
#include "phase.h"
#include "prim.h"
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

struct _eval {
   eval_flags_t  flags;
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

eval_t *eval_new(eval_flags_t flags)
{
   if (opt_get_verbose(OPT_EVAL_VERBOSE, NULL))
      flags |= EVAL_VERBOSE;

   if (flags & EVAL_VERBOSE)
      flags |= EVAL_WARN;

   eval_t *ex = xcalloc(sizeof(eval_t));
   ex->flags = flags;

   ex->jit = jit_new();
   jit_set_silent(ex->jit, !(flags & EVAL_WARN));
   jit_limit_backedges(ex->jit, ITER_LIMIT);

   return ex;
}

void eval_free(eval_t *ex)
{
   jit_free(ex->jit);
   free(ex);
}

static tree_t eval_do_fold(eval_t *ex, tree_t expr)
{
   vcode_unit_t thunk = lower_thunk(expr);
   if (thunk == NULL)
      return expr;

   jit_scalar_t result;
   const bool finished = jit_call_thunk(ex->jit, thunk, &result);

   vcode_unit_unref(thunk);
   thunk = NULL;

   if (finished) {
      if (ex->flags & EVAL_VERBOSE) {
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
   else if (ex->flags & (EVAL_WARN | EVAL_VERBOSE)) {
      diag_t *d = diag_new(DIAG_DEBUG, tree_loc(expr));
      diag_printf(d, "failed to evaluate %s", eval_expr_name(expr));
      diag_emit(d);
   }

   return expr;
}

tree_t eval_try_fold(eval_t *ex, tree_t expr)
{
   jit_set_silent(ex->jit, true);
   jit_limit_backedges(ex->jit, ITER_LIMIT);

   return eval_do_fold(ex, expr);
}

tree_t eval_must_fold(eval_t *ex, tree_t expr)
{
   jit_set_silent(ex->jit, false);
   jit_limit_backedges(ex->jit, 0);

   return eval_do_fold(ex, expr);
}

void eval_set_lower_fn(eval_t *ex, lower_fn_t fn, void *ctx)
{
   jit_set_lower_fn(ex->jit, fn, ctx);
}

static bool eval_not_possible(eval_t *e, tree_t t, const char *why)
{
   if (e->flags & EVAL_WARN)
      warn_at(tree_loc(t), "%s prevents constant folding", why);

   return false;
}

bool eval_possible(eval_t *e, tree_t t)
{
   switch (tree_kind(t)) {
   case T_FCALL:
      {
         tree_t decl = tree_ref(t);
         const subprogram_kind_t kind = tree_subkind(decl);
         if (kind == S_USER && !(e->flags & EVAL_FCALL))
            return eval_not_possible(e, t, "call to user defined function");
         else if (kind == S_FOREIGN || kind == S_VHPIDIRECT)
            return eval_not_possible(e, t, "call to foreign function");
         else if (tree_flags(decl) & TREE_F_IMPURE)
            return eval_not_possible(e, t, "call to impure function");
         else if (!(tree_flags(t) & TREE_F_GLOBALLY_STATIC))
            return eval_not_possible(e, t, "non-static expression");
         else if (kind != S_USER && !is_open_coded_builtin(kind)
                  && vcode_find_unit(tree_ident2(decl)) == NULL)
            return eval_not_possible(e, t, "not yet lowered predef");

         const int nparams = tree_params(t);
         for (int i = 0; i < nparams; i++) {
            tree_t p = tree_value(tree_param(t, i));
            if (!eval_possible(e, p))
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
      return eval_possible(e, tree_value(t));

   case T_QUALIFIED:
      return eval_possible(e, tree_value(t));

   case T_REF:
      {
         tree_t decl = tree_ref(t);
         switch (tree_kind(decl)) {
         case T_UNIT_DECL:
         case T_ENUM_LIT:
            return true;

         case T_CONST_DECL:
            if (tree_has_value(decl))
               return eval_possible(e, tree_value(decl));
            else if (!(e->flags & EVAL_FCALL))
               return eval_not_possible(e, t, "deferred constant");
            else
               return true;

         default:
            return eval_not_possible(e, t, "reference");
         }
      }

   case T_RECORD_REF:
      return eval_possible(e, tree_value(t));

   case T_ARRAY_REF:
      {
         const int nparams = tree_params(t);
         for (int i = 0; i < nparams; i++) {
            if (!eval_possible(e, tree_value(tree_param(t, i))))
               return false;
         }

         return eval_possible(e, tree_value(t));
      }

   case T_AGGREGATE:
      {
         const int nassocs = tree_assocs(t);
         for (int i = 0; i < nassocs; i++) {
            if (!eval_possible(e, tree_value(tree_assoc(t, i))))
               return false;
         }

         return true;
      }

   case T_ATTR_REF:
      {
         if (tree_subkind(t) == ATTR_USER)
            return eval_not_possible(e, t, "user defined attribute");

         if (!eval_possible(e, tree_name(t)))
            return false;

         const int nparams = tree_params(t);
         for (int i = 0; i < nparams; i++) {
            if (!eval_possible(e, tree_value(tree_param(t, i))))
               return false;
         }

         return true;
      }

   default:
      return eval_not_possible(e, t, tree_kind_str(tree_kind(t)));
   }
}
