//
//  Copyright (C) 2013-2021  Nick Gasson
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
#include "vcode.h"
#include "exec.h"

#include <assert.h>
#include <string.h>
#include <stdarg.h>
#include <stdlib.h>
#include <inttypes.h>
#include <math.h>
#include <float.h>

static bool eval_possible(tree_t t, eval_flags_t flags, bool top_level);

static void eval_load_vcode(lib_t lib, tree_t unit, eval_flags_t flags)
{
   ident_t unit_name = tree_ident(unit);

   if (flags & EVAL_VERBOSE)
      notef("loading vcode for %s", istr(unit_name));

   if (!lib_load_vcode(lib, unit_name)) {
      if (flags & EVAL_WARN)
         warnf("cannot load vcode for %s", istr(unit_name));
   }
}

static vcode_unit_t eval_find_unit(ident_t func_name, eval_flags_t flags)
{
   vcode_unit_t vcode = vcode_find_unit(func_name);
   if (vcode == NULL) {
      ident_t strip_type_suffix = ident_until(func_name, "("[0]);
      ident_t unit_name = ident_runtil(strip_type_suffix, '.');
      ident_t lib_name = ident_until(strip_type_suffix, '.');

      lib_t lib;
      if (lib_name != unit_name && (lib = lib_find(lib_name, false)) != NULL) {
         tree_t unit = lib_get(lib, unit_name);
         if (unit != NULL) {
            eval_load_vcode(lib, unit, flags);

            if (tree_kind(unit) == T_PACKAGE) {
               ident_t body_name =
                  ident_prefix(unit_name, ident_new("body"), '-');
               tree_t body = lib_get(lib, body_name);
               if (body != NULL)
                  eval_load_vcode(lib, body, flags);
            }

            vcode = vcode_find_unit(func_name);
         }
      }
   }

   if (vcode == NULL && (flags & EVAL_VERBOSE))
      warnf("could not find vcode for unit %s", istr(func_name));

   return vcode;
}

static bool eval_have_lowered(tree_t func, eval_flags_t flags)
{
   if (is_builtin(tree_subkind(func)))
      return true;
   else if (!tree_has_ident2(func))
      return false;

   ident_t mangled = tree_ident2(func);
   if (eval_find_unit(mangled, flags) == NULL) {
      if (!(flags & EVAL_LOWER))
         return false;
      else if (tree_kind(func) != T_FUNC_BODY)
         return false;

      return lower_func(func) != NULL;
   }
   else
      return true;
}

static bool eval_possible(tree_t t, eval_flags_t flags, bool top_level)
{
   switch (tree_kind(t)) {
   case T_FCALL:
      {
         if (!(flags & EVAL_FCALL))
            return false;
         else if (tree_flags(tree_ref(t)) & TREE_F_IMPURE) {
            if (flags & EVAL_WARN)
               warn_at(tree_loc(t),
                       "impure function call prevents constant folding");
            return false;
         }

         const int nparams = tree_params(t);
         for (int i = 0; i < nparams; i++) {
            tree_t p = tree_value(tree_param(t, i));
            const bool is_fcall = tree_kind(p) == T_FCALL;
            if (top_level && (flags & EVAL_FOLDING) && is_fcall
                && type_is_scalar(tree_type(p)))
               return false;   // Would have been folded already if possible
            else if (is_fcall && !(flags & EVAL_FCALL))
               return false;
            else if (!eval_possible(p, flags, false))
               return false;
         }

         // This can actually lower the function on demand so only call
         // it if we know all the parameters can be evaluated now
         return eval_have_lowered(tree_ref(t), flags);
      }

   case T_LITERAL:
      return true;

   case T_TYPE_CONV:
      return eval_possible(tree_value(t), flags, false);

   case T_QUALIFIED:
      return eval_possible(tree_value(t), flags, false);

   case T_REF:
      {
         tree_t decl = tree_ref(t);
         switch (tree_kind(decl)) {
         case T_UNIT_DECL:
         case T_ENUM_LIT:
            return true;

         case T_CONST_DECL:
            return tree_has_value(decl) &&
               eval_possible(tree_value(decl), flags, false);

         default:
            if (flags & EVAL_WARN)
               warn_at(tree_loc(t), "reference to %s prevents constant folding",
                       istr(tree_ident(t)));
            return false;
         }
      }

   case T_RECORD_REF:
      return eval_possible(tree_value(t), flags, false);

   case T_AGGREGATE:
      {
         const int nassocs = tree_assocs(t);
         for (int i = 0; i < nassocs; i++) {
            if (!eval_possible(tree_value(tree_assoc(t, i)), flags, false))
                return false;
         }

         return true;
      }

   default:
      if (flags & EVAL_WARN)
         warn_at(tree_loc(t), "expression prevents constant folding");
      return false;
   }
}

static bool eval_can_represent_type(type_t type)
{
   if (type_is_scalar(type))
      return true;
   else if (type_is_array(type))
      return eval_can_represent_type(type_elem(type));
   else if (type_is_record(type)) {
      const int nfields = type_fields(type);
      for (int i = 0; i < nfields; i++) {
         if (!eval_can_represent_type(tree_type(type_field(type, i))))
            return false;
      }
      return true;
   }
   else
      return false;
}

tree_t eval(tree_t expr, eval_flags_t flags)
{
   static int verbose_env = -1;
   if (verbose_env == -1)
      verbose_env = getenv("NVC_EVAL_VERBOSE") != NULL;
   if (verbose_env)
      flags |= EVAL_VERBOSE;

   if (flags & EVAL_VERBOSE)
      flags |= EVAL_WARN | EVAL_BOUNDS;

   const tree_kind_t kind = tree_kind(expr);

   type_t type = tree_type(expr);
   if (type_is_array(type))
      return expr;   // TODO: eval for array results
   else if (!eval_can_represent_type(type))
      return expr;
   else if (kind == T_FCALL && (tree_flags(tree_ref(expr)) & TREE_F_IMPURE))
      return expr;
   else if (!eval_possible(expr, flags, true))
      return expr;

   vcode_unit_t thunk = lower_thunk(expr);
   if (thunk == NULL)
      return expr;

   if (flags & EVAL_VERBOSE)
      note_at(tree_loc(expr), "evaluate thunk for %s",
              kind == T_FCALL ? istr(tree_ident(expr)) : tree_kind_str(kind));

   exec_t *ex = exec_new(flags);
   tree_t tree = exec_fold(ex, expr, thunk);
   exec_free(ex);

   vcode_unit_unref(thunk);
   thunk = NULL;

   return tree;
}
