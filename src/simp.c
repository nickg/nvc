//
//  Copyright (C) 2011-2023  Nick Gasson
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
#include "array.h"
#include "common.h"
#include "diag.h"
#include "eval.h"
#include "hash.h"
#include "phase.h"
#include "type.h"

#include <assert.h>
#include <string.h>
#include <stdarg.h>
#include <inttypes.h>
#include <stdlib.h>

typedef struct {
   tree_t           top;
   jit_t           *jit;
   unit_registry_t *registry;
   tree_flags_t     eval_mask;
   hash_t          *generics;
} simp_ctx_t;

static tree_t simp_tree(tree_t t, void *context);

static tree_t simp_call_args(tree_t t)
{
   tree_t decl = tree_ref(t);

   const int nparams = tree_params(t);
   const int nports  = tree_ports(decl);

   // Replace named arguments with positional ones

   int last_pos = -1;
   for (int i = 0; i < nparams; i++) {
      if (tree_subkind(tree_param(t, i)) == P_POS)
         last_pos = i;
   }

   if (last_pos == nports - 1)
      return t;


   tree_t new = tree_new(tree_kind(t));
   tree_set_loc(new, tree_loc(t));
   if (tree_has_ident(t))
      tree_set_ident(new, tree_ident(t));
   tree_set_ref(new, tree_ref(t));

   tree_kind_t kind = tree_kind(t);
   if (kind == T_FCALL || kind == T_PROT_FCALL) {
      tree_set_type(new, tree_type(t));
      tree_set_flag(new, tree_flags(t));
   }

   if ((kind == T_PROT_PCALL || kind == T_PROT_FCALL) && tree_has_name(t))
      tree_set_name(new, tree_name(t));

   for (int i = 0; i <= last_pos; i++) {
      tree_t port  = tree_port(decl, i);
      tree_t param = tree_param(t, i);
      tree_t value = tree_value(param);

      if (tree_kind(value) == T_OPEN)
         value = tree_value(port);

      add_param(new, value, P_POS, NULL);
   }

   for (int i = last_pos + 1; i < nports; i++) {
      tree_t port = tree_port(decl, i);

      tree_t agg = NULL;
      bool found = false;
      for (int j = last_pos + 1; j < nparams; j++) {
         tree_t p = tree_param(t, j);
         assert(tree_subkind(p) == P_NAMED);

         tree_t name = tree_name(p);
         const tree_kind_t name_kind = tree_kind(name);
         if (name_kind == T_REF) {
            if (tree_ref(name) == port) {
               tree_t value = tree_value(p);

               if (tree_kind(value) == T_OPEN)
                  value = tree_value(port);

               add_param(new, value, P_POS, NULL);
               found = true;
               break;
            }
         }
         else {
            // Must be a partial association
            tree_t ref = tree_value(name);
            assert(tree_kind(ref) == T_REF);   // Checked by sem
            if (tree_ref(ref) == port) {
               if (agg == NULL) {
                  agg = tree_new(T_AGGREGATE);
                  tree_set_loc(agg, tree_loc(p));
                  tree_set_type(agg, tree_type(port));

                  add_param(new, agg, P_POS, NULL);
               }

               tree_t a = tree_new(T_ASSOC);
               tree_set_loc(a, tree_loc(p));
               tree_set_subkind(a, A_NAMED);
               tree_set_value(a, tree_value(p));

               if (name_kind == T_RECORD_REF)
                  tree_set_name(a, make_ref(tree_ref(name)));
               else
                  tree_set_name(a, tree_value(tree_param(name, 0)));

               tree_add_assoc(agg, a);

               found = true;
            }
         }
      }

      if (!found) {
         assert(tree_has_value(port));  // Checked by sem
         add_param(new, tree_value(port), P_POS, NULL);
      }
   }

   return new;
}

static void simp_generic_subprogram(tree_t t, simp_ctx_t *ctx)
{
   tree_t decl = tree_ref(t);
   if (tree_kind(decl) == T_GENERIC_DECL) {
      tree_t map = hash_get(ctx->generics, tree_ref(t));
      if (map == NULL)
         return;

      if (tree_kind(map) == T_REF)
         map = tree_ref(map);

      assert(is_subprogram(map));
      tree_set_ref(t, map);
   }
}

static tree_t simp_concat(tree_t t)
{
   assert(tree_params(t) == 2);

   tree_t p0 = tree_value(tree_param(t, 0));
   tree_t p1 = tree_value(tree_param(t, 1));

   tree_t p0_enum = NULL, p1_enum = NULL;

   const tree_kind_t p0_kind = tree_kind(p0);
   const tree_kind_t p1_kind = tree_kind(p1);

   // Only handle concatenations of string literals and enumeration
   // literals

   if (p0_kind == T_REF) {
      if (tree_kind((p0_enum = tree_ref(p0))) != T_ENUM_LIT)
         return t;
   }
   else if (p0_kind != T_STRING)
      return t;

   if (p1_kind == T_REF) {
      if (tree_kind((p1_enum = tree_ref(p1))) != T_ENUM_LIT)
         return t;
   }
   else if (p1_kind != T_STRING)
      return t;

   tree_t new = tree_new(T_STRING);
   tree_set_loc(new, tree_loc(t));

   if (p0_enum != NULL)
      tree_add_char(new, make_ref(p0_enum));
   else {
      const int p0_chars = tree_chars(p0);
      for (int i = 0; i < p0_chars; i++)
         tree_add_char(new, tree_char(p0, i));
   }

   if (p1_enum != NULL)
      tree_add_char(new, make_ref(p1_enum));
   else {
      const int p1_chars = tree_chars(p1);
      for (int i = 0; i < p1_chars; i++)
         tree_add_char(new, tree_char(p1, i));
   }

   tree_set_type(new, subtype_for_string(new, tree_type(t)));
   return new;
}

static bool simp_literal_args(tree_t t, int64_t *p0, int64_t *p1)
{
   switch (tree_params(t)) {
   case 2:
      if (!folded_int(tree_value(tree_param(t, 1)), p1))
         return false;
      // Fall-through
   case 1:
      return folded_int(tree_value(tree_param(t, 0)), p0);
   default:
      return false;
   }
}

static tree_t simp_fcall(tree_t t, simp_ctx_t *ctx)
{
   if (standard() >= STD_08 && ctx->generics != NULL)
      simp_generic_subprogram(t, ctx);

   t = simp_call_args(t);

   const subprogram_kind_t kind = tree_subkind(tree_ref(t));
   const tree_flags_t flags = tree_flags(t);

   if (kind == S_CONCAT)
      return simp_concat(t);
   else if (kind != S_USER && kind != S_FOREIGN) {
      // Simplify basic operations on literals without the overhead of
      // generating code
      int64_t p0, p1, result;
      if (simp_literal_args(t, &p0, &p1)) {
         switch (kind) {
         case S_NEGATE:
            if (p0 == INT64_MIN)
               break;
            else
               return get_int_lit(t, NULL, -p0);

         case S_IDENTITY:
            return get_int_lit(t, NULL, p0);

         case S_SCALAR_NOT:
            return get_enum_lit(t, NULL, !p0);

         case S_SCALAR_EQ:
            return get_enum_lit(t, NULL, p0 == p1);

         case S_SCALAR_NEQ:
            return get_enum_lit(t, NULL, p0 != p1);

         case S_SCALAR_GT:
            return get_enum_lit(t, NULL, p0 > p1);

         case S_SCALAR_LT:
            return get_enum_lit(t, NULL, p0 < p1);

         case S_SCALAR_GE:
            return get_enum_lit(t, NULL, p0 >= p1);

         case S_SCALAR_LE:
            return get_enum_lit(t, NULL, p0 <= p1);

         case S_SCALAR_XOR:
            return get_enum_lit(t, NULL, p0 ^ p1);

         case S_SCALAR_XNOR:
            return get_enum_lit(t, NULL, !(p0 ^ p1));

         case S_SCALAR_AND:
            return get_enum_lit(t, NULL, p0 & p1);

         case S_SCALAR_NAND:
            return get_enum_lit(t, NULL, !(p0 & p1));

         case S_SCALAR_OR:
            return get_enum_lit(t, NULL, p0 | p1);

         case S_SCALAR_NOR:
            return get_enum_lit(t, NULL, !(p0 | p1));

         case S_ADD:
            if (__builtin_add_overflow(p0, p1, &result))
               break;
            else
               return get_int_lit(t, NULL, result);

         case S_SUB:
            if (__builtin_sub_overflow(p0, p1, &result))
               break;
            else
               return get_int_lit(t, NULL, result);

         case S_MUL:
            if (__builtin_mul_overflow(p0, p1, &result))
               break;
            else
               return get_int_lit(t, NULL, result);

         case S_DIV:
            if ((p0 == INT64_MIN && p1 == -1) || p1 == 0)
               break;
            else
               return get_int_lit(t, NULL, p0 / p1);

         case S_EXP:
            if (p1 >= 0 && ipow_safe(p0, p1, &result))
               return get_int_lit(t, NULL, result);
            else
               break;

         case S_ABS:
            if (p0 == INT64_MIN)
               break;
            else
               return get_int_lit(t, NULL, llabs(p0));

         default:
            break;
         }
      }
   }

   if (flags & ctx->eval_mask) {
      // Only evaluate non-scalar expressions if they are locally-static
      if (!(flags & TREE_F_LOCALLY_STATIC) && !type_is_scalar(tree_type(t)))
         return t;
      else if (!eval_possible(t, ctx->registry))
         return t;

      return eval_try_fold(ctx->jit, t, NULL, NULL);
   }

   return t;
}

static tree_t simp_type_conv(tree_t t, simp_ctx_t *ctx)
{
   type_t type = tree_type(t);
   if (type_is_array(type)) {
      type_t elem = type_elem(type);
      if (dimension_of(type) > 1 || !type_is_scalar(elem))
         return t;   // Not supported currently
   }

   if (eval_possible(t, ctx->registry))
      return eval_try_fold(ctx->jit, t, NULL, NULL);

   return t;
}

static tree_t simp_pcall(tree_t t, simp_ctx_t *ctx)
{
   if (standard() >= STD_08 && ctx->generics != NULL)
      simp_generic_subprogram(t, ctx);

   return simp_call_args(t);
}

static tree_t simp_record_ref(tree_t t, simp_ctx_t *ctx)
{
   tree_t value = tree_value(t);
   if (tree_kind(value) == T_AGGREGATE) {
      ident_t field = tree_ident(t);
      type_t type = tree_type(value);

      const int nassocs = tree_assocs(value);
      for (int i = 0; i < nassocs; i++) {
         tree_t a = tree_assoc(value, i);
         switch (tree_subkind(a)) {
         case A_POS:
            if (tree_ident(type_field(type, tree_pos(a))) == field)
               return tree_value(a);
            break;

         case A_NAMED:
            if (tree_ident(tree_name(a)) == field)
               return tree_value(a);
            break;
         }
      }
   }

   return t;
}

static tree_t simp_ref(tree_t t, simp_ctx_t *ctx)
{
   tree_t decl = tree_ref(t);

   switch (tree_kind(decl)) {
   case T_CONST_DECL:
      if (tree_flags(t) & TREE_F_FORMAL_NAME)
         return t;
      else if (!type_is_scalar(tree_type(decl)))
         return t;
      else if (tree_has_value(decl)) {
         tree_t value = tree_value(decl);
         switch (tree_kind(value)) {
         case T_LITERAL:
            return value;
         case T_REF:
            if (tree_kind(tree_ref(value)) == T_ENUM_LIT)
               return value;
            // Fall-through
         default:
            return t;
         }
      }
      else
         return t;

   case T_UNIT_DECL:
      return tree_value(decl);

   case T_PORT_DECL:
   case T_GENERIC_DECL:
      if (ctx->generics != NULL) {
         tree_t map = hash_get(ctx->generics, decl);
         if (map != NULL) {
            switch (tree_kind(map)) {
            case T_LITERAL:
            case T_STRING:
            case T_AGGREGATE:
            case T_ARRAY_SLICE:
            case T_ARRAY_REF:
            case T_FCALL:
            case T_RECORD_REF:
            case T_OPEN:
            case T_QUALIFIED:
            case T_TYPE_CONV:
            case T_ATTR_REF:
            case T_REF:
            case T_TYPE_REF:
               // Do not rewrite references to non-references if they appear
               // as formal names
               if (tree_flags(t) & TREE_F_FORMAL_NAME)
                  break;
               return map;
            case T_PORT_DECL:
               tree_set_ref(t, map);
               tree_set_type(t, tree_type(map));
               return t;
            default:
               fatal_trace("cannot rewrite generic %s to tree kind %s",
                           istr(tree_ident(t)), tree_kind_str(tree_kind(map)));
            }
         }
      }
      return t;

   default:
      return t;
   }
}

static tree_t simp_convert_range_bound(tree_t expr, tree_t where)
{
   type_t type = tree_type(where);
   if (type_eq(tree_type(expr), type))
      return expr;

   tree_t conv = tree_new(T_TYPE_CONV);
   tree_set_loc(conv, tree_loc(where));
   tree_set_value(conv, expr);
   tree_set_type(conv, type);

   return conv;
}

static tree_t simp_attr_ref(tree_t t, simp_ctx_t *ctx)
{
   if (tree_has_value(t))
      return tree_value(t);

   const attr_kind_t predef = tree_subkind(t);
   switch (predef) {
   case ATTR_POS:
      {
         tree_t value = tree_value(tree_param(t, 0));

         int64_t ipos;
         if (folded_int(value, &ipos))
            return get_int_lit(t, NULL, ipos);

         return t;
      }

   case ATTR_LENGTH:
   case ATTR_LEFT:
   case ATTR_LOW:
   case ATTR_HIGH:
   case ATTR_RIGHT:
   case ATTR_ASCENDING:
      {
         tree_t name = tree_name(t);
         tree_kind_t name_kind = tree_kind(name);

         if (name_kind == T_ATTR_REF) {
            // Try to rewrite expressions like X'RANGE(1)'LEFT to X'LEFT(1)
            attr_kind_t prefix_predef = tree_subkind(name);
            if (prefix_predef == ATTR_RANGE || prefix_predef == ATTR_BASE
                || prefix_predef == ATTR_REVERSE_RANGE) {
               tree_t new = tree_new(T_ATTR_REF);
               tree_set_loc(new, tree_loc(t));
               tree_set_name(new, tree_name(name));
               tree_set_ident(new, tree_ident(t));
               tree_set_subkind(new, predef);
               tree_set_type(new, tree_type(t));

               const int nparams = tree_params(name);
               for (int i = 0; i < nparams; i++)
                  tree_add_param(new, tree_param(name, i));

               t = new;
               name = tree_name(new);
               name_kind = tree_kind(name);
            }
         }

         type_t type = tree_type(name);
         int64_t dim_i = 1;

         if (type_kind(type) == T_ENUM) {
            // Enumeration subtypes are handled below
            const int nlits = type_enum_literals(type);

            switch (predef) {
            case ATTR_LEFT:
            case ATTR_LOW:
               return make_ref(type_enum_literal(type, 0));
            case ATTR_RIGHT:
            case ATTR_HIGH:
               return make_ref(type_enum_literal(type, nlits - 1));
            case ATTR_ASCENDING:
               return get_enum_lit(t, NULL, true);
            case ATTR_LENGTH:
               return get_int_lit(t, NULL, nlits);
            default:
               fatal_trace("invalid enumeration attribute %d", predef);
            }
         }
         else if (type_is_array(type)) {
            if (tree_params(t) > 0) {
               tree_t value = tree_value(tree_param(t, 0));
               if (!folded_int(value, &dim_i))
                  fatal_at(tree_loc(value), "locally static dimension "
                           "expression was not folded");
            }

            if (name_kind == T_REF
                && tree_kind(tree_ref(name)) == T_TYPE_DECL
                && type_kind(type) == T_ARRAY) {

               // Get index type of unconstrained array

               if (dim_i < 1 || dim_i > type_indexes(type))
                  return t;

               type  = type_index(type, dim_i - 1);
               dim_i = 1;
            }
            else if (type_is_unconstrained(type))
               return t;
            else if (dim_i < 1 || dim_i > dimension_of(type))
               return t;
         }

         tree_t r = range_of(type, dim_i - 1);

         const range_kind_t rkind = tree_subkind(r);
         if (rkind != RANGE_TO && rkind != RANGE_DOWNTO)
            return t;

         switch (predef) {
         case ATTR_LENGTH:
            {
               int64_t low, high, length = 0;
               if (folded_bounds(r, &low, &high)) {
                  if (high < low)
                     return get_int_lit(t, NULL, 0);
                  else {
                     bool overflow = false;
                     overflow |= __builtin_sub_overflow(high, low, &length);
                     overflow |= __builtin_add_overflow(length, 1, &length);

                     if (overflow)
                        error_at(tree_loc(t), "value of LENGTH attribute "
                                 "exceeds universal integer range");
                     else
                        return get_int_lit(t, NULL, length);
                  }
               }

               return t;
            }

         case ATTR_LOW:
            if (tree_subkind(r) == RANGE_TO)
               return simp_convert_range_bound(tree_left(r), t);
            else
               return simp_convert_range_bound(tree_right(r), t);
         case ATTR_HIGH:
            if (tree_subkind(r) == RANGE_TO)
               return simp_convert_range_bound(tree_right(r), t);
            else
               return simp_convert_range_bound(tree_left(r), t);
         case ATTR_LEFT:
            return simp_convert_range_bound(tree_left(r), t);
         case ATTR_RIGHT:
            return simp_convert_range_bound(tree_right(r), t);
         case ATTR_ASCENDING:
            return get_enum_lit(t, NULL, (rkind == RANGE_TO));
         default:
            return t;
         }
      }

   default:
      return t;
   }
}

static void simp_build_wait_cb(tree_t expr, void *ctx)
{
   tree_t wait = ctx;

   if (tree_kind(expr) == T_REF) {
      // Check for duplicates
      tree_t decl = tree_ref(expr);
      const int ntriggers = tree_triggers(wait);
      for (int i = 0; i < ntriggers; i++) {
         tree_t t = tree_trigger(wait, i);
         if (tree_kind(t) == T_REF && tree_ref(t) == decl)
            return;
      }
   }

   tree_add_trigger(wait, expr);
}

static tree_t simp_process(tree_t t)
{
   // Replace sensitivity list with a "wait on" statement
   const int ntriggers = tree_triggers(t);
   if (ntriggers > 0) {
      const int nstmts = tree_stmts(t);
      if (nstmts == 0)
         return NULL;   // Body was optimised away

      tree_t p = tree_new(T_PROCESS);
      tree_set_ident(p, tree_ident(t));
      tree_set_loc(p, tree_loc(t));
      tree_set_flag(p, tree_flags(t));

      const int ndecls = tree_decls(t);
      for (int i = 0; i < ndecls; i++)
         tree_add_decl(p, tree_decl(t, i));

      for (int i = 0; i < nstmts; i++)
         tree_add_stmt(p, tree_stmt(t, i));

      tree_t w = tree_new(T_WAIT);
      tree_set_ident(w, tree_ident(p));
      tree_set_flag(w, TREE_F_STATIC_WAIT);

      if (ntriggers == 1 && tree_kind(tree_trigger(t, 0)) == T_ALL)
         build_wait(t, simp_build_wait_cb, w);
      else {
         for (int i = 0; i < ntriggers; i++)
            tree_add_trigger(w, tree_trigger(t, i));
      }

      tree_add_stmt(p, w);

      return p;
   }

   // Delete processes that contain just a single wait statement
   if (tree_stmts(t) == 1 && tree_kind(tree_stmt(t, 0)) == T_WAIT)
      return NULL;
   else
      return t;
}

static tree_t simp_wait(tree_t t)
{
   // LRM 93 section 8.1
   // If there is no sensitivity list supplied generate one from the
   // condition clause

   if (tree_has_value(t) && tree_triggers(t) == 0)
      build_wait(tree_value(t), simp_build_wait_cb, t);

   return t;
}

static tree_t simp_case(tree_t t)
{
   const int nstmts = tree_stmts(t);
   if (nstmts == 0)
      return NULL;    // All choices are unreachable

   int64_t ival;
   if (!folded_int(tree_value(t), &ival))
      return t;

   for (int i = 0; i < nstmts; i++) {
      tree_t alt = tree_stmt(t, i);

      const int nassocs = tree_assocs(alt);
      for (int j = 0; j < nassocs; j++) {
         tree_t a = tree_assoc(alt, j);
         switch (tree_subkind(a)) {
         case A_NAMED:
            {
               int64_t aval;
               if (!folded_int(tree_name(a), &aval))
                  continue;
               else if (ival != aval)
                  continue;
            }
            break;

         case A_RANGE:
            {
               int64_t low, high;
               if (!folded_bounds(tree_range(a, 0), &low, &high))
                  continue;
               else if (ival < low || ival > high)
                  continue;
            }

         case A_OTHERS:
            break;
         }

         // This choice is always executed
         if (tree_stmts(alt) > 0) {
            const tree_kind_t kind =
               tree_kind(t) == T_CASE_GENERATE ? T_BLOCK : T_SEQUENCE;
            tree_t seq = tree_new(kind);
            tree_set_loc(seq, tree_loc(alt));
            if (tree_has_ident(alt))
               tree_set_ident(seq, tree_ident(alt));
            else if (tree_has_ident(t))
               tree_set_ident(seq, tree_ident(t));

            const int nstmts = tree_stmts(alt);
            for (int i = 0; i < nstmts; i++)
               tree_add_stmt(seq, tree_stmt(alt, i));

            return seq;
         }
         else
            return NULL;
      }
   }

   return NULL;  // No choices can be executed
}

static bool simp_has_drivers(tree_t t)
{
   switch (tree_kind(t)) {
   case T_SIGNAL_ASSIGN:
      return true;
   case T_VAR_ASSIGN:
   case T_WAIT:
   case T_NEXT:
   case T_EXIT:
   case T_FORCE:
   case T_RELEASE:
   case T_RETURN:
      return false;
   case T_IF:
      {
         const int nconds = tree_conds(t);
         for (int i = 0; i < nconds; i++) {
            if (simp_has_drivers(tree_cond(t, i)))
               return true;
         }
         return false;
      }
   case T_CASE:
   case T_MATCH_CASE:
   case T_WHILE:
   case T_COND_STMT:
   case T_SEQUENCE:
      {
         const int nstmts = tree_stmts(t);
         for (int i = 0; i < nstmts; i++) {
            if (simp_has_drivers(tree_stmt(t, i)))
               return true;
         }
         return false;
      }
   default:
      return true;   // Conservative
   }
}

static tree_t simp_cond(tree_t t)
{
   if (tree_has_value(t)) {
      bool value_b;
      if (folded_bool(tree_value(t), &value_b)) {
         if (!tree_has_ident(t) && simp_has_drivers(t)) {
            // The statement part contains drivers that we cannot remove
            // without changing the behaviour
            return t;
         }
         else if (value_b) {
            // Always true, remove the test
            tree_set_value(t, NULL);
            return t;
         }
         else {
            // Always false, delete the condition
            return NULL;
         }
      }
   }

   return t;
}

static tree_t simp_if(tree_t t)
{
   if (tree_conds(t) == 0)
      return NULL;   // All conditions were false

   tree_t c0 = tree_cond(t, 0);
   if (!tree_has_value(c0)) {
      // If statement always executes so replace with then part
      if (tree_stmts(c0) == 1)
         return tree_stmt(c0, 0);
      else {
         tree_t b = tree_new(T_SEQUENCE);
         tree_set_loc(b, tree_loc(t));
         if (tree_has_ident(t))
            tree_set_ident(b, tree_ident(t));

         const int nstmts = tree_stmts(c0);
         for (int i = 0; i < nstmts; i++)
            tree_add_stmt(b, tree_stmt(c0, i));
         return b;
      }
   }

   return t;
}

static tree_t simp_while(tree_t t)
{
   bool value_b;
   if (!tree_has_value(t))
      return t;
   else if (folded_bool(tree_value(t), &value_b) && !value_b
            && !simp_has_drivers(t)) {
      // Condition is false so loop never executes
      return NULL;
   }
   else
      return t;
}

static tree_t simp_guard(tree_t container, tree_t t, tree_t wait, tree_t s0)
{
   // See LRM 93 section 9.3

   tree_t g_if = tree_new(T_IF);
   tree_set_ident(g_if, ident_new("guard_if"));
   tree_set_loc(g_if, tree_loc(t));

   tree_t c0 = tree_new(T_COND_STMT);
   tree_add_cond(g_if, c0);

   tree_t guard_ref = tree_guard(t);
   tree_set_value(c0, guard_ref);
   tree_add_trigger(wait, guard_ref);

   // TODO: handle disconnection specifications here
   //
   // For now just use the default "disconnect S : T after 0 ns;"

   if (tree_kind(s0) == T_SIGNAL_ASSIGN) {
      tree_t target = tree_target(s0);
      tree_t ref = name_to_ref(target);
      if (ref != NULL && is_guarded_signal(tree_ref(ref))) {
         tree_t d = tree_new(T_SIGNAL_ASSIGN);
         tree_set_loc(d, tree_loc(t));
         tree_set_target(d, tree_target(s0));

         tree_t w0 = tree_new(T_WAVEFORM);
         tree_set_loc(w0, tree_loc(t));

         tree_add_waveform(d, w0);

         tree_t c1 = tree_new(T_COND_STMT);
         tree_add_cond(g_if, c1);

         tree_add_stmt(c1, d);
      }
   }

   tree_add_stmt(container, g_if);
   return c0;
}

static tree_t simp_concurrent(tree_t t)
{
   if (tree_stmts(t) == 0)
      return NULL;   // Body was optimised out

   // Replace concurrent statements with a process

   tree_t p = tree_new(T_PROCESS);
   tree_set_ident(p, tree_ident(t));
   tree_set_loc(p, tree_loc(t));
   tree_set_flag(p, tree_flags(t));

   tree_t s0 = tree_stmt(t, 0);

   tree_t w = tree_new(T_WAIT);

   if (tree_kind(s0) == T_PCALL) {
      // Concurrent procedure calls may have internal waits
      tree_t decl = tree_ref(s0);
      if (tree_flags(decl) & TREE_F_NEVER_WAITS)
         tree_set_flag(w, TREE_F_STATIC_WAIT);
   }
   else
      tree_set_flag(w, TREE_F_STATIC_WAIT);

   tree_t container = p;  // Where to add new statements

   if (tree_has_guard(t))
      container = simp_guard(container, t, w, s0);

   tree_add_stmt(container, s0);

   build_wait(s0, simp_build_wait_cb, w);

   tree_add_stmt(p, w);
   return p;
}

static tree_t simp_cond_assign(tree_t t)
{
   const int nconds = tree_conds(t);
   tree_t c0 = tree_cond(t, 0);

   if (nconds == 1 && !tree_has_value(c0))
      return tree_stmt(c0, 0);
   else {
      tree_t s = tree_new(T_IF);
      tree_set_loc(s, tree_loc(t));

      for (int i = 0; i < nconds; i++)
         tree_add_cond(s, tree_cond(t, i));

      return s;
   }
}

static tree_t simp_select(tree_t t)
{
   // Replace a select statement with a case statement

   const tree_kind_t kind =
      tree_kind(t) == T_MATCH_SELECT ? T_MATCH_CASE : T_CASE;

   tree_t c = tree_new(kind);
   tree_set_loc(c, tree_loc(t));
   tree_set_value(c, tree_value(t));

   const int nstmts = tree_stmts(t);
   for (int i = 0; i < nstmts; i++)
      tree_add_stmt(c, tree_stmt(t, i));

   return c;
}

static tree_t simp_context_ref(tree_t t, simp_ctx_t *ctx)
{
   tree_t decl = tree_ref(t);

   const int nctx = tree_contexts(decl);
   for (int i = 2; i < nctx; i++)
      tree_add_context(ctx->top, tree_context(decl, i));

   return NULL;
}

static tree_t simp_assert(tree_t t)
{
   bool value_b;
   if (!tree_has_value(t))
      return t;
   else if (folded_bool(tree_value(t), &value_b) && value_b) {
      // Assertion always passes
      return NULL;
   }
   else
      return t;
}

static tree_t simp_if_generate(tree_t t)
{
   if (tree_conds(t) == 0)
      return NULL;   // All conditions were false

   tree_t c0 = tree_cond(t, 0);
   if (!tree_has_value(c0)) {
      // First condition is always true
      tree_t b = tree_new(T_BLOCK);
      tree_set_loc(b, tree_loc(c0));
      tree_set_ident(b, tree_ident(c0));

      const int ndecls = tree_decls(c0);
      for (int i = 0; i < ndecls; i++)
         tree_add_decl(b, tree_decl(c0, i));

      const int nstmts = tree_stmts(c0);
      for (int i = 0; i < nstmts; i++)
         tree_add_stmt(b, tree_stmt(c0, i));

      return b;
   }

   return t;
}

static tree_t simp_signal_assign(tree_t t)
{
   tree_t target = tree_target(t);

   if (tree_kind(target) == T_OPEN)
      return NULL;    // Delete it

   return t;
}

static tree_t simp_var_assign(tree_t t)
{
   tree_t value = tree_value(t);
   if (tree_kind(value) == T_COND_VALUE) {
      // Replace with an if statement
      tree_t new = tree_new(T_IF);
      tree_set_loc(new, tree_loc(t));

      const int nconds = tree_conds(value);
      for (int i = 0; i < nconds; i++) {
         tree_t e = tree_cond(value, i);

         tree_t c = tree_new(T_COND_STMT);
         tree_set_loc(c, tree_loc(e));

         if (tree_has_value(e))
            tree_set_value(c, tree_value(e));
         else
            assert(i == nconds - 1);

         tree_t s = tree_new(T_VAR_ASSIGN);
         tree_set_loc(s, tree_loc(t));
         tree_set_target(s, tree_target(t));
         tree_set_value(s, tree_result(e));

         tree_add_stmt(c, s);
         tree_add_cond(new, c);
      }

      return new;
   }

   return t;
}

static tree_t simp_return(tree_t t)
{
   if (!tree_has_value(t))
      return t;

   tree_t value = tree_value(t);
   if (tree_kind(value) == T_COND_VALUE) {
      // Replace with an if statement
      tree_t new = tree_new(T_IF);
      tree_set_loc(new, tree_loc(t));

      const int nconds = tree_conds(value);
      for (int i = 0; i < nconds; i++) {
         tree_t e = tree_cond(value, i);

         tree_t c = tree_new(T_COND_STMT);
         tree_set_loc(c, tree_loc(e));

         if (tree_has_value(e))
            tree_set_value(c, tree_value(e));
         else
            assert(i == nconds - 1);

         tree_t s = tree_new(T_RETURN);
         tree_set_loc(s, tree_loc(tree_result(e)));
         tree_set_value(s, tree_result(e));

         tree_add_stmt(c, s);
         tree_add_cond(new, c);
      }

      return new;
   }

   return t;
}

static tree_t simp_cond_return(tree_t t)
{
   tree_t value = tree_value(t);

   bool folded;
   if (folded_bool(tree_value(t), &folded)) {
      if (folded) {
         tree_t new = tree_new(T_RETURN);
         tree_set_loc(new, tree_loc(t));
         return new;
      }
      else
         return NULL;
   }

   // Replace with an if statement
   tree_t new = tree_new(T_IF);
   tree_set_loc(new, tree_loc(t));

   tree_t c = tree_new(T_COND_STMT);
   tree_set_loc(c, tree_loc(t));
   tree_set_value(c, value);

   tree_t s = tree_new(T_RETURN);
   tree_set_loc(s, tree_loc(t));

   tree_add_stmt(c, s);
   tree_add_cond(new, c);

   return new;
}

static tree_t simp_literal(tree_t t)
{
   switch (tree_subkind(t)) {
   case L_PHYSICAL:
      // Rewrite in terms of the base unit
      if (tree_has_ref(t)) {
         tree_t decl = tree_ref(t);
         int64_t base = assume_int(tree_value(decl));

         // TODO: check for overflow here
         if (tree_ival(t) == 0)
            tree_set_ival(t, tree_dval(t) * base);
         else
            tree_set_ival(t, tree_ival(t) * base);

         tree_set_ref(t, NULL);
         tree_set_ident(t, tree_ident(decl));
      }
      return t;

   default:
      return t;
   }
}

static tree_t simp_range(tree_t t)
{
   if (tree_subkind(t) != RANGE_EXPR)
      return t;

   tree_t value = tree_value(t);
   assert(tree_kind(value) == T_ATTR_REF);

   const attr_kind_t attr = tree_subkind(value);
   assert(attr == ATTR_RANGE || attr == ATTR_REVERSE_RANGE);

   tree_t name = tree_name(value);

   type_t type = tree_type(name);
   if (type_is_unconstrained(type))
      return t;

   int dim = 0;
   if (tree_params(value) > 0) {
      int64_t ival;
      if (!folded_int(tree_value(tree_param(value, 0)), &ival))
         return t;
      dim = ival - 1;
   }

   if (attr == ATTR_REVERSE_RANGE) {
      tree_t base_r = range_of(type, dim);
      const range_kind_t base_kind = tree_subkind(base_r);
      if (base_kind == RANGE_EXPR)
         return t;
      else {
         tree_t rev = tree_new(T_RANGE);
         tree_set_subkind(rev, base_kind ^ 1);
         tree_set_loc(rev, tree_loc(t));
         tree_set_type(rev, tree_type(t));
         tree_set_left(rev, tree_right(base_r));
         tree_set_right(rev, tree_left(base_r));

         return rev;
      }
   }
   else
      return range_of(type, dim);
}

static tree_t simp_cond_expr(tree_t t)
{
   if (!tree_has_result(t))
      return NULL;   // "unaffected" expression is redundant
   else if (tree_has_value(t)) {
      bool value_b;
      if (folded_bool(tree_value(t), &value_b)) {
         if (value_b) {
            // Always true, remove the test
            tree_set_value(t, NULL);
            return t;
         }
         else {
            // Always false, delete the condition
            return NULL;
         }
      }
   }

   return t;
}

static tree_t simp_cond_value(tree_t t)
{
   tree_t c0 = tree_cond(t, 0);
   if (!tree_has_value(c0)) {
      // Always evaluates to "else" condition
      return tree_result(c0);
   }

   return t;
}

static tree_t simp_hidden_decl(tree_t decl, simp_ctx_t *ctx)
{
   // Remove predefined operators which are hidden by explicitly defined
   // operators in the same region

   const tree_flags_t flags = tree_flags(decl);
   if ((flags & TREE_F_PREDEFINED) && (flags & TREE_F_HIDDEN))
      return NULL;

   return decl;
}

static void simp_generic_map(tree_t t, tree_t unit)
{
   switch (tree_kind(unit)) {
   case T_CONFIGURATION:
   case T_ARCH:
      unit = tree_primary(unit);
      break;
   default:
      break;
   }

   const int ngenmaps = tree_genmaps(t);
   const int ngenerics = tree_generics(unit);

   int last_pos = 0;
   for (; last_pos < ngenmaps; last_pos++) {
      if (tree_subkind(tree_genmap(t, last_pos)) != P_POS)
         break;
   }

   if (last_pos == ngenmaps && ngenmaps == ngenerics)
      return;

   const tree_kind_t kind = tree_kind(t);

   SCOPED_A(tree_t) values = AINIT;

   for (int i = last_pos; i < ngenerics; i++) {
      tree_t g = tree_generic(unit, i), value = NULL;
      ident_t ident = tree_ident(g);

      for (int j = last_pos; j < ngenmaps; j++) {
         tree_t mj = tree_genmap(t, j);

         if (tree_subkind(mj) == P_POS) {
            // This was added by the parser or checker
            if (tree_pos(mj) == i)
               value = tree_value(mj);
            continue;
         }

         tree_t name = tree_name(mj);
         tree_t ref = name_to_ref(name);
         if (ref == NULL || tree_ident(ref) != ident)
            continue;

         switch (tree_kind(name)) {
         case T_REF:
            assert(value == NULL);
            value = tree_value(mj);
            break;

         case T_ARRAY_REF:
         case T_RECORD_REF:
            {
               tree_t a = tree_new(T_ASSOC);
               tree_set_loc(a, tree_loc(mj));
               tree_set_subkind(a, A_NAMED);
               tree_set_value(a, tree_value(mj));

               if (tree_kind(name) == T_ARRAY_REF)
                  tree_set_name(a, tree_value(tree_param(name, 0)));
               else
                  tree_set_name(a, make_ref(find_record_field(name)));

               if (value == NULL) {
                  value = tree_new(T_AGGREGATE);
                  tree_set_loc(value, tree_loc(mj));
                  tree_set_type(value, tree_type(g));
               }
               else
                  assert(tree_kind(value) == T_AGGREGATE);

               tree_add_assoc(value, a);
            }
            break;

         default:
            fatal_at(tree_loc(name), "sorry, this form of generic map is not "
                     "yet supported");
            break;
         }
      }

      if (value == NULL) {
         if (kind == T_BINDING || !tree_has_value(g)) {
            value = tree_new(T_OPEN);
            tree_set_loc(value, tree_loc(t));
            tree_set_type(value, tree_type(g));
         }
         else
            value = tree_value(g);
      }

      APUSH(values, value);
   }

   for (int i = 0; i < values.count; i++) {
      tree_t m;
      if (last_pos + i < ngenmaps)
         m = tree_genmap(t, last_pos + i);
      else {
         m = tree_new(T_PARAM);
         tree_add_genmap(t, m);
      }

      tree_set_subkind(m, P_POS);
      tree_set_pos(m, last_pos + i);
      tree_set_value(m, values.items[i]);
      tree_set_loc(m, tree_loc(values.items[i]));
   }

   if (last_pos + values.count < ngenmaps)
      tree_trim_genmaps(t, last_pos + values.count);
}

static tree_t simp_tree(tree_t t, void *_ctx)
{
   simp_ctx_t *ctx = _ctx;

   switch (tree_kind(t)) {
   case T_PROCESS:
      return simp_process(t);
   case T_ATTR_REF:
      return simp_attr_ref(t, ctx);
   case T_FCALL:
   case T_PROT_FCALL:
      return simp_fcall(t, ctx);
   case T_PCALL:
   case T_PROT_PCALL:
      return simp_pcall(t, ctx);
   case T_REF:
      return simp_ref(t, ctx);
   case T_IF:
      return simp_if(t);
   case T_CASE:
   case T_CASE_GENERATE:
      return simp_case(t);
   case T_WHILE:
      return simp_while(t);
   case T_CONCURRENT:
      return simp_concurrent(t);
   case T_COND_ASSIGN:
      return simp_cond_assign(t);
   case T_SELECT:
   case T_MATCH_SELECT:
      return simp_select(t);
   case T_WAIT:
      return simp_wait(t);
   case T_NULL:
      return NULL;   // Delete it
   case T_RECORD_REF:
      return simp_record_ref(t, ctx);
   case T_CONTEXT_REF:
      return simp_context_ref(t, ctx);
   case T_ASSERT:
      return simp_assert(t);
   case T_IF_GENERATE:
      return simp_if_generate(t);
   case T_SIGNAL_ASSIGN:
      return simp_signal_assign(t);
   case T_VAR_ASSIGN:
      return simp_var_assign(t);
   case T_RETURN:
      return simp_return(t);
   case T_COND_RETURN:
      return simp_cond_return(t);
   case T_TYPE_CONV:
      return simp_type_conv(t, ctx);
   case T_LITERAL:
      return simp_literal(t);
   case T_RANGE:
      return simp_range(t);
   case T_FUNC_DECL:
   case T_PROC_DECL:
   case T_GENERIC_DECL:
      return simp_hidden_decl(t, ctx);
   case T_INSTANCE:
   case T_BINDING:
   case T_PACKAGE_MAP:
      simp_generic_map(t, tree_ref(t));
      return t;
   case T_BLOCK:
      simp_generic_map(t, t);
      return t;
   case T_COND_STMT:
      return simp_cond(t);
   case T_COND_EXPR:
      return simp_cond_expr(t);
   case T_COND_VALUE:
      return simp_cond_value(t);
   case T_PACK_INST:
   case T_FUNC_INST:
   case T_PROC_INST:
      simp_generic_map(t, t);
      return t;
   case T_PACKAGE:
      if (!is_uninstantiated_package(t))
         simp_generic_map(t, t);
      return t;
   default:
      return t;
   }
}

static type_t simp_type(type_t type, void *__ctx)
{
   simp_ctx_t *ctx = __ctx;

   // Replace generic types with the concrete type from the generic map

   if (type_kind(type) != T_GENERIC)
      return type;

   type_t map = NULL;
   if (ctx->generics != NULL)
      map = hash_get(ctx->generics, type);

   return map ?: type;
}

static void simp_generics(tree_t t, simp_ctx_t *ctx)
{
   const int ngenerics = tree_generics(t);
   for (int i = 0; i < ngenerics; i++) {
      tree_t g = tree_generic(t, i);

      tree_t map = find_generic_map(t, i, g);
      if (map == NULL)
         continue;

      if (tree_kind(map) == T_OPEN)
         map = tree_value(g);

      if (ctx->generics == NULL)
         ctx->generics = hash_new(128);

      hash_put(ctx->generics, g, map);
   }
}

static void simp_pre_cb(tree_t t, void *__ctx)
{
   simp_ctx_t *ctx = __ctx;

   switch (tree_kind(t)) {
   case T_BLOCK:
   case T_PACKAGE:
   case T_PACK_INST:
   case T_FUNC_INST:
   case T_PROC_INST:
      if (tree_genmaps(t) > 0)
         simp_generics(t, ctx);
      break;
   default:
      break;
   }
}

void simplify_local(tree_t top, jit_t *jit, unit_registry_t *ur)
{
   simp_ctx_t ctx = {
      .top       = top,
      .jit       = jit,
      .registry  = ur,
      .eval_mask = TREE_F_LOCALLY_STATIC,
   };

   tree_rewrite(top, simp_pre_cb, simp_tree, NULL, &ctx);

   if (ctx.generics)
      hash_free(ctx.generics);
}

void simplify_global(tree_t top, hash_t *generics, jit_t *jit,
                     unit_registry_t *ur)
{
   simp_ctx_t ctx = {
      .top       = top,
      .jit       = jit,
      .registry  = ur,
      .eval_mask = TREE_F_LOCALLY_STATIC | TREE_F_GLOBALLY_STATIC,
      .generics  = generics,
   };

   type_rewrite_post_fn_t type_cb = NULL;
   if (standard() >= STD_08)
      type_cb = simp_type;

   tree_rewrite(top, simp_pre_cb, simp_tree, type_cb, &ctx);

   if (generics == NULL && ctx.generics != NULL)
      hash_free(ctx.generics);
}
