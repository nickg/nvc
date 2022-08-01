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

typedef struct _imp_signal imp_signal_t;

struct _imp_signal {
   imp_signal_t *next;
   ident_t       name;
   tree_t        signal;
   tree_t        process;
};

typedef struct {
   imp_signal_t *imp_signals;
   tree_t        top;
   eval_t       *eval;
   tree_flags_t  eval_mask;
   hash_t       *generics;
} simp_ctx_t;

static tree_t simp_tree(tree_t t, void *context);
static void simp_build_wait(tree_t wait, tree_t expr, bool all);

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

   if (last_pos < nparams - 1) {
      tree_t new = tree_new(tree_kind(t));
      tree_set_loc(new, tree_loc(t));
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
         tree_t port  = tree_port(decl, i);
         ident_t name = tree_ident(port);

         bool found = false;
         for (int j = last_pos + 1; (j < nparams) && !found; j++) {
            tree_t p = tree_param(t, j);
            assert(tree_subkind(p) == P_NAMED);

            tree_t ref = tree_name(p);
            assert(tree_kind(ref) == T_REF);

            if (name == tree_ident(ref)) {
               tree_t value = tree_value(p);

               if (tree_kind(value) == T_OPEN)
                  value = tree_value(port);

               add_param(new, value, P_POS, NULL);
               found = true;
            }
         }
         assert(found);
      }

      t = new;
   }

   return t;
}

static tree_t simp_fold(tree_t t, simp_ctx_t *ctx)
{
   type_t type = tree_type(t);
   if (!type_is_scalar(type))
      return t;
   else if (!eval_possible(ctx->eval, t))
      return t;

   tree_t folded = eval_try_fold(ctx->eval, t);

   return folded;
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

static tree_t simp_fcall(tree_t t, simp_ctx_t *ctx)
{
   if (standard() >= STD_08 && ctx->generics != NULL)
      simp_generic_subprogram(t, ctx);

   t = simp_call_args(t);

   if (tree_subkind(tree_ref(t)) == S_CONCAT)
      t = simp_concat(t);
   else if (tree_flags(t) & ctx->eval_mask)
      return simp_fold(t, ctx);

   return t;
}

static tree_t simp_type_conv(tree_t t, simp_ctx_t *ctx)
{
   return simp_fold(t, ctx);
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

   if (ctx->eval_mask & TREE_F_GLOBALLY_STATIC) {
      tree_t ref = name_to_ref(value);
      if (ref != NULL && tree_kind(tree_ref(ref)) == T_CONST_DECL)
         return simp_fold(t, ctx);
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
         case T_STRING:
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
               // Fall-through
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

static tree_t simp_signal_attribute(tree_t t, attr_kind_t which,
                                    simp_ctx_t *ctx)
{
   tree_t name = tree_name(t);
   assert(tree_kind(name) == T_REF);

   tree_t decl = tree_ref(name);

   const tree_kind_t kind = tree_kind(decl);
   if (kind != T_SIGNAL_DECL && kind != T_PORT_DECL)
      return t;

   tree_t param = NULL;
   if (which != ATTR_TRANSACTION)
      param = tree_value(tree_param(t, 0));

   int64_t iparam = 0;
   if (param != NULL && !folded_int(param, &iparam))
      return t;

   LOCAL_TEXT_BUF tb = tb_new();
   tb_istr(tb, tree_ident(name));
   switch (which) {
   case ATTR_TRANSACTION: tb_cat(tb, "$transaction"); break;
   case ATTR_DELAYED:     tb_printf(tb, "$delayed_%"PRIi64, iparam); break;
   default: break;
   }

   ident_t id = ident_new(tb_get(tb));

   for (imp_signal_t *it = ctx->imp_signals; it; it = it->next) {
      if (it->name == id)
         return make_ref(it->signal);
   }

   tree_t s = tree_new(T_SIGNAL_DECL);
   tree_set_loc(s, tree_loc(t));
   tree_set_ident(s, id);
   tree_set_type(s, tree_type(t));

   tree_t r = make_ref(s);

   tree_t p = tree_new(T_PROCESS);
   tree_set_loc(p, tree_loc(t));
   tree_set_ident(p, ident_prefix(tree_ident(s), ident_new("p"), '_'));

   tree_t a = tree_new(T_SIGNAL_ASSIGN);
   tree_set_ident(a, ident_new("assign"));
   tree_set_target(a, r);

   switch (which) {
   case ATTR_DELAYED:
      {
         if (tree_has_value(decl))
            tree_set_value(s, tree_value(decl));

         tree_t delay = tree_value(tree_param(t, 0));

         tree_t wave = tree_new(T_WAVEFORM);
         tree_set_value(wave, name);
         tree_set_delay(wave, delay);

         tree_add_waveform(a, wave);
      }
      break;

   case ATTR_TRANSACTION:
      {
         tree_t not_decl = std_func(ident_new("STD.STANDARD.\"not\"(B)B"));
         assert(not_decl != NULL);

         tree_t not = tree_new(T_FCALL);
         tree_set_ident(not, ident_new("\"not\""));
         tree_set_ref(not, not_decl);
         tree_set_type(not, type_result(tree_type(not_decl)));
         add_param(not, r, P_POS, NULL);

         tree_t wave = tree_new(T_WAVEFORM);
         tree_set_value(wave, not);

         tree_add_waveform(a, wave);
      }
      break;

   default:
      break;
   }

   tree_add_stmt(p, a);

   tree_t wait = tree_new(T_WAIT);
   tree_set_ident(wait, ident_new("wait"));
   tree_set_flag(wait, TREE_F_STATIC_WAIT);
   tree_add_trigger(wait, name);

   tree_add_stmt(p, wait);

   imp_signal_t *imp = xmalloc(sizeof(imp_signal_t));
   imp->next    = ctx->imp_signals;
   imp->signal  = s;
   imp->process = p;
   imp->name    = id;

   ctx->imp_signals = imp;

   return r;
}

static tree_t simp_convert_range_bound(attr_kind_t attr, tree_t r)
{
   tree_t expr;
   switch (attr) {
   case ATTR_LEFT:
      expr = tree_left(r);
      break;
   case ATTR_RIGHT:
      expr = tree_right(r);
      break;
   case ATTR_LOW:
      expr = (tree_subkind(r) == RANGE_TO) ? tree_left(r) : tree_right(r);
      break;
   case ATTR_HIGH:
      expr = (tree_subkind(r) == RANGE_TO) ? tree_right(r) : tree_left(r);
      break;
   default:
      fatal_trace("unsupported attribute %d", attr);
   }

   type_t type = tree_type(r);
   if (type_eq(tree_type(expr), type))
      return expr;

   tree_t conv = tree_new(T_TYPE_CONV);
   tree_set_loc(conv, tree_loc(r));
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
   case ATTR_DELAYED:
   case ATTR_TRANSACTION:
      return simp_signal_attribute(t, predef, ctx);

   case ATTR_POS:
      {
         tree_t value = tree_value(tree_param(t, 0));

         unsigned upos;
         int64_t ipos;
         if (folded_int(value, &ipos))
            return get_int_lit(t, NULL, ipos);
         else if (folded_enum(value, &upos))
            return get_int_lit(t, NULL, upos);
         else
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
            default:
               fatal_trace("invalid enumeration attribute %d", predef);
            }
         }

         if (type_is_array(type)) {
            if (tree_params(t) > 0) {
               tree_t value = tree_value(tree_param(t, 0));
               if (!folded_int(value, &dim_i))
                  fatal_at(tree_loc(value), "locally static dimension "
                           "expression was not folded");
            }

            if (name_kind == T_REF
                && tree_kind(tree_ref(name)) == T_TYPE_DECL
                && type_is_unconstrained(type)) {

               // Get index type of unconstrained array

               if (dim_i < 1 || dim_i > type_index_constrs(type))
                  return t;

               type  = type_index_constr(type, dim_i - 1);
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
            if (tree_kind(tree_left(r)) == T_LITERAL
                && tree_kind(tree_right(r)) == T_LITERAL) {
               int64_t low, high;
               range_bounds(r, &low, &high);
               return get_int_lit(t, NULL, (high < low) ? 0 : high - low + 1);
            }
            else
               return t;

         case ATTR_LOW:
         case ATTR_HIGH:
         case ATTR_LEFT:
         case ATTR_RIGHT:
            return simp_convert_range_bound(predef, r);
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

static tree_t simp_extract_string_literal(tree_t literal, int64_t index,
                                          tree_t def)
{
   type_t type = tree_type(literal);
   if (type_is_unconstrained(type))
      return def;

   tree_t bounds = range_of(type, 0);
   int64_t low, high;
   range_bounds(bounds, &low, &high);

   const bool to = (tree_subkind(bounds) == RANGE_TO);

   const int pos = to ? (index + low) : (high - index);
   if ((pos < 0) || (pos > tree_chars(literal)))
      return def;

   return tree_char(literal, pos);
}

static tree_t simp_extract_aggregate(tree_t agg, int64_t index, tree_t def)
{
   type_t type = tree_type(agg);
   if (type_is_unconstrained(type) || type_constraints(type) > 1)
      return def;

   tree_t bounds = range_of(tree_type(agg), 0);
   int64_t low, high;
   range_bounds(bounds, &low, &high);

   const bool to = (tree_subkind(bounds) == RANGE_TO);

   const int nassocs = tree_assocs(agg);
   for (int i = 0; i < nassocs; i++) {
      tree_t a = tree_assoc(agg, i);
      switch (tree_subkind(a)) {
      case A_POS:
         {
            const int pos = tree_pos(a);
            if ((to && (pos + low == index))
                || (!to && (high - pos == index)))
               return tree_value(a);
         }
         break;

      case A_OTHERS:
         return tree_value(a);

      case A_RANGE:
         {
            tree_t r = tree_range(a, 0);
            const int64_t left  = assume_int(tree_left(r));
            const int64_t right = assume_int(tree_right(r));

            if ((to && (index >= left) && (index <= right))
                || (!to && (index <= left) && (index >= right)))
               return tree_value(a);
         }
         break;

      case A_NAMED:
         if (assume_int(tree_name(a)) == index)
            return tree_value(a);
         break;
      }
   }

   return def;
}

static tree_t simp_array_slice(tree_t t)
{
   tree_t value = tree_value(t);

   if (tree_kind(value) == T_OPEN)
      return value;

   return t;
}

static tree_t simp_array_ref(tree_t t, simp_ctx_t *ctx)
{
   tree_t value = tree_value(t);

   if (tree_kind(value) == T_OPEN)
      return value;

   const int nparams = tree_params(t);

   int64_t index0;
   bool can_fold = true;
   for (int i = 0; i < nparams; i++) {
      tree_t p = tree_param(t, i);
      assert(tree_subkind(p) == P_POS);
      can_fold = can_fold && folded_int(tree_value(p), &index0);
   }

   if (!can_fold)
      return t;

   switch (tree_kind(value)) {
   case T_AGGREGATE:
      if (nparams == 1)
         return simp_extract_aggregate(value, index0, t);
      break;

   case T_STRING:
      return simp_extract_string_literal(value, index0, t);

   case T_REF:
      if (nparams == 1) {
         tree_t decl = tree_ref(value);
         if (tree_kind(decl) == T_CONST_DECL && tree_has_value(decl)) {
            tree_t cval = tree_value(decl);
            if (tree_kind(cval) == T_AGGREGATE)
               return simp_extract_aggregate(cval, index0, t);
         }
      }
      break;

   default:
      break;
   }

   if (ctx->eval_mask & TREE_F_GLOBALLY_STATIC) {
      tree_t ref = name_to_ref(value);
      if (ref != NULL && tree_kind(tree_ref(ref)) == T_CONST_DECL)
         return simp_fold(t, ctx);
   }

   return t;
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
         simp_build_wait(w, t, true);
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
      simp_build_wait(t, tree_value(t), false);

   return t;
}

static tree_t simp_case(tree_t t)
{
   const int nassocs = tree_assocs(t);
   if (nassocs == 0)
      return NULL;    // All choices are unreachable

   int64_t ival;
   if (folded_int(tree_value(t), &ival)) {
      for (int i = 0; i < nassocs; i++) {
         tree_t a = tree_assoc(t, i);
         switch ((assoc_kind_t)tree_subkind(a)) {
         case A_NAMED:
            {
               int64_t aval;
               if (folded_int(tree_name(a), &aval) && (ival == aval)) {
                  if (tree_has_value(a))
                     return tree_value(a);
                  else
                     return NULL;
               }
            }
            break;

         case A_RANGE:
            continue;   // TODO

         case A_OTHERS:
            if (tree_has_value(a))
               return tree_value(a);
            else
               return NULL;

         case A_POS:
            break;
         }
      }
   }

   return t;
}

static tree_t simp_cond(tree_t t)
{
   if (tree_has_value(t)) {
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
   else if (folded_bool(tree_value(t), &value_b) && !value_b) {
      // Condition is false so loop never executes
      return NULL;
   }
   else
      return t;
}

static void simp_build_wait_for_target(tree_t wait, tree_t expr, bool all)
{
   switch (tree_kind(expr)) {
   case T_ARRAY_SLICE:
      simp_build_wait(wait, tree_range(expr, 0), all);
      break;

   case T_ARRAY_REF:
      {
         const int nparams = tree_params(expr);
         for (int i = 0; i < nparams; i++)
            simp_build_wait(wait, tree_value(tree_param(expr, i)), all);
      }
      break;

   default:
      break;
   }
}

static void simp_build_wait(tree_t wait, tree_t expr, bool all)
{
   // LRM 08 section 10.2 has rules for building a wait statement from a
   // sensitivity list. LRM 08 section 11.3 extends these rules to
   // all-sensitised processes.

   switch (tree_kind(expr)) {
   case T_REF:
      {
         tree_t decl = tree_ref(expr);
         if (class_of(decl) == C_SIGNAL) {
            // Check for duplicates
            const int ntriggers = tree_triggers(wait);
            for (int i = 0; i < ntriggers; i++) {
               tree_t t = tree_trigger(wait, i);
               if (tree_kind(t) == T_REF && tree_ref(t) == decl)
                  return;
            }

            tree_add_trigger(wait, expr);
         }
      }
      break;

   case T_WAVEFORM:
   case T_RECORD_REF:
   case T_QUALIFIED:
   case T_TYPE_CONV:
   case T_ASSERT:
      if (tree_has_value(expr))
         simp_build_wait(wait, tree_value(expr), all);
      break;

   case T_ARRAY_REF:
   case T_ARRAY_SLICE:
      {
         tree_t ref = name_to_ref(expr);
         if (ref != NULL && class_of(ref) == C_SIGNAL) {
            if (longest_static_prefix(expr) == expr)
               tree_add_trigger(wait, expr);
            else {
               simp_build_wait(wait, tree_value(expr), all);
               simp_build_wait_for_target(wait, expr, all);
            }
         }
         else
            simp_build_wait(wait, tree_value(expr), all);
      }
      break;

   case T_FCALL:
   case T_PCALL:
      {
         tree_t decl = tree_ref(expr);
         const int nparams = tree_params(expr);
         for (int i = 0; i < nparams; i++) {
            const port_mode_t mode = tree_subkind(tree_port(decl, i));
            if (mode == PORT_IN || mode == PORT_INOUT)
               simp_build_wait(wait, tree_value(tree_param(expr, i)), all);
         }

         const tree_kind_t kind = tree_kind(decl);
         if (all && kind == T_PROC_BODY)
            simp_build_wait(wait, decl, all);
      }
      break;

   case T_AGGREGATE:
      {
         const int nassocs = tree_assocs(expr);
         for (int i = 0; i < nassocs; i++)
            simp_build_wait(wait, tree_value(tree_assoc(expr, i)), all);
      }
      break;

   case T_ATTR_REF:
      {
         const attr_kind_t predef = tree_subkind(expr);
         if (predef == ATTR_EVENT || predef == ATTR_ACTIVE)
            simp_build_wait(wait, tree_name(expr), all);

         const int nparams = tree_params(expr);
         for (int i = 0; i < nparams; i++)
            simp_build_wait(wait, tree_value(tree_param(expr, i)), all);
      }
      break;

   case T_LITERAL:
   case T_STRING:
      break;

   case T_IF:
      {
         const int nconds = tree_conds(expr);
         for (int i = 0; i < nconds; i++)
            simp_build_wait(wait, tree_cond(expr, i), all);
      }
      break;

   case T_COND:
      {
         if (tree_has_value(expr))
            simp_build_wait(wait, tree_value(expr), all);

         const int nstmts = tree_stmts(expr);
         for (int i = 0; i < nstmts; i++)
            simp_build_wait(wait, tree_stmt(expr, i), all);
      }
      break;

   case T_PROCESS:
   case T_SEQUENCE:
   case T_PROC_BODY:
      {
         const int nstmts = tree_stmts(expr);
         for (int i = 0; i < nstmts; i++)
            simp_build_wait(wait, tree_stmt(expr, i), all);
      }
      break;

   case T_SIGNAL_ASSIGN:
      {
         simp_build_wait_for_target(wait, tree_target(expr), all);

         const int nwaves = tree_waveforms(expr);
         for (int i = 0; i < nwaves; i++)
            simp_build_wait(wait, tree_waveform(expr, i), all);
      }
      break;

   case T_VAR_ASSIGN:
      simp_build_wait_for_target(wait, tree_target(expr), all);
      simp_build_wait(wait, tree_value(expr), all);
      break;

   case T_CASE:
      {
         simp_build_wait(wait, tree_value(expr), all);

         const int nassocs = tree_assocs(expr);
         for (int i = 0; i < nassocs; i++)
            simp_build_wait(wait, tree_value(tree_assoc(expr, i)), all);
      }
      break;

   case T_FOR:
      {
         simp_build_wait(wait, tree_range(expr, 0), all);

         const int nstmts = tree_stmts(expr);
         for (int i = 0; i < nstmts; i++)
            simp_build_wait(wait, tree_stmt(expr, i), all);
      }
      break;

   case T_WHILE:
      {
         simp_build_wait(wait, tree_value(expr), all);

         const int nstmts = tree_stmts(expr);
         for (int i = 0; i < nstmts; i++)
            simp_build_wait(wait, tree_stmt(expr, i), all);
      }
      break;

   case T_RANGE:
      if (tree_subkind(expr) == RANGE_EXPR)
         simp_build_wait(wait, tree_value(expr), all);
      else {
         simp_build_wait(wait, tree_left(expr), all);
         simp_build_wait(wait, tree_right(expr), all);
      }
      break;

   default:
      fatal_trace("Cannot handle tree kind %s in wait expression",
                  tree_kind_str(tree_kind(expr)));
   }
}

static tree_t simp_guard(tree_t container, tree_t t, tree_t wait, tree_t s0)
{
   // See LRM 93 section 9.3

   tree_t g_if = tree_new(T_IF);
   tree_set_ident(g_if, ident_new("guard_if"));
   tree_set_loc(g_if, tree_loc(t));

   tree_t c0 = tree_new(T_COND);
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

         tree_t c1 = tree_new(T_COND);
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

   // Concurrent procedure calls may have internal waits
   if (tree_kind(s0) != T_PCALL)
      tree_set_flag(w, TREE_F_STATIC_WAIT);

   tree_t container = p;  // Where to add new statements

   if (tree_has_guard(t))
      container = simp_guard(container, t, w, s0);

   tree_add_stmt(container, s0);

   simp_build_wait(w, s0, false);

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
      ident_t label = tree_ident(t);
      if (label != NULL)
         tree_set_ident(s, label);

      for (int i = 0; i < nconds; i++)
         tree_add_cond(s, tree_cond(t, i));

      return s;
   }
}

static tree_t simp_select(tree_t t)
{
   // Replace a select statement with a case statement

   tree_t c = tree_new(T_CASE);
   tree_set_loc(c, tree_loc(t));
   tree_set_value(c, tree_value(t));
   tree_set_ident(c, tree_ident(t));

   const int nassocs = tree_assocs(t);
   for (int i = 0; i < nassocs; i++)
      tree_add_assoc(c, tree_assoc(t, i));

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

static tree_t simp_use(tree_t t)
{
   tree_t lib_decl = tree_ref(t);
   if (tree_kind(lib_decl) != T_LIBRARY)
      return t;

   ident_t qual = tree_ident(t);
   ident_t lalias = ident_until(qual, '.');
   ident_t lname = tree_ident2(lib_decl);

   if (lalias != lname) {
      ident_t rest = ident_from(qual, '.');
      tree_set_ident(t, ident_prefix(lname, rest, '.'));
   }

   return t;
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
   bool value_b;
   if (!folded_bool(tree_value(t), &value_b))
      return t;

   if (value_b) {
      tree_t block = tree_new(T_BLOCK);
      tree_set_ident(block, tree_ident(t));
      tree_set_loc(block, tree_loc(t));

      const int ndecls = tree_decls(t);
      for (int i = 0; i < ndecls; i++)
         tree_add_decl(block, tree_decl(t, i));

      const int nstmts = tree_stmts(t);
      for (int i = 0; i < nstmts; i++)
         tree_add_stmt(block, tree_stmt(t, i));

      return block;
   }
   else
      return NULL;
}

static tree_t simp_signal_assign(tree_t t)
{
   tree_t target = tree_target(t);

   if (tree_kind(target) == T_OPEN)
      return NULL;    // Delete it

   return t;
}

static tree_t simp_assoc(tree_t t)
{
   if (!tree_has_value(t))
      return NULL;   // Delete it

   return t;
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
      assert(base_kind == RANGE_TO || base_kind == RANGE_DOWNTO);

      tree_t rev = tree_new(T_RANGE);
      tree_set_subkind(rev, base_kind ^ 1);
      tree_set_loc(rev, tree_loc(t));
      tree_set_type(rev, tree_type(t));
      tree_set_left(rev, tree_right(base_r));
      tree_set_right(rev, tree_left(base_r));

      return rev;
   }
   else
      return range_of(type, dim);
}

static tree_t simp_subprogram_decl(tree_t decl, simp_ctx_t *ctx)
{
   // Remove predefined operators which are hidden by explicitly defined
   // operators in the same region

   const tree_flags_t flags = tree_flags(decl);
   if ((flags & TREE_F_PREDEFINED) && (flags & TREE_F_HIDDEN))
      return NULL;

   return decl;
}

static void simp_port_map(tree_t t, simp_ctx_t *ctx)
{
   // Replace any non-globally-static actuals with tempoary signals
   assert(standard() >= STD_08);

   const int nparams = tree_params(t);
   for (int i = 0; i < nparams; i++) {
      tree_t m = tree_param(t, i);

      tree_t value = tree_value(m);
      if (tree_kind(value) != T_FCALL)
         continue;
      else if (tree_flags(value) & TREE_F_GLOBALLY_STATIC)
         continue;

      char *signame LOCAL = xasprintf("%s_actual_%d", istr(tree_ident(t)), i);

      tree_t s = tree_new(T_SIGNAL_DECL);
      tree_set_loc(s, tree_loc(value));
      tree_set_ident(s, ident_uniq(signame));
      tree_set_type(s, tree_type(value));

      tree_t p = tree_new(T_PROCESS);
      tree_set_loc(p, tree_loc(t));
      tree_set_ident(p, ident_prefix(tree_ident(s), ident_new("p"), '_'));

      tree_t r = make_ref(s);

      tree_t a = tree_new(T_SIGNAL_ASSIGN);
      tree_set_ident(a, ident_new("assign"));
      tree_set_target(a, r);
      tree_add_stmt(p, a);

      tree_t wave = tree_new(T_WAVEFORM);
      tree_set_value(wave, value);
      tree_add_waveform(a, wave);

      tree_t wait = tree_new(T_WAIT);
      tree_set_ident(wait, ident_new("wait"));
      tree_set_flag(wait, TREE_F_STATIC_WAIT);
      simp_build_wait(wait, value, false);
      tree_add_stmt(p, wait);

      tree_set_value(m, r);

      imp_signal_t *imp = xmalloc(sizeof(imp_signal_t));
      imp->next    = ctx->imp_signals;
      imp->signal  = s;
      imp->process = p;

      ctx->imp_signals = imp;
   }
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

static void simp_add_implicit_signals(tree_t t, simp_ctx_t *ctx)
{
   for (imp_signal_t *s = ctx->imp_signals, *tmp; s; s = tmp) {
      tree_add_decl(t, s->signal);
      if (s->process != NULL)
         tree_add_stmt(t, s->process);

      tmp = s->next;
      free(s);
   }
   ctx->imp_signals = NULL;
}

static tree_t simp_tree(tree_t t, void *_ctx)
{
   simp_ctx_t *ctx = _ctx;

   switch (tree_kind(t)) {
   case T_PROCESS:
      return simp_process(t);
   case T_ARRAY_REF:
      return simp_array_ref(t, ctx);
   case T_ARRAY_SLICE:
      return simp_array_slice(t);
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
      return simp_case(t);
   case T_WHILE:
      return simp_while(t);
   case T_CONCURRENT:
      return simp_concurrent(t);
   case T_COND_ASSIGN:
   case T_COND_VAR_ASSIGN:
      return simp_cond_assign(t);
   case T_SELECT:
      return simp_select(t);
   case T_WAIT:
      return simp_wait(t);
   case T_NULL:
      return NULL;   // Delete it
   case T_RECORD_REF:
      return simp_record_ref(t, ctx);
   case T_CONTEXT_REF:
      return simp_context_ref(t, ctx);
   case T_USE:
      return simp_use(t);
   case T_ASSERT:
      return simp_assert(t);
   case T_IF_GENERATE:
      return simp_if_generate(t);
   case T_SIGNAL_ASSIGN:
      return simp_signal_assign(t);
   case T_ASSOC:
      return simp_assoc(t);
   case T_TYPE_CONV:
      return simp_type_conv(t, ctx);
   case T_LITERAL:
      return simp_literal(t);
   case T_RANGE:
      return simp_range(t);
   case T_FUNC_DECL:
   case T_PROC_DECL:
      return simp_subprogram_decl(t, ctx);
   case T_INSTANCE:
      if (standard() >= STD_08)
         simp_port_map(t, ctx);
      // Fall-through
   case T_BINDING:
      simp_generic_map(t, tree_ref(t));
      return t;
   case T_BLOCK:
      simp_add_implicit_signals(t, ctx);
      simp_generic_map(t, t);
      return t;
   case T_ARCH:
      simp_add_implicit_signals(t, ctx);
      return t;
   case T_COND:
      return simp_cond(t);
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

void simplify_local(tree_t top, eval_t *ex)
{
   simp_ctx_t ctx = {
      .imp_signals = NULL,
      .top         = top,
      .eval        = ex,
      .eval_mask   = TREE_F_LOCALLY_STATIC,
   };

   tree_rewrite(top, simp_pre_cb, simp_tree, NULL, &ctx);

   if (ctx.generics)
      hash_free(ctx.generics);

   assert(ctx.imp_signals == NULL);
}

void simplify_global(tree_t top, hash_t *generics, eval_t *ex)
{
   simp_ctx_t ctx = {
      .imp_signals = NULL,
      .top         = top,
      .eval        = ex,
      .eval_mask   = TREE_F_GLOBALLY_STATIC | TREE_F_LOCALLY_STATIC,
      .generics    = generics,
   };

   type_rewrite_post_fn_t type_cb = NULL;
   if (standard() >= STD_08)
      type_cb = simp_type;

   tree_rewrite(top, simp_pre_cb, simp_tree, type_cb, &ctx);

   if (generics == NULL && ctx.generics != NULL)
      hash_free(ctx.generics);

   assert(ctx.imp_signals == NULL);
}
