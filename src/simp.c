//
//  Copyright (C) 2011-2012  Nick Gasson
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

#include <assert.h>
#include <string.h>
#include <stdarg.h>

#define MAX_BUILTIN_ARGS 2

static ident_t std_bool_i = NULL;
static ident_t builtin_i  = NULL;

static int errors = 0;

#define simp_error(t, ...) \
   { errors++; error_at(tree_loc(t), __VA_ARGS__); return t; }

static tree_t simp_tree(tree_t t, void *context);
static void simp_build_wait(tree_t ref, void *context);

static bool folded_int(tree_t t, literal_t *l)
{
   if (tree_kind(t) == T_LITERAL) {
      *l = tree_literal(t);
      return (l->kind == L_INT);
   }
   else
      return false;
}

static bool folded_real(tree_t t, literal_t *l)
{
   if (tree_kind(t) == T_LITERAL) {
      *l = tree_literal(t);
      return (l->kind == L_REAL);
   }
   else
      return false;
}

static bool folded_bool(tree_t t, bool *b)
{
   if (tree_kind(t) == T_REF) {
      tree_t decl = tree_ref(t);
      if (tree_kind(decl) == T_ENUM_LIT
          && type_ident(tree_type(decl)) == std_bool_i) {
         *b = (tree_pos(decl) == 1);
         return true;
      }
   }

   return false;
}

static bool folded_agg(tree_t t)
{
   if (tree_kind(t) == T_AGGREGATE) {
      for (unsigned i = 0; i < tree_assocs(t); i++) {
         assoc_t a = tree_assoc(t, i);
         literal_t dummy;
         switch (a.kind) {
         case A_NAMED:
            if (!folded_int(a.name, &dummy))
               return false;
            break;
         case A_RANGE:
            if (!folded_int(a.range.left, &dummy)
                || !folded_int(a.range.right, &dummy))
               return false;
            break;
         default:
            break;
         }
      }
      return true;
   }
   else
      return false;
}

static tree_t get_int_lit(tree_t t, int64_t i)
{
   tree_t fdecl = tree_ref(t);
   assert(tree_kind(fdecl) == T_FUNC_DECL);

   literal_t l;
   l.kind = L_INT;
   l.i = i;

   tree_t f = tree_new(T_LITERAL);
   tree_set_loc(f, tree_loc(t));
   tree_set_literal(f, l);
   tree_set_type(f, tree_type(t));

   return f;
}

static tree_t get_real_lit(tree_t t, double r)
{
   tree_t fdecl = tree_ref(t);
   assert(tree_kind(fdecl) == T_FUNC_DECL);

   literal_t l;
   l.kind = L_REAL;
   l.r = r;

   tree_t f = tree_new(T_LITERAL);
   tree_set_loc(f, tree_loc(t));
   tree_set_literal(f, l);
   tree_set_type(f, tree_type(t));

   return f;
}

static tree_t get_bool_lit(tree_t t, bool v)
{
   tree_t fdecl = tree_ref(t);
   assert(tree_kind(fdecl) == T_FUNC_DECL);

   type_t std_bool = type_result(tree_type(fdecl));

   assert(type_ident(std_bool) == std_bool_i);
   assert(type_enum_literals(std_bool) == 2);

   tree_t lit = type_enum_literal(std_bool, v ? 1 : 0);

   tree_t b = tree_new(T_REF);
   tree_set_loc(b, tree_loc(t));
   tree_set_ref(b, lit);
   tree_set_type(b, std_bool);
   tree_set_ident(b, tree_ident(lit));

   return b;
}

static tree_t simp_fcall_log(tree_t t, ident_t builtin, bool *args)
{
   if (icmp(builtin, "not"))
      return get_bool_lit(t, !args[0]);
   else if (icmp(builtin, "and"))
      return get_bool_lit(t, args[0] && args[1]);
   else if (icmp(builtin, "nand"))
      return get_bool_lit(t, !(args[0] && args[1]));
   else if (icmp(builtin, "or"))
      return get_bool_lit(t, args[0] || args[1]);
   else if (icmp(builtin, "nor"))
      return get_bool_lit(t, !(args[0] || args[1]));
   else if (icmp(builtin, "xor"))
      return get_bool_lit(t, args[0] ^ args[1]);
   else if (icmp(builtin, "xnor"))
      return get_bool_lit(t, !(args[0] ^ args[1]));
   else
      return t;
}

static tree_t simp_fcall_real(tree_t t, ident_t builtin, literal_t *args)
{
   const int lkind = args[0].kind;  // Assume all types checked same
   assert(lkind == L_REAL);

   if (icmp(builtin, "mul")) {
      return get_real_lit(t, args[0].r * args[1].r);
   }
   else if (icmp(builtin, "div")) {
      return get_real_lit(t, args[0].r / args[1].r);
   }
   else if (icmp(builtin, "add")) {
      return get_real_lit(t, args[0].r + args[1].r);
   }
   else if (icmp(builtin, "sub")) {
      return get_real_lit(t, args[0].r - args[1].r);
   }
   else if (icmp(builtin, "neg")) {
      return get_real_lit(t, -args[0].r);
   }
   else if (icmp(builtin, "identity")) {
      return get_real_lit(t, args[0].r);
   }
   else if (icmp(builtin, "eq")) {
      return get_bool_lit(t, args[0].r == args[1].r);
   }
   else if (icmp(builtin, "neq")) {
      return get_bool_lit(t, args[0].r != args[1].r);
   }
   else if (icmp(builtin, "gt")) {
      return get_bool_lit(t, args[0].r > args[1].r);
   }
   else if (icmp(builtin, "lt")) {
      return get_bool_lit(t, args[0].r < args[1].r);
   }
   else
      return t;
}

static tree_t simp_fcall_int(tree_t t, ident_t builtin, literal_t *args)
{
   const int lkind = args[0].kind;  // Assume all types checked same
   assert(lkind == L_INT);

   if (icmp(builtin, "mul")) {
      return get_int_lit(t, args[0].i * args[1].i);
   }
   else if (icmp(builtin, "div")) {
      return get_int_lit(t, args[0].i / args[1].i);
   }
   else if (icmp(builtin, "add")) {
      return get_int_lit(t, args[0].i + args[1].i);
   }
   else if (icmp(builtin, "sub")) {
      return get_int_lit(t, args[0].i - args[1].i);
   }
   else if (icmp(builtin, "neg")) {
      return get_int_lit(t, -args[0].i);
   }
   else if (icmp(builtin, "identity")) {
      return get_int_lit(t, args[0].i);
   }
   else if (icmp(builtin, "eq")) {
      return get_bool_lit(t, args[0].i == args[1].i);
   }
   else if (icmp(builtin, "neq")) {
      return get_bool_lit(t, args[0].i != args[1].i);
   }
   else if (icmp(builtin, "gt")) {
      return get_bool_lit(t, args[0].i > args[1].i);
   }
   else if (icmp(builtin, "lt")) {
      return get_bool_lit(t, args[0].i < args[1].i);
   }
   else
      return t;
}

static tree_t simp_fcall_agg(tree_t t, ident_t builtin)
{
   bool agg_low  = icmp(builtin, "agg_low");
   bool agg_high = icmp(builtin, "agg_high");

   if (agg_low || agg_high) {
      int64_t low = INT64_MAX, high = INT64_MIN;
      param_t p = tree_param(t, 0);
      for (unsigned i = 0; i < tree_assocs(p.value); i++) {
         assoc_t a = tree_assoc(p.value, i);
         switch (a.kind) {
         case A_NAMED:
            {
               int64_t tmp = assume_int(a.name);
               if (tmp < low) low = tmp;
               if (tmp > high) high = tmp;
            }
            break;

         case A_RANGE:
            {
               int64_t low_r, high_r;
               range_bounds(a.range, &low_r, &high_r);
               if (low_r < low) low = low_r;
               if (high_r > high) high = high_r;
            }
            break;

         default:
            assert(false);
         }
      }

      return get_int_lit(t, agg_low ? low : high);
   }
   else
      return t;
}

static tree_t simp_fcall(tree_t t)
{
   tree_t decl = tree_ref(t);
   assert(tree_kind(decl) == T_FUNC_DECL
          || tree_kind(decl) == T_FUNC_BODY);

   ident_t builtin = tree_attr_str(decl, builtin_i);
   if (builtin == NULL)
      return t;     // TODO: expand pure function calls

   if (tree_params(t) > MAX_BUILTIN_ARGS)
      return t;

   bool can_fold_int  = true;
   bool can_fold_log  = true;
   bool can_fold_agg  = true;
   bool can_fold_real = true;
   literal_t largs[MAX_BUILTIN_ARGS];
   bool bargs[MAX_BUILTIN_ARGS];
   for (unsigned i = 0; i < tree_params(t); i++) {
      param_t p = tree_param(t, i);
      assert(p.kind == P_POS);
      can_fold_int  = can_fold_int && folded_int(p.value, &largs[i]);
      can_fold_log  = can_fold_log && folded_bool(p.value, &bargs[i]);
      can_fold_agg  = can_fold_agg && folded_agg(p.value);
      can_fold_real = can_fold_real && folded_real(p.value, &largs[i]);
   }

   if (can_fold_int)
      return simp_fcall_int(t, builtin, largs);
   else if (can_fold_log)
      return simp_fcall_log(t, builtin, bargs);
   else if (can_fold_agg)
      return simp_fcall_agg(t, builtin);
   else if (can_fold_real)
      return simp_fcall_real(t, builtin, largs);
   else
      return t;
}

static tree_t simp_ref(tree_t t)
{
   tree_t decl = tree_ref(t);
   type_t type = tree_type(decl);

   switch (tree_kind(decl)) {
   case T_CONST_DECL:
      if (type_kind(type) == T_PHYSICAL) {
         // Slight hack to constant-fold the definitions of
         // physical units that and generated during the sem phase
         tree_t copy = tree_copy(tree_value(decl));
         return tree_rewrite(copy, simp_tree, NULL);
      }
      else if (type_is_array(type))
         return t;
      else
         return tree_value(decl);

   case T_ALIAS:
      return tree_value(decl);

   default:
      return t;
   }
}

static tree_t simp_attr_ref(tree_t t)
{
   if (tree_has_value(t))
      return tree_value(t);
   else {
      tree_t decl = tree_ref(t);
      assert(tree_kind(decl) == T_FUNC_DECL);

      ident_t builtin = tree_attr_str(decl, builtin_i);
      assert(builtin != NULL);

      if (icmp(builtin, "length")) {
         tree_t array = tree_param(t, 0).value;
         if (type_kind(tree_type(array)) != T_UARRAY) {
            range_t r = type_dim(tree_type(array), 0);
            if (tree_kind(r.left) == T_LITERAL
                && tree_kind(r.right) == T_LITERAL) {
               int64_t low, high;
               range_bounds(type_dim(tree_type(array), 0), &low, &high);
               return get_int_lit(t, (high < low) ? 0 : high - low + 1);
            }
         }
      }

      // Convert attributes like 'EVENT to function calls
      tree_t fcall = tree_new(T_FCALL);
      tree_set_loc(fcall, tree_loc(t));
      tree_set_type(fcall, tree_type(t));
      tree_set_ident(fcall, tree_ident2(t));
      tree_set_ref(fcall, decl);

      for (unsigned i = 0; i < tree_params(t); i++)
         tree_add_param(fcall, tree_param(t, i));

      return fcall;
   }
}

static tree_t simp_array_ref(tree_t t)
{
   tree_t value = tree_value(t);
   if (tree_kind(value) != T_REF)
      return t;

   tree_t decl = tree_ref(value);

   literal_t indexes[tree_params(t)];
   bool can_fold = true;
   for (unsigned i = 0; i < tree_params(t); i++) {
      param_t p = tree_param(t, i);
      assert(p.kind == P_POS);
      can_fold = can_fold && folded_int(p.value, &indexes[i]);
   }

   if (!can_fold)
      return t;

   if (tree_params(t) > 1)
      return t;  // TODO: constant folding for multi-dimensional arrays

   switch (tree_kind(decl)) {
   case T_CONST_DECL:
      {
         tree_t v = tree_value(decl);
         assert(tree_kind(v) == T_AGGREGATE);
         assert(indexes[0].kind == L_INT);

         range_t bounds = type_dim(tree_type(decl), 0);
         int64_t left = assume_int(bounds.left);
         int64_t right = assume_int(bounds.right);

         if (indexes[0].i < left || indexes[0].i > right)
            simp_error(t, "array reference out of bounds");

         for (unsigned i = 0; i < tree_assocs(v); i++) {
            assoc_t a = tree_assoc(v, i);
            switch (a.kind) {
            case A_POS:
               if (a.pos + left == indexes[0].i)
                  return a.value;
               break;

            case A_OTHERS:
               return a.value;

            case A_RANGE:
               if ((indexes[0].i >= assume_int(a.range.left))
                   && (indexes[0].i <= assume_int(a.range.right)))
                  return a.value;
               break;

            case A_NAMED:
               if (assume_int(a.name) == indexes[0].i)
                  return a.value;
               break;
            }
         }

         assert(false);
      }
   default:
      return t;
   }
}

static tree_t simp_process(tree_t t)
{
   // Replace sensitivity list with a "wait on" statement
   if (tree_triggers(t) > 0) {
      tree_t p = tree_new(T_PROCESS);
      tree_set_ident(p, tree_ident(t));
      tree_set_loc(p, tree_loc(t));

      for (unsigned i = 0; i < tree_decls(t); i++)
         tree_add_decl(p, tree_decl(t, i));

      for (unsigned i = 0; i < tree_stmts(t); i++)
         tree_add_stmt(p, tree_stmt(t, i));

      tree_t w = tree_new(T_WAIT);
      tree_set_loc(w, tree_loc(t));
      tree_set_ident(w, tree_ident(p));
      for (unsigned i = 0; i < tree_triggers(t); i++)
         tree_add_trigger(w, tree_trigger(t, i));
      tree_add_stmt(p, w);

      return p;
   }
   else
      return t;
}

static tree_t simp_wait(tree_t t)
{
   // LRM 93 section 8.1
   // If there is no sensitivity list supplied generate one from the
   // condition clause

   if (tree_has_value(t) && (tree_triggers(t) == 0))
      tree_visit_only(tree_value(t), simp_build_wait, t, T_REF);

   return t;
}

static tree_t simp_if(tree_t t)
{
   bool value_b;
   if (folded_bool(tree_value(t), &value_b)) {
      if (value_b) {
         // If statement always executes so replace with then part
         if (tree_stmts(t) == 1)
            return tree_stmt(t, 0);
         else {
            tree_t b = tree_new(T_BLOCK);
            tree_set_ident(b, tree_ident(t));
            for (unsigned i = 0; i < tree_stmts(t); i++)
               tree_add_stmt(b, tree_stmt(t, i));
            return b;
         }
      }
      else {
         // If statement never executes so replace with else part
         if (tree_else_stmts(t) == 1)
            return tree_else_stmt(t, 0);
         else if (tree_else_stmts(t) == 0)
            return NULL;   // Delete it
         else {
            tree_t b = tree_new(T_BLOCK);
            tree_set_ident(b, tree_ident(t));
            for (unsigned i = 0; i < tree_else_stmts(t); i++)
               tree_add_stmt(b, tree_else_stmt(t, i));
            return b;
         }
      }
   }
   else
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

static tree_t simp_for(tree_t t)
{
   tree_t b = tree_new(T_BLOCK);
   tree_set_ident(b, tree_ident(t));

   for (unsigned i = 0; i < tree_decls(t); i++)
      tree_add_decl(b, tree_decl(t, i));

   tree_t decl = tree_decl(t, 0);

   tree_t var = tree_new(T_REF);
   tree_set_ident(var, tree_ident(decl));
   tree_set_type(var, tree_type(decl));
   tree_set_ref(var, decl);

   range_t r = tree_range(t);

   tree_t test = NULL;
   switch (r.kind) {
   case RANGE_TO:
      test = call_builtin("leq", NULL, r.left, r.right, NULL);
      break;
   case RANGE_DOWNTO:
      test = call_builtin("geq", NULL, r.left, r.right, NULL);
      break;
   case RANGE_DYN:
      break;
   default:
      assert(false);
   }

   tree_t container = b;
   if (test != NULL) {
      container = tree_new(T_IF);
      tree_set_ident(container, ident_uniq("null_check"));
      tree_set_value(container, test);

      tree_add_stmt(b, container);
   }

   tree_t init = tree_new(T_VAR_ASSIGN);
   tree_set_ident(init, ident_uniq("init"));
   tree_set_target(init, var);
   tree_set_value(init, r.left);

   tree_t wh = tree_new(T_WHILE);
   tree_set_ident(wh, ident_uniq("loop"));

   for (unsigned i = 0; i < tree_stmts(t); i++)
      tree_add_stmt(wh, tree_stmt(t, i));

   tree_t cmp = call_builtin("eq", NULL, var, r.right, NULL);

   tree_t exit = tree_new(T_EXIT);
   tree_set_ident(exit, ident_uniq("for_exit"));
   tree_set_value(exit, cmp);

   tree_t next;
   if (r.kind == RANGE_DYN) {
      assert(tree_kind(r.left) == T_FCALL);
      param_t p = tree_param(r.left, 0);

      tree_t asc = call_builtin("uarray_asc", NULL, p.value, NULL);
      next = tree_new(T_IF);
      tree_set_value(next, asc);
      tree_set_ident(next, ident_uniq("for_next"));

      tree_t succ = call_builtin("succ", tree_type(decl), var, NULL);

      tree_t a1 = tree_new(T_VAR_ASSIGN);
      tree_set_ident(a1, ident_uniq("for_next_asc"));
      tree_set_target(a1, var);
      tree_set_value(a1, succ);

      tree_t pred = call_builtin("pred", tree_type(decl), var, NULL);

      tree_t a2 = tree_new(T_VAR_ASSIGN);
      tree_set_ident(a2, ident_uniq("for_next_dsc"));
      tree_set_target(a2, var);
      tree_set_value(a2, pred);

      tree_add_stmt(next, a1);
      tree_add_else_stmt(next, a2);
   }
   else {
      tree_t call;
      switch (r.kind) {
      case RANGE_TO:
         call = call_builtin("succ", tree_type(decl), var, NULL);
         break;
      case RANGE_DOWNTO:
         call = call_builtin("pred", tree_type(decl), var, NULL);
         break;
      default:
         assert(false);
      }

      next = tree_new(T_VAR_ASSIGN);
      tree_set_ident(next, ident_uniq("for_next"));
      tree_set_target(next, var);
      tree_set_value(next, call);
   }

   tree_add_stmt(wh, exit);
   tree_add_stmt(wh, next);

   tree_add_stmt(container, init);
   tree_add_stmt(container, wh);

   return b;
}

static void simp_build_wait(tree_t ref, void *context)
{
   // Add each signal referenced in an expression to the trigger
   // list for a wait statement

   tree_t wait = context;

   tree_t decl = tree_ref(ref);
   tree_kind_t kind = tree_kind(decl);
   if (kind == T_SIGNAL_DECL || kind == T_PORT_DECL) {
      // Check for duplicates
      for (unsigned i = 0; i < tree_triggers(wait); i++) {
         if (tree_ref(tree_trigger(wait, i)) == decl)
            return;
      }

      tree_add_trigger(wait, ref);
   }
}

static tree_t simp_cassign(tree_t t)
{
   // Replace concurrent assignments with a process

   tree_t p = tree_new(T_PROCESS);
   tree_set_ident(p, tree_ident(t));

   tree_t w = tree_new(T_WAIT);
   tree_set_ident(w, ident_new("cassign"));

   tree_t container = p;  // Where to add new statements
   void (*add_stmt)(tree_t, tree_t) = tree_add_stmt;

   for (unsigned i = 0; i < tree_conds(t); i++) {
      tree_t c = tree_cond(t, i);

      if (tree_has_value(c)) {
         // Replace this with an if statement
         tree_t i = tree_new(T_IF);
         tree_set_value(i, tree_value(c));
         tree_set_ident(i, ident_uniq("cond"));

         tree_visit_only(tree_value(c), simp_build_wait, w, T_REF);

         (*add_stmt)(container, i);

         container = i;
         add_stmt  = tree_add_stmt;
      }

      tree_t s = tree_new(T_SIGNAL_ASSIGN);
      tree_set_loc(s, tree_loc(t));
      tree_set_target(s, tree_target(t));
      tree_set_ident(s, tree_ident(t));

      for (unsigned i = 0; i < tree_waveforms(c); i++) {
         tree_t wave = tree_waveform(c, i);
         tree_add_waveform(s, wave);
         tree_visit_only(wave, simp_build_wait, w, T_REF);
      }

      (*add_stmt)(container, s);

      if (tree_has_value(c)) {
         // Add subsequent statements to the else part
         add_stmt = tree_add_else_stmt;
      }
   }

   tree_add_stmt(p, w);
   return p;
}

static tree_t simp_check_bounds(tree_t i, int64_t low, int64_t high)
{
   literal_t folded;
   if (folded_int(i, &folded)) {
      if (folded.i < low || folded.i > high)
         simp_error(i, "index out of bounds");
   }
   return NULL;
}

static tree_t simp_aggregate(tree_t t)
{
   type_t type = tree_type(t);
   if (type_kind(type) != T_CARRAY)
      return t;

   range_t r = type_dim(type, 0);
   if (tree_kind(r.left) != T_LITERAL || tree_kind(r.right) != T_LITERAL)
      return t;

   // Check for out of bounds indexes

   int64_t low, high;
   range_bounds(type_dim(type, 0), &low, &high);

   for (unsigned i = 0; i < tree_assocs(t); i++) {
      assoc_t a = tree_assoc(t, i);

      switch (a.kind) {
      case A_NAMED:
         if (simp_check_bounds(a.name, low, high))
            return t;
         break;

      case A_RANGE:
         if (simp_check_bounds(a.range.left, low, high)
             || simp_check_bounds(a.range.right, low, high))
            return t;
         break;

      default:
         break;
      }
   }

   return t;
}

static tree_t simp_select(tree_t t)
{
   // Replace a select statement with a case inside a process

   tree_t p = tree_new(T_PROCESS);
   tree_set_ident(p, tree_ident(t));

   tree_t w = tree_new(T_WAIT);
   tree_set_ident(w, ident_new("select_wait"));

   tree_t c = tree_new(T_CASE);
   tree_set_ident(c, ident_new("select_case"));
   tree_set_loc(c, tree_loc(t));
   tree_set_value(c, tree_value(t));

   tree_visit_only(tree_value(t), simp_build_wait, w, T_REF);

   for (unsigned i = 0; i < tree_assocs(t); i++) {
      assoc_t a = tree_assoc(t, i);
      tree_add_assoc(c, a);

      if (a.kind == A_NAMED)
         tree_visit_only(a.name, simp_build_wait, w, T_REF);

      for (unsigned j = 0; j < tree_waveforms(a.value); j++)
         tree_visit_only(tree_waveform(a.value, j), simp_build_wait, w, T_REF);
   }

   tree_add_stmt(p, c);
   tree_add_stmt(p, w);
   return p;
}

static tree_t simp_tree(tree_t t, void *context)
{
   switch (tree_kind(t)) {
   case T_PROCESS:
      return simp_process(t);
   case T_ARRAY_REF:
      return simp_array_ref(t);
   case T_ATTR_REF:
      return simp_attr_ref(t);
   case T_FCALL:
      return simp_fcall(t);
   case T_REF:
      return simp_ref(t);
   case T_IF:
      return simp_if(t);
   case T_WHILE:
      return simp_while(t);
   case T_FOR:
      return simp_for(t);
   case T_CASSIGN:
      return simp_cassign(t);
   case T_AGGREGATE:
      return simp_aggregate(t);
   case T_SELECT:
      return simp_select(t);
   case T_WAIT:
      return simp_wait(t);
   case T_NULL:
      return NULL;   // Delete it
   default:
      return t;
   }
}

static void simp_intern_strings(void)
{
   // Intern some commonly used strings

   std_bool_i = ident_new("STD.STANDARD.BOOLEAN");
   builtin_i  = ident_new("builtin");
}

void simplify(tree_t top)
{
   static bool have_interned = false;
   if (!have_interned) {
      simp_intern_strings();
      have_interned = true;
   }

   tree_rewrite(top, simp_tree, NULL);
}

int simplify_errors(void)
{
   return errors;
}
