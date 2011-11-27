//
//  Copyright (C) 2011  Nick Gasson
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

#define MAX_BUILTIN_ARGS 2

static ident_t std_bool_i = NULL;
static ident_t builtin_i  = NULL;

static int errors = 0;

#define simp_error(t, ...) \
   { errors++; error_at(tree_loc(t), __VA_ARGS__); return t; }

static tree_t simp_tree(tree_t t, void *context);

static bool folded_num(tree_t t, literal_t *l)
{
   if (tree_kind(t) == T_LITERAL) {
      *l = tree_literal(t);
      return true;
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
   tree_set_type(f, type_result(tree_type(fdecl)));

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

static tree_t simp_fcall_log(tree_t t, const char *builtin, bool *args)
{
   if (strcmp(builtin, "not") == 0)
      return get_bool_lit(t, !args[0]);
   else if (strcmp(builtin, "and") == 0)
      return get_bool_lit(t, args[0] && args[1]);
   else if (strcmp(builtin, "nand") == 0)
      return get_bool_lit(t, !(args[0] && args[1]));
   else if (strcmp(builtin, "or") == 0)
      return get_bool_lit(t, args[0] || args[1]);
   else if (strcmp(builtin, "nor") == 0)
      return get_bool_lit(t, !(args[0] || args[1]));
   else if (strcmp(builtin, "xor") == 0)
      return get_bool_lit(t, args[0] ^ args[1]);
   else if (strcmp(builtin, "xnor") == 0)
      return get_bool_lit(t, !(args[0] ^ args[1]));

   fatal("cannot fold builtin %s", builtin);
}

static tree_t simp_fcall_num(tree_t t, const char *builtin, literal_t *args)
{
   const int lkind = args[0].kind;  // Assume all types checked same
   assert(lkind == L_INT);

   if (strcmp(builtin, "mul") == 0) {
      return get_int_lit(t, args[0].i * args[1].i);
   }
   else if (strcmp(builtin, "div") == 0) {
      return get_int_lit(t, args[0].i / args[1].i);
   }
   else if (strcmp(builtin, "add") == 0) {
      return get_int_lit(t, args[0].i + args[1].i);
   }
   else if (strcmp(builtin, "sub") == 0) {
      return get_int_lit(t, args[0].i - args[1].i);
   }
   else if (strcmp(builtin, "neg") == 0) {
      return get_int_lit(t, -args[0].i);
   }
   else if (strcmp(builtin, "identity") == 0) {
      return get_int_lit(t, args[0].i);
   }
   else if (strcmp(builtin, "eq") == 0) {
      if (args[0].kind == L_INT && args[1].kind == L_INT)
         return get_bool_lit(t, args[0].i == args[1].i);
      else
         assert(false);
   }
   else if (strcmp(builtin, "neq") == 0) {
      if (args[0].kind == L_INT && args[1].kind == L_INT)
         return get_bool_lit(t, args[0].i != args[1].i);
      else
         assert(false);
   }
   else if (strcmp(builtin, "gt") == 0) {
      if (args[0].kind == L_INT && args[1].kind == L_INT)
         return get_bool_lit(t, args[0].i > args[1].i);
      else
         assert(false);
   }
   else if (strcmp(builtin, "lt") == 0) {
      if (args[0].kind == L_INT && args[1].kind == L_INT)
         return get_bool_lit(t, args[0].i < args[1].i);
      else
         assert(false);
   }

   fatal("cannot fold builtin %s", builtin);
}

static tree_t simp_fcall(tree_t t)
{
   tree_t decl = tree_ref(t);
   assert(tree_kind(decl) == T_FUNC_DECL
          || tree_kind(decl) == T_FUNC_BODY);

   const char *builtin = tree_attr_str(decl, builtin_i);
   if (builtin == NULL)
      return t;     // TODO: expand pure function calls

   if (tree_params(t) > MAX_BUILTIN_ARGS)
      return t;

   bool can_fold_num = true;
   bool can_fold_log = true;
   literal_t largs[MAX_BUILTIN_ARGS];
   bool bargs[MAX_BUILTIN_ARGS];
   for (unsigned i = 0; i < tree_params(t); i++) {
      param_t p = tree_param(t, i);
      assert(p.kind == P_POS);
      can_fold_num = can_fold_num && folded_num(p.value, &largs[i]);
      can_fold_log = can_fold_log && folded_bool(p.value, &bargs[i]);
   }

   if (can_fold_num)
      return simp_fcall_num(t, builtin, largs);
   else if (can_fold_log)
      return simp_fcall_log(t, builtin, bargs);
   else
      return t;
}

static tree_t simp_ref(tree_t t)
{
   tree_t decl = tree_ref(t);

   switch (tree_kind(decl)) {
   case T_CONST_DECL:
      if (type_kind(tree_type(decl)) == T_PHYSICAL) {
         // Slight hack to constant-fold the definitions of
         // physical units that and generated during the sem phase
         return tree_rewrite(tree_value(decl), simp_tree, NULL);
      }
      else
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

      const char *builtin = tree_attr_str(decl, builtin_i);
      assert(builtin != NULL);

      if (strcmp(builtin, "length") == 0) {
         tree_t array = tree_param(t, 0).value;
         if (type_kind(tree_type(array)) == T_CARRAY) {
            int64_t low, high;
            range_bounds(type_dim(tree_type(array), 0), &low, &high);
            return get_int_lit(t, high - low + 1);
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
   literal_t indexes[tree_params(t)];
   bool can_fold = true;
   for (unsigned i = 0; i < tree_params(t); i++) {
      param_t p = tree_param(t, i);
      assert(p.kind == P_POS);
      can_fold = can_fold && folded_num(p.value, &indexes[i]);
   }

   if (!can_fold)
      return t;

   assert(tree_params(t) == 1);

   tree_t decl = tree_ref(t);
   // XXX: may not be decl e.g. nested array ref

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

static tree_t simp_if(tree_t t)
{
   bool value_b;
   if (folded_bool(tree_value(t), &value_b)) {
      if (value_b) {
         // If statement always executes so replace with then part
         // XXX: make work for more than one statement
         if (tree_stmts(t) == 1)
            return tree_stmt(t, 0);
         else
            return t;
      }
      else {
         // If statement never executes so replace with else part
         if (tree_else_stmts(t) == 1) {
            // XXX: make work for more than one statement
            return tree_else_stmt(t, 0);
         }
         else if (tree_else_stmts(t) == 0)
            return NULL;   // Delete it
         else
            return t;
      }
   }
   else
      return t;
}

static tree_t simp_while(tree_t t)
{
   bool value_b;
   if (folded_bool(tree_value(t), &value_b) && !value_b) {
      // Condition is false so loop never executes
      return NULL;
   }
   else
      return t;
}

static void simp_build_wait(tree_t ref, void *context)
{
   tree_t wait = context;

   if (tree_kind(tree_ref(ref)) == T_SIGNAL_DECL)
      tree_add_trigger(wait, ref);
}

static tree_t simp_cassign(tree_t t)
{
   // Replace concurrent assignments with a process

   tree_t p = tree_new(T_PROCESS);
   tree_set_ident(p, tree_ident(t));

   tree_t w = tree_new(T_WAIT);
   tree_set_ident(w, ident_new("cassign"));

   tree_t s = tree_new(T_SIGNAL_ASSIGN);
   tree_set_loc(s, tree_loc(t));
   tree_set_target(s, tree_target(t));
   tree_set_ident(s, tree_ident(t));

   for (unsigned i = 0; i < tree_waveforms(t); i++) {
      tree_t wave = tree_waveform(t, i);
      tree_add_waveform(s, wave);
      tree_visit_only(wave, simp_build_wait, w, T_REF);
   }

   tree_add_stmt(p, s);
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
   case T_CASSIGN:
      return simp_cassign(t);
   case T_NULL:
      return NULL;   // Delete it
   default:
      return t;
   }
}

static void simp_intern_strings(void)
{
   // Intern some commponly used strings

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
