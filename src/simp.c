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

static tree_t simp_expr(tree_t t);

static ident_t std_bool_i = NULL;
static ident_t builtin_i  = NULL;

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

   literal_t l = { .kind = L_INT, .i = i };

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
   assert(tree_kind(decl) == T_FUNC_DECL);

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
      tree_t p = simp_expr(tree_param(t, i));
      tree_change_param(t, i, p);
      can_fold_num = can_fold_num && folded_num(p, &largs[i]);
      can_fold_log = can_fold_log && folded_bool(p, &bargs[i]);
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
      return simp_expr(tree_value(decl));
   default:
      return t;
   }
}

static tree_t simp_attr_ref(tree_t t)
{
   if (tree_has_value(t))
      return simp_expr(tree_value(t));
   else {
      tree_t decl = tree_ref(t);
      assert(tree_kind(decl) == T_FUNC_DECL);

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

static tree_t simp_expr(tree_t t)
{
   switch (tree_kind(t)) {
   case T_LITERAL:
      return t;

   case T_FCALL:
      return simp_fcall(t);

   case T_REF:
      return simp_ref(t);

   case T_AGGREGATE:
      {
         for (unsigned i = 0; i < tree_assocs(t); i++) {
            assoc_t a = tree_assoc(t, i);
            a.value = simp_expr(a.value);

            switch (a.kind) {
            case A_POS:
            case A_OTHERS:
               break;
            case A_NAMED:
               a.name = simp_expr(a.name);
               break;
            case A_RANGE:
               a.range.left  = simp_expr(a.range.left);
               a.range.right = simp_expr(a.range.right);
               break;
            }

            tree_change_assoc(t, i, a);
         }
         return t;
      }

   case T_ATTR_REF:
      return simp_attr_ref(t);

   default:
      assert(false);
   }
}

static void simp_type_decl(tree_t t)
{
   type_t type = tree_type(t);

   switch (type_kind(type)) {
   case T_INTEGER:
   case T_PHYSICAL:
   case T_CARRAY:
      {
         for (unsigned i = 0; i < type_dims(type); i++) {
            range_t r = type_dim(type, i);
            r.left  = simp_expr(r.left);
            r.right = simp_expr(r.right);
            type_change_dim(type, i, r);
         }
      }
      break;

   default:
      break;
   }
}

static void simp_decl(tree_t t)
{
   switch (tree_kind(t)) {
   case T_SIGNAL_DECL:
   case T_VAR_DECL:
   case T_CONST_DECL:
      if (tree_has_value(t))
         tree_set_value(t, simp_expr(tree_value(t)));
      break;

   case T_TYPE_DECL:
      simp_type_decl(t);
      break;

   case T_FUNC_DECL:
      break;

   default:
      assert(false);
   }
}

static tree_t simp_stmt(tree_t t)
{
   switch (tree_kind(t)) {
   case T_PROCESS:
      {
         for (unsigned i = 0; i < tree_decls(t); i++)
            simp_decl(tree_decl(t, i));

         for (unsigned i = 0; i < tree_stmts(t); i++)
            tree_change_stmt(t, i, simp_stmt(tree_stmt(t, i)));

         return t;
      }

   case T_VAR_ASSIGN:
   case T_SIGNAL_ASSIGN:
      tree_set_target(t, simp_expr(tree_target(t)));
      tree_set_value(t, simp_expr(tree_value(t)));
      return t;

   case T_ASSERT:
      tree_set_value(t, simp_expr(tree_value(t)));
      tree_set_severity(t, simp_expr(tree_severity(t)));
      tree_set_message(t, simp_expr(tree_message(t)));
      return t;

   case T_WAIT:
      if (tree_has_delay(t))
         tree_set_delay(t, simp_expr(tree_delay(t)));
      return t;

   default:
      assert(false);
   }
}

static void simp_entity(tree_t t)
{
   for (unsigned i = 0; i < tree_generics(t); i++)
      simp_decl(tree_generic(t, i));

   for (unsigned i = 0; i < tree_ports(t); i++)
      simp_decl(tree_port(t, i));
}

static void simp_arch(tree_t t)
{
   for (unsigned i = 0; i < tree_decls(t); i++)
      simp_decl(tree_decl(t, i));

   for (unsigned i = 0; i < tree_stmts(t); i++)
      tree_change_stmt(t, i, simp_stmt(tree_stmt(t, i)));
}

static void simp_package(tree_t t)
{
   for (unsigned i = 0; i < tree_decls(t); i++)
      simp_decl(tree_decl(t, i));
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

   switch (tree_kind(top)) {
   case T_ENTITY:
      simp_entity(top);
      break;
   case T_ARCH:
      simp_arch(top);
      break;
   case T_PACKAGE:
      simp_package(top);
      break;
   default:
      assert(false);
   }
}
