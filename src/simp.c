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

#define FOLD_BINARY(args, l, op)                          \
   if (args[0].kind == L_INT && args[1].kind == L_INT) {  \
      l.i = args[0].i op args[1].i;                       \
      l.kind = L_INT;                                     \
   }                                                      \
   else                                                   \
      assert(false);                                      \

#define FOLD_UNARY(args, l, op)                           \
   if (args[0].kind == L_INT) {                           \
      l.i = op args[0].i;                                 \
      l.kind = L_INT;                                     \
   }                                                      \
   else                                                   \
      assert(false);                                      \

static tree_t simp_expr(tree_t t);

static bool folded(tree_t t, literal_t *l)
{
   if (tree_kind(t) == T_LITERAL) {
      *l = tree_literal(t);
      return true;
   }
   else
      return false;
}

static tree_t simp_fcall(tree_t t)
{
   tree_t decl = tree_ref(t);
   assert(tree_kind(decl) == T_FUNC_DECL);

   const char *builtin = tree_attr_str(decl, ident_new("builtin"));
   if (builtin == NULL)
      return t;     // TODO: expand pure function calls

   if (tree_params(t) > MAX_BUILTIN_ARGS)
      return t;

   bool can_fold = true;
   literal_t args[MAX_BUILTIN_ARGS];
   for (unsigned i = 0; i < tree_params(t); i++) {
      tree_t p = simp_expr(tree_param(t, i));
      tree_change_param(t, i, p);
      can_fold = can_fold && folded(p, &args[i]);
   }

   if (!can_fold)
      return t;

   literal_t l;
   if (strcmp(builtin, "mul") == 0) {
      FOLD_BINARY(args, l, *);
   }
   else if (strcmp(builtin, "add") == 0) {
      FOLD_BINARY(args, l, +);
   }
   else if (strcmp(builtin, "sub") == 0) {
      FOLD_BINARY(args, l, -);
   }
   else if (strcmp(builtin, "neg") == 0) {
      FOLD_UNARY(args, l, -);
   }
   else
      fatal("cannot fold builtin %s", builtin);

   tree_t f = tree_new(T_LITERAL);
   tree_set_loc(f, tree_loc(t));
   tree_set_literal(f, l);

   return f;
}

static tree_t simp_expr(tree_t t)
{
   switch (tree_kind(t)) {
   case T_LITERAL:
      return t;

   case T_FCALL:
      return simp_fcall(t);

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
}

static void simp_package(tree_t t)
{
   for (unsigned i = 0; i < tree_decls(t); i++)
      simp_decl(tree_decl(t, i));
}

void simplify(tree_t top)
{
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
