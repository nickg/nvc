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
#include <ctype.h>
#include <inttypes.h>

static void dump_expr(tree_t t);

static void tab(int indent)
{
   while (indent--)
      fputc(' ', stdout);
}

static void dump_params(tree_t t)
{
   if (tree_params(t) > 0) {
      printf("(");
      for (unsigned i = 0; i < tree_params(t); i++) {
         if (i > 0)
            printf(", ");
         param_t p = tree_param(t, i);
         switch (p.kind) {
         case P_POS:
            break;
         case P_RANGE:
            assert(false);
         case P_NAMED:
            printf("%s => ", istr(p.name));
            break;
         }
         dump_expr(p.value);
      }
      printf(")");
   }
}

static void dump_expr(tree_t t)
{
   switch (tree_kind(t)) {
   case T_FCALL:
      {
         const char *name = istr(tree_ident(tree_ref(t)));
         if (isalpha(name[0]))
            printf("%s", name);
         else
            printf("\"%s\"", name);
         dump_params(t);
      }
      break;

   case T_LITERAL:
      {
         literal_t l = tree_literal(t);
         switch (l.kind) {
         case L_INT:
            printf("%"PRIi64, l.i);
            break;
         default:
            assert(false);
         }
      }
      break;

   case T_AGGREGATE:
      printf("(");
      for (unsigned i = 0; i < tree_assocs(t); i++) {
         if (i > 0)
            printf(", ");
         assoc_t a = tree_assoc(t, i);
         switch (a.kind) {
         case A_POS:
            dump_expr(a.value);
            break;
         case A_OTHERS:
            printf("others => ");
            dump_expr(a.value);
            break;
         default:
            assert(false);
         }
      }
      printf(")");
      break;

   case T_REF:
      printf("%s", istr(tree_ident(tree_ref(t))));
      break;

   case T_ARRAY_REF:
      printf("%s", istr(tree_ident(tree_ref(t))));
      dump_params(t);
      break;

   case T_ARRAY_SLICE:
      {
         range_t r = tree_range(t);
         printf("%s(", istr(tree_ident(tree_ref(t))));
         dump_expr(r.left);
         printf(" %s ", r.kind == RANGE_TO ? "to" : "downto");
         dump_expr(r.right);
         printf(")");
      }
      break;

   default:
      assert(false);
   }
}

static void dump_type(type_t type)
{
   if (type_kind(type) == T_CARRAY) {
      printf("%s(", istr(type_ident(type)));
      for (unsigned i = 0; i < type_dims(type); i++) {
         if (i > 0)
            printf(", ");
         range_t r = type_dim(type, i);
         dump_expr(r.left);
         switch (r.kind) {
         case RANGE_TO:
            printf(" to ");
            dump_expr(r.right);
            break;
         case RANGE_DOWNTO:
            printf(" downto ");
            dump_expr(r.right);
            break;
         case RANGE_EXPR:
            break;
         }
      }
      printf(")");
   }
   else
      printf("%s", type_pp(type));
}

static void dump_decl(tree_t t, int indent)
{
   tab(indent);

   switch (tree_kind(t)) {
   case T_SIGNAL_DECL:
      printf("signal %s : ", istr(tree_ident(t)));
      break;

   case T_VAR_DECL:
      printf("variable %s : ", istr(tree_ident(t)));
      break;

   case T_CONST_DECL:
      printf("constant %s : ", istr(tree_ident(t)));
      break;

   case T_TYPE_DECL:
      printf("type %s is ", istr(tree_ident(t)));
      break;

   case T_ALIAS:
      printf("alias %s is ", istr(tree_ident(t)));
      dump_expr(tree_value(t));
      printf(";\n");
      return;

   case T_ATTR_SPEC:
      printf("TODO: T_ATTR_SPEC\n");
      break;

   case T_ATTR_DECL:
      printf("TODO: T_ATTR_DECL\n");
      break;

   default:
      assert(false);
   }

   dump_type(tree_type(t));

   if (tree_has_value(t)) {
      printf(" := ");
      dump_expr(tree_value(t));
   }
   printf(";\n");
}

static void dump_stmt(tree_t t, int indent)
{
   tab(indent);
   printf("%s: ", istr(tree_ident(t)));

   switch (tree_kind(t)) {
   case T_PROCESS:
      printf("process ");
      if (tree_triggers(t) > 0) {
         printf("(");
         for (unsigned i = 0; i < tree_triggers(t); i++) {
            if (i > 0)
               printf(", ");
            dump_expr(tree_trigger(t, i));
         }
         printf(") ");
      }
      printf("is\n");
      for (unsigned i = 0; i < tree_decls(t); i++)
         dump_decl(tree_decl(t, i), indent + 2);
      tab(indent);
      printf("begin\n");
      for (unsigned i = 0; i < tree_stmts(t); i++)
         dump_stmt(tree_stmt(t, i), indent + 2);
      tab(indent);
      printf("end process;\n\n");
      return;

   case T_SIGNAL_ASSIGN:
      dump_expr(tree_target(t));
      printf(" <= ");
      for (unsigned i = 0; i < tree_waveforms(t); i++) {
         if (i > 0)
            printf(", ");
         tree_t w = tree_waveform(t, i);
         dump_expr(tree_value(w));
         if (tree_has_delay(w)) {
            printf(" after ");
            dump_expr(tree_delay(w));
         }
      }
      break;

   case T_VAR_ASSIGN:
      dump_expr(tree_target(t));
      printf(" := ");
      dump_expr(tree_value(t));
      break;

   case T_WAIT:
      printf("wait");
      if (tree_triggers(t) > 0) {
         printf(" on ");
         for (unsigned i = 0; i < tree_triggers(t); i++) {
            if (i > 0)
               printf(", ");
            dump_expr(tree_trigger(t, i));
         }
      }
      if (tree_has_delay(t)) {
         printf(" for ");
         dump_expr(tree_delay(t));
      }
      break;

   case T_ASSERT:
      printf("assert ");
      dump_expr(tree_value(t));
      printf(" report ");
      dump_expr(tree_message(t));
      printf(" severity ");
      dump_expr(tree_severity(t));
      break;

   case T_WHILE:
      printf("while ");
      dump_expr(tree_value(t));
      printf(" loop\n");
      for (unsigned i = 0; i < tree_stmts(t); i++)
         dump_stmt(tree_stmt(t, i), indent + 2);
      tab(indent);
      printf("end loop");
      break;

   case T_IF:
      printf("if ");
      dump_expr(tree_value(t));
      printf(" then\n");
      for (unsigned i = 0; i < tree_stmts(t); i++)
         dump_stmt(tree_stmt(t, i), indent + 2);
      if (tree_else_stmts(t) > 0) {
         tab(indent);
         printf("else\n");
         for (unsigned i = 0; i < tree_else_stmts(t); i++)
            dump_stmt(tree_else_stmt(t, i), indent + 2);
      }
      tab(indent);
      printf("end if");
      break;

   default:
      assert(false);
   }

   printf(";\n");
}

static void dump_port(tree_t t, int indent)
{
   // TODO
}

static void dump_elab(tree_t t)
{
   printf("entity %s is\nend entity;\n\n", istr(tree_ident(t)));
   printf("architecture elab of %s is\n", istr(tree_ident(t)));
   for (unsigned i = 0; i < tree_decls(t); i++)
      dump_decl(tree_decl(t, i), 2);
   printf("begin\n");
   for (unsigned i = 0; i < tree_stmts(t); i++)
      dump_stmt(tree_stmt(t, i), 2);
   printf("end architecture;\n");
}

static void dump_entity(tree_t t)
{
   printf("entity %s is\n", istr(tree_ident(t)));
   for (unsigned i = 0; i < tree_ports(t); i++)
      dump_port(tree_port(t, i), 2);
   printf("end entity;\n");
}

static void dump_arch(tree_t t)
{
   printf("architecture %s of %s is\n",
          istr(tree_ident(t)), istr(tree_ident2(t)));
   for (unsigned i = 0; i < tree_decls(t); i++)
      dump_decl(tree_decl(t, i), 2);
   printf("begin\n");
   for (unsigned i = 0; i < tree_stmts(t); i++)
      dump_stmt(tree_stmt(t, i), 2);
   printf("end architecture;\n");
}

void dump(tree_t t)
{
   switch (tree_kind(t)) {
   case T_ELAB:
      dump_elab(t);
      break;
   case T_ENTITY:
      dump_entity(t);
      break;
   case T_ARCH:
      dump_arch(t);
      break;
   default:
      assert(false);
   }
}
