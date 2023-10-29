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
#include "common.h"
#include "hash.h"
#include "ident.h"
#include "phase.h"
#include "psl/psl-phase.h"
#include "tree.h"
#include "type.h"
#include "vlog/vlog-phase.h"

#include <assert.h>
#include <string.h>
#include <ctype.h>
#include <inttypes.h>

#define DUMP_TYPE_HINT  0
#define DUMP_ADDRESS    0
#define DUMP_STATICNESS 0
#define DUMP_GEN_NAMES  0

static void dump_expr(tree_t t);
static void dump_stmt(tree_t t, int indent);
static void dump_port(tree_t t, int indent);
static void dump_decl(tree_t t, int indent);
static void dump_decls(tree_t t, int indent);
static void dump_type(type_t type);
static void dump_package(tree_t t, int indent);
static void dump_package_body(tree_t t, int indent);
static void dump_constraint(tree_t t);
static void dump_elem_constraints(type_t type);

typedef tree_t (*get_fn_t)(tree_t, unsigned);

static void tab(int indent)
{
   print_syntax("%*s", indent, "");
}

static void cannot_dump(tree_t t, const char *hint)
{
   print_syntax("\n");
   fflush(stdout);
   fatal("cannot dump %s kind %s", hint, tree_kind_str(tree_kind(t)));
}

static void dump_param(tree_t p)
{
   switch (tree_subkind(p)) {
   case P_POS:
      break;
   case P_NAMED:
      dump_expr(tree_name(p));
      print_syntax(" => ");
      break;
   }
   dump_expr(tree_value(p));
}

static void dump_params(tree_t t, get_fn_t get, int n, const char *prefix)
{
   if (n > 0) {
      if (prefix != NULL) {
         print_syntax(prefix, "");
         print_syntax(" ");
      }
      print_syntax("(");
      for (int i = 0; i < n; i++) {
         if (i > 0)
            print_syntax(", ");
         dump_param((*get)(t, i));
      }
      print_syntax(")");
   }
}

static void dump_range(tree_t r)
{
   switch (tree_subkind(r)) {
   case RANGE_TO:
      dump_expr(tree_left(r));
      print_syntax(" #to ");
      dump_expr(tree_right(r));
      break;
   case RANGE_DOWNTO:
      dump_expr(tree_left(r));
      print_syntax(" #downto ");
      dump_expr(tree_right(r));
      break;
   case RANGE_EXPR:
   case RANGE_ERROR:
      if (tree_has_value(r))
         dump_expr(tree_value(r));
      return;
   }
}

static void dump_type_hint(tree_t t)
{
#if DUMP_TYPE_HINT
   static int nested = 0;
   if (++nested == 1 && tree_has_type(t)) {
      color_printf("$red$$<$/*");
      dump_type(tree_type(t));
      color_printf("$>$$red$*/$$");
   }
   --nested;
#endif
}

static void dump_address(tree_t t)
{
#if DUMP_ADDRESS
   uint32_t a = (uint32_t)((uintptr_t)tree_arena(t) >> 2);
   a = (a ^ 61) ^ (a >> 16);
   a = a + (a << 3);
   a = a ^ (a >> 4);
   a = a * UINT32_C(0x27d4eb2d);
   a = a ^ (a >> 15);

   const int r = 1 + a % 5;
   const int g = 1 + (a >> 8) % 5;
   const int b = 1 + (a >> 16) % 5;

   const int color = 16 + 36*r + 6*g + b;

   char *LOCAL fmt = xasprintf("$!#%d${%%p}$$", color);
   color_printf(fmt, t);
#endif
}

static void dump_waveform(tree_t w)
{
   if (tree_has_value(w))
      dump_expr(tree_value(w));
   else
      print_syntax("#null");

   if (tree_has_delay(w)) {
      print_syntax(" #after ");
      dump_expr(tree_delay(w));
   }
}

static void dump_external_name(tree_t t)
{
   print_syntax("<< #%s ", class_str(tree_class(t)));
   const int nparts = tree_parts(t);
   for (int i = 0; i < nparts; i++) {
      tree_t part = tree_part(t, i);
      switch (tree_subkind(part)) {
      case PE_ABSOLUTE:
         print_syntax("^.");
         break;
      case PE_SIMPLE:
         print_syntax("%s%s", istr(tree_ident(part)),
                      i + 1 < nparts ? "." : "");
         break;
      case PE_CARET:
         print_syntax("^.");
         break;
      }
   }
   print_syntax(" : ");
   dump_type(tree_type(t));
   print_syntax(" >>");
}

static void dump_expr(tree_t t)
{
   switch (tree_kind(t)) {
   case T_PROT_FCALL:
      dump_expr(tree_name(t));
      print_syntax(".");
      // Fall-through
   case T_FCALL:
      if (tree_has_ref(t)) {
         tree_t decl = tree_ref(t);
         dump_address(decl);
         print_syntax("%s", istr(tree_ident(decl)));
      }
      else
         print_syntax("%s", istr(tree_ident(t)));
      dump_params(t, tree_param, tree_params(t), NULL);
#if DUMP_STATICNESS
      if (tree_flags(t) & TREE_F_LOCALLY_STATIC)
         color_printf("$red$/* locally static */$$");
      else if (tree_flags(t) & TREE_F_GLOBALLY_STATIC)
         color_printf("$red$/* globally static */$$");
#endif
      break;

   case T_CONV_FUNC:
      if (tree_has_ref(t)) {
         tree_t decl = tree_ref(t);
         dump_address(decl);
         print_syntax("%s", istr(tree_ident(decl)));
      }
      else
         print_syntax("%s", istr(tree_ident(t)));
      print_syntax("(");
      dump_expr(tree_value(t));
      print_syntax(")");
      break;

   case T_LITERAL:
      switch (tree_subkind(t)) {
      case L_INT:
         print_syntax("%"PRIi64, tree_ival(t));
         break;
      case L_PHYSICAL:
         if (tree_has_ident(t))
            print_syntax("%"PRIi64" %s", tree_ival(t), istr(tree_ident(t)));
         else
            print_syntax("%"PRIi64, tree_ival(t));
         break;
      case L_REAL:
         print_syntax("%lf", tree_dval(t));
         break;
      case L_NULL:
         print_syntax("#null");
         break;
      default:
         assert(false);
      }
      break;

   case T_STRING:
      {
         print_syntax("\"");
         const int nchars = tree_chars(t);
         for (int i = 0; i < nchars; i++) {
            ident_t rune = tree_ident(tree_char(t, i));
            if (ident_char(rune, 0) == '\'')
               print_syntax("%c", ident_char(rune, 1));
            else
               print_syntax("\" & %s & \"", istr(rune));
         }
         print_syntax("\"");
      }
      break;

   case T_NEW:
      print_syntax("#new ");
      dump_expr(tree_value(t));
      break;

   case T_ALL:
      if (tree_has_value(t)) {
         dump_expr(tree_value(t));
         print_syntax(".#all");
      }
      else
         print_syntax("#all");
      break;

   case T_TYPE_REF:
      dump_type(tree_type(t));
      break;

   case T_AGGREGATE:
      print_syntax("(");
      for (unsigned i = 0; i < tree_assocs(t); i++) {
         if (i > 0)
            print_syntax(", ");
         tree_t a = tree_assoc(t, i);
         tree_t value = tree_value(a);
         switch (tree_subkind(a)) {
         case A_POS:
            dump_expr(value);
            break;
         case A_NAMED:
            dump_expr(tree_name(a));
            print_syntax(" => ");
            dump_expr(value);
            break;
         case A_OTHERS:
            print_syntax("#others => ");
            dump_expr(value);
            break;
         case A_RANGE:
            dump_range(tree_range(a, 0));
            print_syntax(" => ");
            dump_expr(value);
            break;
         default:
            assert(false);
         }
      }
      print_syntax(")");
      break;

   case T_REF:
      if (tree_has_ref(t)) {
         tree_t decl = tree_ref(t);
         dump_address(decl);
         print_syntax("%s", istr(tree_ident(decl)));
      }
      else
         print_syntax("%s", istr(tree_ident(t)));
      break;

   case T_ATTR_REF:
      dump_expr(tree_name(t));
      print_syntax("'%s", istr(tree_ident(t)));
      if (tree_params(t) > 0)
         dump_params(t, tree_param, tree_params(t), NULL);
      break;

   case T_EXTERNAL_NAME:
      dump_external_name(t);
      break;

   case T_ARRAY_REF:
      dump_expr(tree_value(t));
      dump_params(t, tree_param, tree_params(t), NULL);
      break;

   case T_ARRAY_SLICE:
      dump_expr(tree_value(t));
      print_syntax("(");
      dump_range(tree_range(t, 0));
      print_syntax(")");
      break;

   case T_RECORD_REF:
      dump_expr(tree_value(t));
      print_syntax(".%s", istr(tree_ident(t)));
      break;

   case T_TYPE_CONV:
      print_syntax("%s(", istr(type_ident(tree_type(t))));
      dump_expr(tree_value(t));
      print_syntax(")");
      break;

   case T_QUALIFIED:
      if (tree_has_value(t)) {
         print_syntax("%s'(", istr(type_ident(tree_type(t))));
         dump_expr(tree_value(t));
         print_syntax(")");
      }
      else
         dump_type(tree_type(t));
      break;

   case T_OPEN:
      print_syntax("#open");
      break;

   case T_BOX:
      print_syntax("<>");
      break;

   case T_WAVEFORM:
      dump_waveform(t);
      break;

   case T_PACKAGE_MAP:
      print_syntax("%s ", istr(tree_ident(t)));
      switch (tree_subkind(t)) {
      case PACKAGE_MAP_BOX:
         print_syntax("#generic #map (<>)");
         break;
      case PACKAGE_MAP_DEFAULT:
         print_syntax("#generic map (#default)");
         break;
      case PACKAGE_MAP_MATCHING:
         dump_params(t, tree_genmap, tree_genmaps(t), "#generic #map");
         break;
      }
      break;

   case T_COND_VALUE:
      for (int i = 0; i < tree_conds(t); i++) {
         tree_t c = tree_cond(t, i);
         if (i > 0)
            print_syntax(" #else ");
         if (tree_has_result(c))
            dump_expr(tree_result(c));
         else
            print_syntax("#unaffected");
         if (tree_has_value(c)) {
            print_syntax(" #when ");
            dump_expr(tree_value(c));
         }
      }
      break;

   default:
      cannot_dump(t, "expr");
   }

   dump_type_hint(t);
}

static void dump_record_elem_constraint(tree_t t)
{
   print_syntax("%s", istr(tree_ident(t)));

   type_t ftype = tree_type(t);
   const int ncon = type_constraints(ftype);
   for (int i = 0; i < ncon; i++)
      dump_constraint(type_constraint(ftype, i));

   dump_elem_constraints(ftype);
}

static void dump_constraint(tree_t t)
{
   const int nranges = tree_ranges(t);

   switch (tree_subkind(t)) {
   case C_RANGE:
      print_syntax(" #range ");
      dump_range(tree_range(t, 0));
      break;
   case C_INDEX:
      print_syntax("(");
      for (int i = 0; i < nranges; i++) {
         if (i > 0) print_syntax(", ");
         dump_range(tree_range(t, i));
      }
      print_syntax(")");
      break;
   case C_OPEN:
      print_syntax("(#open)");
      break;
   case C_RECORD:
      print_syntax("(");
      for (int i = 0; i < nranges; i++) {
         if (i > 0) print_syntax(", ");
         dump_record_elem_constraint(tree_range(t, i));
      }
      print_syntax(")");
      break;
   }
}

static void dump_elem_constraints(type_t type)
{
   if (type_is_array(type) && type_has_elem(type)) {
      type_t elem = type_elem(type);
      if (type_kind(elem) == T_SUBTYPE && !type_has_ident(elem)) {
         // Anonymous subtype created for element constraints
         assert(type_constraints(elem) == 1);
         dump_constraint(type_constraint(elem, 0));
         dump_elem_constraints(elem);
      }
   }
}

static void dump_type(type_t type)
{
   if (type_kind(type) == T_SUBTYPE && !type_has_ident(type)) {
      // Anonymous subtype
      print_syntax("%s", type_pp(type));
      if (type_ident(type) == type_ident(type_base(type))) {
         const int ncon = type_constraints(type);
         for (int i = 0; i < ncon; i++)
            dump_constraint(type_constraint(type, i));
      }
      dump_elem_constraints(type);
   }
   else if (type_is_none(type))
      print_syntax("/* error */");
   else
      print_syntax("%s", type_pp(type));
}

static void dump_arguments(tree_t t, int indent, const char *trailer)
{
   const int nports = tree_ports(t);
   if (nports > 0) {
      print_syntax(" (");
      if (nports > 1) {
         print_syntax("\n");
         for (int i = 0; i < nports; i++) {
            if (i > 0) print_syntax(";\n");
            dump_port(tree_port(t, i), indent + 4);
         }
      }
      else
         dump_port(tree_port(t, 0), 1);
      print_syntax(" )%s", trailer);
   }
}

static void dump_ports(tree_t t, int indent)
{
   const int nports = tree_ports(t);
   if (nports > 0) {
      tab(indent);
      print_syntax("#port (");
      if (nports > 1) {
         print_syntax("\n");
         for (unsigned i = 0; i < nports; i++) {
            if (i > 0) print_syntax(";\n");
            dump_port(tree_port(t, i), indent + 2);
         }
      }
      else
         dump_port(tree_port(t, 0), 1);
      print_syntax(" );\n");
   }
}

static void dump_generics(tree_t t, int indent, const char *trailer)
{
   const int ngenerics = tree_generics(t);
   if (ngenerics > 0) {
      tab(indent);
      print_syntax("#generic (");
      if (ngenerics > 1) {
         print_syntax("\n");
         for (int i = 0; i < ngenerics; i++) {
            tree_t g = tree_generic(t, i);
            dump_port(g, indent + 2);
            if (i + 1 == ngenerics && (tree_flags(g) & TREE_F_PREDEFINED)) {
               print_syntax(";\n");
               tab(indent - 1);
            }
            else if (i + 1 < ngenerics)
               print_syntax(";\n");
         }
      }
      else
         dump_port(tree_generic(t, 0), 1);
      print_syntax(" )%s", trailer);
   }
}

static void dump_port_map(tree_t t, int indent, const char *trailer)
{
   const int nparams = tree_params(t);
   if (nparams > 0) {
      tab(indent);
      dump_params(t, tree_param, nparams, "#port #map");
      print_syntax("%s", trailer);
   }
}

static void dump_generic_map(tree_t t, int indent, const char *trailer)
{
   const int ngenmaps = tree_genmaps(t);
   if (ngenmaps > 0) {
      tab(indent);
      dump_params(t, tree_genmap, ngenmaps, "#generic #map");
      print_syntax("%s", trailer);
   }
}

static void dump_binding(tree_t t, int indent)
{
   print_syntax("#use %s", istr(tree_ident(t)));
   if (tree_has_ident2(t))
      print_syntax("(%s)", istr(tree_ident2(t)));
   if (tree_genmaps(t) > 0 || tree_params(t) > 0)
      print_syntax("\n");
   dump_generic_map(t, indent + 2, tree_params(t) > 0 ? "\n" : "");
   dump_port_map(t, indent + 2, "");
   print_syntax(";\n");
}

static void dump_stmts(tree_t t, int indent)
{
   const int nstmts = tree_stmts(t);
   for (int i = 0; i < nstmts; i++) {
      tree_t s = tree_stmt(t, i);
      const tree_kind_t kind = tree_kind(s);
      const bool needs_newline =
         kind == T_BLOCK || kind == T_PROCESS || kind == T_INSTANCE;
      if (needs_newline && i > 0)
         print_syntax("\n");
      dump_stmt(s, indent);
   }
}

static void dump_block(tree_t t, int indent)
{
   dump_decls(t, indent + 2);
   tab(indent);
   print_syntax("#begin\n");
   dump_stmts(t, indent + 2);
}

static void dump_wait_level(tree_t t)
{
   const tree_flags_t flags = tree_flags(t);
   if (flags & TREE_F_NEVER_WAITS)
      print_syntax("   -- Never waits");
   else if (flags & TREE_F_HAS_WAIT)
      print_syntax("   -- Contains wait statement");
}

static void dump_type_decl(tree_t t, int indent)
{
   type_t type = tree_type(t);
   const type_kind_t kind = type_kind(type);

   print_syntax("#type %s", istr(tree_ident(t)));

   if (kind == T_INCOMPLETE) {
      print_syntax(";\n");
      return;
   }

   print_syntax(" #is ");

   if (type_is_integer(type) || type_is_real(type)) {
      print_syntax("#range ");
      dump_range(type_dim(type, 0));
   }
   else if (type_is_physical(type)) {
      print_syntax("#range ");
      dump_range(type_dim(type, 0));
      print_syntax("\n");
      tab(indent + 2);
      print_syntax("#units\n");
      {
         const int nunits = type_units(type);
         for (int i = 0; i < nunits; i++) {
            tree_t u = type_unit(type, i);
            tab(indent + 4);
            print_syntax("%s = ", istr(tree_ident(u)));
            dump_expr(tree_value(u));
            print_syntax(";\n");
         }
      }
      tab(indent + 2);
      print_syntax("#end #units");
   }
   else if (type_is_array(type)) {
      print_syntax("#array ");
      if (kind == T_ARRAY) {
         print_syntax("(");
         const int nindex = type_indexes(type);
         for (int i = 0; i < nindex; i++) {
            if (i > 0) print_syntax(", ");
            dump_type(type_index(type, i));
            print_syntax(" #range <>");
         }
         print_syntax(")");
      }
      else if (kind == T_SUBTYPE) {
         const int ncon = type_constraints(type);
         for (int i = 0; i < ncon; i++)
            dump_constraint(type_constraint(type, i));
      }
      else {
         print_syntax("(");
         const int ndims = type_dims(type);
         for (int i = 0; i < ndims; i++) {
            if (i > 0) print_syntax(", ");
            dump_range(type_dim(type, i));
         }
         print_syntax(")");
      }
      print_syntax(" #of ");
      dump_type(type_elem(type));
   }
   else if (type_is_record(type)) {
      print_syntax("#record\n");
      const int nfields = type_fields(type);
      for (int i = 0; i < nfields; i++)
         dump_decl(type_field(type, i), indent + 2);
      tab(indent);
      print_syntax("#end #record");
   }
   else if (kind == T_ENUM) {
      print_syntax("(");
      for (unsigned i = 0; i < type_enum_literals(type); i++) {
         if (i > 0) print_syntax(", ");
         print_syntax("%s", istr(tree_ident(type_enum_literal(type, i))));
      }
      print_syntax(")");
   }
   else if (kind == T_INCOMPLETE)
      ;
   else
      dump_type(type);

   print_syntax(";\n");
}

static void dump_subtype_decl(tree_t t, int indent)
{
   type_t type = tree_type(t);

   print_syntax("#subtype %s #is ", istr(tree_ident(t)));
   if (type_has_resolution(type)) {
      dump_expr(type_resolution(type));
      print_syntax(" ");
   }
   print_syntax("%s", type_pp(type_base(type)));

   const int ncon = type_constraints(type);
   for (int i = 0; i < ncon; i++)
      dump_constraint(type_constraint(type, i));

   dump_elem_constraints(type);

   print_syntax(";\n");
}

static void dump_component(tree_t t, int indent)
{
   print_syntax("#component %s #is\n", istr(tree_ident(t)));
   dump_generics(t, indent + 2, ";\n");
   dump_ports(t, indent + 2);
   print_syntax("#end #component;\n");
}

static void dump_use(tree_t t)
{
   print_syntax("#use %s", istr(tree_ident(t)));
   if (tree_has_ident2(t))
      print_syntax(".%s", istr(tree_ident2(t)));
   print_syntax(";\n");
}

static void dump_decl(tree_t t, int indent)
{
   tab(indent);
   if (tree_kind(t) != T_HIER) dump_address(t);

   switch (tree_kind(t)) {
   case T_IMPLICIT_SIGNAL:
      print_syntax("/* implicit */ ");
      // Fall-through
   case T_SIGNAL_DECL:
      print_syntax("#signal %s : ", istr(tree_ident(t)));
      break;

   case T_VAR_DECL:
      print_syntax("#variable %s : ", istr(tree_ident(t)));
      break;

   case T_CONST_DECL:
      print_syntax("#constant %s : ", istr(tree_ident(t)));
      break;

   case T_GENERIC_DECL:
      // Loop variable in for-generate statement
      print_syntax("/* loop variable */ %s : ", istr(tree_ident(t)));
      break;

   case T_FIELD_DECL:
      print_syntax("%s : ", istr(tree_ident(t)));
      break;

   case T_TYPE_DECL:
      dump_type_decl(t, indent);
      return;

   case T_SUBTYPE_DECL:
      dump_subtype_decl(t, indent);
      return;

   case T_SPEC:
      print_syntax("#for %s", istr(tree_ident(t)));
      if (tree_has_ref(t))
         print_syntax(" : %s", istr(tree_ident(tree_ref(t))));
      print_syntax("\n");
      tab(indent + 2);
      dump_binding(tree_value(t), indent + 2);
      dump_decls(t, indent + 2);
      tab(indent);
      print_syntax("#end #for;\n");
      return;

   case T_BLOCK_CONFIG:
      print_syntax("#for %s\n", istr(tree_ident(t)));
      dump_decls(t, indent + 2);
      tab(indent);
      print_syntax("#end #for;\n");
      return;

   case T_ENUM_LIT:
      print_syntax("%s", istr(tree_ident(t)));
      return;

   case T_ALIAS:
      if (tree_flags(t) & TREE_F_PREDEFINED)
         print_syntax("-- predefined ");
      print_syntax("#alias %s", istr(tree_ident(t)));
      if (tree_has_type(t)) {
         print_syntax(" : ");
         dump_type(tree_type(t));
      }
      print_syntax(" #is ");
      dump_expr(tree_value(t));
      print_syntax(";\n");
      return;

   case T_ATTR_SPEC:
      print_syntax("#attribute %s #of %s : #%s #is ", istr(tree_ident(t)),
             istr(tree_ident2(t)), class_str(tree_class(t)));
      dump_expr(tree_value(t));
      print_syntax(";\n");
      return;

   case T_ATTR_DECL:
      print_syntax("#attribute %s : ", istr(tree_ident(t)));
      dump_type(tree_type(t));
      print_syntax(";\n");
      return;

   case T_FUNC_DECL:
      if (tree_flags(t) & TREE_F_PREDEFINED)
         print_syntax("-- predefined %s\n", type_pp(tree_type(t)));
      else {
         print_syntax("#function %s", istr(tree_ident(t)));
         dump_generics(t, indent + 2, "");
         dump_arguments(t, indent, "");
         print_syntax(" #return ");
         dump_type(type_result(tree_type(t)));
         print_syntax(";\n");
         if (tree_has_ident2(t)) {
            tab(indent + 2);
            print_syntax("-- %s\n", istr(tree_ident2(t)));
         }
      }
      return;

   case T_FUNC_INST:
   case T_FUNC_BODY:
      print_syntax("#function %s", istr(tree_ident(t)));
      dump_type_hint(t);
      dump_generics(t, indent + 2, tree_ports(t) > 0 ? "\n" : "");
      if (tree_kind(t) == T_FUNC_INST)
         dump_generic_map(t, indent + 2, tree_ports(t) > 0 ? "\n" : "");
      dump_arguments(t, indent, "");
      print_syntax(" #return ");
      dump_type(type_result(tree_type(t)));
      print_syntax(" #is\n");
      if (tree_has_ident2(t)) {
         tab(indent + 2);
         print_syntax("-- %s\n", istr(tree_ident2(t)));
      }
      dump_block(t, indent);
      tab(indent);
      print_syntax("#end #function;\n");
      return;

   case T_PROC_DECL:
      if (tree_flags(t) & TREE_F_PREDEFINED)
         print_syntax("-- predefined %s\n", type_pp(tree_type(t)));
      else {
         print_syntax("#procedure %s", istr(tree_ident(t)));
         dump_generics(t, indent + 2, tree_ports(t) > 0 ? "\n" : "");
         dump_arguments(t, indent, "");
         print_syntax(";");
         dump_wait_level(t);
         print_syntax("\n");
         if (tree_has_ident2(t)) {
            tab(indent + 2);
            print_syntax("-- %s\n", istr(tree_ident2(t)));
         }
      }
      return;

   case T_PROC_INST:
   case T_PROC_BODY:
      print_syntax("#procedure %s", istr(tree_ident(t)));
      dump_type_hint(t);
      dump_generics(t, indent + 2, tree_ports(t) > 0 ? "\n" : "");
      if (tree_kind(t) == T_PROC_INST)
         dump_generic_map(t, indent + 2, tree_ports(t) > 0 ? "\n" : "");
      dump_arguments(t, indent, "");
      print_syntax(" #is");
      dump_wait_level(t);
      print_syntax("\n");
      if (tree_has_ident2(t)) {
         tab(indent + 2);
         print_syntax("-- %s\n", istr(tree_ident2(t)));
      }
      dump_block(t, indent);
      tab(indent);
      print_syntax("#end #procedure;\n");
      return;

   case T_HIER:
      {
         const char *kind = "Scope";
         switch (tree_subkind(t)) {
         case T_ARCH: kind = "Instance"; break;
         case T_IF_GENERATE: kind = "If generate"; break;
         case T_FOR_GENERATE: kind = "For generate"; break;
         case T_BLOCK: kind = "Block"; break;
         }
         print_syntax("-- %s %s\n", kind, istr(tree_ident2(t)));
      }
      return;

   case T_COMPONENT:
      dump_component(t, indent);
      return;

   case T_PROT_DECL:
      print_syntax("#type %s #is #protected\n", istr(tree_ident(t)));
      dump_decls(t, indent + 2);
      tab(indent);
      print_syntax("#end #protected;\n");
      return;

   case T_PROT_BODY:
      print_syntax("#type %s #is #protected #body\n", istr(tree_ident(t)));
      dump_decls(t, indent + 2);
      tab(indent);
      print_syntax("#end #protected #body;\n");
      return;

   case T_FILE_DECL:
      print_syntax("#file %s : ", istr(tree_ident(t)));
      dump_type(tree_type(t));
      if (tree_has_value(t)) {
         print_syntax(" #open ");
         dump_expr(tree_file_mode(t));
         print_syntax(" #is ");
         dump_expr(tree_value(t));
      }
      print_syntax(";\n");
      return;

   case T_USE:
      dump_use(t);
      return;

   case T_PACKAGE:
   case T_PACK_INST:
      dump_package(t, indent);
      return;

   case T_PACK_BODY:
      dump_package_body(t, indent);
      return;

   default:
      cannot_dump(t, "decl");
   }

   dump_type(tree_type(t));

   if (tree_kind(t) != T_FIELD_DECL && tree_has_value(t)) {
      print_syntax(" := ");
      dump_expr(tree_value(t));
   }
   print_syntax(";\n");
}

static void dump_waveforms(tree_t t)
{
   const int nwaves = tree_waveforms(t);
   for (int i = 0; i < nwaves; i++) {
      if (i > 0) print_syntax(", ");
      dump_waveform(tree_waveform(t, i));
   }
}

static void dump_alternative(tree_t t, int indent)
{
   tab(indent);
   print_syntax("#when ");
   if (tree_has_ident(t))
      print_syntax("%s: ", istr(tree_ident(t)));
   for (unsigned i = 0; i < tree_assocs(t); i++) {
      if (i > 0) print_syntax("| ");
      tree_t a = tree_assoc(t, i);
      switch (tree_subkind(a)) {
      case A_NAMED:
         dump_expr(tree_name(a));
         break;
      case A_OTHERS:
         print_syntax("#others");
         break;
      case A_RANGE:
         dump_range(tree_range(a, 0));
         break;
      }
   }
   print_syntax(" =>\n");
   if (tree_decls(t) > 0) {
      dump_decls(t, indent + 4);
      tab(indent + 2);
      print_syntax("#begin\n");
      dump_stmts(t, indent + 4);
      tab(indent + 2);
      print_syntax("#end;\n");
   }
   else
      dump_stmts(t, indent + 2);
}

static void dump_psl(tree_t t, int indent)
{
   if (standard() < STD_08)
      print_syntax("-- psl ");
   psl_dump(tree_psl(t));
}

static void dump_instance(tree_t t, int indent)
{
   switch (tree_class(t)) {
   case C_ENTITY:        print_syntax("#entity "); break;
   case C_COMPONENT:     print_syntax("#component "); break;
   case C_CONFIGURATION: print_syntax("#configuration "); break;
   default: break;
   }

   print_syntax("%s", istr(tree_ident2(t)));

   const int nparams = tree_params(t);
   const int ngenmaps = tree_genmaps(t);

   if (tree_has_spec(t)) {
      tree_t spec = tree_spec(t);
      if (tree_has_value(spec)) {
         tree_t bind = tree_value(spec);
         LOCAL_TEXT_BUF tb = tb_new();
         tb_cat(tb, istr(tree_ident(bind)));
         if (tree_has_ident2(bind))
            tb_printf(tb, "(%s)", istr(tree_ident2(bind)));
         print_syntax("  -- bound to %s\n", tb_get(tb));
      }
   }
   else if (nparams > 0 || ngenmaps > 0)
      print_syntax("\n");

   dump_generic_map(t, indent + 2, nparams > 0 ? "\n" : "");
   dump_port_map(t, indent + 2, "");
   print_syntax(";\n");
}

static void dump_stmt(tree_t t, int indent)
{
   tab(indent);

   if (tree_has_ident(t)) {
      const char *label = istr(tree_ident(t));
#if !DUMP_GEN_NAMES
      if (label[0] != '_')   // Skip generated labels
#endif
         print_syntax("%s: ", label);
   }

   switch (tree_kind(t)) {
   case T_PROCESS:
      dump_address(t);
      if (tree_flags(t) & TREE_F_POSTPONED)
         print_syntax("#postponed ");
      print_syntax("#process ");
      if (tree_triggers(t) > 0) {
         print_syntax("(");
         for (unsigned i = 0; i < tree_triggers(t); i++) {
            if (i > 0)
               print_syntax(", ");
            dump_expr(tree_trigger(t, i));
         }
         print_syntax(") ");
      }
      print_syntax("#is\n");
      dump_decls(t, indent + 2);
      tab(indent);
      print_syntax("#begin\n");
      dump_stmts(t, indent + 2);
      tab(indent);
      print_syntax("#end #process;\n");
      return;

   case T_SIGNAL_ASSIGN:
      dump_expr(tree_target(t));
      print_syntax(" <= #reject ");
      if (tree_has_reject(t))
         dump_expr(tree_reject(t));
      else
         print_syntax("0 ps");
      print_syntax(" #inertial ");
      dump_waveforms(t);
      break;

   case T_FORCE:
      dump_expr(tree_target(t));
      print_syntax(" <= #force ");
      dump_expr(tree_value(t));
      break;

   case T_RELEASE:
      dump_expr(tree_target(t));
      print_syntax(" <= #release");
      break;

   case T_VAR_ASSIGN:
      dump_expr(tree_target(t));
      print_syntax(" := ");
      dump_expr(tree_value(t));
      break;

   case T_WAIT:
      print_syntax("#wait");
      if (tree_triggers(t) > 0) {
         print_syntax(" #on ");
         for (unsigned i = 0; i < tree_triggers(t); i++) {
            if (i > 0)
               print_syntax(", ");
            dump_expr(tree_trigger(t, i));
         }
      }
      if (tree_has_value(t)) {
         print_syntax(" #until ");
         dump_expr(tree_value(t));
      }
      if (tree_has_delay(t)) {
         print_syntax(" #for ");
         dump_expr(tree_delay(t));
      }
      print_syntax(";");
      if (tree_flags(t) & TREE_F_STATIC_WAIT)
         print_syntax("   -- static");
      print_syntax("\n");
      return;

   case T_BLOCK:
      dump_address(t);
      print_syntax("#block #is\n");
      dump_generics(t, indent + 2, ";\n");
      dump_generic_map(t, indent + 2, ";\n");
      dump_ports(t, indent + 2);
      dump_port_map(t, indent + 2, ";\n");
      dump_block(t, indent);
      tab(indent);
      print_syntax("#end #block;\n");
      return;

   case T_SEQUENCE:
      print_syntax("#block #is\n");
      dump_block(t, indent);
      tab(indent);
      print_syntax("#end #block;\n");
      return;

   case T_ASSERT:
      if (tree_has_value(t)) {
         print_syntax("#assert ");
         dump_expr(tree_value(t));
         print_syntax(" ");
      }
      if (tree_has_message(t)) {
         print_syntax("#report ");
         dump_expr(tree_message(t));
         print_syntax(" ");
      }
      print_syntax("#severity ");
      dump_expr(tree_severity(t));
      break;

   case T_WHILE:
      if (tree_has_value(t)) {
         print_syntax("#while ");
         dump_expr(tree_value(t));
         print_syntax(" ");
      }
      print_syntax("#loop\n");
      dump_stmts(t, indent + 2);
      tab(indent);
      print_syntax("#end #loop");
      break;

   case T_IF:
      for (unsigned i = 0; i < tree_conds(t); i++) {
         tree_t c = tree_cond(t, i);
         if (tree_has_value(c)) {
            if (i > 0)
               tab(indent);
            print_syntax(i > 0 ? "#elsif " : "#if ");
            dump_expr(tree_value(c));
            print_syntax(" #then\n");
         }
         else {
            tab(indent);
            print_syntax("#else\n");
         }
         dump_stmts(c, indent + 2);
      }
      tab(indent);
      print_syntax("#end #if");
      break;

   case T_EXIT:
      print_syntax("#exit %s", istr(tree_ident2(t)));
      if (tree_has_value(t)) {
         print_syntax(" #when ");
         dump_expr(tree_value(t));
      }
      break;

   case T_CASE:
   case T_MATCH_CASE:
      {
         const char *suffix = tree_kind(t) == T_MATCH_CASE ? "?" : "";
         print_syntax("#case%s ", suffix);
         dump_expr(tree_value(t));
         print_syntax(" #is\n");
         const int nstmts = tree_stmts(t);
         for (int i = 0; i < nstmts; i++)
            dump_alternative(tree_stmt(t, i), indent + 2);
         tab(indent);
         print_syntax("#end #case%s", suffix);
      }
      break;

   case T_RETURN:
      print_syntax("#return");
      if (tree_has_value(t)) {
         print_syntax(" ");
         dump_expr(tree_value(t));
      }
      break;

   case T_COND_RETURN:
      print_syntax("#return #when ");
      dump_expr(tree_value(t));
      break;

   case T_FOR:
      print_syntax("#for %s #in ", istr(tree_ident(tree_decl(t, 0))));
      dump_range(tree_range(t, 0));
      print_syntax(" #loop\n");
      dump_stmts(t, indent + 2);
      tab(indent);
      print_syntax("#end #for");
      break;

   case T_PROT_PCALL:
      dump_expr(tree_name(t));
      print_syntax(".");
      // Fall-through
   case T_PCALL:
      if (tree_has_ref(t)) {
         tree_t decl = tree_ref(t);
         dump_address(decl);
         print_syntax("%s", istr(tree_ident(decl)));
      }
      else
         print_syntax("%s", istr(tree_ident(t)));
      dump_params(t, tree_param, tree_params(t), NULL);
      break;

   case T_FOR_GENERATE:
      print_syntax("#for %s #in ", istr(tree_ident(tree_decl(t, 0))));
      dump_range(tree_range(t, 0));
      print_syntax(" #generate\n");
      dump_decls(t, indent + 2);
      tab(indent);
      print_syntax("#begin\n");
      dump_stmts(t, indent + 2);
      tab(indent);
      print_syntax("#end #generate");
      break;

   case T_CASE_GENERATE:
      {
         print_syntax("#case ");
         dump_expr(tree_value(t));
         print_syntax(" #generate\n");

         const int nstmts = tree_stmts(t);
         for (int i = 0; i < nstmts; i++)
            dump_alternative(tree_stmt(t, i), indent + 2);

         print_syntax("#end #generate");
         tab(indent);
      }
      break;

   case T_IF_GENERATE:
      for (unsigned i = 0; i < tree_conds(t); i++) {
         tree_t c = tree_cond(t, i);
         if (tree_has_value(c)) {
            if (i > 0)
               tab(indent);
            print_syntax(i > 0 ? "#elsif " : "#if ");
            dump_expr(tree_value(c));
            print_syntax(" #generate\n");
         }
         else {
            tab(indent);
            print_syntax("#else\n");
         }
         for (unsigned i = 0; i < tree_stmts(c); i++)
            dump_stmt(tree_stmt(c, i), indent + 2);
      }
      tab(indent);
      print_syntax("#end #generate");
      break;

   case T_INSTANCE:
      dump_instance(t, indent);
      return;

   case T_NEXT:
      print_syntax("#next");
      if (tree_has_value(t)) {
         print_syntax(" #when ");
         dump_expr(tree_value(t));
      }
      break;

   case T_NULL:
      print_syntax("#null");
      break;

   case T_COND_ASSIGN:
      dump_expr(tree_target(t));
      print_syntax(" <= ");
      for (int i = 0; i < tree_conds(t); i++) {
         tree_t c = tree_cond(t, i);
         dump_waveforms(tree_stmt(c, 0));
         if (tree_has_value(c)) {
            print_syntax(" #when ");
            dump_expr(tree_value(c));
         }
      }
      break;

   case T_SELECT:
      print_syntax(" <= ");
      if (tree_has_guard(t)) print_syntax("#guarded ");
      color_printf("$red$/* TODO: T_SELECT */$$");
      break;

   case T_CONCURRENT:
      if (tree_flags(t) & TREE_F_POSTPONED)
         print_syntax("#postponed ");
      if (tree_has_guard(t))
         print_syntax("#guarded ");
      dump_stmt(tree_stmt(t, 0), 0);
      return;

   case T_PSL:
      dump_psl(t, 0);
      break;

   case T_VERILOG:
      print_syntax("#block #is\n");
      dump_generic_map(t, indent + 2, ";\n");
      dump_port_map(t, indent + 2, ";\n");
      tab(indent);
      print_syntax("#begin\n");
      vlog_dump(tree_vlog(t), indent + 2);
      tab(indent);
      print_syntax("#end #block");
      break;

   default:
      cannot_dump(t, "stmt");
   }

   print_syntax(";\n");
}

static void dump_port(tree_t t, int indent)
{
   tab(indent);
   dump_address(t);

   if (tree_flags(t) & TREE_F_PREDEFINED)
      print_syntax("-- predefined ");

   const class_t class = tree_class(t);
   print_syntax("#%s %s", class_str(class), istr(tree_ident(t)));

   type_t type = get_type_or_null(t);
   if (class == C_PACKAGE) {
      print_syntax(" #is #new ");
      dump_expr(tree_value(t));
   }
   else if (class == C_TYPE && type_kind(type) == T_GENERIC) {
      print_syntax(" #is ");

      switch (type_subkind(type)) {
      case GTYPE_PRIVATE:
         print_syntax("#private");
         break;
      case GTYPE_SCALAR:
         print_syntax("<>");
         break;
      case GTYPE_DISCRETE:
         print_syntax("(<>)");
         break;
      case GTYPE_INTEGER:
         print_syntax("#range <>");
         break;
      case GTYPE_PHYSICAL:
         print_syntax("#units <>");
         break;
      case GTYPE_FLOATING:
         print_syntax("#range <> . <>");
         break;
      case GTYPE_ARRAY:
         {
            print_syntax("#array (");
            const int nindex = type_indexes(type);
            for (int i = 0; i < nindex; i++) {
               if (i > 0) print_syntax(", ");
               dump_type(type_index(type, i));
               print_syntax(" #range <>");
            }
            print_syntax(") #of ");
            dump_type(type_elem(type));
         }
         break;
      case GTYPE_ACCESS:
         print_syntax("#access ..");
         break;
      case GTYPE_FILE:
         print_syntax("#file #of ..");
         break;
      }
   }
   else {
      const char *dir = NULL;
      switch (tree_subkind(t)) {
      case PORT_IN:      dir = "in";     break;
      case PORT_OUT:     dir = "out";    break;
      case PORT_INOUT:   dir = "inout";  break;
      case PORT_BUFFER:  dir = "buffer"; break;
      case PORT_INVALID: dir = "??";     break;
      }
      print_syntax(" : #%s ", dir);
      dump_type(type);

      if (tree_has_value(t)) {
         print_syntax(" := ");
         dump_expr(tree_value(t));
      }
   }
}

static void dump_context(tree_t t, int indent)
{
   const int nctx = tree_contexts(t);
   for (int i = 0; i < nctx; i++) {
      tree_t c = tree_context(t, i);
      switch (tree_kind(c)) {
      case T_LIBRARY:
         switch (is_well_known(tree_ident(c))) {
         case W_STD:
         case W_WORK:
            break;
         default:
            print_syntax("#library %s;\n", istr(tree_ident(c)));
         }
         break;

      case T_USE:
         dump_use(c);
         break;

      case T_CONTEXT_REF:
         print_syntax("#context %s;\n", istr(tree_ident(t)));
         break;

      default:
         break;
      }

      tab(indent);
   }

   if (nctx > 0) {
      print_syntax("\n");
      tab(indent);
   }
}

static void dump_elab(tree_t t)
{
   dump_context(t, 0);
   dump_address(t);
   print_syntax("#entity %s #is\n#end #entity;\n\n", istr(tree_ident(t)));
   print_syntax("#architecture #elab #of %s #is\n", istr(tree_ident(t)));
   dump_decls(t, 2);
   print_syntax("#begin\n");
   for (unsigned i = 0; i < tree_stmts(t); i++)
      dump_stmt(tree_stmt(t, i), 2);
   print_syntax("#end #architecture;\n\n");
}

static void dump_entity(tree_t t)
{
   dump_context(t, 0);
   dump_address(t);
   print_syntax("#entity %s #is\n", istr(tree_ident(t)));
   dump_generics(t, 2, ";\n");
   dump_ports(t, 2);
   dump_decls(t, 2);
   if (tree_stmts(t) > 0) {
      print_syntax("#begin\n");
      for (unsigned i = 0; i < tree_stmts(t); i++) {
         dump_stmt(tree_stmt(t, i), 2);
      }
   }
   print_syntax("#end #entity;\n\n");
}

static void dump_decls(tree_t t, int indent)
{
   const int ndecls = tree_decls(t);
   bool was_body = false;
   for (unsigned i = 0; i < ndecls; i++) {
      tree_t d = tree_decl(t, i);
      tree_kind_t dkind = tree_kind(d);
      const bool is_body = dkind == T_FUNC_BODY || dkind == T_PROT_BODY
         || dkind == T_PROC_BODY;
      if ((was_body && !is_body) || (is_body && i > 0))
         print_syntax("\n");
      was_body = is_body;
      dump_decl(d, indent);
   }
}

static void dump_arch(tree_t t)
{
   dump_context(t, 0);
   dump_address(t);
   print_syntax("#architecture %s #of %s #is\n",
                istr(tree_ident(t)), istr(tree_ident2(t)));
   dump_decls(t, 2);
   print_syntax("#begin\n");
   dump_stmts(t, 2);
   print_syntax("#end #architecture;\n\n");
}

static void dump_package(tree_t t, int indent)
{
   dump_context(t, indent);
   dump_address(t);
   print_syntax("#package %s #is\n", istr(tree_ident(t)));
   if (tree_kind(t) == T_PACK_INST && tree_has_ref(t)) {
      tab(indent);
      print_syntax("  -- Instantiated from %s\n", istr(tree_ident(tree_ref(t))));
   }
   dump_generics(t, indent + 2, ";\n");
   dump_generic_map(t, indent + 2, ";\n");
   dump_decls(t, indent + 2);
   tab(indent);
   print_syntax("#end #package;\n\n");
}

static void dump_package_body(tree_t t, int indent)
{
   dump_context(t, indent);
   dump_address(t);
   print_syntax("#package #body %s #is\n", istr(tree_ident(t)));
   dump_decls(t, indent + 2);
   tab(indent);
   print_syntax("#end #package #body;\n\n");
}

static void dump_configuration(tree_t t)
{
   dump_address(t);
   print_syntax("#configuration %s #of %s #is\n",
          istr(tree_ident(t)), istr(tree_ident2(t)));
   dump_decls(t, 2);
   print_syntax("#end #configuration\n");
}

void vhdl_dump(tree_t t, int indent)
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
   case T_PACKAGE:
   case T_PACK_INST:
      dump_package(t, indent);
      break;
   case T_PACK_BODY:
      dump_package_body(t, indent);
      break;
   case T_CONFIGURATION:
      dump_configuration(t);
      break;
   case T_REF:
   case T_FCALL:
   case T_PROT_FCALL:
   case T_LITERAL:
   case T_AGGREGATE:
   case T_ARRAY_REF:
   case T_ARRAY_SLICE:
   case T_TYPE_CONV:
   case T_RECORD_REF:
   case T_ATTR_REF:
   case T_CONV_FUNC:
   case T_QUALIFIED:
   case T_EXTERNAL_NAME:
   case T_STRING:
   case T_PACKAGE_MAP:
      dump_expr(t);
      break;
   case T_INSTANCE:
   case T_FOR_GENERATE:
   case T_CASE_GENERATE:
   case T_BLOCK:
   case T_PROCESS:
   case T_CASE:
   case T_FOR:
   case T_SIGNAL_ASSIGN:
   case T_IF:
   case T_WAIT:
   case T_PSL:
   case T_VAR_ASSIGN:
   case T_RETURN:
      dump_stmt(t, indent);
      break;
   case T_CONST_DECL:
   case T_VAR_DECL:
   case T_SIGNAL_DECL:
   case T_TYPE_DECL:
   case T_FIELD_DECL:
   case T_FUNC_DECL:
   case T_PROC_BODY:
   case T_FUNC_BODY:
   case T_PROC_DECL:
   case T_ATTR_DECL:
   case T_ATTR_SPEC:
   case T_ENUM_LIT:
   case T_COMPONENT:
   case T_BLOCK_CONFIG:
   case T_SPEC:
   case T_ALIAS:
   case T_FUNC_INST:
   case T_PROC_INST:
   case T_SUBTYPE_DECL:
      dump_decl(t, indent);
      break;
   case T_PORT_DECL:
   case T_GENERIC_DECL:
      dump_port(t, indent);
      break;
   case T_RANGE:
      dump_range(t);
      break;
   case T_BINDING:
      dump_binding(t, indent);
      break;
   case T_PARAM:
      dump_param(t);
      break;
   case T_CONSTRAINT:
      dump_constraint(t);
      break;
   case T_ELEM_CONSTRAINT:
      dump_record_elem_constraint(t);
      break;
   case T_ALTERNATIVE:
      dump_alternative(t, indent);
      break;
   default:
      cannot_dump(t, "tree");
   }
}

void dump(tree_t t)
{
   vhdl_dump(t, 0);
   print_syntax("\r");
}
