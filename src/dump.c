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

#include "phase.h"
#include "tree.h"
#include "util.h"
#include "hash.h"
#include "common.h"
#include "ident.h"
#include "type.h"

#include <assert.h>
#include <string.h>
#include <ctype.h>
#include <inttypes.h>

#define DUMP_TYPE_HINT  0
#define DUMP_ADDRESS    0
#define DUMP_STATICNESS 0
#define DUMP_GEN_NAMES  0

LCOV_EXCL_START

static void dump_expr(tree_t t);
static void dump_stmt(tree_t t, int indent);
static void dump_port(tree_t t, int indent);
static void dump_decl(tree_t t, int indent);
static void dump_decls(tree_t t, int indent);
static void dump_type(type_t type);
static void dump_package(tree_t t, int indent);
static void dump_package_body(tree_t t, int indent);

typedef tree_t (*get_fn_t)(tree_t, unsigned);

static void tab(int indent)
{
   while (indent--)
      fputc(' ', stdout);
}

static void cannot_dump(tree_t t, const char *hint)
{
   printf("\n");
   fflush(stdout);
   fatal("cannot dump %s kind %s", hint, tree_kind_str(tree_kind(t)));
}

__attribute__((format(printf,1,2)))
static void syntax(const char *fmt, ...)
{
   LOCAL_TEXT_BUF tb = tb_new();
   bool highlighting = false;
   static bool comment = false;
   for (const char *p = fmt; *p != '\0'; p++) {
      if (comment) {
         if (*p == '\n') {
            comment = false;
            tb_printf(tb, "$$");
         }
         if (*p != '~' && *p != '#')
            tb_append(tb, *p);
         if (p > fmt && *p == '/' && *(p - 1) == '*') {
            tb_printf(tb, "$$");
            comment = false;
         }
      }
      else if (*p == '#') {
         tb_printf(tb, "$bold$$cyan$");
         highlighting = true;
      }
      else if (*p == '~') {
         tb_printf(tb, "$yellow$");
         highlighting = true;
      }
      else if ((*p == '-' && *(p + 1) == '-')
               || (*p == '/' && *(p + 1) == '*')) {
         tb_printf(tb, "$red$%c", *p);
         comment = true;
      }
      else if (!isalnum((int)*p) && *p != '_' && *p != '%' && highlighting) {
         tb_printf(tb, "$$%c", *p);
         highlighting = false;
      }
      else
         tb_append(tb, *p);
   }

   if (highlighting || comment)
      tb_printf(tb, "$$");

   va_list ap;
   va_start(ap, fmt);
   color_vprintf(tb_get(tb), ap);
   va_end(ap);
}

static void dump_param(tree_t p)
{
   switch (tree_subkind(p)) {
   case P_POS:
      break;
   case P_NAMED:
      dump_expr(tree_name(p));
      printf(" => ");
      break;
   }
   dump_expr(tree_value(p));
}

static void dump_params(tree_t t, get_fn_t get, int n, const char *prefix)
{
   if (n > 0) {
      if (prefix != NULL) {
         syntax(prefix, "");
         printf(" ");
      }
      printf("(");
      for (int i = 0; i < n; i++) {
         if (i > 0)
            printf(", ");
         dump_param((*get)(t, i));
      }
      printf(")");
   }
}

static void dump_range(tree_t r)
{
   switch (tree_subkind(r)) {
   case RANGE_TO:
      dump_expr(tree_left(r));
      syntax(" #to ");
      dump_expr(tree_right(r));
      break;
   case RANGE_DOWNTO:
      dump_expr(tree_left(r));
      syntax(" #downto ");
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

static void dump_expr(tree_t t)
{
   switch (tree_kind(t)) {
   case T_PROT_FCALL:
      dump_expr(tree_name(t));
      printf(".");
      // Fall-through
   case T_FCALL:
      if (tree_has_ref(t)) {
         tree_t decl = tree_ref(t);
         dump_address(decl);
         printf("%s", istr(tree_ident(decl)));
      }
      else
         printf("%s", istr(tree_ident(t)));
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
         printf("%s", istr(tree_ident(decl)));
      }
      else
         printf("%s", istr(tree_ident(t)));
      printf("(");
      dump_expr(tree_value(t));
      printf(")");
      break;

   case T_LITERAL:
      switch (tree_subkind(t)) {
      case L_INT:
         printf("%"PRIi64, tree_ival(t));
         break;
      case L_PHYSICAL:
         if (tree_has_ident(t))
            printf("%"PRIi64" %s", tree_ival(t), istr(tree_ident(t)));
         else
            printf("%"PRIi64, tree_ival(t));
         break;
      case L_REAL:
         printf("%lf", tree_dval(t));
         break;
      case L_NULL:
         syntax("#null");
         break;
      default:
         assert(false);
      }
      break;

   case T_STRING:
      {
         printf("\"");
         const int nchars = tree_chars(t);
         for (int i = 0; i < nchars; i++)
            printf("%c", ident_char(tree_ident(tree_char(t, i)), 1));
         printf("\"");
      }
      break;

   case T_NEW:
      syntax("#new ");
      dump_expr(tree_value(t));
      break;

   case T_ALL:
      if (tree_has_value(t)) {
         dump_expr(tree_value(t));
         syntax(".#all");
      }
      else
         syntax("#all");
      break;

   case T_TYPE_REF:
      dump_type(tree_type(t));
      break;

   case T_AGGREGATE:
      printf("(");
      for (unsigned i = 0; i < tree_assocs(t); i++) {
         if (i > 0)
            printf(", ");
         tree_t a = tree_assoc(t, i);
         tree_t value = tree_value(a);
         switch (tree_subkind(a)) {
         case A_POS:
            dump_expr(value);
            break;
         case A_NAMED:
            dump_expr(tree_name(a));
            printf(" => ");
            dump_expr(value);
            break;
         case A_OTHERS:
            syntax("#others => ");
            dump_expr(value);
            break;
         case A_RANGE:
            dump_range(tree_range(a, 0));
            printf(" => ");
            dump_expr(value);
            break;
         default:
            assert(false);
         }
      }
      printf(")");
      break;

   case T_REF:
      if (tree_has_ref(t)) {
         tree_t decl = tree_ref(t);
         dump_address(decl);
         printf("%s", istr(tree_ident(decl)));
      }
      else
         printf("%s", istr(tree_ident(t)));
      break;

   case T_ATTR_REF:
      dump_expr(tree_name(t));
      printf("'%s", istr(tree_ident(t)));
      if (tree_params(t) > 0)
         dump_params(t, tree_param, tree_params(t), NULL);
      break;

   case T_EXTERNAL_NAME:
      {
         syntax("<< #%s ", class_str(tree_class(t)));
         const int nparts = tree_parts(t);
         for (int i = 0; i < nparts; i++) {
            tree_t part = tree_part(t, i);
            if (i > 0 || tree_subkind(part) == PE_SIMPLE)
               printf(".");
            switch (tree_subkind(part)) {
            case PE_SIMPLE:
               printf("%s", istr(tree_ident(part)));
               break;
            case PE_ABSOLUTE:
               printf(".");
               break;
            case PE_CARET:
               printf("^");
               break;
            }
         }
         printf(" : ");
         dump_type(tree_type(t));
         printf(" >>");
      }
      break;

   case T_ARRAY_REF:
      dump_expr(tree_value(t));
      dump_params(t, tree_param, tree_params(t), NULL);
      break;

   case T_ARRAY_SLICE:
      dump_expr(tree_value(t));
      printf("(");
      dump_range(tree_range(t, 0));
      printf(")");
      break;

   case T_RECORD_REF:
      dump_expr(tree_value(t));
      printf(".%s", istr(tree_ident(t)));
      break;

   case T_TYPE_CONV:
      printf("%s(", istr(type_ident(tree_type(t))));
      dump_expr(tree_value(t));
      printf(")");
      break;

   case T_QUALIFIED:
      if (tree_has_value(t)) {
         printf("%s'(", istr(type_ident(tree_type(t))));
         dump_expr(tree_value(t));
         printf(")");
      }
      else
         dump_type(tree_type(t));
      break;

   case T_OPEN:
      syntax("#open");
      break;

   case T_BOX:
      printf("<>");
      break;

   default:
      cannot_dump(t, "expr");
   }

   dump_type_hint(t);
}

static void dump_constraint(tree_t t)
{
   const int nranges = tree_ranges(t);

   switch (tree_subkind(t)) {
   case C_RANGE:
      syntax(" #range ");
      dump_range(tree_range(t, 0));
      break;
   case C_INDEX:
      printf("(");
      for (int i = 0; i < nranges; i++) {
         if (i > 0) printf(", ");
         dump_range(tree_range(t, i));
      }
      printf(")");
      break;
   case C_OPEN:
      syntax("(#open)");
      break;
   case C_RECORD:
      printf("(");
      for (int i = 0; i < nranges; i++) {
         if (i > 0) printf(", ");
         tree_t fc = tree_range(t, i);
         printf("%s", istr(tree_ident(tree_ref(fc))));

         type_t ftype = tree_type(fc);
         const int ncon = type_constraints(ftype);
         for (int i = 0; i < ncon; i++)
            dump_constraint(type_constraint(ftype, i));
      }
      printf(")");
      break;
   }
}

static void dump_type(type_t type)
{
   if (type_kind(type) == T_SUBTYPE && !type_has_ident(type)) {
      // Anonymous subtype
      printf("%s", type_pp(type));
      if (type_ident(type) == type_ident(type_base(type))) {
         const int ncon = type_constraints(type);
         for (int i = 0; i < ncon; i++)
            dump_constraint(type_constraint(type, i));
      }
   }
   else
      printf("%s", type_pp(type));
}

static void dump_arguments(tree_t t, int indent, const char *trailer)
{
   const int nports = tree_ports(t);
   if (nports > 0) {
      printf(" (");
      if (nports > 1) {
         printf("\n");
         for (int i = 0; i < nports; i++) {
            if (i > 0) printf(";\n");
            dump_port(tree_port(t, i), indent + 4);
         }
      }
      else
         dump_port(tree_port(t, 0), 1);
      printf(" )%s", trailer);
   }
}

static void dump_ports(tree_t t, int indent)
{
   const int nports = tree_ports(t);
   if (nports > 0) {
      tab(indent);
      syntax("#port (");
      if (nports > 1) {
         printf("\n");
         for (unsigned i = 0; i < nports; i++) {
            if (i > 0) printf(";\n");
            dump_port(tree_port(t, i), indent + 2);
         }
      }
      else
         dump_port(tree_port(t, 0), 1);
      printf(" );\n");
   }
}

static void dump_generics(tree_t t, int indent, const char *trailer)
{
   const int ngenerics = tree_generics(t);
   if (ngenerics > 0) {
      tab(indent);
      syntax("#generic (");
      if (ngenerics > 1) {
         printf("\n");
         for (int i = 0; i < ngenerics; i++) {
            if (i > 0) printf(";\n");
            dump_port(tree_generic(t, i), indent + 2);
         }
      }
      else
         dump_port(tree_generic(t, 0), 1);
      printf(" )%s", trailer);
   }
}

static void dump_port_map(tree_t t, int indent, const char *trailer)
{
   const int nparams = tree_params(t);
   if (nparams > 0) {
      tab(indent);
      dump_params(t, tree_param, nparams, "#port #map");
      fputs(trailer, stdout);
   }
}

static void dump_generic_map(tree_t t, int indent, const char *trailer)
{
   const int ngenmaps = tree_genmaps(t);
   if (ngenmaps > 0) {
      tab(indent);
      dump_params(t, tree_genmap, ngenmaps, "#generic #map");
      fputs(trailer, stdout);
   }
}

static void dump_binding(tree_t t, int indent)
{
   syntax("#use %s", istr(tree_ident(t)));
   if (tree_has_ident2(t))
      printf("(%s)", istr(tree_ident2(t)));
   printf("\n");
   dump_generic_map(t, indent + 2, tree_params(t) > 0 ? "\n" : "");
   dump_port_map(t, indent + 2, "");
   printf(";\n");
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
         printf("\n");
      dump_stmt(s, indent);
   }
}

static void dump_block(tree_t t, int indent)
{
   dump_decls(t, indent + 2);
   tab(indent);
   syntax("#begin\n");
   dump_stmts(t, indent + 2);
}

static void dump_wait_level(tree_t t)
{
   const tree_flags_t flags = tree_flags(t);
   if (flags & TREE_F_NEVER_WAITS)
      syntax("   -- Never waits");
   else if (flags & TREE_F_HAS_WAIT)
      syntax("   -- Contains wait statement");
}

static void dump_type_decl(tree_t t, int indent)
{
   type_t type = tree_type(t);
   const type_kind_t kind = type_kind(type);

   syntax("#type %s", istr(tree_ident(t)));

   if (kind == T_INCOMPLETE) {
      printf(";\n");
      return;
   }

   syntax(" #is ");

   if (type_is_integer(type) || type_is_real(type)) {
      syntax("#range ");
      dump_range(type_dim(type, 0));
   }
   else if (type_is_physical(type)) {
      syntax("#range ");
      dump_range(type_dim(type, 0));
      printf("\n");
      tab(indent + 2);
      syntax("#units\n");
      {
         const int nunits = type_units(type);
         for (int i = 0; i < nunits; i++) {
            tree_t u = type_unit(type, i);
            tab(indent + 4);
            printf("%s = ", istr(tree_ident(u)));
            dump_expr(tree_value(u));
            printf(";\n");
         }
      }
      tab(indent + 2);
      syntax("#end #units");
   }
   else if (type_is_array(type)) {
      syntax("#array ");
      if (kind == T_ARRAY) {
         printf("(");
         const int nindex = type_index_constrs(type);
         for (int i = 0; i < nindex; i++) {
            if (i > 0) printf(", ");
            dump_type(type_index_constr(type, i));
            syntax(" #range <>");
         }
         printf(")");
      }
      else if (kind == T_SUBTYPE) {
         const int ncon = type_constraints(type);
         for (int i = 0; i < ncon; i++)
            dump_constraint(type_constraint(type, i));
      }
      else {
         printf("(");
         const int ndims = type_dims(type);
         for (int i = 0; i < ndims; i++) {
            if (i > 0) printf(", ");
            dump_range(type_dim(type, i));
         }
         printf(")");
      }
      syntax(" #of ");
      dump_type(type_elem(type));
   }
   else if (type_kind(type) == T_PROTECTED) {
      syntax("#protected\n");
      for (unsigned i = 0; i < type_decls(type); i++)
         dump_decl(type_decl(type, i), indent + 2);
      tab(indent);
      syntax("#end #protected");
   }
   else if (type_is_record(type)) {
      syntax("#record\n");
      const int nfields = type_fields(type);
      for (int i = 0; i < nfields; i++)
         dump_decl(type_field(type, i), indent + 2);
      tab(indent);
      syntax("#end #record");
   }
   else if (kind == T_ENUM) {
      printf("(");
      for (unsigned i = 0; i < type_enum_literals(type); i++) {
         if (i > 0) printf(", ");
         printf("%s", istr(tree_ident(type_enum_literal(type, i))));
      }
      printf(")");
   }
   else if (kind == T_INCOMPLETE)
      ;
   else
      dump_type(type);

   printf(";\n");
}

static void dump_subtype_decl(tree_t t, int indent)
{
   type_t type = tree_type(t);

   syntax("#subtype %s #is ", istr(tree_ident(t)));
   if (type_has_resolution(type)) {
      dump_expr(type_resolution(type));
      printf(" ");
   }
   printf("%s", type_pp(type_base(type)));

   const int ncon = type_constraints(type);
   for (int i = 0; i < ncon; i++)
      dump_constraint(type_constraint(type, i));

   printf(";\n");
}

static void dump_decl(tree_t t, int indent)
{
   tab(indent);
   if (tree_kind(t) != T_HIER) dump_address(t);

   switch (tree_kind(t)) {
   case T_IMPLICIT_SIGNAL:
      syntax("/* implicit */ ");
      // Fall-through
   case T_SIGNAL_DECL:
      syntax("#signal %s : ", istr(tree_ident(t)));
      break;

   case T_VAR_DECL:
      syntax("#variable %s : ", istr(tree_ident(t)));
      break;

   case T_CONST_DECL:
      syntax("#constant %s : ", istr(tree_ident(t)));
      break;

   case T_FIELD_DECL:
      syntax("%s : ", istr(tree_ident(t)));
      break;

   case T_TYPE_DECL:
      dump_type_decl(t, indent);
      return;

   case T_SUBTYPE_DECL:
      dump_subtype_decl(t, indent);
      return;

   case T_SPEC:
      syntax("#for %s", istr(tree_ident(t)));
      if (tree_has_ref(t))
         printf(" : %s", istr(tree_ident(tree_ref(t))));
      printf("\n");
      tab(indent + 2);
      dump_binding(tree_value(t), indent + 2);
      dump_decls(t, indent + 2);
      tab(indent);
      syntax("#end #for;\n");
      return;

   case T_BLOCK_CONFIG:
      syntax("#for %s\n", istr(tree_ident(t)));
      dump_decls(t, indent + 2);
      tab(indent);
      syntax("#end #for;\n");
      return;

   case T_ENUM_LIT:
      printf("%s", istr(tree_ident(t)));
      return;

   case T_ALIAS:
      syntax("#alias %s ", istr(tree_ident(t)));
      if (tree_has_type(t)) {
         printf(": ");
         dump_type(tree_type(t));
      }
      syntax(" #is ");
      dump_expr(tree_value(t));
      printf(";\n");
      return;

   case T_ATTR_SPEC:
      syntax("#attribute %s #of %s : #%s #is ", istr(tree_ident(t)),
             istr(tree_ident2(t)), class_str(tree_class(t)));
      dump_expr(tree_value(t));
      printf(";\n");
      return;

   case T_ATTR_DECL:
      syntax("#attribute %s : ", istr(tree_ident(t)));
      dump_type(tree_type(t));
      printf(";\n");
      return;

   case T_FUNC_DECL:
      if (tree_flags(t) & TREE_F_PREDEFINED)
         syntax("-- predefined %s\n", type_pp(tree_type(t)));
      else {
         syntax("#function %s", istr(tree_ident(t)));
         dump_generics(t, indent + 2, "");
         dump_arguments(t, indent, "");
         syntax(" #return %s;\n", type_pp(type_result(tree_type(t))));
         if (tree_has_ident2(t)) {
            tab(indent + 2);
            syntax("-- %s\n", istr(tree_ident2(t)));
         }
      }
      return;

   case T_FUNC_INST:
   case T_FUNC_BODY:
      syntax("#function %s", istr(tree_ident(t)));
      dump_type_hint(t);
      dump_generics(t, indent + 2, tree_ports(t) > 0 ? "\n" : "");
      if (tree_kind(t) == T_FUNC_INST)
         dump_generic_map(t, indent + 2, tree_ports(t) > 0 ? "\n" : "");
      dump_arguments(t, indent, "");
      syntax(" #return %s #is\n", type_pp(type_result(tree_type(t))));
      if (tree_has_ident2(t)) {
         tab(indent + 2);
         syntax("-- %s\n", istr(tree_ident2(t)));
      }
      dump_block(t, indent);
      tab(indent);
      syntax("#end #function;\n");
      return;

   case T_PROC_DECL:
      if (tree_flags(t) & TREE_F_PREDEFINED)
         syntax("-- predefined %s\n", type_pp(tree_type(t)));
      else {
         syntax("#procedure %s", istr(tree_ident(t)));
         dump_generics(t, indent + 2, tree_ports(t) > 0 ? "\n" : "");
         dump_arguments(t, indent, "");
         printf(";");
         dump_wait_level(t);
         syntax("\n");
         if (tree_has_ident2(t)) {
            tab(indent + 2);
            syntax("-- %s\n", istr(tree_ident2(t)));
         }
      }
      return;

   case T_PROC_INST:
   case T_PROC_BODY:
      syntax("#procedure %s", istr(tree_ident(t)));
      dump_type_hint(t);
      dump_generics(t, indent + 2, tree_ports(t) > 0 ? "\n" : "");
      if (tree_kind(t) == T_PROC_INST)
         dump_generic_map(t, indent + 2, tree_ports(t) > 0 ? "\n" : "");
      dump_arguments(t, indent, "");
      syntax(" #is");
      dump_wait_level(t);
      syntax("\n");
      if (tree_has_ident2(t)) {
         tab(indent + 2);
         syntax("-- %s\n", istr(tree_ident2(t)));
      }
      dump_block(t, indent);
      tab(indent);
      syntax("#end #procedure;\n");
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
         syntax("-- %s %s\n", kind, istr(tree_ident2(t)));
      }
      return;

   case T_COMPONENT:
      syntax("#component %s #is\n", istr(tree_ident(t)));
      dump_generics(t, indent + 2, ";\n");
      if (tree_ports(t) > 0) {
         syntax("    #port (\n");
         for (unsigned i = 0; i < tree_ports(t); i++) {
            if (i > 0)
               printf(";\n");
            tab(4);
            dump_port(tree_port(t, i), 2);
         }
         printf(" );\n");
      }
      syntax("  #end #component;\n");
      return;

   case T_PROT_BODY:
      syntax("#type %s #is #protected #body\n", istr(tree_ident(t)));
      dump_decls(t, indent + 2);
      tab(indent);
      syntax("#end #protected #body;\n");
      return;

   case T_FILE_DECL:
      syntax("#file %s : ", istr(tree_ident(t)));
      dump_type(tree_type(t));
      if (tree_has_value(t)) {
         syntax(" #open ");
         dump_expr(tree_file_mode(t));
         syntax(" #is ");
         dump_expr(tree_value(t));
      }
      printf(";\n");
      return;

   case T_USE:
      syntax("#use %s", istr(tree_ident(t)));
      if (tree_has_ident2(t))
         printf(".%s", istr(tree_ident2(t)));
      printf(";\n");
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
      printf(" := ");
      dump_expr(tree_value(t));
   }
   printf(";\n");
}

static void dump_waveforms(tree_t t)
{
   const int nwaves = tree_waveforms(t);
   for (int i = 0; i < nwaves; i++) {
      if (i > 0) printf(", ");
      tree_t w = tree_waveform(t, i);
      if (tree_has_value(w))
         dump_expr(tree_value(w));
      else
         syntax("#null");
      if (tree_has_delay(w)) {
         syntax(" #after ");
         dump_expr(tree_delay(w));
      }
   }
}

static void dump_stmt(tree_t t, int indent)
{
   tab(indent);

   if (tree_has_ident(t)) {
      const char *label = istr(tree_ident(t));
#ifndef DUMP_GEN_NAMES
      if (label[0] != '_')   // Skip generated labels
#endif
         printf("%s: ", label);

   }

   switch (tree_kind(t)) {
   case T_PROCESS:
      dump_address(t);
      if (tree_flags(t) & TREE_F_POSTPONED)
         syntax("#postponed ");
      syntax("#process ");
      if (tree_triggers(t) > 0) {
         printf("(");
         for (unsigned i = 0; i < tree_triggers(t); i++) {
            if (i > 0)
               printf(", ");
            dump_expr(tree_trigger(t, i));
         }
         printf(") ");
      }
      syntax("#is\n");
      dump_decls(t, indent + 2);
      tab(indent);
      syntax("#begin\n");
      dump_stmts(t, indent + 2);
      tab(indent);
      syntax("#end #process;\n");
      return;

   case T_SIGNAL_ASSIGN:
      dump_expr(tree_target(t));
      syntax(" <= #reject ");
      if (tree_has_reject(t))
         dump_expr(tree_reject(t));
      else
         printf("0 ps");
      syntax(" #inertial ");
      dump_waveforms(t);
      break;

   case T_FORCE:
      dump_expr(tree_target(t));
      syntax(" <= #force ");
      dump_expr(tree_value(t));
      break;

   case T_RELEASE:
      dump_expr(tree_target(t));
      syntax(" <= #release");
      break;

   case T_VAR_ASSIGN:
      dump_expr(tree_target(t));
      printf(" := ");
      dump_expr(tree_value(t));
      break;

   case T_WAIT:
      syntax("#wait");
      if (tree_triggers(t) > 0) {
         syntax(" #on ");
         for (unsigned i = 0; i < tree_triggers(t); i++) {
            if (i > 0)
               printf(", ");
            dump_expr(tree_trigger(t, i));
         }
      }
      if (tree_has_value(t)) {
         syntax(" #until ");
         dump_expr(tree_value(t));
      }
      if (tree_has_delay(t)) {
         syntax(" #for ");
         dump_expr(tree_delay(t));
      }
      printf(";");
      if (tree_flags(t) & TREE_F_STATIC_WAIT)
         syntax("   -- static");
      syntax("\n");
      return;

   case T_BLOCK:
      dump_address(t);
      syntax("#block #is\n");
      dump_generics(t, indent + 2, ";\n");
      dump_generic_map(t, indent + 2, ";\n");
      dump_ports(t, indent + 2);
      dump_port_map(t, indent + 2, ";\n");
      dump_block(t, indent);
      tab(indent);
      syntax("#end #block;\n");
      return;

   case T_SEQUENCE:
      syntax("#block #is\n");
      dump_block(t, indent);
      tab(indent);
      syntax("#end #block;\n");
      return;

   case T_ASSERT:
      if (tree_has_value(t)) {
         syntax("#assert ");
         dump_expr(tree_value(t));
         printf(" ");
      }
      if (tree_has_message(t)) {
         syntax("#report ");
         dump_expr(tree_message(t));
         printf(" ");
      }
      syntax("#severity ");
      dump_expr(tree_severity(t));
      break;

   case T_WHILE:
      if (tree_has_value(t)) {
         syntax("#while ");
         dump_expr(tree_value(t));
         printf(" ");
      }
      syntax("#loop\n");
      for (unsigned i = 0; i < tree_stmts(t); i++)
         dump_stmt(tree_stmt(t, i), indent + 2);
      tab(indent);
      syntax("#end #loop");
      break;

   case T_IF:
      for (unsigned i = 0; i < tree_conds(t); i++) {
         tree_t c = tree_cond(t, i);
         if (tree_has_value(c)) {
            if (i > 0)
               tab(indent);

#ifdef DUMP_GEN_NAMES
            // T_CONDS can only have generate name
            const char *label = istr(tree_ident(c));
            printf("%s: ", label);
#endif
            if (i > 0) {
               syntax("#elsif ");
            } else {
               syntax("#if ");
            }
            dump_expr(tree_value(c));
            syntax(" #then\n");
         }
         else {
            tab(indent);
            syntax("#else\n");
         }
         for (unsigned i = 0; i < tree_stmts(c); i++)
            dump_stmt(tree_stmt(c, i), indent + 2);
      }
      tab(indent);
      syntax("#end #if");
      break;

   case T_EXIT:
      syntax("#exit %s", istr(tree_ident2(t)));
      if (tree_has_value(t)) {
         syntax(" #when ");
         dump_expr(tree_value(t));
      }
      break;

   case T_CASE:
      syntax("#case ");
      dump_expr(tree_value(t));
      syntax(" #is\n");
      for (unsigned i = 0; i < tree_assocs(t); i++) {
         tab(indent + 2);
         tree_t a = tree_assoc(t, i);   
#ifdef DUMP_GEN_NAMES
         // T_ASSOC can only have generate name
         const char *label = istr(tree_ident(a));
         printf("%s: ", label);
#endif
         switch (tree_subkind(a)) {
         case A_NAMED:
            syntax("#when ");
            dump_expr(tree_name(a));
            printf(" =>\n");
            break;
         case A_OTHERS:
            syntax("#when #others =>\n");
            break;
         case A_RANGE:
            syntax("#when ");
            dump_range(tree_range(a, 0));
            printf(" => ");
            break;
         default:
            assert(false);
         }
         dump_stmt(tree_value(a), indent + 4);
      }
      tab(indent);
      syntax("#end #case");
      break;

   case T_RETURN:
      syntax("#return");
      if (tree_has_value(t)) {
         printf(" ");
         dump_expr(tree_value(t));
      }
      break;

   case T_FOR:
      syntax("#for %s #in ", istr(tree_ident(tree_decl(t, 0))));
      dump_range(tree_range(t, 0));
      syntax(" #loop\n");
      for (unsigned i = 0; i < tree_stmts(t); i++)
         dump_stmt(tree_stmt(t, i), indent + 2);
      tab(indent);
      syntax("#end #for");
      break;

   case T_PROT_PCALL:
      dump_expr(tree_name(t));
      printf(".");
      // Fall-through
   case T_PCALL:
      if (tree_has_ref(t)) {
         tree_t decl = tree_ref(t);
         dump_address(decl);
         printf("%s", istr(tree_ident(decl)));
      }
      else
         printf("%s", istr(tree_ident(t)));
      dump_params(t, tree_param, tree_params(t), NULL);
      break;

   case T_FOR_GENERATE:
      syntax("#for %s #in ", istr(tree_ident(tree_decl(t, 0))));
      dump_range(tree_range(t, 0));
      syntax(" #generate\n");
      for (unsigned i = 1; i < tree_decls(t); i++)
         dump_decl(tree_decl(t, i), indent + 2);
      tab(indent);
      syntax("#begin\n");
      for (unsigned i = 0; i < tree_stmts(t); i++)
         dump_stmt(tree_stmt(t, i), indent + 2);
      tab(indent);
      syntax("#end #generate");
      break;

   case T_IF_GENERATE:
      syntax("#if ");
      dump_expr(tree_value(t));
      syntax(" #generate\n");
      for (unsigned i = 0; i < tree_decls(t); i++)
         dump_decl(tree_decl(t, i), indent + 2);
      tab(indent);
      syntax("#begin\n");
      for (unsigned i = 0; i < tree_stmts(t); i++)
         dump_stmt(tree_stmt(t, i), indent + 2);
      tab(indent);
      syntax("#end #generate");
      break;

   case T_INSTANCE:
      switch (tree_class(t)) {
      case C_ENTITY:    syntax("#entity "); break;
      case C_COMPONENT: syntax("#component "); break;
      default:
         assert(false);
      }
      printf("%s", istr(tree_ident2(t)));
      if (tree_has_spec(t)) {
         tree_t spec = tree_spec(t);
         if (tree_has_value(spec)) {
            tree_t bind = tree_value(spec);
            LOCAL_TEXT_BUF tb = tb_new();
            tb_cat(tb, istr(tree_ident(bind)));
            if (tree_has_ident2(bind))
               tb_printf(tb, "(%s)", istr(tree_ident2(bind)));
            syntax("  -- bound to %s\n", tb_get(tb));
         }
      }
      else if (tree_params(t) > 0 || tree_genmaps(t) > 0)
         printf("\n");
      dump_generic_map(t, indent + 4, tree_params(t) > 0 ? "\n" : "");
      dump_port_map(t, indent + 4, "");
      printf(";\n\n");
      return;

   case T_NEXT:
      syntax("#next");
      if (tree_has_value(t)) {
         syntax(" #when ");
         dump_expr(tree_value(t));
      }
      break;

   case T_NULL:
      syntax("#null");
      break;

   case T_COND_ASSIGN:
      dump_expr(tree_target(t));
      printf(" <= ");
      color_printf("$red$/* TODO: T_COND_ASSIGN */$$");
      break;

   case T_SELECT:
      printf(" <= ");
      if (tree_has_guard(t)) syntax("#guarded ");
      color_printf("$red$/* TODO: T_SELECT */$$");
      break;

   case T_CONCURRENT:
      if (tree_flags(t) & TREE_F_POSTPONED)
         syntax("#postponed ");
      if (tree_has_guard(t))
         syntax("#guarded ");
      dump_stmt(tree_stmt(t, 0), 0);
      break;

   default:
      cannot_dump(t, "stmt");
   }

   printf(";\n");
}

static void dump_port(tree_t t, int indent)
{
   tab(indent);
   dump_address(t);
   const char *class = NULL, *dir = NULL;
   switch (tree_class(t)) {
   case C_SIGNAL:    class = "signal";    break;
   case C_VARIABLE:  class = "variable";  break;
   case C_DEFAULT:   class = "";          break;
   case C_CONSTANT:  class = "constant";  break;
   case C_FILE:      class = "file";      break;
   case C_TYPE:      class = "type";      break;
   case C_FUNCTION:  class = "function";  break;
   case C_PROCEDURE: class = "procedure"; break;
   case C_PACKAGE:   class = "package";   break;
   default:
      assert(false);
   }
   switch (tree_subkind(t)) {
   case PORT_IN:      dir = "in";     break;
   case PORT_OUT:     dir = "out";    break;
   case PORT_INOUT:   dir = "inout";  break;
   case PORT_BUFFER:  dir = "buffer"; break;
   case PORT_INVALID: dir = "??";     break;
   }
   syntax("#%s %s : #%s ", class, istr(tree_ident(t)), dir);
   dump_type(tree_type(t));

   if (tree_has_value(t)) {
      printf(" := ");
      dump_expr(tree_value(t));
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
            syntax("#library %s;\n", istr(tree_ident(c)));
         }
         break;

      case T_USE:
         syntax("#use %s", istr(tree_ident(c)));
         if (tree_has_ident2(c)) {
            printf(".%s", istr(tree_ident2(c)));
         }
         printf(";\n");
         break;

      case T_CONTEXT_REF:
         syntax("#context %s;\n", istr(tree_ident(t)));
         break;

      default:
         break;
      }

      tab(indent);
   }

   if (nctx > 0) {
      printf("\n");
      tab(indent);
   }
}

static void dump_elab(tree_t t)
{
   dump_context(t, 0);
   dump_address(t);
   syntax("#entity %s #is\n#end #entity;\n\n", istr(tree_ident(t)));
   syntax("#architecture #elab #of %s #is\n", istr(tree_ident(t)));
   dump_decls(t, 2);
   syntax("#begin\n");
   for (unsigned i = 0; i < tree_stmts(t); i++)
      dump_stmt(tree_stmt(t, i), 2);
   syntax("#end #architecture;\n\n");
}

static void dump_entity(tree_t t)
{
   dump_context(t, 0);
   dump_address(t);
   syntax("#entity %s #is\n", istr(tree_ident(t)));
   if (tree_generics(t) > 0) {
      syntax("  #generic (\n");
      for (unsigned i = 0; i < tree_generics(t); i++) {
         if (i > 0)
            printf(";\n");
         tab(4);
         dump_port(tree_generic(t, i), 2);
      }
      printf("  );\n");
   }
   dump_ports(t, 2);
   dump_decls(t, 2);
   if (tree_stmts(t) > 0) {
      syntax("#begin\n");
      for (unsigned i = 0; i < tree_stmts(t); i++) {
         dump_stmt(tree_stmt(t, i), 2);
      }
   }
   syntax("#end #entity;\n\n");
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
         syntax("\n");
      was_body = is_body;
      dump_decl(d, indent);
   }
}

static void dump_arch(tree_t t)
{
   dump_context(t, 0);
   dump_address(t);
   syntax("#architecture %s #of %s #is\n",
          istr(tree_ident(t)), istr(tree_ident2(t)));
   dump_decls(t, 2);
   syntax("#begin\n");
   dump_stmts(t, 2);
   syntax("#end #architecture;\n\n");
}

static void dump_package(tree_t t, int indent)
{
   dump_context(t, indent);
   dump_address(t);
   syntax("#package %s #is\n", istr(tree_ident(t)));
   if (tree_kind(t) == T_PACK_INST && tree_has_ref(t)) {
      tab(indent);
      syntax("  -- Instantiated from %s\n", istr(tree_ident(tree_ref(t))));
   }
   dump_generics(t, indent + 2, ";\n");
   dump_generic_map(t, indent + 2, ";\n");
   dump_decls(t, indent + 2);
   tab(indent);
   syntax("#end #package;\n\n");
}

static void dump_package_body(tree_t t, int indent)
{
   dump_context(t, indent);
   dump_address(t);
   syntax("#package #body %s #is\n", istr(tree_ident(t)));
   dump_decls(t, indent + 2);
   tab(indent);
   syntax("#end #package #body;\n\n");
}

static void dump_configuration(tree_t t)
{
   dump_address(t);
   syntax("#configuration %s #of %s #is\n",
          istr(tree_ident(t)), istr(tree_ident2(t)));
   dump_decls(t, 2);
   syntax("#end #configuration\n");
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
   case T_PACKAGE:
   case T_PACK_INST:
      dump_package(t, 0);
      break;
   case T_PACK_BODY:
      dump_package_body(t, 0);
      break;
   case T_CONFIGURATION:
      dump_configuration(t);
      break;
   case T_REF:
   case T_FCALL:
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
      dump_expr(t);
      printf("\n");
      break;
   case T_INSTANCE:
   case T_FOR_GENERATE:
   case T_BLOCK:
   case T_PROCESS:
   case T_CASE:
   case T_FOR:
   case T_SIGNAL_ASSIGN:
   case T_IF:
      dump_stmt(t, 0);
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
      dump_decl(t, 0);
      break;
   case T_PORT_DECL:
   case T_GENERIC_DECL:
      dump_port(t, 0);
      printf("\n");
      break;
   case T_RANGE:
      dump_range(t);
      printf("\n");
      break;
   case T_BINDING:
      dump_binding(t, 0);
      break;
   case T_PARAM:
      dump_param(t);
      printf("\n");
      break;
   case T_CONSTRAINT:
      dump_constraint(t);
      printf("\n");
      break;
   default:
      cannot_dump(t, "tree");
   }
}

LCOV_EXCL_STOP
