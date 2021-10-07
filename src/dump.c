//
//  Copyright (C) 2011-2021  Nick Gasson
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
#include "hash.h"
#include "common.h"

#include <assert.h>
#include <string.h>
#include <ctype.h>
#include <inttypes.h>

#define DUMP_TYPE_HINT 0
#define DUMP_ADDRESS   0

LCOV_EXCL_START

static void dump_expr(tree_t t);
static void dump_stmt(tree_t t, int indent);
static void dump_port(tree_t t, int indent);
static void dump_decl(tree_t t, int indent);
static void dump_decls(tree_t t, int indent);
static void dump_type(type_t type);

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
      }
      else if (*p == '#') {
         tb_printf(tb, "$bold$$cyan$");
         highlighting = true;
      }
      else if (*p == '~') {
         tb_printf(tb, "$yellow$");
         highlighting = true;
      }
      else if (*p == '-' && *(p + 1) == '-') {
         tb_printf(tb, "$red$-");
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
         tree_t p = (*get)(t, i);
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
   color_printf("$!green${%p}$$", t);
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
      case L_STRING:
         {
            printf("\"");
            const int nchars = tree_chars(t);
            for (int i = 0; i < nchars; i++)
               printf("%c", ident_char(tree_ident(tree_char(t, i)), 1));
            printf("\"");
         }
         break;
      default:
         assert(false);
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
      printf("%s'(", istr(type_ident(tree_type(t))));
      dump_expr(tree_value(t));
      printf(")");
      break;

   case T_OPEN:
      syntax("#open");
      break;

   default:
      cannot_dump(t, "expr");
   }

   dump_type_hint(t);
}

static void dump_type(type_t type)
{
   if (type_kind(type) == T_SUBTYPE && type_has_ident(type))
      printf("%s", type_pp(type));
   else if (type_is_array(type) && !type_is_unconstrained(type)) {
      printf("%s(", type_pp(type));
      const int ndims = dimension_of(type);
      for (int i = 0; i < ndims; i++) {
         if (i > 0) printf(", ");
         dump_range(range_of(type, i));
      }
      printf(")");
   }
   else
      printf("%s", type_pp(type));
}

static void dump_arguments(tree_t t, int indent)
{
   const int nports = tree_ports(t);
   if (nports > 0) {
      if (nports > 1) {
         printf(" (\n");
         indent += 4;
      }
      else {
         printf(" (");
         indent = 0;
      }
      for (int i = 0; i < nports; i++) {
         if (i > 0)
            printf(";\n");
         dump_port(tree_port(t, i), indent);
      }
      printf(" )");
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

static void dump_generics(tree_t t, int indent)
{
   const int ngenerics = tree_generics(t);
   if (ngenerics > 0) {
      tab(indent);
      syntax("#generic (");
      if (ngenerics > 1) {
         printf("\n");
         for (unsigned i = 0; i < ngenerics; i++) {
            if (i > 0) printf(";\n");
            dump_port(tree_generic(t, i), indent + 2);
         }
      }
      else
         dump_port(tree_generic(t, 0), 1);
      printf(" );\n");
   }
}

static void dump_port_map(tree_t t, int indent)
{
   const int nparams = tree_params(t);
   if (nparams > 0) {
      tab(indent);
      dump_params(t, tree_param, nparams, "#port #map");
      printf(";\n");
   }
}

static void dump_generic_map(tree_t t, int indent)
{
   const int ngenmaps = tree_genmaps(t);
   if (ngenmaps > 0) {
      tab(indent);
      dump_params(t, tree_genmap, ngenmaps, "#generic #map");
      printf(";\n");
   }
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
   if (is_subprogram(t) && tree_has_ident2(t)) {
      tab(indent + 2);
      syntax("-- %s\n", istr(tree_ident2(t)));
   }
   dump_decls(t, indent + 2);
   tab(indent);
   syntax("#begin\n");
   dump_stmts(t, indent + 2);
}

static void dump_wait_level(tree_t t)
{
   switch (tree_attr_int(t, wait_level_i, WAITS_MAYBE)) {
   case WAITS_NO:
      syntax("   -- Never waits");
      break;
   case WAITS_MAYBE:
      syntax("   -- Maybe waits");
      break;
   case WAITS_YES:
      syntax("   -- Waits");
      break;
   }
}

static void dump_type_decl(tree_t t, int indent)
{
   type_t type = tree_type(t);
   const type_kind_t kind = type_kind(type);

   if (kind == T_SUBTYPE) {
      syntax("#subtype %s #is ", istr(tree_ident(t)));
      if (type_has_resolution(type)) {
         dump_expr(type_resolution(type));
         printf(" ");
      }
      dump_type(type_base(type));
      if (type_has_constraint(type)) {
         tree_t c = type_constraint(type);
         switch (tree_subkind(c)) {
         case C_INDEX:
            {
               printf(" (");
               const int nranges = tree_ranges(c);
               for (int i = 0; i < nranges; i++) {
                  if (i > 0) printf(", ");
                  dump_range(tree_range(c, i));
               }
               printf(")");
            }
            break;
         case C_RANGE:
            syntax(" #range ");
            dump_range(tree_range(c, 0));
            break;
         }
      }
      printf(";\n");
      return;
   }

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
      printf("(");
      if (kind == T_UARRAY) {
         const int nindex = type_index_constrs(type);
         for (int i = 0; i < nindex; i++) {
            if (i > 0) printf(", ");
            dump_type(type_index_constr(type, i));
            syntax(" #range <>");
         }
      }
      else if (kind == T_SUBTYPE) {
         tree_t constraint = type_constraint(type);
         const int nranges = tree_ranges(constraint);
         for (int i = 0; i < nranges; i++) {
            if (i > 0) printf(", ");
            dump_range(tree_range(constraint, i));
         }
      }
      else {
         const int ndims = type_dims(type);
         for (int i = 0; i < ndims; i++) {
            if (i > 0) printf(", ");
            dump_range(type_dim(type, i));
         }
      }
      printf(")");
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

static void dump_decl(tree_t t, int indent)
{
   tab(indent);
   if (tree_kind(t) != T_HIER) dump_address(t);

   switch (tree_kind(t)) {
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

   case T_SPEC:
      syntax("#for %s\n", istr(tree_ident(t)));
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
      syntax("#alias %s : ", istr(tree_ident(t)));
      dump_type(tree_type(t));
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

   case T_GENVAR:
      syntax("#genvar %s : ", istr(tree_ident(t)));
      dump_type(tree_type(t));
      printf(";\n");
      return;

   case T_FUNC_DECL:
      if (tree_flags(t) & TREE_F_PREDEFINED)
         syntax("-- predefined %s\n", type_pp(tree_type(t)));
      else {
         syntax("#function %s", istr(tree_ident(t)));
         dump_arguments(t, indent);
         syntax(" #return %s;\n", type_pp(type_result(tree_type(t))));
      }
      return;

   case T_FUNC_BODY:
      syntax("#function %s", istr(tree_ident(t)));
      dump_arguments(t, indent);
      syntax(" #return %s #is\n", type_pp(type_result(tree_type(t))));
      dump_block(t, indent);
      tab(indent);
      syntax("#end #function;\n");
      return;

   case T_PROC_DECL:
      if (tree_flags(t) & TREE_F_PREDEFINED)
         syntax("-- predefined %s\n", type_pp(tree_type(t)));
      else {
         syntax("#procedure %s", istr(tree_ident(t)));
         dump_arguments(t, indent);
         printf(";");
         dump_wait_level(t);
         syntax("\n");
      }
      return;

   case T_PROC_BODY:
      syntax("#procedure %s", istr(tree_ident(t)));
      dump_arguments(t, indent);
      syntax(" #is");
      dump_wait_level(t);
      syntax("\n");
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
      syntax("#component %s is\n", istr(tree_ident(t)));
      dump_generics(t, indent + 2);
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

   default:
      cannot_dump(t, "decl");
   }

   dump_type(tree_type(t));

   if (tree_kind(t) != T_FIELD_DECL && tree_has_value(t)) {
      printf(" := ");
      dump_expr(tree_value(t));
   }
   printf(";");

   if (tree_attr_int(t, ident_new("returned"), 0))
      syntax(" -- returned");

   printf("\n");
}

static void dump_stmt(tree_t t, int indent)
{
   tab(indent);

   if (tree_has_ident(t)) {
      const char *label = istr(tree_ident(t));
      if (label[0] != 'l')   // Skip generated labels
         printf("%s: ", label);
   }

   switch (tree_kind(t)) {
   case T_PROCESS:
      dump_address(t);
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
      for (unsigned i = 0; i < tree_waveforms(t); i++) {
         if (i > 0)
            printf(", ");
         tree_t w = tree_waveform(t, i);
         dump_expr(tree_value(w));
         if (tree_has_delay(w)) {
            syntax(" #after ");
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
      syntax("#wait");
      if (tree_triggers(t) > 0) {
         syntax(" #on ");
         for (unsigned i = 0; i < tree_triggers(t); i++) {
            if (i > 0)
               printf(", ");
            dump_expr(tree_trigger(t, i));
         }
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
      dump_generics(t, indent + 2);
      dump_generic_map(t, indent + 2);
      dump_ports(t, indent + 2);
      dump_port_map(t, indent + 2);
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
      syntax("#if ");
      dump_expr(tree_value(t));
      syntax(" #then\n");
      for (unsigned i = 0; i < tree_stmts(t); i++)
         dump_stmt(tree_stmt(t, i), indent + 2);
      if (tree_else_stmts(t) > 0) {
         tab(indent);
         syntax("#else\n");
         for (unsigned i = 0; i < tree_else_stmts(t); i++)
            dump_stmt(tree_else_stmt(t, i), indent + 2);
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
         switch (tree_subkind(a)) {
         case A_NAMED:
            syntax("#when ");
            dump_expr(tree_name(a));
            printf(" =>\n");
            break;
         case A_OTHERS:
            syntax("#when #others =>\n");
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
         tree_t bind = tree_value(tree_spec(t));
         syntax(" -- bound to %s", istr(tree_ident(bind)));
         if (tree_has_ident2(bind))
            printf("(%s)", istr(tree_ident2(bind)));
      }
      printf("\n");
      if (tree_genmaps(t) > 0) {
         tab(indent + 4);
         dump_params(t, tree_genmap, tree_genmaps(t), "#generic #map");
         printf("\n");
      }
      if (tree_params(t) > 0) {
         tab(indent + 4);
         dump_params(t, tree_param, tree_params(t), "#port #map");
      }
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
   case C_SIGNAL:   class = "signal";   break;
   case C_VARIABLE: class = "variable"; break;
   case C_DEFAULT:  class = "";         break;
   case C_CONSTANT: class = "constant"; break;
   case C_FILE:     class = "file";     break;
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
}

static void dump_context(tree_t t)
{
   const int nctx = tree_contexts(t);
   for (int i = 0; i < nctx; i++) {
      tree_t c = tree_context(t, i);
      switch (tree_kind(c)) {
      case T_LIBRARY:
         if (tree_ident(c) != std_i && tree_ident(c) != work_i)
            syntax("#library %s;\n", istr(tree_ident(c)));
         break;

      case T_USE:
         syntax("#use %s", istr(tree_ident(c)));
         if (tree_has_ident2(c)) {
            printf(".%s", istr(tree_ident2(c)));
         }
         printf(";\n");
         break;

      case T_CTXREF:
         syntax("#context %s;\n", istr(tree_ident(t)));

      default:
         break;
      }
   }

   if (nctx > 0)
      printf("\n");
}

static void dump_elab(tree_t t)
{
   dump_context(t);
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
   dump_context(t);
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
   dump_context(t);
   dump_address(t);
   syntax("#architecture %s #of %s #is\n",
          istr(tree_ident(t)), istr(tree_ident2(t)));
   dump_decls(t, 2);
   syntax("#begin\n");
   for (unsigned i = 0; i < tree_stmts(t); i++)
      dump_stmt(tree_stmt(t, i), 2);
   syntax("#end #architecture;\n\n");
}

static void dump_package(tree_t t)
{
   dump_context(t);
   syntax("#package %s #is\n", istr(tree_ident(t)));
   dump_decls(t, 2);
   syntax("#end #package;\n\n");
}

static void dump_package_body(tree_t t)
{
   dump_context(t);
   syntax("#package #body %s #is\n", istr(tree_ident(t)));
   dump_decls(t, 2);
   syntax("#end #package #body;\n\n");
}

static void dump_configuration(tree_t t)
{
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
      dump_package(t);
      break;
   case T_PACK_BODY:
      dump_package_body(t);
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
      dump_expr(t);
      printf("\n");
      break;
   case T_INSTANCE:
   case T_FOR_GENERATE:
   case T_BLOCK:
   case T_PROCESS:
   case T_CASE:
   case T_FOR:
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
      dump_decl(t, 0);
      break;
   case T_PORT_DECL:
      dump_port(t, 0);
      break;
   case T_RANGE:
      dump_range(t);
      printf("\n");
      break;
   default:
      cannot_dump(t, "tree");
   }
}

LCOV_EXCL_STOP
