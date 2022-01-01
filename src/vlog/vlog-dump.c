//
//  Copyright (C) 2022 Nick Gasson
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
#include "ident.h"
#include "vlog/vlog-node.h"
#include "vlog/vlog-phase.h"

#include <ctype.h>

static void vlog_dump_tab(vlog_node_t v, int indent);

static void tab(int indent)
{
   while (indent--)
      fputc(' ', stdout);
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
               || (*p == '/' && *(p + 1) == '*')
               || (*p == '/' && *(p + 1) == '/')) {
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

static void vlog_dump_module(vlog_node_t v, int indent)
{
   syntax("#module %s", istr(vlog_ident(v)));

   const int nports = vlog_ports(v);
   if (nports > 0) {
      printf(" (");
      for (int i = 0; i < nports; i++) {
         if (i > 0) printf(", ");
         vlog_dump(vlog_port(v, i));
      }
      printf(")");
   }

   printf(";\n");

   const int ndecls = vlog_decls(v);
   for (int i = 0; i < ndecls; i++)
      vlog_dump_tab(vlog_decl(v, i), indent + 2);

   const int nstmts = vlog_stmts(v);
   for (int i = 0; i < nstmts; i++)
      vlog_dump_tab(vlog_stmt(v, i), indent + 2);

   syntax("#endmodule // %s\n\n", istr(vlog_ident(v)));
}

static void vlog_dump_port_decl(vlog_node_t v, int indent)
{
   tab(indent);

   switch (vlog_subkind(v)) {
   case V_PORT_INPUT: syntax("#input"); break;
   case V_PORT_INOUT: syntax("#inout"); break;
   case V_PORT_OUTPUT: syntax("#output"); break;
   case V_PORT_OUTPUT_REG: syntax("#output #reg"); break;
   }

   printf(" %s;\n", istr(vlog_ident(v)));
}

static void vlog_dump_always(vlog_node_t v, int indent)
{
   tab(indent);

   syntax("#always ");
   vlog_dump_tab(vlog_stmt(v, 0), indent);
}

static void vlog_dump_initial(vlog_node_t v, int indent)
{
   tab(indent);

   syntax("#initial ");
   vlog_dump_tab(vlog_stmt(v, 0), indent);
}

static void vlog_dump_seq_block(vlog_node_t v, int indent)
{
   syntax("#begin\n");

   const int nstmts = vlog_stmts(v);
   for (int i = 0; i < nstmts; i++)
      vlog_dump_tab(vlog_stmt(v, i), indent + 2);

   tab(indent);
   syntax("#end\n");
}

static void vlog_dump_timing(vlog_node_t v, int indent)
{
   printf("@(");
   vlog_dump(vlog_value(v));
   printf(")\n");

   if (vlog_stmts(v) > 0)
      vlog_dump_tab(vlog_stmt(v, 0), indent + 2);
   else
      printf(";\n");
}

static void vlog_dump_event(vlog_node_t v)
{
   switch (vlog_subkind(v)) {
   case V_EVENT_POSEDGE: syntax("#posedge "); break;
   case V_EVENT_NEGEDGE: syntax("#negedge "); break;
   }

   vlog_dump(vlog_value(v));
}

static void vlog_dump_nbassign(vlog_node_t v, int indent)
{
   tab(indent);
   vlog_dump(vlog_target(v));
   printf(" <= ");
   vlog_dump(vlog_value(v));
   printf(";\n");
}

static void vlog_dump_systask_enable(vlog_node_t v, int indent)
{
   tab(indent);
   printf("%s", istr(vlog_ident(v)));

   const int nparams = vlog_params(v);
   if (nparams > 0) {
      printf("(");
      for (int i = 0; i < nparams; i++) {
         if (i > 0) printf(", ");
         vlog_dump(vlog_param(v, i));
      }
      printf(")");
   }

   printf(";\n");
}

static void vlog_dump_string(vlog_node_t v)
{
   //printf("\"%s\"", vlog_text(v));
}

static void vlog_dump_number(vlog_node_t v)
{
   //printf("'b%s", vlog_text(v));
}

static void vlog_dump_tab(vlog_node_t v, int indent)
{
   switch (vlog_kind(v)) {
   case V_MODULE:
      vlog_dump_module(v, indent);
      break;
   case V_REF:
      printf("%s", istr(vlog_ident(v)));
      break;
   case V_PORT_DECL:
      vlog_dump_port_decl(v, indent);
      break;
   case V_ALWAYS:
      vlog_dump_always(v, indent);
      break;
   case V_INITIAL:
      vlog_dump_initial(v, indent);
      break;
   case V_TIMING:
      vlog_dump_timing(v, indent);
      break;
   case V_EVENT:
      vlog_dump_event(v);
      break;
   case V_NBASSIGN:
      vlog_dump_nbassign(v, indent);
      break;
   case V_SEQ_BLOCK:
      vlog_dump_seq_block(v, indent);
      break;
   case V_SYSTASK_ENABLE:
      vlog_dump_systask_enable(v, indent);
      break;
   case V_STRING:
      vlog_dump_string(v);
      break;
   case V_NUMBER:
      vlog_dump_number(v);
      break;
   default:
      fatal_trace("cannot dump %s", vlog_kind_str(vlog_kind(v)));
   }
}

void vlog_dump(vlog_node_t v)
{
   vlog_dump_tab(v, 0);
}
