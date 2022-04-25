//
//  Copyright (C) 2022-2023 Nick Gasson
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

static void vlog_dump_module(vlog_node_t v, int indent)
{
   print_syntax("#module %s", istr(vlog_ident(v)));

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

   print_syntax("#endmodule // %s\n\n", istr(vlog_ident(v)));
}

static void vlog_dump_port_decl(vlog_node_t v, int indent)
{
   tab(indent);

   switch (vlog_subkind(v)) {
   case V_PORT_INPUT: print_syntax("#input"); break;
   case V_PORT_INOUT: print_syntax("#inout"); break;
   case V_PORT_OUTPUT: print_syntax("#output"); break;
   case V_PORT_OUTPUT_REG: print_syntax("#output #reg"); break;
   }

   printf(" %s;\n", istr(vlog_ident(v)));
}

static void vlog_dump_always(vlog_node_t v, int indent)
{
   tab(indent);

   print_syntax("#always ");
   vlog_dump_tab(vlog_stmt(v, 0), indent);
}

static void vlog_dump_initial(vlog_node_t v, int indent)
{
   tab(indent);

   print_syntax("#initial ");
   vlog_dump_tab(vlog_stmt(v, 0), indent);
}

static void vlog_dump_seq_block(vlog_node_t v, int indent)
{
   print_syntax("#begin\n");

   const int nstmts = vlog_stmts(v);
   for (int i = 0; i < nstmts; i++)
      vlog_dump_tab(vlog_stmt(v, i), indent + 2);

   tab(indent);
   print_syntax("#end\n");
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
   case V_EVENT_POSEDGE: print_syntax("#posedge "); break;
   case V_EVENT_NEGEDGE: print_syntax("#negedge "); break;
   }

   vlog_dump(vlog_value(v));
}

static void vlog_dump_nbassign(vlog_node_t v, int indent)
{
   tab(indent);
   vlog_dump(vlog_target(v));
   print_syntax(" <= ");
   vlog_dump(vlog_value(v));
   print_syntax(";\n");
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

   print_syntax(";\n");
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
      print_syntax("\n");
      fflush(stdout);
      fatal_trace("cannot dump %s", vlog_kind_str(vlog_kind(v)));
   }
}

void vlog_dump(vlog_node_t v)
{
   vlog_dump_tab(v, 0);
}
