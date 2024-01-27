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
#include "vlog/vlog-number.h"
#include "vlog/vlog-phase.h"

#include <ctype.h>

static inline void tab(int indent)
{
   print_syntax("%*s", indent, "");
}

static void vlog_dump_module(vlog_node_t v, int indent)
{
   print_syntax("#module %s", istr(vlog_ident2(v)));

   const int nports = vlog_ports(v);
   if (nports > 0) {
      print_syntax(" (");
      for (int i = 0; i < nports; i++) {
         if (i > 0) print_syntax(", ");
         vlog_dump(vlog_port(v, i), 0);
      }
      print_syntax(")");
   }

   print_syntax(";\n");

   const int ndecls = vlog_decls(v);
   for (int i = 0; i < ndecls; i++)
      vlog_dump(vlog_decl(v, i), indent + 2);

   const int nstmts = vlog_stmts(v);
   for (int i = 0; i < nstmts; i++)
      vlog_dump(vlog_stmt(v, i), indent + 2);

   print_syntax("#endmodule // %s\n\n", istr(vlog_ident2(v)));
}

static void vlog_dump_root(vlog_node_t v, int indent)
{
   const int ndecls = vlog_decls(v);
   for (int i = 0; i < ndecls; i++)
      vlog_dump(vlog_decl(v, i), indent);

   const int nstmts = vlog_stmts(v);
   for (int i = 0; i < nstmts; i++)
      vlog_dump(vlog_stmt(v, i), indent);
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

   print_syntax(" %s;\n", istr(vlog_ident(v)));
}

static void vlog_dump_net_decl(vlog_node_t v, int indent)
{
   tab(indent);

   switch (vlog_subkind(v)) {
   case V_NET_WIRE: print_syntax("#wire"); break;
   }

   const int ndims = vlog_ranges(v);
   int dim = 0;
   for (; dim < ndims; dim++) {
      vlog_node_t d = vlog_range(v, dim);
      if (vlog_subkind(d) != V_DIM_PACKED)
         break;

      print_syntax(" [");
      vlog_dump(vlog_left(d), indent);
      print_syntax(":");
      vlog_dump(vlog_right(d), indent);
      print_syntax("]");
   }

   print_syntax(" %s;\n", istr(vlog_ident(v)));
}

static void vlog_dump_var_decl(vlog_node_t v, int indent)
{
   tab(indent);
   print_syntax("#reg");

   const int ndims = vlog_ranges(v);
   int dim = 0;
   for (; dim < ndims; dim++) {
      vlog_node_t d = vlog_range(v, dim);
      if (vlog_subkind(d) != V_DIM_PACKED)
         break;

      print_syntax(" [");
      vlog_dump(vlog_left(d), indent);
      print_syntax(":");
      vlog_dump(vlog_right(d), indent);
      print_syntax("]");
   }

   print_syntax(" %s;\n", istr(vlog_ident(v)));
}

static void vlog_dump_always(vlog_node_t v, int indent)
{
   tab(indent);

   print_syntax("#always ");
   vlog_dump(vlog_stmt(v, 0), indent);
}

static void vlog_dump_initial(vlog_node_t v, int indent)
{
   tab(indent);

   print_syntax("#initial ");
   vlog_dump(vlog_stmt(v, 0), indent);
}

static void vlog_dump_seq_block(vlog_node_t v, int indent)
{
   print_syntax("#begin\n");

   const int nstmts = vlog_stmts(v);
   for (int i = 0; i < nstmts; i++)
      vlog_dump(vlog_stmt(v, i), indent + 2);

   tab(indent);
   print_syntax("#end\n");
}

static void vlog_dump_timing(vlog_node_t v, int indent)
{
   print_syntax("@(");
   vlog_dump(vlog_value(v), 0);
   print_syntax(")\n");

   if (vlog_stmts(v) > 0)
      vlog_dump(vlog_stmt(v, 0), indent + 2);
   else
      print_syntax(";\n");
}

static void vlog_dump_event(vlog_node_t v)
{
   switch (vlog_subkind(v)) {
   case V_EVENT_POSEDGE: print_syntax("#posedge "); break;
   case V_EVENT_NEGEDGE: print_syntax("#negedge "); break;
   }

   vlog_dump(vlog_value(v), 0);
}

static void vlog_dump_nbassign(vlog_node_t v, int indent)
{
   tab(indent);
   vlog_dump(vlog_target(v), 0);
   print_syntax(" <= ");
   vlog_dump(vlog_value(v), 0);
   print_syntax(";\n");
}

static void vlog_dump_bassign(vlog_node_t v, int indent)
{
   tab(indent);
   vlog_dump(vlog_target(v), 0);

   switch (vlog_subkind(v)) {
   case V_ASSIGN_EQUALS:
      print_syntax(" = ");
      break;
   }

   vlog_dump(vlog_value(v), 0);
   print_syntax(";\n");
}

static void vlog_dump_assign(vlog_node_t v, int indent)
{
   tab(indent);
   print_syntax("#assign ");
   vlog_dump(vlog_target(v), 0);
   print_syntax(" = ");
   vlog_dump(vlog_value(v), 0);
   print_syntax(";\n");
}

static void vlog_dump_if(vlog_node_t v, int indent)
{
   tab(indent);
   print_syntax("#if ");

   const int nconds = vlog_conds(v);
   for (int i = 0; i < nconds; i++) {
      if (i > 0) {
         tab(indent);
         print_syntax("#else ");
      }

      vlog_node_t c = vlog_cond(v, i);
      if (vlog_has_value(c)) {
         print_syntax("(");
         vlog_dump(vlog_value(c), 0);
         print_syntax(")");
      }

      const int nstmts = vlog_stmts(c);
      if (nstmts == 0)
         print_syntax(";\n");
      else {
         print_syntax("\n");
         for (int i = 0; i < nstmts; i++)
            vlog_dump(vlog_stmt(c, i), indent + 2);
      }
   }
}

static void vlog_dump_systask(vlog_node_t v, int indent)
{
   tab(indent);
   print_syntax("%s", istr(vlog_ident(v)));

   const int nparams = vlog_params(v);
   if (nparams > 0) {
      print_syntax("(");
      for (int i = 0; i < nparams; i++) {
         if (i > 0) print_syntax(", ");
         vlog_dump(vlog_param(v, i), 0);
      }
      print_syntax(")");
   }

   print_syntax(";\n");
}

static void vlog_dump_string(vlog_node_t v)
{
   print_syntax("\"%s\"", vlog_text(v));
}

static void vlog_dump_number(vlog_node_t v)
{
   LOCAL_TEXT_BUF tb = tb_new();
   number_print(vlog_number(v), tb);

   print_syntax("%s", tb_get(tb));
}

static void vlog_dump_binary(vlog_node_t v)
{
   vlog_dump(vlog_left(v), 0);

   switch (vlog_subkind(v)) {
   case V_BINARY_OR: print_syntax(" | "); break;
   case V_BINARY_AND: print_syntax(" & "); break;
   }

   vlog_dump(vlog_right(v), 0);
}

void vlog_dump(vlog_node_t v, int indent)
{
   switch (vlog_kind(v)) {
   case V_MODULE:
      vlog_dump_module(v, indent);
      break;
   case V_ROOT:
      vlog_dump_root(v, indent);
      break;
   case V_REF:
      print_syntax("%s", istr(vlog_ident(v)));
      break;
   case V_PORT_DECL:
      vlog_dump_port_decl(v, indent);
      break;
   case V_NET_DECL:
      vlog_dump_net_decl(v, indent);
      break;
   case V_VAR_DECL:
      vlog_dump_var_decl(v, indent);
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
   case V_BASSIGN:
      vlog_dump_bassign(v, indent);
      break;
   case V_ASSIGN:
      vlog_dump_assign(v, indent);
      break;
   case V_SEQ_BLOCK:
      vlog_dump_seq_block(v, indent);
      break;
   case V_SYSTASK:
      vlog_dump_systask(v, indent);
      break;
   case V_STRING:
      vlog_dump_string(v);
      break;
   case V_NUMBER:
      vlog_dump_number(v);
      break;
   case V_IF:
      vlog_dump_if(v, indent);
      break;
   case V_BINARY:
      vlog_dump_binary(v);
      break;
   default:
      print_syntax("\n");
      fflush(stdout);
      fatal_trace("cannot dump %s", vlog_kind_str(vlog_kind(v)));
   }
}
