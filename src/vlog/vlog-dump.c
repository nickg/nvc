//
//  Copyright (C) 2022-2025 Nick Gasson
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
#include "object.h"
#include "ident.h"
#include "vlog/vlog-node.h"
#include "vlog/vlog-number.h"
#include "vlog/vlog-phase.h"

#include <ctype.h>

#define DUMP_ADDRESS 0

static inline void tab(int indent)
{
   print_syntax("%*s", indent, "");
}

static void vlog_dump_address(vlog_node_t v)
{
#if DUMP_ADDRESS
   uint32_t a = (uint32_t)((uintptr_t)object_arena(vlog_to_object(v)) >> 2);
   a = (a ^ 61) ^ (a >> 16);
   a = a + (a << 3);
   a = a ^ (a >> 4);
   a = a * UINT32_C(0x27d4eb2d);
   a = a ^ (a >> 15);

   const int r = 1 + a % 5;
   const int g = 1 + (a >> 8) % 5;
   const int b = 1 + (a >> 16) % 5;

   const int color = 16 + 36*r + 6*g + b;

   char *LOCAL fmt = xasprintf("$!#%d${%%p:%s}$$", color,
                               vlog_kind_str(vlog_kind(v)));
   color_printf(fmt, v);
#endif
}

static void vlog_dump_paren(vlog_node_t v, int indent)
{
   switch (vlog_kind(v)) {
   case V_REF:
   case V_BIT_SELECT:
   case V_NUMBER:
   case V_USER_FCALL:
   case V_SYS_FCALL:
      vlog_dump(v, indent);
      break;
   default:
      print_syntax("(");
      vlog_dump(v, indent);
      print_syntax(")");
   }
}

static void vlog_dump_module(vlog_node_t v, int indent)
{
   vlog_dump_address(v);
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

static void vlog_dump_primitive(vlog_node_t v, int indent)
{
   vlog_dump_address(v);
   print_syntax("#primitive %s", istr(vlog_ident2(v)));

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

   print_syntax("#endprimitive // %s\n\n", istr(vlog_ident2(v)));
}

static void vlog_dump_udp_entry(vlog_node_t v, int indent)
{
   tab(indent);

   vlog_udp_symbol_t last = V_UDP_SYMBOL_INPUT;
   const int nsymbols = vlog_params(v);
   for (int i = 0; i < nsymbols; i++) {
      vlog_node_t sym = vlog_param(v, i);

      const vlog_udp_symbol_t kind = vlog_subkind(sym);
      if (kind != last) {
         print_syntax(":");
         last = kind;
      }

      switch (vlog_kind(sym)) {
      case V_UDP_LEVEL:
         print_syntax("%c", (char)vlog_ival(sym));
         break;
      case V_UDP_EDGE:
         print_syntax("(%c%c)", (char)vlog_ival(vlog_left(sym)),
                      (char)vlog_ival(vlog_right(sym)));
         break;
      default:
         should_not_reach_here();
      }
   }

   print_syntax(";\n");
}

static void vlog_dump_udp_table(vlog_node_t v, int indent)
{
   tab(indent);
   print_syntax("#table\n");

   const int nparams = vlog_params(v);
   for (int i = 0; i < nparams; i++)
      vlog_dump(vlog_param(v, i), indent + 2);

   tab(indent);
   print_syntax("#endtable\n");
}

static void vlog_dump_port_decl(vlog_node_t v, int indent)
{
   tab(indent);
   vlog_dump_address(v);

   switch (vlog_subkind(v)) {
   case V_PORT_INPUT: print_syntax("#input"); break;
   case V_PORT_INOUT: print_syntax("#inout"); break;
   case V_PORT_OUTPUT: print_syntax("#output"); break;
   }

   print_syntax(" %s;\n", istr(vlog_ident(v)));
}

static void vlog_dump_dimensions(vlog_node_t v, int indent)
{
   const int ndims = vlog_ranges(v);
   int dim = 0;
   for (; dim < ndims; dim++) {
      vlog_node_t d = vlog_range(v, dim);
      print_syntax(" [");
      vlog_dump(vlog_left(d), indent);
      print_syntax(":");
      vlog_dump(vlog_right(d), indent);
      print_syntax("]");
   }
}

static void vlog_dump_net_decl(vlog_node_t v, int indent)
{
   tab(indent);
   vlog_dump_address(v);

   switch (vlog_subkind(v)) {
   case V_NET_WIRE: print_syntax("#wire "); break;
   }

   vlog_dump(vlog_type(v), indent);
   print_syntax(" %s", istr(vlog_ident(v)));
   vlog_dump_dimensions(v, indent);
   print_syntax(";\n");
}

static void vlog_dump_var_decl(vlog_node_t v, int indent)
{
   tab(indent);
   vlog_dump_address(v);

   vlog_dump(vlog_type(v), indent);
   print_syntax(" %s", istr(vlog_ident(v)));
   vlog_dump_dimensions(v, indent);

   if (vlog_has_value(v)) {
      print_syntax(" = ");
      vlog_dump(vlog_value(v), indent);
   }

   print_syntax(";\n");
}

static void vlog_dump_always(vlog_node_t v, int indent)
{
   tab(indent);

   switch (vlog_subkind(v)) {
   case V_ALWAYS_FF:    print_syntax("#always_ff "); break;
   case V_ALWAYS_COMB:  print_syntax("#always_comb "); break;
   case V_ALWAYS_LATCH: print_syntax("#always_latch "); break;
   default:             print_syntax("#always "); break;
   }

   vlog_dump(vlog_stmt(v, 0), indent);
   print_syntax("\n");
}

static void vlog_dump_initial(vlog_node_t v, int indent)
{
   tab(indent);

   print_syntax("#initial ");
   vlog_dump(vlog_stmt(v, 0), indent);
   print_syntax("\n");
}

static void vlog_dump_block(vlog_node_t v, int indent)
{
   print_syntax("#begin\n");

   const int ndecls = vlog_decls(v);
   for (int i = 0; i < ndecls; i++)
      vlog_dump(vlog_decl(v, i), indent + 2);

   const int nstmts = vlog_stmts(v);
   for (int i = 0; i < nstmts; i++) {
      tab(indent + 2);
      vlog_dump(vlog_stmt(v, i), indent + 2);
      print_syntax("\n");
   }

   tab(indent);
   print_syntax("#end");
}

static void vlog_dump_timing(vlog_node_t v, int indent)
{
   vlog_dump(vlog_value(v), indent);

   if (vlog_stmts(v) > 0) {
      print_syntax(" ");
      vlog_dump(vlog_stmt(v, 0), indent);
   }
   else
      print_syntax(";");
}

static void vlog_dump_event(vlog_node_t v)
{
   switch (vlog_subkind(v)) {
   case V_EVENT_POSEDGE: print_syntax("#posedge "); break;
   case V_EVENT_NEGEDGE: print_syntax("#negedge "); break;
   }

   vlog_dump(vlog_value(v), 0);
}

static void vlog_dump_event_control(vlog_node_t v)
{
   print_syntax("@(");

   const int nparams = vlog_params(v);
   for (int i = 0; i < nparams; i++) {
      if (i > 0) print_syntax(", ");
      vlog_dump(vlog_param(v, i), 0);
   }

   print_syntax(")");
}

static void vlog_dump_nbassign(vlog_node_t v, int indent)
{
   vlog_dump(vlog_target(v), 0);
   print_syntax(" <= ");
   if (vlog_has_delay(v)) {
      vlog_dump(vlog_delay(v), 0);
      print_syntax(" ");
   }
   vlog_dump(vlog_value(v), 0);
   print_syntax(";");
}

static void vlog_dump_bassign(vlog_node_t v, int indent)
{
   vlog_dump(vlog_target(v), 0);
   print_syntax(" = ");

   if (vlog_has_delay(v)) {
      vlog_dump(vlog_delay(v), 0);
      print_syntax(" ");
   }

   vlog_dump(vlog_value(v), 0);
   print_syntax(";");
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

static void vlog_dump_stmt_or_null(vlog_node_t v, int indent)
{
   switch (vlog_stmts(v)) {
   case 0:  print_syntax(";"); break;
   case 1:  vlog_dump(vlog_stmt(v, 0), indent); break;
   default: should_not_reach_here();
   }
}

static void vlog_dump_if(vlog_node_t v, int indent)
{
   print_syntax("#if ");

   const int nconds = vlog_conds(v);
   for (int i = 0; i < nconds; i++) {
      if (i > 0) {
         print_syntax("\n");
         tab(indent);
         print_syntax("#else ");
      }

      vlog_node_t c = vlog_cond(v, i);
      if (vlog_has_value(c)) {
         print_syntax("(");
         vlog_dump(vlog_value(c), 0);
         print_syntax(") ");
      }

      vlog_dump_stmt_or_null(c, indent);
   }
}

static void vlog_dump_sys_tcall(vlog_node_t v, int indent)
{
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

   print_syntax(";");
}

static void vlog_dump_sys_fcall(vlog_node_t v)
{
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
}

static void vlog_dump_string(vlog_node_t v)
{
   print_syntax("\"");

   number_t n = vlog_number(v);
   for (int i = number_width(n)/8 - 1; i >= 0; i--) {
      const uint8_t byte = number_byte(n, i);
      if (byte >= 0x20 && byte <= 0x7e)
         print_syntax("%c", byte);
      else
         print_syntax("\\x%02x", byte);
   }

   print_syntax("\"");
}

static void vlog_dump_number(vlog_node_t v)
{
   LOCAL_TEXT_BUF tb = tb_new();
   number_print(vlog_number(v), tb);

   print_syntax("%s", tb_get(tb));
}

static void vlog_dump_real(vlog_node_t v)
{
   print_syntax("%g", vlog_dval(v));
}

static void vlog_dump_binary(vlog_node_t v)
{
   vlog_dump_paren(vlog_left(v), 0);

   switch (vlog_subkind(v)) {
   case V_BINARY_OR:       print_syntax(" | "); break;
   case V_BINARY_AND:      print_syntax(" & "); break;
   case V_BINARY_XOR:      print_syntax(" ^ "); break;
   case V_BINARY_CASE_EQ:  print_syntax(" === "); break;
   case V_BINARY_CASE_NEQ: print_syntax(" !== "); break;
   case V_BINARY_LOG_EQ:   print_syntax(" == "); break;
   case V_BINARY_LOG_NEQ:  print_syntax(" != "); break;
   case V_BINARY_LOG_OR:   print_syntax(" || "); break;
   case V_BINARY_LOG_AND:  print_syntax(" && "); break;
   case V_BINARY_PLUS:     print_syntax(" + "); break;
   case V_BINARY_MINUS:    print_syntax(" - "); break;
   case V_BINARY_TIMES:    print_syntax(" * "); break;
   case V_BINARY_DIVIDE:   print_syntax(" / "); break;
   case V_BINARY_LT:       print_syntax(" < "); break;
   case V_BINARY_GT:       print_syntax(" > "); break;
   case V_BINARY_SHIFT_LL: print_syntax(" << "); break;
   case V_BINARY_SHIFT_RL: print_syntax(" >> "); break;
   case V_BINARY_SHIFT_LA: print_syntax(" <<< "); break;
   case V_BINARY_SHIFT_RA: print_syntax(" >>> "); break;
   }

   vlog_dump_paren(vlog_right(v), 0);
}

static void vlog_dump_unary(vlog_node_t v)
{
   switch (vlog_subkind(v)) {
   case V_UNARY_NOT: print_syntax("!"); break;
   case V_UNARY_BITNEG: print_syntax("~~"); break;
   }

   vlog_dump_paren(vlog_value(v), 0);
}

static void vlog_dump_postfix(vlog_node_t v)
{
   vlog_dump(vlog_target(v), 0);

   switch (vlog_subkind(v)) {
   case V_INCDEC_PLUS: print_syntax("++"); break;
   case V_INCDEC_MINUS: print_syntax("--"); break;
   }
}

static void vlog_dump_prefix(vlog_node_t v)
{
   switch (vlog_subkind(v)) {
   case V_INCDEC_PLUS: print_syntax("++"); break;
   case V_INCDEC_MINUS: print_syntax("--"); break;
   }

   vlog_dump(vlog_target(v), 0);
}

static void vlog_dump_user_fcall(vlog_node_t v)
{
   print_syntax("%s(", istr(vlog_ident(v)));

   const int nparams = vlog_params(v);
   for (int i = 0; i < nparams; i++) {
      if (i > 0) print_syntax(", ");
      vlog_dump(vlog_param(v, i), 0);
   }

   print_syntax(")");
}

static void vlog_dump_delay_control(vlog_node_t v, int indent)
{
   print_syntax("##");
   vlog_dump(vlog_value(v), 0);
}

static void vlog_dump_gate_inst(vlog_node_t v, int indent)
{
   tab(indent);

   switch (vlog_subkind(v)) {
   case V_GATE_PULLUP: print_syntax("#pullup "); break;
   case V_GATE_PULLDOWN: print_syntax("#pulldown "); break;
   }

   const int nparams = vlog_params(v);
   if (nparams > 0) {
      print_syntax("(");
      for (int i = 0; i < nparams; i++) {
         if (i > 0) print_syntax(",");
         vlog_dump(vlog_param(v, i), 0);
      }
      print_syntax(") ");
   }

   if (vlog_has_ident(v))
      print_syntax("%s ", istr(vlog_ident(v)));

   print_syntax("(");
   vlog_dump(vlog_target(v), 0);
   print_syntax(");\n");
}

static void vlog_dump_mod_inst(vlog_node_t v, int indent)
{
   print_syntax("%s ", istr(vlog_ident(v)));

   const int nparams = vlog_params(v);
   print_syntax("(");
   for (int i = 0; i < nparams; i++) {
      if (i > 0) print_syntax(",");
      vlog_dump(vlog_param(v, i), 0);
   }
   print_syntax(")");
}

static void vlog_dump_inst_list(vlog_node_t v, int indent)
{
   tab(indent);

   print_syntax("%s ", istr(vlog_ident(v)));

   const int nparams = vlog_params(v);
   if (nparams > 0) {
      print_syntax("##(");
      for (int i = 0; i < nparams; i++) {
         if (i > 0) print_syntax(",");
         vlog_dump(vlog_param(v, i), 0);
      }
      print_syntax(") ");
   }

   const int nstmts = vlog_stmts(v);
   for (int i = 0; i < nstmts; i++) {
      if (i > 0) print_syntax(", ");
      vlog_dump(vlog_stmt(v, i), indent + 2);
   }

   print_syntax(";\n");
}

static void vlog_dump_param_assign(vlog_node_t v, int indent)
{
   if (vlog_has_ident(v)) {
      print_syntax(".%s(", istr(vlog_ident(v)));
      if (vlog_has_value(v))
         vlog_dump(vlog_value(v), indent);
      print_syntax(")");
   }
   else
      vlog_dump(vlog_value(v), indent);
}

static void vlog_dump_strength(vlog_node_t v, int indent)
{
   static const char *map[] = {
      "highz", "small", "medium", "weak", "large", "pull", "strong", "supply"
   };

   const unsigned level = vlog_subkind(v);
   print_syntax("%s0,%s1", map[STRENGTH0(level)], map[STRENGTH1(level)]);
}

static void vlog_dump_bit_select(vlog_node_t v, int indent)
{
   vlog_dump(vlog_value(v), 0);

   const int nparams = vlog_params(v);
   for (int i = 0; i < nparams; i++) {
      print_syntax("[");
      vlog_dump(vlog_param(v, i), 0);
      print_syntax("]");
   }
}

static void vlog_dump_part_select(vlog_node_t v, int indent)
{
   vlog_dump(vlog_value(v), 0);
   print_syntax("[");
   vlog_dump(vlog_left(v), 0);
   print_syntax(":");
   vlog_dump(vlog_right(v), 0);
   print_syntax("]");
}

static void vlog_dump_data_type(vlog_node_t v, int indent)
{
   switch (vlog_subkind(v)) {
   case DT_LOGIC: print_syntax("#logic"); break;
   case DT_REAL: print_syntax("#real"); break;
   case DT_REALTIME: print_syntax("#realtime"); break;
   case DT_SHORTREAL: print_syntax("#shortreal"); break;
   case DT_INTEGER: print_syntax("#integer"); break;
   case DT_BIT: print_syntax("#bit"); break;
   case DT_INT: print_syntax("#int"); break;
   case DT_BYTE: print_syntax("#byte"); break;
   default: should_not_reach_here();
   }

   vlog_dump_dimensions(v, indent);
}

static void vlog_dump_do_while(vlog_node_t v, int indent)
{
   print_syntax("#do ");

   vlog_dump_stmt_or_null(v, indent);

   print_syntax(" #while (");
   vlog_dump(vlog_value(v), indent);
   print_syntax(");");
}

static void vlog_dump_while(vlog_node_t v, int indent)
{
   print_syntax("#while (");
   vlog_dump(vlog_value(v), indent);
   print_syntax(")");

   vlog_dump_stmt_or_null(v, indent);
}

static void vlog_dump_repeat(vlog_node_t v, int indent)
{
   print_syntax("#repeat (");
   vlog_dump(vlog_value(v), indent);
   print_syntax(") ");

   vlog_dump_stmt_or_null(v, indent);
}

static void vlog_dump_forever(vlog_node_t v, int indent)
{
   print_syntax("#forever ");
   vlog_dump_stmt_or_null(v, indent);
}

static void vlog_dump_for_init(vlog_node_t v, int indent)
{
   const int nstmts = vlog_stmts(v);
   if (nstmts == 0)
      print_syntax(";");
   else {
      for (int i = 0; i < nstmts; i++) {
         vlog_dump(vlog_stmt(v, i), indent);
         print_syntax(" ");
      }
   }
}

static void vlog_dump_for_step(vlog_node_t v, int indent)
{
   const int nstmts = vlog_stmts(v);
   if (nstmts == 0)
      print_syntax(";");
   else {
      for (int i = 0; i < nstmts; i++) {
         print_syntax(" ");
         vlog_dump(vlog_stmt(v, i), indent);
      }
   }
}

static void vlog_dump_for_loop(vlog_node_t v, int indent)
{
   print_syntax("#for (");
   vlog_dump(vlog_left(v), 0);

   if (vlog_has_value(v))
      vlog_dump(vlog_value(v), 0);

   print_syntax(";");
   vlog_dump(vlog_right(v), 0);
   print_syntax(") ");

   vlog_dump_stmt_or_null(v, indent);
}

static void vlog_dump_case(vlog_node_t v, int indent)
{
   static const char *suffix[] = { "", "x", "z" };

   print_syntax("#case%s (", suffix[vlog_subkind(v)]);
   vlog_dump(vlog_value(v), indent);
   print_syntax(")\n");

   const int nstmts = vlog_stmts(v);
   for (int i = 0; i < nstmts; i++)
      vlog_dump(vlog_stmt(v, i), indent + 2);

   tab(indent);
   print_syntax("#endcase");
}

static void vlog_dump_case_item(vlog_node_t v, int indent)
{
   tab(indent);

   const int nparams = vlog_params(v);
   if (nparams == 0)
      print_syntax("#default");
   else {
      vlog_dump(vlog_param(v, 0), 0);
      for (int i = 1; i < nparams; i++) {
         print_syntax(", ");
         vlog_dump(vlog_param(v, i), 0);
      }
   }

   print_syntax(": ");
   vlog_dump_stmt_or_null(v, indent);
   print_syntax("\n");
}

static void vlog_dump_cond_expr(vlog_node_t v, int indent)
{
   vlog_dump(vlog_value(v), 0);
   print_syntax(" ? ");
   vlog_dump(vlog_left(v), 0);
   print_syntax(" : ");
   vlog_dump(vlog_right(v), 0);
}

static void vlog_dump_concat(vlog_node_t v, int indent)
{
   print_syntax("{");

   const bool multiple = vlog_has_value(v);
   if (multiple) {
      vlog_dump_paren(vlog_value(v), 0);
      print_syntax("{");
   }

   const int nparams = vlog_params(v);
   for (int i = 0; i < nparams; i++) {
      if (i > 0) print_syntax(", ");
      vlog_dump(vlog_param(v, i), 0);
   }

   if (multiple)
      print_syntax("}");

   print_syntax("}");
}

static void vlog_dump_param_decl(vlog_node_t v, int indent)
{
   tab(indent);
   vlog_dump_address(v);

   switch (vlog_kind(v)) {
   case V_PARAM_DECL:
      print_syntax("#parameter ");
      break;
   case V_LOCALPARAM:
      print_syntax("#localparam ");
      break;
   default:
      should_not_reach_here();
   }

   vlog_dump(vlog_type(v), indent);
   print_syntax(" %s", istr(vlog_ident(v)));

   if (vlog_has_value(v)) {
      print_syntax(" = ");
      vlog_dump(vlog_value(v), indent);
   }

   print_syntax(";\n");
}

static void vlog_dump_ref(vlog_node_t v, int indent)
{
   if (vlog_has_ref(v))
      vlog_dump_address(vlog_ref(v));

   print_syntax("%s", istr(vlog_ident(v)));
}

static void vlog_dump_hier_ref(vlog_node_t v, int indent)
{
   if (vlog_has_ref(v))
      vlog_dump_address(vlog_ref(v));

   print_syntax("%s.%s", istr(vlog_ident2(v)), istr(vlog_ident(v)));
}

static void vlog_dump_task_decl(vlog_node_t v, int indent)
{
   tab(indent);
   print_syntax("#task %s(", istr(vlog_ident(v)));

   print_syntax(");\n");

   const int nstmts = vlog_stmts(v);
   for (int i = 0; i < nstmts; i++) {
      tab(indent + 2);
      vlog_dump(vlog_stmt(v, i), indent + 2);
      print_syntax("\n");
   }

   tab(indent);
   print_syntax("#endtask\n");
}

static void vlog_dump_func_decl(vlog_node_t v, int indent)
{
   tab(indent);
   print_syntax("#function %s;", istr(vlog_ident(v)));

   if (vlog_has_ident2(v))
      print_syntax(" -- %s.%s", istr(vlog_ident2(v)), istr(vlog_ident(v)));

   print_syntax("\n");

   const int nports = vlog_ports(v);
   for (int i = 0; i < nports; i++)
      vlog_dump(vlog_port(v, i), indent + 2);

   const int ndecls = vlog_decls(v);
   for (int i = 0; i < ndecls; i++)
      vlog_dump(vlog_decl(v, i), indent + 2);

   const int nstmts = vlog_stmts(v);
   for (int i = 0; i < nstmts; i++) {
      tab(indent + 2);
      vlog_dump(vlog_stmt(v, i), indent + 2);
      print_syntax("\n");
   }

   tab(indent);
   print_syntax("#endfunction\n");
}

static void vlog_dump_wait(vlog_node_t v, int indent)
{
   print_syntax("#wait (");
   vlog_dump(vlog_value(v), 0);
   print_syntax(") ");
   vlog_dump_stmt_or_null(v, indent);
}

static void vlog_dump_op_assign(vlog_node_t v)
{
   vlog_dump(vlog_target(v), 0);

   switch (vlog_subkind(v)) {
   case V_ASSIGN_EQUALS: print_syntax(" = "); break;
   case V_ASSIGN_PLUS:   print_syntax(" += "); break;
   }

   vlog_dump(vlog_value(v), 0);
   print_syntax(";");
}

static void vlog_dump_enum_decl(vlog_node_t v)
{
   print_syntax("#enum");

   if (vlog_has_type(v)) {
      print_syntax(" ");
      vlog_dump(vlog_type(v), 0);
   }

   print_syntax(" { ");

   const int ndecls = vlog_decls(v);
   for (int i = 0; i < ndecls; i++) {
      if (i > 0) print_syntax(", ");
      vlog_dump(vlog_decl(v, i), 0);
   }

   print_syntax(" }");
}

static void vlog_dump_enum_name(vlog_node_t v)
{
   print_syntax("%s", istr(vlog_ident(v)));

   if (vlog_has_value(v)) {
      print_syntax("=");
      vlog_dump(vlog_value(v), 0);
   }
}

void vlog_dump(vlog_node_t v, int indent)
{
   switch (vlog_kind(v)) {
   case V_INST_BODY:
      print_syntax("// Instantiated module %s\n", istr(vlog_ident(v)));
      // Fall-through
   case V_MODULE:
      vlog_dump_module(v, indent);
      break;
   case V_PRIMITIVE:
      vlog_dump_primitive(v, indent);
      break;
   case V_REF:
      vlog_dump_ref(v, indent);
      break;
   case V_HIER_REF:
      vlog_dump_hier_ref(v, indent);
      break;
   case V_PORT_DECL:
   case V_TF_PORT_DECL:
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
   case V_EVENT_CONTROL:
      vlog_dump_event_control(v);
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
   case V_BLOCK:
      vlog_dump_block(v, indent);
      break;
   case V_SYS_TCALL:
      vlog_dump_sys_tcall(v, indent);
      break;
   case V_SYS_FCALL:
      vlog_dump_sys_fcall(v);
      break;
   case V_STRING:
      vlog_dump_string(v);
      break;
   case V_NUMBER:
      vlog_dump_number(v);
      break;
   case V_REAL:
      vlog_dump_real(v);
      break;
   case V_IF:
   case V_IF_GENERATE:
      vlog_dump_if(v, indent);
      break;
   case V_BINARY:
      vlog_dump_binary(v);
      break;
   case V_UNARY:
      vlog_dump_unary(v);
      break;
   case V_POSTFIX:
      vlog_dump_postfix(v);
      break;
   case V_PREFIX:
      vlog_dump_prefix(v);
      break;
   case V_DELAY_CONTROL:
      vlog_dump_delay_control(v, indent);
      break;
   case V_GATE_INST:
      vlog_dump_gate_inst(v, indent);
      break;
   case V_MOD_INST:
      vlog_dump_mod_inst(v, indent);
      break;
   case V_INST_LIST:
      vlog_dump_inst_list(v, indent);
      break;
   case V_STRENGTH:
      vlog_dump_strength(v, indent);
      break;
   case V_BIT_SELECT:
      vlog_dump_bit_select(v, indent);
      break;
   case V_PART_SELECT:
      vlog_dump_part_select(v, indent);
      break;
   case V_UDP_ENTRY:
      vlog_dump_udp_entry(v, indent);
      break;
   case V_UDP_TABLE:
      vlog_dump_udp_table(v, indent);
      break;
   case V_DATA_TYPE:
      vlog_dump_data_type(v, indent);
      break;
   case V_EMPTY:
      break;
   case V_DO_WHILE:
      vlog_dump_do_while(v, indent);
      break;
   case V_WHILE:
      vlog_dump_while(v, indent);
      break;
   case V_REPEAT:
      vlog_dump_repeat(v, indent);
      break;
   case V_FOREVER:
      vlog_dump_forever(v, indent);
      break;
   case V_FOR_LOOP:
      vlog_dump_for_loop(v, indent);
      break;
   case V_FOR_INIT:
      vlog_dump_for_init(v, indent);
      break;
   case V_FOR_STEP:
      vlog_dump_for_step(v, indent);
      break;
   case V_CASE:
      vlog_dump_case(v, indent);
      break;
   case V_COND_EXPR:
      vlog_dump_cond_expr(v, indent);
      break;
   case V_CASE_ITEM:
      vlog_dump_case_item(v, indent);
      break;
   case V_CONCAT:
      vlog_dump_concat(v, indent);
      break;
   case V_PARAM_DECL:
   case V_LOCALPARAM:
      vlog_dump_param_decl(v, indent);
      break;
   case V_PARAM_ASSIGN:
   case V_PORT_CONN:
      vlog_dump_param_assign(v, indent);
      break;
   case V_TASK_DECL:
      vlog_dump_task_decl(v, indent);
      break;
   case V_FUNC_DECL:
      vlog_dump_func_decl(v, indent);
      break;
   case V_WAIT:
      vlog_dump_wait(v, indent);
      break;
   case V_USER_FCALL:
      vlog_dump_user_fcall(v);
      break;
   case V_OP_ASSIGN:
      vlog_dump_op_assign(v);
      break;
   case V_ENUM_DECL:
      vlog_dump_enum_decl(v);
      break;
   case V_ENUM_NAME:
      vlog_dump_enum_name(v);
      break;
   default:
      print_syntax("\n");
      fflush(stdout);
      fatal_trace("cannot dump %s", vlog_kind_str(vlog_kind(v)));
   }
}
