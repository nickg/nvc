//
//  Copyright (C) 2022  Nick Gasson
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

#include <assert.h>

#include "util.h"
#include "sdf/sdf-node.h"
#include "common.h"
#include "ident.h"

typedef struct {
   unsigned       flag;
   const char    *flag_str;
} value_flag_pair_t;

value_flag_pair_t flag_pairs[2] = {
      {.flag = S_F_VALUE_ABSOLUTE,   .flag_str = "ABSOLUTE"},
      {.flag = S_F_VALUE_INCREMENT,  .flag_str = "INCREMENT"}
   };


static void tab(int indent)
{
   print_syntax("%*s", indent, "");
}

static void sdf_dump_number(sdf_node_t number)
{
   int kind = sdf_subkind(number);
   switch (kind) {
   case S_NUMBER_DOUBLE:
      print_syntax("%lf", sdf_dval(number));
      break;
   case S_NUMBER_INTEGER:
      print_syntax("%ld", sdf_ival(number));
      break;
   default:
      fatal_trace("cannot dump number subkind: %d", kind);
   }
}

static void sdf_dump_port_instance(sdf_node_t port)
{
   int ndims = sdf_dims(port);

   // TODO: Walk through the ident and replace '.' by actual hchar!
   print_syntax("%s", istr(sdf_ident(port)));

   if (ndims) {
      print_syntax("[");

      sdf_dump_number(sdf_dim(port, 0));

      if (ndims > 1) {
         print_syntax(":");
         sdf_dump_number(sdf_dim(port, 1));
      }

      print_syntax("]");
   }

   print_syntax(" ");
}

static void sdf_dump_port_edge(sdf_node_t port)
{
   sdf_flags_t flags = sdf_flags(port);

   print_syntax("(");

   if (flags & S_F_POSEDGE)
      print_syntax("posedge ");
   else
      print_syntax("negedge ");
   sdf_dump_port_instance(port);

   print_syntax(")");
}

static void sdf_dump_port_spec(sdf_node_t port)
{
   if (sdf_flags(port) != 0)
      sdf_dump_port_edge(port);
   else
      sdf_dump_port_instance(port);
}

static void sdf_dump_tripple(sdf_node_t tripple)
{
   if (sdf_has_min(tripple))
      sdf_dump_number(sdf_min(tripple));

   print_syntax(":");

   if (sdf_has_typ(tripple))
      sdf_dump_number(sdf_typ(tripple));

   print_syntax(":");

   if (sdf_has_max(tripple))
      sdf_dump_number(sdf_max(tripple));
}

static void sdf_dump_rvalue(sdf_node_t rvalue)
{
   if (sdf_has_number(rvalue)) {
      sdf_node_t num = sdf_number(rvalue);
      sdf_kind_t kind = sdf_kind(num);

      switch (kind) {
      case S_TRIPPLE:
         sdf_dump_tripple(num);
         break;
      case S_NUMBER:
         sdf_dump_number(num);
         break;
      default:
         fatal_trace("cannot dump ralue kind %s", sdf_kind_str(kind));
      }
   }
}

static void sdf_dump_delval(sdf_node_t delval)
{
   int nrvalues = sdf_values(delval);

   if (nrvalues)
      print_syntax("(");

   for (int j = 0; j < nrvalues; j++)
      sdf_dump_rvalue(sdf_value(delval, j));

   if (nrvalues)
      print_syntax(") ");
}

static void sdf_dump_value_list(sdf_node_t s)
{
   for (int i = 0; i < sdf_values(s); i++) {
      sdf_node_t val = sdf_value(s, i);
      sdf_kind_t kind = sdf_kind(val);
      switch (kind) {
      case S_DELVAL:
         sdf_dump_delval(val);
         break;
      default:
         fatal_trace("cannot dump value kind: %s", sdf_kind_str(kind));
      }
   }
}

static void sdf_dump_expr(sdf_node_t expr)
{
   sdf_kind_t kind = sdf_kind(expr);

   switch (kind) {
   case S_UNARY:
   {
      sdf_unary_expr_kind_t subkind = sdf_subkind(expr);
      switch (subkind) {
      case S_UNARY_EXPR_PLUS:
         print_syntax("+");
         break;
      case S_UNARY_EXPR_MINUS:
         print_syntax("-");
         break;
      case S_UNARY_EXPR_LOGNOT:
         print_syntax("!");
         break;
      case S_UNARY_EXPR_BITNOT:
         print_syntax("~");
         break;
      case S_UNARY_EXPR_AND:
         print_syntax("&");
         break;
      case S_UNARY_EXPR_NAND:
         print_syntax("~&");
         break;
      case S_UNARY_EXPR_OR:
         print_syntax("|");
         break;
      case S_UNARY_EXPR_NOR:
         print_syntax("~|");
         break;
      case S_UNARY_EXPR_XOR:
         print_syntax("^");
         break;
      case S_UNARY_EXPR_XNOR:
         print_syntax("~^");
         break;
      default:
         fatal_trace("cannot dump unary kind: %d", subkind);
      }

      print_syntax("(");
      sdf_dump_expr(sdf_value(expr, 0));
      print_syntax(")");
      break;
   }
   case S_BINARY:
      print_syntax("(");
      sdf_dump_expr(sdf_value(expr, 0));
      print_syntax(")");

      sdf_binary_expr_kind_t subkind = sdf_subkind(expr);
      switch (subkind) {
      case S_BINARY_EXPR_PLUS:
         print_syntax("+");
         break;
      case S_BINARY_EXPR_MINUS:
         print_syntax("-");
         break;
      case S_BINARY_EXPR_MULT:
         print_syntax("*");
         break;
      case S_BINARY_EXPR_DIV:
         print_syntax("/");
         break;
      case S_BINARY_EXPR_MOD:
         print_syntax("%%");
         break;
      case S_BINARY_EXPR_LOGEQ:
         print_syntax("==");
         break;
      case S_BINARY_EXPR_LOGNEQ:
         print_syntax("!=");
         break;
      case S_BINARY_EXPR_CASEEQ:
         print_syntax("===");
         break;
      case S_BINARY_EXPR_CASENEQ:
         print_syntax("!==");
         break;
      case S_BINARY_EXPR_LOGAND:
         print_syntax("&&");
         break;
      case S_BINARY_EXPR_LOGOR:
         print_syntax("||");
         break;
      case S_BINARY_EXPR_LT:
         print_syntax("<");
         break;
      case S_BINARY_EXPR_LTEQ:
         print_syntax("<=");
         break;
      case S_BINARY_EXPR_GT:
         print_syntax(">");
         break;
      case S_BINARY_EXPR_GTEQ:
         print_syntax(">=");
         break;
      case S_BINARY_EXPR_BITAND:
         print_syntax("&");
         break;
      case S_BINARY_EXPR_BITOR:
         print_syntax("|");
         break;
      case S_BINARY_EXPR_BITXOR:
         print_syntax("^");
         break;
      case S_BINARY_EXPR_BITXNOR:
         print_syntax("~^");
         break;
      case S_BINARY_EXPR_SHRIGHT:
         print_syntax(">>");
         break;
      case S_BINARY_EXPR_SHLEFT:
         print_syntax("<<");
         break;
      default:
         fatal_trace("cannot dump binary kind: %d", subkind);
      }

      print_syntax("(");
      sdf_dump_expr(sdf_value(expr, 1));
      print_syntax(")");
      break;

   case S_SIGNAL:
      sdf_dump_port_instance(expr);
      break;

   default:
      fatal_trace("cannot dump expr kind: %s", sdf_kind_str(kind));
   }
}

static void sdf_dump_cond(sdf_node_t cond)
{
   sdf_cond_kind_t kind = sdf_subkind(cond);

   switch (kind) {
   case S_COND_COND:
      print_syntax("#COND ");
      break;
   case S_COND_CONDELSE:
      print_syntax("#CONDELSE ");
      break;
   case S_COND_SCOND:
      print_syntax("#SCOND ");
      break;
   case S_COND_CCOND:
      print_syntax("#CCOND ");
      break;
   default:
      fatal_trace("cannot cond subkind: %d", kind);
   }

   if (sdf_has_ident(cond))
      print_syntax("%s ", istr(sdf_ident(cond)));

   if (sdf_has_expr(cond))
      sdf_dump_expr(sdf_expr(cond));

   // Signals related to cond are not dumped together with cond,
   // but they are dumped by the caler!
}

static void sdf_dump_port_tchk(sdf_node_t port)
{
   int nconds = sdf_conds(port);
   if (nconds) {
      print_syntax("(");
      sdf_dump_cond(sdf_cond(port, 0));
   }

   sdf_dump_port_spec(port);

   if (nconds)
      print_syntax(")");
}

static void sdf_dump_edge(sdf_node_t e, int indent)
{
   tab(indent);
   print_syntax("(");
   if (sdf_flags(e) & S_F_POSEDGE)
      print_syntax("posedge ");
   else
      print_syntax("negedge ");

   sdf_dump_number(sdf_value(e, 0));
   print_syntax(" ");
   if (sdf_values(e) > 1)
      sdf_dump_number(sdf_value(e, 1));

   print_syntax(")\n");
}

static void sdf_dump_edge_list(sdf_node_t tenv, int indent)
{
   // value[0] is "real_number" in WAVEFORM env

   for (int i = 1; i < sdf_values(tenv); i++) {
      sdf_node_t edge = sdf_value(tenv, i);
      sdf_dump_edge(sdf_value(edge, 0), indent);
      sdf_dump_edge(sdf_value(edge, 1), indent);
   }
}

static void sdf_dump_del_def(sdf_node_t delay, int indent)
{
   sdf_delay_kind_t kind = sdf_subkind(delay);

   int used_indent = indent;
   int nconds = sdf_conds(delay);

   if (nconds) {
      tab(used_indent);
      print_syntax("(");
      sdf_dump_cond(sdf_cond(delay, 0));
      print_syntax("\n");
      used_indent += 2;
   }

   tab(used_indent);
   print_syntax("(");

   int n_sigs = sdf_signals(delay);
   sdf_node_t s_0 = (n_sigs > 0) ? sdf_signal(delay, 0) : NULL;
   sdf_node_t s_1 = (n_sigs > 1) ? sdf_signal(delay, 1) : NULL;

   switch (kind) {
   case S_DELAY_KIND_IOPATH:
      print_syntax("#IOPATH ");
      sdf_dump_port_spec(s_0);
      sdf_dump_port_instance(s_1);
      break;
   case S_DELAY_KIND_PORT:
      print_syntax("#PORT ");
      sdf_dump_port_instance(s_0);
      break;
   case S_DELAY_KIND_INTERCONNECT:
      print_syntax("#INTERCONNECT ");
      sdf_dump_port_instance(s_0);
      sdf_dump_port_instance(s_1);
      break;
   case S_DELAY_KIND_NETDELAY:
      print_syntax("#NETDELAY ");
      sdf_dump_port_spec(s_0);
      break;
   case S_DELAY_KIND_DEVICE:
      print_syntax("#DEVICE ");
      if (n_sigs > 0)
         sdf_dump_port_instance(s_0);
      break;
   default:
      fatal_trace("cannot dump delay subkind: %d", kind);
      break;
   }

   sdf_dump_value_list(delay);
   print_syntax(")\n");

   if (nconds) {
      tab(indent);
      print_syntax(")\n");
   }
}

static void sdf_dump_label(sdf_node_t label, int indent)
{
   tab(indent);
   print_syntax("(%s ", istr(sdf_ident(label)));
   sdf_dump_value_list(label);
   print_syntax(")\n");
}

static void sdf_dump_lbl_spec(sdf_node_t cell, int indent)
{
   tab(indent);
   print_syntax("(#LABEL\n");

   // Dump ABSOLUTE/INCREMENT labels groupped together by deltyp flag
   for (int i = 0; i < 2; i++) {
      bool exist = false;
      for (int j = 0; j < sdf_labels(cell); j++) {
         sdf_node_t label = sdf_label(cell, j);
         sdf_flags_t flags = sdf_flags(label);

         if (flags & flag_pairs[i].flag) {
            if (!exist) {
               tab(indent + 2);
               print_syntax("(#%s\n", flag_pairs[i].flag_str);
               exist = true;
            }
            sdf_dump_label(label, indent + 4);
         }
      }

      if (exist) {
         tab(indent + 2);
         print_syntax(")\n");
      }
   }

   tab(indent);
   print_syntax(")\n");
}

static void sdf_dump_del_spec(sdf_node_t cell, int indent)
{
   tab(indent);
   print_syntax("(#DELAY\n");

   // Dump ABSOLUTE/INCREMENT delays groupped together by deltype flag
   for (int i = 0; i < 2; i++) {
      bool exist = false;
      for (int j = 0; j < sdf_delays(cell); j++) {
         sdf_node_t delay = sdf_delay(cell, j);
         sdf_flags_t flags = sdf_flags(delay);

         if (flags & flag_pairs[i].flag) {
            if (!exist) {
               tab(indent + 2);
               print_syntax("(#%s\n", flag_pairs[i].flag_str);
               exist = true;
            }
            sdf_dump_del_def(delay, indent + 4);
         }
      }

      if (exist) {
         tab(indent + 2);
         print_syntax(")\n");
      }
   }

   // Dump PATHPULSE/PATHPULSEPERCENT delays -> No deltype flag
   for (int i = 0; i < sdf_delays(cell); i++) {
      sdf_node_t delay = sdf_delay(cell, i);
      sdf_delay_kind_t kind = sdf_subkind(delay);

      if (kind == S_DELAY_KIND_PATHPULSE || kind == S_DELAY_KIND_PATHPULSEP)
         sdf_dump_del_def(delay, indent + 2);
   }

   tab(indent);
   print_syntax(")\n");
}

static void sdf_dump_tc_spec(sdf_node_t cell, int indent)
{
   tab(indent);
   print_syntax("(#TIMINGCHECK\n");

   for (int i = 0; i < sdf_tchecks(cell); i++) {

      tab(indent + 2);

      sdf_node_t tcheck = sdf_tcheck(cell, i);
      sdf_tcheck_kind_t kind = sdf_subkind(tcheck);

      int n_sigs = sdf_signals(tcheck);
      int n_vals = sdf_values(tcheck);

      sdf_node_t s_0 = sdf_signal(tcheck, 0);
      sdf_node_t s_1 = (n_sigs > 1) ? sdf_signal(tcheck, 1) : NULL;
      sdf_node_t v_0 = sdf_value(tcheck, 0);
      sdf_node_t v_1 = (n_vals > 1) ? sdf_value(tcheck, 1) : NULL;

      switch (kind) {
      case S_TCHECK_SETUP:
         print_syntax("(#SETUP ");
         sdf_dump_port_tchk(s_1);
         sdf_dump_port_tchk(s_1);
         sdf_dump_rvalue(v_0);
         break;

      case S_TCHECK_HOLD:
         print_syntax("(#HOLD ");
         sdf_dump_port_tchk(s_0);
         sdf_dump_port_tchk(s_1);
         sdf_dump_rvalue(v_0);
         break;

      case S_TCHECK_SETUPHOLD:
         print_syntax("(#SETUPHOLD ");
         // parser satisfies that port_tcheck is only port_spec when optional trailing
         // scond and ccond are present!
         sdf_dump_port_tchk(s_0);
         sdf_dump_port_tchk(s_1);
         sdf_dump_rvalue(s_0);
         sdf_dump_rvalue(s_1);
         for (int j = 0; j < sdf_conds(tcheck); j++) {
            print_syntax("(");
            sdf_dump_cond(sdf_cond(tcheck, j));
            print_syntax(")");
         }
         break;

      case S_TCHECK_RECOVERY:
         print_syntax("(#RECOVERY ");
         sdf_dump_port_tchk(s_0);
         sdf_dump_port_tchk(s_1);
         sdf_dump_rvalue(v_0);
         break;

      case S_TCHECK_REMOVAL:
         print_syntax("(#REMOVAL ");
         sdf_dump_port_tchk(s_0);
         sdf_dump_port_tchk(s_1);
         sdf_dump_rvalue(v_0);
         break;

      case S_TCHECK_RECREM:
         print_syntax("(#SETUPHOLD ");
         // parser satisfies that port_tcheck is only port_spec when optional trailing
         // scond and ccond are present!
         sdf_dump_port_tchk(s_0);
         sdf_dump_port_tchk(s_1);
         sdf_dump_rvalue(v_0);
         sdf_dump_rvalue(v_1);
         for (int j = 0; j < sdf_conds(tcheck); j++) {
            print_syntax("(");
            sdf_dump_cond(sdf_cond(tcheck, j));
            print_syntax(")");
         }
         break;

      case S_TCHECK_SKEW:
         print_syntax("(#SKEW ");
         sdf_dump_port_tchk(s_0);
         sdf_dump_port_tchk(s_1);
         sdf_dump_rvalue(v_0);
         break;

      case S_TCHECK_BIDIRSKEW:
         print_syntax("(#BIDIRECTSKEW ");
         sdf_dump_port_tchk(s_0);
         sdf_dump_port_tchk(s_1);
         sdf_dump_rvalue(v_0);
         sdf_dump_rvalue(v_1);
         break;

      case S_TCHECK_WIDTH:
         print_syntax("(#WIDTH ");
         sdf_dump_port_tchk(s_0);
         sdf_dump_rvalue(v_0);
         break;

      case S_TCHECK_PERIOD:
         print_syntax("(#PERIOD ");
         sdf_dump_port_tchk(s_0);
         sdf_dump_rvalue(v_0);
         break;

      case S_TCHECK_NOCHANGE:
         print_syntax("(#NOCHANGE ");
         sdf_dump_port_tchk(s_0);
         sdf_dump_port_tchk(s_1);
         sdf_dump_rvalue(v_0);
         sdf_dump_rvalue(v_1);
         break;
      default:
         fatal_trace("cannot dump timing check subkind: %d", kind);
      }

      print_syntax(")\n");
   }

   tab(indent);
   print_syntax(")\n");
}

static void sdf_dump_te_spec(sdf_node_t cell, int indent)
{
   tab(indent);
   print_syntax("(TIMINGENV\n");

   for (int i = 0; i < sdf_tenvs(cell); i++) {
      sdf_node_t tenv = sdf_tenv(cell, i);
      sdf_tenv_kind_t kind = sdf_subkind(tenv);

      int n_sigs = sdf_signals(tenv);
      int n_vals = sdf_values(tenv);

      sdf_node_t s_0 = (n_sigs > 0) ? sdf_signal(tenv, 0) : NULL;
      sdf_node_t s_1 = (n_sigs > 1) ? sdf_signal(tenv, 1) : NULL;

      switch (kind) {
      case S_TENV_KIND_ARRIVAL:
         print_syntax("(#ARRIVAL ");
         if (n_sigs > 1)
            sdf_dump_port_edge(s_0);
         sdf_dump_port_instance(s_1);
         for (int j = 0; j < 4; j++)
            sdf_dump_rvalue(sdf_value(tenv, i));
         break;

      case S_TENV_KIND_DEPARTURE:
         print_syntax("(#DEPARTURE ");
         if (n_sigs > 1)
            sdf_dump_port_edge(s_0);
         sdf_dump_port_instance(s_1);
         for (int j = 0; j < 4; j++)
            sdf_dump_rvalue(sdf_value(tenv, i));
         break;

      case S_TENV_KIND_SLACK:
         print_syntax("(#SLACK ");
         sdf_dump_port_instance(s_0);
         for (int j = 0; j < 4; j++)
            sdf_dump_rvalue(sdf_value(tenv, i));
         if (n_vals > 4)
            sdf_dump_number(sdf_value(tenv, 4));
         break;

      case S_TENV_KIND_WAVEFORM:
         print_syntax("(#WAVEFORM ");
         sdf_dump_port_instance(s_0);
         sdf_dump_rvalue(sdf_value(tenv, 0));
         print_syntax("\n");
         sdf_dump_edge_list(tenv, indent + 2);
         tab(indent);
         break;

      default:
         fatal_trace("cannot dump timing env subkind: %d", kind);
      }

      print_syntax(")\n");
   }

   tab(indent);
   print_syntax(")\n");
}

static void sdf_dump_timing_spec(sdf_node_t cell, int indent)
{
   int inner_indent = indent + 2;

   if (sdf_delays(cell))
      sdf_dump_del_spec(cell, inner_indent);

   if (sdf_labels(cell))
      sdf_dump_lbl_spec(cell, inner_indent);

   if (sdf_tchecks(cell))
      sdf_dump_tc_spec(cell, inner_indent);

   if (sdf_tenvs(cell))
      sdf_dump_te_spec(cell, inner_indent);
}

static void sdf_dump_cell(sdf_node_t cell, int indent)
{
   assert(sdf_kind(cell) == S_CELL);

   tab(indent);
   print_syntax("(#CELL\n");

   int inner_indent = indent + 2;

   tab(inner_indent);
   print_syntax("(#CELLTYPE %s)\n", istr(sdf_ident(cell)));

   tab(inner_indent);
   print_syntax("(#INSTANCE %s)\n", istr(sdf_ident2(cell)));

   sdf_dump_timing_spec(cell, inner_indent);

   tab(indent);
   print_syntax(")\n");
}

static void sdf_dump_header_item(sdf_node_t s, int indent)
{
   assert(sdf_kind(s) == S_HEADER_ITEM);

   tab(indent);

   unsigned int sub = sdf_subkind(s);
   switch (sub) {
   case S_HEADER_SDF_VERSION:
      print_syntax("(#SDFVERSION ");
      break;
   case S_HEADER_DESIGN:
      print_syntax("(#DESIGN ");
      break;
   case S_HEADER_DATE:
      print_syntax("(#DATE ");
      break;
   case S_HEADER_VENDOR:
      print_syntax("(#VENDOR ");
      break;
   case S_HEADER_PROGRAM:
      print_syntax("(#PROGRAM ");
      break;
   case S_HEADER_VERSION:
      print_syntax("(#VERSION ");
      break;
   case S_HEADER_DIVIDER:
      print_syntax("(#DIVIDER ");
      break;
   case S_HEADER_VOLTAGE:
      print_syntax("(#VOLTAGE ");
      break;
   case S_HEADER_PROCESS:
      print_syntax("(#PROCESS ");
      break;
   case S_HEADER_TEMPERATURE:
      print_syntax("(#TEMPERATURE ");
      break;
   case S_HEADER_TIMESCALE:
      print_syntax("(#TIMESCALE ");
      break;
   default:
      fatal_trace("cannot dump: %d", sub);
   }

   if (sdf_has_number(s)) {
      sdf_node_t num = sdf_number(s);
      sdf_kind_t kind = sdf_kind(num);
      switch (kind) {
      case S_TRIPPLE:
         sdf_dump_tripple(num);
         break;
      case S_NUMBER:
         sdf_dump_number(num);
         break;
      default:
         fatal_trace("cannot dump: %d", kind);
      }
   }

   if (sdf_has_ident(s))
      print_syntax("%s ", istr(sdf_ident(s)));

   print_syntax(")\n");
}

static void sdf_dump_delay_file(sdf_node_t s, int indent)
{
   print_syntax("(#DELAYFILE\n");

   for (int i = 0; i < sdf_decls(s); i++)
      sdf_dump_header_item(sdf_decl(s, i), indent);

   for (int i = 0; i < sdf_cells(s); i++)
      sdf_dump_cell(sdf_cell(s, i), indent);

   print_syntax(")\n");
}

void sdf_dump(sdf_file_t *sdf, int indent)
{
   if (sdf == NULL)
      return;

   sdf_node_t root = sdf->root;
   if (root == NULL)
      return;

   switch (sdf_kind(root)) {
   case S_DELAY_FILE:
      sdf_dump_delay_file(root, indent);
      break;
   default:
      fatal_trace("cannot dump: %s", sdf_kind_str(sdf_kind(root)));
   }
}