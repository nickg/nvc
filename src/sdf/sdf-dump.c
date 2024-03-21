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


static void tab(int indent)
{
   print_syntax("%*s", indent, "");
}

void sdf_dump_number(sdf_node_t number)
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

void sdf_dump_port(sdf_node_t port)
{
   sdf_flags_t flags = sdf_flags(port);
   bool port_edge = !!((flags & S_F_POSEDGE) || (flags & S_F_NEGEDGE));

   if (port_edge) {
      print_syntax("(");
      if (flags & S_F_POSEDGE)
         print_syntax("posedge ");
      else
         print_syntax("negedge ");

      print_syntax("%s", istr(sdf_ident(port)));
   }
   else
      print_syntax("%s", istr(sdf_ident(port)));

   int ndims = sdf_dims(port);

   if (ndims) {
      print_syntax("[");

      sdf_dump_number(sdf_dim(port, 0));

      if (ndims > 1) {
         print_syntax(":");
         sdf_dump_number(sdf_dim(port, 1));
      }

      print_syntax("]");
   }

   if (port_edge)
      print_syntax(")");

   print_syntax(" ");
}

void sdf_dump_tripple(sdf_node_t tripple)
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

void sdf_dump_rvalue(sdf_node_t rvalue)
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

void sdf_dump_delval(sdf_node_t delval)
{
   int nrvalues = sdf_values(delval);

   if (nrvalues)
      print_syntax("(");

   for (int j = 0; j < nrvalues; j++)
      sdf_dump_rvalue(sdf_value(delval, j));

   if (nrvalues)
      print_syntax(") ");
}

void sdf_dump_value_list(sdf_node_t s)
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

void sdf_dump_expr(sdf_node_t expr)
{
   sdf_kind_t kind = sdf_kind(expr);
   switch (kind) {
   case S_PORT:
      sdf_dump_port(expr);
      break;
   default:
      fatal_trace("cannot dump expression kind: %s", sdf_kind_str(kind));
   }
}

void sdf_dump_delay(sdf_node_t delay, int indent)
{
   sdf_delay_kind_t kind = sdf_subkind(delay);

   int used_indent = indent;
   int nconds = sdf_conds(delay);

   if (nconds) {
      tab(indent);
      sdf_node_t cond = sdf_cond(delay, 0);

      if (sdf_has_expr(cond)) {
         print_syntax("(#COND ");
         sdf_dump_expr(sdf_expr(cond));
         print_syntax("\n");
      } else
         print_syntax("(#CONDELSE \n");

      used_indent += 2;
   }

   tab(used_indent);
   print_syntax("(");

   switch (kind) {
   case S_DELAY_KIND_IOPATH:
      print_syntax("#IOPATH ");
      break;
   case S_DELAY_KIND_PORT:
      print_syntax("#PORT ");
      break;
   case S_DELAY_KIND_INTERCONNECT:
      print_syntax("#INTERCONNECT ");
      break;
   case S_DELAY_KIND_NETDELAY:
      print_syntax("#NETDELAY ");
      break;
   case S_DELAY_KIND_DEVICE:
      print_syntax("#DEVICE ");
      break;
   default:
      fatal_trace("cannot dump delay subkind: %d", kind);
      break;
   }

   for (int i = 0; i < sdf_ports(delay); i++)
      sdf_dump_port(sdf_port(delay, i));

   sdf_dump_value_list(delay);
   print_syntax(")\n");

   if (nconds) {
      tab(indent);
      print_syntax(")\n");
   }
}

void sdf_dump_label(sdf_node_t label, int indent)
{
   tab(indent);
   print_syntax("(%s ", istr(sdf_ident(label)));
   sdf_dump_value_list(label);
   print_syntax(")\n");
}

void sdf_dump_cell(sdf_node_t cell, int indent)
{
   assert(sdf_kind(cell) == S_CELL);

   tab(indent);
   print_syntax("(#CELL\n");

   int inner_indent = indent + 2;

   tab(inner_indent);
   print_syntax("(#CELLTYPE %s)\n", istr(sdf_ident(cell)));

   tab(inner_indent);
   print_syntax("(#INSTANCE %s)\n", istr(sdf_ident2(cell)));

   // Delays and Labels are held flat, and delay_kind is only a flag. Dump with
   // standard compliant syntax at expense of looping through delays/cells
   // multiple times.
   value_flag_pair_t flag_pairs[2] = {
      {.flag = S_F_VALUE_ABSOLUTE,   .flag_str = "ABSOLUTE"},
      {.flag = S_F_VALUE_INCREMENT,  .flag_str = "INCREMENT"}
   };

   int delays = sdf_delays(cell);

   if (delays) {
      tab(inner_indent);
      print_syntax("(#DELAY\n");

      // Dump ABSOLUTE/INCREMENT delays
      for (int i = 0; i < 2; i++) {
         bool exist = false;
         for (int j = 0; j < sdf_delays(cell); j++) {
            sdf_node_t delay = sdf_delay(cell, j);
            sdf_flags_t flags = sdf_flags(delay);

            if (flags & flag_pairs[i].flag) {
               if (!exist) {
                  tab(inner_indent + 2);
                  print_syntax("(#%s\n", flag_pairs[i].flag_str);
                  exist = true;
               }
               sdf_dump_delay(delay, inner_indent + 4);
            }
         }

         if (exist) {
            tab(inner_indent + 2);
            print_syntax(")\n");
         }
      }

      // Dump PATHPULSE/PATHPULSEPERCENT delays -> No deltype flag
      for (int i = 0; i < sdf_delays(cell); i++) {
         sdf_node_t delay = sdf_delay(cell, i);
         sdf_delay_kind_t kind = sdf_subkind(delay);

         if (kind == S_DELAY_KIND_PATHPULSE || kind == S_DELAY_KIND_PATHPULSEP)
            sdf_dump_delay(sdf_delay(cell, i), inner_indent + 2);
      }

      tab(inner_indent);
      print_syntax(")\n");
   }

   int labels = sdf_labels(cell);

   if (labels) {

      tab(inner_indent);
      print_syntax("(#LABEL\n");

      // Dump ABSOLUTE/INCREMENT labels
      for (int i = 0; i < 2; i++) {
         bool exist = false;
         for (int j = 0; j < sdf_labels(cell); j++) {
            sdf_node_t label = sdf_label(cell, j);
            sdf_flags_t flags = sdf_flags(label);

            if (flags & flag_pairs[i].flag) {
               if (!exist) {
                  tab(inner_indent + 2);
                  print_syntax("(#%s\n", flag_pairs[i].flag_str);
                  exist = true;
               }
               sdf_dump_label(label, inner_indent + 4);
            }
         }

         if (exist) {
            tab(inner_indent + 2);
            print_syntax(")\n");
         }
      }

      tab(inner_indent);
      print_syntax(")\n");
   }

   tab(indent);
   print_syntax(")\n");
}

void sdf_dump_header_item(sdf_node_t s, int indent)
{
   assert(sdf_kind(s) == S_HEADER_ITEM);

   tab(indent);

   unsigned int sub = sdf_subkind(s);
   switch (sub) {
   case S_HEADER_SDF_VESION:
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

void sdf_dump_delay_file(sdf_node_t s, int indent)
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