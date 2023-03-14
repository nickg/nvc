//
//  Copyright (C) 2023 Nick Gasson
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
#include "phase.h"
#include "psl/psl-node.h"
#include "psl/psl-phase.h"

#include <ctype.h>

static void psl_dump_assert(psl_node_t p)
{
   print_syntax("#assert ");
   psl_dump(psl_value(p));
}

static void psl_dump_assume(psl_node_t p)
{
   if (psl_subkind(p) == PSL_GUARANTEE)
      print_syntax("#assume guarantee");
   else
      print_syntax("#assume");
   psl_dump(psl_value(p));
}

static void psl_dump_restrict(psl_node_t p)
{
   if (psl_subkind(p) == PSL_GUARANTEE)
      print_syntax("#restrict guarantee");
   else
      print_syntax("#restrict");
   psl_dump(psl_value(p));
}

static void psl_dump_fairness(psl_node_t p)
{
   if (psl_subkind(p) == PSL_STRONG)
      print_syntax("#strong fairness");
   else
      print_syntax("#fairness");

   for (int i = 0; i < psl_operands(p); i++)
      psl_dump(psl_operand(p, i));
}

static void psl_dump_cover(psl_node_t p)
{
   print_syntax("#cover");
   psl_dump(psl_value(p));
}

static void psl_dump_always(psl_node_t p)
{
   print_syntax("#always ");
   psl_dump(psl_value(p));
}

static void psl_dump_never(psl_node_t p)
{
   print_syntax("#never ");
   psl_dump(psl_value(p));
}

static void psl_dump_implication(psl_node_t p)
{
   psl_dump(psl_operand(p, 0));
   print_syntax(" -> (");
   psl_dump(psl_operand(p, 1));
   print_syntax(")");
}

static void psl_dump_next(psl_node_t p)
{
   print_syntax("#next ");
   psl_dump(psl_value(p));
}

static void psl_dump_next_a(psl_node_t p)
{
   print_syntax("#next_a ");
   psl_dump(psl_value(p));
}

static void psl_dump_next_event(psl_node_t p)
{
   print_syntax("#next_event (");
   psl_dump(psl_value(p));
   print_syntax(")");
}

static void psl_dump_sere(psl_node_t p)
{
   print_syntax("{");

   const int n = psl_operands(p);
   for (int i = 0; i < n; i++) {
      if (i > 0) print_syntax("; ");
      psl_dump(psl_operand(p, i));
   }

   print_syntax("}");
}

static void psl_dump_clock_decl(psl_node_t p)
{
   print_syntax("#default #clock #is ");
   vhdl_dump(psl_tree(p), 0);
}

void psl_dump(psl_node_t p)
{
   switch (psl_kind(p)) {
   case P_ASSERT:
      psl_dump_assert(p);
      break;
   case P_ASSUME:
      psl_dump_assume(p);
      break;
   case P_RESTRICT:
      psl_dump_restrict(p);
      break;
   case P_FAIRNESS:
      psl_dump_fairness(p);
      break;
   case P_COVER:
      psl_dump_cover(p);
      break;
   case P_ALWAYS:
      psl_dump_always(p);
      break;
   case P_NEVER:
      psl_dump_never(p);
      break;
   case P_HDL_EXPR:
      vhdl_dump(psl_tree(p), 0);
      break;
   case P_IMPLICATION:
      psl_dump_implication(p);
      break;
   case P_NEXT:
      psl_dump_next(p);
      break;
   case P_NEXT_A:
      psl_dump_next_a(p);
      break;
   case P_NEXT_EVENT:
      psl_dump_next_event(p);
      break;
   case P_CLOCK_DECL:
      psl_dump_clock_decl(p);
      break;
   case P_SERE:
      psl_dump_sere(p);
      break;
   default:
      print_syntax("\n");
      fflush(stdout);
      fatal_trace("cannot dump %s", psl_kind_str(psl_kind(p)));
   }
}
