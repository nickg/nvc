//
//  Copyright (C) 2022-2023  Nick Gasson
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
#include "diag.h"
#include "psl/psl-node.h"
#include "psl/psl-phase.h"
#include "type.h"

#include <assert.h>

static void psl_check_static(tree_t t)
{
   switch (tree_kind(t)) {
   case T_LITERAL:
      return;
   case T_FCALL:
      if (tree_flags(t) & (TREE_F_LOCALLY_STATIC | TREE_F_GLOBALLY_STATIC))
         return;
      break;
   case T_REF:
      {
         tree_t decl = tree_ref(t);
         if (tree_kind(decl) == T_CONST_DECL && tree_has_value(decl))
            return;
      }
      break;
   default:
      break;
   }

   error_at(tree_loc(t), "expression must be static");
}

static void psl_check_number(tree_t t)
{
   type_t type = tree_type(t);
   type_t std_int = std_type(NULL, STD_INTEGER);

   if (!type_eq(type, std_int)) {
      error_at(tree_loc(t), "expression must be a PSL Number but have type %s",
               type_pp(type));
      return;
   }

   psl_check_static(t);
}

static void psl_check_clock_decl(psl_node_t p)
{

}

static void psl_check_property_decl(psl_node_t p)
{

}

static void psl_check_sequence_decl(psl_node_t p)
{

}

static void psl_check_assert(psl_node_t p)
{
   psl_check(psl_value(p));
}

static void psl_check_assume(psl_node_t p)
{

}

static void psl_check_restrict(psl_node_t p)
{

}

static void psl_check_fairness(psl_node_t p)
{

}

static void psl_check_cover(psl_node_t p)
{

}

static void psl_check_always(psl_node_t p)
{

}

static void psl_check_never(psl_node_t p)
{

}

static void psl_check_eventually(psl_node_t p)
{

}

static void psl_check_hdl_expr(psl_node_t p)
{

}

static void psl_check_property_inst(psl_node_t p)
{

}

static void psl_check_sequence_inst(psl_node_t p)
{

}

static void psl_check_sere(psl_node_t p)
{

}

static void psl_check_implication(psl_node_t p)
{
   assert(psl_operands(p) == 2);

   psl_node_t left = psl_operand(p, 0);
   psl_check(left);

   psl_node_t right = psl_operand(p, 1);
   psl_check(right);
}

static void psl_check_next(psl_node_t p)
{
   if (psl_has_delay(p))
      psl_check_number(psl_delay(p));

   psl_check(psl_value(p));
}

static void psl_check_until(psl_node_t p)
{
   assert(psl_operands(p) == 2);

   psl_node_t left = psl_operand(p, 0);
   psl_check(left);

   psl_node_t right = psl_operand(p, 1);
   psl_check(right);
}

void psl_check(psl_node_t p)
{
   switch (psl_kind(p)) {
   case P_ALWAYS:
      psl_check_always(p);
      break;
   case P_ASSERT:
      psl_check_assert(p);
      break;
   case P_ASSUME:
      psl_check_assume(p);
      break;
   case P_RESTRICT:
      psl_check_restrict(p);
      break;
   case P_FAIRNESS:
      psl_check_fairness(p);
      break;
   case P_COVER:
      psl_check_cover(p);
      break;
   case P_CLOCK_DECL:
      psl_check_clock_decl(p);
      break;
   case P_PROPERTY_DECL:
      psl_check_property_decl(p);
      break;
   case P_SEQUENCE_DECL:
      psl_check_sequence_decl(p);
      break;
   case P_HDL_EXPR:
      psl_check_hdl_expr(p);
      break;
   case P_PROPERTY_INST:
      psl_check_property_inst(p);
      break;
   case P_SEQUENCE_INST:
      psl_check_sequence_inst(p);
      break;
   case P_NEVER:
      psl_check_never(p);
      break;
   case P_EVENTUALLY:
      psl_check_eventually(p);
      break;
   case P_SERE:
      psl_check_sere(p);
      break;
   case P_IMPLICATION:
      psl_check_implication(p);
      break;
   case P_NEXT:
      psl_check_next(p);
      break;
   case P_UNTIL:
      psl_check_until(p);
      break;
   default:
      fatal_trace("cannot check PSL kind %s", psl_kind_str(psl_kind(p)));
   }
}
