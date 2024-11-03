//
//  Copyright (C) 2022-2024  Nick Gasson
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
#include "names.h"
#include "phase.h"
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
   case T_RANGE:
      {
         const range_kind_t kind = tree_subkind(t);
         if (kind == RANGE_TO || kind == RANGE_DOWNTO) {
            psl_check_static(tree_left(t));
            psl_check_static(tree_right(t));
            return;
         }
      }
      break;

   default:
      break;
   }

   error_at(tree_loc(t), "expression must be static");
}

static void psl_check_number(tree_t t, nametab_t *tab)
{
   type_t std_int = std_type(NULL, STD_INTEGER);
   type_t type = solve_types(tab, t, std_int);

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

static void psl_check_sequence_decl(psl_node_t p, nametab_t *tab)
{
   psl_check(psl_value(p), tab);
}

static void psl_check_assert(psl_node_t p, nametab_t *tab)
{
   psl_check(psl_value(p), tab);
}

static void psl_check_assume(psl_node_t p, nametab_t *tab)
{
   psl_check(psl_value(p), tab);
}

static void psl_check_restrict(psl_node_t p, nametab_t *tab)
{
   psl_check(psl_value(p), tab);
}

static void psl_check_fairness(psl_node_t p)
{

}

static void psl_check_cover(psl_node_t p, nametab_t *tab)
{
   psl_check(psl_value(p), tab);
}

static void psl_check_always(psl_node_t p, nametab_t *tab)
{
   psl_check(psl_value(p), tab);
}

static void psl_check_never(psl_node_t p, nametab_t *tab)
{
   psl_node_t value = psl_value(p);
   psl_check(value, tab);

   const psl_kind_t kind = psl_kind(value);
   if (kind != P_HDL_EXPR && kind != P_SERE)
      error_at(psl_loc(p), "property is not in the simple subset as the "
               "operand of never is not a Boolean or Sequence");
}

static void psl_check_eventually(psl_node_t p, nametab_t *tab)
{
   psl_node_t value = psl_value(p);
   psl_check(value, tab);

   const psl_kind_t kind = psl_kind(value);
   if (kind != P_HDL_EXPR && kind != P_SERE)
      error_at(psl_loc(p), "property is not in the simple subset as the "
               "operand of eventually! is not a Boolean or Sequence");
}

static void psl_check_hdl_expr(psl_node_t p, nametab_t *tab)
{
   tree_t value = psl_tree(p);

   type_t type = solve_psl_condition(tab, &value);
   if (type_is_none(type))
      return;

   psl_set_tree(p, value);   // May be replaced with condition conversion

   assert(psl_type(p) == PSL_TYPE_BOOLEAN);

   if (!sem_check(value, tab))
      return;

   type_t std_bool = std_type(NULL, STD_BOOLEAN);
   bool ok = type_eq(type, std_bool);

   if (!ok && standard() < STD_08) {
      // Later standards use condition conversion operator
      type_t std_bit = std_type(NULL, STD_BIT);
      type_t ulogic = ieee_type(IEEE_STD_ULOGIC);

      ok = type_eq(type, std_bit) || type_eq(type, ulogic);
   }

   if (!ok)
      error_at(tree_loc(value), "expression must be a PSL Boolean but "
               "have type %s", type_pp(type));
}

static void psl_check_property_inst(psl_node_t p)
{

}

static void psl_check_sequence_inst(psl_node_t p)
{

}

static void psl_check_sere(psl_node_t p, nametab_t *tab)
{
   const int nops = psl_operands(p);
   for (int i = 0; i < nops; i++)
      psl_check(psl_operand(p, i), tab);
}

static void psl_check_logical(psl_node_t p, nametab_t *tab)
{
   assert(psl_operands(p) == 2);

   psl_node_t left = psl_operand(p, 0);
   psl_check(left, tab);

   psl_node_t right = psl_operand(p, 1);
   psl_check(right, tab);

   switch (psl_subkind(p)) {
   case PSL_LOGIC_IFF:
      if (psl_kind(right) != P_HDL_EXPR)
         error_at(psl_loc(p), "property is not in the simple subset as the "
                  "right hand side of this implication is non-Boolean");
      // Fall-through
   case PSL_LOGIC_IF:
      if (psl_kind(left) != P_HDL_EXPR)
         error_at(psl_loc(p), "property is not in the simple subset as the "
                  "left hand side of this implication is non-Boolean");
      break;

   case PSL_LOGIC_OR:
      if (psl_kind(left) != P_HDL_EXPR && psl_kind(right) != P_HDL_EXPR)
         error_at(psl_loc(p), "property is not in the simple subset as both "
                  "operands of this logical OR are non-Boolean");
      break;

   case PSL_LOGIC_AND:
      break;
   }
}

static void psl_check_next(psl_node_t p, nametab_t *tab)
{
   if (psl_has_delay(p))
      psl_check_number(psl_delay(p), tab);

   psl_check(psl_value(p), tab);
}

static void psl_check_next_e(psl_node_t p, nametab_t *tab)
{
   if (psl_has_delay(p))
      psl_check_number(psl_delay(p), tab);

   psl_node_t value = psl_value(p);
   psl_check(value, tab);

   if (psl_kind(value) != P_HDL_EXPR)
      error_at(psl_loc(p), "property is not in the simple subset as the "
               "operand of this next_e operator is non-Boolean");
}

static void psl_check_until(psl_node_t p, nametab_t *tab)
{
   assert(psl_operands(p) == 2);

   psl_node_t left = psl_operand(p, 0);
   psl_check(left, tab);

   psl_node_t right = psl_operand(p, 1);
   psl_check(right, tab);

   const psl_flags_t flags = psl_flags(p);
   if ((flags & PSL_F_INCLUSIVE) && psl_kind(right) != P_HDL_EXPR)
      error_at(psl_loc(p), "property is not in the simple subset as the right "
               "hand side of this overlapping until operator is non-Boolean");
   else if (psl_kind(left) != P_HDL_EXPR)
      error_at(psl_loc(p), "property is not in the simple subset as the left "
               "hand side of this until operator is non-Boolean");
}

static void psl_check_before(psl_node_t p, nametab_t *tab)
{
   assert(psl_operands(p) == 2);

   psl_node_t left = psl_operand(p, 0);
   psl_check(left, tab);

   psl_node_t right = psl_operand(p, 1);
   psl_check(right, tab);

   if (psl_kind(left) != P_HDL_EXPR)
      error_at(psl_loc(p), "property is not in the simple subset as the left "
               "hand side of this before operator is non-Boolean");
   else if (psl_kind(right) != P_HDL_EXPR)
      error_at(psl_loc(p), "property is not in the simple subset as the right "
               "hand side of this before operator is non-Boolean");
}

static void psl_check_abort(psl_node_t p, nametab_t *tab)
{
   assert(psl_operands(p) == 2);

   psl_node_t left = psl_operand(p, 0);
   psl_check(left, tab);

   psl_node_t right = psl_operand(p, 1);
   psl_check(right, tab);
}

static void psl_check_suffix_impl(psl_node_t p, nametab_t *tab)
{
   assert(psl_operands(p) == 2);

   psl_node_t left = psl_operand(p, 0);
   psl_check(left, tab);

   const psl_kind_t lkind = psl_kind(left);
   if (lkind != P_HDL_EXPR && lkind != P_SERE)
      error_at(psl_loc(left), "left hand side of suffix implication operator "
               "must be a Sequence");

   psl_node_t right = psl_operand(p, 1);
   psl_check(right, tab);
}

void psl_check(psl_node_t p, nametab_t *tab)
{
   switch (psl_kind(p)) {
   case P_ALWAYS:
      psl_check_always(p, tab);
      break;
   case P_ASSERT:
      psl_check_assert(p, tab);
      break;
   case P_ASSUME:
      psl_check_assume(p, tab);
      break;
   case P_RESTRICT:
      psl_check_restrict(p, tab);
      break;
   case P_FAIRNESS:
      psl_check_fairness(p);
      break;
   case P_COVER:
      psl_check_cover(p, tab);
      break;
   case P_CLOCK_DECL:
      psl_check_clock_decl(p);
      break;
   case P_PROPERTY_DECL:
      psl_check_property_decl(p);
      break;
   case P_SEQUENCE_DECL:
      psl_check_sequence_decl(p, tab);
      break;
   case P_HDL_EXPR:
      psl_check_hdl_expr(p, tab);
      break;
   case P_PROPERTY_INST:
      psl_check_property_inst(p);
      break;
   case P_SEQUENCE_INST:
      psl_check_sequence_inst(p);
      break;
   case P_NEVER:
      psl_check_never(p, tab);
      break;
   case P_EVENTUALLY:
      psl_check_eventually(p, tab);
      break;
   case P_SERE:
      psl_check_sere(p, tab);
      break;
   case P_LOGICAL:
      psl_check_logical(p, tab);
      break;
   case P_NEXT:
   case P_NEXT_A:
   case P_NEXT_EVENT:
      psl_check_next(p, tab);
      break;
   case P_NEXT_E:
      psl_check_next_e(p, tab);
      break;
   case P_UNTIL:
      psl_check_until(p, tab);
      break;
   case P_BEFORE:
      psl_check_before(p, tab);
      break;
   case P_ABORT:
      psl_check_abort(p, tab);
      break;
   case P_SUFFIX_IMPL:
      psl_check_suffix_impl(p, tab);
      break;
   default:
      fatal_trace("cannot check PSL kind %s", psl_kind_str(psl_kind(p)));
   }
}
