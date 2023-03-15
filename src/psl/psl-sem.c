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

#include "util.h"
#include "diag.h"
#include "psl/psl-node.h"

static void psl_check_clock_decl(psl_node_t p)
{

}

static void psl_check_assert(psl_node_t p)
{

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

static void psl_check_hdl_expr(psl_node_t p)
{

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

   case P_HDL_EXPR:
      psl_check_hdl_expr(p);
      break;

   default:
      fatal_trace("cannot check PSL kind %s", psl_kind_str(psl_kind(p)));
   }
}
