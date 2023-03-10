//
//  Copyright (C) 2023  Nick Gasson
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
#include "lower.h"
#include "option.h"
#include "psl/psl-fsm.h"
#include "psl/psl-node.h"
#include "psl/psl-phase.h"
#include "tree.h"
#include "type.h"
#include "vcode.h"

#include <assert.h>

static void psl_wait_cb(tree_t t, void *ctx)
{
   lower_unit_t *lu = ctx;

   vcode_reg_t nets_reg = lower_lvalue(lu, t);
   vcode_reg_t count_reg = emit_const(vtype_offset(), type_width(tree_type(t)));
   emit_sched_event(nets_reg, count_reg);
}

static void psl_lower_clock(lower_unit_t *lu, psl_node_t p)
{
   psl_node_t clk = psl_clock(p);
   tree_t expr = psl_tree(clk);

   build_wait(expr, psl_wait_cb, lu);
}

void psl_lower(lower_unit_t *parent, psl_node_t p, ident_t name)
{
   assert(psl_kind(p) == P_ASSERT);

   psl_fsm_t *fsm = psl_fsm_new(psl_value(p));

   if (opt_get_verbose(OPT_PSL_VERBOSE, istr(name))) {
      char *fname LOCAL = xasprintf("%s.dot", istr(name));
      psl_fsm_dump(fsm, fname);
   }

   vcode_unit_t vu = emit_property(name, psl_to_object(p), get_vcode(parent));
   lower_unit_t *lu = lower_unit_new(parent, vu, NULL, NULL);

   psl_lower_clock(lu, psl_value(p));
   emit_return(VCODE_INVALID_REG);

   lower_unit_free(lu);

   psl_fsm_free(fsm);
}
