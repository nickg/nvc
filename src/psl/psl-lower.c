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
#include "lib.h"
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

static vcode_reg_t psl_lower_boolean(lower_unit_t *lu, psl_node_t p)
{
   assert(psl_kind(p) == P_HDL_EXPR);
   vcode_reg_t test_reg = lower_rvalue(lu, psl_tree(p));

   if (standard() >= STD_08)
      return test_reg;  // Always converted to boolean by ?? operator
   else if (!vtype_eq(vcode_reg_type(test_reg), vtype_bool())) {
      vcode_type_t std_logic = vtype_int(0, 8);
      vcode_reg_t one_reg = emit_const(std_logic, 3);
      return emit_cmp(VCODE_CMP_EQ, test_reg, one_reg);
   }
   else
      return test_reg;
}

static vcode_reg_t psl_debug_locus(psl_node_t p)
{
   ident_t unit;
   ptrdiff_t offset;
   psl_locus(p, &unit, &offset);

   return emit_debug_locus(unit, offset);
}

static void psl_lower_state(lower_unit_t *lu, fsm_state_t *state)
{
   emit_comment("Property state %d", state->id);

   vcode_type_t vint32 = vtype_int(INT32_MIN, INT32_MAX);

   if (state->test != NULL) {
      vcode_reg_t test_reg = psl_lower_boolean(lu, state->test);
      vcode_reg_t severity_reg = emit_const(vtype_int(0, 3), 1);

      vcode_reg_t locus = psl_debug_locus(state->test);
      emit_assert(test_reg, VCODE_INVALID_REG, VCODE_INVALID_REG, severity_reg,
                  locus, VCODE_INVALID_REG, VCODE_INVALID_REG);
   }

   if (state->initial)
      emit_enter_state(emit_const(vint32, state->id));

   for (fsm_edge_t *e = state->edges; e; e = e->next) {
      vcode_block_t enter_bb = emit_block();
      vcode_block_t skip_bb = emit_block();

      vcode_reg_t guard_reg = psl_lower_boolean(lu, e->guard);
      emit_cond(guard_reg, enter_bb, skip_bb);

      vcode_select_block(enter_bb);

      emit_enter_state(emit_const(vint32, e->dest->id));
      emit_jump(skip_bb);

      vcode_select_block(skip_bb);
   }

   emit_return(VCODE_INVALID_REG);
}

void psl_lower(lower_unit_t *parent, psl_node_t p, ident_t label)
{
   assert(psl_kind(p) == P_ASSERT);

   psl_fsm_t *fsm = psl_fsm_new(psl_value(p));

   if (opt_get_verbose(OPT_PSL_VERBOSE, istr(label))) {
      char *fname LOCAL = xasprintf("%s.dot", istr(label));
      psl_fsm_dump(fsm, fname);
   }

   vcode_unit_t context = parent ? get_vcode(parent) : NULL;
   vcode_select_unit(context);

   ident_t prefix = parent ? vcode_unit_name() : lib_name(lib_work());
   ident_t name = ident_prefix(prefix, label, '.');

   vcode_unit_t vu = emit_property(name, psl_to_object(p), context);
   lower_unit_t *lu = lower_unit_new(NULL, parent, vu, NULL, NULL);

   vcode_type_t vcontext = vtype_context(prefix);
   emit_param(vcontext, vcontext, ident_new("context"));

   vcode_type_t vint32 = vtype_int(INT32_MIN, INT32_MAX);
   vcode_reg_t state_reg = emit_param(vint32, vint32, ident_new("state"));

   vcode_block_t reset_bb = emit_block();
   vcode_block_t clock_bb = emit_block();
   vcode_block_t skip_bb = emit_block();
   vcode_block_t case_bb = emit_block();
   vcode_block_t abort_bb = emit_block();

   vcode_reg_t zero_reg = emit_const(vint32, 0);
   vcode_reg_t cmp_reg = emit_cmp(VCODE_CMP_LT, state_reg, zero_reg);
   emit_cond(cmp_reg, reset_bb, clock_bb);

   // Only handle a single clock for the whole property
   psl_node_t clk = psl_clock(psl_value(p));
   tree_t clk_expr = psl_tree(clk);

   vcode_select_block(reset_bb);

   emit_comment("Reset property");

   build_wait(clk_expr, psl_wait_cb, lu);

   emit_return(emit_const(vint32, fsm->next_id));

   vcode_select_block(clock_bb);

   emit_comment("Test clock expression");

   vcode_reg_t clk_reg = lower_rvalue(lu, clk_expr);
   emit_cond(clk_reg, case_bb, skip_bb);

   vcode_select_block(skip_bb);

   emit_enter_state(state_reg);
   emit_return(VCODE_INVALID_REG);

   vcode_select_block(case_bb);

   vcode_block_t *state_bb LOCAL =
      xmalloc_array(fsm->next_id, sizeof(vcode_block_t));
   vcode_reg_t *state_ids LOCAL =
      xmalloc_array(fsm->next_id, sizeof(vcode_reg_t));

   for (int i = 0; i < fsm->next_id; i++) {
      state_bb[i] = emit_block();
      state_ids[i] = emit_const(vint32, i);
   }

   emit_case(state_reg, abort_bb, state_ids, state_bb, fsm->next_id);

   vcode_select_block(abort_bb);

   emit_unreachable(VCODE_INVALID_REG);

   int pos = 0;
   for (fsm_state_t *s = fsm->states; s; s = s->next) {
      vcode_select_block(state_bb[pos++]);
      psl_lower_state(lu, s);
   }
   assert(pos == fsm->next_id);

   lower_unit_free(lu);

   psl_fsm_free(fsm);
}
