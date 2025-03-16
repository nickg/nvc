//
//  Copyright (C) 2014-2025  Nick Gasson
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
#include "mir/mir-node.h"
#include "mir/mir-unit.h"
#include "tree.h"
#include "type.h"
#include "vhdl/vhdl-lower.h"
#include "vhdl/vhdl-priv.h"

#include <stdlib.h>
#include <assert.h>

static void predef_bit_shift(mir_unit_t *mu, tree_t decl,
                             subprogram_kind_t kind)
{
   type_t type = tree_type(tree_port(decl, 0));

   const type_info_t *elem = type_info(mu, type_elem(type));
   mir_type_t t_offset = mir_offset_type(mu);
   mir_type_t t_bool = mir_bool_type(mu);

   mir_value_t input = mir_get_param(mu, 1);
   mir_value_t shift = mir_get_param(mu, 2);

   mir_value_t data = mir_build_unwrap(mu, input);
   mir_value_t len  = mir_build_uarray_len(mu, input, 0);

   mir_block_t null_bb = mir_add_block(mu);
   mir_block_t non_null_bb = mir_add_block(mu);

   mir_value_t zero = mir_const(mu, t_offset, 0);
   mir_value_t one = mir_const(mu, t_offset, 1);
   mir_value_t is_null = mir_build_cmp(mu, MIR_CMP_EQ, len, zero);
   mir_build_cond(mu, is_null, null_bb, non_null_bb);

   mir_set_cursor(mu, null_bb, MIR_APPEND);

   mir_build_return(mu, input);

   mir_set_cursor(mu, non_null_bb, MIR_APPEND);

   mir_value_t mem = mir_build_alloc(mu, elem->type, elem->stamp, len);
   mir_value_t i_var = mir_add_var(mu, t_offset, MIR_NULL_STAMP,
                                   ident_new("i"), MIR_VAR_TEMP);
   mir_build_store(mu, i_var, zero);

   mir_block_t cmp_bb  = mir_add_block(mu);
   mir_block_t body_bb = mir_add_block(mu);
   mir_block_t exit_bb = mir_add_block(mu);

   mir_value_t def_val = MIR_NULL_VALUE;
   switch (kind) {
   case S_SLL: case S_SRL: case S_ROL: case S_ROR:
      def_val = zero;
      break;
   case S_SRA:
      {
         mir_value_t len_minus_1 = mir_build_sub(mu, t_offset, len, one);
         mir_value_t last_ptr = mir_build_array_ref(mu, data, len_minus_1);
         def_val = mir_build_load(mu, last_ptr);
      }
      break;
   case S_SLA:
      def_val = mir_build_load(mu, data);
      break;
   default:
      should_not_reach_here();
   }

   mir_value_t shift_cast = mir_build_cast(mu, t_offset, shift);
   mir_value_t shift_is_neg = mir_build_cmp(mu, MIR_CMP_LT, shift_cast, zero);
   mir_build_jump(mu, cmp_bb);

   mir_set_cursor(mu, cmp_bb, MIR_APPEND);

   mir_value_t i_val = mir_build_load(mu, i_var);
   mir_value_t eq = mir_build_cmp(mu, MIR_CMP_EQ, i_val, len);
   mir_build_cond(mu, eq, exit_bb, body_bb);

   mir_set_cursor(mu, body_bb, MIR_APPEND);

   mir_value_t cmp = MIR_NULL_VALUE;
   switch (kind) {
   case S_SRL: case S_SRA:
      {
         mir_value_t len_plus_shift =
            mir_build_add(mu, t_offset, len, shift_cast);
         mir_value_t neg = mir_build_cmp(mu, MIR_CMP_LT, i_val, len_plus_shift);
         mir_value_t pos = mir_build_cmp(mu, MIR_CMP_GEQ, i_val, shift_cast);
         cmp = mir_build_select(mu, t_bool, shift_is_neg, neg, pos);
      }
      break;
   case S_SLL: case S_SLA:
      {
         mir_value_t neg_shift = mir_build_neg(mu, t_offset, shift_cast);
         mir_value_t len_minus_shift =
            mir_build_sub(mu, t_offset, len, shift_cast);
         mir_value_t neg = mir_build_cmp(mu, MIR_CMP_GEQ, i_val, neg_shift);
         mir_value_t pos =
            mir_build_cmp(mu, MIR_CMP_LT, i_val, len_minus_shift);
         cmp = mir_build_select(mu, t_bool, shift_is_neg, neg, pos);
      }
      break;
   case S_ROL: case S_ROR:
      cmp = mir_const(mu, t_bool, 1);
      break;
   default:
      should_not_reach_here();
   }

   mir_value_t dst_ptr = mir_build_array_ref(mu, mem, i_val);

   mir_value_t next = mir_build_add(mu, t_offset, i_val, one);
   mir_build_store(mu, i_var, next);

   mir_block_t true_bb = mir_add_block(mu);
   mir_block_t false_bb = mir_add_block(mu);

   mir_build_cond(mu, cmp, true_bb, false_bb);

   mir_set_cursor(mu, true_bb, MIR_APPEND);

   mir_value_t src_idx = MIR_NULL_VALUE;
   switch (kind) {
   case S_SLL: case S_SLA:
      src_idx = mir_build_add(mu, t_offset, i_val, shift_cast);
      break;
   case S_SRL: case S_SRA:
      src_idx = mir_build_sub(mu, t_offset, i_val, shift_cast);
      break;
   case S_ROL:
      {
         mir_value_t len_plus_shift =
            mir_build_add(mu, t_offset, len, shift_cast);
         mir_value_t idx = mir_build_add(mu, t_offset, i_val, len_plus_shift);
         src_idx = mir_build_mod(mu, t_offset, idx, len);
      }
      break;
   case S_ROR:
      {
         mir_value_t len_minus_shift =
            mir_build_sub(mu, t_offset, len, shift_cast);
         mir_value_t idx = mir_build_add(mu, t_offset, i_val, len_minus_shift);
         src_idx = mir_build_mod(mu, t_offset, idx, len);
      }
      break;
   default:
      should_not_reach_here();
   }

   mir_value_t src_ptr = mir_build_array_ref(mu, data, src_idx);
   mir_value_t src_val = mir_build_load(mu, src_ptr);
   mir_build_store(mu, dst_ptr, src_val);
   mir_build_jump(mu, cmp_bb);

   mir_set_cursor(mu, false_bb, MIR_APPEND);

   mir_build_store(mu, dst_ptr, def_val);
   mir_build_jump(mu, cmp_bb);

   mir_set_cursor(mu, exit_bb, MIR_APPEND);

   mir_value_t left  = mir_build_uarray_left(mu, input, 0);
   mir_value_t right = mir_build_uarray_right(mu, input, 0);
   mir_value_t dir   = mir_build_uarray_dir(mu, input, 0);

   mir_dim_t dims[] = { { left, right, dir } };
   mir_build_return(mu, mir_build_wrap(mu, mem, dims, 1));
}

void vhdl_lower_predef(mir_unit_t *mu, object_t *obj)
{
   tree_t decl = tree_from_object(obj);

   // XXX: should not have null parent
   mir_type_t t_parent = mir_context_type(mu, ident_new("dummy"));
   mir_add_param(mu, t_parent, MIR_NULL_STAMP, ident_new("context"));

   const int nports = tree_ports(decl);
   for (int i = 0; i < nports; i++) {
      tree_t p = tree_port(decl, i);
      assert(tree_subkind(p) == PORT_IN);

      const type_info_t *ti = type_info(mu, tree_type(p));
      mir_add_param(mu, ti->type, ti->stamp, tree_ident(p));
   }

   const type_info_t *ti = type_info(mu, type_result(tree_type(decl)));
   mir_set_result(mu, ti->type);

   const subprogram_kind_t kind = tree_subkind(decl);
   assert(kind != S_USER);
   assert(!is_open_coded_builtin(kind));

   switch (kind) {
   case S_SLL:
   case S_SRL:
   case S_SLA:
   case S_SRA:
   case S_ROL:
   case S_ROR:
      predef_bit_shift(mu, decl, kind);
      break;
   default:
      should_not_reach_here();
   }
}
