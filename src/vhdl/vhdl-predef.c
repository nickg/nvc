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
#include "rt/assert.h"
#include "tree.h"
#include "type.h"
#include "vhdl/vhdl-lower.h"
#include "vhdl/vhdl-priv.h"

#include <stdlib.h>
#include <assert.h>

typedef enum {
   _U = 0x0,
   _X = 0x1,
   _0 = 0x2,
   _1 = 0x3,
   _Z = 0x4,
   _W = 0x5,
   _L = 0x6,
   _H = 0x7,
   _D = 0x8
} std_ulogic_t;

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

static void predef_match_op(mir_unit_t *mu, tree_t decl, subprogram_kind_t kind)
{
   type_t type = tree_type(tree_port(decl, 0));

   mir_cmp_t cmp;
   bool invert = false;
   switch (kind) {
   case S_MATCH_NEQ:
      invert = true;
   case S_MATCH_EQ:
      cmp = MIR_CMP_EQ;
      break;
   case S_MATCH_GE:
      invert = true;
   case S_MATCH_LT:
      cmp = MIR_CMP_LT;
      break;
   case S_MATCH_GT:
      invert = true;
   case S_MATCH_LE:
      cmp = MIR_CMP_LEQ;
      break;
   default:
      should_not_reach_here();
   }

   bool is_array = false, is_bit = false;
   if (type_is_array(type)) {
      is_array = true;
      is_bit = type_ident(type_elem(type)) == well_known(W_STD_BIT);
   }
   else
      is_bit = type_ident(type) == well_known(W_STD_BIT);

   mir_value_t left = mir_get_param(mu, 1);
   mir_value_t right = mir_get_param(mu, 2);

   mir_value_t result = MIR_NULL_VALUE;
   if (is_array) {
      assert(kind == S_MATCH_EQ || kind == S_MATCH_NEQ);

      mir_value_t left_len = mir_build_uarray_len(mu, left, 0);
      mir_value_t right_len = mir_build_uarray_len(mu, right, 0);

      mir_block_t fail_bb = mir_add_block(mu);
      mir_block_t cont_bb = mir_add_block(mu);

      mir_value_t len_eq = mir_build_cmp(mu, MIR_CMP_EQ, left_len, right_len);
      mir_build_cond(mu, len_eq, cont_bb, fail_bb);

      mir_set_cursor(mu, fail_bb, MIR_APPEND);

      mir_type_t t_severity = mir_int_type(mu, 0, SEVERITY_FAILURE - 1);
      mir_value_t failure = mir_const(mu, t_severity, SEVERITY_FAILURE);

      mir_type_t t_offset = mir_offset_type(mu);

      static const char msg[] = "arguments have different lengths";
      mir_value_t msg_buf = mir_const_string(mu, msg);
      mir_value_t msg_ptr = mir_build_address_of(mu, msg_buf);
      mir_value_t msg_len = mir_const(mu, t_offset, sizeof(msg) - 1);

      mir_value_t locus = mir_build_locus(mu, tree_to_object(decl));
      mir_build_report(mu, msg_ptr, msg_len, failure, locus);
      mir_build_jump(mu, cont_bb);

      mir_set_cursor(mu, cont_bb, MIR_APPEND);

      const type_info_t *ti = type_info(mu, type_elem(type));
      mir_value_t mem = mir_build_alloc(mu, ti->type, ti->stamp, left_len);

      mir_value_t result_var = mir_add_var(mu, ti->type, ti->stamp,
                                           ident_new("result"), MIR_VAR_TEMP);
      mir_build_store(mu, result_var, mir_const(mu, ti->type, 0));

      mir_value_t i_var = mir_add_var(mu, t_offset, MIR_NULL_STAMP,
                                      ident_new("i"), MIR_VAR_TEMP);
      mir_value_t zero = mir_const(mu, t_offset, 0);
      mir_build_store(mu, i_var, zero);

      mir_value_t null = mir_build_cmp(mu, MIR_CMP_EQ, left_len, zero);

      mir_value_t left_ptr = mir_build_unwrap(mu, left);
      mir_value_t right_ptr = mir_build_unwrap(mu, right);

      mir_block_t body_bb = mir_add_block(mu);
      mir_block_t exit_bb = mir_add_block(mu);

      mir_build_cond(mu, null, exit_bb, body_bb);

      mir_set_cursor(mu, body_bb, MIR_APPEND);

      mir_value_t i_val = mir_build_load(mu, i_var);
      mir_value_t left_ptr_i = mir_build_array_ref(mu, left_ptr, i_val);
      mir_value_t right_ptr_i = mir_build_array_ref(mu, right_ptr, i_val);

      mir_value_t left_src = mir_build_load(mu, left_ptr_i);
      mir_value_t right_src = mir_build_load(mu, right_ptr_i);

      mir_value_t tmp;
      if (is_bit)
         tmp = mir_build_cmp(mu, cmp, left_src, right_src);
      else {
         ident_t func = ident_new( "IEEE.STD_LOGIC_1164.\"?=\"(UU)U$predef");
         mir_value_t context =
            mir_build_link_package(mu, well_known(W_IEEE_1164));
         mir_value_t args[] = { context, left_src, right_src };
         tmp = mir_build_fcall(mu, func, ti->type, ti->stamp, args, 3);
      }
      mir_build_store(mu, mir_build_array_ref(mu, mem, i_val), tmp);

      mir_value_t one = mir_const(mu, t_offset, 1);
      mir_value_t next = mir_build_add(mu, t_offset, i_val, one);
      mir_value_t finished = mir_build_cmp(mu, MIR_CMP_EQ, next, left_len);
      mir_build_store(mu, i_var, next);
      mir_build_cond(mu, finished, exit_bb, body_bb);

      mir_set_cursor(mu, exit_bb, MIR_APPEND);

      mir_dim_t dims[1] = {
         {
            .left  = mir_build_uarray_left(mu, left, 0),
            .right = mir_build_uarray_right(mu, left, 0),
            .dir   = mir_build_uarray_dir(mu, left, 0),
         }
      };
      mir_value_t wrap = mir_build_wrap(mu, mem, dims, 1);

      ident_t func, context_name;
      if (is_bit) {
         func = ident_new("STD.STANDARD.\"and\"(Q)J$predef");
         context_name = well_known(W_STD_STANDARD);
      }
      else {
         func = ident_new("IEEE.STD_LOGIC_1164.\"and\"(Y)U");
         context_name = well_known(W_IEEE_1164);
      }

      mir_value_t context = mir_build_link_package(mu, context_name);
      mir_value_t args[] = { context, wrap };
      result = mir_build_fcall(mu, func, ti->type, ti->stamp, args, 2);
   }
   else if (is_bit)
      result = mir_build_cmp(mu, cmp, left, right);
   else if (cmp == MIR_CMP_LEQ) {
      ident_t less_func = ident_new("IEEE.STD_LOGIC_1164.\"?<\"(UU)U$predef");
      ident_t eq_func = ident_new("IEEE.STD_LOGIC_1164.\"?=\"(UU)U$predef");

      mir_value_t context = mir_build_link_package(mu, well_known(W_IEEE_1164));
      mir_value_t args1[] = { context, left, right };

      const type_info_t *ti = type_info(mu, type);

      mir_value_t eq =
         mir_build_fcall(mu, eq_func, ti->type, ti->stamp, args1, 3);
      mir_value_t less =
         mir_build_fcall(mu, less_func, ti->type, ti->stamp, args1, 3);

      ident_t or_func =
         ident_new("IEEE.STD_LOGIC_1164.\"or\"(UU)24IEEE.STD_LOGIC_1164.UX01");

      mir_value_t args2[] = { context, eq, less };
      result = mir_build_fcall(mu, or_func, ti->type, ti->stamp, args2, 3);
   }
   else {
      static const std_ulogic_t match_eq_table[9][9] = {
         { _U, _U, _U, _U, _U, _U, _U, _U, _1 },
         { _U, _X, _X, _X, _X, _X, _X, _X, _1 },
         { _U, _X, _1, _0, _X, _X, _1, _0, _1 },
         { _U, _X, _0, _1, _X, _X, _0, _1, _1 },
         { _U, _X, _X, _X, _X, _X, _X, _X, _1 },
         { _U, _X, _X, _X, _X, _X, _X, _X, _1 },
         { _U, _X, _1, _0, _X, _X, _1, _0, _1 },
         { _U, _X, _0, _1, _X, _X, _0, _1, _1 },
         { _1, _1, _1, _1, _1, _1, _1, _1, _1 }
      };

      static const std_ulogic_t match_lt_table[9][9] = {
         { _U, _U, _U, _U, _U, _U, _U, _U, _X },
         { _U, _X, _X, _X, _X, _X, _X, _X, _X },
         { _U, _X, _0, _1, _X, _X, _0, _1, _X },
         { _U, _X, _0, _0, _X, _X, _0, _0, _X },
         { _U, _X, _X, _X, _X, _X, _X, _X, _X },
         { _U, _X, _X, _X, _X, _X, _X, _X, _X },
         { _U, _X, _0, _1, _X, _X, _0, _1, _X },
         { _U, _X, _0, _0, _X, _X, _0, _0, _X },
         { _X, _X, _X, _X, _X, _X, _X, _X, _X }
      };

      const std_ulogic_t (*table)[9] =
         cmp == MIR_CMP_LT ? match_lt_table : match_eq_table;

      const type_info_t *ti = type_info(mu, type);
      mir_type_t t_table = mir_carray_type(mu, 9*9, ti->type);

      mir_value_t elems[9 * 9];
      for (int i = 0; i < 9; i++) {
         for (int j = 0; j < 9; j++)
            elems[i*9 + j] = mir_const(mu, ti->type, table[i][j]);
      }

      mir_value_t table_buf = mir_const_array(mu, t_table, elems, 9*9);
      mir_value_t table_ptr = mir_build_address_of(mu, table_buf);

      mir_type_t t_offset = mir_offset_type(mu);

      if (cmp == MIR_CMP_LT) {
         mir_value_t dontcare = mir_const(mu, ti->type, _D);
         mir_value_t lcmp = mir_build_cmp(mu, MIR_CMP_NEQ, left, dontcare);
         mir_value_t rcmp = mir_build_cmp(mu, MIR_CMP_NEQ, right, dontcare);
         mir_value_t and = mir_build_and(mu, lcmp, rcmp);

         const char *msg =
            "STD_LOGIC_1164: '-' operand for matching ordering operator";
         mir_value_t msg_buf = mir_const_string(mu, msg);
         mir_value_t msg_ptr = mir_build_address_of(mu, msg_buf);
         mir_value_t msg_len = mir_const(mu, t_offset, sizeof(msg) - 1);

         mir_type_t t_severity = mir_int_type(mu, 0, SEVERITY_FAILURE - 1);
         mir_value_t error = mir_const(mu, t_severity, SEVERITY_ERROR);
         mir_value_t locus = mir_build_locus(mu, tree_to_object(decl));

         mir_build_assert(mu, and, msg_ptr, msg_len, error, locus,
                          MIR_NULL_VALUE, MIR_NULL_VALUE);
      }

      mir_value_t l_off = mir_build_cast(mu, t_offset, left);
      mir_value_t r_off = mir_build_cast(mu, t_offset, right);
      mir_value_t nine = mir_const(mu, t_offset, 9);
      mir_value_t index =
         mir_build_add(mu, t_offset,
                       mir_build_mul(mu, t_offset, l_off, nine), r_off);
      mir_value_t ptr = mir_build_array_ref(mu, table_ptr, index);

      result = mir_build_load(mu, ptr);
   }

   if (invert && is_bit)
      mir_build_return(mu, mir_build_not(mu, result));
   else if (invert) {
      const type_info_t *ti = type_info(mu, type_result(tree_type(decl)));
      ident_t func = ident_new(
         "IEEE.STD_LOGIC_1164.\"not\"(U)24IEEE.STD_LOGIC_1164.UX01");
      mir_value_t context = mir_build_link_package(mu, well_known(W_IEEE_1164));
      mir_value_t args[2] = { context, result };
      mir_value_t call =
         mir_build_fcall(mu, func, ti->type, ti->stamp, args, 2);
      mir_build_return(mu, call);
   }
   else
      mir_build_return(mu, result);
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
   case S_MATCH_EQ:
   case S_MATCH_NEQ:
   case S_MATCH_LT:
   case S_MATCH_LE:
   case S_MATCH_GT:
   case S_MATCH_GE:
      predef_match_op(mu, decl, kind);
      break;
   default:
      should_not_reach_here();
   }
}
