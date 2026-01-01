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
#include "vhdl/vhdl-util.h"

#include <stdlib.h>
#include <assert.h>
#include <string.h>

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

static mir_value_t wrap_string(mir_unit_t *mu, const char *str)
{
   mir_value_t array = mir_const_string(mu, str);
   mir_value_t addr = mir_build_address_of(mu, array);

   mir_type_t t_offset = mir_offset_type(mu);
   mir_type_t t_bool = mir_bool_type(mu);

   mir_dim_t dims[] = {
      { mir_const(mu, t_offset, 1),
        mir_const(mu, t_offset, strlen(str)),
        mir_const(mu, t_bool, RANGE_TO)
      },
   };
   return mir_build_wrap(mu, addr, dims, 1);
}

static void predef_bit_shift(mir_unit_t *mu, tree_t decl,
                             subprogram_kind_t kind)
{
   type_t type = tree_type(tree_port(decl, 0));

   const type_info_t *elem = type_info(mu, type_elem(type));
   mir_type_t t_offset = mir_offset_type(mu);
   mir_type_t t_bool = mir_bool_type(mu);

   mir_value_t input = mir_get_param(mu, 0);
   mir_value_t shift = mir_get_param(mu, 1);

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

   mir_value_t left = mir_get_param(mu, 0);
   mir_value_t right = mir_get_param(mu, 1);

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
         mir_value_t args[] = { left_src, right_src };
         tmp = mir_build_fcall(mu, func, ti->type, ti->stamp, args,
                               ARRAY_LEN(args));
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

      if (is_bit) {
         ident_t func = ident_new("STD.STANDARD.\"and\"(Q)J$predef");
         mir_value_t args[] = { wrap };
         result = mir_build_fcall(mu, func, ti->type, ti->stamp, args,
                                  ARRAY_LEN(args));
      }
      else {
         ident_t func = ident_new("IEEE.STD_LOGIC_1164.\"and\"(Y)U");
         ident_t context_name = well_known(W_IEEE_1164);

         mir_value_t context = mir_build_link_package(mu, context_name);
         mir_value_t args[] = { context, wrap };
         result = mir_build_fcall(mu, func, ti->type, ti->stamp, args,
                                  ARRAY_LEN(args));
      }
   }
   else if (is_bit)
      result = mir_build_cmp(mu, cmp, left, right);
   else if (cmp == MIR_CMP_LEQ) {
      ident_t less_func = ident_new("IEEE.STD_LOGIC_1164.\"?<\"(UU)U$predef");
      ident_t eq_func = ident_new("IEEE.STD_LOGIC_1164.\"?=\"(UU)U$predef");

      mir_value_t args1[] = { left, right };

      const type_info_t *ti = type_info(mu, type);

      mir_value_t eq = mir_build_fcall(mu, eq_func, ti->type, ti->stamp,
                                       args1, ARRAY_LEN(args1));
      mir_value_t less =
         mir_build_fcall(mu, less_func, ti->type, ti->stamp, args1, ARRAY_LEN(args1));

      ident_t or_func =
         ident_new("IEEE.STD_LOGIC_1164.\"or\"(UU)24IEEE.STD_LOGIC_1164.UX01");

      mir_value_t context = mir_build_link_package(mu, well_known(W_IEEE_1164));
      mir_value_t args2[] = { context, eq, less };
      result = mir_build_fcall(mu, or_func, ti->type, ti->stamp, args2,
                               ARRAY_LEN(args2));
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

static void predef_bit_vec_op(mir_unit_t *mu, tree_t decl,
                              subprogram_kind_t kind)
{
   type_t type = tree_type(tree_port(decl, 0));
   type_t elem = type_elem(type);

   const type_info_t *ti = type_info(mu, elem);

   mir_type_t t_offset = mir_offset_type(mu);

   mir_value_t left = mir_get_param(mu, 0), right = MIR_NULL_VALUE;
   if (kind != S_ARRAY_NOT)
      right = mir_get_param(mu, 1);

   mir_value_t left_len = mir_build_uarray_len(mu, left, 0);
   mir_value_t right_len = MIR_NULL_VALUE;

   mir_value_t left_data = mir_build_unwrap(mu, left);
   mir_value_t right_data = MIR_NULL_VALUE;

   if (kind != S_ARRAY_NOT) {
      right_len = mir_build_uarray_len(mu, right, 0);
      right_data = mir_build_unwrap(mu, right);

      mir_block_t fail_bb = mir_add_block(mu);
      mir_block_t cont_bb = mir_add_block(mu);

      mir_value_t len_eq = mir_build_cmp(mu, MIR_CMP_EQ, left_len, right_len);
      mir_build_cond(mu, len_eq, cont_bb, fail_bb);

      mir_set_cursor(mu, fail_bb, MIR_APPEND);

      mir_type_t t_severity = mir_int_type(mu, 0, SEVERITY_FAILURE - 1);
      mir_value_t failure = mir_const(mu, t_severity, SEVERITY_FAILURE);

      static const char msg_str[] = "arguments have different lengths";
      mir_value_t msg_array = mir_const_string(mu, msg_str);
      mir_value_t msg_ptr = mir_build_address_of(mu, msg_array);
      mir_value_t msg_len = mir_const(mu, t_offset, sizeof(msg_str) - 1);

      mir_value_t locus = mir_build_locus(mu, tree_to_object(decl));
      mir_build_report(mu, msg_ptr, msg_len, failure, locus);

      mir_build_return(mu, left);

      mir_set_cursor(mu, cont_bb, MIR_APPEND);
   }

   mir_value_t mem = mir_build_alloc(mu, ti->type, ti->stamp, left_len);

   mir_value_t i_var = mir_add_var(mu, t_offset, MIR_NULL_STAMP,
                                   ident_new("i"), MIR_VAR_TEMP);
   mir_build_store(mu, i_var, mir_const(mu, t_offset, 0));

   mir_block_t cmp_bb  = mir_add_block(mu);
   mir_block_t body_bb = mir_add_block(mu);
   mir_block_t exit_bb = mir_add_block(mu);

   mir_build_jump(mu, cmp_bb);

   mir_set_cursor(mu, cmp_bb, MIR_APPEND);

   mir_value_t i_val = mir_build_load(mu, i_var);
   mir_value_t eq = mir_build_cmp(mu, MIR_CMP_EQ, i_val, left_len);
   mir_build_cond(mu, eq, exit_bb, body_bb);

   mir_set_cursor(mu, body_bb, MIR_APPEND);

   mir_value_t dst_ptr = mir_build_array_ref(mu, mem, i_val);

   mir_value_t src0_ptr = mir_build_array_ref(mu, left_data, i_val);
   mir_value_t src0 = mir_build_load(mu, src0_ptr);

   mir_value_t src1 = MIR_NULL_VALUE;
   if (kind != S_ARRAY_NOT) {
      mir_value_t src1_ptr = mir_build_array_ref(mu, right_data, i_val);
      src1 = mir_build_load(mu, src1_ptr);
   }

   bool negate = false;
   mir_value_t op;
   switch (kind) {
   case S_ARRAY_NOT:  op = mir_build_not(mu, src0); break;
   case S_ARRAY_NAND: negate = true;
   case S_ARRAY_AND:  op = mir_build_and(mu, src0, src1); break;
   case S_ARRAY_NOR:  negate = true;
   case S_ARRAY_OR:   op = mir_build_or(mu, src0, src1); break;
   case S_ARRAY_XNOR: negate = true;
   case S_ARRAY_XOR:  op = mir_build_xor(mu, src0, src1); break;
   default:           should_not_reach_here();
   }

   if (negate)
      op = mir_build_not(mu, op);

   mir_build_store(mu, dst_ptr, op);

   mir_value_t one = mir_const(mu, t_offset, 1);
   mir_value_t next = mir_build_add(mu, t_offset, i_val, one);
   mir_build_store(mu, i_var, next);
   mir_build_jump(mu, cmp_bb);

   mir_set_cursor(mu, exit_bb, MIR_APPEND);

   mir_value_t b_left  = mir_build_uarray_left(mu, left, 0);
   mir_value_t b_right = mir_build_uarray_right(mu, left, 0);
   mir_value_t b_dir   = mir_build_uarray_dir(mu, left, 0);

   mir_dim_t dims[] = { { b_left, b_right, b_dir } };
   mir_build_return(mu, mir_build_wrap(mu, mem, dims, 1));
}

static void predef_edge_op(mir_unit_t *mu, tree_t decl)
{
   mir_type_t t_offset = mir_offset_type(mu);

   mir_value_t nets = mir_get_param(mu, 0);
   mir_value_t count = mir_const(mu, t_offset, 1);
   mir_value_t value = mir_build_load(mu, mir_build_resolved(mu, nets));

   if (tree_subkind(decl) == S_FALLING_EDGE)
      value = mir_build_not(mu, value);

   mir_value_t event = mir_build_event_flag(mu, nets, count);
   mir_build_return(mu, mir_build_and(mu, event, value));
}

static void predef_array_cmp(mir_unit_t *mu, tree_t decl, mir_cmp_t pred)
{
   mir_value_t lhs_array = mir_get_param(mu, 0);
   mir_value_t rhs_array = mir_get_param(mu, 1);
   mir_value_t lhs_data = mir_build_unwrap(mu, lhs_array);
   mir_value_t rhs_data = mir_build_unwrap(mu, rhs_array);

   mir_block_t fail_bb = mir_add_block(mu);

   // Behaviour of relational operators on arrays is described in
   // LRM 93 section 7.2.2

   assert(pred == MIR_CMP_LT || pred == MIR_CMP_LEQ);

#ifdef DEBUG
   type_t type = tree_type(tree_port(decl, 0));
   assert(tree_type(tree_port(decl, 1)) == type);

   const type_info_t *ti = type_info(mu, type);
   const type_info_t *elem = type_info(mu, type_elem(type));
   assert(ti->ndims == 1);
   assert(elem->kind != T_ARRAY && elem->kind != T_RECORD);
#endif

   mir_type_t t_offset = mir_offset_type(mu);
   mir_type_t t_bool = mir_bool_type(mu);

   mir_value_t left_len = mir_build_uarray_len(mu, lhs_array, 0);
   mir_value_t right_len = mir_build_uarray_len(mu, rhs_array, 0);

   mir_value_t len_eq = mir_build_cmp(mu, MIR_CMP_EQ, left_len, right_len);

   mir_value_t i_var = mir_add_var(mu, t_offset, MIR_NULL_STAMP,
                                   ident_uniq("i"), MIR_VAR_TEMP);
   mir_build_store(mu, i_var, mir_const(mu, t_offset, 0));

   mir_block_t test_bb = mir_add_block(mu);
   mir_block_t body_bb = mir_add_block(mu);
   mir_block_t exit_bb = mir_add_block(mu);

   mir_build_jump(mu, test_bb);

   // Loop test

   mir_set_cursor(mu, test_bb, MIR_APPEND);

   mir_value_t i_loaded = mir_build_load(mu, i_var);

   mir_block_t check_r_len_bb = mir_add_block(mu);

   mir_value_t len_ge_l = mir_build_cmp(mu, MIR_CMP_GEQ, i_loaded, left_len);
   mir_build_cond(mu, len_ge_l, exit_bb, check_r_len_bb);

   mir_set_cursor(mu, check_r_len_bb, MIR_APPEND);

   mir_value_t len_ge_r = mir_build_cmp(mu, MIR_CMP_GEQ, i_loaded, right_len);
   mir_build_cond(mu, len_ge_r, fail_bb, body_bb);

   // Loop body

   mir_set_cursor(mu, body_bb, MIR_APPEND);

   mir_value_t one = mir_const(mu, t_offset, 1);
   mir_value_t inc = mir_build_add(mu, t_offset, i_loaded, one);
   mir_build_store(mu, i_var, inc);

   mir_value_t i_eq_len = mir_build_cmp(mu, MIR_CMP_EQ, inc, left_len);

   mir_value_t l_ptr = mir_build_array_ref(mu, lhs_data, i_loaded);
   mir_value_t r_ptr = mir_build_array_ref(mu, rhs_data, i_loaded);

   mir_value_t l_val = mir_build_load(mu, l_ptr);
   mir_value_t r_val = mir_build_load(mu, r_ptr);

   mir_value_t cmp = mir_build_cmp(mu, pred, l_val, r_val);
   mir_value_t eq  = mir_build_cmp(mu, MIR_CMP_EQ, l_val, r_val);

   mir_value_t done = mir_build_or(mu, mir_build_not(mu, eq),
                                   mir_build_and(mu, len_eq, i_eq_len));

   mir_block_t cmp_result_bb = mir_add_block(mu);
   mir_build_cond(mu, done, cmp_result_bb, test_bb);

   mir_set_cursor(mu, cmp_result_bb, MIR_APPEND);
   mir_build_cond(mu, cmp, exit_bb, fail_bb);

   // Epilogue

   mir_set_cursor(mu, exit_bb, MIR_APPEND);

   mir_build_return(mu, mir_const(mu, t_bool, 1));

   mir_set_cursor(mu, fail_bb, MIR_APPEND);

   mir_build_return(mu, mir_const(mu, t_bool, 0));
}

static void predef_array_eq(mir_unit_t *mu, tree_t decl)
{
   type_t type = tree_type(tree_port(decl, 0));
   assert(tree_type(tree_port(decl, 1)) == type);

   mir_value_t lhs_array = mir_get_param(mu, 0);
   mir_value_t rhs_array = mir_get_param(mu, 1);
   mir_value_t lhs_data = mir_build_unwrap(mu, lhs_array);
   mir_value_t rhs_data = mir_build_unwrap(mu, rhs_array);

   mir_block_t fail_bb = mir_add_block(mu);

   const type_info_t *ti = type_info(mu, type);
   const type_info_t *elem = type_info(mu, type_elem_recur(type));

   mir_type_t t_offset = mir_offset_type(mu);
   mir_type_t t_bool = mir_bool_type(mu);

   mir_value_t len_eq = mir_const(mu, t_bool, 1);
   mir_value_t total = mir_const(mu, t_offset, 1);

   int pos = 0;
   for (const type_info_t *e = ti; e->kind == T_ARRAY;
        pos += e->ndims, e = type_info(mu, type_elem(e->source))) {

      if (e->size < SIZE_MAX && e->stride < SIZE_MAX) {
         mir_value_t size = mir_const(mu, t_offset, e->size * e->stride);
         total = mir_build_mul(mu, t_offset, total, size);
         break;
      }

      for (int i = 0; i < e->ndims; i++) {
         mir_value_t left_len = mir_build_uarray_len(mu, lhs_array, pos + i);
         mir_value_t right_len = mir_build_uarray_len(mu, rhs_array, pos + i);

         mir_value_t cmp = mir_build_cmp(mu, MIR_CMP_EQ, left_len, right_len);
         len_eq = mir_build_and(mu, len_eq, cmp);
         total = mir_build_mul(mu, t_offset, total, left_len);
      }
   }
   assert(pos == ti->udims);

   mir_value_t i_var = mir_add_var(mu, t_offset, MIR_NULL_STAMP,
                                   ident_uniq("i"), MIR_VAR_TEMP);
   mir_build_store(mu, i_var, mir_const(mu, t_offset, 0));

   mir_block_t test_bb = mir_add_block(mu);
   mir_block_t body_bb = mir_add_block(mu);
   mir_block_t exit_bb = mir_add_block(mu);

   mir_value_t stride = MIR_NULL_VALUE;

   mir_build_cond(mu, len_eq, test_bb, fail_bb);

   // Loop test

   mir_set_cursor(mu, test_bb, MIR_APPEND);

   mir_value_t i_loaded = mir_build_load(mu, i_var);

   mir_value_t done = mir_build_cmp(mu, MIR_CMP_EQ, i_loaded, total);
   mir_build_cond(mu, done, exit_bb, body_bb);

   // Loop body

   mir_set_cursor(mu, body_bb, MIR_APPEND);

   mir_value_t ptr_inc = i_loaded;
   if (!mir_is_null(stride))
      ptr_inc = mir_build_mul(mu, t_offset, ptr_inc, stride);

   mir_value_t one = mir_const(mu, t_offset, 1);
   mir_value_t inc = mir_build_add(mu, t_offset, i_loaded, one);
   mir_build_store(mu, i_var, inc);

   mir_value_t l_ptr = mir_build_array_ref(mu, lhs_data, ptr_inc);
   mir_value_t r_ptr = mir_build_array_ref(mu, rhs_data, ptr_inc);

   if (elem->kind == T_RECORD) {   // XXX: should recurse
      ident_t func = predef_func_name(elem->source, "=");

      mir_value_t args[] = { l_ptr, r_ptr };
      mir_value_t eq = mir_build_fcall(mu, func, t_bool, MIR_NULL_STAMP,
                                       args, ARRAY_LEN(args));

      mir_build_cond(mu, eq, test_bb, fail_bb);
   }
   else {
      mir_value_t l_val = mir_build_load(mu, l_ptr);
      mir_value_t r_val = mir_build_load(mu, r_ptr);

      mir_value_t eq = mir_build_cmp(mu, MIR_CMP_EQ, l_val, r_val);
      mir_build_cond(mu, eq, test_bb, fail_bb);
   }

   // Epilogue

   mir_set_cursor(mu, exit_bb, MIR_APPEND);

   mir_build_return(mu, mir_const(mu, t_bool, 1));

   mir_set_cursor(mu, fail_bb, MIR_APPEND);

   mir_build_return(mu, mir_const(mu, t_bool, 0));
}


static void predef_min_max(mir_unit_t *mu, tree_t decl, mir_cmp_t cmp)
{
   type_t type = tree_type(tree_port(decl, 0));

   if (type_is_array(type) && tree_ports(decl) == 1) {
      const type_info_t *elem = type_info(mu, type_elem(type));
      assert(type_is_scalar(elem->source));

      mir_value_t array = mir_get_param(mu, 0);

      mir_type_t t_offset = mir_offset_type(mu);
      mir_value_t i_var = mir_add_var(mu, t_offset, MIR_NULL_STAMP,
                                      ident_new("i"), MIR_VAR_TEMP);
      mir_value_t zero = mir_const(mu, t_offset, 0);
      mir_build_store(mu, i_var, zero);

      mir_value_t result_var =
         mir_add_var(mu, elem->type, elem->stamp, ident_new("result"), 0);

      tree_t elem_r = range_of(elem->source, 0);
      tree_t def = (cmp == MIR_CMP_GT && tree_subkind(elem_r) == RANGE_TO)
         || (cmp == MIR_CMP_LT && tree_subkind(elem_r) == RANGE_DOWNTO)
         ? tree_left(elem_r) : tree_right(elem_r);

      mir_value_t def_val;
      if (tree_kind(def) == T_LITERAL && tree_subkind(def) == L_REAL)
         def_val = mir_const_real(mu, elem->type, tree_dval(def));
      else
         def_val = mir_const(mu, elem->type, assume_int(def));

      mir_build_store(mu, result_var, def_val);

      mir_value_t len   = mir_build_uarray_len(mu, array, 0);
      mir_value_t data  = mir_build_unwrap(mu, array);
      mir_value_t null = mir_build_cmp(mu, MIR_CMP_EQ, len, zero);

      mir_block_t body_bb = mir_add_block(mu);
      mir_block_t exit_bb = mir_add_block(mu);

      mir_build_cond(mu, null, exit_bb, body_bb);

      mir_set_cursor(mu, body_bb, MIR_APPEND);

      mir_value_t i_val    = mir_build_load(mu, i_var);
      mir_value_t elem_ptr = mir_build_array_ref(mu, data, i_val);
      mir_value_t elem_val = mir_build_load(mu, elem_ptr);
      mir_value_t cur_val  = mir_build_load(mu, result_var);
      mir_value_t cmp_val  = mir_build_cmp(mu, cmp, elem_val, cur_val);

      mir_value_t next =
         mir_build_select(mu, elem->type, cmp_val, elem_val, cur_val);
      mir_build_store(mu, result_var, next);

      mir_value_t one = mir_const(mu, t_offset, 1);
      mir_value_t i_next = mir_build_add(mu, t_offset, i_val, one);
      mir_build_store(mu, i_var, i_next);

      mir_value_t done = mir_build_cmp(mu, MIR_CMP_EQ, i_next, len);
      mir_build_cond(mu, done, exit_bb, body_bb);

      mir_set_cursor(mu, exit_bb, MIR_APPEND);
      mir_build_return(mu, mir_build_load(mu, result_var));
   }
   else {
      mir_value_t lhs = mir_get_param(mu, 0);
      mir_value_t rhs = mir_get_param(mu, 1);

      mir_value_t test;
      if (type_is_scalar(type))
         test = mir_build_cmp(mu, cmp, lhs, rhs);
      else {
         const char *op = cmp == MIR_CMP_GT ? ">" : "<";
         ident_t func = predef_func_name(type, op);
         mir_value_t args[] = { lhs, rhs };
         mir_type_t t_bool = mir_bool_type(mu);
         test = mir_build_fcall(mu, func, t_bool, MIR_NULL_STAMP, args,
                                ARRAY_LEN(args));
      }

      mir_type_t t_result = mir_get_type(mu, lhs);
      mir_build_return(mu, mir_build_select(mu, t_result, test, lhs, rhs));
   }
}

static void predef_negate(mir_unit_t *mu, tree_t decl, const char *op)
{
   mir_value_t args[2];
   for (int i = 0; i < ARRAY_LEN(args); i++)
      args[i] = mir_get_param(mu, i);

   type_t type = tree_type(tree_port(decl, 0));
   ident_t func = predef_func_name(type, op);

   mir_type_t t_bool = mir_bool_type(mu);

   mir_value_t eq = mir_build_fcall(mu, func, t_bool, MIR_NULL_STAMP,
                                    args, ARRAY_LEN(args));
   mir_build_return(mu, mir_build_not(mu, eq));
}

static void predef_reduction_op(mir_unit_t *mu, tree_t decl,
                                subprogram_kind_t kind)
{
   mir_value_t param = mir_get_param(mu, 0);

   mir_type_t t_bool = mir_bool_type(mu);
   mir_type_t t_offset = mir_offset_type(mu);

   mir_value_t result_var = mir_add_var(mu, t_bool, MIR_NULL_STAMP,
                                        ident_new("result"), MIR_VAR_TEMP);
   mir_value_t init = mir_const(mu, t_bool,
                                kind == S_REDUCE_NAND || kind == S_REDUCE_AND);
   mir_build_store(mu, result_var, init);

   mir_value_t zero = mir_const(mu, t_offset, 0);

   mir_value_t i_var = mir_add_var(mu, t_offset, MIR_NULL_STAMP,
                                   ident_new("i"), MIR_VAR_TEMP);
   mir_build_store(mu, i_var, zero);

   mir_value_t len   = mir_build_uarray_len(mu, param, 0);
   mir_value_t data  = mir_build_unwrap(mu, param);
   mir_value_t null  = mir_build_cmp(mu, MIR_CMP_EQ, len, zero);

   mir_block_t body_bb = mir_add_block(mu);
   mir_block_t exit_bb = mir_add_block(mu);

   mir_build_cond(mu, null, exit_bb, body_bb);

   mir_set_cursor(mu, body_bb, MIR_APPEND);

   mir_value_t i_val = mir_build_load(mu, i_var);
   mir_value_t ptr   = mir_build_array_ref(mu, data, i_val);
   mir_value_t src   = mir_build_load(mu, ptr);
   mir_value_t cur   = mir_build_load(mu, result_var);

   mir_value_t result;
   switch (kind) {
   case S_REDUCE_OR:
   case S_REDUCE_NOR:
      result = mir_build_or(mu, cur, src);
      break;
   case S_REDUCE_AND:
   case S_REDUCE_NAND:
      result = mir_build_and(mu, cur, src);
      break;
   case S_REDUCE_XOR:
   case S_REDUCE_XNOR:
      result = mir_build_xor(mu, cur, src);
      break;
   default:
      should_not_reach_here();
   }

   mir_build_store(mu, result_var, result);

   mir_value_t one  = mir_const(mu, t_offset, 1);
   mir_value_t next = mir_build_add(mu, t_offset, i_val, one);
   mir_value_t cmp  = mir_build_cmp(mu, MIR_CMP_EQ, next, len);
   mir_build_store(mu, i_var, next);
   mir_build_cond(mu, cmp, exit_bb, body_bb);

   mir_set_cursor(mu, exit_bb, MIR_APPEND);

   mir_value_t loaded = mir_build_load(mu, result_var);

   if (kind == S_REDUCE_NOR || kind == S_REDUCE_NAND || kind == S_REDUCE_XNOR)
      mir_build_return(mu, mir_build_not(mu, loaded));
   else
      mir_build_return(mu, loaded);
}

void vhdl_lower_predef(mir_unit_t *mu, object_t *obj)
{
   tree_t decl = tree_from_object(obj);

   const int nports = tree_ports(decl);
   for (int i = 0; i < nports; i++) {
      tree_t p = tree_port(decl, i);
      assert(tree_subkind(p) == PORT_IN);

      const type_info_t *ti = type_info(mu, tree_type(p));

      mir_type_t type = ti->type;
      if (tree_class(p) == C_SIGNAL)
         type = mir_signal_type(mu, type);

      mir_add_param(mu, type, ti->stamp, tree_ident(p));
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
   case S_ARRAY_NOT:
   case S_ARRAY_AND:
   case S_ARRAY_OR:
   case S_ARRAY_XOR:
   case S_ARRAY_XNOR:
   case S_ARRAY_NAND:
   case S_ARRAY_NOR:
      predef_bit_vec_op(mu, decl, kind);
      break;
   case S_ARRAY_NEQ:
   case S_RECORD_NEQ:
      predef_negate(mu, decl, "=");
      break;
   case S_ARRAY_EQ:
      predef_array_eq(mu, decl);
      break;
   case S_ARRAY_LT:
      predef_array_cmp(mu, decl, MIR_CMP_LT);
      break;
   case S_ARRAY_LE:
      predef_array_cmp(mu, decl, MIR_CMP_LEQ);
      break;
   case S_ARRAY_GE:
      predef_negate(mu, decl, "<");
      break;
   case S_ARRAY_GT:
      predef_negate(mu, decl, "<=");
      break;
   case S_RISING_EDGE:
   case S_FALLING_EDGE:
      predef_edge_op(mu, decl);
      break;
   case S_MAXIMUM:
      predef_min_max(mu, decl, MIR_CMP_GT);
      break;
   case S_MINIMUM:
      predef_min_max(mu, decl, MIR_CMP_LT);
      break;
   case S_REDUCE_OR:
   case S_REDUCE_AND:
   case S_REDUCE_NAND:
   case S_REDUCE_NOR:
   case S_REDUCE_XOR:
   case S_REDUCE_XNOR:
      predef_reduction_op(mu, decl, kind);
      break;
   default:
      should_not_reach_here();
   }

   mir_optimise(mu, MIR_PASS_O0);
}

static void enum_image_helper(mir_unit_t *mu, type_t type, mir_value_t arg)
{
   const int nlits = type_enum_literals(type);
   assert(nlits >= 1);

   mir_block_t *blocks LOCAL = xmalloc_array(nlits, sizeof(mir_block_t));
   mir_value_t *cases LOCAL = xmalloc_array(nlits, sizeof(mir_value_t));

   const type_info_t *ti = type_info(mu, type);

   for (int i = 0; i < nlits; i++) {
      cases[i]  = mir_const(mu, ti->type, i);
      blocks[i] = mir_add_block(mu);
   }

   mir_build_case(mu, arg, blocks[0], cases, blocks, nlits);

   for (int i = 0; i < nlits; i++) {
      // LRM specifies result is lowercase for enumerated types when
      // the value is a basic identifier
      ident_t id = tree_ident(type_enum_literal(type, i));
      if (ident_char(id, 0) != '\'')
         id = ident_downcase(id);

      mir_set_cursor(mu, blocks[i], MIR_APPEND);

      mir_value_t str = wrap_string(mu, istr(id));
      mir_build_return(mu, str);
   }
}

static void numeric_image_helper(mir_unit_t *mu, type_t type, mir_value_t arg)
{
   ident_t conv_fn;
   mir_value_t cast;
   if (type_is_real(type)) {
      cast = arg;
      conv_fn = ident_new("NVC.TEXT_UTIL.REAL_TO_STRING(R)S");
   }
   else {
      mir_type_t t_int64 = mir_int_type(mu, INT64_MIN, INT64_MAX);
      cast = mir_build_cast(mu, t_int64, arg);
      conv_fn = ident_new(
         "NVC.TEXT_UTIL.INT_TO_STRING(21NVC.TEXT_UTIL.T_INT64)S");
   }

   mir_type_t t_string = mir_string_type(mu);

   mir_value_t text_util = mir_build_link_package(mu, well_known(W_TEXT_UTIL));
   mir_value_t conv_args[] = { text_util, cast };
   mir_value_t str = mir_build_fcall(mu, conv_fn, t_string, MIR_NULL_STAMP,
                                     conv_args, 2);
   mir_build_return(mu, str);
}

static void physical_image_helper(mir_unit_t *mu, type_t type, mir_value_t arg)
{
   mir_type_t t_int64 = mir_int_type(mu, INT64_MIN, INT64_MAX);
   mir_type_t t_string = mir_string_type(mu);
   mir_type_t t_offset = mir_offset_type(mu);
   mir_type_t t_char = mir_char_type(mu);
   mir_type_t t_bool = mir_bool_type(mu);

   mir_value_t cast = mir_build_cast(mu, t_int64, arg);
   ident_t conv_fn =
      ident_new("NVC.TEXT_UTIL.INT_TO_STRING(21NVC.TEXT_UTIL.T_INT64)S");
   mir_value_t text_util = mir_build_link_package(mu, well_known(W_TEXT_UTIL));
   mir_value_t conv_args[] = { text_util, cast };
   mir_value_t num_str = mir_build_fcall(mu, conv_fn, t_string, MIR_NULL_STAMP,
                                         conv_args, ARRAY_LEN(conv_args));

   mir_value_t num_len = mir_build_uarray_len(mu, num_str, 0);

   const char *unit0 = istr(ident_downcase(tree_ident(type_unit(type, 0))));

   mir_value_t append_len = mir_const(mu, t_offset, strlen(unit0) + 1);
   mir_value_t total_len = mir_build_add(mu, t_offset, num_len, append_len);

   mir_value_t mem = mir_build_alloc(mu, t_char, MIR_NULL_STAMP, total_len);
   mir_build_copy(mu, mem, mir_build_unwrap(mu, num_str), num_len);

   mir_value_t ptr0 = mir_build_array_ref(mu, mem, num_len);
   mir_build_store(mu, ptr0, mir_const(mu, t_char, ' '));

   mir_value_t unit = wrap_string(mu, unit0);
   mir_value_t ptr1 =
      mir_build_array_ref(mu, ptr0, mir_const(mu, t_offset, 1));
   mir_build_copy(mu, ptr1, mir_build_unwrap(mu, unit),
                  mir_const(mu, t_offset, strlen(unit0)));

   mir_dim_t dims[] = {
      { .left  = mir_const(mu, t_offset, 1),
        .right = total_len,
        .dir   = mir_const(mu, t_bool, RANGE_TO),
      }
   };
   mir_build_return(mu, mir_build_wrap(mu, mem, dims, 1));
}

static void record_image_helper(mir_unit_t *mu, type_t type, mir_value_t arg)
{
   const int nfields = type_fields(type);
   mir_value_t regs[nfields];
   mir_value_t lengths[nfields];

   mir_type_t t_string = mir_string_type(mu);
   mir_type_t t_char = mir_char_type(mu);
   mir_type_t t_offset = mir_offset_type(mu);
   mir_type_t t_bool = mir_bool_type(mu);

   mir_value_t sum = mir_const(mu, t_offset, nfields + 1);

   for (int i = 0; i < nfields; i++) {
      type_t ftype = type_base_recur(tree_type(type_field(type, i)));
      ident_t func = ident_prefix(type_ident(ftype), ident_new("image"), '$');

      mir_value_t field = mir_build_record_ref(mu, arg, i);

      if (type_is_scalar(ftype) || mir_points_to(mu, field, MIR_TYPE_UARRAY))
         field = mir_build_load(mu, field);

      mir_value_t args[] = { field };
      regs[i] = mir_build_fcall(mu, func, t_string, MIR_NULL_STAMP, args,
                                ARRAY_LEN(args));
      lengths[i] = mir_build_uarray_len(mu, regs[i], 0);

      sum = mir_build_add(mu, t_offset, sum, lengths[i]);
   }

   mir_value_t mem = mir_build_alloc(mu, t_char, MIR_NULL_STAMP, sum);
   mir_value_t zero = mir_const(mu, t_offset, 0);
   mir_value_t lparen_ptr = mir_build_array_ref(mu, mem, zero);
   mir_build_store(mu, lparen_ptr, mir_const(mu, t_char, '('));

   mir_value_t one = mir_const(mu, t_offset, 1);
   mir_value_t comma = mir_const(mu, t_char, ',');

   mir_value_t index = one;
   for (int i = 0; i < nfields; i++) {
      if (i > 0) {
         mir_value_t comma_ptr = mir_build_array_ref(mu, mem, index);
         mir_build_store(mu, comma_ptr, comma);

         index = mir_build_add(mu, t_offset, index, one);
      }

      mir_value_t src_ptr = mir_build_unwrap(mu, regs[i]);
      mir_value_t dest_ptr = mir_build_array_ref(mu, mem, index);
      mir_build_copy(mu, dest_ptr, src_ptr, lengths[i]);

      index = mir_build_add(mu, t_offset, index, lengths[i]);
   }

   mir_value_t rparen_ptr = mir_build_array_ref(mu, mem, index);
   mir_build_store(mu, rparen_ptr, mir_const(mu, t_char, ')'));

   mir_dim_t dims[] = {
      { .left  = one,
        .right = sum,
        .dir   = mir_const(mu, t_bool, RANGE_TO)
      }
   };
   mir_build_return(mu, mir_build_wrap(mu, mem, dims, 1));
}

static void array_image_helper(mir_unit_t *mu, type_t type, mir_value_t arg)
{
   mir_type_t t_char = mir_char_type(mu);
   mir_type_t t_string = mir_string_type(mu);
   mir_type_t t_offset = mir_offset_type(mu);
   mir_type_t t_bool = mir_bool_type(mu);

   mir_type_t fields[] = { mir_pointer_type(mu, t_char), t_offset };
   mir_type_t t_rec = mir_record_type(mu, ident_new("elem"), fields, 2);

   mir_value_t length = mir_build_uarray_len(mu, arg, 0);

   mir_value_t elems_mem = mir_build_alloc(mu, t_rec, MIR_NULL_STAMP, length);
   mir_value_t zero = mir_const(mu, t_offset, 0);
   mir_value_t one = mir_const(mu, t_offset, 1);
   mir_value_t data_ptr = mir_build_unwrap(mu, arg);

   mir_value_t i_var = mir_add_var(mu, t_offset, MIR_NULL_STAMP,
                                   ident_new("i"), MIR_VAR_TEMP);
   mir_build_store(mu, i_var, zero);

   mir_block_t loop1_bb = mir_add_block(mu);
   mir_block_t alloc_bb = mir_add_block(mu);

   mir_value_t sum_var = mir_add_var(mu, t_offset, MIR_NULL_STAMP,
                                     ident_new("sum"), MIR_VAR_TEMP);
   mir_build_store(mu, sum_var, mir_build_add(mu, t_offset, length, one));

   type_t elem = type_base_recur(type_elem(type));
   ident_t func = ident_prefix(type_ident(elem), ident_new("image"), '$');

   mir_value_t null = mir_build_cmp(mu, MIR_CMP_EQ, length, zero);
   mir_build_cond(mu, null, alloc_bb, loop1_bb);

   mir_set_cursor(mu, loop1_bb, MIR_APPEND);

   {
      mir_value_t i_val = mir_build_load(mu, i_var);
      mir_value_t sum_val = mir_build_load(mu, sum_var);

      mir_value_t elem_arg = mir_build_array_ref(mu, data_ptr, i_val);
      if (type_is_scalar(elem))
         elem_arg = mir_build_load(mu, elem_arg);

      mir_value_t args[] = { elem_arg };
      mir_value_t str = mir_build_fcall(mu, func, t_string, MIR_NULL_STAMP,
                                        args, ARRAY_LEN(args));

      mir_value_t edata_ptr = mir_build_unwrap(mu, str);
      mir_value_t elen = mir_build_uarray_len(mu, str, 0);
      mir_build_store(mu, sum_var, mir_build_add(mu, t_offset, sum_val, elen));

      mir_value_t rptr = mir_build_array_ref(mu, elems_mem, i_val);
      mir_build_store(mu, mir_build_record_ref(mu, rptr, 0), edata_ptr);
      mir_build_store(mu, mir_build_record_ref(mu, rptr, 1), elen);

      mir_value_t i_next = mir_build_add(mu, t_offset, i_val, one);
      mir_build_store(mu, i_var, i_next);

      mir_value_t done = mir_build_cmp(mu, MIR_CMP_EQ, i_next, length);
      mir_build_cond(mu, done, alloc_bb, loop1_bb);
   }

   mir_set_cursor(mu, alloc_bb, MIR_APPEND);

   mir_value_t sum_val = mir_build_load(mu, sum_var);
   mir_value_t mem = mir_build_alloc(mu, t_char, MIR_NULL_STAMP, sum_val);

   mir_value_t lparen_ptr = mir_build_array_ref(mu, mem, zero);
   mir_build_store(mu, lparen_ptr, mir_const(mu, t_char, '('));

   mir_block_t loop2_bb = mir_add_block(mu);
   mir_block_t exit_bb = mir_add_block(mu);

   mir_value_t index_var = mir_add_var(mu, t_offset, MIR_NULL_STAMP,
                                       ident_new("index"), MIR_VAR_TEMP);
   mir_build_store(mu, i_var, zero);
   mir_build_store(mu, index_var, one);

   mir_build_cond(mu, null, exit_bb, loop2_bb);

   mir_set_cursor(mu, loop2_bb, MIR_APPEND);

   {
      mir_value_t i_val = mir_build_load(mu, i_var);
      mir_value_t index = mir_build_load(mu, index_var);

      mir_value_t rptr = mir_build_array_ref(mu, elems_mem, i_val);
      mir_value_t edata = mir_build_load(mu, mir_build_record_ref(mu, rptr, 0));
      mir_value_t elen = mir_build_load(mu, mir_build_record_ref(mu, rptr, 1));

      mir_value_t dest_ptr = mir_build_array_ref(mu, mem, index);
      mir_build_copy(mu, dest_ptr, edata, elen);

      mir_value_t comma_index = mir_build_add(mu, t_offset, index, elen);
      mir_value_t comma_ptr = mir_build_array_ref(mu, mem, comma_index);
      mir_build_store(mu, comma_ptr, mir_const(mu, t_char, ','));

      mir_value_t index_next = mir_build_add(mu, t_offset, comma_index, one);
      mir_build_store(mu, index_var, index_next);

      mir_value_t i_next = mir_build_add(mu, t_offset, i_val, one);
      mir_build_store(mu, i_var, i_next);

      mir_value_t done = mir_build_cmp(mu, MIR_CMP_EQ, i_next, length);
      mir_build_cond(mu, done, exit_bb, loop2_bb);
   }

   mir_set_cursor(mu, exit_bb, MIR_APPEND);

   mir_value_t last = mir_build_sub(mu, t_offset, sum_val, one);
   mir_value_t rparen_ptr = mir_build_array_ref(mu, mem, last);
   mir_build_store(mu, rparen_ptr, mir_const(mu, t_char, ')'));

   mir_dim_t dims[] = {
      { .left  = mir_const(mu, t_offset, 1),
        .right = sum_val,
        .dir   = mir_const(mu, t_bool, RANGE_TO),
      }
   };
   mir_build_return(mu, mir_build_wrap(mu, mem, dims, 1));
}

static void character_array_image_helper(mir_unit_t *mu, type_t type,
                                         mir_value_t arg, bool quote)
{
   type_t elem = type_base_recur(type_elem(type));

   mir_type_t t_char = mir_char_type(mu);
   mir_type_t t_offset = mir_offset_type(mu);
   mir_type_t t_bool = mir_bool_type(mu);

   const int nlits = type_enum_literals(elem);
   mir_value_t *map LOCAL = xmalloc_array(nlits, sizeof(mir_value_t));
   for (int i = 0; i < nlits; i++) {
      const ident_t id = tree_ident(type_enum_literal(elem, i));
      assert(ident_char(id, 0) == '\'');
      map[i] = mir_const(mu, t_char, ident_char(id, 1));
   }

   mir_type_t t_map = mir_carray_type(mu, nlits, t_char);
   mir_value_t map_array = mir_const_array(mu, t_map, map, nlits);

   mir_value_t zero = mir_const(mu, t_offset, 0);
   mir_value_t one = mir_const(mu, t_offset, 1);
   mir_value_t two = mir_const(mu, t_offset, 2);

   mir_value_t length = mir_build_uarray_len(mu, arg, 0);
   mir_value_t data_ptr = mir_build_unwrap(mu, arg);

   mir_value_t total = length;
   if (quote)
      total = mir_build_add(mu, t_offset, length, two);

   mir_value_t mem = mir_build_alloc(mu, t_char, MIR_NULL_STAMP, total);

   mir_value_t i_var = mir_add_var(mu, t_offset, MIR_NULL_STAMP,
                                   ident_new("i"), MIR_VAR_TEMP);
   mir_build_store(mu, i_var, zero);

   if (quote) {
      mir_value_t lquote_ptr = mir_build_array_ref(mu, mem, zero);
      mir_build_store(mu, lquote_ptr, mir_const(mu, t_char, '"'));
   }

   mir_value_t null_reg = mir_build_cmp(mu, MIR_CMP_EQ, length, zero);

   mir_block_t body_bb = mir_add_block(mu);
   mir_block_t exit_bb = mir_add_block(mu);

   mir_build_cond(mu, null_reg, exit_bb, body_bb);

   mir_set_cursor(mu, body_bb, MIR_APPEND);

   mir_value_t i_val   = mir_build_load(mu, i_var);
   mir_value_t sptr    = mir_build_array_ref(mu, data_ptr, i_val);
   mir_value_t src     = mir_build_load(mu, sptr);
   mir_value_t off     = mir_build_cast(mu, t_offset, src);
   mir_value_t map_ptr = mir_build_address_of(mu, map_array);
   mir_value_t lptr    = mir_build_array_ref(mu, map_ptr, off);

   mir_value_t doff = i_val;
   if (quote)
      doff = mir_build_add(mu, t_offset, doff, one);

   mir_value_t dptr = mir_build_array_ref(mu, mem, doff);

   mir_build_store(mu, dptr, mir_build_load(mu, lptr));

   mir_value_t next = mir_build_add(mu, t_offset, i_val, one);
   mir_value_t cmp  = mir_build_cmp(mu, MIR_CMP_EQ, next, length);
   mir_build_store(mu, i_var, next);
   mir_build_cond(mu, cmp, exit_bb, body_bb);

   mir_set_cursor(mu, exit_bb, MIR_APPEND);

   if (quote) {
      mir_value_t right = mir_build_add(mu, t_offset, length, one);
      mir_value_t rquote_ptr = mir_build_array_ref(mu, mem, right);
      mir_build_store(mu, rquote_ptr, mir_const(mu, t_char, '"'));
   }

   mir_dim_t dims[] = {
      {
         .left  = one,
         .right = total,
         .dir   = mir_const(mu, t_bool, RANGE_TO)
      }
   };
   mir_build_return(mu, mir_build_wrap(mu, mem, dims, 1));
}

void vhdl_lower_image_helper(mir_unit_t *mu, object_t *obj)
{
   tree_t decl = tree_from_object(obj);

   type_t type = type_base_recur(tree_type(decl));
   assert(type_is_representable(type));

   mir_set_result(mu, mir_string_type(mu));

   const type_info_t *ti = type_info(mu, type);
   mir_type_t t_param = ti->kind == T_RECORD
      ? mir_pointer_type(mu, ti->type) : ti->type;
   mir_value_t arg = mir_add_param(mu, t_param, ti->stamp, ident_new("VAL"));

   switch (type_kind(type)) {
   case T_ENUM:
      enum_image_helper(mu, type, arg);
      break;
   case T_INTEGER:
   case T_REAL:
      numeric_image_helper(mu, type, arg);
      break;
   case T_PHYSICAL:
      physical_image_helper(mu, type, arg);
      break;
   case T_RECORD:
      record_image_helper(mu, type, arg);
      break;
   case T_ARRAY:
      {
         type_t elem = type_elem(type);
         if (type_is_enum(elem) && all_character_literals(elem))
            character_array_image_helper(mu, type, arg, true);
         else
            array_image_helper(mu, type, arg);
      }
      break;
   default:
      fatal_trace("cannot lower image helper for type %s", type_pp(type));
   }

   mir_optimise(mu, MIR_PASS_O0);
}
