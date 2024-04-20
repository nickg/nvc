//
//  Copyright (C) 2023-2024  Nick Gasson
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
#include "hash.h"
#include "lib.h"
#include "lower.h"
#include "tree.h"
#include "vcode.h"
#include "vlog/vlog-defs.h"
#include "vlog/vlog-node.h"
#include "vlog/vlog-number.h"
#include "vlog/vlog-phase.h"
#include "vlog/vlog-util.h"

#include <assert.h>
#include <string.h>
#include <stdlib.h>

#define CANNOT_HANDLE(v) do {                                           \
      fatal_at(vlog_loc(v), "cannot handle %s in %s" ,                  \
               vlog_kind_str(vlog_kind(v)), __FUNCTION__);              \
   } while (0)

#define PUSH_DEBUG_INFO(v)                               \
   __attribute__((cleanup(emit_debug_info), unused))     \
   const loc_t _old_loc = *vcode_last_loc();             \
   emit_debug_info(vlog_loc((v)));                       \

static void vlog_lower_stmts(lower_unit_t *lu, vlog_node_t v);
static vcode_reg_t vlog_lower_rvalue(lower_unit_t *lu, vlog_node_t v);

static inline vcode_type_t vlog_logic_type(void)
{
   return vtype_int(0, 3);
}

static inline vcode_type_t vlog_net_value_type(void)
{
   return vtype_int(0, 16);
}

static inline vcode_type_t vlog_packed_logic_type(void)
{
   vcode_type_t vlogic = vlog_logic_type();
   return vtype_uarray(1, vlogic, vlogic);
}

#if 0
static vcode_reg_t vlog_debug_locus(vlog_node_t v)
{
   ident_t unit;
   ptrdiff_t offset;
   vlog_locus(v, &unit, &offset);

   return emit_debug_locus(unit, offset);
}
#endif

static vcode_reg_t vlog_helper_package(void)
{
   return emit_link_package(ident_new("NVC.VERILOG"));
}

static vcode_reg_t vlog_lower_wrap(lower_unit_t *lu, vcode_reg_t reg)
{
   vcode_type_t voffset = vtype_offset();
   vcode_type_t regtype = vcode_reg_type(reg);
   vcode_reg_t left_reg = emit_const(voffset, 0), right_reg, data_reg;

   switch (vtype_kind(regtype)) {
   case VCODE_TYPE_CARRAY:
      data_reg = emit_address_of(reg);
      right_reg = emit_const(voffset, vtype_size(regtype) - 1);
      break;
   case VCODE_TYPE_INT:
      {
         ident_t name = ident_uniq("wrap_temp");
         vcode_var_t tmp = emit_var(regtype, regtype, name, VAR_TEMP);
         emit_store(reg, tmp);

         data_reg = emit_index(tmp, VCODE_INVALID_REG);
         right_reg = left_reg;
      }
      break;
   case VCODE_TYPE_UARRAY:
      return reg;
   default:
      vcode_dump();
      fatal_trace("cannot wrap r%d", reg);
   }

   vcode_reg_t dir_reg = emit_const(vtype_bool(), RANGE_TO);

   vcode_dim_t dims[1] = {
      { left_reg, right_reg, dir_reg }
   };
   return emit_wrap(data_reg, dims, 1);
}

static vcode_reg_t vlog_lower_resize(lower_unit_t *lu, vcode_reg_t value_reg,
                                     vcode_reg_t count_reg)
{
   vcode_reg_t arg_reg;
   ident_t fn;
   switch (vcode_reg_kind(value_reg)) {
   case VCODE_TYPE_UARRAY:
   case VCODE_TYPE_CARRAY:
      arg_reg = vlog_lower_wrap(lu, value_reg);
      fn = ident_new("NVC.VERILOG.RESIZE(" T_PACKED_LOGIC "N)" T_PACKED_LOGIC);
      break;
   case VCODE_TYPE_INT:
      arg_reg = value_reg;
      fn = ident_new("NVC.VERILOG.RESIZE(" T_LOGIC "N)" T_PACKED_LOGIC);
      break;
   default:
      vcode_dump();
      fatal_trace("cannot resize r%d", value_reg);
   }

   vcode_type_t vlogic = vlog_logic_type();
   vcode_type_t vpacked = vlog_packed_logic_type();

   vcode_reg_t context_reg = vlog_helper_package();
   vcode_reg_t args[] = { context_reg, arg_reg, count_reg };
   return emit_fcall(fn, vpacked, vlogic, args, ARRAY_LEN(args));
}

static vcode_reg_t vlog_lower_to_time(lower_unit_t *lu, vcode_reg_t reg)
{
   vcode_reg_t context_reg = vlog_helper_package();
   vcode_reg_t wrap_reg = vlog_lower_wrap(lu, reg);
   vcode_reg_t args[] = { context_reg, wrap_reg };
   vcode_type_t vtime = vtype_time();
   ident_t func = ident_new("NVC.VERILOG.TO_TIME(" T_PACKED_LOGIC ")"
                            "25STD.STANDARD.DELAY_LENGTH");
   return emit_fcall(func, vtime, vtime, args, ARRAY_LEN(args));
}

static vcode_reg_t vlog_lower_to_integer(lower_unit_t *lu, vcode_reg_t reg)
{
   vcode_reg_t context_reg = vlog_helper_package();
   vcode_reg_t wrap_reg = vlog_lower_wrap(lu, reg);
   vcode_reg_t args[] = { context_reg, wrap_reg };
   vcode_type_t vint64 = vtype_int(INT64_MIN, INT64_MAX);
   ident_t func = ident_new("NVC.VERILOG.TO_INTEGER("
                            T_PACKED_LOGIC ")" T_INT64);
   return emit_fcall(func, vint64, vint64, args, ARRAY_LEN(args));
}

static vcode_reg_t vlog_lower_to_offset(lower_unit_t *lu, vcode_reg_t reg)
{
   vcode_type_t voffset = vtype_offset();
   vcode_reg_t int_reg = vlog_lower_to_integer(lu, reg);
   return emit_cast(voffset, voffset, int_reg);
}

static vcode_reg_t vlog_lower_to_bool(lower_unit_t *lu, vcode_reg_t reg)
{
   switch (vcode_reg_kind(reg)) {
   case VCODE_TYPE_INT:
      if (vtype_eq(vcode_reg_type(reg), vtype_bool()))
         return reg;
      else {
         vcode_type_t vlogic = vlog_logic_type();
         vcode_reg_t one_reg = emit_const(vlogic, LOGIC_1);
         assert(vtype_eq(vcode_reg_type(reg), vlogic));
         return emit_cmp(VCODE_CMP_EQ, reg, one_reg);
      }
   default:
      vcode_dump();
      fatal_trace("cannot convert r%d to bool", reg);
   }
}

static vcode_reg_t vlog_lower_to_logic(lower_unit_t *lu, vcode_reg_t reg)
{
   vcode_type_t vlogic = vlog_logic_type();

   switch (vcode_reg_kind(reg)) {
   case VCODE_TYPE_INT:
      if (vtype_eq(vcode_reg_type(reg), vlogic))
         return reg;
      else {
         vcode_reg_t context_reg = vlog_helper_package();
         vcode_reg_t args[] = { context_reg, reg };
         ident_t func = ident_new("NVC.VERILOG.TO_LOGIC("
                                  T_NET_VALUE ")" T_LOGIC);
         return emit_fcall(func, vlogic, vlogic, args, ARRAY_LEN(args));
      }
   case VCODE_TYPE_UARRAY:
      {
         vcode_type_t vpacked = vlog_packed_logic_type();
         vcode_reg_t context_reg = vlog_helper_package();
         vcode_reg_t args[] = { context_reg, reg };
         ident_t func = ident_new("NVC.VERILOG.TO_LOGIC("
                                  T_NET_ARRAY ")" T_PACKED_LOGIC);
         return emit_fcall(func, vpacked, vlogic, args, ARRAY_LEN(args));
      }
   default:
      vcode_dump();
      fatal_trace("cannot convert r%d to logic", reg);
   }
}

static vcode_reg_t vlog_lower_integer(lower_unit_t *lu, vlog_node_t expr)
{
   if (vlog_kind(expr) == V_NUMBER) {
      number_t n = vlog_number(expr);
      if (number_is_defined(n)) {
         vcode_type_t vint64 = vtype_int(INT64_MIN, INT64_MAX);
         return emit_const(vint64, number_integer(n));
      }
   }

   return vlog_lower_to_integer(lu, vlog_lower_rvalue(lu, expr));
}

static vcode_reg_t vlog_lower_decl_bounds(lower_unit_t *lu, vlog_node_t decl,
                                          vcode_reg_t data)
{
   const int nranges = vlog_ranges(decl);
   vcode_dim_t *dims LOCAL = xmalloc_array(nranges, sizeof(vcode_dim_t));

   for (int i = 0; i < nranges; i++) {
      vlog_node_t r = vlog_range(decl, i);

      vcode_reg_t left_reg = vlog_lower_integer(lu, vlog_left(r));
      vcode_reg_t right_reg = vlog_lower_integer(lu, vlog_right(r));

      vcode_reg_t dir_reg = emit_cmp(VCODE_CMP_GT, left_reg, right_reg);

      dims[i].left = left_reg;
      dims[i].right = right_reg;
      dims[i].dir = dir_reg;
   };

   return emit_wrap(data, dims, nranges);
}

static vcode_reg_t vlog_lower_lvalue_ref(lower_unit_t *lu, vlog_node_t ref)
{
   vlog_node_t decl = vlog_ref(ref);
   if (vlog_kind(decl) == V_PORT_DECL)
      decl = vlog_ref(decl);

   int hops;
   vcode_var_t var = lower_search_vcode_obj(decl, lu, &hops);
   assert(var != VCODE_INVALID_VAR);

   vcode_reg_t nets_reg;
   if (hops == 0)
      nets_reg = emit_load(var);
   else
      nets_reg = emit_load_indirect(emit_var_upref(hops, var));

   if (vcode_reg_kind(nets_reg) != VCODE_TYPE_UARRAY && vlog_ranges(decl) > 0)
      return vlog_lower_decl_bounds(lu, decl, nets_reg);
   else
      return nets_reg;
}

static vcode_reg_t vlog_lower_lvalue(lower_unit_t *lu, vlog_node_t v)
{
   PUSH_DEBUG_INFO(v);

   switch (vlog_kind(v)) {
   case V_REF:
      return vlog_lower_lvalue_ref(lu, v);
   case V_BIT_SELECT:
      {
         vcode_reg_t wrap_reg = vlog_lower_lvalue_ref(lu, v);
         vcode_reg_t nets_reg = emit_unwrap(wrap_reg);

         assert(vlog_params(v) == 1);
         vcode_reg_t p0_reg = vlog_lower_rvalue(lu, vlog_param(v, 0));
         vcode_reg_t off_reg = vlog_lower_to_offset(lu, p0_reg);

         return emit_array_ref(nets_reg, off_reg);
      }
   default:
      CANNOT_HANDLE(v);
   }
}

static vcode_reg_t vlog_lower_unary(lower_unit_t *lu, vlog_unary_t op,
                                    vcode_reg_t reg)
{
   LOCAL_TEXT_BUF tb = tb_new();
   tb_cat(tb, "NVC.VERILOG.");

   switch (op) {
   case V_UNARY_BITNEG:
   case V_UNARY_NOT:
      tb_cat(tb, "\"not\"(");
      break;
   }

   vcode_type_t vlogic = vlog_logic_type();
   vcode_type_t vpacked = vlog_packed_logic_type();

   vcode_type_t rtype;
   if (vcode_reg_kind(reg) == VCODE_TYPE_INT) {
      tb_cat(tb, T_LOGIC ")" T_LOGIC);
      rtype = vlogic;
   }
   else {
      reg = vlog_lower_wrap(lu, reg);
      tb_cat(tb, T_PACKED_LOGIC ")");

      if (op == V_UNARY_NOT) {
         tb_cat(tb, T_LOGIC);
         rtype = vlogic;
      }
      else {
         tb_cat(tb, T_PACKED_LOGIC);
         rtype = vpacked;
      }
   }

   vcode_reg_t context_reg = vlog_helper_package();

   ident_t func = ident_new(tb_get(tb));

   vcode_reg_t args[] = { context_reg, reg };
   return emit_fcall(func, rtype, vlogic, args, ARRAY_LEN(args));
}

static vcode_reg_t vlog_lower_binary(lower_unit_t *lu, vlog_binary_t op,
                                     vcode_reg_t left_reg,
                                     vcode_reg_t right_reg)
{
   LOCAL_TEXT_BUF tb = tb_new();
   tb_cat(tb, "NVC.VERILOG.");

   switch (op) {
   case V_BINARY_AND:
      tb_cat(tb, "\"and\"(");
      break;
   case V_BINARY_OR:
      tb_cat(tb, "\"or\"(");
      break;
   case V_BINARY_CASE_EQ:
   case V_BINARY_LOG_EQ:
      tb_cat(tb, "\"=\"(");
      break;
   case V_BINARY_CASE_NEQ:
   case V_BINARY_LOG_NEQ:
      tb_cat(tb, "\"/=\"(");
      break;
   case V_BINARY_PLUS:
      tb_cat(tb, "\"+\"(");
      break;
   case V_BINARY_MINUS:
      tb_cat(tb, "\"-\"(");
      break;
   }

   tb_cat(tb, T_PACKED_LOGIC T_PACKED_LOGIC ")");

   vcode_type_t rtype;
   switch (op) {
   case V_BINARY_CASE_EQ:
   case V_BINARY_CASE_NEQ:
      rtype = vtype_bool();
      tb_cat(tb, "B");
      break;
   case V_BINARY_LOG_EQ:
   case V_BINARY_LOG_NEQ:
      rtype = vlog_logic_type();
      tb_cat(tb, T_LOGIC);
      break;
   default:
      rtype = vlog_packed_logic_type();
      tb_cat(tb, T_PACKED_LOGIC);
      break;
   }

   ident_t func = ident_new(tb_get(tb));

   vcode_reg_t context_reg = vlog_helper_package();

   vcode_reg_t args[] = {
      context_reg,
      vlog_lower_wrap(lu, left_reg),
      vlog_lower_wrap(lu, right_reg)
   };

   return emit_fcall(func, rtype, rtype, args, ARRAY_LEN(args));
}

static vcode_reg_t vlog_lower_sysfunc(lower_unit_t *lu, vlog_node_t v)
{
   const v_sysfunc_kind_t kind = vlog_subkind(v);
   static const char *fns[] = {
      "NVC.VERILOG.SYS_TIME()" T_PACKED_LOGIC
   };
   assert(kind < ARRAY_LEN(fns));

   vcode_reg_t context_reg = vlog_helper_package();

   switch (kind) {
   case V_SYS_TIME:
      {
         vcode_type_t vlogic = vlog_logic_type();
         vcode_type_t vpacked = vlog_packed_logic_type();
         vcode_reg_t args[] = { context_reg };
         return emit_fcall(ident_new(fns[kind]), vpacked, vlogic,
                           args, ARRAY_LEN(args));
      }

   default:
      CANNOT_HANDLE(v);
   }
}

static vcode_reg_t vlog_lower_rvalue(lower_unit_t *lu, vlog_node_t v)
{
   PUSH_DEBUG_INFO(v);

   switch (vlog_kind(v)) {
   case V_REF:
      {
         vlog_node_t decl = vlog_ref(v);
         if (vlog_kind(decl) == V_PORT_DECL)
            decl = vlog_ref(decl);

         int hops;
         vcode_var_t var = lower_search_vcode_obj(decl, lu, &hops);
         assert(var != VCODE_INVALID_VAR);

         vcode_reg_t nets_reg;
         if (hops == 0)
            nets_reg = emit_load(var);
         else
            nets_reg = emit_load_indirect(emit_var_upref(hops, var));

         vcode_reg_t resolved_reg;
         if (vcode_reg_kind(nets_reg) == VCODE_TYPE_UARRAY) {
            vcode_reg_t data_reg = emit_resolved(emit_unwrap(nets_reg));

            // XXX: add a rewrap opcode
            vcode_dim_t dims[1] = {
               { .left = emit_uarray_left(nets_reg, 0),
                 .right = emit_uarray_right(nets_reg, 0),
                 .dir = emit_uarray_dir(nets_reg, 0) },
            };
            resolved_reg = emit_wrap(data_reg, dims, 1);
         }
         else {
            vcode_reg_t data_reg = emit_resolved(nets_reg);
            if (vlog_ranges(decl) > 0)
               resolved_reg = vlog_lower_decl_bounds(lu, decl, data_reg);
            else
               resolved_reg = emit_load_indirect(data_reg);
         }

         if (vlog_is_net(decl))
            return vlog_lower_to_logic(lu, resolved_reg);
         else
            return resolved_reg;
      }
   case V_EVENT:
      {
         vlog_node_t value = vlog_value(v);
         vcode_reg_t nets_reg = vlog_lower_lvalue(lu, value);

         vcode_reg_t count_reg = emit_const(vtype_offset(), 1);
         vcode_reg_t event_reg = emit_event_flag(nets_reg, count_reg);

         const v_event_kind_t ekind = vlog_subkind(v);
         if (ekind == V_EVENT_LEVEL)
            return event_reg;

         vcode_type_t vlogic = vlog_logic_type();

         const vlog_logic_t level =
            ekind == V_EVENT_POSEDGE ? LOGIC_1 : LOGIC_0;

         vcode_reg_t const_reg = emit_const(vlogic, level);
         vcode_reg_t value_reg = vlog_lower_rvalue(lu, value);
         vcode_reg_t cmp_reg = emit_cmp(VCODE_CMP_EQ, value_reg, const_reg);

         return emit_and(event_reg, cmp_reg);
      }
   case V_STRING:
      {
         const char *text = vlog_text(v);
         size_t len = strlen(text) + 1;
         vcode_reg_t *chars LOCAL = xmalloc_array(len, sizeof(vcode_reg_t));
         vcode_type_t vchar = vtype_char();

         for (int i = 0; i < len; i++)
            chars[i] = emit_const(vchar, text[i]);

         vcode_type_t vtype = vtype_carray(len, vchar, vchar);
         return emit_const_array(vtype, chars, len);
      }
   case V_NUMBER:
      {
         vcode_type_t vlogic = vlog_logic_type();

         number_t num = vlog_number(v);
         const int width = number_width(num);

         if (width == 1) {
            abort();
         }
         else {
            vcode_reg_t *bits LOCAL = xmalloc_array(width, sizeof(vcode_reg_t));
            for (int i = 0; i < width; i++)
               bits[i] = emit_const(vlogic, number_bit(num, i));

            vcode_type_t varray = vtype_carray(width, vlogic, vlogic);
            return emit_const_array(varray, bits, width);
         }
      }
   case V_BINARY:
      {
         vcode_reg_t left_reg = vlog_lower_rvalue(lu, vlog_left(v));
         vcode_reg_t right_reg = vlog_lower_rvalue(lu, vlog_right(v));
         return vlog_lower_binary(lu, vlog_subkind(v), left_reg, right_reg);
      }
   case V_UNARY:
      {
         vcode_reg_t value_reg = vlog_lower_rvalue(lu, vlog_value(v));
         return vlog_lower_unary(lu, vlog_subkind(v), value_reg);
      }
   case V_STRENGTH:
      {
         static const int8_t map[] = {
            _SUPPLY0, _STRONG0, _PULL0, _WEAK0,
            _SUPPLY1, _STRONG1, _PULL1, _WEAK1
         };
         vcode_type_t vnet = vlog_net_value_type();
         return emit_const(vnet, map[vlog_subkind(v)]);
      }
   case V_SYSFUNC:
      return vlog_lower_sysfunc(lu, v);
   default:
      CANNOT_HANDLE(v);
   }
}

static void vlog_lower_sensitivity(lower_unit_t *lu, vlog_node_t v)
{
   switch (vlog_kind(v)) {
   case V_REF:
      {
         vcode_reg_t nets_reg = vlog_lower_lvalue(lu, v);
         vcode_reg_t count_reg = emit_const(vtype_offset(), 1);
         emit_sched_event(nets_reg, count_reg);
      }
      break;
   case V_EVENT:
      vlog_lower_sensitivity(lu, vlog_value(v));
      break;
   case V_NUMBER:
      break;
   case V_BINARY:
      vlog_lower_sensitivity(lu, vlog_left(v));
      vlog_lower_sensitivity(lu, vlog_right(v));
      break;
   case V_UNARY:
      vlog_lower_sensitivity(lu, vlog_value(v));
      break;
   default:
      CANNOT_HANDLE(v);
   }
}

static void vlog_lower_timing(lower_unit_t *lu, vlog_node_t v, bool is_static)
{
   vcode_block_t true_bb = emit_block(), false_bb = VCODE_INVALID_BLOCK;

   vlog_node_t ctrl = vlog_value(v);
   switch (vlog_kind(ctrl)) {
   case V_DELAY_CONTROL:
      {
         vcode_reg_t delay_reg = vlog_lower_rvalue(lu, vlog_value(ctrl));
         emit_wait(true_bb, vlog_lower_to_time(lu, delay_reg));

         vcode_select_block(true_bb);
      }
      break;
   case V_EVENT:
      {
         vcode_reg_t test_reg = vlog_lower_rvalue(lu, vlog_value(v));

         false_bb = emit_block();
         emit_cond(test_reg, true_bb, false_bb);

         vcode_select_block(true_bb);
      }
      break;
   default:
      CANNOT_HANDLE(ctrl);
   }

   vlog_lower_stmts(lu, v);

   if (false_bb != VCODE_INVALID_BLOCK) {
      if (!vcode_block_finished())
         emit_jump(false_bb);

      vcode_select_block(false_bb);
   }
}

static void vlog_lower_procedural_assign(lower_unit_t *lu, vlog_node_t v)
{
   if (vlog_kind(v) == V_BASSIGN && vlog_has_delay(v)) {
      vlog_node_t delay = vlog_delay(v);
      assert(vlog_kind(delay) == V_DELAY_CONTROL);

      vcode_block_t delay_bb = emit_block();
      vcode_reg_t delay_reg = vlog_lower_rvalue(lu, vlog_value(delay));
      vcode_reg_t time_reg = vlog_lower_to_time(lu, delay_reg);

      emit_wait(delay_bb, time_reg);

      vcode_select_block(delay_bb);
   }

   vlog_node_t target = vlog_target(v);

   vcode_reg_t target_reg = vlog_lower_lvalue(lu, target);
   vcode_reg_t value_reg = vlog_lower_rvalue(lu, vlog_value(v));

   vcode_reg_t count_reg, nets_reg;
   if (vcode_reg_kind(target_reg) == VCODE_TYPE_UARRAY) {
      nets_reg = emit_unwrap(target_reg);
      count_reg = emit_uarray_len(target_reg, 0);
   }
   else {
      nets_reg = target_reg;
      count_reg = emit_const(vtype_offset(), 1);
   }

   vcode_reg_t resize_reg = vlog_lower_resize(lu, value_reg, count_reg);

   vcode_reg_t data_reg;
   if (vlog_is_net(target)) {
      vcode_reg_t context_reg = vlog_helper_package();
      vcode_reg_t args[] = { context_reg, resize_reg };
      vcode_type_t vnet = vlog_net_value_type();
      vcode_type_t varray = vtype_uarray(1, vnet, vnet);
      ident_t func = ident_new("NVC.VERILOG.TO_NET_VALUE("
                               T_PACKED_LOGIC ")" T_NET_ARRAY);
      vcode_reg_t call_reg = emit_fcall(func, varray, vnet,
                                        args, ARRAY_LEN(args));
      data_reg = emit_unwrap(call_reg);
   }
   else
      data_reg = emit_unwrap(resize_reg);

   switch (vlog_kind(v)) {
   case V_NBASSIGN:
   case V_ASSIGN:
      {
         vcode_type_t vtime = vtype_time();
         vcode_reg_t reject_reg = emit_const(vtime, 0);

         vcode_reg_t after_reg;
         if (vlog_has_delay(v)) {
            vlog_node_t delay = vlog_delay(v);
            assert(vlog_kind(delay) == V_DELAY_CONTROL);

            vcode_reg_t delay_reg = vlog_lower_rvalue(lu, vlog_value(delay));
            after_reg = vlog_lower_to_time(lu, delay_reg);
         }
         else
            after_reg = emit_const(vtime, 0);

         emit_sched_waveform(nets_reg, count_reg, data_reg,
                             reject_reg, after_reg);
      }
      break;
   case V_BASSIGN:
      {
         emit_deposit_signal(nets_reg, count_reg, data_reg);

         // Delay one delta cycle to see the update

         vcode_type_t vtime = vtype_time();
         vcode_reg_t zero_time_reg = emit_const(vtime, 0);

         vcode_block_t resume_bb = emit_block();
         emit_wait(resume_bb, zero_time_reg);

         vcode_select_block(resume_bb);
      }
      break;
   default:
      CANNOT_HANDLE(v);
   }
}

static void vlog_lower_systask(lower_unit_t *lu, vlog_node_t v)
{
   const v_systask_kind_t kind = vlog_subkind(v);
   static const char *fns[] = {
      "NVC.VERILOG.SYS_DISPLAY(S)",
      "NVC.VERILOG.SYS_WRITE(S)",
      "NVC.VERILOG.SYS_FINISH"
   };
   assert(kind < ARRAY_LEN(fns));

   vcode_reg_t context_reg = vlog_helper_package();

   switch (kind) {
   case V_SYS_DISPLAY:
   case V_SYS_WRITE:
      {
         const int nparams = vlog_params(v);
         vcode_reg_t *args LOCAL =
            xmalloc_array(nparams + 1, sizeof(vcode_reg_t));
         args[0] = context_reg;
         for (int i = 0; i < nparams; i++) {
            vcode_reg_t p_reg = vlog_lower_rvalue(lu, vlog_param(v, i));
            args[i + 1] = vlog_lower_wrap(lu, p_reg);
         }

         emit_fcall(ident_new(fns[kind]), VCODE_INVALID_TYPE,
                    VCODE_INVALID_TYPE, args, nparams + 1);
      }
      break;

   case V_SYS_FINISH:
      {
         vcode_reg_t args[] = { context_reg };
         emit_fcall(ident_new(fns[kind]), VCODE_INVALID_TYPE,
                    VCODE_INVALID_TYPE, args, ARRAY_LEN(args));
      }
      break;

   default:
      CANNOT_HANDLE(v);
   }
}

static void vlog_lower_if(lower_unit_t *lu, vlog_node_t v)
{
   vcode_block_t true_bb = emit_block();
   vcode_block_t false_bb = emit_block(), skip_bb = false_bb;

   const int nconds = vlog_conds(v);
   assert(nconds == 1 || nconds == 2);

   if (nconds == 2)
      skip_bb = emit_block();

   vlog_node_t c0 = vlog_cond(v, 0);

   vcode_reg_t test_reg = vlog_lower_rvalue(lu, vlog_value(c0));
   vcode_reg_t bool_reg = vlog_lower_to_bool(lu, test_reg);
   emit_cond(bool_reg, true_bb, false_bb);

   vcode_select_block(true_bb);

   vlog_lower_stmts(lu, c0);

   if (!vcode_block_finished())
      emit_jump(skip_bb);

   if (nconds == 2) {
      vlog_node_t c1 = vlog_cond(v, 1);
      assert(!vlog_has_value(c1));

      vcode_select_block(false_bb);

      vlog_lower_stmts(lu, c1);

      if (!vcode_block_finished())
         emit_jump(skip_bb);
   }

   vcode_select_block(skip_bb);
}

static void vlog_lower_stmts(lower_unit_t *lu, vlog_node_t v)
{
   const int nstmts = vlog_stmts(v);
   for (int i = 0; i < nstmts; i++) {
      vlog_node_t s = vlog_stmt(v, i);
      emit_debug_info(vlog_loc(s));

      switch (vlog_kind(s)) {
      case V_TIMING:
         vlog_lower_timing(lu, s, false);
         break;
      case V_NBASSIGN:
      case V_BASSIGN:
         vlog_lower_procedural_assign(lu, s);
         break;
      case V_SEQ_BLOCK:
         vlog_lower_stmts(lu, s);
         break;
      case V_SYSTASK:
         vlog_lower_systask(lu, s);
         break;
      case V_IF:
         vlog_lower_if(lu, s);
         break;
      default:
         CANNOT_HANDLE(s);
      }
   }
}

static void vlog_lower_driver(lower_unit_t *lu, vlog_node_t v)
{
   vcode_reg_t target_reg = vlog_lower_lvalue(lu, v);

   vcode_reg_t nets_reg, count_reg;
   if (vcode_reg_kind(target_reg) == VCODE_TYPE_UARRAY) {
      nets_reg = emit_unwrap(target_reg);
      count_reg = emit_uarray_len(target_reg, 0);
   }
   else {
      nets_reg = target_reg;
      count_reg = emit_const(vtype_offset(), 1);
   }

   emit_drive_signal(nets_reg, count_reg);
}

static void vlog_driver_cb(vlog_node_t v, void *context)
{
   lower_unit_t *lu = context;

   switch (vlog_kind(v)) {
   case V_NBASSIGN:
   case V_ASSIGN:
      vlog_lower_driver(lu, vlog_target(v));
      break;
   default:
      break;
   }
}

static void vlog_lower_always(unit_registry_t *ur, lower_unit_t *parent,
                              vlog_node_t stmt)
{
   vcode_unit_t context = get_vcode(parent);

   ident_t name = ident_prefix(vcode_unit_name(context), vlog_ident(stmt), '.');
   vcode_unit_t vu = emit_process(name, vlog_to_object(stmt), context);

   vcode_block_t start_bb = emit_block();
   assert(start_bb == 1);

   lower_unit_t *lu = lower_unit_new(ur, parent, vu, NULL, NULL);
   unit_registry_put(ur, lu);

   vlog_visit(stmt, vlog_driver_cb, lu);

   vlog_node_t timing = NULL, s0 = vlog_stmt(stmt, 0);
   if (vlog_kind(s0) == V_TIMING) {
      timing = s0;
      vlog_lower_sensitivity(lu, vlog_value(timing));
   }

   emit_return(VCODE_INVALID_REG);

   vcode_select_block(start_bb);

   if (timing != NULL)
      vlog_lower_timing(lu, timing, true);
   else
      vlog_lower_stmts(lu, stmt);

   emit_wait(start_bb, VCODE_INVALID_REG);

   unit_registry_finalise(ur, lu);
}

static void vlog_lower_initial(unit_registry_t *ur, lower_unit_t *parent,
                               vlog_node_t stmt)
{
   vcode_unit_t context = get_vcode(parent);

   ident_t name = ident_prefix(vcode_unit_name(context), vlog_ident(stmt), '.');
   vcode_unit_t vu = emit_process(name, vlog_to_object(stmt), context);

   vcode_block_t start_bb = emit_block();
   assert(start_bb == 1);

   lower_unit_t *lu = lower_unit_new(ur, parent, vu, NULL, NULL);
   unit_registry_put(ur, lu);

   vlog_visit(stmt, vlog_driver_cb, lu);

   emit_return(VCODE_INVALID_REG);

   vcode_select_block(start_bb);

   vlog_lower_stmts(lu, stmt);

   emit_wait(start_bb, VCODE_INVALID_REG);

   unit_registry_finalise(ur, lu);
}

static void vlog_lower_continuous_assign(unit_registry_t *ur,
                                         lower_unit_t *parent,
                                         vlog_node_t stmt)
{
   vcode_unit_t context = get_vcode(parent);

   ident_t name = ident_prefix(vcode_unit_name(context), vlog_ident(stmt), '.');
   vcode_unit_t vu = emit_process(name, vlog_to_object(stmt), context);

   vcode_block_t start_bb = emit_block();
   assert(start_bb == 1);

   lower_unit_t *lu = lower_unit_new(ur, parent, vu, NULL, NULL);
   unit_registry_put(ur, lu);

   vlog_visit(stmt, vlog_driver_cb, lu);

   vlog_lower_sensitivity(lu, vlog_value(stmt));

   emit_return(VCODE_INVALID_REG);

   vcode_select_block(start_bb);

   vlog_lower_procedural_assign(lu, stmt);

   emit_wait(start_bb, VCODE_INVALID_REG);

   unit_registry_finalise(ur, lu);
}

static void vlog_lower_gate_inst(unit_registry_t *ur, lower_unit_t *parent,
                                 vlog_node_t stmt)
{
   vcode_unit_t context = get_vcode(parent);

   ident_t name = ident_prefix(vcode_unit_name(context), vlog_ident(stmt), '.');
   vcode_unit_t vu = emit_process(name, vlog_to_object(stmt), context);

   vcode_block_t start_bb = emit_block();
   assert(start_bb == 1);

   lower_unit_t *lu = lower_unit_new(ur, parent, vu, NULL, NULL);
   unit_registry_put(ur, lu);

   vlog_lower_driver(lu, vlog_target(stmt));

   emit_return(VCODE_INVALID_REG);

   vcode_select_block(start_bb);

   vcode_reg_t value_reg = 0;
   if (vlog_params(stmt) == 0) {
      const vlog_gate_kind_t kind = vlog_subkind(stmt);
      vcode_type_t vnet = vlog_net_value_type();
      value_reg = emit_const(vnet, kind == V_GATE_PULLUP ? _PULL1 : _PULL0);
   }
   else
      value_reg = vlog_lower_rvalue(lu, vlog_param(stmt, 0));

   vcode_type_t vtime = vtype_time();
   vcode_reg_t reject_reg = emit_const(vtime, 0);
   vcode_reg_t after_reg = emit_const(vtime, 0);

   vcode_reg_t nets_reg = vlog_lower_lvalue(lu, vlog_target(stmt));
   vcode_reg_t count_reg = emit_const(vtype_offset(), 1);

   emit_sched_waveform(nets_reg, count_reg, value_reg, reject_reg, after_reg);

   emit_wait(start_bb, VCODE_INVALID_REG);

   unit_registry_finalise(ur, lu);
}

static void vlog_lower_concurrent(unit_registry_t *ur, lower_unit_t *parent,
                                  vlog_node_t scope)
{
   const int nstmts = vlog_stmts(scope);
   for (int i = 0; i < nstmts; i++) {
      vlog_node_t s = vlog_stmt(scope, i);
      switch (vlog_kind(s)) {
      case V_ALWAYS:
         vlog_lower_always(ur, parent, s);
         break;
      case V_INITIAL:
         vlog_lower_initial(ur, parent, s);
         break;
      case V_ASSIGN:
         vlog_lower_continuous_assign(ur, parent, s);
         break;
      case V_GATE_INST:
         vlog_lower_gate_inst(ur, parent, s);
         break;
      case V_MOD_INST:
         break;
      default:
         CANNOT_HANDLE(s);
      }
   }
}

vcode_unit_t vlog_lower(unit_registry_t *ur, vlog_node_t mod)
{
   assert(vlog_kind(mod) == V_MODULE);

   ident_t name = ident_prefix(vlog_ident(mod), well_known(W_SHAPE), '.');

   vcode_unit_t vu = emit_shape(name, vlog_to_object(mod), NULL);

   lower_unit_t *lu = lower_unit_new(ur, NULL, vu, NULL, NULL);
   unit_registry_put(ur, lu);

   vcode_type_t vlogic = vlog_logic_type();
   vcode_type_t vnetvalue = vlog_net_value_type();
   vcode_type_t vlogicsignal = vtype_signal(vlogic);
   vcode_type_t vnetsignal = vtype_signal(vnetvalue);

   const int nports = vlog_ports(mod);
   for (int i = 0; i < nports; i++) {
      vlog_node_t ref = vlog_port(mod, i);
      assert(vlog_kind(ref) == V_REF);

      vlog_node_t port = vlog_ref(ref);
      assert(vlog_kind(port) == V_PORT_DECL);

      vcode_type_t vtype = vlog_is_net(port) ? vnetsignal : vlogicsignal;
      vcode_var_t var = emit_var(vtype, vtype, vlog_ident(port), VAR_SIGNAL);

      lower_put_vcode_obj(port, var, lu);
      lower_put_vcode_obj(vlog_ref(port), var, lu);
   }

   const int ndecls = vlog_decls(mod);
   for (int i = 0, hops; i < ndecls; i++) {
      vlog_node_t d = vlog_decl(mod, i);
      switch (vlog_kind(d)) {
      case V_PORT_DECL:
         break;   // Translated above
      case V_NET_DECL:
      case V_VAR_DECL:
         if (lower_search_vcode_obj(d, lu, &hops) == VCODE_INVALID_VAR) {
            vcode_type_t vtype = vlog_is_net(d) ? vnetsignal : vlogicsignal;
            vcode_var_t var = emit_var(vtype, vtype, vlog_ident(d), VAR_SIGNAL);
            lower_put_vcode_obj(d, var, lu);
         }
         break;
      default:
         CANNOT_HANDLE(d);
      }
   }

   emit_return(VCODE_INVALID_REG);

   lower_finished(lu, NULL);

   vlog_lower_concurrent(ur, lu, mod);

   unit_registry_finalise(ur, lu);
   return vu;
}
