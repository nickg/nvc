//
//  Copyright (C) 2023-2025  Nick Gasson
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
#include "mir/mir-unit.h"
#include "mir/mir-node.h"
#include "tree.h"
#include "vcode.h"
#include "vlog/vlog-defs.h"
#include "vlog/vlog-lower.h"
#include "vlog/vlog-node.h"
#include "vlog/vlog-number.h"
#include "vlog/vlog-phase.h"
#include "vlog/vlog-util.h"

#include <assert.h>
#include <inttypes.h>
#include <string.h>
#include <stdlib.h>

typedef struct {
   mir_type_t  type;
   unsigned    size;
   mir_value_t temp;
} type_info_t;

typedef struct {
   mir_value_t  nets;
   unsigned     size;
   type_info_t *base;
} vlog_lvalue_t;

STATIC_ASSERT(sizeof(vlog_lvalue_t) <= 16);

#define CANNOT_HANDLE(v) do {                                           \
      fatal_at(vlog_loc(v), "cannot handle %s in %s" ,                  \
               vlog_kind_str(vlog_kind(v)), __FUNCTION__);              \
   } while (0)

#define PUSH_DEBUG_INFO(mu, v)                                          \
   __attribute__((cleanup(_mir_pop_debug_info), unused))                \
   const mir_saved_loc_t _old_loc =                                     \
      _mir_push_debug_info((mu), vlog_loc((v)));

static void vlog_lower_stmts(mir_unit_t *mu, vlog_node_t v);
static mir_value_t vlog_lower_rvalue(mir_unit_t *mu, vlog_node_t v);

static inline vcode_type_t vlog_logic_type(void)
{
   return vtype_int(0, 3);
}

static inline vcode_type_t vlog_net_value_type(void)
{
   return vtype_int(0, 255);
}

static type_info_t *vlog_type_info(mir_unit_t *mu, vlog_node_t v)
{
   assert(vlog_kind(v) == V_DATA_TYPE);

   type_info_t *ti = mir_get_priv(mu, v);
   if (ti == NULL) {
      ti = mir_malloc(mu, sizeof(type_info_t));
      ti->size = vlog_size(v);
      ti->type = mir_vec4_type(mu, ti->size, false);
      ti->temp = MIR_NULL_VALUE;
   }

   return ti;
}

static mir_value_t vlog_get_temp(mir_unit_t *mu, type_info_t *ti)
{
   if (mir_is_null(ti->temp)) {
      mir_type_t t_array = mir_carray_type(mu, ti->size, mir_logic_type(mu));
      ti->temp = mir_add_var(mu, t_array, MIR_NULL_STAMP, ident_uniq("tmp"),
                             MIR_VAR_TEMP);
   }

   return ti->temp;
}

static vlog_lvalue_t vlog_lower_lvalue_ref(mir_unit_t *mu, vlog_node_t v)
{
   vlog_node_t decl = vlog_ref(v);
   if (vlog_kind(decl) == V_PORT_DECL)
      decl = vlog_ref(decl);

   int hops;
   mir_value_t var = mir_search_object(mu, decl, &hops);
   assert(!mir_is_null(var));

   assert(hops > 0);
   mir_value_t upref = mir_build_var_upref(mu, hops, var.id);

   type_info_t *ti = vlog_type_info(mu, vlog_type(decl));

   vlog_lvalue_t result = {
      .nets = mir_build_load(mu, upref),
      .size = ti->size,
      .base = ti,
   };
   return result;
}

static vlog_lvalue_t vlog_lower_lvalue(mir_unit_t *mu, vlog_node_t v)
{
   PUSH_DEBUG_INFO(mu, v);

   switch (vlog_kind(v)) {
   case V_REF:
      return vlog_lower_lvalue_ref(mu, v);
   case V_BIT_SELECT:
      {
         vlog_lvalue_t ref = vlog_lower_lvalue_ref(mu, vlog_value(v));

         assert(vlog_params(v) == 1);
         mir_value_t p0 = vlog_lower_rvalue(mu, vlog_param(v, 0));

         mir_type_t p0_type = mir_get_type(mu, p0);
         if (mir_get_class(mu, p0_type) == MIR_TYPE_VEC4) {
            // TODO: check X/Z handling
            mir_type_t t_vec2 =
               mir_vec2_type(mu, mir_get_size(mu, p0_type), false);
            p0 = mir_build_cast(mu, t_vec2, p0);
         }

         mir_type_t t_offset = mir_offset_type(mu);
         mir_value_t off = mir_build_cast(mu, t_offset, p0);

         vlog_lvalue_t lvalue = {
            .nets = mir_build_array_ref(mu, ref.nets, off),
            .size = 1,
            .base = ref.base,
         };
         return lvalue;
      }
   default:
      CANNOT_HANDLE(v);
   }
}

static void vlog_assign_variable(mir_unit_t *mu, vlog_node_t target,
                                 mir_value_t value)
{
   assert(mir_is_vector(mu, value));

   vlog_lvalue_t lvalue = vlog_lower_lvalue(mu, target);

   mir_type_t t_offset = mir_offset_type(mu);
   mir_type_t t_vec = mir_vec4_type(mu, lvalue.size, false);

   mir_value_t resize = mir_build_cast(mu, t_vec, value);
   mir_value_t count = mir_const(mu, t_offset, lvalue.size);

   mir_value_t tmp = MIR_NULL_VALUE;
   if (lvalue.size > 1)
      tmp = vlog_get_temp(mu, lvalue.base);

   mir_value_t unpacked = mir_build_unpack(mu, resize, 0, tmp);

   mir_build_deposit_signal(mu, lvalue.nets, count, unpacked);

   // Delay one delta cycle to see the update

   mir_value_t zero_time = mir_const(mu, mir_time_type(mu), 0);

   mir_block_t resume_bb = mir_add_block(mu);
   mir_build_wait(mu, resume_bb, zero_time);

   mir_set_cursor(mu, resume_bb, MIR_APPEND);
}

static mir_value_t vlog_lower_unary(mir_unit_t *mu, vlog_node_t v)
{
   mir_value_t input = vlog_lower_rvalue(mu, vlog_value(v));
   mir_type_t type = mir_get_type(mu, input);

   mir_vec_op_t mop;
   switch (vlog_subkind(v)) {
   case V_UNARY_BITNEG: mop = MIR_VEC_BIT_NOT; break;
   case V_UNARY_NOT:    mop = MIR_VEC_LOG_NOT; break;
   case V_UNARY_NEG:    mop = MIR_VEC_SUB; break;
   default:
      CANNOT_HANDLE(v);
   }

   return mir_build_unary(mu, mop, type, input);
}

static mir_value_t vlog_lower_binary(mir_unit_t *mu, vlog_node_t v)
{
   mir_value_t left = vlog_lower_rvalue(mu, vlog_left(v));
   mir_value_t right = vlog_lower_rvalue(mu, vlog_right(v));

   assert(mir_is_vector(mu, left));
   assert(mir_is_vector(mu, right));

   mir_type_t ltype = mir_get_type(mu, left);
   mir_type_t rtype = mir_get_type(mu, right);

   const mir_class_t lclass = mir_get_class(mu, ltype);
   const mir_class_t rclass = mir_get_class(mu, rtype);

   const int lsize = mir_get_size(mu, ltype);
   const int rsize = mir_get_size(mu, rtype);

   mir_type_t type;
   if (lclass == MIR_TYPE_VEC4 || rclass == MIR_TYPE_VEC4)
      type = mir_vec4_type(mu, MAX(lsize, rsize), false);
   else
      type = mir_vec2_type(mu, MAX(lsize, rsize), false);

   mir_value_t lcast = mir_build_cast(mu, type, left);
   mir_value_t rcast = mir_build_cast(mu, type, right);

   mir_vec_op_t mop;
   switch (vlog_subkind(v)) {
   case V_BINARY_AND:      mop = MIR_VEC_BIT_AND; break;
   case V_BINARY_OR:       mop = MIR_VEC_BIT_OR; break;
   case V_BINARY_LOG_AND:  mop = MIR_VEC_LOG_AND; break;
   case V_BINARY_LOG_OR:   mop = MIR_VEC_LOG_OR; break;
   case V_BINARY_LT:       mop = MIR_VEC_LT; break;
   case V_BINARY_LEQ:      mop = MIR_VEC_LEQ; break;
   case V_BINARY_GT:       mop = MIR_VEC_GT; break;
   case V_BINARY_GEQ:      mop = MIR_VEC_GEQ; break;
   case V_BINARY_LOG_EQ:   mop = MIR_VEC_LOG_EQ; break;
   case V_BINARY_LOG_NEQ:  mop = MIR_VEC_LOG_NEQ; break;
   case V_BINARY_CASE_EQ:  mop = MIR_VEC_CASE_EQ; break;
   case V_BINARY_CASE_NEQ: mop = MIR_VEC_CASE_NEQ; break;
   case V_BINARY_PLUS:     mop = MIR_VEC_ADD; break;
   case V_BINARY_MINUS:    mop = MIR_VEC_SUB; break;
   case V_BINARY_SHIFT_LL: mop = MIR_VEC_SLL; break;
   case V_BINARY_SHIFT_RL: mop = MIR_VEC_SRL; break;
   default:
      CANNOT_HANDLE(v);
   }

   return mir_build_binary(mu, mop, type, lcast, rcast);
}

static mir_value_t vlog_lower_systf_param(mir_unit_t *mu, vlog_node_t v)
{
   switch (vlog_kind(v)) {
   case V_REF:
   case V_STRING:
   case V_NUMBER:
   case V_EMPTY:
      return MIR_NULL_VALUE;
   case V_UNARY:
   case V_BINARY:
   case V_SYS_FCALL:
   case V_PREFIX:
   case V_POSTFIX:
      // TODO: these should not be evaluated until vpi_get_value is called
      return vlog_lower_rvalue(mu, v);
   default:
      CANNOT_HANDLE(v);
   }
}

static mir_value_t vlog_lower_sys_tfcall(mir_unit_t *mu, vlog_node_t v)
{
   const int nparams = vlog_params(v);
   mir_value_t *args LOCAL =
      xmalloc_array((nparams * 2) + 1, sizeof(mir_value_t));
   int actual = 0;
   mir_type_t t_offset = mir_offset_type(mu);
   for (int i = 0; i < nparams; i++) {
      mir_value_t arg = vlog_lower_systf_param(mu, vlog_param(v, i));
      if (mir_is_null(arg))
         continue;

      mir_type_t type = mir_get_type(mu, arg);
      switch (mir_get_class(mu, type)) {
      case MIR_TYPE_VEC4:
         args[actual++] = mir_const(mu, t_offset, mir_get_size(mu, type));
         args[actual++] = arg;
         break;
      case MIR_TYPE_VEC2:
         {
            // TODO: remove the cast
            const int size = mir_get_size(mu, type);
            mir_type_t t_vec4 = mir_vec4_type(mu, size, false);
            args[actual++] = mir_const(mu, t_offset, size);
            args[actual++] = mir_build_cast(mu, t_vec4, arg);
         }
         break;
      default:
         should_not_reach_here();
      }
   }

   mir_value_t locus = mir_build_locus(mu, vlog_to_object(v));

   mir_type_t type = MIR_NULL_TYPE;
   if (vlog_kind(v) == V_SYS_FCALL)
      type = mir_vec2_type(mu, 64, false);  // XXX: hack for $time

   return mir_build_syscall(mu, vlog_ident(v), type, MIR_NULL_STAMP,
                            locus, args, actual);
}

static mir_value_t vlog_lower_resolved(mir_unit_t *mu, vlog_node_t v)
{
   switch (vlog_kind(v)) {
   case V_PORT_DECL:
   case V_REF:
      return vlog_lower_resolved(mu, vlog_ref(v));
   case V_VAR_DECL:
   case V_NET_DECL:
      {
         int hops;
         mir_value_t var = mir_search_object(mu, v, &hops);
         assert(!mir_is_null(var));

         assert(hops > 0);
         mir_value_t upref = mir_build_var_upref(mu, hops, var.id);
         mir_value_t nets = mir_build_load(mu, upref);
         return mir_build_resolved(mu, nets);
      }
   default:
      CANNOT_HANDLE(v);
   }
}

static mir_value_t vlog_lower_array_off(mir_unit_t *mu, vlog_node_t dt,
                                        vlog_node_t v)
{
   mir_value_t index = vlog_lower_rvalue(mu, v);

   mir_type_t index_type = mir_get_type(mu, index);
   if (mir_get_class(mu, index_type) == MIR_TYPE_VEC4) {
      // TODO: check X/Z handling
      mir_type_t vec2 = mir_vec2_type(mu, mir_get_size(mu, index_type), false);
      index = mir_build_cast(mu, vec2, index);
   }

   mir_type_t t_offset = mir_offset_type(mu);
   mir_value_t cast = mir_build_cast(mu, t_offset, index);

   vlog_node_t r = vlog_range(dt, 0);
   assert(vlog_subkind(r) == V_DIM_PACKED);

   int64_t left, right;
   vlog_bounds(r, &left, &right);

   if (left < right)
      return mir_build_sub(mu, t_offset, cast, mir_const(mu, t_offset, left));
   else
      return mir_build_sub(mu, t_offset, mir_const(mu, t_offset, left), cast);
}

static mir_value_t vlog_lower_rvalue_bit_select(mir_unit_t *mu, vlog_node_t v)
{
   vlog_node_t value = vlog_value(v);
   assert(vlog_kind(value) == V_REF);

   mir_value_t data = vlog_lower_resolved(mu, value);

   vlog_node_t dt = vlog_type(vlog_ref(value));
   assert(vlog_ranges(dt) == 1);
   assert(vlog_params(v) == 1);

   mir_type_t t_offset = mir_offset_type(mu);
   mir_value_t off = vlog_lower_array_off(mu, dt, vlog_param(v, 0));

   mir_value_t cmp_low = mir_build_cmp(mu, MIR_CMP_GEQ, off,
                                       mir_const(mu, t_offset, 0));
   mir_value_t cmp_high = mir_build_cmp(mu, MIR_CMP_LT, off,
                                        mir_const(mu, t_offset, vlog_size(dt)));
   mir_value_t in_range = mir_build_and(mu, cmp_low, cmp_high);

   mir_type_t type = mir_vec4_type(mu, 1, false);

   int64_t in_range_const;
   if (mir_get_const(mu, in_range, &in_range_const)) {
      if (in_range_const) {
         mir_value_t ptr = mir_build_array_ref(mu, data, off);
         mir_value_t bit = mir_build_load(mu, ptr);
         return mir_build_pack(mu, type, bit);
      }
      else
         return mir_const_vec(mu, type, 1, 1);
   }
   else {
      // TODO: use a phi node here
      mir_value_t tmp = mir_add_var(mu, type, MIR_NULL_STAMP,
                                    ident_uniq("tmp"), MIR_VAR_TEMP);
      mir_build_store(mu, tmp, mir_const_vec(mu, type, 1, 1));

      mir_block_t guarded_bb = mir_add_block(mu);
      mir_block_t merge_bb = mir_add_block(mu);

      mir_build_cond(mu, in_range, guarded_bb, merge_bb);

      mir_set_cursor(mu, guarded_bb, MIR_APPEND);

      mir_value_t ptr = mir_build_array_ref(mu, data, off);
      mir_value_t bit = mir_build_load(mu, ptr);
      mir_build_store(mu, tmp, mir_build_pack(mu, type, bit));
      mir_build_jump(mu, merge_bb);

      mir_set_cursor(mu, merge_bb, MIR_APPEND);

      return mir_build_load(mu, tmp);
   }
}

static mir_value_t vlog_lower_rvalue(mir_unit_t *mu, vlog_node_t v)
{
   PUSH_DEBUG_INFO(mu, v);

   switch (vlog_kind(v)) {
   case V_REF:
      {
         vlog_node_t decl = vlog_ref(v);
         if (vlog_kind(decl) == V_LOCALPARAM)
            return vlog_lower_rvalue(mu, vlog_value(decl));

         mir_value_t data = vlog_lower_resolved(mu, decl);

         type_info_t *ti = vlog_type_info(mu, vlog_type(decl));

         if (ti->size == 1)
            data = mir_build_load(mu, data);

         return mir_build_pack(mu, ti->type, data);
      }
   case V_EVENT:
      {
         vlog_node_t value = vlog_value(v);

         mir_type_t t_offset = mir_offset_type(mu);

         vlog_lvalue_t lvalue = vlog_lower_lvalue(mu, value);
         mir_value_t count = mir_const(mu, t_offset, lvalue.size);

         mir_value_t event = mir_build_event_flag(mu, lvalue.nets, count);

         const v_event_kind_t ekind = vlog_subkind(v);
         if (ekind == V_EVENT_LEVEL)
            return event;

         mir_type_t t_logic = mir_vec4_type(mu, 1, false);

         mir_value_t level =
            mir_const_vec(mu, t_logic, ekind == V_EVENT_POSEDGE, 0);
         mir_value_t rvalue = vlog_lower_rvalue(mu, value);
         mir_value_t cmp = mir_build_cmp(mu, MIR_CMP_EQ, rvalue, level);

         return mir_build_and(mu, event, cmp);
      }
   case V_NUMBER:
      {

         number_t num = vlog_number(v);
         const int width = number_width(num);
         assert(width < 64);

         if (number_is_defined(num)) {
            mir_type_t t_vec2 = mir_vec2_type(mu, width, false);
            return mir_const_vec(mu, t_vec2, number_integer(num), 0);
         }
         else {
            uint64_t abits = 0, bbits = 0;
            for (int i = 0; i < width; i++) {
               vlog_logic_t bit = number_bit(num, width - i - 1);
               abits = (abits << 1) | (bit & 1);
               bbits = (bbits << 1) | ((bit >> 1) & 1);
            }

            mir_type_t t_vec4 = mir_vec4_type(mu, width, false);
            return mir_const_vec(mu, t_vec4, abits, bbits);
         }
      }
   case V_BINARY:
      return vlog_lower_binary(mu, v);
   case V_UNARY:
      return vlog_lower_unary(mu, v);
   case V_SYS_FCALL:
      return vlog_lower_sys_tfcall(mu, v);
   case V_BIT_SELECT:
      return vlog_lower_rvalue_bit_select(mu, v);
   case V_PART_SELECT:
      {
         mir_value_t base = vlog_lower_resolved(mu, vlog_value(v));

         vlog_node_t dt = vlog_type(vlog_ref(vlog_value(v)));

         mir_value_t off = vlog_lower_array_off(mu, dt, vlog_left(v));
         mir_value_t ptr = mir_build_array_ref(mu, base, off);

         int64_t left, right;
         vlog_bounds(v, &left, &right);

         int64_t size = left < right ? right - left + 1 : left - right + 1;

         mir_type_t t_vec = mir_vec4_type(mu, size, false);
         return mir_build_pack(mu, t_vec, ptr);
      }
   case V_CONCAT:
      {
         int size = 0, repeat = 1, pos = 0;
         const int nparams = vlog_params(v);
         mir_value_t *inputs LOCAL =
            xmalloc_array(nparams, sizeof(mir_value_t));

         for (int i = 0; i < nparams; i++) {
            inputs[i] = vlog_lower_rvalue(mu, vlog_param(v, i));
            assert(mir_is_vector(mu, inputs[i]));
            size += mir_get_size(mu, mir_get_type(mu, inputs[i]));
         }

         if (vlog_has_value(v))
            size *= (repeat = vlog_get_const(vlog_value(v)));

         mir_type_t type = mir_vec4_type(mu, size, false);
         mir_value_t result = mir_const_vec(mu, type, 0, 0);

         for (int i = 0; i < repeat; i++) {
            for (int j = nparams - 1; j >= 0; j--) {
               // TODO: add a vector-insert operation
               mir_value_t cast = mir_build_cast(mu, type, inputs[j]);
               mir_value_t amount = mir_const_vec(mu, type, pos, 0);
               mir_value_t shift =
                  mir_build_binary(mu, MIR_VEC_SLL, type, cast, amount);
               result = mir_build_binary(mu, MIR_VEC_BIT_OR, type,
                                         result, shift);
               pos += mir_get_size(mu, mir_get_type(mu, inputs[j]));
            }
         }

         return result;
      }
   case V_PREFIX:
      {
         vlog_node_t target = vlog_target(v);

         mir_value_t prev = vlog_lower_rvalue(mu, target);
         mir_type_t type = mir_get_type(mu, prev);
         mir_value_t one = mir_const_vec(mu, type, 1, 0);
         mir_value_t inc = mir_build_binary(mu, MIR_VEC_ADD, type, prev, one);

         // Must save/restore around blocking assignment
         mir_value_t tmp = mir_add_var(mu, type, MIR_NULL_STAMP,
                                       ident_uniq("prefix"), MIR_VAR_TEMP);
         mir_build_store(mu, tmp, inc);

         vlog_assign_variable(mu, target, inc);

         return mir_build_load(mu, tmp);
      }
   default:
      CANNOT_HANDLE(v);
   }
}

static mir_value_t vlog_lower_time(mir_unit_t *mu, vlog_node_t v)
{
   assert(vlog_kind(v) == V_NUMBER);

   mir_type_t t_time = mir_time_type(mu);
   number_t num = vlog_number(v);

   return mir_const(mu, t_time, number_integer(num));
}

static void vlog_lower_sensitivity(mir_unit_t *mu, vlog_node_t v)
{
   switch (vlog_kind(v)) {
   case V_REF:
      {
         switch (vlog_kind(vlog_ref(v))) {
         case V_PORT_DECL:
         case V_NET_DECL:
         case V_VAR_DECL:
            {
               mir_type_t t_offset = mir_offset_type(mu);

               vlog_lvalue_t lvalue = vlog_lower_lvalue(mu, v);
               mir_value_t count = mir_const(mu, t_offset, lvalue.size);

               mir_build_sched_event(mu, lvalue.nets, count);
            }
            break;
         case V_PARAM_DECL:
         case V_LOCALPARAM:
            break;
         default:
            CANNOT_HANDLE(v);
         }
      }
      break;
   case V_BIT_SELECT:
      {
         vlog_lower_sensitivity(mu, vlog_value(v));

         const int nparams = vlog_params(v);
         for (int i = 0; i < nparams; i++)
            vlog_lower_sensitivity(mu, vlog_param(v, i));
      }
      break;
   case V_EVENT:
      vlog_lower_sensitivity(mu, vlog_value(v));
      break;
   case V_NUMBER:
      break;
   case V_BINARY:
      vlog_lower_sensitivity(mu, vlog_left(v));
      vlog_lower_sensitivity(mu, vlog_right(v));
      break;
   case V_UNARY:
      vlog_lower_sensitivity(mu, vlog_value(v));
      break;
   case V_CONCAT:
      {
         const int nparams = vlog_params(v);
         for (int i = 0; i < nparams; i++)
            vlog_lower_sensitivity(mu, vlog_param(v, i));
      }
      break;
   default:
      CANNOT_HANDLE(v);
   }
}

static void vlog_lower_timing(mir_unit_t *mu, vlog_node_t v, bool is_static)
{
   mir_block_t true_bb = mir_add_block(mu), false_bb = MIR_NULL_BLOCK;

   vlog_node_t ctrl = vlog_value(v);
   switch (vlog_kind(ctrl)) {
   case V_DELAY_CONTROL:
      {
         mir_type_t t_time = mir_time_type(mu);
         mir_value_t delay = vlog_lower_rvalue(mu, vlog_value(ctrl));
         mir_value_t cast = mir_build_cast(mu, t_time, delay);

         mir_build_wait(mu, true_bb, cast);

         mir_set_cursor(mu, true_bb, MIR_APPEND);
      }
      break;
   case V_EVENT_CONTROL:
      {
         mir_value_t test = MIR_NULL_VALUE;
         const int nparams = vlog_params(ctrl);
         if (nparams > 0) {
            test = vlog_lower_rvalue(mu, vlog_param(ctrl, 0));
            for (int i = 1; i < nparams; i++) {
               mir_value_t sub = vlog_lower_rvalue(mu, vlog_param(ctrl, i));
               test = mir_build_or(mu, test, sub);
            }
         }

         false_bb = mir_add_block(mu);

         if (mir_is_null(test))
            mir_build_jump(mu, false_bb);
         else
            mir_build_cond(mu, test, true_bb, false_bb);

         mir_set_cursor(mu, true_bb, MIR_APPEND);
      }
      break;
   default:
      CANNOT_HANDLE(ctrl);
   }

   vlog_lower_stmts(mu, v);

   if (!mir_is_null(false_bb)) {
      if (!mir_block_finished(mu, mir_get_cursor(mu, NULL)))
         mir_build_jump(mu, false_bb);

      mir_set_cursor(mu, false_bb, MIR_APPEND);
   }
}

static void vlog_lower_blocking_assignment(mir_unit_t *mu, vlog_node_t v)
{
   if (vlog_has_delay(v)) {
      vlog_node_t delay = vlog_delay(v);
      assert(vlog_kind(delay) == V_DELAY_CONTROL);

      mir_block_t delay_bb = mir_add_block(mu);

      mir_build_wait(mu, delay_bb, vlog_lower_time(mu, vlog_value(delay)));

      mir_set_cursor(mu, delay_bb, MIR_APPEND);
   }

   mir_value_t value = vlog_lower_rvalue(mu, vlog_value(v));
   vlog_assign_variable(mu, vlog_target(v), value);
}

static void vlog_lower_non_blocking_assignment(mir_unit_t *mu, vlog_node_t v)
{
   vlog_node_t target = vlog_target(v);

   vlog_lvalue_t lvalue = vlog_lower_lvalue(mu, target);
   mir_value_t value = vlog_lower_rvalue(mu, vlog_value(v));
   assert(mir_is_vector(mu, value));

   mir_type_t t_offset = mir_offset_type(mu);
   mir_type_t t_vec = mir_vec4_type(mu, lvalue.size, false);

   mir_value_t resize = mir_build_cast(mu, t_vec, value);
   mir_value_t count = mir_const(mu, t_offset, lvalue.size);

   mir_value_t tmp = MIR_NULL_VALUE;
   if (lvalue.size > 1)
      tmp = vlog_get_temp(mu, lvalue.base);

   const uint8_t strength = vlog_is_net(target) ? ST_STRONG : 0;
   mir_value_t unpacked = mir_build_unpack(mu, resize, strength, tmp);

   mir_type_t t_time = mir_time_type(mu);
   mir_value_t reject = mir_const(mu, t_time, 0);

   mir_value_t after;
   if (vlog_has_delay(v)) {
      vlog_node_t delay = vlog_delay(v);
      assert(vlog_kind(delay) == V_DELAY_CONTROL);

      after = vlog_lower_time(mu, vlog_value(delay));
   }
   else
      after = mir_const(mu, t_time, 0);

   mir_build_sched_waveform(mu, lvalue.nets, count, unpacked,
                            reject, after);
}

static void vlog_lower_if(mir_unit_t *mu, vlog_node_t v)
{
   mir_block_t exit_bb = MIR_NULL_BLOCK;

   const int nconds = vlog_conds(v);
   for (int i = 0; i < nconds; i++) {
      vlog_node_t c = vlog_cond(v, i);
      mir_block_t next_bb = MIR_NULL_BLOCK;

      if (vlog_has_value(c)) {
         mir_value_t test = vlog_lower_rvalue(mu, vlog_value(c));
         assert(mir_is_vector(mu, test));

         mir_value_t zero = mir_const_vec(mu, mir_get_type(mu, test), 0, 0);
         mir_value_t cmp = mir_build_cmp(mu, MIR_CMP_NEQ, test, zero);

         mir_block_t btrue = mir_add_block(mu);

         if (i == nconds - 1) {
            if (mir_is_null(exit_bb))
               exit_bb = mir_add_block(mu);
            next_bb = exit_bb;
         }
         else
            next_bb = mir_add_block(mu);

         mir_build_cond(mu, cmp, btrue, next_bb);

         mir_set_cursor(mu, btrue, MIR_APPEND);
      }

      vlog_lower_stmts(mu, c);

      if (!mir_block_finished(mu, MIR_NULL_BLOCK)) {
         if (mir_is_null(exit_bb))
            exit_bb = mir_add_block(mu);
         mir_build_jump(mu, exit_bb);
      }

      if (mir_is_null(next_bb))
         break;

      mir_set_cursor(mu, next_bb, MIR_APPEND);
   }

   if (!mir_is_null(exit_bb))
      mir_set_cursor(mu, exit_bb, MIR_APPEND);
}

static void vlog_lower_forever(mir_unit_t *mu, vlog_node_t v)
{
   mir_block_t body_bb = mir_add_block(mu);
   mir_build_jump(mu, body_bb);

   mir_set_cursor(mu, body_bb, MIR_APPEND);

   vlog_lower_stmts(mu, v);

   mir_build_jump(mu, body_bb);
}

static void vlog_lower_for_loop(mir_unit_t *mu, vlog_node_t v)
{
   mir_comment(mu, "Begin for loop");

   vlog_node_t init = vlog_left(v);
   assert(vlog_kind(init) == V_FOR_INIT);

   assert(vlog_decls(init) == 0);   // TODO

   vlog_lower_stmts(mu, init);

   mir_block_t body_bb = mir_add_block(mu);
   mir_block_t step_bb = mir_add_block(mu);
   mir_block_t test_bb = mir_add_block(mu);
   mir_block_t exit_bb = mir_add_block(mu);

   mir_build_jump(mu, test_bb);

   mir_set_cursor(mu, test_bb, MIR_APPEND);

   mir_comment(mu, "For loop test");

   mir_value_t test = vlog_lower_rvalue(mu, vlog_value(v));
   assert(mir_is_vector(mu, test));

   mir_value_t zero = mir_const_vec(mu, mir_get_type(mu, test), 0, 0);
   mir_value_t cmp = mir_build_cmp(mu, MIR_CMP_NEQ, test, zero);
   mir_build_cond(mu, cmp, body_bb, exit_bb);

   mir_set_cursor(mu, body_bb, MIR_APPEND);

   mir_comment(mu, "For loop body");

   vlog_lower_stmts(mu, v);

   if (!mir_block_finished(mu, MIR_NULL_BLOCK))
      mir_build_jump(mu, step_bb);

   mir_set_cursor(mu, step_bb, MIR_APPEND);

   mir_comment(mu, "For loop step");

   vlog_node_t step = vlog_right(v);
   assert(vlog_kind(step) == V_FOR_STEP);

   vlog_lower_stmts(mu, step);

   mir_build_jump(mu, test_bb);

   mir_set_cursor(mu, exit_bb, MIR_APPEND);

   mir_comment(mu, "End for loop");
}

static void vlog_lower_case(mir_unit_t *mu, vlog_node_t v)
{
   mir_comment(mu, "Begin case statement");

   mir_value_t value = vlog_lower_rvalue(mu, vlog_value(v));
   mir_type_t type = mir_get_type(mu, value);

   // TODO: use a parallel case for small integer types

   const int nitems = vlog_stmts(v);
   mir_block_t *blocks LOCAL = xmalloc_array(nitems, sizeof(mir_block_t));

   mir_value_t zero = mir_const_vec(mu, mir_vec4_type(mu, 1, false), 0, 0);

   for (int i = 0; i < nitems; i++) {
      vlog_node_t item = vlog_stmt(v, i);
      assert(vlog_kind(item) == V_CASE_ITEM);

      blocks[i] = mir_add_block(mu);

      mir_block_t else_bb = mir_add_block(mu);

      mir_value_t comb = MIR_NULL_VALUE;
      const int nparams = vlog_params(item);
      for (int j = 0; j < nparams; j++) {
         mir_value_t test = vlog_lower_rvalue(mu, vlog_param(item, j));
         mir_value_t cast = mir_build_cast(mu, type, test);
         mir_value_t case_eq =
            mir_build_binary(mu, MIR_VEC_CASE_EQ, type, cast, value);
         mir_value_t cmp = mir_build_cmp(mu, MIR_CMP_NEQ, case_eq, zero);

         if (mir_is_null(comb))
            comb = cmp;
         else
            comb = mir_build_or(mu, comb, cmp);
      }

      if (mir_is_null(comb))
         mir_build_jump(mu, blocks[i]);
      else
         mir_build_cond(mu, comb, blocks[i], else_bb);

      mir_set_cursor(mu, else_bb, MIR_APPEND);
   }

   mir_block_t exit_bb = mir_get_cursor(mu, NULL);

   for (int i = 0; i < nitems; i++) {
      vlog_node_t item = vlog_stmt(v, i);
      assert(vlog_kind(item) == V_CASE_ITEM);

      mir_set_cursor(mu, blocks[i], MIR_APPEND);
      vlog_lower_stmts(mu, item);

      if (!mir_block_finished(mu, MIR_NULL_BLOCK))
         mir_build_jump(mu, exit_bb);
   }

   mir_set_cursor(mu, exit_bb, MIR_APPEND);

   mir_comment(mu, "End case statement");
}

static void vlog_lower_stmts(mir_unit_t *mu, vlog_node_t v)
{
   const int nstmts = vlog_stmts(v);
   for (int i = 0; i < nstmts; i++) {
      vlog_node_t s = vlog_stmt(v, i);
      mir_set_loc(mu, vlog_loc(s));

      switch (vlog_kind(s)) {
      case V_TIMING:
         vlog_lower_timing(mu, s, false);
         break;
      case V_BASSIGN:
         vlog_lower_blocking_assignment(mu, s);
         break;
      case V_NBASSIGN:
         vlog_lower_non_blocking_assignment(mu, s);
         break;
      case V_BLOCK:
         vlog_lower_stmts(mu, s);
         break;
      case V_SYS_TCALL:
         vlog_lower_sys_tfcall(mu, s);
         break;
      case V_IF:
         vlog_lower_if(mu, s);
         break;
      case V_FOREVER:
         vlog_lower_forever(mu, s);
         break;
      case V_FOR_LOOP:
         vlog_lower_for_loop(mu, s);
         break;
      case V_CASE:
         vlog_lower_case(mu, s);
         break;
      default:
         CANNOT_HANDLE(s);
      }
   }
}

static void vlog_lower_driver(mir_unit_t *mu, vlog_node_t v)
{
   mir_type_t t_offset = mir_offset_type(mu);

   vlog_node_t prefix = vlog_longest_static_prefix(v);

   vlog_lvalue_t target = vlog_lower_lvalue(mu, prefix);
   mir_value_t one = mir_const(mu, t_offset, target.size);

   mir_build_drive_signal(mu, target.nets, one);
}

static void vlog_driver_cb(vlog_node_t v, void *context)
{
   mir_unit_t *mu = context;

   switch (vlog_kind(v)) {
   case V_NBASSIGN:
   case V_ASSIGN:
      vlog_lower_driver(mu, vlog_target(v));
      break;
   default:
      break;
   }
}

static void vlog_lower_always(mir_context_t *mc, ident_t parent, vlog_node_t v)
{
   mir_shape_t *shape = mir_get_shape(mc, parent);
   assert(shape != NULL);

   ident_t name = ident_prefix(parent, vlog_ident(v), '.');
   mir_unit_t *mu = mir_unit_new(mc, name, vlog_to_object(v),
                                 MIR_UNIT_PROCESS, shape);

   mir_block_t start_bb = mir_add_block(mu);
   assert(start_bb.id == 1);

   vlog_visit(v, vlog_driver_cb, mu);

   vlog_node_t timing = NULL, s0 = vlog_stmt(v, 0);
   if (vlog_kind(s0) == V_TIMING) {
      timing = s0;

      vlog_node_t ctrl = vlog_value(timing);
      assert(vlog_kind(ctrl) == V_EVENT_CONTROL);

      const int nparams = vlog_params(ctrl);
      for (int i = 0; i < nparams; i++)
         vlog_lower_sensitivity(mu, vlog_param(ctrl, i));
   }

   mir_build_return(mu, MIR_NULL_VALUE);

   mir_set_cursor(mu, start_bb, MIR_APPEND);

   if (timing != NULL)
      vlog_lower_timing(mu, timing, true);
   else
      vlog_lower_stmts(mu, v);

   mir_build_wait(mu, start_bb, MIR_NULL_VALUE);

   mir_put_unit(mc, mu);
}

static void vlog_lower_initial(mir_context_t *mc, ident_t parent, vlog_node_t v)
{
   mir_shape_t *shape = mir_get_shape(mc, parent);
   assert(shape != NULL);

   ident_t name = ident_prefix(parent, vlog_ident(v), '.');
   mir_unit_t *mu = mir_unit_new(mc, name, vlog_to_object(v),
                                 MIR_UNIT_PROCESS, shape);

   mir_block_t start_bb = mir_add_block(mu);
   assert(start_bb.id == 1);

   vlog_visit(v, vlog_driver_cb, mu);

   mir_build_return(mu, MIR_NULL_VALUE);

   mir_set_cursor(mu, start_bb, MIR_APPEND);

   vlog_lower_stmts(mu, v);

   if (!mir_block_finished(mu, MIR_NULL_BLOCK))
      mir_build_return(mu, MIR_NULL_VALUE);

   mir_put_unit(mc, mu);
}

static void vlog_lower_continuous_assign(mir_context_t *mc, ident_t parent,
                                         vlog_node_t v)
{
   mir_shape_t *shape = mir_get_shape(mc, parent);
   assert(shape != NULL);

   ident_t name = ident_prefix(parent, vlog_ident(v), '.');
   mir_unit_t *mu = mir_unit_new(mc, name, vlog_to_object(v),
                                 MIR_UNIT_PROCESS, shape);

   mir_block_t start_bb = mir_add_block(mu);
   assert(start_bb.id == 1);

   vlog_visit(v, vlog_driver_cb, mu);

   vlog_lower_sensitivity(mu, vlog_value(v));

   mir_build_return(mu, MIR_NULL_VALUE);

   mir_set_cursor(mu, start_bb, MIR_APPEND);

   vlog_lower_non_blocking_assignment(mu, v);

   mir_build_wait(mu, start_bb, MIR_NULL_VALUE);

   mir_put_unit(mc, mu);
}

static void vlog_lower_gate_inst(mir_context_t *mc, ident_t parent,
                                 vlog_node_t v)
{
   mir_shape_t *shape = mir_get_shape(mc, parent);
   assert(shape != NULL);

   ident_t name = ident_prefix(parent, vlog_ident(v), '.');
   mir_unit_t *mu = mir_unit_new(mc, name, vlog_to_object(v),
                                 MIR_UNIT_PROCESS, shape);

   mir_block_t start_bb = mir_add_block(mu);
   assert(start_bb.id == 1);

   vlog_lower_driver(mu, vlog_target(v));

   mir_type_t t_offset = mir_offset_type(mu);
   mir_type_t t_time = mir_time_type(mu);
   mir_type_t t_logic = mir_vec4_type(mu, 1, false);

   const int nparams = vlog_params(v);
   int first_term = 0;
   for (int i = 0; i < nparams; i++) {
      vlog_node_t p = vlog_param(v, i);
      if (vlog_kind(p) == V_STRENGTH)
         first_term = i + 1;
      else {
         vlog_lvalue_t lvalue = vlog_lower_lvalue(mu, p);
         mir_value_t count = mir_const(mu, t_offset, lvalue.size);
         mir_build_sched_event(mu, lvalue.nets, count);
      }
   }

   mir_build_return(mu, MIR_NULL_VALUE);

   mir_set_cursor(mu, start_bb, MIR_APPEND);

   bool negate = false;
   uint8_t strength = ST_STRONG;
   mir_value_t value = MIR_NULL_VALUE;
   const vlog_gate_kind_t kind = vlog_subkind(v);
   switch (kind) {
   case V_GATE_PULLUP:
   case V_GATE_PULLDOWN:
      strength = vlog_subkind(vlog_param(v, 0));
      value = mir_const_vec(mu, t_logic, kind == V_GATE_PULLUP, 0);
      break;

   case V_GATE_NAND:
   case V_GATE_NOR:
   case V_GATE_XNOR:
      negate = true;
   case V_GATE_AND:
   case V_GATE_OR:
   case V_GATE_XOR:
      {
         static const mir_vec_op_t op_map[] = {
            [V_GATE_AND] = MIR_VEC_BIT_AND,
            [V_GATE_NAND] = MIR_VEC_BIT_AND,
            [V_GATE_OR] = MIR_VEC_BIT_OR,
            [V_GATE_NOR] = MIR_VEC_BIT_OR,
            [V_GATE_XOR] = MIR_VEC_BIT_XOR,
            [V_GATE_XNOR] = MIR_VEC_BIT_XOR,
         };

         value = vlog_lower_rvalue(mu, vlog_param(v, first_term));

         const int nelems = nparams - first_term;
         for (int i = 1; i < nelems; i++) {
            vlog_node_t p = vlog_param(v, first_term + i);
            mir_value_t arg = vlog_lower_rvalue(mu, p);
            value = mir_build_binary(mu, op_map[kind], t_logic, value, arg);
         }

         if (negate)
            value = mir_build_unary(mu, MIR_VEC_BIT_NOT, t_logic, value);
      }
      break;

   case V_GATE_NOT:
      {
         const int nparams = vlog_params(v);
         mir_value_t input = vlog_lower_rvalue(mu, vlog_param(v, nparams - 1));
         value = mir_build_unary(mu, MIR_VEC_BIT_NOT, t_logic, input);
      }
      break;

   default:
      CANNOT_HANDLE(v);
   }

   mir_value_t unpacked = mir_build_unpack(mu, value, strength, MIR_NULL_VALUE);

   mir_value_t reject = mir_const(mu, t_time, 0);
   mir_value_t after = mir_const(mu, t_time, 0);

   vlog_lvalue_t lvalue = vlog_lower_lvalue(mu, vlog_target(v));
   mir_value_t count = mir_const(mu, t_offset, lvalue.size);

   mir_build_sched_waveform(mu, lvalue.nets, count, unpacked, reject, after);
   mir_build_wait(mu, start_bb, MIR_NULL_VALUE);

   mir_put_unit(mc, mu);
}

static void vlog_lower_concurrent(mir_context_t *mc, ident_t parent,
                                  vlog_node_t scope)
{
   const int nstmts = vlog_stmts(scope);
   for (int i = 0; i < nstmts; i++) {
      vlog_node_t s = vlog_stmt(scope, i);
      switch (vlog_kind(s)) {
      case V_ALWAYS:
         vlog_lower_always(mc, parent, s);
         break;
      case V_INITIAL:
         vlog_lower_initial(mc, parent, s);
         break;
      case V_ASSIGN:
         vlog_lower_continuous_assign(mc, parent, s);
         break;
      case V_GATE_INST:
         vlog_lower_gate_inst(mc, parent, s);
         break;
      case V_UDP_TABLE:
         vlog_lower_udp(mc, parent, scope);
         break;
      case V_INST_LIST:
         break;
      default:
         CANNOT_HANDLE(s);
      }
   }
}

vcode_unit_t vlog_lower(unit_registry_t *ur, mir_context_t *mc, vlog_node_t mod)
{
   assert(is_top_level(mod));

   ident_t name = ident_prefix(vlog_ident(mod), well_known(W_SHAPE), '.');

   vcode_unit_t vu = emit_shape(name, vlog_to_object(mod), NULL);

   lower_unit_t *lu = lower_unit_new(ur, NULL, vu, NULL, NULL);
   unit_registry_put(ur, lu);

   vcode_type_t vlogic = vlog_logic_type();
   vcode_type_t vnetvalue = vlog_net_value_type();
   vcode_type_t vlogicsignal = vtype_signal(vlogic);
   vcode_type_t vnetsignal = vtype_signal(vnetvalue);

   mir_unit_t *mu = mir_unit_new(mc, name, vlog_to_object(mod),
                                 MIR_UNIT_PLACEHOLDER, NULL);
   mir_type_t t_logic = mir_int_type(mu, 0, 3);
   mir_type_t t_net_value = mir_int_type(mu, 0, 255);
   mir_type_t t_logic_signal = mir_signal_type(mu, t_logic);
   mir_type_t t_net_signal = mir_signal_type(mu, t_net_value);

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

      mir_type_t type = vlog_is_net(port) ? t_net_signal : t_logic_signal;
      mir_value_t var2 = mir_add_var(mu, type, MIR_NULL_STAMP, vlog_ident(port),
                                     MIR_VAR_SIGNAL);

      mir_put_object(mu, port, var2);
      mir_put_object(mu, vlog_ref(port), var2);
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

            mir_type_t type = vlog_is_net(d) ? t_net_signal : t_logic_signal;
            mir_value_t var2 = mir_add_var(mu, type, MIR_NULL_STAMP,
                                           vlog_ident(d), MIR_VAR_SIGNAL);
            mir_put_object(mu, d, var2);
         }
         break;
      case V_LOCALPARAM:
         break;  // Always inlined for now
      default:
         CANNOT_HANDLE(d);
      }
   }

   emit_return(VCODE_INVALID_REG);

   lower_finished(lu, NULL);

   mir_put_unit(mc, mu);
   // TODO: should free here

   assert(vlog_kind(mod) == V_INST_BODY);

   vlog_lower_concurrent(mc, name, mod);

   unit_registry_finalise(ur, lu);
   return vu;
}
