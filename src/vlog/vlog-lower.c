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
#include "common.h"
#include "diag.h"
#include "hash.h"
#include "ident.h"
#include "mir/mir-node.h"
#include "mir/mir-unit.h"
#include "type.h"
#include "vlog/vlog-defs.h"
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
   mir_stamp_t stamp;
   unsigned    size;
} type_info_t;

typedef struct {
   mir_value_t  nets;
   mir_value_t  offset;
   mir_value_t  in_range;
   unsigned     size;
} vlog_lvalue_t;

STATIC_ASSERT(sizeof(vlog_lvalue_t) <= 16);

typedef struct {
   mir_unit_t *mu;
   ihash_t    *temps;
} vlog_gen_t;

#define PUSH_DEBUG_INFO(mu, v)                                          \
   __attribute__((cleanup(_mir_pop_debug_info), unused))                \
   const mir_saved_loc_t _old_loc =                                     \
      _mir_push_debug_info((mu), vlog_loc((v)));

static void vlog_lower_stmts(vlog_gen_t *g, vlog_node_t v);
static mir_value_t vlog_lower_rvalue(vlog_gen_t *g, vlog_node_t v);

static const type_info_t *vlog_type_info(vlog_gen_t *g, vlog_node_t v)
{
   assert(vlog_kind(v) == V_DATA_TYPE);

   type_info_t *ti = mir_get_priv(g->mu, v);
   if (ti == NULL) {
      ti = mir_malloc(g->mu, sizeof(type_info_t));
      ti->size  = vlog_size(v);
      ti->type  = mir_vec4_type(g->mu, ti->size, false);
      ti->stamp = MIR_NULL_STAMP;
   }

   return ti;
}

static mir_value_t vlog_get_temp(vlog_gen_t *g, mir_type_t type)
{
   if (g->temps == NULL)
      g->temps = ihash_new(16);
   else {
      mir_value_t exist = { .bits = (uintptr_t)ihash_get(g->temps, type.bits) };
      if (!mir_is_null(exist))
         return exist;
   }

   mir_value_t temp = mir_add_var(g->mu, type, MIR_NULL_STAMP,
                                  ident_uniq("tmp"), MIR_VAR_TEMP);

   ihash_put(g->temps, type.bits, (void *)(uintptr_t)temp.bits);
   return temp;
}

static mir_value_t vlog_lower_array_off(vlog_gen_t *g, vlog_node_t r,
                                        vlog_node_t v)
{
   mir_value_t index = vlog_lower_rvalue(g, v);

   mir_type_t index_type = mir_get_type(g->mu, index);
   if (mir_get_class(g->mu, index_type) == MIR_TYPE_VEC4) {
      // TODO: check X/Z handling
      const int size = mir_get_size(g->mu, index_type);
      mir_type_t vec2 = mir_vec2_type(g->mu, size, false);
      index = mir_build_cast(g->mu, vec2, index);
   }

   mir_type_t t_offset = mir_offset_type(g->mu);
   mir_value_t cast = mir_build_cast(g->mu, t_offset, index);

   assert(vlog_kind(r) == V_DIMENSION);

   int64_t lconst, rconst;
   vlog_bounds(r, &lconst, &rconst);

   mir_value_t left = mir_const(g->mu, t_offset, lconst);

   if (lconst < rconst)
      return mir_build_sub(g->mu, t_offset, cast, left);
   else
      return mir_build_sub(g->mu, t_offset, left, cast);
}

static mir_value_t vlog_lower_part_select_off(vlog_gen_t *g, vlog_node_t r,
                                              vlog_node_t v)
{
   mir_value_t base = vlog_lower_array_off(g, r, vlog_left(v));

   const vlog_range_kind_t kind = vlog_subkind(v);
   if (kind == V_RANGE_CONST)
      return base;

   const bool is_up = vlog_is_up(r);
   if ((kind == V_RANGE_POS && is_up) || (kind == V_RANGE_NEG && !is_up))
      return base;

   mir_type_t t_offset = mir_offset_type(g->mu);
   mir_value_t adj = mir_const(g->mu, t_offset, vlog_size(v) - 1);
   return mir_build_sub(g->mu, t_offset, base, adj);
}

static vlog_lvalue_t vlog_lower_lvalue_ref(vlog_gen_t *g, vlog_node_t v)
{
   vlog_node_t decl = vlog_ref(v);
   if (vlog_kind(decl) == V_PORT_DECL)
      decl = vlog_ref(decl);

   int hops;
   mir_value_t var = mir_search_object(g->mu, decl, &hops);
   assert(!mir_is_null(var));

   mir_value_t ptr = var;
   if (hops > 0)
      ptr = mir_build_var_upref(g->mu, hops, var.id);

   mir_value_t nets = ptr;
   if (mir_points_to(g->mu, ptr, MIR_TYPE_SIGNAL))
      nets = mir_build_load(g->mu, ptr);

   const type_info_t *ti = vlog_type_info(g, vlog_type(decl));

   mir_type_t t_bool = mir_bool_type(g->mu);
   mir_type_t t_offset = mir_offset_type(g->mu);

   vlog_lvalue_t result = {
      .nets     = nets,
      .size     = ti->size,
      .offset   = mir_const(g->mu, t_offset, 0),
      .in_range = mir_const(g->mu, t_bool, 1),
   };
   return result;
}

static vlog_lvalue_t vlog_lower_lvalue(vlog_gen_t *g, vlog_node_t v)
{
   PUSH_DEBUG_INFO(g->mu, v);

   switch (vlog_kind(v)) {
   case V_REF:
      return vlog_lower_lvalue_ref(g, v);
   case V_HIER_REF:
      {
         mir_type_t t_net_value = mir_int_type(g->mu, 0, 255);
         mir_type_t t_net_signal = mir_signal_type(g->mu, t_net_value);

         // XXX: reconsider this
         ident_t unit_name =
            ident_prefix(mir_get_parent(g->mu), vlog_ident2(v), '.');
         mir_value_t context = mir_build_link_package(g->mu, unit_name);
         mir_value_t ptr = mir_build_link_var(g->mu, unit_name, context,
                                              vlog_ident(v), t_net_signal);

         mir_type_t t_bool = mir_bool_type(g->mu);
         mir_type_t t_offset = mir_offset_type(g->mu);

         vlog_node_t decl = vlog_ref(v);
         const type_info_t *ti = vlog_type_info(g, vlog_type(decl));

         vlog_lvalue_t result = {
            .nets     = mir_build_load(g->mu, ptr),
            .size     = ti->size,
            .offset   = mir_const(g->mu, t_offset, 0),
            .in_range = mir_const(g->mu, t_bool, 1),
         };
         return result;
      }
   case V_BIT_SELECT:
      {
         vlog_node_t prefix = vlog_value(v);
         assert(vlog_kind(prefix) == V_REF);

         vlog_lvalue_t ref = vlog_lower_lvalue_ref(g, prefix);

         vlog_node_t decl = vlog_ref(prefix), dt = vlog_type(decl);

         const int nunpacked = vlog_ranges(decl);
         const int nparams = vlog_params(v);
         assert(nparams <= vlog_ranges(dt) + nunpacked);

         unsigned size = vlog_size(decl) * ref.size;

         mir_type_t t_offset = mir_offset_type(g->mu);
         mir_value_t zero = mir_const(g->mu, t_offset, 0), off = zero;
         mir_value_t in_range = ref.in_range;

         for (int i = 0; i < nparams; i++) {
            vlog_node_t dim;
            if (i < nunpacked)
               dim = vlog_range(decl, i);
            else
               dim = vlog_range(dt, i - nunpacked);

            const unsigned dim_size = vlog_size(dim);
            assert(size % dim_size == 0);
            size /= dim_size;

            mir_value_t this_off =
               vlog_lower_array_off(g, dim, vlog_param(v, i));

            mir_value_t count = mir_const(g->mu, t_offset, dim_size);
            mir_value_t cmp_low =
               mir_build_cmp(g->mu, MIR_CMP_GEQ, this_off, zero);
            mir_value_t cmp_high =
               mir_build_cmp(g->mu, MIR_CMP_LT, this_off, count);
            mir_value_t this_in_range = mir_build_and(g->mu, cmp_low, cmp_high);

            if (size != 1) {
               mir_value_t scale = mir_const(g->mu, t_offset, size);
               this_off = mir_build_mul(g->mu, t_offset, this_off, scale);
            }

            in_range = mir_build_and(g->mu, in_range, this_in_range);
            off = mir_build_add(g->mu, t_offset, off, this_off);
         }

         vlog_lvalue_t lvalue = {
            .nets     = ref.nets,
            .offset   = off,
            .size     = size,
            .in_range = in_range,
         };
         return lvalue;
      }
   case V_PART_SELECT:
      {
         vlog_lvalue_t prefix = vlog_lower_lvalue(g, vlog_value(v));

         vlog_node_t dt = vlog_type(vlog_ref(vlog_value(v)));
         vlog_node_t dim = vlog_range(dt, 0);

         mir_value_t off = vlog_lower_part_select_off(g, dim, v);

         mir_type_t t_offset = mir_offset_type(g->mu);

         vlog_lvalue_t lvalue = {
            .nets     = prefix.nets,
            .offset   = mir_build_add(g->mu, t_offset, off, prefix.offset),
            .size     = vlog_size(v),
            .in_range = prefix.in_range,   // XXX
         };
         return lvalue;
      }
   default:
      CANNOT_HANDLE(v);
   }
}

static void vlog_assign_variable(vlog_gen_t *g, vlog_node_t target,
                                 mir_value_t value)
{
   assert(mir_is_vector(g->mu, value));

   vlog_lvalue_t lvalue = vlog_lower_lvalue(g, target);

   mir_type_t t_offset = mir_offset_type(g->mu);
   mir_type_t t_vec = mir_vec4_type(g->mu, lvalue.size, false);

   mir_value_t resize = mir_build_cast(g->mu, t_vec, value);
   mir_value_t count = mir_const(g->mu, t_offset, lvalue.size);

   mir_block_t merge_bb = MIR_NULL_BLOCK;

   int64_t in_range_const;
   if (mir_get_const(g->mu, lvalue.in_range, &in_range_const)) {
      if (!in_range_const) {
         mir_comment(g->mu, "Out-of-range assignment");
         return;
      }
   }
   else {
      mir_block_t guarded_bb = mir_add_block(g->mu);
      merge_bb = mir_add_block(g->mu);

      mir_build_cond(g->mu, lvalue.in_range, guarded_bb, merge_bb);

      mir_set_cursor(g->mu, guarded_bb, MIR_APPEND);
   }

   if (mir_is_signal(g->mu, lvalue.nets)) {
      mir_value_t dst = mir_build_array_ref(g->mu, lvalue.nets, lvalue.offset);

      mir_value_t tmp = MIR_NULL_VALUE;
      if (lvalue.size > 1) {
         mir_type_t t_elem = mir_logic_type(g->mu);
         mir_type_t t_array = mir_carray_type(g->mu, lvalue.size, t_elem);
         tmp = vlog_get_temp(g, t_array);
      }

      mir_value_t unpacked = mir_build_unpack(g->mu, resize, 0, tmp);

      mir_build_deposit_signal(g->mu, dst, count, unpacked);
   }
   else if (vlog_kind(target) == V_REF)
      mir_build_store(g->mu, lvalue.nets, resize);
   else {
      mir_value_t cur = mir_build_load(g->mu, lvalue.nets);
      mir_value_t ins = mir_build_insert(g->mu, resize, cur, lvalue.offset);
      mir_build_store(g->mu, lvalue.nets, ins);
   }

   if (!mir_is_null(merge_bb)) {
      mir_build_jump(g->mu, merge_bb);

      mir_set_cursor(g->mu, merge_bb, MIR_APPEND);
   }
}

static mir_value_t vlog_lower_unary(vlog_gen_t *g, vlog_node_t v)
{
   mir_value_t input = vlog_lower_rvalue(g, vlog_value(v));
   mir_type_t type = mir_get_type(g->mu, input);

   bool negate = false;
   mir_vec_op_t mop;
   switch (vlog_subkind(v)) {
   case V_UNARY_BITNEG: mop = MIR_VEC_BIT_NOT; break;
   case V_UNARY_NOT:    mop = MIR_VEC_LOG_NOT; break;
   case V_UNARY_NEG:    mop = MIR_VEC_SUB; break;
   case V_UNARY_OR:     mop = MIR_VEC_BIT_OR; break;
   case V_UNARY_XOR:    mop = MIR_VEC_BIT_XOR; break;
   case V_UNARY_AND:    mop = MIR_VEC_BIT_AND; break;
   case V_UNARY_NAND:   mop = MIR_VEC_BIT_AND; negate = true; break;
   case V_UNARY_NOR:    mop = MIR_VEC_BIT_OR; negate = true; break;
   case V_UNARY_XNOR:   mop = MIR_VEC_BIT_XOR; negate = true; break;
   default:
      CANNOT_HANDLE(v);
   }

   mir_value_t result = mir_build_unary(g->mu, mop, type, input);
   if (!negate)
      return result;

   mir_type_t otype = mir_get_type(g->mu, result);
   assert(mir_get_size(g->mu, otype) == 1);

   return mir_build_unary(g->mu, MIR_VEC_BIT_NOT, otype, result);
}

static bool vlog_has_side_effects(vlog_node_t v)
{
   switch (vlog_kind(v)) {
   case V_REF:
   case V_BIT_SELECT:
   case V_PART_SELECT:
   case V_NUMBER:
      return false;
   case V_UNARY:
      return vlog_has_side_effects(vlog_value(v));
   case V_BINARY:
      return vlog_has_side_effects(vlog_left(v))
         || vlog_has_side_effects(vlog_right(v));
   default:
      return true;
   }
}

static mir_value_t vlog_lower_binary(vlog_gen_t *g, vlog_node_t v)
{
   mir_value_t left = vlog_lower_rvalue(g, vlog_left(v));
   assert(mir_is_vector(g->mu, left));

   // For short-circuiting operators check if the RHS can have side effects
   vlog_node_t rhs_expr = vlog_right(v);
   mir_block_t guard_bb = MIR_NULL_BLOCK, skip_bb = MIR_NULL_BLOCK;
   mir_value_t var = MIR_NULL_VALUE;
   const vlog_binary_t binop = vlog_subkind(v);
   const bool is_short_circuit =
      binop == V_BINARY_LOG_AND || binop == V_BINARY_LOG_OR;

   if (is_short_circuit && vlog_has_side_effects(rhs_expr)) {
      guard_bb = mir_add_block(g->mu);
      skip_bb = mir_add_block(g->mu);

      // TODO: use a phi
      mir_type_t t_logic = mir_vec4_type(g->mu, 1, false);
      var = mir_add_var(g->mu, t_logic, MIR_NULL_STAMP,
                        ident_uniq("shortcircuit"), MIR_VAR_TEMP);

      mir_value_t def = mir_const_vec(g->mu, t_logic,
                                      binop == V_BINARY_LOG_OR, 0);
      mir_build_store(g->mu, var, def);

      mir_value_t test = mir_build_test(g->mu, left);
      if (binop == V_BINARY_LOG_OR)
         mir_build_cond(g->mu, test, skip_bb, guard_bb);
      else
         mir_build_cond(g->mu, test, guard_bb, skip_bb);

      mir_set_cursor(g->mu, guard_bb, MIR_APPEND);
   }

   mir_value_t right = vlog_lower_rvalue(g, rhs_expr);
   assert(mir_is_vector(g->mu, right));

   mir_type_t ltype = mir_get_type(g->mu, left);
   mir_type_t rtype = mir_get_type(g->mu, right);

   const mir_class_t lclass = mir_get_class(g->mu, ltype);
   const mir_class_t rclass = mir_get_class(g->mu, rtype);

   const int lsize = mir_get_size(g->mu, ltype);
   const int rsize = mir_get_size(g->mu, rtype);

   const bool is_signed =
      mir_get_signed(g->mu, ltype) || mir_get_signed(g->mu, rtype);

   mir_type_t type;
   if (lclass == MIR_TYPE_VEC4 || rclass == MIR_TYPE_VEC4)
      type = mir_vec4_type(g->mu, MAX(lsize, rsize), is_signed);
   else
      type = mir_vec2_type(g->mu, MAX(lsize, rsize), is_signed);

   mir_value_t lcast = mir_build_cast(g->mu, type, left);
   mir_value_t rcast = mir_build_cast(g->mu, type, right);

   bool negate = false;
   mir_vec_op_t mop;
   switch (binop) {
   case V_BINARY_AND:      mop = MIR_VEC_BIT_AND; break;
   case V_BINARY_OR:       mop = MIR_VEC_BIT_OR; break;
   case V_BINARY_XOR:      mop = MIR_VEC_BIT_XOR; break;
   case V_BINARY_XNOR:     mop = MIR_VEC_BIT_XOR; negate = true; break;
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
   case V_BINARY_TIMES:    mop = MIR_VEC_MUL; break;
   case V_BINARY_SHIFT_LL: mop = MIR_VEC_SLL; break;
   case V_BINARY_SHIFT_RL: mop = MIR_VEC_SRL; break;
   case V_BINARY_SHIFT_RA: mop = MIR_VEC_SRA; break;
   case V_BINARY_SHIFT_LA: mop = MIR_VEC_SLA; break;
   default:
      CANNOT_HANDLE(v);
   }

   mir_value_t result = mir_build_binary(g->mu, mop, type, lcast, rcast);
   if (negate) {
      mir_type_t otype = mir_get_type(g->mu, result);
      result = mir_build_unary(g->mu, MIR_VEC_BIT_NOT, otype, result);
   }

   if (mir_is_null(var))
      return result;

   mir_build_store(g->mu, var, result);

   mir_set_cursor(g->mu, skip_bb, MIR_APPEND);

   return mir_build_load(g->mu, var);
}

static mir_value_t vlog_lower_systf_param(vlog_gen_t *g, vlog_node_t v)
{
   switch (vlog_kind(v)) {
   case V_REF:
      switch (vlog_kind(vlog_ref(v))) {
      case V_TF_PORT_DECL:
      case V_FUNC_DECL:
         return vlog_lower_rvalue(g, v);
      default:
         return MIR_NULL_VALUE;
      }
   case V_STRING:
   case V_NUMBER:
   case V_EMPTY:
      return MIR_NULL_VALUE;
   case V_UNARY:
   case V_BINARY:
   case V_SYS_FCALL:
   case V_PREFIX:
   case V_POSTFIX:
   case V_BIT_SELECT:
   case V_PART_SELECT:
   case V_COND_EXPR:
      // TODO: these should not be evaluated until vpi_get_value is called
      return vlog_lower_rvalue(g, v);
   default:
      CANNOT_HANDLE(v);
   }
}

static mir_value_t vlog_lower_sys_tfcall(vlog_gen_t *g, vlog_node_t v)
{
   const int nparams = vlog_params(v);
   mir_value_t *args LOCAL =
      xmalloc_array((nparams * 2) + 1, sizeof(mir_value_t));
   int actual = 0;
   mir_type_t t_offset = mir_offset_type(g->mu);
   for (int i = 0; i < nparams; i++) {
      mir_value_t arg = vlog_lower_systf_param(g, vlog_param(v, i));
      if (mir_is_null(arg))
         continue;

      mir_type_t type = mir_get_type(g->mu, arg);
      switch (mir_get_class(g->mu, type)) {
      case MIR_TYPE_VEC4:
         args[actual++] = mir_const(g->mu, t_offset, mir_get_size(g->mu, type));
         args[actual++] = arg;
         break;
      case MIR_TYPE_VEC2:
         {
            // TODO: remove the cast
            const int size = mir_get_size(g->mu, type);
            mir_type_t t_vec4 = mir_vec4_type(g->mu, size, false);
            args[actual++] = mir_const(g->mu, t_offset, size);
            args[actual++] = mir_build_cast(g->mu, t_vec4, arg);
         }
         break;
      default:
         should_not_reach_here();
      }
   }

   mir_value_t locus = mir_build_locus(g->mu, vlog_to_object(v));

   mir_type_t type = MIR_NULL_TYPE;
   if (vlog_kind(v) == V_SYS_FCALL)
      type = mir_vec2_type(g->mu, 64, false);  // XXX: hack for $time

   return mir_build_syscall(g->mu, vlog_ident(v), type, MIR_NULL_STAMP,
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
   case V_FUNC_DECL:
   case V_GENVAR_DECL:
      {
         int hops;
         mir_value_t var = mir_search_object(mu, v, &hops), val;
         assert(!mir_is_null(var));

         if (hops == 0)
            val = mir_build_load(mu, var);
         else {
            mir_value_t upref = mir_build_var_upref(mu, hops, var.id);
            val = mir_build_load(mu, upref);
         }

         if (mir_is_signal(mu, val))
            return mir_build_resolved(mu, val);
         else
            return val;
      }
   case V_TF_PORT_DECL:
      {
         int hops;
         mir_value_t param = mir_search_object(mu, v, &hops);
         assert(!mir_is_null(param));
         assert(param.tag == MIR_TAG_PARAM);
         assert(hops == 0);
         return param;
      }
   default:
      CANNOT_HANDLE(v);
   }
}

static mir_value_t vlog_lower_bit_select(vlog_gen_t *g, vlog_node_t v)
{
   vlog_node_t value = vlog_value(v);
   assert(vlog_kind(value) == V_REF);

   mir_value_t data = vlog_lower_resolved(g->mu, value);

   vlog_node_t decl = vlog_ref(value), dt = vlog_type(decl);

   const int nunpacked = vlog_ranges(decl);
   const int nparams = vlog_params(v);
   assert(nparams <= vlog_ranges(dt) + nunpacked);

   unsigned size = vlog_size(decl) * vlog_size(dt);

   mir_type_t t_offset = mir_offset_type(g->mu);
   mir_value_t zero = mir_const(g->mu, t_offset, 0), off = zero;
   mir_value_t in_range = mir_const(g->mu, mir_bool_type(g->mu), 1);

   for (int i = 0; i < nparams; i++) {
      vlog_node_t dim;
      if (i < nunpacked)
         dim = vlog_range(decl, i);
      else
         dim = vlog_range(dt, i - nunpacked);

      const unsigned dim_size = vlog_size(dim);
      assert(size % dim_size == 0);
      size /= dim_size;

      mir_value_t this_off = vlog_lower_array_off(g, dim, vlog_param(v, i));

      mir_value_t count = mir_const(g->mu, t_offset, dim_size);

      mir_value_t cmp_low = mir_build_cmp(g->mu, MIR_CMP_GEQ, this_off, zero);
      mir_value_t cmp_high = mir_build_cmp(g->mu, MIR_CMP_LT, this_off, count);
      mir_value_t this_in_range = mir_build_and(g->mu, cmp_low, cmp_high);

      if (size != 1) {
         mir_value_t scale = mir_const(g->mu, t_offset, size);
         this_off = mir_build_mul(g->mu, t_offset, this_off, scale);
      }

      in_range = mir_build_and(g->mu, in_range, this_in_range);
      off = mir_build_add(g->mu, t_offset, off, this_off);
   }

   mir_type_t type = mir_vec4_type(g->mu, size, false);

   mir_block_t merge_bb = MIR_NULL_BLOCK;
   mir_value_t tmp;
   int64_t in_range_const;
   if (mir_get_const(g->mu, in_range, &in_range_const)) {
      if (!in_range_const)
         return mir_const_vec(g->mu, type, 1, 1);
   }
   else {
      // TODO: use a phi node here
      tmp = mir_add_var(g->mu, type, MIR_NULL_STAMP,
                        ident_uniq("tmp"), MIR_VAR_TEMP);
      mir_build_store(g->mu, tmp, mir_const_vec(g->mu, type, 1, 1));

      mir_block_t guarded_bb = mir_add_block(g->mu);
      merge_bb = mir_add_block(g->mu);

      mir_build_cond(g->mu, in_range, guarded_bb, merge_bb);

      mir_set_cursor(g->mu, guarded_bb, MIR_APPEND);
   }

   mir_value_t packed;
   if (mir_is_vector(g->mu, data))
      packed = mir_build_extract(g->mu, type, data, off);
   else {
      mir_value_t ptr = mir_build_array_ref(g->mu, data, off);
      if (size == 1) {
         mir_value_t bit = mir_build_load(g->mu, ptr);
         packed = mir_build_pack(g->mu, type, bit);
      }
      else
         packed = mir_build_pack(g->mu, type, ptr);
   }

   if (mir_is_null(merge_bb))
      return packed;
   else {
      mir_build_store(g->mu, tmp, packed);
      mir_build_jump(g->mu, merge_bb);

      mir_set_cursor(g->mu, merge_bb, MIR_APPEND);

      return mir_build_load(g->mu, tmp);
   }
}

static mir_value_t vlog_lower_sys_fcall(vlog_gen_t *g, vlog_node_t v)
{
   switch (is_well_known(vlog_ident(v))) {
   case W_DLR_SIGNED:
      {
         mir_value_t arg = vlog_lower_rvalue(g, vlog_param(v, 0));
         mir_type_t arg_type = mir_get_type(g->mu, arg), type;
         switch (mir_get_class(g->mu, arg_type)) {
         case MIR_TYPE_VEC2:
            type = mir_vec2_type(g->mu, mir_get_size(g->mu, arg_type), true);
            break;
         case MIR_TYPE_VEC4:
            type = mir_vec4_type(g->mu, mir_get_size(g->mu, arg_type), true);
            break;
         default:
            type = arg_type;
            break;
         }

         return mir_build_cast(g->mu, type, arg);
      }
   default:
      return vlog_lower_sys_tfcall(g, v);
   }
}

static mir_value_t vlog_lower_rvalue(vlog_gen_t *g, vlog_node_t v)
{
   PUSH_DEBUG_INFO(g->mu, v);

   switch (vlog_kind(v)) {
   case V_REF:
      {
         vlog_node_t decl = vlog_ref(v);
         if (vlog_kind(decl) == V_LOCALPARAM)
            return vlog_lower_rvalue(g, vlog_value(decl));

         mir_value_t data = vlog_lower_resolved(g->mu, decl);
         if (mir_is_vector(g->mu, data))
            return data;

         const type_info_t *ti = vlog_type_info(g, vlog_type(decl));

         if (ti->size == 1)
            data = mir_build_load(g->mu, data);

         return mir_build_pack(g->mu, ti->type, data);
      }
   case V_HIER_REF:
      {
         mir_type_t t_net_value = mir_int_type(g->mu, 0, 255);
         mir_type_t t_net_signal = mir_signal_type(g->mu, t_net_value);

         // XXX: reconsider this
         ident_t unit_name =
            ident_prefix(mir_get_parent(g->mu), vlog_ident2(v), '.');
         mir_value_t context = mir_build_link_package(g->mu, unit_name);
         mir_value_t ptr = mir_build_link_var(g->mu, unit_name, context,
                                              vlog_ident(v), t_net_signal);
         mir_value_t nets = mir_build_load(g->mu, ptr);

         vlog_node_t decl = vlog_ref(v);
         const type_info_t *ti = vlog_type_info(g, vlog_type(decl));

         mir_value_t data = mir_build_resolved(g->mu, nets);

         if (ti->size == 1)
            data = mir_build_load(g->mu, data);

         return mir_build_pack(g->mu, ti->type, data);
      }
   case V_NUMBER:
      {
         number_t num = vlog_number(v);
         const int width = number_width(num);

         const uint64_t *abits, *bbits;
         number_get(num, &abits, &bbits);

         mir_type_t t_low = mir_vec4_type(g->mu, MIN(width, 64), false);
         mir_value_t low = mir_const_vec(g->mu, t_low, abits[0], bbits[0]);

         if (width <= 64)
            return low;

         mir_type_t t_full = mir_vec4_type(g->mu, width, false);
         mir_value_t full = mir_build_cast(g->mu, t_full, low);

         mir_type_t t_offset = mir_offset_type(g->mu);

         for (int i = 64; i < width; i += 64) {
            const uint64_t aword = abits[i / 64];
            const uint64_t bword = bbits[i / 64];
            if (aword == 0 && bword == 0)
               continue;

            const int part_size = MIN(width - i, 64);

            mir_type_t t_part = mir_vec4_type(g->mu, part_size, false);
            mir_value_t part = mir_const_vec(g->mu, t_part, aword, bword);

            mir_value_t pos = mir_const(g->mu, t_offset, width - i - part_size);
            full = mir_build_insert(g->mu, part, full, pos);
         }

         return full;
      }
   case V_BINARY:
      return vlog_lower_binary(g, v);
   case V_UNARY:
      return vlog_lower_unary(g, v);
   case V_SYS_FCALL:
      return vlog_lower_sys_fcall(g, v);
   case V_BIT_SELECT:
      return vlog_lower_bit_select(g, v);
   case V_PART_SELECT:
      {
         mir_value_t base = vlog_lower_resolved(g->mu, vlog_value(v));

         vlog_node_t dt = vlog_type(vlog_ref(vlog_value(v)));
         vlog_node_t dim = vlog_range(dt, 0);

         mir_value_t off = vlog_lower_part_select_off(g, dim, v);

         mir_type_t t_vec = mir_vec4_type(g->mu, vlog_size(v), false);

         if (mir_is_vector(g->mu, base))
            return mir_build_extract(g->mu, t_vec, base, off);
         else {
            mir_value_t ptr = mir_build_array_ref(g->mu, base, off);
            return mir_build_pack(g->mu, t_vec, ptr);
         }
      }
   case V_CONCAT:
      {
         int size = 0, repeat = 1;
         const int nparams = vlog_params(v);
         mir_value_t *inputs LOCAL =
            xmalloc_array(nparams, sizeof(mir_value_t));

         for (int i = 0; i < nparams; i++) {
            inputs[i] = vlog_lower_rvalue(g, vlog_param(v, i));
            assert(mir_is_vector(g->mu, inputs[i]));

            mir_type_t part_type = mir_get_type(g->mu, inputs[i]);
            int part_size = mir_get_size(g->mu, part_type);

            if (mir_get_class(g->mu, part_type) != MIR_TYPE_VEC4) {
               mir_type_t t_vec4 = mir_vec4_type(g->mu, part_size, false);
               inputs[i] = mir_build_cast(g->mu, t_vec4, inputs[i]);
            }

            size += part_size;
         }

         if (vlog_has_value(v)) {
            int64_t cval;
            vlog_get_const(vlog_value(v), &cval);
            size *= (repeat = MAX(0, cval));
         }

         mir_type_t type = mir_vec4_type(g->mu, size, false);

         mir_value_t result;
         if (size <= 64)
            result = mir_const_vec(g->mu, type, 0, 0);
         else {
            mir_type_t t_bit = mir_vec4_type(g->mu, 1, false);
            mir_value_t zero = mir_const_vec(g->mu, t_bit, 0, 0);
            result = mir_build_cast(g->mu, type, zero);
         }

         mir_type_t t_offset = mir_offset_type(g->mu);

         for (int i = 0, pos = size; i < repeat; i++) {
            for (int j = nparams - 1; j >= 0; j--) {
               pos -= mir_get_size(g->mu, mir_get_type(g->mu, inputs[j]));
               result = mir_build_insert(g->mu, inputs[j], result,
                                         mir_const(g->mu, t_offset, pos));
            }
         }

         return result;
      }
   case V_PREFIX:
      {
         vlog_node_t target = vlog_target(v);

         mir_value_t prev = vlog_lower_rvalue(g, target);
         mir_type_t type = mir_get_type(g->mu, prev);
         mir_value_t one = mir_const_vec(g->mu, type, 1, 0);
         mir_value_t inc =
            mir_build_binary(g->mu, MIR_VEC_ADD, type, prev, one);

         // Must save/restore around blocking assignment
         mir_value_t tmp = mir_add_var(g->mu, type, MIR_NULL_STAMP,
                                       ident_uniq("prefix"), MIR_VAR_TEMP);
         mir_build_store(g->mu, tmp, inc);

         vlog_assign_variable(g, target, inc);

         return mir_build_load(g->mu, tmp);
      }
   case V_POSTFIX:
      {
         vlog_node_t target = vlog_target(v);

         mir_value_t prev = vlog_lower_rvalue(g, target);
         mir_type_t type = mir_get_type(g->mu, prev);
         mir_value_t one = mir_const_vec(g->mu, type, 1, 0);
         mir_value_t inc =
            mir_build_binary(g->mu, MIR_VEC_ADD, type, prev, one);

         vlog_assign_variable(g, target, inc);

         return inc;
      }
   case V_COND_EXPR:
      {
         // See 1800-2023 section 11.4.11 for semantics

         // TODO: do not evaluate both sides

         mir_value_t value = vlog_lower_rvalue(g, vlog_value(v));
         mir_value_t cmp = mir_build_test(g->mu, value);
         mir_value_t left = vlog_lower_rvalue(g, vlog_left(v));
         mir_value_t right = vlog_lower_rvalue(g, vlog_right(v));

         mir_type_t ltype = mir_get_type(g->mu, left);
         mir_type_t rtype = mir_get_type(g->mu, right);

         unsigned size = MAX(mir_get_size(g->mu, ltype),
                             mir_get_size(g->mu, rtype));
         mir_type_t type = mir_vec4_type(g->mu, size, false);

         mir_value_t lcast = mir_build_cast(g->mu, type, left);
         mir_value_t rcast = mir_build_cast(g->mu, type, right);

         return mir_build_select(g->mu, type, cmp, lcast, rcast);
      }
   case V_USER_FCALL:
      {
         vlog_node_t decl = vlog_ref(v);
         ident_t func = ident_prefix(vlog_ident2(decl), vlog_ident(decl), '.');

         const int nparams = vlog_params(v);
         mir_value_t *args LOCAL =
            xmalloc_array(nparams + 1, sizeof(mir_value_t));

         args[0] = mir_build_context_upref(g->mu, 1);  // XXX
         for (int i = 0; i < nparams; i++) {
            mir_value_t value = vlog_lower_rvalue(g, vlog_param(v, i));
            vlog_node_t dt = vlog_type(vlog_port(decl, i));
            const type_info_t *ti = vlog_type_info(g, dt);
            args[i + 1] = mir_build_cast(g->mu, ti->type, value);
         }

         const type_info_t *ti = vlog_type_info(g, vlog_type(decl));
         return mir_build_fcall(g->mu, func, ti->type, MIR_NULL_STAMP,
                                args, nparams + 1);
      }
   case V_REAL:
      {
         mir_type_t t_double = mir_double_type(g->mu);
         return mir_const_real(g->mu, t_double, vlog_dval(v));
      }
   default:
      CANNOT_HANDLE(v);
   }
}

static mir_value_t vlog_lower_time(vlog_gen_t *g, vlog_node_t v)
{
   assert(vlog_kind(v) == V_NUMBER);

   mir_type_t t_time = mir_time_type(g->mu);
   number_t num = vlog_number(v);

   return mir_const(g->mu, t_time, number_integer(num));
}

static mir_value_t vlog_or_triggers(vlog_gen_t *g, int n, ...)
{
   va_list ap;
   va_start(ap, n);

   mir_value_t or = MIR_NULL_VALUE;
   for (int i = 0; i < n; i++) {
      mir_value_t t = va_arg(ap, mir_value_t);
      if (mir_is_null(t))
         continue;
      else if (mir_is_null(or))
         or = t;
      else
         or = mir_build_or_trigger(g->mu, or, t);
   }

   va_end(ap);
   return or;
}

static void vlog_lower_edge_fn(mir_unit_t *mu, int edge)
{
   mir_type_t t_context = mir_context_type(mu, ident_new("dummy"));
   mir_type_t t_bool = mir_bool_type(mu);
   mir_type_t t_logic = mir_vec4_type(mu, 1, false);
   mir_type_t t_net_value = mir_int_type(mu, 0, 255);
   mir_type_t t_net_signal = mir_signal_type(mu, t_net_value);

   mir_set_result(mu, t_bool);

   mir_add_param(mu, t_context, MIR_NULL_STAMP, ident_new("context"));
   mir_value_t nets = mir_add_param(mu, t_net_signal, MIR_NULL_STAMP,
                                    ident_new("arg"));

   mir_value_t ptr = mir_build_resolved(mu, nets);
   mir_value_t data = mir_build_load(mu, ptr);
   mir_value_t rvalue = mir_build_pack(mu, t_logic, data);

   mir_value_t level = mir_const_vec(mu, t_logic, edge, 0);
   mir_value_t cmp = mir_build_cmp(mu, MIR_CMP_EQ, rvalue, level);
   mir_build_return(mu, cmp);
}

static void vlog_lower_posedge_fn(mir_unit_t *mu, object_t *obj)
{
   vlog_lower_edge_fn(mu, 1);
}

static void vlog_lower_negedge_fn(mir_unit_t *mu, object_t *obj)
{
   vlog_lower_edge_fn(mu, 0);
}

static mir_value_t vlog_lower_sensitivity(vlog_gen_t *g, vlog_node_t v)
{
   const vlog_kind_t kind = vlog_kind(v);
   switch (kind) {
   case V_REF:
   case V_HIER_REF:
      {
         switch (vlog_kind(vlog_ref(v))) {
         case V_PORT_DECL:
         case V_NET_DECL:
         case V_VAR_DECL:
            {
               mir_type_t t_offset = mir_offset_type(g->mu);

               vlog_lvalue_t lvalue = vlog_lower_lvalue(g, v);
               mir_value_t count = mir_const(g->mu, t_offset, lvalue.size);

               return mir_build_level_trigger(g->mu, lvalue.nets, count);
            }
         case V_PARAM_DECL:
         case V_LOCALPARAM:
            return MIR_NULL_VALUE;
         default:
            CANNOT_HANDLE(v);
         }
      }
      break;
   case V_BIT_SELECT:
   case V_PART_SELECT:
      {
         mir_value_t trigger;
         vlog_node_t prefix = vlog_longest_static_prefix(v);
         if (prefix == v) {
            mir_type_t t_offset = mir_offset_type(g->mu);

            vlog_lvalue_t lvalue = vlog_lower_lvalue(g, v);

            mir_value_t count = mir_const(g->mu, t_offset, lvalue.size);

            // XXX: check in range
            mir_value_t nets =
               mir_build_array_ref(g->mu, lvalue.nets, lvalue.offset);

            trigger = mir_build_level_trigger(g->mu, nets, count);
         }
         else
            trigger = vlog_lower_sensitivity(g, prefix);

         if (kind == V_BIT_SELECT) {
            const int nparams = vlog_params(v);
            for (int i = 0; i < nparams; i++) {
               mir_value_t p = vlog_lower_sensitivity(g, vlog_param(v, i));
               trigger = vlog_or_triggers(g, 2, trigger, p);
            }
         }
         else {
            mir_value_t left = vlog_lower_sensitivity(g, vlog_left(v));
            trigger = vlog_or_triggers(g, 2, trigger, left);
         }

         return trigger;
      }
   case V_EVENT:
      {
         const v_event_kind_t kind = vlog_subkind(v);
         if (kind == V_EVENT_LEVEL)
            return vlog_lower_sensitivity(g, vlog_value(v));
         else {
            vlog_lvalue_t lvalue = vlog_lower_lvalue(g, vlog_value(v));

            ident_t func;
            switch (kind) {
            case V_EVENT_POSEDGE:
               func = ident_new("__posedge");
               mir_defer(mir_get_context(g->mu), func, NULL, MIR_UNIT_FUNCTION,
                         vlog_lower_posedge_fn, NULL);
               break;
            case V_EVENT_NEGEDGE:
               func = ident_new("__negedge");
               mir_defer(mir_get_context(g->mu), func, NULL, MIR_UNIT_FUNCTION,
                         vlog_lower_negedge_fn, NULL);
               break;
            default:
               should_not_reach_here();
            }

            mir_value_t context = mir_build_context_upref(g->mu, 0);
            mir_value_t args[] = { context, lvalue.nets };
            return mir_build_function_trigger(g->mu, func, args, 2);
         }
      }
   case V_NUMBER:
      return MIR_NULL_VALUE;
   case V_BINARY:
      {
         mir_value_t left = vlog_lower_sensitivity(g, vlog_left(v));
         mir_value_t right = vlog_lower_sensitivity(g, vlog_right(v));

         return vlog_or_triggers(g, 2, left, right);
      }
   case V_UNARY:
      return vlog_lower_sensitivity(g, vlog_value(v));
   case V_COND_EXPR:
      {
         mir_value_t value = vlog_lower_sensitivity(g, vlog_value(v));
         mir_value_t left = vlog_lower_sensitivity(g, vlog_left(v));
         mir_value_t right = vlog_lower_sensitivity(g, vlog_right(v));

         return vlog_or_triggers(g, 3, value, left, right);
      }
   case V_CONCAT:
   case V_USER_FCALL:
      {
         mir_value_t trigger = MIR_NULL_VALUE;
         const int nparams = vlog_params(v);
         for (int i = 0; i < nparams; i++) {
            mir_value_t p = vlog_lower_sensitivity(g, vlog_param(v, i));
            trigger = vlog_or_triggers(g, 2, trigger, p);
         }

         return trigger;
      }
   default:
      CANNOT_HANDLE(v);
   }
}

static void vlog_lower_timing(vlog_gen_t *g, vlog_node_t v, bool is_static)
{
   vlog_node_t ctrl = vlog_value(v);
   switch (vlog_kind(ctrl)) {
   case V_DELAY_CONTROL:
      {
         mir_type_t t_time = mir_time_type(g->mu);
         mir_value_t delay = vlog_lower_rvalue(g, vlog_value(ctrl));
         mir_value_t cast = mir_build_cast(g->mu, t_time, delay);

         mir_block_t wait_bb = mir_add_block(g->mu);
         mir_build_wait(g->mu, wait_bb, cast);

         mir_set_cursor(g->mu, wait_bb, MIR_APPEND);
      }
      break;
   case V_EVENT_CONTROL:
      {
         const int nparams = vlog_params(ctrl);

         mir_value_t trigger_var = MIR_NULL_VALUE;
         if (!is_static) {
            mir_value_t trigger = MIR_NULL_VALUE;
            for (int i = 0; i < nparams; i++) {
               mir_value_t p = vlog_lower_sensitivity(g, vlog_param(ctrl, i));
               trigger = vlog_or_triggers(g, 2, trigger, p);
            }

            if (!mir_is_null(trigger)) {
               mir_type_t t_trigger = mir_trigger_type(g->mu);
               trigger_var = mir_add_var(g->mu, t_trigger, MIR_NULL_STAMP,
                                         ident_uniq("trigger"), 0);
               mir_build_store(g->mu, trigger_var, trigger);

               mir_build_sched_event(g->mu, trigger, MIR_NULL_VALUE);
            }
         }

         mir_block_t wait_bb = mir_add_block(g->mu);
         mir_build_wait(g->mu, wait_bb, MIR_NULL_VALUE);

         mir_set_cursor(g->mu, wait_bb, MIR_APPEND);

         if (!mir_is_null(trigger_var)) {
            mir_value_t trigger = mir_build_load(g->mu, trigger_var);
            mir_build_clear_event(g->mu, trigger, MIR_NULL_VALUE);
         }
      }
      break;
   default:
      CANNOT_HANDLE(ctrl);
   }

   vlog_lower_stmts(g, v);
}

static mir_value_t vlog_lower_default_value(vlog_gen_t *g,
                                            const type_info_t *ti)
{
   assert(ti->size < 64);  // TODO

   switch (mir_get_class(g->mu, ti->type)) {
   case MIR_TYPE_VEC2:
      return mir_const_vec(g->mu, ti->type, 0, 0);
   case MIR_TYPE_VEC4:
      {
         uint64_t mask = ~UINT64_C(0);
         if (ti->size < 64) mask >>= 64 - ti->size;

         return mir_const_vec(g->mu, ti->type, mask, mask);
      }
   default:
      should_not_reach_here();
   }
}

static void vlog_lower_blocking_assignment(vlog_gen_t *g, vlog_node_t v)
{
   if (vlog_has_delay(v)) {
      vlog_node_t delay = vlog_delay(v);
      assert(vlog_kind(delay) == V_DELAY_CONTROL);

      mir_block_t delay_bb = mir_add_block(g->mu);

      mir_build_wait(g->mu, delay_bb, vlog_lower_time(g, vlog_value(delay)));

      mir_set_cursor(g->mu, delay_bb, MIR_APPEND);
   }

   mir_value_t value = vlog_lower_rvalue(g, vlog_value(v));
   vlog_assign_variable(g, vlog_target(v), value);
}

static void vlog_lower_non_blocking_assignment(vlog_gen_t *g, vlog_node_t v)
{
   vlog_node_t target = vlog_target(v);

   vlog_lvalue_t lvalue = vlog_lower_lvalue(g, target);

   // XXX: check in range
   mir_value_t nets = mir_build_array_ref(g->mu, lvalue.nets, lvalue.offset);

   mir_value_t value = vlog_lower_rvalue(g, vlog_value(v));
   assert(mir_is_vector(g->mu, value));

   mir_type_t t_offset = mir_offset_type(g->mu);
   mir_type_t t_vec = mir_vec4_type(g->mu, lvalue.size, false);

   mir_value_t resize = mir_build_cast(g->mu, t_vec, value);
   mir_value_t count = mir_const(g->mu, t_offset, lvalue.size);

   mir_value_t tmp = MIR_NULL_VALUE;
   if (lvalue.size > 1) {
      mir_type_t t_elem = mir_logic_type(g->mu);
      mir_type_t t_array = mir_carray_type(g->mu, lvalue.size, t_elem);
      tmp = vlog_get_temp(g, t_array);
   }

   const uint8_t strength = vlog_is_net(target) ? ST_STRONG : 0;
   mir_value_t unpacked = mir_build_unpack(g->mu, resize, strength, tmp);

   mir_type_t t_time = mir_time_type(g->mu);
   mir_value_t reject = mir_const(g->mu, t_time, 0);

   mir_value_t after;
   if (vlog_has_delay(v)) {
      vlog_node_t delay = vlog_delay(v);
      assert(vlog_kind(delay) == V_DELAY_CONTROL);

      after = vlog_lower_time(g, vlog_value(delay));
   }
   else
      after = mir_const(g->mu, t_time, 0);

   mir_build_sched_waveform(g->mu, nets, count, unpacked, reject, after);
}

static void vlog_lower_if(vlog_gen_t *g, vlog_node_t v)
{
   mir_block_t exit_bb = MIR_NULL_BLOCK;

   const int nconds = vlog_conds(v);
   for (int i = 0; i < nconds; i++) {
      vlog_node_t c = vlog_cond(v, i);
      mir_block_t next_bb = MIR_NULL_BLOCK;

      if (vlog_has_value(c)) {
         mir_value_t value = vlog_lower_rvalue(g, vlog_value(c));
         mir_value_t cmp = mir_build_test(g->mu, value);

         mir_block_t btrue = mir_add_block(g->mu);

         if (i == nconds - 1) {
            if (mir_is_null(exit_bb))
               exit_bb = mir_add_block(g->mu);
            next_bb = exit_bb;
         }
         else
            next_bb = mir_add_block(g->mu);

         mir_build_cond(g->mu, cmp, btrue, next_bb);

         mir_set_cursor(g->mu, btrue, MIR_APPEND);
      }

      vlog_lower_stmts(g, c);

      if (!mir_block_finished(g->mu, MIR_NULL_BLOCK)) {
         if (mir_is_null(exit_bb))
            exit_bb = mir_add_block(g->mu);
         mir_build_jump(g->mu, exit_bb);
      }

      if (mir_is_null(next_bb))
         break;

      mir_set_cursor(g->mu, next_bb, MIR_APPEND);
   }

   if (!mir_is_null(exit_bb))
      mir_set_cursor(g->mu, exit_bb, MIR_APPEND);
}

static void vlog_lower_forever(vlog_gen_t *g, vlog_node_t v)
{
   mir_block_t body_bb = mir_add_block(g->mu);
   mir_build_jump(g->mu, body_bb);

   mir_set_cursor(g->mu, body_bb, MIR_APPEND);

   vlog_lower_stmts(g, v);

   mir_build_jump(g->mu, body_bb);
}

static void vlog_lower_repeat(vlog_gen_t *g, vlog_node_t v)
{
   mir_type_t t_offset = mir_offset_type(g->mu);
   mir_value_t i_var = mir_add_var(g->mu, t_offset, MIR_NULL_STAMP,
                                   ident_new("i"), MIR_VAR_TEMP);
   mir_value_t zero = mir_const(g->mu, t_offset, 0);
   mir_build_store(g->mu, i_var, zero);

   mir_value_t rvalue = vlog_lower_rvalue(g, vlog_value(v));
   mir_value_t limit = mir_build_cast(g->mu, t_offset, rvalue);
   mir_value_t enter = mir_build_cmp(g->mu, MIR_CMP_LT, zero, limit);

   mir_block_t body_bb = mir_add_block(g->mu);
   mir_block_t cont_bb = mir_add_block(g->mu);
   mir_build_cond(g->mu, enter, body_bb, cont_bb);

   mir_set_cursor(g->mu, body_bb, MIR_APPEND);

   vlog_lower_stmts(g, v);

   if (!mir_block_finished(g->mu, MIR_NULL_BLOCK)) {
      mir_value_t i_val = mir_build_load(g->mu, i_var);
      mir_value_t one = mir_const(g->mu, t_offset, 1);
      mir_value_t next = mir_build_add(g->mu, t_offset, i_val, one);
      mir_build_store(g->mu, i_var, next);

      mir_value_t done = mir_build_cmp(g->mu, MIR_CMP_LT, next, limit);
      mir_build_cond(g->mu, done, body_bb, cont_bb);
   }

   mir_set_cursor(g->mu, cont_bb, MIR_APPEND);
}

static void vlog_lower_while(vlog_gen_t *g, vlog_node_t v)
{
   mir_block_t test_bb = mir_add_block(g->mu);
   mir_block_t body_bb = mir_add_block(g->mu);
   mir_block_t cont_bb = mir_add_block(g->mu);

   mir_build_jump(g->mu, test_bb);

   mir_set_cursor(g->mu, test_bb, MIR_APPEND);

   mir_value_t rvalue = vlog_lower_rvalue(g, vlog_value(v));
   mir_value_t test = mir_build_test(g->mu, rvalue);
   mir_build_cond(g->mu, test, body_bb, cont_bb);

   mir_set_cursor(g->mu, body_bb, MIR_APPEND);

   vlog_lower_stmts(g, v);

   if (!mir_block_finished(g->mu, MIR_NULL_BLOCK))
      mir_build_jump(g->mu, test_bb);

   mir_set_cursor(g->mu, cont_bb, MIR_APPEND);
}

static void vlog_lower_for_loop(vlog_gen_t *g, vlog_node_t v)
{
   mir_comment(g->mu, "Begin for loop");

   vlog_node_t init = vlog_left(v);
   assert(vlog_kind(init) == V_FOR_INIT);

   assert(vlog_decls(init) == 0);   // TODO

   vlog_lower_stmts(g, init);

   mir_block_t body_bb = mir_add_block(g->mu);
   mir_block_t step_bb = mir_add_block(g->mu);
   mir_block_t test_bb = mir_add_block(g->mu);
   mir_block_t exit_bb = mir_add_block(g->mu);

   mir_build_jump(g->mu, test_bb);

   mir_set_cursor(g->mu, test_bb, MIR_APPEND);

   mir_comment(g->mu, "For loop test");

   mir_value_t test = vlog_lower_rvalue(g, vlog_value(v));
   assert(mir_is_vector(g->mu, test));

   mir_value_t zero = mir_const_vec(g->mu, mir_get_type(g->mu, test), 0, 0);
   mir_value_t cmp = mir_build_cmp(g->mu, MIR_CMP_NEQ, test, zero);
   mir_build_cond(g->mu, cmp, body_bb, exit_bb);

   mir_set_cursor(g->mu, body_bb, MIR_APPEND);

   mir_comment(g->mu, "For loop body");

   vlog_lower_stmts(g, v);

   if (!mir_block_finished(g->mu, MIR_NULL_BLOCK))
      mir_build_jump(g->mu, step_bb);

   mir_set_cursor(g->mu, step_bb, MIR_APPEND);

   mir_comment(g->mu, "For loop step");

   vlog_node_t step = vlog_right(v);
   assert(vlog_kind(step) == V_FOR_STEP);

   const int nstmts = vlog_stmts(step);
   for (int i = 0; i < nstmts; i++) {
      vlog_node_t s = vlog_stmt(step, i);
      if (vlog_kind(s) == V_BASSIGN)  // XXX: should be op assign
         vlog_lower_blocking_assignment(g, s);
      else
         vlog_lower_rvalue(g, s);
   }

   mir_build_jump(g->mu, test_bb);

   mir_set_cursor(g->mu, exit_bb, MIR_APPEND);

   mir_comment(g->mu, "End for loop");
}

static void vlog_lower_case(vlog_gen_t *g, vlog_node_t v)
{
   mir_comment(g->mu, "Begin case statement");

   mir_value_t value = vlog_lower_rvalue(g, vlog_value(v));
   mir_type_t type = mir_get_type(g->mu, value);

   // TODO: use a parallel case for small integer types

   const int nitems = vlog_stmts(v);
   mir_block_t *blocks LOCAL = xmalloc_array(nitems, sizeof(mir_block_t));

   mir_type_t t_logic = mir_vec4_type(g->mu, 1, false);
   mir_value_t zero = mir_const_vec(g->mu, t_logic, 0, 0);

   const mir_vec_op_t op = vlog_subkind(v) == V_CASE_NORMAL
      ? MIR_VEC_CASE_EQ : MIR_VEC_CASEX_EQ;

   for (int i = 0; i < nitems; i++) {
      vlog_node_t item = vlog_stmt(v, i);
      assert(vlog_kind(item) == V_CASE_ITEM);

      blocks[i] = mir_add_block(g->mu);

      mir_block_t else_bb = mir_add_block(g->mu);

      mir_value_t comb = MIR_NULL_VALUE;
      const int nparams = vlog_params(item);
      for (int j = 0; j < nparams; j++) {
         mir_value_t test = vlog_lower_rvalue(g, vlog_param(item, j));
         mir_value_t cast = mir_build_cast(g->mu, type, test);
         mir_value_t case_eq = mir_build_binary(g->mu, op, type, cast, value);
         mir_value_t cmp = mir_build_cmp(g->mu, MIR_CMP_NEQ, case_eq, zero);

         if (mir_is_null(comb))
            comb = cmp;
         else
            comb = mir_build_or(g->mu, comb, cmp);
      }

      if (mir_is_null(comb))
         mir_build_jump(g->mu, blocks[i]);
      else
         mir_build_cond(g->mu, comb, blocks[i], else_bb);

      mir_set_cursor(g->mu, else_bb, MIR_APPEND);
   }

   mir_block_t exit_bb = mir_get_cursor(g->mu, NULL);

   for (int i = 0; i < nitems; i++) {
      vlog_node_t item = vlog_stmt(v, i);
      assert(vlog_kind(item) == V_CASE_ITEM);

      mir_set_cursor(g->mu, blocks[i], MIR_APPEND);
      vlog_lower_stmts(g, item);

      if (!mir_block_finished(g->mu, MIR_NULL_BLOCK))
         mir_build_jump(g->mu, exit_bb);
   }

   mir_set_cursor(g->mu, exit_bb, MIR_APPEND);

   mir_comment(g->mu, "End case statement");
}

static void vlog_lower_return(vlog_gen_t *g, vlog_node_t v)
{
   mir_value_t result = MIR_NULL_VALUE;
   if (vlog_has_value(v)) {
      mir_value_t value = vlog_lower_rvalue(g, vlog_value(v));

      const type_info_t *ti = vlog_type_info(g, vlog_type(vlog_ref(v)));
      result = mir_build_cast(g->mu, ti->type, value);
   }

   mir_build_return(g->mu, result);
}

static void vlog_lower_stmts(vlog_gen_t *g, vlog_node_t v)
{
   const int nstmts = vlog_stmts(v);
   for (int i = 0; i < nstmts; i++) {
      vlog_node_t s = vlog_stmt(v, i);
      mir_set_loc(g->mu, vlog_loc(s));

      switch (vlog_kind(s)) {
      case V_TIMING:
         vlog_lower_timing(g, s, false);
         break;
      case V_BASSIGN:
         vlog_lower_blocking_assignment(g, s);
         break;
      case V_NBASSIGN:
         vlog_lower_non_blocking_assignment(g, s);
         break;
      case V_BLOCK:
         vlog_lower_stmts(g, s);
         break;
      case V_SYS_TCALL:
         vlog_lower_sys_tfcall(g, s);
         break;
      case V_IF:
         vlog_lower_if(g, s);
         break;
      case V_FOREVER:
         vlog_lower_forever(g, s);
         break;
      case V_REPEAT:
         vlog_lower_repeat(g, s);
         break;
      case V_WHILE:
         vlog_lower_while(g, s);
         break;
      case V_FOR_LOOP:
         vlog_lower_for_loop(g, s);
         break;
      case V_CASE:
         vlog_lower_case(g, s);
         break;
      case V_POSTFIX:
         vlog_lower_rvalue(g, s);
         break;
      case V_RETURN:
         vlog_lower_return(g, s);
         return;
      default:
         CANNOT_HANDLE(s);
      }
   }
}

static void vlog_lower_driver(vlog_gen_t *g, vlog_node_t v)
{
   if (vlog_kind(v) == V_CONCAT) {
      const int nparams = vlog_params(v);
      for (int i = 0; i < nparams; i++)
         vlog_lower_driver(g, vlog_param(v, i));
   }
   else {
      mir_type_t t_offset = mir_offset_type(g->mu);

      vlog_node_t prefix = vlog_longest_static_prefix(v);

      vlog_lvalue_t target = vlog_lower_lvalue(g, prefix);

      // XXX: check in range
      mir_value_t nets = mir_build_array_ref(g->mu, target.nets, target.offset);

      mir_value_t one = mir_const(g->mu, t_offset, target.size);

      mir_build_drive_signal(g->mu, nets, one);
   }
}

static void vlog_driver_cb(vlog_node_t v, void *context)
{
   vlog_gen_t *g = context;

   switch (vlog_kind(v)) {
   case V_NBASSIGN:
   case V_ASSIGN:
      vlog_lower_driver(g, vlog_target(v));
      break;
   default:
      break;
   }
}

static void vlog_lower_always(vlog_gen_t *g, vlog_node_t v)
{
   mir_block_t start_bb = mir_add_block(g->mu);
   assert(start_bb.id == 1);

   vlog_visit(v, vlog_driver_cb, g);

   vlog_node_t timing = NULL, s0 = vlog_stmt(v, 0);
   if (vlog_kind(s0) == V_TIMING) {
      timing = s0;

      vlog_node_t ctrl = vlog_value(timing);
      if (vlog_kind(ctrl) == V_EVENT_CONTROL) {
         mir_value_t trigger = MIR_NULL_VALUE;
         const int nparams = vlog_params(ctrl);
         for (int i = 0; i < nparams; i++) {
            mir_value_t t = vlog_lower_sensitivity(g, vlog_param(ctrl, i));
            trigger = vlog_or_triggers(g, 2, trigger, t);
         }

         if (!mir_is_null(trigger))
            mir_build_sched_event(g->mu, trigger, MIR_NULL_VALUE);
      }
      else
         timing = NULL;
   }

   mir_build_return(g->mu, MIR_NULL_VALUE);

   mir_set_cursor(g->mu, start_bb, MIR_APPEND);

   if (timing != NULL)
      vlog_lower_timing(g, timing, true);
   else
      vlog_lower_stmts(g, v);

   if (!mir_block_finished(g->mu, MIR_NULL_BLOCK))
      mir_build_jump(g->mu, start_bb);
}

static void vlog_lower_initial(vlog_gen_t *g, vlog_node_t v)
{
   mir_block_t start_bb = mir_add_block(g->mu);
   assert(start_bb.id == 1);

   vlog_visit(v, vlog_driver_cb, g);

   mir_build_return(g->mu, MIR_NULL_VALUE);

   mir_set_cursor(g->mu, start_bb, MIR_APPEND);

   vlog_lower_stmts(g, v);

   if (!mir_block_finished(g->mu, MIR_NULL_BLOCK))
      mir_build_return(g->mu, MIR_NULL_VALUE);
}

static void vlog_lower_continuous_assign(vlog_gen_t *g, vlog_node_t v)
{
   mir_block_t start_bb = mir_add_block(g->mu);
   assert(start_bb.id == 1);

   vlog_visit(v, vlog_driver_cb, g);

   mir_value_t trigger = vlog_lower_sensitivity(g, vlog_value(v));
   if (!mir_is_null(trigger))
      mir_build_sched_event(g->mu, trigger, MIR_NULL_VALUE);

   mir_build_return(g->mu, MIR_NULL_VALUE);

   mir_set_cursor(g->mu, start_bb, MIR_APPEND);

   vlog_node_t target = vlog_target(v);

   unsigned nlvalues = 1, targetsz = 0;
   vlog_lvalue_t lvalue1, *lvalues = &lvalue1;
   if (vlog_kind(target) == V_CONCAT) {
      const int nparams = nlvalues = vlog_params(target);
      lvalues = xmalloc_array(nparams, sizeof(vlog_lvalue_t));
      for (int i = 0; i < nparams; i++) {
         lvalues[i] = vlog_lower_lvalue(g, vlog_param(target, i));
         targetsz += lvalues[i].size;
      }
   }
   else {
      lvalue1 = vlog_lower_lvalue(g, target);
      targetsz = lvalue1.size;
   }

   mir_value_t value = vlog_lower_rvalue(g, vlog_value(v));
   assert(mir_is_vector(g->mu, value));

   mir_type_t t_vec = mir_vec4_type(g->mu, targetsz, false);
   mir_value_t resize = mir_build_cast(g->mu, t_vec, value);

   mir_value_t tmp = MIR_NULL_VALUE;
   if (targetsz != 1) {
      mir_type_t t_elem = mir_logic_type(g->mu);
      mir_type_t t_array = mir_carray_type(g->mu, targetsz, t_elem);
      tmp = vlog_get_temp(g, t_array);
   }

   mir_type_t t_offset = mir_offset_type(g->mu);

   mir_value_t unpacked = mir_build_unpack(g->mu, resize, ST_STRONG, tmp);

   for (int i = 0, offset = 0; i < nlvalues; offset += lvalues[i].size, i++) {
      // XXX: check in range
      mir_value_t nets = mir_build_array_ref(g->mu, lvalues[i].nets,
                                             lvalues[i].offset);

      mir_value_t count = mir_const(g->mu, t_offset, lvalues[i].size);

      mir_type_t t_time = mir_time_type(g->mu);
      mir_value_t reject = mir_const(g->mu, t_time, 0);

      mir_value_t after;
      if (vlog_has_delay(v)) {
         vlog_node_t delay = vlog_delay(v);
         assert(vlog_kind(delay) == V_DELAY_CONTROL);

         after = vlog_lower_time(g, vlog_value(delay));
      }
      else
         after = mir_const(g->mu, t_time, 0);

      mir_value_t src = unpacked;
      if (targetsz > 1) {
         mir_value_t pos = mir_const(g->mu, t_offset, offset);
         src = mir_build_array_ref(g->mu, unpacked, pos);
      }

      mir_build_sched_waveform(g->mu, nets, count, src, reject, after);
   }

   mir_build_wait(g->mu, start_bb, MIR_NULL_VALUE);

   if (lvalues != &lvalue1)
      free(lvalues);
}

static void vlog_lower_gate_inst(vlog_gen_t *g, vlog_node_t v)
{
   mir_block_t start_bb = mir_add_block(g->mu);
   assert(start_bb.id == 1);

   vlog_lower_driver(g, vlog_target(v));

   mir_type_t t_offset = mir_offset_type(g->mu);
   mir_type_t t_time = mir_time_type(g->mu);
   mir_type_t t_logic = mir_vec4_type(g->mu, 1, false);

   mir_value_t trigger = MIR_NULL_VALUE;
   const int nparams = vlog_params(v);
   int first_term = 0;
   for (int i = 0; i < nparams; i++) {
      vlog_node_t p = vlog_param(v, i);
      if (vlog_kind(p) == V_STRENGTH)
         first_term = i + 1;
      else {
         mir_value_t t = vlog_lower_sensitivity(g, p);
         trigger = vlog_or_triggers(g, 2, trigger, t);
      }
   }

   if (!mir_is_null(trigger))
      mir_build_sched_event(g->mu, trigger, MIR_NULL_VALUE);

   mir_build_return(g->mu, MIR_NULL_VALUE);

   mir_set_cursor(g->mu, start_bb, MIR_APPEND);

   bool negate = false;
   uint8_t strength = ST_STRONG;
   mir_value_t value = MIR_NULL_VALUE;
   const vlog_gate_kind_t kind = vlog_subkind(v);
   switch (kind) {
   case V_GATE_PULLUP:
   case V_GATE_PULLDOWN:
      strength = vlog_subkind(vlog_param(v, 0));
      value = mir_const_vec(g->mu, t_logic, kind == V_GATE_PULLUP, 0);
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

         value = vlog_lower_rvalue(g, vlog_param(v, first_term));

         const int nelems = nparams - first_term;
         for (int i = 1; i < nelems; i++) {
            vlog_node_t p = vlog_param(v, first_term + i);
            mir_value_t arg = vlog_lower_rvalue(g, p);
            value = mir_build_binary(g->mu, op_map[kind], t_logic, value, arg);
         }

         if (negate)
            value = mir_build_unary(g->mu, MIR_VEC_BIT_NOT, t_logic, value);
      }
      break;

   case V_GATE_NOT:
      {
         mir_value_t input = vlog_lower_rvalue(g, vlog_param(v, nparams - 1));
         value = mir_build_unary(g->mu, MIR_VEC_BIT_NOT, t_logic, input);
      }
      break;

   case V_GATE_BUF:
      {
         // Invert twice for correct X/Z behaviour
         mir_value_t input = vlog_lower_rvalue(g, vlog_param(v, nparams - 1));
         value = mir_build_unary(g->mu, MIR_VEC_BIT_NOT, t_logic, input);
         value = mir_build_unary(g->mu, MIR_VEC_BIT_NOT, t_logic, value);
      }
      break;

   default:
      CANNOT_HANDLE(v);
   }

   mir_value_t unpacked =
      mir_build_unpack(g->mu, value, strength, MIR_NULL_VALUE);

   mir_value_t reject = mir_const(g->mu, t_time, 0);
   mir_value_t after = mir_const(g->mu, t_time, 0);

   vlog_lvalue_t lvalue = vlog_lower_lvalue(g, vlog_target(v));
   mir_value_t count = mir_const(g->mu, t_offset, lvalue.size);

   // XXX: check in range
   mir_value_t nets = mir_build_array_ref(g->mu, lvalue.nets, lvalue.offset);

   mir_build_sched_waveform(g->mu, nets, count, unpacked, reject, after);
   mir_build_wait(g->mu, start_bb, MIR_NULL_VALUE);
}

static void vlog_lower_cleanup(vlog_gen_t *g)
{
   if (g->temps != NULL)
      ihash_free(g->temps);
}

void vlog_lower_deferred(mir_unit_t *mu, object_t *obj)
{
   vlog_node_t v = vlog_from_object(obj);
   assert(v != NULL);

   vlog_gen_t g = {
      .mu = mu,
   };

   switch (vlog_kind(v)) {
   case V_ALWAYS:
      vlog_lower_always(&g, v);
      break;
   case V_INITIAL:
      vlog_lower_initial(&g, v);
      break;
   case V_ASSIGN:
      vlog_lower_continuous_assign(&g, v);
      break;
   case V_GATE_INST:
      vlog_lower_gate_inst(&g, v);
      break;
   default:
      CANNOT_HANDLE(v);
   }

   vlog_lower_cleanup(&g);
}

static void vlog_lower_net_decl(mir_unit_t *mu, vlog_node_t v, tree_t wrap,
                                mir_value_t resfn)
{
   mir_type_t t_net_value = mir_int_type(mu, 0, 255);
   mir_type_t t_net_signal = mir_signal_type(mu, t_net_value);
   mir_type_t t_offset = mir_offset_type(mu);

   assert(wrap != NULL);
   assert(!vlog_has_value(v));   // Should have been replaced with assign

   mir_value_t value = mir_const(mu, t_net_value, LOGIC_X);
   mir_value_t count = mir_const(mu, t_offset, vlog_size(vlog_type(v)));
   mir_value_t size = mir_const(mu, t_offset, 1);
   mir_value_t flags = mir_const(mu, t_offset, 0);
   mir_value_t locus = mir_build_locus(mu, tree_to_object(wrap));

   mir_value_t signal = mir_build_init_signal(mu, t_net_value, count, size,
                                              value, flags, locus,
                                              MIR_NULL_VALUE);

   mir_build_resolve_signal(mu, signal, resfn);

   mir_value_t var = mir_add_var(mu, t_net_signal, MIR_NULL_STAMP,
                                 vlog_ident(v), MIR_VAR_SIGNAL);
   mir_build_store(mu, var, signal);

   mir_put_object(mu, v, var);
}

static void vlog_lower_var_decl(vlog_gen_t *g, vlog_node_t v, tree_t wrap)
{
   mir_type_t t_logic = mir_int_type(g->mu, 0, 3);
   mir_type_t t_logic_signal = mir_signal_type(g->mu, t_logic);
   mir_type_t t_offset = mir_offset_type(g->mu);

   assert(wrap != NULL);

   const type_info_t *ti = vlog_type_info(g, vlog_type(v));
   const int total_size = ti->size * vlog_size(v);

   mir_value_t value;
   if (vlog_has_value(v)) {
      mir_value_t tmp = MIR_NULL_VALUE;
      if (ti->size > 1) {
         mir_type_t t_elem = mir_logic_type(g->mu);
         mir_type_t t_array = mir_carray_type(g->mu, total_size, t_elem);
         tmp = vlog_get_temp(g, t_array);
      }

      mir_value_t packed = vlog_lower_rvalue(g, vlog_value(v));
      mir_value_t cast = mir_build_cast(g->mu, ti->type, packed);
      value = mir_build_unpack(g->mu, cast, 0, tmp);
   }
   else
      value = mir_const(g->mu, t_logic, LOGIC_X);

   mir_value_t count = mir_const(g->mu, t_offset, total_size);
   mir_value_t size = mir_const(g->mu, t_offset, 1);
   mir_value_t flags = mir_const(g->mu, t_offset, 0);
   mir_value_t locus = mir_build_locus(g->mu, tree_to_object(wrap));

   mir_value_t signal = mir_build_init_signal(g->mu, t_logic, count, size,
                                              value, flags, locus,
                                              MIR_NULL_VALUE);

   mir_value_t var = mir_add_var(g->mu, t_logic_signal, MIR_NULL_STAMP,
                                 vlog_ident(v), MIR_VAR_SIGNAL);
   mir_build_store(g->mu, var, signal);

   mir_put_object(g->mu, v, var);
}

static void vlog_lower_genvar_decl(vlog_gen_t *g, vlog_node_t v)
{
   const type_info_t *ti = vlog_type_info(g, vlog_type(v));

   mir_value_t var = mir_add_var(g->mu, ti->type, MIR_NULL_STAMP,
                                 vlog_ident(v), 0);
   mir_build_store(g->mu, var, mir_const_vec(g->mu, ti->type, ~0, ~0));

   mir_put_object(g->mu, v, var);
}

static void vlog_lower_locals(vlog_gen_t *g, vlog_node_t v)
{
   const int ndecls = vlog_decls(v);
   for (int i = 0; i < ndecls; i++) {
      vlog_node_t d = vlog_decl(v, i);
      switch (vlog_kind(d)) {
      case V_VAR_DECL:
         {
            const type_info_t *ti = vlog_type_info(g, vlog_type(d));
            mir_value_t var = mir_add_var(g->mu, ti->type, ti->stamp,
                                          vlog_ident(d), 0);
            mir_put_object(g->mu, d, var);

            mir_value_t init;
            if (vlog_has_value(d))
               init = vlog_lower_rvalue(g, vlog_value(d));
            else
               init = vlog_lower_default_value(g, ti);

            mir_build_store(g->mu, var, init);
         }
         break;
      default:
         CANNOT_HANDLE(d);
      }
   }
}

static void vlog_lower_func_decl(mir_unit_t *mu, object_t *obj)
{
   vlog_node_t v = vlog_from_object(obj);
   assert(vlog_kind(v) == V_FUNC_DECL);

   vlog_gen_t g = {
      .mu = mu,
   };

   const type_info_t *ti = vlog_type_info(&g, vlog_type(v));
   mir_set_result(mu, ti->type);

   mir_type_t t_context = mir_context_type(mu, mir_get_parent(mu));
   mir_add_param(mu, t_context, MIR_NULL_STAMP, ident_new("context"));

   const int nports = vlog_ports(v);
   for (int i = 0; i < nports; i++) {
      vlog_node_t port = vlog_port(v, i);
      const type_info_t *pti = vlog_type_info(&g, vlog_type(port));
      mir_value_t value = mir_add_param(mu, pti->type, pti->stamp,
                                        vlog_ident(port));
      mir_put_object(mu, port, value);
   }

   mir_value_t result = mir_add_var(mu, ti->type, ti->stamp, vlog_ident(v), 0);
   mir_put_object(mu, v, result);

   mir_build_store(mu, result, vlog_lower_default_value(&g, ti));

   vlog_lower_locals(&g, v);
   vlog_lower_stmts(&g, v);

   if (!mir_block_finished(mu, MIR_NULL_BLOCK))
      mir_build_return(mu, mir_build_load(mu, result));
}

static mir_type_t vlog_lower_vhdl_type(mir_unit_t *mu, type_t type)
{
   if (type_eq(type, ieee_type(IEEE_STD_ULOGIC)))
      return mir_int_type(mu, 0, 8);
   else if (type_eq(type, ieee_type(IEEE_STD_ULOGIC_VECTOR)))
      return mir_uarray_type(mu, 1, mir_int_type(mu, 0, 8));
   else if (type_eq(type, verilog_type(VERILOG_NET_VALUE)))
      return mir_int_type(mu, 0, 255);
   else if (type_eq(type, verilog_type(VERILOG_LOGIC)))
      return mir_int_type(mu, 0, 3);
   else if (type_eq(type, verilog_type(VERILOG_WIRE_ARRAY))
            || type_eq(type, verilog_type(VERILOG_NET_ARRAY)))
      return mir_uarray_type(mu, 1, mir_int_type(mu, 0, 3));

   fatal_trace("cannot lower VHDL type %s", type_pp(type));
}

static void vlog_lower_converter(mir_unit_t *mu, tree_t cf, mir_value_t in,
                                 mir_value_t out)
{
   tree_t decl = tree_ref(cf);
   assert(tree_kind(decl) == T_FUNC_DECL);

   type_t rtype = type_result(tree_type(decl));

   // Dummy return value to force function calling convention
   mir_type_t t_offset = mir_offset_type(mu);
   mir_set_result(mu, t_offset);

   mir_type_t t_context = mir_context_type(mu, mir_get_parent(mu));
   mir_add_param(mu, t_context, MIR_NULL_STAMP, ident_new("context"));

   mir_type_t t_conv = mir_conversion_type(mu);
   mir_value_t conv =
      mir_add_param(mu, t_conv, MIR_NULL_STAMP, ident_new("cf"));

   mir_type_t t_out = vlog_lower_vhdl_type(mu, rtype);

   ident_t func = tree_ident2(tree_ref(cf));

   mir_value_t pkg = mir_build_link_package(mu, well_known(W_NVC_VERILOG));

   mir_value_t count = MIR_NULL_VALUE;
   if (mir_is(mu, in, MIR_TYPE_UARRAY)) {
      count = mir_build_uarray_len(mu, in, 0);
      in = mir_build_unwrap(mu, in);
   }
   else if (type_is_array(rtype)) {
      int64_t length;
      if (!folded_length(range_of(tree_type(tree_value(cf)), 0), &length))
         should_not_reach_here();

      count = mir_const(mu, t_offset, length);
   }

   mir_value_t resolved = mir_build_resolved(mu, in);

   mir_value_t arg;
   if (!mir_is_null(count)) {
      mir_dim_t dims[] = {
         { .left  = mir_const(mu, t_offset, 1),
           .right = count,
           .dir   = mir_const(mu, mir_bool_type(mu), RANGE_TO),
         }
      };
      arg = mir_build_wrap(mu, resolved, dims, 1);
   }
   else {
      arg = mir_build_load(mu, resolved);
      count = mir_const(mu, t_offset, 1);
   }

   mir_value_t args[] = { pkg, arg };

   mir_value_t result = mir_build_fcall(mu, func, t_out, MIR_NULL_STAMP,
                                        args, ARRAY_LEN(args));

   if (mir_is(mu, result, MIR_TYPE_UARRAY))
      result = mir_build_unwrap(mu, result);

   if (mir_is(mu, out, MIR_TYPE_UARRAY))
      out = mir_build_unwrap(mu, out);

   mir_build_put_conversion(mu, conv, out, count, result);

   mir_build_return(mu, mir_const(mu, t_offset, 0));
}

static void vlog_lower_convert_in(mir_unit_t *mu, object_t *obj)
{
   tree_t map = tree_from_object(obj);
   assert(tree_kind(map) == T_PARAM);

   tree_t cf = tree_value(map);
   assert(tree_kind(cf) == T_CONV_FUNC);

   mir_context_t *mc = mir_get_context(mu);

   ident_t parent1 = mir_get_parent(mu);
   mir_shape_t *shape1 = mir_get_shape(mc, parent1);

   ident_t parent2 = mir_get_shape_parent(shape1);
   mir_shape_t *shape2 = mir_get_shape(mc, parent2);

   tree_t arg = tree_value(cf);
   assert(tree_kind(arg) == T_REF);

   int nth = mir_find_slot(shape2, tree_ident(tree_ref(arg)));
   assert(nth >= 0);

   mir_value_t in_upref = mir_build_var_upref(mu, 2, nth);
   mir_value_t in_nets = mir_build_load(mu, in_upref);

   int hops;
   mir_value_t var = mir_search_object(mu, map, &hops);
   assert(!mir_is_null(var));
   assert(hops == 1);

   mir_value_t out_upref = mir_build_var_upref(mu, hops, var.id);
   mir_value_t out_nets = mir_build_load(mu, out_upref);

   vlog_lower_converter(mu, cf, in_nets, out_nets);
}

static void vlog_lower_convert_out(mir_unit_t *mu, object_t *obj)
{
   tree_t map = tree_from_object(obj);
   assert(tree_kind(map) == T_PARAM);

   tree_t cf = tree_name(map);
   assert(tree_kind(cf) == T_CONV_FUNC);

   mir_context_t *mc = mir_get_context(mu);

   ident_t parent1 = mir_get_parent(mu);
   mir_shape_t *shape1 = mir_get_shape(mc, parent1);

   ident_t parent2 = mir_get_shape_parent(shape1);
   mir_shape_t *shape2 = mir_get_shape(mc, parent2);

   int hops;
   mir_value_t var = mir_search_object(mu, map, &hops);
   assert(!mir_is_null(var));
   assert(hops == 1);

   mir_value_t in_upref = mir_build_var_upref(mu, hops, var.id);
   mir_value_t in_nets = mir_build_load(mu, in_upref);

   tree_t dst = tree_value(map);
   assert(tree_kind(dst) == T_REF);

   int nth = mir_find_slot(shape2, tree_ident(tree_ref(dst)));
   assert(nth >= 0);

   mir_value_t out_upref = mir_build_var_upref(mu, 2, nth);
   mir_value_t out_nets = mir_build_load(mu, out_upref);

   vlog_lower_converter(mu, cf, in_nets, out_nets);
}

void vlog_lower_block(mir_context_t *mc, ident_t parent, tree_t b)
{
   tree_t hier = tree_decl(b, 0);
   assert(tree_kind(hier) == T_HIER);

   mir_shape_t *shape = mir_get_shape(mc, parent);
   ident_t qual = tree_ident2(hier);
   mir_unit_t *mu = mir_unit_new(mc, qual, tree_to_object(b),
                                 MIR_UNIT_INSTANCE, shape);

   tree_t wrap = tree_ref(hier);
   assert(tree_kind(wrap) == T_VERILOG);

   vlog_node_t body = tree_vlog(wrap);
   const vlog_kind_t body_kind = vlog_kind(body);
   assert(body_kind == V_INST_BODY || body_kind == V_BLOCK);

   hash_t *map = hash_new(16);

   const int vhdl_ndecls = tree_decls(b);
   const int vlog_ndecls = vlog_decls(body);

   for (int i = 1, pos = 0; i < vhdl_ndecls; i++) {
      tree_t t = tree_decl(b, i);
      ident_t id = tree_ident(t);
      for (; pos < vlog_ndecls; pos++) {
         vlog_node_t v = vlog_decl(body, pos);
         if (vlog_kind(v) == V_PORT_DECL)
            continue;
         else if (vlog_ident(v) == id) {
            hash_put(map, v, t);
            break;
         }
      }

      if (pos == vlog_ndecls)
         fatal_trace("missing VHDL signal for %s", istr(id));
   }

   const int vhdl_nports = tree_ports(b);
   const int vlog_nports = body_kind == V_INST_BODY ? vlog_ports(body) : 0;

   for (int i = 0; i < vhdl_nports; i++)
      hash_put(map, vlog_ref(vlog_ref(vlog_port(body, i))), tree_port(b, i));

   ident_t pkg_name = well_known(W_NVC_VERILOG);
   mir_value_t pkg = mir_build_package_init(mu, pkg_name, MIR_NULL_VALUE);

   mir_type_t t_net_value = mir_int_type(mu, 0, 255);
   mir_type_t t_resolution = mir_resolution_type(mu, t_net_value);

   ident_t var_name = ident_new("NVC.VERILOG.T_WIRE$resolution");
   mir_value_t resfn =
      mir_build_link_var(mu, pkg_name, pkg, var_name, t_resolution);

   vlog_gen_t g = {
      .mu = mu,
   };

   for (int i = 0; i < vlog_ndecls; i++) {
      vlog_node_t d = vlog_decl(body, i);

      switch (vlog_kind(d)) {
      case V_PORT_DECL:
         break;   // Translated below
      case V_NET_DECL:
         vlog_lower_net_decl(mu, d, hash_get(map, d), resfn);
         break;
      case V_VAR_DECL:
         vlog_lower_var_decl(&g, d, hash_get(map, d));
         break;
      case V_GENVAR_DECL:
         vlog_lower_genvar_decl(&g, d);
         break;
      case V_LOCALPARAM:
         break;  // Always inlined for now
      case V_FUNC_DECL:
         mir_defer(mc, ident_prefix(vlog_ident2(d), vlog_ident(d), '.'), qual,
                   MIR_UNIT_FUNCTION, vlog_lower_func_decl, vlog_to_object(d));
         break;
      default:
         CANNOT_HANDLE(d);
      }
   }

   mir_value_t self = mir_build_context_upref(mu, 0);
   mir_type_t t_self = mir_context_type(mu, qual);
   mir_type_t t_offset = mir_offset_type(mu);

   for (int i = 0; i < vlog_nports; i++) {
      vlog_node_t ref = vlog_port(body, i);
      assert(vlog_kind(ref) == V_REF);

      vlog_node_t port = vlog_ref(ref);
      assert(vlog_kind(port) == V_PORT_DECL);

      int hops;
      mir_value_t var = mir_search_object(mu, vlog_ref(port), &hops);
      assert(!mir_is_null(var));
      assert(hops == 0);

      mir_put_object(mu, port, var);

      if (i >= vhdl_nports)
         continue;   // Port conversion only for mixed instantiation

      mir_value_t count = mir_const(mu, t_offset, vlog_size(vlog_type(port)));

      tree_t map = tree_param(b, i);
      tree_t value = tree_value(map);

      mir_value_t in_conv = MIR_NULL_VALUE;
      if (tree_kind(value) == T_CONV_FUNC) {
         mir_put_object(mu, map, var);

         ident_t func = ident_sprintf("%s.%s$verilog_convert_in",
                                      istr(qual), istr(vlog_ident(port)));
         mir_defer(mc, func, qual, MIR_UNIT_FUNCTION, vlog_lower_convert_in,
                      tree_to_object(map));

         mir_value_t closure =
            mir_build_closure(mu, func, self, t_self, t_offset);
         in_conv = mir_build_port_conversion(mu, closure, closure);

         value = tree_value(value);
      }

      assert(tree_kind(value) == T_REF);

      tree_t vhdl_port = tree_ref(value);
      assert(tree_kind(vhdl_port) == T_PORT_DECL);

      int nth = mir_find_slot(shape, tree_ident(vhdl_port));
      assert(nth >= 0);

      mir_value_t upref = mir_build_var_upref(mu, 1, nth);
      mir_value_t dst_nets = mir_build_load(mu, upref);

      type_t vhdl_type = tree_type(vhdl_port);
      assert(dimension_of(vhdl_type) == 1);

      mir_value_t dst_count;
      if (mir_is(mu, dst_nets, MIR_TYPE_UARRAY)) {
         dst_count = mir_build_uarray_len(mu, dst_nets, 0);
         dst_nets = mir_build_unwrap(mu, dst_nets);
      }
      else if (type_is_array(vhdl_type)) {
         int64_t cval;
         if (folded_length(range_of(vhdl_type, 0), &cval))
            dst_count = mir_const(mu, t_offset, cval);
         else
            should_not_reach_here();
      }
      else if (type_is_scalar(vhdl_type))
         dst_count = mir_const(mu, t_offset, 1);
      else
         should_not_reach_here();

      mir_value_t out_conv = MIR_NULL_VALUE;
      if (tree_subkind(map) == P_NAMED) {
         tree_t name = tree_name(map);
         if (tree_kind(name) == T_CONV_FUNC) {
            mir_put_object(mu, map, var);

            ident_t func = ident_sprintf("%s.%s$verilog_convert_out",
                                         istr(qual), istr(vlog_ident(port)));
            mir_defer(mc, func, qual, MIR_UNIT_FUNCTION, vlog_lower_convert_out,
                      tree_to_object(map));

            mir_value_t closure =
               mir_build_closure(mu, func, self, t_self, t_offset);
            out_conv = mir_build_port_conversion(mu, closure, closure);
         }
      }

      mir_value_t src_nets = mir_build_load(mu, var);
      mir_value_t src_count =
         mir_const(mu, t_offset, vlog_size(vlog_type(port)));

      mir_value_t locus = mir_build_locus(mu, tree_to_object(map));
      mir_build_length_check(mu, src_count, dst_count, locus, MIR_NULL_VALUE);

      switch (vlog_subkind(port)) {
      case V_PORT_INPUT:
         if (mir_is_null(in_conv))
            mir_build_map_signal(mu, dst_nets, src_nets, count);
         else {
            mir_build_convert_out(mu, in_conv, src_nets, count);
            mir_build_convert_in(mu, in_conv, dst_nets, count);
         }
         break;
      case V_PORT_OUTPUT:
         if (mir_is_null(out_conv))
            mir_build_map_signal(mu, src_nets, dst_nets, count);
         else {
            mir_build_convert_out(mu, out_conv, dst_nets, count);
            mir_build_convert_in(mu, out_conv, src_nets, count);
         }
         break;
      default:
         CANNOT_HANDLE(port);
      }
   }

   mir_build_return(mu, MIR_NULL_VALUE);

   vlog_lower_cleanup(&g);

   hash_free(map);

   mir_optimise(mu, MIR_PASS_O1);
   mir_put_unit(mc, mu);
}

mir_unit_t *vlog_lower_thunk(mir_context_t *mc, ident_t parent, vlog_node_t v)
{
   mir_unit_t *mu = mir_unit_new(mc, NULL, vlog_to_object(v), MIR_UNIT_THUNK,
                                 mir_get_shape(mc, parent));

   vlog_gen_t g = {
      .mu = mu,
   };

   switch (vlog_kind(v)) {
   case V_FOR_INIT:
      assert(vlog_decls(v) == 0);   // TODO
      // Fall-through
   case V_FOR_STEP:
      vlog_lower_stmts(&g, v);
      mir_build_return(mu, MIR_NULL_VALUE);
      break;
   default:
      {
         mir_value_t value = vlog_lower_rvalue(&g, v);
         mir_set_result(mu, mir_get_type(mu, value));
         mir_build_return(mu, value);
      }
   }

   vlog_lower_cleanup(&g);
   return mu;
}
