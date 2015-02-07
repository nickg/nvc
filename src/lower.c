//
//  Copyright (C) 2014-2015  Nick Gasson
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
#include "phase.h"
#include "vcode.h"
#include "common.h"
#include "rt/rt.h"

#include <assert.h>
#include <stdlib.h>
#include <inttypes.h>
#include <string.h>

typedef enum {
   EXPR_LVALUE,
   EXPR_RVALUE
} expr_ctx_t;

typedef enum {
   PATH_NAME,
   INSTANCE_NAME
} name_attr_t;

typedef struct loop_stack loop_stack_t;

struct loop_stack {
   loop_stack_t  *up;
   ident_t        name;
   vcode_block_t  test_bb;
   vcode_block_t  exit_bb;
};

static ident_t builtin_i;
static ident_t foreign_i;
static ident_t nested_i;
static ident_t vcode_obj_i;
static ident_t drives_all_i;
static ident_t driver_init_i;
static ident_t static_i;
static ident_t never_waits_i;
static ident_t mangled_i;
static bool    verbose = false;

static vcode_reg_t lower_expr(tree_t expr, expr_ctx_t ctx);
static vcode_reg_t lower_reify_expr(tree_t expr);
static vcode_type_t lower_bounds(type_t type);
static void lower_stmt(tree_t stmt, loop_stack_t *loops);
static void lower_func_body(tree_t body, vcode_unit_t context);
static void lower_proc_body(tree_t body, vcode_unit_t context);
static vcode_reg_t lower_signal_ref(tree_t decl, expr_ctx_t ctx);
static vcode_reg_t lower_record_aggregate(tree_t expr, bool nest,
                                          bool is_const, expr_ctx_t ctx);
static vcode_reg_t lower_param_ref(tree_t decl, expr_ctx_t ctx);
static vcode_type_t lower_type(type_t type);

typedef vcode_reg_t (*lower_signal_flag_fn_t)(vcode_reg_t, vcode_reg_t);
typedef vcode_reg_t (*arith_fn_t)(vcode_reg_t, vcode_reg_t);

static bool lower_is_const(tree_t t)
{
   if (tree_kind(t) == T_AGGREGATE) {
      bool is_const = true;
      const int nassocs = tree_assocs(t);
      for (int i = 0; i < nassocs; i++) {
         tree_t a = tree_assoc(t, i);
         is_const = is_const && lower_is_const(tree_value(a));
      }
      return is_const;
   }
   else
      return tree_kind(t) == T_LITERAL
         || (tree_kind(t) == T_REF && tree_kind(tree_ref(t)) == T_ENUM_LIT);
}

static bool lower_const_bounds(type_t type)
{
   assert(type_is_array(type));

   if (type_is_unconstrained(type))
      return false;
   else {
      const int ndims = type_dims(type);
      for (int i = 0; i < ndims; i++) {
         range_t r = type_dim(type, i);
         if (!lower_is_const(r.left) || !lower_is_const(r.right))
            return false;
      }

      type_t elem = type_elem(type);
      return type_is_array(elem) ? lower_const_bounds(elem) : true;
   }
}

static vcode_reg_t lower_array_data(vcode_reg_t reg)
{
   vcode_type_t type = vcode_reg_type(reg);
   switch (vtype_kind(type)) {
   case VCODE_TYPE_UARRAY:
      return emit_unwrap(reg);

   case VCODE_TYPE_POINTER:
   case VCODE_TYPE_SIGNAL:
      return reg;

   case VCODE_TYPE_CARRAY:
      return emit_cast(vtype_pointer(vtype_elem(type)), reg);

   default:
      vcode_dump();
      fatal_trace("invalid type in lower_array_data r%d", reg);
   }
}

static vcode_type_t lower_index_type(type_t type, int dim)
{
   if (type_is_unconstrained(type))
      return lower_type(type_index_constr(type, dim));
   else
      return lower_type(tree_type(type_dim(type, dim).left));
}

static vcode_reg_t lower_array_left(type_t type, int dim, vcode_reg_t reg)
{
   if (reg != VCODE_INVALID_REG
       && vtype_kind(vcode_reg_type(reg)) == VCODE_TYPE_UARRAY)
      return emit_cast(lower_index_type(type, dim),
                       emit_uarray_left(reg, dim));
   else {
      range_t r = type_dim(type, dim);
      return lower_reify_expr(r.left);
   }
}

static vcode_reg_t lower_array_right(type_t type, int dim, vcode_reg_t reg)
{
   if (reg != VCODE_INVALID_REG
       && vtype_kind(vcode_reg_type(reg)) == VCODE_TYPE_UARRAY)
      return emit_cast(lower_index_type(type, dim),
                       emit_uarray_right(reg, dim));
   else {
      range_t r = type_dim(type, dim);
      return lower_reify_expr(r.right);
   }
}

static vcode_reg_t lower_array_dir(type_t type, int dim, vcode_reg_t reg)
{
   if (reg != VCODE_INVALID_REG
       && vtype_kind(vcode_reg_type(reg)) == VCODE_TYPE_UARRAY)
      return emit_uarray_dir(reg, dim);
   else {
      assert(!type_is_unconstrained(type));
      range_t r = type_dim(type, dim);
      switch (r.kind) {
      case RANGE_TO:
      case RANGE_DOWNTO:
         return emit_const(vtype_bool(), r.kind);

      case RANGE_DYN:
      case RANGE_RDYN:
         {
            assert(tree_kind(r.left) == T_FCALL);
            assert(tree_kind(r.right) == T_FCALL);

            tree_t base_ref = tree_value(tree_param(r.left, 1));
            assert(tree_kind(base_ref) == T_REF);

            type_t base_type = tree_type(base_ref);
            assert(type_is_array(base_type));

            vcode_reg_t base_reg = lower_expr(base_ref, EXPR_RVALUE);
            assert(vtype_kind(vcode_reg_type(base_reg)) == VCODE_TYPE_UARRAY);

            vcode_reg_t base_dir = lower_array_dir(base_type, dim, base_reg);
            return r.kind == RANGE_RDYN ? emit_not(base_dir) : base_dir;
         }

      case RANGE_EXPR:
         fatal_trace("unexpected range direction in %s", __func__);
      }
   }
}

static vcode_reg_t lower_array_len(type_t type, int dim, vcode_reg_t reg)
{
   const bool have_uarray =
      reg != VCODE_INVALID_REG
      && vtype_kind(vcode_reg_type(reg)) == VCODE_TYPE_UARRAY;

   vcode_reg_t len_reg = VCODE_INVALID_REG;
   if (type_is_unconstrained(type) || have_uarray) {
      assert(reg != VCODE_INVALID_REG);

      vcode_reg_t left_reg  = emit_uarray_left(reg, dim);
      vcode_reg_t right_reg = emit_uarray_right(reg, dim);

      vcode_reg_t downto_reg = emit_sub(left_reg, right_reg);
      vcode_reg_t upto_reg   = emit_sub(right_reg, left_reg);
      vcode_reg_t diff = emit_select(emit_uarray_dir(reg, dim),
                                     downto_reg, upto_reg);

      len_reg = emit_add(diff, emit_const(vcode_reg_type(left_reg), 1));
   }
   else {
      range_t r = type_dim(type, dim);

      int64_t low, high;
      if (folded_bounds(r, &low, &high))
         return emit_const(vtype_offset(), MAX(high - low + 1, 0));

      vcode_reg_t left_reg  = lower_reify_expr(r.left);
      vcode_reg_t right_reg = lower_reify_expr(r.right);

      vcode_reg_t diff = VCODE_INVALID_REG;
      switch (r.kind) {
      case RANGE_TO:
         diff = emit_sub(right_reg, left_reg);
         break;
      case RANGE_DOWNTO:
         diff = emit_sub(left_reg, right_reg);
         break;
      case RANGE_DYN:
      case RANGE_RDYN:
         diff = emit_select(lower_array_dir(type, dim, VCODE_INVALID_REG),
                            emit_sub(left_reg, right_reg),
                            emit_sub(right_reg, left_reg));
         break;

      case RANGE_EXPR:
         fatal_trace("unexpected range direction %d in lower_array_len",
                     r.kind);
      }

      len_reg = emit_add(diff, emit_const(vcode_reg_type(left_reg), 1));
   }

   vcode_type_t offset_type = vtype_offset();
   vcode_reg_t cast_reg = emit_cast(offset_type, len_reg);
   vcode_reg_t zero_reg = emit_const(offset_type, 0);
   vcode_reg_t neg_reg = emit_cmp(VCODE_CMP_LT, cast_reg, zero_reg);

   return emit_select(neg_reg, zero_reg, cast_reg);
}

static vcode_reg_t lower_array_total_len(type_t type, vcode_reg_t reg)
{
   const int ndims = array_dimension(type);

   vcode_reg_t total = VCODE_INVALID_REG;
   for (int i = 0; i < ndims; i++) {
      vcode_reg_t this = lower_array_len(type, i, reg);
      if (total == VCODE_INVALID_REG)
         total = this;
      else
         total = emit_mul(this, total);
   }

   type_t elem = type_elem(type);
   if (type_is_array(elem))
      return emit_mul(total, lower_array_total_len(elem, VCODE_INVALID_REG));
   else
      return total;
}

static int lower_array_const_size(type_t type)
{
   const int ndims = array_dimension(type);

   int size = 1;
   for (int i = 0; i < ndims; i++) {
      range_t r = type_dim(type, i);
      int64_t low, high;
      range_bounds(r, &low, &high);
      size *= MAX(high - low + 1, 0);
   }

   type_t elem = type_elem(type);
   return type_is_array(elem) ? size * lower_array_const_size(elem) : size;
}

static vcode_type_t lower_array_type(type_t type)
{
   type_t elem = type_elem(type);
   while (type_is_array(elem))
      elem = type_elem(elem);

   vcode_type_t elem_type   = lower_type(elem);
   vcode_type_t elem_bounds = lower_bounds(elem);

   if (lower_const_bounds(type))
      return vtype_carray(lower_array_const_size(type), elem_type, elem_bounds);
   else
      return vtype_uarray(array_dimension(type), elem_type, elem_bounds);
}

static vcode_type_t lower_type(type_t type)
{
   switch (type_kind(type)) {
   case T_SUBTYPE:
      if (type_is_array(type))
         return lower_array_type(type);
      else
         return lower_type(type_base(type));

   case T_UARRAY:
   case T_CARRAY:
         return lower_array_type(type);

   case T_PHYSICAL:
   case T_INTEGER:
      {
         range_t r = type_dim(type, 0);
         int64_t low, high;
         range_bounds(r, &low, &high);
         return vtype_int(low, high);
      }

   case T_ENUM:
      return vtype_int(0, type_enum_literals(type) - 1);

   case T_RECORD:
      {
         ident_t name = type_ident(type);
         uint32_t index = type_index(type);
         vcode_type_t record = vtype_named_record(name, index, false);
         if (record == VCODE_INVALID_TYPE) {
            record = vtype_named_record(name, index, true);

            const int nfields = type_fields(type);
            vcode_type_t fields[nfields];
            for (int i = 0; i < nfields; i++)
               fields[i] = lower_type(tree_type(type_field(type, i)));

            vtype_set_record_fields(record, fields, nfields);
         }

         return record;
      }

   case T_FILE:
      return vtype_file(lower_type(type_file(type)));

   case T_ACCESS:
      {
         type_t access = type_access(type);
         if (type_is_array(access) && lower_const_bounds(access))
            return vtype_access(lower_type(type_elem(access)));
         else
            return vtype_access(lower_type(access));
      }

   case T_REAL:
      return vtype_real();

   default:
      fatal("cannot lower type kind %s", type_kind_str(type_kind(type)));
   }
}

static vcode_type_t lower_bounds(type_t type)
{
   if (type_is_scalar(type) && type_kind(type) == T_SUBTYPE) {
      range_t r = type_dim(type, 0);
      int64_t low, high;
      range_bounds(r, &low, &high);
      return vtype_int(low, high);
   }
   else
      return lower_type(type);
}

static vcode_reg_t lower_reify(vcode_reg_t reg)
{
   if (reg == VCODE_INVALID_REG)
      return reg;

   switch (vtype_kind(vcode_reg_type(reg))) {
   case VCODE_TYPE_POINTER:
      return emit_load_indirect(reg);
   case VCODE_TYPE_SIGNAL:
      {
         vcode_reg_t ptr = emit_vec_load(reg, emit_const(vtype_offset(), 1));
         return emit_load_indirect(ptr);
      }

   default:
      return reg;
   }
}

static vcode_reg_t lower_reify_expr(tree_t expr)
{
   return lower_reify(lower_expr(expr, EXPR_RVALUE));
}

static vcode_reg_t lower_wrap(type_t type, vcode_reg_t data)
{
   assert(type_is_array(type));

   const int ndims = array_dimension(type);
   vcode_dim_t dims[ndims];
   for (int i = 0; i < ndims; i++) {
      dims[i].left  = lower_array_left(type, i, data);
      dims[i].right = lower_array_right(type, i, data);
      dims[i].dir   = lower_array_dir(type, i, data);
   }

   return emit_wrap(lower_array_data(data), dims, ndims);
}

static vcode_reg_t lower_subprogram_arg(tree_t fcall, unsigned nth)
{
   if (nth >= tree_params(fcall))
      return VCODE_INVALID_REG;

   tree_t param = tree_param(fcall, nth);

   assert(tree_subkind(param) == P_POS);
   assert(tree_pos(param) == nth);

   tree_t value = tree_value(param);
   type_t value_type = tree_type(value);

   tree_t decl = tree_ref(fcall);

   port_mode_t mode = PORT_IN;
   if (nth < tree_ports(decl))
      mode = tree_subkind(tree_port(decl, nth));

   const bool must_reify =
      (type_is_scalar(value_type) || type_is_access(value_type)
       || type_is_file(value_type))
      && mode == PORT_IN;

   if (tree_attr_str(decl, builtin_i))
      return must_reify
         ? lower_reify_expr(value)
         : lower_expr(value, mode == PORT_IN ? EXPR_RVALUE : EXPR_LVALUE);

   tree_t port = tree_port(decl, nth);
   type_t port_type = tree_type(port);

   if (tree_class(port) == C_SIGNAL)
      return lower_expr(value, EXPR_LVALUE);

   vcode_reg_t reg =
      lower_expr(value, mode == PORT_IN ? EXPR_RVALUE : EXPR_LVALUE);

   if (type_is_array(value_type)) {
      const bool have_uarray =
         vtype_kind(vcode_reg_type(reg)) == VCODE_TYPE_UARRAY;
      const bool need_uarray = !lower_const_bounds(port_type);

      if (have_uarray && need_uarray)
         return reg;
      else if (!have_uarray && need_uarray) {
         // Need to wrap array with metadata
         return lower_wrap(value_type, reg);
      }
      else if (have_uarray && !need_uarray) {
         // Need to unwrap array to get raw pointer
         return emit_unwrap(reg);
      }
      else
         return reg;
   }
   else
      return must_reify ? lower_reify(reg) : reg;
}

static vcode_reg_t lower_array_cmp_inner(vcode_reg_t lhs_data,
                                         vcode_reg_t rhs_data,
                                         vcode_reg_t lhs_array,
                                         vcode_reg_t rhs_array,
                                         type_t left_type, type_t right_type,
                                         vcode_cmp_t pred)
{
   // Behaviour of relational operators on arrays is described in
   // LRM 93 section 7.2.2

   assert(pred == VCODE_CMP_EQ || pred == VCODE_CMP_LT
          || pred == VCODE_CMP_LEQ);

   vcode_reg_t left_len  = lower_array_len(left_type, 0, lhs_array);
   vcode_reg_t right_len = lower_array_len(right_type, 0, rhs_array);

   vcode_reg_t i_reg = emit_alloca(vtype_offset(), vtype_offset(),
                                   VCODE_INVALID_REG);
   emit_store_indirect(emit_const(vtype_offset(), 0), i_reg);

   vcode_block_t init_bb = vcode_active_block();
   vcode_block_t test_bb = emit_block();
   vcode_block_t body_bb = emit_block();
   vcode_block_t exit_bb = emit_block();

   vcode_reg_t len_eq = emit_cmp(VCODE_CMP_EQ, left_len, right_len);

   if (pred == VCODE_CMP_EQ)
      emit_cond(len_eq, test_bb, exit_bb);
   else
      emit_jump(test_bb);

   // Loop test

   vcode_select_block(test_bb);

   vcode_reg_t i_loaded = emit_load_indirect(i_reg);
   vcode_reg_t len_ge_l = emit_cmp(VCODE_CMP_GEQ, i_loaded, left_len);
   vcode_reg_t len_ge_r = emit_cmp(VCODE_CMP_GEQ, i_loaded, right_len);
   emit_cond(emit_or(len_ge_l, len_ge_r), exit_bb, body_bb);

   // Loop body

   vcode_select_block(body_bb);

   vcode_reg_t l_ptr = emit_add(lhs_data, i_loaded);
   vcode_reg_t r_ptr = emit_add(rhs_data, i_loaded);

   type_t elem_type = type_elem(left_type);
   vcode_reg_t cmp, eq;
   if (type_is_array(elem_type)) {
      cmp = eq = lower_array_cmp_inner(l_ptr, r_ptr,
                                       VCODE_INVALID_REG,
                                       VCODE_INVALID_REG,
                                       type_elem(left_type),
                                       type_elem(right_type), pred);
      body_bb = vcode_active_block();
   }
   else {
      if (type_is_record(elem_type)) {
         assert(false);
         //cmp = eq = cgen_record_eq(l_ptr, r_ptr, elem_type, pred, ctx);
         //body_bb = LLVMGetInsertBlock(builder);
      }
      else {
         vcode_reg_t l_val = emit_load_indirect(l_ptr);
         vcode_reg_t r_val = emit_load_indirect(r_ptr);

         cmp = emit_cmp(pred, l_val, r_val);
         eq  = (pred == VCODE_CMP_EQ) ? cmp
            : emit_cmp(VCODE_CMP_EQ, l_val, r_val);
      }
   }

   vcode_reg_t inc = emit_add(i_loaded, emit_const(vtype_offset(), 1));
   emit_store_indirect(inc, i_reg);

   vcode_reg_t i_eq_len = emit_cmp(VCODE_CMP_EQ, inc, left_len);
   vcode_reg_t done = emit_or(emit_not(eq), emit_and(len_eq, i_eq_len));

   emit_cond(done, exit_bb, test_bb);

   // Epilogue

   vcode_select_block(exit_bb);

   vcode_reg_t   values[] = { cmp,     len_ge_l, len_eq  };
   vcode_block_t bbs[]    = { body_bb, test_bb,  init_bb };
   return emit_phi(values, bbs, (pred == VCODE_CMP_EQ) ? 3 : 2);
}

static vcode_reg_t lower_array_cmp(vcode_reg_t r0, vcode_reg_t r1,
                                   type_t r0_type, type_t r1_type,
                                   vcode_cmp_t pred)
{
   const bool can_use_memcmp =
      pred == VCODE_CMP_EQ && type_is_scalar(type_elem(r0_type));

   vcode_reg_t r0_data = lower_array_data(r0);
   vcode_reg_t r1_data = lower_array_data(r1);

   if (can_use_memcmp) {
      vcode_reg_t r0_len = lower_array_len(r0_type, 0, r0);
      vcode_reg_t r1_len = lower_array_len(r1_type, 0, r1);

      if (r0_len == r1_len)
         return emit_memcmp(r0_data, r1_data, r0_len);
      else
         return emit_and(emit_cmp(VCODE_CMP_EQ, r0_len, r1_len),
                         emit_memcmp(r0_data, r1_data, r0_len));
   }
   else
      return lower_array_cmp_inner(r0_data, r1_data, r0, r1,
                                   r0_type, r1_type, pred);
}

static vcode_reg_t lower_signal_flag(tree_t ref, lower_signal_flag_fn_t fn)
{
   tree_t decl = tree_ref(ref);
   switch (tree_kind(decl)) {
   case T_SIGNAL_DECL:
      {
         vcode_reg_t nets   = lower_signal_ref(decl, EXPR_LVALUE);
         vcode_reg_t length = emit_const(vtype_offset(), tree_nets(decl));

         return (*fn)(nets, length);
      }

   case T_PORT_DECL:
      {
         vcode_reg_t nets = lower_param_ref(decl, EXPR_LVALUE);

         vcode_reg_t length = VCODE_INVALID_REG;
         type_t type = tree_type(decl);
         if (type_is_array(type))
            length = lower_array_len(type, 0, nets);
         else
            length = emit_const(vtype_offset(), 1);

         return (*fn)(nets, length);
      }

   default:
      fatal_at(tree_loc(ref), "invalid signal flag operation");
   }
}

static vcode_reg_t lower_record_eq(vcode_reg_t r0, vcode_reg_t r1, type_t type)
{
   vcode_reg_t result = emit_const(vtype_bool(), true);

   const int nfields = type_fields(type);
   for (int i = 0; i < nfields; i++) {
      vcode_reg_t lfield = emit_record_ref(r0, i);
      vcode_reg_t rfield = emit_record_ref(r1, i);

      vcode_reg_t cmp = VCODE_INVALID_REG;
      type_t ftype = tree_type(type_field(type, i));
      if (type_is_array(ftype))
         cmp = lower_array_cmp(lfield, rfield, ftype, ftype, VCODE_CMP_EQ);
      else if (type_is_record(ftype))
         cmp = lower_record_eq(lfield, rfield, ftype);
      else {
         vcode_reg_t lload = emit_load_indirect(lfield);
         vcode_reg_t rload = emit_load_indirect(rfield);
         cmp = emit_cmp(VCODE_CMP_EQ, lload, rload);
      }

      result = emit_and(result, cmp);
   }

   return result;
}

static type_t lower_arg_type(tree_t fcall, int nth)
{
   if (nth >= tree_params(fcall))
      return NULL;
   else
      return tree_type(tree_value(tree_param(fcall, nth)));
}

static void lower_mangle_one_type(text_buf_t *buf, type_t type)
{
   ident_t ident = type_ident(type);

   if (icmp(ident, "STD.STANDARD.INTEGER"))
      tb_printf(buf, "I");
   else if (icmp(ident, "STD.STANDARD.STRING"))
      tb_printf(buf, "S");
   else if (icmp(ident, "STD.STANDARD.REAL"))
      tb_printf(buf, "R");
   else if (icmp(ident, "STD.STANDARD.BOOLEAN"))
      tb_printf(buf, "B");
   else if (icmp(ident, "STD.STANDARD.CHARACTER"))
      tb_printf(buf, "C");
   else if (icmp(ident, "STD.STANDARD.TIME"))
      tb_printf(buf, "T");
   else if (icmp(ident, "STD.STANDARD.NATURAL"))
      tb_printf(buf, "N");
   else if (icmp(ident, "STD.STANDARD.BIT"))
      tb_printf(buf, "J");
   else if (icmp(ident, "IEEE.STD_LOGIC_1164.STD_LOGIC"))
      tb_printf(buf, "L");
   else
      tb_printf(buf, "u%s;", istr(ident));
}

static ident_t lower_mangle_func(tree_t decl)
{
   ident_t prev = tree_attr_str(decl, mangled_i);
   if (prev != NULL)
      return prev;

   LOCAL_TEXT_BUF buf = tb_new();
   tb_printf(buf, "%s", istr(tree_ident(decl)));

   const int nports = tree_ports(decl);
   if (nports > 0) {
      tb_printf(buf, "$");
      for (int i = 0; i < nports; i++) {
         tree_t p = tree_port(decl, i);
         if (tree_class(p) == C_SIGNAL)
            tb_printf(buf, "s");
         lower_mangle_one_type(buf, tree_type(p));
      }
   }

   ident_t new = ident_new(tb_get(buf));
   tree_add_attr_str(decl, mangled_i, new);
   return new;
}

static vcode_reg_t lower_min_max(vcode_cmp_t cmp, tree_t fcall)
{
   vcode_reg_t result = VCODE_INVALID_REG;

   const int nparams = tree_params(fcall);
   for (int i = 0; i < nparams; i++) {
      vcode_reg_t value = lower_subprogram_arg(fcall, i);
      if (result == VCODE_INVALID_REG)
         result = value;
      else {
         vcode_reg_t test = emit_cmp(cmp, value, result);
         result = emit_select(test, value, result);
      }
   }

   return result;
}

static vcode_reg_t lower_name_attr(tree_t ref, type_t type, name_attr_t which)
{
   tree_t decl = tree_ref(ref);

   ident_t i = NULL;
   ident_t instance = tree_attr_str(decl, ident_new("INSTANCE_NAME"));
   if (instance == NULL) {
      // Assume this is a package not an elaborated design
      i = ident_new(package_signal_path_name(tree_ident(decl)));
   }
   else {
      switch (which) {
      case PATH_NAME:
         i = tree_ident(decl);
         break;
      case INSTANCE_NAME:
         i = instance;
         break;
      }
   }

   const char *str = istr(i);
   const size_t len = strlen(str);
   vcode_reg_t chars[len + 1];
   vcode_type_t ctype = vtype_int(0, 255);

   for (int j = 0; j < len; j++)
      chars[j] = emit_const(ctype, str[j]);

   vcode_reg_t data = emit_const_array(vtype_pointer(ctype), chars, len, true);

   vcode_dim_t dim0 = {
      .left  = emit_const(vtype_offset(), 1),
      .right = emit_const(vtype_offset(), len),
      .dir   = emit_const(vtype_bool(), RANGE_TO)
   };
   return emit_wrap(data, &dim0, 1);
}

static vcode_reg_t lower_narrow(type_t result, vcode_reg_t reg)
{
   // Resize arithmetic result to width of target type

   vcode_type_t vtype = lower_type(result);
   if (!vtype_eq(vtype, vcode_reg_type(reg)))
      return emit_cast(vtype, reg);
   else
      return reg;
}

static vcode_reg_t lower_arith(tree_t fcall, arith_fn_t fn, vcode_reg_t r0,
                               vcode_reg_t r1)
{
   return lower_narrow(tree_type(fcall), (*fn)(r0, r1));
}

static vcode_reg_t lower_builtin(tree_t fcall, ident_t builtin)
{
   tree_t p0 = tree_value(tree_param(fcall, 0));

   if (icmp(builtin, "event"))
      return lower_signal_flag(p0, emit_event_flag);
   else if (icmp(builtin, "active"))
      return lower_signal_flag(p0, emit_active_flag);
   else if (icmp(builtin, "left") || icmp(builtin, "right")) {
      tree_t array = tree_value(tree_param(fcall, 1));
      type_t type = tree_type(array);
      int64_t dim = assume_int(p0) - 1;
      const bool left = icmp(builtin, "left");
      vcode_reg_t reg = VCODE_INVALID_REG;
      if (lower_const_bounds(type)) {
         range_t r = type_dim(type, dim);
         reg = lower_expr(left ? r.left : r.right, EXPR_RVALUE);
      }
      else if (left)
         reg = lower_array_left(type, dim, lower_reify_expr(array));
      else
         reg = lower_array_right(type, dim, lower_reify_expr(array));

      return emit_cast(lower_type(tree_type(fcall)), reg);
   }
   else if (icmp(builtin, "low") || icmp(builtin, "high")) {
      tree_t array = tree_value(tree_param(fcall, 1));
      type_t type = tree_type(array);
      int64_t dim = assume_int(p0) - 1;
      const bool low = icmp(builtin, "low");
      vcode_reg_t reg = VCODE_INVALID_REG;
      if (lower_const_bounds(type)) {
         range_t r = type_dim(type, dim);
         if (low)
            reg = lower_reify_expr(r.kind == RANGE_TO ? r.left : r.right);
         else
            reg = lower_reify_expr(r.kind == RANGE_TO ? r.right : r.left);
      }
      else {
         vcode_reg_t array_reg  = lower_reify_expr(array);
         vcode_reg_t left_reg   = lower_array_left(type, dim, array_reg);
         vcode_reg_t right_reg  = lower_array_right(type, dim, array_reg);
         vcode_reg_t downto_reg = lower_array_dir(type, dim, array_reg);
         if (low)
            reg = emit_select(downto_reg, right_reg, left_reg);
         else
            reg = emit_select(downto_reg, left_reg, right_reg);
      }

      return emit_cast(lower_type(tree_type(fcall)), reg);
   }
   else if (icmp(builtin, "ascending")) {
      tree_t array = tree_value(tree_param(fcall, 1));
      type_t type = tree_type(array);
      int64_t dim = assume_int(p0) - 1;
      if (lower_const_bounds(type))
         return emit_const(vtype_bool(), type_dim(type, dim).kind == RANGE_TO);
      else
         return emit_not(lower_array_dir(type, dim, lower_reify_expr(array)));
   }
   else if (icmp(builtin, "length")) {
      const int dim = assume_int(p0) - 1;
      return emit_cast(lower_type(tree_type(fcall)),
                       lower_array_len(lower_arg_type(fcall, 1), dim,
                                       lower_subprogram_arg(fcall, 1)));
   }
   else if (icmp(builtin, "max"))
      return lower_min_max(VCODE_CMP_GT, fcall);
   else if (icmp(builtin, "min"))
      return lower_min_max(VCODE_CMP_LT, fcall);
   else if (icmp(builtin, "instance_name"))
      return lower_name_attr(p0, tree_type(fcall), INSTANCE_NAME);
   else if (icmp(builtin, "path_name"))
      return lower_name_attr(p0, tree_type(fcall), PATH_NAME);

   vcode_reg_t r0 = lower_subprogram_arg(fcall, 0);
   vcode_reg_t r1 = lower_subprogram_arg(fcall, 1);

   type_t r0_type = lower_arg_type(fcall, 0);
   type_t r1_type = lower_arg_type(fcall, 1);

   if (icmp(builtin, "eq"))
      return emit_cmp(VCODE_CMP_EQ, r0, r1);
   else if (icmp(builtin, "neq"))
      return emit_cmp(VCODE_CMP_NEQ, r0, r1);
   else if (icmp(builtin, "lt"))
      return emit_cmp(VCODE_CMP_LT, r0, r1);
   else if (icmp(builtin, "gt"))
      return emit_cmp(VCODE_CMP_GT, r0, r1);
   else if (icmp(builtin, "leq"))
      return emit_cmp(VCODE_CMP_LEQ, r0, r1);
   else if (icmp(builtin, "geq"))
      return emit_cmp(VCODE_CMP_GEQ, r0, r1);
   else if (icmp(builtin, "mul"))
      return lower_arith(fcall, emit_mul, r0, r1);
   else if (icmp(builtin, "add"))
      return lower_arith(fcall, emit_add, r0, r1);
   else if (icmp(builtin, "sub"))
      return lower_arith(fcall, emit_sub, r0, r1);
   else if (icmp(builtin, "div"))
      return emit_div(r0, r1, tree_index(fcall));
   else if (icmp(builtin, "exp")) {
      if (!type_eq(r0_type, r1_type))
         r1 = emit_cast(lower_type(r0_type), r1);
      return lower_arith(fcall, emit_exp, r0, r1);
   }
   else if (icmp(builtin, "mod"))
      return lower_arith(fcall, emit_mod, r0, r1);
   else if (icmp(builtin, "rem"))
      return lower_arith(fcall, emit_rem, r0, r1);
   else if (icmp(builtin, "neg"))
      return emit_neg(r0);
   else if (icmp(builtin, "abs"))
      return emit_abs(r0);
   else if (icmp(builtin, "identity"))
      return r0;
   else if (icmp(builtin, "not"))
      return emit_not(r0);
   else if (icmp(builtin, "and"))
      return emit_and(r0, r1);
   else if (icmp(builtin, "or"))
      return emit_or(r0, r1);
   else if (icmp(builtin, "xor"))
      return emit_xor(r0, r1);
   else if (icmp(builtin, "xnor"))
      return emit_xnor(r0, r1);
   else if (icmp(builtin, "nand"))
      return emit_nand(r0, r1);
   else if (icmp(builtin, "nor"))
      return emit_nor(r0, r1);
   else if (icmp(builtin, "image"))
      return emit_image(r0, tree_index(p0));
   else if (icmp(builtin, "aeq"))
      return lower_array_cmp(r0, r1, r0_type, r1_type, VCODE_CMP_EQ);
   else if (icmp(builtin, "aneq"))
      return emit_not(lower_array_cmp(r0, r1, r0_type, r1_type, VCODE_CMP_EQ));
   else if (icmp(builtin, "alt"))
      return lower_array_cmp(r0, r1, r0_type, r1_type, VCODE_CMP_LT);
   else if (icmp(builtin, "aleq"))
      return lower_array_cmp(r0, r1, r0_type, r1_type, VCODE_CMP_LEQ);
   else if (icmp(builtin, "agt"))
      return emit_not(lower_array_cmp(r0, r1, r0_type, r1_type, VCODE_CMP_LEQ));
   else if (icmp(builtin, "ageq"))
      return emit_not(lower_array_cmp(r0, r1, r0_type, r1_type, VCODE_CMP_LT));
   else if (icmp(builtin, "succ"))
      return emit_add(r0, emit_const(vcode_reg_type(r0), 1));
   else if (icmp(builtin, "pred"))
      return emit_sub(r0, emit_const(vcode_reg_type(r0), 1));
   else if (icmp(builtin, "leftof")) {
      range_t r = type_dim(tree_type(fcall), 0);
      const int dir = (r.kind == RANGE_TO ? -1 : 1);
      return emit_add(r0, emit_const(vcode_reg_type(r0), dir));
   }
   else if (icmp(builtin, "rightof")) {
      range_t r = type_dim(tree_type(fcall), 0);
      const int dir = (r.kind == RANGE_TO ? 1 : -1);;
      return emit_add(r0, emit_const(vcode_reg_type(r0), dir));
   }
   else if (icmp(builtin, "req"))
      return lower_record_eq(r0, r1, r0_type);
   else if (icmp(builtin, "rneq"))
      return emit_not(lower_record_eq(r0, r1, r0_type));
   else if (icmp(builtin, "endfile"))
      return emit_endfile(r0);
   else if (icmp(builtin, "file_open1")) {
      vcode_reg_t name   = lower_array_data(r1);
      vcode_reg_t length = lower_array_len(r1_type, 0, r1);
      emit_file_open(r0, name, length, lower_subprogram_arg(fcall, 2),
                     VCODE_INVALID_REG);
      return VCODE_INVALID_REG;
   }
   else if (icmp(builtin, "file_open2")) {
      vcode_reg_t r2     = lower_subprogram_arg(fcall, 2);
      vcode_reg_t name   = lower_array_data(r2);
      vcode_reg_t length = lower_array_len(lower_arg_type(fcall, 2), 0, r2);
      emit_file_open(r1, name, length, lower_subprogram_arg(fcall, 3), r0);
      return VCODE_INVALID_REG;
   }
   else if (icmp(builtin, "file_write")) {
      vcode_reg_t length = VCODE_INVALID_REG;
      if (type_is_array(r1_type))
         length = lower_array_len(r1_type, 0, r1);
      emit_file_write(r0, r1, length);
      return VCODE_INVALID_REG;
   }
   else if (icmp(builtin, "file_close")) {
      emit_file_close(r0);
      return VCODE_INVALID_REG;
   }
   else if (icmp(builtin, "file_read")) {
      vcode_reg_t inlen = VCODE_INVALID_REG;
      if (type_is_array(r1_type))
         inlen = lower_array_len(r1_type, 0, r1);

      vcode_reg_t outlen = VCODE_INVALID_REG;
      if (tree_params(fcall) == 3)
         outlen = lower_subprogram_arg(fcall, 2);

      emit_file_read(r0, r1, inlen, outlen);
      return VCODE_INVALID_REG;
   }
   else if (icmp(builtin, "deallocate")) {
      emit_deallocate(r0);
      return VCODE_INVALID_REG;
   }
   else if (icmp(builtin, "v_not")) {
      vcode_reg_t r0_len = lower_array_len(r0_type, 0, r0);
      return emit_bit_vec_op(BIT_VEC_NOT, r0, r0_len, VCODE_INVALID_REG,
                             VCODE_INVALID_REG, lower_type(tree_type(fcall)));
   }
   else if (icmp(builtin, "v_and")) {
      vcode_reg_t r0_len = lower_array_len(r0_type, 0, r0);
      vcode_reg_t r1_len = lower_array_len(r1_type, 0, r1);
      return emit_bit_vec_op(BIT_VEC_AND, r0, r0_len, r1, r1_len,
                             lower_type(tree_type(fcall)));
   }
   else if (icmp(builtin, "v_or")) {
      vcode_reg_t r0_len = lower_array_len(r0_type, 0, r0);
      vcode_reg_t r1_len = lower_array_len(r1_type, 0, r1);
      return emit_bit_vec_op(BIT_VEC_OR, r0, r0_len, r1, r1_len,
                             lower_type(tree_type(fcall)));
   }
   else if (icmp(builtin, "v_xor")) {
      vcode_reg_t r0_len = lower_array_len(r0_type, 0, r0);
      vcode_reg_t r1_len = lower_array_len(r1_type, 0, r1);
      return emit_bit_vec_op(BIT_VEC_XOR, r0, r0_len, r1, r1_len,
                             lower_type(tree_type(fcall)));
   }
   else if (icmp(builtin, "v_xnor")) {
      vcode_reg_t r0_len = lower_array_len(r0_type, 0, r0);
      vcode_reg_t r1_len = lower_array_len(r1_type, 0, r1);
      return emit_bit_vec_op(BIT_VEC_XOR, r0, r0_len, r1, r1_len,
                             lower_type(tree_type(fcall)));
   }
   else if (icmp(builtin, "v_nand")) {
      vcode_reg_t r0_len = lower_array_len(r0_type, 0, r0);
      vcode_reg_t r1_len = lower_array_len(r1_type, 0, r1);
      return emit_bit_vec_op(BIT_VEC_XOR, r0, r0_len, r1, r1_len,
                             lower_type(tree_type(fcall)));
   }
   else if (icmp(builtin, "v_nor")) {
      vcode_reg_t r0_len = lower_array_len(r0_type, 0, r0);
      vcode_reg_t r1_len = lower_array_len(r1_type, 0, r1);
      return emit_bit_vec_op(BIT_VEC_XOR, r0, r0_len, r1, r1_len,
                             lower_type(tree_type(fcall)));
   }
   else
      fatal_at(tree_loc(fcall), "cannot lower builtin %s", istr(builtin));
}

static vcode_reg_t lower_fcall(tree_t fcall, expr_ctx_t ctx)
{
   tree_t decl = tree_ref(fcall);

   ident_t builtin = tree_attr_str(decl, builtin_i);
   if (builtin != NULL)
      return lower_builtin(fcall, builtin);

   tree_t foreign = tree_attr_tree(decl, foreign_i);
   ident_t name = NULL;
   if (foreign != NULL) {
      if (tree_kind(foreign) != T_LITERAL)
         fatal_at(tree_loc(decl), "foreign attribute must have string "
                  "literal value");

      const int nchars = tree_chars(foreign);
      char buf[nchars + 1];
      for (int i = 0; i < nchars; i++)
         buf[i] = tree_pos(tree_ref(tree_char(foreign, i)));
      buf[nchars] = '\0';

      name = ident_new(buf);
   }
   else
      name = lower_mangle_func(decl);

   const int nargs = tree_params(fcall);
   vcode_reg_t args[nargs];
   for (int i = 0; i < nargs; i++)
      args[i] = lower_subprogram_arg(fcall, i);

   vcode_type_t rtype = lower_type(tree_type(fcall));
   if (tree_attr_int(decl, nested_i, 0))
      return emit_nested_fcall(name, rtype, args, nargs);
   else
      return emit_fcall(name, rtype, args, nargs);
}

static vcode_reg_t lower_string_literal(tree_t lit, bool allocate)
{
   type_t ltype = tree_type(lit);
   vcode_type_t vtype = lower_type(type_elem(ltype));

   const int nchars = tree_chars(lit);
   vcode_reg_t *tmp LOCAL = xmalloc(nchars * sizeof(vcode_reg_t));

   for (int i = 0; i < nchars; i++)
      tmp[i] = emit_const(vtype, tree_pos(tree_ref(tree_char(lit, i))));

   return emit_const_array(lower_type(ltype), tmp, nchars, allocate);
}

static vcode_reg_t lower_literal(tree_t lit)
{
   switch (tree_subkind(lit)) {
   case L_INT:
      return emit_const(lower_type(tree_type(lit)), tree_ival(lit));

   case L_STRING:
      return lower_string_literal(lit, true);

   case L_NULL:
      return emit_null(lower_type(tree_type(lit)));

   case L_REAL:
      return emit_const_real(tree_dval(lit));

   default:
      fatal_at(tree_loc(lit), "cannot lower literal kind %d",
               tree_subkind(lit));
   }
}

static vcode_var_t lower_get_var(tree_t decl)
{
   vcode_var_t var = tree_attr_int(decl, vcode_obj_i, VCODE_INVALID_VAR);
   if (var == VCODE_INVALID_VAR) {
      vcode_dump();
      fatal_trace("missing vcode var for %s", istr(tree_ident(decl)));
   }

   return var;
}

static vcode_signal_t lower_get_signal(tree_t decl)
{
   vcode_signal_t sig = tree_attr_int(decl, vcode_obj_i, VCODE_INVALID_SIGNAL);
   if (sig == VCODE_INVALID_SIGNAL) {
      vcode_dump();
      fatal_trace("missing vcode signal for %s", istr(tree_ident(decl)));
   }

   return sig;
}

static vcode_reg_t lower_var_ref(tree_t decl, expr_ctx_t ctx)
{
   type_t type = tree_type(decl);

   vcode_var_t var = lower_get_var(decl);
   if (type_is_array(type) && lower_const_bounds(type))
      return emit_index(var, VCODE_INVALID_REG);
   else if (type_is_record(type))
      return emit_index(var, VCODE_INVALID_REG);
   else if ((type_is_scalar(type) || type_is_file(type) || type_is_access(type))
            && ctx == EXPR_LVALUE)
      return emit_index(var, VCODE_INVALID_REG);
   else
      return emit_load(var);
}

static vcode_reg_t lower_signal_ref(tree_t decl, expr_ctx_t ctx)
{
   type_t type = tree_type(decl);

   vcode_signal_t sig = lower_get_signal(decl);
   if (ctx == EXPR_LVALUE)
      return emit_nets(sig);
   else {
      vcode_var_t shadow = vcode_signal_shadow(sig);
      if (shadow != VCODE_INVALID_VAR) {
         vcode_reg_t r = emit_load(shadow);
         return type_is_array(type) ? r : emit_load_indirect(r);
      }
      else
         assert(false);
   }
}

static vcode_reg_t lower_param_ref(tree_t decl, expr_ctx_t ctx)
{
   vcode_reg_t reg = tree_attr_int(decl, vcode_obj_i, VCODE_INVALID_REG);
   if (reg == VCODE_INVALID_REG) {
      vcode_dump();
      fatal_trace("missing register for parameter %s", istr(tree_ident(decl)));
   }

   const int depth = tree_attr_int(decl, nested_i, 0);
   if (depth > 0 && vcode_unit_depth() != depth)
      return emit_param_upref(vcode_unit_depth() - depth, reg);
   else
      return reg;
}

static vcode_reg_t lower_alias_ref(tree_t alias, expr_ctx_t ctx)
{
   assert(tree_kind(alias) == T_ALIAS);

   tree_t value = tree_value(alias);
   vcode_reg_t aliased = lower_expr(value, ctx);

   // The aliased object may have non-constant bounds whereas the
   // alias itself has known bounds

   type_t alias_type = tree_type(alias);
   type_t value_type = tree_type(value);

   if (!type_is_array(alias_type))
      return aliased;

   const bool alias_const = lower_const_bounds(alias_type);
   const bool value_const = lower_const_bounds(value_type);

   if (alias_const && !value_const)
      return lower_array_data(aliased);
   else if (!alias_const)
      return lower_wrap(alias_type, aliased);
   else
      return aliased;
}

static vcode_reg_t lower_ref(tree_t ref, expr_ctx_t ctx)
{
   tree_t decl = tree_ref(ref);

   tree_kind_t kind = tree_kind(decl);
   switch (kind) {
   case T_ENUM_LIT:
      return emit_const(lower_type(tree_type(decl)), tree_pos(decl));

   case T_VAR_DECL:
   case T_FILE_DECL:
      return lower_var_ref(decl, ctx);

   case T_PORT_DECL:
      return lower_param_ref(decl, ctx);

   case T_SIGNAL_DECL:
      return lower_signal_ref(decl, ctx);

   case T_TYPE_DECL:
      return VCODE_INVALID_REG;

   case T_CONST_DECL:
      if (type_is_scalar(tree_type(decl)))
         return lower_expr(tree_value(decl), ctx);
      else
         return lower_var_ref(decl, ctx);

   case T_ALIAS:
      return lower_alias_ref(decl, ctx);

   default:
      vcode_dump();
      fatal_trace("cannot lower reference to %s", tree_kind_str(kind));
   }
}

static vcode_reg_t lower_array_off(vcode_reg_t off, vcode_reg_t array,
                                   type_t type, unsigned dim)
{
   // Convert VHDL offset 'off' to a zero-based array offset

   const bool wrapped = vtype_kind(vcode_reg_type(array)) == VCODE_TYPE_UARRAY
      || type_is_unconstrained(type);

   vcode_reg_t zeroed = VCODE_INVALID_REG;
   if (wrapped) {
      vcode_reg_t meta_reg = lower_reify(array);
      vcode_reg_t left_reg = lower_array_left(type, dim, meta_reg);

      vcode_reg_t downto = emit_sub(left_reg, off);
      vcode_reg_t upto   = emit_sub(off, left_reg);
      zeroed = emit_select(emit_uarray_dir(meta_reg, dim), downto, upto);
   }
   else {
      range_t r = type_dim(type, dim);
      vcode_reg_t left = lower_expr(r.left, EXPR_RVALUE);
      if (r.kind == RANGE_TO)
         zeroed = emit_sub(off, left);
      else
         zeroed = emit_sub(left, off);
   }

   return emit_cast(vtype_offset(), zeroed);
}

static vcode_reg_t lower_unalias_index(tree_t alias, vcode_reg_t index,
                                       vcode_reg_t meta)
{
   type_t alias_type = tree_type(alias);
   type_t base_type  = tree_type(tree_value(alias));

   assert(type_dims(alias_type) == 1);  // TODO: multi-dimensional arrays

   range_t alias_r = type_dim(alias_type, 0);
   vcode_reg_t off = emit_sub(index, lower_reify_expr(alias_r.left));

   vcode_reg_t bleft = VCODE_INVALID_REG, bdir = VCODE_INVALID_REG;
   switch (type_kind(base_type)) {
   case T_CARRAY:
   case T_SUBTYPE:
      // The transformation is a constant offset of indices
      {
         range_t base_r = type_dim(base_type, 0);
         bleft = lower_reify_expr(base_r.left);
         bdir  = lower_array_dir(base_type, 0, VCODE_INVALID_REG);
      }
      break;

   case T_UARRAY:
      // The transformation must be computed at runtime
      {
         bleft = lower_array_left(base_type, 0, meta);
         bdir  = lower_array_dir(base_type, 0, meta);
      }
      break;

   default:
      assert(false);
   }

   vcode_reg_t adir = lower_array_dir(alias_type, 0, VCODE_INVALID_REG);
   vcode_reg_t same_dir = emit_cmp(VCODE_CMP_EQ, bdir, adir);

   return emit_select(same_dir, emit_add(bleft, off), emit_sub(bleft, off));
}

static vcode_reg_t lower_array_ref_offset(tree_t ref, vcode_reg_t array)
{
   tree_t value = tree_value(ref);
   type_t value_type = tree_type(value);

   tree_t alias = NULL;
   if (tree_kind(value) == T_REF) {
      tree_t decl = tree_ref(value);
      if (tree_kind(decl) == T_ALIAS)
         alias = decl;
   }

   vcode_reg_t idx = emit_const(vtype_offset(), 0);
   const int nparams = tree_params(ref);
   for (int i = 0; i < nparams; i++) {
      tree_t p = tree_param(ref, i);
      assert(tree_subkind(p) == P_POS);

      vcode_reg_t offset = lower_expr(tree_value(p), EXPR_RVALUE);

      //if (!elide_bounds)
      //   cgen_check_array_bounds(tree_value(p), type, i, (alias ? NULL : meta),
      //                           offset, ctx);

      if (alias != NULL) {
         offset = lower_unalias_index(alias, offset, array);
         value_type = tree_type(tree_value(alias));
      }

      if (i > 0) {
         vcode_reg_t stride = lower_array_len(value_type, i, array);
         idx = emit_mul(idx, stride);
      }

      idx = emit_add(idx, lower_array_off(offset, array, value_type, i));
   }

   type_t elem = type_elem(value_type);
   if (type_is_array(elem)) {
      emit_comment("array of array stride calculation");
      vcode_reg_t stride = lower_array_total_len(elem, VCODE_INVALID_REG);
      idx = emit_mul(idx, stride);
   }

   return idx;
}

static vcode_reg_t lower_array_ref(tree_t ref, expr_ctx_t ctx)
{
   tree_t value = tree_value(ref);

   vcode_reg_t array = lower_expr(value, ctx);

   const vtype_kind_t vtkind = vtype_kind(vcode_reg_type(array));
   assert(vtkind == VCODE_TYPE_POINTER || vtkind == VCODE_TYPE_UARRAY
          || vtkind == VCODE_TYPE_SIGNAL);

   return emit_add(lower_array_data(array), lower_array_ref_offset(ref, array));
}

static vcode_reg_t lower_array_slice(tree_t slice, expr_ctx_t ctx)
{
   tree_t value = tree_value(slice);
   range_t r = tree_range(slice);

   vcode_reg_t left_reg  = lower_reify_expr(r.left);
   vcode_reg_t right_reg = lower_reify_expr(r.right);

   //vcode_reg_t low  = (r.kind == RANGE_TO) ? left : right;
   //vcode_reg_t high = (r.kind == RANGE_TO) ? right : left;

   //vcode_reg_t null = emit_cmp(VCODE_CMP_LT, high, low);

   /*
   if (!elide_bounds) {
      LLVMBasicBlockRef check_bb = LLVMAppendBasicBlock(ctx->fn, "check");
      LLVMBasicBlockRef merge_bb = LLVMAppendBasicBlock(ctx->fn, "merge");

      LLVMBuildCondBr(builder, null, merge_bb, check_bb);

      LLVMPositionBuilderAtEnd(builder, check_bb);

      cgen_check_array_bounds(r.left, type, 0, array, left, ctx);
      cgen_check_array_bounds(r.right, type, 0, array, right, ctx);

      LLVMBuildBr(builder, merge_bb);

      LLVMPositionBuilderAtEnd(builder, merge_bb);
   }
   */

   tree_t alias = NULL;
   if (tree_kind(value) == T_REF) {
      tree_t decl = tree_ref(value);
      if (tree_kind(decl) == T_ALIAS)
         alias = decl;
   }

   type_t type = tree_type(value);

   vcode_reg_t kind_reg = VCODE_INVALID_REG, array_reg = VCODE_INVALID_REG;
   if (alias != NULL) {
      tree_t aliased = tree_value(alias);
      type = tree_type(aliased);
      array_reg = lower_expr(aliased, ctx);
      left_reg  = lower_unalias_index(alias, left_reg, array_reg);
      right_reg = lower_unalias_index(alias, right_reg, array_reg);
      kind_reg  = lower_array_dir(type, 0, array_reg);
   }
   else {
      kind_reg = emit_const(vtype_bool(), r.kind);
      array_reg = lower_expr(value, ctx);
   }

   vcode_reg_t data_reg = lower_array_data(array_reg);
   vcode_reg_t off_reg  = lower_array_off(left_reg, array_reg, type, 0);
   vcode_reg_t ptr_reg  = emit_add(data_reg, off_reg);

   const bool unwrap = lower_is_const(r.left) && lower_is_const(r.right);

   if (unwrap)
      return ptr_reg;
   else {
      vcode_dim_t dim0 = {
         .left  = left_reg,
         .right = right_reg,
         .dir   = kind_reg
      };
      return emit_wrap(ptr_reg, &dim0, 1);
   }
}

static void lower_copy_vals(vcode_reg_t *dst, const vcode_reg_t *src,
                            unsigned n, bool backwards)
{
   while (n--) {
      *dst = *src;
      ++src;
      dst += (backwards ? -1 : 1);
   }
}

static vcode_reg_t *lower_const_array_aggregate(tree_t t, type_t type,
                                                int dim, int *n_elems)
{
   if ((*n_elems = lower_array_const_size(type)) == 0)
      return NULL;

   vcode_reg_t *vals = xmalloc(*n_elems * sizeof(vcode_reg_t));

   for (int i = 0; i < *n_elems; i++)
      vals[i] = VCODE_INVALID_VAR;

   range_t r = type_dim(type, dim);
   const int64_t left = assume_int(r.left);
   const bool is_downto = (r.kind == RANGE_DOWNTO);

   const int nassocs = tree_assocs(t);
   for (int i = 0; i < nassocs; i++) {
      tree_t a = tree_assoc(t, i);
      tree_t value = tree_value(a);

      const tree_kind_t value_kind = tree_kind(value);

      vcode_reg_t tmp = VCODE_INVALID_REG;
      vcode_reg_t *sub = &tmp;
      int nsub = 1;
      if (value_kind == T_AGGREGATE) {
         type_t sub_type = tree_type(value);
         if (type_is_array(sub_type))
            sub = lower_const_array_aggregate(value, sub_type, 0, &nsub);
         else if (type_is_record(sub_type))
            *sub = lower_record_aggregate(value, true,
                                          lower_is_const(value), EXPR_RVALUE);
         else
            assert(false);
      }
      else if (value_kind == T_LITERAL && tree_subkind(value) == L_STRING)
         *sub = lower_string_literal(value, false);
      else
         *sub = lower_expr(value, EXPR_RVALUE);

      switch (tree_subkind(a)) {
      case A_POS:
         lower_copy_vals(vals + (i * nsub), sub, nsub, false);
         break;

      case A_NAMED:
         {
            const int64_t name = assume_int(tree_name(a));
            const int64_t off  = is_downto ? left - name : name - left;
            lower_copy_vals(vals + (off * nsub), sub, nsub, false);
         }
         break;

      case A_OTHERS:
         assert((*n_elems % nsub) == 0);
         for (int j = 0; j < (*n_elems / nsub); j++) {
            if (vals[j * nsub] == VCODE_INVALID_REG)
               lower_copy_vals(vals + (j * nsub), sub, nsub, false);
         }
         break;

      case A_RANGE:
         {
            int64_t r_low, r_high;
            range_bounds(tree_range(a), &r_low, &r_high);

            for (int j = r_low; j <= r_high; j++) {
               const int64_t off = is_downto ? left - j : j - left;
               lower_copy_vals(vals + (off * nsub), sub, nsub, false);
            }
         }
         break;
      }

      if (sub != &tmp)
         free(sub);
   }

   for (int i = 0; i < *n_elems; i++)
      assert(vals[i] != VCODE_INVALID_VAR);

   return vals;
}

static int lower_bit_width(type_t type)
{
   switch (type_kind(type)) {
   case T_INTEGER:
   case T_PHYSICAL:
      {
         range_t r = type_dim(type, 0);
         return bits_for_range(assume_int(r.left), assume_int(r.right));
      }

   case T_REAL:
       // All real types are doubles at the moment
       return 64;

   case T_SUBTYPE:
      return lower_bit_width(type_base(type));

   case T_ENUM:
      return bits_for_range(0, type_enum_literals(type) - 1);

   default:
      assert(false);
   }
}

static bool lower_memset_bit_pattern(tree_t value, unsigned bits, uint8_t *byte)
{
   // If a tree has a constant value and that value's bit pattern consists
   // of the same repeated byte then we can use memset to initialise an
   // array with this

   int64_t ival;
   if (!folded_int(value, &ival))
      return false;

   const unsigned bytes = (bits + 7) / 8;

   *byte = ival & 0xff;
   for (int i = 0; i < bytes; i++) {
      const uint8_t next = ival & 0xff;
      if (next != *byte)
         return false;

      ival >>= 8;
   }

   return true;
}

static vcode_reg_t lower_dyn_aggregate(tree_t agg, type_t type)
{
   type_t agg_type = tree_type(agg);
   type_t elem_type = type_elem(type);

   emit_comment("Begin dynamic aggregrate line %d", tree_loc(agg)->first_line);

   vcode_reg_t def_reg = VCODE_INVALID_REG;
   tree_t def_value = NULL;
   const int nassocs = tree_assocs(agg);
   for (int i = 0; def_value == NULL && i < nassocs; i++) {
      tree_t a = tree_assoc(agg, i);
      if (tree_subkind(a) == A_OTHERS) {
         def_value = tree_value(a);
         if (type_is_scalar(tree_type(def_value)))
            def_reg = lower_reify_expr(def_value);
      }
   }

   assert(!type_is_unconstrained(agg_type));

   vcode_reg_t dir_reg   = lower_array_dir(agg_type, 0, VCODE_INVALID_REG);
   vcode_reg_t left_reg  = lower_array_left(agg_type, 0, VCODE_INVALID_REG);
   vcode_reg_t right_reg = lower_array_right(agg_type, 0, VCODE_INVALID_REG);
   vcode_reg_t len_reg   = lower_array_total_len(agg_type, VCODE_INVALID_REG);

   type_t scalar_elem_type = elem_type;
   while (type_is_array(scalar_elem_type))
      scalar_elem_type = type_elem(scalar_elem_type);

   vcode_reg_t mem_reg = emit_alloca(lower_type(scalar_elem_type),
                                     lower_bounds(scalar_elem_type), len_reg);

   vcode_type_t offset_type = vtype_offset();

   vcode_dim_t dim0 = {
      .left  = left_reg,
      .right = right_reg,
      .dir   = dir_reg
   };

   tree_t agg0 = tree_assoc(agg, 0);

   uint8_t byte = 0;
   unsigned bits = 0;
   const bool can_use_memset =
      (type_is_integer(elem_type) || type_is_enum(elem_type))
      && tree_assocs(agg) == 1
      && tree_subkind(agg0) == A_OTHERS
      && ((bits = lower_bit_width(elem_type)) <= 8
          || lower_memset_bit_pattern(tree_value(agg0), bits, &byte));

   if (can_use_memset) {
      if (bits <= 8)
         emit_memset(mem_reg, lower_reify_expr(tree_value(agg0)), len_reg);
      else
         emit_memset(mem_reg,
                     emit_const(vtype_int(0, 255), byte),
                     emit_mul(len_reg,
                              emit_const(offset_type, (bits + 7) / 8)));

      return emit_wrap(mem_reg, &dim0, 1);
   }

   vcode_reg_t ivar = emit_alloca(offset_type, offset_type, VCODE_INVALID_REG);
   emit_store_indirect(emit_const(offset_type, 0), ivar);

   vcode_block_t test_bb = emit_block();
   vcode_block_t body_bb = emit_block();
   vcode_block_t exit_bb = emit_block();

   emit_jump(test_bb);

   // Loop test
   vcode_select_block(test_bb);
   vcode_reg_t i_loaded = emit_load_indirect(ivar);
   vcode_reg_t ge = emit_cmp(VCODE_CMP_GEQ, i_loaded, len_reg);
   emit_cond(ge, exit_bb, body_bb);

   // Loop body
   vcode_select_block(body_bb);

   // Regenerate non-scalar default values on each iteration
   if (def_reg == VCODE_INVALID_REG && def_value != NULL)
      def_reg = lower_expr(def_value, EXPR_RVALUE);

   vcode_reg_t what = def_reg;
   for (int i = 0; i < nassocs; i++) {
      tree_t a = tree_assoc(agg, i);

      assoc_kind_t kind = tree_subkind(a);
      vcode_reg_t value_reg = VCODE_INVALID_REG;
      if (kind != A_OTHERS)
         value_reg = lower_expr(tree_value(a), EXPR_RVALUE);

      if (what == VCODE_INVALID_REG) {
         what = value_reg;
         continue;
      }

      switch (kind) {
      case A_POS:
         {
            vcode_reg_t eq = emit_cmp(VCODE_CMP_EQ, i_loaded,
                                      emit_const(offset_type, tree_pos(a)));
            what = emit_select(eq, value_reg, what);
         }
         break;

      case A_NAMED:
         {
            vcode_reg_t name_reg   = lower_reify_expr(tree_name(a));
            vcode_reg_t downto_reg = emit_sub(left_reg, name_reg);
            vcode_reg_t upto_reg   = emit_sub(name_reg, left_reg);

            vcode_reg_t off_reg  = emit_select(dir_reg, downto_reg, upto_reg);
            vcode_reg_t cast_reg = emit_cast(vtype_offset(), off_reg);

            vcode_reg_t eq = emit_cmp(VCODE_CMP_EQ, i_loaded, cast_reg);
            what = emit_select(eq, value_reg, what);
         }
         break;

      case A_RANGE:
         {
            range_t r = tree_range(a);

            vcode_cmp_t lpred =
               (r.kind == RANGE_TO ? VCODE_CMP_GEQ : VCODE_CMP_LEQ);
            vcode_cmp_t rpred =
               (r.kind == RANGE_TO ? VCODE_CMP_LEQ : VCODE_CMP_GEQ);

            vcode_reg_t lcmp_reg =
               emit_cmp(lpred, i_loaded,
                        emit_cast(offset_type, lower_reify_expr(r.left)));
            vcode_reg_t rcmp_reg =
               emit_cmp(rpred, i_loaded,
                        emit_cast(offset_type, lower_reify_expr(r.right)));

            vcode_reg_t in_reg = emit_or(lcmp_reg, rcmp_reg);
            what = emit_select(in_reg, value_reg, what);
         }
         break;

      case A_OTHERS:
         break;
      }
   }

   vcode_reg_t ptr_reg = emit_add(mem_reg, i_loaded);
   if (type_is_array(elem_type)) {
      vcode_reg_t src_reg = lower_array_data(what);
      vcode_reg_t length_reg = lower_array_total_len(elem_type, what);
      emit_copy(ptr_reg, src_reg, length_reg);
   }
   else if (type_is_record(elem_type))
      assert(false);  // TODO
   else
      emit_store_indirect(lower_reify(what), ptr_reg);

   emit_store_indirect(emit_add(i_loaded, emit_const(offset_type, 1)), ivar);
   emit_jump(test_bb);

   // Epilogue
   vcode_select_block(exit_bb);
   emit_comment("End dynamic aggregrate line %d", tree_loc(agg)->first_line);

   return emit_wrap(mem_reg, &dim0, 1);
}

static int lower_field_index(type_t type, ident_t field)
{
   // Lookup the position of this field in the record type

   const int nfields = type_fields(type);
   for (int i = 0; i < nfields; i++) {
      if (tree_ident(type_field(type, i)) == field)
         return i;
   }
   assert(false);
}

static vcode_reg_t lower_record_aggregate(tree_t expr, bool nest,
                                          bool is_const, expr_ctx_t ctx)
{
   type_t type = tree_type(expr);
   const int nfields = type_fields(type);
   const int nassocs = tree_assocs(expr);

   vcode_reg_t vals[nfields];
   for (int i = 0; i < nfields; i++)
      vals[i] = VCODE_INVALID_REG;

   for (int i = 0; i < nassocs; i++) {
      tree_t a = tree_assoc(expr, i);

      tree_t value = tree_value(a);
      type_t value_type = tree_type(value);
      vcode_reg_t v = VCODE_INVALID_REG;
      if (type_is_array(value_type) && is_const) {
         if (tree_kind(value) == T_LITERAL)
            v = lower_string_literal(value, true);
         else {
            assert(false);
#if 0
            LLVMValueRef *vals =
               ? cgen_string_literal(value, &nvals, NULL)
               : cgen_const_aggregate(value, value_type, 0, &nvals, ctx);
            LLVMTypeRef ltype = llvm_type(type_elem(value_type));
            v = LLVMConstArray(ltype, vals, nvals);
#endif
         }
      }
      else if (type_is_record(value_type) && is_const)
         v = lower_record_aggregate(value, true, true, ctx);
      else
         v = lower_expr(value, ctx);

      switch (tree_subkind(a)) {
      case A_POS:
         vals[tree_pos(a)] = v;
         break;

      case A_NAMED:
         {
            int index = lower_field_index(type, tree_ident(tree_name(a)));
            vals[index] = v;
         }
         break;

      case A_OTHERS:
         for (int j = 0; j < nfields; j++) {
            if (vals[j] == VCODE_INVALID_REG)
               vals[j] = v;
         }
         break;

      case A_RANGE:
         assert(false);
      }
   }

   for (int i = 0; i < nfields; i++)
      assert(vals[i] != VCODE_INVALID_REG);

   if (is_const)
      return emit_const_record(lower_type(type), vals, nfields, !nest);
   else {
      assert(false);
#if 0
      for (int i = 0; i < nfields; i++) {
         type_t ftype = tree_type(type_field(type, i));
         LLVMValueRef ptr = LLVMBuildStructGEP(builder, mem, i, "");
         if (type_is_array(ftype))
            cgen_array_copy(ftype, ftype, vals[i], ptr, NULL, ctx);
         else if (type_is_record(ftype))
            cgen_record_copy(ftype, vals[i], ptr);
         else
            LLVMBuildStore(builder, vals[i], ptr);
      }

      return mem;
#endif
   }
}

static vcode_reg_t lower_aggregate(tree_t expr, expr_ctx_t ctx)
{
   type_t type = tree_type(expr);

   if (type_is_record(type))
      return lower_record_aggregate(expr, false, true, ctx);

   assert(type_is_array(type));

   if (lower_const_bounds(type) && lower_is_const(expr)) {
      int nvals;
      vcode_reg_t *values LOCAL =
         lower_const_array_aggregate(expr, type, 0, &nvals);

      return emit_const_array(lower_type(type), values, nvals, true);
   }
   else
      return lower_dyn_aggregate(expr, type);
}

static vcode_reg_t lower_record_ref(tree_t expr, expr_ctx_t ctx)
{
   tree_t value = tree_value(expr);
   vcode_reg_t record = lower_expr(value, ctx);

   const int index = lower_field_index(tree_type(value), tree_ident(expr));

   return emit_record_ref(record, index);
}

static vcode_reg_t lower_concat(tree_t expr, expr_ctx_t ctx)
{
   assert(tree_params(expr) == 2);
   const tree_t args[] = {
      tree_value(tree_param(expr, 0)),
      tree_value(tree_param(expr, 1))
   };

   const type_t arg_types[] = {
      tree_type(args[0]),
      tree_type(args[1])
   };

   const vcode_reg_t arg_regs[] = {
      lower_expr(args[0], ctx),
      lower_expr(args[1], ctx)
   };

   vcode_reg_t var_reg = VCODE_INVALID_REG;
   type_t type = tree_type(expr);
   type_t elem = type_elem(type);
   if (type_is_unconstrained(type)) {
      vcode_reg_t args_len[] = {
         lower_array_len(arg_types[0], 0, arg_regs[0]),
         lower_array_len(arg_types[1], 0, arg_regs[1])
      };
      vcode_reg_t len  = emit_add(args_len[0], args_len[1]);
      vcode_reg_t data = emit_alloca(lower_type(elem), lower_bounds(elem), len);

      vcode_dim_t dims[1] = {
         {
            .left  = emit_const(vtype_offset(), 1),
            .right = len,
            .dir   = emit_const(vtype_bool(), RANGE_TO)
         }
      };
      var_reg = emit_wrap(data, dims, 1);
   }
   else
      var_reg = emit_alloca(lower_type(elem), lower_bounds(elem),
                            lower_array_total_len(type, VCODE_INVALID_REG));

   vcode_reg_t ptr = lower_array_data(var_reg);

   if (type_is_array(arg_types[0])) {
      vcode_reg_t src_len = lower_array_total_len(arg_types[0], arg_regs[0]);
      emit_copy(ptr, lower_array_data(arg_regs[0]), src_len);
      ptr = emit_add(ptr, src_len);
   }
   else {
      emit_store_indirect(arg_regs[0], ptr);
      ptr = emit_add(ptr, emit_const(vtype_offset(), 1));
   }

   if (type_is_array(arg_types[1])) {
      vcode_reg_t src_len = lower_array_total_len(arg_types[1], arg_regs[1]);
      emit_copy(ptr, lower_array_data(arg_regs[1]), src_len);
   }
   else
      emit_store_indirect(arg_regs[1], ptr);

   return var_reg;
}

static vcode_reg_t lower_new(tree_t expr, expr_ctx_t ctx)
{
   type_t type = type_access(tree_type(expr));

   tree_t value = tree_value(expr);
   type_t value_type = tree_type(value);

   vcode_reg_t init_reg = lower_expr(value, EXPR_RVALUE);

   if (type_is_array(type)) {
      vcode_reg_t length_reg = lower_array_total_len(value_type, init_reg);

      vcode_reg_t mem_reg = emit_new(lower_type(type_elem(type)), length_reg);
      vcode_reg_t raw_reg = emit_all(mem_reg);

      emit_copy(raw_reg, lower_array_data(init_reg), length_reg);

      if (!lower_const_bounds(type)) {
          // Need to allocate memory for both the array and its metadata

         vcode_reg_t meta_reg = lower_wrap(value_type, init_reg);
         vcode_reg_t result_reg = emit_new(lower_type(type), VCODE_INVALID_REG);
         emit_store_indirect(meta_reg, emit_all(result_reg));
         return result_reg;
      }
      else
         return mem_reg;
   }
   else {
      vcode_reg_t result_reg = emit_new(lower_type(type), VCODE_INVALID_REG);
      vcode_reg_t all_reg = emit_all(result_reg);

      if (type_is_record(type))
         emit_copy(all_reg, init_reg, emit_const(vtype_offset(), 1));
      else
         emit_store_indirect(lower_reify(init_reg), all_reg);

      return result_reg;
   }
}

static vcode_reg_t lower_all(tree_t all, expr_ctx_t ctx)
{
   vcode_reg_t access_reg = lower_reify_expr(tree_value(all));
   emit_null_check(access_reg, tree_index(all));
   vcode_reg_t all_reg = emit_all(access_reg);

   type_t type = tree_type(all);
   if (type_is_array(type) && !lower_const_bounds(type))
      return lower_reify(all_reg);
   else
      return all_reg;
}

static vcode_reg_t lower_type_conv(tree_t expr, expr_ctx_t ctx)
{
   tree_t value = tree_value(tree_param(expr, 0));

   type_t from = tree_type(value);
   type_t to   = tree_type(expr);

   type_kind_t from_k = type_kind(type_base_recur(from));
   type_kind_t to_k   = type_kind(type_base_recur(to));

   vcode_reg_t value_reg = lower_expr(value, ctx);

   if (from_k == T_REAL && to_k == T_INTEGER)
      return emit_cast(lower_type(to), value_reg);
   else if (from_k == T_INTEGER && to_k == T_REAL)
      return emit_cast(lower_type(to), value_reg);
   else if (type_is_array(to) && !lower_const_bounds(to)) {
      // Need to wrap in metadata
      return lower_wrap(from, value_reg);
   }
   else if (from_k == T_INTEGER && to_k == T_INTEGER)
      // Possibly change width
      return emit_cast(lower_type(to), value_reg);
   else {
      // No conversion to perform
      return value_reg;
   }
}

static vcode_reg_t lower_expr(tree_t expr, expr_ctx_t ctx)
{
   switch (tree_kind(expr)) {
   case T_FCALL:
      return lower_fcall(expr, ctx);
   case T_LITERAL:
      return lower_literal(expr);
   case T_REF:
      return lower_ref(expr, ctx);
   case T_AGGREGATE:
      return lower_aggregate(expr, ctx);
   case T_ARRAY_REF:
      return lower_array_ref(expr, ctx);
   case T_ARRAY_SLICE:
      return lower_array_slice(expr, ctx);
   case T_RECORD_REF:
      return lower_record_ref(expr, ctx);
   case T_CONCAT:
      return lower_concat(expr, ctx);
   case T_NEW:
      return lower_new(expr, ctx);
   case T_ALL:
      return lower_all(expr, ctx);
   case T_TYPE_CONV:
      return lower_type_conv(expr, ctx);
   default:
      fatal_at(tree_loc(expr), "cannot lower expression kind %s",
               tree_kind_str(tree_kind(expr)));
   }
}

static void lower_assert(tree_t stmt)
{
   const int is_report = tree_attr_int(stmt, ident_new("is_report"), 0);

   vcode_reg_t severity = lower_reify_expr(tree_severity(stmt));

   vcode_reg_t value = VCODE_INVALID_REG;
   if (!is_report)
      value = lower_reify_expr(tree_value(stmt));

   vcode_block_t message_bb = VCODE_INVALID_BLOCK;
   vcode_block_t exit_bb = VCODE_INVALID_BLOCK;

   vcode_reg_t message = VCODE_INVALID_REG, length = VCODE_INVALID_REG;
   if (tree_has_message(stmt)) {
      tree_t m = tree_message(stmt);

      // If the message can have side effects then branch to a new block
      const tree_kind_t kind = tree_kind(m);
      const bool side_effects = kind != T_LITERAL;
      if (side_effects && !is_report) {
         message_bb = emit_block();
         exit_bb    = emit_block();
         emit_cond(value, exit_bb, message_bb);
         vcode_select_block(message_bb);
      }

      vcode_reg_t message_wrapped = lower_expr(m, EXPR_RVALUE);
      message = lower_array_data(message_wrapped);
      length  = lower_array_len(tree_type(m), 0, message_wrapped);
   }

   if (is_report)
      emit_report(message, length, severity, tree_index(stmt));
   else {
      emit_assert(value, message, length, severity, tree_index(stmt));
      if (exit_bb != VCODE_INVALID_BLOCK) {
         emit_jump(exit_bb);
         vcode_select_block(exit_bb);
      }
   }
}

static bool lower_signal_sequential_nets(tree_t decl)
{
   const int nnets = tree_nets(decl);
   int i;
   netid_t last = NETID_INVALID;
   for (i = 0; i < nnets; i++) {
      const netid_t nid = tree_net(decl, i);
      if ((last == NETID_INVALID) || (nid == last + 1))
         last = nid;
      else
         break;
   }

   return (i == nnets) && (nnets > 0);
}

static void lower_sched_event(tree_t on, bool is_static)
{
   tree_kind_t expr_kind = tree_kind(on);
   if (expr_kind != T_REF && expr_kind != T_ARRAY_REF
       && expr_kind != T_ARRAY_SLICE) {
      // It is possible for constant folding to replace a signal with
      // a constant which will then appear in a sensitivity list so
      // just ignore it
      return;
   }

   tree_t decl = NULL;
   switch (expr_kind) {
   case T_REF:
      decl = tree_ref(on);
      break;

   case T_ARRAY_REF:
   case T_ARRAY_SLICE:
      decl = tree_ref(tree_value(on));
      break;

   default:
      assert(false);
   }

   tree_kind_t kind = tree_kind(decl);
   if (kind == T_ALIAS) {
      lower_sched_event(tree_value(decl), is_static);
      return;
   }
   else if (kind != T_SIGNAL_DECL && kind != T_PORT_DECL) {
      // As above, a port could have been rewritten to reference a
      // constant declaration or enumeration literal, in which case
      // just ignore it too
      return;
   }

   type_t type = tree_type(decl);
   type_t expr_type = tree_type(on);

   const bool array = type_is_array(type);

   vcode_reg_t n_elems = VCODE_INVALID_REG, nets = VCODE_INVALID_REG;
   bool sequential = false;
   if (expr_kind == T_REF) {
      nets = lower_signal_ref(decl, EXPR_LVALUE);

      if (array)
         n_elems = lower_array_total_len(type, nets);
      else if (type_is_record(type))
         n_elems = emit_const(vtype_offset(), type_width(type));
      else
         n_elems = emit_const(vtype_offset(), 1);

      if (array && !lower_const_bounds(type)) {
         // Unwrap the meta-data to get nets array
         nets = emit_unwrap(nets);
      }

      // Try to optimise the case where the list of nets is sequential
      // and known at compile time
      if (kind == T_SIGNAL_DECL && array)
         sequential = lower_signal_sequential_nets(decl);
   }
   else {
      assert(array);
      nets = lower_expr(on, EXPR_LVALUE);

      if (type_is_array(expr_type))
         n_elems = lower_array_total_len(expr_type, VCODE_INVALID_REG);
      else
         n_elems = emit_const(vtype_offset(),1);
   }

   const int flags =
      (sequential ? SCHED_SEQUENTIAL : 0)
      | (is_static ? SCHED_STATIC : 0);

   emit_sched_event(nets, n_elems, flags);
}

static void lower_wait(tree_t wait)
{
   const bool is_static = tree_attr_int(wait, static_i, 0);
   assert(!is_static || (!tree_has_delay(wait) && !tree_has_value(wait)));

   vcode_block_t active_bb = vcode_active_block();
   if (is_static) {
      // This process is always sensitive to the same set of signals so
      // only call _sched_event once at startup
      vcode_select_block(0);
   }

   const int ntriggers = tree_triggers(wait);
   for (int i = 0; i < ntriggers; i++)
      lower_sched_event(tree_trigger(wait, i), is_static);

   if (is_static)
      vcode_select_block(active_bb);

   vcode_reg_t delay = VCODE_INVALID_REG;
   if (tree_has_delay(wait))
      delay = lower_expr(tree_delay(wait), EXPR_RVALUE);

   vcode_block_t resume = emit_block();
   emit_wait(resume, delay);

   vcode_select_block(resume);
}

static void lower_var_assign(tree_t stmt)
{
   tree_t value = tree_value(stmt);
   vcode_reg_t value_reg = lower_expr(value, EXPR_RVALUE);

   tree_t target = tree_target(stmt);
   type_t type = tree_type(target);

   const bool is_var_decl =
      tree_kind(target) == T_REF && tree_kind(tree_ref(target)) == T_VAR_DECL;
   const bool is_scalar = type_is_scalar(type);

   if (is_scalar || type_is_access(type)) {
      vcode_reg_t loaded_value = lower_reify(value_reg);
      if (is_scalar)
         emit_bounds(loaded_value, lower_bounds(type));
      if (is_var_decl)
         emit_store(loaded_value, lower_get_var(tree_ref(target)));
      else
         emit_store_indirect(loaded_value, lower_expr(target, EXPR_LVALUE));
   }
   else if (type_is_array(type)) {
      vcode_reg_t count = lower_array_total_len(tree_type(value), value_reg);
      vcode_reg_t src_data = lower_array_data(value_reg);
      vcode_reg_t target_reg = lower_expr(target, EXPR_LVALUE);
      emit_copy(lower_array_data(target_reg), src_data, count);
   }
   else {
      vcode_reg_t count = emit_const(vtype_offset(), 1);
      emit_copy(lower_expr(target, EXPR_LVALUE), value_reg, count);
   }
}

static void lower_signal_assign(tree_t stmt)
{
   vcode_reg_t reject;
   if (tree_has_reject(stmt))
      reject = lower_expr(tree_reject(stmt), EXPR_RVALUE);
   else
      reject = emit_const(vtype_int(INT64_MIN, INT64_MAX), 0);

   tree_t target = tree_target(stmt);
   type_t target_type = tree_type(target);

   vcode_reg_t nets = lower_expr(tree_target(stmt), EXPR_LVALUE);

   const int nwaveforms = tree_waveforms(stmt);
   for (int i = 0; i < nwaveforms; i++) {
      tree_t w = tree_waveform(stmt, i);

      vcode_reg_t rhs = lower_expr(tree_value(w), EXPR_RVALUE);

      if (type_is_scalar(target_type))
         emit_bounds(lower_reify(rhs), lower_bounds(target_type));
      else
         ;  // TODO: bounds check

      vcode_reg_t after;
      if (tree_has_delay(w))
         after = lower_expr(tree_delay(w), EXPR_RVALUE);
      else
         after = emit_const(vtype_int(INT64_MIN, INT64_MAX), 0);

      if (type_is_array(target_type)) {
         vcode_reg_t count_reg = lower_array_total_len(target_type, rhs);
         vcode_reg_t data_reg  = lower_array_data(rhs);
         emit_sched_waveform(nets, count_reg, data_reg, reject, after);
      }
      else {
         vcode_reg_t count_reg = emit_const(vtype_offset(), 1);
         emit_sched_waveform(nets, count_reg, rhs, reject, after);
      }
   }
}

static void lower_if(tree_t stmt, loop_stack_t *loops)
{
   vcode_reg_t value = lower_reify_expr(tree_value(stmt));

   const int nelses = tree_else_stmts(stmt);

   vcode_block_t btrue = emit_block();
   vcode_block_t bfalse = nelses > 0 ? emit_block() : VCODE_INVALID_BLOCK;
   vcode_block_t bmerge = nelses > 0 ? VCODE_INVALID_BLOCK : emit_block();

   emit_cond(value, btrue, nelses > 0 ? bfalse : bmerge);

   vcode_select_block(btrue);

   const int nstmts = tree_stmts(stmt);
   for (int i = 0; i < nstmts; i++)
      lower_stmt(tree_stmt(stmt, i), loops);

   if (!vcode_block_finished()) {
      if (bmerge == VCODE_INVALID_BLOCK)
         bmerge = emit_block();
      emit_jump(bmerge);
   }

   if (nelses > 0) {
      vcode_select_block(bfalse);

      for (int i = 0; i < nelses; i++)
         lower_stmt(tree_else_stmt(stmt, i), loops);

      if (!vcode_block_finished()) {
         if (bmerge == VCODE_INVALID_BLOCK)
            bmerge = emit_block();
         emit_jump(bmerge);
      }
   }

   if (bmerge != VCODE_INVALID_BLOCK)
      vcode_select_block(bmerge);
}

static void lower_return(tree_t stmt)
{
   if (tree_has_value(stmt)) {
      tree_t value = tree_value(stmt);

      vtype_kind_t result_kind = vtype_kind(vcode_unit_result());

      type_t type = tree_type(value);
      if (type_is_scalar(type)) {
         vcode_reg_t result = lower_reify_expr(value);
         emit_bounds(result, lower_bounds(type));
         emit_return(result);
      }
      else if (result_kind == VCODE_TYPE_UARRAY) {
         vcode_reg_t array =
            true || lower_const_bounds(type)
            ? lower_expr(value, EXPR_RVALUE)
            : lower_reify_expr(value);

         if (vtype_kind(vcode_reg_type(array)) == VCODE_TYPE_UARRAY)
            emit_return(array);
         else {
            // TODO: reimplement the "returned" attribute

            vcode_reg_t count = lower_array_total_len(type, array);

            type_t elem = type_elem(type);
            vcode_reg_t data =
               emit_alloca(lower_type(elem), lower_bounds(elem), count);
            emit_copy(data, lower_array_data(array), count);

            emit_return(lower_wrap(type, data));
         }
      }
      else if (result_kind == VCODE_TYPE_POINTER)
         emit_return(lower_array_data(lower_expr(value, EXPR_RVALUE)));
      else
         emit_return(lower_expr(value, EXPR_RVALUE));
   }
   else
      emit_return(VCODE_INVALID_REG);
}

static void lower_pcall(tree_t pcall)
{
   tree_t decl = tree_ref(pcall);

   ident_t builtin = tree_attr_str(decl, builtin_i);
   if (builtin != NULL) {
      lower_builtin(pcall, builtin);
      return;
   }

   ident_t name = lower_mangle_func(decl);

   const int nargs = tree_params(pcall);
   vcode_reg_t args[nargs];
   for (int i = 0; i < nargs; i++)
      args[i] = lower_subprogram_arg(pcall, i);

   if (tree_attr_int(decl, nested_i, 0))
      assert(false);
   else {
      if (tree_attr_int(decl, never_waits_i, 0))
         emit_fcall(name, VCODE_INVALID_TYPE, args, nargs);
      else {
         vcode_block_t resume_bb = emit_block();
         emit_pcall(name, args, nargs, resume_bb);
         vcode_select_block(resume_bb);
         emit_resume(name);
      }
   }
}

static void lower_while(tree_t stmt, loop_stack_t *loops)
{
   vcode_block_t test_bb, body_bb, exit_bb;
   if (tree_has_value(stmt)) {
      test_bb = emit_block();
      body_bb = emit_block();
      exit_bb = emit_block();

      emit_jump(test_bb);

      vcode_select_block(test_bb);
      emit_cond(lower_reify_expr(tree_value(stmt)), body_bb, exit_bb);
   }
   else {
      test_bb = body_bb =
         vcode_block_empty() ? vcode_active_block() : emit_block();
      exit_bb = emit_block();

      emit_jump(body_bb);
   }

   vcode_select_block(body_bb);

   loop_stack_t this = {
      .up      = loops,
      .name    = tree_ident(stmt),
      .test_bb = test_bb,
      .exit_bb = exit_bb
   };

   const int nstmts = tree_stmts(stmt);
   for (int i = 0; i < nstmts; i++)
      lower_stmt(tree_stmt(stmt, i), &this);

   if (!vcode_block_finished())
      emit_jump(test_bb);

   vcode_select_block(exit_bb);
}

static void lower_block(tree_t block, loop_stack_t *loops)
{
   const int nstmts = tree_stmts(block);
   for (int i = 0; i < nstmts; i++)
      lower_stmt(tree_stmt(block, i), loops);
}

static void lower_loop_control(tree_t stmt, loop_stack_t *loops)
{
   vcode_block_t false_bb = emit_block();

   if (tree_has_value(stmt)) {
      vcode_block_t true_bb = emit_block();

      // TODO: cond coverage here
      vcode_reg_t result = lower_reify_expr(tree_value(stmt));
      emit_cond(result, true_bb, false_bb);

      vcode_select_block(true_bb);
   }

   ident_t label = tree_ident2(stmt);
   loop_stack_t *it;
   for (it = loops; it != NULL && it->name != label; it = it->up)
      ;
   assert(it != NULL);

   emit_jump(tree_kind(stmt) == T_EXIT ? it->exit_bb : it->test_bb);

   vcode_select_block(false_bb);
}

static void lower_case_scalar(tree_t stmt, loop_stack_t *loops)
{
   const int nassocs = tree_assocs(stmt);

   vcode_block_t start_bb = vcode_active_block();
   vcode_block_t def_bb = VCODE_INVALID_BLOCK;

   vcode_block_t *blocks LOCAL = xcalloc(nassocs * sizeof(vcode_block_t));
   vcode_reg_t *cases LOCAL = xcalloc(nassocs * sizeof(vcode_reg_t));

   int ncases = 0;
   for (int i = 0; i < nassocs; i++) {
      tree_t a = tree_assoc(stmt, i);
      blocks[i] = emit_block();

      if (tree_subkind(a) == A_OTHERS)
         def_bb = blocks[i];
      else {
         vcode_select_block(start_bb);
         cases[i] = lower_reify_expr(tree_name(a));
         ncases++;
      }

      vcode_select_block(blocks[i]);
      lower_stmt(tree_value(a), loops);

      blocks[i] = vcode_active_block();
   }

   vcode_block_t exit_bb = emit_block();
   if (def_bb == VCODE_INVALID_BLOCK)
      def_bb = exit_bb;

   for (int i = 0; i < nassocs; i++) {
      vcode_select_block(blocks[i]);
      if (!vcode_block_finished())
         emit_jump(exit_bb);
   }

   vcode_select_block(start_bb);
   vcode_reg_t value_reg = lower_reify_expr(tree_value(stmt));
   emit_case(value_reg, def_bb, cases, blocks, ncases);

   vcode_select_block(exit_bb);
}

static void lower_case(tree_t stmt, loop_stack_t *loops)
{
   assert(type_is_scalar(tree_type(tree_value(stmt))));

   lower_case_scalar(stmt, loops);
}

static void lower_stmt(tree_t stmt, loop_stack_t *loops)
{
   switch (tree_kind(stmt)) {
   case T_ASSERT:
      lower_assert(stmt);
      break;
   case T_WAIT:
      lower_wait(stmt);
      break;
   case T_VAR_ASSIGN:
      lower_var_assign(stmt);
      break;
   case T_SIGNAL_ASSIGN:
      lower_signal_assign(stmt);
      break;
   case T_IF:
      lower_if(stmt, loops);
      break;
   case T_RETURN:
      lower_return(stmt);
      break;
   case T_PCALL:
      lower_pcall(stmt);
      break;
   case T_WHILE:
      lower_while(stmt, loops);
      break;
   case T_BLOCK:
      lower_block(stmt, loops);
      break;
   case T_EXIT:
   case T_NEXT:
      lower_loop_control(stmt, loops);
      break;
   case T_CASE:
      lower_case(stmt, loops);
      break;
   default:
      fatal_at(tree_loc(stmt), "cannot lower statement kind %s",
               tree_kind_str(tree_kind(stmt)));
   }
}

static void lower_decl(tree_t decl)
{
   switch (tree_kind(decl)) {
   case T_CONST_DECL:
      if (type_is_scalar(tree_type(decl)))
          break;
      // Fall-through

   case T_VAR_DECL:
      {
         type_t type = tree_type(decl);
         vcode_type_t vtype = lower_type(type);
         vcode_type_t vbounds = lower_bounds(type);
         vcode_var_t var = emit_var(vtype, vbounds, tree_ident(decl));
         tree_add_attr_int(decl, vcode_obj_i, var);

         if (tree_has_value(decl)) {
            vcode_reg_t value = lower_expr(tree_value(decl), EXPR_RVALUE);
            if (type_is_array(type) && lower_const_bounds(type)) {
               vcode_reg_t count = lower_array_total_len(type, value);
               vcode_reg_t dest  = emit_index(var, VCODE_INVALID_REG);
               emit_copy(dest, value, count);
            }
            else if (type_is_record(type)) {
               vcode_reg_t count = emit_const(vtype_offset(), 1);
               vcode_reg_t dest  = emit_index(var, VCODE_INVALID_REG);
               emit_copy(dest, value, count);
            }
            else if (type_is_access(type))
               emit_store(value, var);
            else {
               emit_bounds(value, vbounds);
               emit_store(value, var);
            }
         }
      }
      break;

   case T_SIGNAL_DECL:
      {
         type_t type = tree_type(decl);
         vcode_type_t ltype = lower_type(type);
         vcode_type_t bounds = lower_bounds(type);
         ident_t name = tree_ident(decl);

         vcode_var_t shadow = VCODE_INVALID_VAR;
         if (lower_signal_sequential_nets(decl)) {
            vcode_type_t shadow_type   = ltype;
            vcode_type_t shadow_bounds = bounds;
            if (type_is_array(type)) {
               shadow_type = vtype_elem(ltype);
               shadow_bounds = vtype_elem(bounds);
            }

            shadow = emit_var(vtype_pointer(shadow_type), shadow_bounds,
                              ident_prefix(ident_new("resolved"), name, '_'));
         }

         const int nnets = tree_nets(decl);
         netid_t *nets = xmalloc(sizeof(netid_t) * nnets);
         for (int i = 0; i < nnets; i++)
            nets[i] = tree_net(decl, i);

         vcode_type_t stype = vtype_signal(ltype);
         vcode_signal_t sig = emit_signal(stype, bounds, name, shadow,
                                          nets, nnets);
         tree_add_attr_int(decl, vcode_obj_i, sig);

         // Internal signals that were generated from ports will not have
         // an initial value
         if (tree_has_value(decl))
            emit_set_initial(sig, lower_expr(tree_value(decl), EXPR_RVALUE),
                             tree_index(decl));

         if (shadow != VCODE_INVALID_VAR)
            emit_resolved_address(shadow, sig);
      }
      break;

   case T_FILE_DECL:
      {
         type_t type = tree_type(decl);
         vcode_type_t vtype = lower_type(type);
         vcode_var_t var = emit_var(vtype, vtype, tree_ident(decl));
         tree_add_attr_int(decl, vcode_obj_i, var);

         // TODO: set file to NULL here

         if (tree_has_value(decl)) {
            // Generate initial call to file_open

            tree_t value = tree_value(decl);

            vcode_reg_t name_array = lower_expr(tree_value(decl), EXPR_RVALUE);
            vcode_reg_t name_data  = lower_array_data(name_array);
            vcode_reg_t name_len   = lower_array_len(tree_type(value), 0,
                                                     name_array);
            vcode_reg_t file_ptr   = emit_index(var, VCODE_INVALID_REG);
            vcode_reg_t mode       = lower_reify_expr(tree_file_mode(decl));

            emit_file_open(file_ptr, name_data, name_len, mode,
                           VCODE_INVALID_REG);
         }
      }
      break;

   case T_TYPE_DECL:
   case T_HIER:
   case T_ALIAS:
   case T_FUNC_DECL:
   case T_PROC_DECL:
   case T_ATTR_SPEC:
      break;

   default:
      fatal_at(tree_loc(decl), "cannot lower decl kind %s",
               tree_kind_str(tree_kind(decl)));
   }
}

static void lower_finished(void)
{
   vcode_opt();

   if (verbose)
      vcode_dump();
}

static void lower_cleanup(tree_t scope)
{
   if (tree_kind(scope) == T_FUNC_BODY) {
      const int nports = tree_ports(scope);
      for (int i = 0; i < nports; i++)
         tree_remove_attr(tree_port(scope, i), vcode_obj_i);
   }

   const int ndecls = tree_decls(scope);
   for (int i = 0; i < ndecls; i++)
      tree_remove_attr(tree_decl(scope, i), vcode_obj_i);
}

static void lower_decls(tree_t scope)
{
   const int ndecls = tree_decls(scope);
   for (int i = 0; i < ndecls; i++) {
      tree_t d = tree_decl(scope, i);
      const tree_kind_t kind = tree_kind(d);
      if (kind != T_PROC_BODY && kind != T_FUNC_BODY)
         lower_decl(d);
   }
}

static void lower_subprograms(tree_t scope, vcode_unit_t context)
{
   const tree_kind_t scope_kind = tree_kind(scope);
   const bool nested = scope_kind != T_ELAB && scope_kind != T_PACK_BODY;

   const int ndecls = tree_decls(scope);
   for (int i = 0; i < ndecls; i++) {
      tree_t d = tree_decl(scope, i);
      const tree_kind_t kind = tree_kind(d);
      switch (kind) {
      case T_FUNC_BODY:
         if (nested)
            tree_add_attr_int(d, nested_i, 1);
         lower_func_body(d, context);
         break;
      case T_PROC_BODY:
         if (nested)
            tree_add_attr_int(d, nested_i, 1);
         lower_proc_body(d, context);
         break;
      default:
         break;
      }
   }
}

static bool lower_has_subprograms(tree_t scope)
{
   const int ndecls = tree_decls(scope);
   for (int i = 0; i < ndecls; i++) {
      const tree_kind_t kind = tree_kind(tree_decl(scope, i));
      if (kind == T_FUNC_BODY || kind == T_PROC_BODY)
         return true;
   }

   return false;
}

static void lower_subprogram_ports(tree_t body, bool has_subprograms)
{
   const int nports = tree_ports(body);
   for (int i = 0; i < nports; i++) {
      tree_t p = tree_port(body, i);
      type_t type = tree_type(p);

      const bool is_uarray = type_is_array(type) && !lower_const_bounds(type);

      vcode_type_t vtype, vbounds;
      switch (tree_class(p)) {
      case C_SIGNAL:
         if (is_uarray)
            assert(false);
         else {
            vtype   = vtype_signal(lower_type(type));
            vbounds = vtype;
         }
         break;

      case C_VARIABLE:
      case C_DEFAULT:
      case C_CONSTANT:
         {
            if (type_is_array(type) && lower_const_bounds(type)) {
               vtype = vtype_pointer(lower_type(type_elem(type)));
               vbounds = vtype;
            }
            else if (type_is_record(type)) {
               vtype = vtype_pointer(lower_type(type));
               vbounds = vtype;
            }
            else {
               vtype = lower_type(type);
               vbounds = lower_bounds(type);
            }

            const port_mode_t mode = tree_subkind(p);
            if ((mode == PORT_OUT || mode == PORT_INOUT) && !is_uarray)
               vtype = vtype_pointer(vtype);
         }
         break;

      case C_FILE:
         vtype = vtype_pointer(lower_type(type));
         vbounds = vtype;
         break;

      default:
         assert(false);
      }

      vcode_reg_t reg = emit_param(vtype, vbounds, tree_ident(p));
      tree_add_attr_int(p, vcode_obj_i, reg);
      if (has_subprograms)
         tree_add_attr_int(p, nested_i, vcode_unit_depth());
   }
}

static void lower_proc_body(tree_t body, vcode_unit_t context)
{
   const bool never_waits = tree_attr_int(body, never_waits_i, 0);

   vcode_select_unit(context);

   vcode_unit_t vu;
   if (never_waits)
      vu = emit_function(lower_mangle_func(body), context, VCODE_INVALID_TYPE);
   else
      vu = emit_procedure(lower_mangle_func(body), context);

   vcode_block_t bb = vcode_active_block();

   const bool has_subprograms = lower_has_subprograms(body);
   lower_subprogram_ports(body, has_subprograms);

   lower_decls(body);

   if (has_subprograms) {
      lower_subprograms(body, vu);
      vcode_select_unit(vu);
      vcode_select_block(bb);
   }

   const int nstmts = tree_stmts(body);
   for (int i = 0; i < nstmts; i++)
      lower_stmt(tree_stmt(body, i), NULL);

   if (!vcode_block_finished())
      emit_return(VCODE_INVALID_REG);

   lower_finished();

   assert(!tree_has_code(body));
   tree_set_code(body, vu);

   lower_cleanup(body);
}

static void lower_func_body(tree_t body, vcode_unit_t context)
{
   vcode_select_unit(context);

   type_t result = type_result(tree_type(body));
   vcode_type_t vtype = VCODE_INVALID_TYPE;
   if (type_is_array(result) && lower_const_bounds(result))
      vtype = vtype_pointer(lower_type(type_elem(result)));
   else
      vtype = lower_type(result);

   vcode_unit_t vu = emit_function(lower_mangle_func(body), context, vtype);
   vcode_block_t bb = vcode_active_block();

   const bool has_subprograms = lower_has_subprograms(body);
   lower_subprogram_ports(body, has_subprograms);

   lower_decls(body);

   if (has_subprograms) {
      lower_subprograms(body, vu);
      vcode_select_unit(vu);
      vcode_select_block(bb);
   }

   const int nstmts = tree_stmts(body);
   for (int i = 0; i < nstmts; i++)
      lower_stmt(tree_stmt(body, i), NULL);

   lower_finished();

   assert(!tree_has_code(body));
   tree_set_code(body, vu);

   lower_cleanup(body);
}

static bool lower_driver_nets(tree_t t, tree_t *decl,
                              vcode_reg_t *all_nets, int64_t *all_length,
                              vcode_reg_t *driven_nets, int64_t *driven_length,
                              bool *has_non_const, tree_t proc)
{
   switch (tree_kind(t)) {
   case T_REF:
      {
         *decl = tree_ref(t);

         if (tree_attr_ptr(*decl, drives_all_i) == proc)
            return false;
         else if ((tree_kind(*decl) != T_SIGNAL_DECL)
                  || (tree_nets(*decl) == 0))
            return false;

         *all_nets = *driven_nets = lower_signal_ref(*decl, EXPR_LVALUE);
         *all_length = *driven_length = type_width(tree_type(*decl));
      }
      break;

   case T_ARRAY_REF:
      {
         if (!lower_driver_nets(tree_value(t), decl, all_nets, all_length,
                                driven_nets, driven_length,
                                has_non_const, proc))
            return false;
         else if (*driven_nets == VCODE_INVALID_REG)
            return false;
         else if (*has_non_const)
            return true;

         bool all_const = true;
         const int nparams = tree_params(t);
         for (int i = 0; (i < nparams) && all_const; i++) {
            if (!lower_is_const(tree_value(tree_param(t, i))))
               all_const = false;
         }

         if (all_const) {
            vcode_reg_t idx =
               emit_mul(lower_array_ref_offset(t, *driven_nets),
                        emit_const(vtype_offset(), type_width(tree_type(t))));

            *driven_nets   = emit_add(*driven_nets, idx);
            *driven_length = type_width(tree_type(t));
         }
         else
            *has_non_const = true;
      }
      break;

   case T_ARRAY_SLICE:
      {
         tree_t value = tree_value(t);
         if (!lower_driver_nets(value, decl, all_nets, all_length, driven_nets,
                                driven_length, has_non_const, proc))
            return false;
         else if (*driven_nets == VCODE_INVALID_REG)
            return false;
         else if (*has_non_const)
            return true;

         range_t r = tree_range(t);
         if (lower_is_const(r.left) && lower_is_const(r.right)) {
            const int stride = type_width(type_elem(tree_type(t)));
            vcode_reg_t idx =
               emit_mul(lower_array_off(lower_reify_expr(r.left), *driven_nets,
                                        tree_type(value), 0),
                        emit_const(vtype_offset(), stride));

            *driven_nets   = emit_add(*driven_nets, idx);
            *driven_length = type_width(tree_type(t));
         }
         else
            *has_non_const = true;
      }
      break;

#if 0
   case T_RECORD_REF:
      {
         tree_t value = tree_value(t);
         if (!cgen_driver_nets(value, decl, all_nets, all_length,
                               driven_nets, driven_length, has_non_const, ctx))
            return false;
         else if (*driven_nets == NULL)
            return false;
         else if (*has_non_const)
            return true;

         type_t rtype = tree_type(value);
         const netid_t offset = record_field_to_net(rtype, tree_ident(t));

         LLVMValueRef indexes[] = { llvm_int32(offset) };
         LLVMValueRef field_nets = LLVMBuildGEP(builder, *driven_nets, indexes,
                                                ARRAY_LEN(indexes), "");

         *driven_nets   = field_nets;
         *driven_length = type_width(tree_type(t));
      }
      break;
#endif

   case T_LITERAL:
      return false;

   default:
      assert(false);
   }

   return true;
}

static void lower_driver_target(tree_t expr, tree_t proc)
{
   if (tree_kind(expr) == T_AGGREGATE) {
      const int nassocs = tree_assocs(expr);
      for (int i = 0; i < nassocs; i++) {
         tree_t a = tree_assoc(expr, i);
         assert(tree_subkind(a) == A_POS);

         lower_driver_target(tree_value(a), proc);
      }

      return;
   }

   tree_t decl = NULL;
   vcode_reg_t all_nets = VCODE_INVALID_REG, driven_nets = VCODE_INVALID_REG;
   int64_t all_length = 0, driven_length = 0;
   bool has_non_const = false;

   if (!lower_driver_nets(expr, &decl, &all_nets, &all_length,
                          &driven_nets, &driven_length, &has_non_const, proc))
      return;

   assert(decl != NULL);

   if (all_length == driven_length)
      tree_add_attr_ptr(decl, drives_all_i, proc);

   vcode_reg_t init_reg = VCODE_INVALID_REG;
   tree_t init = tree_attr_tree(decl, driver_init_i);
   if (init != NULL)
      init_reg = lower_expr(init, EXPR_RVALUE);

   vcode_reg_t all_length_reg = emit_const(vtype_offset(), all_length);
   vcode_reg_t driven_length_reg = emit_const(vtype_offset(), driven_length);
   emit_alloc_driver(all_nets, all_length_reg, driven_nets,
                     driven_length_reg, init_reg);
}

static void lower_driver_fn(tree_t t, void *_ctx)
{
   tree_t proc = (tree_t)_ctx;

   switch (tree_kind(t)) {
   case T_SIGNAL_ASSIGN:
      lower_driver_target(tree_target(t), proc);
      break;

   case T_PCALL:
      {
         tree_t pdecl = tree_ref(t);
         const int nparams = tree_params(t);
         assert(nparams == tree_ports(pdecl));

         for (int i = 0; i < nparams; i++) {
            tree_t port = tree_port(pdecl, i);
            if (tree_class(port) == C_SIGNAL && tree_subkind(port) != PORT_IN) {
               tree_t param = tree_param(t, i);
               assert(tree_subkind(param) == P_POS);
               lower_driver_target(tree_value(param), proc);
            }
         }
      }
      break;

   default:
      return;
   }
}

static void lower_process(tree_t proc, vcode_unit_t context)
{
   vcode_unit_t vu = emit_process(tree_ident(proc), context);

   lower_decls(proc);
   tree_visit(proc, lower_driver_fn, proc);

   lower_subprograms(proc, vu);
   vcode_select_unit(vu);

   vcode_block_t start_bb = emit_block();
   vcode_select_block(start_bb);

   const int nstmts = tree_stmts(proc);
   for (int i = 0; i < nstmts; i++)
      lower_stmt(tree_stmt(proc, i), NULL);

   if (!vcode_block_finished())
      emit_jump(start_bb);

   vcode_select_block(0);
   emit_return(VCODE_INVALID_REG);

   lower_finished();

   assert(!tree_has_code(proc));
   tree_set_code(proc, vu);

   lower_cleanup(proc);
}

static void lower_elab(tree_t unit)
{
   vcode_unit_t context = emit_context(tree_ident(unit));
   tree_set_code(unit, context);

   lower_decls(unit);

   emit_return(VCODE_INVALID_REG);

   lower_finished();

   const int nstmts = tree_stmts(unit);
   for (int i = 0; i < nstmts; i++) {
      tree_t s = tree_stmt(unit, i);
      assert(tree_kind(s) == T_PROCESS);
      lower_process(s, context);
   }

   lower_subprograms(unit, context);
   lower_cleanup(unit);
}

static void lower_pack_body(tree_t unit)
{
   vcode_unit_t context = emit_context(tree_ident(unit));
   tree_set_code(unit, context);

   lower_decls(unit);

   emit_return(VCODE_INVALID_REG);

   lower_finished();

   lower_subprograms(unit, context);
   lower_cleanup(unit);
}

static void lower_package(tree_t unit)
{
   vcode_unit_t context = emit_context(tree_ident(unit));
   tree_set_code(unit, context);

   lower_decls(unit);

   emit_return(VCODE_INVALID_REG);

   lower_finished();
   lower_cleanup(unit);
}

void lower_unit(tree_t unit)
{
   builtin_i     = ident_new("builtin");
   foreign_i     = ident_new("FOREIGN");
   vcode_obj_i   = ident_new("vcode_obj");
   nested_i      = ident_new("nested");
   drives_all_i  = ident_new("drives_all");
   driver_init_i = ident_new("driver_init");
   static_i      = ident_new("static");
   never_waits_i = ident_new("never_waits");
   mangled_i     = ident_new("mangled");

   verbose = (getenv("NVC_LOWER_VERBOSE") != NULL);

   switch (tree_kind(unit)) {
   case T_ELAB:
      lower_elab(unit);
      break;
   case T_PACK_BODY:
      lower_pack_body(unit);
      break;
   case T_PACKAGE:
      lower_package(unit);
      break;
   default:
      fatal("cannot lower unit kind %s to vcode",
            tree_kind_str(tree_kind(unit)));
   }

   vcode_close();
}
