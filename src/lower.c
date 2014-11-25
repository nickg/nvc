//
//  Copyright (C) 2014  Nick Gasson
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

#include <assert.h>
#include <stdlib.h>
#include <inttypes.h>

typedef enum {
   EXPR_LVALUE,
   EXPR_RVALUE
} expr_ctx_t;

static ident_t builtin_i;
static ident_t foreign_i;
static ident_t vcode_obj_i;
static bool    verbose = false;

static vcode_reg_t lower_expr(tree_t expr, expr_ctx_t ctx);
static vcode_reg_t lower_reify_expr(tree_t expr);
static vcode_type_t lower_bounds(type_t type);
static void lower_stmt(tree_t stmt);
static void lower_func_body(tree_t body, vcode_unit_t context);

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
   if (type_is_unconstrained(type))
      return false;
   else {
      const int ndims = type_dims(type);
      for (int i = 0; i < ndims; i++) {
         range_t r = type_dim(type, i);
         if (!lower_is_const(r.left) || !lower_is_const(r.right))
            return false;
      }

      return true;
   }
}

static vcode_reg_t lower_array_data(vcode_reg_t reg)
{
   vcode_type_t type = vcode_reg_type(reg);
   switch (vtype_kind(type)) {
   case VCODE_TYPE_UARRAY:
      return emit_unwrap(reg);

   case VCODE_TYPE_POINTER:
      return reg;

   case VCODE_TYPE_CARRAY:
      return emit_cast(vtype_pointer(vtype_elem(type)), reg);

   default:
      fatal_trace("invalid type in lower_array_data");
   }
}

static vcode_reg_t lower_array_left(type_t type, int dim, vcode_reg_t reg)
{
   if (!lower_const_bounds(type)) {
      assert(reg != VCODE_INVALID_REG);
      return emit_uarray_left(reg, dim);
   }
   else {
      range_t r = type_dim(type, dim);
      return lower_reify_expr(r.left);
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

      vcode_reg_t diff = emit_select(emit_uarray_dir(reg, dim),
                                     emit_sub(left_reg, right_reg),
                                     emit_sub(right_reg, left_reg));

      len_reg = emit_add(diff, emit_const(vcode_reg_type(left_reg), 1));
   }
   else {
      assert(type_dims(type) == 1);
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
      case RANGE_EXPR:
         fatal_trace("unexpected range direction in lower_array_len");
      }

      len_reg = emit_add(diff, emit_const(vcode_reg_type(left_reg), 1));
   }

   vcode_type_t offset_type = vtype_offset();
   vcode_reg_t cast_reg = emit_cast(offset_type, len_reg);
   vcode_reg_t zero_reg = emit_const(offset_type, 0);
   vcode_reg_t neg_reg = emit_cmp(VCODE_CMP_LT, cast_reg, zero_reg);

   return emit_select(neg_reg, zero_reg, cast_reg);
}

static vcode_type_t lower_type(type_t type)
{
   switch (type_kind(type)) {
   case T_SUBTYPE:
      if (type_is_array(type)) {
         type_t elem = type_elem(type);
         vcode_type_t elem_type   = lower_type(elem);
         vcode_type_t elem_bounds = lower_bounds(elem);

         const int ndims = type_dims(type);
         assert(ndims > 0);

         if (lower_const_bounds(type)) {
            vcode_type_t bounds[ndims];
            for (int i = 0; i < ndims; i++) {
               range_t r = type_dim(type, i);
               int64_t low, high;
               range_bounds(r, &low, &high);
               bounds[i] = vtype_int(low, high);
            }

            return vtype_carray(bounds, ndims, elem_type, elem_bounds);
         }
         else {
            vcode_type_t dim_types[ndims];
            for (int i = 0; i < ndims; i++)
               dim_types[i] = lower_type(tree_type(type_dim(type, i).left));
            return vtype_uarray(dim_types, ndims, elem_type, elem_bounds);
         }
      }
      else
         return lower_type(type_base(type));

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

   case T_UARRAY:
      {
         const int nindex = type_index_constrs(type);
         vcode_type_t dim_types[nindex];
         for (int i = 0; i < nindex; i++)
            dim_types[i] = lower_type(type_index_constr(type, i));

         type_t elem = type_elem(type);
         return vtype_uarray(dim_types, nindex, lower_type(elem),
                             lower_bounds(elem));
      }

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
   else if (vtype_kind(vcode_reg_type(reg)) == VCODE_TYPE_POINTER)
      return emit_load_indirect(reg);
   else
      return reg;
}

static vcode_reg_t lower_reify_expr(tree_t expr)
{
   return lower_reify(lower_expr(expr, EXPR_RVALUE));
}

static vcode_reg_t lower_func_arg(tree_t fcall, int nth)
{
   if (nth < tree_params(fcall)) {
      tree_t param = tree_param(fcall, nth);

      assert(tree_subkind(param) == P_POS);
      assert(tree_pos(param) == nth);

      tree_t value = tree_value(param);
      vcode_reg_t reg = lower_expr(value, EXPR_RVALUE);
      return type_is_scalar(tree_type(value)) ? lower_reify(reg) : reg;
   }
   else
      return VCODE_INVALID_REG;
}

#if 0
static vcode_reg_t lower_wrap(type_t type, vcode_reg_t data)
{
   assert(type_is_array(type));
   const vtype_kind_t vtkind = vtype_kind(vcode_reg_type(data));
   if (vtkind == VCODE_TYPE_UARRAY)
      return data;
   else {
      //assert(vtkind == VCODE_TYPE_POINTER || vtkind == VCODE_TYPE_CARRAY);
      assert(!type_is_unconstrained(type));

      const int ndims = type_dims(type);
      vcode_dim_t dims[ndims];
      for (int i = 0; i < ndims; i++) {
         range_t r = type_dim(type, i);
         dims[i].left  = lower_reify_expr(r.left);
         dims[i].right = lower_reify_expr(r.right);
         dims[i].dir   = emit_const(vtype_bool(), r.kind == RANGE_DOWNTO);
      }

      return emit_wrap(data, dims, ndims);
   }
}
#endif

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
                                   tree_t fcall, vcode_cmp_t pred)
{
   emit_comment("Begin array cmp line %d", tree_loc(fcall)->first_line);
   return lower_array_cmp_inner(lower_array_data(r0),
                                lower_array_data(r1),
                                r0, r1,
                                tree_type(tree_value(tree_param(fcall, 0))),
                                tree_type(tree_value(tree_param(fcall, 1))),
                                pred);
}

static vcode_reg_t lower_builtin(tree_t fcall, ident_t builtin)
{
   vcode_reg_t r0 = lower_func_arg(fcall, 0);
   vcode_reg_t r1 = lower_func_arg(fcall, 1);

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
      return emit_mul(r0, r1);
   else if (icmp(builtin, "add"))
      return emit_add(r0, r1);
   else if (icmp(builtin, "sub"))
      return emit_sub(r0, r1);
   else if (icmp(builtin, "div"))
      return emit_div(r0, r1);
   else if (icmp(builtin, "exp"))
      return emit_exp(r0, r1);
   else if (icmp(builtin, "mod"))
      return emit_mod(r0, r1);
   else if (icmp(builtin, "rem"))
      return emit_rem(r0, r1);
   else if (icmp(builtin, "neg"))
      return emit_neg(r0);
   else if (icmp(builtin, "abs"))
      return emit_abs(r0);
   else if (icmp(builtin, "identity"))
      return r0;
   else if (icmp(builtin, "not"))
      return emit_not(r0);
   else if (icmp(builtin, "image"))
      return emit_image(r0, tree_index(tree_value(tree_param(fcall, 0))));
   else if (icmp(builtin, "aeq"))
      return lower_array_cmp(r0, r1, fcall, VCODE_CMP_EQ);
   else if (icmp(builtin, "alt"))
      return lower_array_cmp(r0, r1, fcall, VCODE_CMP_LT);
   else if (icmp(builtin, "aleq"))
      return lower_array_cmp(r0, r1, fcall, VCODE_CMP_LEQ);
   else if (icmp(builtin, "agt"))
      return emit_not(lower_array_cmp(r0, r1, fcall, VCODE_CMP_LEQ));
   else if (icmp(builtin, "ageq"))
      return emit_not(lower_array_cmp(r0, r1, fcall, VCODE_CMP_LT));
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
   ident_t name = tree_ident(decl);
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

   const int nargs = tree_params(fcall);
   vcode_reg_t args[nargs];
   for (int i = 0; i < nargs; i++)
      args[i] = lower_func_arg(fcall, i);

   return emit_fcall(name, lower_type(tree_type(fcall)), args, nargs);
}

static vcode_reg_t lower_string_literal(tree_t lit)
{
   type_t ltype = tree_type(lit);
   vcode_type_t vtype = lower_type(type_elem(ltype));

   const int nchars = tree_chars(lit);
   vcode_reg_t *tmp LOCAL = xmalloc(nchars * sizeof(vcode_reg_t));

   for (int i = 0; i < nchars; i++)
      tmp[i] = emit_const(vtype, tree_pos(tree_ref(tree_char(lit, i))));

   return emit_const_array(lower_type(ltype), tmp, nchars);
}

static vcode_reg_t lower_literal(tree_t lit)
{
   switch (tree_subkind(lit)) {
   case L_INT:
      return emit_const(lower_type(tree_type(lit)), tree_ival(lit));

   case L_STRING:
      return lower_string_literal(lit);

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
      return emit_index(var, emit_const(vtype_offset(), 0));
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
         vcode_reg_t r = emit_index(shadow, emit_const(vtype_offset(), 0));
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
   return reg;
}

static vcode_reg_t lower_ref(tree_t ref, expr_ctx_t ctx)
{
   tree_t decl = tree_ref(ref);

   tree_kind_t kind = tree_kind(decl);
   switch (kind) {
   case T_ENUM_LIT:
      return emit_const(lower_type(tree_type(decl)), tree_pos(decl));

   case T_VAR_DECL:
      return lower_var_ref(decl, ctx);

   case T_PORT_DECL:
      return lower_param_ref(decl, ctx);

   case T_SIGNAL_DECL:
      return lower_signal_ref(decl, ctx);

   case T_TYPE_DECL:
      return VCODE_INVALID_REG;

   case T_CONST_DECL:
      // TODO: handle array constants like variables
      assert(!type_is_array(tree_type(decl)));
      return lower_expr(tree_value(decl), ctx);

   default:
      vcode_dump();
      fatal_trace("cannot lower reference to %s", tree_kind_str(kind));
   }
}

static vcode_reg_t lower_array_off(vcode_reg_t off, vcode_reg_t array,
                                   type_t type, unsigned dim)
{
   // Convert VHDL offset 'off' to a zero-based array offset

   vcode_reg_t zeroed = VCODE_INVALID_REG;
   if (vtype_kind(vcode_reg_type(array)) == VCODE_TYPE_UARRAY) {
      vcode_reg_t left_reg = lower_array_left(type, dim, array);

      zeroed = emit_select(emit_uarray_dir(array, dim),
                           emit_sub(off, left_reg),
                           emit_sub(left_reg, off));
   }
   else {
      assert(!type_is_unconstrained(type));
      range_t r = type_dim(type, dim);
      vcode_reg_t left = lower_expr(r.left, EXPR_RVALUE);
      if (r.kind == RANGE_TO)
         zeroed = emit_sub(off, left);
      else
         zeroed = emit_sub(left, off);
   }

   return emit_cast(vtype_offset(), zeroed);
}

static vcode_reg_t lower_array_ref(tree_t ref, expr_ctx_t ctx)
{
   tree_t value = tree_value(ref);
   type_t value_type = tree_type(value);

   vcode_reg_t base = lower_array_data(lower_expr(value, ctx));

   const vtype_kind_t vtkind = vtype_kind(vcode_reg_type(base));
   assert(vtkind == VCODE_TYPE_POINTER || vtkind == VCODE_TYPE_UARRAY);

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
         assert(false);
         //offset = cgen_unalias_index(alias, offset, meta, ctx);
         //type = tree_type(tree_value(alias));
      }

      if (i > 0) {
         assert(false);
         //LLVMValueRef stride = cgen_array_len(type, i, meta);
         //idx = LLVMBuildMul(builder, idx, stride, "stride");
      }

      idx = emit_add(idx, lower_array_off(offset, base, value_type, i));
   }

   return emit_add(base, idx);
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
   *n_elems = 1;
   const int ndims = type_dims(type);
   for (int i = dim; i < ndims; i++) {
      range_t r = type_dim(type, i);

      int64_t low, high;
      range_bounds(r, &low, &high);

      if (high < low)
         *n_elems = 0;
      else
         *n_elems *= (high - low + 1);
   }

   if (*n_elems == 0)
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
      int nsub;
      if (dim < ndims - 1)
         sub = lower_const_array_aggregate(value, type, dim + 1, &nsub);
      else if (value_kind == T_AGGREGATE) {
         sub  = xmalloc(sizeof(vcode_reg_t));   // XXX
         nsub = 1;

         assert(false);
         /*
         type_t sub_type = tree_type(value);
         if (type_is_array(sub_type)) {
            int nvals;
            LLVMValueRef *v = cgen_const_aggregate(value, sub_type,
                                                   0, &nvals, ctx);
            LLVMTypeRef ltype = llvm_type(type_elem(sub_type));

            *sub = LLVMConstArray(ltype, v, nvals);
            free(v);
         }
         else if (type_is_record(sub_type))
            *sub = cgen_record_aggregate(value, true,
                                         cgen_is_const(value), ctx);
         else
         assert(false);*/
      }
      else if (value_kind == T_LITERAL && tree_subkind(value) == L_STRING) {
         /*
         sub  = xmalloc(sizeof(LLVMValueRef));
         nsub = 1;

         int nchars;
         LLVMTypeRef et;
         LLVMValueRef *tmp = cgen_string_literal(value, &nchars, &et);

         *sub = LLVMConstArray(et, tmp, nchars);
         free(tmp);*/
         assert(false);
      }
      else {
         tmp  = lower_expr(value, EXPR_RVALUE);
         nsub = 1;
      }

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
   range_t r = type_dim(agg_type, 0);

   assert(r.kind == RANGE_TO || r.kind == RANGE_DOWNTO);  // XXX
   vcode_reg_t is_downto = emit_const(vtype_bool(), r.kind == RANGE_DOWNTO);

   vcode_reg_t left_reg  = lower_reify_expr(r.left);
   vcode_reg_t right_reg = lower_reify_expr(r.right);
   vcode_reg_t len_reg   = lower_array_len(agg_type, 0, VCODE_INVALID_REG);

   vcode_reg_t mem_reg = emit_alloca(lower_type(elem_type),
                                     lower_bounds(elem_type), len_reg);

   vcode_type_t offset_type = vtype_offset();
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

   if (def_reg == VCODE_INVALID_REG && def_value != NULL)
      def_reg = lower_expr(def_value, EXPR_RVALUE);

   vcode_reg_t what = def_reg;
   for (int i = 0; i < nassocs; i++) {
      tree_t a = tree_assoc(agg, i);

      assoc_kind_t kind = tree_subkind(a);
      vcode_reg_t value_reg = VCODE_INVALID_REG;
      if (kind != A_OTHERS)
         value_reg = lower_expr(tree_value(a), EXPR_RVALUE);

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
            vcode_reg_t name_reg = lower_reify_expr(tree_name(a));
            vcode_reg_t off_reg =
               emit_select(is_downto,
                           emit_sub(left_reg, name_reg),
                           emit_sub(name_reg, left_reg));

            vcode_reg_t eq = emit_cmp(VCODE_CMP_EQ, i_loaded, off_reg);
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
   emit_store_indirect(what, ptr_reg);

   emit_store_indirect(emit_add(i_loaded, emit_const(offset_type, 1)), ivar);
   emit_jump(test_bb);

   // Epilogue
   vcode_select_block(exit_bb);
   emit_comment("End dynamic aggregrate line %d", tree_loc(agg)->first_line);

   vcode_dim_t dim0 = {
      .left  = left_reg,
      .right = right_reg,
      .dir   = emit_const(vtype_bool(), r.kind)
   };
   return emit_wrap(mem_reg, &dim0, 1);
}

static vcode_reg_t lower_aggregate(tree_t expr, expr_ctx_t ctx)
{
   type_t type = tree_type(expr);
   assert(type_is_array(type));

   if (lower_const_bounds(type) && lower_is_const(expr)) {
      int nvals;
      vcode_reg_t *values LOCAL =
         lower_const_array_aggregate(expr, type, 0, &nvals);

      return emit_const_array(lower_type(type), values, nvals);
   }
   else
      return lower_dyn_aggregate(expr, type);
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
   default:
      fatal_at(tree_loc(expr), "cannot lower expression kind %s",
               tree_kind_str(tree_kind(expr)));
   }
}

static void lower_assert(tree_t stmt)
{
   const int is_report = tree_attr_int(stmt, ident_new("is_report"), 0);

   vcode_reg_t severity = lower_reify_expr(tree_severity(stmt));

   vcode_reg_t message = VCODE_INVALID_REG;
   if (tree_has_message(stmt))
      message = lower_expr(tree_message(stmt), EXPR_RVALUE);

   if (is_report)
      emit_report(message, severity, tree_index(stmt));
   else {
      vcode_reg_t value = lower_reify_expr(tree_value(stmt));
      emit_assert(value, message, severity, tree_index(stmt));
   }
}

static void lower_wait(tree_t wait)
{
   vcode_reg_t delay = VCODE_INVALID_REG;
   if (tree_has_delay(wait))
      delay = lower_expr(tree_delay(wait), EXPR_RVALUE);

   vcode_block_t resume = emit_block();
   emit_wait(resume, delay);

   vcode_select_block(resume);
}

static void lower_var_assign(tree_t stmt)
{
   vcode_reg_t value = lower_expr(tree_value(stmt), EXPR_RVALUE);

   tree_t target = tree_target(stmt);
   type_t type = tree_type(target);
   const bool can_use_store =
      type_is_scalar(type)
      || (type_is_array(type) || lower_const_bounds(type));

   if (can_use_store) {
      vcode_reg_t loaded_value = lower_reify(value);
      emit_bounds(loaded_value, lower_bounds(type));
      if (tree_kind(target) == T_REF)
         emit_store(loaded_value, lower_get_var(tree_ref(target)));
      else
         emit_store_indirect(loaded_value, lower_expr(target, EXPR_LVALUE));
   }
   else {
      vcode_dump();
      assert(false);
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
         assert(false);

      vcode_reg_t after;
      if (tree_has_delay(w))
         after = lower_expr(tree_delay(w), EXPR_RVALUE);
      else
         after = emit_const(vtype_int(INT64_MIN, INT64_MAX), 0);

      emit_sched_waveform(nets, emit_const(vtype_offset(), 1),
                          rhs, reject, after);
   }
}

static void lower_if(tree_t stmt)
{
   vcode_reg_t value = lower_reify_expr(tree_value(stmt));

   const int nelses = tree_else_stmts(stmt);

   vcode_block_t btrue = emit_block();
   vcode_block_t bfalse = nelses > 0 ? emit_block() : VCODE_INVALID_BLOCK;
   vcode_block_t bmerge = emit_block();

   emit_cond(value, btrue, nelses > 0 ? bfalse : bmerge);

   vcode_select_block(btrue);

   const int nstmts = tree_stmts(stmt);
   for (int i = 0; i < nstmts; i++)
      lower_stmt(tree_stmt(stmt, i));

   emit_jump(bmerge);

   if (nelses > 0) {
      vcode_select_block(bfalse);

      for (int i = 0; i < nelses; i++)
         lower_stmt(tree_else_stmt(stmt, i));

      emit_jump(bmerge);
   }

   vcode_select_block(bmerge);
}

static void lower_return(tree_t stmt)
{
   if (tree_has_value(stmt)) {
      tree_t value = tree_value(stmt);
      vcode_reg_t result = lower_reify_expr(value);

      type_t type = tree_type(value);
      if (type_is_scalar(type))
         emit_bounds(result, lower_bounds(type));

      emit_return(result);
   }
   else
      emit_return(VCODE_INVALID_REG);
}

static void lower_stmt(tree_t stmt)
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
      lower_if(stmt);
      break;
   case T_RETURN:
      lower_return(stmt);
      break;
   default:
      fatal_at(tree_loc(stmt), "cannot lower statement kind %s",
               tree_kind_str(tree_kind(stmt)));
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

static void lower_decl(tree_t decl)
{
   switch (tree_kind(decl)) {
   case T_VAR_DECL:
      {
         type_t type = tree_type(decl);
         vcode_type_t vtype = lower_type(type);
         vcode_type_t vbounds = lower_bounds(type);
         vcode_var_t var = emit_var(vtype, vbounds, tree_ident(decl));
         tree_add_attr_int(decl, vcode_obj_i, var);

         if (tree_has_value(decl)) {
            vcode_reg_t value = lower_expr(tree_value(decl), EXPR_RVALUE);
            emit_bounds(value, vbounds);
            emit_store(value, var);
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
            shadow = emit_var(vtype_pointer(ltype), bounds,
                              ident_prefix(ident_new("shadow"), name, '_'));
            // TODO: init shadow
         }

         const int nnets = tree_nets(decl);
         netid_t *nets = xmalloc(sizeof(netid_t) * nnets);
         for (int i = 0; i < nnets; i++)
            nets[i] = tree_net(decl, i);

         vcode_type_t stype = vtype_signal(ltype);
         vcode_signal_t sig = emit_signal(stype, bounds, name, shadow,
                                          nets, nnets);
         tree_add_attr_int(decl, vcode_obj_i, sig);
      }
      break;
   case T_TYPE_DECL:
   case T_HIER:
   case T_CONST_DECL:
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
   const int ndecls = tree_decls(scope);
   for (int i = 0; i < ndecls; i++) {
      tree_t d = tree_decl(scope, i);
      const tree_kind_t kind = tree_kind(d);
      switch (kind) {
      case T_FUNC_BODY:
         lower_func_body(d, context);
         break;
      default:
         break;
      }
   }
}

static void lower_func_body(tree_t body, vcode_unit_t context)
{
   vcode_unit_t vu = emit_function(tree_ident(body), context,
                                   lower_type(type_result(tree_type(body))));

   const int nports = tree_ports(body);
   for (int i = 0; i < nports; i++) {
      tree_t p = tree_port(body, i);
      type_t type = tree_type(p);
      vcode_reg_t reg = emit_param(lower_type(type), lower_bounds(type),
                                   tree_ident(p));
      tree_add_attr_int(p, vcode_obj_i, reg);
   }

   lower_decls(body);

   const int nstmts = tree_stmts(body);
   for (int i = 0; i < nstmts; i++)
      lower_stmt(tree_stmt(body, i));

   lower_finished();

   assert(!tree_has_code(body));
   tree_set_code(body, vu);

   lower_cleanup(body);
}

static void lower_process(tree_t proc, vcode_unit_t context)
{
   vcode_unit_t vu = emit_process(tree_ident(proc), context);

   lower_decls(proc);

   emit_return(VCODE_INVALID_REG);

   vcode_block_t start_bb = emit_block();
   vcode_select_block(start_bb);

   const int nstmts = tree_stmts(proc);
   for (int i = 0; i < nstmts; i++)
      lower_stmt(tree_stmt(proc, i));

   if (!vcode_block_finished())
      emit_jump(start_bb);

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

void lower_unit(tree_t unit)
{
   builtin_i   = ident_new("builtin");
   foreign_i   = ident_new("FOREIGN");
   vcode_obj_i = ident_new("vcode_obj");

   verbose = (getenv("NVC_LOWER_VERBOSE") != NULL);

   switch (tree_kind(unit)) {
   case T_ELAB:
      lower_elab(unit);
      break;
   case T_PACK_BODY:
      lower_pack_body(unit);
      break;
   default:
      fatal("cannot lower unit kind %s to vcode",
            tree_kind_str(tree_kind(unit)));
   }

   vcode_close();
}
