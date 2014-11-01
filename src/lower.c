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

static ident_t builtin_i;
static ident_t foreign_i;
static ident_t vcode_var_i;
static bool    verbose = false;

static vcode_reg_t lower_expr(tree_t expr);
static vcode_type_t lower_bounds(type_t type);

static vcode_type_t lower_type(type_t type)
{
   switch (type_kind(type)) {
   case T_SUBTYPE:
      if (type_is_array(type)) {
         const int ndims = type_dims(type);
         assert(ndims > 0);
         vcode_type_t bounds[ndims];
         for (int i = 0; i < ndims; i++) {
            range_t r = type_dim(type, i);
            int64_t low, high;
            range_bounds(r, &low, &high);
            bounds[i] = vtype_int(low, high);
         }

         type_t elem = type_elem(type);
         return vtype_carray(bounds, ndims, lower_type(elem),
                             lower_bounds(elem));
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
      range_t r = type_dim(type, 0);
      return lower_is_const(r.left) && lower_is_const(r.right);
   }
}

static vcode_reg_t lower_reify(vcode_reg_t reg)
{
   if (vtype_kind(vcode_reg_type(reg)) == VCODE_TYPE_POINTER)
      return emit_load_indirect(reg);
   else
      return reg;
}

static vcode_reg_t lower_func_arg(tree_t fcall, int nth)
{
   assert(nth < tree_params(fcall));

   tree_t param = tree_param(fcall, nth);

   assert(tree_subkind(param) == P_POS);
   assert(tree_pos(param) == nth);

   return lower_reify(lower_expr(tree_value(param)));
}

static vcode_reg_t lower_builtin(tree_t fcall, ident_t builtin)
{
   if (icmp(builtin, "eq")) {
      vcode_reg_t r0 = lower_func_arg(fcall, 0);
      vcode_reg_t r1 = lower_func_arg(fcall, 1);
      return emit_cmp(VCODE_CMP_EQ, r0, r1);
   }
   else if (icmp(builtin, "mul")) {
      vcode_reg_t r0 = lower_func_arg(fcall, 0);
      vcode_reg_t r1 = lower_func_arg(fcall, 1);
      return emit_mul(r0, r1);
   }
   else if (icmp(builtin, "add")) {
      vcode_reg_t r0 = lower_func_arg(fcall, 0);
      vcode_reg_t r1 = lower_func_arg(fcall, 1);
      return emit_add(r0, r1);
   }
   else
      fatal_at(tree_loc(fcall), "cannot lower builtin %s", istr(builtin));
}

static vcode_reg_t lower_fcall(tree_t fcall)
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

static vcode_reg_t lower_literal(tree_t lit)
{
   switch (tree_subkind(lit)) {
   case L_INT:
      return emit_const(lower_type(tree_type(lit)), tree_ival(lit));

   default:
      fatal_at(tree_loc(lit), "cannot lower literal kind %d",
               tree_subkind(lit));
   }
}

static vcode_var_t lower_get_var(tree_t decl)
{
   vcode_var_t var = tree_attr_int(decl, vcode_var_i, VCODE_INVALID_VAR);
   if (var == VCODE_INVALID_VAR) {
      vcode_dump();
      fatal_trace("missing vcode var for %s", istr(tree_ident(decl)));
   }

   return var;
}

static vcode_reg_t lower_ref(tree_t ref)
{
   tree_t decl = tree_ref(ref);
   type_t type = tree_type(decl);

   tree_kind_t kind = tree_kind(decl);
   switch (kind) {
   case T_ENUM_LIT:
      return emit_const(lower_type(type), tree_pos(decl));

   case T_VAR_DECL:
      {
         vcode_var_t var = lower_get_var(decl);
         if (type_is_array(type))
            return emit_index(var, emit_const(vtype_offset(), 0));
         else
            return emit_load(var);
      }

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
   if (!lower_const_bounds(type)) {
      assert(false);
#if 0
      assert(array != NULL);

      LLVMValueRef dim_struct = cgen_uarray_dim(array, dim);

      LLVMValueRef dir =
         LLVMBuildExtractValue(builder, dim_struct, 2, "dir");
      LLVMValueRef is_downto =
         LLVMBuildICmp(builder, LLVMIntEQ, dir,
                       llvm_int8(RANGE_DOWNTO), "is_downto");
      LLVMValueRef left =
         LLVMBuildExtractValue(builder, dim_struct, 0, "left");

      LLVMValueRef downto_zero = LLVMBuildSub(builder, left, off, "");
      LLVMValueRef to_zero     = LLVMBuildSub(builder, off, left, "");

      zeroed = LLVMBuildSelect(builder, is_downto, downto_zero, to_zero, "");
#endif
   }
   else {
      range_t r = type_dim(type, dim);
      vcode_reg_t left = lower_expr(r.left);
      if (r.kind == RANGE_TO)
         zeroed = emit_sub(off, left);
      else
         zeroed = emit_sub(left, off);
   }

   return emit_cast(vtype_offset(), zeroed);
}

static vcode_reg_t lower_array_ref(tree_t ref)
{
   tree_t value = tree_value(ref);
   type_t value_type = tree_type(value);

   vcode_reg_t base = lower_expr(value);
   assert(vtype_kind(vcode_reg_type(base)) == VCODE_TYPE_POINTER);

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

      vcode_reg_t offset = lower_expr(tree_value(p));

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
         tmp  = lower_expr(value);
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

static vcode_reg_t lower_aggregate(tree_t expr)
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
      assert(false);
}

static vcode_reg_t lower_expr(tree_t expr)
{
   switch (tree_kind(expr)) {
   case T_FCALL:
      return lower_fcall(expr);
   case T_LITERAL:
      return lower_literal(expr);
   case T_REF:
      return lower_ref(expr);
   case T_AGGREGATE:
      return lower_aggregate(expr);
   case T_ARRAY_REF:
      return lower_array_ref(expr);
   default:
      fatal_at(tree_loc(expr), "cannot lower expression kind %s",
               tree_kind_str(tree_kind(expr)));
   }
}

static void lower_assert(tree_t stmt)
{
   emit_assert(lower_expr(tree_value(stmt)));
}

static void lower_wait(tree_t wait)
{
   vcode_reg_t delay = VCODE_INVALID_REG;
   if (tree_has_delay(wait))
      delay = lower_expr(tree_delay(wait));

   vcode_block_t resume = emit_block();
   emit_wait(resume, delay);

   vcode_select_block(resume);
}

static void lower_var_assign(tree_t stmt)
{
   vcode_reg_t value = lower_expr(tree_value(stmt));

   tree_t target = tree_target(stmt);
   type_t type = tree_type(target);
   if (type_is_scalar(type)) {
      vcode_reg_t loaded_value = lower_reify(value);
      emit_bounds(loaded_value, lower_bounds(type));
      if (tree_kind(target) == T_REF)
         emit_store(loaded_value, lower_get_var(tree_ref(target)));
      else
         emit_store_indirect(loaded_value, lower_expr(target));
   }
   else
      assert(false);
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
   default:
      fatal_at(tree_loc(stmt), "cannot lower statement kind %s",
               tree_kind_str(tree_kind(stmt)));
   }
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
         tree_add_attr_int(decl, vcode_var_i, var);

         if (tree_has_value(decl)) {
            vcode_reg_t value = lower_expr(tree_value(decl));
            emit_bounds(value, vbounds);
            emit_store(value, var);
         }
      }
      break;
   case T_SIGNAL_DECL:
      {
         type_t type = tree_type(decl);
         vcode_type_t stype = lower_type(type);
         vcode_type_t sbounds = lower_bounds(type);
         vcode_signal_t sig = emit_signal(stype, sbounds, tree_ident(decl));
         (void)sig;
         //tree_add_attr_int(decl, vcode_var_i, var);
      }
      break;
   case T_TYPE_DECL:
   case T_HIER:
      break;
   default:
      fatal_at(tree_loc(decl), "cannot lower decl kind %s",
               tree_kind_str(tree_kind(decl)));
   }
}

static void lower_process(tree_t proc)
{
   vcode_unit_t vu = emit_process(tree_ident(proc));

   const int ndecls = tree_decls(proc);
   for (int i = 0; i < ndecls; i++)
      lower_decl(tree_decl(proc, i));

   vcode_block_t start_bb = emit_block();
   emit_jump(start_bb);
   vcode_select_block(start_bb);

   const int nstmts = tree_stmts(proc);
   for (int i = 0; i < nstmts; i++)
      lower_stmt(tree_stmt(proc, i));

   if (!vcode_block_finished())
      emit_jump(start_bb);

   vcode_opt();

   if (verbose)
      vcode_dump();

   assert(!tree_has_code(proc));
   tree_set_code(proc, vu);

   for (int i = 0; i < ndecls; i++)
      tree_remove_attr(tree_decl(proc, i), vcode_var_i);
}

static void lower_elab(tree_t unit)
{
   vcode_unit_t context = emit_context(tree_ident(unit));
   tree_set_code(unit, context);

   const int ndecls = tree_decls(unit);
   for (int i = 0; i < ndecls; i++)
      lower_decl(tree_decl(unit, i));

   emit_return(VCODE_INVALID_REG);

   vcode_opt();

   if (verbose)
      vcode_dump();

   const int nstmts = tree_stmts(unit);
   for (int i = 0; i < nstmts; i++) {
      tree_t s = tree_stmt(unit, i);
      assert(tree_kind(s) == T_PROCESS);
      lower_process(s);
   }
}

void lower_unit(tree_t unit)
{
   builtin_i   = ident_new("builtin");
   foreign_i   = ident_new("FOREIGN");
   vcode_var_i = ident_new("vcode_var");

   verbose = (getenv("NVC_LOWER_VERBOSE") != NULL);

   switch (tree_kind(unit)) {
   case T_ELAB:
      lower_elab(unit);
      break;
   default:
      fatal("cannot lower to level unit kind %s to vcode",
            tree_kind_str(tree_kind(unit)));
   }

   vcode_close();
}
