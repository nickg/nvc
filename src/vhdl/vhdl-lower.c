//
//  Copyright (C) 2014-2026  Nick Gasson
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
#include "mir/mir-unit.h"
#include "tree.h"
#include "type.h"
#include "vhdl/vhdl-lower.h"
#include "vhdl/vhdl-util.h"

#include <assert.h>

static void fill_array_type_info(vhdl_gen_t *g, type_t type, type_info_t *ti)
{
   ti->ndims = dimension_of(type);

   for (type_t t = type_base_recur(type); type_is_array(t);
        t = type_elem(t))
      ti->udims += dimension_of(t);

   const type_info_t *elem = type_info(g, type_elem(type));
   const type_info_t *base = type_info(g, type_elem_recur(type));

   if (elem->size != SIZE_MAX && elem->stride != SIZE_MAX)
      ti->stride = elem->size * elem->stride;
   else
      ti->stride = SIZE_MAX;

   if (type_const_bounds(type)) {
      for (int i = 0; i < ti->ndims; i++) {
         tree_t r = range_of(type, i);
         int64_t low, high;
         range_bounds(r, &low, &high);
         ti->size *= MAX(high - low + 1, 0);
      }

      ti->type = mir_carray_type(g->mu, ti->size * ti->stride, base->type);
      ti->bounds = mir_uarray_type(g->mu, ti->udims, base->type);
   }
   else {
      ti->type = ti->bounds = mir_uarray_type(g->mu, ti->udims, base->type);
      ti->size = SIZE_MAX;
   }

   ti->stamp = elem->stamp;
}

const type_info_t *type_info(vhdl_gen_t *g, type_t type)
{
   type_info_t *ti = mir_get_priv(g->mu, type);
   if (ti != NULL)
      return ti;

   ti = mir_malloc(g->mu, sizeof(type_info_t));
   ti->tree   = type;
   ti->type   = MIR_NULL_TYPE;
   ti->stamp  = MIR_NULL_STAMP;
   ti->bounds = MIR_NULL_TYPE;
   ti->kind   = type_base_kind(type);
   ti->ndims  = 0;
   ti->udims  = 0;
   ti->size   = 1;
   ti->stride = 1;

   switch (type_kind(type)) {
   case T_SUBTYPE:
      if (type_is_array(type))
         fill_array_type_info(g, type, ti);
      else {
         const type_info_t *base = type_info(g, type_base(type));
         ti->type   = base->type;
         ti->bounds = base->bounds;

         if (type_is_integer(type) || type_is_enum(type)) {
            tree_t r = range_of(type, 0);
            int64_t low, high;
            if (folded_bounds(r, &low, &high))
               ti->stamp = mir_int_stamp(g->mu, low, high);
         }
         else if (type_is_real(type)) {
            tree_t r = range_of(type, 0);
            double low, high;
            if (folded_bounds_real(r, &low, &high))
               ti->stamp = mir_real_stamp(g->mu, low, high);
         }
      }
      break;

   case T_ARRAY:
      fill_array_type_info(g, type, ti);
      break;

   case T_PHYSICAL:
   case T_INTEGER:
      {
         tree_t r = type_dim(type, 0);
         int64_t low, high;
         const bool folded = folded_bounds(r, &low, &high);
         if (folded) {
            ti->type = mir_int_type(g->mu, low, high);
            ti->stamp = mir_int_stamp(g->mu, low, high);
         }
         else
            ti->type = mir_int_type(g->mu, INT64_MIN, INT64_MAX);

         ti->bounds = mir_uarray_type(g->mu, 1, ti->type);
      }
      break;

   case T_ENUM:
      {
         const int nlits = type_enum_literals(type);
         ti->type = mir_int_type(g->mu, 0, nlits - 1);
         ti->bounds = mir_uarray_type(g->mu, 1, ti->type);
         ti->stamp = mir_int_stamp(g->mu, 0, nlits - 1);
      }
      break;

   case T_RECORD:
      {
         const int nfields = type_fields(type);
         mir_type_t *fields = xmalloc_array(nfields, sizeof(mir_type_t));
         for (int i = 0; i < nfields; i++)
            fields[i] = type_info(g, tree_type(type_field(type, i)))->type;

         ti->type = mir_record_type(g->mu, type_ident(type), fields, nfields);
         ti->bounds = ti->type;
      }
      break;

   case T_PROTECTED:
      ti->type = mir_context_type(g->mu, type_ident(type));
      break;

   case T_FILE:
      {
         const type_info_t *base = type_info(g, type_designated(type));
         ti->type = mir_file_type(g->mu, base->type);
      }
      break;

   case T_ACCESS:
      {
         mir_type_t designated = type_info(g, type_designated(type))->type;
         ti->type = mir_access_type(g->mu, designated);
      }
      break;

   case T_REAL:
      {
         tree_t r = type_dim(type, 0);
         double low, high;
         const bool folded = folded_bounds_real(r, &low, &high);
         if (folded) {
            ti->type = mir_real_type(g->mu, low, high);
            ti->stamp = mir_real_stamp(g->mu, low, high);
         }
         else
            ti->type = mir_double_type(g->mu);

         ti->bounds = mir_uarray_type(g->mu, 1, ti->type);
      }
      break;

   case T_INCOMPLETE:
      ti->type = mir_opaque_type(g->mu);
      break;

   default:
      fatal_trace("cannot lower type kind %s", type_kind_str(type_kind(type)));
   }

#ifdef DEBUG
   LOCAL_TEXT_BUF tb = tb_new();
   if (is_anonymous_subtype(type))
      tb_cat(tb, "Anonymous subtype ");
   tb_printf(tb, "%s %s", type_pp(type), type_kind_str(ti->kind));

   if (ti->kind == T_ARRAY) {
      tb_printf(tb, " ndims:%d udims:%d", ti->ndims, ti->udims);
      if (ti->size == SIZE_MAX)
         tb_cat(tb, " size:*");
      else
         tb_printf(tb, " size:%zd", ti->size);
      if (ti->stride == SIZE_MAX)
         tb_cat(tb, " stride:*");
      else
         tb_printf(tb, " stride:%zd", ti->stride);
   }

   mir_comment(g->mu, "%s", tb_get(tb));
#endif

   mir_put_priv(g->mu, type, ti);
   return ti;
}

static mir_value_t get_bounds_var(vhdl_gen_t *g, const type_info_t *ti)
{
   assert(!is_anonymous_subtype(ti->tree));

   int hops;
   mir_value_t var = mir_search_object(g->mu, ti->tree, &hops);
   if (mir_is_null(var)) {
      ident_t name = type_ident(ti->tree), it = name;
      ident_t prefix = ident_walk_selected(&it);
      ident_t next = ident_walk_selected(&it);
      mir_value_t pkg = MIR_NULL_VALUE;
      for (; it != NULL; next = ident_walk_selected(&it)) {
         ident_t uname = ident_prefix(prefix, next, '.');
         pkg = mir_build_link_package(g->mu, uname);
         prefix = uname;
      }

      mir_value_t ptr = mir_build_link_var(g->mu, pkg, name, ti->bounds);
      return mir_build_load(g->mu, ptr);
   }
   else {
      assert(var.tag == MIR_TAG_VAR);
      assert(hops == 0);

      return mir_build_load(g->mu, var);
   }
}

static mir_type_t get_func_result_type(vhdl_gen_t *g, type_t result)
{
   const type_info_t *ti = type_info(g, result);

   switch (ti->kind) {
   case T_ARRAY:
      if (mir_get_class(g->mu, ti->type) == MIR_TYPE_CARRAY)
         return mir_pointer_type(g->mu, mir_get_elem(g->mu, ti->type));
      else
         return ti->type;
   case T_RECORD:
      return mir_pointer_type(g->mu, ti->type);
   default:
      return ti->type;
   }
}

static mir_type_t get_param_type(vhdl_gen_t *g, const type_info_t *ti,
                                 class_t class, port_mode_t mode)
{
   switch (class) {
   case C_SIGNAL:
      assert(false);  // TODO

   case C_VARIABLE:
   case C_DEFAULT:
   case C_CONSTANT:
      if (ti->kind == T_ARRAY) {
         if (mir_get_class(g->mu, ti->type) == MIR_TYPE_CARRAY)
            return mir_pointer_type(g->mu, mir_get_elem(g->mu, ti->type));
         else
            return ti->type;
      }
      else if (ti->kind == T_RECORD)
         return mir_pointer_type(g->mu, ti->type);
      else if (mode == PORT_OUT || mode == PORT_INOUT)
         return mir_pointer_type(g->mu, ti->type);
      else
         return ti->type;

   case C_FILE:
      return mir_pointer_type(g->mu, ti->type);

   default:
      should_not_reach_here();
   }
}

static void check_scalar_bounds(vhdl_gen_t *g, const type_info_t *ti,
                                mir_value_t value, tree_t where, tree_t hint)
{
   tree_t r = range_of(ti->tree, 0);

   mir_value_t left, right, dir;
   int64_t low, high;
   if (folded_bounds(r, &low, &high)) {
      if (mir_within_int(g->mu, value, low, high)) {
         mir_comment(g->mu, "Elided scalar bounds check");
         return;
      }
      else {
         left = mir_const(g->mu, ti->type, low);
         right = mir_const(g->mu, ti->type, high);
         dir = mir_const(g->mu, mir_bool_type(g->mu), RANGE_TO);
      }
   }
   else {
      mir_value_t bounds = get_bounds_var(g, ti);
      left = mir_build_uarray_left(g->mu, bounds, 0);
      right = mir_build_uarray_right(g->mu, bounds, 0);
      dir = mir_build_uarray_dir(g->mu, bounds, 0);
   }

   mir_value_t locus = mir_build_debug_locus(g->mu, tree_to_object(where));

   mir_value_t hint_locus = locus;
   if (hint != NULL && hint != where)
      hint_locus = mir_build_debug_locus(g->mu, tree_to_object(hint));

   mir_build_range_check(g->mu, value, left, right, dir, locus, hint_locus);
}

mir_value_t vhdl_lower_wrap(vhdl_gen_t *g, const type_info_t *ti,
                            mir_value_t data)
{
   assert(mir_is(g->mu, data, MIR_TYPE_POINTER));
   assert(ti->size < SIZE_MAX);
   assert(type_const_bounds(ti->tree));

   mir_type_t t_bool = mir_bool_type(g->mu);

   if (ti->udims == 1) {
      tree_t r = range_of(ti->tree, 0);

      mir_dim_t dims[1];
      dims[0].left  = vhdl_lower_rvalue(g, tree_left(r));
      dims[0].right = vhdl_lower_rvalue(g, tree_right(r));
      dims[0].dir   = mir_const(g->mu, t_bool, tree_subkind(r));

      return mir_build_wrap(g->mu, data, dims, 1);
   }
   else {
      mir_dim_t *dims LOCAL = xmalloc_array(ti->udims, sizeof(mir_dim_t));

      for (int i = 0; i < ti->udims; i++) {
         tree_t r = range_of(ti->tree, i);

         dims[i].left = vhdl_lower_rvalue(g, tree_left(r));
         dims[i].right = vhdl_lower_rvalue(g, tree_right(r));
         dims[i].dir   = mir_const(g->mu, t_bool, tree_subkind(r));
      }

      return mir_build_wrap(g->mu, data, dims, ti->udims);
   }

   return data;
}

mir_value_t vhdl_lower_array_stride(vhdl_gen_t *g, const type_info_t *ti,
                                    mir_value_t array)
{
   assert(ti->kind == T_ARRAY);

   mir_type_t t_offset = mir_offset_type(g->mu);

   if (ti->stride < SIZE_MAX)
      return mir_const(g->mu, t_offset, ti->stride);

   assert(mir_is(g->mu, array, MIR_TYPE_UARRAY));

   int pos = ti->ndims;
   mir_value_t stride = mir_const(g->mu, t_offset, 1);

   for (const type_info_t *elem = type_info(g, type_elem(ti->tree));
        elem->kind == T_ARRAY;
        elem = type_info(g, type_elem(elem->tree))) {

      if (elem->size < SIZE_MAX) {
         mir_value_t size = mir_const(g->mu, t_offset, elem->size);
         stride = mir_build_mul(g->mu, t_offset, stride, size);
         break;
      }

      mir_value_t length = mir_build_uarray_len(g->mu, array, pos++);
      for (int i = 1; i < elem->ndims; i++) {
         mir_value_t dim_len = mir_build_uarray_len(g->mu, array, pos++);
         length = mir_build_mul(g->mu, t_offset, dim_len, length);
      }

      stride = mir_build_mul(g->mu, t_offset, stride, length);
   }

   assert(pos == ti->udims);
   return stride;
}

#if 0
mir_value_t lower_total_elements(mir_unit_t *mu, const type_info_t *ti,
                                 mir_value_t array)
{
   mir_type_t t_offset = mir_offset_type(mu);

   if (ti->size < SIZE_MAX && ti->stride < SIZE_MAX)
      return mir_const(mu, t_offset, ti->size * ti->stride);

   mir_value_t total = lower_array_length(mu, ti, array);
   mir_value_t stride = lower_array_stride(mu, ti, array);

   return mir_build_mul(mu, t_offset, total, stride);
}
#endif

static mir_value_t vhdl_lower_subprogram_arg(vhdl_gen_t *g, tree_t t, int nth,
                                             mir_value_t context)
{
   if (nth >= tree_params(t))
      return MIR_NULL_VALUE;

   tree_t param = tree_param(t, nth);

   assert(tree_subkind(param) == P_POS);
   assert(tree_pos(param) == nth);

   tree_t value = tree_value(param);
   tree_t decl = tree_ref(t);
   tree_t port = tree_port(decl, nth);

   type_t value_type = tree_type(value);
   type_t port_type = tree_type(port);

   const class_t class = tree_class(port);
   const port_mode_t mode = tree_subkind(port);

   mir_value_t reg;
   if (class == C_SIGNAL && tree_kind(value) == T_AGGREGATE)
      assert(false);  // TODO
   else if (class == C_SIGNAL || class == C_FILE || mode != PORT_IN)
      assert(false);  // TODO
   else if (tree_kind(value) == T_OPEN)
      assert(false);  // TODO
   else
      reg = vhdl_lower_rvalue(g, value);

   if (type_is_array(value_type))
      assert(false);  // TODO
   else if (class == C_SIGNAL || class == C_FILE)
      return reg;
   else if (mode == PORT_OUT || !type_is_scalar(port_type))
      return reg;
   else if (mode == PORT_INOUT)
      assert(false);  // TODO
   else {
      check_scalar_bounds(g, type_info(g, port_type), reg, value, port);
      return reg;
   }
}

static mir_value_t vhdl_lower_default(vhdl_gen_t *g, const type_info_t *ti)
{
   switch (ti->kind) {
   case T_INTEGER:
      {
         tree_t left = tree_left(range_of(ti->tree, 0));
         int64_t ival;
         if (folded_int(left, &ival))
            return mir_const(g->mu, ti->type, ival);
         else
            should_not_reach_here();
      }
   default:
      should_not_reach_here();
   }
}

static mir_value_t vhdl_lower_arith(vhdl_gen_t *g, tree_t t,
                                    subprogram_kind_t kind,
                                    mir_value_t arg0, mir_value_t arg1)
{
   const type_info_t *ti = type_info(g, tree_type(t));

   switch (kind) {
   case S_ADD:
      if (ti->kind == T_INTEGER) {
         mir_value_t locus = mir_build_debug_locus(g->mu, tree_to_object(t));
         return mir_build_trap_add(g->mu, ti->type, arg0, arg1, locus);
      }
      else
         return mir_build_add(g->mu, ti->type, arg0, arg1);
   case S_MUL:
      if (ti->kind == T_INTEGER) {
         mir_value_t locus = mir_build_debug_locus(g->mu, tree_to_object(t));
         return mir_build_trap_mul(g->mu, ti->type, arg0, arg1, locus);
      }
      else
         return mir_build_mul(g->mu, ti->type, arg0, arg1);
   case S_SUB:
      if (ti->kind == T_INTEGER) {
         mir_value_t locus = mir_build_debug_locus(g->mu, tree_to_object(t));
         return mir_build_trap_sub(g->mu, ti->type, arg0, arg1, locus);
      }
      else
         return mir_build_sub(g->mu, ti->type, arg0, arg1);
   case S_MOD:
      return mir_build_mod(g->mu, ti->type, arg0, arg1);
   case S_REM:
      return mir_build_rem(g->mu, ti->type, arg0, arg1);
   case S_DIV:
      return mir_build_div(g->mu, ti->type, arg0, arg1);
   case S_EXP:
      {
         mir_value_t locus = mir_build_debug_locus(g->mu, tree_to_object(t));
         return mir_build_trap_exp(g->mu, ti->type, arg0, arg1, locus);
      }
   default:
      should_not_reach_here();
   }
}

static mir_value_t vhdl_lower_builtin(vhdl_gen_t *g, tree_t t,
                                      subprogram_kind_t builtin,
                                      mir_value_t out[2])
{
   if (vhdl_is_short_circuit(builtin))
      assert(false);

   mir_value_t arg0 = vhdl_lower_subprogram_arg(g, t, 0, MIR_NULL_VALUE);
   mir_value_t arg1 = vhdl_lower_subprogram_arg(g, t, 1, MIR_NULL_VALUE);

   if (out != NULL) {
      out[0] = arg0;
      out[1] = arg1;
   }

   switch (builtin) {
   case S_MUL:
   case S_ADD:
   case S_SUB:
      return vhdl_lower_arith(g, t, builtin, arg0, arg1);
   default:
      CANNOT_HANDLE(t);
   }
}

static mir_value_t vhdl_lower_literal(vhdl_gen_t *g, tree_t t)
{
   const type_info_t *ti = type_info(g, tree_type(t));

   switch (tree_subkind(t)) {
   case L_PHYSICAL:
      assert(!tree_has_ref(t));
      // Fall-through
   case L_INT:
      return mir_const(g->mu, ti->type, tree_ival(t));
   case L_NULL:
      return mir_build_null(g->mu, ti->type);
   case L_REAL:
      return mir_const_real(g->mu, ti->type, tree_dval(t));
   default:
      should_not_reach_here();
   }
}

static mir_value_t vhdl_lower_context_for_call(vhdl_gen_t *g, tree_t t)
{
   if (tree_flags(t) & TREE_F_PREDEFINED)
      return MIR_NULL_VALUE;

   int hops;
   mir_value_t context = mir_search_object(g->mu, t, &hops);
   if (mir_is_null(context))  // TODO
      return mir_build_null(g->mu, mir_context_type(g->mu, tree_ident2(t)));

   assert(hops == 0);

   return context;
}

static mir_value_t vhdl_lower_fcall(vhdl_gen_t *g, tree_t t)
{
   tree_t decl = tree_ref(t);

   const subprogram_kind_t kind = tree_subkind(decl);
   if (is_open_coded_builtin(kind))
      return vhdl_lower_builtin(g, t, kind, NULL);

   const type_info_t *ti = type_info(g, type_result(tree_type(decl)));

   const int nparams = tree_params(t);
   mir_value_t *args LOCAL = xmalloc_array(nparams + 2, sizeof(mir_value_t));
   int nargs = 0;

   mir_value_t context = vhdl_lower_context_for_call(g, decl);
   if (!mir_is_null(context))
      args[nargs++] = context;

   for (int i = 0; i < nparams; i++)
      args[nargs++] = vhdl_lower_rvalue(g, tree_value(tree_param(t, i)));

   assert(nargs <= nparams + 2);

   return mir_build_fcall(g->mu, tree_ident2(decl), ti->type, ti->stamp,
                          args, nargs);
}

static mir_value_t vhdl_lower_rvalue_ref(vhdl_gen_t *g, tree_t t)
{
   tree_t decl = tree_ref(t);

   int hops = 0;
   mir_value_t value = mir_search_object(g->mu, decl, &hops);
   if (mir_is_null(value)) {
      mir_dump(g->mu);
      fatal_trace("missing value for %pI", tree_ident(decl));
   }

   switch (tree_kind(decl)) {
   case T_PARAM_DECL:
      assert(hops == 0);
      assert(value.tag == MIR_TAG_PARAM);
      return value;
   case T_VAR_DECL:
      assert(hops == 0);
      assert(value.tag == MIR_TAG_VAR);
      return mir_build_load(g->mu, value);
   default:
      CANNOT_HANDLE(decl);
   }
}

mir_value_t vhdl_lower_rvalue(vhdl_gen_t *g, tree_t t)
{
   switch (tree_kind(t)) {
   case T_REF:
      return vhdl_lower_rvalue_ref(g, t);
   case T_LITERAL:
      return vhdl_lower_literal(g, t);
   case T_FCALL:
      return vhdl_lower_fcall(g, t);
   default:
      CANNOT_HANDLE(t);
   }
}

static mir_value_t vhdl_lower_lvalue_ref(vhdl_gen_t *g, tree_t t)
{
   tree_t decl = tree_ref(t);

   int hops = 0;
   mir_value_t value = mir_search_object(g->mu, decl, &hops);
   if (mir_is_null(value)) {
      mir_dump(g->mu);
      fatal_trace("missing value for %pI", tree_ident(decl));
   }

   switch (tree_kind(decl)) {
   case T_VAR_DECL:
      assert(hops == 0);
      assert(value.tag == MIR_TAG_VAR);
      return value;
   default:
      CANNOT_HANDLE(decl);
   }
}

static mir_value_t vhdl_lower_lvalue(vhdl_gen_t *g, tree_t t)
{
   switch (tree_kind(t)) {
   case T_REF:
      return vhdl_lower_lvalue_ref(g, t);
   default:
      CANNOT_HANDLE(t);
   }
}

static void vhdl_lower_const_decl(vhdl_gen_t *g, tree_t t)
{
   mir_comment(g->mu, "Constant declaration %pI", tree_ident(t));

   const type_info_t *ti = type_info(g, tree_type(t));

   mir_value_t var = mir_add_var(g->mu, ti->type, ti->stamp,
                                 tree_ident(t), MIR_VAR_CONST);

   mir_value_t init = vhdl_lower_rvalue(g, tree_value(t));
   if (type_is_scalar(ti->tree))
      check_scalar_bounds(g, ti, init, t, t);

   mir_build_store(g->mu, var, init);

   mir_put_object(g->mu, t, var);
}

static void vhdl_lower_var_decl(vhdl_gen_t *g, tree_t t)
{
   mir_comment(g->mu, "Variable declaration %pI", tree_ident(t));

   const type_info_t *ti = type_info(g, tree_type(t));

   mir_value_t var = mir_add_var(g->mu, ti->type, ti->stamp, tree_ident(t), 0);

   if (tree_has_value(t)) {
      mir_value_t init = vhdl_lower_rvalue(g, tree_value(t));
      if (type_is_scalar(ti->tree))
         check_scalar_bounds(g, ti, init, t, t);

      mir_build_store(g->mu, var, init);
   }
   else {
      mir_value_t def = vhdl_lower_default(g, ti);
      mir_build_store(g->mu, var, def);
   }

   mir_put_object(g->mu, t, var);
}

static void vhdl_lower_subtype_decl(vhdl_gen_t *g, tree_t t)
{
   mir_comment(g->mu, "Subtype declaration %pI", tree_ident(t));

   const type_info_t *ti = type_info(g, tree_type(t));

   if (type_is_scalar(ti->tree)) {
      mir_type_t t_bool = mir_bool_type(g->mu);
      mir_type_t t_ptr = mir_pointer_type(g->mu, ti->type);

      tree_t r = range_of(ti->tree, 0);
      mir_value_t left = vhdl_lower_rvalue(g, tree_left(r));
      mir_value_t right = vhdl_lower_rvalue(g, tree_right(r));
      mir_value_t dir = mir_const(g->mu, t_bool, tree_subkind(r));

      mir_dim_t dims[1] = {
         { left, right, dir },
      };

      mir_value_t null = mir_build_null(g->mu, t_ptr);
      mir_value_t bounds = mir_build_wrap(g->mu, null, dims, 1);

      mir_value_t var = mir_add_var(g->mu, ti->bounds, ti->stamp,
                                    type_ident(ti->tree), MIR_VAR_CONST);
      mir_build_store(g->mu, var, bounds);

      mir_put_object(g->mu, ti->tree, var);
   }
   else if (type_is_array(ti->tree)) {
      mir_type_t t_ptr = mir_pointer_type(g->mu, mir_get_elem(g->mu, ti->type));
      mir_value_t null = mir_build_null(g->mu, t_ptr);

      mir_type_t t_bool = mir_bool_type(g->mu);

      mir_dim_t *dims LOCAL = xmalloc_array(ti->udims, sizeof(mir_dim_t));

      for (int i = 0; i < ti->udims; i++) {
         tree_t r = range_of(ti->tree, i);

         dims[i].left  = vhdl_lower_rvalue(g, tree_left(r));
         dims[i].right = vhdl_lower_rvalue(g, tree_right(r));
         dims[i].dir   = mir_const(g->mu, t_bool, tree_subkind(r));
      }

      mir_value_t bounds = mir_build_wrap(g->mu, null, dims, ti->udims);

      mir_value_t var = mir_add_var(g->mu, ti->bounds, ti->stamp,
                                    type_ident(ti->tree), MIR_VAR_CONST);
      mir_build_store(g->mu, var, bounds);

      mir_put_object(g->mu, ti->tree, var);
   }
   else
      should_not_reach_here();
}

static void vhdl_lower_type_decl(vhdl_gen_t *g, tree_t t)
{

}

static void vhdl_lower_decls(vhdl_gen_t *g, tree_t t)
{
   const int ndecls = tree_decls(t);
   for (int i = 0; i < ndecls; i++) {
      tree_t d = tree_decl(t, i);
      switch (tree_kind(d)) {
      case T_FUNC_DECL:
         break;
      case T_TYPE_DECL:
         vhdl_lower_type_decl(g, d);
         break;
      case T_SUBTYPE_DECL:
         vhdl_lower_subtype_decl(g, d);
         break;
      case T_CONST_DECL:
         vhdl_lower_const_decl(g, d);
         break;
      case T_VAR_DECL:
         vhdl_lower_var_decl(g, d);
         break;
      case T_ATTR_DECL:
      case T_ATTR_SPEC:
         break;
      default:
         CANNOT_HANDLE(d);
      }
   }
}

static void vhdl_lower_return(vhdl_gen_t *g, tree_t t)
{
   mir_value_t result = MIR_NULL_VALUE;
   if (tree_has_value(t)) {
      tree_t value = tree_value(t);
      const type_info_t *ti = type_info(g, tree_type(value));

      result = vhdl_lower_rvalue(g, value);

      if (type_is_scalar(ti->tree))
         check_scalar_bounds(g, ti, result, value, value);
      else if (type_is_access(ti->tree))
         should_not_reach_here();
      else if (type_is_array(ti->tree))
         should_not_reach_here();
   }

   mir_build_return(g->mu, result);
}

static void vhdl_lower_var_assign(vhdl_gen_t *g, tree_t t)
{
   tree_t value  = tree_value(t);
   tree_t target = tree_target(t);
   type_t type   = tree_type(target);

   // TODO: statement coverage

   assert(type_is_scalar(type));

   mir_value_t rvalue = vhdl_lower_rvalue(g, value);
   mir_value_t lvalue = vhdl_lower_lvalue(g, target);

   const type_info_t *ti = type_info(g, type);
   check_scalar_bounds(g, ti, rvalue, value, target);

   mir_build_store(g->mu, lvalue, rvalue);
}

static void vhdl_lower_wait(vhdl_gen_t *g, tree_t t)
{
   const bool is_static = !!(tree_flags(t) & TREE_F_STATIC_WAIT);
   assert(!is_static || (!tree_has_delay(t) && !tree_has_value(t)));

   const int ntriggers = tree_triggers(t);
   assert(ntriggers == 0);

   mir_block_t resume_bb = mir_add_block(g->mu);
   mir_build_wait(g->mu, resume_bb);

   mir_set_cursor(g->mu, resume_bb, MIR_APPEND);
}

static void vhdl_lower_stmts(vhdl_gen_t *g, tree_t t)
{
   const int nstmts = tree_stmts(t);
   for (int i = 0; i < nstmts; i++) {
      tree_t s = tree_stmt(t, i);
      switch (tree_kind(s)) {
      case T_RETURN:
         vhdl_lower_return(g, s);
         break;
      case T_VAR_ASSIGN:
         vhdl_lower_var_assign(g, s);
         break;
      case T_WAIT:
         vhdl_lower_wait(g, s);
         break;
      default:
         CANNOT_HANDLE(s);
      }
   }
}

static void vhdl_lower_subprogram_ports(vhdl_gen_t *g, tree_t t,
                                        bool params_as_vars)
{
   const int nports = tree_ports(t);
   for (int i = 0; i < nports; i++) {
      tree_t p = tree_port(t, i);

      const class_t class = tree_class(p);
      const port_mode_t mode = tree_subkind(p);

      const type_info_t *ti = type_info(g, tree_type(p));
      mir_type_t type = get_param_type(g, ti, class, mode);

      mir_value_t preg = mir_add_param(g->mu, type, ti->stamp, tree_ident(p));
      if (params_as_vars)
         assert(false);
      else
         mir_put_object(g->mu, p, preg);
   }
}

static void vhdl_lower_pack_body(vhdl_gen_t *g, tree_t t)
{
   tree_t pack = tree_primary(t);
   assert(!is_uninstantiated_package(pack));

   vhdl_lower_decls(g, pack);

   mir_build_return(g->mu, MIR_NULL_VALUE);
}

static void vhdl_lower_package(vhdl_gen_t *g, tree_t t)
{
   assert(!is_uninstantiated_package(t));
   assert(!package_needs_body(t));

   vhdl_lower_decls(g, t);

   mir_build_return(g->mu, MIR_NULL_VALUE);
}

static void vhdl_lower_func_body(vhdl_gen_t *g, tree_t t)
{
   type_t result = type_result(tree_type(t));
   mir_set_result(g->mu, get_func_result_type(g, result));

   mir_type_t t_context = mir_context_type(g->mu, mir_get_parent(g->mu));
   mir_add_param(g->mu, t_context, MIR_NULL_STAMP, ident_new("context"));

   const bool has_subprograms = false;
   vhdl_lower_subprogram_ports(g, t, has_subprograms);

   vhdl_lower_decls(g, t);
   vhdl_lower_stmts(g, t);

   if (!mir_block_finished(g->mu, MIR_NULL_BLOCK)) {
      mir_value_t locus = mir_build_debug_locus(g->mu, tree_to_object(t));
      mir_build_unreachable(g->mu, locus);
   }
}

static void vhdl_lower_process(vhdl_gen_t *g, tree_t t)
{
   // The code generator assumes the first state starts at block number
   // one. Allocate it here in case lowering the declarations generates
   // additional basic blocks.
   mir_block_t start_bb = mir_add_block(g->mu);
   assert(start_bb.tag == 1);

   vhdl_lower_decls(g, t);

   mir_build_return(g->mu, MIR_NULL_VALUE);

   mir_set_cursor(g->mu, start_bb, MIR_APPEND);

   vhdl_lower_stmts(g, t);

   if (!mir_block_finished(g->mu, MIR_NULL_BLOCK))
      mir_build_jump(g->mu, start_bb);
}

static void vhdl_lower_cleanup(vhdl_gen_t *g)
{

}

void vhdl_lower_deferred(mir_unit_t *mu, object_t *obj)
{
   tree_t t = tree_from_object(obj);
   assert(t != NULL);

   vhdl_gen_t g = {
      .mu = mu,
   };

   switch (tree_kind(t)) {
   case T_PACKAGE:
      vhdl_lower_package(&g, t);
      break;
   case T_PACK_BODY:
      vhdl_lower_pack_body(&g, t);
      break;
   case T_FUNC_BODY:
      vhdl_lower_func_body(&g, t);
      break;
   case T_PROCESS:
      vhdl_lower_process(&g, t);
      break;
   default:
      CANNOT_HANDLE(t);
   }

   mir_optimise(mu, MIR_PASS_O1);

   vhdl_lower_cleanup(&g);
}
