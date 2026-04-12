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
#include "mir/mir-unit.h"
#include "tree.h"
#include "type.h"
#include "vhdl/vhdl-lower.h"
#include "vhdl/vhdl-util.h"

#include <assert.h>

static void fill_array_type_info(mir_unit_t *mu, type_t type, type_info_t *ti)
{
   ti->ndims = ti->udims = dimension_of(type);

   for (type_t e = type_elem(type);
        type_is_array(e) && !type_const_bounds(e);
        e = type_elem(e))
      ti->udims += dimension_of(e);  // XXX: equal to elem->udims?

   const type_info_t *elem = type_info(mu, type_elem(type));
   const type_info_t *base = type_info(mu, type_elem_recur(type));

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

      ti->type = mir_carray_type(mu, ti->size * ti->stride, base->type);
   }
   else {
      ti->type = mir_uarray_type(mu, ti->udims, base->type);
      ti->size = SIZE_MAX;
   }

   ti->stamp = elem->stamp;
}

const type_info_t *type_info(mir_unit_t *mu, type_t type)
{
   type_info_t *ti = mir_get_priv(mu, type);
   if (ti != NULL)
      return ti;

   ti = mir_malloc(mu, sizeof(type_info_t));
   ti->tree   = type;
   ti->type   = MIR_NULL_TYPE;
   ti->stamp  = MIR_NULL_STAMP;
   ti->kind   = type_base_kind(type);
   ti->ndims  = 0;
   ti->udims  = 0;
   ti->size   = 1;
   ti->stride = 1;

   switch (type_kind(type)) {
   case T_SUBTYPE:
      if (type_is_array(type))
         fill_array_type_info(mu, type, ti);
      else {
         ti->type = type_info(mu, type_base(type))->type;

         if (type_is_integer(type) || type_is_enum(type)) {
            tree_t r = range_of(type, 0);
            int64_t low, high;
            if (folded_bounds(r, &low, &high))
               ti->stamp = mir_int_stamp(mu, low, high);
         }
         else if (type_is_real(type)) {
            tree_t r = range_of(type, 0);
            double low, high;
            if (folded_bounds_real(r, &low, &high))
               ti->stamp = mir_real_stamp(mu, low, high);
         }
      }
      break;

   case T_ARRAY:
      fill_array_type_info(mu, type, ti);
      break;

   case T_PHYSICAL:
   case T_INTEGER:
      {
         tree_t r = type_dim(type, 0);
         int64_t low, high;
         const bool folded = folded_bounds(r, &low, &high);
         if (folded)
            ti->type = mir_int_type(mu, low, high);
         else
            ti->type = mir_int_type(mu, INT64_MIN, INT64_MAX);
      }
      break;

   case T_ENUM:
      ti->type = mir_int_type(mu, 0, type_enum_literals(type) - 1);
      break;

   case T_RECORD:
      {
         const int nfields = type_fields(type);
         mir_type_t *fields = xmalloc_array(nfields, sizeof(mir_type_t));
         for (int i = 0; i < nfields; i++)
            fields[i] = type_info(mu, tree_type(type_field(type, i)))->type;

         ti->type = mir_record_type(mu, type_ident(type), fields, nfields);
      }
      break;

   case T_PROTECTED:
      ti->type = mir_context_type(mu, type_ident(type));
      break;

   case T_FILE:
      ti->type = mir_file_type(mu, type_info(mu, type_designated(type))->type);
      break;

   case T_ACCESS:
      {
         mir_type_t designated = type_info(mu, type_designated(type))->type;
         ti->type = mir_access_type(mu, designated);
      }
      break;

   case T_REAL:
      {
         tree_t r = type_dim(type, 0);
         double low, high;
         const bool folded = folded_bounds_real(r, &low, &high);
         if (folded)
            ti->type = mir_real_type(mu, low, high);
         else
            ti->type = mir_double_type(mu);
      }
      break;

   case T_INCOMPLETE:
      ti->type = mir_opaque_type(mu);
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

   mir_comment(mu, "%s", tb_get(tb));
#endif

   mir_put_priv(mu, type, ti);
   return ti;
}

static mir_value_t get_bounds_var(vhdl_gen_t *g, const type_info_t *ti)
{
   assert(!is_anonymous_subtype(ti->tree));

   int hops;
   mir_value_t var = mir_search_object(g->mu, ti->tree, &hops);
   assert(!mir_is_null(var));
   assert(var.tag == MIR_TAG_VAR);
   assert(hops == 0);

   return mir_build_load(g->mu, var);
}

static void check_scalar_bounds(vhdl_gen_t *g, const type_info_t *ti,
                                mir_value_t value, tree_t where)
{
   mir_value_t bounds = get_bounds_var(g, ti);
   mir_value_t left = mir_build_uarray_left(g->mu, bounds, 0);
   mir_value_t right = mir_build_uarray_right(g->mu, bounds, 0);
   mir_value_t dir = mir_build_uarray_dir(g->mu, bounds, 0);
   mir_value_t locus = mir_build_locus(g->mu, tree_to_object(where));

   mir_build_range_check(g->mu, value, left, right, dir, locus, locus);
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

   for (const type_info_t *elem = type_info(g->mu, type_elem(ti->tree));
        elem->kind == T_ARRAY;
        elem = type_info(g->mu, type_elem(elem->tree))) {

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

static mir_value_t vhdl_lower_literal(vhdl_gen_t *g, tree_t t)
{
   const type_info_t *ti = type_info(g->mu, tree_type(t));

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

static mir_value_t vhdl_lower_fcall(vhdl_gen_t *g, tree_t t)
{
   tree_t decl = tree_ref(t);
   const type_info_t *ti = type_info(g->mu, type_result(tree_type(decl)));

   return mir_build_fcall(g->mu, tree_ident2(decl), ti->type, ti->stamp,
                          NULL, 0);
}

mir_value_t vhdl_lower_rvalue(vhdl_gen_t *g, tree_t t)
{
   switch (tree_kind(t)) {
   case T_LITERAL:
      return vhdl_lower_literal(g, t);
   case T_FCALL:
      return vhdl_lower_fcall(g, t);
   default:
      CANNOT_HANDLE(t);
   }
}

static void vhdl_lower_const_decl(vhdl_gen_t *g, tree_t t)
{
   mir_comment(g->mu, "Constant declaration %pI", tree_ident(t));

   const type_info_t *ti = type_info(g->mu, tree_type(t));

   mir_value_t var = mir_add_var(g->mu, ti->type, ti->stamp,
                                 tree_ident(t), MIR_VAR_CONST);

   mir_value_t init = vhdl_lower_rvalue(g, tree_value(t));
   if (type_is_scalar(ti->tree))
      check_scalar_bounds(g, ti, init, t);

   mir_build_store(g->mu, var, init);
}

static void vhdl_lower_subtype_decl(vhdl_gen_t *g, tree_t t)
{
   mir_comment(g->mu, "Subtype declaration %pI", tree_ident(t));

   const type_info_t *ti = type_info(g->mu, tree_type(t));

   if (type_is_scalar(ti->tree)) {
      mir_type_t t_bounds = mir_uarray_type(g->mu, 1, ti->type);
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

      mir_value_t var = mir_add_var(g->mu, t_bounds, ti->stamp,
                                    type_ident(ti->tree), MIR_VAR_CONST);
      mir_build_store(g->mu, var, bounds);

      mir_put_object(g->mu, ti->tree, var);
   }
   else
      should_not_reach_here();
}

static void vhdl_lower_decls(vhdl_gen_t *g, tree_t t)
{
   const int ndecls = tree_decls(t);
   for (int i = 0; i < ndecls; i++) {
      tree_t d = tree_decl(t, i);
      switch (tree_kind(d)) {
      case T_FUNC_DECL:
         break;
      case T_SUBTYPE_DECL:
         vhdl_lower_subtype_decl(g, d);
         break;
      case T_CONST_DECL:
         vhdl_lower_const_decl(g, d);
         break;
      default:
         CANNOT_HANDLE(d);
      }
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
   default:
      CANNOT_HANDLE(t);
   }

   mir_optimise(mu, MIR_PASS_O1);

   vhdl_lower_cleanup(&g);
}
