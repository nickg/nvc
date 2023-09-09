//
//  Copyright (C) 2014-2023  Nick Gasson
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
#include "array.h"
#include "common.h"
#include "diag.h"
#include "driver.h"
#include "hash.h"
#include "lib.h"
#include "lower.h"
#include "object.h"
#include "option.h"
#include "phase.h"
#include "psl/psl-phase.h"
#include "rt/cover.h"
#include "rt/rt.h"
#include "type.h"
#include "vcode.h"

#include <assert.h>
#include <stdlib.h>
#include <inttypes.h>
#include <string.h>
#include <ctype.h>
#include <float.h>

typedef enum {
   EXPR_LVALUE,
   EXPR_RVALUE,
} expr_ctx_t;

typedef struct loop_stack loop_stack_t;

struct loop_stack {
   loop_stack_t  *up;
   ident_t        name;
   vcode_block_t  test_bb;
   vcode_block_t  exit_bb;
};

typedef enum {
   LOWER_NORMAL,
   LOWER_THUNK
} lower_mode_t;

typedef enum {
   SCOPE_HAS_PROTECTED = (1 << 1),
} scope_flags_t;

#define INSTANCE_BIT  0x80000000
#define PARAM_VAR_BIT 0x40000000

typedef A(vcode_var_t) var_list_t;

typedef struct _lower_unit {
   unit_registry_t *registry;
   hash_t          *objects;
   lower_unit_t    *parent;
   ident_t          name;
   scope_flags_t    flags;
   tree_t           hier;
   tree_t           container;
   var_list_t       free_temps;
   vcode_unit_t     vunit;
   cover_tagging_t *cover;
   bool             finished;
   bool             elaborating;
   lower_mode_t     mode;
   unsigned         deferred;
} lower_unit_t;

typedef enum {
   PART_ALL,
   PART_ELEM,
   PART_FIELD,
   PART_PUSH_FIELD,
   PART_PUSH_ELEM,
   PART_POP,
   PART_SLICE
} part_kind_t;

typedef struct {
   part_kind_t kind;
   vcode_reg_t reg;
   vcode_reg_t off;
   tree_t      target;
} target_part_t;

typedef struct {
   tree_t      value;
   type_t      type;
   vcode_reg_t reg;
} concat_param_t;

typedef struct {
   vcode_reg_t conv_func;
   vcode_reg_t conv_reg;
   vcode_reg_t conv_count;
   bool        reverse;
   bool        is_const;
} map_signal_param_t;

typedef struct {
   lower_unit_t *lu;
   vcode_reg_t   wake;
} wait_param_t;

typedef void (*lower_field_fn_t)(lower_unit_t *, tree_t, vcode_reg_t,
                                 vcode_reg_t, vcode_reg_t, void *);

typedef A(concat_param_t) concat_list_t;

static vcode_reg_t lower_expr(lower_unit_t *lu, tree_t expr, expr_ctx_t ctx);
static vcode_type_t lower_bounds(type_t type);
static void lower_stmt(lower_unit_t *lu, tree_t stmt, loop_stack_t *loops);
static void lower_func_body(lower_unit_t *lu, object_t *obj);
static void lower_proc_body(lower_unit_t *lu, object_t *obj);
static vcode_reg_t lower_record_aggregate(lower_unit_t *lu, tree_t expr,
                                          bool nest, bool is_const,
                                          vcode_reg_t hint);
static vcode_reg_t lower_aggregate(lower_unit_t *lu, tree_t expr,
                                   vcode_reg_t hint);
static vcode_type_t lower_type(type_t type);
static void lower_decls(lower_unit_t *lu, tree_t scope);
static void lower_check_array_sizes(lower_unit_t *lu, type_t ltype,
                                    type_t rtype, vcode_reg_t lval,
                                    vcode_reg_t rval, vcode_reg_t locus);
static vcode_type_t lower_alias_type(tree_t alias);
static bool lower_const_bounds(type_t type);
static type_t lower_elem_recur(type_t type);
static void lower_predef(lower_unit_t *lu, object_t *obj);
static ident_t lower_predef_func_name(type_t type, const char *op);
static void lower_generics(lower_unit_t *lu, tree_t block);
static vcode_reg_t lower_default_value(lower_unit_t *lu, type_t type,
                                       vcode_reg_t hint_reg);
static vcode_reg_t lower_array_total_len(lower_unit_t *lu, type_t type,
                                         vcode_reg_t reg);
static vcode_var_t lower_temp_var(lower_unit_t *lu, const char *prefix,
                                  vcode_type_t vtype, vcode_type_t vbounds);
static void lower_release_temp(lower_unit_t *lu, vcode_var_t tmp);
static vcode_reg_t lower_resolved(lower_unit_t *lu, type_t type,
                                  vcode_reg_t reg);
static void lower_copy_record(lower_unit_t *lu, type_t type,
                              vcode_reg_t dst_ptr, vcode_reg_t src_ptr,
                              vcode_reg_t locus);
static bool lower_is_signal_ref(tree_t expr);
static vcode_reg_t lower_rewrap(vcode_reg_t data, vcode_reg_t bounds);
static void lower_predef_field_eq_cb(lower_unit_t *lu, tree_t field,
                                     vcode_reg_t r0, vcode_reg_t r1,
                                     vcode_reg_t locus, void *context);
static void lower_check_indexes(lower_unit_t *lu, type_t from, type_t to,
                                vcode_reg_t array, tree_t where);
static vcode_reg_t lower_conversion(lower_unit_t *lu, vcode_reg_t value_reg,
                                    tree_t where, type_t from, type_t to);

typedef vcode_reg_t (*lower_signal_flag_fn_t)(vcode_reg_t, vcode_reg_t);
typedef vcode_reg_t (*arith_fn_t)(vcode_reg_t, vcode_reg_t);

#define PUSH_DEBUG_INFO(t)                              \
   __attribute__((cleanup(emit_debug_info), unused))    \
   const loc_t _old_loc = *vcode_last_loc();            \
   emit_debug_info(tree_loc((t)));                      \

static bool lower_is_const(tree_t t)
{
   switch (tree_kind(t)) {
   case T_AGGREGATE:
      {
         type_t type = tree_type(t);
         if (type_is_record(type) && !lower_const_bounds(type))
            return false;

         const int nassocs = tree_assocs(t);
         for (int i = 0; i < nassocs; i++) {
            tree_t a = tree_assoc(t, i);
            switch (tree_subkind(a)) {
            case A_NAMED:
               if (!lower_is_const(tree_name(a)))
                  return false;
               break;
            case A_RANGE:
               {
                  tree_t r = tree_range(a, 0);
                  if (tree_subkind(r) == RANGE_EXPR)
                     return false;
                  else if (!lower_is_const(tree_left(r)))
                     return false;
                  else if (!lower_is_const(tree_right(r)))
                     return false;
               }
               break;
            }

            if (!lower_is_const(tree_value(tree_assoc(t, i))))
               return false;
         }

         return true;
      }

   case T_REF:
      {
         tree_t decl = tree_ref(t);
         const tree_kind_t decl_kind = tree_kind(decl);
         if (decl_kind == T_CONST_DECL && type_is_scalar(tree_type(t)))
            return tree_has_value(decl) && lower_is_const(tree_value(decl));
         else
            return decl_kind == T_ENUM_LIT || decl_kind == T_FIELD_DECL;
      }

   case T_LITERAL:
   case T_STRING:
      return true;

   case T_RANGE:
      if (tree_subkind(t) == RANGE_EXPR)
         return lower_is_const(tree_value(t));
      else
         return lower_is_const(tree_left(t)) && lower_is_const(tree_right(t));

   default:
      return false;
   }
}

static bool lower_const_range(tree_t r)
{
   switch (tree_subkind(r)) {
   case RANGE_TO:
   case RANGE_DOWNTO:
      return lower_is_const(tree_left(r)) && lower_is_const(tree_right(r));
   default:
      return false;
   }
}

static bool lower_const_bounds(type_t type)
{
   if (type_is_record(type)) {
      const int nfields = type_fields(type);
      for (int i = 0; i < nfields; i++) {
         type_t ftype = tree_type(type_field(type, i));
         if (type_is_composite(ftype) && !lower_const_bounds(ftype))
            return false;
      }

      return true;
   }
   else if (type_is_unconstrained(type))
      return false;
   else {
      const int ndims = dimension_of(type);
      for (int i = 0; i < ndims; i++) {
         if (!lower_const_range(range_of(type, i)))
            return false;
      }

      type_t elem = type_elem(type);
      return type_is_composite(elem) ? lower_const_bounds(elem) : true;
   }
}

static int dims_for_type(type_t type)
{
   int ndims = dimension_of(type);

   for (type_t e = type_elem(type);
        type_is_array(e) && !lower_const_bounds(e);
        e = type_elem(e))
      ndims += dimension_of(e);

   return ndims;
}

static bool have_uarray_ptr(vcode_reg_t reg)
{
   vcode_type_t vtype = vcode_reg_type(reg);
   if (vtype_kind(vtype) != VCODE_TYPE_POINTER)
      return false;

   return vtype_kind(vtype_pointed(vtype)) == VCODE_TYPE_UARRAY;
}

static bool lower_trivially_copyable(type_t type)
{
   if (type_is_record(type))
      return lower_const_bounds(type);
   else if (type_is_array(type))
      return lower_trivially_copyable(lower_elem_recur(type));
   else
      return true;
}

static bool lower_can_be_signal(type_t type)
{
   switch (type_kind(type)) {
   case T_RECORD:
      {
         const int nfields = type_fields(type);
         for (int i = 0; i < nfields; i++) {
            if (!lower_can_be_signal(tree_type(type_field(type, i))))
               return false;
         }

         return true;
      }
   case T_ARRAY:
      return lower_can_be_signal(type_elem(type));
   case T_SUBTYPE:
      return lower_can_be_signal(type_base(type));
   case T_ACCESS:
   case T_FILE:
   case T_PROTECTED:
   case T_INCOMPLETE:
      return false;
   default:
      return true;
   }
}

static bool lower_is_reverse_range(tree_t r, int *dim)
{
   tree_t value = tree_value(r);
   assert(tree_kind(value) == T_ATTR_REF);

   if (tree_params(value) > 0)
      *dim = assume_int(tree_value(tree_param(value, 0))) - 1;
   else
      *dim = 0;

   return tree_subkind(value) == ATTR_REVERSE_RANGE;
}

static vcode_reg_t lower_range_left(lower_unit_t *lu, tree_t r)
{
   assert(tree_kind(r) == T_RANGE);

   if (tree_subkind(r) == RANGE_EXPR) {
      tree_t array = tree_name(tree_value(r));
      assert(!lower_const_bounds(tree_type(array)));

      vcode_reg_t array_reg = lower_rvalue(lu, array), left_reg;

      int dim;
      if (lower_is_reverse_range(r, &dim))
         left_reg = emit_uarray_right(array_reg, dim);
      else
         left_reg = emit_uarray_left(array_reg, dim);

      type_t index_type = index_type_of(tree_type(array), 0);
      vcode_type_t vtype = lower_type(index_type);
      vcode_type_t vbounds = lower_bounds(index_type);
      return emit_cast(vtype, vbounds, left_reg);
   }
   else
      return lower_rvalue(lu, tree_left(r));
}

static vcode_reg_t lower_range_right(lower_unit_t *lu, tree_t r)
{
   assert(tree_kind(r) == T_RANGE);

   if (tree_subkind(r) == RANGE_EXPR) {
      tree_t array = tree_name(tree_value(r));
      assert(!lower_const_bounds(tree_type(array)));

      vcode_reg_t array_reg = lower_rvalue(lu, array), right_reg;

      int dim;
      if (lower_is_reverse_range(r, &dim))
         right_reg = emit_uarray_left(array_reg, dim);
      else
         right_reg = emit_uarray_right(array_reg, dim);

      type_t index_type = index_type_of(tree_type(array), 0);
      vcode_type_t vtype = lower_type(index_type);
      vcode_type_t vbounds = lower_bounds(index_type);
      return emit_cast(vtype, vbounds, right_reg);
   }
   else
      return lower_rvalue(lu, tree_right(r));
}

static vcode_reg_t lower_range_dir(lower_unit_t *lu, tree_t r)
{
   const range_kind_t rkind = tree_subkind(r);

   switch (rkind) {
   case RANGE_TO:
   case RANGE_DOWNTO:
      return emit_const(vtype_bool(), rkind);

   case RANGE_EXPR:
      {
         tree_t array = tree_name(tree_value(r));
         assert(!lower_const_bounds(tree_type(array)));

         vcode_reg_t array_reg = lower_rvalue(lu, array);

         int dim;
         if (lower_is_reverse_range(r, &dim))
            return emit_not(emit_uarray_dir(array_reg, dim));
         else
            return emit_uarray_dir(array_reg, dim);
      }

   case RANGE_ERROR:
      break;
   }

   return VCODE_INVALID_REG;
}

static vcode_reg_t lower_array_data(vcode_reg_t reg)
{
   vcode_type_t type = vcode_reg_type(reg);
   switch (vtype_kind(type)) {
   case VCODE_TYPE_UARRAY:
      return emit_unwrap(reg);

   case VCODE_TYPE_POINTER:
      if (vtype_kind(vtype_pointed(type)) == VCODE_TYPE_UARRAY)
         return emit_unwrap(emit_load_indirect(reg));
      else
         return reg;

   case VCODE_TYPE_SIGNAL:
      return reg;

   default:
      vcode_dump();
      fatal_trace("invalid type in lower_array_data r%d", reg);
   }
}

static bool have_array_metadata(type_t type, vcode_reg_t reg)
{
   if (reg == VCODE_INVALID_REG)
      return false;
   else if (lower_const_bounds(type))
      return false;
   else if (type_is_unconstrained(type)) {
      assert(reg != VCODE_INVALID_REG);
      assert(vcode_reg_kind(reg) == VCODE_TYPE_UARRAY);
      return true;
   }
   else
      return vcode_reg_kind(reg) == VCODE_TYPE_UARRAY;
}

static vcode_reg_t lower_array_left(lower_unit_t *lu, type_t type, int dim,
                                    vcode_reg_t reg)
{
   if (have_array_metadata(type, reg)) {
      type_t index_type = index_type_of(type, dim);
      return emit_cast(lower_type(index_type), lower_bounds(index_type),
                       emit_uarray_left(reg, dim));
   }
   else
      return lower_range_left(lu, range_of(type, dim));
}

static vcode_reg_t lower_array_right(lower_unit_t *lu, type_t type, int dim,
                                     vcode_reg_t reg)
{
   if (have_array_metadata(type, reg)) {
      type_t index_type = index_type_of(type, dim);
      return emit_cast(lower_type(index_type), lower_bounds(index_type),
                       emit_uarray_right(reg, dim));
   }
   else
      return lower_range_right(lu, range_of(type, dim));
}

static vcode_reg_t lower_array_dir(lower_unit_t *lu, type_t type, int dim,
                                   vcode_reg_t reg)
{
   if (have_array_metadata(type, reg))
      return emit_uarray_dir(reg, dim);
   else
      return lower_range_dir(lu, range_of(type, dim));
}

static vcode_reg_t lower_array_len(lower_unit_t *lu, type_t type, int dim,
                                   vcode_reg_t reg)
{
   assert(type_is_array(type));

   if (have_array_metadata(type, reg))
      return emit_uarray_len(reg, dim);
   else {
      tree_t r = range_of(type, dim);

      int64_t low, high;
      if (folded_bounds(r, &low, &high))
         return emit_const(vtype_offset(), MAX(high - low + 1, 0));

      vcode_reg_t left_reg  = lower_range_left(lu, r);
      vcode_reg_t right_reg = lower_range_right(lu, r);
      vcode_reg_t dir_reg   = lower_range_dir(lu, r);

      return emit_range_length(left_reg, right_reg, dir_reg);
   }
}

static vcode_reg_t lower_array_stride(lower_unit_t *lu, type_t type,
                                      vcode_reg_t reg)
{
   vcode_reg_t stride = emit_const(vtype_offset(), 1);

   type_t elem = type_elem(type);
   if (!type_is_array(elem))
      return stride;

   int udims = 0, dim = 0;
   if (have_array_metadata(type, reg)) {
      udims = dims_for_type(type);
      dim = dimension_of(type);   // Skip to element dimensions

      assert(vtype_dims(vcode_reg_type(reg)) == udims);
   }

   do {
      const int edims = dimension_of(elem);
      for (int i = 0; i < edims; i++, dim++) {
         vcode_reg_t len_reg;
         if (dim < udims)
            len_reg = emit_uarray_len(reg, dim);
         else {
            tree_t r = range_of(elem, i);

            int64_t low, high;
            if (folded_bounds(r, &low, &high))
               len_reg = emit_const(vtype_offset(), MAX(high - low + 1, 0));

            vcode_reg_t left_reg  = lower_range_left(lu, r);
            vcode_reg_t right_reg = lower_range_right(lu, r);
            vcode_reg_t dir_reg   = lower_range_dir(lu, r);

            len_reg = emit_range_length(left_reg, right_reg, dir_reg);
         }

         stride = emit_mul(stride, len_reg);
      }
   } while (type_is_array((elem = type_elem(elem))));

   emit_comment("Array of array stride is r%d", stride);
   return stride;
}

static vcode_reg_t lower_array_total_len(lower_unit_t *lu, type_t type,
                                         vcode_reg_t reg)
{
   const int ndims = dimension_of(type);

   vcode_reg_t total = VCODE_INVALID_REG;
   for (int i = 0; i < ndims; i++) {
      vcode_reg_t this = lower_array_len(lu, type, i, reg);
      if (total == VCODE_INVALID_REG)
         total = this;
      else
         total = emit_mul(this, total);
   }

   type_t elem = type_elem(type);
   if (type_is_array(elem))
      return emit_mul(total, lower_array_stride(lu, type, reg));
   else
      return total;
}

static vcode_reg_t lower_type_width(lower_unit_t *lu, type_t type,
                                    vcode_reg_t reg)
{
   if (type_is_array(type))
      return lower_array_total_len(lu, type, reg);
   else
      return emit_const(vtype_offset(), type_width(type));
}

static int lower_array_const_size(type_t type)
{
   const int ndims = dimension_of(type);

   int size = 1;
   for (int i = 0; i < ndims; i++) {
      tree_t r = range_of(type, i);
      int64_t low, high;
      range_bounds(r, &low, &high);
      size *= MAX(high - low + 1, 0);
   }

   type_t elem = type_elem(type);
   return type_is_array(elem) ? size * lower_array_const_size(elem) : size;
}

static type_t lower_elem_recur(type_t type)
{
   while (type_is_array(type))
      type = type_elem(type);
   return type;
}

static vcode_type_t lower_array_type(type_t type)
{
   type_t elem = lower_elem_recur(type);

   vcode_type_t elem_type   = lower_type(elem);
   vcode_type_t elem_bounds = lower_bounds(elem);

   if (lower_const_bounds(type))
      return vtype_carray(lower_array_const_size(type), elem_type, elem_bounds);
   else
      return vtype_uarray(dims_for_type(type), elem_type, elem_bounds);
}

static vcode_type_t lower_type(type_t type)
{
   switch (type_kind(type)) {
   case T_SUBTYPE:
      if (type_is_array(type))
         return lower_array_type(type);
      else
         return lower_type(type_base(type));

   case T_ARRAY:
      return lower_array_type(type);

   case T_PHYSICAL:
   case T_INTEGER:
      {
         tree_t r = type_dim(type, 0);
         int64_t low, high;
         const bool folded = folded_bounds(r, &low, &high);
         if (folded)
            return vtype_int(low, high);
         else
            return vtype_int(INT64_MIN, INT64_MAX);
      }

   case T_ENUM:
      return vtype_int(0, type_enum_literals(type) - 1);

   case T_RECORD:
      {
         ident_t name = type_ident(type);
         vcode_type_t record = vtype_find_named_record(name);
         if (record == VCODE_INVALID_TYPE) {
            vtype_named_record(name, NULL, 0);  // Forward-declare the name

            const int nfields = type_fields(type);
            vcode_type_t fields[nfields];
            for (int i = 0; i < nfields; i++)
               fields[i] = lower_type(tree_type(type_field(type, i)));

            record = vtype_named_record(name, fields, nfields);
         }

         return record;
      }

   case T_PROTECTED:
      return vtype_context(type_ident(type));

   case T_FILE:
      return vtype_file(lower_type(type_designated(type)));

   case T_ACCESS:
      {
         type_t access = type_designated(type);
         if (type_is_array(access) && lower_const_bounds(access))
            return vtype_access(lower_type(lower_elem_recur(access)));
         else
            return vtype_access(lower_type(access));
      }

   case T_REAL:
      {
         tree_t r = type_dim(type, 0);
         double low, high;
         const bool folded = folded_bounds_real(r, &low, &high);
         if (folded)
            return vtype_real(low, high);
         else
            return vtype_real(-DBL_MAX, DBL_MAX);
      }

   case T_INCOMPLETE:
      return vtype_opaque();

   default:
      fatal_trace("cannot lower type kind %s", type_kind_str(type_kind(type)));
   }
}

static vcode_type_t lower_bounds(type_t type)
{
   if (type_kind(type) == T_SUBTYPE) {
      if (type_is_integer(type) || type_is_enum(type)) {
         tree_t r = range_of(type, 0);
         int64_t low, high;
         if (folded_bounds(r, &low, &high))
            return vtype_int(low, high);
      }
      else if (type_is_real(type)) {
         tree_t r = range_of(type, 0);
         double low, high;
         if (folded_bounds_real(r, &low, &high))
            return vtype_real(low, high);
      }
   }

   if (type_is_array(type))
      return lower_bounds(type_elem(type));

   return lower_type(type);
}

static vcode_type_t lower_signal_type(type_t type)
{
   if (type_is_array(type)) {
      if (type_is_homogeneous(type)) {
         vcode_type_t base = vtype_signal(lower_type(lower_elem_recur(type)));
         if (lower_const_bounds(type))
            return base;
         else
            return vtype_uarray(dims_for_type(type), base, base);
      }
      else {
         vcode_type_t base = lower_signal_type(lower_elem_recur(type));
         if (lower_const_bounds(type))
            return vtype_carray(lower_array_const_size(type), base, base);
         else
            return vtype_uarray(dims_for_type(type), base, base);
      }
   }
   else if (type_is_record(type)) {
      type_t base = type_base_recur(type);
      ident_t name = ident_prefix(type_ident(base), ident_new("$"), '\0');
      vcode_type_t record = vtype_find_named_record(name);
      if (record == VCODE_INVALID_TYPE) {
         vtype_named_record(name, NULL, 0);  // Forward-declare the name

         const int nfields = type_fields(base);
         vcode_type_t fields[nfields];
         for (int i = 0; i < nfields; i++)
            fields[i] = lower_signal_type(tree_type(type_field(base, i)));

         record = vtype_named_record(name, fields, nfields);
      }

      return record;
   }
   else
      return vtype_signal(lower_type(type));
}

static vcode_reg_t lower_debug_locus(tree_t t)
{
   ident_t unit;
   ptrdiff_t offset;
   tree_locus(t, &unit, &offset);

   return emit_debug_locus(unit, offset);
}

static vcode_reg_t lower_wrap_with_new_bounds(lower_unit_t *lu,
                                              type_t from_type,
                                              type_t to_type,
                                              vcode_reg_t array,
                                              vcode_reg_t data)
{
   assert(type_is_array(from_type));
   assert(type_is_array(to_type));

   const int ncons = dims_for_type(to_type);
   vcode_dim_t dims[ncons];
   int dptr = 0;

   int udims = 0;
   if (have_array_metadata(from_type, array)) {
      udims = dims_for_type(from_type);
      assert(vtype_dims(vcode_reg_type(array)) == udims);
   }

   for (; dptr < ncons; from_type = type_elem(from_type)) {
      const int ndims = dimension_of(from_type);
      for (int i = 0; i < ndims; i++, dptr++) {
         if (dptr < udims) {
            dims[dptr].left  = emit_uarray_left(array, dptr);
            dims[dptr].right = emit_uarray_right(array, dptr);
            dims[dptr].dir   = emit_uarray_dir(array, dptr);
         }
         else {
            tree_t r = range_of(from_type, i);
            dims[dptr].left  = lower_range_left(lu, r);
            dims[dptr].right = lower_range_right(lu, r);
            dims[dptr].dir   = lower_range_dir(lu, r);
         }
      }
   }

   assert(dptr == ncons);

   return emit_wrap(lower_array_data(data), dims, ncons);
}

static vcode_reg_t lower_wrap(lower_unit_t *lu, type_t type, vcode_reg_t data)
{
   return lower_wrap_with_new_bounds(lu, type, type, data, data);
}

static vcode_reg_t lower_wrap_element(lower_unit_t *lu, type_t type,
                                      vcode_reg_t array, vcode_reg_t data)
{
   assert(type_is_array(type));
   assert(!lower_const_bounds(type_elem(type)));
   assert(array != VCODE_INVALID_REG);

   const int ndims = dimension_of(type);
   const int ncons = vtype_dims(vcode_reg_type(array)) - ndims;
   assert(ncons > 0);

   vcode_dim_t dims[ncons];
   for (int i = 0; i < ncons; i++) {
      dims[i].left  = emit_uarray_left(array, ndims + i);
      dims[i].right = emit_uarray_right(array, ndims + i);
      dims[i].dir   = emit_uarray_dir(array, ndims + i);
   }

   return emit_wrap(lower_array_data(data), dims, ncons);
}

static vcode_reg_t lower_rewrap(vcode_reg_t data, vcode_reg_t bounds)
{
   const int ndims = vtype_dims(vcode_reg_type(bounds));
   vcode_dim_t dims[ndims];

   for (int i = 0; i < ndims; i++) {
      dims[i].left  = emit_uarray_left(bounds, i);
      dims[i].right = emit_uarray_right(bounds, i);
      dims[i].dir   = emit_uarray_dir(bounds, i);
   }

   return emit_wrap(data, dims, ndims);
}

static void lower_for_each_field(lower_unit_t *lu, type_t type,
                                 vcode_reg_t rec1_ptr, vcode_reg_t rec2_ptr,
                                 vcode_reg_t locus, lower_field_fn_t fn,
                                 void *context)
{
   if (type_is_array(type)) {
      assert(!type_is_homogeneous(type));   // Otherwise why call this

      if (have_uarray_ptr(rec1_ptr))
         rec1_ptr = emit_load_indirect(rec1_ptr);

      if (rec2_ptr != VCODE_INVALID_REG && have_uarray_ptr(rec2_ptr))
         rec2_ptr = emit_load_indirect(rec2_ptr);

      if (locus != VCODE_INVALID_REG && rec2_ptr != VCODE_INVALID_REG)
         lower_check_array_sizes(lu, type, type, rec1_ptr, rec2_ptr, locus);

      vcode_type_t voffset = vtype_offset();
      vcode_var_t i_var = lower_temp_var(lu, "i", voffset, voffset);
      emit_store(emit_const(voffset, 0), i_var);

      vcode_reg_t count1_reg = lower_array_total_len(lu, type, rec1_ptr);
      vcode_reg_t data1_reg  = lower_array_data(rec1_ptr);

      vcode_block_t body_bb = emit_block();
      vcode_block_t exit_bb = emit_block();

      vcode_block_t null_reg = emit_cmp(VCODE_CMP_EQ, count1_reg,
                                        emit_const(voffset, 0));
      emit_cond(null_reg, exit_bb, body_bb);

      vcode_select_block(body_bb);

      vcode_reg_t i_reg = emit_load(i_var);
      vcode_reg_t ptr1_reg = emit_array_ref(data1_reg, i_reg);

      vcode_reg_t ptr2_reg = VCODE_INVALID_REG;
      if (rec2_ptr != VCODE_INVALID_REG) {
         vcode_reg_t data2_reg = lower_array_data(rec2_ptr);
         ptr2_reg = emit_array_ref(data2_reg, i_reg);
      }

      type_t elem = lower_elem_recur(type);
      lower_for_each_field(lu, elem, ptr1_reg, ptr2_reg, locus, fn, context);

      vcode_reg_t next_reg = emit_add(i_reg, emit_const(voffset, 1));
      emit_store(next_reg, i_var);

      vcode_reg_t done_reg = emit_cmp(VCODE_CMP_EQ, next_reg, count1_reg);
      emit_cond(done_reg, exit_bb, body_bb);

      vcode_select_block(exit_bb);
      lower_release_temp(lu, i_var);
   }
   else {
      assert(vcode_reg_kind(rec1_ptr) == VCODE_TYPE_POINTER);

      const int nfields = type_fields(type);
      for (int i = 0; i < nfields; i++) {
         tree_t f = type_field(type, i);
         vcode_reg_t f1_reg = emit_record_ref(rec1_ptr, i);
         vcode_reg_t f2_reg = VCODE_INVALID_REG;
         if (rec2_ptr != VCODE_INVALID_REG)
            f2_reg = emit_record_ref(rec2_ptr, i);
         (*fn)(lu, f, f1_reg, f2_reg, locus, context);
      }
   }
}

static void lower_check_scalar_bounds(lower_unit_t *lu, vcode_reg_t value,
                                      type_t type, tree_t where, tree_t hint)
{
   tree_t r = range_of(type, 0);

   vcode_reg_t left_reg  = lower_range_left(lu, r);
   vcode_reg_t right_reg = lower_range_right(lu, r);
   vcode_reg_t dir_reg   = lower_range_dir(lu, r);

   vcode_reg_t locus = lower_debug_locus(where);

   vcode_reg_t hint_locus = locus;
   if (hint != NULL && hint != where)
      hint_locus = lower_debug_locus(hint);

   emit_range_check(value, left_reg, right_reg, dir_reg, locus, hint_locus);
}

static bool lower_have_signal(vcode_reg_t reg)
{
   return vtype_is_signal(vcode_reg_type(reg));
}

static vcode_reg_t lower_coerce_arrays(lower_unit_t *lu, type_t from, type_t to,
                                       vcode_reg_t reg)
{
   vcode_type_t reg_vtype = vcode_reg_type(reg);
   const bool have_uarray = vtype_kind(reg_vtype) == VCODE_TYPE_UARRAY;
   const bool need_uarray = !lower_const_bounds(to);

   if (have_uarray && need_uarray && vtype_dims(reg_vtype) == dims_for_type(to))
      return reg;
   else if (need_uarray) {
      // Need to wrap array with metadata
      return lower_wrap_with_new_bounds(lu, from, to, reg, reg);
   }
   else if (have_uarray && !need_uarray) {
      // Need to unwrap array to get raw pointer
      return emit_unwrap(reg);
   }
   else
      return reg;
}

static void lower_resolved_field_cb(lower_unit_t *lu, tree_t field,
                                    vcode_reg_t field_ptr, vcode_reg_t dst_ptr,
                                    vcode_reg_t locus, void *__ctx)
{
   type_t ftype = tree_type(field);
   if (!type_is_homogeneous(ftype)) {
      if (have_uarray_ptr(dst_ptr)) {
         // Need to allocate memory for the array
         assert(have_uarray_ptr(field_ptr));

         vcode_reg_t field_reg = emit_load_indirect(field_ptr);
         vcode_reg_t count_reg = lower_array_total_len(lu, ftype, field_reg);

         type_t elem = lower_elem_recur(ftype);
         vcode_type_t vtype = lower_type(elem);
         vcode_type_t vbounds = lower_bounds(elem);

         vcode_reg_t mem_reg = emit_alloc(vtype, vbounds, count_reg);

         vcode_reg_t wrap_reg =
            lower_wrap_with_new_bounds(lu, ftype, ftype, field_reg, mem_reg);
         emit_store_indirect(wrap_reg, dst_ptr);
      }

      lower_for_each_field(lu, ftype, field_ptr, dst_ptr, locus,
                           lower_resolved_field_cb, NULL);
   }
   else {
      vcode_reg_t sig_reg = emit_load_indirect(field_ptr);

      if (type_is_array(ftype)) {
         vcode_reg_t r_reg = emit_resolved(lower_array_data(sig_reg));

         if (lower_const_bounds(ftype)) {
            vcode_reg_t count_reg =
               lower_array_total_len(lu, ftype, VCODE_INVALID_REG);
            emit_copy(dst_ptr, r_reg, count_reg);
         }
         else {
            vcode_reg_t wrap_reg = lower_rewrap(r_reg, sig_reg);
            emit_store_indirect(wrap_reg, dst_ptr);
         }
      }
      else {
         vcode_reg_t r_reg = emit_resolved(sig_reg);
         emit_store_indirect(emit_load_indirect(r_reg), dst_ptr);
      }
   }
}

static vcode_reg_t lower_signal_record_aggregate(lower_unit_t *lu, tree_t expr)
{
   type_t type = tree_type(expr);
   const int nfields = type_fields(type);
   const int nassocs = tree_assocs(expr);

   vcode_reg_t *vals LOCAL = xcalloc_array(nfields, sizeof(vcode_reg_t));
   for (int i = 0; i < nfields; i++)
      vals[i] = VCODE_INVALID_REG;

   type_t *value_types LOCAL = xcalloc_array(nfields, sizeof(type));

   for (int i = 0; i < nassocs; i++) {
      tree_t a = tree_assoc(expr, i);
      tree_t value = tree_value(a);

      int pos = 0;
      switch (tree_subkind(a)) {
      case A_POS:
         pos = tree_pos(a);
         break;
      case A_NAMED:
         pos = tree_pos(tree_ref(tree_name(a)));
         break;
      case A_OTHERS:
      case A_RANGE:
         fatal_trace("unexpected association in record signal aggregate");
      }

      vals[pos] = lower_lvalue(lu, value);
      value_types[pos] = tree_type(value);
   }

   for (int i = 0; i < nfields; i++)
      assert(vals[i] != VCODE_INVALID_REG);

   vcode_type_t vtype = lower_signal_type(type);
   vcode_var_t tmp_var = lower_temp_var(lu, "signalagg", vtype, vtype);
   vcode_reg_t mem_reg = emit_index(tmp_var, VCODE_INVALID_REG);

   for (int i = 0; i < nfields; i++) {
      type_t ftype = tree_type(type_field(type, i));
      vcode_reg_t ptr_reg = emit_record_ref(mem_reg, i);
      vcode_reg_t val_reg = vals[i];
      assert(lower_have_signal(val_reg));

      if (type_is_record(ftype))
         emit_copy(ptr_reg, val_reg, VCODE_INVALID_REG);
      else
         emit_store_indirect(val_reg, ptr_reg);
   }

   return mem_reg;
}

static vcode_reg_t lower_signal_array_aggregate(lower_unit_t *lu, tree_t expr)
{
   const int nassocs = tree_assocs(expr);
   if (nassocs != 1)
      fatal_at(tree_loc(expr), "sorry, this form of parameter is not "
               "yet supported");

   tree_t a0 = tree_assoc(expr, 0);
   vcode_reg_t a0_reg = lower_lvalue(lu, tree_value(a0));

   type_t type = tree_type(expr);
   if (type_is_unconstrained(type)) {
      assert(tree_subkind(a0) == A_NAMED);
      vcode_reg_t left_reg = lower_rvalue(lu, tree_name(a0));
      vcode_reg_t dir_reg = emit_const(vtype_bool(), RANGE_TO);
      vcode_dim_t dims[1] = { { left_reg, left_reg, dir_reg } };
      return emit_wrap(a0_reg, dims, 1);
   }
   else if (!lower_const_bounds(type))
      return lower_wrap(lu, type, a0_reg);
   else
      return a0_reg;
}

static vcode_reg_t lower_subprogram_arg(lower_unit_t *lu, tree_t fcall,
                                        unsigned nth)
{
   if (nth >= tree_params(fcall))
      return VCODE_INVALID_REG;

   tree_t param = tree_param(fcall, nth);

   assert(tree_subkind(param) == P_POS);
   assert(tree_pos(param) == nth);

   tree_t value = tree_value(param);
   tree_t decl = tree_ref(fcall);

   port_mode_t mode = PORT_IN;
   if (nth < tree_ports(decl))
      mode = tree_subkind(tree_port(decl, nth));

   tree_t port = NULL;
   if (!is_open_coded_builtin(tree_subkind(decl)))
      port = tree_port(decl, nth);

   type_t value_type = tree_type(value);
   type_t port_type = port ? tree_type(port) : value_type;

   const class_t class = port ? tree_class(port) : C_DEFAULT;

   // This is not legal in VHDL anyway, but we allow it at analysis time
   // with --relaxed
   if (class == C_SIGNAL && lu->elaborating)
      fatal_at(tree_loc(value), "cannot pass signal argument during "
               "static elaboration");

   vcode_reg_t reg;
   if (class == C_SIGNAL && tree_kind(value) == T_AGGREGATE) {
      if (type_is_record(value_type))
         reg = lower_signal_record_aggregate(lu, value);
      else
         reg = lower_signal_array_aggregate(lu, value);
   }
   else if (class == C_SIGNAL || class == C_FILE || mode != PORT_IN)
      reg = lower_lvalue(lu, value);
   else
      reg = lower_rvalue(lu, value);

   if (reg == VCODE_INVALID_REG)
      return reg;

   if (type_is_array(value_type)) {
      if (port != NULL && !type_is_unconstrained(port_type)) {
         vcode_reg_t locus = lower_debug_locus(port);
         lower_check_array_sizes(lu, port_type, value_type,
                                 VCODE_INVALID_REG, reg, locus);
      }
      return lower_coerce_arrays(lu, value_type, port_type, reg);
   }
   else if (class == C_SIGNAL || class == C_FILE)
      return reg;
   else if (mode == PORT_OUT || port == NULL || !type_is_scalar(port_type))
      return reg;
   else if (mode == PORT_INOUT) {
      vcode_reg_t scalar_reg = emit_load_indirect(reg);
      lower_check_scalar_bounds(lu, scalar_reg, port_type, value, port);
      return reg;
   }
   else {
      lower_check_scalar_bounds(lu, reg, port_type, value, port);
      return reg;
   }
}

static void lower_signal_flag_field_cb(lower_unit_t *lu, tree_t field,
                                       vcode_reg_t field_ptr,
                                       vcode_reg_t unused, vcode_reg_t locus,
                                       void *__ctx)
{
   type_t ftype = tree_type(field);
   if (!type_is_homogeneous(ftype))
      lower_for_each_field(lu, ftype, field_ptr, VCODE_INVALID_REG,
                           locus, lower_signal_flag_field_cb, __ctx);
   else {
      struct {
         lower_signal_flag_fn_t  fn;
         vcode_reg_t            *result;
      } *args = __ctx;

      vcode_reg_t flag;
      if (type_is_array(ftype)) {
         vcode_reg_t nets_reg = emit_load_indirect(field_ptr);
         vcode_reg_t len_reg = lower_array_total_len(lu, ftype, nets_reg);
         flag = (*args->fn)(nets_reg, len_reg);
      }
      else {
         vcode_reg_t nets_reg = emit_load_indirect(field_ptr);
         flag = (*args->fn)(nets_reg, emit_const(vtype_offset(), 1));
      }

      *(args->result) = emit_or(*(args->result), flag);
   }
}

static vcode_reg_t lower_signal_flag(lower_unit_t *lu, tree_t ref,
                                     lower_signal_flag_fn_t fn)
{
   vcode_reg_t nets = lower_lvalue(lu, ref);
   if (nets == VCODE_INVALID_REG)
      return emit_const(vtype_bool(), 0);

   type_t type = tree_type(ref);
   if (!type_is_homogeneous(type)) {
      vcode_reg_t result = emit_const(vtype_bool(), 0);
      struct {
         lower_signal_flag_fn_t  fn;
         vcode_reg_t            *result;
      } args = { fn, &result };
      lower_for_each_field(lu, type, nets, VCODE_INVALID_REG,
                           VCODE_INVALID_REG,
                           lower_signal_flag_field_cb, &args);
      return result;
   }
   else if (type_is_array(type)) {
      vcode_reg_t data_reg = lower_array_data(nets);
      vcode_reg_t len_reg = lower_array_total_len(lu, type, nets);
      return (*fn)(data_reg, len_reg);
   }
   else
      return (*fn)(nets, emit_const(vtype_offset(), 1));
}

static vcode_reg_t lower_last_value(lower_unit_t *lu, tree_t ref)
{
   vcode_reg_t nets = lower_lvalue(lu, ref);

   type_t type = tree_type(ref);
   if (type_is_array(type) && !lower_const_bounds(type)) {
      assert(vcode_reg_kind(nets) == VCODE_TYPE_UARRAY);
      vcode_reg_t last_reg = emit_last_value(emit_unwrap(nets));
      return lower_wrap_with_new_bounds(lu, type, type, nets, last_reg);
   }
   else
      return emit_last_value(nets);
}


static type_t lower_arg_type(tree_t fcall, int nth)
{
   if (nth >= tree_params(fcall))
      return NULL;
   else
      return tree_type(tree_value(tree_param(fcall, nth)));
}

static vcode_reg_t lower_wrap_string(const char *str)
{
   const size_t len = strlen(str);
   vcode_reg_t chars[len + 1];
   vcode_type_t ctype = vtype_char();

   for (int j = 0; j < len; j++)
      chars[j] = emit_const(ctype, str[j]);

   vcode_type_t str_type = vtype_carray(len, ctype, ctype);
   vcode_reg_t data = emit_const_array(str_type, chars, len);

   vcode_dim_t dim0 = {
      .left  = emit_const(vtype_offset(), 1),
      .right = emit_const(vtype_offset(), len),
      .dir   = emit_const(vtype_bool(), RANGE_TO)
   };
   return emit_wrap(emit_address_of(data), &dim0, 1);
}

static vcode_reg_t lower_name_attr(lower_unit_t *lu, tree_t ref,
                                   attr_kind_t which)
{
   tree_t decl = tree_ref(ref);

   if (which == ATTR_SIMPLE_NAME) {
      LOCAL_TEXT_BUF tb = tb_new();
      tb_istr(tb, tree_ident(decl));
      tb_downcase(tb);
      return lower_wrap_string(tb_get(tb));
   }
   else if (lu->mode == LOWER_THUNK) {
      vcode_type_t vchar = vtype_char();
      return emit_undefined(vtype_uarray(1, vchar, vchar), vchar);
   }

   switch (tree_kind(decl)) {
   case T_PACK_BODY:
      decl = tree_primary(decl);
      // Fall-through
   case T_PACKAGE:
   case T_PACK_INST:
      {
         LOCAL_TEXT_BUF tb = tb_new();
         tb_append(tb, ':');
         tb_istr(tb, tree_ident(decl));
         tb_append(tb, ':');
         tb_replace(tb, '.', ':');
         tb_downcase(tb);
         return lower_wrap_string(tb_get(tb));
      }

   case T_INSTANCE:
      {
         ident_t dname = tree_ident(decl);
         lower_unit_t *it;
         for (it = lu; it != NULL; it = it->parent) {
            if (tree_ident(it->container) == dname)
               break;
         }

         if (it == NULL)
            fatal_trace("cannot find instance %s", istr(tree_ident(decl)));

         LOCAL_TEXT_BUF tb = tb_new();

         tree_t hier = tree_decl(it->container, 0);
         assert(tree_kind(hier) == T_HIER);

         if (which == ATTR_PATH_NAME)
            tb_istr(tb, tree_ident(hier));
         else
            tb_istr(tb, tree_ident2(hier));

         tb_append(tb, ':');
         return lower_wrap_string(tb_get(tb));
      }

   case T_BLOCK:
   case T_ENTITY:
   case T_ARCH:
      {
         ident_t dname = tree_ident(decl);
         lower_unit_t *it;
         for (it = lu; it != NULL; it = it->parent) {
            if (it->hier == NULL)
               continue;

            tree_t unit = tree_ref(it->hier);
            if (unit == decl || tree_ident(unit) == dname)
               break;
            else if (tree_kind(unit) == T_ARCH) {
               tree_t entity = tree_primary(unit);
               if (tree_ident(entity) == dname)
                  break;
            }
         }

         if (it == NULL)
            fatal_trace("cannot find %s %s", tree_kind_str(tree_kind(decl)),
                        istr(tree_ident(decl)));

         LOCAL_TEXT_BUF tb = tb_new();

         if (which == ATTR_PATH_NAME)
            tb_istr(tb, tree_ident(it->hier));
         else
            tb_istr(tb, tree_ident2(it->hier));

         tb_append(tb, ':');
         return lower_wrap_string(tb_get(tb));
      }

   case T_PROCESS:
      {
         lower_unit_t *scope = lu;
         while (scope->hier == NULL)
            scope = scope->parent;

         LOCAL_TEXT_BUF tb = tb_new();
         if (which == ATTR_PATH_NAME)
            tb_istr(tb, tree_ident(scope->hier));
         else
            tb_istr(tb, tree_ident2(scope->hier));
         tb_append(tb, ':');

         if (!(tree_flags(decl) & TREE_F_SYNTHETIC_NAME))
            tb_istr(tb, tree_ident(decl));

         tb_append(tb, ':');
         tb_downcase(tb);
         return lower_wrap_string(tb_get(tb));
      }

   case T_PROC_DECL:
   case T_FUNC_DECL:
   case T_PROC_BODY:
   case T_FUNC_BODY:
   case T_VAR_DECL:
   case T_SIGNAL_DECL:
   case T_ALIAS:
   case T_PORT_DECL:
   case T_CONST_DECL:
   case T_GENERIC_DECL:
   case T_PARAM_DECL:
      {
         LOCAL_TEXT_BUF tb = tb_new();
         tree_t container;
         int hops, obj = lower_search_vcode_obj(decl, lu, &hops);

         if (obj == -1 && is_package((container = tree_container(decl)))) {
            tb_append(tb, ':');
            if (tree_kind(container) == T_PACK_BODY)
               tb_istr(tb, tree_ident(tree_primary(container)));
            else
               tb_istr(tb, tree_ident(container));
         }
         else {
            lower_unit_t *scope = lu;
            for (; hops--; scope = scope->parent);

            SCOPED_A(lower_unit_t *) scopes = AINIT;
            for (; scope && scope->hier == NULL; scope = scope->parent)
               APUSH(scopes, scope);

            if (scope != NULL) {
               switch (which) {
               case ATTR_PATH_NAME:
                  tb_istr(tb, tree_ident(scope->hier));
                  break;
               case ATTR_INSTANCE_NAME:
                  tb_istr(tb, tree_ident2(scope->hier));
                  break;
               default:
                  break;
               }
            }

            for (int i = scopes.count - 1; i >= 0; i--) {
               lower_unit_t *s = scopes.items[i];
               if (s->container == decl)
                  continue;  // Printed below

               tb_append(tb, ':');

               const bool synthetic =
                  tree_kind(s->container) == T_PROCESS
                  && (tree_flags(s->container) & TREE_F_SYNTHETIC_NAME);

               if (synthetic)
                  ;   // Blank
               else if (tree_kind(s->container) == T_PACK_BODY)
                  tb_istr(tb, tree_ident(tree_primary(s->container)));
               else
                  tb_istr(tb, tree_ident(s->container));

               if (standard() >= STD_02 && is_subprogram(s->container))
                  type_signature(tree_type(s->container), tb);
            }
         }

         tb_append(tb, ':');
         tb_istr(tb, tree_ident(decl));
         if (standard() >= STD_02 && is_subprogram(decl))
            type_signature(tree_type(decl), tb);

         if (is_container(decl) || is_subprogram(decl))
            tb_append(tb, ':');

         tb_replace(tb, '.', ':');
         tb_downcase(tb);

         return lower_wrap_string(tb_get(tb));
      }

   default:
      fatal_trace("cannot handle decl kind %s in lower_name_attr",
                  tree_kind_str(tree_kind(decl)));
   }
}

static vcode_reg_t lower_narrow(type_t result, vcode_reg_t reg)
{
   // Resize arithmetic result to width of target type

   vcode_type_t vtype = lower_type(result);
   if (!vtype_eq(vtype, vcode_reg_type(reg)))
      return emit_cast(vtype, vcode_reg_bounds(reg), reg);
   else
      return reg;
}

static void lower_coerce_scalar(vcode_reg_t *r0, vcode_reg_t *r1)
{
   vcode_type_t r0_type = vcode_reg_type(*r0);
   vcode_type_t r1_type = vcode_reg_type(*r1);
   if (!vtype_eq(r0_type, r1_type)) {
      const unsigned r0_bits = bits_for_range(vtype_low(r0_type),
                                              vtype_high(r0_type));
      const unsigned r1_bits = bits_for_range(vtype_low(r1_type),
                                              vtype_high(r1_type));

      if (r1_bits > r0_bits)
         *r0 = emit_cast(r1_type, vcode_reg_bounds(*r0), *r0);
      else
         *r1 = emit_cast(r0_type, vcode_reg_bounds(*r1), *r1);
   }
}

static vcode_reg_t lower_arith(tree_t fcall, subprogram_kind_t kind,
                               vcode_reg_t r0, vcode_reg_t r1)
{
   lower_coerce_scalar(&r0, &r1);

   type_t type = tree_type(fcall);

   vcode_reg_t result = VCODE_INVALID_REG;
   switch (kind) {
   case S_ADD:
      if (type_is_integer(type))
         result = emit_trap_add(r0, r1, lower_debug_locus(fcall));
      else
         result = emit_add(r0, r1);
      break;
   case S_MUL:
      if (type_is_integer(type))
         result = emit_trap_mul(r0, r1, lower_debug_locus(fcall));
      else
         result = emit_mul(r0, r1);
      break;
   case S_SUB:
      if (type_is_integer(type))
         result = emit_trap_sub(r0, r1, lower_debug_locus(fcall));
      else
         result = emit_sub(r0, r1);
      break;
   case S_MOD: result = emit_mod(r0, r1); break;
   case S_REM: result = emit_rem(r0, r1); break;
   case S_DIV: result = emit_div(r0, r1); break;
   case S_EXP:
      if (type_is_integer(type))
         result = emit_trap_exp(r0, r1, lower_debug_locus(fcall));
      else
         result = emit_exp(r0, r1);
      break;
   default:
      fatal_trace("invalid subprogram kind %d in lower_arith", kind);
   }

   return lower_narrow(tree_type(fcall), result);
}

static void lower_branch_coverage(lower_unit_t *lu, tree_t b,
                                  unsigned int flags, vcode_reg_t hit_reg)
{
   assert(cover_enabled(lu->cover, COVER_MASK_BRANCH));

   // Refer location of test condition instead of branch statement to
   // get accurate test condition location in the coverage report
   const loc_t *loc = (tree_kind(b) == T_ASSOC) ?
                        tree_loc(b) : tree_loc(tree_value(b));

   cover_tag_t *tag = cover_add_tag(b, loc, NULL, lu->cover, TAG_BRANCH, flags);
   if (tag != NULL)
      emit_cover_branch(hit_reg, tag->tag, flags);
}

static int32_t lower_toggle_tag_for(lower_unit_t *lu, type_t type, tree_t where,
                                    ident_t prefix, int curr_dim)
{
   type_t root = type;

   // Gets well known type for scalar and vectorized version of
   // standard types (std_[u]logic[_vector], signed, unsigned)
   while (type_base_kind(root) == T_ARRAY)
      root = type_elem(root);
   root = type_base_recur(root);

   well_known_t known = is_well_known(type_ident(root));
   if (known != W_IEEE_ULOGIC && known != W_IEEE_ULOGIC_VECTOR)
      return -1;

   unsigned int flags = COV_FLAG_TOGGLE_TO_0 | COV_FLAG_TOGGLE_TO_1;
   if (tree_kind(where) == T_SIGNAL_DECL)
      flags |= COV_FLAG_TOGGLE_SIGNAL;
   else
      flags |= COV_FLAG_TOGGLE_PORT;

   if (type_is_array(type)) {
      int t_dims = dimension_of(type);
      tree_t r = range_of(type, t_dims - curr_dim);
      int32_t first_tag = -1;
      int64_t low, high;

      if (folded_bounds(r, &low, &high)) {
         assert(low <= high);

         int64_t first, last, i;
         int inc;

         if (cover_skip_array_toggle(lu->cover, high - low + 1))
            return -1;

         cover_inc_array_depth(lu->cover);

         switch (tree_subkind(r)) {
         case RANGE_DOWNTO:
            i = high;
            first = high;
            last = low;
            inc = -1;
            break;
         case RANGE_TO:
            i = low;
            first = low;
            last = high;
            inc = +1;
            break;
         default:
            fatal("Invalid subkind for range: %d", tree_subkind(r));
         }

         while (1) {
            char arr_index[16];
            int32_t tmp = -1;
            checked_sprintf(arr_index, sizeof(arr_index), "(%"PRIi64")", i);
            ident_t arr_suffix =
               ident_prefix(prefix, ident_new(arr_index), '\0');

            // On lowest dimension walk through elements, if elements
            // are arrays, then start new (nested) recursion.
            if (curr_dim == 1) {
               type_t e_type = type_elem(type);
               if (type_is_array(e_type))
                  tmp = lower_toggle_tag_for(lu, e_type, where, arr_suffix,
                                             dimension_of(e_type));
               else {
                  cover_tag_t *tag = cover_add_tag(where, tree_loc(where),
                                                   arr_suffix, lu->cover,
                                                   TAG_TOGGLE, flags);
                  if (tag)
                     tmp = tag->tag;
               }
            }
            else   // Recurse to lower dimension
               tmp = lower_toggle_tag_for(lu, type, where, arr_suffix,
                                          curr_dim - 1);

            if (i == first)
               first_tag = tmp;
            if (i == last)
               break;

            i += inc;
         }

         cover_dec_array_depth(lu->cover);
      }
      return first_tag;
   }
   else {
      cover_tag_t *tag = cover_add_tag(where, tree_loc(where), NULL, lu->cover,
                                       TAG_TOGGLE, flags);
      return tag ? tag->tag : -1;
   }
}

static void lower_toggle_coverage_cb(lower_unit_t *lu, tree_t field,
                                     vcode_reg_t field_ptr,
                                     vcode_reg_t unused, vcode_reg_t locus,
                                     void *ctx)
{
   type_t ftype = tree_type(field);

   cover_push_scope(lu->cover, field);

   if (!type_is_homogeneous(ftype))
      lower_for_each_field(lu, ftype, field_ptr, VCODE_INVALID_REG,
                           locus, lower_toggle_coverage_cb, NULL);
   else {
      const int ndims = dimension_of(ftype);
      int32_t tag = lower_toggle_tag_for(lu, ftype, field, NULL, ndims);
      if (tag == -1) {
         cover_pop_scope(lu->cover);
         return;
      }

      vcode_reg_t nets_reg = emit_load_indirect(field_ptr);
      emit_cover_toggle(nets_reg, tag);
   }

   cover_pop_scope(lu->cover);
}

static void lower_toggle_coverage(lower_unit_t *lu, tree_t decl)
{
   assert(cover_enabled(lu->cover, COVER_MASK_TOGGLE));

   int hops = 0;
   vcode_var_t var = lower_search_vcode_obj(decl, lu, &hops);
   assert(var != VCODE_INVALID_VAR);
   assert(hops == 0);

   cover_push_scope(lu->cover, decl);

   type_t type = tree_type(decl);
   if (!type_is_homogeneous(type)) {
      vcode_reg_t rec_ptr = emit_index(var, VCODE_INVALID_REG);
      lower_for_each_field(lu, type, rec_ptr, VCODE_INVALID_REG,
                           VCODE_INVALID_REG, lower_toggle_coverage_cb, NULL);
   }
   else {
      const int ndims = dimension_of(type);
      int32_t tag = lower_toggle_tag_for(lu, type, decl, NULL, ndims);
      if (tag == -1) {
         cover_pop_scope(lu->cover);
         return;
      }

      vcode_reg_t nets_reg = emit_load(var);
      emit_cover_toggle(nets_reg, tag);
   }

   cover_pop_scope(lu->cover);
}

static void lower_expression_coverage(lower_unit_t *lu, tree_t fcall,
                                      unsigned flags, vcode_reg_t mask,
                                      unsigned unrc_msk)
{
   assert(cover_enabled(lu->cover, COVER_MASK_EXPRESSION));

   cover_tag_t *tag = cover_add_tag(fcall, tree_loc(fcall), NULL, lu->cover,
                                    TAG_EXPRESSION, flags);
   if (tag != NULL) {
      emit_cover_expr(mask, tag->tag);
      tag->unrc_msk = unrc_msk;
   }
}

static vcode_reg_t lower_logical(lower_unit_t *lu, tree_t fcall,
                                 vcode_reg_t result, vcode_reg_t lhs,
                                 vcode_reg_t rhs, subprogram_kind_t builtin,
                                 unsigned unrc_msk)
{
   if (!cover_enabled(lu->cover, COVER_MASK_EXPRESSION))
      return result;

   uint32_t flags = 0;
   vcode_type_t vc_int = vtype_int(0, INT32_MAX);

   switch (builtin) {
   case S_SCALAR_AND:
   case S_SCALAR_NAND:
      flags = COVER_FLAGS_AND_EXPR;
      break;

   case S_SCALAR_OR:
   case S_SCALAR_NOR:
      flags = COVER_FLAGS_OR_EXPR;
      break;

   case S_SCALAR_XOR:
   case S_SCALAR_XNOR:
      flags = COVER_FLAGS_XOR_EXPR;
      break;

   case S_SCALAR_EQ:
   case S_SCALAR_NEQ:
   case S_SCALAR_LT:
   case S_SCALAR_GT:
   case S_SCALAR_LE:
   case S_SCALAR_GE:
   case S_SCALAR_NOT:
      {
         vcode_reg_t c_true = emit_const(vc_int, COV_FLAG_TRUE);
         vcode_reg_t c_false = emit_const(vc_int, COV_FLAG_FALSE);
         vcode_reg_t mask = emit_select(result, c_true, c_false);
         flags = COV_FLAG_TRUE | COV_FLAG_FALSE;
         lower_expression_coverage(lu, fcall, flags, mask, unrc_msk);
      }
   default:
      return result;
   }

   vcode_reg_t lhs_n = emit_not(lhs);
   vcode_reg_t rhs_n = emit_not(rhs);

   struct {
      unsigned    flag;
      vcode_reg_t lhs;
      vcode_reg_t rhs;
   } bins[] = {
      { COV_FLAG_00, lhs_n, rhs_n },
      { COV_FLAG_01, lhs_n, rhs   },
      { COV_FLAG_10, lhs,   rhs_n },
      { COV_FLAG_11, lhs,   rhs   },
   };

   // Check LHS/RHS combinations
   vcode_reg_t zero = emit_const(vc_int, 0);
   vcode_reg_t mask = emit_const(vc_int, 0);
   for (int i = 0; i < ARRAY_LEN(bins); i++) {
      if (flags & bins[i].flag) {
         vcode_reg_t select = emit_and(bins[i].lhs, bins[i].rhs);
         vcode_reg_t flag = emit_const(vc_int, bins[i].flag);
         vcode_reg_t set_bit = emit_select(select, flag, zero);
         mask = emit_add(mask, set_bit);
      }
   }

   lower_expression_coverage(lu, fcall, flags, mask, unrc_msk);

   return result;
}

static vcode_reg_t lower_logic_expr_coverage_for(tree_t fcall,
                                                 vcode_reg_t lhs,
                                                 vcode_reg_t rhs,
                                                 unsigned flags)
{
   // Corresponds to values how std_ulogic enum is translated
   vcode_type_t vc_logic = vcode_reg_type(lhs);
   vcode_reg_t log_0 = emit_const(vc_logic, 2);
   vcode_reg_t log_1 = emit_const(vc_logic, 3);

   struct {
      unsigned flag;
      vcode_reg_t lhs_exp;
      vcode_reg_t rhs_exp;
   } bins[] = {
      { COV_FLAG_00, log_0, log_0 },
      { COV_FLAG_01, log_0, log_1 },
      { COV_FLAG_10, log_1, log_0 },
      { COV_FLAG_11, log_1, log_1 },
   };

   vcode_type_t vc_int = vtype_int(0, INT32_MAX);
   vcode_reg_t zero = emit_const(vc_int, 0);
   vcode_reg_t mask = emit_const(vc_int, 0);

   // Build logic to check combinations of LHS and RHS
   for (int i = 0; i < ARRAY_LEN(bins); i++) {
      if (flags & bins[i].flag) {
         vcode_reg_t cmp_lhs = emit_cmp(VCODE_CMP_EQ, lhs, bins[i].lhs_exp);
         vcode_reg_t cmp_rhs = emit_cmp(VCODE_CMP_EQ, rhs, bins[i].rhs_exp);
         vcode_reg_t lhs_rhs_match = emit_and(cmp_lhs, cmp_rhs);
         vcode_reg_t new_mask = emit_select(lhs_rhs_match,
                                            emit_const(vc_int, bins[i].flag),
                                            zero);
         mask = emit_add(mask, new_mask);
      }
   }

   return mask;
}

static void lower_logic_expr_coverage(lower_unit_t *lu, tree_t fcall,
                                      tree_t decl, vcode_reg_t *args)
{
   unsigned flags = cover_get_std_log_expr_flags(decl);
   if (!flags)
      return;

   flags |= COV_FLAG_EXPR_STD_LOGIC;

   if (tree_params(fcall) != 2)
      return;

   // Skip arrays -> Matches behavior of VCS and Modelsim
   if (type_is_array(type_param(tree_type(decl), 0)) ||
       type_is_array(type_param(tree_type(decl), 1)))
      return;

   vcode_reg_t mask =
      lower_logic_expr_coverage_for(fcall, args[1], args[2], flags);
   lower_expression_coverage(lu, fcall, flags, mask, 0);
}

static bool lower_side_effect_free(tree_t expr)
{
   // True if expression is side-effect free with no function calls
   switch (tree_kind(expr)) {
   case T_REF:
   case T_LITERAL:
   case T_STRING:
      return true;
   case T_FCALL:
      {
         const subprogram_kind_t kind = tree_subkind(tree_ref(expr));
         if (kind == S_DIV || kind == S_DIV_PR || kind == S_DIV_RI
             || kind == S_REM || kind == S_MOD)
            return false;
         else if (kind == S_CONCAT)
            return false;   // Allocates memory
         else if (!is_builtin(kind))
            return false;

         const int nparams = tree_params(expr);
         for (int i = 0; i < nparams; i++) {
            if (!lower_side_effect_free(tree_value(tree_param(expr, i))))
               return false;
         }

         return true;
      }
      break;
   case T_RECORD_REF:
   case T_QUALIFIED:
   case T_TYPE_CONV:
      return lower_side_effect_free(tree_value(expr));
   case T_ATTR_REF:
      {
         const attr_kind_t kind = tree_subkind(expr);
         if (kind == ATTR_EVENT || kind == ATTR_ACTIVE)
            return lower_side_effect_free(tree_name(expr));
         else
            return false;
      }
   default:
      return false;
   }
}

static vcode_var_t lower_temp_var(lower_unit_t *lu, const char *prefix,
                                  vcode_type_t vtype, vcode_type_t vbounds)
{
   vcode_var_t tmp = VCODE_INVALID_VAR;
   unsigned pos = 0;
   for (; pos < lu->free_temps.count; pos++) {
      tmp = lu->free_temps.items[pos];
      if (vtype_eq(vcode_var_type(tmp), vtype)
          && vtype_eq(vcode_var_bounds(tmp), vbounds))
         break;
   }

   if (pos == lu->free_temps.count)
      return emit_var(vtype, vbounds, ident_uniq(prefix), VAR_TEMP);

   emit_comment("Reusing temp var %s", istr(vcode_var_name(tmp)));

   for (; pos < lu->free_temps.count - 1; pos++)
      lu->free_temps.items[pos] = lu->free_temps.items[pos + 1];
   APOP(lu->free_temps);

   return tmp;
}

static void lower_release_temp(lower_unit_t *lu, vcode_var_t tmp)
{
   assert(vcode_var_flags(tmp) & VAR_TEMP);
   APUSH(lu->free_temps, tmp);
}

static vcode_reg_t lower_falling_rising_edge(lower_unit_t *lu, tree_t fcall,
                                             subprogram_kind_t kind)
{
   tree_t p0 = tree_value(tree_param(fcall, 0));

   vcode_reg_t nets_reg  = lower_lvalue(lu, p0);
   vcode_reg_t value_reg = lower_rvalue(lu, p0);

   if (kind == S_FALLING_EDGE)
      value_reg = emit_not(value_reg);

   vcode_reg_t event_reg =
      emit_event_flag(nets_reg, emit_const(vtype_offset(), 1));
   return emit_and(event_reg, value_reg);
}

static vcode_reg_t lower_short_circuit(lower_unit_t *lu, tree_t fcall,
                                       subprogram_kind_t builtin)
{
   vcode_reg_t r0 = lower_subprogram_arg(lu, fcall, 0);
   int64_t value;
   if (vcode_reg_const(r0, &value)) {
      vcode_reg_t result = VCODE_INVALID_REG;
      switch (builtin) {
      case S_SCALAR_AND:
         result = value ? lower_subprogram_arg(lu, fcall, 1) : r0;
         break;
      case S_SCALAR_OR:
         result = value ? r0 : lower_subprogram_arg(lu, fcall, 1);
         break;
      case S_SCALAR_NOR:
         result = emit_not(value ? r0 : lower_subprogram_arg(lu, fcall, 1));
         break;
      case S_SCALAR_NAND:
         result = emit_not(value ? lower_subprogram_arg(lu, fcall, 1) : r0);
         break;
      default:
         fatal_trace("unhandled subprogram kind %d in lower_short_circuit",
                      builtin);
      }

      return result;
   }

   if (lower_side_effect_free(tree_value(tree_param(fcall, 1)))) {
      vcode_reg_t r1 = lower_subprogram_arg(lu, fcall, 1);
      switch (builtin) {
      case S_SCALAR_AND:
         return lower_logical(lu, fcall, emit_and(r0, r1), r0, r1, builtin, 0);
      case S_SCALAR_OR:
         return lower_logical(lu, fcall, emit_or(r0, r1), r0, r1, builtin, 0);
      case S_SCALAR_NOR:
         return lower_logical(lu, fcall, emit_nor(r0, r1), r0, r1, builtin, 0);
      case S_SCALAR_NAND:
         return lower_logical(lu, fcall, emit_nand(r0, r1), r0, r1, builtin, 0);
      default:
         fatal_trace("unhandled subprogram kind %d in lower_short_circuit",
                      builtin);
      }
   }

   vcode_block_t arg1_bb = emit_block();
   vcode_block_t after_bb = emit_block();

   vcode_type_t vbool = vtype_bool();
   vcode_var_t tmp_var = lower_temp_var(lu, "shortcircuit", vbool, vbool);

   if (builtin == S_SCALAR_NOR || builtin == S_SCALAR_NAND)
      emit_store(emit_not(r0), tmp_var);
   else
      emit_store(r0, tmp_var);

   if (builtin == S_SCALAR_AND || builtin == S_SCALAR_NAND)
      emit_cond(r0, arg1_bb, after_bb);
   else
      emit_cond(r0, after_bb, arg1_bb);

   vcode_select_block(arg1_bb);
   vcode_reg_t r1 = lower_subprogram_arg(lu, fcall, 1);

   switch (builtin) {
   case S_SCALAR_AND:
      emit_store(emit_and(r0, r1), tmp_var);
      break;
   case S_SCALAR_OR:
      emit_store(emit_or(r0, r1), tmp_var);
      break;
   case S_SCALAR_NOR:
      emit_store(emit_nor(r0, r1), tmp_var);
      break;
   case S_SCALAR_NAND:
      emit_store(emit_nand(r0, r1), tmp_var);
      break;
   default:
      fatal_trace("unhandled subprogram kind %d in lower_short_circuit",
                      builtin);
   }

   // Automaticaly flag non-executed bins as un-reachable if set so
   unsigned unrc_msk = 0;
   if (cover_enabled(lu->cover, COVER_MASK_EXCLUDE_UNREACHABLE)) {
      if (builtin == S_SCALAR_AND || builtin == S_SCALAR_NAND)
         unrc_msk = COV_FLAG_00 | COV_FLAG_01;
      else
         unrc_msk = COV_FLAG_11 | COV_FLAG_10;
   }

   // Only emit expression coverage when also arg1 is evaluated.
   lower_logical(lu, fcall, emit_load(tmp_var), r0, r1, builtin,
                 unrc_msk);

   emit_jump(after_bb);

   vcode_select_block(after_bb);
   vcode_reg_t result = emit_load(tmp_var);
   lower_release_temp(lu, tmp_var);

   return result;
}

static void lower_flatten_concat(lower_unit_t *lu, tree_t arg,
                                 concat_list_t *list)
{
   if (tree_kind(arg) == T_FCALL && tree_subkind(tree_ref(arg)) == S_CONCAT) {
      assert(tree_params(arg) == 2);
      lower_flatten_concat(lu, tree_value(tree_param(arg, 0)), list);
      lower_flatten_concat(lu, tree_value(tree_param(arg, 1)), list);
   }
   else {
      vcode_reg_t reg = lower_rvalue(lu, arg);
      APUSH(*list, ((concat_param_t){ arg, tree_type(arg), reg }));
   }
}

static vcode_reg_t lower_concat(lower_unit_t *lu, tree_t expr, vcode_reg_t hint,
                                vcode_reg_t hint_count)
{
   assert(tree_params(expr) == 2);

   concat_list_t args = AINIT;
   lower_flatten_concat(lu, expr, &args);

   type_t type = tree_type(expr);
   type_t elem = type_elem(type);
   assert(type_is_unconstrained(type));

   type_t scalar_elem = lower_elem_recur(elem);

   vcode_type_t voffset = vtype_offset();

   type_t index_type = index_type_of(type, 0);
   tree_t index_r = range_of(index_type, 0);
   vcode_type_t itype = lower_type(index_type);
   vcode_type_t ibounds = lower_bounds(index_type);

   vcode_reg_t len   = emit_const(voffset, 0);
   vcode_reg_t elems = emit_const(voffset, -1);
   vcode_reg_t dir   = lower_range_dir(lu, index_r);
   vcode_reg_t left  = lower_range_left(lu, index_r);

   for (unsigned i = 0; i < args.count; i++) {
      concat_param_t *p = &(args.items[i]);
      if (type_is_array(p->type) && type_eq(p->type, type)) {
         elems = emit_add(elems, lower_array_len(lu, p->type, 0, p->reg));
         len   = emit_add(len, lower_array_total_len(lu, p->type, p->reg));
      }
      else {
         vcode_reg_t one_reg = emit_const(vtype_offset(), 1);
         elems = emit_add(elems, one_reg);
         len   = emit_add(len, one_reg);
      }
   }

   vcode_reg_t mem_reg;
   if (hint != VCODE_INVALID_REG && len == hint_count)
      mem_reg = hint;
   else
      mem_reg = emit_alloc(lower_type(scalar_elem),
                           lower_bounds(scalar_elem), len);

   vcode_reg_t cast_reg   = emit_cast(itype, ibounds, elems);
   vcode_reg_t right_to   = emit_add(left, cast_reg);
   vcode_reg_t right_down = emit_sub(left, cast_reg);
   vcode_reg_t right      = emit_select(dir, right_down, right_to);

   vcode_dim_t dims[1] = { { left, right, dir } };
   vcode_reg_t var_reg = emit_wrap(mem_reg, dims, 1);

   vcode_reg_t off_reg = emit_const(voffset, 0);
   for (unsigned i = 0; i < args.count; i++) {
      concat_param_t *p = &(args.items[i]);
      vcode_reg_t ptr = emit_array_ref(mem_reg, off_reg);
      if (type_is_array(p->type)) {
         vcode_reg_t src_len = lower_array_total_len(lu, p->type, p->reg);

         vcode_reg_t data_reg;
         if (lower_have_signal(p->reg))
            data_reg = lower_array_data(lower_resolved(lu, p->type, p->reg));
         else
            data_reg = lower_array_data(p->reg);

         emit_copy(ptr, data_reg, src_len);
         if (i + 1 < args.count)
            off_reg = emit_add(off_reg, src_len);
      }
      else if (type_is_record(p->type)) {
         emit_copy(ptr, p->reg, VCODE_INVALID_REG);
         if (i + 1 < args.count)
            off_reg = emit_add(off_reg, emit_const(vtype_offset(), 1));
      }
      else {
         emit_store_indirect(p->reg, ptr);
         if (i + 1 < args.count)
            off_reg = emit_add(off_reg, emit_const(vtype_offset(), 1));
      }
   }

   ACLEAR(args);
   return var_reg;
}

static vcode_reg_t lower_comparison(lower_unit_t *lu, tree_t fcall,
                                    subprogram_kind_t builtin,
                                    vcode_reg_t r0, vcode_reg_t r1)
{
   lower_coerce_scalar(&r0, &r1);

   vcode_cmp_t cmp;
   switch (builtin) {
   case S_SCALAR_EQ:  cmp = VCODE_CMP_EQ; break;
   case S_SCALAR_NEQ: cmp = VCODE_CMP_NEQ; break;
   case S_SCALAR_LT:  cmp = VCODE_CMP_LT; break;
   case S_SCALAR_GT:  cmp = VCODE_CMP_GT; break;
   case S_SCALAR_LE:  cmp = VCODE_CMP_LEQ; break;
   case S_SCALAR_GE:  cmp = VCODE_CMP_GEQ; break;
   default:
      fatal_trace("unhandled built-in comparison %d", builtin);
   }

   vcode_reg_t result = emit_cmp(cmp, r0, r1);
   return lower_logical(lu, fcall, result, r0, r1, builtin, 0);
}

static vcode_reg_t lower_builtin(lower_unit_t *lu, tree_t fcall,
                                 subprogram_kind_t builtin,
                                 vcode_reg_t *out_r0, vcode_reg_t *out_r1)
{
   if (builtin == S_SCALAR_AND || builtin == S_SCALAR_OR ||
       builtin == S_SCALAR_NOR || builtin == S_SCALAR_NAND)
      return lower_short_circuit(lu, fcall, builtin);
   else if (builtin == S_CONCAT)
      return lower_concat(lu, fcall, VCODE_INVALID_REG, VCODE_INVALID_REG);
   else if (builtin == S_RISING_EDGE || builtin == S_FALLING_EDGE)
      return lower_falling_rising_edge(lu, fcall, builtin);

   vcode_reg_t r0 = lower_subprogram_arg(lu, fcall, 0);
   vcode_reg_t r1 = lower_subprogram_arg(lu, fcall, 1);

   if (out_r0 != NULL) *out_r0 = r0;
   if (out_r1 != NULL) *out_r1 = r1;

   type_t r0_type = lower_arg_type(fcall, 0);
   type_t r1_type = lower_arg_type(fcall, 1);

   switch (builtin) {
   case S_SCALAR_EQ:
   case S_SCALAR_NEQ:
   case S_SCALAR_LT:
   case S_SCALAR_GT:
   case S_SCALAR_LE:
   case S_SCALAR_GE:
      return lower_comparison(lu, fcall, builtin, r0, r1);
   case S_MOD:
   case S_REM:
   case S_DIV:
      if (type_is_integer(r1_type)) {
         vcode_reg_t locus = lower_debug_locus(fcall);
         emit_zero_check(r1, locus);
      }
      // Fall-through
   case S_MUL:
   case S_ADD:
   case S_SUB:
      return lower_arith(fcall, builtin, r0, r1);
   case S_EXP:
      {
         if (type_is_integer(r0_type)) {
            vcode_reg_t locus = lower_debug_locus(fcall);
            emit_exponent_check(r1, locus);
         }

         if (!type_eq(r0_type, r1_type))
            r1 = emit_cast(lower_type(r0_type), lower_bounds(r0_type), r1);

         return lower_arith(fcall, S_EXP, r0, r1);
      }
   case S_NEGATE:
      {
         if (type_is_integer(r0_type)) {
            vcode_reg_t locus = lower_debug_locus(fcall);
            return lower_narrow(tree_type(fcall), emit_trap_neg(r0, locus));
         }
         else
            return emit_neg(r0);
      }
   case S_ABS:
      return emit_abs(r0);
   case S_IDENTITY:
      return r0;
   case S_SCALAR_NOT:
      return lower_logical(lu, fcall, emit_not(r0), r0, 0, builtin, 0);
   case S_SCALAR_XOR:
      return lower_logical(lu, fcall, emit_xor(r0, r1), r0, r1, builtin, 0);
   case S_SCALAR_XNOR:
      return lower_logical(lu, fcall, emit_xnor(r0, r1), r0, r1, builtin, 0);
   case S_ENDFILE:
      return emit_endfile(r0);
   case S_FILE_OPEN1:
      {
         vcode_reg_t name   = lower_array_data(r1);
         vcode_reg_t length = lower_array_len(lu, r1_type, 0, r1);
         vcode_reg_t kind   = lower_subprogram_arg(lu, fcall, 2);
         emit_file_open(r0, name, length, kind, VCODE_INVALID_REG);
         return VCODE_INVALID_REG;
      }
   case S_FILE_OPEN2:
      {
         type_t arg_type = lower_arg_type(fcall, 2);
         vcode_reg_t r2     = lower_subprogram_arg(lu, fcall, 2);
         vcode_reg_t name   = lower_array_data(r2);
         vcode_reg_t length = lower_array_len(lu, arg_type, 0, r2);
         vcode_reg_t kind   = lower_subprogram_arg(lu, fcall, 3);
         emit_file_open(r1, name, length, kind, r0);
         return VCODE_INVALID_REG;
      }
   case S_FILE_WRITE:
      {
         vcode_reg_t length = VCODE_INVALID_REG;
         vcode_reg_t data   = r1;
         if (type_is_array(r1_type)) {
            length = lower_array_total_len(lu, r1_type, r1);
            data   = lower_array_data(r1);
         }
         emit_file_write(r0, data, length);
         return VCODE_INVALID_REG;
      }
   case S_FILE_CLOSE:
      emit_file_close(r0);
      return VCODE_INVALID_REG;
   case S_FILE_READ:
      {
         vcode_reg_t inlen = VCODE_INVALID_REG;
         if (type_is_array(r1_type))
            inlen = lower_array_total_len(lu, r1_type, r1);

         vcode_reg_t outlen = VCODE_INVALID_REG;
         if (tree_params(fcall) == 3)
            outlen = lower_subprogram_arg(lu, fcall, 2);

         emit_file_read(r0, r1, inlen, outlen);
         return VCODE_INVALID_REG;
      }
   case S_DEALLOCATE:
      emit_deallocate(r0);
      return VCODE_INVALID_REG;
   case S_MUL_RP:
   case S_MUL_RI:
      {
         vcode_type_t vreal = vtype_real(-DBL_MAX, DBL_MAX);
         vcode_type_t rtype = lower_type(tree_type(fcall));
         return emit_cast(rtype, rtype,
                          emit_mul(r0, emit_cast(vreal, vreal, r1)));
      }
   case S_MUL_PR:
   case S_MUL_IR:
      {
         vcode_type_t vreal = vtype_real(-DBL_MAX, DBL_MAX);
         vcode_type_t rtype = lower_type(tree_type(fcall));
         return emit_cast(rtype, rtype,
                          emit_mul(emit_cast(vreal, vreal, r0), r1));
      }
   case S_DIV_PR:
      {
         vcode_type_t vreal = vtype_real(-DBL_MAX, DBL_MAX);
         vcode_type_t rtype = lower_type(tree_type(fcall));
         return emit_cast(rtype, rtype,
                          emit_div(emit_cast(vreal, vreal, r0), r1));
      }
   case S_DIV_RI:
      {
         vcode_type_t vreal = vtype_real(-DBL_MAX, DBL_MAX);
         vcode_type_t rtype = lower_type(tree_type(fcall));
         vcode_reg_t locus = lower_debug_locus(fcall);
         emit_zero_check(r1, locus);
         return emit_cast(rtype, rtype,
                          emit_div(r0, emit_cast(vreal, vreal, r1)));
      }
   default:
      fatal_at(tree_loc(fcall), "cannot lower builtin %d", builtin);
   }
}

static vcode_type_t lower_func_result_type(type_t result)
{
   if (type_is_array(result) && lower_const_bounds(result))
      return vtype_pointer(lower_type(lower_elem_recur(result)));
   else if (type_is_record(result))
      return vtype_pointer(lower_type(result));
   else
      return lower_type(result);
}

static vcode_type_t lower_param_type(type_t type, class_t class,
                                     port_mode_t mode)
{
   switch (class) {
   case C_SIGNAL:
      if (type_is_homogeneous(type))
         return lower_signal_type(type);
      else if (type_is_array(type)) {
         if (lower_const_bounds(type))
            return vtype_pointer(lower_signal_type(lower_elem_recur(type)));
         else
            return lower_signal_type(type);
      }
      else
         return vtype_pointer(lower_signal_type(type));

   case C_VARIABLE:
   case C_DEFAULT:
   case C_CONSTANT:
      {
         if (type_is_array(type)) {
            if (lower_const_bounds(type))
               return vtype_pointer(lower_type(lower_elem_recur(type)));
            else
               return lower_type(type);
         }
         else if (type_is_record(type))
            return vtype_pointer(lower_type(type));
         else if (mode == PORT_OUT || mode == PORT_INOUT)
            return vtype_pointer(lower_type(type));
         else
            return lower_type(type);
      }
      break;

   case C_FILE:
      return vtype_pointer(lower_type(type));

   default:
      fatal_trace("unhandled class %s in lower_param_type", class_str(class));
   }
}

static vcode_cc_t lower_cc_for_call(tree_t call)
{
   tree_t decl = tree_ref(call);
   const subprogram_kind_t skind = tree_subkind(decl);

   if (skind == S_FOREIGN)
      return VCODE_CC_FOREIGN;
   else if (is_builtin(skind))
      return VCODE_CC_PREDEF;
   else
      return VCODE_CC_VHDL;
}

static vcode_reg_t lower_context_for_call(lower_unit_t *lu, ident_t unit_name)
{
   vcode_unit_t caller = vcode_active_unit();

   vcode_state_t state;
   vcode_state_save(&state);

   if (lu->registry != NULL && unit_registry_query(lu->registry, unit_name)) {
      vcode_unit_t vu = unit_registry_get(lu->registry, unit_name);

      vcode_select_unit(vu);
      vcode_unit_t context = vcode_unit_context();

      if (context == caller) {
         vcode_state_restore(&state);
         return emit_context_upref(0);
      }
      else if (context == NULL) {
         assert(vcode_unit_kind() == VCODE_UNIT_THUNK);
         vcode_state_restore(&state);
         return emit_null(vtype_context(unit_name));
      }

      vcode_select_unit(context);

      ident_t context_name = vcode_unit_name();
      vunit_kind_t context_kind = vcode_unit_kind();
      vcode_state_restore(&state);

      if (context_kind == VCODE_UNIT_PACKAGE) {
         if (vcode_unit_kind() == VCODE_UNIT_THUNK)
            return emit_package_init(context_name, VCODE_INVALID_REG);
         else if (context == vcode_unit_context())
            return emit_context_upref(1);
         else
            return emit_link_package(context_name);
      }
      else {
         int hops = 0;
         for (; vcode_active_unit() != context; hops++) {
            vcode_unit_t up = vcode_unit_context();
            if (up == NULL) {
               vcode_state_restore(&state);
               return emit_null(vtype_context(context_name));
            }

            vcode_select_unit(up);
         }

         vcode_state_restore(&state);
         return emit_context_upref(hops);
      }
   }

   vcode_state_restore(&state);

   int hops = 0;
   for (; ; hops++) {
      ident_t this = vcode_unit_name();
      assert(this != unit_name);
      if (ident_starts_with(unit_name, this)
          && ident_char(unit_name, ident_len(this)) == '.') {
         vcode_state_restore(&state);
         return emit_context_upref(hops);
      }

      vcode_unit_t context = vcode_unit_context();
      if (context == NULL)
         break;

      vcode_select_unit(context);
   }

   vcode_state_restore(&state);

   ident_t scope_name = ident_runtil(ident_until(unit_name, '('), '.');
   tree_t pack = lib_get_qualified(scope_name);
   if (pack != NULL && is_package(pack)) {
      assert(!is_uninstantiated_package(pack));
      if (vcode_unit_kind() == VCODE_UNIT_THUNK)
         return emit_package_init(scope_name, VCODE_INVALID_REG);
      else
         return emit_link_package(scope_name);
   }
   else  // Call to function defined in architecture
      return emit_null(vtype_context(scope_name));
}

static vcode_reg_t lower_fcall(lower_unit_t *lu, tree_t fcall,
                               vcode_reg_t bounds_reg)
{
   tree_t decl = tree_ref(fcall);

   const subprogram_kind_t kind = tree_subkind(decl);
   if (is_open_coded_builtin(kind))
      return lower_builtin(lu, fcall, kind, NULL, NULL);

   const int nparams = tree_params(fcall);
   SCOPED_A(vcode_reg_t) args = AINIT;

   const vcode_cc_t cc = lower_cc_for_call(fcall);
   ident_t name = tree_ident2(decl);

   if (tree_kind(fcall) == T_PROT_FCALL && tree_has_name(fcall))
      APUSH(args, lower_rvalue(lu, tree_name(fcall)));
   else if (cc != VCODE_CC_FOREIGN)
      APUSH(args, lower_context_for_call(lu, name));

   for (int i = 0; i < nparams; i++) {
      vcode_reg_t arg_reg = lower_subprogram_arg(lu, fcall, i);
      if (kind == S_FOREIGN && vcode_reg_kind(arg_reg) == VCODE_TYPE_UARRAY) {
         // Do not pass wrapped arrays into foreign functions
         APUSH(args, emit_unwrap(arg_reg));

         vcode_type_t vint64 = vtype_int(INT64_MIN, INT64_MAX);
         const int ndims = vtype_dims(vcode_reg_type(arg_reg));
         for (int i = 0; i < ndims; i++) {
            vcode_reg_t len_reg = emit_uarray_len(arg_reg, 0);
            APUSH(args, emit_cast(vint64, vint64, len_reg));
         }
      }
      else
         APUSH(args, arg_reg);
   }

   if (bounds_reg != VCODE_INVALID_REG)
      APUSH(args, bounds_reg);

   type_t result = tree_type(fcall);
   vcode_type_t rtype = lower_func_result_type(result);
   vcode_type_t rbounds = lower_bounds(result);

   if (cover_enabled(lu->cover, COVER_MASK_EXPRESSION))
      lower_logic_expr_coverage(lu, fcall, decl, args.items);

   return emit_fcall(name, rtype, rbounds, cc, args.items, args.count);
}

static vcode_reg_t lower_known_subtype(lower_unit_t *lu, tree_t value,
                                       type_t type, vcode_reg_t bounds_reg)
{
   if (tree_kind(value) == T_FCALL
       && (tree_flags(tree_ref(value)) & TREE_F_KNOWS_SUBTYPE)) {

      if (type_is_array(type)) {
         if (bounds_reg == VCODE_INVALID_REG
             || vcode_reg_kind(bounds_reg) != VCODE_TYPE_UARRAY) {
            vcode_type_t velem = lower_type(lower_elem_recur(type));
            vcode_reg_t null_reg = emit_null(vtype_pointer(velem));
            bounds_reg = lower_wrap(lu, type, null_reg);
         }
      }
      else if (type_is_record(type)) {
         if (bounds_reg == VCODE_INVALID_REG) {
            // This is inefficient but should only occur in declarations
            bounds_reg = lower_default_value(lu, type, VCODE_INVALID_REG);
         }
         else if (vtype_is_signal(vcode_reg_type(bounds_reg)))
            bounds_reg = lower_resolved(lu, type, bounds_reg);
      }

      return lower_fcall(lu, value, bounds_reg);
   }
   else
      return lower_rvalue(lu, value);
}

static vcode_reg_t *lower_string_literal_chars(tree_t lit, int *nchars)
{
   type_t ltype = tree_type(lit);
   vcode_type_t vtype = lower_type(type_elem(ltype));

   *nchars = tree_chars(lit);
   vcode_reg_t *tmp = xmalloc_array(*nchars, sizeof(vcode_reg_t));

   for (int i = 0; i < *nchars; i++)
      tmp[i] = emit_const(vtype, tree_pos(tree_ref(tree_char(lit, i))));

   return tmp;
}

static vcode_reg_t lower_string_literal(tree_t lit, bool nest)
{
   int nchars;
   vcode_reg_t *tmp LOCAL = lower_string_literal_chars(lit, &nchars);

   type_t type = tree_type(lit);
   if (type_is_array(type) && !lower_const_bounds(type)) {
      vcode_type_t elem = lower_type(type_elem(type));
      vcode_type_t array_type = vtype_carray(nchars, elem, elem);
      vcode_reg_t data = emit_const_array(array_type, tmp, nchars);

      vcode_dim_t dim0 = {
         .left  = emit_const(vtype_offset(), 1),
         .right = emit_const(vtype_offset(), nchars),
         .dir   = emit_const(vtype_bool(), RANGE_TO)
      };
      return emit_wrap(emit_address_of(data), &dim0, 1);
   }
   else {
      vcode_reg_t array = emit_const_array(lower_type(type), tmp, nchars);
      return nest ? array : emit_address_of(array);
   }
}

static vcode_reg_t lower_literal(tree_t lit)
{
   switch (tree_subkind(lit)) {
   case L_PHYSICAL:
      assert(!tree_has_ref(lit));
      // Fall-through
   case L_INT:
      return emit_const(lower_type(tree_type(lit)), tree_ival(lit));

   case L_NULL:
      return emit_null(lower_type(tree_type(lit)));

   case L_REAL:
      return emit_const_real(lower_type(tree_type(lit)), tree_dval(lit));

   default:
      fatal_at(tree_loc(lit), "cannot lower literal kind %d",
               tree_subkind(lit));
   }
}

int lower_search_vcode_obj(void *key, lower_unit_t *scope, int *hops)
{
   *hops = 0;
   for (; scope != NULL; scope = scope->parent) {
      const void *ptr = hash_get(scope->objects, key);
      const int obj = (uintptr_t)ptr - 1;
      if (obj != VCODE_INVALID_REG)
         return obj;
      (*hops)++;
   }

   *hops = 0;
   return VCODE_INVALID_REG;
}

void lower_put_vcode_obj(void *key, int obj, lower_unit_t *scope)
{
   hash_put(scope->objects, key, (void *)(uintptr_t)(obj + 1));
}

static vcode_var_t lower_get_var(lower_unit_t *lu, tree_t decl, int *hops)
{
   return lower_search_vcode_obj(decl, lu, hops);
}

static vcode_type_t lower_var_type(tree_t decl)
{
   type_t type = tree_type(decl);

   if (tree_kind(decl) == T_ALIAS)
      return lower_alias_type(decl);
   else if (class_of(decl) == C_SIGNAL)
      return lower_signal_type(type);
   else if (type_is_array(type) && lower_const_bounds(type))
      return lower_type(lower_elem_recur(type));
   else
      return lower_type(type);
}

static vcode_reg_t lower_link_var(lower_unit_t *lu, tree_t decl)
{
   tree_t container = tree_container(decl);
   const tree_kind_t kind = tree_kind(container);
   vcode_reg_t context = VCODE_INVALID_REG;
   vcode_type_t vtype = lower_var_type(decl);

   if (kind != T_PACKAGE && kind != T_PACK_INST) {
      if (lu->mode == LOWER_THUNK)
         return emit_undefined(vtype_pointer(vtype), vtype);
      else {
         vcode_dump();
         fatal_trace("invalid container kind %s for %s", tree_kind_str(kind),
                     istr(tree_ident(decl)));
      }
   }
   else if (lu->mode == LOWER_THUNK && lu->parent == NULL
            && tree_has_value(decl)) {
      // Handle a special case of simplifying locally static expressions
      // that reference constant declarations
      assert(tree_kind(decl) == T_CONST_DECL);

      type_t type = tree_type(decl);
      tree_t value = tree_value(decl);
      vcode_reg_t value_reg = lower_rvalue(lu, value), ptr_reg;

      switch (vcode_reg_kind(value_reg)) {
      case VCODE_TYPE_POINTER:
      case VCODE_TYPE_UARRAY:
         ptr_reg = value_reg;
         break;
      default:
         {
            vcode_type_t vtype = lower_type(type);
            vcode_type_t vbounds = lower_bounds(type);
            vcode_var_t var = emit_var(vtype, vbounds, tree_ident(decl), 0);
            lower_put_vcode_obj(decl, var, lu);
            emit_store(value_reg, var);

            ptr_reg = emit_index(var, VCODE_INVALID_REG);
         }
         break;
      }

      if (type_is_array(type))
         return lower_coerce_arrays(lu, tree_type(value), type, ptr_reg);
      else
         return ptr_reg;
   }

   assert(!is_uninstantiated_package(container));

   if (lu->mode == LOWER_THUNK)
      context = emit_package_init(tree_ident(container), VCODE_INVALID_REG);
   else
      context = emit_link_package(tree_ident(container));

   vcode_reg_t ptr_reg = emit_link_var(context, tree_ident(decl), vtype);
   if (have_uarray_ptr(ptr_reg))
      return emit_load_indirect(ptr_reg);
   else
      return ptr_reg;
}

static vcode_reg_t lower_var_ref(lower_unit_t *lu, tree_t decl, expr_ctx_t ctx)
{
   type_t type = tree_type(decl);

   vcode_reg_t ptr_reg = VCODE_INVALID_REG;
   int hops = 0;
   vcode_var_t var = lower_get_var(lu, decl, &hops);
   if (var == VCODE_INVALID_VAR)
      ptr_reg = lower_link_var(lu, decl);   // External variable
   else if (var & INSTANCE_BIT) {
      // This variable is declared in an instantiated package
      vcode_var_t pkg_var = var & ~INSTANCE_BIT;
      vcode_reg_t pkg_reg;
      if (hops == 0)
         pkg_reg = emit_load(pkg_var);
      else
         pkg_reg = emit_load_indirect(emit_var_upref(hops, pkg_var));

      vcode_type_t vtype = lower_var_type(decl);
      ptr_reg = emit_link_var(pkg_reg, tree_ident(decl), vtype);
   }
   else if (hops > 0)
      ptr_reg = emit_var_upref(hops, var);

   if (ptr_reg != VCODE_INVALID_REG) {
      if (ctx == EXPR_LVALUE)
         return ptr_reg;
      else if (type_is_scalar(type))
         return emit_load_indirect(ptr_reg);
      else if (have_uarray_ptr(ptr_reg))
         return emit_load_indirect(ptr_reg);
      else
         return ptr_reg;
   }
   else if (type_is_array(type) && lower_const_bounds(type))
      return emit_index(var, VCODE_INVALID_REG);
   else if (type_is_record(type) || type_is_protected(type))
      return emit_index(var, VCODE_INVALID_REG);
   else if ((type_is_scalar(type) || type_is_file(type) || type_is_access(type))
            && ctx == EXPR_LVALUE)
      return emit_index(var, VCODE_INVALID_REG);
   else
      return emit_load(var);
}

static vcode_reg_t lower_signal_ref(lower_unit_t *lu, tree_t decl)
{
   type_t type = tree_type(decl);

   if (lu->mode == LOWER_THUNK)
      return emit_undefined(lower_signal_type(type), lower_bounds(type));

   int hops = 0;
   vcode_var_t var = lower_search_vcode_obj(decl, lu, &hops);

   if (var == VCODE_INVALID_VAR || (var & INSTANCE_BIT)) {
      // Link to external package signal
      vcode_reg_t ptr;
      if (var == VCODE_INVALID_VAR)
         ptr = lower_link_var(lu, decl);
      else {
         // This signal is declared in an instantiated package
         vcode_var_t pkg_var = var & ~INSTANCE_BIT;
         vcode_reg_t pkg_reg;
         if (hops == 0)
            pkg_reg = emit_load(pkg_var);
         else
            pkg_reg = emit_load_indirect(emit_var_upref(hops, pkg_var));

         vcode_type_t vtype = lower_signal_type(type);
         ptr = emit_link_var(pkg_reg, tree_ident(decl), vtype);
      }

      if (!type_is_homogeneous(type))
         return ptr;
      else if (vcode_reg_kind(ptr) == VCODE_TYPE_UARRAY)
         return ptr;
      else
         return emit_load_indirect(ptr);
   }
   else if (hops == 0) {
      if (!type_is_homogeneous(type))
         return emit_index(var, VCODE_INVALID_REG);
      else
         return emit_load(var);
   }
   else if (!type_is_homogeneous(type))
      return emit_var_upref(hops, var);
   else
      return emit_load_indirect(emit_var_upref(hops, var));
}

static vcode_reg_t lower_port_ref(lower_unit_t *lu, tree_t decl)
{
   int hops = 0;
   int obj = lower_search_vcode_obj(decl, lu, &hops);

   if (lu->mode == LOWER_THUNK) {
      type_t type = tree_type(decl);
      emit_comment("Cannot resolve reference to %s", istr(tree_ident(decl)));
      return emit_undefined(lower_signal_type(type), lower_bounds(type));
   }
   else if (obj != VCODE_INVALID_VAR) {
      vcode_var_t var = obj;
      if (!type_is_homogeneous(tree_type(decl))) {
         if (hops == 0)
            return emit_index(var, VCODE_INVALID_REG);
         else
            return emit_var_upref(hops, var);
      }
      else if (hops == 0)
         return emit_load(var);
      else
         return emit_load_indirect(emit_var_upref(hops, var));
   }
   else {
      vcode_dump();
      fatal_trace("missing variable for port %s", istr(tree_ident(decl)));
   }
}

static vcode_reg_t lower_param_ref(lower_unit_t *lu, tree_t decl)
{
   int hops = 0;
   int obj = lower_search_vcode_obj(decl, lu, &hops);

   const bool is_proc_var = (obj != -1 && !!(obj & PARAM_VAR_BIT));
   obj &= ~PARAM_VAR_BIT;

   if (hops > 0) {
      // Reference to parameter in parent subprogram
      return emit_load_indirect(emit_var_upref(hops, obj));
   }
   else if (is_proc_var)
      return emit_load(obj);
   else {
      vcode_reg_t reg = obj;
      const bool undefined_in_thunk =
         lu->mode == LOWER_THUNK && (reg == VCODE_INVALID_REG
                                     || tree_class(decl) == C_SIGNAL
                                     || type_is_protected(tree_type(decl)));
      if (undefined_in_thunk) {
         type_t type = tree_type(decl);
         emit_comment("Cannot resolve reference to %s", istr(tree_ident(decl)));
         if (tree_class(decl) == C_SIGNAL)
            return emit_undefined(lower_signal_type(type), lower_bounds(type));
         else {
            vcode_type_t vtype = lower_type(tree_type(decl));
            if (vtype_kind(vtype) == VCODE_TYPE_RECORD)
               return emit_undefined(vtype_pointer(vtype), lower_bounds(type));
            else
               return emit_undefined(vtype, lower_bounds(type));
         }
      }
      else if (reg == VCODE_INVALID_REG) {
         vcode_dump();
         fatal_trace("missing register for parameter %s",
                     istr(tree_ident(decl)));
      }

      return reg;
   }
}

static vcode_reg_t lower_generic_ref(lower_unit_t *lu, tree_t decl,
                                     expr_ctx_t ctx)
{
   type_t type = tree_type(decl);

   int hops = 0;
   vcode_var_t var = lower_search_vcode_obj(decl, lu, &hops);
   vcode_reg_t ptr_reg = VCODE_INVALID_REG;

   if (var == VCODE_INVALID_VAR) {
      tree_t unit;
      if (tree_kind((unit = tree_container(decl))) == T_PACK_INST) {
         vcode_reg_t context = emit_link_package(tree_ident(unit));
         ptr_reg = emit_link_var(context, tree_ident(decl), lower_type(type));
      }
      else if (vcode_unit_kind() == VCODE_UNIT_INSTANCE) {
         // This can happen when a type contains a reference to a
         // component generic. The elaborator does not currently rewrite
         // it to point at the corresponding entity generic.

         var = vcode_find_var(tree_ident(decl));
         assert(var != VCODE_INVALID_VAR);
      }
      else if (lu->mode == LOWER_THUNK) {
         type_t type = tree_type(decl);
         emit_comment("Cannot resolve generic %s", istr(tree_ident(decl)));
         return emit_undefined(lower_type(type), lower_bounds(type));
      }
      else {
         vcode_dump();
         fatal_trace("missing variable for generic %s in %s",
                     istr(tree_ident(decl)), istr(tree_ident(unit)));
      }
   }

   if (hops > 0)
      ptr_reg = emit_var_upref(hops, var);

   if (ptr_reg != VCODE_INVALID_REG) {
      if (type_is_scalar(type))
         return emit_load_indirect(ptr_reg);
      else if (type_is_array(type) && !lower_const_bounds(type))
         return emit_load_indirect(ptr_reg);
      else
         return ptr_reg;
   }
   else if (type_is_array(type) && lower_const_bounds(type))
      return emit_index(var, VCODE_INVALID_REG);
   else if (type_is_record(type) || type_is_protected(type))
      return emit_index(var, VCODE_INVALID_REG);
   else
      return emit_load(var);
}

static vcode_reg_t lower_alias_ref(lower_unit_t *lu, tree_t alias,
                                   expr_ctx_t ctx)
{
   tree_t value = tree_value(alias);
   type_t type = tree_type(value);

   if (!type_is_array(type))
      return lower_expr(lu, tree_value(alias), ctx);

   int hops = 0;
   vcode_var_t var = lower_get_var(lu, alias, &hops);
   if (var == VCODE_INVALID_VAR) {
      if (lu->mode == LOWER_THUNK)
         return emit_undefined(lower_type(type), lower_bounds(type));
      else {
         // External alias variable
         return lower_link_var(lu, alias);
      }
   }

   vcode_state_t state;
   vcode_state_save(&state);

   for (int i = 0; i < hops; i++)
      vcode_select_unit(vcode_unit_context());

   vcode_state_restore(&state);

   if (hops == 0)
      return emit_load(var);
   else
      return emit_load_indirect(emit_var_upref(hops, var));
}

static bool lower_is_trivial_constant(tree_t decl)
{
   if (!type_is_scalar(tree_type(decl)))
      return false;
   else if (!tree_has_value(decl))
      return false;
   else
      return tree_kind(tree_value(decl)) == T_LITERAL;
}

static vcode_reg_t lower_signal_dummy_rvalue(lower_unit_t *lu, tree_t decl)
{
   // This is a workaround to allow referencing signal initial values
   // during elaboration with --relaxed
   if (tree_has_value(decl))
      return lower_expr(lu, tree_value(decl), EXPR_RVALUE);
   else {
      type_t type = tree_type(decl);
      return lower_default_value(lu, type, VCODE_INVALID_REG);
   }
}

static vcode_reg_t lower_ref(lower_unit_t *lu, tree_t ref, expr_ctx_t ctx)
{
   tree_t decl = tree_ref(ref);

   tree_kind_t kind = tree_kind(decl);
   switch (kind) {
   case T_ENUM_LIT:
      if (ctx == EXPR_LVALUE)
         return VCODE_INVALID_REG;
      else
         return emit_const(lower_type(tree_type(decl)), tree_pos(decl));

   case T_VAR_DECL:
   case T_FILE_DECL:
      return lower_var_ref(lu, decl, ctx);

   case T_PORT_DECL:
      if (ctx == EXPR_RVALUE && lu->elaborating)
         return lower_signal_dummy_rvalue(lu, decl);
      else
         return lower_port_ref(lu, decl);

   case T_PARAM_DECL:
      return lower_param_ref(lu, decl);

   case T_GENERIC_DECL:
      return lower_generic_ref(lu, decl, ctx);

   case T_SIGNAL_DECL:
   case T_IMPLICIT_SIGNAL:
      if (ctx == EXPR_RVALUE && lu->elaborating)
         return lower_signal_dummy_rvalue(lu, decl);
      else
         return lower_signal_ref(lu, decl);

   case T_TYPE_DECL:
      return VCODE_INVALID_REG;

   case T_CONST_DECL:
      if (ctx == EXPR_LVALUE)
         return VCODE_INVALID_REG;
      else if (lower_is_trivial_constant(decl))
         return lower_expr(lu, tree_value(decl), ctx);
      else
         return lower_var_ref(lu, decl, ctx);

   case T_UNIT_DECL:
      return lower_expr(lu, tree_value(decl), ctx);

   case T_ALIAS:
      return lower_alias_ref(lu, decl, ctx);

   case T_FUNC_BODY:
      // Used to implement the "function knows result size" feature
      assert(standard() >= STD_19);
      assert(tree_flags(decl) & TREE_F_KNOWS_SUBTYPE);
      return lower_param_ref(lu, decl);

   default:
      vcode_dump();
      fatal_trace("cannot lower reference to %s", tree_kind_str(kind));
   }
}

static vcode_reg_t lower_external_name(tree_t ref, expr_ctx_t ctx)
{
   if (!tree_has_ref(ref))
      fatal_at(tree_loc(ref), "sorry, external names in packages are not "
               "yet supported");

   ident_t path = NULL;
   const int nparts = tree_parts(ref);
   for (int i = 0; i < nparts - 1; i++) {
      tree_t pe = tree_part(ref, i);
      assert(tree_subkind(pe) == PE_SIMPLE);
      if (tree_has_ident(pe))
         path = ident_prefix(path, tree_ident(pe), '.');
   }

   vcode_reg_t locus = lower_debug_locus(ref);
   vcode_reg_t context = emit_link_instance(path, locus);

   tree_t decl = tree_ref(ref);
   type_t type = tree_type(decl);
   vcode_type_t vtype = lower_var_type(decl);

   vcode_reg_t ptr_reg = emit_link_var(context, tree_ident(decl), vtype);
   if (have_uarray_ptr(ptr_reg))
      return emit_load_indirect(ptr_reg);
   else if (tree_class(ref) == C_SIGNAL && type_is_homogeneous(type))
      return emit_load_indirect(ptr_reg);
   else
      return ptr_reg;
}

static type_t lower_base_type(type_t type)
{
   for (type_t base; type_kind(type) == T_SUBTYPE; type = base) {
      base = type_base(type);
      if (type_ident(base) == type_ident(type))
         break;   // Subtype created for constrained array definition
   }

   return type;
}

static vcode_reg_t lower_resolved(lower_unit_t *lu, type_t type,
                                  vcode_reg_t reg)
{
   if (!lower_have_signal(reg))
      return reg;
   else if (have_uarray_ptr(reg))
      reg = emit_load_indirect(reg);

   if (type_is_homogeneous(type)) {
      vcode_reg_t data_reg;
      if (vcode_reg_kind(reg) == VCODE_TYPE_POINTER)
         data_reg = emit_resolved(emit_load_indirect(reg));
      else
         data_reg = emit_resolved(lower_array_data(reg));

      if (vcode_reg_kind(reg) == VCODE_TYPE_UARRAY)
         return lower_rewrap(data_reg, reg);
      else
         return data_reg;
   }
   else {
      // Use a helper function to convert a record signal into a record
      // containing the resolved values

      type_t base = lower_base_type(type);
      ident_t helper_func =
         ident_prefix(type_ident(base), ident_new("resolved"), '$');

      vcode_reg_t arg_reg = reg;
      if (type_is_array(type)) {
         if (have_uarray_ptr(arg_reg))
            arg_reg = emit_load_indirect(arg_reg);
         else if (vcode_reg_kind(arg_reg) != VCODE_TYPE_UARRAY)
            arg_reg = lower_wrap(lu, type, reg);
      }

      vcode_type_t vrtype = lower_func_result_type(base);

      vcode_reg_t args[] = { lower_context_for_call(lu, helper_func), arg_reg };
      return emit_fcall(helper_func, vrtype, vrtype, VCODE_CC_VHDL, args, 2);
   }
}

static vcode_reg_t lower_array_off(lower_unit_t *lu, vcode_reg_t off,
                                   vcode_reg_t array, type_t type,
                                   unsigned dim)
{
   // Convert VHDL offset 'off' to a zero-based array offset

   assert(vtype_kind(vcode_reg_type(off)) == VCODE_TYPE_INT);

   vcode_reg_t zeroed = VCODE_INVALID_REG;
   if (have_array_metadata(type, array)) {
      vcode_reg_t left_reg = lower_array_left(lu, type, dim, array);

      vcode_reg_t downto = emit_sub(left_reg, off);
      vcode_reg_t upto   = emit_sub(off, left_reg);
      zeroed = emit_select(emit_uarray_dir(array, dim), downto, upto);
   }
   else {
      tree_t r = range_of(type, dim);
      vcode_reg_t left = lower_range_left(lu, r);
      switch (tree_subkind(r)) {
      case RANGE_TO:
         zeroed = emit_sub(off, left);
         break;
      case RANGE_DOWNTO:
         zeroed = emit_sub(left, off);
         break;
      case RANGE_EXPR:
         {
            vcode_reg_t dir = lower_range_dir(lu, r);
            vcode_reg_t to = emit_sub(off, left);
            vcode_reg_t downto = emit_sub(left, off);
            zeroed = emit_select(dir, downto, to);
         }
         break;
      }
   }

   return emit_cast(vtype_offset(), VCODE_INVALID_TYPE, zeroed);
}

static vcode_reg_t lower_array_ref(lower_unit_t *lu, tree_t ref, expr_ctx_t ctx)
{
   tree_t value = tree_value(ref);

   vcode_reg_t array = lower_expr(lu, value, ctx);
   if (array == VCODE_INVALID_REG)
      return array;

   if (have_uarray_ptr(array))
      array = emit_load_indirect(array);

   DEBUG_ONLY({
         const vtype_kind_t vtkind = vtype_kind(vcode_reg_type(array));
         assert(vtkind == VCODE_TYPE_POINTER || vtkind == VCODE_TYPE_UARRAY
                || vtkind == VCODE_TYPE_SIGNAL);
      });

   type_t value_type = tree_type(value);

   const bool elide_bounds = tree_flags(ref) & TREE_F_ELIDE_BOUNDS;

   vcode_reg_t offset_reg = emit_const(vtype_offset(), 0);
   const int nparams = tree_params(ref);
   for (int i = 0; i < nparams; i++) {
      tree_t p = tree_param(ref, i);
      assert(tree_subkind(p) == P_POS);

      tree_t index = tree_value(p);
      vcode_reg_t index_reg = lower_rvalue(lu, index);

      if (!elide_bounds) {
         vcode_reg_t left_reg  = lower_array_left(lu, value_type, i, array);
         vcode_reg_t right_reg = lower_array_right(lu, value_type, i, array);
         vcode_reg_t dir_reg   = lower_array_dir(lu, value_type, i, array);

         vcode_reg_t locus = lower_debug_locus(index);
         emit_index_check(index_reg, left_reg, right_reg, dir_reg,
                          locus, locus);
      }

      if (i > 0) {
         vcode_reg_t stride = lower_array_len(lu, value_type, i, array);
         offset_reg = emit_mul(offset_reg, stride);
      }

      vcode_reg_t zerored_reg =
         lower_array_off(lu, index_reg, array, value_type, i);
      offset_reg = emit_add(offset_reg, zerored_reg);
   }

   vcode_reg_t stride_reg = lower_array_stride(lu, value_type, array);
   offset_reg = emit_mul(offset_reg, stride_reg);

   vcode_reg_t data_reg = lower_array_data(array);
   vcode_reg_t ptr_reg = emit_array_ref(data_reg, offset_reg);

   type_t elem_type = tree_type(ref);
   if (type_is_array(elem_type) && !lower_const_bounds(elem_type))
      return lower_wrap_element(lu, value_type, array, ptr_reg);
   else
      return ptr_reg;
}

static vcode_reg_t lower_array_slice(lower_unit_t *lu, tree_t slice,
                                     expr_ctx_t ctx)
{
   tree_t value = tree_value(slice);
   tree_t r     = tree_range(slice, 0);
   type_t type  = tree_type(value);

   vcode_reg_t left_reg  = lower_range_left(lu, r);
   vcode_reg_t right_reg = lower_range_right(lu, r);
   vcode_reg_t kind_reg  = lower_range_dir(lu, r);
   vcode_reg_t null_reg  = emit_range_null(left_reg, right_reg, kind_reg);
   vcode_reg_t array_reg = lower_expr(lu, value, ctx);

   int64_t null_const;
   const bool known_not_null =
      vcode_reg_const(null_reg, &null_const) && null_const == 0;

   vcode_block_t after_bounds_bb = VCODE_INVALID_BLOCK;
   if (!known_not_null) {
      vcode_block_t not_null_bb = emit_block();
      after_bounds_bb = emit_block();
      emit_cond(null_reg, after_bounds_bb, not_null_bb);

      vcode_select_block(not_null_bb);
   }

   vcode_reg_t aleft_reg  = lower_array_left(lu, type, 0, array_reg);
   vcode_reg_t aright_reg = lower_array_right(lu, type, 0, array_reg);
   vcode_reg_t adir_reg   = lower_array_dir(lu, type, 0, array_reg);

   vcode_reg_t locus = lower_debug_locus(r);
   emit_index_check(left_reg, aleft_reg, aright_reg, adir_reg,
                    locus, locus);
   emit_index_check(right_reg, aleft_reg, aright_reg, adir_reg,
                    locus, locus);

   if (!known_not_null) {
      emit_jump(after_bounds_bb);
      vcode_select_block(after_bounds_bb);
   }

   if (array_reg == VCODE_INVALID_REG)
      return VCODE_INVALID_REG;

   vcode_reg_t stride_reg = lower_array_stride(lu, type, array_reg);

   vcode_reg_t data_reg = lower_array_data(array_reg);
   vcode_reg_t off_reg = lower_array_off(lu, left_reg, array_reg, type, 0);
   vcode_reg_t ptr_reg =
      emit_array_ref(data_reg, emit_mul(off_reg, stride_reg));

   if (lower_const_bounds(type))
      return ptr_reg;

   vcode_dim_t dim0 = {
      .left  = left_reg,
      .right = right_reg,
      .dir   = kind_reg
   };

   const int ndims = dims_for_type(type);
   if (ndims > 1) {
      assert(vcode_reg_kind(array_reg) == VCODE_TYPE_UARRAY);

      vcode_dim_t *dims LOCAL = xmalloc_array(ndims, sizeof(vcode_dim_t));
      dims[0] = dim0;
      for (int i = 1; i < ndims; i++) {
         dims[i].left  = emit_uarray_left(array_reg, i);
         dims[i].right = emit_uarray_right(array_reg, i);
         dims[i].dir   = emit_uarray_dir(array_reg, i);
      }

      return emit_wrap(ptr_reg, dims, ndims);
   }
   else
      return emit_wrap(ptr_reg, &dim0, 1);
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

static vcode_reg_t *lower_const_array_aggregate(lower_unit_t *lu, tree_t t,
                                                type_t type, int dim,
                                                int *n_elems)
{
   if ((*n_elems = lower_array_const_size(type)) == 0)
      return NULL;

   vcode_reg_t *vals = xmalloc_array(*n_elems, sizeof(vcode_reg_t));

   for (int i = 0; i < *n_elems; i++)
      vals[i] = VCODE_INVALID_VAR;

   tree_t r = range_of(type, dim);
   const int64_t left = assume_int(tree_left(r));
   const bool is_downto = (tree_subkind(r) == RANGE_DOWNTO);
   const bool multidim = dim > 0 || dimension_of(type) > 1;

   const int nassocs = tree_assocs(t);
   for (int i = 0; i < nassocs; i++) {
      tree_t a = tree_assoc(t, i);
      tree_t value = tree_value(a);
      type_t value_type = tree_type(value);

      const tree_kind_t value_kind = tree_kind(value);

      vcode_reg_t tmp = VCODE_INVALID_REG;
      vcode_reg_t *sub = &tmp;
      int nsub = 1;
      if (value_kind == T_AGGREGATE) {
         if (type_is_array(value_type))
            sub = lower_const_array_aggregate(lu, value, value_type, 0, &nsub);
         else if (type_is_record(value_type))
            *sub = lower_record_aggregate(lu, value, true,
                                          lower_is_const(value),
                                          VCODE_INVALID_REG);
         else
            assert(false);
      }
      else if (value_kind == T_STRING)
         sub = lower_string_literal_chars(value, &nsub);
      else
         *sub = lower_rvalue(lu, value);

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
            range_bounds(tree_range(a, 0), &r_low, &r_high);

            if (!multidim && type_eq(type, value_type)) {
               // Element has same type as whole aggregate
               assert(standard() >= STD_08);
               assert(nsub == r_high - r_low + 1);
               const int64_t off = is_downto ? left - r_low : r_low - left;
               lower_copy_vals(vals + off * 1, sub, nsub, false);
            }
            else {
               for (int j = r_low; j <= r_high; j++) {
                  const int64_t off = is_downto ? left - j : j - left;
                  lower_copy_vals(vals + (off * nsub), sub, nsub, false);
               }
            }
         }
         break;
      }

      if (sub != &tmp)
         free(sub);
   }

   if (lu->mode == LOWER_THUNK DEBUG_ONLY(|| true)) {
      // We may attempt to evaluate a locally static expression that
      // references this array before the bounds checker has run
      for (int i = 0; i < *n_elems; i++) {
         if (vals[i] != VCODE_INVALID_REG)
            continue;
         else if (lu->mode == LOWER_THUNK) {
            type_t elem = type_elem(type);
            vals[i] = emit_undefined(lower_type(elem), lower_bounds(elem));
         }
         else
            fatal_trace("missing constant array element %d", i);
      }
   }

   return vals;
}

static vcode_reg_t lower_record_sub_aggregate(lower_unit_t *lu, tree_t value,
                                              tree_t field, bool is_const)
{
   type_t ftype = tree_type(field);

   if (is_const && type_is_array(ftype)) {
      if (tree_kind(value) == T_STRING)
         return lower_string_literal(value, true);
      else if (lu->mode == LOWER_THUNK && !lower_const_bounds(ftype))
         return emit_undefined(lower_type(ftype), lower_bounds(ftype));
      else {
         int nvals;
         vcode_reg_t *values LOCAL =
            lower_const_array_aggregate(lu, value, ftype, 0, &nvals);
         return emit_const_array(lower_type(ftype), values, nvals);
      }
   }
   else if (is_const && type_is_record(ftype))
      return lower_record_aggregate(lu, value, true, true, VCODE_INVALID_REG);
   else if (type_is_scalar(ftype) || type_is_access(ftype))
      return lower_rvalue(lu, value);
   else if (type_is_array(ftype)) {
      vcode_reg_t value_reg = lower_rvalue(lu, value);
      if (!type_is_unconstrained(ftype)) {
         vcode_reg_t locus = lower_debug_locus(field);
         lower_check_array_sizes(lu, ftype, tree_type(value),
                                 VCODE_INVALID_REG, value_reg, locus);
      }
      return value_reg;
   }
   else
      return lower_rvalue(lu, value);
}

static vcode_reg_t lower_record_aggregate(lower_unit_t *lu, tree_t expr,
                                          bool nest, bool is_const,
                                          vcode_reg_t hint)
{
   type_t type = tree_type(expr);
   const int nfields = type_fields(type);
   const int nassocs = tree_assocs(expr);

   vcode_reg_t *vals LOCAL = xcalloc_array(nfields, sizeof(vcode_reg_t));
   for (int i = 0; i < nfields; i++)
      vals[i] = VCODE_INVALID_REG;

   tree_t *map LOCAL = xcalloc_array(nfields, sizeof(tree_t));

   for (int i = 0; i < nassocs; i++) {
      tree_t a = tree_assoc(expr, i);
      tree_t value = tree_value(a);

      switch (tree_subkind(a)) {
      case A_POS:
         {
            const int pos = tree_pos(a);
            tree_t field = type_field(type, pos);
            vals[pos] = lower_record_sub_aggregate(lu, value, field, is_const);
            map[pos] = value;
         }
         break;

      case A_NAMED:
         {
            const int pos = tree_pos(tree_ref(tree_name(a)));
            tree_t field = type_field(type, pos);
            vals[pos] = lower_record_sub_aggregate(lu, value, field, is_const);
            map[pos] = value;
         }
         break;

      case A_OTHERS:
         for (int j = 0; j < nfields; j++) {
            if (vals[j] == VCODE_INVALID_REG) {
               tree_t field = type_field(type, j);
               vals[j] = lower_record_sub_aggregate(lu, value, field, is_const);
               map[j] = value;
            }
         }
         break;

      case A_RANGE:
         fatal_trace("unexpected range association in record aggregate");
      }
   }

   for (int i = 0; i < nfields; i++)
      assert(vals[i] != VCODE_INVALID_REG);

   if (is_const) {
      vcode_reg_t reg = emit_const_record(lower_type(type), vals, nfields);
      return nest ? reg : emit_address_of(reg);
   }
   else {
      vcode_type_t vtype = lower_type(type);
      vcode_reg_t mem_reg = hint;
      if (mem_reg == VCODE_INVALID_REG) {
         vcode_var_t tmp_var = lower_temp_var(lu, "record", vtype, vtype);
         mem_reg = emit_index(tmp_var, VCODE_INVALID_REG);
      }

      for (int i = 0; i < nfields; i++) {
         tree_t f = type_field(type, i), cons;
         type_t ftype = tree_type(f);
         vcode_reg_t ptr_reg = emit_record_ref(mem_reg, i);
         if (type_is_array(ftype)) {
            if (lower_const_bounds(ftype)) {
               vcode_reg_t src_reg = lower_array_data(vals[i]);
               vcode_reg_t length_reg =
                  lower_array_total_len(lu, ftype, vals[i]);
               emit_copy(ptr_reg, src_reg, length_reg);
            }
            else {
               vcode_reg_t src_reg = vals[i];
               if (vcode_reg_kind(src_reg) != VCODE_TYPE_UARRAY)
                  src_reg = lower_wrap(lu, tree_type(map[i]), src_reg);

               if ((cons = type_constraint_for_field(type, f))) {
                  // Element constraint may be OPEN for constants
                  type_t ctype = tree_type(cons);
                  if (!type_is_unconstrained(ctype)) {
                     vcode_reg_t locus = lower_debug_locus(map[i]);
                     lower_check_array_sizes(lu, ctype, tree_type(map[i]),
                                             VCODE_INVALID_REG,
                                             src_reg, locus);
                  }
               }

               emit_store_indirect(src_reg, ptr_reg);
            }
         }
         else if (type_is_record(ftype))
            emit_copy(ptr_reg, vals[i], VCODE_INVALID_REG);
         else
            emit_store_indirect(vals[i], ptr_reg);
      }

      return mem_reg;
   }
}

static bool lower_can_use_const_rep(tree_t expr, int *length, tree_t *elem)
{
   if (tree_kind(expr) != T_AGGREGATE)
      return false;

   type_t type = tree_type(expr);
   assert(lower_const_bounds(type));

   tree_t a0 = tree_assoc(expr, 0);
   if (tree_subkind(a0) != A_OTHERS)
      return false;

   tree_t others = tree_value(a0);
   type_t elem_type = tree_type(others);

   if (type_is_array(elem_type)) {
      if (!lower_can_use_const_rep(others, length, elem))
         return false;
   }
   else if (type_is_scalar(elem_type))
      *elem = others;
   else
      return false;

   *length = lower_array_const_size(type);
   return true;
}

static vcode_reg_t lower_aggregate_bounds(lower_unit_t *lu, tree_t expr,
                                          vcode_reg_t *a0_reg)
{
   // Calculate the direction and bounds of an unconstrained array
   // aggregate using the rules in LRM 93 7.3.2.2

   type_t type = tree_type(expr);
   assert(type_is_unconstrained(type));

   type_t index_type = index_type_of(type, 0);
   tree_t base_r = range_of(index_type, 0);

   int64_t low, high;
   if (!folded_bounds(base_r, &low, &high))
      fatal_trace("index type %s has unknown bounds", type_pp(index_type));

   int64_t clow = high, chigh = low;  // Actual bounds computed below
   range_kind_t dir = tree_subkind(base_r);

   const int nassocs = tree_assocs(expr);

   bool known_elem_count = true;
   for (int i = 0; i < nassocs; i++) {
      tree_t a = tree_assoc(expr, i);
      int64_t ilow = 0, ihigh = 0;

      switch (tree_subkind(a)) {
      case A_NAMED:
         {
            tree_t name = tree_name(a);
            if (folded_int(name, &ilow))
               ihigh = ilow;
            else
               known_elem_count = false;
         }
         break;

      case A_RANGE:
         {
            tree_t r = tree_range(a, 0);
            const range_kind_t rkind = tree_subkind(r);
            if (rkind == RANGE_TO || rkind == RANGE_DOWNTO) {
               tree_t left = tree_left(r), right = tree_right(r);

               int64_t ileft, iright;
               if (folded_int(left, &ileft) && folded_int(right, &iright)) {
                  ilow = (rkind == RANGE_TO ? ileft : iright);
                  ihigh = (rkind == RANGE_TO ? iright : ileft);
               }
               else
                  known_elem_count = false;

               // VHDL-2008 range association determines index direction
               // for unconstrained aggregate
               if (standard() >= STD_08)
                  dir = rkind;
            }
            else
               known_elem_count = false;
         }
         break;

      case A_OTHERS:
         known_elem_count = false;
         break;

      case A_POS:
         ihigh = ilow = (dir == RANGE_TO ? low + i : high - i);
         break;
      }

      clow = MIN(clow, ilow);
      chigh = MAX(chigh, ihigh);
   }

   vcode_reg_t left_reg, right_reg, dir_reg;
   if (known_elem_count) {
      const int64_t ileft = dir == RANGE_TO ? clow : chigh;
      const int64_t iright = dir == RANGE_TO ? chigh : clow;

      vcode_type_t vindex = lower_type(index_type);

      if (type_is_enum(index_type)) {
         left_reg = emit_const(vindex, ileft);
         right_reg = emit_const(vindex, iright);
      }
      else if (type_is_integer(index_type)) {
         left_reg = emit_const(vindex, ileft);
         right_reg = emit_const(vindex, iright);
      }
      else
         fatal_trace("cannot handle aggregate index type %s",
                     type_pp(index_type));

      dir_reg = emit_const(vtype_bool(), dir);
   }
   else {
      // Must have a single association
      assert(nassocs == 1);
      tree_t a0 = tree_assoc(expr, 0);
      switch (tree_subkind(a0)) {
      case A_NAMED:
         left_reg = right_reg = lower_rvalue(lu, tree_name(a0));
         dir_reg = emit_const(vtype_bool(), dir);
         break;
      case A_RANGE:
         {
            tree_t a0r = tree_range(a0, 0);
            left_reg = lower_range_left(lu, a0r);
            right_reg = lower_range_right(lu, a0r);
            dir_reg = lower_range_dir(lu, a0r);
         }
         break;
      default:
         fatal_trace("unexpected association kind %d in unconstrained "
                     "aggregate", tree_subkind(a0));
      }
   }

   vcode_reg_t null_reg = emit_null(vtype_pointer(vtype_offset()));

   type_t elem = type_elem(type);
   const bool expand_a0 =
      dimension_of(type) > 1
      || (type_is_array(elem) && !lower_const_bounds(elem));

   if (expand_a0) {
      const int ndims = dims_for_type(type);
      vcode_dim_t *dims LOCAL = xmalloc_array(ndims, sizeof(vcode_dim_t));

      dims[0].left  = left_reg;
      dims[0].right = right_reg;
      dims[0].dir   = dir_reg;

      tree_t a0 = tree_value(tree_assoc(expr, 0));
      *a0_reg = lower_rvalue(lu, a0);

      if (vcode_reg_kind(*a0_reg) == VCODE_TYPE_UARRAY) {
         for (int i = 1; i < ndims; i++) {
            dims[i].left  = emit_uarray_left(*a0_reg, i - 1);
            dims[i].right = emit_uarray_right(*a0_reg, i - 1);
            dims[i].dir   = emit_uarray_dir(*a0_reg, i - 1);
         }
      }
      else {
         type_t a0_type = tree_type(a0);
         for (int i = 1; i < ndims; i++) {
            dims[i].left  = lower_array_left(lu, a0_type, i - 1, *a0_reg);
            dims[i].right = lower_array_right(lu, a0_type, i - 1, *a0_reg);
            dims[i].dir   = lower_array_dir(lu, a0_type, i - 1, *a0_reg);
         }
      }

      return emit_wrap(null_reg, dims, ndims);
   }
   else {
      vcode_dim_t dims[] = {
         { left_reg, right_reg, dir_reg }
      };
      return emit_wrap(null_reg, dims, 1);
   }
}

static vcode_reg_t lower_array_aggregate(lower_unit_t *lu, tree_t expr,
                                         vcode_reg_t hint)
{
   emit_debug_info(tree_loc(expr));

   type_t type = tree_type(expr);

   if (lower_const_bounds(type) && lower_is_const(expr)) {
      int rep_size = -1;
      tree_t rep_elem = NULL;
      if (lower_can_use_const_rep(expr, &rep_size, &rep_elem) && rep_size > 1) {
         vcode_reg_t elem_reg = lower_rvalue(lu, rep_elem);
         if (hint != VCODE_INVALID_REG) {
            emit_memset(hint, elem_reg, emit_const(vtype_offset(), rep_size));
            return hint;
         }
         else {
            vcode_type_t vtype = lower_type(type);
            vcode_reg_t array = emit_const_rep(vtype, elem_reg, rep_size);
            return emit_address_of(array);
         }
      }
      else {
         int nvals;
         vcode_reg_t *values LOCAL =
            lower_const_array_aggregate(lu, expr, type, 0, &nvals);

         vcode_reg_t array = emit_const_array(lower_type(type), values, nvals);
         return emit_address_of(array);
      }
   }

   bool all_literals = true;
   tree_t def_value = NULL;
   const int nassocs = tree_assocs(expr);
   for (int i = 0; i < nassocs; i++) {
      tree_t a = tree_assoc(expr, i);
      switch (tree_subkind(a)) {
      case A_POS:
         all_literals &= (tree_kind(tree_value(a)) == T_LITERAL);
         break;
      case A_OTHERS:
         def_value = tree_value(a);
         // Fall-through
      default:
         all_literals = false;
         break;
      }
   }

   emit_comment("Begin array aggregrate line %d", tree_loc(expr)->first_line);

   vcode_reg_t bounds_reg = VCODE_INVALID_REG, a0_reg = VCODE_INVALID_REG;
   if (type_is_unconstrained(type))
      bounds_reg = lower_aggregate_bounds(lu, expr, &a0_reg);

   vcode_reg_t dir_reg = lower_array_dir(lu, type, 0, bounds_reg);
   vcode_reg_t left_reg = lower_array_left(lu, type, 0, bounds_reg);
   vcode_reg_t right_reg = lower_array_right(lu, type, 0, bounds_reg);

   vcode_reg_t null_reg = emit_range_null(left_reg, right_reg, dir_reg);

   type_t elem_type = type_elem(type);
   type_t scalar_elem_type = lower_elem_recur(type);

   const bool array_of_array = type_is_array(elem_type);

   vcode_type_t velem   = lower_type(scalar_elem_type);
   vcode_type_t vbounds = lower_bounds(scalar_elem_type);
   vcode_type_t voffset = vtype_offset();

   if (all_literals) {
      // The array has non-constant bounds but the elements are all
      // constants so just create a constant array and wrap it
      vcode_reg_t *values LOCAL = xmalloc_array(nassocs, sizeof(vcode_reg_t));
      for (int i = 0; i < nassocs; i++)
         values[i] = lower_literal(tree_value(tree_assoc(expr, i)));

      type_t elem = type_elem(type);

      vcode_type_t velem   = lower_type(elem);
      vcode_type_t vbounds = lower_bounds(elem);
      vcode_type_t vtype = vtype_carray(nassocs, velem, vbounds);

      vcode_reg_t array_reg = emit_const_array(vtype, values, nassocs);
      vcode_reg_t mem_reg = emit_address_of(array_reg);

      vcode_reg_t nassocs_reg = emit_const(voffset, nassocs);
      vcode_reg_t length_reg = emit_range_length(left_reg, right_reg, dir_reg);

      vcode_reg_t locus = lower_debug_locus(expr);
      emit_length_check(length_reg, nassocs_reg, locus, VCODE_INVALID_REG);

      vcode_dim_t dims[1] = {
         { left_reg, right_reg, dir_reg }
      };
      return emit_wrap(mem_reg, dims, 1);
   }

   int64_t null_const;
   bool known_not_null = false;
   if (vcode_reg_const(null_reg, &null_const)) {
      if (null_const) {
         vcode_type_t vtype = vtype_carray(0, velem, vbounds);
         vcode_reg_t mem_reg =
            emit_address_of(emit_const_array(vtype, NULL, 0));

         if (bounds_reg != VCODE_INVALID_REG)
            return lower_rewrap(mem_reg, bounds_reg);
         else
            return mem_reg;
      }
      else
         known_not_null = true;
   }

   vcode_reg_t stride;
   if (array_of_array)
      stride = lower_array_stride(lu, type, bounds_reg);
   else
      stride = emit_const(voffset, 1);

   const int ndims = dimension_of(type);
   const bool multidim = ndims > 1;

   if (multidim) {
      for (int i = 1; i < ndims; i++)
         stride = emit_mul(stride, lower_array_len(lu, type, i, bounds_reg));
      emit_comment("Multidimensional array stride is r%d", stride);
   }

   vcode_reg_t dim0_len = lower_array_len(lu, type, 0, bounds_reg);
   vcode_reg_t len_reg = emit_mul(dim0_len, stride);

   vcode_reg_t mem_reg;
   if (hint != VCODE_INVALID_REG)
      mem_reg = lower_array_data(hint);
   else
      mem_reg = emit_alloc(velem, vbounds, len_reg);

   vcode_reg_t wrap_reg;
   if (lower_const_bounds(type))
      wrap_reg = mem_reg;
   else if (bounds_reg != VCODE_INVALID_REG)
      wrap_reg = lower_rewrap(mem_reg, bounds_reg);
   else if (multidim) {
      vcode_dim_t *dims LOCAL = xmalloc_array(ndims, sizeof(vcode_dim_t));
      for (int i = 0; i < ndims; i++) {
         dims[i].left  = lower_array_left(lu, type, i, bounds_reg);
         dims[i].right = lower_array_right(lu, type, i, bounds_reg);
         dims[i].dir   = lower_array_dir(lu, type, i, bounds_reg);
      }
      wrap_reg = emit_wrap(mem_reg, dims, ndims);
   }
   else if (array_of_array)
      wrap_reg = lower_wrap(lu, type, mem_reg);
   else {
      vcode_dim_t dim0 = {
         .left  = left_reg,
         .right = right_reg,
         .dir   = dir_reg
      };
      wrap_reg = emit_wrap(mem_reg, &dim0, 1);
   }

   vcode_block_t skip_bb = VCODE_INVALID_BLOCK;
   if (!known_not_null) {
      vcode_block_t not_null_bb = emit_block();
      skip_bb = emit_block();
      emit_cond(null_reg, skip_bb, not_null_bb);

      vcode_select_block(not_null_bb);
   }

   if (def_value != NULL) {
      // Initialise the array with the default value
      if (type_is_scalar(elem_type) && !multidim) {
         vcode_reg_t def_reg = lower_rvalue(lu, def_value);
         emit_memset(mem_reg, def_reg, len_reg);
      }
      else {
         vcode_block_t loop_bb = emit_block();
         vcode_block_t exit_bb = emit_block();

         vcode_var_t i_var = lower_temp_var(lu, "i", voffset, voffset);
         emit_store(emit_const(voffset, 0), i_var);

         // TODO: this is a hack to work around the lack of a block
         // ordering pass in vcode
         vcode_reg_t def_reg = VCODE_INVALID_REG;
         if (type_is_scalar(elem_type) && !multidim)
            def_reg = lower_rvalue(lu, def_value);

         emit_cond(null_reg, exit_bb, loop_bb);

         vcode_select_block(loop_bb);

         vcode_reg_t inc_reg = stride;
         if (inc_reg == VCODE_INVALID_REG)
            inc_reg = emit_const(voffset, 1);

         vcode_reg_t i_reg = emit_load(i_var);
         vcode_reg_t next_reg = emit_add(i_reg, inc_reg);
         emit_store(next_reg, i_var);

         vcode_reg_t ptr_reg = emit_array_ref(mem_reg, i_reg);

         if (def_reg == VCODE_INVALID_REG) {
            if (tree_kind(def_value) == T_AGGREGATE)
               def_reg = lower_aggregate(lu, def_value, ptr_reg);
            else
               def_reg = lower_rvalue(lu, def_value);
         }

         if (array_of_array || multidim) {
            assert(stride != VCODE_INVALID_REG);
            vcode_reg_t src_reg = lower_array_data(def_reg);
            emit_copy(ptr_reg, src_reg, stride);
         }
         else if (type_is_record(elem_type))
            emit_copy(ptr_reg, def_reg, VCODE_INVALID_REG);
         else
            emit_store_indirect(def_reg, ptr_reg);

         vcode_reg_t done_reg = emit_cmp(VCODE_CMP_EQ, next_reg, len_reg);
         emit_cond(done_reg, exit_bb, loop_bb);

         vcode_select_block(exit_bb);
         lower_release_temp(lu, i_var);
      }
   }

   vcode_reg_t next_pos = VCODE_INVALID_REG;

   type_t index_type = index_type_of(type, 0);
   tree_t index_r = range_of(index_type, 0);

   vcode_type_t vindex = lower_type(index_type);

   for (int i = 0; i < nassocs; i++) {
      tree_t a = tree_assoc(expr, i);
      tree_t value = tree_value(a);

      vcode_reg_t value_reg = VCODE_INVALID_REG;
      if (tree_kind(value) != T_AGGREGATE)
         value_reg = lower_rvalue(lu, value);

      vcode_reg_t count_reg = VCODE_INVALID_REG;
      type_t value_type = tree_type(value);
      if (!multidim && type_eq(type, value_type)) {
         // Element has same type as whole aggregate
         assert(standard() >= STD_08);
         count_reg = lower_array_len(lu, value_type, 0, value_reg);
      }

      vcode_reg_t loop_bb = VCODE_INVALID_BLOCK;
      vcode_reg_t exit_bb = VCODE_INVALID_BLOCK;

      vcode_var_t tmp_var = VCODE_INVALID_VAR;
      vcode_reg_t off_reg = VCODE_INVALID_REG;

      switch (tree_subkind(a)) {
      case A_POS:
         if (next_pos == VCODE_INVALID_REG) {
            off_reg = emit_const(voffset, tree_pos(a));
            next_pos = count_reg;
         }
         else if (count_reg == VCODE_INVALID_REG) {
            off_reg = next_pos;
            next_pos = emit_add(next_pos, emit_const(voffset, 1));
         }
         else {
            off_reg = next_pos;
            next_pos = emit_add(next_pos, count_reg);
         }

         vcode_reg_t locus = lower_debug_locus(a);
         vcode_reg_t hint = lower_debug_locus(index_r);

         vcode_reg_t index_off_reg = emit_cast(vindex, vindex, off_reg);

         vcode_reg_t up_left_reg = emit_add(left_reg, index_off_reg);
         vcode_reg_t down_left_reg = emit_add(right_reg, index_off_reg);
         vcode_reg_t left_index_reg =
            emit_select(dir_reg, down_left_reg, up_left_reg);

         emit_index_check(left_index_reg, left_reg, right_reg,
                          dir_reg, locus, hint);

         if (count_reg != VCODE_INVALID_REG) {
            vcode_reg_t one_reg = emit_const(voffset, 1);
            vcode_reg_t right_off_reg =
               emit_sub(emit_add(index_off_reg, count_reg), one_reg);
            vcode_reg_t up_right_reg = emit_add(left_reg, right_off_reg);
            vcode_reg_t down_right_reg = emit_add(right_reg, right_off_reg);
            vcode_reg_t right_index_reg =
               emit_select(dir_reg, down_right_reg, up_right_reg);

            emit_index_check(right_index_reg, left_reg, right_reg,
                             dir_reg, locus, hint);
         }
         break;

      case A_NAMED:
         {
            tree_t name = tree_name(a);
            vcode_reg_t name_reg = lower_rvalue(lu, name);
            vcode_reg_t locus = lower_debug_locus(name);
            emit_index_check(name_reg, left_reg, right_reg, dir_reg,
                             locus, locus);
            off_reg = lower_array_off(lu, name_reg, wrap_reg, type, 0);
         }
         break;

      case A_RANGE:
         if (count_reg != VCODE_INVALID_REG) {
            tree_t r = tree_range(a, 0);

            vcode_reg_t r_left_reg  = lower_range_left(lu, r);
            vcode_reg_t r_right_reg = lower_range_right(lu, r);
            vcode_reg_t r_dir_reg   = lower_range_dir(lu, r);

            vcode_reg_t locus = lower_debug_locus(r);
            emit_index_check(r_left_reg, left_reg, right_reg, dir_reg,
                             locus, locus);
            emit_index_check(r_right_reg, left_reg, right_reg, dir_reg,
                             locus, locus);

            vcode_reg_t dir_cmp_reg =
               emit_cmp(VCODE_CMP_EQ, dir_reg, r_dir_reg);
            vcode_reg_t base_reg =
               emit_select(dir_cmp_reg, r_left_reg, r_right_reg);

            off_reg = lower_array_off(lu, base_reg, wrap_reg, type, 0);
         }
         else {
            loop_bb = emit_block();
            exit_bb = emit_block();

            tree_t r = tree_range(a, 0);
            type_t rtype = tree_type(r);

            vcode_reg_t r_left_reg  = lower_range_left(lu, r);
            vcode_reg_t r_right_reg = lower_range_right(lu, r);
            vcode_reg_t r_dir_reg   = lower_range_dir(lu, r);

            vcode_reg_t locus = lower_debug_locus(r);
            emit_index_check(r_left_reg, left_reg, right_reg, dir_reg,
                             locus, locus);
            emit_index_check(r_right_reg, left_reg, right_reg, dir_reg,
                             locus, locus);

            vcode_type_t vtype   = lower_type(rtype);
            vcode_type_t vbounds = lower_bounds(rtype);

            tmp_var = lower_temp_var(lu, "i", vtype, vbounds);
            emit_store(r_left_reg, tmp_var);

            vcode_reg_t null_reg =
               emit_range_null(r_left_reg, r_right_reg, r_dir_reg);
            emit_cond(null_reg, exit_bb, loop_bb);

            vcode_select_block(loop_bb);
            emit_debug_info(tree_loc(a));

            vcode_reg_t i_reg = emit_load(tmp_var);
            off_reg = lower_array_off(lu, i_reg, wrap_reg, type, 0);
         }
         break;

      case A_OTHERS:
         // Handled above
         continue;
      }

      if (stride != VCODE_INVALID_REG)
         off_reg = emit_mul(off_reg, stride);

      vcode_reg_t ptr_reg = emit_array_ref(mem_reg, off_reg);

      if (value_reg == VCODE_INVALID_REG) {
         // Prefer generating aggregates in-place
         assert(tree_kind(value) == T_AGGREGATE);
         value_reg = lower_aggregate(lu, value, ptr_reg);
      }

      if (a0_reg != VCODE_INVALID_REG && i > 0 && ndims == 1
          && vcode_reg_kind(a0_reg) == VCODE_TYPE_UARRAY) {
         // Element type is unconstrained so we need a length check here
         vcode_reg_t locus = lower_debug_locus(a);
         lower_check_array_sizes(lu, elem_type, elem_type,
                                 a0_reg, value_reg, locus);
      }

      if (count_reg != VCODE_INVALID_REG) {
         vcode_reg_t src_reg = lower_array_data(value_reg);
         emit_copy(ptr_reg, src_reg, count_reg);
      }
      else if (array_of_array || multidim) {
         assert(stride != VCODE_INVALID_REG);
         vcode_reg_t src_reg = lower_array_data(value_reg);
         emit_copy(ptr_reg, src_reg, stride);
      }
      else if (type_is_record(elem_type))
         emit_copy(ptr_reg, value_reg, VCODE_INVALID_REG);
      else
         emit_store_indirect(value_reg, ptr_reg);

      if (loop_bb != VCODE_INVALID_BLOCK) {
         assert(tree_subkind(a) == A_RANGE);
         tree_t r = tree_range(a, 0);

         vcode_type_t vtype = lower_type(tree_type(r));

         vcode_reg_t r_dir_reg = lower_range_dir(lu, r);
         vcode_reg_t step_down = emit_const(vtype, -1);
         vcode_reg_t step_up   = emit_const(vtype, 1);
         vcode_reg_t step_reg  = emit_select(r_dir_reg, step_down, step_up);
         vcode_reg_t i_reg     = emit_load(tmp_var);
         vcode_reg_t next_reg  = emit_add(i_reg, step_reg);
         emit_store(next_reg, tmp_var);

         vcode_reg_t r_right_reg = lower_range_right(lu, r);
         vcode_reg_t done_reg    = emit_cmp(VCODE_CMP_EQ, i_reg, r_right_reg);
         emit_cond(done_reg, exit_bb, loop_bb);

         vcode_select_block(exit_bb);
      }

      if (tmp_var != VCODE_INVALID_VAR)
         lower_release_temp(lu, tmp_var);
   }

   if (skip_bb != VCODE_INVALID_BLOCK) {
      emit_jump(skip_bb);
      vcode_select_block(skip_bb);
   }

   return wrap_reg;
}

static vcode_reg_t lower_aggregate(lower_unit_t *lu, tree_t expr,
                                   vcode_reg_t hint)
{
   type_t type = tree_type(expr);

   if (type_is_record(type))
      return lower_record_aggregate(lu, expr, false,
                                    lower_is_const(expr), hint);
   else if (type_is_array(type))
      return lower_array_aggregate(lu, expr, hint);
   else
      fatal_trace("invalid type %s in lower_aggregate", type_pp(type));
}

static vcode_reg_t lower_record_ref(lower_unit_t *lu, tree_t expr,
                                    expr_ctx_t ctx)
{
   tree_t value = tree_value(expr);
   type_t type = tree_type(value);
   vcode_reg_t record = lower_expr(lu, value, ctx);

   const int index = tree_pos(tree_ref(expr));
   type_t ftype = tree_type(type_field(type, index));

   vcode_reg_t f_reg = emit_record_ref(record, index);
   if (lower_have_signal(f_reg) && type_is_homogeneous(ftype))
      return emit_load_indirect(f_reg);
   else if (type_is_array(ftype) && !lower_const_bounds(ftype)) {
      // The field type may be unconstrained but this particular
      // instance has a record element constraint
      vcode_reg_t array = emit_load_indirect(f_reg);
      if (lower_const_bounds(tree_type(expr)))
         return lower_array_data(array);
      else
         return array;
   }
   else
      return f_reg;
}

static vcode_reg_t lower_prot_ref(lower_unit_t *lu, tree_t expr)
{
   assert(standard() >= STD_19);

   vcode_reg_t prefix = lower_rvalue(lu, tree_value(expr));

   vcode_type_t vtype = lower_type(tree_type(expr));
   return emit_link_var(prefix, tree_ident(expr), vtype);
}

static void lower_new_record(lower_unit_t *lu, type_t type,
                             vcode_reg_t dst_ptr, vcode_reg_t src_ptr)
{
   if (src_ptr == dst_ptr)
      return;
   else if (lower_const_bounds(type))
      emit_copy(dst_ptr, src_ptr, VCODE_INVALID_REG);
   else {
      type_t base = lower_base_type(type);
      ident_t helper_func =
         ident_prefix(type_ident(base), ident_new("new"), '$');

      vcode_reg_t args[] = {
         lower_context_for_call(lu, helper_func),
         dst_ptr,
         src_ptr,
      };
      emit_fcall(helper_func, VCODE_INVALID_TYPE, VCODE_INVALID_TYPE,
                 VCODE_CC_VHDL, args, ARRAY_LEN(args));
   }
}

static vcode_reg_t lower_new(lower_unit_t *lu, tree_t expr)
{
   tree_t qual = tree_value(expr);
   assert(tree_kind(qual) == T_QUALIFIED);

   type_t type = tree_type(qual);

   if (type_is_array(type)) {
      type_t value_type = type, result_type = type_designated(tree_type(expr));
      type_t elem = lower_elem_recur(type);

      const bool wrap_result = !lower_const_bounds(result_type);
      const bool wrap_value = !lower_const_bounds(type);

      type_t alloc_type = wrap_result ? result_type : elem;

      vcode_reg_t init_reg = VCODE_INVALID_REG, bounds_reg = VCODE_INVALID_REG;
      tree_t value = NULL;
      if (tree_has_value(qual)) {
         value = tree_value(qual);
         value_type = tree_type(value);

         const bool in_place_aggregate =
            tree_kind(value) == T_AGGREGATE && lower_const_bounds(value_type);

         if (!in_place_aggregate)
            init_reg = bounds_reg = lower_rvalue(lu, qual);
      }
      else if (wrap_value) {
         vcode_reg_t null_reg = emit_null(vtype_pointer(lower_type(elem)));
         bounds_reg = lower_wrap(lu, type, null_reg);
      }

      vcode_reg_t length_reg =
         lower_array_total_len(lu, value_type, bounds_reg);
      vcode_reg_t mem_reg = emit_new(lower_type(alloc_type), length_reg);
      vcode_reg_t all_reg = emit_all(mem_reg);
      vcode_reg_t data_reg = lower_array_data(all_reg);

      vcode_reg_t array_reg = all_reg;
      if (wrap_value)
         array_reg = lower_wrap_with_new_bounds(lu, value_type, type,
                                                bounds_reg, data_reg);

      if (value == NULL)
         init_reg = lower_default_value(lu, type, array_reg);
      else if (init_reg == VCODE_INVALID_REG)
         init_reg = lower_aggregate(lu, value, data_reg);

      if (init_reg != array_reg) {
         vcode_reg_t src_reg = lower_array_data(init_reg);
         emit_copy(data_reg, src_reg, length_reg);
      }

      if (wrap_result) {
         // Need to initialise the array bounds
         vcode_reg_t meta_reg =
            lower_wrap_with_new_bounds(lu, value_type, result_type,
                                       bounds_reg, data_reg);
         emit_store_indirect(meta_reg, all_reg);
      }

      return mem_reg;
   }
   else if (type_is_record(type)) {
      vcode_reg_t result_reg =
         emit_new(lower_type(type), VCODE_INVALID_REG);
      vcode_reg_t all_reg = emit_all(result_reg);

      vcode_reg_t init_reg;
      if (tree_has_value(qual)) {
         tree_t value = tree_value(qual);
         if (tree_kind(value) == T_AGGREGATE && lower_const_bounds(type))
            init_reg = lower_aggregate(lu, value, all_reg);
         else
            init_reg = lower_rvalue(lu, qual);
      }
      else
         init_reg = lower_default_value(lu, type, all_reg);

      lower_new_record(lu, type, all_reg, init_reg);
      return result_reg;
   }
   else if (type_is_protected(type)) {
      vcode_type_t vtype = lower_type(type);
      vcode_reg_t context_reg = lower_context_for_call(lu, type_ident(type));
      vcode_reg_t obj_reg = emit_protected_init(vtype, context_reg);
      vcode_reg_t result_reg = emit_new(vtype, VCODE_INVALID_REG);
      vcode_reg_t all_reg = emit_all(result_reg);
      emit_store_indirect(obj_reg, all_reg);
      return result_reg;
   }
   else {
      vcode_reg_t result_reg = emit_new(lower_type(type), VCODE_INVALID_REG);
      vcode_reg_t all_reg = emit_all(result_reg);

      vcode_reg_t init_reg;
      if (tree_has_value(qual))
         init_reg = lower_rvalue(lu, qual);
      else
         init_reg = lower_default_value(lu, type, VCODE_INVALID_REG);

      emit_store_indirect(init_reg, all_reg);
      return result_reg;
   }
}

static vcode_reg_t lower_incomplete_access(vcode_reg_t in_reg, type_t type)
{
   assert(vcode_reg_kind(in_reg) == VCODE_TYPE_ACCESS);

   vcode_type_t pointed = vtype_pointed(vcode_reg_type(in_reg));

   const bool have_opaque = vtype_kind(pointed) == VCODE_TYPE_OPAQUE;
   const bool have_incomplete = type_is_incomplete(type);

   if (have_incomplete ^ have_opaque) {
      vcode_type_t ptr_type = vtype_access(lower_type(type));
      return emit_cast(ptr_type, ptr_type, in_reg);
   }

   return in_reg;
}

static vcode_reg_t lower_all(lower_unit_t *lu, tree_t all, expr_ctx_t ctx)
{
   type_t type = tree_type(all);
   vcode_reg_t access_reg = lower_rvalue(lu, tree_value(all));
   emit_null_check(access_reg, lower_debug_locus(all));
   access_reg = lower_incomplete_access(access_reg, tree_type(all));
   vcode_reg_t all_reg = emit_all(access_reg);

   if (type_is_array(type) && !lower_const_bounds(type))
      return emit_load_indirect(all_reg);
   else
      return all_reg;
}

static void lower_convert_record(lower_unit_t *lu, vcode_reg_t value_reg,
                                 tree_t where, type_t from, type_t to,
                                 vcode_reg_t ptr_reg)
{
   const int to_nf = type_fields(to);
   const int from_nf = type_fields(from);

   for (int i = 0; i < to_nf; i++) {
      tree_t to_f = type_field(to, i);
      type_t to_type = tree_type(to_f), from_type = NULL;
      ident_t name = tree_ident(to_f);

      vcode_reg_t from_reg = VCODE_INVALID_REG;
      for (int j = 0; j < from_nf; j++) {
         tree_t f = type_field(from, j);
         if (tree_ident(f) == name) {
            from_reg = emit_record_ref(value_reg, j);
            from_type = tree_type(f);
            break;
         }
      }
      assert(from_reg != VCODE_INVALID_REG);

      if (have_uarray_ptr(from_reg))
         from_reg = emit_load_indirect(from_reg);

      vcode_reg_t to_reg = emit_record_ref(ptr_reg, i);

      if (type_is_array(to_type)) {
         vcode_reg_t conv_reg =
            lower_conversion(lu, from_reg, where, from_type, to_type);

         // Shallow copy is ok here as the result of this conversion is
         // effectively constant
         if (have_uarray_ptr(to_reg)) {
            assert(vcode_reg_kind(conv_reg) == VCODE_TYPE_UARRAY);
            emit_store_indirect(conv_reg, to_reg);
         }
         else {
            vcode_reg_t data_reg = lower_array_data(conv_reg);
            vcode_reg_t count_reg =
               lower_array_total_len(lu, to_type, conv_reg);
            emit_copy(to_reg, data_reg, count_reg);
         }
      }
      else if (type_is_record(to_type))
         lower_convert_record(lu, from_reg, where, from_type, to_type, to_reg);
      else {
         vcode_reg_t elem_reg = emit_load_indirect(from_reg);
         vcode_reg_t conv_reg =
            lower_conversion(lu, elem_reg, where, from_type, to_type);
         emit_store_indirect(conv_reg, to_reg);
      }
   }
}

static vcode_reg_t lower_conversion(lower_unit_t *lu, vcode_reg_t value_reg,
                                    tree_t where, type_t from, type_t to)
{
   type_kind_t from_k = type_kind(type_base_recur(from));
   type_kind_t to_k   = type_kind(type_base_recur(to));

   if (from_k == T_REAL && to_k == T_INTEGER) {
      vcode_type_t to_vtype = lower_type(to);
      vcode_reg_t cast = emit_cast(to_vtype, to_vtype, value_reg);
      lower_check_scalar_bounds(lu, cast, to, where, NULL);
      return cast;
   }
   else if (from_k == T_INTEGER && to_k == T_REAL)
      return emit_cast(lower_type(to), lower_bounds(to), value_reg);
   else if (to_k == T_ARRAY && !lower_const_bounds(to)) {
      // Need to wrap in metadata
      vcode_reg_t wrap_reg = lower_wrap(lu, from, value_reg);
      if (type_is_unconstrained(to))
         lower_check_indexes(lu, from, to, wrap_reg, where);
      else {
         vcode_reg_t locus = lower_debug_locus(where);
         lower_check_array_sizes(lu, to, from, VCODE_INVALID_REG,
                                 wrap_reg, locus);
      }
      return wrap_reg;
   }
   else if (to_k == T_ARRAY && !type_is_unconstrained(from)) {
      vcode_reg_t locus = lower_debug_locus(where);
      lower_check_array_sizes(lu, to, from, VCODE_INVALID_REG,
                              VCODE_INVALID_REG, locus);
      return value_reg;
   }
   else if ((from_k == T_INTEGER && to_k == T_INTEGER)
            || (from_k == T_REAL && to_k == T_REAL)) {
      // Possibly change width
      lower_check_scalar_bounds(lu, value_reg, to, where, NULL);
      return emit_cast(lower_type(to), lower_bounds(to), value_reg);
   }
   else if (from_k == T_RECORD && to_k == T_RECORD) {
      vcode_type_t vtype_to = lower_type(to);
      vcode_var_t tmp_var = lower_temp_var(lu, "conv", vtype_to, vtype_to);
      vcode_reg_t ptr_reg = emit_index(tmp_var, VCODE_INVALID_REG);
      lower_convert_record(lu, value_reg, where, from, to, ptr_reg);
      return ptr_reg;
   }
   else {
      // No conversion to perform
      return value_reg;
   }
}

static vcode_reg_t lower_type_conv(lower_unit_t *lu, tree_t expr)
{
   tree_t value = tree_value(expr);

   type_t from = tree_type(value);
   type_t to   = tree_type(expr);

   vcode_reg_t value_reg = lower_rvalue(lu, value);
   return lower_conversion(lu, value_reg, expr, from, to);
}

static const int lower_get_attr_dimension(tree_t expr)
{
   if (tree_params(expr) > 0)
      return assume_int(tree_value(tree_param(expr, 0))) - 1;
   else
      return 0;
}

static vcode_reg_t lower_attr_prefix(lower_unit_t *lu, tree_t prefix)
{
   switch (class_of(prefix)) {
   case C_SIGNAL:
      return lower_lvalue(lu, prefix);
   case C_TYPE:
   case C_SUBTYPE:
      return VCODE_INVALID_REG;
   default:
      return lower_rvalue(lu, prefix);
   }
}

static vcode_reg_t lower_reflect_attr(lower_unit_t *lu, tree_t expr)
{
   tree_t name = tree_name(expr);

   type_t type = tree_type(expr), pt = type_designated(type);
   assert(type_is_protected(pt));

   ident_t init_func = type_ident(pt);
   vcode_reg_t context_reg = lower_context_for_call(lu, init_func);

   type_t value_mirror = reflection_type(REFLECT_VALUE_MIRROR);
   const bool is_value_mirror = type_eq(type, value_mirror);

   vcode_reg_t value_reg = VCODE_INVALID_REG, bounds_reg = VCODE_INVALID_REG;
   if (is_value_mirror)
      value_reg = lower_attr_prefix(lu, name);

   type_t value_type = tree_type(name);
   if (type_is_array(value_type))
      bounds_reg = lower_wrap(lu, value_type, value_reg);

   vcode_reg_t locus = lower_debug_locus(name);

   if (is_value_mirror)
      return emit_reflect_value(init_func, value_reg, context_reg,
                                locus, bounds_reg);
   else
      return emit_reflect_subtype(init_func, context_reg,
                                  locus, bounds_reg);
}

static vcode_reg_t lower_attr_param(lower_unit_t *lu, tree_t value,
                                    type_t port_type, class_t class)
{
   type_t value_type = tree_type(value);

   vcode_reg_t reg;
   if (class == C_SIGNAL || class == C_FILE)
      reg = lower_lvalue(lu, value);
   else
      reg = lower_rvalue(lu, value);

   if (reg == VCODE_INVALID_REG)
      return reg;

   if (type_is_array(value_type))
      return lower_coerce_arrays(lu, value_type, port_type ?: value_type, reg);
   else if (class == C_SIGNAL || class == C_FILE)
      return reg;
   else
      return reg;
}

static vcode_reg_t lower_attr_ref(lower_unit_t *lu, tree_t expr)
{
   tree_t name = tree_name(expr);

   const attr_kind_t predef = tree_subkind(expr);
   switch (predef) {
   case ATTR_LEFT:
   case ATTR_RIGHT:
      {
         const int dim = lower_get_attr_dimension(expr);

         type_t type = tree_type(name);
         if (type_is_unconstrained(type)) {
            vcode_reg_t array_reg = lower_attr_prefix(lu, name);
            if (predef == ATTR_LEFT)
               return lower_array_left(lu, type, dim, array_reg);
            else
               return lower_array_right(lu, type, dim, array_reg);
         }
         else {
            tree_t r = range_of(type, dim);
            if (predef == ATTR_LEFT)
               return lower_range_left(lu, r);
            else
               return lower_range_right(lu, r);
         }
      }

   case ATTR_LOW:
   case ATTR_HIGH:
      {
         const int dim = lower_get_attr_dimension(expr);

         vcode_reg_t left_reg  = VCODE_INVALID_REG;
         vcode_reg_t right_reg = VCODE_INVALID_REG;
         vcode_reg_t dir_reg   = VCODE_INVALID_REG;

         type_t type = tree_type(name);
         if (type_is_unconstrained(type)) {
            vcode_reg_t array_reg = lower_attr_prefix(lu, name);
            left_reg  = lower_array_left(lu, type, dim, array_reg);
            right_reg = lower_array_right(lu, type, dim, array_reg);
            dir_reg   = lower_array_dir(lu, type, dim, array_reg);
         }
         else {
            tree_t r = range_of(type, dim);
            const range_kind_t rkind = tree_subkind(r);
            if (rkind == RANGE_TO) {
               return predef == ATTR_LOW
                  ? lower_range_left(lu, r) : lower_range_right(lu, r);
            }
            else if (rkind == RANGE_DOWNTO) {
               return predef == ATTR_LOW
                  ? lower_range_right(lu, r) : lower_range_left(lu, r);
            }

            left_reg  = lower_range_left(lu, r);
            right_reg = lower_range_right(lu, r);
            dir_reg   = lower_range_dir(lu, r);
         }

         if (predef == ATTR_LOW)
            return emit_select(dir_reg, right_reg, left_reg);
         else
            return emit_select(dir_reg, left_reg, right_reg);
      }

   case ATTR_LENGTH:
      {
         const int dim = lower_get_attr_dimension(expr);
         vcode_reg_t name_reg = lower_attr_prefix(lu, name);
         vcode_reg_t len_reg =
            lower_array_len(lu, tree_type(name), dim, name_reg);
         return emit_cast(lower_type(tree_type(expr)),
                          VCODE_INVALID_TYPE, len_reg);
      }

   case ATTR_ELEMENT:
      {
         vcode_reg_t array_reg = lower_attr_prefix(lu, name);
         type_t type = tree_type(name);
         type_t elem = lower_elem_recur(type);
         vcode_reg_t null_reg = emit_null(vtype_pointer(lower_type(elem)));
         return lower_wrap_element(lu, type, array_reg, null_reg);
      }

   case ATTR_ASCENDING:
      {
         type_t type = tree_type(name);
         const int dim = lower_get_attr_dimension(expr);
         if (lower_const_bounds(type))
            return emit_const(vtype_bool(),
                              direction_of(type, dim) == RANGE_TO);
         else {
            vcode_reg_t name_reg = lower_attr_prefix(lu, name);
            return emit_not(lower_array_dir(lu, type, dim, name_reg));
         }
      }

   case ATTR_LAST_EVENT:
   case ATTR_LAST_ACTIVE:
      {
         type_t name_type = tree_type(name);
         vcode_reg_t name_reg = lower_attr_prefix(lu, name);
         vcode_reg_t len_reg = VCODE_INVALID_REG;
         if (type_is_array(name_type)) {
            len_reg = lower_array_total_len(lu, name_type, name_reg);
            name_reg = lower_array_data(name_reg);
         }

         if (predef == ATTR_LAST_EVENT)
            return emit_last_event(name_reg, len_reg);
         else if (predef == ATTR_LAST_ACTIVE)
            return emit_last_active(name_reg, len_reg);
         else
            return emit_driving_flag(name_reg, len_reg);
      }

   case ATTR_DRIVING_VALUE:
      {
         type_t name_type = tree_type(name);
         vcode_reg_t name_reg = lower_attr_prefix(lu, name);
         if (type_is_array(name_type)) {
            vcode_reg_t len_reg =
               lower_array_total_len(lu, name_type, name_reg);
            vcode_reg_t ptr_reg = emit_driving_value(name_reg, len_reg);
            if (lower_const_bounds(name_type))
               return ptr_reg;
            else
               return lower_wrap(lu, name_type, ptr_reg);
         }
         else {
            vcode_reg_t ptr_reg =
               emit_driving_value(name_reg, VCODE_INVALID_REG);
            return emit_load_indirect(ptr_reg);
         }
      }

   case ATTR_EVENT:
      return lower_signal_flag(lu, name, emit_event_flag);

   case ATTR_ACTIVE:
      return lower_signal_flag(lu, name, emit_active_flag);

   case ATTR_DRIVING:
      return lower_signal_flag(lu, name, emit_driving_flag);

   case ATTR_LAST_VALUE:
      return lower_last_value(lu, name);

   case ATTR_INSTANCE_NAME:
   case ATTR_PATH_NAME:
   case ATTR_SIMPLE_NAME:
      return lower_name_attr(lu, name, predef);

   case ATTR_IMAGE:
      {
         tree_t value = tree_value(tree_param(expr, 0));
         type_t type = tree_type(value);
         type_t base = type_base_recur(type);
         ident_t func = ident_prefix(type_ident(base), ident_new("image"), '$');
         vcode_type_t ctype = vtype_char();
         vcode_type_t strtype = vtype_uarray(1, ctype, ctype);

         vcode_reg_t arg_reg = lower_attr_param(lu, value, base, C_CONSTANT);

         vcode_reg_t args[] = {
            lower_context_for_call(lu, func),
            arg_reg,
         };
         return emit_fcall(func, strtype, strtype, VCODE_CC_PREDEF, args, 2);
      }

   case ATTR_VALUE:
      {
         type_t name_type = tree_type(name);
         tree_t value = tree_value(tree_param(expr, 0));
         type_t value_type = tree_type(value);

         vcode_reg_t value_reg = lower_rvalue(lu, value);

         if (lower_have_signal(value_reg))
            value_reg = emit_resolved(value_reg);

         if (lower_const_bounds(value_type))
            value_reg = lower_wrap(lu, value_type, value_reg);

         type_t base = type_base_recur(name_type);
         ident_t func = ident_prefix(type_ident(base), ident_new("value"), '$');
         vcode_reg_t args[] = {
            lower_context_for_call(lu, func),
            value_reg
         };
         vcode_reg_t reg = emit_fcall(func, lower_type(base),
                                      lower_bounds(base),
                                      VCODE_CC_PREDEF, args, 2);

         if (type_is_scalar(name_type))
            lower_check_scalar_bounds(lu, reg, name_type, expr, NULL);
         else if (type_is_array(name_type) && lower_const_bounds(name_type)) {
            vcode_reg_t locus = lower_debug_locus(expr);
            lower_check_array_sizes(lu, name_type, base, VCODE_INVALID_REG,
                                    reg, locus);
         }

         return reg;
      }

   case ATTR_SUCC:
      {
         tree_t value = tree_value(tree_param(expr, 0));
         vcode_reg_t arg_reg = lower_attr_param(lu, value, NULL, C_CONSTANT);
         return emit_add(arg_reg, emit_const(vcode_reg_type(arg_reg), 1));
      }

   case ATTR_PRED:
      {
         tree_t value = tree_value(tree_param(expr, 0));
         vcode_reg_t arg_reg = lower_attr_param(lu, value, NULL, C_CONSTANT);
         return emit_sub(arg_reg, emit_const(vcode_reg_type(arg_reg), 1));
      }

   case ATTR_LEFTOF:
      {
         tree_t value = tree_value(tree_param(expr, 0));
         vcode_reg_t arg_reg = lower_attr_param(lu, value, NULL, C_CONSTANT);

         type_t type = tree_type(expr);
         const int dir =
            (type_is_enum(type) || direction_of(type, 0) == RANGE_TO) ? -1 : 1;
         return emit_add(arg_reg, emit_const(vcode_reg_type(arg_reg), dir));
      }

   case ATTR_RIGHTOF:
      {
         tree_t value = tree_value(tree_param(expr, 0));
         vcode_reg_t arg_reg = lower_attr_param(lu, value, NULL, C_CONSTANT);

         type_t type = tree_type(expr);
         const int dir =
            (type_is_enum(type) || direction_of(type, 0) == RANGE_TO) ? 1 : -1;
         return emit_add(arg_reg, emit_const(vcode_reg_type(arg_reg), dir));
      }

   case ATTR_POS:
      {
         tree_t value = tree_value(tree_param(expr, 0));
         vcode_reg_t arg_reg = lower_attr_param(lu, value, NULL, C_CONSTANT);

         type_t type = tree_type(expr);
         return emit_cast(lower_type(type), lower_bounds(type), arg_reg);
      }

   case ATTR_VAL:
      {
         tree_t value = tree_value(tree_param(expr, 0));
         vcode_reg_t arg_reg = lower_attr_param(lu, value, NULL, C_CONSTANT);

         type_t type = tree_type(expr);
         lower_check_scalar_bounds(lu, arg_reg, type, expr, NULL);
         return emit_cast(lower_type(type), lower_bounds(type), arg_reg);
      }

   case ATTR_STABLE:
   case ATTR_QUIET:
      {
         tree_t param = tree_value(tree_param(expr, 0));
         vcode_reg_t param_reg = lower_attr_param(lu, param, NULL, C_SIGNAL);

         type_t name_type = tree_type(name);
         vcode_reg_t name_reg = lower_lvalue(lu, name), len_reg;
         if (type_is_array(name_type)) {
            len_reg = lower_array_total_len(lu, name_type, name_reg);
            name_reg = lower_array_data(name_reg);
         }
         else
            len_reg = emit_const(vtype_offset(), 1);

         vcode_reg_t flag_reg, time_reg;
         if (predef == ATTR_STABLE) {
            time_reg = emit_last_event(name_reg, len_reg);
            flag_reg = emit_event_flag(name_reg, len_reg);
         }
         else {
            time_reg = emit_last_active(name_reg, len_reg);
            flag_reg = emit_active_flag(name_reg, len_reg);
         }

         vcode_reg_t cmp_reg = emit_cmp(VCODE_CMP_GEQ, time_reg, param_reg);
         return emit_and(cmp_reg, emit_not(flag_reg));
      }

   case ATTR_REFLECT:
      return lower_reflect_attr(lu, expr);

   default:
      fatal_at(tree_loc(expr), "cannot lower attribute %s (%d)",
               istr(tree_ident(expr)), predef);
   }
}

static vcode_reg_t lower_qualified(lower_unit_t *lu, tree_t expr)
{
   tree_t value = tree_value(expr);

   type_t from_type = tree_type(value);
   type_t to_type = tree_type(expr);

   vcode_reg_t value_reg = lower_rvalue(lu, value);

   if (type_is_array(to_type)) {
      const bool from_const = lower_const_bounds(from_type);
      const bool to_const = lower_const_bounds(to_type);

      if (to_const && !from_const)
         return lower_array_data(value_reg);
      else if (!to_const && from_const)
         return lower_wrap(lu, from_type, value_reg);
   }

   return value_reg;
}

static vcode_reg_t lower_cond_value(lower_unit_t *lu, tree_t expr)
{
   vcode_block_t exit_bb = VCODE_INVALID_BLOCK;

   // TODO: vcode really needs phi nodes...
   type_t type = tree_type(expr);
   vcode_type_t vtype = lower_type(type);
   vcode_type_t vbounds = lower_bounds(type);
   vcode_var_t temp_var = lower_temp_var(lu, "cond", vtype, vbounds);

   const int nconds = tree_conds(expr);
   for (int i = 0; i < nconds; i++) {
      tree_t c = tree_cond(expr, i);
      assert(tree_kind(c) == T_COND_EXPR);

      vcode_block_t next_bb = VCODE_INVALID_BLOCK;

      if (tree_has_value(c)) {
         vcode_reg_t test = lower_rvalue(lu, tree_value(c));

         vcode_block_t btrue = emit_block();

         if (i == nconds - 1) {
            if (exit_bb == VCODE_INVALID_BLOCK)
               exit_bb = emit_block();
            next_bb = exit_bb;
         }
         else
            next_bb = emit_block();

         emit_cond(test, btrue, next_bb);
         vcode_select_block(btrue);
      }

      vcode_reg_t result_reg = lower_rvalue(lu, tree_result(c));

      if (type_is_array(type) && lower_const_bounds(type)) {
         vcode_reg_t count_reg =
            lower_array_total_len(lu, type, VCODE_INVALID_REG);
         vcode_reg_t dest_reg = emit_index(temp_var, VCODE_INVALID_REG);
         vcode_reg_t src_reg = lower_array_data(result_reg);
         emit_copy(dest_reg, src_reg, count_reg);
      }
      else if (type_is_record(type)) {
         vcode_reg_t dest_reg = emit_index(temp_var, VCODE_INVALID_REG);
         emit_copy(dest_reg, result_reg, VCODE_INVALID_REG);
      }
      else
         emit_store(result_reg, temp_var);

      if (exit_bb == VCODE_INVALID_BLOCK)
         exit_bb = emit_block();
      emit_jump(exit_bb);

      if (next_bb == VCODE_INVALID_BLOCK)
         break;
      else
         vcode_select_block(next_bb);
   }

   if (exit_bb != VCODE_INVALID_BLOCK)
      vcode_select_block(exit_bb);

   if (type_is_scalar(type))
      return emit_load(temp_var);
   else
      return emit_index(temp_var, VCODE_INVALID_REG);
}

static vcode_reg_t lower_expr(lower_unit_t *lu, tree_t expr, expr_ctx_t ctx)
{
   PUSH_DEBUG_INFO(expr);

   switch (tree_kind(expr)) {
   case T_FCALL:
   case T_PROT_FCALL:
      return lower_fcall(lu, expr, VCODE_INVALID_REG);
   case T_LITERAL:
      return lower_literal(expr);
   case T_STRING:
      return lower_string_literal(expr, false);
   case T_REF:
      return lower_ref(lu, expr, ctx);
   case T_EXTERNAL_NAME:
      return lower_external_name(expr, ctx);
   case T_AGGREGATE:
      return lower_aggregate(lu, expr, VCODE_INVALID_VAR);
   case T_ARRAY_REF:
      return lower_array_ref(lu, expr, ctx);
   case T_ARRAY_SLICE:
      return lower_array_slice(lu, expr, ctx);
   case T_RECORD_REF:
      return lower_record_ref(lu, expr, ctx);
   case T_PROT_REF:
      return lower_prot_ref(lu, expr);
   case T_NEW:
      return lower_new(lu, expr);
   case T_ALL:
      return lower_all(lu, expr, ctx);
   case T_TYPE_CONV:
      return lower_type_conv(lu, expr);
   case T_ATTR_REF:
      return lower_attr_ref(lu, expr);
   case T_QUALIFIED:
      return lower_qualified(lu, expr);
   case T_OPEN:
      return VCODE_INVALID_REG;
   case T_COND_VALUE:
      return lower_cond_value(lu, expr);
   default:
      fatal_at(tree_loc(expr), "cannot lower expression kind %s",
               tree_kind_str(tree_kind(expr)));
   }
}

static vcode_reg_t lower_nested_default_value(lower_unit_t *lu, type_t type)
{
   if (type_is_scalar(type))
      return lower_range_left(lu, range_of(type, 0));
   else if (type_is_array(type)) {
      assert(lower_const_bounds(type));
      type_t elem = lower_elem_recur(type);
      vcode_reg_t elem_reg = lower_nested_default_value(lu, elem);
      const int size = lower_array_const_size(type);

      if (vtype_is_scalar(vcode_reg_type(elem_reg)))
         return emit_const_rep(lower_type(type), elem_reg, size);
      else {
         vcode_reg_t *values LOCAL = xmalloc_array(size, sizeof(vcode_reg_t));
         for (int i = 0; i < size; i++)
            values[i] = elem_reg;
         return emit_const_array(lower_type(type), values, size);
      }
   }
   else if (type_is_record(type)) {
      assert(lower_const_bounds(type));
      const int nfields = type_fields(type);
      vcode_type_t vtype = lower_type(type);

      vcode_reg_t *values LOCAL = xmalloc_array(nfields, sizeof(vcode_reg_t));
      for (int i = 0; i < nfields; i++) {
         type_t ftype = tree_type(type_field(type, i));
         values[i] = lower_nested_default_value(lu, ftype);
      }

      return emit_const_record(vtype, values, nfields);
   }
   else if (type_is_access(type))
      return emit_null(lower_type(type));

   fatal_trace("cannot handle type %s in lower_nested_default_value",
               type_pp(type));
}

static vcode_reg_t lower_default_value(lower_unit_t *lu, type_t type,
                                       vcode_reg_t hint_reg)
{
   if (type_is_scalar(type))
      return lower_range_left(lu, range_of(type, 0));
   else if (type_is_array(type)) {
      assert(!type_is_unconstrained(type));

      type_t elem_type = lower_elem_recur(type);

      if (lower_const_bounds(type)) {
         if (type_is_scalar(elem_type)) {
            vcode_reg_t elem_reg = lower_nested_default_value(lu, elem_type);
            const int size = lower_array_const_size(type);
            if (hint_reg == VCODE_INVALID_REG) {
               vcode_type_t vtype = lower_type(type);
               return emit_address_of(emit_const_rep(vtype, elem_reg, size));
            }
            else {
               vcode_reg_t count_reg = emit_const(vtype_offset(), size);
               vcode_reg_t data_reg = lower_array_data(hint_reg);
               emit_memset(data_reg, elem_reg, count_reg);
               return hint_reg;
            }
         }
         else
            return emit_address_of(lower_nested_default_value(lu, type));
      }

      vcode_reg_t count_reg = lower_array_total_len(lu, type, hint_reg);
      vcode_reg_t def_reg =
         lower_default_value(lu, elem_type, VCODE_INVALID_REG);

      if (hint_reg == VCODE_INVALID_REG) {
         vcode_type_t velem = lower_type(elem_type);
         vcode_type_t vbounds = lower_bounds(elem_type);
         hint_reg = emit_alloc(velem, vbounds, count_reg);
      }

      vcode_reg_t data_reg = lower_array_data(hint_reg);

      if (type_is_scalar(elem_type))
         emit_memset(data_reg, def_reg, count_reg);
      else {
         // Loop to initialise the array
         vcode_type_t voffset = vtype_offset();
         vcode_var_t i_var = lower_temp_var(lu, "i", voffset, voffset);
         emit_store(emit_const(voffset, 0), i_var);

         vcode_block_t body_bb = emit_block();
         vcode_block_t exit_bb = emit_block();

         vcode_block_t null_reg = emit_cmp(VCODE_CMP_EQ, count_reg,
                                           emit_const(voffset, 0));
         emit_cond(null_reg, exit_bb, body_bb);

         vcode_select_block(body_bb);

         vcode_reg_t i_reg = emit_load(i_var);
         vcode_reg_t ptr_reg = emit_array_ref(data_reg, i_reg);

         if (type_is_scalar(elem_type))
            emit_store_indirect(def_reg, ptr_reg);
         else if (type_is_record(elem_type))
            lower_new_record(lu, elem_type, ptr_reg, def_reg);
         else
            emit_store_indirect(def_reg, ptr_reg);

         vcode_reg_t next_reg = emit_add(i_reg, emit_const(voffset, 1));
         emit_store(next_reg, i_var);

         vcode_reg_t done_reg = emit_cmp(VCODE_CMP_EQ, next_reg, count_reg);
         emit_cond(done_reg, exit_bb, body_bb);

         vcode_select_block(exit_bb);
         lower_release_temp(lu, i_var);
      }

      return hint_reg;
   }
   else if (type_is_record(type)) {
      if (lower_const_bounds(type))
         return emit_address_of(lower_nested_default_value(lu, type));

      type_t base = type_base_recur(type);
      const int nfields = type_fields(base);
      vcode_type_t vtype = lower_type(type);

      vcode_reg_t mem_reg = hint_reg;
      if (hint_reg == VCODE_INVALID_REG) {
         vcode_var_t tmp_var = lower_temp_var(lu, "def", vtype, vtype);
         mem_reg = emit_index(tmp_var, VCODE_INVALID_REG);
      }

      for (int i = 0; i < nfields; i++) {
         tree_t f = type_field(base, i);
         vcode_reg_t ptr_reg = emit_record_ref(mem_reg, i);

         type_t ftype = tree_type(f);
         if (base != type && type_is_unconstrained(ftype)) {
            tree_t ec = type_constraint_for_field(type, f);
            assert(ec != NULL);
            ftype = tree_type(ec);
         }

         if (type_is_scalar(ftype)) {
            vcode_reg_t def_reg = lower_default_value(lu, ftype, ptr_reg);
            emit_store_indirect(def_reg, ptr_reg);
         }
         else if (type_is_array(ftype)) {
            if (have_uarray_ptr(ptr_reg)) {
               // Need to allocate memory for the array
               type_t elem_type = lower_elem_recur(ftype);
               vcode_type_t vtype = lower_type(elem_type);
               vcode_type_t vbounds = lower_bounds(elem_type);

               vcode_reg_t count_reg =
                  lower_array_total_len(lu, ftype, VCODE_INVALID_REG);
               vcode_reg_t mem_reg = emit_alloc(vtype, vbounds, count_reg);
               vcode_reg_t def_reg = lower_default_value(lu, ftype, mem_reg);
               assert(def_reg == mem_reg);

               vcode_reg_t wrap_reg =
                  lower_wrap_with_new_bounds(lu, ftype, tree_type(f),
                                             def_reg, mem_reg);
               emit_store_indirect(wrap_reg, ptr_reg);
            }
            else {
               vcode_reg_t def_reg = lower_default_value(lu, ftype, ptr_reg);
               vcode_reg_t count_reg =
                  lower_array_total_len(lu, ftype, VCODE_INVALID_REG);
               emit_copy(ptr_reg, def_reg, count_reg);
            }
         }
         else if (type_is_record(ftype)) {
            vcode_reg_t def_reg =
               lower_default_value(lu, ftype, VCODE_INVALID_REG);

            emit_copy(ptr_reg, def_reg, VCODE_INVALID_REG);
         }
         else {
            vcode_reg_t def_reg = lower_default_value(lu, ftype, ptr_reg);
            emit_store_indirect(def_reg, ptr_reg);
         }
      }

      return mem_reg;
   }
   else if (type_is_access(type))
      return emit_null(lower_type(type));
   else
      fatal_trace("cannot handle type %s in lower_default_value",
                  type_pp(type));
}

static void lower_report(lower_unit_t *lu, tree_t stmt)
{
   assert(!tree_has_value(stmt));

   vcode_reg_t severity = lower_rvalue(lu, tree_severity(stmt));

   vcode_reg_t message = VCODE_INVALID_REG, length = VCODE_INVALID_REG;
   if (tree_has_message(stmt)) {
      tree_t m = tree_message(stmt);

      vcode_reg_t message_wrapped = lower_rvalue(lu, m);
      message = lower_array_data(message_wrapped);
      length  = lower_array_len(lu, tree_type(m), 0, message_wrapped);
   }

   vcode_reg_t locus = lower_debug_locus(stmt);
   emit_report(message, length, severity, locus);
}

static bool lower_can_hint_assert(tree_t expr)
{
   if (tree_kind(expr) != T_FCALL)
      return false;

   switch (tree_subkind(tree_ref(expr))) {
   case S_SCALAR_EQ:
   case S_SCALAR_NEQ:
   case S_SCALAR_LT:
   case S_SCALAR_LE:
   case S_SCALAR_GT:
   case S_SCALAR_GE:
      return true;
   default:
      return false;
   }
}

static void lower_assert(lower_unit_t *lu, tree_t stmt)
{
   if (!tree_has_value(stmt)) {
      lower_report(lu, stmt);
      return;
   }

   tree_t value = tree_value(stmt);

   vcode_reg_t value_reg,
      hint_left_reg = VCODE_INVALID_REG,
      hint_right_reg = VCODE_INVALID_REG;

   if (!tree_has_message(stmt) && lower_can_hint_assert(value))
      value_reg = lower_builtin(lu, value, tree_subkind(tree_ref(value)),
                                &hint_left_reg, &hint_right_reg);
   else
      value_reg = lower_rvalue(lu, value);

   int64_t value_const;
   if (vcode_reg_const(value_reg, &value_const) && value_const)
      return;

   vcode_block_t fail_bb = VCODE_INVALID_BLOCK;
   vcode_block_t exit_bb = VCODE_INVALID_BLOCK;

   tree_t message = tree_has_message(stmt) ? tree_message(stmt) : NULL;
   tree_t severity = tree_severity(stmt);

   // If evaluating the message or severity expressions can have side
   // effects then branch to a new block
   const bool has_side_effects =
      (message != NULL && !lower_side_effect_free(message))
      || !lower_side_effect_free(severity);

   if (has_side_effects) {
      fail_bb = emit_block();
      exit_bb = emit_block();
      emit_cond(value_reg, exit_bb, fail_bb);
      vcode_select_block(fail_bb);
   }

   vcode_reg_t message_reg = VCODE_INVALID_REG, length_reg = VCODE_INVALID_REG;
   if (message != NULL) {
      vcode_reg_t message_wrapped = lower_rvalue(lu, message);
      message_reg = lower_array_data(message_wrapped);
      length_reg  = lower_array_len(lu, tree_type(message), 0, message_wrapped);
   }

   vcode_reg_t severity_reg = lower_rvalue(lu, severity);

   vcode_reg_t locus = lower_debug_locus(value);
   emit_assert(value_reg, message_reg, length_reg, severity_reg, locus,
               hint_left_reg, hint_right_reg);

   if (exit_bb != VCODE_INVALID_BLOCK) {
      emit_jump(exit_bb);
      vcode_select_block(exit_bb);
   }
}

static void lower_sched_event_field_cb(lower_unit_t *lu, tree_t field,
                                       vcode_reg_t ptr, vcode_reg_t unused,
                                       vcode_reg_t locus, void *ctx)
{
   type_t type = tree_type(field);
   if (!type_is_homogeneous(type))
      lower_for_each_field(lu, type, ptr, VCODE_INVALID_REG, locus,
                           lower_sched_event_field_cb, ctx);
   else {
      vcode_reg_t nets_reg = emit_load_indirect(ptr);
      vcode_reg_t data_reg = lower_array_data(nets_reg);
      vcode_reg_t count_reg = lower_type_width(lu, type, nets_reg);

      const bool clear = (uintptr_t)ctx;
      if (clear)
         emit_clear_event(data_reg, count_reg);
      else
         emit_sched_event(data_reg, count_reg);
   }
}

static void lower_sched_event(lower_unit_t *lu, tree_t on, vcode_reg_t wake)
{
   type_t type = tree_type(on);

   vcode_reg_t nets_reg = lower_lvalue(lu, on);
   assert(nets_reg != VCODE_INVALID_REG);

   if (!type_is_homogeneous(type))
      lower_for_each_field(lu, type, nets_reg, VCODE_INVALID_REG,
                           VCODE_INVALID_REG, lower_sched_event_field_cb, NULL);
   else {
      vcode_reg_t count_reg = lower_type_width(lu, type, nets_reg);
      vcode_reg_t data_reg = lower_array_data(nets_reg);

      if (wake != VCODE_INVALID_REG)
         emit_implicit_event(data_reg, count_reg, wake);
      else
         emit_sched_event(data_reg, count_reg);
   }
}

static void lower_clear_event(lower_unit_t *lu, tree_t on)
{
   type_t type = tree_type(on);

   vcode_reg_t nets_reg = lower_lvalue(lu, on);
   assert(nets_reg != VCODE_INVALID_REG);

   if (!type_is_homogeneous(type))
      lower_for_each_field(lu, type, nets_reg, VCODE_INVALID_REG,
                           VCODE_INVALID_REG, lower_sched_event_field_cb,
                           (void *)1);
   else {
      vcode_reg_t count_reg = lower_type_width(lu, type, nets_reg);
      vcode_reg_t data_reg = lower_array_data(nets_reg);
      emit_clear_event(data_reg, count_reg);
   }
}

static void lower_wait(lower_unit_t *lu, tree_t wait)
{
   const bool is_static = !!(tree_flags(wait) & TREE_F_STATIC_WAIT);
   assert(!is_static || (!tree_has_delay(wait) && !tree_has_value(wait)));

   const int ntriggers = tree_triggers(wait);

   if (!is_static) {
      // The _sched_event for static waits is emitted in the reset block
      for (int i = 0; i < ntriggers; i++)
         lower_sched_event(lu, tree_trigger(wait, i), VCODE_INVALID_REG);
   }

   const bool has_delay = tree_has_delay(wait);
   const bool has_value = tree_has_value(wait);

   vcode_reg_t delay = VCODE_INVALID_REG;
   if (has_delay)
      delay = lower_rvalue(lu, tree_delay(wait));

   vcode_var_t remain = VCODE_INVALID_VAR;
   if (has_value && has_delay) {
      ident_t remain_i = ident_new("wait_remain");
      remain = vcode_find_var(remain_i);
      if (remain == VCODE_INVALID_VAR) {
         vcode_type_t time = vtype_time();
         remain = emit_var(time, time, remain_i, 0);
      }

      vcode_type_t rtype = vtype_time();
      vcode_reg_t now_reg = emit_fcall(ident_new("_std_standard_now"),
                                       rtype, rtype, VCODE_CC_FOREIGN, NULL, 0);
      vcode_reg_t abs_reg = emit_add(now_reg, delay);
      emit_store(abs_reg, remain);
   }

   vcode_block_t resume = emit_block();
   emit_wait(resume, delay);

   vcode_select_block(resume);

   if (!is_static) {
      for (int i = 0; i < ntriggers; i++)
         lower_clear_event(lu, tree_trigger(wait, i));
   }

   if (has_value) {
      // Generate code to loop until condition is met

      vcode_reg_t until_reg = lower_rvalue(lu, tree_value(wait));

      vcode_reg_t timeout_reg = VCODE_INVALID_REG;
      vcode_reg_t done_reg = until_reg;
      if (has_delay) {
         vcode_type_t rtype = vtype_time();
         vcode_reg_t remain_reg = emit_load(remain);
         vcode_reg_t now_reg = emit_fcall(ident_new("_std_standard_now"),
                                          rtype, rtype, VCODE_CC_FOREIGN,
                                          NULL, 0);
         timeout_reg = emit_sub(remain_reg, now_reg);

         vcode_reg_t expired_reg = emit_cmp(VCODE_CMP_EQ, timeout_reg,
                                            emit_const(vtype_time(), 0));
         done_reg = emit_or(expired_reg, until_reg);
      }

      vcode_block_t done_bb  = emit_block();
      vcode_block_t again_bb = emit_block();

      emit_cond(done_reg, done_bb, again_bb);

      vcode_select_block(again_bb);

      for (int i = 0; i < ntriggers; i++)
         lower_sched_event(lu, tree_trigger(wait, i), VCODE_INVALID_REG);

      emit_wait(resume, timeout_reg);

      vcode_select_block(done_bb);
   }
}

static void lower_check_array_sizes(lower_unit_t *lu, type_t ltype,
                                    type_t rtype, vcode_reg_t lval,
                                    vcode_reg_t rval, vcode_reg_t locus)
{
   const int ndims = dimension_of(ltype);
   for (int i = 0; i < ndims; i++) {
      vcode_reg_t llen_reg = lower_array_len(lu, ltype, i, lval);
      vcode_reg_t rlen_reg = lower_array_len(lu, rtype, i, rval);

      vcode_reg_t dim_reg = VCODE_INVALID_REG;
      if (ndims > 1)
         dim_reg = emit_const(vtype_offset(), i + 1);

      emit_length_check(llen_reg, rlen_reg, locus, dim_reg);
   }
}

static void lower_find_matching_refs(tree_t ref, void *context)
{
   tree_t *decl = (tree_t *)context;
   if (tree_ref(ref) == *decl)
      *decl = NULL;
}

static bool lower_can_hint_aggregate(tree_t target, tree_t value)
{
   if (tree_kind(value) != T_AGGREGATE)
      return false;

   type_t type = tree_type(target);
   if (!lower_const_bounds(type))
      return false;

   tree_t ref = name_to_ref(target);
   if (ref == NULL)
      return false;

   tree_t decl = tree_ref(ref);
   tree_visit_only(value, lower_find_matching_refs, &decl, T_REF);
   return decl != NULL;
}

static bool lower_can_hint_concat(tree_t target, tree_t value)
{
   if (tree_kind(value) != T_FCALL)
      return false;

   tree_t fdecl = tree_ref(value);
   if (tree_subkind(fdecl) != S_CONCAT)
      return false;

   if (!lower_const_bounds(tree_type(target)))
      return false;

   tree_t ref = name_to_ref(target);
   if (ref == NULL)
      return false;

   tree_t decl = tree_ref(ref);
   tree_visit_only(value, lower_find_matching_refs, &decl, T_REF);
   return decl != NULL;
}

static int lower_count_target_parts(tree_t target, int depth)
{
   if (tree_kind(target) == T_AGGREGATE) {
      int count = 0;
      const int nassocs = tree_assocs(target);
      for (int i = 0; i < nassocs; i++) {
         tree_t value = tree_value(tree_assoc(target, i));
         count += lower_count_target_parts(value, depth + 1);
      }

      return count + (depth > 0 ? 2 : 1);
   }
   else
      return depth == 0 ? 2 : 1;
}

static void lower_fill_target_parts(lower_unit_t *lu, tree_t target,
                                    part_kind_t kind, target_part_t **ptr)
{
   type_t target_type = tree_type(target);

   if (tree_kind(target) == T_AGGREGATE) {
      const bool is_record = type_is_record(target_type);
      const int ndims = dimension_of(target_type);

      if (kind != PART_ALL) {
         (*ptr)->reg    = VCODE_INVALID_REG;
         (*ptr)->off    = VCODE_INVALID_REG;
         (*ptr)->target = NULL;
         (*ptr)->kind   = kind == PART_FIELD ? PART_PUSH_FIELD : PART_PUSH_ELEM;
         ++(*ptr);
      }

      vcode_reg_t sum_reg = emit_const(vtype_offset(), 0);

      const int nassocs = tree_assocs(target);
      for (int i = 0; i < nassocs; i++) {
         tree_t value = tree_value(tree_assoc(target, i));

         part_kind_t newkind;
         if (is_record)
            newkind = PART_FIELD;
         else if (ndims == 1 && type_eq(tree_type(value), target_type))
            newkind = PART_SLICE;
         else
            newkind = PART_ELEM;

         lower_fill_target_parts(lu, value, newkind, ptr);

         vcode_reg_t len_reg;
         if (newkind == PART_SLICE)
            len_reg = (*ptr - 1)->off;
         else
            len_reg = emit_const(vtype_offset(), 1);

         sum_reg = emit_add(sum_reg, len_reg);
      }

      (*ptr)->reg    = VCODE_INVALID_REG;
      (*ptr)->off    = sum_reg;
      (*ptr)->target = NULL;
      (*ptr)->kind   = PART_POP;
      ++(*ptr);
   }
   else {
      (*ptr)->reg    = lower_lvalue(lu, target);
      (*ptr)->target = target;
      (*ptr)->kind   = kind;

      if (type_is_array(target_type))
         (*ptr)->off = lower_array_total_len(lu, target_type, (*ptr)->reg);
      else
         (*ptr)->off = emit_const(vtype_offset(), 1);

      ++(*ptr);

      if (kind == PART_ALL) {
         (*ptr)->reg    = VCODE_INVALID_REG;
         (*ptr)->off    = (*ptr - 1)->off;
         (*ptr)->target = NULL;
         (*ptr)->kind   = PART_POP;
         ++(*ptr);
      }
   }
}

static void lower_copy_record(lower_unit_t *lu, type_t type,
                              vcode_reg_t dst_ptr, vcode_reg_t src_ptr,
                              vcode_reg_t locus)
{
   if (dst_ptr == src_ptr)
      return;
   else if (lower_trivially_copyable(type))
      emit_copy(dst_ptr, src_ptr, VCODE_INVALID_REG);
   else {
      type_t base = lower_base_type(type);
      ident_t helper_func =
         ident_prefix(type_ident(base), ident_new("copy"), '$');

      vcode_reg_t args[] = {
         lower_context_for_call(lu, helper_func),
         dst_ptr,
         src_ptr,
         locus
      };
      emit_fcall(helper_func, VCODE_INVALID_TYPE, VCODE_INVALID_TYPE,
                 VCODE_CC_VHDL, args, ARRAY_LEN(args));
   }
}

static void lower_copy_array(lower_unit_t *lu, type_t dst_type, type_t src_type,
                             vcode_reg_t dst_array, vcode_reg_t src_array,
                             vcode_reg_t locus)
{
   if (dst_array == src_array)
      return;
   else if (lower_trivially_copyable(dst_type)) {
      lower_check_array_sizes(lu, dst_type, src_type, dst_array,
                              src_array, locus);

      vcode_reg_t count_reg = lower_array_total_len(lu, dst_type, dst_array);

      vcode_reg_t src_data = lower_array_data(src_array);
      vcode_reg_t dst_data = lower_array_data(dst_array);
      emit_copy(dst_data, src_data, count_reg);
   }
   else {
      type_t base = lower_base_type(dst_type);
      ident_t helper_func =
         ident_prefix(type_ident(base), ident_new("copy"), '$');

      assert(vcode_reg_kind(dst_array) == VCODE_TYPE_UARRAY);
      assert(vcode_reg_kind(src_array) == VCODE_TYPE_UARRAY);

      vcode_reg_t args[] = {
         lower_context_for_call(lu, helper_func),
         dst_array,
         src_array,
         locus
      };
      emit_fcall(helper_func, VCODE_INVALID_TYPE, VCODE_INVALID_TYPE,
                 VCODE_CC_VHDL, args, ARRAY_LEN(args));
   }
}

static void lower_var_assign_target(lower_unit_t *lu, target_part_t **ptr,
                                    tree_t where, vcode_reg_t rhs,
                                    type_t rhs_type)
{
   int fieldno = 0;
   for (const target_part_t *p = (*ptr)++; p->kind != PART_POP; p = (*ptr)++) {
      vcode_reg_t src_reg = rhs;
      type_t src_type = rhs_type;
      if (p->kind == PART_FIELD || p->kind == PART_PUSH_FIELD) {
         assert(vcode_reg_kind(rhs) == VCODE_TYPE_POINTER);
         src_reg = emit_record_ref(rhs, fieldno);
         src_type = tree_type(type_field(src_type, fieldno));
         fieldno++;
      }

      if (p->kind == PART_PUSH_FIELD || p->kind == PART_PUSH_ELEM) {
         lower_var_assign_target(lu, ptr, where, src_reg, src_type);
         continue;
      }
      else if (p->reg == VCODE_INVALID_REG)
         continue;

      if (p->kind == PART_ELEM)
         src_type = type_elem(src_type);

      type_t type = tree_type(p->target);

      if (p->kind != PART_SLICE && type_is_array(type)) {
         vcode_reg_t locus = lower_debug_locus(p->target);
         lower_check_array_sizes(lu, type, src_type, p->reg, src_reg, locus);
      }

      if (p->kind == PART_ELEM || p->kind == PART_SLICE)
         src_reg = lower_array_data(src_reg);

      vcode_reg_t scalar_reg = src_reg;
      if ((p->kind == PART_ELEM || p->kind == PART_FIELD)
          && !type_is_composite(type))
         scalar_reg = emit_load_indirect(src_reg);

      if (type_is_scalar(type)) {
         lower_check_scalar_bounds(lu, scalar_reg, type, where, p->target);
         emit_store_indirect(scalar_reg, p->reg);
      }
      else if (type_is_array(type)) {
         vcode_reg_t data_reg = lower_array_data(src_reg);
         vcode_reg_t count_reg = lower_array_total_len(lu, type, p->reg);
         vcode_reg_t dest_reg = lower_array_data(p->reg);

         emit_copy(dest_reg, data_reg, count_reg);
      }
      else if (type_is_record(type)) {
         vcode_reg_t locus = lower_debug_locus(where);
         lower_copy_record(lu, type, p->reg, src_reg, locus);
      }
      else
         emit_store_indirect(scalar_reg, p->reg);

      if (p->kind == PART_ELEM || p->kind == PART_SLICE) {
         assert(vcode_reg_kind(src_reg) == VCODE_TYPE_POINTER);
         rhs = emit_array_ref(src_reg, p->off);
      }
   }
}

static void lower_var_assign(lower_unit_t *lu, tree_t stmt)
{
   tree_t value  = tree_value(stmt);
   tree_t target = tree_target(stmt);
   type_t type   = tree_type(target);

   const bool is_var_decl =
      tree_kind(target) == T_REF && tree_kind(tree_ref(target)) == T_VAR_DECL;
   const bool is_scalar = type_is_scalar(type);
   const bool is_access = type_is_access(type);

   if (is_scalar || is_access) {
      vcode_reg_t value_reg = lower_rvalue(lu, value);
      vcode_var_t var = VCODE_INVALID_VAR;
      int hops = 0;
      if (is_scalar)
         lower_check_scalar_bounds(lu, value_reg, type, value, target);
      else
         value_reg = lower_incomplete_access(value_reg, type_designated(type));

      if (is_var_decl
          && (var = lower_get_var(lu, tree_ref(target),
                                  &hops)) != VCODE_INVALID_VAR
          && hops == 0)
         emit_store(value_reg, var);
      else {
         vcode_reg_t ptr_reg = lower_lvalue(lu, target);
         emit_store_indirect(value_reg, ptr_reg);
      }
   }
   else if (tree_kind(target) == T_AGGREGATE) {
      const int nparts = lower_count_target_parts(target, 0);
      target_part_t *parts LOCAL = xmalloc_array(nparts, sizeof(target_part_t));

      target_part_t *ptr = parts;
      lower_fill_target_parts(lu, target, PART_ALL, &ptr);
      assert(ptr == parts + nparts);

      vcode_reg_t rhs = lower_rvalue(lu, value);

      if (type_is_array(type)) {
         vcode_reg_t rhs_len = lower_array_len(lu, tree_type(value), 0, rhs);
         vcode_reg_t locus = lower_debug_locus(target);
         assert(parts[nparts - 1].kind == PART_POP);
         emit_length_check(parts[nparts - 1].off, rhs_len, locus,
                           VCODE_INVALID_REG);
      }

      ptr = parts;
      lower_var_assign_target(lu, &ptr, value, rhs, tree_type(value));
      assert(ptr == parts + nparts);
   }
   else if (type_is_array(type)) {
      vcode_reg_t target_reg  = lower_lvalue(lu, target);

      vcode_reg_t value_reg;
      if (lower_can_hint_aggregate(target, value))
         value_reg = lower_aggregate(lu, value, lower_array_data(target_reg));
      else if (lower_can_hint_concat(target, value)) {
         vcode_reg_t hint_reg = lower_array_data(target_reg);
         vcode_reg_t count_reg = lower_array_total_len(lu, type, target_reg);
         value_reg = lower_concat(lu, value, hint_reg, count_reg);
      }
      else
         value_reg = lower_known_subtype(lu, value, type, target_reg);

      vcode_reg_t locus = lower_debug_locus(target);

      type_t value_type = tree_type(value);
      lower_copy_array(lu, type, value_type, target_reg, value_reg, locus);
   }
   else {
      vcode_reg_t target_reg = lower_lvalue(lu, target);

      vcode_reg_t value_reg;
      if (lower_can_hint_aggregate(target, value))
         value_reg = lower_aggregate(lu, value, target_reg);
      else
         value_reg = lower_known_subtype(lu, value, type, target_reg);

      vcode_reg_t locus = lower_debug_locus(stmt);
      lower_copy_record(lu, type, target_reg, value_reg, locus);
   }
}

static void lower_signal_target_field_cb(lower_unit_t *lu, tree_t field,
                                         vcode_reg_t dst_ptr,
                                         vcode_reg_t src_ptr,
                                         vcode_reg_t locus, void *__ctx)
{
   type_t type = tree_type(field);
   vcode_reg_t *args = __ctx;

   if (!type_is_homogeneous(type))
      lower_for_each_field(lu, type, dst_ptr, src_ptr, locus,
                           lower_signal_target_field_cb, __ctx);
   else if (type_is_array(type)) {
      vcode_reg_t src_array = VCODE_INVALID_REG;
      vcode_reg_t dst_array = VCODE_INVALID_REG;

      vcode_reg_t nets_reg;
      if (have_uarray_ptr(dst_ptr)) {
         dst_array = emit_load_indirect(dst_ptr);
         nets_reg  = lower_array_data(dst_array);
      }
      else
         nets_reg = emit_load_indirect(dst_ptr);

      vcode_reg_t array_reg;
      if (have_uarray_ptr(src_ptr)) {
         src_array = emit_load_indirect(src_ptr);
         array_reg = lower_resolved(lu, type, src_array);
      }
      else
         array_reg = lower_resolved(lu, type, src_ptr);

      lower_check_array_sizes(lu, type, type, dst_array, src_array, locus);

      vcode_reg_t count_reg = lower_array_total_len(lu, type, array_reg);
      vcode_reg_t data_reg  = lower_array_data(array_reg);
      emit_sched_waveform(nets_reg, count_reg, data_reg,
                          args[0], args[1]);
   }
   else {
      vcode_reg_t nets_reg = emit_load_indirect(dst_ptr);
      vcode_reg_t data_reg = lower_resolved(lu, type, src_ptr);
      emit_sched_waveform(nets_reg, emit_const(vtype_offset(), 1),
                          data_reg, args[0], args[1]);
   }
}

static void lower_signal_assign_target(lower_unit_t *lu, target_part_t **ptr,
                                       tree_t where, vcode_reg_t rhs,
                                       type_t rhs_type, vcode_reg_t reject,
                                       vcode_reg_t after)
{
   int fieldno = 0;
   for (const target_part_t *p = (*ptr)++; p->kind != PART_POP; p = (*ptr)++) {
      vcode_reg_t src_reg = rhs;
      type_t src_type = rhs_type;
      if (p->kind == PART_FIELD || p->kind == PART_PUSH_FIELD) {
         assert(vcode_reg_kind(rhs) == VCODE_TYPE_POINTER);
         src_reg = emit_record_ref(rhs, fieldno);
         src_type = tree_type(type_field(src_type, fieldno));
         fieldno++;
      }

      if (p->kind == PART_PUSH_FIELD || p->kind == PART_PUSH_ELEM) {
         lower_signal_assign_target(lu, ptr, where, src_reg, src_type,
                                    reject, after);
         continue;
      }
      else if (p->reg == VCODE_INVALID_REG)
         continue;

      if (p->kind == PART_ELEM)
         src_type = type_elem(src_type);

      type_t type = tree_type(p->target);

      if (p->kind != PART_SLICE && type_is_array(type)) {
         vcode_reg_t locus = lower_debug_locus(p->target);
         lower_check_array_sizes(lu, type, src_type, p->reg, src_reg, locus);
      }

      if (p->kind == PART_ELEM || p->kind == PART_SLICE)
         src_reg = lower_array_data(src_reg);

      if (type_is_scalar(type))
         lower_check_scalar_bounds(lu, src_reg, type, where, p->target);

      if (!type_is_homogeneous(type)) {
         vcode_reg_t args[2] = { reject, after };
         vcode_reg_t locus = lower_debug_locus(where);
         lower_for_each_field(lu, type, p->reg, src_reg, locus,
                              lower_signal_target_field_cb, &args);
      }
      else if (type_is_array(type)) {
         vcode_reg_t resolved_reg = lower_resolved(lu, type, src_reg);
         vcode_reg_t data_reg = lower_array_data(resolved_reg);
         vcode_reg_t count_reg = lower_array_total_len(lu, type, p->reg);
         vcode_reg_t nets_raw = lower_array_data(p->reg);

         emit_sched_waveform(nets_raw, count_reg, data_reg, reject, after);
      }
      else {
         vcode_reg_t data_reg = lower_resolved(lu, type, src_reg);
         emit_sched_waveform(p->reg, emit_const(vtype_offset(), 1),
                             data_reg, reject, after);
      }

      if (p->kind == PART_ELEM || p->kind == PART_SLICE) {
         assert(vcode_reg_kind(src_reg) == VCODE_TYPE_POINTER);
         rhs = emit_array_ref(src_reg, p->off);
      }
   }
}

static void lower_disconnect_target(lower_unit_t *lu, target_part_t **ptr,
                                    vcode_reg_t reject, vcode_reg_t after)
{
   for (const target_part_t *p = (*ptr)++; p->kind != PART_POP; p = (*ptr)++) {
      if (p->kind == PART_PUSH_FIELD || p->kind == PART_PUSH_ELEM) {
         lower_disconnect_target(lu, ptr, reject, after);
         continue;
      }
      else if (p->reg == VCODE_INVALID_REG)
         continue;

      vcode_reg_t nets_reg = lower_array_data(p->reg);

      type_t type = tree_type(p->target);

      if (type_is_record(type)) {
         // XXX: this seems wrong
         const int width = type_width(type);
         emit_disconnect(nets_reg, emit_const(vtype_offset(), width),
                         reject, after);
      }
      else {
         vcode_reg_t count_reg = lower_type_width(lu, type, p->reg);
         emit_disconnect(nets_reg, count_reg, reject, after);
      }
   }
}

static void lower_signal_assign(lower_unit_t *lu, tree_t stmt)
{
   tree_t target = tree_target(stmt);

   const int nparts = lower_count_target_parts(target, 0);
   target_part_t *parts LOCAL = xmalloc_array(nparts, sizeof(target_part_t));

   target_part_t *ptr = parts;
   lower_fill_target_parts(lu, target, PART_ALL, &ptr);
   assert(ptr == parts + nparts);

   const int nwaveforms = tree_waveforms(stmt);
   for (int i = 0; i < nwaveforms; i++) {
      tree_t w = tree_waveform(stmt, i);

      tree_t delay = NULL;
      vcode_reg_t delay_reg;
      if (tree_has_delay(w)) {
         delay = tree_delay(w);
         delay_reg = lower_rvalue(lu, delay);
      }
      else
         delay_reg = emit_const(vtype_time(), 0);

      vcode_reg_t reject_reg;
      if (i == 0 && tree_has_reject(stmt)) {
         tree_t reject = tree_reject(stmt);
         if (reject == delay) {
            // If delay is the same as reject ensure the expression is
            // only evaluated once
            reject_reg = delay_reg;
         }
         else
            reject_reg = lower_rvalue(lu, reject);
      }
      else {
         // All but the first waveform have zero reject time
         reject_reg = emit_const(vtype_time(), 0);
      }

      vcode_var_t tmp_var = VCODE_INVALID_VAR;

      target_part_t *ptr = parts;
      if (tree_has_value(w)) {
         tree_t wvalue = tree_value(w);
         type_t wtype = tree_type(wvalue);
         vcode_reg_t rhs = VCODE_INVALID_REG;
         if (ptr->kind == PART_ALL) {
            if (lower_can_hint_concat(ptr->target, wvalue)) {
               type_t ptype = tree_type(ptr->target);

               vcode_type_t vtype = lower_type(ptype);
               vcode_type_t vbounds = lower_bounds(ptype);
               tmp_var = lower_temp_var(lu, "tmp", vtype, vbounds);

               vcode_reg_t count_reg =
                  lower_array_total_len(lu, ptype, ptr->reg);
               vcode_reg_t hint_reg = emit_index(tmp_var, VCODE_INVALID_REG);
               rhs = lower_concat(lu, wvalue, hint_reg, count_reg);
            }
            else if (lower_can_hint_aggregate(ptr->target, wvalue)) {
               type_t ptype = tree_type(ptr->target);

               vcode_type_t vtype = lower_type(ptype);
               vcode_type_t vbounds = lower_bounds(ptype);
               tmp_var = lower_temp_var(lu, "tmp", vtype, vbounds);

               vcode_reg_t hint_reg  = emit_index(tmp_var, VCODE_INVALID_REG);
               rhs = lower_aggregate(lu, wvalue, hint_reg);
            }
            else if (type_is_record(wtype) && lower_is_signal_ref(wvalue)) {
               // If the RHS is another record signal we can avoid creating
               // a temporary resolved copy of the whole record by
               // resolving each field individually in the field callback.
               rhs = lower_lvalue(lu, wvalue);
            }
            else if (standard() >= STD_19) {
               type_t ptype = tree_type(ptr->target);
               rhs = lower_known_subtype(lu, wvalue, ptype, ptr->reg);
            }
         }

         if (rhs == VCODE_INVALID_REG)
            rhs = lower_rvalue(lu, wvalue);

         if (ptr->kind != PART_ALL && type_is_array(wtype)) {
            vcode_reg_t rhs_len = lower_array_len(lu, wtype, 0, rhs);
            vcode_reg_t locus = lower_debug_locus(target);
            assert(parts[nparts - 1].kind == PART_POP);
            emit_length_check(parts[nparts - 1].off, rhs_len, locus,
                              VCODE_INVALID_REG);
         }

         lower_signal_assign_target(lu, &ptr, wvalue, rhs, wtype,
                                    reject_reg, delay_reg);
      }
      else
         lower_disconnect_target(lu, &ptr, reject_reg, delay_reg);
      assert(ptr == parts + nparts);

      if (tmp_var != VCODE_INVALID_VAR)
         lower_release_temp(lu, tmp_var);
   }
}

static void lower_force_field_cb(lower_unit_t *lu, tree_t field,
                                 vcode_reg_t ptr, vcode_reg_t value,
                                 vcode_reg_t locus, void *__ctx)
{
   type_t type = tree_type(field);
   if (type_is_homogeneous(type)) {
      vcode_reg_t nets_reg = emit_load_indirect(ptr);
      vcode_reg_t count_reg = lower_type_width(lu, type, nets_reg);
      emit_force(lower_array_data(nets_reg), count_reg, value);
   }
   else
      lower_for_each_field(lu, type, ptr, value, locus,
                           lower_force_field_cb, NULL);
}

static void lower_force(lower_unit_t *lu, tree_t stmt)
{
   tree_t target = tree_target(stmt);
   type_t type = tree_type(target);

   vcode_reg_t nets = lower_lvalue(lu, target);

   tree_t value = tree_value(stmt);
   vcode_reg_t value_reg = lower_rvalue(lu, value);

   if (type_is_array(type)) {
      vcode_reg_t count_reg = lower_array_total_len(lu, type, nets);
      vcode_reg_t data_reg = lower_array_data(value_reg);
      emit_force(lower_array_data(nets), count_reg, data_reg);
   }
   else if (type_is_record(type)) {
      vcode_reg_t locus = lower_debug_locus(value);
      lower_for_each_field(lu, type, nets, value_reg, locus,
                           lower_force_field_cb, NULL);
   }
   else
      emit_force(nets, emit_const(vtype_offset(), 1), value_reg);
}

static void lower_release(lower_unit_t *lu, tree_t stmt)
{
   tree_t target = tree_target(stmt);
   type_t type = tree_type(target);

   vcode_reg_t nets = lower_lvalue(lu, target);

   if (type_is_array(type)) {
      vcode_reg_t count_reg = lower_array_total_len(lu, type, nets);
      emit_release(lower_array_data(nets), count_reg);
   }
   else if (type_is_record(type)) {
      assert(false);
   }
   else
      emit_release(nets, emit_const(vtype_offset(), 1));
}

static void lower_sequence(lower_unit_t *lu, tree_t block, loop_stack_t *loops)
{
   const int nstmts = tree_stmts(block);
   for (int i = 0; i < nstmts; i++)
      lower_stmt(lu, tree_stmt(block, i), loops);
}

static void lower_if(lower_unit_t *lu, tree_t stmt, loop_stack_t *loops)
{
   vcode_block_t exit_bb = VCODE_INVALID_BLOCK;

   const int nconds = tree_conds(stmt);
   for (int i = 0; i < nconds; i++) {
      tree_t c = tree_cond(stmt, i);
      vcode_block_t next_bb = VCODE_INVALID_BLOCK;
      cover_push_scope(lu->cover, c);

      if (tree_has_value(c)) {
         vcode_reg_t test = lower_rvalue(lu, tree_value(c));

         if (cover_enabled(lu->cover, COVER_MASK_BRANCH))
            lower_branch_coverage(lu, c, COV_FLAG_FALSE | COV_FLAG_TRUE, test);

         vcode_block_t btrue = emit_block();

         if (i == nconds - 1) {
            if (exit_bb == VCODE_INVALID_BLOCK)
               exit_bb = emit_block();
            next_bb = exit_bb;
         }
         else
            next_bb = emit_block();

         emit_cond(test, btrue, next_bb);
         vcode_select_block(btrue);
      }

      lower_sequence(lu, c, loops);

      if (!vcode_block_finished()) {
         if (exit_bb == VCODE_INVALID_BLOCK)
            exit_bb = emit_block();
         emit_jump(exit_bb);
      }

      cover_pop_scope(lu->cover);

      if (next_bb == VCODE_INVALID_BLOCK)
         break;
      else
         vcode_select_block(next_bb);
   }

   if (exit_bb != VCODE_INVALID_BLOCK)
      vcode_select_block(exit_bb);
}

static void lower_leave_subprogram(lower_unit_t *lu)
{
   // Release resources for protected and file variables

   const int ndecls = tree_decls(lu->container);
   for (int i = 0; i < ndecls; i++) {
      tree_t d = tree_decl(lu->container, i);
      switch (tree_kind(d)) {
      case T_VAR_DECL:
         if (type_is_protected(tree_type(d))) {
            int hops = 0;
            vcode_var_t var = lower_get_var(lu, d, &hops);
            assert(var != VCODE_INVALID_VAR);

            vcode_reg_t obj_reg;
            if (hops == 0)
               obj_reg = emit_load(var);
            else
               obj_reg = emit_load_indirect(emit_var_upref(hops, var));

            emit_protected_free(obj_reg);
         }
         break;

      case T_FILE_DECL:
         {
            vcode_block_t open_bb = emit_block();
            vcode_block_t closed_bb = emit_block();

            vcode_reg_t ptr_reg  = lower_var_ref(lu, d, EXPR_LVALUE);
            vcode_reg_t file_reg = emit_load_indirect(ptr_reg);
            vcode_reg_t null_reg = emit_null(lower_type(tree_type(d)));
            vcode_reg_t cmp_reg  = emit_cmp(VCODE_CMP_EQ, file_reg, null_reg);
            emit_cond(cmp_reg, closed_bb, open_bb);

            vcode_select_block(open_bb);
            emit_file_close(ptr_reg);
            emit_jump(closed_bb);

            vcode_select_block(closed_bb);
         }
         break;

      default:
         break;
      }
   }
}

static void lower_return(lower_unit_t *lu, tree_t stmt)
{
   if (is_subprogram(lu->container))
      lower_leave_subprogram(lu);

   if (tree_has_value(stmt)) {
      tree_t value = tree_value(stmt);
      type_t type = tree_type(value);

      vcode_reg_t value_reg = lower_rvalue(lu, value);

      if (type_is_scalar(type)) {
         lower_check_scalar_bounds(lu, value_reg, type, value, NULL);
         emit_return(value_reg);
      }
      else if (type_is_access(type)) {
         type_t access = type_designated(type);
         emit_return(lower_incomplete_access(value_reg, access));
      }
      else if (type_is_array(type)) {
         type_t rtype = type_result(tree_type(lu->container));
         emit_return(lower_coerce_arrays(lu, type, rtype, value_reg));
      }
      else
         emit_return(value_reg);
   }
   else
      emit_return(VCODE_INVALID_REG);
}

static void lower_pcall(lower_unit_t *lu, tree_t pcall)
{
   tree_t decl = tree_ref(pcall);

   const subprogram_kind_t kind = tree_subkind(decl);
   if (is_builtin(kind)) {
      lower_builtin(lu, pcall, kind, NULL, NULL);
      return;
   }

   bool use_fcall;
   switch (vcode_unit_kind()) {
   case VCODE_UNIT_FUNCTION:
   case VCODE_UNIT_THUNK:
      use_fcall = true;
      break;
   default:
      use_fcall = !!(tree_flags(decl) & TREE_F_NEVER_WAITS);
      break;
   }

   const int nparams = tree_params(pcall);
   SCOPED_A(vcode_reg_t) args = AINIT;

   const vcode_cc_t cc = lower_cc_for_call(pcall);
   ident_t name = tree_ident2(decl);

   if (tree_kind(pcall) == T_PROT_PCALL && tree_has_name(pcall))
      APUSH(args, lower_rvalue(lu, tree_name(pcall)));
   else if (cc != VCODE_CC_FOREIGN)
      APUSH(args, lower_context_for_call(lu, name));

   const int arg0 = args.count;
   for (int i = 0; i < nparams; i++) {
      vcode_reg_t arg_reg = lower_subprogram_arg(lu, pcall, i);
      if (!use_fcall)
         vcode_heap_allocate(arg_reg);

      if (kind == S_FOREIGN && vcode_reg_kind(arg_reg) == VCODE_TYPE_UARRAY) {
         // Do not pass wrapped arrays into foreign functions
         APUSH(args, emit_unwrap(arg_reg));

         vcode_type_t vint64 = vtype_int(INT64_MIN, INT64_MAX);
         const int ndims = vtype_dims(vcode_reg_type(arg_reg));
         for (int i = 0; i < ndims; i++) {
            vcode_reg_t len_reg = emit_uarray_len(arg_reg, 0);
            APUSH(args, emit_cast(vint64, vint64, len_reg));
         }
      }
      else
         APUSH(args, arg_reg);
   }

   if (use_fcall)
      emit_fcall(name, VCODE_INVALID_TYPE, VCODE_INVALID_TYPE,
                 cc, args.items, args.count);
   else {
      vcode_block_t resume_bb = emit_block();

      emit_pcall(name, args.items, args.count, resume_bb);
      vcode_select_block(resume_bb);
      emit_resume(name);
   }

   // Copy any out parameters passed as aggregates
   const int nports = tree_ports(decl);
   for (int i = 0; i < nports; i++) {
      tree_t p = tree_port(decl, i);
      if (tree_class(p) != C_VARIABLE || tree_subkind(p) == PORT_IN)
         continue;

      tree_t value = tree_value(tree_param(pcall, i));
      if (tree_kind(value) != T_AGGREGATE)
         continue;

      const int nparts = lower_count_target_parts(value, 0);
      target_part_t *parts LOCAL = xmalloc_array(nparts, sizeof(target_part_t));

      target_part_t *ptr = parts;
      lower_fill_target_parts(lu, value, PART_ALL, &ptr);
      assert(ptr == parts + nparts);

      vcode_reg_t arg_reg = args.items[arg0 + i];

      ptr = parts;
      lower_var_assign_target(lu, &ptr, value, arg_reg, tree_type(value));
      assert(ptr == parts + nparts);
   }
}

static void lower_wait_free_cb(tree_t t, void *ctx)
{
   int *count = ctx;

   const tree_kind_t kind = tree_kind(t);
   if (kind == T_PCALL || kind == T_PROT_PCALL || kind == T_WAIT)
      (*count)++;
}

static bool lower_is_wait_free(tree_t stmt)
{
   switch (vcode_unit_kind()) {
   case VCODE_UNIT_PROCEDURE:
   case VCODE_UNIT_PROCESS:
      {
         int count = 0;
         tree_visit(stmt, lower_wait_free_cb, &count);
         return count == 0;
      }
   default:
      return true;   // Can never wait
   }
}

static void lower_for(lower_unit_t *lu, tree_t stmt, loop_stack_t *loops)
{
   tree_t r = tree_range(stmt, 0);
   vcode_reg_t left_reg  = lower_range_left(lu, r);
   vcode_reg_t right_reg = lower_range_right(lu, r);
   vcode_reg_t dir_reg   = lower_range_dir(lu, r);
   vcode_reg_t null_reg  = emit_range_null(left_reg, right_reg, dir_reg);

   vcode_block_t exit_bb = VCODE_INVALID_BLOCK;

   int64_t null_const;
   if (vcode_reg_const(null_reg, &null_const) && null_const)
      return;   // Loop range is always null
   else {
      vcode_block_t init_bb = emit_block();
      exit_bb = emit_block();
      emit_cond(null_reg, exit_bb, init_bb);
      vcode_select_block(init_bb);
   }

   tree_t idecl = tree_decl(stmt, 0);

   vcode_type_t vtype  = lower_type(tree_type(idecl));
   vcode_type_t bounds = vtype;

   vcode_reg_t step_down = emit_const(vtype, -1);
   vcode_reg_t step_up   = emit_const(vtype, 1);
   vcode_reg_t step_reg  = emit_select(dir_reg, step_down, step_up);

   // If the body of the loop may wait we need to store the bounds in a
   // variable as the range is evaluated only on entry to the loop
   const bool is_wait_free = lower_is_wait_free(stmt);
   vcode_var_t right_var = VCODE_INVALID_VAR, step_var = VCODE_INVALID_VAR;
   if (!is_wait_free) {
      right_var = lower_temp_var(lu, "right", vtype, vtype);
      emit_store(right_reg, right_var);

      step_var = lower_temp_var(lu, "step", vtype, vtype);
      emit_store(step_reg, step_var);
   }

   int64_t lconst, rconst, dconst;
   const bool l_is_const = vcode_reg_const(left_reg, &lconst);
   const bool r_is_const = vcode_reg_const(right_reg, &rconst);
   if (l_is_const && r_is_const)
      bounds = vtype_int(MIN(lconst, rconst), MAX(lconst, rconst));
   else if ((l_is_const || r_is_const) && vcode_reg_const(dir_reg, &dconst)) {
      if (dconst == RANGE_TO)
         bounds = vtype_int(l_is_const ? lconst : vtype_low(vtype),
                            r_is_const ? rconst : vtype_high(vtype));
      else
         bounds = vtype_int(r_is_const ? rconst : vtype_low(vtype),
                            l_is_const ? lconst : vtype_high(vtype));
   }

   ident_t ident = ident_prefix(tree_ident(idecl), tree_ident(stmt), '.');
   vcode_var_t ivar = emit_var(vtype, bounds, ident, 0);

   emit_store(left_reg, ivar);

   vcode_block_t body_bb = emit_block();
   emit_jump(body_bb);
   vcode_select_block(body_bb);

   vcode_reg_t ireg = VCODE_INVALID_VAR;
   if (is_wait_free) {
      ireg = emit_load(ivar);
      lower_put_vcode_obj(idecl, ireg, lu);
   }
   else
      lower_put_vcode_obj(idecl, ivar | PARAM_VAR_BIT, lu);

   if (exit_bb == VCODE_INVALID_BLOCK)
      exit_bb = emit_block();

   loop_stack_t this = {
      .up      = loops,
      .name    = tree_ident(stmt),
      .test_bb = VCODE_INVALID_BLOCK,
      .exit_bb = exit_bb
   };

   lower_sequence(lu, stmt, &this);

   if (this.test_bb != VCODE_INVALID_BLOCK) {
      // Loop body contained a "next" statement
      if (!vcode_block_finished())
         emit_jump(this.test_bb);
      vcode_select_block(this.test_bb);
   }

   vcode_reg_t rightn_reg = right_reg;
   if (right_var != VCODE_INVALID_VAR)
      rightn_reg = emit_load(right_var);

   vcode_reg_t stepn_reg = step_reg;
   if (step_var != VCODE_INVALID_VAR)
      stepn_reg = emit_load(step_var);

   if (ireg == VCODE_INVALID_REG)
      ireg = emit_load(ivar);

   if (!vcode_block_finished()) {
      vcode_reg_t next_reg = emit_add(ireg, stepn_reg);
      emit_store(next_reg, ivar);

      vcode_reg_t done_reg = emit_cmp(VCODE_CMP_EQ, ireg, rightn_reg);
      emit_cond(done_reg, exit_bb, body_bb);
   }

   vcode_select_block(exit_bb);

   if (!is_wait_free) {
      lower_release_temp(lu, right_var);
      lower_release_temp(lu, step_var);
   }
}

static void lower_while(lower_unit_t *lu, tree_t stmt, loop_stack_t *loops)
{
   vcode_block_t test_bb, body_bb, exit_bb;

   if (tree_has_value(stmt)) {
      test_bb = emit_block();
      body_bb = emit_block();
      exit_bb = emit_block();

      emit_jump(test_bb);

      vcode_select_block(test_bb);
      vcode_reg_t test = lower_rvalue(lu, tree_value(stmt));

      if (cover_enabled(lu->cover, COVER_MASK_BRANCH))
         lower_branch_coverage(lu, stmt, COV_FLAG_FALSE | COV_FLAG_TRUE, test);

      emit_cond(test, body_bb, exit_bb);
   }
   else {
      test_bb = body_bb =
         vcode_block_empty() ? vcode_active_block() : emit_block();
      exit_bb = emit_block();

      emit_jump(body_bb);
   }

   vcode_select_block(body_bb);
   cover_push_scope(lu->cover, stmt);

   loop_stack_t this = {
      .up      = loops,
      .name    = tree_ident(stmt),
      .test_bb = test_bb,
      .exit_bb = exit_bb
   };

   lower_sequence(lu, stmt, &this);

   if (!vcode_block_finished())
      emit_jump(test_bb);

   vcode_select_block(exit_bb);
   cover_pop_scope(lu->cover);
}

static void lower_loop_control(lower_unit_t *lu, tree_t stmt,
                               loop_stack_t *loops)
{
   vcode_block_t false_bb = emit_block();

   if (tree_has_value(stmt)) {
      vcode_block_t true_bb = emit_block();
      vcode_reg_t result = lower_rvalue(lu, tree_value(stmt));

      if (cover_enabled(lu->cover, COVER_MASK_BRANCH))
         lower_branch_coverage(lu, stmt, COV_FLAG_FALSE | COV_FLAG_TRUE, result);

      emit_cond(result, true_bb, false_bb);

      vcode_select_block(true_bb);
   }

   ident_t label = tree_ident2(stmt);
   loop_stack_t *it;
   for (it = loops; it != NULL && it->name != label; it = it->up)
      ;
   assert(it != NULL);

   if (tree_kind(stmt) == T_EXIT)
      emit_jump(it->exit_bb);
   else {
      if (it->test_bb == VCODE_INVALID_BLOCK)
         it->test_bb = emit_block();
      emit_jump(it->test_bb);
   }

   vcode_select_block(false_bb);
}

static void lower_case_scalar(lower_unit_t *lu, tree_t stmt,
                              loop_stack_t *loops)
{
   const int nstmts = tree_stmts(stmt);

   vcode_block_t def_bb = VCODE_INVALID_BLOCK;
   vcode_block_t exit_bb = emit_block();

   vcode_reg_t value_reg = lower_rvalue(lu, tree_value(stmt));

   int nchoices = 0;
   for (int i = 0; i < nstmts; i++) {
      tree_t alt = tree_stmt(stmt, i);

      const int nassocs = tree_assocs(alt);
      nchoices += nassocs;

      for (int j = 0; j < nassocs; j++) {
         tree_t a = tree_assoc(alt, j);

         // Pre-filter range choices in case the number of elements is large
         if (tree_subkind(a) == A_RANGE) {
            cover_push_scope(lu->cover, a);

            tree_t r = tree_range(a, 0);
            vcode_reg_t left_reg = lower_range_left(lu, r);
            vcode_reg_t right_reg = lower_range_right(lu, r);

            const range_kind_t dir = tree_subkind(r);
            vcode_reg_t low_reg = dir == RANGE_TO ? left_reg : right_reg;
            vcode_reg_t high_reg = dir == RANGE_TO ? right_reg : left_reg;

            vcode_reg_t lcmp_reg = emit_cmp(VCODE_CMP_GEQ, value_reg, low_reg);
            vcode_reg_t hcmp_reg = emit_cmp(VCODE_CMP_LEQ, value_reg, high_reg);
            vcode_reg_t hit_reg = emit_and(lcmp_reg, hcmp_reg);

            if (cover_enabled(lu->cover, COVER_MASK_BRANCH))
               lower_branch_coverage(lu, a, COV_FLAG_CHOICE, hit_reg);

            vcode_block_t skip_bb = emit_block();

            vcode_block_t hit_bb = emit_block();
            emit_cond(hit_reg, hit_bb, skip_bb);
            vcode_select_block(hit_bb);

            lower_sequence(lu, alt, loops);

            if (!vcode_block_finished())
               emit_jump(exit_bb);

            vcode_select_block(skip_bb);

            cover_pop_scope(lu->cover);
         }
      }
   }

   vcode_block_t start_bb = vcode_active_block();

   vcode_reg_t *cases LOCAL = xcalloc_array(nchoices, sizeof(vcode_reg_t));
   vcode_block_t *blocks LOCAL = xcalloc_array(nchoices, sizeof(vcode_block_t));

   int cptr = 0;
   for (int i = 0; i < nstmts; i++) {
      tree_t alt = tree_stmt(stmt, i);
      vcode_block_t hit_bb = VCODE_INVALID_BLOCK;

      const int nassocs = tree_assocs(alt);
      for (int j = 0; j < nassocs; j++) {
         tree_t a = tree_assoc(alt, j);
         const assoc_kind_t kind = tree_subkind(a);

         if (kind == A_RANGE)
            continue;    // Handled separately above

         cover_push_scope(lu->cover, a);

         if (hit_bb == VCODE_INVALID_BLOCK)
            hit_bb = emit_block();

         if (kind == A_OTHERS) {
            assert(def_bb == VCODE_INVALID_BLOCK);
            def_bb = hit_bb;

            if (cover_enabled(lu->cover, COVER_MASK_BRANCH)) {
               vcode_select_block(hit_bb);
               vcode_reg_t true_reg = emit_const(vtype_bool(), 1);
               lower_branch_coverage(lu, a, COV_FLAG_CHOICE, true_reg);
            }
         }
         else {
            vcode_select_block(start_bb);
            cases[cptr] = lower_rvalue(lu, tree_name(a));
            blocks[cptr] = hit_bb;

            if (cover_enabled(lu->cover, COVER_MASK_BRANCH)) {
               vcode_select_block(hit_bb);
               cover_push_scope(lu->cover, a);
               vcode_reg_t hit_reg =
                  emit_cmp(VCODE_CMP_EQ, cases[cptr], value_reg);
               lower_branch_coverage(lu, a, COV_FLAG_CHOICE, hit_reg);
               cover_pop_scope(lu->cover);
            }

            cptr++;
         }

         cover_pop_scope(lu->cover);
      }

      if (hit_bb == VCODE_INVALID_BLOCK)
         continue;

      vcode_select_block(hit_bb);

      lower_sequence(lu, alt, loops);

      if (!vcode_block_finished())
         emit_jump(exit_bb);
   }

   assert(cptr <= nchoices);

   if (def_bb == VCODE_INVALID_BLOCK)
      def_bb = exit_bb;

   vcode_select_block(start_bb);

   emit_case(value_reg, def_bb, cases, blocks, cptr);
   vcode_select_block(exit_bb);
}

static void lower_case_array(lower_unit_t *lu, tree_t stmt, loop_stack_t *loops)
{
   vcode_type_t vint64 = vtype_int(INT64_MIN, INT64_MAX);
   vcode_type_t voffset = vtype_offset();

   tree_t value = tree_value(stmt);
   type_t type = tree_type(value);
   vcode_reg_t val_reg = lower_rvalue(lu, tree_value(stmt));
   vcode_reg_t data_ptr = lower_array_data(val_reg);

   vcode_block_t def_bb   = VCODE_INVALID_BLOCK;
   vcode_block_t exit_bb  = emit_block();
   vcode_block_t hit_bb   = VCODE_INVALID_BLOCK;
   vcode_block_t start_bb = vcode_active_block();

   int64_t length = INT64_MAX;
   if (type_is_unconstrained(type)
       || !folded_length(range_of(type, 0), &length)) {
      vcode_reg_t length_reg = lower_array_len(lu, type, 0, val_reg);

      // Need a runtime check of expression length against choice length
      vcode_reg_t c0_length_reg;
      tree_t a0 = tree_assoc(tree_stmt(stmt, 0), 0);
      if (tree_subkind(a0) == A_NAMED) {
         tree_t c0 = tree_name(a0);
         vcode_reg_t c0_reg = lower_rvalue(lu, c0);
         c0_length_reg = lower_array_len(lu, tree_type(c0), 0, c0_reg);
      }
      else
         c0_length_reg = length_reg;   // Only others choice

      if (!vcode_reg_const(length_reg, &length)) {
         if (!vcode_reg_const(c0_length_reg, &length)) {
            error_at(tree_loc(stmt), "cannot determine length of "
                     "case expression");
            return;
         }
      }

      vcode_reg_t locus = lower_debug_locus(stmt);
      emit_length_check(c0_length_reg, length_reg, locus, VCODE_INVALID_REG);
   }

   type_t base = type_base_recur(type_elem(type));
   assert(type_kind(base) == T_ENUM);

   const int nbits = ilog2(type_enum_literals(base));
   const bool exact_map = length * nbits <= 64;

   // Limit the number of cases branches we generate so it can be
   // efficiently implemented with a jump table
   static const int max_cases = 256;

   if (!exact_map) {
      // Hash function may have collisions so need to emit calls to
      // comparison function
      if (vcode_reg_kind(val_reg) != VCODE_TYPE_UARRAY)
         val_reg = lower_wrap(lu, type, val_reg);
   }

   vcode_type_t enc_type = VCODE_INVALID_TYPE;
   vcode_reg_t enc_reg = VCODE_INVALID_REG;
   if (exact_map && length <= 4) {
      // Unroll the encoding calculation
      enc_type = voffset;
      enc_reg = emit_const(enc_type, 0);
      for (int64_t i = 0; i < length; i++) {
         vcode_reg_t i_reg = emit_const(voffset, i);
         vcode_reg_t ptr_reg = emit_array_ref(data_ptr, i_reg);
         vcode_reg_t byte_reg = emit_load_indirect(ptr_reg);
         enc_reg = emit_mul(enc_reg, emit_const(enc_type, 1 << nbits));
         enc_reg = emit_add(enc_reg, emit_cast(enc_type, enc_type, byte_reg));
      }
   }
   else {
      enc_type = vint64;
      vcode_var_t enc_var = lower_temp_var(lu, "enc", enc_type, enc_type);
      emit_store(emit_const(enc_type, 0), enc_var);

      vcode_var_t i_var = lower_temp_var(lu, "i", voffset, voffset);
      emit_store(emit_const(voffset, 0), i_var);

      vcode_block_t body_bb = emit_block();
      vcode_block_t exit_bb = start_bb = emit_block();

      emit_jump(body_bb);

      vcode_select_block(body_bb);

      vcode_reg_t i_reg    = emit_load(i_var);
      vcode_reg_t ptr_reg  = emit_array_ref(data_ptr, i_reg);
      vcode_reg_t byte_reg = emit_load_indirect(ptr_reg);
      vcode_reg_t tmp_reg  = emit_load(enc_var);

      if (exact_map)
         tmp_reg = emit_mul(tmp_reg, emit_const(enc_type, 1 << nbits));
      else
         tmp_reg = emit_mul(tmp_reg, emit_const(enc_type, 0x27d4eb2d));
      tmp_reg = emit_add(tmp_reg, emit_cast(enc_type, enc_type, byte_reg));
      emit_store(tmp_reg, enc_var);

      vcode_reg_t i_next = emit_add(i_reg, emit_const(voffset, 1));
      emit_store(i_next, i_var);

      vcode_reg_t done_reg = emit_cmp(VCODE_CMP_EQ, i_next,
                                      emit_const(voffset, length));
      emit_cond(done_reg, exit_bb, body_bb);

      vcode_select_block(exit_bb);

      enc_reg = emit_load(enc_var);

      if (!exact_map)
         enc_reg = emit_rem(enc_reg, emit_const(enc_type, max_cases));

      lower_release_temp(lu, i_var);
      lower_release_temp(lu, enc_var);
   }

   int nchoices = 0;
   const int nstmts = tree_stmts(stmt);
   for (int i = 0; i < nstmts; i++)
      nchoices += tree_assocs(tree_stmt(stmt, i));

   vcode_reg_t *cases LOCAL = xcalloc_array(nchoices, sizeof(vcode_reg_t));
   vcode_block_t *blocks LOCAL = xcalloc_array(nchoices, sizeof(vcode_block_t));
   int64_t *encoding LOCAL = xcalloc_array(nchoices, sizeof(int64_t));

   ident_t cmp_func = NULL;
   vcode_type_t vbool = vtype_bool();
   vcode_block_t fallthrough_bb = VCODE_INVALID_BLOCK;

   if (!exact_map) {
      fallthrough_bb = emit_block();
      cmp_func = lower_predef_func_name(tree_type(value), "=");
   }

   int cptr = 0;
   for (int i = 0; i < nstmts; i++) {
      tree_t alt = tree_stmt(stmt, i);

      const int nassocs = tree_assocs(alt);
      for (int j = 0; j < nassocs; j++) {
         tree_t a = tree_assoc(alt, j);
         const assoc_kind_t kind = tree_subkind(a);
         assert(kind != A_RANGE);

         hit_bb = emit_block();

         cover_push_scope(lu->cover, a);

         if (kind == A_OTHERS) {
            assert(def_bb == VCODE_INVALID_BLOCK);
            def_bb = hit_bb;

             if (cover_enabled(lu->cover, COVER_MASK_BRANCH)) {
                vcode_select_block(hit_bb);
                vcode_reg_t true_reg = emit_const(vtype_bool(), 1);
                lower_branch_coverage(lu, a, COV_FLAG_CHOICE, true_reg);
             }
         }
         else {
            tree_t name = tree_name(a);
            const int exact_bits = exact_map ? nbits : 0;
            int64_t enc = encode_case_choice(name, length, exact_bits);
            if (!exact_map) enc %= max_cases;

            vcode_block_t entry_bb = hit_bb;
            bool have_dup = false;
            if (!exact_map) {
               // There may be collisions in the hash function
               vcode_block_t chain_bb = fallthrough_bb;
               for (int k = 0; k < cptr; k++) {
                  if (encoding[k] == enc) {
                     chain_bb  = blocks[k];
                     blocks[k] = hit_bb;
                     have_dup  = true;
                     break;
                  }
               }

               vcode_select_block(hit_bb);
               hit_bb = emit_block();

               vcode_reg_t name_reg = lower_rvalue(lu, name);
               if (vcode_reg_kind(name_reg) != VCODE_TYPE_UARRAY)
                  name_reg = lower_wrap(lu, type, name_reg);

               vcode_reg_t context_reg = lower_context_for_call(lu, cmp_func);
               vcode_reg_t args[] = { context_reg, name_reg, val_reg };
               vcode_reg_t eq_reg = emit_fcall(cmp_func, vbool, vbool,
                                               VCODE_CC_PREDEF, args, 3);
               emit_cond(eq_reg, hit_bb, chain_bb);
            }

            if (!have_dup) {
               vcode_select_block(start_bb);
               cases[cptr]    = emit_const(enc_type, enc);
               blocks[cptr]   = entry_bb;
               encoding[cptr] = enc;

               // TODO: How to handle have_dup == true ?
               if (cover_enabled(lu->cover, COVER_MASK_BRANCH)) {
                  vcode_select_block(hit_bb);
                  vcode_reg_t hit_reg =
                     emit_cmp(VCODE_CMP_EQ, cases[cptr], enc_reg);
                  lower_branch_coverage(lu, a, COV_FLAG_CHOICE, hit_reg);
               }

               cptr++;
            }
         }

         cover_pop_scope(lu->cover);

         vcode_select_block(hit_bb);

         lower_sequence(lu, alt, loops);

         if (!vcode_block_finished())
            emit_jump(exit_bb);
      }
   }

   assert(cptr <= nchoices);

   if (def_bb == VCODE_INVALID_BLOCK)
      def_bb = exit_bb;

   if (fallthrough_bb != VCODE_INVALID_BLOCK) {
      vcode_select_block(fallthrough_bb);
      emit_jump(def_bb);
   }

   vcode_select_block(start_bb);

   emit_case(enc_reg, def_bb, cases, blocks, cptr);

   vcode_select_block(exit_bb);
}

static void lower_case(lower_unit_t *lu, tree_t stmt, loop_stack_t *loops)
{
   if (type_is_scalar(tree_type(tree_value(stmt))))
      lower_case_scalar(lu, stmt, loops);
   else
      lower_case_array(lu, stmt, loops);
}

static void lower_match_case(lower_unit_t *lu, tree_t stmt, loop_stack_t *loops)
{
   vcode_block_t exit_bb = VCODE_INVALID_BLOCK;

   tree_t value = tree_value(stmt);
   type_t type = tree_type(value);

   const bool is_array = type_is_array(type);
   type_t scalar = is_array ? type_elem(type) : type;

   if (type_eq(scalar, std_type(NULL, STD_BIT))) {
      // The "?=" operator on BIT is the same as "=" so just use a
      // normal case statement
      lower_case(lu, stmt, loops);
      return;
   }

   vcode_reg_t value_reg = lower_rvalue(lu, value);
   if (is_array && vcode_reg_kind(value_reg) != VCODE_TYPE_UARRAY)
      value_reg = lower_wrap(lu, type, value_reg);

   // Call support function to check argument does not contain '-'
   {
      ident_t func = ident_new(
         is_array ? "NVC.IEEE_SUPPORT.CHECK_MATCH_EXPRESSION(Y)"
         : "NVC.IEEE_SUPPORT.CHECK_MATCH_EXPRESSION(U)");
      vcode_reg_t context_reg = lower_context_for_call(lu, func);

      vcode_reg_t args[] = { context_reg, value_reg };
      emit_fcall(func, VCODE_INVALID_REG, VCODE_INVALID_REG,
                 VCODE_CC_VHDL, args, ARRAY_LEN(args));
   }

   ident_t func = ident_new(is_array ? "IEEE.STD_LOGIC_1164.\"?=\"(YY)U"
                            : "IEEE.STD_LOGIC_1164.\"?=\"(UU)U");
   vcode_reg_t context_reg = lower_context_for_call(lu, func);

   vcode_type_t vscalar = lower_type(scalar);
   vcode_reg_t true_reg = emit_const(vscalar, 3);  // STD_LOGIC'POS('1')

   const int nstmts = tree_stmts(stmt);
   for (int i = 0; i < nstmts; i++) {
      tree_t alt = tree_stmt(stmt, i);

      vcode_block_t hit_bb = emit_block();
      vcode_block_t skip_bb = VCODE_INVALID_BLOCK;

      const int nassocs = tree_assocs(alt);
      for (int j = 0; j < nassocs; j++) {
         vcode_block_t loop_bb = VCODE_INVALID_BLOCK;
         vcode_reg_t test_reg = VCODE_INVALID_REG;
         vcode_var_t tmp_var = VCODE_INVALID_VAR;

         tree_t a = tree_assoc(alt, j);
         switch (tree_subkind(a)) {
         case A_NAMED:
            {
               tree_t name = tree_name(a);
               test_reg = lower_rvalue(lu, name);

               if (is_array && vcode_reg_kind(test_reg) != VCODE_TYPE_UARRAY)
                  test_reg = lower_wrap(lu, tree_type(name), test_reg);

               skip_bb = emit_block();
            }
            break;

         case A_RANGE:
            {
               tree_t r = tree_range(a, 0);
               vcode_reg_t left_reg  = lower_range_left(lu, r);
               vcode_reg_t right_reg = lower_range_right(lu, r);
               vcode_reg_t dir_reg   = lower_range_dir(lu, r);

               if (tmp_var == VCODE_INVALID_VAR)
                  tmp_var = lower_temp_var(lu, "i", vscalar, vscalar);

               emit_store(left_reg, tmp_var);

               vcode_reg_t null_reg  =
                  emit_range_null(left_reg, right_reg, dir_reg);

               vcode_block_t body_bb = emit_block();
               loop_bb = emit_block();
               skip_bb = emit_block();

               emit_cond(null_reg, skip_bb, body_bb);

               vcode_select_block(body_bb);

               test_reg = emit_load(tmp_var);

               vcode_select_block(loop_bb);

               vcode_reg_t step_down = emit_const(vscalar, -1);
               vcode_reg_t step_up   = emit_const(vscalar, 1);
               vcode_reg_t step_reg  = emit_select(dir_reg, step_down, step_up);
               vcode_reg_t next_reg  = emit_add(test_reg, step_reg);
               emit_store(next_reg, tmp_var);

               vcode_reg_t done_reg =
                  emit_cmp(VCODE_CMP_EQ, test_reg, right_reg);
               emit_cond(done_reg, skip_bb, body_bb);

               vcode_select_block(body_bb);
            }
            break;

         case A_OTHERS:
            emit_jump(hit_bb);
            continue;
         }

         vcode_reg_t args[] = {
            context_reg,
            value_reg,
            test_reg,
         };
         vcode_reg_t result_reg = emit_fcall(func, vscalar, vscalar,
                                             VCODE_CC_VHDL, args,
                                             ARRAY_LEN(args));
         vcode_reg_t cmp_reg = emit_cmp(VCODE_CMP_EQ, result_reg, true_reg);

         if (loop_bb != VCODE_INVALID_BLOCK)
            emit_cond(cmp_reg, hit_bb, loop_bb);
         else
            emit_cond(cmp_reg, hit_bb, skip_bb);

         if (tmp_var != VCODE_INVALID_VAR)
            lower_release_temp(lu, tmp_var);
      }

      vcode_select_block(hit_bb);
      lower_sequence(lu, alt, loops);

      if (!vcode_block_finished()) {
         if (exit_bb == VCODE_INVALID_BLOCK)
            exit_bb = emit_block();
         emit_jump(exit_bb);
      }

      if (skip_bb != VCODE_INVALID_BLOCK)
         vcode_select_block(skip_bb);
   }

   if (exit_bb != VCODE_INVALID_BLOCK) {
      if (!vcode_block_finished())
         emit_jump(exit_bb);
      vcode_select_block(exit_bb);
   }
}

static void lower_sequential_block(lower_unit_t *lu, tree_t stmt,
                                   loop_stack_t *loops)
{
   lower_decls(lu, stmt);
   lower_sequence(lu, stmt, loops);
}

static void lower_stmt(lower_unit_t *lu, tree_t stmt, loop_stack_t *loops)
{
   PUSH_DEBUG_INFO(stmt);

   if (vcode_block_finished())
      return;   // Unreachable

   cover_push_scope(lu->cover, stmt);
   if (cover_enabled(lu->cover, COVER_MASK_STMT) && cover_is_stmt(stmt)) {
      cover_tag_t *tag = cover_add_tag(stmt, tree_loc(stmt), NULL,
                                       lu->cover, TAG_STMT, 0);
      if (tag != NULL)
         emit_cover_stmt(tag->tag);
   }

   emit_debug_info(tree_loc(stmt));

   switch (tree_kind(stmt)) {
   case T_ASSERT:
      lower_assert(lu, stmt);
      break;
   case T_WAIT:
      lower_wait(lu, stmt);
      break;
   case T_VAR_ASSIGN:
      lower_var_assign(lu, stmt);
      break;
   case T_SIGNAL_ASSIGN:
      lower_signal_assign(lu, stmt);
      break;
   case T_FORCE:
      lower_force(lu, stmt);
      break;
   case T_RELEASE:
      lower_release(lu, stmt);
      break;
   case T_IF:
      lower_if(lu, stmt, loops);
      break;
   case T_RETURN:
      lower_return(lu, stmt);
      break;
   case T_PCALL:
   case T_PROT_PCALL:
      lower_pcall(lu, stmt);
      break;
   case T_WHILE:
      lower_while(lu, stmt, loops);
      break;
   case T_FOR:
      lower_for(lu, stmt, loops);
      break;
   case T_SEQUENCE:
      lower_sequential_block(lu, stmt, loops);
      break;
   case T_EXIT:
   case T_NEXT:
      lower_loop_control(lu, stmt, loops);
      break;
   case T_CASE:
      lower_case(lu, stmt, loops);
      break;
   case T_MATCH_CASE:
      lower_match_case(lu, stmt, loops);
      break;
   default:
      fatal_at(tree_loc(stmt), "cannot lower statement kind %s",
               tree_kind_str(tree_kind(stmt)));
   }

   cover_pop_scope(lu->cover);
}

static void lower_check_indexes(lower_unit_t *lu, type_t from, type_t to,
                                vcode_reg_t array, tree_t where)
{
   const int ndims = dimension_of(to);
   for (int i = 0; i < ndims; i++) {
      type_t index = index_type_of(to, i);
      tree_t r = range_of(index, 0);

      vcode_reg_t ileft_reg  = lower_range_left(lu, r);
      vcode_reg_t iright_reg = lower_range_right(lu, r);
      vcode_reg_t idir_reg   = lower_range_dir(lu, r);

      vcode_reg_t aleft_reg  = lower_array_left(lu, from, i, array);
      vcode_reg_t aright_reg = lower_array_right(lu, from, i, array);
      vcode_reg_t adir_reg   = lower_array_dir(lu, from, i, array);

      vcode_reg_t null_reg = emit_range_null(aleft_reg, aright_reg, adir_reg);

      vcode_block_t after_bb = VCODE_INVALID_BLOCK;

      int64_t null_const;
      if (vcode_reg_const(null_reg, &null_const)) {
         if (null_const == 1)
            continue;   // Array range is statically known to be null
      }
      else {
         vcode_block_t check_bb = emit_block();
         after_bb = emit_block();
         emit_cond(null_reg, after_bb, check_bb);
         vcode_select_block(check_bb);
      }

      vcode_reg_t hint_reg;
      if (type_is_unconstrained(to))
         hint_reg = lower_debug_locus(r);
      else
         hint_reg = lower_debug_locus(range_of(to, i));

      vcode_reg_t locus = where ? lower_debug_locus(where) : hint_reg;

      emit_index_check(aleft_reg, ileft_reg, iright_reg, idir_reg,
                       locus, hint_reg);
      emit_index_check(aright_reg, ileft_reg, iright_reg, idir_reg,
                       locus, hint_reg);

      if (after_bb != VCODE_INVALID_BLOCK) {
         emit_jump(after_bb);
         vcode_select_block(after_bb);
      }
   }
}

static void lower_var_decl(lower_unit_t *lu, tree_t decl)
{
   type_t type = tree_type(decl);
   vcode_type_t vtype = lower_type(type);
   vcode_type_t vbounds = lower_bounds(type);
   const bool is_const = tree_kind(decl) == T_CONST_DECL;

   bool skip_copy = false;
   if (is_const && !tree_has_value(decl)) {
      // Deferred constant in package
      return;
   }
   else if (is_const && type_is_array(type)
            && !lower_const_bounds(type)  // TODO: remove this restriction
            && lower_is_const(tree_value(decl))) {
      skip_copy = true;   // Will be allocated in constant data
   }

   vcode_var_flags_t flags = 0;
   if (is_const) flags |= VAR_CONST;

   vcode_var_t var = emit_var(vtype, vbounds, tree_ident(decl), flags);
   lower_put_vcode_obj(decl, var, lu);

   if (type_is_protected(type)) {
      vcode_reg_t context_reg = lower_context_for_call(lu, type_ident(type));
      vcode_reg_t obj_reg = emit_protected_init(lower_type(type), context_reg);
      emit_store(obj_reg, var);
      lu->flags |= SCOPE_HAS_PROTECTED;
      return;
   }

   emit_debug_info(tree_loc(decl));

   vcode_reg_t dest_reg  = VCODE_INVALID_REG;
   vcode_reg_t count_reg = VCODE_INVALID_REG;

   if (type_is_record(type))
      dest_reg = emit_index(var, VCODE_INVALID_REG);
   else if (type_is_array(type) && !type_is_unconstrained(type)) {
      count_reg = lower_array_total_len(lu, type, VCODE_INVALID_REG);

      if (!lower_const_bounds(type)) {
         type_t scalar_elem = lower_elem_recur(type);
         dest_reg = emit_alloc(lower_type(scalar_elem),
                               lower_bounds(scalar_elem),
                               count_reg);
         emit_store(lower_wrap(lu, type, dest_reg), var);
      }
      else
         dest_reg = emit_index(var, VCODE_INVALID_REG);
   }

   type_t value_type = NULL;
   vcode_reg_t value_reg;
   if (tree_has_value(decl)) {
      tree_t value = tree_value(decl);
      value_type = tree_type(value);
      if (tree_kind(value) == T_AGGREGATE)
         value_reg = lower_aggregate(lu, value, dest_reg);
      else
         value_reg = lower_known_subtype(lu, value, type, VCODE_INVALID_REG);
   }
   else {
      value_type = type;
      value_reg = lower_default_value(lu, type, dest_reg);
   }

   if (type_is_array(type)) {
      vcode_reg_t data_reg = lower_array_data(value_reg);

      if (is_const && skip_copy) {
         if (vcode_reg_kind(value_reg) == VCODE_TYPE_UARRAY)
            emit_store(value_reg, var);
         else {
            vcode_reg_t wrapped_reg = lower_wrap(lu, value_type, data_reg);
            emit_store(wrapped_reg, var);
         }
      }
      else if (type_is_unconstrained(type)) {
         count_reg = lower_array_total_len(lu, value_type, value_reg);

         type_t scalar_elem = lower_elem_recur(type);
         dest_reg = emit_alloc(lower_type(scalar_elem),
                               lower_bounds(scalar_elem),
                               count_reg);
         emit_copy(dest_reg, data_reg, count_reg);

         vcode_reg_t wrap_reg;
         if (vcode_reg_kind(value_reg) == VCODE_TYPE_UARRAY)
            wrap_reg = lower_rewrap(dest_reg, value_reg);
         else
            wrap_reg = lower_wrap(lu, value_type, dest_reg);

         emit_store(wrap_reg, var);
      }
      else {
         vcode_reg_t locus = lower_debug_locus(decl);
         lower_check_indexes(lu, value_type, type, value_reg, NULL);
         lower_check_array_sizes(lu, type, value_type,
                                 VCODE_INVALID_REG, value_reg, locus);
         emit_copy(dest_reg, data_reg, count_reg);
      }
   }
   else if (type_is_record(type))
      lower_new_record(lu, type, dest_reg, value_reg);
   else if (type_is_scalar(type)) {
      lower_check_scalar_bounds(lu, value_reg, type, decl, decl);
      emit_store(value_reg, var);
   }
   else if (type_is_access(type)) {
      type_t designated = type_designated(type);
      vcode_reg_t src_reg = lower_incomplete_access(value_reg, designated);
      emit_store(src_reg, var);
   }
   else
      emit_store(value_reg, var);
}

static vcode_reg_t lower_resolution_func(lower_unit_t *lu, type_t type,
                                         bool *is_array)
{
   tree_t rname = NULL;
   for (type_t t = type; type_kind(t) == T_SUBTYPE; t = type_base(t)) {
      if (type_has_resolution(t)) {
         rname = type_resolution(t);
         break;
      }
   }

   if (rname == NULL && type_is_array(type))
      return lower_resolution_func(lu, type_elem(type), is_array);
   else if (rname == NULL)
      return VCODE_INVALID_REG;

   while (tree_kind(rname) == T_AGGREGATE) {
      assert(type_is_array(type));
      assert(tree_assocs(rname) == 1);

      rname = tree_value(tree_assoc(rname, 0));
      type = type_elem(type);
   }

   tree_t rdecl = tree_ref(rname);
   ident_t rfunc = tree_ident2(rdecl);
   vcode_type_t vtype = lower_type(type);

   type_t uarray_param = type_param(tree_type(rdecl), 0);
   assert(type_kind(uarray_param) == T_ARRAY);
   tree_t r = range_of(type_index(uarray_param, 0), 0);

   vcode_reg_t ileft_reg = emit_const(vtype_offset(), assume_int(tree_left(r)));

   vcode_reg_t nlits_reg;
   if (type_is_enum(type)) {
      // This resolution function can potentially be memoised
      if (type_kind(type) == T_SUBTYPE) {
         int64_t low, high;
         range_bounds(range_of(type, 0), &low, &high);
         nlits_reg = emit_const(vtype_offset(), high - low + 1);
      }
      else
         nlits_reg = emit_const(vtype_offset(), type_enum_literals(type));
   }
   else
      nlits_reg = emit_const(vtype_offset(), 0);

   *is_array = vtype_kind(vtype) == VCODE_TYPE_CARRAY;

   vcode_type_t elem = *is_array ? vtype_elem(vtype) : vtype;
   vcode_type_t rtype = lower_func_result_type(type);
   vcode_type_t atype = vtype_uarray(1, elem, vtype_int(0, INT32_MAX));

   vcode_reg_t context_reg = lower_context_for_call(lu, rfunc);
   vcode_reg_t closure_reg = emit_closure(rfunc, context_reg, atype, rtype);
   return emit_resolution_wrapper(rtype, closure_reg, ileft_reg, nlits_reg);
}

static void lower_sub_signals(lower_unit_t *lu, type_t type, type_t var_type,
                              type_t init_type, tree_t where, tree_t view,
                              vcode_var_t sig_var, vcode_reg_t sig_ptr,
                              vcode_reg_t init_reg, vcode_reg_t resolution,
                              vcode_reg_t null_reg, sig_flags_t flags,
                              vcode_reg_t bounds_reg)
{
   bool has_scope = false;
   if (resolution == VCODE_INVALID_REG)
      resolution = lower_resolution_func(lu, type, &has_scope);

   if (type_is_scalar(type)) {
      vcode_type_t voffset = vtype_offset();
      vcode_reg_t size_reg = emit_const(voffset, type_byte_width(type));
      vcode_reg_t len_reg = emit_const(voffset, 1);
      vcode_type_t vtype = lower_type(type);

      vcode_reg_t locus = lower_debug_locus(where);

      if (init_reg == VCODE_INVALID_REG)
         init_reg = lower_range_left(lu, range_of(type, 0));
      else
         lower_check_scalar_bounds(lu, init_reg, type, where, where);

      assert(!has_scope);
      assert(bounds_reg == VCODE_INVALID_REG);

      well_known_t wk = is_well_known(type_ident(type_base_recur(type)));
      if (wk == W_IEEE_ULOGIC || wk == W_IEEE_LOGIC)
         flags |= SIG_F_STD_LOGIC;

      vcode_reg_t flags_reg = emit_const(voffset, flags);
      vcode_reg_t sig = emit_init_signal(vtype, len_reg, size_reg, init_reg,
                                         flags_reg, locus, null_reg);

      if (resolution != VCODE_INVALID_REG)
         emit_resolve_signal(sig, resolution);

      if (sig_var != VCODE_INVALID_VAR)
         emit_store(sig, sig_var);
      else
         emit_store_indirect(sig, sig_ptr);
   }
   else if (type_is_homogeneous(type)) {
      assert(type_is_array(type));

      vcode_type_t voffset = vtype_offset();
      vcode_reg_t size_reg = emit_const(voffset, type_byte_width(type));
      vcode_reg_t len_reg = lower_array_total_len(lu, type, bounds_reg);
      vcode_type_t vtype = lower_type(lower_elem_recur(type));

      vcode_reg_t locus = lower_debug_locus(where);

      if (init_reg == VCODE_INVALID_REG) {
         type_t elem = lower_elem_recur(type);
         init_reg = lower_range_left(lu, range_of(elem, 0));
      }
      else {
         lower_check_array_sizes(lu, type, init_type, bounds_reg,
                                 init_reg, locus);
         init_reg = lower_array_data(init_reg);
      }

      if (has_scope)
         emit_push_scope(locus, lower_type(type));

      well_known_t wk = is_well_known(type_ident(type_base_recur(type)));
      if (wk == W_IEEE_ULOGIC_VECTOR || wk == W_IEEE_LOGIC_VECTOR)
         flags |= SIG_F_STD_LOGIC;

      vcode_reg_t flags_reg = emit_const(voffset, flags);
      vcode_reg_t sig = emit_init_signal(vtype, len_reg, size_reg, init_reg,
                                         flags_reg, locus, null_reg);

      if (resolution != VCODE_INVALID_REG)
         emit_resolve_signal(sig, resolution);

      if (bounds_reg != VCODE_INVALID_REG)
         sig = lower_rewrap(sig, bounds_reg);
      else
         sig = lower_coerce_arrays(lu, type, var_type, sig);

      if (sig_var != VCODE_INVALID_VAR)
         emit_store(sig, sig_var);
      else
         emit_store_indirect(sig, sig_ptr);

      if (has_scope)
         emit_pop_scope();
   }
   else if (type_is_array(type)) {
      // Array of non-homogeneous type (e.g. records). Need a loop to
      // initialise each sub-signal.

      type_t elem = lower_elem_recur(type);

      if (null_reg == VCODE_INVALID_REG || have_uarray_ptr(null_reg))
         null_reg = emit_null(vtype_pointer(lower_type(elem)));

      if (sig_ptr == VCODE_INVALID_REG)
         sig_ptr = emit_index(sig_var, VCODE_INVALID_REG);

      if (init_reg != VCODE_INVALID_REG) {
         vcode_reg_t locus = lower_debug_locus(where);
         lower_check_array_sizes(lu, type, init_type, VCODE_INVALID_REG,
                                 init_reg, locus);
      }

      vcode_reg_t len_reg = lower_array_total_len(lu, type, VCODE_INVALID_REG);

      if (have_uarray_ptr(sig_ptr)) {
         // Need to allocate separate memory for the array
         vcode_type_t vtype = lower_signal_type(elem);
         vcode_type_t vbounds = lower_bounds(elem);
         vcode_reg_t mem_reg = emit_alloc(vtype, vbounds, len_reg);

         vcode_reg_t wrap_reg;
         if (bounds_reg != VCODE_INVALID_REG)
            wrap_reg = lower_rewrap(mem_reg, bounds_reg);
         else
            wrap_reg = lower_wrap(lu, type, mem_reg);

         emit_store_indirect(wrap_reg, sig_ptr);

         sig_ptr = mem_reg;
      }

      vcode_type_t voffset = vtype_offset();
      vcode_var_t i_var = lower_temp_var(lu, "i", voffset, voffset);
      emit_store(emit_const(voffset, 0), i_var);

      emit_push_scope(lower_debug_locus(where), lower_type(type));

      vcode_block_t cmp_bb  = emit_block();
      vcode_block_t body_bb = emit_block();
      vcode_block_t exit_bb = emit_block();

      emit_jump(cmp_bb);

      vcode_select_block(cmp_bb);

      vcode_reg_t i_reg  = emit_load(i_var);
      vcode_reg_t eq_reg = emit_cmp(VCODE_CMP_EQ, i_reg, len_reg);
      emit_cond(eq_reg, exit_bb, body_bb);

      vcode_select_block(body_bb);

      vcode_reg_t ptr_reg = emit_array_ref(sig_ptr, i_reg);
      vcode_reg_t null_off_reg = emit_array_ref(null_reg, i_reg);

      vcode_reg_t data_reg = VCODE_INVALID_REG;
      if (init_reg != VCODE_INVALID_REG)
         data_reg = emit_array_ref(lower_array_data(init_reg), i_reg);

      lower_sub_signals(lu, elem, elem, elem, where, view, VCODE_INVALID_VAR,
                        ptr_reg, data_reg, resolution, null_off_reg,
                        flags, VCODE_INVALID_REG);

      emit_store(emit_add(i_reg, emit_const(voffset, 1)), i_var);

      emit_jump(cmp_bb);

      vcode_select_block(exit_bb);
      emit_pop_scope();
      lower_release_temp(lu, i_var);
   }
   else if (type_is_record(type)) {
      vcode_type_t vtype = lower_type(type);
      emit_push_scope(lower_debug_locus(where), vtype);

      if (null_reg == VCODE_INVALID_REG)
         null_reg = emit_null(vtype_pointer(vtype));

      if (sig_ptr == VCODE_INVALID_REG)
         sig_ptr = emit_index(sig_var, VCODE_INVALID_REG);

      type_t base = type_base_recur(type);

      const int nfields = type_fields(base);
      for (int i = 0; i < nfields; i++) {
         tree_t f = type_field(base, i);

         type_t ft = tree_type(f), et = ft;
         if (base != type && type_is_unconstrained(ft)) {
            tree_t ec = type_constraint_for_field(type, f);
            assert(ec != NULL);
            et = tree_type(ec);
         }

         sig_flags_t newflags = flags;
         tree_t fview = NULL;
         if (view != NULL) {
            bool converse = false;
            tree_t e = find_element_mode_indication(view, f, &converse);
            assert(e != NULL);

            switch (tree_subkind(e)) {
            case PORT_INOUT:
               newflags |= NET_F_EFFECTIVE | NET_F_INOUT;
               break;
            case PORT_ARRAY_VIEW:
            case PORT_RECORD_VIEW:
               fview = tree_value(e);
               break;
            }
         }

         vcode_type_t fvtype = vtype_field(vtype, i);

         vcode_reg_t null_field_reg;
         if (vtype_kind(fvtype) == VCODE_TYPE_UARRAY)
            null_field_reg = emit_null(vtype_pointer(fvtype));
         else
            null_field_reg = emit_record_ref(null_reg, i);

         vcode_reg_t field_reg = VCODE_INVALID_REG;
         if (init_reg != VCODE_INVALID_REG) {
            field_reg = emit_record_ref(init_reg, i);

            if (have_uarray_ptr(field_reg))
               field_reg = emit_load_indirect(field_reg);
            else if (type_is_scalar(ft))
               field_reg = emit_load_indirect(field_reg);
         }

         vcode_reg_t fbounds_reg = VCODE_INVALID_REG;
         if (bounds_reg != VCODE_INVALID_REG) {
            fbounds_reg = emit_record_ref(bounds_reg, i);

            if (have_uarray_ptr(fbounds_reg))
               fbounds_reg = emit_load_indirect(fbounds_reg);
         }

         vcode_reg_t ptr_reg = emit_record_ref(sig_ptr, i);

         lower_sub_signals(lu, et, ft, ft, f, fview, VCODE_INVALID_VAR, ptr_reg,
                           field_reg, resolution, null_field_reg, newflags,
                           fbounds_reg);
      }

      emit_pop_scope();
   }
   else
      fatal_trace("unhandled type %s in lower_sub_signals", type_pp(type));
}

static void lower_signal_decl(lower_unit_t *lu, tree_t decl)
{
   type_t type = tree_type(decl);
   vcode_type_t vtype = lower_signal_type(type);
   vcode_type_t vbounds = lower_bounds(type);
   vcode_var_t var = emit_var(vtype, vbounds, tree_ident(decl), VAR_SIGNAL);
   lower_put_vcode_obj(decl, var, lu);

   type_t value_type = type;
   vcode_reg_t init_reg = VCODE_INVALID_REG;
   if (tree_has_value(decl)) {
      tree_t value = tree_value(decl);
      value_type = tree_type(value);
      init_reg = lower_known_subtype(lu, value, type, VCODE_INVALID_REG);
   }

   vcode_reg_t bounds_reg = VCODE_INVALID_REG;
   if (standard() >= STD_19 && type_is_unconstrained(type)) {
      type = value_type;
      bounds_reg = init_reg;
   }

   sig_flags_t flags = 0;
   if (tree_flags(decl) & TREE_F_REGISTER)
      flags |= NET_F_REGISTER;

   lower_sub_signals(lu, type, type, value_type, decl, NULL, var,
                     VCODE_INVALID_REG, init_reg, VCODE_INVALID_REG,
                     VCODE_INVALID_REG, flags, bounds_reg);

   if (cover_enabled(lu->cover, COVER_MASK_TOGGLE))
      lower_toggle_coverage(lu, decl);
}

static void lower_build_wait_cb(tree_t expr, void *ctx)
{
   wait_param_t *param = ctx;
   type_t type = tree_type(expr);

   vcode_reg_t nets_reg = lower_lvalue(param->lu, expr);
   vcode_reg_t count_reg = lower_type_width(param->lu, type, nets_reg);
   vcode_reg_t data_reg = lower_array_data(nets_reg);

   if (param->wake != VCODE_INVALID_REG)
      emit_implicit_event(data_reg, count_reg, param->wake);
   else
      emit_sched_event(data_reg, count_reg);
}

static void lower_implicit_decl(lower_unit_t *parent, tree_t decl)
{
   ident_t name = tree_ident(decl);
   type_t type = tree_type(decl);

   vcode_type_t signal_type = lower_signal_type(type);
   vcode_type_t vtype = lower_type(type);
   vcode_type_t vbounds = lower_bounds(type);
   vcode_var_t var = emit_var(signal_type, vbounds, name, VAR_SIGNAL);
   lower_put_vcode_obj(decl, var, parent);

   switch (tree_subkind(decl)) {
   case IMPLICIT_GUARD:
      {
         tree_t expr = tree_value(decl);

         ident_t context_id = vcode_unit_name();
         ident_t qual = ident_prefix(context_id, name, '.');
         ident_t func = ident_prefix(qual, ident_new("guard"), '$');

         vcode_state_t state;
         vcode_state_save(&state);

         object_t *obj = tree_to_object(expr);
         vcode_unit_t vu = emit_function(func, obj, parent->vunit);
         vcode_set_result(vtype);

         vcode_type_t vcontext = vtype_context(context_id);
         emit_param(vcontext, vcontext, ident_new("context"));

         lower_unit_t *lu = lower_unit_new(parent->registry, parent,
                                           vu, NULL, NULL);
         unit_registry_put(parent->registry, lu);

         emit_return(lower_rvalue(lu, expr));

         unit_registry_finalise(parent->registry, lu);

         vcode_state_restore(&state);

         vcode_reg_t one_reg = emit_const(vtype_offset(), 1);
         vcode_reg_t locus = lower_debug_locus(decl);
         vcode_reg_t context_reg = lower_context_for_call(parent, func);
         vcode_reg_t closure =
            emit_closure(func, context_reg, VCODE_INVALID_TYPE, vtype);
         vcode_reg_t kind_reg = emit_const(vtype_offset(), IMPLICIT_GUARD);
         vcode_reg_t sig = emit_implicit_signal(vtype, one_reg, one_reg, locus,
                                                kind_reg, closure);
         emit_store(sig, var);

         wait_param_t param = { parent, sig };
         build_wait(expr, lower_build_wait_cb, &param);
      }
      break;

   case IMPLICIT_DELAYED:
      {
         tree_t wave = tree_value(decl);
         assert(tree_kind(wave) == T_WAVEFORM);

         tree_t expr = tree_value(wave);

         vcode_reg_t init_reg =
            lower_default_value(parent, type, VCODE_INVALID_REG);

         lower_sub_signals(parent, type, type, type, decl, NULL, var,
                           VCODE_INVALID_REG, init_reg, VCODE_INVALID_REG,
                           VCODE_INVALID_REG, 0, VCODE_INVALID_REG);

         vcode_state_t state;
         vcode_state_save(&state);

         object_t *obj = tree_to_object(decl);
         ident_t name = ident_prefix(vcode_unit_name(), tree_ident(decl), '.');
         vcode_unit_t vu = emit_process(name, obj, parent->vunit);

         lower_unit_t *lu = lower_unit_new(parent->registry, parent,
                                           vu, NULL, NULL);
         unit_registry_put(parent->registry, lu);

         vcode_reg_t nets_reg = lower_signal_ref(lu, decl);
         vcode_reg_t count_reg = lower_type_width(lu, type, nets_reg);
         vcode_reg_t data_reg = lower_array_data(nets_reg);

         emit_drive_signal(data_reg, count_reg);

         wait_param_t param = { lu, VCODE_INVALID_REG };
         build_wait(expr, lower_build_wait_cb, &param);

         emit_return(VCODE_INVALID_REG);

         vcode_block_t main_bb = emit_block();
         vcode_select_block(main_bb);

         vcode_reg_t delay_reg = lower_rvalue(lu, tree_delay(wave));
         vcode_reg_t reject_reg = emit_const(vtype_time(), 0);
         vcode_reg_t value_reg = lower_rvalue(lu, expr);

         target_part_t parts[] = {
            { .kind = PART_ALL,
              .reg = lower_signal_ref(lu, decl),
              .off = VCODE_INVALID_REG,
              .target = decl },
            { .kind = PART_POP,
              .reg = VCODE_INVALID_REG,
              .off = VCODE_INVALID_REG,
            }
         };

         target_part_t *ptr = parts;
         lower_signal_assign_target(lu, &ptr, decl, value_reg, type,
                                    reject_reg, delay_reg);

         emit_return(VCODE_INVALID_REG);

         unit_registry_finalise(parent->registry, lu);

         vcode_state_restore(&state);

         emit_process_init(name, lower_debug_locus(decl));
      }
      break;

   case IMPLICIT_TRANSACTION:
      {
         ident_t context_id = vcode_unit_name();
         ident_t func = ident_prefix(context_id, name, '.');

         vcode_state_t state;
         vcode_state_save(&state);

         tree_t expr = tree_value(decl);
         object_t *obj = tree_to_object(expr);
         vcode_unit_t vu = emit_function(func, obj, parent->vunit);
         vcode_set_result(vtype);

         vcode_type_t vcontext = vtype_context(context_id);
         emit_param(vcontext, vcontext, ident_new("context"));

         lower_unit_t *lu = lower_unit_new(parent->registry, parent,
                                           vu, NULL, NULL);
         unit_registry_put(parent->registry, lu);

         emit_return(lower_rvalue(lu, expr));

         unit_registry_finalise(parent->registry, lu);

         vcode_state_restore(&state);

         vcode_reg_t one_reg = emit_const(vtype_offset(), 1);
         vcode_reg_t locus = lower_debug_locus(decl);
         vcode_reg_t context_reg = lower_context_for_call(parent, func);
         vcode_reg_t closure =
            emit_closure(func, context_reg, VCODE_INVALID_TYPE, vtype);
         vcode_reg_t kind_reg =
            emit_const(vtype_offset(), IMPLICIT_TRANSACTION);
         vcode_reg_t sig = emit_implicit_signal(vtype, one_reg, one_reg, locus,
                                                kind_reg, closure);
         emit_store(sig, var);

         assert(tree_triggers(decl) == 1);
         tree_t trigger = tree_trigger(decl, 0);

         wait_param_t param = { parent, sig };
         build_wait(trigger, lower_build_wait_cb, &param);
      }
      break;
   }
}

static void lower_file_decl(lower_unit_t *lu, tree_t decl)
{
   type_t type = tree_type(decl);
   vcode_type_t vtype = lower_type(type);
   vcode_var_t var = emit_var(vtype, vtype, tree_ident(decl), 0);
   lower_put_vcode_obj(decl, var, lu);

   emit_store(emit_null(vtype), var);

   if (tree_has_value(decl)) {
      // Generate initial call to file_open

      tree_t value = tree_value(decl);

      vcode_reg_t name_array = lower_rvalue(lu, tree_value(decl));
      vcode_reg_t name_data  = lower_array_data(name_array);
      vcode_reg_t name_len   = lower_array_len(lu, tree_type(value), 0,
                                               name_array);
      vcode_reg_t file_ptr   = emit_index(var, VCODE_INVALID_REG);
      vcode_reg_t mode       = lower_rvalue(lu, tree_file_mode(decl));

      emit_file_open(file_ptr, name_data, name_len, mode, VCODE_INVALID_REG);
   }
}

static vcode_type_t lower_alias_type(tree_t alias)
{
   type_t type = tree_has_type(alias)
      ? tree_type(alias)
      : tree_type(tree_value(alias));

   if (!type_is_array(type))
      return VCODE_INVALID_TYPE;

   vcode_type_t velem;
   switch (class_of(tree_value(alias))) {
   case C_SIGNAL:
      velem = lower_signal_type(lower_elem_recur(type));
      break;
   case C_VARIABLE:
   case C_CONSTANT:
      velem = lower_type(lower_elem_recur(type));
      break;
   default:
      return VCODE_INVALID_TYPE;
   }

   vcode_type_t vbounds = lower_bounds(type);
   return vtype_uarray(dims_for_type(type), velem, vbounds);
}

static void lower_alias_decl(lower_unit_t *lu, tree_t decl)
{
   vcode_type_t vtype = lower_alias_type(decl);
   if (vtype == VCODE_INVALID_TYPE)
      return;

   tree_t value = tree_value(decl);
   type_t value_type = tree_type(value);
   type_t type = tree_has_type(decl) ? tree_type(decl) : value_type;

   vcode_var_flags_t flags = 0;
   if (class_of(value) == C_SIGNAL)
      flags |= VAR_SIGNAL;

   vcode_type_t vbounds = lower_bounds(type);
   vcode_var_t var = emit_var(vtype, vbounds, tree_ident(decl), flags);
   lower_put_vcode_obj(decl, var, lu);

   vcode_reg_t value_reg =
      (flags & VAR_SIGNAL) ? lower_lvalue(lu, value) : lower_rvalue(lu, value);
   vcode_reg_t data_reg  = lower_array_data(value_reg);

   vcode_reg_t wrap_reg;
   if (tree_has_type(decl) && !type_is_unconstrained(type)) {
      vcode_reg_t locus = lower_debug_locus(decl);
      lower_check_array_sizes(lu, type, value_type, VCODE_INVALID_REG,
                              value_reg, locus);
      wrap_reg = lower_wrap(lu, type, data_reg);
   }
   else
      wrap_reg = lower_wrap_with_new_bounds(lu, value_type, value_type,
                                            value_reg, data_reg);

   emit_store(wrap_reg, var);
}

static void lower_enum_image_helper(type_t type, vcode_reg_t preg)
{
   const int nlits = type_enum_literals(type);
   assert(nlits >= 1);

   vcode_block_t *blocks LOCAL = xmalloc_array(nlits, sizeof(vcode_block_t));
   vcode_reg_t *cases LOCAL = xmalloc_array(nlits, sizeof(vcode_reg_t));

   vcode_type_t vtype = lower_type(type);

   for (int i = 0; i < nlits; i++) {
      cases[i]  = emit_const(vtype, i);
      blocks[i] = emit_block();
   }

   emit_case(preg, blocks[0], cases, blocks, nlits);

   for (int i = 0; i < nlits; i++) {
      // LRM specifies result is lowercase for enumerated types when
      // the value is a basic identifier
      ident_t id = tree_ident(type_enum_literal(type, i));
      if (ident_char(id, 0) != '\'')
         id = ident_downcase(id);

      vcode_select_block(blocks[i]);
      vcode_reg_t str = lower_wrap_string(istr(id));
      emit_return(str);
   }
}

static void lower_physical_image_helper(lower_unit_t *lu, type_t type,
                                        vcode_reg_t preg)
{
   vcode_type_t vint64 = vtype_int(INT64_MIN, INT64_MAX);
   vcode_type_t vchar = vtype_char();
   vcode_type_t vstring = vtype_uarray(1, vchar, vchar);

   vcode_reg_t cast_reg = emit_cast(vint64, vint64, preg);
   ident_t conv_fn =
      ident_new("NVC.TEXT_UTIL.INT_TO_STRING(21NVC.TEXT_UTIL.T_INT64)S");
   vcode_reg_t conv_args[] = {
      lower_context_for_call(lu, conv_fn),
      cast_reg
   };
   vcode_reg_t num_reg = emit_fcall(conv_fn, vstring, vstring,
                                    VCODE_CC_VHDL, conv_args, 2);

   vcode_reg_t num_len = emit_uarray_len(num_reg, 0);

   const char *unit0 = istr(ident_downcase(tree_ident(type_unit(type, 0))));

   vcode_reg_t append_len = emit_const(vtype_offset(), strlen(unit0) + 1);
   vcode_reg_t total_len = emit_add(num_len, append_len);

   vcode_type_t ctype = vtype_char();
   vcode_reg_t mem_reg = emit_alloc(ctype, ctype, total_len);
   emit_copy(mem_reg, emit_unwrap(num_reg), num_len);

   vcode_reg_t ptr0_reg = emit_array_ref(mem_reg, num_len);
   emit_store_indirect(emit_const(ctype, ' '), ptr0_reg);

   vcode_reg_t unit_reg = lower_wrap_string(unit0);
   vcode_reg_t ptr1_reg =
      emit_array_ref(ptr0_reg, emit_const(vtype_offset(), 1));
   emit_copy(ptr1_reg, emit_unwrap(unit_reg),
             emit_const(vtype_offset(), strlen(unit0)));

   vcode_dim_t dims[] = {
      { .left  = emit_const(vtype_offset(), 1),
        .right = total_len,
        .dir   = emit_const(vtype_bool(), RANGE_TO)
      }
   };
   emit_return(emit_wrap(mem_reg, dims, 1));
}

static void lower_numeric_image_helper(lower_unit_t *lu, type_t type,
                                       vcode_reg_t preg)
{
   vcode_type_t vchar = vtype_char();
   vcode_type_t vstring = vtype_uarray(1, vchar, vchar);

   ident_t conv_fn;
   vcode_reg_t arg_reg;

   if (type_is_real(type)) {
      arg_reg = preg;
      conv_fn = ident_new("NVC.TEXT_UTIL.REAL_TO_STRING(R)S");
   }
   else {
      vcode_type_t vint64 = vtype_int(INT64_MIN, INT64_MAX);
      arg_reg = emit_cast(vint64, vint64, preg);
      conv_fn = ident_new(
         "NVC.TEXT_UTIL.INT_TO_STRING(21NVC.TEXT_UTIL.T_INT64)S");
   }

   vcode_reg_t conv_args[] = {
      lower_context_for_call(lu, conv_fn),
      arg_reg
   };
   vcode_reg_t str_reg = emit_fcall(conv_fn, vstring, vstring,
                                    VCODE_CC_VHDL, conv_args, 2);
   emit_return(str_reg);
}

static void lower_record_image_helper(lower_unit_t *lu, type_t type,
                                      vcode_reg_t preg)
{
   const int nfields = type_fields(type);
   vcode_reg_t regs[nfields];
   vcode_reg_t lengths[nfields];

   vcode_type_t ctype = vtype_char();
   vcode_type_t strtype = vtype_uarray(1, ctype, ctype);
   vcode_type_t voffset = vtype_offset();

   vcode_reg_t sum_reg = emit_const(voffset, nfields + 1);

   for (int i = 0; i < nfields; i++) {
      type_t ftype = type_base_recur(tree_type(type_field(type, i)));
      ident_t func = ident_prefix(type_ident(ftype), ident_new("image"), '$');

      vcode_reg_t context_reg = lower_context_for_call(lu, func);
      vcode_reg_t field_reg = emit_record_ref(preg, i);

      if (type_is_scalar(ftype) || have_uarray_ptr(field_reg))
         field_reg = emit_load_indirect(field_reg);

      vcode_reg_t args[] = { context_reg, field_reg };
      regs[i] = emit_fcall(func, strtype, strtype, VCODE_CC_PREDEF, args, 2);
      lengths[i] = emit_uarray_len(regs[i], 0);

      sum_reg = emit_add(sum_reg, lengths[i]);
   }

   vcode_reg_t mem_reg = emit_alloc(ctype, ctype, sum_reg);

   vcode_reg_t lparen_reg = emit_array_ref(mem_reg, emit_const(voffset, 0));
   emit_store_indirect(emit_const(ctype, '('), lparen_reg);

   vcode_reg_t index_reg = emit_const(voffset, 1);
   for (int i = 0; i < nfields; i++) {
      if (i > 0) {
         vcode_reg_t comma_reg = emit_array_ref(mem_reg, index_reg);
         emit_store_indirect(emit_const(ctype, ','), comma_reg);
         index_reg = emit_add(index_reg, emit_const(voffset, 1));
      }

      vcode_reg_t src_reg = emit_unwrap(regs[i]);
      vcode_reg_t dest_reg = emit_array_ref(mem_reg, index_reg);
      emit_copy(dest_reg, src_reg, lengths[i]);
      index_reg = emit_add(index_reg, lengths[i]);
   }

   vcode_reg_t rparen_reg = emit_array_ref(mem_reg, index_reg);
   emit_store_indirect(emit_const(ctype, ')'), rparen_reg);

   vcode_dim_t dims[] = {
      { .left  = emit_const(vtype_offset(), 1),
        .right = sum_reg,
        .dir   = emit_const(vtype_bool(), RANGE_TO)
      }
   };
   emit_return(emit_wrap(mem_reg, dims, 1));
}

static void lower_array_image_helper(lower_unit_t *lu, type_t type,
                                     vcode_reg_t preg)
{
   vcode_type_t ctype = vtype_char();
   vcode_type_t strtype = vtype_uarray(1, ctype, ctype);
   vcode_type_t voffset = vtype_offset();
   vcode_type_t fields[] = { vtype_pointer(ctype), voffset };
   vcode_type_t rtype = vtype_named_record(ident_new("elem"), fields, 2);

   vcode_reg_t length_reg = lower_array_len(lu, type, 0, preg);

   vcode_reg_t elems_reg = emit_alloc(rtype, rtype, length_reg);
   vcode_reg_t zero_reg = emit_const(voffset, 0);
   vcode_reg_t one_reg = emit_const(voffset, 1);
   vcode_reg_t data_reg = lower_array_data(preg);

   vcode_var_t i_var = lower_temp_var(lu, "i", voffset, voffset);
   emit_store(zero_reg, i_var);

   vcode_block_t loop1_bb = emit_block();
   vcode_block_t alloc_bb = emit_block();

   vcode_var_t sum_var = lower_temp_var(lu, "sum", voffset, voffset);
   emit_store(emit_add(length_reg, one_reg), sum_var);

   type_t elem = type_base_recur(type_elem(type));
   ident_t func = ident_prefix(type_ident(elem), ident_new("image"), '$');

   vcode_reg_t context_reg = lower_context_for_call(lu, func);

   vcode_reg_t null_reg = emit_cmp(VCODE_CMP_EQ, length_reg, zero_reg);
   emit_cond(null_reg, alloc_bb, loop1_bb);

   vcode_select_block(loop1_bb);

   {
      vcode_reg_t i_reg = emit_load(i_var);
      vcode_reg_t sum_reg = emit_load(sum_var);

      vcode_reg_t elem_reg = emit_array_ref(data_reg, i_reg);
      if (type_is_scalar(elem))
         elem_reg = emit_load_indirect(elem_reg);

      vcode_reg_t args[] = { context_reg, elem_reg };
      vcode_reg_t str_reg = emit_fcall(func, strtype, strtype,
                                       VCODE_CC_PREDEF, args, 2);

      vcode_reg_t edata_reg = emit_unwrap(str_reg);
      vcode_reg_t elen_reg = emit_uarray_len(str_reg, 0);
      emit_store(emit_add(sum_reg, elen_reg), sum_var);

      vcode_reg_t rptr_reg = emit_array_ref(elems_reg, i_reg);
      emit_store_indirect(edata_reg, emit_record_ref(rptr_reg, 0));
      emit_store_indirect(elen_reg, emit_record_ref(rptr_reg, 1));

      vcode_reg_t next_reg = emit_add(i_reg, one_reg);
      emit_store(next_reg, i_var);

      vcode_reg_t done_reg = emit_cmp(VCODE_CMP_EQ, next_reg, length_reg);
      emit_cond(done_reg, alloc_bb, loop1_bb);
   }

   vcode_select_block(alloc_bb);

   vcode_reg_t sum_reg = emit_load(sum_var);
   vcode_reg_t mem_reg = emit_alloc(ctype, ctype, sum_reg);

   vcode_reg_t lparen_reg = emit_array_ref(mem_reg, zero_reg);
   emit_store_indirect(emit_const(ctype, '('), lparen_reg);

   vcode_block_t loop2_bb = emit_block();
   vcode_block_t exit_bb = emit_block();

   vcode_var_t index_var = lower_temp_var(lu, "index", voffset, voffset);
   emit_store(zero_reg, i_var);
   emit_store(one_reg, index_var);

   emit_cond(null_reg, exit_bb, loop2_bb);

   vcode_select_block(loop2_bb);

   {
      vcode_reg_t i_reg = emit_load(i_var);
      vcode_reg_t index_reg = emit_load(index_var);

      vcode_reg_t rptr_reg = emit_array_ref(elems_reg, i_reg);
      vcode_reg_t edata_reg = emit_load_indirect(emit_record_ref(rptr_reg, 0));
      vcode_reg_t elen_reg = emit_load_indirect(emit_record_ref(rptr_reg, 1));

      vcode_reg_t dest_reg = emit_array_ref(mem_reg, index_reg);
      emit_copy(dest_reg, edata_reg, elen_reg);

      index_reg = emit_add(index_reg, elen_reg);
      vcode_reg_t comma_reg = emit_array_ref(mem_reg, index_reg);
      emit_store_indirect(emit_const(ctype, ','), comma_reg);

      emit_store(emit_add(index_reg, one_reg), index_var);

      vcode_reg_t next_reg = emit_add(i_reg, one_reg);
      emit_store(next_reg, i_var);

      vcode_reg_t done_reg = emit_cmp(VCODE_CMP_EQ, next_reg, length_reg);
      emit_cond(done_reg, exit_bb, loop2_bb);
   }

   vcode_select_block(exit_bb);

   vcode_reg_t last_reg = emit_sub(sum_reg, one_reg);
   vcode_reg_t rparen_reg = emit_array_ref(mem_reg, last_reg);
   emit_store_indirect(emit_const(ctype, ')'), rparen_reg);

   vcode_dim_t dims[] = {
      { .left  = emit_const(voffset, 1),
        .right = sum_reg,
        .dir   = emit_const(vtype_bool(), RANGE_TO)
      }
   };
   emit_return(emit_wrap(mem_reg, dims, 1));
}

static void lower_character_array_image_helper(lower_unit_t *lu, type_t type,
                                               vcode_reg_t preg, bool quote)
{
   type_t elem = type_base_recur(type_elem(type));
   vcode_type_t ctype = vtype_char();
   vcode_type_t voffset = vtype_offset();

   const int nlits = type_enum_literals(elem);
   vcode_reg_t *map LOCAL = xmalloc_array(nlits, sizeof(vcode_reg_t));
   for (int i = 0; i < nlits; i++) {
      const ident_t id = tree_ident(type_enum_literal(elem, i));
      assert(ident_char(id, 0) == '\'');
      map[i] = emit_const(ctype, ident_char(id, 1));
   }

   vcode_type_t map_vtype = vtype_carray(nlits, ctype, ctype);
   vcode_reg_t map_reg = emit_const_array(map_vtype, map, nlits);

   vcode_reg_t zero_reg = emit_const(voffset, 0);
   vcode_reg_t one_reg = emit_const(voffset, 1);
   vcode_reg_t two_reg = emit_const(voffset, 2);

   vcode_reg_t length_reg = lower_array_len(lu, type, 0, preg);
   vcode_reg_t data_reg = lower_array_data(preg);
   vcode_reg_t total_reg = quote ? emit_add(length_reg, two_reg) : length_reg;

   vcode_reg_t mem_reg = emit_alloc(ctype, ctype, total_reg);

   vcode_var_t i_var = lower_temp_var(lu, "i", voffset, voffset);
   emit_store(zero_reg, i_var);

   if (quote) {
      vcode_reg_t lquote_reg = emit_array_ref(mem_reg, zero_reg);
      emit_store_indirect(emit_const(ctype, '"'), lquote_reg);
   }

   vcode_reg_t null_reg = emit_cmp(VCODE_CMP_EQ, length_reg, zero_reg);

   vcode_block_t body_bb = emit_block();
   vcode_block_t exit_bb = emit_block();

   emit_cond(null_reg, exit_bb, body_bb);

   vcode_select_block(body_bb);

   vcode_reg_t i_reg    = emit_load(i_var);
   vcode_reg_t sptr_reg = emit_array_ref(data_reg, i_reg);
   vcode_reg_t src_reg  = emit_load_indirect(sptr_reg);
   vcode_reg_t off_reg  = emit_cast(voffset, voffset, src_reg);
   vcode_reg_t lptr_reg = emit_array_ref(emit_address_of(map_reg), off_reg);
   vcode_reg_t doff_reg = quote ? emit_add(i_reg, one_reg) : i_reg;
   vcode_reg_t dptr_reg = emit_array_ref(mem_reg, doff_reg);

   emit_store_indirect(emit_load_indirect(lptr_reg), dptr_reg);

   vcode_reg_t next_reg = emit_add(i_reg, one_reg);
   vcode_reg_t cmp_reg  = emit_cmp(VCODE_CMP_EQ, next_reg, length_reg);
   emit_store(next_reg, i_var);
   emit_cond(cmp_reg, exit_bb, body_bb);

   vcode_select_block(exit_bb);

   if (quote) {
      vcode_reg_t right_reg = emit_add(length_reg, one_reg);
      vcode_reg_t rquote_reg = emit_array_ref(mem_reg, right_reg);
      emit_store_indirect(emit_const(ctype, '"'), rquote_reg);
   }

   vcode_dim_t dims[] = {
      {
         .left  = emit_const(voffset, 1),
         .right = total_reg,
         .dir   = emit_const(vtype_bool(), RANGE_TO)
      }
   };
   emit_return(emit_wrap(mem_reg, dims, 1));
}

static void lower_image_helper(lower_unit_t *lu, object_t *obj)
{
   tree_t decl = tree_from_object(obj);

   type_t type = tree_type(decl);
   assert(type_is_representable(type));

   vcode_type_t ctype = vtype_char();
   vcode_type_t strtype = vtype_uarray(1, ctype, ctype);
   vcode_set_result(strtype);

   vcode_type_t vcontext = vtype_context(lu->parent->name);
   emit_param(vcontext, vcontext, ident_new("context"));

   vcode_type_t ptype = lower_param_type(type, C_CONSTANT, PORT_IN);
   vcode_reg_t preg = emit_param(ptype, lower_bounds(type), ident_new("VAL"));

   switch (type_kind(type)) {
   case T_ENUM:
      lower_enum_image_helper(type, preg);
      break;
   case T_INTEGER:
   case T_REAL:
      lower_numeric_image_helper(lu, type, preg);
      break;
   case T_PHYSICAL:
      lower_physical_image_helper(lu, type, preg);
      break;
   case T_RECORD:
      lower_record_image_helper(lu, type, preg);
      break;
   case T_SUBTYPE:
      assert(type_is_array(type));
      // Fall-through
   case T_ARRAY:
      {
         type_t elem = type_elem(type);
         if (type_is_enum(elem) && all_character_literals(elem))
            lower_character_array_image_helper(lu, type, preg, true);
         else
            lower_array_image_helper(lu, type, preg);
      }
      break;
   default:
      fatal_trace("cannot lower image helper for type %s", type_pp(type));
   }
}

static void lower_enum_value_helper(lower_unit_t *lu, type_t type,
                                    tree_t decl, vcode_reg_t preg)
{
   const int nlits = type_enum_literals(type);
   assert(nlits >= 1);

   vcode_reg_t arg_len_reg  = emit_uarray_len(preg, 0);
   vcode_reg_t arg_data_reg = emit_unwrap(preg);

   vcode_type_t voffset = vtype_offset();
   vcode_type_t vchar = vtype_char();
   vcode_type_t vstring = vtype_uarray(1, vchar, vchar);

   ident_t canon_fn = ident_new("NVC.TEXT_UTIL.CANON_VALUE(S)S");
   vcode_reg_t canon_args[] = {
      lower_context_for_call(lu, canon_fn),
      preg,
   };
   vcode_reg_t canon_reg = emit_fcall(canon_fn, vstring, vchar, VCODE_CC_VHDL,
                                      canon_args, ARRAY_LEN(canon_args));
   vcode_reg_t canon_len_reg  = emit_uarray_len(canon_reg, 0);

   size_t stride = 0;
   vcode_reg_t *len_regs LOCAL = xmalloc_array(nlits, sizeof(vcode_reg_t));
   for (int i = 0; i < nlits; i++) {
      size_t len = ident_len(tree_ident(type_enum_literal(type, i)));
      len_regs[i] = emit_const(voffset, len);
      stride = MAX(stride, len);
   }

   vcode_type_t len_array_type = vtype_carray(nlits, voffset, voffset);
   vcode_reg_t len_array_reg =
      emit_const_array(len_array_type, len_regs, nlits);
   vcode_reg_t len_array_ptr = emit_address_of(len_array_reg);

   const size_t nchars = nlits * stride;
   vcode_reg_t *char_regs LOCAL = xmalloc_array(nchars, sizeof(vcode_reg_t));
   for (int i = 0; i < nlits; i++) {
      const char *str = istr(tree_ident(type_enum_literal(type, i)));
      size_t pos = 0;
      for (const char *p = str; *p; p++, pos++)
         char_regs[(i * stride) + pos] = emit_const(vchar, *p);
      for (; pos < stride; pos++)
         char_regs[(i * stride) + pos] = emit_const(vchar, 0);
   }

   vcode_type_t char_array_type = vtype_carray(nchars, vchar, vchar);
   vcode_reg_t char_array_reg =
      emit_const_array(char_array_type, char_regs, nchars);
   vcode_reg_t char_array_ptr = emit_address_of(char_array_reg);

   vcode_var_t i_var = lower_temp_var(lu, "i", voffset, voffset);
   emit_store(emit_const(voffset, 0), i_var);

   vcode_block_t head_bb = emit_block();
   vcode_block_t fail_bb = emit_block();
   emit_jump(head_bb);

   const loc_t *loc = vcode_last_loc();

   vcode_select_block(head_bb);

   vcode_reg_t i_reg = emit_load(i_var);

   vcode_block_t memcmp_bb = emit_block();
   vcode_block_t skip_bb   = emit_block();
   vcode_block_t match_bb  = emit_block();

   vcode_reg_t len_ptr = emit_array_ref(len_array_ptr, i_reg);
   vcode_reg_t len_reg = emit_load_indirect(len_ptr);
   vcode_reg_t len_eq  = emit_cmp(VCODE_CMP_EQ, len_reg, canon_len_reg);
   emit_cond(len_eq, memcmp_bb, skip_bb);

   vcode_select_block(memcmp_bb);
   vcode_reg_t char_off = emit_mul(i_reg, emit_const(voffset, stride));
   vcode_reg_t char_ptr = emit_array_ref(char_array_ptr, char_off);

   vcode_dim_t dims[] = {
      { .left  = emit_const(vtype_offset(), 1),
        .right = len_reg,
        .dir   = emit_const(vtype_bool(), RANGE_TO)
      }
   };
   vcode_reg_t str_reg = emit_wrap(char_ptr, dims, 1);

   type_t std_string = std_type(NULL, STD_STRING);
   ident_t func = lower_predef_func_name(std_string, "=");

   vcode_reg_t context_reg = lower_context_for_call(lu, func);
   vcode_reg_t str_cmp_args[] = { context_reg, str_reg, canon_reg };
   vcode_reg_t eq_reg = emit_fcall(func, vtype_bool(), vtype_bool(),
                                   VCODE_CC_PREDEF, str_cmp_args, 3);
   emit_cond(eq_reg, match_bb, skip_bb);

   vcode_select_block(skip_bb);

   vcode_reg_t i_next = emit_add(i_reg, emit_const(voffset, 1));
   emit_store(i_next, i_var);

   vcode_reg_t done_reg = emit_cmp(VCODE_CMP_EQ, i_next,
                                   emit_const(voffset, nlits));
   emit_cond(done_reg, fail_bb, head_bb);

   vcode_select_block(fail_bb);
   emit_debug_info(loc);

   vcode_type_t vseverity = vtype_int(0, SEVERITY_FAILURE - 1);
   vcode_reg_t failure_reg = emit_const(vseverity, SEVERITY_FAILURE);

   vcode_reg_t const_str_reg =
      lower_wrap_string("\" is not a valid enumeration value");
   vcode_reg_t const_str_len = emit_uarray_len(const_str_reg, 0);
   vcode_reg_t extra_len = emit_add(const_str_len, emit_const(voffset, 1));
   vcode_reg_t msg_len = emit_add(arg_len_reg, extra_len);
   vcode_reg_t mem_reg = emit_alloc(vchar, vchar, msg_len);

   emit_store_indirect(emit_const(vchar, '\"'), mem_reg);

   vcode_reg_t ptr1_reg = emit_array_ref(mem_reg, emit_const(voffset, 1));
   emit_copy(ptr1_reg, arg_data_reg, arg_len_reg);

   vcode_reg_t ptr2_reg = emit_array_ref(ptr1_reg, arg_len_reg);
   emit_copy(ptr2_reg, emit_unwrap(const_str_reg), const_str_len);

   vcode_type_t vtype = lower_type(type);
   vcode_type_t vbounds = lower_bounds(type);

   vcode_reg_t locus = lower_debug_locus(type_enum_literal(type, 0));
   emit_report(mem_reg, msg_len, failure_reg, locus);
   emit_return(emit_const(vtype, 0));

   vcode_select_block(match_bb);

   lower_check_scalar_bounds(lu, i_reg, type, decl, NULL);
   emit_return(emit_cast(vtype, vbounds, i_reg));
}

static void lower_physical_value_helper(lower_unit_t *lu, type_t type,
                                        tree_t decl, vcode_reg_t preg)
{
   vcode_reg_t arg_len_reg  = emit_uarray_len(preg, 0);
   vcode_reg_t arg_data_reg = emit_unwrap(preg);

   vcode_type_t voffset = vtype_offset();
   vcode_type_t vchar = vtype_char();
   vcode_type_t vint64 = vtype_int(INT64_MIN, INT64_MAX);
   vcode_type_t vstring = vtype_uarray(1, vchar, vchar);

   vcode_type_t vstdint = (standard() < STD_19)
      ? vtype_int(INT32_MIN, INT32_MAX) : vtype_int(INT64_MIN, INT64_MAX);

   vcode_var_t used_var = lower_temp_var(lu, "used", vstdint, vstdint);
   vcode_reg_t used_ptr = emit_index(used_var, VCODE_INVALID_REG);

   vcode_var_t int_var = lower_temp_var(lu, "int", vint64, vint64);
   vcode_reg_t int_ptr = emit_index(int_var, VCODE_INVALID_REG);

   ident_t conv_fn =
      ident_new("NVC.TEXT_UTIL.STRING_TO_INT(S21NVC.TEXT_UTIL.T_INT64N)");
   vcode_reg_t text_util_reg = lower_context_for_call(lu, conv_fn);
   vcode_reg_t conv_args[] = {
      text_util_reg,
      preg,
      int_ptr,
      used_ptr,
   };
   emit_fcall(conv_fn, VCODE_INVALID_TYPE, VCODE_INVALID_TYPE,
              VCODE_CC_VHDL, conv_args, ARRAY_LEN(conv_args));

   vcode_reg_t int_reg = emit_load(int_var);
   vcode_reg_t used_reg = emit_cast(voffset, voffset, emit_load(used_var));

   vcode_reg_t tail_ptr = emit_array_ref(arg_data_reg, used_reg);
   vcode_reg_t tail_len = emit_sub(arg_len_reg, used_reg);

   vcode_reg_t one_reg = emit_const(vtype_offset(), 1);
   vcode_reg_t to_reg = emit_const(vtype_bool(), RANGE_TO);

   vcode_dim_t tail_dims[1] = {
      { .left  = one_reg,
        .right = tail_len,
        .dir   = to_reg
      },
   };
   vcode_reg_t tail_reg = emit_wrap(tail_ptr, tail_dims, 1);

   ident_t canon_fn = ident_new("NVC.TEXT_UTIL.CANON_VALUE(S)S");
   vcode_reg_t canon_args[] = {
      text_util_reg,
      tail_reg,
   };
   vcode_reg_t canon_reg = emit_fcall(canon_fn, vstring, vchar, VCODE_CC_VHDL,
                                      canon_args, ARRAY_LEN(canon_args));
   vcode_reg_t canon_len_reg = emit_uarray_len(canon_reg, 0);

   const int nunits = type_units(type);
   assert(nunits >= 1);

   size_t stride = 0;
   vcode_reg_t *len_regs LOCAL = xmalloc_array(nunits, sizeof(vcode_reg_t));
   vcode_reg_t *mul_regs LOCAL = xmalloc_array(nunits, sizeof(vcode_reg_t));
   for (int i = 0; i < nunits; i++) {
      tree_t unit = type_unit(type, i);
      size_t len = ident_len(tree_ident(unit));
      len_regs[i] = emit_const(voffset, len);
      stride = MAX(stride, len);

      vcode_reg_t value_reg = lower_rvalue(lu, tree_value(unit));
      mul_regs[i] = emit_cast(vint64, vint64, value_reg);
   }

   vcode_type_t len_array_type = vtype_carray(nunits, voffset, voffset);
   vcode_reg_t len_array_reg =
      emit_const_array(len_array_type, len_regs, nunits);
   vcode_reg_t len_array_ptr = emit_address_of(len_array_reg);

   vcode_type_t mul_array_type = vtype_carray(nunits, vint64, vint64);
   vcode_reg_t mul_array_reg =
      emit_const_array(mul_array_type, mul_regs, nunits);
   vcode_reg_t mul_array_ptr = emit_address_of(mul_array_reg);

   const size_t nchars = nunits * stride;
   vcode_reg_t *char_regs LOCAL = xmalloc_array(nchars, sizeof(vcode_reg_t));
   for (int i = 0; i < nunits; i++) {
      const char *str = istr(tree_ident(type_unit(type, i)));
      size_t pos = 0;
      for (const char *p = str; *p; p++, pos++)
         char_regs[(i * stride) + pos] = emit_const(vchar, *p);
      for (; pos < stride; pos++)
         char_regs[(i * stride) + pos] = emit_const(vchar, 0);
   }

   vcode_type_t char_array_type = vtype_carray(nchars, vchar, vchar);
   vcode_reg_t char_array_reg =
      emit_const_array(char_array_type, char_regs, nchars);
   vcode_reg_t char_array_ptr = emit_address_of(char_array_reg);

   vcode_var_t i_var = lower_temp_var(lu, "i", voffset, voffset);
   emit_store(emit_const(voffset, 0), i_var);

   vcode_block_t head_bb = emit_block();
   vcode_block_t fail_bb = emit_block();
   emit_jump(head_bb);

   const loc_t *loc = vcode_last_loc();

   vcode_select_block(head_bb);

   vcode_reg_t i_reg = emit_load(i_var);

   vcode_block_t memcmp_bb = emit_block();
   vcode_block_t skip_bb   = emit_block();
   vcode_block_t match_bb  = emit_block();

   vcode_reg_t len_ptr = emit_array_ref(len_array_ptr, i_reg);
   vcode_reg_t len_reg = emit_load_indirect(len_ptr);
   vcode_reg_t len_eq  = emit_cmp(VCODE_CMP_EQ, len_reg, canon_len_reg);
   emit_cond(len_eq, memcmp_bb, skip_bb);

   vcode_select_block(memcmp_bb);
   vcode_reg_t char_off = emit_mul(i_reg, emit_const(voffset, stride));
   vcode_reg_t char_ptr = emit_array_ref(char_array_ptr, char_off);

   vcode_dim_t dims[] = {
      { .left  = one_reg,
        .right = len_reg,
        .dir   = to_reg,
      }
   };
   vcode_reg_t str_reg = emit_wrap(char_ptr, dims, 1);

   type_t std_string = std_type(NULL, STD_STRING);
   ident_t func = lower_predef_func_name(std_string, "=");

   vcode_reg_t std_reg = emit_link_package(well_known(W_STD_STANDARD));
   vcode_reg_t str_cmp_args[] = { std_reg, str_reg, canon_reg };
   vcode_reg_t eq_reg = emit_fcall(func, vtype_bool(), vtype_bool(),
                                   VCODE_CC_PREDEF, str_cmp_args, 3);
   emit_cond(eq_reg, match_bb, skip_bb);

   vcode_select_block(skip_bb);

   vcode_reg_t i_next = emit_add(i_reg, emit_const(voffset, 1));
   emit_store(i_next, i_var);

   vcode_reg_t done_reg = emit_cmp(VCODE_CMP_EQ, i_next,
                                   emit_const(voffset, nunits));
   emit_cond(done_reg, fail_bb, head_bb);

   vcode_select_block(fail_bb);
   emit_debug_info(loc);

   vcode_type_t vseverity = vtype_int(0, SEVERITY_FAILURE - 1);
   vcode_reg_t failure_reg = emit_const(vseverity, SEVERITY_FAILURE);

   vcode_reg_t const_str_reg = lower_wrap_string("\" is not a valid unit name");
   vcode_reg_t const_str_len = emit_uarray_len(const_str_reg, 0);
   vcode_reg_t extra_len = emit_add(const_str_len, emit_const(voffset, 1));
   vcode_reg_t msg_len = emit_add(tail_len, extra_len);
   vcode_reg_t mem_reg = emit_alloc(vchar, vchar, msg_len);

   emit_store_indirect(emit_const(vchar, '\"'), mem_reg);

   vcode_reg_t ptr1_reg = emit_array_ref(mem_reg, emit_const(voffset, 1));
   emit_copy(ptr1_reg, tail_ptr, tail_len);

   vcode_reg_t ptr2_reg = emit_array_ref(ptr1_reg, tail_len);
   emit_copy(ptr2_reg, emit_unwrap(const_str_reg), const_str_len);

   vcode_type_t vtype = lower_type(type);
   vcode_type_t vbounds = lower_bounds(type);

   vcode_reg_t locus = lower_debug_locus(type_unit(type, 0));
   emit_report(mem_reg, msg_len, failure_reg, locus);
   emit_return(emit_const(vtype, 0));

   vcode_select_block(match_bb);

   vcode_reg_t mul_ptr = emit_array_ref(mul_array_ptr, i_reg);
   vcode_reg_t mul_reg = emit_load_indirect(mul_ptr);
   vcode_reg_t result_reg = emit_mul(int_reg, mul_reg);

   lower_check_scalar_bounds(lu, result_reg, type, decl, NULL);
   emit_return(emit_cast(vtype, vbounds, result_reg));
}

static void lower_numeric_value_helper(lower_unit_t *lu, type_t type,
                                       tree_t decl, vcode_reg_t preg)
{
   vcode_reg_t result_reg;
   if (type_is_real(type)) {
      ident_t conv_fn = ident_new("NVC.TEXT_UTIL.STRING_TO_REAL(S)R");

      vcode_reg_t text_util_reg = lower_context_for_call(lu, conv_fn);
      vcode_reg_t conv_args[] = { text_util_reg, preg };

      vcode_type_t vreal = vtype_real(-DBL_MAX, DBL_MAX);

      result_reg = emit_fcall(conv_fn, vreal, vreal, VCODE_CC_VHDL,
                              conv_args, ARRAY_LEN(conv_args));
   }
   else {
      ident_t conv_fn =
         ident_new("NVC.TEXT_UTIL.STRING_TO_INT(S)21NVC.TEXT_UTIL.T_INT64");

      vcode_reg_t text_util_reg = lower_context_for_call(lu, conv_fn);
      vcode_reg_t conv_args[] = { text_util_reg, preg };

      vcode_type_t vint64 = vtype_int(INT64_MIN, INT64_MAX);

      result_reg = emit_fcall(conv_fn, vint64, vint64, VCODE_CC_VHDL,
                              conv_args, ARRAY_LEN(conv_args));
   }

   vcode_type_t vtype = lower_type(type);
   vcode_type_t vbounds = lower_bounds(type);

   lower_check_scalar_bounds(lu, result_reg, type, decl, NULL);
   emit_return(emit_cast(vtype, vbounds, result_reg));
}

static void lower_record_value_helper(lower_unit_t *lu, type_t type,
                                      tree_t decl, vcode_reg_t preg)
{
   vcode_type_t voffset = vtype_offset();
   vcode_type_t ctype = vtype_char();
   vcode_type_t strtype = vtype_uarray(1, ctype, ctype);
   vcode_type_t vtype = lower_type(type);
   vcode_type_t vnat = vtype_int(0, INT64_MAX);

   vcode_reg_t locus = lower_debug_locus(decl);

   vcode_var_t result_var =
      emit_var(vtype, vtype, ident_new("result"), VAR_HEAP);

   ident_t find_open_fn = ident_new("NVC.TEXT_UTIL.FIND_OPEN(S)N");
   ident_t find_close_fn = ident_new("NVC.TEXT_UTIL.FIND_CLOSE(SN)");
   ident_t next_delim_fn = ident_new("NVC.TEXT_UTIL.NEXT_DELIMITER(SN)S");
   vcode_reg_t text_util_reg = lower_context_for_call(lu, next_delim_fn);

   vcode_reg_t open_args[] = { text_util_reg, preg };
   vcode_reg_t open_reg = emit_fcall(find_open_fn, vnat, vnat,
                                     VCODE_CC_VHDL, open_args, 2);

   vcode_reg_t off_reg = emit_cast(voffset, voffset, open_reg);
   vcode_reg_t ptr_reg = emit_index(result_var, VCODE_INVALID_REG);

   const int nfields = type_fields(type);
   for (int i = 0; i < nfields; i++) {
      vcode_reg_t nd_args[] = { text_util_reg, preg, off_reg };
      vcode_reg_t nd_reg = emit_fcall(next_delim_fn, strtype, ctype,
                                      VCODE_CC_VHDL, nd_args, 3);

      type_t ftype = type_base_recur(tree_type(type_field(type, i)));

      ident_t func = ident_prefix(type_ident(ftype), ident_new("value"), '$');
      vcode_reg_t args[] = {
         lower_context_for_call(lu, func),
         nd_reg,
      };

      vcode_type_t fvtype = lower_func_result_type(ftype);
      vcode_type_t fvbounds = lower_bounds(ftype);

      vcode_reg_t value_reg = emit_fcall(func, fvtype, fvbounds,
                                         VCODE_CC_PREDEF, args, 2);

      vcode_reg_t vlen_reg = emit_uarray_len(nd_reg, 0);
      off_reg = emit_add(off_reg, vlen_reg);
      off_reg = emit_add(off_reg, emit_const(voffset, 1));   // Skip comma

      vcode_reg_t fptr_reg = emit_record_ref(ptr_reg, i);
      if (type_is_array(ftype)) {
         vcode_reg_t data_reg = lower_array_data(value_reg);
         vcode_reg_t count_reg = lower_array_total_len(lu, ftype, value_reg);
         emit_copy(fptr_reg, data_reg, count_reg);
      }
      else if (type_is_record(ftype))
         lower_copy_record(lu, ftype, fptr_reg, value_reg, locus);
      else
         emit_store_indirect(value_reg, fptr_reg);
   }

   vcode_reg_t close_args[] = { text_util_reg, preg, off_reg };
   emit_fcall(find_close_fn, VCODE_INVALID_TYPE, VCODE_INVALID_TYPE,
              VCODE_CC_VHDL, close_args, ARRAY_LEN(close_args));

   emit_return(ptr_reg);
}

static void lower_array_value_helper(lower_unit_t *lu, type_t type,
                                     tree_t decl, vcode_reg_t preg)
{
   vcode_type_t voffset = vtype_offset();
   vcode_type_t ctype = vtype_char();
   vcode_type_t strtype = vtype_uarray(1, ctype, ctype);
   vcode_type_t vnat = vtype_int(0, INT64_MAX);

   vcode_reg_t locus = lower_debug_locus(decl);

   vcode_reg_t zero_reg = emit_const(voffset, 0);
   vcode_reg_t one_reg = emit_const(voffset, 1);

   vcode_var_t i_var = lower_temp_var(lu, "i", voffset, voffset);
   emit_store(zero_reg, i_var);

   ident_t next_delim_fn = ident_new("NVC.TEXT_UTIL.NEXT_DELIMITER(SN)S");
   ident_t count_delim_fn = ident_new("NVC.TEXT_UTIL.COUNT_DELIMITERS(S)N");
   ident_t find_open_fn = ident_new("NVC.TEXT_UTIL.FIND_OPEN(S)N");
   ident_t find_quote_fn = ident_new("NVC.TEXT_UTIL.FIND_QUOTE(S)N");
   ident_t find_unquote_fn = ident_new("NVC.TEXT_UTIL.FIND_UNQUOTE(SN)N");
   ident_t find_close_fn = ident_new("NVC.TEXT_UTIL.FIND_CLOSE(SN)");
   ident_t bad_char_fn = ident_new("NVC.TEXT_UTIL.REPORT_BAD_CHAR(SC)");
   vcode_reg_t text_util_reg = lower_context_for_call(lu, next_delim_fn);

   type_t elem = type_base_recur(lower_elem_recur(type));
   vcode_type_t velem = lower_type(elem);
   vcode_type_t vbounds = lower_bounds(elem);

   if (type_is_character_array(type)) {
      vcode_block_t quote_bb = emit_block();
      vcode_block_t body_bb = emit_block();
      vcode_block_t bad_bb = emit_block();
      vcode_block_t good_bb = emit_block();
      vcode_block_t exit_bb = emit_block();
      vcode_block_t paren_bb = emit_block();

      vcode_reg_t quote_args[] = { text_util_reg, preg };
      vcode_reg_t quote_result_reg = emit_fcall(find_quote_fn, vnat, vnat,
                                                VCODE_CC_VHDL, quote_args, 2);
      vcode_reg_t quote_pos_reg = emit_cast(voffset, voffset, quote_result_reg);

      vcode_reg_t quoted_reg = emit_cmp(VCODE_CMP_EQ, quote_pos_reg, zero_reg);
      emit_cond(quoted_reg, paren_bb, quote_bb);

      vcode_select_block(quote_bb);

      vcode_reg_t unquote_args[] = { text_util_reg, preg, quote_result_reg };
      vcode_reg_t unquote_result_reg = emit_fcall(find_unquote_fn, vnat, vnat,
                                                  VCODE_CC_VHDL, unquote_args,
                                                  ARRAY_LEN(unquote_args));
      vcode_reg_t unquote_pos_reg =
         emit_cast(voffset, voffset, unquote_result_reg);

      vcode_reg_t count_reg = emit_sub(unquote_pos_reg, quote_pos_reg);
      vcode_reg_t mem_reg = emit_alloc(velem, vbounds, count_reg);

      vcode_var_t pos_var = lower_temp_var(lu, "pos", voffset, voffset);
      emit_store(emit_cast(voffset, voffset, quote_pos_reg), pos_var);

      const int nlits = type_enum_literals(elem);
      vcode_reg_t entry_vtype = vtype_int(0, nlits);
      vcode_reg_t map[256], def_reg = emit_const(entry_vtype, nlits);
      for (int i = 0; i < 256; i++)
         map[i] = def_reg;
      for (int i = 0; i < nlits; i++) {
         const ident_t id = tree_ident(type_enum_literal(elem, i));
         if (ident_char(id, 0) == '\'')
            map[(uint8_t)ident_char(id, 1)] = emit_const(entry_vtype, i);
      }

      vcode_type_t map_vtype = vtype_carray(256, entry_vtype, entry_vtype);
      vcode_reg_t map_array_reg = emit_const_array(map_vtype, map, 256);
      vcode_reg_t map_reg = emit_address_of(map_array_reg);

      vcode_reg_t data_reg = lower_array_data(preg);

      vcode_reg_t null_reg = emit_cmp(VCODE_CMP_EQ, count_reg, zero_reg);
      emit_cond(null_reg, exit_bb, body_bb);

      vcode_select_block(body_bb);

      vcode_reg_t i_reg = emit_load(i_var);
      vcode_reg_t pos_reg = emit_load(pos_var);
      vcode_reg_t sptr_reg = emit_array_ref(data_reg, pos_reg);
      vcode_reg_t dptr_reg = emit_array_ref(mem_reg, i_reg);
      vcode_reg_t char_reg = emit_load_indirect(sptr_reg);
      vcode_reg_t cast_reg = emit_cast(voffset, ctype, char_reg);
      vcode_reg_t mptr_reg = emit_array_ref(map_reg, cast_reg);
      vcode_reg_t entry_reg = emit_load_indirect(mptr_reg);

      vcode_reg_t bad_reg = emit_cmp(VCODE_CMP_EQ, entry_reg, def_reg);
      emit_cond(bad_reg, bad_bb, good_bb);

      vcode_select_block(good_bb);

      vcode_reg_t good_reg = emit_cast(velem, velem, entry_reg);
      emit_store_indirect(good_reg, dptr_reg);

      vcode_reg_t next_pos_reg = emit_add(pos_reg, one_reg);
      emit_store(next_pos_reg, pos_var);

      vcode_reg_t next_i_reg = emit_add(i_reg, one_reg);
      emit_store(next_i_reg, i_var);

      vcode_reg_t done_reg = emit_cmp(VCODE_CMP_EQ, next_i_reg, count_reg);
      emit_cond(done_reg, exit_bb, body_bb);

      vcode_select_block(bad_bb);

      vcode_reg_t bad_char_args[] = { text_util_reg, preg, char_reg };
      emit_fcall(bad_char_fn, VCODE_INVALID_TYPE, VCODE_INVALID_TYPE,
                 VCODE_CC_VHDL, bad_char_args, ARRAY_LEN(bad_char_args));

      emit_unreachable(locus);

      vcode_select_block(exit_bb);

      vcode_dim_t dims[] = {
         { .left  = emit_const(vtype_offset(), 1),
           .right = count_reg,
           .dir   = emit_const(vtype_bool(), RANGE_TO)
         }
      };

      vcode_reg_t wrap_reg = emit_wrap(mem_reg, dims, 1);
      emit_return(wrap_reg);

      vcode_select_block(paren_bb);

      lower_release_temp(lu, pos_var);
   }

   vcode_block_t body_bb = emit_block();
   vcode_block_t exit_bb = emit_block();

   vcode_reg_t count_args[] = { text_util_reg, preg };
   vcode_reg_t count_result_reg = emit_fcall(count_delim_fn, vnat, vnat,
                                             VCODE_CC_VHDL, count_args, 2);
   vcode_reg_t count_reg = emit_cast(voffset, voffset, count_result_reg);

   vcode_reg_t mem_reg = emit_alloc(velem, vbounds, count_reg);

   vcode_reg_t open_args[] = { text_util_reg, preg };
   vcode_reg_t open_reg = emit_fcall(find_open_fn, vnat, vnat,
                                     VCODE_CC_VHDL, open_args, 2);

   vcode_var_t pos_var = lower_temp_var(lu, "pos", voffset, voffset);
   emit_store(emit_cast(voffset, voffset, open_reg), pos_var);

   vcode_reg_t null_reg = emit_cmp(VCODE_CMP_EQ, count_reg, zero_reg);
   emit_cond(null_reg, exit_bb, body_bb);

   vcode_select_block(body_bb);

   vcode_reg_t i_reg = emit_load(i_var);
   vcode_reg_t pos_reg = emit_load(pos_var);

   vcode_reg_t nd_args[] = { text_util_reg, preg, pos_reg };
   vcode_reg_t nd_reg = emit_fcall(next_delim_fn, strtype, ctype,
                                   VCODE_CC_VHDL, nd_args, 3);

   ident_t func = ident_prefix(type_ident(elem), ident_new("value"), '$');
   vcode_reg_t args[] = {
      lower_context_for_call(lu, func),
      nd_reg,
   };

   vcode_reg_t value_reg = emit_fcall(func, lower_func_result_type(elem),
                                      vbounds, VCODE_CC_PREDEF, args, 2);
   vcode_reg_t ptr_reg = emit_array_ref(mem_reg, i_reg);

   if (type_is_record(elem))
      lower_copy_record(lu, elem, ptr_reg, value_reg, locus);
   else
      emit_store_indirect(value_reg, ptr_reg);

   vcode_reg_t next_i_reg = emit_add(i_reg, one_reg);
   emit_store(next_i_reg, i_var);

   vcode_reg_t vlen_reg = emit_add(emit_uarray_len(nd_reg, 0), one_reg);
   vcode_reg_t next_pos_reg = emit_add(pos_reg, vlen_reg);
   emit_store(next_pos_reg, pos_var);

   vcode_reg_t done_reg = emit_cmp(VCODE_CMP_EQ, next_i_reg, count_reg);
   emit_cond(done_reg, exit_bb, body_bb);

   vcode_select_block(exit_bb);

   vcode_reg_t close_args[] = { text_util_reg, preg, emit_load(pos_var) };
   emit_fcall(find_close_fn, VCODE_INVALID_TYPE, VCODE_INVALID_TYPE,
              VCODE_CC_VHDL, close_args, ARRAY_LEN(close_args));

   vcode_dim_t dims[] = {
      { .left  = emit_const(vtype_offset(), 1),
        .right = count_reg,
        .dir   = emit_const(vtype_bool(), RANGE_TO)
      }
   };

   vcode_reg_t wrap_reg = emit_wrap(mem_reg, dims, 1);
   emit_return(wrap_reg);
}

static void lower_value_helper(lower_unit_t *lu, object_t *obj)
{
   tree_t decl = tree_from_object(obj);

   type_t type = type_base_recur(tree_type(decl));
   assert(type_is_representable(type));

   vcode_set_result(lower_func_result_type(type));

   vcode_type_t vcontext = vtype_context(lu->parent->name);
   emit_param(vcontext, vcontext, ident_new("context"));

   vcode_type_t ctype = vtype_char();
   vcode_type_t strtype = vtype_uarray(1, ctype, ctype);
   vcode_reg_t preg = emit_param(strtype, strtype, ident_new("VAL"));

   switch (type_kind(type)) {
   case T_ENUM:
      lower_enum_value_helper(lu, type, decl, preg);
      break;
   case T_INTEGER:
   case T_REAL:
      lower_numeric_value_helper(lu, type, decl, preg);
      break;
   case T_PHYSICAL:
      lower_physical_value_helper(lu, type, decl, preg);
      break;
   case T_RECORD:
      lower_record_value_helper(lu, type, decl, preg);
      break;
   case T_ARRAY:
      lower_array_value_helper(lu, type, decl, preg);
      break;
   default:
      fatal_trace("cannot lower value helper for type %s", type_pp(type));
   }
}

static void lower_resolved_helper(lower_unit_t *lu, object_t *obj)
{
   tree_t decl = tree_from_object(obj);

   type_t type = tree_type(decl);
   assert(!type_is_homogeneous(type) && lower_can_be_signal(type));

   vcode_set_result(lower_func_result_type(type));

   vcode_type_t vtype = lower_type(type);
   vcode_type_t vbounds = lower_bounds(type);
   vcode_type_t vatype = lower_param_type(type, C_SIGNAL, PORT_IN);

   vcode_type_t vcontext = vtype_context(lu->parent->name);
   emit_param(vcontext, vcontext, ident_new("context"));

   vcode_reg_t p_reg = emit_param(vatype, vbounds, ident_new("p"));

   vcode_var_t var = emit_var(vtype, vbounds, ident_new("result"), 0);

   vcode_reg_t data_reg, result_reg;
   if (vtype_kind(vtype) == VCODE_TYPE_UARRAY) {
      type_t elem = lower_elem_recur(type);
      vcode_reg_t count_reg = lower_array_total_len(lu, type, p_reg);
      data_reg = emit_alloc(lower_type(elem), lower_bounds(elem),
                            count_reg);

      result_reg = lower_rewrap(data_reg, p_reg);
      emit_store(result_reg, var);
   }
   else
      data_reg = result_reg = emit_index(var, VCODE_INVALID_VAR);

   lower_for_each_field(lu, type, p_reg, data_reg, VCODE_INVALID_REG,
                        lower_resolved_field_cb, NULL);
   emit_return(result_reg);
}

static void lower_copy_helper(lower_unit_t *lu, object_t *obj)
{
   tree_t decl = tree_from_object(obj);

   type_t type = tree_type(decl);
   assert(!lower_trivially_copyable(type));

   vcode_type_t vbounds = lower_bounds(type);
   vcode_type_t vdtype = lower_param_type(type, C_VARIABLE, PORT_OUT);
   vcode_type_t vstype = lower_param_type(type, C_VARIABLE, PORT_IN);
   vcode_type_t vlocus = vtype_debug_locus();
   vcode_type_t vcontext = vtype_context(lu->parent->name);
   emit_param(vcontext, vcontext, ident_new("context"));

   vcode_reg_t pdst_reg = emit_param(vdtype, vbounds, ident_new("dest"));
   vcode_reg_t psrc_reg = emit_param(vstype, vbounds, ident_new("src"));
   vcode_reg_t plocus = emit_param(vlocus, vlocus, ident_new("locus"));

   if (type_is_record(type)) {
      const int nfields = type_fields(type);
      for (int i = 0; i < nfields; i++) {
         type_t ftype = tree_type(type_field(type, i));

         vcode_reg_t dst_ptr = emit_record_ref(pdst_reg, i);
         vcode_reg_t src_ptr = emit_record_ref(psrc_reg, i);

         if (type_is_scalar(ftype) || type_is_access(ftype)) {
            vcode_reg_t src_reg = emit_load_indirect(src_ptr);
            emit_store_indirect(src_reg, dst_ptr);
         }
         else if (type_is_array(ftype)) {
            vcode_reg_t src_reg = src_ptr;
            if (have_uarray_ptr(src_reg))
               src_reg = emit_load_indirect(src_ptr);

            vcode_reg_t dst_reg = dst_ptr;
            if (have_uarray_ptr(dst_reg))
               dst_reg = emit_load_indirect(dst_ptr);

            lower_copy_array(lu, ftype, ftype, dst_reg, src_reg, plocus);
         }
         else if (type_is_record(ftype))
            lower_copy_record(lu, ftype, dst_ptr, src_ptr, plocus);
         else
            fatal_trace("unhandled field type %s in lower_copy_helper",
                        type_pp(ftype));
      }
   }
   else {
      assert(type_is_array(type));

      type_t elem = lower_elem_recur(type);
      lower_check_array_sizes(lu, type, type, pdst_reg, psrc_reg, plocus);

      vcode_reg_t count_reg = lower_array_total_len(lu, type, psrc_reg);

      vcode_reg_t src_data = lower_array_data(psrc_reg);
      vcode_reg_t dst_data = lower_array_data(pdst_reg);

      assert(!lower_trivially_copyable(elem));

      vcode_type_t voffset = vtype_offset();

      vcode_var_t i_var = lower_temp_var(lu, "i", voffset, voffset);
      vcode_reg_t zero_reg = emit_const(voffset, 0);
      emit_store(zero_reg, i_var);

      vcode_block_t body_bb = emit_block();
      vcode_block_t exit_bb = emit_block();

      vcode_reg_t null_reg = emit_cmp(VCODE_CMP_EQ, count_reg, zero_reg);
      emit_cond(null_reg, exit_bb, body_bb);

      vcode_select_block(body_bb);

      vcode_reg_t i_reg = emit_load(i_var);
      vcode_reg_t dptr_reg = emit_array_ref(dst_data, i_reg);
      vcode_reg_t sptr_reg = emit_array_ref(src_data, i_reg);

      lower_copy_record(lu, elem, dptr_reg, sptr_reg, plocus);

      vcode_reg_t next_reg = emit_add(i_reg, emit_const(voffset, 1));
      emit_store(next_reg, i_var);

      vcode_reg_t done_reg = emit_cmp(VCODE_CMP_EQ, next_reg, count_reg);
      emit_cond(done_reg, exit_bb, body_bb);

      vcode_select_block(exit_bb);

      lower_release_temp(lu, i_var);
   }

   emit_return(VCODE_INVALID_REG);
}

static void lower_new_helper(lower_unit_t *lu, object_t *obj)
{
   tree_t decl = tree_from_object(obj);

   type_t type = tree_type(decl);
   assert(type_is_record(type) && !lower_const_bounds(type));

   vcode_type_t vbounds = lower_bounds(type);
   vcode_type_t vdtype = lower_param_type(type, C_VARIABLE, PORT_OUT);
   vcode_type_t vstype = lower_param_type(type, C_VARIABLE, PORT_IN);
   vcode_type_t vcontext = vtype_context(lu->parent->name);
   emit_param(vcontext, vcontext, ident_new("context"));

   vcode_reg_t pdst_reg = emit_param(vdtype, vbounds, ident_new("dest"));
   vcode_reg_t psrc_reg = emit_param(vstype, vbounds, ident_new("src"));

   const int nfields = type_fields(type);
   for (int i = 0; i < nfields; i++) {
      type_t ftype = tree_type(type_field(type, i));

      vcode_reg_t dst_ptr = emit_record_ref(pdst_reg, i);
      vcode_reg_t src_ptr = emit_record_ref(psrc_reg, i);

      if (type_is_scalar(ftype) || type_is_access(ftype)) {
         vcode_reg_t src_reg = emit_load_indirect(src_ptr);
         emit_store_indirect(src_reg, dst_ptr);
      }
      else if (type_is_array(ftype)) {
         vcode_reg_t src_reg = src_ptr;
         if (have_uarray_ptr(src_reg))
            src_reg = emit_load_indirect(src_ptr);

         vcode_reg_t count_reg = lower_array_total_len(lu, ftype, src_reg);
         vcode_reg_t dst_reg = dst_ptr;

         if (have_uarray_ptr(dst_reg)) {
            type_t scalar_elem = lower_elem_recur(ftype);
            vcode_type_t vtype = lower_type(scalar_elem);
            vcode_reg_t mem_reg = emit_new(vtype, count_reg);
            vcode_reg_t all_reg = emit_all(mem_reg);
            vcode_reg_t wrap_reg = lower_rewrap(all_reg, src_reg);

            emit_store_indirect(wrap_reg, dst_ptr);
            dst_reg = wrap_reg;
         }

         vcode_reg_t src_data = lower_array_data(src_reg);
         vcode_reg_t dst_data = lower_array_data(dst_reg);
         emit_copy(dst_data, src_data, count_reg);
      }
      else if (type_is_record(ftype))
         lower_new_record(lu, ftype, dst_ptr, src_ptr);
      else
         fatal_trace("unhandled type %s in lower_new_helper", type_pp(ftype));
   }

   emit_return(VCODE_INVALID_REG);
}

static void lower_instantiated_package(lower_unit_t *parent, tree_t decl)
{
   vcode_state_t state;
   vcode_state_save(&state);

   vcode_select_unit(parent->vunit);

   ident_t name = ident_prefix(vcode_unit_name(), tree_ident(decl), '.');

   vcode_unit_t vu = emit_package(name, tree_to_object(decl), parent->vunit);

   lower_unit_t *lu = lower_unit_new(parent->registry, parent,
                                     vu, parent->cover, decl);
   unit_registry_put(parent->registry, lu);

   cover_push_scope(lu->cover, decl);

   lower_generics(lu, decl);
   lower_decls(lu, decl);

   emit_return(VCODE_INVALID_REG);

   cover_pop_scope(lu->cover);

   unit_registry_finalise(parent->registry, lu);

   vcode_state_restore(&state);

   vcode_type_t vcontext = vtype_context(name);
   vcode_var_t var = emit_var(vcontext, vcontext, name, 0);

   vcode_reg_t pkg_reg = emit_package_init(name, emit_context_upref(0));
   emit_store(pkg_reg, var);

   const int ndecls = tree_decls(tree_ref(decl));
   for (int i = 0; i < ndecls; i++)
      lower_put_vcode_obj(tree_decl(decl, i), var | INSTANCE_BIT, parent);
}

static void lower_hier_decl(lower_unit_t *lu, tree_t decl)
{
   lu->hier = decl;

   if (lu->cover != NULL)
      cover_ignore_from_pragmas(lu->cover, tree_ref(decl));
}

static void lower_decl(lower_unit_t *lu, tree_t decl)
{
   PUSH_DEBUG_INFO(decl);

   switch (tree_kind(decl)) {
   case T_CONST_DECL:
   case T_VAR_DECL:
      lower_var_decl(lu, decl);
      break;

   case T_SIGNAL_DECL:
      lower_signal_decl(lu, decl);
      break;

   case T_IMPLICIT_SIGNAL:
      lower_implicit_decl(lu, decl);
      break;

   case T_FILE_DECL:
      lower_file_decl(lu, decl);
      break;

   case T_ALIAS:
      lower_alias_decl(lu, decl);
      break;

   case T_HIER:
      lower_hier_decl(lu, decl);
      break;

   case T_TYPE_DECL:
      {
         type_t type = tree_type(decl);
         ident_t id = type_ident(type);
         object_t *obj = tree_to_object(decl);

         if (type_is_representable(type)) {
            ident_t image = ident_prefix(id, ident_new("image"), '$');
            unit_registry_defer(lu->registry, image, lu, emit_function,
                                lower_image_helper, NULL, obj);

            ident_t value = ident_prefix(id, ident_new("value"), '$');
            unit_registry_defer(lu->registry, value, lu, emit_function,
                                lower_value_helper, NULL, obj);
         }

         if (!type_is_homogeneous(type) && lower_can_be_signal(type)) {
            ident_t resolved = ident_prefix(id, ident_new("resolved"), '$');
            unit_registry_defer(lu->registry, resolved, lu, emit_function,
                                lower_resolved_helper, NULL, obj);
         }

         if (!lower_trivially_copyable(type)) {
            ident_t copy = ident_prefix(id, ident_new("copy"), '$');
            unit_registry_defer(lu->registry, copy, lu, emit_function,
                                lower_copy_helper, NULL, obj);
         }

         if (type_is_record(type) && !lower_const_bounds(type)) {
            ident_t new = ident_prefix(id, ident_new("new"), '$');
            unit_registry_defer(lu->registry, new, lu, emit_function,
                                lower_new_helper, NULL, obj);
         }
      }
      break;

   case T_FUNC_DECL:
   case T_PROC_DECL:
   case T_ATTR_SPEC:
   case T_ATTR_DECL:
   case T_COMPONENT:
   case T_USE:
   case T_SPEC:
   case T_GROUP:
   case T_GROUP_TEMPLATE:
   case T_SUBTYPE_DECL:
   case T_VIEW_DECL:
   case T_PROT_DECL:
      break;

   case T_PACKAGE:
   case T_PACK_BODY:
   case T_PACK_INST:
      if (unit_needs_cgen(decl))
         lower_instantiated_package(lu, decl);
      break;

   case T_PSL:
      psl_lower_decl(lu->registry, lu, tree_psl(decl), tree_ident(decl));
      break;

   default:
      fatal_trace("cannot lower decl kind %s", tree_kind_str(tree_kind(decl)));
   }
}

void lower_finished(lower_unit_t *lu)
{
   assert(!lu->finished);

   vcode_select_unit(lu->vunit);
   vcode_opt();

   if (opt_get_verbose(OPT_DUMP_VCODE, istr(vcode_unit_name())))
      vcode_dump();

   lu->finished = true;
}

static void lower_protected_body(lower_unit_t *lu, object_t *obj)
{
   tree_t body = tree_from_object(obj);
   assert(tree_kind(body) == T_PROT_BODY);

   cover_push_scope(lu->cover, body);

   if (standard() >= STD_19)
      lower_decls(lu, tree_primary(body));

   lower_decls(lu, body);
   emit_return(VCODE_INVALID_REG);

   cover_pop_scope(lu->cover);
}

static void lower_decls(lower_unit_t *lu, tree_t scope)
{
   // Lower declarations in two passes with subprograms after signals,
   // variables, constants, etc.

   const int ndecls = tree_decls(scope);

   for (int i = 0; i < ndecls; i++) {
      tree_t d = tree_decl(scope, i);
      const tree_kind_t kind = tree_kind(d);
      if (lu->mode == LOWER_THUNK && kind == T_SIGNAL_DECL)
         continue;
      else if (is_subprogram(d) || kind == T_PROT_BODY)
         continue;
      else
         lower_decl(lu, d);
   }

   for (int i = 0; i < ndecls; i++) {
      tree_t d = tree_decl(scope, i);
      const tree_kind_t kind = tree_kind(d);
      if (!is_subprogram(d) && kind != T_PROT_BODY)
         continue;

      vcode_block_t bb = vcode_active_block();

      switch (kind) {
      case T_FUNC_INST:
      case T_FUNC_BODY:
         unit_registry_defer(lu->registry, tree_ident2(d),
                             lu, emit_function, lower_func_body,
                             lu->cover, tree_to_object(d));
         break;
      case T_PROC_INST:
      case T_PROC_BODY:
         {
            const bool never_waits = !!(tree_flags(d) & TREE_F_NEVER_WAITS)
               || (tree_flags(d) & TREE_F_PROTECTED);
            emit_fn_t emitfn = never_waits ? emit_function : emit_procedure;

            unit_registry_defer(lu->registry, tree_ident2(d),
                                lu, emitfn, lower_proc_body, lu->cover,
                                tree_to_object(d));
         }
         break;
      case T_PROT_BODY:
         unit_registry_defer(lu->registry, type_ident(tree_type(d)),
                             lu, emit_protected, lower_protected_body,
                             lu->cover, tree_to_object(d));
         break;
      case T_FUNC_DECL:
        {
           const subprogram_kind_t kind = tree_subkind(d);
           if (kind == S_USER || kind == S_FOREIGN)
              continue;
           else if (is_open_coded_builtin(kind))
              continue;

           unit_registry_defer(lu->registry, tree_ident2(d),
                               lu, emit_function, lower_predef,
                               lu->cover, tree_to_object(d));
        }
        break;
      default:
         break;
      }

      vcode_select_unit(lu->vunit);
      vcode_select_block(bb);
   }
}

static bool lower_has_subprograms(tree_t scope)
{
   const int ndecls = tree_decls(scope);
   for (int i = 0; i < ndecls; i++) {
      tree_t d = tree_decl(scope, i);
      const tree_kind_t kind = tree_kind(d);
      if (kind == T_FUNC_BODY || kind == T_PROC_BODY)
         return true;
      else if (kind == T_FUNC_INST || kind == T_PROC_INST)
         return true;
      else if (kind == T_TYPE_DECL) {
         // Predefined operators for certain types may reference the
         // parameters: e.g. an array with non-static length
         type_t type = tree_type(d);
         if (type_is_record(type) || type_is_array(type))
            return true;
      }
   }

   return false;
}

static void lower_subprogram_ports(lower_unit_t *lu, tree_t body,
                                   bool params_as_vars)
{
   const int nports = tree_ports(body);
   for (int i = 0; i < nports; i++) {
      tree_t p = tree_port(body, i);
      type_t type = tree_type(p);

      const class_t class = tree_class(p);
      const port_mode_t mode = tree_subkind(p);

      vcode_type_t vtype = lower_param_type(type, class, mode);
      vcode_type_t vbounds = lower_bounds(type);

      vcode_reg_t preg = emit_param(vtype, vbounds, tree_ident(p));
      if (params_as_vars) {
         vcode_var_t var = emit_var(vtype, vbounds, tree_ident(p), 0);
         emit_store(preg, var);
         lower_put_vcode_obj(p, var | PARAM_VAR_BIT, lu);
      }
      else
         lower_put_vcode_obj(p, preg, lu);
   }
}

static ident_t lower_predef_func_name(type_t type, const char *op)
{
   type_t base = type_base_recur(type);

   LOCAL_TEXT_BUF tb = tb_new();
   tb_printf(tb, "%s.\"%s\"(", istr(ident_runtil(type_ident(base), '.')), op);
   mangle_one_type(tb, base);
   mangle_one_type(tb, base);
   tb_cat(tb, ")");
   mangle_one_type(tb, std_type(NULL, STD_BOOLEAN));

   return ident_new(tb_get(tb));
}

static void lower_array_cmp_inner(lower_unit_t *lu,
                                  vcode_reg_t lhs_data,
                                  vcode_reg_t rhs_data,
                                  vcode_reg_t lhs_array,
                                  vcode_reg_t rhs_array,
                                  type_t left_type,
                                  type_t right_type,
                                  vcode_cmp_t pred,
                                  vcode_block_t fail_bb)
{
   // Behaviour of relational operators on arrays is described in
   // LRM 93 section 7.2.2

   assert(pred == VCODE_CMP_EQ || pred == VCODE_CMP_LT
          || pred == VCODE_CMP_LEQ);

   const int ndims = dimension_of(left_type);
   assert(dimension_of(right_type) == ndims);

   vcode_reg_t left_len = lower_array_len(lu, left_type, 0, lhs_array);
   for (int i = 1; i < ndims; i++) {
      vcode_reg_t dim_len = lower_array_len(lu, left_type, i, lhs_array);
      left_len = emit_mul(dim_len, left_len);
   }

   vcode_reg_t right_len = lower_array_len(lu, right_type, 0, rhs_array);
   for (int i = 1; i < ndims; i++) {
      vcode_reg_t dim_len = lower_array_len(lu, right_type, i, rhs_array);
      right_len = emit_mul(dim_len, right_len);
   }

   vcode_type_t voffset = vtype_offset();
   vcode_var_t i_var = lower_temp_var(lu, "i", voffset, voffset);
   emit_store(emit_const(voffset, 0), i_var);

   vcode_block_t test_bb = emit_block();
   vcode_block_t body_bb = emit_block();
   vcode_block_t exit_bb = emit_block();

   type_t elem_type = type_elem(left_type);

   vcode_reg_t stride = VCODE_INVALID_REG;
   if (type_is_array(elem_type))
      stride = lower_array_stride(lu, left_type, lhs_array);

   vcode_reg_t len_eq = emit_cmp(VCODE_CMP_EQ, left_len, right_len);

   if (pred == VCODE_CMP_EQ)
      emit_cond(len_eq, test_bb, fail_bb);
   else
      emit_jump(test_bb);

   // Loop test

   vcode_select_block(test_bb);

   vcode_reg_t i_loaded = emit_load(i_var);

   if (pred == VCODE_CMP_EQ) {
      vcode_reg_t done = emit_cmp(VCODE_CMP_EQ, i_loaded, left_len);
      emit_cond(done, exit_bb, body_bb);
   }
   else {
      vcode_block_t check_r_len_bb = emit_block();

      vcode_reg_t len_ge_l = emit_cmp(VCODE_CMP_GEQ, i_loaded, left_len);
      emit_cond(len_ge_l, exit_bb, check_r_len_bb);

      vcode_select_block(check_r_len_bb);

      vcode_reg_t len_ge_r = emit_cmp(VCODE_CMP_GEQ, i_loaded, right_len);
      emit_cond(len_ge_r, fail_bb, body_bb);
   }

   // Loop body

   vcode_select_block(body_bb);

   vcode_reg_t ptr_inc = i_loaded;
   if (stride != VCODE_INVALID_REG)
      ptr_inc = emit_mul(ptr_inc, stride);

   vcode_reg_t inc = emit_add(i_loaded, emit_const(voffset, 1));
   emit_store(inc, i_var);

   vcode_reg_t i_eq_len = emit_cmp(VCODE_CMP_EQ, inc, left_len);

   vcode_reg_t l_ptr = emit_array_ref(lhs_data, ptr_inc);
   vcode_reg_t r_ptr = emit_array_ref(rhs_data, ptr_inc);

   if (type_is_array(elem_type)) {
      vcode_reg_t lhs_sub_array = VCODE_INVALID_REG;
      vcode_reg_t rhs_sub_array = VCODE_INVALID_REG;
      if (type_is_unconstrained(elem_type)) {
         lhs_sub_array = lower_wrap_element(lu, left_type, lhs_array, l_ptr);
         rhs_sub_array = lower_wrap_element(lu, right_type, rhs_array, r_ptr);
      }

      lower_array_cmp_inner(lu, l_ptr, r_ptr, lhs_sub_array, rhs_sub_array,
                            type_elem(left_type), type_elem(right_type),
                            pred, fail_bb);
      emit_jump(test_bb);
   }
   else if (type_is_record(elem_type)) {
      lower_for_each_field(lu, elem_type, l_ptr, r_ptr, VCODE_INVALID_REG,
                           lower_predef_field_eq_cb,
                           (void *)(uintptr_t)fail_bb);
      emit_jump(test_bb);
   }
   else {
      vcode_reg_t l_val = emit_load_indirect(l_ptr);
      vcode_reg_t r_val = emit_load_indirect(r_ptr);

      if (pred == VCODE_CMP_EQ) {
         vcode_reg_t eq = emit_cmp(pred, l_val, r_val);
         emit_cond(eq, test_bb, fail_bb);
      }
      else {
         vcode_reg_t cmp = emit_cmp(pred, l_val, r_val);
         vcode_reg_t eq  = emit_cmp(VCODE_CMP_EQ, l_val, r_val);

         vcode_reg_t done = emit_or(emit_not(eq), emit_and(len_eq, i_eq_len));

         vcode_block_t cmp_result_bb = emit_block();
         emit_cond(done, cmp_result_bb, test_bb);

         vcode_select_block(cmp_result_bb);
         emit_cond(cmp, exit_bb, fail_bb);
      }
   }

   // Epilogue

   vcode_select_block(exit_bb);
}

static void lower_predef_array_cmp(lower_unit_t *lu, tree_t decl,
                                   vcode_cmp_t pred)
{
   type_t r0_type = tree_type(tree_port(decl, 0));
   type_t r1_type = tree_type(tree_port(decl, 1));

   vcode_reg_t r0 = 1, r1 = 2;
   vcode_reg_t r0_data = lower_array_data(r0);
   vcode_reg_t r1_data = lower_array_data(r1);

   vcode_block_t fail_bb = emit_block();

   lower_array_cmp_inner(lu, r0_data, r1_data, r0, r1, r0_type, r1_type,
                         pred, fail_bb);

   emit_return(emit_const(vtype_bool(), 1));

   vcode_select_block(fail_bb);
   emit_return(emit_const(vtype_bool(), 0));
}

static void lower_predef_field_eq_cb(lower_unit_t *lu, tree_t field,
                                     vcode_reg_t r0, vcode_reg_t r1,
                                     vcode_reg_t locus, void *context)
{
   vcode_block_t fail_bb = (uintptr_t)context;

   type_t ftype = tree_type(field);
   if (type_is_array(ftype)) {
      vcode_reg_t r0_data = r0, r0_array = VCODE_INVALID_REG;
      if (have_uarray_ptr(r0)) {
         r0_array = emit_load_indirect(r0);
         r0_data = lower_array_data(r0_array);
      }

      vcode_reg_t r1_data = r1, r1_array = VCODE_INVALID_REG;
      if (have_uarray_ptr(r1)) {
         r1_array = emit_load_indirect(r1);
         r1_data = lower_array_data(r1_array);
      }

      lower_array_cmp_inner(lu, r0_data, r1_data, r0_array, r1_array,
                            ftype, ftype, VCODE_CMP_EQ, fail_bb);
   }
   else if (type_is_record(ftype))
      lower_for_each_field(lu, ftype, r0, r1, locus, lower_predef_field_eq_cb,
                           (void *)(uintptr_t)fail_bb);
   else {
      vcode_reg_t r0_load = emit_load_indirect(r0);
      vcode_reg_t r1_load = emit_load_indirect(r1);
      vcode_reg_t cmp_reg = emit_cmp(VCODE_CMP_EQ, r0_load, r1_load);

      vcode_block_t next_bb = emit_block();
      emit_cond(cmp_reg, next_bb, fail_bb);
      vcode_select_block(next_bb);
   }
}

static void lower_predef_record_eq(lower_unit_t *lu, tree_t decl)
{
   vcode_reg_t r0 = 1, r1 = 2;
   type_t type = tree_type(tree_port(decl, 0));

   vcode_block_t fail_bb = emit_block();

   lower_for_each_field(lu, type, r0, r1, VCODE_INVALID_REG,
                        lower_predef_field_eq_cb, (void *)(uintptr_t)fail_bb);

   emit_return(emit_const(vtype_bool(), 1));

   vcode_select_block(fail_bb);
   emit_return(emit_const(vtype_bool(), 0));
}

static void lower_predef_to_string(lower_unit_t *lu, tree_t decl)
{
   // LRM 08 section 5.7 on string representations

   type_t arg_type = tree_type(tree_port(decl, 0));
   vcode_reg_t context_reg = 0, r0 = 1;

   if (type_is_array(arg_type)) {
      type_t elem = type_elem(arg_type);
      if (type_is_enum(elem) && all_character_literals(elem)) {
         lower_character_array_image_helper(lu, arg_type, r0, false);
         return;
      }
   }

   assert(type_is_representable(arg_type));

   ident_t func = ident_prefix(type_ident(arg_type), ident_new("image"), '$');
   vcode_type_t ctype = vtype_char();
   vcode_type_t rtype = vtype_uarray(1, ctype, ctype);
   vcode_reg_t args[] = { context_reg, r0 };
   vcode_reg_t str_reg =
      emit_fcall(func, rtype, ctype, VCODE_CC_PREDEF, args, 2);

   if (type_is_enum(arg_type)) {
      // If the result is a character literal return just the character
      // without the quotes
      vcode_reg_t quote_reg = emit_const(ctype, '\'');
      vcode_reg_t data_reg  = lower_array_data(str_reg);
      vcode_reg_t char0_reg = emit_load_indirect(data_reg);
      vcode_reg_t is_quote  = emit_cmp(VCODE_CMP_EQ, char0_reg, quote_reg);

      vcode_block_t char_bb  = emit_block();
      vcode_block_t other_bb = emit_block();

      emit_cond(is_quote, char_bb, other_bb);

      vcode_select_block(char_bb);

      vcode_reg_t one_reg   = emit_const(vtype_offset(), 1);
      vcode_reg_t char1_ptr = emit_array_ref(data_reg, one_reg);
      vcode_reg_t left_reg  = emit_uarray_left(str_reg, 0);
      vcode_reg_t dir_reg   = emit_uarray_dir(str_reg, 0);

      vcode_dim_t dims[] = {
         { .left  = left_reg,
           .right = left_reg,
           .dir   = dir_reg
         }
      };
      emit_return(emit_wrap(char1_ptr, dims, 1));

      vcode_select_block(other_bb);
   }

   emit_return(str_reg);
}

static void lower_predef_bit_shift(lower_unit_t *lu, tree_t decl,
                                   subprogram_kind_t kind)
{
   type_t type = tree_type(tree_port(decl, 0));
   type_t elem = type_elem(type);

   vcode_type_t vtype = lower_type(elem);
   vcode_type_t vbounds = lower_bounds(elem);
   vcode_type_t voffset = vtype_offset();

   vcode_reg_t r0 = 1, r1 = 2;

   vcode_reg_t data_reg = lower_array_data(r0);
   vcode_reg_t len_reg  = lower_array_len(lu, type, 0, r0);

   vcode_block_t null_bb = emit_block();
   vcode_block_t non_null_bb = emit_block();

   vcode_reg_t is_null_reg =
      emit_cmp(VCODE_CMP_EQ, len_reg, emit_const(voffset, 0));
   emit_cond(is_null_reg, null_bb, non_null_bb);

   vcode_select_block(null_bb);
   emit_return(r0);

   vcode_select_block(non_null_bb);

   vcode_reg_t shift_reg = emit_cast(voffset, voffset, r1);
   vcode_reg_t mem_reg = emit_alloc(vtype, vbounds, len_reg);

   vcode_var_t i_var = lower_temp_var(lu, "i", voffset, voffset);
   emit_store(emit_const(voffset, 0), i_var);

   vcode_block_t cmp_bb  = emit_block();
   vcode_block_t body_bb = emit_block();
   vcode_block_t exit_bb = emit_block();

   vcode_reg_t def_reg = VCODE_INVALID_REG;
   switch (kind) {
   case S_SLL: case S_SRL: case S_ROL: case S_ROR:
      def_reg = emit_const(vtype, 0);
      break;
   case S_SRA:
      {
         vcode_reg_t len_minus_1 = emit_sub(len_reg, emit_const(voffset, 1));
         vcode_reg_t last_ptr = emit_array_ref(data_reg, len_minus_1);
         def_reg = emit_load_indirect(last_ptr);
      }
      break;
   case S_SLA:
      def_reg = emit_load_indirect(data_reg);
      break;
   default:
      break;
   }

   vcode_reg_t shift_is_neg =
      emit_cmp(VCODE_CMP_LT, shift_reg, emit_const(voffset, 0));

   emit_jump(cmp_bb);

   vcode_select_block(cmp_bb);

   vcode_reg_t i_reg  = emit_load(i_var);
   vcode_reg_t eq_reg = emit_cmp(VCODE_CMP_EQ, i_reg, len_reg);
   emit_cond(eq_reg, exit_bb, body_bb);

   vcode_select_block(body_bb);

   vcode_reg_t cmp_reg = VCODE_INVALID_REG;
   switch (kind) {
   case S_SRL: case S_SRA:
      {
         vcode_reg_t neg_reg =
            emit_cmp(VCODE_CMP_LT, i_reg, emit_add(len_reg, shift_reg));
         vcode_reg_t pos_reg =
            emit_cmp(VCODE_CMP_GEQ, i_reg, shift_reg);
         cmp_reg = emit_select(shift_is_neg, neg_reg, pos_reg);
      }
      break;
   case S_SLL: case S_SLA:
      {
         vcode_reg_t neg_reg =
            emit_cmp(VCODE_CMP_GEQ, i_reg, emit_neg(shift_reg));
         vcode_reg_t pos_reg =
            emit_cmp(VCODE_CMP_LT, i_reg, emit_sub(len_reg, shift_reg));
         cmp_reg = emit_select(shift_is_neg, neg_reg, pos_reg);
      }
      break;
   case S_ROL: case S_ROR:
      cmp_reg = emit_const(vtype_bool(), 1);
   default:
      break;
   }

   vcode_reg_t dst_ptr = emit_array_ref(mem_reg, i_reg);

   vcode_reg_t next_reg = emit_add(i_reg, emit_const(vtype_offset(), 1));
   emit_store(next_reg, i_var);

   vcode_block_t true_bb = emit_block();
   vcode_block_t false_bb = emit_block();

   emit_cond(cmp_reg, true_bb, false_bb);

   vcode_select_block(true_bb);

   vcode_reg_t src_reg = VCODE_INVALID_REG;
   switch (kind) {
   case S_SLL: case S_SLA:
      src_reg = emit_add(i_reg, shift_reg);
      break;
   case S_SRL: case S_SRA:
      src_reg = emit_sub(i_reg, shift_reg);
      break;
   case S_ROL:
      src_reg = emit_mod(emit_add(i_reg, emit_add(len_reg, shift_reg)),
                         len_reg);
      break;
   case S_ROR:
      src_reg = emit_mod(emit_add(i_reg, emit_sub(len_reg, shift_reg)),
                         len_reg);
      break;
   default:
      break;
   }

   vcode_reg_t load_reg = emit_load_indirect(emit_array_ref(data_reg, src_reg));
   emit_store_indirect(load_reg, dst_ptr);
   emit_jump(cmp_bb);

   vcode_select_block(false_bb);
   emit_store_indirect(def_reg, dst_ptr);
   emit_jump(cmp_bb);

   vcode_select_block(exit_bb);

   vcode_reg_t left_reg  = emit_uarray_left(r0, 0);
   vcode_reg_t right_reg = emit_uarray_right(r0, 0);
   vcode_reg_t dir_reg   = emit_uarray_dir(r0, 0);

   vcode_dim_t dims[] = { { left_reg, right_reg, dir_reg } };
   emit_return(emit_wrap(mem_reg, dims, 1));
}

static void lower_predef_bit_vec_op(lower_unit_t *lu, tree_t decl,
                                    subprogram_kind_t kind)
{
   type_t type = tree_type(tree_port(decl, 0));
   type_t elem = type_elem(type);

   vcode_type_t vtype = lower_type(elem);
   vcode_type_t vbounds = lower_bounds(elem);
   vcode_type_t voffset = vtype_offset();

   vcode_reg_t r0 = 1, r1 = 2;

   vcode_reg_t data0_reg = lower_array_data(r0);
   vcode_reg_t data1_reg = VCODE_INVALID_REG;
   if (kind != S_ARRAY_NOT)
      data1_reg = lower_array_data(r1);

   vcode_reg_t len0_reg = lower_array_len(lu, type, 0, r0);
   vcode_reg_t len1_reg = VCODE_INVALID_REG;
   if (kind != S_ARRAY_NOT) {
      len1_reg = lower_array_len(lu, type, 0, r1);

      vcode_block_t fail_bb = emit_block();
      vcode_block_t cont_bb = emit_block();

      vcode_reg_t len_eq = emit_cmp(VCODE_CMP_EQ, len0_reg, len1_reg);
      emit_cond(len_eq, cont_bb, fail_bb);

      vcode_select_block(fail_bb);

      vcode_type_t vseverity = vtype_int(0, SEVERITY_FAILURE - 1);
      vcode_reg_t failure_reg = emit_const(vseverity, SEVERITY_FAILURE);

      vcode_reg_t msg_reg =
         lower_wrap_string("arguments have different lengths");
      vcode_reg_t msg_len = emit_uarray_len(msg_reg, 0);

      vcode_reg_t locus = lower_debug_locus(decl);
      emit_report(emit_unwrap(msg_reg), msg_len, failure_reg, locus);
      emit_return(r0);

      vcode_select_block(cont_bb);
   }

   vcode_reg_t mem_reg = emit_alloc(vtype, vbounds, len0_reg);

   vcode_var_t i_var = lower_temp_var(lu, "i", voffset, voffset);
   emit_store(emit_const(voffset, 0), i_var);

   vcode_block_t cmp_bb  = emit_block();
   vcode_block_t body_bb = emit_block();
   vcode_block_t exit_bb = emit_block();

   emit_jump(cmp_bb);

   vcode_select_block(cmp_bb);

   vcode_reg_t i_reg  = emit_load(i_var);
   vcode_reg_t eq_reg = emit_cmp(VCODE_CMP_EQ, i_reg, len0_reg);
   emit_cond(eq_reg, exit_bb, body_bb);

   vcode_select_block(body_bb);

   vcode_reg_t dst_ptr = emit_array_ref(mem_reg, i_reg);

   vcode_reg_t src0_reg = emit_load_indirect(emit_array_ref(data0_reg, i_reg));
   vcode_reg_t src1_reg = VCODE_INVALID_REG;
   if (kind != S_ARRAY_NOT)
      src1_reg = emit_load_indirect(emit_array_ref(data1_reg, i_reg));

   vcode_reg_t op_reg;
   switch (kind) {
   case S_ARRAY_NOT:  op_reg = emit_not(src0_reg); break;
   case S_ARRAY_AND:  op_reg = emit_and(src0_reg, src1_reg); break;
   case S_ARRAY_OR:   op_reg = emit_or(src0_reg, src1_reg); break;
   case S_ARRAY_XOR:  op_reg = emit_xor(src0_reg, src1_reg); break;
   case S_ARRAY_XNOR: op_reg = emit_xnor(src0_reg, src1_reg); break;
   case S_ARRAY_NAND: op_reg = emit_nand(src0_reg, src1_reg); break;
   case S_ARRAY_NOR:  op_reg = emit_nor(src0_reg, src1_reg); break;
   default:
      fatal_trace("unhandled bitvec operator kind %d", kind);
   }

   emit_store_indirect(op_reg, dst_ptr);

   vcode_reg_t next_reg = emit_add(i_reg, emit_const(vtype_offset(), 1));
   emit_store(next_reg, i_var);
   emit_jump(cmp_bb);

   vcode_select_block(exit_bb);

   vcode_reg_t left_reg  = emit_uarray_left(r0, 0);
   vcode_reg_t right_reg = emit_uarray_right(r0, 0);
   vcode_reg_t dir_reg   = emit_uarray_dir(r0, 0);

   vcode_dim_t dims[] = { { left_reg, right_reg, dir_reg } };
   emit_return(emit_wrap(mem_reg, dims, 1));
}

static void lower_predef_mixed_bit_vec_op(lower_unit_t *lu, tree_t decl,
                                          subprogram_kind_t kind)
{
   // Mixed scalar/array bit vector operations

   vcode_reg_t r0 = 1, r1 = 2;

   type_t r0_type = tree_type(tree_port(decl, 0));
   type_t r1_type = tree_type(tree_port(decl, 1));

   vcode_type_t voffset = vtype_offset();

   vcode_var_t i_var = lower_temp_var(lu, "i", voffset, voffset);
   emit_store(emit_const(vtype_offset(), 0), i_var);

   const bool r0_is_array = type_is_array(r0_type);

   type_t array_type = r0_is_array ? r0_type : r1_type;
   vcode_reg_t array_reg = r0_is_array ? r0 : r1;

   vcode_reg_t len_reg   = lower_array_len(lu, array_type, 0, array_reg);
   vcode_reg_t data_reg  = lower_array_data(array_reg);
   vcode_reg_t left_reg  = lower_array_left(lu, array_type, 0, array_reg);
   vcode_reg_t right_reg = lower_array_right(lu, array_type, 0, array_reg);
   vcode_reg_t dir_reg   = lower_array_dir(lu, array_type, 0, array_reg);
   vcode_reg_t null_reg  = emit_range_null(left_reg, right_reg, dir_reg);

   vcode_reg_t mem_reg = emit_alloc(vtype_bool(), vtype_bool(), len_reg);

   vcode_block_t body_bb = emit_block();
   vcode_block_t exit_bb = emit_block();

   emit_cond(null_reg, exit_bb, body_bb);

   vcode_select_block(body_bb);

   vcode_reg_t i_reg = emit_load(i_var);
   vcode_reg_t l_reg = emit_load_indirect(emit_array_ref(data_reg, i_reg));
   vcode_reg_t r_reg = r0_is_array ? r1 : r0;

   vcode_reg_t result_reg = VCODE_INVALID_REG;
   switch (kind) {
   case S_MIXED_AND:  result_reg = emit_and(l_reg, r_reg);  break;
   case S_MIXED_OR:   result_reg = emit_or(l_reg, r_reg);   break;
   case S_MIXED_NAND: result_reg = emit_nand(l_reg, r_reg); break;
   case S_MIXED_NOR:  result_reg = emit_nor(l_reg, r_reg);  break;
   case S_MIXED_XOR:  result_reg = emit_xor(l_reg, r_reg);  break;
   case S_MIXED_XNOR: result_reg = emit_xnor(l_reg, r_reg); break;
   default: break;
   }

   emit_store_indirect(result_reg, emit_array_ref(mem_reg, i_reg));

   vcode_reg_t next_reg = emit_add(i_reg, emit_const(voffset, 1));
   vcode_reg_t cmp_reg  = emit_cmp(VCODE_CMP_EQ, next_reg, len_reg);
   emit_store(next_reg, i_var);
   emit_cond(cmp_reg, exit_bb, body_bb);

   vcode_select_block(exit_bb);

   vcode_dim_t dims[1] = {
      {
         .left  = left_reg,
         .right = right_reg,
         .dir   = dir_reg
      }
   };
   emit_return(emit_wrap(mem_reg, dims, 1));
}

static void lower_predef_reduction_op(lower_unit_t *lu, tree_t decl,
                                      subprogram_kind_t kind)
{
   vcode_reg_t r0 = 1;
   type_t r0_type = tree_type(tree_port(decl, 0));

   vcode_type_t vbool = vtype_bool();
   vcode_type_t voffset = vtype_offset();

   vcode_var_t result_var = lower_temp_var(lu, "result", vbool, vbool);
   vcode_reg_t init_reg =
      emit_const(vbool, kind == S_REDUCE_NAND || kind == S_REDUCE_AND);
   emit_store(init_reg, result_var);

   vcode_var_t i_var = lower_temp_var(lu, "i", voffset, voffset);
   emit_store(emit_const(vtype_offset(), 0), i_var);

   vcode_reg_t len_reg   = lower_array_len(lu, r0_type, 0, r0);
   vcode_reg_t data_reg  = lower_array_data(r0);
   vcode_reg_t left_reg  = lower_array_left(lu, r0_type, 0, r0);
   vcode_reg_t right_reg = lower_array_right(lu, r0_type, 0, r0);
   vcode_reg_t dir_reg   = lower_array_dir(lu, r0_type, 0, r0);
   vcode_reg_t null_reg  = emit_range_null(left_reg, right_reg, dir_reg);

   vcode_block_t body_bb = emit_block();
   vcode_block_t exit_bb = emit_block();

   emit_cond(null_reg, exit_bb, body_bb);

   vcode_select_block(body_bb);

   vcode_reg_t i_reg   = emit_load(i_var);
   vcode_reg_t src_reg = emit_load_indirect(emit_array_ref(data_reg, i_reg));
   vcode_reg_t cur_reg = emit_load(result_var);

   vcode_reg_t result_reg = VCODE_INVALID_REG;
   switch (kind) {
   case S_REDUCE_OR:
   case S_REDUCE_NOR:
      result_reg = emit_or(cur_reg, src_reg);
      break;
   case S_REDUCE_AND:
   case S_REDUCE_NAND:
      result_reg = emit_and(cur_reg, src_reg);
      break;
   case S_REDUCE_XOR:
   case S_REDUCE_XNOR:
      result_reg = emit_xor(cur_reg, src_reg);
      break;
   default:
      break;
   }

   emit_store(result_reg, result_var);

   vcode_reg_t next_reg = emit_add(i_reg, emit_const(vtype_offset(), 1));
   vcode_reg_t cmp_reg  = emit_cmp(VCODE_CMP_EQ, next_reg, len_reg);
   emit_store(next_reg, i_var);
   emit_cond(cmp_reg, exit_bb, body_bb);

   vcode_select_block(exit_bb);

   if (kind == S_REDUCE_NOR || kind == S_REDUCE_NAND || kind == S_REDUCE_XNOR)
      emit_return(emit_not(emit_load(result_var)));
   else
      emit_return(emit_load(result_var));
}

static void lower_predef_match_op(lower_unit_t *lu, tree_t decl,
                                  subprogram_kind_t kind)
{
   vcode_reg_t r0 = 1, r1 = 2;

   type_t r0_type = tree_type(tree_port(decl, 0));
   type_t r1_type = tree_type(tree_port(decl, 1));

   vcode_cmp_t cmp;
   bool invert = false;
   switch (kind) {
   case S_MATCH_NEQ:
      invert = true;
   case S_MATCH_EQ:
      cmp = VCODE_CMP_EQ;
      break;
   case S_MATCH_GE:
      invert = true;
   case S_MATCH_LT:
      cmp = VCODE_CMP_LT;
      break;
   case S_MATCH_GT:
      invert = true;
   case S_MATCH_LE:
      cmp = VCODE_CMP_LEQ;
      break;
   default:
      fatal_trace("invalid match operator %d", kind);
   }

   bool is_array = false, is_bit = false;
   if (type_is_array(r0_type)) {
      is_array = true;
      is_bit = type_ident(type_elem(r0_type)) == well_known(W_STD_BIT);
   }
   else
      is_bit = type_ident(r0_type) == well_known(W_STD_BIT);

   vcode_reg_t result = VCODE_INVALID_REG;
   if (is_array) {
      assert(kind == S_MATCH_EQ || kind == S_MATCH_NEQ);

      vcode_reg_t len0_reg = lower_array_len(lu, r0_type, 0, r0);
      vcode_reg_t len1_reg = lower_array_len(lu, r1_type, 0, r1);

      vcode_block_t fail_bb = emit_block();
      vcode_block_t cont_bb = emit_block();

      vcode_reg_t len_eq = emit_cmp(VCODE_CMP_EQ, len0_reg, len1_reg);
      emit_cond(len_eq, cont_bb, fail_bb);

      vcode_select_block(fail_bb);

      vcode_type_t vseverity = vtype_int(0, SEVERITY_FAILURE - 1);
      vcode_reg_t failure_reg = emit_const(vseverity, SEVERITY_FAILURE);

      vcode_reg_t msg_reg =
         lower_wrap_string("arguments have different lengths");
      vcode_reg_t msg_len = emit_uarray_len(msg_reg, 0);

      vcode_reg_t locus = lower_debug_locus(decl);
      emit_report(emit_unwrap(msg_reg), msg_len, failure_reg, locus);
      emit_jump(cont_bb);

      vcode_select_block(cont_bb);

      vcode_type_t vtype = lower_type(type_elem(r0_type));
      vcode_type_t vbounds = lower_bounds(type_elem(r0_type));
      vcode_reg_t mem_reg = emit_alloc(vtype, vbounds, len0_reg);

      vcode_var_t result_var = lower_temp_var(lu, "result", vtype, vbounds);
      emit_store(emit_const(vtype, 0), result_var);

      vcode_type_t voffset = vtype_offset();
      vcode_var_t i_var = lower_temp_var(lu, "i", voffset, voffset);
      emit_store(emit_const(vtype_offset(), 0), i_var);

      vcode_reg_t left_reg  = lower_array_left(lu, r0_type, 0, r0);
      vcode_reg_t right_reg = lower_array_right(lu, r0_type, 0, r0);
      vcode_reg_t dir_reg   = lower_array_dir(lu, r0_type, 0, r0);
      vcode_reg_t null_reg  = emit_range_null(left_reg, right_reg, dir_reg);

      vcode_reg_t r0_ptr = lower_array_data(r0);
      vcode_reg_t r1_ptr = lower_array_data(r1);

      vcode_block_t body_bb = emit_block();
      vcode_block_t exit_bb = emit_block();

      emit_cond(null_reg, exit_bb, body_bb);

      vcode_select_block(body_bb);

      vcode_reg_t i_reg = emit_load(i_var);

      vcode_reg_t i0_ptr = emit_array_ref(r0_ptr, i_reg);
      vcode_reg_t i1_ptr = emit_array_ref(r1_ptr, i_reg);

      vcode_reg_t r0_src_reg = emit_load_indirect(i0_ptr);
      vcode_reg_t r1_src_reg = emit_load_indirect(i1_ptr);

      vcode_reg_t tmp;
      if (is_bit)
         tmp = emit_cmp(cmp, r0_src_reg, r1_src_reg);
      else {
         ident_t func = ident_new("NVC.IEEE_SUPPORT.REL_MATCH_EQ(UU)U");
         vcode_reg_t context_reg = lower_context_for_call(lu, func);
         vcode_reg_t args[] = { context_reg, r0_src_reg, r1_src_reg };
         tmp = emit_fcall(func, vtype, vbounds, VCODE_CC_PREDEF, args, 3);
      }
      emit_store_indirect(tmp, emit_array_ref(mem_reg, i_reg));

      vcode_reg_t next_reg = emit_add(i_reg, emit_const(vtype_offset(), 1));
      vcode_reg_t cmp_reg  = emit_cmp(VCODE_CMP_EQ, next_reg, len0_reg);
      emit_store(next_reg, i_var);
      emit_cond(cmp_reg, exit_bb, body_bb);

      vcode_select_block(exit_bb);

      vcode_dim_t dims[1] = {
         {
            .left  = left_reg,
            .right = right_reg,
            .dir   = dir_reg
         }
      };
      vcode_reg_t wrap_reg = emit_wrap(mem_reg, dims, 1);

      ident_t func = is_bit
         ? ident_new("STD.STANDARD.\"and\"(Q)J")
         : ident_new("IEEE.STD_LOGIC_1164.\"and\"(Y)U");
      vcode_reg_t context_reg = lower_context_for_call(lu, func);
      vcode_reg_t args[] = { context_reg, wrap_reg };
      result = emit_fcall(func, vtype, vbounds, VCODE_CC_PREDEF, args, 2);
   }
   else if (is_bit)
      result = emit_cmp(cmp, r0, r1);
   else {
      ident_t func = NULL;
      switch (cmp) {
      case VCODE_CMP_LT:
         func = ident_new("NVC.IEEE_SUPPORT.REL_MATCH_LT(UU)U");
         break;
      case VCODE_CMP_LEQ:
         func = ident_new("NVC.IEEE_SUPPORT.REL_MATCH_LEQ(UU)U");
         break;
      case VCODE_CMP_EQ:
         func = ident_new("NVC.IEEE_SUPPORT.REL_MATCH_EQ(UU)U");
         break;
      default:
         fatal_trace("unexpected comparison operator %d", cmp);
      }

      vcode_reg_t context_reg = lower_context_for_call(lu, func);
      vcode_reg_t args[3] = { context_reg, r0, r1 };

      vcode_type_t rtype = lower_type(r0_type);
      result = emit_fcall(func, rtype, rtype, VCODE_CC_PREDEF, args, 3);
   }

   if (invert && is_bit)
      emit_return(emit_not(result));
   else if (invert) {
      ident_t func = ident_new(
         "IEEE.STD_LOGIC_1164.\"not\"(U)24IEEE.STD_LOGIC_1164.UX01");
      vcode_reg_t context_reg = lower_context_for_call(lu, func);
      vcode_reg_t args[2] = { context_reg, result };
      vcode_type_t rtype = vcode_reg_type(result);
      emit_return(emit_fcall(func, rtype, rtype, VCODE_CC_PREDEF, args, 2));
   }
   else
      emit_return(result);
}

static void lower_predef_min_max(lower_unit_t *lu, tree_t decl, vcode_cmp_t cmp)
{
   type_t type = tree_type(tree_port(decl, 0));

   if (type_is_array(type) && tree_ports(decl) == 1) {
      type_t elem = type_elem(type);
      assert(type_is_scalar(elem));

      vcode_reg_t array_reg = 1;
      vcode_type_t voffset = vtype_offset();

      vcode_var_t i_var = lower_temp_var(lu, "i", voffset, voffset);
      emit_store(emit_const(voffset, 0), i_var);

      vcode_type_t elem_vtype = lower_type(elem);
      vcode_var_t result_var =
         lower_temp_var(lu, "result", elem_vtype, elem_vtype);

      tree_t elem_r = range_of(elem, 0);
      vcode_reg_t def_reg =
         (cmp == VCODE_CMP_GT && tree_subkind(elem_r) == RANGE_TO)
         || (cmp == VCODE_CMP_LT && tree_subkind(elem_r) == RANGE_DOWNTO)
         ? lower_range_left(lu, elem_r)
         : lower_range_right(lu, elem_r);

      emit_store(def_reg, result_var);

      vcode_reg_t left_reg  = lower_array_left(lu, type, 0, array_reg);
      vcode_reg_t right_reg = lower_array_right(lu, type, 0, array_reg);
      vcode_reg_t len_reg   = lower_array_len(lu, type, 0, array_reg);
      vcode_reg_t kind_reg  = lower_array_dir(lu, type, 0, array_reg);
      vcode_reg_t data_reg  = lower_array_data(array_reg);
      vcode_reg_t null_reg  = emit_range_null(left_reg, right_reg, kind_reg);

      vcode_block_t body_bb = emit_block();
      vcode_block_t exit_bb = emit_block();

      emit_cond(null_reg, exit_bb, body_bb);

      vcode_select_block(body_bb);

      vcode_reg_t i_reg    = emit_load(i_var);
      vcode_reg_t elem_ptr = emit_array_ref(data_reg, i_reg);
      vcode_reg_t elem_reg = emit_load_indirect(elem_ptr);
      vcode_reg_t cur_reg  = emit_load(result_var);
      vcode_reg_t cmp_reg  = emit_cmp(cmp, elem_reg, cur_reg);
      vcode_reg_t next_reg = emit_select(cmp_reg, elem_reg, cur_reg);
      emit_store(next_reg, result_var);

      vcode_reg_t i_next = emit_add(i_reg, emit_const(voffset, 1));
      emit_store(i_next, i_var);

      vcode_reg_t done_reg = emit_cmp(VCODE_CMP_EQ, i_next, len_reg);
      emit_cond(done_reg, exit_bb, body_bb);

      vcode_select_block(exit_bb);
      emit_return(emit_load(result_var));
   }
   else {
      vcode_reg_t context_reg = 0, r0 = 1, r1 = 2;

      vcode_reg_t test_reg;
      if (type_is_scalar(type))
         test_reg = emit_cmp(cmp, r0, r1);
      else {
         const char *op = cmp == VCODE_CMP_GT ? ">" : "<";
         ident_t func = lower_predef_func_name(type, op);
         vcode_reg_t args[] = { context_reg, r0, r1 };
         vcode_type_t vbool = vtype_bool();
         test_reg = emit_fcall(func, vbool, vbool, VCODE_CC_PREDEF, args, 3);
      }

      emit_return(emit_select(test_reg, r0, r1));
   }
}

static void lower_predef_negate(tree_t decl, const char *op)
{
   type_t type = tree_type(tree_port(decl, 0));
   vcode_type_t vbool = vtype_bool();
   vcode_reg_t args[] = { 0, 1, 2 };
   vcode_reg_t eq_reg = emit_fcall(lower_predef_func_name(type, op),
                                   vbool, vbool, VCODE_CC_PREDEF, args, 3);

   emit_return(emit_not(eq_reg));
}

static void lower_predef(lower_unit_t *lu, object_t *obj)
{
   tree_t decl = tree_from_object(obj);

   const subprogram_kind_t kind = tree_subkind(decl);
   assert(kind != S_USER && kind != S_FOREIGN);
   assert(!is_open_coded_builtin(kind));

   type_t type = tree_type(decl);
   vcode_set_result(lower_func_result_type(type_result(type)));

   vcode_type_t vcontext = vtype_context(lu->parent->name);
   emit_param(vcontext, vcontext, ident_new("context"));

   lower_subprogram_ports(lu, decl, false);

   switch (tree_subkind(decl)) {
   case S_ARRAY_EQ:
      lower_predef_array_cmp(lu, decl, VCODE_CMP_EQ);
      break;
   case S_ARRAY_LE:
      lower_predef_array_cmp(lu, decl, VCODE_CMP_LEQ);
      break;
   case S_ARRAY_LT:
      lower_predef_array_cmp(lu, decl, VCODE_CMP_LT);
      break;
   case S_ARRAY_GE:
      lower_predef_negate(decl, "<");
      break;
   case S_ARRAY_GT:
      lower_predef_negate(decl, "<=");
      break;
   case S_RECORD_EQ:
      lower_predef_record_eq(lu, decl);
      break;
   case S_ARRAY_NEQ:
   case S_RECORD_NEQ:
      lower_predef_negate(decl, "=");
      break;
   case S_TO_STRING:
      lower_predef_to_string(lu, decl);
      break;
   case S_SLL:
   case S_SRL:
   case S_SLA:
   case S_SRA:
   case S_ROL:
   case S_ROR:
      lower_predef_bit_shift(lu, decl, kind);
      break;
   case S_ARRAY_NOT:
   case S_ARRAY_AND:
   case S_ARRAY_OR:
   case S_ARRAY_XOR:
   case S_ARRAY_XNOR:
   case S_ARRAY_NAND:
   case S_ARRAY_NOR:
      lower_predef_bit_vec_op(lu, decl, kind);
      break;
   case S_MIXED_AND:
   case S_MIXED_OR:
   case S_MIXED_XOR:
   case S_MIXED_XNOR:
   case S_MIXED_NAND:
   case S_MIXED_NOR:
      lower_predef_mixed_bit_vec_op(lu, decl, kind);
      break;
   case S_REDUCE_OR:
   case S_REDUCE_AND:
   case S_REDUCE_NAND:
   case S_REDUCE_NOR:
   case S_REDUCE_XOR:
   case S_REDUCE_XNOR:
      lower_predef_reduction_op(lu, decl, kind);
      break;
   case S_MATCH_EQ:
   case S_MATCH_NEQ:
   case S_MATCH_LT:
   case S_MATCH_LE:
   case S_MATCH_GT:
   case S_MATCH_GE:
      lower_predef_match_op(lu, decl, kind);
      break;
   case S_MAXIMUM:
      lower_predef_min_max(lu, decl, VCODE_CMP_GT);
      break;
   case S_MINIMUM:
      lower_predef_min_max(lu, decl, VCODE_CMP_LT);
      break;
   default:
      break;
   }
}

static void lower_proc_body(lower_unit_t *lu, object_t *obj)
{
   tree_t body = tree_from_object(obj);
   assert(!is_uninstantiated_subprogram(body));

   ident_t context_id = vcode_unit_name();

   cover_push_scope(lu->cover, body);

   vcode_type_t vcontext = vtype_context(context_id);
   emit_param(vcontext, vcontext, ident_new("context"));

   if (tree_kind(body) == T_PROC_INST)
      lower_generics(lu, body);

   const bool never_waits = vcode_unit_kind() == VCODE_UNIT_FUNCTION;
   const bool has_subprograms = lower_has_subprograms(body);
   lower_subprogram_ports(lu, body, has_subprograms || !never_waits);

   lower_decls(lu, body);

   lower_sequence(lu, body, NULL);

   if (!vcode_block_finished()) {
      lower_leave_subprogram(lu);
      emit_return(VCODE_INVALID_REG);
   }

   cover_pop_scope(lu->cover);
}

static void lower_func_body(lower_unit_t *lu, object_t *obj)
{
   tree_t body = tree_from_object(obj);
   assert(!is_uninstantiated_subprogram(body));

   type_t result = type_result(tree_type(body));
   vcode_set_result(lower_func_result_type(result));

   vcode_type_t vcontext = vtype_context(lu->parent->name);
   emit_param(vcontext, vcontext, ident_new("context"));

   cover_push_scope(lu->cover, body);

   if (tree_kind(body) == T_FUNC_INST)
      lower_generics(lu, body);

   const bool has_subprograms = lower_has_subprograms(body);
   lower_subprogram_ports(lu, body, has_subprograms);

   if (tree_flags(body) & TREE_F_KNOWS_SUBTYPE) {
      // Extra hidden parameter for result bounds
      vcode_type_t vresult = lower_param_type(result, C_CONSTANT, PORT_IN);
      vcode_reg_t bounds_reg =
         emit_param(vresult, vresult, ident_new("result"));

      lower_put_vcode_obj(body, bounds_reg, lu);
   }

   lower_decls(lu, body);

   lower_sequence(lu, body, NULL);

   if (!vcode_block_finished())
      emit_unreachable(lower_debug_locus(body));

   cover_pop_scope(lu->cover);
}

static void lower_driver_field_cb(lower_unit_t *lu, tree_t field,
                                  vcode_reg_t ptr, vcode_reg_t unused,
                                  vcode_reg_t locus, void *__ctx)
{
   type_t type = tree_type(field);
   if (type_is_homogeneous(type)) {
      vcode_reg_t nets_reg = emit_load_indirect(ptr);
      vcode_reg_t count_reg = lower_type_width(lu, type, nets_reg);
      emit_drive_signal(lower_array_data(nets_reg), count_reg);
   }
   else
      lower_for_each_field(lu, type, ptr, VCODE_INVALID_REG,
                           VCODE_INVALID_REG, lower_driver_field_cb, NULL);
}

void lower_process(lower_unit_t *parent, tree_t proc, driver_set_t *ds)
{
   assert(tree_kind(proc) == T_PROCESS);

   vcode_select_unit(parent->vunit);
   ident_t label = tree_ident(proc);
   ident_t name = ident_prefix(vcode_unit_name(), label, '.');
   vcode_unit_t vu = emit_process(name, tree_to_object(proc), parent->vunit);

   // The code generator assumes the first state starts at block number
   // one. Allocate it here in case lowering the declarations generates
   // additional basic blocks.
   vcode_block_t start_bb = emit_block();
   assert(start_bb == 1);

   lower_unit_t *lu = lower_unit_new(parent->registry, parent, vu,
                                     parent->cover, proc);
   unit_registry_put(parent->registry, lu);

   cover_push_scope(lu->cover, proc);

   lower_decls(lu, proc);

   driver_info_t *di = get_drivers(ds, proc);
   for (; di; di = di->chain_proc) {
      assert(!di->tentative);
      type_t prefix_type = tree_type(di->prefix);
      vcode_reg_t nets_reg = lower_lvalue(lu, di->prefix);

      if (!type_is_homogeneous(prefix_type))
         lower_for_each_field(lu, prefix_type, nets_reg, VCODE_INVALID_REG,
                              VCODE_INVALID_REG, lower_driver_field_cb, NULL);
      else {
         vcode_reg_t count_reg = lower_type_width(lu, prefix_type, nets_reg);
         emit_drive_signal(lower_array_data(nets_reg), count_reg);
      }
   }

   // If the last statement in the process is a static wait then this
   // process is always sensitive to the same set of signals and we can
   // emit a single _sched_event call in the reset block
   tree_t wait = NULL;
   const int nstmts = tree_stmts(proc);
   if (nstmts > 0
       && tree_kind((wait = tree_stmt(proc, nstmts - 1))) == T_WAIT
       && (tree_flags(wait) & TREE_F_STATIC_WAIT)) {

      const int ntriggers = tree_triggers(wait);
      for (int i = 0; i < ntriggers; i++)
         lower_sched_event(lu, tree_trigger(wait, i), VCODE_INVALID_REG);
   }

   emit_return(VCODE_INVALID_REG);

   vcode_select_block(start_bb);

   lower_sequence(lu, proc, NULL);

   if (!vcode_block_finished())
      emit_jump(start_bb);

   cover_pop_scope(lu->cover);

   lower_finished(lu);
   unit_registry_finalise(parent->registry, lu);
}

static bool lower_is_signal_ref(tree_t expr)
{
   switch (tree_kind(expr)) {
   case T_REF:
      return class_of(tree_ref(expr)) == C_SIGNAL;

   case T_ALIAS:
   case T_ARRAY_SLICE:
   case T_ARRAY_REF:
   case T_RECORD_REF:
   case T_QUALIFIED:
   case T_TYPE_CONV:
      return lower_is_signal_ref(tree_value(expr));

   default:
      return false;
   }
}

static ident_t lower_converter(lower_unit_t *parent, tree_t expr,
                               type_t atype, type_t rtype, type_t check_type,
                               vcode_type_t *vatype, vcode_type_t *vrtype)
{
   const tree_kind_t kind = tree_kind(expr);
   tree_t fdecl = kind == T_CONV_FUNC ? tree_ref(expr) : NULL;
   bool p0_uarray = false, r_uarray = false;

   // Detect some trivial cases and avoid generating a conversion function
   if (kind == T_TYPE_CONV && type_is_array(atype) && type_is_array(rtype)) {
      if (type_eq(type_elem(atype), type_elem(rtype)))
         return NULL;
   }
   else if (kind == T_TYPE_CONV && type_is_enum(atype) && type_is_enum(rtype))
      return NULL;
   else if (kind == T_CONV_FUNC) {
      type_t p0_type = tree_type(tree_port(fdecl, 0));
      p0_uarray = type_is_array(p0_type) && !lower_const_bounds(p0_type);
      r_uarray = type_is_array(rtype) && !lower_const_bounds(rtype);

      if (!p0_uarray && !r_uarray) {
         *vatype = lower_param_type(atype, C_CONSTANT, PORT_IN);
         *vrtype = lower_func_result_type(rtype);
         return tree_ident2(fdecl);
      }
   }

   vcode_unit_t context = vcode_active_unit();
   ident_t context_id = vcode_unit_name();

   LOCAL_TEXT_BUF tb = tb_new();
   tb_printf(tb, "%s.", istr(context_id));
   if (kind == T_TYPE_CONV)
      tb_printf(tb, "convert_%s_%s", type_pp(atype), type_pp(rtype));
   else {
      tree_t p0 = tree_value(expr);
      ident_t signame = tree_ident(name_to_ref(p0));
      tb_printf(tb, "wrap_%s.%s", istr(tree_ident2(fdecl)), istr(signame));
   }

   ident_t name = ident_new(tb_get(tb));
   if (unit_registry_query(parent->registry, name))
      return name;

   vcode_state_t state;
   vcode_state_save(&state);

   vcode_type_t vabounds, vrbounds;
   if (kind == T_TYPE_CONV) {
      *vatype = lower_type(atype);
      *vrtype = lower_type(rtype);
      vabounds = lower_bounds(atype);
      vrbounds = lower_bounds(rtype);
   }
   else {
      if (p0_uarray) {
         type_t elem = lower_elem_recur(atype);
         *vatype = vtype_pointer(lower_type(elem));
         vabounds = lower_bounds(elem);
      }
      else {
         *vatype = lower_type(atype);
         vabounds = lower_bounds(atype);
      }

      if (r_uarray) {
         type_t elem = lower_elem_recur(rtype);
         *vrtype = vtype_pointer(lower_type(elem));
         vrbounds = lower_bounds(elem);
      }
      else {
         *vrtype = lower_func_result_type(rtype);
         vrbounds = lower_bounds(rtype);
      }
   }

   vcode_unit_t vu = emit_function(name, tree_to_object(expr), context);
   vcode_set_result(*vrtype);
   emit_debug_info(tree_loc(expr));

   lower_unit_t *lu = lower_unit_new(parent->registry, parent, vu, NULL, NULL);
   unit_registry_put(parent->registry, lu);

   vcode_type_t vcontext = vtype_context(context_id);
   emit_param(vcontext, vcontext, ident_new("context"));

   vcode_reg_t p0 = emit_param(*vatype, vabounds, ident_new("p0"));

   if (kind == T_TYPE_CONV)
      emit_return(lower_conversion(lu, p0, expr, atype, rtype));
   else {
      vcode_reg_t arg_reg = p0;
      if (p0_uarray)
         arg_reg = lower_wrap(lu, atype, p0);

      ident_t func = tree_ident2(fdecl);
      vcode_reg_t context_reg = lower_context_for_call(lu, func);
      vcode_reg_t args[] = { context_reg, arg_reg };
      vcode_reg_t result_reg = emit_fcall(func, lower_type(rtype), vrbounds,
                                          VCODE_CC_VHDL, args, 2);

      if (r_uarray) {
         vcode_reg_t locus = lower_debug_locus(expr);
         lower_check_array_sizes(lu, check_type, rtype, VCODE_INVALID_REG,
                                 result_reg, locus);
         result_reg = emit_unwrap(result_reg);
      }

      emit_return(result_reg);
   }

   unit_registry_finalise(parent->registry, lu);

   vcode_state_restore(&state);

   return name;
}

static void lower_map_view_field_cb(lower_unit_t *lu, tree_t field,
                                    vcode_reg_t src_ptr, vcode_reg_t dst_ptr,
                                    vcode_reg_t locus, void *__ctx)
{
   tree_t view = untag_pointer(__ctx, void);
   bool converse = !!pointer_tag(__ctx);

   tree_t elem = find_element_mode_indication(view, field, &converse);
   assert(elem != NULL);

   type_t ftype = tree_type(field);
   if (type_is_homogeneous(ftype)) {
      vcode_reg_t src_reg = emit_load_indirect(src_ptr);
      vcode_reg_t dst_reg = emit_load_indirect(dst_ptr);

      if (type_is_array(ftype))
         lower_check_array_sizes(lu, ftype, ftype, src_reg, dst_reg, locus);

      vcode_reg_t count_reg = lower_type_width(lu, ftype, src_reg);

      vcode_reg_t src_nets = lower_array_data(src_reg);
      vcode_reg_t dst_nets = lower_array_data(dst_reg);

      vcode_reg_t conv_reg = VCODE_INVALID_REG;   // Not valid for interfaces

      switch (converse_mode(elem, converse)) {
      case PORT_IN:
         emit_map_signal(dst_nets, src_nets, count_reg, count_reg, conv_reg);
         break;
      case PORT_OUT:
      case PORT_INOUT:
      case PORT_BUFFER:
         emit_map_signal(src_nets, dst_nets, count_reg, count_reg, conv_reg);
         break;
      default:
         fatal_trace("unhandled port mode in lower_map_view_field_cb");
      }
   }
   else {
      assert(tree_subkind(elem) == PORT_RECORD_VIEW);

      void *new_ctx = tag_pointer(tree_value(elem), converse);
      lower_for_each_field(lu, ftype, src_ptr, dst_ptr, locus,
                           lower_map_view_field_cb, new_ctx);
   }
}

static void lower_map_signal_field_cb(lower_unit_t *lu, tree_t field,
                                      vcode_reg_t src_ptr, vcode_reg_t dst_ptr,
                                      vcode_reg_t locus, void *__ctx)
{
   type_t ftype = tree_type(field);
   map_signal_param_t *args = __ctx;

   if (type_is_homogeneous(ftype)) {
      vcode_reg_t dst_reg, dst_count = VCODE_INVALID_REG;
      if (!args->reverse && args->conv_func != VCODE_INVALID_REG) {
         dst_reg = args->conv_reg;
         dst_count = args->conv_count;
      }
      else if (args->reverse)
         dst_reg = emit_load_indirect(src_ptr);
      else
         dst_reg = emit_load_indirect(dst_ptr);

      vcode_reg_t src_reg, src_count = VCODE_INVALID_REG;
      if (args->reverse && args->conv_func != VCODE_INVALID_REG) {
         src_reg = args->conv_reg;
         src_count = args->conv_count;
      }
      else if (args->reverse && !args->is_const)
         src_reg = emit_load_indirect(dst_ptr);
      else if (!args->is_const)
         src_reg = emit_load_indirect(src_ptr);
      else if (have_uarray_ptr(src_ptr))
         src_reg = emit_load_indirect(src_ptr);
      else
         src_reg = src_ptr;

      if (type_is_array(ftype) && args->conv_func == VCODE_INVALID_REG)
         lower_check_array_sizes(lu, ftype, ftype, src_reg, dst_reg, locus);

      if (src_count == VCODE_INVALID_REG)
         src_count = lower_type_width(lu, ftype, src_reg);

      if (dst_count == VCODE_INVALID_REG)
         dst_count = lower_type_width(lu, ftype, dst_reg);

      src_reg = lower_array_data(src_reg);
      dst_reg = lower_array_data(dst_reg);

      if (args->is_const)
         emit_map_const(src_reg, dst_reg, src_count);
      else
         emit_map_signal(src_reg, dst_reg, src_count, dst_count,
                         args->conv_func);
   }
   else
      lower_for_each_field(lu, ftype, src_ptr, dst_ptr, locus,
                           lower_map_signal_field_cb, __ctx);
}

static void lower_map_signal(lower_unit_t *lu, vcode_reg_t src_reg,
                             vcode_reg_t dst_reg, type_t src_type,
                             type_t dst_type, vcode_reg_t conv_func,
                             tree_t where)
{
   if (!type_is_homogeneous(src_type)) {
      map_signal_param_t args = {
         .conv_func = conv_func,
         .conv_reg  = dst_reg,
         .is_const  = !lower_have_signal(src_reg),
      };

      if (conv_func != VCODE_INVALID_REG) {
         args.conv_count = lower_type_width(lu, dst_type, dst_reg);
         dst_reg = VCODE_INVALID_REG;
      }

      vcode_reg_t locus = lower_debug_locus(where);
      lower_for_each_field(lu, src_type, src_reg, dst_reg, locus,
                           lower_map_signal_field_cb, &args);
   }
   else if (!type_is_homogeneous(dst_type)) {
      map_signal_param_t args = {
         .conv_func = conv_func,
         .conv_reg  = src_reg,
         .reverse   = true,
         .is_const  = !lower_have_signal(src_reg),
      };

      if (conv_func != VCODE_INVALID_REG) {
         args.conv_count = lower_type_width(lu, src_type, src_reg);
         src_reg = VCODE_INVALID_REG;
      }

      vcode_reg_t locus = lower_debug_locus(where);
      lower_for_each_field(lu, dst_type, dst_reg, src_reg, locus,
                           lower_map_signal_field_cb, &args);
   }
   else {
      vcode_reg_t src_count = lower_type_width(lu, src_type, src_reg);
      vcode_reg_t dst_count = lower_type_width(lu, dst_type, dst_reg);

      vcode_reg_t dst_nets = lower_array_data(dst_reg);

      if (lower_have_signal(src_reg)) {
         vcode_reg_t src_nets = lower_array_data(src_reg);
         emit_map_signal(src_nets, dst_nets, src_count, dst_count, conv_func);
      }
      else if (type_is_array(src_type)) {
         vcode_reg_t src_data = lower_array_data(src_reg);
         emit_map_const(src_data, dst_nets, src_count);
      }
      else
         emit_map_const(src_reg, dst_nets, src_count);
   }
}

static void lower_non_static_actual(lower_unit_t *parent, tree_t port,
                                    type_t type, vcode_reg_t port_reg,
                                    tree_t wave)
{
   // Construct the equivalent process according the procedure in LRM 08
   // section 6.5.6.3

   assert(standard() >= STD_08);

   LOCAL_TEXT_BUF tb = tb_new();
   tb_istr(tb, tree_ident(port));
   tb_cat(tb, "_actual");

   ident_t name = ident_uniq(tb_get(tb));

   vcode_type_t signal_type = lower_signal_type(type);
   vcode_type_t vbounds = lower_bounds(type);
   vcode_var_t var = emit_var(signal_type, vbounds, name, VAR_SIGNAL);
   emit_store(port_reg, var);

   tree_t expr = tree_value(wave);

   vcode_state_t state;
   vcode_state_save(&state);

   ident_t pname = ident_prefix(vcode_unit_name(), name, '.');
   vcode_unit_t vu = emit_process(pname, tree_to_object(wave), parent->vunit);

   lower_unit_t *lu = lower_unit_new(parent->registry, parent, vu, NULL, NULL);
   unit_registry_put(parent->registry, lu);

   {
      vcode_reg_t nets_reg = emit_load_indirect(emit_var_upref(1, var));
      vcode_reg_t count_reg = lower_type_width(lu, type, nets_reg);
      vcode_reg_t data_reg = lower_array_data(nets_reg);

      emit_drive_signal(data_reg, count_reg);
   }

   wait_param_t param = { lu, VCODE_INVALID_REG };
   build_wait(expr, lower_build_wait_cb, &param);

   emit_return(VCODE_INVALID_REG);

   vcode_block_t main_bb = emit_block();
   vcode_select_block(main_bb);

   vcode_reg_t zero_time_reg = emit_const(vtype_time(), 0);
   vcode_reg_t value_reg = lower_rvalue(lu, expr);
   vcode_reg_t nets_reg = emit_load_indirect(emit_var_upref(1, var));

   target_part_t parts[] = {
      { .kind = PART_ALL,
        .reg = nets_reg,
        .off = VCODE_INVALID_REG,
        .target = port },
      { .kind = PART_POP,
        .reg = VCODE_INVALID_REG,
        .off = VCODE_INVALID_REG,
      }
   };

   target_part_t *ptr = parts;
   lower_signal_assign_target(lu, &ptr, port, value_reg, type,
                              zero_time_reg, zero_time_reg);

   emit_return(VCODE_INVALID_REG);

   unit_registry_finalise(parent->registry, lu);

   vcode_state_restore(&state);

   emit_process_init(pname, lower_debug_locus(wave));
}

static void lower_port_map(lower_unit_t *lu, tree_t block, tree_t map,
                           vcode_reg_t value_reg)
{
   vcode_reg_t port_reg = VCODE_INVALID_REG;
   tree_t port = NULL;
   type_t name_type = NULL;
   vcode_reg_t out_conv = VCODE_INVALID_REG;
   vcode_reg_t in_conv = VCODE_INVALID_REG;
   tree_t value = tree_value(map);

   tree_t value_conv = NULL;
   const tree_kind_t value_kind = tree_kind(value);
   if (value_kind == T_CONV_FUNC || value_kind == T_TYPE_CONV) {
      assert(value_reg == VCODE_INVALID_REG);
      tree_t p0 = tree_value(value);
      if (lower_is_signal_ref(p0)) {
         value_conv = p0;
         value_reg = lower_lvalue(lu, p0);
      }
      else
         value_reg = lower_rvalue(lu, value);
   }

   switch (tree_subkind(map)) {
   case P_POS:
      {
         port = tree_port(block, tree_pos(map));
         name_type = tree_type(port);

         int hops;
         vcode_var_t var = lower_get_var(lu, port, &hops);
         assert(hops == 0);

         if (type_is_homogeneous(name_type))
            port_reg = emit_load(var);
         else
            port_reg = emit_index(var, VCODE_INVALID_REG);
      }
      break;
   case P_NAMED:
      {
         tree_t name = tree_name(map);
         const tree_kind_t kind = tree_kind(name);
         if (kind == T_CONV_FUNC) {
            tree_t p0 = tree_value(name);
            type_t atype = tree_type(p0);
            type_t rtype = tree_type(name);
            vcode_type_t vatype = VCODE_INVALID_TYPE;
            vcode_type_t vrtype = VCODE_INVALID_TYPE;
            ident_t func = lower_converter(lu, name, atype, rtype,
                                           tree_type(value_conv ?: value),
                                           &vatype, &vrtype);
            vcode_reg_t context_reg = lower_context_for_call(lu, func);
            out_conv = emit_closure(func, context_reg, vatype, vrtype);
            name = p0;
         }
         else if (kind == T_TYPE_CONV) {
            tree_t value = tree_value(name);
            type_t rtype = tree_type(name);
            type_t atype = tree_type(value);
            vcode_type_t vatype = VCODE_INVALID_TYPE;
            vcode_type_t vrtype = VCODE_INVALID_TYPE;
            ident_t func = lower_converter(lu, name, atype, rtype,
                                           tree_type(value_conv ?: value),
                                           &vatype, &vrtype);
            if (func != NULL) {
               vcode_reg_t context_reg = lower_context_for_call(lu, func);
               out_conv = emit_closure(func, context_reg, vatype, vrtype);
            }
            name = value;
         }

         port_reg = lower_lvalue(lu, name);
         port = tree_ref(name_to_ref(name));
         name_type = tree_type(name);
      }
      break;
   }

   assert(tree_kind(port) == T_PORT_DECL);

   if (value_kind == T_OPEN && tree_has_value(port))
      value = tree_value(port);
   else if (value_conv != NULL) {
      // Value has conversion function
      type_t atype = tree_type(value_conv);
      type_t rtype = tree_type(value);
      vcode_type_t vatype = VCODE_INVALID_TYPE;
      vcode_type_t vrtype = VCODE_INVALID_TYPE;
      ident_t func = NULL;

      switch (value_kind) {
      case T_CONV_FUNC:
         func = lower_converter(lu, value, atype, rtype, name_type,
                                &vatype, &vrtype);
         break;
      case T_TYPE_CONV:
         func = lower_converter(lu, value, atype, rtype, name_type,
                                &vatype, &vrtype);
         break;
      default:
         assert(false);
      }

      if (func != NULL) {
         vcode_reg_t context_reg = lower_context_for_call(lu, func);
         in_conv = emit_closure(func, context_reg, vatype, vrtype);
      }
      value = value_conv;
   }

   const port_mode_t mode = tree_subkind(port);

   if (mode == PORT_ARRAY_VIEW || mode == PORT_RECORD_VIEW) {
      assert(lower_is_signal_ref(value));
      tree_t view = tree_value(port);
      vcode_reg_t locus = lower_debug_locus(view);
      lower_for_each_field(lu, name_type, port_reg, value_reg,
                           locus, lower_map_view_field_cb, view);
   }
   else if (lower_is_signal_ref(value)) {
      type_t value_type = tree_type(value);

      vcode_reg_t src_reg = mode == PORT_IN ? value_reg : port_reg;
      vcode_reg_t dst_reg = mode == PORT_IN ? port_reg : value_reg;
      vcode_reg_t conv_func = mode == PORT_IN ? in_conv : out_conv;

      type_t src_type = mode == PORT_IN ? value_type : name_type;
      type_t dst_type = mode == PORT_IN ? name_type : value_type;

      lower_map_signal(lu, src_reg, dst_reg, src_type, dst_type,
                       conv_func, port);
   }
   else if (tree_kind(value) == T_WAVEFORM)
      lower_non_static_actual(lu, port, name_type, port_reg, value);
   else if (value_reg != VCODE_INVALID_REG) {
      type_t value_type = tree_type(value);
      lower_map_signal(lu, value_reg, port_reg, value_type, name_type,
                       in_conv, port);
   }
}

static bool lower_direct_mapped_port(lower_unit_t *lu, driver_set_t *ds,
                                     tree_t block, tree_t map, hset_t *direct,
                                     hset_t **poison)
{
   tree_t port = NULL;
   int field = -1;
   switch (tree_subkind(map)) {
   case P_POS:
      port = tree_port(block, tree_pos(map));
      break;
   case P_NAMED:
      {
         tree_t name = tree_name(map);
         tree_kind_t kind = tree_kind(name);

         if (kind == T_RECORD_REF) {
            field = tree_pos(tree_ref(name));
            name  = tree_value(name);
            kind  = tree_kind(name);
         }

         if (kind == T_REF)
            port = tree_ref(name);
      }
      break;
   }

   if (port == NULL)
      return false;

   assert(tree_kind(port) == T_PORT_DECL);

   const class_t class = tree_class(port);
   if (class == C_SIGNAL) {
      switch (tree_subkind(port)) {
      case PORT_IN:
         break;   // Always safe
      case PORT_OUT:
         if (!has_unique_driver(ds, port))
            return false;
         break;
      default:
         return false;
      }
   }

   tree_t value = tree_value(map);

   if (class == C_VARIABLE) {
      // Variable ports are always directly aliased to the actual
      // variable in the parent scope
      vcode_type_t vtype = lower_type(tree_type(port));
      vcode_var_t var = emit_var(vtype, vtype, tree_ident(port), 0);
      lower_put_vcode_obj(port, var, lu);

      vcode_reg_t src_reg = lower_rvalue(lu, value);
      emit_store(src_reg, var);

      hset_insert(direct, map);
      hset_insert(direct, port);
      return true;
   }
   else if (!lower_is_signal_ref(value) || tree_kind(value) == T_TYPE_CONV) {
      if (field != -1) {
         // We can't use direct mapping for this record element so make
         // sure we don't direct map any other elements of this signal
         if (*poison == NULL)
            *poison = hset_new(32);
         hset_insert(*poison, port);
      }
      return false;
   }
   else if (*poison != NULL && hset_contains(*poison, port))
      return false;

   vcode_reg_t src_reg = lower_lvalue(lu, value);

   int hops = 0;
   vcode_var_t var = VCODE_INVALID_VAR;
   if (field != -1) var = lower_search_vcode_obj(port, lu, &hops);

   type_t type = tree_type(value);
   type_t port_type = tree_type(port);

   if (var == VCODE_INVALID_VAR || hops > 0) {
      vcode_type_t vtype = lower_signal_type(port_type);
      var = emit_var(vtype, vtype, tree_ident(port), VAR_SIGNAL);
      lower_put_vcode_obj(port, var, lu);
   }

   if (!type_is_homogeneous(type)) {
      vcode_reg_t ptr = emit_index(var, VCODE_INVALID_REG);
      if (field != -1)
         ptr = emit_record_ref(ptr, field);

      if (have_uarray_ptr(ptr)) {
         assert(have_uarray_ptr(src_reg));
         vcode_reg_t meta_reg = emit_load_indirect(src_reg);
         emit_store_indirect(meta_reg, ptr);
      }
      else {
         vcode_reg_t count_reg = VCODE_INVALID_REG;
         if (type_is_array(type)) {
            count_reg = lower_array_total_len(lu, type, src_reg);
            src_reg   = lower_array_data(src_reg);
         }

         emit_copy(ptr, src_reg, count_reg);
      }
   }
   else if (field == -1) {
      vcode_reg_t data_reg = lower_array_data(src_reg);
      emit_alias_signal(data_reg, lower_debug_locus(port));
      if (vtype_kind(vcode_var_type(var)) == VCODE_TYPE_UARRAY) {
         vcode_reg_t wrap_reg = lower_wrap(lu, port_type, data_reg);
         emit_store(wrap_reg, var);
      }
      else
         emit_store(data_reg, var);
   }
   else {
      vcode_reg_t data_reg = lower_array_data(src_reg);
      vcode_reg_t port_reg = emit_index(var, VCODE_INVALID_REG);
      vcode_reg_t field_reg = emit_record_ref(port_reg, field);
      if (have_uarray_ptr(field_reg)) {
         vcode_reg_t wrap_reg = lower_wrap(lu, type, data_reg);
         emit_store_indirect(wrap_reg, field_reg);
      }
      else
         emit_store_indirect(data_reg, field_reg);
   }

   hset_insert(direct, map);
   hset_insert(direct, port);
   return true;
}

static void lower_port_signal(lower_unit_t *lu, tree_t port,
                              vcode_reg_t bounds_reg)
{
   type_t type = tree_type(port);
   type_t value_type = type;

   const port_mode_t mode = tree_subkind(port);

   vcode_type_t vtype = lower_signal_type(type);
   vcode_var_t var = emit_var(vtype, vtype, tree_ident(port), VAR_SIGNAL);
   lower_put_vcode_obj(port, var, lu);

   tree_t view = NULL;
   vcode_reg_t init_reg = VCODE_INVALID_REG;
   if (mode == PORT_RECORD_VIEW || mode == PORT_ARRAY_VIEW) {
      // No explicit initial value
      view = tree_value(port);
   }
   else if (tree_has_value(port)) {
      tree_t value = tree_value(port);
      value_type = tree_type(value);
      init_reg = lower_rvalue(lu, value);
   }

   sig_flags_t flags = 0;
   if (tree_flags(port) & TREE_F_REGISTER)
      flags |= NET_F_REGISTER;

   // Port signals will need separate driving/effective values if they
   // are inout or have conversion functions.
   if (mode == PORT_INOUT)
      flags |= NET_F_EFFECTIVE | NET_F_INOUT;

   lower_sub_signals(lu, type, type, value_type, port, view, var,
                     VCODE_INVALID_REG, init_reg, VCODE_INVALID_REG,
                     VCODE_INVALID_REG, flags, bounds_reg);
}

static vcode_reg_t lower_constrain_port(lower_unit_t *lu, tree_t port, int pos,
                                        tree_t block, vcode_reg_t *map_regs)
{
   ident_t pname = tree_ident(port);
   const int nparams = tree_params(block);
   for (int i = 0; i < nparams; i++) {
      tree_t map = tree_param(block, i);
      switch (tree_subkind(map)) {
      case P_POS:
         if (tree_pos(map) != pos)
            continue;
         break;

      case P_NAMED:
         {
            tree_t name = tree_name(map);
            if (tree_kind(name) != T_REF || tree_ident(name) != pname)
               continue;
         }
         break;
      }

      assert(map_regs[i] != VCODE_INVALID_REG);
      assert(vcode_reg_kind(map_regs[i]) == VCODE_TYPE_UARRAY);

      return map_regs[i];
   }

   // TODO: this should be an assert and a proper error generated
   //       during sem/elab
   fatal_at(tree_loc(block), "cannot infer bounds for port %s",
            istr(tree_ident(port)));
}

static void lower_ports(lower_unit_t *lu, driver_set_t *ds, tree_t block)
{
   const int nports = tree_ports(block);
   const int nparams = tree_params(block);

   hset_t *direct = hset_new(nports * 2), *poison = NULL;
   vcode_reg_t *map_regs LOCAL = xmalloc_array(nparams, sizeof(vcode_reg_t));

   const bool collapse = !opt_get_int(OPT_NO_COLLAPSE);

   // Filter out "direct mapped" inputs which can be aliased to signals
   // in the scope above
   for (int i = 0; i < nparams; i++) {
      tree_t p = tree_param(block, i);

      if (collapse && lower_direct_mapped_port(lu, ds, block,
                                               p, direct, &poison))
         map_regs[i] = VCODE_INVALID_REG;
      else {
         tree_t value = tree_value(p);
         const tree_kind_t kind = tree_kind(value);
         if (kind == T_TYPE_CONV || kind == T_CONV_FUNC || kind == T_WAVEFORM)
            map_regs[i] = VCODE_INVALID_REG;
         else if (lower_is_signal_ref(value))
            map_regs[i] = lower_lvalue(lu, value);
         else
            map_regs[i] = lower_rvalue(lu, value);
      }
   }

   for (int i = 0; i < nports; i++) {
      tree_t port = tree_port(block, i);
      type_t type = tree_type(port);

      vcode_reg_t bounds_reg = VCODE_INVALID_REG;
      if (type_is_unconstrained(type))
         bounds_reg = lower_constrain_port(lu, port, i, block, map_regs);

      if (!hset_contains(direct, port))
         lower_port_signal(lu, port, bounds_reg);
      else if (poison != NULL && hset_contains(poison, port))
         lower_port_signal(lu, port, bounds_reg);
      else if (tree_class(port) == C_SIGNAL && tree_subkind(port) != PORT_IN) {
         // Any drivers for collapsed output ports must take their
         // initial value from the port declaration
         vcode_reg_t def_reg;
         if (tree_has_value(port))
            def_reg = lower_rvalue(lu, tree_value(port));
         else
            def_reg = lower_default_value(lu, type, VCODE_INVALID_REG);

         vcode_reg_t nets_reg = lower_port_ref(lu, port);
         lower_map_signal(lu, def_reg, nets_reg, type, type,
                          VCODE_INVALID_REG, port);
      }
   }

   for (int i = 0; i < nparams; i++) {
      tree_t map = tree_param(block, i);
      if (!hset_contains(direct, map))
         lower_port_map(lu, block, map, map_regs[i]);
      else if (poison != NULL && tree_subkind(map) == P_NAMED) {
         tree_t port = tree_ref(name_to_ref(tree_name(map)));
         if (hset_contains(poison, port))
            lower_port_map(lu, block, map, map_regs[i]);
      }
   }

   if (cover_enabled(lu->cover, COVER_MASK_TOGGLE)) {
      for (int i = 0; i < nports; i++)
         lower_toggle_coverage(lu, tree_port(block, i));
   }

   hset_free(direct);
   if (poison != NULL)
      hset_free(poison);
}

static void lower_check_generic_constraint(lower_unit_t *lu, tree_t expect,
                                           tree_t generic, vcode_reg_t locus)
{
   vcode_reg_t expect_reg = lower_rvalue(lu, expect);

   int hops;
   vcode_var_t actual_var = lower_get_var(lu, generic, &hops);
   assert(hops == 0);

   type_t type = tree_type(expect);

   vcode_reg_t test_reg;
   if (type_is_scalar(type)) {
      vcode_reg_t actual_reg = emit_load(actual_var);
      test_reg = emit_cmp(VCODE_CMP_EQ, expect_reg, actual_reg);
   }
   else {
      vcode_reg_t actual_reg;
      if (vtype_kind(vcode_var_type(actual_var)) == VCODE_TYPE_UARRAY)
         actual_reg = emit_load(actual_var);
      else
         actual_reg = emit_index(actual_var, VCODE_INVALID_REG);

      vcode_reg_t left_reg = actual_reg, right_reg = expect_reg;
      if (type_is_array(type)) {
         left_reg = lower_wrap(lu, type, actual_reg);
         right_reg = lower_wrap(lu, type, expect_reg);
      }

      ident_t func = lower_predef_func_name(type, "=");
      vcode_reg_t context_reg = lower_context_for_call(lu, func);
      vcode_reg_t args[] = { context_reg, left_reg, right_reg };
      vcode_type_t vbool = vtype_bool();
      test_reg = emit_fcall(func, vbool, vbool, VCODE_CC_PREDEF, args, 3);
   }

   vcode_type_t vseverity = vtype_int(0, SEVERITY_FAILURE - 1);
   vcode_reg_t error_reg = emit_const(vseverity, SEVERITY_ERROR);

   emit_assert(test_reg, VCODE_INVALID_REG, VCODE_INVALID_REG, error_reg,
               locus, VCODE_INVALID_REG, VCODE_INVALID_REG);
}

static void lower_pack_inst_generics(lower_unit_t *lu, tree_t inst, tree_t map)
{
   ident_t iname = tree_ident(inst);
   if (ident_runtil(iname, '.') == iname) {
      vcode_state_t state;
      vcode_state_save(&state);
      vcode_select_unit(vcode_unit_context());
      iname = ident_prefix(vcode_unit_name(), iname, '.');
      vcode_state_restore(&state);
   }

   vcode_reg_t context = emit_link_package(iname);

   const int ngenerics = tree_generics(inst);
   for (int i = 0; i < ngenerics; i++) {
      tree_t g = tree_generic(inst, i);
      type_t type = tree_type(g);

      if (tree_class(g) != C_CONSTANT)
         continue;

      vcode_type_t vtype = lower_type(type);
      vcode_type_t vbounds = lower_bounds(type);

      ident_t name = tree_ident(g);
      vcode_var_t var = emit_var(vtype, vbounds, name, VAR_CONST);

      vcode_reg_t ptr_reg = emit_link_var(context, name, vtype);

      if (type_is_scalar(type))
         emit_store(emit_load_indirect(ptr_reg), var);
      else if (type_is_array(type)) {
         if (lower_const_bounds(type)) {
            vcode_reg_t count_reg = lower_array_total_len(lu, type, ptr_reg);
            vcode_reg_t dest_reg = emit_index(var, VCODE_INVALID_REG);
            vcode_reg_t src_reg = lower_array_data(ptr_reg);
            emit_copy(dest_reg, src_reg, count_reg);
         }
      }
      else {
         vcode_reg_t dest_reg = emit_index(var, VCODE_INVALID_REG);
         emit_copy(dest_reg, ptr_reg, VCODE_INVALID_REG);
      }

      lower_put_vcode_obj(g, var, lu);

      tree_t g0 = tree_generic(tree_ref(inst), i);
      lower_put_vcode_obj(g0, var, lu);
   }

   switch (tree_subkind(map)) {
   case PACKAGE_MAP_MATCHING:
      {
         const int count = tree_genmaps(map);
         for (int i = 0; i < count; i++) {
            tree_t m = tree_genmap(map, i);
            assert(tree_subkind(m) == P_POS);

            tree_t g = tree_generic(inst, i);

            vcode_reg_t locus = lower_debug_locus(m);
            lower_check_generic_constraint(lu, tree_value(m), g, locus);
         }
      }
      break;

   case PACKAGE_MAP_DEFAULT:
      for (int i = 0; i < ngenerics; i++) {
         tree_t g = tree_generic(inst, i);
         tree_t g0 = tree_generic(tree_ref(map), i);

         vcode_reg_t locus = lower_debug_locus(map);
         lower_check_generic_constraint(lu, tree_value(g0), g, locus);
      }
      break;

   case PACKAGE_MAP_BOX:
      break;
   }
}

static void lower_generics(lower_unit_t *lu, tree_t block)
{
   const int ngenerics = tree_generics(block);
   assert(ngenerics == tree_genmaps(block));

   for (int i = 0; i < ngenerics; i++) {
      tree_t g = tree_generic(block, i);
      tree_t m = tree_genmap(block, i);
      assert(tree_subkind(m) == P_POS);

      const class_t class = tree_class(g);
      if (class == C_PACKAGE) {
         // Make generics in a package instance passed via a package
         // interface generic available
         tree_t map = tree_value(g);
         assert(tree_kind(map) == T_PACKAGE_MAP);

         tree_t inst = tree_ref(tree_value(m));
         assert(tree_kind(inst) == T_PACK_INST);

         lower_pack_inst_generics(lu, inst, map);
      }

      if (class != C_CONSTANT)
         continue;   // Skip type generics, etc.

      type_t type = tree_type(g);

      vcode_type_t vtype = lower_type(type);
      vcode_type_t vbounds = lower_bounds(type);
      vcode_var_t var = emit_var(vtype, vbounds, tree_ident(g), VAR_CONST);

      vcode_reg_t mem_reg = VCODE_INVALID_REG, count_reg = VCODE_INVALID_REG;

      const bool is_array = type_is_array(type);

      if (is_array && lower_const_bounds(type)) {
         mem_reg = emit_index(var, VCODE_INVALID_REG);
         count_reg = lower_array_total_len(lu, type, VCODE_INVALID_REG);
      }
      else if (type_is_record(type))
         mem_reg = emit_index(var, VCODE_INVALID_REG);

      tree_t value = tree_value(m);
      vcode_reg_t value_reg;
      switch (tree_kind(value)) {
      case T_AGGREGATE:
         value_reg = lower_aggregate(lu, value, mem_reg);
         break;
      case T_OPEN:
         assert(tree_has_value(g));
         value_reg = lower_rvalue(lu, tree_value(g));
         break;
      default:
         value_reg = lower_rvalue(lu, value);
         break;
      }

      if (is_array && mem_reg != VCODE_INVALID_REG) {
         vcode_reg_t locus = lower_debug_locus(g);
         lower_check_array_sizes(lu, type, tree_type(value),
                                 VCODE_INVALID_REG, value_reg, locus);
      }
      else if (type_is_scalar(type))
         lower_check_scalar_bounds(lu, value_reg, type, value, g);

      if (mem_reg != VCODE_INVALID_REG)
         emit_copy(mem_reg, lower_array_data(value_reg), count_reg);
      else if (is_array)
         emit_store(lower_wrap(lu, tree_type(value), value_reg), var);
      else
         emit_store(value_reg, var);

      lower_put_vcode_obj(g, var, lu);
   }
}

static void lower_deps_cb(ident_t unit_name, void *__ctx)
{
   lib_t lib = lib_require(ident_until(unit_name, '.'));

   ident_t this_unit = vcode_unit_name();

   const tree_kind_t kind = lib_index_kind(lib, unit_name);
   if (kind != T_ENTITY && unit_name == this_unit)
      return;   // Package body depends on package

   if (kind == T_PACKAGE && standard() >= STD_08) {
      tree_t unit = lib_get(lib, unit_name);
      if (is_uninstantiated_package(unit))
         return;   // No code generated for uninstantiated packages
   }

   if (kind == T_PACKAGE || kind == T_PACK_INST)
      emit_package_init(unit_name, VCODE_INVALID_REG);
}

static void lower_dependencies(tree_t primary, tree_t secondary)
{
   tree_walk_deps(primary, lower_deps_cb, NULL);
   if (secondary != NULL)
      tree_walk_deps(secondary, lower_deps_cb, NULL);
}

static void lower_pack_body(lower_unit_t *lu, object_t *obj)
{
   tree_t body = tree_from_object(obj);

   tree_t pack = tree_primary(body);
   assert(!is_uninstantiated_package(pack));

   if (standard() >= STD_08 && is_well_known(lu->name) == W_IEEE_1164) {
      // VHDL-2008 and later matching operators on STD_LOGIC are
      // implemented in the support package
      ident_t ieee_support = ident_new("NVC.IEEE_SUPPORT");
      emit_package_init(ieee_support, VCODE_INVALID_REG);
   }

   lower_dependencies(pack, body);
   lower_decls(lu, pack);
   lower_decls(lu, body);

   emit_return(VCODE_INVALID_REG);
}

static void lower_package(lower_unit_t *lu, object_t *obj)
{
   tree_t pack = tree_from_object(obj);
   assert(!is_uninstantiated_package(pack));

   lower_dependencies(pack, NULL);
   lower_generics(lu, pack);
   lower_decls(lu, pack);

   emit_return(VCODE_INVALID_REG);
}

vcode_reg_t lower_lvalue(lower_unit_t *lu, tree_t expr)
{
   return lower_expr(lu, expr, EXPR_LVALUE);
}

vcode_reg_t lower_rvalue(lower_unit_t *lu, tree_t expr)
{
   vcode_reg_t reg = lower_expr(lu, expr, EXPR_RVALUE);
   if (reg == VCODE_INVALID_REG)
      return reg;

   for (;;) {
      switch (vcode_reg_kind(reg)) {
      case VCODE_TYPE_SIGNAL:
         reg = lower_resolved(lu, tree_type(expr), reg);
         continue;
      case VCODE_TYPE_POINTER:
         {
            type_t type = tree_type(expr);
            if (lower_have_signal(reg)) {
               reg = lower_resolved(lu, tree_type(expr), reg);
               continue;
            }
            else if (!type_is_composite(type))
               return emit_load_indirect(reg);
            else
               return reg;
         }
      case VCODE_TYPE_UARRAY:
         if (lower_have_signal(reg))
            return lower_resolved(lu, tree_type(expr), reg);
         else
            return reg;
      default:
         return reg;
      }
   }
}

vcode_unit_t lower_case_generate_thunk(lower_unit_t *parent, tree_t t)
{
   // TODO: this should really be in eval.c

   ident_t context_id = NULL;
   if (parent != NULL) {
      vcode_select_unit(parent->vunit);
      context_id = vcode_unit_name();
   }

   vcode_unit_t context = parent ? parent->vunit : NULL;
   vcode_unit_t thunk = emit_thunk(NULL, tree_to_object(t), context);
   lower_unit_t *lu = lower_unit_new(parent->registry, parent, thunk, NULL, NULL);

   vcode_type_t vbool = vtype_bool();
   vcode_type_t vint = vtype_int(INT32_MIN, INT32_MAX);
   vcode_set_result(vint);

   if (parent != NULL) {
      vcode_type_t vcontext = vtype_context(context_id);
      emit_param(vcontext, vcontext, ident_new("context"));
   }

   tree_t value = tree_value(t);
   type_t type = tree_type(value);

   ident_t cmp_func = NULL;
   if (!type_is_scalar(type))
      cmp_func = lower_predef_func_name(tree_type(value), "=");

   vcode_reg_t value_reg = lower_rvalue(lu, value);

   if (cmp_func != NULL)
      value_reg = lower_wrap(lu, type, value_reg);

   const int nstmts = tree_stmts(t);
   for (int i = 0; i < nstmts; i++) {
      tree_t alt = tree_stmt(t, i);

      const int nassocs = tree_assocs(alt);
      for (int j = 0; j < nassocs; j++) {
         tree_t a = tree_assoc(alt, j);
         switch (tree_subkind(a)) {
         case A_NAMED:
            {
               tree_t name = tree_name(a);
               vcode_reg_t name_reg = lower_rvalue(lu, name);
               vcode_block_t match_bb = emit_block();
               vcode_block_t skip_bb = emit_block();

               if (cmp_func != NULL) {
                  if (vcode_reg_kind(name_reg) != VCODE_TYPE_UARRAY)
                     name_reg = lower_wrap(lu, tree_type(name), name_reg);

                  vcode_reg_t context_reg =
                     lower_context_for_call(lu, cmp_func);
                  vcode_reg_t args[] = { context_reg, name_reg, value_reg };
                  vcode_reg_t eq_reg = emit_fcall(cmp_func, vbool, vbool,
                                                  VCODE_CC_PREDEF, args, 3);
                  emit_cond(eq_reg, match_bb, skip_bb);
               }
               else {
                  vcode_reg_t eq_reg =
                     emit_cmp(VCODE_CMP_EQ, name_reg, value_reg);
                  emit_cond(eq_reg, match_bb, skip_bb);
               }

               vcode_select_block(match_bb);
               emit_return(emit_const(vint, i));

               vcode_select_block(skip_bb);
            }
            break;

         case A_RANGE:
            fatal_at(tree_loc(a), "sorry, this form of choice is not "
                     "yet supported");

         case A_OTHERS:
            emit_return(emit_const(vint, i));
            break;
         }
      }
   }

   if (!vcode_block_finished())
      emit_return(emit_const(vint, -1));

   lower_finished(lu);
   lower_unit_free(lu);

   return thunk;
}

vcode_unit_t lower_thunk(lower_unit_t *parent, tree_t t)
{
   ident_t context_id = NULL;
   if (parent != NULL) {
      vcode_select_unit(parent->vunit);
      context_id = vcode_unit_name();
   }

   vcode_unit_t context = parent ? parent->vunit : NULL;
   vcode_unit_t thunk = emit_thunk(NULL, tree_to_object(t), context);
   lower_unit_t *lu = lower_unit_new(parent ? parent->registry : NULL,
                                     parent, thunk, NULL, NULL);

   vcode_type_t vtype = VCODE_INVALID_TYPE;
   switch (tree_kind(t)) {
   case T_FCALL:
      {
         tree_t decl = tree_ref(t);
         if (tree_has_type(decl))
            vtype = lower_func_result_type(type_result(tree_type(decl)));
      }
      break;

   case T_ATTR_REF:
      vtype = lower_type(tree_type(t));
      break;

   default:
      break;
   }

   if (vtype == VCODE_INVALID_TYPE)
      vtype = lower_func_result_type(tree_type(t));

   vcode_set_result(vtype);

   if (parent != NULL) {
      vcode_type_t vcontext = vtype_context(context_id);
      emit_param(vcontext, vcontext, ident_new("context"));
   }

   vcode_reg_t result_reg = lower_rvalue(lu, t);
   if (type_is_scalar(tree_type(t)))
      emit_return(emit_cast(vtype, vtype, result_reg));
   else
      emit_return(result_reg);

   lower_finished(lu);
   lower_unit_free(lu);

   if (vcode_unit_has_undefined()) {
      vcode_unit_unref(thunk);
      return NULL;
   }

   vcode_close();
   return thunk;
}

lower_unit_t *lower_instance(unit_registry_t *ur, lower_unit_t *parent,
                             driver_set_t *ds, cover_tagging_t *cover,
                             tree_t block)
{
   assert(tree_kind(block) == T_BLOCK);

   vcode_select_unit(parent ? parent->vunit : NULL);

   ident_t prefix = parent ? vcode_unit_name() : lib_name(lib_work());
   ident_t label = tree_ident(block);
   ident_t name = ident_prefix(prefix, label, '.');

   vcode_unit_t vu = emit_instance(name, tree_to_object(block),
                                   parent ? parent->vunit : NULL);

   lower_unit_t *lu = lower_unit_new(ur, parent, vu, cover, block);
   unit_registry_put(ur, lu);

   cover_push_scope(cover, block);

   tree_t hier = tree_decl(block, 0);
   assert(tree_kind(hier) == T_HIER);

   lower_dependencies(block, tree_ref(hier));
   lower_generics(lu, block);
   lower_ports(lu, ds, block);
   lower_decls(lu, block);

   emit_return(VCODE_INVALID_REG);

   lower_finished(lu);
   return lu;
}

lower_unit_t *lower_unit_new(unit_registry_t *ur, lower_unit_t *parent,
                             vcode_unit_t vunit, cover_tagging_t *cover,
                             tree_t container)
{
   lower_unit_t *new = xcalloc(sizeof(lower_unit_t));
   new->parent    = parent;
   new->objects   = hash_new(128);
   new->container = container;
   new->vunit     = vunit;
   new->cover     = cover;
   new->registry  = ur;

   vcode_state_t state;
   vcode_state_save(&state);

   vcode_select_unit(vunit);
   const vunit_kind_t kind = vcode_unit_kind();

   new->name = vcode_unit_name();
   new->mode = (kind == VCODE_UNIT_THUNK) ? LOWER_THUNK : LOWER_NORMAL;

   vcode_state_restore(&state);

   // Prevent access to resolved signals during static elaboration
   new->elaborating = kind == VCODE_UNIT_INSTANCE
      || kind == VCODE_UNIT_PROTECTED || kind == VCODE_UNIT_PACKAGE;

   return new;
}

void lower_unit_free(lower_unit_t *lu)
{
   assert(lu->finished);

   hash_free(lu->objects);
   ACLEAR(lu->free_temps);
   free(lu);
}

vcode_unit_t get_vcode(lower_unit_t *lu)
{
   return lu->vunit;
}

////////////////////////////////////////////////////////////////////////////////

typedef enum {
   UNIT_DEFERRED = 1,
   UNIT_GENERATED = 2,
   UNIT_FINALISED = 3,
} unit_kind_t;

typedef struct _unit_registry {
   hash_t *map;
} unit_registry_t;

typedef struct {
   lower_unit_t    *parent;
   emit_fn_t        emit_fn;
   lower_fn_t       fn;
   ident_t          arena;
   ptrdiff_t        offset;
   cover_tagging_t *cover;
} deferred_unit_t;

unit_registry_t *unit_registry_new(void)
{
   unit_registry_t *ur = xcalloc(sizeof(unit_registry_t));
   ur->map = hash_new(128);

   return ur;
}

void unit_registry_free(unit_registry_t *ur)
{
   const void *key;
   void *value;
   for (hash_iter_t it = HASH_BEGIN; hash_iter(ur->map, &it, &key, &value); ) {

      switch (pointer_tag(value)) {
      case UNIT_FINALISED:
         // Let this leak for now as it may be referenced from the library
         break;

      case UNIT_GENERATED:
         {
            lower_unit_t *lu = untag_pointer(value, lower_unit_t);
            assert(lu->finished);
            assert(lu->registry == ur);
            lower_unit_free(lu);
         }
         break;

      case UNIT_DEFERRED:
         {
            deferred_unit_t *du = untag_pointer(value, deferred_unit_t);
            free(du);
         }
         break;

      default:
         fatal_trace("invalid tagged pointer %p", value);
      }
   }

   hash_free(ur->map);
   free(ur);
}

void unit_registry_put(unit_registry_t *ur, lower_unit_t *lu)
{
   assert(hash_get(ur->map, lu->name) == NULL);
   hash_put(ur->map, lu->name, tag_pointer(lu, UNIT_GENERATED));
}

bool unit_registry_query(unit_registry_t *ur, ident_t ident)
{
   return hash_get(ur->map, ident) != NULL;
}

void unit_registry_purge(unit_registry_t *ur, ident_t prefix)
{
   if (hash_get(ur->map, prefix) == NULL)
      return;   // Fail fast if root not registered

   const void *key;
   void *value;
   for (hash_iter_t it = HASH_BEGIN; hash_iter(ur->map, &it, &key, &value); ) {
      if (ident_starts_with((ident_t)key, prefix)) {
         switch (pointer_tag(value)) {
         case UNIT_FINALISED:
            // TODO: unref vcode unit?
            hash_delete(ur->map, key);
            break;

         case UNIT_GENERATED:
         case UNIT_DEFERRED:
            fatal_trace("cannot purge this unit kind");

         default:
            fatal_trace("invalid tagged pointer %p", value);
         }
      }
   }
}

void unit_registry_finalise(unit_registry_t *ur, lower_unit_t *lu)
{
   assert(pointer_tag(hash_get(ur->map, lu->name)) == UNIT_GENERATED);

   if (!lu->finished)
      lower_finished(lu);

   if (lu->deferred > 0)
      return;

   hash_put(ur->map, lu->name, tag_pointer(lu->vunit, UNIT_FINALISED));

   lower_unit_free(lu);
}

void unit_registry_put_all(unit_registry_t *ur, vcode_unit_t vu)
{
   vcode_select_unit(vu);

   ident_t ident = vcode_unit_name();
   assert(hash_get(ur->map, ident) == NULL);
   hash_put(ur->map, ident, tag_pointer(vu, UNIT_FINALISED));

   for (vcode_unit_t it = vcode_unit_child(vu); it; it = vcode_unit_next(it))
      unit_registry_put_all(ur, it);
}

vcode_unit_t unit_registry_get(unit_registry_t *ur, ident_t ident)
{
   void *ptr = hash_get(ur->map, ident);
   if (ptr == NULL) {
      ident_t it = ident;
      ident_t lname = ident_walk_selected(&it);
      ident_t uname = ident_walk_selected(&it);

      lib_t lib = lib_require(lname);

      ident_t unit_name = ident_prefix(lname, uname, '.');
      tree_t unit = lib_get(lib, unit_name);
      if (unit == NULL || !is_package(unit))
         return NULL;

      if (tree_kind(unit) == T_PACKAGE && package_needs_body(unit)) {
         tree_t body = body_of(unit);
         if (body == NULL)
            return NULL;

         unit_registry_defer(ur, unit_name, NULL, emit_package,
                             lower_pack_body, NULL, tree_to_object(body));
      }
      else
         unit_registry_defer(ur, unit_name, NULL, emit_package,
                             lower_package, NULL, tree_to_object(unit));

      if (unit_name != ident) {
         // We actually wanted a unit inside this package so need to
         // force code generation
         (void)unit_registry_get(ur, unit_name);
      }

      if ((ptr = hash_get(ur->map, ident)) == NULL)
         return NULL;
   }

   switch (pointer_tag(ptr)) {
   case UNIT_DEFERRED:
      {
         deferred_unit_t *du = untag_pointer(ptr, deferred_unit_t);

         vcode_state_t state;
         vcode_state_save(&state);

         object_t *obj = object_from_locus(du->arena, du->offset,
                                           lib_load_handler);

         vcode_unit_t context = du->parent ? du->parent->vunit : NULL;
         vcode_unit_t vu = (*du->emit_fn)(ident, obj, context);
         tree_t container = tree_from_object(obj);
         lower_unit_t *lu = lower_unit_new(ur, du->parent, vu,
                                           du->cover, container);

         hash_put(ur->map, ident, tag_pointer(lu, UNIT_GENERATED));

         (*du->fn)(lu, obj);

         for (lower_unit_t *p = du->parent; p; p = p->parent) {
            assert(p->deferred > 0);
            p->deferred--;
         }

         unit_registry_finalise(ur, lu);

         vcode_state_restore(&state);

         free(du);
         return vu;
      }

   case UNIT_GENERATED:
      {
         lower_unit_t *lu = untag_pointer(ptr, lower_unit_t);
         return lu->vunit;
      }

   case UNIT_FINALISED:
      return untag_pointer(ptr, struct _vcode_unit);

   default:
      fatal_trace("invalid tagged pointer %p", ptr);
   }
}

void unit_registry_defer(unit_registry_t *ur, ident_t ident,
                         lower_unit_t *parent, emit_fn_t emit_fn,
                         lower_fn_t fn, cover_tagging_t *cover,
                         object_t *object)
{
   void *ptr = hash_get(ur->map, ident);
   if (ptr == NULL) {
      deferred_unit_t *du = xcalloc(sizeof(deferred_unit_t));
      du->emit_fn = emit_fn;
      du->fn      = fn;
      du->parent  = parent;
      du->cover   = cover;

      for (lower_unit_t *p = du->parent; p; p = p->parent)
         p->deferred++;

      object_locus(object, &du->arena, &du->offset);

      hash_put(ur->map, ident, tag_pointer(du, UNIT_DEFERRED));
   }
#ifdef DEBUG
   else if (pointer_tag(ptr) == UNIT_DEFERRED) {
      deferred_unit_t *du = untag_pointer(ptr, deferred_unit_t);
      assert(du->emit_fn == emit_fn);
      assert(du->fn == fn);
      assert(du->cover == cover);

      object_t *o = object_from_locus(du->arena, du->offset, lib_load_handler);
      assert(o == object);
   }
#endif
}
