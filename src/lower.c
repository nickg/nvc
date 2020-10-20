//
//  Copyright (C) 2014-2018  Nick Gasson
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
#include "hash.h"

#include <assert.h>
#include <stdlib.h>
#include <inttypes.h>
#include <string.h>
#include <ctype.h>

typedef enum {
   EXPR_LVALUE,
   EXPR_RVALUE
} expr_ctx_t;

typedef enum {
   PATH_NAME,
   INSTANCE_NAME
} name_attr_t;

#define MAX_CASE_ARCS 32

typedef struct case_arc   case_arc_t;
typedef struct case_state case_state_t;
typedef struct loop_stack loop_stack_t;

struct loop_stack {
   loop_stack_t  *up;
   ident_t        name;
   vcode_block_t  test_bb;
   vcode_block_t  exit_bb;
};

// Connects case_state_t elements
struct case_arc {
   int64_t       value;
   case_state_t *next;
};

// Decision tree used for array case statements
struct case_state {
   tree_t        stmts;
   int           narcs;
   vcode_block_t block;
   case_arc_t    arcs[MAX_CASE_ARCS];
};

typedef enum {
   LOWER_NORMAL,
   LOWER_THUNK
} lower_mode_t;

typedef enum {
   SHORT_CIRCUIT_AND,
   SHORT_CIRCUIT_OR,
   SHORT_CIRCUIT_NOR
} short_circuit_op_t;

static const char  *verbose = NULL;
static bool         tmp_alloc_used = false;
static lower_mode_t mode = LOWER_NORMAL;
static hash_t      *vcode_objs = NULL;

static vcode_reg_t lower_expr(tree_t expr, expr_ctx_t ctx);
static vcode_reg_t lower_reify_expr(tree_t expr);
static vcode_type_t lower_bounds(type_t type);
static void lower_stmt(tree_t stmt, loop_stack_t *loops);
static vcode_unit_t lower_func_body(tree_t body, vcode_unit_t context);
static void lower_proc_body(tree_t body, vcode_unit_t context);
static vcode_reg_t lower_signal_ref(tree_t decl, expr_ctx_t ctx);
static vcode_reg_t lower_record_aggregate(tree_t expr, bool nest,
                                          bool is_const, expr_ctx_t ctx);
static vcode_reg_t lower_param_ref(tree_t decl, expr_ctx_t ctx);
static vcode_type_t lower_type(type_t type);
static vcode_reg_t lower_record_eq(vcode_reg_t r0, vcode_reg_t r1, type_t type);
static void lower_decls(tree_t scope, vcode_unit_t context);
static vcode_reg_t lower_array_dir(type_t type, int dim, vcode_reg_t reg);

typedef vcode_reg_t (*lower_signal_flag_fn_t)(vcode_reg_t, vcode_reg_t);
typedef vcode_reg_t (*arith_fn_t)(vcode_reg_t, vcode_reg_t);

#define SAVE_DEBUG_INFO \
   __attribute__((cleanup(emit_debug_info))) \
   const loc_t _old_loc = *vcode_last_loc()

static bool lower_is_const(tree_t t)
{
   if (tree_kind(t) == T_AGGREGATE) {
      bool is_const = true;
      const int nassocs = tree_assocs(t);
      for (int i = 0; i < nassocs; i++)
         is_const = is_const && lower_is_const(tree_value(tree_assoc(t, i)));
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
      const int ndims = array_dimension(type);
      for (int i = 0; i < ndims; i++) {
         range_t r = range_of(type, i);
         if (!lower_is_const(r.left) || !lower_is_const(r.right))
            return false;
         else if (r.kind != RANGE_TO && r.kind != RANGE_DOWNTO)
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
      return emit_cast(vtype_pointer(vtype_elem(type)),
                       VCODE_INVALID_TYPE, reg);

   default:
      vcode_dump();
      fatal_trace("invalid type in lower_array_data r%d", reg);
   }
}

static vcode_reg_t lower_array_left(type_t type, int dim, vcode_reg_t reg)
{
   if (type_is_unconstrained(type)) {
      assert(reg != VCODE_INVALID_REG);
      type_t index_type = index_type_of(type, dim);
      return emit_cast(lower_type(index_type), lower_bounds(index_type),
                       emit_uarray_left(reg, dim));
   }
   else {
      range_t r = range_of(type, dim);
      return lower_reify_expr(r.kind == RANGE_RDYN ? r.right : r.left);
   }
}

static vcode_reg_t lower_array_right(type_t type, int dim, vcode_reg_t reg)
{
   if (type_is_unconstrained(type)) {
      assert(reg != VCODE_INVALID_REG);
      type_t index_type = index_type_of(type, dim);
      return emit_cast(lower_type(index_type), lower_bounds(index_type),
                       emit_uarray_right(reg, dim));
   }
   else {
      range_t r = range_of(type, dim);
      return lower_reify_expr(r.kind == RANGE_RDYN ? r.left : r.right);
   }
}

static vcode_reg_t lower_range_dir(range_t r, int dim)
{
   switch (r.kind) {
   case RANGE_TO:
   case RANGE_DOWNTO:
      return emit_const(vtype_bool(), r.kind);

   case RANGE_DYN:
   case RANGE_RDYN:
      {
         assert(tree_kind(r.left) == T_ATTR_REF);
         assert(tree_kind(r.right) == T_ATTR_REF);

         tree_t base_expr = tree_name(r.left);
         type_t base_type = tree_type(base_expr);
         assert(type_is_array(base_type));

         vcode_reg_t base_reg = lower_expr(base_expr, EXPR_RVALUE);
         assert(vtype_kind(vcode_reg_type(base_reg)) == VCODE_TYPE_UARRAY);

         vcode_reg_t base_dir = lower_array_dir(base_type, dim, base_reg);
         return r.kind == RANGE_RDYN ? emit_not(base_dir) : base_dir;
      }

   case RANGE_EXPR:
   default:
      fatal_trace("unexpected range direction in %s", __func__);
   }
}

static vcode_reg_t lower_array_dir(type_t type, int dim, vcode_reg_t reg)
{
   if (type_is_unconstrained(type)) {
      assert(reg != VCODE_INVALID_REG);
      assert(vcode_reg_kind(reg) == VCODE_TYPE_UARRAY);
      return emit_uarray_dir(reg, dim);
   }
   else {
      assert(!type_is_unconstrained(type));
      return lower_range_dir(range_of(type, dim), dim);
   }
}

static vcode_reg_t lower_array_len(type_t type, int dim, vcode_reg_t reg)
{
   if (type_is_unconstrained(type)) {
      assert(reg != VCODE_INVALID_REG);
      return emit_uarray_len(reg, dim);
   }
   else {
      range_t r = range_of(type, dim);

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
         {
            vcode_reg_t dir_reg = lower_array_dir(type, dim, VCODE_INVALID_REG);
            vcode_reg_t downto_reg = emit_sub(left_reg, right_reg);
            vcode_reg_t upto_reg = emit_sub(right_reg, left_reg);
            diff = emit_select(dir_reg, downto_reg, upto_reg);
         }
         break;

      case RANGE_EXPR:
         fatal_trace("unexpected range direction %d in lower_array_len",
                     r.kind);
      }

      vcode_reg_t len_reg = emit_addi(diff, 1);
      vcode_type_t offset_type = vtype_offset();
      vcode_reg_t cast_reg =
         emit_cast(offset_type, VCODE_INVALID_TYPE, len_reg);
      vcode_reg_t zero_reg = emit_const(offset_type, 0);
      vcode_reg_t neg_reg = emit_cmp(VCODE_CMP_LT, cast_reg, zero_reg);

      return emit_select(neg_reg, zero_reg, cast_reg);
   }
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
      range_t r = range_of(type, i);
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
      return vtype_uarray(array_dimension(type), elem_type, elem_bounds);
}

static ident_t lower_record_unique_name(type_t type)
{
   // If a record type is not qualified with a package name then add a unique
   // suffix to its type name to avoid collisions
   ident_t name = type_ident(type);
   if (ident_until(name, '.') == name) {
      char buf[32];
      checked_sprintf(buf, sizeof(buf), "%"PRIxPTR, (uintptr_t)type);
      return ident_prefix(name, ident_new(buf), '@');
   }
   else
      return name;
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
         ident_t name = lower_record_unique_name(type);
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
      {
         ident_t name = lower_record_unique_name(type);
         vcode_type_t record = vtype_find_named_record(name);
         if (record == VCODE_INVALID_TYPE) {
            tree_t body = type_body(type);

            const int ndecls = tree_decls(body);
            int nfields = 0;
            vcode_type_t fields[ndecls];
            for (int i = 0; i < ndecls; i++) {
               tree_t decl = tree_decl(body, i);
               const tree_kind_t kind = tree_kind(decl);
               if (kind == T_VAR_DECL || kind == T_FILE_DECL)
                  fields[nfields++] = lower_type(tree_type(decl));
            }

            record = vtype_named_record(name, fields, nfields);
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
   if (type_kind(type) == T_SUBTYPE
       && (type_is_integer(type) || type_is_enum(type))) {
      range_t r = range_of(type, 0);
      int64_t low, high;
      if (folded_bounds(r, &low, &high))
         return vtype_int(low, high);
   }

   return lower_type(type);
}

static vcode_type_t lower_signal_type(type_t type)
{
   if (type_is_array(type)) {
      vcode_type_t base = vtype_signal(lower_type(type_elem(type)));
      if (lower_const_bounds(type))
         return base;
      else
         return vtype_uarray(array_dimension(type), base, base);
   }
   else
      return vtype_signal(lower_type(type));
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
         vcode_reg_t ptr = emit_vec_load(reg, VCODE_INVALID_REG, false);
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

static vcode_reg_t lower_wrap_with_new_bounds(type_t type, vcode_reg_t array,
                                              vcode_reg_t data)
{
   assert(type_is_array(type));

   const int ndims = array_dimension(type);
   vcode_dim_t dims[ndims];
   for (int i = 0; i < ndims; i++) {
      dims[i].left  = lower_array_left(type, i, array);
      dims[i].right = lower_array_right(type, i, array);
      dims[i].dir   = lower_array_dir(type, i, array);
   }

   return emit_wrap(lower_array_data(data), dims, ndims);
}

static vcode_reg_t lower_wrap(type_t type, vcode_reg_t data)
{
   return lower_wrap_with_new_bounds(type, data, data);
}

static bounds_kind_t lower_type_bounds_kind(type_t type)
{
   if (type_is_enum(type))
      return BOUNDS_ENUM;
   else
      return direction_of(type, 0) == RANGE_TO
         ? BOUNDS_TYPE_TO : BOUNDS_TYPE_DOWNTO;
}

static bool lower_scalar_has_static_bounds(type_t type, vcode_reg_t *low_reg,
                                           vcode_reg_t *high_reg)
{
   if (type_is_real(type))
      return true;  // TODO

   switch (type_kind(type)) {
   case T_INTEGER:
   case T_SUBTYPE:
      {
         range_t r = range_of(type, 0);
         int64_t low, high;
         if (!folded_bounds(r, &low, &high)) {
            vcode_reg_t dir_reg   = lower_range_dir(r, 0);
            vcode_reg_t left_reg  = lower_reify_expr(r.left);
            vcode_reg_t right_reg = lower_reify_expr(r.right);

            *low_reg  = emit_select(dir_reg, right_reg, left_reg);
            *high_reg = emit_select(dir_reg, left_reg, right_reg);
            return false;
         }
      }
      break;

   case T_ENUM:
   case T_PHYSICAL:
      break;

   default:
      fatal_trace("invalid type kind %s in lower_scalar_has_static_bounds",
                  type_kind_str(type_kind(type)));
   }

   *low_reg = *high_reg = VCODE_INVALID_TYPE;
   return true;
}

static char *lower_get_hint_string(tree_t where, const char *prefix)
{
   switch (tree_kind(where)) {
   case T_PORT_DECL:
      return xasprintf("%s|for parameter %s",
                       prefix ?: "", istr(tree_ident(where)));
   case T_VAR_DECL:
      return xasprintf("%s|for variable %s",
                       prefix ?: "", istr(tree_ident(where)));
   default:
      if (prefix != NULL)
         return xstrdup(prefix);
      else
         return NULL;
   }
}

static void lower_check_scalar_bounds(vcode_reg_t value, type_t type,
                                      tree_t where, tree_t hint)
{
   SAVE_DEBUG_INFO;

   if (tree_kind(where) == T_PORT_DECL)
      emit_debug_info(tree_loc(hint));
   else
      emit_debug_info(tree_loc(where));

   const bounds_kind_t kind = lower_type_bounds_kind(type);
   const char *prefix = kind == BOUNDS_ENUM ? type_pp(type) : NULL;
   char *hint_str LOCAL = lower_get_hint_string(where, prefix);

   vcode_reg_t low_reg, high_reg;
   if (lower_scalar_has_static_bounds(type, &low_reg, &high_reg))
      emit_bounds(value, lower_bounds(type), kind, hint_str);
   else {
      vcode_reg_t kind_reg = emit_const(vtype_offset(), kind);
      emit_dynamic_bounds(value, low_reg, high_reg, kind_reg, hint_str);
   }
}

static bool lower_have_signal(vcode_reg_t reg)
{
   const vtype_kind_t reg_kind = vcode_reg_kind(reg);
   return reg_kind == VCODE_TYPE_SIGNAL
      || (reg_kind == VCODE_TYPE_UARRAY
          && vtype_kind(vtype_elem(vcode_reg_type(reg))) == VCODE_TYPE_SIGNAL);
}

static vcode_reg_t lower_param(tree_t value, tree_t port, port_mode_t mode)
{
   type_t value_type = tree_type(value);

   class_t class = C_DEFAULT;
   type_t port_type = value_type;
   if (port != NULL) {
      port_type = tree_type(port);
      class = tree_class(port);
   }

   const bool must_reify =
      (type_is_scalar(value_type) || type_is_access(value_type)
       || type_is_file(value_type))
      && mode == PORT_IN;

   const bool lvalue = class == C_SIGNAL || class == C_FILE || mode != PORT_IN;

   vcode_reg_t reg = lower_expr(value, lvalue ? EXPR_LVALUE : EXPR_RVALUE);
   if (reg == VCODE_INVALID_REG)
      return reg;

   if (lower_have_signal(reg) && class != C_SIGNAL) {
      vcode_reg_t data_reg = lower_array_data(reg);
      vcode_reg_t len_reg = VCODE_INVALID_REG;
      if (type_is_array(value_type))
         len_reg = lower_array_total_len(value_type, reg);
      vcode_reg_t new_reg = emit_vec_load(data_reg, len_reg, false);

      if (vcode_reg_kind(reg) == VCODE_TYPE_UARRAY)
         reg = lower_wrap_with_new_bounds(value_type, reg, new_reg);
      else
         reg = new_reg;
   }

   if (type_is_array(value_type)) {
      const bool have_uarray = vcode_reg_kind(reg) == VCODE_TYPE_UARRAY;
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
   else if (class == C_SIGNAL || class == C_FILE)
      return reg;
   else {
      vcode_reg_t final = must_reify ? lower_reify(reg) : reg;
      if (mode != PORT_OUT && port != NULL && type_is_scalar(port_type))
         lower_check_scalar_bounds(lower_reify(final), port_type, port, value);
      return final;
   }
}

static vcode_reg_t lower_subprogram_arg(tree_t fcall, unsigned nth)
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
   if (tree_attr_str(decl, builtin_i) == NULL)
      port = tree_port(decl, nth);

   return lower_param(value, port, mode);
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

   vcode_block_t test_bb = emit_block();
   vcode_block_t body_bb = emit_block();
   vcode_block_t exit_bb = emit_block();

   vcode_type_t vbool = vtype_bool();
   vcode_reg_t result_reg = emit_alloca(vbool, vbool, VCODE_INVALID_REG);
   emit_store_indirect(emit_const(vbool, 0), result_reg);

   type_t elem_type = type_elem(left_type);

   vcode_reg_t stride = VCODE_INVALID_REG;
   if (type_is_array(elem_type))
      stride = lower_array_total_len(elem_type, VCODE_INVALID_REG);

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
   emit_store_indirect(len_ge_l, result_reg);
   emit_cond(emit_or(len_ge_l, len_ge_r), exit_bb, body_bb);

   // Loop body

   vcode_select_block(body_bb);

   vcode_reg_t ptr_inc = i_loaded;
   if (stride != VCODE_INVALID_REG)
      ptr_inc = emit_mul(ptr_inc, stride);

   vcode_reg_t l_ptr = emit_add(lhs_data, ptr_inc);
   vcode_reg_t r_ptr = emit_add(rhs_data, ptr_inc);

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
         cmp = eq = lower_record_eq(l_ptr, r_ptr, elem_type);
         body_bb = vcode_active_block();
      }
      else {
         vcode_reg_t l_val = emit_load_indirect(l_ptr);
         vcode_reg_t r_val = emit_load_indirect(r_ptr);

         cmp = emit_cmp(pred, l_val, r_val);
         eq  = (pred == VCODE_CMP_EQ) ? cmp
            : emit_cmp(VCODE_CMP_EQ, l_val, r_val);
      }
   }

   vcode_reg_t inc = emit_addi(i_loaded, 1);
   emit_store_indirect(inc, i_reg);

   emit_store_indirect(cmp, result_reg);

   vcode_reg_t i_eq_len = emit_cmp(VCODE_CMP_EQ, inc, left_len);
   vcode_reg_t done = emit_or(emit_not(eq), emit_and(len_eq, i_eq_len));

   emit_cond(done, exit_bb, test_bb);

   // Epilogue

   vcode_select_block(exit_bb);

   return emit_load_indirect(result_reg);
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
      vcode_reg_t r0_len = lower_array_total_len(r0_type, r0);
      vcode_reg_t r1_len = lower_array_total_len(r1_type, r1);

      if (r0_len == r1_len)
         return emit_memcmp(r0_data, r1_data, r0_len);
      else {
         vcode_reg_t eq_reg = emit_cmp(VCODE_CMP_EQ, r0_len, r1_len);

         vcode_type_t vbool = vtype_bool();
         vcode_reg_t result_reg = emit_alloca(vbool, vbool, VCODE_INVALID_REG);
         emit_store_indirect(eq_reg, result_reg);

         vcode_block_t cmp_bb = emit_block();
         vcode_block_t skip_bb = emit_block();

         emit_cond(eq_reg, cmp_bb, skip_bb);

         vcode_select_block(cmp_bb);
         emit_store_indirect(emit_memcmp(r0_data, r1_data, r0_len), result_reg);
         emit_jump(skip_bb);

         vcode_select_block(skip_bb);
         return emit_load_indirect(result_reg);
      }
   }
   else
      return lower_array_cmp_inner(r0_data, r1_data, r0, r1,
                                   r0_type, r1_type, pred);
}

static vcode_reg_t lower_signal_flag(tree_t ref, lower_signal_flag_fn_t fn)
{
   vcode_reg_t nets = lower_expr(ref, EXPR_LVALUE);
   if (nets == VCODE_INVALID_REG)
      return emit_const(vtype_bool(), 0);

   vcode_reg_t length = VCODE_INVALID_REG;
   type_t type = tree_type(ref);
   if (type_is_array(type))
      length = lower_array_total_len(type, nets);
   else
      length = emit_const(vtype_offset(), 1);

   return (*fn)(nets, length);
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

static ident_t lower_mangle_func(tree_t decl, vcode_unit_t context)
{
   ident_t prev = tree_attr_str(decl, mangled_i);
   if (prev != NULL)
      return prev;

   bool save_mangled_name = true;
   char *prefix LOCAL = NULL;

   const int nest_depth = tree_attr_int(decl, nested_i, 0);
   if (context == NULL || mode == LOWER_THUNK) {
      ident_t name = tree_ident(decl);
      if (ident_contains(name, ":"))
         save_mangled_name = true;   // Elaborated subprogram
      else if (lib_loaded(ident_until(name, '.')))
         save_mangled_name = true;   // Subprogram in package
      else {
         prefix = xasprintf("p%"PRIxPTR"__", (uintptr_t)decl);
         save_mangled_name = false;
      }
   }
   else if (nest_depth > 0 || !ident_contains(tree_ident(decl), ".:")) {
      vcode_state_t state;
      vcode_state_save(&state);
      vcode_select_unit(context);

      const vunit_kind_t ckind = vcode_unit_kind();
      if (ckind == VCODE_UNIT_PROCESS)
         ;
      else if (ckind != VCODE_UNIT_CONTEXT)
         prefix = xasprintf("%s__", istr(vcode_unit_name()));
      else
         prefix = xasprintf("%s.", istr(vcode_unit_name()));

      vcode_state_restore(&state);
   }

   ident_t new = mangle_func(decl, prefix);
   if (save_mangled_name)
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

static vcode_reg_t lower_name_attr(tree_t ref, name_attr_t which)
{
   if (mode == LOWER_THUNK)
      return emit_undefined(vtype_uarray(1, vtype_char(), vtype_char()));

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
   vcode_type_t ctype = vtype_char();

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
      return emit_cast(vtype, lower_bounds(result), reg);
   else
      return reg;
}

static vcode_reg_t lower_arith(tree_t fcall, arith_fn_t fn, vcode_reg_t r0,
                               vcode_reg_t r1)
{
   vcode_type_t r0_type = vcode_reg_type(r0);
   vcode_type_t r1_type = vcode_reg_type(r1);
   if (!vtype_eq(r0_type, r1_type)) {
      const unsigned r0_bits = bits_for_range(vtype_low(r0_type),
                                              vtype_high(r0_type));
      const unsigned r1_bits = bits_for_range(vtype_low(r1_type),
                                              vtype_high(r1_type));

      if (r1_bits > r0_bits)
         r0 = emit_cast(r1_type, vcode_reg_bounds(r0), r0);
      else
         r1 = emit_cast(r0_type, vcode_reg_bounds(r1), r1);
   }

   return lower_narrow(tree_type(fcall), (*fn)(r0, r1));
}

static vcode_reg_t lower_bit_vec_op(bit_vec_op_kind_t kind, vcode_reg_t r0,
                                    vcode_reg_t r1, tree_t fcall)
{
   type_t r0_type = lower_arg_type(fcall, 0);
   vcode_reg_t r0_len  = lower_array_len(r0_type, 0, r0);
   vcode_reg_t r0_data = lower_array_data(r0);
   vcode_reg_t r0_dir  = lower_array_dir(r0_type, 0, r0);

   type_t r1_type = lower_arg_type(fcall, 1);
   vcode_reg_t r1_len  = VCODE_INVALID_REG;
   vcode_reg_t r1_data = VCODE_INVALID_REG;
   vcode_reg_t r1_dir  = VCODE_INVALID_REG;
   if (r1 != VCODE_INVALID_REG) {
      r1_len  = lower_array_len(lower_arg_type(fcall, 1), 0, r1);
      r1_data = lower_array_data(r1);
      r1_dir  = lower_array_dir(r1_type, 0, r1);
   }

   return emit_bit_vec_op(kind, r0_data, r0_len, r0_dir, r1_data, r1_len,
                          r1_dir, lower_type(tree_type(fcall)));
}

static vcode_reg_t lower_bit_shift(bit_shift_kind_t kind, vcode_reg_t array,
                                   type_t type, vcode_reg_t shift)
{
   type_t elem = type_elem(type);
   vcode_type_t vtype = vtype_uarray(1, lower_type(elem), lower_bounds(elem));

   vcode_reg_t data_reg = lower_array_data(array);
   vcode_reg_t len_reg  = lower_array_len(type, 0, array);
   vcode_reg_t dir_reg  = lower_array_dir(type, 0, array);

   vcode_reg_t shift_reg = emit_cast(vtype_offset(), VCODE_INVALID_TYPE, shift);

   return emit_bit_shift(kind, data_reg, len_reg, dir_reg, shift_reg, vtype);
}

static void lower_cond_coverage(tree_t test, vcode_reg_t value)
{
   const int cover_tag = tree_attr_int(test, cond_tag_i, -1);
   if (cover_tag == -1)
      return;

   const int sub_cond = tree_attr_int(test, sub_cond_i, 0);

   emit_cover_cond(value, cover_tag, sub_cond);
}

static vcode_reg_t lower_logical(tree_t fcall, vcode_reg_t result)
{
   if (tree_attr_int(fcall, sub_cond_i, 0) > 0) {
      // This is a sub-condition of a Boolean expression being annotated
      // for coverage
      lower_cond_coverage(fcall, result);
   }

   return result;
}

static bool lower_trivial_expression(tree_t expr)
{
   // True if expression is side-effect free with no function calls
   switch (tree_kind(expr)) {
   case T_REF:
   case T_LITERAL:
      return true;
   case T_FCALL:
      {
         if (tree_attr_str(tree_ref(expr), builtin_i) == NULL)
            return false;

         const int nparams = tree_params(expr);
         for (int i = 0; i < nparams; i++) {
            if (!lower_trivial_expression(tree_value(tree_param(expr, i))))
               return false;
         }

         return true;
      }
      break;
   default:
      return false;
   }
}

static vcode_reg_t lower_short_circuit(tree_t fcall, short_circuit_op_t op)
{
   vcode_reg_t r0 = lower_subprogram_arg(fcall, 0);

   int64_t value;
   if (vcode_reg_const(r0, &value)) {
      vcode_reg_t result = VCODE_INVALID_REG;
      switch (op) {
      case SHORT_CIRCUIT_AND:
         result = value ? lower_subprogram_arg(fcall, 1) : r0;
         break;
      case SHORT_CIRCUIT_OR:
         result = value ? r0 : lower_subprogram_arg(fcall, 1);
         break;
      case SHORT_CIRCUIT_NOR:
         result = emit_not(value ? r0 : lower_subprogram_arg(fcall, 1));
         break;
      }

      return lower_logical(fcall, result);
   }

   if (lower_trivial_expression(tree_value(tree_param(fcall, 1)))) {
      vcode_reg_t r1 = lower_subprogram_arg(fcall, 1);
      switch (op) {
      case SHORT_CIRCUIT_AND: return lower_logical(fcall, emit_and(r0, r1));
      case SHORT_CIRCUIT_OR: return lower_logical(fcall, emit_or(r0, r1));
      case SHORT_CIRCUIT_NOR: return lower_logical(fcall, emit_nor(r0, r1));
      }
   }

   vcode_block_t arg1_bb = emit_block();
   vcode_block_t after_bb = emit_block();

   vcode_type_t vbool = vtype_bool();
   vcode_reg_t result_reg = emit_alloca(vbool, vbool, VCODE_INVALID_REG);
   if (op == SHORT_CIRCUIT_NOR)
      emit_store_indirect(emit_not(r0), result_reg);
   else
      emit_store_indirect(r0, result_reg);

   if (op == SHORT_CIRCUIT_AND)
      emit_cond(r0, arg1_bb, after_bb);
   else
      emit_cond(r0, after_bb, arg1_bb);

   vcode_select_block(arg1_bb);
   vcode_reg_t r1 = lower_subprogram_arg(fcall, 1);

   switch (op) {
   case SHORT_CIRCUIT_AND:
      emit_store_indirect(emit_and(r0, r1), result_reg);
      break;
   case SHORT_CIRCUIT_OR:
      emit_store_indirect(emit_or(r0, r1), result_reg);
      break;
   case SHORT_CIRCUIT_NOR:
      emit_store_indirect(emit_nor(r0, r1), result_reg);
      break;
   }

   emit_jump(after_bb);

   vcode_select_block(after_bb);
   vcode_reg_t result = emit_load_indirect(result_reg);
   return lower_logical(fcall, result);
}

static vcode_reg_t lower_builtin(tree_t fcall, ident_t builtin)
{
   if (icmp(builtin, "max"))
      return lower_min_max(VCODE_CMP_GT, fcall);
   else if (icmp(builtin, "min"))
      return lower_min_max(VCODE_CMP_LT, fcall);
   else if (icmp(builtin, "and"))
      return lower_short_circuit(fcall, SHORT_CIRCUIT_AND);
   else if (icmp(builtin, "or"))
      return lower_short_circuit(fcall, SHORT_CIRCUIT_OR);
   else if (icmp(builtin, "nor"))
      return lower_short_circuit(fcall, SHORT_CIRCUIT_NOR);

   vcode_reg_t r0 = lower_subprogram_arg(fcall, 0);
   vcode_reg_t r1 = lower_subprogram_arg(fcall, 1);

   type_t r0_type = lower_arg_type(fcall, 0);
   type_t r1_type = lower_arg_type(fcall, 1);

   if (icmp(builtin, "eq"))
      return lower_logical(fcall, emit_cmp(VCODE_CMP_EQ, r0, r1));
   else if (icmp(builtin, "neq"))
      return lower_logical(fcall, emit_cmp(VCODE_CMP_NEQ, r0, r1));
   else if (icmp(builtin, "lt"))
      return lower_logical(fcall, emit_cmp(VCODE_CMP_LT, r0, r1));
   else if (icmp(builtin, "gt"))
      return lower_logical(fcall, emit_cmp(VCODE_CMP_GT, r0, r1));
   else if (icmp(builtin, "leq"))
      return lower_logical(fcall, emit_cmp(VCODE_CMP_LEQ, r0, r1));
   else if (icmp(builtin, "geq"))
      return lower_logical(fcall, emit_cmp(VCODE_CMP_GEQ, r0, r1));
   else if (icmp(builtin, "mul"))
      return lower_arith(fcall, emit_mul, r0, r1);
   else if (icmp(builtin, "add"))
      return lower_arith(fcall, emit_add, r0, r1);
   else if (icmp(builtin, "sub"))
      return lower_arith(fcall, emit_sub, r0, r1);
   else if (icmp(builtin, "div")) {
      if (!type_eq(r0_type, r1_type))
         r1 = emit_cast(lower_type(r0_type), lower_bounds(r0_type), r1);
      return lower_narrow(tree_type(fcall), emit_div(r0, r1));
   }
   else if (icmp(builtin, "exp")) {
      if (!type_eq(r0_type, r1_type))
         r1 = emit_cast(lower_type(r0_type), lower_bounds(r0_type), r1);
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
      return lower_logical(fcall, emit_not(r0));
   else if (icmp(builtin, "xor"))
      return lower_logical(fcall, emit_xor(r0, r1));
   else if (icmp(builtin, "xnor"))
      return lower_logical(fcall, emit_xnor(r0, r1));
   else if (icmp(builtin, "nand"))
      return lower_logical(fcall, emit_nand(r0, r1));
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
   else if (icmp(builtin, "req"))
      return lower_logical(fcall, lower_record_eq(r0, r1, r0_type));
   else if (icmp(builtin, "rneq"))
      return lower_logical(fcall, emit_not(lower_record_eq(r0, r1, r0_type)));
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
      vcode_reg_t data   = r1;
      if (type_is_array(r1_type)) {
         length = lower_array_len(r1_type, 0, r1);
         data   = lower_array_data(r1);
      }
      emit_file_write(r0, data, length);
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
   else if (icmp(builtin, "v_not"))
      return lower_bit_vec_op(BIT_VEC_NOT, r0, r1, fcall);
   else if (icmp(builtin, "v_and"))
      return lower_bit_vec_op(BIT_VEC_AND, r0, r1, fcall);
   else if (icmp(builtin, "v_or"))
      return lower_bit_vec_op(BIT_VEC_OR, r0, r1, fcall);
   else if (icmp(builtin, "v_xor"))
      return lower_bit_vec_op(BIT_VEC_XOR, r0, r1, fcall);
   else if (icmp(builtin, "v_xnor"))
      return lower_bit_vec_op(BIT_VEC_XNOR, r0, r1, fcall);
   else if (icmp(builtin, "v_nand"))
      return lower_bit_vec_op(BIT_VEC_NAND, r0, r1, fcall);
   else if (icmp(builtin, "v_nor"))
      return lower_bit_vec_op(BIT_VEC_NOR, r0, r1, fcall);
   else if (icmp(builtin, "sll"))
      return lower_bit_shift(BIT_SHIFT_SLL, r0, r0_type, r1);
   else if (icmp(builtin, "srl"))
      return lower_bit_shift(BIT_SHIFT_SRL, r0, r0_type, r1);
   else if (icmp(builtin, "sla"))
      return lower_bit_shift(BIT_SHIFT_SLA, r0, r0_type, r1);
   else if (icmp(builtin, "sra"))
      return lower_bit_shift(BIT_SHIFT_SRA, r0, r0_type, r1);
   else if (icmp(builtin, "rol"))
      return lower_bit_shift(BIT_SHIFT_ROL, r0, r0_type, r1);
   else if (icmp(builtin, "ror"))
      return lower_bit_shift(BIT_SHIFT_ROR, r0, r0_type, r1);
   else if (icmp(builtin, "mulrp") || icmp(builtin, "mulri")) {
      vcode_type_t vreal  = vtype_real();
      vcode_type_t rtype  = lower_type(tree_type(fcall));
      return emit_cast(rtype, rtype, emit_mul(r0, emit_cast(vreal, vreal, r1)));
   }
   else if (icmp(builtin, "mulpr") || icmp(builtin, "mulir")) {
      vcode_type_t vreal  = vtype_real();
      vcode_type_t rtype  = lower_type(tree_type(fcall));
      return emit_cast(rtype, rtype, emit_mul(emit_cast(vreal, vreal, r0), r1));
   }
   else if (icmp(builtin, "divpr")) {
      vcode_type_t vreal  = vtype_real();
      vcode_type_t rtype  = lower_type(tree_type(fcall));
      return emit_cast(rtype, rtype,
                       emit_div(emit_cast(vreal, vreal, r0), r1));
   }
   else if (icmp(builtin, "divri")) {
      vcode_type_t vreal  = vtype_real();
      vcode_type_t rtype  = lower_type(tree_type(fcall));
      return emit_cast(rtype, rtype,
                       emit_div(r0, emit_cast(vreal, vreal, r1)));
   }
   else
      fatal_at(tree_loc(fcall), "cannot lower builtin %s", istr(builtin));
}

static vcode_type_t lower_func_result_type(tree_t func)
{
   type_t result = type_result(tree_type(func));
   if (type_is_array(result) && lower_const_bounds(result)) {
      return vtype_pointer(lower_type(lower_elem_recur(result)));
   }
   if (type_is_record(result))
      return vtype_pointer(lower_type(result));
   else
      return lower_type(result);
}

static vcode_reg_t lower_fcall(tree_t fcall, expr_ctx_t ctx)
{
   tree_t decl = tree_ref(fcall);

   ident_t builtin = tree_attr_str(decl, builtin_i);
   if (builtin != NULL)
      return lower_builtin(fcall, builtin);

   ident_t name = lower_mangle_func(decl, vcode_unit_context());

   const int nargs = tree_params(fcall);
   vcode_reg_t args[nargs];
   for (int i = 0; i < nargs; i++)
      args[i] = lower_subprogram_arg(fcall, i);

   if (!type_is_scalar(type_result(tree_type(decl))))
      tmp_alloc_used = true;

   vcode_type_t rtype = lower_func_result_type(decl);
   const int nest_depth = tree_attr_int(decl, nested_i, 0);
   if (nest_depth > 0) {
      const int hops = vcode_unit_depth() - nest_depth;
      return emit_nested_fcall(name, rtype, args, nargs, hops);
   }
   else
      return emit_fcall(name, rtype, args, nargs);
}

static vcode_reg_t *lower_string_literal_chars(tree_t lit, int *nchars)
{
   type_t ltype = tree_type(lit);
   vcode_type_t vtype = lower_type(type_elem(ltype));

   *nchars = tree_chars(lit);
   vcode_reg_t *tmp = xmalloc(*nchars * sizeof(vcode_reg_t));

   for (int i = 0; i < *nchars; i++)
      tmp[i] = emit_const(vtype, tree_pos(tree_ref(tree_char(lit, i))));

   return tmp;
}

static vcode_reg_t lower_string_literal(tree_t lit, bool allocate)
{
   int nchars;
   vcode_reg_t *tmp LOCAL = lower_string_literal_chars(lit, &nchars);

   type_t type = tree_type(lit);
   if (type_is_array(type) && !lower_const_bounds(type)) {
      vcode_type_t base = vtype_pointer(lower_type(type_elem(type)));
      vcode_reg_t data = emit_const_array(base, tmp, nchars, allocate);
      if (type_is_unconstrained(type)) {
         // Will occur with overridden generic strings
         vcode_dim_t dim0 = {
            .left  = emit_const(vtype_offset(), 1),
            .right = emit_const(vtype_offset(), nchars),
            .dir   = emit_const(vtype_bool(), RANGE_TO)
         };
         return emit_wrap(data, &dim0, 1);
      }
      else
         return lower_wrap(type, data);
   }
   else
      return emit_const_array(lower_type(type), tmp, nchars, allocate);
}

static vcode_reg_t lower_literal(tree_t lit, expr_ctx_t ctx)
{
   if (ctx == EXPR_LVALUE)
      return VCODE_INVALID_REG;

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

static int lower_get_vcode_obj(tree_t t)
{
   if (vcode_objs == NULL)
      return VCODE_INVALID_REG;
   else {
      const void *ptr = hash_get(vcode_objs, t);
      return (uintptr_t)ptr - 1;
   }
}

static void lower_put_vcode_obj(tree_t t, int obj)
{
   hash_put(vcode_objs, t, (void *)(uintptr_t)(obj + 1));
}

static vcode_var_t lower_get_var(tree_t decl)
{
   vcode_var_t var = lower_get_vcode_obj(decl);

   const bool is_extern =
      var == VCODE_INVALID_VAR
      && tree_attr_int(decl, prot_field_i, -1) == -1
      && mode == LOWER_NORMAL;

   if (is_extern) {
      vcode_state_t state;
      vcode_state_save(&state);

      while (vcode_unit_kind() != VCODE_UNIT_CONTEXT)
         vcode_select_unit(vcode_unit_context());

      type_t type = tree_type(decl);
      var = emit_extern_var(lower_type(type), lower_bounds(type),
                            tree_ident(decl));

      vcode_state_restore(&state);
   }

   return var;
}

static vcode_signal_t lower_get_signal(tree_t decl)
{
   if (mode == LOWER_THUNK)
      return emit_undefined(vtype_signal(lower_type(tree_type(decl))));

   vcode_signal_t sig = lower_get_vcode_obj(decl);
   if (sig == VCODE_INVALID_SIGNAL) {
      vcode_state_t state;
      vcode_state_save(&state);
      vcode_select_unit(vcode_unit_context());

      type_t type = tree_type(decl);
      const char *name = package_signal_path_name(tree_ident(decl));

      sig = emit_signal(vtype_signal(lower_type(type)), lower_bounds(type),
                        ident_new(name), VCODE_INVALID_VAR, NULL, 0, true);
      lower_put_vcode_obj(decl, sig);

      vcode_state_restore(&state);
   }

   return sig;
}

static vcode_reg_t lower_protected_var(tree_t decl)
{
   const int pfield = tree_attr_int(decl, prot_field_i, -1);
   assert(pfield != -1);

   vcode_reg_t pstruct = vcode_count_params() - 1;
   assert(pstruct >= 0);

   return emit_record_ref(pstruct, pfield);
}

static vcode_reg_t lower_var_ref(tree_t decl, expr_ctx_t ctx)
{
   type_t type = tree_type(decl);

   vcode_var_t var = lower_get_var(decl);
   if (var == VCODE_INVALID_VAR) {
      if (mode == LOWER_THUNK) {
         if (tree_kind(decl) == T_CONST_DECL && tree_has_value(decl))
            return lower_expr(tree_value(decl), ctx);
         else {
            emit_comment("Cannot resolve variable %s", istr(tree_ident(decl)));
            vcode_type_t vtype = lower_type(type);
            const vtype_kind_t vtkind = vtype_kind(vtype);
            if (vtkind == VCODE_TYPE_CARRAY)
               return emit_undefined(vtype_pointer(vtype_elem(vtype)));
            else if (ctx == EXPR_LVALUE || vtkind == VCODE_TYPE_RECORD)
               return emit_undefined(vtype_pointer(vtype));
            else
               return emit_undefined(vtype);
         }
      }
      else
         return lower_protected_var(decl);
   }

   const int depth = tree_attr_int(decl, nested_i, 0);
   if (depth > 0 && vcode_unit_depth() != depth) {
      vcode_reg_t ptr_reg = emit_var_upref(vcode_unit_depth() - depth, var);
      if (ctx == EXPR_LVALUE)
         return ptr_reg;
      else if (type_is_scalar(type))
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
   else if ((type_is_scalar(type) || type_is_file(type) || type_is_access(type))
            && ctx == EXPR_LVALUE)
      return emit_index(var, VCODE_INVALID_REG);
   else
      return emit_load(var);
}

static vcode_reg_t lower_signal_ref(tree_t decl, expr_ctx_t ctx)
{
   type_t type = tree_type(decl);

   if (mode == LOWER_THUNK)
      return emit_undefined(lower_signal_type(type));

   vcode_signal_t sig = lower_get_signal(decl);
   if (ctx == EXPR_LVALUE)
      return emit_nets(sig);
   else {
      vcode_var_t shadow = vcode_signal_shadow(sig);
      if (shadow != VCODE_INVALID_VAR) {
         vcode_reg_t r = emit_load(shadow);
         if (type_is_array(type) || type_is_record(type))
            return r;
         else
            return emit_load_indirect(r);
      }
      else
         return emit_nets(sig);
   }
}

static vcode_reg_t lower_param_ref(tree_t decl, expr_ctx_t ctx)
{
   vcode_reg_t reg = lower_get_vcode_obj(decl);

   const bool undefined_in_thunk =
      mode == LOWER_THUNK && (reg == VCODE_INVALID_REG
                              || tree_class(decl) == C_SIGNAL
                              || type_is_protected(tree_type(decl)));
   if (undefined_in_thunk) {
      emit_comment("Cannot resolve reference to %s", istr(tree_ident(decl)));
      if (tree_class(decl) == C_SIGNAL)
         return emit_undefined(lower_signal_type(tree_type(decl)));
      else {
         vcode_type_t vtype = lower_type(tree_type(decl));
         if (vtype_kind(vtype) == VCODE_TYPE_RECORD)
            return emit_undefined(vtype_pointer(vtype));
         else
            return emit_undefined(vtype);
      }
   }
   else if (reg == VCODE_INVALID_REG && tree_class(decl) != C_SIGNAL) {
      vcode_dump();
      fatal_trace("missing register for parameter %s",
                  istr(tree_ident(decl)));
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

   if (!alias_const || !value_const)
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
      if (ctx == EXPR_LVALUE)
         return VCODE_INVALID_REG;
      else
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
      if (ctx == EXPR_LVALUE)
         return VCODE_INVALID_REG;
      else if (type_is_scalar(tree_type(decl)))
         return lower_expr(tree_value(decl), ctx);
      else
         return lower_var_ref(decl, ctx);

   case T_UNIT_DECL:
      return lower_expr(tree_value(decl), ctx);

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

   assert(vtype_kind(vcode_reg_type(off)) == VCODE_TYPE_INT);

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
      range_t r = range_of(type, dim);
      vcode_reg_t left = lower_reify_expr(r.left);
      if (r.kind == RANGE_TO)
         zeroed = emit_sub(off, left);
      else
         zeroed = emit_sub(left, off);
   }

   return emit_cast(vtype_offset(), VCODE_INVALID_TYPE, zeroed);
}

static vcode_reg_t lower_unalias_index(tree_t alias, vcode_reg_t index,
                                       vcode_reg_t meta)
{
   type_t alias_type = tree_type(alias);
   type_t base_type  = tree_type(tree_value(alias));

   assert(array_dimension(alias_type) == 1);  // TODO: multi-dimensional arrays

   range_t alias_r = range_of(alias_type, 0);
   vcode_reg_t off = emit_sub(index, lower_reify_expr(alias_r.left));

   vcode_reg_t bleft = VCODE_INVALID_REG, bdir = VCODE_INVALID_REG;
   switch (type_kind(base_type)) {
   case T_CARRAY:
   case T_SUBTYPE:
      // The transformation is a constant offset of indices
      {
         range_t base_r = range_of(base_type, 0);
         bleft = lower_reify_expr(base_r.left);
         bdir  = lower_array_dir(base_type, 0, VCODE_INVALID_REG);
      }
      break;

   case T_UARRAY:
      // The transformation must be computed at runtime
      {
         assert(meta != VCODE_INVALID_REG);
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

static void lower_check_array_bounds(type_t type, int dim, vcode_reg_t array,
                                     vcode_reg_t value, tree_t where,
                                     tree_t hint)
{
   SAVE_DEBUG_INFO;

   if (tree_kind(where) == T_PORT_DECL)
      emit_debug_info(tree_loc(hint));
   else
      emit_debug_info(tree_loc(where));

   vcode_reg_t left_reg  = lower_array_left(type, dim, array);
   vcode_reg_t right_reg = lower_array_right(type, dim, array);
   vcode_reg_t dir_reg   = lower_array_dir(type, dim, array);

   vcode_reg_t min_reg = emit_select(dir_reg, right_reg, left_reg);
   vcode_reg_t max_reg = emit_select(dir_reg, left_reg, right_reg);

   vcode_type_t kind_type = vtype_offset();
   vcode_reg_t kind_reg =
      emit_select(dir_reg,
                  emit_const(kind_type, BOUNDS_ARRAY_DOWNTO),
                  emit_const(kind_type, BOUNDS_ARRAY_TO));

   char *hint_str LOCAL = lower_get_hint_string(where, NULL);

   emit_dynamic_bounds(value, min_reg, max_reg, kind_reg, hint_str);
}

static vcode_reg_t lower_array_stride(vcode_reg_t array, type_t type)
{
   type_t elem = type_elem(type);
   if (type_is_array(elem)) {
      vcode_reg_t stride = lower_array_total_len(elem, VCODE_INVALID_REG);
      emit_comment("Array of array stride is r%d", stride);
      return stride;
   }
   else if (type_is_record(elem) && lower_have_signal(array))
      return emit_const(vtype_offset(), type_width(elem));
   else
      return emit_const(vtype_offset(), 1);
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

   const bool elide_bounds = tree_flags(ref) & TREE_F_ELIDE_BOUNDS;

   vcode_reg_t idx = emit_const(vtype_offset(), 0);
   const int nparams = tree_params(ref);
   for (int i = 0; i < nparams; i++) {
      tree_t p = tree_param(ref, i);
      assert(tree_subkind(p) == P_POS);

      vcode_reg_t offset = lower_reify_expr(tree_value(p));

      if (!elide_bounds)
         lower_check_array_bounds(value_type, i,
                                  (alias ? VCODE_INVALID_REG : array), offset,
                                  tree_value(p), NULL);

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

   idx = emit_mul(idx, lower_array_stride(array, value_type));

   return idx;
}

static vcode_reg_t lower_array_ref(tree_t ref, expr_ctx_t ctx)
{
   tree_t value = tree_value(ref);

   vcode_reg_t array = lower_expr(value, ctx);
   if (array == VCODE_INVALID_REG)
      return array;

   const vtype_kind_t vtkind = vtype_kind(vcode_reg_type(array));
   assert(vtkind == VCODE_TYPE_POINTER || vtkind == VCODE_TYPE_UARRAY
          || vtkind == VCODE_TYPE_SIGNAL);

   vcode_reg_t offset_reg = lower_array_ref_offset(ref, array);
   vcode_reg_t data_reg   = lower_array_data(array);
   return emit_add(data_reg, offset_reg);
}

static vcode_reg_t lower_range_null(vcode_reg_t left, vcode_reg_t right,
                                    vcode_reg_t dir, range_kind_t kind)
{
   if (kind == RANGE_RDYN)
      dir = emit_not(dir);

   return emit_range_null(left, right, dir);
}

static vcode_reg_t lower_array_slice(tree_t slice, expr_ctx_t ctx)
{
   tree_t value = tree_value(slice);
   range_t r    = tree_range(slice, 0);
   type_t type  = tree_type(value);

   vcode_reg_t left_reg  = lower_reify_expr(r.left);
   vcode_reg_t right_reg = lower_reify_expr(r.right);
   vcode_reg_t kind_reg = lower_range_dir(r, 0);

   vcode_reg_t null_reg =
      lower_range_null(left_reg, right_reg, kind_reg, r.kind);

   tree_t alias = NULL;
   if (tree_kind(value) == T_REF) {
      tree_t decl = tree_ref(value);
      if (tree_kind(decl) == T_ALIAS)
         alias = decl;
   }

   vcode_reg_t array_reg = VCODE_INVALID_REG;
   if (alias == NULL)
      array_reg = lower_expr(value, ctx);

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

   lower_check_array_bounds(type, 0, array_reg, left_reg, r.left, NULL);
   lower_check_array_bounds(type, 0, array_reg, right_reg, r.left, NULL);

   if (!known_not_null) {
      emit_jump(after_bounds_bb);
      vcode_select_block(after_bounds_bb);
   }

   if (alias != NULL) {
      tree_t aliased = tree_value(alias);
      type = tree_type(aliased);
      array_reg = lower_expr(aliased, ctx);
      left_reg  = lower_unalias_index(alias, left_reg, array_reg);
      right_reg = lower_unalias_index(alias, right_reg, array_reg);
      kind_reg  = lower_array_dir(type, 0, array_reg);
   }

   if (array_reg == VCODE_INVALID_REG)
      return VCODE_INVALID_REG;

   vcode_reg_t stride_reg = lower_array_stride(array_reg, type);

   vcode_reg_t data_reg = lower_array_data(array_reg);
   vcode_reg_t off_reg  = lower_array_off(left_reg, array_reg, type, 0);
   vcode_reg_t ptr_reg  = emit_add(data_reg, emit_mul(off_reg, stride_reg));

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

   range_t r = range_of(type, dim);
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
         sub = lower_string_literal_chars(value, &nsub);
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
            range_bounds(tree_range(a, 0), &r_low, &r_high);

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
         range_t r = range_of(type, 0);
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

   type_t scalar_elem_type = lower_elem_recur(elem_type);

   const bool multidim =
      type_is_array(agg_type) && array_dimension(agg_type) > 1;

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
      else {
         vcode_reg_t byte_reg  = emit_const(vtype_int(0, 255), byte);
         emit_memset(mem_reg, byte_reg,
                     emit_mul(len_reg,
                              emit_const(offset_type, (bits + 7) / 8)));
      }

      return emit_wrap(mem_reg, &dim0, 1);
   }

   vcode_reg_t ivar = emit_alloca(offset_type, offset_type, VCODE_INVALID_REG);
   emit_store_indirect(emit_const(offset_type, 0), ivar);

   vcode_reg_t stride = VCODE_INVALID_REG;
   if (type_is_array(elem_type)) {
      stride = lower_array_total_len(elem_type, VCODE_INVALID_REG);
      emit_comment("Array of array stride is r%d", stride);
   }

   if (multidim) {
      if (stride == VCODE_INVALID_REG)
         stride = emit_const(vtype_offset(), 1);

      const int dims = array_dimension(agg_type);
      for (int i = 1; i < dims; i++)
         stride = emit_mul(stride,
                           lower_array_len(agg_type, i, VCODE_INVALID_REG));
      emit_comment("Multidimensional array stride is r%d", stride);
   }


   vcode_reg_t len0_reg = len_reg;
   if (type_is_array(elem_type) || multidim)
      len0_reg = lower_array_len(agg_type, 0, VCODE_INVALID_REG);

   vcode_block_t test_bb = emit_block();
   vcode_block_t body_bb = emit_block();
   vcode_block_t exit_bb = emit_block();

   emit_jump(test_bb);

   // Loop test
   vcode_select_block(test_bb);
   vcode_reg_t i_loaded = emit_load_indirect(ivar);
   vcode_reg_t ge = emit_cmp(VCODE_CMP_GEQ, i_loaded, len0_reg);
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
      if (kind != A_OTHERS) {
         tree_t value = tree_value(a);
         value_reg = lower_expr(value, EXPR_RVALUE);
         if (type_is_scalar(tree_type(value)))
            value_reg = lower_reify(value_reg);
      }

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
            vcode_reg_t cast_reg = emit_cast(vtype_offset(),
                                             VCODE_INVALID_TYPE, off_reg);

            vcode_reg_t eq = emit_cmp(VCODE_CMP_EQ, i_loaded, cast_reg);
            what = emit_select(eq, value_reg, what);
         }
         break;

      case A_RANGE:
         {
            range_t r = tree_range(a, 0);

            vcode_cmp_t lpred =
               (r.kind == RANGE_TO ? VCODE_CMP_GEQ : VCODE_CMP_LEQ);
            vcode_cmp_t rpred =
               (r.kind == RANGE_TO ? VCODE_CMP_LEQ : VCODE_CMP_GEQ);

            vcode_reg_t lcmp_reg =
               emit_cmp(lpred, i_loaded,
                        emit_cast(offset_type, VCODE_INVALID_TYPE,
                                  lower_reify_expr(r.left)));
            vcode_reg_t rcmp_reg =
               emit_cmp(rpred, i_loaded,
                        emit_cast(offset_type, VCODE_INVALID_TYPE,
                                  lower_reify_expr(r.right)));

            vcode_reg_t in_reg = emit_or(lcmp_reg, rcmp_reg);
            what = emit_select(in_reg, value_reg, what);
         }
         break;

      case A_OTHERS:
         break;
      }
   }

   vcode_reg_t i_stride = i_loaded;
   if (stride != VCODE_INVALID_REG)
      i_stride = emit_mul(i_loaded, stride);

   vcode_reg_t ptr_reg = emit_add(mem_reg, i_stride);
   if (type_is_array(elem_type)) {
      vcode_reg_t src_reg = lower_array_data(what);
      vcode_reg_t length_reg = lower_array_total_len(elem_type, what);
      emit_copy(ptr_reg, src_reg, length_reg);
   }
   else if (multidim) {
      vcode_reg_t src_reg = lower_array_data(what);
      emit_copy(ptr_reg, src_reg, stride);
   }
   else if (type_is_record(elem_type))
      emit_copy(ptr_reg, what, VCODE_INVALID_REG);
   else
      emit_store_indirect(lower_reify(what), ptr_reg);

   emit_store_indirect(emit_addi(i_loaded, 1), ivar);
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
            v = lower_string_literal(value, false);
         else if (mode == LOWER_THUNK && !lower_const_bounds(value_type))
            v = emit_undefined(lower_type(value_type));
         else {
            int nvals;
            vcode_reg_t *values LOCAL =
               lower_const_array_aggregate(value, value_type, 0, &nvals);
            v = emit_const_array(lower_type(value_type), values, nvals, false);
         }
      }
      else if (type_is_record(value_type) && is_const)
         v = lower_record_aggregate(value, true, true, ctx);
      else if (type_is_scalar(value_type))
         v = lower_reify_expr(value);
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

   if (is_const) {
      vcode_reg_t reg = emit_const_record(lower_type(type), vals, nfields);
      if (!nest) {
         vcode_type_t vtype = lower_type(type);
         vcode_reg_t mem_reg = emit_alloca(vtype, vtype, VCODE_INVALID_REG);
         emit_store_indirect(reg, mem_reg);
         return mem_reg;
      }
      else
         return reg;
   }
   else {
      vcode_type_t vtype = lower_type(type);
      vcode_reg_t mem_reg = emit_alloca(vtype, vtype, VCODE_INVALID_REG);

      for (int i = 0; i < nfields; i++) {
         type_t ftype = tree_type(type_field(type, i));
         vcode_reg_t ptr_reg = emit_record_ref(mem_reg, i);
         if (type_is_array(ftype)) {
            vcode_reg_t src_reg = lower_array_data(vals[i]);
            vcode_reg_t length_reg = lower_array_total_len(ftype, vals[i]);
            emit_copy(ptr_reg, src_reg, length_reg);
         }
         else if (type_is_record(ftype))
            emit_copy(ptr_reg, vals[i], VCODE_INVALID_REG);
         else
            emit_store_indirect(vals[i], ptr_reg);
      }

      return mem_reg;
   }
}

static vcode_reg_t lower_aggregate(tree_t expr, expr_ctx_t ctx)
{
   type_t type = tree_type(expr);

   if (type_is_record(type))
      return lower_record_aggregate(expr, false, lower_is_const(expr), ctx);

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
   type_t type = tree_type(value);
   vcode_reg_t record = lower_expr(value, ctx);

   const int index = lower_field_index(type, tree_ident(expr));

   if (lower_have_signal(record)) {
      if (ctx == EXPR_RVALUE) {
         vcode_reg_t count_reg = emit_const(vtype_offset(), type_width(type));
         return emit_record_ref(emit_vec_load(record, count_reg, false), index);
      }
      else {
         const netid_t offset = record_field_to_net(type, tree_ident(expr));
         return emit_addi(record, offset);
      }
   }
   else
      return emit_record_ref(record, index);
}

static vcode_reg_t lower_concat(tree_t expr, expr_ctx_t ctx)
{
   const int nparams = tree_params(expr);

   struct {
      tree_t      value;
      type_t      type;
      vcode_reg_t reg;
   } args[nparams];

   for (int i = 0; i < nparams; i++) {
      args[i].value = tree_value(tree_param(expr, i));
      args[i].type  = tree_type(args[i].value);
      args[i].reg   = lower_expr(args[i].value, ctx);
   }

   type_t type = tree_type(expr);
   type_t elem = type_elem(type);

   type_t scalar_elem = lower_elem_recur(elem);

   vcode_reg_t var_reg = VCODE_INVALID_REG;
   if (type_is_unconstrained(type)) {
      vcode_reg_t len = emit_const(vtype_offset(), 0);
      for (int i = 0; i < nparams; i++) {
         vcode_reg_t len_i;
         if (type_is_array(args[i].type))
            len_i = lower_array_len(args[i].type, 0, args[i].reg);
         else
            len_i = emit_const(vtype_offset(), 1);

         len = emit_add(len, len_i);
      }

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
      var_reg = emit_alloca(lower_type(scalar_elem),
                            lower_bounds(scalar_elem),
                            lower_array_total_len(type, VCODE_INVALID_REG));

   vcode_reg_t ptr = lower_array_data(var_reg);

   for (int i = 0; i < nparams; i++) {
      if (type_is_array(args[i].type)) {
         vcode_reg_t src_len =
            lower_array_total_len(args[i].type, args[i].reg);

         if (lower_have_signal(args[i].reg)) {
            vcode_reg_t data = lower_array_data(args[i].reg);
            args[i].reg = emit_vec_load(data, src_len, false);
         }

         emit_copy(ptr, lower_array_data(args[i].reg), src_len);
         if (i + 1 < nparams)
            ptr = emit_add(ptr, src_len);
      }
      else {
         emit_store_indirect(lower_reify(args[i].reg), ptr);
         if (i + 1 < nparams)
            ptr = emit_addi(ptr, 1);
      }
   }

   return var_reg;
}

static vcode_reg_t lower_new(tree_t expr, expr_ctx_t ctx)
{
   type_t type = type_access(tree_type(expr));

   tree_t value = tree_value(expr);
   type_t value_type = tree_type(value);

   if (type_is_array(type)) {
      vcode_reg_t init_reg = lower_expr(value, EXPR_RVALUE);
      vcode_reg_t length_reg = lower_array_total_len(value_type, init_reg);

      vcode_reg_t mem_reg = emit_new(lower_type(type_elem(type)), length_reg);
      vcode_reg_t raw_reg = emit_all(mem_reg);

      emit_copy(raw_reg, lower_array_data(init_reg), length_reg);

      if (!lower_const_bounds(type)) {
          // Need to allocate memory for both the array and its metadata
         vcode_reg_t meta_reg =
            lower_wrap_with_new_bounds(value_type, init_reg, raw_reg);
         vcode_reg_t result_reg = emit_new(lower_type(type), VCODE_INVALID_REG);
         emit_store_indirect(meta_reg, emit_all(result_reg));
         return result_reg;
      }
      else
         return mem_reg;
   }
   else if (type_is_record(type)) {
      vcode_reg_t result_reg = emit_new(lower_type(type), VCODE_INVALID_REG);
      vcode_reg_t all_reg = emit_all(result_reg);

      uint32_t hint = emit_storage_hint(all_reg, VCODE_INVALID_REG);
      vcode_reg_t init_reg = lower_expr(value, EXPR_RVALUE);
      vcode_clear_storage_hint(hint);

      emit_copy(all_reg, init_reg, VCODE_INVALID_REG);

      return result_reg;
   }
   else {
      vcode_reg_t result_reg = emit_new(lower_type(type), VCODE_INVALID_REG);
      vcode_reg_t all_reg = emit_all(result_reg);

      vcode_reg_t init_reg = lower_expr(value, EXPR_RVALUE);
      emit_store_indirect(lower_reify(init_reg), all_reg);

      return result_reg;
   }
}

static vcode_reg_t lower_all(tree_t all, expr_ctx_t ctx)
{
   vcode_reg_t access_reg = lower_reify_expr(tree_value(all));
   emit_null_check(access_reg);
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

   if (from_k == T_REAL && to_k == T_INTEGER) {
      vcode_type_t to_vtype = lower_type(to);
      vcode_reg_t cast = emit_cast(to_vtype, to_vtype, value_reg);
      lower_check_scalar_bounds(cast, to, expr, NULL);
      return cast;
   }
   else if (from_k == T_INTEGER && to_k == T_REAL)
      return emit_cast(lower_type(to), lower_bounds(to), value_reg);
   else if (type_is_array(to) && !lower_const_bounds(to)) {
      // Need to wrap in metadata
      return lower_wrap(from, value_reg);
   }
   else if (from_k == T_INTEGER && to_k == T_INTEGER) {
      // Possibly change width
      lower_check_scalar_bounds(value_reg, to, expr, NULL);
      return emit_cast(lower_type(to), lower_bounds(to), value_reg);
   }
   else {
      // No conversion to perform
      return value_reg;
   }
}

static const int lower_get_attr_dimension(tree_t expr)
{
   if (tree_params(expr) > 0)
      return assume_int(tree_value(tree_param(expr, 0))) - 1;
   else
      return 0;
}

static vcode_reg_t lower_image_map(type_t type)
{
   type_t base = type_base_recur(type);

   vcode_reg_t result = VCODE_INVALID_REG;
   switch (type_kind(base)) {
   case T_INTEGER:
   case T_REAL:
      break;

   case T_ENUM:
      {
         const int nlits = type_enum_literals(base);
         ident_t *map LOCAL = xmalloc(sizeof(ident_t) * nlits);
         for (int i = 0; i < nlits; i++) {
            // LRM specifies result is lowercase for enumerated types when
            // the value is a basic identifier
            const ident_t id = tree_ident(type_enum_literal(base, i));
            if (ident_char(id, 0) == '\'')
               map[i] = id;
            else
               map[i] = ident_downcase(id);
         }
         result = emit_enum_map(type_ident(base), nlits, map);
      }
      break;

   case T_PHYSICAL:
      {
         const int nunits = type_units(base);
         ident_t *map LOCAL = xmalloc(sizeof(ident_t) * nunits);
         int64_t *values LOCAL = xmalloc(sizeof(int64_t) * nunits);
         for (int i = 0; i < nunits; i++) {
            tree_t unit = type_unit(base, i);
            map[i] = tree_ident(unit);
            values[i] = assume_int(tree_value(unit));
         }
         result = emit_physical_map(type_ident(base), nunits, map, values);
      }
      break;

   default:
      fatal_trace("cannot generate image map for %s", type_pp(type));
   }

   return result;
}

static vcode_reg_t lower_attr_ref(tree_t expr, expr_ctx_t ctx)
{
   tree_t name = tree_name(expr);

   const predef_attr_t predef = tree_attr_int(expr, builtin_i, -1);
   switch (predef) {
   case ATTR_LEFT:
   case ATTR_RIGHT:
      {
         const bool left = predef == ATTR_LEFT;
         const int dim = lower_get_attr_dimension(expr);

         type_t type = tree_type(name);
         vcode_reg_t reg = VCODE_INVALID_REG;
         if (lower_const_bounds(type)) {
            range_t r = range_of(type, dim);
            reg = lower_expr(left ? r.left : r.right, EXPR_RVALUE);
         }
         else {
            vcode_reg_t array_reg = lower_param(name, NULL, PORT_IN);
            if (left)
               reg = lower_array_left(type, dim, array_reg);
            else
               reg = lower_array_right(type, dim, array_reg);
         }

         return emit_cast(lower_type(tree_type(expr)), VCODE_INVALID_TYPE, reg);
      }

   case ATTR_LOW:
   case ATTR_HIGH:
      {
         const bool low = predef == ATTR_LOW;
         const int dim = lower_get_attr_dimension(expr);

         type_t type = tree_type(name);

         if (tree_kind(name) == T_REF) {
            tree_t decl = tree_ref(name);
            if (tree_kind(decl) == T_TYPE_DECL && type_is_unconstrained(type))
               type = type_index_constr(type, dim);
         }

         vcode_reg_t reg = VCODE_INVALID_REG;
         if (!type_is_array(type) || lower_const_bounds(type)) {
            range_t r = range_of(type, dim);
            if (low)
               reg = lower_reify_expr(r.kind == RANGE_TO ? r.left : r.right);
            else
               reg = lower_reify_expr(r.kind == RANGE_TO ? r.right : r.left);
         }
         else {
            vcode_reg_t array_reg  = lower_reify_expr(name);
            vcode_reg_t left_reg   = lower_array_left(type, dim, array_reg);
            vcode_reg_t right_reg  = lower_array_right(type, dim, array_reg);
            vcode_reg_t downto_reg = lower_array_dir(type, dim, array_reg);
            if (low)
               reg = emit_select(downto_reg, right_reg, left_reg);
            else
               reg = emit_select(downto_reg, left_reg, right_reg);
         }

         return emit_cast(lower_type(tree_type(expr)), VCODE_INVALID_TYPE, reg);
      }

   case ATTR_LENGTH:
      {
         const int dim = lower_get_attr_dimension(expr);
         return emit_cast(lower_type(tree_type(expr)),
                          VCODE_INVALID_TYPE,
                          lower_array_len(tree_type(name), dim,
                                          lower_param(name, NULL, PORT_IN)));
      }

   case ATTR_ASCENDING:
      {
         type_t type = tree_type(name);
         const int dim = lower_get_attr_dimension(expr);
         if (lower_const_bounds(type))
            return emit_const(vtype_bool(),
                              direction_of(type, dim) == RANGE_TO);
         else
            return emit_not(lower_array_dir(type, dim,
                                            lower_param(name, NULL, PORT_IN)));
      }

   case ATTR_LAST_EVENT:
      {
         type_t name_type = tree_type(name);
         vcode_reg_t name_reg = lower_expr(name, EXPR_LVALUE);
         if (type_is_array(name_type))
            return emit_last_event(name_reg,
                                   lower_array_total_len(name_type, name_reg));
         else
            return emit_last_event(name_reg, VCODE_INVALID_REG);
      }

   case ATTR_EVENT:
      return lower_signal_flag(name, emit_event_flag);

   case ATTR_ACTIVE:
      return lower_signal_flag(name, emit_active_flag);

   case ATTR_LAST_VALUE:
      {
         type_t name_type = tree_type(name);
         vcode_reg_t len_reg  = VCODE_INVALID_REG;
         vcode_reg_t nets_reg = lower_expr(name, EXPR_LVALUE);
         if (type_is_array(name_type))
            len_reg = lower_array_total_len(name_type, nets_reg);
         return emit_vec_load(nets_reg, len_reg, true);
      }

   case ATTR_INSTANCE_NAME:
      return lower_name_attr(name, INSTANCE_NAME);

   case ATTR_PATH_NAME:
      return lower_name_attr(name, PATH_NAME);

   case ATTR_IMAGE:
      {
         tree_t value = tree_value(tree_param(expr, 0));
         vcode_reg_t map = lower_image_map(tree_type(value));
         tmp_alloc_used = true;
         return emit_image(lower_param(value, NULL, PORT_IN), map);
      }

   case ATTR_VALUE:
      {
         type_t name_type = tree_type(name);
         tree_t value = tree_value(tree_param(expr, 0));
         vcode_reg_t arg = lower_param(value, NULL, PORT_IN);
         vcode_reg_t map = lower_image_map(name_type);
         vcode_reg_t len = lower_array_len(tree_type(value), 0, arg);
         vcode_reg_t reg = emit_value(lower_array_data(arg), len, map);
         lower_check_scalar_bounds(reg, name_type, expr, NULL);
         return emit_cast(lower_type(name_type), lower_bounds(name_type), reg);
      }

   case ATTR_SUCC:
      {
         tree_t value = tree_value(tree_param(expr, 0));
         vcode_reg_t arg = lower_param(value, NULL, PORT_IN);
         return emit_addi(arg, 1);
      }

   case ATTR_PRED:
      {
         tree_t value = tree_value(tree_param(expr, 0));
         vcode_reg_t arg = lower_param(value, NULL, PORT_IN);
         return emit_addi(arg, -1);
      }

   case ATTR_LEFTOF:
      {
         tree_t value = tree_value(tree_param(expr, 0));
         vcode_reg_t arg = lower_param(value, NULL, PORT_IN);

         type_t type = tree_type(expr);
         const int dir =
            (type_is_enum(type) || direction_of(type, 0) == RANGE_TO) ? -1 : 1;
         return emit_addi(arg, dir);
      }

   case ATTR_RIGHTOF:
      {
         tree_t value = tree_value(tree_param(expr, 0));
         vcode_reg_t arg = lower_param(value, NULL, PORT_IN);

         type_t type = tree_type(expr);
         const int dir =
            (type_is_enum(type) || direction_of(type, 0) == RANGE_TO) ? 1 : -1;
         return emit_addi(arg, dir);
      }

   case ATTR_POS:
      {
         tree_t value = tree_value(tree_param(expr, 0));
         vcode_reg_t arg = lower_param(value, NULL, PORT_IN);

         type_t type = tree_type(expr);
         return emit_cast(lower_type(type), lower_bounds(type), arg);
      }

   case ATTR_VAL:
      {
         tree_t value = tree_value(tree_param(expr, 0));
         vcode_reg_t arg = lower_param(value, NULL, PORT_IN);

         type_t type = tree_type(expr);
         lower_check_scalar_bounds(arg, type, expr, NULL);
         return emit_cast(lower_type(type), lower_bounds(type), arg);
      }

   default:
      fatal("cannot lower attribute %s", istr(tree_ident(expr)));
   }
}

static vcode_reg_t lower_qualified(tree_t expr, expr_ctx_t ctx)
{
   tree_t value = tree_value(expr);

   type_t from_type = tree_type(value);
   type_t to_type = tree_type(expr);

   vcode_reg_t value_reg = lower_expr(value, ctx);

   if (type_is_array(to_type)) {
      const bool from_const = lower_const_bounds(from_type);
      const bool to_const = lower_const_bounds(to_type);

      if (to_const && !from_const)
         return lower_array_data(value_reg);
      else if (!to_const && from_const)
         return lower_wrap(from_type, value_reg);

   }

   return value_reg;
}

static vcode_reg_t lower_expr(tree_t expr, expr_ctx_t ctx)
{
   switch (tree_kind(expr)) {
   case T_FCALL:
      return lower_fcall(expr, ctx);
   case T_LITERAL:
      return lower_literal(expr, ctx);
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
   case T_ATTR_REF:
      return lower_attr_ref(expr, ctx);
   case T_QUALIFIED:
      return lower_qualified(expr, ctx);
   case T_OPEN:
      return VCODE_INVALID_REG;
   default:
      fatal_at(tree_loc(expr), "cannot lower expression kind %s",
               tree_kind_str(tree_kind(expr)));
   }
}

static void lower_cleanup_temp_objects(vcode_reg_t saved_heap)
{
   if (tmp_alloc_used) {
      emit_heap_restore(saved_heap);
      tmp_alloc_used = false;
   }
}

static void lower_assert(tree_t stmt)
{
   const int is_report = tree_attr_int(stmt, ident_new("is_report"), 0);

   vcode_reg_t severity = lower_reify_expr(tree_severity(stmt));

   vcode_reg_t value = VCODE_INVALID_REG;
   if (!is_report) {
      value = lower_reify_expr(tree_value(stmt));

      int64_t value_const;
      if (vcode_reg_const(value, &value_const) && value_const)
         return;
   }

   vcode_block_t message_bb = VCODE_INVALID_BLOCK;
   vcode_block_t exit_bb = VCODE_INVALID_BLOCK;

   vcode_reg_t message = VCODE_INVALID_REG, length = VCODE_INVALID_REG;
   vcode_reg_t saved_heap = VCODE_INVALID_REG;
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

      saved_heap = emit_heap_save();

      vcode_reg_t message_wrapped = lower_expr(m, EXPR_RVALUE);
      message = lower_array_data(message_wrapped);
      length  = lower_array_len(tree_type(m), 0, message_wrapped);
   }

   if (is_report)
      emit_report(message, length, severity);
   else
      emit_assert(value, message, length, severity);

   if (saved_heap != VCODE_INVALID_REG)
      lower_cleanup_temp_objects(saved_heap);

   if (exit_bb != VCODE_INVALID_BLOCK) {
      emit_jump(exit_bb);
      vcode_select_block(exit_bb);
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
   tree_t ref = on, decl = NULL;
   while (decl == NULL) {
      switch (tree_kind(ref)) {
      case T_REF:
         decl = tree_ref(ref);
         break;

      case T_ARRAY_REF:
      case T_ARRAY_SLICE:
         ref = tree_value(ref);
         break;

      default:
         // It is possible for constant folding to replace a signal with
         // a constant which will then appear in a sensitivity list so
         // just ignore it
         return;
      }
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
   if (tree_kind(on) == T_REF) {
      switch (tree_kind(decl)) {
      case T_SIGNAL_DECL:
         nets = lower_signal_ref(decl, EXPR_LVALUE);
         break;
      case T_PORT_DECL:
         if (tree_class(decl) != C_SIGNAL)
            return;
         nets = lower_param_ref(decl, EXPR_LVALUE);
         break;
      default:
         assert(false);
      }

      if (array) {
         type_t elem = type_elem(type);
         n_elems = lower_array_total_len(type, nets);
         if (type_is_record(elem))
            n_elems = emit_mul(n_elems,
                               emit_const(vtype_offset(), type_width(elem)));
      }
      else
         n_elems = emit_const(vtype_offset(), type_width(type));

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

   const bool has_delay = tree_has_delay(wait);
   const bool has_value = tree_has_value(wait);

   vcode_reg_t delay = VCODE_INVALID_REG;
   if (has_delay)
      delay = lower_expr(tree_delay(wait), EXPR_RVALUE);

   vcode_var_t remain = VCODE_INVALID_VAR;
   if (has_value && has_delay) {
      ident_t remain_i = ident_new("wait_remain");
      remain = vcode_find_var(remain_i);
      if (remain == VCODE_INVALID_VAR) {
         vcode_type_t time = vtype_time();
         remain = emit_var(time, time, remain_i, false);
      }

      vcode_reg_t now_reg = emit_fcall(ident_new("_std_standard_now"),
                                       vtype_time(), NULL, 0);
      vcode_reg_t abs_reg = emit_add(now_reg, delay);
      emit_store(abs_reg, remain);
   }

   vcode_block_t resume = emit_block();
   emit_wait(resume, delay);

   vcode_select_block(resume);

   if (has_value) {
      // Generate code to loop until condition is met

      vcode_reg_t until_reg = lower_reify_expr(tree_value(wait));

      vcode_reg_t timeout_reg = VCODE_INVALID_REG;
      vcode_reg_t done_reg = until_reg;
      if (has_delay) {
         vcode_reg_t remain_reg = emit_load(remain);
         vcode_reg_t now_reg = emit_fcall(ident_new("_std_standard_now"),
                                          vtype_time(), NULL, 0);
         timeout_reg = emit_sub(remain_reg, now_reg);

         vcode_reg_t expired_reg = emit_cmp(VCODE_CMP_EQ, timeout_reg,
                                            emit_const(vtype_time(), 0));
         done_reg = emit_or(expired_reg, until_reg);
      }

      vcode_block_t done_bb  = emit_block();
      vcode_block_t again_bb = emit_block();

      emit_cond(done_reg, done_bb, again_bb);

      vcode_select_block(again_bb);

      if (!is_static) {
         const int ntriggers = tree_triggers(wait);
         for (int i = 0; i < ntriggers; i++)
            lower_sched_event(tree_trigger(wait, i), is_static);
      }

      emit_wait(resume, timeout_reg);

      vcode_select_block(done_bb);
   }
}

static void lower_check_array_sizes(tree_t t, type_t ltype, type_t rtype,
                                    vcode_reg_t lval, vcode_reg_t rval)
{
   // TODO: for each dimension

   vcode_reg_t llen_reg = lower_array_len(ltype, 0, lval);
   vcode_reg_t rlen_reg = lower_array_len(rtype, 0, rval);

   emit_array_size(llen_reg, rlen_reg);
}

static void lower_find_matching_refs(tree_t ref, void *context)
{
   tree_t *decl = (tree_t *)context;
   if (tree_ref(ref) == *decl)
      *decl = NULL;
}

static bool lower_assign_can_use_storage_hint(tree_t stmt)
{
   // We cannot write directly into an array variable's storage if the RHS
   // references that array

   tree_t target = tree_target(stmt);

   tree_kind_t kind;
   while ((kind = tree_kind(target)) != T_REF) {
      switch (kind) {
      case T_ARRAY_REF:
      case T_ARRAY_SLICE:
         target = tree_value(target);
         break;

      default:
         return false;
      }
   }

   tree_t decl = tree_ref(target);
   tree_visit_only(tree_value(stmt), lower_find_matching_refs, &decl, T_REF);
   return decl != NULL;
}

static void lower_var_assign(tree_t stmt)
{
   tree_t value  = tree_value(stmt);
   tree_t target = tree_target(stmt);
   type_t type   = tree_type(target);

   const bool is_var_decl =
      tree_kind(target) == T_REF && tree_kind(tree_ref(target)) == T_VAR_DECL;
   const bool is_scalar = type_is_scalar(type);

   const int saved_heap = emit_heap_save();
   uint32_t hint = VCODE_INVALID_HINT;

   if (is_scalar || type_is_access(type)) {
      vcode_reg_t value_reg = lower_expr(value, EXPR_RVALUE);
      vcode_reg_t loaded_value = lower_reify(value_reg);
      vcode_var_t var = VCODE_INVALID_VAR;
      if (is_scalar)
         lower_check_scalar_bounds(loaded_value, type, stmt, NULL);
      if (is_var_decl
          && (var = lower_get_var(tree_ref(target))) != VCODE_INVALID_VAR)
         emit_store(loaded_value, lower_get_var(tree_ref(target)));
      else
         emit_store_indirect(loaded_value, lower_expr(target, EXPR_LVALUE));
   }
   else if (type_is_array(type)) {
      vcode_reg_t target_reg  = lower_expr(target, EXPR_LVALUE);
      vcode_reg_t count_reg   = lower_array_total_len(type, target_reg);
      vcode_reg_t target_data = lower_array_data(target_reg);

      if (lower_assign_can_use_storage_hint(stmt))
         hint = emit_storage_hint(target_data, count_reg);

      vcode_reg_t value_reg = lower_expr(value, EXPR_RVALUE);
      vcode_reg_t src_data = lower_array_data(value_reg);
      lower_check_array_sizes(stmt, type, tree_type(value),
                              target_reg, value_reg);

      if (lower_have_signal(src_data))
         src_data = emit_vec_load(src_data, count_reg, false);

      emit_copy(target_data, src_data, count_reg);
   }
   else {
      vcode_reg_t value_reg  = lower_expr(value, EXPR_RVALUE);
      vcode_reg_t target_reg = lower_expr(target, EXPR_LVALUE);

      if (lower_assign_can_use_storage_hint(stmt))
         hint = emit_storage_hint(target_reg, VCODE_INVALID_REG);

      emit_copy(target_reg, value_reg, VCODE_INVALID_REG);
   }

   if (hint != VCODE_INVALID_HINT)
      vcode_clear_storage_hint(hint);

   lower_cleanup_temp_objects(saved_heap);
}

static void lower_signal_assign(tree_t stmt)
{
   const int saved_heap = emit_heap_save();

   vcode_reg_t reject;
   if (tree_has_reject(stmt))
      reject = lower_reify_expr(tree_reject(stmt));
   else
      reject = emit_const(vtype_int(INT64_MIN, INT64_MAX), 0);

   tree_t target = tree_target(stmt);
   type_t target_type = tree_type(target);
   type_t part_type = target_type;

   const bool aggregate = tree_kind(target) == T_AGGREGATE;
   const int nparts = aggregate ? tree_assocs(target) : 1;

   vcode_reg_t nets[nparts];
   if (aggregate) {
      part_type = type_elem(target_type);
      for (int i = 0; i < nparts; i++)
         nets[i] = lower_expr(tree_value(tree_assoc(target, i)), EXPR_LVALUE);
   }
   else
      nets[0] = lower_expr(target, EXPR_LVALUE);

   vcode_reg_t nets_raw[nparts];
   for (int i = 0; i < nparts; i++) {
      if (nets[i] != VCODE_INVALID_REG)
         nets_raw[i] = lower_array_data(nets[i]);
      else
         nets_raw[i] = VCODE_INVALID_REG;
   }

   const int nwaveforms = tree_waveforms(stmt);
   for (int i = 0; i < nwaveforms; i++) {
      tree_t w = tree_waveform(stmt, i);
      tree_t wvalue = tree_value(w);

      vcode_reg_t rhs = lower_expr(wvalue, EXPR_RVALUE);

      if (type_is_scalar(target_type))
         lower_check_scalar_bounds(lower_reify(rhs), target_type, wvalue, NULL);
      else if (type_is_array(target_type))
         lower_check_array_sizes(wvalue, target_type, tree_type(wvalue),
                                 nparts > 1 ? VCODE_INVALID_REG : nets[0], rhs);

      vcode_reg_t after;
      if (tree_has_delay(w))
         after = lower_expr(tree_delay(w), EXPR_RVALUE);
      else
         after = emit_const(vtype_int(INT64_MIN, INT64_MAX), 0);

      vcode_reg_t count_reg = VCODE_INVALID_REG;
      if (type_is_array(part_type)) {
         type_t wtype = tree_type(wvalue);
         count_reg = lower_array_total_len(wtype, rhs);

         type_t elem = type_elem(wtype);
         if (type_is_record(elem))
            count_reg = emit_mul(count_reg,
                                 emit_const(vtype_offset(), type_width(elem)));
      }

      if (lower_have_signal(rhs))
         rhs = emit_vec_load(lower_array_data(rhs), count_reg, false);
      else if (type_is_array(target_type))
         rhs = lower_array_data(rhs);

      for (int i = 0; i < nparts; i++) {
         if (nets[i] == VCODE_INVALID_REG)
            ;
         else if (type_is_array(part_type)) {
            assert(i == 0);
            vcode_reg_t data_reg = lower_array_data(rhs);
            emit_sched_waveform(nets_raw[i], count_reg, data_reg,
                                reject, after);
         }
         else if (type_is_record(part_type)) {
            const int width = type_width(part_type);
            emit_sched_waveform(nets_raw[i], emit_const(vtype_offset(), width),
                                rhs, reject, after);
         }
         else {
            assert(i == 0 || vcode_reg_kind(rhs) == VCODE_TYPE_POINTER);
            emit_sched_waveform(nets_raw[i], emit_const(vtype_offset(), 1),
                                rhs, reject, after);

            if (i + 1 < nparts)
               rhs = emit_addi(rhs, 1);
         }
      }

      // All but the first waveform have zero reject time
      if (nwaveforms > 1 && tree_has_reject(stmt))
         reject = emit_const(vtype_int(INT64_MIN, INT64_MAX), 0);
   }

   lower_cleanup_temp_objects(saved_heap);
}

static vcode_reg_t lower_test_expr(tree_t value)
{
   const int saved_heap = emit_heap_save();
   vcode_reg_t test = lower_reify_expr(value);
   lower_cleanup_temp_objects(saved_heap);
   lower_cond_coverage(value, test);
   return test;
}

static void lower_if(tree_t stmt, loop_stack_t *loops)
{
   vcode_reg_t test = lower_test_expr(tree_value(stmt));

   const int nelses = tree_else_stmts(stmt);
   const int nstmts = tree_stmts(stmt);

   int64_t cval;
   if (vcode_reg_const(test, &cval)) {
      emit_comment("Condition of if statement line %d is always %s",
                   tree_loc(stmt)->first_line, cval ? "true" : "false");
      if (cval) {
         for (int i = 0; i < nstmts; i++)
            lower_stmt(tree_stmt(stmt, i), loops);
      }
      else {
         for (int i = 0; i < nelses; i++)
            lower_stmt(tree_else_stmt(stmt, i), loops);
      }

      return;
   }

   vcode_block_t btrue = emit_block();
   vcode_block_t bfalse = nelses > 0 ? emit_block() : VCODE_INVALID_BLOCK;
   vcode_block_t bmerge = nelses > 0 ? VCODE_INVALID_BLOCK : emit_block();

   emit_cond(test, btrue, nelses > 0 ? bfalse : bmerge);

   vcode_select_block(btrue);

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
         lower_check_scalar_bounds(result, type, value, NULL);
         emit_return(result);
      }
      else if (result_kind == VCODE_TYPE_UARRAY) {
         vcode_reg_t array = lower_expr(value, EXPR_RVALUE);

         if (vtype_kind(vcode_reg_type(array)) == VCODE_TYPE_UARRAY)
            emit_return(array);
         else
            emit_return(lower_wrap(type, lower_array_data(array)));
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

   const int saved_heap = emit_heap_save();

   ident_t builtin = tree_attr_str(decl, builtin_i);
   if (builtin != NULL) {
      lower_builtin(pcall, builtin);
      lower_cleanup_temp_objects(saved_heap);
      return;
   }

   ident_t name = lower_mangle_func(decl, vcode_unit_context());

   const int nest_depth = tree_attr_int(decl, nested_i, 0);
   const bool never_waits =
      tree_attr_int(decl, wait_level_i, WAITS_MAYBE) == WAITS_NO;
   const bool use_fcall =
      never_waits || vcode_unit_kind() == VCODE_UNIT_FUNCTION;

   const int nargs = tree_params(pcall);
   vcode_reg_t args[nargs];
   for (int i = 0; i < nargs; i++) {
      args[i] = lower_subprogram_arg(pcall, i);
      if (!use_fcall)
         vcode_heap_allocate(args[i]);
   }

   if (use_fcall) {
      if (nest_depth > 0) {
         const int hops = vcode_unit_depth() - nest_depth;
         emit_nested_fcall(name, VCODE_INVALID_TYPE, args, nargs, hops);
      }
      else
         emit_fcall(name, VCODE_INVALID_TYPE, args, nargs);
      lower_cleanup_temp_objects(saved_heap);
   }
   else {
      const int hops = nest_depth > 0 ? vcode_unit_depth() - nest_depth : 0;
      vcode_block_t resume_bb = emit_block();
      if (nest_depth > 0) {
         emit_nested_pcall(name, args, nargs, resume_bb, hops);
         vcode_select_block(resume_bb);
         emit_nested_resume(name, hops);
      }
      else {
         emit_pcall(name, args, nargs, resume_bb);
         vcode_select_block(resume_bb);
         emit_resume(name);
      }
   }
}

static void lower_for(tree_t stmt, loop_stack_t *loops)
{
   range_t r = tree_range(stmt, 0);
   vcode_reg_t left_reg  = lower_reify_expr(r.left);
   vcode_reg_t right_reg = lower_reify_expr(r.right);
   vcode_reg_t dir_reg   = lower_range_dir(r, 0);

   vcode_reg_t null_reg =
      lower_range_null(left_reg, right_reg, dir_reg, r.kind);

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

   vcode_type_t vtype  = vcode_reg_type(left_reg);
   vcode_type_t bounds = vtype;

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

   tree_t idecl = tree_decl(stmt, 0);
   ident_t ident = ident_prefix(tree_ident2(stmt), tree_ident(stmt), '.');
   tree_set_ident(idecl, ident);

   vcode_var_t ivar = emit_var(vtype, bounds, ident, false);
   lower_put_vcode_obj(idecl, ivar);

   vcode_reg_t init_reg = r.kind == RANGE_RDYN ? right_reg : left_reg;
   emit_store(init_reg, ivar);

   vcode_reg_t dirn_reg = lower_range_dir(r, 0);
   vcode_reg_t step_reg = emit_select(dirn_reg, emit_const(vtype, -1),
                                      emit_const(vtype, 1));

   vcode_block_t body_bb = emit_block();
   emit_jump(body_bb);
   vcode_select_block(body_bb);

   if (exit_bb == VCODE_INVALID_BLOCK)
      exit_bb = emit_block();

   loop_stack_t this = {
      .up      = loops,
      .name    = tree_ident(stmt),
      .test_bb = VCODE_INVALID_BLOCK,
      .exit_bb = exit_bb
   };

   const int nstmts = tree_stmts(stmt);
   for (int i = 0; i < nstmts; i++)
      lower_stmt(tree_stmt(stmt, i), &this);

   if (this.test_bb != VCODE_INVALID_BLOCK) {
      // Loop body contained a "next" statement
      if (!vcode_block_finished())
         emit_jump(this.test_bb);
      vcode_select_block(this.test_bb);
   }

   vcode_reg_t ireg     = emit_load(ivar);
   vcode_reg_t next_reg = emit_add(ireg, step_reg);
   emit_store(next_reg, ivar);

   vcode_reg_t final_reg =
      lower_reify_expr(r.kind == RANGE_RDYN ? r.left : r.right);

   vcode_reg_t done_reg = emit_cmp(VCODE_CMP_EQ, ireg, final_reg);
   emit_cond(done_reg, exit_bb, body_bb);

   vcode_select_block(exit_bb);
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

      vcode_reg_t test = lower_test_expr(tree_value(stmt));
      emit_cond(test, body_bb, exit_bb);
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

      vcode_reg_t result = lower_test_expr(tree_value(stmt));
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

static void lower_case_scalar(tree_t stmt, loop_stack_t *loops)
{
   const int nassocs = tree_assocs(stmt);

   vcode_block_t start_bb = vcode_active_block();
   vcode_block_t def_bb = VCODE_INVALID_BLOCK;

   int ncases = 0;
   for (int i = 0; i < nassocs; i++) {
      tree_t a = tree_assoc(stmt, i);
      switch (tree_subkind(a)) {
      case A_RANGE:
         {
            int64_t low, high;
            range_bounds(tree_range(a, 0), &low, &high);
            ncases += high - low + 1;
         }
         break;
      case A_NAMED:
         ncases++;
         break;
      }
   }

   vcode_block_t *blocks LOCAL = xcalloc(ncases * sizeof(vcode_block_t));
   vcode_reg_t *cases LOCAL = xcalloc(ncases * sizeof(vcode_reg_t));

   vcode_block_t exit_bb = emit_block();

   tree_t last = NULL;
   int cptr = 0;
   for (int i = 0; i < nassocs; i++) {
      tree_t a = tree_assoc(stmt, i);

      tree_t stmt = tree_value(a);
      vcode_block_t bb = stmt != last ? emit_block() : blocks[cptr - 1];

      switch (tree_subkind(a)) {
      case A_OTHERS:
         def_bb = bb;
         break;

      case A_NAMED:
         vcode_select_block(start_bb);
         cases[cptr]  = lower_reify_expr(tree_name(a));
         blocks[cptr] = bb;
         cptr++;
         break;

      case A_RANGE:
         {
            vcode_select_block(start_bb);

            range_t r = tree_range(a, 0);
            int64_t low, high;
            range_bounds(r, &low, &high);

            vcode_type_t vt = lower_type(tree_type(r.left));
            for (int64_t j = low; j <= high; j++) {
               cases[cptr]  = emit_const(vt, j);
               blocks[cptr] = bb;
               cptr++;
            }
         }
         break;
      }

      if (stmt != last) {
         vcode_select_block(bb);
         lower_stmt(stmt, loops);
         if (!vcode_block_finished())
            emit_jump(exit_bb);

         last = stmt;
      }
   }

   if (def_bb == VCODE_INVALID_BLOCK)
      def_bb = exit_bb;

   vcode_select_block(start_bb);
   vcode_reg_t value_reg = lower_reify_expr(tree_value(stmt));
   emit_case(value_reg, def_bb, cases, blocks, ncases);

   vcode_select_block(exit_bb);
}

static int64_t lower_case_find_choice_element(tree_t value, int depth)
{
   switch (tree_kind(value)) {
   case T_LITERAL:
      {
         assert(tree_subkind(value) == L_STRING);
         tree_t ch = tree_char(value, depth);
         return tree_pos(tree_ref(ch));
      }
      break;

   case T_AGGREGATE:
      {
         const int nassocs = tree_assocs(value);
         type_t type = tree_type(value);

         for (int i = 0; i < nassocs; i++) {
            tree_t a = tree_assoc(value, i);
            switch (tree_subkind(a)) {
            case A_NAMED:
               if (rebase_index(type, 0, assume_int(tree_name(a))) == depth)
                  return assume_int(tree_value(a));
               break;

            case A_POS:
               if (tree_pos(a) == (unsigned)depth)
                  return assume_int(tree_value(a));
               break;

            case A_OTHERS:
               return assume_int(tree_value(a));
            }
         }
      }
      break;

   case T_REF:
      {
         tree_t decl = tree_ref(value);
         assert(tree_kind(decl) == T_CONST_DECL);
         return lower_case_find_choice_element(tree_value(decl), depth);
      }
      break;

   case T_ARRAY_SLICE:
      {
         tree_t base = tree_value(value);
         range_t r = tree_range(value, 0);
         const int64_t rleft = assume_int(r.left);
         const int64_t offset = rebase_index(tree_type(base), 0, rleft);
         return lower_case_find_choice_element(base, depth + offset);
      }

   case T_CONCAT:
      {
         tree_t left = tree_value(tree_param(value, 0));
         tree_t right = tree_value(tree_param(value, 1));

         range_t lr = range_of(tree_type(left), 0);
         int64_t left_len;
         if (!folded_length(lr, &left_len))
            fatal_at(tree_loc(left), "cannot determine length of left hand "
                     "side of concatenation");

         if (depth < left_len)
            return lower_case_find_choice_element(left, depth);
         else
            return lower_case_find_choice_element(right, depth - left_len);
      }

   default:
      fatal_at(tree_loc(value), "unsupported tree type %s in case choice",
               tree_kind_str(tree_kind(value)));
   }

   fatal_at(tree_loc(value), "cannot find element %d in choice", depth);
}

static void lower_case_add_branch(case_state_t *where, int left, int right,
                                  int depth, int dirmul,
                                  tree_t value, tree_t stmts)
{
   const int n = left + (depth * dirmul);

   if ((dirmul == -1 && n < right) || (dirmul == 1 && n > right)) {
      if (where->stmts != NULL)
         fatal_at(tree_loc(value), "duplicate choice in case statement");

      assert(where->narcs == 0);
      where->stmts = stmts;
   }
   else {
      const int64_t this = lower_case_find_choice_element(value, depth);

      for (int i = 0; i < where->narcs; i++) {
         if (where->arcs[i].value == this) {
            lower_case_add_branch(where->arcs[i].next,
                                  left, right, depth + 1, dirmul, value, stmts);
            return;
         }
      }

      case_state_t *next = xmalloc(sizeof(case_state_t));
      next->stmts = NULL;
      next->narcs = 0;

      assert(where->narcs < MAX_CASE_ARCS);
      case_arc_t *arc = &(where->arcs[(where->narcs)++]);
      arc->value = this;
      arc->next  = next;

      lower_case_add_branch(next, left, right, depth + 1,
                            dirmul, value, stmts);
   }
}

static void lower_case_alloc_basic_blocks(case_state_t *state)
{
   state->block = emit_block();
   for (int i = 0; i < state->narcs; i++)
      lower_case_alloc_basic_blocks(state->arcs[i].next);
}

static void lower_case_emit_state_code(case_state_t *state,
                                       vcode_reg_t value, int depth,
                                       vcode_block_t exit_bb,
                                       vcode_block_t others_bb,
                                       loop_stack_t *loops)
{
   vcode_select_block(state->block);
   emit_comment("Case state depth %d", depth);

   if (state->stmts != NULL) {
      lower_stmt(state->stmts, loops);
      if (!vcode_block_finished())
         emit_jump(exit_bb);
   }
   else {
      vcode_reg_t ptr_reg = emit_addi(value, depth);
      vcode_reg_t loaded  = lower_reify(ptr_reg);

      vcode_block_t blocks[state->narcs];
      vcode_reg_t cases[state->narcs];

      for (int i = 0; i < state->narcs; i++) {
         vcode_select_block(state->block);

         blocks[i] = state->arcs[i].next->block;
         cases[i]  = emit_const(vcode_reg_type(loaded), state->arcs[i].value);

         lower_case_emit_state_code(state->arcs[i].next, value,
                                    depth + 1, exit_bb, others_bb, loops);
      }

      vcode_select_block(state->block);
      emit_case(loaded, others_bb, cases, blocks, state->narcs);
   }
}

static void lower_case_cleanup(case_state_t *state, bool root)
{
   for (int i = 0; i < state->narcs; i++)
      lower_case_cleanup(state->arcs[i].next, false);
   if (!root)
      free(state);
}

static void lower_case_array(tree_t stmt, loop_stack_t *loops)
{
   // Case staments on arrays are implemented by building a decision tree
   // where each state is mapped to a basic block

   vcode_block_t exit_bb   = emit_block();
   vcode_block_t others_bb = exit_bb;

   vcode_reg_t val = lower_expr(tree_value(stmt), EXPR_RVALUE);
   type_t type = tree_type(tree_value(stmt));

   const int nassocs = tree_assocs(stmt);

   case_state_t root = {
      .stmts = NULL,
      .narcs = 0,
      .block = VCODE_INVALID_BLOCK
   };

   range_t r = range_of(type, 0);
   const int64_t left  = assume_int(r.left);
   const int64_t right = assume_int(r.right);
   const int dirmul = (r.kind == RANGE_DOWNTO) ? -1 : 1;

   int others_assoc = -1;
   for (int i = 0; i < nassocs; i++) {
      tree_t a = tree_assoc(stmt, i);
      switch (tree_subkind(a)) {
      case A_NAMED:
         lower_case_add_branch(&root, left, right, 0, dirmul,
                               tree_name(a), tree_value(a));
         break;

      case A_OTHERS:
         others_assoc = i;
         others_bb = emit_block();
         break;

      default:
         assert(false);
      }
   }

   vcode_reg_t data_ptr = lower_array_data(val);

   lower_case_alloc_basic_blocks(&root);

   emit_jump(root.block);

   lower_case_emit_state_code(&root, data_ptr, 0, exit_bb, others_bb, loops);
   lower_case_cleanup(&root, true);

   if (others_assoc != -1) {
      vcode_select_block(others_bb);
      lower_stmt(tree_value(tree_assoc(stmt, others_assoc)), loops);
      if (!vcode_block_finished())
         emit_jump(exit_bb);
   }

   vcode_select_block(exit_bb);
}

static void lower_case(tree_t stmt, loop_stack_t *loops)
{
   if (type_is_scalar(tree_type(tree_value(stmt))))
      lower_case_scalar(stmt, loops);
   else
      lower_case_array(stmt, loops);
}

static void lower_stmt(tree_t stmt, loop_stack_t *loops)
{
   if (vcode_block_finished()) {
      warn_at(tree_loc(stmt), "statement is unreachable");
      return;
   }

   const int cover_tag = tree_attr_int(stmt, stmt_tag_i, -1);
   if (cover_tag != -1)
      emit_cover_stmt(cover_tag);

   emit_debug_info(tree_loc(stmt));

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
   case T_FOR:
      lower_for(stmt, loops);
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

static void lower_check_indexes(type_t type, vcode_reg_t array, tree_t hint)
{
   SAVE_DEBUG_INFO;
   emit_debug_info(tree_loc(hint));

   const int ndims = array_dimension(type);
   for (int i = 0; i < ndims; i++) {
      type_t index = index_type_of(type, i);

      vcode_type_t vbounds = lower_bounds(index);

      vcode_reg_t left_reg  = lower_array_left(type, i, array);
      vcode_reg_t right_reg = lower_array_right(type, i, array);

      if (type_is_enum(index))
         emit_index_check(left_reg, right_reg, vbounds, BOUNDS_INDEX_TO);
      else {
         range_t rindex = range_of(index, 0);
         bounds_kind_t bkind  = rindex.kind == RANGE_TO
            ? BOUNDS_INDEX_TO : BOUNDS_INDEX_DOWNTO;

         vcode_reg_t rlow_reg, rhigh_reg;
         if (lower_const_bounds(type)) {
            range_t r = range_of(type, i);
            rlow_reg  = r.kind == RANGE_TO ? left_reg : right_reg;
            rhigh_reg = r.kind == RANGE_TO ? right_reg : left_reg;
         }
         else {
            vcode_reg_t dir_reg = lower_array_dir(type, i, array);
            rlow_reg  = emit_select(dir_reg, right_reg, left_reg);
            rhigh_reg = emit_select(dir_reg, left_reg, right_reg);
         }

         if (lower_is_const(rindex.left) && lower_is_const(rindex.right)) {
            emit_index_check(rlow_reg, rhigh_reg, vbounds, bkind);
         }
         else {
            vcode_reg_t bleft  = lower_reify_expr(rindex.left);
            vcode_reg_t bright = lower_reify_expr(rindex.right);

            vcode_reg_t bmin = bkind == BOUNDS_INDEX_TO ? bleft : bright;
            vcode_reg_t bmax = bkind == BOUNDS_INDEX_TO ? bright : bleft;

            emit_dynamic_index_check(rlow_reg, rhigh_reg, bmin, bmax, bkind);
         }
      }
   }
}

static void lower_protected_init(tree_t decl, vcode_reg_t pstruct)
{
   tree_t body = type_body(tree_type(decl));

   int nvar = 0;
   const int ndecls = tree_decls(body);
   for (int i = 0; i < ndecls; i++) {
      tree_t d = tree_decl(body, i);
      const tree_kind_t kind = tree_kind(d);
      if (kind != T_VAR_DECL && kind != T_FILE_DECL)
         continue;

      type_t type = tree_type(d);

      vcode_reg_t field = emit_record_ref(pstruct, nvar++);
      if (type_is_protected(type)) {
         lower_protected_init(d, field);
      }
      else if (type_is_file(type))
         emit_store_indirect(emit_null(lower_type(type)), field);
      else {
         vcode_reg_t value = lower_expr(tree_value(d), EXPR_RVALUE);

         if (type_is_array(type)) {
            vcode_reg_t count_reg =
               lower_array_total_len(type, VCODE_INVALID_REG);
            lower_check_indexes(type, value, d);
            lower_check_array_sizes(d, type, tree_type(tree_value(d)),
                                    VCODE_INVALID_REG, value);
            emit_copy(field, lower_array_data(value), count_reg);
         }
         else if (type_is_record(type))
            emit_copy(field, value, VCODE_INVALID_REG);
         else if (type_is_scalar(type)) {
            value = lower_reify(value);
            lower_check_scalar_bounds(value, type, decl, NULL);
            emit_store_indirect(value, field);
         }
         else
            emit_store_indirect(value, field);
      }
   }
}

static void lower_var_decl(tree_t decl)
{
   type_t type = tree_type(decl);
   vcode_type_t vtype = lower_type(type);
   vcode_type_t vbounds = lower_bounds(type);
   vcode_var_t var = emit_var(vtype, vbounds, tree_ident(decl),
                              tree_kind(decl) == T_CONST_DECL);
   lower_put_vcode_obj(decl, var);

   if (type_is_protected(type))
      lower_protected_init(decl, emit_index(var, VCODE_INVALID_REG));

   if (!tree_has_value(decl))
      return;

   emit_debug_info(tree_loc(decl));

   vcode_reg_t dest_reg  = VCODE_INVALID_REG;
   vcode_reg_t count_reg = VCODE_INVALID_REG;
   uint32_t hint = VCODE_INVALID_HINT;

   if (type_is_record(type)) {
      dest_reg = emit_index(var, VCODE_INVALID_REG);
      hint = emit_storage_hint(dest_reg, VCODE_INVALID_REG);
   }
   else if (type_is_array(type) && !type_is_unconstrained(type)) {
      count_reg = lower_array_total_len(type, VCODE_INVALID_REG);

      if (!lower_const_bounds(type)) {
         type_t scalar_elem = lower_elem_recur(type);
         dest_reg = emit_alloca(lower_type(scalar_elem),
                                lower_bounds(scalar_elem),
                                count_reg);
         emit_store(lower_wrap(type, dest_reg), var);

         if (vcode_unit_kind() == VCODE_UNIT_PROCEDURE)
            vcode_heap_allocate(dest_reg);
      }
      else
         dest_reg = emit_index(var, VCODE_INVALID_REG);

      hint = emit_storage_hint(dest_reg, count_reg);
   }

   vcode_reg_t value = lower_expr(tree_value(decl), EXPR_RVALUE);

   if (hint != VCODE_INVALID_HINT)
      vcode_clear_storage_hint(hint);

   if (type_is_array(type)) {
      lower_check_indexes(type, value, decl);

      if (type_is_unconstrained(type)) {
         vcode_heap_allocate(value);
         emit_store(value, var);
      }
      else {
         lower_check_array_sizes(decl, type, tree_type(tree_value(decl)),
                                 VCODE_INVALID_REG, value);
         emit_copy(dest_reg, lower_array_data(value), count_reg);
      }
   }
   else if (type_is_record(type)) {
      emit_copy(dest_reg, value, VCODE_INVALID_REG);
   }
   else if (type_is_scalar(type)) {
      value = lower_reify(value);
      lower_check_scalar_bounds(value, type, decl, NULL);
      emit_store(value, var);
   }
   else if (type_is_access(type))
      emit_store(lower_reify(value), var);
   else
      emit_store(value, var);
}

static bool lower_resolution_func(type_t type, vcode_res_fn_t **data,
                                  size_t *max_elems, vcode_res_elem_t *rparent)
{
   if (rparent == NULL && type_kind(type) == T_SUBTYPE
       && type_has_resolution(type)) {
      tree_t rdecl = tree_ref(type_resolution(type));
      ident_t rfunc = lower_mangle_func(rdecl, vcode_unit_context());
      vcode_type_t rtype = lower_type(type);

      const bool is_record = type_is_record(type);

      type_t uarray_param = type_param(tree_type(rdecl), 0);
      range_t r = range_of(type_index_constr(uarray_param, 0), 0);
      vcode_res_elem_t parent = {
         .name     = rfunc,
         .type     = rtype,
         .ileft    = assume_int(r.left),
         .kind     = is_record ? RES_RECORD : RES_SCALAR,
         .boundary = is_record
      };
      return lower_resolution_func(type, data, max_elems, &parent);
   }
   else if (type_is_array(type))
      return lower_resolution_func(type_elem(type), data, max_elems, rparent);
   else if (type_is_record(type)) {
      const int nfields = type_fields(type);
      for (int i = 0; i < nfields; i++) {
         type_t ftype = tree_type(type_field(type, i));
         if (!lower_resolution_func(ftype, data, max_elems, rparent))
            return false;
      }

      return true;
   }
   else if (rparent != NULL) {
      if (*data == NULL) {
         *data = xcalloc(sizeof(vcode_res_fn_t) + sizeof(vcode_res_elem_t));
         *max_elems = 1;
      }
      else if ((*data)->count == *max_elems) {
         *max_elems *= 4;
         const size_t newsz =
            sizeof(vcode_res_fn_t) + *max_elems * sizeof(vcode_res_elem_t);
         *data = xrealloc(*data, newsz);
      }

      (*data)->element[(*data)->count++] = *rparent;
      rparent->boundary = false;
      return true;
   }
   else
      return false;
}

static void lower_signal_decl(tree_t decl)
{
   int nnets = tree_nets(decl);
   bool is_package_signal = false;
   ident_t name = tree_ident(decl);

   if (nnets == 0 && !tree_attr_int(decl, null_range_i, 0)) {
      // Signal declared in a package
      nnets = type_width(tree_type(decl));
      is_package_signal = true;
      name = ident_new(package_signal_path_name(name));
   }

   type_t type = tree_type(decl);
   vcode_type_t ltype = lower_type(type);
   vcode_type_t bounds = lower_bounds(type);

   const bool can_use_shadow =
      !is_package_signal
      && lower_signal_sequential_nets(decl)
      && !tree_attr_int(decl, partial_map_i, 0);

   vcode_var_t shadow = VCODE_INVALID_VAR;
   if (can_use_shadow) {
      vcode_type_t shadow_type   = ltype;
      vcode_type_t shadow_bounds = bounds;
      if (type_is_array(type)) {
         shadow_type = vtype_elem(ltype);
         shadow_bounds = vtype_elem(bounds);
      }

      shadow = emit_var(vtype_pointer(shadow_type), shadow_bounds,
                        ident_prefix(ident_new("resolved"), name, '_'),
                        true);
   }

   netid_t *nets = xmalloc(sizeof(netid_t) * nnets);
   for (int i = 0; i < nnets; i++)
      nets[i] = is_package_signal ? NETID_INVALID : tree_net(decl, i);

   const bool from_package = tree_flags(decl) & TREE_F_PACKAGE_SIGNAL;
   assert(!(is_package_signal && from_package));

   vcode_type_t stype = vtype_signal(ltype);
   vcode_signal_t sig = emit_signal(stype, bounds, name, shadow,
                                    nets, nnets, from_package);
   lower_put_vcode_obj(decl, sig);

   if (nnets == 0 || is_package_signal)
      return;

   // Internal signals that were generated from ports will not have
   // an initial value
   if (tree_has_value(decl)) {
      tree_t value = tree_value(decl);
      vcode_reg_t init_reg = lower_expr(value, EXPR_RVALUE);
      if (type_is_array(tree_type(value))) {
         lower_check_array_sizes(decl, type, tree_type(value),
                                 VCODE_INVALID_REG, init_reg);
         init_reg = lower_array_data(init_reg);
      }

      vcode_res_fn_t *resolution = NULL;
      size_t max_elems = 0;
      if (!lower_resolution_func(type, &resolution, &max_elems, NULL)) {
         free(resolution);
         resolution = NULL;
      }

      emit_set_initial(sig, init_reg, resolution);
   }

   // Identify signals which potentially need 'LAST_VALUE
   if (tree_flags(decl) & TREE_F_LAST_VALUE)
      emit_needs_last_value(sig);

   if (shadow != VCODE_INVALID_VAR)
      emit_resolved_address(shadow, sig);
}

static void lower_file_decl(tree_t decl)
{
   type_t type = tree_type(decl);
   vcode_type_t vtype = lower_type(type);
   vcode_var_t var = emit_var(vtype, vtype, tree_ident(decl), false);
   lower_put_vcode_obj(decl, var);

   emit_store(emit_null(vtype), var);

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

static void lower_protected_constants(tree_t body)
{
   const int ndecls = tree_decls(body);
   for (int i = 0; i < ndecls; i++) {
      tree_t decl = tree_decl(body, i);
      if (tree_kind(decl) != T_CONST_DECL)
         continue;
      else if (type_is_scalar(tree_type(decl)))
         continue;
      else if (tree_attr_int(decl, deferred_i, 0))
         continue;
      else
         lower_var_decl(decl);
   }
}

static void lower_decl(tree_t decl)
{
   switch (tree_kind(decl)) {
   case T_CONST_DECL:
      if (type_is_scalar(tree_type(decl)) || tree_attr_int(decl, deferred_i, 0))
          break;
      // Fall-through

   case T_VAR_DECL:
      lower_var_decl(decl);
      break;

   case T_SIGNAL_DECL:
      lower_signal_decl(decl);
      break;

   case T_FILE_DECL:
      lower_file_decl(decl);
      break;

   case T_HIER:
   case T_TYPE_DECL:
   case T_ALIAS:
   case T_FUNC_DECL:
   case T_PROC_DECL:
   case T_ATTR_SPEC:
   case T_ATTR_DECL:
   case T_COMPONENT:
   case T_USE:
   case T_SPEC:
      break;

   default:
      fatal_at(tree_loc(decl), "cannot lower decl kind %s",
               tree_kind_str(tree_kind(decl)));
   }
}

static void lower_finished(void)
{
   vcode_opt();

   if (verbose != NULL) {
      if (*verbose == '\0' || strstr(istr(vcode_unit_name()), verbose) != NULL)
         vcode_dump();
   }
}

static void lower_protected_body(tree_t body)
{
   int nvars = 0;
   const int ndecls = tree_decls(body);
   for (int i = 0; i < ndecls; i++) {
      tree_t d = tree_decl(body, i);
      const tree_kind_t kind = tree_kind(d);
      if (kind == T_VAR_DECL || kind == T_FILE_DECL)
         tree_add_attr_int(d, prot_field_i, nvars++);
   }

   lower_decls(body, vcode_active_unit());

   for (int i = 0; i < ndecls; i++) {
      tree_t decl = tree_decl(body, i);
      if (tree_kind(decl) != T_USE)
         tree_remove_attr(decl, prot_field_i);
   }
}

static void lower_decls(tree_t scope, vcode_unit_t context)
{
   const tree_kind_t scope_kind = tree_kind(scope);
   const bool nested = scope_kind == T_FUNC_BODY || scope_kind == T_PROC_BODY
      || scope_kind == T_PROCESS;

   const int nest_depth = tree_attr_int(scope, nested_i, 0);

   // Lower declarations in two passes with subprograms after signals,
   // variables, constants, etc.

   const int ndecls = tree_decls(scope);

   for (int i = 0; i < ndecls; i++) {
      tree_t d = tree_decl(scope, i);
      const tree_kind_t kind = tree_kind(d);
      if (mode == LOWER_THUNK
          && (kind == T_SIGNAL_DECL || kind == T_PROT_BODY))
         continue;
      else if (kind == T_FUNC_BODY || kind == T_PROC_BODY || kind == T_FUNC_DECL
               || kind == T_PROC_DECL) {
         if (nested)
            tree_add_attr_int(d, nested_i, nest_depth + 1);
         lower_mangle_func(d, context);
      }
      else if (kind == T_PROT_BODY)
         lower_protected_constants(d);
      else if (scope_kind != T_PROT_BODY) {
         lower_decl(d);
         if (nested)
            tree_add_attr_int(d, nested_i, nest_depth + 1);
      }
   }

   for (int i = 0; i < ndecls; i++) {
      tree_t d = tree_decl(scope, i);
      const tree_kind_t kind = tree_kind(d);
      if (kind != T_FUNC_BODY && kind != T_PROC_BODY && kind != T_PROT_BODY)
         continue;

      vcode_block_t bb = vcode_active_block();

      if (kind == T_PROT_BODY && mode == LOWER_THUNK)
         continue;

      switch (kind) {
      case T_FUNC_BODY: lower_func_body(d, context); break;
      case T_PROC_BODY: lower_proc_body(d, context); break;
      case T_PROT_BODY: lower_protected_body(d); break;
      default: break;
      }

      vcode_select_unit(context);
      vcode_select_block(bb);
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

      vcode_type_t vtype, vbounds;
      switch (tree_class(p)) {
      case C_SIGNAL:
         vtype = lower_signal_type(type);
         vbounds = vtype;
         break;

      case C_VARIABLE:
      case C_DEFAULT:
      case C_CONSTANT:
         {
            if (type_is_array(type) && lower_const_bounds(type)) {
               type_t elem = lower_elem_recur(type);
               vtype = vtype_pointer(lower_type(elem));
               vbounds = lower_bounds(elem);
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
            if ((mode == PORT_OUT || mode == PORT_INOUT)
                && !type_is_array(type) && !type_is_record(type))
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
      lower_put_vcode_obj(p, reg);
      if (has_subprograms)
         tree_add_attr_int(p, nested_i, vcode_unit_depth());
   }
}

static vcode_unit_t lower_find_subprogram(ident_t name, vcode_unit_t context)
{
   vcode_unit_t vu = vcode_find_unit(name);
   if (vu == NULL)
      return false;

   vcode_state_t state;
   vcode_state_save(&state);
   vcode_select_unit(vu);

   const bool same_context = vcode_unit_context() == context
      || (mode == LOWER_THUNK && vcode_unit_kind() == VCODE_UNIT_THUNK);

   vcode_state_restore(&state);
   return same_context ? vu : NULL;
}

static void lower_proc_body(tree_t body, vcode_unit_t context)
{
   const bool never_waits =
      tree_attr_int(body, wait_level_i, WAITS_MAYBE) == WAITS_NO;

   vcode_select_unit(context);

   ident_t name = lower_mangle_func(body, context);
   vcode_unit_t vu = lower_find_subprogram(name, context);
   if (vu != NULL)
      return;

   tree_add_attr_str(body, mangled_i, name);

   if (never_waits)
      vu = emit_function(name, context, VCODE_INVALID_TYPE);
   else
      vu = emit_procedure(name, context);

   emit_debug_info(tree_loc(body));

   const bool has_subprograms = lower_has_subprograms(body);
   lower_subprogram_ports(body, has_subprograms);

   lower_decls(body, vu);

   const int nstmts = tree_stmts(body);
   for (int i = 0; i < nstmts; i++)
      lower_stmt(tree_stmt(body, i), NULL);

   if (!vcode_block_finished())
      emit_return(VCODE_INVALID_REG);

   lower_finished();

   if (vcode_unit_has_undefined())
      vcode_unit_unref(vu);
}

static vcode_unit_t lower_func_body(tree_t body, vcode_unit_t context)
{
   vcode_select_unit(context);

   vcode_type_t vtype = lower_func_result_type(body);

   ident_t name = lower_mangle_func(body, context);
   vcode_unit_t vu = lower_find_subprogram(name, context);
   if (vu != NULL)
      return vu;

   tree_add_attr_str(body, mangled_i, name);
   vu = emit_function(name, context, vtype);
   emit_debug_info(tree_loc(body));

   const bool has_subprograms = lower_has_subprograms(body);
   lower_subprogram_ports(body, has_subprograms);

   lower_decls(body, vu);

   const int nstmts = tree_stmts(body);
   for (int i = 0; i < nstmts; i++)
      lower_stmt(tree_stmt(body, i), NULL);

   lower_finished();

   if (vcode_unit_has_undefined()) {
      vcode_unit_unref(vu);
      return NULL;
   }
   else
      return vu;
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

         const tree_kind_t kind = tree_kind(*decl);
         if (kind == T_ALIAS)
            return lower_driver_nets(tree_value(*decl), decl, all_nets,
                                     all_length, driven_nets, driven_length,
                                     has_non_const, proc);
         else if (kind != T_SIGNAL_DECL || tree_nets(*decl) == 0)
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
            type_t type = tree_type(t);
            const int stride =
               type_is_array(type) ? type_width(type_elem(type)) : 1;

            vcode_reg_t idx =
               emit_mul(lower_array_ref_offset(t, *driven_nets),
                        emit_const(vtype_offset(), stride));

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

         range_t r = tree_range(t, 0);
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

   case T_RECORD_REF:
      {
         tree_t value = tree_value(t);
         if (!lower_driver_nets(value, decl, all_nets, all_length, driven_nets,
                                driven_length, has_non_const, proc))
            return false;
         else if (*driven_nets == VCODE_INVALID_REG)
            return false;
         else if (*has_non_const)
            return true;

         type_t rtype = tree_type(value);
         const netid_t offset = record_field_to_net(rtype, tree_ident(t));

         vcode_reg_t field_nets = emit_addi(*driven_nets, offset);

         *driven_nets   = field_nets;
         *driven_length = type_width(tree_type(t));
      }
      break;

   case T_LITERAL:
   case T_OPEN:
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
   emit_debug_info(tree_loc(proc));

   lower_decls(proc, vu);
   tree_visit(proc, lower_driver_fn, proc);

   vcode_block_t reset_bb = vcode_active_block();

   vcode_block_t start_bb = emit_block();
   vcode_select_block(start_bb);

   const int nstmts = tree_stmts(proc);
   for (int i = 0; i < nstmts; i++)
      lower_stmt(tree_stmt(proc, i), NULL);

   if (!vcode_block_finished())
      emit_jump(start_bb);

   vcode_select_block(reset_bb);
   emit_return(VCODE_INVALID_REG);

   lower_finished();
}

static vcode_unit_t lower_elab(tree_t unit)
{
   vcode_unit_t context = emit_context(tree_ident(unit));
   emit_debug_info(tree_loc(unit));

   lower_decls(unit, context);

   emit_return(VCODE_INVALID_REG);

   lower_finished();

   const int nstmts = tree_stmts(unit);
   for (int i = 0; i < nstmts; i++) {
      tree_t s = tree_stmt(unit, i);
      assert(tree_kind(s) == T_PROCESS);
      lower_process(s, context);
   }

   return context;
}

static vcode_unit_t lower_pack_body(tree_t unit)
{
   vcode_unit_t context = emit_context(tree_ident(unit));

   lower_decls(unit, context);

   emit_return(VCODE_INVALID_REG);

   lower_finished();
   return context;
}

static vcode_unit_t lower_package(tree_t unit)
{
   vcode_unit_t context = emit_context(tree_ident(unit));

   lower_decls(unit, context);

   emit_return(VCODE_INVALID_REG);

   lower_finished();
   return context;
}

static void lower_set_verbose(void)
{
   static bool set = false;
   if (!set) {
      const char *venv = getenv("NVC_LOWER_VERBOSE");
      if (venv != NULL)
         verbose = isalpha((int)venv[0]) || venv[0] == ':' ? venv : "";
      else
         verbose = opt_get_str("dump-vcode");
   }
}

vcode_unit_t lower_unit(tree_t unit)
{
   lower_set_verbose();

   vcode_objs = hash_new(4096, true);
   mode = LOWER_NORMAL;
   tmp_alloc_used = false;

   vcode_unit_t context = NULL;
   switch (tree_kind(unit)) {
   case T_ELAB:
      context = lower_elab(unit);
      break;
   case T_PACK_BODY:
      context = lower_pack_body(unit);
      break;
   case T_PACKAGE:
      context = lower_package(unit);
      break;
   default:
      fatal("cannot lower unit kind %s to vcode",
            tree_kind_str(tree_kind(unit)));
   }

   vcode_close();

   hash_free(vcode_objs);
   vcode_objs = NULL;

   return context;
}

vcode_unit_t lower_thunk(tree_t expr)
{
   lower_set_verbose();

   vcode_unit_t context = emit_context(thunk_i);

   vcode_select_unit(context);
   mode = LOWER_THUNK;
   tmp_alloc_used = false;

   vcode_type_t vtype = VCODE_INVALID_TYPE;
   ident_t name_i;
   if (tree_kind(expr) == T_FCALL) {
      tree_t decl = tree_ref(expr);
      if (tree_has_type(decl))
         vtype = lower_func_result_type(decl);
      name_i = tree_ident(expr);
   }
   else
      name_i = thunk_i;

   if (vtype == VCODE_INVALID_TYPE)
      vtype = lower_type(tree_type(expr));

   vcode_unit_t thunk = emit_thunk(name_i, context, vtype);

   vcode_reg_t result_reg = lower_expr(expr, EXPR_RVALUE);
   if (type_is_scalar(tree_type(expr)))
      emit_return(emit_cast(vtype, vtype, result_reg));
   else
      emit_return(result_reg);

   lower_finished();
   vcode_unit_unref(context);

   if (vcode_unit_has_undefined()) {
      vcode_unit_unref(thunk);
      return NULL;
   }

   vcode_close();
   return thunk;
}

vcode_unit_t lower_func(tree_t body)
{
   lower_set_verbose();

   vcode_unit_t context = emit_context(thunk_i);

   vcode_select_unit(context);
   vcode_objs = hash_new(128, true);
   mode = LOWER_THUNK;
   tmp_alloc_used = false;

   vcode_unit_t vu = lower_func_body(body, context);
   vcode_close();

   hash_free(vcode_objs);
   vcode_objs = NULL;

   return vu;
}
