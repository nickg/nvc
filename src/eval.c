//
//  Copyright (C) 2013-2022  Nick Gasson
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
#include "eval.h"
#include "hash.h"
#include "ident.h"
#include "lib.h"
#include "opt.h"
#include "phase.h"
#include "prim.h"
#include "rt/mspace.h"
#include "tree.h"
#include "type.h"
#include "vcode.h"

#include <assert.h>
#include <stdint.h>
#include <stdlib.h>
#include <stdarg.h>
#include <inttypes.h>
#include <string.h>
#include <math.h>

struct _eval {
   eval_flags_t  flags;
   hash_t       *link_map;
   lower_fn_t    lower_fn;
   void         *lower_ctx;
   mspace_t     *mspace;
};

////////////////////////////////////////////////////////////////////////////////
// Vcode interpreter

#define ITER_LIMIT 100000

typedef enum {
   VALUE_INVALID,
   VALUE_REAL,
   VALUE_INTEGER,
   VALUE_POINTER,
   VALUE_UARRAY,
   VALUE_CARRAY,
   VALUE_RECORD,
   VALUE_ACCESS,
   VALUE_CONTEXT,
   VALUE_DEBUG_LOCUS,
} value_kind_t;

typedef struct _value value_t;

struct _value {
   value_kind_t kind;
   uint32_t     length;
   union {
      double        real;
      int64_t       integer;
      value_t      *pointer;
      eval_frame_t *context;
      tree_t        debug;
   };
};

STATIC_ASSERT(sizeof(value_t) == 16);

struct _eval_frame {
   eval_frame_t *context;
   unsigned      nvars;
   value_t      *vars[0];
};

typedef struct {
   int            result;
   tree_t         hint;
   eval_flags_t   flags;
   bool           failed;
   int            iterations;
   int            op;
   eval_frame_t  *frame;
   value_t      **regs;
   eval_t        *eval;
} eval_state_t;

#define EVAL_WARN(state, op, ...) do {                                  \
      if ((state)->flags & EVAL_WARN) {                                 \
         if ((op) != -1)                                                \
            warn_at(vcode_get_loc(op), __VA_ARGS__);                    \
         else                                                           \
            warnf(__VA_ARGS__);                                         \
      }                                                                 \
   } while (0)

#define EVAL_ASSERT_VALUE(op, value, expect) do {                       \
      if (unlikely((value)->kind != expect))                            \
         eval_assert_fail(op, value, #value, #expect,                   \
                          __FILE__, __LINE__);                          \
   } while (0)

#define EVAL_ASSERT_VALID(op, value) do {                               \
      if (unlikely((value)->kind == VALUE_INVALID))                     \
         eval_assert_fail(op, value, #value, NULL, __FILE__, __LINE__); \
   } while (0)

#define FOR_EACH_REPR(repr, macro) do {                 \
      switch ((repr)) {                                 \
      case VCODE_REPR_U1:                               \
      case VCODE_REPR_U8: macro(uint8_t); break;        \
      case VCODE_REPR_I8: macro(int8_t); break;         \
      case VCODE_REPR_U16: macro(uint16_t); break;      \
      case VCODE_REPR_I16: macro(int16_t); break;       \
      case VCODE_REPR_U32: macro(uint32_t); break;      \
      case VCODE_REPR_I32: macro(int32_t); break;       \
      case VCODE_REPR_U64: macro(uint64_t); break;      \
      case VCODE_REPR_I64: macro(int64_t); break;       \
      }                                                 \
   } while (0)

static void eval_vcode(eval_state_t *state);
static tree_t eval_value_to_tree(value_t *value, type_t type, const loc_t *loc);

static vcode_unit_t eval_find_unit(ident_t func_name, eval_flags_t flags)
{
   vcode_unit_t vcode = vcode_find_unit(func_name);
   if (vcode == NULL) {
      ident_t strip_type_suffix = ident_until(func_name, '(');
      ident_t unit_name = ident_runtil(strip_type_suffix, '.');
      ident_t lib_name = ident_until(strip_type_suffix, '.');

      if (unit_name == lib_name)
         unit_name = func_name;

      if (flags & EVAL_VERBOSE)
         notef("loading vcode for %s", istr(unit_name));

      tree_t unit = lib_get_qualified(unit_name);
      if (unit != NULL && tree_kind(unit) == T_PACKAGE)
         (void)body_of(unit);  // Make sure body is loaded

      vcode = vcode_find_unit(func_name);
      if (vcode == NULL)
         fatal("failed to load vcode for %s", istr(func_name));
   }

   return vcode;
}

static int eval_dump(text_buf_t *tb, value_t *value)
{
   switch (value->kind) {
   case VALUE_INTEGER:
      tb_printf(tb, "%"PRIi64, value->integer);
      return 1;
   case VALUE_REAL:
      tb_printf(tb, "%lf", value->real);
      return 1;
   case VALUE_POINTER:
   case VALUE_ACCESS:
      tb_printf(tb, "*");
      if (value->pointer == NULL)
         tb_printf(tb, "NULL");
      else
         eval_dump(tb, &(value->pointer[-1]));
      return 1;
   case VALUE_CARRAY:
      {
         tb_printf(tb, "[");
         unsigned offset = 1;
         while (offset < value->length + 1) {
            if (offset > 1) tb_printf(tb, ",");
            offset += eval_dump(tb, &(value[offset]));
         }
         tb_printf(tb, "]");
         return offset;
      }
   case VALUE_UARRAY:
      {
         tb_printf(tb, "#[");
         int total = 1;
         for (unsigned dim = 0; dim < value[0].length; dim++) {
            if (dim > 0) tb_printf(tb, ", ");
            const int left   = value[1 + dim*2].integer;
            const int length = value[1 + dim*2 + 1].integer;
            const int right  =
               length < 0 ? left + length + 1 : left + length - 1;
            if (length < 0)
               tb_printf(tb, "%d downto %d", left, right);
            else
               tb_printf(tb, "%d to %d", left, right);
            total *= abs(length);
         }
         tb_printf(tb, " : ");
         for (int i = 0; i < total; i++) {
            if (i > 0) tb_printf(tb, ",");
            eval_dump(tb, &(value->pointer[i]));
         }
         tb_printf(tb, "]");
         return 1 + value->length * 2;
      }
   case VALUE_RECORD:
      {
         tb_printf(tb, "{");
         unsigned offset = 1;
         while (offset < value->length + 1) {
            if (offset > 1) tb_printf(tb, ",");
            offset += eval_dump(tb, &(value[offset]));
         }
         tb_printf(tb, "}");
         return offset;
      }
   case VALUE_CONTEXT:
      tb_printf(tb, "<CONTEXT>");
      return 1;
   default:
      tb_printf(tb, "<INVALID>");
      return 1;
   }
}

static void eval_dump_frame(text_buf_t *tb, eval_frame_t *frame)
{
   assert(frame->nvars == vcode_count_vars());

   for (unsigned i = 0; i < frame->nvars; i++) {
      tb_printf(tb, "\n%-20s : ", istr(vcode_var_name(i)));
      eval_dump(tb, frame->vars[i]);
   }
}

__attribute__((noreturn))
static void eval_assert_fail(int op, value_t *value, const char *value_str,
                             const char *expect, const char *file, int line)
{
   vcode_dump_with_mark(op, NULL, NULL);
   if (expect == NULL)
      fatal_trace("Expected %s to have valid value (at %s:%d)",
                  value_str, file, line);
   else {
      LOCAL_TEXT_BUF tb = tb_new();
      eval_dump(tb, value);
      fatal_trace("Expected %s to have kind %s but is %s (at %s:%d)",
                  value_str, expect, tb_get(tb), file, line);
   }
}

static value_t *eval_alloc(int count, eval_state_t *state)
{
   if (count == 0)
      return NULL;

   return mspace_alloc_array(state->eval->mspace, count, sizeof(value_t));
}

static void eval_make_pointer_to(value_t *dst, value_t *src)
{
   dst->kind = VALUE_POINTER;

   if (src == NULL)
      dst->pointer = NULL;
   else if (src->kind == VALUE_CARRAY || src->kind == VALUE_RECORD)
      dst->pointer = src + 1;   // Array or record data follows header
   else
      dst->pointer = src;
}

static int eval_slots_for_type(vcode_type_t vtype)
{
   switch (vtype_kind(vtype)) {
   case VCODE_TYPE_CARRAY:
      return 1 + vtype_size(vtype) * eval_slots_for_type(vtype_elem(vtype));
   case VCODE_TYPE_UARRAY:
      return 1 + vtype_dims(vtype) * 2;
   case VCODE_TYPE_RECORD:
      {
         int sum = 1;
         const int nfields = vtype_fields(vtype);
         for (int i = 0; i < nfields; i++)
            sum += eval_slots_for_type(vtype_field(vtype, i));
         return sum;
      }
   default:
      return 1;
   }
}

static int eval_setup_var(vcode_type_t vtype, value_t *value)
{
   switch (vtype_kind(vtype)) {
   case VCODE_TYPE_INT:
   case VCODE_TYPE_OFFSET:
      value->kind = VALUE_INTEGER;
      value->integer = 0;
      return 1;

   case VCODE_TYPE_REAL:
      value->kind = VALUE_REAL;
      value->real = 0.0;
      return 1;

   case VCODE_TYPE_POINTER:
   case VCODE_TYPE_ACCESS:
   case VCODE_TYPE_CONTEXT:
   case VCODE_TYPE_FILE:
      value->kind = VALUE_POINTER;
      value->pointer = NULL;
      return 1;

   case VCODE_TYPE_CARRAY:
      {
         const int nelems = vtype_size(vtype);
         vcode_type_t elem = vtype_elem(vtype);
         int offset = 1;
         for (int i = 0; i < nelems; i++)
            offset += eval_setup_var(elem, value + offset);

         value[0].kind = VALUE_CARRAY;
         value[0].length = offset - 1;

         return offset;
      }

   case VCODE_TYPE_RECORD:
      {
         const int nfields = vtype_fields(vtype);
         int offset = 1;
         for (unsigned i = 0; i < nfields; i++)
            offset += eval_setup_var(vtype_field(vtype, i), value + offset);

         value[0].kind = VALUE_RECORD;
         value[0].length = offset - 1;

         return offset;
      }

   case VCODE_TYPE_UARRAY:
      {
         const int ndims = vtype_dims(vtype);

         value[0].kind    = VALUE_UARRAY;
         value[0].length  = ndims;
         value[0].pointer = NULL;

         for (int i = 0; i < ndims; i++) {
            value[i*2 + 1].kind = VALUE_INTEGER;
            value[i*2 + 1].integer = 0;
            value[i*2 + 2].kind = VALUE_INTEGER;
            value[i*2 + 2].integer = 0;
         }

         return ndims*2 + 1;
      }

   default:
      fatal_trace("cannot evaluate variables with type %d", vtype_kind(vtype));
   }
}

static void eval_setup_state(eval_state_t *state, eval_frame_t *context)
{
   const int nvars = vcode_count_vars();
   const int nregs = vcode_count_regs();

   state->regs = xcalloc_array(nregs, sizeof(value_t *));
   state->frame = mspace_alloc_flex(state->eval->mspace, sizeof(eval_frame_t),
                                    nvars, sizeof(value_t *));
   state->frame->nvars = nvars;
   state->frame->context = context;

   int nrslots = 0;
   for (int i = 0; i < nregs; i++)
      nrslots += eval_slots_for_type(vcode_reg_type(i));

   int nvslots = 0;
   for (int i = 0; i < nvars; i++)
      nvslots += eval_slots_for_type(vcode_var_type(i));

   value_t *vslots = eval_alloc(nvslots, state);
   value_t *rslots = eval_alloc(nrslots, state);

   int vnext = 0;
   for (int i = 0; i < nvars; i++) {
      vcode_type_t vtype = vcode_var_type(i);
      value_t *value = state->frame->vars[i] = &(vslots[vnext]);
      vnext += eval_setup_var(vtype, value);
   }
   assert(vnext == nvslots);

   int rnext = 0;
   for (int i = 0; i < nregs; i++) {
      state->regs[i] = &(rslots[rnext]);
      rnext += eval_slots_for_type(vcode_reg_type(i));
   }
   assert(rnext == nrslots);
}

static void eval_cleanup_state(eval_state_t *state)
{
   state->frame = NULL;

   free(state->regs);
   state->regs = NULL;
}

static value_t *eval_get_reg(vcode_reg_t reg, eval_state_t *state)
{
   return state->regs[reg];
}

static value_t *eval_get_var(vcode_var_t var, eval_frame_t *frame)
{
   assert(var < frame->nvars);
   return frame->vars[var];
}

static int64_t eval_value_cmp(value_t *lhs, value_t *rhs)
{
   assert(lhs->kind == rhs->kind);
   switch (lhs->kind) {
   case VALUE_INTEGER:
      return lhs->integer - rhs->integer;

   case VALUE_REAL:
      {
         const double diff = lhs->real - rhs->real;
         if (diff < 0.0)
            return -1;
         else if (diff > 0.0)
            return 1;
         else
            return 0;
      }

   case VALUE_POINTER:
   case VALUE_ACCESS:
      return lhs->pointer - rhs->pointer;

   default:
      fatal_trace("invalid value type %d in %s", lhs->kind, __func__);
   }
}

static void eval_message(value_t *text, value_t *length, value_t *severity,
                         const loc_t *loc, const char *prefix)
{
   const char *levels[] = {
      "Note", "Warning", "Error", "Failure"
   };

   EVAL_ASSERT_VALUE(-1, text, VALUE_POINTER);

   char *copy = NULL ;
   if (length->integer > 0) {
      copy = xmalloc(length->integer + 1);
      for (int i = 0; i < length->integer; i++)
         copy[i] = text->pointer[i].integer;
      copy[length->integer] = '\0';
   }

   void (*fn)(const loc_t *loc, const char *fmt, ...) = fatal_at;

   switch (severity->integer) {
   case SEVERITY_NOTE:    fn = note_at; break;
   case SEVERITY_WARNING: fn = warn_at; break;
   case SEVERITY_ERROR:
   case SEVERITY_FAILURE: fn = error_at; break;
   }

   (*fn)(loc, "%s %s: %s", prefix, levels[severity->integer],
         copy ?: "Assertion violation");
   free(copy);
}

static void eval_branch(int op, vcode_block_t target, eval_state_t *state)
{
   if (++(state->iterations) >= ITER_LIMIT) {
      EVAL_WARN(state, op, "iteration limit reached while evaluating %s",
                state->hint ? istr(tree_ident(state->hint)) : "call");
      state->failed = true;
      return;
   }

   vcode_select_block(target);
   state->op = 0;
}

static void eval_op_const(int op, eval_state_t *state)
{
   value_t *dst = eval_get_reg(vcode_get_result(op), state);
   dst->kind    = VALUE_INTEGER;
   dst->integer = vcode_get_value(op);
}

static void eval_op_const_real(int op, eval_state_t *state)
{
   value_t *dst = eval_get_reg(vcode_get_result(op), state);
   dst->kind = VALUE_REAL;
   dst->real = vcode_get_real(op);
}

static void eval_op_return(int op, eval_state_t *state)
{
   if (vcode_count_args(op) > 0)
      state->result = vcode_get_arg(op, 0);
}

static void eval_op_not(int op, eval_state_t *state)
{
   value_t *dst = eval_get_reg(vcode_get_result(op), state);
   value_t *src = eval_get_reg(vcode_get_arg(op, 0), state);
   dst->kind    = VALUE_INTEGER;
   dst->integer = !(src->integer);
}

static void eval_op_add(int op, eval_state_t *state)
{
   vcode_reg_t result = vcode_get_result(op);
   value_t *dst = eval_get_reg(result, state);
   value_t *lhs = eval_get_reg(vcode_get_arg(op, 0), state);
   value_t *rhs = eval_get_reg(vcode_get_arg(op, 1), state);

   switch (lhs->kind) {
   case VALUE_INTEGER:
      dst->kind    = VALUE_INTEGER;
      dst->integer = lhs->integer + rhs->integer;
      break;

   case VALUE_REAL:
      dst->kind = VALUE_REAL;
      dst->real = lhs->real + rhs->real;
      break;

   default:
      fatal_trace("invalid value type in %s", __func__);
   }
}

static void eval_op_trap_add(int op, eval_state_t *state)
{
   vcode_reg_t result = vcode_get_result(op);
   value_t *dst = eval_get_reg(result, state);
   value_t *lhs = eval_get_reg(vcode_get_arg(op, 0), state);
   value_t *rhs = eval_get_reg(vcode_get_arg(op, 1), state);

   EVAL_ASSERT_VALUE(op, lhs, VALUE_INTEGER);
   EVAL_ASSERT_VALUE(op, rhs, VALUE_INTEGER);

   dst->kind = VALUE_INTEGER;

   bool overflow = false;
   vtype_repr_t repr = vtype_repr(vcode_reg_type(result));

#define ADD_OVERFLOW(type) do {                                 \
      type i1 = lhs->integer, i2 = rhs->integer, i0;            \
      overflow = __builtin_add_overflow(i1, i2, &i0);           \
      dst->integer = i0;                                        \
   } while (0)

   FOR_EACH_REPR(repr, ADD_OVERFLOW);

   if (overflow) {
      value_t *locus = eval_get_reg(vcode_get_arg(op, 2), state);
      EVAL_ASSERT_VALUE(op, locus, VALUE_DEBUG_LOCUS);

      error_at(vcode_get_loc(op), "result of %"PRIi64" + %"PRIi64
               " cannot be represented as %s", lhs->integer, rhs->integer,
               type_pp(tree_type(locus->debug)));
      state->failed = true;
   }
}

static void eval_op_sub(int op, eval_state_t *state)
{
   value_t *dst = eval_get_reg(vcode_get_result(op), state);
   value_t *lhs = eval_get_reg(vcode_get_arg(op, 0), state);
   value_t *rhs = eval_get_reg(vcode_get_arg(op, 1), state);

   switch (lhs->kind) {
   case VALUE_INTEGER:
      dst->kind    = VALUE_INTEGER;
      dst->integer = lhs->integer - rhs->integer;
      break;

   case VALUE_REAL:
      dst->kind = VALUE_REAL;
      dst->real = lhs->real - rhs->real;
      break;

   default:
      fatal_trace("invalid value type in %s", __func__);
   }
}

static void eval_op_trap_sub(int op, eval_state_t *state)
{
   vcode_reg_t result = vcode_get_result(op);
   value_t *dst = eval_get_reg(result, state);
   value_t *lhs = eval_get_reg(vcode_get_arg(op, 0), state);
   value_t *rhs = eval_get_reg(vcode_get_arg(op, 1), state);

   EVAL_ASSERT_VALUE(op, lhs, VALUE_INTEGER);
   EVAL_ASSERT_VALUE(op, rhs, VALUE_INTEGER);

   dst->kind = VALUE_INTEGER;

   bool overflow = false;
   vtype_repr_t repr = vtype_repr(vcode_reg_type(result));

#define SUB_OVERFLOW(type) do {                                 \
      type i1 = lhs->integer, i2 = rhs->integer, i0;            \
      overflow = __builtin_sub_overflow(i1, i2, &i0);           \
      dst->integer = i0;                                        \
   } while (0)

   FOR_EACH_REPR(repr, SUB_OVERFLOW);

   if (overflow) {
      value_t *locus = eval_get_reg(vcode_get_arg(op, 2), state);
      EVAL_ASSERT_VALUE(op, locus, VALUE_DEBUG_LOCUS);

      error_at(vcode_get_loc(op), "result of %"PRIi64" - %"PRIi64
               " cannot be represented as %s", lhs->integer, rhs->integer,
               type_pp(tree_type(locus->debug)));
      state->failed = true;
   }
}

static void eval_op_mul(int op, eval_state_t *state)
{
   value_t *dst = eval_get_reg(vcode_get_result(op), state);
   value_t *lhs = eval_get_reg(vcode_get_arg(op, 0), state);
   value_t *rhs = eval_get_reg(vcode_get_arg(op, 1), state);

   switch (lhs->kind) {
   case VALUE_INTEGER:
      dst->kind    = VALUE_INTEGER;
      dst->integer = lhs->integer * rhs->integer;
      break;

   case VALUE_REAL:
      dst->kind = VALUE_REAL;
      dst->real = lhs->real * rhs->real;
      break;

   default:
      fatal_trace("invalid value type in %s", __func__);
   }
}

static void eval_op_trap_mul(int op, eval_state_t *state)
{
   vcode_reg_t result = vcode_get_result(op);
   value_t *dst = eval_get_reg(result, state);
   value_t *lhs = eval_get_reg(vcode_get_arg(op, 0), state);
   value_t *rhs = eval_get_reg(vcode_get_arg(op, 1), state);

   EVAL_ASSERT_VALUE(op, lhs, VALUE_INTEGER);
   EVAL_ASSERT_VALUE(op, rhs, VALUE_INTEGER);

   dst->kind = VALUE_INTEGER;

   bool overflow = false;
   vtype_repr_t repr = vtype_repr(vcode_reg_type(result));

#define MUL_OVERFLOW(type) do {                                 \
      type i1 = lhs->integer, i2 = rhs->integer, i0;            \
      overflow = __builtin_mul_overflow(i1, i2, &i0);           \
      dst->integer = i0;                                        \
   } while (0)

   FOR_EACH_REPR(repr, MUL_OVERFLOW);

   if (overflow) {
      value_t *locus = eval_get_reg(vcode_get_arg(op, 2), state);
      EVAL_ASSERT_VALUE(op, locus, VALUE_DEBUG_LOCUS);

      error_at(vcode_get_loc(op), "result of %"PRIi64" * %"PRIi64
               " cannot be represented as %s", lhs->integer, rhs->integer,
               type_pp(tree_type(locus->debug)));
      state->failed = true;
   }
}

static void eval_op_zero_check(int op, eval_state_t *state)
{
   value_t *denom = eval_get_reg(vcode_get_arg(op, 0), state);

   EVAL_ASSERT_VALUE(op, denom, VALUE_INTEGER);

   if (denom->integer == 0) {
      error_at(vcode_get_loc(op), "division by zero");
      state->failed = true;
   }
}

static void eval_op_div(int op, eval_state_t *state)
{
   value_t *dst = eval_get_reg(vcode_get_result(op), state);
   value_t *lhs = eval_get_reg(vcode_get_arg(op, 0), state);
   value_t *rhs = eval_get_reg(vcode_get_arg(op, 1), state);

   switch (lhs->kind) {
   case VALUE_INTEGER:
      assert(rhs->integer != 0);
      dst->kind    = VALUE_INTEGER;
      dst->integer = lhs->integer / rhs->integer;
      break;

   case VALUE_REAL:
      dst->kind = VALUE_REAL;
      dst->real = lhs->real / rhs->real;
      break;

   default:
      fatal_trace("invalid value type in %s", __func__);
   }
}

static void eval_op_mod(int op, eval_state_t *state)
{
   value_t *dst = eval_get_reg(vcode_get_result(op), state);
   value_t *lhs = eval_get_reg(vcode_get_arg(op, 0), state);
   value_t *rhs = eval_get_reg(vcode_get_arg(op, 1), state);

   switch (lhs->kind) {
   case VALUE_INTEGER:
      if (rhs->integer == 0) {
         error_at(vcode_get_loc(op), "division by zero");
         state->failed = true;
      }
      else {
         const int64_t numer = lhs->integer;
         const int64_t denom = rhs->integer;
         int64_t r = numer % denom;
         if ((r > 0 && denom < 0) || (r < 0 && denom > 0))
            r = r + denom;

         dst->kind    = VALUE_INTEGER;
         dst->integer = r;
      }
      break;

   default:
      fatal_trace("invalid value type in %s", __func__);
   }
}

static void eval_op_rem(int op, eval_state_t *state)
{
   value_t *dst = eval_get_reg(vcode_get_result(op), state);
   value_t *lhs = eval_get_reg(vcode_get_arg(op, 0), state);
   value_t *rhs = eval_get_reg(vcode_get_arg(op, 1), state);

   switch (lhs->kind) {
   case VALUE_INTEGER:
      if (rhs->integer == 0) {
         error_at(vcode_get_loc(op), "division by zero");
         state->failed = true;
      }
      else {
         dst->kind = VALUE_INTEGER;
         dst->integer =
            lhs->integer - (lhs->integer / rhs->integer) * rhs->integer;
      }
      break;

   default:
      fatal_trace("invalid value type in %s", __func__);
   }
}

static void eval_op_exponent_check(int op, eval_state_t *state)
{
   value_t *exp = eval_get_reg(vcode_get_arg(op, 0), state);
   EVAL_ASSERT_VALUE(op, exp, VALUE_INTEGER);

   if (exp->integer < 0) {
      if (state->flags & EVAL_BOUNDS)
         error_at(vcode_get_loc(op), "negative exponent %"PRIi64" only "
                  "allowed for floating-point types", exp->integer);
      else
         EVAL_WARN(state, state->op, "negative exponent prevents constant folding");
      state->failed = true;
   }
}

static void eval_op_exp(int op, eval_state_t *state)
{
   value_t *dst = eval_get_reg(vcode_get_result(op), state);
   value_t *lhs = eval_get_reg(vcode_get_arg(op, 0), state);
   value_t *rhs = eval_get_reg(vcode_get_arg(op, 1), state);

   switch (lhs->kind) {
   case VALUE_INTEGER:
      EVAL_ASSERT_VALUE(op, rhs, VALUE_INTEGER);
      dst->kind = VALUE_INTEGER;
      dst->integer = ipow(lhs->integer, rhs->integer);
      break;
   case VALUE_REAL:
      EVAL_ASSERT_VALUE(op, rhs, VALUE_REAL);
      dst->kind = VALUE_REAL;
      dst->real = pow(lhs->real, rhs->real);
      break;
   default:
      fatal_trace("invalid value type in %s", __func__);
      break;
   }
}

static void eval_op_cmp(int op, eval_state_t *state)
{
   value_t *dst = eval_get_reg(vcode_get_result(op), state);
   value_t *lhs = eval_get_reg(vcode_get_arg(op, 0), state);
   value_t *rhs = eval_get_reg(vcode_get_arg(op, 1), state);

   dst->kind = VALUE_INTEGER;

   switch (vcode_get_cmp(op)) {
   case VCODE_CMP_EQ:
      dst->integer = eval_value_cmp(lhs, rhs) == 0;
      break;
   case VCODE_CMP_NEQ:
      dst->integer = eval_value_cmp(lhs, rhs) != 0;
      break;
   case VCODE_CMP_GT:
      dst->integer = eval_value_cmp(lhs, rhs) > 0;
      break;
   case VCODE_CMP_GEQ:
      dst->integer = eval_value_cmp(lhs, rhs) >= 0;
      break;
   case VCODE_CMP_LT:
      dst->integer = eval_value_cmp(lhs, rhs) < 0;
      break;
   case VCODE_CMP_LEQ:
      dst->integer = eval_value_cmp(lhs, rhs) <= 0;
      break;
   default:
      vcode_dump();
      fatal_trace("cannot handle comparison");
   }
}

static void eval_op_cast(int op, eval_state_t *state)
{
   value_t *dst = eval_get_reg(vcode_get_result(op), state);
   value_t *src = eval_get_reg(vcode_get_arg(op, 0), state);

   switch (vtype_kind(vcode_get_type(op))) {
   case VCODE_TYPE_INT:
   case VCODE_TYPE_OFFSET:
      dst->kind = VALUE_INTEGER;
      switch (src->kind) {
      case VALUE_INTEGER: dst->integer = src->integer; break;
      case VALUE_REAL: dst->integer = (int64_t)round(src->real); break;
      default: break;
      }
      break;

   case VCODE_TYPE_REAL:
      dst->kind = VALUE_REAL;
      switch (src->kind) {
      case VALUE_INTEGER: dst->real = (double)src->integer; break;
      case VALUE_REAL: dst->real = src->real; break;
      default: break;
      }
      break;

   default:
      vcode_dump();
      fatal("cannot handle destination type in cast");
   }
}

static void eval_op_neg(int op, eval_state_t *state)
{
   value_t *dst = eval_get_reg(vcode_get_result(op), state);
   value_t *src = eval_get_reg(vcode_get_arg(op, 0), state);

   switch (src->kind) {
   case VALUE_INTEGER:
      dst->kind    = VALUE_INTEGER;
      dst->integer = -(src->integer);
      break;

   case VALUE_REAL:
      dst->kind = VALUE_REAL;
      dst->real = -(src->real);
      break;

   default:
      fatal_trace("invalid value type in %s", __func__);
   }
}

static void eval_op_abs(int op, eval_state_t *state)
{
   value_t *dst = eval_get_reg(vcode_get_result(op), state);
   value_t *src = eval_get_reg(vcode_get_arg(op, 0), state);

   switch (src->kind) {
   case VALUE_INTEGER:
      dst->kind    = VALUE_INTEGER;
      dst->integer = llabs(src->integer);
      break;

   case VALUE_REAL:
      dst->kind = VALUE_REAL;
      dst->real = fabs(src->real);
      break;

   default:
      fatal_trace("invalid value type in %s", __func__);
   }
}

static void eval_copy_value(value_t *dst, value_t *src)
{
   switch (src->kind) {
   case VALUE_POINTER:
   case VALUE_REAL:
   case VALUE_INTEGER:
   case VALUE_ACCESS:
   case VALUE_CONTEXT:
      *dst = *src;
      break;
   case VALUE_UARRAY:
      for (unsigned i = 0; i < src->length * 2 + 1; i++)
         dst[i] = src[i];
      break;
   default:
      fatal_trace("cannot copy value kind %d", src->kind);
   }
}

static void eval_int_to_string(value_t *dst, value_t *src, eval_state_t *state)
{
   char *buf LOCAL = xasprintf("%"PRIi64, src->integer);
   const size_t len = strlen(buf);

   value_t *slots = eval_alloc(len, state);
   for (size_t i = 0; i < len; i++) {
      slots[i].kind    = VALUE_INTEGER;
      slots[i].integer = (int64_t)buf[i];
   }

   dst[0].kind    = VALUE_UARRAY;
   dst[0].pointer = slots;
   dst[0].length  = 1;
   dst[1].kind    = VALUE_INTEGER;
   dst[1].integer = 1;
   dst[2].kind    = VALUE_INTEGER;
   dst[2].integer = len;
}

static void eval_real_to_string(value_t *dst, value_t *src, eval_state_t *state)
{
   char *buf LOCAL = xasprintf("%g", src->real);
   const size_t len = strlen(buf);

   value_t *slots = eval_alloc(len, state);
   for (size_t i = 0; i < len; i++) {
      slots[i].kind    = VALUE_INTEGER;
      slots[i].integer = (int64_t)buf[i];
   }

   dst[0].kind    = VALUE_UARRAY;
   dst[0].pointer = slots;
   dst[0].length  = 1;
   dst[1].kind    = VALUE_INTEGER;
   dst[1].integer = 1;
   dst[2].kind    = VALUE_INTEGER;
   dst[2].integer = len;
}

static void eval_op_fcall(int op, eval_state_t *state)
{
   ident_t func_name = vcode_get_func(op);

   const vcode_cc_t cc = vcode_get_subkind(op);
   if (cc == VCODE_CC_FOREIGN && func_name == ident_new("_nvc_ieee_warnings")) {
      // Emulate warnings always enabled
      value_t *dst = eval_get_reg(vcode_get_result(op), state);
      dst->kind    = VALUE_INTEGER;
      dst->integer = 1;
      return;
   }
   else if (cc == VCODE_CC_FOREIGN
            && func_name == ident_new("_int_to_string")) {
      value_t *dst = eval_get_reg(vcode_get_result(op), state);
      value_t *src = eval_get_reg(vcode_get_arg(op, 0), state);
      EVAL_ASSERT_VALUE(op, src, VALUE_INTEGER);
      eval_int_to_string(dst, src, state);
      return;
   }
   else if (cc == VCODE_CC_FOREIGN
            && func_name == ident_new("_real_to_string")) {
      value_t *dst = eval_get_reg(vcode_get_result(op), state);
      value_t *src = eval_get_reg(vcode_get_arg(op, 0), state);
      EVAL_ASSERT_VALUE(op, src, VALUE_REAL);
      eval_real_to_string(dst, src, state);
      return;
   }
   else if (cc == VCODE_CC_PREDEF) {
      // Always evaluate predefined operators
   }
   else if (!(state->flags & EVAL_FCALL)) {
      state->failed = true;
   }
   else if (cc != VCODE_CC_VHDL) {
      EVAL_WARN(state, op, "function call to foreign or protected "
                "function %s prevents constant folding", istr(func_name));
      state->failed = true;
      return;
   }

   value_t *arg0 = eval_get_reg(vcode_get_arg(op, 0), state);
   EVAL_ASSERT_VALUE(op, arg0, VALUE_CONTEXT);

   vcode_state_t vcode_state;
   vcode_state_save(&vcode_state);

   vcode_unit_t vcode = vcode_find_unit(func_name);

   if (vcode == NULL)
      vcode = vcode_find_unit(ident_prefix(func_name, ident_new("thunk"), '$'));

   if (vcode == NULL && state->eval->lower_fn != NULL)
      vcode = (*state->eval->lower_fn)(func_name, state->eval->lower_ctx);

   if (vcode == NULL) {
      vcode_state_restore(&vcode_state);
      EVAL_WARN(state, op, "function call to %s prevents "
                "constant folding", istr(func_name));
      state->failed = true;
      return;
   }

   vcode_select_unit(vcode);
   vcode_select_block(0);

   eval_state_t new = {
      .result  = -1,
      .hint    = state->hint,
      .failed  = false,
      .flags   = state->flags | EVAL_BOUNDS,
      .eval    = state->eval,
   };

   eval_setup_state(&new, arg0->context);

   vcode_state_restore(&vcode_state);

   const int nparams = vcode_count_args(op);
   for (int i = 0; i < nparams; i++) {
      vcode_reg_t arg = vcode_get_arg(op, i);
      value_t *src = eval_get_reg(arg, state);
      value_t *dst = eval_get_reg(i, &new);
      eval_copy_value(dst, src);

#if 0
      LOCAL_TEXT_BUF tb = tb_new();
      eval_dump(tb, dst);
      printf("P%d: %s\n", i, tb_get(tb));
#endif
   }

   vcode_select_unit(vcode);
   vcode_select_block(0);

   eval_vcode(&new);
   vcode_state_restore(&vcode_state);

   if (new.failed)
      state->failed = true;
   else if (vcode_get_result(op) != VCODE_INVALID_REG) {
      assert(new.result != -1);
      value_t *dst = eval_get_reg(vcode_get_result(op), state);
      value_t *result = new.regs[new.result];
      EVAL_ASSERT_VALID(op, result);
      eval_copy_value(dst, result);

      if (state->flags & EVAL_VERBOSE) {
         LOCAL_TEXT_BUF tb = tb_new();
         tb_printf(tb, "%s", istr(vcode_get_func(op)));
         if (state->hint && tree_kind(state->hint) == T_FCALL)
            tb_printf(tb, " (in %s)", istr(tree_ident(state->hint)));
         tb_printf(tb, " returned ");
         eval_dump(tb, new.regs[new.result]);
         notef("%s", tb_get(tb));
      }
   }

   eval_cleanup_state(&new);
}

static void eval_op_var_upref(int op, eval_state_t *state)
{
   vcode_var_t var = vcode_get_address(op);

   eval_frame_t *where = state->frame;
   for (int hops = vcode_get_hops(op); hops > 0;
        hops--, where = where->context)
      assert(where != NULL);

   if (where == NULL) {
      EVAL_WARN(state, op, "missing context prevents constant folding");
      state->failed = true;
   }
   else {
      value_t *src = eval_get_var(var, where);
      value_t *dst = eval_get_reg(vcode_get_result(op), state);

      eval_make_pointer_to(dst, src);
   }
}

static void eval_op_context_upref(int op, eval_state_t *state)
{
   eval_frame_t *where = state->frame;
   for (int hops = vcode_get_hops(op); hops > 0;
        hops--, where = where->context)
      assert(where != NULL);

   value_t *dst = eval_get_reg(vcode_get_result(op), state);
   dst->kind = VALUE_CONTEXT;
   dst->context = where;
}

static void eval_op_const_array(int op, eval_state_t *state)
{
   value_t *dst = eval_get_reg(vcode_get_result(op), state);

   const int nargs = vcode_count_args(op);
   unsigned offset = 1;
   for (int i = 0; i < nargs; i++) {
      value_t *elt = eval_get_reg(vcode_get_arg(op, i), state);
      switch (elt->kind) {
      case VALUE_INTEGER:
      case VALUE_REAL:
      case VALUE_ACCESS:
         dst[offset++] = *elt;
         break;
      case VALUE_CARRAY:
      case VALUE_RECORD:
         dst[offset++] = *elt;
         for (unsigned i = 0; i < elt->length; i++)
            dst[offset++] = elt[i + 1];
         break;
      default:
         vcode_dump_with_mark(op, NULL, NULL);
         fatal_trace("cannot handle array element kind %d", elt->kind);
      }
   }

   dst[0].kind = VALUE_CARRAY;
   dst[0].length = offset - 1;
}

static void eval_op_const_rep(int op, eval_state_t *state)
{
   value_t *dst = eval_get_reg(vcode_get_result(op), state);

   const int rep = vcode_get_value(op);
   value_t *arg0 = eval_get_reg(vcode_get_arg(op, 0), state);

   value_t *mem = eval_alloc(rep, state);
   for (int i = 0; i < rep; i++)
      mem[i] = *arg0;

   eval_make_pointer_to(dst, mem);
}

static void eval_op_wrap(int op, eval_state_t *state)
{
   value_t *dst = eval_get_reg(vcode_get_result(op), state);
   value_t *src = eval_get_reg(vcode_get_arg(op, 0), state);

   EVAL_ASSERT_VALUE(op, src, VALUE_POINTER);

   const int ndims = (vcode_count_args(op) - 1) / 3;

   dst[0].kind    = VALUE_UARRAY;
   dst[0].length  = ndims;
   dst[0].pointer = src->pointer;

   for (int i = 0; i < ndims; i++) {
      const int64_t left =
         eval_get_reg(vcode_get_arg(op, (i * 3) + 1), state)->integer;
      const int64_t right =
         eval_get_reg(vcode_get_arg(op, (i * 3) + 2), state)->integer;
      const range_kind_t dir =
         eval_get_reg(vcode_get_arg(op, (i * 3) + 3), state)->integer;

      const int64_t diff = dir == RANGE_DOWNTO ? left - right : right - left;
      const int64_t length = diff < 0 ? 0 : diff + 1;

      dst[1 + i*2 + 0].kind = VALUE_INTEGER;
      dst[1 + i*2 + 0].integer = left;
      dst[1 + i*2 + 1].kind = VALUE_INTEGER;
      dst[1 + i*2 + 1].integer = (dir == RANGE_DOWNTO ? -length : length);
   }
}

static void eval_op_store(int op, eval_state_t *state)
{
   value_t *src = eval_get_reg(vcode_get_arg(op, 0), state);
   value_t *var = eval_get_var(vcode_get_address(op), state->frame);

   eval_copy_value(var, src);
}

static void eval_op_load(int op, eval_state_t *state)
{
   value_t *dst = eval_get_reg(vcode_get_result(op), state);
   value_t *var = eval_get_var(vcode_get_address(op), state->frame);

   EVAL_ASSERT_VALID(op, var);
   eval_copy_value(dst, var);
}

static void eval_op_unwrap(int op, eval_state_t *state)
{
   value_t *dst = eval_get_reg(vcode_get_result(op), state);
   value_t *src = eval_get_reg(vcode_get_arg(op, 0), state);

   EVAL_ASSERT_VALUE(op, src, VALUE_UARRAY);
   eval_make_pointer_to(dst, src->pointer);
}

static void eval_op_uarray_len(int op, eval_state_t *state)
{
   value_t *dst = eval_get_reg(vcode_get_result(op), state);
   value_t *src = eval_get_reg(vcode_get_arg(op, 0), state);
   const int dim = vcode_get_dim(op);

   EVAL_ASSERT_VALUE(op, src, VALUE_UARRAY);
   EVAL_ASSERT_VALUE(op, &(src[1 + dim*2 + 1]), VALUE_INTEGER);

   dst->kind    = VALUE_INTEGER;
   dst->integer = llabs(src[1 + dim*2 + 1].integer);
}

static void eval_op_uarray_dir(int op, eval_state_t *state)
{
   value_t *dst = eval_get_reg(vcode_get_result(op), state);
   value_t *src = eval_get_reg(vcode_get_arg(op, 0), state);
   const int dim = vcode_get_dim(op);

   EVAL_ASSERT_VALUE(op, src, VALUE_UARRAY);
   EVAL_ASSERT_VALUE(op, &(src[1 + dim*2 + 1]), VALUE_INTEGER);

   dst->kind    = VALUE_INTEGER;
   dst->integer = src[1 + dim*2 + 1].integer < 0 ? RANGE_DOWNTO : RANGE_TO;
}

static void eval_op_and(int op, eval_state_t *state)
{
   value_t *dst = eval_get_reg(vcode_get_result(op), state);
   value_t *lhs = eval_get_reg(vcode_get_arg(op, 0), state);
   value_t *rhs = eval_get_reg(vcode_get_arg(op, 1), state);

   switch (lhs->kind) {
   case VALUE_INTEGER:
      dst->kind    = VALUE_INTEGER;
      dst->integer = lhs->integer & rhs->integer;
      break;

   default:
      fatal_trace("invalid value type in %s", __func__);
   }
}

static void eval_op_or(int op, eval_state_t *state)
{
   value_t *dst = eval_get_reg(vcode_get_result(op), state);
   value_t *lhs = eval_get_reg(vcode_get_arg(op, 0), state);
   value_t *rhs = eval_get_reg(vcode_get_arg(op, 1), state);

   switch (lhs->kind) {
   case VALUE_INTEGER:
      dst->kind    = VALUE_INTEGER;
      dst->integer = lhs->integer | rhs->integer;
      break;

   default:
      fatal_trace("invalid value type in %s", __func__);
   }
}

static void eval_op_xor(int op, eval_state_t *state)
{
   value_t *dst = eval_get_reg(vcode_get_result(op), state);
   value_t *lhs = eval_get_reg(vcode_get_arg(op, 0), state);
   value_t *rhs = eval_get_reg(vcode_get_arg(op, 1), state);

   switch (lhs->kind) {
   case VALUE_INTEGER:
      dst->kind    = VALUE_INTEGER;
      dst->integer = lhs->integer ^ rhs->integer;
      break;

   default:
      fatal_trace("invalid value type in %s", __func__);
   }
}

static void eval_op_xnor(int op, eval_state_t *state)
{
   value_t *dst = eval_get_reg(vcode_get_result(op), state);
   value_t *lhs = eval_get_reg(vcode_get_arg(op, 0), state);
   value_t *rhs = eval_get_reg(vcode_get_arg(op, 1), state);

   switch (lhs->kind) {
   case VALUE_INTEGER:
      dst->kind    = VALUE_INTEGER;
      dst->integer = !(lhs->integer ^ rhs->integer);
      break;

   default:
      fatal_trace("invalid value type in %s", __func__);
   }
}

static void eval_op_nand(int op, eval_state_t *state)
{
   value_t *dst = eval_get_reg(vcode_get_result(op), state);
   value_t *lhs = eval_get_reg(vcode_get_arg(op, 0), state);
   value_t *rhs = eval_get_reg(vcode_get_arg(op, 1), state);

   switch (lhs->kind) {
   case VALUE_INTEGER:
      dst->kind    = VALUE_INTEGER;
      dst->integer = !(lhs->integer & rhs->integer);
      break;

   default:
      fatal_trace("invalid value type in %s", __func__);
   }
}

static void eval_op_nor(int op, eval_state_t *state)
{
   value_t *dst = eval_get_reg(vcode_get_result(op), state);
   value_t *lhs = eval_get_reg(vcode_get_arg(op, 0), state);
   value_t *rhs = eval_get_reg(vcode_get_arg(op, 1), state);

   switch (lhs->kind) {
   case VALUE_INTEGER:
      dst->kind    = VALUE_INTEGER;
      dst->integer = !(lhs->integer | rhs->integer);
      break;

   default:
      fatal_trace("invalid value type in %s", __func__);
   }
}

static void eval_op_jump(int op, eval_state_t *state)
{
   eval_branch(op, vcode_get_target(op, 0), state);
}

static void eval_op_cond(int op, eval_state_t *state)
{
   value_t *test = eval_get_reg(vcode_get_arg(op, 0), state);
   eval_branch(op, vcode_get_target(op, !(test->integer)), state);
}

static void eval_op_undefined(int op, eval_state_t *state)
{
   EVAL_WARN(state, op, "reference to object without defined "
             "value in this phase prevents constant folding");

   state->failed = true;
}

static void eval_op_index(int op, eval_state_t *state)
{
   value_t *value = eval_get_var(vcode_get_address(op), state->frame);
   value_t *dst = eval_get_reg(vcode_get_result(op), state);

   eval_make_pointer_to(dst, value);
}

static void eval_op_load_indirect(int op, eval_state_t *state)
{
   value_t *dst = eval_get_reg(vcode_get_result(op), state);
   value_t *src = eval_get_reg(vcode_get_arg(op, 0), state);

   EVAL_ASSERT_VALUE(op, src, VALUE_POINTER);
   EVAL_ASSERT_VALID(op, src->pointer);

   eval_copy_value(dst, src->pointer);
}

static void eval_op_store_indirect(int op, eval_state_t *state)
{
   vcode_reg_t dst_reg = vcode_get_arg(op, 1);

   value_t *dst = eval_get_reg(dst_reg, state);
   value_t *src = eval_get_reg(vcode_get_arg(op, 0), state);

   EVAL_ASSERT_VALUE(op, dst, VALUE_POINTER);

   eval_copy_value(dst->pointer, src);
}

static void eval_op_case(int op, eval_state_t *state)
{
   value_t *test = eval_get_reg(vcode_get_arg(op, 0), state);
   vcode_block_t target = vcode_get_target(op, 0);

   const int num_args = vcode_count_args(op);
   for (int i = 1; i < num_args; i++) {
      value_t *cmp = eval_get_reg(vcode_get_arg(op, i), state);
      if (eval_value_cmp(test, cmp) == 0) {
         target = vcode_get_target(op, i);
         break;
      }
   }

   eval_branch(op, target, state);
}

static void eval_op_copy(int op, eval_state_t *state)
{
   value_t *dst = eval_get_reg(vcode_get_arg(op, 0), state);
   value_t *src = eval_get_reg(vcode_get_arg(op, 1), state);

   EVAL_ASSERT_VALUE(op, dst, VALUE_POINTER);
   EVAL_ASSERT_VALUE(op, src, VALUE_POINTER);

   int count;
   if (vcode_count_args(op) > 2) {
      value_t *count_val = eval_get_reg(vcode_get_arg(op, 2), state);
      EVAL_ASSERT_VALUE(op, count_val, VALUE_INTEGER);
      count = count_val->integer;
   }
   else {
      EVAL_ASSERT_VALUE(op, &(src->pointer[-1]), VALUE_RECORD);
      count = src->pointer[-1].length;
   }

   const uintptr_t dstp = (uintptr_t)dst->pointer;
   const uintptr_t srcp = (uintptr_t)src->pointer;

   if (dstp - srcp >= (uintptr_t)(count * sizeof(value_t))) {
      // Copy forwards
      for (int i = 0; i < count; i++)
         dst->pointer[i] = src->pointer[i];
   }
   else {
      // Copy backwards for overlapping case
      for (int i = count - 1; i >= 0; i--)
         dst->pointer[i] = src->pointer[i];
   }
}

static void eval_op_report(int op, eval_state_t *state)
{
   value_t *text = eval_get_reg(vcode_get_arg(op, 1), state);
   value_t *length = eval_get_reg(vcode_get_arg(op, 2), state);
   value_t *severity = eval_get_reg(vcode_get_arg(op, 0), state);

   if (state->flags & EVAL_REPORT)
      eval_message(text, length, severity, vcode_get_loc(op), "Report");
   else {
      EVAL_WARN(state, op, "report statement prevents constant folding");
      state->failed = true;  // Cannot fold as would change runtime behaviour
   }
}

static void eval_op_assert(int op, eval_state_t *state)
{
   value_t *test = eval_get_reg(vcode_get_arg(op, 0), state);
   value_t *text = eval_get_reg(vcode_get_arg(op, 2), state);
   value_t *length = eval_get_reg(vcode_get_arg(op, 3), state);
   value_t *severity = eval_get_reg(vcode_get_arg(op, 1), state);

   if (test->integer == 0) {
      if (state->flags & EVAL_REPORT)
         eval_message(text, length, severity, vcode_get_loc(op), "Assertion");
      if ((state->failed = severity->integer >= SEVERITY_ERROR))
         EVAL_WARN(state, op, "assertion failure prevents constant folding");
   }
}

static void eval_op_select(int op, eval_state_t *state)
{
   value_t *test = eval_get_reg(vcode_get_arg(op, 0), state);
   value_t *left = eval_get_reg(vcode_get_arg(op, 1), state);
   value_t *right = eval_get_reg(vcode_get_arg(op, 2), state);
   value_t *result = eval_get_reg(vcode_get_result(op), state);

   EVAL_ASSERT_VALUE(op, test, VALUE_INTEGER);

   *result = test->integer ? *left : *right;
}

static void eval_op_alloca(int op, eval_state_t *state)
{
   value_t *result = eval_get_reg(vcode_get_result(op), state);

   value_t *length = eval_get_reg(vcode_get_arg(op, 0), state);
   assert(length->kind == VALUE_INTEGER);

   vcode_type_t vtype = vcode_get_type(op);
   const int slots = length->integer * eval_slots_for_type(vtype);

   value_t *new = eval_alloc(slots, state);

   unsigned off = 0;
   for (int i = 0; i < length->integer; i++)
      off += eval_setup_var(vtype, new + off);

   eval_make_pointer_to(result, new);
}

static void eval_op_index_check(int op, eval_state_t *state)
{
   value_t *value = eval_get_reg(vcode_get_arg(op, 0), state);
   value_t *left  = eval_get_reg(vcode_get_arg(op, 1), state);
   value_t *right = eval_get_reg(vcode_get_arg(op, 2), state);
   value_t *dir   = eval_get_reg(vcode_get_arg(op, 3), state);
   value_t *locus = eval_get_reg(vcode_get_arg(op, 4), state);

   const int64_t low =
      dir->integer == RANGE_DOWNTO ? right->integer : left->integer;
   const int64_t high =
      dir->integer == RANGE_DOWNTO ? left->integer : right->integer;

   if (low > high)
      return;   // Null range
   else if (value->integer >= low && value->integer <= high)
      return;   // In bounds

   if (state->flags & EVAL_BOUNDS) {
      EVAL_ASSERT_VALUE(op, locus, VALUE_DEBUG_LOCUS);

      type_t type = tree_type(locus->debug);

      diag_t *d = diag_new(DIAG_ERROR, tree_loc(locus->debug));

      LOCAL_TEXT_BUF tb = tb_new();
      tb_cat(tb, "index ");
      to_string(tb, type, value->integer);
      tb_printf(tb, " outside of %s range ", type_pp(type));
      to_string(tb, type, left->integer);
      tb_cat(tb, dir == RANGE_TO ? " to " : " downto ");
      to_string(tb, type, right->integer);

      diag_printf(d, "%s", tb_get(tb));

      if (state->hint != NULL && tree_kind(state->hint) == T_FCALL)
         diag_hint(d, tree_loc(state->hint), "while evaluating call to %s",
                   istr(tree_ident(state->hint)));

      if (diag_hints(d) > 0) {
         LOCAL_TEXT_BUF vbuf = tb_new();
         to_string(vbuf, type, value->integer);
         diag_hint(d, tree_loc(locus->debug), "evaluated to %s", tb_get(vbuf));
      }

      diag_emit(d);
   }
   else
      EVAL_WARN(state, op, "bounds check failure prevents constant folding");

   state->failed = true;
}

static void eval_op_range_check(int op, eval_state_t *state)
{
   value_t *value = eval_get_reg(vcode_get_arg(op, 0), state);
   value_t *left  = eval_get_reg(vcode_get_arg(op, 1), state);
   value_t *right = eval_get_reg(vcode_get_arg(op, 2), state);
   value_t *dir   = eval_get_reg(vcode_get_arg(op, 3), state);
   value_t *locus = eval_get_reg(vcode_get_arg(op, 4), state);

   if (value->kind == VALUE_REAL)
      return;   // TODO: range checks for reals

   const int64_t low =
      dir->integer == RANGE_DOWNTO ? right->integer : left->integer;
   const int64_t high =
      dir->integer == RANGE_DOWNTO ? left->integer : right->integer;

   if (low > high)
      return;   // Null range
   else if (value->integer >= low && value->integer <= high)
      return;   // In bounds

   if (state->flags & EVAL_BOUNDS) {
      EVAL_ASSERT_VALUE(op, locus, VALUE_DEBUG_LOCUS);

      type_t type = tree_type(locus->debug);

      LOCAL_TEXT_BUF tb = tb_new();
      tb_cat(tb, "value ");
      to_string(tb, type, value->integer);
      tb_printf(tb, " outside of %s range ", type_pp(type));
      to_string(tb, type, left->integer);
      tb_cat(tb, dir == RANGE_TO ? " to " : " downto ");
      to_string(tb, type, right->integer);

      diag_t *d = diag_new(DIAG_ERROR, tree_loc(locus->debug));
      diag_printf(d, "%s", tb_get(tb));

      if (state->hint != NULL && tree_kind(state->hint) == T_FCALL)
         diag_hint(d, tree_loc(state->hint), "while evaluating call to %s",
                   istr(tree_ident(state->hint)));

      if (diag_hints(d) > 0) {
         LOCAL_TEXT_BUF vbuf = tb_new();
         to_string(vbuf, type, value->integer);
         diag_hint(d, tree_loc(locus->debug), "evaluated to %s", tb_get(vbuf));
      }

      diag_emit(d);
   }
   else
      EVAL_WARN(state, op, "range check failure prevents constant folding");

   state->failed = true;
}

static void eval_op_uarray_left(int op, eval_state_t *state)
{
   value_t *array = eval_get_reg(vcode_get_arg(op, 0), state);
   value_t *dst = eval_get_reg(vcode_get_result(op), state);
   const int dim = vcode_get_dim(op);

   EVAL_ASSERT_VALUE(op, array, VALUE_UARRAY);
   EVAL_ASSERT_VALUE(op, &(array[1 + dim*2]), VALUE_INTEGER);

   dst->kind = VALUE_INTEGER;
   dst->integer = array[1 + dim*2].integer;
}

static void eval_op_uarray_right(int op, eval_state_t *state)
{
   value_t *array = eval_get_reg(vcode_get_arg(op, 0), state);
   value_t *dst = eval_get_reg(vcode_get_result(op), state);
   const int dim = vcode_get_dim(op);

   EVAL_ASSERT_VALUE(op, array, VALUE_UARRAY);
   EVAL_ASSERT_VALUE(op, &(array[1 + dim*2]), VALUE_INTEGER);
   EVAL_ASSERT_VALUE(op, &(array[1 + dim*2 + 1]), VALUE_INTEGER);

   const int32_t left = array[1 + dim*2].integer;
   const int32_t length = array[1 + dim*2 + 1].integer;

   dst->kind = VALUE_INTEGER;
   if (length < 0)
      dst->integer = left + length + 1;
   else
      dst->integer = left + length - 1;
}

static void eval_op_const_record(int op, eval_state_t *state)
{
   value_t *dst = eval_get_reg(vcode_get_result(op), state);

   const int nfields = vcode_count_args(op);
   unsigned offset = 1;
   for (int i = 0; i < nfields; i++) {
      value_t *field = eval_get_reg(vcode_get_arg(op, i), state);
      switch (field->kind) {
      case VALUE_INTEGER:
      case VALUE_REAL:
      case VALUE_ACCESS:
         dst[offset++] = *field;
         break;
      case VALUE_CARRAY:
      case VALUE_RECORD:
         for (unsigned i = 0; i < field->length + 1; i++)
            dst[offset++] = field[i];
         break;
      default:
         vcode_dump_with_mark(op, NULL, NULL);
         fatal_trace("cannot handle fields of kind %d", field->kind);
      }
   }

   dst[0].kind = VALUE_RECORD;
   dst[0].length = offset - 1;
}

static void eval_op_address_of(int op, eval_state_t *state)
{
   value_t *src = eval_get_reg(vcode_get_arg(op, 0), state);
   value_t *dst = eval_get_reg(vcode_get_result(op), state);

   switch (src->kind) {
   case VALUE_RECORD:
   case VALUE_CARRAY:
      eval_make_pointer_to(dst, src);
      break;
   default:
      fatal_trace("unexpected value kind %d in address of", src->kind);
   }
}

static int eval_next_field_offset(value_t *field)
{
   switch (field->kind) {
   case VALUE_REAL:
   case VALUE_ACCESS:
   case VALUE_INTEGER:
      return 1;
   case VALUE_CARRAY:
   case VALUE_RECORD:
      return field->length + 1;
   default:
      fatal_trace("cannot skip over field kind %d", field->kind);
   }
}

static void eval_op_record_ref(int op, eval_state_t *state)
{
   value_t *ptr = eval_get_reg(vcode_get_arg(op, 0), state);
   value_t *dst = eval_get_reg(vcode_get_result(op), state);

   EVAL_ASSERT_VALUE(op, ptr, VALUE_POINTER);
   EVAL_ASSERT_VALUE(op, &(ptr->pointer[-1]), VALUE_RECORD);

   value_t *field = ptr->pointer;
   const int nth = vcode_get_field(op);
   for (int i = 0; i < nth; i++)
      field += eval_next_field_offset(field);

   EVAL_ASSERT_VALID(op, field);
   eval_make_pointer_to(dst, field);
}

static void eval_op_array_ref(int op, eval_state_t *state)
{
   vcode_reg_t result = vcode_get_result(op);
   value_t *ptr = eval_get_reg(vcode_get_arg(op, 0), state);
   value_t *offset = eval_get_reg(vcode_get_arg(op, 1), state);
   value_t *dst = eval_get_reg(result, state);

   EVAL_ASSERT_VALUE(op, ptr, VALUE_POINTER);
   EVAL_ASSERT_VALUE(op, offset, VALUE_INTEGER);

   vcode_type_t vtype = vtype_pointed(vcode_reg_type(result));
   const int stride = eval_slots_for_type(vtype);
   eval_make_pointer_to(dst, ptr->pointer + offset->integer * stride);
   EVAL_ASSERT_VALID(op, dst->pointer);
}

static void eval_op_memset(int op, eval_state_t *state)
{
   value_t *dst = eval_get_reg(vcode_get_arg(op, 0), state);
   value_t *fill = eval_get_reg(vcode_get_arg(op, 1), state);
   value_t *length = eval_get_reg(vcode_get_arg(op, 2), state);

   EVAL_ASSERT_VALUE(op, dst, VALUE_POINTER);
   EVAL_ASSERT_VALUE(op, length, VALUE_INTEGER);

   for (int i = 0; i < length->integer; i++)
      dst->pointer[i] = *fill;
}

static void eval_op_length_check(int op, eval_state_t *state)
{
   value_t *llen = eval_get_reg(vcode_get_arg(op, 0), state);
   value_t *rlen = eval_get_reg(vcode_get_arg(op, 1), state);

   if (rlen->integer != llen->integer) {
      if (state->flags & EVAL_BOUNDS) {
         // TODO: improve this to use the same message format as rtkern
         error_at(vcode_get_loc(op), "length of target %"PRIi64" does not "
                  "match length of value %"PRIi64,
                  llen->integer, rlen->integer);
      }
      else
         EVAL_WARN(state, op, "length check failure prevents constant folding");

      state->failed = true;
   }
}

static void eval_op_null(int op, eval_state_t *state)
{
   vcode_reg_t result = vcode_get_result(op);
   value_t *dst = eval_get_reg(result, state);

   if (vcode_reg_kind(result) == VCODE_TYPE_CONTEXT)
      dst->kind = VALUE_CONTEXT;
   else
      dst->kind = VALUE_ACCESS;

   dst->pointer = NULL;
}

static void eval_op_pcall(int op, eval_state_t *state)
{
   EVAL_WARN(state, op, "procedure call to %s prevents "
             "constant folding", istr(vcode_get_func(op)));
   state->failed = true;
}

static void eval_op_new(int op, eval_state_t *state)
{
   vcode_reg_t result = vcode_get_result(op);

   int length = 1;
   if (vcode_count_args(op) > 0)
      length = eval_get_reg(vcode_get_arg(op, 0), state)->integer;

   vcode_type_t vtype = vtype_pointed(vcode_reg_type(result));
   const int slots = eval_slots_for_type(vtype);

   value_t *dst = eval_get_reg(result, state);
   dst->kind    = VALUE_ACCESS;
   dst->pointer = eval_alloc(length * slots, state);

   unsigned off = 0;
   for (int i = 0; i < length; i++)
      off += eval_setup_var(vtype, dst->pointer + off);
}

static void eval_op_deallocate(int op, eval_state_t *state)
{
   value_t *ptr = eval_get_reg(vcode_get_arg(op, 0), state);
   EVAL_ASSERT_VALUE(op, ptr, VALUE_POINTER);
   EVAL_ASSERT_VALUE(op, ptr->pointer, VALUE_ACCESS);

   ptr->pointer->pointer = NULL;
   // Memory will be freed when evaluation context cleaned up
}

static void eval_op_all(int op, eval_state_t *state)
{
   value_t *access = eval_get_reg(vcode_get_arg(op, 0), state);
   EVAL_ASSERT_VALUE(op, access, VALUE_ACCESS);

   value_t *dst = eval_get_reg(vcode_get_result(op), state);
   dst->kind    = VALUE_POINTER;
   dst->pointer = access->pointer;
   dst->length  = 1;
}

static void eval_op_null_check(int op, eval_state_t *state)
{
   value_t *access = eval_get_reg(vcode_get_arg(op, 0), state);
   EVAL_ASSERT_VALUE(op, access, VALUE_ACCESS);

   if (access->pointer == NULL) {
      if (state->flags & EVAL_BOUNDS)
         error_at(vcode_get_loc(op), "null access dereference");
      else
         EVAL_WARN(state, op, "null dereference prevents constant folding");
      state->failed = true;
   }
}

static void eval_op_range_null(int op, eval_state_t *state)
{
   value_t *result = eval_get_reg(vcode_get_result(op), state);
   value_t *left = eval_get_reg(vcode_get_arg(op, 0), state);
   value_t *right = eval_get_reg(vcode_get_arg(op, 1), state);
   value_t *dir = eval_get_reg(vcode_get_arg(op, 2), state);

   assert(left->kind == VALUE_INTEGER);
   assert(right->kind == VALUE_INTEGER);
   assert(dir->kind == VALUE_INTEGER);

   result->kind = VALUE_INTEGER;

   if (dir->integer == RANGE_TO)
      result->integer = left->integer > right->integer;
   else
      result->integer = right->integer > left->integer;
}

static void eval_op_range_length(int op, eval_state_t *state)
{
   value_t *result = eval_get_reg(vcode_get_result(op), state);
   value_t *left = eval_get_reg(vcode_get_arg(op, 0), state);
   value_t *right = eval_get_reg(vcode_get_arg(op, 1), state);
   value_t *dir = eval_get_reg(vcode_get_arg(op, 2), state);

   assert(left->kind == VALUE_INTEGER);
   assert(right->kind == VALUE_INTEGER);
   assert(dir->kind == VALUE_INTEGER);

   result->kind = VALUE_INTEGER;

   int64_t diff;
   if (dir->integer == RANGE_TO)
      diff = right->integer - left->integer;
   else
      diff = left->integer - right->integer;

   result->integer = diff < 0 ? 0 : diff + 1;
}

static void eval_op_link_var(int op, eval_state_t *state)
{
   value_t *result = eval_get_reg(vcode_get_result(op), state);
   value_t *arg0 = eval_get_reg(vcode_get_arg(op, 0), state);

   EVAL_ASSERT_VALUE(op, arg0, VALUE_CONTEXT);

   ident_t unit_name = vtype_name(vcode_reg_type(vcode_get_arg(op, 0)));
   ident_t var_name = vcode_get_ident(op);

   vcode_unit_t vu = vcode_find_unit(unit_name);
   assert(vu);

   vcode_state_t vcode_state;
   vcode_state_save(&vcode_state);

   vcode_select_unit(vu);

   const int nvars = vcode_count_vars();
   vcode_var_t var = 0;
   for (; var < nvars; var++) {
      if (vcode_var_name(var) == var_name)
         break;
   }

   if (var == nvars) {
      vcode_dump();
      vcode_state_restore(&vcode_state);
      vcode_dump_with_mark(op, NULL, NULL);

      fatal_trace("variable %s not found in unit %s", istr(var_name),
                  istr(unit_name));
   }

   vcode_state_restore(&vcode_state);

   eval_frame_t *ctx = arg0->context;
   assert(var < ctx->nvars);

   eval_make_pointer_to(result, ctx->vars[var]);
}

static void eval_op_link_package(int op, eval_state_t *state)
{
   value_t *result = eval_get_reg(vcode_get_result(op), state);
   result->kind = VALUE_CONTEXT;
   result->context = eval_link(state->eval, vcode_get_ident(op));

   if ((state->failed = (result->context == NULL)))
      EVAL_WARN(state, op, "missing package %s prevents constant folding",
                istr(vcode_get_ident(op)));
}

static void eval_op_debug_locus(int op, eval_state_t *state)
{
   ident_t unit = vcode_get_ident(op);
   const ptrdiff_t offset = vcode_get_value(op);

   value_t *result = eval_get_reg(vcode_get_result(op), state);
   result->kind = VALUE_DEBUG_LOCUS;
   result->debug = tree_from_locus(unit, offset, lib_get_qualified);
}

static void eval_op_debug_out(int op, eval_state_t *state)
{
   vcode_reg_t reg = vcode_get_arg(op, 0);
   value_t *value = eval_get_reg(reg, state);
   assert(value->kind == VALUE_INTEGER);

   printf("DEBUG: r%d val=%"PRIi64"\n", reg, value->integer);
}

static void eval_op_file_open(int op, eval_state_t *state)
{
   value_t *arg0 = eval_get_reg(vcode_get_arg(op, 0), state);
   EVAL_ASSERT_VALUE(op, arg0, VALUE_POINTER);
   // No-op
}

static void eval_op_file_close(int op, eval_state_t *state)
{
   value_t *arg0 = eval_get_reg(vcode_get_arg(op, 0), state);
   EVAL_ASSERT_VALUE(op, arg0, VALUE_POINTER);
   // No-op
}

static void eval_op_protected_init(int op, eval_state_t *state)
{
   value_t *result = eval_get_reg(vcode_get_result(op), state);
   result->kind = VALUE_CONTEXT;
   result->context = NULL;

   ident_t unit_name = vcode_get_func(op);
   vcode_unit_t vcode = vcode_find_unit(unit_name);

   if (vcode == NULL) {
      EVAL_WARN(state, op, "missing vcode unit for %s prevents "
                "constant folding", istr(unit_name));
      state->failed = true;
      return;
   }

   vcode_state_t vcode_state;
   vcode_state_save(&vcode_state);

   vcode_select_unit(vcode);
   vcode_select_block(0);

   eval_state_t new = {
      .result  = -1,
      .hint    = state->hint,
      .failed  = false,
      .flags   = state->flags | EVAL_BOUNDS,
      .eval    = state->eval,
   };

   eval_setup_state(&new, state->frame);

   eval_vcode(&new);

   if (!new.failed && (state->flags & EVAL_VERBOSE)) {
      LOCAL_TEXT_BUF tb = tb_new();
      tb_printf(tb, "init protected type %s", istr(unit_name));
      eval_dump_frame(tb, new.frame);
      notef("%s", tb_get(tb));
   }

   if (!new.failed) {
      result->context = new.frame;
      new.frame = NULL;
   }

   eval_cleanup_state(&new);

   vcode_state_restore(&vcode_state);
}

static void eval_op_protected_free(int op, eval_state_t *state)
{
   value_t *arg0 = eval_get_reg(vcode_get_arg(op, 0), state);
   EVAL_ASSERT_VALUE(op, arg0, VALUE_CONTEXT);
   // Memory will be freed when evaluation context cleaned up
}

static void eval_vcode(eval_state_t *state)
{
   state->op = 0;

   while (!(state->failed)) {
      switch (vcode_get_op(state->op)) {
      case VCODE_OP_COMMENT:
         break;

      case VCODE_OP_CONST:
         eval_op_const(state->op, state);
         break;

      case VCODE_OP_CONST_REAL:
         eval_op_const_real(state->op, state);
         break;

      case VCODE_OP_RETURN:
         eval_op_return(state->op, state);
         return;

      case VCODE_OP_NOT:
         eval_op_not(state->op, state);
         break;

      case VCODE_OP_ADD:
         eval_op_add(state->op, state);
         break;

      case VCODE_OP_TRAP_ADD:
         eval_op_trap_add(state->op, state);
         break;

      case VCODE_OP_SUB:
         eval_op_sub(state->op, state);
         break;

      case VCODE_OP_TRAP_SUB:
         eval_op_trap_sub(state->op, state);
         break;

      case VCODE_OP_MUL:
         eval_op_mul(state->op, state);
         break;

      case VCODE_OP_TRAP_MUL:
         eval_op_trap_mul(state->op, state);
         break;

      case VCODE_OP_ZERO_CHECK:
         eval_op_zero_check(state->op, state);
         break;

      case VCODE_OP_DIV:
         eval_op_div(state->op, state);
         break;

      case VCODE_OP_CMP:
         eval_op_cmp(state->op, state);
         break;

      case VCODE_OP_CAST:
         eval_op_cast(state->op, state);
         break;

      case VCODE_OP_NEG:
         eval_op_neg(state->op, state);
         break;

      case VCODE_OP_FCALL:
         eval_op_fcall(state->op, state);
         break;

      case VCODE_OP_CONST_ARRAY:
         eval_op_const_array(state->op, state);
         break;

      case VCODE_OP_CONST_REP:
         eval_op_const_rep(state->op, state);
         break;

      case VCODE_OP_WRAP:
         eval_op_wrap(state->op, state);
         break;

      case VCODE_OP_STORE:
         eval_op_store(state->op, state);
         break;

      case VCODE_OP_UNWRAP:
         eval_op_unwrap(state->op, state);
         break;

      case VCODE_OP_UARRAY_LEN:
         eval_op_uarray_len(state->op, state);
         break;

      case VCODE_OP_AND:
         eval_op_and(state->op, state);
         break;

      case VCODE_OP_OR:
         eval_op_or(state->op, state);
         break;

      case VCODE_OP_XOR:
         eval_op_xor(state->op, state);
         break;

      case VCODE_OP_XNOR:
         eval_op_xnor(state->op, state);
         break;

      case VCODE_OP_NAND:
         eval_op_nand(state->op, state);
         break;

      case VCODE_OP_NOR:
         eval_op_nor(state->op, state);
         break;

      case VCODE_OP_COND:
         eval_op_cond(state->op, state);
         continue;

      case VCODE_OP_JUMP:
         eval_op_jump(state->op, state);
         continue;

      case VCODE_OP_LOAD:
         eval_op_load(state->op, state);
         break;

      case VCODE_OP_UNDEFINED:
         eval_op_undefined(state->op, state);
         break;

      case VCODE_OP_CASE:
         eval_op_case(state->op, state);
         continue;

      case VCODE_OP_MOD:
         eval_op_mod(state->op, state);
         break;

      case VCODE_OP_REM:
         eval_op_rem(state->op, state);
         break;

      case VCODE_OP_INDEX:
         eval_op_index(state->op, state);
         break;

      case VCODE_OP_COPY:
         eval_op_copy(state->op, state);
         break;

      case VCODE_OP_LOAD_INDIRECT:
         eval_op_load_indirect(state->op, state);
         break;

      case VCODE_OP_STORE_INDIRECT:
         eval_op_store_indirect(state->op, state);
         break;

      case VCODE_OP_REPORT:
         eval_op_report(state->op, state);
         break;

      case VCODE_OP_ASSERT:
         eval_op_assert(state->op, state);
         break;

      case VCODE_OP_SELECT:
         eval_op_select(state->op, state);
         break;

      case VCODE_OP_ALLOCA:
         eval_op_alloca(state->op, state);
         break;

      case VCODE_OP_INDEX_CHECK:
         eval_op_index_check(state->op, state);
         break;

      case VCODE_OP_RANGE_CHECK:
         eval_op_range_check(state->op, state);
         break;

      case VCODE_OP_ABS:
         eval_op_abs(state->op, state);
         break;

      case VCODE_OP_UARRAY_LEFT:
         eval_op_uarray_left(state->op, state);
         break;

      case VCODE_OP_UARRAY_RIGHT:
         eval_op_uarray_right(state->op, state);
         break;

      case VCODE_OP_UARRAY_DIR:
         eval_op_uarray_dir(state->op, state);
         break;

      case VCODE_OP_EXPONENT_CHECK:
         eval_op_exponent_check(state->op, state);
         break;

      case VCODE_OP_EXP:
         eval_op_exp(state->op, state);
         break;

      case VCODE_OP_CONST_RECORD:
         eval_op_const_record(state->op, state);
         break;

      case VCODE_OP_ADDRESS_OF:
         eval_op_address_of(state->op, state);
         break;

      case VCODE_OP_RECORD_REF:
         eval_op_record_ref(state->op, state);
         break;

      case VCODE_OP_ARRAY_REF:
         eval_op_array_ref(state->op, state);
         break;

      case VCODE_OP_MEMSET:
         eval_op_memset(state->op, state);
         break;

      case VCODE_OP_LENGTH_CHECK:
         eval_op_length_check(state->op, state);
         break;

      case VCODE_OP_NULL:
         eval_op_null(state->op, state);
         break;

      case VCODE_OP_PCALL:
         eval_op_pcall(state->op, state);
         break;

      case VCODE_OP_NEW:
         eval_op_new(state->op, state);
         break;

      case VCODE_OP_ALL:
         eval_op_all(state->op, state);
         break;

      case VCODE_OP_NULL_CHECK:
         eval_op_null_check(state->op, state);
         break;

      case VCODE_OP_DEALLOCATE:
         eval_op_deallocate(state->op, state);
         break;

      case VCODE_OP_VAR_UPREF:
         eval_op_var_upref(state->op, state);
         break;

      case VCODE_OP_CONTEXT_UPREF:
         eval_op_context_upref(state->op, state);
         break;

      case VCODE_OP_RANGE_NULL:
         eval_op_range_null(state->op, state);
         break;

      case VCODE_OP_RANGE_LENGTH:
         eval_op_range_length(state->op, state);
         break;

      case VCODE_OP_DEBUG_OUT:
         eval_op_debug_out(state->op, state);
         break;

      case VCODE_OP_LINK_VAR:
         eval_op_link_var(state->op, state);
         break;

      case VCODE_OP_PROTECTED_INIT:
         eval_op_protected_init(state->op, state);
         break;

      case VCODE_OP_PROTECTED_FREE:
         eval_op_protected_free(state->op, state);
         break;

      case VCODE_OP_FILE_READ:
      case VCODE_OP_FILE_WRITE:
      case VCODE_OP_ENDFILE:
         EVAL_WARN(state, state->op, "vcode op %s prevents constant folding",
                   vcode_op_string(vcode_get_op(state->op)));
         state->failed = true;
         break;

      case VCODE_OP_FILE_OPEN:
         eval_op_file_open(state->op, state);
         break;

      case VCODE_OP_FILE_CLOSE:
         eval_op_file_close(state->op, state);
         break;

      case VCODE_OP_LINK_PACKAGE:
         eval_op_link_package(state->op, state);
         break;

      case VCODE_OP_DEBUG_LOCUS:
         eval_op_debug_locus(state->op, state);
         break;

      default:
         vcode_dump();
         fatal("cannot evaluate vcode op %s",
               vcode_op_string(vcode_get_op(state->op)));
      }

      (state->op)++;
   }
}

static eval_scalar_t eval_get_scalar(value_t *value)
{
   switch (value->kind) {
   case VALUE_INTEGER:
      return (eval_scalar_t){ .integer = value->integer };
   case VALUE_REAL:
      return (eval_scalar_t){ .real = value->real };
   default:
      fatal_trace("value kind %d is not scalar", value->kind);
   }
}

static tree_t eval_array_to_tree(const loc_t *loc, value_t *wrap,
                                 value_t *array, type_t type,
                                 int dim, unsigned *offset)
{
   tree_t tree = tree_new(T_AGGREGATE);
   tree_set_loc(tree, loc);

   const int ndims = wrap != NULL ? wrap[0].length : dimension_of(type);

   int64_t length = 0;
   for (int d = ndims - 1; d >= dim; d--) {
      if (wrap != NULL)
         length = llabs(wrap[1 + d*2 + 1].integer);
      else if (!folded_length(range_of(type, d), &length))
         fatal_at(loc, "array bound is not a constant");
   }

   type_t elem = type_elem(type);
   unsigned pos = 0;
   while (pos < length) {
      tree_t value;
      if (dim + 1 == ndims) {
         value = eval_value_to_tree(&(array[*offset]), elem, loc);
         *offset += eval_next_field_offset(&(array[*offset]));
      }
      else {
         value = eval_array_to_tree(loc, wrap, array, type, dim + 1, offset);
         tree_set_type(value, array_aggregate_type(type, dim + 1));
      }

      tree_t assoc = tree_new(T_ASSOC);
      tree_set_subkind(assoc, A_POS);
      tree_set_pos(assoc, pos++);
      tree_set_value(assoc, value);

      tree_add_assoc(tree, assoc);
   }

   return tree;
}

static tree_t eval_value_to_tree(value_t *value, type_t type, const loc_t *loc)
{
   tree_t tree = NULL;

   switch (value->kind) {
   case VALUE_INTEGER:
      if (type_is_enum(type)) {
         type_t enum_type = type_base_recur(type);
         if ((unsigned)value->integer >= type_enum_literals(enum_type))
            fatal_at(loc, "enum position %"PRIi64" out of range for type %s",
                     value->integer, type_pp(enum_type));
         tree_t lit = type_enum_literal(enum_type, value->integer);

         tree = tree_new(T_REF);
         tree_set_ref(tree, lit);
         tree_set_ident(tree, tree_ident(lit));
      }
      else if (type_is_physical(type)) {
         tree = tree_new(T_LITERAL);
         tree_set_subkind(tree, L_PHYSICAL);
         tree_set_ival(tree, value->integer);
      }
      else {
         tree = tree_new(T_LITERAL);
         tree_set_subkind(tree, L_INT);
         tree_set_ival(tree, value->integer);
      }
      break;
   case VALUE_REAL:
      tree = tree_new(T_LITERAL);
      tree_set_subkind(tree, L_REAL);
      tree_set_dval(tree, value->real);
      break;
   case VALUE_POINTER:
      if (type_is_record(type)) {
         value_t *rec = &(value->pointer[-1]);
         EVAL_ASSERT_VALUE(-1, rec, VALUE_RECORD);
         return eval_value_to_tree(rec, type, loc);
      }
      else if (type_is_array(type)) {
         value_t *arr = &(value->pointer[-1]);
         EVAL_ASSERT_VALUE(-1, arr, VALUE_CARRAY);
         return eval_value_to_tree(arr, type, loc);
      }
      else
         fatal_trace("pointer cannot be converted to tree");
      break;
   case VALUE_RECORD:
      {
         tree = tree_new(T_AGGREGATE);
         unsigned offset = 1;
         const int nfields = type_fields(type);
         for (int i = 0; i < nfields; i++) {
            tree_t field = type_field(type, i);
            tree_t assoc = tree_new(T_ASSOC);
            tree_set_subkind(assoc, A_POS);
            tree_set_pos(assoc, i);
            tree_set_value(assoc, eval_value_to_tree(&(value[offset]),
                                                     tree_type(field), loc));
            tree_add_assoc(tree, assoc);

            offset += eval_next_field_offset(&(value[offset]));
         }
      }
      break;
   case VALUE_CARRAY:
      {
         unsigned offset = 0;
         tree = eval_array_to_tree(loc, NULL, value + 1, type, 0, &offset);
         assert(offset == value->length);
      }
      break;
   case VALUE_UARRAY:
      {
         if (type_is_unconstrained(type)) {
            tree_t constraint = tree_new(T_CONSTRAINT);
            tree_set_subkind(constraint, C_INDEX);

            for (unsigned dim = 0; dim < value[0].length; dim++) {
               const int ileft   = value[1 + dim*2].integer;
               const int ilength = value[1 + dim*2 + 1].integer;
               const int iright  =
                  ilength < 0 ? ileft + ilength + 1 : ileft + ilength - 1;
               const range_kind_t dir = ilength < 0 ? RANGE_DOWNTO : RANGE_TO;

               type_t index = index_type_of(type, dim);

               tree_t left = tree_new(T_LITERAL);
               tree_set_subkind(left, L_INT);
               tree_set_type(left, index);
               tree_set_ival(left, ileft);

               tree_t right = tree_new(T_LITERAL);
               tree_set_subkind(right, L_INT);
               tree_set_type(right, index);
               tree_set_ival(right, iright);

               tree_t r = tree_new(T_RANGE);
               tree_set_subkind(r, dir);
               tree_set_left(r, left);
               tree_set_right(r, right);
               tree_set_type(r, index);

               tree_add_range(constraint, r);
            }

            type_t sub = type_new(T_SUBTYPE);
            type_set_base(sub, type);
            type_add_constraint(sub, constraint);

            type = sub;
         }

         unsigned offset = 0;
         tree = eval_array_to_tree(loc, value, value->pointer,
                                   type, 0, &offset);
      }
      break;
   default:
      fatal_trace("cannot convert value %d to tree", value->kind);
   }

   tree_set_type(tree, type);
   tree_set_loc(tree, loc);
   return tree;
}

static void eval_oom_cb(mspace_t *m, size_t size)
{
   diag_t *d = diag_new(DIAG_FATAL, NULL);
   diag_printf(d, "out of memory attempting to allocate %zu byte object", size);

   const int heapsize = opt_get_int(OPT_HEAP_SIZE);
   diag_hint(d, NULL, "the current heap size is %u bytes which you can "
             "increase with the $bold$-H$$ option, for example $bold$-H %um$$",
             heapsize, MAX(1, (heapsize * 2) / 1024 / 1024));

   diag_emit(d);
   fatal_exit(EXIT_FAILURE);
}

////////////////////////////////////////////////////////////////////////////////
// Public interface

eval_t *eval_new(eval_flags_t flags)
{
   if (opt_get_verbose(OPT_EVAL_VERBOSE, NULL))
      flags |= EVAL_VERBOSE;

   if (flags & EVAL_VERBOSE)
      flags |= EVAL_WARN;

   eval_t *ex = xcalloc(sizeof(eval_t));
   ex->link_map = hash_new(128, true);
   ex->flags    = flags;
   ex->mspace   = mspace_new(opt_get_int(OPT_HEAP_SIZE));

   mspace_set_oom_handler(ex->mspace, eval_oom_cb);

   return ex;
}

void eval_free(eval_t *ex)
{
   const void *key;
   void *value;
   hash_iter_t it = HASH_BEGIN;
   while (hash_iter(ex->link_map, &it, &key, &value)) {
      if (value != (void *)-1) {
         mptr_t mptr = (uintptr_t)value;
         mptr_free(ex->mspace, &mptr);
      }
   }

   mspace_destroy(ex->mspace);
   hash_free(ex->link_map);
   free(ex);
}

static bool eval_try_vcall(eval_t *ex, ident_t func, eval_frame_t *context,
                           eval_scalar_t *result, const char *fmt, va_list ap)
{
   vcode_unit_t unit = eval_find_unit(func, EVAL_VERBOSE);
   assert(unit != NULL);

   assert(ex->flags & EVAL_FCALL);

   vcode_select_unit(unit);
   vcode_select_block(0);

   eval_state_t state = {
      .result = -1,
      .hint   = NULL,
      .failed = false,
      .flags  = ex->flags,
      .eval   = ex,
   };

   eval_setup_state(&state, context);

   value_t *p0 = eval_get_reg(0, &state);
   p0->kind = VALUE_CONTEXT;
   p0->context = context;

   const int nparams = vcode_count_params();

   for (int nth = 1; *fmt; fmt++) {
      if (nth >= nparams)
         fatal_trace("too many parameters for %s (expect %d)",
                     istr(func), nparams);

      value_t *p = eval_get_reg(nth++, &state);
      switch (*fmt) {
      case 'I':
         p->kind = VALUE_INTEGER;
         p->integer = va_arg(ap, int64_t);
         break;
      case 'i':
         p->kind = VALUE_INTEGER;
         p->integer = va_arg(ap, int32_t);
         break;
      case 'R':
         p->kind = VALUE_REAL;
         p->real = va_arg(ap, double);
         break;
      case 'u':
         {
            const void *data = va_arg(ap, void *);
            const int32_t left = va_arg(ap, int32_t);
            const int32_t length = va_arg(ap, int32_t);

            p[0].kind    = VALUE_UARRAY;
            p[0].length  = 1;
            p[0].pointer = eval_alloc(abs(length), &state);
            p[1].kind    = VALUE_INTEGER;
            p[1].integer = left;
            p[2].kind    = VALUE_INTEGER;
            p[2].integer = length;

            const int32_t *dp = data;
            for (int i = 0; i < abs(length); i++) {
               p[0].pointer[i].kind = VALUE_INTEGER;
               p[0].pointer[i].integer = *dp++;
            }
         }
         break;
      default:
         fatal_trace("invalid character '%c' in eval_call format", *fmt);
      }
   }

   eval_vcode(&state);

   if (!state.failed)
      *result = eval_get_scalar(eval_get_reg(state.result, &state));

   eval_cleanup_state(&state);

   return !state.failed;
}

bool eval_try_call(eval_t *ex, ident_t func, eval_frame_t *context,
                   eval_scalar_t *result, const char *fmt, ...)
{
   va_list ap;
   va_start(ap, fmt);
   bool ok = eval_try_vcall(ex, func, context, result, fmt, ap);
   va_end(ap);
   return ok;
}

eval_scalar_t eval_call(eval_t *ex, ident_t func, eval_frame_t *context,
                        const char *fmt, ...)
{
   va_list ap;
   va_start(ap, fmt);

   eval_scalar_t result;
   if (!eval_try_vcall(ex, func, context, &result, fmt, ap))
      fatal_trace("call to %s failed", istr(func));

   va_end(ap);
   return result;
}

eval_frame_t *eval_link(eval_t *ex, ident_t ident)
{
   void *map = hash_get(ex->link_map, ident);
   if (map == (void *)-1)
      return NULL;
   else if (map != NULL)
      return mptr_get(ex->mspace, (uintptr_t)map);

   // Poison value to detect recursive linking
   hash_put(ex->link_map, ident, (void *)-1);

   eval_flags_t flags = ex->flags | EVAL_WARN | EVAL_FCALL | EVAL_BOUNDS;

   vcode_unit_t unit = eval_find_unit(ident, flags);
   if (unit == NULL)
      return NULL;

   vcode_state_t vcode_state;
   vcode_state_save(&vcode_state);

   vcode_select_unit(unit);
   vcode_select_block(0);

   assert(vcode_unit_kind() == VCODE_UNIT_PACKAGE);

   eval_state_t state = {
      .result = -1,
      .hint   = NULL,
      .failed = false,
      .flags  = flags,
      .eval   = ex,
   };

   eval_setup_state(&state, NULL);

   eval_vcode(&state);

   if (!state.failed && (ex->flags & EVAL_VERBOSE)) {
      LOCAL_TEXT_BUF tb = tb_new();
      tb_printf(tb, "linked unit %s", istr(ident));
      eval_dump_frame(tb, state.frame);
      notef("%s", tb_get(tb));
   }

   vcode_state_restore(&vcode_state);

   if (state.failed) {
      eval_cleanup_state(&state);
      return NULL;
   }

   // Move the frame with the local variables out of the state
   eval_frame_t *frame = state.frame;
   state.frame = NULL;

   eval_cleanup_state(&state);

   mptr_t mptr = mptr_new(ex->mspace, "linked unit");
   mptr_put(ex->mspace, mptr, frame);

   hash_put(ex->link_map, ident, (void *)(uintptr_t)mptr);
   return frame;
}

eval_scalar_t eval_get_frame_var(eval_t *ex, eval_frame_t *frame, unsigned nth)
{
   assert(nth < frame->nvars);
   return eval_get_scalar(frame->vars[nth]);
}

tree_t eval_fold(eval_t *ex, tree_t expr, vcode_unit_t thunk)
{
   vcode_select_unit(thunk);
   vcode_select_block(0);
   assert(vcode_unit_kind() == VCODE_UNIT_THUNK);

   const tree_kind_t kind = tree_kind(expr);
   if (ex->flags & EVAL_VERBOSE)
      note_at(tree_loc(expr), "evaluate thunk for %s",
              kind == T_FCALL ? istr(tree_ident(expr)) : tree_kind_str(kind));

   eval_state_t state = {
      .result = -1,
      .hint   = expr,
      .failed = false,
      .flags  = ex->flags,
      .eval   = ex,
   };

   eval_setup_state(&state, NULL);

   eval_vcode(&state);

   if (state.failed) {
      eval_cleanup_state(&state);
      return expr;
   }

   assert(state.result != -1);

   value_t *result = eval_get_reg(state.result, &state);

   if (ex->flags & EVAL_VERBOSE) {
      const char *name = kind == T_FCALL
         ? istr(tree_ident(expr)) : tree_kind_str(kind);
      LOCAL_TEXT_BUF tb = tb_new();
      eval_dump(tb, result);
      note_at(tree_loc(expr), "%s returned %s", name, tb_get(tb));
   }

   type_t type = tree_type(expr);
   tree_t tree = eval_value_to_tree(result, type, tree_loc(expr));
   eval_cleanup_state(&state);
   return tree;
}

void eval_set_lower_fn(eval_t *ex, lower_fn_t fn, void *ctx)
{
   ex->lower_fn = fn;
   ex->lower_ctx = ctx;
}

static bool eval_not_possible(eval_t *e, tree_t t, const char *why)
{
   if (e->flags & EVAL_WARN)
      warn_at(tree_loc(t), "%s prevents constant folding", why);

   return false;
}

bool eval_possible(eval_t *e, tree_t t)
{
   switch (tree_kind(t)) {
   case T_FCALL:
      {
         tree_t decl = tree_ref(t);
         const subprogram_kind_t kind = tree_subkind(decl);
         if (kind == S_USER && !(e->flags & EVAL_FCALL))
            return eval_not_possible(e, t, "call to user defined function");
         else if (kind == S_FOREIGN || kind == S_VHPIDIRECT)
            return eval_not_possible(e, t, "call to foreign function");
         else if (tree_flags(decl) & TREE_F_IMPURE)
            return eval_not_possible(e, t, "call to impure function");
         else if (!(tree_flags(t) & TREE_F_GLOBALLY_STATIC))
            return eval_not_possible(e, t, "non-static expression");
         else if (kind != S_USER && !is_open_coded_builtin(kind)
                  && vcode_find_unit(tree_ident2(decl)) == NULL)
            return eval_not_possible(e, t, "not yet lowered predef");

         const int nparams = tree_params(t);
         for (int i = 0; i < nparams; i++) {
            tree_t p = tree_value(tree_param(t, i));
            if (!eval_possible(e, p))
               return false;
            else if (tree_kind(p) == T_FCALL && type_is_scalar(tree_type(p)))
               return false;  // Would have been folded already if possible
         }

         return true;
      }

   case T_LITERAL:
      return true;

   case T_TYPE_CONV:
      return eval_possible(e, tree_value(t));

   case T_QUALIFIED:
      return eval_possible(e, tree_value(t));

   case T_REF:
      {
         tree_t decl = tree_ref(t);
         switch (tree_kind(decl)) {
         case T_UNIT_DECL:
         case T_ENUM_LIT:
            return true;

         case T_CONST_DECL:
            if (tree_has_value(decl))
               return eval_possible(e, tree_value(decl));
            else if (!(e->flags & EVAL_FCALL))
               return eval_not_possible(e, t, "deferred constant");
            else
               return true;

         default:
            return eval_not_possible(e, t, "reference");
         }
      }

   case T_RECORD_REF:
      return eval_possible(e, tree_value(t));

   case T_ARRAY_REF:
      {
         const int nparams = tree_params(t);
         for (int i = 0; i < nparams; i++) {
            if (!eval_possible(e, tree_value(tree_param(t, i))))
               return false;
         }

         return eval_possible(e, tree_value(t));
      }

   case T_AGGREGATE:
      {
         const int nassocs = tree_assocs(t);
         for (int i = 0; i < nassocs; i++) {
            if (!eval_possible(e, tree_value(tree_assoc(t, i))))
               return false;
         }

         return true;
      }

   case T_ATTR_REF:
      {
         if (tree_subkind(t) == ATTR_USER)
            return eval_not_possible(e, t, "user defined attribute");

         if (!eval_possible(e, tree_name(t)))
            return false;

         const int nparams = tree_params(t);
         for (int i = 0; i < nparams; i++) {
            if (!eval_possible(e, tree_value(tree_param(t, i))))
               return false;
         }

         return true;
      }

   default:
      return eval_not_possible(e, t, tree_kind_str(tree_kind(t)));
   }
}
