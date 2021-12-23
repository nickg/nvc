//
//  Copyright (C) 2013-2021  Nick Gasson
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

#include "prim.h"
#include "exec.h"
#include "util.h"
#include "ident.h"
#include "hash.h"
#include "vcode.h"
#include "tree.h"
#include "phase.h"

#include <assert.h>
#include <stdint.h>
#include <stdlib.h>
#include <stdarg.h>
#include <inttypes.h>
#include <string.h>
#include <math.h>

struct _exec {
   eval_flags_t  flags;
   hash_t       *link_map;
};

////////////////////////////////////////////////////////////////////////////////
// Vcode interpreter

#define ITER_LIMIT 1000

typedef enum {
   VALUE_INVALID,
   VALUE_REAL,
   VALUE_INTEGER,
   VALUE_POINTER,
   VALUE_UARRAY,
   VALUE_CARRAY,
   VALUE_RECORD,
   VALUE_ACCESS,
   VALUE_CONTEXT
} value_kind_t;

typedef struct _value value_t;
typedef struct _eval_alloc eval_alloc_t;

struct _value {
   value_kind_t kind;
   uint32_t     length;
   union {
      double        real;
      int64_t       integer;
      value_t      *pointer;
      eval_frame_t *context;
   };
};

STATIC_ASSERT(sizeof(value_t) == 16);

struct _eval_alloc {
   eval_alloc_t *next;
   value_t       mem[0];
};

struct _eval_frame {
   eval_frame_t *context;
   unsigned      nvars;
   ident_t      *names;
   eval_alloc_t *allocs;
   value_t      *vars[0];
};

typedef struct {
   int            result;
   tree_t         hint;
   eval_flags_t   flags;
   bool           failed;
   eval_alloc_t  *allocs;
   int            iterations;
   int            op;
   eval_frame_t  *frame;
   value_t      **regs;
   exec_t        *exec;
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

static void eval_vcode(eval_state_t *state);

static vcode_unit_t eval_find_unit(ident_t func_name, eval_flags_t flags)
{
   vcode_unit_t vcode = vcode_find_unit(func_name);
   if (vcode == NULL) {
      ident_t strip_type_suffix = ident_until(func_name, '(');
      ident_t unit_name = ident_runtil(strip_type_suffix, '.');
      ident_t lib_name = ident_until(strip_type_suffix, '.');

      if (unit_name == lib_name)
         unit_name = func_name;

      lib_t lib = lib_require(lib_name);

      if (flags & EVAL_VERBOSE)
         notef("loading vcode for %s", istr(unit_name));

      if (!lib_load_vcode(lib, unit_name)) {
         if (flags & EVAL_WARN)
            warnf("cannot load vcode for %s", istr(unit_name));
         return NULL;
      }

      vcode = vcode_find_unit(func_name);
      if (vcode == NULL)
         fatal_trace("failed to load vcode for %s", istr(func_name));
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
         eval_dump(tb, value->pointer);
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
         int total = 1;
         for (unsigned dim = 0; dim < value[0].length; dim++) {
            if (dim > 0) tb_printf(tb, ", ");
            const int left   = value[1 + dim*2].integer;
            const int length = value[1 + dim*2 + 1].integer;
            const int right  =
               length < 0 ? left + length + 1 : left + length - 1;
            tb_printf(tb, "#[");
            if (length < 0)
               tb_printf(tb, "%d downto %d : ", left, right);
            else
               tb_printf(tb, "%d to %d : ", left, right);
            total *= abs(length);
         }
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
   eval_alloc_t *new =
      xmalloc_flex(sizeof(eval_alloc_t), count, sizeof(value_t));
   new->next = state->allocs;

   state->allocs = new;
   return new->mem;
}

static void eval_make_pointer_to(value_t *dst, value_t *src)
{
   dst->kind = VALUE_POINTER;

   if (src->kind == VALUE_CARRAY || src->kind == VALUE_RECORD)
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
   state->frame = xcalloc_flex(sizeof(eval_frame_t), nvars, sizeof(value_t *));
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

   if (vcode_unit_kind() == VCODE_UNIT_PACKAGE)
      state->frame->names = xmalloc_array(nvars, sizeof(ident_t));

   int vnext = 0;
   for (int i = 0; i < nvars; i++) {
      vcode_type_t vtype = vcode_var_type(i);
      value_t *value = state->frame->vars[i] = &(vslots[vnext]);
      vnext += eval_setup_var(vtype, value);

      if (state->frame->names != NULL)
         state->frame->names[i] = vcode_var_name(i);
   }
   assert(vnext == nvslots);

   int rnext = 0;
   for (int i = 0; i < nregs; i++) {
      state->regs[i] = &(rslots[rnext]);
      rnext += eval_slots_for_type(vcode_reg_type(i));
   }
   assert(rnext == nrslots);
}

static void eval_free_frame(eval_frame_t *frame)
{
   eval_alloc_t *it, *next;
   for (it = frame->allocs; it != NULL; it = next) {
      next = it->next;
      free(it);
   }

   free(frame->names);
   free(frame);
}

static void eval_cleanup_state(eval_state_t *state)
{
   if (state->frame != NULL) {
      assert(state->frame->allocs == NULL);
      eval_free_frame(state->frame);
      state->frame = NULL;
   }

   free(state->regs);
   state->regs = NULL;

   eval_alloc_t *it, *next;
   for (it = state->allocs; it != NULL; it = next) {
      next = it->next;
      free(it);
   }
   state->allocs = NULL;
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

   case VALUE_POINTER:
      {
         EVAL_ASSERT_VALUE(op, rhs, VALUE_INTEGER);
         vcode_type_t vtype = vtype_pointed(vcode_reg_type(result));
         const int stride = eval_slots_for_type(vtype);
         eval_make_pointer_to(dst, lhs->pointer + rhs->integer * stride);
         EVAL_ASSERT_VALID(op, dst->pointer);
      }
      break;

   default:
      fatal_trace("invalid value type in %s", __func__);
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

static void eval_op_div(int op, eval_state_t *state)
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
         dst->kind    = VALUE_INTEGER;
         dst->integer = lhs->integer / rhs->integer;
      }
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
         dst->kind    = VALUE_INTEGER;
         dst->integer =
            lhs->integer - (lhs->integer / rhs->integer) * rhs->integer;
      }
      break;

   default:
      fatal_trace("invalid value type in %s", __func__);
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
      if (rhs->integer >= 0) {
         dst->kind = VALUE_INTEGER;
         dst->integer = ipow(lhs->integer, rhs->integer);
      }
      else
         state->failed = true;
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

   vcode_state_t vcode_state;
   vcode_state_save(&vcode_state);

   vcode_unit_t vcode = eval_find_unit(func_name, state->flags);
   if (vcode == NULL) {
      EVAL_WARN(state, op, "function call to %s prevents "
                "constant folding", istr(func_name));
      state->failed = true;
      vcode_state_restore(&vcode_state);
      return;
   }

   value_t *arg0 = eval_get_reg(vcode_get_arg(op, 0), state);
   EVAL_ASSERT_VALUE(op, arg0, VALUE_CONTEXT);

   vcode_select_unit(vcode);
   vcode_select_block(0);

   eval_state_t new = {
      .result  = -1,
      .hint    = state->hint,
      .failed  = false,
      .flags   = state->flags | EVAL_BOUNDS,
      .exec    = state->exec,
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

   bool escaping_alloc = false;
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
         if (state->hint)
            tb_printf(tb, " (in %s)", istr(tree_ident(state->hint)));
         tb_printf(tb, " returned ");
         eval_dump(tb, new.regs[new.result]);
         notef("%s", tb_get(tb));
      }

      escaping_alloc = result->kind == VALUE_UARRAY
         || result->kind == VALUE_POINTER
         || result->kind == VALUE_ACCESS;
   }
   else
      escaping_alloc = true;   // Procedure may write to pointer argument

   if (escaping_alloc) {
      // Take ownership of the callee frame's allocations
      eval_alloc_t **tailp;
      for (tailp = &(state->allocs); *tailp; tailp = &((*tailp)->next))
         ;

      *tailp = new.allocs;
      new.allocs = NULL;
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

   value_t *src = eval_get_var(var, where);
   value_t *dst = eval_get_reg(vcode_get_result(op), state);

   eval_make_pointer_to(dst, src);
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

static void eval_op_bounds(int op, eval_state_t *state)
{
   value_t *reg = eval_get_reg(vcode_get_arg(op, 0), state);
   vcode_type_t bounds = vcode_get_type(op);

   switch (reg->kind) {
   case VALUE_INTEGER:
      {
         const int64_t low  = vtype_low(bounds);
         const int64_t high = vtype_high(bounds);
         if (low > high)
            break;
         else if (reg->integer < low || reg->integer > high) {
            if (state->flags & EVAL_BOUNDS) {
               if (state->hint != NULL && tree_kind(state->hint) == T_FCALL)
                  hint_at(tree_loc(state->hint), "while evaluating call to %s",
                          istr(tree_ident(state->hint)));

               switch ((bounds_kind_t)vcode_get_subkind(op)) {
               case BOUNDS_ARRAY_TO:
                  error_at(vcode_get_loc(op), "array index %"PRIi64" outside "
                           "bounds %"PRIi64" to %"PRIi64,
                           reg->integer, low, high);
                  break;

               case BOUNDS_ARRAY_DOWNTO:
                  error_at(vcode_get_loc(op), "array index %"PRIi64" outside "
                           "bounds %"PRIi64" downto %"PRIi64,
                           reg->integer, high, low);
                  break;

               default:
                  vcode_dump_with_mark(op, NULL, NULL);
                  fatal_trace("unhandled bounds kind %d in %s",
                              vcode_get_subkind(op), __func__);
               }
            }
            EVAL_WARN(state, op, "bounds check failure prevents "
                      "constant folding");
            state->failed = true;
         }
      }
      break;

   case VALUE_REAL:
      break;

   default:
      fatal_trace("invalid value type in %s", __func__);
   }
}

static void eval_op_dynamic_bounds(int op, eval_state_t *state)
{
   value_t *reg = eval_get_reg(vcode_get_arg(op, 0), state);
   value_t *low = eval_get_reg(vcode_get_arg(op, 1), state);
   value_t *high = eval_get_reg(vcode_get_arg(op, 2), state);

   switch (reg->kind) {
   case VALUE_INTEGER:
      {
         if (low->integer > high->integer)
            break;
         else if (reg->integer < low->integer || reg->integer > high->integer)
            state->failed = true;
      }
      break;

   case VALUE_REAL:
      break;

   default:
      fatal_trace("invalid value type in %s", __func__);
   }
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
   else
      state->failed = true;  // Cannot fold as would change runtime behaviour
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
      state->failed = severity->integer >= SEVERITY_ERROR;
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

   int length = 1;
   if (vcode_count_args(op) > 0) {
      value_t *length_reg = eval_get_reg(vcode_get_arg(op, 0), state);
      assert(length_reg->kind == VALUE_INTEGER);
      length = length_reg->integer;
   }

   vcode_type_t vtype = vcode_get_type(op);
   const int slots = length * eval_slots_for_type(vtype);

   value_t *new = eval_alloc(slots, state);

   unsigned off = 0;
   for (int i = 0; i < length; i++)
      off += eval_setup_var(vtype, new + off);

   eval_make_pointer_to(result, new);
}

static void eval_op_index_check(int op, eval_state_t *state)
{
   value_t *low = eval_get_reg(vcode_get_arg(op, 0), state);
   value_t *high = eval_get_reg(vcode_get_arg(op, 1), state);

   int64_t min, max;
   if (vcode_count_args(op) == 2) {
      vcode_type_t bounds = vcode_get_type(op);
      min = vtype_low(bounds);
      max = vtype_high(bounds);
   }
   else {
      min = eval_get_reg(vcode_get_arg(op, 2), state)->integer;
      max = eval_get_reg(vcode_get_arg(op, 3), state)->integer;
   }

   if (high->integer < low->integer)
      return;
   else if (low->integer < min)
      state->failed = true;    // TODO: report error here if EVAL_BOUNDS
   else if (high->integer > max)
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
         dst[offset++] = *field;
         break;
      case VALUE_CARRAY:
      case VALUE_RECORD:
         for (unsigned i = 0; i < field->length + 1; i++)
            dst[offset++] = field[i];
         break;
      default:
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

static void eval_op_memset(int op, eval_state_t *state)
{
   value_t *dst = eval_get_reg(vcode_get_arg(op, 0), state);
   value_t *fill = eval_get_reg(vcode_get_arg(op, 1), state);
   value_t *length = eval_get_reg(vcode_get_arg(op, 2), state);

   EVAL_ASSERT_VALUE(op, dst, VALUE_POINTER);
   EVAL_ASSERT_VALUE(op, length, VALUE_INTEGER);
   EVAL_ASSERT_VALUE(op, fill, VALUE_INTEGER);

   for (int i = 0; i < length->integer; i++)
      dst->pointer[i] = *fill;
}

static void eval_op_array_size(int op, eval_state_t *state)
{
   value_t *llen = eval_get_reg(vcode_get_arg(op, 0), state);
   value_t *rlen = eval_get_reg(vcode_get_arg(op, 1), state);

   if (rlen->integer != llen->integer) {
      if (state->flags & EVAL_BOUNDS) {
         error_at(vcode_get_loc(op), "length of target %"PRIi64" does not "
                  "match length of value %"PRIi64,
                  llen->integer, rlen->integer);
      }
      state->failed = true;
   }
}

static void eval_op_null(int op, eval_state_t *state)
{
   value_t *dst = eval_get_reg(vcode_get_result(op), state);
   dst->kind    = VALUE_ACCESS;
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

   int length;
   if (vcode_count_args(op) > 0)
      length = eval_get_reg(vcode_get_arg(op, 0), state)->integer;
   else
      length = eval_slots_for_type(vtype_pointed(vcode_reg_type(result)));

   value_t *dst = eval_get_reg(result, state);
   dst->kind    = VALUE_ACCESS;
   dst->pointer = eval_alloc(length, state);
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

static void eval_op_link_var(int op, eval_state_t *state)
{
   value_t *result = eval_get_reg(vcode_get_result(op), state);
   ident_t var_name = vcode_get_ident(op);
   ident_t unit_name = ident_runtil(var_name, '.');

   eval_frame_t *ctx = exec_link(state->exec, unit_name);
   if (ctx == NULL)
      state->failed = true;
   else {
      assert(ctx->names != NULL);

      for (unsigned i = 0; i < ctx->nvars; i++) {
         if (ctx->names[i] == var_name) {
            eval_make_pointer_to(result, ctx->vars[i]);
            return;
         }
      }

      fatal_trace("variable %s not found in %s", istr(var_name),
                  istr(unit_name));
   }
}

static void eval_op_link_package(int op, eval_state_t *state)
{
   value_t *result = eval_get_reg(vcode_get_result(op), state);
   result->kind = VALUE_CONTEXT;
   result->context = exec_link(state->exec, vcode_get_ident(op));

   state->failed = (result->context == NULL);
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

      case VCODE_OP_SUB:
         eval_op_sub(state->op, state);
         break;

      case VCODE_OP_MUL:
         eval_op_mul(state->op, state);
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

      case VCODE_OP_BOUNDS:
         eval_op_bounds(state->op, state);
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

      case VCODE_OP_DYNAMIC_BOUNDS:
         eval_op_dynamic_bounds(state->op, state);
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

      case VCODE_OP_ABS:
         eval_op_abs(state->op, state);
         break;

      case VCODE_OP_TEMP_STACK_MARK:
      case VCODE_OP_TEMP_STACK_RESTORE:
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

      case VCODE_OP_MEMSET:
         eval_op_memset(state->op, state);
         break;

      case VCODE_OP_ARRAY_SIZE:
         eval_op_array_size(state->op, state);
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

      case VCODE_OP_DEBUG_OUT:
         eval_op_debug_out(state->op, state);
         break;

      case VCODE_OP_LINK_VAR:
         eval_op_link_var(state->op, state);
         break;

      case VCODE_OP_LINK_SIGNAL:
      case VCODE_OP_PROTECTED_INIT:
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
         type_t elem = type_elem(type);
         tree = tree_new(T_AGGREGATE);
         unsigned offset = 1, pos = 0;
         while (offset < value->length + 1) {
            tree_t assoc = tree_new(T_ASSOC);
            tree_set_subkind(assoc, A_POS);
            tree_set_pos(assoc, pos++);
            tree_set_value(assoc, eval_value_to_tree(&(value[offset]),
                                                     elem, loc));
            tree_add_assoc(tree, assoc);

            offset += eval_next_field_offset(&(value[offset]));
         }
      }
      break;
   default:
      fatal_trace("cannot convert value %d to tree", value->kind);
   }

   tree_set_type(tree, type);
   tree_set_loc(tree, loc);
   return tree;
}

////////////////////////////////////////////////////////////////////////////////
// Public interface

exec_t *exec_new(eval_flags_t flags)
{
   static int verbose_env = -1;
   if (verbose_env == -1)
      verbose_env = getenv("NVC_EVAL_VERBOSE") != NULL;
   if (verbose_env)
      flags |= EVAL_VERBOSE;

   if (flags & EVAL_VERBOSE)
      flags |= EVAL_WARN | EVAL_BOUNDS;

   exec_t *ex = xcalloc(sizeof(exec_t));
   ex->link_map = hash_new(128, true);
   ex->flags    = flags;

   return ex;
}

void exec_free(exec_t *ex)
{
   const void *key;
   void *value;
   hash_iter_t it = HASH_BEGIN;
   while (hash_iter(ex->link_map, &it, &key, &value)) {
      if (value != (void *)-1)
         eval_free_frame((eval_frame_t *)value);
   }

   hash_free(ex->link_map);
   free(ex);
}

eval_scalar_t exec_call(exec_t *ex, ident_t func, eval_frame_t *context,
                        const char *fmt, ...)
{
   vcode_unit_t unit = eval_find_unit(func, EVAL_VERBOSE);
   assert(unit);

   assert(ex->flags & EVAL_FCALL);

   vcode_select_unit(unit);
   vcode_select_block(0);

   eval_state_t state = {
      .result = -1,
      .hint   = NULL,
      .failed = false,
      .flags  = ex->flags,
      .exec   = ex,
   };

   eval_setup_state(&state, context);

   value_t *p0 = eval_get_reg(0, &state);
   p0->kind = VALUE_CONTEXT;
   p0->context = context;

   va_list ap;
   va_start(ap, fmt);

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
         fatal_trace("invalid character '%c' in exec_call format", *fmt);
      }
   }
   va_end(ap);

   eval_vcode(&state);

   if (state.failed)
      fatal_trace("call to %s failed", istr(func));

   eval_scalar_t result = eval_get_scalar(eval_get_reg(state.result, &state));
   eval_cleanup_state(&state);

   return result;
}

eval_frame_t *exec_link(exec_t *ex, ident_t ident)
{
   eval_frame_t *ctx = hash_get(ex->link_map, ident);
   if (ctx == (eval_frame_t *)-1)
      return NULL;
   else if (ctx != NULL)
      return ctx;

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

   eval_state_t state = {
      .result = -1,
      .hint   = NULL,
      .failed = false,
      .flags  = flags,
      .exec   = ex,
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

   if (state.failed)
      return NULL;

   // Move the frame with the local variables out of the state
   eval_frame_t *frame = state.frame;
   frame->allocs = state.allocs;
   state.frame = NULL;
   state.allocs = NULL;

   eval_cleanup_state(&state);

   hash_put(ex->link_map, ident, frame);
   return frame;
}

eval_scalar_t exec_get_var(exec_t *ex, eval_frame_t *frame, unsigned nth)
{
   assert(nth < frame->nvars);
   return eval_get_scalar(frame->vars[nth]);
}

tree_t exec_fold(exec_t *ex, tree_t expr, vcode_unit_t thunk)
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
      .exec   = ex,
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

eval_flags_t exec_get_flags(exec_t *ex)
{
   return ex->flags;
}
