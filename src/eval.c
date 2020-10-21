//
//  Copyright (C) 2013-2020  Nick Gasson
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

#include "phase.h"
#include "util.h"
#include "common.h"
#include "vcode.h"

#include <assert.h>
#include <string.h>
#include <stdarg.h>
#include <stdlib.h>
#include <inttypes.h>
#include <math.h>
#include <float.h>

#define MAX_DIMS   4
#define ITER_LIMIT 1000

typedef enum {
   VALUE_INVALID,
   VALUE_REAL,
   VALUE_INTEGER,
   VALUE_POINTER,
   VALUE_UARRAY,
   VALUE_CARRAY,
   VALUE_RECORD,
   VALUE_HEAP_PROXY,
   VALUE_IMAGE_MAP,
   VALUE_ACCESS
} value_kind_t;

typedef struct value value_t;
typedef struct context context_t;

typedef struct {
   struct {
      int64_t      left;
      int64_t      right;
      range_kind_t dir;
   } dim[MAX_DIMS];
   int      ndims;
   value_t *data;
} uarray_t;

struct value {
   value_kind_t kind;
   uint32_t     length;
   union {
      double       real;
      int64_t      integer;
      value_t     *pointer;
      uarray_t    *uarray;
      value_t     *fields;
      image_map_t *image_map;
   };
};

struct context {
   context_t *parent;
   value_t   *regs;
   value_t   *vars;
   int        refcount;
};

struct eval_alloc_struct;
typedef struct eval_alloc_struct eval_alloc_t;
struct eval_alloc_struct {
   eval_alloc_t *next;
   char mem[];
};

typedef struct {
   context_t   *context;
   int          result;
   tree_t       fcall;
   eval_flags_t flags;
   bool         failed;
   eval_alloc_t *allocations;
   loc_t        last_loc;
   int          iterations;
} eval_state_t;

#define EVAL_WARN(t, ...) do {                                          \
      if (state->flags & EVAL_WARN)                                     \
         warn_at(tree_loc(t), __VA_ARGS__);                             \
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

static int errors = 0;

static void eval_vcode(eval_state_t *state);
static bool eval_possible(tree_t t, eval_flags_t flags, bool top_level);

static void eval_load_vcode(lib_t lib, tree_t unit, eval_flags_t flags)
{
   ident_t unit_name = tree_ident(unit);

   if (flags & EVAL_VERBOSE)
      notef("loading vcode for %s", istr(unit_name));

   char *name LOCAL = vcode_file_name(unit_name);
   fbuf_t *f = lib_fbuf_open(lib, name, FBUF_IN);
   if (f == NULL) {
      if (flags & EVAL_WARN)
         warnf("cannot load vcode for %s", istr(unit_name));
      return;
   }

   vcode_read(f);
   fbuf_close(f);
}

static vcode_unit_t eval_find_unit(ident_t func_name, eval_flags_t flags)
{
   vcode_unit_t vcode = vcode_find_unit(func_name);
   if (vcode == NULL) {
      ident_t strip_type_suffix = ident_until(func_name, "("[0]);
      ident_t unit_name = ident_runtil(strip_type_suffix, '.');
      ident_t lib_name = ident_until(strip_type_suffix, '.');

      lib_t lib;
      if (lib_name != unit_name && (lib = lib_find(lib_name, false)) != NULL) {
         tree_t unit = lib_get(lib, unit_name);
         if (unit != NULL) {
            eval_load_vcode(lib, unit, flags);

            if (tree_kind(unit) == T_PACKAGE) {
               ident_t body_name =
                  ident_prefix(unit_name, ident_new("body"), '-');
               tree_t body = lib_get(lib, body_name);
               if (body != NULL)
                  eval_load_vcode(lib, body, flags);
            }

            vcode = vcode_find_unit(func_name);
         }
      }
   }

   if (vcode == NULL && (flags & EVAL_VERBOSE))
      warnf("could not find vcode for unit %s", istr(func_name));

   return vcode;
}

static bool eval_have_lowered(tree_t func, eval_flags_t flags)
{
   if (tree_attr_str(func, builtin_i))
      return true;

   ident_t mangled = mangle_func(func, NULL);
   if (mangled == NULL)
      return false;
   else if (eval_find_unit(mangled, flags) == NULL) {
      if (!(flags & EVAL_LOWER))
         return false;
      else if (tree_kind(func) != T_FUNC_BODY)
         return false;

      return lower_func(func) != NULL;
   }
   else
      return true;
}

static bool eval_possible(tree_t t, eval_flags_t flags, bool top_level)
{
   switch (tree_kind(t)) {
   case T_FCALL:
      {
         if (!(flags & EVAL_FCALL))
            return false;
         else if (tree_flags(tree_ref(t)) & TREE_F_IMPURE) {
            if (flags & EVAL_WARN)
               warn_at(tree_loc(t),
                       "impure function call prevents constant folding");
            return false;
         }
         else if (!eval_have_lowered(tree_ref(t), flags))
            return false;

         const int nparams = tree_params(t);
         for (int i = 0; i < nparams; i++) {
            tree_t p = tree_value(tree_param(t, i));
            const bool is_fcall = tree_kind(p) == T_FCALL;
            if (top_level && (flags & EVAL_FOLDING) && is_fcall
                && type_is_scalar(tree_type(p)))
               return false;   // Would have been folded already if possible
            else if (is_fcall && !(flags & EVAL_FCALL))
               return false;
            else if (!eval_possible(p, flags, false))
               return false;
         }

         return true;
      }

   case T_LITERAL:
      return true;

   case T_TYPE_CONV:
      return eval_possible(tree_value(tree_param(t, 0)), flags, false);

   case T_QUALIFIED:
      return eval_possible(tree_value(t), flags, false);

   case T_REF:
      {
         tree_t decl = tree_ref(t);
         switch (tree_kind(decl)) {
         case T_UNIT_DECL:
         case T_ENUM_LIT:
            return true;

         case T_CONST_DECL:
            return eval_possible(tree_value(decl), flags, false);

         default:
            if (flags & EVAL_WARN)
               warn_at(tree_loc(t), "reference to %s prevents constant folding",
                       istr(tree_ident(t)));
            return false;
         }
      }

   case T_RECORD_REF:
      return eval_possible(tree_value(t), flags, false);

   case T_AGGREGATE:
      {
         const int nassocs = tree_assocs(t);
         for (int i = 0; i < nassocs; i++) {
            if (!eval_possible(tree_value(tree_assoc(t, i)), flags, false))
                return false;
         }

         return true;
      }

   default:
      if (flags & EVAL_WARN)
         warn_at(tree_loc(t), "expression prevents constant folding");
      return false;
   }
}

static void eval_dump(text_buf_t *tb, value_t *value, type_t type)
{
   switch (value->kind) {
   case VALUE_INTEGER:
      tb_printf(tb, "%"PRIi64, value->integer);
      break;
   case VALUE_REAL:
      tb_printf(tb, "%lf", value->real);
      break;
   case VALUE_POINTER:
      tb_printf(tb, "*");
      if (type != NULL && type_kind(type) == T_ACCESS)
         eval_dump(tb, value->pointer, type_base(type));
      else
         eval_dump(tb, value->pointer, type);
      break;
   case VALUE_CARRAY:
      tb_printf(tb, "[");
      for (unsigned i = 0; i < value->length; i++) {
         if (i > 0)
            tb_printf(tb, ",");
         eval_dump(tb, &(value->pointer[i]), type ? type_elem(type) : NULL);
      }
      tb_printf(tb, "]");
      break;
   case VALUE_UARRAY:
      tb_printf(tb, "#[");
      if (value->uarray->ndims == 1) {
         const int left  = value->uarray->dim[0].left;
         const int right = value->uarray->dim[0].right;
         for (int i = 0; i <= MAX(left, right) - MIN(left, right); i++) {
            if (i > 0)
               tb_printf(tb, ",");
            eval_dump(tb, &(value->uarray->data[i]),
                      type ? type_elem(type) : NULL);
         }
      }
      else
         tb_printf(tb, "...");
      tb_printf(tb, "]");
      break;
   case VALUE_RECORD:
      tb_printf(tb, "{");
      if (type == NULL)
         tb_printf(tb, "???");
      else {
         for (int i = 0; i < value->length; i++) {
            tree_t field = type_field(type, i);
            if (i > 0)
               tb_printf(tb, ", ");
            tb_printf(tb, "%s=>", istr(tree_ident(field)));
            eval_dump(tb, &(value->fields[i]), tree_type(field));
         }
      }
      tb_printf(tb, "}");
      break;
   default:
      tb_printf(tb, "<%d>", value->kind);
      break;
   }
}

__attribute__((noreturn))
static void eval_assert_fail(int op, value_t *value, const char *value_str,
                             const char *expect, const char *file, int line)
{
   vcode_dump_with_mark(op);
   if (expect == NULL)
      fatal_trace("Expected %s to have valid value (at %s:%d)",
                  value_str, file, line);
   else {
      LOCAL_TEXT_BUF tb = tb_new();
      eval_dump(tb, value, NULL);
      fatal_trace("Expected %s to have kind %s but is %s (at %s:%d)",
                  value_str, expect, tb_get(tb), file, line);
   }
}

static void *eval_alloc(size_t nbytes, eval_state_t *state)
{
   eval_alloc_t *new_alloc = xmalloc(sizeof(eval_alloc_t) + nbytes);
   new_alloc->next = state->allocations;
   state->allocations = new_alloc;
   return new_alloc->mem;
}

static bool eval_new_var(value_t *value, vcode_type_t type, eval_state_t *state)
{
   switch (vtype_kind(type)) {
   case VCODE_TYPE_CARRAY:
      value->kind = VALUE_CARRAY;
      if ((value->pointer =
           eval_alloc(sizeof(value_t) * vtype_size(type), state)) == NULL)
         return false;
      break;

   case VCODE_TYPE_INT:
      value->kind = VALUE_INTEGER;
      value->integer = 0;
      break;

   case VCODE_TYPE_REAL:
      value->kind = VALUE_REAL;
      value->real = 0;
      break;

   case VCODE_TYPE_UARRAY:
      value->kind = VALUE_UARRAY;
      value->uarray = NULL;
      break;

   case VCODE_TYPE_RECORD:
      {
         const int nfields = vtype_fields(type);
         value->kind = VALUE_RECORD;
         value->length = nfields;
         value->fields = eval_alloc(nfields * sizeof(value_t), state);

         if (value->fields == NULL)
            return false;

         for (int i = 0; i < nfields; i++) {
            if (!eval_new_var(&(value->fields[i]), vtype_field(type, i), state))
               return false;
         }
      }
      break;

   case VCODE_TYPE_ACCESS:
      value->kind = VALUE_ACCESS;
      value->pointer = NULL;
      break;

   default:
      fatal_at(tree_loc(state->fcall), "cannot evaluate variables with "
               "type %d", vtype_kind(type));
   }

   return true;
}

static context_t *eval_new_context(eval_state_t *state)
{
   const int nregs = vcode_count_regs();
   const int nvars = vcode_count_vars();

   void *mem = xcalloc(sizeof(context_t) + sizeof(value_t) * (nregs + nvars));

   context_t *context = mem;
   context->regs = (value_t *)((uint8_t *)mem + sizeof(context_t));
   context->vars = (value_t *)
      ((uint8_t *)mem + sizeof(context_t) + (nregs * sizeof(value_t)));

   context->refcount = 1;

   for (int i = 0; i < nvars; i++) {
      vcode_type_t type = vcode_var_type(i);

      value_t *value = &(context->vars[i]);
      if (vcode_var_use_heap(i)) {
         value->kind = VALUE_HEAP_PROXY;
         if ((value->pointer = eval_alloc(sizeof(value_t), state)) == NULL)
            goto fail;

         value = value->pointer;
      }

      if (!eval_new_var(value, type, state))
         goto fail;
   }

   return context;

 fail:
   free(context);
   return NULL;
}

static context_t *eval_ref_context(context_t *context)
{
   assert(context->refcount > 0);
   (context->refcount)++;
   return context;
}

static void eval_free_context(context_t *context)
{
   assert(context->refcount > 0);
   if (--(context->refcount) == 0) {
      if (context->parent)
         eval_free_context(context->parent);

      context->regs = NULL;
      context->vars = NULL;
      free(context);
   }
}

void eval_cleanup_state(eval_state_t *state)
{
   eval_alloc_t *next, *current;

   eval_free_context(state->context);
   for (current = state->allocations; current != NULL; current = next)
   {
      next = current->next;
      free(current);
   }
   state->allocations = NULL;
}

static value_t *eval_get_reg(vcode_reg_t reg, eval_state_t *state)
{
   return &(state->context->regs[reg]);
}

static value_t *eval_get_var(vcode_var_t var, context_t *context)
{
#if 0
   const int var_depth = vcode_var_context(var);

   context_t *context = state->context;
   for (int depth = vcode_unit_depth(); depth > var_depth; depth--) {
      if (context->parent == NULL) {
         assert(vcode_unit_kind() != VCODE_UNIT_THUNK);

         vcode_state_t vcode_state;
         vcode_state_save(&vcode_state);

         vcode_select_unit(vcode_unit_context());
         assert(vcode_unit_kind() == VCODE_UNIT_CONTEXT);
         vcode_select_block(0);

         context_t *new_context = eval_new_context(state);
         if (new_context == NULL) {
            state->failed = true;
            return NULL;
         }

         context->parent = new_context;

         eval_state_t new_state = {
            .context = new_context,
            .result  = -1,
            .fcall   = state->fcall,
            .failed  = false,
            .flags   = state->flags | EVAL_BOUNDS,
            .allocations = state->allocations
         };

         eval_vcode(&new_state);
         vcode_state_restore(&vcode_state);

         state->allocations = new_state.allocations;

         if (new_state.failed) {
            state->failed = true;
            return NULL;
         }
      }

      context = context->parent;
   }
#endif

   value_t *value = &(context->vars[var]);
   if (value->kind == VALUE_HEAP_PROXY)
      return value->pointer;
   else
      return value;
}

static int eval_value_cmp(value_t *lhs, value_t *rhs)
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
   value_t *dst = eval_get_reg(vcode_get_result(op), state);
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
      EVAL_ASSERT_VALUE(op, rhs, VALUE_INTEGER);
      dst->kind = VALUE_POINTER;
      dst->pointer = lhs->pointer + rhs->integer;
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
         error_at(&(state->last_loc), "division by zero");
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
         error_at(tree_loc(state->fcall), "division by zero");
         state->failed = true;
      }
      else {
         dst->kind    = VALUE_INTEGER;
         dst->integer = labs(lhs->integer % rhs->integer);
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
         error_at(tree_loc(state->fcall), "division by zero");
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

static void eval_op_fcall(int op, eval_state_t *state)
{
   vcode_state_t vcode_state;
   vcode_state_save(&vcode_state);

   const int nparams = vcode_count_args(op);
   value_t *params[nparams];
   for (int i = 0; i < nparams; i++)
      params[i] = eval_get_reg(vcode_get_arg(op, i), state);

   ident_t func_name = vcode_get_func(op);
   vcode_unit_t vcode = eval_find_unit(func_name, state->flags);
   if (vcode == NULL) {
      EVAL_WARN(state->fcall, "function call to %s prevents "
                "constant folding", istr(func_name));
      state->failed = true;
      vcode_state_restore(&vcode_state);
      return;
   }

   const bool nested = vcode_get_op(op) == VCODE_OP_NESTED_FCALL;

   vcode_select_unit(vcode);
   vcode_select_block(0);

   context_t *context = eval_new_context(state);
   if (context == NULL)
      return;

   if (nested)
      context->parent = eval_ref_context(state->context);

   for (int i = 0; i < nparams; i++)
      context->regs[i] = *params[i];

   eval_state_t new = {
      .context = context,
      .result  = -1,
      .fcall   = state->fcall,
      .failed  = false,
      .flags   = state->flags | EVAL_BOUNDS,
      .allocations = state->allocations
   };

   eval_vcode(&new);
   vcode_state_restore(&vcode_state);

   state->allocations = new.allocations;

   if (new.failed)
      state->failed = true;
   else if (vcode_get_result(op) != VCODE_INVALID_REG) {
      assert(new.result != -1);
      value_t *dst = eval_get_reg(vcode_get_result(op), state);
      EVAL_ASSERT_VALID(op, &(context->regs[new.result]));
      *dst = context->regs[new.result];

      if (state->flags & EVAL_VERBOSE) {
         const char *name = istr(vcode_get_func(op));
         const char *nest = istr(tree_ident(state->fcall));
         LOCAL_TEXT_BUF tb = tb_new();
         eval_dump(tb, &(context->regs[new.result]), NULL);
         notef("%s (in %s) returned %s", name, nest, tb_get(tb));
      }
   }

   eval_free_context(context);
}

static void eval_op_param_upref(int op, eval_state_t *state)
{
   context_t *where = state->context;
   const int hops = vcode_get_hops(op);
   for (int i = 0; i < hops; i++)
      where = where->parent;

   value_t *src = &(where->regs[vcode_get_arg(op, 0)]);
   value_t *dst = eval_get_reg(vcode_get_result(op), state);
   *dst = *src;
}

static void eval_op_var_upref(int op, eval_state_t *state)
{
   context_t *where = state->context;
   const int hops = vcode_get_hops(op);
   for (int i = 0; where && i < hops; i++)
      where = where->parent;

   if (where == NULL) {
      vcode_dump_with_mark(op);
      fatal_trace("upref outside eval context");
   }

   value_t *src = eval_get_var(vcode_get_address(op), where);
   value_t *dst = eval_get_reg(vcode_get_result(op), state);
   dst->kind = VALUE_POINTER;
   dst->pointer = src;
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
               hint_at(tree_loc(state->fcall), "while evaluating call to %s",
                       istr(tree_ident(state->fcall)));

               switch ((bounds_kind_t)vcode_get_subkind(op)) {
               case BOUNDS_ARRAY_TO:
                  error_at(&(state->last_loc), "array index %"PRIi64" outside "
                           "bounds %"PRIi64" to %"PRIi64,
                           reg->integer, low, high);
                  break;

               case BOUNDS_ARRAY_DOWNTO:
                  error_at(&(state->last_loc), "array index %"PRIi64" outside "
                           "bounds %"PRIi64" downto %"PRIi64,
                           reg->integer, high, low);
                  break;

               default:
                  fatal_trace("unhandled bounds kind %d in %s",
                              vcode_get_subkind(op), __func__);
               }

               errors++;
            }
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

   dst->kind = VALUE_POINTER;
   dst->length = nargs;
   if ((dst->pointer = eval_alloc(sizeof(value_t) * nargs, state))) {
      for (int i = 0; i < nargs; i++)
         dst->pointer[i] = *eval_get_reg(vcode_get_arg(op, i), state);
   }
}

static void eval_op_wrap(int op, eval_state_t *state)
{
   value_t *dst = eval_get_reg(vcode_get_result(op), state);
   value_t *src = eval_get_reg(vcode_get_arg(op, 0), state);

   EVAL_ASSERT_VALUE(op, src, VALUE_POINTER);

   dst->kind = VALUE_UARRAY;
   if ((dst->uarray = eval_alloc(sizeof(uarray_t), state)) == NULL)
      return;
   dst->uarray->data = src->pointer;

   const int ndims = (vcode_count_args(op) - 1) / 3;
   if (ndims > MAX_DIMS) {
      state->failed = true;
      EVAL_WARN(state->fcall, "%d dimensional array prevents "
                "constant folding", ndims);
   }
   else {
      dst->uarray->ndims = ndims;
      for (int i = 0; i < ndims; i++) {
         dst->uarray->dim[i].left =
            eval_get_reg(vcode_get_arg(op, (i * 3) + 1), state)->integer;
         dst->uarray->dim[i].right =
            eval_get_reg(vcode_get_arg(op, (i * 3) + 2), state)->integer;
         dst->uarray->dim[i].dir =
            eval_get_reg(vcode_get_arg(op, (i * 3) + 3), state)->integer;
      }
   }
}

static void eval_op_store(int op, eval_state_t *state)
{
   value_t *src = eval_get_reg(vcode_get_arg(op, 0), state);
   value_t *var = eval_get_var(vcode_get_address(op), state->context);

   if (var != NULL)
      *var = *src;
}

static void eval_op_load(int op, eval_state_t *state)
{
   value_t *dst = eval_get_reg(vcode_get_result(op), state);
   value_t *var = eval_get_var(vcode_get_address(op), state->context);

   if (var != NULL) {
      EVAL_ASSERT_VALID(op, var);
      *dst = *var;
   }
}

static void eval_op_unwrap(int op, eval_state_t *state)
{
   value_t *dst = eval_get_reg(vcode_get_result(op), state);
   value_t *src = eval_get_reg(vcode_get_arg(op, 0), state);

   EVAL_ASSERT_VALUE(op, src, VALUE_UARRAY);

   dst->kind    = VALUE_POINTER;
   dst->pointer = src->uarray->data;
}

static void eval_op_uarray_len(int op, eval_state_t *state)
{
   value_t *dst = eval_get_reg(vcode_get_result(op), state);
   value_t *src = eval_get_reg(vcode_get_arg(op, 0), state);

   const int dim = vcode_get_dim(op);
   const int64_t left = src->uarray->dim[dim].left;
   const int64_t right = src->uarray->dim[dim].right;
   const range_kind_t dir = src->uarray->dim[dim].dir;

   const int64_t len = (dir == RANGE_TO ? right - left : left - right) + 1;

   dst->kind    = VALUE_INTEGER;
   dst->integer = MAX(len, 0);
}

static void eval_op_uarray_dir(int op, eval_state_t *state)
{
   value_t *dst = eval_get_reg(vcode_get_result(op), state);
   value_t *src = eval_get_reg(vcode_get_arg(op, 0), state);

   const int dim = vcode_get_dim(op);
   const range_kind_t dir = src->uarray->dim[dim].dir;

   dst->kind    = VALUE_INTEGER;
   dst->integer = dir;
}

static void eval_op_memcmp(int op, eval_state_t *state)
{
   value_t *dst = eval_get_reg(vcode_get_result(op), state);
   value_t *lhs = eval_get_reg(vcode_get_arg(op, 0), state);
   value_t *rhs = eval_get_reg(vcode_get_arg(op, 1), state);
   value_t *len = eval_get_reg(vcode_get_arg(op, 2), state);

   dst->kind    = VALUE_INTEGER;
   dst->integer = 1;

   EVAL_ASSERT_VALUE(op, lhs, VALUE_POINTER);
   EVAL_ASSERT_VALUE(op, rhs, VALUE_POINTER);

   for (int i = 0; i < len->integer; i++) {
      if (eval_value_cmp(&(lhs->pointer[i]), &(rhs->pointer[i]))) {
         dst->integer = 0;
         return;
      }
   }
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

static void eval_op_jump(int op, eval_state_t *state)
{
   vcode_select_block(vcode_get_target(op, 0));
   eval_vcode(state);
}

static void eval_op_cond(int op, eval_state_t *state)
{
   value_t *test = eval_get_reg(vcode_get_arg(op, 0), state);

   const vcode_block_t next = vcode_get_target(op, !(test->integer));
   vcode_select_block(next);
   eval_vcode(state);
}

static void eval_op_undefined(int op, eval_state_t *state)
{
   EVAL_WARN(state->fcall, "reference to object without defined "
             "value in this phase prevents constant folding");

   state->failed = true;
}

static void eval_op_index(int op, eval_state_t *state)
{
   value_t *value = eval_get_var(vcode_get_address(op), state->context);
   if (value == NULL)
      return;

   value_t *dst = eval_get_reg(vcode_get_result(op), state);
   dst->kind = VALUE_POINTER;
   dst->pointer = (value->kind == VALUE_CARRAY) ? value->pointer : value;
}

static void eval_op_load_indirect(int op, eval_state_t *state)
{
   value_t *dst = eval_get_reg(vcode_get_result(op), state);
   value_t *src = eval_get_reg(vcode_get_arg(op, 0), state);

   EVAL_ASSERT_VALUE(op, src, VALUE_POINTER);
   EVAL_ASSERT_VALID(op, src->pointer);

   *dst = *(src->pointer);
}

static void eval_deep_copy(int op, value_t *dst, value_t *src)
{
   EVAL_ASSERT_VALUE(op, dst, VALUE_RECORD);
   assert(dst->length == src->length);

   for (int i = 0; i < src->length; i++) {
      if (src->fields[i].kind == VALUE_RECORD)
         eval_deep_copy(op, dst->fields + i, src->fields + i);
      else
         dst->fields[i] = src->fields[i];
   }
}

static void eval_op_store_indirect(int op, eval_state_t *state)
{
   vcode_reg_t dst_reg = vcode_get_arg(op, 1);

   value_t *dst = eval_get_reg(dst_reg, state);
   value_t *src = eval_get_reg(vcode_get_arg(op, 0), state);

   EVAL_ASSERT_VALUE(op, dst, VALUE_POINTER);

   if (src->kind == VALUE_RECORD)
      eval_deep_copy(op, dst->pointer, src);
   else
      *(dst->pointer) = *src;
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

   vcode_select_block(target);
   eval_vcode(state);
}

static void eval_op_copy(int op, eval_state_t *state)
{
   value_t *dst = eval_get_reg(vcode_get_arg(op, 0), state);
   value_t *src = eval_get_reg(vcode_get_arg(op, 1), state);

   EVAL_ASSERT_VALUE(op, dst, VALUE_POINTER);
   EVAL_ASSERT_VALUE(op, src, VALUE_POINTER);

   int count = 1;
   if (vcode_count_args(op) > 2) {
      value_t *count_val = eval_get_reg(vcode_get_arg(op, 2), state);
      EVAL_ASSERT_VALUE(op, count_val, VALUE_INTEGER);
      count = count_val->integer;
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
      eval_message(text, length, severity, &(state->last_loc), "Report");
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
         eval_message(text, length, severity, &(state->last_loc), "Assertion");
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

   result->kind = VALUE_POINTER;

   vcode_type_t vtype = vcode_get_type(op);
   switch (vtype_kind(vtype)) {
   case VCODE_TYPE_RECORD:
      if ((result->pointer = eval_alloc(sizeof(value_t), state))) {
         const int nfields = vtype_fields(vtype);
         result->pointer->kind = VALUE_RECORD;
         result->pointer->length = nfields;
         result->pointer->fields = eval_alloc(sizeof(value_t) * nfields, state);
      }
      break;

   default:
      result->pointer = eval_alloc(sizeof(value_t) * length, state);
      break;
   }
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

static void eval_op_image_map(int op, eval_state_t *state)
{
   image_map_t *map = eval_alloc(sizeof(image_map_t), state);
   if (map == NULL)
      return;

   vcode_get_image_map(op, map);

   value_t *result = eval_get_reg(vcode_get_result(op), state);
   result->kind = VALUE_IMAGE_MAP;
   result->image_map = map;
}

static void eval_op_image(int op, eval_state_t *state)
{
   value_t *object = eval_get_reg(vcode_get_arg(op, 0), state);
   char *buf LOCAL = NULL;

   if (vcode_count_args(op) == 1) {
      // No image map
      switch (object->kind) {
      case VALUE_INTEGER:
         buf = xasprintf("%"PRIi64, object->integer);
         break;

      case VALUE_REAL:
         buf = xasprintf("%.*g", DBL_DIG + 3, object->real);
         break;

      default:
         fatal_trace("bad value type for image operation");
      }
   }
   else {
      value_t *map = eval_get_reg(vcode_get_arg(op, 1), state);
      EVAL_ASSERT_VALUE(op, map, VALUE_IMAGE_MAP);

      switch (map->image_map->kind) {
      case IMAGE_ENUM:
         if (object->integer < 0 || object->integer >= map->image_map->nelems)
            fatal_trace("invalid enum value %"PRIi64, object->integer);
         buf = xasprintf("%s", istr(map->image_map->elems[object->integer]));
         break;

      case IMAGE_PHYSICAL:
         buf = xasprintf("%"PRIi64" %s", object->integer,
                         istr(map->image_map->elems[0]));
         break;

      default:
         fatal_trace("unexpected image map kind %d", map->image_map->kind);
      }
   }

   size_t len = strlen(buf);
   value_t *dst = eval_get_reg(vcode_get_result(op), state);
   dst->kind = VALUE_UARRAY;
   if ((dst->uarray = eval_alloc(sizeof(uarray_t), state)) == NULL)
      return;
   if ((dst->uarray->data = eval_alloc(sizeof(value_t) * len, state)) == NULL)
      return;

   dst->uarray->ndims = 1;
   dst->uarray->dim[0].left  = 1;
   dst->uarray->dim[0].right = len;
   dst->uarray->dim[0].dir   = RANGE_TO;

   for (size_t i = 0; i < len; i++) {
      value_t *ch = &(dst->uarray->data[i]);
      ch->kind = VALUE_INTEGER;
      ch->integer = buf[i];
   }
}

static void eval_op_uarray_left(int op, eval_state_t *state)
{
   value_t *array = eval_get_reg(vcode_get_arg(op, 0), state);
   value_t *dst = eval_get_reg(vcode_get_result(op), state);

   EVAL_ASSERT_VALUE(op, array, VALUE_UARRAY);

   dst->kind = VALUE_INTEGER;
   dst->integer = array->uarray->dim[vcode_get_dim(op)].left;
}

static void eval_op_uarray_right(int op, eval_state_t *state)
{
   value_t *array = eval_get_reg(vcode_get_arg(op, 0), state);
   value_t *dst = eval_get_reg(vcode_get_result(op), state);

   EVAL_ASSERT_VALUE(op, array, VALUE_UARRAY);

   dst->kind = VALUE_INTEGER;
   dst->integer = array->uarray->dim[vcode_get_dim(op)].right;
}

static void eval_op_const_record(int op, eval_state_t *state)
{
   value_t *dst = eval_get_reg(vcode_get_result(op), state);

   const int nfields = vcode_count_args(op);
   dst->kind = VALUE_RECORD;
   dst->length = nfields;
   if ((dst->fields = eval_alloc(nfields * sizeof(value_t), state)) == NULL)
      return;

   vcode_type_t vtype = vcode_reg_type(vcode_get_result(op));
   for (int i = 0; i < nfields; i++) {
      dst->fields[i] = *eval_get_reg(vcode_get_arg(op, i), state);
      if (vtype_kind(vtype_field(vtype, i)) == VCODE_TYPE_CARRAY)
         dst->fields[i].kind = VALUE_CARRAY;
   }
}

static void eval_op_record_ref(int op, eval_state_t *state)
{
   value_t *ptr = eval_get_reg(vcode_get_arg(op, 0), state);
   value_t *dst = eval_get_reg(vcode_get_result(op), state);

   EVAL_ASSERT_VALUE(op, ptr, VALUE_POINTER);
   EVAL_ASSERT_VALUE(op, ptr->pointer, VALUE_RECORD);

   value_t *field = &(ptr->pointer->fields[vcode_get_field(op)]);

   dst->kind = VALUE_POINTER;
   dst->pointer = (field->kind == VALUE_CARRAY) ? field->pointer : field;
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

static void eval_op_bit_shift(int op, eval_state_t *state)
{
   value_t *data = eval_get_reg(vcode_get_arg(op, 0), state);
   value_t *length = eval_get_reg(vcode_get_arg(op, 1), state);
   value_t *dir = eval_get_reg(vcode_get_arg(op, 2), state);
   value_t *shift = eval_get_reg(vcode_get_arg(op, 3), state);
   value_t *dst = eval_get_reg(vcode_get_result(op), state);

   bit_shift_kind_t kind = vcode_get_subkind(op);

   EVAL_ASSERT_VALUE(op, data, VALUE_POINTER);
   EVAL_ASSERT_VALUE(op, length, VALUE_INTEGER);
   EVAL_ASSERT_VALUE(op, dir, VALUE_INTEGER);
   EVAL_ASSERT_VALUE(op, shift, VALUE_INTEGER);

   int shift_i = shift->integer;
   if (shift_i < 0) {
      kind = kind ^ 1;
      shift_i = -shift_i;
   }

   shift_i %= length->integer;

   value_t *buf = eval_alloc(sizeof(value_t) * length->integer, state);
   if (buf == NULL)
      return;

   for (int i = 0; i < length->integer; i++) {
      buf[i].kind = VALUE_INTEGER;
      switch (kind) {
      case BIT_SHIFT_SLL:
         buf[i].integer = (i < length->integer - shift_i)
            ? data->pointer[i + shift_i].integer : 0;
         break;
      case BIT_SHIFT_SRL:
         buf[i].integer = (i >= shift_i)
            ? data->pointer[i - shift_i].integer : 0;
         break;
      case BIT_SHIFT_SLA:
         buf[i].integer = (i < length->integer - shift_i)
            ? data->pointer[i + shift_i].integer
            : data->pointer[length->integer - 1].integer;
         break;
      case BIT_SHIFT_SRA:
         buf[i].integer = (i >= shift_i)
            ? data->pointer[i - shift_i].integer : data->pointer[0].integer;
         break;
      case BIT_SHIFT_ROL:
         buf[i].integer = (i < length->integer - shift_i)
            ? data->pointer[i + shift_i].integer
            : data->pointer[(i + shift_i) % length->integer].integer;
         break;
      case BIT_SHIFT_ROR:
         buf[i].integer = (i >= shift_i)
            ? data->pointer[i - shift_i].integer
            : data->pointer[length->integer + i - shift_i].integer;
         break;
      }
   }

   dst->kind = VALUE_UARRAY;
   if ((dst->uarray = eval_alloc(sizeof(uarray_t), state)) == NULL)
      return;
   dst->uarray->data = buf;
   dst->uarray->ndims = 1;
   dst->uarray->dim[0].left =
      (dir->integer == RANGE_TO) ? 0 : length->integer - 1;
   dst->uarray->dim[0].right =
      (dir->integer == RANGE_TO) ? length->integer - 1 : 0;
   dst->uarray->dim[0].dir = dir->integer;
}

static void eval_op_array_size(int op, eval_state_t *state)
{
   value_t *llen = eval_get_reg(vcode_get_arg(op, 0), state);
   value_t *rlen = eval_get_reg(vcode_get_arg(op, 1), state);

   if (rlen->integer != llen->integer) {
      if (state->flags & EVAL_BOUNDS) {
         error_at(&(state->last_loc), "length of target %"PRIi64" does not "
                  "match length of value %"PRIi64,
                  llen->integer, rlen->integer);
      }
      state->failed = true;
   }
}

static void eval_op_debug_info(int op, eval_state_t *state)
{
   state->last_loc = *vcode_get_loc(op);
}

static void eval_op_null(int op, eval_state_t *state)
{
   value_t *dst = eval_get_reg(vcode_get_result(op), state);
   dst->kind    = VALUE_ACCESS;
   dst->pointer = NULL;
}

static void eval_op_pcall(int op, eval_state_t *state)
{
   EVAL_WARN(state->fcall, "procedure call to %s prevents "
             "constant folding", istr(vcode_get_func(op)));
   state->failed = true;
}

static void eval_op_new(int op, eval_state_t *state)
{
   int length = 1;
   if (vcode_count_args(op) > 0)
      length = eval_get_reg(vcode_get_arg(op, 0), state)->integer;

   value_t *dst = eval_get_reg(vcode_get_result(op), state);
   dst->kind    = VALUE_ACCESS;
   dst->pointer = eval_alloc(sizeof(value_t) * length, state);
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
}

static void eval_op_null_check(int op, eval_state_t *state)
{
   value_t *access = eval_get_reg(vcode_get_arg(op, 0), state);
   EVAL_ASSERT_VALUE(op, access, VALUE_ACCESS);

   if (access->pointer == NULL) {
      if (state->flags & EVAL_BOUNDS)
         error_at(&(state->last_loc), "null access dereference");
      state->failed = true;
   }
}

static void eval_op_bitvec_op(int op, eval_state_t *state)
{
   value_t *lhs_data = eval_get_reg(vcode_get_arg(op, 0), state);
   value_t *lhs_len = eval_get_reg(vcode_get_arg(op, 1), state);
   value_t *lhs_dir = eval_get_reg(vcode_get_arg(op, 2), state);

   value_t *rhs_data = NULL;
   value_t *rhs_len = NULL;
   value_t *rhs_dir = NULL;
   if (vcode_count_args(op) > 3) {
      rhs_data = eval_get_reg(vcode_get_arg(op, 3), state);
      rhs_len = eval_get_reg(vcode_get_arg(op, 4), state);
      rhs_dir = eval_get_reg(vcode_get_arg(op, 5), state);
   }

   (void)rhs_dir;

   EVAL_ASSERT_VALUE(op, lhs_data, VALUE_POINTER);
   EVAL_ASSERT_VALUE(op, lhs_len, VALUE_INTEGER);
   EVAL_ASSERT_VALUE(op, lhs_dir, VALUE_INTEGER);

   const int result_len = rhs_len == NULL
      ? lhs_len->integer
      : MIN(rhs_len->integer, lhs_len->integer);

   value_t *dst = eval_get_reg(vcode_get_result(op), state);

   value_t *buf = eval_alloc(sizeof(value_t) * result_len, state);
   if (buf == NULL)
      return;

   dst->kind = VALUE_UARRAY;
   if ((dst->uarray = eval_alloc(sizeof(uarray_t), state)) == NULL)
      return;
   dst->uarray->data = buf;
   dst->uarray->ndims = 1;
   dst->uarray->dim[0].left =
      (lhs_dir->integer == RANGE_TO) ? 0 : result_len - 1;
   dst->uarray->dim[0].right =
      (lhs_dir->integer == RANGE_TO) ? result_len - 1 : 0;
   dst->uarray->dim[0].dir = lhs_dir->integer;

   const bit_vec_op_kind_t kind = vcode_get_subkind(op);

   for (int i = 0; i < result_len; i++) {
      buf[i].kind = VALUE_INTEGER;
      switch (kind) {
      case BIT_VEC_AND:
         buf[i].integer =
            lhs_data->pointer[i].integer & rhs_data->pointer[i].integer;
         break;
      case BIT_VEC_OR:
         buf[i].integer =
            lhs_data->pointer[i].integer | rhs_data->pointer[i].integer;
         break;
      case BIT_VEC_NOT:
         buf[i].integer = !lhs_data->pointer[i].integer;
         break;
      case BIT_VEC_XOR:
         buf[i].integer =
            lhs_data->pointer[i].integer ^ rhs_data->pointer[i].integer;
         break;
      case BIT_VEC_XNOR:
         buf[i].integer =
            !(lhs_data->pointer[i].integer ^ rhs_data->pointer[i].integer);
         break;
      case BIT_VEC_NAND:
         buf[i].integer =
            !(lhs_data->pointer[i].integer & rhs_data->pointer[i].integer);
         break;
      case BIT_VEC_NOR:
         buf[i].integer =
            !(lhs_data->pointer[i].integer | rhs_data->pointer[i].integer);
         break;
      }
   }
}

static void eval_op_addi(int op, eval_state_t *state)
{
   value_t *dst = eval_get_reg(vcode_get_result(op), state);
   value_t *lhs = eval_get_reg(vcode_get_arg(op, 0), state);
   int64_t rhs = vcode_get_value(op);

   switch (lhs->kind) {
   case VALUE_INTEGER:
      dst->kind    = VALUE_INTEGER;
      dst->integer = lhs->integer + rhs;
      break;

   case VALUE_REAL:
      dst->kind = VALUE_REAL;
      dst->real = lhs->real + rhs;
      break;

   case VALUE_POINTER:
      dst->kind = VALUE_POINTER;
      dst->pointer = lhs->pointer + rhs;
      break;

   default:
      fatal_trace("invalid value type in %s", __func__);
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

static void eval_vcode(eval_state_t *state)
{
   if (++(state->iterations) >= ITER_LIMIT) {
      EVAL_WARN(state->fcall, "iteration limit reached while evaluating %s",
                istr(tree_ident(state->fcall)));
      state->failed = true;
      return;
   }

   const int nops = vcode_count_ops();
   for (int i = 0; i < nops && !(state->failed); i++) {
      switch (vcode_get_op(i)) {
      case VCODE_OP_COMMENT:
         break;

      case VCODE_OP_CONST:
         eval_op_const(i, state);
         break;

      case VCODE_OP_CONST_REAL:
         eval_op_const_real(i, state);
         break;

      case VCODE_OP_RETURN:
         eval_op_return(i, state);
         return;

      case VCODE_OP_NOT:
         eval_op_not(i, state);
         break;

      case VCODE_OP_ADD:
         eval_op_add(i, state);
         break;

      case VCODE_OP_SUB:
         eval_op_sub(i, state);
         break;

      case VCODE_OP_MUL:
         eval_op_mul(i, state);
         break;

      case VCODE_OP_DIV:
         eval_op_div(i, state);
         break;

      case VCODE_OP_CMP:
         eval_op_cmp(i, state);
         break;

      case VCODE_OP_CAST:
         eval_op_cast(i, state);
         break;

      case VCODE_OP_NEG:
         eval_op_neg(i, state);
         break;

      case VCODE_OP_FCALL:
      case VCODE_OP_NESTED_FCALL:
         if (state->flags & EVAL_FCALL)
            eval_op_fcall(i, state);
         else
            state->failed = true;
         break;

      case VCODE_OP_BOUNDS:
         eval_op_bounds(i, state);
         break;

      case VCODE_OP_CONST_ARRAY:
         eval_op_const_array(i, state);
         break;

      case VCODE_OP_WRAP:
         eval_op_wrap(i, state);
         break;

      case VCODE_OP_STORE:
         eval_op_store(i, state);
         break;

      case VCODE_OP_UNWRAP:
         eval_op_unwrap(i, state);
         break;

      case VCODE_OP_UARRAY_LEN:
         eval_op_uarray_len(i, state);
         break;

      case VCODE_OP_MEMCMP:
         eval_op_memcmp(i, state);
         break;

      case VCODE_OP_AND:
         eval_op_and(i, state);
         break;

      case VCODE_OP_OR:
         eval_op_or(i, state);
         break;

      case VCODE_OP_COND:
         eval_op_cond(i, state);
         return;

      case VCODE_OP_JUMP:
         eval_op_jump(i, state);
         return;

      case VCODE_OP_LOAD:
         eval_op_load(i, state);
         break;

      case VCODE_OP_UNDEFINED:
         eval_op_undefined(i, state);
         break;

      case VCODE_OP_CASE:
         eval_op_case(i, state);
         return;

      case VCODE_OP_MOD:
         eval_op_mod(i, state);
         break;

      case VCODE_OP_REM:
         eval_op_rem(i, state);
         break;

      case VCODE_OP_DYNAMIC_BOUNDS:
         eval_op_dynamic_bounds(i, state);
         break;

      case VCODE_OP_INDEX:
         eval_op_index(i, state);
         break;

      case VCODE_OP_COPY:
         eval_op_copy(i, state);
         break;

      case VCODE_OP_LOAD_INDIRECT:
         eval_op_load_indirect(i, state);
         break;

      case VCODE_OP_STORE_INDIRECT:
         eval_op_store_indirect(i, state);
         break;

      case VCODE_OP_REPORT:
         eval_op_report(i, state);
         break;

      case VCODE_OP_ASSERT:
         eval_op_assert(i, state);
         break;

      case VCODE_OP_SELECT:
         eval_op_select(i, state);
         break;

      case VCODE_OP_ALLOCA:
         eval_op_alloca(i, state);
         break;

      case VCODE_OP_INDEX_CHECK:
         eval_op_index_check(i, state);
         break;

      case VCODE_OP_ABS:
         eval_op_abs(i, state);
         break;

      case VCODE_OP_IMAGE:
         eval_op_image(i, state);
         break;

      case VCODE_OP_HEAP_SAVE:
      case VCODE_OP_HEAP_RESTORE:
         break;

      case VCODE_OP_UARRAY_LEFT:
         eval_op_uarray_left(i, state);
         break;

      case VCODE_OP_UARRAY_RIGHT:
         eval_op_uarray_right(i, state);
         break;

      case VCODE_OP_UARRAY_DIR:
         eval_op_uarray_dir(i, state);
         break;

      case VCODE_OP_EXP:
         eval_op_exp(i, state);
         break;

      case VCODE_OP_CONST_RECORD:
         eval_op_const_record(i, state);
         break;

      case VCODE_OP_RECORD_REF:
         eval_op_record_ref(i, state);
         break;

      case VCODE_OP_MEMSET:
         eval_op_memset(i, state);
         break;

      case VCODE_OP_BIT_SHIFT:
         eval_op_bit_shift(i, state);
         break;

      case VCODE_OP_ARRAY_SIZE:
         eval_op_array_size(i, state);
         break;

      case VCODE_OP_IMAGE_MAP:
         eval_op_image_map(i, state);
         break;

      case VCODE_OP_DEBUG_INFO:
         eval_op_debug_info(i, state);
         break;

      case VCODE_OP_NULL:
         eval_op_null(i, state);
         break;

      case VCODE_OP_PCALL:
         eval_op_pcall(i, state);
         break;

      case VCODE_OP_NEW:
         eval_op_new(i, state);
         break;

      case VCODE_OP_ALL:
         eval_op_all(i, state);
         break;

      case VCODE_OP_NULL_CHECK:
         eval_op_null_check(i, state);
         break;

      case VCODE_OP_DEALLOCATE:
         eval_op_deallocate(i, state);
         break;

      case VCODE_OP_BIT_VEC_OP:
         eval_op_bitvec_op(i, state);
         break;

      case VCODE_OP_ADDI:
         eval_op_addi(i, state);
         break;

      case VCODE_OP_PARAM_UPREF:
         eval_op_param_upref(i, state);
         break;

      case VCODE_OP_VAR_UPREF:
         eval_op_var_upref(i, state);
         break;

      case VCODE_OP_RANGE_NULL:
         eval_op_range_null(i, state);
         break;

      default:
         vcode_dump();
         fatal("cannot evaluate vcode op %s", vcode_op_string(vcode_get_op(i)));
      }
   }
}

static tree_t eval_value_to_tree(value_t *value, type_t type, const loc_t *loc)
{
   tree_t tree = NULL;

   switch (value->kind) {
   case VALUE_INTEGER:
      if (type_is_enum(type)) {
         type_t enum_type = type_base_recur(type);
         tree_t lit = type_enum_literal(enum_type, value->integer);

         tree = tree_new(T_REF);
         tree_set_ref(tree, lit);
         tree_set_ident(tree, tree_ident(lit));
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
      if (type_is_record(type))
         return eval_value_to_tree(value->pointer, type, loc);
      else
         fatal_trace("pointer cannot be converted to tree");
   case VALUE_RECORD:
      {
         const int nfields = type_fields(type);
         assert(nfields == value->length);
         tree = tree_new(T_AGGREGATE);
         for (int i = 0; i < nfields; i++) {
            tree_t field = type_field(type, i);
            tree_t assoc = tree_new(T_ASSOC);
            tree_set_subkind(assoc, A_POS);
            tree_set_pos(assoc, i);
            tree_set_value(assoc, eval_value_to_tree(&(value->fields[i]),
                                                     tree_type(field), loc));
            tree_add_assoc(tree, assoc);
         }
      }
      break;
   case VALUE_CARRAY:
      {
         type_t elem = type_elem(type);
         tree = tree_new(T_AGGREGATE);
         for (int i = 0; i < value->length; i++) {
            tree_t assoc = tree_new(T_ASSOC);
            tree_set_subkind(assoc, A_POS);
            tree_set_pos(assoc, i);
            tree_set_value(assoc,
                           eval_value_to_tree(&(value->pointer[i]), elem, loc));
            tree_add_assoc(tree, assoc);
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

static bool eval_can_represent_type(type_t type)
{
   if (type_is_scalar(type))
      return true;
   else if (type_is_array(type))
      return eval_can_represent_type(type_elem(type));
   else if (type_is_record(type)) {
      const int nfields = type_fields(type);
      for (int i = 0; i < nfields; i++) {
         if (!eval_can_represent_type(tree_type(type_field(type, i))))
            return false;
      }
      return true;
   }
   else
      return false;
}

tree_t eval(tree_t expr, eval_flags_t flags)
{
   static int verbose_env = -1;
   if (verbose_env == -1)
      verbose_env = getenv("NVC_EVAL_VERBOSE") != NULL;
   if (verbose_env)
      flags |= EVAL_VERBOSE;

   if (flags & EVAL_VERBOSE)
      flags |= EVAL_WARN | EVAL_BOUNDS;

   const tree_kind_t kind = tree_kind(expr);

   type_t type = tree_type(expr);
   if (type_is_array(type))
      return expr;   // TODO: eval for array results
   else if (!eval_can_represent_type(type))
      return expr;
   else if (kind == T_FCALL && (tree_flags(tree_ref(expr)) & TREE_F_IMPURE))
      return expr;
   else if (!eval_possible(expr, flags, true))
      return expr;

   vcode_unit_t thunk = lower_thunk(expr);
   if (thunk == NULL)
      return expr;

   if (flags & EVAL_VERBOSE)
      note_at(tree_loc(expr), "evaluate thunk for %s",
              kind == T_FCALL ? istr(tree_ident(expr)) : tree_kind_str(kind));

   vcode_select_unit(thunk);
   vcode_select_block(0);

   context_t *context = eval_new_context(NULL);
   if (context == NULL)
      return expr;

   eval_state_t state = {
      .context = context,
      .result  = -1,
      .fcall   = expr,
      .failed  = false,
      .flags   = flags,
   };

   eval_vcode(&state);

   vcode_unit_unref(thunk);
   thunk = NULL;

   if (state.failed) {
      eval_cleanup_state(&state);
      return expr;
   }

   assert(state.result != -1);
   value_t result = context->regs[state.result];

   if (flags & EVAL_VERBOSE) {
      const char *name = kind == T_FCALL
         ? istr(tree_ident(expr)) : tree_kind_str(kind);
      LOCAL_TEXT_BUF tb = tb_new();
      eval_dump(tb, &result, type);
      note_at(tree_loc(expr), "%s returned %s", name, tb_get(tb));
   }

   tree_t tree = eval_value_to_tree(&result, type, tree_loc(expr));
   eval_cleanup_state(&state);
   return tree;
}

int eval_errors(void)
{
   return errors;
}

void reset_eval_errors(void)
{
   errors = 0;
}
