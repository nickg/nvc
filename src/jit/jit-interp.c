//
//  Copyright (C) 2022  Nick Gasson
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
#include "jit/jit-priv.h"
#include "rt/mspace.h"
#include "tree.h"
#include "type.h"

#include <assert.h>
#include <ctype.h>
#include <inttypes.h>
#include <math.h>
#include <stdlib.h>
#include <string.h>

typedef struct _jit_interp {
   jit_scalar_t  *args;
   jit_scalar_t  *regs;
   unsigned       nargs;
   unsigned       pc;
   jit_func_t    *func;
   unsigned char *frame;
   unsigned       flags;
   bool           abort;
   mspace_t      *mspace;
   unsigned       backedge;
   jit_interp_t  *caller;
} jit_interp_t;

#ifdef DEBUG
#define JIT_ASSERT(expr) do {                                           \
      if (unlikely(!(expr))) {                                          \
         interp_dump(state);                                            \
         fatal_trace("assertion '" #expr "' failed");                   \
      }                                                                 \
   } while (0)
#else
#define JIT_ASSERT(expr)
#endif

#define FOR_EACH_SIZE(sz, macro) do {                   \
      assert((sz) != JIT_SZ_UNSPEC);                    \
      switch ((sz)) {                                   \
      case JIT_SZ_8: macro(int8_t); break;              \
      case JIT_SZ_16: macro(int16_t); break;            \
      case JIT_SZ_32: macro(int32_t); break;            \
      case JIT_SZ_64: macro(int64_t); break;            \
      default: break;                                   \
      }                                                 \
   } while (0)

#define BOUNDED_LIMIT 10000   // Max backedges in bounded mode

static void interp_dump_reg(jit_interp_t *state, int64_t ival)
{
   printf("%"PRIx64, ival);
   if ((ival >= -256 && ival < 0) || (ival >= 10 && ival < 256))
      printf(" (%"PRIi64")\n", ival);
   else {
      size_t size;
      void *base;
      if ((base = mspace_find(state->mspace, (void *)(intptr_t)ival, &size))) {
         if (size == 0)
            color_printf("$!red$ ==> bad mspace object$$\n");
         else {
            printf(" ==> %zu byte mspace object\n", size);
            jit_hexdump(base, size, 8, (void *)(intptr_t)ival, "\t\t\t");
         }
      }
      else
         printf("\n");
   }
}

static void interp_dump(jit_interp_t *state)
{
   jit_dump_with_mark(state->func, state->pc - 1);

   printf("Arguments:\n");
   for (int i = 0; i < state->nargs; i++) {
      printf("\tA%d\t", i);
      interp_dump_reg(state, state->args[i].integer);
   }

   printf("\nRegisters:\n");
   for (int i = 0; i < state->func->nregs; i++) {
      printf("\tR%d\t", i);
      interp_dump_reg(state, state->regs[i].integer);
   }

   printf("\nFlags:\t");
   if (state->flags & (1 << JIT_CC_EQ)) printf("EQ ");
   if (state->flags & (1 << JIT_CC_NE)) printf("NE ");
   if (state->flags & (1 << JIT_CC_LT)) printf("LT ");
   if (state->flags & (1 << JIT_CC_GE)) printf("GE ");
   if (state->flags & (1 << JIT_CC_GT)) printf("GT ");
   if (state->flags & (1 << JIT_CC_LE)) printf("LE ");
   if (state->flags & (1 << JIT_CC_OF)) printf("OF ");
   if (state->flags & (1 << JIT_CC_NO)) printf("NO ");
   printf("\n");

   if (state->func->framesz > 0) {
      printf("\nFrame:\n");
      jit_hexdump(state->frame, state->func->framesz, 16, NULL, "\t");
   }

   if (state->func->cpoolsz > 0) {
      printf("\nConstant pool:\n");
      jit_hexdump(state->func->cpool, state->func->cpoolsz, 16, NULL, "\t");
   }

   printf("\n");
   fflush(stdout);
}

static jit_scalar_t interp_get_value(jit_interp_t *state, jit_value_t value)
{
   switch (value.kind) {
   case JIT_VALUE_REG:
      JIT_ASSERT(value.reg < state->func->nregs);
      return state->regs[value.reg];
   case JIT_VALUE_INT64:
      return (jit_scalar_t){ .integer = value.int64 };
   case JIT_VALUE_DOUBLE:
      return (jit_scalar_t){ .real = value.dval };
   case JIT_ADDR_FRAME:
      JIT_ASSERT(value.int64 >= 0 && value.int64 < state->func->framesz);
      return (jit_scalar_t){ .pointer = state->frame + value.int64 };
   case JIT_ADDR_CPOOL:
      JIT_ASSERT(value.int64 >= 0 && value.int64 <= state->func->cpoolsz);
      return (jit_scalar_t){ .pointer = state->func->cpool + value.int64 };
   case JIT_ADDR_REG:
      JIT_ASSERT(value.reg < state->func->nregs);
      return (jit_scalar_t){
         .pointer = state->regs[value.reg].pointer + value.disp
      };
   case JIT_ADDR_ABS:
      return (jit_scalar_t){ .pointer = (void *)(intptr_t)value.int64 };
   case JIT_VALUE_LABEL:
      return (jit_scalar_t){ .integer = value.label };
   default:
      interp_dump(state);
      fatal_trace("cannot handle value kind %d", value.kind);
   }
}


static void interp_stack_trace(diag_t *d, jit_interp_t *frame)
{
   ident_t name = frame->func->name;
   ident_t lib_name = ident_walk_selected(&name);
   ident_t unit_name = ident_walk_selected(&name);

   if (unit_name == NULL)
      return;

   ident_t qual = ident_prefix(lib_name, unit_name, '.');

   const char *symbol = istr(frame->func->name);
   tree_t enclosing = find_enclosing_decl(qual, symbol);
   if (enclosing == NULL)
      return;

   // TODO: this is much less accurate than the DWARF info
   const loc_t *loc = tree_loc(enclosing);

   // TODO: this is copied from rt_emit_trace and should be merged
   switch (tree_kind(enclosing)) {
   case T_PROCESS:
      diag_trace(d, loc, "Process %s", istr(tree_ident(enclosing)));
      break;
   case T_FUNC_BODY:
   case T_FUNC_DECL:
      diag_trace(d, loc, "Function %s", type_pp(tree_type(enclosing)));
      break;
   case T_PROC_BODY:
   case T_PROC_DECL:
      diag_trace(d, loc, "Procedure %s", type_pp(tree_type(enclosing)));
      break;
   case T_TYPE_DECL:
      if (strstr(symbol, "$value"))
         diag_trace(d, loc, "Attribute %s'VALUE",
                    istr(tree_ident(enclosing)));
      else
         diag_trace(d, loc, "Type %s", istr(tree_ident(enclosing)));
      break;
   case T_BLOCK:
      diag_trace(d, loc, "Process (init)");
      break;
   case T_PACKAGE:
   case T_PACK_INST:
   case T_PACK_BODY:
      diag_trace(d, loc, "Package %s", istr(tree_ident(enclosing)));
      break;
   default:
      diag_trace(d, loc, "%s", istr(tree_ident(enclosing)));
      break;
   }
}

__attribute__((format(printf, 3, 4)))
static void interp_error(jit_interp_t *state, const loc_t *loc,
                         const char *fmt, ...)
{
   va_list ap;
   va_start(ap, fmt);

   if (jit_show_errors(state->func->jit)) {
      diag_t *d = diag_new(DIAG_ERROR, loc);
      diag_vprintf(d, fmt, ap);

      for (jit_interp_t *p = state; p != NULL; p = p->caller)
         interp_stack_trace(d, p);

      diag_emit(d);
   }

   va_end(ap);
   state->abort = true;
}

static void interp_recv(jit_interp_t *state, jit_ir_t *ir)
{
   JIT_ASSERT(ir->arg1.kind == JIT_VALUE_INT64);
   const int nth = ir->arg1.int64;

   JIT_ASSERT(nth < JIT_MAX_ARGS);
   state->regs[ir->result] = state->args[nth];
   state->nargs = MAX(state->nargs, nth + 1);
}

static void interp_send(jit_interp_t *state, jit_ir_t *ir)
{
   JIT_ASSERT(ir->arg1.kind == JIT_VALUE_INT64);
   const int nth = ir->arg1.int64;

   JIT_ASSERT(nth < JIT_MAX_ARGS);
   state->args[nth] = interp_get_value(state, ir->arg2);
   state->nargs = MAX(state->nargs, nth + 1);
}

static void interp_and(jit_interp_t *state, jit_ir_t *ir)
{
   jit_scalar_t arg1 = interp_get_value(state, ir->arg1);
   jit_scalar_t arg2 = interp_get_value(state, ir->arg2);

   state->regs[ir->result].integer = arg1.integer && arg2.integer;
}

static void interp_or(jit_interp_t *state, jit_ir_t *ir)
{
   jit_scalar_t arg1 = interp_get_value(state, ir->arg1);
   jit_scalar_t arg2 = interp_get_value(state, ir->arg2);

   state->regs[ir->result].integer = arg1.integer || arg2.integer;
}

static void interp_xor(jit_interp_t *state, jit_ir_t *ir)
{
   jit_scalar_t arg1 = interp_get_value(state, ir->arg1);
   jit_scalar_t arg2 = interp_get_value(state, ir->arg2);

   state->regs[ir->result].integer = arg1.integer ^ arg2.integer;
}


static void interp_mul(jit_interp_t *state, jit_ir_t *ir)
{
   jit_scalar_t arg1 = interp_get_value(state, ir->arg1);
   jit_scalar_t arg2 = interp_get_value(state, ir->arg2);

   if (ir->cc == JIT_CC_OF) {
      int overflow = 0;

#define MUL_OVERFLOW(type) do {                                 \
         type i1 = arg1.integer, i2 = arg2.integer, i0;         \
         overflow = __builtin_mul_overflow(i1, i2, &i0);        \
         state->regs[ir->result].integer = i0;                  \
      } while (0)

      FOR_EACH_SIZE(ir->size, MUL_OVERFLOW);

      state->flags = (overflow << JIT_CC_OF) | (!overflow << JIT_CC_NO);
   }
   else
      state->regs[ir->result].integer = arg1.integer * arg2.integer;
}

static void interp_umul(jit_interp_t *state, jit_ir_t *ir)
{
   jit_scalar_t arg1 = interp_get_value(state, ir->arg1);
   jit_scalar_t arg2 = interp_get_value(state, ir->arg2);

   if (ir->cc == JIT_CC_OF) {
      int overflow = 0;

#define UMUL_OVERFLOW(type) do {                                \
         u##type i1 = arg1.integer, i2 = arg2.integer, i0;      \
         overflow = __builtin_mul_overflow(i1, i2, &i0);        \
         state->regs[ir->result].integer = i0;                  \
      } while (0)

      FOR_EACH_SIZE(ir->size, UMUL_OVERFLOW);

      state->flags = (overflow << JIT_CC_OF) | (!overflow << JIT_CC_NO);
   }
   else
      state->regs[ir->result].integer = arg1.integer * arg2.integer;
}

static void interp_fmul(jit_interp_t *state, jit_ir_t *ir)
{
   jit_scalar_t arg1 = interp_get_value(state, ir->arg1);
   jit_scalar_t arg2 = interp_get_value(state, ir->arg2);

   state->regs[ir->result].real = arg1.real * arg2.real;
}

static void interp_div(jit_interp_t *state, jit_ir_t *ir)
{
   jit_scalar_t arg1 = interp_get_value(state, ir->arg1);
   jit_scalar_t arg2 = interp_get_value(state, ir->arg2);

   state->regs[ir->result].integer = arg1.integer / arg2.integer;
}

static void interp_fdiv(jit_interp_t *state, jit_ir_t *ir)
{
   jit_scalar_t arg1 = interp_get_value(state, ir->arg1);
   jit_scalar_t arg2 = interp_get_value(state, ir->arg2);

   state->regs[ir->result].real = arg1.real / arg2.real;
}

static void interp_sub(jit_interp_t *state, jit_ir_t *ir)
{
   jit_scalar_t arg1 = interp_get_value(state, ir->arg1);
   jit_scalar_t arg2 = interp_get_value(state, ir->arg2);

   if (ir->cc == JIT_CC_OF) {
      int overflow = 0;

#define SUB_OVERFLOW(type) do {                                 \
         type i1 = arg1.integer, i2 = arg2.integer, i0;         \
         overflow = __builtin_sub_overflow(i1, i2, &i0);        \
         state->regs[ir->result].integer = i0;                  \
      } while (0)

      FOR_EACH_SIZE(ir->size, SUB_OVERFLOW);

      state->flags = (overflow << JIT_CC_OF) | (!overflow << JIT_CC_NO);
   }
   else
      state->regs[ir->result].integer = arg1.integer - arg2.integer;
}

static void interp_usub(jit_interp_t *state, jit_ir_t *ir)
{
   jit_scalar_t arg1 = interp_get_value(state, ir->arg1);
   jit_scalar_t arg2 = interp_get_value(state, ir->arg2);

   if (ir->cc == JIT_CC_OF) {
      int overflow = 0;

#define USUB_OVERFLOW(type) do {                                \
         u##type i1 = arg1.integer, i2 = arg2.integer, i0;      \
         overflow = __builtin_sub_overflow(i1, i2, &i0);        \
         state->regs[ir->result].integer = i0;                  \
      } while (0)

      FOR_EACH_SIZE(ir->size, USUB_OVERFLOW);

      state->flags = (overflow << JIT_CC_OF) | (!overflow << JIT_CC_NO);
   }
   else
      state->regs[ir->result].integer = arg1.integer - arg2.integer;
}

static void interp_fsub(jit_interp_t *state, jit_ir_t *ir)
{
   jit_scalar_t arg1 = interp_get_value(state, ir->arg1);
   jit_scalar_t arg2 = interp_get_value(state, ir->arg2);

   state->regs[ir->result].real = arg1.real - arg2.real;
}

static void interp_add(jit_interp_t *state, jit_ir_t *ir)
{
   jit_scalar_t arg1 = interp_get_value(state, ir->arg1);
   jit_scalar_t arg2 = interp_get_value(state, ir->arg2);

   if (ir->cc == JIT_CC_OF) {
      int overflow = 0;

#define ADD_OVERFLOW(type) do {                                 \
         type i1 = arg1.integer, i2 = arg2.integer, i0;         \
         overflow = __builtin_add_overflow(i1, i2, &i0);        \
         state->regs[ir->result].integer = i0;                  \
      } while (0)

      FOR_EACH_SIZE(ir->size, ADD_OVERFLOW);

      state->flags = (overflow << JIT_CC_OF) | (!overflow << JIT_CC_NO);
   }
   else
      state->regs[ir->result].integer = arg1.integer + arg2.integer;
}

static void interp_uadd(jit_interp_t *state, jit_ir_t *ir)
{
   jit_scalar_t arg1 = interp_get_value(state, ir->arg1);
   jit_scalar_t arg2 = interp_get_value(state, ir->arg2);

   if (ir->cc == JIT_CC_OF) {
      int overflow = 0;

#define UADD_OVERFLOW(type) do {                                \
         u##type i1 = arg1.integer, i2 = arg2.integer, i0;      \
         overflow = __builtin_add_overflow(i1, i2, &i0);        \
         state->regs[ir->result].integer = i0;                  \
      } while (0)

      FOR_EACH_SIZE(ir->size, UADD_OVERFLOW);

      state->flags = (overflow << JIT_CC_OF) | (!overflow << JIT_CC_NO);
   }
   else
      state->regs[ir->result].integer = arg1.integer + arg2.integer;
}

static void interp_fadd(jit_interp_t *state, jit_ir_t *ir)
{
   jit_scalar_t arg1 = interp_get_value(state, ir->arg1);
   jit_scalar_t arg2 = interp_get_value(state, ir->arg2);

   state->regs[ir->result].real = arg1.real + arg2.real;
}

static void interp_store(jit_interp_t *state, jit_ir_t *ir)
{
   jit_scalar_t arg1 = interp_get_value(state, ir->arg1);
   jit_scalar_t arg2 = interp_get_value(state, ir->arg2);

   JIT_ASSERT(ir->size != JIT_SZ_UNSPEC);

   switch (ir->size) {
   case JIT_SZ_8:
      *(uint8_t *)arg2.pointer = (uint8_t)arg1.integer;
      break;
   case JIT_SZ_16:
      *(uint16_t *)arg2.pointer = (uint16_t)arg1.integer;
      break;
   case JIT_SZ_32:
      *(uint32_t *)arg2.pointer = (uint32_t)arg1.integer;
      break;
   case JIT_SZ_64:
      *(uint64_t *)arg2.pointer = arg1.integer;
      break;
   case JIT_SZ_UNSPEC:
      break;
   }
}

#ifdef DEBUG
static void interp_check_poison(jit_interp_t *state, jit_reg_t reg)
{
   bool all_ff = true;
   for (int i = 56; i >= 0; i -= 8) {
      const uint8_t byte = (state->regs[reg].integer >> i) & 0xff;
      if (all_ff && byte == 0xff && i > 0)
         continue;
      else if (byte != 0xde)
         return;
      all_ff = false;
   }

   interp_dump(state);
   fatal_trace("loaded poison value in R%d", reg);
}
#endif

static void interp_uload(jit_interp_t *state, jit_ir_t *ir)
{
   jit_scalar_t arg1 = interp_get_value(state, ir->arg1);

   JIT_ASSERT(ir->size != JIT_SZ_UNSPEC);
   JIT_ASSERT(arg1.pointer != NULL);

   switch (ir->size) {
   case JIT_SZ_8:
      state->regs[ir->result].integer = *(uint8_t *)arg1.pointer;
      break;
   case JIT_SZ_16:
      state->regs[ir->result].integer = *(uint16_t *)arg1.pointer;
      break;
   case JIT_SZ_32:
      state->regs[ir->result].integer = *(uint32_t *)arg1.pointer;
      break;
   case JIT_SZ_64:
      state->regs[ir->result].integer = *(uint64_t *)arg1.pointer;
      break;
   case JIT_SZ_UNSPEC:
      break;
   }

   DEBUG_ONLY(interp_check_poison(state, ir->result));
}

static void interp_load(jit_interp_t *state, jit_ir_t *ir)
{
   jit_scalar_t arg1 = interp_get_value(state, ir->arg1);

   JIT_ASSERT(ir->size != JIT_SZ_UNSPEC);
   JIT_ASSERT(arg1.pointer != NULL);

   switch (ir->size) {
   case JIT_SZ_8:
      state->regs[ir->result].integer = *(int8_t *)arg1.pointer;
      break;
   case JIT_SZ_16:
      state->regs[ir->result].integer = *(int16_t *)arg1.pointer;
      break;
   case JIT_SZ_32:
      state->regs[ir->result].integer = *(int32_t *)arg1.pointer;
      break;
   case JIT_SZ_64:
      state->regs[ir->result].integer = *(int64_t *)arg1.pointer;
      break;
   case JIT_SZ_UNSPEC:
      break;
   }

   DEBUG_ONLY(interp_check_poison(state, ir->result));
}

static void interp_cmp(jit_interp_t *state, jit_ir_t *ir)
{
   jit_scalar_t arg1 = interp_get_value(state, ir->arg1);
   jit_scalar_t arg2 = interp_get_value(state, ir->arg2);

   state->flags = 0;

   if (arg1.integer == arg2.integer)
      state->flags |= (1 << JIT_CC_EQ);
   else
      state->flags |= (1 << JIT_CC_NE);

   if (arg1.integer < arg2.integer)
      state->flags |= (1 << JIT_CC_LT);
   else
      state->flags |= (1 << JIT_CC_GE);

   if (arg1.integer > arg2.integer)
      state->flags |= (1 << JIT_CC_GT);
   else
      state->flags |= (1 << JIT_CC_LE);
}

static void interp_fcmp(jit_interp_t *state, jit_ir_t *ir)
{
   jit_scalar_t arg1 = interp_get_value(state, ir->arg1);
   jit_scalar_t arg2 = interp_get_value(state, ir->arg2);

   state->flags = 0;

   if (arg1.real == arg2.real)
      state->flags |= (1 << JIT_CC_EQ);
   else
      state->flags |= (1 << JIT_CC_NE);

   if (arg1.real < arg2.real)
      state->flags |= (1 << JIT_CC_LT);
   else
      state->flags |= (1 << JIT_CC_GE);

   if (arg1.real > arg2.real)
      state->flags |= (1 << JIT_CC_GT);
   else
      state->flags |= (1 << JIT_CC_LE);
}

static void interp_rem(jit_interp_t *state, jit_ir_t *ir)
{
   const int64_t x = interp_get_value(state, ir->arg1).integer;
   const int64_t y = interp_get_value(state, ir->arg2).integer;

   state->regs[ir->result].integer = x - (x / y) * y;
}

static void interp_cset(jit_interp_t *state, jit_ir_t *ir)
{
   state->regs[ir->result].integer = !!(state->flags & (1 << ir->cc));
}

static void interp_jump(jit_interp_t *state, jit_ir_t *ir)
{
   bool taken = false;
   if (ir->cc == JIT_CC_NONE)
      taken = true;
   else
      taken = !!(state->flags & (1 << ir->cc));

   if (taken) {
      const int target = interp_get_value(state, ir->arg1).integer;
      if (state->backedge > 0 && target < state->pc) {
         // Limit the number of loop iterations in bounded mode
         if (--(state->backedge) == 0)
            interp_error(state, NULL, "maximum iteration limit reached");
      }

      state->pc = target;
      JIT_ASSERT(state->pc < state->func->nirs);
   }
}

static void interp_trap(jit_interp_t *state, jit_ir_t *ir)
{
   interp_dump(state);
   fatal_trace("executed trap opcode");
}

static void interp_call(jit_interp_t *state, jit_ir_t *ir)
{
   JIT_ASSERT(ir->arg1.kind == JIT_VALUE_HANDLE);

   if (ir->arg1.handle == JIT_HANDLE_INVALID)
      state->abort = true;
   else {
      jit_func_t *f = jit_get_func(state->func->jit, ir->arg1.handle);
      if (!jit_interp(f, state->args, state->nargs, state->backedge, state))
         state->abort = true;
   }
}

static void interp_mov(jit_interp_t *state, jit_ir_t *ir)
{
   state->regs[ir->result] = interp_get_value(state, ir->arg1);
}

static void interp_csel(jit_interp_t *state, jit_ir_t *ir)
{
   if (state->flags & (1 << ir->cc))
      state->regs[ir->result] = interp_get_value(state, ir->arg1);
   else
      state->regs[ir->result] = interp_get_value(state, ir->arg2);
}

static void interp_neg(jit_interp_t *state, jit_ir_t *ir)
{
   state->regs[ir->result].integer = -interp_get_value(state, ir->arg1).integer;
}

static void interp_fneg(jit_interp_t *state, jit_ir_t *ir)
{
   state->regs[ir->result].real = -interp_get_value(state, ir->arg1).real;
}

static void interp_not(jit_interp_t *state, jit_ir_t *ir)
{
   state->regs[ir->result].integer = !interp_get_value(state, ir->arg1).integer;
}

static void interp_scvtf(jit_interp_t *state, jit_ir_t *ir)
{
   state->regs[ir->result].real = interp_get_value(state, ir->arg1).integer;
}

static void interp_fcvtns(jit_interp_t *state, jit_ir_t *ir)
{
   const double f = interp_get_value(state, ir->arg1).real;
   state->regs[ir->result].integer = round(f);
}

static void interp_lea(jit_interp_t *state, jit_ir_t *ir)
{
   state->regs[ir->result].pointer = interp_get_value(state, ir->arg1).pointer;
}

static void interp_fexp(jit_interp_t *state, jit_ir_t *ir)
{
   const double x = interp_get_value(state, ir->arg1).real;
   const double y = interp_get_value(state, ir->arg2).real;

   state->regs[ir->result].real = pow(x, y);
}

static void interp_exp(jit_interp_t *state, jit_ir_t *ir)
{
   const int64_t x = interp_get_value(state, ir->arg1).integer;
   const int64_t y = interp_get_value(state, ir->arg2).integer;

   state->regs[ir->result].integer = ipow(x, y);
}

static void interp_copy(jit_interp_t *state, jit_ir_t *ir)
{
   const size_t count = state->regs[ir->result].integer;
   void *dest = interp_get_value(state, ir->arg1).pointer;
   const void *src = interp_get_value(state, ir->arg2).pointer;

   memmove(dest, src, count);
}

static void interp_bzero(jit_interp_t *state, jit_ir_t *ir)
{
   const size_t count = state->regs[ir->result].integer;
   void *dest = interp_get_value(state, ir->arg1).pointer;

   memset(dest, '\0', count);
}

static void interp_galloc(jit_interp_t *state, jit_ir_t *ir)
{
   const size_t bytes = interp_get_value(state, ir->arg1).integer;
   state->regs[ir->result].pointer = mspace_alloc(state->mspace, bytes);

   if (state->regs[ir->result].pointer == NULL && bytes > 0)
      state->abort = true;   // Out of memory
}

static void interp_range_fail(jit_interp_t *state)
{
   int64_t      value = state->args[0].integer;
   int64_t      left  = state->args[1].integer;
   int64_t      right = state->args[2].integer;
   range_kind_t dir   = state->args[3].integer;
   tree_t       where = state->args[4].pointer;
   tree_t       hint  = state->args[5].pointer;

   type_t type = tree_type(hint);

   LOCAL_TEXT_BUF tb = tb_new();
   tb_cat(tb, "value ");
   to_string(tb, type, value);
   tb_printf(tb, " outside of %s range ", type_pp(type));
   to_string(tb, type, left);
   tb_cat(tb, dir == RANGE_TO ? " to " : " downto ");
   to_string(tb, type, right);

   switch (tree_kind(hint)) {
   case T_SIGNAL_DECL:
   case T_CONST_DECL:
   case T_VAR_DECL:
   case T_REF:
      tb_printf(tb, " for %s %s", class_str(class_of(hint)),
                istr(tree_ident(hint)));
      break;
   case T_PORT_DECL:
      tb_printf(tb, " for port %s", istr(tree_ident(hint)));
      break;
   case T_PARAM_DECL:
      tb_printf(tb, " for parameter %s", istr(tree_ident(hint)));
      break;
   case T_GENERIC_DECL:
      tb_printf(tb, " for generic %s", istr(tree_ident(hint)));
      break;
   case T_ATTR_REF:
      tb_printf(tb, " for attribute '%s", istr(tree_ident(hint)));
      break;
   default:
      break;
   }

   interp_error(state, tree_loc(where), "%s", tb_get(tb));
}

static void interp_index_fail(jit_interp_t *state)
{
   int32_t      value = state->args[0].integer;
   int32_t      left  = state->args[1].integer;
   int32_t      right = state->args[2].integer;
   range_kind_t dir   = state->args[3].integer;
   tree_t       where = state->args[4].pointer;
   tree_t       hint  = state->args[5].pointer;

   type_t type = tree_type(hint);

   LOCAL_TEXT_BUF tb = tb_new();
   tb_cat(tb, "index ");
   to_string(tb, type, value);
   tb_printf(tb, " outside of %s range ", type_pp(type));
   to_string(tb, type, left);
   tb_cat(tb, dir == RANGE_TO ? " to " : " downto ");
   to_string(tb, type, right);

   interp_error(state, tree_loc(where), "%s", tb_get(tb));
}

static void interp_overflow(jit_interp_t *state)
{
   int32_t lhs   = state->args[0].integer;
   int32_t rhs   = state->args[1].integer;
   tree_t  where = state->args[2].pointer;

   const char *op = "??";
   if (tree_kind(where) == T_FCALL) {
      switch (tree_subkind(tree_ref(where))) {
      case S_ADD: op = "+"; break;
      case S_MUL: op = "*"; break;
      case S_SUB: op = "-"; break;
      }
   }

   interp_error(state, tree_loc(where), "result of %d %s %d cannot be "
                "represented as %s", lhs, op, rhs, type_pp(tree_type(where)));
}

static void interp_null_deref(jit_interp_t *state)
{
   tree_t where = state->args[0].pointer;

   interp_error(state, tree_loc(where), "null access dereference");
}

static void interp_length_fail(jit_interp_t *state)
{
   int32_t left  = state->args[0].integer;
   int32_t right = state->args[1].integer;
   int32_t dim   = state->args[2].integer;
   tree_t  where = state->args[3].pointer;

   const tree_kind_t kind = tree_kind(where);

   LOCAL_TEXT_BUF tb = tb_new();
   if (kind == T_PORT_DECL || kind == T_GENERIC_DECL || kind == T_PARAM_DECL)
      tb_cat(tb, "actual");
   else if (kind == T_CASE || kind == T_MATCH_CASE)
      tb_cat(tb, "expression");
   else
      tb_cat(tb, "value");
   tb_printf(tb, " length %d", right);
   if (dim > 0)
      tb_printf(tb, " for dimension %d", dim);
   tb_cat(tb, " does not match ");

   switch (kind) {
   case T_PORT_DECL:
      tb_printf(tb, "port %s", istr(tree_ident(where)));
      break;
   case T_PARAM_DECL:
      tb_printf(tb, "parameter %s", istr(tree_ident(where)));
      break;
   case T_GENERIC_DECL:
      tb_printf(tb, "generic %s", istr(tree_ident(where)));
      break;
   case T_VAR_DECL:
      tb_printf(tb, "variable %s", istr(tree_ident(where)));
      break;
   case T_SIGNAL_DECL:
      tb_printf(tb, "signal %s", istr(tree_ident(where)));
      break;
   case T_REF:
      tb_printf(tb, "%s %s", class_str(class_of(where)),
                istr(tree_ident(where)));
      break;
   case T_FIELD_DECL:
      tb_printf(tb, "field %s", istr(tree_ident(where)));
      break;
   case T_ALIAS:
      tb_printf(tb, "alias %s", istr(tree_ident(where)));
      break;
   case T_CASE:
   case T_MATCH_CASE:
      tb_cat(tb, "case choice");
      break;
   default:
      tb_cat(tb, "target");
      break;
   }

   tb_printf(tb, " length %d", left);

   interp_error(state, tree_loc(where), "%s", tb_get(tb));
}

static void interp_div_zero(jit_interp_t *state)
{
   tree_t where = state->args[0].pointer;

   interp_error(state, tree_loc(where), "division by zero");
}

static void interp_exponent_fail(jit_interp_t *state)
{
   int32_t value = state->args[0].integer;
   tree_t  where = state->args[1].pointer;

   interp_error(state, tree_loc(where), "negative exponent %d only "
                "allowed for floating-point types", value);
}

static void interp_wait(jit_interp_t *state)
{
   // Silently abort for evaluation
   state->abort = true;
}

static void interp_unreachable(jit_interp_t *state)
{
   tree_t where = state->args[0].pointer;

   if (where != NULL && tree_kind(where) == T_FUNC_BODY)
      interp_error(state, tree_loc(where), "function %s did not return a value",
                   istr(tree_ident(where)));
   else
      fatal_trace("executed unreachable instruction");
}

static void interp_report(jit_interp_t *state)
{
   char    *msg      = state->args[0].pointer;
   int32_t  len      = state->args[1].integer;
   int32_t  severity = state->args[2].integer;
   tree_t   where    = state->args[3].pointer;

   static const char *levels[] = {
      "Note", "Warning", "Error", "Failure"
   };

   const diag_level_t level = diag_severity(severity);

   if (jit_show_errors(state->func->jit)) {
      diag_t *d = diag_new(level, tree_loc(where));
      diag_printf(d, "Report %s: %.*s", levels[severity], len, msg);
      diag_show_source(d, false);
      diag_emit(d);
   }

   if (level == DIAG_FATAL)
      state->abort = true;
}

static void interp_assert_fail(jit_interp_t *state)
{
   char    *msg      = state->args[0].pointer;
   int32_t  len      = state->args[1].integer;
   int32_t  severity = state->args[2].integer;
   tree_t   where    = state->args[3].pointer;

   static const char *levels[] = {
      "Note", "Warning", "Error", "Failure"
   };

   const diag_level_t level = diag_severity(severity);

   if (jit_show_errors(state->func->jit)) {
      diag_t *d = diag_new(level, tree_loc(where));
      if (msg == NULL)
         diag_printf(d, "Assertion %s: Assertion violation.", levels[severity]);
      else {
         diag_printf(d, "Assertion %s: %.*s", levels[severity], len, msg);

         // Assume we don't want to dump the source code if the user
         // provided their own message
         diag_show_source(d, false);
      }

      diag_emit(d);
   }

   if (level == DIAG_FATAL)
      state->abort = true;
}

static void interp_int_to_string(jit_interp_t *state)
{
   int64_t value = state->args[0].integer;

   char *buf = mspace_alloc(state->mspace, 20);
   size_t len = checked_sprintf(buf, 20, "%"PRIi64, value);

   state->args[0].pointer = buf;
   state->args[1].integer = 1;
   state->args[2].integer = len;

   state->nargs = MAX(state->nargs, 3);
}

static void interp_real_to_string(jit_interp_t *state)
{
   double value = state->args[0].real;

   char *buf = mspace_alloc(state->mspace, 32);
   size_t len = checked_sprintf(buf, 32, "%.*g", 17, value);

   state->args[0].pointer = buf;
   state->args[1].integer = 1;
   state->args[2].integer = len;

   state->nargs = MAX(state->nargs, 3);
}

static void interp_exit(jit_interp_t *state, jit_ir_t *ir)
{
   switch (ir->arg1.exit) {
   case JIT_EXIT_INDEX_FAIL:
      interp_index_fail(state);
      break;

   case JIT_EXIT_OVERFLOW:
      interp_overflow(state);
      break;

   case JIT_EXIT_NULL_DEREF:
      interp_null_deref(state);
      break;

   case JIT_EXIT_LENGTH_FAIL:
      interp_length_fail(state);
      break;

   case JIT_EXIT_WAIT:
      interp_wait(state);
      break;

   case JIT_EXIT_UNREACHABLE:
      interp_unreachable(state);
      break;

   case JIT_EXIT_DIV_ZERO:
      interp_div_zero(state);
      break;

   case JIT_EXIT_EXPONENT_FAIL:
      interp_exponent_fail(state);
      break;

   case JIT_EXIT_REPORT:
      interp_report(state);
      break;

   case JIT_EXIT_ASSERT_FAIL:
      interp_assert_fail(state);
      break;

   case JIT_EXIT_INT_TO_STRING:
      interp_int_to_string(state);
      break;

   case JIT_EXIT_REAL_TO_STRING:
      interp_real_to_string(state);
      break;

   case JIT_EXIT_RANGE_FAIL:
      interp_range_fail(state);
      break;

   default:
      fatal_trace("cannot interpret exit %s", jit_exit_name(ir->arg1.exit));
   }
}

static void interp_fficall(jit_interp_t *state, jit_ir_t *ir)
{
   // Not currently implemented
   state->abort = true;
}

static void interp_loop(jit_interp_t *state)
{
   do {
      JIT_ASSERT(state->pc < state->func->nirs);
      jit_ir_t *ir = &(state->func->irbuf[state->pc++]);
      switch (ir->op) {
      case J_RECV:
         interp_recv(state, ir);
         break;
      case J_SEND:
         interp_send(state, ir);
         break;
      case J_AND:
         interp_and(state, ir);
         break;
      case J_OR:
         interp_or(state, ir);
         break;
      case J_XOR:
         interp_xor(state, ir);
         break;
      case J_SUB:
         interp_sub(state, ir);
         break;
      case J_USUB:
         interp_usub(state, ir);
         break;
      case J_FSUB:
         interp_fsub(state, ir);
         break;
      case J_ADD:
         interp_add(state, ir);
         break;
      case J_UADD:
         interp_uadd(state, ir);
         break;
      case J_FADD:
         interp_fadd(state, ir);
         break;
      case J_MUL:
         interp_mul(state, ir);
         break;
      case J_UMUL:
         interp_umul(state, ir);
         break;
      case J_FMUL:
         interp_fmul(state, ir);
         break;
      case J_DIV:
         interp_div(state, ir);
         break;
      case J_FDIV:
         interp_fdiv(state, ir);
         break;
      case J_RET:
         return;
      case J_STORE:
         interp_store(state, ir);
         break;
      case J_ULOAD:
         interp_uload(state, ir);
         break;
      case J_LOAD:
         interp_load(state, ir);
         break;
      case J_CMP:
         interp_cmp(state, ir);
         break;
      case J_FCMP:
         interp_fcmp(state, ir);
         break;
      case J_CSET:
         interp_cset(state, ir);
         break;
      case J_JUMP:
         interp_jump(state, ir);
         break;
      case J_TRAP:
         interp_trap(state, ir);
         break;
      case J_CALL:
         interp_call(state, ir);
         break;
      case J_MOV:
         interp_mov(state, ir);
         break;
      case J_CSEL:
         interp_csel(state, ir);
         break;
      case J_NEG:
         interp_neg(state, ir);
         break;
      case J_FNEG:
         interp_fneg(state, ir);
         break;
      case J_NOT:
         interp_not(state, ir);
         break;
      case J_SCVTF:
         interp_scvtf(state, ir);
         break;
      case J_FCVTNS:
         interp_fcvtns(state, ir);
         break;
      case J_LEA:
         interp_lea(state, ir);
         break;
      case J_REM:
         interp_rem(state, ir);
         break;
      case MACRO_COPY:
         interp_copy(state, ir);
         break;
      case MACRO_BZERO:
         interp_bzero(state, ir);
         break;
      case MACRO_GALLOC:
         interp_galloc(state, ir);
         break;
      case MACRO_EXIT:
         interp_exit(state, ir);
         break;
      case MACRO_FEXP:
         interp_fexp(state, ir);
         break;
      case MACRO_EXP:
         interp_exp(state, ir);
         break;
      case MACRO_FFICALL:
         interp_fficall(state, ir);
         break;
      default:
         interp_dump(state);
         fatal_trace("cannot interpret opcode %s", jit_op_name(ir->op));
      }
   } while (!state->abort);
}

bool jit_interp(jit_func_t *f, jit_scalar_t *args, int nargs, int backedge,
                jit_interp_t *caller)
{
   if (f->irbuf == NULL)
      jit_irgen(f);

   // Using VLAs here as we need these allocated on the stack so the
   // mspace GC can scan them
   jit_scalar_t regs[f->nregs];
   unsigned char frame[f->framesz];

#ifdef DEBUG
   memset(regs, 0xde, sizeof(jit_scalar_t) * f->nregs);
   memset(frame, 0xde, f->framesz);
#endif

   jit_interp_t state = {
      .args     = args,
      .regs     = regs,
      .nargs    = nargs,
      .pc       = 0,
      .func     = f,
      .frame    = frame,
      .mspace   = jit_get_mspace(f->jit),
      .backedge = backedge,
      .caller   = caller,
   };

   interp_loop(&state);

   return !state.abort;
}
