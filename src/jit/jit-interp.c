//
//  Copyright (C) 2022-2024  Nick Gasson
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
#include "jit/jit-exits.h"
#include "jit/jit-priv.h"
#include "jit/jit-ffi.h"
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
   mspace_t      *mspace;
   jit_anchor_t  *anchor;
   tlab_t        *tlab;
} jit_interp_t;

#ifdef DEBUG
#define JIT_ASSERT(expr) do {                                      \
      if (unlikely(!(expr))) {                                     \
         interp_dump(state);                                       \
         fatal_trace("assertion '%s' failed", #expr);              \
      }                                                            \
   } while (0)
#define CANNOT_HANDLE(value) do {                                  \
      interp_dump(state);                                          \
      fatal_trace("cannot handle value kind %d", value.kind);      \
   } while (0)
#else
#define JIT_ASSERT(expr)
#define CANNOT_HANDLE(value) __builtin_unreachable()
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

static void interp_dump_reg(jit_interp_t *state, int64_t ival)
{
   printf("%"PRIx64, ival);
   if ((ival >= -256 && ival < 0) || (ival >= 10 && ival < 256))
      printf(" (%"PRIi64")\n", ival);
   else if (ival >= (intptr_t)state->func->cpool
            && ival < (intptr_t)state->func->cpool + state->func->cpoolsz) {
      printf(" ==> cpool pointer\n");
      jit_hexdump((void *)(intptr_t)ival, 8, 8, NULL, "\t\t\t");
   }
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

   printf("\nFlags: %c\n", state->flags ? 'T' : 'F');

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

__attribute__((always_inline))
static inline int64_t interp_get_int(jit_interp_t *state, jit_value_t value)
{
   switch (value.kind) {
   case JIT_VALUE_REG:
      JIT_ASSERT(value.reg < state->func->nregs);
      return state->regs[value.reg].integer;
   case JIT_VALUE_INT64:
   case JIT_VALUE_DOUBLE:
   case JIT_ADDR_ABS:
      return value.int64;
   default:
      CANNOT_HANDLE(value);
   }
}

__attribute__((always_inline))
static inline double interp_get_real(jit_interp_t *state, jit_value_t value)
{
   switch (value.kind) {
   case JIT_VALUE_REG:
      JIT_ASSERT(value.reg < state->func->nregs);
      return state->regs[value.reg].real;
   case JIT_VALUE_INT64:
   case JIT_VALUE_DOUBLE:
      return value.dval;
   default:
      CANNOT_HANDLE(value);
   }
}

__attribute__((always_inline))
static inline void *interp_get_pointer(jit_interp_t *state, jit_value_t value)
{
   switch (value.kind) {
   case JIT_VALUE_REG:
      JIT_ASSERT(value.reg < state->func->nregs);
      return state->regs[value.reg].pointer;
   case JIT_ADDR_CPOOL:
      JIT_ASSERT(value.int64 >= 0 && value.int64 <= state->func->cpoolsz);
      return state->func->cpool + value.int64;
   case JIT_ADDR_REG:
      JIT_ASSERT(value.reg < state->func->nregs);
      return state->regs[value.reg].pointer + value.disp;
   case JIT_ADDR_ABS:
      return (void *)(intptr_t)value.int64;
   case JIT_ADDR_COVER:
      return jit_get_cover_ptr(state->func->jit, value);
   default:
      CANNOT_HANDLE(value);
   }
}

__attribute__((always_inline))
static inline jit_scalar_t interp_get_scalar(jit_interp_t *state,
                                             jit_value_t value)
{
   switch (value.kind) {
   case JIT_VALUE_REG:
      JIT_ASSERT(value.reg < state->func->nregs);
      return state->regs[value.reg];
   case JIT_VALUE_INT64:
      return (jit_scalar_t){ .integer = value.int64 };
   case JIT_VALUE_DOUBLE:
      return (jit_scalar_t){ .real = value.dval };
   case JIT_VALUE_LABEL:
      return (jit_scalar_t){ .integer = value.label };
   case JIT_VALUE_HANDLE:
      return (jit_scalar_t){ .integer = value.handle };
   case JIT_VALUE_LOCUS:
      return (jit_scalar_t){ .pointer = value.locus };
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
   case JIT_ADDR_COVER:
      return (jit_scalar_t){
         .pointer = jit_get_cover_ptr(state->func->jit, value)
      };
   default:
      CANNOT_HANDLE(value);
   }
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
   state->args[nth] = interp_get_scalar(state, ir->arg2);
   state->nargs = MAX(state->nargs, nth + 1);
}

static void interp_and(jit_interp_t *state, jit_ir_t *ir)
{
   const int64_t arg1 = interp_get_int(state, ir->arg1);
   const int64_t arg2 = interp_get_int(state, ir->arg2);

   state->regs[ir->result].integer = arg1 & arg2;
}

static void interp_or(jit_interp_t *state, jit_ir_t *ir)
{
   const int64_t arg1 = interp_get_int(state, ir->arg1);
   const int64_t arg2 = interp_get_int(state, ir->arg2);

   state->regs[ir->result].integer = arg1 | arg2;
}

static void interp_xor(jit_interp_t *state, jit_ir_t *ir)
{
   const int64_t arg1 = interp_get_int(state, ir->arg1);
   const int64_t arg2 = interp_get_int(state, ir->arg2);

   state->regs[ir->result].integer = arg1 ^ arg2;
}

static void interp_mul(jit_interp_t *state, jit_ir_t *ir)
{
   const int64_t arg1 = interp_get_int(state, ir->arg1);
   const int64_t arg2 = interp_get_int(state, ir->arg2);

   if (ir->cc == JIT_CC_O) {
      int overflow = 0;

#define MUL_OVERFLOW(type) do {                                 \
         type i1 = arg1, i2 = arg2, i0;                         \
         overflow = __builtin_mul_overflow(i1, i2, &i0);        \
         state->regs[ir->result].integer = i0;                  \
      } while (0)

      FOR_EACH_SIZE(ir->size, MUL_OVERFLOW);

      state->flags = overflow;
   }
   else if (ir->cc == JIT_CC_C) {
      int overflow = 0;

#define UMUL_OVERFLOW(type) do {                                \
         u##type i1 = arg1, i2 = arg2, i0;                      \
         overflow = __builtin_mul_overflow(i1, i2, &i0);        \
         state->regs[ir->result].integer = i0;                  \
      } while (0)

      FOR_EACH_SIZE(ir->size, UMUL_OVERFLOW);

      state->flags = overflow;
   }
   else
      state->regs[ir->result].integer = arg1 * arg2;
}

static void interp_fmul(jit_interp_t *state, jit_ir_t *ir)
{
   const double arg1 = interp_get_real(state, ir->arg1);
   const double arg2 = interp_get_real(state, ir->arg2);

   state->regs[ir->result].real = arg1 * arg2;
}

static void interp_div(jit_interp_t *state, jit_ir_t *ir)
{
   const int64_t arg1 = interp_get_int(state, ir->arg1);
   const int64_t arg2 = interp_get_int(state, ir->arg2);

   state->regs[ir->result].integer = arg1 / arg2;
}

static void interp_fdiv(jit_interp_t *state, jit_ir_t *ir)
{
   const double arg1 = interp_get_real(state, ir->arg1);
   const double arg2 = interp_get_real(state, ir->arg2);

   state->regs[ir->result].real = arg1 / arg2;
}

static void interp_sub(jit_interp_t *state, jit_ir_t *ir)
{
   const int64_t arg1 = interp_get_int(state, ir->arg1);
   const int64_t arg2 = interp_get_int(state, ir->arg2);

   if (ir->cc == JIT_CC_O) {
      int overflow = 0;

#define SUB_OVERFLOW(type) do {                                 \
         type i1 = arg1, i2 = arg2, i0;                         \
         overflow = __builtin_sub_overflow(i1, i2, &i0);        \
         state->regs[ir->result].integer = i0;                  \
      } while (0)

      FOR_EACH_SIZE(ir->size, SUB_OVERFLOW);

      state->flags = overflow;
   }
   else if (ir->cc == JIT_CC_C) {
      int overflow = 0;

#define USUB_OVERFLOW(type) do {                                \
         u##type i1 = arg1, i2 = arg2, i0;                      \
         overflow = __builtin_sub_overflow(i1, i2, &i0);        \
         state->regs[ir->result].integer = i0;                  \
      } while (0)

      FOR_EACH_SIZE(ir->size, USUB_OVERFLOW);

      state->flags = overflow;
   }
   else
      state->regs[ir->result].integer = arg1 - arg2;
}

static void interp_fsub(jit_interp_t *state, jit_ir_t *ir)
{
   const double arg1 = interp_get_real(state, ir->arg1);
   const double arg2 = interp_get_real(state, ir->arg2);

   state->regs[ir->result].real = arg1 - arg2;
}

static void interp_add(jit_interp_t *state, jit_ir_t *ir)
{
   const int64_t arg1 = interp_get_int(state, ir->arg1);
   const int64_t arg2 = interp_get_int(state, ir->arg2);

   if (ir->cc == JIT_CC_O) {
      int overflow = 0;

#define ADD_OVERFLOW(type) do {                                 \
         type i1 = arg1, i2 = arg2, i0;                         \
         overflow = __builtin_add_overflow(i1, i2, &i0);        \
         state->regs[ir->result].integer = i0;                  \
      } while (0)

      FOR_EACH_SIZE(ir->size, ADD_OVERFLOW);

      state->flags = overflow;
   }
   else if (ir->cc == JIT_CC_C) {
      int overflow = 0;

#define UADD_OVERFLOW(type) do {                                \
         u##type i1 = arg1, i2 = arg2, i0;                      \
         overflow = __builtin_add_overflow(i1, i2, &i0);        \
         state->regs[ir->result].integer = i0;                  \
      } while (0)

      FOR_EACH_SIZE(ir->size, UADD_OVERFLOW);

      state->flags = overflow;
   }
   else
      state->regs[ir->result].integer = arg1 + arg2;
}

static void interp_fadd(jit_interp_t *state, jit_ir_t *ir)
{
   const double arg1 = interp_get_real(state, ir->arg1);
   const double arg2 = interp_get_real(state, ir->arg2);

   state->regs[ir->result].real = arg1 + arg2;
}

static void interp_shl(jit_interp_t *state, jit_ir_t *ir)
{
   const uint64_t arg1 = interp_get_int(state, ir->arg1);
   const uint64_t arg2 = interp_get_int(state, ir->arg2);

   state->regs[ir->result].integer = arg1 << arg2;
}

static void interp_shr(jit_interp_t *state, jit_ir_t *ir)
{
   const uint64_t arg1 = interp_get_int(state, ir->arg1);
   const uint64_t arg2 = interp_get_int(state, ir->arg2);

   state->regs[ir->result].integer = arg1 >> arg2;
}

static void interp_asr(jit_interp_t *state, jit_ir_t *ir)
{
   const int64_t arg1 = interp_get_int(state, ir->arg1);
   const int64_t arg2 = interp_get_int(state, ir->arg2);

   state->regs[ir->result].integer = arg1 >> arg2;
}

static void interp_store(jit_interp_t *state, jit_ir_t *ir)
{
   jit_scalar_t arg1 = interp_get_scalar(state, ir->arg1);
   void *arg2 = interp_get_pointer(state, ir->arg2);

   JIT_ASSERT(ir->size != JIT_SZ_UNSPEC);
   JIT_ASSERT(arg2 != NULL);
   JIT_ASSERT((intptr_t)arg2 >= 4096);

   switch (ir->size) {
   case JIT_SZ_8:
      *(uint8_t *)arg2 = (uint8_t)arg1.integer;
      break;
   case JIT_SZ_16:
      *(uint16_t *)arg2 = (uint16_t)arg1.integer;
      break;
   case JIT_SZ_32:
      *(uint32_t *)arg2 = (uint32_t)arg1.integer;
      break;
   case JIT_SZ_64:
      *(uint64_t *)arg2 = arg1.integer;
      break;
   case JIT_SZ_UNSPEC:
      break;
   }
}

static void interp_uload(jit_interp_t *state, jit_ir_t *ir)
{
   void *arg1 = interp_get_pointer(state, ir->arg1);

   JIT_ASSERT(ir->size != JIT_SZ_UNSPEC);
   JIT_ASSERT(arg1 != NULL);
   JIT_ASSERT((intptr_t)arg1 >= 4096);

   switch (ir->size) {
   case JIT_SZ_8:
      state->regs[ir->result].integer = *(uint8_t *)arg1;
      break;
   case JIT_SZ_16:
      state->regs[ir->result].integer = *(uint16_t *)arg1;
      break;
   case JIT_SZ_32:
      state->regs[ir->result].integer = *(uint32_t *)arg1;
      break;
   case JIT_SZ_64:
      state->regs[ir->result].integer = *(uint64_t *)arg1;
      break;
   case JIT_SZ_UNSPEC:
      break;
   }
}

static void interp_load(jit_interp_t *state, jit_ir_t *ir)
{
   void *arg1 = interp_get_pointer(state, ir->arg1);

   JIT_ASSERT(ir->size != JIT_SZ_UNSPEC);
   JIT_ASSERT(arg1 != NULL);
   JIT_ASSERT((intptr_t)arg1 >= 4096);

   switch (ir->size) {
   case JIT_SZ_8:
      state->regs[ir->result].integer = *(int8_t *)arg1;
      break;
   case JIT_SZ_16:
      state->regs[ir->result].integer = *(int16_t *)arg1;
      break;
   case JIT_SZ_32:
      state->regs[ir->result].integer = *(int32_t *)arg1;
      break;
   case JIT_SZ_64:
      state->regs[ir->result].integer = *(int64_t *)arg1;
      break;
   case JIT_SZ_UNSPEC:
      break;
   }
}

static void interp_cmp(jit_interp_t *state, jit_ir_t *ir)
{
   const int64_t arg1 = interp_get_int(state, ir->arg1);
   const int64_t arg2 = interp_get_int(state, ir->arg2);

   switch (ir->cc) {
   case JIT_CC_EQ: state->flags = (arg1 == arg2); break;
   case JIT_CC_NE: state->flags = (arg1 != arg2); break;
   case JIT_CC_LT: state->flags = (arg1 < arg2); break;
   case JIT_CC_GT: state->flags = (arg1 > arg2); break;
   case JIT_CC_LE: state->flags = (arg1 <= arg2); break;
   case JIT_CC_GE: state->flags = (arg1 >= arg2); break;
   default: state->flags = 0; break;
   }
}

static void interp_ccmp(jit_interp_t *state, jit_ir_t *ir)
{
   const int64_t arg1 = interp_get_int(state, ir->arg1);
   const int64_t arg2 = interp_get_int(state, ir->arg2);

   switch (ir->cc) {
   case JIT_CC_EQ: state->flags &= (arg1 == arg2); break;
   case JIT_CC_NE: state->flags &= (arg1 != arg2); break;
   case JIT_CC_LT: state->flags &= (arg1 < arg2); break;
   case JIT_CC_GT: state->flags &= (arg1 > arg2); break;
   case JIT_CC_LE: state->flags &= (arg1 <= arg2); break;
   case JIT_CC_GE: state->flags &= (arg1 >= arg2); break;
   default: state->flags = 0; break;
   }
}

static void interp_fcmp(jit_interp_t *state, jit_ir_t *ir)
{
   const double arg1 = interp_get_real(state, ir->arg1);
   const double arg2 = interp_get_real(state, ir->arg2);

   switch (ir->cc) {
   case JIT_CC_EQ: state->flags = (arg1 == arg2); break;
   case JIT_CC_NE: state->flags = (arg1 != arg2); break;
   case JIT_CC_LT: state->flags = (arg1 < arg2); break;
   case JIT_CC_GT: state->flags = (arg1 > arg2); break;
   case JIT_CC_LE: state->flags = (arg1 <= arg2); break;
   case JIT_CC_GE: state->flags = (arg1 >= arg2); break;
   default: state->flags = 0; break;
   }
}

static void interp_fccmp(jit_interp_t *state, jit_ir_t *ir)
{
   const double arg1 = interp_get_real(state, ir->arg1);
   const double arg2 = interp_get_real(state, ir->arg2);

   switch (ir->cc) {
   case JIT_CC_EQ: state->flags &= (arg1 == arg2); break;
   case JIT_CC_NE: state->flags &= (arg1 != arg2); break;
   case JIT_CC_LT: state->flags &= (arg1 < arg2); break;
   case JIT_CC_GT: state->flags &= (arg1 > arg2); break;
   case JIT_CC_LE: state->flags &= (arg1 <= arg2); break;
   case JIT_CC_GE: state->flags &= (arg1 >= arg2); break;
   default: state->flags = 0; break;
   }
}

static void interp_rem(jit_interp_t *state, jit_ir_t *ir)
{
   const int64_t x = interp_get_int(state, ir->arg1);
   const int64_t y = interp_get_int(state, ir->arg2);

   state->regs[ir->result].integer = x - (x / y) * y;
}

static void interp_clamp(jit_interp_t *state, jit_ir_t *ir)
{
   const int64_t value = interp_get_int(state, ir->arg1);

   state->regs[ir->result].integer = value < 0 ? 0 : value;
}

static void interp_cset(jit_interp_t *state, jit_ir_t *ir)
{
   state->regs[ir->result].integer = !!(state->flags);
}

static void interp_branch_to(jit_interp_t *state, jit_value_t label)
{
   JIT_ASSERT(label.kind == JIT_VALUE_LABEL);
   state->pc = label.label;
   JIT_ASSERT(state->pc < state->func->nirs);
}

static void interp_jump(jit_interp_t *state, jit_ir_t *ir)
{
   switch (ir->cc) {
   case JIT_CC_NONE:
      interp_branch_to(state, ir->arg1);
      break;
   case JIT_CC_T:
      if (state->flags)
         interp_branch_to(state, ir->arg1);
      break;
   case JIT_CC_F:
      if (!state->flags)
         interp_branch_to(state, ir->arg1);
      break;
   default:
      interp_dump(state);
      fatal_trace("unhandled jump condition code");
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

   state->anchor->irpos = ir - state->func->irbuf;

   if (ir->arg1.handle == JIT_HANDLE_INVALID) {
      jit_dump_with_mark(state->func, state->anchor->irpos);
      jit_msg(NULL, DIAG_FATAL, "missing definition for subprogram");
   }
   else {
      jit_func_t *f = jit_get_func(state->func->jit, ir->arg1.handle);
      jit_entry_fn_t entry = load_acquire(&f->entry);
      (*entry)(f, state->anchor, state->args, state->tlab);
   }
}

static void interp_mov(jit_interp_t *state, jit_ir_t *ir)
{
   state->regs[ir->result] = interp_get_scalar(state, ir->arg1);
}

static void interp_csel(jit_interp_t *state, jit_ir_t *ir)
{
   if (state->flags)
      state->regs[ir->result].integer = interp_get_int(state, ir->arg1);
   else
      state->regs[ir->result].integer = interp_get_int(state, ir->arg2);
}

static void interp_neg(jit_interp_t *state, jit_ir_t *ir)
{
   state->regs[ir->result].integer = -interp_get_int(state, ir->arg1);
}

static void interp_fneg(jit_interp_t *state, jit_ir_t *ir)
{
   state->regs[ir->result].real = -interp_get_real(state, ir->arg1);
}

static void interp_not(jit_interp_t *state, jit_ir_t *ir)
{
   state->regs[ir->result].integer = !interp_get_int(state, ir->arg1);
}

static void interp_scvtf(jit_interp_t *state, jit_ir_t *ir)
{
   state->regs[ir->result].real = interp_get_int(state, ir->arg1);
}

static void interp_fcvtns(jit_interp_t *state, jit_ir_t *ir)
{
   const double f = interp_get_real(state, ir->arg1);
   state->regs[ir->result].integer = (int64_t)(f + copysign(0.5, f));
}

static void interp_lea(jit_interp_t *state, jit_ir_t *ir)
{
   state->regs[ir->result].pointer = interp_get_pointer(state, ir->arg1);
}

static void interp_fexp(jit_interp_t *state, jit_ir_t *ir)
{
   const double x = interp_get_real(state, ir->arg1);
   const double y = interp_get_real(state, ir->arg2);

   state->regs[ir->result].real = pow(x, y);
}

static void interp_exp(jit_interp_t *state, jit_ir_t *ir)
{
   const int64_t x = interp_get_int(state, ir->arg1);
   const int64_t y = interp_get_int(state, ir->arg2);

   if (ir->cc == JIT_CC_O) {
      int overflow = 0, xo = 0;

#define EXP_OVERFLOW(type) do {                                         \
         type xt = x, yt = y, r = 1;                                    \
         while (yt) {                                                   \
            if (yt & 1)                                                 \
               overflow |= xo || __builtin_mul_overflow(r, xt, &r);     \
            yt >>= 1;                                                   \
            xo |= __builtin_mul_overflow(xt, xt, &xt);                  \
         }                                                              \
         state->regs[ir->result].integer = r;                           \
      } while (0)

      FOR_EACH_SIZE(ir->size, EXP_OVERFLOW);

      state->flags = overflow;
   }
   else if (ir->cc == JIT_CC_C) {
      int overflow = 0, xo = 0;

#define UEXP_OVERFLOW(type) do {                                        \
         u##type xt = x, yt = y, r = 1;                                 \
         while (yt) {                                                   \
            if (yt & 1)                                                 \
               overflow |= xo || __builtin_mul_overflow(r, xt, &r);     \
            yt >>= 1;                                                   \
            xo |= __builtin_mul_overflow(xt, xt, &xt);                  \
         }                                                              \
         state->regs[ir->result].integer = r;                           \
      } while (0)

      FOR_EACH_SIZE(ir->size, UEXP_OVERFLOW);

      state->flags = overflow;
   }
   else
      state->regs[ir->result].integer = ipow(x, y);
}

static void interp_copy(jit_interp_t *state, jit_ir_t *ir)
{
   const size_t count = state->regs[ir->result].integer;
   void *dest = interp_get_pointer(state, ir->arg1);
   const void *src = interp_get_pointer(state, ir->arg2);

   JIT_ASSERT((uintptr_t)dest >= 4096 || count == 0);
   JIT_ASSERT((uintptr_t)src >= 4096 || count == 0);
   JIT_ASSERT(dest + count <= src || src + count <= dest);

   memcpy(dest, src, count);
}

static void interp_move(jit_interp_t *state, jit_ir_t *ir)
{
   const size_t count = state->regs[ir->result].integer;
   void *dest = interp_get_pointer(state, ir->arg1);
   const void *src = interp_get_pointer(state, ir->arg2);

   JIT_ASSERT((uintptr_t)dest >= 4096 || count == 0);
   JIT_ASSERT((uintptr_t)src >= 4096 || count == 0);

   memmove(dest, src, count);
}

static void interp_bzero(jit_interp_t *state, jit_ir_t *ir)
{
   const size_t count = state->regs[ir->result].integer;
   void *dest = interp_get_pointer(state, ir->arg1);

   memset(dest, '\0', count);
}

static void interp_memset(jit_interp_t *state, jit_ir_t *ir)
{
   const size_t bytes = state->regs[ir->result].integer;
   void *dest = interp_get_pointer(state, ir->arg1);
   const uint64_t value = interp_get_int(state, ir->arg2);

   JIT_ASSERT(dest != NULL);

#define MEMSET_LOOP(type) do {                      \
      JIT_ASSERT(bytes % sizeof(type) == 0);        \
      type *eptr = dest + bytes;                    \
      for (type *p = dest; p < eptr; p++)           \
         *p = value;                                \
   } while (0)

   FOR_EACH_SIZE(ir->size, MEMSET_LOOP);
}

static void interp_galloc(jit_interp_t *state, jit_ir_t *ir)
{
   jit_thread_local_t *thread = jit_thread_local();
   thread->anchor = state->anchor;

   state->anchor->irpos = ir - state->func->irbuf;

   uint64_t bytes = interp_get_int(state, ir->arg1);

   if (bytes > UINT32_MAX)
      jit_msg(NULL, DIAG_FATAL, "attempting to allocate %"PRIu64" byte object "
              "which is larger than the maximum supported %u bytes",
              bytes, UINT32_MAX);
   else if (bytes == 0)
      bytes = 1;   // Never return a NULL pointer

   state->regs[ir->result].pointer = mspace_alloc(state->mspace, bytes);

   thread->anchor = NULL;
}

static void interp_lalloc(jit_interp_t *state, jit_ir_t *ir)
{
   jit_thread_local_t *thread = jit_thread_local();
   thread->anchor = state->anchor;

   state->anchor->irpos = ir - state->func->irbuf;

   const size_t bytes = interp_get_int(state, ir->arg1);
   state->regs[ir->result].pointer = tlab_alloc(state->tlab, bytes);

   thread->anchor = NULL;
}

static void interp_salloc(jit_interp_t *state, jit_ir_t *ir)
{
   assert(ir->arg1.int64 + ir->arg2.int64 <= state->func->framesz);
   state->regs[ir->result].pointer = state->frame + ir->arg1.int64;
}

static void interp_exit(jit_interp_t *state, jit_ir_t *ir)
{
   state->anchor->irpos = ir - state->func->irbuf;
   __nvc_do_exit(ir->arg1.exit, state->anchor, state->args, state->tlab);
}

static void interp_getpriv(jit_interp_t *state, jit_ir_t *ir)
{
   JIT_ASSERT(ir->arg1.kind == JIT_VALUE_HANDLE);
   jit_func_t *f = jit_get_func(state->func->jit, ir->arg1.handle);
   void *ptr = load_acquire(jit_get_privdata_ptr(state->func->jit, f));
   state->regs[ir->result].pointer = ptr;
}

static void interp_putpriv(jit_interp_t *state, jit_ir_t *ir)
{
   JIT_ASSERT(ir->arg1.kind == JIT_VALUE_HANDLE);
   jit_func_t *f = jit_get_func(state->func->jit, ir->arg1.handle);
   void *ptr = interp_get_pointer(state, ir->arg2);
   store_release(jit_get_privdata_ptr(state->func->jit, f), ptr);
}

static void interp_case(jit_interp_t *state, jit_ir_t *ir)
{
   jit_scalar_t test = state->regs[ir->result];
   const int64_t cmp = interp_get_int(state, ir->arg1);

   if (test.integer == cmp)
      interp_branch_to(state, ir->arg2);
}

static void interp_trim(jit_interp_t *state, jit_ir_t *ir)
{
   assert(state->tlab->alloc >= state->anchor->watermark);
   state->tlab->alloc = state->anchor->watermark;
}

static void interp_reexec(jit_interp_t *state, jit_ir_t *ir)
{
   jit_entry_fn_t entry = load_acquire(&state->func->entry);
   (*entry)(state->func, state->anchor->caller, state->args, state->tlab);
}

static void interp_sadd(jit_interp_t *state, jit_ir_t *ir)
{
   const void *ptr = interp_get_pointer(state, ir->arg1);
   const int64_t addend = interp_get_int(state, ir->arg2);

#define SADD(type) do {                                         \
      u##type cur = *(u##type *)ptr;                            \
      *(u##type *)ptr = saturate_add(cur, addend);              \
   } while (0)

   FOR_EACH_SIZE(ir->size, SADD);
}

static void interp_pack(jit_interp_t *state, jit_ir_t *ir)
{
   const uint8_t *src = interp_get_pointer(state, ir->arg1);
   const size_t size = interp_get_int(state, ir->arg2);
   assert(size <= 64);  // TODO

   uint64_t abits = 0, bbits = 0;

   for (size_t i = 0; i < size; i++) {
      abits = (abits << 1) | (src[i] & 1);
      bbits = (bbits << 1) | ((src[i] >> 1) & 1);
   }

   state->args[0].integer = abits;
   state->args[1].integer = bbits;
}

static void interp_unpack(jit_interp_t *state, jit_ir_t *ir)
{
   uint8_t *dest = state->args[0].pointer;
   size_t size = state->args[1].integer;
   uint8_t strength = state->args[2].integer;

   uint64_t abits = interp_get_int(state, ir->arg1);
   uint64_t bbits = interp_get_int(state, ir->arg2);

   for (size_t i = 0; i < size; i++, abits >>= 1, bbits >>= 1)
      dest[size - i - 1] = (abits & 1) | ((bbits & 1) << 1) | strength;
}

static void interp_loop(jit_interp_t *state)
{
   for (;;) {
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
      case J_FSUB:
         interp_fsub(state, ir);
         break;
      case J_ADD:
         interp_add(state, ir);
         break;
      case J_FADD:
         interp_fadd(state, ir);
         break;
      case J_MUL:
         interp_mul(state, ir);
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
      case J_SHL:
         interp_shl(state, ir);
         break;
      case J_SHR:
         interp_shr(state, ir);
         break;
      case J_ASR:
         interp_asr(state, ir);
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
      case J_CCMP:
         interp_ccmp(state, ir);
         break;
      case J_FCMP:
         interp_fcmp(state, ir);
         break;
      case J_FCCMP:
         interp_fccmp(state, ir);
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
      case J_CLAMP:
         interp_clamp(state, ir);
         break;
      case J_DEBUG:
      case J_NOP:
         break;
      case MACRO_COPY:
         interp_copy(state, ir);
         break;
      case MACRO_MOVE:
         interp_move(state, ir);
         break;
      case MACRO_BZERO:
         interp_bzero(state, ir);
         break;
      case MACRO_MEMSET:
         interp_memset(state, ir);
         break;
      case MACRO_GALLOC:
         interp_galloc(state, ir);
         break;
      case MACRO_LALLOC:
         interp_lalloc(state, ir);
         break;
      case MACRO_SALLOC:
         interp_salloc(state, ir);
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
      case MACRO_GETPRIV:
         interp_getpriv(state, ir);
         break;
      case MACRO_PUTPRIV:
         interp_putpriv(state, ir);
         break;
      case MACRO_CASE:
         interp_case(state, ir);
         break;
      case MACRO_TRIM:
         interp_trim(state, ir);
         break;
      case MACRO_REEXEC:
         interp_reexec(state, ir);
         return;
      case MACRO_SADD:
         interp_sadd(state, ir);
         break;
      case MACRO_PACK:
         interp_pack(state, ir);
         break;
      case MACRO_UNPACK:
         interp_unpack(state, ir);
         break;
      default:
         interp_dump(state);
         fatal_trace("cannot interpret opcode %s", jit_op_name(ir->op));
      }
   }
}

void jit_interp(jit_func_t *f, jit_anchor_t *caller, jit_scalar_t *args,
                tlab_t *tlab)
{
   jit_entry_fn_t entry = load_acquire(&f->entry);
   if (unlikely(entry != jit_interp)) {
      // Raced with a code generation thread installing a compiled
      // version of this function
      return (*entry)(f, caller, args, tlab);
   }

   jit_fill_irbuf(f);

   if (f->next_tier && --(f->hotness) <= 0)
      jit_tier_up(f);

   jit_anchor_t anchor = {
      .caller    = caller,
      .func      = f,
      .watermark = tlab->alloc,
   };

   // Using VLAs here as we need these allocated on the stack so the
   // mspace GC can scan them
   jit_scalar_t regs[f->nregs + 1];
   unsigned char frame[f->framesz + 1];

#ifdef DEBUG
   memset(regs, 0xde, sizeof(jit_scalar_t) * f->nregs);
   memset(frame, 0xde, f->framesz);
#endif

   jit_interp_t state = {
      .args     = args,
      .regs     = regs,
      .nargs    = 0,
      .pc       = 0,
      .func     = f,
      .frame    = frame,
      .mspace   = jit_get_mspace(f->jit),
      .anchor   = &anchor,
      .tlab     = tlab,
   };

   interp_loop(&state);
}
