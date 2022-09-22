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
#include "jit/jit-exits.h"
#include "jit/jit-priv.h"
#include "rt/ffi.h"
#include "rt/mspace.h"
#include "tree.h"
#include "type.h"
#include "vcode.h"

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

static __thread jit_interp_t *call_stack = NULL;

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
   jit_dump_with_mark(state->func, state->pc - 1, false);

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
   if (state->flags & (1 << JIT_CC_O))  printf("O ");
   if (state->flags & (1 << JIT_CC_NO)) printf("NO ");
   if (state->flags & (1 << JIT_CC_C))  printf("C ");
   if (state->flags & (1 << JIT_CC_NC)) printf("NC ");
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

void jit_interp_trace(diag_t *d)
{
   for (jit_interp_t *p = call_stack; p != NULL; p = p->caller) {
      vcode_state_t state;
      vcode_state_save(&state);

      vcode_select_unit(p->func->unit);
      while (vcode_unit_context() != NULL)
         vcode_select_unit(vcode_unit_context());

      ident_t unit_name = vcode_unit_name();
      if (vcode_unit_kind() == VCODE_UNIT_INSTANCE)
         unit_name = ident_prefix(unit_name, well_known(W_ELAB), '.');

      vcode_state_restore(&state);

      const char *symbol = istr(p->func->name);
      tree_t enclosing = find_enclosing_decl(unit_name, symbol);
      if (enclosing == NULL)
         return;

      // TODO: this is much less accurate than the DWARF info
      const loc_t *loc = tree_loc(enclosing);

      jit_emit_trace(d, loc, enclosing, symbol);
   }
}

__attribute__((format(printf, 3, 4)))
static void interp_error(jit_interp_t *state, const loc_t *loc,
                         const char *fmt, ...)
{
   va_list ap;
   va_start(ap, fmt);

   diag_t *d = diag_new(DIAG_ERROR, loc);
   diag_vprintf(d, fmt, ap);
   diag_emit(d);

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

   if (ir->cc == JIT_CC_O) {
      int overflow = 0;

#define MUL_OVERFLOW(type) do {                                 \
         type i1 = arg1.integer, i2 = arg2.integer, i0;         \
         overflow = __builtin_mul_overflow(i1, i2, &i0);        \
         state->regs[ir->result].integer = i0;                  \
      } while (0)

      FOR_EACH_SIZE(ir->size, MUL_OVERFLOW);

      state->flags = (overflow << JIT_CC_O);
   }
   else if (ir->cc == JIT_CC_C) {
      int overflow = 0;

#define UMUL_OVERFLOW(type) do {                                \
         u##type i1 = arg1.integer, i2 = arg2.integer, i0;      \
         overflow = __builtin_mul_overflow(i1, i2, &i0);        \
         state->regs[ir->result].integer = i0;                  \
      } while (0)

      FOR_EACH_SIZE(ir->size, UMUL_OVERFLOW);

      state->flags = (overflow << JIT_CC_C);
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

   if (ir->cc == JIT_CC_O) {
      int overflow = 0;

#define SUB_OVERFLOW(type) do {                                 \
         type i1 = arg1.integer, i2 = arg2.integer, i0;         \
         overflow = __builtin_sub_overflow(i1, i2, &i0);        \
         state->regs[ir->result].integer = i0;                  \
      } while (0)

      FOR_EACH_SIZE(ir->size, SUB_OVERFLOW);

      state->flags = (overflow << JIT_CC_O);
   }
   else if (ir->cc == JIT_CC_C) {
      int overflow = 0;

#define USUB_OVERFLOW(type) do {                                \
         u##type i1 = arg1.integer, i2 = arg2.integer, i0;      \
         overflow = __builtin_sub_overflow(i1, i2, &i0);        \
         state->regs[ir->result].integer = i0;                  \
      } while (0)

      FOR_EACH_SIZE(ir->size, USUB_OVERFLOW);

      state->flags = (overflow << JIT_CC_C);
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

   if (ir->cc == JIT_CC_O) {
      int overflow = 0;

#define ADD_OVERFLOW(type) do {                                 \
         type i1 = arg1.integer, i2 = arg2.integer, i0;         \
         overflow = __builtin_add_overflow(i1, i2, &i0);        \
         state->regs[ir->result].integer = i0;                  \
      } while (0)

      FOR_EACH_SIZE(ir->size, ADD_OVERFLOW);

      state->flags = (overflow << JIT_CC_O);
   }
   else if (ir->cc == JIT_CC_C) {
      int overflow = 0;

#define UADD_OVERFLOW(type) do {                                \
         u##type i1 = arg1.integer, i2 = arg2.integer, i0;      \
         overflow = __builtin_add_overflow(i1, i2, &i0);        \
         state->regs[ir->result].integer = i0;                  \
      } while (0)

      FOR_EACH_SIZE(ir->size, UADD_OVERFLOW);

      state->flags = (overflow << JIT_CC_C);
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
   JIT_ASSERT(arg2.pointer != NULL);
   JIT_ASSERT((uintptr_t)arg2.pointer >= 4096);

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
   warnf("loaded poison value in R%d", reg);
}
#endif

static void interp_uload(jit_interp_t *state, jit_ir_t *ir)
{
   jit_scalar_t arg1 = interp_get_value(state, ir->arg1);

   JIT_ASSERT(ir->size != JIT_SZ_UNSPEC);
   JIT_ASSERT(arg1.pointer != NULL);
   JIT_ASSERT((uintptr_t)arg1.pointer >= 4096);

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
   JIT_ASSERT((uintptr_t)arg1.pointer >= 4096);

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

   switch (ir->cc) {
   case JIT_CC_EQ:
      state->flags = (arg1.integer == arg2.integer) << JIT_CC_EQ;
      break;
   case JIT_CC_NE:
      state->flags = (arg1.integer != arg2.integer) << JIT_CC_NE;
      break;
   case JIT_CC_LT:
      state->flags = (arg1.integer < arg2.integer) << JIT_CC_LT;
      break;
   case JIT_CC_GT:
      state->flags = (arg1.integer > arg2.integer) << JIT_CC_GT;
      break;
   case JIT_CC_LE:
      state->flags = (arg1.integer <= arg2.integer) << JIT_CC_LE;
      break;
   case JIT_CC_GE:
      state->flags = (arg1.integer >= arg2.integer) << JIT_CC_GE;
      break;
   default:
      state->flags = 0;
      break;
   }
}

static void interp_fcmp(jit_interp_t *state, jit_ir_t *ir)
{
   jit_scalar_t arg1 = interp_get_value(state, ir->arg1);
   jit_scalar_t arg2 = interp_get_value(state, ir->arg2);

   switch (ir->cc) {
   case JIT_CC_EQ:
      state->flags = (arg1.real == arg2.real) << JIT_CC_EQ;
      break;
   case JIT_CC_NE:
      state->flags = (arg1.real != arg2.real) << JIT_CC_NE;
      break;
   case JIT_CC_LT:
      state->flags = (arg1.real < arg2.real) << JIT_CC_LT;
      break;
   case JIT_CC_GT:
      state->flags = (arg1.real > arg2.real) << JIT_CC_GT;
      break;
   case JIT_CC_LE:
      state->flags = (arg1.real <= arg2.real) << JIT_CC_LE;
      break;
   case JIT_CC_GE:
      state->flags = (arg1.real >= arg2.real) << JIT_CC_GE;
      break;
   default:
      state->flags = 0;
   }
}

static void interp_rem(jit_interp_t *state, jit_ir_t *ir)
{
   const int64_t x = interp_get_value(state, ir->arg1).integer;
   const int64_t y = interp_get_value(state, ir->arg2).integer;

   state->regs[ir->result].integer = x - (x / y) * y;
}

static void interp_cset(jit_interp_t *state, jit_ir_t *ir)
{
   state->regs[ir->result].integer = !!(state->flags);
}

static void interp_branch_to(jit_interp_t *state, jit_value_t label)
{
   const int target = interp_get_value(state, label).integer;
   if (state->backedge > 0 && target < state->pc) {
      // Limit the number of loop iterations in bounded mode
      if (--(state->backedge) == 0)
         interp_error(state, NULL, "maximum iteration limit reached");
   }

   state->pc = target;
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

   if (ir->arg1.handle == JIT_HANDLE_INVALID)
      state->abort = true;
   else {
      jit_func_t *f = jit_get_func(state->func->jit, ir->arg1.handle);
      if (!(*f->entry)(f, state->args))
         state->abort = true;
   }
}

static void interp_mov(jit_interp_t *state, jit_ir_t *ir)
{
   state->regs[ir->result] = interp_get_value(state, ir->arg1);
}

static void interp_csel(jit_interp_t *state, jit_ir_t *ir)
{
   if (state->flags)
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

   JIT_ASSERT((uintptr_t)dest >= 4096 || count == 0);
   JIT_ASSERT((uintptr_t)src >= 4096 || count == 0);

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

   x_range_fail(value, left, right, dir, where, hint);
}

static void interp_index_fail(jit_interp_t *state)
{
   int32_t      value = state->args[0].integer;
   int32_t      left  = state->args[1].integer;
   int32_t      right = state->args[2].integer;
   range_kind_t dir   = state->args[3].integer;
   tree_t       where = state->args[4].pointer;
   tree_t       hint  = state->args[5].pointer;

   x_index_fail(value, left, right, dir, where, hint);
}

static void interp_overflow(jit_interp_t *state)
{
   int32_t lhs   = state->args[0].integer;
   int32_t rhs   = state->args[1].integer;
   tree_t  where = state->args[2].pointer;

   x_overflow(lhs, rhs, where);
}

static void interp_null_deref(jit_interp_t *state)
{
   tree_t where = state->args[0].pointer;

   x_null_deref(where);
}

static void interp_length_fail(jit_interp_t *state)
{
   int32_t left  = state->args[0].integer;
   int32_t right = state->args[1].integer;
   int32_t dim   = state->args[2].integer;
   tree_t  where = state->args[3].pointer;

   x_length_fail(left, right, dim, where);
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

   x_exponent_fail(value, where);
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

static void interp_func_wait(jit_interp_t *state)
{
   interp_error(state, NULL, "cannot wait inside function call");
}

static void interp_report(jit_interp_t *state)
{
   uint8_t *msg      = state->args[0].pointer;
   int32_t  len      = state->args[1].integer;
   int32_t  severity = state->args[2].integer;
   tree_t   where    = state->args[3].pointer;

   x_report(msg, len, severity, where);
}

static void interp_assert_fail(jit_interp_t *state)
{
   uint8_t *msg        = state->args[0].pointer;
   int32_t  len        = state->args[1].integer;
   int32_t  severity   = state->args[2].integer;
   int64_t  hint_left  = state->args[3].integer;
   int64_t  hint_right = state->args[4].integer;
   int8_t   hint_valid = state->args[5].integer;
   tree_t   where      = state->args[6].pointer;

   x_assert_fail(msg, len, severity, hint_left, hint_right, hint_valid, where);
}

static void interp_scalar_init_signal(jit_interp_t *state)
{
   int32_t count  = state->args[0].integer;
   int32_t size   = state->args[1].integer;
   int64_t value  = state->args[2].integer;
   int32_t flags  = state->args[3].integer;
   tree_t  where  = state->args[4].pointer;
   int32_t offset = state->args[5].integer;

   sig_shared_t *ss;
   if (!jit_has_runtime(state->func->jit))
      ss = NULL;   // Called during constant folding
   else
      ss = x_init_signal(count, size, (const uint8_t *)&value,
                         flags, where, offset);

   state->args[0].pointer = ss;
   state->nargs = 1;
}

static void interp_scalar_init_signals(jit_interp_t *state)
{
   int32_t  count  = state->args[0].integer;
   int32_t  size   = state->args[1].integer;
   uint8_t *value  = state->args[2].pointer;
   int32_t  flags  = state->args[3].integer;
   tree_t   where  = state->args[4].pointer;
   int32_t  offset = state->args[5].integer;

   sig_shared_t *ss;
   if (!jit_has_runtime(state->func->jit))
      ss = NULL;   // Called during constant folding
   else
      ss = x_init_signal(count, size, value, flags, where, offset);

   state->args[0].pointer = ss;
   state->nargs = 1;
}

static void interp_drive_signal(jit_interp_t *state)
{
   if (!jit_has_runtime(state->func->jit))
      return;   // Called during constant folding

   sig_shared_t *ss     = state->args[0].pointer;
   int32_t       offset = state->args[1].integer;
   int32_t       count  = state->args[2].integer;

   x_drive_signal(ss, offset, count);
}

static void interp_sched_process(jit_interp_t *state)
{
   if (!jit_has_runtime(state->func->jit))
      return;   // TODO: this should not be necessary

   int64_t after = state->args[0].integer;

   x_sched_process(after);
}

static void interp_sched_waveform(jit_interp_t *state)
{
   sig_shared_t *shared = state->args[0].pointer;
   int32_t       offset = state->args[1].integer;
   int32_t       count  = state->args[2].integer;
   jit_scalar_t  value  = { .integer = state->args[3].integer };
   int64_t       after  = state->args[4].integer;
   int64_t       reject = state->args[5].integer;
   bool          scalar = state->args[6].integer;

   if (scalar)
      x_sched_waveform_s(shared, offset, value.integer, after, reject);
   else
      x_sched_waveform(shared, offset, value.pointer, count, after, reject);
}

static void interp_sched_event(jit_interp_t *state)
{
   sig_shared_t *shared = state->args[0].pointer;
   int32_t       offset = state->args[1].integer;
   int32_t       count  = state->args[2].integer;
   int8_t        recur  = state->args[3].integer;
   sig_shared_t *wake   = state->args[4].pointer;

   x_sched_event(shared, offset, count, recur, wake);
}

static void interp_test_event(jit_interp_t *state)
{
   sig_shared_t *shared = state->args[0].pointer;
   int32_t       offset = state->args[1].integer;
   int32_t       count  = state->args[2].integer;

   state->args[0].integer = x_test_net_event(shared, offset, count);
   state->nargs = 1;
}

static void interp_test_active(jit_interp_t *state)
{
   sig_shared_t *shared = state->args[0].pointer;
   int32_t       offset = state->args[1].integer;
   int32_t       count  = state->args[2].integer;

   state->args[0].integer = x_test_net_active(shared, offset, count);
   state->nargs = 1;
}

static void interp_now(jit_interp_t *state)
{
   state->args[0].integer = x_now();
   state->nargs = 1;
}

static void interp_file_open(jit_interp_t *state)
{
   int8_t   *status     = state->args[0].pointer;
   void    **_fp        = state->args[1].pointer;
   uint8_t  *name_bytes = state->args[2].pointer;
   int32_t   name_len   = state->args[3].integer;
   int32_t   mode       = state->args[4].integer;
   tree_t    where      = state->args[5].pointer;

   x_file_open(status, _fp, name_bytes, name_len, mode, where);
}

static void interp_file_close(jit_interp_t *state)
{
   void **_fp = state->args[0].pointer;

   x_file_close(_fp);
}

static void interp_file_read(jit_interp_t *state)
{
   void    **_fp   = state->args[0].pointer;
   uint8_t  *data  = state->args[1].pointer;
   int32_t   size  = state->args[2].integer;
   int32_t   count = state->args[3].integer;
   int32_t  *out   = state->args[4].pointer;

   x_file_read(_fp, data, size, count, out);
}

static void interp_file_write(jit_interp_t *state)
{
   void    **_fp  = state->args[0].pointer;
   uint8_t  *data = state->args[1].pointer;
   int32_t   len  = state->args[2].integer;

   x_file_write(_fp, data, len);
}

static void interp_endfile(jit_interp_t *state)
{
   void *_fp = state->args[0].pointer;

   state->args[0].integer = x_endfile(_fp);
   state->nargs = 1;
}

static void interp_file_flush(jit_interp_t *state)
{
   void *_fp = state->args[0].pointer;

   x_file_flush(_fp);
}

static void interp_string_to_int(jit_interp_t *state)
{
   uint8_t *ptr  = state->args[0].pointer;
   int32_t  len  = state->args[1].integer;
   int32_t *used = state->args[2].pointer;

   state->args[0].integer = x_string_to_int(ptr, len, used);
   state->nargs = 1;
}

static void interp_string_to_real(jit_interp_t *state)
{
   uint8_t *ptr = state->args[0].pointer;
   int32_t  len = state->args[1].integer;

   state->args[0].real = x_string_to_real(ptr, len);
   state->nargs = 1;
}

static void interp_int_to_string(jit_interp_t *state)
{
   int64_t value = state->args[0].integer;

   char *buf = mspace_alloc(state->mspace, 20);
   if (buf == NULL)
      return;

   ffi_uarray_t u = x_int_to_string(value, buf, 20);
   state->args[0].pointer = u.ptr;
   state->args[1].integer = u.dims[0].left;
   state->args[2].integer = u.dims[0].length;
   state->nargs = 3;
}

static void interp_real_to_string(jit_interp_t *state)
{
   double value = state->args[0].real;

   char *buf = mspace_alloc(state->mspace, 32);
   if (buf == NULL)
      return;

   ffi_uarray_t u = x_real_to_string(value, buf, 32);
   state->args[0].pointer = u.ptr;
   state->args[1].integer = u.dims[0].left;
   state->args[2].integer = u.dims[0].length;
   state->nargs = 3;
}

static void interp_canon_value(jit_interp_t *state)
{
   uint8_t *ptr = state->args[0].pointer;
   int32_t  len = state->args[1].integer;

   char *buf = mspace_alloc(state->mspace, len);
   if (buf == NULL)
      return;

   ffi_uarray_t u = x_canon_value(ptr, len, buf);
   state->args[0].pointer = u.ptr;
   state->args[1].integer = u.dims[0].left;
   state->args[2].integer = u.dims[0].length;
   state->nargs = 3;
}

static void interp_debug_out(jit_interp_t *state)
{
   int64_t value = state->args[0].integer;
   debugf("DEBUG %"PRIi64, value);
}

static void interp_alias_signal(jit_interp_t *state)
{
   sig_shared_t *ss    = state->args[0].pointer;
   tree_t        where = state->args[1].pointer;

   x_alias_signal(ss, where);
}

static void interp_map_signal(jit_interp_t *state)
{
   sig_shared_t  *src_ss     = state->args[0].pointer;
   uint32_t       src_offset = state->args[1].integer;
   sig_shared_t  *dst_ss     = state->args[2].pointer;
   uint32_t       dst_offset = state->args[3].integer;
   uint32_t       src_count  = state->args[4].integer;
   uint32_t       dst_count  = state->args[5].integer;
   ffi_closure_t *closure    = state->args[6].pointer;

   x_map_signal(src_ss, src_offset, dst_ss, dst_offset, src_count,
                dst_count, closure);
}

static void interp_map_const(jit_interp_t *state)
{
   sig_shared_t *dst_ss     = state->args[0].pointer;
   uint32_t      dst_offset = state->args[1].integer;
   jit_scalar_t  initval    = { .integer = state->args[2].integer };
   uint32_t      dst_count  = state->args[3].integer;
   bool          scalar     = state->args[4].integer;

   const void *vptr = scalar ? &initval.integer : initval.pointer;

   x_map_const(dst_ss, dst_offset, vptr, dst_count);
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

   case JIT_EXIT_FUNC_WAIT:
      interp_func_wait(state);
      break;

   case JIT_EXIT_INIT_SIGNAL:
      interp_scalar_init_signal(state);
      break;

   case JIT_EXIT_INIT_SIGNALS:
      interp_scalar_init_signals(state);
      break;

   case JIT_EXIT_DRIVE_SIGNAL:
      interp_drive_signal(state);
      break;

   case JIT_EXIT_SCHED_PROCESS:
      interp_sched_process(state);
      break;

   case JIT_EXIT_SCHED_WAVEFORM:
      interp_sched_waveform(state);
      break;

   case JIT_EXIT_TEST_EVENT:
      interp_test_event(state);
      break;

   case JIT_EXIT_TEST_ACTIVE:
      interp_test_active(state);
      break;

   case JIT_EXIT_SCHED_EVENT:
      interp_sched_event(state);
      break;

   case JIT_EXIT_NOW:
      interp_now(state);
      break;

   case JIT_EXIT_FILE_OPEN:
      interp_file_open(state);
      break;

   case JIT_EXIT_FILE_CLOSE:
      interp_file_close(state);
      break;

   case JIT_EXIT_FILE_READ:
      interp_file_read(state);
      break;

   case JIT_EXIT_FILE_WRITE:
      interp_file_write(state);
      break;

   case JIT_EXIT_ENDFILE:
      interp_endfile(state);
      break;

   case JIT_EXIT_FILE_FLUSH:
      interp_file_flush(state);
      break;

   case JIT_EXIT_STRING_TO_INT:
      interp_string_to_int(state);
      break;

   case JIT_EXIT_STRING_TO_REAL:
      interp_string_to_real(state);
      break;

   case JIT_EXIT_CANON_VALUE:
      interp_canon_value(state);
      break;

   case JIT_EXIT_DEBUG_OUT:
      interp_debug_out(state);
      break;

   case JIT_EXIT_ALIAS_SIGNAL:
      interp_alias_signal(state);
      break;

   case JIT_EXIT_MAP_SIGNAL:
      interp_map_signal(state);
      break;

   case JIT_EXIT_MAP_CONST:
      interp_map_const(state);
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

static void interp_getpriv(jit_interp_t *state, jit_ir_t *ir)
{
   JIT_ASSERT(ir->arg1.kind == JIT_VALUE_HANDLE);
   jit_func_t *f = jit_get_func(state->func->jit, ir->arg1.handle);
   void *ptr = jit_get_privdata(state->func->jit, f);
   state->regs[ir->result].pointer = ptr;
}

static void interp_putpriv(jit_interp_t *state, jit_ir_t *ir)
{
   JIT_ASSERT(ir->arg1.kind == JIT_VALUE_HANDLE);
   jit_func_t *f = jit_get_func(state->func->jit, ir->arg1.handle);
   void *ptr = interp_get_value(state, ir->arg2).pointer;
   jit_put_privdata(state->func->jit, f, ptr);
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
      case MACRO_GETPRIV:
         interp_getpriv(state, ir);
         break;
      case MACRO_PUTPRIV:
         interp_putpriv(state, ir);
         break;
      default:
         interp_dump(state);
         fatal_trace("cannot interpret opcode %s", jit_op_name(ir->op));
      }
   } while (!state->abort);
}

bool jit_interp(jit_func_t *f, jit_scalar_t *args)
{
   if (f->irbuf == NULL)
      jit_irgen(f);

   if (f->next_tier && --(f->hotness) <= 0)
      jit_tier_up(f);

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
      .nargs    = 0,
      .pc       = 0,
      .func     = f,
      .frame    = frame,
      .mspace   = jit_get_mspace(f->jit),
      .backedge = jit_backedge_limit(f->jit),
      .caller   = call_stack,
   };

   call_stack = &state;

   interp_loop(&state);

   assert(call_stack == &state);
   call_stack = state.caller;

   return !state.abort;
}

void jit_interp_abort(int code)
{
   assert(call_stack != NULL);
   call_stack->abort = true;
   jit_set_exit_status(call_stack->func->jit, code);
}
