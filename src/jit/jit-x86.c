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
#include "ident.h"
#include "opt.h"
#include "jit/jit-priv.h"
#include "jit/jit.h"

#include <assert.h>
#include <inttypes.h>
#include <string.h>
#include <stdlib.h>

typedef enum {
   EXIT_STUB,
   CALL_STUB,
   ALLOC_STUB,

   NUM_STUBS
} jit_x86_stub_t;

typedef struct {
   jit_t          *jit;
   code_cache_t   *code;
   jit_entry_fn_t  stubs[NUM_STUBS];
} jit_x86_state_t;

// Conservative guess at the number of bytes emitted per IR
#define BYTES_PER_IR 8

// Size of fixed part of call frame
#define FRAME_FIXED_SIZE 40

////////////////////////////////////////////////////////////////////////////////
// X86 assembler

#define __(...) do {                                                    \
      const uint8_t __b[] = { __VA_ARGS__ };                            \
      code_blob_emit(blob, __b, ARRAY_LEN(__b));                        \
   } while (0)

typedef enum {
   X86_REG,
   X86_IMM,
   X86_ADDR,
   X86_PATCH,
} x86_kind_t;

typedef int8_t x86_reg_t;

typedef struct {
   x86_kind_t kind;
   union {
      x86_reg_t reg;
      int64_t   imm;
      struct {
         x86_reg_t reg;
         int32_t   off;
      } addr;
   };
} x86_operand_t;

#define REG(x) ((x86_operand_t){ X86_REG, { .reg = (x) }})
#define IMM(x) ((x86_operand_t){ X86_IMM, { .imm = (x) }})
#define PTR(x) ((x86_operand_t){ X86_IMM, { .imm = (uintptr_t)(x) }})
#define ADDR(r, o) ((x86_operand_t){ X86_ADDR, { .addr = { (r).reg, (o) }}})
#define PATCH(n) ((x86_operand_t){ X86_PATCH, { .imm = (n) }})

#define COMBINE(a, b) (((a).kind << 4) | (b).kind)
#define REG_REG ((X86_REG << 4) | X86_REG)
#define MEM_REG ((X86_ADDR << 4) | X86_REG)
#define REG_MEM ((X86_REG << 4) | X86_ADDR)
#define REG_IMM ((X86_REG << 4) | X86_IMM)

static const x86_operand_t __EAX = REG(0);
static const x86_operand_t __ECX = REG(1);
static const x86_operand_t __EDX = REG(2);
static const x86_operand_t __EBX = REG(3);
static const x86_operand_t __ESP = REG(4);
static const x86_operand_t __EBP = REG(5);
static const x86_operand_t __ESI = REG(6);
static const x86_operand_t __EDI = REG(7);
static const x86_operand_t __R8  = REG(16);
static const x86_operand_t __R9  = REG(17);

#define FPTR_REG   __EDI
#define ANCHOR_REG __ESI
#define ARGS_REG   __R8
#define TLAB_REG   __R9
#define FLAGS_REG  __EBX

typedef enum {
   __BYTE = 1,
   __WORD = 2,
   __DWORD = 4,
   __QWORD = 8
} x86_size_t;

typedef enum {
   X86_CMP_O  = 0x00,
   X86_CMP_C  = 0x02,
   X86_CMP_EQ = 0x04,
   X86_CMP_NE = 0x05,
   X86_CMP_LE = 0x0e,
   X86_CMP_LT = 0x0c,
   X86_CMP_GE = 0x0d,
   X86_CMP_GT = 0x0f,
} x86_cmp_t;

#define NOP() __(0x90)
#define RET() __(0xc3)
#define LEAVE() __(0xc9)
#define INT3() __(0xcc)
#define CQO() __(0x48, 0x99)
#define CDQ() __(0x99)
#define CWD() __(0x66, 0x99)
#define CWDE() __(0x98)
#define CDQE() __(0x48, 0x98)
#define PUSH(src) asm_push(blob, (src))
#define POP(dst) asm_pop(blob, (dst))
#define ADD(dst, src, size) asm_add(blob, (dst), (src), (size))
#define SUB(dst, src, size) asm_sub(blob, (dst), (src), (size))
#define MUL(src, size) asm_mul(blob, (src), (size))
#define IMUL(dst, src, size) asm_imul(blob, (dst), (src), (size))
#define IDIV(src, size) asm_idiv(blob, (src), (size))
#define AND(dst, src, size) asm_and(blob, (dst), (src), (size))
#define OR(dst, src, size) asm_or(blob, (dst), (src), (size))
#define SHL(src, size) asm_shl(blob, (src), (size))
#define SAR(src, size) asm_sar(blob, (src), (size))
#define XOR(dst, src, size) asm_xor(blob, (dst), (src), (size))
#define NEG(dst, size) asm_neg(blob, (dst), (size))
#define MOV(dst, src, size) asm_mov(blob, (dst), (src), (size))
#define MOVSX(dst, src, dsize, ssize) \
   asm_movsx(blob, (dst), (src), (dsize), (ssize))
#define MOVZX(dst, src, dsize, ssize) \
   asm_movzx(blob, (dst), (src), (dsize), (ssize))
#define CMOVZ(dst, src, size)                           \
   asm_cmovcc(blob, (dst), (src), (size), X86_CMP_EQ)
#define CMOVNZ(dst, src, size)                          \
   asm_cmovcc(blob, (dst), (src), (size), X86_CMP_NE)
#define LEA(dst, addr) asm_lea(blob, (dst), (addr))
#define SETO(dst) asm_setcc(blob, (dst), X86_CMP_O)
#define SETC(dst) asm_setcc(blob, (dst), X86_CMP_C)
#define SETZ(dst) asm_setcc(blob, (dst), X86_CMP_EQ)
#define SETNZ(dst) asm_setcc(blob, (dst), X86_CMP_NE)
#define SETLT(dst) asm_setcc(blob, (dst), X86_CMP_LT)
#define SETGT(dst) asm_setcc(blob, (dst), X86_CMP_GT)
#define SETGE(dst) asm_setcc(blob, (dst), X86_CMP_GE)
#define SETLE(dst) asm_setcc(blob, (dst), X86_CMP_LE)
#define TEST(src1, src2, size) asm_test(blob, (src1), (src2), (size))
#define CMP(src1, src2, size) asm_cmp(blob, (src1), (src2), (size))
#define CALL(addr) asm_call(blob, (addr))
#define JMP(addr) asm_jmp(blob, (addr))
#define JZ(addr) asm_jcc(blob, (addr), X86_CMP_EQ)
#define JNZ(addr) asm_jcc(blob, (addr), X86_CMP_NE)

#define __MODRM(m, r, rm) (((m & 3) << 6) | (((r) & 7) << 3) | (rm & 7))
#define __REX(size, xr, xsib, xrm) \
   __(0x40 | ((size) << 3) | ((xr) << 2) | ((xsib) << 1) | (xrm))

#define __IMM64(x) __IMM32(x), __IMM32((x) >> 32)
#define __IMM32(x) __IMM16(x), __IMM16((x) >> 16)
#define __IMM16(x) (x) & 0xff, ((x) >> 8) & 0xff

#define is_imm8(x) ((x) >= INT8_MIN && (x) <= INT8_MAX)
#define is_imm16(x) ((x) >= INT16_MIN && (x) <= INT16_MAX)
#define is_imm32(x) ((x) >= INT32_MIN && (x) <= INT32_MAX)

static void asm_rex(code_blob_t *blob, x86_size_t size, x86_reg_t modrm_reg,
                    x86_reg_t modrm_rm, x86_reg_t modrm_sib)
{
   if (size > __DWORD || (modrm_reg & 0x10) || (modrm_sib & 0x10)
       || (modrm_rm & 0x10))
      __REX(size > __DWORD, !!(modrm_reg & 0x10), !!(modrm_sib & 0x10),
            !!(modrm_rm & 0x10));
}

static void asm_xor(code_blob_t *blob, x86_operand_t dst, x86_operand_t src,
                    x86_size_t size)
{
   assert(COMBINE(dst, src) == REG_REG);
   asm_rex(blob, size, src.reg, dst.reg, 0);
   __(0x33, __MODRM(3, src.reg, dst.reg));
}

static void asm_neg(code_blob_t *blob, x86_operand_t dst, x86_size_t size)
{
   assert(dst.kind == X86_REG);
   asm_rex(blob, size, 0, dst.reg, 0);
   __(0xf7, __MODRM(3, 3, dst.reg));
}

static void asm_lea(code_blob_t *blob, x86_operand_t dst, x86_operand_t src)
{
   assert(COMBINE(dst, src) == REG_MEM);
   asm_rex(blob, __QWORD, src.reg, dst.reg, 0);
   if (is_imm8(src.addr.off))
      __(0x8d, __MODRM(1, dst.reg, src.addr.reg), src.addr.off);
   else
      __(0x8d, __MODRM(2, dst.reg, src.addr.reg), __IMM32(src.addr.off));
}

static void asm_mov(code_blob_t *blob, x86_operand_t dst, x86_operand_t src,
                    x86_size_t size)
{
   switch (COMBINE(dst, src)) {
   case REG_REG:
      if (dst.reg != src.reg) {
         asm_rex(blob, size, dst.reg, src.reg, 0);
         __(0x8b, __MODRM(3, dst.reg, src.reg));
      }
      break;

   case MEM_REG:
      asm_rex(blob, size, src.addr.reg, dst.reg, 0);
      switch (size) {
      case __QWORD:
      case __DWORD: __(0x89); break;
      case __WORD: __(0x66, 0x89); break;
      case __BYTE: __(0x88); break;
      }
      if (is_imm8(dst.addr.off))
         __(__MODRM(1, src.reg, dst.addr.reg), dst.addr.off);
      else
         __(__MODRM(2, src.reg, dst.addr.reg), __IMM32(dst.addr.off));
      break;

   case REG_MEM:
      asm_rex(blob, size, dst.reg, src.addr.reg, 0);
      switch (size) {
      case __QWORD:
      case __DWORD: __(0x8b); break;
      case __WORD: __(0x66, 0x8b); break;
      case __BYTE: __(0x8a); break;
      }
      if (is_imm8(src.addr.off))
         __(__MODRM(1, dst.reg, src.addr.reg), src.addr.off);
      else
         __(__MODRM(2, dst.reg, src.addr.reg), __IMM32(src.addr.off));
      break;

   case REG_IMM:
      if (src.imm == 0)
         XOR(dst, dst, MIN(size, __DWORD));
      else {
         switch (size) {
         case __QWORD:
            if (is_imm32(src.imm) && src.imm > 0) {
               asm_rex(blob, __DWORD, 0, dst.reg, 0);
               __(0xb8 + (dst.reg & 7), __IMM32(src.imm));
            }
            else {
               asm_rex(blob, __QWORD, 0, dst.reg, 0);
               __(0xb8 + (dst.reg & 7), __IMM64(src.imm));
            }
            break;
         case __DWORD:
            assert(is_imm32(src.imm));
            asm_rex(blob, __DWORD, 0, dst.reg, 0);
            __(0xb8 + (dst.reg & 7), __IMM32(src.imm));
            break;
         default:
            fatal_trace("unhandled immediate size %d in asm_mov", size);
         }
      }
      break;

   default:
      fatal_trace("unhandled operand combination in asm_mov");
   }
}

static void asm_movsx(code_blob_t *blob, x86_operand_t dst, x86_operand_t src,
                      x86_size_t dsize, x86_size_t ssize)
{
   switch (COMBINE(dst, src)) {
   case REG_REG:
      asm_rex(blob, dsize, src.reg, dst.reg, 0);
      switch (ssize) {
      case __BYTE: __(0x0f, 0xbe, __MODRM(3, src.reg, dst.reg)); break;
      default:
         fatal_trace("unhandled source size %d in asm_movsx", ssize);
      }
      break;

   case REG_MEM:
      asm_rex(blob, dsize, src.reg, dst.reg, 0);
      switch (ssize) {
      case __DWORD:
         __(0x63, __MODRM(1, dst.reg, src.addr.reg), src.addr.off);
         break;
      case __BYTE:
         __(0x0f, 0xbe, __MODRM(1, dst.reg, src.addr.reg), src.addr.off);
         break;
      default:
         fatal_trace("unhandled source size %d in asm_movsx", ssize);
      }
      break;

   default:
      fatal_trace("unhandled operand combination in asm_mov");
   }
}

static void asm_movzx(code_blob_t *blob, x86_operand_t dst, x86_operand_t src,
                      x86_size_t dsize, x86_size_t ssize)
{
   assert(COMBINE(dst, src) == REG_MEM);
   asm_rex(blob, ssize, dst.reg, src.addr.reg, 0);
   switch (ssize) {
   case __QWORD:
   case __DWORD: __(0x8b); break;
   case __WORD: __(0x0f, 0xb7); break;
   case __BYTE: __(0x0f, 0xb6); break;
   }
   if (is_imm8(src.addr.off))
      __(__MODRM(1, dst.reg, src.addr.reg), src.addr.off);
   else
      __(__MODRM(2, dst.reg, src.addr.reg), __IMM32(src.addr.off));
}

static void asm_cmovcc(code_blob_t *blob, x86_operand_t dst, x86_operand_t src,
                       x86_size_t size, x86_cmp_t cmp)
{
   assert(COMBINE(dst, src) == REG_REG);
   asm_rex(blob, size, src.reg, dst.reg, 0);
   switch (size) {
   case __DWORD:
   case __QWORD:
      __(0x0f, 0x40 + cmp, __MODRM(3, dst.reg, src.reg));
      break;
   default:
      fatal_trace("unhandled size %d in asm_cmovcc", size);
   }
}

static void asm_push(code_blob_t *blob, x86_operand_t src)
{
   asm_rex(blob, __DWORD, 0, src.reg, 0);   // Actually pushes QWORD
   __(0x50 + (src.reg & 7));
}

static void asm_pop(code_blob_t *blob, x86_operand_t dst)
{
   asm_rex(blob, __DWORD, 0, dst.reg, 0);   // Actually pops QWORD
   __(0x58 + (dst.reg & 7));
}

static void asm_add(code_blob_t *blob, x86_operand_t dst, x86_operand_t src,
                    x86_size_t size)
{
   assert(COMBINE(dst, src) == REG_REG);
   asm_rex(blob, size, src.reg, dst.reg, 0);
   switch (size) {
   case __BYTE:
      __(0x00, __MODRM(3, src.reg, dst.reg));
      break;
   case __WORD:
      __(0x66, 0x01, __MODRM(3, src.reg, dst.reg));
      break;
   case __DWORD:
   case __QWORD:
      __(0x01, __MODRM(3, src.reg, dst.reg));
      break;
   }
}

static void asm_sub(code_blob_t *blob, x86_operand_t dst, x86_operand_t src,
                    x86_size_t size)
{
   switch (COMBINE(dst, src)) {
   case REG_IMM:
      asm_rex(blob, size, 0, dst.reg, 0);
      if (is_imm8(src.imm))
         __(0x83, __MODRM(3, 5, dst.reg), src.imm);
      else
         __(0x81, __MODRM(3, 5, dst.reg), __IMM32(src.imm));
      break;

   case REG_REG:
      asm_rex(blob, size, src.reg, dst.reg, 0);
      __(0x29, __MODRM(3, src.reg, dst.reg));
      break;

   default:
      fatal_trace("unhandled operand combination in asm_sub");
   }
}

static void asm_mul(code_blob_t *blob, x86_operand_t src, x86_size_t size)
{
   assert(src.kind == X86_REG);
   asm_rex(blob, size, src.reg, 0, 0);
   __(0xf7, __MODRM(3, 4, src.reg));
}

static void asm_imul(code_blob_t *blob, x86_operand_t dst, x86_operand_t src,
                     x86_size_t size)
{
   assert(COMBINE(dst, src) == REG_REG);
   asm_rex(blob, size, dst.reg, src.reg, 0);
   __(0x0f, 0xaf, __MODRM(3, dst.reg, src.reg));
}

static void asm_idiv(code_blob_t *blob, x86_operand_t src, x86_size_t size)
{
   assert(src.kind == X86_REG);
   asm_rex(blob, size, src.reg, 0, 0);
   switch (size) {
   case __BYTE:
   case __WORD:
      __(0x66, 0xf7, __MODRM(3, 7, src.reg));
      break;
   case __DWORD:
   case __QWORD:
      __(0xf7, __MODRM(3, 7, src.reg));
      break;
   default:
      fatal_trace("unhandled size %d in asm_idiv", size);
   }
}

static void asm_and(code_blob_t *blob, x86_operand_t dst, x86_operand_t src,
                    x86_size_t size)
{
   switch (COMBINE(dst, src)) {
   case REG_IMM:
      asm_rex(blob, size, src.reg, dst.reg, 0);
      switch (size) {
      case __QWORD:
      case __DWORD:
         if (is_imm8(src.imm))
            __(0x83, __MODRM(3, 4, dst.reg), src.imm);
         else
            __(0x81, __MODRM(3, 4, dst.reg), __IMM32(src.imm));
         break;
      default:
         fatal_trace("unhandled size %d in asm_and", size);
      }
      break;

   case REG_REG:
      asm_rex(blob, size, src.reg, dst.reg, 0);
      switch (size) {
      case __QWORD:
      case __DWORD:
         __(0x21, __MODRM(3, src.reg, dst.reg));
         break;
      case __BYTE:
         __(0x20, __MODRM(3, src.reg, dst.reg));
         break;
      default:
         fatal_trace("unhandled size %d in asm_and", size);
      }
      break;

   default:
      fatal_trace("unhandled operand combination in asm_and");
   }
}

static void asm_or(code_blob_t *blob, x86_operand_t dst, x86_operand_t src,
                   x86_size_t size)
{
   assert(COMBINE(dst, src) == REG_REG);
   asm_rex(blob, size, src.reg, dst.reg, 0);
   switch (size) {
   case __QWORD:
   case __DWORD:
      __(0x09, __MODRM(3, src.reg, dst.reg));
      break;
   case __BYTE:
      __(0x08, __MODRM(3, src.reg, dst.reg));
      break;
   default:
      fatal_trace("unhandled size %d in asm_or", size);
   }
}

static void asm_shl(code_blob_t *blob, x86_operand_t src, x86_size_t size)
{
   assert(src.kind == X86_REG);
   asm_rex(blob, size, src.reg, 0, 0);
   __(0xd3, __MODRM(3, 4, src.reg));
}

static void asm_sar(code_blob_t *blob, x86_operand_t src, x86_size_t size)
{
   assert(src.kind == X86_REG);
   asm_rex(blob, size, src.reg, 0, 0);
   __(0xd3, __MODRM(3, 7, src.reg));
}

static void asm_test(code_blob_t *blob, x86_operand_t src1,
                     x86_operand_t src2, x86_size_t size)
{
   switch (COMBINE(src1, src2)) {
   case REG_IMM:
      asm_rex(blob, size, 0, src1.reg, 0);
      switch (size) {
      case __BYTE: __(0xf6, __MODRM(3, 0, src1.reg), src2.imm); break;
      default: __(0xf7, __MODRM(3, 0, src1.reg), __IMM32(src2.imm)); break;
      }
      break;

   case REG_REG:
      asm_rex(blob, size, 0, src1.reg, 0);
      switch (size) {
      case __BYTE: __(0x84, __MODRM(3, src2.reg, src1.reg)); break;
      default: __(0x85, __MODRM(3, src2.reg, src1.reg)); break;
      }
      break;

   default:
      fatal_trace("unhandled operand combination in asm_test");
   }
}

static void asm_cmp(code_blob_t *blob, x86_operand_t src1,
                    x86_operand_t src2, x86_size_t size)
{
   assert(COMBINE(src1, src2) == REG_REG);
   asm_rex(blob, size, src2.reg, src1.reg, 0);
   __(0x39, __MODRM(3, src2.reg, src1.reg));
}

static void asm_setcc(code_blob_t *blob, x86_operand_t dst, x86_cmp_t cmp)
{
   assert(dst.kind == X86_REG);
   asm_rex(blob, 1, 0, dst.reg, 0);
   __(0x0f, 0x90 + cmp, __MODRM(3, 0, dst.reg));
}

static void asm_call(code_blob_t *blob, x86_operand_t addr)
{
   switch (addr.kind) {
   case X86_IMM:
      {
         const ptrdiff_t rel = (uint8_t *)addr.imm - blob->wptr - 5;
         assert(is_imm32(rel));
         __(0xe8, __IMM32(rel));
      }
      break;
   case X86_REG:
      __(0xff, __MODRM(3, 2, addr.reg));
      break;
   default:
      fatal_trace("unhandled operand kind in asm_call");
   }
}

static void asm_jmp(code_blob_t *blob, x86_operand_t addr)
{
   assert(addr.kind == X86_PATCH);
   if (is_imm8(addr.imm))
      __(0xeb, 8);
   else
      __(0xe9, 0, 0, 0, 32);
}

static void asm_jcc(code_blob_t *blob, x86_operand_t addr, x86_cmp_t cmp)
{
   assert(addr.kind == X86_PATCH);
   if (is_imm8(addr.imm))
      __(0x70 + cmp, 8);
   else
      __(0x0f, 0x80 + cmp, 0, 0, 0, 32);
}

////////////////////////////////////////////////////////////////////////////////
// JIT IR to X86 assembly lowering

static void jit_x86_push_call_clobbered(code_blob_t *blob)
{
   PUSH(FPTR_REG);
   PUSH(ANCHOR_REG);
   PUSH(ARGS_REG);
   PUSH(TLAB_REG);
}

static void jit_x86_pop_call_clobbered(code_blob_t *blob)
{
   POP(TLAB_REG);
   POP(ARGS_REG);
   POP(ANCHOR_REG);
   POP(FPTR_REG);
}

static void jit_x86_get(code_blob_t *blob, x86_operand_t dst, jit_value_t src)
{
   switch (src.kind) {
   case JIT_VALUE_REG:
      MOV(dst, ADDR(__EBP, -FRAME_FIXED_SIZE - src.reg*8), __QWORD);
      break;
   case JIT_VALUE_INT64:
      MOV(dst, IMM(src.int64), __QWORD);
      break;
   case JIT_VALUE_TREE:
      MOV(dst, IMM((intptr_t)src.tree), __QWORD);
      break;
   case JIT_ADDR_REG:
      MOV(dst, ADDR(__EBP, -FRAME_FIXED_SIZE - src.reg*8), __QWORD);
      if (src.disp != 0)
         LEA(dst, ADDR(dst, src.disp));
      break;
   case JIT_ADDR_CPOOL:
      MOV(dst, IMM((intptr_t)blob->func->cpool + src.disp), __QWORD);
      break;
   default:
      fatal_trace("cannot handle value kind %d in jit_x86_get", src.kind);
   }
}

static x86_operand_t jit_x86_get_addr(code_blob_t *blob, jit_value_t addr,
                                      x86_operand_t tmp)
{
   switch (addr.kind) {
   case JIT_ADDR_REG:
      MOV(tmp, ADDR(__EBP, -FRAME_FIXED_SIZE - addr.reg*8), __QWORD);
      return ADDR(tmp, addr.disp);
   default:
      fatal_trace("cannot handle value kind %d in jit_x86_get_addr", addr.kind);
   }
}

static void jit_x86_patch(code_blob_t *blob, jit_label_t label, uint8_t *wptr,
                          const uint8_t *dest)
{
   const int size = *(wptr - 1);
   assert(size == 8 || size == 32);

   const ptrdiff_t rel = dest - wptr;
   if (unlikely(size == 8 && !is_imm8(rel)))
      fatal_trace("relative displacement %"PRIiPTR" does not fit "
                  "in 8-bit immediate", rel);

   if (size == 32) {
      *(wptr - 1) = (rel >> 24) & 0xff;
      *(wptr - 2) = (rel >> 16) & 0xff;
      *(wptr - 3) = (rel >> 8) & 0xff;
      *(wptr - 4) = rel & 0xff;
   }
   else
      *(wptr - 1) = rel;
}

static void jit_x86_put(code_blob_t *blob, jit_reg_t dst, x86_operand_t src)
{
   MOV(ADDR(__EBP, -FRAME_FIXED_SIZE - dst*8), src, __QWORD);
}

static void jit_x86_set_flags(code_blob_t *blob, jit_ir_t *ir)
{
   switch (ir->cc) {
   case JIT_CC_O: SETO(FLAGS_REG); break;
   case JIT_CC_C: SETC(FLAGS_REG); break;
   default: break;
   }
}

static void jit_x86_sext(code_blob_t *blob, x86_operand_t reg, x86_size_t size)
{
   assert(reg.reg == __EAX.reg);
   switch (size) {
   case __BYTE: MOVSX(reg, reg, __QWORD, __BYTE); break;
   case __WORD: CWDE(); // Fall-through
   case __DWORD: CDQE(); break;
   default: break;
   }
}

static x86_size_t jit_x86_size(jit_ir_t *ir)
{
   switch (ir->size) {
   case JIT_SZ_8: return __BYTE;
   case JIT_SZ_16: return __WORD;
   case JIT_SZ_32: return __DWORD;
   default: return __QWORD;
   }
}

static void jit_x86_recv(code_blob_t *blob, jit_ir_t *ir)
{
   assert(ir->arg1.kind == JIT_VALUE_INT64);
   const int nth = ir->arg1.int64;

   MOV(__EAX, ADDR(ARGS_REG, nth * sizeof(int64_t)), __QWORD);
   jit_x86_put(blob, ir->result, __EAX);
}

static void jit_x86_send(code_blob_t *blob, jit_ir_t *ir)
{
   assert(ir->arg1.kind == JIT_VALUE_INT64);
   const int nth = ir->arg1.int64;

   jit_x86_get(blob, __EAX, ir->arg2);
   MOV(ADDR(ARGS_REG, nth * sizeof(int64_t)), __EAX, __QWORD);
}

static void jit_x86_add(code_blob_t *blob, jit_ir_t *ir)
{
   jit_x86_get(blob, __EAX, ir->arg1);
   jit_x86_get(blob, __ECX, ir->arg2);

   const x86_size_t size = jit_x86_size(ir);

   ADD(__EAX, __ECX, size);

   jit_x86_set_flags(blob, ir);
   jit_x86_sext(blob, __EAX, size);
   jit_x86_put(blob, ir->result, __EAX);
}

static void jit_x86_sub(code_blob_t *blob, jit_ir_t *ir)
{
   jit_x86_get(blob, __EAX, ir->arg1);
   jit_x86_get(blob, __ECX, ir->arg2);

   const x86_size_t size = jit_x86_size(ir);

   SUB(__EAX, __ECX, size);

   jit_x86_set_flags(blob, ir);
   jit_x86_sext(blob, __EAX, size);
   jit_x86_put(blob, ir->result, __EAX);
}

static void jit_x86_mul(code_blob_t *blob, jit_ir_t *ir)
{
   jit_x86_get(blob, __EAX, ir->arg1);
   jit_x86_get(blob, __ECX, ir->arg2);

   const x86_size_t size = jit_x86_size(ir);

   if (ir->cc == JIT_CC_O)
      IMUL(__EAX, __ECX, size);
   else
      MUL(__ECX, size);

   jit_x86_set_flags(blob, ir);
   jit_x86_sext(blob, __EAX, size);
   jit_x86_put(blob, ir->result, __EAX);
}

static void jit_x86_rem(code_blob_t *blob, jit_ir_t *ir)
{
   jit_x86_get(blob, __EAX, ir->arg1);
   jit_x86_get(blob, __ECX, ir->arg2);

   const x86_size_t size = jit_x86_size(ir);

   switch (size) {
   case __WORD: CWD(); break;
   case __DWORD: CDQ(); break;
   default: CQO(); break;
   }

   IDIV(__ECX, size);
   MOV(__EAX, __EDX, MAX(size, __DWORD));

   jit_x86_sext(blob, __EAX, size);
   jit_x86_put(blob, ir->result, __EAX);
}

static void jit_x86_div(code_blob_t *blob, jit_ir_t *ir)
{
   jit_x86_get(blob, __EAX, ir->arg1);
   jit_x86_get(blob, __ECX, ir->arg2);

   const x86_size_t size = jit_x86_size(ir);

   switch (size) {
   case __WORD: CWD(); break;
   case __DWORD: CDQ(); break;
   default: CQO(); break;
   }

   IDIV(__ECX, size);

   jit_x86_sext(blob, __EAX, size);
   jit_x86_put(blob, ir->result, __EAX);
}

static void jit_x86_neg(code_blob_t *blob, jit_ir_t *ir)
{
   jit_x86_get(blob, __EAX, ir->arg1);

   NEG(__EAX, __QWORD);

   jit_x86_put(blob, ir->result, __EAX);
}

static void jit_x86_not(code_blob_t *blob, jit_ir_t *ir)
{
   jit_x86_get(blob, __ECX, ir->arg1);

   XOR(__EAX, __EAX, __DWORD);
   TEST(__ECX, __ECX, __BYTE);
   SETZ(__EAX);

   jit_x86_put(blob, ir->result, __EAX);
}

static void jit_x86_and(code_blob_t *blob, jit_ir_t *ir)
{
   jit_x86_get(blob, __EAX, ir->arg1);
   jit_x86_get(blob, __ECX, ir->arg2);

   AND(__EAX, __ECX, __BYTE);

   jit_x86_put(blob, ir->result, __EAX);
}

static void jit_x86_or(code_blob_t *blob, jit_ir_t *ir)
{
   jit_x86_get(blob, __EAX, ir->arg1);
   jit_x86_get(blob, __ECX, ir->arg2);

   OR(__EAX, __ECX, __BYTE);

   jit_x86_put(blob, ir->result, __EAX);
}

static void jit_x86_mov(code_blob_t *blob, jit_ir_t *ir)
{
   jit_x86_get(blob, __EAX, ir->arg1);
   jit_x86_put(blob, ir->result, __EAX);
}

static void jit_x86_jump(code_blob_t *blob, jit_ir_t *ir)
{
   const int distance = (ir - blob->func->irbuf) * BYTES_PER_IR;

   if (ir->cc == JIT_CC_NONE)
      JMP(PATCH(distance));
   else if (ir->cc == JIT_CC_T) {
      TEST(FLAGS_REG, IMM(1), __BYTE);
      JNZ(PATCH(distance));
   }
   else if (ir->cc == JIT_CC_F) {
      TEST(FLAGS_REG, IMM(1), __BYTE);
      JZ(PATCH(distance));
   }
   else
      fatal_trace("invalid JUMP condition code");

   code_blob_patch(blob, ir->arg1.label, jit_x86_patch);
}

static void jit_x86_ret(code_blob_t *blob, jit_ir_t *ir)
{
   MOV(__EBX, ADDR(__EBP, -8), __QWORD);
   LEAVE();
   RET();
}

static void jit_x86_load(code_blob_t *blob, jit_ir_t *ir)
{
   x86_operand_t addr = jit_x86_get_addr(blob, ir->arg1, __ECX);
   MOVSX(__EAX, addr, __QWORD, jit_x86_size(ir));

   jit_x86_put(blob, ir->result, __EAX);
}

static void jit_x86_uload(code_blob_t *blob, jit_ir_t *ir)
{
   x86_operand_t addr = jit_x86_get_addr(blob, ir->arg1, __ECX);
   MOVZX(__EAX, addr, __QWORD, jit_x86_size(ir));

   jit_x86_put(blob, ir->result, __EAX);
}

static void jit_x86_store(code_blob_t *blob, jit_ir_t *ir)
{
   jit_x86_get(blob, __EAX, ir->arg1);
   x86_operand_t addr = jit_x86_get_addr(blob, ir->arg2, __ECX);
   MOV(addr, __EAX, jit_x86_size(ir));
}

static void jit_x86_lea(code_blob_t *blob, jit_ir_t *ir)
{
   jit_x86_get(blob, __EAX, ir->arg1);
   jit_x86_put(blob, ir->result, __EAX);
}

static void jit_x86_cmp(code_blob_t *blob, jit_ir_t *ir)
{
   jit_x86_get(blob, __EAX, ir->arg1);
   jit_x86_get(blob, __ECX, ir->arg2);

   CMP(__EAX, __ECX, __QWORD);

   switch (ir->cc) {
   case JIT_CC_EQ:
      SETZ(FLAGS_REG);
      break;
   case JIT_CC_NE:
      SETNZ(FLAGS_REG);
      break;
   case JIT_CC_LT:
      SETLT(FLAGS_REG);
      break;
   case JIT_CC_GT:
      SETGT(FLAGS_REG);
      break;
   case JIT_CC_LE:
      SETLE(FLAGS_REG);
      break;
   case JIT_CC_GE:
      SETGE(FLAGS_REG);
      break;
   default:
      fatal_trace("unhandled JIT comparison code %d", ir->cc);
   }
}

static void jit_x86_cset(code_blob_t *blob, jit_ir_t *ir)
{
   jit_x86_put(blob, ir->result, FLAGS_REG);
}

static void jit_x86_csel(code_blob_t *blob, jit_ir_t *ir)
{
   jit_x86_get(blob, __EAX, ir->arg1);
   jit_x86_get(blob, __ECX, ir->arg2);

   TEST(FLAGS_REG, FLAGS_REG, __BYTE);
   CMOVZ(__EAX, __ECX, __QWORD);

   jit_x86_put(blob, ir->result, __EAX);
}

static void jit_x86_call(code_blob_t *blob, jit_x86_state_t *state,
                         jit_ir_t *ir)
{
   jit_func_t *f = jit_get_func(state->jit, ir->arg1.handle);

   MOV(__EAX, PTR(f), __QWORD);
   CALL(PTR(state->stubs[CALL_STUB]));
}

static void jit_x86_shl(code_blob_t *blob, jit_ir_t *ir)
{
   jit_x86_get(blob, __EAX, ir->arg1);
   jit_x86_get(blob, __ECX, ir->arg2);

   SHL(__EAX, __QWORD);

   jit_x86_put(blob, ir->result, __EAX);
}

static void jit_x86_asr(code_blob_t *blob, jit_ir_t *ir)
{
   jit_x86_get(blob, __EAX, ir->arg1);
   jit_x86_get(blob, __ECX, ir->arg2);

   SAR(__EAX, __QWORD);

   jit_x86_put(blob, ir->result, __EAX);
}

static void jit_x86_macro_exit(code_blob_t *blob, jit_x86_state_t *state,
                               jit_ir_t *ir)
{
   MOV(__EAX, IMM(ir->arg1.exit), __DWORD);
   CALL(PTR(state->stubs[EXIT_STUB]));

#ifdef DEBUG
   if (jit_will_abort(ir))
      INT3();
#endif
}

static void jit_x86_macro_salloc(code_blob_t *blob, jit_ir_t *ir)
{
   assert(ir->arg1.int64 + ir->arg2.int64 <= blob->func->framesz);

   const ptrdiff_t locals =
      FRAME_FIXED_SIZE + blob->func->nregs*8 + blob->func->framesz;

   LEA(__EAX, ADDR(__EBP, -locals + ir->arg1.int64));

   jit_x86_put(blob, ir->result, __EAX);
}

static void jit_x86_macro_lalloc(code_blob_t *blob, jit_x86_state_t *state,
                                 jit_ir_t *ir)
{
   jit_x86_get(blob, __EAX, ir->arg1);

   CALL(PTR(state->stubs[ALLOC_STUB]));

   jit_x86_put(blob, ir->result, __EAX);
}

static void jit_x86_macro_bzero(code_blob_t *blob, jit_ir_t *ir)
{
   jit_x86_get(blob, __EDI, ir->arg1);   // Clobbers FPTR_REG

   XOR(__EAX, __EAX, __DWORD);
   MOV(__ECX, ADDR(__EBP, -FRAME_FIXED_SIZE - ir->result*8), __QWORD);
   __(0xf3, 0x48, 0xaa);   // REP STOS
}

static void jit_x86_macro_copy(code_blob_t *blob, jit_ir_t *ir)
{
   PUSH(__ESI);

   jit_x86_get(blob, __EDI, ir->arg1);   // Clobbers FPTR_REG
   jit_x86_get(blob, __ESI, ir->arg2);   // Clobbers ANCHOR_REG

   MOV(__ECX, ADDR(__EBP, -FRAME_FIXED_SIZE - ir->result*8), __QWORD);
   __(0xf3, 0x48, 0xa4);   // REP MOVS

   POP(__ESI);
}

static void jit_x86_macro_getpriv(code_blob_t *blob, jit_ir_t *ir)
{
   jit_func_t *f = jit_get_func(blob->func->jit, ir->arg1.handle);
   void **ptr = jit_get_privdata_ptr(blob->func->jit, f);

   MOV(__EAX, IMM((intptr_t)ptr), __QWORD);
   MOV(__EAX, ADDR(__EAX, 0), __QWORD);
}

static void jit_x86_op(code_blob_t *blob, jit_x86_state_t *state, jit_ir_t *ir)
{
   switch (ir->op) {
   case J_RECV:
      jit_x86_recv(blob, ir);
      break;
   case J_SEND:
      jit_x86_send(blob, ir);
      break;
   case J_ADD:
      jit_x86_add(blob, ir);
      break;
   case J_SUB:
      jit_x86_sub(blob, ir);
      break;
   case J_MUL:
      jit_x86_mul(blob, ir);
      break;
   case J_REM:
      jit_x86_rem(blob, ir);
      break;
   case J_DIV:
      jit_x86_div(blob, ir);
      break;
   case J_NEG:
      jit_x86_neg(blob, ir);
      break;
   case J_MOV:
      jit_x86_mov(blob, ir);
      break;
   case J_NOT:
      jit_x86_not(blob, ir);
      break;
   case J_AND:
      jit_x86_and(blob, ir);
      break;
   case J_OR:
      jit_x86_or(blob, ir);
      break;
   case J_DEBUG:
   case J_NOP:
      break;
   case J_JUMP:
      jit_x86_jump(blob, ir);
      break;
   case J_RET:
      jit_x86_ret(blob, ir);
      break;
   case J_LOAD:
      jit_x86_load(blob, ir);
      break;
   case J_ULOAD:
      jit_x86_uload(blob, ir);
      break;
   case J_STORE:
      jit_x86_store(blob, ir);
      break;
   case J_LEA:
      jit_x86_lea(blob, ir);
      break;
   case J_CMP:
      jit_x86_cmp(blob, ir);
      break;
   case J_CSET:
      jit_x86_cset(blob, ir);
      break;
   case J_CSEL:
      jit_x86_csel(blob, ir);
      break;
   case J_CALL:
      jit_x86_call(blob, state, ir);
      break;
   case J_SHL:
      jit_x86_shl(blob, ir);
      break;
   case J_ASR:
      jit_x86_asr(blob, ir);
      break;
   case MACRO_EXIT:
      jit_x86_macro_exit(blob, state, ir);
      break;
   case MACRO_SALLOC:
      jit_x86_macro_salloc(blob, ir);
      break;
   case MACRO_LALLOC:
      jit_x86_macro_lalloc(blob, state, ir);
      break;
   case MACRO_BZERO:
      jit_x86_macro_bzero(blob, ir);
      break;
   case MACRO_COPY:
      jit_x86_macro_copy(blob, ir);
      break;
   case MACRO_GETPRIV:
      jit_x86_macro_getpriv(blob, ir);
      break;
   default:
      jit_dump_with_mark(blob->func, ir - blob->func->irbuf, false);
      fatal_trace("unhandled opcode %s in x86 backend", jit_op_name(ir->op));
   }
}

static void jit_x86_cgen(jit_t *j, jit_handle_t handle, void *context)
{
   jit_x86_state_t *state = context;

   jit_func_t *f = jit_get_func(j, handle);

   code_blob_t *blob = code_blob_new(state->code, f->name, f);

   PUSH(__EBP);
   MOV(__EBP, __ESP, __QWORD);
   PUSH(__EBX);

   // Shuffle incoming arguments
#ifdef __MINGW32__
   MOV(FPTR_REG, __ECX, __QWORD);
   MOV(ANCHOR_REG, __EDX, __QWORD);
   MOV(ARGS_REG, __R8, __QWORD);
   MOV(TLAB_REG, __R9, __QWORD);
#else
   MOV(FPTR_REG, __EDI, __QWORD);
   MOV(ANCHOR_REG, __ESI, __QWORD);
   MOV(ARGS_REG, __EDX, __QWORD);
   MOV(TLAB_REG, __ECX, __QWORD);
#endif

   XOR(FLAGS_REG, FLAGS_REG, __DWORD);

   // Frame layout
   //
   //       |-------------------|
   //    +8 | Caller's PC       |
   //       |-------------------|
   //     0 | Saved RBP         |    <--- RBP
   //       |-------------------|
   //    -8 | Saved RBX         |
   //       |-------------------|
   //   -12 | Padding           |
   //       |-------------------|
   //   -16 | IR position       |
   //   -24 | Function pointer  |
   //   -32 | Caller's anchor   |    <--- Frame anchor
   //       |-------------------|
   //   -40 | Local registers   |
   //       .                   .
   //       |-------------------|
   //       | Local variables   |
   //       .                   .
   //       |-------------------|

   const size_t framebytes =
      f->framesz + f->nregs * sizeof(int64_t) + FRAME_FIXED_SIZE;
   const size_t framesz = ALIGN_UP(framebytes, 16) - 8 /* RBX pushed above */;
   SUB(__ESP, IMM(framesz), __QWORD);

   // Build frame anchor
   MOV(ADDR(__EBP, -32), ANCHOR_REG, __QWORD);
   MOV(ADDR(__EBP, -24), FPTR_REG, __QWORD);
   MOV(ADDR(__EBP, -16), FLAGS_REG, __DWORD);

   LEA(ANCHOR_REG, ADDR(__EBP, -32));

   for (int i = 0; i < f->nirs; i++) {
      if (f->irbuf[i].target)
         code_blob_mark(blob, i);
      jit_x86_op(blob, state, &(f->irbuf[i]));
   }

   code_blob_finalise(blob, &(f->entry));
}

static void jit_x86_gen_exit_stub(jit_x86_state_t *state)
{
   ident_t name = ident_new("exit stub");
   code_blob_t *blob = code_blob_new(state->code, name, NULL);

   PUSH(__EBP);
   MOV(__EBP, __ESP, __QWORD);

   jit_x86_push_call_clobbered(blob);

   // Exit number in EAX, clobbers FPTR
#ifdef __MINGW32__
   MOV(__ECX, __EAX, __DWORD);
   MOV(__EDX, ANCHOR_REG, __QWORD);
   MOV(__R8, ARGS_REG, __QWORD);
   MOV(__R9, TLAB_REG, __QWORD);
#else
   MOV(__EDI, __EAX, __DWORD);
   MOV(__ESI, ANCHOR_REG, __QWORD);
   MOV(__EDX, ARGS_REG, __QWORD);
   MOV(__ECX, TLAB_REG, __QWORD);
#endif

   MOV(__EAX, PTR(__nvc_do_exit), __QWORD);
   CALL(__EAX);

   jit_x86_pop_call_clobbered(blob);

   LEAVE();
   RET();

   code_blob_finalise(blob, &(state->stubs[EXIT_STUB]));
}

static void jit_x86_gen_call_stub(jit_x86_state_t *state)
{
   ident_t name = ident_new("call stub");
   code_blob_t *blob = code_blob_new(state->code, name, NULL);

   PUSH(__EBP);
   MOV(__EBP, __ESP, __QWORD);

   jit_x86_push_call_clobbered(blob);

#ifdef __MINGW32__
   MOV(__ECX, FPTR_REG, __QWORD);
   MOV(__EDX, ANCHOR_REG, __QWORD);
   MOV(__R8, ARGS_REG, __QWORD);
   MOV(__R9, TLAB_REG, __QWORD);
#else
   MOV(__EDI, FPTR_REG, __QWORD);
   MOV(__ESI, ANCHOR_REG, __QWORD);
   MOV(__EDX, ARGS_REG, __QWORD);
   MOV(__ECX, TLAB_REG, __QWORD);
#endif

   MOV(FPTR_REG, __EAX, __QWORD);
   MOV(__EAX, ADDR(__EAX, offsetof(jit_func_t, entry)), __QWORD);
   CALL(__EAX);

   jit_x86_pop_call_clobbered(blob);

   LEAVE();
   RET();

   code_blob_finalise(blob, &(state->stubs[CALL_STUB]));
}

static void jit_x86_gen_alloc_stub(jit_x86_state_t *state)
{
   ident_t name = ident_new("alloc stub");
   code_blob_t *blob = code_blob_new(state->code, name, NULL);

   PUSH(__EBP);
   MOV(__EBP, __ESP, __QWORD);

   jit_x86_push_call_clobbered(blob);

   // Size in EAX, clobbers FPTR
#ifdef __MINGW32__
   MOV(__ECX, __EAX, __DWORD);
   MOV(__EDX, ANCHOR_REG, __QWORD);
#else
   MOV(__EDI, __EAX, __DWORD);
   MOV(__ESI, ANCHOR_REG, __QWORD);
#endif

   MOV(__EAX, PTR(__nvc_mspace_alloc2), __QWORD);
   CALL(__EAX);

   jit_x86_pop_call_clobbered(blob);

   LEAVE();
   RET();

   code_blob_finalise(blob, &(state->stubs[ALLOC_STUB]));
}

static void *jit_x86_init(jit_t *jit)
{
   jit_x86_state_t *state = xcalloc(sizeof(jit_x86_state_t));
   state->jit  = jit;
   state->code = code_cache_new();

   jit_x86_gen_exit_stub(state);
   jit_x86_gen_call_stub(state);
   jit_x86_gen_alloc_stub(state);

   return state;
}

static void jit_x86_cleanup(void *context)
{
   jit_x86_state_t *state = context;

   code_cache_free(state->code);
   free(state);
}

static const jit_plugin_t jit_x86 = {
   .init    = jit_x86_init,
   .cgen    = jit_x86_cgen,
   .cleanup = jit_x86_cleanup
};

void jit_register_native_plugin(jit_t *j)
{
   const int threshold = opt_get_int(OPT_JIT_THRESHOLD);
   if (threshold > 0)
      jit_add_tier(j, threshold, &jit_x86);
   else if (threshold < 0)
      warnf("invalid NVC_JIT_THRESOLD setting %d", threshold);
}
