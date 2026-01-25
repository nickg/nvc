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
#include "ident.h"
#include "option.h"
#include "jit/jit-priv.h"
#include "jit/jit.h"
#include "rt/rt.h"

#include <assert.h>
#include <inttypes.h>
#include <math.h>
#include <string.h>
#include <stdlib.h>
#include <smmintrin.h>

typedef enum {
   EXIT_STUB,
   CALL_STUB,
   ALLOC_STUB,
   FFI_STUB,
   DEBUG_STUB,
   TLAB_STUB,
   FEXP_STUB,
   ROUND_STUB,

   NUM_STUBS
} jit_x86_stub_t;

typedef struct {
   jit_t          *jit;
   code_cache_t   *code;
   jit_entry_fn_t  stubs[NUM_STUBS];
} jit_x86_state_t;

// Conservative guess at the number of bytes emitted per IR
#define BYTES_PER_IR 16

#define FRAME_FIXED_SIZE 80    // Size of fixed part of call frame
#define ANCHOR_OFFSET    -24   // Offset of frame anchor from RBP

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
   X86_XMM,
   X86_ADDR2,
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
      struct {
         x86_reg_t reg1;
         x86_reg_t reg2;
         int32_t   off;
      } addr2;
   };
} x86_operand_t;

STATIC_ASSERT(sizeof(x86_operand_t) == 16);

#define REG(x) ((x86_operand_t){ X86_REG, { .reg = (x) }})
#define XMM(x) ((x86_operand_t){ X86_XMM, { .reg = (x) }})
#define IMM(x) ((x86_operand_t){ X86_IMM, { .imm = (x) }})
#define PTR(x) ((x86_operand_t){ X86_IMM, { .imm = (uintptr_t)(x) }})
#define ADDR(r, o) ((x86_operand_t){ X86_ADDR, { .addr = { (r).reg, (o) }}})
#define ADDR2(r1, r2, o) \
   ((x86_operand_t){ X86_ADDR2, { .addr2 = { (r1).reg, (r2).reg, (o) }}})
#define PATCH(n) ((x86_operand_t){ X86_PATCH, { .imm = (n) }})

#define COMBINE(a, b) (((a).kind << 4) | (b).kind)
#define REG_REG ((X86_REG << 4) | X86_REG)
#define MEM_REG ((X86_ADDR << 4) | X86_REG)
#define MEM_IMM ((X86_ADDR << 4) | X86_IMM)
#define REG_MEM ((X86_REG << 4) | X86_ADDR)
#define REG_IMM ((X86_REG << 4) | X86_IMM)
#define XMM_MEM ((X86_XMM << 4) | X86_ADDR)
#define MEM_XMM ((X86_ADDR << 4) | X86_XMM)
#define REG_XMM ((X86_REG << 4) | X86_XMM)
#define XMM_REG ((X86_XMM << 4) | X86_REG)
#define XMM_XMM ((X86_XMM << 4) | X86_XMM)
#define REG_MEM2 ((X86_REG << 4) | X86_ADDR2)

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
static const x86_operand_t __R10 = REG(18);
static const x86_operand_t __R11 = REG(19);
#if 0
static const x86_operand_t __R12 = REG(20);
static const x86_operand_t __R13 = REG(21);
static const x86_operand_t __R14 = REG(22);
static const x86_operand_t __R15 = REG(23);
#endif

static const x86_operand_t __XMM0 = XMM(0);
static const x86_operand_t __XMM1 = XMM(1);

#define ARGS_REG   __R8
#define TLAB_REG   __R9
#define FLAGS_REG  __EBX

#ifdef __MINGW32__
#define CARG0_REG __ECX
#define CARG1_REG __EDX
#define CARG2_REG __R8
#define CARG3_REG __R9
#else
#define CARG0_REG __EDI
#define CARG1_REG __ESI
#define CARG2_REG __EDX
#define CARG3_REG __ECX
#endif

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
   X86_CMP_BE = 0x06,
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
#define STD() __(0xfd)
#define CLD() __(0xfc)
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
#define SAR(src, count, size) asm_sar(blob, (src), (count), (size))
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
#define CMOVGT(dst, src, size)                          \
   asm_cmovcc(blob, (dst), (src), (size), X86_CMP_GT)
#define CMOVLT(dst, src, size)                          \
   asm_cmovcc(blob, (dst), (src), (size), X86_CMP_LT)
#define LEA(dst, addr) asm_lea(blob, (dst), (addr))
#define SETO(dst) asm_setcc(blob, (dst), X86_CMP_O)
#define SETC(dst) asm_setcc(blob, (dst), X86_CMP_C)
#define SETZ(dst) asm_setcc(blob, (dst), X86_CMP_EQ)
#define SETNZ(dst) asm_setcc(blob, (dst), X86_CMP_NE)
#define SETLT(dst) asm_setcc(blob, (dst), X86_CMP_LT)
#define SETGT(dst) asm_setcc(blob, (dst), X86_CMP_GT)
#define SETGE(dst) asm_setcc(blob, (dst), X86_CMP_GE)
#define SETLE(dst) asm_setcc(blob, (dst), X86_CMP_LE)
#define SETA(dst) asm_setcc(blob, (dst), 0x7)
#define SETAE(dst) asm_setcc(blob, (dst), 0x3)
#define SETB(dst) asm_setcc(blob, (dst), 0x2)
#define SETBE(dst) asm_setcc(blob, (dst), 0x6)
#define TEST(src1, src2, size) asm_test(blob, (src1), (src2), (size))
#define CMP(src1, src2, size) asm_cmp(blob, (src1), (src2), (size))
#define CALL(addr) asm_call(blob, (addr))
#define JMP(addr) asm_jmp(blob, (addr))
#define JZ(addr) asm_jcc(blob, (addr), X86_CMP_EQ)
#define JNZ(addr) asm_jcc(blob, (addr), X86_CMP_NE)
#define JLT(addr) asm_jcc(blob, (addr), X86_CMP_LT)
#define JB(addr) asm_jcc(blob, (addr), X86_CMP_C)
#define JBE(addr) asm_jcc(blob, (addr), X86_CMP_BE)
#define MULSD(dst, src) asm_mulsd(blob, (dst), (src))
#define DIVSD(dst, src) asm_divsd(blob, (dst), (src))
#define ADDSD(dst, src) asm_addsd(blob, (dst), (src))
#define SUBSD(dst, src) asm_subsd(blob, (dst), (src))
#define PXOR(dst, src) asm_pxor(blob, (dst), (src))
#define UCOMISD(src1, src2) asm_ucomisd(blob, (src1), (src2))
#define CVTSD2SI(dst, src, size) asm_cvtsd2si(blob, (dst), (src), (size))
#define CVTSI2SD(dst, src, size) asm_cvtsi2sd(blob, (dst), (src), (size))
#define ROUNDSD(dst, src, mode) asm_roundsd(blob, (dst), (src), (mode))
#define REPSTOS(size) asm_repstos(blob, (size))
#define REPMOVS(size) asm_repmovs(blob, (size))

#define is_imm8(x) ((x) >= INT8_MIN && (x) <= INT8_MAX)
#define is_imm16(x) ((x) >= INT16_MIN && (x) <= INT16_MAX)
#define is_imm32(x) ((x) >= INT32_MIN && (x) <= INT32_MAX)

typedef struct {
   uint8_t len;
   uint8_t bytes[15];
} x86_insn_t;

__attribute__((always_inline))
static inline void x86_prefix(x86_insn_t *insn, uint8_t b0)
{
   insn->bytes[insn->len++] = b0;
}

__attribute__((always_inline))
static inline void x86_opcode(x86_insn_t *insn, uint8_t b0)
{
   insn->bytes[insn->len++] = b0;
}

__attribute__((always_inline))
static inline void x86_opcode_2(x86_insn_t *insn, uint8_t b0, uint8_t b1)
{
   insn->bytes[insn->len++] = b0;
   insn->bytes[insn->len++] = b1;
}

__attribute__((always_inline))
static inline void x86_opcode_3(x86_insn_t *insn, uint8_t b0, uint8_t b1,
                                uint8_t b2)
{
   insn->bytes[insn->len++] = b0;
   insn->bytes[insn->len++] = b1;
   insn->bytes[insn->len++] = b2;
}

__attribute__((always_inline))
static inline void x86_imm32(x86_insn_t *insn, int64_t imm)
{
   assert(is_imm32(imm));
   insn->bytes[insn->len++] = imm & 0xff;
   insn->bytes[insn->len++] = (imm >> 8) & 0xff;
   insn->bytes[insn->len++] = (imm >> 16) & 0xff;
   insn->bytes[insn->len++] = (imm >> 24) & 0xff;
}

static inline void x86_imm64(x86_insn_t *insn, int64_t imm)
{
   insn->bytes[insn->len++] = imm & 0xff;
   insn->bytes[insn->len++] = (imm >> 8) & 0xff;
   insn->bytes[insn->len++] = (imm >> 16) & 0xff;
   insn->bytes[insn->len++] = (imm >> 24) & 0xff;
   insn->bytes[insn->len++] = (imm >> 32) & 0xff;
   insn->bytes[insn->len++] = (imm >> 40) & 0xff;
   insn->bytes[insn->len++] = (imm >> 48) & 0xff;
   insn->bytes[insn->len++] = (imm >> 56) & 0xff;
}

__attribute__((always_inline))
static inline void x86_imm8(x86_insn_t *insn, int64_t imm)
{
   assert(is_imm8(imm));
   insn->bytes[insn->len++] = imm & 0xff;
}

static void x86_rex(x86_insn_t *insn, x86_size_t size, x86_reg_t modrm_reg,
                    x86_reg_t modrm_rm, x86_reg_t modrm_sib)
{
   const bool want_prefix =
      // Quadword operation
      size > __DWORD ||
      // Uses R8 - R15
      (modrm_reg & 0x10) || (modrm_sib & 0x10) || (modrm_rm & 0x10) ||
      // Byte operation on SPL, BPL, SIL, DIL
      (size == __BYTE && ((modrm_rm & 7) >= 4 || ((modrm_reg & 7) >= 4)));

   if (want_prefix) {
      const int xsz = size > __DWORD;
      const int xr = !!(modrm_reg & 0x10);
      const int xsib = !!(modrm_sib & 0x10);
      const int xrm = !!(modrm_rm & 0x10);
      x86_prefix(insn, 0x40 | (xsz << 3) | (xr << 2) | (xsib << 1) | xrm);
   }
}

static inline void x86_modrm(x86_insn_t *insn, int m, int r, int rm)
{
   insn->bytes[insn->len++] = ((m & 3) << 6) | ((r & 7) << 3) | (rm & 7);
}

static inline void x86_sib(x86_insn_t *insn, int s, int i, int b)
{
   insn->bytes[insn->len++] = ((s & 3) << 6) | ((i & 7) << 3) | (b & 7);
}

static inline void x86_override(x86_insn_t *insn, bool cond)
{
   if (cond)
      x86_prefix(insn, 0x66);
}

static inline void x86_emit(code_blob_t *blob, const x86_insn_t *insn)
{
   assert(insn->len <= sizeof(insn->bytes));
   code_blob_emit(blob, insn->bytes, insn->len);
}

static void asm_xor(code_blob_t *blob, x86_operand_t dst, x86_operand_t src,
                    x86_size_t size)
{
   x86_insn_t insn = {};

   assert(COMBINE(dst, src) == REG_REG);
   x86_rex(&insn, size, dst.reg, src.reg, 0);
   x86_opcode(&insn, 0x33);
   x86_modrm(&insn, 3, dst.reg, src.reg);

   x86_emit(blob, &insn);
}

static void asm_neg(code_blob_t *blob, x86_operand_t dst, x86_size_t size)
{
   x86_insn_t insn = {};

   assert(dst.kind == X86_REG);
   x86_rex(&insn, size, 0, dst.reg, 0);
   x86_opcode(&insn, 0xf7);
   x86_modrm(&insn, 3, 3, dst.reg);

   x86_emit(blob, &insn);
}

static void asm_lea(code_blob_t *blob, x86_operand_t dst, x86_operand_t src)
{
   x86_insn_t insn = {};

   switch (COMBINE(dst, src)) {
   case REG_MEM:
      x86_rex(&insn, __QWORD, dst.reg, src.addr.reg, 0);
      x86_opcode(&insn, 0x8d);
      if (is_imm8(src.addr.off)) {
         x86_modrm(&insn, 1, dst.reg, src.addr.reg);
         x86_imm8(&insn, src.addr.off);
      }
      else {
         x86_modrm(&insn, 2, dst.reg, src.addr.reg);
         x86_imm32(&insn, src.addr.off);
      }
      break;

   case REG_MEM2:
      x86_rex(&insn, __QWORD, dst.reg, src.addr2.reg1, src.addr2.reg2);
      x86_opcode(&insn, 0x8d);
      if (src.addr2.off == 0) {
         x86_modrm(&insn, 0, dst.reg, 4);
         x86_sib(&insn, 0, src.addr2.reg2, src.addr2.reg1);
      }
      else if (is_imm8(src.addr2.off)) {
         x86_modrm(&insn, 1, dst.reg, 4);
         x86_sib(&insn, 0, src.addr2.reg2, src.addr2.reg1);
         x86_imm8(&insn, src.addr2.off);
      }
      else
         fatal_trace("immediate too big for LEA instruction");
      break;

   default:
      fatal_trace("unhandled operand combination in asm_lea");
   }

   x86_emit(blob, &insn);
}

static void asm_mov(code_blob_t *blob, x86_operand_t dst, x86_operand_t src,
                    x86_size_t size)
{
   x86_insn_t insn = {};

   switch (COMBINE(dst, src)) {
   case REG_REG:
      if (dst.reg != src.reg) {
         x86_rex(&insn, size, dst.reg, src.reg, 0);
         x86_opcode(&insn, 0x8b);
         x86_modrm(&insn, 3, dst.reg, src.reg);
      }
      break;

   case MEM_REG:
      x86_rex(&insn, size, src.reg, dst.addr.reg, 0);
      x86_override(&insn, size == __WORD);
      x86_opcode(&insn, size == __BYTE ? 0x88 : 0x89);
      if (is_imm8(dst.addr.off)) {
         x86_modrm(&insn, 1, src.reg, dst.addr.reg);
         x86_imm8(&insn, dst.addr.off);
      }
      else {
         x86_modrm(&insn, 2, src.reg, dst.addr.reg);
         x86_imm32(&insn, dst.addr.off);
      }
      break;

   case REG_MEM:
      x86_rex(&insn, size, dst.reg, src.addr.reg, 0);
      x86_override(&insn, size == __WORD);
      x86_opcode(&insn, size == __BYTE ? 0x8a : 0x8b);
      if (is_imm8(src.addr.off)) {
         x86_modrm(&insn, 1, dst.reg, src.addr.reg);
         x86_imm8(&insn, src.addr.off);
      }
      else {
         x86_modrm(&insn, 2, dst.reg, src.addr.reg);
         x86_imm32(&insn, src.addr.off);
      }
      break;

   case XMM_MEM:
      assert(size == __QWORD);
      x86_override(&insn, true);
      x86_rex(&insn, size, dst.reg, src.addr.reg, 0);
      x86_opcode_2(&insn, 0x0f, 0x6e);
      if (is_imm8(src.addr.off)) {
         x86_modrm(&insn, 1, dst.reg, src.addr.reg);
         x86_imm8(&insn, src.addr.off);
      }
      else {
         x86_modrm(&insn, 2, dst.reg, src.addr.reg);
         x86_imm32(&insn, src.addr.off);
      }
      break;

   case MEM_XMM:
      assert(size == __QWORD);
      x86_override(&insn, true);
      x86_rex(&insn, size, src.addr.reg, dst.reg, 0);
      x86_opcode_2(&insn, 0x0f, 0x7e);
      if (is_imm8(dst.addr.off)) {
         x86_modrm(&insn, 1, src.reg, dst.addr.reg);
         x86_imm8(&insn, dst.addr.off);
      }
      else {
         x86_modrm(&insn, 2, src.reg, dst.addr.reg);
         x86_imm32(&insn, dst.addr.off);
      }
      break;

   case XMM_REG:
      assert(size == __QWORD);
      x86_override(&insn, true);
      x86_rex(&insn, size, dst.reg, src.reg, 0);
      x86_opcode_2(&insn, 0x0f, 0x6e);
      x86_modrm(&insn, 3, dst.reg, src.reg);
      break;

   case REG_XMM:
      assert(size == __QWORD);
      x86_override(&insn, true);
      x86_rex(&insn, size, src.reg, dst.reg, 0);
      x86_opcode_2(&insn, 0x0f, 0x7e);
      x86_modrm(&insn, 3, src.reg, dst.reg);
      break;

   case REG_IMM:
      if (src.imm == 0)
         XOR(dst, dst, MIN(size, __DWORD));
      else {
         switch (size) {
         case __QWORD:
            if (is_imm32(src.imm)) {
               if (src.imm > 0) {
                  x86_rex(&insn, __DWORD, 0, dst.reg, 0);
                  x86_opcode(&insn, 0xb8 + (dst.reg & 7));
               }
               else {
                  x86_rex(&insn, __QWORD, 0, dst.reg, 0);
                  x86_opcode(&insn, 0xc7);
                  x86_modrm(&insn, 3, 0, dst.reg);
               }
               x86_imm32(&insn, src.imm);
            }
            else {
               x86_rex(&insn, __QWORD, 0, dst.reg, 0);
               x86_opcode(&insn, 0xb8 + (dst.reg & 7));
               x86_imm64(&insn, src.imm);
            }
            break;
         case __DWORD:
            x86_rex(&insn, __DWORD, 0, dst.reg, 0);
            x86_opcode(&insn, 0xb8 + (dst.reg & 7));
            x86_imm32(&insn, src.imm);
            break;
         default:
            fatal_trace("unhandled immediate size %d in asm_mov", size);
         }
      }
      break;

   case MEM_IMM:
      switch (size) {
      case __QWORD:
         assert(is_imm32(src.imm));
         x86_rex(&insn, size, 0, dst.addr.reg, 0);
         x86_opcode(&insn, 0xc7);
         if (is_imm8(dst.addr.off)) {
            x86_modrm(&insn, 1, 0, dst.addr.reg);
            x86_imm8(&insn, dst.addr.off);
         }
         else {
            x86_modrm(&insn, 2, 0, dst.addr.reg);
            x86_imm32(&insn, dst.addr.off);
         }
         x86_imm32(&insn, src.imm);
         break;
      default:
         fatal_trace("unhandled immediate size %d in asm_mov", size);
      }
      break;

   default:
      fatal_trace("unhandled operand combination in asm_mov");
   }

   x86_emit(blob, &insn);
}

static void asm_movsx(code_blob_t *blob, x86_operand_t dst, x86_operand_t src,
                      x86_size_t dsize, x86_size_t ssize)
{
   x86_insn_t insn = {};

   switch (COMBINE(dst, src)) {
   case REG_REG:
      x86_rex(&insn, dsize, src.reg, dst.reg, 0);
      switch (ssize) {
      case __BYTE:
         x86_opcode_2(&insn, 0x0f, 0xbe);
         x86_modrm(&insn, 3, src.reg, dst.reg);
         break;
      default:
         fatal_trace("unhandled source size %d in asm_movsx", ssize);
      }
      break;

   case REG_MEM:
      x86_rex(&insn, dsize, src.addr.reg, dst.reg, 0);
      switch (ssize) {
      case __QWORD: x86_opcode(&insn, 0x8b); break;
      case __DWORD: x86_opcode(&insn, 0x63); break;
      case __BYTE: x86_opcode_2(&insn, 0x0f, 0xbe); break;
      default:
         fatal_trace("unhandled source size %d in asm_movsx", ssize);
      }
      if (is_imm8(src.addr.off)) {
         x86_modrm(&insn, 1, dst.reg, src.addr.reg);
         x86_imm8(&insn, src.addr.off);
      }
      else {
         x86_modrm(&insn, 2, dst.reg, src.addr.reg);
         x86_imm32(&insn, src.addr.off);
      }
      break;

   default:
      fatal_trace("unhandled operand combination in asm_mov");
   }

   x86_emit(blob, &insn);
}

static void asm_movzx(code_blob_t *blob, x86_operand_t dst, x86_operand_t src,
                      x86_size_t dsize, x86_size_t ssize)
{
   x86_insn_t insn = {};

   assert(COMBINE(dst, src) == REG_MEM);
   x86_rex(&insn, ssize, dst.reg, src.addr.reg, 0);
   switch (ssize) {
   case __QWORD:
   case __DWORD: x86_opcode(&insn, 0x8b); break;
   case __WORD: x86_opcode_2(&insn, 0x0f, 0xb7); break;
   case __BYTE: x86_opcode_2(&insn, 0x0f, 0xb6); break;
   }
   if (is_imm8(src.addr.off)) {
      x86_modrm(&insn, 1, dst.reg, src.addr.reg);
      x86_imm8(&insn, src.addr.off);
   }
   else {
      x86_modrm(&insn, 2, dst.reg, src.addr.reg);
      x86_imm32(&insn, src.addr.off);
   }

   x86_emit(blob, &insn);
}

static void asm_cmovcc(code_blob_t *blob, x86_operand_t dst, x86_operand_t src,
                       x86_size_t size, x86_cmp_t cmp)
{
   x86_insn_t insn = {};

   assert(COMBINE(dst, src) == REG_REG);
   x86_rex(&insn, size, dst.reg, src.reg, 0);
   switch (size) {
   case __DWORD:
   case __QWORD:
      x86_opcode_2(&insn, 0x0f, 0x40 + cmp);
      x86_modrm(&insn, 3, dst.reg, src.reg);
      break;
   default:
      fatal_trace("unhandled size %d in asm_cmovcc", size);
   }

   x86_emit(blob, &insn);
}

static void asm_push(code_blob_t *blob, x86_operand_t src)
{
   x86_insn_t insn = {};

   x86_rex(&insn, __DWORD, 0, src.reg, 0);   // Actually pushes QWORD
   x86_opcode(&insn, 0x50 + (src.reg & 7));

   x86_emit(blob, &insn);
}

static void asm_pop(code_blob_t *blob, x86_operand_t dst)
{
   x86_insn_t insn = {};

   x86_rex(&insn, __DWORD, 0, dst.reg, 0);   // Actually pops QWORD
   x86_opcode(&insn, 0x58 + (dst.reg & 7));

   x86_emit(blob, &insn);
}

static void asm_add(code_blob_t *blob, x86_operand_t dst, x86_operand_t src,
                    x86_size_t size)
{
   x86_insn_t insn = {};

   x86_override(&insn, size == __WORD);

   switch (COMBINE(dst, src)) {
   case REG_REG:
      x86_rex(&insn, size, src.reg, dst.reg, 0);
      x86_opcode(&insn, size == __BYTE ? 0x00 : 0x01);
      x86_modrm(&insn, 3, src.reg, dst.reg);
      break;

   case REG_IMM:
      x86_rex(&insn, size, 0, dst.reg, 0);
      if (size == __BYTE) {
         x86_opcode(&insn, 0x80);
         x86_modrm(&insn, 3, 0, dst.reg);
         x86_imm8(&insn, src.imm);
      }
      else if (is_imm8(src.imm)) {
         x86_opcode(&insn, 0x83);
         x86_modrm(&insn, 3, 0, dst.reg);
         x86_imm8(&insn, src.imm);
      }
      else {
         x86_opcode(&insn, 0x81);
         x86_modrm(&insn, 3, 0, dst.reg);
         x86_imm32(&insn, src.imm);
      }
      break;

   default:
      fatal_trace("unhandled operand combination in asm_sub");
   }

   x86_emit(blob, &insn);
}

static void asm_sub(code_blob_t *blob, x86_operand_t dst, x86_operand_t src,
                    x86_size_t size)
{
   x86_insn_t insn = {};

   x86_override(&insn, size == __WORD);

   switch (COMBINE(dst, src)) {
   case REG_REG:
      x86_rex(&insn, size, src.reg, dst.reg, 0);
      x86_opcode(&insn, size == __BYTE ? 0x28 : 0x29);
      x86_modrm(&insn, 3, src.reg, dst.reg);
      break;

   case REG_IMM:
      x86_rex(&insn, size, 0, dst.reg, 0);
      if (size == __BYTE) {
         x86_opcode(&insn, 0x80);
         x86_modrm(&insn, 3, 5, dst.reg);
         x86_imm8(&insn, src.imm);
      }
      else if (is_imm8(src.imm)) {
         x86_opcode(&insn, 0x83);
         x86_modrm(&insn, 3, 5, dst.reg);
         x86_imm8(&insn, src.imm);
      }
      else {
         x86_opcode(&insn, 0x81);
         x86_modrm(&insn, 3, 5, dst.reg);
         x86_imm32(&insn, src.imm);
      }
      break;

   default:
      fatal_trace("unhandled operand combination in asm_sub");
   }

   x86_emit(blob, &insn);
}

static void asm_mul(code_blob_t *blob, x86_operand_t src, x86_size_t size)
{
   x86_insn_t insn = {};

   assert(src.kind == X86_REG);   // No immediate encoding
   x86_override(&insn, size == __WORD || size == __BYTE);
   x86_rex(&insn, size, 0, src.reg, 0);
   x86_opcode(&insn, 0xf7);
   x86_modrm(&insn, 3, 4, src.reg);

   x86_emit(blob, &insn);
}

static void asm_imul(code_blob_t *blob, x86_operand_t dst, x86_operand_t src,
                     x86_size_t size)
{
   x86_insn_t insn = {};

   assert(COMBINE(dst, src) == REG_REG);
   x86_override(&insn, size == __WORD || size == __BYTE);
   x86_rex(&insn, size, dst.reg, src.reg, 0);
   x86_opcode_2(&insn, 0x0f, 0xaf);
   x86_modrm(&insn, 3, dst.reg, src.reg);

   x86_emit(blob, &insn);
}

static void asm_idiv(code_blob_t *blob, x86_operand_t src, x86_size_t size)
{
   x86_insn_t insn = {};

   assert(src.kind == X86_REG);
   x86_override(&insn, size == __WORD || size == __BYTE);
   x86_rex(&insn, size, 0, src.reg, 0);
   x86_opcode(&insn, 0xf7);
   x86_modrm(&insn, 3, 7, src.reg);

   x86_emit(blob, &insn);
}

static void asm_and(code_blob_t *blob, x86_operand_t dst, x86_operand_t src,
                    x86_size_t size)
{
   x86_insn_t insn = {};

   switch (COMBINE(dst, src)) {
   case REG_IMM:
      x86_rex(&insn, size, 0, dst.reg, 0);
      switch (size) {
      case __QWORD:
      case __DWORD:
         if (is_imm8(src.imm)) {
            x86_opcode(&insn, 0x83);
            x86_modrm(&insn, 3, 4, dst.reg);
            x86_imm8(&insn, src.imm);
         }
         else {
            x86_opcode(&insn, 0x81);
            x86_modrm(&insn, 3, 4, dst.reg);
            x86_imm32(&insn, src.imm);
         }
         break;
      default:
         fatal_trace("unhandled size %d in asm_and", size);
      }
      break;

   case REG_REG:
      x86_rex(&insn, size, src.reg, dst.reg, 0);
      x86_opcode(&insn, size == __BYTE ? 0x20 : 0x21);
      x86_modrm(&insn, 3, src.reg, dst.reg);
      break;

   default:
      fatal_trace("unhandled operand combination in asm_and");
   }

   x86_emit(blob, &insn);
}

static void asm_or(code_blob_t *blob, x86_operand_t dst, x86_operand_t src,
                   x86_size_t size)
{
   x86_insn_t insn = {};

   assert(COMBINE(dst, src) == REG_REG);
   x86_rex(&insn, size, src.reg, dst.reg, 0);
   x86_opcode(&insn, size == __BYTE ? 0x08 : 0x09);
   x86_modrm(&insn, 3, src.reg, dst.reg);

   x86_emit(blob, &insn);
}

static void asm_shl(code_blob_t *blob, x86_operand_t src, x86_size_t size)
{
   x86_insn_t insn = {};

   assert(src.kind == X86_REG);
   x86_rex(&insn, size, src.reg, 0, 0);
   x86_opcode(&insn, 0xd3);
   x86_modrm(&insn, 3, 4, src.reg);

   x86_emit(blob, &insn);
}

static void asm_sar(code_blob_t *blob, x86_operand_t src, x86_operand_t count,
                    x86_size_t size)
{
   x86_insn_t insn = {};

   switch (COMBINE(src, count)) {
   case REG_REG:
      assert(count.reg == __ECX.reg);
      x86_rex(&insn,size, src.reg, 0, 0);
      x86_opcode(&insn, 0xd3);
      x86_modrm(&insn, 3, 7, src.reg);
      break;

   case REG_IMM:
      x86_rex(&insn, size, src.reg, 0, 0);
      if (count.imm == 1) {
         x86_opcode(&insn, 0xd1);
         x86_modrm(&insn, 3, 7, src.reg);
      }
      else {
         x86_opcode(&insn, 0xc1);
         x86_modrm(&insn, 3, 7, src.reg);
         x86_imm8(&insn, count.imm);
      }
      break;

   default:
      fatal_trace("unhandled operand combination in asm_sar");
   }

   x86_emit(blob, &insn);
}

static void asm_test(code_blob_t *blob, x86_operand_t src1,
                     x86_operand_t src2, x86_size_t size)
{
   x86_insn_t insn = {};

   switch (COMBINE(src1, src2)) {
   case REG_IMM:
      x86_rex(&insn, size, 0, src1.reg, 0);
      if (size == __BYTE) {
         x86_opcode(&insn, 0xf6);
         x86_modrm(&insn, 3, 0, src1.reg);
         x86_imm8(&insn, src2.imm);
      }
      else {
         x86_opcode(&insn, 0xf7);
         x86_modrm(&insn, 3, 0, src1.reg);
         x86_imm32(&insn, src2.imm);
      }
      break;

   case REG_REG:
      x86_rex(&insn, size, src2.reg, src1.reg, 0);
      x86_opcode(&insn, size == __BYTE ? 0x84 : 0x85);
      x86_modrm(&insn, 3, src2.reg, src1.reg);
      break;

   default:
      fatal_trace("unhandled operand combination in asm_test");
   }

   x86_emit(blob, &insn);
}

static void asm_cmp(code_blob_t *blob, x86_operand_t src1,
                    x86_operand_t src2, x86_size_t size)
{
   x86_insn_t insn = {};

   switch (COMBINE(src1, src2)) {
   case REG_REG:
      x86_rex(&insn, size, src2.reg, src1.reg, 0);
      x86_opcode(&insn, 0x39);
      x86_modrm(&insn, 3, src2.reg, src1.reg);
      break;

   case MEM_REG:
      x86_rex(&insn, size, src2.reg, src1.addr.reg, 0);
      x86_opcode(&insn, 0x39);
      if (is_imm8(src1.addr.off)) {
         x86_modrm(&insn, 1, src2.reg, src1.reg);
         x86_imm8(&insn, src1.addr.off);
      }
      else {
         x86_modrm(&insn, 2, src2.reg, src1.reg);
         x86_imm32(&insn, src1.addr.off);
      }
      break;

   default:
      fatal_trace("unhandled operand combination in asm_test");
   }

   x86_emit(blob, &insn);
}

static void asm_setcc(code_blob_t *blob, x86_operand_t dst, x86_cmp_t cmp)
{
   x86_insn_t insn = {};

   assert(dst.kind == X86_REG);
   x86_rex(&insn, 1, 0, dst.reg, 0);
   x86_opcode_2(&insn, 0x0f, 0x90 + cmp);
   x86_modrm(&insn, 3, 0, dst.reg);

   x86_emit(blob, &insn);
}

static void asm_call(code_blob_t *blob, x86_operand_t addr)
{
   x86_insn_t insn = {};

   switch (addr.kind) {
   case X86_IMM:
      {
         const ptrdiff_t rel = (uint8_t *)addr.imm - blob->wptr - 5;
         x86_opcode(&insn, 0xe8);
         x86_imm32(&insn, rel);
      }
      break;
   case X86_REG:
      x86_opcode(&insn, 0xff);
      x86_modrm(&insn, 3, 2, addr.reg);
      break;
   default:
      fatal_trace("unhandled operand kind in asm_call");
   }

   x86_emit(blob, &insn);
}

static void asm_jmp(code_blob_t *blob, x86_operand_t addr)
{
   x86_insn_t insn = {};

   switch (addr.kind) {
   case X86_PATCH:
      if (is_imm8(addr.imm)) {
         x86_opcode(&insn, 0xeb);
         x86_imm8(&insn, 8);
      }
      else {
         x86_opcode(&insn, 0xe9);
         x86_imm32(&insn, __builtin_bswap32(32));
      }
      break;
   case X86_IMM:
      if (is_imm8(addr.imm)) {
         x86_opcode(&insn, 0xeb);
         x86_imm8(&insn, addr.imm);
      }
      else {
         x86_opcode(&insn, 0xe9);
         x86_imm32(&insn, addr.imm);
      }
      break;
   default:
      fatal_trace("unhandled operand kind in asm_jmp");
   }

   x86_emit(blob, &insn);
}

static void asm_jcc(code_blob_t *blob, x86_operand_t addr, x86_cmp_t cmp)
{
   x86_insn_t insn = {};

   switch (addr.kind) {
   case X86_PATCH:
      if (is_imm8(addr.imm)) {
         x86_opcode(&insn, 0x70 + cmp);
         x86_imm8(&insn, 8);
      }
      else {
         x86_opcode_2(&insn, 0x0f, 0x80 + cmp);
         x86_imm32(&insn, __builtin_bswap32(32));
      }
      break;
   case X86_IMM:
      if (is_imm8(addr.imm)) {
         x86_opcode(&insn, 0x70 + cmp);
         x86_imm8(&insn, addr.imm);
      }
      else {
         x86_opcode_2(&insn, 0x0f, 0x80 + cmp);
         x86_imm32(&insn, addr.imm);
      }
      break;
   default:
      fatal_trace("unhandled operand kind in asm_jcc");
   }

   x86_emit(blob, &insn);
}

static void asm_mulsd(code_blob_t *blob, x86_operand_t dst, x86_operand_t src)
{
   x86_insn_t insn = {};

   assert(COMBINE(dst, src) == XMM_XMM);
   x86_opcode_3(&insn, 0xf2, 0x0f, 0x59);
   x86_modrm(&insn, 3, dst.reg, src.reg);

   x86_emit(blob, &insn);
}

static void asm_divsd(code_blob_t *blob, x86_operand_t dst, x86_operand_t src)
{
   x86_insn_t insn = {};

   assert(COMBINE(dst, src) == XMM_XMM);
   x86_opcode_3(&insn, 0xf2, 0x0f, 0x5e);
   x86_modrm(&insn, 3, dst.reg, src.reg);

   x86_emit(blob, &insn);
}

static void asm_addsd(code_blob_t *blob, x86_operand_t dst, x86_operand_t src)
{
   x86_insn_t insn = {};

   assert(COMBINE(dst, src) == XMM_XMM);
   x86_opcode_3(&insn, 0xf2, 0x0f, 0x58);
   x86_modrm(&insn, 3, dst.reg, src.reg);

   x86_emit(blob, &insn);
}

static void asm_subsd(code_blob_t *blob, x86_operand_t dst, x86_operand_t src)
{
   x86_insn_t insn = {};

   assert(COMBINE(dst, src) == XMM_XMM);
   x86_opcode_3(&insn, 0xf2, 0x0f, 0x5c);
   x86_modrm(&insn, 3, dst.reg, src.reg);

   x86_emit(blob, &insn);
}

static void asm_pxor(code_blob_t *blob, x86_operand_t dst, x86_operand_t src)
{
   x86_insn_t insn = {};

   assert(COMBINE(dst, src) == XMM_XMM);
   x86_opcode_3(&insn, 0x66, 0x0f, 0xef);
   x86_modrm(&insn, 3, dst.reg, src.reg);

   x86_emit(blob, &insn);
}

static void asm_ucomisd(code_blob_t *blob, x86_operand_t src1,
                        x86_operand_t src2)
{
   x86_insn_t insn = {};

   assert(COMBINE(src1, src2) == XMM_XMM);
   x86_opcode_3(&insn, 0x66, 0x0f, 0x2e);
   x86_modrm(&insn, 3, src1.reg, src2.reg);

   x86_emit(blob, &insn);
}

static void asm_cvtsd2si(code_blob_t *blob, x86_operand_t dst,
                         x86_operand_t src, x86_size_t size)
{
   x86_insn_t insn = {};

   assert(COMBINE(dst, src) == REG_XMM);
   x86_prefix(&insn, 0xf2);
   x86_rex(&insn, size, dst.reg, src.reg, 0);
   x86_opcode_2(&insn, 0x0f, 0x2d);
   x86_modrm(&insn, 3, dst.reg, src.reg);

   x86_emit(blob, &insn);
}

static void asm_cvtsi2sd(code_blob_t *blob, x86_operand_t dst,
                         x86_operand_t src, x86_size_t size)
{
   x86_insn_t insn = {};

   assert(COMBINE(dst, src) == XMM_REG);
   x86_prefix(&insn, 0xf2);
   x86_rex(&insn, size, dst.reg, src.reg, 0);
   x86_opcode_2(&insn, 0x0f, 0x2a);
   x86_modrm(&insn, 3, dst.reg, src.reg);

   x86_emit(blob, &insn);
}

static void asm_roundsd(code_blob_t *blob, x86_operand_t dst,
                        x86_operand_t src, uint8_t mode)
{
   x86_insn_t insn = {};

   assert(COMBINE(dst, src) == XMM_XMM);
   x86_override(&insn, true);
   x86_opcode_3(&insn, 0x0F, 0x3A, 0x0B);
   x86_modrm(&insn, 3, dst.reg, src.reg);
   x86_imm8(&insn, mode);

   x86_emit(blob, &insn);
}

static void asm_repstos(code_blob_t *blob, x86_size_t size)
{
   x86_insn_t insn = {};

   x86_prefix(&insn, 0xf3);
   x86_rex(&insn, size, 0, 0, 0);
   x86_override(&insn, size == __WORD);
   x86_opcode(&insn, size == __BYTE ? 0xaa : 0xab);

   x86_emit(blob, &insn);
}

static void asm_repmovs(code_blob_t *blob, x86_size_t size)
{
   x86_insn_t insn = {};

   x86_prefix(&insn, 0xf3);
   x86_rex(&insn, size, 0, 0, 0);
   x86_override(&insn, size == __WORD);
   x86_opcode(&insn, size == __BYTE ? 0xa4 : 0xa5);

   x86_emit(blob, &insn);
}

////////////////////////////////////////////////////////////////////////////////
// JIT IR to X86 assembly lowering

static void jit_x86_push_call_clobbered(code_blob_t *blob)
{
   PUSH(__ECX);
   PUSH(__EDX);
#ifndef __MINGW32__
   PUSH(__ESI);
   PUSH(__EDI);
#endif
   PUSH(__R8);
   PUSH(__R9);
   PUSH(__R10);
   PUSH(__R11);
}

static void jit_x86_pop_call_clobbered(code_blob_t *blob)
{
   POP(__R11);
   POP(__R10);
   POP(__R9);
   POP(__R8);
#ifndef __MINGW32__
   POP(__EDI);
   POP(__ESI);
#endif
   POP(__EDX);
   POP(__ECX);
}

static x86_operand_t jit_x86_locals(code_blob_t *blob, ptrdiff_t off)
{
   const ptrdiff_t locals =
      FRAME_FIXED_SIZE + blob->func->nregs*8 + blob->func->framesz;
   return ADDR(__EBP, -locals + off);
}

static x86_operand_t jit_x86_spill_slot(code_blob_t *blob, unsigned slot)
{
   assert(slot >= STACK_BASE);
   const ptrdiff_t off = (slot - STACK_BASE) * sizeof(int64_t);
   return jit_x86_locals(blob, -off);
}

static void jit_x86_get_reg(code_blob_t *blob, x86_operand_t dst, jit_reg_t reg,
                            const phys_slot_t *slots)
{
   if (slots[reg] >= STACK_BASE)
      MOV(dst, jit_x86_spill_slot(blob, slots[reg]), __QWORD);
   else {
      assert(slots[reg] < FLOAT_BASE);
      MOV(dst, REG(slots[reg]), __QWORD);
   }
}

static void jit_x86_get_copy(code_blob_t *blob, x86_operand_t dst,
                             jit_value_t src, const phys_slot_t *slots)
{
   switch (src.kind) {
   case JIT_VALUE_REG:
      jit_x86_get_reg(blob, dst, src.reg, slots);
      break;
   case JIT_VALUE_INT64:
   case JIT_ADDR_ABS:
      MOV(dst, IMM(src.int64), __QWORD);
      break;
   case JIT_VALUE_HANDLE:
      MOV(dst, IMM(src.handle), __DWORD);
      break;
   case JIT_VALUE_DOUBLE:
      MOV(__EAX, IMM(src.int64), __QWORD);
      MOV(dst, __EAX, __QWORD);
      break;
   case JIT_VALUE_LOCUS:
      MOV(dst, PTR(src.locus), __QWORD);
      break;
   case JIT_ADDR_REG:
      if (src.disp == 0)
         jit_x86_get_reg(blob, dst, src.reg, slots);
      else if (slots[src.reg] >= STACK_BASE) {
         MOV(dst, jit_x86_spill_slot(blob, slots[src.reg]), __QWORD);
         LEA(dst, ADDR(dst, src.disp));
      }
      else
         LEA(dst, ADDR(REG(slots[src.reg]), src.disp));
      break;
   case JIT_ADDR_CPOOL:
      MOV(dst, PTR(blob->func->cpool + src.int64), __QWORD);
      break;
   default:
      fatal_trace("cannot handle value kind %d in jit_x86_get", src.kind);
   }
}

static x86_operand_t jit_x86_get(code_blob_t *blob, x86_operand_t tmp,
                                 jit_value_t src, const phys_slot_t *slots)
{
   switch (src.kind) {
   case JIT_VALUE_REG:
      if (slots[src.reg] < FLOAT_BASE)
         return REG(slots[src.reg]);
      else {
         jit_x86_get_reg(blob, tmp, src.reg, slots);
         return tmp;
      }
   case JIT_VALUE_INT64:
      if (is_imm32(src.int64))
         return IMM(src.int64);
      // Fall-through
   case JIT_ADDR_ABS:
      jit_x86_get_copy(blob, tmp, src, slots);
      return tmp;
   case JIT_VALUE_HANDLE:
      MOV(tmp, IMM(src.handle), __DWORD);
      return tmp;
   case JIT_VALUE_DOUBLE:
      MOV(tmp, IMM(src.int64), __QWORD);
      return tmp;
   case JIT_VALUE_LOCUS:
      MOV(tmp, PTR(src.locus), __QWORD);
      return tmp;
   case JIT_ADDR_REG:
      jit_x86_get_copy(blob, tmp, src, slots);
      return tmp;
   case JIT_ADDR_CPOOL:
      MOV(tmp, PTR(blob->func->cpool + src.int64), __QWORD);
      return tmp;
   default:
      fatal_trace("cannot handle value kind %d in jit_x86_get", src.kind);
   }
}

static x86_operand_t jit_x86_get_addr(code_blob_t *blob, jit_value_t addr,
                                      x86_operand_t tmp,
                                      const phys_slot_t *slots)
{
   switch (addr.kind) {
   case JIT_ADDR_REG:
      jit_x86_get_reg(blob, tmp, addr.reg, slots);
      return ADDR(tmp, addr.disp);
   case JIT_ADDR_CPOOL:
      MOV(tmp, PTR(blob->func->cpool + addr.int64), __QWORD);
      return ADDR(tmp, 0);
   case JIT_ADDR_ABS:
      MOV(tmp, IMM(addr.int64), __QWORD);
      return ADDR(tmp, 0);
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

static void jit_x86_put(code_blob_t *blob, jit_reg_t dst, x86_operand_t src,
                        const phys_slot_t *slots)
{
   if (slots[dst] >= STACK_BASE) {
      x86_operand_t addr = jit_x86_spill_slot(blob, slots[dst]);
      MOV(addr, src, __QWORD);
   }
   else {
      assert(slots[dst] < FLOAT_BASE);
      MOV(REG(slots[dst]), src, __QWORD);
   }
}

static void jit_x86_set_flags(code_blob_t *blob, jit_ir_t *ir)
{
   switch (ir->cc) {
   case JIT_CC_O:  SETO(FLAGS_REG); break;
   case JIT_CC_C:  SETC(FLAGS_REG); break;
   case JIT_CC_EQ: SETZ(FLAGS_REG); break;
   case JIT_CC_NE: SETNZ(FLAGS_REG); break;
   case JIT_CC_LT: SETLT(FLAGS_REG); break;
   case JIT_CC_GT: SETGT(FLAGS_REG); break;
   case JIT_CC_LE: SETLE(FLAGS_REG); break;
   case JIT_CC_GE: SETGE(FLAGS_REG); break;
   case JIT_CC_NONE: break;
   default:
      fatal_trace("unhandled JIT comparison code %d", ir->cc);
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

#ifdef DEBUG
__attribute__((unused))
static void jit_x86_debug_out(code_blob_t *blob, jit_x86_state_t *state,
                              x86_operand_t value, jit_reg_t reg)
{
   MOV(__EAX, value, __QWORD);
   MOV(__ECX, IMM(reg), __DWORD);
   CALL(PTR(state->stubs[DEBUG_STUB]));
}
#endif

static void jit_x86_recv(code_blob_t *blob, jit_ir_t *ir,
                         const phys_slot_t *slots)
{
   assert(ir->arg1.kind == JIT_VALUE_INT64);
   const int nth = ir->arg1.int64;

   MOV(__EAX, ADDR(ARGS_REG, nth * sizeof(int64_t)), __QWORD);
   jit_x86_put(blob, ir->result, __EAX, slots);
}

static void jit_x86_send(code_blob_t *blob, jit_ir_t *ir,
                         const phys_slot_t *slots)
{
   assert(ir->arg1.kind == JIT_VALUE_INT64);
   const int nth = ir->arg1.int64;

   x86_operand_t src = jit_x86_get(blob, __EAX, ir->arg2, slots);
   MOV(ADDR(ARGS_REG, nth * sizeof(int64_t)), src, __QWORD);
}

static void jit_x86_add(code_blob_t *blob, jit_ir_t *ir,
                        const phys_slot_t *slots)
{
   jit_x86_get_copy(blob, __EAX, ir->arg1, slots);
   x86_operand_t rhs = jit_x86_get(blob, __ECX, ir->arg2, slots);

   const x86_size_t size = jit_x86_size(ir);

   ADD(__EAX, rhs, size);

   jit_x86_set_flags(blob, ir);
   jit_x86_sext(blob, __EAX, size);
   jit_x86_put(blob, ir->result, __EAX, slots);
}

static void jit_x86_sub(code_blob_t *blob, jit_ir_t *ir,
                        const phys_slot_t *slots)
{
   jit_x86_get_copy(blob, __EAX, ir->arg1, slots);
   x86_operand_t rhs = jit_x86_get(blob, __ECX, ir->arg2, slots);

   const x86_size_t size = jit_x86_size(ir);

   SUB(__EAX, rhs, size);

   jit_x86_set_flags(blob, ir);
   jit_x86_sext(blob, __EAX, size);
   jit_x86_put(blob, ir->result, __EAX, slots);
}

static void jit_x86_mul(code_blob_t *blob, jit_ir_t *ir,
                        const phys_slot_t *slots)
{
   jit_x86_get_copy(blob, __EAX, ir->arg1, slots);
   jit_x86_get_copy(blob, __ECX, ir->arg2, slots);  // No immediate version

   const x86_size_t size = jit_x86_size(ir);

   if (ir->cc == JIT_CC_O)
      IMUL(__EAX, __ECX, size);
   else
      MUL(__ECX, size);

   jit_x86_set_flags(blob, ir);
   jit_x86_sext(blob, __EAX, size);
   jit_x86_put(blob, ir->result, __EAX, slots);
}

static void jit_x86_rem(code_blob_t *blob, jit_ir_t *ir,
                        const phys_slot_t *slots)
{
   jit_x86_get_copy(blob, __EAX, ir->arg1, slots);
   x86_operand_t rhs = jit_x86_get(blob, __ECX, ir->arg2, slots);

   const x86_size_t size = jit_x86_size(ir);

   switch (size) {
   case __WORD: CWD(); break;
   case __DWORD: CDQ(); break;
   default: CQO(); break;
   }

   IDIV(rhs, size);
   MOV(__EAX, __EDX, MAX(size, __DWORD));

   jit_x86_sext(blob, __EAX, size);
   jit_x86_put(blob, ir->result, __EAX, slots);
}

static void jit_x86_div(code_blob_t *blob, jit_ir_t *ir,
                        const phys_slot_t *slots)
{
   jit_x86_get_copy(blob, __EAX, ir->arg1, slots);
   x86_operand_t rhs = jit_x86_get(blob, __ECX, ir->arg2, slots);

   const x86_size_t size = jit_x86_size(ir);

   switch (size) {
   case __WORD: CWD(); break;
   case __DWORD: CDQ(); break;
   default: CQO(); break;
   }

   IDIV(rhs, size);

   jit_x86_sext(blob, __EAX, size);
   jit_x86_put(blob, ir->result, __EAX, slots);
}

static void jit_x86_neg(code_blob_t *blob, jit_ir_t *ir,
                        const phys_slot_t *slots)
{
   jit_x86_get_copy(blob, __EAX, ir->arg1, slots);

   NEG(__EAX, __QWORD);

   jit_x86_put(blob, ir->result, __EAX, slots);
}

static void jit_x86_not(code_blob_t *blob, jit_ir_t *ir,
                        const phys_slot_t *slots)
{
   x86_operand_t src = jit_x86_get(blob, __ECX, ir->arg1, slots);

   XOR(__EAX, __EAX, __DWORD);
   TEST(src, src, __BYTE);
   SETZ(__EAX);

   jit_x86_put(blob, ir->result, __EAX, slots);
}

static void jit_x86_and(code_blob_t *blob, jit_ir_t *ir,
                        const phys_slot_t *slots)
{
   jit_x86_get_copy(blob, __EAX, ir->arg1, slots);
   x86_operand_t rhs = jit_x86_get(blob, __ECX, ir->arg2, slots);

   AND(__EAX, rhs, __QWORD);

   jit_x86_put(blob, ir->result, __EAX, slots);
}

static void jit_x86_or(code_blob_t *blob, jit_ir_t *ir,
                       const phys_slot_t *slots)
{
   jit_x86_get_copy(blob, __EAX, ir->arg1, slots);
   x86_operand_t rhs = jit_x86_get(blob, __ECX, ir->arg2, slots);

   OR(__EAX, rhs, __QWORD);

   jit_x86_put(blob, ir->result, __EAX, slots);
}

static void jit_x86_xor(code_blob_t *blob, jit_ir_t *ir,
                        const phys_slot_t *slots)
{
   jit_x86_get_copy(blob, __EAX, ir->arg1, slots);
   x86_operand_t rhs = jit_x86_get(blob, __ECX, ir->arg2, slots);

   XOR(__EAX, rhs, __QWORD);

   jit_x86_put(blob, ir->result, __EAX, slots);
}

static void jit_x86_mov(code_blob_t *blob, jit_ir_t *ir,
                        const phys_slot_t *slots)
{
   x86_operand_t src = jit_x86_get(blob, __EAX, ir->arg1, slots);
   jit_x86_put(blob, ir->result, src, slots);
}

static void jit_x86_clamp(code_blob_t *blob, jit_ir_t *ir,
                          const phys_slot_t *slots)
{
   x86_operand_t src = jit_x86_get(blob, __ECX, ir->arg1, slots);

   XOR(__EAX, __EAX, __DWORD);
   TEST(src, src, __QWORD);
   CMOVGT(__EAX, src, __QWORD);

   jit_x86_put(blob, ir->result, __EAX, slots);
}

static void jit_x86_jump(code_blob_t *blob, jit_ir_t *ir)
{
   const int this = ir - blob->func->irbuf;
   const int distance = (this - ir->arg1.label) * BYTES_PER_IR;

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
   jit_ir_t *endir = blob->func->irbuf + blob->func->nirs;
   if (ir + 1 < endir) {
      const int distance = (endir - ir) * BYTES_PER_IR;
      JMP(PATCH(distance));
      code_blob_patch(blob, JIT_LABEL_INVALID, jit_x86_patch);
   }
}

static void jit_x86_load(code_blob_t *blob, jit_ir_t *ir,
                         const phys_slot_t *slots)
{
   x86_operand_t addr = jit_x86_get_addr(blob, ir->arg1, __ECX, slots);
   MOVSX(__EAX, addr, __QWORD, jit_x86_size(ir));

   jit_x86_put(blob, ir->result, __EAX, slots);
}

static void jit_x86_uload(code_blob_t *blob, jit_ir_t *ir,
                          const phys_slot_t *slots)
{
   x86_operand_t addr = jit_x86_get_addr(blob, ir->arg1, __ECX, slots);
   MOVZX(__EAX, addr, __QWORD, jit_x86_size(ir));

   jit_x86_put(blob, ir->result, __EAX, slots);
}

static void jit_x86_store(code_blob_t *blob, jit_ir_t *ir,
                          const phys_slot_t *slots)
{
   jit_x86_get_copy(blob, __EAX, ir->arg1, slots);
   x86_operand_t addr = jit_x86_get_addr(blob, ir->arg2, __ECX, slots);
   MOV(addr, __EAX, jit_x86_size(ir));
}

static void jit_x86_lea(code_blob_t *blob, jit_ir_t *ir,
                        const phys_slot_t *slots)
{
   jit_x86_get_copy(blob, __EAX, ir->arg1, slots);
   jit_x86_put(blob, ir->result, __EAX, slots);
}

static void jit_x86_cmp(code_blob_t *blob, jit_ir_t *ir,
                        const phys_slot_t *slots)
{
   jit_x86_get_copy(blob, __EAX, ir->arg1, slots);
   jit_x86_get_copy(blob, __ECX, ir->arg2, slots);

   CMP(__EAX, __ECX, __QWORD);

   jit_x86_set_flags(blob, ir);
}

static void jit_x86_ccmp(code_blob_t *blob, jit_ir_t *ir,
                        const phys_slot_t *slots)
{
   // TODO: move this below JZ
   jit_x86_get_copy(blob, __EAX, ir->arg1, slots);
   jit_x86_get_copy(blob, __ECX, ir->arg2, slots);

   TEST(FLAGS_REG, IMM(1), __BYTE);
   JZ(IMM(6));

   CMP(__EAX, __ECX, __QWORD);

   jit_x86_set_flags(blob, ir);
}

static void jit_x86_cset(code_blob_t *blob, jit_ir_t *ir,
                         const phys_slot_t *slots)
{
   jit_x86_put(blob, ir->result, FLAGS_REG, slots);
}

static void jit_x86_csel(code_blob_t *blob, jit_ir_t *ir,
                         const phys_slot_t *slots)
{
   jit_x86_get_copy(blob, __EAX, ir->arg1, slots);
   x86_operand_t rhs = jit_x86_get(blob, __ECX, ir->arg2, slots);

   TEST(FLAGS_REG, FLAGS_REG, __BYTE);
   CMOVZ(__EAX, rhs, __QWORD);

   jit_x86_put(blob, ir->result, __EAX, slots);
}

static void jit_x86_call(code_blob_t *blob, jit_x86_state_t *state,
                         jit_ir_t *ir)
{
   jit_func_t *f = jit_get_func(state->jit, ir->arg1.handle);

   MOV(__EAX, PTR(f), __QWORD);
   CALL(PTR(state->stubs[CALL_STUB]));
}

static void jit_x86_shl(code_blob_t *blob, jit_ir_t *ir,
                        const phys_slot_t *slots)
{
   jit_x86_get_copy(blob, __EAX, ir->arg1, slots);
   jit_x86_get_copy(blob, __ECX, ir->arg2, slots);

   SHL(__EAX, __QWORD);

   jit_x86_put(blob, ir->result, __EAX, slots);
}

static void jit_x86_asr(code_blob_t *blob, jit_ir_t *ir,
                        const phys_slot_t *slots)
{
   jit_x86_get_copy(blob, __EAX, ir->arg1, slots);
   jit_x86_get_copy(blob, __ECX, ir->arg2, slots);

   SAR(__EAX, __ECX, __QWORD);

   jit_x86_put(blob, ir->result, __EAX, slots);
}

static void jit_x86_trap(code_blob_t *blob, jit_ir_t *ir)
{
   INT3();
}

static void jit_x86_fmul(code_blob_t *blob, jit_ir_t *ir,
                         const phys_slot_t *slots)
{
   jit_x86_get_copy(blob, __XMM0, ir->arg1, slots);
   jit_x86_get_copy(blob, __XMM1, ir->arg2, slots);

   MULSD(__XMM0, __XMM1);

   jit_x86_put(blob, ir->result, __XMM0, slots);
}

static void jit_x86_fdiv(code_blob_t *blob, jit_ir_t *ir,
                         const phys_slot_t *slots)
{
   jit_x86_get_copy(blob, __XMM0, ir->arg1, slots);
   jit_x86_get_copy(blob, __XMM1, ir->arg2, slots);

   DIVSD(__XMM0, __XMM1);

   jit_x86_put(blob, ir->result, __XMM0, slots);
}

static void jit_x86_fadd(code_blob_t *blob, jit_ir_t *ir,
                         const phys_slot_t *slots)
{
   jit_x86_get_copy(blob, __XMM0, ir->arg1, slots);
   jit_x86_get_copy(blob, __XMM1, ir->arg2, slots);

   ADDSD(__XMM0, __XMM1);

   jit_x86_put(blob, ir->result, __XMM0, slots);
}

static void jit_x86_fsub(code_blob_t *blob, jit_ir_t *ir,
                         const phys_slot_t *slots)
{
   jit_x86_get_copy(blob, __XMM0, ir->arg1, slots);
   jit_x86_get_copy(blob, __XMM1, ir->arg2, slots);

   SUBSD(__XMM0, __XMM1);

   jit_x86_put(blob, ir->result, __XMM0, slots);
}

static void jit_x86_fneg(code_blob_t *blob, jit_ir_t *ir,
                         const phys_slot_t *slots)
{
   jit_x86_get_copy(blob, __XMM0, ir->arg1, slots);

   PXOR(__XMM1, __XMM1);
   SUBSD(__XMM1, __XMM0);

   jit_x86_put(blob, ir->result, __XMM1, slots);
}

static void jit_x86_fcmp(code_blob_t *blob, jit_ir_t *ir,
                         const phys_slot_t *slots)
{
   jit_x86_get_copy(blob, __XMM0, ir->arg1, slots);
   jit_x86_get_copy(blob, __XMM1, ir->arg2, slots);

   UCOMISD(__XMM0, __XMM1);

   switch (ir->cc) {
   case JIT_CC_LT: SETB(FLAGS_REG); break;
   case JIT_CC_LE: SETBE(FLAGS_REG); break;
   case JIT_CC_GT: SETA(FLAGS_REG); break;
   case JIT_CC_GE: SETAE(FLAGS_REG); break;
   case JIT_CC_EQ: SETZ(FLAGS_REG); break;
   case JIT_CC_NE: SETNZ(FLAGS_REG); break;
   default:
      fatal_trace("unhandled FCMP comparison code %d", ir->cc);
   }
}

static void jit_x86_fcvtns(code_blob_t *blob, jit_x86_state_t *state,
                           jit_ir_t *ir, const phys_slot_t *slots)
{
   jit_x86_get_copy(blob, __XMM0, ir->arg1, slots);

   if (__builtin_cpu_supports("sse4.1"))
      ROUNDSD(__XMM0, __XMM0, _MM_FROUND_TO_NEAREST_INT | _MM_FROUND_NO_EXC);
   else
      CALL(PTR(state->stubs[ROUND_STUB]));

   CVTSD2SI(__EAX, __XMM0, __QWORD);

   jit_x86_put(blob, ir->result, __EAX, slots);
}

static void jit_x86_scvtf(code_blob_t *blob, jit_ir_t *ir,
                          const phys_slot_t *slots)
{
   jit_x86_get_copy(blob, __EAX, ir->arg1, slots);

   CVTSI2SD(__XMM0, __EAX, __QWORD);

   jit_x86_put(blob, ir->result, __XMM0, slots);
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

static void jit_x86_macro_salloc(code_blob_t *blob, jit_ir_t *ir,
                                 const phys_slot_t *slots)
{
   assert(ir->arg1.int64 + ir->arg2.int64 <= blob->func->framesz);

   x86_operand_t addr = jit_x86_locals(blob, ir->arg1.int64);
   LEA(__EAX, addr);

   jit_x86_put(blob, ir->result, __EAX, slots);
}

static void jit_x86_macro_lalloc(code_blob_t *blob, jit_x86_state_t *state,
                                 jit_ir_t *ir, const phys_slot_t *slots)
{
   jit_x86_get_copy(blob, __EAX, ir->arg1, slots);

   CALL(PTR(state->stubs[TLAB_STUB]));

   jit_x86_put(blob, ir->result, __EAX, slots);
}

static void jit_x86_macro_galloc(code_blob_t *blob, jit_x86_state_t *state,
                                 jit_ir_t *ir, const phys_slot_t *slots)
{
   jit_x86_get_copy(blob, __EAX, ir->arg1, slots);

   CALL(PTR(state->stubs[ALLOC_STUB]));

   jit_x86_put(blob, ir->result, __EAX, slots);
}

static void jit_x86_macro_bzero(code_blob_t *blob, jit_ir_t *ir,
                                const phys_slot_t *slots)
{
   jit_x86_get_copy(blob, __EDI, ir->arg1, slots);
   jit_x86_get_reg(blob, __ECX, ir->result, slots);

   XOR(__EAX, __EAX, __DWORD);
   REPSTOS(__BYTE);
}

static void jit_x86_macro_copy(code_blob_t *blob, jit_ir_t *ir,
                               const phys_slot_t *slots)
{
   PUSH(__ESI);

   jit_x86_get_copy(blob, __EDI, ir->arg1, slots);
   jit_x86_get_copy(blob, __ESI, ir->arg2, slots);
   jit_x86_get_reg(blob, __ECX, ir->result, slots);

   REPMOVS(__BYTE);

   POP(__ESI);
}

static void jit_x86_macro_move(code_blob_t *blob, jit_ir_t *ir,
                               const phys_slot_t *slots)
{
   PUSH(__ESI);

   jit_x86_get_copy(blob, __EDI, ir->arg1, slots);
   jit_x86_get_copy(blob, __ESI, ir->arg2, slots);
   jit_x86_get_reg(blob, __ECX, ir->result, slots);

   CMP(__EDI, __ESI, __QWORD);
   JB(IMM(19));                   // Source before destination
   LEA(__EAX, ADDR2(__EDI, __ECX, -1));
   CMP(__EAX, __ESI, __QWORD);
   JBE(IMM(9));                   // Destination end before source

   // Overlap
   MOV(__EDI, __EAX, __QWORD);
   LEA(__ESI, ADDR2(__ESI, __ECX, -1));
   STD();

   REPMOVS(__BYTE);
   CLD();

   POP(__ESI);
}

static void jit_x86_macro_memset(code_blob_t *blob, jit_ir_t *ir,
                                 const phys_slot_t *slots)
{
   jit_x86_get_copy(blob, __EDI, ir->arg1, slots);
   jit_x86_get_copy(blob, __EAX, ir->arg2, slots);
   jit_x86_get_reg(blob, __ECX, ir->result, slots);

   switch (ir->size) {
   case JIT_SZ_64:
      SAR(__ECX, IMM(3), __DWORD);
      REPSTOS(__QWORD);
      break;
   case JIT_SZ_32:
      SAR(__ECX, IMM(2), __DWORD);
      REPSTOS(__DWORD);
      break;
   case JIT_SZ_16:
      SAR(__ECX, IMM(1), __DWORD);
      REPSTOS(__WORD);
      break;
   default:
      REPSTOS(__BYTE);
      break;
   }
}

static void jit_x86_macro_getpriv(code_blob_t *blob, jit_ir_t *ir,
                                  const phys_slot_t *slots)
{
   jit_func_t *f = jit_get_func(blob->func->jit, ir->arg1.handle);
   void **ptr = jit_get_privdata_ptr(blob->func->jit, f);

   MOV(__EAX, IMM((intptr_t)ptr), __QWORD);
   MOV(__EAX, ADDR(__EAX, 0), __QWORD);

   jit_x86_put(blob, ir->result, __EAX, slots);
}

static void jit_x86_macro_putpriv(code_blob_t *blob, jit_ir_t *ir,
                                  const phys_slot_t *slots)
{
   jit_func_t *f = jit_get_func(blob->func->jit, ir->arg1.handle);
   void **ptr = jit_get_privdata_ptr(blob->func->jit, f);

   jit_x86_get_copy(blob, __ECX, ir->arg2, slots);

   MOV(__EAX, IMM((intptr_t)ptr), __QWORD);
   MOV(ADDR(__EAX, 0), __ECX, __QWORD);
}

static void jit_x86_macro_case(code_blob_t *blob, jit_ir_t *ir,
                               const phys_slot_t *slots)
{
   jit_x86_get_copy(blob, __EAX, ir->arg1, slots);

   // Reuse test value from preceding $CASE macro if possible
   if (ir == blob->func->irbuf || (ir - 1)->op != MACRO_CASE
       || (ir - 1)->result != ir->result)
      jit_x86_get_reg(blob, __ECX, ir->result, slots);

   CMP(__EAX, __ECX, __QWORD);

   const int this = ir - blob->func->irbuf;
   const int distance = (this - ir->arg2.label) * BYTES_PER_IR;

   JZ(PATCH(distance));

   code_blob_patch(blob, ir->arg2.label, jit_x86_patch);
}

static void jit_x86_macro_exp(code_blob_t *blob, jit_ir_t *ir,
                              const phys_slot_t *slots)
{
   jit_x86_get_copy(blob, __EDI, ir->arg1, slots);
   jit_x86_get_copy(blob, __ECX, ir->arg2, slots);

   TEST(__ECX, __ECX, __QWORD);
   MOV(__EAX, IMM(1), __DWORD);
   JZ(IMM(18));
   TEST(__ECX, IMM(1), __BYTE);
   JZ(IMM(4));
   IMUL(__EAX, __EDI, __QWORD);
   IMUL(__EDI, __EDI, __QWORD);
   SAR(__ECX, IMM(1), __QWORD);
   JNZ(IMM(-18));

   jit_x86_put(blob, ir->result, __EAX, slots);
}

static void jit_x86_macro_fexp(code_blob_t *blob, jit_x86_state_t *state,
                               jit_ir_t *ir, const phys_slot_t *slots)
{
   jit_x86_get_copy(blob, __XMM0, ir->arg1, slots);
   jit_x86_get_copy(blob, __XMM1, ir->arg2, slots);

   CALL(PTR(state->stubs[FEXP_STUB]));

   jit_x86_put(blob, ir->result, __XMM0, slots);
}

static void jit_x86_macro_trim(code_blob_t *blob, jit_ir_t *ir)
{
   const ptrdiff_t off = offsetof(jit_anchor_t, watermark);
   MOV(__EAX, ADDR(__EBP, ANCHOR_OFFSET + off), __DWORD);
   MOV(ADDR(TLAB_REG, offsetof(tlab_t, alloc)), __EAX, __DWORD);
}

static void jit_x86_op(code_blob_t *blob, jit_x86_state_t *state, jit_ir_t *ir,
                       const phys_slot_t *slots)
{
   switch (ir->op) {
   case J_RECV:
      jit_x86_recv(blob, ir, slots);
      break;
   case J_SEND:
      jit_x86_send(blob, ir, slots);
      break;
   case J_ADD:
      jit_x86_add(blob, ir, slots);
      break;
   case J_SUB:
      jit_x86_sub(blob, ir, slots);
      break;
   case J_MUL:
      jit_x86_mul(blob, ir, slots);
      break;
   case J_REM:
      jit_x86_rem(blob, ir, slots);
      break;
   case J_DIV:
      jit_x86_div(blob, ir, slots);
      break;
   case J_NEG:
      jit_x86_neg(blob, ir, slots);
      break;
   case J_CLAMP:
      jit_x86_clamp(blob, ir, slots);
      break;
   case J_MOV:
      jit_x86_mov(blob, ir, slots);
      break;
   case J_NOT:
      jit_x86_not(blob, ir, slots);
      break;
   case J_AND:
      jit_x86_and(blob, ir, slots);
      break;
   case J_OR:
      jit_x86_or(blob, ir, slots);
      break;
   case J_XOR:
      jit_x86_xor(blob, ir, slots);
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
      jit_x86_load(blob, ir, slots);
      break;
   case J_ULOAD:
      jit_x86_uload(blob, ir, slots);
      break;
   case J_STORE:
      jit_x86_store(blob, ir, slots);
      break;
   case J_LEA:
      jit_x86_lea(blob, ir, slots);
      break;
   case J_CMP:
      jit_x86_cmp(blob, ir, slots);
      break;
   case J_CCMP:
      jit_x86_ccmp(blob, ir, slots);
      break;
   case J_CSET:
      jit_x86_cset(blob, ir, slots);
      break;
   case J_CSEL:
      jit_x86_csel(blob, ir, slots);
      break;
   case J_CALL:
      jit_x86_call(blob, state, ir);
      break;
   case J_SHL:
      jit_x86_shl(blob, ir, slots);
      break;
   case J_ASR:
      jit_x86_asr(blob, ir, slots);
      break;
   case J_TRAP:
      jit_x86_trap(blob, ir);
      break;
   case J_FMUL:
      jit_x86_fmul(blob, ir, slots);
      break;
   case J_FADD:
      jit_x86_fadd(blob, ir, slots);
      break;
   case J_FSUB:
      jit_x86_fsub(blob, ir, slots);
      break;
   case J_FDIV:
      jit_x86_fdiv(blob, ir, slots);
      break;
   case J_FNEG:
      jit_x86_fneg(blob, ir, slots);
      break;
   case J_FCMP:
      jit_x86_fcmp(blob, ir, slots);
      break;
   case J_FCVTNS:
      jit_x86_fcvtns(blob, state, ir, slots);
      break;
   case J_SCVTF:
      jit_x86_scvtf(blob, ir, slots);
      break;
   case MACRO_EXIT:
      jit_x86_macro_exit(blob, state, ir);
      break;
   case MACRO_SALLOC:
      jit_x86_macro_salloc(blob, ir, slots);
      break;
   case MACRO_LALLOC:
      jit_x86_macro_lalloc(blob, state, ir, slots);
      break;
   case MACRO_GALLOC:
      jit_x86_macro_galloc(blob, state, ir, slots);
      break;
   case MACRO_BZERO:
      jit_x86_macro_bzero(blob, ir, slots);
      break;
   case MACRO_COPY:
      jit_x86_macro_copy(blob, ir, slots);
      break;
   case MACRO_MOVE:
      jit_x86_macro_move(blob, ir, slots);
      break;
   case MACRO_MEMSET:
      jit_x86_macro_memset(blob, ir, slots);
      break;
   case MACRO_GETPRIV:
      jit_x86_macro_getpriv(blob, ir, slots);
      break;
   case MACRO_PUTPRIV:
      jit_x86_macro_putpriv(blob, ir, slots);
      break;
   case MACRO_CASE:
      jit_x86_macro_case(blob, ir, slots);
      break;
   case MACRO_EXP:
      jit_x86_macro_exp(blob, ir, slots);
      break;
   case MACRO_FEXP:
      jit_x86_macro_fexp(blob, state, ir, slots);
      break;
   case MACRO_TRIM:
      jit_x86_macro_trim(blob, ir);
      break;
   default:
      jit_dump_with_mark(blob->func, ir - blob->func->irbuf);
      fatal_trace("unhandled opcode %s in x86 backend", jit_op_name(ir->op));
   }
}

static void jit_x86_cgen(jit_t *j, jit_handle_t handle, void *context)
{
   jit_x86_state_t *state = context;

   jit_func_t *f = jit_get_func(j, handle);

#ifdef DEBUG
   const char *only = getenv("NVC_JIT_ONLY");
   if (only != NULL && !icmp(f->name, only))
      return;
#endif

   code_blob_t *blob = code_blob_new(state->code, f->name, 0);
   if (blob == NULL)
      return;

   blob->func = f;

   const uint64_t allowmask = (1 << __R10.reg) | (1 << __R11.reg);

   phys_slot_t *slots LOCAL = xmalloc_array(f->nregs, sizeof(phys_slot_t));
   const int spills = jit_do_lscan(f, slots, ~allowmask);

   PUSH(__EBP);
   MOV(__EBP, __ESP, __QWORD);

   // Frame layout
   //
   //       |-------------------|
   //    +8 | Caller's PC       |
   //       |-------------------|
   //     0 | Saved RBP         |    <--- RBP
   //       |-------------------|
   //    -4 | TLAB watermark    |
   //    -8 | IR position       |
   //   -16 | Function pointer  |
   //   -24 | Caller's anchor   |    <--- Frame anchor
   //       |-------------------|
   //   -32 | Saved RBX         |
   //       | Saved RDI (Win)   |
   //       | Saved RSI (Win)   |
   //       | Saved R12         |
   //       | Saved R13         |
   //       | Saved R14         |
   //   -80 | Saved R15         |
   //       |-------------------|    <--- End of fixed frame
   //       | Local variables   |
   //       .                   .
   //       |-------------------|
   //       | Spill slots       |
   //       .                   .
   //       |-------------------|    <--- RSP

   const size_t framebytes =
      f->framesz + spills * sizeof(int64_t) + FRAME_FIXED_SIZE;
   const size_t framesz = ALIGN_UP(framebytes, 16);
   SUB(__ESP, IMM(framesz*2), __QWORD);

   // Callee saves
   MOV(ADDR(__EBP, -32), __EBX, __QWORD);
#ifdef __MINGW32__
   MOV(ADDR(__EBP, -40), __EDI, __QWORD);
   MOV(ADDR(__EBP, -48), __ESI, __QWORD);
#endif
#if 0
   // Not currently used
   MOV(ADDR(__EBP, -56), __R12, __QWORD);
   MOV(ADDR(__EBP, -64), __R13, __QWORD);
   MOV(ADDR(__EBP, -72), __R14, __QWORD);
   MOV(ADDR(__EBP, -80), __R15, __QWORD);
#endif

   XOR(FLAGS_REG, FLAGS_REG, __DWORD);

   // Shuffle incoming arguments
   MOV(ARGS_REG, CARG2_REG, __QWORD);
   MOV(TLAB_REG, CARG3_REG, __QWORD);

   // Build frame anchor
   MOV(ADDR(__EBP, -24), CARG1_REG, __QWORD);
   MOV(ADDR(__EBP, -16), CARG0_REG, __QWORD);
   MOV(ADDR(__EBP, -8), FLAGS_REG, __DWORD);
   MOV(__EAX, ADDR(TLAB_REG, offsetof(tlab_t, alloc)), __DWORD);
   MOV(ADDR(__EBP, -4), __EAX, __DWORD);

   STATIC_ASSERT(ANCHOR_OFFSET == -24);

   for (int i = 0; i < f->nirs; i++) {
      if (f->irbuf[i].target)
         code_blob_mark(blob, i);
      code_blob_print_ir(blob, &(f->irbuf[i]));
      jit_x86_op(blob, state, &(f->irbuf[i]), slots);
   }

   code_blob_mark(blob, JIT_LABEL_INVALID);

   MOV(__EBX, ADDR(__EBP, -32), __QWORD);
#ifdef __MINGW32__
   MOV(__EDI, ADDR(__EBP, -40), __QWORD);
   MOV(__ESI, ADDR(__EBP, -48), __QWORD);
#endif
#if 0
   MOV(__R12, ADDR(__EBP, -56), __QWORD);
   MOV(__R13, ADDR(__EBP, -64), __QWORD);
   MOV(__R14, ADDR(__EBP, -72), __QWORD);
   MOV(__R15, ADDR(__EBP, -80), __QWORD);
#endif

   LEAVE();
   RET();

   code_blob_finalise(blob, &(f->entry));
}

static void jit_x86_gen_exit_stub(jit_x86_state_t *state)
{
   ident_t name = ident_new("exit stub");
   code_blob_t *blob = code_blob_new(state->code, name, 0);

   SUB(__ESP, IMM(8), __QWORD);   // Ensure stack aligned

   jit_x86_push_call_clobbered(blob);

   // Exit number in EAX
   MOV(CARG0_REG, __EAX, __DWORD);
   LEA(CARG1_REG, ADDR(__EBP, ANCHOR_OFFSET));
   MOV(CARG2_REG, ARGS_REG, __QWORD);
   MOV(CARG3_REG, TLAB_REG, __QWORD);

   MOV(__EAX, PTR(__nvc_do_exit), __QWORD);
   CALL(__EAX);

   jit_x86_pop_call_clobbered(blob);

   ADD(__ESP, IMM(8), __QWORD);
   RET();

   code_blob_finalise(blob, &(state->stubs[EXIT_STUB]));
}

static void jit_x86_gen_call_stub(jit_x86_state_t *state)
{
   ident_t name = ident_new("call stub");
   code_blob_t *blob = code_blob_new(state->code, name, 0);

   SUB(__ESP, IMM(8), __QWORD);   // Ensure stack aligned

   jit_x86_push_call_clobbered(blob);

   MOV(CARG0_REG, __EAX, __QWORD);
   LEA(CARG1_REG, ADDR(__EBP, ANCHOR_OFFSET));
   MOV(CARG2_REG, ARGS_REG, __QWORD);
   MOV(CARG3_REG, TLAB_REG, __QWORD);

   MOV(__EAX, ADDR(__EAX, offsetof(jit_func_t, entry)), __QWORD);
   CALL(__EAX);

   jit_x86_pop_call_clobbered(blob);

   ADD(__ESP, IMM(8), __QWORD);
   RET();

   code_blob_finalise(blob, &(state->stubs[CALL_STUB]));
}

static void jit_x86_gen_alloc_stub(jit_x86_state_t *state)
{
   ident_t name = ident_new("alloc stub");
   code_blob_t *blob = code_blob_new(state->code, name, 0);

   SUB(__ESP, IMM(8), __QWORD);   // Ensure stack aligned

   jit_x86_push_call_clobbered(blob);

   // Size in EAX
   MOV(CARG0_REG, __EAX, __DWORD);
   LEA(CARG1_REG, ADDR(__EBP, ANCHOR_OFFSET));

   MOV(__EAX, PTR(__nvc_mspace_alloc), __QWORD);
   CALL(__EAX);

   jit_x86_pop_call_clobbered(blob);

   ADD(__ESP, IMM(8), __QWORD);
   RET();

   code_blob_finalise(blob, &(state->stubs[ALLOC_STUB]));
}

static void jit_x86_gen_tlab_stub(jit_x86_state_t *state)
{
   ident_t name = ident_new("tlab stub");
   code_blob_t *blob = code_blob_new(state->code, name, 0);

   // Fast path: allocate from TLAB

   MOV(__ECX, ADDR(TLAB_REG, offsetof(tlab_t, alloc)), __DWORD);
   MOV(__EDI, __ECX, __DWORD);
   ADD(__EDI, __EAX, __DWORD);
   ADD(__EDI, IMM(RT_ALIGN_MASK), __DWORD);
   AND(__EDI, IMM(~RT_ALIGN_MASK), __DWORD);

   CMP(ADDR(TLAB_REG, offsetof(tlab_t, limit)), __EDI, __DWORD);
   JLT(IMM(12));

   MOV(ADDR(TLAB_REG, offsetof(tlab_t, alloc)), __EDI, __DWORD);
   LEA(__EAX, ADDR(TLAB_REG, offsetof(tlab_t, data)));
   ADD(__EAX, __ECX, __QWORD);
   RET();

   // Slow path: call into runtime

   SUB(__ESP, IMM(8), __QWORD);   // Ensure stack aligned

   jit_x86_push_call_clobbered(blob);

   MOV(CARG0_REG, __EAX, __DWORD);
   LEA(CARG1_REG, ADDR(__EBP, ANCHOR_OFFSET));

   MOV(__EAX, PTR(__nvc_mspace_alloc), __QWORD);
   CALL(__EAX);

   jit_x86_pop_call_clobbered(blob);

   ADD(__ESP, IMM(8), __QWORD);
   RET();

   code_blob_finalise(blob, &(state->stubs[TLAB_STUB]));
}

#if DEBUG
static void jit_x86_gen_debug_stub(jit_x86_state_t *state)
{
   ident_t name = ident_new("debug stub");
   code_blob_t *blob = code_blob_new(state->code, name, 0);

   SUB(__ESP, IMM(8), __QWORD);   // Ensure stack aligned

   jit_x86_push_call_clobbered(blob);

   MOV(CARG0_REG, __EAX, __QWORD);    // Value
   MOV(CARG1_REG, __ECX, __DWORD);    // Register number

   MOV(__EAX, PTR(_debug_out), __QWORD);
   CALL(__EAX);

   jit_x86_pop_call_clobbered(blob);

   ADD(__ESP, IMM(8), __QWORD);
   RET();

   code_blob_finalise(blob, &(state->stubs[DEBUG_STUB]));
}
#endif

static void jit_x86_gen_fexp_stub(jit_x86_state_t *state)
{
   ident_t name = ident_new("fexp stub");
   code_blob_t *blob = code_blob_new(state->code, name, 0);

   SUB(__ESP, IMM(8), __QWORD);   // Ensure stack aligned

   jit_x86_push_call_clobbered(blob);

   MOV(__EAX, PTR(pow), __QWORD);
   CALL(__EAX);

   jit_x86_pop_call_clobbered(blob);

   ADD(__ESP, IMM(8), __QWORD);
   RET();

   code_blob_finalise(blob, &(state->stubs[FEXP_STUB]));
}

static void jit_x86_gen_round_stub(jit_x86_state_t *state)
{
   ident_t name = ident_new("round stub");
   code_blob_t *blob = code_blob_new(state->code, name, 0);

   SUB(__ESP, IMM(8), __QWORD);   // Ensure stack aligned

   jit_x86_push_call_clobbered(blob);

   MOV(__EAX, PTR(round), __QWORD);
   CALL(__EAX);

   jit_x86_pop_call_clobbered(blob);

   ADD(__ESP, IMM(8), __QWORD);
   RET();

   code_blob_finalise(blob, &(state->stubs[ROUND_STUB]));
}

static void *jit_x86_init(jit_t *jit)
{
   jit_x86_state_t *state = xcalloc(sizeof(jit_x86_state_t));
   state->jit  = jit;
   state->code = code_cache_new();

   jit_x86_gen_exit_stub(state);
   jit_x86_gen_call_stub(state);
   jit_x86_gen_alloc_stub(state);
   jit_x86_gen_tlab_stub(state);
   jit_x86_gen_fexp_stub(state);
   DEBUG_ONLY(jit_x86_gen_debug_stub(state));

   if (!__builtin_cpu_supports("sse4.1"))
      jit_x86_gen_round_stub(state);

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
