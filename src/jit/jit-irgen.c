//
//  Copyright (C) 2022-2025  Nick Gasson
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
#include "cov/cov-api.h"
#include "diag.h"
#include "ident.h"
#include "jit/jit-ffi.h"
#include "jit/jit-ffi.h"
#include "jit/jit-priv.h"
#include "mask.h"
#include "mir/mir-node.h"
#include "mir/mir-unit.h"
#include "option.h"
#include "rt/rt.h"
#include "tree.h"

#include <assert.h>
#include <inttypes.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>

typedef struct _irgen_label irgen_label_t;
typedef struct _patch_list  patch_list_t;

#define PATCH_CHUNK_SZ  4
#define MAX_STACK_ALLOC 65536
#define DEDUP_PREFIX    16384

struct _patch_list {
   patch_list_t *next;
   unsigned      count;
   unsigned      offsets[PATCH_CHUNK_SZ];
};

struct _irgen_label {
   jit_label_t    label;
   unsigned       uses;
   patch_list_t   patchlist;
   irgen_label_t *next;
};

typedef struct {
   jit_func_t     *func;
   jit_value_t    *vars;
   jit_value_t    *params;
   jit_reg_t       next_reg;
   jit_value_t    *map;
   irgen_label_t  *labels;
   irgen_label_t **blocks;
   mir_unit_t     *mu;
   size_t          cpoolptr;
   size_t          oldptr;
   jit_value_t     statereg;
   jit_value_t     contextarg;
   mir_value_t     flags;
   mir_block_t     curblock;
   unsigned        bufsz;
   bool            used_tlab;
   bool            stateless;
   bool            needs_context;
} jit_irgen_t;

////////////////////////////////////////////////////////////////////////////////
// IR emission

static inline jit_value_t jit_value_from_reg(jit_reg_t reg)
{
   jit_value_t value = { .kind = JIT_VALUE_REG, .reg = reg };
   return value;
}

static inline jit_value_t jit_value_from_int64(int64_t immed)
{
   jit_value_t value = { .kind = JIT_VALUE_INT64, .int64 = immed };
   return value;
}

static inline jit_value_t jit_value_from_double(double immed)
{
   return (jit_value_t){ .kind = JIT_VALUE_DOUBLE, .dval = immed };
}

static inline jit_value_t jit_value_from_cpool_addr(int offset)
{
   jit_value_t value = { .kind = JIT_ADDR_CPOOL, .int64 = offset };
   return value;
}

static inline jit_value_t jit_value_from_loc(const loc_t *loc)
{
   jit_value_t value = { .kind = JIT_VALUE_LOC, .loc = *loc };
   return value;
}

static inline jit_value_t jit_null_ptr(void)
{
   jit_value_t value = { .kind = JIT_ADDR_ABS, .int64 = 0 };
   return value;
}

static inline jit_reg_t jit_value_as_reg(jit_value_t value)
{
   assert(value.kind == JIT_VALUE_REG);
   return value.reg;
}

static inline bool jit_value_is_addr(jit_value_t value)
{
   switch (value.kind) {
   case JIT_ADDR_ABS:
   case JIT_ADDR_CPOOL:
   case JIT_ADDR_REG:
      return true;
   default:
      return false;
   }
}

static jit_value_t jit_addr_from_value(jit_value_t value, int32_t disp)
{
   switch (value.kind) {
   case JIT_VALUE_REG:
      return (jit_value_t){
         .kind = JIT_ADDR_REG,
         .disp = disp,
         .reg = value.reg
      };
   case JIT_VALUE_INT64:
   case JIT_ADDR_ABS:
      return (jit_value_t){
         .kind = JIT_ADDR_ABS,
         .int64 = value.int64 + disp
      };
   case JIT_ADDR_CPOOL:
      return (jit_value_t){
         .kind = value.kind,
         .int64 = value.int64 + disp
      };
   case JIT_ADDR_REG:
      return (jit_value_t){
         .kind = JIT_ADDR_REG,
         .disp = value.disp + disp,
         .reg = value.reg
      };
   default:
      fatal_trace("cannot convert value kind %d to address", value.kind);
   }
}

static jit_value_t jit_addr_from_cover_tag(uint32_t tag)
{
   return (jit_value_t){
      .kind = JIT_ADDR_COVER,
      .int64 = tag,
   };
}

static jit_value_t jit_value_from_label(irgen_label_t *l)
{
   return (jit_value_t){ .kind = JIT_VALUE_LABEL, .label = l->label };
}

static jit_value_t jit_value_from_handle(jit_handle_t handle)
{
   return (jit_value_t){ .kind = JIT_VALUE_HANDLE, .handle = handle };
}

static jit_value_t jit_value_from_exit(jit_exit_t exit)
{
   return (jit_value_t){ .kind = JIT_VALUE_EXIT, .exit = exit };
}

static jit_ir_t *irgen_append(jit_irgen_t *g)
{
   if (g->func->nirs == g->bufsz) {
      g->bufsz = MAX(g->bufsz * 2, 1024);
      g->func->irbuf = xrealloc_array(g->func->irbuf, g->bufsz,
                                      sizeof(jit_ir_t));
   }

   return &(g->func->irbuf[g->func->nirs++]);
}

static jit_reg_t irgen_alloc_reg(jit_irgen_t *g)
{
   if (unlikely(g->next_reg == JIT_REG_INVALID))
      fatal("unit %s is too big to compile", istr(g->func->name));
   else
      return g->next_reg++;
}

static irgen_label_t *irgen_alloc_label(jit_irgen_t *g)
{
   irgen_label_t *l = xcalloc(sizeof(irgen_label_t));
   l->next  = g->labels;
   l->label = JIT_LABEL_INVALID;

   return (g->labels = l);
}

static void irgen_bind_label(jit_irgen_t *g, irgen_label_t *l)
{
   assert(l->label == JIT_LABEL_INVALID);

   l->label = g->func->nirs;

   for (patch_list_t *p = &(l->patchlist); p; p = p->next) {
      for (int i = 0; i < p->count; i++) {
         jit_ir_t *ir = &(g->func->irbuf[p->offsets[i]]);
         if (ir->arg1.kind == JIT_VALUE_LABEL)
            ir->arg1.label = l->label;
         else if (ir->arg2.kind == JIT_VALUE_LABEL)
            ir->arg2.label = l->label;
         else
            fatal_trace("cannot find label argument to patch");
      }
   }

   for (patch_list_t *p = l->patchlist.next, *tmp; p; p = tmp) {
      tmp = p->next;
      free(p);
   }
}

static jit_cc_t irgen_get_jit_cc(mir_cmp_t cmp)
{
   switch (cmp) {
   case MIR_CMP_EQ: return JIT_CC_EQ;
   case MIR_CMP_NEQ: return JIT_CC_NE;
   case MIR_CMP_LT: return JIT_CC_LT;
   case MIR_CMP_GT: return JIT_CC_GT;
   case MIR_CMP_LEQ: return JIT_CC_LE;
   case MIR_CMP_GEQ: return JIT_CC_GE;
   default: return JIT_CC_NONE;
   }
}

static void irgen_emit_nullary(jit_irgen_t *g, jit_op_t op, jit_cc_t cc,
                               jit_reg_t result)
{
   jit_ir_t *ir = irgen_append(g);
   ir->op        = op;
   ir->size      = JIT_SZ_UNSPEC;
   ir->cc        = cc;
   ir->target    = 0;
   ir->result    = result;
   ir->arg1.kind = JIT_VALUE_INVALID;
   ir->arg2.kind = JIT_VALUE_INVALID;
}

static jit_ir_t *irgen_emit_unary(jit_irgen_t *g, jit_op_t op, jit_size_t sz,
                                  jit_cc_t cc, jit_reg_t result,
                                  jit_value_t arg)
{
   jit_ir_t *ir = irgen_append(g);
   ir->op        = op;
   ir->size      = sz;
   ir->cc        = cc;
   ir->target    = 0;
   ir->result    = result;
   ir->arg1      = arg;
   ir->arg2.kind = JIT_VALUE_INVALID;

   return ir;
}

static jit_ir_t *irgen_emit_binary(jit_irgen_t *g, jit_op_t op, jit_size_t sz,
                                   jit_cc_t cc, jit_reg_t result,
                                   jit_value_t arg1, jit_value_t arg2)
{
   jit_ir_t *ir = irgen_append(g);
   ir->op     = op;
   ir->size   = sz;
   ir->cc     = cc;
   ir->target = 0;
   ir->size   = sz;
   ir->result = result;
   ir->arg1   = arg1;
   ir->arg2   = arg2;

   return ir;
}

static void irgen_patch_label(jit_irgen_t *g, jit_ir_t *ir, irgen_label_t *l)
{
   if (l->label == JIT_LABEL_INVALID) {
      patch_list_t *p = &(l->patchlist);
      for (; p->count == PATCH_CHUNK_SZ; p = p->next) {
         if (p->next == NULL)
            p->next = xcalloc(sizeof(patch_list_t));
      }
      p->offsets[p->count++] = ir - g->func->irbuf;
   }

   l->uses++;
}

static jit_value_t j_recv(jit_irgen_t *g, int pos)
{
   assert(pos < JIT_MAX_ARGS);
   jit_reg_t r = irgen_alloc_reg(g);
   irgen_emit_unary(g, J_RECV, JIT_SZ_UNSPEC, JIT_CC_NONE, r,
                    jit_value_from_int64(pos));
   return jit_value_from_reg(r);
}

static void j_send(jit_irgen_t *g, int pos, jit_value_t value)
{
   assert(pos < JIT_MAX_ARGS);
   irgen_emit_binary(g, J_SEND, JIT_SZ_UNSPEC, JIT_CC_NONE, JIT_REG_INVALID,
                     jit_value_from_int64(pos), value);
}

static void j_ret(jit_irgen_t *g)
{
   irgen_emit_nullary(g, J_RET, JIT_CC_NONE, JIT_REG_INVALID);
   g->flags = MIR_NULL_VALUE;
}

__attribute__((unused))
static void j_trap(jit_irgen_t *g)
{
   irgen_emit_nullary(g, J_TRAP, JIT_CC_NONE, JIT_REG_INVALID);
}

static void j_jump(jit_irgen_t *g, jit_cc_t cc, irgen_label_t *l)
{
   jit_ir_t *ir = irgen_emit_unary(g, J_JUMP, JIT_SZ_UNSPEC, cc,
                                   JIT_REG_INVALID, jit_value_from_label(l));
   irgen_patch_label(g, ir, l);
}

static void j_call(jit_irgen_t *g, jit_handle_t handle)
{
   irgen_emit_unary(g, J_CALL, JIT_SZ_UNSPEC, JIT_CC_NONE,
                    JIT_REG_INVALID, jit_value_from_handle(handle));
   g->flags = MIR_NULL_VALUE;
}

static jit_value_t j_neg(jit_irgen_t *g, jit_value_t arg)
{
   jit_reg_t r = irgen_alloc_reg(g);
   irgen_emit_unary(g, J_NEG, JIT_SZ_UNSPEC, JIT_CC_NONE, r, arg);
   return jit_value_from_reg(r);
}

static jit_value_t j_fneg(jit_irgen_t *g, jit_value_t arg)
{
   jit_reg_t r = irgen_alloc_reg(g);
   irgen_emit_unary(g, J_FNEG, JIT_SZ_UNSPEC, JIT_CC_NONE, r, arg);
   return jit_value_from_reg(r);
}

static jit_value_t j_rem(jit_irgen_t *g, jit_value_t lhs, jit_value_t rhs)
{
   jit_reg_t r = irgen_alloc_reg(g);
   irgen_emit_binary(g, J_REM, JIT_SZ_UNSPEC, JIT_CC_NONE, r, lhs, rhs);
   return jit_value_from_reg(r);
}

static jit_value_t j_not(jit_irgen_t *g, jit_value_t arg)
{
   jit_reg_t r = irgen_alloc_reg(g);
   irgen_emit_unary(g, J_NOT, JIT_SZ_UNSPEC, JIT_CC_NONE, r, arg);
   return jit_value_from_reg(r);
}

static jit_value_t j_and(jit_irgen_t *g, jit_value_t lhs, jit_value_t rhs)
{
   jit_reg_t r = irgen_alloc_reg(g);
   irgen_emit_binary(g, J_AND, JIT_SZ_UNSPEC, JIT_CC_NONE, r, lhs, rhs);
   return jit_value_from_reg(r);
}

static jit_value_t j_or(jit_irgen_t *g, jit_value_t lhs, jit_value_t rhs)
{
   jit_reg_t r = irgen_alloc_reg(g);
   irgen_emit_binary(g, J_OR, JIT_SZ_UNSPEC, JIT_CC_NONE, r, lhs, rhs);
   return jit_value_from_reg(r);
}

static jit_value_t j_xor(jit_irgen_t *g, jit_value_t lhs, jit_value_t rhs)
{
   jit_reg_t r = irgen_alloc_reg(g);
   irgen_emit_binary(g, J_XOR, JIT_SZ_UNSPEC, JIT_CC_NONE, r, lhs, rhs);
   return jit_value_from_reg(r);
}

static jit_value_t j_asr(jit_irgen_t *g, jit_value_t lhs, jit_value_t rhs)
{
   jit_reg_t r = irgen_alloc_reg(g);
   irgen_emit_binary(g, J_ASR, JIT_SZ_UNSPEC, JIT_CC_NONE, r, lhs, rhs);
   return jit_value_from_reg(r);
}

static jit_value_t j_shl(jit_irgen_t *g, jit_value_t lhs, jit_value_t rhs)
{
   jit_reg_t r = irgen_alloc_reg(g);
   irgen_emit_binary(g, J_SHL, JIT_SZ_UNSPEC, JIT_CC_NONE, r, lhs, rhs);
   return jit_value_from_reg(r);
}

static jit_value_t j_shr(jit_irgen_t *g, jit_value_t lhs, jit_value_t rhs)
{
   jit_reg_t r = irgen_alloc_reg(g);
   irgen_emit_binary(g, J_SHR, JIT_SZ_UNSPEC, JIT_CC_NONE, r, lhs, rhs);
   return jit_value_from_reg(r);
}

static jit_value_t j_add(jit_irgen_t *g, jit_value_t lhs, jit_value_t rhs)
{
   jit_reg_t r = irgen_alloc_reg(g);
   irgen_emit_binary(g, J_ADD, JIT_SZ_UNSPEC, JIT_CC_NONE, r, lhs, rhs);
   return jit_value_from_reg(r);
}

static jit_value_t j_adds(jit_irgen_t *g, jit_size_t sz, jit_cc_t cc,
                          jit_value_t lhs, jit_value_t rhs)
{
   jit_reg_t r = irgen_alloc_reg(g);
   irgen_emit_binary(g, J_ADD, sz, cc, r, lhs, rhs);
   g->flags = MIR_NULL_VALUE;
   return jit_value_from_reg(r);
}

static jit_value_t j_fadd(jit_irgen_t *g, jit_value_t lhs, jit_value_t rhs)
{
   jit_reg_t r = irgen_alloc_reg(g);
   irgen_emit_binary(g, J_FADD, JIT_SZ_UNSPEC, JIT_CC_NONE, r, lhs, rhs);
   return jit_value_from_reg(r);
}

static jit_value_t j_mul(jit_irgen_t *g, jit_value_t lhs, jit_value_t rhs)
{
   jit_reg_t r = irgen_alloc_reg(g);
   irgen_emit_binary(g, J_MUL, JIT_SZ_UNSPEC, JIT_CC_NONE, r, lhs, rhs);
   return jit_value_from_reg(r);
}

static jit_value_t j_muls(jit_irgen_t *g, jit_size_t sz, jit_cc_t cc,
                          jit_value_t lhs, jit_value_t rhs)
{
   jit_reg_t r = irgen_alloc_reg(g);
   irgen_emit_binary(g, J_MUL, sz, cc, r, lhs, rhs);
   g->flags = MIR_NULL_VALUE;
   return jit_value_from_reg(r);
}

static jit_value_t j_fmul(jit_irgen_t *g, jit_value_t lhs, jit_value_t rhs)
{
   jit_reg_t r = irgen_alloc_reg(g);
   irgen_emit_binary(g, J_FMUL, JIT_SZ_UNSPEC, JIT_CC_NONE, r, lhs, rhs);
   return jit_value_from_reg(r);
}

static jit_value_t j_div(jit_irgen_t *g, jit_value_t lhs, jit_value_t rhs)
{
   jit_reg_t r = irgen_alloc_reg(g);
   irgen_emit_binary(g, J_DIV, JIT_SZ_UNSPEC, JIT_CC_NONE, r, lhs, rhs);
   return jit_value_from_reg(r);
}

static jit_value_t j_fdiv(jit_irgen_t *g, jit_value_t lhs, jit_value_t rhs)
{
   jit_reg_t r = irgen_alloc_reg(g);
   irgen_emit_binary(g, J_FDIV, JIT_SZ_UNSPEC, JIT_CC_NONE, r, lhs, rhs);
   return jit_value_from_reg(r);
}

static jit_value_t j_sub(jit_irgen_t *g, jit_value_t lhs, jit_value_t rhs)
{
   jit_reg_t r = irgen_alloc_reg(g);
   irgen_emit_binary(g, J_SUB, JIT_SZ_UNSPEC, JIT_CC_NONE, r, lhs, rhs);
   return jit_value_from_reg(r);
}

static jit_value_t j_subs(jit_irgen_t *g, jit_size_t sz, jit_cc_t cc,
                          jit_value_t lhs, jit_value_t rhs)
{
   jit_reg_t r = irgen_alloc_reg(g);
   irgen_emit_binary(g, J_SUB, sz, cc, r, lhs, rhs);
   g->flags = MIR_NULL_VALUE;
   return jit_value_from_reg(r);
}

static jit_value_t j_fsub(jit_irgen_t *g, jit_value_t lhs, jit_value_t rhs)
{
   jit_reg_t r = irgen_alloc_reg(g);
   irgen_emit_binary(g, J_FSUB, JIT_SZ_UNSPEC, JIT_CC_NONE, r, lhs, rhs);
   return jit_value_from_reg(r);
}

static void j_cmp(jit_irgen_t *g, jit_cc_t cc, jit_value_t lhs, jit_value_t rhs)
{
   irgen_emit_binary(g, J_CMP, JIT_SZ_UNSPEC, cc, JIT_REG_INVALID, lhs, rhs);
   g->flags = MIR_NULL_VALUE;
}

static void j_ccmp(jit_irgen_t *g, jit_cc_t cc, jit_value_t lhs, jit_value_t rhs)
{
   irgen_emit_binary(g, J_CCMP, JIT_SZ_UNSPEC, cc, JIT_REG_INVALID, lhs, rhs);
   g->flags = MIR_NULL_VALUE;
}

static void j_fcmp(jit_irgen_t *g, jit_cc_t cc, jit_value_t lhs,
                   jit_value_t rhs)
{
   irgen_emit_binary(g, J_FCMP, JIT_SZ_UNSPEC, cc, JIT_REG_INVALID, lhs, rhs);
   g->flags = MIR_NULL_VALUE;
}

static void j_fccmp(jit_irgen_t *g, jit_cc_t cc, jit_value_t lhs,
                    jit_value_t rhs)
{
   irgen_emit_binary(g, J_FCCMP, JIT_SZ_UNSPEC, cc, JIT_REG_INVALID, lhs, rhs);
   g->flags = MIR_NULL_VALUE;
}

static jit_value_t j_cset(jit_irgen_t *g)
{
   jit_reg_t r = irgen_alloc_reg(g);
   irgen_emit_nullary(g, J_CSET, JIT_CC_NONE, r);
   return jit_value_from_reg(r);
}

static jit_value_t j_uload(jit_irgen_t *g, jit_size_t sz, jit_value_t addr)
{
   assert(jit_value_is_addr(addr));
   jit_reg_t r = irgen_alloc_reg(g);
   irgen_emit_unary(g, J_ULOAD, sz, JIT_CC_NONE, r, addr);
   return jit_value_from_reg(r);
}

static jit_value_t j_load(jit_irgen_t *g, jit_size_t sz, jit_value_t addr)
{
   jit_reg_t r = irgen_alloc_reg(g);
   irgen_emit_unary(g, J_LOAD, sz, JIT_CC_NONE, r, addr);
   return jit_value_from_reg(r);
}

static void j_store(jit_irgen_t *g, jit_size_t sz, jit_value_t value,
                    jit_value_t addr)
{
   assert(!jit_value_is_addr(value));
   irgen_emit_binary(g, J_STORE, sz, JIT_CC_NONE, JIT_REG_INVALID, value, addr);
}

static void j_mov(jit_irgen_t *g, jit_reg_t dest, jit_value_t value)
{
   assert(!jit_value_is_addr(value));
   irgen_emit_unary(g, J_MOV, JIT_SZ_UNSPEC, JIT_CC_NONE, dest, value);
}

static jit_value_t j_lea(jit_irgen_t *g, jit_value_t addr)
{
   assert(jit_value_is_addr(addr));
   jit_reg_t r = irgen_alloc_reg(g);
   irgen_emit_unary(g, J_LEA, JIT_SZ_UNSPEC, JIT_CC_NONE, r, addr);
   return jit_value_from_reg(r);
}

static jit_value_t j_scvtf(jit_irgen_t *g, jit_value_t value)
{
   jit_reg_t r = irgen_alloc_reg(g);
   irgen_emit_unary(g, J_SCVTF, JIT_SZ_UNSPEC, JIT_CC_NONE, r, value);
   return jit_value_from_reg(r);
}

static jit_value_t j_fcvtns(jit_irgen_t *g, jit_value_t value)
{
   jit_reg_t r = irgen_alloc_reg(g);
   irgen_emit_unary(g, J_FCVTNS, JIT_SZ_UNSPEC, JIT_CC_NONE, r, value);
   return jit_value_from_reg(r);
}

static jit_value_t j_csel(jit_irgen_t *g, jit_value_t iftrue,
                          jit_value_t iffalse)
{
   jit_reg_t r = irgen_alloc_reg(g);
   irgen_emit_binary(g, J_CSEL, JIT_SZ_UNSPEC, JIT_CC_NONE, r, iftrue, iffalse);
   return jit_value_from_reg(r);
}

static jit_value_t j_clamp(jit_irgen_t *g, jit_value_t value)
{
   jit_reg_t r = irgen_alloc_reg(g);
   irgen_emit_unary(g, J_CLAMP, JIT_SZ_UNSPEC, JIT_CC_NONE, r, value);
   return jit_value_from_reg(r);
}

#if 0
static void macro_copy(jit_irgen_t *g, jit_value_t dest, jit_value_t src,
                       jit_reg_t count)
{
   irgen_emit_binary(g, MACRO_COPY, JIT_SZ_UNSPEC, JIT_CC_NONE,
                     count, dest, src);
}
#endif

static void macro_move(jit_irgen_t *g, jit_value_t dest, jit_value_t src,
                       jit_reg_t count)
{
   irgen_emit_binary(g, MACRO_MOVE, JIT_SZ_UNSPEC, JIT_CC_NONE,
                     count, dest, src);
}

static void macro_bzero(jit_irgen_t *g, jit_value_t dest, jit_reg_t count)
{
   assert(jit_value_is_addr(dest));
   irgen_emit_unary(g, MACRO_BZERO, JIT_SZ_UNSPEC, JIT_CC_NONE,
                    count, dest);
}

static void macro_memset(jit_irgen_t *g, jit_size_t sz, jit_value_t dest,
                         jit_value_t value, jit_reg_t count)
{
   assert(jit_value_is_addr(dest));
   irgen_emit_binary(g, MACRO_MEMSET, sz, JIT_CC_NONE, count, dest, value);
}

static jit_value_t macro_galloc(jit_irgen_t *g, jit_value_t bytes)
{
   jit_reg_t r = irgen_alloc_reg(g);
   irgen_emit_unary(g, MACRO_GALLOC, JIT_SZ_UNSPEC, JIT_CC_NONE, r, bytes);
   return jit_value_from_reg(r);
}

static jit_value_t macro_lalloc(jit_irgen_t *g, jit_value_t bytes)
{
   jit_reg_t r = irgen_alloc_reg(g);
   irgen_emit_unary(g, MACRO_LALLOC, JIT_SZ_UNSPEC, JIT_CC_NONE, r, bytes);
   return jit_value_from_reg(r);
}

static jit_value_t macro_salloc(jit_irgen_t *g, size_t size)
{
   jit_reg_t r = irgen_alloc_reg(g);
   jit_value_t arg1 = jit_value_from_int64(g->func->framesz);
   jit_value_t arg2 = jit_value_from_int64(size);
   irgen_emit_binary(g, MACRO_SALLOC, JIT_SZ_UNSPEC, JIT_CC_NONE,
                     r, arg1, arg2);
   g->func->framesz += ALIGN_UP(size, 8);
   return jit_value_from_reg(r);
}

static void macro_exit(jit_irgen_t *g, jit_exit_t exit)
{
   irgen_emit_unary(g, MACRO_EXIT, JIT_SZ_UNSPEC, JIT_CC_NONE, JIT_REG_INVALID,
                    jit_value_from_exit(exit));
   g->flags = MIR_NULL_VALUE;
}

static jit_value_t macro_fexp(jit_irgen_t *g, jit_value_t lhs, jit_value_t rhs)
{
   jit_reg_t r = irgen_alloc_reg(g);
   irgen_emit_binary(g, MACRO_FEXP, JIT_SZ_UNSPEC, JIT_CC_NONE, r, lhs, rhs);
   return jit_value_from_reg(r);
}

static jit_value_t macro_exp(jit_irgen_t *g, jit_size_t sz, jit_cc_t cc,
                             jit_value_t lhs, jit_value_t rhs)
{
   jit_reg_t r = irgen_alloc_reg(g);
   irgen_emit_binary(g, MACRO_EXP, sz, cc, r, lhs, rhs);
   return jit_value_from_reg(r);
}

static jit_value_t macro_getpriv(jit_irgen_t *g, jit_handle_t handle)
{
   jit_reg_t r = irgen_alloc_reg(g);
   irgen_emit_unary(g, MACRO_GETPRIV, JIT_SZ_UNSPEC, JIT_CC_NONE, r,
                    jit_value_from_handle(handle));
   return jit_value_from_reg(r);
}

static void macro_putpriv(jit_irgen_t *g, jit_handle_t handle, jit_value_t ptr)
{
   irgen_emit_binary(g, MACRO_PUTPRIV, JIT_SZ_UNSPEC, JIT_CC_NONE,
                     JIT_REG_INVALID, jit_value_from_handle(handle), ptr);
}

static void macro_case(jit_irgen_t *g, jit_reg_t test, jit_value_t cmp,
                       irgen_label_t *l)
{
   jit_ir_t *ir = irgen_emit_binary(g, MACRO_CASE, JIT_SZ_UNSPEC, JIT_CC_NONE,
                                    test, cmp, jit_value_from_label(l));
   irgen_patch_label(g, ir, l);
}

static void macro_trim(jit_irgen_t *g)
{
   irgen_emit_nullary(g, MACRO_TRIM, JIT_CC_NONE, JIT_REG_INVALID);
}

static void macro_reexec(jit_irgen_t *g)
{
   irgen_emit_nullary(g, MACRO_REEXEC, JIT_CC_NONE, JIT_REG_INVALID);
   g->flags = MIR_NULL_VALUE;
}

static void macro_sadd(jit_irgen_t *g, jit_size_t sz, jit_value_t addr,
                       jit_value_t addend)
{
   irgen_emit_binary(g, MACRO_SADD, sz, JIT_CC_NONE, JIT_REG_INVALID,
                     addr, addend);
}

static void macro_pack(jit_irgen_t *g, jit_value_t ptr, jit_value_t size)
{
   irgen_emit_binary(g, MACRO_PACK, JIT_SZ_UNSPEC, JIT_CC_NONE,
                     JIT_REG_INVALID, ptr, size);
}

static void macro_unpack(jit_irgen_t *g, jit_value_t abits, jit_value_t bbits)
{
   irgen_emit_binary(g, MACRO_UNPACK, JIT_SZ_UNSPEC, JIT_CC_NONE,
                     JIT_REG_INVALID, abits, bbits);
}

static void macro_vec4op(jit_irgen_t *g, jit_vec_op_t op, int size)
{
   irgen_emit_binary(g, MACRO_VEC4OP, JIT_SZ_UNSPEC, JIT_CC_NONE,
                     JIT_REG_INVALID, jit_value_from_int64(op),
                     jit_value_from_int64(size));
}

////////////////////////////////////////////////////////////////////////////////
// MIR to JIT IR lowering

static int irgen_slots_for_type(jit_irgen_t *g, mir_type_t type)
{
   if (mir_is_null(type))
      return 1;   // Untyped constants

   switch (mir_get_class(g->mu, type)) {
   case MIR_TYPE_UARRAY:
      // Always passed around scalarised
      if (mir_get_class(g->mu, mir_get_elem(g->mu, type)) == MIR_TYPE_SIGNAL)
         return 2 + mir_get_dims(g->mu, type) * 2;
      else
         return 1 + mir_get_dims(g->mu, type) * 2;
   case MIR_TYPE_SIGNAL:
      // Signal pointer plus offset
      return 2;
   case MIR_TYPE_CLOSURE:
      // Function pointer, context
      return 2;
   case MIR_TYPE_RESOLUTION:
      // Closure slots plus nlits, and flags (this is silly)
      return 4;
   case MIR_TYPE_VEC4:
      return 2;
   default:
      // Passed by pointer or fits in 64-bit register
      return 1;
   }
}

static int irgen_repr_bits(mir_repr_t repr)
{
   switch (repr) {
   case MIR_REPR_U1: return 1;
   case MIR_REPR_U8: case MIR_REPR_I8: return 8;
   case MIR_REPR_U16: case MIR_REPR_I16: return 16;
   case MIR_REPR_U32: case MIR_REPR_I32: return 32;
   case MIR_REPR_U64: case MIR_REPR_I64: return 64;
   default: return -1;
   }
}

static bool irgen_repr_signed(mir_repr_t repr)
{
   return repr == MIR_REPR_I8 || repr == MIR_REPR_I16
      || repr == MIR_REPR_I32 || repr == MIR_REPR_I64;
}

static int irgen_align_of(jit_irgen_t *g, mir_type_t type)
{
   switch (mir_get_class(g->mu, type)) {
   case MIR_TYPE_INT:
   case MIR_TYPE_OFFSET:
      {
         const int bits = irgen_repr_bits(mir_get_repr(g->mu, type));
         return ALIGN_UP(bits, 8) / 8;
      }
   case MIR_TYPE_REAL:
   case MIR_TYPE_RECORD:
      return sizeof(double);
   case MIR_TYPE_POINTER:
   case MIR_TYPE_ACCESS:
   case MIR_TYPE_UARRAY:
   case MIR_TYPE_SIGNAL:
   case MIR_TYPE_CONTEXT:
   case MIR_TYPE_TRIGGER:
   case MIR_TYPE_RESOLUTION:
      return sizeof(void *);
   case MIR_TYPE_VEC4:
   case MIR_TYPE_VEC2:
      return sizeof(uint64_t);
   case MIR_TYPE_FILE:
      return sizeof(uint32_t);
   case MIR_TYPE_CARRAY:
      return irgen_align_of(g, mir_get_elem(g->mu, type));
   default:
      fatal_trace("cannot handle type %d in irgen_align_of",
                  mir_get_class(g->mu, type));
   }
}

static int irgen_size_bits(jit_irgen_t *g, mir_type_t type)
{
   switch (mir_get_class(g->mu, type)) {
   case MIR_TYPE_INT:
   case MIR_TYPE_OFFSET:
      return irgen_repr_bits(mir_get_repr(g->mu, type));
   case MIR_TYPE_REAL:
      return sizeof(double) * 8;
   case MIR_TYPE_POINTER:
   case MIR_TYPE_ACCESS:
   case MIR_TYPE_CONTEXT:
   case MIR_TYPE_TRIGGER:
      return sizeof(void *) * 8;
   case MIR_TYPE_FILE:
      return sizeof(uint32_t) * 8;
   default:
      fatal_trace("cannot handle type %d in irgen_size_bits",
                  mir_get_class(g->mu, type));
   }
}

static int irgen_size_bytes(jit_irgen_t *g, mir_type_t type)
{
   switch (mir_get_class(g->mu, type)) {
   case MIR_TYPE_INT:
   case MIR_TYPE_OFFSET:
      {
         const int bits = irgen_size_bits(g, type);
         return ALIGN_UP(bits, 8) / 8;
      }

   case MIR_TYPE_REAL:
      return sizeof(double);

   case MIR_TYPE_CARRAY:
      {
         mir_type_t elem = mir_get_elem(g->mu, type);
         return mir_get_size(g->mu, type) * irgen_size_bytes(g, elem);
      }

   case MIR_TYPE_RECORD:
      {
         // TODO: cache this
         size_t nfields, bytes = 0;
         const mir_type_t *fields = mir_get_fields(g->mu, type, &nfields);
         for (int i = 0; i < nfields; i++) {
            const int fb = irgen_size_bytes(g, fields[i]);
            const int align = irgen_align_of(g, fields[i]);

            bytes = ALIGN_UP(bytes, align);
            bytes += fb;
         }

         return ALIGN_UP(bytes, sizeof(double));
      }

   case MIR_TYPE_UARRAY:
      {
         const int ndims = mir_get_dims(g->mu, type);
         if (mir_get_class(g->mu, mir_get_elem(g->mu, type)) == MIR_TYPE_SIGNAL)
            return 2*sizeof(void *) + 2 * sizeof(int64_t) * ndims;
         else
            return sizeof(void *) + 2 * sizeof(int64_t) * ndims;
      }

   case MIR_TYPE_ACCESS:
   case MIR_TYPE_POINTER:
   case MIR_TYPE_CONTEXT:
   case MIR_TYPE_TRIGGER:
      return sizeof(void *);

   case MIR_TYPE_FILE:
      return sizeof(uint32_t);

   case MIR_TYPE_SIGNAL:
      return sizeof(void *) + sizeof(int32_t);

   case MIR_TYPE_RESOLUTION:
      return 2*sizeof(void *) + sizeof(int64_t) + sizeof(int32_t);

   case MIR_TYPE_VEC2:
      return sizeof(uint64_t);

   case MIR_TYPE_VEC4:
      return 2*sizeof(uint64_t);

   default:
      fatal_trace("cannot handle type %d in irgen_size_bytes",
                  mir_get_class(g->mu, type));
   }
}

static jit_size_t irgen_jit_size(jit_irgen_t *g, mir_type_t type)
{
   const int bits = irgen_size_bits(g, type);
   switch (bits) {
   case 1:
   case 8: return JIT_SZ_8;
   case 16: return JIT_SZ_16;
   case 32: return JIT_SZ_32;
   case 64: return JIT_SZ_64;
   default:
      fatal_trace("illegal operand size %d", bits);
   }
}

static jit_value_t irgen_vector_mask(int size)
{
   assert(size <= 64);

   uint64_t mask = ~UINT64_C(0);
   if (size < 64) mask >>= 64 - size;

   return jit_value_from_int64(mask);
}

static jit_value_t irgen_get_value(jit_irgen_t *g, mir_value_t value)
{
   assert(!mir_is_null(value));

   switch (value.tag) {
   case MIR_TAG_NODE:
      assert(g->map[value.id].kind != JIT_VALUE_INVALID);
      return g->map[value.id];
   case MIR_TAG_CONST:
      {
         int64_t cval;
         if (mir_get_const(g->mu, value, &cval))
            return jit_value_from_int64(cval);

         should_not_reach_here();
      }
   case MIR_TAG_VAR:
      return g->vars[value.id];
   case MIR_TAG_PARAM:
      return g->params[value.id];
   default:
      DEBUG_ONLY(mir_dump(g->mu));
      should_not_reach_here();
   }
}

static jit_value_t irgen_get_arg(jit_irgen_t *g, mir_value_t n, int arg)
{
   return irgen_get_value(g, mir_get_arg(g->mu, n, arg));
}

static unsigned irgen_get_enum(jit_irgen_t *g, mir_value_t n, int arg)
{
   mir_value_t value = mir_get_arg(g->mu, n, arg);
   assert(value.tag == MIR_TAG_ENUM);
   return value.id;
}

static int64_t irgen_get_const(jit_irgen_t *g, mir_value_t n, int arg)
{
   mir_value_t value = mir_get_arg(g->mu, n, arg);

   int64_t result;
   if (mir_get_const(g->mu, value, &result))
      return result;

#ifdef DEBUG
   mir_dump(g->mu);
   fatal_trace("argument %d is not constant", arg);
#else
   should_not_reach_here();
#endif
}

static jit_value_t irgen_lea(jit_irgen_t *g, jit_value_t addr)
{
   switch (addr.kind) {
   case JIT_ADDR_REG:
      if (addr.disp == 0)
         return jit_value_from_reg(addr.reg);
      else
         return j_lea(g, addr);
   case JIT_ADDR_CPOOL:
      return j_lea(g, addr);
   case JIT_ADDR_ABS:
      return jit_value_from_int64(addr.int64);
   case JIT_VALUE_REG:
   case JIT_VALUE_INT64:
      return addr;
   default:
      fatal_trace("cannot load address of value kind %d", addr.kind);
   }
}

static jit_reg_t irgen_as_reg(jit_irgen_t *g, jit_value_t value)
{
   switch (value.kind) {
   case JIT_VALUE_REG:
      return value.reg;
   case JIT_ADDR_REG:
   case JIT_ADDR_CPOOL:
   case JIT_ADDR_ABS:
      return jit_value_as_reg(irgen_lea(g, value));
   default:
      {
         jit_reg_t r = irgen_alloc_reg(g);
         j_mov(g, r, value);
         return r;
      }
   }
}

static jit_value_t irgen_is_scalar(jit_irgen_t *g, mir_value_t n, int arg)
{
   mir_value_t value = mir_get_arg(g->mu, n, arg);
   return jit_value_from_int64(!mir_is(g->mu, value, MIR_TYPE_POINTER));
}

static jit_value_t irgen_get_context(jit_irgen_t *g)
{
   if (g->statereg.kind != JIT_VALUE_INVALID)
      return j_load(g, JIT_SZ_PTR, jit_addr_from_value(g->statereg, 0));
   else if (g->contextarg.kind != JIT_VALUE_INVALID)
      return g->contextarg;
   else
      return g->params[mir_get_param(g->mu, 0).id];
}

static size_t irgen_append_cpool(jit_irgen_t *g, size_t sz, int align)
{
   if (g->cpoolptr + sz + align - 1 > g->func->cpoolsz) {
      g->func->cpoolsz = MAX(128, MAX((g->func->cpoolsz * 3) / 2,
                                      g->cpoolptr + sz + align - 1));
      g->func->cpool = xrealloc(g->func->cpool, g->func->cpoolsz);
      g->func->owns_cpool = true;
   }

   const size_t result = ALIGN_UP(g->cpoolptr, align);
   g->oldptr = result;
   g->cpoolptr = result + sz;
   return result;
}

static jit_value_t irgen_dedup_cpool(jit_irgen_t *g)
{
   unsigned char *dup = memmem(g->func->cpool, MIN(g->oldptr, DEDUP_PREFIX),
                               g->func->cpool + g->oldptr,
                               g->cpoolptr - g->oldptr);
   if (dup != NULL) {
      g->cpoolptr = g->oldptr;
      return jit_value_from_cpool_addr(dup - g->func->cpool);
   }
   else
      return jit_value_from_cpool_addr(g->oldptr);
}

static void irgen_emit_debuginfo(jit_irgen_t *g, mir_value_t n)
{
   jit_value_t arg1 = jit_value_from_loc(mir_get_loc(g->mu, n));

   jit_value_t arg2 = {
      .kind = JIT_VALUE_VPOS,
      .vpos = { .block = g->curblock.id, .op = n.id }
   };

   if (g->func->nirs > 0 && g->func->irbuf[g->func->nirs - 1].op == J_DEBUG) {
      jit_ir_t *prev = &(g->func->irbuf[g->func->nirs - 1]);
      prev->arg1 = arg1;
      prev->arg2 = arg2;
   }
   else
      irgen_emit_binary(g, J_DEBUG, JIT_SZ_UNSPEC, JIT_CC_NONE,
                        JIT_REG_INVALID, arg1, arg2);
}

static ffi_type_t irgen_ffi_type(jit_irgen_t *g, mir_type_t type)
{
   if (mir_is_null(type))
      return FFI_VOID;

   switch (mir_get_class(g->mu, type)) {
   case MIR_TYPE_INT:
   case MIR_TYPE_OFFSET:
      switch (mir_get_repr(g->mu, type)) {
      case MIR_REPR_U1:
      case MIR_REPR_U8:
         return FFI_UINT8;
      case MIR_REPR_I8:
         return FFI_INT8;
      case MIR_REPR_I16:
         return FFI_INT16;
      case MIR_REPR_U16:
         return FFI_UINT16;
      case MIR_REPR_I32:
         return FFI_INT32;
      case MIR_REPR_U32:
         return FFI_UINT32;
      default:
         return FFI_INT64;
      }
   case MIR_TYPE_REAL:
      return FFI_FLOAT;
   case MIR_TYPE_CARRAY:
   case MIR_TYPE_RECORD:
   case MIR_TYPE_POINTER:
   case MIR_TYPE_CONTEXT:
   case MIR_TYPE_ACCESS:
   case MIR_TYPE_LOCUS:
   case MIR_TYPE_CONVERSION:
      return FFI_POINTER;
   case MIR_TYPE_FILE:
      return FFI_UINT32;
   case MIR_TYPE_UARRAY:
      return FFI_UARRAY;
   case MIR_TYPE_SIGNAL:
      return FFI_SIGNAL;
   case MIR_TYPE_VEC2:
      return FFI_VEC2;
   case MIR_TYPE_VEC4:
      return FFI_VEC4;
   default:
      fatal_trace("cannot handle type %d in irgen_ffi_type",
                  mir_get_class(g->mu, type));
   }
}

static jit_handle_t irgen_get_handle(jit_irgen_t *g, mir_value_t n, int arg)
{
   ident_t func = mir_get_name(g->mu, mir_get_arg(g->mu, n, arg));
   jit_handle_t handle = jit_lazy_compile(g->func->jit, func);
   if (handle == JIT_HANDLE_INVALID)
      fatal_at(mir_get_loc(g->mu, n), "missing definition for %s", istr(func));

   return handle;
}

static jit_value_t irgen_state_ptr(jit_irgen_t *g)
{
   assert(g->statereg.kind != JIT_VALUE_INVALID);
   return jit_addr_from_value(g->statereg, 2 * sizeof(void *));
}

static jit_value_t irgen_pcall_ptr(jit_irgen_t *g)
{
   assert(g->statereg.kind != JIT_VALUE_INVALID);
   return jit_addr_from_value(g->statereg, 1 * sizeof(void *));
}

static void irgen_op_null(jit_irgen_t *g, mir_value_t n)
{
   switch (mir_get_class(g->mu, mir_get_type(g->mu, n))) {
   case MIR_TYPE_POINTER:
   case MIR_TYPE_ACCESS:
   case MIR_TYPE_CONTEXT:
      g->map[n.id] = jit_null_ptr();
      break;
   case MIR_TYPE_FILE:
      g->map[n.id] = jit_value_from_int64(0);
      break;
   default:
      should_not_reach_here();
   }
}

static void irgen_op_const(jit_irgen_t *g, mir_value_t n)
{
   int64_t cval;
   if (mir_get_const(g->mu, n, &cval))
      g->map[n.id] = jit_value_from_int64(cval);
   else
      should_not_reach_here();
}

static void irgen_op_const_real(jit_irgen_t *g, mir_value_t n)
{
   double val;
   if (mir_get_const_real(g->mu, n, &val))
      g->map[n.id] = jit_value_from_double(val);
   else
      should_not_reach_here();
}

static void irgen_op_const_vec(jit_irgen_t *g, mir_value_t n)
{
   uint64_t abits, bbits;
   mir_get_bits(g->mu, n, &abits, &bbits);

   mir_type_t type = mir_get_type(g->mu, n);
   unsigned size = mir_get_size(g->mu, type);
   assert(size <= 64);

   if (mir_get_signed(g->mu, type) && size < 64)
      abits = (int64_t)(abits << (64 - size)) >> (64 - size);

   if (mir_is(g->mu, n, MIR_TYPE_VEC2)) {
      assert(bbits == 0);
      g->map[n.id] = jit_value_from_int64(abits);
   }
   else {
      // Registers must be contiguous
      jit_reg_t areg = irgen_alloc_reg(g);
      j_mov(g, areg, jit_value_from_int64(abits));
      jit_reg_t breg = irgen_alloc_reg(g);
      j_mov(g, breg, jit_value_from_int64(bbits));

      g->map[n.id] = jit_value_from_reg(areg);
   }
}

static void irgen_copy_const(jit_irgen_t *g, unsigned char *p,
                             jit_value_t value, int bytes)
{
   switch (value.kind) {
   case JIT_VALUE_INT64:
      switch (bytes) {
      case 1: *(uint8_t *)p = (uint8_t)value.int64; break;
      case 2: *(uint16_t *)p = (uint16_t)value.int64; break;
      case 4: *(uint32_t *)p = (uint32_t)value.int64; break;
      case 8: *(uint64_t *)p = (uint64_t)value.int64; break;
      default:
         fatal_trace("cannot handle value size %d", bytes);
      }
      break;

   case JIT_VALUE_DOUBLE:
      assert(bytes == sizeof(double));
      *(double *)p = value.dval;
      break;

   case JIT_ADDR_CPOOL:
      assert(value.int64 >= 0 && value.int64 + bytes <= g->func->cpoolsz);
      memcpy(p, g->func->cpool + value.int64, bytes);
      break;

   case JIT_ADDR_ABS:
      assert(value.int64 == 0);
      switch (bytes) {
      case sizeof(void *): *(uintptr_t *)p = (uintptr_t)value.int64; break;
      case 0: break;
      default: should_not_reach_here();
      }
      break;

   default:
      fatal_trace("cannot handle value kind %d", value.kind);
   }
}

static void irgen_op_const_array(jit_irgen_t *g, mir_value_t n)
{
   mir_type_t type = mir_get_type(g->mu, n);
   mir_type_t elem = mir_get_elem(g->mu, type);

   const int elemsz = irgen_size_bytes(g, elem);
   const int align = irgen_align_of(g, elem);
   const int count = mir_get_size(g->mu, type);

   if (count > 0) {
      const size_t offset = irgen_append_cpool(g, elemsz * count, align);

      unsigned char *p = g->func->cpool + offset;
      for (int i = 0; i < count; i++, p += elemsz) {
         jit_value_t elem = irgen_get_arg(g, n, i);
         irgen_copy_const(g, p, elem, elemsz);
      }

      g->map[n.id] = irgen_dedup_cpool(g);
   }
   else
      g->map[n.id] = jit_null_ptr();
}

static void irgen_op_const_rep(jit_irgen_t *g, mir_value_t n)
{
   mir_type_t elem = mir_get_elem(g->mu, mir_get_type(g->mu, n));

   const int count = irgen_get_const(g, n, 1);
   jit_value_t value = irgen_get_arg(g, n, 0);

   const int elemsz = irgen_size_bytes(g, elem);
   const int align = irgen_align_of(g, elem);

   const size_t offset = irgen_append_cpool(g, elemsz * count, align);

   unsigned char *p = g->func->cpool + offset;
   for (int i = 0; i < count; i++, p += elemsz)
      irgen_copy_const(g, p, value, elemsz);

   g->map[n.id] = irgen_dedup_cpool(g);
}

static void irgen_op_const_record(jit_irgen_t *g, mir_value_t n)
{
   mir_type_t type = mir_get_type(g->mu, n);

   size_t nfields;
   const mir_type_t *fields = mir_get_fields(g->mu, type, &nfields);

   const int sz = irgen_size_bytes(g, type);
   const int align = irgen_align_of(g, type);
   const size_t offset = irgen_append_cpool(g, sz, align);

   unsigned char *p = g->func->cpool + offset;
   for (int i = 0; i < nfields; i++) {
      jit_value_t elem = irgen_get_arg(g, n, i);

      const int bytes = irgen_size_bytes(g, fields[i]);
      const int align = irgen_align_of(g, fields[i]);

      while (p != ALIGN_UP(p, align))
         *p++ = 0;   // Pad to field alignment

      irgen_copy_const(g, p, elem, bytes);
      p += bytes;
   }
   assert(g->func->cpool + offset + sz - p < sizeof(double));

   g->map[n.id] = jit_value_from_cpool_addr(offset);
}

static void irgen_op_address_of(jit_irgen_t *g, mir_value_t n)
{
   // No-op
   g->map[n.id] = irgen_get_arg(g, n, 0);
}

static void irgen_op_copy(jit_irgen_t *g, mir_value_t n)
{
   jit_value_t arg0 = irgen_get_arg(g, n, 0);
   jit_value_t arg1 = irgen_get_arg(g, n, 1);

   mir_type_t elem = mir_get_type(g->mu, n);

   jit_value_t bytes;
   if (mir_count_args(g->mu, n) > 2) {
      jit_value_t arg2 = irgen_get_arg(g, n, 2);

      const int scale = irgen_size_bytes(g, elem);
      bytes = j_mul(g, arg2, jit_value_from_int64(scale));
   }
   else
      bytes = jit_value_from_int64(irgen_size_bytes(g, elem));

   jit_value_t dest = jit_addr_from_value(arg0, 0);
   jit_value_t src = jit_addr_from_value(arg1, 0);

   macro_move(g, dest, src, irgen_as_reg(g, bytes));
}

static void irgen_op_set(jit_irgen_t *g, mir_value_t n)
{
   jit_value_t base   = irgen_get_arg(g, n, 0);
   jit_value_t value  = irgen_get_arg(g, n, 1);
   jit_value_t length = irgen_get_arg(g, n, 2);

   mir_type_t type = mir_get_type(g->mu, n);

   jit_value_t addr = jit_addr_from_value(base, 0);
   jit_value_t scale = jit_value_from_int64(irgen_size_bytes(g, type));
   jit_value_t bytes = j_mul(g, length, scale);

   jit_reg_t bytes_r = jit_value_as_reg(bytes);

   if (value.kind == JIT_VALUE_INT64 && value.int64 == 0)
      macro_bzero(g, addr, bytes_r);
   else if (value.kind == JIT_VALUE_DOUBLE) {
      jit_value_t bits = jit_value_from_int64(value.int64);
      macro_memset(g, irgen_jit_size(g, type), addr, bits, bytes_r);
   }
   else
      macro_memset(g, irgen_jit_size(g, type), addr, value, bytes_r);
}

static void irgen_send_args(jit_irgen_t *g, mir_value_t n, int first)
{
   const int nargs = mir_count_args(g->mu, n);

   jit_value_t spill = { .kind = JIT_VALUE_INVALID };
   int32_t spilloff = 0;

   for (int i = 0, pslot = first; i < nargs; i++) {
      mir_value_t arg = mir_get_arg(g->mu, n, i);
      if (arg.tag == MIR_TAG_LINKAGE || arg.tag == MIR_TAG_BLOCK)
         continue;   // Skip function name and resume block

      int slots = irgen_slots_for_type(g, mir_get_type(g->mu, arg));

      if (pslot + slots >= JIT_MAX_ARGS - 1) {
         // Large number of arguments spill to the heap
         if (spill.kind == JIT_VALUE_INVALID) {
            size_t size = slots * sizeof(jit_scalar_t);
            for (int j = i + 1; j < nargs; j++) {
               mir_type_t type = mir_get_type(g->mu, mir_get_arg(g->mu, n, j));
               size += irgen_slots_for_type(g, type) * sizeof(jit_scalar_t);
            }

            spill = macro_lalloc(g, jit_value_from_int64(size));
            j_send(g, JIT_MAX_ARGS - 1, spill);
            pslot = JIT_MAX_ARGS;
            g->used_tlab = true;
         }

         jit_reg_t base = irgen_as_reg(g, irgen_get_value(g, arg));
         for (int j = 0; j < slots; j++, spilloff += sizeof(jit_scalar_t)) {
            jit_value_t ptr = jit_addr_from_value(spill, spilloff);
            j_store(g, JIT_SZ_64, jit_value_from_reg(base + j), ptr);
         }
      }
      else if (slots > 1) {
         jit_reg_t base = jit_value_as_reg(irgen_get_value(g, arg));
         for (int j = 0; j < slots; j++)
            j_send(g, pslot++, jit_value_from_reg(base + j));
      }
      else
         j_send(g, pslot++, irgen_get_value(g, arg));
   }
}

static void irgen_op_return(jit_irgen_t *g, mir_value_t n)
{
   switch (mir_get_kind(g->mu)) {
   case MIR_UNIT_PROCESS:
      if (g->statereg.kind != JIT_VALUE_INVALID) {
         // Set up for first call to process after reset
         jit_value_t ptr = irgen_state_ptr(g);
         j_store(g, JIT_SZ_32, jit_value_from_int64(1), ptr);
         j_send(g, 0, g->statereg);
      }
      else
         j_send(g, 0, jit_null_ptr());   // Stateless process
      break;

   case MIR_UNIT_PROCEDURE:
      j_send(g, 0, jit_null_ptr());
      macro_trim(g);
      break;

   case MIR_UNIT_INSTANCE:
   case MIR_UNIT_PROTECTED:
   case MIR_UNIT_PACKAGE:
      j_send(g, 0, g->statereg);
      break;

   case MIR_UNIT_PROPERTY:
      if (mir_count_args(g->mu, n) > 0)
         irgen_send_args(g, n, 1);

      if (g->statereg.kind != JIT_VALUE_INVALID)
         j_send(g, 0, g->statereg);
      else
         j_send(g, 0, jit_null_ptr());   // Stateless property
      break;

   case MIR_UNIT_FUNCTION:
   case MIR_UNIT_THUNK:
      if (mir_count_args(g->mu, n) > 0)
         irgen_send_args(g, n, 0);
      else
         j_send(g, 0, jit_null_ptr());  // Procedure compiled as function

      if (g->used_tlab) {
         const mir_mem_t mem = mir_get_mem(g->mu, mir_get_arg(g->mu, n, 0));
         if (mem <= MIR_MEM_CONST)
            macro_trim(g);
         else if (mem == MIR_MEM_STACK) {
            mir_dump(g->mu);
            fatal_trace("returning pointer to stack allocation");
         }
      }
      break;
   }

   j_ret(g);
}

static void irgen_op_not(jit_irgen_t *g, mir_value_t n)
{
   jit_value_t arg0 = irgen_get_arg(g, n, 0);
   g->map[n.id] = j_not(g, arg0);
}

static void irgen_op_and(jit_irgen_t *g, mir_value_t n)
{
   jit_value_t arg0 = irgen_get_arg(g, n, 0);
   jit_value_t arg1 = irgen_get_arg(g, n, 1);

   g->map[n.id] = j_and(g, arg0, arg1);
}

static void irgen_op_or(jit_irgen_t *g, mir_value_t n)
{
   jit_value_t arg0 = irgen_get_arg(g, n, 0);
   jit_value_t arg1 = irgen_get_arg(g, n, 1);

   g->map[n.id] = j_or(g, arg0, arg1);
}

static void irgen_op_xor(jit_irgen_t *g, mir_value_t n)
{
   jit_value_t arg0 = irgen_get_arg(g, n, 0);
   jit_value_t arg1 = irgen_get_arg(g, n, 1);

   g->map[n.id] = j_xor(g, arg0, arg1);
}

static void irgen_op_trap_add(jit_irgen_t *g, mir_value_t n)
{
   jit_value_t arg0 = irgen_get_arg(g, n, 0);
   jit_value_t arg1 = irgen_get_arg(g, n, 1);
   jit_value_t locus = irgen_get_arg(g, n, 2);

   mir_type_t type = mir_get_type(g->mu, n);
   jit_size_t sz = irgen_jit_size(g, type);
   mir_repr_t repr = mir_get_repr(g->mu, type);

   jit_cc_t cc = irgen_repr_signed(repr) ? JIT_CC_O : JIT_CC_C;
   g->map[n.id] = j_adds(g, sz, cc, arg0, arg1);

   irgen_label_t *l_pass = irgen_alloc_label(g);
   j_jump(g, JIT_CC_F, l_pass);

   j_send(g, 0, arg0);
   j_send(g, 1, arg1);
   j_send(g, 2, locus);
   macro_exit(g, JIT_EXIT_OVERFLOW);

   irgen_bind_label(g, l_pass);
}

static void irgen_op_add(jit_irgen_t *g, mir_value_t n)
{
   jit_value_t arg0 = irgen_get_arg(g, n, 0);
   jit_value_t arg1 = irgen_get_arg(g, n, 1);

   if (mir_is(g->mu, n, MIR_TYPE_REAL))
      g->map[n.id] = j_fadd(g, arg0, arg1);
   else
      g->map[n.id] = j_add(g, arg0, arg1);
}

static void irgen_op_trap_mul(jit_irgen_t *g, mir_value_t n)
{
   jit_value_t arg0 = irgen_get_arg(g, n, 0);
   jit_value_t arg1 = irgen_get_arg(g, n, 1);
   jit_value_t locus = irgen_get_arg(g, n, 2);

   mir_type_t type = mir_get_type(g->mu, n);
   jit_size_t sz = irgen_jit_size(g, type);
   mir_repr_t repr = mir_get_repr(g->mu, type);

   jit_cc_t cc = irgen_repr_signed(repr) ? JIT_CC_O : JIT_CC_C;
   g->map[n.id] = j_muls(g, sz, cc, arg0, arg1);

   irgen_label_t *l_pass = irgen_alloc_label(g);
   j_jump(g, JIT_CC_F, l_pass);

   j_send(g, 0, arg0);
   j_send(g, 1, arg1);
   j_send(g, 2, locus);
   macro_exit(g, JIT_EXIT_OVERFLOW);

   irgen_bind_label(g, l_pass);
}

static void irgen_op_mul(jit_irgen_t *g, mir_value_t n)
{
   jit_value_t arg0 = irgen_get_arg(g, n, 0);
   jit_value_t arg1 = irgen_get_arg(g, n, 1);

   if (mir_is(g->mu, n, MIR_TYPE_REAL))
      g->map[n.id] = j_fmul(g, arg0, arg1);
   else
      g->map[n.id] = j_mul(g, arg0, arg1);
}

static void irgen_op_div(jit_irgen_t *g, mir_value_t n)
{
   jit_value_t arg0 = irgen_get_arg(g, n, 0);
   jit_value_t arg1 = irgen_get_arg(g, n, 1);

   if (mir_is(g->mu, n, MIR_TYPE_REAL))
      g->map[n.id] = j_fdiv(g, arg0, arg1);
   else
      g->map[n.id] = j_div(g, arg0, arg1);
}

static void irgen_op_exp(jit_irgen_t *g, mir_value_t n)
{
   jit_value_t arg0 = irgen_get_arg(g, n, 0);
   jit_value_t arg1 = irgen_get_arg(g, n, 1);

   if (mir_is(g->mu, n, MIR_TYPE_REAL))
      g->map[n.id] = macro_fexp(g, arg0, arg1);
   else
      g->map[n.id] = macro_exp(g, JIT_SZ_UNSPEC, JIT_CC_NONE, arg0, arg1);
}

static void irgen_op_trap_exp(jit_irgen_t *g, mir_value_t n)
{
   jit_value_t arg0 = irgen_get_arg(g, n, 0);
   jit_value_t arg1 = irgen_get_arg(g, n, 1);
   jit_value_t locus = irgen_get_arg(g, n, 2);

   mir_type_t type = mir_get_type(g->mu, n);
   jit_size_t sz = irgen_jit_size(g, type);
   mir_repr_t repr = mir_get_repr(g->mu, type);

   jit_cc_t cc = irgen_repr_signed(repr) ? JIT_CC_O : JIT_CC_C;
   g->map[n.id] = macro_exp(g, sz, cc, arg0, arg1);

   irgen_label_t *l_pass = irgen_alloc_label(g);
   j_jump(g, JIT_CC_F, l_pass);

   j_send(g, 0, arg0);
   j_send(g, 1, arg1);
   j_send(g, 2, locus);
   macro_exit(g, JIT_EXIT_OVERFLOW);

   irgen_bind_label(g, l_pass);
}

static void irgen_op_sub(jit_irgen_t *g, mir_value_t n)
{
   jit_value_t arg0 = irgen_get_arg(g, n, 0);
   jit_value_t arg1 = irgen_get_arg(g, n, 1);

   if (mir_is(g->mu, n, MIR_TYPE_REAL))
      g->map[n.id] = j_fsub(g, arg0, arg1);
   else
      g->map[n.id] = j_sub(g, arg0, arg1);
}

static void irgen_op_trap_sub(jit_irgen_t *g, mir_value_t n)
{
   jit_value_t arg0 = irgen_get_arg(g, n, 0);
   jit_value_t arg1 = irgen_get_arg(g, n, 1);
   jit_value_t locus = irgen_get_arg(g, n, 2);

   mir_type_t type = mir_get_type(g->mu, n);
   jit_size_t sz = irgen_jit_size(g, type);
   mir_repr_t repr = mir_get_repr(g->mu, type);

   jit_cc_t cc = irgen_repr_signed(repr) ? JIT_CC_O : JIT_CC_C;
   g->map[n.id] = j_subs(g, sz, cc, arg0, arg1);

   irgen_label_t *l_pass = irgen_alloc_label(g);
   j_jump(g, JIT_CC_F, l_pass);

   j_send(g, 0, arg0);
   j_send(g, 1, arg1);
   j_send(g, 2, locus);
   macro_exit(g, JIT_EXIT_OVERFLOW);

   irgen_bind_label(g, l_pass);
}

static void irgen_op_neg(jit_irgen_t *g, mir_value_t n)
{
   jit_value_t arg0 = irgen_get_arg(g, n, 0);

   if (mir_is_integral(g->mu, n))
      g->map[n.id] = j_neg(g, arg0);
   else
      g->map[n.id] = j_fneg(g, arg0);
}

static void irgen_op_trap_neg(jit_irgen_t *g, mir_value_t n)
{
   jit_value_t arg0 = irgen_get_arg(g, n, 0);
   jit_value_t locus = irgen_get_arg(g, n, 1);

   int64_t cmp;
   switch (irgen_jit_size(g, mir_get_type(g->mu, n))) {
   case JIT_SZ_8: cmp = INT8_MIN; break;
   case JIT_SZ_16: cmp = INT16_MIN; break;
   case JIT_SZ_32: cmp = INT32_MIN; break;
   case JIT_SZ_64: cmp = INT64_MIN; break;
   default: cmp = 0;
   }

   irgen_label_t *cont = irgen_alloc_label(g);
   j_cmp(g, JIT_CC_GT, arg0, jit_value_from_int64(cmp));
   j_jump(g, JIT_CC_T, cont);

   j_send(g, 0, arg0);
   j_send(g, 1, jit_value_from_int64(0));
   j_send(g, 2, locus);
   macro_exit(g, JIT_EXIT_OVERFLOW);

   irgen_bind_label(g, cont);
   g->map[n.id] = j_neg(g, arg0);
}

static void irgen_op_abs(jit_irgen_t *g, mir_value_t n)
{
   jit_value_t arg0 = irgen_get_arg(g, n, 0);

   if (mir_is(g->mu, n, MIR_TYPE_REAL)) {
      jit_value_t neg = j_fneg(g, arg0);
      j_fcmp(g, JIT_CC_LT, arg0, jit_value_from_double(0.0));
      g->map[n.id] = j_csel(g, neg, arg0);
   }
   else {
      jit_value_t neg = j_neg(g, arg0);
      j_cmp(g, JIT_CC_LT, arg0, jit_value_from_int64(0));
      g->map[n.id] = j_csel(g, neg, arg0);
   }
}

static void irgen_op_mod(jit_irgen_t *g, mir_value_t n)
{
   jit_value_t numer = irgen_get_arg(g, n, 0);
   jit_value_t denom = irgen_get_arg(g, n, 1);

   // Calculate the following:
   //
   //   long r = numer % denom;
   //   if ((r > 0 && denom < 0) || (r < 0 && denom > 0))
   //      r = r + denom;

   jit_value_t r = j_rem(g, numer, denom);

   irgen_label_t *l1 = irgen_alloc_label(g);
   irgen_label_t *l2 = irgen_alloc_label(g);

   jit_value_t zero = jit_value_from_int64(0);

   j_cmp(g, JIT_CC_GT, r, zero);
   j_ccmp(g, JIT_CC_LT, denom, zero);
   j_jump(g, JIT_CC_T, l1);

   j_cmp(g, JIT_CC_LT, r, zero);
   j_ccmp(g, JIT_CC_GT, denom, zero);
   j_jump(g, JIT_CC_F, l2);

   irgen_bind_label(g, l1);

   jit_value_t r2 = j_add(g, r, denom);
   j_mov(g, jit_value_as_reg(r), r2);

   irgen_bind_label(g, l2);

   g->map[n.id] = r;
}

static void irgen_op_rem(jit_irgen_t *g, mir_value_t n)
{
   jit_value_t arg0 = irgen_get_arg(g, n, 0);
   jit_value_t arg1 = irgen_get_arg(g, n, 1);

   g->map[n.id] = j_rem(g, arg0, arg1);
}

static void irgen_op_cmp(jit_irgen_t *g, mir_value_t n)
{
   mir_value_t arg1 = mir_get_arg(g->mu, n, 1);
   jit_value_t left = irgen_get_value(g, arg1);
   jit_value_t right = irgen_get_arg(g, n, 2);

   jit_cc_t cc = irgen_get_jit_cc(irgen_get_enum(g, n, 0));

   if (mir_is(g->mu, arg1, MIR_TYPE_REAL))
      j_fcmp(g, cc, left, right);
   else
      j_cmp(g, cc, left, right);

   g->map[n.id] = j_cset(g);
   g->flags = n;
}

static jit_value_t irgen_load_addr(jit_irgen_t *g, mir_type_t type,
                                   jit_value_t ptr)
{
   jit_value_t addr = jit_addr_from_value(ptr, 0);

   switch (mir_get_class(g->mu, type)) {
   case MIR_TYPE_OFFSET:
   case MIR_TYPE_INT:
      if (irgen_repr_signed(mir_get_repr(g->mu, type)))
         return j_load(g, irgen_jit_size(g, type), addr);
      else
         return j_uload(g, irgen_jit_size(g, type), addr);

   case MIR_TYPE_REAL:
      return j_load(g, JIT_SZ_64, addr);

   case MIR_TYPE_ACCESS:
   case MIR_TYPE_POINTER:
   case MIR_TYPE_CONTEXT:
   case MIR_TYPE_TRIGGER:
      return j_load(g, JIT_SZ_PTR, addr);

   case MIR_TYPE_FILE:
      return j_load(g, JIT_SZ_32, addr);

   case MIR_TYPE_SIGNAL:
      {
         jit_value_t shared = j_load(g, JIT_SZ_PTR, addr);
         addr = jit_addr_from_value(addr, sizeof(void *));
         j_load(g, JIT_SZ_32, addr);   // Offset
         return shared;
      }

   case MIR_TYPE_UARRAY:
      {
         const int slots = irgen_slots_for_type(g, mir_get_elem(g->mu, type));
         jit_value_t base = j_load(g, JIT_SZ_PTR, addr);
         addr = jit_addr_from_value(addr, sizeof(void *));
         for (int i = 1; i < slots; i++) {
            j_load(g, JIT_SZ_PTR, addr);
            addr = jit_addr_from_value(addr, sizeof(void *));
         }

         const int ndims = mir_get_dims(g->mu, type);
         for (int i = 0; i < 2*ndims; i++) {
            j_load(g, JIT_SZ_64, addr);
            addr = jit_addr_from_value(addr, sizeof(int64_t));
         }

         return base;
      }

   case MIR_TYPE_RESOLUTION:
      {
         jit_value_t base = j_load(g, JIT_SZ_PTR, addr);  // Function pointer
         addr = jit_addr_from_value(addr, sizeof(void *));
         j_load(g, JIT_SZ_PTR, addr);   // Context
         addr = jit_addr_from_value(addr, sizeof(void *));
         j_load(g, JIT_SZ_64, addr);   // Literals
         addr = jit_addr_from_value(addr, sizeof(int64_t));
         j_load(g, JIT_SZ_32, addr);   // Flags
         return base;
      }

   case MIR_TYPE_VEC2:
      return j_load(g, JIT_SZ_64, addr);

   case MIR_TYPE_VEC4:
      {
         jit_value_t base = j_load(g, JIT_SZ_64, addr);
         addr = jit_addr_from_value(addr, sizeof(uint64_t));
         j_load(g, JIT_SZ_PTR, addr);
         return base;
      }

   default:
      mir_dump(g->mu);
      fatal_trace("cannot load type kind %d", mir_get_class(g->mu, type));
   }
}

static void irgen_op_load(jit_irgen_t *g, mir_value_t n)
{
   jit_value_t addr = irgen_get_arg(g, n, 0);
   mir_type_t type = mir_get_type(g->mu, n);

   g->map[n.id] = irgen_load_addr(g, type, addr);
}

static void irgen_store_addr(jit_irgen_t *g, mir_type_t type,
                             jit_value_t value, jit_value_t ptr)
{
   jit_value_t addr = jit_addr_from_value(ptr, 0);

   if (jit_value_is_addr(value))
      value = irgen_lea(g, value);   // Storing an address

   switch (mir_get_class(g->mu, type)) {
   case MIR_TYPE_OFFSET:
   case MIR_TYPE_INT:
   case MIR_TYPE_ACCESS:
   case MIR_TYPE_POINTER:
   case MIR_TYPE_CONTEXT:
   case MIR_TYPE_REAL:
   case MIR_TYPE_TRIGGER:
      j_store(g, irgen_jit_size(g, type), value, addr);
      break;

   case MIR_TYPE_FILE:
      j_store(g, JIT_SZ_32, value, addr);
      break;

   case MIR_TYPE_UARRAY:
      {
         const int slots = irgen_slots_for_type(g, mir_get_elem(g->mu, type));
         jit_reg_t base = jit_value_as_reg(value);
         for (int i = 0; i < slots; i++) {
            j_store(g, JIT_SZ_PTR, jit_value_from_reg(base + i), addr);
            addr = jit_addr_from_value(addr, sizeof(void *));
         }

         const int ndims = mir_get_dims(g->mu, type);
         for (int i = 0; i < 2*ndims; i++) {
            j_store(g, JIT_SZ_64, jit_value_from_reg(base + slots + i), addr);
            addr = jit_addr_from_value(addr, sizeof(int64_t));
         }
      }
      break;

   case MIR_TYPE_SIGNAL:
      {
         jit_reg_t base = jit_value_as_reg(value);
         j_store(g, JIT_SZ_PTR, value, addr);
         addr = jit_addr_from_value(addr, sizeof(void *));
         j_store(g, JIT_SZ_32, jit_value_from_reg(base + 1), addr);
      }
      break;

   case MIR_TYPE_RESOLUTION:
      {
         jit_reg_t base = jit_value_as_reg(value);
         j_store(g, JIT_SZ_PTR, value, addr);  // Function pointer
         addr = jit_addr_from_value(addr, sizeof(void *));
         j_store(g, JIT_SZ_PTR, jit_value_from_reg(base + 1), addr);  // Context
         addr = jit_addr_from_value(addr, sizeof(void *));
         j_store(g, JIT_SZ_64, jit_value_from_reg(base + 2), addr);  // Literals
         addr = jit_addr_from_value(addr, sizeof(int64_t));
         j_store(g, JIT_SZ_32, jit_value_from_reg(base + 3), addr);  // Flags
      }
      break;

   case MIR_TYPE_VEC2:
      j_store(g, JIT_SZ_64, value, addr);
      break;

   case MIR_TYPE_VEC4:
      {
         jit_reg_t base = jit_value_as_reg(value);
         j_store(g, JIT_SZ_64, value, addr);
         addr = jit_addr_from_value(addr, sizeof(uint64_t));
         j_store(g, JIT_SZ_PTR, jit_value_from_reg(base + 1), addr);
      }
      break;

   default:
      fatal_trace("cannot store type kind %d", mir_get_class(g->mu, type));
   }
}

static void irgen_op_store(jit_irgen_t *g, mir_value_t n)
{
   jit_value_t addr = irgen_get_arg(g, n, 0);
   jit_value_t value = irgen_get_arg(g, n, 1);
   mir_type_t type = mir_get_type(g->mu, n);

   irgen_store_addr(g, type, value, addr);
}

static void irgen_op_locus(jit_irgen_t *g, mir_value_t n)
{
   g->map[n.id] = (jit_value_t){
      .kind  = JIT_VALUE_LOCUS,
      .locus = mir_get_locus(g->mu, n),
   };
}

static void irgen_op_wrap(jit_irgen_t *g, mir_value_t n)
{
   mir_value_t arg0 = mir_get_arg(g->mu, n, 0);
   jit_value_t ptr = irgen_lea(g, irgen_get_value(g, arg0));

   mir_type_t type = mir_get_type(g->mu, arg0);

   const int ndims = mir_count_args(g->mu, n) / 3;
   const int slots = irgen_slots_for_type(g, type);

   // Registers must be contiguous
   jit_reg_t base = irgen_alloc_reg(g);
   j_mov(g, base, ptr);
   for (int i = 1; i < slots; i++) {
      jit_reg_t slot = irgen_alloc_reg(g);
      j_mov(g, slot, jit_value_from_reg(jit_value_as_reg(ptr) + i));
   }
   jit_reg_t dims[ndims * 2];
   for (int i = 0; i < ndims * 2; i++)
      dims[i] = irgen_alloc_reg(g);

   for (int i = 0; i < ndims; i++) {
      jit_value_t left  = irgen_get_arg(g, n, (i * 3) + 1);
      jit_value_t right = irgen_get_arg(g, n, (i * 3) + 2);
      jit_value_t dir   = irgen_get_arg(g, n, (i * 3) + 3);

      jit_value_t diff_up   = j_sub(g, right, left);
      jit_value_t diff_down = j_sub(g, left, right);

      j_cmp(g, JIT_CC_EQ, dir, jit_value_from_int64(RANGE_DOWNTO));
      jit_value_t diff    = j_csel(g, diff_down, diff_up);
      jit_value_t length  = j_add(g, diff, jit_value_from_int64(1));
      jit_value_t clamped = j_clamp(g, length);
      jit_value_t mask    = j_neg(g, dir);
      jit_value_t signlen = j_xor(g, mask, clamped);

      j_mov(g, dims[i*2], left);
      j_mov(g, dims[i*2 + 1], signlen);
   }

   g->map[n.id] = jit_value_from_reg(base);
}

static void irgen_op_uarray_right(jit_irgen_t *g, mir_value_t n)
{
   mir_value_t arg0 = mir_get_arg(g->mu, n, 0);
   jit_reg_t base = jit_value_as_reg(irgen_get_value(g, arg0));

   mir_type_t elem = mir_get_elem(g->mu, mir_get_type(g->mu, arg0));

   const int dim = irgen_get_const(g, n, 1);
   const int slots = irgen_slots_for_type(g, elem);

   jit_value_t left   = jit_value_from_reg(base + slots + dim*2);
   jit_value_t length = jit_value_from_reg(base + slots + 1 + dim*2);
   jit_value_t diff   = j_add(g, left, length);
   j_cmp(g, JIT_CC_LT, length, jit_value_from_int64(0));

   jit_value_t adj = j_csel(g, jit_value_from_int64(2),
                            jit_value_from_int64(-1));

   g->map[n.id] = j_add(g, diff, adj);
}

static void irgen_op_uarray_left(jit_irgen_t *g, mir_value_t n)
{
   mir_value_t arg0 = mir_get_arg(g->mu, n, 0);
   jit_reg_t base = jit_value_as_reg(irgen_get_value(g, arg0));

   mir_type_t elem = mir_get_elem(g->mu, mir_get_type(g->mu, arg0));

   const int dim = irgen_get_const(g, n, 1);
   const int slots = irgen_slots_for_type(g, elem);

   g->map[n.id] = jit_value_from_reg(base + slots + dim*2);
}

static void irgen_op_uarray_dir(jit_irgen_t *g, mir_value_t n)
{
   mir_value_t arg0 = mir_get_arg(g->mu, n, 0);
   jit_reg_t base = jit_value_as_reg(irgen_get_value(g, arg0));

   mir_type_t elem = mir_get_elem(g->mu, mir_get_type(g->mu, arg0));

   const int dim = irgen_get_const(g, n, 1);
   const int slots = irgen_slots_for_type(g, elem);

   jit_value_t length = jit_value_from_reg(base + slots + 1 + dim*2);
   j_cmp(g, JIT_CC_LT, length, jit_value_from_int64(0));

   STATIC_ASSERT(RANGE_DOWNTO == 1);

   g->map[n.id] = j_cset(g);
   g->flags = n;
}

static void irgen_op_uarray_len(jit_irgen_t *g, mir_value_t n)
{
   mir_value_t arg0 = mir_get_arg(g->mu, n, 0);
   jit_reg_t base = jit_value_as_reg(irgen_get_value(g, arg0));

   mir_type_t elem = mir_get_elem(g->mu, mir_get_type(g->mu, arg0));

   const int dim = irgen_get_const(g, n, 1);
   const int slots = irgen_slots_for_type(g, elem);

   jit_value_t length = jit_value_from_reg(base + slots + 1 + dim*2);
   jit_value_t mask = j_asr(g, length, jit_value_from_int64(63));
   g->map[n.id] = j_xor(g, mask, length);
}

static void irgen_op_unwrap(jit_irgen_t *g, mir_value_t n)
{
   jit_value_t base = irgen_get_arg(g, n, 0);
   g->map[n.id] = base;
}

static void irgen_op_array_ref(jit_irgen_t *g, mir_value_t n)
{
   if (mir_is_signal(g->mu, n)) {
      jit_value_t shared = irgen_get_arg(g, n, 0);
      jit_value_t offset = jit_value_from_reg(jit_value_as_reg(shared) + 1);

      jit_value_t arg1 = irgen_get_arg(g, n, 1);

      if (arg1.kind == JIT_VALUE_INT64 && arg1.int64 == 0)
         g->map[n.id] = shared;
      else {
         jit_reg_t new = irgen_alloc_reg(g);
         j_mov(g, new, shared);
         g->map[n.id] = jit_value_from_reg(new);

         // Offset must be next sequential register
         j_add(g, offset, arg1);
      }
   }
   else {
      assert(mir_is(g->mu, n, MIR_TYPE_POINTER));
      mir_type_t elem = mir_get_elem(g->mu, mir_get_type(g->mu, n));
      const int scale = irgen_size_bytes(g, elem);

      jit_value_t arg0 = irgen_get_arg(g, n, 0);
      jit_value_t arg1 = irgen_get_arg(g, n, 1);

      if (arg1.kind == JIT_VALUE_INT64)
         g->map[n.id] = jit_addr_from_value(arg0, arg1.int64 * scale);
      else {
         jit_value_t scaled = j_mul(g, arg1, jit_value_from_int64(scale));
         jit_value_t addr = irgen_lea(g, arg0);
         g->map[n.id] = j_add(g, addr, scaled);
      }
   }
}

static void irgen_op_record_ref(jit_irgen_t *g, mir_value_t n)
{
   mir_value_t arg0 = mir_get_arg(g->mu, n, 0);
   mir_type_t rtype = mir_get_elem(g->mu, mir_get_type(g->mu, arg0));
   assert(mir_get_class(g->mu, rtype) == MIR_TYPE_RECORD);

   const int field = irgen_get_const(g, n, 1);

   size_t nfields;
   const mir_type_t *ftypes = mir_get_fields(g->mu, rtype, &nfields);
   assert(field < nfields);

   // TODO: cache field offsets somewhere? This duplicates irgen_size_bytes
   int offset = 0;
   for (int i = 0; i < field; i++) {
      const int fb = irgen_size_bytes(g, ftypes[i]);
      const int align = irgen_align_of(g, ftypes[i]);

      offset = ALIGN_UP(offset, align);
      offset += fb;
   }

   const int align = irgen_align_of(g, ftypes[field]);
   offset = ALIGN_UP(offset, align);

   jit_value_t rptr = irgen_get_value(g, arg0);
   g->map[n.id] = jit_addr_from_value(rptr, offset);
}

static void irgen_op_context_upref(jit_irgen_t *g, mir_value_t n)
{
   const int hops = irgen_get_const(g, n, 0);

   if (hops == 0) {
      assert(g->statereg.kind != JIT_VALUE_INVALID);
      g->map[n.id] = g->statereg;
   }
   else {
      jit_value_t context = irgen_get_context(g);

      for (int i = 1; i < hops; i++)
         context = j_load(g, JIT_SZ_PTR, jit_addr_from_value(context, 0));

      g->map[n.id] = context;
   }
}

static void irgen_op_var_upref(jit_irgen_t *g, mir_value_t n)
{
   jit_value_t context = irgen_get_context(g);

   const int hops = irgen_get_const(g, n, 0);
   const int nth = irgen_get_const(g, n, 2);
   ident_t unit_name = mir_get_name(g->mu, mir_get_arg(g->mu, n, 1));

   for (int i = 1; i < hops; i++)
      context = j_load(g, JIT_SZ_PTR, jit_addr_from_value(context, 0));

   // TODO: maybe we should cache these somewhere?
   jit_handle_t handle = jit_lazy_compile(g->func->jit, unit_name);
   jit_func_t *cf = jit_get_func(g->func->jit, handle);

   // Handle potential circular dependency
   // TODO: it would be better to avoid this entirely
   const link_tab_t *tab = load_acquire(&(cf->linktab));
   if (tab == NULL) {
      jit_fill_irbuf(cf);
      tab = load_acquire(&(cf->linktab));
      assert(tab);
   }

   assert(nth < cf->nvars);
   const int offset = tab[nth].offset;

   g->map[n.id] = jit_addr_from_value(context, offset);
}

static void irgen_op_cast(jit_irgen_t *g, mir_value_t n)
{
   mir_value_t arg = mir_get_arg(g->mu, n, 0);

   mir_type_t result_type = mir_get_type(g->mu, n);
   mir_class_t result_kind = mir_get_class(g->mu, result_type);

   if (result_kind == MIR_TYPE_REAL && mir_is_integral(g->mu, arg))
      g->map[n.id] = j_scvtf(g, irgen_get_value(g, arg));
   else if (result_kind == MIR_TYPE_INT && mir_is(g->mu, arg, MIR_TYPE_REAL))
      g->map[n.id] = j_fcvtns(g, irgen_get_value(g, arg));
   else if ((result_kind == MIR_TYPE_INT || result_kind == MIR_TYPE_OFFSET)
            && mir_is_integral(g->mu, arg)) {
      // No sign extension or truncation is necessary as integer
      // registers are always 64 bits wide
      g->map[n.id] = irgen_get_value(g, arg);
   }
   else if (result_kind == MIR_TYPE_REAL) {
      // Reals are always represented with IEEE doubles
      assert(mir_is(g->mu, arg, MIR_TYPE_REAL));
      g->map[n.id] = irgen_get_value(g, arg);
   }
   else if (result_kind == MIR_TYPE_ACCESS) {
      // Casting away opaqueness
      assert(mir_is(g->mu, arg, MIR_TYPE_ACCESS));
      g->map[n.id] = irgen_get_value(g, arg);
   }
   else if (result_kind == MIR_TYPE_VEC4 || result_kind == MIR_TYPE_VEC2) {
      mir_type_t arg_type = mir_get_type(g->mu, arg);

      jit_value_t abits = irgen_get_value(g, arg), bbits;
      if (mir_get_class(g->mu, arg_type) == MIR_TYPE_VEC4)
         bbits = jit_value_from_reg(jit_value_as_reg(abits) + 1);
      else
         bbits = jit_value_from_int64(0);

      const bool arg_signed = mir_get_signed(g->mu, arg_type);
      const bool result_signed = mir_get_signed(g->mu, result_type);

      const int result_size = mir_get_size(g->mu, result_type);
      const int arg_size = mir_get_size(g->mu, arg_type);
      if (arg_size > 64 || result_size > 64) {
         j_send(g, 0, abits);
         j_send(g, 1, bbits);
         j_send(g, 2, jit_value_from_int64(arg_size));

         if (arg_signed || result_signed)
            macro_vec4op(g, JIT_VEC_SEXT, result_size);
         else
            macro_vec4op(g, JIT_VEC_ZEXT, result_size);

         g->map[n.id] = j_recv(g, 0);
         j_recv(g, 1);
      }
      else {
         if (result_signed && !arg_signed && arg_size < 64) {
            jit_value_t shift = jit_value_from_int64(64 - arg_size);
            abits = j_shl(g, abits, shift);
            abits = j_asr(g, abits, shift);

            jit_reg_t breg = irgen_alloc_reg(g);
            j_mov(g, breg, bbits);
            bbits = jit_value_from_reg(breg);
         }

         if (result_size < arg_size) {
            jit_value_t mask = irgen_vector_mask(result_size);
            g->map[n.id] = j_and(g, abits, mask);
            j_and(g, bbits, mask);
         }
         else if (bbits.kind != JIT_VALUE_REG) {
            jit_reg_t areg = irgen_alloc_reg(g), breg = irgen_alloc_reg(g);
            j_mov(g, areg, abits);
            j_mov(g, breg, bbits);
            g->map[n.id] = jit_value_from_reg(areg);
         }
         else
            g->map[n.id] = abits;  // No-op
      }
   }
   else if ((result_kind == MIR_TYPE_INT || result_kind == MIR_TYPE_OFFSET)
            && mir_is_vector(g->mu, arg))
      g->map[n.id] = irgen_get_value(g, arg);  // No-op (take A-bits)
   else {
      mir_dump(g->mu);
      fatal_trace("unhandled cast");
   }
}

static void irgen_op_range_null(jit_irgen_t *g, mir_value_t n)
{
   jit_value_t left  = irgen_get_arg(g, n, 0);
   jit_value_t right = irgen_get_arg(g, n, 1);

   irgen_label_t *l_downto = irgen_alloc_label(g);
   irgen_label_t *l_after  = irgen_alloc_label(g);

   mir_value_t arg2 = mir_get_arg(g->mu, n, 2);
   if (!mir_equals(arg2, g->flags)) {
      jit_value_t dir = irgen_get_value(g, arg2);
      j_cmp(g, JIT_CC_EQ, dir, jit_value_from_int64(RANGE_DOWNTO));
   }

   j_jump(g, JIT_CC_T, l_downto);

   j_cmp(g, JIT_CC_GT, left, right);
   j_jump(g, JIT_CC_NONE, l_after);

   irgen_bind_label(g, l_downto);

   j_cmp(g, JIT_CC_GT, right, left);

   irgen_bind_label(g, l_after);

   g->map[n.id] = j_cset(g);
   g->flags = n;
}

static void irgen_op_range_length(jit_irgen_t *g, mir_value_t n)
{
   jit_value_t left  = irgen_get_arg(g, n, 0);
   jit_value_t right = irgen_get_arg(g, n, 1);

   mir_value_t arg2 = mir_get_arg(g->mu, n, 2);

   jit_value_t diff;
   int64_t dconst;
   if (mir_get_const(g->mu, arg2, &dconst)) {
      jit_value_t high = dconst == RANGE_TO ? right : left;
      jit_value_t low = dconst == RANGE_TO ? left : right;

      diff = j_sub(g, high, low);
   }
   else {
      irgen_label_t *l_downto = irgen_alloc_label(g);
      irgen_label_t *l_after  = irgen_alloc_label(g);
      if (!mir_equals(arg2, g->flags)) {
         jit_value_t dir = irgen_get_value(g, arg2);
         j_cmp(g, JIT_CC_EQ, dir, jit_value_from_int64(RANGE_DOWNTO));
      }

      j_jump(g, JIT_CC_T, l_downto);

      jit_value_t diff_up = j_sub(g, right, left);
      j_jump(g, JIT_CC_NONE, l_after);

      irgen_bind_label(g, l_downto);

      jit_value_t diff_down = j_sub(g, left, right);

      irgen_bind_label(g, l_after);

      diff = j_csel(g, diff_down, diff_up);
   }

   jit_value_t length = j_add(g, diff, jit_value_from_int64(1));
   jit_value_t clamped = j_clamp(g, length);

   g->map[n.id] = clamped;
}

static void irgen_op_cond(jit_irgen_t *g, mir_value_t n)
{
   mir_value_t arg0 = mir_get_arg(g->mu, n, 0);
   if (!mir_equals(arg0, g->flags)) {
      jit_value_t test = irgen_get_value(g, arg0);
      j_cmp(g, JIT_CC_NE, test, jit_value_from_int64(0));
   }

   mir_block_t t0 = mir_cast_block(mir_get_arg(g->mu, n, 1));
   mir_block_t t1 = mir_cast_block(mir_get_arg(g->mu, n, 2));

   if (t0.id == g->curblock.id + 1)
      j_jump(g, JIT_CC_F, g->blocks[t1.id]);
   else if (t1.id == g->curblock.id + 1)
      j_jump(g, JIT_CC_T, g->blocks[t0.id]);
   else {
      j_jump(g, JIT_CC_T, g->blocks[t0.id]);
      j_jump(g, JIT_CC_NONE, g->blocks[t1.id]);
   }
}

static void irgen_op_case(jit_irgen_t *g, mir_value_t n)
{
   jit_value_t value = irgen_get_arg(g, n, 0);
   jit_reg_t reg = jit_value_as_reg(value);
   mir_block_t def = mir_cast_block(mir_get_arg(g->mu, n, 1));

   const int nargs = mir_count_args(g->mu, n);
   for (int i = 2; i < nargs; i += 2) {
      jit_value_t cmp = irgen_get_arg(g, n, i);
      mir_block_t b = mir_cast_block(mir_get_arg(g->mu, n, i + 1));
      irgen_label_t *l = g->blocks[b.id];
      macro_case(g, reg, cmp, l);
   }

   j_jump(g, JIT_CC_NONE, g->blocks[def.id]);
}

static void irgen_op_select(jit_irgen_t *g, mir_value_t n)
{
   jit_value_t iftrue  = irgen_get_arg(g, n, 1);
   jit_value_t iffalse = irgen_get_arg(g, n, 2);

   mir_value_t arg0 = mir_get_arg(g->mu, n, 0);
   if (!mir_equals(arg0, g->flags)) {
      jit_value_t test = irgen_get_arg(g, n, 0);
      j_cmp(g, JIT_CC_NE, test, jit_value_from_int64(0));
   }

   g->map[n.id] = j_csel(g, iftrue, iffalse);

   const int slots = irgen_slots_for_type(g, mir_get_type(g->mu, n));
   for (int i = 1; i < slots; i++) {
      jit_value_t iftrue_i = jit_value_from_reg(jit_value_as_reg(iftrue) + i);
      jit_value_t iffalse_i = jit_value_from_reg(jit_value_as_reg(iffalse) + i);
      j_csel(g, iftrue_i, iffalse_i);
   }
}

static void irgen_op_jump(jit_irgen_t *g, mir_value_t n)
{
   mir_block_t target = mir_cast_block(mir_get_arg(g->mu, n, 0));
   j_jump(g, JIT_CC_NONE, g->blocks[target.id]);
}

static void irgen_op_fcall(jit_irgen_t *g, mir_value_t n)
{
   irgen_emit_debuginfo(g, n);   // For stack traces

   mir_type_t rtype = mir_get_type(g->mu, n);
   if (mir_is_null(rtype)) {
      // Must call using procedure calling convention
      j_send(g, 0, jit_value_from_int64(0));
      irgen_send_args(g, n, 1);
   }
   else
      irgen_send_args(g, n, 0);

   j_call(g, irgen_get_handle(g, n, 0));

   if (!mir_is_null(rtype)) {
      const int slots = irgen_slots_for_type(g, rtype);
      g->map[n.id] = j_recv(g, 0);
      for (int i = 1; i < slots; i++)
         j_recv(g, i);   // Must be contiguous registers

      const mir_class_t rclass = mir_get_class(g->mu, rtype);
      g->used_tlab |= rclass == MIR_TYPE_UARRAY
         || rclass == MIR_TYPE_POINTER;
   }
   else {
      irgen_label_t *cont = irgen_alloc_label(g);
      jit_value_t state = j_recv(g, 0);
      j_cmp(g, JIT_CC_EQ, state, jit_null_ptr());
      j_jump(g, JIT_CC_T, cont);
      macro_exit(g, JIT_EXIT_FUNC_WAIT);
      irgen_bind_label(g, cont);
   }
}

static void irgen_op_syscall(jit_irgen_t *g, mir_value_t n)
{
   irgen_emit_debuginfo(g, n);   // For stack traces

   irgen_send_args(g, n, 0);
   macro_exit(g, JIT_EXIT_SYSCALL);

   mir_type_t rtype = mir_get_type(g->mu, n);
   if (!mir_is_null(rtype)) {
      const int slots = irgen_slots_for_type(g, rtype);
      g->map[n.id] = j_recv(g, 0);
      for (int i = 1; i < slots; i++)
         j_recv(g, i);   // Must be contiguous registers
   }
}

static void irgen_pcall_suspend(jit_irgen_t *g, jit_value_t state,
                                irgen_label_t *cont)
{
   jit_value_t pcall_ptr = irgen_pcall_ptr(g);
   j_store(g, JIT_SZ_PTR, state, pcall_ptr);

   j_cmp(g, JIT_CC_EQ, state, jit_null_ptr());
   j_jump(g, JIT_CC_T, cont);

   j_send(g, 0, g->statereg);

   j_ret(g);
}

static void irgen_op_pcall(jit_irgen_t *g, mir_value_t n)
{
   irgen_emit_debuginfo(g, n);   // For stack traces

   // First argument to procedure is suspended state
   j_send(g, 0, jit_value_from_int64(0));

   irgen_send_args(g, n, 1);

   jit_handle_t handle = irgen_get_handle(g, n, 1);
   j_call(g, handle);

   irgen_label_t *cont = irgen_alloc_label(g);
   jit_value_t state = j_recv(g, 0);

   mir_block_t resume = mir_cast_block(mir_get_arg(g->mu, n, 0));

   jit_value_t state_ptr = irgen_state_ptr(g);
   j_store(g, JIT_SZ_32, jit_value_from_int64(resume.id), state_ptr);

   irgen_pcall_suspend(g, state, cont);

   irgen_bind_label(g, cont);
   j_jump(g, JIT_CC_NONE, g->blocks[resume.id]);
}

static void irgen_op_resume(jit_irgen_t *g, mir_value_t n)
{
   jit_value_t old_state = j_load(g, JIT_SZ_PTR, irgen_pcall_ptr(g));
   irgen_label_t *cont = irgen_alloc_label(g);
   j_cmp(g, JIT_CC_EQ, old_state, jit_null_ptr());
   j_jump(g, JIT_CC_T, cont);

   jit_handle_t handle = irgen_get_handle(g, n, 0);

   j_send(g, 0, old_state);
   j_call(g, handle);

   jit_value_t new_state = j_recv(g, 0);
   irgen_pcall_suspend(g, new_state, cont);

   irgen_bind_label(g, cont);
}

static void irgen_op_wait(jit_irgen_t *g, mir_value_t n)
{
   if (mir_count_args(g->mu, n) > 1) {
      jit_value_t after = irgen_get_arg(g, n, 1);
      j_send(g, 0, after);
      macro_exit(g, JIT_EXIT_SCHED_PROCESS);
   }

   if (g->statereg.kind != JIT_VALUE_INVALID) {
      mir_block_t target = mir_cast_block(mir_get_arg(g->mu, n, 0));
      jit_value_t ptr = irgen_state_ptr(g);
      j_store(g, JIT_SZ_32, jit_value_from_int64(target.id), ptr);
   }

   if (mir_get_kind(g->mu) == MIR_UNIT_PROCEDURE)
      j_send(g, 0, g->statereg);
   else
      j_send(g, 0, jit_value_from_int64(0));

   j_ret(g);
}

static void irgen_op_protected_init(jit_irgen_t *g, mir_value_t n)
{
   jit_handle_t handle = irgen_get_handle(g, n, 0);

   irgen_send_args(g, n, 0);
   j_call(g, handle);

   g->map[n.id] = j_recv(g, 0);
}

static void irgen_op_reflect_value(jit_irgen_t *g, mir_value_t n)
{
   jit_value_t value = irgen_get_arg(g, n, 0);
   jit_value_t context = irgen_get_arg(g, n, 1);
   jit_value_t locus = irgen_get_arg(g, n, 2);

   j_send(g, 0, context);
   j_send(g, 1, value);
   j_send(g, 2, locus);

   if (mir_count_args(g->mu, n) > 3) {
      mir_value_t bounds = mir_get_arg(g->mu, n, 3);
      const int slots = irgen_slots_for_type(g, mir_get_type(g->mu, bounds));
      if (slots > 1) {
         jit_reg_t base = jit_value_as_reg(irgen_get_value(g, bounds));
         for (int j = 0; j < slots; j++)
            j_send(g, j + 3, jit_value_from_reg(base + j));
      }
      else
         j_send(g, 3, irgen_get_value(g, bounds));
   }
   else
      j_send(g, 3, value);

   macro_exit(g, JIT_EXIT_REFLECT_VALUE);

   g->map[n.id] = j_recv(g, 0);
}

static void irgen_op_reflect_subtype(jit_irgen_t *g, mir_value_t n)
{
   jit_value_t context = irgen_get_arg(g, n, 0);
   jit_value_t locus = irgen_get_arg(g, n, 1);

   j_send(g, 0, context);
   j_send(g, 1, locus);

   if (mir_count_args(g->mu, n) > 2) {
      mir_value_t bounds = mir_get_arg(g->mu, n, 2);
      const int slots = irgen_slots_for_type(g, mir_get_type(g->mu, bounds));
      if (slots > 1) {
         jit_reg_t base = jit_value_as_reg(irgen_get_value(g, bounds));
         for (int j = 0; j < slots; j++)
            j_send(g, j + 2, jit_value_from_reg(base + j));
      }
      else
         j_send(g, 2, irgen_get_value(g, bounds));
   }

   macro_exit(g, JIT_EXIT_REFLECT_SUBTYPE);

   g->map[n.id] = j_recv(g, 0);
}

static void irgen_op_process_init(jit_irgen_t *g, mir_value_t n)
{
   jit_handle_t handle = irgen_get_handle(g, n, 0);
   jit_value_t locus = irgen_get_arg(g, n, 1);

   j_send(g, 0, jit_value_from_handle(handle));
   j_send(g, 1, locus);
   macro_exit(g, JIT_EXIT_PROCESS_INIT);
}

static void irgen_op_index_check(jit_irgen_t *g, mir_value_t n)
{
   mir_value_t arg3 = mir_get_arg(g->mu, n, 3);

   jit_value_t value = irgen_get_arg(g, n, 0);
   jit_value_t left  = irgen_get_arg(g, n, 1);
   jit_value_t right = irgen_get_arg(g, n, 2);
   jit_value_t dir   = irgen_get_value(g, arg3);
   jit_value_t locus = irgen_get_arg(g, n, 4);
   jit_value_t hint  = irgen_get_arg(g, n, 5);

   if (!mir_equals(arg3, g->flags))
      j_cmp(g, JIT_CC_EQ, dir, jit_value_from_int64(RANGE_DOWNTO));

   jit_value_t low = j_csel(g, right, left);
   jit_value_t high = j_csel(g, left, right);

   irgen_label_t *l_pass = irgen_alloc_label(g);

   j_cmp(g, JIT_CC_GE, value, low);
   j_ccmp(g, JIT_CC_LE, value, high);
   j_jump(g, JIT_CC_T, l_pass);

   j_send(g, 0, value);
   j_send(g, 1, left);
   j_send(g, 2, right);
   j_send(g, 3, dir);
   j_send(g, 4, locus);
   j_send(g, 5, hint);
   macro_exit(g, JIT_EXIT_INDEX_FAIL);

   irgen_bind_label(g, l_pass);
}

static void irgen_op_dir_check(jit_irgen_t *g, mir_value_t n)
{
   jit_value_t value = irgen_get_arg(g, n, 0);
   jit_value_t dir   = irgen_get_arg(g, n, 1);
   jit_value_t locus = irgen_get_arg(g, n, 2);

   irgen_label_t *l_pass = irgen_alloc_label(g);

   j_cmp(g, JIT_CC_EQ, value, dir);
   j_jump(g, JIT_CC_T, l_pass);

   j_send(g, 0, value);
   j_send(g, 1, dir);
   j_send(g, 2, locus);
   macro_exit(g, JIT_EXIT_DIR_FAIL);

   irgen_bind_label(g, l_pass);
}

static void irgen_op_range_check(jit_irgen_t *g, mir_value_t n)
{
   mir_value_t arg0 = mir_get_arg(g->mu, n, 0);
   mir_value_t arg3 = mir_get_arg(g->mu, n, 3);

   jit_value_t value = irgen_get_value(g, arg0);
   jit_value_t left  = irgen_get_arg(g, n, 1);
   jit_value_t right = irgen_get_arg(g, n, 2);
   jit_value_t dir   = irgen_get_value(g, arg3);
   jit_value_t locus = irgen_get_arg(g, n, 4);
   jit_value_t hint  = irgen_get_arg(g, n, 5);

   if (!mir_equals(arg3, g->flags))
      j_cmp(g, JIT_CC_EQ, dir, jit_value_from_int64(RANGE_DOWNTO));

   jit_value_t low = j_csel(g, right, left);
   jit_value_t high = j_csel(g, left, right);

   irgen_label_t *l_fail = irgen_alloc_label(g);
   irgen_label_t *l_pass = irgen_alloc_label(g);

   if (mir_is(g->mu, arg0, MIR_TYPE_REAL)) {
      j_fcmp(g, JIT_CC_GE, value, low);
      j_fccmp(g, JIT_CC_LE, value, high);
      j_jump(g, JIT_CC_T, l_pass);
   }
   else {
      j_cmp(g, JIT_CC_GE, value, low);
      j_ccmp(g, JIT_CC_LE, value, high);
      j_jump(g, JIT_CC_T, l_pass);
   }

   irgen_bind_label(g, l_fail);
   j_send(g, 0, value);
   j_send(g, 1, left);
   j_send(g, 2, right);
   j_send(g, 3, dir);
   j_send(g, 4, locus);
   j_send(g, 5, hint);
   macro_exit(g, JIT_EXIT_RANGE_FAIL);

   irgen_bind_label(g, l_pass);
}

static void irgen_op_null_check(jit_irgen_t *g, mir_value_t n)
{
   jit_value_t ptr = irgen_get_arg(g, n, 0);
   jit_value_t locus = irgen_get_arg(g, n, 1);

   irgen_label_t *l_pass = irgen_alloc_label(g);

   j_cmp(g, JIT_CC_NE, ptr, jit_value_from_int64(0));
   j_jump(g, JIT_CC_T, l_pass);

   j_send(g, 0, locus);
   macro_exit(g, JIT_EXIT_NULL_DEREF);

   irgen_bind_label(g, l_pass);
}

static void irgen_op_zero_check(jit_irgen_t *g, mir_value_t n)
{
   jit_value_t value = irgen_get_arg(g, n, 0);
   jit_value_t locus = irgen_get_arg(g, n, 1);

   irgen_label_t *l_pass = irgen_alloc_label(g);

   j_cmp(g, JIT_CC_NE, value, jit_value_from_int64(0));
   j_jump(g, JIT_CC_T, l_pass);

   j_send(g, 0, locus);
   macro_exit(g, JIT_EXIT_DIV_ZERO);

   irgen_bind_label(g, l_pass);
}

static void irgen_op_exponent_check(jit_irgen_t *g, mir_value_t n)
{
   jit_value_t value = irgen_get_arg(g, n, 0);
   jit_value_t locus = irgen_get_arg(g, n, 1);

   irgen_label_t *l_pass = irgen_alloc_label(g);

   j_cmp(g, JIT_CC_GE, value, jit_value_from_int64(0));
   j_jump(g, JIT_CC_T, l_pass);

   j_send(g, 0, value);
   j_send(g, 1, locus);
   macro_exit(g, JIT_EXIT_EXPONENT_FAIL);

   irgen_bind_label(g, l_pass);
}

static void irgen_op_length_check(jit_irgen_t *g, mir_value_t n)
{
   jit_value_t llen  = irgen_get_arg(g, n, 0);
   jit_value_t rlen  = irgen_get_arg(g, n, 1);
   jit_value_t locus = irgen_get_arg(g, n, 2);

   // TODO: why isn't this encoded with an enum?
   jit_value_t dim;
   if (mir_count_args(g->mu, n) > 3)
      dim = irgen_get_arg(g, n, 3);
   else
      dim = jit_value_from_int64(0);

   irgen_label_t *l_pass = irgen_alloc_label(g);

   j_cmp(g, JIT_CC_EQ, llen, rlen);
   j_jump(g, JIT_CC_T, l_pass);

   j_send(g, 0, llen);
   j_send(g, 1, rlen);
   j_send(g, 2, dim);
   j_send(g, 3, locus);
   macro_exit(g, JIT_EXIT_LENGTH_FAIL);

   irgen_bind_label(g, l_pass);
}

static void irgen_op_package_init(jit_irgen_t *g, mir_value_t n)
{
   if (mir_count_args(g->mu, n) > 1)
      j_send(g, 0, irgen_get_arg(g, n, 1));
   else
      j_send(g, 0, jit_value_from_int64(0));

   j_call(g, irgen_get_handle(g, n, 0));

   g->map[n.id] = j_recv(g, 0);
}

static void irgen_op_link_package(jit_irgen_t *g, mir_value_t n)
{
   jit_handle_t handle = irgen_get_handle(g, n, 0);

   g->map[n.id] = macro_getpriv(g, handle);
}

static void irgen_op_link_var(jit_irgen_t *g, mir_value_t n)
{
   jit_handle_t handle = irgen_get_handle(g, n, 0);
   jit_value_t context = irgen_get_arg(g, n, 1);
   ident_t var_name = mir_get_name(g->mu, mir_get_arg(g->mu, n, 2));

   jit_func_t *f = jit_get_func(g->func->jit, handle);
   jit_fill_irbuf(f);

   link_tab_t *tab = f->linktab;
   for (; tab < f->linktab + f->nvars; tab++) {
      if (tab->name == var_name)
         break;
   }

   if (tab == f->linktab + f->nvars) {
      mir_dump(g->mu);
      fatal_trace("variable %s not found in unit %s", istr(var_name),
                  istr(f->name));
   }

   g->map[n.id] = jit_addr_from_value(context, tab->offset);
}

static void irgen_op_new(jit_irgen_t *g, mir_value_t n)
{
   mir_type_t elem = mir_get_elem(g->mu, mir_get_type(g->mu, n));

   int headersz = 0;
   if (mir_get_class(g->mu, elem) == MIR_TYPE_UARRAY) {
      headersz = irgen_size_bytes(g, elem);
      elem = mir_get_elem(g->mu, elem);
   }

   jit_value_t bytes = jit_value_from_int64(irgen_size_bytes(g, elem));

   irgen_emit_debuginfo(g, n);   // For out-of-memory stack traces

   if (mir_count_args(g->mu, n) > 0)
      bytes = j_mul(g, bytes, irgen_get_arg(g, n, 0));

   if (headersz > 0)
      bytes = j_add(g, bytes, jit_value_from_int64(headersz));

   jit_value_t mem = macro_galloc(g, bytes);
   g->map[n.id] = mem;

   if (headersz > 0) {
      // Initialise the header to point at the body
      jit_value_t ptr = jit_addr_from_value(mem, 0);
      jit_value_t body = j_add(g, mem, jit_value_from_int64(headersz));
      j_store(g, JIT_SZ_PTR, body, ptr);
   }
}

static void irgen_op_alloc(jit_irgen_t *g, mir_value_t n)
{
   mir_type_t type = mir_get_pointer(g->mu, mir_get_type(g->mu, n));

   const int bytes = irgen_size_bytes(g, type);

   jit_value_t count = irgen_get_arg(g, n, 0);
   jit_value_t total = j_mul(g, count, jit_value_from_int64(bytes));

   g->map[n.id] = macro_lalloc(g, total);
   g->used_tlab = true;
}

static void irgen_op_all(jit_irgen_t *g, mir_value_t n)
{
   jit_value_t arg = irgen_get_arg(g, n, 0);
   g->map[n.id] = jit_addr_from_value(arg, 0);
}

static void irgen_op_closure(jit_irgen_t *g, mir_value_t n)
{
   jit_handle_t handle = irgen_get_handle(g, n, 0);

   jit_reg_t base = irgen_alloc_reg(g);
   j_mov(g, base, jit_value_from_handle(handle));

   jit_reg_t context = irgen_alloc_reg(g);
   j_mov(g, context, irgen_get_arg(g, n, 1));

   g->map[n.id] = jit_value_from_reg(base);
}

static void irgen_op_resolution_wrapper(jit_irgen_t *g, mir_value_t n)
{
   jit_value_t closure = irgen_get_arg(g, n, 0);

   jit_reg_t base = irgen_alloc_reg(g);
   j_mov(g, base, closure);

   jit_reg_t context = irgen_alloc_reg(g);
   j_mov(g, context, jit_value_from_reg(jit_value_as_reg(closure) + 1));

   jit_reg_t nlits = irgen_alloc_reg(g);
   j_mov(g, nlits, irgen_get_arg(g, n, 1));

   mir_type_t type = mir_get_base(g->mu, mir_get_type(g->mu, n));

   uint32_t flagbits = 0;
   if (mir_get_class(g->mu, type) == MIR_TYPE_POINTER)
      flagbits |= R_COMPOSITE;

   jit_reg_t flags = irgen_alloc_reg(g);
   j_mov(g, flags, jit_value_from_int64(flagbits));

   g->map[n.id] = jit_value_from_reg(base);
}

static void irgen_op_init_signal(jit_irgen_t *g, mir_value_t n)
{
   jit_value_t count = irgen_get_arg(g, n, 0);
   jit_value_t size  = irgen_get_arg(g, n, 1);
   jit_value_t value = irgen_get_arg(g, n, 2);
   jit_value_t flags = irgen_get_arg(g, n, 3);
   jit_value_t locus = irgen_get_arg(g, n, 4);

   jit_value_t offset;
   if (mir_count_args(g->mu, n) > 5)
      offset = irgen_get_arg(g, n, 5);
   else
      offset = jit_value_from_int64(0);

   jit_value_t scalar = irgen_is_scalar(g, n, 2);

   j_send(g, 0, count);
   j_send(g, 1, size);
   j_send(g, 2, value);
   j_send(g, 3, flags);
   j_send(g, 4, locus);
   j_send(g, 5, offset);
   j_send(g, 6, scalar);

   macro_exit(g, JIT_EXIT_INIT_SIGNAL);

   g->map[n.id] = j_recv(g, 0);

   // Offset into signal must be next sequential register
   jit_reg_t next = irgen_alloc_reg(g);
   j_mov(g, next, jit_value_from_int64(0));
}

static void irgen_op_implicit_signal(jit_irgen_t *g, mir_value_t n)
{
   jit_value_t count   = irgen_get_arg(g, n, 0);
   jit_value_t size    = irgen_get_arg(g, n, 1);
   jit_value_t locus   = irgen_get_arg(g, n, 2);
   jit_value_t kind    = irgen_get_arg(g, n, 3);
   jit_value_t closure = irgen_get_arg(g, n, 4);
   jit_value_t context = jit_value_from_reg(jit_value_as_reg(closure) + 1);
   jit_value_t delay   = irgen_get_arg(g, n, 5);

   j_send(g, 0, count);
   j_send(g, 1, size);
   j_send(g, 2, locus);
   j_send(g, 3, kind);
   j_send(g, 4, closure);
   j_send(g, 5, context);
   j_send(g, 6, delay);

   macro_exit(g, JIT_EXIT_IMPLICIT_SIGNAL);

   g->map[n.id] = j_recv(g, 0);

   // Offset into signal must be next sequential register
   jit_reg_t next = irgen_alloc_reg(g);
   j_mov(g, next, jit_value_from_int64(0));
}

static void irgen_op_alias_signal(jit_irgen_t *g, mir_value_t n)
{
   jit_value_t shared = irgen_get_arg(g, n, 0);
   jit_value_t locus  = irgen_get_arg(g, n, 1);

   j_send(g, 0, shared);
   j_send(g, 1, locus);

   macro_exit(g, JIT_EXIT_ALIAS_SIGNAL);
}

static void irgen_op_map_signal(jit_irgen_t *g, mir_value_t n)
{
   jit_value_t src_ss  = irgen_get_arg(g, n, 0);
   jit_value_t src_off = jit_value_from_reg(jit_value_as_reg(src_ss) + 1);
   jit_value_t dst_ss  = irgen_get_arg(g, n, 1);
   jit_value_t dst_off = jit_value_from_reg(jit_value_as_reg(dst_ss) + 1);
   jit_value_t count   = irgen_get_arg(g, n, 2);

   j_send(g, 0, src_ss);
   j_send(g, 1, src_off);
   j_send(g, 2, dst_ss);
   j_send(g, 3, dst_off);
   j_send(g, 4, count);

   macro_exit(g, JIT_EXIT_MAP_SIGNAL);
}

static void irgen_op_map_const(jit_irgen_t *g, mir_value_t n)
{
   jit_value_t initval   = irgen_get_arg(g, n, 0);
   jit_value_t dst_ss    = irgen_get_arg(g, n, 1);
   jit_value_t dst_off   = jit_value_from_reg(jit_value_as_reg(dst_ss) + 1);
   jit_value_t dst_count = irgen_get_arg(g, n, 2);

   jit_value_t scalar = irgen_is_scalar(g, n, 0);

   j_send(g, 0, dst_ss);
   j_send(g, 1, dst_off);
   j_send(g, 2, initval);
   j_send(g, 3, dst_count);
   j_send(g, 4, scalar);

   macro_exit(g, JIT_EXIT_MAP_CONST);
}

static void irgen_op_map_implicit(jit_irgen_t *g, mir_value_t n)
{
   jit_value_t src_ss  = irgen_get_arg(g, n, 0);
   jit_value_t src_off = jit_value_from_reg(jit_value_as_reg(src_ss) + 1);
   jit_value_t dst_ss  = irgen_get_arg(g, n, 1);
   jit_value_t dst_off = jit_value_from_reg(jit_value_as_reg(dst_ss) + 1);
   jit_value_t count   = irgen_get_arg(g, n, 2);

   j_send(g, 0, src_ss);
   j_send(g, 1, src_off);
   j_send(g, 2, dst_ss);
   j_send(g, 3, dst_off);
   j_send(g, 4, count);

   macro_exit(g, JIT_EXIT_MAP_IMPLICIT);
}

static void irgen_op_resolve_signal(jit_irgen_t *g, mir_value_t n)
{
   jit_value_t shared  = irgen_get_arg(g, n, 0);
   jit_value_t resfn   = irgen_get_arg(g, n, 1);

   j_send(g, 0, shared);
   j_send(g, 1, resfn);

   macro_exit(g, JIT_EXIT_RESOLVE_SIGNAL);
}

static void irgen_op_unreachable(jit_irgen_t *g, mir_value_t n)
{
   jit_value_t locus;
   if (mir_count_args(g->mu, n) > 0)
      locus = irgen_get_arg(g, n, 0);
   else
      locus = jit_value_from_int64(0);

   j_send(g, 0, locus);
   macro_exit(g, JIT_EXIT_UNREACHABLE);
}

static void irgen_op_report(jit_irgen_t *g, mir_value_t n)
{
   jit_value_t severity = irgen_get_arg(g, n, 0);
   jit_value_t msg      = irgen_get_arg(g, n, 1);
   jit_value_t length   = irgen_get_arg(g, n, 2);
   jit_value_t locus    = irgen_get_arg(g, n, 3);

   j_send(g, 0, msg);
   j_send(g, 1, length);
   j_send(g, 2, severity);
   j_send(g, 3, locus);
   macro_exit(g, JIT_EXIT_REPORT);
}

static void irgen_op_assert(jit_irgen_t *g, mir_value_t n)
{
   jit_value_t severity = irgen_get_arg(g, n, 1);
   jit_value_t locus    = irgen_get_arg(g, n, 4);

   // TODO: having null arguments sucks - why not have optional args?
   jit_value_t msg    = jit_value_from_int64(0);
   jit_value_t length = jit_value_from_int64(0);
   if (!mir_is_null(mir_get_arg(g->mu, n, 2))) {
      msg    = irgen_get_arg(g, n, 2);
      length = irgen_get_arg(g, n, 3);
   }

   jit_value_t hint_left, hint_right, hint_valid;
   if (mir_count_args(g->mu, n) > 5) {
      hint_left  = irgen_get_arg(g, n, 5);
      hint_right = irgen_get_arg(g, n, 6);
      hint_valid = jit_value_from_int64(true);
   }
   else {
      hint_left = hint_right = jit_value_from_int64(0);
      hint_valid = jit_value_from_int64(false);
   }

   irgen_label_t *l_pass = irgen_alloc_label(g);

   mir_value_t arg0 = mir_get_arg(g->mu, n, 0);
   if (!mir_equals(arg0, g->flags)) {
      jit_value_t test = irgen_get_value(g, arg0);
      j_cmp(g, JIT_CC_NE, test, jit_value_from_int64(0));
   }

   j_jump(g, JIT_CC_T, l_pass);

   j_send(g, 0, msg);
   j_send(g, 1, length);
   j_send(g, 2, severity);
   j_send(g, 3, hint_left);
   j_send(g, 4, hint_right);
   j_send(g, 5, hint_valid);
   j_send(g, 6, locus);
   macro_exit(g, JIT_EXIT_ASSERT_FAIL);

   irgen_bind_label(g, l_pass);
}

static void irgen_op_file_open(jit_irgen_t *g, mir_value_t n)
{
   jit_value_t file   = irgen_get_arg(g, n, 0);
   jit_value_t name   = irgen_get_arg(g, n, 1);
   jit_value_t length = irgen_get_arg(g, n, 2);
   jit_value_t kind   = irgen_get_arg(g, n, 3);

   irgen_emit_debuginfo(g, n);   // For stack traces

   jit_value_t status = jit_null_ptr();
   if (mir_count_args(g->mu, n) == 5)
      status = irgen_get_arg(g, n, 4);

   j_send(g, 0, status);
   j_send(g, 1, file);
   j_send(g, 2, name);
   j_send(g, 3, length);
   j_send(g, 4, kind);

   macro_exit(g, JIT_EXIT_FILE_OPEN);
}

static void irgen_op_file_read(jit_irgen_t *g, mir_value_t n)
{
   mir_value_t arg0 = mir_get_arg(g->mu, n, 0);
   mir_value_t arg1 = mir_get_arg(g->mu, n, 1);

   jit_value_t file = irgen_get_value(g, arg0);
   jit_value_t ptr  = irgen_get_value(g, arg1);

   jit_value_t count = jit_value_from_int64(1);
   if (mir_count_args(g->mu, n) >= 3)
      count = irgen_get_arg(g, n, 2);

   mir_type_t file_type = mir_get_elem(g->mu, mir_get_type(g->mu, arg0));
   assert(mir_get_class(g->mu, file_type) == MIR_TYPE_FILE);

   mir_type_t elem_type = mir_get_base(g->mu, file_type);
   switch (mir_get_class(g->mu, elem_type)) {
   case MIR_TYPE_UARRAY:
   case MIR_TYPE_CARRAY:
      elem_type = mir_get_elem(g->mu, elem_type);
      break;
   default:
      break;
   }

   jit_value_t size = jit_value_from_int64(irgen_size_bytes(g, elem_type));

   j_send(g, 0, file);
   j_send(g, 1, ptr);
   j_send(g, 2, size);
   j_send(g, 3, count);

   macro_exit(g, JIT_EXIT_FILE_READ);

   if (mir_count_args(g->mu, n) >= 4) {
      mir_value_t arg3 = mir_get_arg(g->mu, n, 3);
      mir_type_t out_type = mir_get_elem(g->mu, mir_get_type(g->mu, arg3));
      jit_size_t out_size = irgen_jit_size(g, out_type);

      jit_value_t outarg = irgen_get_value(g, arg3);
      jit_value_t outptr = jit_addr_from_value(outarg, 0);
      jit_value_t len = j_recv(g, 0);
      j_store(g, out_size, len, outptr);
   }
}

static void irgen_op_file_write(jit_irgen_t *g, mir_value_t n)
{
   mir_value_t arg0 = mir_get_arg(g->mu, n, 0);
   mir_value_t arg1 = mir_get_arg(g->mu, n, 1);

   jit_value_t file = irgen_get_value(g, arg0);
   jit_value_t data = irgen_get_value(g, arg1);

   jit_value_t count;
   if (mir_count_args(g->mu, n) > 2)
      count = irgen_get_arg(g, n, 2);
   else
      count = jit_value_from_int64(1);

   mir_type_t file_type = mir_get_elem(g->mu, mir_get_type(g->mu, arg0));
   assert(mir_get_class(g->mu, file_type) == MIR_TYPE_FILE);

   mir_type_t elem_type = mir_get_base(g->mu, file_type);
   switch (mir_get_class(g->mu, elem_type)) {
   case MIR_TYPE_UARRAY:
   case MIR_TYPE_CARRAY:
      elem_type = mir_get_elem(g->mu, elem_type);
      break;
   default:
      break;
   }

   jit_value_t bytes = jit_value_from_int64(irgen_size_bytes(g, elem_type));
   jit_value_t scalar = jit_value_from_int64(mir_is_numeric(g->mu, arg1));

   j_send(g, 0, file);
   j_send(g, 1, data);
   j_send(g, 2, bytes);
   j_send(g, 3, count);
   j_send(g, 4, scalar);

   macro_exit(g, JIT_EXIT_FILE_WRITE);
}

static void irgen_op_package_scope(jit_irgen_t *g, mir_value_t n)
{
   jit_value_t locus = irgen_get_arg(g, n, 0);
   j_send(g, 0, locus);
   j_send(g, 2, jit_value_from_int64(0));
   j_send(g, 2, jit_value_from_int64(SCOPE_PACKAGE));

   macro_exit(g, JIT_EXIT_PUSH_SCOPE);
}

static void irgen_op_array_scope(jit_irgen_t *g, mir_value_t n)
{
   jit_value_t locus = irgen_get_arg(g, n, 0);
   j_send(g, 0, locus);

   const int size = irgen_size_bytes(g, mir_get_type(g->mu, n));
   j_send(g, 1, jit_value_from_int64(size));
   j_send(g, 2, jit_value_from_int64(SCOPE_ARRAY));

   macro_exit(g, JIT_EXIT_PUSH_SCOPE);
}

static void irgen_op_record_scope(jit_irgen_t *g, mir_value_t n)
{
   jit_value_t locus = irgen_get_arg(g, n, 0);
   j_send(g, 0, locus);

   const int size = irgen_size_bytes(g, mir_get_type(g->mu, n));
   j_send(g, 1, jit_value_from_int64(size));
   j_send(g, 2, jit_value_from_int64(SCOPE_RECORD));

   macro_exit(g, JIT_EXIT_PUSH_SCOPE);
}

static void irgen_op_pop_scope(jit_irgen_t *g, mir_value_t n)
{
   macro_exit(g, JIT_EXIT_POP_SCOPE);
}

static void irgen_op_drive_signal(jit_irgen_t *g, mir_value_t n)
{
   jit_value_t shared = irgen_get_arg(g, n, 0);
   jit_value_t offset = jit_value_from_reg(jit_value_as_reg(shared) + 1);
   jit_value_t count  = irgen_get_arg(g, n, 1);

   j_send(g, 0, shared);
   j_send(g, 1, offset);
   j_send(g, 2, count);
   macro_exit(g, JIT_EXIT_DRIVE_SIGNAL);
}

static void irgen_op_transfer_signal(jit_irgen_t *g, mir_value_t n)
{
   jit_value_t target  = irgen_get_arg(g, n, 0);
   jit_value_t toffset = jit_value_from_reg(jit_value_as_reg(target) + 1);
   jit_value_t source  = irgen_get_arg(g, n, 1);
   jit_value_t soffset = jit_value_from_reg(jit_value_as_reg(source) + 1);
   jit_value_t count   = irgen_get_arg(g, n, 2);
   jit_value_t reject  = irgen_get_arg(g, n, 3);
   jit_value_t after   = irgen_get_arg(g, n, 4);

   j_send(g, 0, target);
   j_send(g, 1, toffset);
   j_send(g, 2, source);
   j_send(g, 3, soffset);
   j_send(g, 4, count);
   j_send(g, 5, after);
   j_send(g, 6, reject);

   macro_exit(g, JIT_EXIT_TRANSFER_SIGNAL);
}

static void irgen_op_resolved(jit_irgen_t *g, mir_value_t n)
{
   jit_value_t shared = irgen_get_arg(g, n, 0);
   jit_value_t offset = jit_value_from_reg(jit_value_as_reg(shared) + 1);

   jit_value_t data_ptr = irgen_lea(g, jit_addr_from_value(shared, 8));

   mir_type_t type = mir_get_pointer(g->mu, mir_get_type(g->mu, n));

   const int scale = irgen_size_bytes(g, type);
   jit_value_t scaled = j_mul(g, offset, jit_value_from_int64(scale));

   g->map[n.id] = j_add(g, data_ptr, scaled);
}

static void irgen_op_last_value(jit_irgen_t *g, mir_value_t n)
{
   jit_value_t shared = irgen_get_arg(g, n, 0);
   jit_value_t offset = jit_value_from_reg(jit_value_as_reg(shared) + 1);

   jit_value_t data_ptr = irgen_lea(g, jit_addr_from_value(shared, 8));
   jit_value_t size = j_load(g, JIT_SZ_32, jit_addr_from_value(shared, 0));

   jit_value_t last_value = j_add(g, data_ptr, size);

   mir_type_t type = mir_get_elem(g->mu, mir_get_type(g->mu, n));

   const int scale = irgen_size_bytes(g, type);
   jit_value_t scaled = j_mul(g, offset, jit_value_from_int64(scale));

   g->map[n.id] = j_add(g, last_value, scaled);
}

static void irgen_op_sched_waveform(jit_irgen_t *g, mir_value_t n)
{
   jit_value_t shared = irgen_get_arg(g, n, 0);
   jit_value_t offset = jit_value_from_reg(jit_value_as_reg(shared) + 1);
   jit_value_t count  = irgen_get_arg(g, n, 1);
   jit_value_t value  = irgen_get_arg(g, n, 2);
   jit_value_t reject = irgen_get_arg(g, n, 3);
   jit_value_t after  = irgen_get_arg(g, n, 4);

   jit_value_t scalar = irgen_is_scalar(g, n, 2);

   j_send(g, 0, shared);
   j_send(g, 1, offset);
   j_send(g, 2, count);
   j_send(g, 3, value);
   j_send(g, 4, after);
   j_send(g, 5, reject);
   j_send(g, 6, scalar);

   macro_exit(g, JIT_EXIT_SCHED_WAVEFORM);
}

static void irgen_op_disconnect(jit_irgen_t *g, mir_value_t n)
{
   jit_value_t shared = irgen_get_arg(g, n, 0);
   jit_value_t offset = jit_value_from_reg(jit_value_as_reg(shared) + 1);
   jit_value_t count  = irgen_get_arg(g, n, 1);
   jit_value_t reject = irgen_get_arg(g, n, 2);
   jit_value_t after  = irgen_get_arg(g, n, 3);

   j_send(g, 0, shared);
   j_send(g, 1, offset);
   j_send(g, 2, count);
   j_send(g, 3, reject);
   j_send(g, 4, after);

   macro_exit(g, JIT_EXIT_DISCONNECT);
}

static void irgen_op_force(jit_irgen_t *g, mir_value_t n)
{
   jit_value_t shared = irgen_get_arg(g, n, 0);
   jit_value_t offset = jit_value_from_reg(jit_value_as_reg(shared) + 1);
   jit_value_t count  = irgen_get_arg(g, n, 1);
   jit_value_t value  = irgen_get_arg(g, n, 2);

   jit_value_t scalar = irgen_is_scalar(g, n, 2);

   j_send(g, 0, shared);
   j_send(g, 1, offset);
   j_send(g, 2, count);
   j_send(g, 3, value);
   j_send(g, 4, scalar);

   macro_exit(g, JIT_EXIT_FORCE);
}

static void irgen_op_release(jit_irgen_t *g, mir_value_t n)
{
   jit_value_t shared = irgen_get_arg(g, n, 0);
   jit_value_t offset = jit_value_from_reg(jit_value_as_reg(shared) + 1);
   jit_value_t count  = irgen_get_arg(g, n, 1);

   j_send(g, 0, shared);
   j_send(g, 1, offset);
   j_send(g, 2, count);

   macro_exit(g, JIT_EXIT_RELEASE);
}

static void irgen_op_deposit_signal(jit_irgen_t *g, mir_value_t n)
{
   jit_value_t shared = irgen_get_arg(g, n, 0);
   jit_value_t offset = jit_value_from_reg(jit_value_as_reg(shared) + 1);
   jit_value_t count  = irgen_get_arg(g, n, 1);
   jit_value_t value  = irgen_get_arg(g, n, 2);

   jit_value_t scalar = irgen_is_scalar(g, n, 2);

   j_send(g, 0, shared);
   j_send(g, 1, offset);
   j_send(g, 2, count);
   j_send(g, 3, value);
   j_send(g, 4, scalar);

   macro_exit(g, JIT_EXIT_DEPOSIT_SIGNAL);
}

static void irgen_op_put_conversion(jit_irgen_t *g, mir_value_t n)
{
   jit_value_t cf     = irgen_get_arg(g, n, 0);
   jit_value_t shared = irgen_get_arg(g, n, 1);
   jit_value_t offset = jit_value_from_reg(jit_value_as_reg(shared) + 1);
   jit_value_t count  = irgen_get_arg(g, n, 2);
   jit_value_t value  = irgen_get_arg(g, n, 3);

   jit_value_t scalar = irgen_is_scalar(g, n, 3);

   j_send(g, 0, cf);
   j_send(g, 1, shared);
   j_send(g, 2, offset);
   j_send(g, 3, count);
   j_send(g, 4, value);
   j_send(g, 5, scalar);

   macro_exit(g, JIT_EXIT_PUT_CONVERSION);
}

static void irgen_op_sched_event(jit_irgen_t *g, mir_value_t n)
{
   mir_value_t on = mir_get_arg(g->mu, n, 0);
   if (mir_is_signal(g->mu, on)) {
      jit_value_t shared = irgen_get_value(g, on);
      jit_value_t offset = jit_value_from_reg(jit_value_as_reg(shared) + 1);
      jit_value_t count = irgen_get_arg(g, n, 1);

      j_send(g, 0, shared);
      j_send(g, 1, offset);
      j_send(g, 2, count);
      macro_exit(g, JIT_EXIT_SCHED_EVENT);
   }
   else {
      jit_value_t trigger = irgen_get_value(g, on);

      j_send(g, 0, trigger);
      macro_exit(g, JIT_EXIT_ENABLE_TRIGGER);
   }
}

static void irgen_op_clear_event(jit_irgen_t *g, mir_value_t n)
{
   mir_value_t on = mir_get_arg(g->mu, n, 0);
   if (mir_is_signal(g->mu, on)) {
      jit_value_t shared = irgen_get_value(g, on);
      jit_value_t offset = jit_value_from_reg(jit_value_as_reg(shared) + 1);
      jit_value_t count = irgen_get_arg(g, n, 1);

      j_send(g, 0, shared);
      j_send(g, 1, offset);
      j_send(g, 2, count);
      macro_exit(g, JIT_EXIT_CLEAR_EVENT);
   }
   else {
      jit_value_t trigger = irgen_get_value(g, on);

      j_send(g, 0, trigger);
      macro_exit(g, JIT_EXIT_DISABLE_TRIGGER);
   }
}

static void irgen_op_event(jit_irgen_t *g, mir_value_t n)
{
   jit_value_t shared = irgen_get_arg(g, n, 0);
   jit_value_t offset = jit_value_from_reg(jit_value_as_reg(shared) + 1);
   jit_value_t count  = irgen_get_arg(g, n, 1);

   jit_value_t flags = j_load(g, JIT_SZ_32, jit_addr_from_value(shared, 4));

   jit_value_t cachebit = jit_value_from_int64(SIG_F_CACHE_EVENT);
   jit_value_t cacheflag = j_and(g, flags, cachebit);

   irgen_label_t *l_slow = irgen_alloc_label(g);
   irgen_label_t *l_cont = irgen_alloc_label(g);

   j_cmp(g, JIT_CC_EQ, cacheflag, jit_value_from_int64(0));
   j_jump(g, JIT_CC_T, l_slow);

   jit_value_t eventbit = jit_value_from_int64(SIG_F_EVENT_FLAG);
   jit_value_t eventflag = j_and(g, flags, eventbit);

   jit_value_t shift = jit_value_from_int64(ilog2(SIG_F_EVENT_FLAG));
   jit_value_t result = j_asr(g, eventflag, shift);
   g->map[n.id] = result;

   j_jump(g, JIT_CC_NONE, l_cont);

   irgen_bind_label(g, l_slow);

   j_send(g, 0, shared);
   j_send(g, 1, offset);
   j_send(g, 2, count);
   macro_exit(g, JIT_EXIT_TEST_EVENT);

   jit_value_t retval = j_recv(g, 0);
   j_mov(g, jit_value_as_reg(result), retval);

   irgen_bind_label(g, l_cont);
}

static void irgen_op_active(jit_irgen_t *g, mir_value_t n)
{
   jit_value_t shared = irgen_get_arg(g, n, 0);
   jit_value_t offset = jit_value_from_reg(jit_value_as_reg(shared) + 1);
   jit_value_t count  = irgen_get_arg(g, n, 1);

   j_send(g, 0, shared);
   j_send(g, 1, offset);
   j_send(g, 2, count);
   macro_exit(g, JIT_EXIT_TEST_ACTIVE);

   g->map[n.id] = j_recv(g, 0);
}

static void irgen_op_debug_out(jit_irgen_t *g, mir_value_t n)
{
   jit_value_t value = irgen_get_arg(g, n, 0);

   j_send(g, 0, value);
   macro_exit(g, JIT_EXIT_DEBUG_OUT);
}

static void irgen_op_last_event(jit_irgen_t *g, mir_value_t n)
{
   jit_value_t shared = irgen_get_arg(g, n, 0);
   jit_value_t offset = jit_value_from_reg(jit_value_as_reg(shared) + 1);

   jit_value_t count;
   if (mir_count_args(g->mu, n) > 1)
      count = irgen_get_arg(g, n, 1);
   else
      count = jit_value_from_int64(1);

   j_send(g, 0, shared);
   j_send(g, 1, offset);
   j_send(g, 2, count);
   macro_exit(g, JIT_EXIT_LAST_EVENT);

   g->map[n.id] = j_recv(g, 0);
}

static void irgen_op_last_active(jit_irgen_t *g, mir_value_t n)
{
   jit_value_t shared = irgen_get_arg(g, n, 0);
   jit_value_t offset = jit_value_from_reg(jit_value_as_reg(shared) + 1);

   jit_value_t count;
   if (mir_count_args(g->mu, n) > 1)
      count = irgen_get_arg(g, n, 1);
   else
      count = jit_value_from_int64(1);

   j_send(g, 0, shared);
   j_send(g, 1, offset);
   j_send(g, 2, count);
   macro_exit(g, JIT_EXIT_LAST_ACTIVE);

   g->map[n.id] = j_recv(g, 0);
}

static void irgen_op_driving(jit_irgen_t *g, mir_value_t n)
{
   jit_value_t shared = irgen_get_arg(g, n, 0);
   jit_value_t offset = jit_value_from_reg(jit_value_as_reg(shared) + 1);
   jit_value_t count  = irgen_get_arg(g, n, 1);

   j_send(g, 0, shared);
   j_send(g, 1, offset);
   j_send(g, 2, count);

   macro_exit(g, JIT_EXIT_DRIVING);

   g->map[n.id] = j_recv(g, 0);
}

static void irgen_op_driving_value(jit_irgen_t *g, mir_value_t n)
{
   jit_value_t shared = irgen_get_arg(g, n, 0);
   jit_value_t offset = jit_value_from_reg(jit_value_as_reg(shared) + 1);

   jit_value_t count;
   if (mir_count_args(g->mu, n) > 1)
      count = irgen_get_arg(g, n, 1);
   else
      count = jit_value_from_int64(1);

   j_send(g, 0, shared);
   j_send(g, 1, offset);
   j_send(g, 2, count);

   macro_exit(g, JIT_EXIT_DRIVING_VALUE);

   g->map[n.id] = j_recv(g, 0);
}

static void irgen_op_cover_increment(jit_irgen_t *g, mir_value_t n)
{
   uint32_t tag = irgen_get_const(g, n, 0);
   jit_value_t mem = jit_addr_from_cover_tag(tag);

   macro_sadd(g, JIT_SZ_32, mem, jit_value_from_int64(1));
}

static void irgen_op_cover_toggle(jit_irgen_t *g, mir_value_t n)
{
   jit_value_t shared = irgen_get_arg(g, n, 0);
   uint32_t tag = irgen_get_const(g, n, 1);

   j_send(g, 0, shared);
   j_send(g, 1, jit_value_from_int64(tag));
   macro_exit(g, JIT_EXIT_COVER_TOGGLE);
}

static void irgen_op_cover_state(jit_irgen_t *g, mir_value_t n)
{
   jit_value_t shared = irgen_get_arg(g, n, 0);
   jit_value_t low = irgen_get_arg(g, n, 1);
   uint32_t tag = irgen_get_const(g, n, 2);

   j_send(g, 0, shared);
   j_send(g, 1, low);
   j_send(g, 2, jit_value_from_int64(tag));
   macro_exit(g, JIT_EXIT_COVER_STATE);
}

static void irgen_op_enter_state(jit_irgen_t *g, mir_value_t n)
{
   jit_value_t state = irgen_get_arg(g, n, 0);

   jit_value_t strong;
   if (mir_count_args(g->mu, n) > 1)
      strong = irgen_get_arg(g, n, 1);
   else
      strong = jit_value_from_int64(0);

   j_send(g, 0, state);
   j_send(g, 1, strong);
   macro_exit(g, JIT_EXIT_ENTER_STATE);
}

static void irgen_op_function_trigger(jit_irgen_t *g, mir_value_t n)
{
   jit_handle_t handle = irgen_get_handle(g, n, 0);

   const int nargs = mir_count_args(g->mu, n);

   int nslots = 0;
   for (int i = 1; i < nargs; i++) {
      mir_type_t type = mir_get_type(g->mu, mir_get_arg(g->mu, n, i));
      nslots += irgen_slots_for_type(g, type);
   }

   j_send(g, 0, jit_value_from_handle(handle));
   j_send(g, 1, jit_value_from_int64(nslots));
   irgen_send_args(g, n, 2);

   macro_exit(g, JIT_EXIT_FUNCTION_TRIGGER);

   g->map[n.id] = j_recv(g, 0);
}

static void irgen_op_or_trigger(jit_irgen_t *g, mir_value_t n)
{
   jit_value_t left = irgen_get_arg(g, n, 0);
   jit_value_t right = irgen_get_arg(g, n, 1);

   j_send(g, 0, left);
   j_send(g, 1, right);

   macro_exit(g, JIT_EXIT_OR_TRIGGER);

   g->map[n.id] = j_recv(g, 0);
}

static void irgen_op_cmp_trigger(jit_irgen_t *g, mir_value_t n)
{
   jit_value_t shared = irgen_get_arg(g, n, 0);
   jit_value_t offset = jit_value_from_reg(jit_value_as_reg(shared) + 1);
   jit_value_t right = irgen_get_arg(g, n, 1);

   j_send(g, 0, shared);
   j_send(g, 1, offset);
   j_send(g, 2, right);

   macro_exit(g, JIT_EXIT_CMP_TRIGGER);

   g->map[n.id] = j_recv(g, 0);
}

static void irgen_op_level_trigger(jit_irgen_t *g, mir_value_t n)
{
   jit_value_t shared = irgen_get_arg(g, n, 0);
   jit_value_t offset = jit_value_from_reg(jit_value_as_reg(shared) + 1);
   jit_value_t count = irgen_get_arg(g, n, 1);

   j_send(g, 0, shared);
   j_send(g, 1, offset);
   j_send(g, 2, count);

   macro_exit(g, JIT_EXIT_LEVEL_TRIGGER);

   g->map[n.id] = j_recv(g, 0);
}

static void irgen_op_add_trigger(jit_irgen_t *g, mir_value_t n)
{
   jit_value_t trigger = irgen_get_arg(g, n, 0);

   j_send(g, 0, trigger);
   macro_exit(g, JIT_EXIT_ADD_TRIGGER);
}

static void irgen_op_port_conversion(jit_irgen_t *g, mir_value_t n)
{
   jit_value_t closure1 = irgen_get_arg(g, n, 0);
   jit_value_t context1 = jit_value_from_reg(jit_value_as_reg(closure1) + 1);

   jit_value_t closure2, context2;
   if (mir_count_args(g->mu, n) > 1) {
      closure2 = irgen_get_arg(g, n, 1);
      context2 = jit_value_from_reg(jit_value_as_reg(closure2) + 1);
   }
   else {
      closure2 = jit_value_from_handle(JIT_HANDLE_INVALID);
      context2 = jit_value_from_int64(0);
   }

   j_send(g, 0, closure1);
   j_send(g, 1, context1);
   j_send(g, 2, closure2);
   j_send(g, 3, context2);
   macro_exit(g, JIT_EXIT_PORT_CONVERSION);

   g->map[n.id] = j_recv(g, 0);
}

static void irgen_op_convert_in(jit_irgen_t *g, mir_value_t n)
{
   jit_value_t conv   = irgen_get_arg(g, n, 0);
   jit_value_t shared = irgen_get_arg(g, n, 1);
   jit_value_t offset = jit_value_from_reg(jit_value_as_reg(shared) + 1);
   jit_value_t count  = irgen_get_arg(g, n, 2);

   j_send(g, 0, conv);
   j_send(g, 1, shared);
   j_send(g, 2, offset);
   j_send(g, 3, count);
   macro_exit(g, JIT_EXIT_CONVERT_IN);
}

static void irgen_op_convert_out(jit_irgen_t *g, mir_value_t n)
{
   jit_value_t conv   = irgen_get_arg(g, n, 0);
   jit_value_t shared = irgen_get_arg(g, n, 1);
   jit_value_t offset = jit_value_from_reg(jit_value_as_reg(shared) + 1);
   jit_value_t count  = irgen_get_arg(g, n, 2);

   j_send(g, 0, conv);
   j_send(g, 1, shared);
   j_send(g, 2, offset);
   j_send(g, 3, count);
   macro_exit(g, JIT_EXIT_CONVERT_OUT);
}

static void irgen_op_bind_foreign(jit_irgen_t *g, mir_value_t n)
{
   jit_value_t spec   = irgen_get_arg(g, n, 0);
   jit_value_t length = irgen_get_arg(g, n, 1);

   jit_value_t locus;
   if (mir_count_args(g->mu, n) > 2)
      locus = irgen_get_arg(g, n, 2);
   else
      locus = jit_value_from_int64(0);

   j_send(g, 0, spec);
   j_send(g, 1, length);
   j_send(g, 2, locus);
   macro_exit(g, JIT_EXIT_BIND_FOREIGN);

   int pslot = 0;
   if (mir_is_null(mir_get_result(g->mu)))
      j_send(g, pslot++, jit_value_from_int64(0));    // Procedure state

   const int nparams = mir_count_params(g->mu);
   for (int i = 0; i < nparams; i++) {
      mir_value_t p = mir_get_param(g->mu, i);
      const int slots = irgen_slots_for_type(g, mir_get_type(g->mu, p));
      if (unlikely(pslot + slots >= JIT_MAX_ARGS))
         fatal("foreign subprogram %s requires more than the maximum supported "
               "%d arguments", istr(mir_get_name(g->mu, MIR_NULL_VALUE)),
               JIT_MAX_ARGS);

      jit_value_t r = irgen_get_value(g, p);
      j_send(g, pslot++, r);
      for (int j = 1; j < slots; j++)
         j_send(g, pslot++, jit_value_from_reg(jit_value_as_reg(r) + j));
   }

   macro_reexec(g);
}

static void irgen_op_instance_name(jit_irgen_t *g, mir_value_t n)
{
   irgen_send_args(g, n, 0);

   macro_exit(g, JIT_EXIT_INSTANCE_NAME);

   const int slots = irgen_slots_for_type(g, mir_get_type(g->mu, n));
   g->map[n.id] = j_recv(g, 0);
   for (int i = 1; i < slots; i++)
      j_recv(g, i);
}

static void irgen_op_bind_external(jit_irgen_t *g, mir_value_t n)
{
   jit_value_t locus = irgen_get_arg(g, n, 0);
   jit_handle_t handle = irgen_get_handle(g, n, 1);

   j_send(g, 0, locus);
   j_send(g, 1, jit_value_from_handle(handle));
   macro_exit(g, JIT_EXIT_BIND_EXTERNAL);

   g->map[n.id] = j_recv(g, 0);
}

static void irgen_op_pack(jit_irgen_t *g, mir_value_t n)
{
   assert(mir_is(g->mu, n, MIR_TYPE_VEC4));  // TODO

   mir_value_t arg = mir_get_arg(g->mu, n, 0);
   jit_value_t unpacked = irgen_get_value(g, arg);

   if (mir_is(g->mu, arg, MIR_TYPE_POINTER)) {
      const int size = mir_get_size(g->mu, mir_get_type(g->mu, n));
      macro_pack(g, unpacked, jit_value_from_int64(size));

      g->map[n.id] = j_recv(g, 0);
      j_recv(g, 1);  // B-bits
   }
   else {
      jit_value_t one = jit_value_from_int64(1);
      jit_value_t shifted = j_asr(g, unpacked, one);
      jit_value_t abits = j_and(g, unpacked, one);
      jit_value_t bbits = j_and(g, shifted, one);
      assert(jit_value_as_reg(bbits) == jit_value_as_reg(abits) + 1);

      g->map[n.id] = abits;
      (void)bbits;
   }
}

static void irgen_op_unpack(jit_irgen_t *g, mir_value_t n)
{
   mir_value_t arg = mir_get_arg(g->mu, n, 0);

   jit_value_t strength = jit_value_from_int64(irgen_get_enum(g, n, 1));

   jit_value_t abits = irgen_get_value(g, arg), bbits;
   if (mir_is(g->mu, arg, MIR_TYPE_VEC4))
      bbits = jit_value_from_reg(jit_value_as_reg(abits) + 1);
   else
      bbits = jit_value_from_int64(0);

   if (mir_is_scalar(g->mu, n)) {
      jit_value_t bshift = j_shl(g, bbits, jit_value_from_int64(1));
      jit_value_t bits = j_or(g, abits, bshift);

      g->map[n.id] = j_or(g, bits, strength);
   }
   else {
      const int size = mir_get_size(g->mu, mir_get_type(g->mu, arg));
      jit_value_t dest = irgen_get_arg(g, n, 2);
      j_send(g, 0, dest);
      j_send(g, 1, jit_value_from_int64(size));
      j_send(g, 2, strength);
      macro_unpack(g, abits, bbits);
   }
}

static jit_cc_t irgen_vector_cmp(mir_vec_op_t op)
{
   switch (op) {
   case MIR_VEC_LT:      return JIT_CC_LT;
   case MIR_VEC_LEQ:     return JIT_CC_LE;
   case MIR_VEC_GT:      return JIT_CC_GT;
   case MIR_VEC_GEQ:     return JIT_CC_GE;
   case MIR_VEC_LOG_EQ:  return JIT_CC_EQ;
   case MIR_VEC_LOG_NEQ: return JIT_CC_NE;
   default: should_not_reach_here();
   }
}

static void irgen_op_binary(jit_irgen_t *g, mir_value_t n)
{
   mir_value_t left = mir_get_arg(g->mu, n, 1);
   mir_value_t right = mir_get_arg(g->mu, n, 2);

   jit_value_t aleft = irgen_get_value(g, left);
   jit_value_t aright = irgen_get_value(g, right);

   const bool is_vec4 = mir_is(g->mu, left, MIR_TYPE_VEC4);

   jit_value_t bleft, bright, xbits;
   if (is_vec4) {
      bleft = jit_value_from_reg(jit_value_as_reg(aleft) + 1);
      bright = jit_value_from_reg(jit_value_as_reg(aright) + 1);
      xbits = j_or(g, bleft, bright);
   }
   else
      xbits = jit_value_from_int64(0);

   mir_type_t type = mir_get_type(g->mu, left);
   const int size = mir_get_size(g->mu, type);
   assert(mir_get_size(g->mu, mir_get_type(g->mu, right)) == size);

   const mir_vec_op_t op = irgen_get_enum(g, n, 0);

   if (size > 64) {
      static const jit_vec_op_t map[] = {
         [MIR_VEC_ADD] = JIT_VEC_ADD,
         [MIR_VEC_MUL] = JIT_VEC_MUL,
         [MIR_VEC_SLL] = JIT_VEC_SHL,
         [MIR_VEC_CASE_EQ] = JIT_VEC_CASE_EQ,
         [MIR_VEC_CASE_NEQ] = JIT_VEC_CASE_NEQ,
      };
      assert(op < ARRAY_LEN(map) && map[op] != 0);

      j_send(g, 0, aleft);
      j_send(g, 1, bleft);
      j_send(g, 2, aright);
      j_send(g, 3, bright);

      macro_vec4op(g, map[op], size);

      g->map[n.id] = j_recv(g, 0);
      j_recv(g, 1);

      return;
   }

   jit_value_t mask = irgen_vector_mask(size);

   bool logical = false, arith = false;
   jit_value_t abits;
   switch (op) {
   case MIR_VEC_LOG_AND:
      logical = true;
      // Fall-through
   case MIR_VEC_BIT_AND:
      abits = j_and(g, aleft, aright);
      break;
   case MIR_VEC_LOG_OR:
      logical = true;
      // Fall-through
   case MIR_VEC_BIT_OR:
      abits = j_or(g, aleft, aright);
      break;
   case MIR_VEC_BIT_XOR:
      abits = j_xor(g, aleft, aright);
      break;
   case MIR_VEC_ADD:
      arith = true;
      abits = j_add(g, aleft, aright);
      break;
   case MIR_VEC_SUB:
      arith = true;
      abits = j_sub(g, aleft, aright);
      break;
   case MIR_VEC_MUL:
      arith = true;
      abits = j_mul(g, aleft, aright);
      break;
   case MIR_VEC_CASE_EQ:
      j_cmp(g, JIT_CC_EQ, aleft, aright);
      if (is_vec4) j_ccmp(g, JIT_CC_EQ, bleft, bright);
      abits = j_cset(g);
      xbits = jit_value_from_int64(0);
      break;
   case MIR_VEC_CASE_NEQ:
      j_cmp(g, JIT_CC_EQ, aleft, aright);
      if (is_vec4) j_ccmp(g, JIT_CC_EQ, bleft, bright);
      abits = j_not(g, j_cset(g));
      xbits = jit_value_from_int64(0);
      break;
   case MIR_VEC_CASEX_EQ:
      {
         jit_value_t lcmp = j_or(g, aleft, xbits);
         jit_value_t rcmp = j_or(g, aright, xbits);
         j_cmp(g, JIT_CC_EQ, lcmp, rcmp);
         abits = j_cset(g);
         xbits = jit_value_from_int64(0);
      }
      break;
   case MIR_VEC_LT:
   case MIR_VEC_LEQ:
   case MIR_VEC_GT:
   case MIR_VEC_GEQ:
   case MIR_VEC_LOG_EQ:
   case MIR_VEC_LOG_NEQ:
      arith = true;
      j_cmp(g, irgen_vector_cmp(op), aleft, aright);
      abits = j_cset(g);
      break;
   case MIR_VEC_SLL:
   case MIR_VEC_SLA:
      arith = true;
      abits = j_shl(g, aleft, aright);
      break;
   case MIR_VEC_SRL:
      arith = true;
      abits = j_shr(g, aleft, aright);
      break;
   case MIR_VEC_SRA:
      arith = true;
      abits = j_asr(g, aleft, aright);
      break;
   default:
      should_not_reach_here();
   }

   if (logical) {
      j_cmp(g, JIT_CC_NE, abits, jit_value_from_int64(0));
      abits = j_cset(g);
   }

   if (arith) {
      jit_value_t zero = jit_value_from_int64(0);
      j_cmp(g, JIT_CC_NE, xbits, zero);
      xbits = j_csel(g, mask, zero);
   }

   abits = j_or(g, abits, xbits);

   if (is_vec4) {
      jit_reg_t bbits = irgen_alloc_reg(g);
      assert(abits.kind == JIT_VALUE_REG);
      assert(bbits == abits.reg + 1);

      j_mov(g, bbits, xbits);
   }

   g->map[n.id] = abits;
}

static void irgen_op_unary(jit_irgen_t *g, mir_value_t n)
{
   mir_value_t arg1 = mir_get_arg(g->mu, n, 1);

   const mir_vec_op_t op = irgen_get_enum(g, n, 0);
   const int isize = mir_get_size(g->mu, mir_get_type(g->mu, arg1));
   const bool is_vec4 = mir_is(g->mu, arg1, MIR_TYPE_VEC4);

   jit_value_t input = irgen_get_value(g, arg1);

   jit_value_t xbits;
   if (is_vec4)
      xbits = jit_value_from_reg(jit_value_as_reg(input) + 1);
   else
      xbits = jit_value_from_int64(0);

   if (isize > 64) {
      static const jit_vec_op_t map[] = {
         [MIR_VEC_BIT_NOT] = JIT_VEC_NOT,
         [MIR_VEC_BIT_AND] = JIT_VEC_AND1,
         [MIR_VEC_BIT_OR] = JIT_VEC_OR1,
      };
      assert(op < ARRAY_LEN(map) && map[op] != 0);

      j_send(g, 0, input);
      j_send(g, 1, xbits);
      j_send(g, 2, jit_value_from_int64(0));
      j_send(g, 3, jit_value_from_int64(0));

      macro_vec4op(g, map[op], isize);

      g->map[n.id] = j_recv(g, 0);
      j_recv(g, 1);

      return;
   }

   jit_value_t imask = irgen_vector_mask(isize), abits;
   switch (op) {
   case MIR_VEC_BIT_NOT:
      abits = j_xor(g, input, imask);
      break;
   case MIR_VEC_LOG_NOT:
      abits = j_not(g, input);
      break;
   case MIR_VEC_SUB:
      abits = j_neg(g, input);
      break;
   case MIR_VEC_BIT_OR:
      abits = j_not(g, j_not(g, input));
      break;
   case MIR_VEC_BIT_AND:
      j_cmp(g, JIT_CC_EQ, input, imask);
      abits = j_cset(g);
      break;
   case MIR_VEC_BIT_XOR:
      {
         abits = input;
         for (int n = isize; n > 1; n /= 2) {
            jit_value_t shr = j_shr(g, abits, jit_value_from_int64(n / 2));
            abits = j_xor(g, shr, abits);
         }
      }
      break;
   default:
      should_not_reach_here();
   }

   const int osize = mir_get_size(g->mu, mir_get_type(g->mu, n));
   if (osize != isize)
      abits = j_and(g, abits, irgen_vector_mask(osize));

   jit_reg_t bbits = irgen_alloc_reg(g);
   assert(abits.kind == JIT_VALUE_REG);
   assert(bbits == abits.reg + 1);

   j_mov(g, bbits, jit_value_from_int64(0));
   g->map[n.id] = abits;
}

static void irgen_op_test(jit_irgen_t *g, mir_value_t n)
{
   mir_value_t arg = mir_get_arg(g->mu, n, 0);

   jit_value_t abits = irgen_get_value(g, arg);
   jit_value_t zero = jit_value_from_int64(0);
   j_cmp(g, JIT_CC_NE, abits, zero);

   if (mir_is(g->mu, arg, MIR_TYPE_VEC4)) {
      jit_value_t bbits = jit_value_from_reg(jit_value_as_reg(abits) + 1);
      j_ccmp(g, JIT_CC_EQ, bbits, zero);
   }

   g->map[n.id] = j_cset(g);
   g->flags = n;
}

static void irgen_op_insert(jit_irgen_t *g, mir_value_t n)
{
   mir_value_t arg0 = mir_get_arg(g->mu, n, 0);
   mir_value_t arg1 = mir_get_arg(g->mu, n, 1);

   jit_value_t part = irgen_get_value(g, arg0);
   jit_value_t full = irgen_get_value(g, arg1);
   jit_value_t pos  = irgen_get_arg(g, n, 2);

   const int part_size = mir_get_size(g->mu, mir_get_type(g->mu, arg0));
   const int full_size = mir_get_size(g->mu, mir_get_type(g->mu, arg1));
   assert(part_size <= full_size);

   if (full_size > 64) {
      if (mir_is(g->mu, n, MIR_TYPE_VEC4)) {
         jit_value_t bpart = jit_value_from_reg(jit_value_as_reg(part) + 1);
         jit_value_t bfull = jit_value_from_reg(jit_value_as_reg(full) + 1);

         j_send(g, 0, full);
         j_send(g, 1, bfull);
         j_send(g, 2, part);
         j_send(g, 3, bpart);
         j_send(g, 4, jit_value_from_int64(part_size));
         j_send(g, 5, pos);

         macro_vec4op(g, JIT_VEC_INSERT, full_size);

         g->map[n.id] = j_recv(g, 0);
         j_recv(g, 1);
      }
      else
         assert(false);  // TODO

      return;
   }

   jit_value_t align = jit_value_from_int64(full_size - part_size);
   jit_value_t bitpos = j_sub(g, align, pos);
   jit_value_t mask = j_shl(g, irgen_vector_mask(part_size), bitpos);
   jit_value_t nmask = j_xor(g, mask, jit_value_from_int64(~UINT64_C(0)));

   jit_value_t abits = j_shl(g, part, bitpos);
   jit_value_t amask = j_and(g, full, nmask);
   abits = g->map[n.id] = j_or(g, amask, abits);

   if (mir_is(g->mu, n, MIR_TYPE_VEC4)) {
      jit_reg_t bbits = irgen_alloc_reg(g);
      assert(abits.kind == JIT_VALUE_REG);
      assert(bbits == abits.reg + 1);

      jit_value_t bpart = jit_value_from_reg(jit_value_as_reg(part) + 1);
      jit_value_t bfull = jit_value_from_reg(jit_value_as_reg(full) + 1);

      jit_value_t tmp = j_shl(g, bpart, bitpos);
      jit_value_t bmask = j_and(g, bfull, nmask);
      tmp = j_or(g, bmask, tmp);

      j_mov(g, bbits, tmp);
   }
}

static void irgen_op_extract(jit_irgen_t *g, mir_value_t n)
{
   mir_value_t arg0 = mir_get_arg(g->mu, n, 0);

   jit_value_t full = irgen_get_value(g, arg0);
   jit_value_t pos = irgen_get_arg(g, n, 1);

   const int arg_size = mir_get_size(g->mu, mir_get_type(g->mu, arg0));
   assert(arg_size <= 64);  // TODO

   mir_type_t type = mir_get_type(g->mu, n);
   const int size = mir_get_size(g->mu, type);

   jit_value_t bitpos = j_sub(g, jit_value_from_int64(arg_size - size), pos);
   jit_value_t mask = irgen_vector_mask(size);
   jit_value_t ashift = j_shr(g, full, bitpos);
   jit_value_t abits = g->map[n.id] = j_and(g, ashift, mask);

   if (mir_is(g->mu, n, MIR_TYPE_VEC4)) {
      jit_reg_t bbits = irgen_alloc_reg(g);
      assert(abits.kind == JIT_VALUE_REG);
      assert(bbits == abits.reg + 1);

      jit_value_t bfull = jit_value_from_reg(jit_value_as_reg(full) + 1);

      jit_value_t bshift = j_shr(g, bfull, bitpos);
      j_mov(g, bbits, j_and(g, bshift, mask));
   }
}

static void irgen_block(jit_irgen_t *g, mir_block_t block)
{
   irgen_bind_label(g, g->blocks[block.id]);

   g->flags = MIR_NULL_VALUE;
   g->curblock = block;

   const int nops = mir_count_nodes(g->mu, block);
   for (int i = 0; i < nops; i++) {
      mir_value_t n = mir_get_node(g->mu, block, i);

      const mir_op_t op = mir_get_op(g->mu, n);
      if (op == MIR_OP_COMMENT)
         continue;

#ifdef DEBUG
      mir_set_cursor(g->mu, block, i);
      irgen_emit_debuginfo(g, n);
#endif

      switch (op) {
      case MIR_OP_CONST:
         irgen_op_const(g, n);
         break;
      case MIR_OP_CONST_REAL:
         irgen_op_const_real(g, n);
         break;
      case MIR_OP_CONST_VEC:
         irgen_op_const_vec(g, n);
         break;
      case MIR_OP_CONST_ARRAY:
         irgen_op_const_array(g, n);
         break;
      case MIR_OP_CONST_REP:
         irgen_op_const_rep(g, n);
         break;
      case MIR_OP_CONST_RECORD:
         irgen_op_const_record(g, n);
         break;
      case MIR_OP_ADDRESS_OF:
         irgen_op_address_of(g, n);
         break;
      case MIR_OP_LOCUS:
         irgen_op_locus(g, n);
         break;
      case MIR_OP_TRAP_ADD:
         irgen_op_trap_add(g, n);
         break;
      case MIR_OP_NOT:
         irgen_op_not(g, n);
         break;
      case MIR_OP_AND:
         irgen_op_and(g, n);
         break;
      case MIR_OP_OR:
         irgen_op_or(g, n);
         break;
      case MIR_OP_XOR:
         irgen_op_xor(g, n);
         break;
      case MIR_OP_ADD:
         irgen_op_add(g, n);
         break;
      case MIR_OP_TRAP_MUL:
         irgen_op_trap_mul(g, n);
         break;
      case MIR_OP_MUL:
         irgen_op_mul(g, n);
         break;
      case MIR_OP_DIV:
         irgen_op_div(g, n);
         break;
      case MIR_OP_EXP:
         irgen_op_exp(g, n);
         break;
      case MIR_OP_TRAP_EXP:
         irgen_op_trap_exp(g, n);
         break;
      case MIR_OP_SUB:
         irgen_op_sub(g, n);
         break;
      case MIR_OP_TRAP_SUB:
         irgen_op_trap_sub(g, n);
         break;
      case MIR_OP_NEG:
         irgen_op_neg(g, n);
         break;
      case MIR_OP_TRAP_NEG:
         irgen_op_trap_neg(g, n);
         break;
      case MIR_OP_ABS:
         irgen_op_abs(g, n);
         break;
      case MIR_OP_MOD:
         irgen_op_mod(g, n);
         break;
      case MIR_OP_REM:
         irgen_op_rem(g, n);
         break;
      case MIR_OP_CMP:
         irgen_op_cmp(g, n);
         break;
      case MIR_OP_RETURN:
         irgen_op_return(g, n);
         break;
      case MIR_OP_STORE:
         irgen_op_store(g, n);
         break;
      case MIR_OP_LOAD:
         irgen_op_load(g, n);
         break;
      case MIR_OP_WRAP:
         irgen_op_wrap(g, n);
         break;
      case MIR_OP_UARRAY_LEFT:
         irgen_op_uarray_left(g, n);
         break;
      case MIR_OP_UARRAY_RIGHT:
         irgen_op_uarray_right(g, n);
         break;
      case MIR_OP_UARRAY_DIR:
         irgen_op_uarray_dir(g, n);
         break;
      case MIR_OP_UARRAY_LEN:
         irgen_op_uarray_len(g, n);
         break;
      case MIR_OP_UNWRAP:
         irgen_op_unwrap(g, n);
         break;
      case MIR_OP_CAST:
         irgen_op_cast(g, n);
         break;
      case MIR_OP_RANGE_NULL:
         irgen_op_range_null(g, n);
         break;
      case MIR_OP_RANGE_LENGTH:
         irgen_op_range_length(g, n);
         break;
      case MIR_OP_COND:
         irgen_op_cond(g, n);
         break;
      case MIR_OP_SELECT:
         irgen_op_select(g, n);
         break;
      case MIR_OP_JUMP:
         irgen_op_jump(g, n);
         break;
      case MIR_OP_ARRAY_REF:
         irgen_op_array_ref(g, n);
         break;
      case MIR_OP_CONTEXT_UPREF:
         irgen_op_context_upref(g, n);
         break;
      case MIR_OP_FCALL:
         irgen_op_fcall(g, n);
         break;
      case MIR_OP_PCALL:
         irgen_op_pcall(g, n);
         break;
      case MIR_OP_RESUME:
         irgen_op_resume(g, n);
         break;
      case MIR_OP_SYSCALL:
         irgen_op_syscall(g, n);
         break;
      case MIR_OP_WAIT:
         irgen_op_wait(g, n);
         break;
      case MIR_OP_COPY:
         irgen_op_copy(g, n);
         break;
      case MIR_OP_SET:
         irgen_op_set(g, n);
         break;
      case MIR_OP_VAR_UPREF:
         irgen_op_var_upref(g, n);
         break;
      case MIR_OP_INDEX_CHECK:
         irgen_op_index_check(g, n);
         break;
      case MIR_OP_DIR_CHECK:
         irgen_op_dir_check(g, n);
         break;
      case MIR_OP_RANGE_CHECK:
         irgen_op_range_check(g, n);
         break;
      case MIR_OP_PACKAGE_INIT:
         irgen_op_package_init(g, n);
         break;
      case MIR_OP_LINK_PACKAGE:
         irgen_op_link_package(g, n);
         break;
      case MIR_OP_LINK_VAR:
         irgen_op_link_var(g, n);
         break;
      case MIR_OP_RECORD_REF:
         irgen_op_record_ref(g, n);
         break;
      case MIR_OP_NULL:
         irgen_op_null(g, n);
         break;
      case MIR_OP_NEW:
         irgen_op_new(g, n);
         break;
      case MIR_OP_ALLOC:
         irgen_op_alloc(g, n);
         break;
      case MIR_OP_ALL:
         irgen_op_all(g, n);
         break;
      case MIR_OP_EXPONENT_CHECK:
         irgen_op_exponent_check(g, n);
         break;
      case MIR_OP_NULL_CHECK:
         irgen_op_null_check(g, n);
         break;
      case MIR_OP_ZERO_CHECK:
         irgen_op_zero_check(g, n);
         break;
      case MIR_OP_LENGTH_CHECK:
         irgen_op_length_check(g, n);
         break;
      case MIR_OP_CLOSURE:
         irgen_op_closure(g, n);
         break;
      case MIR_OP_RESOLUTION_WRAPPER:
         irgen_op_resolution_wrapper(g, n);
         break;
      case MIR_OP_INIT_SIGNAL:
         irgen_op_init_signal(g, n);
         break;
      case MIR_OP_IMPLICIT_SIGNAL:
         irgen_op_implicit_signal(g, n);
         break;
      case MIR_OP_ALIAS_SIGNAL:
         irgen_op_alias_signal(g, n);
         break;
      case MIR_OP_MAP_SIGNAL:
         irgen_op_map_signal(g, n);
         break;
      case MIR_OP_MAP_CONST:
         irgen_op_map_const(g, n);
         break;
      case MIR_OP_MAP_IMPLICIT:
         irgen_op_map_implicit(g, n);
         break;
      case MIR_OP_RESOLVE_SIGNAL:
         irgen_op_resolve_signal(g, n);
         break;
      case MIR_OP_UNREACHABLE:
         irgen_op_unreachable(g, n);
         break;
      case MIR_OP_REPORT:
         irgen_op_report(g, n);
         break;
      case MIR_OP_ASSERT:
         irgen_op_assert(g, n);
         break;
      case MIR_OP_CASE:
         irgen_op_case(g, n);
         break;
      case MIR_OP_PROTECTED_INIT:
         irgen_op_protected_init(g, n);
         break;
      case MIR_OP_REFLECT_VALUE:
         irgen_op_reflect_value(g, n);
         break;
      case MIR_OP_REFLECT_SUBTYPE:
         irgen_op_reflect_subtype(g, n);
         break;
      case MIR_OP_PROCESS_INIT:
         irgen_op_process_init(g, n);
         break;
      case MIR_OP_FILE_OPEN:
         irgen_op_file_open(g, n);
         break;
      case MIR_OP_FILE_READ:
         irgen_op_file_read(g, n);
         break;
      case MIR_OP_FILE_WRITE:
         irgen_op_file_write(g, n);
         break;
      case MIR_OP_PACKAGE_SCOPE:
         irgen_op_package_scope(g, n);
         break;
      case MIR_OP_ARRAY_SCOPE:
         irgen_op_array_scope(g, n);
         break;
      case MIR_OP_RECORD_SCOPE:
         irgen_op_record_scope(g, n);
         break;
      case MIR_OP_POP_SCOPE:
         irgen_op_pop_scope(g, n);
         break;
      case MIR_OP_DRIVE_SIGNAL:
         irgen_op_drive_signal(g, n);
         break;
      case MIR_OP_TRANSFER_SIGNAL:
         irgen_op_transfer_signal(g, n);
         break;
      case MIR_OP_RESOLVED:
         irgen_op_resolved(g, n);
         break;
      case MIR_OP_LAST_VALUE:
         irgen_op_last_value(g, n);
         break;
      case MIR_OP_SCHED_WAVEFORM:
         irgen_op_sched_waveform(g, n);
         break;
      case MIR_OP_DISCONNECT:
         irgen_op_disconnect(g, n);
         break;
      case MIR_OP_FORCE:
         irgen_op_force(g, n);
         break;
      case MIR_OP_RELEASE:
         irgen_op_release(g, n);
         break;
      case MIR_OP_DEPOSIT_SIGNAL:
         irgen_op_deposit_signal(g, n);
         break;
      case MIR_OP_PUT_CONVERSION:
         irgen_op_put_conversion(g, n);
         break;
      case MIR_OP_EVENT:
         irgen_op_event(g, n);
         break;
      case MIR_OP_ACTIVE:
         irgen_op_active(g, n);
         break;
      case MIR_OP_SCHED_EVENT:
         irgen_op_sched_event(g, n);
         break;
      case MIR_OP_CLEAR_EVENT:
         irgen_op_clear_event(g, n);
         break;
      case MIR_OP_DEBUG_OUT:
         irgen_op_debug_out(g, n);
         break;
      case MIR_OP_LAST_EVENT:
         irgen_op_last_event(g, n);
         break;
      case MIR_OP_LAST_ACTIVE:
         irgen_op_last_active(g, n);
         break;
      case MIR_OP_DRIVING:
         irgen_op_driving(g, n);
         break;
      case MIR_OP_DRIVING_VALUE:
         irgen_op_driving_value(g, n);
         break;
      case MIR_OP_COVER_STMT:
      case MIR_OP_COVER_BRANCH:
      case MIR_OP_COVER_EXPR:
         irgen_op_cover_increment(g, n);
         break;
      case MIR_OP_COVER_TOGGLE:
         irgen_op_cover_toggle(g, n);
         break;
      case MIR_OP_COVER_STATE:
         irgen_op_cover_state(g, n);
         break;
      case MIR_OP_ENTER_STATE:
         irgen_op_enter_state(g, n);
         break;
      case MIR_OP_FUNCTION_TRIGGER:
         irgen_op_function_trigger(g, n);
         break;
      case MIR_OP_OR_TRIGGER:
         irgen_op_or_trigger(g, n);
         break;
      case MIR_OP_CMP_TRIGGER:
         irgen_op_cmp_trigger(g, n);
         break;
      case MIR_OP_LEVEL_TRIGGER:
         irgen_op_level_trigger(g, n);
         break;
      case MIR_OP_ADD_TRIGGER:
         irgen_op_add_trigger(g, n);
         break;
      case MIR_OP_PORT_CONVERSION:
         irgen_op_port_conversion(g, n);
         break;
      case MIR_OP_CONVERT_IN:
         irgen_op_convert_in(g, n);
         break;
      case MIR_OP_CONVERT_OUT:
         irgen_op_convert_out(g, n);
         break;
      case MIR_OP_BIND_FOREIGN:
         irgen_op_bind_foreign(g, n);
         break;
      case MIR_OP_INSTANCE_NAME:
         irgen_op_instance_name(g, n);
         break;
      case MIR_OP_BIND_EXTERNAL:
         irgen_op_bind_external(g, n);
         break;
      case MIR_OP_PACK:
         irgen_op_pack(g, n);
         break;
      case MIR_OP_UNPACK:
         irgen_op_unpack(g, n);
         break;
      case MIR_OP_BINARY:
         irgen_op_binary(g, n);
         break;
      case MIR_OP_UNARY:
         irgen_op_unary(g, n);
         break;
      case MIR_OP_INSERT:
         irgen_op_insert(g, n);
         break;
      case MIR_OP_EXTRACT:
         irgen_op_extract(g, n);
         break;
      case MIR_OP_TEST:
         irgen_op_test(g, n);
         break;
      default:
         DEBUG_ONLY(mir_dump(g->mu));
         fatal_trace("cannot generate JIT IR for MIR op %s", mir_op_string(op));
      }
   }

   DEBUG_ONLY(if (nops == 0) j_trap(g));
}

static void irgen_locals(jit_irgen_t *g)
{
   const int nvars = mir_count_vars(g->mu);
   g->vars = xmalloc_array(nvars, sizeof(jit_value_t));

   bool on_stack;
   const mir_unit_kind_t kind = mir_get_kind(g->mu);
   switch (kind) {
   case MIR_UNIT_PROCESS:
   case MIR_UNIT_PROPERTY:
      on_stack = g->stateless;
      break;
   case MIR_UNIT_INSTANCE:
   case MIR_UNIT_PROCEDURE:
   case MIR_UNIT_PACKAGE:
   case MIR_UNIT_PROTECTED:
      on_stack = false;
      break;
   default:
      on_stack = !g->needs_context;
      break;
   }

   if (on_stack) {
      // Local variables on stack
      for (int i = 0; i < nvars; i++) {
         mir_value_t var = mir_get_var(g->mu, i);
         mir_type_t type = mir_get_var_type(g->mu, var);
         const int sz = irgen_size_bytes(g, type);
         if ((mir_get_var_flags(g->mu, var) & MIR_VAR_HEAP)
             || sz > MAX_STACK_ALLOC) {
            g->vars[i] = macro_lalloc(g, jit_value_from_int64(sz));
            g->used_tlab = true;
         }
         else
            g->vars[i] = macro_salloc(g, sz);
      }
   }
   else {
      // Local variables on heap
      size_t sz = 0;
      sz += sizeof(void *);   // Context parameter
      if (kind == MIR_UNIT_PROCESS || kind == MIR_UNIT_PROCEDURE) {
         sz += sizeof(void *);   // Suspended procedure state
         sz += sizeof(int32_t);  // State number
      }

      link_tab_t *linktab = xmalloc_array(nvars, sizeof(link_tab_t));

      for (int i = 0; i < nvars; i++) {
         mir_value_t var = mir_get_var(g->mu, i);
         mir_type_t type = mir_get_var_type(g->mu, var);
         const int align = irgen_align_of(g, type);
         sz = ALIGN_UP(sz, align);
         linktab[i].name = mir_get_name(g->mu, var);
         linktab[i].offset = sz;
         sz += irgen_size_bytes(g, type);
      }

      jit_value_t mem;
      if (kind == MIR_UNIT_PROTECTED) {
         // Protected types must always be allocated on the global heap
         // as the result may be stored in an access
         mem = macro_galloc(g, jit_value_from_int64(sz));
      }
      else {
         mem = macro_lalloc(g, jit_value_from_int64(sz));
         g->used_tlab = true;
      }

      if (g->statereg.kind != JIT_VALUE_INVALID) {
         // A null state was passed in by the caller
         j_mov(g, jit_value_as_reg(g->statereg), mem);
      }
      else
         g->statereg = mem;

      for (int i = 0; i < nvars; i++)
         g->vars[i] = jit_addr_from_value(g->statereg, linktab[i].offset);

      // Publish the variable offset table early to handle circular references
      // This may be read by another thread while in the COMPILING state
      g->func->nvars = nvars;
      store_release(&(g->func->linktab), linktab);
   }
}

static void irgen_params(jit_irgen_t *g, int first)
{
   // We never need more than the first few argument types
   ffi_type_t types[7];
   types[first] = FFI_POINTER;
   types[0] = FFI_VOID;

   jit_value_t spill = { .kind = JIT_VALUE_INVALID };
   int32_t spilloff = 0;

   const int nparams = mir_count_params(g->mu);
   g->params = xmalloc_array(nparams, sizeof(jit_value_t));

   for (int i = 0, pslot = first; i < nparams; i++) {
      mir_value_t p = mir_get_param(g->mu, i);
      mir_type_t type = mir_get_type(g->mu, p);
      int slots = irgen_slots_for_type(g, type);

      if (pslot + slots >= JIT_MAX_ARGS - 1) {
         // Large number of arguments spill to the heap
         if (spill.kind == JIT_VALUE_INVALID) {
            spill = j_recv(g, JIT_MAX_ARGS - 1);
            pslot = JIT_MAX_ARGS;
         }

         jit_value_t ptr = jit_addr_from_value(spill, spilloff);
         g->params[p.id] = j_load(g, JIT_SZ_64, ptr);
         spilloff += sizeof(jit_scalar_t);

         for (int i = 1; i < slots; i++, spilloff += sizeof(jit_scalar_t)) {
            jit_value_t ptr = jit_addr_from_value(spill, spilloff);
            j_load(g, JIT_SZ_64, ptr);   // Must be contiguous registers
         }
      }
      else {
         g->params[p.id] = j_recv(g, pslot++);
         for (int i = 1; i < slots; i++)
            j_recv(g, pslot++);   // Must be contiguous registers
      }

      if (i + first + 1 < ARRAY_LEN(types))
         types[i + first + 1] = irgen_ffi_type(g, type);
   }

   mir_unit_kind_t kind = mir_get_kind(g->mu);
   if (kind == MIR_UNIT_FUNCTION || kind == MIR_UNIT_THUNK) {
      mir_type_t rtype = mir_get_result(g->mu);
      if (!mir_is_null(rtype))
         types[0] = irgen_ffi_type(g, rtype);
   }

   const int ntypes = MIN(ARRAY_LEN(types), first + nparams + 1);
   g->func->spec = ffi_spec_new(types, ntypes);
}

static void irgen_jump_table(jit_irgen_t *g)
{
   g->statereg = j_recv(g, 0);

   irgen_label_t *cont = irgen_alloc_label(g);
   j_cmp(g, JIT_CC_EQ, g->statereg, jit_value_from_int64(0));
   j_jump(g, JIT_CC_T, cont);

   jit_value_t state_ptr = irgen_state_ptr(g);
   jit_value_t state = j_load(g, JIT_SZ_32, state_ptr);
   jit_reg_t state_reg = jit_value_as_reg(state);

   const bool is_process = (mir_get_kind(g->mu) == MIR_UNIT_PROCESS);
   const int nblocks = mir_count_blocks(g->mu);

   bit_mask_t have;
   mask_init(&have, nblocks);

   for (int i = 0; i < nblocks; i++) {
      mir_block_t b = mir_get_block(g->mu, i);

      const int num_nodes = mir_count_nodes(g->mu, b);
      if (num_nodes == 0)
         continue;

      mir_value_t last = mir_get_node(g->mu, b, num_nodes - 1);

      mir_block_t target = MIR_NULL_BLOCK;
      const mir_op_t last_op = mir_get_op(g->mu, last);

      if (last_op == MIR_OP_WAIT || last_op == MIR_OP_PCALL)
         target = mir_cast_block(mir_get_arg(g->mu, last, 0));
      else if (is_process && last_op == MIR_OP_RETURN)
         target = mir_get_block(g->mu, 1);

      if (mir_is_null(target) || mask_test(&have, target.id))
         continue;

      macro_case(g, state_reg, jit_value_from_int64(target.id),
                 g->blocks[target.id]);

      mask_set(&have, target.id);
   }

   DEBUG_ONLY(j_trap(g));

   irgen_bind_label(g, cont);
   mask_free(&have);
}

static void irgen_analyse(jit_irgen_t *g)
{
   // A process is stateless if it has no non-temporary variables and
   // all wait statements resume at the initial block
   const mir_unit_kind_t kind = mir_get_kind(g->mu);
   g->stateless = (kind == MIR_UNIT_PROCESS || kind == MIR_UNIT_PROPERTY);

   const int nvars = mir_count_vars(g->mu);
   for (int i = 0; i < nvars; i++) {
      mir_value_t var = mir_get_var(g->mu, i);
      if (!(mir_get_var_flags(g->mu, var) & MIR_VAR_TEMP)) {
         g->stateless = false;
         break;
      }
   }

   const int nblocks = mir_count_blocks(g->mu);
   for (int i = 0; i < nblocks; i++) {
      mir_block_t b = mir_get_block(g->mu, i);

      const int nops = mir_count_nodes(g->mu, b);
      for (int j = 0; j < nops; j++) {
         mir_value_t n = mir_get_node(g->mu, b, j);
         const mir_op_t op = mir_get_op(g->mu, n);
         if (j == nops - 1 && op == MIR_OP_WAIT) {
            mir_block_t target = mir_cast_block(mir_get_arg(g->mu, n, 0));
            if (target.id != 1) {
               // This a kludge: remove it when MIR optimises better
               if (mir_count_nodes(g->mu, target) != 1)
                  g->stateless = false;
               else {
                  mir_value_t n0 = mir_get_node(g->mu, target, 0);
                  if (mir_get_op(g->mu, n0) != MIR_OP_JUMP)
                     g->stateless = false;
                  else if (mir_cast_block(mir_get_arg(g->mu, n0, 0)).id != 1)
                     g->stateless = false;
               }
            }
         }
         else if (j == nops - 1 && op == MIR_OP_PCALL)
            g->stateless = false;
         else if (op == MIR_OP_CONTEXT_UPREF) {
            const int hops = irgen_get_const(g, n, 0);
            if (hops == 0) {
               g->needs_context = true;
               g->stateless = false;
            }
         }
      }
   }
}

static void irgen_instance_entry(jit_irgen_t *g)
{
   const ffi_type_t types[] = { FFI_POINTER, FFI_POINTER };
   g->func->spec = ffi_spec_new(types, ARRAY_LEN(types));

#ifdef DEBUG
   // Instances should only be initialised once
   irgen_label_t *cont = irgen_alloc_label(g);
   jit_value_t priv = macro_getpriv(g, g->func->handle);
   j_cmp(g, JIT_CC_EQ, priv, jit_value_from_int64(0));
   j_jump(g, JIT_CC_T, cont);
   j_trap(g);
   irgen_bind_label(g, cont);
#endif

   irgen_locals(g);

   // Stash context pointer
   jit_value_t context = j_recv(g, 0);
   j_store(g, JIT_SZ_PTR, context, jit_addr_from_value(g->statereg, 0));

   macro_putpriv(g, g->func->handle, g->statereg);
}

static void irgen_process_entry(jit_irgen_t *g)
{
   const ffi_type_t types[] = { FFI_POINTER, FFI_POINTER, FFI_POINTER };
   g->func->spec = ffi_spec_new(types, ARRAY_LEN(types));

   if (!g->stateless)
      irgen_jump_table(g);

   irgen_locals(g);

   if (g->stateless) {
      g->contextarg = j_recv(g, 1);

      jit_value_t state = j_recv(g, 0);
      j_cmp(g, JIT_CC_EQ, state, jit_null_ptr());
      j_jump(g, JIT_CC_F, g->blocks[1]);
   }
   else {
      // Stash context pointer
      jit_value_t context = j_recv(g, 1);
      j_store(g, JIT_SZ_PTR, context, jit_addr_from_value(g->statereg, 0));
   }
}

static void irgen_property_entry(jit_irgen_t *g)
{
   irgen_params(g, 1);

   if (g->stateless)
      irgen_locals(g);

   jit_value_t state = j_recv(g, 0);
   j_cmp(g, JIT_CC_EQ, state, jit_null_ptr());
   j_jump(g, JIT_CC_F, g->blocks[1]);

   if (g->stateless)
      g->contextarg = g->params[0];
   else
      g->statereg = state;

   if (!g->stateless) {
      irgen_locals(g);

      // Stash context pointer
      jit_value_t context = jit_addr_from_value(g->statereg, 0);
      j_store(g, JIT_SZ_PTR, g->params[0], context);
   }
}

static void irgen_function_entry(jit_irgen_t *g)
{
   const bool is_procedure = mir_is_null(mir_get_result(g->mu));
   const int first_param = is_procedure ? 1 : 0;
   irgen_params(g, first_param);

   irgen_locals(g);

   if (g->statereg.kind != JIT_VALUE_INVALID) {
      // Stash context pointer
      j_store(g, JIT_SZ_PTR, g->params[0], jit_addr_from_value(g->statereg, 0));
   }
}

static void irgen_procedure_entry(jit_irgen_t *g)
{
   irgen_jump_table(g);
   irgen_params(g, 1);
   irgen_locals(g);

   // Stash context pointer
   j_store(g, JIT_SZ_PTR, g->params[0], jit_addr_from_value(g->statereg, 0));
}

static void irgen_thunk_entry(jit_irgen_t *g)
{
   g->contextarg = j_recv(g, 0);
   irgen_locals(g);
}

static void irgen_package_entry(jit_irgen_t *g)
{
   // It's harmless to initialise a package multiple times, just return
   // the existing context pointer
   irgen_label_t *cont = irgen_alloc_label(g);
   jit_value_t priv = macro_getpriv(g, g->func->handle);
   j_cmp(g, JIT_CC_EQ, priv, jit_value_from_int64(0));
   j_jump(g, JIT_CC_T, cont);
   j_send(g, 0, priv);
   j_ret(g);
   irgen_bind_label(g, cont);

   irgen_locals(g);

   // Stash context pointer
   jit_value_t context = j_recv(g, 0);
   j_store(g, JIT_SZ_PTR, context, jit_addr_from_value(g->statereg, 0));

   macro_putpriv(g, g->func->handle, g->statereg);
}

static void irgen_protected_entry(jit_irgen_t *g)
{
   const ffi_type_t types[] = { FFI_POINTER };
   g->func->spec = ffi_spec_new(types, ARRAY_LEN(types));

   irgen_params(g, 1);
   irgen_locals(g);

   // Stash context pointer
   jit_value_t context = j_recv(g, 0);
   j_store(g, JIT_SZ_PTR, context, jit_addr_from_value(g->statereg, 0));
}

void jit_irgen(jit_func_t *f, mir_unit_t *mu)
{
   assert(load_acquire(&f->state) == JIT_FUNC_COMPILING);
   assert(f->irbuf == NULL);

   const bool debug_log = opt_get_int(OPT_JIT_LOG) && f->name != NULL;
   const uint64_t start_ticks = debug_log ? get_timestamp_us() : 0;

   const int num_nodes = mir_count_nodes(mu, MIR_NULL_BLOCK);

   jit_irgen_t *g = xcalloc(sizeof(jit_irgen_t));
   g->func = f;
   g->map  = xmalloc_array(num_nodes, sizeof(jit_value_t));
   g->mu   = mu;

   const int nblocks = mir_count_blocks(g->mu);
   g->blocks = xmalloc_array(nblocks, sizeof(irgen_label_t *));

   for (int i = 0; i < nblocks; i++)
      g->blocks[i] = irgen_alloc_label(g);

   irgen_analyse(g);

   switch (mir_get_kind(g->mu)) {
   case MIR_UNIT_INSTANCE:
      irgen_instance_entry(g);
      break;
   case MIR_UNIT_PROCESS:
      irgen_process_entry(g);
      break;
   case MIR_UNIT_FUNCTION:
      irgen_function_entry(g);
      break;
   case MIR_UNIT_PROCEDURE:
      irgen_procedure_entry(g);
      break;
   case MIR_UNIT_THUNK:
      irgen_thunk_entry(g);
      break;
   case MIR_UNIT_PACKAGE:
      irgen_package_entry(g);
      break;
   case MIR_UNIT_PROTECTED:
      irgen_protected_entry(g);
      break;
   case MIR_UNIT_PROPERTY:
      irgen_property_entry(g);
      break;
   default:
      should_not_reach_here();
   }

   for (int i = 0; i < nblocks; i++)
      irgen_block(g, mir_get_block(mu, i));

   f->nregs   = g->next_reg;
   f->cpoolsz = g->cpoolptr;
   f->object  = mir_get_object(g->mu);

   for (irgen_label_t *it = g->labels, *tmp; it; it = tmp) {
      assert(it->label < g->func->nirs);
      if (it->uses > 0)
         g->func->irbuf[it->label].target = 1;
      tmp = it->next;
      free(it);
   }
   g->labels = NULL;

   if (mir_get_kind(mu) != MIR_UNIT_THUNK) {
      jit_do_mem2reg(f);
      jit_do_lvn(f);
      jit_do_cprop(f);
      jit_do_dce(f);
      jit_delete_nops(f);
   }

   // Function can be executed immediately after this store
   store_release(&(f->state), JIT_FUNC_READY);

   if (opt_get_verbose(OPT_JIT_VERBOSE, istr(f->name))) {
#ifdef DEBUG
      jit_dump_interleaved(f, mu);
#else
      jit_dump(f);
#endif
   }

   if (debug_log) {
      const int ticks = get_timestamp_us() - start_ticks;
      diag_t *d = diag_new(DIAG_DEBUG, NULL);
      diag_printf(d, "%s: %d instructions", istr(f->name), f->nirs);
      if (f->cpoolsz > 0)
         diag_printf(d, "; %d cpool bytes", f->cpoolsz);
      diag_printf(d, " [%d us]", ticks);
      diag_emit(d);
   }

   free(g->blocks);
   free(g->map);
   free(g->vars);
   free(g->params);
   free(g);
}
