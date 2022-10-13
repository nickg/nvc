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
#include "diag.h"
#include "jit/jit-ffi.h"
#include "jit/jit-ffi.h"
#include "jit/jit-priv.h"
#include "lib.h"
#include "mask.h"
#include "opt.h"
#include "tree.h"
#include "vcode.h"

#include <assert.h>
#include <inttypes.h>
#include <stdlib.h>
#include <string.h>

typedef struct _irgen_label irgen_label_t;
typedef struct _patch_list  patch_list_t;

#define PATCH_CHUNK_SZ 4

struct _patch_list {
   patch_list_t *next;
   unsigned      count;
   unsigned      offsets[PATCH_CHUNK_SZ];
};

struct _irgen_label {
   jit_label_t    label;
   patch_list_t   patchlist;
   irgen_label_t *next;
};

typedef struct {
   jit_func_t     *func;
   jit_value_t    *vars;
   jit_reg_t       next_reg;
   jit_value_t    *map;
   irgen_label_t  *labels;
   irgen_label_t **blocks;
   size_t          cpoolptr;
   jit_value_t     statereg;
   vcode_reg_t     flags;
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

static inline jit_value_t jit_value_from_frame_addr(int offset)
{
   jit_value_t value = { .kind = JIT_ADDR_FRAME, .int64 = offset };
   return value;
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
   jit_value_t value = { .kind = JIT_VALUE_INT64, .int64 = 0 };
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
   case JIT_ADDR_FRAME:
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
   case JIT_ADDR_FRAME:
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

static jit_value_t jit_addr_from_ptr(void *ptr)
{
   return (jit_value_t){ .kind = JIT_ADDR_ABS, .int64 = (intptr_t)ptr };
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

static jit_ir_t *irgen_append(jit_func_t *f)
{
   if (f->nirs == f->bufsz) {
      f->bufsz = MAX(f->bufsz * 2, 1024);
      f->irbuf = xrealloc_array(f->irbuf, f->bufsz, sizeof(jit_ir_t));
   }

   return &(f->irbuf[f->nirs++]);
}

static jit_reg_t irgen_alloc_reg(jit_irgen_t *g)
{
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
         else
            fatal_trace("cannot find label argument to patch");
      }
   }

   for (patch_list_t *p = l->patchlist.next, *tmp; p; p = tmp) {
      tmp = p->next;
      free(p);
   }
}

static jit_cc_t irgen_get_jit_cc(vcode_cmp_t cmp)
{
   switch (cmp) {
   case VCODE_CMP_EQ: return JIT_CC_EQ;
   case VCODE_CMP_NEQ: return JIT_CC_NE;
   case VCODE_CMP_LT: return JIT_CC_LT;
   case VCODE_CMP_GT: return JIT_CC_GT;
   case VCODE_CMP_LEQ: return JIT_CC_LE;
   case VCODE_CMP_GEQ: return JIT_CC_GE;
   default: return JIT_CC_NONE;
   }
}

static void irgen_emit_nullary(jit_irgen_t *g, jit_op_t op, jit_cc_t cc,
                               jit_reg_t result)
{
   jit_ir_t *ir = irgen_append(g->func);
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
   jit_ir_t *ir = irgen_append(g->func);
   ir->op        = op;
   ir->size      = sz;
   ir->cc        = cc;
   ir->target    = 0;
   ir->result    = result;
   ir->arg1      = arg;
   ir->arg2.kind = JIT_VALUE_INVALID;

   return ir;
}

static void irgen_emit_binary(jit_irgen_t *g, jit_op_t op, jit_size_t sz,
                              jit_cc_t cc, jit_reg_t result, jit_value_t arg1,
                              jit_value_t arg2)
{
   jit_ir_t *ir = irgen_append(g->func);
   ir->op     = op;
   ir->size   = sz;
   ir->cc     = cc;
   ir->target = 0;
   ir->size   = sz;
   ir->result = result;
   ir->arg1   = arg1;
   ir->arg2   = arg2;
}

static jit_value_t j_recv(jit_irgen_t *g, int pos)
{
   jit_reg_t r = irgen_alloc_reg(g);
   irgen_emit_unary(g, J_RECV, JIT_SZ_UNSPEC, JIT_CC_NONE, r,
                    jit_value_from_int64(pos));
   return jit_value_from_reg(r);
}

static void j_send(jit_irgen_t *g, int pos, jit_value_t value)
{
   irgen_emit_binary(g, J_SEND, JIT_SZ_UNSPEC, JIT_CC_NONE, JIT_REG_INVALID,
                     jit_value_from_int64(pos), value);
}

static void j_ret(jit_irgen_t *g)
{
   irgen_emit_nullary(g, J_RET, JIT_CC_NONE, JIT_REG_INVALID);
   g->flags = VCODE_INVALID_REG;
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

   if (l->label == JIT_LABEL_INVALID) {
      patch_list_t *p = &(l->patchlist);
      for (; p->count == PATCH_CHUNK_SZ; p = p->next) {
         if (p->next == NULL)
            p->next = xcalloc(sizeof(patch_list_t));
      }
      p->offsets[p->count++] = ir - g->func->irbuf;
   }
}

static void j_call(jit_irgen_t *g, jit_handle_t handle)
{
   irgen_emit_unary(g, J_CALL, JIT_SZ_UNSPEC, JIT_CC_NONE,
                    JIT_REG_INVALID, jit_value_from_handle(handle));
   g->flags = VCODE_INVALID_REG;
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
   g->flags = VCODE_INVALID_REG;
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
   g->flags = VCODE_INVALID_REG;
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
   g->flags = VCODE_INVALID_REG;
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
   g->flags = VCODE_INVALID_REG;
}

static void j_fcmp(jit_irgen_t *g, jit_cc_t cc, jit_value_t lhs,
                   jit_value_t rhs)
{
   irgen_emit_binary(g, J_FCMP, JIT_SZ_UNSPEC, cc, JIT_REG_INVALID, lhs, rhs);
   g->flags = VCODE_INVALID_REG;
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
   assert(jit_value_is_addr(addr));
   jit_reg_t r = irgen_alloc_reg(g);
   irgen_emit_unary(g, J_LOAD, sz, JIT_CC_NONE, r, addr);
   return jit_value_from_reg(r);
}

static void j_store(jit_irgen_t *g, jit_size_t sz, jit_value_t value,
                    jit_value_t addr)
{
   assert(!jit_value_is_addr(value));
   assert(jit_value_is_addr(addr));
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

static void macro_copy(jit_irgen_t *g, jit_value_t dest, jit_value_t src,
                       jit_reg_t count)
{
   irgen_emit_binary(g, MACRO_COPY, JIT_SZ_UNSPEC, JIT_CC_NONE,
                     count, dest, src);
}

static void macro_bzero(jit_irgen_t *g, jit_value_t dest, jit_reg_t count)
{
   assert(jit_value_is_addr(dest));
   irgen_emit_unary(g, MACRO_BZERO, JIT_SZ_UNSPEC, JIT_CC_NONE,
                    count, dest);
}

static jit_value_t macro_galloc(jit_irgen_t *g, jit_value_t bytes)
{
   jit_reg_t r = irgen_alloc_reg(g);
   irgen_emit_unary(g, MACRO_GALLOC, JIT_SZ_UNSPEC, JIT_CC_NONE, r, bytes);
   return jit_value_from_reg(r);
}

static void macro_exit(jit_irgen_t *g, jit_exit_t exit)
{
   irgen_emit_unary(g, MACRO_EXIT, JIT_SZ_UNSPEC, JIT_CC_NONE, JIT_REG_INVALID,
                    jit_value_from_exit(exit));
   g->flags = VCODE_INVALID_REG;
}

static jit_value_t macro_fexp(jit_irgen_t *g, jit_value_t lhs, jit_value_t rhs)
{
   jit_reg_t r = irgen_alloc_reg(g);
   irgen_emit_binary(g, MACRO_FEXP, JIT_SZ_UNSPEC, JIT_CC_NONE, r, lhs, rhs);
   return jit_value_from_reg(r);
}

static jit_value_t macro_exp(jit_irgen_t *g, jit_value_t lhs, jit_value_t rhs)
{
   jit_reg_t r = irgen_alloc_reg(g);
   irgen_emit_binary(g, MACRO_EXP, JIT_SZ_UNSPEC, JIT_CC_NONE, r, lhs, rhs);
   return jit_value_from_reg(r);
}

static void macro_fficall(jit_irgen_t *g, jit_value_t addr)
{
   assert(jit_value_is_addr(addr));
   irgen_emit_unary(g, MACRO_FFICALL, JIT_SZ_UNSPEC, JIT_CC_NONE,
                    JIT_REG_INVALID, addr);
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

////////////////////////////////////////////////////////////////////////////////
// Vcode to JIT IR lowering

static int irgen_slots_for_type(vcode_type_t vtype)
{
   switch (vtype_kind(vtype)) {
   case VCODE_TYPE_UARRAY:
      // Always passed around scalarised
      if (vtype_kind(vtype_elem(vtype)) == VCODE_TYPE_SIGNAL)
         return 2 + vtype_dims(vtype) * 2;
      else
         return 1 + vtype_dims(vtype) * 2;
   case VCODE_TYPE_SIGNAL:
      // Signal pointer plus offset
      return 2;
   case VCODE_TYPE_CLOSURE:
      // Function pointer, context
      return 2;
   case VCODE_TYPE_RESOLUTION:
      // Closure slots plus left and nlits (this is silly)
      return 4;
   default:
      // Passed by pointer or fits in 64-bit register
      return 1;
   }
}

static int irgen_align_of(vcode_type_t vtype)
{
   switch (vtype_kind(vtype)) {
   case VCODE_TYPE_INT:
   case VCODE_TYPE_OFFSET:
      {
         const int bits = vtype_repr_bits(vtype_repr(vtype));
         return ALIGN_UP(bits, 8) / 8;
      }
   case VCODE_TYPE_REAL:
   case VCODE_TYPE_RECORD:
      return sizeof(double);
   case VCODE_TYPE_POINTER:
   case VCODE_TYPE_ACCESS:
   case VCODE_TYPE_UARRAY:
   case VCODE_TYPE_SIGNAL:
   case VCODE_TYPE_CONTEXT:
   case VCODE_TYPE_FILE:
      return sizeof(void *);
   case VCODE_TYPE_CARRAY:
      return irgen_align_of(vtype_elem(vtype));
   default:
      fatal_trace("cannot handle type %d in irgen_align_of", vtype_kind(vtype));
   }
}

static int irgen_size_bits(vcode_type_t vtype)
{
   switch (vtype_kind(vtype)) {
   case VCODE_TYPE_INT:
   case VCODE_TYPE_OFFSET:
      return vtype_repr_bits(vtype_repr(vtype));
   case VCODE_TYPE_REAL:
      return sizeof(double) * 8;
   case VCODE_TYPE_POINTER:
   case VCODE_TYPE_ACCESS:
   case VCODE_TYPE_CONTEXT:
   case VCODE_TYPE_FILE:
      return sizeof(void *) * 8;
   default:
      fatal_trace("cannot handle type %d in irgen_size_bits",
                  vtype_kind(vtype));
   }
}

static int irgen_size_bytes(vcode_type_t vtype)
{
   switch (vtype_kind(vtype)) {
   case VCODE_TYPE_INT:
   case VCODE_TYPE_OFFSET:
      {
         const int bits = irgen_size_bits(vtype);
         return ALIGN_UP(bits, 8) / 8;
      }

   case VCODE_TYPE_REAL:
      return sizeof(double);

   case VCODE_TYPE_CARRAY:
      return vtype_size(vtype) * irgen_size_bytes(vtype_elem(vtype));

   case VCODE_TYPE_RECORD:
      {
         const int nfields = vtype_fields(vtype);
         int bytes = 0;
         for (int i = 0; i < nfields; i++) {
            vcode_type_t ftype = vtype_field(vtype, i);
            const int fb = irgen_size_bytes(ftype);
            const int align = irgen_align_of(ftype);

            bytes = ALIGN_UP(bytes, align);
            bytes += fb;
         }

         return ALIGN_UP(bytes, sizeof(double));
      }

   case VCODE_TYPE_UARRAY:
      if (vtype_kind(vtype_elem(vtype)) == VCODE_TYPE_SIGNAL)
         return 2*sizeof(void *) + 2 * 4 * vtype_dims(vtype);
      else
         return sizeof(void *) + 2 * 4 * vtype_dims(vtype);

   case VCODE_TYPE_ACCESS:
   case VCODE_TYPE_POINTER:
   case VCODE_TYPE_CONTEXT:
   case VCODE_TYPE_FILE:
      return sizeof(void *);

   case VCODE_TYPE_SIGNAL:
      return sizeof(void *) + sizeof(int32_t);

   default:
      fatal_trace("cannot handle type %d in irgen_size_bytes",
                  vtype_kind(vtype));
   }
}

static jit_size_t irgen_jit_size(vcode_type_t vtype)
{
   const int bits = irgen_size_bits(vtype);
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

static jit_value_t irgen_get_value(jit_irgen_t *g, vcode_reg_t reg)
{
   assert(reg != VCODE_INVALID_REG);
   assert(g->map[reg].kind != JIT_VALUE_INVALID);

   return g->map[reg];
}

static jit_value_t irgen_get_arg(jit_irgen_t *g, int op, int arg)
{
   return irgen_get_value(g, vcode_get_arg(op, arg));
}

static jit_reg_t irgen_as_reg(jit_irgen_t *g, jit_value_t value)
{
   if (value.kind == JIT_VALUE_REG)
      return value.reg;
   else {
      jit_reg_t r = irgen_alloc_reg(g);
      j_mov(g, r, value);
      return r;
   }
}

static jit_value_t irgen_stack_space(jit_irgen_t *g, vcode_type_t vtype)
{
   const int bytes = irgen_size_bytes(vtype);
   const int align = irgen_align_of(vtype);

   g->func->framesz = ALIGN_UP(g->func->framesz, align);
   jit_value_t addr = jit_value_from_frame_addr(g->func->framesz);
   g->func->framesz += bytes;
   return addr;
}

static jit_value_t irgen_lea(jit_irgen_t *g, jit_value_t addr)
{
   switch (addr.kind) {
   case JIT_ADDR_REG:
      if (addr.disp == 0)
         return jit_value_from_reg(addr.reg);
      else
         return j_lea(g, addr);
   case JIT_ADDR_FRAME:
   case JIT_ADDR_CPOOL:
   case JIT_ADDR_ABS:
      return j_lea(g, addr);
   case JIT_VALUE_REG:
   case JIT_VALUE_INT64:
      return addr;
   default:
      fatal_trace("cannot load address of value kind %d", addr.kind);
   }
}

static jit_value_t irgen_is_scalar(jit_irgen_t *g, int op, int arg)
{
   const vtype_kind_t kind = vcode_reg_kind(vcode_get_arg(op, arg));
   return jit_value_from_int64(kind != VCODE_TYPE_POINTER);
}

static jit_value_t irgen_get_context(jit_irgen_t *g)
{
   if (g->statereg.kind != JIT_VALUE_INVALID)
      return j_load(g, JIT_SZ_PTR, jit_addr_from_value(g->statereg, 0));
   else
      return g->map[vcode_param_reg(0)];
}

static size_t irgen_append_cpool(jit_irgen_t *g, size_t sz, int align)
{
   if (g->cpoolptr + sz + align - 1 > g->func->cpoolsz) {
      g->func->cpoolsz = MAX(128, MAX((g->func->cpoolsz * 3) / 2,
                                      g->cpoolptr + sz + align - 1));
      g->func->cpool = xrealloc(g->func->cpool, g->func->cpoolsz);
   }

   const size_t result = ALIGN_UP(g->cpoolptr, align);
   DEBUG_ONLY(memset(g->func->cpool + result, 0xde, sz));
   g->cpoolptr = result + sz;
   return result;
}

static void irgen_emit_debuginfo(jit_irgen_t *g, int op)
{
   const int64_t enc = ((int64_t)vcode_active_block() << 32) | op;

   jit_value_t arg1 = jit_value_from_loc(vcode_get_loc(op));
   jit_value_t arg2 = jit_value_from_int64(enc);

   irgen_emit_binary(g, J_DEBUG, JIT_SZ_UNSPEC, JIT_CC_NONE,
                     JIT_REG_INVALID, arg1, arg2);
}

static ffi_type_t irgen_ffi_type(vcode_type_t type)
{
   if (type == VCODE_INVALID_TYPE)
      return FFI_VOID;

   switch (vtype_kind(type)) {
   case VCODE_TYPE_INT:
   case VCODE_TYPE_OFFSET:
      switch (vtype_repr(type)) {
      case VCODE_REPR_U1:
      case VCODE_REPR_U8:
      case VCODE_REPR_I8:
         return FFI_INT8;
      case VCODE_REPR_I16:
      case VCODE_REPR_U16:
         return FFI_INT16;
      case VCODE_REPR_I32:
      case VCODE_REPR_U32:
         return FFI_INT32;
      default:
         return FFI_INT64;
      }
   case VCODE_TYPE_REAL:
      return FFI_FLOAT;
   case VCODE_TYPE_CARRAY:
   case VCODE_TYPE_RECORD:
   case VCODE_TYPE_POINTER:
   case VCODE_TYPE_CONTEXT:
   case VCODE_TYPE_ACCESS:
   case VCODE_TYPE_FILE:
      return FFI_POINTER;
   case VCODE_TYPE_UARRAY:
      return FFI_UARRAY;
   default:
      fatal_trace("cannot handle type %d in irgen_ffi_type", vtype_kind(type));
   }
}

static jit_foreign_t *irgen_ffi_for_call(jit_irgen_t *g, int op)
{
   ident_t func = vcode_get_func(op);
   jit_foreign_t *ff = jit_ffi_get(func);
   if (ff != NULL)
      return ff;

   ffi_spec_t spec = 0;

   const int nargs = vcode_count_args(op);
   for (int i = 0; i < nargs; i++) {
      vcode_type_t vtype = vcode_reg_type(vcode_get_arg(op, i));
      spec |= irgen_ffi_type(vtype) << (i + 1) * 4;
   }

   vcode_reg_t result = vcode_get_result(op);
   if (result != VCODE_INVALID_REG)
      spec |= irgen_ffi_type(vcode_reg_type(result));
   else
      spec |= FFI_VOID;

   return jit_ffi_bind(func, spec, NULL);
}

static void irgen_op_null(jit_irgen_t *g, int op)
{
   vcode_reg_t result = vcode_get_result(op);
   g->map[result] = jit_value_from_int64(0);
}

static void irgen_op_const(jit_irgen_t *g, int op)
{
   vcode_reg_t result = vcode_get_result(op);
   g->map[result] = jit_value_from_int64(vcode_get_value(op));
}

static void irgen_op_const_real(jit_irgen_t *g, int op)
{
   vcode_reg_t result = vcode_get_result(op);
   g->map[result] = jit_value_from_double(vcode_get_real(op));
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
      *(double *)p = value.dval;
      break;

   case JIT_ADDR_CPOOL:
      assert(value.int64 >= 0 && value.int64 + bytes <= g->func->cpoolsz);
      memcpy(p, g->func->cpool + value.int64, bytes);
      break;

   default:
      fatal_trace("cannot handle value kind %d", value.kind);
   }
}

static void irgen_op_const_array(jit_irgen_t *g, int op)
{
   vcode_reg_t result = vcode_get_result(op);
   vcode_type_t vtype = vcode_reg_type(result);
   vcode_type_t velem = vtype_elem(vtype);

   const int elemsz = irgen_size_bytes(velem);
   const int align = irgen_align_of(velem);
   const int count = vtype_size(vtype);

   const size_t offset = irgen_append_cpool(g, elemsz * count, align);

   unsigned char *p = g->func->cpool + offset;
   for (int i = 0; i < count; i++, p += elemsz) {
      jit_value_t elem = irgen_get_arg(g, op, i);
      p = ALIGN_UP(p, align);
      irgen_copy_const(g, p, elem, elemsz);
   }

   g->map[result] = jit_value_from_cpool_addr(offset);
}

static void irgen_op_const_rep(jit_irgen_t *g, int op)
{
   vcode_reg_t result = vcode_get_result(op);
   vcode_type_t velem = vtype_pointed(vcode_reg_type(result));

   const int count = vcode_get_value(op);
   jit_value_t arg0 = irgen_get_arg(g, op, 0);

   const int elemsz = irgen_size_bytes(velem);
   const int align = irgen_align_of(velem);

   const size_t offset = irgen_append_cpool(g, elemsz * count, align);

   unsigned char *p = g->func->cpool + offset;
   for (int i = 0; i < count; i++, p += elemsz) {
      p = ALIGN_UP(p, align);
      irgen_copy_const(g, p, arg0, elemsz);
   }

   g->map[result] = jit_value_from_cpool_addr(offset);
}

static void irgen_op_const_record(jit_irgen_t *g, int op)
{
   vcode_reg_t result = vcode_get_result(op);
   vcode_type_t vtype = vcode_reg_type(result);

   const int nfields = vtype_fields(vtype);
   const int sz = irgen_size_bytes(vtype);
   const int align = irgen_align_of(vtype);
   const size_t offset = irgen_append_cpool(g, sz, align);

   unsigned char *p = g->func->cpool + offset;
   for (int i = 0; i < nfields; i++) {
      jit_value_t elem = irgen_get_arg(g, op, i);

      vcode_type_t ftype = vtype_field(vtype, i);
      const int bytes = irgen_size_bytes(ftype);
      const int align = irgen_align_of(ftype);

      p = ALIGN_UP(p, align);
      irgen_copy_const(g, p, elem, bytes);
      p += bytes;
   }
   assert(g->func->cpool + offset + sz - p < sizeof(double));

   g->map[result] = jit_value_from_cpool_addr(offset);
}

static void irgen_op_address_of(jit_irgen_t *g, int op)
{
   // No-op
   g->map[vcode_get_result(op)] = irgen_get_arg(g, op, 0);
}

static void irgen_op_copy(jit_irgen_t *g, int op)
{
   jit_value_t arg0 = irgen_get_arg(g, op, 0);
   jit_value_t arg1 = irgen_get_arg(g, op, 1);

   jit_value_t bytes;
   if (vcode_count_args(op) > 2) {
      jit_value_t arg2 = irgen_get_arg(g, op, 2);

      const int scale = irgen_size_bytes(vcode_get_type(op));
      bytes = j_mul(g, arg2, jit_value_from_int64(scale));
   }
   else
      bytes = jit_value_from_int64(irgen_size_bytes(vcode_get_type(op)));

   macro_copy(g, arg0, arg1, irgen_as_reg(g, bytes));
}

static void irgen_op_memset(jit_irgen_t *g, int op)
{
   jit_value_t base   = irgen_get_arg(g, op, 0);
   jit_value_t value  = irgen_get_arg(g, op, 1);
   jit_value_t length = irgen_get_arg(g, op, 2);

   vcode_type_t vtype = vcode_reg_type(vcode_get_arg(op, 1));
   jit_value_t scale = jit_value_from_int64(irgen_size_bytes(vtype));

   if (value.kind == JIT_VALUE_INT64 && value.int64 == 0) {
      jit_value_t bytes = j_mul(g, length, scale);
      jit_value_t addr = jit_addr_from_value(base, 0);
      macro_bzero(g, addr, jit_value_as_reg(bytes));
   }
   else {
      irgen_label_t *l_exit = irgen_alloc_label(g);
      irgen_label_t *l_head = irgen_alloc_label(g);

      j_cmp(g, JIT_CC_LE, length, jit_value_from_int64(0));
      j_jump(g, JIT_CC_T, l_exit);

      jit_reg_t ctr_r = irgen_alloc_reg(g);
      j_mov(g, ctr_r, jit_value_from_int64(0));

      irgen_bind_label(g, l_head);

      jit_value_t ctr = jit_value_from_reg(ctr_r);
      jit_value_t off = j_mul(g, ctr, scale);
      jit_value_t ptr = j_add(g, base, off);
      j_store(g, irgen_jit_size(vtype), value, jit_addr_from_value(ptr, 0));

      jit_value_t next = j_add(g, ctr, jit_value_from_int64(1));
      j_mov(g, ctr_r, next);
      j_cmp(g, JIT_CC_LT, next, length);
      j_jump(g, JIT_CC_T, l_head);

      irgen_bind_label(g, l_exit);
   }
}

static void irgen_send_args(jit_irgen_t *g, int op, int first)
{
   const int nargs = vcode_count_args(op);

   for (int i = 0, pslot = first; i < nargs; i++) {
      vcode_reg_t vreg = vcode_get_arg(op, i);
      int slots = irgen_slots_for_type(vcode_reg_type(vreg));
      if (vcode_reg_kind(vreg) == VCODE_TYPE_SIGNAL)
         slots++;
      if (slots > 1) {
         jit_reg_t base = jit_value_as_reg(irgen_get_value(g, vreg));
         for (int j = 0; j < slots; j++)
            j_send(g, pslot++, jit_value_from_reg(base + j));
      }
      else
         j_send(g, pslot++, irgen_get_value(g, vreg));
   }
}

static void irgen_op_return(jit_irgen_t *g, int op)
{
   switch (vcode_unit_kind()) {
   case VCODE_UNIT_PROCESS:
      {
         // Set up for first call to process after reset
         jit_value_t ptr = jit_addr_from_value(g->statereg, 2 * sizeof(void *));
         j_store(g, JIT_SZ_32, jit_value_from_int64(1), ptr);
         j_send(g, 0, g->statereg);
      }
      break;

   case VCODE_UNIT_PROCEDURE:
      j_send(g, 0, jit_null_ptr());
      break;

   case VCODE_UNIT_INSTANCE:
   case VCODE_UNIT_PROTECTED:
   case VCODE_UNIT_PACKAGE:
      j_send(g, 0, g->statereg);
      break;

   case VCODE_UNIT_FUNCTION:
   case VCODE_UNIT_THUNK:
      if (vcode_count_args(op) > 0)
         irgen_send_args(g, op, 0);
      else
         j_send(g, 0, jit_null_ptr());  // Procedure compiled as function
      break;
   }

   j_ret(g);
}

static void irgen_op_not(jit_irgen_t *g, int op)
{
   jit_value_t arg0 = irgen_get_arg(g, op, 0);
   g->map[vcode_get_result(op)] = j_not(g, arg0);
}

static void irgen_op_and(jit_irgen_t *g, int op)
{
   jit_value_t arg0 = irgen_get_arg(g, op, 0);
   jit_value_t arg1 = irgen_get_arg(g, op, 1);

   g->map[vcode_get_result(op)] = j_and(g, arg0, arg1);
}

static void irgen_op_nand(jit_irgen_t *g, int op)
{
   jit_value_t arg0 = irgen_get_arg(g, op, 0);
   jit_value_t arg1 = irgen_get_arg(g, op, 1);

   jit_value_t and = j_and(g, arg0, arg1);
   g->map[vcode_get_result(op)] = j_not(g, and);
}

static void irgen_op_or(jit_irgen_t *g, int op)
{
   jit_value_t arg0 = irgen_get_arg(g, op, 0);
   jit_value_t arg1 = irgen_get_arg(g, op, 1);

   g->map[vcode_get_result(op)] = j_or(g, arg0, arg1);
}

static void irgen_op_nor(jit_irgen_t *g, int op)
{
   jit_value_t arg0 = irgen_get_arg(g, op, 0);
   jit_value_t arg1 = irgen_get_arg(g, op, 1);

   jit_value_t or = j_or(g, arg0, arg1);
   g->map[vcode_get_result(op)] = j_not(g, or);
}

static void irgen_op_xor(jit_irgen_t *g, int op)
{
   jit_value_t arg0 = irgen_get_arg(g, op, 0);
   jit_value_t arg1 = irgen_get_arg(g, op, 1);

   g->map[vcode_get_result(op)] = j_xor(g, arg0, arg1);
}

static void irgen_op_xnor(jit_irgen_t *g, int op)
{
   jit_value_t arg0 = irgen_get_arg(g, op, 0);
   jit_value_t arg1 = irgen_get_arg(g, op, 1);

   jit_value_t xor = j_xor(g, arg0, arg1);
   g->map[vcode_get_result(op)] = j_not(g, xor);
}

static void irgen_op_trap_add(jit_irgen_t *g, int op)
{
   vcode_reg_t result = vcode_get_result(op);
   vcode_type_t vtype = vcode_reg_type(result);

   jit_value_t arg0 = irgen_get_arg(g, op, 0);
   jit_value_t arg1 = irgen_get_arg(g, op, 1);
   jit_value_t locus = irgen_get_arg(g, op, 2);

   jit_size_t sz = irgen_jit_size(vtype);

   jit_cc_t cc = vtype_repr_signed(vtype_repr(vtype)) ? JIT_CC_O : JIT_CC_C;
   g->map[result] = j_adds(g, sz, cc, arg0, arg1);

   irgen_label_t *l_pass = irgen_alloc_label(g);
   j_jump(g, JIT_CC_F, l_pass);

   j_send(g, 0, arg0);
   j_send(g, 1, arg1);
   j_send(g, 2, locus);
   macro_exit(g, JIT_EXIT_OVERFLOW);

   irgen_bind_label(g, l_pass);
}

static void irgen_op_add(jit_irgen_t *g, int op)
{
   vcode_reg_t result = vcode_get_result(op);

   jit_value_t arg0 = irgen_get_arg(g, op, 0);
   jit_value_t arg1 = irgen_get_arg(g, op, 1);

   if (vcode_reg_kind(result) == VCODE_TYPE_REAL)
      g->map[result] = j_fadd(g, arg0, arg1);
   else
      g->map[result] = j_add(g, arg0, arg1);
}

static void irgen_op_trap_mul(jit_irgen_t *g, int op)
{
   vcode_reg_t result = vcode_get_result(op);
   vcode_type_t vtype = vcode_reg_type(result);

   jit_value_t arg0 = irgen_get_arg(g, op, 0);
   jit_value_t arg1 = irgen_get_arg(g, op, 1);
   jit_value_t locus = irgen_get_arg(g, op, 2);

   jit_size_t sz = irgen_jit_size(vcode_reg_type(result));

   jit_cc_t cc = vtype_repr_signed(vtype_repr(vtype)) ? JIT_CC_O : JIT_CC_C;
   g->map[result] = j_muls(g, sz, cc, arg0, arg1);

   irgen_label_t *l_pass = irgen_alloc_label(g);
   j_jump(g, JIT_CC_F, l_pass);

   j_send(g, 0, arg0);
   j_send(g, 1, arg1);
   j_send(g, 2, locus);
   macro_exit(g, JIT_EXIT_OVERFLOW);

   irgen_bind_label(g, l_pass);
}

static void irgen_op_mul(jit_irgen_t *g, int op)
{
   vcode_reg_t result = vcode_get_result(op);

   jit_value_t arg0 = irgen_get_arg(g, op, 0);
   jit_value_t arg1 = irgen_get_arg(g, op, 1);

   if (vcode_reg_kind(result) == VCODE_TYPE_REAL)
      g->map[result] = j_fmul(g, arg0, arg1);
   else
      g->map[result] = j_mul(g, arg0, arg1);
}

static void irgen_op_div(jit_irgen_t *g, int op)
{
   vcode_reg_t result = vcode_get_result(op);

   jit_value_t arg0 = irgen_get_arg(g, op, 0);
   jit_value_t arg1 = irgen_get_arg(g, op, 1);

   if (vcode_reg_kind(result) == VCODE_TYPE_REAL)
      g->map[result] = j_fdiv(g, arg0, arg1);
   else
      g->map[result] = j_div(g, arg0, arg1);
}

static void irgen_op_exp(jit_irgen_t *g, int op)
{
   vcode_reg_t result = vcode_get_result(op);

   jit_value_t arg0 = irgen_get_arg(g, op, 0);
   jit_value_t arg1 = irgen_get_arg(g, op, 1);

   if (vcode_reg_kind(result) == VCODE_TYPE_REAL)
      g->map[result] = macro_fexp(g, arg0, arg1);
   else
      g->map[result] = macro_exp(g, arg0, arg1);
}

static void irgen_op_sub(jit_irgen_t *g, int op)
{
   vcode_reg_t result = vcode_get_result(op);

   jit_value_t arg0 = irgen_get_arg(g, op, 0);
   jit_value_t arg1 = irgen_get_arg(g, op, 1);

   if (vcode_reg_kind(result) == VCODE_TYPE_REAL)
      g->map[result] = j_fsub(g, arg0, arg1);
   else
      g->map[result] = j_sub(g, arg0, arg1);
}

static void irgen_op_trap_sub(jit_irgen_t *g, int op)
{
   vcode_reg_t result = vcode_get_result(op);
   vcode_type_t vtype = vcode_reg_type(result);

   jit_value_t arg0 = irgen_get_arg(g, op, 0);
   jit_value_t arg1 = irgen_get_arg(g, op, 1);
   jit_value_t locus = irgen_get_arg(g, op, 2);

   jit_size_t sz = irgen_jit_size(vcode_reg_type(result));

   jit_cc_t cc = vtype_repr_signed(vtype_repr(vtype)) ? JIT_CC_O : JIT_CC_C;
   g->map[result] = j_subs(g, sz, cc, arg0, arg1);

   irgen_label_t *l_pass = irgen_alloc_label(g);
   j_jump(g, JIT_CC_F, l_pass);

   j_send(g, 0, arg0);
   j_send(g, 1, arg1);
   j_send(g, 2, locus);
   macro_exit(g, JIT_EXIT_OVERFLOW);

   irgen_bind_label(g, l_pass);
}

static void irgen_op_neg(jit_irgen_t *g, int op)
{
   vcode_reg_t result = vcode_get_result(op);

   jit_value_t arg0 = irgen_get_arg(g, op, 0);

   if (vcode_reg_kind(result) == VCODE_TYPE_REAL)
      g->map[result] = j_fneg(g, arg0);
   else
      g->map[result] = j_neg(g, arg0);
}

static void irgen_op_abs(jit_irgen_t *g, int op)
{
   vcode_reg_t result = vcode_get_result(op);

   jit_value_t arg0 = irgen_get_arg(g, op, 0);

   if (vcode_reg_kind(result) == VCODE_TYPE_REAL) {
      jit_value_t neg = j_fneg(g, arg0);
      j_fcmp(g, JIT_CC_LT, arg0, jit_value_from_double(0.0));
      g->map[result] = j_csel(g, neg, arg0);
   }
   else {
      jit_value_t neg = j_neg(g, arg0);
      j_cmp(g, JIT_CC_LT, arg0, jit_value_from_int64(0));
      g->map[result] = j_csel(g, neg, arg0);
   }
}

static void irgen_op_mod(jit_irgen_t *g, int op)
{
   jit_value_t numer = irgen_get_arg(g, op, 0);
   jit_value_t denom = irgen_get_arg(g, op, 1);

   // Calculate the following:
   //
   //   long r = numer % denom;
   //   if ((r > 0 && denom < 0) || (r < 0 && denom > 0))
   //      r = r + denom;

   jit_value_t r = j_rem(g, numer, denom);

   irgen_label_t *l1 = irgen_alloc_label(g);
   irgen_label_t *l2 = irgen_alloc_label(g);
   irgen_label_t *l3 = irgen_alloc_label(g);

   jit_value_t zero = jit_value_from_int64(0);

   // TODO: this could be optimised a bit

   j_cmp(g, JIT_CC_LE, r, zero);
   j_jump(g, JIT_CC_T, l1);
   j_cmp(g, JIT_CC_LT, denom, zero);
   j_jump(g, JIT_CC_T, l2);

   irgen_bind_label(g, l1);

   j_cmp(g, JIT_CC_GE, r, zero);
   j_jump(g, JIT_CC_T, l3);
   j_cmp(g, JIT_CC_LE, denom, zero);
   j_jump(g, JIT_CC_T, l3);

   irgen_bind_label(g, l2);

   jit_value_t r2 = j_add(g, r, denom);
   j_mov(g, jit_value_as_reg(r), r2);

   irgen_bind_label(g, l3);

   g->map[vcode_get_result(op)] = r;
}

static void irgen_op_rem(jit_irgen_t *g, int op)
{
   jit_value_t arg0 = irgen_get_arg(g, op, 0);
   jit_value_t arg1 = irgen_get_arg(g, op, 1);

   g->map[vcode_get_result(op)] = j_rem(g, arg0, arg1);
}

static void irgen_op_cmp(jit_irgen_t *g, int op)
{
   jit_value_t arg0 = irgen_get_arg(g, op, 0);
   jit_value_t arg1 = irgen_get_arg(g, op, 1);

   jit_cc_t cc = irgen_get_jit_cc(vcode_get_cmp(op));

   if (vcode_reg_kind(vcode_get_arg(op, 0)) == VCODE_TYPE_REAL)
      j_fcmp(g, cc, arg0, arg1);
   else
      j_cmp(g, cc, arg0, arg1);

   vcode_reg_t result = vcode_get_result(op);
   g->map[result] = j_cset(g);
   g->flags = result;
}

static void irgen_op_index(jit_irgen_t *g, int op)
{
   vcode_var_t var = vcode_get_address(op);
   g->map[vcode_get_result(op)] = g->vars[var];
}

static jit_value_t irgen_load_addr(jit_irgen_t *g, vcode_type_t vtype,
                                   jit_value_t addr)
{
   switch (vtype_kind(vtype)) {
   case VCODE_TYPE_OFFSET:
   case VCODE_TYPE_INT:
      if (vtype_repr_signed(vtype_repr(vtype)))
         return j_load(g, irgen_jit_size(vtype), addr);
      else
         return j_uload(g, irgen_jit_size(vtype), addr);

   case VCODE_TYPE_REAL:
      return j_load(g, JIT_SZ_64, addr);

   case VCODE_TYPE_ACCESS:
   case VCODE_TYPE_POINTER:
   case VCODE_TYPE_CONTEXT:
   case VCODE_TYPE_FILE:
      return j_load(g, JIT_SZ_PTR, addr);

   case VCODE_TYPE_SIGNAL:
      {
         jit_value_t shared = j_load(g, JIT_SZ_PTR, addr);
         addr = jit_addr_from_value(addr, sizeof(void *));
         j_load(g, JIT_SZ_32, addr);   // Offset
         return shared;
      }

   case VCODE_TYPE_UARRAY:
      {
         const int slots = irgen_slots_for_type(vtype_elem(vtype));
         jit_value_t base = j_load(g, JIT_SZ_PTR, addr);
         addr = jit_addr_from_value(addr, sizeof(void *));
         for (int i = 1; i < slots; i++) {
            j_load(g, JIT_SZ_PTR, addr);
            addr = jit_addr_from_value(addr, sizeof(void *));
         }

         const int ndims = vtype_dims(vtype);
         for (int i = 0; i < 2*ndims; i++) {
            j_load(g, JIT_SZ_32, addr);
            addr = jit_addr_from_value(addr, 4);
         }

         return base;
      }

   default:
      fatal_trace("cannot load type kind %d", vtype_kind(vtype));
   }
}

static void irgen_op_load(jit_irgen_t *g, int op)
{
   vcode_var_t var = vcode_get_address(op);
   vcode_type_t vtype = vcode_var_type(var);
   jit_value_t addr = g->vars[var];

   g->map[vcode_get_result(op)] = irgen_load_addr(g, vtype, addr);
}

static void irgen_op_load_indirect(jit_irgen_t *g, int op)
{
   vcode_reg_t result = vcode_get_result(op);
   vcode_type_t vtype = vcode_reg_type(result);
   jit_value_t addr = jit_addr_from_value(irgen_get_arg(g, op, 0), 0);

   g->map[result] = irgen_load_addr(g, vtype, addr);
}

static void irgen_store_addr(jit_irgen_t *g, vcode_type_t vtype,
                             jit_value_t value, jit_value_t addr)
{
   switch (vtype_kind(vtype)) {
   case VCODE_TYPE_OFFSET:
   case VCODE_TYPE_INT:
   case VCODE_TYPE_ACCESS:
   case VCODE_TYPE_POINTER:
   case VCODE_TYPE_CONTEXT:
   case VCODE_TYPE_REAL:
   case VCODE_TYPE_FILE:
      j_store(g, irgen_jit_size(vtype), value, addr);
      break;

   case VCODE_TYPE_UARRAY:
      {
         const int slots = irgen_slots_for_type(vtype_elem(vtype));
         jit_reg_t base = jit_value_as_reg(value);
         for (int i = 0; i < slots; i++) {
            j_store(g, JIT_SZ_PTR, jit_value_from_reg(base + i), addr);
            addr = jit_addr_from_value(addr, sizeof(void *));
         }

         const int ndims = vtype_dims(vtype);
         for (int i = 0; i < 2*ndims; i++) {
            j_store(g, JIT_SZ_32, jit_value_from_reg(base + slots + i), addr);
            addr = jit_addr_from_value(addr, 4);
         }
      }
      break;

   case VCODE_TYPE_SIGNAL:
      {
         jit_reg_t base = jit_value_as_reg(value);
         j_store(g, JIT_SZ_PTR, value, addr);
         addr = jit_addr_from_value(addr, sizeof(void *));
         j_store(g, JIT_SZ_32, jit_value_from_reg(base + 1), addr);
      }
      break;

   default:
      fatal_trace("cannot store type kind %d", vtype_kind(vtype));
   }
}

static void irgen_op_store_indirect(jit_irgen_t *g, int op)
{
   vcode_type_t vtype = vcode_reg_type(vcode_get_arg(op, 0));
   jit_value_t value = irgen_get_arg(g, op, 0);
   jit_value_t addr = jit_addr_from_value(irgen_get_arg(g, op, 1), 0);

   irgen_store_addr(g, vtype, value, addr);
}

static void irgen_op_store(jit_irgen_t *g, int op)
{
   vcode_var_t var = vcode_get_address(op);
   vcode_type_t vtype = vcode_var_type(var);

   jit_value_t addr = g->vars[var];
   jit_value_t value = irgen_get_arg(g, op, 0);

   irgen_store_addr(g, vtype, value, addr);
}

static void irgen_op_debug_locus(jit_irgen_t *g, int op)
{
   ident_t unit = vcode_get_ident(op);
   const ptrdiff_t offset = vcode_get_value(op);

   tree_t tree = tree_from_locus(unit, offset, lib_get_qualified);

   g->map[vcode_get_result(op)] = jit_value_from_int64((intptr_t)tree);
}

static void irgen_op_wrap(jit_irgen_t *g, int op)
{
   vcode_reg_t arg0 = vcode_get_arg(op, 0);
   jit_value_t ptr = irgen_lea(g, irgen_get_value(g, arg0));

   vcode_type_t vtype = vcode_reg_type(arg0);

   const int ndims = vcode_count_args(op) / 3;
   const int slots = irgen_slots_for_type(vtype);

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
      jit_value_t left  = irgen_get_arg(g, op, (i * 3) + 1);
      jit_value_t right = irgen_get_arg(g, op, (i * 3) + 2);
      jit_value_t dir   = irgen_get_arg(g, op, (i * 3) + 3);

      jit_value_t diff_up   = j_sub(g, right, left);
      jit_value_t diff_down = j_sub(g, left, right);

      j_cmp(g, JIT_CC_EQ, dir, jit_value_from_int64(RANGE_DOWNTO));
      jit_value_t diff   = j_csel(g, diff_down, diff_up);
      jit_value_t length = j_add(g, diff, jit_value_from_int64(1));
      jit_value_t zero   = jit_value_from_int64(0);
      j_cmp(g, JIT_CC_LT, diff, zero);
      jit_value_t clamped = j_csel(g, zero, length);
      jit_value_t neg     = j_neg(g, clamped);
      j_cmp(g, JIT_CC_EQ, dir, jit_value_from_int64(RANGE_DOWNTO));
      jit_value_t signlen = j_csel(g, neg, clamped);

      j_mov(g, dims[i*2], left);
      j_mov(g, dims[i*2 + 1], signlen);
   }

   g->map[vcode_get_result(op)] = jit_value_from_reg(base);
}

static void irgen_op_uarray_right(jit_irgen_t *g, int op)
{
   vcode_reg_t arg0 = vcode_get_arg(op, 0);
   jit_reg_t base = jit_value_as_reg(irgen_get_value(g, arg0));

   const int dim = vcode_get_dim(op);
   const int slots = irgen_slots_for_type(vtype_elem(vcode_reg_type(arg0)));

   jit_value_t left   = jit_value_from_reg(base + slots + dim*2);
   jit_value_t length = jit_value_from_reg(base + slots + 1 + dim*2);
   jit_value_t diff   = j_add(g, left, length);
   j_cmp(g, JIT_CC_LT, length, jit_value_from_int64(0));

   jit_value_t adj = j_csel(g, jit_value_from_int64(1),
                            jit_value_from_int64(-1));

   g->map[vcode_get_result(op)] = j_add(g, diff, adj);
}

static void irgen_op_uarray_left(jit_irgen_t *g, int op)
{
   vcode_reg_t arg0 = vcode_get_arg(op, 0);
   jit_reg_t base = jit_value_as_reg(irgen_get_arg(g, op, 0));

   const int dim = vcode_get_dim(op);
   const int slots = irgen_slots_for_type(vtype_elem(vcode_reg_type(arg0)));

   g->map[vcode_get_result(op)] = jit_value_from_reg(base + slots + dim*2);
}

static void irgen_op_uarray_dir(jit_irgen_t *g, int op)
{
   vcode_reg_t arg0 = vcode_get_arg(op, 0);
   jit_reg_t base = jit_value_as_reg(irgen_get_value(g, arg0));

   const int dim = vcode_get_dim(op);
   const int slots = irgen_slots_for_type(vtype_elem(vcode_reg_type(arg0)));

   jit_value_t length = jit_value_from_reg(base + slots + 1 + dim*2);
   j_cmp(g, JIT_CC_LT, length, jit_value_from_int64(0));

   STATIC_ASSERT(RANGE_DOWNTO == 1);

   vcode_reg_t result = vcode_get_result(op);
   g->map[result] = j_cset(g);
   g->flags = result;
}

static void irgen_op_uarray_len(jit_irgen_t *g, int op)
{
   vcode_reg_t arg0 = vcode_get_arg(op, 0);
   jit_reg_t base = jit_value_as_reg(irgen_get_value(g, arg0));

   const int dim = vcode_get_dim(op);
   const int slots = irgen_slots_for_type(vtype_elem(vcode_reg_type(arg0)));

   jit_value_t length = jit_value_from_reg(base + slots + 1 + dim*2);
   jit_value_t neg = j_neg(g, length);
   j_cmp(g, JIT_CC_LT, length, jit_value_from_int64(0));

   g->map[vcode_get_result(op)] = j_csel(g, neg, length);
}

static void irgen_op_unwrap(jit_irgen_t *g, int op)
{
   jit_value_t base = irgen_get_arg(g, op, 0);
   g->map[vcode_get_result(op)] = base;
}

static void irgen_op_array_ref(jit_irgen_t *g, int op)
{
   vcode_reg_t result = vcode_get_result(op);

   switch (vcode_reg_kind(result)) {
   case VCODE_TYPE_POINTER:
      {
         vcode_type_t vtype = vtype_pointed(vcode_reg_type(result));
         const int scale = irgen_size_bytes(vtype);

         jit_value_t arg0 = irgen_get_arg(g, op, 0);
         jit_value_t arg1 = irgen_get_arg(g, op, 1);

         // TODO: merge this into an address somehow
         jit_value_t scaled = j_mul(g, arg1, jit_value_from_int64(scale));

         jit_value_t addr = irgen_lea(g, arg0);

         g->map[vcode_get_result(op)] = j_add(g, addr, scaled);
      }
      break;

   case VCODE_TYPE_SIGNAL:
      {
         jit_value_t shared = irgen_get_arg(g, op, 0);
         jit_value_t offset = jit_value_from_reg(jit_value_as_reg(shared) + 1);

         jit_reg_t new = irgen_alloc_reg(g);
         j_mov(g, new, shared);
         g->map[vcode_get_result(op)] = jit_value_from_reg(new);

         // Offset must be next sequential register
         j_add(g, offset, irgen_get_arg(g, op, 1));
      }
      break;

   default:
      vcode_dump_with_mark(op, NULL, NULL);
      fatal_trace("unexpected array ref result kind");
   }
}

static void irgen_op_record_ref(jit_irgen_t *g, int op)
{
   // TODO: store the type directly in record ref op?
   vcode_type_t rtype = vtype_pointed(vcode_reg_type(vcode_get_arg(op, 0)));
   assert(vtype_kind(rtype) == VCODE_TYPE_RECORD);

   const int field = vcode_get_field(op);

   // TODO: cache field offsets somewhere? This duplicates irgen_size_bytes
   int offset = 0;
   for (int i = 0; i < field; i++) {
      vcode_type_t ftype = vtype_field(rtype, i);
      const int fb = irgen_size_bytes(ftype);
      const int align = irgen_align_of(ftype);

      offset = ALIGN_UP(offset, align);
      offset += fb;
   }

   const int align = irgen_align_of(vtype_field(rtype, field));
   offset = ALIGN_UP(offset, align);

   jit_value_t rptr = irgen_get_arg(g, op, 0);
   g->map[vcode_get_result(op)] = jit_addr_from_value(rptr, offset);
}

static void irgen_op_context_upref(jit_irgen_t *g, int op)
{
   const int hops = vcode_get_hops(op);

   if (hops == 0) {
      assert(g->statereg.kind != JIT_VALUE_INVALID);
      g->map[vcode_get_result(op)] = g->statereg;
   }
   else {
      jit_value_t context = irgen_get_context(g);

      for (int i = 1; i < hops; i++)
         context = j_load(g, JIT_SZ_PTR, jit_addr_from_value(context, 0));

      g->map[vcode_get_result(op)] = context;
   }
}

static void irgen_op_var_upref(jit_irgen_t *g, int op)
{
   jit_value_t context = irgen_get_context(g);

   const int hops = vcode_get_hops(op);
   vcode_var_t address = vcode_get_address(op);

   vcode_state_t state;
   vcode_state_save(&state);

   vcode_select_unit(vcode_unit_context());

   for (int i = 1; i < hops; i++) {
      context = j_load(g, JIT_SZ_PTR, jit_addr_from_value(context, 0));
      vcode_select_unit(vcode_unit_context());
   }

   // TODO: maybe we should cache these somewhere?
   jit_handle_t handle = jit_compile(g->func->jit, vcode_unit_name());
   jit_func_t *cf = jit_get_func(g->func->jit, handle);

   vcode_state_restore(&state);

   assert(address < cf->nvars);
   const int offset = cf->varoff[address];

   g->map[vcode_get_result(op)] = jit_addr_from_value(context, offset);
}

static void irgen_op_cast(jit_irgen_t *g, int op)
{
   vcode_reg_t arg    = vcode_get_arg(op, 0);
   vcode_reg_t result = vcode_get_result(op);

   vcode_type_t arg_type    = vcode_reg_type(arg);
   vcode_type_t result_type = vcode_reg_type(result);

   vtype_kind_t arg_kind    = vtype_kind(arg_type);
   vtype_kind_t result_kind = vtype_kind(result_type);

   if (arg_kind == VCODE_TYPE_CARRAY) {
      // This is a no-op as constrained arrays are implemented as pointers
      g->map[result] = g->map[arg];
   }
   else if (result_kind == VCODE_TYPE_REAL && arg_kind == VCODE_TYPE_INT) {
      g->map[vcode_get_result(op)] = j_scvtf(g, g->map[arg]);
   }
   else if (result_kind == VCODE_TYPE_INT && arg_kind == VCODE_TYPE_REAL) {
      g->map[vcode_get_result(op)] = j_fcvtns(g, g->map[arg]);
   }
   else if (result_kind == VCODE_TYPE_INT || result_kind == VCODE_TYPE_OFFSET) {
      // No sign extension or truncation is necessary as integer
      // registers are always 64 bits wide
      g->map[result] = g->map[arg];
   }
   else if (result_kind == VCODE_TYPE_REAL && arg_kind == VCODE_TYPE_REAL) {
      // Reals are always represented with IEEE doubles
      g->map[result] = g->map[arg];
   }
   else if (result_kind == VCODE_TYPE_ACCESS && arg_kind == VCODE_TYPE_ACCESS) {
      // Casting away opaqueness
      g->map[result] = g->map[arg];
   }
   else {
      vcode_dump_with_mark(op, NULL, NULL);
      fatal_trace("unhandled cast");
   }
}

static void irgen_op_range_null(jit_irgen_t *g, int op)
{
   jit_value_t left  = irgen_get_arg(g, op, 0);
   jit_value_t right = irgen_get_arg(g, op, 1);

   irgen_label_t *l_downto = irgen_alloc_label(g);
   irgen_label_t *l_after  = irgen_alloc_label(g);

   vcode_reg_t arg2 = vcode_get_arg(op, 2);
   if (arg2 != g->flags) {
      jit_value_t dir = irgen_get_value(g, arg2);
      j_cmp(g, JIT_CC_EQ, dir, jit_value_from_int64(RANGE_DOWNTO));
   }

   j_jump(g, JIT_CC_T, l_downto);

   j_cmp(g, JIT_CC_GT, left, right);
   j_jump(g, JIT_CC_NONE, l_after);

   irgen_bind_label(g, l_downto);

   j_cmp(g, JIT_CC_GT, right, left);

   irgen_bind_label(g, l_after);

   vcode_reg_t result = vcode_get_result(op);
   g->map[result] = j_cset(g);
   g->flags = result;
}

static void irgen_op_range_length(jit_irgen_t *g, int op)
{
   jit_value_t left  = irgen_get_arg(g, op, 0);
   jit_value_t right = irgen_get_arg(g, op, 1);
   jit_value_t dir   = irgen_get_arg(g, op, 2);

   irgen_label_t *l_downto = irgen_alloc_label(g);
   irgen_label_t *l_after  = irgen_alloc_label(g);

   j_cmp(g, JIT_CC_EQ, dir, jit_value_from_int64(RANGE_DOWNTO));
   j_jump(g, JIT_CC_T, l_downto);

   jit_value_t diff_up = j_sub(g, right, left);
   j_jump(g, JIT_CC_NONE, l_after);

   irgen_bind_label(g, l_downto);

   jit_value_t diff_down = j_sub(g, left, right);

   irgen_bind_label(g, l_after);

   jit_value_t diff = j_csel(g, diff_down, diff_up);
   jit_value_t zero = jit_value_from_int64(0);
   jit_value_t length = j_add(g, diff, jit_value_from_int64(1));
   j_cmp(g, JIT_CC_LT, diff, zero);
   jit_value_t clamped = j_csel(g, zero, length);

   g->map[vcode_get_result(op)] = clamped;
}

static void irgen_op_cond(jit_irgen_t *g, int op)
{
   vcode_reg_t arg0 = vcode_get_arg(op, 0);
   if (arg0 != g->flags) {
      jit_value_t test = irgen_get_value(g, arg0);
      j_cmp(g, JIT_CC_NE, test, jit_value_from_int64(0));
   }

   j_jump(g, JIT_CC_T, g->blocks[vcode_get_target(op, 0)]);
   j_jump(g, JIT_CC_NONE, g->blocks[vcode_get_target(op, 1)]);
}

static void irgen_op_case(jit_irgen_t *g, int op)
{
   jit_value_t value = irgen_get_arg(g, op, 0);

   const int nargs = vcode_count_args(op);
   for (int i = 1; i < nargs; i++) {
      j_cmp(g, JIT_CC_EQ, value, irgen_get_arg(g, op, i));
      j_jump(g, JIT_CC_T, g->blocks[vcode_get_target(op, i)]);
   }

   j_jump(g, JIT_CC_NONE, g->blocks[vcode_get_target(op, 0)]);
}

static void irgen_op_select(jit_irgen_t *g, int op)
{
   jit_value_t iftrue  = irgen_get_arg(g, op, 1);
   jit_value_t iffalse = irgen_get_arg(g, op, 2);

   vcode_reg_t arg0 = vcode_get_arg(op, 0);
   if (arg0 != g->flags) {
      jit_value_t test = irgen_get_arg(g, op, 0);
      j_cmp(g, JIT_CC_NE, test, jit_value_from_int64(0));
   }

   g->map[vcode_get_result(op)] = j_csel(g, iftrue, iffalse);
}

static void irgen_op_jump(jit_irgen_t *g, int op)
{
   j_jump(g, JIT_CC_NONE, g->blocks[vcode_get_target(op, 0)]);
}

static void irgen_op_fcall(jit_irgen_t *g, int op)
{
   irgen_emit_debuginfo(g, op);   // For stack traces

   if (vcode_get_subkind(op) == VCODE_CC_FOREIGN) {
      irgen_send_args(g, op, 0);

      jit_foreign_t *ff = irgen_ffi_for_call(g, op);

      jit_value_t addr = jit_addr_from_ptr(ff);
      macro_fficall(g, addr);

      vcode_reg_t result = vcode_get_result(op);
      if (result != VCODE_INVALID_REG)
         g->map[result] = j_recv(g, 0);
   }
   else {
      vcode_reg_t result = vcode_get_result(op);
      if (result == VCODE_INVALID_REG) {
         // Must call using procedure calling convention
         j_send(g, 0, jit_value_from_int64(0));
         irgen_send_args(g, op, 1);
      }
      else
         irgen_send_args(g, op, 0);

      ident_t func = vcode_get_func(op);
      jit_handle_t handle = jit_lazy_compile(g->func->jit, func);
      j_call(g, handle);

      if (result != VCODE_INVALID_REG) {
         const int slots = irgen_slots_for_type(vcode_reg_type(result));
         g->map[result] = j_recv(g, 0);
         for (int i = 1; i < slots; i++)
            j_recv(g, i);   // Must be contiguous registers
      }
   }
}

static void irgen_op_pcall(jit_irgen_t *g, int op)
{
   irgen_emit_debuginfo(g, op);   // For stack traces

   // First argument to procedure is suspended state
   j_send(g, 0, jit_value_from_int64(0));

   irgen_send_args(g, op, 1);

   ident_t func = vcode_get_func(op);

   jit_handle_t handle = jit_lazy_compile(g->func->jit, func);
   j_call(g, handle);

   irgen_label_t *cont = irgen_alloc_label(g);
   jit_value_t state = j_recv(g, 0);
   j_cmp(g, JIT_CC_EQ, state, jit_null_ptr());
   j_jump(g, JIT_CC_T, cont);
   macro_exit(g, JIT_EXIT_FUNC_WAIT);
   irgen_bind_label(g, cont);

   j_jump(g, JIT_CC_NONE, g->blocks[vcode_get_target(op, 0)]);
}

static void irgen_op_resume(jit_irgen_t *g, int op)
{
   // No-op without suspend
}

static void irgen_op_wait(jit_irgen_t *g, int op)
{
   vcode_reg_t after = vcode_get_arg(op, 0);
   if (after != VCODE_INVALID_REG) {
      // TODO: make this an optional arg, not invalid
      j_send(g, 0, irgen_get_arg(g, op, 0));
      macro_exit(g, JIT_EXIT_SCHED_PROCESS);
   }

   if (g->statereg.kind != JIT_VALUE_INVALID) {
      vcode_block_t target = vcode_get_target(op, 0);
      jit_value_t ptr = jit_addr_from_value(g->statereg, 2 * sizeof(void *));
      j_store(g, JIT_SZ_32, jit_value_from_int64(target), ptr);
   }

   if (vcode_unit_kind() == VCODE_UNIT_PROCEDURE)
      j_send(g, 0, g->statereg);
   else
      j_send(g, 0, jit_value_from_int64(0));

   j_ret(g);
}

static void irgen_op_protected_init(jit_irgen_t *g, int op)
{
   jit_value_t arg0 = irgen_get_arg(g, op, 0);
   ident_t func = vcode_get_func(op);

   jit_handle_t handle = jit_lazy_compile(g->func->jit, func);

   j_send(g, 0, arg0);
   j_call(g, handle);

   g->map[vcode_get_result(op)] = j_recv(g, 0);
}

static void irgen_op_protected_free(jit_irgen_t *g, int op)
{
   // No-op
   // TODO: files allowed in protected types
}

static void irgen_op_index_check(jit_irgen_t *g, int op)
{
   jit_value_t value = irgen_get_arg(g, op, 0);
   jit_value_t left  = irgen_get_arg(g, op, 1);
   jit_value_t right = irgen_get_arg(g, op, 2);
   jit_value_t dir   = irgen_get_arg(g, op, 3);
   jit_value_t locus = irgen_get_arg(g, op, 4);
   jit_value_t hint  = irgen_get_arg(g, op, 5);

   j_cmp(g, JIT_CC_EQ, dir, jit_value_from_int64(RANGE_DOWNTO));

   jit_value_t low = j_csel(g, right, left);
   jit_value_t high = j_csel(g, left, right);

   irgen_label_t *l_fail = irgen_alloc_label(g);
   irgen_label_t *l_pass = irgen_alloc_label(g);

   j_cmp(g, JIT_CC_GT, value, high);
   j_jump(g, JIT_CC_T, l_fail);
   j_cmp(g, JIT_CC_GE, value, low);
   j_jump(g, JIT_CC_T, l_pass);

   irgen_bind_label(g, l_fail);
   j_send(g, 0, value);
   j_send(g, 1, left);
   j_send(g, 2, right);
   j_send(g, 3, dir);
   j_send(g, 4, locus);
   j_send(g, 5, hint);
   macro_exit(g, JIT_EXIT_INDEX_FAIL);

   irgen_bind_label(g, l_pass);
}

static void irgen_op_range_check(jit_irgen_t *g, int op)
{
   jit_value_t value = irgen_get_arg(g, op, 0);
   jit_value_t left  = irgen_get_arg(g, op, 1);
   jit_value_t right = irgen_get_arg(g, op, 2);
   jit_value_t dir   = irgen_get_arg(g, op, 3);
   jit_value_t locus = irgen_get_arg(g, op, 4);
   jit_value_t hint  = irgen_get_arg(g, op, 5);

   j_cmp(g, JIT_CC_EQ, dir, jit_value_from_int64(RANGE_DOWNTO));

   jit_value_t low = j_csel(g, right, left);
   jit_value_t high = j_csel(g, left, right);

   irgen_label_t *l_fail = irgen_alloc_label(g);
   irgen_label_t *l_pass = irgen_alloc_label(g);

   if (vcode_reg_kind(vcode_get_arg(op, 0)) == VCODE_TYPE_REAL) {
      j_fcmp(g, JIT_CC_GT, value, high);
      j_jump(g, JIT_CC_T, l_fail);
      j_fcmp(g, JIT_CC_GE, value, low);
      j_jump(g, JIT_CC_T, l_pass);
   }
   else {
      j_cmp(g, JIT_CC_GT, value, high);
      j_jump(g, JIT_CC_T, l_fail);
      j_cmp(g, JIT_CC_GE, value, low);
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

static void irgen_op_null_check(jit_irgen_t *g, int op)
{
   jit_value_t ptr = irgen_get_arg(g, op, 0);
   jit_value_t locus = irgen_get_arg(g, op, 1);

   irgen_label_t *l_pass = irgen_alloc_label(g);

   j_cmp(g, JIT_CC_NE, ptr, jit_value_from_int64(0));
   j_jump(g, JIT_CC_T, l_pass);

   j_send(g, 0, locus);
   macro_exit(g, JIT_EXIT_NULL_DEREF);

   irgen_bind_label(g, l_pass);
}

static void irgen_op_zero_check(jit_irgen_t *g, int op)
{
   jit_value_t value = irgen_get_arg(g, op, 0);
   jit_value_t locus = irgen_get_arg(g, op, 1);

   irgen_label_t *l_pass = irgen_alloc_label(g);

   j_cmp(g, JIT_CC_NE, value, jit_value_from_int64(0));
   j_jump(g, JIT_CC_T, l_pass);

   j_send(g, 0, locus);
   macro_exit(g, JIT_EXIT_DIV_ZERO);

   irgen_bind_label(g, l_pass);
}

static void irgen_op_exponent_check(jit_irgen_t *g, int op)
{
   jit_value_t value = irgen_get_arg(g, op, 0);
   jit_value_t locus = irgen_get_arg(g, op, 1);

   irgen_label_t *l_pass = irgen_alloc_label(g);

   j_cmp(g, JIT_CC_GE, value, jit_value_from_int64(0));
   j_jump(g, JIT_CC_T, l_pass);

   j_send(g, 0, value);
   j_send(g, 1, locus);
   macro_exit(g, JIT_EXIT_EXPONENT_FAIL);

   irgen_bind_label(g, l_pass);
}

static void irgen_op_length_check(jit_irgen_t *g, int op)
{
   jit_value_t llen  = irgen_get_arg(g, op, 0);
   jit_value_t rlen  = irgen_get_arg(g, op, 1);
   jit_value_t locus = irgen_get_arg(g, op, 2);

   // TODO: why isn't this encoded with vcode_get_dim?
   jit_value_t dim;
   if (vcode_count_args(op) > 3)
      dim = irgen_get_arg(g, op, 3);
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

static void irgen_op_package_init(jit_irgen_t *g, int op)
{
   ident_t unit_name = vcode_get_func(op);
   jit_handle_t handle = jit_lazy_compile(g->func->jit, unit_name);

   if (vcode_count_args(op) > 0)
      j_send(g, 0, irgen_get_arg(g, op, 0));
   else
      j_send(g, 0, jit_value_from_int64(0));

   j_call(g, handle);

   g->map[vcode_get_result(op)] = j_recv(g, 0);
}

static void irgen_op_link_package(jit_irgen_t *g, int op)
{
   ident_t unit_name = vcode_get_ident(op);
   jit_handle_t handle = jit_lazy_compile(g->func->jit, unit_name);

   g->map[vcode_get_result(op)] = macro_getpriv(g, handle);
}

static void irgen_op_link_var(jit_irgen_t *g, int op)
{
   ident_t var_name = vcode_get_ident(op);
   ident_t unit_name = vtype_name(vcode_reg_type(vcode_get_arg(op, 0)));

   vcode_state_t state;
   vcode_state_save(&state);

   jit_handle_t handle = jit_compile(g->func->jit, unit_name);
   jit_func_t *f = jit_get_func(g->func->jit, handle);

   vcode_select_unit(f->unit);

   const int nvars = vcode_count_vars();
   vcode_var_t var = 0;
   for (; var < nvars; var++) {
      if (vcode_var_name(var) == var_name)
         break;
   }

   if (var == nvars) {
      vcode_dump();
      vcode_state_restore(&state);
      vcode_dump_with_mark(op, NULL, NULL);

      fatal_trace("variable %s not found in unit %s", istr(var_name),
                  istr(unit_name));
   }

   // TODO: can we store names in this table too to avoid loop above and
   //       better debug?
   assert(var < f->nvars);
   const unsigned offset = f->varoff[var];

   vcode_state_restore(&state);

   jit_value_t context = irgen_get_arg(g, op, 0);
   g->map[vcode_get_result(op)] = jit_addr_from_value(context, offset);
}

static void irgen_op_new(jit_irgen_t *g, int op)
{
   vcode_reg_t result = vcode_get_result(op);
   vcode_type_t vtype = vtype_pointed(vcode_reg_type(result));

   jit_value_t bytes = jit_value_from_int64(irgen_size_bytes(vtype));

   if (vcode_count_args(op) > 0)
      bytes = j_mul(g, bytes, irgen_get_arg(g, op, 0));

   g->map[vcode_get_result(op)] = macro_galloc(g, bytes);
}

static void irgen_op_alloc(jit_irgen_t *g, int op)
{
   vcode_reg_t result = vcode_get_result(op);
   vcode_type_t vtype = vtype_pointed(vcode_reg_type(result));

   const int bytes = irgen_size_bytes(vtype);

   jit_value_t count = irgen_get_arg(g, op, 0);
   jit_value_t total = j_mul(g, count, jit_value_from_int64(bytes));

   g->map[vcode_get_result(op)] = macro_galloc(g, total);
}

static void irgen_op_all(jit_irgen_t *g, int op)
{
   jit_value_t arg = irgen_get_arg(g, op, 0);
   g->map[vcode_get_result(op)] = jit_addr_from_value(arg, 0);
}

static void irgen_op_closure(jit_irgen_t *g, int op)
{
   jit_handle_t handle = jit_lazy_compile(g->func->jit, vcode_get_func(op));

   jit_reg_t base = irgen_alloc_reg(g);
   j_mov(g, base, jit_value_from_handle(handle));

   jit_reg_t context = irgen_alloc_reg(g);
   j_mov(g, context, irgen_get_arg(g, op, 0));

   g->map[vcode_get_result(op)] = jit_value_from_reg(base);
}

static void irgen_op_resolution_wrapper(jit_irgen_t *g, int op)
{
   jit_value_t closure = irgen_get_arg(g, op, 0);

   jit_reg_t base = irgen_alloc_reg(g);
   j_mov(g, base, closure);

   jit_reg_t context = irgen_alloc_reg(g);
   j_mov(g, context, jit_value_from_reg(jit_value_as_reg(closure) + 1));

   jit_reg_t ileft = irgen_alloc_reg(g);
   j_mov(g, ileft, irgen_get_arg(g, op, 1));

   jit_reg_t nlits = irgen_alloc_reg(g);
   j_mov(g, nlits, irgen_get_arg(g, op, 2));

   g->map[vcode_get_result(op)] = jit_value_from_reg(base);
}

static void irgen_op_init_signal(jit_irgen_t *g, int op)
{
   jit_value_t count = irgen_get_arg(g, op, 0);
   jit_value_t size  = irgen_get_arg(g, op, 1);
   jit_value_t value = irgen_get_arg(g, op, 2);
   jit_value_t flags = irgen_get_arg(g, op, 3);
   jit_value_t locus = irgen_get_arg(g, op, 4);

   jit_value_t offset;
   if (vcode_count_args(op) > 5)
      offset = irgen_get_arg(g, op, 5);
   else
      offset = jit_value_from_int64(0);

   jit_value_t scalar = irgen_is_scalar(g, op, 2);

   j_send(g, 0, count);
   j_send(g, 1, size);
   j_send(g, 2, value);
   j_send(g, 3, flags);
   j_send(g, 4, locus);
   j_send(g, 5, offset);
   j_send(g, 6, scalar);

   macro_exit(g, JIT_EXIT_INIT_SIGNAL);

   g->map[vcode_get_result(op)] = j_recv(g, 0);

   // Offset into signal must be next sequential register
   jit_reg_t next = irgen_alloc_reg(g);
   j_mov(g, next, jit_value_from_int64(0));
}

static void irgen_op_alias_signal(jit_irgen_t *g, int op)
{
   jit_value_t shared = irgen_get_arg(g, op, 0);
   jit_value_t locus  = irgen_get_arg(g, op, 1);

   j_send(g, 0, shared);
   j_send(g, 1, locus);

   macro_exit(g, JIT_EXIT_ALIAS_SIGNAL);
}

static void irgen_op_map_signal(jit_irgen_t *g, int op)
{
   jit_value_t src_ss    = irgen_get_arg(g, op, 0);
   jit_value_t src_off   = jit_value_from_reg(jit_value_as_reg(src_ss) + 1);
   jit_value_t dst_ss    = irgen_get_arg(g, op, 1);
   jit_value_t dst_off   = jit_value_from_reg(jit_value_as_reg(dst_ss) + 1);
   jit_value_t src_count = irgen_get_arg(g, op, 2);
   jit_value_t dst_count = irgen_get_arg(g, op, 3);

   jit_value_t closure;
   if (vcode_count_args(op) == 5)
      closure = irgen_get_arg(g, op, 4);
   else
      closure = jit_null_ptr();

   j_send(g, 0, src_ss);
   j_send(g, 1, src_off);
   j_send(g, 2, dst_ss);
   j_send(g, 3, dst_off);
   j_send(g, 4, src_count);
   j_send(g, 5, dst_count);
   j_send(g, 6, closure);

   macro_exit(g, JIT_EXIT_MAP_SIGNAL);
}

static void irgen_op_map_const(jit_irgen_t *g, int op)
{
   jit_value_t initval   = irgen_get_arg(g, op, 0);
   jit_value_t dst_ss    = irgen_get_arg(g, op, 1);
   jit_value_t dst_off   = jit_value_from_reg(jit_value_as_reg(dst_ss) + 1);
   jit_value_t dst_count = irgen_get_arg(g, op, 2);

   jit_value_t scalar = irgen_is_scalar(g, op, 0);

   j_send(g, 0, dst_ss);
   j_send(g, 1, dst_off);
   j_send(g, 2, initval);
   j_send(g, 3, dst_count);
   j_send(g, 4, scalar);

   macro_exit(g, JIT_EXIT_MAP_CONST);
}

static void irgen_op_resolve_signal(jit_irgen_t *g, int op)
{
   jit_value_t shared  = irgen_get_arg(g, op, 0);
   jit_value_t resfn   = irgen_get_arg(g, op, 1);
   jit_reg_t   base    = jit_value_as_reg(resfn);
   jit_value_t context = jit_value_from_reg(base + 1);
   jit_value_t ileft   = jit_value_from_reg(base + 3);
   jit_value_t nlits   = jit_value_from_reg(base + 4);

   j_send(g, 0, shared);
   j_send(g, 1, resfn);
   j_send(g, 2, context);
   j_send(g, 3, ileft);
   j_send(g, 4, nlits);

   macro_exit(g, JIT_EXIT_RESOLVE_SIGNAL);
}

static void irgen_op_unreachable(jit_irgen_t *g, int op)
{
   jit_value_t locus;
   if (vcode_count_args(op) > 0)
      locus = irgen_get_arg(g, op, 0);
   else
      locus = jit_value_from_int64(0);

   j_send(g, 0, locus);
   macro_exit(g, JIT_EXIT_UNREACHABLE);
}

static void irgen_op_report(jit_irgen_t *g, int op)
{
   jit_value_t severity = irgen_get_arg(g, op, 0);
   jit_value_t msg      = irgen_get_arg(g, op, 1);
   jit_value_t length   = irgen_get_arg(g, op, 2);
   jit_value_t locus    = irgen_get_arg(g, op, 3);

   j_send(g, 0, msg);
   j_send(g, 1, length);
   j_send(g, 2, severity);
   j_send(g, 3, locus);
   macro_exit(g, JIT_EXIT_REPORT);
}

static void irgen_op_assert(jit_irgen_t *g, int op)
{
   jit_value_t value    = irgen_get_arg(g, op, 0);
   jit_value_t severity = irgen_get_arg(g, op, 1);
   jit_value_t locus    = irgen_get_arg(g, op, 4);

   // TODO: returning VCODE_INVALID_REG sucks - why not have optional args?
   jit_value_t msg    = jit_value_from_int64(0);
   jit_value_t length = jit_value_from_int64(0);
   if (vcode_get_arg(op, 2) != VCODE_INVALID_REG) {
      msg    = irgen_get_arg(g, op, 2);
      length = irgen_get_arg(g, op, 3);
   }

   jit_value_t hint_left, hint_right, hint_valid;
   if (vcode_count_args(op) > 5) {
      hint_left  = irgen_get_arg(g, op, 5);
      hint_right = irgen_get_arg(g, op, 6);
      hint_valid = jit_value_from_int64(true);
   }
   else {
      hint_left = hint_right = jit_value_from_int64(0);
      hint_valid = jit_value_from_int64(false);
   }

   irgen_label_t *l_pass = irgen_alloc_label(g);

   j_cmp(g, JIT_CC_NE, value, jit_value_from_int64(0));
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

static void irgen_op_deallocate(jit_irgen_t *g, int op)
{
   jit_value_t ptr  = irgen_get_arg(g, op, 0);
   jit_value_t null = jit_value_from_int64(0);

   j_store(g, JIT_SZ_PTR, null, jit_addr_from_value(ptr, 0));
}

static void irgen_op_file_open(jit_irgen_t *g, int op)
{
   jit_value_t file   = irgen_get_arg(g, op, 0);
   jit_value_t name   = irgen_get_arg(g, op, 1);
   jit_value_t length = irgen_get_arg(g, op, 2);
   jit_value_t kind   = irgen_get_arg(g, op, 3);
   jit_value_t locus  = irgen_get_arg(g, op, 4);

   jit_value_t status = jit_null_ptr();
   if (vcode_count_args(op) == 6)
      status = irgen_get_arg(g, op, 5);

   j_send(g, 0, status);
   j_send(g, 1, file);
   j_send(g, 2, name);
   j_send(g, 3, length);
   j_send(g, 4, kind);
   j_send(g, 5, locus);

   macro_exit(g, JIT_EXIT_FILE_OPEN);
}

static void irgen_op_file_close(jit_irgen_t *g, int op)
{
   jit_value_t file = irgen_get_arg(g, op, 0);

   j_send(g, 0, file);
   macro_exit(g, JIT_EXIT_FILE_CLOSE);
}

static void irgen_op_file_read(jit_irgen_t *g, int op)
{
   jit_value_t file = irgen_get_arg(g, op, 0);
   jit_value_t ptr  = irgen_get_arg(g, op, 1);

   jit_value_t count = jit_value_from_int64(1);
   if (vcode_count_args(op) >= 3)
      count = irgen_get_arg(g, op, 2);

   vcode_type_t ptr_type = vcode_reg_type(vcode_get_arg(op, 1));
   vcode_type_t elem_type = vtype_pointed(ptr_type);

   jit_value_t size = jit_value_from_int64(irgen_size_bytes(elem_type));

   jit_value_t outlen = jit_null_ptr();
   if (vcode_count_args(op) >= 4)
      outlen = irgen_get_arg(g, op, 3);

   j_send(g, 0, file);
   j_send(g, 1, ptr);
   j_send(g, 2, size);
   j_send(g, 3, count);
   j_send(g, 4, outlen);

   macro_exit(g, JIT_EXIT_FILE_READ);
}

static void irgen_op_file_write(jit_irgen_t *g, int op)
{
   jit_value_t file = irgen_get_arg(g, op, 0);
   jit_value_t data = irgen_get_arg(g, op, 1);

   jit_value_t ptr = data;
   vcode_type_t data_type = vcode_reg_type(vcode_get_arg(op, 1)), elem_type;
   if (vtype_kind(data_type) != VCODE_TYPE_POINTER) {
      ptr = irgen_stack_space(g, data_type);
      j_store(g, irgen_jit_size(data_type), data, ptr);
      elem_type = data_type;
   }
   else
      elem_type = vtype_pointed(data_type);

   jit_value_t bytes = jit_value_from_int64(irgen_size_bytes(elem_type));
   if (vcode_count_args(op) >= 3) {
      jit_value_t count = irgen_get_arg(g, op, 2);
      bytes = j_mul(g, bytes, count);
   }

   j_send(g, 0, file);
   j_send(g, 1, ptr);
   j_send(g, 2, bytes);

   macro_exit(g, JIT_EXIT_FILE_WRITE);
}

static void irgen_op_endfile(jit_irgen_t *g, int op)
{
   jit_value_t file = irgen_get_arg(g, op, 0);

   j_send(g, 0, file);
   macro_exit(g, JIT_EXIT_ENDFILE);

   g->map[vcode_get_result(op)] = j_recv(g, 0);
}

static void irgen_op_push_scope(jit_irgen_t *g, int op)
{
   // No-op
}

static void irgen_op_pop_scope(jit_irgen_t *g, int op)
{
   // No-op
}

static void irgen_op_drive_signal(jit_irgen_t *g, int op)
{
   jit_value_t shared = irgen_get_arg(g, op, 0);
   jit_value_t offset = jit_value_from_reg(jit_value_as_reg(shared) + 1);
   jit_value_t count  = irgen_get_arg(g, op, 1);

   j_send(g, 0, shared);
   j_send(g, 1, offset);
   j_send(g, 2, count);
   macro_exit(g, JIT_EXIT_DRIVE_SIGNAL);
}

static void irgen_op_resolved(jit_irgen_t *g, int op)
{
   jit_value_t shared = irgen_get_arg(g, op, 0);
   jit_value_t offset = jit_value_from_reg(jit_value_as_reg(shared) + 1);

   jit_value_t data_ptr = irgen_lea(g, jit_addr_from_value(shared, 8));

   // XXX: scale by type size???

   g->map[vcode_get_result(op)] = j_add(g, data_ptr, offset);
}

static void irgen_op_last_value(jit_irgen_t *g, int op)
{
   jit_value_t shared = irgen_get_arg(g, op, 0);
   jit_value_t offset = jit_value_from_reg(jit_value_as_reg(shared) + 1);

   jit_value_t data_ptr = irgen_lea(g, jit_addr_from_value(shared, 8));
   jit_value_t size = j_load(g, JIT_SZ_32, jit_addr_from_value(shared, 0));

   jit_value_t last_value = j_add(g, data_ptr, size);

   // XXX: scale by type size???

   g->map[vcode_get_result(op)] = j_add(g, last_value, offset);
}

static void irgen_op_sched_waveform(jit_irgen_t *g, int op)
{
   jit_value_t shared = irgen_get_arg(g, op, 0);
   jit_value_t offset = jit_value_from_reg(jit_value_as_reg(shared) + 1);
   jit_value_t count  = irgen_get_arg(g, op, 1);
   jit_value_t value  = irgen_get_arg(g, op, 2);
   jit_value_t reject = irgen_get_arg(g, op, 3);
   jit_value_t after  = irgen_get_arg(g, op, 4);

   jit_value_t scalar = irgen_is_scalar(g, op, 2);

   j_send(g, 0, shared);
   j_send(g, 1, offset);
   j_send(g, 2, count);
   j_send(g, 3, value);
   j_send(g, 4, after);
   j_send(g, 5, reject);
   j_send(g, 6, scalar);

   macro_exit(g, JIT_EXIT_SCHED_WAVEFORM);
}

static void irgen_op_sched_event(jit_irgen_t *g, int op)
{
   jit_value_t shared = irgen_get_arg(g, op, 0);
   jit_value_t offset = jit_value_from_reg(jit_value_as_reg(shared) + 1);
   jit_value_t count  = irgen_get_arg(g, op, 1);

   j_send(g, 0, shared);
   j_send(g, 1, offset);
   j_send(g, 2, count);
   j_send(g, 3, jit_value_from_int64(false));
   j_send(g, 4, jit_null_ptr());
   macro_exit(g, JIT_EXIT_SCHED_EVENT);
}

static void irgen_op_sched_static(jit_irgen_t *g, int op)
{
   jit_value_t shared = irgen_get_arg(g, op, 0);
   jit_value_t offset = jit_value_from_reg(jit_value_as_reg(shared) + 1);
   jit_value_t count  = irgen_get_arg(g, op, 1);

   assert(vcode_count_args(op) == 2);  // TODO: guarded signals

   j_send(g, 0, shared);
   j_send(g, 1, offset);
   j_send(g, 2, count);
   j_send(g, 3, jit_value_from_int64(true));
   j_send(g, 4, jit_null_ptr());
   macro_exit(g, JIT_EXIT_SCHED_EVENT);
}

static void irgen_op_event(jit_irgen_t *g, int op)
{
   jit_value_t shared = irgen_get_arg(g, op, 0);
   jit_value_t offset = jit_value_from_reg(jit_value_as_reg(shared) + 1);
   jit_value_t count  = irgen_get_arg(g, op, 1);

   j_send(g, 0, shared);
   j_send(g, 1, offset);
   j_send(g, 2, count);
   macro_exit(g, JIT_EXIT_TEST_EVENT);

   g->map[vcode_get_result(op)] = j_recv(g, 0);
}

static void irgen_op_active(jit_irgen_t *g, int op)
{
   jit_value_t shared = irgen_get_arg(g, op, 0);
   jit_value_t offset = jit_value_from_reg(jit_value_as_reg(shared) + 1);
   jit_value_t count  = irgen_get_arg(g, op, 1);

   j_send(g, 0, shared);
   j_send(g, 1, offset);
   j_send(g, 2, count);
   macro_exit(g, JIT_EXIT_TEST_ACTIVE);

   g->map[vcode_get_result(op)] = j_recv(g, 0);
}

static void irgen_op_strconv(jit_irgen_t *g, int op)
{
   jit_value_t ptr = irgen_get_arg(g, op, 0);
   jit_value_t len = irgen_get_arg(g, op, 1);

   jit_value_t used = jit_null_ptr();
   if (vcode_count_args(op) > 2)
      used = irgen_get_arg(g, op, 2);

   j_send(g, 0, ptr);
   j_send(g, 1, len);
   j_send(g, 2, used);

   switch (vtype_kind(vcode_get_type(op))) {
   case VCODE_TYPE_INT:
      macro_exit(g, JIT_EXIT_STRING_TO_INT);
      break;
   case VCODE_TYPE_REAL:
      macro_exit(g, JIT_EXIT_STRING_TO_REAL);
      break;
   default:
      vcode_dump_with_mark(op, NULL, NULL);
      fatal_trace("invalid type in strconv");
   }

   g->map[vcode_get_result(op)] = j_recv(g, 0);
}

static void irgen_op_convstr(jit_irgen_t *g, int op)
{
   jit_value_t value = irgen_get_arg(g, op, 0);

   j_send(g, 0, value);

   switch (vcode_reg_kind(vcode_get_arg(op, 0))) {
   case VCODE_TYPE_INT:
      macro_exit(g, JIT_EXIT_INT_TO_STRING);
      break;
   case VCODE_TYPE_REAL:
      macro_exit(g, JIT_EXIT_REAL_TO_STRING);
      break;
   default:
      vcode_dump_with_mark(op, NULL, NULL);
      fatal_trace("invalid type in convstr");
   }

   g->map[vcode_get_result(op)] = j_recv(g, 0);
   j_recv(g, 1);   // Left
   j_recv(g, 2);   // Length
}

static void irgen_op_canon_value(jit_irgen_t *g, int op)
{
   jit_value_t ptr = irgen_get_arg(g, op, 0);
   jit_value_t len = irgen_get_arg(g, op, 1);

   j_send(g, 0, ptr);
   j_send(g, 1, len);
   macro_exit(g, JIT_EXIT_CANON_VALUE);

   g->map[vcode_get_result(op)] = j_recv(g, 0);
   j_recv(g, 1);   // Left
   j_recv(g, 2);   // Length
}

static void irgen_op_debug_out(jit_irgen_t *g, int op)
{
   jit_value_t value = irgen_get_arg(g, op, 0);

   j_send(g, 0, value);
   macro_exit(g, JIT_EXIT_DEBUG_OUT);
}

static void irgen_op_last_event(jit_irgen_t *g, int op)
{
   jit_value_t shared = irgen_get_arg(g, op, 0);
   jit_value_t offset = jit_value_from_reg(jit_value_as_reg(shared) + 1);

   jit_value_t count;
   if (vcode_count_args(op) > 1)
      count = irgen_get_arg(g, op, 1);
   else
      count = jit_value_from_int64(1);

   j_send(g, 0, shared);
   j_send(g, 1, offset);
   j_send(g, 2, count);
   macro_exit(g, JIT_EXIT_LAST_EVENT);

   g->map[vcode_get_result(op)] = j_recv(g, 0);
}

static void irgen_op_last_active(jit_irgen_t *g, int op)
{
   jit_value_t shared = irgen_get_arg(g, op, 0);
   jit_value_t offset = jit_value_from_reg(jit_value_as_reg(shared) + 1);

   jit_value_t count;
   if (vcode_count_args(op) > 1)
      count = irgen_get_arg(g, op, 1);
   else
      count = jit_value_from_int64(1);

   j_send(g, 0, shared);
   j_send(g, 1, offset);
   j_send(g, 2, count);
   macro_exit(g, JIT_EXIT_LAST_ACTIVE);

   g->map[vcode_get_result(op)] = j_recv(g, 0);
}

static void irgen_block(jit_irgen_t *g, vcode_block_t block)
{
   vcode_select_block(block);

   irgen_bind_label(g, g->blocks[block]);

   g->flags = VCODE_INVALID_REG;

   const int nops = vcode_count_ops();
   for (int i = 0; i < nops; i++) {
      const vcode_op_t op = vcode_get_op(i);
      if (op == VCODE_OP_COMMENT)
         continue;

      DEBUG_ONLY(irgen_emit_debuginfo(g, i));

      switch (op) {
      case VCODE_OP_CONST:
         irgen_op_const(g, i);
         break;
      case VCODE_OP_CONST_REAL:
         irgen_op_const_real(g, i);
         break;
      case VCODE_OP_CONST_ARRAY:
         irgen_op_const_array(g, i);
         break;
      case VCODE_OP_CONST_REP:
         irgen_op_const_rep(g, i);
         break;
      case VCODE_OP_CONST_RECORD:
         irgen_op_const_record(g, i);
         break;
      case VCODE_OP_ADDRESS_OF:
         irgen_op_address_of(g, i);
         break;
      case VCODE_OP_DEBUG_LOCUS:
         irgen_op_debug_locus(g, i);
         break;
      case VCODE_OP_TRAP_ADD:
         irgen_op_trap_add(g, i);
         break;
      case VCODE_OP_NOT:
         irgen_op_not(g, i);
         break;
      case VCODE_OP_AND:
         irgen_op_and(g, i);
         break;
      case VCODE_OP_NAND:
         irgen_op_nand(g, i);
         break;
      case VCODE_OP_OR:
         irgen_op_or(g, i);
         break;
      case VCODE_OP_NOR:
         irgen_op_nor(g, i);
         break;
      case VCODE_OP_XOR:
         irgen_op_xor(g, i);
         break;
      case VCODE_OP_XNOR:
         irgen_op_xnor(g, i);
         break;
      case VCODE_OP_ADD:
         irgen_op_add(g, i);
         break;
      case VCODE_OP_TRAP_MUL:
         irgen_op_trap_mul(g, i);
         break;
      case VCODE_OP_MUL:
         irgen_op_mul(g, i);
         break;
      case VCODE_OP_DIV:
         irgen_op_div(g, i);
         break;
      case VCODE_OP_EXP:
         irgen_op_exp(g, i);
         break;
      case VCODE_OP_SUB:
         irgen_op_sub(g, i);
         break;
      case VCODE_OP_TRAP_SUB:
         irgen_op_trap_sub(g, i);
         break;
      case VCODE_OP_NEG:
         irgen_op_neg(g, i);
         break;
      case VCODE_OP_ABS:
         irgen_op_abs(g, i);
         break;
      case VCODE_OP_MOD:
         irgen_op_mod(g, i);
         break;
      case VCODE_OP_REM:
         irgen_op_rem(g, i);
         break;
      case VCODE_OP_CMP:
         irgen_op_cmp(g, i);
         break;
      case VCODE_OP_RETURN:
         irgen_op_return(g, i);
         break;
      case VCODE_OP_STORE:
         irgen_op_store(g, i);
         break;
      case VCODE_OP_LOAD:
         irgen_op_load(g, i);
         break;
      case VCODE_OP_LOAD_INDIRECT:
         irgen_op_load_indirect(g, i);
         break;
      case VCODE_OP_STORE_INDIRECT:
         irgen_op_store_indirect(g, i);
         break;
      case VCODE_OP_WRAP:
         irgen_op_wrap(g, i);
         break;
      case VCODE_OP_UARRAY_LEFT:
         irgen_op_uarray_left(g, i);
         break;
      case VCODE_OP_UARRAY_RIGHT:
         irgen_op_uarray_right(g, i);
         break;
      case VCODE_OP_UARRAY_DIR:
         irgen_op_uarray_dir(g, i);
         break;
      case VCODE_OP_UARRAY_LEN:
         irgen_op_uarray_len(g, i);
         break;
      case VCODE_OP_UNWRAP:
         irgen_op_unwrap(g, i);
         break;
      case VCODE_OP_CAST:
         irgen_op_cast(g, i);
         break;
      case VCODE_OP_RANGE_NULL:
         irgen_op_range_null(g, i);
         break;
      case VCODE_OP_RANGE_LENGTH:
         irgen_op_range_length(g, i);
         break;
      case VCODE_OP_COND:
         irgen_op_cond(g, i);
         break;
      case VCODE_OP_SELECT:
         irgen_op_select(g, i);
         break;
      case VCODE_OP_JUMP:
         irgen_op_jump(g, i);
         break;
      case VCODE_OP_ARRAY_REF:
         irgen_op_array_ref(g, i);
         break;
      case VCODE_OP_CONTEXT_UPREF:
         irgen_op_context_upref(g, i);
         break;
      case VCODE_OP_FCALL:
         irgen_op_fcall(g, i);
         break;
      case VCODE_OP_PCALL:
         irgen_op_pcall(g, i);
         break;
      case VCODE_OP_RESUME:
         irgen_op_resume(g, i);
         break;
      case VCODE_OP_WAIT:
         irgen_op_wait(g, i);
         break;
      case VCODE_OP_INDEX:
         irgen_op_index(g, i);
         break;
      case VCODE_OP_COPY:
         irgen_op_copy(g, i);
         break;
      case VCODE_OP_MEMSET:
         irgen_op_memset(g, i);
         break;
      case VCODE_OP_VAR_UPREF:
         irgen_op_var_upref(g, i);
         break;
      case VCODE_OP_INDEX_CHECK:
         irgen_op_index_check(g, i);
         break;
      case VCODE_OP_RANGE_CHECK:
         irgen_op_range_check(g, i);
         break;
      case VCODE_OP_PACKAGE_INIT:
         irgen_op_package_init(g, i);
         break;
      case VCODE_OP_LINK_PACKAGE:
         irgen_op_link_package(g, i);
         break;
      case VCODE_OP_LINK_VAR:
         irgen_op_link_var(g, i);
         break;
      case VCODE_OP_RECORD_REF:
         irgen_op_record_ref(g, i);
         break;
      case VCODE_OP_NULL:
         irgen_op_null(g, i);
         break;
      case VCODE_OP_NEW:
         irgen_op_new(g, i);
         break;
      case VCODE_OP_ALLOC:
         irgen_op_alloc(g, i);
         break;
      case VCODE_OP_ALL:
         irgen_op_all(g, i);
         break;
      case VCODE_OP_EXPONENT_CHECK:
         irgen_op_exponent_check(g, i);
         break;
      case VCODE_OP_NULL_CHECK:
         irgen_op_null_check(g, i);
         break;
      case VCODE_OP_ZERO_CHECK:
         irgen_op_zero_check(g, i);
         break;
      case VCODE_OP_LENGTH_CHECK:
         irgen_op_length_check(g, i);
         break;
      case VCODE_OP_CLOSURE:
         irgen_op_closure(g, i);
         break;
      case VCODE_OP_RESOLUTION_WRAPPER:
         irgen_op_resolution_wrapper(g, i);
         break;
      case VCODE_OP_INIT_SIGNAL:
         irgen_op_init_signal(g, i);
         break;
      case VCODE_OP_ALIAS_SIGNAL:
         irgen_op_alias_signal(g, i);
         break;
      case VCODE_OP_MAP_SIGNAL:
         irgen_op_map_signal(g, i);
         break;
      case VCODE_OP_MAP_CONST:
         irgen_op_map_const(g, i);
         break;
      case VCODE_OP_RESOLVE_SIGNAL:
         irgen_op_resolve_signal(g, i);
         break;
      case VCODE_OP_UNREACHABLE:
         irgen_op_unreachable(g, i);
         break;
      case VCODE_OP_REPORT:
         irgen_op_report(g, i);
         break;
      case VCODE_OP_ASSERT:
         irgen_op_assert(g, i);
         break;
      case VCODE_OP_CASE:
         irgen_op_case(g, i);
         break;
      case VCODE_OP_DEALLOCATE:
         irgen_op_deallocate(g, i);
         break;
      case VCODE_OP_PROTECTED_INIT:
         irgen_op_protected_init(g, i);
         break;
      case VCODE_OP_PROTECTED_FREE:
         irgen_op_protected_free(g, i);
         break;
      case VCODE_OP_FILE_OPEN:
         irgen_op_file_open(g, i);
         break;
      case VCODE_OP_FILE_CLOSE:
         irgen_op_file_close(g, i);
         break;
      case VCODE_OP_FILE_READ:
         irgen_op_file_read(g, i);
         break;
      case VCODE_OP_FILE_WRITE:
         irgen_op_file_write(g, i);
         break;
      case VCODE_OP_ENDFILE:
         irgen_op_endfile(g, i);
         break;
      case VCODE_OP_PUSH_SCOPE:
         irgen_op_push_scope(g, i);
         break;
      case VCODE_OP_POP_SCOPE:
         irgen_op_pop_scope(g, i);
         break;
      case VCODE_OP_DRIVE_SIGNAL:
         irgen_op_drive_signal(g, i);
         break;
      case VCODE_OP_RESOLVED:
         irgen_op_resolved(g, i);
         break;
      case VCODE_OP_LAST_VALUE:
         irgen_op_last_value(g, i);
         break;
      case VCODE_OP_SCHED_WAVEFORM:
         irgen_op_sched_waveform(g, i);
         break;
      case VCODE_OP_EVENT:
         irgen_op_event(g, i);
         break;
      case VCODE_OP_ACTIVE:
         irgen_op_active(g, i);
         break;
      case VCODE_OP_SCHED_STATIC:
         irgen_op_sched_static(g, i);
         break;
      case VCODE_OP_SCHED_EVENT:
         irgen_op_sched_event(g, i);
         break;
      case VCODE_OP_STRCONV:
         irgen_op_strconv(g, i);
         break;
      case VCODE_OP_CONVSTR:
         irgen_op_convstr(g, i);
         break;
      case VCODE_OP_CANON_VALUE:
         irgen_op_canon_value(g, i);
         break;
      case VCODE_OP_DEBUG_OUT:
         irgen_op_debug_out(g, i);
         break;
      case VCODE_OP_LAST_EVENT:
         irgen_op_last_event(g, i);
         break;
      case VCODE_OP_LAST_ACTIVE:
         irgen_op_last_active(g, i);
         break;
      default:
         fatal("cannot generate JIT IR for vcode op %s", vcode_op_string(op));
      }
   }
}

static void irgen_locals(jit_irgen_t *g)
{
   const int nvars = g->func->nvars = vcode_count_vars();

   g->vars = xmalloc_array(nvars, sizeof(jit_value_t));
   g->func->varoff = xmalloc_array(nvars, sizeof(unsigned));

   bool on_stack = true;
   const vunit_kind_t kind = vcode_unit_kind();
   if (kind != VCODE_UNIT_THUNK && kind != VCODE_UNIT_FUNCTION)
      on_stack = false;
   else if (vcode_unit_child(vcode_active_unit()) != NULL)
      on_stack = false;
   else {
      for (int i = 0; i < nvars; i++) {
         // TODO: this seems a bit extreme, we only need to allocate
         // this one variable on the heap. Likewise temp vars can always
         // go on the stack.
         if (vcode_var_flags(i) & VAR_HEAP)
            on_stack = false;
      }
   }

   if (on_stack) {
      // Local variables on stack
      size_t sz = 0;
      for (int i = 0; i < nvars; i++) {
         vcode_type_t vtype = vcode_var_type(i);
         const int align = irgen_align_of(vtype);
         sz = ALIGN_UP(sz, align);
         g->vars[i] = jit_value_from_frame_addr(sz);
         g->func->varoff[i] = sz;
         sz += irgen_size_bytes(vtype);
      }

      g->func->framesz = sz;
   }
   else {
      // Local variables on heap
      size_t sz = 0;
      sz += sizeof(void *);   // Context parameter
      sz += sizeof(void *);   // Suspended procedure state
      sz += sizeof(int32_t);  // State number

      for (int i = 0; i < nvars; i++) {
         vcode_type_t vtype = vcode_var_type(i);
         const int align = irgen_align_of(vtype);
         sz = ALIGN_UP(sz, align);
         g->func->varoff[i] = sz;
         sz += irgen_size_bytes(vtype);
      }

      jit_value_t mem = macro_galloc(g, jit_value_from_int64(sz));
      if (g->statereg.kind != JIT_VALUE_INVALID) {
         // A null state was passed in by the caller
         j_mov(g, jit_value_as_reg(g->statereg), mem);
      }
      else
         g->statereg = mem;

      for (int i = 0; i < nvars; i++)
         g->vars[i] = jit_addr_from_value(g->statereg, g->func->varoff[i]);
   }
}

static void irgen_params(jit_irgen_t *g, int first)
{
   ffi_spec_t spec = 0;

   const int nparams = vcode_count_params();
   for (int i = 0, pslot = first; i < nparams; i++) {
      vcode_reg_t preg = vcode_param_reg(i);
      vcode_type_t vtype = vcode_param_type(i);
      int slots = irgen_slots_for_type(vtype);
      if (vcode_reg_kind(preg) == VCODE_TYPE_SIGNAL)
         slots++;
      g->map[preg] = j_recv(g, pslot++);
      for (int i = 1; i < slots; i++)
         j_recv(g, pslot++);   // Must be contiguous registers
      spec |= irgen_ffi_type(vtype) << (i + 1) * 4;
   }

   vunit_kind_t kind = vcode_unit_kind();
   bool has_state_arg = (kind == VCODE_UNIT_PROCEDURE);
   if (kind == VCODE_UNIT_FUNCTION || kind == VCODE_UNIT_THUNK) {
      vcode_type_t rtype = vcode_unit_result();
      if (rtype == VCODE_INVALID_TYPE)
         has_state_arg = true;
      else
         spec |= irgen_ffi_type(rtype);
   }

   if (has_state_arg)
      spec = (spec << 4) | (FFI_POINTER << 4);

   g->func->spec = spec;
}

static void irgen_jump_table(jit_irgen_t *g)
{
   irgen_label_t *cont = irgen_alloc_label(g);
   jit_value_t locals = g->statereg = j_recv(g, 0);
   j_cmp(g, JIT_CC_EQ, locals, jit_value_from_int64(0));
   j_jump(g, JIT_CC_T, cont);

   jit_value_t state_ptr = jit_addr_from_value(locals, 2 * sizeof(void *));
   jit_value_t state = j_load(g, JIT_SZ_32, state_ptr);

   const bool is_process = (vcode_unit_kind() == VCODE_UNIT_PROCESS);
   const int nblocks = vcode_count_blocks();

   bit_mask_t have;
   mask_init(&have, nblocks);

   for (int i = 0; i < nblocks; i++) {
      vcode_select_block(i);

      const int last = vcode_count_ops() - 1;
      if (last < 0)
         continue;

      vcode_block_t target = VCODE_INVALID_BLOCK;
      vcode_op_t last_op = vcode_get_op(last);

      if (last_op == VCODE_OP_WAIT || last_op == VCODE_OP_PCALL)
         target = vcode_get_target(last, 0);
      else if (is_process && last_op == VCODE_OP_RETURN)
         target = 1;

      if (target == VCODE_INVALID_BLOCK || mask_test(&have, target))
         continue;

      j_cmp(g, JIT_CC_EQ, state, jit_value_from_int64(target));
      j_jump(g, JIT_CC_T, g->blocks[target]);

      mask_set(&have, target);
   }

   irgen_bind_label(g, cont);
   mask_free(&have);
}

static bool irgen_is_procedure(void)
{
   switch (vcode_unit_kind()) {
   case VCODE_UNIT_PROCEDURE:
      return true;
   case VCODE_UNIT_FUNCTION:
   case VCODE_UNIT_THUNK:
      // Procedure compiled as function
      return vcode_unit_result() == VCODE_INVALID_TYPE;
   default:
      return false;
   }
}

void jit_irgen(jit_func_t *f)
{
   assert(f->irbuf == NULL);

   vcode_select_unit(f->unit);

   const bool debug_log = opt_get_int(OPT_JIT_LOG) && f->name != NULL;
   const uint64_t start_ticks = debug_log ? get_timestamp_us() : 0;

   jit_irgen_t *g = xcalloc(sizeof(jit_irgen_t));
   g->func = f;
   g->map  = xmalloc_array(vcode_count_regs(), sizeof(jit_value_t));

   const vunit_kind_t kind = vcode_unit_kind();
   const bool has_privdata =
      kind == VCODE_UNIT_PACKAGE || kind == VCODE_UNIT_INSTANCE;
   const bool has_params =
      kind == VCODE_UNIT_FUNCTION || kind == VCODE_UNIT_PROCEDURE
      || kind == VCODE_UNIT_THUNK;
   const bool has_jump_table =
      kind == VCODE_UNIT_PROCESS || kind == VCODE_UNIT_PROCEDURE;

   if (has_privdata) {
      // It's harmless to initialise a package multiple times, just
      // return the existing context pointer
      irgen_label_t *cont = irgen_alloc_label(g);
      jit_value_t priv = macro_getpriv(g, g->func->handle);
      j_cmp(g, JIT_CC_EQ, priv, jit_value_from_int64(0));
      j_jump(g, JIT_CC_T, cont);
      j_send(g, 0, priv);
      j_ret(g);
      irgen_bind_label(g, cont);
   }

   const int first_param = irgen_is_procedure() || has_jump_table ? 1 : 0;
   if (has_params)
      irgen_params(g, first_param);
   else if (kind == VCODE_UNIT_PROCESS)
      g->func->spec = (FFI_POINTER << 8) | (FFI_POINTER << 4);

   const int nblocks = vcode_count_blocks();
   g->blocks = xmalloc_array(nblocks, sizeof(irgen_label_t *));

   for (int i = 0; i < nblocks; i++)
      g->blocks[i] = irgen_alloc_label(g);

   if (has_jump_table)
      irgen_jump_table(g);

   if (kind == VCODE_UNIT_PROCESS) {
      // Schedule the process to run immediately
      j_send(g, 0, jit_value_from_int64(0));
      macro_exit(g, JIT_EXIT_SCHED_PROCESS);
   }

   irgen_locals(g);

   if (g->statereg.kind != JIT_VALUE_INVALID) {
      // Stash context pointer
      jit_value_t context = has_params ? g->map[0] : j_recv(g, first_param);
      j_store(g, JIT_SZ_PTR, context, jit_addr_from_value(g->statereg, 0));
   }

   if (has_privdata)
      macro_putpriv(g, g->func->handle, g->statereg);

   for (int i = 0; i < nblocks; i++)
      irgen_block(g, i);

   f->nregs   = g->next_reg;
   f->cpoolsz = g->cpoolptr;

   for (irgen_label_t *it = g->labels, *tmp; it; it = tmp) {
      assert(it->label < g->func->nirs);
      g->func->irbuf[it->label].target = 1;
      tmp = it->next;
      free(it);
   }
   g->labels = NULL;

   if (opt_get_verbose(OPT_JIT_VERBOSE, istr(f->name))) {
#ifdef DEBUG
      jit_dump_interleaved(f);
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
   free(g);
}
