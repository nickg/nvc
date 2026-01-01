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
#include "array.h"
#include "jit/jit-priv.h"

#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>

////////////////////////////////////////////////////////////////////////////////
// Control flow graph construction

static bool cfg_is_terminator(jit_func_t *func, jit_ir_t *ir)
{
   if (ir->op == MACRO_CASE)
      return ir + 1 < func->irbuf + func->nirs && (ir + 1)->op != MACRO_CASE;
   else
      return ir->op == J_JUMP || ir->op == J_RET || ir->op == MACRO_REEXEC;
}

static void cfg_add_one_edge(jit_edge_list_t *list, unsigned edge)
{
   if (list->count < 4)
      list->u.edges[list->count++] = edge;
   else if (list->count == 4) {
      unsigned *ptr = xmalloc_array(16, sizeof(unsigned));
      memcpy(ptr, list->u.edges, 4 * sizeof(unsigned));

      list->max = 16;
      list->u.external = ptr;
      list->u.external[list->count++] = edge;
   }
   else if (list->count == list->max) {
      list->max *= 2;
      list->u.external =
         xrealloc_array(list->u.external, list->max, sizeof(unsigned));
      list->u.external[list->count++] = edge;
   }
   else
      list->u.external[list->count++] = edge;
}

static void cfg_add_edge(jit_cfg_t *cfg, jit_block_t *from, jit_block_t *to)
{
   cfg_add_one_edge(&(from->out), to - cfg->blocks);
   cfg_add_one_edge(&(to->in), from - cfg->blocks);
}

static jit_reg_t cfg_get_reg(jit_value_t value)
{
   switch (value.kind) {
   case JIT_VALUE_REG:
   case JIT_ADDR_REG:
      return value.reg;
   default:
      return JIT_REG_INVALID;
   }
}

static inline bool cfg_reads_result(jit_ir_t *ir)
{
   return ir->op == MACRO_COPY || ir->op == MACRO_CASE || ir->op == MACRO_BZERO
      || ir->op == MACRO_MOVE || ir->op == MACRO_MEMSET;
}

static inline bool cfg_writes_result(jit_ir_t *ir)
{
   return ir->result != JIT_REG_INVALID && ir->op != MACRO_CASE;
}

static void cfg_liveness(jit_cfg_t *cfg, jit_func_t *f)
{
   // Algorithm from "Engineering a Compiler" chapter 8.6

   for (int i = 0; i < cfg->nblocks; i++) {
      jit_block_t *b = &(cfg->blocks[i]);
      mask_init(&b->livein, f->nregs + 1);
      mask_init(&b->varkill, f->nregs + 1);
      mask_init(&b->liveout, f->nregs + 1);

      for (int j = b->first; j <= b->last; j++) {
         jit_ir_t *ir = &(f->irbuf[j]);

         jit_reg_t reg1 = cfg_get_reg(ir->arg1);
         if (reg1 != JIT_REG_INVALID && !mask_test(&b->varkill, reg1))
            mask_set(&b->livein, reg1);

         jit_reg_t reg2 = cfg_get_reg(ir->arg2);
         if (reg2 != JIT_REG_INVALID && !mask_test(&b->varkill, reg2))
            mask_set(&b->livein, reg2);

         if (cfg_reads_result(ir) && !mask_test(&b->varkill, ir->result))
            mask_set(&b->livein, ir->result);

         if (cfg_writes_result(ir))
            mask_set(&b->varkill, ir->result);

         if (jit_writes_flags(ir))
            mask_set(&b->varkill, f->nregs);
         else if (jit_reads_flags(ir) && !mask_test(&b->varkill, f->nregs))
            mask_set(&b->livein, f->nregs);
      }
   }

   bit_mask_t new, tmp;
   mask_init(&new, f->nregs + 1);
   mask_init(&tmp, f->nregs + 1);

   bool changed;
   do {
      changed = false;

      for (int i = cfg->nblocks - 1; i >= 0; i--) {
         jit_block_t *b = &(cfg->blocks[i]);
         mask_clearall(&new);

         for (int j = 0; j < b->out.count; j++) {
            jit_block_t *succ = &(cfg->blocks[jit_get_edge(&b->out, j)]);
            mask_copy(&tmp, &succ->liveout);
            mask_subtract(&tmp, &succ->varkill);
            mask_union(&tmp, &succ->livein);
            mask_union(&new, &tmp);
         }

         if (!mask_eq(&new, &b->liveout)) {
            mask_copy(&b->liveout, &new);
            changed = true;
         }
      }
   } while (changed);

   // Replaced "upward exposed variables" set with live-in
   for (int i = 0; i < cfg->nblocks; i++) {
      jit_block_t *b = &(cfg->blocks[i]);
      mask_copy(&tmp, &b->liveout);
      mask_subtract(&tmp, &b->varkill);
      mask_union(&b->livein, &tmp);
   }

   mask_free(&new);
   mask_free(&tmp);
}

jit_cfg_t *jit_get_cfg(jit_func_t *f)
{
   int nb = 1;
   for (int i = 0, first = 0; i < f->nirs; i++) {
      jit_ir_t *ir = &(f->irbuf[i]);
      if (ir->target && i > 0 && first != i)
         first = i, nb++;
      if (cfg_is_terminator(f, ir) && i + 1 < f->nirs)
         first = i + 1, nb++;
   }

   jit_cfg_t *cfg = xcalloc_flex(sizeof(jit_cfg_t), nb, sizeof(jit_block_t));
   cfg->nblocks = nb;

   jit_block_t *bb = cfg->blocks;
   for (int i = 0; i < f->nirs; i++) {
      jit_ir_t *ir = &(f->irbuf[i]);
      if (ir->target && i > 0 && bb->first != i) {
         if (!bb->returns && !bb->aborts)
            cfg_add_edge(cfg, bb, bb + 1);
         (++bb)->first = i;
      }

      bb->last = i;

      if (ir->op == J_RET)
         bb->returns = 1;
      else if (jit_will_abort(ir))
         bb->aborts = 1;

      if (cfg_is_terminator(f, ir) && i + 1 < f->nirs) {
         if ((ir->op == J_JUMP && ir->cc != JIT_CC_NONE)
             || ir->op == MACRO_CASE)
            cfg_add_edge(cfg, bb, bb + 1);   // Fall-through case
         (++bb)->first = i + 1;
      }
   }

   for (int i = 0; i < f->nirs; i++) {
      jit_ir_t *ir = &(f->irbuf[i]);
      jit_label_t label = JIT_LABEL_INVALID;

      if (ir->op == J_JUMP)
         label = ir->arg1.label;
      else if (ir->op == MACRO_CASE)
         label = ir->arg2.label;

      if (label != JIT_LABEL_INVALID) {
         assert(label < f->nirs);
         jit_block_t *from = jit_block_for(cfg, i);
         jit_block_t *to = jit_block_for(cfg, label);
         cfg_add_edge(cfg, from, to);
      }
   }

   cfg_liveness(cfg, f);

   return cfg;
}

void jit_free_cfg(jit_cfg_t *cfg)
{
   for (int i = 0; i < cfg->nblocks; i++) {
      jit_block_t *b = &(cfg->blocks[i]);
      mask_free(&b->livein);
      mask_free(&b->liveout);
      mask_free(&b->varkill);

      if (b->in.max > 4) free(b->in.u.external);
      if (b->out.max > 4) free(b->out.u.external);
   }

   free(cfg);
}

jit_block_t *jit_block_for(jit_cfg_t *cfg, int pos)
{
   for (int low = 0, high = cfg->nblocks - 1; low <= high; ) {
      const int mid = (low + high) / 2;
      jit_block_t *bb = &(cfg->blocks[mid]);

      if (bb->last < pos)
         low = mid + 1;
      else if (bb->first > pos)
         high = mid - 1;
      else
         return bb;
   }

   fatal_trace("operation %d is not in any block", pos);
}

int jit_get_edge(jit_edge_list_t *list, int nth)
{
   assert(nth < list->count);
   if (list->max <= 4)
      return list->u.edges[nth];
   else
      return list->u.external[nth];
}

////////////////////////////////////////////////////////////////////////////////
// Local value numbering and simple peepholes

#define FOR_ALL_JIT_SIZES(size, macro) do {             \
      switch (size) {                                   \
      case JIT_SZ_8: macro(int8_t); break;              \
      case JIT_SZ_16: macro(int16_t); break;            \
      case JIT_SZ_32: macro(int32_t); break;            \
      case JIT_SZ_UNSPEC:                               \
      case JIT_SZ_64: macro(int64_t); break;            \
      }                                                 \
   } while (0)

typedef unsigned valnum_t;

#define VN_INVALID  UINT_MAX
#define SMALL_CONST 100
#define MAX_CONSTS  32
#define FIRST_VN    (SMALL_CONST + MAX_CONSTS)
#define TRACK_ARGS  8

#define LVN_REG(r) ((jit_value_t){ .kind = JIT_VALUE_REG, .reg = (r) })
#define LVN_CONST(i) ((jit_value_t){ .kind = JIT_VALUE_INT64, .int64 = (i) })

typedef struct {
   jit_ir_t *ir;
   valnum_t  vn;
   int       tuple[3];
} lvn_tab_t;

typedef struct {
   jit_func_t *func;
   valnum_t   *regvn;
   valnum_t    nextvn;
   lvn_tab_t  *hashtab;
   size_t      tabsz;
   int64_t     consttab[MAX_CONSTS];
   unsigned    nconsts;
} lvn_state_t;

static void jit_lvn_mov(jit_ir_t *ir, lvn_state_t *state);

static inline valnum_t lvn_new_value(lvn_state_t *state)
{
   return state->nextvn++;
}

static bool lvn_get_const(valnum_t vn, lvn_state_t *state, int64_t *cval)
{
   if (vn < SMALL_CONST) {
      *cval = vn;
      return true;
   }
   else if (vn < SMALL_CONST + MAX_CONSTS) {
      *cval = state->consttab[vn - SMALL_CONST];
      return true;
   }
   else
      return false;
}

static bool lvn_is_const(jit_value_t value, lvn_state_t *state, int64_t *cval)
{
   switch (value.kind) {
   case JIT_VALUE_INT64:
      *cval = value.int64;
      return true;
   case JIT_VALUE_REG:
      return lvn_get_const(state->regvn[value.reg], state, cval);
   default:
      return false;
   }
}

static inline bool lvn_can_fold(jit_ir_t *ir, lvn_state_t *state,
                                int64_t *arg1, int64_t *arg2)
{
   return lvn_is_const(ir->arg1, state, arg1)
      && lvn_is_const(ir->arg2, state, arg2);
}

static void lvn_convert_mov(jit_ir_t *ir, lvn_state_t *state, jit_value_t value)
{
   ir->op        = J_MOV;
   ir->size      = JIT_SZ_UNSPEC;
   ir->cc        = JIT_CC_NONE;
   ir->arg1      = value;
   ir->arg2.kind = JIT_VALUE_INVALID;

   jit_lvn_mov(ir, state);
}

static void lvn_convert_nop(jit_ir_t *ir)
{
   ir->op        = J_NOP;
   ir->size      = JIT_SZ_UNSPEC;
   ir->cc        = JIT_CC_NONE;
   ir->result    = JIT_REG_INVALID;
   ir->arg1.kind = JIT_VALUE_INVALID;
   ir->arg2.kind = JIT_VALUE_INVALID;
}

static valnum_t lvn_value_num(jit_value_t value, lvn_state_t *state)
{
   switch (value.kind) {
   case JIT_VALUE_REG:
      if (state->regvn[value.reg] != VN_INVALID)
         return state->regvn[value.reg];
      else
         return (state->regvn[value.reg] = lvn_new_value(state));

   case JIT_VALUE_INT64:
      if (value.int64 >= 0 && value.int64 < SMALL_CONST)
         return value.int64;
      else {
         for (int i = 0; i < state->nconsts; i++) {
            if (state->consttab[i] == value.int64)
               return SMALL_CONST + i;
         }

         if (state->nconsts < MAX_CONSTS) {
            state->consttab[state->nconsts] = value.int64;
            return SMALL_CONST + state->nconsts++;
         }
         else
            return lvn_new_value(state);
      }

   case JIT_VALUE_INVALID:
      return VN_INVALID;

   case JIT_VALUE_HANDLE:
   case JIT_VALUE_DOUBLE:
      return lvn_new_value(state);

   default:
      fatal_trace("cannot handle value kind %d in lvn_value_num", value.kind);
   }
}

static inline bool lvn_is_commutative(jit_op_t op)
{
   return op == J_ADD || op == J_MUL || op == J_AND || op == J_OR;
}

static inline void lvn_kill_flags(lvn_state_t *state)
{
   state->regvn[state->func->nregs] = VN_INVALID;
}

static void lvn_kill_args(lvn_state_t *state)
{
   for (int i = 0; i < TRACK_ARGS; i++)
      state->regvn[state->func->nregs + i + 1] = VN_INVALID;
}

static void lvn_commute_const(jit_ir_t *ir, lvn_state_t *state)
{
   assert(lvn_is_commutative(ir->op));

   int64_t cval;
   if (lvn_is_const(ir->arg1, state, &cval)) {
      jit_value_t tmp = ir->arg1;
      ir->arg1 = ir->arg2;
      ir->arg2 = tmp;
   }
}

static void lvn_get_tuple(jit_ir_t *ir, lvn_state_t *state, int tuple[3])
{
   tuple[0] = ir->op | ir->size << 8 | ir->cc << 11;

   const valnum_t vn1 = lvn_value_num(ir->arg1, state);
   const valnum_t vn2 = lvn_value_num(ir->arg2, state);

   if (lvn_is_commutative(ir->op) && vn1 > vn2) {
      tuple[1] = vn2;
      tuple[2] = vn1;
   }
   else {
      tuple[1] = vn1;
      tuple[2] = vn2;
   }
}

static void jit_lvn_generic(jit_ir_t *ir, lvn_state_t *state, valnum_t vn)
{
   assert(ir->result != JIT_REG_INVALID);

   int tuple[3];
   lvn_get_tuple(ir, state, tuple);

   const uint32_t hash =
      mix_bits_32(tuple[0]*29 + tuple[1]*1093 + tuple[2]*6037);

   for (int idx = hash & (state->tabsz - 1), limit = 0, stale = -1; limit < 10;
        idx = (idx + 1) & (state->tabsz - 1), limit++) {

      lvn_tab_t *tab = &(state->hashtab[idx]);
      if (tab->ir == NULL) {
         if (stale >= 0)  // Reuse earlier stale entry if possible
            tab = &(state->hashtab[stale]);
         tab->ir = ir;
         tab->vn = state->regvn[ir->result] =
            (vn == VN_INVALID ? lvn_new_value(state) : vn);
         memcpy(tab->tuple, tuple, sizeof(tuple));
         return;
      }
      else if (tab->vn != state->regvn[tab->ir->result]) {
         // Stale entry may be reused if we do not find matching value
         stale = idx;
         continue;
      }
      else if (memcmp(tuple, tab->tuple, sizeof(tuple)) == 0) {
         assert(tab->ir->result != JIT_REG_INVALID);

         ir->op   = J_MOV;
         ir->size = JIT_SZ_UNSPEC;
         ir->cc   = JIT_CC_NONE;

         // Propagate constants where possible
         int64_t cval;
         if (lvn_get_const(state->regvn[tab->ir->result], state, &cval))
            ir->arg1 = LVN_CONST(cval);
         else
            ir->arg1 = LVN_REG(tab->ir->result);

         ir->arg2.kind = JIT_VALUE_INVALID;

         state->regvn[ir->result] = tab->vn;
         return;
      }
   }

   state->regvn[ir->result] = VN_INVALID;   // Reached iteration limit
}

static void jit_lvn_mul(jit_ir_t *ir, lvn_state_t *state)
{
   if (ir->cc != JIT_CC_NONE)
      lvn_kill_flags(state);

   int64_t lhs, rhs;
   if (lvn_can_fold(ir, state, &lhs, &rhs)) {
#define FOLD_MUL(type) do {                                             \
         type result;                                                   \
         if (!__builtin_mul_overflow(lhs, rhs, &result)) {              \
            lvn_convert_mov(ir, state, LVN_CONST(result));              \
            return;                                                     \
         }                                                              \
      } while (0)

      FOR_ALL_JIT_SIZES(ir->size, FOLD_MUL);
#undef FOLD_MUL
   }

   lvn_commute_const(ir, state);

   if (lvn_is_const(ir->arg2, state, &rhs)) {
      if (rhs == 0) {
         lvn_convert_mov(ir, state, LVN_CONST(0));
         return;
      }
      else if (rhs == 1) {
         lvn_convert_mov(ir, state, LVN_REG(ir->arg1.reg));
         return;
      }
      else if (rhs > 0 && is_power_of_2(rhs) && ir->size == JIT_SZ_UNSPEC) {
         ir->op = J_SHL;
         ir->arg2 = LVN_CONST(ilog2(rhs));
         jit_lvn_generic(ir, state, VN_INVALID);
         return;
      }
   }

   jit_lvn_generic(ir, state, VN_INVALID);
}

static void jit_lvn_div(jit_ir_t *ir, lvn_state_t *state)
{
   if (ir->cc != JIT_CC_NONE)
      lvn_kill_flags(state);

   int64_t lhs, rhs;
   if (lvn_can_fold(ir, state, &lhs, &rhs) && rhs != 0) {
      // XXX: potential bug here with INT_MIN/-1
#define FOLD_DIV(type) do {                                             \
         type result = (type)lhs / (type)rhs;                           \
         lvn_convert_mov(ir, state, LVN_CONST(result));                 \
         return;                                                        \
      } while (0)

      FOR_ALL_JIT_SIZES(ir->size, FOLD_DIV);
#undef FOLD_DIV
   }
   else if (lvn_is_const(ir->arg2, state, &rhs) && rhs == 1) {
      lvn_convert_mov(ir, state, LVN_REG(ir->arg1.reg));
      return;
   }

   jit_lvn_generic(ir, state, VN_INVALID);
}

static void jit_lvn_add(jit_ir_t *ir, lvn_state_t *state)
{
   if (ir->cc != JIT_CC_NONE)
      lvn_kill_flags(state);

   int64_t lhs, rhs;
   if (lvn_can_fold(ir, state, &lhs, &rhs)) {
#define FOLD_ADD(type) do {                                             \
         type result;                                                   \
         if (!__builtin_add_overflow(lhs, rhs, &result)) {              \
            lvn_convert_mov(ir, state, LVN_CONST(result));              \
            return;                                                     \
         }                                                              \
      } while (0)

      FOR_ALL_JIT_SIZES(ir->size, FOLD_ADD);
#undef FOLD_ADD
   }

   lvn_commute_const(ir, state);

   if (lvn_is_const(ir->arg2, state, &rhs) && rhs == 0) {
      lvn_convert_mov(ir, state, LVN_REG(ir->arg1.reg));
      return;
   }

   jit_lvn_generic(ir, state, VN_INVALID);
}

static void jit_lvn_sub(jit_ir_t *ir, lvn_state_t *state)
{
   if (ir->cc != JIT_CC_NONE)
      lvn_kill_flags(state);

   int64_t lhs, rhs;
   if (lvn_can_fold(ir, state, &lhs, &rhs)) {
#define FOLD_SUB(type) do {                                             \
         type result;                                                   \
         if (!__builtin_sub_overflow(lhs, rhs, &result)) {              \
            lvn_convert_mov(ir, state, LVN_CONST(result));              \
            return;                                                     \
         }                                                              \
      } while (0)

      FOR_ALL_JIT_SIZES(ir->size, FOLD_SUB);
#undef FOLD_SUB
   }

   if (lvn_is_const(ir->arg2, state, &rhs) && rhs == 0) {
      lvn_convert_mov(ir, state, LVN_REG(ir->arg1.reg));
      return;
   }
   else if (lvn_is_const(ir->arg1, state, &lhs) && lhs == 0
            && ir->cc == JIT_CC_NONE && ir->size == JIT_SZ_UNSPEC) {
     ir->op        = J_NEG;
     ir->arg1      = ir->arg2;
     ir->arg2.kind = JIT_VALUE_INVALID;
     jit_lvn_generic(ir, state, VN_INVALID);
     return;
   }

   jit_lvn_generic(ir, state, VN_INVALID);
}

static void jit_lvn_neg(jit_ir_t *ir, lvn_state_t *state)
{
   int64_t value;
   if (lvn_is_const(ir->arg1, state, &value))
      lvn_convert_mov(ir, state, LVN_CONST(-value));
   else
      jit_lvn_generic(ir, state, VN_INVALID);
}

static void jit_lvn_or(jit_ir_t *ir, lvn_state_t *state)
{
   int64_t lhs, rhs;
   if (lvn_can_fold(ir, state, &lhs, &rhs))
      lvn_convert_mov(ir, state, LVN_CONST(lhs | rhs));
   else if (lvn_is_const(ir->arg1, state, &lhs) && lhs == 0)
      lvn_convert_mov(ir, state, ir->arg2);
   else if (lvn_is_const(ir->arg2, state, &rhs) && rhs == 0)
      lvn_convert_mov(ir, state, ir->arg1);
   else
      jit_lvn_generic(ir, state, VN_INVALID);
}

static void jit_lvn_xor(jit_ir_t *ir, lvn_state_t *state)
{
   int64_t lhs, rhs;
   if (lvn_can_fold(ir, state, &lhs, &rhs))
      lvn_convert_mov(ir, state, LVN_CONST(lhs ^ rhs));
   else if (lvn_is_const(ir->arg1, state, &lhs) && lhs == 0)
      lvn_convert_mov(ir, state, ir->arg2);
   else if (lvn_is_const(ir->arg2, state, &rhs) && rhs == 0)
      lvn_convert_mov(ir, state, ir->arg1);
   else
      jit_lvn_generic(ir, state, VN_INVALID);
}

static void jit_lvn_shl(jit_ir_t *ir, lvn_state_t *state)
{
   int64_t lhs, rhs;
   if (lvn_can_fold(ir, state, &lhs, &rhs) && rhs >= 0 && rhs < 64)
      lvn_convert_mov(ir, state, LVN_CONST(lhs << rhs));
   else if (lvn_is_const(ir->arg2, state, &rhs) && rhs == 0)
      lvn_convert_mov(ir, state, ir->arg1);
   else
      jit_lvn_generic(ir, state, VN_INVALID);
}

static void jit_lvn_shr(jit_ir_t *ir, lvn_state_t *state)
{
   int64_t lhs, rhs;
   if (lvn_can_fold(ir, state, &lhs, &rhs) && rhs >= 0 && rhs < 64)
      lvn_convert_mov(ir, state, LVN_CONST(lhs >> rhs));
   else if (lvn_is_const(ir->arg2, state, &rhs) && rhs == 0)
      lvn_convert_mov(ir, state, ir->arg1);
   else
      jit_lvn_generic(ir, state, VN_INVALID);
}

static void jit_lvn_asr(jit_ir_t *ir, lvn_state_t *state)
{
   int64_t lhs, rhs;
   if (lvn_can_fold(ir, state, &lhs, &rhs) && rhs >= 0 && rhs < 64)
      lvn_convert_mov(ir, state, LVN_CONST(lhs >> rhs));
   else if (lvn_is_const(ir->arg2, state, &rhs) && rhs == 0)
      lvn_convert_mov(ir, state, ir->arg1);
   else
      jit_lvn_generic(ir, state, VN_INVALID);
}

static void jit_lvn_mov(jit_ir_t *ir, lvn_state_t *state)
{
   if (ir->arg1.kind == JIT_VALUE_REG && ir->arg1.reg == ir->result) {
      lvn_convert_nop(ir);
      return;
   }

   valnum_t vn = lvn_value_num(ir->arg1, state);
   if (state->regvn[ir->result] == vn) {
      // Result register already contains this value
      lvn_convert_nop(ir);
      return;
   }

   jit_lvn_generic(ir, state, vn);
}

static void jit_lvn_cmp(jit_ir_t *ir, lvn_state_t *state)
{
   int64_t lhs, rhs;
   if (lvn_can_fold(ir, state, &lhs, &rhs)) {
      bool result = false;
      switch (ir->cc) {
      case JIT_CC_EQ: result = (lhs == rhs); break;
      case JIT_CC_NE: result = (lhs != rhs); break;
      case JIT_CC_LT: result = (lhs < rhs); break;
      case JIT_CC_GT: result = (lhs > rhs); break;
      case JIT_CC_LE: result = (lhs <= rhs); break;
      case JIT_CC_GE: result = (lhs >= rhs); break;
      default:
         fatal_trace("unhandled condition code in jit_lvn_cmp");
      }

      state->regvn[state->func->nregs] = result;
   }
   else
      state->regvn[state->func->nregs] = VN_INVALID;
}

static void jit_lvn_ccmp(jit_ir_t *ir, lvn_state_t *state)
{
   const int fconst = state->regvn[state->func->nregs];
   if (fconst == 0)
      lvn_convert_nop(ir);
   else if (fconst != VN_INVALID) {
      ir->op = J_CMP;
      jit_lvn_cmp(ir, state);
   }
   else
      lvn_kill_flags(state);
}

static void jit_lvn_csel(jit_ir_t *ir, lvn_state_t *state)
{
   const int fconst = state->regvn[state->func->nregs];
   if (fconst != VN_INVALID)
      lvn_convert_mov(ir, state, fconst ? ir->arg1 : ir->arg2);
   else
      state->regvn[ir->result] = VN_INVALID;
}

static void jit_lvn_cset(jit_ir_t *ir, lvn_state_t *state)
{
   const int fconst = state->regvn[state->func->nregs];
   if (fconst != VN_INVALID)
      lvn_convert_mov(ir, state, LVN_CONST(fconst));
   else
      state->regvn[ir->result] = VN_INVALID;
}

static void jit_lvn_jump(jit_ir_t *ir, lvn_state_t *state)
{
   assert(ir->arg1.label < state->func->nirs);
   jit_ir_t *dest = &(state->func->irbuf[ir->arg1.label]);

   const int fconst = state->regvn[state->func->nregs];
   if (ir->cc != JIT_CC_NONE && fconst != VN_INVALID) {
      if (fconst == (ir->cc == JIT_CC_T))
         ir->cc = JIT_CC_NONE;
      else {
         lvn_convert_nop(ir);
         return;
      }
   }

   if (dest == ir + 1)
      lvn_convert_nop(ir);
   else if (dest->op == J_JUMP && dest->cc == JIT_CC_NONE)
      ir->arg1 = dest->arg1;     // Simple jump threading
}

static void jit_lvn_clamp(jit_ir_t *ir, lvn_state_t *state)
{
   int64_t value;
   if (lvn_is_const(ir->arg1, state, &value))
      lvn_convert_mov(ir, state, LVN_CONST(value < 0 ? 0 : value));
   else
      jit_lvn_generic(ir, state, VN_INVALID);
}

static void jit_lvn_recv(jit_ir_t *ir, lvn_state_t *state)
{
   const unsigned nth = ir->arg1.int64;
   if (nth >= TRACK_ARGS)
      return;

   const unsigned vreg = state->func->nregs + 1 + nth;

   if (state->regvn[vreg] != VN_INVALID)
      state->regvn[ir->result] = state->regvn[vreg];
   else {
      valnum_t vn = lvn_new_value(state);
      state->regvn[vreg] = vn;
      state->regvn[ir->result] = vn;
   }
}

static void jit_lvn_send(jit_ir_t *ir, lvn_state_t *state)
{
   const unsigned nth = ir->arg1.int64;
   if (nth >= TRACK_ARGS)
      return;
   else if (ir->arg2.kind != JIT_VALUE_REG)
      return;

   const unsigned vreg = state->func->nregs + 1 + nth;

   valnum_t vn = lvn_value_num(ir->arg2, state);
   if (vn != VN_INVALID && state->regvn[vreg] == vn)
      lvn_convert_nop(ir);   // Already in argument slot
}

static void jit_lvn_copy(jit_ir_t *ir, lvn_state_t *state)
{
   if (ir->op == MACRO_MOVE && ir->arg2.kind == JIT_ADDR_CPOOL)
      ir->op = MACRO_COPY;   // Cannot overlap when copying from constant pool

   int64_t count;
   if (lvn_get_const(state->regvn[ir->result], state, &count)) {
      if (count == 0) {
         lvn_convert_nop(ir);
         return;
      }
      else if (count > 0 && count <= 8 && is_power_of_2(count)
               && ir->arg2.kind == JIT_ADDR_CPOOL) {
         // Replace a small copy from the constant pool with a store
         const void *src = state->func->cpool + ir->arg2.int64;
         ir->op = J_STORE;
         ir->arg2 = ir->arg1;
         ir->result = JIT_REG_INVALID;

         switch (count) {
         case 8:
            ir->size = JIT_SZ_64;
            ir->arg1 = LVN_CONST(unaligned_load(src, uint64_t));
            break;
         case 4:
            ir->size = JIT_SZ_32;
            ir->arg1 = LVN_CONST(unaligned_load(src, uint32_t));
            break;
         case 2:
            ir->size = JIT_SZ_16;
            ir->arg1 = LVN_CONST(unaligned_load(src, uint16_t));
            break;
         case 1:
            ir->size = JIT_SZ_8;
            ir->arg1 = LVN_CONST(*(uint8_t *)src);
            break;
         default:
            jit_dump_with_mark(state->func, ir - state->func->irbuf);
            fatal_trace("unhandled constant copy from constant pool");
         }

         return;
      }
   }

   // Clobbers the count register
   state->regvn[ir->result] = lvn_new_value(state);
}

static void jit_lvn_bzero(jit_ir_t *ir, lvn_state_t *state)
{
   int64_t count;
   if (lvn_get_const(state->regvn[ir->result], state, &count)) {
      if (count == 0) {
         lvn_convert_nop(ir);
         return;
      }
      else if (count > 0 && count <= 8 && is_power_of_2(count)) {
         // Convert to a store of zero
         ir->op     = J_STORE;
         ir->arg2   = ir->arg1;
         ir->arg1   = LVN_CONST(0);
         ir->result = JIT_REG_INVALID;
         ir->size   = ilog2(count);
         return;
      }
   }

   // Clobbers the count register
   state->regvn[ir->result] = lvn_new_value(state);
}

static void jit_lvn_memset(jit_ir_t *ir, lvn_state_t *state)
{
   int64_t value;
   if (lvn_is_const(ir->arg2, state, &value)) {
      static const uint64_t compress[JIT_SZ_UNSPEC] = {
         0x01, 0x0101, 0x01010101, 0x0101010101010101
      };
      static const uint64_t expand[JIT_SZ_UNSPEC] = {
         0x0101010101010101, 0x0001000100010001, 0x100000001, 1,
      };
      static const uint64_t mask[JIT_SZ_UNSPEC] = {
         0xff, 0xffff, 0xffffffff, 0xffffffffffffffff
      };

      uint64_t bits = value;
      if (value == 0) {
         ir->op = MACRO_BZERO;
         ir->size = JIT_SZ_UNSPEC;
         ir->arg2.kind = JIT_VALUE_INVALID;
         jit_lvn_bzero(ir, state);
         return;
      }
      else if (bits == (bits & 0xff) * compress[ir->size]) {
         ir->size = JIT_SZ_8;
         ir->arg2 = LVN_CONST((bits &= 0xff));
      }

      int64_t bytes;
      if (lvn_get_const(state->regvn[ir->result], state, &bytes)) {
         if (bytes == 0) {
            lvn_convert_nop(ir);
            return;
         }
         else if (bytes > 0 && bytes <= 8 && is_power_of_2(bytes)) {
            // Convert to a store
            const jit_size_t newsz = ilog2(bytes);
            ir->op     = J_STORE;
            ir->arg2   = ir->arg1;
            ir->arg1   = LVN_CONST((bits * expand[ir->size]) & mask[newsz]);
            ir->result = JIT_REG_INVALID;
            ir->size   = newsz;
            return;
         }
      }
   }

   // Clobbers the count register
   state->regvn[ir->result] = lvn_new_value(state);
}

static void jit_lvn_exp(jit_ir_t *ir, lvn_state_t *state)
{
   int64_t base, exp;
   if (ir->cc != JIT_CC_NONE) {
      lvn_kill_flags(state);
      jit_lvn_generic(ir, state, VN_INVALID);
   }
   else if (lvn_can_fold(ir, state, &base, &exp))
      lvn_convert_mov(ir, state, LVN_CONST(ipow(base, exp)));
   else if (lvn_is_const(ir->arg1, state, &base) && base == 2) {
      ir->op = J_SHL;
      ir->arg1.int64 = 1;
      jit_lvn_generic(ir, state, VN_INVALID);
   }
   else
      jit_lvn_generic(ir, state, VN_INVALID);
}

void jit_do_lvn(jit_func_t *f)
{
   lvn_state_t state = {
      .tabsz  = next_power_of_2(f->nirs),
      .func   = f,
      .nextvn = FIRST_VN
   };

   state.regvn = xmalloc_array(f->nregs + 1 + TRACK_ARGS, sizeof(valnum_t));
   state.hashtab = xcalloc_array(state.tabsz, sizeof(lvn_tab_t));

   bool reset = true;
   for (jit_ir_t *ir = f->irbuf; ir < f->irbuf + f->nirs;
        reset = cfg_is_terminator(f, ir), ir++) {

      if (reset || ir->target) {
         for (int j = 0; j < f->nregs + 1 + TRACK_ARGS; j++)
            state.regvn[j] = VN_INVALID;
      }

      switch (ir->op) {
      case J_MUL: jit_lvn_mul(ir, &state); break;
      case J_DIV: jit_lvn_div(ir, &state); break;
      case J_ADD: jit_lvn_add(ir, &state); break;
      case J_SUB: jit_lvn_sub(ir, &state); break;
      case J_NEG: jit_lvn_neg(ir, &state); break;
      case J_OR:  jit_lvn_or(ir, &state); break;
      case J_XOR: jit_lvn_xor(ir, &state); break;
      case J_SHL: jit_lvn_shl(ir, &state); break;
      case J_SHR: jit_lvn_shr(ir, &state); break;
      case J_ASR: jit_lvn_asr(ir, &state); break;
      case J_MOV: jit_lvn_mov(ir, &state); break;
      case J_CMP: jit_lvn_cmp(ir, &state); break;
      case J_CCMP: jit_lvn_ccmp(ir, &state); break;
      case J_CSEL: jit_lvn_csel(ir, &state); break;
      case J_CSET: jit_lvn_cset(ir, &state); break;
      case J_JUMP: jit_lvn_jump(ir, &state); break;
      case J_CLAMP: jit_lvn_clamp(ir, &state); break;
      case J_RECV: jit_lvn_recv(ir, &state); break;
      case J_SEND: jit_lvn_send(ir, &state); break;
      case MACRO_MOVE:
      case MACRO_COPY: jit_lvn_copy(ir, &state); break;
      case MACRO_BZERO: jit_lvn_bzero(ir, &state); break;
      case MACRO_MEMSET: jit_lvn_memset(ir, &state); break;
      case MACRO_EXP: jit_lvn_exp(ir, &state); break;
      case MACRO_EXIT:
      case MACRO_VEC4OP:
      case MACRO_VEC2OP:
      case MACRO_PACK:
      case MACRO_UNPACK:
      case J_CALL:
         lvn_kill_args(&state);
         // Fall-through
      default:
         if (jit_writes_flags(ir))
            state.regvn[f->nregs] = VN_INVALID;
         if (ir->result != JIT_REG_INVALID)
            state.regvn[ir->result] = VN_INVALID;
      }
   }

   free(state.regvn);
   free(state.hashtab);
}

////////////////////////////////////////////////////////////////////////////////
// Copy propagation

void jit_do_cprop(jit_func_t *f)
{
   jit_value_t *map LOCAL = xmalloc_array(f->nregs, sizeof(jit_value_t));

   bool reset = true;
   for (jit_ir_t *ir = f->irbuf; ir < f->irbuf + f->nirs;
        reset = cfg_is_terminator(f, ir), ir++) {

      if (reset || ir->target) {
         for (int j = 0; j < f->nregs; j++)
            map[j].kind = JIT_VALUE_INVALID;
      }

      if (ir->arg1.kind == JIT_VALUE_REG) {
         jit_value_t copy = map[ir->arg1.reg];
         if (copy.kind != JIT_VALUE_INVALID)
            ir->arg1 = copy;
      }

      if (ir->arg2.kind == JIT_VALUE_REG) {
         jit_value_t copy = map[ir->arg2.reg];
         if (copy.kind != JIT_VALUE_INVALID)
            ir->arg2 = copy;
      }

      if (ir->result == JIT_REG_INVALID)
         continue;
      else if (ir->op == MACRO_MEMSET || ir->op == MACRO_COPY
               || ir->op == MACRO_MOVE || ir->op == MACRO_BZERO)
         continue;   // Does not write to result
      else if (map[ir->result].kind != JIT_VALUE_INVALID) {
         // Invalidate any existing copies of this register
         for (int j = 0; j < f->nregs; j++) {
            if (map[j].kind == JIT_VALUE_REG && map[j].reg == ir->result)
               map[j].kind = JIT_VALUE_INVALID;
         }
      }

      if (ir->op == J_MOV) {
         map[ir->result] = ir->arg1;
         if (ir->arg1.kind == JIT_VALUE_REG)
            map[ir->arg1.reg] = ir->arg1;
      }
      else
         map[ir->result].kind = JIT_VALUE_INVALID;
   }
}

////////////////////////////////////////////////////////////////////////////////
// Dead code elimination

typedef struct {
   int       *count;
   jit_reg_t *renumber;
   jit_reg_t  next;
} dce_state_t;

static void dce_count_use(jit_value_t value, dce_state_t *state)
{
   if (value.kind == JIT_VALUE_REG || value.kind == JIT_ADDR_REG) {
      if (state->count[value.reg]++ == 0)
         state->renumber[value.reg] = state->next++;
   }
}

static void dce_renumber(jit_value_t *value, dce_state_t *state)
{
   if (value->kind == JIT_VALUE_REG || value->kind == JIT_ADDR_REG)
      value->reg = state->renumber[value->reg];
}

void jit_do_dce(jit_func_t *f)
{
   dce_state_t state = {};
   state.count = xcalloc_array(f->nregs, sizeof(int));
   state.renumber = xcalloc_array(f->nregs, sizeof(jit_reg_t));

#ifdef DEBUG
   for (int i = 0; i < f->nregs; i++)
      state.renumber[i] = JIT_REG_INVALID;
#endif

   for (jit_ir_t *ir = f->irbuf; ir < f->irbuf + f->nirs; ir++) {
      dce_count_use(ir->arg1, &state);
      dce_count_use(ir->arg2, &state);

      const bool must_keep_result =
         ir->result != JIT_REG_INVALID &&
         (cfg_reads_result(ir) || jit_writes_flags(ir));

      if (must_keep_result && state.count[ir->result]++ == 0)
         state.renumber[ir->result] = state.next++;
   }

   for (jit_ir_t *ir = f->irbuf, *cmp = NULL; ir < f->irbuf + f->nirs; ir++) {
      dce_renumber(&ir->arg1, &state);
      dce_renumber(&ir->arg2, &state);

      if (jit_reads_flags(ir) || cfg_is_terminator(f, ir))
         cmp = NULL;   // Consumed flags
      else if (jit_writes_flags(ir)) {
         if (cmp != NULL)
            lvn_convert_nop(cmp);   // Flags are never read
         if (ir->op == J_CMP || ir->op == J_FCMP)
            cmp = ir;
         else
            cmp = NULL;
      }

      if (ir->result == JIT_REG_INVALID)
         continue;
      else if (state.count[ir->result] == 0 && !jit_writes_flags(ir))
         lvn_convert_nop(ir);
      else
         ir->result = state.renumber[ir->result];
   }

   f->nregs = state.next;

   free(state.count);
   free(state.renumber);
}

////////////////////////////////////////////////////////////////////////////////
// NOP deletion

void jit_delete_nops(jit_func_t *f)
{
   jit_label_t *map LOCAL = xmalloc_array(f->nirs, sizeof(jit_label_t));

   int wptr = 0;
   for (jit_ir_t *ir = f->irbuf; ir < f->irbuf + f->nirs; ir++) {
      map[ir - f->irbuf] = wptr;

      if (ir->op != J_NOP) {
         jit_ir_t *dest = f->irbuf + wptr++;
         if (dest != ir)
            *dest = *ir;
      }
   }

   for (jit_ir_t *ir = f->irbuf; ir < f->irbuf + f->nirs; ir++) {
      if (ir->arg1.kind == JIT_VALUE_LABEL) {
         ir->arg1.label = map[ir->arg1.label];
         f->irbuf[ir->arg1.label].target = 1;
      }
      if (ir->arg2.kind == JIT_VALUE_LABEL) {
         ir->arg2.label = map[ir->arg2.label];
         f->irbuf[ir->arg2.label].target = 1;
      }
   }

   f->nirs = wptr;
}

////////////////////////////////////////////////////////////////////////////////
// Memory to register promotion

static inline jit_reg_t get_value_reg(jit_value_t value)
{
   switch (value.kind) {
   case JIT_ADDR_REG:
   case JIT_VALUE_REG:
      return value.reg;
   default:
      return JIT_REG_INVALID;
   }
}

static bool try_promote_allocation(jit_func_t *f, jit_ir_t *alloc)
{
   assert(alloc->arg2.kind == JIT_VALUE_INT64);

   if (alloc->arg2.int64 > 8 || !is_power_of_2(alloc->arg2.int64))
      return false;

   jit_size_t sz = JIT_SZ_UNSPEC;
   switch (alloc->arg2.int64) {
   case 1: sz = JIT_SZ_8; break;
   case 2: sz = JIT_SZ_16; break;
   case 4: sz = JIT_SZ_32; break;
   case 8: sz = JIT_SZ_64; break;
   }

   jit_reg_t reg = alloc->result;
   assert(reg != JIT_REG_INVALID);

   int first = -1, last = -1;
   for (int i = 0; i < f->nirs; i++) {
      jit_ir_t *ir = &(f->irbuf[i]);
      if (ir->result == reg && ir != alloc)
         return false;
      else if (ir->op == J_LOAD || ir->op == J_STORE) {
         jit_value_t addr = ir->op == J_LOAD ? ir->arg1 : ir->arg2;
         if (get_value_reg(addr) != reg)
            continue;
         else if (addr.disp != 0 || ir->size != sz)
            return false;
         else if (ir->op == J_STORE && get_value_reg(ir->arg1) == reg)
            return false;
         else if (first == -1)
            first = last = i;
         else
            last = i;
      }
      else if (get_value_reg(ir->arg1) == reg)
         return false;
      else if (get_value_reg(ir->arg2) == reg)
         return false;
   }

   if (first != -1) {
      for (jit_ir_t *ir = f->irbuf + first; ir <= f->irbuf + last; ir++) {
         if (ir->op == J_STORE && get_value_reg(ir->arg2) == reg) {
            ir->op        = J_MOV;
            ir->size      = JIT_SZ_UNSPEC;
            ir->cc        = JIT_CC_NONE;
            ir->result    = reg;
            ir->arg2.kind = JIT_VALUE_INVALID;
         }
         else if (ir->op == J_LOAD && get_value_reg(ir->arg1) == reg) {
            ir->op        = J_MOV;
            ir->size      = JIT_SZ_UNSPEC;
            ir->cc        = JIT_CC_NONE;
            ir->arg1.kind = JIT_VALUE_REG;
            ir->arg1.reg  = reg;
         }
      }
   }

   alloc->op        = J_NOP;
   alloc->size      = JIT_SZ_UNSPEC;
   alloc->cc        = JIT_CC_NONE;
   alloc->result    = JIT_REG_INVALID;
   alloc->arg1.kind = JIT_VALUE_INVALID;
   alloc->arg2.kind = JIT_VALUE_INVALID;

   return true;
}

void jit_do_mem2reg(jit_func_t *f)
{
   if (f->framesz == 0)
      return;

   int replaced = 0;
   for (int i = 0; i < f->nirs; i++) {
      jit_ir_t *ir = &(f->irbuf[i]);
      if (ir->op == MACRO_SALLOC && try_promote_allocation(f, ir))
         replaced++;
      else if (cfg_is_terminator(f, ir))
         break;
   }

   if (replaced == 0)
      return;

   int newsize = 0;
   for (int i = 0; i < f->nirs; i++) {
      jit_ir_t *ir = &(f->irbuf[i]);
      if (ir->op == MACRO_SALLOC) {
         assert(ir->arg1.kind == JIT_VALUE_INT64);
         assert(ir->arg1.int64 >= newsize);
         ir->arg1.int64 = newsize;

         assert(ir->arg2.kind == JIT_VALUE_INT64);
         newsize += ALIGN_UP(ir->arg2.int64, 8);
      }
      else if (cfg_is_terminator(f, ir))
         break;
   }

   assert(newsize <= f->framesz);
   f->framesz = newsize;
}

////////////////////////////////////////////////////////////////////////////////
// Register allocation

typedef struct {
   jit_reg_t   reg;
   unsigned    first;
   unsigned    last;
} lscan_interval_t;

static void lscan_grow_range(jit_reg_t reg, lscan_interval_t *li, int pos)
{
   assert(reg != JIT_REG_INVALID);
   li[reg].first = MIN(li[reg].first, pos);
   li[reg].last = MAX(li[reg].last, pos);
}

static void lscan_walk_cfg(jit_func_t *f, jit_cfg_t *cfg, int bi,
                           lscan_interval_t *li, bit_mask_t *visited)
{
   if (mask_test(visited, bi))
      return;

   mask_set(visited, bi);

   jit_block_t *b = &(cfg->blocks[bi]);

   for (size_t bit = -1; mask_iter(&b->livein, &bit);)
      lscan_grow_range(bit, li, b->first);

   for (int i = b->first; i <= b->last; i++) {
      jit_ir_t *ir = &(f->irbuf[i]);

      if (ir->result != JIT_REG_INVALID)
         lscan_grow_range(ir->result, li, i);

      if (ir->arg1.kind == JIT_VALUE_REG || ir->arg1.kind == JIT_ADDR_REG)
         lscan_grow_range(ir->arg1.reg, li, i);

      if (ir->arg2.kind == JIT_VALUE_REG || ir->arg2.kind == JIT_ADDR_REG)
         lscan_grow_range(ir->arg2.reg, li, i);
   }

   for (size_t bit = -1; mask_iter(&b->liveout, &bit); )
      lscan_grow_range(bit, li, b->last);

   for (int i = 0; i < b->out.count; i++) {
      const int next = jit_get_edge(&(b->out), i);
      lscan_walk_cfg(f, cfg, next, li, visited);
   }
}

static int lscan_interval_cmp(const void *a, const void *b)
{
   const lscan_interval_t *la = a;
   const lscan_interval_t *lb = b;

   if (la->first < lb->first)
      return -1;
   else if (la->first > lb->first)
      return 1;
   else
      return 0;
}

static inline void lscan_shift_active(lscan_interval_t **active, unsigned to,
                                      unsigned from, unsigned count)
{
   if (to != from && count == 1)
      active[to] = active[from];
   else if (to != from && count > 1)
      memmove(active + to, active + from, count * sizeof(lscan_interval_t *));
}

int jit_do_lscan(jit_func_t *f, phys_slot_t *slots, uint64_t badmask)
{
   //
   // Massimiliano Poletto and Vivek Sarkar
   // Linear Scan Register Allocation
   // ACM Trans. Program. Lang. Syst., Vol. 21, 5 (sep 1999), 895--913
   // https://doi.org/10.1145/330249.330250
   //

   jit_cfg_t *cfg = jit_get_cfg(f);

   lscan_interval_t *li LOCAL =
      xmalloc_array(f->nregs, sizeof(lscan_interval_t));
   for (int i = 0; i < f->nregs; i++) {
      li[i].reg = i;
      li[i].first = UINT_MAX;
      li[i].last = 0;
      slots[i] = UINT_MAX;
   }

   bit_mask_t visited;
   mask_init(&visited, cfg->nblocks);
   lscan_walk_cfg(f, cfg, 0, li, &visited);
   mask_free(&visited);

   jit_free_cfg(cfg);

   qsort(li, f->nregs, sizeof(lscan_interval_t), lscan_interval_cmp);

   const int Rint = 32 - __builtin_popcountl(badmask & 0xffffffff);
   uint32_t freeregs = ~(badmask & 0xffffffff);

   lscan_interval_t **active LOCAL =
      xmalloc_array(f->nregs, sizeof(lscan_interval_t *));
   unsigned nactive = 0, next_spill = STACK_BASE;

   for (int i = 0; i < f->nregs; i++) {
      // Expire old intervals
      int expire = 0;
      for (; expire < nactive; expire++) {
         const lscan_interval_t *next = active[expire];
         if (next->last >= li[i].first)
            break;
         else {
            assert(slots[next->reg] < STACK_BASE);
            freeregs |= (1 << slots[next->reg]);
         }
      }

      lscan_shift_active(active, 0, expire, nactive - expire);
      nactive -= expire;

      const lscan_interval_t *this = &li[i];

      bool allocated = false;
      if (nactive == Rint) {
         // Spill at interval
         const lscan_interval_t *spill = active[nactive - 1];
         if (spill->last > this->last) {
            slots[this->reg] = slots[spill->reg];
            slots[spill->reg] = next_spill++;
            nactive--;
            allocated = true;
         }
         else
            slots[this->reg] = next_spill++;
      }
      else {
         const int bit = __builtin_ffsl(freeregs) - 1;
         assert(bit >= 0);
         assert(freeregs & (1 << bit));
         freeregs &= ~(1 << bit);
         slots[this->reg] = bit;
         allocated = true;
      }

      if (allocated) {
         // Add to active list, sorted by increasing end point
         int pos = 0;
         for (; pos < nactive && active[pos]->last <= li[i].last; pos++)
            assert(active[pos] != &li[i]);
         assert(pos < f->nregs);
         if (pos < nactive)
            lscan_shift_active(active, pos + 1, pos, nactive - pos);
         active[pos] = &li[i];
         nactive++;
      }
   }

   return next_spill - STACK_BASE;
}
