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
#include "jit/jit-priv.h"

#include <assert.h>
#include <stdlib.h>
#include <string.h>

////////////////////////////////////////////////////////////////////////////////
// Control flow graph construction

static bool cfg_is_terminator(jit_op_t op)
{
   return op == J_JUMP || op == J_RET;
}

static bool cfg_will_abort(jit_ir_t *ir)
{
   if (ir->op == MACRO_EXIT) {
      return ir->arg1.exit == JIT_EXIT_INDEX_FAIL
         || ir->arg1.exit == JIT_EXIT_OVERFLOW
         || ir->arg1.exit == JIT_EXIT_NULL_DEREF
         || ir->arg1.exit == JIT_EXIT_UNREACHABLE
         || ir->arg1.exit == JIT_EXIT_LENGTH_FAIL
         || ir->arg1.exit == JIT_EXIT_DIV_ZERO
         || ir->arg1.exit == JIT_EXIT_EXPONENT_FAIL
         || ir->arg1.exit == JIT_EXIT_RANGE_FAIL
         || ir->arg1.exit == JIT_EXIT_ELAB_ORDER_FAIL;
   }
   else
      return ir->op == J_TRAP;
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

static void cfg_liveness(jit_cfg_t *cfg, jit_func_t *f)
{
   // Algorithm from "Engineering a Compiler" chapter 8.6

   for (int i = 0; i < cfg->nblocks; i++) {
      jit_block_t *b = &(cfg->blocks[i]);
      mask_init(&b->livein, f->nregs);
      mask_init(&b->varkill, f->nregs);
      mask_init(&b->liveout, f->nregs);

      for (int j = b->first; j <= b->last; j++) {
         jit_ir_t *ir = &(f->irbuf[j]);

         jit_reg_t reg1 = cfg_get_reg(ir->arg1);
         if (reg1 != JIT_REG_INVALID && !mask_test(&b->varkill, reg1))
            mask_set(&b->livein, reg1);

         jit_reg_t reg2 = cfg_get_reg(ir->arg2);
         if (reg2 != JIT_REG_INVALID && !mask_test(&b->varkill, reg2))
            mask_set(&b->livein, reg2);

         if (ir->result != JIT_REG_INVALID)
            mask_set(&b->varkill, ir->result);
      }
   }

   bit_mask_t new, tmp;
   mask_init(&new, f->nregs);
   mask_init(&tmp, f->nregs);

   bool changed;
   do {
      changed = false;

      for (int i = 0; i < cfg->nblocks; i++) {
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
   if (f->cfg != NULL)
      return f->cfg;

   int nb = 1;
   for (int i = 0, first = 0; i < f->nirs; i++) {
      jit_ir_t *ir = &(f->irbuf[i]);
      if (ir->target && i > 0 && first != i)
         first = i, nb++;
      if (cfg_is_terminator(ir->op) && i + 1 < f->nirs)
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
      else if (cfg_will_abort(ir))
         bb->aborts = 1;

      if (cfg_is_terminator(ir->op) && i + 1 < f->nirs) {
         if (ir->op == J_JUMP && ir->cc != JIT_CC_NONE)
            cfg_add_edge(cfg, bb, bb + 1);   // Fall-through case
         (++bb)->first = i + 1;
      }
   }

   for (int i = 0; i < f->nirs; i++) {
      jit_ir_t *ir = &(f->irbuf[i]);
      if (ir->op == J_JUMP) {
         jit_label_t label = ir->arg1.label;
         assert(label < f->nirs);
         jit_block_t *from = jit_block_for(cfg, i);
         jit_block_t *to = jit_block_for(cfg, label);
         cfg_add_edge(cfg, from, to);
      }
   }

   cfg_liveness(cfg, f);

   return (f->cfg = cfg);
}

void jit_free_cfg(jit_func_t *f)
{
   if (f->cfg != NULL) {
      for (int i = 0; i < f->cfg->nblocks; i++) {
         jit_block_t *b = &(f->cfg->blocks[i]);
         mask_free(&b->livein);
         mask_free(&b->liveout);
         mask_free(&b->varkill);
      }

      free(f->cfg);
      f->cfg = NULL;
   }
}

jit_block_t *jit_block_for(jit_cfg_t *cfg, int pos)
{
   // TODO: do binary search
   for (int i = 0; i < cfg->nblocks; i++) {
      jit_block_t *bb = &(cfg->blocks[i]);
      if (pos >= bb->first && pos <= bb->last)
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

#define FOR_ALL_SIZES(size, macro) do {                 \
      switch (size) {                                   \
      case JIT_SZ_8: macro(int8_t); break;              \
      case JIT_SZ_16: macro(int16_t); break;            \
      case JIT_SZ_32: macro(int32_t); break;            \
      case JIT_SZ_UNSPEC:                               \
      case JIT_SZ_64: macro(int64_t); break;            \
      }                                                 \
   } while (0)

static inline bool lvn_can_fold(jit_ir_t *ir)
{
   return ir->arg1.kind == JIT_VALUE_INT64
      && (ir->arg2.kind == JIT_VALUE_INVALID
          || ir->arg2.kind == JIT_VALUE_INT64);
}

static void lvn_convert_mov(jit_ir_t *ir, int64_t cval)
{
   ir->op         = J_MOV;
   ir->size       = JIT_SZ_UNSPEC;
   ir->arg1.kind  = JIT_VALUE_INT64;
   ir->arg1.int64 = cval;
   ir->arg2.kind  = JIT_VALUE_INVALID;
}

static void jit_lvn_mul(jit_ir_t *ir)
{
   if (lvn_can_fold(ir)) {
      const int64_t lhs = ir->arg1.int64;
      const int64_t rhs = ir->arg2.int64;

#define FOLD_MUL(type) do {                                             \
         type result;                                                   \
         if (!__builtin_mul_overflow(lhs, rhs, &result)) {              \
            lvn_convert_mov(ir, result);                                \
            return;                                                     \
         }                                                              \
      } while (0)

      FOR_ALL_SIZES(ir->size, FOLD_MUL);
#undef FOLD_MUL
   }
}

static void jit_lvn_add(jit_ir_t *ir)
{
   if (lvn_can_fold(ir)) {
      const int64_t lhs = ir->arg1.int64;
      const int64_t rhs = ir->arg2.int64;

#define FOLD_ADD(type) do {                                             \
         type result;                                                   \
         if (!__builtin_add_overflow(lhs, rhs, &result)) {              \
            lvn_convert_mov(ir, result);                                \
            return;                                                     \
         }                                                              \
      } while (0)

      FOR_ALL_SIZES(ir->size, FOLD_ADD);
#undef FOLD_ADD
   }
}

static void jit_lvn_sub(jit_ir_t *ir)
{
   if (lvn_can_fold(ir)) {
      const int64_t lhs = ir->arg1.int64;
      const int64_t rhs = ir->arg2.int64;

#define FOLD_SUB(type) do {                                             \
         type result;                                                   \
         if (!__builtin_sub_overflow(lhs, rhs, &result)) {              \
            lvn_convert_mov(ir, result);                                \
            return;                                                     \
         }                                                              \
      } while (0)

      FOR_ALL_SIZES(ir->size, FOLD_SUB);
#undef FOLD_SUB
   }
}

void jit_do_lvn(jit_func_t *f)
{
   for (int i = 0; i < f->nirs; i++) {
      jit_ir_t *ir = &(f->irbuf[i]);
      switch (ir->op) {
      case J_MUL: jit_lvn_mul(ir); break;
      case J_ADD: jit_lvn_add(ir); break;
      case J_SUB: jit_lvn_sub(ir); break;
      default: break;
      }
   }
}
