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
         || ir->arg1.exit == JIT_EXIT_RANGE_FAIL;
   }
   else
      return ir->op == J_TRAP;
}

static void cfg_add_edge(jit_cfg_t *cfg, jit_block_t *from, jit_block_t *to)
{
   assert(from->out.count < 4);
   assert(to->in.count < 4);

   from->out.edges[from->out.count++] = to - cfg->blocks;
   to->in.edges[to->in.count++] = from - cfg->blocks;
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
      mask_union(&b->livein, &b->liveout);
      mask_subtract(&b->livein, &b->varkill);
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
   assert(nth < 4);
   return list->edges[nth];
}
