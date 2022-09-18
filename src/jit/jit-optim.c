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

   return (f->cfg = cfg);
}

void jit_free_cfg(jit_func_t *f)
{
   if (f->cfg != NULL) {
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
