//
//  Copyright (C) 2014  Nick Gasson
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
#include "vcode.h"

#include <assert.h>
#include <inttypes.h>

#define MAX_BLOCKS 32
#define MAX_REGS   32
#define MAX_OPS    32
#define MAX_ARGS   8

typedef struct {
   vcode_op_t  kind;
   vcode_reg_t args[MAX_ARGS];
   int         nargs;
   vcode_reg_t result;
   union {
      vcode_cmp_t   cmp;
      ident_t       func;
      vcode_block_t target;
      int64_t       value;
   };
} op_t;

typedef struct {
   op_t ops[MAX_OPS];
   int  nops;
} block_t;

typedef struct {

} reg_t;

struct vcode_unit {
   ident_t  name;
   block_t  blocks[MAX_BLOCKS];
   int      nblocks;
   reg_t    regs[MAX_REGS];
   int      nregs;
};

static vcode_unit_t  active_unit = NULL;
static vcode_block_t active_block = VCODE_INVALID_BLOCK;

static vcode_reg_t vcode_get_reg(void)
{
   assert(active_unit != NULL);
   assert(active_unit->nregs < MAX_REGS);

   return active_unit->nregs++;
}

static op_t *vcode_add_op(vcode_op_t kind)
{
   assert(active_unit != NULL);
   assert(active_block != VCODE_INVALID_BLOCK);

   block_t *block = &(active_unit->blocks[active_block]);

   assert(block->nops < MAX_OPS);

   op_t *op = &(block->ops[block->nops++]);
   op->kind   = kind;
   op->result = VCODE_INVALID_REG;

   return op;
}

static void vcode_add_arg(op_t *op, vcode_reg_t arg)
{
   assert(op->nargs < MAX_ARGS);
   op->args[op->nargs++] = arg;
}

void vcode_close(void)
{
   active_unit  = NULL;
   active_block = -1;
}

int vcode_count_blocks(void)
{
   assert(active_unit != NULL);
   return active_unit->nblocks;
}

int vcode_count_ops(void)
{
   assert(active_unit != NULL);
   assert(active_block != VCODE_INVALID_BLOCK);
   return active_unit->blocks[active_block].nops;
}

vcode_op_t vcode_get_op(int op)
{
   assert(active_unit != NULL);
   assert(active_block != VCODE_INVALID_BLOCK);

   block_t *b = &(active_unit->blocks[active_block]);
   assert(op < b->nops);
   return b->ops[op].kind;
}

ident_t vcode_get_func(int op)
{
   assert(active_unit != NULL);
   assert(active_block != VCODE_INVALID_BLOCK);

   block_t *b = &(active_unit->blocks[active_block]);
   assert(op < b->nops);
   assert(b->ops[op].kind == VCODE_OP_FCALL);
   return b->ops[op].func;
}

int64_t vcode_get_value(int op)
{
   assert(active_unit != NULL);
   assert(active_block != VCODE_INVALID_BLOCK);

   block_t *b = &(active_unit->blocks[active_block]);
   assert(op < b->nops);
   assert(b->ops[op].kind == VCODE_OP_CONST);
   return b->ops[op].value;
}

vcode_cmp_t vcode_get_cmp(int op)
{
   assert(active_unit != NULL);
   assert(active_block != VCODE_INVALID_BLOCK);

   block_t *b = &(active_unit->blocks[active_block]);
   assert(op < b->nops);
   assert(b->ops[op].kind == VCODE_OP_CMP);
   return b->ops[op].cmp;
}

vcode_block_t vcode_get_target(int op)
{
   assert(active_unit != NULL);
   assert(active_block != VCODE_INVALID_BLOCK);

   block_t *b = &(active_unit->blocks[active_block]);
   assert(op < b->nops);

   op_t *o = &(b->ops[op]);
   assert((o->kind == VCODE_OP_WAIT) || (o->kind == VCODE_OP_JUMP));
   return o->target;
}

bool vcode_block_finished(void)
{
   assert(active_unit != NULL);
   assert(active_block != VCODE_INVALID_BLOCK);

   const block_t *b = &(active_unit->blocks[active_block]);
   if (b->nops == 0)
      return false;
   else {
      vcode_op_t kind = b->ops[b->nops - 1].kind;
      return (kind == VCODE_OP_WAIT) || (kind == VCODE_OP_JUMP);
   }
}

const char *vcode_op_string(vcode_op_t op)
{
   static const char *strs[] = {
      "cmp", "fcall", "wait", "const", "assert", "jump"
   };
   if (op >= ARRAY_LEN(strs))
      return "???";
   else
      return strs[op];
}

static void vcode_dump_reg(vcode_reg_t reg)
{
   if (reg == VCODE_INVALID_REG)
      color_printf("$red$invalid$$");
   else
      color_printf("$green$r%d$$", reg);
}

void vcode_dump(void)
{
   assert(active_unit != NULL);

   const vcode_unit_t vu = active_unit;

   printf("\n");
   color_printf("Name       $cyan$%s$$\n", istr(vu->name));
   color_printf("Type       $cyan$%s$$\n", "process");
   color_printf("Blocks     %d\n", vu->nblocks);
   color_printf("Registers  %d\n", vu->nregs);

   for (int i = 0; i < vu->nblocks; i++) {
      const block_t *b = &(vu->blocks[i]);
      for (int j = 0; j < b->nops; j++) {
         if (j == 0)
            color_printf("  $yellow$%2d:$$ ", i);
         else
            printf("      ");

         const op_t *op = &(b->ops[j]);
         switch (op->kind) {
         case VCODE_OP_CMP:
            {
               vcode_dump_reg(op->result);
               printf(" := %s ", vcode_op_string(op->kind));
               vcode_dump_reg(op->args[0]);
               switch (op->cmp) {
               case VCODE_CMP_EQ: printf(" == "); break;
               }
               vcode_dump_reg(op->args[1]);
            }
            break;

         case VCODE_OP_CONST:
            {
               vcode_dump_reg(op->result);
               printf(" := %s %"PRIi64"", vcode_op_string(op->kind), op->value);
            }
            break;

         case VCODE_OP_FCALL:
            {
               vcode_dump_reg(op->result);
               printf("");
               color_printf(" := %s $magenta$%s$$", vcode_op_string(op->kind),
                            istr(op->func));
               for (int i = 0; i < op->nargs; i++) {
                  if (i > 0)
                     printf(", ");
                  vcode_dump_reg(op->args[i]);
               }
            }
            break;

         case VCODE_OP_WAIT:
            {
               color_printf("%s $yellow$%d$$", vcode_op_string(op->kind),
                            op->target);
               if (op->args[0] != VCODE_INVALID_REG) {
                  printf(" for ");
                  vcode_dump_reg(op->args[0]);
               }
            }
            break;

         case VCODE_OP_JUMP:
            {
               color_printf("%s $yellow$%d$$", vcode_op_string(op->kind),
                            op->target);
            }
            break;

         case VCODE_OP_ASSERT:
            {
               printf("%s ", vcode_op_string(op->kind));
               vcode_dump_reg(op->args[0]);
            }
            break;
         }

         printf("\n");
      }

      if (b->nops == 0)
         color_printf("  $yellow$%2d:$$ $red$Empty basic block$$\n", i);
   }


   printf("\n");
}

vcode_block_t emit_block(void)
{
   assert(active_unit != NULL);
   assert(active_unit->nblocks < MAX_BLOCKS);

   return active_unit->nblocks++;
}

void vcode_select_unit(vcode_unit_t unit)
{
   active_unit  = unit;
   active_block = VCODE_INVALID_BLOCK;
}

void vcode_select_block(vcode_block_t block)
{
   assert(active_unit != NULL);
   active_block = block;
}

vcode_unit_t emit_process(ident_t name)
{
   vcode_unit_t vu = xcalloc(sizeof(struct vcode_unit));
   vu->name = name;

   active_unit = vu;
   vcode_select_block(emit_block());

   return vu;
}

void emit_assert(vcode_reg_t value)
{
   op_t *op = vcode_add_op(VCODE_OP_ASSERT);
   vcode_add_arg(op, value);
}

vcode_reg_t emit_cmp(vcode_cmp_t cmp, vcode_reg_t lhs, vcode_reg_t rhs)
{
   op_t *op = vcode_add_op(VCODE_OP_CMP);
   vcode_add_arg(op, lhs);
   vcode_add_arg(op, rhs);
   op->cmp = cmp;
   return (op->result = vcode_get_reg());
}

vcode_reg_t emit_fcall(ident_t func, const vcode_reg_t *args, int nargs)
{
   op_t *op = vcode_add_op(VCODE_OP_FCALL);
   op->func = func;
   for (int i = 0; i < nargs; i++)
      vcode_add_arg(op, args[i]);
   return (op->result = vcode_get_reg());
}

vcode_reg_t emit_const(int64_t value)
{
   op_t *op = vcode_add_op(VCODE_OP_CONST);
   op->value = value;
   return (op->result = vcode_get_reg());
}

void emit_wait(vcode_block_t target, vcode_reg_t time)
{
   op_t *op = vcode_add_op(VCODE_OP_WAIT);
   op->target = target;
   vcode_add_arg(op, time);
}

void emit_jump(vcode_block_t target)
{
   op_t *op = vcode_add_op(VCODE_OP_JUMP);
   op->target = target;
}
