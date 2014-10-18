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
#define MAX_TYPES  16

typedef struct {
   vcode_op_t   kind;
   vcode_reg_t  args[MAX_ARGS];
   int          nargs;
   vcode_reg_t  result;
   vcode_type_t type;
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
   vcode_type_t type;
} reg_t;

typedef struct {
   vtype_kind_t kind;
   int64_t      low;
   int64_t      high;
} vtype_t;

struct vcode_unit {
   ident_t name;
   block_t blocks[MAX_BLOCKS];
   int     nblocks;
   reg_t   regs[MAX_REGS];
   int     nregs;
   vtype_t types[MAX_TYPES];
   int     ntypes;
};

static vcode_unit_t  active_unit = NULL;
static vcode_block_t active_block = VCODE_INVALID_BLOCK;

static vcode_reg_t vcode_add_reg(vcode_type_t type)
{
   assert(active_unit != NULL);
   assert(active_unit->nregs < MAX_REGS);

   reg_t *r = &(active_unit->regs[active_unit->nregs]);
   r->type = type;

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

vcode_type_t vcode_reg_type(vcode_reg_t reg)
{
   assert(active_unit != NULL);
   assert(reg < active_unit->nregs);

   return active_unit->regs[reg].type;
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

static int vcode_dump_reg(vcode_reg_t reg)
{
   if (reg == VCODE_INVALID_REG)
      return color_printf("$red$invalid$$");
   else
      return color_printf("$green$r%d$$", reg);
}

static vtype_t *vcode_type_data(vcode_type_t type)
{
   assert(active_unit != NULL);
   assert(type < active_unit->ntypes);
   return &(active_unit->types[type]);
}

static void vcode_pretty_print_int(int64_t n)
{
   if (n == INT64_MAX)
      printf("2^63 - 1");
   else if (n == INT64_MIN)
      printf("-2^63");
   else if (n == INT32_MAX)
      printf("2^31 - 1");
   else if (n == INT32_MIN)
      printf("-2^31");
   else
      printf("%"PRIx64, n);
}

static void vcode_dump_type(int col, vcode_type_t type)
{
   while (col < 40)
      col += printf(" ");

   color_printf("$cyan$// ");
   vtype_t *vt = vcode_type_data(type);
   switch (vt->kind) {
   case VCODE_TYPE_INT:
      vcode_pretty_print_int(vt->low);
      printf(" .. ");
      vcode_pretty_print_int(vt->high);
      break;
   }
   color_printf("$$ ");
}

void vcode_dump(void)
{
   assert(active_unit != NULL);

   const vcode_unit_t vu = active_unit;

   printf("\n");
   color_printf("Name       $cyan$%s$$\n", istr(vu->name));
   color_printf("Kind       $cyan$%s$$\n", "process");
   color_printf("Blocks     %d\n", vu->nblocks);
   color_printf("Registers  %d\n", vu->nregs);
   color_printf("Types      %d\n", vu->ntypes);

   for (int i = 0; i < vu->nblocks; i++) {
      const block_t *b = &(vu->blocks[i]);
      for (int j = 0; j < b->nops; j++) {
         int col = 0;
         if (j == 0)
            col += color_printf("  $yellow$%2d:$$ ", i);
         else
            col += printf("      ");

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
               col += vcode_dump_reg(op->result);
               col += printf(" := %s %"PRIi64"",
                             vcode_op_string(op->kind),
                             op->value);
               vcode_dump_type(col, op->type);
            }
            break;

         case VCODE_OP_FCALL:
            {
               col += vcode_dump_reg(op->result);
               col += color_printf(" := %s $magenta$%s$$",
                                   vcode_op_string(op->kind),
                                   istr(op->func));
               for (int i = 0; i < op->nargs; i++) {
                  if (i > 0)
                     col += printf(", ");
                  col += vcode_dump_reg(op->args[i]);
               }
               vcode_dump_type(col, op->type);
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

bool vtype_eq(vcode_type_t a, vcode_type_t b)
{
   assert(active_unit != NULL);

   if (a == b)
      return true;
   else {
      assert(a < active_unit->ntypes);
      assert(b < active_unit->ntypes);

      const vtype_t *at = &(active_unit->types[a]);
      const vtype_t *bt = &(active_unit->types[b]);

      if (at->kind != bt->kind)
         return false;
      else {
         switch (at->kind) {
         case VCODE_TYPE_INT:
            return (at->low == bt->low) && (at->high == bt->high);
         }
      }
   }
}

vcode_type_t vtype_int(int64_t low, int64_t high)
{
   assert(active_unit != NULL);
   assert(low <= high);

   for (int i = 0; i < active_unit->ntypes; i++) {
      const vtype_t *t = &(active_unit->types[i]);
      if ((t->kind == VCODE_TYPE_INT) && (t->low == low) && (t->high == high))
         return i;
   }

   assert(active_unit->ntypes < MAX_TYPES);
   vcode_type_t r = active_unit->ntypes++;

   vtype_t *n = &(active_unit->types[r]);
   n->kind = VCODE_TYPE_INT;
   n->low  = low;
   n->high = high;

   return r;
}

vcode_type_t vtype_bool(void)
{
   return vtype_int(0, 1);
}

vtype_kind_t vtype_kind(vcode_type_t type)
{
   vtype_t *vt = vcode_type_data(type);
   return vt->kind;
}

int64_t vtype_low(vcode_type_t type)
{
   vtype_t *vt = vcode_type_data(type);
   assert(vt->kind == VCODE_TYPE_INT);
   return vt->low;
}

int64_t vtype_high(vcode_type_t type)
{
   vtype_t *vt = vcode_type_data(type);
   assert(vt->kind == VCODE_TYPE_INT);
   return vt->high;
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
   if (!vtype_eq(vcode_reg_type(value), vtype_bool()))
      fatal_trace("value parameter to assert is not bool");

   op_t *op = vcode_add_op(VCODE_OP_ASSERT);
   vcode_add_arg(op, value);
}

vcode_reg_t emit_cmp(vcode_cmp_t cmp, vcode_reg_t lhs, vcode_reg_t rhs)
{
   op_t *op = vcode_add_op(VCODE_OP_CMP);
   vcode_add_arg(op, lhs);
   vcode_add_arg(op, rhs);
   op->cmp = cmp;
   return (op->result = vcode_add_reg(vtype_bool()));
}

vcode_reg_t emit_fcall(ident_t func, vcode_type_t type,
                       const vcode_reg_t *args, int nargs)
{
   op_t *op = vcode_add_op(VCODE_OP_FCALL);
   op->func = func;
   op->type = type;
   for (int i = 0; i < nargs; i++)
      vcode_add_arg(op, args[i]);
   return (op->result = vcode_add_reg(type));
}

vcode_reg_t emit_const(vcode_type_t type, int64_t value)
{
   op_t *op = vcode_add_op(VCODE_OP_CONST);
   op->value = value;
   op->type  = type;
   return (op->result = vcode_add_reg(type));
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
