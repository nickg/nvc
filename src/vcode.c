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
#include "array.h"

#include <assert.h>
#include <inttypes.h>
#include <string.h>
#include <stdlib.h>

#define MAX_BLOCKS  32
#define MAX_REGS    64
#define MAX_OPS     128
#define MAX_ARGS    16
#define MAX_TYPES   32
#define MAX_VARS    8
#define MAX_DIM     4
#define MAX_SIGNALS 16

typedef struct {
   vcode_op_t    kind;
   vcode_reg_t   args[MAX_ARGS];
   int           nargs;
   vcode_reg_t   result;
   vcode_type_t  type;
   vcode_block_t target;
   union {
      vcode_block_t  target_else;
      vcode_cmp_t    cmp;
      ident_t        func;
      int64_t        value;
      vcode_var_t    address;
      char          *comment;
      vcode_signal_t signal;
      uint32_t       index;
   };
} op_t;

typedef struct {
   op_t ops[MAX_OPS];
   int  nops;
} block_t;

typedef struct {
   vcode_type_t type;
   vcode_type_t bounds;
} reg_t;

typedef struct {
   vtype_kind_t  kind;
   union {
      struct {
         int64_t low;
         int64_t high;
      };
      struct {
         vcode_type_t dim[MAX_DIM];
         unsigned     ndim;
         vcode_type_t elem;
         vcode_type_t bounds;
      };
      vcode_type_t pointed;
      vcode_type_t base;
   };
} vtype_t;

typedef struct {
   vcode_type_t type;
   vcode_type_t bounds;
   ident_t      name;
} var_t;

typedef struct {
   vcode_type_t type;
   vcode_type_t bounds;
   ident_t      name;
   vcode_var_t  shadow;
   netid_t     *nets;
   size_t       nnets;
} signal_t;

typedef struct {
   vcode_type_t type;
   vcode_type_t bounds;
   ident_t      name;
   vcode_reg_t  reg;
} param_t;

DECLARE_AND_DEFINE_ARRAY(param);

struct vcode_unit {
   vunit_kind_t  kind;
   vcode_unit_t  context;
   ident_t       name;
   vcode_type_t  result;
   block_t       blocks[MAX_BLOCKS];
   int           nblocks;
   reg_t         regs[MAX_REGS];
   int           nregs;
   vtype_t       types[MAX_TYPES];
   int           ntypes;
   var_t         vars[MAX_VARS];
   int           nvars;
   signal_t      signals[MAX_SIGNALS];
   int           nsignals;
   param_array_t params;
};

static vcode_unit_t  active_unit = NULL;
static vcode_block_t active_block = VCODE_INVALID_BLOCK;

static inline int64_t sadd64(int64_t a, int64_t b)
{
   if (a > 0) {
      if (b > INT64_MAX - a)
         return INT64_MAX;
   }
   else if (b < INT64_MIN - a)
      return INT64_MIN;

   return a + b;
}

static inline int64_t smul64(int64_t a, int64_t b)
{
   if ((a > 0 && b > 0) || (a < 0 && b < 0)) {
      if ((b > INT32_MAX && a > INT32_MAX)
          || (b < INT32_MIN && a < INT32_MIN))
         return INT64_MAX;
   }
   else if ((b < INT32_MIN && a > INT32_MAX)
            || (b > INT32_MAX && a < INT32_MIN))
      return INT64_MIN;

   return a * b;
}

static vcode_reg_t vcode_add_reg(vcode_type_t type)
{
   assert(active_unit != NULL);
   assert(active_unit->nregs < MAX_REGS);

   reg_t *r = &(active_unit->regs[active_unit->nregs]);
   r->type   = type;
   r->bounds = type;

   return active_unit->nregs++;
}

static op_t *vcode_add_op(vcode_op_t kind)
{
   assert(active_unit != NULL);
   assert(active_block != VCODE_INVALID_BLOCK);

   if (vcode_block_finished()) {
      vcode_dump();
      fatal_trace("attempt to add to already finished block %d", active_block);
   }

   block_t *block = &(active_unit->blocks[active_block]);

   assert(block->nops < MAX_OPS);

   op_t *op = &(block->ops[block->nops++]);
   op->kind   = kind;
   op->result = VCODE_INVALID_REG;

   return op;
}

static void vcode_add_comment(const char *fmt, ...)
{
   op_t *op = vcode_add_op(VCODE_OP_COMMENT);

   va_list ap;
   va_start(ap, fmt);
   op->comment = xvasprintf(fmt, ap);
   va_end(ap);
}

static void vcode_add_arg(op_t *op, vcode_reg_t arg)
{
   assert(op->nargs < MAX_ARGS);
   op->args[op->nargs++] = arg;
}

static op_t *vcode_op_data(int op)
{
   assert(active_unit != NULL);
   assert(active_block != VCODE_INVALID_BLOCK);

   block_t *b = &(active_unit->blocks[active_block]);

   assert(op < b->nops);
   return &(b->ops[op]);
}

static reg_t *vcode_reg_data(vcode_reg_t reg)
{
   assert(active_unit != NULL);
   assert(reg < active_unit->nregs);
   assert(reg != VCODE_INVALID_REG);
   return &(active_unit->regs[reg]);
}

static vtype_t *vcode_type_data(vcode_type_t type)
{
   assert(active_unit != NULL);
   vcode_unit_t unit = active_unit;
   if (type & 0x80000000) {
      assert(unit->context || unit->kind == VCODE_UNIT_CONTEXT);
      unit = unit->context;
   }
   type &= 0x7fffffff;
   assert(type < unit->ntypes);
   return &(unit->types[type]);
}

static signal_t *vcode_signal_data(vcode_signal_t sig)
{
   assert(active_unit != NULL);
   vcode_unit_t unit = active_unit->context;
   assert(sig < unit->nsignals);
   return &(unit->signals[sig]);
}

static var_t *vcode_var_data(vcode_var_t var)
{
   assert(active_unit != NULL);
   assert(var != VCODE_INVALID_VAR);
   vcode_unit_t unit = active_unit;
   if (var & 0x80000000) {
      assert(unit->context || unit->kind == VCODE_UNIT_CONTEXT);
      unit = unit->context;
   }
   var &= 0x7fffffff;
   assert(var < unit->nvars);
   return &(unit->vars[var]);
}

int vcode_count_regs(void)
{
   assert(active_unit != NULL);
   return active_unit->nregs;
}

vcode_type_t vcode_reg_type(vcode_reg_t reg)
{
   return vcode_reg_data(reg)->type;
}

vcode_type_t vcode_reg_bounds(vcode_reg_t reg)
{
   return vcode_reg_data(reg)->bounds;
}

static bool vcode_reg_const(vcode_reg_t reg, int64_t *value)
{
   reg_t *r = vcode_reg_data(reg);

   vtype_kind_t kind = vtype_kind(r->type);
   if (kind != VCODE_TYPE_INT && kind != VCODE_TYPE_OFFSET)
      return false;

   vtype_t *bounds = vcode_type_data(r->bounds);
   if (bounds->low == bounds->high) {
      *value = bounds->low;
      return true;
   }
   else
      return false;
}

void vcode_opt(void)
{
   // Prune assignments to unused registers

   int *uses = xmalloc(active_unit->nregs * sizeof(int));

   int pruned = 0;
   do {
      memset(uses, '\0', active_unit->nregs * sizeof(int));
      pruned = 0;

      for (int i = active_unit->nblocks - 1; i >= 0; i--) {
         block_t *b = &(active_unit->blocks[i]);

         for (int j = b->nops - 1; j >= 0; j--) {
            op_t *o = &(b->ops[j]);

            switch (o->kind) {
            case VCODE_OP_CONST:
            case VCODE_OP_LOAD:
            case VCODE_OP_LOAD_INDIRECT:
            case VCODE_OP_FCALL:
            case VCODE_OP_ADD:
            case VCODE_OP_SUB:
            case VCODE_OP_MUL:
            case VCODE_OP_CMP:
            case VCODE_OP_INDEX:
            case VCODE_OP_NETS:
               if (uses[o->result] == -1) {
                  vcode_dump();
                  fatal("defintion of r%d does not dominate all uses",
                        o->result);
               }
               else if (uses[o->result] == 0) {
                  o->comment = xasprintf("Dead %s definition of r%d",
                                         vcode_op_string(o->kind), o->result);
                  o->kind  = VCODE_OP_COMMENT;
                  o->nargs = 0;
                  pruned++;
               }
               uses[o->result] = -1;
               break;

            default:
               break;
            }

            for (int k = 0; k < o->nargs; k++) {
               if (o->args[k] != VCODE_INVALID_REG)
                  uses[o->args[k]]++;
            }
         }

      }
   } while (pruned > 0);

   free(uses);
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

int vcode_count_vars(void)
{
   assert(active_unit != NULL);
   return active_unit->nvars;
}

ident_t vcode_var_name(vcode_var_t var)
{
   return vcode_var_data(var)->name;
}

vcode_type_t vcode_var_type(vcode_var_t var)
{
   return vcode_var_data(var)->type;
}

int vcode_count_signals(void)
{
   assert(active_unit != NULL);
   assert(active_unit->kind == VCODE_UNIT_CONTEXT);

   return active_unit->nsignals;
}

ident_t vcode_signal_name(vcode_signal_t sig)
{
   return vcode_signal_data(sig)->name;
}

vcode_var_t vcode_signal_shadow(vcode_signal_t sig)
{
   return vcode_signal_data(sig)->shadow;
}

size_t vcode_signal_count_nets(vcode_signal_t sig)
{
   return vcode_signal_data(sig)->nnets;
}

const netid_t *vcode_signal_nets(vcode_signal_t sig)
{
   return vcode_signal_data(sig)->nets;
}

vcode_type_t vcode_signal_type(vcode_signal_t sig)
{
   return vcode_signal_data(sig)->type;
}

vcode_type_t vcode_signal_bounds(vcode_signal_t sig)
{
   return vcode_signal_data(sig)->bounds;
}

vcode_op_t vcode_get_op(int op)
{
   return vcode_op_data(op)->kind;
 }

ident_t vcode_get_func(int op)
{
   op_t *o = vcode_op_data(op);
   assert(o->kind == VCODE_OP_FCALL);
   return o->func;
}

int64_t vcode_get_value(int op)
{
   op_t *o = vcode_op_data(op);
   assert(o->kind == VCODE_OP_CONST);
   return o->value;
}

vcode_var_t vcode_get_address(int op)
{
   op_t *o = vcode_op_data(op);
   assert(o->kind == VCODE_OP_LOAD || o->kind == VCODE_OP_STORE
          || o->kind == VCODE_OP_INDEX);
   return o->address;
}

vcode_signal_t vcode_get_signal(int op)
{
   op_t *o = vcode_op_data(op);
   assert(o->kind == VCODE_OP_NETS);
   return o->address;
}

vcode_var_t vcode_get_type(int op)
{
   op_t *o = vcode_op_data(op);
   assert(o->kind == VCODE_OP_BOUNDS);
   return o->type;
}

int vcode_count_args(int op)
{
   return vcode_op_data(op)->nargs;
}

vcode_reg_t vcode_get_arg(int op, int arg)
{
   op_t *o = vcode_op_data(op);
   assert(arg < o->nargs);
   return o->args[arg];
}

vcode_reg_t vcode_get_result(int op)
{
   op_t *o = vcode_op_data(op);
   assert(o->result != VCODE_INVALID_REG);
   return o->result;
}

vcode_cmp_t vcode_get_cmp(int op)
{
   op_t *o = vcode_op_data(op);
   assert(o->kind == VCODE_OP_CMP);
   return o->cmp;
}

uint32_t vcode_get_index(int op)
{
   op_t *o = vcode_op_data(op);
   assert(o->kind == VCODE_OP_ASSERT || o->kind == VCODE_OP_REPORT
          || o->kind == VCODE_OP_IMAGE);
   return o->index;
}

vcode_block_t vcode_get_target(int op)
{
   op_t *o = vcode_op_data(op);
   assert(o->kind == VCODE_OP_WAIT || o->kind == VCODE_OP_JUMP
      || o->kind == VCODE_OP_COND);
   return o->target;
}

vcode_block_t vcode_get_target_else(int op)
{
   op_t *o = vcode_op_data(op);
   assert(o->kind == VCODE_OP_COND);
   return o->target_else;
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
      return kind == VCODE_OP_WAIT || kind == VCODE_OP_JUMP
         || kind == VCODE_OP_COND;
   }
}

const char *vcode_op_string(vcode_op_t op)
{
   static const char *strs[] = {
      "cmp", "fcall", "wait", "const", "assert", "jump", "load", "store",
      "mul", "add", "bounds", "comment", "const array", "index", "sub",
      "cast", "load indirect", "store indirect", "return", "nets",
      "sched waveform", "cond", "report", "div", "neg", "exp", "abs", "mod",
      "rem", "image"
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

static void vcode_pretty_print_int(int64_t n)
{
   if (n == INT64_MAX)
      printf("2^63-1");
   else if (n == INT64_MIN)
      printf("-2^63");
   else if (n == INT64_MIN + 1)
      printf("-2^63");   // XXX: bug in lexer/parser
   else if (n == INT32_MAX)
      printf("2^31-1");
   else if (n == INT32_MIN)
      printf("-2^31");
   else
      printf("%"PRIi64, n);
}

static void vcode_dump_one_type(vcode_type_t type)
{
   vtype_t *vt = vcode_type_data(type);
   switch (vt->kind) {
   case VCODE_TYPE_INT:
      if (vt->low != vt->high) {
         vcode_pretty_print_int(vt->low);
         printf("..");
         vcode_pretty_print_int(vt->high);
      }
      else
         vcode_pretty_print_int(vt->low);
      break;

   case VCODE_TYPE_CARRAY:
      {
         printf("[");
         for (unsigned i = 0; i < vt->ndim; i++) {
            if (i > 0)
               printf(", ");
            vcode_dump_one_type(vt->dim[i]);
         }
         printf("] : ");
         vcode_dump_one_type(vt->elem);
         if (!vtype_eq(vt->elem, vt->bounds)) {
            printf(" => ");
            vcode_dump_one_type(vt->bounds);
         }
      }
      break;

   case VCODE_TYPE_UARRAY:
      {
         printf("[");
         for (unsigned i = 0; i < vt->ndim; i++)
            printf("%s*", i > 0 ? ", " : "");
         printf("] : ");
         vcode_dump_one_type(vt->elem);
         if (!vtype_eq(vt->elem, vt->bounds)) {
            printf(" => ");
            vcode_dump_one_type(vt->bounds);
         }
      }
      break;

   case VCODE_TYPE_POINTER:
      printf("@<");
      vcode_dump_one_type(vt->pointed);
      printf(">");
      break;

   case VCODE_TYPE_SIGNAL:
      printf("$<");
      vcode_dump_one_type(vt->base);
      printf(">");
      break;

   case VCODE_TYPE_OFFSET:
      printf("#");
      break;
   }
}

static void vcode_dump_type(int col, vcode_type_t type, vcode_type_t bounds)
{
   if (col >= 40)
      printf(" ");
   else {
      while (col < 40)
         col += printf(" ");
   }

   color_printf("$cyan$// ");
   vcode_dump_one_type(type);
   if (!vtype_eq(type, bounds)) {
      printf(" => ");
      vcode_dump_one_type(bounds);
   }
   color_printf("$$ ");
}

void vcode_dump(void)
{
   assert(active_unit != NULL);

   const vcode_unit_t vu = active_unit;

   printf("\n");
   color_printf("Name       $cyan$%s$$\n", istr(vu->name));
   color_printf("Kind       $cyan$");
   switch (vu->kind) {
   case VCODE_UNIT_PROCESS: printf("process"); break;
   case VCODE_UNIT_CONTEXT: printf("context"); break;
   case VCODE_UNIT_FUNCTION: printf("function"); break;
   }
   color_printf("$$\n");
   if (vu->kind != VCODE_UNIT_CONTEXT)
      color_printf("Context    $cyan$%s$$\n", istr(vu->context->name));
   printf("Blocks     %d\n", vu->nblocks);
   printf("Registers  %d\n", vu->nregs);
   printf("Types      %d\n", vu->ntypes);
   printf("Variables  %d\n", vu->nvars);

   for (int i = 0; i < vu->nvars; i++) {
      const var_t *v = &(vu->vars[i]);
      int col = color_printf("  $magenta$%s$$", istr(v->name));
      vcode_dump_type(col, v->type, v->bounds);
      printf("\n");
   }

   if (vu->kind == VCODE_UNIT_CONTEXT) {
      printf("Signals    %d\n", vu->nsignals);

      for (int i = 0; i < vu->nsignals; i++) {
         const signal_t *s = &(vu->signals[i]);
         int col = color_printf("  $white$%s$$", istr(s->name));
         vcode_dump_type(col, s->type, s->bounds);
         printf("\n");
         if (s->shadow != VCODE_INVALID_VAR)
            color_printf("    Shadow $magenta$%s$$\n",
                         istr(vcode_var_name(s->shadow)));
         printf("    Nets   [");
         for (size_t j = 0; j < s->nnets; j++)
            printf("%s%d", j > 0 ? "," : "", s->nets[j]);
         printf("]\n");
      }
   }
   else if (vu->kind == VCODE_UNIT_FUNCTION) {
      color_printf("Result     $cyan$");
      vcode_dump_one_type(vu->result);
      color_printf("$$\n");

      printf("Parameters %d\n", vu->params.count);

      for (size_t i = 0; i < vu->params.count; i++) {
         const param_t *p = &(vu->params.items[i]);
         int col = printf("  ");
         col += vcode_dump_reg(p->reg);
         while (col < 8)
            col += printf(" ");
         col += color_printf("$magenta$%s$$", istr(p->name));
         vcode_dump_type(col, p->type, p->bounds);
         printf("\n");
      }
   }

   printf("Begin\n");
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
               case VCODE_CMP_EQ:  printf(" == "); break;
               case VCODE_CMP_NEQ: printf(" != "); break;
               case VCODE_CMP_LT:  printf(" < "); break;
               case VCODE_CMP_GT:  printf(" > "); break;
               case VCODE_CMP_LEQ: printf(" <= "); break;
               case VCODE_CMP_GEQ: printf(" >= "); break;
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
               reg_t *r = vcode_reg_data(op->result);
               vcode_dump_type(col, r->type, r->bounds);
            }
            break;

         case VCODE_OP_FCALL:
            {
               col += vcode_dump_reg(op->result);
               col += color_printf(" := %s $magenta$%s$$ ",
                                   vcode_op_string(op->kind),
                                   istr(op->func));
               for (int i = 0; i < op->nargs; i++) {
                  if (i > 0)
                     col += printf(", ");
                  col += vcode_dump_reg(op->args[i]);
               }
               reg_t *r = vcode_reg_data(op->result);
               vcode_dump_type(col, r->type, r->bounds);
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

         case VCODE_OP_COND:
            {
               printf("%s ", vcode_op_string(op->kind));
               vcode_dump_reg(op->args[0]);
               color_printf(" then $yellow$%d$$ else $yellow$%d$$",
                            op->target, op->target_else);
            }
            break;

         case VCODE_OP_ASSERT:
            {
               printf("%s ", vcode_op_string(op->kind));
               vcode_dump_reg(op->args[0]);
               if (op->args[2] != VCODE_INVALID_REG) {
                  printf(" report ");
                  vcode_dump_reg(op->args[2]);
               }
               printf(" severity ");
               vcode_dump_reg(op->args[1]);
            }
            break;

         case VCODE_OP_REPORT:
            {
               printf("%s ", vcode_op_string(op->kind));
               vcode_dump_reg(op->args[1]);
               printf(" severity ");
               vcode_dump_reg(op->args[0]);
            }
            break;

         case VCODE_OP_LOAD:
            {
               col += vcode_dump_reg(op->result);
               col += color_printf(" := %s $magenta$%s$$",
                                   vcode_op_string(op->kind),
                                   istr(vcode_var_data(op->address)->name));
               reg_t *r = vcode_reg_data(op->result);
               vcode_dump_type(col, r->type, r->bounds);
            }
            break;

         case VCODE_OP_LOAD_INDIRECT:
            {
               col += vcode_dump_reg(op->result);
               col += color_printf(" := %s ", vcode_op_string(op->kind));
               col += vcode_dump_reg(op->args[0]);
               reg_t *r = vcode_reg_data(op->result);
               vcode_dump_type(col, r->type, r->bounds);
            }
            break;

         case VCODE_OP_STORE:
            {
               color_printf("$magenta$%s$$ := ",
                            istr(vcode_var_data(op->address)->name));
               printf("%s ", vcode_op_string(op->kind));
               vcode_dump_reg(op->args[0]);
            }
            break;

         case VCODE_OP_STORE_INDIRECT:
            {
               vcode_dump_reg(op->args[1]);
               printf(" := %s ", vcode_op_string(op->kind));
               vcode_dump_reg(op->args[0]);
            }
            break;

         case VCODE_OP_INDEX:
            {
               col += vcode_dump_reg(op->result);
               col += color_printf(" := %s $magenta$%s$$ + ",
                                   vcode_op_string(op->kind),
                                   istr(vcode_var_data(op->address)->name));
               col += vcode_dump_reg(op->args[0]);
               if (op->result != VCODE_INVALID_REG) {
                  reg_t *r = vcode_reg_data(op->result);
                  vcode_dump_type(col, r->type, r->bounds);
               }
            }
            break;

         case VCODE_OP_MUL:
         case VCODE_OP_ADD:
         case VCODE_OP_SUB:
         case VCODE_OP_DIV:
         case VCODE_OP_EXP:
         case VCODE_OP_MOD:
         case VCODE_OP_REM:
            {
               col += vcode_dump_reg(op->result);
               col += printf(" := %s ", vcode_op_string(op->kind));
               col += vcode_dump_reg(op->args[0]);
               switch (op->kind) {
               case VCODE_OP_MUL: col += printf(" * "); break;
               case VCODE_OP_ADD: col += printf(" + "); break;
               case VCODE_OP_SUB: col += printf(" - "); break;
               case VCODE_OP_DIV: col += printf(" / "); break;
               case VCODE_OP_EXP: col += printf(" ** "); break;
               case VCODE_OP_MOD: col += printf(" %% "); break;
               case VCODE_OP_REM: col += printf(" %% "); break;
               default: break;
               }
               col += vcode_dump_reg(op->args[1]);
               reg_t *r = vcode_reg_data(op->result);
               vcode_dump_type(col, r->type, r->bounds);
            }
            break;

         case VCODE_OP_BOUNDS:
            {
               vtype_t *vt = vcode_type_data(op->type);
               printf("%s ", vcode_op_string(op->kind));
               vcode_dump_reg(op->args[0]);
               printf(" in ");
               vcode_pretty_print_int(vt->low);
               printf(" .. ");
               vcode_pretty_print_int(vt->high);
            }
            break;

         case VCODE_OP_COMMENT:
            {
               color_printf("$cyan$// %s$$ ", op->comment);
            }
            break;

         case VCODE_OP_CONST_ARRAY:
            {
               col += vcode_dump_reg(op->result);
               col += printf(" := const [");
               for (int k = 0; k < op->nargs; k++) {
                  if (k > 0)
                     col += printf(",");
                  col += vcode_dump_reg(op->args[k]);
               }

               col += printf("]");
               reg_t *r = vcode_reg_data(op->result);
               vcode_dump_type(col, r->type, r->bounds);
            }
            break;

         case VCODE_OP_CAST:
            {
               col += vcode_dump_reg(op->result);
               col += printf(" := %s ", vcode_op_string(op->kind));
               col += vcode_dump_reg(op->args[0]);
               reg_t *r = vcode_reg_data(op->result);
               vcode_dump_type(col, r->type, r->bounds);
            }
            break;

         case VCODE_OP_RETURN:
            {
               printf("%s ", vcode_op_string(op->kind));
               if (op->nargs > 0)
                  vcode_dump_reg(op->args[0]);
            }
            break;

         case VCODE_OP_NETS:
            {
               col += vcode_dump_reg(op->result);
               col += color_printf(" := %s $white$%s$$",
                                   vcode_op_string(op->kind),
                                   istr(vcode_signal_name(op->signal)));
               reg_t *r = vcode_reg_data(op->result);
               vcode_dump_type(col, r->type, r->bounds);
            }
            break;

         case VCODE_OP_SCHED_WAVEFORM:
            {
               printf("%s ", vcode_op_string(op->kind));
               vcode_dump_reg(op->args[0]);
               printf(" count ");
               vcode_dump_reg(op->args[1]);
               printf(" values ");
               vcode_dump_reg(op->args[2]);
               printf(" reject ");
               vcode_dump_reg(op->args[3]);
               printf(" after ");
               vcode_dump_reg(op->args[4]);
            }
            break;

         case VCODE_OP_NEG:
         case VCODE_OP_ABS:
            {
               col += vcode_dump_reg(op->result);
               col += printf(" := %s ", vcode_op_string(op->kind));
               col += vcode_dump_reg(op->args[0]);
               reg_t *r = vcode_reg_data(op->result);
               vcode_dump_type(col, r->type, r->bounds);
            }
            break;

         case VCODE_OP_IMAGE:
            {
               col += vcode_dump_reg(op->result);
               col += printf(" := %s ", vcode_op_string(op->kind));
               col += vcode_dump_reg(op->args[0]);
               reg_t *r = vcode_reg_data(op->result);
               vcode_dump_type(col, r->type, r->bounds);
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

      const vtype_t *at = vcode_type_data(a);
      const vtype_t *bt = vcode_type_data(b);

      if (at->kind != bt->kind)
         return false;
      else {
         switch (at->kind) {
         case VCODE_TYPE_INT:
            return (at->low == bt->low) && (at->high == bt->high);
         case VCODE_TYPE_CARRAY:
            {
               if (at->ndim != bt->ndim)
                  return false;

               for (unsigned i = 0; i < at->ndim; i++) {
                  if (!vtype_eq(at->dim[i], bt->dim[i]))
                     return false;
               }

               return vtype_eq(at->elem, bt->elem)
                  && vtype_eq(at->bounds, bt->bounds);
            }
         case VCODE_TYPE_UARRAY:
            return at->ndim == bt->ndim
               && vtype_eq(at->elem, bt->elem)
               && vtype_eq(at->bounds, bt->bounds);

         case VCODE_TYPE_POINTER:
            return vtype_eq(at->pointed, bt->pointed);
         case VCODE_TYPE_OFFSET:
            return true;
         case VCODE_TYPE_SIGNAL:
            return vtype_eq(at->base, bt->base);
         }
      }

      return false;
   }
}

bool vtype_includes(vcode_type_t type, vcode_type_t bounds)
{
   const vtype_t *tt = vcode_type_data(type);
   const vtype_t *bt = vcode_type_data(bounds);

   if (bt->kind != tt->kind) {
      vcode_dump();
      fatal_trace("type mismatch in vtype_includes");
   }

   switch (bt->kind) {
   case VCODE_TYPE_INT:
      return bt->low >= tt->low && bt->high <= tt->high;

   case VCODE_TYPE_CARRAY:
   case VCODE_TYPE_UARRAY:
      return vtype_eq(type, bounds);

   case VCODE_TYPE_POINTER:
   case VCODE_TYPE_OFFSET:
      return false;

   case VCODE_TYPE_SIGNAL:
      return vtype_includes(tt->base, bt->base);
   }

   return false;
}

static vcode_type_t vtype_new(vcode_type_t new)
{
   for (int i = 0; i < active_unit->ntypes - 1; i++) {
      if (vtype_eq(i, new)) {
         active_unit->ntypes--;
         new = i;
         break;
      }
   }

   if (active_unit->kind == VCODE_UNIT_CONTEXT)
      new |= 0x80000000;

   return new;
}

vcode_type_t vtype_int(int64_t low, int64_t high)
{
   assert(active_unit != NULL);
   assert(low <= high);

   assert(active_unit->ntypes < MAX_TYPES);
   vcode_type_t r = active_unit->ntypes++;

   vtype_t *n = &(active_unit->types[r]);
   n->kind = VCODE_TYPE_INT;
   n->low  = low;
   n->high = high;

   return vtype_new(r);
}

vcode_type_t vtype_bool(void)
{
   return vtype_int(0, 1);
}

vcode_type_t vtype_carray(const vcode_type_t *dim, int ndim,
                          vcode_type_t elem, vcode_type_t bounds)
{
   assert(active_unit != NULL);

   assert(active_unit->ntypes < MAX_TYPES);
   vcode_type_t r = active_unit->ntypes++;

   vtype_t *n = &(active_unit->types[r]);
   n->kind   = VCODE_TYPE_CARRAY;
   n->elem   = elem;
   n->bounds = bounds;
   n->ndim   = ndim;
   for (int i = 0; i < ndim; i++)
      n->dim[i] = dim[i];

   return vtype_new(r);
}

vcode_type_t vtype_uarray(int ndim, vcode_type_t elem, vcode_type_t bounds)
{
   assert(active_unit != NULL);

   assert(active_unit->ntypes < MAX_TYPES);
   vcode_type_t r = active_unit->ntypes++;

   vtype_t *n = &(active_unit->types[r]);
   n->kind   = VCODE_TYPE_UARRAY;
   n->elem   = elem;
   n->bounds = bounds;
   n->ndim   = ndim;

   return vtype_new(r);
}

vcode_type_t vtype_pointer(vcode_type_t to)
{
   assert(active_unit != NULL);

   assert(active_unit->ntypes < MAX_TYPES);
   vcode_type_t r = active_unit->ntypes++;

   vtype_t *n = &(active_unit->types[r]);
   n->kind    = VCODE_TYPE_POINTER;
   n->pointed = to;

   return vtype_new(r);
}

vcode_type_t vtype_signal(vcode_type_t base)
{
   assert(active_unit != NULL);

   assert(active_unit->ntypes < MAX_TYPES);
   vcode_type_t r = active_unit->ntypes++;

   vtype_t *n = &(active_unit->types[r]);
   n->kind = VCODE_TYPE_SIGNAL;
   n->base = base;

   return vtype_new(r);
}

vcode_type_t vtype_offset(void)
{
   assert(active_unit != NULL);

   assert(active_unit->ntypes < MAX_TYPES);
   vcode_type_t r = active_unit->ntypes++;

   vtype_t *n = &(active_unit->types[r]);
   n->kind = VCODE_TYPE_OFFSET;

   return vtype_new(r);
}

vtype_kind_t vtype_kind(vcode_type_t type)
{
   vtype_t *vt = vcode_type_data(type);
   return vt->kind;
}

vcode_type_t vtype_elem(vcode_type_t type)
{
   vtype_t *vt = vcode_type_data(type);
   assert(vt->kind == VCODE_TYPE_CARRAY || vt->kind == VCODE_TYPE_UARRAY);
   return vt->elem;
}

vcode_type_t vtype_bounds(vcode_type_t type)
{
   vtype_t *vt = vcode_type_data(type);
   assert(vt->kind == VCODE_TYPE_CARRAY || vt->kind == VCODE_TYPE_UARRAY);
   return vt->bounds;
}

int vtype_dims(vcode_type_t type)
{
   vtype_t *vt = vcode_type_data(type);
   assert(vt->kind == VCODE_TYPE_CARRAY || vt->kind == VCODE_TYPE_UARRAY);
   return vt->ndim;
}

vcode_type_t vtype_dim(vcode_type_t type, int dim)
{
   vtype_t *vt = vcode_type_data(type);
   assert(vt->kind == VCODE_TYPE_CARRAY);
   assert(dim < vt->ndim);
   return vt->dim[dim];
}

vcode_type_t vtype_pointed(vcode_type_t type)
{
   vtype_t *vt = vcode_type_data(type);
   assert(vt->kind == VCODE_TYPE_POINTER);
   return vt->pointed;
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

int vcode_count_params(void)
{
   assert(active_unit != NULL);
   assert(active_unit->kind == VCODE_UNIT_FUNCTION);

   return active_unit->params.count;
}

vcode_type_t vcode_param_type(int param)
{
   assert(active_unit != NULL);
   assert(active_unit->kind == VCODE_UNIT_FUNCTION);
   assert(param < active_unit->params.count);

   return active_unit->params.items[param].type;
}

vcode_reg_t vcode_param_reg(int param)
{
   assert(active_unit != NULL);
   assert(active_unit->kind == VCODE_UNIT_FUNCTION);
   assert(param < active_unit->params.count);

   return active_unit->params.items[param].reg;
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

ident_t vcode_unit_name(void)
{
   assert(active_unit != NULL);
   return active_unit->name;
}

vcode_type_t vcode_unit_result(void)
{
   assert(active_unit != NULL);
   assert(active_unit->kind = VCODE_UNIT_FUNCTION);
   return active_unit->result;
}

vunit_kind_t vcode_unit_kind(void)
{
   assert(active_unit != NULL);
   return active_unit->kind;
}

vcode_unit_t emit_function(ident_t name, vcode_unit_t context,
                           vcode_type_t result)
{
   assert(context->kind == VCODE_UNIT_CONTEXT);

   vcode_unit_t vu = xcalloc(sizeof(struct vcode_unit));
   vu->kind    = VCODE_UNIT_FUNCTION;
   vu->name    = name;
   vu->context = context;
   vu->result  = result;

   active_unit = vu;
   vcode_select_block(emit_block());

   return vu;
}

vcode_unit_t emit_process(ident_t name, vcode_unit_t context)
{
   assert(context->kind == VCODE_UNIT_CONTEXT);

   vcode_unit_t vu = xcalloc(sizeof(struct vcode_unit));
   vu->kind    = VCODE_UNIT_PROCESS;
   vu->name    = name;
   vu->context = context;

   active_unit = vu;
   vcode_select_block(emit_block());

   return vu;
}

vcode_unit_t emit_context(ident_t name)
{
   vcode_unit_t vu = xcalloc(sizeof(struct vcode_unit));
   vu->kind    = VCODE_UNIT_CONTEXT;
   vu->name    = name;
   vu->context = vu;

   active_unit = vu;
   vcode_select_block(emit_block());

   return vu;
}

void emit_assert(vcode_reg_t value, vcode_reg_t message,
                 vcode_reg_t severity, uint32_t index)
{
   if (vtype_eq(vcode_reg_data(value)->bounds, vtype_int(1, 1))) {
      vcode_add_comment("Always true assertion on r%d", value);
      return;
   }

   op_t *op = vcode_add_op(VCODE_OP_ASSERT);
   vcode_add_arg(op, value);
   vcode_add_arg(op, severity);
   vcode_add_arg(op, message);
   op->index = index;

   if (!vtype_eq(vcode_reg_type(value), vtype_bool())) {
      vcode_dump();
      fatal_trace("value parameter to assert is not bool");
   }
}

void emit_report(vcode_reg_t message, vcode_reg_t severity, uint32_t index)
{
   op_t *op = vcode_add_op(VCODE_OP_REPORT);
   vcode_add_arg(op, severity);
   vcode_add_arg(op, message);
   op->index = index;
}

vcode_reg_t emit_cmp(vcode_cmp_t cmp, vcode_reg_t lhs, vcode_reg_t rhs)
{
   if (cmp == VCODE_CMP_EQ && lhs == rhs)
      return emit_const(vtype_bool(), 1);

   op_t *op = vcode_add_op(VCODE_OP_CMP);
   vcode_add_arg(op, lhs);
   vcode_add_arg(op, rhs);
   op->cmp    = cmp;
   op->result = vcode_add_reg(vtype_bool());

   if (!vtype_eq(vcode_reg_type(lhs), vcode_reg_type(rhs))) {
      vcode_dump();
      fatal_trace("arguments to cmp are not the same type");
   }

   return op->result;
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
   // Reuse any previous constant in this block with the same type and value
   block_t *b = &(active_unit->blocks[active_block]);
   for (int i = b->nops - 1; i >= 0; i--) {
      const op_t *op = &(b->ops[i]);
      if (op->kind == VCODE_OP_CONST && op->value == value
          && vtype_eq(type, op->type))
         return op->result;
   }

   op_t *op = vcode_add_op(VCODE_OP_CONST);
   op->value  = value;
   op->type   = type;
   op->result = vcode_add_reg(type);

   vtype_kind_t type_kind = vtype_kind(type);
   if (type_kind != VCODE_TYPE_INT && type_kind != VCODE_TYPE_OFFSET) {
      vcode_dump();
      fatal_trace("constant must have integer or offset type");
   }

   reg_t *r = vcode_reg_data(op->result);
   r->bounds = vtype_int(value, value);

   return op->result;
}

vcode_reg_t emit_const_array(vcode_type_t type, vcode_reg_t *values, int num)
{
   op_t *op = vcode_add_op(VCODE_OP_CONST_ARRAY);
   op->type   = type;
   op->result = vcode_add_reg(type);

   for (int i = 0; i < num; i++)
      vcode_add_arg(op, values[i]);

   if (vtype_kind(type) != VCODE_TYPE_CARRAY) {
      vcode_dump();
      fatal_trace("constant array must have constrained array type");
   }

   return op->result;
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

vcode_var_t emit_var(vcode_type_t type, vcode_type_t bounds, ident_t name)
{
   assert(active_unit != NULL);
   assert(active_unit->nvars < MAX_VARS);

   var_t *v = &(active_unit->vars[active_unit->nvars]);
   v->type   = type;
   v->bounds = bounds;
   v->name   = name;

   if (active_unit->kind == VCODE_UNIT_CONTEXT) {
      assert(type & 0x80000000);
      assert(bounds & 0x80000000);
      return active_unit->nvars++ | 0x80000000;
   }
   else
      return active_unit->nvars++;
}

vcode_reg_t emit_param(vcode_type_t type, vcode_type_t bounds, ident_t name)
{
   assert(active_unit != NULL);

   param_t p = {
      .type   = type,
      .bounds = bounds,
      .name   = name,
      .reg    = vcode_add_reg(type)
   };

   param_array_add(&(active_unit->params), p);

   reg_t *rr = vcode_reg_data(p.reg);
   rr->bounds = bounds;

   return p.reg;
}

vcode_signal_t emit_signal(vcode_type_t type, vcode_type_t bounds,
                           ident_t name, vcode_var_t shadow,
                           netid_t *nets, size_t nnets)
{
   assert(active_unit != NULL);
   assert(active_unit->nsignals < MAX_SIGNALS);
   assert(active_unit->kind == VCODE_UNIT_CONTEXT);

   signal_t *s = &(active_unit->signals[active_unit->nsignals]);
   s->type   = type;
   s->bounds = bounds;
   s->name   = name;
   s->shadow = shadow;
   s->nets   = nets;
   s->nnets  = nnets;

   return active_unit->nsignals++;
}

vcode_reg_t emit_load(vcode_var_t var)
{
   // Try scanning backwards through the block for another load or store to
   // this variable
   block_t *b = &(active_unit->blocks[active_block]);
   for (int i = b->nops - 1; i >= 0; i--) {
      const op_t *op = &(b->ops[i]);
      if ((op->kind == VCODE_OP_LOAD) && (op->address == var))
         return op->result;
      else if ((op->kind == VCODE_OP_STORE) && (op->address == var))
         return op->args[0];
   }

   var_t *v = vcode_var_data(var);

   op_t *op = vcode_add_op(VCODE_OP_LOAD);
   op->address = var;
   op->result  = vcode_add_reg(v->type);

   if (vtype_kind(v->type) != VCODE_TYPE_INT) {
      vcode_dump();
      fatal_trace("cannot load non-scalar type");
   }

   reg_t *r = vcode_reg_data(op->result);
   r->bounds = v->bounds;

   return op->result;
}

vcode_reg_t emit_load_indirect(vcode_reg_t reg)
{
   op_t *op = vcode_add_op(VCODE_OP_LOAD_INDIRECT);
   vcode_add_arg(op, reg);

   vcode_type_t rtype = vcode_reg_type(reg);

   if (vtype_kind(rtype) != VCODE_TYPE_POINTER) {
      vcode_dump();
      fatal_trace("load indirect with non-pointer argument");
   }

   vcode_type_t deref = vtype_pointed(rtype);
   op->result = vcode_add_reg(deref);

   if (vtype_kind(deref) != VCODE_TYPE_INT) {
      vcode_dump();
      fatal_trace("cannot load non-scalar type");
   }

   vcode_reg_data(op->result)->bounds = vcode_reg_data(reg)->bounds;

   return op->result;
}

void emit_store(vcode_reg_t reg, vcode_var_t var)
{
   // Any previous store to this variable in this block is dead
   block_t *b = &(active_unit->blocks[active_block]);
   for (int i = 0; i < b->nops; i++) {
      op_t *op = &(b->ops[i]);
      if (op->kind == VCODE_OP_STORE && op->address == var) {
         b->nops--;
         memmove(op, op + 1, sizeof(op_t) * (b->nops - i));
      }
   }

   var_t *v = vcode_var_data(var);
   reg_t *r = vcode_reg_data(reg);

   op_t *op = vcode_add_op(VCODE_OP_STORE);
   vcode_add_arg(op, reg);
   op->address = var;

   if (!vtype_eq(v->type, r->type)) {
      vcode_dump();
      fatal_trace("variable and stored value do not have same type");
   }
}

void emit_store_indirect(vcode_reg_t reg, vcode_reg_t ptr)
{
   reg_t *p = vcode_reg_data(ptr);
   reg_t *r = vcode_reg_data(reg);

   op_t *op = vcode_add_op(VCODE_OP_STORE_INDIRECT);
   vcode_add_arg(op, reg);
   vcode_add_arg(op, ptr);

   if (!vtype_eq(vtype_pointed(p->type), r->type)) {
      vcode_dump();
      fatal_trace("pointer and stored value do not have same type");
   }
}

static vcode_reg_t emit_arith(vcode_op_t kind, vcode_reg_t lhs, vcode_reg_t rhs)
{
   op_t *op = vcode_add_op(kind);
   vcode_add_arg(op, lhs);
   vcode_add_arg(op, rhs);
   op->result = vcode_add_reg(vcode_reg_type(lhs));

   vcode_type_t lhs_type = vcode_reg_type(lhs);
   vcode_type_t rhs_type = vcode_reg_type(rhs);

   if (vtype_kind(lhs_type) == VCODE_TYPE_POINTER
       && vtype_kind(rhs_type) == VCODE_TYPE_OFFSET)
      ;
   else if (!vtype_eq(lhs_type, rhs_type)) {
      vcode_dump();
      fatal_trace("arguments to %s are not the same type",
                  vcode_op_string(kind));
   }

   return op->result;
}

vcode_reg_t emit_mul(vcode_reg_t lhs, vcode_reg_t rhs)
{
   int64_t lconst, rconst;
   if (vcode_reg_const(lhs, &lconst) && vcode_reg_const(rhs, &rconst))
      return emit_const(vcode_reg_type(lhs), lconst * rconst);

   vcode_reg_t reg = emit_arith(VCODE_OP_MUL, lhs, rhs);

   vtype_t *bl = vcode_type_data(vcode_reg_data(lhs)->bounds);
   vtype_t *br = vcode_type_data(vcode_reg_data(rhs)->bounds);

   const int64_t ll = smul64(bl->low, br->low);
   const int64_t lh = smul64(bl->low, br->high);
   const int64_t hl = smul64(bl->high, br->low);
   const int64_t hh = smul64(bl->high, br->high);

   const int64_t min = MIN(MIN(ll, lh), MIN(hl, hh));
   const int64_t max = MAX(MAX(ll, lh), MAX(hl, hh));

   reg_t *rr = vcode_reg_data(reg);
   rr->bounds = vtype_int(min, max);

   return reg;
}

vcode_reg_t emit_div(vcode_reg_t lhs, vcode_reg_t rhs)
{
   int64_t lconst, rconst;
   if (vcode_reg_const(lhs, &lconst) && vcode_reg_const(rhs, &rconst))
      return emit_const(vcode_reg_type(lhs), lconst / rconst);

   return emit_arith(VCODE_OP_DIV, lhs, rhs);
}

vcode_reg_t emit_exp(vcode_reg_t lhs, vcode_reg_t rhs)
{
   int64_t lconst, rconst;
   if (vcode_reg_const(lhs, &lconst) && vcode_reg_const(rhs, &rconst))
      return emit_const(vcode_reg_type(lhs), ipow(lconst, rconst));

   return emit_arith(VCODE_OP_EXP, lhs, rhs);
}

vcode_reg_t emit_mod(vcode_reg_t lhs, vcode_reg_t rhs)
{
   int64_t lconst, rconst;
   if (vcode_reg_const(lhs, &lconst) && vcode_reg_const(rhs, &rconst)
       && lconst > 0 && rconst > 0)
      return emit_const(vcode_reg_type(lhs), lconst % rconst);

   return emit_arith(VCODE_OP_MOD, lhs, rhs);
}

vcode_reg_t emit_rem(vcode_reg_t lhs, vcode_reg_t rhs)
{
   int64_t lconst, rconst;
   if (vcode_reg_const(lhs, &lconst) && vcode_reg_const(rhs, &rconst)
       && lconst > 0 && rconst > 0)
      return emit_const(vcode_reg_type(lhs), lconst % rconst);

   return emit_arith(VCODE_OP_REM, lhs, rhs);
}

vcode_reg_t emit_add(vcode_reg_t lhs, vcode_reg_t rhs)
{
   int64_t lconst, rconst;
   if (vcode_reg_const(lhs, &lconst) && vcode_reg_const(rhs, &rconst))
      return emit_const(vcode_reg_type(lhs), lconst + rconst);
   else if (vtype_kind(vcode_reg_type(lhs)) == VCODE_TYPE_POINTER
            && vcode_reg_const(rhs, &rconst) && rconst == 0)
      return lhs;

   vcode_reg_t reg = emit_arith(VCODE_OP_ADD, lhs, rhs);

   reg_t *rr = vcode_reg_data(reg);
   if (vtype_kind(vcode_reg_type(reg)) == VCODE_TYPE_POINTER)
      rr->bounds = vcode_reg_data(lhs)->bounds;
   else {
      vtype_t *bl = vcode_type_data(vcode_reg_data(lhs)->bounds);
      vtype_t *br = vcode_type_data(vcode_reg_data(rhs)->bounds);

      rr->bounds = vtype_int(sadd64(bl->low, br->low),
                             sadd64(bl->high, br->high));
   }

   return reg;
}

vcode_reg_t emit_sub(vcode_reg_t lhs, vcode_reg_t rhs)
{
   int64_t lconst, rconst;
   if (vcode_reg_const(lhs, &lconst) && vcode_reg_const(rhs, &rconst))
      return emit_const(vcode_reg_type(lhs), lconst - rconst);

   vcode_reg_t reg = emit_arith(VCODE_OP_SUB, lhs, rhs);

   reg_t *rr = vcode_reg_data(reg);
   if (vtype_kind(vcode_reg_type(reg)) == VCODE_TYPE_POINTER)
      rr->bounds = vcode_reg_data(lhs)->bounds;
   else {
      vtype_t *bl = vcode_type_data(vcode_reg_data(lhs)->bounds);
      vtype_t *br = vcode_type_data(vcode_reg_data(rhs)->bounds);

      reg_t *rr = vcode_reg_data(reg);
      rr->bounds = vtype_int(sadd64(bl->low, -br->high),
                             sadd64(bl->high, -br->low));
   }

   return reg;
}

void emit_bounds(vcode_reg_t reg, vcode_type_t bounds)
{
   if (vtype_includes(bounds, vcode_reg_data(reg)->bounds)) {
      vcode_add_comment("Elided bounds check for r%d", reg);
      return;
   }

   op_t *op = vcode_add_op(VCODE_OP_BOUNDS);
   vcode_add_arg(op, reg);
   op->type = bounds;
}

vcode_reg_t emit_index(vcode_var_t var, vcode_reg_t offset)
{
   // Try to find a previous index of this var by this offset
   block_t *b = &(active_unit->blocks[active_block]);
   for (int i = b->nops - 1; i >= 0; i--) {
      const op_t *op = &(b->ops[i]);
      if (op->kind == VCODE_OP_INDEX && op->address == var
          && op->args[0] == offset)
         return op->result;
   }

   op_t *op = vcode_add_op(VCODE_OP_INDEX);
   vcode_add_arg(op, offset);
   op->address = var;

   vcode_type_t typeref = vcode_var_type(var);
   vtype_t *vt = vcode_type_data(typeref);
   switch (vt->kind) {
   case VCODE_TYPE_CARRAY:
      op->type = vtype_pointer(vt->elem);
      op->result = vcode_add_reg(op->type);
      vcode_reg_data(op->result)->bounds = vt->bounds;
      break;

   case VCODE_TYPE_POINTER:
      op->type = typeref;
      op->result = vcode_add_reg(op->type);
      vcode_reg_data(op->result)->bounds = vcode_var_data(var)->bounds;
      break;

   default:
      vcode_dump();
      fatal_trace("indexed variable %s is not an array or pointer",
                  istr(vcode_var_name(var)));
   }

   if (vtype_kind(vcode_reg_type(offset)) != VCODE_TYPE_OFFSET) {
      vcode_dump();
      fatal_trace("index offset r%d does not have offset type", offset);
   }

   return op->result;
}

vcode_reg_t emit_cast(vcode_type_t type, vcode_reg_t reg)
{
   int64_t value;
   if (vcode_reg_const(reg, &value))
      return emit_const(type, value);

   // Try to find a previous cast of this register to this type
   block_t *b = &(active_unit->blocks[active_block]);
   for (int i = b->nops - 1; i >= 0; i--) {
      const op_t *op = &(b->ops[i]);
      if (op->kind == VCODE_OP_CAST && vtype_eq(op->type, type)
          && op->args[0] == reg)
         return op->result;
   }

   op_t *op = vcode_add_op(VCODE_OP_CAST);
   vcode_add_arg(op, reg);
   op->type   = type;
   op->result = vcode_add_reg(type);

   reg_t *rr = vcode_reg_data(op->result);
   rr->bounds = vcode_reg_bounds(reg);

   static const vcode_type_t allowed[][2] = {
      { VCODE_TYPE_INT, VCODE_TYPE_OFFSET }
   };

   vtype_kind_t from = vtype_kind(vcode_reg_type(reg));
   vtype_kind_t to   = vtype_kind(type);

   for (size_t i = 0; i < ARRAY_LEN(allowed); i++) {
      if (from == allowed[i][0] && to == allowed[i][1])
         return op->result;
   }

   vcode_dump();
   fatal_trace("invalid type conversion in cast");
}

void emit_return(vcode_reg_t reg)
{
   op_t *op = vcode_add_op(VCODE_OP_RETURN);
   if (reg != VCODE_INVALID_REG) {
      if (active_unit->kind != VCODE_UNIT_FUNCTION) {
         vcode_dump();
         fatal_trace("returning value fron non-function unit");
      }
      else if (!vtype_eq(active_unit->result, vcode_reg_type(reg))) {
         vcode_dump();
         fatal_trace("return value incorrect type");
      }

      vcode_add_arg(op, reg);
   }
}

vcode_reg_t emit_nets(vcode_signal_t sig)
{
   op_t *op = vcode_add_op(VCODE_OP_NETS);
   op->signal = sig;

   vcode_type_t stype = vcode_signal_type(sig);
   op->type = stype;

   op->result = vcode_add_reg(stype);

   reg_t *rr = vcode_reg_data(op->result);
   rr->bounds = vcode_signal_bounds(sig);

   return op->result;
}

void emit_sched_waveform(vcode_reg_t nets, vcode_reg_t nnets,
                         vcode_reg_t values, vcode_reg_t reject,
                         vcode_reg_t after)
{
   op_t *op = vcode_add_op(VCODE_OP_SCHED_WAVEFORM);
   vcode_add_arg(op, nets);
   vcode_add_arg(op, nnets);
   vcode_add_arg(op, values);
   vcode_add_arg(op, reject);
   vcode_add_arg(op, after);

   if (vtype_kind(vcode_reg_type(nets)) != VCODE_TYPE_SIGNAL) {
      vcode_dump();
      fatal_trace("sched_waveform target is not signal");
   }
   else if (vtype_kind(vcode_reg_type(nnets)) != VCODE_TYPE_OFFSET) {
      vcode_dump();
      fatal_trace("sched_waveform net count is not offset type");
   }
}

void emit_cond(vcode_reg_t test, vcode_block_t btrue, vcode_block_t bfalse)
{
   op_t *op = vcode_add_op(VCODE_OP_COND);
   vcode_add_arg(op, test);
   op->target = btrue;
   op->target_else = bfalse;

   if (!vtype_eq(vcode_reg_type(test), vtype_bool())) {
      vcode_dump();
      fatal_trace("cond test is not a bool");
   }
}

vcode_reg_t emit_neg(vcode_reg_t lhs)
{
   int64_t lconst;
   if (vcode_reg_const(lhs, &lconst))
      return emit_const(vcode_reg_type(lhs), -lconst);

   op_t *op = vcode_add_op(VCODE_OP_NEG);
   vcode_add_arg(op, lhs);
   op->result = vcode_add_reg(vcode_reg_type(lhs));

   return op->result;
}

vcode_reg_t emit_abs(vcode_reg_t lhs)
{
   int64_t lconst;
   if (vcode_reg_const(lhs, &lconst))
      return emit_const(vcode_reg_type(lhs), -lconst);

   op_t *op = vcode_add_op(VCODE_OP_ABS);
   vcode_add_arg(op, lhs);
   op->result = vcode_add_reg(vcode_reg_type(lhs));

   return op->result;
}

vcode_reg_t emit_image(vcode_reg_t value, uint32_t index)
{
   op_t *op = vcode_add_op(VCODE_OP_IMAGE);
   vcode_add_arg(op, value);
   op->index = index;

   op->result = vcode_add_reg(
      vtype_uarray(1, vtype_int(0, 255), vtype_int(0, 127)));

   return op->result;
}
