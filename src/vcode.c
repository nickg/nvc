//
//  Copyright (C) 2014-2015  Nick Gasson
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

DECLARE_AND_DEFINE_ARRAY(vcode_reg);
DECLARE_AND_DEFINE_ARRAY(vcode_block);
DECLARE_AND_DEFINE_ARRAY(vcode_type);

typedef struct {
   vcode_op_t          kind;
   vcode_reg_array_t   args;
   vcode_reg_t         result;
   vcode_type_t        type;
   vcode_block_array_t targets;
   vcode_var_t         address;
   uint32_t            index;
   union {
      vcode_cmp_t    cmp;
      ident_t        func;
      int64_t        value;
      char          *comment;
      vcode_signal_t signal;
      unsigned       dim;
      unsigned       hops;
      unsigned       field;
      unsigned       flags;
   };
} op_t;

DECLARE_AND_DEFINE_ARRAY(op);

typedef struct {
   op_array_t ops;
} block_t;

typedef struct {
   vcode_type_t type;
   vcode_type_t bounds;
} reg_t;

typedef struct {
   vtype_kind_t       kind;
   union {
      struct {
         int64_t low;
         int64_t high;
      };
      struct {
         vcode_type_array_t dims;
         vcode_type_t       elem;
         vcode_type_t       bounds;
      };
      vcode_type_t       pointed;
      vcode_type_t       base;
      vcode_type_array_t fields;
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
DECLARE_AND_DEFINE_ARRAY(var);
DECLARE_AND_DEFINE_ARRAY(reg);
DECLARE_AND_DEFINE_ARRAY(block);
DECLARE_AND_DEFINE_ARRAY(signal);
DECLARE_AND_DEFINE_ARRAY(vtype);

struct vcode_unit {
   vunit_kind_t   kind;
   vcode_unit_t   context;
   ident_t        name;
   vcode_type_t   result;
   block_array_t  blocks;
   reg_array_t    regs;
   vtype_array_t  types;
   var_array_t    vars;
   signal_array_t signals;
   param_array_t  params;
   unsigned       depth;
};

#define MASK_CONTEXT(x)   ((x) >> 24)
#define MASK_INDEX(x)     ((x) & 0xffffff)
#define MAKE_HANDLE(c, i) (((c) & 0xff) << 24 | ((i) & 0xffffff))

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

   vcode_reg_t reg = active_unit->regs.count;
   reg_t *r = reg_array_alloc(&(active_unit->regs));
   memset(r, '\0', sizeof(reg_t));
   r->type   = type;
   r->bounds = type;

   return reg;
}

static block_t *vcode_block_data(void)
{
   assert(active_unit != NULL);
   assert(active_block != -1);
   return &(active_unit->blocks.items[active_block]);
}

static op_t *vcode_add_op(vcode_op_t kind)
{
   assert(active_unit != NULL);
   assert(active_block != VCODE_INVALID_BLOCK);

   if (vcode_block_finished()) {
      vcode_dump();
      fatal_trace("attempt to add to already finished block %d", active_block);
   }

   block_t *block = vcode_block_data();

   op_t *op = op_array_alloc(&(block->ops));
   memset(op, '\0', sizeof(op_t));
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
   vcode_reg_array_add(&(op->args), arg);
}

static void vcode_add_target(op_t *op, vcode_block_t block)
{
   vcode_block_array_add(&(op->targets), block);
}

static op_t *vcode_op_data(int op)
{
   assert(active_unit != NULL);
   assert(active_block != VCODE_INVALID_BLOCK);

   block_t *b = &(active_unit->blocks.items[active_block]);
   return op_array_nth_ptr(&(b->ops), op);
}

static reg_t *vcode_reg_data(vcode_reg_t reg)
{
   assert(active_unit != NULL);
   assert(reg != VCODE_INVALID_REG);
   return reg_array_nth_ptr(&(active_unit->regs), reg);
}

static vtype_t *vcode_type_data(vcode_type_t type)
{
   assert(type != VCODE_INVALID_TYPE);
   assert(active_unit != NULL);
   vcode_unit_t unit = active_unit;

   int depth = MASK_CONTEXT(type);
   assert(depth <= unit->depth);
   while (depth != unit->depth)
      unit = unit->context;

   return vtype_array_nth_ptr(&(unit->types), MASK_INDEX(type));
}

static signal_t *vcode_signal_data(vcode_signal_t sig)
{
   assert(active_unit != NULL);
   vcode_unit_t unit = active_unit->context;
   assert(unit->kind == VCODE_UNIT_CONTEXT);
   return signal_array_nth_ptr(&(unit->signals), sig);
}

static var_t *vcode_var_data(vcode_var_t var)
{
   assert(active_unit != NULL);
   assert(var != VCODE_INVALID_VAR);
   vcode_unit_t unit = active_unit;

   int depth = MASK_CONTEXT(var);
   assert(depth <= unit->depth);
   while (depth != unit->depth)
      unit = unit->context;

   return var_array_nth_ptr(&(unit->vars), MASK_INDEX(var));
}

__attribute__((format(printf, 1, 2), noreturn))
static void vcode_fail(const char *fmt, ...)
{
   va_list ap;
   va_start(ap, fmt);
   char *buf LOCAL = xvasprintf(fmt, ap);
   va_end(ap);

   vcode_dump();
   fatal_trace("%s", buf);
}

static bool vcode_dominating_ops(vcode_op_t kind, op_t **next)
{
   block_t *b = vcode_block_data();

   int index = b->ops.count;
   if (*next != NULL) {
      index = *next - b->ops.items;
      assert(index >= 0 && index < b->ops.count);
   }

   while (index > 0) {
      index--;
      if (b->ops.items[index].kind == kind) {
         *next = &(b->ops.items[index]);
         return true;
      }
   }

   return false;
}

int vcode_count_regs(void)
{
   assert(active_unit != NULL);
   return active_unit->regs.count;
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

   int *uses = xmalloc(active_unit->regs.count * sizeof(int));

   int pruned = 0;
   do {
      memset(uses, '\0', active_unit->regs.count * sizeof(int));
      pruned = 0;

      for (int i = active_unit->blocks.count - 1; i >= 0; i--) {
         block_t *b = &(active_unit->blocks.items[i]);

         for (int j = b->ops.count - 1; j >= 0; j--) {
            op_t *o = &(b->ops.items[j]);

            switch (o->kind) {
            case VCODE_OP_FCALL:
               if (o->result == VCODE_INVALID_REG)
                  break;
            case VCODE_OP_CONST:
            case VCODE_OP_LOAD:
            case VCODE_OP_LOAD_INDIRECT:
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
                  o->kind = VCODE_OP_COMMENT;
                  vcode_reg_array_resize(&(o->args), 0, VCODE_INVALID_REG);
                  pruned++;
               }
               uses[o->result] = -1;
               break;

            default:
               break;
            }

            for (int k = 0; k < o->args.count; k++) {
               if (o->args.items[k] != VCODE_INVALID_REG)
                  uses[o->args.items[k]]++;
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
   return active_unit->blocks.count;
}

int vcode_count_ops(void)
{
   assert(active_unit != NULL);
   assert(active_block != VCODE_INVALID_BLOCK);
   return active_unit->blocks.items[active_block].ops.count;
}

int vcode_count_vars(void)
{
   assert(active_unit != NULL);
   return active_unit->vars.count;
}

vcode_var_t vcode_var_handle(int index)
{
   assert(active_unit != NULL);
   assert(index < active_unit->vars.count);
   return MAKE_HANDLE(active_unit->depth, index);
}

int vcode_var_index(vcode_var_t var)
{
   return MASK_INDEX(var);
}

int vcode_var_context(vcode_var_t var)
{
   return MASK_CONTEXT(var);
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

   return active_unit->signals.count;
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
   assert(o->kind == VCODE_OP_FCALL || o->kind == VCODE_OP_NESTED_FCALL
          || o->kind == VCODE_OP_PCALL || o->kind == VCODE_OP_RESUME);
   return o->func;
}

unsigned vcode_get_flags(int op)
{
   op_t *o = vcode_op_data(op);
   assert(o->kind == VCODE_OP_SCHED_EVENT);
   return o->flags;
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
          || o->kind == VCODE_OP_INDEX || o->kind == VCODE_OP_RESOLVED_ADDRESS);
   return o->address;
}

vcode_signal_t vcode_get_signal(int op)
{
   op_t *o = vcode_op_data(op);
   assert(o->kind == VCODE_OP_NETS || o->kind == VCODE_OP_RESOLVED_ADDRESS
          || o->kind == VCODE_OP_SET_INITIAL);
   return o->signal;
}

unsigned vcode_get_dim(int op)
{
   op_t *o = vcode_op_data(op);
   assert(o->kind == VCODE_OP_UARRAY_LEFT || o->kind == VCODE_OP_UARRAY_RIGHT
          || o->kind == VCODE_OP_UARRAY_DIR);
   return o->dim;
}

int vcode_get_hops(int op)
{
   op_t *o = vcode_op_data(op);
   assert(o->kind == VCODE_OP_PARAM_UPREF);
   return o->hops;
}

int vcode_get_field(int op)
{
   op_t *o = vcode_op_data(op);
   assert(o->kind == VCODE_OP_RECORD_REF);
   return o->field;
}

vcode_var_t vcode_get_type(int op)
{
   op_t *o = vcode_op_data(op);
   assert(o->kind == VCODE_OP_BOUNDS || o->kind == VCODE_OP_ALLOCA
          || o->kind == VCODE_OP_COPY);
   return o->type;
}

int vcode_count_args(int op)
{
   return vcode_op_data(op)->args.count;
}

vcode_reg_t vcode_get_arg(int op, int arg)
{
   op_t *o = vcode_op_data(op);
   return vcode_reg_array_nth(&(o->args), arg);
}

vcode_reg_t vcode_get_result(int op)
{
   op_t *o = vcode_op_data(op);
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
          || o->kind == VCODE_OP_IMAGE || o->kind == VCODE_OP_SET_INITIAL
          || o->kind == VCODE_OP_DIV);
   return o->index;
}

vcode_block_t vcode_get_target(int op, int nth)
{
   op_t *o = vcode_op_data(op);
   assert(o->kind == VCODE_OP_WAIT || o->kind == VCODE_OP_JUMP
          || o->kind == VCODE_OP_COND || o->kind == VCODE_OP_PHI
          || o->kind == VCODE_OP_PCALL || o->kind == VCODE_OP_CASE);
   return vcode_block_array_nth(&(o->targets), nth);
}

bool vcode_block_empty(void)
{
   assert(active_unit != NULL);
   assert(active_block != VCODE_INVALID_BLOCK);

   return active_unit->blocks.items[active_block].ops.count == 0;
}

bool vcode_block_finished(void)
{
   assert(active_unit != NULL);
   assert(active_block != VCODE_INVALID_BLOCK);

   const block_t *b = &(active_unit->blocks.items[active_block]);
   if (b->ops.count == 0)
      return false;
   else {
      vcode_op_t kind = b->ops.items[b->ops.count - 1].kind;
      return kind == VCODE_OP_WAIT || kind == VCODE_OP_JUMP
         || kind == VCODE_OP_COND || kind == VCODE_OP_PCALL
         || kind == VCODE_OP_RETURN || kind == VCODE_OP_CASE;
   }
}

const char *vcode_op_string(vcode_op_t op)
{
   static const char *strs[] = {
      "cmp", "fcall", "wait", "const", "assert", "jump", "load", "store",
      "mul", "add", "bounds", "comment", "const array", "index", "sub",
      "cast", "load indirect", "store indirect", "return", "nets",
      "sched waveform", "cond", "report", "div", "neg", "exp", "abs", "mod",
      "rem", "image", "alloca", "select", "or", "wrap", "uarray left",
      "uarray right", "uarray dir", "unwrap", "not", "phi", "and",
      "nested fcall", "param upref", "resolved address", "set initial",
      "alloc driver", "event", "active", "const record", "record ref", "copy",
      "sched event", "pcall", "resume", "memcmp", "xor", "xnor", "nand", "nor",
      "memset", "vec load", "case", "endfile", "file open", "file write",
      "file close", "file read"
   };
   if ((unsigned)op >= ARRAY_LEN(strs))
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
         for (unsigned i = 0; i < vt->dims.count; i++) {
            if (i > 0)
               printf(", ");
            vcode_dump_one_type(vt->dims.items[i]);
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
         for (unsigned i = 0; i < vt->dims.count; i++)
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

   case VCODE_TYPE_RECORD:
      {
         printf("{");
         for (unsigned i = 0; i < vt->fields.count; i++) {
            if (i > 0)
               printf(", ");
            vcode_dump_one_type(vt->fields.items[i]);
         }
         printf("}");
      }
      break;

   case VCODE_TYPE_FILE:
      printf("F<");
      vcode_dump_one_type(vt->base);
      printf(">");
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

static int vcode_dump_var(vcode_var_t var)
{
   if (MASK_CONTEXT(var) != active_unit->depth) {
      vcode_unit_t owner = active_unit;
      for (int i = 0; i < MASK_CONTEXT(var); i++)
         owner = owner->context;
      return color_printf("$magenta$%s@%s$$", istr(vcode_var_name(var)),
                          istr(owner->name));
   }
   else
      return color_printf("$magenta$%s$$", istr(vcode_var_name(var)));
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
   case VCODE_UNIT_PROCEDURE: printf("procedure"); break;
   }
   color_printf("$$\n");
   if (vu->kind != VCODE_UNIT_CONTEXT)
      color_printf("Context    $cyan$%s$$\n", istr(vu->context->name));
   printf("Blocks     %d\n", vu->blocks.count);
   printf("Registers  %d\n", vu->regs.count);
   printf("Types      %d\n", vu->types.count);
   printf("Variables  %d\n", vu->vars.count);

   for (int i = 0; i < vu->vars.count; i++) {
      const var_t *v = &(vu->vars.items[i]);
      int col = color_printf("  $magenta$%s$$", istr(v->name));
      vcode_dump_type(col, v->type, v->bounds);
      printf("\n");
   }

   if (vu->kind == VCODE_UNIT_CONTEXT) {
      printf("Signals    %d\n", vu->signals.count);

      for (int i = 0; i < vu->signals.count; i++) {
         const signal_t *s = &(vu->signals.items[i]);
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
   else if (vu->kind == VCODE_UNIT_FUNCTION
            || vu->kind == VCODE_UNIT_PROCEDURE) {
      if (vu->result != VCODE_INVALID_TYPE) {
         color_printf("Result     $cyan$");
         vcode_dump_one_type(vu->result);
         color_printf("$$\n");
      }

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
   for (int i = 0; i < vu->blocks.count; i++) {
      const block_t *b = &(vu->blocks.items[i]);
      for (int j = 0; j < b->ops.count; j++) {
         int col = 0;
         if (j == 0)
            col += color_printf("  $yellow$%2d:$$ ", i);
         else
            col += printf("      ");

         const op_t *op = &(b->ops.items[j]);
         switch (op->kind) {
         case VCODE_OP_CMP:
            {
               vcode_dump_reg(op->result);
               printf(" := %s ", vcode_op_string(op->kind));
               vcode_dump_reg(op->args.items[0]);
               switch (op->cmp) {
               case VCODE_CMP_EQ:  printf(" == "); break;
               case VCODE_CMP_NEQ: printf(" != "); break;
               case VCODE_CMP_LT:  printf(" < "); break;
               case VCODE_CMP_GT:  printf(" > "); break;
               case VCODE_CMP_LEQ: printf(" <= "); break;
               case VCODE_CMP_GEQ: printf(" >= "); break;
               }
               vcode_dump_reg(op->args.items[1]);
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

         case VCODE_OP_ALLOCA:
            {
               col += vcode_dump_reg(op->result);
               col += printf(" := %s ", vcode_op_string(op->kind));
               if (op->args.count > 0)
                  col += vcode_dump_reg(op->args.items[0]);
               reg_t *r = vcode_reg_data(op->result);
               vcode_dump_type(col, r->type, r->bounds);
            }
            break;

         case VCODE_OP_FCALL:
         case VCODE_OP_NESTED_FCALL:
            {
               if (op->result != VCODE_INVALID_REG) {
                  col += vcode_dump_reg(op->result);
                  col += printf(" := ");
               }
               col += color_printf("%s $magenta$%s$$ ",
                                   vcode_op_string(op->kind),
                                   istr(op->func));
               for (int i = 0; i < op->args.count; i++) {
                  if (i > 0)
                     col += printf(", ");
                  col += vcode_dump_reg(op->args.items[i]);
               }
               if (op->result != VCODE_INVALID_REG) {
                  reg_t *r = vcode_reg_data(op->result);
                  vcode_dump_type(col, r->type, r->bounds);
               }
            }
            break;

         case VCODE_OP_WAIT:
            {
               color_printf("%s $yellow$%d$$", vcode_op_string(op->kind),
                            op->targets.items[0]);
               if (op->args.items[0] != VCODE_INVALID_REG) {
                  printf(" for ");
                  vcode_dump_reg(op->args.items[0]);
               }
            }
            break;

         case VCODE_OP_JUMP:
            {
               color_printf("%s $yellow$%d$$", vcode_op_string(op->kind),
                            op->targets.items[0]);
            }
            break;

         case VCODE_OP_COND:
            {
               printf("%s ", vcode_op_string(op->kind));
               vcode_dump_reg(op->args.items[0]);
               color_printf(" then $yellow$%d$$ else $yellow$%d$$",
                            op->targets.items[0], op->targets.items[1]);
            }
            break;

         case VCODE_OP_ASSERT:
            {
               printf("%s ", vcode_op_string(op->kind));
               vcode_dump_reg(op->args.items[0]);
               if (op->args.items[2] != VCODE_INVALID_REG) {
                  printf(" report ");
                  vcode_dump_reg(op->args.items[2]);
                  printf(" length ");
                  vcode_dump_reg(op->args.items[3]);
               }
               printf(" severity ");
               vcode_dump_reg(op->args.items[1]);
            }
            break;

         case VCODE_OP_REPORT:
            {
               printf("%s ", vcode_op_string(op->kind));
               vcode_dump_reg(op->args.items[1]);
               printf(" length ");
               vcode_dump_reg(op->args.items[2]);
               printf(" severity ");
               vcode_dump_reg(op->args.items[0]);
            }
            break;

         case VCODE_OP_LOAD:
            {
               col += vcode_dump_reg(op->result);
               col += printf(" := %s ", vcode_op_string(op->kind));
               col += vcode_dump_var(op->address);
               reg_t *r = vcode_reg_data(op->result);
               vcode_dump_type(col, r->type, r->bounds);
            }
            break;

         case VCODE_OP_LOAD_INDIRECT:
            {
               col += vcode_dump_reg(op->result);
               col += color_printf(" := %s ", vcode_op_string(op->kind));
               col += vcode_dump_reg(op->args.items[0]);
               reg_t *r = vcode_reg_data(op->result);
               vcode_dump_type(col, r->type, r->bounds);
            }
            break;

         case VCODE_OP_STORE:
            {
               vcode_dump_var(op->address);
               printf(" := %s ", vcode_op_string(op->kind));
               vcode_dump_reg(op->args.items[0]);
            }
            break;

         case VCODE_OP_STORE_INDIRECT:
            {
               vcode_dump_reg(op->args.items[1]);
               printf(" := %s ", vcode_op_string(op->kind));
               vcode_dump_reg(op->args.items[0]);
            }
            break;

         case VCODE_OP_INDEX:
            {
               col += vcode_dump_reg(op->result);
               col += color_printf(" := %s $magenta$%s$$",
                                   vcode_op_string(op->kind),
                                   istr(vcode_var_data(op->address)->name));
               if (op->args.count > 0) {
                  col += printf(" + ");
                  col += vcode_dump_reg(op->args.items[0]);
               }
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
         case VCODE_OP_OR:
         case VCODE_OP_AND:
         case VCODE_OP_XOR:
         case VCODE_OP_XNOR:
         case VCODE_OP_NAND:
         case VCODE_OP_NOR:
            {
               col += vcode_dump_reg(op->result);
               col += printf(" := %s ", vcode_op_string(op->kind));
               col += vcode_dump_reg(op->args.items[0]);
               switch (op->kind) {
               case VCODE_OP_MUL:  col += printf(" * "); break;
               case VCODE_OP_ADD:  col += printf(" + "); break;
               case VCODE_OP_SUB:  col += printf(" - "); break;
               case VCODE_OP_DIV:  col += printf(" / "); break;
               case VCODE_OP_EXP:  col += printf(" ** "); break;
               case VCODE_OP_MOD:  col += printf(" %% "); break;
               case VCODE_OP_REM:  col += printf(" %% "); break;
               case VCODE_OP_OR:   col += printf(" || "); break;
               case VCODE_OP_AND:  col += printf(" && "); break;
               case VCODE_OP_XOR:  col += printf(" ^ "); break;
               case VCODE_OP_XNOR: col += printf(" !^ "); break;
               case VCODE_OP_NAND: col += printf(" !& "); break;
               case VCODE_OP_NOR:  col += printf(" !| "); break;
               default: break;
               }
               col += vcode_dump_reg(op->args.items[1]);
               reg_t *r = vcode_reg_data(op->result);
               vcode_dump_type(col, r->type, r->bounds);
            }
            break;

         case VCODE_OP_NOT:
            {
               col += vcode_dump_reg(op->result);
               col += printf(" := %s ", vcode_op_string(op->kind));
               col += vcode_dump_reg(op->args.items[0]);
               if (op->result != VCODE_INVALID_REG) {
                  reg_t *r = vcode_reg_data(op->result);
                  vcode_dump_type(col, r->type, r->bounds);
               }
            }
            break;

         case VCODE_OP_BOUNDS:
            {
               vtype_t *vt = vcode_type_data(op->type);
               printf("%s ", vcode_op_string(op->kind));
               vcode_dump_reg(op->args.items[0]);
               if (vt->kind == VCODE_TYPE_INT) {
                  printf(" in ");
                  vcode_pretty_print_int(vt->low);
                  printf(" .. ");
                  vcode_pretty_print_int(vt->high);
               }
               else {
                  printf(" match ");
                  vcode_dump_one_type(op->type);
               }
            }
            break;

         case VCODE_OP_COMMENT:
            {
               color_printf("$cyan$// %s$$ ", op->comment);
            }
            break;

         case VCODE_OP_CONST_ARRAY:
         case VCODE_OP_CONST_RECORD:
            {
               col += vcode_dump_reg(op->result);
               col += printf(" := const %c",
                             op->kind == VCODE_OP_CONST_ARRAY ? '[' : '{');
               for (int k = 0; k < op->args.count; k++) {
                  if (k > 0)
                     col += printf(",");
                  col += vcode_dump_reg(op->args.items[k]);
               }

               putchar(op->kind == VCODE_OP_CONST_ARRAY ? ']' : '}');
               reg_t *r = vcode_reg_data(op->result);
               vcode_dump_type(col + 1, r->type, r->bounds);
            }
            break;

         case VCODE_OP_CAST:
            {
               col += vcode_dump_reg(op->result);
               col += printf(" := %s ", vcode_op_string(op->kind));
               col += vcode_dump_reg(op->args.items[0]);
               reg_t *r = vcode_reg_data(op->result);
               vcode_dump_type(col, r->type, r->bounds);
            }
            break;

         case VCODE_OP_RETURN:
            {
               printf("%s ", vcode_op_string(op->kind));
               if (op->args.count > 0)
                  vcode_dump_reg(op->args.items[0]);
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
               vcode_dump_reg(op->args.items[0]);
               printf(" count ");
               vcode_dump_reg(op->args.items[1]);
               printf(" values ");
               vcode_dump_reg(op->args.items[2]);
               printf(" reject ");
               vcode_dump_reg(op->args.items[3]);
               printf(" after ");
               vcode_dump_reg(op->args.items[4]);
            }
            break;

         case VCODE_OP_NEG:
         case VCODE_OP_ABS:
            {
               col += vcode_dump_reg(op->result);
               col += printf(" := %s ", vcode_op_string(op->kind));
               col += vcode_dump_reg(op->args.items[0]);
               reg_t *r = vcode_reg_data(op->result);
               vcode_dump_type(col, r->type, r->bounds);
            }
            break;

         case VCODE_OP_IMAGE:
            {
               col += vcode_dump_reg(op->result);
               col += printf(" := %s ", vcode_op_string(op->kind));
               col += vcode_dump_reg(op->args.items[0]);
               reg_t *r = vcode_reg_data(op->result);
               vcode_dump_type(col, r->type, r->bounds);
            }
            break;

         case VCODE_OP_SELECT:
            {
               col += vcode_dump_reg(op->result);
               col += printf(" := %s ", vcode_op_string(op->kind));
               col += vcode_dump_reg(op->args.items[0]);
               col += printf(" then ");
               col += vcode_dump_reg(op->args.items[1]);
               col += printf(" else ");
               col += vcode_dump_reg(op->args.items[2]);
               reg_t *r = vcode_reg_data(op->result);
               vcode_dump_type(col, r->type, r->bounds);
            }
            break;

         case VCODE_OP_WRAP:
            {
               col += vcode_dump_reg(op->result);
               col += printf(" := %s ", vcode_op_string(op->kind));
               col += vcode_dump_reg(op->args.items[0]);
               col += printf(" [");
               for (int i = 1; i < op->args.count; i += 3) {
                  if (i > 1)
                     col += printf(", ");
                  col += vcode_dump_reg(op->args.items[i + 0]);
                  col += printf(" ");
                  col += vcode_dump_reg(op->args.items[i + 1]);
                  col += printf(" ");
                  col += vcode_dump_reg(op->args.items[i + 2]);
               }
               col += printf("]");
               if (op->result != VCODE_INVALID_REG) {
                  reg_t *r = vcode_reg_data(op->result);
                  vcode_dump_type(col, r->type, r->bounds);
               }
            }
            break;

         case VCODE_OP_UARRAY_LEFT:
         case VCODE_OP_UARRAY_RIGHT:
         case VCODE_OP_UARRAY_DIR:
            {
               col += vcode_dump_reg(op->result);
               col += printf(" := %s ", vcode_op_string(op->kind));
               col += vcode_dump_reg(op->args.items[0]);
               col += printf(" dim %d", op->dim);
               if (op->result != VCODE_INVALID_REG) {
                  reg_t *r = vcode_reg_data(op->result);
                  vcode_dump_type(col, r->type, r->bounds);
               }
            }
            break;

         case VCODE_OP_UNWRAP:
            {
               col += vcode_dump_reg(op->result);
               col += printf(" := %s ", vcode_op_string(op->kind));
               col += vcode_dump_reg(op->args.items[0]);
               if (op->result != VCODE_INVALID_REG) {
                  reg_t *r = vcode_reg_data(op->result);
                  vcode_dump_type(col, r->type, r->bounds);
               }
            }
            break;

         case VCODE_OP_PHI:
            {
               col += vcode_dump_reg(op->result);
               col += printf(" := %s", vcode_op_string(op->kind));
               for (int i = 0; i < op->args.count; i++) {
                  col += printf(" [");
                  col += vcode_dump_reg(op->args.items[i]);
                  col += color_printf(" $yellow$%d$$]", op->targets.items[i]);
               }
               if (op->result != VCODE_INVALID_REG) {
                  reg_t *r = vcode_reg_data(op->result);
                  vcode_dump_type(col, r->type, r->bounds);
               }
            }
            break;

         case VCODE_OP_PARAM_UPREF:
            {
               col += vcode_dump_reg(op->result);
               col += printf(" := %s %d, ", vcode_op_string(op->kind),
                             op->hops);
               col += vcode_dump_reg(op->args.items[0]);
               if (op->result != VCODE_INVALID_REG) {
                  reg_t *r = vcode_reg_data(op->result);
                  vcode_dump_type(col, r->type, r->bounds);
               }
            }
            break;

         case VCODE_OP_RESOLVED_ADDRESS:
            {
               vcode_dump_var(op->address);
               color_printf(" := %s $white$%s$$", vcode_op_string(op->kind),
                            istr(vcode_signal_name(op->signal)));
            }
            break;

         case VCODE_OP_SET_INITIAL:
            {
               color_printf("$white$%s$$ := %s ",
                            istr(vcode_signal_name(op->signal)),
                            vcode_op_string(op->kind));
               vcode_dump_reg(op->args.items[0]);
            }
            break;

         case VCODE_OP_ALLOC_DRIVER:
            {
               printf("%s nets ", vcode_op_string(op->kind));
               vcode_dump_reg(op->args.items[0]);
               printf("+");
               vcode_dump_reg(op->args.items[1]);
               printf(" driven ");
               vcode_dump_reg(op->args.items[2]);
               printf("+");
               vcode_dump_reg(op->args.items[3]);
               if (op->args.items[4] != VCODE_INVALID_REG) {
                  printf(" init ");
                  vcode_dump_reg(op->args.items[4]);
               }
            }
            break;

         case VCODE_OP_ACTIVE:
         case VCODE_OP_EVENT:
            {
               col += vcode_dump_reg(op->result);
               col += printf(" := %s ", vcode_op_string(op->kind));
               col += vcode_dump_reg(op->args.items[0]);
               col += printf(" length ");
               col += vcode_dump_reg(op->args.items[1]);
               if (op->result != VCODE_INVALID_REG) {
                  reg_t *r = vcode_reg_data(op->result);
                  vcode_dump_type(col, r->type, r->bounds);
               }
            }
            break;

         case VCODE_OP_RECORD_REF:
            {
               col += vcode_dump_reg(op->result);
               col += printf(" := %s ", vcode_op_string(op->kind));
               col += vcode_dump_reg(op->args.items[0]);
               col += printf(" field %d", op->field);
               if (op->result != VCODE_INVALID_REG) {
                  reg_t *r = vcode_reg_data(op->result);
                  vcode_dump_type(col, r->type, r->bounds);
               }
            }
            break;

         case VCODE_OP_COPY:
            {
               vcode_dump_reg(op->args.items[0]);
               printf(" := %s ", vcode_op_string(op->kind));
               vcode_dump_reg(op->args.items[1]);
               printf(" count " );
               vcode_dump_reg(op->args.items[2]);
            }
            break;

         case VCODE_OP_SCHED_EVENT:
            {
               printf("%s on ", vcode_op_string(op->kind));
               vcode_dump_reg(op->args.items[0]);
               printf(" count ");
               vcode_dump_reg(op->args.items[1]);
               printf(" flags %x", op->flags);
            }
            break;

         case VCODE_OP_PCALL:
            {
               color_printf("%s $magenta$%s$$", vcode_op_string(op->kind),
                            istr(op->func));
               for (int i = 0; i < op->args.count; i++) {
                  printf("%s", i > 0 ? ", " : " ");
                  vcode_dump_reg(op->args.items[i]);
               }
               if (op->targets.count > 0)
                  color_printf(" resume $yellow$%d$$", op->targets.items[0]);
            }
            break;

         case VCODE_OP_RESUME:
            {
               color_printf("%s $magenta$%s$$", vcode_op_string(op->kind),
                            istr(op->func));
            }
            break;

         case VCODE_OP_MEMCMP:
            {
               col += vcode_dump_reg(op->result);
               col += printf(" := %s ", vcode_op_string(op->kind));
               col += vcode_dump_reg(op->args.items[0]);
               col += printf(" == " );
               col += vcode_dump_reg(op->args.items[1]);
               col += printf(" length ");
               col += vcode_dump_reg(op->args.items[2]);
               if (op->result != VCODE_INVALID_REG) {
                  reg_t *r = vcode_reg_data(op->result);
                  vcode_dump_type(col, r->type, r->bounds);
               }
            }
            break;

         case VCODE_OP_MEMSET:
            {
               vcode_dump_reg(op->args.items[0]);
               printf(" := %s ", vcode_op_string(op->kind));
               vcode_dump_reg(op->args.items[1]);
               printf(" length ");
               vcode_dump_reg(op->args.items[2]);
            }
            break;

         case VCODE_OP_VEC_LOAD:
            {
               col += vcode_dump_reg(op->result);
               col += printf(" := %s ", vcode_op_string(op->kind));
               col += vcode_dump_reg(op->args.items[0]);
               col += printf(" length ");
               col += vcode_dump_reg(op->args.items[1]);
               if (op->result != VCODE_INVALID_REG) {
                  reg_t *r = vcode_reg_data(op->result);
                  vcode_dump_type(col, r->type, r->bounds);
               }
            }
            break;

         case VCODE_OP_CASE:
            {
               printf("%s ", vcode_op_string(op->kind));
               vcode_dump_reg(op->args.items[0]);
               color_printf(" default $yellow$%d$$", op->targets.items[0]);
               for (int i = 1; i < op->args.count; i++) {
                  printf(" [");
                  vcode_dump_reg(op->args.items[i]);
                  color_printf(" $yellow$%d$$]", op->targets.items[i]);
               }
            }
            break;

         case VCODE_OP_ENDFILE:
            {
               col += vcode_dump_reg(op->result);
               col += printf(" := %s ", vcode_op_string(op->kind));
               col += vcode_dump_reg(op->args.items[0]);
               if (op->result != VCODE_INVALID_REG) {
                  reg_t *r = vcode_reg_data(op->result);
                  vcode_dump_type(col, r->type, r->bounds);
               }
            }
            break;

         case VCODE_OP_FILE_OPEN:
            {
               printf("%s ", vcode_op_string(op->kind));
               vcode_dump_reg(op->args.items[0]);
               printf(" name ");
               vcode_dump_reg(op->args.items[1]);
               printf(" length ");
               vcode_dump_reg(op->args.items[2]);
               printf(" kind ");
               vcode_dump_reg(op->args.items[3]);
               if (op->args.count == 5) {
                  printf(" status ");
                  vcode_dump_reg(op->args.items[4]);
               }
            }
            break;

         case VCODE_OP_FILE_CLOSE:
            {
               printf("%s ", vcode_op_string(op->kind));
               vcode_dump_reg(op->args.items[0]);
            }
            break;

         case VCODE_OP_FILE_WRITE:
            {
               printf("%s ", vcode_op_string(op->kind));
               vcode_dump_reg(op->args.items[0]);
               printf(" value ");
               vcode_dump_reg(op->args.items[1]);
               if (op->args.count == 3) {
                  printf(" length ");
                  vcode_dump_reg(op->args.items[2]);
               }
            }
            break;

         case VCODE_OP_FILE_READ:
            {
               printf("%s ", vcode_op_string(op->kind));
               vcode_dump_reg(op->args.items[0]);
               printf(" ptr ");
               vcode_dump_reg(op->args.items[1]);
               if (op->args.count == 3) {
                  printf(" length ");
                  vcode_dump_reg(op->args.items[2]);
               }
            }
            break;
         }

         printf("\n");
      }

      if (b->ops.count == 0)
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
               if (at->dims.count != bt->dims.count)
                  return false;

               for (unsigned i = 0; i < at->dims.count; i++) {
                  if (!vtype_eq(at->dims.items[i], bt->dims.items[i]))
                     return false;
               }

               return vtype_eq(at->elem, bt->elem);
            }
         case VCODE_TYPE_UARRAY:
            return at->dims.count == bt->dims.count
               && vtype_eq(at->elem, bt->elem);

         case VCODE_TYPE_POINTER:
            return vtype_eq(at->pointed, bt->pointed);
         case VCODE_TYPE_OFFSET:
            return true;
         case VCODE_TYPE_SIGNAL:
         case VCODE_TYPE_FILE:
            return vtype_eq(at->base, bt->base);
         case VCODE_TYPE_RECORD:
            {
               if (at->fields.count != bt->fields.count)
                  return false;

               for (unsigned i = 0; i < at->fields.count; i++) {
                  if (!vtype_eq(at->fields.items[i], bt->fields.items[i]))
                     return false;
               }

               return true;
            }
         }
      }

      return false;
   }
}

bool vtype_includes(vcode_type_t type, vcode_type_t bounds)
{
   const vtype_t *tt = vcode_type_data(type);
   const vtype_t *bt = vcode_type_data(bounds);

   if (bt->kind == VCODE_TYPE_UARRAY || tt->kind == VCODE_TYPE_UARRAY)
      return false;
   else if (bt->kind != tt->kind)
      return false;

   switch (bt->kind) {
   case VCODE_TYPE_INT:
      return bt->low >= tt->low && bt->high <= tt->high;

   case VCODE_TYPE_CARRAY:
   case VCODE_TYPE_UARRAY:
   case VCODE_TYPE_RECORD:
      return vtype_eq(type, bounds);

   case VCODE_TYPE_POINTER:
   case VCODE_TYPE_OFFSET:
   case VCODE_TYPE_FILE:
      return false;

   case VCODE_TYPE_SIGNAL:
      return vtype_includes(tt->base, bt->base);
   }

   return false;
}

static vcode_type_t vtype_new(vtype_t *new)
{
   int index = active_unit->types.count - 1;
   vcode_type_t type = MAKE_HANDLE(active_unit->depth, index);

   for (int i = 0; i < index; i++) {
      vcode_type_t this = MAKE_HANDLE(active_unit->depth, i);
      if (vtype_eq(this, type)) {
         active_unit->types.count--;
         return this;
      }
   }

   return type;
}

vcode_type_t vtype_int(int64_t low, int64_t high)
{
   assert(active_unit != NULL);

   vtype_t *n = vtype_array_alloc(&(active_unit->types));
   n->kind = VCODE_TYPE_INT;
   n->low  = low;
   n->high = high;

   return vtype_new(n);
}

vcode_type_t vtype_bool(void)
{
   return vtype_int(0, 1);
}

vcode_type_t vtype_carray(const vcode_type_t *dim, int ndim,
                          vcode_type_t elem, vcode_type_t bounds)
{
   assert(active_unit != NULL);

   vtype_t *n = vtype_array_alloc(&(active_unit->types));
   memset(n, '\0', sizeof(vtype_t));
   n->kind   = VCODE_TYPE_CARRAY;
   n->elem   = elem;
   n->bounds = bounds;
   for (int i = 0; i < ndim; i++)
      vcode_type_array_add(&(n->dims), dim[i]);

   return vtype_new(n);
}

vcode_type_t vtype_record(const vcode_type_t *field_types, int nfields)
{
   assert(active_unit != NULL);

   vtype_t *n = vtype_array_alloc(&(active_unit->types));
   memset(n, '\0', sizeof(vtype_t));
   n->kind = VCODE_TYPE_RECORD;
   for (int i = 0; i < nfields; i++)
      vcode_type_array_add(&(n->fields), field_types[i]);

   return vtype_new(n);
}

vcode_type_t vtype_uarray(const vcode_type_t *dim_types, int ndim,
                          vcode_type_t elem, vcode_type_t bounds)
{
   assert(active_unit != NULL);

   vtype_t *n = vtype_array_alloc(&(active_unit->types));
   memset(n, '\0', sizeof(vtype_t));
   n->kind   = VCODE_TYPE_UARRAY;
   n->elem   = elem;
   n->bounds = bounds;
   for (int i = 0; i < ndim; i++)
      vcode_type_array_add(&(n->dims), dim_types[i]);

   return vtype_new(n);
}

vcode_type_t vtype_pointer(vcode_type_t to)
{
   assert(active_unit != NULL);

   vtype_t *n = vtype_array_alloc(&(active_unit->types));
   n->kind    = VCODE_TYPE_POINTER;
   n->pointed = to;

   return vtype_new(n);
}

vcode_type_t vtype_signal(vcode_type_t base)
{
   assert(active_unit != NULL);

   vtype_t *n = vtype_array_alloc(&(active_unit->types));
   n->kind = VCODE_TYPE_SIGNAL;
   n->base = base;

   return vtype_new(n);
}

vcode_type_t vtype_file(vcode_type_t base)
{
   assert(active_unit != NULL);

   vtype_t *n = vtype_array_alloc(&(active_unit->types));
   n->kind = VCODE_TYPE_FILE;
   n->base = base;

   return vtype_new(n);
}

vcode_type_t vtype_offset(void)
{
   assert(active_unit != NULL);

   vtype_t *n = vtype_array_alloc(&(active_unit->types));
   n->kind = VCODE_TYPE_OFFSET;
   n->low  = INT64_MIN;
   n->high = INT64_MAX;

   return vtype_new(n);
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

vcode_type_t vtype_base(vcode_type_t type)
{
   vtype_t *vt = vcode_type_data(type);
   assert(vt->kind == VCODE_TYPE_SIGNAL || vt->kind == VCODE_TYPE_FILE);
   return vt->base;
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
   return vt->dims.count;
}

vcode_type_t vtype_dim(vcode_type_t type, int dim)
{
   vtype_t *vt = vcode_type_data(type);
   assert(vt->kind == VCODE_TYPE_CARRAY);
   return vcode_type_array_nth(&(vt->dims), dim);
}

int vtype_fields(vcode_type_t type)
{
   vtype_t *vt = vcode_type_data(type);
   assert(vt->kind == VCODE_TYPE_RECORD);
   return vt->fields.count;
}

vcode_type_t vtype_field(vcode_type_t type, int field)
{
   vtype_t *vt = vcode_type_data(type);
   assert(vt->kind == VCODE_TYPE_RECORD);
   return vcode_type_array_nth(&(vt->fields), field);
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

static bool vtype_is_pointer(vcode_type_t type, vtype_kind_t to)
{
   return vtype_kind(type) == VCODE_TYPE_POINTER
      && vtype_kind(vtype_pointed(type)) == to;
}

int vcode_count_params(void)
{
   assert(active_unit != NULL);
   assert(active_unit->kind == VCODE_UNIT_FUNCTION
          || active_unit->kind == VCODE_UNIT_PROCEDURE);

   return active_unit->params.count;
}

vcode_type_t vcode_param_type(int param)
{
   assert(active_unit != NULL);
   assert(active_unit->kind == VCODE_UNIT_FUNCTION
          || active_unit->kind == VCODE_UNIT_PROCEDURE);
   assert(param < active_unit->params.count);

   return active_unit->params.items[param].type;
}

vcode_reg_t vcode_param_reg(int param)
{
   assert(active_unit != NULL);
   assert(active_unit->kind == VCODE_UNIT_FUNCTION
          || active_unit->kind == VCODE_UNIT_PROCEDURE);
   assert(param < active_unit->params.count);

   return active_unit->params.items[param].reg;
}

vcode_block_t emit_block(void)
{
   assert(active_unit != NULL);

   vcode_block_t bnum = active_unit->blocks.count;

   block_t *bptr = block_array_alloc(&(active_unit->blocks));
   memset(bptr, '\0', sizeof(block_t));

   return bnum;
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

vcode_block_t vcode_active_block(void)
{
   assert(active_unit != NULL);
   assert(active_block != -1);
   return active_block;
}

vcode_unit_t vcode_active_unit(void)
{
   assert(active_unit != NULL);
   return active_unit;
}

ident_t vcode_unit_name(void)
{
   assert(active_unit != NULL);
   return active_unit->name;
}

int vcode_unit_depth(void)
{
   assert(active_unit != NULL);
   return active_unit->depth;
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

vcode_unit_t vcode_unit_context(void)
{
   assert(active_unit != NULL);
   return active_unit->context;
}

static unsigned vcode_unit_calc_depth(vcode_unit_t unit)
{
   int hops = 0;
   for (; unit->kind != VCODE_UNIT_CONTEXT; unit = unit->context)
      hops++;
   return hops;
}

vcode_unit_t emit_function(ident_t name, vcode_unit_t context,
                           vcode_type_t result)
{
   vcode_unit_t vu = xcalloc(sizeof(struct vcode_unit));
   vu->kind    = VCODE_UNIT_FUNCTION;
   vu->name    = name;
   vu->context = context;
   vu->result  = result;
   vu->depth   = vcode_unit_calc_depth(vu);

   active_unit = vu;
   vcode_select_block(emit_block());

   return vu;
}

vcode_unit_t emit_procedure(ident_t name, vcode_unit_t context)
{
   vcode_unit_t vu = xcalloc(sizeof(struct vcode_unit));
   vu->kind    = VCODE_UNIT_PROCEDURE;
   vu->name    = name;
   vu->context = context;
   vu->result  = VCODE_INVALID_TYPE;
   vu->depth   = vcode_unit_calc_depth(vu);

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
   vu->depth   = vcode_unit_calc_depth(vu);

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

void emit_assert(vcode_reg_t value, vcode_reg_t message, vcode_reg_t length,
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
   vcode_add_arg(op, length);
   op->index = index;

   if (!vtype_eq(vcode_reg_type(value), vtype_bool())) {
      vcode_dump();
      fatal_trace("value parameter to assert is not bool");
   }
}

void emit_report(vcode_reg_t message, vcode_reg_t length, vcode_reg_t severity,
                 uint32_t index)
{
   op_t *op = vcode_add_op(VCODE_OP_REPORT);
   vcode_add_arg(op, severity);
   vcode_add_arg(op, message);
   vcode_add_arg(op, length);
   op->index = index;
}

vcode_reg_t emit_cmp(vcode_cmp_t cmp, vcode_reg_t lhs, vcode_reg_t rhs)
{
   if (lhs == rhs) {
      if (cmp == VCODE_CMP_EQ)
         return emit_const(vtype_bool(), 1);
      else if (cmp == VCODE_CMP_NEQ)
         return emit_const(vtype_bool(), 0);
   }

   int64_t lconst, rconst;
   if (vcode_reg_const(lhs, &lconst) && vcode_reg_const(rhs, &rconst)) {
      switch (cmp) {
      case VCODE_CMP_EQ:
         return emit_const(vtype_bool(), lconst == rconst);
      case VCODE_CMP_NEQ:
         return emit_const(vtype_bool(), lconst != rconst);
      case VCODE_CMP_LT:
         return emit_const(vtype_bool(), lconst < rconst);
      case VCODE_CMP_GT:
         return emit_const(vtype_bool(), lconst > rconst);
      case VCODE_CMP_LEQ:
         return emit_const(vtype_bool(), lconst <= rconst);
      case VCODE_CMP_GEQ:
         return emit_const(vtype_bool(), lconst >= rconst);
      default:
         fatal_trace("cannot fold comparison %d", cmp);
      }
   }

   // Reuse any previous operation in this block with the same arguments
   block_t *b = &(active_unit->blocks.items[active_block]);
   for (int i = b->ops.count - 1; i >= 0; i--) {
      const op_t *op = &(b->ops.items[i]);
      if (op->kind == VCODE_OP_CMP && op->args.count == 2 && op->args.items[0] == lhs
          && op->args.items[1] == rhs && op->cmp == cmp)
         return op->result;
   }

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

static vcode_reg_t emit_fcall_op(vcode_op_t op, ident_t func, vcode_type_t type,
                                 const vcode_reg_t *args, int nargs,
                                 vcode_block_t resume_bb)
{
   op_t *o = vcode_add_op(op);
   o->func = func;
   o->type = type;
   for (int i = 0; i < nargs; i++)
      vcode_add_arg(o, args[i]);

   if (resume_bb != VCODE_INVALID_BLOCK)
      vcode_block_array_add(&(o->targets), resume_bb);

   if (type == VCODE_INVALID_TYPE)
      return (o->result = VCODE_INVALID_REG);
   else
      return (o->result = vcode_add_reg(type));
}

vcode_reg_t emit_fcall(ident_t func, vcode_type_t type,
                       const vcode_reg_t *args, int nargs)
{
   return emit_fcall_op(VCODE_OP_FCALL, func, type, args, nargs,
                        VCODE_INVALID_BLOCK);
}

vcode_reg_t emit_nested_fcall(ident_t func, vcode_type_t type,
                              const vcode_reg_t *args, int nargs)
{
   return emit_fcall_op(VCODE_OP_NESTED_FCALL, func, type, args, nargs,
                        VCODE_INVALID_BLOCK);
}

void emit_pcall(ident_t func, const vcode_reg_t *args, int nargs,
                vcode_block_t resume_bb)
{
   emit_fcall_op(VCODE_OP_PCALL, func, VCODE_INVALID_TYPE, args, nargs,
                 resume_bb);
}

vcode_reg_t emit_alloca(vcode_type_t type, vcode_type_t bounds,
                        vcode_reg_t count)
{
   op_t *op = vcode_add_op(VCODE_OP_ALLOCA);
   op->type   = type;
   op->result = vcode_add_reg(vtype_pointer(type));

   if (count != VCODE_INVALID_REG)
      vcode_add_arg(op, count);

   reg_t *r = vcode_reg_data(op->result);
   r->bounds = bounds;

   return op->result;
}

vcode_reg_t emit_const(vcode_type_t type, int64_t value)
{
   // Reuse any previous constant in this block with the same type and value
   block_t *b = &(active_unit->blocks.items[active_block]);
   for (int i = b->ops.count - 1; i >= 0; i--) {
      const op_t *op = &(b->ops.items[i]);
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

vcode_reg_t emit_const_array(vcode_type_t type, vcode_reg_t *values, int num,
                             bool allocate)
{
   vcode_type_t rtype = allocate && vtype_kind(type) == VCODE_TYPE_CARRAY
      ? vtype_pointer(vtype_elem(type))
      : type;

   // Reuse any previous operation in this block with the same arguments
   op_t *other = NULL;
   while (vcode_dominating_ops(VCODE_OP_CONST_ARRAY, &other)) {
      if (other->args.count == num && vtype_eq(type, other->type)) {
         bool match = true;
         for (int i = 0; match && i < num; i++) {
            if (other->args.items[i] != values[i])
               match = false;
         }

         if (match && vtype_eq(vcode_reg_type(other->result), rtype))
            return other->result;
      }
   }

   op_t *op = vcode_add_op(VCODE_OP_CONST_ARRAY);
   op->type   = type;
   op->result = vcode_add_reg(rtype);

   for (int i = 0; i < num; i++)
      vcode_add_arg(op, values[i]);

   vtype_kind_t kind = vtype_kind(type);
   if (kind != VCODE_TYPE_CARRAY && (!allocate || kind != VCODE_TYPE_POINTER)) {
      vcode_dump();
      fatal_trace("constant array must have constrained array type");
   }

   return op->result;
}

vcode_reg_t emit_const_record(vcode_type_t type, vcode_reg_t *values, int num,
                              bool allocate)
{
   vcode_type_t rtype = allocate ? vtype_pointer(type) : type;

   // Reuse any previous constant in this block with the same type and value
   op_t *other = NULL;
   while (vcode_dominating_ops(VCODE_OP_CONST_RECORD, &other)) {
      if (vtype_eq(type, other->type) && other->args.count == num) {
         bool same_regs = true;
         for (int i = 0; i < num; i++)
            same_regs = same_regs && other->args.items[i] == values[i];

         if (same_regs && vtype_eq(vcode_reg_type(other->result), rtype))
            return other->result;
      }
   }

   op_t *op = vcode_add_op(VCODE_OP_CONST_RECORD);
   op->type   = type;
   op->result = vcode_add_reg(rtype);

   for (int i = 0; i < num; i++)
      vcode_add_arg(op, values[i]);

   if (vtype_kind(type) != VCODE_TYPE_RECORD) {
      vcode_dump();
      fatal_trace("constant record must have record type");
   }

   return op->result;
}

void emit_wait(vcode_block_t target, vcode_reg_t time)
{
   op_t *op = vcode_add_op(VCODE_OP_WAIT);
   vcode_add_target(op, target);
   vcode_add_arg(op, time);
}

void emit_jump(vcode_block_t target)
{
   op_t *op = vcode_add_op(VCODE_OP_JUMP);
   vcode_add_target(op, target);
}

vcode_var_t emit_var(vcode_type_t type, vcode_type_t bounds, ident_t name)
{
   assert(active_unit != NULL);

   vcode_var_t var = active_unit->vars.count;
   var_t *v = var_array_alloc(&(active_unit->vars));
   memset(v, '\0', sizeof(var_t));
   v->type   = type;
   v->bounds = bounds;
   v->name   = name;

   return MAKE_HANDLE(active_unit->depth, var);
}

vcode_reg_t emit_param(vcode_type_t type, vcode_type_t bounds, ident_t name)
{
   assert(active_unit != NULL);

   param_t *p = param_array_alloc(&(active_unit->params));
   memset(p, '\0', sizeof(param_t));
   p->type   = type;
   p->bounds = bounds;
   p->name   = name;
   p->reg    = vcode_add_reg(type);

   reg_t *rr = vcode_reg_data(p->reg);
   rr->bounds = bounds;

   return p->reg;
}

vcode_signal_t emit_signal(vcode_type_t type, vcode_type_t bounds,
                           ident_t name, vcode_var_t shadow,
                           netid_t *nets, size_t nnets)
{
   assert(active_unit != NULL);
   assert(active_unit->kind == VCODE_UNIT_CONTEXT);

   vcode_signal_t snum = active_unit->signals.count;

   signal_t *s = signal_array_alloc(&(active_unit->signals));
   memset(s, '\0', sizeof(signal_t));
   s->type   = type;
   s->bounds = bounds;
   s->name   = name;
   s->shadow = shadow;
   s->nets   = nets;
   s->nnets  = nnets;

   return snum;
}

vcode_reg_t emit_load(vcode_var_t var)
{
   // Try scanning backwards through the block for another load or store to
   // this variable
   block_t *b = vcode_block_data();
   vcode_reg_t fold = VCODE_INVALID_REG;
   bool aliased = false;
   for (int i = b->ops.count - 1; i >= 0; i--) {
      const op_t *op = &(b->ops.items[i]);
      if (fold == VCODE_INVALID_REG) {
         if (op->kind == VCODE_OP_LOAD && op->address == var)
            fold = op->result;
         else if (op->kind == VCODE_OP_STORE && op->address == var)
            fold = op->args.items[0];
      }

      if (op->kind == VCODE_OP_INDEX && op->address == var)
         aliased = true;
   }

   if (fold != VCODE_INVALID_REG && !aliased)
      return fold;

   var_t *v = vcode_var_data(var);

   op_t *op = vcode_add_op(VCODE_OP_LOAD);
   op->address = var;
   op->result  = vcode_add_reg(v->type);

   vtype_kind_t type_kind = vtype_kind(v->type);
   if (type_kind != VCODE_TYPE_INT && type_kind != VCODE_TYPE_OFFSET
       && type_kind != VCODE_TYPE_UARRAY && type_kind != VCODE_TYPE_POINTER
       && type_kind != VCODE_TYPE_FILE) {
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

   vtype_kind_t type_kind = vtype_kind(deref);
   if (type_kind != VCODE_TYPE_INT && type_kind != VCODE_TYPE_OFFSET
       && type_kind != VCODE_TYPE_UARRAY && type_kind != VCODE_TYPE_POINTER) {
      vcode_dump();
      fatal_trace("cannot load non-scalar type");
   }

   vcode_reg_data(op->result)->bounds = vcode_reg_data(reg)->bounds;

   return op->result;
}

void emit_store(vcode_reg_t reg, vcode_var_t var)
{
   // Any previous store to this variable in this block is dead
   block_t *b = vcode_block_data();
   for (int i = 0; i < b->ops.count; i++) {
      op_t *op = &(b->ops.items[i]);
      if (op->kind == VCODE_OP_STORE && op->address == var) {
         op->kind = VCODE_OP_COMMENT;
         op->comment = xasprintf("Dead store to %s", istr(vcode_var_name(var)));
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

   if (vtype_kind(p->type) != VCODE_TYPE_POINTER) {
      vcode_dump();
      fatal_trace("store indirect target is not a pointer");
   }
   else if (!vtype_eq(vtype_pointed(p->type), r->type)) {
      vcode_dump();
      fatal_trace("pointer and stored value do not have same type");
   }
}

static vcode_reg_t emit_arith(vcode_op_t kind, vcode_reg_t lhs, vcode_reg_t rhs,
                              uint32_t index)
{
   // Reuse any previous operation in this block with the same arguments
   op_t *other = NULL;
   while (vcode_dominating_ops(kind, &other)) {
      if (other->args.count == 2 && other->args.items[0] == lhs
          && other->args.items[1] == rhs)
         return other->result;
   }

   op_t *op = vcode_add_op(kind);
   vcode_add_arg(op, lhs);
   vcode_add_arg(op, rhs);
   op->result = vcode_add_reg(vcode_reg_type(lhs));
   op->index = index;

   vcode_type_t lhs_type = vcode_reg_type(lhs);
   vcode_type_t rhs_type = vcode_reg_type(rhs);

   const vtype_kind_t ltypek = vtype_kind(vcode_reg_type(lhs));
   const bool is_pointer =
      ltypek == VCODE_TYPE_POINTER || ltypek == VCODE_TYPE_SIGNAL;

   if (is_pointer && vtype_kind(rhs_type) == VCODE_TYPE_OFFSET)
      ;
   else if (!vtype_eq(lhs_type, rhs_type))
      vcode_fail("arguments to %s are not the same type",
                 vcode_op_string(kind));

   return op->result;
}

vcode_reg_t emit_mul(vcode_reg_t lhs, vcode_reg_t rhs)
{
   int64_t lconst, rconst;
   if (vcode_reg_const(lhs, &lconst) && vcode_reg_const(rhs, &rconst))
      return emit_const(vcode_reg_type(lhs), lconst * rconst);

   vcode_reg_t reg = emit_arith(VCODE_OP_MUL, lhs, rhs, UINT32_MAX);

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

vcode_reg_t emit_div(vcode_reg_t lhs, vcode_reg_t rhs, uint32_t index)
{
   int64_t lconst, rconst;
   if (vcode_reg_const(lhs, &lconst) && vcode_reg_const(rhs, &rconst))
      return emit_const(vcode_reg_type(lhs), lconst / rconst);

   return emit_arith(VCODE_OP_DIV, lhs, rhs, index);
}

vcode_reg_t emit_exp(vcode_reg_t lhs, vcode_reg_t rhs)
{
   int64_t lconst, rconst;
   if (vcode_reg_const(lhs, &lconst) && vcode_reg_const(rhs, &rconst))
      return emit_const(vcode_reg_type(lhs), ipow(lconst, rconst));

   return emit_arith(VCODE_OP_EXP, lhs, rhs, UINT32_MAX);
}

vcode_reg_t emit_mod(vcode_reg_t lhs, vcode_reg_t rhs)
{
   int64_t lconst, rconst;
   if (vcode_reg_const(lhs, &lconst) && vcode_reg_const(rhs, &rconst)
       && lconst > 0 && rconst > 0)
      return emit_const(vcode_reg_type(lhs), lconst % rconst);

   return emit_arith(VCODE_OP_MOD, lhs, rhs, UINT32_MAX);
}

vcode_reg_t emit_rem(vcode_reg_t lhs, vcode_reg_t rhs)
{
   int64_t lconst, rconst;
   if (vcode_reg_const(lhs, &lconst) && vcode_reg_const(rhs, &rconst)
       && lconst > 0 && rconst > 0)
      return emit_const(vcode_reg_type(lhs), lconst % rconst);

   return emit_arith(VCODE_OP_REM, lhs, rhs, UINT32_MAX);
}

vcode_reg_t emit_add(vcode_reg_t lhs, vcode_reg_t rhs)
{
   const vtype_kind_t ltypek = vtype_kind(vcode_reg_type(lhs));
   const bool is_pointer =
      ltypek == VCODE_TYPE_POINTER || ltypek == VCODE_TYPE_SIGNAL;

   int64_t lconst, rconst;
   if (vcode_reg_const(lhs, &lconst) && vcode_reg_const(rhs, &rconst))
      return emit_const(vcode_reg_type(lhs), lconst + rconst);
   else if (is_pointer && vcode_reg_const(rhs, &rconst) && rconst == 0)
      return lhs;

   vcode_reg_t reg = emit_arith(VCODE_OP_ADD, lhs, rhs, UINT32_MAX);

   reg_t *rr = vcode_reg_data(reg);
   if (is_pointer)
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

   vcode_reg_t reg = emit_arith(VCODE_OP_SUB, lhs, rhs, UINT32_MAX);

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
   if (reg == VCODE_INVALID_REG)
      return;
   else if (vtype_includes(bounds, vcode_reg_data(reg)->bounds)) {
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
   op_t *other = NULL;
   while (vcode_dominating_ops(VCODE_OP_INDEX, &other)) {
      if (other->address == var
          && ((offset == VCODE_INVALID_REG && other->args.count == 0)
              || (offset != VCODE_INVALID_REG
                  && other->args.items[0] == offset)))
         return other->result;
   }

   op_t *op = vcode_add_op(VCODE_OP_INDEX);
   op->address = var;

   if (offset != VCODE_INVALID_REG)
      vcode_add_arg(op, offset);

   vcode_type_t typeref = vcode_var_type(var);
   vtype_t *vt = vcode_type_data(typeref);
   switch (vt->kind) {
   case VCODE_TYPE_CARRAY:
      op->type = vtype_pointer(vt->elem);
      op->result = vcode_add_reg(op->type);
      vcode_reg_data(op->result)->bounds = vt->bounds;
      break;

   case VCODE_TYPE_RECORD:
      op->type = vtype_pointer(typeref);
      op->result = vcode_add_reg(op->type);
      break;

   case VCODE_TYPE_INT:
   case VCODE_TYPE_FILE:
      op->type = vtype_pointer(typeref);
      op->result = vcode_add_reg(op->type);
      break;

   default:
      vcode_fail("variable %s cannot be indexed", istr(vcode_var_name(var)));
   }

   if (offset != VCODE_INVALID_REG) {
      if (vtype_kind(vcode_reg_type(offset)) != VCODE_TYPE_OFFSET)
         vcode_fail("index offset r%d does not have offset type", offset);
   }

   return op->result;
}

vcode_reg_t emit_cast(vcode_type_t type, vcode_reg_t reg)
{
   if (vtype_eq(vcode_reg_type(reg), type))
      return reg;

   int64_t value;
   if (vcode_reg_const(reg, &value))
      return emit_const(type, value);

   // Try to find a previous cast of this register to this type
   op_t *other = NULL;
   while (vcode_dominating_ops(VCODE_OP_CAST, &other)) {
      if (vtype_eq(other->type, type) && other->args.items[0] == reg)
         return other->result;
   }

   op_t *op = vcode_add_op(VCODE_OP_CAST);
   vcode_add_arg(op, reg);
   op->type   = type;
   op->result = vcode_add_reg(type);

   static const vcode_type_t allowed[][2] = {
      { VCODE_TYPE_INT,    VCODE_TYPE_OFFSET  },
      { VCODE_TYPE_CARRAY, VCODE_TYPE_POINTER },
      { VCODE_TYPE_OFFSET, VCODE_TYPE_INT     },
   };

   vtype_kind_t from = vtype_kind(vcode_reg_type(reg));
   vtype_kind_t to   = vtype_kind(type);

   if (from == VCODE_TYPE_INT) {
      reg_t *rr = vcode_reg_data(op->result);
      rr->bounds = vcode_reg_bounds(reg);
   }

   for (size_t i = 0; i < ARRAY_LEN(allowed); i++) {
      if (from == allowed[i][0] && to == allowed[i][1])
         return op->result;
   }

   vcode_fail("invalid type conversion in cast");
}

void emit_return(vcode_reg_t reg)
{
   op_t *op = vcode_add_op(VCODE_OP_RETURN);
   if (reg != VCODE_INVALID_REG) {
      vcode_add_arg(op, reg);

      if (active_unit->kind != VCODE_UNIT_FUNCTION) {
         vcode_dump();
         fatal_trace("returning value fron non-function unit");
      }
      else if (!vtype_eq(active_unit->result, vcode_reg_type(reg))) {
         vcode_dump();
         fatal_trace("return value incorrect type");
      }
   }
}

vcode_reg_t emit_nets(vcode_signal_t sig)
{
   block_t *b = &(active_unit->blocks.items[active_block]);
   for (int i = b->ops.count - 1; i >= 0; i--) {
      const op_t *op = &(b->ops.items[i]);
      if (op->kind == VCODE_OP_NETS && op->signal == sig)
         return op->result;
   }

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

   if (vtype_kind(vcode_reg_type(nets)) != VCODE_TYPE_SIGNAL)
      vcode_fail("sched_waveform target is not signal");
   else if (vtype_kind(vcode_reg_type(nnets)) != VCODE_TYPE_OFFSET)
      vcode_fail("sched_waveform net count is not offset type");
}

void emit_cond(vcode_reg_t test, vcode_block_t btrue, vcode_block_t bfalse)
{
   int64_t tconst;
   if (vcode_reg_const(test, &tconst)) {
      emit_jump(!!tconst ? btrue : bfalse);
      return;
   }

   op_t *op = vcode_add_op(VCODE_OP_COND);
   vcode_add_arg(op, test);
   vcode_add_target(op, btrue);
   vcode_add_target(op, bfalse);

   if (!vtype_eq(vcode_reg_type(test), vtype_bool()))
      vcode_fail("cond test is not a bool");
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

   vcode_type_t dim_types[1] = {
      vtype_int(1, INT32_MAX)
   };
   op->result = vcode_add_reg(
      vtype_uarray(dim_types, 1, vtype_int(0, 183), vtype_int(0, 127)));

   return op->result;
}

void emit_comment(const char *fmt, ...)
{
   va_list ap;
   va_start(ap, fmt);
   vcode_add_op(VCODE_OP_COMMENT)->comment = xvasprintf(fmt, ap);
   va_end(ap);
}

vcode_reg_t emit_select(vcode_reg_t test, vcode_reg_t rtrue,
                        vcode_reg_t rfalse)
{
   int64_t tconst;
   if (vcode_reg_const(test, &tconst))
      return !!tconst ? rtrue : rfalse;

   op_t *op = vcode_add_op(VCODE_OP_SELECT);
   vcode_add_arg(op, test);
   vcode_add_arg(op, rtrue);
   vcode_add_arg(op, rfalse);
   op->result = vcode_add_reg(vcode_reg_type(rtrue));

   if (!vtype_eq(vcode_reg_type(test), vtype_bool()))
      vcode_fail("select test must have bool type");
   else if (!vtype_eq(vcode_reg_type(rtrue), vcode_reg_type(rfalse)))
      vcode_fail("select arguments are not the same type");

   return op->result;
}

static vcode_reg_t emit_logical_identity(vcode_op_t op, vcode_reg_t reg, bool b)
{
   switch (op) {
   case VCODE_OP_AND:  return b ? reg : emit_const(vtype_bool(), 0);
   case VCODE_OP_OR:   return b ? emit_const(vtype_bool(), 1) : reg;
   case VCODE_OP_XOR:  return b ? emit_not(reg) : reg;
   case VCODE_OP_XNOR: return b ? reg : emit_not(reg);
   case VCODE_OP_NAND: return b ? emit_not(reg) : emit_const(vtype_bool(), 1);
   case VCODE_OP_NOR:  return b ? emit_const(vtype_bool(), 0) : emit_not(reg);
   default:
      fatal_trace("missing logicial identity for %s", vcode_op_string(op));
   }
}

static vcode_reg_t emit_logical(vcode_op_t op, vcode_reg_t lhs, vcode_reg_t rhs)
{
   vcode_type_t vtbool = vtype_bool();

   int64_t lconst, rconst;
   const bool l_is_const = vcode_reg_const(lhs, &lconst);
   const bool r_is_const = vcode_reg_const(rhs, &rconst);
   if (l_is_const && r_is_const) {
      switch (op) {
      case VCODE_OP_AND: return emit_const(vtbool, l_is_const && r_is_const);
      case VCODE_OP_OR:  return emit_const(vtbool, l_is_const || r_is_const);
      default:
         fatal_trace("cannot constant fold logical %s", vcode_op_string(op));
      }
   }
   else if (l_is_const)
      return emit_logical_identity(op, rhs, !!lconst);
   else if (r_is_const)
      return emit_logical_identity(op, lhs, !!rconst);
   else if (lhs == rhs) {
      switch (op) {
      case VCODE_OP_AND:
      case VCODE_OP_OR:
         return lhs;
      default:
         fatal_trace("cannot simplify X %s X", vcode_op_string(op));
      }
   }

   vcode_reg_t result = emit_arith(op, lhs, rhs, UINT32_MAX);

   if (!vtype_eq(vcode_reg_type(lhs), vtbool)
       || !vtype_eq(vcode_reg_type(rhs), vtbool))
      vcode_fail("arguments to %s are not boolean", vcode_op_string(op));

   return result;
}

vcode_reg_t emit_or(vcode_reg_t lhs, vcode_reg_t rhs)
{
   return emit_logical(VCODE_OP_OR, lhs, rhs);
}

vcode_reg_t emit_and(vcode_reg_t lhs, vcode_reg_t rhs)
{
   return emit_logical(VCODE_OP_AND, lhs, rhs);
}

vcode_reg_t emit_nand(vcode_reg_t lhs, vcode_reg_t rhs)
{
   return emit_logical(VCODE_OP_NAND, lhs, rhs);
}

vcode_reg_t emit_nor(vcode_reg_t lhs, vcode_reg_t rhs)
{
   return emit_logical(VCODE_OP_NOR, lhs, rhs);
}

vcode_reg_t emit_xor(vcode_reg_t lhs, vcode_reg_t rhs)
{
   return emit_logical(VCODE_OP_XOR, lhs, rhs);
}

vcode_reg_t emit_xnor(vcode_reg_t lhs, vcode_reg_t rhs)
{
   return emit_logical(VCODE_OP_XNOR, lhs, rhs);
}

vcode_reg_t emit_not(vcode_reg_t arg)
{
   op_t *op = vcode_add_op(VCODE_OP_NOT);
   vcode_add_arg(op, arg);

   vcode_type_t vtbool = vtype_bool();
   if (!vtype_eq(vcode_reg_type(arg), vtbool))
      vcode_fail("argument to not is not boolean");

   return (op->result = vcode_add_reg(vtbool));
}

vcode_reg_t emit_wrap(vcode_reg_t data, const vcode_dim_t *dims, int ndims)
{
   op_t *op = vcode_add_op(VCODE_OP_WRAP);
   vcode_add_arg(op, data);
   vcode_type_t dim_types[ndims];
   for (int i = 0; i < ndims; i++) {
      vcode_add_arg(op, dims[i].left);
      vcode_add_arg(op, dims[i].right);
      vcode_add_arg(op, dims[i].dir);
      dim_types[i] = vcode_reg_type(dims[i].left);
   }

   vcode_type_t ptr_type = vcode_reg_type(data);
   const vtype_kind_t ptrkind = vtype_kind(ptr_type);
   if (ptrkind != VCODE_TYPE_POINTER && ptrkind != VCODE_TYPE_CARRAY)
      vcode_fail("wrapped data is not pointer");

   vcode_type_t elem = (ptrkind == VCODE_TYPE_POINTER)
      ? vtype_pointed(ptr_type) : vtype_elem(ptr_type);

   op->result = vcode_add_reg(vtype_uarray(dim_types, ndims,
                                           elem, vcode_reg_bounds(data)));

   return op->result;
}

static vcode_reg_t emit_uarray_op(vcode_op_t o, vcode_type_t rtype,
                                  vcode_reg_t array, unsigned dim,
                                  unsigned arg_index)
{
   // Reuse any previous operation in this block with the same arguments
   op_t *other = NULL;
   while (vcode_dominating_ops(o, &other)) {
      if (other->args.items[0] == array && other->dim == dim
          && (rtype == VCODE_INVALID_TYPE
              || vtype_eq(rtype, vcode_reg_type(other->result))))
         return other->result;
   }

   other = NULL;
   while (vcode_dominating_ops(VCODE_OP_WRAP, &other)) {
      if (other->result == array)
         return other->args.items[1 + (dim * 3) + arg_index];
   }

   op_t *op = vcode_add_op(o);
   vcode_add_arg(op, array);
   op->dim = dim;

   vcode_type_t atype = vcode_reg_type(array);
   if (vtype_kind(atype) != VCODE_TYPE_UARRAY)
      vcode_fail("cannot use %s with non-uarray type", vcode_op_string(o));

   vtype_t *vt = vcode_type_data(atype);
   if (dim >= vt->dims.count)
      vcode_fail("invalid dimension %d", dim);

   if (rtype == VCODE_INVALID_TYPE)
      rtype = vt->dims.items[dim];

   return (op->result = vcode_add_reg(rtype));
}

vcode_reg_t emit_uarray_left(vcode_reg_t array, unsigned dim)
{
   return emit_uarray_op(VCODE_OP_UARRAY_LEFT, VCODE_INVALID_TYPE,
                         array, dim, 0);
}

vcode_reg_t emit_uarray_right(vcode_reg_t array, unsigned dim)
{
   return emit_uarray_op(VCODE_OP_UARRAY_RIGHT, VCODE_INVALID_TYPE,
                         array, dim, 1);
}

vcode_reg_t emit_uarray_dir(vcode_reg_t array, unsigned dim)
{
   return emit_uarray_op(VCODE_OP_UARRAY_DIR, vtype_bool(),
                         array, dim, 2);
}

vcode_reg_t emit_unwrap(vcode_reg_t array)
{
   op_t *other = NULL;
   while (vcode_dominating_ops(VCODE_OP_WRAP, &other)) {
      if (other->result == array)
         return other->args.items[0];
   }

   op_t *op = vcode_add_op(VCODE_OP_UNWRAP);
   vcode_add_arg(op, array);

   vtype_t *vt = vcode_type_data(vcode_reg_type(array));
   if (vt->kind != VCODE_TYPE_UARRAY)
      vcode_fail("unwrap can only only be used with uarray types");

   return (op->result = vcode_add_reg(vtype_pointer(vt->elem)));
}

vcode_reg_t emit_phi(const vcode_reg_t *values, const vcode_block_t *blocks,
                     unsigned count)
{
   op_t *op = vcode_add_op(VCODE_OP_PHI);
   for (unsigned i = 0; i < count; i++) {
      // Check input block is predecessor of active block
      block_t *b = &(active_unit->blocks.items[blocks[i]]);
      assert(b->ops.count > 0);
      op_t *last = &(b->ops.items[b->ops.count - 1]);
      bool valid = false;
      for (unsigned j = 0; !valid && j < last->targets.count; j++) {
         if (last->targets.items[j] == active_block)
            valid = true;
      }

      if (valid) {
         vcode_add_arg(op, values[i]);
         vcode_add_target(op, blocks[i]);
      }

      vcode_type_t vt = vcode_reg_type(values[i]);
      for (unsigned j = 0; j < i; j++) {
         if (!vtype_eq(vt, vcode_reg_type(values[j]))) {
            vcode_dump();
            fatal_trace("phi values have different types");
         }
      }
   }

   return (op->result = vcode_add_reg(vcode_reg_type(values[0])));
}

vcode_reg_t emit_param_upref(int hops, vcode_reg_t reg)
{
   op_t *op = vcode_add_op(VCODE_OP_PARAM_UPREF);
   op->hops = hops;
   vcode_add_arg(op, reg);

   if (hops <= 0) {
      vcode_dump();
      fatal_trace("invalid hop count");
   }

   vcode_unit_t vu = active_unit;
   for (int i = 0; i < hops; i++)
      vu = vu->context;

   if (vu->kind == VCODE_UNIT_CONTEXT) {
      vcode_dump();
      fatal_trace("upref context is not a subprogram");
   }
   else if (reg >= vu->params.count) {
      vcode_dump();
      fatal_trace("upref register is not a parameter");
   }

   param_t *p = &(vu->params.items[reg]);
   op->result = vcode_add_reg(p->type);

   reg_t *rr = vcode_reg_data(op->result);
   rr->bounds = p->bounds;

   return op->result;
}

void emit_resolved_address(vcode_var_t var, vcode_signal_t signal)
{
   op_t *op = vcode_add_op(VCODE_OP_RESOLVED_ADDRESS);
   op->signal  = signal;
   op->address = var;
}

void emit_set_initial(vcode_signal_t signal, vcode_reg_t value, uint32_t index)
{
   op_t *op = vcode_add_op(VCODE_OP_SET_INITIAL);
   op->signal = signal;
   op->index  = index;
   vcode_add_arg(op, value);
}

void emit_alloc_driver(vcode_reg_t all_nets, vcode_reg_t all_length,
                       vcode_reg_t driven_nets, vcode_reg_t driven_length,
                       vcode_reg_t init)
{
   op_t *op = vcode_add_op(VCODE_OP_ALLOC_DRIVER);
   vcode_add_arg(op, all_nets);
   vcode_add_arg(op, all_length);
   vcode_add_arg(op, driven_nets);
   vcode_add_arg(op, driven_length);
   vcode_add_arg(op, init);
}

static vcode_reg_t emit_signal_flag(vcode_op_t opkind, vcode_reg_t nets,
                                    vcode_reg_t len)
{
   op_t *op = vcode_add_op(opkind);
   vcode_add_arg(op, nets);
   vcode_add_arg(op, len);

   if (vtype_kind(vcode_reg_type(nets)) != VCODE_TYPE_SIGNAL) {
      vcode_dump();
      fatal_trace("argument to %s is not a signal", vcode_op_string(opkind));
   }

   return (op->result = vcode_add_reg(vtype_bool()));
}

vcode_reg_t emit_event_flag(vcode_reg_t nets, vcode_reg_t len)
{
   return emit_signal_flag(VCODE_OP_EVENT, nets, len);
}

vcode_reg_t emit_active_flag(vcode_reg_t nets, vcode_reg_t len)
{
   return emit_signal_flag(VCODE_OP_ACTIVE, nets, len);
}

vcode_reg_t emit_record_ref(vcode_reg_t record, unsigned field)
{
   // Try scanning backwards through the block for another record ref
   block_t *b = vcode_block_data();
   for (int i = b->ops.count - 1; i >= 0; i--) {
      const op_t *op = &(b->ops.items[i]);
      if (op->kind == VCODE_OP_RECORD_REF && op->args.items[0] == record
          && op->field == field)
         return op->result;
   }

   op_t *op = vcode_add_op(VCODE_OP_RECORD_REF);
   op->field = field;
   vcode_add_arg(op, record);

   vtype_t *rptype = vcode_type_data(vcode_reg_type(record));

   if (rptype->kind != VCODE_TYPE_POINTER) {
      vcode_dump();
      fatal_trace("argument to record ref must be a pointer");
   }

   vtype_t *rtype = vcode_type_data(rptype->pointed);

   if (rtype->kind != VCODE_TYPE_RECORD) {
      vcode_dump();
      fatal_trace("argument must be pointer to array");
   }
   else if (field >= rtype->fields.count) {
      vcode_dump();
      fatal_trace("invalid field %d", field);
   }

   vcode_type_t result_type = vtype_pointer(rtype->fields.items[field]);
   return (op->result = vcode_add_reg(result_type));
}

void emit_copy(vcode_reg_t dest, vcode_reg_t src, vcode_reg_t count)
{
   op_t *op = vcode_add_op(VCODE_OP_COPY);
   vcode_add_arg(op, dest);
   vcode_add_arg(op, src);
   vcode_add_arg(op, count);

   vcode_type_t dtype = vcode_reg_type(dest);
   vcode_type_t stype = vcode_reg_type(src);

   if (vtype_kind(dtype) != VCODE_TYPE_POINTER) {
      vcode_dump();
      fatal_trace("destination type is not a pointer");
   }
   else if (vtype_kind(stype) != VCODE_TYPE_POINTER) {
      vcode_dump();
      fatal_trace("source type is not a pointer");
   }
   else if (!vtype_eq(dtype, stype)) {
      vcode_dump();
      fatal_trace("source and destination types do not match");
   }
   else if (vtype_kind(vcode_reg_type(count)) != VCODE_TYPE_OFFSET) {
      vcode_dump();
      fatal_trace("count is not offset type");
   }

   op->type = vtype_pointed(dtype);
}

void emit_sched_event(vcode_reg_t nets, vcode_reg_t n_elems, unsigned flags)
{
   op_t *op = vcode_add_op(VCODE_OP_SCHED_EVENT);
   vcode_add_arg(op, nets);
   vcode_add_arg(op, n_elems);
   op->flags = flags;
}

void emit_resume(ident_t func)
{
   op_t *op = vcode_add_op(VCODE_OP_RESUME);
   op->func = func;

   block_t *b = &(active_unit->blocks.items[active_block]);
   if (b->ops.count != 1)
      vcode_fail("resume must be first op in a block");
}

vcode_reg_t emit_memcmp(vcode_reg_t lhs, vcode_reg_t rhs, vcode_reg_t len)
{
   op_t *op = vcode_add_op(VCODE_OP_MEMCMP);
   vcode_add_arg(op, lhs);
   vcode_add_arg(op, rhs);
   vcode_add_arg(op, len);

   if (vtype_kind(vcode_reg_type(lhs)) != VCODE_TYPE_POINTER)
      vcode_fail("LHS of memcmp must have pointer type");
   else if (vtype_kind(vcode_reg_type(rhs)) != VCODE_TYPE_POINTER)
      vcode_fail("RHS of memcmp must have pointer type");
   else if (vtype_kind(vcode_reg_type(len)) != VCODE_TYPE_OFFSET)
      vcode_fail("length of memcmp must have offset type");

   return (op->result = vcode_add_reg(vtype_bool()));
}

void emit_memset(vcode_reg_t ptr, vcode_reg_t value, vcode_reg_t len)
{
   op_t *op = vcode_add_op(VCODE_OP_MEMSET);
   vcode_add_arg(op, ptr);
   vcode_add_arg(op, value);
   vcode_add_arg(op, len);

   if (vtype_kind(vcode_reg_type(ptr)) != VCODE_TYPE_POINTER)
      vcode_fail("target of memset must have pointer type");
   else if (vtype_kind(vcode_reg_type(value)) != VCODE_TYPE_INT)
      vcode_fail("value of memset must have integer type");
   else if (vtype_kind(vcode_reg_type(len)) != VCODE_TYPE_OFFSET)
      vcode_fail("length of memset must have offset type");
   else if (vtype_high(vcode_reg_type(value)) > UINT8_MAX)
      vcode_fail("memset value must be max 8-bit");
}

vcode_reg_t emit_vec_load(vcode_reg_t signal, vcode_reg_t length)
{
   op_t *op = vcode_add_op(VCODE_OP_VEC_LOAD);
   vcode_add_arg(op, signal);
   vcode_add_arg(op, length);

   vcode_type_t signal_type = vcode_reg_type(signal);

   if (vtype_kind(signal_type) != VCODE_TYPE_SIGNAL)
      vcode_fail("signal of vec load must have signal type");
   else if (vtype_kind(vcode_reg_type(length)) != VCODE_TYPE_OFFSET)
      vcode_fail("length of vec load must have offset type");

   vcode_type_t base_type = vtype_base(signal_type);
   op->result = vcode_add_reg(vtype_pointer(base_type));

   vcode_reg_data(op->result)->bounds = base_type;

   return op->result;
}

void emit_case(vcode_reg_t value, vcode_block_t def, const vcode_reg_t *cases,
               const vcode_block_t *blocks, int ncases)
{
   for (int i = 0; i < ncases; i++) {
      if (cases[i] == value) {
         emit_jump(blocks[i]);
         return;
      }
   }

   op_t *op = vcode_add_op(VCODE_OP_CASE);
   vcode_add_arg(op, value);
   vcode_add_target(op, def);

   for (int i = 0; i < ncases; i++) {
      vcode_add_arg(op, cases[i]);
      vcode_add_target(op, blocks[i]);
   }
}

vcode_reg_t emit_endfile(vcode_reg_t file)
{
   op_t *op = vcode_add_op(VCODE_OP_ENDFILE);
   vcode_add_arg(op, file);

   if (vtype_kind(vcode_reg_type(file)) != VCODE_TYPE_FILE)
      vcode_fail("endfile argument must have file type");

   return (op->result = vcode_add_reg(vtype_bool()));
}

void emit_file_open(vcode_reg_t file, vcode_reg_t name, vcode_reg_t length,
                    vcode_reg_t kind, vcode_reg_t status)
{
   op_t *op = vcode_add_op(VCODE_OP_FILE_OPEN);
   vcode_add_arg(op, file);
   vcode_add_arg(op, name);
   vcode_add_arg(op, length);
   vcode_add_arg(op, kind);
   if (status != VCODE_INVALID_REG)
      vcode_add_arg(op, status);

   if (!vtype_is_pointer(vcode_reg_type(file), VCODE_TYPE_FILE))
      vcode_fail("file open first argument must have file pointer type");
}

void emit_file_write(vcode_reg_t file, vcode_reg_t value, vcode_reg_t length)
{
   op_t *op = vcode_add_op(VCODE_OP_FILE_WRITE);
   vcode_add_arg(op, file);
   vcode_add_arg(op, value);
   if (length != VCODE_INVALID_REG)
      vcode_add_arg(op, length);

   if (!vtype_is_pointer(vcode_reg_type(file), VCODE_TYPE_FILE))
      vcode_fail("file write first argument must have file pointer type");
}

void emit_file_close(vcode_reg_t file)
{
   op_t *op = vcode_add_op(VCODE_OP_FILE_CLOSE);
   vcode_add_arg(op, file);

   if (!vtype_is_pointer(vcode_reg_type(file), VCODE_TYPE_FILE))
      vcode_fail("file close argument must have file pointer type");
}

void emit_file_read(vcode_reg_t file, vcode_reg_t ptr,
                    vcode_reg_t inlen, vcode_reg_t outlen)
{
   op_t *op = vcode_add_op(VCODE_OP_FILE_READ);
   vcode_add_arg(op, file);
   vcode_add_arg(op, ptr);
   if (inlen != VCODE_INVALID_REG) {
      vcode_add_arg(op, inlen);
      if (outlen != VCODE_INVALID_REG)
         vcode_add_arg(op, outlen);
   }

   if (!vtype_is_pointer(vcode_reg_type(file), VCODE_TYPE_FILE))
      vcode_fail("file read first argument must have file pointer type");
   else if (vtype_kind(vcode_reg_type(ptr)) != VCODE_TYPE_POINTER)
      vcode_fail("file read pointer argument must have pointer type");
   else if (outlen != VCODE_INVALID_REG
            && vtype_kind(vcode_reg_type(outlen)) != VCODE_TYPE_POINTER)
      vcode_fail("file read outlen argument must have pointer type");
}
