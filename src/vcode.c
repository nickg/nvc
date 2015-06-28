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
   ident_t             func;
   unsigned            subkind;
   union {
      vcode_cmp_t     cmp;
      int64_t         value;
      double          real;
      char           *comment;
      vcode_signal_t  signal;
      unsigned        dim;
      unsigned        hops;
      unsigned        field;
      unsigned        elems;
      uint32_t        hint;
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
         unsigned      dims;
         unsigned      size;
         vcode_type_t  elem;
         vcode_type_t  bounds;
      };
      vcode_type_t       pointed;
      vcode_type_t       base;
      struct {
         ident_t            name;
         uint32_t           index;
         vcode_type_array_t fields;
      };
   };
} vtype_t;

typedef struct {
   vcode_type_t type;
   vcode_type_t bounds;
   ident_t      name;
   bool         is_extern;
   bool         is_const;
   bool         use_heap;
} var_t;

typedef struct {
   vcode_type_t type;
   vcode_type_t bounds;
   ident_t      name;
   vcode_var_t  shadow;
   netid_t     *nets;
   size_t       nnets;
   bool         is_extern;
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
   bool           pure;
};

#define MASK_CONTEXT(x)   ((x) >> 24)
#define MASK_INDEX(x)     ((x) & 0xffffff)
#define MAKE_HANDLE(c, i) (((c) & 0xff) << 24 | ((i) & 0xffffff))

#define VCODE_ASSERT(expr, ...)    \
   if (unlikely(!(expr))) {        \
      vcode_dump();                \
      fatal_trace(__VA_ARGS__);    \
   }

#define VCODE_FOR_EACH_OP(name)                                 \
   block_t *_b = vcode_block_data();                            \
   op_t *name; int _i;                                          \
   for (_i = _b->ops.count - 1, name = &(_b->ops.items[_i]);    \
        _i >= 0; name = &(_b->ops.items[--_i]))

#define VCODE_FOR_EACH_MATCHING_OP(name, k) \
   VCODE_FOR_EACH_OP(name) if (name->kind == k)

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

   VCODE_ASSERT(
      !vcode_block_finished(),
      "attempt to add to already finished block %d", active_block);

   block_t *block = vcode_block_data();

   op_t *op = op_array_alloc(&(block->ops));
   memset(op, '\0', sizeof(op_t));
   op->kind   = kind;
   op->result = VCODE_INVALID_REG;

   return op;
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

static op_t *vcode_find_definition(vcode_reg_t reg)
{
   for (int i = active_block; i >= 0; i--) {
      block_t *b = &(active_unit->blocks.items[i]);
      for (int j = b->ops.count - 1; j >= 0; j--) {
         if (b->ops.items[j].result == reg)
            return &(b->ops.items[j]);
      }
   }

   return NULL;
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
   while (unit->kind != VCODE_UNIT_CONTEXT)
      unit = unit->context;
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

void vcode_heap_allocate(vcode_reg_t reg)
{
   op_t *defn = vcode_find_definition(reg);
   if (defn == NULL) {
      // It is always safe to return a pointer to an argument
      return;
   }

   switch (defn->kind) {
   case VCODE_OP_CONST:
   case VCODE_OP_CONST_REAL:
   case VCODE_OP_CONST_ARRAY:
   case VCODE_OP_NULL:
      break;

   case VCODE_OP_ALLOCA:
      defn->subkind = VCODE_ALLOCA_HEAP;
      break;

   case VCODE_OP_INDEX:
      vcode_var_data(defn->address)->use_heap = true;
      break;

   case VCODE_OP_WRAP:
      vcode_heap_allocate(defn->args.items[0]);
      break;

   case VCODE_OP_ADD:
      // When adding pointers only the first argument is a pointer
      vcode_heap_allocate(defn->args.items[0]);
      break;

   case VCODE_OP_UNWRAP:
      vcode_heap_allocate(defn->args.items[0]);
      break;

   case VCODE_OP_LOAD:
      {
         if (vcode_reg_kind(reg) != VCODE_TYPE_UARRAY)
            return;

         // Any store to this variable must be heap allocated
         for (int i = 0; i < active_unit->blocks.count; i++) {
            block_t *b = &(active_unit->blocks.items[i]);
            for (int j = 0; j < b->ops.count; j++) {
               op_t *op = &(b->ops.items[j]);
               if (op->kind == VCODE_OP_STORE && op->address == defn->address)
                  vcode_heap_allocate(op->args.items[0]);

               VCODE_ASSERT(
                  op->kind != VCODE_OP_INDEX || op->address != defn->address,
                  "cannot heap allocate aliased pointer r%d", reg);
            }
         }
      }
      break;

   case VCODE_OP_FCALL:
   case VCODE_OP_NESTED_FCALL:
      // Must have been safety checked by definition
      break;

   case VCODE_OP_IMAGE:
      // Runtime allocates from temporary stack
      break;

   case VCODE_OP_RECORD_REF:
      vcode_heap_allocate(defn->args.items[0]);
      break;

   case VCODE_OP_SELECT:
      vcode_heap_allocate(defn->args.items[1]);
      vcode_heap_allocate(defn->args.items[2]);
      break;

   case VCODE_OP_LOAD_INDIRECT:
      {
         // Always OK if scalar otherwise check the pointer source
         const vtype_kind_t vtkind = vcode_reg_kind(reg);
         if (vtkind != VCODE_TYPE_INT && vtkind != VCODE_TYPE_REAL)
            vcode_heap_allocate(defn->args.items[0]);
      }
      break;

   case VCODE_OP_ALL:
      // Must have been allocated on the heap
      break;

   case VCODE_OP_NETS:
      // Pointer to a global
      break;

   case VCODE_OP_NEW:
      // On the heap by definition
      break;

   case VCODE_OP_SUB:
   case VCODE_OP_MUL:
   case VCODE_OP_DIV:
   case VCODE_OP_CAST:
   case VCODE_OP_CMP:
   case VCODE_OP_OR:
   case VCODE_OP_NOT:
   case VCODE_OP_AND:
   case VCODE_OP_NOR:
   case VCODE_OP_NAND:
   case VCODE_OP_XOR:
   case VCODE_OP_XNOR:
   case VCODE_OP_EVENT:
   case VCODE_OP_ACTIVE:
   case VCODE_OP_BIT_VEC_OP:
   case VCODE_OP_VALUE:
   case VCODE_OP_STORAGE_HINT:
   case VCODE_OP_UARRAY_LEN:
   case VCODE_OP_UARRAY_LEFT:
   case VCODE_OP_UARRAY_RIGHT:
   case VCODE_OP_UARRAY_DIR:
   case VCODE_OP_LAST_EVENT:
   case VCODE_OP_PARAM_UPREF:
   case VCODE_OP_NEG:
   case VCODE_OP_EXP:
   case VCODE_OP_ABS:
   case VCODE_OP_MOD:
   case VCODE_OP_REM:
   case VCODE_OP_ENDFILE:
   case VCODE_OP_VEC_LOAD:
   case VCODE_OP_MEMCMP:
      // Result cannot reference pointer
      break;

   default:
      VCODE_ASSERT(false, "cannot heap allocate r%d", reg);
   }
}

void vcode_state_save(vcode_state_t *state)
{
   state->unit  = active_unit;
   state->block = active_block;
}

void vcode_state_restore(const vcode_state_t *state)
{
   active_unit  = state->unit;
   active_block = state->block;
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

vtype_kind_t vcode_reg_kind(vcode_reg_t reg)
{
   return vtype_kind(vcode_reg_type(reg));
}

vcode_type_t vcode_reg_bounds(vcode_reg_t reg)
{
   return vcode_reg_data(reg)->bounds;
}

bool vcode_reg_const(vcode_reg_t reg, int64_t *value)
{
   reg_t *r = vcode_reg_data(reg);

   vtype_kind_t kind = vtype_kind(r->type);
   if (kind != VCODE_TYPE_INT && kind != VCODE_TYPE_OFFSET)
      return false;

   vtype_t *bounds = vcode_type_data(r->bounds);

   VCODE_ASSERT(
      bounds->kind == VCODE_TYPE_INT || bounds->kind == VCODE_TYPE_OFFSET,
      "integer register r%d has non-integer bounds", reg);

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
            case VCODE_OP_CONST_REAL:
            case VCODE_OP_CONST_ARRAY:
            case VCODE_OP_CONST_RECORD:
            case VCODE_OP_LOAD:
            case VCODE_OP_LOAD_INDIRECT:
            case VCODE_OP_ADD:
            case VCODE_OP_SUB:
            case VCODE_OP_MUL:
            case VCODE_OP_CMP:
            case VCODE_OP_INDEX:
            case VCODE_OP_NETS:
            case VCODE_OP_WRAP:
            case VCODE_OP_VEC_LOAD:
            case VCODE_OP_HEAP_SAVE:
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

            case VCODE_OP_STORAGE_HINT:
               o->comment = xasprintf("Unused storage hint for r%d",
                                      o->args.items[0]);
               o->kind = VCODE_OP_COMMENT;
               vcode_reg_array_resize(&(o->args), 0, VCODE_INVALID_REG);
               pruned++;
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

vcode_var_t vcode_find_var(ident_t name)
{
   assert(active_unit != NULL);
   for (int i = 0; i < active_unit->vars.count; i++) {
      if (active_unit->vars.items[i].name == name)
         return MAKE_HANDLE(active_unit->depth, i);
   }

   return VCODE_INVALID_VAR;
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

bool vcode_var_extern(vcode_var_t var)
{
   return vcode_var_data(var)->is_extern;
}

bool vcode_var_use_heap(vcode_var_t var)
{
   return vcode_var_data(var)->use_heap;
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

bool vcode_signal_extern(vcode_signal_t sig)
{
   return vcode_signal_data(sig)->is_extern;
}

vcode_op_t vcode_get_op(int op)
{
   return vcode_op_data(op)->kind;
 }

ident_t vcode_get_func(int op)
{
   op_t *o = vcode_op_data(op);
   assert(o->kind == VCODE_OP_FCALL || o->kind == VCODE_OP_NESTED_FCALL
          || o->kind == VCODE_OP_PCALL || o->kind == VCODE_OP_RESUME
          || o->kind == VCODE_OP_SET_INITIAL
          || o->kind == VCODE_OP_NESTED_PCALL);
   return o->func;
}

unsigned vcode_get_subkind(int op)
{
   op_t *o = vcode_op_data(op);
   assert(o->kind == VCODE_OP_SCHED_EVENT || o->kind == VCODE_OP_BOUNDS
          || o->kind == VCODE_OP_VEC_LOAD || o->kind == VCODE_OP_BIT_VEC_OP
          || o->kind == VCODE_OP_INDEX_CHECK || o->kind == VCODE_OP_BIT_SHIFT
          || o->kind == VCODE_OP_ALLOCA || o->kind == VCODE_OP_RESUME
          || o->kind == VCODE_OP_COVER_COND);
   return o->subkind;
}

int64_t vcode_get_value(int op)
{
   op_t *o = vcode_op_data(op);
   assert(o->kind == VCODE_OP_CONST);
   return o->value;
}

double vcode_get_real(int op)
{
   op_t *o = vcode_op_data(op);
   assert(o->kind == VCODE_OP_CONST_REAL);
   return o->real;
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
          || o->kind == VCODE_OP_SET_INITIAL
          || o->kind == VCODE_OP_NEEDS_LAST_VALUE);
   return o->signal;
}

unsigned vcode_get_dim(int op)
{
   op_t *o = vcode_op_data(op);
   assert(o->kind == VCODE_OP_UARRAY_LEFT || o->kind == VCODE_OP_UARRAY_RIGHT
          || o->kind == VCODE_OP_UARRAY_DIR || o->kind == VCODE_OP_UARRAY_LEN);
   return o->dim;
}

int vcode_get_hops(int op)
{
   op_t *o = vcode_op_data(op);
   assert(o->kind == VCODE_OP_PARAM_UPREF || o->kind == VCODE_OP_NESTED_FCALL
          || o->kind == VCODE_OP_NESTED_PCALL || o->kind == VCODE_OP_RESUME);
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
          || o->kind == VCODE_OP_COPY || o->kind == VCODE_OP_SET_INITIAL
          || o->kind == VCODE_OP_INDEX_CHECK);
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
          || o->kind == VCODE_OP_DIV || o->kind == VCODE_OP_NULL_CHECK
          || o->kind == VCODE_OP_VALUE || o->kind == VCODE_OP_BOUNDS
          || o->kind == VCODE_OP_DYNAMIC_BOUNDS
          || o->kind == VCODE_OP_ARRAY_SIZE || o->kind == VCODE_OP_INDEX_CHECK
          || o->kind == VCODE_OP_COVER_STMT || o->kind == VCODE_OP_COVER_COND);
   return o->index;
}

uint32_t vcode_get_hint(int op)
{
   op_t *o = vcode_op_data(op);
   assert(o->kind == VCODE_OP_BOUNDS
          || o->kind == VCODE_OP_DYNAMIC_BOUNDS
          || o->kind == VCODE_OP_INDEX_CHECK);
   return o->hint;
}

vcode_block_t vcode_get_target(int op, int nth)
{
   op_t *o = vcode_op_data(op);
   assert(o->kind == VCODE_OP_WAIT || o->kind == VCODE_OP_JUMP
          || o->kind == VCODE_OP_COND || o->kind == VCODE_OP_PCALL
          || o->kind == VCODE_OP_CASE || o->kind == VCODE_OP_NESTED_PCALL);
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
      "uarray right", "uarray dir", "unwrap", "not", "and", "nested fcall",
      "param upref", "resolved address", "set initial", "alloc driver",
      "event", "active", "const record", "record ref", "copy", "sched event",
      "pcall", "resume", "memcmp", "xor", "xnor", "nand", "nor", "memset",
      "vec load", "case", "endfile", "file open", "file write", "file close",
      "file read", "null", "new", "null check", "deallocate", "all",
      "bit vec op", "const real", "value", "last event", "needs last value",
      "dynamic bounds", "array size", "index check", "bit shift",
      "storage hint", "debug out", "nested pcall", "cover stmt", "cover cond",
      "uarray len", "heap save", "heap restore"
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

   case VCODE_TYPE_REAL:
      printf("%%");
      break;

   case VCODE_TYPE_CARRAY:
      {
         printf("[%u] : ", vt->size);
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
         for (unsigned i = 0; i < vt->dims; i++)
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

   case VCODE_TYPE_ACCESS:
      printf("A<");
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
         char *name LOCAL = vtype_record_name(type);
         printf("%s{}", name);
      }
      break;

   case VCODE_TYPE_FILE:
      printf("F<");
      vcode_dump_one_type(vt->base);
      printf(">");
      break;
   }
}

static void vcode_dump_tab(int col, int to_col)
{
   if (col >= to_col)
      printf(" ");
   else {
      while (col < to_col)
         col += printf(" ");
   }
}

static void vcode_dump_type(int col, vcode_type_t type, vcode_type_t bounds)
{
   vcode_dump_tab(col, 40);

   color_printf("$cyan$// ");
   vcode_dump_one_type(type);
   if (!vtype_eq(type, bounds)) {
      printf(" => ");
      vcode_dump_one_type(bounds);
   }
   color_printf("$$ ");
}

static void vcode_dump_result_type(int col, const op_t *op)
{
   if (op->result != VCODE_INVALID_REG) {
      reg_t *r = vcode_reg_data(op->result);
      vcode_dump_type(col, r->type, r->bounds);
   }
}

static int vcode_dump_var(vcode_var_t var)
{
   if (MASK_CONTEXT(var) != active_unit->depth) {
      vcode_unit_t owner = active_unit;
      for (int i = MASK_CONTEXT(var); i < active_unit->depth; i++)
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

   for (int i = 0; i < vu->types.count; i++) {
      const vtype_t *t = &(vu->types.items[i]);
      if (t->kind == VCODE_TYPE_RECORD) {
         int col = 0;
         if (t->index != UINT32_MAX)
            col += color_printf("  $magenta$%s.%u$$", istr(t->name), t->index);
         else
            col += color_printf("  $magenta$%s$$", istr(t->name));
         vcode_dump_tab(col, 40);
         color_printf("$cyan${");
         for (unsigned i = 0; i < t->fields.count; i++) {
            if (i > 0)
               printf(", ");
            vcode_dump_one_type(t->fields.items[i]);
         }
         color_printf("}$$\n");
      }
   }

   printf("Variables  %d\n", vu->vars.count);

   for (int i = 0; i < vu->vars.count; i++) {
      const var_t *v = &(vu->vars.items[i]);
      int col = printf("  ");
      col += color_printf("$magenta$%s$$", istr(v->name));

      if (v->is_extern)
         col += printf(" extern");
      if (v->use_heap)
         col += printf(" heap");

      if (vu->kind == VCODE_UNIT_CONTEXT && !v->is_const)
         col += printf(" mutable");
      else if (vu->kind != VCODE_UNIT_CONTEXT && v->is_const)
         col += printf(" const");

      vcode_dump_type(col, v->type, v->bounds);
      printf("\n");
   }

   if (vu->kind == VCODE_UNIT_CONTEXT) {
      printf("Signals    %d\n", vu->signals.count);

      for (int i = 0; i < vu->signals.count; i++) {
         const signal_t *s = &(vu->signals.items[i]);
         int col = printf("  ");
         if (s->is_extern)
            col += printf("extern ");
         col += color_printf("$white$%s$$", istr(s->name));
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
               vcode_dump_result_type(col, op);
            }
            break;

         case VCODE_OP_CONST_REAL:
            {
               col += vcode_dump_reg(op->result);
               col += printf(" := %s %g",
                             vcode_op_string(op->kind),
                             op->real);
               vcode_dump_result_type(col, op);
            }
            break;

         case VCODE_OP_ALLOCA:
            {
               col += vcode_dump_reg(op->result);
               col += printf(" := %s ", vcode_op_string(op->kind));
               if (op->subkind == VCODE_ALLOCA_HEAP)
                  col += printf("heap ");
               if (op->args.count > 0)
                  col += vcode_dump_reg(op->args.items[0]);
               vcode_dump_result_type(col, op);
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
               vcode_dump_result_type(col, op);
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
               vcode_dump_result_type(col, op);
            }
            break;

         case VCODE_OP_LOAD_INDIRECT:
            {
               col += vcode_dump_reg(op->result);
               col += color_printf(" := %s ", vcode_op_string(op->kind));
               col += vcode_dump_reg(op->args.items[0]);
               vcode_dump_result_type(col, op);
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
               col += printf(" := %s ", vcode_op_string(op->kind));
               col += vcode_dump_var(op->address);
               if (op->args.count > 0) {
                  col += printf(" + ");
                  col += vcode_dump_reg(op->args.items[0]);
               }
               vcode_dump_result_type(col, op);
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
               vcode_dump_result_type(col, op);
            }
            break;

         case VCODE_OP_NOT:
            {
               col += vcode_dump_reg(op->result);
               col += printf(" := %s ", vcode_op_string(op->kind));
               col += vcode_dump_reg(op->args.items[0]);
               vcode_dump_result_type(col, op);
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

         case VCODE_OP_DYNAMIC_BOUNDS:
            {
               printf("%s ", vcode_op_string(op->kind));
               vcode_dump_reg(op->args.items[0]);
               printf(" in ");
               vcode_dump_reg(op->args.items[1]);
               printf(" .. ");
               vcode_dump_reg(op->args.items[2]);
               printf(" kind ");
               vcode_dump_reg(op->args.items[3]);
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
               vcode_dump_result_type(col + 1, op);
            }
            break;

         case VCODE_OP_CAST:
            {
               col += vcode_dump_reg(op->result);
               col += printf(" := %s ", vcode_op_string(op->kind));
               col += vcode_dump_reg(op->args.items[0]);
               vcode_dump_result_type(col, op);
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
               vcode_dump_result_type(col, op);
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
               vcode_dump_result_type(col, op);
            }
            break;

         case VCODE_OP_IMAGE:
            {
               col += vcode_dump_reg(op->result);
               col += printf(" := %s ", vcode_op_string(op->kind));
               col += vcode_dump_reg(op->args.items[0]);
               vcode_dump_result_type(col, op);
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
               vcode_dump_result_type(col, op);
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
               vcode_dump_result_type(col, op);
            }
            break;

         case VCODE_OP_UARRAY_LEFT:
         case VCODE_OP_UARRAY_RIGHT:
         case VCODE_OP_UARRAY_DIR:
         case VCODE_OP_UARRAY_LEN:
            {
               col += vcode_dump_reg(op->result);
               col += printf(" := %s ", vcode_op_string(op->kind));
               col += vcode_dump_reg(op->args.items[0]);
               col += printf(" dim %d", op->dim);
               vcode_dump_result_type(col, op);
            }
            break;

         case VCODE_OP_UNWRAP:
            {
               col += vcode_dump_reg(op->result);
               col += printf(" := %s ", vcode_op_string(op->kind));
               col += vcode_dump_reg(op->args.items[0]);
               vcode_dump_result_type(col, op);
            }
            break;

         case VCODE_OP_PARAM_UPREF:
            {
               col += vcode_dump_reg(op->result);
               col += printf(" := %s %d, ", vcode_op_string(op->kind),
                             op->hops);
               col += vcode_dump_reg(op->args.items[0]);
               vcode_dump_result_type(col, op);
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
               vcode_dump_result_type(col, op);
            }
            break;

         case VCODE_OP_RECORD_REF:
            {
               col += vcode_dump_reg(op->result);
               col += printf(" := %s ", vcode_op_string(op->kind));
               col += vcode_dump_reg(op->args.items[0]);
               col += printf(" field %d", op->field);
               vcode_dump_result_type(col, op);
            }
            break;

         case VCODE_OP_COPY:
            {
               vcode_dump_reg(op->args.items[0]);
               printf(" := %s ", vcode_op_string(op->kind));
               vcode_dump_reg(op->args.items[1]);
               if (op->args.count > 2) {
                  printf(" count " );
                  vcode_dump_reg(op->args.items[2]);
               }
            }
            break;

         case VCODE_OP_SCHED_EVENT:
            {
               printf("%s on ", vcode_op_string(op->kind));
               vcode_dump_reg(op->args.items[0]);
               printf(" count ");
               vcode_dump_reg(op->args.items[1]);
               printf(" flags %x", op->subkind);
            }
            break;

         case VCODE_OP_PCALL:
         case VCODE_OP_NESTED_PCALL:
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
               vcode_dump_result_type(col, op);
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
               if (op->args.count > 1) {
                  col += printf(" length ");
                  col += vcode_dump_reg(op->args.items[1]);
               }
               if (op->subkind)
                  col += printf(" last");
               vcode_dump_result_type(col, op);
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
               vcode_dump_result_type(col, op);
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
               if (op->args.count >= 3) {
                  printf(" inlen ");
                  vcode_dump_reg(op->args.items[2]);
                  if (op->args.count >= 4) {
                     printf(" outlen ");
                     vcode_dump_reg(op->args.items[3]);
                  }
               }
            }
            break;

         case VCODE_OP_NULL:
         case VCODE_OP_NEW:
            {
               col += vcode_dump_reg(op->result);
               col += printf(" := %s", vcode_op_string(op->kind));
               if (op->args.count == 1) {
                  col += printf(" length ");
                  col += vcode_dump_reg(op->args.items[0]);
               }
               vcode_dump_result_type(col, op);
            }
            break;

         case VCODE_OP_NULL_CHECK:
         case VCODE_OP_DEALLOCATE:
            {
               printf("%s ", vcode_op_string(op->kind));
               vcode_dump_reg(op->args.items[0]);
            }
            break;

         case VCODE_OP_ALL:
            {
               col += vcode_dump_reg(op->result);
               col += printf(" := %s ", vcode_op_string(op->kind));
               col += vcode_dump_reg(op->args.items[0]);
               vcode_dump_result_type(col, op);
            }
            break;

         case VCODE_OP_BIT_VEC_OP:
            {
               col += vcode_dump_reg(op->result);
               col += printf(" := %s %d lhs ", vcode_op_string(op->kind),
                             op->subkind);
               col += vcode_dump_reg(op->args.items[0]);
               col += printf(" length ");
               col += vcode_dump_reg(op->args.items[1]);
               col += printf(" dir ");
               col += vcode_dump_reg(op->args.items[2]);
               if (op->args.count > 2) {
                  col += printf(" rhs ");
                  col += vcode_dump_reg(op->args.items[3]);
                  col += printf(" length ");
                  col += vcode_dump_reg(op->args.items[4]);
                  col += printf(" dir ");
                  col += vcode_dump_reg(op->args.items[5]);
               }
               vcode_dump_result_type(col, op);
            }
            break;

         case VCODE_OP_VALUE:
         case VCODE_OP_LAST_EVENT:
            {
               col += vcode_dump_reg(op->result);
               col += printf(" := %s ", vcode_op_string(op->kind));
               col += vcode_dump_reg(op->args.items[0]);
               if (op->args.count > 1) {
                  col += printf(" length ");
                  col += vcode_dump_reg(op->args.items[1]);
               }
               vcode_dump_result_type(col, op);
            }
            break;

         case VCODE_OP_NEEDS_LAST_VALUE:
            {
               printf("%s ", vcode_op_string(op->kind));
               color_printf("$white$%s$$ ",
                            istr(vcode_signal_name(op->signal)));
            }
            break;

         case VCODE_OP_ARRAY_SIZE:
            {
               printf("%s left ", vcode_op_string(op->kind));
               vcode_dump_reg(op->args.items[0]);
               printf(" == right ");
               vcode_dump_reg(op->args.items[1]);
            }
            break;

         case VCODE_OP_INDEX_CHECK:
            {
               printf("%s low ", vcode_op_string(op->kind));
               vcode_dump_reg(op->args.items[0]);
               printf(" high ");
               vcode_dump_reg(op->args.items[1]);
               printf(" in ");
               if (op->args.count == 2)
                  vcode_dump_one_type(op->type);
               else {
                  vcode_dump_reg(op->args.items[2]);
                  printf(" .. ");
                  vcode_dump_reg(op->args.items[3]);
               }
               printf(" subkind %d", op->subkind);
            }
            break;

         case VCODE_OP_BIT_SHIFT:
            {
               col += vcode_dump_reg(op->result);
               col += printf(" := %s %d ", vcode_op_string(op->kind),
                             op->subkind);
               col += vcode_dump_reg(op->args.items[0]);
               col += printf(" length ");
               col += vcode_dump_reg(op->args.items[1]);
               col += printf(" dir ");
               col += vcode_dump_reg(op->args.items[2]);
               col += printf(" shift ");
               col += vcode_dump_reg(op->args.items[3]);
               vcode_dump_result_type(col, op);
            }
            break;

         case VCODE_OP_STORAGE_HINT:
            {
               col += printf("%s ", vcode_op_string(op->kind));
               col += vcode_dump_reg(op->args.items[0]);
               if (op->args.count > 1) {
                  col += printf(" length ");
                  col += vcode_dump_reg(op->args.items[1]);
               }
               vcode_dump_type(col, op->type, op->type);
            }
            break;

         case VCODE_OP_DEBUG_OUT:
            {
               col += printf("%s ", vcode_op_string(op->kind));
               col += vcode_dump_reg(op->args.items[0]);
            }
            break;

         case VCODE_OP_COVER_STMT:
            {
               printf("%s %u", vcode_op_string(op->kind), op->index);
            }
            break;

         case VCODE_OP_COVER_COND:
            {
               printf("%s %u sub %u ", vcode_op_string(op->kind),
                      op->index, op->subkind);
               vcode_dump_reg(op->args.items[0]);
            }
            break;

         case VCODE_OP_HEAP_SAVE:
            {
               col += vcode_dump_reg(op->result);
               col += printf(" := %s", vcode_op_string(op->kind));
               vcode_dump_result_type(col, op);
            }
            break;

         case VCODE_OP_HEAP_RESTORE:
            {
               printf("%s ", vcode_op_string(op->kind));
               vcode_dump_reg(op->args.items[0]);
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

      switch (at->kind) {
      case VCODE_TYPE_INT:
         return (at->low == bt->low) && (at->high == bt->high);
      case VCODE_TYPE_CARRAY:
         return at->size == bt->size && vtype_eq(at->elem, bt->elem);
      case VCODE_TYPE_UARRAY:
         return at->dims == bt->dims && vtype_eq(at->elem, bt->elem);
      case VCODE_TYPE_POINTER:
      case VCODE_TYPE_ACCESS:
         return vtype_eq(at->pointed, bt->pointed);
      case VCODE_TYPE_OFFSET:
      case VCODE_TYPE_REAL:
         return true;
      case VCODE_TYPE_SIGNAL:
      case VCODE_TYPE_FILE:
         return vtype_eq(at->base, bt->base);
      case VCODE_TYPE_RECORD:
         return at->name == bt->name && at->index == bt->index;
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
   case VCODE_TYPE_ACCESS:
   case VCODE_TYPE_OFFSET:
   case VCODE_TYPE_FILE:
   case VCODE_TYPE_REAL:
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

vcode_type_t vtype_carray(int size, vcode_type_t elem, vcode_type_t bounds)
{
   assert(active_unit != NULL);

   const vtype_kind_t ekind = vtype_kind(elem);
   VCODE_ASSERT(ekind != VCODE_TYPE_CARRAY && ekind != VCODE_TYPE_UARRAY,
                "array types may not be nested");

   vtype_t *n = vtype_array_alloc(&(active_unit->types));
   memset(n, '\0', sizeof(vtype_t));
   n->kind   = VCODE_TYPE_CARRAY;
   n->elem   = elem;
   n->bounds = bounds;
   n->size   = MAX(size, 0);

   return vtype_new(n);
}

vcode_type_t vtype_named_record(ident_t name, uint32_t index, bool create)
{
   assert(active_unit != NULL);

   for (int i = 0; i < active_unit->types.count; i++) {
      vtype_t *other = &(active_unit->types.items[i]);
      if (other->kind == VCODE_TYPE_RECORD && other->name == name
          && other->index == index)
         return MAKE_HANDLE(active_unit->depth, i);
   }

   if (create) {
      vtype_t *n = vtype_array_alloc(&(active_unit->types));
      memset(n, '\0', sizeof(vtype_t));
      n->kind  = VCODE_TYPE_RECORD;
      n->name  = name;
      n->index = index;

      return vtype_new(n);
   }
   else
      return VCODE_INVALID_TYPE;
}

vcode_type_t vtype_uarray(int ndim, vcode_type_t elem, vcode_type_t bounds)
{
   assert(active_unit != NULL);

   const vtype_kind_t ekind = vtype_kind(elem);
   VCODE_ASSERT(ekind != VCODE_TYPE_CARRAY && ekind != VCODE_TYPE_UARRAY,
                "array types may not be nested");

   vtype_t *n = vtype_array_alloc(&(active_unit->types));
   memset(n, '\0', sizeof(vtype_t));
   n->kind   = VCODE_TYPE_UARRAY;
   n->elem   = elem;
   n->bounds = bounds;
   n->dims   = ndim;

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

vcode_type_t vtype_access(vcode_type_t to)
{
   assert(active_unit != NULL);

   vtype_t *n = vtype_array_alloc(&(active_unit->types));
   n->kind    = VCODE_TYPE_ACCESS;
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
   n->low  = INT32_MIN;
   n->high = INT32_MAX;

   return vtype_new(n);
}

vcode_type_t vtype_time(void)
{
   // XXX: should be INT64_MIN
   return vtype_int(INT64_MIN + 1, INT64_MAX);
}

vcode_type_t vtype_char(void)
{
   return vtype_int(0, 183);
}

vcode_type_t vtype_real(void)
{
   assert(active_unit != NULL);

   vtype_t *n = vtype_array_alloc(&(active_unit->types));
   n->kind = VCODE_TYPE_REAL;

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

unsigned vtype_dims(vcode_type_t type)
{
   vtype_t *vt = vcode_type_data(type);
   assert(vt->kind == VCODE_TYPE_UARRAY);
   return vt->dims;
}

unsigned vtype_size(vcode_type_t type)
{
   vtype_t *vt = vcode_type_data(type);
   assert(vt->kind == VCODE_TYPE_CARRAY);
   return vt->size;
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

void vtype_set_record_fields(vcode_type_t type, const vcode_type_t *field_types,
                             int nfields)
{
   vtype_t *n = vcode_type_data(type);

   if (n->fields.count > 0)
      vcode_type_array_resize(&(n->fields), 0, VCODE_INVALID_TYPE);

   for (int i = 0; i < nfields; i++)
      vcode_type_array_add(&(n->fields), field_types[i]);
}

char *vtype_record_name(vcode_type_t type)
{
   vtype_t *vt = vcode_type_data(type);
   assert(vt->kind == VCODE_TYPE_RECORD);
   if (vt->index != UINT32_MAX)
      return xasprintf("%s.%u", istr(vt->name), vt->index);
   else
      return xasprintf("%s", istr(vt->name));
}

vcode_type_t vtype_pointed(vcode_type_t type)
{
   vtype_t *vt = vcode_type_data(type);
   assert(vt->kind == VCODE_TYPE_POINTER || vt->kind == VCODE_TYPE_ACCESS);
   return vt->pointed;
}

int64_t vtype_low(vcode_type_t type)
{
   vtype_t *vt = vcode_type_data(type);
   assert(vt->kind == VCODE_TYPE_INT || vt->kind == VCODE_TYPE_OFFSET);
   return vt->low;
}

int64_t vtype_high(vcode_type_t type)
{
   vtype_t *vt = vcode_type_data(type);
   assert(vt->kind == VCODE_TYPE_INT || vt->kind == VCODE_TYPE_OFFSET);
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

bool vcode_unit_pure(void)
{
   assert(active_unit != NULL);
   return active_unit->pure;
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
   vu->pure    = true;

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
   int64_t value_const;
   if (vcode_reg_const(value, &value_const)) {
      if (value_const == 0)
         active_unit->pure = false;
      else {
         emit_comment("Always true assertion on r%d", value);
         return;
      }
   }

   op_t *op = vcode_add_op(VCODE_OP_ASSERT);
   vcode_add_arg(op, value);
   vcode_add_arg(op, severity);
   vcode_add_arg(op, message);
   vcode_add_arg(op, length);
   op->index = index;

   VCODE_ASSERT(vtype_eq(vcode_reg_type(value), vtype_bool()),
                "value parameter to assert is not bool");
}

void emit_report(vcode_reg_t message, vcode_reg_t length, vcode_reg_t severity,
                 uint32_t index)
{
   op_t *op = vcode_add_op(VCODE_OP_REPORT);
   vcode_add_arg(op, severity);
   vcode_add_arg(op, message);
   vcode_add_arg(op, length);
   op->index = index;

   active_unit->pure = false;
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
   VCODE_FOR_EACH_MATCHING_OP(other, VCODE_OP_CMP) {
      if (other->args.count == 2 && other->args.items[0] == lhs
          && other->args.items[1] == rhs && other->cmp == cmp)
         return other->result;
   }

   op_t *op = vcode_add_op(VCODE_OP_CMP);
   vcode_add_arg(op, lhs);
   vcode_add_arg(op, rhs);
   op->cmp    = cmp;
   op->result = vcode_add_reg(vtype_bool());

   VCODE_ASSERT(vtype_eq(vcode_reg_type(lhs), vcode_reg_type(rhs)),
                "arguments to cmp are not the same type");

   return op->result;
}

static vcode_reg_t emit_fcall_op(vcode_op_t op, ident_t func, vcode_type_t type,
                                 const vcode_reg_t *args, int nargs,
                                 vcode_block_t resume_bb, int hops)
{
   op_t *o = vcode_add_op(op);
   o->func = func;
   o->type = type;
   o->hops = hops;
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
                        VCODE_INVALID_BLOCK, 0);
}

vcode_reg_t emit_nested_fcall(ident_t func, vcode_type_t type,
                              const vcode_reg_t *args, int nargs, int hops)
{
   return emit_fcall_op(VCODE_OP_NESTED_FCALL, func, type, args, nargs,
                        VCODE_INVALID_BLOCK, hops);
}

void emit_pcall(ident_t func, const vcode_reg_t *args, int nargs,
                vcode_block_t resume_bb)
{
   emit_fcall_op(VCODE_OP_PCALL, func, VCODE_INVALID_TYPE, args, nargs,
                 resume_bb, 0);
}

void emit_nested_pcall(ident_t func, const vcode_reg_t *args, int nargs,
                       vcode_block_t resume_bb, int hops)
{
   emit_fcall_op(VCODE_OP_NESTED_PCALL, func, VCODE_INVALID_TYPE, args,
                 nargs, resume_bb, hops);
}

vcode_reg_t emit_alloca(vcode_type_t type, vcode_type_t bounds,
                        vcode_reg_t count)
{
   // Search backwards for a storage hint with this type and count
   VCODE_FOR_EACH_MATCHING_OP(other, VCODE_OP_STORAGE_HINT) {
      if (vtype_eq(other->type, type)
          && ((other->args.count == 1 && count == VCODE_INVALID_REG)
              || other->args.items[1] == count)) {
         other->comment = xasprintf("Used storage hint for r%d",
                                    other->args.items[0]);
         other->kind = VCODE_OP_COMMENT;
         return other->args.items[0];
      }
   }

   op_t *op = vcode_add_op(VCODE_OP_ALLOCA);
   op->type    = type;
   op->result  = vcode_add_reg(vtype_pointer(type));
   op->subkind = VCODE_ALLOCA_STACK;

   if (count != VCODE_INVALID_REG)
      vcode_add_arg(op, count);

   const vtype_kind_t tkind = vtype_kind(type);
   VCODE_ASSERT(tkind != VCODE_TYPE_CARRAY && tkind != VCODE_TYPE_UARRAY,
                "alloca element type cannot be array");

   reg_t *r = vcode_reg_data(op->result);
   r->bounds = bounds;

   return op->result;
}

vcode_reg_t emit_const(vcode_type_t type, int64_t value)
{
   // Reuse any previous constant in this block with the same type and value
   VCODE_FOR_EACH_MATCHING_OP(other, VCODE_OP_CONST) {
      if (other->kind == VCODE_OP_CONST && other->value == value
          && vtype_eq(type, other->type))
         return other->result;
   }

   op_t *op = vcode_add_op(VCODE_OP_CONST);
   op->value  = value;
   op->type   = type;
   op->result = vcode_add_reg(type);

   vtype_kind_t type_kind = vtype_kind(type);
   VCODE_ASSERT(type_kind == VCODE_TYPE_INT || type_kind == VCODE_TYPE_OFFSET,
                "constant must have integer or offset type");

   reg_t *r = vcode_reg_data(op->result);
   r->bounds = vtype_int(value, value);

   return op->result;
}

vcode_reg_t emit_const_real(double value)
{
   vcode_type_t real = vtype_real();

   VCODE_FOR_EACH_MATCHING_OP(other, VCODE_OP_CONST_REAL) {
      if (other->real == value && other->type == real)
         return other->result;
   }

   op_t *op = vcode_add_op(VCODE_OP_CONST_REAL);
   op->real   = value;
   op->type   = real;
   op->result = vcode_add_reg(op->type);

   return op->result;
}

vcode_reg_t emit_const_array(vcode_type_t type, vcode_reg_t *values, int num,
                             bool allocate)
{
   vtype_kind_t kind = vtype_kind(type);
   vcode_type_t rtype = allocate && kind == VCODE_TYPE_CARRAY
      ? vtype_pointer(vtype_elem(type))
      : type;

   // Reuse any previous operation in this block with the same arguments
   VCODE_FOR_EACH_MATCHING_OP(other, VCODE_OP_CONST_ARRAY) {
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

   VCODE_ASSERT(
      kind == VCODE_TYPE_CARRAY || (allocate && kind == VCODE_TYPE_POINTER),
      "constant array must have constrained array type");

   reg_t *r = vcode_reg_data(op->result);
   r->bounds = (kind == VCODE_TYPE_POINTER)
      ? vtype_pointed(type) : vtype_elem(type);

   return op->result;
}

vcode_reg_t emit_const_record(vcode_type_t type, vcode_reg_t *values, int num)
{
   // Reuse any previous constant in this block with the same type and value
   VCODE_FOR_EACH_MATCHING_OP(other, VCODE_OP_CONST_RECORD) {
      if (vtype_eq(type, other->type) && other->args.count == num) {
         bool same_regs = true;
         for (int i = 0; i < num; i++)
            same_regs = same_regs && other->args.items[i] == values[i];

         if (same_regs && vtype_eq(vcode_reg_type(other->result), type))
            return other->result;
      }
   }

   op_t *op = vcode_add_op(VCODE_OP_CONST_RECORD);
   op->type   = type;
   op->result = vcode_add_reg(type);

   for (int i = 0; i < num; i++)
      vcode_add_arg(op, values[i]);

   VCODE_ASSERT(vtype_kind(type) == VCODE_TYPE_RECORD,
                "constant record must have record type");

   VCODE_ASSERT(vtype_fields(type) == num, "expected %d fields but have %d",
                vtype_fields(type), num);

   for (int i = 0; i < num; i++) {
      VCODE_ASSERT(vtype_eq(vtype_field(type, i), vcode_reg_type(values[i])),
                   "wrong type for field %d", i);

      op_t *defn = vcode_find_definition(values[i]);
      VCODE_ASSERT(defn != NULL, "constant record uses parameter r%d",
                   values[i]);
      VCODE_ASSERT(defn->kind == VCODE_OP_CONST
                   || defn->kind == VCODE_OP_CONST_REAL
                   || defn->kind == VCODE_OP_CONST_RECORD
                   || defn->kind == VCODE_OP_CONST_ARRAY
                   || defn->kind == VCODE_OP_NULL,
                   "constant record field r%d is not constant", values[i]);
   }

   return op->result;
}

void emit_wait(vcode_block_t target, vcode_reg_t time)
{
   op_t *op = vcode_add_op(VCODE_OP_WAIT);
   vcode_add_target(op, target);
   vcode_add_arg(op, time);

   VCODE_ASSERT(active_unit->kind == VCODE_UNIT_PROCEDURE
                || active_unit->kind == VCODE_UNIT_PROCESS,
                "wait only allowed in process or procedure");
}

void emit_jump(vcode_block_t target)
{
   op_t *op = vcode_add_op(VCODE_OP_JUMP);
   vcode_add_target(op, target);
}

vcode_var_t emit_var(vcode_type_t type, vcode_type_t bounds, ident_t name,
                     bool is_const)
{
   assert(active_unit != NULL);

   vcode_var_t var = active_unit->vars.count;
   var_t *v = var_array_alloc(&(active_unit->vars));
   memset(v, '\0', sizeof(var_t));
   v->type     = type;
   v->bounds   = bounds;
   v->name     = name;
   v->is_const = is_const;
   v->use_heap = false;

   return MAKE_HANDLE(active_unit->depth, var);
}

vcode_var_t emit_extern_var(vcode_type_t type, vcode_type_t bounds,
                            ident_t name)
{
   assert(active_unit != NULL);
   assert(active_unit->kind == VCODE_UNIT_CONTEXT);

   // Try to find an existing extern with this name
   for (unsigned i = 0; i < active_unit->vars.count; i++) {
      var_t *v = &(active_unit->vars.items[i]);
      if (v->is_extern && v->name == name)
         return MAKE_HANDLE(active_unit->depth, i);
   }

   vcode_var_t var = emit_var(type, bounds, name, false);
   vcode_var_data(var)->is_extern = true;
   return var;
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

vcode_signal_t emit_extern_signal(vcode_type_t type, vcode_type_t bounds,
                                  ident_t name)
{
   assert(active_unit != NULL);
   assert(active_unit->kind == VCODE_UNIT_CONTEXT);

   // Try to find an existing extern with this name
   for (unsigned i = 0; i < active_unit->vars.count; i++) {
      signal_t *s = &(active_unit->signals.items[i]);
      if (s->is_extern && s->name == name)
         return i;
   }

   vcode_signal_t sig = emit_signal(type, bounds, name, VCODE_INVALID_VAR,
                                    NULL, 0);
   vcode_signal_data(sig)->is_extern = true;
   return sig;
}

vcode_reg_t emit_load(vcode_var_t var)
{
   // Try scanning backwards through the block for another load or store to
   // this variable
   vcode_reg_t fold = VCODE_INVALID_REG;
   bool aliased = false;
   VCODE_FOR_EACH_OP(other) {
      if (fold == VCODE_INVALID_REG) {
         if (other->kind == VCODE_OP_LOAD && other->address == var)
            fold = other->result;
         else if (other->kind == VCODE_OP_STORE && other->address == var)
            fold = other->args.items[0];
      }

      if (other->kind == VCODE_OP_INDEX && other->address == var)
         aliased = true;
   }

   var_t *v = vcode_var_data(var);

   if (fold != VCODE_INVALID_REG && !aliased
       && (v->is_const || MASK_CONTEXT(var) == active_unit->depth))
      return fold;

   op_t *op = vcode_add_op(VCODE_OP_LOAD);
   op->address = var;
   op->result  = vcode_add_reg(v->type);

   vtype_kind_t type_kind = vtype_kind(v->type);
   VCODE_ASSERT(
      type_kind == VCODE_TYPE_INT || type_kind == VCODE_TYPE_OFFSET
      || type_kind == VCODE_TYPE_UARRAY || type_kind == VCODE_TYPE_POINTER
      || type_kind == VCODE_TYPE_FILE || type_kind == VCODE_TYPE_ACCESS
      || type_kind == VCODE_TYPE_REAL,
      "cannot load non-scalar type");

   reg_t *r = vcode_reg_data(op->result);
   r->bounds = v->bounds;

   return op->result;
}

vcode_reg_t emit_load_indirect(vcode_reg_t reg)
{
   op_t *op = vcode_add_op(VCODE_OP_LOAD_INDIRECT);
   vcode_add_arg(op, reg);

   vcode_type_t rtype = vcode_reg_type(reg);

   VCODE_ASSERT(vtype_kind(rtype) == VCODE_TYPE_POINTER,
                "load indirect with non-pointer argument");

   vcode_type_t deref = vtype_pointed(rtype);
   op->result = vcode_add_reg(deref);

   vtype_kind_t type_kind = vtype_kind(deref);
   VCODE_ASSERT(
      type_kind == VCODE_TYPE_INT || type_kind == VCODE_TYPE_OFFSET
      || type_kind == VCODE_TYPE_UARRAY || type_kind == VCODE_TYPE_POINTER
      || type_kind == VCODE_TYPE_FILE || type_kind == VCODE_TYPE_ACCESS
      || type_kind == VCODE_TYPE_REAL,
      "cannot load non-scalar type");

   vcode_reg_data(op->result)->bounds = vcode_reg_data(reg)->bounds;

   return op->result;
}

void emit_store(vcode_reg_t reg, vcode_var_t var)
{
   // Any previous store to this variable in this block is dead
   VCODE_FOR_EACH_OP(other) {
      if (other->kind == VCODE_OP_STORE && other->address == var) {
         other->kind = VCODE_OP_COMMENT;
         other->comment =
            xasprintf("Dead store to %s", istr(vcode_var_name(var)));
      }
      else if (other->kind == VCODE_OP_NESTED_FCALL
               || other->kind == VCODE_OP_NESTED_PCALL)
         break;   // Needs to get variable for display
   }

   var_t *v = vcode_var_data(var);
   reg_t *r = vcode_reg_data(reg);

   op_t *op = vcode_add_op(VCODE_OP_STORE);
   vcode_add_arg(op, reg);
   op->address = var;

   VCODE_ASSERT(vtype_eq(v->type, r->type),
                "variable and stored value do not have same type");
}

void emit_store_indirect(vcode_reg_t reg, vcode_reg_t ptr)
{
   reg_t *p = vcode_reg_data(ptr);
   reg_t *r = vcode_reg_data(reg);

   op_t *op = vcode_add_op(VCODE_OP_STORE_INDIRECT);
   vcode_add_arg(op, reg);
   vcode_add_arg(op, ptr);

   VCODE_ASSERT(vtype_kind(p->type) == VCODE_TYPE_POINTER,
                "store indirect target is not a pointer");
   VCODE_ASSERT(vtype_eq(vtype_pointed(p->type), r->type),
                "pointer and stored value do not have same type");
}

static vcode_reg_t emit_arith(vcode_op_t kind, vcode_reg_t lhs, vcode_reg_t rhs,
                              uint32_t index)
{
   // Reuse any previous operation in this block with the same arguments
   VCODE_FOR_EACH_MATCHING_OP(other, kind) {
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
   else
      VCODE_ASSERT(vtype_eq(lhs_type, rhs_type),
                   "arguments to %s are not the same type",
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
   const bool l_is_const = vcode_reg_const(lhs, &lconst);
   const bool r_is_const = vcode_reg_const(rhs, &rconst);
   if (l_is_const && r_is_const)
      return emit_const(vcode_reg_type(lhs), lconst + rconst);
   else if (r_is_const && rconst == 0)
      return lhs;
   else if (l_is_const && lconst == 0)
      return rhs;

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
   const bool l_is_const = vcode_reg_const(lhs, &lconst);
   const bool r_is_const = vcode_reg_const(rhs, &rconst);
   if (l_is_const && r_is_const)
      return emit_const(vcode_reg_type(lhs), lconst - rconst);
   else if (r_is_const && rconst == 0)
      return lhs;
   else if (l_is_const && lconst == 0)
      return rhs;

   vcode_reg_t reg = emit_arith(VCODE_OP_SUB, lhs, rhs, UINT32_MAX);

   reg_t *rr = vcode_reg_data(reg);
   if (vtype_kind(vcode_reg_type(reg)) == VCODE_TYPE_POINTER)
      rr->bounds = vcode_reg_data(lhs)->bounds;
   else {
      vtype_t *bl = vcode_type_data(vcode_reg_data(lhs)->bounds);
      vtype_t *br = vcode_type_data(vcode_reg_data(rhs)->bounds);

      reg_t *rr = vcode_reg_data(reg);
      // XXX: this is wrong - see TO_UNSIGNED
      rr->bounds = vtype_int(sadd64(bl->low, -br->high),
                             sadd64(bl->high, -br->low));
   }

   return reg;
}

void emit_bounds(vcode_reg_t reg, vcode_type_t bounds, bounds_kind_t kind,
                 uint32_t index, uint32_t hint)
{
   if (reg == VCODE_INVALID_REG)
      return;
   else if (vtype_includes(bounds, vcode_reg_data(reg)->bounds)) {
      emit_comment("Elided bounds check for r%d", reg);
      return;
   }

   op_t *op = vcode_add_op(VCODE_OP_BOUNDS);
   vcode_add_arg(op, reg);
   op->type    = bounds;
   op->subkind = kind;
   op->index   = index;
   op->hint    = hint;

   const vtype_kind_t tkind = vtype_kind(bounds);
   VCODE_ASSERT(tkind == VCODE_TYPE_INT || tkind == VCODE_TYPE_REAL,
                "type argument to bounds must be integer or real");

   const vtype_kind_t rkind = vcode_reg_kind(reg);
   VCODE_ASSERT(rkind == VCODE_TYPE_INT || tkind == VCODE_TYPE_REAL,
                "reg argument to bounds must be integer or real");
}

vcode_reg_t emit_index(vcode_var_t var, vcode_reg_t offset)
{
   // Try to find a previous index of this var by this offset
   VCODE_FOR_EACH_MATCHING_OP(other, VCODE_OP_INDEX) {
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
   case VCODE_TYPE_ACCESS:
   case VCODE_TYPE_REAL:
      op->type = vtype_pointer(typeref);
      op->result = vcode_add_reg(op->type);
      break;

   default:
      VCODE_ASSERT(false, "variable %s cannot be indexed",
                   istr(vcode_var_name(var)));
   }

   if (offset != VCODE_INVALID_REG)
      VCODE_ASSERT(vtype_kind(vcode_reg_type(offset)) == VCODE_TYPE_OFFSET,
                   "index offset r%d does not have offset type", offset);

   return op->result;
}

vcode_reg_t emit_cast(vcode_type_t type, vcode_type_t bounds, vcode_reg_t reg)
{
   if (vtype_eq(vcode_reg_type(reg), type))
      return reg;

   vtype_kind_t from = vtype_kind(vcode_reg_type(reg));
   vtype_kind_t to   = vtype_kind(type);

   const bool integral =
      (from == VCODE_TYPE_OFFSET || from == VCODE_TYPE_INT)
      && (to == VCODE_TYPE_OFFSET || to == VCODE_TYPE_INT);

   int64_t value;
   if (integral && vcode_reg_const(reg, &value))
      return emit_const(type, value);

   // Try to find a previous cast of this register to this type
   VCODE_FOR_EACH_MATCHING_OP(other, VCODE_OP_CAST) {
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
      { VCODE_TYPE_INT,    VCODE_TYPE_INT     },
      { VCODE_TYPE_INT,    VCODE_TYPE_REAL    },
      { VCODE_TYPE_REAL,   VCODE_TYPE_INT     },
   };

   if (from == VCODE_TYPE_INT && bounds == VCODE_INVALID_TYPE) {
      reg_t *rr = vcode_reg_data(op->result);
      rr->bounds = vcode_reg_bounds(reg);
   }
   else if (to == VCODE_TYPE_INT && bounds != VCODE_INVALID_TYPE) {
      reg_t *rr = vcode_reg_data(op->result);
      rr->bounds = bounds;
   }

   for (size_t i = 0; i < ARRAY_LEN(allowed); i++) {
      if (from == allowed[i][0] && to == allowed[i][1])
         return op->result;
   }

   VCODE_ASSERT(false, "invalid type conversion in cast");
}

void emit_return(vcode_reg_t reg)
{
   op_t *op = vcode_add_op(VCODE_OP_RETURN);
   if (reg != VCODE_INVALID_REG) {
      vcode_add_arg(op, reg);

      VCODE_ASSERT(active_unit->kind == VCODE_UNIT_FUNCTION,
                   "returning value fron non-function unit");
      VCODE_ASSERT(vtype_eq(active_unit->result, vcode_reg_type(reg)),
                   "return value incorrect type");

      vtype_kind_t rkind = vcode_reg_kind(reg);
      if (rkind == VCODE_TYPE_POINTER || rkind == VCODE_TYPE_UARRAY)
         vcode_heap_allocate(reg);
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

   VCODE_ASSERT(vtype_kind(stype) == VCODE_TYPE_SIGNAL,
                "argument to nets is not a signal");

   op->result = vcode_add_reg(stype);

   reg_t *rr = vcode_reg_data(op->result);
   rr->bounds = vcode_signal_bounds(sig);

   return op->result;
}

void emit_sched_waveform(vcode_reg_t nets, vcode_reg_t nnets,
                         vcode_reg_t values, vcode_reg_t reject,
                         vcode_reg_t after)
{
   int64_t nconst;
   if (vcode_reg_const(nnets, &nconst) && nconst == 0) {
      emit_comment("Skip empty waveform");
      return;
   }

   op_t *op = vcode_add_op(VCODE_OP_SCHED_WAVEFORM);
   vcode_add_arg(op, nets);
   vcode_add_arg(op, nnets);
   vcode_add_arg(op, values);
   vcode_add_arg(op, reject);
   vcode_add_arg(op, after);

   VCODE_ASSERT(vcode_reg_kind(nets) == VCODE_TYPE_SIGNAL,
                "sched_waveform target is not signal");
   VCODE_ASSERT(vcode_reg_kind(nnets) == VCODE_TYPE_OFFSET,
                "sched_waveform net count is not offset type");
   VCODE_ASSERT(vcode_reg_kind(values) != VCODE_TYPE_SIGNAL,
                "signal cannot be values argument for sched_waveform");
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

   VCODE_ASSERT(vtype_eq(vcode_reg_type(test), vtype_bool()),
                "cond test is not a bool");
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
      vtype_uarray(1, vtype_char(), vtype_int(0, 127)));

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

   // Find a previous identical select
   VCODE_FOR_EACH_MATCHING_OP(other, VCODE_OP_SELECT) {
      if (other->args.items[0] == test && other->args.items[1] == rtrue
          && other->args.items[2] == rfalse)
         return other->result;
   }

   op_t *op = vcode_add_op(VCODE_OP_SELECT);
   vcode_add_arg(op, test);
   vcode_add_arg(op, rtrue);
   vcode_add_arg(op, rfalse);
   op->result = vcode_add_reg(vcode_reg_type(rtrue));

   VCODE_ASSERT(vtype_eq(vcode_reg_type(test), vtype_bool()),
                "select test must have bool type");
   VCODE_ASSERT(vtype_eq(vcode_reg_type(rtrue), vcode_reg_type(rfalse)),
                "select arguments are not the same type");

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

   VCODE_ASSERT(vtype_eq(vcode_reg_type(lhs), vtbool)
                && vtype_eq(vcode_reg_type(rhs), vtbool),
                "arguments to %s are not boolean", vcode_op_string(op));

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
   VCODE_ASSERT(vtype_eq(vcode_reg_type(arg), vtbool),
                "argument to not is not boolean");

   return (op->result = vcode_add_reg(vtbool));
}

vcode_reg_t emit_wrap(vcode_reg_t data, const vcode_dim_t *dims, int ndims)
{
   VCODE_FOR_EACH_MATCHING_OP(other, VCODE_OP_WRAP) {
      if (other->args.count == ndims*3 + 1 && other->args.items[0] == data) {
         bool match = true;
         for (int i = 0; match && i < ndims; i++) {
            match = other->args.items[i*3 + 1] == dims[i].left
               && other->args.items[i*3 + 2] == dims[i].right
               && other->args.items[i*3 + 3] == dims[i].dir;
         }
         if (match)
            return other->result;
      }
   }

   op_t *op = vcode_add_op(VCODE_OP_WRAP);
   vcode_add_arg(op, data);
   for (int i = 0; i < ndims; i++) {
      vcode_add_arg(op, dims[i].left);
      vcode_add_arg(op, dims[i].right);
      vcode_add_arg(op, dims[i].dir);
   }

   vcode_type_t ptr_type = vcode_reg_type(data);
   const vtype_kind_t ptrkind = vtype_kind(ptr_type);
   VCODE_ASSERT(ptrkind == VCODE_TYPE_POINTER || ptrkind == VCODE_TYPE_SIGNAL,
                "wrapped data is not pointer or signal");

   vcode_type_t elem = (ptrkind == VCODE_TYPE_POINTER)
      ? vtype_pointed(ptr_type) : ptr_type;

   op->result = vcode_add_reg(
      vtype_uarray(ndims, elem, vcode_reg_bounds(data)));

   return op->result;
}

static vcode_reg_t emit_uarray_op(vcode_op_t o, vcode_type_t rtype,
                                  vcode_reg_t array, unsigned dim,
                                  unsigned arg_index)
{
   // Reuse any previous operation in this block with the same arguments
   VCODE_FOR_EACH_OP(other) {
      if (other->kind == o && other->args.items[0] == array && other->dim == dim
          && (rtype == VCODE_INVALID_TYPE
              || vtype_eq(rtype, vcode_reg_type(other->result))))
         return other->result;
      else if (other->kind == VCODE_OP_WRAP && other->result == array)
         return other->args.items[1 + (dim * 3) + arg_index];
   }

   op_t *op = vcode_add_op(o);
   vcode_add_arg(op, array);
   op->dim = dim;

   vcode_type_t atype = vcode_reg_type(array);
   VCODE_ASSERT(vtype_kind(atype) == VCODE_TYPE_UARRAY,
                "cannot use %s with non-uarray type", vcode_op_string(o));

   vtype_t *vt = vcode_type_data(atype);
   VCODE_ASSERT(dim < vt->dims, "invalid dimension %d", dim);

   if (rtype == VCODE_INVALID_TYPE)
      rtype = vtype_offset();

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

vcode_reg_t emit_uarray_len(vcode_reg_t array, unsigned dim)
{
   VCODE_FOR_EACH_MATCHING_OP(other, VCODE_OP_UARRAY_LEN) {
      if (other->args.items[0] == array && other->dim == dim)
         return other->result;
   }

   op_t *op = vcode_add_op(VCODE_OP_UARRAY_LEN);
   vcode_add_arg(op, array);
   op->dim = dim;

   vcode_type_t atype = vcode_reg_type(array);
   VCODE_ASSERT(vtype_kind(atype) == VCODE_TYPE_UARRAY,
                "cannot use uarray len with non-uarray type");

   vtype_t *vt = vcode_type_data(atype);
   VCODE_ASSERT(dim < vt->dims, "invalid dimension %d", dim);

   return (op->result = vcode_add_reg(vtype_offset()));
}

vcode_reg_t emit_unwrap(vcode_reg_t array)
{
   VCODE_FOR_EACH_OP(other) {
      if (other->kind == VCODE_OP_WRAP && other->result == array)
         return other->args.items[0];
      else if (other->kind == VCODE_OP_UNWRAP && other->args.items[0] == array)
         return other->result;
   }

   op_t *op = vcode_add_op(VCODE_OP_UNWRAP);
   vcode_add_arg(op, array);

   vtype_t *vt = vcode_type_data(vcode_reg_type(array));
   VCODE_ASSERT(vt->kind == VCODE_TYPE_UARRAY,
                "unwrap can only only be used with uarray types");

   vcode_type_t rtype = (vtype_kind(vt->elem) == VCODE_TYPE_SIGNAL)
      ? vt->elem : vtype_pointer(vt->elem);

   op->result = vcode_add_reg(rtype);

   reg_t *rr = vcode_reg_data(op->result);
   rr->bounds = vt->elem;

   return op->result;
}

vcode_reg_t emit_param_upref(int hops, vcode_reg_t reg)
{
   op_t *op = vcode_add_op(VCODE_OP_PARAM_UPREF);
   op->hops = hops;
   vcode_add_arg(op, reg);

   VCODE_ASSERT(hops > 0, "invalid hop count");

   vcode_unit_t vu = active_unit;
   for (int i = 0; i < hops; i++)
      vu = vu->context;

   VCODE_ASSERT(vu->kind != VCODE_UNIT_CONTEXT,
                "upref context is not a subprogram or process");
   VCODE_ASSERT(reg < vu->params.count, "upref register is not a parameter");

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

void emit_set_initial(vcode_signal_t signal, vcode_reg_t value, uint32_t index,
                      ident_t resolution, vcode_type_t type)
{
   op_t *op = vcode_add_op(VCODE_OP_SET_INITIAL);
   op->signal = signal;
   op->index  = index;
   op->func   = resolution;
   op->type   = type;
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

   VCODE_ASSERT(vcode_reg_kind(nets) == VCODE_TYPE_SIGNAL,
                "argument to %s is not a signal", vcode_op_string(opkind));

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
   VCODE_FOR_EACH_MATCHING_OP(other, VCODE_OP_RECORD_REF) {
      if (other->args.items[0] == record && other->field == field)
         return other->result;
   }

   op_t *op = vcode_add_op(VCODE_OP_RECORD_REF);
   op->field = field;
   vcode_add_arg(op, record);

   vtype_t *rptype = vcode_type_data(vcode_reg_type(record));

   VCODE_ASSERT(
      rptype->kind == VCODE_TYPE_POINTER
      || rptype->kind == VCODE_TYPE_ACCESS,
      "argument to record ref must be a pointer or access");

   vtype_t *rtype = vcode_type_data(rptype->pointed);

   VCODE_ASSERT(rtype->kind == VCODE_TYPE_RECORD,
                "argument must be pointer to record");

   VCODE_ASSERT(field < rtype->fields.count, "invalid field %d", field);

   vcode_type_t field_type  = rtype->fields.items[field];
   vcode_type_t bounds_type = field_type;

   vcode_type_t result_type = VCODE_INVALID_TYPE;

   if (vtype_kind(field_type) == VCODE_TYPE_CARRAY) {
      bounds_type = vtype_elem(field_type);
      result_type = vtype_pointer(bounds_type);
   }
   else
      result_type = vtype_pointer(field_type);

   op->result = vcode_add_reg(result_type);

   reg_t *rr = vcode_reg_data(op->result);
   rr->bounds = bounds_type;

   return op->result;
}

void emit_copy(vcode_reg_t dest, vcode_reg_t src, vcode_reg_t count)
{
   int64_t cconst;
   if (count != VCODE_INVALID_REG && vcode_reg_const(count, &cconst)
       && cconst == 0)
      return;
   else if (dest == src)
      return;

   op_t *op = vcode_add_op(VCODE_OP_COPY);
   vcode_add_arg(op, dest);
   vcode_add_arg(op, src);
   if (count != VCODE_INVALID_REG)
      vcode_add_arg(op, count);

   vcode_type_t dtype = vcode_reg_type(dest);
   vcode_type_t stype = vcode_reg_type(src);

   vtype_kind_t dkind = vtype_kind(dtype);
   vtype_kind_t skind = vtype_kind(stype);

   VCODE_ASSERT(dkind == VCODE_TYPE_POINTER || dkind == VCODE_TYPE_ACCESS,
                "destination type is not a pointer or access");
   VCODE_ASSERT(skind == VCODE_TYPE_POINTER || skind == VCODE_TYPE_ACCESS,
                "source type is not a pointer or access");
   VCODE_ASSERT(vtype_eq(dtype, stype),
                "source and destination types do not match");
   VCODE_ASSERT(count == VCODE_INVALID_REG
                || vcode_reg_kind(count) == VCODE_TYPE_OFFSET,
                "count is not offset type");

   op->type = vtype_pointed(dtype);
}

void emit_sched_event(vcode_reg_t nets, vcode_reg_t n_elems, unsigned flags)
{
   op_t *op = vcode_add_op(VCODE_OP_SCHED_EVENT);
   vcode_add_arg(op, nets);
   vcode_add_arg(op, n_elems);
   op->subkind = flags;

   VCODE_ASSERT(vcode_reg_kind(nets) == VCODE_TYPE_SIGNAL,
                "nets argument to sched event must be signal");
}

void emit_resume(ident_t func, bool nested, int hops)
{
   op_t *op = vcode_add_op(VCODE_OP_RESUME);
   op->func    = func;
   op->subkind = nested;
   op->hops    = hops;

   block_t *b = &(active_unit->blocks.items[active_block]);
   VCODE_ASSERT(b->ops.count == 1, "resume must be first op in a block");
}

vcode_reg_t emit_memcmp(vcode_reg_t lhs, vcode_reg_t rhs, vcode_reg_t len)
{
   op_t *op = vcode_add_op(VCODE_OP_MEMCMP);
   vcode_add_arg(op, lhs);
   vcode_add_arg(op, rhs);
   vcode_add_arg(op, len);

   VCODE_ASSERT(vtype_kind(vcode_reg_type(lhs)) == VCODE_TYPE_POINTER,
                "LHS of memcmp must have pointer type");
   VCODE_ASSERT(vtype_kind(vcode_reg_type(rhs)) == VCODE_TYPE_POINTER,
                "RHS of memcmp must have pointer type");
   VCODE_ASSERT(vtype_kind(vcode_reg_type(len)) == VCODE_TYPE_OFFSET,
                "length of memcmp must have offset type");

   return (op->result = vcode_add_reg(vtype_bool()));
}

void emit_memset(vcode_reg_t ptr, vcode_reg_t value, vcode_reg_t len)
{
   op_t *op = vcode_add_op(VCODE_OP_MEMSET);
   vcode_add_arg(op, ptr);
   vcode_add_arg(op, value);
   vcode_add_arg(op, len);

   VCODE_ASSERT(vtype_kind(vcode_reg_type(ptr)) == VCODE_TYPE_POINTER,
                "target of memset must have pointer type");
   VCODE_ASSERT(vtype_kind(vcode_reg_type(value)) == VCODE_TYPE_INT,
                "value of memset must have integer type");
   VCODE_ASSERT(vtype_kind(vcode_reg_type(len)) == VCODE_TYPE_OFFSET,
                "length of memset must have offset type");
   VCODE_ASSERT(vtype_high(vcode_reg_type(value)) <= UINT8_MAX,
                "memset value must be max 8-bit");
}

vcode_reg_t emit_vec_load(vcode_reg_t signal, vcode_reg_t length,
                          bool last_value)
{
   VCODE_FOR_EACH_MATCHING_OP(other, VCODE_OP_VEC_LOAD) {
      if (other->args.items[0] == signal
          && (other->args.count == 1 || other->args.items[1] == length)
          && other->subkind == last_value)
         return other->result;
   }

   op_t *op = vcode_add_op(VCODE_OP_VEC_LOAD);
   vcode_add_arg(op, signal);
   if (length != VCODE_INVALID_REG)
      vcode_add_arg(op, length);
   op->subkind = last_value;

   vcode_type_t signal_type = vcode_reg_type(signal);

   VCODE_ASSERT(vtype_kind(signal_type) == VCODE_TYPE_SIGNAL,
                "signal of vec load must have signal type");
   VCODE_ASSERT(length == VCODE_INVALID_REG
                || vcode_reg_kind(length) == VCODE_TYPE_OFFSET,
                "length of vec load must have offset type");

   vcode_type_t base_type = vtype_base(signal_type);
   if (vtype_kind(base_type) == VCODE_TYPE_CARRAY)
      base_type = vtype_elem(base_type);

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

   VCODE_ASSERT(vtype_kind(vcode_reg_type(file)) == VCODE_TYPE_FILE,
                "endfile argument must have file type");

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

   VCODE_ASSERT(vtype_is_pointer(vcode_reg_type(file), VCODE_TYPE_FILE),
                "file open first argument must have file pointer type");
}

void emit_file_write(vcode_reg_t file, vcode_reg_t value, vcode_reg_t length)
{
   op_t *op = vcode_add_op(VCODE_OP_FILE_WRITE);
   vcode_add_arg(op, file);
   vcode_add_arg(op, value);
   if (length != VCODE_INVALID_REG)
      vcode_add_arg(op, length);

   VCODE_ASSERT(vtype_is_pointer(vcode_reg_type(file), VCODE_TYPE_FILE),
                "file write first argument must have file pointer type");

   active_unit->pure = false;
}

void emit_file_close(vcode_reg_t file)
{
   op_t *op = vcode_add_op(VCODE_OP_FILE_CLOSE);
   vcode_add_arg(op, file);

   VCODE_ASSERT(vtype_is_pointer(vcode_reg_type(file), VCODE_TYPE_FILE),
                "file close argument must have file pointer type");
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

   VCODE_ASSERT(vtype_is_pointer(vcode_reg_type(file), VCODE_TYPE_FILE),
                "file read first argument must have file pointer type");
   VCODE_ASSERT(vtype_kind(vcode_reg_type(ptr)) == VCODE_TYPE_POINTER,
                "file read pointer argument must have pointer type");
   VCODE_ASSERT(outlen == VCODE_INVALID_REG
                || vtype_kind(vcode_reg_type(outlen)) == VCODE_TYPE_POINTER,
                "file read outlen argument must have pointer type");

   active_unit->pure = false;
}

vcode_reg_t emit_null(vcode_type_t type)
{
   VCODE_FOR_EACH_MATCHING_OP(other, VCODE_OP_NULL) {
      if (vtype_eq(vcode_reg_type(other->result), type))
         return other->result;
   }

   op_t *op = vcode_add_op(VCODE_OP_NULL);

   vtype_kind_t kind = vtype_kind(type);
   VCODE_ASSERT(kind == VCODE_TYPE_POINTER || kind == VCODE_TYPE_FILE
                || kind == VCODE_TYPE_ACCESS,
                "null type must be file, access, or pointer");

   return (op->result = vcode_add_reg(type));
}

vcode_reg_t emit_new(vcode_type_t type, vcode_reg_t length)
{
   op_t *op = vcode_add_op(VCODE_OP_NEW);
   if (length != VCODE_INVALID_REG)
      vcode_add_arg(op, length);

   vtype_kind_t kind = vtype_kind(type);
   VCODE_ASSERT(kind == VCODE_TYPE_INT || kind == VCODE_TYPE_RECORD
                || kind == VCODE_TYPE_UARRAY || kind == VCODE_TYPE_ACCESS,
                "new type must be int, record, access, or uarray");
   VCODE_ASSERT(length == VCODE_INVALID_REG
                || vtype_kind(vcode_reg_type(length)) == VCODE_TYPE_OFFSET,
                "new length must have offset type");

   return (op->result = vcode_add_reg(vtype_access(type)));
}

void emit_null_check(vcode_reg_t ptr, uint32_t index)
{
   VCODE_FOR_EACH_MATCHING_OP(other, VCODE_OP_NULL_CHECK) {
      if (other->args.items[0] == ptr)
         return;
   }

   op_t *op = vcode_add_op(VCODE_OP_NULL_CHECK);
   vcode_add_arg(op, ptr);
   op->index = index;

   VCODE_ASSERT(vtype_kind(vcode_reg_type(ptr)) == VCODE_TYPE_ACCESS,
                "null check argument must be an access");
}

void emit_deallocate(vcode_reg_t ptr)
{
   op_t *op = vcode_add_op(VCODE_OP_DEALLOCATE);
   vcode_add_arg(op, ptr);

   vcode_type_t ptype = vcode_reg_type(ptr);
   VCODE_ASSERT(vtype_kind(ptype) == VCODE_TYPE_POINTER
                && vtype_kind(vtype_pointed(ptype)) == VCODE_TYPE_ACCESS,
                "deallocate argument must be pointer to access");
}

vcode_reg_t emit_all(vcode_reg_t reg)
{
   VCODE_FOR_EACH_MATCHING_OP(other, VCODE_OP_ALL) {
      if (other->args.items[0] == reg)
         return other->result;
   }

   op_t *op = vcode_add_op(VCODE_OP_ALL);
   vcode_add_arg(op, reg);

   vcode_type_t vtype = vcode_reg_type(reg);

   VCODE_ASSERT(vtype_kind(vtype) == VCODE_TYPE_ACCESS,
                "all argument must be an access");

   vcode_type_t pointed = vtype_pointed(vtype);
   op->result = vcode_add_reg(vtype_pointer(pointed));

   reg_t *rr = vcode_reg_data(op->result);
   rr->bounds = pointed;

   return op->result;
}

vcode_reg_t emit_bit_vec_op(bit_vec_op_kind_t kind, vcode_reg_t lhs_data,
                            vcode_reg_t lhs_len, vcode_reg_t lhs_dir,
                            vcode_reg_t rhs_data, vcode_reg_t rhs_len,
                            vcode_reg_t rhs_dir, vcode_type_t result)
{
   op_t *op = vcode_add_op(VCODE_OP_BIT_VEC_OP);
   op->subkind = kind;
   vcode_add_arg(op, lhs_data);
   vcode_add_arg(op, lhs_len);
   vcode_add_arg(op, lhs_dir);
   if (rhs_data != VCODE_INVALID_REG) {
      vcode_add_arg(op, rhs_data);
      vcode_add_arg(op, rhs_len);
      vcode_add_arg(op, rhs_dir);
   }

   VCODE_ASSERT(vcode_reg_kind(lhs_data) == VCODE_TYPE_POINTER,
                "LHS to bit vec op must be pointer");
   VCODE_ASSERT(vcode_reg_kind(lhs_len) == VCODE_TYPE_OFFSET,
                "LHS length to bit vec op must be offset");
   if (rhs_data != VCODE_INVALID_REG) {
      VCODE_ASSERT(vcode_reg_kind(rhs_data) == VCODE_TYPE_POINTER,
                   "RHS to bit vec op must be pointer");
      VCODE_ASSERT(vcode_reg_kind(rhs_len) == VCODE_TYPE_OFFSET,
                   "RHS length to bit vec op must be offset");
   }
   VCODE_ASSERT(vtype_kind(result) == VCODE_TYPE_UARRAY,
                "result of bit vec op must be uarray");

   return (op->result = vcode_add_reg(result));
}

vcode_reg_t emit_value(vcode_reg_t string, vcode_reg_t len, uint32_t index)
{
   op_t *op = vcode_add_op(VCODE_OP_VALUE);
   vcode_add_arg(op, string);
   vcode_add_arg(op, len);
   op->index = index;

   VCODE_ASSERT(vcode_reg_kind(string) == VCODE_TYPE_POINTER,
                "string argument to value must be a pointer");
   VCODE_ASSERT(vcode_reg_kind(len) == VCODE_TYPE_OFFSET,
                "length argument to value must be an offset");

   return (op->result = vcode_add_reg(vtype_int(INT64_MIN, INT64_MAX)));
}

vcode_reg_t emit_last_event(vcode_reg_t signal, vcode_reg_t len)
{
   op_t *op = vcode_add_op(VCODE_OP_LAST_EVENT);
   vcode_add_arg(op, signal);
   if (len != VCODE_INVALID_REG)
      vcode_add_arg(op, len);

   VCODE_ASSERT(vcode_reg_kind(signal) == VCODE_TYPE_SIGNAL,
                "signal argument to last event must have signal type");
   VCODE_ASSERT(len == VCODE_INVALID_REG
                || vcode_reg_kind(len) == VCODE_TYPE_OFFSET,
                "length argument to last event must have offset type");

   return (op->result = vcode_add_reg(vtype_time()));
}

void emit_needs_last_value(vcode_signal_t sig)
{
   op_t *op = vcode_add_op(VCODE_OP_NEEDS_LAST_VALUE);
   op->signal = sig;
}

void emit_dynamic_bounds(vcode_reg_t reg, vcode_reg_t low, vcode_reg_t high,
                         vcode_reg_t kind, uint32_t index, uint32_t hint)
{
   int64_t lconst, hconst;
   if (vcode_reg_const(low, &lconst) && vcode_reg_const(high, &hconst)) {
      vcode_type_t bounds = vcode_reg_bounds(reg);
      if (lconst <= vtype_low(bounds) && hconst >= vtype_high(bounds)) {
         emit_comment("Elided dynamic bounds check for r%d", reg);
         return;
      }

      int64_t kconst;
      if (vcode_reg_const(kind, &kconst)) {
         emit_bounds(reg, vtype_int(lconst, hconst), kconst, index, hint);
         return;
      }
   }
   else if (reg == low || reg == high) {
      emit_comment("Elided dynamic bounds check for r%d", reg);
      return;
   }

   op_t *op = vcode_add_op(VCODE_OP_DYNAMIC_BOUNDS);
   vcode_add_arg(op, reg);
   vcode_add_arg(op, low);
   vcode_add_arg(op, high);
   vcode_add_arg(op, kind);
   op->index = index;
   op->hint  = hint;

   VCODE_ASSERT(vtype_eq(vcode_reg_type(low), vcode_reg_type(high)),
                "type mismatch in dynamic bounds range");
   VCODE_ASSERT(vcode_reg_kind(kind) == VCODE_TYPE_OFFSET,
                "dynamic bounds kind argument must be offset");
}

void emit_array_size(vcode_reg_t llen, vcode_reg_t rlen, uint32_t index)
{
   if (rlen == llen)
      return;

   op_t *op = vcode_add_op(VCODE_OP_ARRAY_SIZE);
   vcode_add_arg(op, llen);
   vcode_add_arg(op, rlen);
   op->index = index;
}

static op_t *emit_index_check_null(vcode_reg_t rlow, vcode_reg_t rhigh,
                                   bounds_kind_t kind, uint32_t index)
{
   int64_t rlow_const, rhigh_const;
   const bool null =
      vcode_reg_const(rlow, &rlow_const)
       && vcode_reg_const(rhigh, &rhigh_const)
      && rhigh_const < rlow_const;

   if (null) {
      emit_comment("Index check on null range for r%d and r%d", rlow, rhigh);
      return NULL;
   }

   op_t *op = vcode_add_op(VCODE_OP_INDEX_CHECK);
   vcode_add_arg(op, rlow);
   vcode_add_arg(op, rhigh);
   op->subkind = kind;
   op->index   = index;
   op->type    = VCODE_INVALID_TYPE;

   return op;
}

void emit_index_check(vcode_reg_t rlow, vcode_reg_t rhigh, vcode_type_t bounds,
                      bounds_kind_t kind, uint32_t index)
{
   if (vtype_includes(bounds, vcode_reg_data(rlow)->bounds)
       && vtype_includes(bounds, vcode_reg_data(rhigh)->bounds)) {
      emit_comment("Elided index check for r%d and r%d", rlow, rhigh);
      return;
   }

   op_t *op = emit_index_check_null(rlow, rhigh, kind, index);
   if (op != NULL)
      op->type = bounds;
}

void emit_dynamic_index_check(vcode_reg_t rlow, vcode_reg_t rhigh,
                              vcode_reg_t blow, vcode_reg_t bhigh,
                              bounds_kind_t kind, uint32_t index)
{
   op_t *op = emit_index_check_null(rlow, rhigh, kind, index);
   if (op != NULL) {
      vcode_add_arg(op, blow);
      vcode_add_arg(op, bhigh);
   }
}

vcode_reg_t emit_bit_shift(bit_shift_kind_t kind, vcode_reg_t data,
                           vcode_reg_t len, vcode_reg_t dir, vcode_reg_t shift,
                           vcode_type_t result)
{
   op_t *op = vcode_add_op(VCODE_OP_BIT_SHIFT);
   vcode_add_arg(op, data);
   vcode_add_arg(op, len);
   vcode_add_arg(op, dir);
   vcode_add_arg(op, shift);
   op->subkind = kind;

   VCODE_ASSERT(vcode_reg_kind(shift) == VCODE_TYPE_OFFSET,
                "shift argument must be offset");
   VCODE_ASSERT(vcode_reg_kind(dir) == VCODE_TYPE_INT,
                "dir argument must be offset");
   VCODE_ASSERT(vcode_reg_kind(len) == VCODE_TYPE_OFFSET,
                "len argument must be offset");
   VCODE_ASSERT(vcode_reg_kind(data) == VCODE_TYPE_POINTER,
                "data argument must be offset");
   VCODE_ASSERT(vtype_kind(result) == VCODE_TYPE_UARRAY,
                "result type must be uarray");

   return (op->result = vcode_add_reg(result));
}

void emit_storage_hint(vcode_reg_t mem, vcode_reg_t length)
{
   op_t *op = vcode_add_op(VCODE_OP_STORAGE_HINT);
   vcode_add_arg(op, mem);
   if (length != VCODE_INVALID_REG)
      vcode_add_arg(op, length);

   VCODE_ASSERT(vcode_reg_kind(mem) == VCODE_TYPE_POINTER,
                "storage hint mem must be pointer");
   VCODE_ASSERT(length == VCODE_INVALID_REG
                || vcode_reg_kind(length) == VCODE_TYPE_OFFSET,
                "storage hint length must be offset");

   op->type = vtype_pointed(vcode_reg_type(mem));
}

void emit_debug_out(vcode_reg_t reg)
{
   op_t *op = vcode_add_op(VCODE_OP_DEBUG_OUT);
   vcode_add_arg(op, reg);
}

void emit_cover_stmt(uint32_t tag)
{
   op_t *op = vcode_add_op(VCODE_OP_COVER_STMT);
   op->index = tag;
}

void emit_cover_cond(vcode_reg_t test, uint32_t tag, unsigned sub)
{
   op_t *op = vcode_add_op(VCODE_OP_COVER_COND);
   vcode_add_arg(op, test);
   op->index   = tag;
   op->subkind = sub;
}

vcode_reg_t emit_heap_save(void)
{
   op_t *op = vcode_add_op(VCODE_OP_HEAP_SAVE);
   return (op->result = vcode_add_reg(vtype_offset()));
}

void emit_heap_restore(vcode_reg_t reg)
{
   op_t *op = vcode_add_op(VCODE_OP_HEAP_RESTORE);
   vcode_add_arg(op, reg);

   VCODE_ASSERT(vcode_reg_kind(reg) == VCODE_TYPE_OFFSET,
                "saved heap must have offset type");
   VCODE_ASSERT(vcode_find_definition(reg)->kind == VCODE_OP_HEAP_SAVE,
                "register for heap restore must come from heap save");
}
