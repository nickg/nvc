//
//  Copyright (C) 2014-2021  Nick Gasson
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
#include "hash.h"
#include "tree.h"
#include "common.h"

#include <assert.h>
#include <inttypes.h>
#include <string.h>
#include <stdlib.h>
#include <float.h>

DECLARE_AND_DEFINE_ARRAY(vcode_reg);
DECLARE_AND_DEFINE_ARRAY(vcode_block);
DECLARE_AND_DEFINE_ARRAY(vcode_type);

#define OP_HAS_TYPE(x)                                                  \
   (x == VCODE_OP_BOUNDS || x == VCODE_OP_ALLOCA  || x == VCODE_OP_COPY \
    || x == VCODE_OP_INDEX_CHECK || x == VCODE_OP_CONST                 \
    || x == VCODE_OP_CAST || x == VCODE_OP_CONST_RECORD                 \
    || x == VCODE_OP_CLOSURE)
#define OP_HAS_ADDRESS(x)                                               \
   (x == VCODE_OP_LOAD || x == VCODE_OP_STORE || x == VCODE_OP_INDEX    \
    || x == VCODE_OP_VAR_UPREF)
#define OP_HAS_SUBKIND(x)                                               \
   (x == VCODE_OP_BOUNDS || x == VCODE_OP_INDEX_CHECK                   \
    || x == VCODE_OP_ALLOCA || x == VCODE_OP_COVER_COND                 \
    || x == VCODE_OP_ARRAY_SIZE || x == VCODE_OP_PCALL                  \
    || x == VCODE_OP_FCALL || x == VCODE_OP_RESOLUTION_WRAPPER          \
    || x == VCODE_OP_CLOSURE || x == VCODE_OP_PROTECTED_INIT)
#define OP_HAS_FUNC(x)                                                  \
   (x == VCODE_OP_FCALL || x == VCODE_OP_PCALL || x == VCODE_OP_RESUME  \
    || x == VCODE_OP_CLOSURE || x == VCODE_OP_PROTECTED_INIT)
#define OP_HAS_IDENT(x)                                                 \
   (x == VCODE_OP_LINK_SIGNAL || x == VCODE_OP_LINK_VAR                 \
    || x == VCODE_OP_LINK_PACKAGE)
#define OP_HAS_REAL(x)                                                  \
   (x == VCODE_OP_CONST_REAL)
#define OP_HAS_VALUE(x)                                                 \
   (x == VCODE_OP_CONST || x == VCODE_OP_CONST_REP)
#define OP_HAS_DIM(x)                                                   \
   (x == VCODE_OP_UARRAY_LEFT || x == VCODE_OP_UARRAY_RIGHT             \
    || x == VCODE_OP_UARRAY_DIR || x == VCODE_OP_UARRAY_LEN)
#define OP_HAS_HOPS(x)                                                  \
   (x == VCODE_OP_VAR_UPREF || x == VCODE_OP_CONTEXT_UPREF)
#define OP_HAS_FIELD(x)                                                 \
   (x == VCODE_OP_RECORD_REF)
#define OP_HAS_CMP(x)                                                   \
   (x == VCODE_OP_CMP)
#define OP_HAS_TAG(x)                                                   \
   (x == VCODE_OP_COVER_STMT || x == VCODE_OP_COVER_COND)
#define OP_HAS_COMMENT(x)                                               \
   (x == VCODE_OP_COMMENT)
#define OP_HAS_HINT(x)                                                  \
   (x == VCODE_OP_BOUNDS || x == VCODE_OP_DYNAMIC_BOUNDS                \
    || x == VCODE_OP_ARRAY_SIZE)
#define OP_HAS_TARGET(x)                                                \
   (x == VCODE_OP_WAIT || x == VCODE_OP_JUMP || x == VCODE_OP_COND      \
    || x == VCODE_OP_PCALL || x == VCODE_OP_CASE)

typedef struct {
   vcode_op_t          kind;
   vcode_reg_array_t   args;
   vcode_reg_t         result;
   loc_t               loc;
   vcode_type_t        type;          // OP_HAS_TYPE
   unsigned            subkind;       // OP_HAS_SUBKIND
   union {
      ident_t          func;          // OP_HAS_FUNC
      ident_t          ident;         // OP_HAS_IDENT
      vcode_var_t      address;       // OP_HAS_ADDRESS
   };
   union {
      vcode_block_array_t targets;    // OP_HAS_TARGET
   };
   union {
      vcode_cmp_t      cmp;           // OP_HAS_CMP
      int64_t          value;         // OP_HAS_VALUE
      double           real;          // OP_HAS_REAL
      char            *comment;       // OP_HAS_COMMENT
      unsigned         dim;           // OP_HAS_DIM
      unsigned         hops;          // OP_HAS_HOPS
      unsigned         field;         // OP_HAS_FIELD
      char            *hint;          // OP_HAS_HINT
      uint32_t         tag;           // OP_HAS_TAG
   };
} op_t;

DECLARE_AND_DEFINE_ARRAY(op);

typedef struct {
   op_array_t ops;
   loc_t      last_loc;
} block_t;

typedef struct {
   vcode_type_t type;
   vcode_type_t bounds;
} reg_t;

typedef struct {
   vtype_kind_t kind;
   union {
      struct {
         int64_t low;
         int64_t high;
      };
      struct {
         double rlow;
         double rhigh;
      };
      struct {
         unsigned      dims;
         unsigned      size;
         vcode_type_t  elem;
         vcode_type_t  bounds;
      };
      vcode_type_t pointed;
      vcode_type_t base;
      struct {
         ident_t            name;
         vcode_type_array_t fields;
      };
   };
} vtype_t;

typedef struct {
   vcode_type_t      type;
   vcode_type_t      bounds;
   ident_t           name;
   vcode_var_flags_t flags;
} var_t;

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
DECLARE_AND_DEFINE_ARRAY(vtype);

typedef enum {
   UNIT_UNDEFINED = (1 << 1)
} unit_flags_t;

struct vcode_unit {
   vunit_kind_t   kind;
   vcode_unit_t   context;
   ident_t        name;
   vcode_type_t   result;
   block_array_t  blocks;
   reg_array_t    regs;
   vtype_array_t  types;
   var_array_t    vars;
   param_array_t  params;
   unsigned       depth;
   unit_flags_t   flags;
   vcode_unit_t   children;
   vcode_unit_t   next;
   unsigned       refcount;
   loc_t          loc;
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
   if (_b->ops.count > 0)                                       \
      for (_i = _b->ops.count - 1, name = &(_b->ops.items[_i]); \
           _i >= 0; name = &(_b->ops.items[--_i]))

#define VCODE_FOR_EACH_MATCHING_OP(name, k) \
   VCODE_FOR_EACH_OP(name) if (name->kind == k)

#define VCODE_MAGIC        0x76636f64
#define VCODE_VERSION      9
#define VCODE_CHECK_UNIONS 0

static __thread vcode_unit_t  active_unit = NULL;
static __thread vcode_block_t active_block = VCODE_INVALID_BLOCK;

static hash_t         *registry = NULL;
static vcode_dump_fn_t dump_callback = NULL;
static void           *dump_arg = NULL;

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
   op->loc    = block->last_loc;

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

static var_t *vcode_var_data(vcode_var_t var)
{
   assert(active_unit != NULL);
   assert(var != VCODE_INVALID_VAR);

   return var_array_nth_ptr(&(active_unit->vars), var);
}

void vcode_clear_storage_hint(uint32_t tag)
{
   VCODE_ASSERT(tag != VCODE_INVALID_HINT, "passed invalid hint");

   const int op = tag & 0xfffff;
   const int block = tag >> 20;

   block_t *b = &(active_unit->blocks.items[block]);
   op_t *o = op_array_nth_ptr(&(b->ops), op);

   if (o->kind == VCODE_OP_COMMENT)
      return;

   VCODE_ASSERT(o->kind == VCODE_OP_STORAGE_HINT,
                "operation %d is not a storage hint", op);

   o->comment = xasprintf("Unused storage hint for r%d", o->args.items[0]);
   o->kind = VCODE_OP_COMMENT;
   vcode_reg_array_resize(&(o->args), 0, VCODE_INVALID_REG);
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
   case VCODE_OP_CONST_REP:
   case VCODE_OP_NULL:
   case VCODE_OP_UNDEFINED:
   case VCODE_OP_ADDRESS_OF:
   case VCODE_OP_LINK_VAR:
   case VCODE_OP_LINK_PACKAGE:
      break;

   case VCODE_OP_ALLOCA:
      defn->subkind = VCODE_ALLOCA_HEAP;
      break;

   case VCODE_OP_INDEX:
      vcode_var_data(defn->address)->flags |= VAR_HEAP;
      break;

   case VCODE_OP_VAR_UPREF:
      // TODO: check this
      break;

   case VCODE_OP_ADD:
      // When adding pointers only the first argument is a pointer
      vcode_heap_allocate(defn->args.items[0]);
      break;

   case VCODE_OP_WRAP:
   case VCODE_OP_UNWRAP:
   case VCODE_OP_RESOLVED:
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
      // Must have been safety checked by definition
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
   case VCODE_OP_STORAGE_HINT:
   case VCODE_OP_UARRAY_LEN:
   case VCODE_OP_UARRAY_LEFT:
   case VCODE_OP_UARRAY_RIGHT:
   case VCODE_OP_UARRAY_DIR:
   case VCODE_OP_LAST_EVENT:
   case VCODE_OP_NEG:
   case VCODE_OP_EXP:
   case VCODE_OP_ABS:
   case VCODE_OP_MOD:
   case VCODE_OP_REM:
   case VCODE_OP_ENDFILE:
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

void vcode_unit_unref(vcode_unit_t unit)
{
   assert(unit != NULL);
   assert(unit->refcount > 0);

   if (unit == active_unit)
      vcode_close();

   if (--(unit->refcount) > 0)
      return;

   assert(unit->children == NULL);

   if (unit->context != NULL) {
      if (unit == unit->context->children)
         unit->context->children = unit->next;
      else {
         vcode_unit_t it;
         for (it = unit->context->children;
              it != NULL && it->next != unit;
              it = it->next)
            ;
         assert(it != NULL);
         it->next = it->next->next;
      }

      vcode_unit_unref(unit->context);
   }

   if (unit->name != NULL)
      hash_put(registry, unit->name, NULL);

   for (unsigned i = 0; i < unit->blocks.count; i++) {
      block_t *b = &(unit->blocks.items[i]);

      for (unsigned j = 0; j < b->ops.count; j++) {
         op_t *o = &(b->ops.items[j]);
         if (OP_HAS_COMMENT(o->kind))
            free(o->comment);
         if (OP_HAS_HINT(o->kind))
            free(o->hint);
         free(o->args.items);
      }
      free(b->ops.items);
   }
   free(unit->blocks.items);

   for (unsigned i = 0; i < unit->types.count; i++) {
      vtype_t *vt = &(unit->types.items[i]);
      if (vt->kind == VCODE_TYPE_RECORD)
         free(vt->fields.items);
   }
   free(unit->types.items);

   free(unit->regs.items);
   free(unit->vars.items);
   free(unit->params.items);
   free(unit);
}

vcode_unit_t vcode_unit_next(vcode_unit_t unit)
{
   return unit->next;
}

vcode_unit_t vcode_unit_child(vcode_unit_t unit)
{
   return unit->children;
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
      if (value) *value = bounds->low;
      return true;
   }
   else
      return false;
}

void vcode_opt(void)
{
   // Prune assignments to unused registers

   int *uses LOCAL = xmalloc_array(active_unit->regs.count, sizeof(int));

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
            case VCODE_OP_WRAP:
            case VCODE_OP_TEMP_STACK_MARK:
            case VCODE_OP_EXP:
            case VCODE_OP_UNDEFINED:
            case VCODE_OP_UARRAY_LEN:
            case VCODE_OP_UARRAY_DIR:
            case VCODE_OP_UARRAY_LEFT:
            case VCODE_OP_UARRAY_RIGHT:
            case VCODE_OP_UNWRAP:
            case VCODE_OP_NULL:
            case VCODE_OP_ADDRESS_OF:
               if (uses[o->result] == -1) {
                  vcode_dump_with_mark(j, NULL, NULL);
                  fatal("defintion of r%d does not dominate all uses",
                        o->result);
               }
               else if (uses[o->result] == 0) {
                  if (o->kind == VCODE_OP_CONST)
                     o->kind = (vcode_op_t)-1;
                  else {
                     o->comment = xasprintf("Dead %s definition of r%d",
                                            vcode_op_string(o->kind),
                                            o->result);
                     o->kind = VCODE_OP_COMMENT;
                  }
                  vcode_reg_array_resize(&(o->args), 0, VCODE_INVALID_REG);
                  pruned++;
               }
               uses[o->result] = -1;
               break;

            case VCODE_OP_STORAGE_HINT:
               vcode_dump_with_mark(j, NULL, NULL);
               fatal("Unused storage hint for r%d was not removed",
                     o->args.items[0]);
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

   for (int i = active_unit->blocks.count - 1; i >= 0; i--) {
      block_t *b = &(active_unit->blocks.items[i]);
      op_t *dst = &(b->ops.items[0]);
      size_t copied = 0;
      for (int j = 0; j < b->ops.count; j++) {
         const op_t *src = &(b->ops.items[j]);
         if (src->kind != (vcode_op_t)-1) {
            if (src != dst) {
               assert(dst < src);
               *dst = *src;
            }
            dst++;
            copied++;
         }
      }

      assert(copied <= b->ops.count);
      b->ops.count = copied;
   }
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
         return i;
   }

   return VCODE_INVALID_VAR;
}

ident_t vcode_var_name(vcode_var_t var)
{
   return vcode_var_data(var)->name;
}

vcode_type_t vcode_var_type(vcode_var_t var)
{
   return vcode_var_data(var)->type;
}

vcode_var_flags_t vcode_var_flags(vcode_var_t var)
{
   return vcode_var_data(var)->flags;
}

vcode_op_t vcode_get_op(int op)
{
   return vcode_op_data(op)->kind;
}

ident_t vcode_get_func(int op)
{
   op_t *o = vcode_op_data(op);
   assert(OP_HAS_FUNC(o->kind));
   return o->func;
}

ident_t vcode_get_ident(int op)
{
   op_t *o = vcode_op_data(op);
   assert(OP_HAS_IDENT(o->kind));
   return o->ident;
}

unsigned vcode_get_subkind(int op)
{
   op_t *o = vcode_op_data(op);
   assert(OP_HAS_SUBKIND(o->kind));
   return o->subkind;
}

int64_t vcode_get_value(int op)
{
   op_t *o = vcode_op_data(op);
   assert(OP_HAS_VALUE(o->kind));
   return o->value;
}

double vcode_get_real(int op)
{
   op_t *o = vcode_op_data(op);
   assert(OP_HAS_REAL(o->kind));
   return o->real;
}

vcode_var_t vcode_get_address(int op)
{
   op_t *o = vcode_op_data(op);
   assert(OP_HAS_ADDRESS(o->kind));
   return o->address;
}

unsigned vcode_get_dim(int op)
{
   op_t *o = vcode_op_data(op);
   assert(OP_HAS_DIM(o->kind));
   return o->dim;
}

int vcode_get_hops(int op)
{
   op_t *o = vcode_op_data(op);
   assert(OP_HAS_HOPS(o->kind));
   return o->hops;
}

int vcode_get_field(int op)
{
   op_t *o = vcode_op_data(op);
   assert(OP_HAS_FIELD(o->kind));
   return o->field;
}

vcode_var_t vcode_get_type(int op)
{
   op_t *o = vcode_op_data(op);
   assert(OP_HAS_TYPE(o->kind));
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
   assert(OP_HAS_CMP(o->kind));
   return o->cmp;
}

uint32_t vcode_get_tag(int op)
{
   op_t *o = vcode_op_data(op);
   assert(OP_HAS_TAG(o->kind));
   return o->tag;
}

const loc_t *vcode_get_loc(int op)
{
   op_t *o = vcode_op_data(op);
   return &(o->loc);
}

const char *vcode_get_hint(int op)
{
   op_t *o = vcode_op_data(op);
   assert(OP_HAS_HINT(o->kind));
   return o->hint;
}

vcode_block_t vcode_get_target(int op, int nth)
{
   op_t *o = vcode_op_data(op);
   assert(OP_HAS_TARGET(o->kind));
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
      "cast", "load indirect", "store indirect", "return",
      "sched waveform", "cond", "report", "div", "neg", "exp", "abs", "mod",
      "rem", "alloca", "select", "or", "wrap", "uarray left",
      "uarray right", "uarray dir", "unwrap", "not", "and",
      "event", "active", "const record", "record ref", "copy", "sched event",
      "pcall", "resume", "xor", "xnor", "nand", "nor", "memset",
      "case", "endfile", "file open", "file write", "file close",
      "file read", "null", "new", "null check", "deallocate", "all",
      "const real", "last event", "dynamic bounds", "array size",
      "index check", "storage hint", "debug out", "cover stmt", "cover cond",
      "uarray len", "temp stack mark", "temp stack restore",
      "undefined", "range null", "var upref", "link signal",
      "resolved", "last value", "init signal", "map signal", "drive signal",
      "link var", "resolution wrapper", "last active", "driving",
      "driving value", "address of", "closure", "protected init",
      "context upref", "const rep", "protected free", "sched static",
      "implicit signal", "disconnect", "link package",
   };
   if ((unsigned)op >= ARRAY_LEN(strs))
      return "???";
   else
      return strs[op];
}

static int vcode_dump_reg(vcode_reg_t reg)
{
   int printed;
   if (reg == VCODE_INVALID_REG)
      printed = color_printf("$red$invalid$$");
   else
      printed = color_printf("$green$r%d$$", reg);

   if (dump_callback != NULL)
      printed += (*dump_callback)(VCODE_DUMP_REG, reg, dump_arg);

   return printed;
}

static int vcode_pretty_print_int(int64_t n)
{
   if (n == INT64_MAX)
      return printf("2^63-1");
   else if (n == INT64_MIN)
      return printf("-2^63");
   else if (n == INT64_MIN + 1)
      return printf("-2^63");   // XXX: bug in lexer/parser
   else if (n == INT32_MAX)
      return printf("2^31-1");
   else if (n == INT32_MIN)
      return printf("-2^31");
   else
      return printf("%"PRIi64, n);
}

static int vcode_dump_one_type(vcode_type_t type)
{
   int col = 0;
   vtype_t *vt = vcode_type_data(type);
   switch (vt->kind) {
   case VCODE_TYPE_INT:
      if (vt->low != vt->high) {
         col += vcode_pretty_print_int(vt->low);
         col += printf("..");
         col += vcode_pretty_print_int(vt->high);
      }
      else
         col += vcode_pretty_print_int(vt->low);
      break;

   case VCODE_TYPE_REAL:
      col += printf("%%");
      break;

   case VCODE_TYPE_CARRAY:
      {
         col += printf("[%u] : ", vt->size);
         col += vcode_dump_one_type(vt->elem);
         if (!vtype_eq(vt->elem, vt->bounds)) {
            col += printf(" => ");
            col += vcode_dump_one_type(vt->bounds);
         }
      }
      break;

   case VCODE_TYPE_UARRAY:
      {
         col += printf("[");
         for (unsigned i = 0; i < vt->dims; i++)
            col += printf("%s*", i > 0 ? ", " : "");
         col += printf("] : ");
         col += vcode_dump_one_type(vt->elem);
         if (!vtype_eq(vt->elem, vt->bounds)) {
            col += printf(" => ");
            col += vcode_dump_one_type(vt->bounds);
         }
      }
      break;

   case VCODE_TYPE_POINTER:
      col += printf("@<");
      col += vcode_dump_one_type(vt->pointed);
      col += printf(">");
      break;

   case VCODE_TYPE_ACCESS:
      col += printf("A<");
      col += vcode_dump_one_type(vt->pointed);
      col += printf(">");
      break;

   case VCODE_TYPE_SIGNAL:
      col += printf("$<");
      col += vcode_dump_one_type(vt->base);
      col += printf(">");
      break;

   case VCODE_TYPE_OFFSET:
      col += printf("#");
      break;

   case VCODE_TYPE_RECORD:
      col += printf("%s{}", istr(vt->name));
      break;

   case VCODE_TYPE_FILE:
      col += printf("F<");
      col += vcode_dump_one_type(vt->base);
      col += printf(">");
      break;

   case VCODE_TYPE_OPAQUE:
      col += printf("?");
      break;

   case VCODE_TYPE_RESOLUTION:
      col += printf("R<");
      col += vcode_dump_one_type(vt->base);
      col += printf(">");
      break;

   case VCODE_TYPE_CLOSURE:
      col += printf("C<");
      col += vcode_dump_one_type(vt->base);
      col += printf(">");
      break;

   case VCODE_TYPE_CONTEXT:
      col += printf("P<%s>", istr(vt->name));
      break;
   }

   return col;
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

static void vcode_dump_loc(const loc_t *loc)
{
   if (!loc_invalid_p(loc)) {
      color_printf("%s:%d", loc_file_str(loc), loc->first_line);
   }
}

static void vcode_dump_comment(int col)
{
   vcode_dump_tab(col, 40);
   color_printf("$cyan$// ");
}

static void vcode_dump_type(int col, vcode_type_t type, vcode_type_t bounds)
{
   vcode_dump_comment(col);
   vcode_dump_one_type(type);
   if (!vtype_eq(type, bounds)) {
      printf(" => ");
      vcode_dump_one_type(bounds);
   }
}

static void vcode_dump_result_type(int col, const op_t *op)
{
   if (op->result != VCODE_INVALID_REG) {
      reg_t *r = vcode_reg_data(op->result);
      vcode_dump_type(col, r->type, r->bounds);
   }
}

static int vcode_dump_var(vcode_var_t var, int hops)
{
   vcode_unit_t owner = active_unit;
   while (owner && hops--)
      owner = owner->context;

   if (owner == NULL || var >= owner->vars.count)
      return color_printf("$red$invalid$$");
   else {
      var_t *v = var_array_nth_ptr(&(owner->vars), var);
      return color_printf("$magenta$%s$$", istr(v->name));
   }
}

void vcode_dump_with_mark(int mark_op, vcode_dump_fn_t callback, void *arg)
{
   assert(active_unit != NULL);

   dump_callback = callback;
   dump_arg = arg;

   const vcode_unit_t vu = active_unit;
   vcode_block_t old_block = active_block;

   printf("\n");
   if (vu->name != NULL)
      color_printf("Name       $cyan$%s$$\n", istr(vu->name));
   color_printf("Kind       $cyan$");
   switch (vu->kind) {
   case VCODE_UNIT_PROCESS:   printf("process"); break;
   case VCODE_UNIT_FUNCTION:  printf("function"); break;
   case VCODE_UNIT_PROCEDURE: printf("procedure"); break;
   case VCODE_UNIT_INSTANCE:  printf("instance"); break;
   case VCODE_UNIT_THUNK:     printf("thunk"); break;
   case VCODE_UNIT_PACKAGE:   printf("package"); break;
   case VCODE_UNIT_PROTECTED: printf("protected"); break;
   }
   color_printf("$$\n");
   if (vu->context != NULL)
      color_printf("Context    $cyan$%s$$\n", istr(vu->context->name));
   printf("Blocks     %d\n", vu->blocks.count);
   printf("Registers  %d\n", vu->regs.count);
   printf("Types      %d\n", vu->types.count);

   for (int i = 0; i < vu->types.count; i++) {
      const vtype_t *t = &(vu->types.items[i]);
      if (t->kind == VCODE_TYPE_RECORD) {
         int col = 0;
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
      vcode_dump_type(col, v->type, v->bounds);
      if (v->flags & VAR_SIGNAL)
         col += printf(", signal");
      if (v->flags & VAR_HEAP)
         col += printf(", heap");
      if (v->flags & VAR_CONST)
         col += printf(", constant");
      if (v->flags & VAR_GLOBAL)
         col += printf(", global");
      color_printf("$$\n");
   }

   if (vu->result != VCODE_INVALID_TYPE) {
      color_printf("Result     $cyan$");
      vcode_dump_one_type(vu->result);
      color_printf("$$\n");
   }

   if (vu->kind == VCODE_UNIT_FUNCTION
       || vu->kind == VCODE_UNIT_PROCEDURE
       || (vu->kind == VCODE_UNIT_THUNK && vu->params.count > 0)) {

      printf("Parameters %d\n", vu->params.count);

      for (size_t i = 0; i < vu->params.count; i++) {
         const param_t *p = &(vu->params.items[i]);
         int col = printf("  ");
         col += vcode_dump_reg(p->reg);
         while (col < (dump_callback ? 12 : 8))
            col += printf(" ");
         col += color_printf("$magenta$%s$$", istr(p->name));
         vcode_dump_type(col, p->type, p->bounds);
         color_printf("$$\n");
      }
   }

   printf("Begin\n");
   for (int i = 0; i < vu->blocks.count; i++) {
      active_block = i;
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
            {
               if (op->result != VCODE_INVALID_REG) {
                  col += vcode_dump_reg(op->result);
                  col += printf(" := ");
               }
               if (op->subkind == VCODE_CC_FOREIGN)
                  col += printf("foreign ");
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

         case VCODE_OP_MAP_SIGNAL:
            {
               printf("%s ", vcode_op_string(op->kind));
               vcode_dump_reg(op->args.items[0]);
               printf(" to ");
               vcode_dump_reg(op->args.items[1]);
               if (op->args.items[2] == op->args.items[3]) {
                  printf(" count ");
                  vcode_dump_reg(op->args.items[2]);
               }
               else {
                  printf(" src count ");
                  vcode_dump_reg(op->args.items[2]);
                  printf(" dst count ");
                  vcode_dump_reg(op->args.items[3]);
               }
               if (op->args.count > 4) {
                  printf(" conv ");
                  vcode_dump_reg(op->args.items[4]);
               }
            }
            break;

         case VCODE_OP_DRIVE_SIGNAL:
            {
               printf("%s ", vcode_op_string(op->kind));
               vcode_dump_reg(op->args.items[0]);
               printf(" count ");
               vcode_dump_reg(op->args.items[1]);
            }
            break;

         case VCODE_OP_INIT_SIGNAL:
            {
               printf("%s ", vcode_op_string(op->kind));
               vcode_dump_reg(op->args.items[0]);
               printf(" value ");
               vcode_dump_reg(op->args.items[1]);
               printf(" count ");
               vcode_dump_reg(op->args.items[2]);
               printf(" size ");
               vcode_dump_reg(op->args.items[3]);
               if (op->args.count > 4) {
                  printf(" resolution ");
                  vcode_dump_reg(op->args.items[4]);
               }
            }
            break;

         case VCODE_OP_IMPLICIT_SIGNAL:
            {
               printf("%s ", vcode_op_string(op->kind));
               vcode_dump_reg(op->args.items[0]);
               printf(" count ");
               vcode_dump_reg(op->args.items[1]);
               printf(" kind ");
               vcode_dump_reg(op->args.items[2]);
               printf(" closure ");
               vcode_dump_reg(op->args.items[3]);
            }
            break;

         case VCODE_OP_RESOLUTION_WRAPPER:
            {
               col += vcode_dump_reg(op->result);
               col += color_printf(" := %s ", vcode_op_string(op->kind));
               col += vcode_dump_reg(op->args.items[0]);
               col += printf(" ileft ");
               col += vcode_dump_reg(op->args.items[1]);
               col += printf(" nlits ");
               col += vcode_dump_reg(op->args.items[2]);
               vcode_dump_result_type(col, op);
            }
            break;

         case VCODE_OP_CLOSURE:
         case VCODE_OP_PROTECTED_INIT:
            {
               col += vcode_dump_reg(op->result);
               col += color_printf(" := %s $magenta$%s$$ context ",
                                   vcode_op_string(op->kind), istr(op->func));
               col += vcode_dump_reg(op->args.items[0]);
               vcode_dump_result_type(col, op);
            }
            break;

         case VCODE_OP_PROTECTED_FREE:
            {
               printf("%s ", vcode_op_string(op->kind));
               vcode_dump_reg(op->args.items[0]);
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
               col += vcode_dump_var(op->address, 0);
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
               vcode_dump_var(op->address, 0);
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
               col += vcode_dump_var(op->address, 0);
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
               col += printf("%s ", vcode_op_string(op->kind));
               col += vcode_dump_reg(op->args.items[0]);
               if (vt->kind == VCODE_TYPE_INT) {
                  col += printf(" in ");
                  col += vcode_pretty_print_int(vt->low);
                  col += printf(" .. ");
                  col += vcode_pretty_print_int(vt->high);
               }
               else {
                  col += printf(" match ");
                  col += vcode_dump_one_type(op->type);
               }
               vcode_dump_comment(col);
               if (op->hint != NULL)
                  printf("%s ", op->hint);
               vcode_dump_loc(&(op->loc));
            }
            break;

         case VCODE_OP_DYNAMIC_BOUNDS:
            {
               col += printf("%s ", vcode_op_string(op->kind));
               col += vcode_dump_reg(op->args.items[0]);
               col += printf(" in ");
               col += vcode_dump_reg(op->args.items[1]);
               col += printf(" .. ");
               col += vcode_dump_reg(op->args.items[2]);
               col += printf(" kind ");
               col += vcode_dump_reg(op->args.items[3]);
               vcode_dump_comment(col);
               if (op->hint != NULL)
                  printf("%s ", op->hint);
               vcode_dump_loc(&(op->loc));
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

         case VCODE_OP_CONST_REP:
            {
               col += vcode_dump_reg(op->result);
               col += printf(" := const [");
               col += vcode_dump_reg(op->args.items[0]);
               col += printf("]*%"PRIi64, op->value);
               vcode_dump_result_type(col, op);
            }
            break;

         case VCODE_OP_ADDRESS_OF:
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

         case VCODE_OP_DISCONNECT:
            {
               printf("%s ", vcode_op_string(op->kind));
               vcode_dump_reg(op->args.items[0]);
               printf(" count ");
               vcode_dump_reg(op->args.items[1]);
               printf(" reject ");
               vcode_dump_reg(op->args.items[2]);
               printf(" after ");
               vcode_dump_reg(op->args.items[3]);
            }
            break;

         case VCODE_OP_NEG:
         case VCODE_OP_ABS:
         case VCODE_OP_RESOLVED:
         case VCODE_OP_LAST_VALUE:
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

         case VCODE_OP_VAR_UPREF:
            {
               col += vcode_dump_reg(op->result);
               col += printf(" := %s %d, ", vcode_op_string(op->kind),
                             op->hops);
               col += vcode_dump_var(op->address, op->hops);
               vcode_dump_result_type(col, op);
            }
            break;

         case VCODE_OP_CONTEXT_UPREF:
            {
               col += vcode_dump_reg(op->result);
               col += printf(" := %s %d", vcode_op_string(op->kind), op->hops);
               vcode_dump_result_type(col, op);
            }
            break;

         case VCODE_OP_ACTIVE:
         case VCODE_OP_EVENT:
         case VCODE_OP_DRIVING:
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
         case VCODE_OP_SCHED_STATIC:
            {
               printf("%s on ", vcode_op_string(op->kind));
               vcode_dump_reg(op->args.items[0]);
               printf(" count ");
               vcode_dump_reg(op->args.items[1]);
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

         case VCODE_OP_MEMSET:
            {
               vcode_dump_reg(op->args.items[0]);
               printf(" := %s ", vcode_op_string(op->kind));
               vcode_dump_reg(op->args.items[1]);
               printf(" length ");
               vcode_dump_reg(op->args.items[2]);
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
               col += printf("%s ", vcode_op_string(op->kind));
               col += vcode_dump_reg(op->args.items[0]);
               vcode_dump_comment(col);
               vcode_dump_loc(&(op->loc));
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

         case VCODE_OP_LAST_EVENT:
         case VCODE_OP_LAST_ACTIVE:
         case VCODE_OP_DRIVING_VALUE:
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

         case VCODE_OP_ARRAY_SIZE:
            {
               col += printf("%s left ", vcode_op_string(op->kind));
               col += vcode_dump_reg(op->args.items[0]);
               col += printf(" == right ");
               col += vcode_dump_reg(op->args.items[1]);
               vcode_dump_comment(col);
               vcode_dump_loc(&(op->loc));
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
               printf("%s %u", vcode_op_string(op->kind), op->tag);
            }
            break;

         case VCODE_OP_COVER_COND:
            {
               printf("%s %u sub %u ", vcode_op_string(op->kind),
                      op->tag, op->subkind);
               vcode_dump_reg(op->args.items[0]);
            }
            break;

         case VCODE_OP_TEMP_STACK_MARK:
            {
               col += vcode_dump_reg(op->result);
               col += printf(" := %s", vcode_op_string(op->kind));
               vcode_dump_result_type(col, op);
            }
            break;

         case VCODE_OP_TEMP_STACK_RESTORE:
            {
               printf("%s ", vcode_op_string(op->kind));
               vcode_dump_reg(op->args.items[0]);
            }
            break;

         case VCODE_OP_UNDEFINED:
            {
               col += vcode_dump_reg(op->result);
               col += printf(" := %s", vcode_op_string(op->kind));
               vcode_dump_result_type(col, op);
            }
            break;

         case VCODE_OP_RANGE_NULL:
            {
               col += vcode_dump_reg(op->result);
               col += printf(" := %s left ", vcode_op_string(op->kind));
               vcode_dump_reg(op->args.items[0]);
               col += printf(" right ");
               vcode_dump_reg(op->args.items[1]);
               col += printf(" dir ");
               vcode_dump_reg(op->args.items[2]);
               vcode_dump_result_type(col, op);
            }
            break;

         case VCODE_OP_LINK_SIGNAL:
            {
               col += vcode_dump_reg(op->result);
               col += color_printf(" := %s $white$%s$$",
                                   vcode_op_string(op->kind), istr(op->ident));
               vcode_dump_result_type(col, op);
            }
            break;

         case VCODE_OP_LINK_VAR:
         case VCODE_OP_LINK_PACKAGE:
            {
               col += vcode_dump_reg(op->result);
               col += color_printf(" := %s $magenta$%s$$",
                                   vcode_op_string(op->kind), istr(op->ident));
               vcode_dump_result_type(col, op);
            }
            break;
         }

         if (j == mark_op && i == old_block)
            color_printf("\t $red$<----$$");

         color_printf("$$\n");

         if (callback != NULL)
            (*callback)(VCODE_DUMP_OP, j, arg);
      }

      if (b->ops.count == 0)
         color_printf("  $yellow$%2d:$$ $red$Empty basic block$$\n", i);
   }

   printf("\n");

   active_block = old_block;
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
      case VCODE_TYPE_OPAQUE:
         return true;
      case VCODE_TYPE_RESOLUTION:
      case VCODE_TYPE_CLOSURE:
      case VCODE_TYPE_SIGNAL:
      case VCODE_TYPE_FILE:
         return vtype_eq(at->base, bt->base);
      case VCODE_TYPE_RECORD:
      case VCODE_TYPE_CONTEXT:
         return at->name == bt->name;
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
   case VCODE_TYPE_RESOLUTION:
   case VCODE_TYPE_CLOSURE:
   case VCODE_TYPE_OPAQUE:
   case VCODE_TYPE_CONTEXT:
      return false;

   case VCODE_TYPE_REAL:
      return bt->rlow >= tt->rlow && bt->rhigh <= tt->rhigh;

   case VCODE_TYPE_SIGNAL:
      return vtype_includes(tt->base, bt->base);
   }

   return false;
}

void vcode_dump(void)
{
   vcode_dump_with_mark(-1, NULL, NULL);
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

vcode_type_t vtype_find_named_record(ident_t name)
{
   assert(active_unit != NULL);

   for (int i = 0; i < active_unit->types.count; i++) {
      vtype_t *other = &(active_unit->types.items[i]);
      if (other->kind == VCODE_TYPE_RECORD && other->name == name)
         return MAKE_HANDLE(active_unit->depth, i);
   }

   return VCODE_INVALID_TYPE;
}

vcode_type_t vtype_named_record(ident_t name, const vcode_type_t *field_types,
                                int nfields)
{
   assert(active_unit != NULL);

   vtype_t *data = NULL;
   vcode_type_t handle = vtype_find_named_record(name);
   if (handle == VCODE_INVALID_TYPE) {
      data = vtype_array_alloc(&(active_unit->types));
      memset(data, '\0', sizeof(vtype_t));
      data->kind = VCODE_TYPE_RECORD;
      data->name = name;

      handle = vtype_new(data);
   }
   else {
      data = vcode_type_data(handle);
      VCODE_ASSERT(data->fields.count == 0,
                    "record type %s already defined", istr(name));
   }

   vcode_type_array_resize(&(data->fields), 0, VCODE_INVALID_TYPE);
   for (int i = 0; i < nfields; i++)
      vcode_type_array_add(&(data->fields), field_types[i]);

   return handle;
}

vcode_type_t vtype_uarray(int ndim, vcode_type_t elem, vcode_type_t bounds)
{
   assert(active_unit != NULL);

   const vtype_kind_t ekind = vtype_kind(elem);
   VCODE_ASSERT(ekind != VCODE_TYPE_CARRAY && ekind != VCODE_TYPE_UARRAY,
                "array types may not be nested");

   VCODE_ASSERT(ndim > 0, "uarray must have at least one dimension");

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

   VCODE_ASSERT(vtype_kind(to) != VCODE_TYPE_CARRAY,
                "cannot get pointer to carray type");

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

   VCODE_ASSERT(vtype_kind(base) != VCODE_TYPE_UARRAY,
                "signal cannot have unconstrained array type");

   return vtype_new(n);
}

vcode_type_t vtype_resolution(vcode_type_t base)
{
   assert(active_unit != NULL);

   vtype_t *n = vtype_array_alloc(&(active_unit->types));
   n->kind = VCODE_TYPE_RESOLUTION;
   n->base = base;

   return vtype_new(n);
}

vcode_type_t vtype_closure(vcode_type_t result)
{
   assert(active_unit != NULL);

   vtype_t *n = vtype_array_alloc(&(active_unit->types));
   n->kind = VCODE_TYPE_CLOSURE;
   n->base = result;

   return vtype_new(n);
}

vcode_type_t vtype_context(ident_t name)
{
   assert(active_unit != NULL);

   vtype_t *n = vtype_array_alloc(&(active_unit->types));
   n->kind = VCODE_TYPE_CONTEXT;
   n->name = name;

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
   return vtype_int(0, 255);
}

vcode_type_t vtype_opaque(void)
{
   assert(active_unit != NULL);

   vtype_t *n = vtype_array_alloc(&(active_unit->types));
   n->kind = VCODE_TYPE_OPAQUE;

   return vtype_new(n);
}

vcode_type_t vtype_real(void)
{
   assert(active_unit != NULL);

   vtype_t *n = vtype_array_alloc(&(active_unit->types));
   n->kind  = VCODE_TYPE_REAL;
   n->rlow  = DBL_MIN;
   n->rhigh = DBL_MAX;

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
   assert(vt->kind == VCODE_TYPE_SIGNAL || vt->kind == VCODE_TYPE_FILE
          || vt->kind == VCODE_TYPE_RESOLUTION
          || vt->kind == VCODE_TYPE_CLOSURE);
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

ident_t vtype_name(vcode_type_t type)
{
   vtype_t *vt = vcode_type_data(type);
   assert(vt->kind == VCODE_TYPE_RECORD || vt->kind == VCODE_TYPE_CONTEXT);
   return vt->name;
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

bool vtype_is_scalar(vcode_type_t type)
{
   const vtype_kind_t kind = vtype_kind(type);
   return kind == VCODE_TYPE_INT || kind == VCODE_TYPE_OFFSET
      || kind == VCODE_TYPE_UARRAY || kind == VCODE_TYPE_POINTER
      || kind == VCODE_TYPE_FILE || kind == VCODE_TYPE_ACCESS
      || kind == VCODE_TYPE_REAL || kind == VCODE_TYPE_SIGNAL
      || kind == VCODE_TYPE_CONTEXT;
}

bool vtype_is_composite(vcode_type_t type)
{
   const vtype_kind_t kind = vtype_kind(type);
   return kind == VCODE_TYPE_RECORD || kind == VCODE_TYPE_CARRAY;
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
   bptr->last_loc = LOC_INVALID;

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

const loc_t *vcode_last_loc(void)
{
   return &(vcode_block_data()->last_loc);
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

bool vcode_unit_has_undefined(void)
{
   assert(active_unit != NULL);
   return active_unit->flags & UNIT_UNDEFINED;
}

int vcode_unit_depth(void)
{
   assert(active_unit != NULL);
   return active_unit->depth;
}

void vcode_set_result(vcode_type_t type)
{
   assert(active_unit != NULL);
   assert(active_unit->kind == VCODE_UNIT_FUNCTION
          || active_unit->kind == VCODE_UNIT_THUNK);

   active_unit->result = type;
}

vcode_type_t vcode_unit_result(void)
{
   assert(active_unit != NULL);
   assert(active_unit->kind == VCODE_UNIT_FUNCTION
          || active_unit->kind == VCODE_UNIT_THUNK);
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

const loc_t *vcode_unit_loc(void)
{
   assert(active_unit != NULL);
   return &(active_unit->loc);
}

static unsigned vcode_unit_calc_depth(vcode_unit_t unit)
{
   int hops = 0;
   for (; (unit = unit->context); hops++)
      ;
   return hops;
}

static void vcode_registry_add(vcode_unit_t vu)
{
   if (registry == NULL)
      registry = hash_new(512, true);

   assert(vu->refcount > 0);
   hash_put(registry, vu->name, vu);
}

void vcode_drop_cache(void)
{
   if (registry == NULL)
      return;

#if 0
   const void *key;
   void *value;
   hash_iter_t it = HASH_BEGIN;
   while (hash_iter(registry, &it, &key, &value)) {
      if (value != NULL)
         vcode_unit_unref(value);
   }
#endif

   hash_free(registry);
   registry = NULL;
}

vcode_unit_t vcode_find_unit(ident_t name)
{
   if (registry == NULL)
      return NULL;
   else {
      vcode_unit_t vu = hash_get(registry, name);
      assert(vu == NULL || vu->refcount > 0);
      return vu;
   }
}

static void vcode_add_child(vcode_unit_t context, vcode_unit_t child)
{
   assert(context->refcount > 0);
   context->refcount++;

   child->next = NULL;
   if (context->children == NULL)
      context->children = child;
   else {
      vcode_unit_t it;
      for (it = context->children; it->next != NULL; it = it->next)
         ;
      it->next = child;
   }
}

vcode_unit_t emit_function(ident_t name, const loc_t *loc, vcode_unit_t context)
{
   vcode_unit_t vu = xcalloc(sizeof(struct vcode_unit));
   vu->kind     = VCODE_UNIT_FUNCTION;
   vu->name     = name;
   vu->context  = context;
   vu->result   = VCODE_INVALID_TYPE;
   vu->depth    = vcode_unit_calc_depth(vu);
   vu->loc      = *loc;
   vu->refcount = 1;

   vcode_add_child(context, vu);

   active_unit = vu;
   vcode_select_block(emit_block());
   emit_debug_info(loc);

   vcode_registry_add(vu);

   return vu;
}

vcode_unit_t emit_procedure(ident_t name, const loc_t *loc,
                            vcode_unit_t context)
{
   vcode_unit_t vu = xcalloc(sizeof(struct vcode_unit));
   vu->kind     = VCODE_UNIT_PROCEDURE;
   vu->name     = name;
   vu->context  = context;
   vu->result   = VCODE_INVALID_TYPE;
   vu->depth    = vcode_unit_calc_depth(vu);
   vu->loc      = *loc;
   vu->refcount = 1;

   vcode_add_child(context, vu);

   active_unit = vu;
   vcode_select_block(emit_block());
   emit_debug_info(loc);

   vcode_registry_add(vu);

   return vu;
}

vcode_unit_t emit_process(ident_t name, const loc_t *loc, vcode_unit_t context)
{
   assert(context->kind == VCODE_UNIT_INSTANCE);

   vcode_unit_t vu = xcalloc(sizeof(struct vcode_unit));
   vu->kind     = VCODE_UNIT_PROCESS;
   vu->name     = name;
   vu->context  = context;
   vu->depth    = vcode_unit_calc_depth(vu);
   vu->result   = VCODE_INVALID_TYPE;
   vu->loc      = *loc;
   vu->refcount = 1;

   vcode_add_child(context, vu);

   active_unit = vu;
   vcode_select_block(emit_block());
   emit_debug_info(loc);

   vcode_registry_add(vu);

   return vu;
}

vcode_unit_t emit_instance(ident_t name, const loc_t *loc, vcode_unit_t context)
{
   assert(context == NULL || context->kind == VCODE_UNIT_INSTANCE);

   vcode_unit_t vu = xcalloc(sizeof(struct vcode_unit));
   vu->kind     = VCODE_UNIT_INSTANCE;
   vu->name     = name;
   vu->context  = context;
   vu->depth    = vcode_unit_calc_depth(vu);
   vu->result   = VCODE_INVALID_TYPE;
   vu->loc      = *loc;
   vu->refcount = 1;

   if (context != NULL)
      vcode_add_child(context, vu);

   active_unit = vu;
   vcode_select_block(emit_block());
   emit_debug_info(loc);

   vcode_registry_add(vu);

   return vu;
}

vcode_unit_t emit_package(ident_t name, const loc_t *loc)
{
   vcode_unit_t vu = xcalloc(sizeof(struct vcode_unit));
   vu->kind     = VCODE_UNIT_PACKAGE;
   vu->name     = name;
   vu->context  = NULL;
   vu->result   = VCODE_INVALID_TYPE;
   vu->loc      = *loc;
   vu->refcount = 1;

   active_unit = vu;
   vcode_select_block(emit_block());
   emit_debug_info(loc);

   vcode_registry_add(vu);

   return vu;
}

vcode_unit_t emit_protected(ident_t name, const loc_t *loc,
                            vcode_unit_t context)
{
   vcode_unit_t vu = xcalloc(sizeof(struct vcode_unit));
   vu->kind     = VCODE_UNIT_PROTECTED;
   vu->name     = name;
   vu->context  = context;
   vu->depth    = vcode_unit_calc_depth(vu);
   vu->result   = VCODE_INVALID_TYPE;
   vu->loc      = *loc;
   vu->refcount = 1;

   if (context != NULL)
      vcode_add_child(context, vu);

   active_unit = vu;
   vcode_select_block(emit_block());
   emit_debug_info(loc);

   vcode_registry_add(vu);

   return vu;
}

vcode_unit_t emit_thunk(ident_t name)
{
   vcode_unit_t vu = xcalloc(sizeof(struct vcode_unit));
   vu->kind     = VCODE_UNIT_THUNK;
   vu->name     = name;
   vu->result   = VCODE_INVALID_TYPE;
   vu->depth    = vcode_unit_calc_depth(vu);
   vu->refcount = 1;

   active_unit = vu;
   vcode_select_block(emit_block());

   if (name != NULL)
      vcode_registry_add(vu);

   return vu;
}

void emit_assert(vcode_reg_t value, vcode_reg_t message, vcode_reg_t length,
                 vcode_reg_t severity)
{
   int64_t value_const;
   if (vcode_reg_const(value, &value_const) && value_const != 0) {
      emit_comment("Always true assertion on r%d", value);
      return;
   }

   op_t *op = vcode_add_op(VCODE_OP_ASSERT);
   vcode_add_arg(op, value);
   vcode_add_arg(op, severity);
   vcode_add_arg(op, message);
   vcode_add_arg(op, length);

   VCODE_ASSERT(vtype_eq(vcode_reg_type(value), vtype_bool()),
                "value parameter to assert is not bool");
   VCODE_ASSERT(message == VCODE_INVALID_REG
                || vcode_reg_kind(message) == VCODE_TYPE_POINTER,
                "message parameter to assert is not a pointer");
   VCODE_ASSERT(vtype_eq(vcode_reg_type(value), vtype_bool()),
                "value parameter to assert is not bool");
   VCODE_ASSERT(!loc_invalid_p(vcode_last_loc()), "assert needs debug info");
}

void emit_report(vcode_reg_t message, vcode_reg_t length, vcode_reg_t severity)
{
   op_t *op = vcode_add_op(VCODE_OP_REPORT);
   vcode_add_arg(op, severity);
   vcode_add_arg(op, message);
   vcode_add_arg(op, length);

   VCODE_ASSERT(vcode_reg_kind(message) == VCODE_TYPE_POINTER,
                "message parameter to report is not a pointer");
   VCODE_ASSERT(vtype_eq(vtype_pointed(vcode_reg_type(message)), vtype_char()),
                "message parameter to report is not a character pointer");
   VCODE_ASSERT(!loc_invalid_p(vcode_last_loc()), "report needs debug info");
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
                                 vcode_type_t bounds, vcode_cc_t cc,
                                 const vcode_reg_t *args, int nargs,
                                 vcode_block_t resume_bb, int hops)
{
   op_t *o = vcode_add_op(op);
   o->func    = func;
   o->type    = type;
   o->hops    = hops;
   o->subkind = cc;
   for (int i = 0; i < nargs; i++)
      vcode_add_arg(o, args[i]);

   if (resume_bb != VCODE_INVALID_BLOCK)
      vcode_block_array_add(&(o->targets), resume_bb);

   for (int i = 0; i < nargs; i++)
      VCODE_ASSERT(args[i] != VCODE_INVALID_REG,
                   "invalid argument to function");

   if (cc != VCODE_CC_FOREIGN)
      VCODE_ASSERT(nargs > 0 && vcode_reg_kind(args[0]) == VCODE_TYPE_CONTEXT,
                   "first argument to VHDL function must be context pointer");

   if (type == VCODE_INVALID_TYPE)
      return (o->result = VCODE_INVALID_REG);
   else {
      o->result = vcode_add_reg(type);

      reg_t *rr = vcode_reg_data(o->result);
      rr->bounds = bounds;

      return o->result;
   }
}

vcode_reg_t emit_fcall(ident_t func, vcode_type_t type, vcode_type_t bounds,
                       vcode_cc_t cc, const vcode_reg_t *args, int nargs)
{
   return emit_fcall_op(VCODE_OP_FCALL, func, type, bounds, cc, args, nargs,
                        VCODE_INVALID_BLOCK, 0);
}

void emit_pcall(ident_t func, const vcode_reg_t *args, int nargs,
                vcode_block_t resume_bb)
{
   emit_fcall_op(VCODE_OP_PCALL, func, VCODE_INVALID_TYPE, VCODE_INVALID_TYPE,
                 VCODE_CC_VHDL, args, nargs, resume_bb, 0);
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

vcode_reg_t emit_const_array(vcode_type_t type, vcode_reg_t *values, int num)
{
   vtype_kind_t kind = vtype_kind(type);

   // Reuse any previous operation in this block with the same arguments
   VCODE_FOR_EACH_MATCHING_OP(other, VCODE_OP_CONST_ARRAY) {
      if (other->args.count != num)
         continue;
      else if (!vtype_eq(vcode_reg_type(other->result), type))
         continue;

      bool match = true;
      for (int i = 0; match && i < num; i++) {
         if (other->args.items[i] != values[i])
            match = false;
      }

      if (match) return other->result;
   }

   op_t *op = vcode_add_op(VCODE_OP_CONST_ARRAY);
   op->result = vcode_add_reg(type);

   for (int i = 0; i < num; i++)
      vcode_add_arg(op, values[i]);

   VCODE_ASSERT(kind == VCODE_TYPE_CARRAY,
                "constant array must have constrained array type");

   reg_t *r = vcode_reg_data(op->result);
   r->bounds = vtype_elem(type);

   return op->result;
}

vcode_reg_t emit_const_rep(vcode_type_t type, vcode_reg_t value, int rep)
{
   // Reuse any previous operation in this block with the same arguments
   VCODE_FOR_EACH_MATCHING_OP(other, VCODE_OP_CONST_REP) {
      if (other->args.items[0] == value && other->value == rep)
         return other->result;
   }

   op_t *op = vcode_add_op(VCODE_OP_CONST_REP);
   op->value = rep;
   vcode_add_arg(op, value);

   VCODE_ASSERT(vcode_reg_const(value, NULL)
                || vcode_reg_kind(value) == VCODE_TYPE_REAL,
                "constant array must have constant values");
   VCODE_ASSERT(vtype_kind(type) == VCODE_TYPE_CARRAY,
                "constant array must have constrained array type");

   op->result = vcode_add_reg(vtype_pointer(vtype_elem(type)));

   reg_t *r = vcode_reg_data(op->result);
   r->bounds = vtype_bounds(type);

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
                   || defn->kind == VCODE_OP_NULL
                   || defn->kind == VCODE_OP_UNDEFINED,
                   "constant record field r%d is not constant", values[i]);
   }

   return op->result;
}

vcode_reg_t emit_address_of(vcode_reg_t value)
{
   VCODE_FOR_EACH_MATCHING_OP(other, VCODE_OP_ADDRESS_OF) {
      if (other->args.items[0] == value)
         return other->result;
   }

   op_t *op = vcode_add_op(VCODE_OP_ADDRESS_OF);
   vcode_add_arg(op, value);

   vcode_type_t type = vcode_reg_type(value);
   VCODE_ASSERT(vtype_is_composite(type),
                "address of argument must be record or array");

   if (vtype_kind(type) == VCODE_TYPE_CARRAY) {
      vcode_type_t elem = vtype_elem(type);
      op->result = vcode_add_reg(vtype_pointer(elem));

      reg_t *rr = vcode_reg_data(op->result);
      rr->bounds = elem;

      return op->result;
   }
   else
      return (op->result = vcode_add_reg(vtype_pointer(type)));
}

void emit_wait(vcode_block_t target, vcode_reg_t time)
{
   op_t *op = vcode_add_op(VCODE_OP_WAIT);
   vcode_add_target(op, target);
   vcode_add_arg(op, time);

   VCODE_ASSERT(time == VCODE_INVALID_REG
                || vcode_reg_kind(time) == VCODE_TYPE_INT,
                "wait time must have integer type");
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
                     vcode_var_flags_t flags)
{
   assert(active_unit != NULL);

   vcode_var_t var = active_unit->vars.count;
   var_t *v = var_array_alloc(&(active_unit->vars));
   memset(v, '\0', sizeof(var_t));
   v->type     = type;
   v->bounds   = bounds;
   v->name     = name;
   v->flags    = flags;

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
      else if (other->kind == VCODE_OP_FCALL || other->kind == VCODE_OP_PCALL)
         break;   // Nested call captures variables
   }

   var_t *v = vcode_var_data(var);

   if (fold != VCODE_INVALID_REG && !aliased)
      return fold;

   op_t *op = vcode_add_op(VCODE_OP_LOAD);
   op->address = var;
   op->result  = vcode_add_reg(v->type);

   VCODE_ASSERT(vtype_is_scalar(v->type), "cannot load non-scalar type");

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

   VCODE_ASSERT(vtype_is_scalar(deref), "cannot load non-scalar type");

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
         vcode_reg_array_resize(&(other->args), 0, VCODE_INVALID_REG);
      }
      else if (other->kind == VCODE_OP_FCALL || other->kind == VCODE_OP_PCALL)
         break;   // Needs to get variable for display
      else if ((other->kind == VCODE_OP_INDEX || other->kind == VCODE_OP_LOAD)
               && other->address == var)
         break;   // Previous value may be used
   }

   var_t *v = vcode_var_data(var);
   reg_t *r = vcode_reg_data(reg);

   op_t *op = vcode_add_op(VCODE_OP_STORE);
   vcode_add_arg(op, reg);
   op->address = var;

   VCODE_ASSERT(vtype_eq(v->type, r->type),
                "variable and stored value do not have same type");
   VCODE_ASSERT(vtype_is_scalar(v->type), "cannot store non-scalar type");
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
   VCODE_ASSERT(vtype_is_scalar(r->type), "cannot store non-scalar type");
}

static vcode_reg_t emit_arith(vcode_op_t kind, vcode_reg_t lhs, vcode_reg_t rhs)
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
   const bool l_is_const = vcode_reg_const(lhs, &lconst);
   const bool r_is_const = vcode_reg_const(rhs, &rconst);
   if (l_is_const && r_is_const)
      return emit_const(vcode_reg_type(lhs), lconst * rconst);
   else if (r_is_const && rconst == 1)
      return lhs;
   else if (l_is_const && lconst == 1)
      return rhs;
   else if ((r_is_const && rconst == 0) || (l_is_const && lconst == 0))
      return emit_const(vcode_reg_type(lhs), 0);

   vcode_reg_t reg = emit_arith(VCODE_OP_MUL, lhs, rhs);

   vtype_t *bl = vcode_type_data(vcode_reg_data(lhs)->bounds);
   vtype_t *br = vcode_type_data(vcode_reg_data(rhs)->bounds);

   if (bl->kind == VCODE_TYPE_REAL)
      return reg;

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
   const bool l_is_const = vcode_reg_const(lhs, &lconst);
   const bool r_is_const = vcode_reg_const(rhs, &rconst);
   if (l_is_const && r_is_const && rconst != 0)
      return emit_const(vcode_reg_type(lhs), lconst / rconst);
   else if (r_is_const && rconst == 1)
      return lhs;

   vcode_reg_t reg = emit_arith(VCODE_OP_DIV, lhs, rhs);

   vtype_t *bl = vcode_type_data(vcode_reg_data(lhs)->bounds);

   if (bl->kind == VCODE_TYPE_INT && r_is_const) {
      reg_t *rr = vcode_reg_data(reg);
      rr->bounds = vtype_int(bl->low / rconst, bl->high / rconst);
   }

   return reg;
}

vcode_reg_t emit_exp(vcode_reg_t lhs, vcode_reg_t rhs)
{
   int64_t lconst, rconst;
   if (vcode_reg_const(lhs, &lconst) && vcode_reg_const(rhs, &rconst)
       && rconst >= 0)
      return emit_const(vcode_reg_type(lhs), ipow(lconst, rconst));

   return emit_arith(VCODE_OP_EXP, lhs, rhs);
}

vcode_reg_t emit_mod(vcode_reg_t lhs, vcode_reg_t rhs)
{
   int64_t lconst, rconst;
   if (vcode_reg_const(lhs, &lconst) && vcode_reg_const(rhs, &rconst)
       && lconst > 0 && rconst > 0)
      return emit_const(vcode_reg_type(lhs), lconst % rconst);

   vtype_t *bl = vcode_type_data(vcode_reg_data(lhs)->bounds);
   vtype_t *br = vcode_type_data(vcode_reg_data(rhs)->bounds);

   if (bl->low >= 0 && br->low >= 0) {
      // If both arguments are non-negative then rem is equivalent and
      // cheaper to compute
      vcode_reg_t reg = emit_arith(VCODE_OP_REM, lhs, rhs);

      reg_t *rr = vcode_reg_data(reg);
      rr->bounds = vtype_int(0, br->high - 1);

      return reg;
   }
   else
      return emit_arith(VCODE_OP_MOD, lhs, rhs);
}

vcode_reg_t emit_rem(vcode_reg_t lhs, vcode_reg_t rhs)
{
   int64_t lconst, rconst;
   if (vcode_reg_const(lhs, &lconst) && vcode_reg_const(rhs, &rconst)
       && lconst > 0 && rconst > 0)
      return emit_const(vcode_reg_type(lhs), lconst % rconst);

   vcode_reg_t reg = emit_arith(VCODE_OP_REM, lhs, rhs);

   vtype_t *bl = vcode_type_data(vcode_reg_data(lhs)->bounds);
   vtype_t *br = vcode_type_data(vcode_reg_data(rhs)->bounds);

   if (bl->low >= 0 && br->low >= 0) {
      reg_t *rr = vcode_reg_data(reg);
      rr->bounds = vtype_int(0, br->high - 1);
   }

   return reg;
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

   vcode_reg_t reg = emit_arith(VCODE_OP_ADD, lhs, rhs);

   reg_t *rr = vcode_reg_data(reg);
   if (is_pointer)
      rr->bounds = vcode_reg_data(lhs)->bounds;
   else if (ltypek != VCODE_TYPE_REAL) {
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

   vcode_reg_t reg = emit_arith(VCODE_OP_SUB, lhs, rhs);

   vtype_t *bl = vcode_type_data(vcode_reg_data(lhs)->bounds);
   vtype_t *br = vcode_type_data(vcode_reg_data(rhs)->bounds);

   reg_t *rr = vcode_reg_data(reg);
   if (vcode_reg_kind(lhs) == VCODE_TYPE_POINTER
       && vcode_reg_kind(rhs) == VCODE_TYPE_POINTER)
      rr->type = rr->bounds = vtype_offset();
   else if (bl->kind == VCODE_TYPE_POINTER || bl->kind == VCODE_TYPE_SIGNAL)
      rr->bounds = vcode_reg_data(lhs)->bounds;
   else if (bl->kind != VCODE_TYPE_REAL) {
      // XXX: this is wrong - see TO_UNSIGNED
      rr->bounds = vtype_int(sadd64(bl->low, -br->high),
                             sadd64(bl->high, -br->low));
   }

   return reg;
}

void emit_bounds(vcode_reg_t reg, vcode_type_t bounds, bounds_kind_t kind,
                 const char *hint)
{
   if (reg == VCODE_INVALID_REG)
      return;
   else if (vtype_includes(bounds, vcode_reg_data(reg)->bounds)) {
      emit_comment("Elided bounds check for r%d", reg);
      return;
   }

   op_t *op = vcode_add_op(VCODE_OP_BOUNDS);
   vcode_add_arg(op, reg);
   op->type     = bounds;
   op->subkind  = kind;
   op->hint     = hint ? xstrdup(hint) : NULL;

   const vtype_kind_t tkind = vtype_kind(bounds);
   VCODE_ASSERT(tkind == VCODE_TYPE_INT || tkind == VCODE_TYPE_REAL,
                "type argument to bounds must be integer or real");

   const vtype_kind_t rkind = vcode_reg_kind(reg);
   VCODE_ASSERT(rkind == VCODE_TYPE_INT || tkind == VCODE_TYPE_REAL
                || rkind == VCODE_TYPE_OFFSET,
                "reg argument to bounds must be integer or real");

   VCODE_ASSERT(!loc_invalid_p(vcode_last_loc()),
                "bounds check needs debug info");
}

static void vcode_calculate_var_index_type(op_t *op, var_t *var)
{
   switch (vtype_kind(var->type)) {
   case VCODE_TYPE_CARRAY:
      op->type = vtype_pointer(vtype_elem(var->type));
      op->result = vcode_add_reg(op->type);
      vcode_reg_data(op->result)->bounds = vtype_bounds(var->type);
      break;

   case VCODE_TYPE_RECORD:
      op->type = vtype_pointer(var->type);
      op->result = vcode_add_reg(op->type);
      break;

   case VCODE_TYPE_INT:
   case VCODE_TYPE_FILE:
   case VCODE_TYPE_ACCESS:
   case VCODE_TYPE_REAL:
   case VCODE_TYPE_UARRAY:
   case VCODE_TYPE_POINTER:
   case VCODE_TYPE_SIGNAL:
   case VCODE_TYPE_CONTEXT:
      op->type = vtype_pointer(var->type);
      op->result = vcode_add_reg(op->type);
      vcode_reg_data(op->result)->bounds = var->bounds;
      break;

   default:
      VCODE_ASSERT(false, "variable %s cannot be indexed",
                   istr(var->name));
   }
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

   vcode_calculate_var_index_type(op, vcode_var_data(var));

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
      { VCODE_TYPE_ACCESS, VCODE_TYPE_ACCESS  },
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

      VCODE_ASSERT(active_unit->kind == VCODE_UNIT_FUNCTION
                   || active_unit->kind == VCODE_UNIT_THUNK,
                   "returning value fron non-function unit");
      VCODE_ASSERT(vtype_eq(active_unit->result, vcode_reg_type(reg)),
                   "return value incorrect type");

      vtype_kind_t rkind = vcode_reg_kind(reg);
      if (rkind == VCODE_TYPE_POINTER || rkind == VCODE_TYPE_UARRAY)
         vcode_heap_allocate(reg);
   }
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

void emit_disconnect(vcode_reg_t nets, vcode_reg_t nnets, vcode_reg_t reject,
                     vcode_reg_t after)
{
   op_t *op = vcode_add_op(VCODE_OP_DISCONNECT);
   vcode_add_arg(op, nets);
   vcode_add_arg(op, nnets);
   vcode_add_arg(op, reject);
   vcode_add_arg(op, after);

   VCODE_ASSERT(vcode_reg_kind(nets) == VCODE_TYPE_SIGNAL,
                "disconnect target is not signal");
   VCODE_ASSERT(vcode_reg_kind(nnets) == VCODE_TYPE_OFFSET,
                "disconnect net count is not offset type");
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
      return emit_const(vcode_reg_type(lhs), llabs(lconst));

   op_t *op = vcode_add_op(VCODE_OP_ABS);
   vcode_add_arg(op, lhs);
   op->result = vcode_add_reg(vcode_reg_type(lhs));

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
   else if (rtrue == rfalse)
      return rtrue;

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
      case VCODE_OP_AND:  return emit_const(vtbool, lconst && rconst);
      case VCODE_OP_OR:   return emit_const(vtbool, lconst || rconst);
      case VCODE_OP_XOR:  return emit_const(vtbool, lconst ^ rconst);
      case VCODE_OP_XNOR: return emit_const(vtbool, !(lconst ^ rconst));
      case VCODE_OP_NAND: return emit_const(vtbool, !(lconst && rconst));
      case VCODE_OP_NOR:  return emit_const(vtbool, !(lconst || rconst));
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
      case VCODE_OP_NAND:
      case VCODE_OP_NOR:
         return emit_not(lhs);
      case VCODE_OP_XOR:
         return emit_const(vtbool, 0);
      case VCODE_OP_XNOR:
         return emit_const(vtbool, 1);
      default:
         fatal_trace("cannot constant fold logical %s", vcode_op_string(op));
      }
   }

   vcode_reg_t result = emit_arith(op, lhs, rhs);

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
   VCODE_FOR_EACH_OP(other) {
      if (other->kind == VCODE_OP_UARRAY_LEN) {
         if (other->args.items[0] == array && other->dim == dim)
            return other->result;
      }
      else if (other->kind == VCODE_OP_WRAP && other->result == array) {
         VCODE_ASSERT(dim < (other->args.count - 1) / 3,
                      "array dimension %d out of bounds", dim);

         int64_t left, right, dir;
         if (vcode_reg_const(other->args.items[dim * 3 + 1], &left)
             && vcode_reg_const(other->args.items[dim * 3 + 2], &right)
             && vcode_reg_const(other->args.items[dim * 3 + 3], &dir)) {
            if (dir == RANGE_TO)
               return emit_const(vtype_offset(), right - left + 1);
            else
               return emit_const(vtype_offset(), left - right + 1);
         }
      }
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

   vcode_type_t elem = vt->elem;

   vcode_type_t rtype = (vtype_kind(elem) == VCODE_TYPE_SIGNAL)
      ? elem : vtype_pointer(elem);

   op->result = vcode_add_reg(rtype);

   reg_t *rr = vcode_reg_data(op->result);
   rr->bounds = elem;

   return op->result;
}

vcode_reg_t emit_range_null(vcode_reg_t left, vcode_reg_t right,
                            vcode_reg_t dir)
{
   VCODE_FOR_EACH_MATCHING_OP(other, VCODE_OP_RANGE_NULL) {
      if (other->args.items[0] == left
          && other->args.items[1] == right
          && other->args.items[2] == dir)
         return other->result;
   }

   int64_t dir_const;
   if (vcode_reg_const(dir, &dir_const)) {
      if (dir_const == RANGE_TO)
         return emit_cmp(VCODE_CMP_GT, left, right);
      else
         return emit_cmp(VCODE_CMP_GT, right, left);
   }

   op_t *op = vcode_add_op(VCODE_OP_RANGE_NULL);
   vcode_add_arg(op, left);
   vcode_add_arg(op, right);
   vcode_add_arg(op, dir);

   return (op->result = vcode_add_reg(vtype_bool()));
}

vcode_reg_t emit_var_upref(int hops, vcode_var_t var)
{
   VCODE_FOR_EACH_MATCHING_OP(other, VCODE_OP_VAR_UPREF) {
      if (other->hops == hops && other->address == var)
         return other->result;
   }

   op_t *op = vcode_add_op(VCODE_OP_VAR_UPREF);
   op->hops    = hops;
   op->address = var;

   VCODE_ASSERT(hops > 0, "invalid hop count");

   vcode_unit_t vu = active_unit;
   for (int i = 0; i < hops; i++) {
      vu = vu->context;
      VCODE_ASSERT(vu, "hop count is greater than depth");
   }

   VCODE_ASSERT(var < vu->vars.count, "upref %d is not a variable", var);

   vcode_calculate_var_index_type(op, &(vu->vars.items[var]));

   return op->result;
}

void emit_init_signal(vcode_reg_t signal, vcode_reg_t value, vcode_reg_t count,
                      vcode_reg_t size, vcode_reg_t resolution)
{
   op_t *op = vcode_add_op(VCODE_OP_INIT_SIGNAL);
   vcode_add_arg(op, signal);
   vcode_add_arg(op, value);
   vcode_add_arg(op, count);
   vcode_add_arg(op, size);
   if (resolution != VCODE_INVALID_REG)
      vcode_add_arg(op, resolution);

   vcode_type_t stype = vcode_reg_type(signal);
   vcode_type_t vtype = vcode_reg_type(value);
   VCODE_ASSERT(vtype_kind(stype) == VCODE_TYPE_SIGNAL,
                "argument to init signal is not a signal");
   VCODE_ASSERT(vtype_eq(vtype, vtype_base(stype))
                || vtype_kind(vtype) == VCODE_TYPE_POINTER,
                "init signal value type does not match signal type");
   VCODE_ASSERT(resolution == VCODE_INVALID_REG
                || vcode_reg_kind(resolution) == VCODE_TYPE_RESOLUTION,
                "resolution wrapper argument has wrong type");
}

void emit_implicit_signal(vcode_reg_t signal, vcode_reg_t count,
                          vcode_reg_t kind, vcode_reg_t closure)
{
   op_t *op = vcode_add_op(VCODE_OP_IMPLICIT_SIGNAL);
   vcode_add_arg(op, signal);
   vcode_add_arg(op, count);
   vcode_add_arg(op, kind);
   vcode_add_arg(op, closure);

   VCODE_ASSERT(vcode_reg_kind(signal) == VCODE_TYPE_SIGNAL,
                "argument to implicit signal is not a signal");
   VCODE_ASSERT(vcode_reg_kind(count) == VCODE_TYPE_OFFSET,
                "count argument to implicit signal is not offset");
   VCODE_ASSERT(vcode_reg_kind(kind) == VCODE_TYPE_OFFSET,
                "kind argument to implicit signal is not offset");
   VCODE_ASSERT(vcode_reg_kind(closure) == VCODE_TYPE_CLOSURE,
                "closure argument to implicit signal is not a closure");
}

void emit_map_signal(vcode_reg_t src, vcode_reg_t dst, vcode_reg_t src_count,
                     vcode_reg_t dst_count, vcode_reg_t conv)
{
   op_t *op = vcode_add_op(VCODE_OP_MAP_SIGNAL);
   vcode_add_arg(op, src);
   vcode_add_arg(op, dst);
   vcode_add_arg(op, src_count);
   vcode_add_arg(op, dst_count);
   if (conv != VCODE_INVALID_REG)
      vcode_add_arg(op, conv);

   VCODE_ASSERT(vcode_reg_kind(src) == VCODE_TYPE_SIGNAL,
                "src argument to map signal is not a signal");
   VCODE_ASSERT(vcode_reg_kind(dst) == VCODE_TYPE_SIGNAL,
                "dst argument to map signal is not a signal");
   VCODE_ASSERT(vcode_reg_kind(src_count) == VCODE_TYPE_OFFSET,
                "src count argument type to map signal is not offset");
   VCODE_ASSERT(vcode_reg_kind(dst_count) == VCODE_TYPE_OFFSET,
                "dst count argument type to map signal is not offset");
   VCODE_ASSERT(conv != VCODE_INVALID_REG || dst_count == src_count ,
                "dst count must equal src count without conversion");
   VCODE_ASSERT(conv == VCODE_INVALID_REG
                || vcode_reg_kind(conv) == VCODE_TYPE_CLOSURE,
                "conv argument type to map signal is not closure");
}

void emit_drive_signal(vcode_reg_t target, vcode_reg_t count)
{
   op_t *op = vcode_add_op(VCODE_OP_DRIVE_SIGNAL);
   vcode_add_arg(op, target);
   vcode_add_arg(op, count);

   VCODE_ASSERT(vcode_reg_kind(target) == VCODE_TYPE_SIGNAL,
                "target argument to drive signal is not a signal");
   VCODE_ASSERT(vcode_reg_kind(count) == VCODE_TYPE_OFFSET,
                "count argument type to drive signal is not offset");
}

vcode_reg_t emit_resolution_wrapper(vcode_type_t type, vcode_reg_t closure,
                                    vcode_reg_t ileft, vcode_reg_t nlits)
{
   VCODE_FOR_EACH_MATCHING_OP(other, VCODE_OP_RESOLUTION_WRAPPER) {
      if (other->args.items[0] == closure
          && other->args.items[1] == ileft
          && other->args.items[2] == nlits)
         return other->result;
   }

   VCODE_ASSERT(vcode_reg_kind(closure) == VCODE_TYPE_CLOSURE,
                "first argument to resolution wrapper must be closure");

   op_t *op = vcode_add_op(VCODE_OP_RESOLUTION_WRAPPER);
   vcode_add_arg(op, closure);
   vcode_add_arg(op, ileft);
   vcode_add_arg(op, nlits);
   op->subkind = VCODE_CC_VHDL;

   return (op->result = vcode_add_reg(vtype_resolution(type)));
}

vcode_reg_t emit_closure(ident_t func, vcode_reg_t context, vcode_type_t atype,
                         vcode_type_t rtype)
{
   VCODE_FOR_EACH_MATCHING_OP(other, VCODE_OP_CLOSURE) {
      if (other->func == func && other->subkind == VCODE_CC_VHDL
          && other->args.items[0] == context)
         return other->result;
   }

   op_t *op = vcode_add_op(VCODE_OP_CLOSURE);
   vcode_add_arg(op, context);
   op->func    = func;
   op->subkind = VCODE_CC_VHDL;
   op->type    = atype;

   VCODE_ASSERT(vcode_reg_kind(context) == VCODE_TYPE_CONTEXT,
                "invalid closure context argument");

   return (op->result = vcode_add_reg(vtype_closure(rtype)));
}

vcode_reg_t emit_protected_init(vcode_type_t type, vcode_reg_t context)
{
   op_t *op = vcode_add_op(VCODE_OP_PROTECTED_INIT);
   vcode_add_arg(op, context);
   op->func    = vtype_name(type);
   op->subkind = VCODE_CC_VHDL;

   VCODE_ASSERT(vtype_kind(type) == VCODE_TYPE_CONTEXT,
                "protected init type must be context");
   VCODE_ASSERT(vcode_reg_kind(context) == VCODE_TYPE_CONTEXT,
                "invalid protected init context argument");

   return (op->result = vcode_add_reg(type));
}

void emit_protected_free(vcode_reg_t obj)
{
   op_t *op = vcode_add_op(VCODE_OP_PROTECTED_FREE);
   vcode_add_arg(op, obj);

   VCODE_ASSERT(vcode_reg_kind(obj) == VCODE_TYPE_CONTEXT,
                "protected object type must be context");
}

vcode_reg_t emit_context_upref(int hops)
{
   VCODE_FOR_EACH_MATCHING_OP(other, VCODE_OP_CONTEXT_UPREF) {
      if (other->hops == hops)
         return other->result;
   }

   op_t *op = vcode_add_op(VCODE_OP_CONTEXT_UPREF);
   op->hops = hops;

   VCODE_ASSERT(hops >= 0, "invalid hop count");

   vcode_unit_t vu = active_unit;
   for (int i = 0; i < hops; i++) {
      vu = vu->context;
      VCODE_ASSERT(vu, "hop count is greater than depth");
   }

   return (op->result = vcode_add_reg(vtype_context(vu->name)));
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
      || rptype->kind == VCODE_TYPE_ACCESS
      || rptype->kind == VCODE_TYPE_SIGNAL,
      "argument to record ref must be a pointer, signal, or access");

   vtype_t *rtype = vcode_type_data(rptype->pointed);

   VCODE_ASSERT(rtype->kind == VCODE_TYPE_RECORD,
                "argument must be pointer to record");

   VCODE_ASSERT(field < rtype->fields.count, "invalid field %d", field);

   vcode_type_t field_type  = rtype->fields.items[field];
   vcode_type_t bounds_type = field_type;
   vcode_type_t result_type = field_type;

   const vtype_kind_t fkind = vtype_kind(field_type);
   if (fkind == VCODE_TYPE_CARRAY)
      result_type = bounds_type = vtype_elem(field_type);
   else if (fkind == VCODE_TYPE_UARRAY) {
      bounds_type = vtype_elem(field_type);
      result_type = field_type;
   }

   if (rptype->kind == VCODE_TYPE_SIGNAL)
      op->result = vcode_add_reg(vtype_signal(result_type));
   else
      op->result = vcode_add_reg(vtype_pointer(result_type));

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

void emit_sched_event(vcode_reg_t nets, vcode_reg_t n_elems)
{
   op_t *op = vcode_add_op(VCODE_OP_SCHED_EVENT);
   vcode_add_arg(op, nets);
   vcode_add_arg(op, n_elems);

   VCODE_ASSERT(vcode_reg_kind(nets) == VCODE_TYPE_SIGNAL,
                "nets argument to sched event must be signal");
}

void emit_sched_static(vcode_reg_t nets, vcode_reg_t n_elems)
{
   op_t *op = vcode_add_op(VCODE_OP_SCHED_STATIC);
   vcode_add_arg(op, nets);
   vcode_add_arg(op, n_elems);

   VCODE_ASSERT(vcode_reg_kind(nets) == VCODE_TYPE_SIGNAL,
                "nets argument to sched event must be signal");
}

void emit_resume(ident_t func)
{
   op_t *op = vcode_add_op(VCODE_OP_RESUME);
   op->func = func;

   block_t *b = &(active_unit->blocks.items[active_block]);
   VCODE_ASSERT(b->ops.count == 1, "resume must be first op in a block");
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
                || kind == VCODE_TYPE_ACCESS || kind == VCODE_TYPE_CONTEXT,
                "null type must be file, access, context, or pointer");

   return (op->result = vcode_add_reg(type));
}

vcode_reg_t emit_new(vcode_type_t type, vcode_reg_t length)
{
   op_t *op = vcode_add_op(VCODE_OP_NEW);
   if (length != VCODE_INVALID_REG)
      vcode_add_arg(op, length);

   op->result = vcode_add_reg(vtype_access(type));

   vtype_kind_t kind = vtype_kind(type);
   VCODE_ASSERT(kind == VCODE_TYPE_INT || kind == VCODE_TYPE_RECORD
                || kind == VCODE_TYPE_UARRAY || kind == VCODE_TYPE_ACCESS
                || kind == VCODE_TYPE_REAL,
                "new type must be int, real, record, access, or uarray");
   VCODE_ASSERT(length == VCODE_INVALID_REG
                || vtype_kind(vcode_reg_type(length)) == VCODE_TYPE_OFFSET,
                "new length must have offset type");

   return op->result;
}

void emit_null_check(vcode_reg_t ptr)
{
   VCODE_FOR_EACH_OP(other) {
      if (other->kind == VCODE_OP_NULL_CHECK && other->args.items[0] == ptr)
         return;
      else if (other->kind == VCODE_OP_NEW && other->result == ptr)
         return;
   }

   op_t *op = vcode_add_op(VCODE_OP_NULL_CHECK);
   vcode_add_arg(op, ptr);

   VCODE_ASSERT(vtype_kind(vcode_reg_type(ptr)) == VCODE_TYPE_ACCESS,
                "null check argument must be an access");
   VCODE_ASSERT(!loc_invalid_p(vcode_last_loc()),
                "null check needs debug info");
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

   VCODE_ASSERT(vtype_kind(pointed) != VCODE_TYPE_OPAQUE,
                "cannot dereference opaque type");

   return op->result;
}

static vcode_reg_t emit_signal_data_op(vcode_op_t kind, vcode_reg_t sig)
{
   block_t *b = &(active_unit->blocks.items[active_block]);
   for (int i = b->ops.count - 1; i >= 0; i--) {
      const op_t *other = &(b->ops.items[i]);
      if (other->kind == kind && other->args.items[0] == sig)
         return other->result;
   }

   op_t *op = vcode_add_op(kind);
   vcode_add_arg(op, sig);

   vcode_type_t stype = vcode_reg_type(sig);
   op->type = stype;

   VCODE_ASSERT(vtype_kind(stype) == VCODE_TYPE_SIGNAL,
                "argument r%d to resolved is not a signal", sig);

   vcode_type_t rtype = vtype_base(stype);

   const vtype_kind_t rkind = vtype_kind(rtype);
   if (rkind == VCODE_TYPE_CARRAY || rkind == VCODE_TYPE_UARRAY)
      rtype = vtype_elem(rtype);

   op->result = vcode_add_reg(vtype_pointer(rtype));

   reg_t *rr = vcode_reg_data(op->result);
   rr->bounds = rtype;

   return op->result;
}

vcode_reg_t emit_resolved(vcode_reg_t sig)
{
   return emit_signal_data_op(VCODE_OP_RESOLVED, sig);
}

vcode_reg_t emit_last_value(vcode_reg_t sig)
{
   return emit_signal_data_op(VCODE_OP_LAST_VALUE, sig);
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

vcode_reg_t emit_last_active(vcode_reg_t signal, vcode_reg_t len)
{
   op_t *op = vcode_add_op(VCODE_OP_LAST_ACTIVE);
   vcode_add_arg(op, signal);
   if (len != VCODE_INVALID_REG)
      vcode_add_arg(op, len);

   VCODE_ASSERT(vcode_reg_kind(signal) == VCODE_TYPE_SIGNAL,
                "signal argument to last active must have signal type");
   VCODE_ASSERT(len == VCODE_INVALID_REG
                || vcode_reg_kind(len) == VCODE_TYPE_OFFSET,
                "length argument to last active must have offset type");

   return (op->result = vcode_add_reg(vtype_time()));
}

vcode_reg_t emit_driving_flag(vcode_reg_t signal, vcode_reg_t len)
{
   op_t *op = vcode_add_op(VCODE_OP_DRIVING);
   vcode_add_arg(op, signal);
   vcode_add_arg(op, len);

   VCODE_ASSERT(vcode_reg_kind(signal) == VCODE_TYPE_SIGNAL,
                "signal argument to last active must have signal type");
   VCODE_ASSERT(vcode_reg_kind(len) == VCODE_TYPE_OFFSET,
                "length argument to last active must have offset type");

   return (op->result = vcode_add_reg(vtype_bool()));
}

vcode_reg_t emit_driving_value(vcode_reg_t signal, vcode_reg_t len)
{
   op_t *op = vcode_add_op(VCODE_OP_DRIVING_VALUE);
   vcode_add_arg(op, signal);
   if (len != VCODE_INVALID_REG)
      vcode_add_arg(op, len);

   vcode_type_t signal_type = vcode_reg_type(signal);

   VCODE_ASSERT(vtype_kind(signal_type) == VCODE_TYPE_SIGNAL,
                "signal argument to last active must have signal type");
   VCODE_ASSERT(len == VCODE_INVALID_REG
                || vcode_reg_kind(len) == VCODE_TYPE_OFFSET,
                "length argument to last active must have offset type");

   vcode_type_t base_type = vtype_base(signal_type);
   op->result = vcode_add_reg(vtype_pointer(base_type));

   reg_t *rr = vcode_reg_data(op->result);
   rr->bounds = base_type;

   return op->result;
}

void emit_dynamic_bounds(vcode_reg_t reg, vcode_reg_t low, vcode_reg_t high,
                         vcode_reg_t kind, const char *hint)
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
         emit_bounds(reg, vtype_int(lconst, hconst), kconst, hint);
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
   op->hint = hint ? xstrdup(hint) : NULL;

   VCODE_ASSERT(vtype_eq(vcode_reg_type(low), vcode_reg_type(high)),
                "type mismatch in dynamic bounds range");
   VCODE_ASSERT(vcode_reg_kind(kind) == VCODE_TYPE_OFFSET,
                "dynamic bounds kind argument must be offset");
   VCODE_ASSERT(!loc_invalid_p(vcode_last_loc()),
                "bounds check needs debug info");
}

void emit_array_size(vcode_reg_t llen, vcode_reg_t rlen, bounds_kind_t kind,
                     const char *hint)
{
   if (rlen == llen)
      return;

   op_t *op = vcode_add_op(VCODE_OP_ARRAY_SIZE);
   vcode_add_arg(op, llen);
   vcode_add_arg(op, rlen);
   op->subkind = kind;
   op->hint    = hint ? xstrdup(hint) : NULL;
}

static op_t *emit_index_check_null(vcode_reg_t rlow, vcode_reg_t rhigh,
                                   bounds_kind_t kind)
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
   op->subkind  = kind;
   op->type     = VCODE_INVALID_TYPE;

   return op;
}

void emit_index_check(vcode_reg_t rlow, vcode_reg_t rhigh, vcode_type_t bounds,
                      bounds_kind_t kind)
{
   if (vtype_includes(bounds, vcode_reg_data(rlow)->bounds)
       && vtype_includes(bounds, vcode_reg_data(rhigh)->bounds)) {
      emit_comment("Elided index check for r%d and r%d", rlow, rhigh);
      return;
   }

   op_t *op = emit_index_check_null(rlow, rhigh, kind);
   if (op != NULL)
      op->type = bounds;

   VCODE_ASSERT(!loc_invalid_p(vcode_last_loc()),
                "index check needs debug info");
}

void emit_dynamic_index_check(vcode_reg_t rlow, vcode_reg_t rhigh,
                              vcode_reg_t blow, vcode_reg_t bhigh,
                              bounds_kind_t kind)
{
   op_t *op = emit_index_check_null(rlow, rhigh, kind);
   if (op != NULL) {
      vcode_add_arg(op, blow);
      vcode_add_arg(op, bhigh);
   }

   VCODE_ASSERT(!loc_invalid_p(vcode_last_loc()),
                "dynamic index check needs debug info");
}

uint32_t emit_storage_hint(vcode_reg_t mem, vcode_reg_t length)
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
   return (active_block << 20) | (vcode_block_data()->ops.count - 1);
}

void emit_debug_out(vcode_reg_t reg)
{
   op_t *op = vcode_add_op(VCODE_OP_DEBUG_OUT);
   vcode_add_arg(op, reg);
}

void emit_cover_stmt(uint32_t tag)
{
   op_t *op = vcode_add_op(VCODE_OP_COVER_STMT);
   op->tag = tag;
}

void emit_cover_cond(vcode_reg_t test, uint32_t tag, unsigned sub)
{
   op_t *op = vcode_add_op(VCODE_OP_COVER_COND);
   vcode_add_arg(op, test);
   op->tag     = tag;
   op->subkind = sub;
}

vcode_reg_t emit_temp_stack_mark(void)
{
   op_t *op = vcode_add_op(VCODE_OP_TEMP_STACK_MARK);
   return (op->result = vcode_add_reg(vtype_offset()));
}

void emit_temp_stack_restore(vcode_reg_t reg)
{
   VCODE_FOR_EACH_OP(other) {
      if (other->kind == VCODE_OP_ALLOCA
          || other->kind == VCODE_OP_PCALL
          || other->kind == VCODE_OP_FCALL
          || other->kind == VCODE_OP_RESUME)
         break;
      else if (other->kind == VCODE_OP_TEMP_STACK_MARK)
         return;   // No use of temp stack between mark and restore
   }

   op_t *op = vcode_add_op(VCODE_OP_TEMP_STACK_RESTORE);
   vcode_add_arg(op, reg);

   VCODE_ASSERT(vcode_reg_kind(reg) == VCODE_TYPE_OFFSET,
                "saved heap must have offset type");
}

vcode_reg_t emit_undefined(vcode_type_t type)
{
   active_unit->flags |= UNIT_UNDEFINED;

   op_t *op = vcode_add_op(VCODE_OP_UNDEFINED);
   return (op->result = vcode_add_reg(type));
}

void emit_debug_info(const loc_t *loc)
{
   if (!loc_invalid_p(loc))
      vcode_block_data()->last_loc = *loc;
}

vcode_reg_t emit_link_signal(ident_t name, vcode_type_t type)
{
   op_t *op = vcode_add_op(VCODE_OP_LINK_SIGNAL);
   op->ident = name;

   VCODE_ASSERT(vtype_kind(type) == VCODE_TYPE_SIGNAL,
                "link signal type must be a signal");

   return (op->result = vcode_add_reg(type));
}

vcode_reg_t emit_link_var(ident_t name, vcode_type_t type)
{
   VCODE_FOR_EACH_MATCHING_OP(other, VCODE_OP_LINK_VAR) {
      if (other->ident == name)
         return other->result;
   }

   op_t *op = vcode_add_op(VCODE_OP_LINK_VAR);
   op->ident = name;

   if (vtype_kind(type) == VCODE_TYPE_CARRAY) {
      op->result = vcode_add_reg(vtype_pointer(vtype_elem(type)));
      vcode_reg_data(op->result)->bounds = vtype_bounds(type);
   }
   else {
      op->result = vcode_add_reg(vtype_pointer(type));
      vcode_reg_data(op->result)->bounds = type;
   }

   return op->result;
}

vcode_reg_t emit_link_package(ident_t name)
{
   VCODE_FOR_EACH_MATCHING_OP(other, VCODE_OP_LINK_PACKAGE) {
      if (other->ident == name)
         return other->result;
   }

   op_t *op = vcode_add_op(VCODE_OP_LINK_PACKAGE);
   op->ident = name;

   VCODE_ASSERT(name != active_unit->name, "cannot link the current unit");

   return (op->result = vcode_add_reg(vtype_context(name)));
}

static void vcode_write_unit(vcode_unit_t unit, fbuf_t *f,
                             ident_wr_ctx_t ident_wr_ctx,
                             loc_wr_ctx_t *loc_wr_ctx)
{
   write_u8(unit->kind, f);
   ident_write(unit->name, ident_wr_ctx);
   write_u32(unit->result, f);
   write_u32(unit->flags, f);
   write_u32(unit->depth, f);

   if (unit->context != NULL) {
      vcode_select_unit(unit);
      vcode_select_unit(vcode_unit_context());
      ident_write(vcode_unit_name(), ident_wr_ctx);
      vcode_close();
   }

   fbuf_put_uint(f, unit->blocks.count);
   for (unsigned i = 0; i < unit->blocks.count; i++) {
      const block_t *b = &(unit->blocks.items[i]);
      fbuf_put_uint(f, b->ops.count);

      for (unsigned j = 0; j < b->ops.count; j++) {
         const op_t *op = &(b->ops.items[j]);

         fbuf_put_uint(f, op->kind);
         fbuf_put_uint(f, op->result);
         loc_write(&(op->loc), loc_wr_ctx);

         fbuf_put_uint(f, op->args.count);
         for (unsigned k = 0; k < op->args.count; k++)
            fbuf_put_uint(f, op->args.items[k]);

         if (OP_HAS_TARGET(op->kind)) {
            fbuf_put_uint(f, op->targets.count);
            for (unsigned k = 0; k < op->targets.count; k++)
               fbuf_put_uint(f, op->targets.items[k]);
         }

         if (OP_HAS_TYPE(op->kind))
            fbuf_put_uint(f, op->type);
         if (OP_HAS_ADDRESS(op->kind))
            fbuf_put_uint(f, op->address);
         if (OP_HAS_FUNC(op->kind) || OP_HAS_IDENT(op->kind))
            ident_write(op->func, ident_wr_ctx);
         if (OP_HAS_SUBKIND(op->kind))
            fbuf_put_uint(f, op->subkind);
         if (OP_HAS_CMP(op->kind))
            fbuf_put_uint(f, op->cmp);
         if (OP_HAS_VALUE(op->kind))
            fbuf_put_int(f, op->value);
         if (OP_HAS_REAL(op->kind))
            write_double(op->real, f);
         if (OP_HAS_COMMENT(op->kind))
            ;   // Do not save comments
         if (OP_HAS_DIM(op->kind))
            fbuf_put_uint(f, op->dim);
         if (OP_HAS_HOPS(op->kind))
            fbuf_put_uint(f, op->hops);
         if (OP_HAS_FIELD(op->kind))
            fbuf_put_uint(f, op->field);
         if (OP_HAS_HINT(op->kind)) {
            if (op->hint == NULL)
               fbuf_put_uint(f, 0);
            else {
               const size_t len = strlen(op->hint);
               fbuf_put_uint(f, len);
               write_raw(op->hint, len, f);
            }
         }
         if (OP_HAS_TAG(op->kind))
            fbuf_put_uint(f, op->tag);
      }
   }

   fbuf_put_uint(f, unit->regs.count);
   for (unsigned i = 0; i < unit->regs.count; i++) {
      const reg_t *r = &(unit->regs.items[i]);
      fbuf_put_uint(f, r->type);
      fbuf_put_uint(f, r->bounds);
   }

   fbuf_put_uint(f, unit->types.count);
   for (unsigned i = 0; i < unit->types.count; i++) {
      const vtype_t *t = &(unit->types.items[i]);
      fbuf_put_uint(f, t->kind);
      switch (t->kind) {
      case VCODE_TYPE_INT:
      case VCODE_TYPE_OFFSET:
         fbuf_put_int(f, t->low);
         fbuf_put_int(f, t->high);
         break;

      case VCODE_TYPE_REAL:
         write_double(t->rlow, f);
         write_double(t->rhigh, f);
         break;

      case VCODE_TYPE_CARRAY:
      case VCODE_TYPE_UARRAY:
         fbuf_put_uint(f, t->dims);
         fbuf_put_uint(f, t->size);
         fbuf_put_uint(f, t->elem);
         fbuf_put_uint(f, t->bounds);
         break;

      case VCODE_TYPE_ACCESS:
      case VCODE_TYPE_POINTER:
         fbuf_put_uint(f, t->pointed);
         break;

      case VCODE_TYPE_FILE:
      case VCODE_TYPE_SIGNAL:
      case VCODE_TYPE_RESOLUTION:
      case VCODE_TYPE_CLOSURE:
         fbuf_put_uint(f, t->base);
         break;

      case VCODE_TYPE_OPAQUE:
         break;

      case VCODE_TYPE_CONTEXT:
         ident_write(t->name, ident_wr_ctx);
         break;

      case VCODE_TYPE_RECORD:
         ident_write(t->name, ident_wr_ctx);
         fbuf_put_uint(f, t->fields.count);
         for (unsigned j = 0; j < t->fields.count; j++)
            fbuf_put_uint(f, t->fields.items[j]);
         break;
      }
   }

   fbuf_put_uint(f, unit->vars.count);
   for (unsigned i = 0; i < unit->vars.count; i++) {
      const var_t *v = &(unit->vars.items[i]);
      fbuf_put_uint(f, v->type);
      fbuf_put_uint(f, v->bounds);
      ident_write(v->name, ident_wr_ctx);
      fbuf_put_uint(f, v->flags);
   }

   fbuf_put_uint(f, unit->params.count);
   for (unsigned i = 0; i < unit->params.count; i++) {
      const param_t *p = &(unit->params.items[i]);
      fbuf_put_uint(f, p->type);
      fbuf_put_uint(f, p->bounds);
      ident_write(p->name, ident_wr_ctx);
      fbuf_put_uint(f, p->reg);
   }

   if (unit->next != NULL)
      vcode_write_unit(unit->next, f, ident_wr_ctx, loc_wr_ctx);

   if (unit->children != NULL)
      vcode_write_unit(unit->children, f, ident_wr_ctx, loc_wr_ctx);
}

void vcode_write(vcode_unit_t unit, fbuf_t *f)
{
   assert(unit->kind == VCODE_UNIT_PACKAGE);

   write_u32(VCODE_MAGIC, f);
   write_u8(VCODE_VERSION, f);

   ident_wr_ctx_t ident_wr_ctx = ident_write_begin(f);
   loc_wr_ctx_t *loc_wr_ctx = loc_write_begin(f);
   vcode_write_unit(unit, f, ident_wr_ctx, loc_wr_ctx);
   write_u8(0xff, f);  // End marker
   ident_write_end(ident_wr_ctx);
   loc_write_end(loc_wr_ctx);
}

static vcode_unit_t vcode_read_unit(fbuf_t *f, ident_rd_ctx_t ident_rd_ctx,
                                    loc_rd_ctx_t *loc_rd_ctx)
{
   const uint8_t marker = read_u8(f);
   if (marker == 0xff)
      return false;

   vcode_unit_t unit = xcalloc(sizeof(struct vcode_unit));
   unit->kind     = marker;
   unit->name     = ident_read(ident_rd_ctx);
   unit->result   = read_u32(f);
   unit->flags    = read_u32(f);
   unit->depth    = read_u32(f);
   unit->refcount = 1;

   if (unit->kind != VCODE_UNIT_PACKAGE) {
      ident_t context_name = ident_read(ident_rd_ctx);
      unit->context = vcode_find_unit(context_name);
      if (unit->context == NULL)
         fatal("%s references nonexistent context %s", fbuf_file_name(f),
               istr(context_name));
   }
   else
      unit->context = NULL;

   block_array_resize(&(unit->blocks), fbuf_get_uint(f), 0);
   for (unsigned i = 0; i < unit->blocks.count; i++) {
      block_t *b = &(unit->blocks.items[i]);
      op_array_resize(&(b->ops), fbuf_get_uint(f), 0);

      for (unsigned j = 0; j < b->ops.count; j++) {
         op_t *op = &(b->ops.items[j]);

         op->kind = fbuf_get_uint(f);
         op->result = fbuf_get_uint(f);
         loc_read(&(op->loc), loc_rd_ctx);

         vcode_reg_array_resize(&(op->args), fbuf_get_uint(f), 0);
         for (unsigned k = 0; k < op->args.count; k++)
            op->args.items[k] = fbuf_get_uint(f);

         if (OP_HAS_TARGET(op->kind)) {
            vcode_block_array_resize(&(op->targets), fbuf_get_uint(f), 0);
            for (unsigned k = 0; k < op->targets.count; k++)
               op->targets.items[k] = fbuf_get_uint(f);
         }

         if (OP_HAS_TYPE(op->kind))
            op->type = fbuf_get_uint(f);
         if (OP_HAS_ADDRESS(op->kind))
            op->address = fbuf_get_uint(f);
         if (OP_HAS_FUNC(op->kind) || OP_HAS_IDENT(op->kind))
            op->func = ident_read(ident_rd_ctx);
         if (OP_HAS_SUBKIND(op->kind))
            op->subkind = fbuf_get_uint(f);
         if (OP_HAS_CMP(op->kind))
            op->cmp = fbuf_get_uint(f);
         if (OP_HAS_VALUE(op->kind))
            op->value = fbuf_get_int(f);
         if (OP_HAS_REAL(op->kind))
            op->real = read_double(f);
         if (OP_HAS_COMMENT(op->kind))
            op->comment = NULL;
         if (OP_HAS_DIM(op->kind))
            op->dim = fbuf_get_uint(f);
         if (OP_HAS_HOPS(op->kind))
            op->hops = fbuf_get_uint(f);
         if (OP_HAS_FIELD(op->kind))
            op->field = fbuf_get_uint(f);
         if (OP_HAS_HINT(op->kind)) {
            const size_t len = fbuf_get_uint(f);
            if (len == 0)
               op->hint = NULL;
            else {
               op->hint = xmalloc(len + 1);
               read_raw(op->hint, len, f);
               op->hint[len] = '\0';
            }
         }
         if (OP_HAS_TAG(op->kind))
            op->tag = fbuf_get_uint(f);
      }
   }

   reg_array_resize(&(unit->regs), fbuf_get_uint(f), 0);
   for (unsigned i = 0; i < unit->regs.count; i++) {
      reg_t *r = &(unit->regs.items[i]);
      r->type = fbuf_get_uint(f);
      r->bounds = fbuf_get_uint(f);
   }

   vtype_array_resize(&(unit->types), fbuf_get_uint(f), 0);
   for (unsigned i = 0; i < unit->types.count; i++) {
      vtype_t *t = &(unit->types.items[i]);
      switch ((t->kind = fbuf_get_uint(f))) {
      case VCODE_TYPE_INT:
      case VCODE_TYPE_OFFSET:
         t->low = fbuf_get_int(f);
         t->high = fbuf_get_int(f);
         break;

      case VCODE_TYPE_REAL:
         t->rlow = read_double(f);
         t->rhigh = read_double(f);
         break;

      case VCODE_TYPE_CARRAY:
      case VCODE_TYPE_UARRAY:
         t->dims = fbuf_get_uint(f);
         t->size = fbuf_get_uint(f);
         t->elem = fbuf_get_uint(f);
         t->bounds = fbuf_get_uint(f);
         break;

      case VCODE_TYPE_POINTER:
      case VCODE_TYPE_ACCESS:
         t->base = fbuf_get_uint(f);
         break;

      case VCODE_TYPE_FILE:
      case VCODE_TYPE_SIGNAL:
      case VCODE_TYPE_RESOLUTION:
      case VCODE_TYPE_CLOSURE:
         t->base = fbuf_get_uint(f);
         break;

      case VCODE_TYPE_OPAQUE:
         break;

      case VCODE_TYPE_CONTEXT:
         t->name = ident_read(ident_rd_ctx);
         break;

      case VCODE_TYPE_RECORD:
         {
            t->name = ident_read(ident_rd_ctx);
            vcode_type_array_resize(&(t->fields), fbuf_get_uint(f), 0);
            for (unsigned j = 0; j < t->fields.count; j++)
               t->fields.items[j] = fbuf_get_uint(f);
            break;
         }
      }
   }

   var_array_resize(&(unit->vars), fbuf_get_uint(f), 0);
   for (unsigned i = 0; i < unit->vars.count; i++) {
      var_t *v = &(unit->vars.items[i]);
      v->type = fbuf_get_uint(f);
      v->bounds = fbuf_get_uint(f);
      v->name = ident_read(ident_rd_ctx);
      v->flags = fbuf_get_uint(f);
   }

   param_array_resize(&(unit->params), fbuf_get_uint(f), 0);
   for (unsigned i = 0; i < unit->params.count; i++) {
      param_t *p = &(unit->params.items[i]);
      p->type = fbuf_get_uint(f);
      p->bounds = fbuf_get_uint(f);
      p->name = ident_read(ident_rd_ctx);
      p->reg = fbuf_get_uint(f);
   }

   vcode_registry_add(unit);

   return unit;
}

vcode_unit_t vcode_read(fbuf_t *f)
{
   if (read_u32(f) != VCODE_MAGIC)
      fatal("%s has invalid vcode header", fbuf_file_name(f));

   const uint8_t version = read_u8(f);
   if (version != VCODE_VERSION)
      fatal("%s was created with vcode format version %d (expected %d)",
            fbuf_file_name(f), version, VCODE_VERSION);

   ident_rd_ctx_t ident_rd_ctx = ident_read_begin(f);
   loc_rd_ctx_t *loc_rd_ctx = loc_read_begin(f);

   vcode_unit_t vu, root = NULL;
   while ((vu = vcode_read_unit(f, ident_rd_ctx, loc_rd_ctx))) {
      if (root == NULL)
         root = vu;
   }

   ident_read_end(ident_rd_ctx);
   loc_read_end(loc_rd_ctx);

   return root;
}

#if VCODE_CHECK_UNIONS
#define OP_USE_COUNT_U0(x)                                              \
   (OP_HAS_CMP(x) + OP_HAS_VALUE(x) + OP_HAS_REAL(x) +                  \
    OP_HAS_COMMENT(x) + OP_HAS_SIGNAL(x) + OP_HAS_DIM(x) +              \
    OP_HAS_HOPS(x) + OP_HAS_FIELD(x) + OP_HAS_HINT(x) +                 \
    OP_HAS_TAG(x))
#define OP_USE_COUNT_U1(x)                                              \
   (OP_HAS_SUBKIND(x) + OP_HAS_FUNC(x) + OP_HAS_ADDRESS(x))
#define OP_USE_COUNT_U2(x)                                              \
   (OP_HAS_BOOKMARK(x) + OP_HAS_TARGET(x))

__attribute__((constructor))
static void vcode_check_unions(void)
{
   printf("sizeof(op_t) = %ld\n", sizeof(op_t));
   for (int i = 0; i < 256; i++) {
      assert(OP_USE_COUNT_U0(i) <= 1);
      assert(OP_USE_COUNT_U1(i) <= 1);
      assert(OP_USE_COUNT_U2(i) <= 1);
   }
}
#endif  // VCODE_CHECK_UNIONS
