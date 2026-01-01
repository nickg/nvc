//
//  Copyright (C) 2024-2025  Nick Gasson
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
#include "mir/mir-node.h"
#include "mir/mir-structs.h"
#include "mir/mir-priv.h"

#include <assert.h>
#include <limits.h>
#include <stdarg.h>
#include <string.h>
#include <stdlib.h>

#define SMALL_CONST_BIAS (1 << (_MIR_ID_BITS - 1))
#define SMALL_CONST_MIN  (-SMALL_CONST_BIAS)
#define SMALL_CONST_MAX  (SMALL_CONST_BIAS - 1)

block_data_t *mir_block_data(mir_unit_t *mu, mir_block_t block)
{
   assert(block.tag == MIR_TAG_BLOCK);
   return AREF(mu->blocks, block.id);
}

mir_block_t mir_add_block(mir_unit_t *mu)
{
   mir_block_t b = { .tag = MIR_TAG_BLOCK, .id = mu->blocks.count };

   block_data_t bd = {
      .gap_pos = -1,
      .last_loc = mu->cursor.loc,
   };
   APUSH(mu->blocks, bd);

   return b;
}

void mir_set_cursor(mir_unit_t *mu, mir_block_t block, unsigned pos)
{
   if (!mir_is_null(mu->cursor.block))
      mir_block_data(mu, mu->cursor.block)->last_loc = mu->cursor.loc;

   mu->cursor.block = block;
   mu->cursor.pos   = pos;

   if (mir_is_null(block))
      mu->cursor.loc = LOC_INVALID;
   else
      mu->cursor.loc = mir_block_data(mu, block)->last_loc;
}

mir_block_t mir_get_cursor(mir_unit_t *mu, unsigned *pos)
{
   if (pos != NULL)
      *pos = mu->cursor.pos;

   return mu->cursor.block;
}

void mir_set_loc(mir_unit_t *mu, const loc_t *loc)
{
   mu->cursor.loc = *loc;
}

void mir_delete(mir_unit_t *mu)
{
   block_data_t *bd = mir_block_data(mu, mu->cursor.block);
   assert(mu->cursor.pos < bd->num_nodes);

   mir_value_t node = { .tag = MIR_TAG_NODE, .id = bd->nodes[mu->cursor.pos] };
   node_data_t *nd = mir_node_data(mu, node);
   nd->op = _MIR_DELETED_OP;
   nd->nargs = 0;

   if (bd->gap_pos == -1 || bd->gap_pos > mu->cursor.pos)
      bd->gap_pos = mu->cursor.pos;
}

void mir_compact(mir_unit_t *mu)
{
   for (int i = 0; i < mu->blocks.count; i++) {
      block_data_t *bd = &(mu->blocks.items[i]);
      if (bd->gap_pos == -1)
         continue;

      int wptr = bd->gap_pos;
      for (int j = bd->gap_pos; j < bd->num_nodes; j++) {
         if (mu->nodes[bd->nodes[j]].op != _MIR_DELETED_OP)
            bd->nodes[wptr++] = bd->nodes[j];
      }
      bd->num_nodes = wptr;
   }

   mir_set_cursor(mu, MIR_NULL_BLOCK, MIR_APPEND);
}

unsigned mir_count_blocks(mir_unit_t *mu)
{
   return mu->blocks.count;
}

mir_block_t mir_get_block(mir_unit_t *mu, unsigned nth)
{
   assert(nth < mu->blocks.count);
   return (mir_block_t) { .tag = MIR_TAG_BLOCK, .id = nth };
}

unsigned mir_count_nodes(mir_unit_t *mu, mir_block_t block)
{
   if (mir_is_null(block))
      return mu->num_nodes;
   else
      return mir_block_data(mu, block)->num_nodes;
}

mir_value_t mir_get_node(mir_unit_t *mu, mir_block_t block, unsigned nth)
{
   const block_data_t *bd = mir_block_data(mu, block);
   assert(nth < bd->num_nodes);
   return (mir_value_t) { .tag = MIR_TAG_NODE, .id = bd->nodes[nth] };
}

unsigned mir_count_vars(mir_unit_t *mu)
{
   return mu->vars.count;
}

mir_value_t mir_get_var(mir_unit_t *mu, unsigned nth)
{
   assert(nth < mu->vars.count);
   return (mir_value_t) { .tag = MIR_TAG_VAR, .id = nth };
}

mir_value_t mir_get_param(mir_unit_t *mu, unsigned nth)
{
   assert(nth < mu->params.count);
   return (mir_value_t) { .tag = MIR_TAG_PARAM, .id = nth };
}

unsigned mir_count_params(mir_unit_t *mu)
{
   return mu->params.count;
}

unsigned mir_count_vregs(mir_unit_t *mu)
{
   return mu->num_vregs;
}

mir_op_t mir_get_op(mir_unit_t *mu, mir_value_t node)
{
   switch (node.tag) {
   case MIR_TAG_NODE:
      return mir_node_data(mu, node)->op;
   default:
      return _MIR_DELETED_OP;
   }
}

const loc_t *mir_get_loc(mir_unit_t *mu, mir_value_t node)
{
   switch (node.tag) {
   case MIR_TAG_NODE:
      return &(mir_node_data(mu, node)->loc);
   case MIR_TAG_NULL:
      return &(mu->cursor.loc);
   default:
      should_not_reach_here();
   }
}

unsigned mir_count_args(mir_unit_t *mu, mir_value_t node)
{
   return mir_node_data(mu, node)->nargs;
}

const mir_value_t *mir_get_args(mir_unit_t *mu, const node_data_t *nd)
{
   if (nd->nargs <= MIR_INLINE_ARGS)
      return nd->args;
   else {
      assert(nd->spilloff < mu->num_argspill);
      return mu->argspill + nd->spilloff;
   }
}

mir_value_t mir_get_arg(mir_unit_t *mu, mir_value_t node, unsigned nth)
{
   const node_data_t *nd = mir_node_data(mu, node);
   if (nth >= nd->nargs)
      return MIR_NULL_VALUE;
   else
      return mir_get_args(mu, nd)[nth];
}

mir_value_t mir_add_param(mir_unit_t *mu, mir_type_t type, mir_stamp_t stamp,
                          ident_t name)
{
   mir_value_t p = { .tag = MIR_TAG_PARAM, .id = mu->params.count };

   param_data_t pd = {
      .name  = name,
      .type  = type,
      .stamp = stamp,
   };
   APUSH(mu->params, pd);

   return p;
}

mir_value_t mir_add_var(mir_unit_t *mu, mir_type_t type, mir_stamp_t stamp,
                        ident_t name, mir_var_flags_t flags)
{
   mir_value_t v = { .tag = MIR_TAG_VAR, .id = mu->vars.count };

   mir_mem_t mem = MIR_MEM_STACK;
   if (flags & MIR_VAR_HEAP)
      mem = MIR_MEM_LOCAL;

   var_data_t vd = {
      .name    = name,
      .type    = type,
      .pointer = mir_get_var_pointer(mu, type),
      .stamp   = mir_pointer_stamp(mu, mem, stamp),
      .flags   = flags,
   };
   APUSH(mu->vars, vd);

   return v;
}

bool mir_is_terminator(mir_op_t op)
{
   switch (op) {
   case MIR_OP_RETURN:
   case MIR_OP_JUMP:
   case MIR_OP_COND:
   case MIR_OP_WAIT:
   case MIR_OP_CASE:
   case MIR_OP_PCALL:
   case MIR_OP_UNREACHABLE:
      return true;
   default:
      return false;
   }
}

bool mir_block_finished(mir_unit_t *mu, mir_block_t block)
{
   if (mir_is_null(block))
      block = mu->cursor.block;

   const block_data_t *bd = mir_block_data(mu, block);
   if (bd->num_nodes == 0)
      return false;

   return mir_is_terminator(mu->nodes[bd->nodes[bd->num_nodes - 1]].op);
}

mir_vreg_t mir_get_vreg(mir_unit_t *mu, mir_value_t value)
{
   if (mu->vregs == NULL)
      fatal_trace("%pi has not been optimised", mu->name);

   assert(value.tag == MIR_TAG_NODE);
   return mu->vregs[value.id];
}

static inline node_id_t mir_node_id(mir_unit_t *mu, node_data_t *n)
{
   assert(n >= mu->nodes && n < mu->nodes + mu->num_nodes);
   return n - mu->nodes;
}

static node_data_t *mir_alloc_node(mir_unit_t *mu)
{
   assert(!mir_is_null(mu->cursor.block));

   node_data_t *n;
   block_data_t *bd = mir_block_data(mu, mu->cursor.block);
   if (mu->cursor.pos >= bd->num_nodes) {
      // Append new node
      if (bd->num_nodes == bd->max_nodes) {
         bd->max_nodes = MAX(4, bd->num_nodes * 2);
         bd->nodes = xrealloc_array(bd->nodes, bd->max_nodes,
                                    sizeof(node_id_t));
      }

      if (mu->num_nodes == mu->max_nodes) {
         mu->max_nodes = MAX(8, mu->num_nodes * 2);
         mu->nodes = xrealloc_array(mu->nodes, mu->max_nodes,
                                    sizeof(node_data_t));
      }

      bd->nodes[bd->num_nodes++] = mu->num_nodes;
      n = &(mu->nodes[mu->num_nodes++]);
   }
   else {
      n = &(mu->nodes[bd->nodes[mu->cursor.pos]]);
      assert(n->op == _MIR_DELETED_OP);
   }

   return n;
}

static size_t mir_spill_args(mir_unit_t *mu, unsigned num)
{
   if (mu->num_argspill + num > mu->max_argspill) {
      mu->max_argspill = MAX(16, MAX(mu->num_argspill + num,
                                     mu->max_argspill * 2));
      mu->argspill = xrealloc_array(mu->argspill, mu->max_argspill,
                                    sizeof(mir_value_t));
   }

   const size_t off = mu->num_argspill;
   mu->num_argspill += num;
   return off;
}

static node_data_t *mir_add_node(mir_unit_t *mu, mir_op_t op, mir_type_t type,
                                 mir_stamp_t stamp, unsigned nargs)
{
   node_data_t *n = mir_alloc_node(mu);
   n->loc   = mu->cursor.loc;
   n->op    = op;
   n->type  = type;
   n->stamp = stamp;
   n->nargs = nargs;

   if (nargs > MIR_INLINE_ARGS)
      n->spilloff = mir_spill_args(mu, nargs);

   MIR_ASSERT(mu->vregs == NULL, "cannot add nodes after register allocation");

   return n;
}

static mir_value_t mir_build_0(mir_unit_t *mu, mir_op_t op, mir_type_t type,
                               mir_stamp_t stamp)
{
   node_data_t *n = mir_add_node(mu, op, type, stamp, 0);
   return (mir_value_t){ .tag = MIR_TAG_NODE, .id = mir_node_id(mu, n) };
}

static mir_value_t mir_build_1(mir_unit_t *mu, mir_op_t op, mir_type_t type,
                               mir_stamp_t stamp, mir_value_t arg)
{
   node_data_t *n = mir_add_node(mu, op, type, stamp, 1);
   n->args[0] = arg;

   return (mir_value_t){ .tag = MIR_TAG_NODE, .id = mir_node_id(mu, n) };
}

static mir_value_t mir_build_2(mir_unit_t *mu, mir_op_t op, mir_type_t type,
                               mir_stamp_t stamp, mir_value_t arg1,
                               mir_value_t arg2)
{
   STATIC_ASSERT(MIR_INLINE_ARGS >= 2);

   node_data_t *n = mir_add_node(mu, op, type, stamp, 2);
   n->args[0] = arg1;
   n->args[1] = arg2;

   return (mir_value_t){ .tag = MIR_TAG_NODE, .id = mir_node_id(mu, n) };
}

static mir_value_t mir_build_3(mir_unit_t *mu, mir_op_t op, mir_type_t type,
                               mir_stamp_t stamp, mir_value_t arg1,
                               mir_value_t arg2, mir_value_t arg3)
{
   STATIC_ASSERT(MIR_INLINE_ARGS >= 3);

   node_data_t *n = mir_add_node(mu, op, type, stamp, 3);
   n->args[0] = arg1;
   n->args[1] = arg2;
   n->args[2] = arg3;

   return (mir_value_t){ .tag = MIR_TAG_NODE, .id = mir_node_id(mu, n) };
}

static mir_value_t mir_build_4(mir_unit_t *mu, mir_op_t op, mir_type_t type,
                               mir_stamp_t stamp, mir_value_t arg1,
                               mir_value_t arg2, mir_value_t arg3,
                               mir_value_t arg4)
{
   STATIC_ASSERT(MIR_INLINE_ARGS >= 4);

   node_data_t *n = mir_add_node(mu, op, type, stamp, 4);
   n->args[0] = arg1;
   n->args[1] = arg2;
   n->args[2] = arg3;
   n->args[3] = arg4;

   return (mir_value_t){ .tag = MIR_TAG_NODE, .id = mir_node_id(mu, n) };
}

static mir_value_t mir_build_5(mir_unit_t *mu, mir_op_t op, mir_type_t type,
                               mir_stamp_t stamp, mir_value_t arg1,
                               mir_value_t arg2, mir_value_t arg3,
                               mir_value_t arg4, mir_value_t arg5)
{
   node_data_t *n = mir_add_node(mu, op, type, stamp, 5);
   n->nargs = 5;

   STATIC_ASSERT(MIR_INLINE_ARGS < 5);

   mu->argspill[n->spilloff + 0] = arg1;
   mu->argspill[n->spilloff + 1] = arg2;
   mu->argspill[n->spilloff + 2] = arg3;
   mu->argspill[n->spilloff + 3] = arg4;
   mu->argspill[n->spilloff + 4] = arg5;

   return (mir_value_t){ .tag = MIR_TAG_NODE, .id = mir_node_id(mu, n) };
}

static mir_value_t mir_build_6(mir_unit_t *mu, mir_op_t op, mir_type_t type,
                               mir_stamp_t stamp, mir_value_t arg1,
                               mir_value_t arg2, mir_value_t arg3,
                               mir_value_t arg4, mir_value_t arg5,
                               mir_value_t arg6)
{
   node_data_t *n = mir_add_node(mu, op, type, stamp, 6);

   STATIC_ASSERT(MIR_INLINE_ARGS < 6);

   mu->argspill[n->spilloff + 0] = arg1;
   mu->argspill[n->spilloff + 1] = arg2;
   mu->argspill[n->spilloff + 2] = arg3;
   mu->argspill[n->spilloff + 3] = arg4;
   mu->argspill[n->spilloff + 4] = arg5;
   mu->argspill[n->spilloff + 5] = arg6;

   return (mir_value_t){ .tag = MIR_TAG_NODE, .id = mir_node_id(mu, n) };
}

static mir_value_t mir_add_linkage(mir_unit_t *mu, ident_t ident)
{
   for (int i = 0; i < mu->linkage.count; i++) {
      if (mu->linkage.items[i] == ident)
         return (mir_value_t){ .tag = MIR_TAG_LINKAGE, .id = i };
   }

   mir_value_t link = { .tag = MIR_TAG_LINKAGE, .id = mu->linkage.count };

   APUSH(mu->linkage, ident);

   return link;
}

static mir_value_t mir_add_extvar(mir_unit_t *mu, ident_t ident)
{
   for (int i = 0; i < mu->extvars.count; i++) {
      if (mu->extvars.items[i] == ident)
         return (mir_value_t){ .tag = MIR_TAG_EXTVAR, .id = i };
   }

   mir_value_t var = { .tag = MIR_TAG_EXTVAR, .id = mu->extvars.count };

   APUSH(mu->extvars, ident);

   return var;
}

void mir_set_arg(mir_unit_t *mu, node_data_t *n, unsigned nth,
                 mir_value_t value)
{
   assert(nth < n->nargs);

   if (n->nargs <= MIR_INLINE_ARGS)
      n->args[nth] = value;
   else {
      assert(n->spilloff < mu->num_argspill);
      mu->argspill[n->spilloff + nth] = value;
   }
}

node_data_t *mir_node_data(mir_unit_t *mu, mir_value_t value)
{
   assert(value.tag == MIR_TAG_NODE);
   assert(value.id < mu->num_nodes);

   return &(mu->nodes[value.id]);
}

const param_data_t *mir_param_data(mir_unit_t *mu, mir_value_t value)
{
   assert(value.tag == MIR_TAG_PARAM);
   return AREF(mu->params, value.id);
}

const var_data_t *mir_var_data(mir_unit_t *mu, mir_value_t value)
{
   assert(value.tag == MIR_TAG_VAR);
   return AREF(mu->vars, value.id);
}

#ifdef DEBUG
void mir_comment(mir_unit_t *mu, const char *fmt, ...)
{
   va_list ap;
   va_start(ap, fmt);

   if (mu->comments == NULL)
      mu->comments = tb_new();

   mir_value_t value = { .tag = MIR_TAG_COMMENT, .id = tb_len(mu->comments) };

   tb_vprintf(mu->comments, fmt, ap);
   tb_append(mu->comments, '\0');

   va_end(ap);

   mir_build_1(mu, MIR_OP_COMMENT, MIR_NULL_TYPE, MIR_NULL_STAMP, value);
}
#endif

mir_saved_loc_t _mir_push_debug_info(mir_unit_t *mu, const loc_t *loc)
{
   mir_saved_loc_t result = { mu, mu->cursor.loc };
   mu->cursor.loc = *loc;
   return result;
}

bool mir_get_const(mir_unit_t *mu, mir_value_t value, int64_t *result)
{
   switch (value.tag) {
   case MIR_TAG_CONST:
      *result = (int64_t)value.id - SMALL_CONST_BIAS;
      return true;
   case MIR_TAG_ENUM:
      *result = value.id;
      return true;
   case MIR_TAG_NODE:
      {
         const node_data_t *n = mir_node_data(mu, value);
         if (n->op == MIR_OP_CONST) {
            *result = n->iconst;
            return true;
         }
         else
            return false;
      }
   default:
      return false;
   }
}

bool mir_get_const_real(mir_unit_t *mu, mir_value_t value, double *result)
{
   switch (value.tag) {
   case MIR_TAG_NODE:
      {
         const node_data_t *n = mir_node_data(mu, value);
         if (n->op == MIR_OP_CONST_REAL) {
            *result = n->dconst;
            return true;
         }
         else
            return false;
      }
   default:
      return false;
   }
}

mir_type_t mir_get_type(mir_unit_t *mu, mir_value_t value)
{
   switch (value.tag) {
   case MIR_TAG_NODE:
      return mir_node_data(mu, value)->type;
   case MIR_TAG_PARAM:
      return mir_param_data(mu, value)->type;
   case MIR_TAG_VAR:
      return mir_var_data(mu, value)->pointer;
   default:
      return MIR_NULL_TYPE;
   }
}

mir_stamp_t mir_get_stamp(mir_unit_t *mu, mir_value_t value)
{
   switch (value.tag) {
   case MIR_TAG_NODE:
      return mir_node_data(mu, value)->stamp;
   case MIR_TAG_PARAM:
      return mir_param_data(mu, value)->stamp;
   case MIR_TAG_VAR:
      return mir_var_data(mu, value)->stamp;
   case MIR_TAG_CONST:
      {
         const int64_t ival = (int64_t)value.id - SMALL_CONST_BIAS;
         return mir_int_stamp(mu, ival, ival);
      }
   default:
      return MIR_NULL_STAMP;
   }
}

mir_mem_t mir_get_mem(mir_unit_t *mu, mir_value_t value)
{
   mir_type_t type = mir_get_type(mu, value);
   if (mir_is_null(type))
      return MIR_MEM_NONE;

   switch (mir_type_data(mu, type)->class) {
   case MIR_TYPE_UARRAY:
   case MIR_TYPE_CARRAY:
   case MIR_TYPE_RECORD:
   case MIR_TYPE_POINTER:
   case MIR_TYPE_ACCESS:
      break;
   default:
      return MIR_MEM_NONE;
   }

   mir_stamp_t stamp = mir_get_stamp(mu, value);
   if (mir_is_null(stamp))
      return MIR_MEM_TOP;

   const stamp_data_t *sd = mir_stamp_data(mu, stamp);
   if (sd->kind == MIR_STAMP_POINTER)
      return sd->u.pointer.memory;

   return MIR_MEM_NONE;
}

ident_t mir_get_name(mir_unit_t *mu, mir_value_t value)
{
   switch (value.tag) {
   case MIR_TAG_PARAM:
      return mir_param_data(mu, value)->name;
   case MIR_TAG_VAR:
      return mir_var_data(mu, value)->name;
   case MIR_TAG_LINKAGE:
      return AGET(mu->linkage, value.id);
   case MIR_TAG_EXTVAR:
      return AGET(mu->extvars, value.id);
   case MIR_TAG_NULL:
      return mu->name;
   default:
      return NULL;
   }
}

void mir_set_result(mir_unit_t *mu, mir_type_t type)
{
   assert(mu->kind == MIR_UNIT_FUNCTION || mu->kind == MIR_UNIT_THUNK);
   mu->result = type;
}

mir_type_t mir_get_result(mir_unit_t *mu)
{
   return mu->result;
}

object_t *mir_get_locus(mir_unit_t *mu, mir_value_t value)
{
   switch (value.tag) {
   case MIR_TAG_NODE:
      {
         node_data_t *n = mir_node_data(mu, value);
         assert(n->op == MIR_OP_LOCUS);
         return n->locus;
      }
      break;
   default:
      should_not_reach_here();
   }
}

void mir_get_bits(mir_unit_t *mu, mir_value_t value, uint64_t *abits,
                  uint64_t *bbits)
{
   switch (value.tag) {
   case MIR_TAG_NODE:
      {
         node_data_t *n = mir_node_data(mu, value);
         assert(n->op == MIR_OP_CONST_VEC);

         *abits = n->bits[0];
         *bbits = n->bits[1];
      }
      break;
   default:
      should_not_reach_here();
   }
}

mir_var_flags_t mir_get_var_flags(mir_unit_t *mu, mir_value_t value)
{
   switch (value.tag) {
   case MIR_TAG_VAR:
      return mir_var_data(mu, value)->flags;
   default:
      return 0;
   }
}

mir_type_t mir_get_var_type(mir_unit_t *mu, mir_value_t value)
{
   switch (value.tag) {
   case MIR_TAG_VAR:
      return mir_var_data(mu, value)->type;
   default:
      return MIR_NULL_TYPE;
   }
}

bool mir_is_integral(mir_unit_t *mu, mir_value_t value)
{
   if (value.tag == MIR_TAG_CONST)
      return true;

   mir_type_t type = mir_get_type(mu, value);
   if (mir_is_null(type))
      return false;

   switch (mir_type_data(mu, type)->class) {
   case MIR_TYPE_INT:
   case MIR_TYPE_OFFSET:
      return true;
   default:
      return false;
   }
}

bool mir_is_numeric(mir_unit_t *mu, mir_value_t value)
{
   if (value.tag == MIR_TAG_CONST)
      return true;

   mir_type_t type = mir_get_type(mu, value);
   if (mir_is_null(type))
      return false;

   switch (mir_type_data(mu, type)->class) {
   case MIR_TYPE_INT:
   case MIR_TYPE_OFFSET:
   case MIR_TYPE_REAL:
   case MIR_TYPE_VEC2:
   case MIR_TYPE_VEC4:
      return true;
   default:
      return false;
   }
}

bool mir_is_scalar(mir_unit_t *mu, mir_value_t value)
{
   if (value.tag == MIR_TAG_CONST)
      return true;

   mir_type_t type = mir_get_type(mu, value);
   if (mir_is_null(type))
      return false;

   switch (mir_type_data(mu, type)->class) {
   case MIR_TYPE_INT:
   case MIR_TYPE_OFFSET:
   case MIR_TYPE_REAL:
   case MIR_TYPE_ACCESS:
   case MIR_TYPE_POINTER:
   case MIR_TYPE_UARRAY:
   case MIR_TYPE_SIGNAL:
   case MIR_TYPE_CONTEXT:
   case MIR_TYPE_RESOLUTION:
   case MIR_TYPE_FILE:
   case MIR_TYPE_TRIGGER:
   case MIR_TYPE_VEC2:
   case MIR_TYPE_VEC4:
      return true;
   default:
      return false;
   }
}

bool mir_is_vector(mir_unit_t *mu, mir_value_t value)
{
   mir_type_t type = mir_get_type(mu, value);
   if (mir_is_null(type))
      return false;

   switch (mir_type_data(mu, type)->class) {
   case MIR_TYPE_VEC2:
   case MIR_TYPE_VEC4:
      return true;
   default:
      return false;
   }
}

bool mir_is_bool(mir_unit_t *mu, mir_value_t value)
{
   if (value.tag == MIR_TAG_CONST)
      return value.id == SMALL_CONST_BIAS || value.id == SMALL_CONST_BIAS + 1;
   else
      return mir_equals(mir_get_type(mu, value), mir_bool_type(mu));
}

bool mir_is_time(mir_unit_t *mu, mir_value_t value)
{
   if (value.tag == MIR_TAG_CONST)
      return true;

   return mir_equals(mir_get_type(mu, value), mir_time_type(mu));
}

bool mir_is(mir_unit_t *mu, mir_value_t value, mir_class_t class)
{
   mir_type_t type = mir_get_type(mu, value);
   if (mir_is_null(type))
      return false;

   return mir_type_data(mu, type)->class == class;
}

bool mir_points_to(mir_unit_t *mu, mir_value_t value, mir_class_t class)
{
   mir_type_t type = mir_get_type(mu, value);
   if (mir_is_null(type))
      return false;

   const type_data_t *td = mir_type_data(mu, type);
   if (td->class != MIR_TYPE_POINTER)
      return false;

   return mir_type_data(mu, td->u.pointer)->class == class;
}

bool mir_is_signal(mir_unit_t *mu, mir_value_t value)
{
   return mir_is(mu, value, MIR_TYPE_SIGNAL);
}

bool mir_is_offset(mir_unit_t *mu, mir_value_t value)
{
   if (value.tag == MIR_TAG_CONST)
      return true;

   return mir_is(mu, value, MIR_TYPE_OFFSET);
}

bool mir_is_const(mir_unit_t *mu, mir_value_t value)
{
   switch (value.tag) {
   case MIR_TAG_CONST:
      return true;
   case MIR_TAG_NODE:
      {
         node_data_t *n = mir_node_data(mu, value);
         switch (n->op) {
         case MIR_OP_CONST:
         case MIR_OP_CONST_REAL:
         case MIR_OP_CONST_ARRAY:
         case MIR_OP_CONST_REP:
         case MIR_OP_CONST_RECORD:
         case MIR_OP_NULL:
            return true;
         default:
            return false;
         }
      }
   default:
      return false;
   }
}

bool mir_may_alias(mir_unit_t *mu, mir_value_t a, mir_value_t b)
{
   const mir_mem_t a_mem = mir_get_mem(mu, a);
   if (a_mem == MIR_MEM_NONE)
      return false;

   if (mir_equals(a, b))
      return true;

   const mir_mem_t b_mem = mir_get_mem(mu, b);
   if (b_mem == MIR_MEM_NONE)
      return false;

   if (a_mem == MIR_MEM_CONST || b_mem == MIR_MEM_CONST) {
      // Aliasing is only relevant in the presence of mutability
      return false;
   }

   return a_mem == b_mem || a_mem == MIR_MEM_TOP || b_mem == MIR_MEM_TOP;
}

#ifdef DEBUG
static bool mir_check_type(mir_unit_t *mu, mir_value_t value, mir_type_t type)
{
   if (value.tag == MIR_TAG_CONST) {
      const mir_class_t class = mir_type_data(mu, type)->class;
      return class == MIR_TYPE_INT || class == MIR_TYPE_OFFSET;
   }
   else
      return mir_equals(mir_get_type(mu, value), type);
}
#endif

mir_value_t mir_enum(unsigned value)
{
   assert(value < MIR_ID_MAX);
   return (mir_value_t){ .tag = MIR_TAG_ENUM, .id = value };
}

mir_value_t mir_const(mir_unit_t *mu, mir_type_t type, int64_t value)
{
   mir_value_t result;
   if (value >= SMALL_CONST_MIN && value <= SMALL_CONST_MAX) {
      const unsigned biased = value + SMALL_CONST_BIAS;
      result = (mir_value_t){ .tag = MIR_TAG_CONST, .id = biased };
   }
   else {
      mir_stamp_t stamp = mir_int_stamp(mu, value, value);
      node_data_t *n = mir_add_node(mu, MIR_OP_CONST, type, stamp, 0);
      n->iconst = value;

      result = (mir_value_t){ .tag = MIR_TAG_NODE, .id = mir_node_id(mu, n) };
   }

#ifdef DEBUG
   const mir_class_t class = mir_get_class(mu, type);
   MIR_ASSERT(class == MIR_TYPE_INT || class == MIR_TYPE_OFFSET,
              "constant must have integral type");
#endif

   return result;
}

mir_value_t mir_const_real(mir_unit_t *mu, mir_type_t type, double value)
{
   mir_stamp_t stamp = mir_real_stamp(mu, value, value);
   node_data_t *n = mir_add_node(mu, MIR_OP_CONST_REAL, type, stamp, 0);
   n->dconst = value;

   return (mir_value_t){ .tag = MIR_TAG_NODE, .id = mir_node_id(mu, n) };
}

mir_value_t mir_const_vec(mir_unit_t *mu, mir_type_t type, uint64_t abits,
                          uint64_t bbits)
{
   uint64_t mask = ~UINT64_C(0), size = mir_get_size(mu, type);
   if (size < 64)
      mask >>= 64 - size;

   node_data_t *n = mir_add_node(mu, MIR_OP_CONST_VEC, type, MIR_NULL_STAMP, 0);
   n->bits[0] = abits & mask;
   n->bits[1] = bbits & mask;

#ifdef DEBUG
   const type_data_t *td = mir_type_data(mu, type);
   MIR_ASSERT(td->class == MIR_TYPE_VEC2 || td->class == MIR_TYPE_VEC4,
              "constant vector must have vector type");
   MIR_ASSERT(bbits == 0 || td->class == MIR_TYPE_VEC4,
              "b-bits cannot be set for two-value vector");
   MIR_ASSERT(size <= 64, "constant vector size must be 64 bits or less");
#endif

   return (mir_value_t){ .tag = MIR_TAG_NODE, .id = mir_node_id(mu, n) };
}

mir_value_t mir_const_array(mir_unit_t *mu, mir_type_t type,
                            const mir_value_t *values, size_t count)
{
   node_data_t *n = mir_add_node(mu, MIR_OP_CONST_ARRAY, type,
                                 MIR_NULL_STAMP, count);

   MIR_ASSERT(mir_get_class(mu, type) == MIR_TYPE_CARRAY,
              "constant array must have constrained array type");
   MIR_ASSERT(mir_get_size(mu, type) == count, "expected %d elements but "
              "have %zd", mir_get_size(mu, type), count);

   mir_type_t elem = mir_get_elem(mu, type);

   const bool integral = mir_get_class(mu, elem) == MIR_TYPE_INT;
   int64_t low = INT64_MAX, high = INT64_MIN;
   for (size_t i = 0; i < count; i++) {
      mir_set_arg(mu, n, i, values[i]);

      MIR_ASSERT(mir_check_type(mu, values[i], elem),
                 "element %zd has wrong type", i);
      MIR_ASSERT(mir_is_const(mu, values[i]), "element %zd not const", i);

      int64_t cval;
      if (integral && mir_get_const(mu, values[i], &cval)) {
         low = MIN(low, cval);
         high = MAX(high, cval);
      }
   }

   mir_stamp_t stamp = MIR_NULL_STAMP;
   if (integral && low <= high)
      stamp = mir_int_stamp(mu, low, high);

   n->stamp = mir_pointer_stamp(mu, MIR_MEM_CONST, stamp);

   return (mir_value_t){ .tag = MIR_TAG_NODE, .id = mir_node_id(mu, n) };
}

mir_value_t mir_const_string(mir_unit_t *mu, const char *str)
{
   int count = 0, low = UINT8_MAX, high = 0;
   for (const char *p = str; *p; p++, count++) {
      low = MIN(low, (unsigned char)*p);
      high = MAX(high, (unsigned char)*p);
   }

   mir_type_t t_char = mir_char_type(mu);
   mir_type_t type = mir_carray_type(mu, count, t_char);
   mir_stamp_t stamp = mir_int_stamp(mu, low, high);

   node_data_t *n = mir_add_node(mu, MIR_OP_CONST_ARRAY, type,
                                 stamp, count);

   for (int i = 0; i < count; i++)
      mir_set_arg(mu, n, i, mir_const(mu, t_char, (unsigned char)str[i]));

   return (mir_value_t){ .tag = MIR_TAG_NODE, .id = mir_node_id(mu, n) };
}

mir_value_t mir_build_const_rep(mir_unit_t *mu, mir_type_t type,
                                mir_value_t value, unsigned rep)
{
   mir_stamp_t stamp = mir_get_stamp(mu, value);
   mir_value_t result = mir_build_2(mu, MIR_OP_CONST_REP, type, stamp,
                                    value, mir_enum(rep));

   MIR_ASSERT(mir_get_class(mu, type) == MIR_TYPE_CARRAY,
              "constant array must have constrained array class");
   MIR_ASSERT(rep < MIR_ID_MAX, "repetitions out of range");

   return result;
}

mir_value_t mir_const_record(mir_unit_t *mu, mir_type_t type,
                             const mir_value_t *values, size_t count)
{
   node_data_t *n = mir_add_node(mu, MIR_OP_CONST_RECORD, type,
                                 MIR_NULL_STAMP, count);

   DEBUG_ONLY(const type_data_t *td = mir_type_data(mu, type));

   MIR_ASSERT(td->class == MIR_TYPE_RECORD,
              "const record must have record type");
   MIR_ASSERT(td->u.record.count == count, "expected %u fields but have %zu",
              td->u.record.count, count);

   for (int i = 0; i < count; i++) {
      mir_set_arg(mu, n, i, values[i]);

      MIR_ASSERT(mir_same_type(mu, mir_get_type(mu, values[i]),
                               td->u.record.fields[i]),
                 "wrong type for element %d", i);
      MIR_ASSERT(mir_is_const(mu, values[i]), "element %d is not constant", i);
   }

   return (mir_value_t){ .tag = MIR_TAG_NODE, .id = mir_node_id(mu, n) };
}

static mir_value_t mir_build_arith(mir_unit_t *mu, mir_op_t op, mir_type_t type,
                                   mir_value_t left, mir_value_t right,
                                   mir_value_t locus, mir_stamp_t stamp)
{
   mir_value_t result;
   if (mir_is_null(locus))
      result = mir_build_2(mu, op, type, stamp, left, right);
   else
      result = mir_build_3(mu, op, type, stamp, left, right, locus);

#ifdef DEBUG
   switch (mir_get_class(mu, type)) {
   case MIR_TYPE_INT:
   case MIR_TYPE_REAL:
   case MIR_TYPE_OFFSET:
      {
         mir_type_t ltype = mir_get_type(mu, left);
         mir_type_t rtype = mir_get_type(mu, right);

         MIR_ASSERT(mir_is_null(ltype) || mir_is_null(rtype)
                    || mir_equals(ltype, rtype),
                    "arguments to %s are not the same type", mir_op_string(op));
      }
      break;

   case MIR_TYPE_VEC2:
   case MIR_TYPE_VEC4:
      {
         const type_data_t *td = mir_type_data(mu, type);
         const type_data_t *ltd = mir_type_data(mu, mir_get_type(mu, left));
         const type_data_t *rtd = mir_type_data(mu, mir_get_type(mu, right));

         MIR_ASSERT(td->class == ltd->class && td->class == rtd->class,
                    "cannot mix vector types");
         MIR_ASSERT(td->u.vec.issigned == ltd->u.vec.issigned
                    && td->u.vec.issigned == rtd->u.vec.issigned,
                    "cannot mix vector signedness");
         MIR_ASSERT(td->u.vec.size >= ltd->u.vec.size
                    && td->u.vec.size >= rtd->u.vec.size,
                    "implicit narrowing conversions are not allowed");
      }
      break;

   default:
      MIR_ASSERT(false, "arithmetic is not allowed on this type");
      break;
   }

   MIR_ASSERT(mir_is_null(locus) || mir_is(mu, locus, MIR_TYPE_LOCUS),
              "locus argument to %s is not a locus", mir_op_string(op));
#endif

   return result;
}

static mir_value_t mir_build_add_op(mir_unit_t *mu, mir_op_t op,
                                    mir_type_t type, mir_value_t left,
                                    mir_value_t right, mir_value_t locus)
{
   int64_t lval, rval, cval;
   const bool lconst = mir_get_const(mu, left, &lval);
   const bool rconst = mir_get_const(mu, right, &rval);

   if (lconst && lval == 0)
      return right;
   else if (rconst && rval == 0)
      return left;
   else if (lconst && rconst && !__builtin_add_overflow(lval, rval, &cval))
      return mir_const(mu, type, cval);

   double lreal, rreal;
   const bool lconst_real = mir_get_const_real(mu, left, &lreal);
   const bool rconst_real = mir_get_const_real(mu, right, &rreal);

   if (lconst_real && (lreal == 0.0 || lreal == -0.0))
      return right;
   else if (rconst_real && (rreal == 0.0 || rreal == -0.0))
      return left;
   else if (lconst_real && rconst_real)
      return mir_const_real(mu, type, lreal + rreal);

   mir_stamp_t lstamp = mir_get_stamp(mu, left);
   mir_stamp_t rstamp = mir_get_stamp(mu, right);
   mir_stamp_t stamp = mir_stamp_add(mu, lstamp, rstamp);

   return mir_build_arith(mu, op, type, left, right, locus, stamp);
}

mir_value_t mir_build_add(mir_unit_t *mu, mir_type_t type, mir_value_t left,
                          mir_value_t right)
{
   return mir_build_add_op(mu, MIR_OP_ADD, type, left, right, MIR_NULL_VALUE);
}

mir_value_t mir_build_trap_add(mir_unit_t *mu, mir_type_t type,
                               mir_value_t left, mir_value_t right,
                               mir_value_t locus)
{
   return mir_build_add_op(mu, MIR_OP_TRAP_ADD, type, left, right, locus);
}

static mir_value_t mir_build_sub_op(mir_unit_t *mu, mir_op_t op,
                                    mir_type_t type, mir_value_t left,
                                    mir_value_t right, mir_value_t locus)
{
   int64_t lval, rval, cval;
   const bool lconst = mir_get_const(mu, left, &lval);
   const bool rconst = mir_get_const(mu, right, &rval);

   if (lconst && lval == 0)
      return right;
   else if (rconst && rval == 0)
      return left;
   else if (lconst && rconst && !__builtin_sub_overflow(lval, rval, &cval))
      return mir_const(mu, type, cval);
   else if (mir_equals(left, right))
      return mir_get_class(mu, type) == MIR_TYPE_REAL
         ? mir_const_real(mu, type, 0.0) : mir_const(mu, type, 0);

   double lreal, rreal;
   const bool lconst_real = mir_get_const_real(mu, left, &lreal);
   const bool rconst_real = mir_get_const_real(mu, right, &rreal);

   if (lconst_real && (lreal == 0.0 || lreal == -0.0))
      return right;
   else if (rconst_real && (rreal == 0.0 || rreal == -0.0))
      return left;
   else if (lconst_real && rconst_real)
      return mir_const_real(mu, type, lreal - rreal);

   mir_stamp_t lstamp = mir_get_stamp(mu, left);
   mir_stamp_t rstamp = mir_get_stamp(mu, right);
   mir_stamp_t stamp = mir_stamp_sub(mu, lstamp, rstamp);

   return mir_build_arith(mu, op, type, left, right, locus, stamp);
}

mir_value_t mir_build_sub(mir_unit_t *mu, mir_type_t type, mir_value_t left,
                          mir_value_t right)
{
   return mir_build_sub_op(mu, MIR_OP_SUB, type, left, right, MIR_NULL_VALUE);
}

mir_value_t mir_build_trap_sub(mir_unit_t *mu, mir_type_t type,
                               mir_value_t left, mir_value_t right,
                               mir_value_t locus)
{
   return mir_build_sub_op(mu, MIR_OP_TRAP_SUB, type, left, right, locus);
}

mir_value_t mir_build_neg(mir_unit_t *mu, mir_type_t type, mir_value_t value)
{
   int64_t cval;
   if (mir_get_const(mu, value, &cval))
      return mir_const(mu, type, -cval);

   mir_value_t result = mir_build_1(mu, MIR_OP_NEG, type,
                                    MIR_NULL_STAMP, value);

   MIR_ASSERT(mir_is_numeric(mu, value), "argument must be numeric");

   return result;
}

mir_value_t mir_build_trap_neg(mir_unit_t *mu, mir_type_t type,
                               mir_value_t value, mir_value_t locus)
{
   int64_t cval;
   if (mir_get_const(mu, value, &cval) && cval >= 0)
      return mir_const(mu, type, -cval);
   // TODO: check if stamp bounds are non-negative

   mir_value_t result = mir_build_2(mu, MIR_OP_TRAP_NEG, type,
                                    MIR_NULL_STAMP, value, locus);

   MIR_ASSERT(mir_is_numeric(mu, value), "argument must be numeric");
   MIR_ASSERT(mir_is(mu, locus, MIR_TYPE_LOCUS),
              "locus argument must be debug locus");

   return result;
}

mir_value_t mir_build_abs(mir_unit_t *mu, mir_type_t type, mir_value_t value)
{
   int64_t lconst;
   if (mir_get_const(mu, value, &lconst))
      return mir_const(mu, type, llabs(lconst));

   mir_value_t result = mir_build_1(mu, MIR_OP_ABS, type, MIR_NULL_STAMP,
                                    value);

   MIR_ASSERT(mir_is_numeric(mu, value), "argument must be numeric");

   return result;
}

static mir_value_t mir_build_mul_op(mir_unit_t *mu, mir_op_t op,
                                    mir_type_t type, mir_value_t left,
                                    mir_value_t right, mir_value_t locus)
{
   int64_t lval, rval, cval;
   const bool lconst = mir_get_const(mu, left, &lval);
   const bool rconst = mir_get_const(mu, right, &rval);

   if (lconst && lval == 0)
      return left;
   else if (rconst && rval == 0)
      return right;
   else if (lconst && lval == 1)
      return right;
   else if (rconst && rval == 1)
      return left;
   else if (lconst && rconst && !__builtin_mul_overflow(lval, rval, &cval))
      return mir_const(mu, type, cval);

   double lreal, rreal;
   const bool lconst_real = mir_get_const_real(mu, left, &lreal);
   const bool rconst_real = mir_get_const_real(mu, right, &rreal);

   if (lconst_real && rconst_real)
      return mir_const_real(mu, type, lreal * rreal);

   mir_stamp_t lstamp = mir_get_stamp(mu, left);
   mir_stamp_t rstamp = mir_get_stamp(mu, right);
   mir_stamp_t stamp = mir_stamp_mul(mu, lstamp, rstamp);

   return mir_build_arith(mu, op, type, left, right, locus, stamp);
}

mir_value_t mir_build_mul(mir_unit_t *mu, mir_type_t type, mir_value_t left,
                          mir_value_t right)
{
   return mir_build_mul_op(mu, MIR_OP_MUL, type, left, right, MIR_NULL_VALUE);
}

mir_value_t mir_build_trap_mul(mir_unit_t *mu, mir_type_t type,
                               mir_value_t left, mir_value_t right,
                               mir_value_t locus)
{
   return mir_build_mul_op(mu, MIR_OP_TRAP_MUL, type, left, right, locus);
}

mir_value_t mir_build_div(mir_unit_t *mu, mir_type_t type, mir_value_t left,
                          mir_value_t right)
{
   int64_t lval, rval;
   const bool lconst = mir_get_const(mu, left, &lval);
   const bool rconst = mir_get_const(mu, right, &rval);

   if (lconst && rconst && rval != 0)
      return mir_const(mu, type, lval / rval);
   else if (rconst && rval == 1)
      return left;

   mir_stamp_t lstamp = mir_get_stamp(mu, left);
   mir_stamp_t rstamp = mir_get_stamp(mu, right);
   mir_stamp_t stamp = mir_stamp_div(mu, lstamp, rstamp);

   return mir_build_arith(mu, MIR_OP_DIV, type, left, right,
                          MIR_NULL_VALUE, stamp);
}

mir_value_t mir_build_rem(mir_unit_t *mu, mir_type_t type, mir_value_t left,
                          mir_value_t right)
{
   int64_t lval, rval;
   const bool lconst = mir_get_const(mu, left, &lval);
   const bool rconst = mir_get_const(mu, right, &rval);

   if (lconst && rconst && lval > 0 && rval > 0)
      return mir_const(mu, type, lval % rval);

   mir_stamp_t lstamp = mir_get_stamp(mu, left);
   mir_stamp_t rstamp = mir_get_stamp(mu, right);
   mir_stamp_t stamp = mir_stamp_rem(mu, lstamp, rstamp);

   return mir_build_arith(mu, MIR_OP_REM, type, left, right,
                          MIR_NULL_VALUE, stamp);
}

mir_value_t mir_build_mod(mir_unit_t *mu, mir_type_t type, mir_value_t left,
                          mir_value_t right)
{
   int64_t lval, rval;
   const bool lconst = mir_get_const(mu, left, &lval);
   const bool rconst = mir_get_const(mu, right, &rval);

   if (lconst && rconst && lval > 0 && rval > 0)
      return mir_const(mu, type, lval % rval);

   mir_stamp_t lstamp = mir_get_stamp(mu, left);
   mir_stamp_t rstamp = mir_get_stamp(mu, right);

   if (!mir_is_null(lstamp) && !mir_is_null(rstamp)) {
      const stamp_data_t *lsd = mir_stamp_data(mu, lstamp);
      const stamp_data_t *rsd = mir_stamp_data(mu, rstamp);

      if (lsd->u.intg.low >= 0 && rsd->u.intg.low >= 0) {
         // If both arguments are non-negative then rem is equivalent
         // and cheaper to compute
         mir_stamp_t stamp = mir_stamp_rem(mu, lstamp, rstamp);
         return mir_build_arith(mu, MIR_OP_REM, type, left, right,
                                MIR_NULL_VALUE, stamp);
      }
   }

   return mir_build_arith(mu, MIR_OP_MOD, type, left, right,
                          MIR_NULL_VALUE, MIR_NULL_STAMP);
}

mir_value_t mir_build_exp(mir_unit_t *mu, mir_type_t type, mir_value_t left,
                          mir_value_t right)
{
   return mir_build_arith(mu, MIR_OP_EXP, type, left, right,
                          MIR_NULL_VALUE, MIR_NULL_STAMP);
}

mir_value_t mir_build_trap_exp(mir_unit_t *mu, mir_type_t type,
                               mir_value_t left, mir_value_t right,
                               mir_value_t locus)
{
   int64_t rconst;
   if (mir_get_const(mu, right, &rconst)) {
      if (rconst == 0)
         return mir_const(mu, type, 1);
      else if (rconst == 1)
         return left;
   }

   mir_value_t result = mir_build_arith(mu, MIR_OP_TRAP_EXP, type, left, right,
                                        locus, MIR_NULL_STAMP);

   MIR_ASSERT(mir_is_integral(mu, result),
              "trapping exp may only be used with integer types");

   return result;
}

static mir_value_t mir_build_logical(mir_unit_t *mu, mir_op_t op,
                                     mir_type_t type, mir_value_t left,
                                     mir_value_t right)
{
   mir_value_t result = mir_build_2(mu, op, type, MIR_NULL_STAMP, left, right);

   MIR_ASSERT(mir_is_bool(mu, left), "left argument to %s is not bool",
              mir_op_string(op));
   MIR_ASSERT(mir_is_bool(mu, right), "right argument to %s is not bool",
              mir_op_string(op));

   return result;
}

mir_value_t mir_build_and(mir_unit_t *mu, mir_value_t left, mir_value_t right)
{
   int64_t lval, rval;
   const bool lconst = mir_get_const(mu, left, &lval);
   const bool rconst = mir_get_const(mu, right, &rval);

   mir_type_t t_bool = mir_bool_type(mu);

   if (lconst && rconst)
      return mir_const(mu, t_bool, lval && rval);
   else if (lconst)
      return lval ? right : mir_const(mu, t_bool, 0);
   else if (rconst)
      return rval ? left : mir_const(mu, t_bool, 0);

   return mir_build_logical(mu, MIR_OP_AND, t_bool, left, right);
}

mir_value_t mir_build_or(mir_unit_t *mu, mir_value_t left, mir_value_t right)
{
   int64_t lval, rval;
   const bool lconst = mir_get_const(mu, left, &lval);
   const bool rconst = mir_get_const(mu, right, &rval);

   mir_type_t t_bool = mir_bool_type(mu);

   if (lconst && rconst)
      return mir_const(mu, t_bool, lval || rval);
   else if (lconst)
      return lval ? mir_const(mu, t_bool, 1) : right;
   else if (rconst)
      return rval ? mir_const(mu, t_bool, 1) : left;

   return mir_build_logical(mu, MIR_OP_OR, t_bool, left, right);
}

mir_value_t mir_build_xor(mir_unit_t *mu, mir_value_t left, mir_value_t right)
{
   int64_t lval, rval;
   const bool lconst = mir_get_const(mu, left, &lval);
   const bool rconst = mir_get_const(mu, right, &rval);

   mir_type_t t_bool = mir_bool_type(mu);
   if (lconst && rconst)
      return mir_const(mu, t_bool, lval ^ rval);
   else if (mir_equals(left, right))
      return mir_const(mu, t_bool, 0);

   return mir_build_logical(mu, MIR_OP_XOR, t_bool, left, right);
}

mir_value_t mir_build_not(mir_unit_t *mu, mir_value_t value)
{
   mir_type_t t_bool = mir_bool_type(mu);

   int64_t cval;
   if (mir_get_const(mu, value, &cval))
      return mir_const(mu, t_bool, !cval);

   if (mir_get_op(mu, value) == MIR_OP_NOT)
      return mir_get_arg(mu, value, 0);

   mir_value_t result = mir_build_1(mu, MIR_OP_NOT, t_bool,
                                    MIR_NULL_STAMP, value);

   MIR_ASSERT(mir_is_bool(mu, value), "argument to not is not bool");

   return result;
}

mir_value_t mir_build_cmp(mir_unit_t *mu, mir_cmp_t cmp, mir_value_t left,
                          mir_value_t right)
{
   int64_t lval, rval;
   const bool lconst = mir_get_const(mu, left, &lval);
   const bool rconst = mir_get_const(mu, right, &rval);

   mir_type_t t_bool = mir_bool_type(mu);

   if (lconst && rconst) {
      switch (cmp) {
      case MIR_CMP_EQ:  return mir_const(mu, t_bool, lval == rval);
      case MIR_CMP_NEQ: return mir_const(mu, t_bool, lval != rval);
      case MIR_CMP_LT:  return mir_const(mu, t_bool, lval < rval);
      case MIR_CMP_GT:  return mir_const(mu, t_bool, lval > rval);
      case MIR_CMP_GEQ: return mir_const(mu, t_bool, lval >= rval);
      case MIR_CMP_LEQ: return mir_const(mu, t_bool, lval <= rval);
      default: should_not_reach_here();
      }
   }

   mir_stamp_t lstamp = mir_get_stamp(mu, left);
   mir_stamp_t rstamp = mir_get_stamp(mu, right);
   mir_stamp_t stamp = mir_stamp_cmp(mu, cmp, lstamp, rstamp);

   int64_t sconst;
   if (mir_stamp_const(mu, stamp, &sconst))
      return mir_const(mu, t_bool, sconst);

   mir_value_t result = mir_build_3(mu, MIR_OP_CMP, t_bool,
                                    stamp, mir_enum(cmp), left, right);

   MIR_ASSERT(mir_same_type(mu, mir_get_type(mu, left),
                            mir_get_type(mu, right)),
              "arguments to cmp are not the same type");

   return result;
}

mir_value_t mir_build_pack(mir_unit_t *mu, mir_type_t type, mir_value_t arg)
{
   mir_value_t result = mir_build_1(mu, MIR_OP_PACK, type, MIR_NULL_STAMP, arg);

#ifdef DEBUG
   const mir_class_t class = mir_get_class(mu, type);
   MIR_ASSERT(class == MIR_TYPE_VEC2 || class == MIR_TYPE_VEC4,
              "pack type must be vector");
   MIR_ASSERT(mir_is(mu, arg, MIR_TYPE_POINTER)
              || (mir_get_size(mu, type) == 1 && mir_is_integral(mu, arg)),
              "pack argument must be pointer if size != 1");
#endif

   return result;
}

mir_value_t mir_build_unpack(mir_unit_t *mu, mir_value_t vec, uint8_t strength,
                             mir_value_t dest)
{
   mir_value_t result = dest;
   if (mir_is_null(dest))
      result = mir_build_2(mu, MIR_OP_UNPACK, mir_logic_type(mu),
                           MIR_NULL_STAMP, vec, mir_enum(strength));
   else
      mir_build_3(mu, MIR_OP_UNPACK, MIR_NULL_TYPE, MIR_NULL_STAMP,
                  vec, mir_enum(strength), dest);

   MIR_ASSERT(mir_is_vector(mu, vec), "unpack argument must be vector");
   MIR_ASSERT(mir_is_null(dest) || mir_points_to(mu, dest, MIR_TYPE_INT),
              "unpack dest must be pointer to logic or null");
   MIR_ASSERT(mir_get_size(mu, mir_get_type(mu, vec)) == 1
              || !mir_is_null(dest),
              "unpack dest must be non-null if wide vector");
   MIR_ASSERT((strength & 3) == 0, "strength lower bits must be zero");

   return result;
}

mir_value_t mir_build_binary(mir_unit_t *mu, mir_vec_op_t op, mir_type_t type,
                             mir_value_t left, mir_value_t right)
{
   mir_type_t otype;
   switch (op) {
   case MIR_VEC_CASE_EQ:
   case MIR_VEC_CASE_NEQ:
   case MIR_VEC_CASEX_EQ:
   case MIR_VEC_LOG_AND:
   case MIR_VEC_LOG_OR:
      // XXX: these should be vec2
   case MIR_VEC_LT:
   case MIR_VEC_LEQ:
   case MIR_VEC_GT:
   case MIR_VEC_GEQ:
   case MIR_VEC_LOG_EQ:
   case MIR_VEC_LOG_NEQ:
      otype = mir_vec4_type(mu, 1, false);
      break;
   default:
      otype = type;
   }

   mir_value_t result = mir_build_3(mu, MIR_OP_BINARY, otype, MIR_NULL_STAMP,
                                    mir_enum(op), left, right);

   MIR_ASSERT(mir_is_vector(mu, result), "binary operation must be vector");
   MIR_ASSERT(mir_check_type(mu, left, type), "left type does not match");
   MIR_ASSERT(mir_check_type(mu, right, type), "right type does not match");

   return result;
}

mir_value_t mir_build_unary(mir_unit_t *mu, mir_vec_op_t op, mir_type_t type,
                            mir_value_t arg)
{
   mir_type_t otype;
   switch (op) {
   case MIR_VEC_LOG_NOT:
   case MIR_VEC_BIT_AND:
   case MIR_VEC_BIT_OR:
   case MIR_VEC_BIT_XOR:
      otype = mir_vec2_type(mu, 1, false);
      break;
   default:
      otype = type;
   }

   mir_value_t result = mir_build_2(mu, MIR_OP_UNARY, otype, MIR_NULL_STAMP,
                                    mir_enum(op), arg);

   MIR_ASSERT(mir_is_vector(mu, result), "unary operation must be vector");
   MIR_ASSERT(mir_check_type(mu, arg, type), "arg type does not match");

   return result;
}

mir_value_t mir_build_insert(mir_unit_t *mu, mir_value_t part, mir_value_t full,
                             mir_value_t pos)
{
   mir_type_t type = mir_get_type(mu, full);
   mir_value_t result = mir_build_3(mu, MIR_OP_INSERT, type, MIR_NULL_STAMP,
                                    part, full, pos);

   MIR_ASSERT(mir_is_vector(mu, full), "full argument must be vector");
   MIR_ASSERT(mir_is_vector(mu, part), "part argument must be vector");

#ifdef DEBUG
   mir_type_t part_type = mir_get_type(mu, part);
   MIR_ASSERT(mir_get_size(mu, part_type) <= mir_get_size(mu, type),
              "out of bounds insert");
   MIR_ASSERT(mir_get_class(mu, part_type) == mir_get_class(mu, type),
              "mismatched vector types");
#endif

   return result;
}

mir_value_t mir_build_extract(mir_unit_t *mu, mir_type_t type, mir_value_t full,
                              mir_value_t pos)
{
   mir_value_t result = mir_build_2(mu, MIR_OP_EXTRACT, type, MIR_NULL_STAMP,
                                    full, pos);

   MIR_ASSERT(mir_is_vector(mu, full), "extract argument must be vector");
   MIR_ASSERT(mir_is_offset(mu, pos), "extract position must be offset");

#ifdef DEBUG
   mir_type_t full_type = mir_get_type(mu, full);
   MIR_ASSERT(mir_get_class(mu, full_type) == mir_get_class(mu, type),
              "mismatched vector types");
#endif

   return result;
}

mir_value_t mir_build_test(mir_unit_t *mu, mir_value_t vec)
{
   mir_value_t result = mir_build_1(mu, MIR_OP_TEST, mir_bool_type(mu),
                                    MIR_NULL_STAMP, vec);

   MIR_ASSERT(mir_is_vector(mu, vec), "argument must be vector");

   return result;
}

void mir_build_store(mir_unit_t *mu, mir_value_t dest, mir_value_t src)
{
   mir_build_2(mu, MIR_OP_STORE, MIR_NULL_TYPE, MIR_NULL_STAMP, dest, src);

#ifdef DEBUG
   mir_type_t type = mir_get_type(mu, dest);
   mir_type_t pointed = mir_get_pointer(mu, type);
#endif

   MIR_ASSERT(mir_get_class(mu, type) == MIR_TYPE_POINTER,
              "store destination is not a pointer or variable");
   MIR_ASSERT(mir_same_type(mu, pointed, mir_get_type(mu, src)),
              "source and destination have different types");
}

mir_value_t mir_build_load(mir_unit_t *mu, mir_value_t value)
{
   mir_type_t type = mir_get_type(mu, value);
   mir_stamp_t stamp = mir_stamp_elem(mu, mir_get_stamp(mu, value));
   mir_type_t pointed = mir_get_pointer(mu, type);

   mir_value_t result = mir_build_1(mu, MIR_OP_LOAD, pointed, stamp, value);

   MIR_ASSERT(!mir_is_null(type), "cannot load this value");
   MIR_ASSERT(mir_get_class(mu, type) == MIR_TYPE_POINTER,
              "argument to load is not a pointer or variable");
   MIR_ASSERT(mir_is_scalar(mu, result), "cannot load non-scalar type");
   MIR_ASSERT(mir_get_mem(mu, value) != MIR_MEM_NULL,
              "null pointer dereference");

   return result;
}

void mir_build_copy(mir_unit_t *mu, mir_value_t dest, mir_value_t src,
                    mir_value_t count)
{
   int64_t cval;
   if (!mir_is_null(count) && mir_get_const(mu, count, &cval) && cval == 0)
      return;
   else if (mir_equals(dest, src))
      return;

   mir_type_t dtype = mir_get_type(mu, dest);
   mir_type_t elem = mir_get_pointer(mu, dtype);

   if (mir_is_null(count))
      mir_build_2(mu, MIR_OP_COPY, elem, MIR_NULL_STAMP, dest, src);
   else
      mir_build_3(mu, MIR_OP_COPY, elem, MIR_NULL_STAMP, dest, src, count);

   MIR_ASSERT(mir_is(mu, dest, MIR_TYPE_POINTER),
              "destination type is not a pointer");
   MIR_ASSERT(mir_is(mu, src, MIR_TYPE_POINTER),
              "source type is not a pointer");
   MIR_ASSERT(mir_equals(mir_get_type(mu, src), dtype),
              "source and destination types do not match");
   MIR_ASSERT(mir_is_null(count) || mir_is_offset(mu, count),
              "count is not offset type");
}

void mir_build_set(mir_unit_t *mu, mir_value_t dest, mir_value_t value,
                   mir_value_t count)
{
   int64_t cval;
   if (mir_get_const(mu, count, &cval) && cval == 0)
      return;

   mir_type_t dtype = mir_get_type(mu, dest);
   mir_type_t elem = mir_get_pointer(mu, dtype);

   mir_build_3(mu, MIR_OP_SET, elem, MIR_NULL_STAMP, dest, value, count);

   MIR_ASSERT(mir_is(mu, dest, MIR_TYPE_POINTER),
              "destination type is not a pointer");
   MIR_ASSERT(mir_is_scalar(mu, value), "memset value must have scalar type");
   MIR_ASSERT(mir_is_offset(mu, count), "count is not offset type");
}

mir_value_t mir_build_alloc(mir_unit_t *mu, mir_type_t type, mir_stamp_t stamp,
                            mir_value_t count)
{
   mir_type_t ptr = mir_pointer_type(mu, type);
   mir_value_t result = mir_build_1(mu, MIR_OP_ALLOC, ptr, stamp, count);

   MIR_ASSERT(mir_get_class(mu, type) != MIR_TYPE_CARRAY,
              "alloc element type cannot be array");
   MIR_ASSERT(mir_is_offset(mu, count), "count must be offset type");

   return result;
}

mir_value_t mir_build_null(mir_unit_t *mu, mir_type_t type)
{
   mir_stamp_t stamp = mir_pointer_stamp(mu, MIR_MEM_NULL, MIR_NULL_STAMP);
   mir_value_t result = mir_build_0(mu, MIR_OP_NULL, type, stamp);

#ifdef DEBUG
   const mir_class_t class = mir_get_class(mu, type);
   MIR_ASSERT(class == MIR_TYPE_POINTER || class == MIR_TYPE_FILE
              || class == MIR_TYPE_ACCESS || class == MIR_TYPE_CONTEXT,
              "null type must be file, access, context, or pointer");
#endif

   return result;
}

mir_value_t mir_build_new(mir_unit_t *mu, mir_type_t type, mir_stamp_t stamp,
                          mir_value_t count)
{
   mir_type_t access = mir_access_type(mu, type);

   mir_value_t result;
   if (mir_is_null(count))
      result = mir_build_0(mu, MIR_OP_NEW, access, stamp);
   else
      result = mir_build_1(mu, MIR_OP_NEW, access, stamp, count);

#ifdef DEBUG
   const mir_class_t class = mir_get_class(mu, type);
   MIR_ASSERT(class == MIR_TYPE_INT || class == MIR_TYPE_RECORD
              || class == MIR_TYPE_UARRAY || class == MIR_TYPE_ACCESS
              || class == MIR_TYPE_REAL || class == MIR_TYPE_CONTEXT,
              "new type must be int, real, record, access, or uarray");
   MIR_ASSERT(mir_is_null(count) || mir_is_offset(mu, count),
              "new count must have offset type");
#endif

   return result;
}

mir_value_t mir_build_all(mir_unit_t *mu, mir_value_t access)
{
   mir_type_t type = mir_get_type(mu, access);
   mir_stamp_t stamp = mir_get_stamp(mu, access);
   mir_type_t pointed = mir_get_pointer(mu, type);
   mir_value_t result = mir_build_1(mu, MIR_OP_ALL, pointed, stamp, access);

   MIR_ASSERT(mir_get_class(mu, type) == MIR_TYPE_ACCESS,
              "argument to all must be access type");

   return result;
}

mir_value_t mir_build_address_of(mir_unit_t *mu, mir_value_t array)
{
   mir_type_t type = mir_get_type(mu, array);
   mir_type_t pointer = mir_get_pointer(mu, type);
   mir_stamp_t stamp = mir_get_stamp(mu, array);

   if (mir_is_null(pointer))
      pointer = mir_pointer_type(mu, type);

   mir_value_t result = mir_build_1(mu, MIR_OP_ADDRESS_OF,
                                    pointer, stamp, array);

   MIR_ASSERT(mir_is(mu, array, MIR_TYPE_CARRAY)
              || mir_is(mu, array, MIR_TYPE_RECORD),
              "argument to address of must be array or record");

   return result;
}

mir_value_t mir_build_array_ref(mir_unit_t *mu, mir_value_t array,
                                mir_value_t offset)
{
   mir_type_t type = mir_get_type(mu, array);
   mir_stamp_t stamp = mir_get_stamp(mu, array);

   mir_value_t result = mir_build_2(mu, MIR_OP_ARRAY_REF, type, stamp,
                                    array, offset);

   MIR_ASSERT(mir_is(mu, array, MIR_TYPE_POINTER) || mir_is_signal(mu, array),
              "argument to array ref must be pointer or signal");
   MIR_ASSERT(mir_is_offset(mu, offset),
              "offset argument to array ref must be offset");

   return result;
}

mir_value_t mir_build_table_ref(mir_unit_t *mu, mir_value_t array,
                                mir_value_t stride, const mir_value_t *args,
                                int nargs)
{
   mir_type_t type = mir_get_type(mu, array);
   mir_stamp_t stamp = mir_get_stamp(mu, array);

   node_data_t *n = mir_add_node(mu, MIR_OP_TABLE_REF, type, stamp, nargs + 2);

   mir_set_arg(mu, n, 0, array);
   mir_set_arg(mu, n, 1, stride);

   for (int i = 0; i < nargs; i++)
      mir_set_arg(mu, n, i + 2, args[i]);

   MIR_ASSERT(mir_is(mu, array, MIR_TYPE_POINTER),
              "argument to table ref must be pointer");
   MIR_ASSERT(mir_is_offset(mu, stride),
              "stride argument to table ref must be offset");

   for (int i = 0; i < nargs; i++)
      MIR_ASSERT(mir_is_integral(mu, args[i]),
                 "table ref indices must be integral");

   return (mir_value_t){ .tag = MIR_TAG_NODE, .id = mir_node_id(mu, n) };
}

mir_value_t mir_build_record_ref(mir_unit_t *mu, mir_value_t record,
                                 unsigned field)
{
   mir_type_t pointer = mir_get_type(mu, record);

   mir_mem_t mem = mir_get_mem(mu, record);
   mir_stamp_t stamp = MIR_NULL_STAMP;
   if (mem != MIR_MEM_TOP)
      stamp = mir_pointer_stamp(mu, mem, MIR_NULL_STAMP);

   const type_data_t *td = mir_type_data(mu, mir_get_elem(mu, pointer));

   mir_type_t type = MIR_NULL_TYPE;
   if (td->class == MIR_TYPE_RECORD && field < td->u.record.count)
      type = td->u.record.fields[field + td->u.record.count];

   mir_value_t result = mir_build_2(mu, MIR_OP_RECORD_REF, type, stamp,
                                    record, mir_enum(field));

   MIR_ASSERT(mir_get_class(mu, pointer) == MIR_TYPE_POINTER,
              "record ref argument must be pointer to record");
   MIR_ASSERT(td->class == MIR_TYPE_RECORD,
              "record ref argument must be pointer to record");
   MIR_ASSERT(field < td->u.record.count, "field index %d out of range", field);

   return result;
}

mir_value_t mir_build_wrap(mir_unit_t *mu, mir_value_t data,
                           const mir_dim_t *dims, int ndims)
{
   mir_type_t type = mir_get_type(mu, data);
   mir_stamp_t stamp = mir_get_stamp(mu, data);

   const type_data_t *td = mir_type_data(mu, type);
   mir_type_t elem = td->class == MIR_TYPE_POINTER ? td->u.pointer : type;

   mir_type_t uarray = mir_uarray_type(mu, ndims, elem);

   node_data_t *n = mir_add_node(mu, MIR_OP_WRAP, uarray, stamp,
                                  ndims * 3 + 1);

   mir_set_arg(mu, n, 0, data);

   for (int i = 0; i < ndims; i++) {
      mir_set_arg(mu, n, i*3 + 1, dims[i].left);
      mir_set_arg(mu, n, i*3 + 2, dims[i].right);
      mir_set_arg(mu, n, i*3 + 3, dims[i].dir);
   }

   MIR_ASSERT(td->class == MIR_TYPE_POINTER || td->class == MIR_TYPE_SIGNAL,
              "wrapped data is not pointer or signal");

   for (int i = 0; i < ndims; i++) {
      MIR_ASSERT(mir_is_integral(mu, dims[i].left),
                 "dimension %d left bound must be integral", i + 1);
      MIR_ASSERT(mir_is_integral(mu, dims[i].right),
                 "dimension %d right bound must be integral", i + 1);
      MIR_ASSERT(mir_is_bool(mu, dims[i].dir),
                 "dimension %d direction must be bool", i + 1);
   }

   return (mir_value_t){ .tag = MIR_TAG_NODE, .id = mir_node_id(mu, n) };
}

mir_value_t mir_build_unwrap(mir_unit_t *mu, mir_value_t array)
{
   if (mir_get_op(mu, array) == MIR_OP_WRAP)
      return mir_get_arg(mu, array, 0);

   mir_type_t type = mir_get_type(mu, array);
   mir_type_t pointer = mir_get_pointer(mu, type);
   mir_stamp_t stamp = mir_get_stamp(mu, array);

   mir_value_t result = mir_build_1(mu, MIR_OP_UNWRAP, pointer, stamp, array);

   MIR_ASSERT(mir_get_class(mu, type) == MIR_TYPE_UARRAY,
              "unwrap argument must be uarray");

   return result;
}

mir_value_t mir_build_uarray_len(mir_unit_t *mu, mir_value_t array, int dim)
{
   mir_type_t t_offset = mir_offset_type(mu);
   mir_value_t result = mir_build_2(mu, MIR_OP_UARRAY_LEN, t_offset,
                                    MIR_NULL_STAMP, array,
                                    mir_const(mu, t_offset, dim));

   MIR_ASSERT(mir_is(mu, array, MIR_TYPE_UARRAY),
              "uarray len argument must be uarrray");

#ifdef DEBUG
   const type_data_t *td = mir_type_data(mu, mir_get_type(mu, array));
   MIR_ASSERT(dim >= 0 && dim < td->u.uarray.dims, "invalid dimension %d", dim);
#endif

   return result;
}

static mir_value_t mir_build_uarray_op(mir_unit_t *mu, mir_op_t op,
                                       mir_type_t type, int arg_index,
                                       mir_value_t array, int dim)
{
   if (mir_get_op(mu, array) == MIR_OP_WRAP)
      return mir_get_arg(mu, array, 1 + (dim * 3) + arg_index);

   mir_value_t result = mir_build_2(mu, op, type, MIR_NULL_STAMP,
                                    array, mir_enum(dim));

   MIR_ASSERT(mir_is(mu, array, MIR_TYPE_UARRAY),
              "cannot use %s with non-uarray type", mir_op_string(op));

#ifdef DEBUG
   const type_data_t *td = mir_type_data(mu, mir_get_type(mu, array));
   MIR_ASSERT(dim >= 0 && dim < td->u.uarray.dims, "invalid dimension %d", dim);
#endif

   return result;
}

mir_value_t mir_build_uarray_left(mir_unit_t *mu, mir_value_t array, int dim)
{
   return mir_build_uarray_op(mu, MIR_OP_UARRAY_LEFT, mir_offset_type(mu),
                              0, array, dim);
}

mir_value_t mir_build_uarray_right(mir_unit_t *mu, mir_value_t array, int dim)
{
   return mir_build_uarray_op(mu, MIR_OP_UARRAY_RIGHT, mir_offset_type(mu),
                              1, array, dim);
}

mir_value_t mir_build_uarray_dir(mir_unit_t *mu, mir_value_t array, int dim)
{
   return mir_build_uarray_op(mu, MIR_OP_UARRAY_DIR, mir_bool_type(mu),
                              2, array, dim);
}

mir_value_t mir_build_range_length(mir_unit_t *mu, mir_value_t left,
                                   mir_value_t right, mir_value_t dir)
{
   mir_value_t result = mir_build_3(mu, MIR_OP_RANGE_LENGTH,
                                    mir_offset_type(mu), MIR_NULL_STAMP,
                                    left, right, dir);

   MIR_ASSERT(mir_same_type(mu, mir_get_type(mu, left),
                            mir_get_type(mu, right)),
              "left and right are not the same type");
   MIR_ASSERT(mir_is_integral(mu, left), "left/right type is not integeral");
   MIR_ASSERT(mir_is_bool(mu, dir), "dir type is not bool");

   return result;
}

mir_value_t mir_build_range_null(mir_unit_t *mu, mir_value_t left,
                                 mir_value_t right, mir_value_t dir)
{
   mir_value_t result = mir_build_3(mu, MIR_OP_RANGE_NULL,
                                    mir_bool_type(mu), MIR_NULL_STAMP,
                                    left, right, dir);

   MIR_ASSERT(mir_same_type(mu, mir_get_type(mu, left),
                            mir_get_type(mu, right)),
              "left and right are not the same type");
   MIR_ASSERT(mir_is_integral(mu, left), "left/right type is not integeral");
   MIR_ASSERT(mir_is_bool(mu, dir), "dir type is not bool");

   return result;
}

void mir_build_jump(mir_unit_t *mu, mir_block_t target)
{
   mir_build_1(mu, MIR_OP_JUMP, MIR_NULL_TYPE, MIR_NULL_STAMP,
               mir_cast_value(target));

   MIR_ASSERT(!mir_is_null(target), "invalid jump target");
}

void mir_build_cond(mir_unit_t *mu, mir_value_t test, mir_block_t btrue,
                    mir_block_t bfalse)
{
   int64_t tconst;
   if (mir_get_const(mu, test, &tconst)) {
      mir_build_jump(mu, !!tconst ? btrue : bfalse);
      return;
   }

   mir_build_3(mu, MIR_OP_COND, MIR_NULL_TYPE, MIR_NULL_STAMP,
               test, mir_cast_value(btrue), mir_cast_value(bfalse));

   MIR_ASSERT(mir_is_bool(mu, test), "cond test is not a bool");
   MIR_ASSERT(!mir_is_null(btrue) && !mir_is_null(bfalse),
              "invalid cond targets");
}

mir_value_t mir_build_select(mir_unit_t *mu, mir_type_t type, mir_value_t test,
                             mir_value_t vtrue, mir_value_t vfalse)
{
   int64_t tconst;
   if (mir_get_const(mu, test, &tconst))
      return tconst ? vtrue : vfalse;
   else if (mir_equals(vtrue, vfalse))
      return vtrue;

   mir_stamp_t s_true = mir_get_stamp(mu, vtrue);
   mir_stamp_t s_false = mir_get_stamp(mu, vfalse);
   mir_stamp_t stamp = mir_stamp_union(mu, s_true, s_false);

   mir_value_t result = mir_build_3(mu, MIR_OP_SELECT, type, stamp, test,
                                    vtrue, vfalse);

   MIR_ASSERT(mir_is_bool(mu, test), "select test is not a bool");
   MIR_ASSERT(mir_check_type(mu, vtrue, type),
              "true argument to select is not expected type");
   MIR_ASSERT(mir_check_type(mu, vfalse, type),
              "false argument to select is not expected type");

   return result;
}

mir_value_t mir_build_phi(mir_unit_t *mu, mir_type_t type, unsigned ninputs)
{
   node_data_t *n = mir_add_node(mu, MIR_OP_PHI, type, MIR_NULL_STAMP,
                                 ninputs * 2);

   for (int i = 0; i < ninputs; i++)
      mir_set_arg(mu, n, i, MIR_NULL_VALUE);

   return (mir_value_t){ .tag = MIR_TAG_NODE, .id = mir_node_id(mu, n) };
}

void mir_set_input(mir_unit_t *mu, mir_value_t phi, unsigned nth,
                   mir_block_t block, mir_value_t value)
{
   node_data_t *n = mir_node_data(mu, phi);
   assert(n->op == MIR_OP_PHI);
   assert(nth * 2 < n->nargs);

   mir_set_arg(mu, n, nth * 2,  mir_cast_value(block));
   mir_set_arg(mu, n, nth * 2 + 1, value);

   n->stamp = mir_stamp_union(mu, n->stamp, mir_get_stamp(mu, value));

   MIR_ASSERT(mir_equals(n->type, mir_get_type(mu, value)),
              "phi input %d has wrong type", nth);

#ifdef DEBUG
   if (value.tag != MIR_TAG_CONST) {
      const block_data_t *bd = mir_block_data(mu, block);
      for (int i = 0; i < bd->num_nodes; i++) {
         if (bd->nodes[i] == value.id)
         return;
      }

      MIR_ASSERT(false, "phi input %d not in block %d", nth, block.id);
   }
#endif
}

void mir_build_case(mir_unit_t *mu, mir_value_t value, mir_block_t def,
                    const mir_value_t *cases, const mir_block_t *blocks,
                    int ncases)
{
   int64_t cval;
   if (mir_get_const(mu, value, &cval)) {
      for (int i = 0; i < ncases; i++) {
         int64_t cmp;
         if (mir_get_const(mu, cases[i], &cmp) && cmp == cval) {
            mir_build_jump(mu, blocks[i]);
            return;
         }
      }

      mir_build_jump(mu, def);
      return;
   }

   node_data_t *n = mir_add_node(mu, MIR_OP_CASE, MIR_NULL_TYPE,
                                 MIR_NULL_STAMP, (ncases + 1) * 2);

   mir_set_arg(mu, n, 0, value);
   mir_set_arg(mu, n, 1, mir_cast_value(def));

   for (int i = 0; i < ncases; i++) {
      mir_set_arg(mu, n, (i + 1) * 2, cases[i]);
      mir_set_arg(mu, n, (i + 1) * 2 + 1, mir_cast_value(blocks[i]));
   }

   MIR_ASSERT(mir_is_integral(mu, value), "case choice must be integral");

#ifdef DEBUG
   mir_type_t type = mir_get_type(mu, value);
   for (int i = 0; i < ncases; i++) {
      MIR_ASSERT(mir_is_const(mu, cases[i]), "case choice is not constant");
      MIR_ASSERT(mir_same_type(mu, mir_get_type(mu, cases[i]), type),
                 "choice and value types do not match");

      for (int j = 0; j < i; j++)
         MIR_ASSERT(!mir_equals(cases[i], cases[j]), "duplicate case choice");
   }
#endif
}

void mir_build_return(mir_unit_t *mu, mir_value_t value)
{
   if (mir_is_null(value))
      mir_add_node(mu, MIR_OP_RETURN, MIR_NULL_TYPE, MIR_NULL_STAMP, 0);
   else
      mir_build_1(mu, MIR_OP_RETURN, MIR_NULL_TYPE, MIR_NULL_STAMP, value);

   MIR_ASSERT(mir_is_null(mu->result) || mir_check_type(mu, value, mu->result),
              "wrong result type");
   MIR_ASSERT(!mir_is_null(mu->result) || mu->kind == MIR_UNIT_PROPERTY
              || mir_is_null(value), "cannot return a result");
}

void mir_build_wait(mir_unit_t *mu, mir_block_t target)
{
   mir_build_1(mu, MIR_OP_WAIT, MIR_NULL_TYPE, MIR_NULL_STAMP,
               mir_cast_value(target));

   MIR_ASSERT(mu->kind == MIR_UNIT_PROCEDURE || mu->kind == MIR_UNIT_PROCESS,
              "wait only allowed in process or procedure");
}

void mir_build_consume(mir_unit_t *mu, mir_value_t value)
{
   if (value.tag != MIR_TAG_NODE)
      return;   // Only useful for keeping nodes alive

   mir_build_1(mu, MIR_OP_CONSUME, MIR_NULL_TYPE, MIR_NULL_STAMP, value);
}

mir_value_t mir_build_fcall(mir_unit_t *mu, ident_t name, mir_type_t type,
                            mir_stamp_t stamp, const mir_value_t *args,
                            unsigned nargs)
{
   node_data_t *n = mir_add_node(mu, MIR_OP_FCALL, type, stamp, nargs + 1);

   mir_set_arg(mu, n, 0, mir_add_linkage(mu, name));

   for (int i = 0; i < nargs; i++)
      mir_set_arg(mu, n, i + 1, args[i]);

   if (mir_is_null(type))
      return MIR_NULL_VALUE;
   else
      return (mir_value_t){ .tag = MIR_TAG_NODE, .id = mir_node_id(mu, n) };
}

void mir_build_pcall(mir_unit_t *mu, ident_t name, mir_block_t resume,
                     const mir_value_t *args, unsigned nargs)
{
   node_data_t *n = mir_add_node(mu, MIR_OP_PCALL, MIR_NULL_TYPE,
                                 MIR_NULL_STAMP, nargs + 2);
   mir_set_arg(mu, n, 0, mir_cast_value(resume));
   mir_set_arg(mu, n, 1, mir_add_linkage(mu, name));

   for (int i = 0; i < nargs; i++)
      mir_set_arg(mu, n, i + 2, args[i]);

   MIR_ASSERT(nargs > 0 && mir_is(mu, args[0], MIR_TYPE_CONTEXT),
              "first argument to VHDL procedure must be context pointer");
}

void mir_build_resume(mir_unit_t *mu, ident_t name)
{
   mir_value_t link = mir_add_linkage(mu, name);
   mir_build_1(mu, MIR_OP_RESUME, MIR_NULL_TYPE, MIR_NULL_STAMP, link);

   MIR_ASSERT(mir_count_nodes(mu, mu->cursor.block) == 1,
              "resume must be first op in a block");
}

mir_value_t mir_build_syscall(mir_unit_t *mu, ident_t func, mir_type_t type,
                              mir_stamp_t stamp, mir_value_t locus,
                              const mir_value_t *args, int nargs)
{
   node_data_t *n = mir_add_node(mu, MIR_OP_SYSCALL, type, stamp, nargs + 2);

   mir_set_arg(mu, n, 0, mir_add_linkage(mu, func));
   mir_set_arg(mu, n, 1, locus);

   for (int i = 0; i < nargs; i++)
      mir_set_arg(mu, n, i + 2, args[i]);

   MIR_ASSERT(mir_is(mu, locus, MIR_TYPE_LOCUS),
              "locus argument to syscall must be a debug locus");

   for (int i = 0; i < nargs; i++)
      MIR_ASSERT(!mir_is_null(args[i]), "invalid argument to syscall");

   if (mir_is_null(type))
      return MIR_NULL_VALUE;
   else
      return (mir_value_t){ .tag = MIR_TAG_NODE, .id = mir_node_id(mu, n) };
}

void mir_build_unreachable(mir_unit_t *mu, mir_value_t locus)
{
   if (mir_is_null(locus))
      mir_build_0(mu, MIR_OP_UNREACHABLE, MIR_NULL_TYPE, MIR_NULL_STAMP);
   else
      mir_build_1(mu, MIR_OP_UNREACHABLE, MIR_NULL_TYPE, MIR_NULL_STAMP, locus);

   MIR_ASSERT(mir_is_null(locus) || mir_is(mu, locus, MIR_TYPE_LOCUS),
              "locus argument to unreachable must be debug locus");
}

static void mir_build_bounds_op(mir_unit_t *mu, mir_op_t op, mir_value_t value,
                                mir_value_t left, mir_value_t right,
                                mir_value_t dir, mir_value_t locus,
                                mir_value_t hint)
{
   // TODO: check if can elide bounds

   mir_build_6(mu, op, MIR_NULL_TYPE, MIR_NULL_STAMP, value, left, right,
               dir, locus, hint);

   MIR_ASSERT(mir_is_numeric(mu, value), "value must be numeric");
   MIR_ASSERT(mir_is_numeric(mu, left), "left bound must be numeric");
   MIR_ASSERT(mir_is_numeric(mu, right), "right bound must be numeric");
   MIR_ASSERT(mir_check_type(mu, dir, mir_bool_type(mu)),
              "direction must be a bool");
   MIR_ASSERT(mir_is(mu, locus, MIR_TYPE_LOCUS), "locus must be a debug locus");
}

void mir_build_range_check(mir_unit_t *mu, mir_value_t value, mir_value_t left,
                           mir_value_t right, mir_value_t dir,
                           mir_value_t locus, mir_value_t hint)
{
   mir_build_bounds_op(mu, MIR_OP_RANGE_CHECK, value, left, right, dir,
                       locus, hint);
}

void mir_build_index_check(mir_unit_t *mu, mir_value_t value, mir_value_t left,
                           mir_value_t right, mir_value_t dir,
                           mir_value_t locus, mir_value_t hint)
{
   mir_build_bounds_op(mu, MIR_OP_INDEX_CHECK, value, left, right, dir,
                       locus, hint);
}

void mir_build_dir_check(mir_unit_t *mu, mir_value_t value, mir_value_t dir,
                         mir_value_t locus)
{
   if (mir_equals(value, dir)) {
      mir_comment(mu, "Elided direction check");
      return;
   }

   mir_build_3(mu, MIR_OP_DIR_CHECK, MIR_NULL_TYPE, MIR_NULL_STAMP,
               value, dir, locus);

   MIR_ASSERT(mir_check_type(mu, value, mir_bool_type(mu)),
              "null check argument must be a bool");
   MIR_ASSERT(mir_check_type(mu, dir, mir_bool_type(mu)),
              "null check direction must be a bool");
   MIR_ASSERT(mir_is(mu, locus, MIR_TYPE_LOCUS),
              "locus argument to null check must be a debug locus");
}

void mir_build_null_check(mir_unit_t *mu, mir_value_t ptr, mir_value_t locus)
{
   mir_build_2(mu, MIR_OP_NULL_CHECK, MIR_NULL_TYPE, MIR_NULL_STAMP,
               ptr, locus);

   MIR_ASSERT(mir_is(mu, ptr, MIR_TYPE_ACCESS),
              "null check argument must be an access");
   MIR_ASSERT(mir_is(mu, locus, MIR_TYPE_LOCUS),
              "locus argument to null check must be a debug locus");
}

void mir_build_zero_check(mir_unit_t *mu, mir_value_t value, mir_value_t locus)
{
   int64_t cval;
   if (mir_get_const(mu, value, &cval) && cval != 0)
      return;

   mir_build_2(mu, MIR_OP_ZERO_CHECK, MIR_NULL_TYPE, MIR_NULL_STAMP,
               value, locus);

   MIR_ASSERT(mir_is_integral(mu, value),
              "argument to zero check must be integral");
   MIR_ASSERT(mir_is(mu, locus, MIR_TYPE_LOCUS),
              "locus argument to zero check must be a debug locus");
}

void mir_build_length_check(mir_unit_t *mu, mir_value_t llen, mir_value_t rlen,
                            mir_value_t locus, mir_value_t dim)
{
   if (mir_equals(llen, rlen))
      return;

   if (mir_is_null(dim))
      mir_build_3(mu, MIR_OP_LENGTH_CHECK, MIR_NULL_TYPE, MIR_NULL_STAMP,
                  llen, rlen, locus);
   else
      mir_build_4(mu, MIR_OP_LENGTH_CHECK, MIR_NULL_TYPE, MIR_NULL_STAMP,
                  llen, rlen, locus, dim);

   MIR_ASSERT(mir_is(mu, locus, MIR_TYPE_LOCUS),
              "locus argument to length check must be a debug locus");
}

void mir_build_exponent_check(mir_unit_t *mu, mir_value_t exp,
                              mir_value_t locus)
{
   int64_t cval;
   if (mir_get_const(mu, exp, &cval) && cval >= 0)
      return;

   mir_build_2(mu, MIR_OP_EXPONENT_CHECK, MIR_NULL_TYPE, MIR_NULL_STAMP,
               exp, locus);

   MIR_ASSERT(mir_is_integral(mu, exp),
              "exp argument to exponent check must be a integer");
   MIR_ASSERT(mir_is(mu, locus, MIR_TYPE_LOCUS),
              "locus argument to exponent check must be a debug locus");
}

void mir_build_file_open(mir_unit_t *mu, mir_value_t file, mir_value_t name,
                         mir_value_t length, mir_value_t kind,
                         mir_value_t status)
{
   if (mir_is_null(status))
      mir_build_4(mu, MIR_OP_FILE_OPEN, MIR_NULL_TYPE, MIR_NULL_STAMP,
                  file, name, length, kind);
   else
      mir_build_5(mu, MIR_OP_FILE_OPEN, MIR_NULL_TYPE, MIR_NULL_STAMP,
                  file, name, length, kind, status);

   MIR_ASSERT(mir_is(mu, file, MIR_TYPE_POINTER),
              "file open first argument must be pointer to file");
}

void mir_build_file_read(mir_unit_t *mu, mir_value_t file, mir_value_t ptr,
                         mir_value_t inlen, mir_value_t outlen)
{
   if (mir_is_null(inlen))
      mir_build_2(mu, MIR_OP_FILE_READ, MIR_NULL_TYPE, MIR_NULL_STAMP,
                  file, ptr);
   else if (mir_is_null(outlen))
      mir_build_3(mu, MIR_OP_FILE_READ, MIR_NULL_TYPE, MIR_NULL_STAMP,
                  file, ptr, inlen);
   else
      mir_build_4(mu, MIR_OP_FILE_READ, MIR_NULL_TYPE, MIR_NULL_STAMP,
                  file, ptr, inlen, outlen);

   MIR_ASSERT(mir_points_to(mu, file, MIR_TYPE_FILE),
              "file read first argument must have file pointer type");
   MIR_ASSERT(mir_is(mu, file, MIR_TYPE_POINTER),
              "file read pointer argument must have pointer type");
   MIR_ASSERT(mir_is_null(inlen) || mir_is_integral(mu, inlen),
              "file read inlen argument must be integral");
   MIR_ASSERT(mir_is_null(outlen) || mir_is(mu, outlen, MIR_TYPE_POINTER),
              "file read outlen argument must have pointer type");
}

void mir_build_file_write(mir_unit_t *mu, mir_value_t file, mir_value_t value,
                          mir_value_t length)
{
   if (mir_is_null(length))
      mir_build_2(mu, MIR_OP_FILE_WRITE, MIR_NULL_TYPE, MIR_NULL_STAMP,
                  file, value);
   else
      mir_build_3(mu, MIR_OP_FILE_WRITE, MIR_NULL_TYPE, MIR_NULL_STAMP,
                  file, value, length);

   MIR_ASSERT(mir_points_to(mu, file, MIR_TYPE_FILE),
              "file write first argument must have file pointer type");
}

mir_value_t mir_build_port_conversion(mir_unit_t *mu, mir_value_t driving,
                                      mir_value_t effective)
{
   mir_type_t type = mir_conversion_type(mu);

   mir_value_t result;
   if (mir_is_null(effective) || mir_equals(effective, driving))
      result = mir_build_1(mu, MIR_OP_PORT_CONVERSION, type, MIR_NULL_STAMP,
                           driving);
   else
      result = mir_build_2(mu, MIR_OP_PORT_CONVERSION, type, MIR_NULL_STAMP,
                           driving, effective);

   MIR_ASSERT(mir_is(mu, driving, MIR_TYPE_CLOSURE),
              "port conversion argument must be a closure");
   MIR_ASSERT(mir_is_null(effective) || mir_is(mu, effective, MIR_TYPE_CLOSURE),
              "port conversion argument must be a closure");

   return result;
}

void mir_build_convert_in(mir_unit_t *mu, mir_value_t conv, mir_value_t nets,
                          mir_value_t count)
{
   mir_build_3(mu, MIR_OP_CONVERT_IN, MIR_NULL_TYPE, MIR_NULL_STAMP,
               conv, nets, count);

   MIR_ASSERT(mir_is(mu, conv, MIR_TYPE_CONVERSION),
              "conv argument to convert must be a port conversion");
   MIR_ASSERT(mir_is_signal(mu, nets),
              "nets argument to convert must be a signal");
   MIR_ASSERT(mir_is_offset(mu, count),
              "count argument to convert must be offset");
}

void mir_build_convert_out(mir_unit_t *mu, mir_value_t conv, mir_value_t nets,
                           mir_value_t count)
{
   mir_build_3(mu, MIR_OP_CONVERT_OUT, MIR_NULL_TYPE, MIR_NULL_STAMP,
               conv, nets, count);

   MIR_ASSERT(mir_is(mu, conv, MIR_TYPE_CONVERSION),
              "conv argument to convert must be a port conversion");
   MIR_ASSERT(mir_is_signal(mu, nets),
              "nets argument to convert must be a signal");
   MIR_ASSERT(mir_is_offset(mu, count),
              "count argument to convert must be offset");
}

void mir_build_put_conversion(mir_unit_t *mu, mir_value_t cf,
                              mir_value_t target, mir_value_t count,
                              mir_value_t values)
{
   mir_build_4(mu, MIR_OP_PUT_CONVERSION, MIR_NULL_TYPE, MIR_NULL_STAMP,
               cf, target, count, values);

   MIR_ASSERT(mir_is_signal(mu, target),
              "put conversion target is not signal");
   MIR_ASSERT(mir_is_offset(mu, count),
              "put conversion net count is not offset type");
   MIR_ASSERT(!mir_is_signal(mu, values),
              "signal cannot be values argument for put conversion");
   MIR_ASSERT(mir_is(mu, cf, MIR_TYPE_CONVERSION),
              "cf argument to put conversion must be conversion function");
}

mir_value_t mir_build_init_signal(mir_unit_t *mu, mir_type_t type,
                                  mir_value_t count, mir_value_t size,
                                  mir_value_t value, mir_value_t flags,
                                  mir_value_t locus, mir_value_t offset)
{
   mir_type_t stype = mir_signal_type(mu, type);

   mir_value_t result;
   if (mir_is_null(offset))
      result = mir_build_5(mu, MIR_OP_INIT_SIGNAL, stype, MIR_NULL_STAMP,
                           count, size, value, flags, locus);
   else
      result = mir_build_6(mu, MIR_OP_INIT_SIGNAL, stype, MIR_NULL_STAMP,
                           count, size, value, flags, locus, offset);

   MIR_ASSERT(mir_get_class(mu, type) == MIR_TYPE_INT
              || mir_get_class(mu, type) == MIR_TYPE_REAL,
              "signal must have integer or real type");
   MIR_ASSERT(mir_is_offset(mu, count), "count argument must be offset");
   MIR_ASSERT(mir_is_offset(mu, size), "size argument must be offset");
   MIR_ASSERT(mir_is_offset(mu, flags), "flags argument must be offset");
   MIR_ASSERT(mir_is(mu, locus, MIR_TYPE_LOCUS),
              "locus argument to init signal is not debug locus");
   MIR_ASSERT(mir_is_null(offset) || mir_is(mu, offset, MIR_TYPE_POINTER),
              "offset argument must be pointer or null");

   return result;
}

mir_value_t mir_build_implicit_signal(mir_unit_t *mu, mir_type_t type,
                                      mir_value_t count, mir_value_t size,
                                      mir_value_t locus, mir_value_t kind,
                                      mir_value_t closure, mir_value_t delay)
{
   mir_type_t stype = mir_signal_type(mu, type);

   mir_value_t result = mir_build_6(mu, MIR_OP_IMPLICIT_SIGNAL, stype,
                                    MIR_NULL_STAMP, count, size, locus,
                                    kind, closure, delay);

   MIR_ASSERT(mir_is_offset(mu, count),
              "count argument to implicit signal is not offset");
   MIR_ASSERT(mir_is_offset(mu, kind),  // XXX: should be enum
              "kind argument to implicit signal is not offset");
   MIR_ASSERT(mir_is(mu, closure, MIR_TYPE_CLOSURE),
              "closure argument to implicit signal is not a closure");
   MIR_ASSERT(mir_is(mu, locus, MIR_TYPE_LOCUS),
              "locus argument to implicit signal must be a debug locus");
   MIR_ASSERT(mir_is_integral(mu, delay),
              "delay argument to implicit signal must be time");

   return result;
}

void mir_build_drive_signal(mir_unit_t *mu, mir_value_t target,
                            mir_value_t count)
{
   mir_build_2(mu, MIR_OP_DRIVE_SIGNAL, MIR_NULL_TYPE, MIR_NULL_STAMP,
               target, count);

   MIR_ASSERT(mir_is_signal(mu, target), "target must be signal");
   MIR_ASSERT(mir_is_offset(mu, count), "count argument must be offset");
}

void mir_build_sched_waveform(mir_unit_t *mu, mir_value_t target,
                              mir_value_t count, mir_value_t values,
                              mir_value_t reject, mir_value_t after)
{
   int64_t nconst;
   if (mir_get_const(mu, count, &nconst) && nconst == 0) {
      mir_comment(mu, "Skip empty waveform");
      return;
   }

   mir_build_5(mu, MIR_OP_SCHED_WAVEFORM, MIR_NULL_TYPE, MIR_NULL_STAMP,
               target, count, values, reject, after);

   MIR_ASSERT(mir_is_signal(mu, target), "target is not a signal");
   MIR_ASSERT(mir_is_offset(mu, count), "count is not offset type");
   MIR_ASSERT(!mir_is_signal(mu, values), "values cannot be signal");
}

void mir_build_put_driver(mir_unit_t *mu, mir_value_t target,
                          mir_value_t count, mir_value_t values)
{
   mir_build_3(mu, MIR_OP_PUT_DRIVER, MIR_NULL_TYPE, MIR_NULL_STAMP,
               target, count, values);

   MIR_ASSERT(mir_is_signal(mu, target), "target is not a signal");
   MIR_ASSERT(mir_is_offset(mu, count), "count is not offset type");
   MIR_ASSERT(!mir_is_signal(mu, values), "values cannot be signal");
}

void mir_build_deposit_signal(mir_unit_t *mu, mir_value_t target,
                              mir_value_t count, mir_value_t values)
{
   mir_build_3(mu, MIR_OP_DEPOSIT_SIGNAL, MIR_NULL_TYPE, MIR_NULL_STAMP,
               target, count, values);

   MIR_ASSERT(mir_is_signal(mu, target),
              "deposit signal target is not signal");
   MIR_ASSERT(mir_is_offset(mu, count),
              "deposit signal count is not offset type");
   MIR_ASSERT(!mir_is_signal(mu, values),
              "signal cannot be values argument for deposit signal");
}

void mir_build_sched_deposit(mir_unit_t *mu, mir_value_t target,
                             mir_value_t count, mir_value_t values,
                             mir_value_t after)
{
   mir_build_4(mu, MIR_OP_SCHED_DEPOSIT, MIR_NULL_TYPE, MIR_NULL_STAMP,
               target, count, values, after);

   MIR_ASSERT(mir_is_signal(mu, target),
              "sched deposit target is not signal");
   MIR_ASSERT(mir_is_offset(mu, count),
              "sched deposit count is not offset type");
   MIR_ASSERT(!mir_is_signal(mu, values),
              "signal cannot be values argument for sched deposit");
}

mir_value_t mir_build_resolved(mir_unit_t *mu, mir_value_t signal)
{
   mir_type_t type = mir_get_type(mu, signal);
   mir_type_t pointer = mir_get_pointer(mu, type);
   mir_stamp_t stamp = mir_get_stamp(mu, signal);

   mir_value_t result = mir_build_1(mu, MIR_OP_RESOLVED, pointer,
                                    stamp, signal);

   MIR_ASSERT(mir_get_class(mu, type) == MIR_TYPE_SIGNAL,
              "argument to resolved must be signal");

   return result;
}

mir_value_t mir_build_last_value(mir_unit_t *mu, mir_value_t signal)
{
   mir_type_t type = mir_get_type(mu, signal);
   mir_type_t pointer = mir_get_pointer(mu, type);
   mir_stamp_t stamp = mir_get_stamp(mu, signal);

   mir_value_t result = mir_build_1(mu, MIR_OP_LAST_VALUE, pointer,
                                    stamp, signal);

   MIR_ASSERT(mir_get_class(mu, type) == MIR_TYPE_SIGNAL,
              "argument to resolved must be signal");

   return result;
}

mir_value_t mir_build_driving_value(mir_unit_t *mu, mir_value_t signal,
                                    mir_value_t count)
{
   mir_type_t type = mir_get_pointer(mu, mir_get_type(mu, signal));
   mir_stamp_t stamp = mir_get_stamp(mu, signal);

   mir_value_t result;
   if (mir_is_null(count))
      result = mir_build_1(mu, MIR_OP_DRIVING_VALUE, type, stamp, signal);
   else
      result = mir_build_2(mu, MIR_OP_DRIVING_VALUE, type, stamp,
                           signal, count);

   MIR_ASSERT(mir_is_signal(mu, signal), "argument must have signal type");
   MIR_ASSERT(mir_is_null(count) || mir_is_offset(mu, count),
              "count argument must have offset type");

   return result;
}

void mir_build_force(mir_unit_t *mu, mir_value_t target, mir_value_t count,
                     mir_value_t values)
{
   mir_build_3(mu, MIR_OP_FORCE, MIR_NULL_TYPE, MIR_NULL_STAMP,
               target, count, values);

   MIR_ASSERT(mir_is_signal(mu, target), "force target is not signal");
   MIR_ASSERT(mir_is_offset(mu, count), "force count is not offset type");
}

void mir_build_release(mir_unit_t *mu, mir_value_t target, mir_value_t count)
{
   mir_build_2(mu, MIR_OP_RELEASE, MIR_NULL_TYPE, MIR_NULL_STAMP,
               target, count);

   MIR_ASSERT(mir_is_signal(mu, target), "release target is not signal");
   MIR_ASSERT(mir_is_offset(mu, count), "release net count is not offset type");
}

void mir_build_disconnect(mir_unit_t *mu, mir_value_t target, mir_value_t count,
                          mir_value_t reject, mir_value_t after)
{
   mir_build_4(mu, MIR_OP_DISCONNECT, MIR_NULL_TYPE, MIR_NULL_STAMP,
               target, count, reject, after);

   MIR_ASSERT(mir_is_signal(mu, target), "disconnect target is not signal");
   MIR_ASSERT(mir_is_offset(mu, count), "disconnect count is not offset type");
}


mir_value_t mir_build_last_event(mir_unit_t *mu, mir_value_t signal,
                                 mir_value_t count)
{
   mir_type_t type = mir_time_type(mu);

   mir_value_t result;
   if (mir_is_null(count))
      result = mir_build_1(mu, MIR_OP_LAST_EVENT, type, MIR_NULL_STAMP, signal);
   else
      result = mir_build_2(mu, MIR_OP_LAST_EVENT, type, MIR_NULL_STAMP,
                           signal, count);

   MIR_ASSERT(mir_is_signal(mu, signal),
              "signal argument to last event must have signal type");
   MIR_ASSERT(mir_is_null(count) || mir_is_offset(mu, count),
              "length argument to last event must have offset type");

   return result;
}

mir_value_t mir_build_last_active(mir_unit_t *mu, mir_value_t signal,
                                  mir_value_t count)
{
   mir_type_t type = mir_time_type(mu);

   mir_value_t result;
   if (mir_is_null(count))
      result = mir_build_1(mu, MIR_OP_LAST_ACTIVE, type, MIR_NULL_STAMP,
                           signal);
   else
      result = mir_build_2(mu, MIR_OP_LAST_ACTIVE, type, MIR_NULL_STAMP,
                           signal, count);

   MIR_ASSERT(mir_is_signal(mu, signal),
              "signal argument to last event must have signal type");
   MIR_ASSERT(mir_is_null(count) || mir_is_offset(mu, count),
              "length argument to last event must have offset type");

   return result;
}


static mir_value_t mir_build_signal_flag(mir_unit_t *mu, mir_op_t op,
                                         mir_value_t signal, mir_value_t count)
{
   mir_type_t t_bool = mir_bool_type(mu);
   mir_value_t result = mir_build_2(mu, op, t_bool, MIR_NULL_STAMP,
                                    signal, count);

   MIR_ASSERT(mir_is_signal(mu, signal), "signal argument must be signal");
   MIR_ASSERT(mir_is_offset(mu, count), "count argument must be offset");

   return result;
}

mir_value_t mir_build_event_flag(mir_unit_t *mu, mir_value_t signal,
                                 mir_value_t count)
{
   return mir_build_signal_flag(mu, MIR_OP_EVENT, signal, count);
}

mir_value_t mir_build_active_flag(mir_unit_t *mu, mir_value_t signal,
                                  mir_value_t count)
{
   return mir_build_signal_flag(mu, MIR_OP_ACTIVE, signal, count);
}

mir_value_t mir_build_driving_flag(mir_unit_t *mu, mir_value_t signal,
                                   mir_value_t count)
{
   return mir_build_signal_flag(mu, MIR_OP_DRIVING, signal, count);
}

void mir_build_resolve_signal(mir_unit_t *mu, mir_value_t signal,
                              mir_value_t resolution)
{
   mir_build_2(mu, MIR_OP_RESOLVE_SIGNAL, MIR_NULL_TYPE, MIR_NULL_STAMP,
               signal, resolution);

   MIR_ASSERT(mir_is_signal(mu, signal), "signal argument has wrong type");
   MIR_ASSERT(mir_points_to(mu, resolution, MIR_TYPE_RESOLUTION),
              "resolution wrapper argument has wrong type");
}

void mir_build_transfer_signal(mir_unit_t *mu, mir_value_t target,
                               mir_value_t source, mir_value_t count,
                               mir_value_t reject, mir_value_t after)
{
   mir_build_5(mu, MIR_OP_TRANSFER_SIGNAL, MIR_NULL_TYPE, MIR_NULL_STAMP,
               target, source, count, reject, after);

   MIR_ASSERT(mir_is_signal(mu, target), "target is not a signal");
   MIR_ASSERT(mir_is_offset(mu, count), "count argument must be offset");
   MIR_ASSERT(mir_is_signal(mu, source), "source is not a signal");
}

void mir_build_cover_stmt(mir_unit_t *mu, uint32_t tag)
{
   mir_build_1(mu, MIR_OP_COVER_STMT, MIR_NULL_TYPE, MIR_NULL_STAMP,
               mir_enum(tag));
}

void mir_build_cover_branch(mir_unit_t *mu, uint32_t tag)
{
   mir_build_1(mu, MIR_OP_COVER_BRANCH, MIR_NULL_TYPE, MIR_NULL_STAMP,
               mir_enum(tag));
}

void mir_build_cover_expr(mir_unit_t *mu, uint32_t tag)
{
   mir_build_1(mu, MIR_OP_COVER_EXPR, MIR_NULL_TYPE, MIR_NULL_STAMP,
               mir_enum(tag));
}

void mir_build_cover_toggle(mir_unit_t *mu, mir_value_t signal, uint32_t tag)
{
   mir_build_2(mu, MIR_OP_COVER_TOGGLE, MIR_NULL_TYPE, MIR_NULL_STAMP,
               signal, mir_enum(tag));

   MIR_ASSERT(mir_is_signal(mu, signal),
              "argument to cover toggle must be signal");
}

void mir_build_cover_state(mir_unit_t *mu, mir_value_t signal, mir_value_t low,
                           uint32_t tag)
{
   mir_build_3(mu, MIR_OP_COVER_STATE, MIR_NULL_TYPE, MIR_NULL_STAMP,
               signal, low, mir_enum(tag));

   MIR_ASSERT(mir_is_signal(mu, signal),
              "argument to cover state must be signal");
}
mir_value_t mir_build_package_init(mir_unit_t *mu, ident_t name,
                                   mir_value_t context)
{
   mir_value_t link = mir_add_linkage(mu, name);
   mir_type_t type = mir_context_type(mu, name);

   mir_value_t result;
   if (mir_is_null(context))
      result = mir_build_1(mu, MIR_OP_PACKAGE_INIT, type,
                           MIR_NULL_STAMP, link);
   else
      result = mir_build_2(mu, MIR_OP_PACKAGE_INIT, type,
                           MIR_NULL_STAMP, link, context);

   MIR_ASSERT(mir_is_null(context) || mir_is(mu, context, MIR_TYPE_CONTEXT),
              "invalid package init context argument");
   MIR_ASSERT(mu->kind == MIR_UNIT_INSTANCE
              || mu->kind == MIR_UNIT_PACKAGE
              || mu->kind == MIR_UNIT_THUNK,
              "cannot use package init here");
   MIR_ASSERT(name != mu->name, "cyclic package init");

   return result;
}

void mir_build_process_init(mir_unit_t *mu, ident_t name, mir_value_t locus)
{
   mir_value_t link = mir_add_linkage(mu, name);
   mir_build_2(mu, MIR_OP_PROCESS_INIT, MIR_NULL_TYPE, MIR_NULL_STAMP,
               link, locus);

   MIR_ASSERT(mir_is(mu, locus, MIR_TYPE_LOCUS),
              "locus argument to process init must be a debug locus");
}

mir_value_t mir_build_protected_init(mir_unit_t *mu, mir_type_t type,
                                     mir_value_t context, mir_value_t path_name,
                                     mir_value_t inst_name)
{
   mir_value_t link = mir_add_linkage(mu, mir_type_data(mu, type)->u.context);

   mir_value_t result;
   if (mir_is_null(path_name) && mir_is_null(inst_name))
      result = mir_build_2(mu, MIR_OP_PROTECTED_INIT, type, MIR_NULL_STAMP,
                           link, context);
   else {
      result = mir_build_4(mu, MIR_OP_PROTECTED_INIT, type, MIR_NULL_STAMP,
                           link, context, path_name, inst_name);

      MIR_ASSERT(mir_is(mu, path_name, MIR_TYPE_UARRAY),
                 "path name argument must be array");
      MIR_ASSERT(mir_is(mu, inst_name, MIR_TYPE_UARRAY),
                 "inst name argument must be array");
   }

   MIR_ASSERT(mir_get_class(mu, type) == MIR_TYPE_CONTEXT,
                "protected init type must be context");
   MIR_ASSERT(mir_is(mu, context, MIR_TYPE_CONTEXT),
              "invalid protected init context argument");

   return result;
}

void mir_build_record_scope(mir_unit_t *mu, mir_value_t locus, mir_type_t type)
{
   mir_build_1(mu, MIR_OP_RECORD_SCOPE, type, MIR_NULL_STAMP, locus);

   MIR_ASSERT(mir_is(mu, locus, MIR_TYPE_LOCUS),
              "locus argument to record scope must be a debug locus");
   MIR_ASSERT(mir_get_class(mu, type) == MIR_TYPE_RECORD,
              "record scope type must be record");
}

void mir_build_array_scope(mir_unit_t *mu, mir_value_t locus, mir_type_t type)
{
   mir_build_1(mu, MIR_OP_ARRAY_SCOPE, type, MIR_NULL_STAMP, locus);

   MIR_ASSERT(mir_is(mu, locus, MIR_TYPE_LOCUS),
              "locus argument to array scope must be a debug locus");
}

void mir_build_package_scope(mir_unit_t *mu, mir_value_t locus)
{
   mir_build_1(mu, MIR_OP_PACKAGE_SCOPE, MIR_NULL_TYPE, MIR_NULL_STAMP, locus);

   MIR_ASSERT(mir_is(mu, locus, MIR_TYPE_LOCUS),
              "locus argument to package scope must be a debug locus");
}

void mir_build_pop_scope(mir_unit_t *mu)
{
   mir_build_0(mu, MIR_OP_POP_SCOPE, MIR_NULL_TYPE, MIR_NULL_STAMP);
}

void mir_build_alias_signal(mir_unit_t *mu, mir_value_t signal,
                            mir_value_t locus)
{
   mir_build_2(mu, MIR_OP_ALIAS_SIGNAL, MIR_NULL_TYPE, MIR_NULL_STAMP,
               signal, locus);

   MIR_ASSERT(mir_is_signal(mu, signal), "argument must have signal type");
   MIR_ASSERT(mir_is(mu, locus, MIR_TYPE_LOCUS),
              "locus argument must have debug locus type");
}

void mir_build_map_signal(mir_unit_t *mu, mir_value_t src, mir_value_t dst,
                          mir_value_t count)
{
   mir_build_3(mu, MIR_OP_MAP_SIGNAL, MIR_NULL_TYPE, MIR_NULL_STAMP,
               src, dst, count);

   MIR_ASSERT(mir_is_signal(mu, src),
              "src argument to map signal is not a signal");
   MIR_ASSERT(mir_is_signal(mu, dst),
              "dst argument to map signal is not a signal");
   MIR_ASSERT(mir_is_offset(mu, count),
              "count argument to map signal is not offset type");
}

void mir_build_map_const(mir_unit_t *mu, mir_value_t src, mir_value_t dst,
                         mir_value_t count)
{
   mir_build_3(mu, MIR_OP_MAP_CONST, MIR_NULL_TYPE, MIR_NULL_STAMP,
               src, dst, count);

   MIR_ASSERT(mir_is_signal(mu, dst),
              "dst argument to map const is not a signal");
   MIR_ASSERT(mir_is_offset(mu, count),
              "count argument to map const is not offset type");
}

void mir_build_map_implicit(mir_unit_t *mu, mir_value_t src, mir_value_t dst,
                            mir_value_t count)
{
   mir_build_3(mu, MIR_OP_MAP_IMPLICIT, MIR_NULL_TYPE, MIR_NULL_STAMP,
               src, dst, count);

   MIR_ASSERT(mir_is_signal(mu, src),
              "src argument to map implicit is not a signal");
   MIR_ASSERT(mir_is_signal(mu, dst),
              "dst argument to map implicit is not a signal");
   MIR_ASSERT(mir_is_offset(mu, count),
              "count argument type to map implicit is not offset");
}

mir_value_t mir_build_level_trigger(mir_unit_t *mu, mir_value_t signal,
                                    mir_value_t count)
{
   mir_type_t type = mir_trigger_type(mu);
   mir_value_t result = mir_build_2(mu, MIR_OP_LEVEL_TRIGGER, type,
                                    MIR_NULL_STAMP, signal, count);

   MIR_ASSERT(mir_is_signal(mu, signal),
              "level trigger argument must be signal");
   MIR_ASSERT(mir_is_offset(mu, count),
              "level trigger count argument must be offset");

   return result;
}

mir_value_t mir_build_cmp_trigger(mir_unit_t *mu, mir_value_t left,
                                  mir_value_t right)
{
   mir_type_t type = mir_trigger_type(mu);
   mir_value_t result = mir_build_2(mu, MIR_OP_CMP_TRIGGER, type,
                                    MIR_NULL_STAMP, left, right);

   MIR_ASSERT(mir_is_signal(mu, left),
              "cmp trigger left argument must be signal");
   MIR_ASSERT(mir_is_integral(mu, right),
              "cmp trigger right argument must be integer");

   return result;
}

mir_value_t mir_build_function_trigger(mir_unit_t *mu, ident_t name,
                                       const mir_value_t *args, unsigned nargs)
{
   mir_type_t type = mir_trigger_type(mu);
   node_data_t *n = mir_add_node(mu, MIR_OP_FUNCTION_TRIGGER, type,
                                 MIR_NULL_STAMP, nargs + 1);

   mir_set_arg(mu, n, 0, mir_add_linkage(mu, name));

   for (int i = 0; i < nargs; i++)
      mir_set_arg(mu, n, i + 1, args[i]);

   MIR_ASSERT(nargs >= 1, "function trigger requires at least one argument");

   return (mir_value_t){ .tag = MIR_TAG_NODE, .id = mir_node_id(mu, n) };
}

mir_value_t mir_build_or_trigger(mir_unit_t *mu, mir_value_t left,
                                 mir_value_t right)
{
   mir_type_t type = mir_trigger_type(mu);
   mir_value_t result = mir_build_2(mu, MIR_OP_OR_TRIGGER, type,
                                    MIR_NULL_STAMP, left, right);

   MIR_ASSERT(mir_is(mu, left, MIR_TYPE_TRIGGER),
              "or trigger left argument must be trigger");
   MIR_ASSERT(mir_is(mu, right, MIR_TYPE_TRIGGER),
              "or trigger right argument must be trigger");

   return result;
}

void mir_build_add_trigger(mir_unit_t *mu, mir_value_t trigger)
{
   mir_build_1(mu, MIR_OP_ADD_TRIGGER, MIR_NULL_TYPE, MIR_NULL_STAMP, trigger);

   MIR_ASSERT(mir_is(mu, trigger, MIR_TYPE_TRIGGER),
              "add trigger argument must be trigger");
}

mir_value_t mir_build_link_package(mir_unit_t *mu, ident_t name)
{
   mir_value_t link = mir_add_linkage(mu, name);
   mir_type_t type = mir_context_type(mu, name);

   mir_value_t result = mir_build_1(mu, MIR_OP_LINK_PACKAGE, type,
                                    MIR_NULL_STAMP, link);

   MIR_ASSERT(name != mu->name, "cannot link the current unit");

   return result;
}

mir_value_t mir_build_link_var(mir_unit_t *mu, mir_value_t context,
                               ident_t name, mir_type_t type)
{
   mir_type_t pointer = mir_get_var_pointer(mu, type);
   mir_type_t context_type = mir_get_type(mu, context);

   ident_t unit_name = mir_type_data(mu, context_type)->u.context;

   mir_value_t link = mir_add_linkage(mu, unit_name);
   mir_value_t var = mir_add_extvar(mu, name);
   mir_value_t result = mir_build_3(mu, MIR_OP_LINK_VAR, pointer,
                                    MIR_NULL_STAMP, link, context, var);

   MIR_ASSERT(mir_is(mu, context, MIR_TYPE_CONTEXT),
              "first argument to link var must be context");

   return result;
}

void mir_build_bind_foreign(mir_unit_t *mu, mir_value_t spec,
                            mir_value_t length, mir_value_t locus)
{
   if (mir_is_null(locus))
      mir_build_2(mu, MIR_OP_BIND_FOREIGN, MIR_NULL_TYPE, MIR_NULL_STAMP,
                  spec, length);
   else
      mir_build_3(mu, MIR_OP_BIND_FOREIGN, MIR_NULL_TYPE, MIR_NULL_STAMP,
                  spec, length, locus);

   MIR_ASSERT(mir_is(mu, spec, MIR_TYPE_POINTER),
              "spec argument to bind foreign must be a pointer");
   MIR_ASSERT(mir_is_offset(mu, length),
              "legnth argument to bind foreign must be offset");
   MIR_ASSERT(mir_is_null(locus) || mir_is(mu, locus, MIR_TYPE_LOCUS),
              "locus argument to bind foreign value must be a debug locus");
}

mir_value_t mir_build_bind_external(mir_unit_t *mu, mir_value_t locus,
                                    ident_t scope, mir_type_t type,
                                    mir_stamp_t stamp, const mir_value_t *args,
                                    int nargs)
{
   mir_type_t pointer = mir_get_var_pointer(mu, type);
   mir_value_t link = mir_add_linkage(mu, scope);

   node_data_t *n = mir_add_node(mu, MIR_OP_BIND_EXTERNAL, pointer, stamp,
                                 nargs + 2);
   mir_set_arg(mu, n, 0, locus);
   mir_set_arg(mu, n, 1, link);

   for (int i = 0; i < nargs; i++)
      mir_set_arg(mu, n, i + 2, args[i]);

   MIR_ASSERT(mir_is(mu, locus, MIR_TYPE_LOCUS),
              "bind external argument must be locus");

   return (mir_value_t){ .tag = MIR_TAG_NODE, .id = mir_node_id(mu, n) };
}

mir_value_t mir_build_context_upref(mir_unit_t *mu, int hops)
{
   mir_type_t type = MIR_NULL_TYPE;
   if (hops == 0)
      type = mir_self_type(mu);
   else {
      mir_shape_t *s = mu->parent;
      for (int i = 1; s != NULL && i < hops; i++, s = s->parent);

      if (s != NULL) type = s->type;
   }

   mir_value_t result = mir_build_1(mu, MIR_OP_CONTEXT_UPREF, type,
                                    MIR_NULL_STAMP, mir_enum(hops));

   MIR_ASSERT(hops >= 0, "invalid hop count");
   MIR_ASSERT(!mir_is_null(type), "hop count is greater than depth");

   return result;
}

mir_value_t mir_build_var_upref(mir_unit_t *mu, int hops, int nth)
{
   mir_type_t type = MIR_NULL_TYPE;
   mir_value_t link = MIR_NULL_VALUE;

   mir_shape_t *s = mu->parent;
   for (int i = 1; s != NULL && i < hops; i++, s = s->parent);

   if (s != NULL && nth >= 0 && nth < s->num_slots) {
      type = s->slots[nth].pointer;
      link = mir_add_linkage(mu, s->name);
   }

   mir_value_t result = mir_build_3(mu, MIR_OP_VAR_UPREF, type, MIR_NULL_STAMP,
                                    mir_enum(hops), link, mir_enum(nth));

   MIR_ASSERT(hops > 0, "invalid hop count");
   MIR_ASSERT(!mir_is_null(type), "invalid variable reference");

   return result;
}

void mir_build_sched_event(mir_unit_t *mu, mir_value_t on, mir_value_t count)
{
   if (mir_is_null(count)) {
      mir_build_1(mu, MIR_OP_SCHED_EVENT, MIR_NULL_TYPE, MIR_NULL_STAMP, on);

      MIR_ASSERT(mir_is(mu, on, MIR_TYPE_TRIGGER), "argument must be trigger");
   }
   else {
      mir_build_2(mu, MIR_OP_SCHED_EVENT, MIR_NULL_TYPE, MIR_NULL_STAMP,
                  on, count);

      MIR_ASSERT(mir_is_signal(mu, on), "argument must be signal");
      MIR_ASSERT(mir_is_offset(mu, count), "count argument must be offset");
   }
}

void mir_build_clear_event(mir_unit_t *mu, mir_value_t on, mir_value_t count)
{
   if (mir_is_null(count)) {
      mir_build_1(mu, MIR_OP_CLEAR_EVENT, MIR_NULL_TYPE, MIR_NULL_STAMP, on);

      MIR_ASSERT(mir_is(mu, on, MIR_TYPE_TRIGGER), "argument must be trigger");
   }
   else {
      mir_build_2(mu, MIR_OP_CLEAR_EVENT, MIR_NULL_TYPE, MIR_NULL_STAMP,
                  on, count);

      MIR_ASSERT(mir_is_signal(mu, on), "argument must be signal");
      MIR_ASSERT(mir_is_offset(mu, count), "count argument must be offset");
   }
}

void mir_build_sched_process(mir_unit_t *mu, mir_value_t delay)
{
   mir_build_1(mu, MIR_OP_SCHED_PROCESS, MIR_NULL_TYPE, MIR_NULL_STAMP, delay);

   MIR_ASSERT(mir_is_time(mu, delay), "delay argument is not a time");
   MIR_ASSERT(mu->kind == MIR_UNIT_PROCEDURE || mu->kind == MIR_UNIT_PROCESS,
              "sched process only allowed in process or procedure");
}

void mir_build_sched_inactive(mir_unit_t *mu)
{
   mir_build_0(mu, MIR_OP_SCHED_INACTIVE, MIR_NULL_TYPE, MIR_NULL_STAMP);

   MIR_ASSERT(mu->kind == MIR_UNIT_PROCEDURE || mu->kind == MIR_UNIT_PROCESS,
              "sched inactive only allowed in process or procedure");
}

mir_value_t mir_build_reflect_value(mir_unit_t *mu, mir_value_t value,
                                    mir_value_t context, mir_value_t locus,
                                    mir_value_t bounds)
{
   mir_type_t type = mir_access_type(mu, mir_opaque_type(mu));

   mir_value_t result;
   if (mir_is_null(bounds))
      result = mir_build_3(mu, MIR_OP_REFLECT_VALUE, type, MIR_NULL_STAMP,
                           value, context, locus);
   else
      result = mir_build_4(mu, MIR_OP_REFLECT_VALUE, type, MIR_NULL_STAMP,
                           value, context, locus, bounds);

   MIR_ASSERT(mir_is(mu, context, MIR_TYPE_CONTEXT),
              "invalid reflect value context argument");
   MIR_ASSERT(mir_is(mu, locus, MIR_TYPE_LOCUS),
              "locus argument to reflect value must be a debug locus");

   return result;
}

mir_value_t mir_build_reflect_subtype(mir_unit_t *mu, mir_value_t context,
                                      mir_value_t locus, mir_value_t bounds)
{
   mir_type_t type = mir_access_type(mu, mir_opaque_type(mu));

   mir_value_t result;
   if (mir_is_null(bounds))
      result = mir_build_2(mu, MIR_OP_REFLECT_SUBTYPE, type, MIR_NULL_STAMP,
                           context, locus);
   else
      result = mir_build_3(mu, MIR_OP_REFLECT_SUBTYPE, type, MIR_NULL_STAMP,
                           context, locus, bounds);

   MIR_ASSERT(mir_is(mu, context, MIR_TYPE_CONTEXT),
              "invalid reflect subtype context argument");
   MIR_ASSERT(mir_is(mu, locus, MIR_TYPE_LOCUS),
              "locus argument to reflect subtype must be a debug locus");

   return result;
}

void mir_build_assert(mir_unit_t *mu, mir_value_t value, mir_value_t message,
                      mir_value_t length, mir_value_t severity,
                      mir_value_t locus, mir_value_t hint_left,
                      mir_value_t hint_right)
{
   int64_t value_const;
   if (mir_get_const(mu, value, &value_const) && value_const != 0) {
      mir_comment(mu, "Always true assertion");
      return;
   }

   if (mir_is_null(hint_left))
      mir_build_5(mu, MIR_OP_ASSERT, MIR_NULL_TYPE, MIR_NULL_STAMP,
                  value, severity, message, length, locus);
   else {
      node_data_t *n = mir_add_node(mu, MIR_OP_ASSERT, MIR_NULL_TYPE,
                                     MIR_NULL_STAMP, 7);
      mir_set_arg(mu, n, 0, value);
      mir_set_arg(mu, n, 1, severity);
      mir_set_arg(mu, n, 2, message);
      mir_set_arg(mu, n, 3, length);
      mir_set_arg(mu, n, 4, locus);
      mir_set_arg(mu, n, 5, hint_left);
      mir_set_arg(mu, n, 6, hint_right);

      MIR_ASSERT(mir_is_scalar(mu, hint_left), "left hint must be scalar");
      MIR_ASSERT(mir_is_scalar(mu, hint_right), "right hint must be scalar");
   }

   MIR_ASSERT(mir_is_bool(mu, value), "value parameter to assert is not bool");
   MIR_ASSERT(mir_is_null(message) || mir_is(mu, message, MIR_TYPE_POINTER),
              "message parameter to assert is not a pointer");
   MIR_ASSERT(mir_is_bool(mu, value), "value parameter to assert is not bool");
   MIR_ASSERT(mir_is(mu, locus, MIR_TYPE_LOCUS),
              "locus argument to report must be a debug locus");
}

void mir_build_report(mir_unit_t *mu, mir_value_t message, mir_value_t length,
                      mir_value_t severity, mir_value_t locus)
{
   mir_build_4(mu, MIR_OP_REPORT, MIR_NULL_TYPE, MIR_NULL_STAMP,
               severity, message, length, locus);

   MIR_ASSERT(mir_is(mu, message, MIR_TYPE_POINTER),
              "message parameter to report is not a pointer");
   MIR_ASSERT(mir_is_offset(mu, length), "length argument must be offset type");
   MIR_ASSERT(mir_is(mu, locus, MIR_TYPE_LOCUS),
              "locus argument to report must be a debug locus");
}

mir_value_t mir_build_instance_name(mir_unit_t *mu, mir_value_t kind)
{
   mir_type_t type = mir_string_type(mu);
   mir_value_t result = mir_build_1(mu, MIR_OP_INSTANCE_NAME, type,
                                    MIR_NULL_STAMP, kind);

   MIR_ASSERT(mir_is_offset(mu, kind),
              "kind argument to instance name must be offset");

   return result;
}

void mir_build_enter_state(mir_unit_t *mu, mir_value_t state,
                           mir_value_t strong)
{
   if (mir_is_null(strong))
      mir_build_1(mu, MIR_OP_ENTER_STATE, MIR_NULL_TYPE, MIR_NULL_STAMP, state);
   else
      mir_build_2(mu, MIR_OP_ENTER_STATE, MIR_NULL_TYPE, MIR_NULL_STAMP,
                  state, strong);

   MIR_ASSERT(mir_is_integral(mu, state), "state must have integer type");
   MIR_ASSERT(mir_is_null(strong)
              || mir_check_type(mu, strong, mir_bool_type(mu)),
                "strong argument not is not boolean");
}

mir_value_t mir_build_closure(mir_unit_t *mu, ident_t func, mir_value_t context,
                              mir_type_t atype, mir_type_t rtype)
{
   mir_type_t ctype = mir_closure_type(mu, atype, rtype);
   mir_value_t link = mir_add_linkage(mu, func);

   mir_value_t result = mir_build_2(mu, MIR_OP_CLOSURE, ctype, MIR_NULL_STAMP,
                                    link, context);

   MIR_ASSERT(mir_is(mu, context, MIR_TYPE_CONTEXT),
              "invalid closure context argument");

   return result;
}

mir_value_t mir_build_resolution_wrapper(mir_unit_t *mu, mir_type_t type,
                                         mir_value_t closure, mir_value_t nlits)
{
   mir_type_t rtype = mir_resolution_type(mu, type);
   mir_value_t result = mir_build_2(mu, MIR_OP_RESOLUTION_WRAPPER, rtype,
                                    MIR_NULL_STAMP, closure, nlits);

   MIR_ASSERT(mir_is(mu, closure, MIR_TYPE_CLOSURE),
              "first argument to resolution wrapper must be closure");

   return result;
}

mir_value_t mir_build_locus(mir_unit_t *mu, object_t *obj)
{
   node_data_t *n = mir_add_node(mu, MIR_OP_LOCUS, mir_locus_type(mu),
                                  MIR_NULL_STAMP, 0);
   n->locus = obj;

   return (mir_value_t){ .tag = MIR_TAG_NODE, .id = mir_node_id(mu, n) };
}

mir_value_t mir_build_cast(mir_unit_t *mu, mir_type_t type, mir_value_t value)
{
   mir_type_t from = mir_get_type(mu, value);
   if (mir_equals(from, type))
      return value;

   const mir_class_t class = mir_get_class(mu, type);
   const bool integral = (class == MIR_TYPE_OFFSET || class == MIR_TYPE_INT);

   int64_t cval;
   if (integral && mir_get_const(mu, value, &cval))
      return mir_const(mu, type, cval);

   if (class == MIR_TYPE_VEC2 || class == MIR_TYPE_VEC4) {
      if (value.tag == MIR_TAG_NODE) {
         node_data_t *n = mir_node_data(mu, value);
         if (n->op == MIR_OP_CONST_VEC && mir_get_size(mu, type) <= 64) {
            const uint64_t bbits = (class == MIR_TYPE_VEC2 ? 0 : n->bits[1]);
            return mir_const_vec(mu, type, n->bits[0], bbits);
         }
      }
   }
   else if (integral && mir_is_vector(mu, value)) {
      node_data_t *n = mir_node_data(mu, value);
      if (n->op == MIR_OP_CONST_VEC) {
         MIR_ASSERT(n->bits[1] == 0, "X value in integral cast");
         return mir_const(mu, type, n->bits[0]);
      }
   }

   mir_stamp_t stamp = mir_stamp_cast(mu, type, mir_get_stamp(mu, value));

   mir_value_t result = mir_build_1(mu, MIR_OP_CAST, type, stamp, value);

#ifdef DEBUG
   static const mir_class_t allowed[][2] = {
      { MIR_TYPE_INT,    MIR_TYPE_OFFSET  },
      { MIR_TYPE_OFFSET, MIR_TYPE_INT     },
      { MIR_TYPE_INT,    MIR_TYPE_INT     },
      { MIR_TYPE_INT,    MIR_TYPE_REAL    },
      { MIR_TYPE_REAL,   MIR_TYPE_INT     },
      { MIR_TYPE_REAL,   MIR_TYPE_REAL    },
      { MIR_TYPE_ACCESS, MIR_TYPE_ACCESS  },
      { MIR_TYPE_VEC4,   MIR_TYPE_VEC2    },
      { MIR_TYPE_VEC2,   MIR_TYPE_VEC4    },
      { MIR_TYPE_VEC2,   MIR_TYPE_VEC2    },
      { MIR_TYPE_VEC4,   MIR_TYPE_VEC4    },
      { MIR_TYPE_VEC2,   MIR_TYPE_INT     },
      { MIR_TYPE_VEC2,   MIR_TYPE_OFFSET  },
   };

   if (value.tag == MIR_TAG_CONST)
      return result;

   const mir_class_t from_k = mir_get_class(mu, from);

   for (size_t i = 0; i < ARRAY_LEN(allowed); i++) {
      if (from_k == allowed[i][0] && class == allowed[i][1])
         return result;
   }

   MIR_ASSERT(false, "invalid type conversion in cast");
#else
   return result;
#endif
}

void mir_build_debug_out(mir_unit_t *mu, mir_value_t value)
{
   mir_build_1(mu, MIR_OP_DEBUG_OUT, MIR_NULL_TYPE, MIR_NULL_STAMP, value);

   MIR_ASSERT(mir_is_integral(mu, value), "argument must be integral");
}
