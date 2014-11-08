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

#ifndef _VCODE_H
#define _VCODE_H

#include "util.h"
#include "ident.h"
#include "prim.h"

typedef int32_t vcode_type_t;
typedef int32_t vcode_block_t;
typedef int32_t vcode_var_t;
typedef int32_t vcode_reg_t;
typedef int32_t vcode_signal_t;

typedef enum {
   VCODE_CMP_EQ
} vcode_cmp_t;

typedef enum {
   VCODE_OP_CMP,
   VCODE_OP_FCALL,
   VCODE_OP_WAIT,
   VCODE_OP_CONST,
   VCODE_OP_ASSERT,
   VCODE_OP_JUMP,
   VCODE_OP_LOAD,
   VCODE_OP_STORE,
   VCODE_OP_MUL,
   VCODE_OP_ADD,
   VCODE_OP_BOUNDS,
   VCODE_OP_COMMENT,
   VCODE_OP_CONST_ARRAY,
   VCODE_OP_INDEX,
   VCODE_OP_SUB,
   VCODE_OP_CAST,
   VCODE_OP_LOAD_INDIRECT,
   VCODE_OP_STORE_INDIRECT,
   VCODE_OP_RETURN,
   VCODE_OP_NETS,
   VCODE_OP_SCHED_WAVEFORM,
   VCODE_OP_COND,
   VCODE_OP_REPORT
} vcode_op_t;

typedef enum {
   VCODE_TYPE_INT,
   VCODE_TYPE_CARRAY,
   VCODE_TYPE_POINTER,
   VCODE_TYPE_OFFSET,
   VCODE_TYPE_SIGNAL
} vtype_kind_t;

typedef enum {
   VCODE_UNIT_PROCESS,
   VCODE_UNIT_CONTEXT
} vunit_kind_t;

#define VCODE_INVALID_REG    -1
#define VCODE_INVALID_BLOCK  -1
#define VCODE_INVALID_VAR    -1
#define VCODE_INVALID_SIGNAL -1

vcode_type_t vtype_int(int64_t low, int64_t high);
vcode_type_t vtype_dynamic(vcode_reg_t low, vcode_reg_t high);
vcode_type_t vtype_bool(void);
vcode_type_t vtype_carray(const vcode_type_t *dim, int ndim,
                          vcode_type_t elem, vcode_type_t bounds);
vcode_type_t vtype_pointer(vcode_type_t to);
vcode_type_t vtype_signal(vcode_type_t base);
vcode_type_t vtype_offset(void);
bool vtype_eq(vcode_type_t a, vcode_type_t b);
bool vtype_includes(vcode_type_t type, vcode_type_t bounds);
vtype_kind_t vtype_kind(vcode_type_t type);
int64_t vtype_low(vcode_type_t type);
int64_t vtype_high(vcode_type_t type);
vcode_type_t vtype_elem(vcode_type_t type);
vcode_type_t vtype_pointed(vcode_type_t type);
vcode_type_t vtype_bounds(vcode_type_t type);
int vtype_dims(vcode_type_t type);
vcode_type_t vtype_dim(vcode_type_t type, int dim);

void vcode_opt(void);
void vcode_close(void);
void vcode_dump(void);
void vcode_select_unit(vcode_unit_t vu);
void vcode_select_block(vcode_block_t block);
int vcode_count_blocks(void);
const char *vcode_op_string(vcode_op_t op);
bool vcode_block_finished(void);
ident_t vcode_unit_name(void);

int vcode_count_regs(void);
vcode_type_t vcode_reg_type(vcode_reg_t reg);
vcode_type_t vcode_reg_bounds(vcode_reg_t reg);

int vcode_count_signals(void);
vcode_var_t vcode_signal_shadow(vcode_signal_t sig);
ident_t vcode_signal_name(vcode_signal_t sig);
size_t vcode_signal_count_nets(vcode_signal_t sig);
const netid_t *vcode_signal_nets(vcode_signal_t sig);
vcode_type_t vcode_signal_type(vcode_signal_t sig);
vcode_type_t vcode_signal_bounds(vcode_signal_t sig);

int vcode_count_ops(void);
vcode_op_t vcode_get_op(int op);
ident_t vcode_get_func(int op);
int64_t vcode_get_value(int op);
vcode_cmp_t vcode_get_cmp(int op);
uint32_t vcode_get_index(int op);
vcode_block_t vcode_get_target(int op);
vcode_block_t vcode_get_target_else(int op);
vcode_var_t vcode_get_address(int op);
int vcode_count_args(int op);
vcode_reg_t vcode_get_arg(int op, int arg);
vcode_type_t vcode_get_type(int op);
vcode_reg_t vcode_get_result(int op);
vcode_signal_t vcode_get_signal(int op);

int vcode_count_vars(void);
ident_t vcode_var_name(vcode_var_t var);
vcode_type_t vcode_var_type(vcode_var_t var);

vcode_unit_t emit_func(ident_t name);
vcode_unit_t emit_process(ident_t name, vcode_unit_t context);
vcode_unit_t emit_context(ident_t name);
vcode_block_t emit_block(void);
vcode_var_t emit_var(vcode_type_t type, vcode_type_t bounds, ident_t name);
vcode_signal_t emit_signal(vcode_type_t type, vcode_type_t bounds,
                           ident_t name, vcode_var_t shadow,
                           netid_t *nets, size_t nnets);
vcode_reg_t emit_const(vcode_type_t type, int64_t value);
vcode_reg_t emit_const_array(vcode_type_t type, vcode_reg_t *values, int num);
vcode_reg_t emit_add(vcode_reg_t lhs, vcode_reg_t rhs);
vcode_reg_t emit_sub(vcode_reg_t lhs, vcode_reg_t rhs);
vcode_reg_t emit_mul(vcode_reg_t lhs, vcode_reg_t rhs);
void emit_assert(vcode_reg_t value, vcode_reg_t message,
                 vcode_reg_t severity, uint32_t index);
void emit_report(vcode_reg_t message, vcode_reg_t severity, uint32_t index);
vcode_reg_t emit_cmp(vcode_cmp_t cmp, vcode_reg_t lhs, vcode_reg_t rhs);
vcode_reg_t emit_fcall(ident_t func, vcode_type_t type,
                       const vcode_reg_t *args, int nargs);
void emit_wait(vcode_block_t target, vcode_reg_t time);
void emit_jump(vcode_block_t target);
vcode_reg_t emit_load(vcode_var_t var);
vcode_reg_t emit_load_indirect(vcode_reg_t reg);
void emit_store(vcode_reg_t reg, vcode_var_t var);
void emit_store_indirect(vcode_reg_t reg, vcode_reg_t ptr);
void emit_bounds(vcode_reg_t reg, vcode_type_t bounds);
vcode_reg_t emit_index(vcode_var_t var, vcode_reg_t offset);
vcode_reg_t emit_cast(vcode_type_t type, vcode_reg_t reg);
void emit_return(vcode_reg_t reg);
vcode_reg_t emit_nets(vcode_signal_t sig);
void emit_sched_waveform(vcode_reg_t nets, vcode_reg_t nnets,
                         vcode_reg_t values, vcode_reg_t reject,
                         vcode_reg_t after);
void emit_cond(vcode_reg_t test, vcode_block_t btrue, vcode_block_t bfalse);

#endif  // _VCODE_H
