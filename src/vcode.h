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

typedef struct vtype *vtype_t;

typedef int vcode_block_t;
typedef int vcode_var_t;
typedef int vcode_reg_t;

typedef enum {
   VCODE_CMP_EQ
} vcode_cmp_t;

typedef enum {
   VCODE_OP_CMP,
   VCODE_OP_FCALL,
   VCODE_OP_WAIT,
   VCODE_OP_CONST,
   VCODE_OP_ASSERT,
   VCODE_OP_JUMP
} vcode_op_t;

#define VCODE_INVALID_REG   -1
#define VCODE_INVALID_BLOCK -1

vtype_t vtype_int(int64_t low, int64_t high);
vtype_t vtype_dynamic(vcode_reg_t low, vcode_reg_t high);

void vcode_close(void);
void vcode_dump(void);
void vcode_select_unit(vcode_unit_t vu);
void vcode_select_block(vcode_block_t block);
int vcode_count_blocks(void);
int vcode_count_ops(void);
vcode_op_t vcode_get_op(int op);
const char *vcode_op_string(vcode_op_t op);
ident_t vcode_get_func(int op);
int64_t vcode_get_value(int op);
vcode_cmp_t vcode_get_cmp(int op);
vcode_block_t vcode_get_target(int op);
bool vcode_block_finished(void);

vcode_unit_t emit_func(ident_t name);
vcode_unit_t emit_process(ident_t name);
vcode_block_t emit_block(void);
vcode_var_t emit_var(ident_t name);
vcode_reg_t emit_const(int64_t value);
void emit_add(vcode_reg_t target, vcode_reg_t lhs, vcode_reg_t rhs);
void emit_assert(vcode_reg_t value);
vcode_reg_t emit_cmp(vcode_cmp_t cmp, vcode_reg_t lhs, vcode_reg_t rhs);
vcode_reg_t emit_fcall(ident_t func, const vcode_reg_t *args, int nargs);
void emit_wait(vcode_block_t target, vcode_reg_t time);
void emit_jump(vcode_block_t target);

#endif  // _VCODE_H
