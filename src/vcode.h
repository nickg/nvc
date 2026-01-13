//
//  Copyright (C) 2014-2024  Nick Gasson
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
#include "rt/rt.h"

typedef int32_t vcode_type_t;
typedef int32_t vcode_stamp_t;
typedef int32_t vcode_block_t;
typedef int32_t vcode_var_t;
typedef int32_t vcode_reg_t;

typedef enum {
   VCODE_CMP_EQ,
   VCODE_CMP_NEQ,
   VCODE_CMP_LT,
   VCODE_CMP_GT,
   VCODE_CMP_LEQ,
   VCODE_CMP_GEQ,
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
   VCODE_OP_COMMENT,
   VCODE_OP_CONST_ARRAY,
   VCODE_OP_INDEX,
   VCODE_OP_SUB,
   VCODE_OP_CAST,
   VCODE_OP_LOAD_INDIRECT,
   VCODE_OP_STORE_INDIRECT,
   VCODE_OP_RETURN,
   VCODE_OP_SCHED_WAVEFORM,
   VCODE_OP_COND,
   VCODE_OP_REPORT,
   VCODE_OP_DIV,
   VCODE_OP_NEG,
   VCODE_OP_EXP,
   VCODE_OP_ABS,
   VCODE_OP_MOD,
   VCODE_OP_REM,
   VCODE_OP_ALLOC,
   VCODE_OP_SELECT,
   VCODE_OP_OR,
   VCODE_OP_WRAP,
   VCODE_OP_UARRAY_LEFT,
   VCODE_OP_UARRAY_RIGHT,
   VCODE_OP_UARRAY_DIR,
   VCODE_OP_UNWRAP,
   VCODE_OP_NOT,
   VCODE_OP_AND,
   VCODE_OP_EVENT,
   VCODE_OP_ACTIVE,
   VCODE_OP_CONST_RECORD,
   VCODE_OP_RECORD_REF,
   VCODE_OP_COPY,
   VCODE_OP_SCHED_EVENT,
   VCODE_OP_PCALL,
   VCODE_OP_RESUME,
   VCODE_OP_XOR,
   VCODE_OP_XNOR,
   VCODE_OP_NAND,
   VCODE_OP_NOR,
   VCODE_OP_MEMSET,
   VCODE_OP_CASE,
   VCODE_OP_FILE_OPEN,
   VCODE_OP_FILE_WRITE,
   VCODE_OP_FILE_READ,
   VCODE_OP_NULL,
   VCODE_OP_NEW,
   VCODE_OP_NULL_CHECK,
   VCODE_OP_DEALLOCATE,
   VCODE_OP_ALL,
   VCODE_OP_CONST_REAL,
   VCODE_OP_LAST_EVENT,
   VCODE_OP_DEBUG_OUT,
   VCODE_OP_COVER_STMT,
   VCODE_OP_COVER_BRANCH,
   VCODE_OP_COVER_TOGGLE,
   VCODE_OP_COVER_EXPR,
   VCODE_OP_COVER_STATE,
   VCODE_OP_UARRAY_LEN,
   VCODE_OP_UNDEFINED,
   VCODE_OP_RANGE_NULL,
   VCODE_OP_VAR_UPREF,
   VCODE_OP_RESOLVED,
   VCODE_OP_LAST_VALUE,
   VCODE_OP_INIT_SIGNAL,
   VCODE_OP_MAP_SIGNAL,
   VCODE_OP_DRIVE_SIGNAL,
   VCODE_OP_LINK_VAR,
   VCODE_OP_RESOLUTION_WRAPPER,
   VCODE_OP_LAST_ACTIVE,
   VCODE_OP_DRIVING,
   VCODE_OP_DRIVING_VALUE,
   VCODE_OP_ADDRESS_OF,
   VCODE_OP_CLOSURE,
   VCODE_OP_PROTECTED_INIT,
   VCODE_OP_CONTEXT_UPREF,
   VCODE_OP_CONST_REP,
   VCODE_OP_PROTECTED_FREE,
   VCODE_OP_IMPLICIT_SIGNAL,
   VCODE_OP_DISCONNECT,
   VCODE_OP_LINK_PACKAGE,
   VCODE_OP_INDEX_CHECK,
   VCODE_OP_DEBUG_LOCUS,
   VCODE_OP_LENGTH_CHECK,
   VCODE_OP_RANGE_CHECK,
   VCODE_OP_ARRAY_REF,
   VCODE_OP_RANGE_LENGTH,
   VCODE_OP_EXPONENT_CHECK,
   VCODE_OP_ZERO_CHECK,
   VCODE_OP_MAP_CONST,
   VCODE_OP_RESOLVE_SIGNAL,
   VCODE_OP_PACKAGE_SCOPE,
   VCODE_OP_POP_SCOPE,
   VCODE_OP_ALIAS_SIGNAL,
   VCODE_OP_TRAP_ADD,
   VCODE_OP_TRAP_SUB,
   VCODE_OP_TRAP_MUL,
   VCODE_OP_FORCE,
   VCODE_OP_RELEASE,
   VCODE_OP_UNREACHABLE,
   VCODE_OP_PACKAGE_INIT,
   VCODE_OP_TRAP_NEG,
   VCODE_OP_PROCESS_INIT,
   VCODE_OP_CLEAR_EVENT,
   VCODE_OP_TRAP_EXP,
   VCODE_OP_ENTER_STATE,
   VCODE_OP_REFLECT_VALUE,
   VCODE_OP_REFLECT_SUBTYPE,
   VCODE_OP_FUNCTION_TRIGGER,
   VCODE_OP_ADD_TRIGGER,
   VCODE_OP_TRANSFER_SIGNAL,
   VCODE_OP_PORT_CONVERSION,
   VCODE_OP_CONVERT_IN,
   VCODE_OP_CONVERT_OUT,
   VCODE_OP_BIND_FOREIGN,
   VCODE_OP_OR_TRIGGER,
   VCODE_OP_CMP_TRIGGER,
   VCODE_OP_INSTANCE_NAME,
   VCODE_OP_MAP_IMPLICIT,
   VCODE_OP_BIND_EXTERNAL,
   VCODE_OP_ARRAY_SCOPE,
   VCODE_OP_RECORD_SCOPE,
   VCODE_OP_PUT_CONVERSION,
   VCODE_OP_DIR_CHECK,
   VCODE_OP_SCHED_PROCESS,
   VCODE_OP_TABLE_REF,
} vcode_op_t;

typedef enum {
   VCODE_TYPE_INT,
   VCODE_TYPE_CARRAY,
   VCODE_TYPE_POINTER,
   VCODE_TYPE_OFFSET,
   VCODE_TYPE_SIGNAL,
   VCODE_TYPE_UARRAY,
   VCODE_TYPE_RECORD,
   VCODE_TYPE_FILE,
   VCODE_TYPE_ACCESS,
   VCODE_TYPE_REAL,
   VCODE_TYPE_OPAQUE,
   VCODE_TYPE_RESOLUTION,
   VCODE_TYPE_CLOSURE,
   VCODE_TYPE_CONTEXT,
   VCODE_TYPE_DEBUG_LOCUS,
   VCODE_TYPE_TRIGGER,
   VCODE_TYPE_CONVERSION,
} vtype_kind_t;

typedef enum {
   VCODE_REPR_U1,
   VCODE_REPR_I8,
   VCODE_REPR_U8,
   VCODE_REPR_I16,
   VCODE_REPR_U16,
   VCODE_REPR_I32,
   VCODE_REPR_U32,
   VCODE_REPR_I64,
   VCODE_REPR_U64,
} vtype_repr_t;

typedef enum {
   VCODE_UNIT_PROCESS,
   VCODE_UNIT_FUNCTION,
   VCODE_UNIT_PROCEDURE,
   VCODE_UNIT_THUNK,
   VCODE_UNIT_INSTANCE,
   VCODE_UNIT_PACKAGE,
   VCODE_UNIT_PROTECTED,
   VCODE_UNIT_PROPERTY,
} vunit_kind_t;

typedef struct {
   vcode_reg_t left;
   vcode_reg_t right;
   vcode_reg_t dir;
} vcode_dim_t;

typedef struct {
   vcode_unit_t  unit;
   vcode_block_t block;
} vcode_state_t;

typedef enum {
   VAR_TEMP    = (1 << 1),
   VAR_HEAP    = (1 << 2),
   VAR_CONST   = (1 << 3),
   VAR_SIGNAL  = (1 << 4)
} vcode_var_flags_t;

#define VCODE_INVALID_REG    -1
#define VCODE_INVALID_BLOCK  -1
#define VCODE_INVALID_VAR    -1
#define VCODE_INVALID_TYPE   -1
#define VCODE_INVALID_STAMP  -1

typedef int (*vcode_dump_fn_t)(int, void *);
typedef void (*vcode_dep_fn_t)(ident_t, void *);

vcode_type_t vtype_int(int64_t low, int64_t high);
vcode_type_t vtype_dynamic(vcode_reg_t low, vcode_reg_t high);
vcode_type_t vtype_bool(void);
vcode_type_t vtype_carray(int size, vcode_type_t elem);
vcode_type_t vtype_uarray(int ndim, vcode_type_t elem);
vcode_type_t vtype_pointer(vcode_type_t to);
vcode_type_t vtype_access(vcode_type_t to);
vcode_type_t vtype_signal(vcode_type_t base);
vcode_type_t vtype_offset(void);
vcode_type_t vtype_time(void);
vcode_type_t vtype_char(void);
vcode_type_t vtype_opaque(void);
vcode_type_t vtype_find_named_record(ident_t name);
vcode_type_t vtype_named_record(ident_t name, const vcode_type_t *field_types,
                                int nfields);
vcode_type_t vtype_file(vcode_type_t base);
vcode_type_t vtype_resolution(vcode_type_t base);
vcode_type_t vtype_closure(vcode_type_t result);
vcode_type_t vtype_context(ident_t name);
vcode_type_t vtype_debug_locus(void);
vcode_type_t vtype_trigger(void);
vcode_type_t vtype_conversion(void);
bool vtype_eq(vcode_type_t a, vcode_type_t b);
vtype_kind_t vtype_kind(vcode_type_t type);
bool vtype_is_scalar(vcode_type_t type);
bool vtype_is_composite(vcode_type_t type);
bool vtype_is_signal(vcode_type_t type);
bool vtype_is_numeric(vcode_type_t type);
bool vtype_is_integral(vcode_type_t type);
int64_t vtype_low(vcode_type_t type);
int64_t vtype_high(vcode_type_t type);
vcode_type_t vtype_elem(vcode_type_t type);
vcode_type_t vtype_pointed(vcode_type_t type);
unsigned vtype_dims(vcode_type_t type);
unsigned vtype_size(vcode_type_t type);
int vtype_fields(vcode_type_t type);
vcode_type_t vtype_field(vcode_type_t type, int field);
vcode_type_t vtype_base(vcode_type_t type);
ident_t vtype_name(vcode_type_t type);
vcode_type_t vtype_real(double low, double high);
vtype_repr_t vtype_repr(vcode_type_t type);
int vtype_repr_bits(vtype_repr_t repr);
bool vtype_repr_signed(vtype_repr_t repr);

vcode_stamp_t vstamp_int(int64_t low, int64_t high);
vcode_stamp_t vstamp_real(double low, double high);
vcode_stamp_t vstamp_char(void);

vcode_unit_t vcode_unit_next(vcode_unit_t unit);
vcode_unit_t vcode_unit_child(vcode_unit_t unit);
void vcode_unit_unref(vcode_unit_t unit);
void vcode_walk_dependencies(vcode_unit_t vu, vcode_dep_fn_t fn, void *ctx);

void vcode_opt(void);
void vcode_close(void);
void vcode_dump(void);
void vcode_dump_with_mark(int mark_op, vcode_dump_fn_t callback, void *arg);
void vcode_select_unit(vcode_unit_t vu);
void vcode_select_block(vcode_block_t block);
int vcode_count_blocks(void);
const loc_t *vcode_last_loc(void);
const char *vcode_op_string(vcode_op_t op);
bool vcode_block_finished(void);
bool vcode_block_empty(void);
ident_t vcode_unit_name(vcode_unit_t vu);
int vcode_unit_depth(vcode_unit_t vu);
bool vcode_unit_has_undefined(vcode_unit_t vu);
vunit_kind_t vcode_unit_kind(vcode_unit_t vu);
vcode_type_t vcode_unit_result(vcode_unit_t vu);
vcode_block_t vcode_active_block(void);
vcode_unit_t vcode_active_unit(void);
vcode_unit_t vcode_unit_context(vcode_unit_t vu);
object_t *vcode_unit_object(vcode_unit_t vu);
void vcode_set_result(vcode_type_t type);

void vcode_state_save(vcode_state_t *state);
void vcode_state_restore(const vcode_state_t *state);

int vcode_count_params(void);
vcode_type_t vcode_param_type(int param);
ident_t vcode_param_name(int param);
vcode_reg_t vcode_param_reg(int param);

int vcode_count_regs(void);
vcode_type_t vcode_reg_type(vcode_reg_t reg);
vtype_kind_t vcode_reg_kind(vcode_reg_t reg);
vcode_stamp_t vcode_reg_stamp(vcode_reg_t reg);
bool vcode_reg_const(vcode_reg_t reg, int64_t *value);
bool vcode_reg_bounds(vcode_reg_t reg, int64_t *low, int64_t *high);
void vcode_heap_allocate(vcode_reg_t reg);

int vcode_count_ops(void);
vcode_op_t vcode_get_op(int op);
ident_t vcode_get_func(int op);
int64_t vcode_get_value(int op);
double vcode_get_real(int op);
vcode_cmp_t vcode_get_cmp(int op);
const loc_t *vcode_get_loc(int op);
vcode_block_t vcode_get_target(int op, int nth);
vcode_var_t vcode_get_address(int op);
ident_t vcode_get_ident(int op);
object_t *vcode_get_object(int op);
int vcode_count_args(int op);
vcode_reg_t vcode_get_arg(int op, int arg);
vcode_type_t vcode_get_type(int op);
vcode_reg_t vcode_get_result(int op);
unsigned vcode_get_dim(int op);
int vcode_get_hops(int op);
int vcode_get_field(int op);
uint32_t vcode_get_tag(int op);

int vcode_count_vars(void);
vcode_var_t vcode_find_var(ident_t name);
ident_t vcode_var_name(vcode_var_t var);
vcode_type_t vcode_var_type(vcode_var_t var);
vcode_var_flags_t vcode_var_flags(vcode_var_t var);

vcode_unit_t emit_function(ident_t name, object_t *obj, vcode_unit_t context);
vcode_unit_t emit_procedure(ident_t name, object_t *obj, vcode_unit_t context);
vcode_unit_t emit_process(ident_t name, object_t *obj, vcode_unit_t context);
vcode_unit_t emit_instance(ident_t name, object_t *obj, vcode_unit_t context);
vcode_unit_t emit_package(ident_t name, object_t *obj, vcode_unit_t context);
vcode_unit_t emit_protected(ident_t name, object_t *obj, vcode_unit_t context);
vcode_unit_t emit_property(ident_t name, object_t *obj, vcode_unit_t context);
vcode_unit_t emit_thunk(ident_t name, object_t *obj, vcode_unit_t context);
vcode_block_t emit_block(void);
vcode_var_t emit_var(vcode_type_t type, vcode_stamp_t stamp, ident_t name,
                     vcode_var_flags_t flags);
vcode_reg_t emit_alloc(vcode_type_t type, vcode_stamp_t stamp,
                       vcode_reg_t count);
vcode_reg_t emit_param(vcode_type_t type, vcode_stamp_t stamp, ident_t name);
vcode_reg_t emit_const(vcode_type_t type, int64_t value);
vcode_reg_t emit_const_array(vcode_type_t type, vcode_reg_t *values, int num);
vcode_reg_t emit_const_rep(vcode_type_t type, vcode_reg_t value, int rep);
vcode_reg_t emit_const_record(vcode_type_t type, vcode_reg_t *values, int num);
vcode_reg_t emit_const_real(vcode_type_t type, double value);
vcode_reg_t emit_address_of(vcode_reg_t value);
vcode_reg_t emit_add(vcode_reg_t lhs, vcode_reg_t rhs);
vcode_reg_t emit_sub(vcode_reg_t lhs, vcode_reg_t rhs);
vcode_reg_t emit_mul(vcode_reg_t lhs, vcode_reg_t rhs);
vcode_reg_t emit_div(vcode_reg_t lhs, vcode_reg_t rhs);
vcode_reg_t emit_exp(vcode_reg_t lhs, vcode_reg_t rhs);
vcode_reg_t emit_mod(vcode_reg_t lhs, vcode_reg_t rhs);
vcode_reg_t emit_rem(vcode_reg_t lhs, vcode_reg_t rhs);
vcode_reg_t emit_trap_add(vcode_reg_t lhs, vcode_reg_t rhs, vcode_reg_t locus);
vcode_reg_t emit_trap_sub(vcode_reg_t lhs, vcode_reg_t rhs, vcode_reg_t locus);
vcode_reg_t emit_trap_mul(vcode_reg_t lhs, vcode_reg_t rhs, vcode_reg_t locus);
vcode_reg_t emit_trap_exp(vcode_reg_t lhs, vcode_reg_t rhs, vcode_reg_t locus);
void emit_assert(vcode_reg_t value, vcode_reg_t message, vcode_reg_t length,
                 vcode_reg_t severity, vcode_reg_t locus, vcode_reg_t hint_left,
                 vcode_reg_t hint_right);
void emit_report(vcode_reg_t message, vcode_reg_t length, vcode_reg_t severity,
                 vcode_reg_t locus);
vcode_reg_t emit_cmp(vcode_cmp_t cmp, vcode_reg_t lhs, vcode_reg_t rhs);
vcode_reg_t emit_fcall(ident_t func, vcode_type_t type, vcode_stamp_t stamp,
                       const vcode_reg_t *args, int nargs);
void emit_pcall(ident_t func, const vcode_reg_t *args, int nargs,
                vcode_block_t resume_bb);
void emit_wait(vcode_block_t target);
void emit_jump(vcode_block_t target);
vcode_reg_t emit_load(vcode_var_t var);
vcode_reg_t emit_load_indirect(vcode_reg_t reg);
void emit_store(vcode_reg_t reg, vcode_var_t var);
void emit_store_indirect(vcode_reg_t reg, vcode_reg_t ptr);
void emit_range_check(vcode_reg_t reg, vcode_reg_t left, vcode_reg_t right,
                      vcode_reg_t dir, vcode_reg_t locus, vcode_reg_t hint);
void emit_index_check(vcode_reg_t reg, vcode_reg_t left, vcode_reg_t right,
                      vcode_reg_t dir, vcode_reg_t locus, vcode_reg_t hint);
void emit_dir_check(vcode_reg_t reg, vcode_reg_t dir, vcode_reg_t locus);
vcode_reg_t emit_index(vcode_var_t var, vcode_reg_t offset);
vcode_reg_t emit_cast(vcode_type_t type, vcode_reg_t bounds, vcode_reg_t reg);
void emit_return(vcode_reg_t reg);
void emit_sched_waveform(vcode_reg_t nets, vcode_reg_t nnets,
                         vcode_reg_t values, vcode_reg_t reject,
                         vcode_reg_t after);
void emit_force(vcode_reg_t nets, vcode_reg_t nnets, vcode_reg_t values);
void emit_release(vcode_reg_t nets, vcode_reg_t nnets);
void emit_disconnect(vcode_reg_t nets, vcode_reg_t nnets, vcode_reg_t reject,
                     vcode_reg_t after);
void emit_cond(vcode_reg_t test, vcode_block_t btrue, vcode_block_t bfalse);
vcode_reg_t emit_neg(vcode_reg_t lhs);
vcode_reg_t emit_trap_neg(vcode_reg_t lhs, vcode_reg_t locus);
vcode_reg_t emit_abs(vcode_reg_t lhs);
void emit_comment(const char *fmt, ...) __attribute__((format(printf, 1, 2)));
vcode_reg_t emit_select(vcode_reg_t test, vcode_reg_t rtrue,
                        vcode_reg_t rfalse);
vcode_reg_t emit_or(vcode_reg_t lhs, vcode_reg_t rhs);
vcode_reg_t emit_and(vcode_reg_t lhs, vcode_reg_t rhs);
vcode_reg_t emit_xor(vcode_reg_t lhs, vcode_reg_t rhs);
vcode_reg_t emit_xnor(vcode_reg_t lhs, vcode_reg_t rhs);
vcode_reg_t emit_nand(vcode_reg_t lhs, vcode_reg_t rhs);
vcode_reg_t emit_nor(vcode_reg_t lhs, vcode_reg_t rhs);
vcode_reg_t emit_wrap(vcode_reg_t data, const vcode_dim_t *dims, int ndims);
vcode_reg_t emit_uarray_left(vcode_reg_t array, unsigned dim);
vcode_reg_t emit_uarray_right(vcode_reg_t array, unsigned dim);
vcode_reg_t emit_uarray_dir(vcode_reg_t array, unsigned dim);
vcode_reg_t emit_uarray_len(vcode_reg_t array, unsigned dim);
vcode_reg_t emit_unwrap(vcode_reg_t array);
vcode_reg_t emit_not(vcode_reg_t arg);
vcode_reg_t emit_var_upref(int hops, vcode_var_t var);
vcode_reg_t emit_init_signal(vcode_type_t type, vcode_reg_t count,
                             vcode_reg_t size, vcode_reg_t value,
                             vcode_reg_t flags, vcode_reg_t locus,
                             vcode_reg_t offset);
void emit_resolve_signal(vcode_reg_t signal, vcode_reg_t resolution);
vcode_reg_t emit_implicit_signal(vcode_type_t type, vcode_reg_t count,
                                 vcode_reg_t size, vcode_reg_t locus,
                                 vcode_reg_t kind, vcode_reg_t closure,
                                 vcode_reg_t delay);
vcode_reg_t emit_resolved(vcode_reg_t sig, vcode_reg_t count);
vcode_reg_t emit_last_value(vcode_reg_t sig, vcode_reg_t count);
vcode_reg_t emit_event_flag(vcode_reg_t nets, vcode_reg_t len);
vcode_reg_t emit_active_flag(vcode_reg_t nets, vcode_reg_t len);
vcode_reg_t emit_record_ref(vcode_reg_t record, unsigned field);
vcode_reg_t emit_array_ref(vcode_reg_t array, vcode_reg_t offset);
vcode_reg_t emit_table_ref(vcode_reg_t array, vcode_reg_t stride,
                           const vcode_reg_t *args, int nargs);
void emit_copy(vcode_reg_t dest, vcode_reg_t src, vcode_reg_t count);
void emit_sched_event(vcode_reg_t nets, vcode_reg_t n_elems);
void emit_clear_event(vcode_reg_t nets, vcode_reg_t count);
void emit_sched_process(vcode_reg_t delay);
void emit_resume(ident_t func);
void emit_memset(vcode_reg_t ptr, vcode_reg_t value, vcode_reg_t len);
void emit_case(vcode_reg_t value, vcode_block_t def, const vcode_reg_t *cases,
               const vcode_block_t *blocks, int ncases);
void emit_file_open(vcode_reg_t file, vcode_reg_t name, vcode_reg_t length,
                    vcode_reg_t kind, vcode_reg_t status);
void emit_file_write(vcode_reg_t file, vcode_reg_t value, vcode_reg_t length);
void emit_file_read(vcode_reg_t file, vcode_reg_t ptr,
                    vcode_reg_t inlen, vcode_reg_t outlen);
vcode_reg_t emit_null(vcode_type_t type);
vcode_reg_t emit_new(vcode_type_t type, vcode_reg_t length);
void emit_null_check(vcode_reg_t ptr, vcode_reg_t locus);
void emit_deallocate(vcode_reg_t ptr);
vcode_reg_t emit_all(vcode_reg_t reg);
vcode_reg_t emit_last_event(vcode_reg_t signal, vcode_reg_t len);
vcode_reg_t emit_last_active(vcode_reg_t signal, vcode_reg_t len);
vcode_reg_t emit_driving_flag(vcode_reg_t signal, vcode_reg_t len);
vcode_reg_t emit_driving_value(vcode_reg_t signal, vcode_reg_t len);
void emit_length_check(vcode_reg_t llen, vcode_reg_t rlen, vcode_reg_t locus,
                       vcode_reg_t dim);
void emit_exponent_check(vcode_reg_t exp, vcode_reg_t locus);
void emit_zero_check(vcode_reg_t denom, vcode_reg_t locus);
void emit_debug_out(vcode_reg_t reg);
void emit_cover_stmt(uint32_t tag);
void emit_cover_branch(uint32_t tag);
void emit_cover_toggle(vcode_reg_t signal, uint32_t tag);
void emit_cover_state(vcode_reg_t signal, vcode_reg_t low, uint32_t tag);
void emit_cover_expr(uint32_t tag);
vcode_reg_t emit_undefined(vcode_type_t type, vcode_stamp_t stamp);
void emit_debug_info(const loc_t *loc);
vcode_reg_t emit_range_null(vcode_reg_t left, vcode_reg_t right,
                            vcode_reg_t dir);
vcode_reg_t emit_range_length(vcode_reg_t left, vcode_reg_t right,
                              vcode_reg_t dir);
vcode_reg_t emit_link_var(vcode_reg_t context, ident_t name, vcode_type_t type);
vcode_reg_t emit_link_package(ident_t name);
void emit_map_signal(vcode_reg_t src, vcode_reg_t dst, vcode_reg_t count);
void emit_map_const(vcode_reg_t src, vcode_reg_t dst, vcode_reg_t count);
void emit_map_implicit(vcode_reg_t src, vcode_reg_t dst, vcode_reg_t count);
void emit_drive_signal(vcode_reg_t target, vcode_reg_t count);
void emit_transfer_signal(vcode_reg_t target, vcode_reg_t source,
                          vcode_reg_t count, vcode_reg_t reject,
                          vcode_reg_t after);
vcode_reg_t emit_resolution_wrapper(vcode_type_t type, vcode_reg_t closure,
                                    vcode_reg_t nlits);
vcode_reg_t emit_closure(ident_t func, vcode_reg_t context, vcode_type_t rtype);
vcode_reg_t emit_protected_init(vcode_type_t type, vcode_reg_t context,
                                vcode_reg_t path_name, vcode_reg_t inst_name);
void emit_process_init(ident_t name, vcode_reg_t locus);
vcode_reg_t emit_package_init(ident_t name, vcode_reg_t context);
void emit_protected_free(vcode_reg_t obj);
vcode_reg_t emit_context_upref(int hops);
vcode_reg_t emit_debug_locus(object_t *obj);
void emit_package_scope(vcode_reg_t locus);
void emit_array_scope(vcode_reg_t locus, vcode_type_t type);
void emit_record_scope(vcode_reg_t locus, vcode_type_t type);
void emit_pop_scope(void);
void emit_alias_signal(vcode_reg_t signal, vcode_reg_t locus);
void emit_unreachable(vcode_reg_t locus);
void emit_enter_state(vcode_reg_t state, vcode_reg_t strong);
vcode_reg_t emit_reflect_value(vcode_reg_t value, vcode_reg_t context,
                               vcode_reg_t locus, vcode_reg_t bounds);
vcode_reg_t emit_reflect_subtype(vcode_reg_t context, vcode_reg_t locus,
                                 vcode_reg_t bounds);
vcode_reg_t emit_function_trigger(ident_t func, const vcode_reg_t *args,
                                  int nargs);
vcode_reg_t emit_or_trigger(vcode_reg_t left, vcode_reg_t right);
vcode_reg_t emit_cmp_trigger(vcode_reg_t left, vcode_reg_t right);
void emit_add_trigger(vcode_reg_t trigger);
vcode_reg_t emit_port_conversion(vcode_reg_t driving, vcode_reg_t effective);
void emit_convert_in(vcode_reg_t conv, vcode_reg_t nets, vcode_reg_t count);
void emit_convert_out(vcode_reg_t conv, vcode_reg_t nets, vcode_reg_t count);
void emit_bind_foreign(vcode_reg_t spec, vcode_reg_t length, vcode_reg_t locus);
vcode_reg_t emit_instance_name(vcode_reg_t kind);
vcode_reg_t emit_bind_external(vcode_reg_t locus, ident_t scope,
                               vcode_type_t type, vcode_stamp_t bounds,
                               const vcode_reg_t *args, int nargs);
void emit_put_conversion(vcode_reg_t cf, vcode_reg_t target, vcode_reg_t count,
                         vcode_reg_t values);

#endif  // _VCODE_H
