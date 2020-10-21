//
//  Copyright (C) 2014-2020  Nick Gasson
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
typedef int32_t vcode_block_t;
typedef int32_t vcode_var_t;
typedef int32_t vcode_reg_t;
typedef int32_t vcode_signal_t;

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
   VCODE_OP_REPORT,
   VCODE_OP_DIV,
   VCODE_OP_NEG,
   VCODE_OP_EXP,
   VCODE_OP_ABS,
   VCODE_OP_MOD,
   VCODE_OP_REM,
   VCODE_OP_IMAGE,
   VCODE_OP_ALLOCA,
   VCODE_OP_SELECT,
   VCODE_OP_OR,
   VCODE_OP_WRAP,
   VCODE_OP_UARRAY_LEFT,
   VCODE_OP_UARRAY_RIGHT,
   VCODE_OP_UARRAY_DIR,
   VCODE_OP_UNWRAP,
   VCODE_OP_NOT,
   VCODE_OP_AND,
   VCODE_OP_NESTED_FCALL,
   VCODE_OP_PARAM_UPREF,
   VCODE_OP_RESOLVED_ADDRESS,
   VCODE_OP_SET_INITIAL,
   VCODE_OP_ALLOC_DRIVER,
   VCODE_OP_EVENT,
   VCODE_OP_ACTIVE,
   VCODE_OP_CONST_RECORD,
   VCODE_OP_RECORD_REF,
   VCODE_OP_COPY,
   VCODE_OP_SCHED_EVENT,
   VCODE_OP_PCALL,
   VCODE_OP_RESUME,
   VCODE_OP_MEMCMP,
   VCODE_OP_XOR,
   VCODE_OP_XNOR,
   VCODE_OP_NAND,
   VCODE_OP_NOR,
   VCODE_OP_MEMSET,
   VCODE_OP_VEC_LOAD,
   VCODE_OP_CASE,
   VCODE_OP_ENDFILE,
   VCODE_OP_FILE_OPEN,
   VCODE_OP_FILE_WRITE,
   VCODE_OP_FILE_CLOSE,
   VCODE_OP_FILE_READ,
   VCODE_OP_NULL,
   VCODE_OP_NEW,
   VCODE_OP_NULL_CHECK,
   VCODE_OP_DEALLOCATE,
   VCODE_OP_ALL,
   VCODE_OP_BIT_VEC_OP,
   VCODE_OP_CONST_REAL,
   VCODE_OP_VALUE,
   VCODE_OP_LAST_EVENT,
   VCODE_OP_NEEDS_LAST_VALUE,
   VCODE_OP_DYNAMIC_BOUNDS,
   VCODE_OP_ARRAY_SIZE,
   VCODE_OP_INDEX_CHECK,
   VCODE_OP_BIT_SHIFT,
   VCODE_OP_STORAGE_HINT,
   VCODE_OP_DEBUG_OUT,
   VCODE_OP_NESTED_PCALL,
   VCODE_OP_COVER_STMT,
   VCODE_OP_COVER_COND,
   VCODE_OP_UARRAY_LEN,
   VCODE_OP_HEAP_SAVE,
   VCODE_OP_HEAP_RESTORE,
   VCODE_OP_NESTED_RESUME,
   VCODE_OP_UNDEFINED,
   VCODE_OP_IMAGE_MAP,
   VCODE_OP_DEBUG_INFO,
   VCODE_OP_ADDI,
   VCODE_OP_RANGE_NULL,
   VCODE_OP_VAR_UPREF
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
   VCODE_TYPE_IMAGE_MAP
} vtype_kind_t;

typedef enum {
   VCODE_UNIT_PROCESS,
   VCODE_UNIT_CONTEXT,
   VCODE_UNIT_FUNCTION,
   VCODE_UNIT_PROCEDURE,
   VCODE_UNIT_THUNK
} vunit_kind_t;

typedef enum {
   VCODE_ALLOCA_STACK,
   VCODE_ALLOCA_HEAP
} vcode_alloca_t;

typedef struct {
   vcode_reg_t left;
   vcode_reg_t right;
   vcode_reg_t dir;
} vcode_dim_t;

typedef struct {
   vcode_unit_t  unit;
   vcode_block_t block;
} vcode_state_t;

typedef struct {
   ident_t      name;
   image_kind_t kind;
   ident_t     *elems;
   int64_t     *values;
   size_t       nelems;
} image_map_t;

typedef enum {
   RES_SCALAR,
   RES_RECORD,
} res_kind_t;

typedef struct {
   ident_t      name;
   vcode_type_t type;
   int32_t      ileft;
   res_kind_t   kind;
   bool         boundary;
} vcode_res_elem_t;

typedef struct {
   size_t count;
   vcode_res_elem_t element[];
} vcode_res_fn_t;

#define VCODE_INVALID_REG    -1
#define VCODE_INVALID_BLOCK  -1
#define VCODE_INVALID_VAR    -1
#define VCODE_INVALID_SIGNAL -1
#define VCODE_INVALID_TYPE   -1
#define VCODE_INVALID_IMAGE  -1

#define VCODE_INVALID_HINT 0xffffffff

vcode_type_t vtype_int(int64_t low, int64_t high);
vcode_type_t vtype_dynamic(vcode_reg_t low, vcode_reg_t high);
vcode_type_t vtype_bool(void);
vcode_type_t vtype_carray(int size, vcode_type_t elem, vcode_type_t bounds);
vcode_type_t vtype_uarray(int ndim, vcode_type_t elem, vcode_type_t bounds);
vcode_type_t vtype_pointer(vcode_type_t to);
vcode_type_t vtype_access(vcode_type_t to);
vcode_type_t vtype_signal(vcode_type_t base);
vcode_type_t vtype_offset(void);
vcode_type_t vtype_time(void);
vcode_type_t vtype_char(void);
vcode_type_t vtype_image_map(void);
vcode_type_t vtype_find_named_record(ident_t name);
vcode_type_t vtype_named_record(ident_t name, const vcode_type_t *field_types,
                                int nfields);
vcode_type_t vtype_file(vcode_type_t base);
bool vtype_eq(vcode_type_t a, vcode_type_t b);
bool vtype_includes(vcode_type_t type, vcode_type_t bounds);
vtype_kind_t vtype_kind(vcode_type_t type);
int64_t vtype_low(vcode_type_t type);
int64_t vtype_high(vcode_type_t type);
vcode_type_t vtype_elem(vcode_type_t type);
vcode_type_t vtype_pointed(vcode_type_t type);
vcode_type_t vtype_bounds(vcode_type_t type);
unsigned vtype_dims(vcode_type_t type);
unsigned vtype_size(vcode_type_t type);
int vtype_fields(vcode_type_t type);
vcode_type_t vtype_field(vcode_type_t type, int field);
vcode_type_t vtype_base(vcode_type_t type);
ident_t vtype_record_name(vcode_type_t type);
vcode_type_t vtype_real(void);
vcode_unit_t vcode_find_unit(ident_t name);
vcode_unit_t vcode_unit_next(vcode_unit_t unit);
vcode_unit_t vcode_unit_child(vcode_unit_t unit);
void vcode_unit_unref(vcode_unit_t unit);

void vcode_opt(void);
void vcode_close(void);
void vcode_dump(void);
void vcode_dump_with_mark(int mark_op);
void vcode_select_unit(vcode_unit_t vu);
void vcode_select_block(vcode_block_t block);
int vcode_count_blocks(void);
const loc_t *vcode_last_loc(void);
const char *vcode_op_string(vcode_op_t op);
bool vcode_block_finished(void);
bool vcode_block_empty(void);
ident_t vcode_unit_name(void);
int vcode_unit_depth(void);
bool vcode_unit_pure(void);
bool vcode_unit_has_undefined(void);
vunit_kind_t vcode_unit_kind(void);
vcode_type_t vcode_unit_result(void);
vcode_block_t vcode_active_block(void);
vcode_unit_t vcode_active_unit(void);
vcode_unit_t vcode_unit_context(void);

void vcode_write(vcode_unit_t unit, fbuf_t *fbuf);
void vcode_read(fbuf_t *fbuf);

void vcode_state_save(vcode_state_t *state);
void vcode_state_restore(const vcode_state_t *state);

int vcode_count_params(void);
vcode_type_t vcode_param_type(int param);
vcode_reg_t vcode_param_reg(int param);

int vcode_count_regs(void);
vcode_type_t vcode_reg_type(vcode_reg_t reg);
vtype_kind_t vcode_reg_kind(vcode_reg_t reg);
vcode_type_t vcode_reg_bounds(vcode_reg_t reg);
bool vcode_reg_const(vcode_reg_t reg, int64_t *value);
void vcode_heap_allocate(vcode_reg_t reg);

int vcode_count_signals(void);
vcode_var_t vcode_signal_shadow(vcode_signal_t sig);
ident_t vcode_signal_name(vcode_signal_t sig);
size_t vcode_signal_count_nets(vcode_signal_t sig);
const netid_t *vcode_signal_nets(vcode_signal_t sig);
vcode_type_t vcode_signal_type(vcode_signal_t sig);
vcode_type_t vcode_signal_bounds(vcode_signal_t sig);
bool vcode_signal_extern(vcode_signal_t sig);

int vcode_count_ops(void);
vcode_op_t vcode_get_op(int op);
ident_t vcode_get_func(int op);
int64_t vcode_get_value(int op);
double vcode_get_real(int op);
vcode_cmp_t vcode_get_cmp(int op);
const loc_t *vcode_get_loc(int op);
const char *vcode_get_hint(int op);
vcode_block_t vcode_get_target(int op, int nth);
vcode_var_t vcode_get_address(int op);
int vcode_count_args(int op);
vcode_reg_t vcode_get_arg(int op, int arg);
vcode_type_t vcode_get_type(int op);
vcode_reg_t vcode_get_result(int op);
vcode_signal_t vcode_get_signal(int op);
unsigned vcode_get_dim(int op);
int vcode_get_hops(int op);
int vcode_get_field(int op);
unsigned vcode_get_subkind(int op);
uint32_t vcode_get_tag(int op);
void vcode_get_image_map(int op, image_map_t *map);
void vcode_clear_storage_hint(uint32_t tag);
const vcode_res_fn_t *vcode_get_resolution(int op);

int vcode_count_vars(void);
vcode_var_t vcode_find_var(ident_t name);
ident_t vcode_var_name(vcode_var_t var);
vcode_type_t vcode_var_type(vcode_var_t var);
bool vcode_var_extern(vcode_var_t var);
bool vcode_var_use_heap(vcode_var_t var);

vcode_unit_t emit_function(ident_t name, vcode_unit_t context,
                           vcode_type_t result);
vcode_unit_t emit_procedure(ident_t name, vcode_unit_t context);
vcode_unit_t emit_process(ident_t name, vcode_unit_t context);
vcode_unit_t emit_context(ident_t name);
vcode_unit_t emit_thunk(ident_t name, vcode_unit_t context, vcode_type_t type);
vcode_block_t emit_block(void);
vcode_var_t emit_var(vcode_type_t type, vcode_type_t bounds, ident_t name);
vcode_var_t emit_extern_var(vcode_type_t type, vcode_type_t bounds,
                            ident_t name);
vcode_signal_t emit_signal(vcode_type_t type, vcode_type_t bounds,
                           ident_t name, vcode_var_t shadow,
                           netid_t *nets, size_t nnets, bool is_extern);
vcode_reg_t emit_alloca(vcode_type_t type, vcode_type_t bounds,
                        vcode_reg_t count);
vcode_reg_t emit_param(vcode_type_t type, vcode_type_t bounds, ident_t name);
vcode_reg_t emit_const(vcode_type_t type, int64_t value);
vcode_reg_t emit_const_array(vcode_type_t type, vcode_reg_t *values, int num,
                             bool allocate);
vcode_reg_t emit_const_record(vcode_type_t type, vcode_reg_t *values, int num);
vcode_reg_t emit_const_real(double value);
vcode_reg_t emit_add(vcode_reg_t lhs, vcode_reg_t rhs);
vcode_reg_t emit_sub(vcode_reg_t lhs, vcode_reg_t rhs);
vcode_reg_t emit_mul(vcode_reg_t lhs, vcode_reg_t rhs);
vcode_reg_t emit_div(vcode_reg_t lhs, vcode_reg_t rhs);
vcode_reg_t emit_exp(vcode_reg_t lhs, vcode_reg_t rhs);
vcode_reg_t emit_mod(vcode_reg_t lhs, vcode_reg_t rhs);
vcode_reg_t emit_rem(vcode_reg_t lhs, vcode_reg_t rhs);
vcode_reg_t emit_addi(vcode_reg_t lhs, int64_t rhs);
void emit_assert(vcode_reg_t value, vcode_reg_t message, vcode_reg_t length,
                 vcode_reg_t severity);
void emit_report(vcode_reg_t message, vcode_reg_t length, vcode_reg_t severity);
vcode_reg_t emit_cmp(vcode_cmp_t cmp, vcode_reg_t lhs, vcode_reg_t rhs);
vcode_reg_t emit_fcall(ident_t func, vcode_type_t type,
                       const vcode_reg_t *args, int nargs);
void emit_pcall(ident_t func, const vcode_reg_t *args, int nargs,
                vcode_block_t resume_bb);
vcode_reg_t emit_nested_fcall(ident_t func, vcode_type_t type,
                              const vcode_reg_t *args, int nargs, int hops);
void emit_wait(vcode_block_t target, vcode_reg_t time);
void emit_jump(vcode_block_t target);
vcode_reg_t emit_load(vcode_var_t var);
vcode_reg_t emit_load_indirect(vcode_reg_t reg);
void emit_store(vcode_reg_t reg, vcode_var_t var);
void emit_store_indirect(vcode_reg_t reg, vcode_reg_t ptr);
void emit_bounds(vcode_reg_t reg, vcode_type_t bounds, bounds_kind_t kind,
                 const char *hint);
void emit_dynamic_bounds(vcode_reg_t reg, vcode_reg_t low, vcode_reg_t high,
                         vcode_reg_t kind, const char *hint);
void emit_index_check(vcode_reg_t rlow, vcode_reg_t rhigh, vcode_type_t bounds,
                      bounds_kind_t kind);
void emit_dynamic_index_check(vcode_reg_t rlow, vcode_reg_t rhigh,
                              vcode_reg_t blow, vcode_reg_t bhigh,
                              bounds_kind_t kind);
vcode_reg_t emit_index(vcode_var_t var, vcode_reg_t offset);
vcode_reg_t emit_cast(vcode_type_t type, vcode_reg_t bounds, vcode_reg_t reg);
void emit_return(vcode_reg_t reg);
vcode_reg_t emit_nets(vcode_signal_t sig);
void emit_sched_waveform(vcode_reg_t nets, vcode_reg_t nnets,
                         vcode_reg_t values, vcode_reg_t reject,
                         vcode_reg_t after);
void emit_cond(vcode_reg_t test, vcode_block_t btrue, vcode_block_t bfalse);
vcode_reg_t emit_neg(vcode_reg_t lhs);
vcode_reg_t emit_abs(vcode_reg_t lhs);
vcode_reg_t emit_image(vcode_reg_t value, vcode_reg_t map);
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
vcode_reg_t emit_array_cmp(vcode_cmp_t cmp, vcode_reg_t lhs, vcode_reg_t rhs);
vcode_reg_t emit_not(vcode_reg_t arg);
vcode_reg_t emit_param_upref(int hops, vcode_reg_t reg);
vcode_reg_t emit_var_upref(int hops, vcode_var_t var);
void emit_resolved_address(vcode_var_t var, vcode_signal_t signal);
void emit_set_initial(vcode_signal_t signal, vcode_reg_t value,
                      vcode_res_fn_t *resolution);
void emit_alloc_driver(vcode_reg_t all_nets, vcode_reg_t all_length,
                       vcode_reg_t driven_nets, vcode_reg_t driven_length,
                       vcode_reg_t init);
vcode_reg_t emit_event_flag(vcode_reg_t nets, vcode_reg_t len);
vcode_reg_t emit_active_flag(vcode_reg_t nets, vcode_reg_t len);
vcode_reg_t emit_record_ref(vcode_reg_t record, unsigned field);
void emit_copy(vcode_reg_t dest, vcode_reg_t src, vcode_reg_t count);
void emit_sched_event(vcode_reg_t nets, vcode_reg_t n_elems, unsigned flags);
void emit_resume(ident_t func);
void emit_nested_resume(ident_t func, int hops);
vcode_reg_t emit_memcmp(vcode_reg_t lhs, vcode_reg_t rhs, vcode_reg_t len);
void emit_memset(vcode_reg_t ptr, vcode_reg_t value, vcode_reg_t len);
vcode_reg_t emit_vec_load(vcode_reg_t signal, vcode_reg_t length,
                          bool last_value);
void emit_case(vcode_reg_t value, vcode_block_t def, const vcode_reg_t *cases,
               const vcode_block_t *blocks, int ncases);
vcode_reg_t emit_endfile(vcode_reg_t file);
void emit_file_open(vcode_reg_t file, vcode_reg_t name, vcode_reg_t length,
                    vcode_reg_t kind, vcode_reg_t status);
void emit_file_write(vcode_reg_t file, vcode_reg_t value, vcode_reg_t length);
void emit_file_close(vcode_reg_t file);
void emit_file_read(vcode_reg_t file, vcode_reg_t ptr,
                    vcode_reg_t inlen, vcode_reg_t outlen);
vcode_reg_t emit_null(vcode_type_t type);
vcode_reg_t emit_new(vcode_type_t type, vcode_reg_t length);
void emit_null_check(vcode_reg_t ptr);
void emit_deallocate(vcode_reg_t ptr);
vcode_reg_t emit_all(vcode_reg_t reg);
vcode_reg_t emit_bit_vec_op(bit_vec_op_kind_t kind, vcode_reg_t lhs_data,
                            vcode_reg_t lhs_len, vcode_reg_t lhs_dir,
                            vcode_reg_t rhs_data, vcode_reg_t rhs_len,
                            vcode_reg_t rhs_dir, vcode_type_t result);
vcode_reg_t emit_value(vcode_reg_t string, vcode_reg_t len, vcode_reg_t map);
vcode_reg_t emit_last_event(vcode_reg_t signal, vcode_reg_t len);
void emit_needs_last_value(vcode_signal_t sig);
void emit_array_size(vcode_reg_t llen, vcode_reg_t rlen);
vcode_reg_t emit_bit_shift(bit_shift_kind_t kind, vcode_reg_t data,
                           vcode_reg_t len, vcode_reg_t dir, vcode_reg_t shift,
                           vcode_type_t result);
uint32_t emit_storage_hint(vcode_reg_t mem, vcode_reg_t length);
void emit_debug_out(vcode_reg_t reg);
void emit_nested_pcall(ident_t func, const vcode_reg_t *args, int nargs,
                       vcode_block_t resume_bb, int hops);
void emit_cover_stmt(uint32_t tag);
void emit_cover_cond(vcode_reg_t test, uint32_t tag, unsigned sub);
vcode_reg_t emit_heap_save(void);
void emit_heap_restore(vcode_reg_t reg);
vcode_reg_t emit_undefined(vcode_type_t type);
vcode_reg_t emit_enum_map(ident_t name, size_t nelems, const ident_t *elems);
vcode_reg_t emit_physical_map(ident_t name, size_t nelems,
                              const ident_t *elems, const int64_t *values);
void emit_debug_info(const loc_t *loc);
vcode_reg_t emit_range_null(vcode_reg_t left, vcode_reg_t right,
                            vcode_reg_t dir);

#endif  // _VCODE_H
