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

#ifndef _MIR_NODE_H
#define _MIR_NODE_H

#include "prim.h"
#include "diag.h"
#include "util.h"

#include <limits.h>

#define MIR_TAG_NULL    0
#define MIR_TAG_BLOCK   1
#define MIR_TAG_PARAM   2
#define MIR_TAG_NODE    3
#define MIR_TAG_COMMENT 4
#define MIR_TAG_TYPE    5
#define MIR_TAG_CONST   6
#define MIR_TAG_STAMP   7
#define MIR_TAG_VAR     8
#define MIR_TAG_ENUM    9
#define MIR_TAG_LINKAGE 10
#define MIR_TAG_EXTVAR  11

#define _MIR_TAG_BITS   4
#define _MIR_ID_BITS    28

STATIC_ASSERT(_MIR_TAG_BITS + _MIR_ID_BITS == 32);

#define MIR_ID_MAX ((1 << _MIR_ID_BITS) - 1)

typedef union _mir_block {
   struct {
      unsigned tag : _MIR_TAG_BITS;
      unsigned id : _MIR_ID_BITS;
   };
   uint32_t bits;
} mir_block_t;

STATIC_ASSERT(sizeof(mir_block_t) == 4);

#define MIR_NULL_BLOCK ((mir_block_t){ .tag = MIR_TAG_NULL })

typedef union _mir_value {
   struct {
      unsigned tag : _MIR_TAG_BITS;
      unsigned id : _MIR_ID_BITS;
   };
   uint32_t bits;
} mir_value_t;

STATIC_ASSERT(sizeof(mir_value_t) == 4);

#define MIR_NULL_VALUE ((mir_value_t){ .tag = MIR_TAG_NULL })

typedef union {
   struct {
      unsigned tag : _MIR_TAG_BITS;
      unsigned id : _MIR_ID_BITS;
   };
   uint32_t bits;
} mir_type_t;

STATIC_ASSERT(sizeof(mir_type_t) == 4);

#define MIR_NULL_TYPE ((mir_type_t){ .tag = MIR_TAG_NULL })

typedef union {
   struct {
      unsigned tag : _MIR_TAG_BITS;
      unsigned id : _MIR_ID_BITS;
   };
   uint32_t bits;
} mir_stamp_t;

STATIC_ASSERT(sizeof(mir_stamp_t) == 4);

#define MIR_NULL_STAMP ((mir_stamp_t){ .tag = MIR_TAG_NULL })

#define mir_is_null(o) ((o).tag == MIR_TAG_NULL)
#define mir_equals(a, b) ((a).bits == (b).bits)

#define mir_cast_value(p) ((mir_value_t){ .bits = (p).bits })

#define mir_cast_block(p) ({                    \
         assert((p).tag == MIR_TAG_BLOCK);      \
         (mir_block_t){ .bits = (p).bits };     \
      })

#define mir_cast_type(p) ({                     \
         assert((p).tag == MIR_TAG_TYPE);       \
         (mir_type_t){ .bits = (p).bits };      \
      })

typedef struct {
   uint16_t first;
   uint16_t width;
} mir_vreg_t;

STATIC_ASSERT(sizeof(mir_vreg_t) == 4);

#define MIR_VREG_MAX UINT16_MAX

typedef enum {
   MIR_CMP_EQ,
   MIR_CMP_NEQ,
   MIR_CMP_LT,
   MIR_CMP_GT,
   MIR_CMP_LEQ,
   MIR_CMP_GEQ,
} mir_cmp_t;

typedef enum {
   _MIR_DELETED_OP,
   MIR_OP_ADD,
   MIR_OP_RETURN,
   MIR_OP_COMMENT,
   MIR_OP_CONST,
   MIR_OP_STORE,
   MIR_OP_LOAD,
   MIR_OP_CONST_REAL,
   MIR_OP_JUMP,
   MIR_OP_COND,
   MIR_OP_CMP,
   MIR_OP_PHI,
   MIR_OP_AND,
   MIR_OP_SUB,
   MIR_OP_MUL,
   MIR_OP_DIV,
   MIR_OP_REM,
   MIR_OP_MOD,
   MIR_OP_EXP,
   MIR_OP_OR,
   MIR_OP_XOR,
   MIR_OP_CONSUME,
   MIR_OP_FCALL,
   MIR_OP_PCALL,
   MIR_OP_RESUME,
   MIR_OP_WRAP,
   MIR_OP_UNWRAP,
   MIR_OP_NOT,
   MIR_OP_RESOLVED,
   MIR_OP_LAST_VALUE,
   MIR_OP_LOCUS,
   MIR_OP_INIT_SIGNAL,
   MIR_OP_IMPLICIT_SIGNAL,
   MIR_OP_SELECT,
   MIR_OP_WAIT,
   MIR_OP_LINK_PACKAGE,
   MIR_OP_PACKAGE_INIT,
   MIR_OP_PROTECTED_INIT,
   MIR_OP_ASSERT,
   MIR_OP_CONTEXT_UPREF,
   MIR_OP_UARRAY_LEN,
   MIR_OP_UARRAY_LEFT,
   MIR_OP_UARRAY_RIGHT,
   MIR_OP_UARRAY_DIR,
   MIR_OP_TRAP_ADD,
   MIR_OP_TRAP_SUB,
   MIR_OP_TRAP_MUL,
   MIR_OP_TRAP_EXP,
   MIR_OP_CONST_ARRAY,
   MIR_OP_CONST_REP,
   MIR_OP_CONST_RECORD,
   MIR_OP_CONST_VEC,
   MIR_OP_ADDRESS_OF,
   MIR_OP_ARRAY_REF,
   MIR_OP_VAR_UPREF,
   MIR_OP_DRIVE_SIGNAL,
   MIR_OP_SCHED_WAVEFORM,
   MIR_OP_EVENT,
   MIR_OP_ACTIVE,
   MIR_OP_CAST,
   MIR_OP_NEG,
   MIR_OP_TRAP_NEG,
   MIR_OP_ABS,
   MIR_OP_COPY,
   MIR_OP_REPORT,
   MIR_OP_RANGE_CHECK,
   MIR_OP_INDEX_CHECK,
   MIR_OP_SCHED_EVENT,
   MIR_OP_CLEAR_EVENT,
   MIR_OP_ALLOC,
   MIR_OP_RANGE_LENGTH,
   MIR_OP_RANGE_NULL,
   MIR_OP_SET,
   MIR_OP_NULL,
   MIR_OP_NEW,
   MIR_OP_ALL,
   MIR_OP_NULL_CHECK,
   MIR_OP_LENGTH_CHECK,
   MIR_OP_ZERO_CHECK,
   MIR_OP_EXPONENT_CHECK,
   MIR_OP_ALIAS_SIGNAL,
   MIR_OP_MAP_SIGNAL,
   MIR_OP_MAP_CONST,
   MIR_OP_MAP_IMPLICIT,
   MIR_OP_CASE,
   MIR_OP_BIND_FOREIGN,
   MIR_OP_BIND_EXTERNAL,
   MIR_OP_UNREACHABLE,
   MIR_OP_RECORD_REF,
   MIR_OP_CLOSURE,
   MIR_OP_RESOLUTION_WRAPPER,
   MIR_OP_RESOLVE_SIGNAL,
   MIR_OP_TRANSFER_SIGNAL,
   MIR_OP_FILE_OPEN,
   MIR_OP_FILE_READ,
   MIR_OP_FILE_WRITE,
   MIR_OP_PORT_CONVERSION,
   MIR_OP_CONVERT_IN,
   MIR_OP_CONVERT_OUT,
   MIR_OP_PUT_CONVERSION,
   MIR_OP_LINK_VAR,
   MIR_OP_DRIVING_VALUE,
   MIR_OP_FORCE,
   MIR_OP_RELEASE,
   MIR_OP_DISCONNECT,
   MIR_OP_PROCESS_INIT,
   MIR_OP_COVER_STMT,
   MIR_OP_COVER_BRANCH,
   MIR_OP_COVER_TOGGLE,
   MIR_OP_COVER_EXPR,
   MIR_OP_COVER_STATE,
   MIR_OP_RECORD_SCOPE,
   MIR_OP_ARRAY_SCOPE,
   MIR_OP_PACKAGE_SCOPE,
   MIR_OP_POP_SCOPE,
   MIR_OP_FUNCTION_TRIGGER,
   MIR_OP_OR_TRIGGER,
   MIR_OP_CMP_TRIGGER,
   MIR_OP_LEVEL_TRIGGER,
   MIR_OP_ADD_TRIGGER,
   MIR_OP_INSTANCE_NAME,
   MIR_OP_LAST_EVENT,
   MIR_OP_LAST_ACTIVE,
   MIR_OP_DRIVING,
   MIR_OP_ENTER_STATE,
   MIR_OP_DEPOSIT_SIGNAL,
   MIR_OP_SYSCALL,
   MIR_OP_REFLECT_VALUE,
   MIR_OP_REFLECT_SUBTYPE,
   MIR_OP_DEBUG_OUT,
   MIR_OP_PACK,
   MIR_OP_UNPACK,
   MIR_OP_BINARY,
   MIR_OP_UNARY,
   MIR_OP_DIR_CHECK,
   MIR_OP_INSERT,
   MIR_OP_TEST,
   MIR_OP_EXTRACT,
   MIR_OP_SCHED_PROCESS,
   MIR_OP_SCHED_INACTIVE,
   MIR_OP_SCHED_DEPOSIT,
   MIR_OP_PUT_DRIVER,
   MIR_OP_TABLE_REF,
} mir_op_t;

typedef enum {
   _MIR_INVALID_TYPE,
   MIR_TYPE_INT,
   MIR_TYPE_OFFSET,
   MIR_TYPE_POINTER,
   MIR_TYPE_REAL,
   MIR_TYPE_CARRAY,
   MIR_TYPE_UARRAY,
   MIR_TYPE_SIGNAL,
   MIR_TYPE_LOCUS,
   MIR_TYPE_CONTEXT,
   MIR_TYPE_CLOSURE,
   MIR_TYPE_RESOLUTION,
   MIR_TYPE_RECORD,
   MIR_TYPE_ACCESS,
   MIR_TYPE_FILE,
   MIR_TYPE_TRIGGER,
   MIR_TYPE_CONVERSION,
   MIR_TYPE_OPAQUE,
   MIR_TYPE_VEC2,
   MIR_TYPE_VEC4,
} mir_class_t;

typedef struct {
   mir_value_t left;
   mir_value_t right;
   mir_value_t dir;
} mir_dim_t;

typedef enum {
   MIR_VAR_TEMP   = (1 << 1),
   MIR_VAR_HEAP   = (1 << 2),
   MIR_VAR_CONST  = (1 << 3),
   MIR_VAR_SIGNAL = (1 << 4)
} mir_var_flags_t;

typedef enum {
   MIR_REPR_U1,
   MIR_REPR_I8,
   MIR_REPR_U8,
   MIR_REPR_I16,
   MIR_REPR_U16,
   MIR_REPR_I32,
   MIR_REPR_U32,
   MIR_REPR_I64,
   MIR_REPR_U64,
} mir_repr_t;

typedef enum {
   MIR_MEM_NONE,
   MIR_MEM_NULL,
   MIR_MEM_CONST,
   MIR_MEM_STACK,
   MIR_MEM_LOCAL,
   MIR_MEM_GLOBAL,
   MIR_MEM_TOP,
} mir_mem_t;

typedef enum {
   MIR_VEC_BIT_AND,
   MIR_VEC_BIT_OR,
   MIR_VEC_BIT_XOR,
   MIR_VEC_BIT_NOT,
   MIR_VEC_LOG_AND,
   MIR_VEC_LOG_OR,
   MIR_VEC_LOG_NOT,
   MIR_VEC_LT,
   MIR_VEC_LEQ,
   MIR_VEC_GT,
   MIR_VEC_GEQ,
   MIR_VEC_LOG_EQ,
   MIR_VEC_LOG_NEQ,
   MIR_VEC_CASE_EQ,
   MIR_VEC_CASE_NEQ,
   MIR_VEC_CASEX_EQ,
   MIR_VEC_ADD,
   MIR_VEC_SUB,
   MIR_VEC_MUL,
   MIR_VEC_DIV,
   MIR_VEC_MOD,
   MIR_VEC_SLL,
   MIR_VEC_SRL,
   MIR_VEC_SRA,
   MIR_VEC_EXP,
} mir_vec_op_t;

#define MIR_APPEND UINT_MAX

mir_type_t mir_int_type(mir_unit_t *mu, int64_t low, int64_t high);
mir_type_t mir_real_type(mir_unit_t *mu, double low, double high);
mir_type_t mir_offset_type(mir_unit_t *mu);
mir_type_t mir_locus_type(mir_unit_t *mu);
mir_type_t mir_conversion_type(mir_unit_t *mu);
mir_type_t mir_trigger_type(mir_unit_t *mu);
mir_type_t mir_self_type(mir_unit_t *mu);
mir_type_t mir_bool_type(mir_unit_t *mu);
mir_type_t mir_time_type(mir_unit_t *mu);
mir_type_t mir_logic_type(mir_unit_t *mu);
mir_type_t mir_double_type(mir_unit_t *mu);
mir_type_t mir_char_type(mir_unit_t *mu);
mir_type_t mir_string_type(mir_unit_t *mu);
mir_type_t mir_pointer_type(mir_unit_t *mu, mir_type_t to);
mir_type_t mir_access_type(mir_unit_t *mu, mir_type_t to);
mir_type_t mir_carray_type(mir_unit_t *mu, int size, mir_type_t elem);
mir_type_t mir_uarray_type(mir_unit_t *mu, int ndim, mir_type_t elem);
mir_type_t mir_signal_type(mir_unit_t *mu, mir_type_t base);
mir_type_t mir_context_type(mir_unit_t *mu, ident_t name);
mir_type_t mir_opaque_type(mir_unit_t *mu);
mir_type_t mir_record_type(mir_unit_t *mu, ident_t name,
                           const mir_type_t *fields, unsigned count);
mir_type_t mir_closure_type(mir_unit_t *mu, mir_type_t atype, mir_type_t rtype);
mir_type_t mir_resolution_type(mir_unit_t *mu, mir_type_t base);
mir_type_t mir_file_type(mir_unit_t *mu, mir_type_t base);
mir_type_t mir_vec2_type(mir_unit_t *mu, int size, bool issigned);
mir_type_t mir_vec4_type(mir_unit_t *mu, int size, bool issigned);

mir_type_t mir_vector_slice(mir_unit_t *mu, mir_type_t base, int size);
mir_type_t mir_get_base(mir_unit_t *mu, mir_type_t type);
mir_type_t mir_get_elem(mir_unit_t *mu, mir_type_t type);
mir_type_t mir_get_pointer(mir_unit_t *mu, mir_type_t type);
unsigned mir_get_dims(mir_unit_t *mu, mir_type_t type);
unsigned mir_get_size(mir_unit_t *mu, mir_type_t type);
unsigned mir_get_slots(mir_unit_t *mu, mir_type_t type);
bool mir_get_signed(mir_unit_t *mu, mir_type_t type);
mir_class_t mir_get_class(mir_unit_t *mu, mir_type_t type);
mir_repr_t mir_get_repr(mir_unit_t *mu, mir_type_t type);
const mir_type_t *mir_get_fields(mir_unit_t *mu, mir_type_t type,
                                 size_t *count);

mir_stamp_t mir_int_stamp(mir_unit_t *mu, int64_t low, int64_t high);
mir_stamp_t mir_real_stamp(mir_unit_t *mu, double low, double high);
mir_stamp_t mir_pointer_stamp(mir_unit_t *mu, mir_mem_t mem, mir_stamp_t elem);

mir_block_t mir_add_block(mir_unit_t *mu);
void mir_set_cursor(mir_unit_t *mu, mir_block_t block, unsigned pos);
mir_block_t mir_get_cursor(mir_unit_t *mu, unsigned *pos);
void mir_set_loc(mir_unit_t *mu, const loc_t *loc);
void mir_delete(mir_unit_t *mu);
void mir_compact(mir_unit_t *mu);
unsigned mir_count_blocks(mir_unit_t *mu);
mir_block_t mir_get_block(mir_unit_t *mu, unsigned nth);
unsigned mir_count_nodes(mir_unit_t *mu, mir_block_t block);
mir_value_t mir_get_node(mir_unit_t *mu, mir_block_t block, unsigned nth);
unsigned mir_count_vars(mir_unit_t *mu);
unsigned mir_count_params(mir_unit_t *mu);
unsigned mir_count_vregs(mir_unit_t *mu);

mir_op_t mir_get_op(mir_unit_t *mu, mir_value_t node);
unsigned mir_count_args(mir_unit_t *mu, mir_value_t node);
mir_value_t mir_get_arg(mir_unit_t *mu, mir_value_t node, unsigned nth);
const loc_t *mir_get_loc(mir_unit_t *mu, mir_value_t node);
mir_value_t mir_get_var(mir_unit_t *mu, unsigned nth);
mir_value_t mir_get_param(mir_unit_t *mu, unsigned nth);

mir_value_t mir_add_param(mir_unit_t *mu, mir_type_t type, mir_stamp_t stamp,
                          ident_t name);
mir_value_t mir_add_var(mir_unit_t *mu, mir_type_t type, mir_stamp_t stamp,
                        ident_t name, mir_var_flags_t flags);
bool mir_get_const(mir_unit_t *mu, mir_value_t value, int64_t *result);
bool mir_get_const_real(mir_unit_t *mu, mir_value_t value, double *result);
mir_type_t mir_get_type(mir_unit_t *mu, mir_value_t value);
mir_stamp_t mir_get_stamp(mir_unit_t *mu, mir_value_t value);
mir_mem_t mir_get_mem(mir_unit_t *mu, mir_value_t value);
ident_t mir_get_name(mir_unit_t *mu, mir_value_t value);
object_t *mir_get_locus(mir_unit_t *mu, mir_value_t value);
void mir_get_bits(mir_unit_t *mu, mir_value_t value, uint64_t *abits,
                  uint64_t *bbits);
mir_var_flags_t mir_get_var_flags(mir_unit_t *mu, mir_value_t value);
mir_type_t mir_get_var_type(mir_unit_t *mu, mir_value_t value);
void mir_set_input(mir_unit_t *mu, mir_value_t phi, unsigned nth,
                   mir_block_t block, mir_value_t value);
bool mir_block_finished(mir_unit_t *mu, mir_block_t block);
mir_vreg_t mir_get_vreg(mir_unit_t *mu, mir_value_t value);

void mir_set_result(mir_unit_t *mu, mir_type_t type);
mir_type_t mir_get_result(mir_unit_t *mu);

bool mir_is_integral(mir_unit_t *mu, mir_value_t value);
bool mir_is_numeric(mir_unit_t *mu, mir_value_t value);
bool mir_is_scalar(mir_unit_t *mu, mir_value_t value);
bool mir_is_vector(mir_unit_t *mu, mir_value_t value);
bool mir_is_bool(mir_unit_t *mu, mir_value_t value);
bool mir_is_time(mir_unit_t *mu, mir_value_t value);
bool mir_is_signal(mir_unit_t *mu, mir_value_t value);
bool mir_is_offset(mir_unit_t *mu, mir_value_t value);
bool mir_is(mir_unit_t *mu, mir_value_t value, mir_class_t class);
bool mir_points_to(mir_unit_t *mu, mir_value_t value, mir_class_t class);
bool mir_may_alias(mir_unit_t *mu, mir_value_t a, mir_value_t b);

const char *mir_op_string(mir_op_t op);

#ifdef DEBUG
__attribute__((format(printf, 2, 3)))
void mir_comment(mir_unit_t *mu, const char *fmt, ...);
#else
#define mir_comment(mu, fmt, ...)
#endif

typedef struct {
   mir_unit_t *mu;
   loc_t       loc;
} mir_saved_loc_t;

mir_saved_loc_t _mir_push_debug_info(mir_unit_t *mu, const loc_t *loc);

__attribute__((always_inline))
static inline void _mir_pop_debug_info(const mir_saved_loc_t *sl)
{
   mir_set_loc(sl->mu, &sl->loc);
}

// Constants
mir_value_t mir_enum(unsigned value);
mir_value_t mir_const(mir_unit_t *mu, mir_type_t type, int64_t value);
mir_value_t mir_const_real(mir_unit_t *mu, mir_type_t type, double value);
mir_value_t mir_const_vec(mir_unit_t *mu, mir_type_t type, uint64_t abits,
                          uint64_t bbits);
mir_value_t mir_const_array(mir_unit_t *mu, mir_type_t type,
                            const mir_value_t *values, size_t count);
mir_value_t mir_const_string(mir_unit_t *mu, const char *str);
mir_value_t mir_build_const_rep(mir_unit_t *mu, mir_type_t type,
                                mir_value_t value, unsigned rep);
mir_value_t mir_const_record(mir_unit_t *mu, mir_type_t type,
                             const mir_value_t *values, size_t count);

// Arithmetic
mir_value_t mir_build_add(mir_unit_t *mu, mir_type_t type, mir_value_t left,
                          mir_value_t right);
mir_value_t mir_build_sub(mir_unit_t *mu, mir_type_t type, mir_value_t left,
                          mir_value_t right);
mir_value_t mir_build_mul(mir_unit_t *mu, mir_type_t type, mir_value_t left,
                          mir_value_t right);
mir_value_t mir_build_div(mir_unit_t *mu, mir_type_t type, mir_value_t left,
                          mir_value_t right);
mir_value_t mir_build_rem(mir_unit_t *mu, mir_type_t type, mir_value_t left,
                          mir_value_t right);
mir_value_t mir_build_mod(mir_unit_t *mu, mir_type_t type, mir_value_t left,
                          mir_value_t right);
mir_value_t mir_build_exp(mir_unit_t *mu, mir_type_t type, mir_value_t left,
                          mir_value_t right);
mir_value_t mir_build_neg(mir_unit_t *mu, mir_type_t type, mir_value_t value);
mir_value_t mir_build_abs(mir_unit_t *mu, mir_type_t type, mir_value_t value);

// Trapping arithmetic
mir_value_t mir_build_trap_add(mir_unit_t *mu, mir_type_t type,
                               mir_value_t left, mir_value_t right,
                               mir_value_t locus);
mir_value_t mir_build_trap_sub(mir_unit_t *mu, mir_type_t type,
                               mir_value_t left, mir_value_t right,
                               mir_value_t locus);
mir_value_t mir_build_trap_mul(mir_unit_t *mu, mir_type_t type,
                               mir_value_t left, mir_value_t right,
                               mir_value_t locus);
mir_value_t mir_build_trap_exp(mir_unit_t *mu, mir_type_t type,
                               mir_value_t left, mir_value_t right,
                               mir_value_t locus);
mir_value_t mir_build_trap_neg(mir_unit_t *mu, mir_type_t type,
                               mir_value_t value, mir_value_t locus);

// Logical
mir_value_t mir_build_and(mir_unit_t *mu, mir_value_t left, mir_value_t right);
mir_value_t mir_build_or(mir_unit_t *mu, mir_value_t left, mir_value_t right);
mir_value_t mir_build_xor(mir_unit_t *mu, mir_value_t left, mir_value_t right);
mir_value_t mir_build_not(mir_unit_t *mu, mir_value_t value);
mir_value_t mir_build_cmp(mir_unit_t *mu, mir_cmp_t cmp, mir_value_t left,
                          mir_value_t right);

// Vectors
mir_value_t mir_build_pack(mir_unit_t *mu, mir_type_t type, mir_value_t arg);
mir_value_t mir_build_unpack(mir_unit_t *mu, mir_value_t vec, uint8_t strength,
                             mir_value_t dest);
mir_value_t mir_build_binary(mir_unit_t *mu, mir_vec_op_t op, mir_type_t type,
                             mir_value_t left, mir_value_t right);
mir_value_t mir_build_unary(mir_unit_t *mu, mir_vec_op_t op, mir_type_t type,
                            mir_value_t arg);
mir_value_t mir_build_insert(mir_unit_t *mu, mir_value_t part, mir_value_t full,
                             mir_value_t pos);
mir_value_t mir_build_extract(mir_unit_t *mu, mir_type_t type, mir_value_t full,
                              mir_value_t pos);
mir_value_t mir_build_test(mir_unit_t *mu, mir_value_t vec);

// Memory
void mir_build_store(mir_unit_t *mu, mir_value_t dest, mir_value_t src);
mir_value_t mir_build_load(mir_unit_t *mu, mir_value_t value);
void mir_build_copy(mir_unit_t *mu, mir_value_t dest, mir_value_t src,
                    mir_value_t count);
void mir_build_set(mir_unit_t *mu, mir_value_t dest, mir_value_t value,
                   mir_value_t count);
mir_value_t mir_build_alloc(mir_unit_t *mu, mir_type_t type, mir_stamp_t stamp,
                            mir_value_t count);
mir_value_t mir_build_null(mir_unit_t *mu, mir_type_t type);
mir_value_t mir_build_new(mir_unit_t *mu, mir_type_t type, mir_stamp_t stamp,
                          mir_value_t count);
mir_value_t mir_build_all(mir_unit_t *mu, mir_value_t access);
mir_value_t mir_build_address_of(mir_unit_t *mu, mir_value_t array);
mir_value_t mir_build_array_ref(mir_unit_t *mu, mir_value_t array,
                                mir_value_t offset);
mir_value_t mir_build_table_ref(mir_unit_t *mu, mir_value_t array,
                                mir_value_t stride, const mir_value_t *args,
                                int nargs);
mir_value_t mir_build_record_ref(mir_unit_t *mu, mir_value_t record,
                                 unsigned field);

// Arrays
mir_value_t mir_build_wrap(mir_unit_t *mu, mir_value_t data,
                           const mir_dim_t *dims, int ndims);
mir_value_t mir_build_unwrap(mir_unit_t *mu, mir_value_t array);
mir_value_t mir_build_uarray_len(mir_unit_t *mu, mir_value_t array, int dim);
mir_value_t mir_build_uarray_left(mir_unit_t *mu, mir_value_t array, int dim);
mir_value_t mir_build_uarray_right(mir_unit_t *mu, mir_value_t array, int dim);
mir_value_t mir_build_uarray_dir(mir_unit_t *mu, mir_value_t array, int dim);

mir_value_t mir_build_range_length(mir_unit_t *mu, mir_value_t left,
                                   mir_value_t right, mir_value_t dir);
mir_value_t mir_build_range_null(mir_unit_t *mu, mir_value_t left,
                                 mir_value_t right, mir_value_t dir);

// Control flow
void mir_build_jump(mir_unit_t *mu, mir_block_t target);
void mir_build_cond(mir_unit_t *mu, mir_value_t test, mir_block_t btrue,
                    mir_block_t bfalse);
mir_value_t mir_build_select(mir_unit_t *mu, mir_type_t type, mir_value_t test,
                             mir_value_t vtrue, mir_value_t vfalse);
mir_value_t mir_build_phi(mir_unit_t *mu, mir_type_t type, unsigned ninputs);
void mir_build_case(mir_unit_t *mu, mir_value_t value, mir_block_t def,
                    const mir_value_t *cases, const mir_block_t *blocks,
                    int ncases);
void mir_build_return(mir_unit_t *mu, mir_value_t value);
void mir_build_wait(mir_unit_t *mu, mir_block_t target);
void mir_build_consume(mir_unit_t *mu, mir_value_t value);
mir_value_t mir_build_fcall(mir_unit_t *mu, ident_t name, mir_type_t type,
                            mir_stamp_t stamp, const mir_value_t *args,
                            unsigned nargs);
void mir_build_pcall(mir_unit_t *mu, ident_t name, mir_block_t resume,
                     const mir_value_t *args, unsigned nargs);
void mir_build_resume(mir_unit_t *mu, ident_t name);
mir_value_t mir_build_syscall(mir_unit_t *mu, ident_t func, mir_type_t type,
                              mir_stamp_t stamp, mir_value_t locus,
                              const mir_value_t *args, int nargs);
void mir_build_unreachable(mir_unit_t *mu, mir_value_t locus);

// Checks
void mir_build_range_check(mir_unit_t *mu, mir_value_t value, mir_value_t left,
                           mir_value_t right, mir_value_t dir,
                           mir_value_t locus, mir_value_t hint);
void mir_build_index_check(mir_unit_t *mu, mir_value_t value, mir_value_t left,
                           mir_value_t right, mir_value_t dir,
                           mir_value_t locus, mir_value_t hint);
void mir_build_dir_check(mir_unit_t *mu, mir_value_t value, mir_value_t dir,
                         mir_value_t locus);
void mir_build_null_check(mir_unit_t *mu, mir_value_t ptr, mir_value_t locus);
void mir_build_zero_check(mir_unit_t *mu, mir_value_t value, mir_value_t locus);
void mir_build_length_check(mir_unit_t *mu, mir_value_t llen, mir_value_t rlen,
                            mir_value_t locus, mir_value_t dim);
void mir_build_exponent_check(mir_unit_t *mu, mir_value_t exp,
                              mir_value_t locus);

// Files
void mir_build_file_open(mir_unit_t *mu, mir_value_t file, mir_value_t name,
                         mir_value_t length, mir_value_t kind,
                         mir_value_t status);
void mir_build_file_read(mir_unit_t *mu, mir_value_t file, mir_value_t ptr,
                         mir_value_t inlen, mir_value_t outlen);
void mir_build_file_write(mir_unit_t *mu, mir_value_t file, mir_value_t value,
                          mir_value_t length);

// Port conversion
mir_value_t mir_build_port_conversion(mir_unit_t *mu, mir_value_t driving,
                                      mir_value_t effective);
void mir_build_convert_in(mir_unit_t *mu, mir_value_t conv, mir_value_t nets,
                          mir_value_t count);
void mir_build_convert_out(mir_unit_t *mu, mir_value_t conv, mir_value_t nets,
                           mir_value_t count);
void mir_build_put_conversion(mir_unit_t *mu, mir_value_t cf,
                              mir_value_t target, mir_value_t count,
                              mir_value_t values);

// Signals
mir_value_t mir_build_init_signal(mir_unit_t *mu, mir_type_t type,
                                  mir_value_t count, mir_value_t size,
                                  mir_value_t value, mir_value_t flags,
                                  mir_value_t locus, mir_value_t offset);
mir_value_t mir_build_implicit_signal(mir_unit_t *mu, mir_type_t type,
                                      mir_value_t count, mir_value_t size,
                                      mir_value_t locus, mir_value_t kind,
                                      mir_value_t closure, mir_value_t delay);
void mir_build_drive_signal(mir_unit_t *mu, mir_value_t target,
                            mir_value_t count);
void mir_build_sched_waveform(mir_unit_t *mu, mir_value_t target,
                              mir_value_t count, mir_value_t values,
                              mir_value_t reject, mir_value_t after);
void mir_build_put_driver(mir_unit_t *mu, mir_value_t target,
                          mir_value_t count, mir_value_t values);
void mir_build_deposit_signal(mir_unit_t *mu, mir_value_t target,
                              mir_value_t count, mir_value_t values);
void mir_build_sched_deposit(mir_unit_t *mu, mir_value_t target,
                             mir_value_t count, mir_value_t values,
                             mir_value_t after);
mir_value_t mir_build_resolved(mir_unit_t *mu, mir_value_t signal);
mir_value_t mir_build_last_value(mir_unit_t *mu, mir_value_t signal);
mir_value_t mir_build_driving_value(mir_unit_t *mu, mir_value_t signal,
                                    mir_value_t count);
void mir_build_force(mir_unit_t *mu, mir_value_t target, mir_value_t count,
                     mir_value_t values);
void mir_build_release(mir_unit_t *mu, mir_value_t target, mir_value_t count);
void mir_build_disconnect(mir_unit_t *mu, mir_value_t target, mir_value_t count,
                          mir_value_t reject, mir_value_t after);
mir_value_t mir_build_last_event(mir_unit_t *mu, mir_value_t signal,
                                 mir_value_t count);
mir_value_t mir_build_last_active(mir_unit_t *mu, mir_value_t signal,
                                  mir_value_t count);
mir_value_t mir_build_event_flag(mir_unit_t *mu, mir_value_t signal,
                                 mir_value_t count);
mir_value_t mir_build_active_flag(mir_unit_t *mu, mir_value_t signal,
                                  mir_value_t count);
mir_value_t mir_build_driving_flag(mir_unit_t *mu, mir_value_t signal,
                                   mir_value_t count);
void mir_build_resolve_signal(mir_unit_t *mu, mir_value_t signal,
                              mir_value_t resolution);
void mir_build_transfer_signal(mir_unit_t *mu, mir_value_t target,
                               mir_value_t source, mir_value_t count,
                               mir_value_t reject, mir_value_t after);

// Coverage
void mir_build_cover_stmt(mir_unit_t *mu, uint32_t tag);
void mir_build_cover_branch(mir_unit_t *mu, uint32_t tag);
void mir_build_cover_expr(mir_unit_t *mu, uint32_t tag);
void mir_build_cover_toggle(mir_unit_t *mu, mir_value_t signal, uint32_t tag);
void mir_build_cover_state(mir_unit_t *mu, mir_value_t signal, mir_value_t low,
                           uint32_t tag);

// Initialisation
mir_value_t mir_build_package_init(mir_unit_t *mu, ident_t name,
                                   mir_value_t context);
void mir_build_process_init(mir_unit_t *mu, ident_t name, mir_value_t locus);
mir_value_t mir_build_protected_init(mir_unit_t *mu, mir_type_t type,
                                     mir_value_t context, mir_value_t path_name,
                                     mir_value_t inst_name);
void mir_build_record_scope(mir_unit_t *mu, mir_value_t locus, mir_type_t type);
void mir_build_array_scope(mir_unit_t *mu, mir_value_t locus, mir_type_t type);
void mir_build_package_scope(mir_unit_t *mu, mir_value_t locus);
void mir_build_pop_scope(mir_unit_t *mu);
void mir_build_alias_signal(mir_unit_t *mu, mir_value_t signal,
                            mir_value_t locus);
void mir_build_map_signal(mir_unit_t *mu, mir_value_t src, mir_value_t dst,
                          mir_value_t count);
void mir_build_map_const(mir_unit_t *mu, mir_value_t src, mir_value_t dst,
                         mir_value_t count);
void mir_build_map_implicit(mir_unit_t *mu, mir_value_t src, mir_value_t dst,
                            mir_value_t count);

// Triggers
mir_value_t mir_build_level_trigger(mir_unit_t *mu, mir_value_t signal,
                                    mir_value_t count);
mir_value_t mir_build_cmp_trigger(mir_unit_t *mu, mir_value_t left,
                                  mir_value_t right);
mir_value_t mir_build_function_trigger(mir_unit_t *mu, ident_t name,
                                       const mir_value_t *args, unsigned nargs);
mir_value_t mir_build_or_trigger(mir_unit_t *mu, mir_value_t left,
                                 mir_value_t right);
void mir_build_add_trigger(mir_unit_t *mu, mir_value_t trigger);

// Linking
mir_value_t mir_build_link_package(mir_unit_t *mu, ident_t name);
mir_value_t mir_build_link_var(mir_unit_t *mu, mir_value_t context,
                               ident_t name, mir_type_t type);
void mir_build_bind_foreign(mir_unit_t *mu, mir_value_t spec,
                            mir_value_t length, mir_value_t locus);
mir_value_t mir_build_bind_external(mir_unit_t *mu, mir_value_t locus,
                                    ident_t scope, mir_type_t type,
                                    mir_stamp_t stamp, const mir_value_t *args,
                                    int nargs);
mir_value_t mir_build_context_upref(mir_unit_t *mu, int hops);
mir_value_t mir_build_var_upref(mir_unit_t *mu, int hops, int nth);

// Events
void mir_build_sched_event(mir_unit_t *mu, mir_value_t on, mir_value_t count);
void mir_build_clear_event(mir_unit_t *mu, mir_value_t on, mir_value_t count);
void mir_build_sched_process(mir_unit_t *mu, mir_value_t delay);
void mir_build_sched_inactive(mir_unit_t *mu);

// Reflection
mir_value_t mir_build_reflect_value(mir_unit_t *mu, mir_value_t value,
                                    mir_value_t context, mir_value_t locus,
                                    mir_value_t bounds);
mir_value_t mir_build_reflect_subtype(mir_unit_t *mu, mir_value_t context,
                                      mir_value_t locus, mir_value_t bounds);

// Assertions
void mir_build_assert(mir_unit_t *mu, mir_value_t value, mir_value_t message,
                      mir_value_t length, mir_value_t severity,
                      mir_value_t locus, mir_value_t hint_left,
                      mir_value_t hint_right);
void mir_build_report(mir_unit_t *mu, mir_value_t message, mir_value_t length,
                      mir_value_t severity, mir_value_t locus);

// Miscellaneous
mir_value_t mir_build_instance_name(mir_unit_t *mu, mir_value_t kind);
void mir_build_enter_state(mir_unit_t *mu, mir_value_t state,
                           mir_value_t strong);
mir_value_t mir_build_closure(mir_unit_t *mu, ident_t func, mir_value_t context,
                              mir_type_t atype, mir_type_t rtype);
mir_value_t mir_build_resolution_wrapper(mir_unit_t *mu, mir_type_t type,
                                         mir_value_t closure,
                                         mir_value_t nlits);
mir_value_t mir_build_locus(mir_unit_t *mu, object_t *obj);
mir_value_t mir_build_cast(mir_unit_t *mu, mir_type_t type, mir_value_t value);
void mir_build_debug_out(mir_unit_t *mu, mir_value_t value);

#endif   // _MIR_NODE_H
