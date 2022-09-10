//
//  Copyright (C) 2022  Nick Gasson
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

#ifndef _JIT_PRIV_H
#define _JIT_PRIV_H

#include "util.h"
#include "jit/jit.h"
#include "rt/mspace.h"

typedef enum {
   J_SEND,
   J_RECV,
   J_ADD,
   J_RET,
   J_TRAP,
   J_ULOAD,
   J_STORE,
   J_JUMP,
   J_CMP,
   J_CSET,
   J_SUB,
   J_MOV,
   J_FADD,
   J_MUL,
   J_FMUL,
   J_CALL,
   J_NEG,
   J_LOAD,
   J_CSEL,
   J_LEA,
   J_NOT,
   J_DIV,
   J_FDIV,
   J_SCVTF,
   J_FNEG,
   J_FCVTNS,
   J_FCMP,
   J_AND,
   J_OR,
   J_XOR,
   J_FSUB,
   J_REM,

   __MACRO_BASE = 0x80,
   MACRO_COPY = __MACRO_BASE,
   MACRO_GALLOC,
   MACRO_EXIT,
   MACRO_FEXP,
   MACRO_EXP,
   MACRO_BZERO,
   MACRO_FFICALL,
   MACRO_GETPRIV,
   MACRO_PUTPRIV,
} jit_op_t;

typedef enum {
   JIT_SZ_UNSPEC,
   JIT_SZ_8,
   JIT_SZ_16,
   JIT_SZ_32,
   JIT_SZ_64,
   JIT_SZ_PTR = JIT_SZ_64
} jit_size_t;

typedef enum {
   JIT_CC_NONE,
   JIT_CC_EQ,
   JIT_CC_NE,
   JIT_CC_LT,
   JIT_CC_GE,
   JIT_CC_GT,
   JIT_CC_LE,
   JIT_CC_O,
   JIT_CC_NO,
   JIT_CC_C,
   JIT_CC_NC,
} jit_cc_t;

typedef enum {
   JIT_EXIT_INDEX_FAIL,
   JIT_EXIT_OVERFLOW,
   JIT_EXIT_NULL_DEREF,
   JIT_EXIT_LENGTH_FAIL,
   JIT_EXIT_UNREACHABLE,
   JIT_EXIT_DIV_ZERO,
   JIT_EXIT_EXPONENT_FAIL,
   JIT_EXIT_REPORT,
   JIT_EXIT_ASSERT_FAIL,
   JIT_EXIT_INT_TO_STRING,
   JIT_EXIT_REAL_TO_STRING,
   JIT_EXIT_RANGE_FAIL,
   JIT_EXIT_FUNC_WAIT,
   JIT_EXIT_INIT_SIGNAL,
   JIT_EXIT_DRIVE_SIGNAL,
   JIT_EXIT_SCHED_WAVEFORM,
   JIT_EXIT_SCHED_PROCESS,
   JIT_EXIT_TEST_EVENT,
   JIT_EXIT_TEST_ACTIVE,
   JIT_EXIT_INIT_SIGNALS,
   JIT_EXIT_SCHED_EVENT,
   JIT_EXIT_NOW,
   JIT_EXIT_SCHED_WAVEFORMS,
   JIT_EXIT_FILE_OPEN,
   JIT_EXIT_FILE_CLOSE,
   JIT_EXIT_FILE_READ,
   JIT_EXIT_FILE_WRITE,
   JIT_EXIT_ENDFILE,
   JIT_EXIT_FILE_FLUSH,
   JIT_EXIT_STRING_TO_INT,
   JIT_EXIT_STRING_TO_REAL,
   JIT_EXIT_CANON_VALUE,
   JIT_EXIT_DEBUG_OUT,
} jit_exit_t;

typedef uint16_t jit_reg_t;
#define JIT_REG_INVALID UINT16_MAX

typedef enum {
   JIT_VALUE_INVALID,
   JIT_VALUE_REG,
   JIT_VALUE_INT64,
   JIT_VALUE_DOUBLE,
   JIT_ADDR_FRAME,
   JIT_ADDR_REG,
   JIT_ADDR_ABS,
   JIT_ADDR_CPOOL,
   JIT_VALUE_LABEL,
   JIT_VALUE_HANDLE,
   JIT_VALUE_EXIT,
} jit_value_kind_t;

typedef uint32_t jit_label_t;
#define JIT_LABEL_INVALID UINT32_MAX

typedef struct {
   jit_value_kind_t kind : 8;
   int32_t          disp;
   union {
      jit_reg_t    reg;
      int64_t      int64;
      double       dval;
      jit_label_t  label;
      jit_handle_t handle;
      jit_exit_t   exit;
   };
} jit_value_t;

STATIC_ASSERT(sizeof(jit_value_t) == 16);

typedef struct {
   jit_op_t    op : 8;
   jit_size_t  size : 3;
   unsigned    target : 1;
   jit_cc_t    cc : 4;
   jit_reg_t   result;
   jit_value_t arg1;
   jit_value_t arg2;
} jit_ir_t;

STATIC_ASSERT(sizeof(jit_ir_t) == 40);

typedef struct {
   jit_t         *jit;
   vcode_unit_t   unit;
   ident_t        name;
   unsigned      *varoff;
   mptr_t         privdata;
   jit_ir_t      *irbuf;
   unsigned char *cpool;
   unsigned       framesz;
   unsigned       nirs;
   unsigned       bufsz;
   unsigned       nregs;
   unsigned       nvars;
   unsigned       cpoolsz;
   jit_handle_t   handle;
   void          *symbol;
} jit_func_t;

#define JIT_MAX_ARGS 64

typedef struct _jit_interp jit_interp_t;

void jit_irgen(jit_func_t *f);
void jit_dump(jit_func_t *f);
void jit_dump_with_mark(jit_func_t *f, jit_label_t label, bool cpool);
const char *jit_op_name(jit_op_t op);
const char *jit_exit_name(jit_exit_t exit);
bool jit_interp(jit_func_t *f, jit_scalar_t *args, int nargs, int backedge);
void jit_interp_abort(int code);
void jit_interp_diag_trace(diag_t *d);
void jit_emit_trace(diag_t *d, const loc_t *loc, tree_t enclosing,
                    const char *symbol);
jit_func_t *jit_get_func(jit_t *j, jit_handle_t handle);
void jit_hexdump(const unsigned char *data, size_t sz, int blocksz,
                 const void *highlight, const char *prefix);
void *jit_get_privdata(jit_t *j, jit_func_t *f);
void jit_put_privdata(jit_t *j, jit_func_t *f, void *ptr);
bool jit_show_errors(jit_t *j);
bool jit_has_runtime(jit_t *j);

#endif  // _JIT_PRIV_H
