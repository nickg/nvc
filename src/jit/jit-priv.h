//
//  Copyright (C) 2022-2024  Nick Gasson
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
#include "jit/jit-ffi.h"
#include "mask.h"
#include "rt/mspace.h"
#include "thread.h"

#include <setjmp.h>
#include <signal.h>

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
   J_DEBUG,
   J_NOP,
   J_ASR,
   J_SHL,
   J_CLAMP,
   J_CCMP,
   J_FCCMP,

   __MACRO_BASE = 0x80,
   MACRO_COPY = __MACRO_BASE,
   MACRO_GALLOC,
   MACRO_EXIT,
   MACRO_FEXP,
   MACRO_EXP,
   MACRO_BZERO,
   MACRO_GETPRIV,
   MACRO_PUTPRIV,
   MACRO_LALLOC,
   MACRO_SALLOC,
   MACRO_CASE,
   MACRO_TRIM,
   MACRO_MOVE,
   MACRO_MEMSET,
   MACRO_REEXEC,
   MACRO_SADD,
} jit_op_t;

typedef enum {
   JIT_SZ_8 = 0,
   JIT_SZ_16 = 1,
   JIT_SZ_32 = 2,
   JIT_SZ_64 = 3,
   JIT_SZ_PTR = JIT_SZ_64,
   JIT_SZ_UNSPEC = 7
} jit_size_t;

typedef enum {
   JIT_CC_NONE,
   JIT_CC_T,
   JIT_CC_F,
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
   JIT_EXIT_RANGE_FAIL,
   JIT_EXIT_FUNC_WAIT,
   JIT_EXIT_INIT_SIGNAL,
   JIT_EXIT_DRIVE_SIGNAL,
   JIT_EXIT_SCHED_WAVEFORM,
   JIT_EXIT_SCHED_PROCESS,
   JIT_EXIT_TEST_EVENT,
   JIT_EXIT_TEST_ACTIVE,
   JIT_EXIT_SCHED_EVENT,
   JIT_EXIT_FILE_OPEN,
   JIT_EXIT_FILE_READ,
   JIT_EXIT_FILE_WRITE,
   JIT_EXIT_DEBUG_OUT,
   JIT_EXIT_ALIAS_SIGNAL,
   JIT_EXIT_MAP_SIGNAL,
   JIT_EXIT_MAP_CONST,
   JIT_EXIT_RESOLVE_SIGNAL,
   JIT_EXIT_LAST_EVENT,
   JIT_EXIT_LAST_ACTIVE,
   JIT_EXIT_DISCONNECT,
   JIT_EXIT_FORCE,
   JIT_EXIT_RELEASE,
   JIT_EXIT_PUSH_SCOPE,
   JIT_EXIT_POP_SCOPE,
   JIT_EXIT_IMPLICIT_SIGNAL,
   JIT_EXIT_DRIVING,
   JIT_EXIT_DRIVING_VALUE,
   JIT_EXIT_COVER_TOGGLE,
   JIT_EXIT_COVER_STATE,
   JIT_EXIT_PROCESS_INIT,
   JIT_EXIT_CLEAR_EVENT,
   JIT_EXIT_ENTER_STATE,
   JIT_EXIT_REFLECT_VALUE,
   JIT_EXIT_REFLECT_SUBTYPE,
   JIT_EXIT_FUNCTION_TRIGGER,
   JIT_EXIT_ADD_TRIGGER,
   JIT_EXIT_TRANSFER_SIGNAL,
   JIT_EXIT_PORT_CONVERSION,
   JIT_EXIT_CONVERT_IN,
   JIT_EXIT_CONVERT_OUT,
   JIT_EXIT_BIND_FOREIGN,
   JIT_EXIT_OR_TRIGGER,
   JIT_EXIT_CMP_TRIGGER,
   JIT_EXIT_INSTANCE_NAME,
   JIT_EXIT_DEPOSIT_SIGNAL,
   JIT_EXIT_MAP_IMPLICIT,
   JIT_EXIT_BIND_EXTERNAL,
   JIT_EXIT_SYSCALL,
   JIT_EXIT_PUT_CONVERSION,
} jit_exit_t;

typedef uint16_t jit_reg_t;
#define JIT_REG_INVALID UINT16_MAX

typedef enum {
   JIT_VALUE_INVALID,
   JIT_VALUE_REG,
   JIT_VALUE_INT64,
   JIT_VALUE_DOUBLE,
   JIT_ADDR_REG,
   JIT_ADDR_ABS,
   JIT_ADDR_CPOOL,
   JIT_ADDR_COVER,
   JIT_VALUE_LABEL,
   JIT_VALUE_HANDLE,
   JIT_VALUE_EXIT,
   JIT_VALUE_LOC,
   JIT_VALUE_VPOS,
   JIT_VALUE_LOCUS,
} jit_value_kind_t;

typedef uint32_t jit_label_t;
#define JIT_LABEL_INVALID UINT32_MAX

typedef struct {
   uint32_t block;
   uint32_t op;
} jit_vpos_t;

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
      loc_t        loc;
      jit_vpos_t   vpos;
      object_t    *locus;
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

STATIC_ASSERT(sizeof(jit_ir_t) <= 40);

typedef struct _jit_tier jit_tier_t;
typedef struct _jit_func jit_func_t;
typedef struct _jit_block jit_block_t;
typedef struct _jit_anchor jit_anchor_t;

typedef void (*jit_entry_fn_t)(jit_func_t *, jit_anchor_t *,
                               jit_scalar_t *, tlab_t *);

typedef struct {
   unsigned count;
   unsigned max;
   union {
      unsigned  edges[4];
      unsigned *external;
   } u;
} jit_edge_list_t;

typedef struct _jit_block {
   unsigned        first;
   unsigned        last;
   unsigned        aborts : 1;
   unsigned        returns : 1;
   jit_edge_list_t in;
   jit_edge_list_t out;
   bit_mask_t      livein;
   bit_mask_t      varkill;
   bit_mask_t      liveout;
} jit_block_t;

typedef struct {
   unsigned    nblocks;
   jit_block_t blocks[0];
} jit_cfg_t;

typedef enum {
   JIT_FUNC_PLACEHOLDER,
   JIT_FUNC_COMPILING,
   JIT_FUNC_READY,
   JIT_FUNC_ERROR,
} func_state_t;

typedef enum {
   RELOC_NULL,
   RELOC_HANDLE,
   RELOC_FUNC,
   RELOC_PRIVDATA,
   RELOC_COVER,
   RELOC_PROCESSED,
} reloc_kind_t;

typedef struct {
   ident_t  name;
   unsigned offset;
} link_tab_t;

typedef struct _jit_func {
   jit_entry_fn_t  entry;    // Must be first
   func_state_t    state;
   jit_t          *jit;
   ident_t         name;
   link_tab_t     *linktab;
   mptr_t          privdata;
   jit_ir_t       *irbuf;
   unsigned char  *cpool;
   unsigned        framesz;
   unsigned        nirs;
   unsigned        nregs;
   unsigned        nvars;
   unsigned        cpoolsz;
   bool            owns_cpool;
   jit_handle_t    handle;
   unsigned        hotness;
   jit_tier_t     *next_tier;
   jit_cfg_t      *cfg;
   ffi_spec_t      spec;
   object_t       *object;
} jit_func_t;

// The code generator knows the layout of this struct
typedef struct _jit_anchor {
   jit_anchor_t *caller;
   jit_func_t   *func;
   uint32_t      irpos;
   uint32_t      watermark;
} jit_anchor_t;

typedef enum {
   JIT_IDLE,
   JIT_RUNNING,
   JIT_COMPILING,
} jit_state_t;

#if defined HAVE___BUILTIN_SETJMP && !defined __clang__
typedef void *jit_jmpbuf_t[5];
#define jit_setjmp(buf) __builtin_setjmp((buf))
#define jit_longjmp(buf, arg) __builtin_longjmp((buf), (arg))
#else
typedef sigjmp_buf jit_jmpbuf_t;
#define jit_setjmp(buf) sigsetjmp((buf), 0)
#define jit_longjmp(buf, arg) siglongjmp((buf), arg)
#endif

typedef struct {
   jit_t                 *jit;
   jit_state_t            state;
   jit_jmpbuf_t           abort_env;
   volatile sig_atomic_t  jmp_buf_valid;
   jit_anchor_t          *anchor;
} jit_thread_local_t;

typedef struct _code_cache code_cache_t;
typedef struct _code_span code_span_t;
typedef struct _patch_list patch_list_t;

typedef struct {
   code_span_t  *span;
   jit_func_t   *func;
   uint8_t      *wptr;
   ihash_t      *labels;
   patch_list_t *patches;
   bool          overflow;
} code_blob_t;

typedef struct _pack_writer pack_writer_t;

#define JIT_MAX_ARGS 64

typedef struct _jit_interp jit_interp_t;

void jit_irgen(jit_func_t *f, mir_unit_t *mu);
void jit_dump(jit_func_t *f);
void jit_dump_with_mark(jit_func_t *f, jit_label_t label, bool cpool);
void jit_dump_interleaved(jit_func_t *f, mir_unit_t *mu);
const char *jit_op_name(jit_op_t op);
const char *jit_cc_name(jit_cc_t cc);
const char *jit_exit_name(jit_exit_t exit);
void jit_interp(jit_func_t *f, jit_anchor_t *caller, jit_scalar_t *args,
                tlab_t *tlab);
jit_func_t *jit_get_func(jit_t *j, jit_handle_t handle);
void jit_hexdump(const unsigned char *data, size_t sz, int blocksz,
                 const void *highlight, const char *prefix);
void **jit_get_privdata_ptr(jit_t *j, jit_func_t *f);
void jit_tier_up(jit_func_t *f);
jit_thread_local_t *jit_thread_local(void);
void jit_fill_irbuf(jit_func_t *f);
int32_t *jit_get_cover_ptr(jit_t *j, jit_value_t addr);
jit_entry_fn_t jit_bind_intrinsic(ident_t name);
jit_thread_local_t *jit_attach_thread(jit_anchor_t *anchor);

jit_cfg_t *jit_get_cfg(jit_func_t *f);
void jit_free_cfg(jit_func_t *f);
jit_block_t *jit_block_for(jit_cfg_t *cfg, int pos);
int jit_get_edge(jit_edge_list_t *list, int nth);

bool jit_will_abort(jit_ir_t *ir);
bool jit_writes_flags(jit_ir_t *ir);
bool jit_reads_flags(jit_ir_t *ir);

void jit_do_lvn(jit_func_t *f);
void jit_do_cprop(jit_func_t *f);
void jit_do_dce(jit_func_t *f);
void jit_delete_nops(jit_func_t *f);
void jit_do_mem2reg(jit_func_t *f);

typedef unsigned phys_slot_t;
#define INT_BASE   0
#define FLOAT_BASE 32
#define STACK_BASE 100

int jit_do_lscan(jit_func_t *f, phys_slot_t *slots, uint64_t badmask);

code_cache_t *code_cache_new(void);
void code_cache_free(code_cache_t *code);

typedef void (*code_patch_fn_t)(code_blob_t *, jit_label_t, uint8_t *,
                                const uint8_t *);

code_blob_t *code_blob_new(code_cache_t *code, ident_t name, size_t hint);
void code_blob_emit(code_blob_t *blob, const uint8_t *bytes, size_t len);
void code_blob_align(code_blob_t *blob, unsigned align);
void code_blob_finalise(code_blob_t *blob, jit_entry_fn_t *entry);
void code_blob_mark(code_blob_t *blob, jit_label_t label);
void code_blob_patch(code_blob_t *blob, jit_label_t label, code_patch_fn_t fn);
void code_load_object(code_blob_t *blob, const void *data, size_t size);

#ifdef DEBUG
__attribute__((format(printf, 2, 3)))
void code_blob_printf(code_blob_t *blob, const char *fmt, ...);

void code_blob_print_ir(code_blob_t *blob, jit_ir_t *ir);
#else

#define code_blob_printf(blob, fmt, ...)
#define code_blob_print_ir(blob, ir)
#endif

bool jit_pack_fill(jit_pack_t *jp, jit_t *j, jit_func_t *f);
void jit_pack_put(jit_pack_t *jp, ident_t name, const uint8_t *cpool,
                  const char *strtab, const uint8_t *buf);

pack_writer_t *pack_writer_new(void);
void pack_writer_emit(pack_writer_t *pw, jit_t *j, jit_handle_t handle,
                      uint8_t **buf, size_t *size);
unsigned pack_writer_get_string(pack_writer_t *pw, const char *str);
void pack_writer_string_table(pack_writer_t *pw, const char **tab,
                              size_t *size);
void pack_writer_free(pack_writer_t *pw);

void jit_bind_foreign(jit_func_t *f, const uint8_t *spec, size_t length,
                      tree_t where);
void jit_do_syscall(vlog_node_t where, jit_anchor_t *caller, jit_scalar_t *args,
                    tlab_t *tlab);

DLLEXPORT void __nvc_do_exit(jit_exit_t which, jit_anchor_t *anchor,
                             jit_scalar_t *args, tlab_t *tlab);
DLLEXPORT void *__nvc_mspace_alloc(uintptr_t size, jit_anchor_t *anchor);
DLLEXPORT void _debug_out(intptr_t val, int32_t reg);

#endif  // _JIT_PRIV_H
