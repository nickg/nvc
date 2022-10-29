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

#include "util.h"
#include "ident.h"
#include "lib.h"
#include "jit/jit-priv.h"
#include "opt.h"
#include "thread.h"

#ifdef LLVM_HAS_LLJIT

#include <assert.h>
#include <stdlib.h>
#include <limits.h>

#include <llvm-c/Analysis.h>
#include <llvm-c/BitReader.h>
#include <llvm-c/Core.h>
#include <llvm-c/DebugInfo.h>
#include <llvm-c/Error.h>
#include <llvm-c/ExecutionEngine.h>
#include <llvm-c/LLJIT.h>
#include <llvm-c/Transforms/Scalar.h>

typedef enum {
   LLVM_VOID,
   LLVM_PTR,
   LLVM_INT1,
   LLVM_INT8,
   LLVM_INT16,
   LLVM_INT32,
   LLVM_INT64,
   LLVM_INTPTR,
   LLVM_DOUBLE,

   LLVM_PAIR_I8_I1,
   LLVM_PAIR_I16_I1,
   LLVM_PAIR_I32_I1,
   LLVM_PAIR_I64_I1,

   LLVM_ENTRY_FN,
   LLVM_ANCHOR,

   LLVM_LAST_TYPE
} llvm_type_t;

typedef enum {
   LLVM_ADD_OVERFLOW_S8,
   LLVM_ADD_OVERFLOW_S16,
   LLVM_ADD_OVERFLOW_S32,
   LLVM_ADD_OVERFLOW_S64,

   LLVM_ADD_OVERFLOW_U8,
   LLVM_ADD_OVERFLOW_U16,
   LLVM_ADD_OVERFLOW_U32,
   LLVM_ADD_OVERFLOW_U64,

   LLVM_SUB_OVERFLOW_S8,
   LLVM_SUB_OVERFLOW_S16,
   LLVM_SUB_OVERFLOW_S32,
   LLVM_SUB_OVERFLOW_S64,

   LLVM_SUB_OVERFLOW_U8,
   LLVM_SUB_OVERFLOW_U16,
   LLVM_SUB_OVERFLOW_U32,
   LLVM_SUB_OVERFLOW_U64,

   LLVM_MUL_OVERFLOW_S8,
   LLVM_MUL_OVERFLOW_S16,
   LLVM_MUL_OVERFLOW_S32,
   LLVM_MUL_OVERFLOW_S64,

   LLVM_MUL_OVERFLOW_U8,
   LLVM_MUL_OVERFLOW_U16,
   LLVM_MUL_OVERFLOW_U32,
   LLVM_MUL_OVERFLOW_U64,

   LLVM_POW_F64,

   LLVM_DO_EXIT,
   LLVM_GETPRIV,
   LLVM_PUTPRIV,
   LLVM_MSPACE_ALLOC,
   LLVM_DO_FFICALL,

   LLVM_LAST_FN,
} llvm_fn_t;

typedef struct {
   LLVMBasicBlockRef  bbref;
   LLVMValueRef       inflags;
   LLVMValueRef       outflags;
   LLVMValueRef      *inregs;
   LLVMValueRef      *outregs;
   jit_block_t       *source;
} cgen_block_t;

typedef struct {
   LLVMModuleRef        module;
   LLVMContextRef       context;
   LLVMBuilderRef       builder;
   LLVMTargetMachineRef target;
   LLVMValueRef         llvmfn;
   LLVMValueRef         args;
   LLVMValueRef         frame;
   LLVMValueRef         anchor;
   LLVMTypeRef          types[LLVM_LAST_TYPE];
   LLVMValueRef         fns[LLVM_LAST_FN];
   LLVMTypeRef          fntypes[LLVM_LAST_FN];
   cgen_block_t        *blocks;
   jit_func_t          *func;
   jit_cfg_t           *cfg;
   char                *name;
   text_buf_t          *textbuf;
} cgen_req_t;

typedef struct {
   LLVMOrcThreadSafeContextRef context;
   LLVMOrcLLJITRef             jit;
   LLVMOrcExecutionSessionRef  session;
   LLVMOrcJITDylibRef          dylib;
} lljit_state_t;

#define LLVM_CHECK(op, ...) do {                        \
      LLVMErrorRef error = op(__VA_ARGS__);             \
      if (unlikely(error != LLVMErrorSuccess)) {        \
         char *msg = LLVMGetErrorMessage(error);        \
         fatal(#op " failed: %s", msg);                 \
      }                                                 \
   } while (0)

#ifdef LLVM_HAS_OPAQUE_POINTERS
#define PTR(x) x
#else
#define PTR(x) \
   LLVMBuildPointerCast(req->builder, (x), req->types[LLVM_PTR], "")
#endif

static LLVMValueRef llvm_int1(cgen_req_t *req, bool b)
{
   return LLVMConstInt(req->types[LLVM_INT1], b, false);
}

static LLVMValueRef llvm_int8(cgen_req_t *req, int8_t i)
{
   return LLVMConstInt(req->types[LLVM_INT8], i, false);
}

static LLVMValueRef llvm_int32(cgen_req_t *req, int32_t i)
{
   return LLVMConstInt(req->types[LLVM_INT32], i, false);
}

static LLVMValueRef llvm_int64(cgen_req_t *req, int64_t i)
{
   return LLVMConstInt(req->types[LLVM_INT64], i, false);
}

static LLVMValueRef llvm_intptr(cgen_req_t *req, intptr_t i)
{
   return LLVMConstInt(req->types[LLVM_INTPTR], i, false);
}

static LLVMValueRef llvm_ptr(cgen_req_t *req, void *ptr)
{
   return LLVMConstIntToPtr(llvm_intptr(req, (intptr_t)ptr),
                            req->types[LLVM_PTR]);
}

static LLVMBasicBlockRef cgen_append_block(cgen_req_t *req, const char *name)
{
   return LLVMAppendBasicBlockInContext(req->context, req->llvmfn, name);
}

static LLVMTypeRef cgen_get_type(cgen_req_t *req, llvm_type_t which)
{
   if (req->types[which] != NULL)
      return req->types[which];

   LLVMTypeRef type = NULL;
   switch (which) {
   case LLVM_PAIR_I32_I1:
      {
         LLVMTypeRef fields[] = {
            req->types[LLVM_INT32],
            req->types[LLVM_INT1]
         };
         type = LLVMStructTypeInContext(req->context, fields, 2, false);
      }
      break;

   default:
      fatal_trace("cannot generate type %d", which);
   }

   return (req->types[which] = type);
}

static LLVMValueRef cgen_get_fn(cgen_req_t *req, llvm_fn_t which)
{
   if (req->fns[which] != NULL)
      return req->fns[which];

   LLVMValueRef fn = NULL;
   switch (which) {
   case LLVM_ADD_OVERFLOW_S8:
   case LLVM_ADD_OVERFLOW_S16:
   case LLVM_ADD_OVERFLOW_S32:
   case LLVM_ADD_OVERFLOW_S64:
      {
         jit_size_t sz = which - LLVM_ADD_OVERFLOW_S8;
         LLVMTypeRef int_type = req->types[LLVM_INT8 + sz];
         LLVMTypeRef pair_type = cgen_get_type(req, LLVM_PAIR_I8_I1 + sz);
         LLVMTypeRef args[] = { int_type, int_type };
         req->fntypes[which] = LLVMFunctionType(pair_type, args,
                                                ARRAY_LEN(args), false);

         static const char *names[] = {
            "llvm.sadd.with.overflow.i8",
            "llvm.sadd.with.overflow.i16",
            "llvm.sadd.with.overflow.i32",
            "llvm.sadd.with.overflow.i64"
         };
         fn = LLVMAddFunction(req->module, names[sz], req->fntypes[which]);
      }
      break;

   case LLVM_ADD_OVERFLOW_U8:
   case LLVM_ADD_OVERFLOW_U16:
   case LLVM_ADD_OVERFLOW_U32:
   case LLVM_ADD_OVERFLOW_U64:
      {
         jit_size_t sz = which - LLVM_ADD_OVERFLOW_U8;
         LLVMTypeRef int_type = req->types[LLVM_INT8 + sz];
         LLVMTypeRef pair_type = cgen_get_type(req, LLVM_PAIR_I8_I1 + sz);
         LLVMTypeRef args[] = { int_type, int_type };
         req->fntypes[which] = LLVMFunctionType(pair_type, args,
                                                ARRAY_LEN(args), false);

         static const char *names[] = {
            "llvm.uadd.with.overflow.i8",
            "llvm.uadd.with.overflow.i16",
            "llvm.uadd.with.overflow.i32",
            "llvm.uadd.with.overflow.i64"
         };
         fn = LLVMAddFunction(req->module, names[sz], req->fntypes[which]);
      }
      break;

   case LLVM_SUB_OVERFLOW_S8:
   case LLVM_SUB_OVERFLOW_S16:
   case LLVM_SUB_OVERFLOW_S32:
   case LLVM_SUB_OVERFLOW_S64:
      {
         jit_size_t sz = which - LLVM_SUB_OVERFLOW_S8;
         LLVMTypeRef int_type = req->types[LLVM_INT8 + sz];
         LLVMTypeRef pair_type = cgen_get_type(req, LLVM_PAIR_I8_I1 + sz);
         LLVMTypeRef args[] = { int_type, int_type };
         req->fntypes[which] = LLVMFunctionType(pair_type, args,
                                                ARRAY_LEN(args), false);

         static const char *names[] = {
            "llvm.ssub.with.overflow.i8",
            "llvm.ssub.with.overflow.i16",
            "llvm.ssub.with.overflow.i32",
            "llvm.ssub.with.overflow.i64"
         };
         fn = LLVMAddFunction(req->module, names[sz], req->fntypes[which]);
      }
      break;

   case LLVM_SUB_OVERFLOW_U8:
   case LLVM_SUB_OVERFLOW_U16:
   case LLVM_SUB_OVERFLOW_U32:
   case LLVM_SUB_OVERFLOW_U64:
      {
         jit_size_t sz = which - LLVM_SUB_OVERFLOW_U8;
         LLVMTypeRef int_type = req->types[LLVM_INT8 + sz];
         LLVMTypeRef pair_type = cgen_get_type(req, LLVM_PAIR_I8_I1 + sz);
         LLVMTypeRef args[] = { int_type, int_type };
         req->fntypes[which] = LLVMFunctionType(pair_type, args,
                                                ARRAY_LEN(args), false);

         static const char *names[] = {
            "llvm.usub.with.overflow.i8",
            "llvm.usub.with.overflow.i16",
            "llvm.usub.with.overflow.i32",
            "llvm.usub.with.overflow.i64"
         };
         fn = LLVMAddFunction(req->module, names[sz], req->fntypes[which]);
      }
      break;

   case LLVM_MUL_OVERFLOW_S8:
   case LLVM_MUL_OVERFLOW_S16:
   case LLVM_MUL_OVERFLOW_S32:
   case LLVM_MUL_OVERFLOW_S64:
      {
         jit_size_t sz = which - LLVM_MUL_OVERFLOW_S8;
         LLVMTypeRef int_type = req->types[LLVM_INT8 + sz];
         LLVMTypeRef pair_type = cgen_get_type(req, LLVM_PAIR_I8_I1 + sz);
         LLVMTypeRef args[] = { int_type, int_type };
         req->fntypes[which] = LLVMFunctionType(pair_type, args,
                                                ARRAY_LEN(args), false);

         static const char *names[] = {
            "llvm.smul.with.overflow.i8",
            "llvm.smul.with.overflow.i16",
            "llvm.smul.with.overflow.i32",
            "llvm.smul.with.overflow.i64"
         };
         fn = LLVMAddFunction(req->module, names[sz], req->fntypes[which]);
      }
      break;

   case LLVM_MUL_OVERFLOW_U8:
   case LLVM_MUL_OVERFLOW_U16:
   case LLVM_MUL_OVERFLOW_U32:
   case LLVM_MUL_OVERFLOW_U64:
      {
         jit_size_t sz = which - LLVM_MUL_OVERFLOW_U8;
         LLVMTypeRef int_type = req->types[LLVM_INT8 + sz];
         LLVMTypeRef pair_type = cgen_get_type(req, LLVM_PAIR_I8_I1 + sz);
         LLVMTypeRef args[] = { int_type, int_type };
         req->fntypes[which] = LLVMFunctionType(pair_type, args,
                                                ARRAY_LEN(args), false);

         static const char *names[] = {
            "llvm.umul.with.overflow.i8",
            "llvm.umul.with.overflow.i16",
            "llvm.umul.with.overflow.i32",
            "llvm.umul.with.overflow.i64"
         };
         fn = LLVMAddFunction(req->module, names[sz], req->fntypes[which]);
      }
      break;

   case LLVM_POW_F64:
      {
         LLVMTypeRef args[] = {
            req->types[LLVM_DOUBLE],
            req->types[LLVM_DOUBLE]
         };
         req->fntypes[which] = LLVMFunctionType(req->types[LLVM_DOUBLE],
                                                args, ARRAY_LEN(args), false);

         fn = LLVMAddFunction(req->module, "llvm.pow.f64", req->fntypes[which]);
      }
      break;

   case LLVM_DO_EXIT:
      {
         LLVMTypeRef args[] = {
            req->types[LLVM_INT32],
            req->types[LLVM_PTR],
            req->types[LLVM_PTR]
         };
         req->fntypes[which] = LLVMFunctionType(req->types[LLVM_VOID], args,
                                                ARRAY_LEN(args), false);

         fn = LLVMAddFunction(req->module, "__nvc_do_exit",
                              req->fntypes[which]);
      }
      break;

   case LLVM_DO_FFICALL:
      {
         LLVMTypeRef args[] = {
            req->types[LLVM_PTR],
            req->types[LLVM_PTR],
            req->types[LLVM_PTR]
         };
         req->fntypes[which] = LLVMFunctionType(req->types[LLVM_VOID], args,
                                                ARRAY_LEN(args), false);

         fn = LLVMAddFunction(req->module, "__nvc_do_fficall",
                              req->fntypes[which]);
      }
      break;

   case LLVM_GETPRIV:
      {
         LLVMTypeRef args[] = { req->types[LLVM_INT32] };
         req->fntypes[which] = LLVMFunctionType(req->types[LLVM_PTR], args,
                                                ARRAY_LEN(args), false);

         fn = LLVMAddFunction(req->module, "__nvc_getpriv",
                              req->fntypes[which]);
      }
      break;

   case LLVM_PUTPRIV:
      {
         LLVMTypeRef args[] = {
            req->types[LLVM_INT32],
            req->types[LLVM_PTR]
         };
         req->fntypes[which] = LLVMFunctionType(req->types[LLVM_VOID], args,
                                                ARRAY_LEN(args), false);

         fn = LLVMAddFunction(req->module, "__nvc_putpriv",
                              req->fntypes[which]);
      }
      break;

   case LLVM_MSPACE_ALLOC:
      {
         LLVMTypeRef args[] = {
            req->types[LLVM_INT32],
            req->types[LLVM_INT32]
         };
         req->fntypes[which] = LLVMFunctionType(req->types[LLVM_PTR], args,
                                                ARRAY_LEN(args), false);

         fn = LLVMAddFunction(req->module, "__nvc_mspace_alloc",
                              req->fntypes[which]);
      }
      break;

   default:
      fatal_trace("cannot generate prototype for function %d", which);
   }

   return (req->fns[which] = fn);
}

static LLVMValueRef cgen_call_fn(cgen_req_t *req, llvm_fn_t which,
                                 LLVMValueRef *args, unsigned count)
{
   LLVMValueRef fn = cgen_get_fn(req, which);
   return LLVMBuildCall2(req->builder, req->fntypes[which], fn,
                         args, count, "");
}

static const char *cgen_reg_name(jit_reg_t reg)
{
#ifdef DEBUG
   static volatile int uniq = 0;
   static __thread char buf[32];
   checked_sprintf(buf, sizeof(buf), "R%d.%d", reg, relaxed_add(&uniq, 1));
   return buf;
#else
   return "";
#endif
}

static const char *cgen_arg_name(int nth)
{
#ifdef DEBUG
   static volatile int uniq = 0;
   static __thread char buf[32];
   checked_sprintf(buf, sizeof(buf), "A%d.%d", nth, relaxed_add(&uniq, 1));
   return buf;
#else
   return "";
#endif
}

static const char *cgen_istr(cgen_req_t *req, ident_t id)
{
   tb_rewind(req->textbuf);
   tb_istr(req->textbuf, id);
   return tb_get(req->textbuf);
}

static LLVMValueRef cgen_get_value(cgen_req_t *req, cgen_block_t *cgb,
                                   jit_value_t value)
{
   switch (value.kind) {
   case JIT_VALUE_REG:
      assert(value.reg < req->func->nregs);
      assert(cgb->outregs[value.reg] != NULL);
      return cgb->outregs[value.reg];
   case JIT_VALUE_INT64:
      return llvm_int64(req, value.int64);
      /*
   case JIT_VALUE_DOUBLE:
   return (jit_scalar_t){ .real = value.dval };*/
   case JIT_ADDR_FRAME:
      {
         assert(value.int64 >= 0 && value.int64 < req->func->framesz);
         LLVMValueRef indexes[] = { llvm_intptr(req, value.int64) };
         LLVMTypeRef byte_type = req->types[LLVM_INT8];
         return LLVMBuildInBoundsGEP2(req->builder, byte_type,
                                      req->frame, indexes,
                                      ARRAY_LEN(indexes), "");
      }
   case JIT_ADDR_CPOOL:
      assert(value.int64 >= 0 && value.int64 <= req->func->cpoolsz);
      return llvm_ptr(req, req->func->cpool + value.int64);
   case JIT_ADDR_REG:
      {
         assert(value.reg < req->func->nregs);
         LLVMValueRef ptr = cgb->outregs[value.reg];

         if (value.disp != 0) {
            LLVMValueRef disp = llvm_int64(req, value.disp);
            ptr = LLVMBuildAdd(req->builder, ptr, disp, "");
         }

         return ptr;
      }
      /*
   case JIT_ADDR_ABS:
      return (jit_scalar_t){ .pointer = (void *)(intptr_t)value.int64 };
   case JIT_VALUE_LABEL:
      return (jit_scalar_t){ .integer = value.label };
      */
   case JIT_VALUE_EXIT:
   case JIT_VALUE_HANDLE:
      return llvm_int32(req, value.exit);
   case JIT_ADDR_ABS:
      return llvm_ptr(req, (void *)(intptr_t)value.int64);
   default:
      fatal_trace("cannot handle value kind %d", value.kind);
   }
}

static LLVMValueRef cgen_coerce_value(cgen_req_t *req, cgen_block_t *cgb,
                                      jit_value_t value, llvm_type_t type)
{
   LLVMValueRef raw = cgen_get_value(req, cgb, value);
   LLVMTypeRef lltype = LLVMTypeOf(raw);

   switch (type) {
   case LLVM_PTR:
      if (LLVMGetTypeKind(lltype) == LLVMIntegerTypeKind)
         return LLVMBuildIntToPtr(req->builder, raw, req->types[LLVM_PTR], "");
      else
         return raw;

   case LLVM_INTPTR:
   case LLVM_INT64:
   case LLVM_INT32:
   case LLVM_INT16:
   case LLVM_INT8:
      if (LLVMGetTypeKind(lltype) == LLVMPointerTypeKind)
         return LLVMBuildIntToPtr(req->builder, raw, req->types[LLVM_PTR], "");
      else {
         const int bits1 = LLVMGetIntTypeWidth(lltype);
         const int bits2 = LLVMGetIntTypeWidth(req->types[type]);

         if (bits1 < bits2)
            return LLVMBuildSExt(req->builder, raw, req->types[type], "");
         else if (bits1 == bits2)
            return raw;
         else
            return LLVMBuildTrunc(req->builder, raw, req->types[type], "");
      }

   default:
      return raw;
   }
}

static void cgen_sext_result(cgen_req_t *req, cgen_block_t *cgb, jit_ir_t *ir,
                             LLVMValueRef value)
{
   LLVMTypeRef type = LLVMTypeOf(value);
   if (LLVMGetTypeKind(type) == LLVMIntegerTypeKind
       && LLVMGetIntTypeWidth(type) == 64) {
      DEBUG_ONLY(LLVMSetValueName(value, cgen_reg_name(ir->result)));
      cgb->outregs[ir->result] = value;
   }
   else
      cgb->outregs[ir->result] = LLVMBuildSExt(req->builder, value,
                                               req->types[LLVM_INT64],
                                               cgen_reg_name(ir->result));
}

static void cgen_zext_result(cgen_req_t *req, cgen_block_t *cgb, jit_ir_t *ir,
                             LLVMValueRef value)
{
   LLVMTypeRef type = LLVMTypeOf(value);
   if (LLVMGetTypeKind(type) == LLVMIntegerTypeKind
       && LLVMGetIntTypeWidth(type) == 64) {
      DEBUG_ONLY(LLVMSetValueName(value, cgen_reg_name(ir->result)));
      cgb->outregs[ir->result] = value;
   }
   else
      cgb->outregs[ir->result] = LLVMBuildZExt(req->builder, value,
                                               req->types[LLVM_INT64],
                                               cgen_reg_name(ir->result));
}

static void cgen_op_recv(cgen_req_t *req, cgen_block_t *cgb, jit_ir_t *ir)
{
   assert(ir->arg1.kind == JIT_VALUE_INT64);
   const int nth = ir->arg1.int64;

   assert(nth < JIT_MAX_ARGS);
   LLVMValueRef indexes[] = { llvm_int32(req, nth) };
   LLVMTypeRef int64_type = req->types[LLVM_INT64];
   LLVMValueRef ptr = LLVMBuildInBoundsGEP2(req->builder, int64_type,
                                            req->args, indexes,
                                            ARRAY_LEN(indexes),
                                            cgen_arg_name(nth));

   cgb->outregs[ir->result] = LLVMBuildLoad2(req->builder, int64_type, ptr,
                                             cgen_reg_name(ir->result));
}

static void cgen_op_send(cgen_req_t *req, cgen_block_t *cgb, jit_ir_t *ir)
{
   assert(ir->arg1.kind == JIT_VALUE_INT64);
   const int nth = ir->arg1.int64;

   LLVMValueRef value = cgen_get_value(req, cgb, ir->arg2);

   assert(nth < JIT_MAX_ARGS);
   LLVMValueRef indexes[] = { llvm_int32(req, nth) };
   LLVMTypeRef int64_type = req->types[LLVM_INT64];
   LLVMValueRef ptr = LLVMBuildInBoundsGEP2(req->builder, int64_type,
                                            req->args, indexes,
                                            ARRAY_LEN(indexes),
                                            cgen_arg_name(nth));

#ifdef LLVM_HAS_OPAQUE_POINTERS
   LLVMBuildStore(req->builder, value, ptr);
#else
   LLVMTypeRef ptr_type = LLVMPointerType(LLVMTypeOf(value), 0);
   LLVMValueRef cast = LLVMBuildPointerCast(req->builder, ptr, ptr_type, "");
   LLVMBuildStore(req->builder, value, cast);
#endif
}

static void cgen_op_store(cgen_req_t *req, cgen_block_t *cgb, jit_ir_t *ir)
{
   llvm_type_t type   = LLVM_INT8 + ir->size;
   LLVMValueRef value = cgen_coerce_value(req, cgb, ir->arg1, type);
   LLVMValueRef ptr   = cgen_coerce_value(req, cgb, ir->arg2, LLVM_PTR);

#ifdef LLVM_HAS_OPAQUE_POINTERS
   LLVMBuildStore(req->builder, value, ptr);
#else
   LLVMTypeRef ptr_type = LLVMPointerType(LLVMTypeOf(value), 0);
   LLVMValueRef cast = LLVMBuildPointerCast(req->builder, ptr, ptr_type, "");
   LLVMBuildStore(req->builder, value, cast);
#endif
}

static void cgen_op_load(cgen_req_t *req, cgen_block_t *cgb, jit_ir_t *ir)
{
   llvm_type_t type = LLVM_INT8 + ir->size;
   LLVMValueRef ptr = cgen_coerce_value(req, cgb, ir->arg1, LLVM_PTR);

   if (type == LLVM_INT64)
      cgb->outregs[ir->result] = LLVMBuildLoad2(req->builder, req->types[type],
                                                ptr, cgen_reg_name(ir->result));
   else {
      LLVMValueRef tmp =
         LLVMBuildLoad2(req->builder, req->types[type], ptr, "");
      if (ir->op == J_ULOAD)
         cgen_zext_result(req, cgb, ir, tmp);
      else
         cgen_sext_result(req, cgb, ir, tmp);
   }
}

static void cgen_op_add(cgen_req_t *req, cgen_block_t *cgb, jit_ir_t *ir)
{
   llvm_fn_t fn = LLVM_LAST_FN;
   if (ir->cc == JIT_CC_O)
      fn = LLVM_ADD_OVERFLOW_S8 + ir->size;
   else if (ir->cc == JIT_CC_C)
      fn = LLVM_ADD_OVERFLOW_U8 + ir->size;

   if (fn != LLVM_LAST_FN) {
      llvm_type_t type = LLVM_INT8 + ir->size;
      LLVMValueRef arg1 = cgen_coerce_value(req, cgb, ir->arg1, type);
      LLVMValueRef arg2 = cgen_coerce_value(req, cgb, ir->arg2, type);

      LLVMValueRef args[] = { arg1, arg2 };
      LLVMValueRef pair = cgen_call_fn(req, fn, args, 2);

      LLVMValueRef result = LLVMBuildExtractValue(req->builder, pair, 0, "");
      cgb->outflags = LLVMBuildExtractValue(req->builder, pair, 1, "FLAGS");

      if (ir->cc == JIT_CC_C)
         cgen_zext_result(req, cgb, ir, result);
      else
         cgen_sext_result(req, cgb, ir, result);
   }
   else {
      LLVMValueRef arg1 = cgen_get_value(req, cgb, ir->arg1);
      LLVMValueRef arg2 = cgen_get_value(req, cgb, ir->arg2);
      cgb->outregs[ir->result] = LLVMBuildAdd(req->builder, arg1, arg2,
                                              cgen_reg_name(ir->result));
   }
}

static void cgen_op_sub(cgen_req_t *req, cgen_block_t *cgb, jit_ir_t *ir)
{
   llvm_fn_t fn = LLVM_LAST_FN;
   if (ir->cc == JIT_CC_O)
      fn = LLVM_SUB_OVERFLOW_S8 + ir->size;
   else if (ir->cc == JIT_CC_C)
      fn = LLVM_SUB_OVERFLOW_U8 + ir->size;

   if (fn != LLVM_LAST_FN) {
      llvm_type_t type = LLVM_INT8 + ir->size;
      LLVMValueRef arg1 = cgen_coerce_value(req, cgb, ir->arg1, type);
      LLVMValueRef arg2 = cgen_coerce_value(req, cgb, ir->arg2, type);

      LLVMValueRef args[] = { arg1, arg2 };
      LLVMValueRef pair = cgen_call_fn(req, fn, args, 2);

      LLVMValueRef result = LLVMBuildExtractValue(req->builder, pair, 0, "");
      cgb->outflags = LLVMBuildExtractValue(req->builder, pair, 1, "FLAGS");

      if (ir->cc == JIT_CC_C)
         cgen_zext_result(req, cgb, ir, result);
      else
         cgen_sext_result(req, cgb, ir, result);
   }
   else {
      LLVMValueRef arg1 = cgen_get_value(req, cgb, ir->arg1);
      LLVMValueRef arg2 = cgen_get_value(req, cgb, ir->arg2);
      cgb->outregs[ir->result] = LLVMBuildSub(req->builder, arg1, arg2,
                                              cgen_reg_name(ir->result));
   }
}

static void cgen_op_mul(cgen_req_t *req, cgen_block_t *cgb, jit_ir_t *ir)
{
   llvm_fn_t fn = LLVM_LAST_FN;
   if (ir->cc == JIT_CC_O)
      fn = LLVM_MUL_OVERFLOW_S8 + ir->size;
   else if (ir->cc == JIT_CC_C)
      fn = LLVM_MUL_OVERFLOW_U8 + ir->size;

   if (fn != LLVM_LAST_FN) {
      llvm_type_t type = LLVM_INT8 + ir->size;
      LLVMValueRef arg1 = cgen_coerce_value(req, cgb, ir->arg1, type);
      LLVMValueRef arg2 = cgen_coerce_value(req, cgb, ir->arg2, type);

      LLVMValueRef args[] = { arg1, arg2 };
      LLVMValueRef pair = cgen_call_fn(req, fn, args, 2);

      LLVMValueRef result = LLVMBuildExtractValue(req->builder, pair, 0, "");
      cgb->outflags = LLVMBuildExtractValue(req->builder, pair, 1, "FLAGS");

      if (ir->cc == JIT_CC_C)
         cgen_zext_result(req, cgb, ir, result);
      else
         cgen_sext_result(req, cgb, ir, result);
   }
   else {
      LLVMValueRef arg1 = cgen_get_value(req, cgb, ir->arg1);
      LLVMValueRef arg2 = cgen_get_value(req, cgb, ir->arg2);
      cgb->outregs[ir->result] = LLVMBuildMul(req->builder, arg1, arg2,
                                              cgen_reg_name(ir->result));
   }
}

static void cgen_op_div(cgen_req_t *req, cgen_block_t *cgb, jit_ir_t *ir)
{
   LLVMValueRef arg1 = cgen_get_value(req, cgb, ir->arg1);
   LLVMValueRef arg2 = cgen_get_value(req, cgb, ir->arg2);

   cgb->outregs[ir->result] = LLVMBuildSDiv(req->builder, arg1, arg2,
                                            cgen_reg_name(ir->result));
}

static void cgen_op_rem(cgen_req_t *req, cgen_block_t *cgb, jit_ir_t *ir)
{
   LLVMValueRef arg1 = cgen_get_value(req, cgb, ir->arg1);
   LLVMValueRef arg2 = cgen_get_value(req, cgb, ir->arg2);

   cgb->outregs[ir->result] = LLVMBuildSRem(req->builder, arg1, arg2,
                                            cgen_reg_name(ir->result));
}

static void cgen_op_not(cgen_req_t *req, cgen_block_t *cgb, jit_ir_t *ir)
{
   LLVMValueRef arg1 = cgen_get_value(req, cgb, ir->arg1);
   cgb->outregs[ir->result] = LLVMBuildNot(req->builder, arg1,
                                           cgen_reg_name(ir->result));
}

static void cgen_op_and(cgen_req_t *req, cgen_block_t *cgb, jit_ir_t *ir)
{
   LLVMValueRef arg1 = cgen_get_value(req, cgb, ir->arg1);
   LLVMValueRef arg2 = cgen_get_value(req, cgb, ir->arg2);

   cgb->outregs[ir->result] = LLVMBuildAnd(req->builder, arg1, arg2,
                                           cgen_reg_name(ir->result));
}

static void cgen_op_or(cgen_req_t *req, cgen_block_t *cgb, jit_ir_t *ir)
{
   LLVMValueRef arg1 = cgen_get_value(req, cgb, ir->arg1);
   LLVMValueRef arg2 = cgen_get_value(req, cgb, ir->arg2);

   cgb->outregs[ir->result] = LLVMBuildOr(req->builder, arg1, arg2,
                                          cgen_reg_name(ir->result));
}

static void cgen_op_xor(cgen_req_t *req, cgen_block_t *cgb, jit_ir_t *ir)
{
   LLVMValueRef arg1 = cgen_get_value(req, cgb, ir->arg1);
   LLVMValueRef arg2 = cgen_get_value(req, cgb, ir->arg2);

   cgb->outregs[ir->result] = LLVMBuildXor(req->builder, arg1, arg2,
                                           cgen_reg_name(ir->result));
}

static void cgen_op_ret(cgen_req_t *req, jit_ir_t *ir)
{
   LLVMBuildRetVoid(req->builder);
}

static void cgen_op_jump(cgen_req_t *req, cgen_block_t *cgb, jit_ir_t *ir)
{
   if (ir->cc == JIT_CC_NONE) {
      assert(cgb->source->out.count == 1);
      LLVMBasicBlockRef dest = req->blocks[cgb->source->out.edges[0]].bbref;
      LLVMBuildBr(req->builder, dest);
   }
   else if (ir->cc == JIT_CC_T) {
      assert(cgb->source->out.count == 2);
      LLVMBasicBlockRef dest_t = req->blocks[cgb->source->out.edges[1]].bbref;
      LLVMBasicBlockRef dest_f = (cgb + 1)->bbref;
      LLVMBuildCondBr(req->builder, cgb->outflags, dest_t, dest_f);
   }
   else if (ir->cc == JIT_CC_F) {
      assert(cgb->source->out.count == 2);
      LLVMBasicBlockRef dest_t = req->blocks[cgb->source->out.edges[1]].bbref;
      LLVMBasicBlockRef dest_f = (cgb + 1)->bbref;
      LLVMBuildCondBr(req->builder, cgb->outflags, dest_f, dest_t);
   }
   else {
      jit_dump(req->func);
      fatal_trace("unhandled jump condition code");
   }
}

static void cgen_op_cmp(cgen_req_t *req, cgen_block_t *cgb, jit_ir_t *ir)
{
   LLVMValueRef arg1 = cgen_get_value(req, cgb, ir->arg1);
   LLVMValueRef arg2 = cgen_get_value(req, cgb, ir->arg2);

   LLVMIntPredicate pred;
   switch (ir->cc) {
   case JIT_CC_EQ: pred = LLVMIntEQ; break;
   case JIT_CC_NE: pred = LLVMIntNE; break;
   case JIT_CC_GT: pred = LLVMIntSGT; break;
   case JIT_CC_LT: pred = LLVMIntSLT; break;
   case JIT_CC_LE: pred = LLVMIntSLE; break;
   case JIT_CC_GE: pred = LLVMIntSGE; break;
   default:
      jit_dump_with_mark(req->func, ir - req->func->irbuf, false);
      fatal_trace("unhandled cmp condition code");
   }

   cgb->outflags = LLVMBuildICmp(req->builder, pred, arg1, arg2, "FLAGS");
}

static void cgen_op_cset(cgen_req_t *req, cgen_block_t *cgb, jit_ir_t *ir)
{
   cgen_zext_result(req, cgb, ir, cgb->outflags);
}

static void cgen_op_csel(cgen_req_t *req, cgen_block_t *cgb, jit_ir_t *ir)
{
   LLVMValueRef arg1 = cgen_get_value(req, cgb, ir->arg1);
   LLVMValueRef arg2 = cgen_get_value(req, cgb, ir->arg2);

   LLVMValueRef result =
      LLVMBuildSelect(req->builder, cgb->outflags, arg1, arg2,
                      cgen_reg_name(ir->result));

   cgb->outregs[ir->result] = result;
}

static void cgen_op_call(cgen_req_t *req, cgen_block_t *cgb, jit_ir_t *ir)
{
   jit_func_t *callee = jit_get_func(req->func->jit, ir->arg1.handle);
   const char *name = cgen_istr(req, callee->name);
   LLVMValueRef global = LLVMGetNamedGlobal(req->module, name);
   if (global == NULL) {
      global = LLVMAddGlobal(req->module, req->types[LLVM_PTR], name);
      LLVMSetGlobalConstant(global, true);
      LLVMSetLinkage(global, LLVMPrivateLinkage);
      LLVMSetUnnamedAddr(global, true);
      LLVMSetInitializer(global, llvm_ptr(req, callee->entry));
   }

#ifdef LLVM_HAS_OPAQUE_POINTERS
   LLVMValueRef fnptr =
      LLVMBuildLoad2(req->builder, req->types[LLVM_PTR], global, name);
#else
   LLVMTypeRef ptr_type = LLVMPointerType(req->types[LLVM_ENTRY_FN], 0);
   LLVMValueRef cast = LLVMBuildPointerCast(req->builder, global, ptr_type, "");
   LLVMValueRef fnptr = LLVMBuildLoad2(req->builder, ptr_type, cast, name);
#endif

   LLVMValueRef args[] = {
      llvm_ptr(req, callee),
      PTR(req->anchor),
      req->args
   };
   LLVMBuildCall2(req->builder, req->types[LLVM_ENTRY_FN], fnptr,
                  args, ARRAY_LEN(args), "");
}

static void cgen_op_lea(cgen_req_t *req, cgen_block_t *cgb, jit_ir_t *ir)
{
   LLVMValueRef ptr = cgen_get_value(req, cgb, ir->arg1);

   if (LLVMGetTypeKind(LLVMTypeOf(ptr)) == LLVMPointerTypeKind)
      cgb->outregs[ir->result] = LLVMBuildPtrToInt(req->builder, ptr,
                                                   req->types[LLVM_INT64],
                                                   cgen_reg_name(ir->result));
   else
      cgen_zext_result(req, cgb, ir, ptr);
}

static void cgen_op_mov(cgen_req_t *req, cgen_block_t *cgb, jit_ir_t *ir)
{
   LLVMValueRef value = cgen_get_value(req, cgb, ir->arg1);
   cgb->outregs[ir->result] = value;
}

static void cgen_op_neg(cgen_req_t *req, cgen_block_t *cgb, jit_ir_t *ir)
{
   LLVMValueRef arg1 = cgen_get_value(req, cgb, ir->arg1);
   LLVMValueRef neg =
      LLVMBuildNeg(req->builder, arg1, cgen_reg_name(ir->result));

   cgb->outregs[ir->result] = neg;
}

static void cgen_macro_exp(cgen_req_t *req, cgen_block_t *cgb, jit_ir_t *ir)
{
   LLVMValueRef arg1 = cgen_get_value(req, cgb, ir->arg1);
   LLVMValueRef arg2 = cgen_get_value(req, cgb, ir->arg1);

   // TODO: implement this without the cast
   LLVMValueRef cast[] = {
      LLVMBuildUIToFP(req->builder, arg1, req->types[LLVM_DOUBLE], ""),
      LLVMBuildUIToFP(req->builder, arg2, req->types[LLVM_DOUBLE], "")
   };
   cgb->outregs[ir->result] = LLVMBuildFPToUI(
      req->builder,
      LLVMBuildCall2(req->builder, req->fntypes[LLVM_POW_F64],
                     cgen_get_fn(req, LLVM_POW_F64), cast, 2, ""),
      req->types[LLVM_INT64], cgen_reg_name(ir->result));
}

static void cgen_macro_copy(cgen_req_t *req, cgen_block_t *cgb, jit_ir_t *ir)
{
   LLVMValueRef count = cgb->outregs[ir->result];
   LLVMValueRef dest  = cgen_coerce_value(req, cgb, ir->arg1, LLVM_PTR);
   LLVMValueRef src   = cgen_coerce_value(req, cgb, ir->arg2, LLVM_PTR);

   LLVMBuildMemMove(req->builder, dest, 0, src, 0, count);
}

static void cgen_macro_bzero(cgen_req_t *req, cgen_block_t *cgb, jit_ir_t *ir)
{
   LLVMValueRef count = cgb->outregs[ir->result];
   LLVMValueRef dest  = cgen_coerce_value(req, cgb, ir->arg1, LLVM_PTR);

   LLVMBuildMemSet(req->builder, PTR(dest), llvm_int8(req, 0), count, 0);
}

static void cgen_macro_exit(cgen_req_t *req, cgen_block_t *cgb, jit_ir_t *ir)
{
   const unsigned irpos = ir - req->func->irbuf;
   LLVMValueRef irpos_ptr = LLVMBuildStructGEP2(req->builder,
                                                req->types[LLVM_ANCHOR],
                                                req->anchor, 2, "");
   LLVMBuildStore(req->builder, llvm_int32(req, irpos), irpos_ptr);

   LLVMValueRef which = cgen_get_value(req, cgb, ir->arg1);
   LLVMValueRef fn    = cgen_get_fn(req, LLVM_DO_EXIT);

   LLVMValueRef args[] = {
      which,
      PTR(req->anchor),
      req->args
   };
   LLVMBuildCall2(req->builder, req->fntypes[LLVM_DO_EXIT], fn,
                  args, ARRAY_LEN(args), "");
}

static void cgen_macro_fficall(cgen_req_t *req, cgen_block_t *cgb, jit_ir_t *ir)
{
   const unsigned irpos = ir - req->func->irbuf;
   LLVMValueRef irpos_ptr = LLVMBuildStructGEP2(req->builder,
                                                req->types[LLVM_ANCHOR],
                                                req->anchor, 2, "");
   LLVMBuildStore(req->builder, llvm_int32(req, irpos), irpos_ptr);

   LLVMValueRef ff = cgen_get_value(req, cgb, ir->arg1);
   LLVMValueRef fn = cgen_get_fn(req, LLVM_DO_FFICALL);

   LLVMValueRef args[] = {
      ff,
      PTR(req->anchor),
      req->args
   };
   LLVMBuildCall2(req->builder, req->fntypes[LLVM_DO_FFICALL], fn,
                  args, ARRAY_LEN(args), "");
}

static void cgen_macro_galloc(cgen_req_t *req, cgen_block_t *cgb, jit_ir_t *ir)
{
   // TODO: use TLAB

   LLVMValueRef fn = cgen_get_fn(req, LLVM_MSPACE_ALLOC);
   LLVMValueRef size = cgen_get_value(req, cgb, ir->arg1);

   LLVMValueRef args[] = {
      LLVMBuildTrunc(req->builder, size, req->types[LLVM_INT32], ""),
      llvm_int32(req, 1),
   };
   LLVMValueRef ptr = LLVMBuildCall2(req->builder,
                                     req->fntypes[LLVM_MSPACE_ALLOC],
                                     fn, args, ARRAY_LEN(args), "");

   cgb->outregs[ir->result] = LLVMBuildPtrToInt(req->builder, ptr,
                                                req->types[LLVM_INT64],
                                                cgen_reg_name(ir->result));
}

static void cgen_macro_getpriv(cgen_req_t *req, cgen_block_t *cgb, jit_ir_t *ir)
{
   // TODO: this needs some kind of fast-path

   LLVMValueRef fn = cgen_get_fn(req, LLVM_GETPRIV);

   LLVMValueRef args[] = {
      cgen_get_value(req, cgb, ir->arg1)
   };
   LLVMValueRef ptr = LLVMBuildCall2(req->builder,req->fntypes[LLVM_GETPRIV],
                                     fn, args, ARRAY_LEN(args), "");

   cgb->outregs[ir->result] = LLVMBuildPtrToInt(req->builder, ptr,
                                                req->types[LLVM_INT64],
                                                cgen_reg_name(ir->result));
}

static void cgen_macro_putpriv(cgen_req_t *req, cgen_block_t *cgb, jit_ir_t *ir)
{
   LLVMValueRef fn = cgen_get_fn(req, LLVM_PUTPRIV);

   LLVMValueRef args[] = {
      cgen_get_value(req, cgb, ir->arg1),
      cgen_coerce_value(req, cgb, ir->arg2, LLVM_PTR),
   };
   LLVMBuildCall2(req->builder, req->fntypes[LLVM_PUTPRIV],
                  fn, args, ARRAY_LEN(args), "");
}

static void cgen_ir(cgen_req_t *req, cgen_block_t *cgb, jit_ir_t *ir)
{
   switch (ir->op) {
   case J_RECV:
      cgen_op_recv(req, cgb, ir);
      break;
   case J_SEND:
      cgen_op_send(req, cgb, ir);
      break;
   case J_STORE:
      cgen_op_store(req, cgb, ir);
      break;
   case J_LOAD:
   case J_ULOAD:
      cgen_op_load(req, cgb, ir);
      break;
   case J_ADD:
      cgen_op_add(req, cgb, ir);
      break;
   case J_SUB:
      cgen_op_sub(req, cgb, ir);
      break;
   case J_MUL:
      cgen_op_mul(req, cgb, ir);
      break;
   case J_DIV:
      cgen_op_div(req, cgb, ir);
      break;
   case J_REM:
      cgen_op_rem(req, cgb, ir);
      break;
   case J_NOT:
      cgen_op_not(req, cgb, ir);
      break;
   case J_AND:
      cgen_op_and(req, cgb, ir);
      break;
   case J_OR:
      cgen_op_or(req, cgb, ir);
      break;
   case J_XOR:
      cgen_op_xor(req, cgb, ir);
      break;
   case J_RET:
      cgen_op_ret(req, ir);
      break;
   case J_JUMP:
      cgen_op_jump(req, cgb, ir);
      break;
   case J_CMP:
      cgen_op_cmp(req, cgb, ir);
      break;
   case J_CSET:
      cgen_op_cset(req, cgb, ir);
      break;
   case J_CSEL:
      cgen_op_csel(req, cgb, ir);
      break;
   case J_DEBUG:
      break;
   case J_CALL:
      cgen_op_call(req, cgb, ir);
      break;
   case J_LEA:
      cgen_op_lea(req, cgb, ir);
      break;
   case J_MOV:
      cgen_op_mov(req, cgb, ir);
      break;
   case J_NEG:
      cgen_op_neg(req, cgb, ir);
      break;
   case MACRO_EXP:
      cgen_macro_exp(req, cgb, ir);
      break;
   case MACRO_COPY:
      cgen_macro_copy(req, cgb, ir);
      break;
   case MACRO_BZERO:
      cgen_macro_bzero(req, cgb, ir);
      break;
   case MACRO_EXIT:
      cgen_macro_exit(req, cgb, ir);
      break;
   case MACRO_FFICALL:
      cgen_macro_fficall(req, cgb, ir);
      break;
   case MACRO_GALLOC:
      cgen_macro_galloc(req, cgb, ir);
      break;
   case MACRO_GETPRIV:
      cgen_macro_getpriv(req, cgb, ir);
      break;
   case MACRO_PUTPRIV:
      cgen_macro_putpriv(req, cgb, ir);
      break;
   default:
      jit_dump_with_mark(req->func, ir - req->func->irbuf, false);
      fatal("cannot generate LLVM for %s", jit_op_name(ir->op));
   }
}

static void cgen_basic_blocks(cgen_req_t *req, jit_cfg_t *cfg)
{
   req->blocks = xcalloc_array(cfg->nblocks, sizeof(cgen_block_t));

   for (int i = 0; i < cfg->nblocks; i++) {
#ifdef DEBUG
      char name[32];
      checked_sprintf(name, sizeof(name), "BB%d", i);
#else
      const char *name = "";
#endif

      cgen_block_t *cgb = &(req->blocks[i]);
      cgb->bbref  = cgen_append_block(req, name);
      cgb->source = &(cfg->blocks[i]);

      cgb->inregs  = xcalloc_array(req->func->nregs, sizeof(LLVMValueRef));
      cgb->outregs = xcalloc_array(req->func->nregs, sizeof(LLVMValueRef));
   }
}

static void cgen_dump_module(cgen_req_t *req, const char *tag)
{
   size_t length;
   const char *module_name = LLVMGetModuleIdentifier(req->module, &length);

   if (!opt_get_verbose(OPT_LLVM_VERBOSE, module_name))
      return;

   LOCAL_TEXT_BUF tb = tb_new();
   tb_printf(tb, "%s.%s.ll", module_name, tag);

   char *error;
   if (LLVMPrintModuleToFile(req->module, tb_get(tb), &error))
      fatal("Failed to write LLVM IR file: %s", error);

   debugf("wrote LLVM IR for %s to %s", module_name, tb_get(tb));
}

static void cgen_frame_anchor(cgen_req_t *req)
{
   LLVMTypeRef type = req->types[LLVM_ANCHOR];
   req->anchor = LLVMBuildAlloca(req->builder, type, "anchor");

   LLVMValueRef func = LLVMGetParam(req->llvmfn, 0);
   LLVMSetValueName(func, "func");

   LLVMValueRef caller = LLVMGetParam(req->llvmfn, 1);
   LLVMSetValueName(caller, "caller");

   LLVMValueRef caller_ptr = LLVMBuildStructGEP2(req->builder, type,
                                                 req->anchor, 0, "");
   LLVMBuildStore(req->builder, caller, caller_ptr);

   LLVMValueRef func_ptr = LLVMBuildStructGEP2(req->builder, type,
                                               req->anchor, 1, "");
   LLVMBuildStore(req->builder, func, func_ptr);

   LLVMValueRef irpos_ptr = LLVMBuildStructGEP2(req->builder, type,
                                                req->anchor, 2, "");
   LLVMBuildStore(req->builder, llvm_int32(req, 0), irpos_ptr);
}

static void cgen_module(cgen_req_t *req)
{
   req->module  = LLVMModuleCreateWithNameInContext(req->name, req->context);
   req->builder = LLVMCreateBuilderInContext(req->context);

   char *triple = LLVMGetTargetMachineTriple(req->target);
   LLVMSetTarget(req->module, triple);
   LLVMDisposeMessage(triple);

   LLVMTargetDataRef data_ref = LLVMCreateTargetDataLayout(req->target);
   LLVMSetModuleDataLayout(req->module, data_ref);

   req->types[LLVM_VOID]   = LLVMVoidTypeInContext(req->context);
   req->types[LLVM_INT1]   = LLVMInt1TypeInContext(req->context);
   req->types[LLVM_INT8]   = LLVMInt8TypeInContext(req->context);
   req->types[LLVM_INT16]  = LLVMInt16TypeInContext(req->context);
   req->types[LLVM_INT32]  = LLVMInt32TypeInContext(req->context);
   req->types[LLVM_INT64]  = LLVMInt64TypeInContext(req->context);
   req->types[LLVM_INTPTR] = LLVMIntPtrTypeInContext(req->context, data_ref);
   req->types[LLVM_DOUBLE] = LLVMDoubleTypeInContext(req->context);


#ifdef LLVM_HAS_OPAQUE_POINTERS
   req->types[LLVM_PTR] = LLVMPointerTypeInContext(req->context, 0);
#else
   req->types[LLVM_PTR] = LLVMPointerType(req->types[LLVM_INT8], 0);
#endif

   LLVMTypeRef atypes[] = {
      req->types[LLVM_PTR],    // Function
      req->types[LLVM_PTR],    // Anchor
      req->types[LLVM_PTR]     // Arguments
   };
   req->types[LLVM_ENTRY_FN] = LLVMFunctionType(req->types[LLVM_VOID], atypes,
                                                ARRAY_LEN(atypes), false);

   LLVMTypeRef fields[] = {
      req->types[LLVM_PTR],    // Caller
      req->types[LLVM_PTR],    // Function
      req->types[LLVM_INT32]   // IR position
   };
   req->types[LLVM_ANCHOR] = LLVMStructTypeInContext(req->context, fields,
                                                     ARRAY_LEN(fields), false);

   req->llvmfn = LLVMAddFunction(req->module, req->name,
                                 req->types[LLVM_ENTRY_FN]);

   LLVMBasicBlockRef entry_bb = cgen_append_block(req, "entry");
   LLVMPositionBuilderAtEnd(req->builder, entry_bb);

   cgen_frame_anchor(req);

   req->args = LLVMGetParam(req->llvmfn, 2);
   LLVMSetValueName(req->args, "args");

   if (req->func->framesz > 0) {
      LLVMTypeRef frame_type =
         LLVMArrayType(req->types[LLVM_INT8], req->func->framesz);
      req->frame = LLVMBuildAlloca(req->builder, frame_type, "frame");
      LLVMSetAlignment(req->frame, sizeof(double));
   }

   jit_cfg_t *cfg = req->cfg = jit_get_cfg(req->func);
   cgen_basic_blocks(req, cfg);

   jit_dump(req->func);

   cgen_block_t *cgb = req->blocks;

   int maxin = 0;
   for (int i = 0; i < req->func->nirs; i++) {
      if (i == cgb->source->first) {
         LLVMPositionBuilderAtEnd(req->builder, cgb->bbref);

         LLVMTypeRef int1_type = req->types[LLVM_INT1];
         cgb->inflags = LLVMBuildPhi(req->builder, int1_type, "FLAGS");
         cgb->outflags = cgb->inflags;

         for (int j = 0; j < req->func->nregs; j++) {
            if (mask_test(&cgb->source->livein, j)) {
               const char *name = cgen_reg_name(j);
               LLVMValueRef init = i == 0   // Entry block
                  ? LLVMConstNull(req->types[LLVM_INT64])
                  : LLVMBuildPhi(req->builder, req->types[LLVM_INT64], name);
               cgb->inregs[j] = cgb->outregs[j] = init;
            }
         }

         maxin = MAX(maxin, cgb->source->in.count);
      }

      assert(i >= cgb->source->first && i <= cgb->source->last);

      cgen_ir(req, cgb, &(req->func->irbuf[i]));

      if (i == cgb->source->last) {
         if (cgb->source->aborts)
            LLVMBuildUnreachable(req->builder);

         if (LLVMGetBasicBlockTerminator(cgb->bbref) == NULL) {
            // Fall through to next block
            assert(!cgb->source->returns);
            assert(cgb + 1 < req->blocks + cfg->nblocks);
            LLVMBuildBr(req->builder, (++cgb)->bbref);
         }
         else
            ++cgb;
      }
   }

   LLVMValueRef flags0_in[] = { llvm_int1(req, false) };
   LLVMBasicBlockRef flags0_bb[] = { entry_bb };
   LLVMAddIncoming(req->blocks[0].inflags, flags0_in, flags0_bb, 1);

   LLVMValueRef *phi_in LOCAL = xmalloc_array(maxin, sizeof(LLVMValueRef));
   LLVMBasicBlockRef *phi_bb LOCAL =
      xmalloc_array(maxin, sizeof(LLVMBasicBlockRef));

   for (int i = 0; i < cfg->nblocks; i++) {
      jit_block_t *bb = &(cfg->blocks[i]);
      cgen_block_t *cgb = &(req->blocks[i]);

      // Flags
      for (int j = 0; j < bb->in.count; j++) {
         const int edge = jit_get_edge(&bb->in, j);
         phi_in[j] = req->blocks[edge].outflags;
         phi_bb[j] = req->blocks[edge].bbref;
      }
      LLVMAddIncoming(cgb->inflags, phi_in, phi_bb, bb->in.count);

      // Live-in registers
      for (int j = 0; j < req->func->nregs; j++) {
         if (cgb->inregs[j] != NULL) {
            for (int k = 0; k < bb->in.count; k++) {
               const int edge = jit_get_edge(&bb->in, k);
               assert(req->blocks[edge].outregs[j] != NULL);
               phi_in[k] = req->blocks[edge].outregs[j];
               phi_bb[k] = req->blocks[edge].bbref;
            }
            LLVMAddIncoming(cgb->inregs[j], phi_in, phi_bb, bb->in.count);
         }
      }
   }

   for (int i = 0; i < cfg->nblocks; i++) {
      cgen_block_t *cgb = &(req->blocks[i]);
      free(cgb->inregs);
      free(cgb->outregs);
      cgb->inregs = cgb->outregs = NULL;
   }

   LLVMPositionBuilderAtEnd(req->builder, entry_bb);
   LLVMBuildBr(req->builder, req->blocks[0].bbref);

   jit_free_cfg(req->func);
   req->cfg = cfg = NULL;

   free(req->blocks);
   req->blocks = NULL;

   LLVMDisposeBuilder(req->builder);
   req->builder = NULL;

   LLVMDisposeTargetData(data_ref);

   cgen_dump_module(req, "initial");

#ifdef DEBUG
   if (LLVMVerifyModule(req->module, LLVMPrintMessageAction, NULL))
      fatal("LLVM verification failed for %s", cgen_istr(req, req->func->name));
#endif
}

static void cgen_optimise(cgen_req_t *req)
{
   LLVMPassManagerRef fpm = LLVMCreateFunctionPassManagerForModule(req->module);

   LLVMAddScalarReplAggregatesPass(fpm);
   LLVMAddInstructionCombiningPass(fpm);
   LLVMAddReassociatePass(fpm);
   LLVMAddGVNPass(fpm);
   LLVMAddCFGSimplificationPass(fpm);

   LLVMInitializeFunctionPassManager(fpm);

   for (LLVMValueRef fn = LLVMGetFirstFunction(req->module);
        fn != NULL; fn = LLVMGetNextFunction(fn))
      LLVMRunFunctionPassManager(fpm, fn);

   LLVMFinalizeFunctionPassManager(fpm);
   LLVMDisposePassManager(fpm);

   cgen_dump_module(req, "final");
}

static void *jit_llvm_init(void)
{
   lljit_state_t *state = xcalloc(sizeof(lljit_state_t));

   LLVMInitializeNativeTarget();
   LLVMInitializeNativeAsmPrinter();

   LLVMOrcLLJITBuilderRef builder = LLVMOrcCreateLLJITBuilder();

   LLVM_CHECK(LLVMOrcCreateLLJIT, &state->jit, builder);

   state->session = LLVMOrcLLJITGetExecutionSession(state->jit);
   state->dylib   = LLVMOrcLLJITGetMainJITDylib(state->jit);
   state->context = LLVMOrcCreateNewThreadSafeContext();

   const char prefix = LLVMOrcLLJITGetGlobalPrefix(state->jit);

   LLVMOrcDefinitionGeneratorRef gen_ref;
   LLVM_CHECK(LLVMOrcCreateDynamicLibrarySearchGeneratorForProcess,
              &gen_ref, prefix, NULL, NULL);

   LLVMOrcJITDylibAddGenerator(state->dylib, gen_ref);

   return state;
}

static void jit_llvm_cgen(jit_t *j, jit_handle_t handle, void *context)
{
   lljit_state_t *state = context;

   jit_func_t *f = jit_get_func(j, handle);

   const char *only = getenv("NVC_JIT_ONLY");
   if (only != NULL && !icmp(f->name, only))
      return;

   printf("LLVM compile %s\n", istr(f->name));

   static __thread LLVMTargetMachineRef tm_ref = NULL;
   if (tm_ref == NULL) {
      char *def_triple = LLVMGetDefaultTargetTriple();
      char *error;
      LLVMTargetRef target_ref;
      if (LLVMGetTargetFromTriple(def_triple, &target_ref, &error))
         fatal("failed to get LLVM target for %s: %s", def_triple, error);

      tm_ref = LLVMCreateTargetMachine(target_ref, def_triple, "", "",
                                       LLVMCodeGenLevelDefault,
                                       LLVMRelocDefault,
                                       LLVMCodeModelJITDefault);
      LLVMDisposeMessage(def_triple);
   }

   LOCAL_TEXT_BUF tb = tb_new();
   tb_istr(tb, f->name);

   cgen_req_t req = {
      .context = LLVMOrcThreadSafeContextGetContext(state->context),
      .target  = tm_ref,
      .name    = tb_claim(tb),
      .func    = f,
      .textbuf = tb_new(),
   };
   cgen_module(&req);
   cgen_optimise(&req);

   LLVMOrcThreadSafeModuleRef tsm =
      LLVMOrcCreateNewThreadSafeModule(req.module, state->context);
   LLVMOrcLLJITAddLLVMIRModule(state->jit, state->dylib, tsm);

   LLVMOrcJITTargetAddress addr;
   LLVM_CHECK(LLVMOrcLLJITLookup, state->jit, &addr, req.name);

   printf("%s at %p\n", req.name, (void *)addr);

   atomic_store(&f->entry, (jit_entry_fn_t)addr);

   tb_free(req.textbuf);
   free(req.name);
}

static void jit_llvm_cleanup(void *context)
{
   lljit_state_t *state = context;

   LLVMOrcDisposeThreadSafeContext(state->context);
   LLVMOrcDisposeLLJIT(state->jit);

   free(state);
}

const jit_plugin_t jit_llvm = {
   .init    = jit_llvm_init,
   .cgen    = jit_llvm_cgen,
   .cleanup = jit_llvm_cleanup
};

#endif  // LLVM_HAS_LLJIT
