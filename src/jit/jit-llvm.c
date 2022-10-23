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
#include "jit/jit-priv.h"
#include "opt.h"
#include "thread.h"

#ifdef LLVM_HAS_LLJIT

#include <assert.h>
#include <stdlib.h>

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

   LLVM_DO_EXIT,

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
   llvm_type_t         *regtypes;
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

static LLVMValueRef llvm_int1(cgen_req_t *req, bool b)
{
   return LLVMConstInt(req->types[LLVM_INT1], b, false);
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

   case LLVM_DO_EXIT:
      {
         LLVMTypeRef args[] = { req->types[LLVM_INT32], req->types[LLVM_PTR] };
         req->fntypes[which] = LLVMFunctionType(req->types[LLVM_VOID], args,
                                                ARRAY_LEN(args), false);

         fn = LLVMAddFunction(req->module, "__nvc_do_exit",
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
      return cgb->outregs[value.reg];
   case JIT_VALUE_INT64:
      return llvm_int64(req, value.int64);
      /*
   case JIT_VALUE_DOUBLE:
   return (jit_scalar_t){ .real = value.dval };*/
   case JIT_ADDR_FRAME:
      {
         assert(value.int64 >= 0 && value.int64 < req->func->framesz);
         LLVMValueRef indexes[] = { llvm_int32(req, value.int64) };
         LLVMTypeRef byte_type = req->types[LLVM_INT8];
         return LLVMBuildInBoundsGEP2(req->builder, byte_type,
                                      req->frame, indexes,
                                      ARRAY_LEN(indexes), "");
      }
   case JIT_ADDR_CPOOL:
      assert(value.int64 >= 0 && value.int64 <= req->func->cpoolsz);
      return llvm_ptr(req, req->func->cpool + value.int64);
   case JIT_ADDR_REG:
      assert(value.reg < req->func->nregs);
      return LLVMBuildIntToPtr(req->builder, cgb->outregs[value.reg],
                               req->types[LLVM_PTR], "");
      /*
   case JIT_ADDR_ABS:
      return (jit_scalar_t){ .pointer = (void *)(intptr_t)value.int64 };
   case JIT_VALUE_LABEL:
      return (jit_scalar_t){ .integer = value.label };
      */
   case JIT_VALUE_EXIT:
      return llvm_int32(req, value.exit);
   default:
      fatal_trace("cannot handle value kind %d", value.kind);
   }
}

static void cgen_coerece(cgen_req_t *req, jit_size_t size, LLVMValueRef *arg1,
                         LLVMValueRef *arg2)
{
   if (size != JIT_SZ_UNSPEC) {
      LLVMTypeRef int_type = req->types[LLVM_INT8 + size];
      *arg1 = LLVMBuildTrunc(req->builder, *arg1, int_type, "");
      *arg2 = LLVMBuildTrunc(req->builder, *arg2, int_type, "");
   }
   else {
      LLVMTypeRef type1 = LLVMTypeOf(*arg1);
      LLVMTypeRef type2 = LLVMTypeOf(*arg2);

      const int bits1 = LLVMGetIntTypeWidth(type1);
      const int bits2 = LLVMGetIntTypeWidth(type2);

      if (bits1 < bits2)
         *arg1 = LLVMBuildSExt(req->builder, *arg1, type2, "");
      else
         *arg2 = LLVMBuildSExt(req->builder, *arg2, type1, "");
   }
}

static LLVMValueRef cgen_zext_to_intptr(cgen_req_t *req, LLVMValueRef value)
{
   return LLVMBuildZExt(req->builder, value, req->types[LLVM_INTPTR], "");
}

static void cgen_op_recv(cgen_req_t *req, cgen_block_t *cgb, jit_ir_t *ir)
{
   assert(ir->arg1.kind == JIT_VALUE_INT64);
   const int nth = ir->arg1.int64;

   assert(nth < JIT_MAX_ARGS);
   LLVMValueRef indexes[] = { llvm_int32(req, nth) };
   LLVMTypeRef int64_type = req->types[LLVM_INT64];
   LLVMTypeRef result_type = req->types[req->regtypes[ir->result]];
   LLVMValueRef ptr = LLVMBuildInBoundsGEP2(req->builder, int64_type,
                                            req->args, indexes,
                                            ARRAY_LEN(indexes), "");
   LLVMValueRef value = LLVMBuildLoad2(req->builder, result_type, ptr,
                                       cgen_reg_name(ir->result));

   cgb->outregs[ir->result] = value;
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
                                            ARRAY_LEN(indexes), "");
   LLVMBuildStore(req->builder, value, ptr);
}

static void cgen_op_store(cgen_req_t *req, cgen_block_t *cgb, jit_ir_t *ir)
{
   LLVMValueRef value = cgen_get_value(req, cgb, ir->arg1);
   LLVMValueRef ptr   = cgen_get_value(req, cgb, ir->arg2);

   LLVMValueRef trunc = value;
   switch (ir->size) {
   case JIT_SZ_8:
      trunc = LLVMBuildTrunc(req->builder, value, req->types[LLVM_INT8], "");
      break;
   case JIT_SZ_16:
      trunc = LLVMBuildTrunc(req->builder, value, req->types[LLVM_INT16], "");
      break;
   case JIT_SZ_32:
      trunc = LLVMBuildTrunc(req->builder, value, req->types[LLVM_INT32], "");
      break;
   case JIT_SZ_64:
   case JIT_SZ_UNSPEC:
      break;
   }

   LLVMBuildStore(req->builder, trunc, ptr);
}

static void cgen_op_load(cgen_req_t *req, cgen_block_t *cgb, jit_ir_t *ir)
{
   LLVMValueRef ptr = cgen_get_value(req, cgb, ir->arg1);

   LLVMValueRef result = NULL;
   switch (ir->size) {
   case JIT_SZ_8:
      result = LLVMBuildLoad2(req->builder, req->types[LLVM_INT8], ptr, "");
      break;
   case JIT_SZ_16:
      result = LLVMBuildLoad2(req->builder, req->types[LLVM_INT16], ptr, "");
      break;
   case JIT_SZ_32:
      result = LLVMBuildLoad2(req->builder, req->types[LLVM_INT32], ptr, "");
      break;
   case JIT_SZ_64:
   case JIT_SZ_UNSPEC:
      result = LLVMBuildLoad2(req->builder, req->types[LLVM_INT64], ptr, "");
      break;
   }

   cgb->outregs[ir->result] = result;
}

static void cgen_op_add(cgen_req_t *req, cgen_block_t *cgb, jit_ir_t *ir)
{
   LLVMValueRef arg1 = cgen_get_value(req, cgb, ir->arg1);
   LLVMValueRef arg2 = cgen_get_value(req, cgb, ir->arg2);

   if (LLVMGetTypeKind(LLVMTypeOf(arg1)) == LLVMPointerTypeKind) {
      LLVMValueRef indexes[] = { cgen_zext_to_intptr(req, arg2) };
      LLVMValueRef result = LLVMBuildGEP2(req->builder, req->types[LLVM_INT8],
                                          arg1, indexes, ARRAY_LEN(indexes),
                                          cgen_reg_name(ir->result));
      cgb->outregs[ir->result] = result;
   }
   else {
      cgen_coerece(req, ir->size, &arg1, &arg2);

      llvm_fn_t fn = LLVM_LAST_FN;
      if (ir->cc == JIT_CC_O)
         fn = LLVM_ADD_OVERFLOW_S8 + ir->size;
      else if (ir->cc == JIT_CC_C)
         fn = LLVM_ADD_OVERFLOW_U8 + ir->size;

      LLVMValueRef result;
      if (fn != LLVM_LAST_FN) {
         LLVMValueRef args[] = { arg1, arg2 };
         LLVMValueRef pair = cgen_call_fn(req, fn, args, 2);

         result = LLVMBuildExtractValue(req->builder, pair, 0, "");
         cgb->outflags = LLVMBuildExtractValue(req->builder, pair, 1, "");
      }
      else
         result = LLVMBuildAdd(req->builder, arg1, arg2, "");

      cgb->outregs[ir->result] = result;
   }
}

static void cgen_op_sub(cgen_req_t *req, cgen_block_t *cgb, jit_ir_t *ir)
{
   LLVMValueRef arg1 = cgen_get_value(req, cgb, ir->arg1);
   LLVMValueRef arg2 = cgen_get_value(req, cgb, ir->arg2);

   cgen_coerece(req, ir->size, &arg1, &arg2);

   llvm_fn_t fn = LLVM_LAST_FN;
   if (ir->cc == JIT_CC_O)
      fn = LLVM_SUB_OVERFLOW_S8 + ir->size;
   else if (ir->cc == JIT_CC_C)
      fn = LLVM_SUB_OVERFLOW_U8 + ir->size;

   LLVMValueRef result;
   if (fn != LLVM_LAST_FN) {
      LLVMValueRef args[] = { arg1, arg2 };
      LLVMValueRef pair = cgen_call_fn(req, fn, args, 2);

      result = LLVMBuildExtractValue(req->builder, pair, 0, "");
      cgb->outflags = LLVMBuildExtractValue(req->builder, pair, 1, "");
   }
   else
      result = LLVMBuildSub(req->builder, arg1, arg2, "");

   cgb->outregs[ir->result] = result;
}

static void cgen_op_mul(cgen_req_t *req, cgen_block_t *cgb, jit_ir_t *ir)
{
   LLVMValueRef arg1 = cgen_get_value(req, cgb, ir->arg1);
   LLVMValueRef arg2 = cgen_get_value(req, cgb, ir->arg2);

   cgen_coerece(req, ir->size, &arg1, &arg2);

   if (ir->size != JIT_SZ_UNSPEC) {
      LLVMTypeRef int_type = req->types[LLVM_INT8 + ir->size];
      arg1 = LLVMBuildTrunc(req->builder, arg1, int_type, "");
      arg2 = LLVMBuildTrunc(req->builder, arg2, int_type, "");
   }

   llvm_fn_t fn = LLVM_LAST_FN;
   if (ir->cc == JIT_CC_O)
      fn = LLVM_MUL_OVERFLOW_S8 + ir->size;
   else if (ir->cc == JIT_CC_C)
      fn = LLVM_MUL_OVERFLOW_U8 + ir->size;

   LLVMValueRef result;
   if (fn != LLVM_LAST_FN) {
      LLVMValueRef args[] = { arg1, arg2 };
      LLVMValueRef pair = cgen_call_fn(req, fn, args, 2);

      result = LLVMBuildExtractValue(req->builder, pair, 0, "");
      cgb->outflags = LLVMBuildExtractValue(req->builder, pair, 1, "");
   }
   else
      result = LLVMBuildMul(req->builder, arg1, arg2, "");

   cgb->outregs[ir->result] = result;
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

   cgen_coerece(req, JIT_SZ_UNSPEC, &arg1, &arg2);

   switch (ir->cc) {
   case JIT_CC_EQ:
      cgb->outflags = LLVMBuildICmp(req->builder, LLVMIntEQ, arg1, arg2, "");
      break;
   case JIT_CC_NE:
      cgb->outflags = LLVMBuildICmp(req->builder, LLVMIntNE, arg1, arg2, "");
      break;
   case JIT_CC_GT:
      cgb->outflags = LLVMBuildICmp(req->builder, LLVMIntSGT, arg1, arg2, "");
      break;
   case JIT_CC_LT:
      cgb->outflags = LLVMBuildICmp(req->builder, LLVMIntSLT, arg1, arg2, "");
      break;
   default:
      jit_dump_with_mark(req->func, ir - req->func->irbuf, false);
      fatal_trace("unhandled cmp condition code");
   }
}

static void cgen_op_cset(cgen_req_t *req, cgen_block_t *cgb, jit_ir_t *ir)
{
   cgb->outregs[ir->result] = cgb->outflags;
}

static void cgen_op_csel(cgen_req_t *req, cgen_block_t *cgb, jit_ir_t *ir)
{
   LLVMValueRef arg1 = cgen_get_value(req, cgb, ir->arg1);
   LLVMValueRef arg2 = cgen_get_value(req, cgb, ir->arg2);

   LLVMValueRef result =
      LLVMBuildSelect(req->builder, cgb->outflags, arg1, arg2, "");
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

   LLVMValueRef fnptr =
      LLVMBuildLoad2(req->builder, req->types[LLVM_PTR], global, "");

   LLVMValueRef args[] = {
      llvm_ptr(req, callee),
      req->anchor,
      req->args
   };
   LLVMBuildCall2(req->builder, req->types[LLVM_ENTRY_FN], fnptr,
                  args, ARRAY_LEN(args), "");
}

static void cgen_op_lea(cgen_req_t *req, cgen_block_t *cgb, jit_ir_t *ir)
{
   LLVMValueRef ptr = cgen_get_value(req, cgb, ir->arg1);
   assert(LLVMGetTypeKind(LLVMTypeOf(ptr)) == LLVMPointerTypeKind);
   cgb->outregs[ir->result] = ptr;
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

static void cgen_macro_copy(cgen_req_t *req, cgen_block_t *cgb, jit_ir_t *ir)
{
   LLVMValueRef count = cgb->outregs[ir->result];
   LLVMValueRef dest  = cgen_get_value(req, cgb, ir->arg1);
   LLVMValueRef src   = cgen_get_value(req, cgb, ir->arg2);

   LLVMBuildMemMove(req->builder, dest, 0, src, 0, count);
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
      req->args
   };
   LLVMBuildCall2(req->builder, req->fntypes[LLVM_DO_EXIT], fn,
                  args, ARRAY_LEN(args), "");
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
   case MACRO_COPY:
      cgen_macro_copy(req, cgb, ir);
      break;
   case MACRO_EXIT:
      cgen_macro_exit(req, cgb, ir);
      break;
   default:
      warnf("cannot generate LLVM for %s", jit_op_name(ir->op));
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

static void cgen_dump_reg_types(cgen_req_t *req)
{
   for (int i = 0; i < req->func->nregs; i++) {
      const char *names[] = {
         "void", "ptr", "int1", "int8", "int16", "int32", "int64", "intptr"
      };
      assert(req->regtypes[i] <= LLVM_INTPTR);
      printf("R%-2d : %s\n", i, names[req->regtypes[i]]);
   }
}

static void cgen_force_reg_type(cgen_req_t *req, jit_reg_t reg,
                                llvm_type_t type)
{
   if (req->regtypes[reg] == LLVM_VOID || req->regtypes[reg] == LLVM_INTPTR)
      req->regtypes[reg] = type;
   else if (type == LLVM_INTPTR)
      return;
   else if (req->regtypes[reg] != type) {
      jit_dump(req->func);
      cgen_dump_reg_types(req);
      fatal_trace("inconsistent type for R%d: %d vs %d", reg,
                  req->regtypes[reg], type);
   }
}

static void cgen_force_reg_size(cgen_req_t *req, jit_reg_t reg, jit_size_t sz,
                                llvm_type_t deftype)
{
   assert(deftype == LLVM_INTPTR);

   llvm_type_t type = deftype;
   switch (sz) {
   case JIT_SZ_8:  type = LLVM_INT8; break;
   case JIT_SZ_16: type = LLVM_INT16; break;
   case JIT_SZ_32: type = LLVM_INT32; break;
   case JIT_SZ_64: type = LLVM_INT64; break;
   case JIT_SZ_UNSPEC: break;
   }

   cgen_force_reg_type(req, reg, type);
}

static void cgen_hint_value_size(cgen_req_t *req, jit_value_t value,
                                 jit_size_t sz, llvm_type_t deftype)
{
   switch (value.kind) {
   case JIT_VALUE_REG:
      cgen_force_reg_size(req, value.reg, sz, deftype);
      break;

   default:
      break;
   }
}

static void cgen_constrain_type(cgen_req_t *req, jit_reg_t reg,
                                jit_value_t value)
{
   switch (value.kind) {
   case JIT_VALUE_REG:
      req->regtypes[reg] = req->regtypes[value.reg];
      break;

   case JIT_VALUE_INT64:
      req->regtypes[reg] = LLVM_INTPTR;
      break;

   default:
      fatal_trace("cannot handle kind %d in cgen_constrain_type", value.kind);
   }
}

static void cgen_reg_types(cgen_req_t *req)
{
   req->regtypes = xcalloc_array(req->func->nregs, sizeof(llvm_type_t));

   for (int i = 0; i < req->func->nirs; i++) {
      jit_ir_t *ir = &(req->func->irbuf[i]);
      switch (ir->op) {
      case J_RECV:
         cgen_force_reg_type(req, ir->result, LLVM_INTPTR);
         break;

      case J_LOAD:
      case J_ULOAD:
      case J_MUL:
      case J_ADD:
      case J_SUB:
         cgen_force_reg_size(req, ir->result, ir->size, LLVM_INTPTR);
         cgen_hint_value_size(req, ir->arg1, ir->size, LLVM_INTPTR);
         cgen_hint_value_size(req, ir->arg2, ir->size, LLVM_INTPTR);
         break;

      case J_CSET:
         cgen_force_reg_type(req, ir->result, LLVM_INT1);
         break;

      case J_STORE:
         cgen_hint_value_size(req, ir->arg1, ir->size, LLVM_INTPTR);
         break;

      case J_CSEL:
         if (ir->arg1.kind == JIT_VALUE_REG)
            cgen_force_reg_type(req, ir->result, req->regtypes[ir->arg1.reg]);
         else if (ir->arg2.kind == JIT_VALUE_REG)
            cgen_force_reg_type(req, ir->result, req->regtypes[ir->arg2.reg]);
         else
            cgen_force_reg_type(req, ir->result, LLVM_INTPTR);
         break;

      case J_LEA:
      case MACRO_GETPRIV:
         cgen_force_reg_type(req, ir->result, LLVM_PTR);
         break;

      case J_MOV:
      case J_NEG:
         cgen_constrain_type(req, ir->result, ir->arg1);
         break;

      case J_JUMP:
      case J_CMP:
      case J_SEND:
      case J_DEBUG:
      case J_RET:
      case J_CALL:
         break;

      case MACRO_EXIT:
      case MACRO_COPY:
         break;

      default:
         jit_dump_with_mark(req->func, i, false);
         fatal_trace("cannot infer result type for %s", jit_op_name(ir->op));
      }
   }

   cgen_dump_reg_types(req);
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
   req->types[LLVM_PTR]    = LLVMPointerTypeInContext(req->context, 0);
   req->types[LLVM_INT1]   = LLVMInt1TypeInContext(req->context);
   req->types[LLVM_INT8]   = LLVMInt8TypeInContext(req->context);
   req->types[LLVM_INT16]  = LLVMInt16TypeInContext(req->context);
   req->types[LLVM_INT32]  = LLVMInt32TypeInContext(req->context);
   req->types[LLVM_INT64]  = LLVMInt64TypeInContext(req->context);
   req->types[LLVM_INTPTR] = LLVMIntPtrTypeInContext(req->context, data_ref);

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

   cgen_reg_types(req);

   jit_dump(req->func);

   cgen_block_t *cgb = req->blocks;

   int maxin = 0;
   for (int i = 0; i < req->func->nirs; i++) {
      if (i == cgb->source->first) {
         LLVMPositionBuilderAtEnd(req->builder, cgb->bbref);

         LLVMTypeRef int1_type = req->types[LLVM_INT1];
         cgb->inflags = LLVMBuildPhi(req->builder, int1_type, "flags");
         cgb->outflags = cgb->inflags;

         for (int j = 0; j < req->func->nregs; j++) {
            if (mask_test(&cgb->source->livein, j)) {
               LLVMTypeRef type = req->types[req->regtypes[j]];
               const char *name = cgen_reg_name(j);
               cgb->inregs[j] = cgb->outregs[j] =
                  LLVMBuildPhi(req->builder, type, name);
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

   free(req->regtypes);
   req->regtypes = NULL;

   LLVMDisposeBuilder(req->builder);
   req->builder = NULL;

   LLVMDisposeTargetData(data_ref);

   LLVMDumpModule(req->module);

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

   LLVMDumpModule(req->module);
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
