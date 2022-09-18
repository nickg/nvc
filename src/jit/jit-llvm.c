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

   LLVM_PAIR_I8_I1,
   LLVM_PAIR_I16_I1,
   LLVM_PAIR_I32_I1,
   LLVM_PAIR_I64_I1,

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

   LLVM_MUL_OVERFLOW_S8,
   LLVM_MUL_OVERFLOW_S16,
   LLVM_MUL_OVERFLOW_S32,
   LLVM_MUL_OVERFLOW_S64,

   LLVM_MUL_OVERFLOW_U8,
   LLVM_MUL_OVERFLOW_U16,
   LLVM_MUL_OVERFLOW_U32,
   LLVM_MUL_OVERFLOW_U64,

   LLVM_LAST_FN,
} llvm_fn_t;

typedef struct {
   LLVMBasicBlockRef  bbref;
   LLVMValueRef       inflags;
   LLVMValueRef       outflags;
   jit_block_t       *source;
} cgen_block_t;

typedef struct {
   LLVMModuleRef        module;
   LLVMContextRef       context;
   LLVMBuilderRef       builder;
   LLVMTargetMachineRef target;
   LLVMValueRef         llvmfn;
   LLVMValueRef         regs;
   LLVMValueRef         args;
   LLVMValueRef         frame;
   LLVMTypeRef          types[LLVM_LAST_TYPE];
   LLVMValueRef         fns[LLVM_LAST_FN];
   LLVMTypeRef          fntypes[LLVM_LAST_FN];
   cgen_block_t        *blocks;
   jit_func_t          *func;
   jit_cfg_t           *cfg;
   char                *name;
} cgen_req_t;

typedef struct {
   LLVMOrcThreadSafeContextRef context;
   LLVMOrcLLJITRef             jit;
   LLVMOrcExecutionSessionRef  session;
   LLVMOrcJITDylibRef          dylib;
} lljit_state_t;

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
   static __thread char buf[32];
   checked_sprintf(buf, sizeof(buf), "R%d", reg);
   return buf;
#else
   return "";
#endif
}

static void cgen_store_reg(cgen_req_t *req, jit_reg_t reg, LLVMValueRef value)
{
   LLVMValueRef indexes[] = { llvm_int32(req, reg) };
   LLVMTypeRef int64_type = req->types[LLVM_INT64];
   LLVMValueRef ptr = LLVMBuildInBoundsGEP2(req->builder, int64_type,
                                            req->regs, indexes,
                                            ARRAY_LEN(indexes), "");
   LLVMBuildStore(req->builder, value, ptr);
}

static LLVMValueRef cgen_get_value(cgen_req_t *req, jit_value_t value)
{
   switch (value.kind) {
   case JIT_VALUE_REG:
      {
         assert(value.reg < req->func->nregs);
         LLVMValueRef indexes[] = { llvm_int32(req, value.reg) };
         LLVMTypeRef int64_type = req->types[LLVM_INT64];
         LLVMValueRef ptr = LLVMBuildInBoundsGEP2(req->builder, int64_type,
                                                  req->regs, indexes,
                                                  ARRAY_LEN(indexes), "");
         return LLVMBuildLoad2(req->builder, int64_type, ptr,
                               cgen_reg_name(value.reg));
      }
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
                                      req->args, indexes,
                                      ARRAY_LEN(indexes), "");
      }
      /*
   case JIT_ADDR_CPOOL:
      JIT_ASSERT(value.int64 >= 0 && value.int64 <= state->func->cpoolsz);
      return (jit_scalar_t){ .pointer = state->func->cpool + value.int64 };
   case JIT_ADDR_REG:
      JIT_ASSERT(value.reg < state->func->nregs);
      return (jit_scalar_t){
         .pointer = state->regs[value.reg].pointer + value.disp
      };
   case JIT_ADDR_ABS:
      return (jit_scalar_t){ .pointer = (void *)(intptr_t)value.int64 };
   case JIT_VALUE_LABEL:
      return (jit_scalar_t){ .integer = value.label };
      */
   default:
      fatal_trace("cannot handle value kind %d", value.kind);
   }
}

static void cgen_op_recv(cgen_req_t *req, jit_ir_t *ir)
{
   assert(ir->arg1.kind == JIT_VALUE_INT64);
   const int nth = ir->arg1.int64;

   assert(nth < JIT_MAX_ARGS);
   LLVMValueRef indexes[] = { llvm_int32(req, nth) };
   LLVMTypeRef int64_type = req->types[LLVM_INT64];
   LLVMValueRef ptr = LLVMBuildInBoundsGEP2(req->builder, int64_type,
                                            req->args, indexes,
                                            ARRAY_LEN(indexes), "");
   LLVMValueRef value = LLVMBuildLoad2(req->builder, int64_type, ptr, "recv");

   cgen_store_reg(req, ir->result, value);
}

static void cgen_op_send(cgen_req_t *req, jit_ir_t *ir)
{
   assert(ir->arg1.kind == JIT_VALUE_INT64);
   const int nth = ir->arg1.int64;

   LLVMValueRef value = cgen_get_value(req, ir->arg2);

   assert(nth < JIT_MAX_ARGS);
   LLVMValueRef indexes[] = { llvm_int32(req, nth) };
   LLVMTypeRef int64_type = req->types[LLVM_INT64];
   LLVMValueRef ptr = LLVMBuildInBoundsGEP2(req->builder, int64_type,
                                            req->args, indexes,
                                            ARRAY_LEN(indexes), "");
   LLVMBuildStore(req->builder, value, ptr);
}

static void cgen_op_store(cgen_req_t *req, jit_ir_t *ir)
{
   LLVMValueRef value = cgen_get_value(req, ir->arg1);
   LLVMValueRef ptr   = cgen_get_value(req, ir->arg2);

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

static void cgen_op_load(cgen_req_t *req, jit_ir_t *ir)
{
   LLVMValueRef ptr = cgen_get_value(req, ir->arg1);

   LLVMValueRef result;
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

   cgen_store_reg(req, ir->result, result);
}

static void cgen_op_add(cgen_req_t *req, cgen_block_t *cgb, jit_ir_t *ir)
{
   LLVMValueRef arg1 = cgen_get_value(req, ir->arg1);
   LLVMValueRef arg2 = cgen_get_value(req, ir->arg2);

   if (ir->size != JIT_SZ_UNSPEC) {
      LLVMTypeRef int_type = req->types[LLVM_INT8 + ir->size];
      arg1 = LLVMBuildTrunc(req->builder, arg1, int_type, "");
      arg2 = LLVMBuildTrunc(req->builder, arg2, int_type, "");
   }

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

   cgen_store_reg(req, ir->result, result);
}

static void cgen_op_mul(cgen_req_t *req, cgen_block_t *cgb, jit_ir_t *ir)
{
   LLVMValueRef arg1 = cgen_get_value(req, ir->arg1);
   LLVMValueRef arg2 = cgen_get_value(req, ir->arg2);

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

   cgen_store_reg(req, ir->result, result);
}

static void cgen_op_ret(cgen_req_t *req, jit_ir_t *ir)
{
   LLVMBuildRet(req->builder, llvm_int1(req, true));
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
      LLVMBasicBlockRef dest_t = req->blocks[cgb->source->out.edges[0]].bbref;
      LLVMBasicBlockRef dest_f = req->blocks[cgb->source->out.edges[1]].bbref;
      LLVMBuildCondBr(req->builder, cgb->outflags, dest_t, dest_f);
   }
   else if (ir->cc == JIT_CC_F) {
      assert(cgb->source->out.count == 2);
      LLVMBasicBlockRef dest_t = req->blocks[cgb->source->out.edges[0]].bbref;
      LLVMBasicBlockRef dest_f = req->blocks[cgb->source->out.edges[1]].bbref;
      LLVMBuildCondBr(req->builder, cgb->outflags, dest_f, dest_t);
   }
   else {
      jit_dump(req->func);
      fatal_trace("unhandled jump condition code");
   }
}

static void cgen_op_cmp(cgen_req_t *req, cgen_block_t *cgb, jit_ir_t *ir)
{
   LLVMValueRef arg1 = cgen_get_value(req, ir->arg1);
   LLVMValueRef arg2 = cgen_get_value(req, ir->arg2);

   switch (ir->cc) {
   case JIT_CC_EQ:
      cgb->outflags = LLVMBuildICmp(req->builder, LLVMIntEQ, arg1, arg2, "");
      break;
   case JIT_CC_GT:
      cgb->outflags = LLVMBuildICmp(req->builder, LLVMIntSGT, arg1, arg2, "");
      break;
   default:
      jit_dump(req->func);
      fatal_trace("unhandled cmp condition code");
   }
}

static void cgen_op_cset(cgen_req_t *req, cgen_block_t *cgb, jit_ir_t *ir)
{
   LLVMValueRef result = LLVMBuildZExt(req->builder, cgb->outflags,
                                       req->types[LLVM_INT64], "");
   cgen_store_reg(req, ir->result, result);
}

static void cgen_ir(cgen_req_t *req, cgen_block_t *cgb, jit_ir_t *ir)
{
   switch (ir->op) {
   case J_RECV:
      cgen_op_recv(req, ir);
      break;
   case J_SEND:
      cgen_op_send(req, ir);
      break;
   case J_STORE:
      cgen_op_store(req, ir);
      break;
   case J_LOAD:
      cgen_op_load(req, ir);
      break;
   case J_ADD:
      cgen_op_add(req, cgb, ir);
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
   }
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

   req->types[LLVM_VOID]  = LLVMVoidTypeInContext(req->context);
   req->types[LLVM_PTR]   = LLVMPointerTypeInContext(req->context, 0);
   req->types[LLVM_INT1]  = LLVMInt1TypeInContext(req->context);
   req->types[LLVM_INT8]  = LLVMInt8TypeInContext(req->context);
   req->types[LLVM_INT16] = LLVMInt16TypeInContext(req->context);
   req->types[LLVM_INT32] = LLVMInt32TypeInContext(req->context);
   req->types[LLVM_INT64] = LLVMInt64TypeInContext(req->context);

   LLVMTypeRef atypes[] = { req->types[LLVM_PTR], req->types[LLVM_PTR] };
   LLVMTypeRef fntype = LLVMFunctionType(req->types[LLVM_INT1], atypes,
                                         ARRAY_LEN(atypes), false);

   req->llvmfn = LLVMAddFunction(req->module, req->name, fntype);

   LLVMBasicBlockRef entry_bb = cgen_append_block(req, "entry");
   LLVMPositionBuilderAtEnd(req->builder, entry_bb);

   req->args = LLVMGetParam(req->llvmfn, 1);
   LLVMSetValueName(req->args, "args");

   LLVMTypeRef regs_type =
      LLVMArrayType(req->types[LLVM_INT64], req->func->nregs);
   req->regs = LLVMBuildAlloca(req->builder, regs_type, "regs");

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

   for (int i = 0; i < req->func->nirs; i++) {
      if (i == cgb->source->first) {
         LLVMPositionBuilderAtEnd(req->builder, cgb->bbref);

         LLVMTypeRef int1_type = req->types[LLVM_INT1];
         cgb->inflags = LLVMBuildPhi(req->builder, int1_type, "flags");
         cgb->outflags = cgb->inflags;
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

   for (int i = 0; i < cfg->nblocks; i++) {
      jit_block_t *bb = &(cfg->blocks[i]);
      cgen_block_t *cgb = &(req->blocks[i]);

      LLVMValueRef *phi_in LOCAL =
         xmalloc_array(bb->in.count, sizeof(LLVMValueRef));
      LLVMBasicBlockRef *phi_bb LOCAL =
         xmalloc_array(bb->in.count, sizeof(LLVMBasicBlockRef));

      for (int j = 0; j < bb->in.count; j++) {
         const int edge = bb->in.edges[j];
         phi_in[j] = req->blocks[edge].outflags;
         phi_bb[j] = req->blocks[edge].bbref;
      }

      LLVMAddIncoming(cgb->inflags, phi_in, phi_bb, bb->in.count);
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

   LLVMDumpModule(req->module);

#ifndef NDEBUG
   if (LLVMVerifyModule(req->module, LLVMPrintMessageAction, NULL))
      fatal("LLVM verification failed");
#endif
}

static void cgen_optimise(cgen_req_t *req)
{
   LLVMPassManagerRef fpm = LLVMCreateFunctionPassManagerForModule(req->module);

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

   LLVMErrorRef error = LLVMOrcCreateLLJIT(&state->jit, builder);
   if (error != LLVMErrorSuccess) {
      char *msg = LLVMGetErrorMessage(error);
      fatal("LLVMOrcCreateLLJIT failed: %s", msg);
   }

   state->session = LLVMOrcLLJITGetExecutionSession(state->jit);
   state->dylib   = LLVMOrcLLJITGetMainJITDylib(state->jit);
   state->context = LLVMOrcCreateNewThreadSafeContext();

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
   };
   cgen_module(&req);
   cgen_optimise(&req);

   LLVMOrcThreadSafeModuleRef tsm =
      LLVMOrcCreateNewThreadSafeModule(req.module, state->context);
   LLVMOrcLLJITAddLLVMIRModule(state->jit, state->dylib, tsm);

   LLVMOrcJITTargetAddress addr;
   LLVMErrorRef error = LLVMOrcLLJITLookup(state->jit, &addr, req.name);
   if (error != LLVMErrorSuccess) {
      char *msg = LLVMGetErrorMessage(error);
      fatal("LLVMOrcLLJITLookup failed: %s", msg);
   }

   printf("%s at %p\n", req.name, (void *)addr);

   f->entry = (jit_entry_fn_t)addr;

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
