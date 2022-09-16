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

   LLVM_LAST_TYPE
} llvm_type_t;

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
   jit_func_t          *func;
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

static void cgen_op_add(cgen_req_t *req, jit_ir_t *ir)
{
   LLVMValueRef arg1 = cgen_get_value(req, ir->arg1);
   LLVMValueRef arg2 = cgen_get_value(req, ir->arg2);

   LLVMValueRef result;
   if (ir->cc == JIT_CC_O) {
      // TODO
      result = LLVMBuildAdd(req->builder, arg1, arg2, "");
   }
   else
      result = LLVMBuildAdd(req->builder, arg1, arg2, "");

   cgen_store_reg(req, ir->result, result);
}

static void cgen_ir(cgen_req_t *req, jit_ir_t *ir)
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
   case J_ADD:
      cgen_op_add(req, ir);
      break;
   default:
      warnf("cannot generate LLVM for %s", jit_op_name(ir->op));
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

   for (int i = 0; i < req->func->nirs; i++)
      cgen_ir(req, &(req->func->irbuf[i]));

   LLVMBuildRet(req->builder, llvm_int1(req, true));

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
