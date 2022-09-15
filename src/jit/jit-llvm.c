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

#ifdef LLVM_HAS_LLJIT

#include <stdlib.h>

#include <llvm-c/Analysis.h>
#include <llvm-c/BitReader.h>
#include <llvm-c/Core.h>
#include <llvm-c/DebugInfo.h>
#include <llvm-c/Error.h>
#include <llvm-c/ExecutionEngine.h>
#include <llvm-c/LLJIT.h>

typedef struct {
   LLVMModuleRef        module;
   LLVMContextRef       context;
   LLVMBuilderRef       builder;
   LLVMTargetMachineRef target;
   LLVMValueRef         func;
   char                *name;
} cgen_req_t;

typedef struct {
   LLVMOrcThreadSafeContextRef context;
   LLVMOrcLLJITRef             jit;
   LLVMOrcExecutionSessionRef  session;
   LLVMOrcJITDylibRef          dylib;
} lljit_state_t;

static LLVMBasicBlockRef cgen_append_block(cgen_req_t *req, const char *name)
{
   return LLVMAppendBasicBlockInContext(req->context, req->func, name);
}

static void cgen_module(cgen_req_t *req, jit_func_t *f)
{
   req->module  = LLVMModuleCreateWithNameInContext(req->name, req->context);
   req->builder = LLVMCreateBuilderInContext(req->context);

   char *triple = LLVMGetTargetMachineTriple(req->target);
   LLVMSetTarget(req->module, triple);
   LLVMDisposeMessage(triple);

   LLVMTargetDataRef data_ref = LLVMCreateTargetDataLayout(req->target);
   LLVMSetModuleDataLayout(req->module, data_ref);

   LLVMTypeRef atypes[] = {};
   LLVMTypeRef fntype = LLVMFunctionType(LLVMVoidTypeInContext(req->context),
                                         atypes, ARRAY_LEN(atypes), false);

   req->func = LLVMAddFunction(req->module, req->name, fntype);

   LLVMBasicBlockRef entry_bb = cgen_append_block(req, "entry");
   LLVMPositionBuilderAtEnd(req->builder, entry_bb);
   LLVMBuildRetVoid(req->builder);

   LLVMDisposeBuilder(req->builder);
   req->builder = NULL;

   LLVMDisposeTargetData(data_ref);

   LLVMDumpModule(req->module);

#ifndef NDEBUG
   if (LLVMVerifyModule(req->module, LLVMPrintMessageAction, NULL))
      fatal("LLVM verification failed");
#endif
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
      .name    = tb_claim(tb)
   };
   cgen_module(&req, f);

   LLVMOrcThreadSafeModuleRef tsm =
      LLVMOrcCreateNewThreadSafeModule(req.module, state->context);
   LLVMOrcLLJITAddLLVMIRModule(state->jit, state->dylib, tsm);

   LLVMOrcJITTargetAddress TestFnAddr;
   LLVMErrorRef error = LLVMOrcLLJITLookup(state->jit, &TestFnAddr, req.name);
   if (error != LLVMErrorSuccess) {
      char *msg = LLVMGetErrorMessage(error);
      fatal("LLVMOrcLLJITLookup failed: %s", msg);
   }

   printf("%s at %p\n", req.name, (void *)TestFnAddr);

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
