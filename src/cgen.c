//
//  Copyright (C) 2011  Nick Gasson
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

#include "phase.h"
#include "util.h"
#include "lib.h"

#include <stdlib.h>
#include <string.h>

#include <llvm-c/Core.h>
#include <llvm-c/BitWriter.h>

#undef NDEBUG
#include <assert.h>

static LLVMModuleRef  module = NULL;
static LLVMBuilderRef builder = NULL;

static LLVMValueRef cgen_expr(tree_t t);

static LLVMValueRef cgen_literal(tree_t t)
{
   literal_t l = tree_literal(t);
   printf("cgen_literal %d\n", l.i);
   switch (l.kind) {
   case L_INT:
      return LLVMConstInt(LLVMInt64Type(), l.i, false);
   default:
      abort();
   }
}

static LLVMValueRef cgen_fcall(tree_t t)
{
   printf("cgen_fcall: %s\n", istr(tree_ident(t)));

   tree_t decl = tree_ref(t);
   assert(tree_kind(decl) == T_FUNC_DECL);

   LLVMValueRef args[tree_params(t)];
   for (unsigned i = 0; i < tree_params(t); i++)
      args[i] = cgen_expr(tree_param(t, i));

   const char *builtin = tree_attr_str(decl, ident_new("builtin"));
   if (builtin) {
      if (strcmp(builtin, "mul") == 0)
         return LLVMBuildMul(builder, args[0], args[1], "");
      else
         fatal("cannot generate code for builtin %s", builtin);
   }
   else
      fatal("non-builtin functions not yet supported");
}

static LLVMValueRef cgen_ref(tree_t t)
{
   printf("cgen_ref: %s\n", istr(tree_ident(t)));
   tree_t decl = tree_ref(t);

   switch (tree_kind(decl)) {
   case T_CONST_DECL:
      return cgen_expr(tree_value(decl));

   default:
      abort();
   }

   return NULL;
}

static LLVMValueRef cgen_expr(tree_t t)
{
   switch (tree_kind(t)) {
   case T_LITERAL:
      return cgen_literal(t);
   case T_FCALL:
      return cgen_fcall(t);
   case T_REF:
      return cgen_ref(t);
   default:
      abort();
   }
}

static void cgen_wait(tree_t t)
{
   // TODO: need to know if we're a function or co-routine here

   if (tree_has_delay(t)) {
      LLVMValueRef sched_process_fn =
         LLVMGetNamedFunction(module, "_sched_process");
      assert(sched_process_fn != NULL);

      LLVMValueRef args[] = { cgen_expr(tree_delay(t)) };
      LLVMBuildCall(builder, sched_process_fn, args, 1, "");
   }
   LLVMBuildRetVoid(builder);
}

static void cgen_stmt(tree_t t)
{
   switch (tree_kind(t)) {
   case T_WAIT:
      cgen_wait(t);
      break;
   default:
      assert(false);
   }
}

static void cgen_process(tree_t t)
{
   assert(tree_kind(t) == T_PROCESS);

   // TODO: if simple then func else co-routine

   printf("cgen_process %s\n", istr(tree_ident(t)));

   LLVMTypeRef ftype = LLVMFunctionType(LLVMVoidType(), NULL, 0, false);

   LLVMValueRef fn = LLVMAddFunction(module, istr(tree_ident(t)), ftype);
   LLVMBasicBlockRef bb = LLVMAppendBasicBlock(fn, "entry");

   LLVMPositionBuilderAtEnd(builder, bb);

   for (unsigned i = 0; i < tree_stmts(t); i++)
      cgen_stmt(tree_stmt(t, i));
}

static void cgen_top(tree_t t)
{
   assert(tree_kind(t) == T_ELAB);

   for (unsigned i = 0; i < tree_stmts(t); i++)
      cgen_process(tree_stmt(t, i));
}

void cgen(tree_t top)
{
   if (tree_kind(top) != T_ELAB)
      fatal("cannot generate code for tree kind %d", tree_kind(top));

   module = LLVMModuleCreateWithName(istr(tree_ident(top)));
   builder = LLVMCreateBuilder();

   LLVMTypeRef _sched_process_args[] = { LLVMInt64Type() };
   LLVMAddFunction(module, "_sched_process",
                   LLVMFunctionType(LLVMVoidType(),
                                    _sched_process_args,
                                    ARRAY_LEN(_sched_process_args),
                                    false));

   cgen_top(top);

   char fname[256];
   snprintf(fname, sizeof(fname), "_%s.bc", istr(tree_ident(top)));

   FILE *f = lib_fopen(lib_work(), fname, "w");
   if (LLVMWriteBitcodeToFD(module, fileno(f), 0, 0) != 0)
      fatal("error writing LLVM bitcode");
   fclose(f);

   LLVMDumpModule(module);

   LLVMDisposeBuilder(builder);
   LLVMDisposeModule(module);
}
