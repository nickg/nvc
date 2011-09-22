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
#include <llvm-c/ExecutionEngine.h>
#include <llvm-c/Analysis.h>
#include <llvm-c/Transforms/Scalar.h>
#include <llvm-c/Transforms/IPO.h>

#undef NDEBUG
#include <assert.h>

static LLVMModuleRef  module = NULL;
static LLVMBuilderRef builder = NULL;
static bool           run_optimiser = true;

// Linked list of entry points to a process
// These correspond to wait statements
struct proc_entry {
   int               state_num;
   tree_t            wait;
   LLVMBasicBlockRef bb;
   struct proc_entry *next;
};

// Code generation context for a process
struct proc_ctx {
   struct proc_entry *entry_list;
   LLVMValueRef      state;
   tree_t            proc;
};

static LLVMValueRef cgen_expr(tree_t t, struct proc_ctx *ctx);

static int64_t literal_int(tree_t t)
{
   assert(tree_kind(t) == T_LITERAL);
   return tree_literal(t).i;
}

static size_t vhdl_array_len(type_t t)
{
   range_t r = type_dim(t, 0);
   return literal_int(r.right) - literal_int(r.left) + 1;
}

static LLVMValueRef llvm_int32(int32_t i)
{
   return LLVMConstInt(LLVMInt32Type(), i, false);
}

static LLVMTypeRef llvm_type(type_t t)
{
   switch (type_kind(t)) {
   case T_INTEGER:
   case T_PHYSICAL:
      {
         range_t r = type_dim(t, 0);
         uint64_t elements = literal_int(r.right) - literal_int(r.left);

         if (elements <= 2)
            return LLVMInt1Type();
         if (elements <= 0xffull)
            return LLVMInt8Type();
         else if (elements <= 0xffffull)
            return LLVMInt16Type();
         else if (elements <= 0xffffffffull)
            return LLVMInt32Type();
         else
            return LLVMInt64Type();
      }

   case T_SUBTYPE:
      return llvm_type(type_base(t));

   case T_CARRAY:
      {
         assert(type_dims(t) == 1);
         return LLVMArrayType(llvm_type(type_base(t)),
                              vhdl_array_len(t));
      }

   case T_ENUM:
      {
         unsigned lits = type_enum_literals(t);

         if (lits <= 2)
            return LLVMInt1Type();
         if (lits <= 256)
            return LLVMInt8Type();
         else if (lits <= 65356)
            return LLVMInt16Type();
         else
            return LLVMInt32Type();
      }

   default:
      abort();
   }
}

static unsigned proc_nvars(tree_t p)
{
   assert(tree_kind(p) == T_PROCESS);

   unsigned nvars = 0;
   for (unsigned i = 0; i < tree_decls(p); i++) {
      if (tree_kind(tree_decl(p, i)) == T_VAR_DECL)
         nvars++;
   }

   return nvars;
}

static LLVMValueRef cgen_proc_var(tree_t decl, struct proc_ctx *ctx)
{
   assert(tree_kind(decl) == T_VAR_DECL);

   unsigned v = 0;
   for (unsigned i = 0; i < tree_decls(ctx->proc); i++) {
      tree_t di = tree_decl(ctx->proc, i);
      if (di == decl)
         return LLVMBuildStructGEP(builder, ctx->state, v + 1, "");
      else if (tree_kind(di) == T_VAR_DECL)
         ++v;
   }

   assert(false);
}

static LLVMValueRef cgen_default_value(type_t ty)
{
   return LLVMConstInt(llvm_type(ty), 0, false);
}

static LLVMValueRef cgen_fdecl(tree_t t)
{
   LLVMValueRef fn = LLVMGetNamedFunction(module, istr(tree_ident(t)));
   if (fn != NULL)
      return fn;
   else {
      type_t ftype = tree_type(t);

      LLVMTypeRef atypes[type_params(ftype)];
      for (unsigned i = 0; i < type_params(ftype); i++)
         atypes[i] = llvm_type(type_param(ftype, i));

      return LLVMAddFunction(
         module,
         istr(tree_ident(t)),
         LLVMFunctionType(llvm_type(type_result(ftype)),
                          atypes,
                          type_params(ftype),
                          false));
   }
}

static LLVMValueRef cgen_literal(tree_t t)
{
   literal_t l = tree_literal(t);
   switch (l.kind) {
   case L_INT:
      return LLVMConstInt(llvm_type(tree_type(t)), l.i, false);
   default:
      abort();
   }
}

static LLVMValueRef cgen_fcall(tree_t t, struct proc_ctx *ctx)
{
   tree_t decl = tree_ref(t);
   assert(tree_kind(decl) == T_FUNC_DECL);

   LLVMValueRef args[tree_params(t)];
   for (unsigned i = 0; i < tree_params(t); i++)
      args[i] = cgen_expr(tree_param(t, i), ctx);

   const char *builtin = tree_attr_str(decl, ident_new("builtin"));
   if (builtin) {
      if (strcmp(builtin, "mul") == 0)
         return LLVMBuildMul(builder, args[0], args[1], "");
      else if (strcmp(builtin, "add") == 0)
         return LLVMBuildAdd(builder, args[0], args[1], "");
      else if (strcmp(builtin, "eq") == 0)
         return LLVMBuildICmp(builder, LLVMIntEQ, args[0], args[1], "");
      else
         fatal("cannot generate code for builtin %s", builtin);
   }
   else
      fatal("non-builtin functions not yet supported");
}

static LLVMValueRef cgen_ref(tree_t t, struct proc_ctx *ctx)
{
   tree_t decl = tree_ref(t);

   switch (tree_kind(decl)) {
   case T_CONST_DECL:
      return cgen_expr(tree_value(decl), ctx);

   case T_FUNC_DECL:
      return LLVMBuildCall(builder, cgen_fdecl(decl), NULL, 0, "");

   case T_ENUM_LIT:
      return LLVMConstInt(llvm_type(tree_type(t)), tree_pos(decl), false);

   case T_VAR_DECL:
      return LLVMBuildLoad(builder, cgen_proc_var(decl, ctx), "");

   default:
      abort();
   }

   return NULL;
}

static LLVMValueRef cgen_aggregate(tree_t t, struct proc_ctx *ctx)
{
   LLVMValueRef vals[tree_assocs(t)];

   for (unsigned i = 0; i < tree_assocs(t); i++) {
      assoc_t a = tree_assoc(t, i);

      switch (a.kind) {
      case A_POS:
         vals[i] = cgen_expr(a.value, ctx);
         break;

      default:
         fatal("only positional associations supported");
      }
   }

   LLVMTypeRef arr_ty = llvm_type(tree_type(t));
   LLVMTypeRef elm_ty = llvm_type(type_base(tree_type(t)));

   LLVMValueRef g = LLVMAddGlobal(module, arr_ty, "");
   LLVMSetGlobalConstant(g, true);
   LLVMSetLinkage(g, LLVMInternalLinkage);
   LLVMSetInitializer(g, LLVMConstArray(elm_ty, vals, tree_assocs(t)));

   return g;
}

static LLVMValueRef cgen_expr(tree_t t, struct proc_ctx *ctx)
{
   switch (tree_kind(t)) {
   case T_LITERAL:
      return cgen_literal(t);
   case T_FCALL:
      return cgen_fcall(t, ctx);
   case T_REF:
      return cgen_ref(t, ctx);
   case T_AGGREGATE:
      return cgen_aggregate(t, ctx);
   default:
      fatal("missing cgen_expr for kind %d\n", tree_kind(t));
   }
}

static void cgen_wait(tree_t t, struct proc_ctx *ctx)
{
   if (tree_has_delay(t)) {
      LLVMValueRef sched_process_fn =
         LLVMGetNamedFunction(module, "_sched_process");
      assert(sched_process_fn != NULL);

      LLVMValueRef args[] = { cgen_expr(tree_delay(t), ctx) };
      LLVMBuildCall(builder, sched_process_fn, args, 1, "");
   }

   // Find the basic block to jump to when the process is next scheduled
   struct proc_entry *it;
   for (it = ctx->entry_list; it && it->wait != t; it = it->next)
      ;
   assert(it != NULL);

   LLVMValueRef state_ptr = LLVMBuildStructGEP(builder, ctx->state, 0, "");
   LLVMBuildStore(builder, llvm_int32(it->state_num), state_ptr);
   LLVMBuildRetVoid(builder);

   LLVMPositionBuilderAtEnd(builder, it->bb);
}

static void cgen_var_assign(tree_t t, struct proc_ctx *ctx)
{
   LLVMValueRef rhs = cgen_expr(tree_value(t), ctx);

   tree_t target = tree_target(t);
   switch (tree_kind(target)) {
   case T_REF:
      {
         LLVMValueRef lhs = cgen_proc_var(tree_ref(target), ctx);
         LLVMBuildStore(builder, rhs, lhs);
      }
      break;

   default:
      assert(false);
   }
}

static LLVMValueRef cgen_array_to_c_string(LLVMValueRef a)
{
   LLVMValueRef indices[] = {
      llvm_int32(0), llvm_int32(0)
   };

   return LLVMBuildGEP(builder, a, indices, ARRAY_LEN(indices), "");
}

static void cgen_assert(tree_t t, struct proc_ctx *ctx)
{
   LLVMValueRef test     = cgen_expr(tree_value(t), ctx);
   LLVMValueRef message  = cgen_expr(tree_message(t), ctx);
   LLVMValueRef severity = cgen_expr(tree_severity(t), ctx);

   LLVMValueRef failed = LLVMBuildNot(builder, test, "");

   LLVMValueRef fn = LLVMGetBasicBlockParent(LLVMGetInsertBlock(builder));
   LLVMBasicBlockRef thenbb = LLVMAppendBasicBlock(fn, "assert_fail");
   LLVMBasicBlockRef elsebb = LLVMAppendBasicBlock(fn, "assert_pass");

   LLVMBuildCondBr(builder, failed, thenbb, elsebb);

   LLVMPositionBuilderAtEnd(builder, thenbb);
   LLVMValueRef assert_fail_fn =
      LLVMGetNamedFunction(module, "_assert_fail");
   assert(assert_fail_fn != NULL);

   LLVMValueRef args[] = {
      LLVMConstInt(LLVMInt8Type(),
                   tree_attr_int(t, ident_new("is_report"), 0),
                   false),
      cgen_array_to_c_string(message),
      LLVMConstInt(LLVMInt32Type(),
                   vhdl_array_len(tree_type(tree_message(t))),
                   false),
      severity
   };
   LLVMBuildCall(builder, assert_fail_fn, args, ARRAY_LEN(args), "");

   LLVMBuildBr(builder, elsebb);

   LLVMPositionBuilderAtEnd(builder, elsebb);
}

static void cgen_stmt(tree_t t, struct proc_ctx *ctx)
{
   switch (tree_kind(t)) {
   case T_WAIT:
      cgen_wait(t, ctx);
      break;
   case T_VAR_ASSIGN:
      cgen_var_assign(t, ctx);
      break;
   case T_ASSERT:
      cgen_assert(t, ctx);
      break;
   default:
      assert(false);
   }
}

static void cgen_jump_table_fn(tree_t t, void *arg)
{
   if (tree_kind(t) == T_WAIT) {
      struct proc_ctx *ctx = arg;

      struct proc_entry *p = xmalloc(sizeof(struct proc_entry));
      p->next = NULL;
      p->wait = t;
      p->bb   = NULL;

      if (ctx->entry_list == NULL) {
         p->state_num = 1;
         ctx->entry_list = p;
      }
      else {
         struct proc_entry *it;
         for (it = ctx->entry_list; it->next != NULL; it = it->next)
            ;
         p->state_num = it->state_num + 1;
         it->next = p;
      }
   }
}

LLVMTypeRef cgen_process_state_type(tree_t t)
{
   LLVMTypeRef fields[proc_nvars(t) + 1];
   fields[0] = LLVMInt32Type();   // State

   unsigned v = 1;
   for (unsigned i = 0; i < tree_decls(t); i++) {
      tree_t decl = tree_decl(t, i);
      if (tree_kind(decl) == T_VAR_DECL)
         fields[v++] = llvm_type(tree_type(decl));
   }

   LLVMTypeRef ty = LLVMStructType(fields, ARRAY_LEN(fields), false);

   char name[64];
   snprintf(name, sizeof(name), "%s_state_s", istr(tree_ident(t)));
   if (LLVMAddTypeName(module, name, ty))
      fatal("failed to add type name %s", name);

   return ty;
}

static void cgen_process(tree_t t)
{
   assert(tree_kind(t) == T_PROCESS);

   struct proc_ctx ctx = {
      .entry_list = NULL,
      .proc       = t
   };

   // Create a global structure to hold process state
   char state_name[64];
   snprintf(state_name, sizeof(state_name), "%s_state", istr(tree_ident(t)));
   ctx.state = LLVMAddGlobal(module,
                             cgen_process_state_type(t),
                             state_name);
   LLVMSetLinkage(ctx.state, LLVMInternalLinkage);

   // TODO: init all to undef
   const unsigned nvars = proc_nvars(t);
   LLVMValueRef state_init[nvars + 1];
   state_init[0] = llvm_int32(0);     // State
   for (unsigned i = 0; i < nvars; i++)
      state_init[i + 1] = LLVMGetUndef(LLVMInt32Type());
   LLVMSetInitializer(ctx.state,
                      LLVMConstStruct(state_init,
                                      ARRAY_LEN(state_init),
                                      false));

   LLVMTypeRef ftype = LLVMFunctionType(LLVMVoidType(), NULL, 0, false);
   LLVMValueRef fn = LLVMAddFunction(module, istr(tree_ident(t)), ftype);

   LLVMBasicBlockRef jtbb = LLVMAppendBasicBlock(fn, "jump_table");
   LLVMPositionBuilderAtEnd(builder, jtbb);

   // Generate the jump table at the start of a process to handle
   // resuming from a wait statement

   tree_visit(t, cgen_jump_table_fn, &ctx);

   if (ctx.entry_list == NULL) {
      const loc_t *loc = tree_loc(t);
      fprintf(stderr, "%s:%d: no wait statement in process\n",
              loc->file, loc->first_line);
   }

   LLVMValueRef state_ptr = LLVMBuildStructGEP(builder, ctx.state, 0, "");
   LLVMValueRef jtarget = LLVMBuildLoad(builder, state_ptr, "");

   LLVMBasicBlockRef init_bb = LLVMAppendBasicBlock(fn, "init");
   LLVMValueRef jswitch = LLVMBuildSwitch(builder, jtarget, init_bb, 10);

   struct proc_entry *it;
   for (it = ctx.entry_list; it != NULL; it = it->next) {
      it->bb = LLVMAppendBasicBlock(fn, istr(tree_ident(it->wait)));

      LLVMAddCase(jswitch, llvm_int32(it->state_num), it->bb);
   }

   LLVMPositionBuilderAtEnd(builder, init_bb);

   // Variable initialisation

   for (unsigned i = 0; i < tree_decls(t); i++) {
      tree_t v = tree_decl(t, i);
      if (tree_kind(v) == T_VAR_DECL) {
         LLVMValueRef val =
            tree_has_value(v)
            ? cgen_expr(tree_value(v), &ctx)
            : cgen_default_value(tree_type(v));
         LLVMBuildStore(builder, val, cgen_proc_var(v, &ctx));
      }
   }

   LLVMBasicBlockRef start_bb = LLVMAppendBasicBlock(fn, "start");
   LLVMBuildBr(builder, start_bb);
   LLVMPositionBuilderAtEnd(builder, start_bb);

   for (unsigned i = 0; i < tree_stmts(t); i++)
      cgen_stmt(tree_stmt(t, i), &ctx);

   LLVMBuildBr(builder, start_bb);
}

static void cgen_top(tree_t t)
{
   assert(tree_kind(t) == T_ELAB);

   for (unsigned i = 0; i < tree_stmts(t); i++)
      cgen_process(tree_stmt(t, i));
}

static void optimise(void)
{
   LLVMPassManagerRef pass_mgr = LLVMCreatePassManager();

   LLVMAddPromoteMemoryToRegisterPass(pass_mgr);
   LLVMAddInstructionCombiningPass(pass_mgr);
   LLVMAddReassociatePass(pass_mgr);
   LLVMAddGVNPass(pass_mgr);
   LLVMAddCFGSimplificationPass(pass_mgr);

   if (LLVMRunPassManager(pass_mgr, module))
      fatal("LLVM pass manager failed");

   LLVMDisposePassManager(pass_mgr);
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

   LLVMTypeRef _assert_fail_args[] = {
      LLVMInt8Type(),
      LLVMPointerType(LLVMInt8Type(), 0),
      LLVMInt32Type(),
      LLVMInt8Type()
   };
   LLVMAddFunction(module, "_assert_fail",
                   LLVMFunctionType(LLVMVoidType(),
                                    _assert_fail_args,
                                    ARRAY_LEN(_assert_fail_args),
                                    false));

   cgen_top(top);

   LLVMDumpModule(module);
   if (LLVMVerifyModule(module, LLVMPrintMessageAction, NULL))
      fatal("LLVM verification failed");

   if (run_optimiser)
      optimise();

   char fname[256];
   snprintf(fname, sizeof(fname), "_%s.bc", istr(tree_ident(top)));

   FILE *f = lib_fopen(lib_work(), fname, "w");
#if LLVM_VERSION > 26
   if (LLVMWriteBitcodeToFD(module, fileno(f), 0, 0) != 0)
      fatal("error writing LLVM bitcode");
#else
   if (LLVMWriteBitcodeToFileHandle(module, fileno(f)) != 0)
      fatal("error writing LLVM bitcode");
#endif
   fclose(f);

   LLVMDisposeBuilder(builder);
   LLVMDisposeModule(module);
}

void cgen_optimise_en(bool en)
{
   run_optimiser = en;
}
