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
#include "rt/signal.h"

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

static LLVMValueRef llvm_int64(int64_t i)
{
   return LLVMConstInt(LLVMInt64Type(), i, false);
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

static LLVMValueRef cgen_signal_flag(tree_t signal, int flag)
{
   tree_t sig_decl = tree_ref(signal);

   LLVMValueRef signal_struct =
      LLVMGetNamedGlobal(module, istr(tree_ident(sig_decl)));
   assert(signal_struct != NULL);

   LLVMValueRef bit = llvm_int32(flag);

   LLVMValueRef ptr =
      LLVMBuildStructGEP(builder, signal_struct, SIGNAL_FLAGS, "");
   LLVMValueRef deref = LLVMBuildLoad(builder, ptr, "");
   LLVMValueRef masked = LLVMBuildAnd(builder, deref, bit, "");
   return LLVMBuildICmp(builder, LLVMIntEQ, masked, bit, "");
}

static LLVMValueRef cgen_fcall(tree_t t, struct proc_ctx *ctx)
{
   tree_t decl = tree_ref(t);
   assert(tree_kind(decl) == T_FUNC_DECL);

   const char *builtin = tree_attr_str(decl, ident_new("builtin"));

   // Special attributes
   if (builtin) {
      if (strcmp(builtin, "event") == 0)
         return cgen_signal_flag(tree_param(t, 0).value, SIGNAL_F_EVENT);
      else if (strcmp(builtin, "active") == 0)
         return cgen_signal_flag(tree_param(t, 0).value, SIGNAL_F_ACTIVE);
   }

   LLVMValueRef args[tree_params(t)];
   for (unsigned i = 0; i < tree_params(t); i++)
      args[i] = cgen_expr(tree_param(t, i).value, ctx);

   // Regular builtin functions
   if (builtin) {
      if (strcmp(builtin, "mul") == 0)
         return LLVMBuildMul(builder, args[0], args[1], "");
      else if (strcmp(builtin, "add") == 0)
         return LLVMBuildAdd(builder, args[0], args[1], "");
      else if (strcmp(builtin, "sub") == 0)
         return LLVMBuildSub(builder, args[0], args[1], "");
      else if (strcmp(builtin, "div") == 0)
         return LLVMBuildSDiv(builder, args[0], args[1], "");
      else if (strcmp(builtin, "eq") == 0)
         return LLVMBuildICmp(builder, LLVMIntEQ, args[0], args[1], "");
      else if (strcmp(builtin, "neq") == 0)
         return LLVMBuildICmp(builder, LLVMIntNE, args[0], args[1], "");
      else if (strcmp(builtin, "lt") == 0)
         return LLVMBuildICmp(builder, LLVMIntSLT, args[0], args[1], "");
      else if (strcmp(builtin, "gt") == 0)
         return LLVMBuildICmp(builder, LLVMIntSGT, args[0], args[1], "");
      else if (strcmp(builtin, "leq") == 0)
         return LLVMBuildICmp(builder, LLVMIntSLE, args[0], args[1], "");
      else if (strcmp(builtin, "geq") == 0)
         return LLVMBuildICmp(builder, LLVMIntSGE, args[0], args[1], "");
      else if (strcmp(builtin, "neg") == 0)
         return LLVMBuildNeg(builder, args[0], "");
      else if (strcmp(builtin, "not") == 0)
         return LLVMBuildNot(builder, args[0], "");
      else
         fatal("cannot generate code for builtin %s", builtin);
   }
   else {
      return LLVMBuildCall(builder, cgen_fdecl(decl), NULL, 0, "");
   }
}

static LLVMValueRef cgen_ref(tree_t t, struct proc_ctx *ctx)
{
   tree_t decl = tree_ref(t);

   switch (tree_kind(decl)) {
   case T_CONST_DECL:
      return cgen_expr(tree_value(decl), ctx);

   case T_ENUM_LIT:
      return LLVMConstInt(llvm_type(tree_type(t)), tree_pos(decl), false);

   case T_VAR_DECL:
      return LLVMBuildLoad(builder, cgen_proc_var(decl, ctx), "");

   case T_SIGNAL_DECL:
      {
         LLVMValueRef signal =
            LLVMGetNamedGlobal(module, istr(tree_ident(decl)));
         assert(signal != NULL);

         LLVMValueRef ptr =
            LLVMBuildStructGEP(builder, signal, SIGNAL_RESOLVED, "");
         LLVMValueRef deref = LLVMBuildLoad(builder, ptr, "");
         return LLVMBuildIntCast(builder, deref,
                                 llvm_type(tree_type(t)), "");
      }
      break;

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

static void cgen_sched_process(LLVMValueRef after)
{
   LLVMValueRef sched_process_fn =
      LLVMGetNamedFunction(module, "_sched_process");
   assert(sched_process_fn != NULL);

   LLVMValueRef args[] = { after };
   LLVMBuildCall(builder, sched_process_fn, args, 1, "");
}

static void cgen_wait(tree_t t, struct proc_ctx *ctx)
{
   if (tree_has_delay(t))
      cgen_sched_process(cgen_expr(tree_delay(t), ctx));

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

static void cgen_sched_waveform(tree_t decl, LLVMValueRef value,
                                struct proc_ctx *ctx)
{
   LLVMValueRef signal =
      LLVMGetNamedGlobal(module, istr(tree_ident(decl)));
   assert(signal != NULL);

   LLVMValueRef sched_waveform_fn =
      LLVMGetNamedFunction(module, "_sched_waveform");
   assert(sched_waveform_fn != NULL);

   LLVMValueRef args[] = {
      LLVMBuildPointerCast(builder, signal,
                           LLVMPointerType(LLVMInt8Type(), 0), ""),
      llvm_int32(0 /* source, TODO */),
      LLVMBuildIntCast(builder, value, LLVMInt64Type(), ""),
      llvm_int64(0)
   };
   LLVMBuildCall(builder, sched_waveform_fn,
                 args, ARRAY_LEN(args), "");
}

static void cgen_signal_assign(tree_t t, struct proc_ctx *ctx)
{
   LLVMValueRef rhs = cgen_expr(tree_value(t), ctx);

   tree_t target = tree_target(t);
   switch (tree_kind(target)) {
   case T_REF:
      cgen_sched_waveform(tree_ref(target), rhs, ctx);
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
   case T_SIGNAL_ASSIGN:
      cgen_signal_assign(t, ctx);
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
   assert(tree_kind(t) == T_WAIT);

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

static void cgen_driver_init_fn(tree_t t, void *arg)
{
   assert(tree_kind(t) == T_SIGNAL_ASSIGN);

   struct proc_ctx *ctx = arg;

   tree_t target = tree_target(t);
   assert(tree_kind(target) == T_REF);

   tree_t decl = tree_ref(target);

   ident_t tag_i = ident_new("driver_tag");
   if (tree_attr_ptr(decl, tag_i) == ctx->proc)
      return;   // Already initialised this signal

   LLVMValueRef val =
      tree_has_value(decl)
      ? cgen_expr(tree_value(decl), ctx)
      : cgen_default_value(tree_type(decl));
   cgen_sched_waveform(decl, val, ctx);

   tree_add_attr_ptr(decl, tag_i, ctx->proc);
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
   snprintf(name, sizeof(name), "%s__state_s", istr(tree_ident(t)));
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
   snprintf(state_name, sizeof(state_name),
            "%s__state", istr(tree_ident(t)));
   LLVMTypeRef state_ty = cgen_process_state_type(t);
   ctx.state = LLVMAddGlobal(module, state_ty, state_name);
   LLVMSetLinkage(ctx.state, LLVMInternalLinkage);

   // Process state is initially undefined: call process function
   // with non-zero argument to initialise
   LLVMSetInitializer(ctx.state, LLVMGetUndef(state_ty));

   LLVMTypeRef pargs[] = { LLVMInt32Type() };
   LLVMTypeRef ftype = LLVMFunctionType(LLVMVoidType(), pargs, 1, false);
   LLVMValueRef fn = LLVMAddFunction(module, istr(tree_ident(t)), ftype);

   LLVMBasicBlockRef entry_bb = LLVMAppendBasicBlock(fn, "entry");
   LLVMBasicBlockRef jt_bb    = LLVMAppendBasicBlock(fn, "jump_table");
   LLVMBasicBlockRef init_bb  = LLVMAppendBasicBlock(fn, "init");
   LLVMBasicBlockRef start_bb = LLVMAppendBasicBlock(fn, "start");

   LLVMPositionBuilderAtEnd(builder, entry_bb);

   // If the parameter is non-zero jump to the init block

   LLVMValueRef param = LLVMGetParam(fn, 0);
   LLVMValueRef reset =
      LLVMBuildICmp(builder, LLVMIntNE, param, llvm_int32(0), "");
   LLVMBuildCondBr(builder, reset, init_bb, jt_bb);


   // Generate the jump table at the start of a process to handle
   // resuming from a wait statement

   LLVMPositionBuilderAtEnd(builder, jt_bb);

   tree_visit_only(t, cgen_jump_table_fn, &ctx, T_WAIT);

   if (ctx.entry_list == NULL) {
      const loc_t *loc = tree_loc(t);
      fprintf(stderr, "%s:%d: no wait statement in process\n",
              loc->file, loc->first_line);
   }

   LLVMValueRef state_ptr = LLVMBuildStructGEP(builder, ctx.state, 0, "");
   LLVMValueRef jtarget = LLVMBuildLoad(builder, state_ptr, "");

   // TODO: if none of the cases match should jump to unreachable
   // block rather than init
   LLVMValueRef jswitch = LLVMBuildSwitch(builder, jtarget, init_bb, 10);

   LLVMAddCase(jswitch, llvm_int32(0), start_bb);

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

   // Signal driver initialisation

   tree_visit_only(t, cgen_driver_init_fn, &ctx, T_SIGNAL_ASSIGN);

   // Return to simulation kernel after initialisation

   cgen_sched_process(llvm_int64(0));
   LLVMBuildStore(builder, llvm_int32(0 /* start */), state_ptr);
   LLVMBuildRetVoid(builder);

   // Sequential statements

   LLVMPositionBuilderAtEnd(builder, start_bb);

   for (unsigned i = 0; i < tree_stmts(t); i++)
      cgen_stmt(tree_stmt(t, i), &ctx);

   LLVMBuildBr(builder, start_bb);
}

static void cgen_signal(tree_t t)
{
   assert(tree_kind(t) == T_SIGNAL_DECL);

   char name[64];
   snprintf(name, sizeof(name), "signal_%d_source_s", tree_drivers(t));

   LLVMTypeRef void_ptr = LLVMPointerType(LLVMInt8Type(), 0);
   LLVMTypeRef ty = LLVMGetTypeByName(module, name);
   if (ty == NULL) {
      LLVMTypeRef fields[tree_drivers(t) + SIGNAL_N_FIELDS];
      fields[SIGNAL_RESOLVED] = LLVMInt64Type();
      fields[SIGNAL_DECL]     = void_ptr;
      fields[SIGNAL_FLAGS]    = LLVMInt32Type();

      for (unsigned i = 0; i < tree_drivers(t); i++)
         fields[i + SIGNAL_N_FIELDS] = void_ptr;  // Driver waveform queue

      ty = LLVMStructType(fields, ARRAY_LEN(fields), false);

      if (LLVMAddTypeName(module, name, ty))
         fatal("failed to add type name %s", name);
   }

   LLVMValueRef v = LLVMAddGlobal(module, ty, istr(tree_ident(t)));

   LLVMValueRef init[tree_drivers(t) + SIGNAL_N_FIELDS];
   init[SIGNAL_RESOLVED] = llvm_int64(0);
   init[SIGNAL_DECL]     = LLVMConstNull(void_ptr);
   init[SIGNAL_FLAGS]    = llvm_int32(0);
   for (unsigned i = 0; i < tree_drivers(t); i++)
      init[i + SIGNAL_N_FIELDS] = LLVMConstNull(void_ptr);

   LLVMSetInitializer(v, LLVMConstStruct(init, ARRAY_LEN(init), false));
}

static void cgen_top(tree_t t)
{
   assert(tree_kind(t) == T_ELAB);

   for (unsigned i = 0; i < tree_decls(t); i++)
      cgen_signal(tree_decl(t, i));

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

static void cgen_support_fns(void)
{
   LLVMTypeRef _sched_process_args[] = { LLVMInt64Type() };
   LLVMAddFunction(module, "_sched_process",
                   LLVMFunctionType(LLVMVoidType(),
                                    _sched_process_args,
                                    ARRAY_LEN(_sched_process_args),
                                    false));

   LLVMTypeRef _sched_waveform_args[] = {
      LLVMPointerType(LLVMInt8Type(), 0),
      LLVMInt32Type(),
      LLVMInt64Type(),
      LLVMInt64Type()
   };
   LLVMAddFunction(module, "_sched_waveform",
                   LLVMFunctionType(LLVMVoidType(),
                                    _sched_waveform_args,
                                    ARRAY_LEN(_sched_waveform_args),
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
}

void cgen(tree_t top)
{
   if (tree_kind(top) != T_ELAB)
      fatal("cannot generate code for tree kind %d", tree_kind(top));

   module = LLVMModuleCreateWithName(istr(tree_ident(top)));
   builder = LLVMCreateBuilder();

   cgen_support_fns();

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
