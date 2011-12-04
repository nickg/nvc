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
static bool           dump_module = false;

// Linked list of entry points to a process
// These correspond to wait statements
struct proc_entry {
   int               state_num;
   tree_t            wait;
   LLVMBasicBlockRef bb;
   struct proc_entry *next;
};

// Code generation context for a process or function
struct cgen_ctx {
   struct proc_entry *entry_list;
   LLVMValueRef      state;
   LLVMValueRef      fn;
   tree_t            proc;
   tree_t            fdecl;
};

static LLVMValueRef cgen_expr(tree_t t, struct cgen_ctx *ctx);
static void cgen_stmt(tree_t t, struct cgen_ctx *ctx);

static LLVMValueRef llvm_int32(int32_t i)
{
   return LLVMConstInt(LLVMInt32Type(), i, false);
}

static LLVMValueRef llvm_int64(int64_t i)
{
   return LLVMConstInt(LLVMInt64Type(), i, false);
}

static LLVMTypeRef llvm_void_ptr(void)
{
   return LLVMPointerType(LLVMInt8Type(), 0);
}

static LLVMValueRef llvm_void_cast(LLVMValueRef ptr)
{
   return LLVMBuildPointerCast(builder, ptr,
                               LLVMPointerType(LLVMInt8Type(), 0), "");
}

static LLVMValueRef llvm_sizeof(LLVMTypeRef type)
{
   return LLVMBuildIntCast(builder, LLVMSizeOf(type),
                           LLVMInt32Type(), "");
}

static LLVMValueRef llvm_fn(const char *name)
{
   LLVMValueRef fn = LLVMGetNamedFunction(module, name);
   assert(fn != NULL);
   return fn;
}

static LLVMValueRef llvm_global(const char *name)
{
   LLVMValueRef glob = LLVMGetNamedGlobal(module, name);
   assert(glob != NULL);
   return glob;
}

static int bit_width(type_t t)
{
   switch (type_kind(t)) {
   case T_INTEGER:
   case T_PHYSICAL:
      {
         range_t r = type_dim(t, 0);
         uint64_t elements = assume_int(r.right) - assume_int(r.left);

         if (elements <= 2)
            return 1;
         if (elements <= 0xffull)
            return 8;
         else if (elements <= 0xffffull)
            return 16;
         else if (elements <= 0xffffffffull)
            return 32;
         else
            return 64;
      }

   case T_SUBTYPE:
      return bit_width(type_base(t));

   case T_ENUM:
      {
         unsigned lits = type_enum_literals(t);

         if (lits <= 2)
            return 1;
         if (lits <= 256)
            return 8;
         else if (lits <= 65356)
            return 16;
         else
            return 32;
      }

   default:
      assert(false);
   }
}

static LLVMTypeRef llvm_type(type_t t)
{
   switch (type_kind(t)) {
   case T_INTEGER:
   case T_PHYSICAL:
   case T_ENUM:
      return LLVMIntType(bit_width(t));

   case T_SUBTYPE:
      return llvm_type(type_base(t));

   case T_CARRAY:
      {
         int64_t low, high;
         range_bounds(type_dim(t, 0), &low, &high);

         return LLVMArrayType(llvm_type(type_base(t)), high - low + 1);
      }

   default:
      assert(false);
   }
}

static LLVMValueRef cgen_range_low(range_t r, struct cgen_ctx *ctx)
{
   return cgen_expr(r.kind == RANGE_TO ? r.left : r.right, ctx);
}

static LLVMValueRef cgen_array_off(LLVMValueRef off, type_t type,
                                   struct cgen_ctx *ctx)
{
   // Convert VHDL offset 'off' to a zero-based LLVM array offset

   range_t r = type_dim(type, 0);
   return LLVMBuildSub(builder, off, cgen_range_low(r, ctx), "");
}

static LLVMValueRef cgen_array_signal_ptr(tree_t decl, LLVMValueRef elem)
{
   assert(tree_kind(decl) == T_SIGNAL_DECL);

   LLVMValueRef indexes[] = { llvm_int32(0), elem };
   return LLVMBuildGEP(builder, llvm_global(istr(tree_ident(decl))),
                       indexes, ARRAY_LEN(indexes), "");
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

static LLVMValueRef cgen_proc_var(tree_t decl, struct cgen_ctx *ctx)
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

static void cgen_array_copy(type_t ty, range_t r,
                            LLVMValueRef src, LLVMValueRef dst)
{
   assert(type_kind(ty) == T_CARRAY);

   int64_t type_low, type_high;
   range_bounds(type_dim(ty, 0), &type_low, &type_high);

   int64_t low, high;
   range_bounds(r, &low, &high);

   printf("low=%lu type_low=%lu\n", low, type_low);

   assert(low >= type_low && high <= type_high);

   LLVMValueRef args[] = {
      llvm_void_cast(dst),         // Destination
      llvm_void_cast(src),         // Source
      llvm_int32(low - type_low),  // Offset
      llvm_int32(high - low + 1),  // Number of elements
      llvm_sizeof(llvm_type(type_base(ty)))
   };
   LLVMBuildCall(builder, llvm_fn("_array_copy"),
                 args, ARRAY_LEN(args), "");
}

static LLVMValueRef cgen_default_value(type_t ty)
{
   if (type_kind(ty) == T_CARRAY) {
      int64_t low, high;
      range_bounds(type_dim(ty, 0), &low, &high);

      const size_t n_elems = high - low + 1;
      LLVMValueRef *vals = xmalloc(n_elems * sizeof(LLVMValueRef));

      LLVMValueRef def = cgen_default_value(type_base(ty));
      for (size_t i = 0; i < n_elems; i++)
         vals[i] = def;

      LLVMTypeRef arr_ty = llvm_type(ty);
      LLVMValueRef g = LLVMAddGlobal(module, arr_ty, "");
      LLVMSetGlobalConstant(g, true);
      LLVMSetLinkage(g, LLVMInternalLinkage);

      LLVMTypeRef elm_ty = llvm_type(type_base(ty));
      LLVMSetInitializer(g, LLVMConstArray(elm_ty, vals, n_elems));

      free(vals);
      return g;
   }
   else {
      // XXX: need to use ty'left
      return LLVMConstInt(llvm_type(ty), 0, false);
   }
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

static LLVMValueRef cgen_array_signal_ref(tree_t decl, type_t slice_type,
                                          struct cgen_ctx *ctx, bool last_value)
{
   type_t type = tree_type(decl);
   assert(type_kind(type) == T_CARRAY);

   // Copy the resolved signal into a temporary array
   LLVMValueRef tmp = LLVMBuildAlloca(builder, llvm_type(slice_type),
                                      istr(tree_ident(decl)));

   char name[256];
   snprintf(name, sizeof(name), "%s_vec_load",
            istr(type_ident(type_base(tree_type(decl)))));

   range_t r = type_dim(slice_type, 0);
   LLVMValueRef left_off =
      cgen_expr(r.kind == RANGE_TO ? r.left : r.right, ctx);
   LLVMValueRef right_off =
      cgen_expr(r.kind == RANGE_TO ? r.right : r.left, ctx);

   LLVMValueRef left_abs = cgen_array_off(left_off, type, ctx);
   LLVMValueRef right_abs = cgen_array_off(right_off, type, ctx);

   LLVMValueRef s_signal = llvm_global(istr(tree_ident(decl)));
   LLVMValueRef indexes[] = { llvm_int32(0), llvm_int32(0) };
   LLVMValueRef p_signal = LLVMBuildGEP(builder, s_signal,
                                        indexes, ARRAY_LEN(indexes), "");
   LLVMValueRef p_tmp = LLVMBuildGEP(builder, tmp,
                                     indexes, ARRAY_LEN(indexes), "");

   LLVMValueRef args[] = {
      p_signal, p_tmp, left_abs, right_abs,
      LLVMConstInt(LLVMInt1Type(), last_value, false)
   };
   LLVMBuildCall(builder, llvm_fn(name), args, ARRAY_LEN(args), "");
   return tmp;
}

static LLVMValueRef cgen_scalar_signal_flag(tree_t signal, int flag)
{
   LLVMValueRef signal_struct = llvm_global(istr(tree_ident(signal)));
   LLVMValueRef bit = llvm_int32(flag);
   LLVMValueRef ptr =
      LLVMBuildStructGEP(builder, signal_struct, SIGNAL_FLAGS, "");
   LLVMValueRef deref = LLVMBuildLoad(builder, ptr, "");
   LLVMValueRef masked = LLVMBuildAnd(builder, deref, bit, "");
   return LLVMBuildICmp(builder, LLVMIntEQ, masked, bit, "");
}

static LLVMValueRef cgen_array_signal_flag(tree_t signal, int flag)
{
   // Need to OR the flag for each sub-element

   LLVMValueRef bit = llvm_int32(flag);

   int64_t low, high;
   range_bounds(type_dim(tree_type(signal), 0), &low, &high);

   LLVMValueRef result = llvm_int32(0);
   for (int i = 0; i < high - low + 1; i++) {
      LLVMValueRef struct_ptr =
         cgen_array_signal_ptr(signal, llvm_int32(i));
      LLVMValueRef flags_ptr =
         LLVMBuildStructGEP(builder, struct_ptr, SIGNAL_FLAGS, "");
      LLVMValueRef deref = LLVMBuildLoad(builder, flags_ptr, "");
      LLVMValueRef masked = LLVMBuildAnd(builder, deref, bit, "");
      result = LLVMBuildOr(builder, result, masked, "");
   }

   return LLVMBuildICmp(builder, LLVMIntEQ, result, bit, "");
}

static LLVMValueRef cgen_signal_flag(tree_t ref, int flag)
{
   tree_t sig_decl = tree_ref(ref);
   if (type_kind(tree_type(sig_decl)) == T_CARRAY)
      return cgen_array_signal_flag(sig_decl, flag);
   else
      return cgen_scalar_signal_flag(sig_decl, flag);
}

static LLVMValueRef cgen_array_eq(type_t lhs_type, LLVMValueRef lhs,
                                  type_t rhs_type, LLVMValueRef rhs)
{
   assert(type_kind(lhs_type) == T_CARRAY);
   assert(type_kind(rhs_type) == T_CARRAY);

   int64_t low, high;
   range_bounds(type_dim(lhs_type, 0), &low, &high);

   bool opposite =
      (type_dim(lhs_type, 0).kind != type_dim(rhs_type, 0).kind);

   LLVMValueRef args[] = {
      llvm_void_cast(lhs),
      llvm_void_cast(rhs),
      llvm_int32(high - low + 1),  // Number of elements
      llvm_sizeof(llvm_type(type_base(lhs_type))),
      llvm_int32(opposite)
   };
   return LLVMBuildCall(builder, llvm_fn("_array_eq"),
                        args, ARRAY_LEN(args), "");
}

static LLVMValueRef cgen_last_value(tree_t signal, struct cgen_ctx *ctx)
{
   tree_t sig_decl = tree_ref(signal);
   assert(tree_kind(sig_decl) == T_SIGNAL_DECL);

   if (type_kind(tree_type(sig_decl)) == T_CARRAY) {
      type_t type = tree_type(sig_decl);
      return cgen_array_signal_ref(sig_decl, type, ctx, true);
   }
   else {
      LLVMValueRef signal = llvm_global(istr(tree_ident(sig_decl)));
      LLVMValueRef ptr =
         LLVMBuildStructGEP(builder, signal, SIGNAL_LAST_VALUE, "");
      LLVMValueRef deref = LLVMBuildLoad(builder, ptr, "");
      return LLVMBuildIntCast(builder, deref,
                              llvm_type(tree_type(sig_decl)), "");
   }
}

static LLVMValueRef cgen_fcall(tree_t t, struct cgen_ctx *ctx)
{
   tree_t decl = tree_ref(t);
   assert(tree_kind(decl) == T_FUNC_DECL
          || tree_kind(decl) == T_FUNC_BODY);

   const char *builtin = tree_attr_str(decl, ident_new("builtin"));

   // Special attributes
   if (builtin) {
      if (strcmp(builtin, "event") == 0)
         return cgen_signal_flag(tree_param(t, 0).value, SIGNAL_F_EVENT);
      else if (strcmp(builtin, "active") == 0)
         return cgen_signal_flag(tree_param(t, 0).value, SIGNAL_F_ACTIVE);
      else if (strcmp(builtin, "last_value") == 0)
         return cgen_last_value(tree_param(t, 0).value, ctx);
   }

   LLVMValueRef args[tree_params(t)];
   for (unsigned i = 0; i < tree_params(t); i++)
      args[i] = cgen_expr(tree_param(t, i).value, ctx);

   // Regular builtin functions
   if (builtin) {
      type_t arg_type = tree_type(tree_param(t, 0).value);

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
      else if (strcmp(builtin, "and") == 0)
         return LLVMBuildAnd(builder, args[0], args[1], "");
      else if (strcmp(builtin, "xor") == 0)
         return LLVMBuildXor(builder, args[0], args[1], "");
      else if (strcmp(builtin, "aeq") == 0) {
         type_t rhs_type = tree_type(tree_param(t, 1).value);
         return cgen_array_eq(arg_type, args[0], rhs_type, args[1]);
      }
      else if (strcmp(builtin, "aneq") == 0) {
         type_t rhs_type = tree_type(tree_param(t, 1).value);
         return LLVMBuildNot(
            builder,
            cgen_array_eq(arg_type, args[0], rhs_type, args[1]), "");
      }
      else if (strcmp(builtin, "image") == 0) {
         LLVMValueRef iargs[] = {
            LLVMBuildIntCast(builder, args[0], LLVMInt64Type(), "")
         };
         return LLVMBuildCall(builder, llvm_fn("_image"),
                              iargs, ARRAY_LEN(iargs), "");
      }
      else
         fatal("cannot generate code for builtin %s", builtin);
   }
   else {
      return LLVMBuildCall(builder, cgen_fdecl(decl),
                           args, tree_params(t), "");
   }
}

static LLVMValueRef cgen_scalar_signal_ref(tree_t decl, struct cgen_ctx *ctx)
{
   LLVMValueRef signal = llvm_global(istr(tree_ident(decl)));
   LLVMValueRef ptr =
      LLVMBuildStructGEP(builder, signal, SIGNAL_RESOLVED, "");
   LLVMValueRef deref = LLVMBuildLoad(builder, ptr, "");
   return LLVMBuildIntCast(builder, deref,
                           llvm_type(tree_type(decl)), "");
}

static LLVMValueRef cgen_ref(tree_t t, struct cgen_ctx *ctx)
{
   tree_t decl = tree_ref(t);

   switch (tree_kind(decl)) {
   case T_CONST_DECL:
      return cgen_expr(tree_value(decl), ctx);

   case T_ENUM_LIT:
      return LLVMConstInt(llvm_type(tree_type(t)), tree_pos(decl), false);

   case T_VAR_DECL:
      {
         LLVMValueRef ptr = cgen_proc_var(decl, ctx);
         if (type_kind(tree_type(decl)) == T_CARRAY)
            return ptr;
         else
            return LLVMBuildLoad(builder, ptr, "");
      }

   case T_SIGNAL_DECL:
      if (type_kind(tree_type(decl)) == T_CARRAY) {
         type_t type = tree_type(decl);
         return cgen_array_signal_ref(decl, type, ctx, false);
      }
      else
         return cgen_scalar_signal_ref(decl, ctx);

   case T_PORT_DECL:
      // Function argument
      for (unsigned i = 0; i < tree_ports(ctx->fdecl); i++) {
         if (decl == tree_port(ctx->fdecl, i))
            return LLVMGetParam(ctx->fn, i);
      }
      assert(false);

   default:
      abort();
   }

   return NULL;
}

static LLVMValueRef cgen_array_ref(tree_t t, struct cgen_ctx *ctx)
{
   tree_t decl = tree_ref(t);

   param_t p = tree_param(t, 0);
   assert(p.kind == P_POS);
   LLVMValueRef idx = cgen_expr(p.value, ctx);

   switch (tree_kind(decl)) {
   case T_VAR_DECL:
      {
         LLVMValueRef array_ptr = cgen_proc_var(decl, ctx);
         LLVMValueRef indexes[] = { llvm_int32(0), idx };
         LLVMValueRef ptr = LLVMBuildGEP(builder, array_ptr,
                                         indexes, ARRAY_LEN(indexes), "");
         return LLVMBuildLoad(builder, ptr, "");
      }

   case T_SIGNAL_DECL:
      {
         LLVMValueRef signal_array = llvm_global(istr(tree_ident(decl)));
         LLVMValueRef indexes[] = { llvm_int32(0), idx };
         LLVMValueRef signal = LLVMBuildGEP(builder, signal_array,
                                            indexes, ARRAY_LEN(indexes), "");
         LLVMValueRef ptr =
            LLVMBuildStructGEP(builder, signal, SIGNAL_RESOLVED, "");
         LLVMValueRef deref = LLVMBuildLoad(builder, ptr, "");
         return LLVMBuildIntCast(builder, deref,
                                 llvm_type(tree_type(t)), "");
      }

   default:
      assert(false);
   }
}

static LLVMValueRef cgen_array_slice(tree_t t, struct cgen_ctx *ctx)
{
   tree_t decl = tree_ref(t);
   range_t r = tree_range(t);

   LLVMValueRef low = cgen_range_low(r, ctx);

   switch (tree_kind(decl)) {
   case T_VAR_DECL:
      {
         LLVMValueRef ptr = cgen_proc_var(decl, ctx);
         LLVMValueRef indexes[] = {
            llvm_int32(0),
            cgen_array_off(low, tree_type(decl), ctx)
         };
         return LLVMBuildGEP(builder, ptr, indexes, ARRAY_LEN(indexes), "");
      }

   case T_SIGNAL_DECL:
      {
         return cgen_array_signal_ref(decl, tree_type(t), ctx, false);
      }

   default:
      assert(false);
   }
}

static LLVMValueRef cgen_aggregate(tree_t t, struct cgen_ctx *ctx)
{
   range_t r = type_dim(tree_type(t), 0);

   int64_t low, high;
   range_bounds(r, &low, &high);

   const size_t n_elems = high - low + 1;
   LLVMValueRef *vals = xmalloc(n_elems * sizeof(LLVMValueRef));

   for (unsigned i = 0; i < n_elems; i++)
      vals[i] = NULL;

   for (unsigned i = 0; i < tree_assocs(t); i++) {
      assoc_t a = tree_assoc(t, i);

      LLVMValueRef v = cgen_expr(a.value, ctx);

      switch (a.kind) {
      case A_POS:
         if (r.kind == RANGE_TO)
            vals[i] = v;
         else
            vals[n_elems - i - 1] = v;
         break;

      case A_NAMED:
         vals[assume_int(a.name) - low] = v;
         break;

      case A_OTHERS:
         for (unsigned j = 0; j < n_elems; j++) {
            if (vals[j] == NULL)
               vals[j] = v;
         }
         break;

      case A_RANGE:
         assert(false);
      }
   }

   for (unsigned i = 0; i < n_elems; i++)
      assert(vals[i] != NULL);

   LLVMTypeRef arr_ty = llvm_type(tree_type(t));
   LLVMTypeRef elm_ty = llvm_type(type_base(tree_type(t)));

   LLVMValueRef g = LLVMAddGlobal(module, arr_ty, "");
   LLVMSetGlobalConstant(g, true);
   LLVMSetLinkage(g, LLVMInternalLinkage);
   LLVMSetInitializer(g, LLVMConstArray(elm_ty, vals, n_elems));

   free(vals);
   return g;
}

static LLVMValueRef cgen_expr(tree_t t, struct cgen_ctx *ctx)
{
   switch (tree_kind(t)) {
   case T_LITERAL:
      return cgen_literal(t);
   case T_FCALL:
      return cgen_fcall(t, ctx);
   case T_REF:
      return cgen_ref(t, ctx);
   case T_ARRAY_REF:
      return cgen_array_ref(t, ctx);
   case T_ARRAY_SLICE:
      return cgen_array_slice(t, ctx);
   case T_AGGREGATE:
      return cgen_aggregate(t, ctx);
   case T_QUALIFIED:
      return cgen_expr(tree_value(t), ctx);
   default:
      fatal("missing cgen_expr for kind %d\n", tree_kind(t));
   }
}

static void cgen_sched_process(LLVMValueRef after)
{
   LLVMValueRef args[] = { after };
   LLVMBuildCall(builder, llvm_fn("_sched_process"), args, 1, "");
}

static void cgen_sched_event(tree_t on)
{
   assert(tree_kind(on) == T_REF);
   tree_t decl = tree_ref(on);

   LLVMValueRef signal = llvm_global(istr(tree_ident(decl)));
   LLVMValueRef args[] = { llvm_void_cast(signal) };
   LLVMBuildCall(builder, llvm_fn("_sched_event"),
                 args, ARRAY_LEN(args), "");
}

static void cgen_wait(tree_t t, struct cgen_ctx *ctx)
{
   if (tree_has_delay(t))
      cgen_sched_process(cgen_expr(tree_delay(t), ctx));

   for (unsigned i = 0; i < tree_triggers(t); i++)
      cgen_sched_event(tree_trigger(t, i));

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

static void cgen_var_assign(tree_t t, struct cgen_ctx *ctx)
{
   LLVMValueRef rhs = cgen_expr(tree_value(t), ctx);

   tree_t target = tree_target(t);
   switch (tree_kind(target)) {
   case T_REF:
      {
         LLVMValueRef lhs = cgen_proc_var(tree_ref(target), ctx);

         type_t ty = tree_type(target);
         if (type_kind(ty) == T_CARRAY)
            cgen_array_copy(ty, type_dim(ty, 0), rhs, lhs);
         else
            LLVMBuildStore(builder, rhs, lhs);
      }
      break;

   case T_ARRAY_REF:
      {
         tree_t target = tree_target(t);

         tree_t decl = tree_ref(target);
         assert(type_kind(tree_type(decl)) == T_CARRAY);

         param_t p = tree_param(target, 0);
         assert(p.kind == P_POS);

         LLVMValueRef idx = cgen_expr(p.value, ctx);

         LLVMValueRef array_ptr = cgen_proc_var(decl, ctx);
         LLVMValueRef indexes[] = { llvm_int32(0), idx };
         LLVMValueRef ptr = LLVMBuildGEP(builder, array_ptr,
                                         indexes, ARRAY_LEN(indexes), "");

         LLVMBuildStore(builder, rhs, ptr);
      }
      break;

   case T_ARRAY_SLICE:
      {
         LLVMValueRef lhs = cgen_proc_var(tree_ref(target), ctx);

         type_t ty = tree_type(tree_ref(target));
         assert(type_kind(ty) == T_CARRAY);

         cgen_array_copy(ty, tree_range(target), rhs, lhs);
      }
      break;

   default:
      assert(false);
   }
}

static void cgen_sched_waveform(LLVMValueRef signal, LLVMValueRef value,
                                LLVMValueRef after)
{
   LLVMValueRef args[] = {
      llvm_void_cast(signal),
      llvm_int32(0 /* source, TODO */),
      LLVMBuildZExt(builder, value, LLVMInt64Type(), ""),
      after
   };
   LLVMBuildCall(builder, llvm_fn("_sched_waveform"),
                 args, ARRAY_LEN(args), "");
}

static void cgen_array_signal_store(tree_t decl, LLVMValueRef rhs,
                                    LLVMValueRef after, struct cgen_ctx *ctx)
{
   char name[256];
   snprintf(name, sizeof(name), "%s_vec_store",
            istr(type_ident(type_base(tree_type(decl)))));

   range_t r = type_dim(tree_type(decl), 0);
   LLVMValueRef left =
      cgen_expr(r.kind == RANGE_TO ? r.left : r.right, ctx);
   LLVMValueRef right =
      cgen_expr(r.kind == RANGE_TO ? r.right : r.left, ctx);

   LLVMValueRef s_signal = llvm_global(istr(tree_ident(decl)));
   LLVMValueRef indexes[] = { llvm_int32(0), llvm_int32(0) };
   LLVMValueRef p_signal = LLVMBuildGEP(builder, s_signal,
                                        indexes, ARRAY_LEN(indexes), "");
   LLVMValueRef p_rhs = LLVMBuildGEP(builder, rhs,
                                     indexes, ARRAY_LEN(indexes), "");

   LLVMValueRef args[] = { p_signal, p_rhs, left, right };
   LLVMBuildCall(builder, llvm_fn(name), args, ARRAY_LEN(args), "");
}

static void cgen_scalar_signal_assign(tree_t t, LLVMValueRef rhs,
                                      LLVMValueRef after, struct cgen_ctx *ctx)
{
   tree_t decl = tree_ref(tree_target(t));
   if (type_kind(tree_type(decl)) == T_CARRAY)
      cgen_array_signal_store(decl, rhs, after, ctx);
   else
      cgen_sched_waveform(llvm_global(istr(tree_ident(decl))), rhs, after);
}

static void cgen_array_signal_assign(tree_t t, LLVMValueRef rhs,
                                     LLVMValueRef after, struct cgen_ctx *ctx)
{
   tree_t target = tree_target(t);

   tree_t decl = tree_ref(target);
   assert(type_kind(tree_type(decl)) == T_CARRAY);

   param_t p = tree_param(target, 0);
   assert(p.kind == P_POS);

   LLVMValueRef elem = cgen_expr(p.value, ctx);
   cgen_sched_waveform(cgen_array_signal_ptr(decl, elem), rhs, after);
}

static void cgen_slice_signal_assign(tree_t t, tree_t value, LLVMValueRef rhs,
                                     LLVMValueRef after, struct cgen_ctx *ctx)
{
   tree_t target = tree_target(t);

   tree_t decl = tree_ref(target);
   assert(type_kind(tree_type(decl)) == T_CARRAY);

   // TODO: make work for non-constant bounds
   int64_t low, high;
   range_bounds(tree_range(target), &low, &high);

   for (int i = low; i <= high; i++) {
      LLVMValueRef lhs_off =
         cgen_array_off(llvm_int32(i), tree_type(decl), ctx);
      LLVMValueRef rhs_off =
         cgen_array_off(llvm_int32(i), tree_type(value), ctx);

      LLVMValueRef indexes[] = { llvm_int32(0), rhs_off };
      LLVMValueRef rhs_p = LLVMBuildGEP(builder, rhs,
                                        indexes, ARRAY_LEN(indexes), "");
      LLVMValueRef rhs_i = LLVMBuildLoad(builder, rhs_p, "");
      cgen_sched_waveform(cgen_array_signal_ptr(decl, lhs_off), rhs_i, after);
   }
}

static void cgen_signal_assign(tree_t t, struct cgen_ctx *ctx)
{
   for (unsigned i = 0; i < tree_waveforms(t); i++) {
      tree_t w = tree_waveform(t, i);

      LLVMValueRef rhs = cgen_expr(tree_value(w), ctx);
      LLVMValueRef after = (tree_has_delay(w)
                            ? cgen_expr(tree_delay(w), ctx)
                            : llvm_int64(0));

      switch (tree_kind(tree_target(t))) {
      case T_REF:
         cgen_scalar_signal_assign(t, rhs, after, ctx);
         break;

      case T_ARRAY_REF:
         cgen_array_signal_assign(t, rhs, after, ctx);
         break;

      case T_ARRAY_SLICE:
         cgen_slice_signal_assign(t, tree_value(w), rhs, after, ctx);
         break;

      default:
         assert(false);
      }
   }
}

static LLVMValueRef cgen_array_to_c_string(LLVMValueRef a)
{
   LLVMValueRef indices[] = {
      llvm_int32(0), llvm_int32(0)
   };

   return LLVMBuildGEP(builder, a, indices, ARRAY_LEN(indices), "");
}

static void cgen_assert(tree_t t, struct cgen_ctx *ctx)
{
   int is_report = tree_attr_int(t, ident_new("is_report"), 0);

   LLVMValueRef message  = cgen_expr(tree_message(t), ctx);
   LLVMValueRef severity = cgen_expr(tree_severity(t), ctx);

   LLVMBasicBlockRef thenbb, elsebb;
   if (!is_report) {
      LLVMValueRef test = cgen_expr(tree_value(t), ctx);
      LLVMValueRef failed = LLVMBuildNot(builder, test, "");

      thenbb = LLVMAppendBasicBlock(ctx->fn, "assert_fail");
      elsebb = LLVMAppendBasicBlock(ctx->fn, "assert_pass");

      LLVMBuildCondBr(builder, failed, thenbb, elsebb);

      LLVMPositionBuilderAtEnd(builder, thenbb);
   }

   int slen = -1;  // String is NULL terminated
   LLVMValueRef msg_val = NULL;
   type_t msg_type = tree_type(tree_message(t));
   if (type_kind(msg_type) == T_CARRAY) {
      int64_t slow, shigh;
      range_bounds(type_dim(msg_type, 0), &slow, &shigh);
      slen = shigh - slow + 1;
      msg_val = cgen_array_to_c_string(message);
   }
   else
      msg_val = message;

   LLVMValueRef args[] = {
      LLVMConstInt(LLVMInt8Type(), is_report, false),
      msg_val,
      LLVMConstInt(LLVMInt32Type(), slen, false),
      severity
   };
   LLVMBuildCall(builder, llvm_fn("_assert_fail"),
                 args, ARRAY_LEN(args), "");

   if (!is_report) {
      LLVMBuildBr(builder, elsebb);
      LLVMPositionBuilderAtEnd(builder, elsebb);
   }
}

static void cgen_if(tree_t t, struct cgen_ctx *ctx)
{
   LLVMBasicBlockRef then_bb = LLVMAppendBasicBlock(ctx->fn, "then");
   LLVMBasicBlockRef else_bb = LLVMAppendBasicBlock(ctx->fn, "else");

   LLVMBasicBlockRef end_bb =
      (tree_else_stmts(t) > 0)
      ? LLVMAppendBasicBlock(ctx->fn, "ifend")
      : else_bb;

   LLVMValueRef test = cgen_expr(tree_value(t), ctx);
   LLVMBuildCondBr(builder, test, then_bb, else_bb);

   LLVMPositionBuilderAtEnd(builder, then_bb);

   for (unsigned i = 0; i < tree_stmts(t); i++)
      cgen_stmt(tree_stmt(t, i), ctx);

   LLVMBuildBr(builder, end_bb);

   if (tree_else_stmts(t) > 0) {
      LLVMPositionBuilderAtEnd(builder, else_bb);

      for (unsigned i = 0; i < tree_else_stmts(t); i++)
         cgen_stmt(tree_else_stmt(t, i), ctx);

      LLVMBuildBr(builder, end_bb);
   }

   LLVMPositionBuilderAtEnd(builder, end_bb);
}

static void cgen_return(tree_t t, struct cgen_ctx *ctx)
{
   LLVMValueRef rval = cgen_expr(tree_value(t), ctx);
   LLVMBuildRet(builder, rval);
}

static void cgen_while(tree_t t, struct cgen_ctx *ctx)
{
   LLVMBasicBlockRef test_bb = LLVMAppendBasicBlock(ctx->fn, "while");
   LLVMBasicBlockRef body_bb = LLVMAppendBasicBlock(ctx->fn, "wbody");
   LLVMBasicBlockRef exit_bb = LLVMAppendBasicBlock(ctx->fn, "wexit");

   LLVMBuildBr(builder, test_bb);

   LLVMPositionBuilderAtEnd(builder, test_bb);
   LLVMValueRef test = cgen_expr(tree_value(t), ctx);
   LLVMBuildCondBr(builder, test, body_bb, exit_bb);

   LLVMPositionBuilderAtEnd(builder, body_bb);
   for (unsigned i = 0; i < tree_stmts(t); i++)
      cgen_stmt(tree_stmt(t, i), ctx);
   LLVMBuildBr(builder, test_bb);

   LLVMPositionBuilderAtEnd(builder, exit_bb);
}

static void cgen_block(tree_t t, struct cgen_ctx *ctx)
{
   for (unsigned i = 0; i < tree_stmts(t); i++)
      cgen_stmt(tree_stmt(t, i), ctx);
}

static void cgen_stmt(tree_t t, struct cgen_ctx *ctx)
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
   case T_IF:
      cgen_if(t, ctx);
      break;
   case T_RETURN:
      cgen_return(t, ctx);
      break;
   case T_WHILE:
      cgen_while(t, ctx);
      break;
   case T_BLOCK:
      cgen_block(t, ctx);
      break;
   default:
      assert(false);
   }
}

static void cgen_jump_table_fn(tree_t t, void *arg)
{
   assert(tree_kind(t) == T_WAIT);

   struct cgen_ctx *ctx = arg;

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

   struct cgen_ctx *ctx = arg;

   tree_t target = tree_target(t);
   assert(tree_kind(target) == T_REF
          || tree_kind(target) == T_ARRAY_REF
          || tree_kind(target) == T_ARRAY_SLICE);

   tree_t decl = tree_ref(target);
   assert(tree_kind(decl) == T_SIGNAL_DECL);

   ident_t tag_i = ident_new("driver_tag");
   if (tree_attr_ptr(decl, tag_i) == ctx->proc)
      return;   // Already initialised this signal

   LLVMValueRef val =
      tree_has_value(decl)
      ? cgen_expr(tree_value(decl), ctx)
      : cgen_default_value(tree_type(decl));

   type_t type = tree_type(decl);
   if (type_kind(type) == T_CARRAY) {
      // Initialise only those sub-elements for which this
      // process is a driver

      int64_t low, high;
      range_bounds(type_dim(type, 0), &low, &high);

      for (unsigned i = 0; i < high - low + 1; i++) {
         for (unsigned j = 0; j < tree_sub_drivers(decl, i); j++) {
            if (tree_sub_driver(decl, i, j) == ctx->proc) {
               LLVMValueRef ptr = cgen_array_signal_ptr(decl, llvm_int32(i));
               LLVMValueRef indices[] = { llvm_int32(0), llvm_int32(i) };
               LLVMValueRef ith = LLVMBuildGEP(builder, val, indices,
                                               ARRAY_LEN(indices), "");
               LLVMValueRef deref = LLVMBuildLoad(builder, ith, "");
               cgen_sched_waveform(ptr, deref, llvm_int64(0));
            }
         }
      }
   }
   else
      cgen_sched_waveform(llvm_global(istr(tree_ident(decl))), val,
                          llvm_int64(0));

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

   struct cgen_ctx ctx = {
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
   ctx.fn = LLVMAddFunction(module, istr(tree_ident(t)), ftype);

   LLVMBasicBlockRef entry_bb = LLVMAppendBasicBlock(ctx.fn, "entry");
   LLVMBasicBlockRef jt_bb    = LLVMAppendBasicBlock(ctx.fn, "jump_table");
   LLVMBasicBlockRef init_bb  = LLVMAppendBasicBlock(ctx.fn, "init");
   LLVMBasicBlockRef start_bb = LLVMAppendBasicBlock(ctx.fn, "start");

   LLVMPositionBuilderAtEnd(builder, entry_bb);

   // If the parameter is non-zero jump to the init block

   LLVMValueRef param = LLVMGetParam(ctx.fn, 0);
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
      it->bb = LLVMAppendBasicBlock(ctx.fn, istr(tree_ident(it->wait)));

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

         LLVMValueRef var_ptr = cgen_proc_var(v, &ctx);

         type_t ty = tree_type(v);
         if (type_kind(ty) == T_CARRAY)
            cgen_array_copy(ty, type_dim(ty, 0), val, var_ptr);
         else
            LLVMBuildStore(builder, val, var_ptr);
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

   // Free context memory

   while (ctx.entry_list != NULL) {
      struct proc_entry *next = ctx.entry_list->next;
      free(ctx.entry_list);
      ctx.entry_list = next;
   }
}

static LLVMTypeRef cgen_signal_type(void)
{
   LLVMTypeRef ty = LLVMGetTypeByName(module, "signal_s");
   if (ty == NULL) {
      LLVMTypeRef fields[SIGNAL_N_FIELDS];
      fields[SIGNAL_RESOLVED]   = LLVMInt64Type();
      fields[SIGNAL_LAST_VALUE] = LLVMInt64Type();
      fields[SIGNAL_DECL]       = llvm_void_ptr();
      fields[SIGNAL_FLAGS]      = LLVMInt32Type();
      fields[SIGNAL_N_SOURCES]  = LLVMInt32Type();
      fields[SIGNAL_SOURCES]    = llvm_void_ptr();
      fields[SIGNAL_SENSITIVE]  = llvm_void_ptr();

      ty = LLVMStructType(fields, ARRAY_LEN(fields), false);

      if (LLVMAddTypeName(module, "signal_s", ty))
         fatal("failed to add type name signal_s");
   }

   return ty;
}

static LLVMValueRef cgen_signal_init(void)
{
   LLVMValueRef init[SIGNAL_N_FIELDS];
   init[SIGNAL_RESOLVED]   = llvm_int64(0);
   init[SIGNAL_LAST_VALUE] = llvm_int64(0);
   init[SIGNAL_DECL]       = LLVMConstNull(llvm_void_ptr());
   init[SIGNAL_FLAGS]      = llvm_int32(0);
   init[SIGNAL_N_SOURCES]  = llvm_int32(0);
   init[SIGNAL_SOURCES]    = LLVMConstNull(llvm_void_ptr());
   init[SIGNAL_SENSITIVE]  = LLVMConstNull(llvm_void_ptr());

   return LLVMConstStruct(init, ARRAY_LEN(init), false);
}

static void cgen_scalar_signal(tree_t t)
{
   LLVMTypeRef ty = cgen_signal_type();
   LLVMValueRef v = LLVMAddGlobal(module, ty, istr(tree_ident(t)));
   LLVMSetInitializer(v, cgen_signal_init());
}

static void cgen_array_signal_load_fn(tree_t t, LLVMValueRef v)
{
   // Build a function to load the array into a temporary

   type_t elem_type = type_base(tree_type(t));

   char name[256];
   snprintf(name, sizeof(name), "%s_vec_load",
            istr(type_ident(elem_type)));

   LLVMValueRef fn;
   if ((fn = LLVMGetNamedFunction(module, name)))
      return;

   LLVMBasicBlockRef saved_bb = LLVMGetInsertBlock(builder);

   LLVMTypeRef args[] = {
      LLVMPointerType(cgen_signal_type(), 0),
      LLVMPointerType(llvm_type(elem_type), 0),
      LLVMInt32Type(),    // Left
      LLVMInt32Type(),    // Right
      LLVMInt1Type()      // Last value
   };
   fn = LLVMAddFunction(module, name,
                        LLVMFunctionType(LLVMVoidType(),
                                         args, ARRAY_LEN(args), false));

   LLVMBasicBlockRef entry_bb = LLVMAppendBasicBlock(fn, "entry");
   LLVMBasicBlockRef test_bb  = LLVMAppendBasicBlock(fn, "test");
   LLVMBasicBlockRef body_bb  = LLVMAppendBasicBlock(fn, "body");
   LLVMBasicBlockRef norm_bb  = LLVMAppendBasicBlock(fn, "normal");
   LLVMBasicBlockRef last_bb  = LLVMAppendBasicBlock(fn, "last");
   LLVMBasicBlockRef merge_bb = LLVMAppendBasicBlock(fn, "merge");
   LLVMBasicBlockRef exit_bb  = LLVMAppendBasicBlock(fn, "exit");

   // Prelude
   LLVMPositionBuilderAtEnd(builder, entry_bb);
   LLVMValueRef i = LLVMBuildAlloca(builder, LLVMInt32Type(), "i");
   LLVMBuildStore(builder, LLVMGetParam(fn, 2), i);
   LLVMBuildBr(builder, test_bb);

   // Loop test
   LLVMPositionBuilderAtEnd(builder, test_bb);
   LLVMValueRef i_loaded = LLVMBuildLoad(builder, i, "");
   LLVMValueRef ge = LLVMBuildICmp(builder, LLVMIntUGE,
                                   LLVMGetParam(fn, 3),
                                   i_loaded, "ge");
   LLVMBuildCondBr(builder, ge, body_bb, exit_bb);

   // Loop body
   LLVMPositionBuilderAtEnd(builder, body_bb);

   LLVMValueRef index[] = { i_loaded };
   LLVMValueRef signal = LLVMBuildGEP(builder, LLVMGetParam(fn, 0),
                                      index, ARRAY_LEN(index),
                                      "signal");

   // Select either the current or last value

   LLVMBuildCondBr(builder, LLVMGetParam(fn, 4), last_bb, norm_bb);

   LLVMPositionBuilderAtEnd(builder, last_bb);
   LLVMValueRef ptr_last =
      LLVMBuildStructGEP(builder, signal, SIGNAL_LAST_VALUE, "last_value");
   LLVMBuildBr(builder, merge_bb);

   LLVMPositionBuilderAtEnd(builder, norm_bb);
   LLVMValueRef ptr_resolved =
      LLVMBuildStructGEP(builder, signal, SIGNAL_RESOLVED, "resolved");
   LLVMBuildBr(builder, merge_bb);

   LLVMPositionBuilderAtEnd(builder, merge_bb);
   LLVMValueRef phi = LLVMBuildPhi(builder, LLVMTypeOf(ptr_last), "ptr");

   LLVMValueRef      values[] = { ptr_last, ptr_resolved };
   LLVMBasicBlockRef bbs[]    = { last_bb,  norm_bb      };
   LLVMAddIncoming(phi, values, bbs, 2);

   LLVMValueRef deref = LLVMBuildLoad(builder, phi, "deref");
   LLVMValueRef val = LLVMBuildIntCast(builder, deref,
                                       llvm_type(elem_type), "val");
   LLVMValueRef dst_index[] = {
      LLVMBuildSub(builder, i_loaded, LLVMGetParam(fn, 2), "")
   };
   LLVMValueRef dst = LLVMBuildGEP(builder, LLVMGetParam(fn, 1),
                                   dst_index, ARRAY_LEN(dst_index),
                                   "dst");
   LLVMBuildStore(builder, val, dst);

   LLVMValueRef inc =
      LLVMBuildAdd(builder, i_loaded, llvm_int32(1), "inc");
   LLVMBuildStore(builder, inc, i);
   LLVMBuildBr(builder, test_bb);

   // Epilogue
   LLVMPositionBuilderAtEnd(builder, exit_bb);
   LLVMBuildRetVoid(builder);

   LLVMPositionBuilderAtEnd(builder, saved_bb);
}

static void cgen_array_signal_store_fn(tree_t t, LLVMValueRef v)
{
   // Build a function to schedule an array assignment

   type_t elem_type = type_base(tree_type(t));

   char name[256];
   snprintf(name, sizeof(name), "%s_vec_store",
            istr(type_ident(elem_type)));

   LLVMValueRef fn;
   if ((fn = LLVMGetNamedFunction(module, name)))
      return;

   LLVMBasicBlockRef saved_bb = LLVMGetInsertBlock(builder);

   LLVMTypeRef args[] = {
      LLVMPointerType(cgen_signal_type(), 0),
      LLVMPointerType(llvm_type(elem_type), 0),
      LLVMInt32Type(),    // Left
      LLVMInt32Type(),    // Right
   };
   fn = LLVMAddFunction(module, name,
                        LLVMFunctionType(LLVMVoidType(),
                                         args, ARRAY_LEN(args), false));

   LLVMBasicBlockRef entry_bb = LLVMAppendBasicBlock(fn, "entry");
   LLVMBasicBlockRef test_bb  = LLVMAppendBasicBlock(fn, "test");
   LLVMBasicBlockRef body_bb  = LLVMAppendBasicBlock(fn, "body");
   LLVMBasicBlockRef exit_bb  = LLVMAppendBasicBlock(fn, "exit");

   // Prelude
   LLVMPositionBuilderAtEnd(builder, entry_bb);
   LLVMValueRef i = LLVMBuildAlloca(builder, LLVMInt32Type(), "i");
   LLVMBuildStore(builder, LLVMGetParam(fn, 2), i);
   LLVMBuildBr(builder, test_bb);

   // Loop test
   LLVMPositionBuilderAtEnd(builder, test_bb);
   LLVMValueRef i_loaded = LLVMBuildLoad(builder, i, "");
   LLVMValueRef ge = LLVMBuildICmp(builder, LLVMIntUGE,
                                   LLVMGetParam(fn, 3),
                                   i_loaded, "ge");
   LLVMBuildCondBr(builder, ge, body_bb, exit_bb);

   // Loop body
   LLVMPositionBuilderAtEnd(builder, body_bb);

   LLVMValueRef index[] = { i_loaded };
   LLVMValueRef signal = LLVMBuildGEP(builder, LLVMGetParam(fn, 0),
                                      index, ARRAY_LEN(index),
                                      "signal");
   LLVMValueRef p_src = LLVMBuildGEP(builder, LLVMGetParam(fn, 1),
                                     index, ARRAY_LEN(index),
                                     "p_src");
   LLVMValueRef val = LLVMBuildLoad(builder, p_src, "val");
   cgen_sched_waveform(signal, val, llvm_int64(0));

   LLVMValueRef inc =
      LLVMBuildAdd(builder, i_loaded, llvm_int32(1), "inc");
   LLVMBuildStore(builder, inc, i);
   LLVMBuildBr(builder, test_bb);

   // Epilogue
   LLVMPositionBuilderAtEnd(builder, exit_bb);
   LLVMBuildRetVoid(builder);

   LLVMPositionBuilderAtEnd(builder, saved_bb);
}

static void cgen_array_signal(tree_t t)
{
   assert(tree_drivers(t) == 0);

   range_t r = type_dim(tree_type(t), 0);
   int64_t low, high;
   range_bounds(r, &low, &high);

   const unsigned n_elems = high - low + 1;

   LLVMTypeRef base_ty = cgen_signal_type();
   LLVMTypeRef array_ty = LLVMArrayType(base_ty, n_elems);
   LLVMValueRef v = LLVMAddGlobal(module, array_ty, istr(tree_ident(t)));

   LLVMValueRef array_init[n_elems];
   for (unsigned i = 0; i < n_elems; i++)
      array_init[i] = cgen_signal_init();
   LLVMSetInitializer(v, LLVMConstArray(base_ty, array_init, n_elems));

   cgen_array_signal_load_fn(t, v);
   cgen_array_signal_store_fn(t, v);
}

static void cgen_signal(tree_t t)
{
   assert(tree_kind(t) == T_SIGNAL_DECL);

   if (type_kind(tree_type(t)) == T_CARRAY)
      cgen_array_signal(t);
   else
      cgen_scalar_signal(t);
}

static void cgen_func_body(tree_t t)
{
   type_t ftype = tree_type(t);

   LLVMTypeRef args[type_params(ftype)];
   for (unsigned i = 0; i < type_params(ftype); i++)
      args[i] = llvm_type(type_param(ftype, i));

   LLVMValueRef fn =
      LLVMAddFunction(module, istr(tree_ident(t)),
                      LLVMFunctionType(llvm_type(type_result(ftype)),
                                       args, type_params(ftype), false));

   LLVMBasicBlockRef entry_bb = LLVMAppendBasicBlock(fn, "entry");
   LLVMPositionBuilderAtEnd(builder, entry_bb);

   struct cgen_ctx ctx = {
      .entry_list = NULL,
      .proc       = NULL,
      .fdecl      = t,
      .fn         = fn
   };

   for (unsigned i = 0; i < tree_stmts(t); i++)
      cgen_stmt(tree_stmt(t, i), &ctx);
}

static void cgen_top(tree_t t)
{
   assert(tree_kind(t) == T_ELAB);

   for (unsigned i = 0; i < tree_decls(t); i++) {
      tree_t decl = tree_decl(t, i);
      switch (tree_kind(decl)) {
      case T_SIGNAL_DECL:
         cgen_signal(decl);
         break;
      case T_FUNC_BODY:
         cgen_func_body(decl);
         break;
      default:
         assert(false);
      }
   }

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
      llvm_void_ptr(),
      LLVMInt32Type(),
      LLVMInt64Type(),
      LLVMInt64Type()
   };
   LLVMAddFunction(module, "_sched_waveform",
                   LLVMFunctionType(LLVMVoidType(),
                                    _sched_waveform_args,
                                    ARRAY_LEN(_sched_waveform_args),
                                    false));

   LLVMTypeRef _sched_event_args[] = {
      llvm_void_ptr()
   };
   LLVMAddFunction(module, "_sched_event",
                   LLVMFunctionType(LLVMVoidType(),
                                    _sched_event_args,
                                    ARRAY_LEN(_sched_event_args),
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

   LLVMTypeRef _array_copy_args[] = {
      llvm_void_ptr(),
      llvm_void_ptr(),
      LLVMInt32Type(),
      LLVMInt32Type(),
      LLVMInt32Type()
   };
   LLVMAddFunction(module, "_array_copy",
                   LLVMFunctionType(LLVMVoidType(),
                                    _array_copy_args,
                                    ARRAY_LEN(_array_copy_args),
                                    false));

   LLVMTypeRef _array_eq_args[] = {
      llvm_void_ptr(),
      llvm_void_ptr(),
      LLVMInt32Type(),
      LLVMInt32Type(),
      LLVMInt32Type()
   };
   LLVMAddFunction(module, "_array_eq",
                   LLVMFunctionType(LLVMInt1Type(),
                                    _array_eq_args,
                                    ARRAY_LEN(_array_eq_args),
                                    false));

   LLVMTypeRef _image_args[] = {
      LLVMInt64Type()
   };
   LLVMAddFunction(module, "_image",
                   LLVMFunctionType(llvm_void_ptr(),
                                    _image_args,
                                    ARRAY_LEN(_image_args),
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

   if (dump_module)
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

void cgen_dump_en(bool on)
{
   dump_module = on;
}
