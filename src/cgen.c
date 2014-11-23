//
//  Copyright (C) 2011-2014  Nick Gasson
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
#include "phase.h"
#include "lib.h"
#include "common.h"
#include "vcode.h"
#include "rt/rt.h"
#include "rt/cover.h"

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

typedef struct {
   LLVMValueRef      *regs;
   LLVMBasicBlockRef *blocks;
   LLVMValueRef       fn;
   LLVMValueRef       state;
   size_t             var_base;
   LLVMValueRef      *locals;
} cgen_ctx_t;

static LLVMModuleRef  module = NULL;
static LLVMBuilderRef builder = NULL;
static LLVMValueRef   mod_name = NULL;

static LLVMValueRef cgen_support_fn(const char *name);

#if 0
static LLVMValueRef llvm_int1(bool b)
{
   return LLVMConstInt(LLVMInt1Type(), b, false);
}
#endif

static LLVMValueRef llvm_int8(int8_t i)
{
   return LLVMConstInt(LLVMInt8Type(), i, false);
}

static LLVMValueRef llvm_int32(int32_t i)
{
   return LLVMConstInt(LLVMInt32Type(), i, false);
}

static LLVMValueRef llvm_int64(int64_t i)
{
   return LLVMConstInt(LLVMInt64Type(), i, false);
}

#if 0
static LLVMValueRef llvm_real(double r)
{
   return LLVMConstReal(LLVMDoubleType(), r);
}
#endif

static LLVMTypeRef llvm_void_ptr(void)
{
   return LLVMPointerType(LLVMInt8Type(), 0);
}

#if 0
static LLVMValueRef llvm_void_cast(LLVMValueRef ptr)
{
   return LLVMBuildPointerCast(builder, ptr, llvm_void_ptr(), "");
}
#endif

#if 0
static LLVMValueRef llvm_sizeof(LLVMTypeRef type)
{
   return LLVMBuildIntCast(builder, LLVMSizeOf(type),
                           LLVMInt32Type(), "");
}
#endif

static LLVMValueRef llvm_fn(const char *name)
{
   LLVMValueRef fn = LLVMGetNamedFunction(module, name);
   if ((fn == NULL) && ((fn = cgen_support_fn(name)) == NULL))
      fatal("cannot find named function %s", name);
   return fn;
}

static void llvm_str(LLVMValueRef *chars, size_t n, const char *str)
{
   for (size_t i = 0; i < n; i++)
      chars[i] = llvm_int8(*str ? *(str++) : '\0');
}

static LLVMTypeRef llvm_uarray_type(LLVMTypeRef base, int dims)
{
   // Unconstrained arrays are represented by a structure
   // containing the left and right indices, a flag indicating
   // direction, and a pointer to the array data

   LLVMTypeRef dim_fields[] = {
      LLVMInt32Type(),      // Left
      LLVMInt32Type(),      // Right
      LLVMInt1Type()        // Direction
   };

   LLVMTypeRef dim_struct =
      LLVMStructType(dim_fields, ARRAY_LEN(dim_fields), false);

   LLVMTypeRef fields[] = {
      LLVMPointerType(base, 0),
      LLVMArrayType(dim_struct, dims)
   };

   return LLVMStructType(fields, ARRAY_LEN(fields), false);
}

static LLVMTypeRef cgen_type(vcode_type_t type)
{
   switch (vtype_kind(type)) {
   case VCODE_TYPE_INT:
      {
         const int64_t low  = vtype_low(type);
         const int64_t high = vtype_high(type);

         if (low < 0) {
            // Signed integers
            if (low >= INT8_MIN && high <= INT8_MAX)
               return LLVMInt8Type();
            else if (low >= INT16_MIN && high <= INT16_MAX)
               return LLVMInt16Type();
            else if (low >= INT32_MIN && high <= INT32_MAX)
               return LLVMInt32Type();
            else
               return LLVMInt64Type();
         }
         else {
            // Unsigned integers
            if (high <= 1)
               return LLVMInt1Type();
            else if (high <= UINT8_MAX)
               return LLVMInt8Type();
            else if (high <= UINT16_MAX)
               return LLVMInt16Type();
            else if (high <= UINT32_MAX)
               return LLVMInt32Type();
            else
               return LLVMInt64Type();
         }
      }
      break;

   case VCODE_TYPE_CARRAY:
      {
         assert(vtype_dims(type) == 1);
         vcode_type_t dim0 = vtype_dim(type, 0);

         const int64_t low  = vtype_low(dim0);
         const int64_t high = vtype_high(dim0);

         return LLVMArrayType(cgen_type(vtype_elem(type)), high - low + 1);
      }
      break;

   case VCODE_TYPE_UARRAY:
      return llvm_uarray_type(cgen_type(vtype_elem(type)), vtype_dims(type));

   case VCODE_TYPE_OFFSET:
      return LLVMInt32Type();

   default:
      fatal("cannot convert vcode type %d to LLVM", vtype_kind(type));
   }
}

static const char *cgen_reg_name(vcode_reg_t r)
{
   static char buf[32];
   checked_sprintf(buf, sizeof(buf), "r%d", r);
   return buf;
}

static LLVMValueRef cgen_get_arg(int op, int arg, cgen_ctx_t *ctx)
{
   vcode_reg_t r = vcode_get_arg(op, arg);
   if (unlikely(ctx->regs[r] == NULL))
      fatal_trace("definition of register r%d is NULL", r);
   return ctx->regs[r];
}

static LLVMValueRef cgen_get_var(vcode_var_t var, cgen_ctx_t *ctx)
{
   if (ctx->state != NULL)
      return LLVMBuildStructGEP(builder, ctx->state, ctx->var_base + var,
                                istr(vcode_var_name(var)));
   else {
      assert(ctx->locals != NULL);
      return ctx->locals[var];
   }
}

static LLVMValueRef cgen_tmp_alloc(LLVMValueRef bytes, LLVMTypeRef type)
{
   LLVMValueRef _tmp_stack_ptr = LLVMGetNamedGlobal(module, "_tmp_stack");
   LLVMValueRef _tmp_alloc_ptr = LLVMGetNamedGlobal(module, "_tmp_alloc");

   LLVMValueRef alloc = LLVMBuildLoad(builder, _tmp_alloc_ptr, "alloc");
   LLVMValueRef stack = LLVMBuildLoad(builder, _tmp_stack_ptr, "stack");

   LLVMValueRef indexes[] = { alloc };
   LLVMValueRef buf = LLVMBuildGEP(builder, stack,
                                   indexes, ARRAY_LEN(indexes), "");

   LLVMValueRef alloc_next =
      LLVMBuildAnd(builder,
                   LLVMBuildAdd(builder, alloc,
                                LLVMBuildAdd(builder, bytes, llvm_int32(3), ""),
                                "alloc_align_max"),
                   llvm_int32(~3),
                   "alloc_next");

   LLVMBuildStore(builder, alloc_next, _tmp_alloc_ptr);

   return LLVMBuildPointerCast(builder, buf,
                               LLVMPointerType(type, 0), "tmp_buf");
}

static bool cgen_is_uarray_struct(LLVMValueRef meta)
{
   return LLVMGetTypeKind(LLVMTypeOf(meta)) == LLVMStructTypeKind;
}

static LLVMValueRef cgen_uarray_dim(LLVMValueRef meta, int dim)
{
   assert(cgen_is_uarray_struct(meta));
   LLVMValueRef dim_array =
      LLVMBuildExtractValue(builder, meta, 1, "dim_array");
   return LLVMBuildExtractValue(builder, dim_array, dim, "dim");
}

static LLVMValueRef cgen_array_data_ptr(vcode_reg_t reg, cgen_ctx_t *ctx)
{
   char name[32];
   checked_sprintf(name, sizeof(name), "r%d_aptr", reg);

   vcode_type_t type = vcode_reg_type(reg);
   switch (vtype_kind(type)) {
   case VCODE_TYPE_CARRAY:
      {
         LLVMValueRef indexes[] = { llvm_int32(0) };
         return LLVMBuildPointerCast(
            builder,
            LLVMBuildGEP(builder, ctx->regs[reg],
                         indexes, ARRAY_LEN(indexes), ""),
            LLVMPointerType(cgen_type(vtype_elem(type)), 0), name);
      }
      break;

   case VCODE_TYPE_UARRAY:
      {
         assert(cgen_is_uarray_struct(ctx->regs[reg]));
         return LLVMBuildExtractValue(builder, ctx->regs[reg], 0, name);
      }

   default:
      fatal_trace("non-array type in %s", __func__);
   }
}

static LLVMValueRef cgen_array_len(vcode_reg_t reg, cgen_ctx_t *ctx)
{
   vcode_type_t type = vcode_reg_type(reg);
   switch (vtype_kind(type)) {
   case VCODE_TYPE_CARRAY:
      {
         assert(vtype_dims(type) == 1);
         vcode_type_t dim0 = vtype_dim(type, 0);

         const int64_t low  = vtype_low(dim0);
         const int64_t high = vtype_high(dim0);

         return llvm_int32(high - low + 1);
      }

   case VCODE_TYPE_UARRAY:
      {
         assert(vtype_dims(type) == 1);

         LLVMValueRef meta = ctx->regs[reg];
         assert(meta != NULL);
         assert(cgen_is_uarray_struct(meta));

         LLVMValueRef dim_struct = cgen_uarray_dim(meta, 0 /* XXX */);

         LLVMValueRef downto = LLVMBuildICmp(
            builder, LLVMIntEQ,
            LLVMBuildExtractValue(builder, dim_struct, 2, "dir"),
            llvm_int8(RANGE_DOWNTO),
            "downto");
         LLVMValueRef left =
            LLVMBuildExtractValue(builder, dim_struct, 0, "left");
         LLVMValueRef right =
            LLVMBuildExtractValue(builder, dim_struct, 1, "right");
         LLVMValueRef diff =
            LLVMBuildSelect(builder, downto,
                            LLVMBuildSub(builder, left, right, ""),
                            LLVMBuildSub(builder, right, left, ""),
                            "diff");
         LLVMValueRef len =
            LLVMBuildAdd(builder, diff, llvm_int32(1), "len");
         LLVMValueRef neg = LLVMBuildICmp(builder, LLVMIntSLT, len,
                                          llvm_int32(0), "negative");
         LLVMValueRef clamp =
            LLVMBuildSelect(builder, neg, llvm_int32(0), len, "len_clamp");

         return clamp;
      }

   default:
      fatal_trace("non-array type in %s", __func__);
   }
}

static void cgen_sched_process(LLVMValueRef after)
{
   LLVMValueRef args[] = { after };
   LLVMBuildCall(builder, llvm_fn("_sched_process"), args, 1, "");
}

static void cgen_op_return(int op, cgen_ctx_t *ctx)
{
   if (vcode_count_args(op) > 0)
      LLVMBuildRet(builder, cgen_get_arg(op, 0, ctx));
   else
      LLVMBuildRetVoid(builder);
}

static void cgen_op_jump(int i, cgen_ctx_t *ctx)
{
   LLVMBuildBr(builder, ctx->blocks[vcode_get_target(i, 0)]);
}

static void cgen_op_fcall(int op, cgen_ctx_t *ctx)
{
   ident_t func = vcode_get_func(op);

   vcode_reg_t result = vcode_get_result(op);

   LLVMValueRef fn = LLVMGetNamedFunction(module, istr(func));
   if (fn == NULL) {
      fn = LLVMAddFunction(
         module,
         istr(func),
         LLVMFunctionType(cgen_type(vcode_reg_type(result)), NULL, 0, false));

      LLVMAddFunctionAttr(fn, LLVMNoUnwindAttribute);
   }

   const int nargs = vcode_count_args(op);
   LLVMValueRef args[nargs];
   for (int i = 0; i < nargs; i++)
      args[i] = cgen_get_arg(op, i, ctx);

   ctx->regs[result] = LLVMBuildCall(builder, fn, args, nargs,
                                     cgen_reg_name(result));
}

static void cgen_op_const(int op, cgen_ctx_t *ctx)
{
   vcode_reg_t result = vcode_get_result(op);
   ctx->regs[result] = LLVMConstInt(cgen_type(vcode_reg_type(result)),
                                    vcode_get_value(op), false);
}

static void cgen_op_const_array(int op, cgen_ctx_t *ctx)
{
   vcode_reg_t result = vcode_get_result(op);
   vcode_type_t type  = vcode_reg_type(result);

   char *name LOCAL = xasprintf("%s_const_array_r%d",
                                istr(vcode_unit_name()), result);

   LLVMValueRef global = LLVMAddGlobal(module, cgen_type(type), name);
   LLVMSetLinkage(global, LLVMInternalLinkage);
   LLVMSetGlobalConstant(global, true);

   const int length = vcode_count_args(op);
   LLVMValueRef *tmp LOCAL = xmalloc(length * sizeof(LLVMValueRef));
   for (int i = 0; i < length; i++)
      tmp[i] = ctx->regs[vcode_get_arg(op, i)];

   LLVMValueRef init = LLVMConstArray(cgen_type(vtype_elem(type)), tmp, length);
   LLVMSetInitializer(global, init);

   LLVMValueRef index[] = { llvm_int32(0), llvm_int32(0) };
   ctx->regs[result] = LLVMBuildGEP(builder, global, index, ARRAY_LEN(index),
                                    cgen_reg_name(result));
}

static void cgen_op_cmp(int op, cgen_ctx_t *ctx)
{
   vcode_reg_t result = vcode_get_result(op);

   const bool is_signed = vtype_low(vcode_reg_type(result)) < 0;

   LLVMIntPredicate pred = 0;
   switch (vcode_get_cmp(op)) {
   case VCODE_CMP_EQ:  pred = LLVMIntEQ; break;
   case VCODE_CMP_NEQ: pred = LLVMIntNE; break;
   case VCODE_CMP_LT:  pred = is_signed ? LLVMIntSLT : LLVMIntULT; break;
   case VCODE_CMP_GT:  pred = is_signed ? LLVMIntSGT : LLVMIntUGT; break;
   case VCODE_CMP_LEQ: pred = is_signed ? LLVMIntSLE : LLVMIntULE; break;
   case VCODE_CMP_GEQ: pred = is_signed ? LLVMIntSGE : LLVMIntUGE; break;
   }

   ctx->regs[result] =
      LLVMBuildICmp(builder, pred, cgen_get_arg(op, 0, ctx),
                    cgen_get_arg(op, 1, ctx), cgen_reg_name(result));
}

static void cgen_op_report(int op, cgen_ctx_t *ctx)
{
   vcode_reg_t mreg = vcode_get_arg(op, 1);

   LLVMValueRef severity    = ctx->regs[vcode_get_arg(op, 0)];
   LLVMValueRef message     = cgen_array_data_ptr(mreg, ctx);
   LLVMValueRef message_len = cgen_array_len(mreg, ctx);

   LLVMValueRef args[] = {
      message,
      message_len,
      severity,
      llvm_int32(vcode_get_index(op)),
      LLVMBuildPointerCast(builder, mod_name,
                           LLVMPointerType(LLVMInt8Type(), 0), "")
   };
   LLVMBuildCall(builder, llvm_fn("_assert_fail"),
                 args, ARRAY_LEN(args), "");
}

static void cgen_op_assert(int op, cgen_ctx_t *ctx)
{
   LLVMValueRef test = ctx->regs[vcode_get_arg(op, 0)];

   LLVMBasicBlockRef thenbb = LLVMAppendBasicBlock(ctx->fn, "assert_fail");
   LLVMBasicBlockRef elsebb = LLVMAppendBasicBlock(ctx->fn, "assert_pass");

   LLVMBuildCondBr(builder, test, elsebb, thenbb);

   LLVMPositionBuilderAtEnd(builder, thenbb);

   vcode_reg_t mreg = vcode_get_arg(op, 2);

   LLVMValueRef message, message_len;
   if (mreg == VCODE_INVALID_REG) {
      const char def_str[] = "Assertion violation.";
      const size_t def_len = sizeof(def_str) - 1;

      LLVMValueRef global = LLVMGetNamedGlobal(module, "default_message");
      if (global == NULL) {
         LLVMValueRef init = LLVMConstString(def_str, def_len, true);

         global = LLVMAddGlobal(module, LLVMTypeOf(init), "default_message");
         LLVMSetInitializer(global, init);
         LLVMSetLinkage(global, LLVMInternalLinkage);
         LLVMSetGlobalConstant(global, true);
      }

      message_len = llvm_int32(def_len);

      LLVMValueRef index[] = { llvm_int32(0), llvm_int32(0) };
      message = LLVMBuildGEP(builder, global, index, ARRAY_LEN(index), "");
   }
   else {
      message     = cgen_array_data_ptr(mreg, ctx);
      message_len = cgen_array_len(mreg, ctx);
   }

   LLVMValueRef severity = ctx->regs[vcode_get_arg(op, 1)];

   LLVMValueRef args[] = {
      message,
      message_len,
      severity,
      llvm_int32(vcode_get_index(op)),
      LLVMBuildPointerCast(builder, mod_name,
                           LLVMPointerType(LLVMInt8Type(), 0), "")
   };
   LLVMBuildCall(builder, llvm_fn("_assert_fail"),
                 args, ARRAY_LEN(args), "");

   LLVMBuildBr(builder, elsebb);
   LLVMPositionBuilderAtEnd(builder, elsebb);
}

static void cgen_op_wait(int op, cgen_ctx_t *ctx)
{
   vcode_reg_t after = vcode_get_arg(op, 0);
   if (after != VCODE_INVALID_REG)
      cgen_sched_process(ctx->regs[after]);

   assert(ctx->state != NULL);
   LLVMValueRef state_ptr = LLVMBuildStructGEP(builder, ctx->state, 0, "");
   LLVMBuildStore(builder, llvm_int32(vcode_get_target(op, 0)), state_ptr);

   LLVMBuildRetVoid(builder);
}

static void cgen_op_store(int op, cgen_ctx_t *ctx)
{
   vcode_var_t var = vcode_get_address(op);
   LLVMBuildStore(builder, cgen_get_arg(op, 0, ctx), cgen_get_var(var, ctx));
}

static void cgen_op_store_indirect(int op, cgen_ctx_t *ctx)
{
   LLVMBuildStore(builder, cgen_get_arg(op, 0, ctx), cgen_get_arg(op, 1, ctx));
}

static void cgen_op_load(int op, cgen_ctx_t *ctx)
{
   vcode_reg_t result = vcode_get_result(op);
   vcode_var_t var = vcode_get_address(op);
   ctx->regs[result] = LLVMBuildLoad(builder, cgen_get_var(var, ctx),
                                     cgen_reg_name(result));
}

static void cgen_op_load_indirect(int op, cgen_ctx_t *ctx)
{
   vcode_reg_t result = vcode_get_result(op);
   LLVMValueRef ptr = cgen_get_arg(op, 0, ctx);
   ctx->regs[result] = LLVMBuildLoad(builder, ptr, cgen_reg_name(result));
}

static void cgen_op_add(int op, cgen_ctx_t *ctx)
{
   vcode_reg_t result = vcode_get_result(op);
   if (vtype_kind(vcode_reg_type(result)) == VCODE_TYPE_POINTER) {
      LLVMValueRef index[] = { cgen_get_arg(op, 1, ctx) };
      ctx->regs[result] = LLVMBuildGEP(builder, cgen_get_arg(op, 0, ctx),
                                       index, ARRAY_LEN(index),
                                       cgen_reg_name(result));
   }
   else
      ctx->regs[result] = LLVMBuildAdd(builder,
                                       cgen_get_arg(op, 0, ctx),
                                       cgen_get_arg(op, 1, ctx),
                                       cgen_reg_name(result));
}

static void cgen_op_sub(int op, cgen_ctx_t *ctx)
{
   vcode_reg_t result = vcode_get_result(op);
   ctx->regs[result] = LLVMBuildSub(builder,
                                    cgen_get_arg(op, 0, ctx),
                                    cgen_get_arg(op, 1, ctx),
                                    cgen_reg_name(result));
}

static void cgen_op_or(int op, cgen_ctx_t *ctx)
{
   vcode_reg_t result = vcode_get_result(op);
   ctx->regs[result] = LLVMBuildOr(builder,
                                   cgen_get_arg(op, 0, ctx),
                                   cgen_get_arg(op, 1, ctx),
                                   cgen_reg_name(result));
}

static void cgen_op_and(int op, cgen_ctx_t *ctx)
{
   vcode_reg_t result = vcode_get_result(op);
   ctx->regs[result] = LLVMBuildAnd(builder,
                                    cgen_get_arg(op, 0, ctx),
                                    cgen_get_arg(op, 1, ctx),
                                    cgen_reg_name(result));
}

static void cgen_op_not(int op, cgen_ctx_t *ctx)
{
   vcode_reg_t result = vcode_get_result(op);
   ctx->regs[result] = LLVMBuildNot(builder,
                                    cgen_get_arg(op, 0, ctx),
                                    cgen_reg_name(result));
}

static void cgen_op_mul(int op, cgen_ctx_t *ctx)
{
   vcode_reg_t result = vcode_get_result(op);
   ctx->regs[result] = LLVMBuildMul(builder,
                                    cgen_get_arg(op, 0, ctx),
                                    cgen_get_arg(op, 1, ctx),
                                    cgen_reg_name(result));
}

static void cgen_op_phi(int op, cgen_ctx_t *ctx)
{
   vcode_reg_t result = vcode_get_result(op);
   ctx->regs[result] = LLVMBuildPhi(builder, cgen_type(vcode_reg_type(result)),
                                    cgen_reg_name(result));

   const int nargs = vcode_count_args(op);
   LLVMValueRef values[nargs];
   LLVMBasicBlockRef bbs[nargs];
   for (int i = 0; i < nargs; i++) {
      values[i] = cgen_get_arg(op, i, ctx);
      bbs[i] = ctx->blocks[vcode_get_target(op, i)];
   }

   LLVMAddIncoming(ctx->regs[result], values, bbs, nargs);
}

static void cgen_op_bounds(int op, cgen_ctx_t *ctx)
{
   // TODO
}

static void cgen_op_image(int op, cgen_ctx_t *ctx)
{
   vcode_reg_t arg = vcode_get_arg(op, 0);

   const bool is_signed = vtype_kind(vcode_reg_type(arg)) == VCODE_TYPE_INT;
   const bool real = false;
   LLVMOpcode cop = real ? LLVMBitCast : (is_signed ? LLVMSExt : LLVMZExt);
   LLVMValueRef res = LLVMBuildAlloca(builder,
                                      llvm_uarray_type(LLVMInt8Type(), 1),
                                      "image");
   LLVMValueRef iargs[] = {
      LLVMBuildCast(builder, cop, ctx->regs[arg], LLVMInt64Type(), ""),
      llvm_int32(vcode_get_index(op)),
      LLVMBuildPointerCast(builder, mod_name,
                           LLVMPointerType(LLVMInt8Type(), 0), ""),
      res
   };
   LLVMBuildCall(builder, llvm_fn("_image"), iargs, ARRAY_LEN(iargs), "");

   vcode_reg_t result = vcode_get_result(op);
   ctx->regs[result] = LLVMBuildLoad(builder, res, cgen_reg_name(result));
}

static void cgen_op_cast(int op, cgen_ctx_t *ctx)
{
   vcode_reg_t arg    = vcode_get_arg(op, 0);
   vcode_reg_t result = vcode_get_result(op);

   vcode_type_t arg_type = vcode_reg_type(arg);
   if (vtype_kind(arg_type) == VCODE_TYPE_CARRAY) {
      // This is a no-op as constrained arrays are implemented as pointers
      ctx->regs[result] = ctx->regs[arg];
   }
   else {
      LLVMOpcode lop = LLVMBitCast;
      if (vtype_kind(arg_type) == VCODE_TYPE_INT)
         lop = (vtype_low(arg_type) < 0) ? LLVMSExt : LLVMZExt;

      ctx->regs[result] = LLVMBuildCast(builder, lop, ctx->regs[arg],
                                        cgen_type(arg_type),
                                        cgen_reg_name(result));
   }
}

static void cgen_op_alloca(int op, cgen_ctx_t *ctx)
{
   vcode_reg_t result = vcode_get_result(op);
   LLVMTypeRef type   = cgen_type(vcode_get_type(op));

   if (vcode_count_args(op) == 0)
      ctx->regs[result] = LLVMBuildAlloca(builder, type, cgen_reg_name(result));
   else {
      vcode_reg_t count  = vcode_get_arg(op, 0);
      ctx->regs[result] = cgen_tmp_alloc(ctx->regs[count], type);
   }
}

static void cgen_op_cond(int op, cgen_ctx_t *ctx)
{
   LLVMValueRef test = cgen_get_arg(op, 0, ctx);
   LLVMBuildCondBr(builder, test,
                   ctx->blocks[vcode_get_target(op, 0)],
                   ctx->blocks[vcode_get_target(op, 1)]);
}

static void cgen_op_wrap(int op, cgen_ctx_t *ctx)
{
   vcode_reg_t result = vcode_get_result(op);

   const int dims = vcode_count_args(op) / 3;
   LLVMTypeRef uarray_type = cgen_type(vcode_reg_type(result));

   LLVMTypeRef field_types[LLVMCountStructElementTypes(uarray_type)];
   LLVMGetStructElementTypes(uarray_type, field_types);

   LLVMTypeRef dim_struct = LLVMGetElementType(field_types[1]);

   LLVMValueRef dim_array = LLVMGetUndef(field_types[1]);

   for (int i = 0; i < dims; i++) {
      LLVMValueRef left  = cgen_get_arg(op, (i * 3) + 1, ctx);
      LLVMValueRef right = cgen_get_arg(op, (i * 3) + 2, ctx);
      LLVMValueRef dir   = cgen_get_arg(op, (i * 3) + 3, ctx);

      LLVMValueRef d = LLVMGetUndef(dim_struct);
      d = LLVMBuildInsertValue(builder, d, left, 0, "");
      d = LLVMBuildInsertValue(builder, d, right, 1, "");
      d = LLVMBuildInsertValue(builder, d, dir, 2, "");

      dim_array = LLVMBuildInsertValue(builder, dim_array, d, i, "");
   }

   LLVMValueRef var = LLVMGetUndef(uarray_type);
   var = LLVMBuildInsertValue(builder, var, cgen_get_arg(op, 0, ctx), 0, "");

   ctx->regs[result] = LLVMBuildInsertValue(builder, var, dim_array, 1,
                                            cgen_reg_name(result));
}

static void cgen_op_unwrap(int op, cgen_ctx_t *ctx)
{
   vcode_reg_t result = vcode_get_result(op);
   ctx->regs[result] = LLVMBuildExtractValue(builder, cgen_get_arg(op, 0, ctx),
                                             0, cgen_reg_name(result));
}

static void cgen_op_index(int op, cgen_ctx_t *ctx)
{
   vcode_reg_t result = vcode_get_result(op);
   vcode_var_t var = vcode_get_address(op);

   LLVMValueRef index[] = {
      llvm_int32(0),
      cgen_get_arg(op, 0, ctx)
   };
   ctx->regs[result] = LLVMBuildGEP(builder, cgen_get_var(var, ctx), index,
                                    ARRAY_LEN(index), cgen_reg_name(result));
}

static void cgen_op(int i, cgen_ctx_t *ctx)
{
   const vcode_op_t op = vcode_get_op(i);
   switch (op) {
   case VCODE_OP_RETURN:
      cgen_op_return(i, ctx);
      break;
   case VCODE_OP_JUMP:
      cgen_op_jump(i, ctx);
      break;
   case VCODE_OP_FCALL:
      cgen_op_fcall(i, ctx);
      break;
   case VCODE_OP_CONST:
      cgen_op_const(i, ctx);
      break;
   case VCODE_OP_CMP:
      cgen_op_cmp(i, ctx);
      break;
   case VCODE_OP_ASSERT:
      cgen_op_assert(i, ctx);
      break;
   case VCODE_OP_REPORT:
      cgen_op_report(i, ctx);
      break;
   case VCODE_OP_WAIT:
      cgen_op_wait(i, ctx);
      break;
   case VCODE_OP_COMMENT:
      break;
   case VCODE_OP_CONST_ARRAY:
      cgen_op_const_array(i, ctx);
      break;
   case VCODE_OP_STORE:
      cgen_op_store(i, ctx);
      break;
   case VCODE_OP_LOAD:
      cgen_op_load(i, ctx);
      break;
   case VCODE_OP_ADD:
      cgen_op_add(i, ctx);
      break;
   case VCODE_OP_SUB:
      cgen_op_sub(i, ctx);
      break;
   case VCODE_OP_OR:
      cgen_op_or(i, ctx);
      break;
   case VCODE_OP_AND:
      cgen_op_and(i, ctx);
      break;
   case VCODE_OP_NOT:
      cgen_op_not(i, ctx);
      break;
   case VCODE_OP_MUL:
      cgen_op_mul(i, ctx);
      break;
   case VCODE_OP_BOUNDS:
      cgen_op_bounds(i, ctx);
      break;
   case VCODE_OP_IMAGE:
      cgen_op_image(i, ctx);
      break;
   case VCODE_OP_CAST:
      cgen_op_cast(i, ctx);
      break;
   case VCODE_OP_ALLOCA:
      cgen_op_alloca(i, ctx);
      break;
   case VCODE_OP_STORE_INDIRECT:
      cgen_op_store_indirect(i, ctx);
      break;
   case VCODE_OP_LOAD_INDIRECT:
      cgen_op_load_indirect(i, ctx);
      break;
   case VCODE_OP_COND:
      cgen_op_cond(i, ctx);
      break;
   case VCODE_OP_WRAP:
      cgen_op_wrap(i, ctx);
      break;
   case VCODE_OP_UNWRAP:
      cgen_op_unwrap(i, ctx);
      break;
   case VCODE_OP_INDEX:
      cgen_op_index(i, ctx);
      break;
   case VCODE_OP_PHI:
      cgen_op_phi(i, ctx);
      break;
   default:
      fatal("cannot generate code for vcode op %s", vcode_op_string(op));
   }
}

static void cgen_block(int block, cgen_ctx_t *ctx)
{
   vcode_select_block(block);

   LLVMPositionBuilderAtEnd(builder, ctx->blocks[block]);

   const int nops = vcode_count_ops();
   for (int i = 0; i < nops; i++)
      cgen_op(i, ctx);
}

static void cgen_alloc_context(cgen_ctx_t *ctx)
{
   assert(ctx->regs == NULL);
   assert(ctx->blocks == NULL);

   const int nregs   = vcode_count_regs();
   const int nblocks = vcode_count_blocks();

   ctx->regs   = xcalloc(nregs * sizeof(LLVMValueRef));
   ctx->blocks = xcalloc(nblocks * sizeof(LLVMBasicBlockRef));

   if (ctx->state == NULL)
      ctx->locals = xcalloc(vcode_count_vars() * sizeof(LLVMValueRef));

   for (int i = 0; i < nblocks; i++) {
      char *name = xasprintf("vcode_block_%d", i);
      ctx->blocks[i] = LLVMAppendBasicBlock(ctx->fn, name);
      free(name);
   }
}

static void cgen_free_context(cgen_ctx_t *ctx)
{
   free(ctx->regs);
   free(ctx->blocks);
   free(ctx->locals);
}

static void cgen_code(cgen_ctx_t *ctx)
{
   const int nblocks = vcode_count_blocks();
   for (int i = 0; i < nblocks; i++)
      cgen_block(i, ctx);
}

static void cgen_params(cgen_ctx_t *ctx)
{
   const int nparams = vcode_count_params();
   for (int i = 0; i < nparams; i++)
      ctx->regs[vcode_param_reg(i)] = LLVMGetParam(ctx->fn, i);
}

static void cgen_locals(cgen_ctx_t *ctx)
{
   LLVMPositionBuilderAtEnd(builder, ctx->blocks[0]);

   const int nvars = vcode_count_vars();
   for (int i = 0; i < nvars; i++)
      ctx->locals[i] = LLVMBuildAlloca(builder, cgen_type(vcode_var_type(i)),
                                       istr(vcode_var_name(i)));
}

static LLVMTypeRef cgen_subprogram_type(void)
{
   const int nparams = vcode_count_params();
   LLVMTypeRef params[nparams];
   for (int i = 0; i < nparams; i++)
      params[i] = cgen_type(vcode_param_type(i));

   return LLVMFunctionType(cgen_type(vcode_unit_result()),
                           params, nparams, false);
}

static void cgen_function(vcode_unit_t code)
{
   vcode_select_unit(code);
   assert(vcode_unit_kind() == VCODE_UNIT_FUNCTION);
   assert(LLVMGetNamedFunction(module, istr(vcode_unit_name())) == NULL);

   LLVMValueRef fn = LLVMAddFunction(module, istr(vcode_unit_name()),
                                     cgen_subprogram_type());

   LLVMAddFunctionAttr(fn, LLVMNoUnwindAttribute);
   LLVMAddFunctionAttr(fn, LLVMReadOnlyAttribute);

   cgen_ctx_t ctx = {
      .fn = fn
   };
   cgen_alloc_context(&ctx);

   cgen_params(&ctx);
   cgen_locals(&ctx);
   cgen_code(&ctx);
   cgen_free_context(&ctx);
}

static void cgen_state_struct(cgen_ctx_t *ctx)
{
   ctx->var_base = 1;

   const int nvars   = vcode_count_vars();
   const int nfields = nvars + ctx->var_base;

   LLVMTypeRef fields[nfields];
   fields[0] = LLVMInt32Type();

   for (int i = 0; i < nvars; i++)
      fields[ctx->var_base + i] = cgen_type(vcode_var_type(i));

   char *name LOCAL = xasprintf("%s__state", istr(vcode_unit_name()));
   LLVMTypeRef state_ty = LLVMStructType(fields, nfields, false);
   ctx->state = LLVMAddGlobal(module, state_ty, name);
   LLVMSetLinkage(ctx->state, LLVMInternalLinkage);
   LLVMSetInitializer(ctx->state, LLVMGetUndef(state_ty));
}

static void cgen_jump_table(cgen_ctx_t *ctx)
{
   assert(ctx->state != NULL);

   LLVMValueRef state_ptr = LLVMBuildStructGEP(builder, ctx->state, 0, "");
   LLVMValueRef jtarget = LLVMBuildLoad(builder, state_ptr, "");
   LLVMValueRef jswitch = LLVMBuildSwitch(builder, jtarget, ctx->blocks[1], 10);

   const int nblocks = vcode_count_blocks();
   bool *have LOCAL = xcalloc(sizeof(bool) * nblocks);
   for (int i = 0; i < nblocks; i++) {
      vcode_select_block(i);

      const int last = vcode_count_ops() - 1;
      if (vcode_get_op(last) != VCODE_OP_WAIT)
         continue;

      const vcode_block_t target = vcode_get_target(last, 0);
      if (have[target])
         continue;

      LLVMAddCase(jswitch, llvm_int32(target), ctx->blocks[target]);
      have[target] = true;
   }
}

static void cgen_process(vcode_unit_t code)
{
   vcode_select_unit(code);
   assert(vcode_unit_kind() == VCODE_UNIT_PROCESS);

   LLVMTypeRef pargs[] = { LLVMInt32Type() };
   LLVMTypeRef ftype = LLVMFunctionType(LLVMVoidType(), pargs, 1, false);
   LLVMValueRef fn = LLVMAddFunction(module, istr(vcode_unit_name()), ftype);

   LLVMBasicBlockRef entry_bb = LLVMAppendBasicBlock(fn, "entry");
   LLVMBasicBlockRef reset_bb = LLVMAppendBasicBlock(fn, "reset");
   LLVMBasicBlockRef jump_bb  = LLVMAppendBasicBlock(fn, "jump_table");

   cgen_ctx_t ctx = {
      .fn = fn
   };
   cgen_state_struct(&ctx);
   cgen_alloc_context(&ctx);

   // If the parameter is non-zero jump to the init block

   LLVMPositionBuilderAtEnd(builder, entry_bb);
   LLVMValueRef reset = LLVMBuildICmp(builder, LLVMIntNE, LLVMGetParam(fn, 0),
                                      llvm_int32(0), "reset");
   LLVMBuildCondBr(builder, reset, reset_bb, jump_bb);

   LLVMPositionBuilderAtEnd(builder, reset_bb);

   LLVMValueRef state_ptr = LLVMBuildStructGEP(builder, ctx.state, 0, "");
   LLVMBuildStore(builder, llvm_int32(1), state_ptr);

   // Schedule the process to run immediately
   cgen_sched_process(llvm_int64(0));

   LLVMBuildBr(builder, ctx.blocks[0]);

   LLVMPositionBuilderAtEnd(builder, jump_bb);
   cgen_jump_table(&ctx);

   cgen_code(&ctx);
   cgen_free_context(&ctx);
}

static void cgen_reset_function(vcode_unit_t vcode)
{
   vcode_select_unit(vcode);

   char *name LOCAL = xasprintf("%s_reset", istr(vcode_unit_name()));
   LLVMValueRef fn =
      LLVMAddFunction(module, name,
                      LLVMFunctionType(LLVMVoidType(), NULL, 0, false));

   cgen_ctx_t ctx = {
      .fn = fn
   };
   cgen_alloc_context(&ctx);
   cgen_code(&ctx);
   cgen_free_context(&ctx);
}

static void cgen_coverage_state(tree_t t)
{
   const int stmt_tags = tree_attr_int(t, ident_new("stmt_tags"), 0);
   if (stmt_tags > 0) {
      LLVMTypeRef type = LLVMArrayType(LLVMInt32Type(), stmt_tags);
      LLVMValueRef var = LLVMAddGlobal(module, type, "cover_stmts");
      LLVMSetInitializer(var, LLVMGetUndef(type));
   }

   const int cond_tags = tree_attr_int(t, ident_new("cond_tags"), 0);
   if (cond_tags > 0) {
      LLVMTypeRef type = LLVMArrayType(LLVMInt32Type(), stmt_tags);
      LLVMValueRef var = LLVMAddGlobal(module, type, "cover_conds");
      LLVMSetInitializer(var, LLVMGetUndef(type));
   }
}

static void cgen_top(tree_t t)
{
   vcode_unit_t vcode = tree_code(t);

   cgen_coverage_state(t);

   cgen_reset_function(vcode);

   const int ndecls = tree_decls(t);
   for (int i = 0; i < ndecls; i++) {
      tree_t d = tree_decl(t, i);
      switch (tree_kind(d)) {
      case T_FUNC_BODY:
         cgen_function(tree_code(d));
         break;
      default:
         break;
      }
   }

   if (tree_kind(t) == T_ELAB) {
      const int nstmts = tree_stmts(t);
      for (int i = 0; i < nstmts; i++)
         cgen_process(tree_code(tree_stmt(t, i)));
   }
}

static void cgen_optimise(void)
{
   LLVMPassManagerRef pass_mgr = LLVMCreatePassManager();

   LLVMAddPromoteMemoryToRegisterPass(pass_mgr);
   LLVMAddInstructionCombiningPass(pass_mgr);
   LLVMAddReassociatePass(pass_mgr);
   LLVMAddGVNPass(pass_mgr);
   LLVMAddCFGSimplificationPass(pass_mgr);

   LLVMRunPassManager(pass_mgr, module);
   LLVMDisposePassManager(pass_mgr);
}

static const char *cgen_memcpy_name(int width)
{
   static char name[64];
   checked_sprintf(name, sizeof(name),
                   "llvm.memcpy.p0i%d.p0i%d.i32", width, width);
   return name;
}

static LLVMValueRef cgen_support_fn(const char *name)
{
   LLVMValueRef fn = NULL;
   if (strcmp(name, "_sched_process") == 0) {
      LLVMTypeRef args[] = { LLVMInt64Type() };
      fn = LLVMAddFunction(module, "_sched_process",
                           LLVMFunctionType(LLVMVoidType(),
                                            args, ARRAY_LEN(args), false));
   }
   else if (strcmp(name, "_sched_waveform") == 0) {
      LLVMTypeRef args[] = {
         llvm_void_ptr(),
         llvm_void_ptr(),
         LLVMInt32Type(),
         LLVMInt64Type(),
         LLVMInt64Type()
      };
      fn = LLVMAddFunction(module, "_sched_waveform",
                           LLVMFunctionType(LLVMVoidType(),
                                            args, ARRAY_LEN(args), false));
   }
   else if (strcmp(name, "_sched_event") == 0) {
      LLVMTypeRef args[] = {
         llvm_void_ptr(),
         LLVMInt32Type(),
         LLVMInt32Type()
      };
      fn = LLVMAddFunction(module, "_sched_event",
                           LLVMFunctionType(LLVMVoidType(),
                                            args, ARRAY_LEN(args), false));
   }
   else if (strcmp(name, "_set_initial") == 0) {
      LLVMTypeRef args[] = {
         LLVMInt32Type(),
         llvm_void_ptr(),
         LLVMPointerType(LLVMInt32Type(), 0),
         LLVMInt32Type(),
         llvm_void_ptr(),
         LLVMInt32Type(),
         LLVMPointerType(LLVMInt8Type(), 0)
      };
      fn = LLVMAddFunction(module, "_set_initial",
                           LLVMFunctionType(LLVMVoidType(),
                                            args, ARRAY_LEN(args), false));
   }
   else if (strcmp(name, "_needs_last_value") == 0) {
      LLVMTypeRef args[] = {
         LLVMPointerType(LLVMInt32Type(), 0),
         LLVMInt32Type()
      };
      fn = LLVMAddFunction(module, "_needs_last_value",
                           LLVMFunctionType(LLVMVoidType(),
                                            args, ARRAY_LEN(args), false));
   }
   else if (strcmp(name, "_resolved_address") == 0) {
      LLVMTypeRef args[] = {
         LLVMInt32Type()
      };
      fn = LLVMAddFunction(module, "_resolved_address",
                           LLVMFunctionType(llvm_void_ptr(),
                                            args, ARRAY_LEN(args), false));
   }
   else if (strcmp(name, "_alloc_driver") == 0) {
      LLVMTypeRef args[] = {
         LLVMPointerType(LLVMInt32Type(), 0),
         LLVMInt32Type(),
         LLVMPointerType(LLVMInt32Type(), 0),
         LLVMInt32Type(),
         llvm_void_ptr()
      };
      fn = LLVMAddFunction(module, "_alloc_driver",
                           LLVMFunctionType(LLVMVoidType(),
                                            args, ARRAY_LEN(args), false));
   }
   else if (strcmp(name, "_assert_fail") == 0) {
      LLVMTypeRef args[] = {
         LLVMPointerType(LLVMInt8Type(), 0),
         LLVMInt32Type(),
         LLVMInt8Type(),
         LLVMInt32Type(),
         LLVMPointerType(LLVMInt8Type(), 0)
      };
      fn = LLVMAddFunction(module, "_assert_fail",
                           LLVMFunctionType(LLVMVoidType(),
                                            args, ARRAY_LEN(args), false));
   }
   else if (strcmp(name, "_vec_load") == 0) {
      LLVMTypeRef args[] = {
         llvm_void_ptr(),
         llvm_void_ptr(),
         LLVMInt32Type(),
         LLVMInt32Type(),
         LLVMInt1Type()
      };
      fn = LLVMAddFunction(module, "_vec_load",
                           LLVMFunctionType(llvm_void_ptr(),
                                            args, ARRAY_LEN(args), false));
   }
   else if (strcmp(name, "_image") == 0) {
      LLVMTypeRef args[] = {
         LLVMInt64Type(),
         LLVMInt32Type(),
         LLVMPointerType(LLVMInt8Type(), 0),
         LLVMPointerType(llvm_uarray_type(LLVMInt8Type(), 1), 0)
      };
      fn = LLVMAddFunction(module, "_image",
                           LLVMFunctionType(LLVMVoidType(),
                                            args, ARRAY_LEN(args), false));
   }
   else if (strcmp(name, "_debug_out") == 0) {
      LLVMTypeRef args[] = {
         LLVMInt32Type()
      };
      fn = LLVMAddFunction(module, "_debug_out",
                           LLVMFunctionType(LLVMVoidType(),
                                            args, ARRAY_LEN(args), false));
   }
   else if (strcmp(name, "_debug_dump") == 0) {
      LLVMTypeRef args[] = {
         llvm_void_ptr(),
         LLVMInt32Type()
      };
      fn = LLVMAddFunction(module, "_debug_dump",
                           LLVMFunctionType(LLVMVoidType(),
                                            args, ARRAY_LEN(args), false));
   }
   else if (strcmp(name, "llvm.pow.f64") == 0) {
      LLVMTypeRef args[] = {
         LLVMDoubleType(),
         LLVMDoubleType()
      };
      fn = LLVMAddFunction(module, "llvm.pow.f64",
                           LLVMFunctionType(LLVMDoubleType(),
                                            args, ARRAY_LEN(args), false));
   }
   else if (strcmp(name, "llvm.memset.p0i8.i32") == 0) {
      LLVMTypeRef args[] = {
         LLVMPointerType(LLVMInt8Type(), 0),
         LLVMInt8Type(),
         LLVMInt32Type(),
         LLVMInt32Type(),
         LLVMInt1Type()
      };
      fn = LLVMAddFunction(module, "llvm.memset.p0i8.i32",
                           LLVMFunctionType(LLVMVoidType(),
                                            args, ARRAY_LEN(args), false));
   }
   else if (strcmp(name, "llvm.expect.i1") == 0) {
      LLVMTypeRef args[] = {
         LLVMInt1Type(),
         LLVMInt1Type()
      };
      fn = LLVMAddFunction(module, "llvm.expect.i1",
                           LLVMFunctionType(LLVMInt1Type(),
                                            args, ARRAY_LEN(args), false));
   }
   else if (strncmp(name, "llvm.memcpy", 11) == 0) {
      int width;
      if (sscanf(name, "llvm.memcpy.p0i%d", &width) != 1)
         fatal("invalid memcpy intrinsic %s", name);

      LLVMTypeRef args[] = {
         LLVMPointerType(LLVMIntType(width), 0),
         LLVMPointerType(LLVMIntType(width), 0),
         LLVMInt32Type(),
         LLVMInt32Type(),
         LLVMInt1Type()
      };
      fn = LLVMAddFunction(module, cgen_memcpy_name(width),
                           LLVMFunctionType(LLVMVoidType(),
                                            args, ARRAY_LEN(args), false));
   }
   else if (strcmp(name, "_file_open") == 0) {
      LLVMTypeRef args[] = {
         LLVMPointerType(LLVMInt8Type(), 0),
         LLVMPointerType(llvm_void_ptr(), 0),
         LLVMPointerType(LLVMInt8Type(), 0),
         LLVMInt32Type(),
         LLVMInt8Type()
      };
      fn = LLVMAddFunction(module, "_file_open",
                           LLVMFunctionType(LLVMVoidType(),
                                            args, ARRAY_LEN(args), false));
   }
   else if (strcmp(name, "_file_write") == 0) {
      LLVMTypeRef args[] = {
         LLVMPointerType(llvm_void_ptr(), 0),
         llvm_void_ptr(),
         LLVMInt32Type()
      };
      fn = LLVMAddFunction(module, "_file_write",
                           LLVMFunctionType(LLVMVoidType(),
                                            args, ARRAY_LEN(args), false));
   }
   else if (strcmp(name, "_file_read") == 0) {
      LLVMTypeRef args[] = {
         LLVMPointerType(llvm_void_ptr(), 0),
         llvm_void_ptr(),
         LLVMInt32Type(),
         LLVMPointerType(LLVMInt32Type(), 0)
      };
      fn = LLVMAddFunction(module, "_file_read",
                           LLVMFunctionType(LLVMVoidType(),
                                            args, ARRAY_LEN(args), false));
   }
   else if (strcmp(name, "_file_close") == 0) {
      LLVMTypeRef args[] = {
         LLVMPointerType(llvm_void_ptr(), 0)
      };
      fn = LLVMAddFunction(module, "_file_close",
                           LLVMFunctionType(LLVMVoidType(),
                                            args, ARRAY_LEN(args), false));
   }
   else if (strcmp(name, "_endfile") == 0) {
      LLVMTypeRef args[] = {
         llvm_void_ptr()
      };
      fn = LLVMAddFunction(module, "_endfile",
                           LLVMFunctionType(LLVMInt1Type(),
                                            args, ARRAY_LEN(args), false));
   }
   else if (strcmp(name, "_bounds_fail") == 0) {
      LLVMTypeRef args[] = {
         LLVMInt32Type(),
         LLVMPointerType(LLVMInt8Type(), 0),
         LLVMInt32Type(),
         LLVMInt32Type(),
         LLVMInt32Type(),
         LLVMInt32Type(),
         LLVMInt32Type()
      };
      fn = LLVMAddFunction(module, "_bounds_fail",
                           LLVMFunctionType(LLVMVoidType(),
                                            args, ARRAY_LEN(args), false));
      LLVMAddFunctionAttr(fn, LLVMNoReturnAttribute);
   }
   else if (strcmp(name, "_div_zero") == 0) {
      LLVMTypeRef args[] = {
         LLVMInt32Type(),
         LLVMPointerType(LLVMInt8Type(), 0)
      };
      fn = LLVMAddFunction(module, "_div_zero",
                           LLVMFunctionType(LLVMVoidType(),
                                            args, ARRAY_LEN(args), false));
      LLVMAddFunctionAttr(fn, LLVMNoReturnAttribute);
   }
   else if (strcmp(name, "_null_deref") == 0) {
      LLVMTypeRef args[] = {
         LLVMInt32Type(),
         LLVMPointerType(LLVMInt8Type(), 0)
      };
      fn = LLVMAddFunction(module, "_null_deref",
                           LLVMFunctionType(LLVMVoidType(),
                                            args, ARRAY_LEN(args), false));
      LLVMAddFunctionAttr(fn, LLVMNoReturnAttribute);
   }
   else if (strcmp(name, "_bit_shift") == 0) {
      LLVMTypeRef args[] = {
         LLVMInt32Type(),
         LLVMPointerType(LLVMInt1Type(), 0),
         LLVMInt32Type(),
         LLVMInt8Type(),
         LLVMInt32Type(),
         LLVMPointerType(llvm_uarray_type(LLVMInt1Type(), 1), 0)
      };
      fn = LLVMAddFunction(module, "_bit_shift",
                           LLVMFunctionType(LLVMVoidType(),
                                            args, ARRAY_LEN(args), false));
   }
   else if (strcmp(name, "_bit_vec_op") == 0) {
      LLVMTypeRef args[] = {
         LLVMInt32Type(),
         LLVMPointerType(LLVMInt1Type(), 0),
         LLVMInt32Type(),
         LLVMInt8Type(),
         LLVMPointerType(LLVMInt1Type(), 0),
         LLVMInt32Type(),
         LLVMInt8Type(),
         LLVMPointerType(llvm_uarray_type(LLVMInt1Type(), 1), 0)
      };
      fn = LLVMAddFunction(module, "_bit_vec_op",
                           LLVMFunctionType(LLVMVoidType(),
                                            args, ARRAY_LEN(args), false));
   }
   else if (strcmp(name, "_test_net_flag") == 0) {
      LLVMTypeRef args[] = {
         llvm_void_ptr(),
         LLVMInt32Type(),
         LLVMInt32Type()
      };
      fn = LLVMAddFunction(module, "_test_net_flag",
                           LLVMFunctionType(LLVMInt1Type(),
                                            args, ARRAY_LEN(args), false));
   }
   else if (strcmp(name, "_last_event") == 0) {
      LLVMTypeRef args[] = {
         llvm_void_ptr(),
         LLVMInt32Type()
      };
      fn = LLVMAddFunction(module, "_last_event",
                           LLVMFunctionType(LLVMInt64Type(),
                                            args, ARRAY_LEN(args), false));
   }
   else if (strcmp(name, "_value_attr") == 0) {
      LLVMTypeRef args[] = {
         llvm_void_ptr(),
         LLVMInt32Type(),
         LLVMInt32Type(),
         LLVMPointerType(LLVMInt8Type(), 0)
      };
      fn = LLVMAddFunction(module, "_value_attr",
                           LLVMFunctionType(LLVMInt64Type(),
                                            args, ARRAY_LEN(args), false));
   }
   else if (strcmp(name, "_std_standard_now") == 0)
      fn = LLVMAddFunction(module, "_std_standard_now",
                           LLVMFunctionType(LLVMInt64Type(), NULL, 0, false));

   if (fn != NULL)
      LLVMAddFunctionAttr(fn, LLVMNoUnwindAttribute);

   return fn;
}

static void cgen_module_name(tree_t top)
{
   const char *name_str = istr(tree_ident(top));

   size_t len = strlen(name_str);
   LLVMValueRef chars[len + 1];
   llvm_str(chars, len + 1, name_str);

   mod_name = LLVMAddGlobal(module,
                            LLVMArrayType(LLVMInt8Type(), len + 1),
                            "module_name");
   LLVMSetInitializer(mod_name,
                      LLVMConstArray(LLVMInt8Type(), chars, len + 1));
   LLVMSetLinkage(mod_name, LLVMPrivateLinkage);
}

static void cgen_tmp_stack(void)
{
   LLVMValueRef _tmp_stack =
      LLVMAddGlobal(module, LLVMPointerType(llvm_void_ptr(), 0), "_tmp_stack");
   LLVMSetLinkage(_tmp_stack, LLVMExternalLinkage);

   LLVMValueRef _tmp_alloc =
      LLVMAddGlobal(module, LLVMInt32Type(), "_tmp_alloc");
   LLVMSetLinkage(_tmp_alloc, LLVMExternalLinkage);
}

void cgen(tree_t top)
{
   tree_kind_t kind = tree_kind(top);
   if (kind != T_ELAB && kind != T_PACK_BODY && kind != T_PACKAGE)
      fatal("cannot generate code for %s", tree_kind_str(kind));

   module = LLVMModuleCreateWithName(istr(tree_ident(top)));
   builder = LLVMCreateBuilder();

   cgen_module_name(top);
   cgen_tmp_stack();

   cgen_top(top);

   if (opt_get_int("dump-llvm"))
      LLVMDumpModule(module);

   if (LLVMVerifyModule(module, LLVMPrintMessageAction, NULL))
      fatal("LLVM verification failed");

   cgen_optimise();

   char *fname = xasprintf("_%s.bc", istr(tree_ident(top)));

   FILE *f = lib_fopen(lib_work(), fname, "w");
   if (LLVMWriteBitcodeToFD(module, fileno(f), 0, 0) != 0)
      fatal("error writing LLVM bitcode");
   fclose(f);
   free(fname);

   LLVMDisposeBuilder(builder);
   LLVMDisposeModule(module);
}
