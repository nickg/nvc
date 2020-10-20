//
//  Copyright (C) 2011-2020  Nick Gasson
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
#include "array.h"
#include "hash.h"
#include "rt/rt.h"
#include "rt/cover.h"

#include <stdlib.h>
#include <string.h>
#include <limits.h>
#include <unistd.h>
#include <sys/stat.h>

#include <llvm-c/Core.h>
#include <llvm-c/BitWriter.h>
#include <llvm-c/ExecutionEngine.h>
#include <llvm-c/Analysis.h>
#include <llvm-c/Transforms/Scalar.h>
#include <llvm-c/Transforms/IPO.h>
#include <llvm-c/Transforms/PassManagerBuilder.h>
#include <llvm-c/TargetMachine.h>

#undef NDEBUG
#include <assert.h>

#define MAX_STATIC_NETS 256

typedef struct {
   LLVMValueRef      *regs;
   LLVMBasicBlockRef *blocks;
   LLVMValueRef       fn;
   LLVMValueRef       state;
   LLVMValueRef       display;
   size_t             var_base;
   size_t             param_base;
   LLVMValueRef      *locals;
   loc_t              last_loc;
} cgen_ctx_t;

typedef struct {
   LLVMValueRef size;
   LLVMValueRef resolution;
   uint32_t     count;
   uint32_t     flags;
   uint32_t     ileft;
} size_list_t;

DECLARE_AND_DEFINE_ARRAY(size_list)

typedef enum {
   FUNC_ATTR_NOUNWIND,
   FUNC_ATTR_NORETURN,
   FUNC_ATTR_READONLY,
   FUNC_ATTR_NOCAPTURE,
   FUNC_ATTR_BYVAL,
   FUNC_ATTR_UWTABLE,

   FUNC_ATTR_DLLEXPORT,   // Should be last
} func_attr_t;

static LLVMModuleRef  module = NULL;
static LLVMBuilderRef builder = NULL;

static char **link_args = NULL;
static size_t n_link_args = 0;
static size_t max_link_args = 0;

static hash_t *string_pool = NULL;

static LLVMValueRef cgen_support_fn(const char *name);
static LLVMValueRef cgen_resolution_wrapper(const vcode_res_elem_t *rdata);

static LLVMValueRef llvm_int1(bool b)
{
   return LLVMConstInt(LLVMInt1Type(), b, false);
}

static LLVMValueRef llvm_int8(int8_t i)
{
   return LLVMConstInt(LLVMInt8Type(), i, false);
}

static LLVMValueRef llvm_int16(int16_t i)
{
   return LLVMConstInt(LLVMInt16Type(), i, false);
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

static LLVMTypeRef llvm_char_ptr(void)
{
   return LLVMPointerType(LLVMInt8Type(), 0);
}

static LLVMTypeRef llvm_rt_loc(void)
{
   LLVMTypeRef fields[] = {
      LLVMInt32Type(),
      LLVMInt32Type(),
      LLVMInt16Type(),
      LLVMInt16Type(),
      llvm_char_ptr()
   };
   return LLVMStructType(fields, ARRAY_LEN(fields), false);
}

static LLVMTypeRef llvm_image_map(void)
{
   LLVMTypeRef field_types[] = {
      LLVMInt32Type(),
      LLVMInt32Type(),
      LLVMPointerType(LLVMInt8Type(), 0),
      LLVMPointerType(LLVMInt64Type(), 0),
      LLVMInt32Type()
   };
   return LLVMStructType(field_types, ARRAY_LEN(field_types), false);
}

static LLVMValueRef llvm_void_cast(LLVMValueRef ptr)
{
   return LLVMBuildPointerCast(builder, ptr, llvm_void_ptr(), "");
}

static LLVMValueRef llvm_sizeof(LLVMTypeRef type)
{
   return LLVMBuildIntCast(builder, LLVMSizeOf(type),
                           LLVMInt32Type(), "");
}

static LLVMValueRef llvm_fn(const char *name)
{
   LLVMValueRef fn = LLVMGetNamedFunction(module, name);
   if ((fn == NULL) && ((fn = cgen_support_fn(name)) == NULL))
      fatal("cannot find named function %s", name);
   return fn;
}

static LLVMTypeRef llvm_uarray_type(LLVMTypeRef base, int dims)
{
   // Unconstrained arrays are represented by a structure
   // containing the left and right indices, a flag indicating
   // direction, and a pointer to the array data

   char *struct_name LOCAL = NULL;
   switch (LLVMGetTypeKind(base)) {
   case LLVMIntegerTypeKind:
      struct_name = xasprintf("uarray.i%d.%d", LLVMGetIntTypeWidth(base), dims);
      break;
   case LLVMDoubleTypeKind:
      struct_name = xasprintf("uarray.f.%d", dims);
      break;
   case LLVMStructTypeKind:
      struct_name = xasprintf("uarray.%s.%d", LLVMGetStructName(base), dims);
      break;
   case LLVMPointerTypeKind:
      struct_name = xasprintf("uarray.signal.%d", dims);
      break;
   default:
      fatal_trace("cannot generate uarray type name for %s",
                  LLVMPrintTypeToString(base));
   }

   LLVMTypeRef exist = LLVMGetTypeByName(module, struct_name);
   if (exist != NULL)
      return exist;

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

   LLVMTypeRef new = LLVMStructCreateNamed(LLVMGetGlobalContext(), struct_name);
   LLVMStructSetBody(new, fields, ARRAY_LEN(fields), false);
   return new;
}

static LLVMTypeRef llvm_size_list_type(void)
{
   LLVMTypeRef struct_elems[] = {
      LLVMInt32Type(),
      LLVMInt32Type(),
      llvm_void_ptr(),
      LLVMInt32Type(),
      LLVMInt32Type()
   };
   return LLVMStructType(struct_elems, ARRAY_LEN(struct_elems), false);
}

#if 0
static void debug_out(LLVMValueRef val)
{
   LLVMValueRef args[] = { val, llvm_int32(-1) };
   LLVMBuildCall(builder, llvm_fn("_debug_out"),
                 args, ARRAY_LEN(args), "");
}
#endif

#if 0
static void debug_dump(LLVMValueRef ptr, LLVMValueRef len)
{
   LLVMValueRef args[] = { llvm_void_cast(ptr), len };
   LLVMBuildCall(builder, llvm_fn("_debug_dump"),
                 args, ARRAY_LEN(args), "");
}
#endif

static void cgen_add_func_attr(LLVMValueRef fn, func_attr_t attr, int param)
{
   if (attr == FUNC_ATTR_DLLEXPORT) {
#ifdef IMPLIB_REQUIRED
      LLVMSetDLLStorageClass(fn, LLVMDLLExportStorageClass);
#endif
      return;
   }

#if LLVM_NEW_ATTRIBUTE_API
   const char *names[] = {
      "nounwind", "noreturn", "readonly", "nocapture", "byval", "uwtable",
   };
   assert(attr < ARRAY_LEN(names));

   const unsigned kind =
      LLVMGetEnumAttributeKindForName(names[attr], strlen(names[attr]));
   if (kind == 0)
      fatal_trace("Cannot get LLVM attribute for %s", names[attr]);

   LLVMAttributeRef ref =
      LLVMCreateEnumAttribute(LLVMGetGlobalContext(), kind, 0);

   LLVMAddAttributeAtIndex(fn, param, ref);
#else
   LLVMAttribute llvm_attrs[] = {
      LLVMNoUnwindAttribute,
      LLVMNoReturnAttribute,
      LLVMReadOnlyAttribute,
      LLVMNoCaptureAttribute,
      LLVMByValAttribute,
      LLVMUWTable,
   };
   assert(attr < ARRAY_LEN(llvm_attrs));

   if (param == -1)
      LLVMAddFunctionAttr(fn, llvm_attrs[attr]);
   else
      LLVMAddAttribute(LLVMGetParam(fn, param - 1), llvm_attrs[attr]);
#endif
}

static LLVMTypeRef cgen_net_id_type(void)
{
   return LLVMInt32Type();
}

static LLVMTypeRef cgen_type(vcode_type_t type)
{
   switch (vtype_kind(type)) {
   case VCODE_TYPE_INT:
      return LLVMIntType(bits_for_range(vtype_low(type), vtype_high(type)));

   case VCODE_TYPE_REAL:
      return LLVMDoubleType();

   case VCODE_TYPE_CARRAY:
      return LLVMArrayType(cgen_type(vtype_elem(type)), vtype_size(type));

   case VCODE_TYPE_UARRAY:
      {
         vcode_type_t elem = vtype_elem(type);
         if (vtype_kind(elem) == VCODE_TYPE_SIGNAL)
            return llvm_uarray_type(cgen_net_id_type(), vtype_dims(type));
         else
            return llvm_uarray_type(cgen_type(elem), vtype_dims(type));
      }

   case VCODE_TYPE_OFFSET:
      return LLVMInt32Type();

   case VCODE_TYPE_POINTER:
   case VCODE_TYPE_ACCESS:
      return LLVMPointerType(cgen_type(vtype_pointed(type)), 0);

   case VCODE_TYPE_RECORD:
      {
         const char *name = istr(vtype_record_name(type));
         LLVMTypeRef lltype = LLVMGetTypeByName(module, name);
         if (lltype != NULL)
            return lltype;

         lltype = LLVMStructCreateNamed(LLVMGetGlobalContext(), name);
         if (lltype == NULL)
            fatal("failed to add record type %s", name);

         const int nfields = vtype_fields(type);
         LLVMTypeRef fields[nfields];

         for (int i = 0; i < nfields; i++)
            fields[i] = cgen_type(vtype_field(type, i));

         LLVMStructSetBody(lltype, fields, nfields, true);
         return lltype;
      }

   case VCODE_TYPE_SIGNAL:
      return LLVMPointerType(cgen_net_id_type(), 0);

   case VCODE_TYPE_FILE:
      return llvm_void_ptr();

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

static const char *cgen_memcpy_name(const char *kind, int width)
{
   static char name[64];
   checked_sprintf(name, sizeof(name),
                   "llvm.%s.p0i%d.p0i%d.i32", kind, width, width);
   return name;
}

static LLVMValueRef cgen_get_arg(int op, int arg, cgen_ctx_t *ctx)
{
   vcode_reg_t r = vcode_get_arg(op, arg);
   if (unlikely(ctx->regs[r] == NULL))
      fatal_trace("definition of register r%d is NULL", r);
   return ctx->regs[r];
}

static LLVMValueRef cgen_display_upref(int hops, cgen_ctx_t *ctx)
{
   assert(ctx->display != NULL);
   assert(LLVMGetTypeKind(LLVMTypeOf(ctx->display)) == LLVMStructTypeKind);

   LLVMValueRef display = ctx->display;
   for (int i = 0; i < hops - 1; i++) {
      const int nelems = LLVMCountStructElementTypes(LLVMTypeOf(display));
      display = LLVMBuildExtractValue(builder, display, nelems - 1, "");
   }

   return display;
}

static LLVMValueRef cgen_get_var(vcode_var_t var, cgen_ctx_t *ctx)
{
   const int my_depth  = vcode_unit_depth();
   const int var_depth = vcode_var_context(var);
   assert(my_depth >= var_depth);

   LLVMValueRef value = NULL;

   if (var_depth == 0) {
      // Shared global variable
      const char *name = safe_symbol(istr(vcode_var_name(var)));
      value = LLVMGetNamedGlobal(module, name);
      if (value == NULL)
         fatal_trace("missing LLVM global for %s", istr(vcode_var_name(var)));
   }
   else if (my_depth == var_depth) {
      // Variable is inside current context
      if (ctx->state != NULL) {
         value = LLVMBuildStructGEP(builder, ctx->state,
                                    ctx->var_base + vcode_var_index(var),
                                    istr(vcode_var_name(var)));
      }
      else {
         assert(ctx->locals != NULL);
         value = ctx->locals[vcode_var_index(var)];
      }
   }
   else {
      // Variable is in a parent context: find it using the display
      assert(my_depth > var_depth);

      LLVMValueRef display = cgen_display_upref(my_depth - var_depth, ctx);
      return LLVMBuildExtractValue(builder, display, vcode_var_index(var),
                                   istr(vcode_var_name(var)));
   }

   if (vtype_kind(vcode_var_type(var)) == VCODE_TYPE_CARRAY) {
      LLVMValueRef index[] = {
         llvm_int32(0),
         llvm_int32(0)
      };
      return LLVMBuildGEP(builder, value, index, ARRAY_LEN(index), "");
   }
   else
      return value;
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

static LLVMTypeRef cgen_display_type(vcode_unit_t unit)
{
   vcode_state_t state;
   vcode_state_save(&state);

   vcode_select_unit(unit);

   const vunit_kind_t kind = vcode_unit_kind();
   if (kind == VCODE_UNIT_CONTEXT) {
      vcode_state_restore(&state);
      return NULL;
   }

   LLVMTypeRef parent = cgen_display_type(vcode_unit_context());
   vcode_select_unit(unit);

   const bool has_params =
      kind == VCODE_UNIT_FUNCTION || kind == VCODE_UNIT_PROCEDURE;
   const int nvars = vcode_count_vars();
   const int nparams = has_params ? vcode_count_params() : 0;
   const int nfields = nvars + nparams + (parent ? 1 : 0);
   LLVMTypeRef fields[nfields];
   LLVMTypeRef *outptr = fields;

   for (int i = 0; i < nvars; i++) {
      vcode_type_t vtype = vcode_var_type(vcode_var_handle(i));
      if (vtype_kind(vtype) == VCODE_TYPE_CARRAY)
         *outptr++ = LLVMPointerType(cgen_type(vtype_elem(vtype)), 0);
      else
         *outptr++ = LLVMPointerType(cgen_type(vtype), 0);
   }

   for (int i = 0; i < nparams; i++)
      *outptr++ = cgen_type(vcode_param_type(i));

   if (parent != NULL)
      *outptr++ = parent;

   vcode_state_restore(&state);

   assert(outptr == fields + nfields);
   return LLVMStructType(fields, nfields, false);
}

static LLVMValueRef cgen_display_struct(cgen_ctx_t *ctx, int hops)
{
   if (hops == 1)
      return ctx->display;
   else
      assert(hops == 0);

   const vunit_kind_t kind = vcode_unit_kind();

   const bool has_params =
      kind == VCODE_UNIT_FUNCTION || kind == VCODE_UNIT_PROCEDURE;
   const int nvars = vcode_count_vars();
   const int nparams = has_params ? vcode_count_params() : 0;
   const int nfields = nvars + nparams + (ctx->display ? 1 : 0);
   LLVMValueRef fields[nfields];
   LLVMValueRef *outptr = fields;

   for (int i = 0; i < nvars; i++)
      *outptr++ = cgen_get_var(vcode_var_handle(i), ctx);

   for (int i = 0; i < nparams; i++)
      *outptr++ = ctx->regs[i];

   if (ctx->display != NULL)
      *outptr++ = ctx->display;

   assert(outptr == fields + nfields);

   LLVMTypeRef types[nfields];
   for (int i = 0; i < nfields; i++)
      types[i] = LLVMTypeOf(fields[i]);

   LLVMValueRef result = LLVMGetUndef(LLVMStructType(types, nfields, false));
   for (int i = 0; i < nfields; i++)
      result = LLVMBuildInsertValue(builder, result, fields[i], i, "");

   return result;
}

static LLVMValueRef cgen_array_pointer(LLVMValueRef array)
{
   LLVMValueRef indexes[] = {
      llvm_int32(0),
      llvm_int32(0)
   };
   return LLVMBuildGEP(builder, array, indexes, ARRAY_LEN(indexes), "");
}

static void cgen_sched_process(LLVMValueRef after)
{
   LLVMValueRef args[] = { after };
   LLVMBuildCall(builder, llvm_fn("_sched_process"), args, 1, "");
}

static char *cgen_signal_nets_name(vcode_signal_t sig)
{
   ident_t name = vcode_signal_name(sig);
   return xasprintf("%s_nets", safe_symbol(istr(name)));
}

static LLVMValueRef cgen_signal_nets(vcode_signal_t sig)
{
   char *buf LOCAL = cgen_signal_nets_name(sig);
   LLVMValueRef nets = LLVMGetNamedGlobal(module, buf);
   assert(nets);

   return cgen_array_pointer(nets);
}

static LLVMValueRef cgen_location(cgen_ctx_t *ctx)
{
   LLVMValueRef file_name = hash_get(string_pool, ctx->last_loc.file);
   if (file_name == NULL) {
      const char *name_str = istr(ctx->last_loc.file);
      const size_t len = strlen(name_str);
      file_name = LLVMAddGlobal(module,
                                LLVMArrayType(LLVMInt8Type(), len + 1),
                                "file_name");
      LLVMSetGlobalConstant(file_name, true);
      LLVMSetInitializer(file_name, LLVMConstString(name_str, len, false));
      LLVMSetLinkage(file_name, LLVMPrivateLinkage);
      LLVMSetUnnamedAddr(file_name, true);

      hash_put(string_pool, ctx->last_loc.file, file_name);
   }

   LLVMTypeRef rt_loc = llvm_rt_loc();

   LLVMValueRef init = LLVMGetUndef(rt_loc);
   init = LLVMBuildInsertValue(builder, init,
                               llvm_int32(ctx->last_loc.first_line), 0, "");
   init = LLVMBuildInsertValue(builder, init,
                               llvm_int32(ctx->last_loc.last_line), 1, "");
   init = LLVMBuildInsertValue(builder, init,
                               llvm_int16(ctx->last_loc.first_column), 2, "");
   init = LLVMBuildInsertValue(builder, init,
                               llvm_int16(ctx->last_loc.last_column), 3, "");
   init = LLVMBuildInsertValue(builder, init,
                               cgen_array_pointer(file_name), 4, "");

   LLVMValueRef tmp = LLVMBuildAlloca(builder, rt_loc, "loc");
   LLVMBuildStore(builder, init, tmp);
   return tmp;
}

static LLVMValueRef cgen_hint_str(int op)
{
   const char *hint = vcode_get_hint(op);
   if (hint == NULL)
      return LLVMConstNull(llvm_char_ptr());

   ident_t hint_id = ident_new(hint);
   LLVMValueRef glob = hash_get(string_pool, hint_id);

   if (glob == NULL) {
      const size_t len = strlen(hint);
      LLVMTypeRef type = LLVMArrayType(LLVMInt8Type(), len + 1);

      glob = LLVMAddGlobal(module, type, "");
      LLVMSetGlobalConstant(glob, true);
      LLVMSetLinkage(glob, LLVMPrivateLinkage);
      LLVMSetInitializer(glob, LLVMConstString(hint, len, false));
      LLVMSetUnnamedAddr(glob, true);

      hash_put(string_pool, hint_id, glob);
   }

   return cgen_array_pointer(glob);
}

static LLVMValueRef cgen_signature(ident_t name, vcode_type_t result,
                                   LLVMTypeRef display_type,
                                   const vcode_type_t *vparams, size_t nparams)
{
   const char *safe_name = safe_symbol(istr(name));
   LLVMValueRef fn = LLVMGetNamedFunction(module, safe_name);
   if (fn != NULL)
      return fn;
   else if (vparams == NULL)
      return NULL;

   const bool is_procedure = result == VCODE_INVALID_TYPE;
   const int nextra  = (display_type ? 1 : 0) + (is_procedure ? 1 : 0);
   LLVMTypeRef params[nparams + nextra + 1];
   LLVMTypeRef *p = params;
   for (size_t i = 0; i < nparams; i++) {
      if (vtype_kind(vparams[i]) == VCODE_TYPE_UARRAY)
         *p++ = LLVMPointerType(cgen_type(vparams[i]), 0);
      else
         *p++ = cgen_type(vparams[i]);
   }

   if (display_type != NULL)
      *p++ = display_type;

   if (is_procedure)
      *p++ = llvm_void_ptr();

   LLVMTypeRef type = NULL;
   if (is_procedure)
      type = LLVMFunctionType(llvm_void_ptr(), params, nparams + nextra, false);
   else
      type = LLVMFunctionType(cgen_type(result), params,
                              nparams + nextra, false);

   fn = LLVMAddFunction(module, safe_name, type);

   cgen_add_func_attr(fn, FUNC_ATTR_DLLEXPORT, -1);
   cgen_add_func_attr(fn, FUNC_ATTR_NOUNWIND, -1);

   for (size_t i = 0; i < nparams; i++) {
      if (vtype_kind(vparams[i]) == VCODE_TYPE_UARRAY) {
         if (!is_procedure) {
            // Procedures may capture argument before suspending
            cgen_add_func_attr(fn, FUNC_ATTR_NOCAPTURE, i + 1);
         }
         LLVMSetParamAlignment(LLVMGetParam(fn, i), sizeof(void *));
      }
   }

   return fn;
}

static LLVMValueRef cgen_subprogram_arg(int op, int arg, cgen_ctx_t *ctx)
{
   LLVMValueRef value = cgen_get_arg(op, arg, ctx);
   if (vcode_reg_kind(vcode_get_arg(op, arg)) == VCODE_TYPE_UARRAY) {
      // Pass all uarray parameters by pointer to match C calling convention
      LLVMValueRef mem = LLVMBuildAlloca(builder, LLVMTypeOf(value), "");
      LLVMBuildStore(builder, value, mem);
      return mem;
   }
   else
      return value;
}

static void cgen_op_return(int op, cgen_ctx_t *ctx)
{
   if (vcode_unit_kind() == VCODE_UNIT_PROCEDURE) {
      LLVMBuildFree(builder, ctx->state);
      LLVMBuildRet(builder, LLVMConstNull(llvm_void_ptr()));
   }
   else {
      if (vcode_count_args(op) > 0)
         LLVMBuildRet(builder, cgen_get_arg(op, 0, ctx));
      else {
         LLVMTypeRef fn_type = LLVMGetElementType(LLVMTypeOf(ctx->fn));
         if (LLVMGetTypeKind(LLVMGetReturnType(fn_type)) == LLVMVoidTypeKind)
            LLVMBuildRetVoid(builder);
         else
            LLVMBuildRet(builder, LLVMConstNull(llvm_void_ptr()));
      }
   }
}

static void cgen_op_jump(int i, cgen_ctx_t *ctx)
{
   LLVMBuildBr(builder, ctx->blocks[vcode_get_target(i, 0)]);
}

static void cgen_op_fcall(int op, bool nested, cgen_ctx_t *ctx)
{
   vcode_reg_t result = vcode_get_result(op);
   const bool proc = (result == VCODE_INVALID_REG);

   ident_t func = vcode_get_func(op);
   const int nargs = vcode_count_args(op);
   const int total_args = nargs + (nested ? 1 : 0) + (proc ? 1 : 0);

   LLVMValueRef fn = cgen_signature(func, VCODE_INVALID_TYPE, NULL, NULL, 0);
   if (fn == NULL) {
      vcode_type_t atypes[nargs];
      for (int i = 0; i < nargs; i++)
         atypes[i] = vcode_reg_type(vcode_get_arg(op, i));

      LLVMTypeRef display_type = NULL;
      if (nested)
         display_type = cgen_display_type(vcode_active_unit());

      vcode_type_t rtype = VCODE_INVALID_TYPE;
      if (result != VCODE_INVALID_REG)
         rtype = vcode_reg_type(result);

      fn = cgen_signature(func, rtype, display_type, atypes, nargs);
   }

   LLVMValueRef args[total_args];
   LLVMValueRef *pa = args;
   for (int i = 0; i < nargs; i++)
      *pa++ = cgen_subprogram_arg(op, i, ctx);

   if (nested)
      *pa++ = cgen_display_struct(ctx, vcode_get_hops(op));

   if (proc)
      *pa++ = LLVMConstNull(llvm_void_ptr());

   if (result != VCODE_INVALID_REG)
      ctx->regs[result] = LLVMBuildCall(builder, fn, args, total_args,
                                        cgen_reg_name(result));
   else
      LLVMBuildCall(builder, fn, args, total_args, "");
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

   const int length = vcode_count_args(op);
   LLVMValueRef *tmp LOCAL = xmalloc(length * sizeof(LLVMValueRef));
   for (int i = 0; i < length; i++)
      tmp[i] = ctx->regs[vcode_get_arg(op, i)];

   if (vtype_kind(vcode_reg_type(result)) == VCODE_TYPE_POINTER) {
      char *name LOCAL = xasprintf("%s_const_array_r%d",
                                   istr(vcode_unit_name()), result);

      LLVMTypeRef elem_type  = cgen_type(vtype_pointed(type));
      LLVMTypeRef array_type = LLVMArrayType(elem_type, length);

      LLVMValueRef global = LLVMAddGlobal(module, array_type, name);
      LLVMSetLinkage(global, LLVMInternalLinkage);
      LLVMSetGlobalConstant(global, true);
      LLVMSetUnnamedAddr(global, true);

      LLVMValueRef init = LLVMConstArray(elem_type, tmp, length);
      LLVMSetInitializer(global, init);

      ctx->regs[result] = cgen_array_pointer(global);
      LLVMSetValueName(ctx->regs[result], cgen_reg_name(result));
   }
   else
      ctx->regs[result] = LLVMConstArray(cgen_type(vtype_elem(type)),
                                         tmp, length);
}

static void cgen_op_cmp(int op, cgen_ctx_t *ctx)
{
   vcode_reg_t result = vcode_get_result(op);

   vcode_type_t arg_type = vcode_reg_type(vcode_get_arg(op, 0));
   vtype_kind_t arg_kind = vtype_kind(arg_type);

   if (arg_kind == VCODE_TYPE_REAL) {
      LLVMRealPredicate pred = 0;
      switch (vcode_get_cmp(op)) {
      case VCODE_CMP_EQ:  pred = LLVMRealUEQ; break;
      case VCODE_CMP_NEQ: pred = LLVMRealUNE; break;
      case VCODE_CMP_LT:  pred = LLVMRealULT; break;
      case VCODE_CMP_GT:  pred = LLVMRealUGT; break;
      case VCODE_CMP_LEQ: pred = LLVMRealULE; break;
      case VCODE_CMP_GEQ: pred = LLVMRealUGE; break;
      }

      ctx->regs[result] =
         LLVMBuildFCmp(builder, pred, cgen_get_arg(op, 0, ctx),
                       cgen_get_arg(op, 1, ctx), cgen_reg_name(result));
   }
   else {
      const bool is_signed =
         arg_kind != VCODE_TYPE_INT || vtype_low(arg_type) < 0;

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
}

static void cgen_op_report(int op, cgen_ctx_t *ctx)
{
   LLVMValueRef severity = cgen_get_arg(op, 0, ctx);
   LLVMValueRef message  = cgen_get_arg(op, 1, ctx);
   LLVMValueRef length   = cgen_get_arg(op, 2, ctx);

   LLVMValueRef args[] = {
      message,
      length,
      severity,
      llvm_int8(1),
      cgen_location(ctx)
   };
   LLVMBuildCall(builder, llvm_fn("_assert_fail"), args, ARRAY_LEN(args), "");
}

static void cgen_op_assert(int op, cgen_ctx_t *ctx)
{
   LLVMValueRef test = ctx->regs[vcode_get_arg(op, 0)];

   LLVMBasicBlockRef thenbb = LLVMAppendBasicBlock(ctx->fn, "assert_fail");
   LLVMBasicBlockRef elsebb = LLVMAppendBasicBlock(ctx->fn, "assert_pass");

   LLVMBuildCondBr(builder, test, elsebb, thenbb);

   LLVMPositionBuilderAtEnd(builder, thenbb);

   vcode_reg_t mreg = vcode_get_arg(op, 2);

   LLVMValueRef message, length;
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
         LLVMSetUnnamedAddr(global, true);
      }

      length = llvm_int32(def_len);

      message = cgen_array_pointer(global);
   }
   else {
      message = cgen_get_arg(op, 2, ctx);
      length  = cgen_get_arg(op, 3, ctx);
   }

   LLVMValueRef severity = ctx->regs[vcode_get_arg(op, 1)];

   LLVMValueRef args[] = {
      message,
      length,
      severity,
      llvm_int8(0),
      cgen_location(ctx)
   };
   LLVMBuildCall(builder, llvm_fn("_assert_fail"), args, ARRAY_LEN(args), "");

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

   if (vcode_unit_kind() == VCODE_UNIT_PROCEDURE) {
      LLVMBuildCall(builder, llvm_fn("_private_stack"), NULL, 0, "");
      LLVMBuildRet(builder, llvm_void_cast(ctx->state));
   }
   else
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
   vtype_kind_t kind = vtype_kind(vcode_reg_type(result));

   LLVMValueRef lhs = cgen_get_arg(op, 0, ctx);
   LLVMValueRef rhs = cgen_get_arg(op, 1, ctx);

   if (kind == VCODE_TYPE_POINTER || kind == VCODE_TYPE_SIGNAL) {
      LLVMValueRef index[] = { rhs };
      ctx->regs[result] = LLVMBuildGEP(builder, lhs,
                                       index, ARRAY_LEN(index),
                                       cgen_reg_name(result));
   }
   else if (vcode_reg_kind(result) == VCODE_TYPE_REAL)
      ctx->regs[result] = LLVMBuildFAdd(builder, lhs, rhs,
                                        cgen_reg_name(result));
   else
      ctx->regs[result] = LLVMBuildAdd(builder, lhs, rhs,
                                       cgen_reg_name(result));
}

static void cgen_op_sub(int op, cgen_ctx_t *ctx)
{
   vcode_reg_t result = vcode_get_result(op);

   LLVMValueRef lhs = cgen_get_arg(op, 0, ctx);
   LLVMValueRef rhs = cgen_get_arg(op, 1, ctx);

   if (vcode_reg_kind(result) == VCODE_TYPE_REAL)
      ctx->regs[result] = LLVMBuildFSub(builder, lhs, rhs,
                                        cgen_reg_name(result));
   else
      ctx->regs[result] = LLVMBuildSub(builder, lhs, rhs,
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

static void cgen_op_nand(int op, cgen_ctx_t *ctx)
{
   vcode_reg_t result = vcode_get_result(op);
   ctx->regs[result] =
      LLVMBuildNot(builder,
                   LLVMBuildAnd(builder,
                                cgen_get_arg(op, 0, ctx),
                                cgen_get_arg(op, 1, ctx),
                                "nand_tmp"),
                   cgen_reg_name(result));
}

static void cgen_op_nor(int op, cgen_ctx_t *ctx)
{
   vcode_reg_t result = vcode_get_result(op);
   ctx->regs[result] =
      LLVMBuildNot(builder,
                   LLVMBuildOr(builder,
                               cgen_get_arg(op, 0, ctx),
                               cgen_get_arg(op, 1, ctx),
                               "nor_tmp"),
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

static void cgen_op_xnor(int op, cgen_ctx_t *ctx)
{
   vcode_reg_t result = vcode_get_result(op);
   ctx->regs[result] = LLVMBuildNot(builder,
                                    LLVMBuildXor(builder,
                                                 cgen_get_arg(op, 0, ctx),
                                                 cgen_get_arg(op, 1, ctx),
                                                 ""),
                                    cgen_reg_name(result));
}

static void cgen_op_xor(int op, cgen_ctx_t *ctx)
{
   vcode_reg_t result = vcode_get_result(op);
   ctx->regs[result] = LLVMBuildXor(builder,
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

   LLVMValueRef lhs = cgen_get_arg(op, 0, ctx);
   LLVMValueRef rhs = cgen_get_arg(op, 1, ctx);

   if (vcode_reg_kind(result) == VCODE_TYPE_REAL)
      ctx->regs[result] = LLVMBuildFMul(builder, lhs, rhs,
                                        cgen_reg_name(result));
   else
      ctx->regs[result] = LLVMBuildMul(builder, lhs, rhs,
                                       cgen_reg_name(result));
}

static void cgen_op_div(int op, cgen_ctx_t *ctx)
{
   vcode_reg_t result = vcode_get_result(op);

   const bool is_real = vtype_kind(vcode_reg_type(result)) == VCODE_TYPE_REAL;

   if (is_real)
      ctx->regs[result] = LLVMBuildFDiv(builder,
                                        cgen_get_arg(op, 0, ctx),
                                        cgen_get_arg(op, 1, ctx),
                                        cgen_reg_name(result));
   else {
      LLVMBasicBlockRef zero_bb = LLVMAppendBasicBlock(ctx->fn, "div_zero");
      LLVMBasicBlockRef ok_bb   = LLVMAppendBasicBlock(ctx->fn, "div_ok");

      LLVMValueRef denom = cgen_get_arg(op, 1, ctx);

      LLVMValueRef zero = LLVMConstInt(LLVMTypeOf(denom), 0, false);
      LLVMValueRef div_by_zero =
         LLVMBuildICmp(builder, LLVMIntEQ, denom, zero, "div0");

      LLVMBuildCondBr(builder, div_by_zero, zero_bb, ok_bb);

      LLVMPositionBuilderAtEnd(builder, zero_bb);

      LLVMValueRef args[] = {
         cgen_location(ctx)
      };
      LLVMBuildCall(builder, llvm_fn("_div_zero"), args, ARRAY_LEN(args), "");
      LLVMBuildUnreachable(builder);

      LLVMPositionBuilderAtEnd(builder, ok_bb);

      ctx->regs[result] = LLVMBuildSDiv(builder,
                                        cgen_get_arg(op, 0, ctx),
                                        denom,
                                        cgen_reg_name(result));
   }
}

static void cgen_op_rem(int op, cgen_ctx_t *ctx)
{
   vcode_reg_t result = vcode_get_result(op);
   ctx->regs[result] = LLVMBuildSRem(builder,
                                     cgen_get_arg(op, 0, ctx),
                                     cgen_get_arg(op, 1, ctx),
                                     cgen_reg_name(result));
}

static void cgen_op_mod(int op, cgen_ctx_t *ctx)
{
   vcode_reg_t result = vcode_get_result(op);
   ctx->regs[result] = LLVMBuildURem(builder,
                                     cgen_get_arg(op, 0, ctx),
                                     cgen_get_arg(op, 1, ctx),
                                     cgen_reg_name(result));
}

static void cgen_op_exp(int op, cgen_ctx_t *ctx)
{
   vcode_reg_t result = vcode_get_result(op);

   // TODO: implement this without the cast
   LLVMValueRef cast[] = {
      LLVMBuildUIToFP(builder, cgen_get_arg(op, 0, ctx), LLVMDoubleType(), ""),
      LLVMBuildUIToFP(builder, cgen_get_arg(op, 1, ctx), LLVMDoubleType(), "")
   };
   ctx->regs[result] = LLVMBuildFPToUI(
      builder,
      LLVMBuildCall(builder, llvm_fn("llvm.pow.f64"), cast, 2, ""),
      cgen_type(vcode_reg_type(result)),
      "pow");
}

static void cgen_op_neg(int op, cgen_ctx_t *ctx)
{
   vcode_reg_t result = vcode_get_result(op);
   const bool real = (vcode_reg_kind(result) == VCODE_TYPE_REAL);
   ctx->regs[result] =
      (real ? LLVMBuildFNeg : LLVMBuildNeg)(builder,
                                            cgen_get_arg(op, 0, ctx),
                                            cgen_reg_name(result));
}

static void cgen_op_abs(int op, cgen_ctx_t *ctx)
{
   vcode_reg_t result = vcode_get_result(op);

   LLVMValueRef arg  = cgen_get_arg(op, 0, ctx);

   const bool real = (vcode_reg_kind(result) == VCODE_TYPE_REAL);

   LLVMValueRef zero = real
      ? LLVMConstReal(LLVMDoubleType(), 0.0)
      : LLVMConstInt(LLVMTypeOf(arg), 0, false);

   LLVMValueRef negative = real
      ? LLVMBuildFCmp(builder, LLVMRealULT, arg, zero, "")
      : LLVMBuildICmp(builder, LLVMIntSLT, arg, zero, "");

   ctx->regs[result] = LLVMBuildSelect(
      builder,
      negative,
      (real ? LLVMBuildFNeg : LLVMBuildNeg)(builder, arg, ""),
      arg,
      cgen_reg_name(result));
}

static void cgen_op_bounds(int op, cgen_ctx_t *ctx)
{
   vcode_type_t vtype = vcode_get_type(op);
   if (vtype_kind(vtype) == VCODE_TYPE_REAL)
      return;  // TODO

   LLVMValueRef value_raw = cgen_get_arg(op, 0, ctx);
   LLVMTypeRef value_type = LLVMTypeOf(value_raw);

   const int value_bits = LLVMGetIntTypeWidth(value_type);
   LLVMValueRef value = NULL, min = NULL, max = NULL;
   if (value_bits < 32) {
      value = LLVMBuildZExt(builder, value_raw, LLVMInt32Type(), "");
      min   = llvm_int32(vtype_low(vtype));
      max   = llvm_int32(vtype_high(vtype));
   }
   else {
      value = value_raw;
      min   = LLVMConstInt(value_type, vtype_low(vtype), false);
      max   = LLVMConstInt(value_type, vtype_high(vtype), false);
   }

   LLVMValueRef above =
      LLVMBuildICmp(builder, LLVMIntSGE, value, min, "above");
   LLVMValueRef below =
      LLVMBuildICmp(builder, LLVMIntSLE, value, max, "below");

   LLVMValueRef in = LLVMBuildAnd(builder, above, below, "in");

   LLVMBasicBlockRef pass_bb  = LLVMAppendBasicBlock(ctx->fn, "bounds_pass");
   LLVMBasicBlockRef fail_bb  = LLVMAppendBasicBlock(ctx->fn, "bounds_fail");

   LLVMBuildCondBr(builder, in, pass_bb, fail_bb);

   LLVMPositionBuilderAtEnd(builder, fail_bb);

   if (value_bits > 32) {
      // TODO: we should probably pass all the arguments as 64-bit here
      LLVMTypeRef ll_int32 = LLVMInt32Type();
      value = LLVMBuildTrunc(builder, value, ll_int32, "");
      min   = LLVMBuildTrunc(builder, min, ll_int32, "");
      max   = LLVMBuildTrunc(builder, max, ll_int32, "");
   }

   LLVMValueRef args[] = {
      value,
      min,
      max,
      llvm_int32(vcode_get_subkind(op)),
      cgen_location(ctx),
      cgen_hint_str(op),
   };

   LLVMBuildCall(builder, llvm_fn("_bounds_fail"), args, ARRAY_LEN(args), "");

   LLVMBuildUnreachable(builder);

   LLVMPositionBuilderAtEnd(builder, pass_bb);
}

static void cgen_op_dynamic_bounds(int op, cgen_ctx_t *ctx)
{
   LLVMValueRef value_raw = cgen_get_arg(op, 0, ctx);
   LLVMTypeRef value_type = LLVMTypeOf(value_raw);

   const int value_bits = LLVMGetIntTypeWidth(value_type);
   LLVMValueRef value = NULL, min = NULL, max = NULL;
   if (value_bits < 32) {
      LLVMTypeRef ll_int32 = LLVMInt32Type();
      value = LLVMBuildZExt(builder, value_raw, ll_int32, "");
      min   = LLVMBuildZExt(builder, cgen_get_arg(op, 1, ctx), ll_int32, "");
      max   = LLVMBuildZExt(builder, cgen_get_arg(op, 2, ctx), ll_int32, "");
   }
   else {
      value = value_raw;
      min   = cgen_get_arg(op, 1, ctx);
      max   = cgen_get_arg(op, 2, ctx);
   }

   LLVMValueRef kind = cgen_get_arg(op, 3, ctx);

   LLVMValueRef above =
      LLVMBuildICmp(builder, LLVMIntSGE, value, min, "above");
   LLVMValueRef below =
      LLVMBuildICmp(builder, LLVMIntSLE, value, max, "below");

   LLVMValueRef in = LLVMBuildAnd(builder, above, below, "in");

   LLVMBasicBlockRef pass_bb  = LLVMAppendBasicBlock(ctx->fn, "bounds_pass");
   LLVMBasicBlockRef fail_bb  = LLVMAppendBasicBlock(ctx->fn, "bounds_fail");

   LLVMBuildCondBr(builder, in, pass_bb, fail_bb);

   LLVMPositionBuilderAtEnd(builder, fail_bb);

   if (value_bits > 32) {
      // TODO: we should probably pass all the arguments as 64-bit here
      LLVMTypeRef ll_int32 = LLVMInt32Type();
      value = LLVMBuildTrunc(builder, value, ll_int32, "");
      min   = LLVMBuildTrunc(builder, min, ll_int32, "");
      max   = LLVMBuildTrunc(builder, max, ll_int32, "");
   }

   LLVMValueRef args[] = {
      value,
      min,
      max,
      kind,
      cgen_location(ctx),
      cgen_hint_str(op),
   };

   LLVMBuildCall(builder, llvm_fn("_bounds_fail"), args, ARRAY_LEN(args), "");

   LLVMBuildUnreachable(builder);

   LLVMPositionBuilderAtEnd(builder, pass_bb);
}

static void cgen_op_image_map(int op, cgen_ctx_t *ctx)
{
   image_map_t map;
   vcode_get_image_map(op, &map);

   size_t max_len = 0;
   for (size_t i = 0; i < map.nelems; i++) {
      const size_t len = ident_len(map.elems[i]);
      max_len = MAX(max_len, len);
   }

   ident_t elems_name = ident_prefix(map.name, ident_new("elems"), '.');
   LLVMValueRef elems_glob = LLVMGetNamedGlobal(module, istr(elems_name));
   if (elems_glob == NULL) {
      const size_t total_chars = map.nelems * (max_len + 1);
      LLVMTypeRef array_type = LLVMArrayType(LLVMInt8Type(), total_chars);

      elems_glob = LLVMAddGlobal(module, array_type, istr(elems_name));
      LLVMSetGlobalConstant(elems_glob, true);
      LLVMSetLinkage(elems_glob, LLVMLinkOnceAnyLinkage);

      char *strings LOCAL = xmalloc(total_chars);
      for (size_t i = 0; i < map.nelems; i++) {
         const size_t tlen = ident_len(map.elems[i]);
         for (size_t j = 0; j < max_len + 1; j++) {
            const size_t off = (i * (max_len + 1)) + j;
            if (j < tlen)
               strings[off] = ident_char(map.elems[i], tlen - j - 1);
            else
               strings[off] = '\0';
         }
      }

      LLVMValueRef init = LLVMConstString(strings, total_chars, true);
      LLVMSetInitializer(elems_glob, init);
   }

   LLVMValueRef elems_ptr = cgen_array_pointer(elems_glob);

   LLVMValueRef values_ptr = NULL;
   if (map.values != NULL) {
      ident_t values_name = ident_prefix(map.name, ident_new("values"), '.');
      LLVMValueRef values_glob = LLVMGetNamedGlobal(module, istr(values_name));
      if (values_glob == NULL) {
         LLVMTypeRef elem_type = LLVMInt64Type();
         LLVMTypeRef array_type = LLVMArrayType(elem_type, map.nelems);
         values_glob = LLVMAddGlobal(module, array_type, istr(values_name));
         LLVMSetGlobalConstant(values_glob, true);
         LLVMSetLinkage(values_glob, LLVMLinkOnceAnyLinkage);

         LLVMValueRef *lvalues = xmalloc(sizeof(LLVMValueRef) * map.nelems);
         for (size_t i = 0; i < map.nelems; i++)
            lvalues[i] = LLVMConstInt(elem_type, map.values[i], false);

         LLVMValueRef init = LLVMConstArray(elem_type, lvalues, map.nelems);
         LLVMSetInitializer(values_glob, init);

         free(lvalues);
      }

      values_ptr = cgen_array_pointer(values_glob);
   }
   else
      values_ptr = LLVMConstNull(LLVMPointerType(LLVMInt64Type(), 0));

   LLVMValueRef lmap = LLVMGetUndef(llvm_image_map());
   lmap = LLVMBuildInsertValue(builder, lmap, llvm_int32(map.kind), 0, "");
   lmap = LLVMBuildInsertValue(builder, lmap, llvm_int32(max_len + 1), 1, "");
   lmap = LLVMBuildInsertValue(builder, lmap, elems_ptr, 2, "");
   lmap = LLVMBuildInsertValue(builder, lmap, values_ptr, 3, "");
   lmap = LLVMBuildInsertValue(builder, lmap, llvm_int32(map.nelems), 4, "");

   vcode_reg_t result = vcode_get_result(op);
   LLVMValueRef tmp =
      LLVMBuildAlloca(builder, llvm_image_map(), cgen_reg_name(result));
   LLVMBuildStore(builder, lmap, tmp);
   ctx->regs[result] = tmp;
}

static void cgen_op_image(int op, cgen_ctx_t *ctx)
{
   vcode_reg_t arg = vcode_get_arg(op, 0);
   vcode_type_t arg_type = vcode_reg_type(arg);
   vtype_kind_t arg_kind = vtype_kind(arg_type);

   const bool is_signed = arg_kind == VCODE_TYPE_INT && vtype_low(arg_type) < 0;
   const bool real = (arg_kind == VCODE_TYPE_REAL);

   LLVMValueRef image_map;
   if (vcode_count_args(op) > 1)
      image_map = cgen_get_arg(op, 1, ctx);
   else {
      image_map = LLVMBuildAlloca(builder, llvm_image_map(), "");
      LLVMBuildStore(
         builder,
         LLVMBuildInsertValue(builder,
                              LLVMGetUndef(llvm_image_map()),
                              llvm_int32(real ? IMAGE_REAL : IMAGE_INTEGER),
                              0, ""),
         image_map);
   }

   LLVMOpcode cop = real ? LLVMBitCast : (is_signed ? LLVMSExt : LLVMZExt);
   LLVMValueRef res = LLVMBuildAlloca(builder,
                                      llvm_uarray_type(LLVMInt8Type(), 1),
                                      "image");
   LLVMValueRef iargs[] = {
      LLVMBuildCast(builder, cop, ctx->regs[arg], LLVMInt64Type(), ""),
      image_map,
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

   vcode_type_t arg_type    = vcode_reg_type(arg);
   vcode_type_t result_type = vcode_reg_type(result);

   vtype_kind_t arg_kind    = vtype_kind(arg_type);
   vtype_kind_t result_kind = vtype_kind(result_type);

   LLVMValueRef arg_ll = cgen_get_arg(op, 0, ctx);

   if (arg_kind == VCODE_TYPE_CARRAY) {
      // This is a no-op as constrained arrays are implemented as pointers
      ctx->regs[result] = ctx->regs[arg];
   }
   else if (result_kind == VCODE_TYPE_REAL && arg_kind == VCODE_TYPE_INT) {
      ctx->regs[result] = LLVMBuildSIToFP(builder, arg_ll,
                                          cgen_type(result_type),
                                          cgen_reg_name(result));
   }
   else if (result_kind == VCODE_TYPE_INT && arg_kind == VCODE_TYPE_REAL) {
      LLVMValueRef args[] = { arg_ll };
      LLVMValueRef rounded = LLVMBuildCall(builder, llvm_fn("llvm.round.f64"),
                                           args, ARRAY_LEN(args), "");
      ctx->regs[result] = LLVMBuildFPToSI(builder, rounded,
                                          cgen_type(result_type),
                                          cgen_reg_name(result));
   }
   else if (result_kind == VCODE_TYPE_INT || result_kind == VCODE_TYPE_OFFSET) {
      const int abits = bits_for_range(vtype_low(arg_type),
                                       vtype_high(arg_type));
      const int rbits = bits_for_range(vtype_low(result_type),
                                       vtype_high(result_type));

      LLVMTypeRef result_type_ll = cgen_type(result_type);

      if (rbits < abits)
         ctx->regs[result] = LLVMBuildTrunc(builder, arg_ll, result_type_ll,
                                            cgen_reg_name(result));
      else if (vtype_low(arg_type) < 0)
         ctx->regs[result] = LLVMBuildSExt(builder, arg_ll, result_type_ll,
                                           cgen_reg_name(result));
      else
         ctx->regs[result] = LLVMBuildZExt(builder, arg_ll, result_type_ll,
                                           cgen_reg_name(result));
   }
   else
      ctx->regs[result] = LLVMBuildCast(builder, LLVMBitCast, arg_ll,
                                        cgen_type(result_type),
                                        cgen_reg_name(result));
}

static void cgen_op_alloca(int op, cgen_ctx_t *ctx)
{
   vcode_reg_t result = vcode_get_result(op);
   LLVMTypeRef type   = cgen_type(vcode_get_type(op));

   if (vcode_get_subkind(op) == VCODE_ALLOCA_HEAP) {
      LLVMValueRef bytes = llvm_sizeof(type);
      if (vcode_count_args(op) > 0)
         bytes = LLVMBuildMul(builder, bytes, cgen_get_arg(op, 0, ctx), "");
      ctx->regs[result] = cgen_tmp_alloc(bytes, type);
   }
   else if (vcode_count_args(op) == 0)
      ctx->regs[result] = LLVMBuildAlloca(builder, type, cgen_reg_name(result));
   else {
      LLVMValueRef count = cgen_get_arg(op, 0, ctx);
      ctx->regs[result] = LLVMBuildArrayAlloca(builder, type, count,
                                               cgen_reg_name(result));
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

   LLVMValueRef var = cgen_get_var(vcode_get_address(op), ctx);
   const char *name = cgen_reg_name(result);

   LLVMValueRef index_ptr[] = {
      (vcode_count_args(op) > 0) ? cgen_get_arg(op, 0, ctx) : llvm_int32(0)
   };
   ctx->regs[result] = LLVMBuildGEP(builder, var, index_ptr,
                                    ARRAY_LEN(index_ptr), name);
}

static void cgen_op_select(int op, cgen_ctx_t *ctx)
{
   vcode_reg_t result = vcode_get_result(op);
   ctx->regs[result] = LLVMBuildSelect(builder,
                                       cgen_get_arg(op, 0, ctx),
                                       cgen_get_arg(op, 1, ctx),
                                       cgen_get_arg(op, 2, ctx),
                                       cgen_reg_name(result));
}

static void cgen_op_uarray_left(int op, cgen_ctx_t *ctx)
{
   LLVMValueRef dim = cgen_uarray_dim(cgen_get_arg(op, 0, ctx),
                                      vcode_get_dim(op));

   vcode_reg_t result = vcode_get_result(op);
   ctx->regs[result] = LLVMBuildExtractValue(builder, dim, 0,
                                             cgen_reg_name(result));
}

static void cgen_op_uarray_right(int op, cgen_ctx_t *ctx)
{
   LLVMValueRef dim = cgen_uarray_dim(cgen_get_arg(op, 0, ctx),
                                      vcode_get_dim(op));

   vcode_reg_t result = vcode_get_result(op);
   ctx->regs[result] = LLVMBuildExtractValue(builder, dim, 1,
                                             cgen_reg_name(result));
}

static void cgen_op_uarray_dir(int op, cgen_ctx_t *ctx)
{
   LLVMValueRef dim = cgen_uarray_dim(cgen_get_arg(op, 0, ctx),
                                      vcode_get_dim(op));

   vcode_reg_t result = vcode_get_result(op);
   ctx->regs[result] = LLVMBuildExtractValue(builder, dim, 2,
                                             cgen_reg_name(result));
}

static void cgen_op_uarray_len(int op, cgen_ctx_t *ctx)
{
   LLVMValueRef dim = cgen_uarray_dim(cgen_get_arg(op, 0, ctx),
                                      vcode_get_dim(op));

   LLVMValueRef left  = LLVMBuildExtractValue(builder, dim, 0, "left");
   LLVMValueRef right = LLVMBuildExtractValue(builder, dim, 1, "right");
   LLVMValueRef dir   = LLVMBuildExtractValue(builder, dim, 2, "dir");

   LLVMValueRef downto = LLVMBuildSub(builder, left, right, "");
   LLVMValueRef upto   = LLVMBuildSub(builder, right, left, "");
   LLVMValueRef diff   = LLVMBuildSelect(builder, dir, downto, upto, "");
   LLVMValueRef len    = LLVMBuildAdd(builder, diff, llvm_int32(1), "");
   LLVMValueRef zero   = llvm_int32(0);
   LLVMValueRef neg    = LLVMBuildICmp(builder, LLVMIntSLT, len, zero, "");

   vcode_reg_t result = vcode_get_result(op);
   ctx->regs[result] = LLVMBuildSelect(builder, neg, zero, len,
                                       cgen_reg_name(result));
}

static void cgen_op_param_upref(int op, cgen_ctx_t *ctx)
{
   const int hops = vcode_get_hops(op);

   vcode_state_t state;
   vcode_state_save(&state);

   for (int i = 0; i < hops; i++)
      vcode_select_unit(vcode_unit_context());

   const int nvars = vcode_count_vars();

   vcode_state_restore(&state);

   LLVMValueRef display = cgen_display_upref(hops, ctx);

   vcode_reg_t result = vcode_get_result(op);
   ctx->regs[result] = LLVMBuildExtractValue(builder, display,
                                             nvars + vcode_get_arg(op, 0),
                                             cgen_reg_name(result));
}

static void cgen_op_var_upref(int op, cgen_ctx_t *ctx)
{
   const int hops = vcode_get_hops(op);

   vcode_state_t state;
   vcode_state_save(&state);

   for (int i = 0; i < hops; i++)
      vcode_select_unit(vcode_unit_context());

   vcode_state_restore(&state);

   LLVMValueRef display = cgen_display_upref(hops, ctx);

   vcode_reg_t result = vcode_get_result(op);
   ctx->regs[result] = LLVMBuildExtractValue(builder, display,
                                             vcode_var_index(vcode_get_address(op)),
                                             cgen_reg_name(result));
}

static void cgen_op_resolved_address(int op, cgen_ctx_t *ctx)
{
   LLVMValueRef args[] = {
      llvm_int32(vcode_signal_nets(vcode_get_signal(op))[0])
   };
   LLVMValueRef res_mem = LLVMBuildCall(builder, llvm_fn("_resolved_address"),
                                        args, ARRAY_LEN(args), "");

   vcode_var_t shadow = vcode_get_address(op);
   vcode_type_t type = vcode_var_type(shadow);

   LLVMValueRef cast = LLVMBuildPointerCast(builder, res_mem,
                                            cgen_type(type), "");

   LLVMBuildStore(builder, cast, cgen_get_var(shadow, ctx));
}

static void cgen_op_nets(int op, cgen_ctx_t *ctx)
{
   vcode_signal_t sig = vcode_get_signal(op);

   vcode_reg_t result = vcode_get_result(op);
   ctx->regs[result] = cgen_signal_nets(sig);
   LLVMSetValueName(ctx->regs[result], cgen_reg_name(result));
}

static LLVMValueRef cgen_pointer_to_arg_data(int op, int arg, cgen_ctx_t *ctx)
{
   const vtype_kind_t kind = vcode_reg_kind(vcode_get_arg(op, arg));
   if (kind == VCODE_TYPE_INT || kind == VCODE_TYPE_REAL) {
      // Need to get a pointer to the data
      LLVMValueRef value = cgen_get_arg(op, arg, ctx);
      LLVMTypeRef lltype = LLVMTypeOf(value);
      LLVMValueRef valptr = LLVMBuildAlloca(builder, lltype, "");
      LLVMBuildStore(builder, value, valptr);
      return valptr;
   }
   else
      return cgen_get_arg(op, arg, ctx);
}

static void cgen_op_sched_waveform(int op, cgen_ctx_t *ctx)
{
   LLVMValueRef value = cgen_get_arg(op, 2, ctx);

   LLVMValueRef valptr = NULL, scalar = NULL;
   const vtype_kind_t kind = vcode_reg_kind(vcode_get_arg(op, 2));
   if (kind == VCODE_TYPE_INT)
      scalar = LLVMBuildZExt(builder, value, LLVMInt64Type(), "");
   else if (kind == VCODE_TYPE_REAL)
      scalar = LLVMBuildBitCast(builder, value, LLVMInt64Type(), "");
   else
      valptr = llvm_void_cast(value);

   if (scalar != NULL) {
      LLVMValueRef args[] = {
         llvm_void_cast(cgen_get_arg(op, 0, ctx)),
         scalar,
         cgen_get_arg(op, 4, ctx),
         cgen_get_arg(op, 3, ctx)
      };
      LLVMBuildCall(builder, llvm_fn("_sched_waveform_s"),
                    args, ARRAY_LEN(args), "");
   }
   else {
      LLVMValueRef args[] = {
         llvm_void_cast(cgen_get_arg(op, 0, ctx)),
         valptr,
         cgen_get_arg(op, 1, ctx),
         cgen_get_arg(op, 4, ctx),
         cgen_get_arg(op, 3, ctx)
      };
      LLVMBuildCall(builder, llvm_fn("_sched_waveform"),
                    args, ARRAY_LEN(args), "");
   }
}

static void cgen_append_size_list(size_list_array_t *list,
                                  vcode_type_t elem, unsigned count,
                                  const vcode_res_fn_t *resolution,
                                  size_t *res_elem)
{
   size_list_t *result = size_list_array_alloc(list);
   result->size  = llvm_sizeof(cgen_type(elem));
   result->count = count;
   result->flags = 0;
   result->ileft = 0;

   if (resolution != NULL) {
      assert(*res_elem < resolution->count);

      result->resolution =
         cgen_resolution_wrapper(&(resolution->element[*res_elem]));
      if (resolution->element[*res_elem].kind == RES_RECORD)
         result->flags |= R_RECORD;
      if (resolution->element[*res_elem].boundary)
         result->flags |= R_BOUNDARY;

      result->ileft = resolution->element[*res_elem].ileft;

      ++(*res_elem);
   }
   else
      result->resolution = LLVMConstNull(llvm_void_ptr());
}

static void cgen_size_list(size_list_array_t *list, vcode_type_t type,
                           const vcode_res_fn_t *resolution, size_t *res_elem)
{
   switch (vtype_kind(type)) {
   case VCODE_TYPE_INT:
   case VCODE_TYPE_REAL:
      cgen_append_size_list(list, type, 1, resolution, res_elem);
      break;
   case VCODE_TYPE_CARRAY:
      {
         vcode_type_t elem = vtype_elem(type);
         if (vtype_kind(elem) == VCODE_TYPE_RECORD) {
            const int nelems = vtype_size(type);
            const size_t res_elem_old = *res_elem;
            for (int i = 0; i < nelems; i++) {
               cgen_size_list(list, elem, resolution, res_elem);
               *res_elem = res_elem_old;
            }
         }
         else
            cgen_append_size_list(list, elem, vtype_size(type),
                                  resolution, res_elem);
      }
      break;
   case VCODE_TYPE_RECORD:
      {
         const int nfields = vtype_fields(type);
         for (int i = 0; i < nfields; i++)
            cgen_size_list(list, vtype_field(type, i), resolution, res_elem);
      }
      break;
   default:
      fatal_trace("cannot handle type %d in size list", vtype_kind(type));
   }
}

static LLVMValueRef cgen_resolution_wrapper(const vcode_res_elem_t *rdata)
{
   // Resolution functions are in LRM 93 section 2.4

   LLVMValueRef rfn = cgen_signature(rdata->name, rdata->type, NULL, NULL, 0);
   if (rfn == NULL) {
      // The resolution function is not visible yet e.g. because it
      // is declared in another package
      const bool is_record = vtype_kind(rdata->type) == VCODE_TYPE_RECORD;
      vcode_type_t rtype = is_record ? vtype_pointer(rdata->type) : rdata->type;
      vcode_type_t args[] = {
         vtype_uarray(1, rdata->type, vtype_int(0, INT32_MAX))
      };
      rfn = cgen_signature(rdata->name, rtype, NULL, args, 1);
   }

   return llvm_void_cast(rfn);
}

static void cgen_op_set_initial(int op, cgen_ctx_t *ctx)
{
   vcode_signal_t sig = vcode_get_signal(op);
   vcode_type_t type  = vtype_base(vcode_signal_type(sig));

   LLVMValueRef valptr = cgen_pointer_to_arg_data(op, 0, ctx);

   const vcode_res_fn_t *rdata = vcode_get_resolution(op);

   size_t res_elem = 0;
   size_list_array_t size_list = { 0, NULL };
   cgen_size_list(&size_list, type, rdata, &res_elem);

   // Assuming array nets are sequential
   netid_t nid = vcode_signal_nets(sig)[0];

   const char *sig_name = istr(vcode_signal_name(sig));
   const char *global_name LOCAL = xasprintf("%s.name", sig_name);
   LLVMValueRef name_ll = LLVMGetNamedGlobal(module, global_name);
   if (name_ll == NULL) {
      const size_t len = strlen(sig_name);
      name_ll = LLVMAddGlobal(module,
                              LLVMArrayType(LLVMInt8Type(), len + 1),
                              global_name);
      LLVMSetGlobalConstant(name_ll, true);
      LLVMSetInitializer(name_ll, LLVMConstString(sig_name, len, false));
      LLVMSetLinkage(name_ll, LLVMPrivateLinkage);
      LLVMSetUnnamedAddr(name_ll, true);
   }

   if (size_list.count == 1 && size_list.items[0].flags == 0) {
      LLVMValueRef args[] = {
         llvm_int32(nid),
         llvm_void_cast(valptr),
         size_list.items[0].size,
         llvm_int32(size_list.items[0].count),
         llvm_void_cast(size_list.items[0].resolution),
         llvm_int32(size_list.items[0].ileft),
         llvm_void_cast(name_ll)
      };
      LLVMBuildCall(builder, llvm_fn("_set_initial_1"), args,
                    ARRAY_LEN(args), "");
   }
   else {
      LLVMValueRef list_mem =
         LLVMBuildArrayAlloca(builder, llvm_size_list_type(),
                              llvm_int32(size_list.count),
                              "size_list");

      for (unsigned i = 0; i < size_list.count; i++) {
         LLVMValueRef offset[] = { llvm_int32(i) };
         LLVMValueRef elemptr = LLVMBuildGEP(builder, list_mem, offset, 1, "");

         LLVMBuildStore(builder, size_list.items[i].size,
                        LLVMBuildStructGEP(builder, elemptr, 0, ""));
         LLVMBuildStore(builder, llvm_int32(size_list.items[i].count),
                        LLVMBuildStructGEP(builder, elemptr, 1, ""));
         LLVMBuildStore(builder, size_list.items[i].resolution,
                        LLVMBuildStructGEP(builder, elemptr, 2, ""));
         LLVMBuildStore(builder, llvm_int32(size_list.items[i].flags),
                        LLVMBuildStructGEP(builder, elemptr, 3, ""));
         LLVMBuildStore(builder, llvm_int32(size_list.items[i].ileft),
                        LLVMBuildStructGEP(builder, elemptr, 4, ""));
      }

      LLVMValueRef args[] = {
         llvm_int32(nid),
         llvm_void_cast(valptr),
         list_mem,
         llvm_int32(size_list.count),
         llvm_void_cast(name_ll)
      };
      LLVMBuildCall(builder, llvm_fn("_set_initial"), args,
                    ARRAY_LEN(args), "");
   }

   free(size_list.items);
}

static void cgen_op_alloc_driver(int op, cgen_ctx_t *ctx)
{
   LLVMValueRef init = NULL;
   if (vcode_get_arg(op, 4) != VCODE_INVALID_REG)
      init = llvm_void_cast(cgen_pointer_to_arg_data(op, 4, ctx));
   else
      init = LLVMConstNull(llvm_void_ptr());

   LLVMValueRef args[] = {
      cgen_get_arg(op, 0, ctx),
      cgen_get_arg(op, 1, ctx),
      cgen_get_arg(op, 2, ctx),
      cgen_get_arg(op, 3, ctx),
      init
   };
   LLVMBuildCall(builder, llvm_fn("_alloc_driver"), args, ARRAY_LEN(args), "");
}

static void cgen_net_flag(int op, net_flags_t flag, cgen_ctx_t *ctx)
{
   vcode_reg_t result = vcode_get_result(op);

   LLVMValueRef nets    = cgen_get_arg(op, 0, ctx);
   LLVMValueRef n_elems = cgen_get_arg(op, 1, ctx);

   LLVMValueRef args[] = {
      llvm_void_cast(nets),
      n_elems,
      llvm_int32(flag)
   };
   ctx->regs[result] = LLVMBuildCall(builder, llvm_fn("_test_net_flag"), args,
                                     ARRAY_LEN(args), cgen_reg_name(result));
}

static void cgen_op_event(int op, cgen_ctx_t *ctx)
{
   cgen_net_flag(op, NET_F_EVENT, ctx);
}

static void cgen_op_active(int op, cgen_ctx_t *ctx)
{
   cgen_net_flag(op, NET_F_ACTIVE, ctx);
}

static void cgen_op_const_record(int op, cgen_ctx_t *ctx)
{
   vcode_reg_t result = vcode_get_result(op);
   vcode_type_t rtype = vcode_reg_type(result);

   const int nargs = vcode_count_args(op);

   LLVMValueRef fields[nargs];
   for (int i = 0; i < nargs; i++)
      fields[i] = cgen_get_arg(op, i, ctx);

   LLVMTypeRef lltype = cgen_type(rtype);
   ctx->regs[result] = LLVMConstNamedStruct(lltype, fields, nargs);
}

static void cgen_op_copy(int op, cgen_ctx_t *ctx)
{
   LLVMValueRef dest = cgen_get_arg(op, 0, ctx);
   LLVMValueRef src  = cgen_get_arg(op, 1, ctx);

   LLVMValueRef count = NULL;
   if (vcode_count_args(op) > 2)
      count = cgen_get_arg(op, 2, ctx);
   else
      count = llvm_int32(1);

   LLVMValueRef size  = llvm_sizeof(cgen_type(vcode_get_type(op)));
   LLVMValueRef bytes = LLVMBuildMul(builder, size, count, "bytes");

   LLVMValueRef memcpy_args[] = {
      llvm_void_cast(dest),
      llvm_void_cast(src),
      bytes,
#if LLVM_INTRINSIC_ALIGN
      llvm_int32(4),
#endif
      llvm_int1(0)
   };
   LLVMBuildCall(builder, llvm_fn(cgen_memcpy_name("memmove", 8)),
                 memcpy_args, ARRAY_LEN(memcpy_args), "");
}

static void cgen_op_record_ref(int op, cgen_ctx_t *ctx)
{
   vcode_reg_t result = vcode_get_result(op);
   ctx->regs[result] = LLVMBuildStructGEP(builder, cgen_get_arg(op, 0, ctx),
                                          vcode_get_field(op),
                                          cgen_reg_name(result));

   LLVMTypeRef field_type = LLVMGetElementType(LLVMTypeOf(ctx->regs[result]));
   if (LLVMGetTypeKind(field_type) == LLVMArrayTypeKind)
      ctx->regs[result] = cgen_array_pointer(ctx->regs[result]);
}

static void cgen_op_sched_event(int op, cgen_ctx_t *ctx)
{
   LLVMValueRef args[] = {
      llvm_void_cast(cgen_get_arg(op, 0, ctx)),
      cgen_get_arg(op, 1, ctx),
      llvm_int32(vcode_get_subkind(op)),
   };
   LLVMBuildCall(builder, llvm_fn("_sched_event"), args, ARRAY_LEN(args), "");
}

static void cgen_pcall_suspend(LLVMValueRef state, LLVMBasicBlockRef cont_bb,
                               cgen_ctx_t *ctx)
{
   LLVMValueRef is_null = LLVMBuildICmp(builder, LLVMIntEQ, state,
                                        LLVMConstNull(llvm_void_ptr()), "");

   LLVMBasicBlockRef suspend_bb = LLVMAppendBasicBlock(ctx->fn, "suspend");

   LLVMBuildCondBr(builder, is_null, cont_bb, suspend_bb);

   LLVMPositionBuilderAtEnd(builder, suspend_bb);

   if (vcode_unit_kind() == VCODE_UNIT_PROCEDURE)
      LLVMBuildRet(builder, llvm_void_cast(ctx->state));
   else
      LLVMBuildRetVoid(builder);

   LLVMPositionBuilderAtEnd(builder, cont_bb);
}

static void cgen_op_pcall(int op, bool nested, cgen_ctx_t *ctx)
{
   ident_t func = vcode_get_func(op);
   const int nargs = vcode_count_args(op);

   LLVMValueRef fn = cgen_signature(func, VCODE_INVALID_TYPE, NULL, NULL, 0);
   if (fn == NULL) {
      vcode_type_t atypes[nargs];
      for (int i = 0; i < nargs; i++)
         atypes[i] = vcode_reg_type(vcode_get_arg(op, i));

      LLVMTypeRef display_type = NULL;
      if (nested)
         display_type = cgen_display_type(vcode_active_unit());

      fn = cgen_signature(func, VCODE_INVALID_TYPE, display_type, atypes, nargs);
   }

   const int total_args = nargs + (nested ? 1 : 0) + 1;
   LLVMValueRef args[total_args];
   LLVMValueRef *ap = args;
   for (int i = 0; i < nargs; i++)
      *ap++ = cgen_subprogram_arg(op, i, ctx);

   if (nested)
      *ap++ = cgen_display_struct(ctx, vcode_get_hops(op));

   *ap++ = LLVMConstNull(llvm_void_ptr());

   LLVMValueRef suspend = LLVMBuildCall(builder, fn, args, total_args, "");

   assert(ctx->state);
   LLVMValueRef pcall_ptr = LLVMBuildStructGEP(builder, ctx->state, 1, "");
   LLVMBuildStore(builder, suspend, pcall_ptr);

   LLVMValueRef state_ptr = LLVMBuildStructGEP(builder, ctx->state, 0, "");
   LLVMBuildStore(builder, llvm_int32(vcode_get_target(op, 0)), state_ptr);

   cgen_pcall_suspend(suspend, ctx->blocks[vcode_get_target(op, 0)], ctx);
}

static void cgen_op_resume(int op, bool nested, cgen_ctx_t *ctx)
{
   LLVMBasicBlockRef after_bb = LLVMAppendBasicBlock(ctx->fn, "resume_after");
   LLVMBasicBlockRef call_bb  = LLVMAppendBasicBlock(ctx->fn, "resume_call");

   assert(ctx->state);
   LLVMValueRef pcall_ptr = LLVMBuildStructGEP(builder, ctx->state, 1, "");
   LLVMValueRef pcall_state = LLVMBuildLoad(builder, pcall_ptr, "");

   LLVMValueRef is_null = LLVMBuildICmp(builder, LLVMIntEQ, pcall_state,
                                        LLVMConstNull(llvm_void_ptr()), "");
   LLVMBuildCondBr(builder, is_null, after_bb, call_bb);

   LLVMPositionBuilderAtEnd(builder, call_bb);

   const char *safe_name = safe_symbol(istr(vcode_get_func(op)));
   LLVMValueRef fn = LLVMGetNamedFunction(module, safe_name);
   assert(fn != NULL);

   LLVMTypeRef fn_type = LLVMGetElementType(LLVMTypeOf(fn));

   const int nparams = LLVMCountParamTypes(fn_type);
   assert(nparams > 0);

   LLVMTypeRef param_types[nparams];
   LLVMGetParamTypes(fn_type, param_types);

   LLVMValueRef args[nparams];
   for (int i = 0; i < nparams - 1 - (nested ? 1 : 0); i++)
      args[i] = LLVMGetUndef(param_types[i]);
   if (nested)
      args[nparams - 2] = cgen_display_struct(ctx, vcode_get_hops(op));
   args[nparams - 1] = pcall_state;

   LLVMValueRef new_state = LLVMBuildCall(builder, fn, args, nparams, "");

   LLVMBuildStore(builder, new_state, pcall_ptr);

   cgen_pcall_suspend(new_state, after_bb, ctx);
}

static void cgen_op_memcmp(int op, cgen_ctx_t *ctx)
{
   // Prologue

   vcode_reg_t result = vcode_get_result(op);

   LLVMValueRef lhs_data = cgen_get_arg(op, 0, ctx);
   LLVMValueRef rhs_data = cgen_get_arg(op, 1, ctx);
   LLVMValueRef length   = cgen_get_arg(op, 2, ctx);

   LLVMValueRef i = LLVMBuildAlloca(builder, LLVMInt32Type(), "i");
   LLVMBuildStore(builder, llvm_int32(0), i);

   LLVMBasicBlockRef test_bb = LLVMAppendBasicBlock(ctx->fn, "memcmp_test");
   LLVMBasicBlockRef body_bb = LLVMAppendBasicBlock(ctx->fn, "memcmp_body");
   LLVMBasicBlockRef exit_bb = LLVMAppendBasicBlock(ctx->fn, "memcmp_exit");

   LLVMBuildBr(builder, test_bb);

   // Loop test

   LLVMPositionBuilderAtEnd(builder, test_bb);

   LLVMValueRef i_loaded = LLVMBuildLoad(builder, i, "i");
   LLVMValueRef len_ge = LLVMBuildICmp(builder, LLVMIntUGE, i_loaded,
                                       length, "len_ge");
   LLVMBuildCondBr(builder, len_ge, exit_bb, body_bb);

   // Loop body

   LLVMPositionBuilderAtEnd(builder, body_bb);

   LLVMValueRef l_ptr = LLVMBuildGEP(builder, lhs_data, &i_loaded, 1, "l_ptr");
   LLVMValueRef r_ptr = LLVMBuildGEP(builder, rhs_data, &i_loaded, 1, "r_ptr");

   LLVMValueRef l_val = LLVMBuildLoad(builder, l_ptr, "l_val");
   LLVMValueRef r_val = LLVMBuildLoad(builder, r_ptr, "r_val");

   LLVMValueRef eq = LLVMBuildICmp(builder, LLVMIntEQ, l_val, r_val, "eq");

   LLVMValueRef inc = LLVMBuildAdd(builder, i_loaded, llvm_int32(1), "inc");
   LLVMBuildStore(builder, inc, i);

   LLVMBuildCondBr(builder, eq, test_bb, exit_bb);

   // Epilogue

   LLVMPositionBuilderAtEnd(builder, exit_bb);

   LLVMValueRef phi = LLVMBuildPhi(builder, LLVMInt1Type(),
                                   cgen_reg_name(result));

   LLVMValueRef      values[] = { eq,      len_ge  };
   LLVMBasicBlockRef bbs[]    = { body_bb, test_bb };
   LLVMAddIncoming(phi, values, bbs, 2);

   ctx->regs[result] = phi;
}

static void cgen_op_memset(int op, cgen_ctx_t *ctx)
{
   LLVMValueRef ptr    = cgen_get_arg(op, 0, ctx);
   LLVMValueRef value  = cgen_get_arg(op, 1, ctx);
   LLVMValueRef length = cgen_get_arg(op, 2, ctx);

   LLVMValueRef args[] = {
      llvm_void_cast(ptr),
      LLVMBuildZExt(builder, value, LLVMInt8Type(), ""),
      length,
#if LLVM_INTRINSIC_ALIGN
      llvm_int32(4),
#endif
      llvm_int1(false)
   };

   LLVMBuildCall(builder, llvm_fn("llvm.memset.p0i8.i32"),
                 args, ARRAY_LEN(args), "");
}

static void cgen_op_vec_load(int op, cgen_ctx_t *ctx)
{
   LLVMValueRef signal = cgen_get_arg(op, 0, ctx);
   LLVMValueRef length =
      vcode_count_args(op) > 1 ? cgen_get_arg(op, 1, ctx) : llvm_int32(1);

   LLVMTypeRef base_type =
      cgen_type(vtype_base(vcode_reg_type(vcode_get_arg(op, 0))));

   LLVMValueRef tmp = LLVMBuildArrayAlloca(builder, base_type, length, "tmp");

   LLVMValueRef args[] = {
      llvm_void_cast(signal),
      llvm_void_cast(tmp),
      llvm_int32(0),
      LLVMBuildSub(builder, length, llvm_int32(1), "high"),
      llvm_int32(vcode_get_subkind(op))
   };

   vcode_reg_t result = vcode_get_result(op);

   LLVMValueRef raw = LLVMBuildCall(builder, llvm_fn("_vec_load"), args,
                                    ARRAY_LEN(args), cgen_reg_name(result));

   ctx->regs[result] =
      LLVMBuildPointerCast(builder, raw, cgen_type(vcode_reg_type(result)), "");
}

static void cgen_op_last_event(int op, cgen_ctx_t *ctx)
{
   LLVMValueRef signal = cgen_get_arg(op, 0, ctx);
   LLVMValueRef length =
      vcode_count_args(op) > 1 ? cgen_get_arg(op, 1, ctx) : llvm_int32(1);

   LLVMValueRef args[] = {
      llvm_void_cast(signal),
      length
   };

   vcode_reg_t result = vcode_get_result(op);
   ctx->regs[result] = LLVMBuildCall(builder, llvm_fn("_last_event"), args,
                                     ARRAY_LEN(args), cgen_reg_name(result));
}

static void cgen_op_case(int op, cgen_ctx_t *ctx)
{
   const int num_cases = vcode_count_args(op) - 1;

   LLVMBasicBlockRef else_bb = ctx->blocks[vcode_get_target(op, 0)];
   LLVMValueRef val = cgen_get_arg(op, 0, ctx);

   LLVMValueRef sw = LLVMBuildSwitch(builder, val, else_bb, num_cases);

   for (int i = 0; i < num_cases; i++)
      LLVMAddCase(sw, cgen_get_arg(op, i + 1, ctx),
                  ctx->blocks[vcode_get_target(op, i + 1)]);
}

static void cgen_op_file_open(int op, cgen_ctx_t *ctx)
{
   LLVMValueRef file   = cgen_get_arg(op, 0, ctx);
   LLVMValueRef name   = cgen_get_arg(op, 1, ctx);
   LLVMValueRef length = cgen_get_arg(op, 2, ctx);
   LLVMValueRef kind   = cgen_get_arg(op, 3, ctx);

   LLVMValueRef status = NULL;
   if (vcode_count_args(op) == 5)
      status = cgen_get_arg(op, 4, ctx);
   else
      status = LLVMConstNull(llvm_char_ptr());

   LLVMValueRef args[] = { status, file, name, length, kind };
   LLVMBuildCall(builder, llvm_fn("_file_open"), args, ARRAY_LEN(args), "");
}

static void cgen_op_file_write(int op, cgen_ctx_t *ctx)
{
   LLVMValueRef file  = cgen_get_arg(op, 0, ctx);
   LLVMValueRef value = cgen_pointer_to_arg_data(op, 1, ctx);

   LLVMTypeRef value_type = LLVMGetElementType(LLVMTypeOf(value));
   LLVMValueRef bytes = LLVMBuildIntCast(builder,
                                         LLVMSizeOf(value_type),
                                         LLVMInt32Type(), "");


   LLVMValueRef length = bytes;
   if (vcode_count_args(op) == 3)
      length = LLVMBuildMul(builder, cgen_get_arg(op, 2, ctx), bytes, "");

   LLVMValueRef args[] = { file, llvm_void_cast(value), length };
   LLVMBuildCall(builder, llvm_fn("_file_write"), args, ARRAY_LEN(args), "");
}

static void cgen_op_file_close(int op, cgen_ctx_t *ctx)
{
   LLVMValueRef file = cgen_get_arg(op, 0, ctx);

   LLVMValueRef args[] = { file };
   LLVMBuildCall(builder, llvm_fn("_file_close"), args, ARRAY_LEN(args), "");
}

static void cgen_op_file_read(int op, cgen_ctx_t *ctx)
{
   LLVMValueRef file = cgen_get_arg(op, 0, ctx);
   LLVMValueRef ptr  = cgen_get_arg(op, 1, ctx);

   LLVMTypeRef value_type = LLVMGetElementType(LLVMTypeOf(ptr));
   LLVMValueRef bytes = LLVMBuildIntCast(builder,
                                         LLVMSizeOf(value_type),
                                         LLVMInt32Type(), "");

   LLVMValueRef inlen = bytes;
   if (vcode_count_args(op) >= 3)
      inlen = LLVMBuildMul(builder, cgen_get_arg(op, 2, ctx), bytes, "");

   LLVMValueRef outlen;
   if (vcode_count_args(op) >= 4)
      outlen = cgen_get_arg(op, 3, ctx);
   else
      outlen = LLVMConstNull(LLVMPointerType(LLVMInt32Type(), 0));

   LLVMValueRef args[] = { file, llvm_void_cast(ptr), inlen, outlen };
   LLVMBuildCall(builder, llvm_fn("_file_read"), args, ARRAY_LEN(args), "");
}

static void cgen_op_endfile(int op, cgen_ctx_t *ctx)
{
   LLVMValueRef file = cgen_get_arg(op, 0, ctx);

   vcode_reg_t result = vcode_get_result(op);

   LLVMValueRef args[] = { file };
   ctx->regs[result] = LLVMBuildCall(builder, llvm_fn("_endfile"), args,
                                     ARRAY_LEN(args), cgen_reg_name(result));
}

static void cgen_op_null(int op, cgen_ctx_t *ctx)
{
   vcode_reg_t result = vcode_get_result(op);
   ctx->regs[result] = LLVMConstNull(cgen_type(vcode_reg_type(result)));
}

static void cgen_op_new(int op, cgen_ctx_t *ctx)
{
   vcode_reg_t result = vcode_get_result(op);
   const char *name = cgen_reg_name(result);

   LLVMTypeRef lltype = cgen_type(vtype_pointed(vcode_reg_type(result)));

   if (vcode_count_args(op) == 0)
      ctx->regs[result] = LLVMBuildMalloc(builder, lltype, name);
   else {
      LLVMValueRef length = cgen_get_arg(op, 0, ctx);
      ctx->regs[result] = LLVMBuildArrayMalloc(builder, lltype, length, name);
   }
}

static void cgen_op_all(int op, cgen_ctx_t *ctx)
{
   vcode_reg_t result = vcode_get_result(op);
   ctx->regs[result] = cgen_get_arg(op, 0, ctx);
}

static void cgen_op_null_check(int op, cgen_ctx_t *ctx)
{
   LLVMValueRef ptr = cgen_get_arg(op, 0, ctx);
   LLVMValueRef null = LLVMBuildICmp(builder, LLVMIntEQ, ptr,
                                     LLVMConstNull(LLVMTypeOf(ptr)), "null");

   LLVMBasicBlockRef null_bb = LLVMAppendBasicBlock(ctx->fn, "all_null");
   LLVMBasicBlockRef ok_bb   = LLVMAppendBasicBlock(ctx->fn, "all_ok");

   LLVMBuildCondBr(builder, null, null_bb, ok_bb);

   LLVMPositionBuilderAtEnd(builder, null_bb);

   LLVMValueRef args[] = {
      cgen_location(ctx)
   };
   LLVMBuildCall(builder, llvm_fn("_null_deref"), args, ARRAY_LEN(args), "");
   LLVMBuildUnreachable(builder);

   LLVMPositionBuilderAtEnd(builder, ok_bb);
}

static void cgen_op_deallocate(int op, cgen_ctx_t *ctx)
{
   LLVMValueRef ptr = cgen_get_arg(op, 0, ctx);
   LLVMValueRef access = LLVMBuildLoad(builder, ptr, "");
   LLVMBuildFree(builder, access);
   LLVMBuildStore(builder, LLVMConstNull(LLVMTypeOf(access)), ptr);
}

static void cgen_op_const_real(int op, cgen_ctx_t *ctx)
{
   vcode_reg_t result = vcode_get_result(op);
   ctx->regs[result] = LLVMConstReal(cgen_type(vcode_reg_type(result)),
                                     vcode_get_real(op));
}

static void cgen_op_value(int op, cgen_ctx_t *ctx)
{
   vcode_reg_t result = vcode_get_result(op);

   LLVMValueRef image_map;
   if (vcode_count_args(op) > 2)
      image_map = cgen_get_arg(op, 2, ctx);
   else {
      const bool real = vcode_reg_kind(result) == VCODE_TYPE_REAL;
      image_map = LLVMBuildAlloca(builder, llvm_image_map(), "");
      LLVMBuildStore(
         builder,
         LLVMBuildInsertValue(builder,
                              LLVMGetUndef(llvm_image_map()),
                              llvm_int32(real ? IMAGE_REAL : IMAGE_INTEGER),
                              0, ""),
         image_map);
   }

   LLVMValueRef args[] = {
      cgen_get_arg(op, 0, ctx),
      cgen_get_arg(op, 1, ctx),
      image_map,
      cgen_location(ctx)
   };
   ctx->regs[result] = LLVMBuildCall(builder, llvm_fn("_value_attr"),
                                      args, ARRAY_LEN(args), "value");
}

static void cgen_op_needs_last_value(int op, cgen_ctx_t *ctx)
{
   vcode_signal_t sig = vcode_get_signal(op);

   char *buf LOCAL = cgen_signal_nets_name(sig);
   LLVMValueRef nets_array = LLVMGetNamedGlobal(module, buf);
   assert(nets_array);

   LLVMValueRef nets = cgen_array_pointer(nets_array);

   LLVMValueRef args[] = {
      nets,
      llvm_int32(vcode_signal_count_nets(sig))
   };
   LLVMBuildCall(builder, llvm_fn("_needs_last_value"),
                 args, ARRAY_LEN(args), "");
}

static void cgen_op_bit_shift(int op, cgen_ctx_t *ctx)
{
   LLVMValueRef tmp = LLVMBuildAlloca(builder,
                                      llvm_uarray_type(LLVMInt1Type(), 1),
                                      "bit_shift");

   LLVMValueRef data  = cgen_get_arg(op, 0, ctx);
   LLVMValueRef len   = cgen_get_arg(op, 1, ctx);
   LLVMValueRef dir   = cgen_get_arg(op, 2, ctx);
   LLVMValueRef shift = cgen_get_arg(op, 3, ctx);

   LLVMValueRef args[] = {
      llvm_int32(vcode_get_subkind(op)),
      data,
      len,
      dir,
      shift,
      tmp
   };
   LLVMBuildCall(builder, llvm_fn("_bit_shift"), args, ARRAY_LEN(args), "");

   vcode_reg_t result = vcode_get_result(op);
   ctx->regs[result] = LLVMBuildLoad(builder, tmp, cgen_reg_name(result));
}

static void cgen_op_bit_vec_op(int op, cgen_ctx_t *ctx)
{
   LLVMValueRef tmp = LLVMBuildAlloca(builder,
                                      llvm_uarray_type(LLVMInt1Type(), 1),
                                      "bit_vec_op");

   LLVMValueRef left_data = cgen_get_arg(op, 0, ctx);
   LLVMValueRef left_len  = cgen_get_arg(op, 1, ctx);
   LLVMValueRef left_dir  = cgen_get_arg(op, 2, ctx);

   LLVMValueRef right_data = NULL, right_len = NULL, right_dir = NULL;
   if (vcode_count_args(op) == 6) {
      right_data = cgen_get_arg(op, 3, ctx);
      right_len  = cgen_get_arg(op, 4, ctx);
      right_dir  = cgen_get_arg(op, 5, ctx);
   }
   else {
      right_data = LLVMConstNull(LLVMPointerType(LLVMInt1Type(), 0));
      right_len  = llvm_int32(0);
      right_dir  = llvm_int1(0);
   }

   LLVMValueRef args[] = {
      llvm_int32(vcode_get_subkind(op)),
      left_data,
      left_len,
      left_dir,
      right_data,
      right_len,
      right_dir,
      tmp
   };
   LLVMBuildCall(builder, llvm_fn("_bit_vec_op"), args, ARRAY_LEN(args), "");

   vcode_reg_t result = vcode_get_result(op);
   ctx->regs[result] = LLVMBuildLoad(builder, tmp, cgen_reg_name(result));
}

static void cgen_op_array_size(int op, cgen_ctx_t *ctx)
{
   LLVMValueRef llen = cgen_get_arg(op, 0, ctx);
   LLVMValueRef rlen = cgen_get_arg(op, 1, ctx);

   LLVMValueRef ok = LLVMBuildICmp(builder, LLVMIntEQ, llen, rlen, "ok");

   LLVMBasicBlockRef pass_bb  = LLVMAppendBasicBlock(ctx->fn, "bounds_pass");
   LLVMBasicBlockRef fail_bb  = LLVMAppendBasicBlock(ctx->fn, "bounds_fail");

   LLVMBuildCondBr(builder, ok, pass_bb, fail_bb);

   LLVMPositionBuilderAtEnd(builder, fail_bb);

   LLVMValueRef args[] = {
      llvm_int32(0),
      llen,
      rlen,
      llvm_int32(BOUNDS_ARRAY_SIZE),
      cgen_location(ctx),
      LLVMConstNull(llvm_char_ptr())
   };

   LLVMBuildCall(builder, llvm_fn("_bounds_fail"), args, ARRAY_LEN(args), "");

   LLVMBuildUnreachable(builder);

   LLVMPositionBuilderAtEnd(builder, pass_bb);
}

static void cgen_op_index_check(int op, cgen_ctx_t *ctx)
{
   LLVMTypeRef int32 = LLVMInt32Type();

   LLVMValueRef min, max;
   if (vcode_count_args(op) == 2) {
      vcode_type_t bounds = vcode_get_type(op);
      min = llvm_int32(vtype_low(bounds));
      max = llvm_int32(vtype_high(bounds));
   }
   else {
      min = LLVMBuildZExt(builder, cgen_get_arg(op, 2, ctx), int32, "");
      max = LLVMBuildZExt(builder, cgen_get_arg(op, 3, ctx), int32, "");
   }

   LLVMValueRef low  = cgen_get_arg(op, 0, ctx);
   LLVMValueRef high = cgen_get_arg(op, 1, ctx);

   LLVMValueRef null = LLVMBuildICmp(builder, LLVMIntSLT, high, low, "null");

   for (int i = 0; i < 2; i++) {
      LLVMValueRef value =
         LLVMBuildZExt(builder, cgen_get_arg(op, i, ctx), int32, "");

      LLVMValueRef above =
         LLVMBuildICmp(builder, LLVMIntSGE, value, min, "above");
      LLVMValueRef below =
         LLVMBuildICmp(builder, LLVMIntSLE, value, max, "below");

      LLVMValueRef in =
         LLVMBuildOr(builder, LLVMBuildAnd(builder, above, below, ""),
                     null, "in");

      LLVMBasicBlockRef pass_bb  = LLVMAppendBasicBlock(ctx->fn, "bounds_pass");
      LLVMBasicBlockRef fail_bb  = LLVMAppendBasicBlock(ctx->fn, "bounds_fail");

      LLVMBuildCondBr(builder, in, pass_bb, fail_bb);

      LLVMPositionBuilderAtEnd(builder, fail_bb);

      LLVMValueRef args[] = {
         value,
         min,
         max,
         llvm_int32(vcode_get_subkind(op)),
         cgen_location(ctx),
         LLVMConstNull(llvm_char_ptr())
      };

      LLVMBuildCall(builder, llvm_fn("_bounds_fail"), args,
                    ARRAY_LEN(args), "");

      LLVMBuildUnreachable(builder);

      LLVMPositionBuilderAtEnd(builder, pass_bb);
   }
}

static void cgen_op_debug_out(int op, cgen_ctx_t *ctx)
{
   LLVMValueRef arg0 = cgen_get_arg(op, 0, ctx);

   if (LLVMGetTypeKind(LLVMTypeOf(arg0)) == LLVMPointerTypeKind) {
      LLVMValueRef args[] = {
         llvm_void_cast(arg0),
         llvm_int32(32)
      };
      LLVMBuildCall(builder, llvm_fn("_debug_dump"),
                    args, ARRAY_LEN(args), "");
   }
   else {
      LLVMValueRef args[] = {
         LLVMBuildZExt(builder, arg0, LLVMInt32Type(), ""),
         llvm_int32(vcode_get_arg(op, 0))
      };
      LLVMBuildCall(builder, llvm_fn("_debug_out"),
                    args, ARRAY_LEN(args), "");
   }
}

static void cgen_op_cover_stmt(int op, cgen_ctx_t *ctx)
{
   const uint32_t cover_tag = vcode_get_tag(op);

   LLVMValueRef cover_counts = LLVMGetNamedGlobal(module, "cover_stmts");

   LLVMValueRef indexes[] = { llvm_int32(0), llvm_int32(cover_tag) };
   LLVMValueRef count_ptr = LLVMBuildGEP(builder, cover_counts,
                                         indexes, ARRAY_LEN(indexes), "");

   LLVMValueRef count = LLVMBuildLoad(builder, count_ptr, "cover_count");
   LLVMValueRef count1 = LLVMBuildAdd(builder, count, llvm_int32(1), "");

   LLVMBuildStore(builder, count1, count_ptr);
}

static void cgen_op_cover_cond(int op, cgen_ctx_t *ctx)
{
   const uint32_t cover_tag = vcode_get_tag(op);
   const int sub_cond  = vcode_get_subkind(op);

   LLVMValueRef cover_conds = LLVMGetNamedGlobal(module, "cover_conds");

   LLVMValueRef indexes[] = { llvm_int32(0), llvm_int32(cover_tag) };
   LLVMValueRef mask_ptr = LLVMBuildGEP(builder, cover_conds,
                                        indexes, ARRAY_LEN(indexes), "");

   LLVMValueRef mask = LLVMBuildLoad(builder, mask_ptr, "cover_conds");

   // Bit zero means evaluated false, bit one means evaluated true
   // Other bits may be used in the future for sub-conditions

   LLVMValueRef or = LLVMBuildSelect(builder, cgen_get_arg(op, 0, ctx),
                                     llvm_int32(1 << ((sub_cond * 2) + 1)),
                                     llvm_int32(1 << (sub_cond * 2)),
                                     "cond_mask_or");

   LLVMValueRef mask1 = LLVMBuildOr(builder, mask, or, "");

   LLVMBuildStore(builder, mask1, mask_ptr);
}

static void cgen_op_heap_save(int op, cgen_ctx_t *ctx)
{
   LLVMValueRef cur_ptr = LLVMGetNamedGlobal(module, "_tmp_alloc");

   vcode_reg_t result = vcode_get_result(op);
   ctx->regs[result] = LLVMBuildLoad(builder, cur_ptr, cgen_reg_name(result));
}

static void cgen_op_heap_restore(int op, cgen_ctx_t *ctx)
{
   LLVMValueRef cur_ptr = LLVMGetNamedGlobal(module, "_tmp_alloc");
   LLVMBuildStore(builder, cgen_get_arg(op, 0, ctx), cur_ptr);
}

static void cgen_op_debug_info(int op, cgen_ctx_t *ctx)
{
   // TODO: emit LLVM debug info here
   ctx->last_loc = *vcode_get_loc(op);
}

static void cgen_op_addi(int op, cgen_ctx_t *ctx)
{
   vcode_reg_t result = vcode_get_result(op);
   vtype_kind_t kind = vtype_kind(vcode_reg_type(result));

   LLVMValueRef lhs = cgen_get_arg(op, 0, ctx);
   int64_t rhs = vcode_get_value(op);

   if (kind == VCODE_TYPE_POINTER || kind == VCODE_TYPE_SIGNAL) {
      LLVMValueRef index[] = { llvm_int32(rhs) };
      ctx->regs[result] = LLVMBuildGEP(builder, lhs,
                                       index, ARRAY_LEN(index),
                                       cgen_reg_name(result));
   }
   else if (vcode_reg_kind(result) == VCODE_TYPE_REAL)
      ctx->regs[result] =
         LLVMBuildFAdd(builder, lhs,
                       LLVMConstReal(cgen_type(vcode_reg_type(result)),
                                     vcode_get_real(op)),
                       cgen_reg_name(result));
   else
      ctx->regs[result] =
         LLVMBuildAdd(builder, lhs,
                      LLVMConstInt(cgen_type(vcode_reg_type(result)),
                                   vcode_get_value(op), false),
                      cgen_reg_name(result));
}

static void cgen_op_range_null(int op, cgen_ctx_t *ctx)
{
   vcode_reg_t result = vcode_get_result(op);

   LLVMValueRef left = cgen_get_arg(op, 0, ctx);
   LLVMValueRef right = cgen_get_arg(op, 1, ctx);
   LLVMValueRef dir = cgen_get_arg(op, 2, ctx);

   LLVMValueRef cmp_to = LLVMBuildICmp(builder, LLVMIntSGT,
                                       left, right, "cmp_to");
   LLVMValueRef cmp_downto = LLVMBuildICmp(builder, LLVMIntSGT,
                                           right, left, "cmp_downto");

   ctx->regs[result] =
      LLVMBuildSelect(builder, dir, cmp_downto, cmp_to, cgen_reg_name(result));

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
      cgen_op_fcall(i, false, ctx);
      break;
   case VCODE_OP_NESTED_FCALL:
      cgen_op_fcall(i, true, ctx);
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
   case VCODE_OP_SELECT:
      cgen_op_select(i, ctx);
      break;
   case VCODE_OP_UARRAY_LEFT:
      cgen_op_uarray_left(i, ctx);
      break;
   case VCODE_OP_UARRAY_RIGHT:
      cgen_op_uarray_right(i, ctx);
      break;
   case VCODE_OP_UARRAY_DIR:
      cgen_op_uarray_dir(i, ctx);
      break;
   case VCODE_OP_DIV:
      cgen_op_div(i, ctx);
      break;
   case VCODE_OP_NEG:
      cgen_op_neg(i, ctx);
      break;
   case VCODE_OP_EXP:
      cgen_op_exp(i, ctx);
      break;
   case VCODE_OP_ABS:
      cgen_op_abs(i, ctx);
      break;
   case VCODE_OP_REM:
      cgen_op_rem(i, ctx);
      break;
   case VCODE_OP_MOD:
      cgen_op_mod(i, ctx);
      break;
   case VCODE_OP_PARAM_UPREF:
      cgen_op_param_upref(i, ctx);
      break;
   case VCODE_OP_VAR_UPREF:
      cgen_op_var_upref(i, ctx);
      break;
   case VCODE_OP_RESOLVED_ADDRESS:
      cgen_op_resolved_address(i, ctx);
      break;
   case VCODE_OP_NETS:
      cgen_op_nets(i, ctx);
      break;
   case VCODE_OP_SCHED_WAVEFORM:
      cgen_op_sched_waveform(i, ctx);
      break;
   case VCODE_OP_SET_INITIAL:
      cgen_op_set_initial(i, ctx);
      break;
   case VCODE_OP_ALLOC_DRIVER:
      cgen_op_alloc_driver(i, ctx);
      break;
   case VCODE_OP_ACTIVE:
      cgen_op_active(i, ctx);
      break;
   case VCODE_OP_EVENT:
      cgen_op_event(i, ctx);
      break;
   case VCODE_OP_CONST_RECORD:
      cgen_op_const_record(i, ctx);
      break;
   case VCODE_OP_COPY:
      cgen_op_copy(i, ctx);
      break;
   case VCODE_OP_RECORD_REF:
      cgen_op_record_ref(i, ctx);
      break;
   case VCODE_OP_SCHED_EVENT:
      cgen_op_sched_event(i, ctx);
      break;
   case VCODE_OP_PCALL:
      cgen_op_pcall(i, false, ctx);
      break;
   case VCODE_OP_NESTED_PCALL:
      cgen_op_pcall(i, true, ctx);
      break;
   case VCODE_OP_RESUME:
      cgen_op_resume(i, false, ctx);
      break;
   case VCODE_OP_NESTED_RESUME:
      cgen_op_resume(i, true, ctx);
      break;
   case VCODE_OP_MEMCMP:
      cgen_op_memcmp(i, ctx);
      break;
   case VCODE_OP_XNOR:
      cgen_op_xnor(i, ctx);
      break;
   case VCODE_OP_XOR:
      cgen_op_xor(i, ctx);
      break;
   case VCODE_OP_MEMSET:
      cgen_op_memset(i, ctx);
      break;
   case VCODE_OP_VEC_LOAD:
      cgen_op_vec_load(i, ctx);
      break;
   case VCODE_OP_CASE:
      cgen_op_case(i, ctx);
      break;
   case VCODE_OP_FILE_OPEN:
      cgen_op_file_open(i, ctx);
      break;
   case VCODE_OP_FILE_WRITE:
      cgen_op_file_write(i, ctx);
      break;
   case VCODE_OP_FILE_CLOSE:
      cgen_op_file_close(i, ctx);
      break;
   case VCODE_OP_FILE_READ:
      cgen_op_file_read(i, ctx);
      break;
   case VCODE_OP_ENDFILE:
      cgen_op_endfile(i, ctx);
      break;
   case VCODE_OP_NULL:
      cgen_op_null(i, ctx);
      break;
   case VCODE_OP_NEW:
      cgen_op_new(i, ctx);
      break;
   case VCODE_OP_ALL:
      cgen_op_all(i, ctx);
      break;
   case VCODE_OP_NULL_CHECK:
      cgen_op_null_check(i, ctx);
      break;
   case VCODE_OP_DEALLOCATE:
      cgen_op_deallocate(i, ctx);
      break;
   case VCODE_OP_CONST_REAL:
      cgen_op_const_real(i, ctx);
      break;
   case VCODE_OP_VALUE:
      cgen_op_value(i, ctx);
      break;
   case VCODE_OP_LAST_EVENT:
      cgen_op_last_event(i, ctx);
      break;
   case VCODE_OP_NEEDS_LAST_VALUE:
      cgen_op_needs_last_value(i, ctx);
      break;
   case VCODE_OP_BIT_VEC_OP:
      cgen_op_bit_vec_op(i, ctx);
      break;
   case VCODE_OP_DYNAMIC_BOUNDS:
      cgen_op_dynamic_bounds(i, ctx);
      break;
   case VCODE_OP_ARRAY_SIZE:
      cgen_op_array_size(i, ctx);
      break;
   case VCODE_OP_INDEX_CHECK:
      cgen_op_index_check(i, ctx);
      break;
   case VCODE_OP_BIT_SHIFT:
      cgen_op_bit_shift(i, ctx);
      break;
   case VCODE_OP_DEBUG_OUT:
      cgen_op_debug_out(i, ctx);
      break;
   case VCODE_OP_COVER_STMT:
      cgen_op_cover_stmt(i, ctx);
      break;
   case VCODE_OP_COVER_COND:
      cgen_op_cover_cond(i, ctx);
      break;
   case VCODE_OP_UARRAY_LEN:
      cgen_op_uarray_len(i, ctx);
      break;
   case VCODE_OP_HEAP_SAVE:
      cgen_op_heap_save(i, ctx);
      break;
   case VCODE_OP_HEAP_RESTORE:
      cgen_op_heap_restore(i, ctx);
      break;
   case VCODE_OP_NAND:
      cgen_op_nand(i, ctx);
      break;
   case VCODE_OP_NOR:
      cgen_op_nor(i, ctx);
      break;
   case VCODE_OP_IMAGE_MAP:
      cgen_op_image_map(i, ctx);
      break;
   case VCODE_OP_DEBUG_INFO:
      cgen_op_debug_info(i, ctx);
      break;
   case VCODE_OP_ADDI:
      cgen_op_addi(i, ctx);
      break;
   case VCODE_OP_RANGE_NULL:
      cgen_op_range_null(i, ctx);
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
   if (nops > 0) {
      for (int i = 0; i < nops; i++)
         cgen_op(i, ctx);
   }
   else
      LLVMBuildUnreachable(builder);
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
      char name[32];
      checked_sprintf(name, sizeof(name), "vcode_block_%d", i);
      ctx->blocks[i] = LLVMAppendBasicBlock(ctx->fn, name);
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
   for (int i = 0; i < nparams; i++) {
      LLVMValueRef param = LLVMGetParam(ctx->fn, i);
      if (vtype_kind(vcode_param_type(i)) == VCODE_TYPE_UARRAY) {
         LLVMPositionBuilderAtEnd(builder, LLVMGetFirstBasicBlock(ctx->fn));
         ctx->regs[vcode_param_reg(i)] = LLVMBuildLoad(builder, param, "");
      }
      else
         ctx->regs[vcode_param_reg(i)] = param;
   }
}

static void cgen_locals(cgen_ctx_t *ctx)
{
   LLVMPositionBuilderAtEnd(builder, ctx->blocks[0]);

   const int nvars = vcode_count_vars();
   for (int i = 0; i < nvars; i++) {
      vcode_var_t var = vcode_var_handle(i);
      LLVMTypeRef lltype = cgen_type(vcode_var_type(var));
      const char *name = istr(vcode_var_name(var));

      if (vcode_var_use_heap(var))
         ctx->locals[i] = cgen_tmp_alloc(llvm_sizeof(lltype), lltype);
      else
         ctx->locals[i] = LLVMBuildAlloca(builder, lltype, name);
   }
}

static void cgen_function(LLVMTypeRef display_type)
{
   assert(vcode_unit_kind() == VCODE_UNIT_FUNCTION);

   const int nparams = vcode_count_params();
   vcode_type_t params[nparams];
   for (int i = 0; i < nparams; i++)
      params[i] = vcode_param_type(i);

   LLVMValueRef fn = cgen_signature(vcode_unit_name(), vcode_unit_result(),
                                    display_type, params, nparams);
   cgen_add_func_attr(fn, FUNC_ATTR_UWTABLE, -1);

   const bool pure =
      display_type == NULL
      && vcode_unit_result() != VCODE_INVALID_TYPE
      && vcode_unit_pure();

   if (pure)
      cgen_add_func_attr(fn, FUNC_ATTR_READONLY, -1);

   cgen_ctx_t ctx = {
      .fn = fn
   };
   cgen_alloc_context(&ctx);

   if (display_type != NULL)
      ctx.display = LLVMGetParam(fn, vcode_count_params());

   cgen_params(&ctx);
   cgen_locals(&ctx);
   cgen_code(&ctx);
   cgen_free_context(&ctx);
}

static LLVMTypeRef cgen_state_type(cgen_ctx_t *ctx)
{
   const bool is_procedure = vcode_unit_kind() == VCODE_UNIT_PROCEDURE;

   const int nparams = is_procedure ? vcode_count_params() : 0;
   const int nvars   = vcode_count_vars();
   const int nfields = nvars + nparams + 2;

   LLVMTypeRef fields[nfields];
   fields[0] = LLVMInt32Type();
   fields[1] = llvm_void_ptr();

   ctx->var_base = ctx->param_base = 2;

   if (is_procedure) {
      for (int i = 0; i < nparams; i++)
         fields[ctx->param_base + i] = cgen_type(vcode_param_type(i));

      ctx->var_base += nparams;
   }

   for (int i = 0; i < nvars; i++) {
      vcode_var_t var = vcode_var_handle(i);
      fields[ctx->var_base + i] = cgen_type(vcode_var_type(var));
   }

   return LLVMStructType(fields, nfields, false);
}

static void cgen_state_struct(cgen_ctx_t *ctx)
{
   char *name LOCAL = xasprintf("%s__state", istr(vcode_unit_name()));
   LLVMTypeRef state_ty = cgen_state_type(ctx);
   ctx->state = LLVMAddGlobal(module, state_ty, safe_symbol(name));
   LLVMSetLinkage(ctx->state, LLVMInternalLinkage);
   LLVMSetInitializer(ctx->state, LLVMGetUndef(state_ty));
}

static void cgen_jump_table(cgen_ctx_t *ctx)
{
   assert(ctx->state != NULL);

   const int first = vcode_unit_kind() == VCODE_UNIT_PROCESS ? 1 : 0;

   LLVMValueRef state_ptr = LLVMBuildStructGEP(builder, ctx->state, 0, "");
   LLVMValueRef jtarget = LLVMBuildLoad(builder, state_ptr, "");
   LLVMValueRef jswitch = LLVMBuildSwitch(builder, jtarget,
                                          ctx->blocks[first], 10);

   const int nblocks = vcode_count_blocks();
   bool *have LOCAL = xcalloc(sizeof(bool) * nblocks);
   for (int i = 0; i < nblocks; i++) {
      vcode_select_block(i);

      const int last = vcode_count_ops() - 1;
      if (last < 0)
         continue;

      vcode_op_t last_op = vcode_get_op(last);
      if (last_op != VCODE_OP_WAIT && last_op != VCODE_OP_PCALL
          && last_op != VCODE_OP_NESTED_PCALL)
         continue;

      const vcode_block_t target = vcode_get_target(last, 0);
      if (have[target])
         continue;

      LLVMAddCase(jswitch, llvm_int32(target), ctx->blocks[target]);
      have[target] = true;
   }
}

static void cgen_procedure(LLVMTypeRef display_type)
{
   assert(vcode_unit_kind() == VCODE_UNIT_PROCEDURE);

   const int nparams = vcode_count_params();
   vcode_type_t params[nparams];
   for (int i = 0; i < nparams; i++)
      params[i] = vcode_param_type(i);

   LLVMValueRef fn = cgen_signature(vcode_unit_name(), VCODE_INVALID_TYPE,
                                    display_type, params, nparams);
   cgen_add_func_attr(fn, FUNC_ATTR_UWTABLE, -1);

   cgen_ctx_t ctx = {
      .fn = fn
   };

   if (display_type != NULL)
      ctx.display = LLVMGetParam(fn, nparams);

   LLVMBasicBlockRef entry_bb = LLVMAppendBasicBlock(fn, "entry");
   LLVMBasicBlockRef alloc_bb = LLVMAppendBasicBlock(fn, "alloc");
   LLVMBasicBlockRef jump_bb  = LLVMAppendBasicBlock(fn, "jump_table");

   cgen_alloc_context(&ctx);
   cgen_params(&ctx);

   LLVMPositionBuilderAtEnd(builder, entry_bb);

   const int state_param = nparams + (display_type == NULL ? 0 : 1);

   LLVMTypeRef state_type = cgen_state_type(&ctx);
   LLVMTypeRef pointer_type = LLVMPointerType(state_type, 0);
   LLVMValueRef old_state =
      LLVMBuildPointerCast(builder, LLVMGetParam(fn, state_param),
                           pointer_type, "old_state");

   LLVMValueRef is_null = LLVMBuildICmp(builder, LLVMIntEQ, old_state,
                                        LLVMConstNull(pointer_type), "");
   LLVMBuildCondBr(builder, is_null, alloc_bb, jump_bb);

   LLVMPositionBuilderAtEnd(builder, alloc_bb);

   LLVMValueRef new_state = LLVMBuildMalloc(builder, state_type, "new_state");

   LLVMValueRef state_ptr = LLVMBuildStructGEP(builder, new_state, 0, "");
   LLVMBuildStore(builder, llvm_int32(0), state_ptr);

   for (int i = 0; i < nparams; i++) {
      LLVMValueRef param_ptr =
         LLVMBuildStructGEP(builder, new_state, ctx.param_base + i, "");
      LLVMBuildStore(builder, ctx.regs[vcode_param_reg(i)], param_ptr);
   }

   LLVMBuildBr(builder, jump_bb);

   LLVMPositionBuilderAtEnd(builder, jump_bb);

   ctx.state = LLVMBuildPhi(builder, pointer_type, "state");

   LLVMValueRef phi_values[]   = { old_state, new_state };
   LLVMBasicBlockRef phi_bbs[] = { entry_bb,  alloc_bb  };
   LLVMAddIncoming(ctx.state, phi_values, phi_bbs, 2);

   for (int i = 0; i < nparams; i++) {
      LLVMValueRef param_ptr =
         LLVMBuildStructGEP(builder, ctx.state, ctx.param_base + i, "");
      ctx.regs[vcode_param_reg(i)] = LLVMBuildLoad(builder, param_ptr, "");
   }

   cgen_jump_table(&ctx);
   cgen_code(&ctx);
   cgen_free_context(&ctx);
}

static void cgen_process(vcode_unit_t code)
{
   vcode_select_unit(code);
   assert(vcode_unit_kind() == VCODE_UNIT_PROCESS);

   LLVMTypeRef pargs[] = { LLVMInt32Type() };
   LLVMTypeRef ftype = LLVMFunctionType(LLVMVoidType(), pargs, 1, false);
   const char *name = safe_symbol(istr(vcode_unit_name()));
   LLVMValueRef fn = LLVMAddFunction(module, name, ftype);
   cgen_add_func_attr(fn, FUNC_ATTR_NOUNWIND, -1);
   cgen_add_func_attr(fn, FUNC_ATTR_DLLEXPORT, -1);
   cgen_add_func_attr(fn, FUNC_ATTR_UWTABLE, -1);

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

   LLVMValueRef pcall_ptr = LLVMBuildStructGEP(builder, ctx.state, 1, "");
   LLVMBuildStore(builder, LLVMConstNull(llvm_void_ptr()), pcall_ptr);

   // Schedule the process to run immediately
   cgen_sched_process(llvm_int64(0));

   LLVMBuildBr(builder, ctx.blocks[0]);

   LLVMPositionBuilderAtEnd(builder, jump_bb);
   cgen_jump_table(&ctx);

   cgen_code(&ctx);
   cgen_free_context(&ctx);
}

static void cgen_net_mapping_table(vcode_signal_t sig, int offset,
                                   netid_t first, netid_t last, LLVMValueRef fn)
{
   LLVMValueRef nets = cgen_signal_nets(sig);

   LLVMValueRef i = LLVMBuildAlloca(builder, cgen_net_id_type(), "i");
   LLVMBuildStore(builder, llvm_int32(first), i);

   LLVMBasicBlockRef test_bb = LLVMAppendBasicBlock(fn, "nm_test");
   LLVMBasicBlockRef body_bb = LLVMAppendBasicBlock(fn, "nm_body");
   LLVMBasicBlockRef exit_bb = LLVMAppendBasicBlock(fn, "nm_exit");

   LLVMBuildBr(builder, test_bb);

   // Loop test

   LLVMPositionBuilderAtEnd(builder, test_bb);

   LLVMValueRef i_loaded = LLVMBuildLoad(builder, i, "i");
   LLVMValueRef done = LLVMBuildICmp(builder, LLVMIntUGT, i_loaded,
                                     llvm_int32(last), "done");
   LLVMBuildCondBr(builder, done, exit_bb, body_bb);

   // Loop body

   LLVMPositionBuilderAtEnd(builder, body_bb);

   LLVMValueRef indexes[] = {
      LLVMBuildAdd(builder,
                   LLVMBuildSub(builder, i_loaded, llvm_int32(first), ""),
                   llvm_int32(offset), "")
   };
   LLVMValueRef ptr = LLVMBuildGEP(builder, nets,
                                   indexes, ARRAY_LEN(indexes), "ptr");
   LLVMBuildStore(builder, i_loaded, ptr);

   LLVMValueRef i_plus_1 = LLVMBuildAdd(builder, i_loaded, llvm_int32(1), "");
   LLVMBuildStore(builder, i_plus_1, i);

   LLVMBuildBr(builder, test_bb);

   // Epilogue

   LLVMPositionBuilderAtEnd(builder, exit_bb);
}

static void cgen_reset_function(tree_t top)
{
   char *name LOCAL = xasprintf("%s_reset", istr(vcode_unit_name()));
   LLVMValueRef fn =
      LLVMAddFunction(module, name,
                      LLVMFunctionType(LLVMVoidType(), NULL, 0, false));
   cgen_add_func_attr(fn, FUNC_ATTR_DLLEXPORT, -1);
   cgen_add_func_attr(fn, FUNC_ATTR_UWTABLE, -1);

   LLVMBasicBlockRef init_bb = NULL;

   const int nsignals = vcode_count_signals();
   for (int i = 0 ; i < nsignals; i++) {
      const int nnets = vcode_signal_count_nets(i);
      if (nnets <= MAX_STATIC_NETS && (!vcode_signal_extern(i) || nnets == 0))
         continue;

      // Need to generate runtime code to fill in net mapping table

      if (init_bb == NULL) {
         init_bb = LLVMAppendBasicBlock(fn, "signal_net_init");
         LLVMPositionBuilderAtEnd(builder, init_bb);
      }

      const netid_t *nets = vcode_signal_nets(i);
      netid_t first = nets[0];
      int     off   = 0;
      netid_t last  = first;
      for (int i = 1; i < nnets; i++) {
         const netid_t this = nets[i];
         if (this != last + 1) {
            cgen_net_mapping_table(i, off, first, last, fn);
            first = last = this;
            off = i;
         }
         else
            last = this;
      }
      cgen_net_mapping_table(i, off, first, last, fn);
   }

   cgen_ctx_t ctx = {
      .fn = fn
   };
   cgen_alloc_context(&ctx);
   if (init_bb != NULL)
      LLVMBuildBr(builder, ctx.blocks[0]);
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
      cgen_add_func_attr(var, FUNC_ATTR_DLLEXPORT, -1);
   }

   const int cond_tags = tree_attr_int(t, ident_new("cond_tags"), 0);
   if (cond_tags > 0) {
      LLVMTypeRef type = LLVMArrayType(LLVMInt32Type(), stmt_tags);
      LLVMValueRef var = LLVMAddGlobal(module, type, "cover_conds");
      LLVMSetInitializer(var, LLVMGetUndef(type));
      cgen_add_func_attr(var, FUNC_ATTR_DLLEXPORT, -1);
   }
}

static void cgen_subprograms(vcode_unit_t vcode)
{
   vcode_select_unit(vcode);

   LLVMTypeRef display = NULL;
   const vunit_kind_t scope_kind = vcode_unit_kind();
   const bool needs_display = scope_kind != VCODE_UNIT_CONTEXT;

   for (vcode_unit_t it = vcode_unit_child(vcode);
        it != NULL;
        it = vcode_unit_next(it)) {

      vcode_select_unit(it);

      switch (vcode_unit_kind()) {
      case VCODE_UNIT_PROCEDURE:
      case VCODE_UNIT_FUNCTION:
         cgen_subprograms(it);
         if (display == NULL && needs_display)
            display = cgen_display_type(vcode);
         vcode_select_unit(it);
         if (vcode_unit_kind() == VCODE_UNIT_FUNCTION)
            cgen_function(display);
         else
            cgen_procedure(display);
         break;
      case VCODE_UNIT_PROCESS:
         cgen_subprograms(it);
         cgen_process(it);
         break;
      default:
         break;
      }
   }
}

static void cgen_shared_variables(void)
{
   const int nvars = vcode_count_vars();
   for (int i = 0; i < nvars; i++) {
      vcode_var_t var = vcode_var_handle(i);

      LLVMTypeRef type = cgen_type(vcode_var_type(var));
      const char *name = safe_symbol(istr(vcode_var_name(var)));
      LLVMValueRef global = LLVMAddGlobal(module, type, name);
      if (vcode_var_extern(var)) {
#ifdef IMPLIB_REQUIRED
         LLVMSetDLLStorageClass(global, LLVMDLLImportStorageClass);
#endif
         LLVMSetLinkage(global, LLVMExternalLinkage);
      }
      else {
#ifdef IMPLIB_REQUIRED
         LLVMSetDLLStorageClass(global, LLVMDLLExportStorageClass);
#endif
         LLVMSetInitializer(global, LLVMConstNull(type));
      }
   }
}

static void cgen_signals(void)
{
   const int nsignals = vcode_count_signals();
   for (int i = 0 ; i < nsignals; i++) {
      char *buf LOCAL = cgen_signal_nets_name(i);

      const int nnets     = vcode_signal_count_nets(i);
      const netid_t *nets = vcode_signal_nets(i);

      LLVMTypeRef nid_type = cgen_net_id_type();
      LLVMTypeRef map_type = LLVMArrayType(nid_type, nnets);

      LLVMValueRef map_var = LLVMAddGlobal(module, map_type, buf);
      if (vcode_signal_extern(i))
         LLVMSetLinkage(map_var, LLVMExternalLinkage);
      else {
         if (nnets <= MAX_STATIC_NETS && nnets > 0
             && nets[0] != NETID_INVALID) {
            // Generate a constant mapping table from sub-element to net ID
            LLVMSetGlobalConstant(map_var, true);
            LLVMSetUnnamedAddr(map_var, true);

            LLVMValueRef *init = xmalloc(sizeof(LLVMValueRef) * nnets);
            for (int i = 0; i < nnets; i++)
               init[i] = LLVMConstInt(nid_type, nets[i], false);

            LLVMSetInitializer(map_var, LLVMConstArray(nid_type, init, nnets));
            free(init);
         }
         else {
            // Values will be filled in by reset function
            LLVMSetInitializer(map_var, LLVMGetUndef(map_type));
         }

#ifdef IMPLIB_REQUIRED
         LLVMSetDLLStorageClass(map_var, LLVMDLLExportStorageClass);
#endif
      }
   }
}

static void cgen_top(tree_t t, vcode_unit_t vcode)
{
   vcode_select_unit(vcode);

   cgen_coverage_state(t);
   cgen_shared_variables();
   cgen_signals();
   cgen_reset_function(t);
   cgen_subprograms(vcode);
}

static void cgen_optimise(void)
{
   LLVMPassManagerRef pass_mgr = LLVMCreatePassManager();

   LLVMAddInstructionCombiningPass(pass_mgr);
   LLVMAddReassociatePass(pass_mgr);
   LLVMAddGVNPass(pass_mgr);
   LLVMAddCFGSimplificationPass(pass_mgr);

   LLVMPassManagerBuilderRef builder = LLVMPassManagerBuilderCreate();
   LLVMPassManagerBuilderSetOptLevel(builder, opt_get_int("optimise"));
   LLVMPassManagerBuilderPopulateModulePassManager(builder, pass_mgr);

   LLVMRunPassManager(pass_mgr, module);

   LLVMDisposePassManager(pass_mgr);
   LLVMPassManagerBuilderDispose(builder);
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
   else if (strcmp(name, "_sched_waveform_s") == 0) {
      LLVMTypeRef args[] = {
         llvm_void_ptr(),
         LLVMInt64Type(),
         LLVMInt64Type(),
         LLVMInt64Type()
      };
      fn = LLVMAddFunction(module, "_sched_waveform_s",
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
         LLVMPointerType(llvm_size_list_type(), 0),
         LLVMInt32Type(),
         LLVMPointerType(LLVMInt8Type(), 0)
      };
      fn = LLVMAddFunction(module, "_set_initial",
                           LLVMFunctionType(LLVMVoidType(),
                                            args, ARRAY_LEN(args), false));
   }
   else if (strcmp(name, "_set_initial_1") == 0) {
      LLVMTypeRef args[] = {
         LLVMInt32Type(),
         llvm_void_ptr(),
         LLVMInt32Type(),
         LLVMInt32Type(),
         llvm_void_ptr(),
         LLVMInt32Type(),
         LLVMPointerType(LLVMInt8Type(), 0)
      };
      fn = LLVMAddFunction(module, "_set_initial_1",
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
         LLVMInt8Type(),
         LLVMPointerType(llvm_rt_loc(), 0)
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
         LLVMInt32Type()
      };
      fn = LLVMAddFunction(module, "_vec_load",
                           LLVMFunctionType(llvm_void_ptr(),
                                            args, ARRAY_LEN(args), false));
   }
   else if (strcmp(name, "_image") == 0) {
      LLVMTypeRef args[] = {
         LLVMInt64Type(),
         LLVMPointerType(llvm_image_map(), 0),
         LLVMPointerType(llvm_uarray_type(LLVMInt8Type(), 1), 0)
      };
      fn = LLVMAddFunction(module, "_image",
                           LLVMFunctionType(LLVMVoidType(),
                                            args, ARRAY_LEN(args), false));
   }
   else if (strcmp(name, "_debug_out") == 0) {
      LLVMTypeRef args[] = {
         LLVMInt32Type(),
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
   else if (strcmp(name, "llvm.round.f64") == 0) {
      LLVMTypeRef args[] = {
         LLVMDoubleType()
      };
      fn = LLVMAddFunction(module, "llvm.round.f64",
                           LLVMFunctionType(LLVMDoubleType(),
                                            args, ARRAY_LEN(args), false));
   }
   else if (strcmp(name, "llvm.memset.p0i8.i32") == 0) {
      LLVMTypeRef args[] = {
         LLVMPointerType(LLVMInt8Type(), 0),
         LLVMInt8Type(),
         LLVMInt32Type(),
#if LLVM_INTRINSIC_ALIGN
         LLVMInt32Type(),
#endif
         LLVMInt1Type()
      };
      fn = LLVMAddFunction(module, "llvm.memset.p0i8.i32",
                           LLVMFunctionType(LLVMVoidType(),
                                            args, ARRAY_LEN(args), false));
   }
   else if (strncmp(name, "llvm.mem", 8) == 0) {
      int width;
      char kind[16];
      if (sscanf(name, "llvm.%16[^.].p0i%d", kind, &width) != 2)
         fatal("invalid memcpy intrinsic %s", name);

      LLVMTypeRef args[] = {
         LLVMPointerType(LLVMIntType(width), 0),
         LLVMPointerType(LLVMIntType(width), 0),
         LLVMInt32Type(),
#if LLVM_INTRINSIC_ALIGN
         LLVMInt32Type(),
#endif
         LLVMInt1Type()
      };
      fn = LLVMAddFunction(module, cgen_memcpy_name(kind, width),
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
         LLVMInt32Type(),
         LLVMInt32Type(),
         LLVMInt32Type(),
         LLVMPointerType(llvm_rt_loc(), 0),
         llvm_char_ptr()
      };
      fn = LLVMAddFunction(module, "_bounds_fail",
                           LLVMFunctionType(LLVMVoidType(),
                                            args, ARRAY_LEN(args), false));
      cgen_add_func_attr(fn, FUNC_ATTR_NORETURN, -1);
   }
   else if (strcmp(name, "_div_zero") == 0) {
      LLVMTypeRef args[] = {
         LLVMPointerType(llvm_rt_loc(), 0)
      };
      fn = LLVMAddFunction(module, "_div_zero",
                           LLVMFunctionType(LLVMVoidType(),
                                            args, ARRAY_LEN(args), false));
      cgen_add_func_attr(fn, FUNC_ATTR_NORETURN, -1);
   }
   else if (strcmp(name, "_null_deref") == 0) {
      LLVMTypeRef args[] = {
         LLVMPointerType(llvm_rt_loc(), 0)
      };
      fn = LLVMAddFunction(module, "_null_deref",
                           LLVMFunctionType(LLVMVoidType(),
                                            args, ARRAY_LEN(args), false));
      cgen_add_func_attr(fn, FUNC_ATTR_NORETURN, -1);
   }
   else if (strcmp(name, "_bit_shift") == 0) {
      LLVMTypeRef args[] = {
         LLVMInt32Type(),
         LLVMPointerType(LLVMInt1Type(), 0),
         LLVMInt32Type(),
         LLVMInt1Type(),
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
         LLVMInt1Type(),
         LLVMPointerType(LLVMInt1Type(), 0),
         LLVMInt32Type(),
         LLVMInt1Type(),
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
         LLVMPointerType(llvm_image_map(), 0),
         LLVMPointerType(llvm_rt_loc(), 0)
      };
      fn = LLVMAddFunction(module, "_value_attr",
                           LLVMFunctionType(LLVMInt64Type(),
                                            args, ARRAY_LEN(args), false));
   }
   else if (strcmp(name, "_private_stack") == 0) {
      fn = LLVMAddFunction(module, "_private_stack",
                           LLVMFunctionType(LLVMVoidType(),
                                            NULL, 0, false));
   }

   if (fn != NULL)
      cgen_add_func_attr(fn, FUNC_ATTR_NOUNWIND, -1);

   return fn;
}

static void cgen_tmp_stack(void)
{
   LLVMValueRef _tmp_stack =
      LLVMAddGlobal(module, llvm_void_ptr(), "_tmp_stack");
   LLVMSetLinkage(_tmp_stack, LLVMExternalLinkage);

   LLVMValueRef _tmp_alloc =
      LLVMAddGlobal(module, LLVMInt32Type(), "_tmp_alloc");
   LLVMSetLinkage(_tmp_alloc, LLVMExternalLinkage);
}

static void cgen_link_arg(const char *fmt, ...)
{
   va_list ap;
   va_start(ap, fmt);
   char *buf = xvasprintf(fmt, ap);
   va_end(ap);

   ARRAY_APPEND(link_args, buf, n_link_args, max_link_args);
}

#ifdef IMPLIB_REQUIRED
static void cgen_find_implib_deps(ident_t unit_name, ident_list_t **deps)
{
   lib_t lib = lib_find(ident_until(unit_name, '.'), true);
   tree_t unit = lib_get(lib, unit_name);

   if (unit == NULL)
      return;

   unit_name = tree_ident(unit);

   if (ident_list_find(*deps, unit_name))
      return;

   ident_list_push(deps, unit_name);

   switch (tree_kind(unit)) {
   case T_PACKAGE:
      {
         ident_t body_name = ident_prefix(unit_name, ident_new("body"), '-');
         cgen_find_implib_deps(body_name, deps);
      }
      break;

   case T_PACK_BODY:
      {
         ident_t pack_name = ident_until(unit_name, '-');
         cgen_find_implib_deps(pack_name, deps);
      }
      break;

   default:
      break;
   }

   const int ncontext = tree_contexts(unit);
   for (int i = 0; i < ncontext; i++) {
      tree_t c = tree_context(unit, i);
      if (tree_kind(c) != T_USE)
         continue;

      cgen_find_implib_deps(tree_ident(c), deps);
   }
}
#endif  // IMPLIB_REQUIRED

static void cgen_native(tree_t top, LLVMTargetMachineRef tm_ref)
{
   ident_t unit_name = tree_ident(top);
   char *obj_name LOCAL = xasprintf("_%s." LLVM_OBJ_EXT, istr(unit_name));

   char obj_path[PATH_MAX];
   lib_realpath(lib_work(), obj_name, obj_path, sizeof(obj_path));

   char *error;
   if (LLVMTargetMachineEmitToFile(tm_ref, module, obj_path,
                                   LLVMObjectFile, &error))
      fatal("Failed to write object file: %s", error);

   max_link_args = 64;
   link_args = xmalloc(sizeof(char *) * max_link_args);
   n_link_args = 0;

#ifdef LINKER_PATH
   cgen_link_arg("%s", LINKER_PATH);
#else
   cgen_link_arg("%s", SYSTEM_CC);
#endif

#if defined __APPLE__
   cgen_link_arg("-bundle");
   cgen_link_arg("-flat_namespace");
   cgen_link_arg("-undefined");
   cgen_link_arg("dynamic_lookup");
#else
   cgen_link_arg("-shared");
#endif

   char *fname LOCAL = xasprintf("_%s." DLL_EXT, istr(unit_name));
   char so_path[PATH_MAX];
   lib_realpath(lib_work(), fname, so_path, PATH_MAX);

   cgen_link_arg("-o");
   cgen_link_arg("%s", so_path);
   cgen_link_arg("%s", obj_path);

#if IMPLIB_REQUIRED
   char *impname LOCAL = xasprintf("_%s.lib", istr(unit_name));
   char imp_path[PATH_MAX];
   lib_realpath(lib_work(), impname, imp_path, PATH_MAX);

   cgen_link_arg("-Wl,--out-implib=%s", imp_path);
   cgen_link_arg("-L%s", lib_path(lib_work()));

   LOCAL_IDENT_LIST deps = NULL;
   cgen_find_implib_deps(unit_name, &deps);

   for (const ident_list_t *it = deps; it != NULL; it = it->next) {
      if (it->ident == unit_name)
         continue;

      lib_t lib = lib_find(ident_until(it->ident, '.'), true);

      char *imp_name LOCAL = xasprintf("_%s.lib", istr(it->ident));
      char import_imp[PATH_MAX];
      lib_realpath(lib, imp_name, import_imp, PATH_MAX);

      if (access(import_imp, F_OK) == 0) {
         if (lib != lib_work())
            cgen_link_arg("-L%s", lib_path(lib));
         cgen_link_arg("-l_%s", istr(it->ident));
      }
   }
#endif

   const char *obj = getenv("NVC_FOREIGN_OBJ");
   if (obj != NULL)
      cgen_link_arg(obj);

#ifdef IMPLIB_REQUIRED
   const char *cyglib = getenv("NVC_IMP_LIB");
   char *cygarg LOCAL = xasprintf("-L%s", (cyglib != NULL) ? cyglib : DATADIR);
   cgen_link_arg(cygarg);
   cgen_link_arg("-lnvcimp");
#endif

   ARRAY_APPEND(link_args, NULL, n_link_args, max_link_args);

   run_program((const char * const *)link_args, n_link_args - 1);

   for (size_t i = 0; i < n_link_args; i++)
      free(link_args[i]);
   free(link_args);
   link_args = NULL;
}

void cgen(tree_t top, vcode_unit_t vcode)
{
   tree_kind_t kind = tree_kind(top);
   if (kind != T_ELAB && kind != T_PACK_BODY && kind != T_PACKAGE)
      fatal("cannot generate code for %s", tree_kind_str(kind));

   module = LLVMModuleCreateWithName(istr(tree_ident(top)));
   builder = LLVMCreateBuilder();

   LLVMInitializeNativeTarget();
   LLVMInitializeNativeAsmPrinter();

   char *def_triple = LLVMGetDefaultTargetTriple();
   char *error;
   LLVMTargetRef target_ref;
   if (LLVMGetTargetFromTriple(def_triple, &target_ref, &error))
      fatal("failed to get LLVM target for %s: %s", def_triple, error);

   LLVMCodeGenOptLevel code_gen_level;
   switch (opt_get_int("optimise")) {
   case 0: code_gen_level = LLVMCodeGenLevelNone; break;
   case 1: code_gen_level = LLVMCodeGenLevelLess; break;
   case 3: code_gen_level = LLVMCodeGenLevelAggressive; break;
   default: code_gen_level = LLVMCodeGenLevelDefault;
   }

   LLVMTargetMachineRef tm_ref =
      LLVMCreateTargetMachine(target_ref, def_triple, "", "",
                              code_gen_level,
                              LLVMRelocPIC,
                              LLVMCodeModelDefault);

   LLVMSetTarget(module, def_triple);

#if LLVM_HAS_CREATE_TARGET_DATA_LAYOUT
   LLVMTargetDataRef data_ref = LLVMCreateTargetDataLayout(tm_ref);
#else
   LLVMTargetDataRef data_ref = LLVMGetTargetMachineData(tm_ref);
#endif

   char *layout LOCAL = LLVMCopyStringRepOfTargetData(data_ref);
   LLVMSetDataLayout(module, layout);

   string_pool = hash_new(128, true);

   cgen_tmp_stack();

   cgen_top(top, vcode);

   if (opt_get_int("dump-llvm"))
      LLVMDumpModule(module);

   if (LLVMVerifyModule(module, LLVMPrintMessageAction, NULL))
      fatal("LLVM verification failed");

   cgen_optimise();
   cgen_native(top, tm_ref);

   hash_free(string_pool);

   LLVMDisposeModule(module);
   LLVMDisposeBuilder(builder);
   LLVMDisposeTargetMachine(tm_ref);
#if LLVM_HAS_CREATE_TARGET_DATA_LAYOUT
   LLVMDisposeTargetData(data_ref);
#endif
   LLVMDisposeMessage(def_triple);
}
