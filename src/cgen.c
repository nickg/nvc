//
//  Copyright (C) 2011-2021  Nick Gasson
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
#include <ctype.h>
#include <libgen.h>
#include <sys/stat.h>

#include <llvm-c/Core.h>
#include <llvm-c/BitWriter.h>
#include <llvm-c/ExecutionEngine.h>
#include <llvm-c/Analysis.h>
#include <llvm-c/DebugInfo.h>
#include <llvm-c/Transforms/Scalar.h>
#include <llvm-c/Transforms/IPO.h>
#include <llvm-c/Transforms/PassManagerBuilder.h>
#include <llvm-c/TargetMachine.h>

#undef NDEBUG
#include <assert.h>

#define DEBUG_METADATA_VERSION 3

#define DUMP_ASSEMBLY 0
#define DUMP_BITCODE  0

typedef struct {
   LLVMValueRef      *regs;
   LLVMBasicBlockRef *blocks;
   LLVMValueRef       fn;
   LLVMValueRef       state;
   LLVMValueRef       display;
   LLVMValueRef      *locals;
} cgen_ctx_t;

typedef enum {
   FUNC_ATTR_NOUNWIND,
   FUNC_ATTR_NORETURN,
   FUNC_ATTR_READONLY,
   FUNC_ATTR_NOCAPTURE,
   FUNC_ATTR_BYVAL,
   FUNC_ATTR_UWTABLE,
   FUNC_ATTR_NOINLINE,
   FUNC_ATTR_WRITEONLY,
   FUNC_ATTR_NONNULL,
   FUNC_ATTR_COLD,

   // Attributes requiring special handling
   FUNC_ATTR_PRESERVE_FP,
   FUNC_ATTR_DLLEXPORT,
} func_attr_t;

static LLVMModuleRef    module = NULL;
static LLVMBuilderRef   builder = NULL;
static LLVMDIBuilderRef debuginfo = NULL;

static A(char *)           link_args;
static hash_t             *string_pool = NULL;
static A(LLVMMetadataRef)  debug_scopes;

#ifndef LLVM_HAVE_DI_SCOPE_GET_FILE
static LLVMMetadataRef debug_file = NULL;
#endif

static LLVMValueRef cgen_support_fn(const char *name);
static LLVMTypeRef cgen_state_type(vcode_unit_t unit);

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

static LLVMValueRef llvm_ensure_int_bits(LLVMValueRef value, int bits)
{
   const int value_bits = LLVMGetIntTypeWidth(LLVMTypeOf(value));
   if (value_bits < bits)
      return LLVMBuildZExt(builder, value, LLVMInt32Type(), "");
   else if (value_bits > bits) {
      return LLVMBuildTrunc(builder, value, LLVMInt32Type(), "");
   }
   else
      return value;
}

static LLVMValueRef llvm_zext_to_intptr(LLVMValueRef value)
{
   return LLVMBuildZExt(builder, value, LLVMIntType(sizeof(void *) * 8), "");
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
   LLVMTargetDataRef td = LLVMGetModuleDataLayout(module);
   return llvm_int32(LLVMABISizeOfType(td, type));
}

static LLVMValueRef llvm_fn(const char *name)
{
   LLVMValueRef fn = LLVMGetNamedFunction(module, name);
   if ((fn == NULL) && ((fn = cgen_support_fn(name)) == NULL))
      fatal("cannot find named function %s", name);
   return fn;
}

static LLVMTypeRef llvm_signal_shared_struct(void)
{
   LLVMTypeRef exist = LLVMGetTypeByName(module, "sig_shared");
   if (exist != NULL)
      return exist;

   LLVMTypeRef fields[] = {
      LLVMInt32Type(),     // Signal ID
      LLVMInt32Type(),     // Pad
      llvm_void_ptr(),     // Resolved pointer
      llvm_void_ptr()      // Last value pointer
   };

   LLVMTypeRef new =
      LLVMStructCreateNamed(LLVMGetGlobalContext(), "sig_shared");
   LLVMStructSetBody(new, fields, ARRAY_LEN(fields), false);
   return new;
}

static LLVMTypeRef llvm_signal_type(void)
{
   LLVMTypeRef exist = LLVMGetTypeByName(module, "signal");
   if (exist != NULL)
      return exist;

   LLVMTypeRef fields[] = {
      LLVMPointerType(llvm_signal_shared_struct(), 0),
      LLVMInt32Type()    // Offset
   };

   LLVMTypeRef new =
      LLVMStructCreateNamed(LLVMGetGlobalContext(), "signal");
   LLVMStructSetBody(new, fields, ARRAY_LEN(fields), false);
   return new;
}

static LLVMTypeRef llvm_uarray_type(LLVMTypeRef base, int dims)
{
   // Unconstrained arrays are represented by a structure
   // containing the left and right indices, a flag indicating
   // direction, and a pointer to the array data

   bool is_signal = false;
   LOCAL_TEXT_BUF tb = tb_new();
   tb_cat(tb, "uarray.");

   LLVMTypeRef it = base;
   while (LLVMGetTypeKind(it) == LLVMPointerTypeKind) {
      tb_cat(tb, "access.");
      it = LLVMGetElementType(it);
   }

   switch (LLVMGetTypeKind(it)) {
   case LLVMIntegerTypeKind:
      tb_printf(tb, "i%d.%d", LLVMGetIntTypeWidth(it), dims);
      break;
   case LLVMDoubleTypeKind:
      tb_printf(tb, "f.%d", dims);
      break;
   case LLVMStructTypeKind:
      {
         const char *name = LLVMGetStructName(it);
         tb_printf(tb, "%s.%d", name, dims);
         is_signal = (it == llvm_signal_type());
      }
      break;
   default:
      fatal_trace("cannot generate uarray type name for %s",
                  LLVMPrintTypeToString(base));
   }

   const char *struct_name = tb_get(tb);

   LLVMTypeRef exist = LLVMGetTypeByName(module, struct_name);
   if (exist != NULL)
      return exist;

   LLVMTypeRef dim_fields[] = {
      LLVMInt32Type(),      // Left
      LLVMInt32Type(),      // Length
   };

   LLVMTypeRef dim_struct =
      LLVMStructType(dim_fields, ARRAY_LEN(dim_fields), false);

   LLVMTypeRef fields[] = {
      is_signal ? base : LLVMPointerType(base, 0),
      LLVMArrayType(dim_struct, dims)
   };

   LLVMTypeRef new = LLVMStructCreateNamed(LLVMGetGlobalContext(), struct_name);
   LLVMStructSetBody(new, fields, ARRAY_LEN(fields), false);
   return new;
}

static LLVMTypeRef llvm_resolution_type(void)
{
   LLVMTypeRef struct_elems[] = {
      llvm_void_ptr(),     // Function pointer
      llvm_void_ptr(),     // Context pointer
      LLVMInt32Type(),     // Flags
      LLVMInt32Type()      // Left index
   };
   return LLVMStructType(struct_elems, ARRAY_LEN(struct_elems), false);
}

static void llvm_add_module_flag(const char *key, int value)
{
   LLVMAddModuleFlag(module, LLVMModuleFlagBehaviorWarning,
                     key, strlen(key),
                     LLVMValueAsMetadata(llvm_int32(value)));
}

static void llvm_lifetime_start(LLVMValueRef ptr, LLVMTypeRef type)
{
   LLVMValueRef fn = llvm_fn("llvm.lifetime.start.p0i8");
   LLVMTargetDataRef td = LLVMGetModuleDataLayout(module);
   LLVMValueRef args[] = {
      llvm_int64(LLVMABISizeOfType(td, type)),
      llvm_void_cast(ptr)
   };
   LLVMBuildCall(builder, fn, args, ARRAY_LEN(args), "");
}

static void llvm_lifetime_end(LLVMValueRef ptr, LLVMTypeRef type)
{
   LLVMValueRef fn = llvm_fn("llvm.lifetime.end.p0i8");
   LLVMTargetDataRef td = LLVMGetModuleDataLayout(module);
   LLVMValueRef args[] = {
      llvm_int64(LLVMABISizeOfType(td, type)),
      llvm_void_cast(ptr)
   };
   LLVMBuildCall(builder, fn, args, ARRAY_LEN(args), "");
}

__attribute__((unused))
static void llvm_dump(const char *tag, LLVMValueRef value)
{
   fprintf(stderr, "%s: ", tag);
   LLVMDumpValue(value);
   fprintf(stderr, "\n");
}

__attribute__((unused))
static void debug_out(LLVMValueRef val)
{
   LLVMValueRef args[] = { val, llvm_int32(-1) };
   LLVMBuildCall(builder, llvm_fn("_debug_out"),
                 args, ARRAY_LEN(args), "");
}

__attribute__((unused))
static void debug_dump(LLVMValueRef ptr, LLVMValueRef len)
{
   LLVMValueRef args[] = { llvm_void_cast(ptr), len };
   LLVMBuildCall(builder, llvm_fn("_debug_dump"),
                 args, ARRAY_LEN(args), "");
}

static void cgen_add_func_attr(LLVMValueRef fn, func_attr_t attr, int param)
{
   LLVMAttributeRef ref;
   if (attr == FUNC_ATTR_DLLEXPORT) {
#ifdef IMPLIB_REQUIRED
      LLVMSetDLLStorageClass(fn, LLVMDLLExportStorageClass);
#endif
      return;
   }
   else if (attr == FUNC_ATTR_PRESERVE_FP) {
      ref = LLVMCreateStringAttribute(LLVMGetGlobalContext(),
                                      "frame-pointer", 13, "all", 3);
   }
   else {
      const char *names[] = {
         "nounwind", "noreturn", "readonly", "nocapture", "byval", "uwtable",
         "noinline", "writeonly", "nonnull", "cold"
      };
      assert(attr < ARRAY_LEN(names));

      const unsigned kind =
         LLVMGetEnumAttributeKindForName(names[attr], strlen(names[attr]));
      if (kind == 0)
         fatal_trace("Cannot get LLVM attribute for %s", names[attr]);

      ref = LLVMCreateEnumAttribute(LLVMGetGlobalContext(), kind, 0);
   }

   LLVMAddAttributeAtIndex(fn, param, ref);
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
      return llvm_uarray_type(cgen_type(vtype_elem(type)), vtype_dims(type));

   case VCODE_TYPE_OFFSET:
      return LLVMInt32Type();

   case VCODE_TYPE_POINTER:
   case VCODE_TYPE_ACCESS:
      {
         vcode_type_t pointed = vtype_pointed(type);
         if (vtype_kind(pointed) == VCODE_TYPE_OPAQUE)
            return llvm_void_ptr();
         else
            return LLVMPointerType(cgen_type(pointed), 0);
      }

   case VCODE_TYPE_RECORD:
      {
         const char *name = istr(vtype_record_name(type));
         LLVMTypeRef lltype = LLVMGetTypeByName(module, name);
         if (lltype != NULL && !LLVMIsOpaqueStruct(lltype))
            return lltype;
         else if (lltype == NULL) {
            lltype = LLVMStructCreateNamed(LLVMGetGlobalContext(), name);
            if (lltype == NULL)
               fatal("failed to add record type %s", name);
         }

         const int nfields = vtype_fields(type);
         if (nfields > 0) {
            LLVMTypeRef fields[nfields];

            for (int i = 0; i < nfields; i++)
               fields[i] = cgen_type(vtype_field(type, i));

            LLVMStructSetBody(lltype, fields, nfields, true);
         }

         return lltype;
      }

   case VCODE_TYPE_SIGNAL:
      return llvm_signal_type();

   case VCODE_TYPE_FILE:
      return llvm_void_ptr();

   case VCODE_TYPE_OPAQUE:
      return LLVMVoidType();

   case VCODE_TYPE_RESOLUTION:
      return llvm_resolution_type();

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

static void cgen_push_debug_scope(LLVMMetadataRef scope)
{
   APUSH(debug_scopes, scope);
}

static void cgen_pop_debug_scope(void)
{
   APOP(debug_scopes);
}

static LLVMMetadataRef cgen_top_debug_scope(void)
{
   assert(debug_scopes.count > 0);
   return AGET(debug_scopes, debug_scopes.count - 1);
}

static LLVMMetadataRef cgen_debug_file(const loc_t *loc)
{
   // Ignore the passed in loc and just return the top-level file
#ifdef LLVM_HAVE_DI_SCOPE_GET_FILE
   return LLVMDIScopeGetFile(AGET(debug_scopes, 0));
#else
   return debug_file;
#endif
}

static void cgen_debug_loc(cgen_ctx_t *ctx, const loc_t *loc, bool force)
{
   static loc_t last_loc = LOC_INVALID;

   if (loc_eq(loc, &last_loc) && !force)
      return;

   LLVMMetadataRef dloc = LLVMDIBuilderCreateDebugLocation(
      LLVMGetGlobalContext(), loc->first_line, loc->first_column,
      cgen_top_debug_scope(), NULL);
#ifdef LLVM_HAVE_SET_CURRENT_DEBUG_LOCATION_2
   LLVMSetCurrentDebugLocation2(builder, dloc);
#else
   LLVMValueRef md = LLVMMetadataAsValue(LLVMGetGlobalContext(), dloc);
   LLVMSetCurrentDebugLocation(builder, md);
#endif
}

static void cgen_debug_push_func(cgen_ctx_t *ctx)
{
   LLVMMetadataRef scope = cgen_top_debug_scope();

   const char *name   = istr(vcode_unit_name());
   const char *symbol = safe_symbol(name);

   const loc_t *loc = vcode_unit_loc();
   LLVMMetadataRef file_ref = cgen_debug_file(loc);
   LLVMMetadataRef dtype = LLVMDIBuilderCreateSubroutineType(
      debuginfo, file_ref, NULL, 0, 0);
   LLVMMetadataRef sp = LLVMDIBuilderCreateFunction(
      debuginfo, scope, name, strlen(name),
      symbol, strlen(symbol), file_ref,
      loc->first_line, dtype, true, true,
      1, 0, opt_get_int("optimise"));
   LLVMSetSubprogram(ctx->fn, sp);

   cgen_push_debug_scope(sp);
   cgen_debug_loc(ctx, vcode_unit_loc(), true);
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

   LLVMValueRef display = ctx->display;
   for (int i = 0; i < hops - 1; i++) {
      LLVMValueRef ptr = LLVMBuildStructGEP(builder, display, 0, "");
      display = LLVMBuildLoad(builder, ptr, "");
   }

   return display;
}

static bool cgen_is_procedure(void)
{
   switch (vcode_unit_kind()) {
   case VCODE_UNIT_PROCEDURE:
      return true;
   case VCODE_UNIT_FUNCTION:
      // Procedure compiled as function
      return vcode_unit_result() == VCODE_INVALID_TYPE;
   default:
      return false;
   }
}

static unsigned cgen_fixed_offset(void)
{
   unsigned base = 0;
   if (vcode_unit_context() != 0)
      base += 1;
   if (cgen_is_procedure() || vcode_unit_kind() == VCODE_UNIT_PROCESS)
      base += 2;
   return base;
}

static unsigned cgen_var_offset(vcode_var_t var)
{
   unsigned base = cgen_fixed_offset();

   const vunit_kind_t kind = vcode_unit_kind();
   if (kind == VCODE_UNIT_PROCEDURE || kind == VCODE_UNIT_FUNCTION)
      base += vcode_count_params();

   return base + var;
}

static unsigned cgen_param_offset(vcode_reg_t param)
{
   return cgen_fixed_offset() + param;
}

static LLVMValueRef cgen_get_var(vcode_var_t var, cgen_ctx_t *ctx)
{
   LLVMValueRef value = NULL;
   if (ctx->state != NULL) {
      value = LLVMBuildStructGEP(builder, ctx->state,
                                 cgen_var_offset(var),
                                 istr(vcode_var_name(var)));
   }
   else {
      assert(ctx->locals != NULL);
      value = ctx->locals[var];
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

   LLVMValueRef indexes[] = { llvm_zext_to_intptr(alloc) };
   LLVMValueRef buf = LLVMBuildInBoundsGEP(builder, stack,
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

static LLVMValueRef cgen_const_string(const char *str)
{
   ident_t key = ident_new(str);
   LLVMValueRef ref = hash_get(string_pool, key);
   if (ref == NULL) {
      const size_t len = strlen(str);
      ref = LLVMAddGlobal(module,
                          LLVMArrayType(LLVMInt8Type(), len + 1),
                          "const_string");
      LLVMSetGlobalConstant(ref, true);
      LLVMSetInitializer(ref, LLVMConstString(str, len, false));
      LLVMSetLinkage(ref, LLVMPrivateLinkage);
      LLVMSetUnnamedAddr(ref, true);

      hash_put(string_pool, key, ref);
   }

   return cgen_array_pointer(ref);
}

static LLVMValueRef cgen_scoped_alloca(LLVMTypeRef type, cgen_ctx_t *ctx)
{
   LLVMBasicBlockRef orig_bb = LLVMGetInsertBlock(builder);
   LLVMBasicBlockRef entry_bb = LLVMGetEntryBasicBlock(ctx->fn);

   LLVMValueRef first = LLVMGetFirstInstruction(entry_bb);
   if (first == NULL)
      LLVMPositionBuilderAtEnd(builder, entry_bb);
   else
      LLVMPositionBuilderBefore(builder, first);

   LLVMValueRef ptr = LLVMBuildAlloca(builder, type, "");
   LLVMPositionBuilderAtEnd(builder, orig_bb);
   return ptr;
}

static LLVMValueRef cgen_location(int op, cgen_ctx_t *ctx)
{
   const loc_t *loc = vcode_get_loc(op);
   assert(!loc_invalid_p(loc));

   unsigned last_line = loc->first_line + loc->line_delta;
   unsigned last_column = loc->first_column + loc->column_delta;

   char *buf LOCAL = xasprintf("loc.%d.%d.%d.%d.%d", loc->file_ref,
                               loc->first_line, last_line,
                               loc->first_column, last_column);

   LLVMValueRef global = LLVMGetNamedGlobal(module, buf);
   if (global != NULL)
      return global;

   LLVMValueRef file_name = cgen_const_string(loc_file_str(loc));

   LLVMTypeRef rt_loc = llvm_rt_loc();

   LLVMValueRef init = LLVMGetUndef(rt_loc);
   init = LLVMBuildInsertValue(builder, init,
                               llvm_int32(loc->first_line), 0, "");
   init = LLVMBuildInsertValue(builder, init, llvm_int32(last_line), 1, "");
   init = LLVMBuildInsertValue(builder, init,
                               llvm_int16(loc->first_column), 2, "");
   init = LLVMBuildInsertValue(builder, init, llvm_int16(last_column), 3, "");
   init = LLVMBuildInsertValue(builder, init, file_name, 4, "");

   global = LLVMAddGlobal(module, rt_loc, buf);
   LLVMSetGlobalConstant(global, true);
   LLVMSetInitializer(global, init);
   LLVMSetLinkage(global, LLVMPrivateLinkage);
   LLVMSetUnnamedAddr(global, true);

   return global;
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
                                   vcode_cc_t cc, LLVMTypeRef display_type,
                                   const vcode_type_t *vparams, size_t nparams)
{
   const char *safe_name = safe_symbol(istr(name));
   LLVMValueRef fn = LLVMGetNamedFunction(module, safe_name);
   if (fn != NULL)
      return fn;
   else if (vparams == NULL)
      return NULL;

   const bool has_state_arg =
      result == VCODE_INVALID_TYPE && cc != VCODE_CC_FOREIGN;
   const bool foreign_uarray_result =
      cc == VCODE_CC_FOREIGN && result != VCODE_INVALID_REG
      && vtype_kind(result) == VCODE_TYPE_UARRAY;

   const int nextra = !!display_type + has_state_arg + foreign_uarray_result;
   LLVMTypeRef params[nparams + nextra];
   LLVMTypeRef *p = params;

   if (display_type != NULL)
      *p++ = LLVMPointerType(display_type, 0);

   if (has_state_arg)
      *p++ = llvm_void_ptr();

   for (size_t i = 0; i < nparams; i++)
      *p++ = cgen_type(vparams[i]);

   if (foreign_uarray_result)
      *p++ = LLVMPointerType(cgen_type(result), 0);

   LLVMTypeRef type = NULL;
   if (result == VCODE_INVALID_TYPE)
      type = LLVMFunctionType(llvm_void_ptr(), params, nparams + nextra, false);
   else if (foreign_uarray_result)
      type = LLVMFunctionType(LLVMVoidType(), params, nparams + nextra, false);
   else
      type = LLVMFunctionType(cgen_type(result), params,
                              nparams + nextra, false);

   fn = LLVMAddFunction(module, safe_name, type);

   cgen_add_func_attr(fn, FUNC_ATTR_DLLEXPORT, -1);
   cgen_add_func_attr(fn, FUNC_ATTR_NOUNWIND, -1);

   if (foreign_uarray_result) {
      cgen_add_func_attr(fn, FUNC_ATTR_NOCAPTURE, nparams + nextra);
      cgen_add_func_attr(fn, FUNC_ATTR_WRITEONLY, nparams + nextra);
   }

   return fn;
}

static LLVMTypeRef cgen_display_type_for_unit(ident_t unit_name)
{
   ident_t scope_name = NULL;

   vcode_unit_t unit = vcode_find_unit(unit_name);
   if (unit != NULL) {
      vcode_state_t state;
      vcode_state_save(&state);

      vcode_select_unit(unit);
      vcode_select_unit(vcode_unit_context());

      scope_name = vcode_unit_name();

      vcode_state_restore(&state);
   }

   if (scope_name == NULL) {
      scope_name = ident_runtil(ident_runtil(unit_name, '('), '.');
      assert(scope_name != NULL);
   }

   const char *type_name LOCAL = xasprintf("%s.state", istr(scope_name));

   LLVMTypeRef opaque = LLVMGetTypeByName(module, type_name);
   if (opaque == NULL)
      opaque = LLVMStructCreateNamed(LLVMGetGlobalContext(), type_name);

   return opaque;
}

static LLVMTypeRef cgen_display_type_for_call(int op, cgen_ctx_t *ctx)
{
   if (vcode_get_subkind(op) == VCODE_CC_FOREIGN)
      return NULL;

   return cgen_display_type_for_unit(vcode_get_func(op));
}

static LLVMValueRef cgen_display_for_call(int op, cgen_ctx_t *ctx)
{
   if (vcode_get_subkind(op) == VCODE_CC_FOREIGN)
      return NULL;

   ident_t subprog = vcode_get_func(op);
   ident_t scope_name = NULL;

   vcode_unit_t unit = vcode_find_unit(subprog);
   if (unit != NULL) {
      vcode_state_t state;
      vcode_state_save(&state);

      vcode_select_unit(unit);

      vcode_unit_t context = vcode_unit_context();
      vcode_select_unit(context);

      const vunit_kind_t kind = vcode_unit_kind();
      scope_name = vcode_unit_name();

      vcode_state_restore(&state);

      if (context == vcode_active_unit())
         return ctx->state;
      else if (context == vcode_unit_context())
         return ctx->display;
      else if (kind == VCODE_UNIT_PACKAGE) {
         // Handled below
      }
      else {
         vcode_state_save(&state);
         vcode_select_unit(vcode_unit_context());

         LLVMValueRef display = ctx->display;
         while (vcode_active_unit() != context)  {
            vcode_select_unit(vcode_unit_context());
            LLVMValueRef ptr = LLVMBuildStructGEP(builder, display, 0, "");
            display = LLVMBuildLoad(builder, ptr, "");
         }

         vcode_state_restore(&state);
         return display;
      }
   }

   if (scope_name == NULL) {
      scope_name = ident_runtil(ident_runtil(subprog, '('), '.');
      assert(scope_name != NULL);

      ident_t next = ident_runtil(scope_name, '.');
      while (ident_runtil(next, '.') != next) {
         scope_name = next;
         next = ident_runtil(scope_name, '.');
      }
   }

   const char *var_name = istr(scope_name);
   LLVMValueRef global = LLVMGetNamedGlobal(module, var_name);
   if (global == NULL) {
      LLVMTypeRef opaque = cgen_display_type_for_call(op, ctx);
      global = LLVMAddGlobal(module, LLVMPointerType(opaque, 0), var_name);
   }

   return LLVMBuildLoad(builder, global, "");
}

static void cgen_op_return(int op, cgen_ctx_t *ctx)
{
   switch (vcode_unit_kind()) {
   case VCODE_UNIT_PROCEDURE:
      LLVMBuildFree(builder, ctx->state);
      LLVMBuildRet(builder, LLVMConstNull(llvm_void_ptr()));
      break;

   case VCODE_UNIT_PROCESS:
      {
         LLVMValueRef fsm_state_ptr =
            LLVMBuildStructGEP(builder, ctx->state, 1, "");
         LLVMBuildStore(builder, llvm_int32(1), fsm_state_ptr);
         LLVMBuildRet(builder, llvm_void_cast(ctx->state));
      }
      break;

   case VCODE_UNIT_INSTANCE:
   case VCODE_UNIT_PACKAGE:
      LLVMBuildRet(builder, llvm_void_cast(ctx->state));
      break;

   default:
      if (vcode_count_args(op) > 0)
         LLVMBuildRet(builder, cgen_get_arg(op, 0, ctx));
      else {
         LLVMTypeRef fn_type = LLVMGetElementType(LLVMTypeOf(ctx->fn));
         if (LLVMGetTypeKind(LLVMGetReturnType(fn_type)) == LLVMVoidTypeKind)
            LLVMBuildRetVoid(builder);
         else
            LLVMBuildRet(builder, LLVMConstNull(llvm_void_ptr()));
      }
      break;
   }
}

static void cgen_op_jump(int i, cgen_ctx_t *ctx)
{
   LLVMBuildBr(builder, ctx->blocks[vcode_get_target(i, 0)]);
}

static void cgen_op_fcall(int op, bool nested, cgen_ctx_t *ctx)
{
   vcode_reg_t result = vcode_get_result(op);

   vcode_type_t rtype = VCODE_INVALID_TYPE;
   if (result != VCODE_INVALID_REG)
      rtype = vcode_reg_type(result);

   const vcode_cc_t cc = vcode_get_subkind(op);
   ident_t func = vcode_get_func(op);
   const bool has_state_arg =
      result == VCODE_INVALID_REG && cc == VCODE_CC_VHDL;
   const bool foreign_uarray_result =
      cc == VCODE_CC_FOREIGN && result != VCODE_INVALID_REG
      && vtype_kind(rtype) == VCODE_TYPE_UARRAY;
   const int nargs = vcode_count_args(op);
   const int total_args =
      nargs + (cc != VCODE_CC_FOREIGN) + has_state_arg + foreign_uarray_result;

   LLVMValueRef fn =
      cgen_signature(func, VCODE_INVALID_TYPE, cc, NULL, NULL, 0);
   if (fn == NULL) {
      vcode_type_t atypes[nargs];
      for (int i = 0; i < nargs; i++)
         atypes[i] = vcode_reg_type(vcode_get_arg(op, i));

      LLVMTypeRef display_type = cgen_display_type_for_call(op, ctx);
      fn = cgen_signature(func, rtype, cc, display_type, atypes, nargs);
   }

   LLVMValueRef args[total_args];
   LLVMValueRef *pa = args;
   if (cc != VCODE_CC_FOREIGN)
      *pa++ = cgen_display_for_call(op, ctx);
   if (has_state_arg)
      *pa++ = LLVMConstNull(llvm_void_ptr());
   for (int i = 0; i < nargs; i++)
      *pa++ = cgen_get_arg(op, i, ctx);

   if (foreign_uarray_result) {
      LLVMTypeRef utype = cgen_type(rtype);
      LLVMValueRef uresult = cgen_scoped_alloca(utype, ctx);
      *pa++ = uresult;
      llvm_lifetime_start(uresult, utype);
      LLVMBuildCall(builder, fn, args, total_args, "");
      ctx->regs[result] =
         LLVMBuildLoad(builder, uresult, cgen_reg_name(result));
      llvm_lifetime_end(uresult, utype);
   }
   else if (result != VCODE_INVALID_REG)
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
   LLVMValueRef *tmp LOCAL = xmalloc_array(length, sizeof(LLVMValueRef));
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
      cgen_location(op, ctx)
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
      cgen_location(op, ctx)
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
   LLVMValueRef state_ptr = LLVMBuildStructGEP(builder, ctx->state, 1, "");
   LLVMBuildStore(builder, llvm_int32(vcode_get_target(op, 0)), state_ptr);

   if (vcode_unit_kind() == VCODE_UNIT_PROCEDURE) {
      LLVMBuildCall(builder, llvm_fn("_private_stack"), NULL, 0, "");
      LLVMBuildRet(builder, llvm_void_cast(ctx->state));
   }
   else
      LLVMBuildRet(builder, LLVMConstNull(llvm_void_ptr()));
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

   if (kind == VCODE_TYPE_POINTER) {
      LLVMValueRef index[] = { rhs };
      ctx->regs[result] = LLVMBuildInBoundsGEP(builder, lhs,
                                               index, ARRAY_LEN(index),
                                               cgen_reg_name(result));
   }
   else if (kind == VCODE_TYPE_SIGNAL) {
      LLVMValueRef base = LLVMBuildExtractValue(builder, lhs, 1, "base");
      vcode_type_t vtype = vtype_base(vcode_reg_type(vcode_get_arg(op, 0)));
      LLVMValueRef null = LLVMConstNull(LLVMPointerType(cgen_type(vtype), 0));
      LLVMValueRef index[] = { rhs };
      LLVMValueRef gep = LLVMBuildInBoundsGEP(builder, null, index, 1, "");
      LLVMValueRef scaled =
         LLVMBuildPtrToInt(builder, gep, LLVMInt32Type(), "");
      LLVMValueRef add = LLVMBuildAdd(builder, base, scaled, "");
      ctx->regs[result] = LLVMBuildInsertValue(builder, lhs,
                                               add, 1, cgen_reg_name(result));
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
         cgen_location(op, ctx)
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

   LLVMValueRef numer = cgen_get_arg(op, 0, ctx);
   LLVMValueRef denom = cgen_get_arg(op, 1, ctx);

   // Calculate the following:
   //
   //   long r = numer % denom;
   //   if ((r > 0 && denom < 0) || (r < 0 && denom > 0))
   //      r = r + denom;

   LLVMValueRef r = LLVMBuildSRem(builder, numer, denom, "");

   LLVMValueRef zero = llvm_int32(0);
   LLVMValueRef c1 = LLVMBuildICmp(builder, LLVMIntSGT, r, zero, "");
   LLVMValueRef c2 = LLVMBuildICmp(builder, LLVMIntSLT, denom, zero, "");
   LLVMValueRef c3 = LLVMBuildAnd(builder, c1, c2, "");
   LLVMValueRef c4 = LLVMBuildICmp(builder, LLVMIntSLT, r, zero, "");
   LLVMValueRef c5 = LLVMBuildICmp(builder, LLVMIntSGT, denom, zero, "");
   LLVMValueRef c6 = LLVMBuildAnd(builder, c4, c5, "");
   LLVMValueRef c7 = LLVMBuildOr(builder, c3, c6, "");

   LLVMValueRef v1 = LLVMBuildAdd(builder, r, denom, "");
   ctx->regs[result] = LLVMBuildSelect(builder, c7, v1, r, "");
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

   LLVMValueRef args[] = {
      llvm_ensure_int_bits(value, 32),
      llvm_ensure_int_bits(min, 32),
      llvm_ensure_int_bits(max, 32),
      llvm_int32(vcode_get_subkind(op)),
      cgen_location(op, ctx),
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
      cgen_location(op, ctx),
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
         bool quoted = false;
         for (size_t j = 0; j < max_len + 1; j++) {
            const size_t off = (i * (max_len + 1)) + j;
            char ch = ident_char(map.elems[i], tlen - j - 1);
            quoted |= ch == '\'';
            if (j < tlen)
               strings[off] = quoted ? ch : tolower(ch);
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

         LLVMValueRef *lvalues LOCAL =
            xmalloc_array(sizeof(LLVMValueRef), map.nelems);
         for (size_t i = 0; i < map.nelems; i++)
            lvalues[i] = LLVMConstInt(elem_type, map.values[i], false);

         LLVMValueRef init = LLVMConstArray(elem_type, lvalues, map.nelems);
         LLVMSetInitializer(values_glob, init);
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
   LLVMValueRef tmp = cgen_scoped_alloca(llvm_image_map(), ctx);
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

   LLVMTypeRef alloca_type = NULL;
   LLVMValueRef image_map;
   if (vcode_count_args(op) > 1)
      image_map = cgen_get_arg(op, 1, ctx);
   else {
      alloca_type = llvm_image_map();
      image_map = cgen_scoped_alloca(alloca_type, ctx);
      llvm_lifetime_start(image_map, alloca_type);
      LLVMBuildStore(
         builder,
         LLVMBuildInsertValue(builder, LLVMGetUndef(alloca_type),
                              llvm_int32(real ? IMAGE_REAL : IMAGE_INTEGER),
                              0, ""),
         image_map);
   }

   LLVMOpcode cop = real ? LLVMBitCast : (is_signed ? LLVMSExt : LLVMZExt);
   LLVMTypeRef utype = llvm_uarray_type(LLVMInt8Type(), 1);
   LLVMValueRef res = cgen_scoped_alloca(utype, ctx);
   llvm_lifetime_start(res, utype);
   LLVMValueRef iargs[] = {
      LLVMBuildCast(builder, cop, ctx->regs[arg], LLVMInt64Type(), ""),
      image_map,
      res
   };
   LLVMBuildCall(builder, llvm_fn("_image"), iargs, ARRAY_LEN(iargs), "");

   vcode_reg_t result = vcode_get_result(op);
   ctx->regs[result] = LLVMBuildLoad(builder, res, cgen_reg_name(result));

   if (alloca_type != NULL)
      llvm_lifetime_end(image_map, alloca_type);
   llvm_lifetime_end(res, utype);
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

   assert(LLVMCountStructElementTypes(uarray_type) == 2);
   LLVMTypeRef field_types[2];
   LLVMGetStructElementTypes(uarray_type, field_types);

   LLVMTypeRef dim_struct = LLVMGetElementType(field_types[1]);

   LLVMValueRef dim_array = LLVMGetUndef(field_types[1]);

   for (int i = 0; i < dims; i++) {
      LLVMValueRef left  = cgen_get_arg(op, (i * 3) + 1, ctx);
      LLVMValueRef right = cgen_get_arg(op, (i * 3) + 2, ctx);
      LLVMValueRef dir   = cgen_get_arg(op, (i * 3) + 3, ctx);

      left  = llvm_ensure_int_bits(left, 32);
      right = llvm_ensure_int_bits(right, 32);

      LLVMValueRef diff_up   = LLVMBuildSub(builder, right, left, "");
      LLVMValueRef diff_down = LLVMBuildSub(builder, left, right, "");

      LLVMValueRef diff = LLVMBuildSelect(builder, dir, diff_down, diff_up, "");
      LLVMValueRef zero = llvm_int32(0);
      LLVMValueRef null = LLVMBuildICmp(builder, LLVMIntSLT, diff, zero, "");

      LLVMValueRef length  = LLVMBuildAdd(builder, diff, llvm_int32(1), "");
      LLVMValueRef clamped = LLVMBuildSelect(builder, null, zero, length, "");
      LLVMValueRef neg     = LLVMBuildNeg(builder, clamped, "");
      LLVMValueRef signlen = LLVMBuildSelect(builder, dir, neg, clamped, "");

      LLVMValueRef d = LLVMGetUndef(dim_struct);
      d = LLVMBuildInsertValue(builder, d, left, 0, "");
      d = LLVMBuildInsertValue(builder, d, signlen, 1, "le");

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

   LLVMValueRef offset;
   if (vcode_count_args(op) > 0)
      offset = cgen_get_arg(op, 0, ctx);
   else
      offset = llvm_int32(0);

   LLVMValueRef index[] = { llvm_zext_to_intptr(offset) };
   ctx->regs[result] = LLVMBuildInBoundsGEP(builder, var, index,
                                            ARRAY_LEN(index), name);
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

   LLVMValueRef left   = LLVMBuildExtractValue(builder, dim, 0, "");
   LLVMValueRef length = LLVMBuildExtractValue(builder, dim, 1, "");
   LLVMValueRef zero   = llvm_int32(0);
   LLVMValueRef sign   = LLVMBuildICmp(builder, LLVMIntSLT, length, zero, "");
   LLVMValueRef diff   = LLVMBuildAdd(builder, left, length, "");
   LLVMValueRef adj    = LLVMBuildSelect(builder, sign, llvm_int32(1), llvm_int32(-1), "");

   vcode_reg_t result = vcode_get_result(op);
   ctx->regs[result] = LLVMBuildAdd(builder, diff, adj, cgen_reg_name(result));
}

static void cgen_op_uarray_dir(int op, cgen_ctx_t *ctx)
{
   LLVMValueRef dim = cgen_uarray_dim(cgen_get_arg(op, 0, ctx),
                                      vcode_get_dim(op));

   vcode_reg_t result = vcode_get_result(op);

   LLVMValueRef length = LLVMBuildExtractValue(builder, dim, 1, "length");
   LLVMValueRef zero = llvm_int32(0);
   ctx->regs[result] = LLVMBuildICmp(builder, LLVMIntSLT, length, zero, "");
}

static void cgen_op_uarray_len(int op, cgen_ctx_t *ctx)
{
   LLVMValueRef dim = cgen_uarray_dim(cgen_get_arg(op, 0, ctx),
                                      vcode_get_dim(op));

   LLVMValueRef length = LLVMBuildExtractValue(builder, dim, 1, "length");
   LLVMValueRef zero = llvm_int32(0);
   LLVMValueRef negative = LLVMBuildICmp(builder, LLVMIntSLT, length, zero, "");

   vcode_reg_t result = vcode_get_result(op);
   ctx->regs[result] = LLVMBuildSelect(builder, negative,
                                       LLVMBuildNeg(builder, length, ""),
                                       length, cgen_reg_name(result));
}

static void cgen_op_param_upref(int op, cgen_ctx_t *ctx)
{
   const int hops = vcode_get_hops(op);
   vcode_reg_t arg = vcode_get_arg(op, 0);

   vcode_state_t state;
   vcode_state_save(&state);

   for (int i = 0; i < hops; i++)
      vcode_select_unit(vcode_unit_context());

   const int offset = cgen_param_offset(arg);

   vcode_state_restore(&state);

   vcode_reg_t result = vcode_get_result(op);

   LLVMValueRef display = cgen_display_upref(hops, ctx);
   LLVMValueRef ptr = LLVMBuildStructGEP(builder, display, offset, "");
   ctx->regs[result] = LLVMBuildLoad(builder, ptr, cgen_reg_name(result));
}

static void cgen_op_var_upref(int op, cgen_ctx_t *ctx)
{
   const int hops = vcode_get_hops(op);
   vcode_var_t address = vcode_get_address(op);

   vcode_state_t state;
   vcode_state_save(&state);

   for (int i = 0; i < hops; i++)
      vcode_select_unit(vcode_unit_context());

   const int offset = cgen_var_offset(address);
   const bool is_carray =
      vtype_kind(vcode_var_type(address)) == VCODE_TYPE_CARRAY;

   vcode_state_restore(&state);

   vcode_reg_t result = vcode_get_result(op);
   LLVMValueRef display = cgen_display_upref(hops, ctx);
   ctx->regs[result] = LLVMBuildStructGEP(builder, display, offset,
                                          cgen_reg_name(result));

   if (is_carray) {
      LLVMValueRef index[] = {
         llvm_int32(0),
         llvm_int32(0)
      };
      ctx->regs[result] = LLVMBuildGEP(builder, ctx->regs[result],
                                       index, ARRAY_LEN(index), "");
   }
}

static void cgen_op_resolved(int op, cgen_ctx_t *ctx)
{
   vcode_reg_t result = vcode_get_result(op);

   LLVMValueRef sigptr = cgen_get_arg(op, 0, ctx);
   LLVMValueRef shared = LLVMBuildExtractValue(builder, sigptr, 0, "");

   LLVMValueRef resolved = LLVMBuildStructGEP(builder, shared, 2, "resolved");
   resolved = LLVMBuildLoad(builder, resolved, "");
   LLVMValueRef index[] = {
      llvm_zext_to_intptr(LLVMBuildExtractValue(builder, sigptr, 1, "offset")),
   };
   LLVMValueRef raw_ptr = LLVMBuildInBoundsGEP(builder, resolved, index,
                                               ARRAY_LEN(index), "");
   ctx->regs[result] = LLVMBuildBitCast(builder, raw_ptr,
                                        cgen_type(vcode_reg_type(result)),
                                        cgen_reg_name(result));
}

static void cgen_op_last_value(int op, cgen_ctx_t *ctx)
{
   vcode_reg_t result = vcode_get_result(op);

   LLVMValueRef sigptr = cgen_get_arg(op, 0, ctx);
   LLVMValueRef shared = LLVMBuildExtractValue(builder, sigptr, 0, "");

   LLVMTypeRef result_type = cgen_type(vcode_reg_type(result));

   LLVMValueRef resolved = LLVMBuildStructGEP(builder, shared, 3, "last_value");
   LLVMValueRef cast = LLVMBuildBitCast(builder, resolved,
                                        LLVMPointerType(result_type, 0), "");

   LLVMValueRef deref = LLVMBuildLoad(builder, cast, cgen_reg_name(result));

   if (LLVMGetTypeKind(LLVMTypeOf(deref)) == LLVMPointerTypeKind) {
      LLVMValueRef offset = LLVMBuildExtractValue(builder, sigptr, 1, "offset");
      LLVMValueRef index[] = { llvm_zext_to_intptr(offset) };
      ctx->regs[result] = LLVMBuildInBoundsGEP(builder, deref, index,
                                               ARRAY_LEN(index), "");
   }
   else
      ctx->regs[result] = deref;
}

static LLVMValueRef cgen_pointer_to_arg_data(int op, int arg,
                                             LLVMTypeRef *alloca_type,
                                             cgen_ctx_t *ctx)
{
   const vtype_kind_t kind = vcode_reg_kind(vcode_get_arg(op, arg));
   if (kind == VCODE_TYPE_INT || kind == VCODE_TYPE_REAL
       || kind == VCODE_TYPE_RESOLUTION) {
      // Need to get a pointer to the data
      LLVMValueRef scalar = cgen_get_arg(op, arg, ctx);
      *alloca_type = LLVMTypeOf(scalar);
      LLVMValueRef mem = cgen_scoped_alloca(*alloca_type, ctx);
      llvm_lifetime_start(mem, *alloca_type);
      LLVMBuildStore(builder, scalar, mem);
      return mem;
   }
   else {
      *alloca_type = NULL;
      return cgen_get_arg(op, arg, ctx);
   }
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

   LLVMValueRef sigptr = cgen_get_arg(op, 0, ctx);
   LLVMValueRef sid    = LLVMBuildExtractValue(builder, sigptr, 0, "sid");
   LLVMValueRef offset = LLVMBuildExtractValue(builder, sigptr, 1, "offset");

   if (scalar != NULL) {
      LLVMValueRef args[] = {
         sid,
         offset,
         scalar,
         cgen_get_arg(op, 4, ctx),
         cgen_get_arg(op, 3, ctx)
      };
      LLVMBuildCall(builder, llvm_fn("_sched_waveform_s"),
                    args, ARRAY_LEN(args), "");
   }
   else {
      LLVMValueRef args[] = {
         sid,
         offset,
         valptr,
         cgen_get_arg(op, 1, ctx),
         cgen_get_arg(op, 4, ctx),
         cgen_get_arg(op, 3, ctx)
      };
      LLVMBuildCall(builder, llvm_fn("_sched_waveform"),
                    args, ARRAY_LEN(args), "");
   }
}

static void cgen_op_resolution_wrapper(int op, cgen_ctx_t *ctx)
{
   // Resolution functions are in LRM 93 section 2.4

   ident_t func = vcode_get_func(op);
   vcode_reg_t result = vcode_get_result(op);
   vcode_type_t type = vtype_base(vcode_reg_type(result));

   LLVMValueRef rfn = cgen_signature(func, VCODE_INVALID_TYPE, VCODE_CC_VHDL,
                                     NULL, NULL, 0);
   if (rfn == NULL) {
      // The resolution function is not visible yet e.g. because it
      // is declared in another package
      const bool is_record = vtype_kind(type) == VCODE_TYPE_RECORD;
      vcode_type_t rtype = is_record ? vtype_pointer(type) : type;
      vcode_type_t args[] = {
         vtype_uarray(1, type, vtype_int(0, INT32_MAX))
      };
      LLVMTypeRef display_type = cgen_display_type_for_call(op, ctx);
      rfn = cgen_signature(func, rtype, VCODE_CC_VHDL, display_type, args, 1);
   }

   uint32_t flags = 0;
   if (vtype_kind(type) == VCODE_TYPE_RECORD)
      flags |= R_RECORD;

   LLVMValueRef display = cgen_display_for_call(op, ctx);
   LLVMValueRef ileft   = cgen_get_arg(op, 0, ctx);

   LLVMValueRef rdata = LLVMGetUndef(llvm_resolution_type());
   rdata = LLVMBuildInsertValue(builder, rdata, llvm_void_cast(rfn), 0, "");
   rdata = LLVMBuildInsertValue(builder, rdata, llvm_void_cast(display), 1, "");
   rdata = LLVMBuildInsertValue(builder, rdata, llvm_int32(flags), 2, "");
   rdata = LLVMBuildInsertValue(builder, rdata, ileft, 3, "");

   ctx->regs[result] = rdata;
}

static void cgen_net_flag(int op, net_flags_t flag, cgen_ctx_t *ctx)
{
   vcode_reg_t result = vcode_get_result(op);

   LLVMValueRef sigptr = cgen_get_arg(op, 0, ctx);
   LLVMValueRef count  = cgen_get_arg(op, 1, ctx);

   LLVMValueRef args[] = {
      LLVMBuildExtractValue(builder, sigptr, 0, "shared"),
      LLVMBuildExtractValue(builder, sigptr, 1, "offset"),
      count,
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

static void cgen_op_address_of(int op, cgen_ctx_t *ctx)
{
   vcode_reg_t result = vcode_get_result(op);

   char *name LOCAL = xasprintf("%s_const_r%d",
                                istr(vcode_unit_name()), result);

   LLVMValueRef init = cgen_get_arg(op, 0, ctx);
   LLVMTypeRef type = LLVMTypeOf(init);

   LLVMValueRef global = LLVMAddGlobal(module, type, name);
   LLVMSetLinkage(global, LLVMInternalLinkage);
   LLVMSetGlobalConstant(global, true);
   LLVMSetUnnamedAddr(global, true);
   LLVMSetInitializer(global, init);

   ctx->regs[result] = global;
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
      llvm_int1(0)
   };
   LLVMBuildCall(builder, llvm_fn(cgen_memcpy_name("memmove", 8)),
                 memcpy_args, ARRAY_LEN(memcpy_args), "");
}

static void cgen_op_record_ref(int op, cgen_ctx_t *ctx)
{
   vcode_reg_t result = vcode_get_result(op);
   vtype_kind_t kind = vtype_kind(vcode_reg_type(result));

   if (kind == VCODE_TYPE_SIGNAL) {
      LLVMValueRef signal = cgen_get_arg(op, 0, ctx);
      LLVMValueRef base = LLVMBuildExtractValue(builder, signal, 1, "base");
      vcode_type_t rtype = vtype_base(vcode_reg_type(vcode_get_arg(op, 0)));
      LLVMValueRef null = LLVMConstNull(LLVMPointerType(cgen_type(rtype), 0));
      LLVMValueRef field =
         LLVMBuildStructGEP(builder, null, vcode_get_field(op), "");
      LLVMValueRef scale =
         LLVMBuildPtrToInt(builder, field, LLVMInt32Type(), "");
      LLVMValueRef add = LLVMBuildAdd(builder, base, scale, "");
      ctx->regs[result] = LLVMBuildInsertValue(builder, signal,
                                               add, 1, cgen_reg_name(result));
   }
   else {
      ctx->regs[result] = LLVMBuildStructGEP(builder, cgen_get_arg(op, 0, ctx),
                                             vcode_get_field(op),
                                             cgen_reg_name(result));

      LLVMTypeRef field_type =
         LLVMGetElementType(LLVMTypeOf(ctx->regs[result]));
      if (LLVMGetTypeKind(field_type) == LLVMArrayTypeKind)
         ctx->regs[result] = cgen_array_pointer(ctx->regs[result]);
   }
}

static void cgen_op_sched_event(int op, cgen_ctx_t *ctx)
{
   LLVMValueRef sigptr = cgen_get_arg(op, 0, ctx);

   LLVMValueRef args[] = {
      LLVMBuildExtractValue(builder, sigptr, 0, "sid"),
      LLVMBuildExtractValue(builder, sigptr, 1, "offset"),
      cgen_get_arg(op, 1, ctx),
      llvm_int32(vcode_get_subkind(op)),
   };
   LLVMBuildCall(builder, llvm_fn("_sched_event"), args, ARRAY_LEN(args), "");
}

static void cgen_pcall_suspend(LLVMValueRef state, LLVMBasicBlockRef cont_bb,
                               cgen_ctx_t *ctx)
{
   LLVMBasicBlockRef suspend_bb = LLVMAppendBasicBlock(ctx->fn, "suspend");

   LLVMValueRef is_null = LLVMBuildIsNull(builder, state, "");
   LLVMBuildCondBr(builder, is_null, cont_bb, suspend_bb);

   LLVMPositionBuilderAtEnd(builder, suspend_bb);

   switch (vcode_unit_kind()) {
   case VCODE_UNIT_PROCEDURE:
      LLVMBuildRet(builder, llvm_void_cast(ctx->state));
      break;
   case VCODE_UNIT_PROCESS:
      LLVMBuildRet(builder, LLVMConstNull(llvm_void_ptr()));
      break;
   default:
      LLVMBuildRetVoid(builder);
   }

   LLVMPositionBuilderAtEnd(builder, cont_bb);
}

static void cgen_op_pcall(int op, bool nested, cgen_ctx_t *ctx)
{
   ident_t func = vcode_get_func(op);
   const int nargs = vcode_count_args(op);

   LLVMValueRef fn = cgen_signature(func, VCODE_INVALID_TYPE, VCODE_CC_VHDL,
                                    NULL, NULL, 0);
   if (fn == NULL) {
      vcode_type_t atypes[nargs];
      for (int i = 0; i < nargs; i++)
         atypes[i] = vcode_reg_type(vcode_get_arg(op, i));

      fn = cgen_signature(func, VCODE_INVALID_TYPE, VCODE_CC_VHDL,
                          cgen_display_type_for_call(op, ctx),
                          atypes, nargs);
   }

   const vcode_cc_t cc = vcode_get_subkind(op);
   const int total_args = nargs + (cc != VCODE_CC_FOREIGN ? 1 : 0) + 1;
   LLVMValueRef args[total_args];
   LLVMValueRef *ap = args;
   if (cc != VCODE_CC_FOREIGN)
      *ap++ = cgen_display_for_call(op, ctx);
   *ap++ = LLVMConstNull(llvm_void_ptr());
   for (int i = 0; i < nargs; i++)
      *ap++ = cgen_get_arg(op, i, ctx);

   LLVMValueRef suspend = LLVMBuildCall(builder, fn, args, total_args, "");

   assert(ctx->state);
   LLVMValueRef pcall_ptr = LLVMBuildStructGEP(builder, ctx->state, 2, "");
   LLVMBuildStore(builder, suspend, pcall_ptr);

   LLVMValueRef state_ptr = LLVMBuildStructGEP(builder, ctx->state, 1, "");
   LLVMBuildStore(builder, llvm_int32(vcode_get_target(op, 0)), state_ptr);

   cgen_pcall_suspend(suspend, ctx->blocks[vcode_get_target(op, 0)], ctx);
}

static void cgen_op_resume(int op, bool nested, cgen_ctx_t *ctx)
{
   LLVMBasicBlockRef after_bb = LLVMAppendBasicBlock(ctx->fn, "resume_after");
   LLVMBasicBlockRef call_bb  = LLVMAppendBasicBlock(ctx->fn, "resume_call");

   assert(ctx->state);
   LLVMValueRef pcall_ptr = LLVMBuildStructGEP(builder, ctx->state, 2, "");
   LLVMValueRef pcall_state = LLVMBuildLoad(builder, pcall_ptr, "");

   LLVMValueRef is_null = LLVMBuildIsNull(builder, pcall_state, "");
   LLVMBuildCondBr(builder, is_null, after_bb, call_bb);

   LLVMPositionBuilderAtEnd(builder, call_bb);

   const char *safe_name = safe_symbol(istr(vcode_get_func(op)));
   LLVMValueRef fn = LLVMGetNamedFunction(module, safe_name);
   assert(fn != NULL);

   LLVMTypeRef fn_type = LLVMGetElementType(LLVMTypeOf(fn));

   const int nparams = LLVMCountParamTypes(fn_type);
   assert(nparams >= 2);

   LLVMTypeRef param_types[nparams];
   LLVMGetParamTypes(fn_type, param_types);

   LLVMValueRef args[nparams];
   args[0] = LLVMConstNull(param_types[0]);
   args[1] = pcall_state;
   for (int i = 2; i < nparams; i++)
      args[i] = LLVMGetUndef(param_types[i]);

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

   LLVMBasicBlockRef entry_bb = LLVMGetInsertBlock(builder);

   LLVMBasicBlockRef test_bb = LLVMAppendBasicBlock(ctx->fn, "memcmp_test");
   LLVMBasicBlockRef body_bb = LLVMAppendBasicBlock(ctx->fn, "memcmp_body");
   LLVMBasicBlockRef exit_bb = LLVMAppendBasicBlock(ctx->fn, "memcmp_exit");

   LLVMBuildBr(builder, test_bb);

   // Loop test

   LLVMPositionBuilderAtEnd(builder, test_bb);

   LLVMValueRef i_test = LLVMBuildPhi(builder, LLVMInt32Type(), "i");
   LLVMValueRef len_ge = LLVMBuildICmp(builder, LLVMIntUGE, i_test,
                                       length, "len_ge");
   LLVMBuildCondBr(builder, len_ge, exit_bb, body_bb);

   // Loop body

   LLVMPositionBuilderAtEnd(builder, body_bb);

   LLVMValueRef index[] = { llvm_zext_to_intptr(i_test) };
   LLVMValueRef l_ptr = LLVMBuildInBoundsGEP(builder, lhs_data, index, 1, "");
   LLVMValueRef r_ptr = LLVMBuildInBoundsGEP(builder, rhs_data, index, 1, "");

   LLVMValueRef l_val = LLVMBuildLoad(builder, l_ptr, "l_val");
   LLVMValueRef r_val = LLVMBuildLoad(builder, r_ptr, "r_val");

   LLVMValueRef eq;
   if (LLVMGetTypeKind(LLVMTypeOf(l_val)) == LLVMDoubleTypeKind)
      eq = LLVMBuildFCmp(builder, LLVMRealUEQ, l_val, r_val, "eq");
   else
      eq = LLVMBuildICmp(builder, LLVMIntEQ, l_val, r_val, "eq");

   LLVMValueRef inc = LLVMBuildAdd(builder, i_test, llvm_int32(1), "inc");

   LLVMValueRef i_test_in_vals[]    = { llvm_int32(0), inc     };
   LLVMBasicBlockRef i_test_in_bb[] = { entry_bb,      body_bb };
   LLVMAddIncoming(i_test, i_test_in_vals, i_test_in_bb, 2);

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
      llvm_int1(false)
   };

   LLVMBuildCall(builder, llvm_fn("llvm.memset.p0i8.i32"),
                 args, ARRAY_LEN(args), "");
}

static void cgen_op_last_event(int op, cgen_ctx_t *ctx)
{
   LLVMValueRef sigptr = cgen_get_arg(op, 0, ctx);
   LLVMValueRef length =
      vcode_count_args(op) > 1 ? cgen_get_arg(op, 1, ctx) : llvm_int32(1);

   LLVMValueRef args[] = {
      LLVMBuildExtractValue(builder, sigptr, 0, "shared"),
      LLVMBuildExtractValue(builder, sigptr, 1, "offset"),
      length
   };

   vcode_reg_t result = vcode_get_result(op);
   ctx->regs[result] = LLVMBuildCall(builder, llvm_fn("_last_event"), args,
                                     ARRAY_LEN(args), cgen_reg_name(result));
}

static void cgen_op_last_active(int op, cgen_ctx_t *ctx)
{
   LLVMValueRef sigptr = cgen_get_arg(op, 0, ctx);
   LLVMValueRef length =
      vcode_count_args(op) > 1 ? cgen_get_arg(op, 1, ctx) : llvm_int32(1);

   LLVMValueRef args[] = {
      LLVMBuildExtractValue(builder, sigptr, 0, "shared"),
      LLVMBuildExtractValue(builder, sigptr, 1, "offset"),
      length
   };

   vcode_reg_t result = vcode_get_result(op);
   ctx->regs[result] = LLVMBuildCall(builder, llvm_fn("_last_active"), args,
                                     ARRAY_LEN(args), cgen_reg_name(result));
}

static void cgen_op_driving(int op, cgen_ctx_t *ctx)
{
   LLVMValueRef sigptr = cgen_get_arg(op, 0, ctx);
   LLVMValueRef length =
      vcode_count_args(op) > 1 ? cgen_get_arg(op, 1, ctx) : llvm_int32(1);

   LLVMValueRef args[] = {
      LLVMBuildExtractValue(builder, sigptr, 0, "shared"),
      LLVMBuildExtractValue(builder, sigptr, 1, "offset"),
      length
   };

   vcode_reg_t result = vcode_get_result(op);
   ctx->regs[result] = LLVMBuildCall(builder, llvm_fn("_driving"), args,
                                     ARRAY_LEN(args), cgen_reg_name(result));
}

static void cgen_op_driving_value(int op, cgen_ctx_t *ctx)
{
   LLVMValueRef sigptr = cgen_get_arg(op, 0, ctx);
   LLVMValueRef length =
      vcode_count_args(op) > 1 ? cgen_get_arg(op, 1, ctx) : llvm_int32(1);

   LLVMValueRef args[] = {
      LLVMBuildExtractValue(builder, sigptr, 0, "shared"),
      LLVMBuildExtractValue(builder, sigptr, 1, "offset"),
      length
   };

   LLVMValueRef raw = LLVMBuildCall(builder, llvm_fn("_driving_value"),
                                    args, ARRAY_LEN(args), "");

   vcode_reg_t result = vcode_get_result(op);
   ctx->regs[result] = LLVMBuildCast(builder, LLVMBitCast, raw,
                                     cgen_type(vcode_reg_type(result)),
                                     cgen_reg_name(result));
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
   LLVMValueRef file = cgen_get_arg(op, 0, ctx);

   LLVMTypeRef alloca_type;
   LLVMValueRef value = cgen_pointer_to_arg_data(op, 1, &alloca_type, ctx);

   LLVMTypeRef value_type = LLVMGetElementType(LLVMTypeOf(value));
   LLVMValueRef bytes = llvm_sizeof(value_type);

   LLVMValueRef length = bytes;
   if (vcode_count_args(op) == 3)
      length = LLVMBuildMul(builder, cgen_get_arg(op, 2, ctx), bytes, "");

   LLVMValueRef args[] = { file, llvm_void_cast(value), length };
   LLVMBuildCall(builder, llvm_fn("_file_write"), args, ARRAY_LEN(args), "");

   if (alloca_type != NULL)
      llvm_lifetime_end(value, alloca_type);
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
      cgen_location(op, ctx)
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

   LLVMTypeRef alloca_type = NULL;
   LLVMValueRef image_map;
   if (vcode_count_args(op) > 2)
      image_map = cgen_get_arg(op, 2, ctx);
   else {
      alloca_type = llvm_image_map();
      image_map = cgen_scoped_alloca(alloca_type, ctx);
      llvm_lifetime_start(image_map, alloca_type);
      const bool real = vcode_reg_kind(result) == VCODE_TYPE_REAL;
      LLVMBuildStore(
         builder,
         LLVMBuildInsertValue(builder,
                              LLVMGetUndef(alloca_type),
                              llvm_int32(real ? IMAGE_REAL : IMAGE_INTEGER),
                              0, ""),
         image_map);
   }

   LLVMValueRef args[] = {
      cgen_get_arg(op, 0, ctx),
      cgen_get_arg(op, 1, ctx),
      image_map,
      cgen_location(op, ctx)
   };
   ctx->regs[result] = LLVMBuildCall(builder, llvm_fn("_value_attr"),
                                      args, ARRAY_LEN(args), "value");

   if (alloca_type != NULL)
      llvm_lifetime_end(image_map, alloca_type);
}

static void cgen_op_bit_shift(int op, cgen_ctx_t *ctx)
{
   LLVMTypeRef utype = llvm_uarray_type(LLVMInt1Type(), 1);
   LLVMValueRef tmp = cgen_scoped_alloca(utype, ctx);
   llvm_lifetime_start(tmp, utype);

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
   llvm_lifetime_end(tmp, utype);
}

static void cgen_op_bit_vec_op(int op, cgen_ctx_t *ctx)
{
   LLVMTypeRef utype = llvm_uarray_type(LLVMInt1Type(), 1);
   LLVMValueRef tmp = cgen_scoped_alloca(utype, ctx);
   llvm_lifetime_start(tmp, utype);

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
   llvm_lifetime_end(tmp, utype);
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
      llvm_int32(vcode_get_subkind(op)),
      cgen_location(op, ctx),
      cgen_hint_str(op),
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
         cgen_location(op, ctx),
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

static void cgen_op_temp_stack_mark(int op, cgen_ctx_t *ctx)
{
   LLVMValueRef cur_ptr = LLVMGetNamedGlobal(module, "_tmp_alloc");

   vcode_reg_t result = vcode_get_result(op);
   ctx->regs[result] = LLVMBuildLoad(builder, cur_ptr, cgen_reg_name(result));
}

static void cgen_op_temp_stack_restore(int op, cgen_ctx_t *ctx)
{
   LLVMValueRef cur_ptr = LLVMGetNamedGlobal(module, "_tmp_alloc");
   LLVMBuildStore(builder, cgen_get_arg(op, 0, ctx), cur_ptr);
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

static void cgen_op_init_signal(int op, cgen_ctx_t *ctx)
{
   LLVMValueRef sigptr = cgen_get_arg(op, 0, ctx);

   const bool have_resolution = vcode_count_args(op) > 4;

   LLVMTypeRef res_alloca_type = NULL;
   LLVMValueRef resfn = NULL;
   if (have_resolution)
      resfn = cgen_pointer_to_arg_data(op, 4, &res_alloca_type, ctx);
   else
      resfn = LLVMConstNull(LLVMPointerType(llvm_resolution_type(), 0));

   LLVMTypeRef alloca_type;
   LLVMValueRef initval = cgen_pointer_to_arg_data(op, 1, &alloca_type, ctx);

   LLVMValueRef args[] = {
      LLVMBuildExtractValue(builder, sigptr, 0, "shared"),
      LLVMBuildExtractValue(builder, sigptr, 1, "offset"),
      cgen_get_arg(op, 2, ctx),
      cgen_get_arg(op, 3, ctx),
      llvm_void_cast(initval),
      resfn
   };
   LLVMBuildCall(builder, llvm_fn("_init_signal"), args, ARRAY_LEN(args), "");

   if (res_alloca_type != NULL)
      llvm_lifetime_end(resfn, res_alloca_type);
   if (alloca_type != NULL)
      llvm_lifetime_end(initval, alloca_type);
}

static void cgen_op_map_signal(int op, cgen_ctx_t *ctx)
{
   if (vcode_count_args(op) < 4)
      return;   // Not a source, nothing to do

   LLVMValueRef sigptr = cgen_get_arg(op, 0, ctx);

   LLVMTypeRef alloca_type;
   LLVMValueRef initval = cgen_pointer_to_arg_data(op, 3, &alloca_type, ctx);

   LLVMValueRef args[] = {
      LLVMBuildExtractValue(builder, sigptr, 0, "shared"),
      LLVMBuildExtractValue(builder, sigptr, 1, "offset"),
      cgen_get_arg(op, 2, ctx),
      llvm_void_cast(initval),
   };
   LLVMBuildCall(builder, llvm_fn("_source_signal"), args, ARRAY_LEN(args), "");

   if (alloca_type != NULL)
      llvm_lifetime_end(initval, alloca_type);
}

static void cgen_op_link_signal(int op, cgen_ctx_t *ctx)
{
   vcode_reg_t result = vcode_get_result(op);

   LLVMValueRef args[] = {
      cgen_const_string(istr(vcode_get_ident(op)))
   };
   LLVMValueRef shared = LLVMBuildCall(builder, llvm_fn("_link_signal"),
                                       args, ARRAY_LEN(args), "");

   LLVMValueRef r = LLVMGetUndef(llvm_signal_type());
   r = LLVMBuildInsertValue(builder, r, shared, 0, "");
   r = LLVMBuildInsertValue(builder, r, llvm_int32(0), 1,
                            cgen_reg_name(result));

   ctx->regs[result] = r;
}

static void cgen_op_link_var(int op, cgen_ctx_t *ctx)
{
   vcode_reg_t result = vcode_get_result(op);

   ident_t var_name = vcode_get_ident(op);
   ident_t unit_name = ident_runtil(var_name, '.');

   const char *unit_name_s = istr(unit_name);
   LLVMValueRef global = LLVMGetNamedGlobal(module, unit_name_s);
   if (global == NULL) {
      const char *type_name LOCAL = xasprintf("%s.state", istr(unit_name));

      LLVMTypeRef opaque = LLVMGetTypeByName(module, type_name);
      if (opaque == NULL)
         opaque = LLVMStructCreateNamed(LLVMGetGlobalContext(), type_name);

      global = LLVMAddGlobal(module, LLVMPointerType(opaque, 0), unit_name_s);
   }

   char *offset_name LOCAL =
      xasprintf("__offset_%s", safe_symbol(istr(var_name)));
   LLVMValueRef offset = LLVMGetNamedGlobal(module, offset_name);
   if (offset == NULL) {
      offset = LLVMAddGlobal(module, LLVMInt32Type(), offset_name);
      LLVMSetLinkage(offset, LLVMExternalLinkage);
      LLVMSetGlobalConstant(offset, true);
#ifdef IMPLIB_REQUIRED
      LLVMSetDLLStorageClass(global, LLVMDLLImportStorageClass);
#endif
   }

   LLVMValueRef base_ptr = llvm_void_cast(LLVMBuildLoad(builder, global, ""));

   LLVMValueRef indexes[] = {
      llvm_zext_to_intptr(LLVMBuildLoad(builder, offset, ""))
   };
   LLVMValueRef raw_ptr = LLVMBuildGEP(builder, base_ptr, indexes, 1, "");

   ctx->regs[result] = LLVMBuildPointerCast(builder, raw_ptr,
                                            cgen_type(vcode_reg_type(result)),
                                            cgen_reg_name(result));
}

static void cgen_op(int i, cgen_ctx_t *ctx)
{
   cgen_debug_loc(ctx, vcode_get_loc(i), false);

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
   case VCODE_OP_DRIVE_SIGNAL:
      break;
   case VCODE_OP_MAP_SIGNAL:
      cgen_op_map_signal(i, ctx);
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
   case VCODE_OP_RESOLVED:
      cgen_op_resolved(i, ctx);
      break;
   case VCODE_OP_LAST_VALUE:
      cgen_op_last_value(i, ctx);
      break;
   case VCODE_OP_SCHED_WAVEFORM:
      cgen_op_sched_waveform(i, ctx);
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
   case VCODE_OP_ADDRESS_OF:
      cgen_op_address_of(i, ctx);
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
   case VCODE_OP_LAST_ACTIVE:
      cgen_op_last_active(i, ctx);
      break;
   case VCODE_OP_DRIVING:
      cgen_op_driving(i, ctx);
      break;
   case VCODE_OP_DRIVING_VALUE:
      cgen_op_driving_value(i, ctx);
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
   case VCODE_OP_TEMP_STACK_MARK:
      cgen_op_temp_stack_mark(i, ctx);
      break;
   case VCODE_OP_TEMP_STACK_RESTORE:
      cgen_op_temp_stack_restore(i, ctx);
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
   case VCODE_OP_RANGE_NULL:
      cgen_op_range_null(i, ctx);
      break;
   case VCODE_OP_INIT_SIGNAL:
      cgen_op_init_signal(i, ctx);
      break;
   case VCODE_OP_LINK_SIGNAL:
      cgen_op_link_signal(i, ctx);
      break;
   case VCODE_OP_LINK_VAR:
      cgen_op_link_var(i, ctx);
      break;
   case VCODE_OP_RESOLUTION_WRAPPER:
      cgen_op_resolution_wrapper(i, ctx);
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

   ctx->regs   = xcalloc_array(nregs, sizeof(LLVMValueRef));
   ctx->blocks = xcalloc_array(nblocks, sizeof(LLVMBasicBlockRef));

   if (ctx->state == NULL)
      ctx->locals = xcalloc_array(vcode_count_vars(), sizeof(LLVMValueRef));

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
   const int p0 = cgen_is_procedure() ? 2 : 1;
   const int nparams = vcode_count_params();
   for (int i = 0; i < nparams; i++)
      ctx->regs[vcode_param_reg(i)] = LLVMGetParam(ctx->fn, p0 + i);
}

static void cgen_locals(cgen_ctx_t *ctx)
{
   LLVMPositionBuilderAtEnd(builder, ctx->blocks[0]);

   vcode_unit_t unit = vcode_active_unit();
   if (vcode_unit_child(unit) != NULL) {
      LLVMTypeRef state_type = cgen_state_type(unit);
      ctx->state = LLVMBuildAlloca(builder, state_type, "state");

      LLVMValueRef context_ptr = LLVMBuildStructGEP(builder, ctx->state, 0, "");
      LLVMBuildStore(builder, ctx->display, context_ptr);

      const int p0 = cgen_is_procedure() ? 2 : 1;
      const int nparams = vcode_count_params();
      for (int i = 0; i < nparams; i++) {
         const int offset = cgen_param_offset(i);
         LLVMValueRef ptr = LLVMBuildStructGEP(builder, ctx->state, offset, "");
         LLVMBuildStore(builder, LLVMGetParam(ctx->fn, p0 + i), ptr);
      }
   }
   else {
      const int nvars = vcode_count_vars();
      for (int i = 0; i < nvars; i++) {
         LLVMTypeRef lltype = cgen_type(vcode_var_type(i));
         const char *name = istr(vcode_var_name(i));

         LLVMValueRef mem;
         if (vcode_var_flags(i) & VAR_HEAP)
            mem = cgen_tmp_alloc(llvm_sizeof(lltype), lltype);
         else
            mem = LLVMBuildAlloca(builder, lltype, name);

         ctx->locals[i] = mem;
      }
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
                                    VCODE_CC_VHDL, display_type,
                                    params, nparams);
   cgen_add_func_attr(fn, FUNC_ATTR_UWTABLE, -1);

   // Do not add FUNC_ATTR_READONLY here: it can result in unexpected
   // optimisations, such as removing assertions.

   cgen_ctx_t ctx = {
      .fn = fn,
      .display = LLVMGetParam(fn, 0)
   };
   cgen_debug_push_func(&ctx);
   cgen_alloc_context(&ctx);

   cgen_params(&ctx);
   cgen_locals(&ctx);
   cgen_code(&ctx);
   cgen_free_context(&ctx);
   cgen_pop_debug_scope();
}

static LLVMTypeRef cgen_state_type(vcode_unit_t unit)
{
   vcode_state_t state;
   vcode_state_save(&state);

   vcode_select_unit(unit);

   char *LOCAL name = xasprintf("%s.state", istr(vcode_unit_name()));

   LLVMTypeRef exist = LLVMGetTypeByName(module, name);
   if (exist) {
      vcode_state_restore(&state);
      return exist;
   }

   const vunit_kind_t kind = vcode_unit_kind();
   const bool is_procedure = cgen_is_procedure();
   const bool has_fsm = (kind == VCODE_UNIT_PROCESS || is_procedure);
   const bool has_params = (kind == VCODE_UNIT_FUNCTION || is_procedure);

   vcode_unit_t context = vcode_unit_context();

   const int nparams = has_params ? vcode_count_params() : 0;
   const int nvars   = vcode_count_vars();
   const int nfields = nvars + nparams + (has_fsm ? 2 : 0) + !!context;

   int next_field = 0;
   LLVMTypeRef fields[nfields];

   if (context != NULL)
      fields[next_field++] = LLVMPointerType(cgen_state_type(context), 0);

   if (has_fsm) {
      fields[next_field++] = LLVMInt32Type();
      fields[next_field++] = llvm_void_ptr();
   }

   if (has_params) {
      for (int i = 0; i < nparams; i++)
         fields[next_field++] = cgen_type(vcode_param_type(i));
   }

   const int var_base = next_field;
   for (int i = 0; i < nvars; i++)
      fields[next_field++] = cgen_type(vcode_var_type(i));

   assert(next_field == nfields);

   LLVMTypeRef new = LLVMStructCreateNamed(LLVMGetGlobalContext(), name);
   LLVMStructSetBody(new, fields, nfields, false);

   for (int i = 0; i < nvars; i++) {
      if (vcode_var_flags(i) & VAR_GLOBAL) {
         char *name LOCAL =
            xasprintf("__offset_%s", safe_symbol(istr(vcode_var_name(i))));
         LLVMValueRef global = LLVMAddGlobal(module, LLVMInt32Type(), name);
#ifdef IMPLIB_REQUIRED
         LLVMSetDLLStorageClass(global, LLVMDLLExportStorageClass);
#endif
         LLVMTargetDataRef td = LLVMGetModuleDataLayout(module);
         const size_t offset = LLVMOffsetOfElement(td, new, var_base + i);
         LLVMSetInitializer(global, llvm_int32(offset));
         LLVMSetGlobalConstant(global, true);
         LLVMSetUnnamedAddr(global, true);
      }
   }

   vcode_state_restore(&state);
   return new;
}

static void cgen_jump_table(cgen_ctx_t *ctx)
{
   assert(ctx->state != NULL);

   LLVMValueRef state_ptr = LLVMBuildStructGEP(builder, ctx->state, 1, "");
   LLVMValueRef jtarget = LLVMBuildLoad(builder, state_ptr, "");
   LLVMValueRef jswitch = LLVMBuildSwitch(builder, jtarget,
                                          ctx->blocks[0], 10);

   const int nblocks = vcode_count_blocks();
   bool *have LOCAL = xcalloc(sizeof(bool) * nblocks);
   for (int i = 0; i < nblocks; i++) {
      vcode_select_block(i);

      const int last = vcode_count_ops() - 1;
      if (last < 0)
         continue;

      vcode_block_t target = VCODE_INVALID_BLOCK;
      vcode_op_t last_op = vcode_get_op(last);

      if (last_op == VCODE_OP_WAIT || last_op == VCODE_OP_PCALL
          || last_op == VCODE_OP_NESTED_PCALL)
         target = vcode_get_target(last, 0);
      else if (vcode_unit_kind() == VCODE_UNIT_PROCESS
               && last_op == VCODE_OP_RETURN)
         target = 1;

      if (target == VCODE_INVALID_BLOCK || have[target])
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
                                    VCODE_CC_VHDL, display_type,
                                    params, nparams);
   cgen_add_func_attr(fn, FUNC_ATTR_UWTABLE, -1);

   cgen_ctx_t ctx = {
      .fn = fn,
   };

   LLVMBasicBlockRef entry_bb = LLVMAppendBasicBlock(fn, "entry");
   LLVMBasicBlockRef alloc_bb = LLVMAppendBasicBlock(fn, "alloc");
   LLVMBasicBlockRef jump_bb  = LLVMAppendBasicBlock(fn, "jump_table");

   cgen_debug_push_func(&ctx);
   cgen_alloc_context(&ctx);
   cgen_params(&ctx);

   LLVMPositionBuilderAtEnd(builder, entry_bb);

   LLVMTypeRef state_type = cgen_state_type(vcode_active_unit());
   LLVMTypeRef pointer_type = LLVMPointerType(state_type, 0);
   LLVMValueRef old_state =
      LLVMBuildPointerCast(builder, LLVMGetParam(fn, 1),
                           pointer_type, "old_state");

   LLVMValueRef is_null = LLVMBuildIsNull(builder, old_state, "");
   LLVMBuildCondBr(builder, is_null, alloc_bb, jump_bb);

   LLVMPositionBuilderAtEnd(builder, alloc_bb);

   LLVMValueRef new_state = LLVMBuildMalloc(builder, state_type, "new_state");

   LLVMValueRef display_ptr = LLVMBuildStructGEP(builder, new_state, 0, "");
   LLVMBuildStore(builder, LLVMGetParam(fn, 0), display_ptr);

   LLVMValueRef state_ptr = LLVMBuildStructGEP(builder, new_state, 1, "");
   LLVMBuildStore(builder, llvm_int32(0), state_ptr);

   for (int i = 0; i < nparams; i++) {
      LLVMValueRef param_ptr =
         LLVMBuildStructGEP(builder, new_state, cgen_param_offset(i), "");
      LLVMBuildStore(builder, ctx.regs[vcode_param_reg(i)], param_ptr);
   }

   LLVMBuildBr(builder, jump_bb);

   LLVMPositionBuilderAtEnd(builder, jump_bb);

   ctx.state = LLVMBuildPhi(builder, pointer_type, "state");

   LLVMValueRef phi_values[]   = { old_state, new_state };
   LLVMBasicBlockRef phi_bbs[] = { entry_bb,  alloc_bb  };
   LLVMAddIncoming(ctx.state, phi_values, phi_bbs, 2);

   LLVMValueRef new_display_ptr = LLVMBuildStructGEP(builder, ctx.state, 0, "");
   ctx.display = LLVMBuildLoad(builder, new_display_ptr, "display");

   for (int i = 0; i < nparams; i++) {
      LLVMValueRef param_ptr =
         LLVMBuildStructGEP(builder, ctx.state, cgen_param_offset(i), "");
      ctx.regs[vcode_param_reg(i)] = LLVMBuildLoad(builder, param_ptr, "");
   }

   cgen_jump_table(&ctx);
   cgen_code(&ctx);
   cgen_free_context(&ctx);
   cgen_pop_debug_scope();
}

static void cgen_process(vcode_unit_t code)
{
   vcode_select_unit(code);
   assert(vcode_unit_kind() == VCODE_UNIT_PROCESS);

   cgen_ctx_t ctx = {};

   LLVMTypeRef display_type = cgen_state_type(vcode_unit_context());
   LLVMTypeRef state_type = cgen_state_type(vcode_active_unit());

   LLVMTypeRef pargs[] = {
      LLVMPointerType(state_type, 0),
      LLVMPointerType(display_type, 0)
   };
   LLVMTypeRef ftype = LLVMFunctionType(llvm_void_ptr(), pargs, 2, false);
   const char *name = safe_symbol(istr(vcode_unit_name()));
   ctx.fn = LLVMAddFunction(module, name, ftype);
   cgen_add_func_attr(ctx.fn, FUNC_ATTR_NOUNWIND, -1);
   cgen_add_func_attr(ctx.fn, FUNC_ATTR_DLLEXPORT, -1);
   cgen_add_func_attr(ctx.fn, FUNC_ATTR_UWTABLE, -1);

   LLVMBasicBlockRef entry_bb = LLVMAppendBasicBlock(ctx.fn, "entry");
   LLVMBasicBlockRef reset_bb = LLVMAppendBasicBlock(ctx.fn, "reset");
   LLVMBasicBlockRef jump_bb  = LLVMAppendBasicBlock(ctx.fn, "jump_table");

   ctx.state   = LLVMGetParam(ctx.fn, 0);
   ctx.display = LLVMGetParam(ctx.fn, 1);

   cgen_debug_push_func(&ctx);
   cgen_alloc_context(&ctx);

   // If the parameter is non-zero jump to the init block

   LLVMPositionBuilderAtEnd(builder, entry_bb);
   LLVMValueRef reset = LLVMBuildIsNull(builder, ctx.state, "reset");
   LLVMBuildCondBr(builder, reset, reset_bb, jump_bb);

   LLVMPositionBuilderAtEnd(builder, reset_bb);

   LLVMValueRef priv_ptr = LLVMBuildMalloc(builder, state_type, "privdata");

   LLVMValueRef context_ptr = LLVMBuildStructGEP(builder, priv_ptr, 0, "");
   LLVMBuildStore(builder, ctx.display, context_ptr);

   LLVMValueRef state_ptr = LLVMBuildStructGEP(builder, priv_ptr, 1, "");
   LLVMBuildStore(builder, llvm_int32(0), state_ptr);

   LLVMValueRef pcall_ptr = LLVMBuildStructGEP(builder, priv_ptr, 2, "");
   LLVMBuildStore(builder, LLVMConstNull(llvm_void_ptr()), pcall_ptr);

   // Schedule the process to run immediately
   cgen_sched_process(llvm_int64(0));

   LLVMBuildBr(builder, jump_bb);

   LLVMPositionBuilderAtEnd(builder, jump_bb);

   ctx.state = LLVMBuildPhi(builder, LLVMPointerType(state_type, 0), "state");

   LLVMValueRef phi_values[]   = { LLVMGetParam(ctx.fn, 0), priv_ptr };
   LLVMBasicBlockRef phi_bbs[] = { entry_bb,                reset_bb };
   LLVMAddIncoming(ctx.state, phi_values, phi_bbs, 2);

   cgen_jump_table(&ctx);
   cgen_code(&ctx);
   cgen_free_context(&ctx);
   cgen_pop_debug_scope();
}

static void cgen_reset_function(void)
{
   cgen_ctx_t ctx = {};

   LLVMTypeRef state_type = cgen_state_type(vcode_active_unit());

   ident_t unit_name = vcode_unit_name();
   char *name LOCAL = xasprintf("%s_reset", safe_symbol(istr(unit_name)));

   vcode_unit_t context = vcode_unit_context();
   LLVMTypeRef display_type = NULL, ftype;
   if (context != NULL) {
      display_type = cgen_state_type(context);

      LLVMTypeRef args[] = {
         LLVMPointerType(display_type, 0)
      };
      ftype = LLVMFunctionType(llvm_void_ptr(), args, 1, false);
   }
   else
      ftype = LLVMFunctionType(llvm_void_ptr(), NULL, 0, false);

   ctx.fn = LLVMAddFunction(module, name, ftype);
   cgen_add_func_attr(ctx.fn, FUNC_ATTR_DLLEXPORT, -1);
   cgen_add_func_attr(ctx.fn, FUNC_ATTR_UWTABLE, -1);

   LLVMBasicBlockRef entry_bb = LLVMAppendBasicBlock(ctx.fn, "entry");
   LLVMPositionBuilderAtEnd(builder, entry_bb);

   cgen_debug_push_func(&ctx);
   cgen_alloc_context(&ctx);

   ctx.state = LLVMBuildMalloc(builder, state_type, "privdata");

   if (context != NULL) {
      ctx.display = LLVMGetParam(ctx.fn, 0);
      LLVMValueRef context_ptr = LLVMBuildStructGEP(builder, ctx.state, 0, "");
      LLVMBuildStore(builder, ctx.display, context_ptr);
   }

   if (vcode_unit_kind() == VCODE_UNIT_PACKAGE) {
      LLVMValueRef global = LLVMAddGlobal(module,
                                          LLVMPointerType(state_type, 0),
                                          istr(vcode_unit_name()));
      LLVMSetInitializer(global, LLVMGetUndef(LLVMPointerType(state_type, 0)));
#ifdef IMPLIB_REQUIRED
      LLVMSetDLLStorageClass(global, LLVMDLLExportStorageClass);
#endif
      LLVMBuildStore(builder, ctx.state, global);
   }

   LLVMBuildBr(builder, ctx.blocks[0]);

   cgen_code(&ctx);
   cgen_free_context(&ctx);
   cgen_pop_debug_scope();
}

static void cgen_coverage_state(tree_t t, cover_tagging_t *tagging)
{
   int32_t stmt_tags, cond_tags;
   cover_count_tags(tagging, &stmt_tags, &cond_tags);

   if (stmt_tags > 0) {
      LLVMTypeRef type = LLVMArrayType(LLVMInt32Type(), stmt_tags);
      LLVMValueRef var = LLVMAddGlobal(module, type, "cover_stmts");
      LLVMSetInitializer(var, LLVMGetUndef(type));
      cgen_add_func_attr(var, FUNC_ATTR_DLLEXPORT, -1);
   }

   if (cond_tags > 0) {
      LLVMTypeRef type = LLVMArrayType(LLVMInt32Type(), stmt_tags);
      LLVMValueRef var = LLVMAddGlobal(module, type, "cover_conds");
      LLVMSetInitializer(var, LLVMGetUndef(type));
      cgen_add_func_attr(var, FUNC_ATTR_DLLEXPORT, -1);
   }
}

static void cgen_children(vcode_unit_t vcode)
{
   vcode_select_unit(vcode);

   LLVMTypeRef display = NULL;
   for (vcode_unit_t it = vcode_unit_child(vcode);
        it != NULL;
        it = vcode_unit_next(it)) {

      vcode_select_unit(it);

      switch (vcode_unit_kind()) {
      case VCODE_UNIT_PROCEDURE:
      case VCODE_UNIT_FUNCTION:
         cgen_children(it);
         if (display == NULL)
            display = cgen_state_type(vcode);
         vcode_select_unit(it);
         if (vcode_unit_kind() == VCODE_UNIT_FUNCTION)
            cgen_function(display);
         else
            cgen_procedure(display);
         break;
      case VCODE_UNIT_PROCESS:
         cgen_children(it);
         cgen_process(it);
         break;
      case VCODE_UNIT_INSTANCE:
         cgen_children(it);
         vcode_select_unit(it);
         cgen_reset_function();
         break;
      default:
         break;
      }
   }
}

static void cgen_module_debug_info(void)
{
   llvm_add_module_flag("Debug Info Version", DEBUG_METADATA_VERSION);
#ifdef __APPLE__
   llvm_add_module_flag("Dwarf Version", 2);
#else
   llvm_add_module_flag("Dwarf Version", 4);
#endif

   const loc_t *loc = vcode_unit_loc();
   assert(!loc_invalid_p(loc));

   const char *file_path = loc_file_str(loc);

   char *basec LOCAL = xstrdup(file_path);
   char *dirc LOCAL = xstrdup(file_path);

   const char *file = basename(basec);
   const size_t file_len = strlen(file);

   const char *dir = dirname(dirc);
   const size_t dir_len = strlen(dir);

   LLVMMetadataRef file_ref =
      LLVMDIBuilderCreateFile(debuginfo, file, file_len, dir, dir_len);

#ifndef LLVM_HAVE_DI_SCOPE_GET_FILE
   debug_file = file_ref;
#endif

   LLVMMetadataRef cu = LLVMDIBuilderCreateCompileUnit(
      debuginfo, LLVMDWARFSourceLanguageAda83,
      file_ref, PACKAGE, sizeof(PACKAGE) - 1,
      opt_get_int("optimise"), "", 0,
      0, "", 0,
      LLVMDWARFEmissionFull, 0, false, false
#if LLVM_CREATE_CU_HAS_SYSROOT
      , "/", 1, "", 0
#endif
   );

   cgen_push_debug_scope(cu);

   size_t name_len;
   const char *module_name = LLVMGetModuleIdentifier(module, &name_len);
   LLVMMetadataRef mod = LLVMDIBuilderCreateModule(
      debuginfo, cu, module_name, name_len, "", 0, "", 0, "", 0);

   cgen_push_debug_scope(mod);
}

static void cgen_top(tree_t top, vcode_unit_t vcode, cover_tagging_t *cover)
{
   vcode_select_unit(vcode);

   cgen_module_debug_info();
   cgen_coverage_state(top, cover);
   cgen_reset_function();
   cgen_children(vcode);
   cgen_pop_debug_scope();
   cgen_pop_debug_scope();
}

static void cgen_dump_module(const char *tag)
{
   size_t length;
   const char *module_name = LLVMGetModuleIdentifier(module, &length);
   char *file_name LOCAL = xasprintf("_%s.%s.ll", module_name, tag);

   char file_path[PATH_MAX];
   lib_realpath(lib_work(), file_name, file_path, sizeof(file_path));

   char *error;
   if (LLVMPrintModuleToFile(module, file_path, &error))
      fatal("Failed to write LLVM IR file: %s", error);

   notef("wrote LLVM IR to %s", file_path);
}

static void cgen_optimise(void)
{
   LLVMPassManagerRef fpm = LLVMCreateFunctionPassManagerForModule(module);
   LLVMPassManagerRef mpm = LLVMCreatePassManager();

   const int olevel = opt_get_int("optimise");

   LLVMPassManagerBuilderRef builder = LLVMPassManagerBuilderCreate();
   LLVMPassManagerBuilderSetOptLevel(builder, olevel);
   LLVMPassManagerBuilderSetSizeLevel(builder, 0);

   if (olevel >= 2)
      LLVMPassManagerBuilderUseInlinerWithThreshold(builder, 50);

   LLVMPassManagerBuilderPopulateFunctionPassManager(builder, fpm);
   LLVMPassManagerBuilderPopulateModulePassManager(builder, mpm);
   LLVMPassManagerBuilderDispose(builder);

   LLVMInitializeFunctionPassManager(fpm);

   for (LLVMValueRef fn = LLVMGetFirstFunction(module);
        fn != NULL; fn = LLVMGetNextFunction(fn))
      LLVMRunFunctionPassManager(fpm, fn);

   LLVMFinalizeFunctionPassManager(fpm);
   LLVMDisposePassManager(fpm);

   progress("running function pass manager");

   LLVMRunPassManager(mpm, module);
   LLVMDisposePassManager(mpm);

   progress("running module pass manager");
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
         LLVMPointerType(llvm_signal_shared_struct(), 0),
         LLVMInt32Type(),
         llvm_void_ptr(),
         LLVMInt32Type(),
         LLVMInt64Type(),
         LLVMInt64Type()
      };
      fn = LLVMAddFunction(module, "_sched_waveform",
                           LLVMFunctionType(LLVMVoidType(),
                                            args, ARRAY_LEN(args), false));
      cgen_add_func_attr(fn, FUNC_ATTR_NOCAPTURE, 1);
      cgen_add_func_attr(fn, FUNC_ATTR_NOCAPTURE, 3);
      cgen_add_func_attr(fn, FUNC_ATTR_READONLY, 3);
   }
   else if (strcmp(name, "_sched_waveform_s") == 0) {
      LLVMTypeRef args[] = {
         LLVMPointerType(llvm_signal_shared_struct(), 0),
         LLVMInt32Type(),
         LLVMInt64Type(),
         LLVMInt64Type(),
         LLVMInt64Type()
      };
      fn = LLVMAddFunction(module, "_sched_waveform_s",
                           LLVMFunctionType(LLVMVoidType(),
                                            args, ARRAY_LEN(args), false));
      cgen_add_func_attr(fn, FUNC_ATTR_NOCAPTURE, 1);
   }
   else if (strcmp(name, "_sched_event") == 0) {
      LLVMTypeRef args[] = {
         LLVMPointerType(llvm_signal_shared_struct(), 0),
         LLVMInt32Type(),
         LLVMInt32Type(),
         LLVMInt32Type()
      };
      fn = LLVMAddFunction(module, "_sched_event",
                           LLVMFunctionType(LLVMVoidType(),
                                            args, ARRAY_LEN(args), false));
      cgen_add_func_attr(fn, FUNC_ATTR_NOCAPTURE, 1);
      cgen_add_func_attr(fn, FUNC_ATTR_READONLY, 1);
   }
   else if (strcmp(name, "_init_signal") == 0) {
      LLVMTypeRef args[] = {
         LLVMPointerType(llvm_signal_shared_struct(), 0),
         LLVMInt32Type(),
         LLVMInt32Type(),
         LLVMInt32Type(),
         llvm_void_ptr(),
         LLVMPointerType(llvm_resolution_type(), 0)
      };
      fn = LLVMAddFunction(module, "_init_signal",
                           LLVMFunctionType(LLVMVoidType(),
                                            args, ARRAY_LEN(args), false));
      cgen_add_func_attr(fn, FUNC_ATTR_NOCAPTURE, 1);
      cgen_add_func_attr(fn, FUNC_ATTR_NOCAPTURE, 5);
      cgen_add_func_attr(fn, FUNC_ATTR_READONLY, 5);
      cgen_add_func_attr(fn, FUNC_ATTR_NOCAPTURE, 6);
      cgen_add_func_attr(fn, FUNC_ATTR_READONLY, 6);
   }
   else if (strcmp(name, "_source_signal") == 0) {
      LLVMTypeRef args[] = {
         LLVMPointerType(llvm_signal_shared_struct(), 0),
         LLVMInt32Type(),
         LLVMInt32Type(),
         llvm_void_ptr(),
      };
      fn = LLVMAddFunction(module, "_source_signal",
                           LLVMFunctionType(LLVMVoidType(),
                                            args, ARRAY_LEN(args), false));
      cgen_add_func_attr(fn, FUNC_ATTR_NOCAPTURE, 1);
      cgen_add_func_attr(fn, FUNC_ATTR_NOCAPTURE, 4);
   }
   else if (strcmp(name, "_link_signal") == 0) {
      LLVMTypeRef args[] = {
         LLVMPointerType(LLVMInt8Type(), 0)
      };
      LLVMTypeRef result_type = LLVMPointerType(llvm_signal_shared_struct(), 0);
      fn = LLVMAddFunction(module, "_link_signal",
                           LLVMFunctionType(result_type,
                                            args, ARRAY_LEN(args), false));
      cgen_add_func_attr(fn, FUNC_ATTR_NOCAPTURE, 1);
      cgen_add_func_attr(fn, FUNC_ATTR_READONLY, 1);
      cgen_add_func_attr(fn, FUNC_ATTR_NONNULL, 0);
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
      cgen_add_func_attr(fn, FUNC_ATTR_NOCAPTURE, 1);
      cgen_add_func_attr(fn, FUNC_ATTR_NOCAPTURE, 5);
      cgen_add_func_attr(fn, FUNC_ATTR_COLD, -1);
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
      cgen_add_func_attr(fn, FUNC_ATTR_WRITEONLY, 3);
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
         LLVMInt1Type()
      };
      fn = LLVMAddFunction(module, "llvm.memset.p0i8.i32",
                           LLVMFunctionType(LLVMVoidType(),
                                            args, ARRAY_LEN(args), false));
   }
   else if (strncmp(name, "llvm.mem", 8) == 0) {
      int width;
      char kind[17];
      if (sscanf(name, "llvm.%16[^.].p0i%d", kind, &width) != 2)
         fatal("invalid memcpy intrinsic %s", name);

      LLVMTypeRef args[] = {
         LLVMPointerType(LLVMIntType(width), 0),
         LLVMPointerType(LLVMIntType(width), 0),
         LLVMInt32Type(),
         LLVMInt1Type()
      };
      fn = LLVMAddFunction(module, cgen_memcpy_name(kind, width),
                           LLVMFunctionType(LLVMVoidType(),
                                            args, ARRAY_LEN(args), false));
   }
   else if (strcmp(name, "llvm.lifetime.start.p0i8") == 0) {
      LLVMTypeRef args[] = {
         LLVMInt64Type(),
         llvm_void_ptr()
      };
      fn = LLVMAddFunction(module, "llvm.lifetime.start.p0i8",
                           LLVMFunctionType(LLVMVoidType(), args, 2, false));
   }
   else if (strcmp(name, "llvm.lifetime.end.p0i8") == 0) {
      LLVMTypeRef args[] = {
         LLVMInt64Type(),
         llvm_void_ptr()
      };
      fn = LLVMAddFunction(module, "llvm.lifetime.end.p0i8",
                           LLVMFunctionType(LLVMVoidType(), args, 2, false));
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
      cgen_add_func_attr(fn, FUNC_ATTR_COLD, -1);
   }
   else if (strcmp(name, "_div_zero") == 0) {
      LLVMTypeRef args[] = {
         LLVMPointerType(llvm_rt_loc(), 0)
      };
      fn = LLVMAddFunction(module, "_div_zero",
                           LLVMFunctionType(LLVMVoidType(),
                                            args, ARRAY_LEN(args), false));
      cgen_add_func_attr(fn, FUNC_ATTR_NORETURN, -1);
      cgen_add_func_attr(fn, FUNC_ATTR_COLD, -1);
   }
   else if (strcmp(name, "_null_deref") == 0) {
      LLVMTypeRef args[] = {
         LLVMPointerType(llvm_rt_loc(), 0)
      };
      fn = LLVMAddFunction(module, "_null_deref",
                           LLVMFunctionType(LLVMVoidType(),
                                            args, ARRAY_LEN(args), false));
      cgen_add_func_attr(fn, FUNC_ATTR_NORETURN, -1);
      cgen_add_func_attr(fn, FUNC_ATTR_COLD, -1);
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
         LLVMPointerType(llvm_signal_shared_struct(), 0),
         LLVMInt32Type(),
         LLVMInt32Type(),
         LLVMInt32Type()
      };
      fn = LLVMAddFunction(module, "_test_net_flag",
                           LLVMFunctionType(LLVMInt1Type(),
                                            args, ARRAY_LEN(args), false));
   }
   else if (strcmp(name, "_last_event") == 0) {
      LLVMTypeRef args[] = {
         LLVMPointerType(llvm_signal_shared_struct(), 0),
         LLVMInt32Type(),
         LLVMInt32Type()
      };
      fn = LLVMAddFunction(module, "_last_event",
                           LLVMFunctionType(LLVMInt64Type(),
                                            args, ARRAY_LEN(args), false));
   }
   else if (strcmp(name, "_last_active") == 0) {
      LLVMTypeRef args[] = {
         LLVMPointerType(llvm_signal_shared_struct(), 0),
         LLVMInt32Type(),
         LLVMInt32Type()
      };
      fn = LLVMAddFunction(module, "_last_active",
                           LLVMFunctionType(LLVMInt64Type(),
                                            args, ARRAY_LEN(args), false));
   }
   else if (strcmp(name, "_driving") == 0) {
      LLVMTypeRef args[] = {
         LLVMPointerType(llvm_signal_shared_struct(), 0),
         LLVMInt32Type(),
         LLVMInt32Type()
      };
      fn = LLVMAddFunction(module, "_driving",
                           LLVMFunctionType(LLVMInt1Type(),
                                            args, ARRAY_LEN(args), false));
   }
   else if (strcmp(name, "_driving_value") == 0) {
      LLVMTypeRef args[] = {
         LLVMPointerType(llvm_signal_shared_struct(), 0),
         LLVMInt32Type(),
         LLVMInt32Type()
      };
      fn = LLVMAddFunction(module, "_driving_value",
                           LLVMFunctionType(llvm_void_ptr(),
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

static void cgen_abi_version(void)
{
   LLVMValueRef global =
      LLVMAddGlobal(module, LLVMInt32Type(), "__nvc_abi_version");
   LLVMSetInitializer(global, llvm_int32(RT_ABI_VERSION));
   LLVMSetGlobalConstant(global, true);
#ifdef IMPLIB_REQUIRED
   LLVMSetDLLStorageClass(global, LLVMDLLExportStorageClass);
#endif
}

static void cgen_link_arg(const char *fmt, ...)
{
   va_list ap;
   va_start(ap, fmt);
   char *buf = xvasprintf(fmt, ap);
   va_end(ap);

   APUSH(link_args, buf);
}

#ifdef IMPLIB_REQUIRED
static void cgen_find_dll_deps(ident_t unit_name, ident_list_t **deps)
{
   tree_t unit = lib_get_qualified(unit_name);
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
         cgen_find_dll_deps(body_name, deps);
      }
      break;

   case T_PACK_BODY:
      {
         ident_t pack_name = ident_until(unit_name, '-');
         cgen_find_dll_deps(pack_name, deps);
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

      cgen_find_dll_deps(tree_ident(c), deps);
   }
}
#endif  // IMPLIB_REQUIRED

static void cgen_native(LLVMTargetMachineRef tm_ref)
{
   size_t name_len;
   const char *module_name = LLVMGetModuleIdentifier(module, &name_len);
   char *obj_name LOCAL = xasprintf("_%s." LLVM_OBJ_EXT, module_name);

   char obj_path[PATH_MAX];
   lib_realpath(lib_work(), obj_name, obj_path, sizeof(obj_path));

   char *error;
   if (LLVMTargetMachineEmitToFile(tm_ref, module, obj_path,
                                   LLVMObjectFile, &error))
      fatal("Failed to write object file: %s", error);

   progress("generating native code");

#if DUMP_ASSEMBLY
   char *asm_name LOCAL = xasprintf("_%s.s", module_name);

   char asm_path[PATH_MAX];
   lib_realpath(lib_work(), asm_name, asm_path, sizeof(asm_path));

    if (LLVMTargetMachineEmitToFile(tm_ref, module, asm_path,
                                   LLVMAssemblyFile, &error))
      fatal("Failed to write assembly file: %s", error);

    progress("writing assembly");
#endif

#if DUMP_BITCODE
    char *bc_name LOCAL = xasprintf("_%s.bc", module_name);

    char bc_path[PATH_MAX];
    lib_realpath(lib_work(), bc_name, bc_path, sizeof(bc_path));

    if (LLVMWriteBitcodeToFile(module, bc_path))
       fatal("Failed to write bitcode to file");

    progress("writing bitcode");
#endif


#ifdef LINKER_PATH
   cgen_link_arg("%s", LINKER_PATH);
   cgen_link_arg("--eh-frame-hdr");
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

   char *fname LOCAL = xasprintf("_%s." DLL_EXT, module_name);
   char so_path[PATH_MAX];
   lib_realpath(lib_work(), fname, so_path, PATH_MAX);

   cgen_link_arg("-o");
   cgen_link_arg("%s", so_path);
   cgen_link_arg("%s", obj_path);

#if IMPLIB_REQUIRED
   // Windows needs all symbols to be resolved when linking a DLL

   cgen_link_arg("-L%s", lib_path(lib_work()));

   ident_t name = ident_new(module_name);
   LOCAL_IDENT_LIST deps = NULL;
   cgen_find_dll_deps(name, &deps);

   for (const ident_list_t *it = deps; it != NULL; it = it->next) {
      if (it->ident == name)
         continue;

      lib_t lib = lib_find(ident_until(it->ident, '.'), true);

      char *dll_name LOCAL = xasprintf("_%s." DLL_EXT, istr(it->ident));
      char dll_path[PATH_MAX];
      lib_realpath(lib, dll_name, dll_path, PATH_MAX);

      if (access(dll_path, F_OK) == 0) {
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

   APUSH(link_args, NULL);

   run_program((const char * const *)link_args.items, link_args.count);

   progress("linking shared library");

   for (size_t i = 0; i < link_args.count; i++)
      free(link_args.items[i]);
   ACLEAR(link_args);
}

void cgen(tree_t top, vcode_unit_t vcode, cover_tagging_t *cover)
{
   vcode_select_unit(vcode);

   ident_t name = tree_ident(top);
   if (tree_kind(top) == T_PACK_BODY)
      name = ident_strip(name, ident_new("-body"));

   module = LLVMModuleCreateWithName(istr(name));
   builder = LLVMCreateBuilder();
   debuginfo = LLVMCreateDIBuilderDisallowUnresolved(module);

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

   LLVMTargetDataRef data_ref = LLVMCreateTargetDataLayout(tm_ref);
   LLVMSetModuleDataLayout(module, data_ref);

   string_pool = hash_new(128, true);

   cgen_abi_version();
   cgen_tmp_stack();

   cgen_top(top, vcode, cover);

   LLVMDIBuilderFinalize(debuginfo);

   progress("generating LLVM IR");

   const bool dump_ir = opt_get_int("dump-llvm");
   if (dump_ir) cgen_dump_module("initial");

   if (getenv("NVC_CGEN_VERBOSE") != NULL)
      LLVMDumpModule(module);

   if (LLVMVerifyModule(module, LLVMPrintMessageAction, NULL))
      fatal("LLVM verification failed");

   progress("verifying module");

   vcode_select_unit(vcode);

   cgen_optimise();
   cgen_native(tm_ref);

   if (dump_ir) cgen_dump_module("final");

   assert(debug_scopes.count == 0);
   hash_free(string_pool);

   LLVMDisposeModule(module);
   LLVMDisposeBuilder(builder);
   LLVMDisposeDIBuilder(debuginfo);
   LLVMDisposeTargetMachine(tm_ref);
   LLVMDisposeTargetData(data_ref);
   LLVMDisposeMessage(def_triple);
}
