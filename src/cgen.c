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
#include "rt/ffi.h"

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

#define DEBUG_METADATA_VERSION 3
#define CONST_REP_ARRAY_LIMIT  32
#define UNITS_PER_JOB          25

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
   FUNC_ATTR_OPTNONE,

   // Attributes requiring special handling
   FUNC_ATTR_PRESERVE_FP,
   FUNC_ATTR_DLLEXPORT,
} func_attr_t;

typedef A(vcode_unit_t) unit_list_t;

typedef struct {
   unit_list_t  units;
   char        *obj_path;
   char        *module_name;
   unsigned     index;
} cgen_job_t;

typedef A(LLVMValueRef) llvm_value_list_t;
typedef A(cgen_job_t) job_list_t;

typedef struct {
   job_list_t      *jobs;
   nvc_mutex_t     *lock;
   cover_tagging_t *cover;
   tree_t           top;
} cgen_thread_params_t;

static __thread LLVMModuleRef       module = NULL;
static __thread LLVMBuilderRef      builder = NULL;
static __thread LLVMDIBuilderRef    debuginfo = NULL;
static __thread shash_t            *string_pool = NULL;
static __thread A(LLVMMetadataRef)  debug_scopes;
static __thread LLVMContextRef      thread_context = NULL;
static __thread llvm_value_list_t   ctors;

static A(char *) link_args;

static LLVMValueRef cgen_support_fn(const char *name);
static LLVMTypeRef cgen_state_type(vcode_unit_t unit);

static inline LLVMContextRef llvm_context(void)
{
   return thread_context;
}

static LLVMTypeRef llvm_void_type(void)
{
   return LLVMVoidTypeInContext(llvm_context());
}

static LLVMTypeRef llvm_int1_type(void)
{
   return LLVMInt1TypeInContext(llvm_context());
}

static LLVMTypeRef llvm_int8_type(void)
{
   return LLVMInt8TypeInContext(llvm_context());
}

static LLVMTypeRef llvm_int16_type(void)
{
   return LLVMInt8TypeInContext(llvm_context());
}

static LLVMTypeRef llvm_int32_type(void)
{
   return LLVMInt32TypeInContext(llvm_context());
}

static LLVMTypeRef llvm_int64_type(void)
{
   return LLVMInt64TypeInContext(llvm_context());
}

static LLVMTypeRef llvm_double_type(void)
{
   return LLVMDoubleTypeInContext(llvm_context());
}

static LLVMValueRef llvm_int1(bool b)
{
   return LLVMConstInt(llvm_int1_type(), b, false);
}

static LLVMValueRef llvm_int8(int8_t i)
{
   return LLVMConstInt(llvm_int8_type(), i, false);
}

static LLVMValueRef llvm_int16(int16_t i)
{
   return LLVMConstInt(llvm_int16_type(), i, false);
}

static LLVMValueRef llvm_int32(int32_t i)
{
   return LLVMConstInt(llvm_int32_type(), i, false);
}

static LLVMValueRef llvm_int64(int64_t i)
{
   return LLVMConstInt(llvm_int64_type(), i, false);
}

#if 0
static LLVMValueRef llvm_real(double r)
{
   return LLVMConstReal(llvm_double_type(), r);
}
#endif

static LLVMTypeRef llvm_void_ptr(void)
{
   return LLVMPointerType(LLVMInt8TypeInContext(llvm_context()), 0);
}

static LLVMTypeRef llvm_char_ptr(void)
{
   return LLVMPointerType(llvm_int8_type(), 0);
}

static LLVMValueRef llvm_ensure_int_bits(LLVMValueRef value, int bits)
{
   const int value_bits = LLVMGetIntTypeWidth(LLVMTypeOf(value));
   if (value_bits < bits)
      return LLVMBuildZExt(builder, value, llvm_int32_type(), "");
   else if (value_bits > bits) {
      return LLVMBuildTrunc(builder, value, llvm_int32_type(), "");
   }
   else
      return value;
}

static LLVMValueRef llvm_zext_to_intptr(LLVMValueRef value)
{
   LLVMTypeRef type = LLVMIntTypeInContext(llvm_context(), sizeof(void *) * 8);
   return LLVMBuildZExt(builder, value, type, "");
}

static LLVMTypeRef llvm_rt_loc(void)
{
   LLVMTypeRef fields[] = {
      llvm_int32_type(),
      llvm_int32_type(),
      llvm_int16_type(),
      llvm_int16_type(),
      llvm_char_ptr()
   };
   return LLVMStructTypeInContext(llvm_context(), fields,
                                  ARRAY_LEN(fields), false);
}

static LLVMTypeRef llvm_ctor_type(void)
{
   LLVMTypeRef fntype = LLVMFunctionType(llvm_void_type(), NULL, 0, false);

   LLVMTypeRef field_types[] = {
      llvm_int32_type(),
      LLVMPointerType(fntype, 0),
      LLVMPointerType(llvm_int8_type(), 0)
   };
   return LLVMStructTypeInContext(llvm_context(), field_types,
                                  ARRAY_LEN(field_types), false);
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
      llvm_int32_type(),   // Signal ID
      llvm_int32_type(),   // Pad
      llvm_void_ptr(),     // Resolved pointer
      llvm_void_ptr()      // Last value pointer
   };

   LLVMTypeRef new =
      LLVMStructCreateNamed(llvm_context(), "sig_shared");
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
      llvm_int32_type()    // Offset
   };

   LLVMTypeRef new =
      LLVMStructCreateNamed(llvm_context(), "signal");
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
      llvm_int32_type(),    // Left
      llvm_int32_type(),    // Length
   };

   LLVMTypeRef dim_struct =
      LLVMStructTypeInContext(llvm_context(), dim_fields,
                              ARRAY_LEN(dim_fields), false);

   LLVMTypeRef fields[] = {
      is_signal ? base : LLVMPointerType(base, 0),
      LLVMArrayType(dim_struct, dims)
   };

   LLVMTypeRef new = LLVMStructCreateNamed(llvm_context(), struct_name);
   LLVMStructSetBody(new, fields, ARRAY_LEN(fields), false);
   return new;
}

static LLVMTypeRef llvm_closure_type(void)
{
   LLVMTypeRef struct_elems[] = {
      llvm_void_ptr(),     // Function pointer
      llvm_void_ptr(),     // Context pointer
      llvm_int32_type(),   // FFI spec
   };
   return LLVMStructTypeInContext(llvm_context(), struct_elems,
                                  ARRAY_LEN(struct_elems), false);
}

static LLVMTypeRef llvm_resolution_type(void)
{
   LLVMTypeRef struct_elems[] = {
      llvm_closure_type(),   // Closure
      llvm_int32_type(),     // Flags
      llvm_int32_type(),     // Left index
      llvm_int32_type(),     // Number of enumeration literals
   };
   return LLVMStructTypeInContext(llvm_context(), struct_elems,
                                  ARRAY_LEN(struct_elems), false);
}

static LLVMTypeRef llvm_debug_locus_type(void)
{
  LLVMTypeRef struct_elems[] = {
     LLVMPointerType(llvm_int8_type(), 0),   // Unit name
     llvm_int32_type(),                      // Offset
   };
   return LLVMStructTypeInContext(llvm_context(), struct_elems,
                                  ARRAY_LEN(struct_elems), false);
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

static LLVMBasicBlockRef llvm_append_block(LLVMValueRef fn, const char *name)
{
   return LLVMAppendBasicBlockInContext(llvm_context(), fn, name);
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
      ref = LLVMCreateStringAttribute(llvm_context(),
                                      "frame-pointer", 13, "all", 3);
   }
   else {
      const char *names[] = {
         "nounwind", "noreturn", "readonly", "nocapture", "byval", "uwtable",
         "noinline", "writeonly", "nonnull", "cold", "optnone",
      };
      assert(attr < ARRAY_LEN(names));

      const unsigned kind =
         LLVMGetEnumAttributeKindForName(names[attr], strlen(names[attr]));
      if (kind == 0)
         fatal_trace("Cannot get LLVM attribute for %s", names[attr]);

      ref = LLVMCreateEnumAttribute(llvm_context(), kind, 0);
   }

   LLVMAddAttributeAtIndex(fn, param, ref);
}

static LLVMTypeRef cgen_type(vcode_type_t type)
{
   switch (vtype_kind(type)) {
   case VCODE_TYPE_INT:
      {
         const int bits = bits_for_range(vtype_low(type), vtype_high(type));
         return LLVMIntTypeInContext(llvm_context(), bits);
      }

   case VCODE_TYPE_REAL:
      return LLVMDoubleTypeInContext(llvm_context());

   case VCODE_TYPE_CARRAY:
      return LLVMArrayType(cgen_type(vtype_elem(type)), vtype_size(type));

   case VCODE_TYPE_UARRAY:
      return llvm_uarray_type(cgen_type(vtype_elem(type)), vtype_dims(type));

   case VCODE_TYPE_OFFSET:
      return llvm_int32_type();

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
         LOCAL_TEXT_BUF tb = tb_new();
         ident_str(vtype_name(type), tb);
         const char *name = tb_get(tb);

         LLVMTypeRef lltype = LLVMGetTypeByName(module, name);
         if (lltype != NULL && !LLVMIsOpaqueStruct(lltype))
            return lltype;
         else if (lltype == NULL) {
            lltype = LLVMStructCreateNamed(llvm_context(), name);
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

   case VCODE_TYPE_CONTEXT:
      {
         LOCAL_TEXT_BUF tb = tb_new();
         ident_str(vtype_name(type), tb);
         tb_cat(tb, ".state");

         const char *type_name = tb_get(tb);

         LLVMTypeRef opaque = LLVMGetTypeByName(module, type_name);
         if (opaque == NULL)
            opaque = LLVMStructCreateNamed(llvm_context(), type_name);

         return LLVMPointerType(opaque, 0);
      }

   case VCODE_TYPE_SIGNAL:
      return llvm_signal_type();

   case VCODE_TYPE_FILE:
      return llvm_void_ptr();

   case VCODE_TYPE_OPAQUE:
      return llvm_void_type();

   case VCODE_TYPE_RESOLUTION:
      return llvm_resolution_type();

   case VCODE_TYPE_CLOSURE:
      return llvm_closure_type();

   default:
      fatal("cannot convert vcode type %d to LLVM", vtype_kind(type));
   }
}

static const char *cgen_reg_name(vcode_reg_t r)
{
   static __thread char buf[32];
   checked_sprintf(buf, sizeof(buf), "r%d", r);
   return buf;
}

static const char *cgen_memcpy_name(const char *kind, int width)
{
   static __thread char name[64];
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

static LLVMMetadataRef cgen_debug_file(const loc_t *loc, bool allow_cached)
{
   static __thread LLVMMetadataRef last = NULL;
   static __thread loc_file_ref_t last_ref = FILE_INVALID;

   if (loc->file_ref == last_ref && allow_cached)
      return last;

   const char *file_path = loc_file_str(loc);
   if (file_path == NULL)
      return last;

   char *basec LOCAL = xstrdup(file_path);
   char *dirc LOCAL = xstrdup(file_path);

   const char *file = basename(basec);
   const size_t file_len = strlen(file);

   const char *dir = dirname(dirc);
   const size_t dir_len = strlen(dir);

   last_ref = loc->file_ref;
   last = LLVMDIBuilderCreateFile(debuginfo, file, file_len, dir, dir_len);

   return last;
}

static void cgen_debug_loc(cgen_ctx_t *ctx, const loc_t *loc, bool force)
{
   static __thread loc_t last_loc = LOC_INVALID;

   if (loc_eq(loc, &last_loc) && !force)
      return;

   LLVMMetadataRef dloc = LLVMDIBuilderCreateDebugLocation(
      llvm_context(), loc->first_line, loc->first_column,
      cgen_top_debug_scope(), NULL);
#ifdef LLVM_HAVE_SET_CURRENT_DEBUG_LOCATION_2
   LLVMSetCurrentDebugLocation2(builder, dloc);
#else
   LLVMValueRef md = LLVMMetadataAsValue(llvm_context(), dloc);
   LLVMSetCurrentDebugLocation(builder, md);
#endif
}

static void cgen_debug_push_func(cgen_ctx_t *ctx)
{
   LLVMMetadataRef scope = cgen_top_debug_scope();

   LOCAL_TEXT_BUF symbol = safe_symbol(vcode_unit_name());

   const loc_t *loc = vcode_unit_loc();
   LLVMMetadataRef file_ref = cgen_debug_file(loc, true);
   LLVMMetadataRef dtype = LLVMDIBuilderCreateSubroutineType(
      debuginfo, file_ref, NULL, 0, 0);
   LLVMMetadataRef sp = LLVMDIBuilderCreateFunction(
      debuginfo, scope, tb_get(symbol), tb_len(symbol),
      tb_get(symbol), tb_len(symbol), file_ref,
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
   if (hops == 0) {
      assert(ctx->state != NULL);
      return ctx->state;
   }
   else {
      assert(ctx->display != NULL);
      LLVMValueRef display = ctx->display;
      for (int i = 0; i < hops - 1; i++) {
         LLVMValueRef ptr = LLVMBuildStructGEP(builder, display, 0, "");
         display = LLVMBuildLoad(builder, ptr, "");
      }
      return display;
   }
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
   unsigned base = 1;  // First field is always parent context
   if (cgen_is_procedure() || vcode_unit_kind() == VCODE_UNIT_PROCESS)
      base += 2;   // State machine and saved pcall state
   return base;
}

static unsigned cgen_var_offset(vcode_var_t var)
{
   return cgen_fixed_offset() + var;
}

static LLVMValueRef cgen_get_var(vcode_var_t var, cgen_ctx_t *ctx)
{
   LLVMValueRef value = NULL;
   if (ctx->state != NULL) {
      LOCAL_TEXT_BUF tb = tb_new();
      ident_str(vcode_var_name(var), tb);

      value = LLVMBuildStructGEP(builder, ctx->state,
                                 cgen_var_offset(var), tb_get(tb));
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
                                LLVMBuildAdd(builder, bytes, llvm_int32(7), ""),
                                "alloc_align_max"),
                   llvm_int32(~7),
                   "alloc_next");

   LLVMBuildStore(builder, alloc_next, _tmp_alloc_ptr);

   return LLVMBuildPointerCast(builder, buf,
                               LLVMPointerType(type, 0), "tmp_buf");
}

static LLVMValueRef cgen_uarray_dim(LLVMValueRef meta, int dim)
{
   assert(LLVMGetTypeKind(LLVMTypeOf(meta)) == LLVMStructTypeKind);
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
   LLVMValueRef ref = shash_get(string_pool, str);
   if (ref == NULL) {
      const size_t len = strlen(str);
      LLVMValueRef init =
         LLVMConstStringInContext(llvm_context(), str, len, false);
      ref = LLVMAddGlobal(module,
                          LLVMArrayType(llvm_int8_type(), len + 1),
                          "const_string");
      LLVMSetGlobalConstant(ref, true);
      LLVMSetInitializer(ref, init);
      LLVMSetLinkage(ref, LLVMPrivateLinkage);
      LLVMSetUnnamedAddr(ref, true);

      shash_put(string_pool, str, ref);
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

   LLVMValueRef glob = shash_get(string_pool, hint);
   if (glob == NULL) {
      const size_t len = strlen(hint);
      LLVMTypeRef type = LLVMArrayType(llvm_int8_type(), len + 1);

      LLVMValueRef init =
         LLVMConstStringInContext(llvm_context(), hint, len, false);

      glob = LLVMAddGlobal(module, type, "");
      LLVMSetGlobalConstant(glob, true);
      LLVMSetLinkage(glob, LLVMPrivateLinkage);
      LLVMSetInitializer(glob, init);
      LLVMSetUnnamedAddr(glob, true);

      shash_put(string_pool, hint, glob);
   }

   return cgen_array_pointer(glob);
}

static LLVMValueRef cgen_signature(ident_t name, vcode_type_t result,
                                   vcode_cc_t cc, const vcode_type_t *vparams,
                                   size_t nparams)
{
   LOCAL_TEXT_BUF safe_name = safe_symbol(name);
   LLVMValueRef fn = LLVMGetNamedFunction(module, tb_get(safe_name));
   if (fn != NULL)
      return fn;
   else if (vparams == NULL)
      return NULL;

   const bool has_state_arg =
      result == VCODE_INVALID_TYPE && cc != VCODE_CC_FOREIGN;
   const bool foreign_uarray_result =
      cc == VCODE_CC_FOREIGN && result != VCODE_INVALID_REG
      && vtype_kind(result) == VCODE_TYPE_UARRAY;

   const int nextra = has_state_arg + foreign_uarray_result;
   LLVMTypeRef params[nparams + nextra];
   LLVMTypeRef *p = params;

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
      type = LLVMFunctionType(llvm_void_type(), params, nparams + nextra, false);
   else
      type = LLVMFunctionType(cgen_type(result), params,
                              nparams + nextra, false);

   fn = LLVMAddFunction(module, tb_get(safe_name), type);

   cgen_add_func_attr(fn, FUNC_ATTR_DLLEXPORT, -1);
   cgen_add_func_attr(fn, FUNC_ATTR_NOUNWIND, -1);

   if (foreign_uarray_result) {
      cgen_add_func_attr(fn, FUNC_ATTR_NOCAPTURE, nparams + nextra);
      cgen_add_func_attr(fn, FUNC_ATTR_WRITEONLY, nparams + nextra);
   }

   return fn;
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
   case VCODE_UNIT_PROTECTED:
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

static void cgen_op_fcall(int op, cgen_ctx_t *ctx)
{
   vcode_reg_t result = vcode_get_result(op);

   vcode_type_t rtype = VCODE_INVALID_TYPE;
   if (result != VCODE_INVALID_REG)
      rtype = vcode_reg_type(result);

   const vcode_cc_t cc = vcode_get_subkind(op);
   ident_t func = vcode_get_func(op);
   const bool has_state_arg =
      result == VCODE_INVALID_REG && cc != VCODE_CC_FOREIGN;
   const bool foreign_uarray_result =
      cc == VCODE_CC_FOREIGN && result != VCODE_INVALID_REG
      && vtype_kind(rtype) == VCODE_TYPE_UARRAY;
   const int nargs = vcode_count_args(op);
   const int total_args = nargs + has_state_arg + foreign_uarray_result;

   LLVMValueRef fn =
      cgen_signature(func, VCODE_INVALID_TYPE, cc, NULL, 0);
   if (fn == NULL) {
      vcode_type_t atypes[nargs];
      for (int i = 0; i < nargs; i++)
         atypes[i] = vcode_reg_type(vcode_get_arg(op, i));

      fn = cgen_signature(func, rtype, cc, atypes, nargs);
   }

   LLVMValueRef args[total_args];
   LLVMValueRef *pa = args;
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

   ctx->regs[result] = LLVMConstArray(cgen_type(vtype_elem(type)), tmp, length);
}

static void cgen_append_ctor(LLVMValueRef fn)
{
   LLVMValueRef entry = LLVMGetUndef(llvm_ctor_type());
   entry = LLVMBuildInsertValue(builder, entry, llvm_int32(65535), 0, "");
   entry = LLVMBuildInsertValue(builder, entry, fn, 1, "");
   entry = LLVMBuildInsertValue(builder, entry,
                                LLVMConstNull(llvm_void_ptr()), 2, "");

   APUSH(ctors, entry);
}

static void cgen_op_const_rep(int op, cgen_ctx_t *ctx)
{
   vcode_reg_t result = vcode_get_result(op);
   vcode_type_t elem_type = vtype_pointed(vcode_reg_type(result));

   const int length = vcode_get_value(op);
   vcode_reg_t arg0 = vcode_get_arg(op, 0);

   LOCAL_TEXT_BUF name = tb_new();
   ident_str(vcode_unit_name(), name);
   tb_printf(name, "_rep_r%d", result);

   LLVMTypeRef lltype = LLVMArrayType(cgen_type(elem_type), length);
   LLVMValueRef global = LLVMAddGlobal(module, lltype, tb_get(name));
   LLVMSetLinkage(global, LLVMPrivateLinkage);

   int64_t init;
   if (vcode_reg_const(arg0, &init) && init == 0) {
      // Make sure this is not a global constant: we want it to go in
      // the .bss section not .rodata
      LLVMSetGlobalConstant(global, false);
      LLVMSetUnnamedAddr(global, true);
      LLVMSetInitializer(global, LLVMConstNull(lltype));
   }
   else if (length <= CONST_REP_ARRAY_LIMIT) {
      LLVMValueRef *tmp LOCAL = xmalloc_array(length, sizeof(LLVMValueRef));
      for (int i = 0; i < length; i++)
         tmp[i] = ctx->regs[arg0];

      LLVMValueRef array = LLVMConstArray(cgen_type(elem_type), tmp, length);
      LLVMSetInitializer(global, array);
      LLVMSetGlobalConstant(global, true);
      LLVMSetUnnamedAddr(global, true);
   }
   else {
      LLVMSetInitializer(global, LLVMGetUndef(lltype));

      LOCAL_TEXT_BUF fname = tb_new();
      ident_str(vcode_unit_name(), fname);
      tb_printf(name, "_init_rep_r%d", result);

      LLVMTypeRef fntype = LLVMFunctionType(llvm_void_type(), NULL, 0, false);
      LLVMValueRef initfn = LLVMAddFunction(module, tb_get(fname), fntype);

      LLVMBasicBlockRef orig_bb = LLVMGetInsertBlock(builder);
      LLVMBasicBlockRef entry_bb = llvm_append_block(initfn, "");
      LLVMBasicBlockRef test_bb = llvm_append_block(initfn, "test");
      LLVMBasicBlockRef body_bb = llvm_append_block(initfn, "body");
      LLVMBasicBlockRef exit_bb = llvm_append_block(initfn, "exit");

      LLVMPositionBuilderAtEnd(builder, entry_bb);
      LLVMBuildBr(builder, test_bb);

      LLVMPositionBuilderAtEnd(builder, test_bb);
      LLVMValueRef i_phi = LLVMBuildPhi(builder, llvm_int32_type(), "i");

      LLVMValueRef cmp =
          LLVMBuildICmp(builder, LLVMIntSLT, i_phi, llvm_int32(length), "");
      LLVMBuildCondBr(builder, cmp, body_bb, exit_bb);

      LLVMPositionBuilderAtEnd(builder, body_bb);

      LLVMValueRef indexes[] = {llvm_int32(0), llvm_zext_to_intptr(i_phi)};
      LLVMValueRef ptr = LLVMBuildGEP(builder, global, indexes, 2, "");
      LLVMBuildStore(builder, ctx->regs[arg0], ptr);

      LLVMValueRef i_inc = LLVMBuildAdd(builder, i_phi, llvm_int32(1), "");

      LLVMValueRef i_phi_in[] = {llvm_int32(0), i_inc};
      LLVMBasicBlockRef i_phi_bbs[] = {entry_bb, body_bb};
      LLVMAddIncoming(i_phi, i_phi_in, i_phi_bbs, 2);

      LLVMBuildBr(builder, test_bb);

      LLVMPositionBuilderAtEnd(builder, exit_bb);
      LLVMBuildRetVoid(builder);

      LLVMPositionBuilderAtEnd(builder, orig_bb);

      cgen_append_ctor(initfn);
   }

   ctx->regs[result] = cgen_array_pointer(global);
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

   LLVMBasicBlockRef thenbb = llvm_append_block(ctx->fn, "assert_fail");
   LLVMBasicBlockRef elsebb = llvm_append_block(ctx->fn, "assert_pass");

   LLVMBuildCondBr(builder, test, elsebb, thenbb);

   LLVMPositionBuilderAtEnd(builder, thenbb);

   vcode_reg_t mreg = vcode_get_arg(op, 2);

   LLVMValueRef message, length;
   if (mreg == VCODE_INVALID_REG) {
      const char def_str[] = "Assertion violation.";
      const size_t def_len = sizeof(def_str) - 1;

      LLVMValueRef global = LLVMGetNamedGlobal(module, "default_message");
      if (global == NULL) {
         LLVMValueRef init =
            LLVMConstStringInContext(llvm_context(), def_str, def_len, true);

         global = LLVMAddGlobal(module, LLVMTypeOf(init), "default_message");
         LLVMSetInitializer(global, init);
         LLVMSetLinkage(global, LLVMPrivateLinkage);
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
         LLVMBuildPtrToInt(builder, gep, llvm_int32_type(), "");
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
   else if (LLVMGetTypeKind(LLVMTypeOf(rhs)) == LLVMPointerTypeKind) {
      LLVMValueRef diff = LLVMBuildPtrDiff(builder, lhs, rhs, "");
      ctx->regs[result] = LLVMBuildTrunc(builder, diff, llvm_int32_type(),
                                         cgen_reg_name(result));
   }
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
      LLVMBasicBlockRef zero_bb = llvm_append_block(ctx->fn, "div_zero");
      LLVMBasicBlockRef ok_bb   = llvm_append_block(ctx->fn, "div_ok");

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
      LLVMBuildUIToFP(builder, cgen_get_arg(op, 0, ctx), llvm_double_type(), ""),
      LLVMBuildUIToFP(builder, cgen_get_arg(op, 1, ctx), llvm_double_type(), "")
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
      ? LLVMConstReal(llvm_double_type(), 0.0)
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
      value = LLVMBuildZExt(builder, value_raw, llvm_int32_type(), "");
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

   LLVMBasicBlockRef pass_bb  = llvm_append_block(ctx->fn, "bounds_pass");
   LLVMBasicBlockRef fail_bb  = llvm_append_block(ctx->fn, "bounds_fail");

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
      LLVMTypeRef ll_int32 = llvm_int32_type();
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

   LLVMBasicBlockRef pass_bb  = llvm_append_block(ctx->fn, "bounds_pass");
   LLVMBasicBlockRef fail_bb  = llvm_append_block(ctx->fn, "bounds_fail");

   LLVMBuildCondBr(builder, in, pass_bb, fail_bb);

   LLVMPositionBuilderAtEnd(builder, fail_bb);

   if (value_bits > 32) {
      // TODO: we should probably pass all the arguments as 64-bit here
      LLVMTypeRef ll_int32 = llvm_int32_type();
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
      LLVMValueRef bytes = LLVMBuildMul(builder, llvm_sizeof(type),
                                        cgen_get_arg(op, 0, ctx), "");
      ctx->regs[result] = cgen_tmp_alloc(bytes, type);
   }
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

static void cgen_op_context_upref(int op, cgen_ctx_t *ctx)
{
   const int hops = vcode_get_hops(op);
   vcode_reg_t result = vcode_get_result(op);

   ctx->regs[result] = cgen_display_upref(hops, ctx);
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
       || kind == VCODE_TYPE_RESOLUTION || kind == VCODE_TYPE_CLOSURE) {
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
      scalar = LLVMBuildZExt(builder, value, llvm_int64_type(), "");
   else if (kind == VCODE_TYPE_REAL)
      scalar = LLVMBuildBitCast(builder, value, llvm_int64_type(), "");
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

static void cgen_op_disconnect(int op, cgen_ctx_t *ctx)
{
   LLVMValueRef sigptr = cgen_get_arg(op, 0, ctx);
   LLVMValueRef sid    = LLVMBuildExtractValue(builder, sigptr, 0, "sid");
   LLVMValueRef offset = LLVMBuildExtractValue(builder, sigptr, 1, "offset");

   LLVMValueRef args[] = {
      sid,
      offset,
      cgen_get_arg(op, 1, ctx),
      cgen_get_arg(op, 3, ctx),
      cgen_get_arg(op, 2, ctx)
   };
   LLVMBuildCall(builder, llvm_fn("_disconnect"), args, ARRAY_LEN(args), "");
}

static void cgen_op_resolution_wrapper(int op, cgen_ctx_t *ctx)
{
   // Resolution functions are in LRM 93 section 2.4

   vcode_reg_t result = vcode_get_result(op);
   vcode_type_t type = vtype_base(vcode_reg_type(result));

   uint32_t flags = 0;
   if (vtype_kind(type) == VCODE_TYPE_POINTER)
      flags |= R_COMPOSITE;

   LLVMValueRef closure = cgen_get_arg(op, 0, ctx);
   LLVMValueRef ileft   = cgen_get_arg(op, 1, ctx);
   LLVMValueRef nlits   = cgen_get_arg(op, 2, ctx);

   LLVMValueRef rdata = LLVMGetUndef(llvm_resolution_type());
   rdata = LLVMBuildInsertValue(builder, rdata, closure, 0, "");
   rdata = LLVMBuildInsertValue(builder, rdata, llvm_int32(flags), 1, "");
   rdata = LLVMBuildInsertValue(builder, rdata, ileft, 2, "");
   rdata = LLVMBuildInsertValue(builder, rdata, nlits, 3, "");

   ctx->regs[result] = rdata;
}

static ffi_type_t cgen_ffi_type(vcode_type_t type)
{
   if (type == VCODE_INVALID_TYPE)
      return FFI_VOID;

   switch (vtype_kind(type)) {
   case VCODE_TYPE_INT:
      switch (bits_for_range(vtype_low(type), vtype_high(type))) {
      case 1: case 8: return FFI_INT8;
      case 16: return FFI_INT16;
      case 32: return FFI_INT32;
      default: return FFI_INT64;
      }
   case VCODE_TYPE_OFFSET:
      return FFI_INT32;
   case VCODE_TYPE_REAL:
      return FFI_FLOAT;
   case VCODE_TYPE_CARRAY:
   case VCODE_TYPE_RECORD:
   case VCODE_TYPE_POINTER:
      return FFI_POINTER;
   case VCODE_TYPE_UARRAY:
      return FFI_UARRAY;
   default:
      fatal_trace("cannot handle type %d in cgen_ffi_type", vtype_kind(type));
   }
}

static void cgen_op_closure(int op, cgen_ctx_t *ctx)
{
   ident_t func = vcode_get_func(op);
   vcode_reg_t result = vcode_get_result(op);

   vcode_type_t rtype = vtype_base(vcode_reg_type(result));
   vcode_type_t atype = vcode_get_type(op);

   LLVMValueRef fn = cgen_signature(func, VCODE_INVALID_TYPE, VCODE_CC_VHDL,
                                    NULL, 0);
   if (fn == NULL) {
      // The function is not visible yet e.g. because it is declared in
      // another package
      vcode_type_t args[] = {
         vcode_reg_type(vcode_get_arg(op, 0)),   // Context type
         atype
      };
      fn = cgen_signature(func, rtype, VCODE_CC_VHDL, args, 2);
   }

   ffi_spec_t spec = {
      .atype = cgen_ffi_type(atype),
      .rtype = cgen_ffi_type(rtype)
   };

   LLVMValueRef display = cgen_get_arg(op, 0, ctx);

   LLVMValueRef cdata = LLVMGetUndef(llvm_closure_type());
   cdata = LLVMBuildInsertValue(builder, cdata, llvm_void_cast(fn), 0, "");
   cdata = LLVMBuildInsertValue(builder, cdata, llvm_void_cast(display), 1, "");
   cdata = LLVMBuildInsertValue(builder, cdata, llvm_int32(spec.bits), 2, "");

   ctx->regs[result] = cdata;
}

static void cgen_op_protected_init(int op, cgen_ctx_t *ctx)
{
   ident_t func = vcode_get_func(op);
   vcode_reg_t result = vcode_get_result(op);

   char *resetfn LOCAL = xasprintf("%s_reset", istr(func));
   LOCAL_TEXT_BUF symbol = safe_symbol_str(resetfn);

   LLVMValueRef fn = LLVMGetNamedFunction(module, tb_get(symbol));
   if (fn == NULL) {
      LLVMTypeRef atypes[] = {
         cgen_type(vcode_reg_type(vcode_get_arg(op, 0)))   // Context type
      };
      LLVMTypeRef fntype = LLVMFunctionType(llvm_void_ptr(), atypes, 1, false);
      fn = LLVMAddFunction(module, tb_get(symbol), fntype);
   }

   LLVMValueRef args[] = { cgen_get_arg(op, 0, ctx) };
   LLVMValueRef ptr = LLVMBuildCall(builder, fn, args, ARRAY_LEN(args),
                                    cgen_reg_name(result));

   LLVMTypeRef rtype = cgen_type(vcode_reg_type(result));
   ctx->regs[result] = LLVMBuildPointerCast(builder, ptr, rtype, "");
}

static void cgen_op_protected_free(int op, cgen_ctx_t *ctx)
{
   LLVMValueRef obj = cgen_get_arg(op, 0, ctx);
   LLVMBuildFree(builder, obj);
}

static void cgen_net_flag(int op, const char *func, cgen_ctx_t *ctx)
{
   vcode_reg_t result = vcode_get_result(op);

   LLVMValueRef sigptr = cgen_get_arg(op, 0, ctx);
   LLVMValueRef count  = cgen_get_arg(op, 1, ctx);

   LLVMValueRef args[] = {
      LLVMBuildExtractValue(builder, sigptr, 0, "shared"),
      LLVMBuildExtractValue(builder, sigptr, 1, "offset"),
      count
   };
   ctx->regs[result] = LLVMBuildCall(builder, llvm_fn(func), args,
                                     ARRAY_LEN(args), cgen_reg_name(result));
}

static void cgen_op_event(int op, cgen_ctx_t *ctx)
{
   cgen_net_flag(op, "_test_net_event", ctx);
}

static void cgen_op_active(int op, cgen_ctx_t *ctx)
{
   cgen_net_flag(op, "_test_net_active", ctx);
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

   LOCAL_TEXT_BUF tb = tb_new();
   ident_str(vcode_unit_name(), tb);
   tb_printf(tb, "_const_r%d", result);

   LLVMValueRef init = cgen_get_arg(op, 0, ctx);
   LLVMTypeRef type = LLVMTypeOf(init);

   LLVMValueRef global = LLVMAddGlobal(module, type, tb_get(tb));
   LLVMSetLinkage(global, LLVMPrivateLinkage);
   LLVMSetGlobalConstant(global, true);
   LLVMSetUnnamedAddr(global, true);
   LLVMSetInitializer(global, init);

   if (LLVMGetTypeKind(type) == LLVMArrayTypeKind)
      ctx->regs[result] = cgen_array_pointer(global);
   else
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
         LLVMBuildPtrToInt(builder, field, llvm_int32_type(), "");
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
   };
   LLVMBuildCall(builder, llvm_fn("_sched_event"), args, ARRAY_LEN(args), "");
}

static void cgen_pcall_suspend(LLVMValueRef state, LLVMBasicBlockRef cont_bb,
                               cgen_ctx_t *ctx)
{
   LLVMBasicBlockRef suspend_bb = llvm_append_block(ctx->fn, "suspend");

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

static void cgen_op_pcall(int op, cgen_ctx_t *ctx)
{
   ident_t func = vcode_get_func(op);
   const int nargs = vcode_count_args(op);
   const vcode_cc_t cc = vcode_get_subkind(op);
   const int total_args = nargs + 1;

   LLVMValueRef fn = cgen_signature(func, VCODE_INVALID_TYPE, cc, NULL, 0);
   if (fn == NULL) {
      vcode_type_t atypes[nargs];
      for (int i = 0; i < nargs; i++)
         atypes[i] = vcode_reg_type(vcode_get_arg(op, i));

      fn = cgen_signature(func, VCODE_INVALID_TYPE, cc, atypes, nargs);
   }

   LLVMValueRef args[total_args];
   LLVMValueRef *ap = args;
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

static void cgen_op_resume(int op, cgen_ctx_t *ctx)
{
   LLVMBasicBlockRef after_bb = llvm_append_block(ctx->fn, "resume_after");
   LLVMBasicBlockRef call_bb  = llvm_append_block(ctx->fn, "resume_call");

   assert(ctx->state);
   LLVMValueRef pcall_ptr = LLVMBuildStructGEP(builder, ctx->state, 2, "");
   LLVMValueRef pcall_state = LLVMBuildLoad(builder, pcall_ptr, "");

   LLVMValueRef is_null = LLVMBuildIsNull(builder, pcall_state, "");
   LLVMBuildCondBr(builder, is_null, after_bb, call_bb);

   LLVMPositionBuilderAtEnd(builder, call_bb);

   LOCAL_TEXT_BUF symbol = safe_symbol(vcode_get_func(op));
   LLVMValueRef fn = LLVMGetNamedFunction(module, tb_get(symbol));
   assert(fn != NULL);

   LLVMTypeRef fn_type = LLVMGetElementType(LLVMTypeOf(fn));

   const int nparams = LLVMCountParamTypes(fn_type);
   assert(nparams >= 2);

   LLVMTypeRef param_types[nparams];
   LLVMGetParamTypes(fn_type, param_types);

   LLVMValueRef args[nparams];
   args[0] = pcall_state;
   for (int i = 1; i < nparams; i++)
      args[i] = LLVMGetUndef(param_types[i]);

   LLVMValueRef new_state = LLVMBuildCall(builder, fn, args, nparams, "");

   LLVMBuildStore(builder, new_state, pcall_ptr);

   cgen_pcall_suspend(new_state, after_bb, ctx);
}

static void cgen_op_memset(int op, cgen_ctx_t *ctx)
{
   LLVMValueRef ptr    = cgen_get_arg(op, 0, ctx);
   LLVMValueRef value  = cgen_get_arg(op, 1, ctx);
   LLVMValueRef length = cgen_get_arg(op, 2, ctx);

   LLVMValueRef args[] = {
      llvm_void_cast(ptr),
      LLVMBuildZExt(builder, value, llvm_int8_type(), ""),
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
   LLVMValueRef length = cgen_get_arg(op, 1, ctx);

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
   LLVMValueRef size = LLVMBuildIntCast(builder,
                                        LLVMSizeOf(value_type),
                                        llvm_int32_type(), "");

   LLVMValueRef count;
   if (vcode_count_args(op) >= 3)
      count = cgen_get_arg(op, 2, ctx);
   else
      count = llvm_int32(1);

   LLVMValueRef outlen;
   if (vcode_count_args(op) >= 4)
      outlen = cgen_get_arg(op, 3, ctx);
   else
      outlen = LLVMConstNull(LLVMPointerType(llvm_int32_type(), 0));

   LLVMValueRef args[] = { file, llvm_void_cast(ptr), size, count, outlen };
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

   LLVMBasicBlockRef null_bb = llvm_append_block(ctx->fn, "all_null");
   LLVMBasicBlockRef ok_bb   = llvm_append_block(ctx->fn, "all_ok");

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

static void cgen_op_array_size(int op, cgen_ctx_t *ctx)
{
   LLVMValueRef llen = cgen_get_arg(op, 0, ctx);
   LLVMValueRef rlen = cgen_get_arg(op, 1, ctx);

   LLVMValueRef ok = LLVMBuildICmp(builder, LLVMIntEQ, llen, rlen, "ok");

   LLVMBasicBlockRef pass_bb  = llvm_append_block(ctx->fn, "bounds_pass");
   LLVMBasicBlockRef fail_bb  = llvm_append_block(ctx->fn, "bounds_fail");

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
   LLVMTypeRef int32 = llvm_int32_type();

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

      LLVMBasicBlockRef pass_bb  = llvm_append_block(ctx->fn, "bounds_pass");
      LLVMBasicBlockRef fail_bb  = llvm_append_block(ctx->fn, "bounds_fail");

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

static void cgen_op_index_check2(int op, cgen_ctx_t *ctx)
{
   LLVMValueRef value = llvm_ensure_int_bits(cgen_get_arg(op, 0, ctx), 32);
   LLVMValueRef left  = llvm_ensure_int_bits(cgen_get_arg(op, 1, ctx), 32);
   LLVMValueRef right = llvm_ensure_int_bits(cgen_get_arg(op, 2, ctx), 32);
   LLVMValueRef dir   = cgen_get_arg(op, 3, ctx);
   LLVMValueRef locus = cgen_get_arg(op, 4, ctx);

   LLVMValueRef low  = LLVMBuildSelect(builder, dir, right, left, "low");
   LLVMValueRef high = LLVMBuildSelect(builder, dir, left, right, "high");

   LLVMValueRef null = LLVMBuildICmp(builder, LLVMIntSLT, high, low, "null");

   LLVMValueRef above =
      LLVMBuildICmp(builder, LLVMIntSGT, value, high, "above");
   LLVMValueRef below =
      LLVMBuildICmp(builder, LLVMIntSLT, value, low, "below");

   LLVMValueRef fail =
      LLVMBuildAnd(builder,
                   LLVMBuildNot(builder, null, ""),
                   LLVMBuildOr(builder, above, below, ""),
                   "fail");

   LLVMBasicBlockRef fail_bb = llvm_append_block(ctx->fn, "fail");
   LLVMBasicBlockRef pass_bb = llvm_append_block(ctx->fn, "pass");

   LLVMBuildCondBr(builder, fail, fail_bb, pass_bb);

   LLVMPositionBuilderAtEnd(builder, fail_bb);

   LLVMValueRef args[] = {
      value, left, right, dir, locus,
   };
   LLVMBuildCall(builder, llvm_fn("__nvc_index_fail"), args,
                 ARRAY_LEN(args), "");

   LLVMBuildUnreachable(builder);

   LLVMPositionBuilderAtEnd(builder, pass_bb);
}

static void cgen_op_debug_locus(int op, cgen_ctx_t *ctx)
{
   vcode_reg_t result = vcode_get_result(op);

   LLVMValueRef unit = cgen_const_string(istr(vcode_get_ident(op)));
   LLVMValueRef offset = llvm_int32(vcode_get_tag(op));

   LLVMValueRef r = LLVMGetUndef(llvm_debug_locus_type());
   r = LLVMBuildInsertValue(builder, r, unit, 0, "");
   r = LLVMBuildInsertValue(builder, r, offset, 1,
                            cgen_reg_name(result));

   ctx->regs[result] = r;
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
         LLVMBuildZExt(builder, arg0, llvm_int32_type(), ""),
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

static void cgen_op_implicit_signal(int op, cgen_ctx_t *ctx)
{
   LLVMValueRef sigptr = cgen_get_arg(op, 0, ctx);

   LLVMTypeRef alloca_type = NULL;
   LLVMValueRef closure = cgen_pointer_to_arg_data(op, 3, &alloca_type, ctx);

   LLVMValueRef args[] = {
      LLVMBuildExtractValue(builder, sigptr, 0, "shared"),
      cgen_get_arg(op, 2, ctx),
      closure,
   };
   LLVMBuildCall(builder, llvm_fn("_implicit_signal"),
                 args, ARRAY_LEN(args), "");

   llvm_lifetime_end(closure, alloca_type);
}

static void cgen_op_link_signal(int op, cgen_ctx_t *ctx)
{
   vcode_reg_t result = vcode_get_result(op);

   LOCAL_TEXT_BUF tb = tb_new();
   ident_str(vcode_get_ident(op), tb);

   LLVMValueRef args[] = {
      cgen_const_string(tb_get(tb))
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

   LOCAL_TEXT_BUF unit_name = tb_new();
   ident_str(ident_runtil(var_name, '.'), unit_name);

   LLVMValueRef global = LLVMGetNamedGlobal(module, tb_get(unit_name));
   if (global == NULL) {
      LOCAL_TEXT_BUF type_name = tb_new();
      tb_cat(type_name, tb_get(unit_name));
      tb_cat(type_name, ".state");

      LLVMTypeRef opaque = LLVMGetTypeByName(module, tb_get(type_name));
      if (opaque == NULL)
         opaque = LLVMStructCreateNamed(llvm_context(), tb_get(type_name));

      global = LLVMAddGlobal(module, LLVMPointerType(opaque, 0),
                             tb_get(unit_name));
   }

   LOCAL_TEXT_BUF symbol = safe_symbol(var_name);
   char *offset_name LOCAL = xasprintf("__offset_%s", tb_get(symbol));
   LLVMValueRef offset = LLVMGetNamedGlobal(module, offset_name);
   if (offset == NULL) {
      offset = LLVMAddGlobal(module, llvm_int32_type(), offset_name);
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

static void cgen_op_link_package(int op, cgen_ctx_t *ctx)
{
   vcode_reg_t result = vcode_get_result(op);

   LOCAL_TEXT_BUF unit_name = tb_new();
   ident_str(vcode_get_ident(op), unit_name);

   LLVMValueRef global = LLVMGetNamedGlobal(module, tb_get(unit_name));
   if (global == NULL) {
      LOCAL_TEXT_BUF type_name = tb_new();
      tb_cat(type_name, tb_get(unit_name));
      tb_cat(type_name, ".state");

      LLVMTypeRef opaque = LLVMGetTypeByName(module, tb_get(type_name));
      if (opaque == NULL)
         opaque = LLVMStructCreateNamed(llvm_context(), tb_get(type_name));

      global = LLVMAddGlobal(module, LLVMPointerType(opaque, 0),
                             tb_get(unit_name));
   }

   ctx->regs[result] = LLVMBuildLoad(builder, global, cgen_reg_name(result));
}

static void cgen_op_map_signal(int op, cgen_ctx_t *ctx)
{
   if (vcode_count_args(op) < 5)
      return;

   LLVMTypeRef alloca_type = NULL;
   LLVMValueRef closure = cgen_pointer_to_arg_data(op, 4, &alloca_type, ctx);

   LLVMValueRef sigptr = cgen_get_arg(op, 1, ctx);

   LLVMValueRef args[] = {
      LLVMBuildExtractValue(builder, sigptr, 0, "shared"),
      LLVMBuildExtractValue(builder, sigptr, 1, "offset"),
      cgen_get_arg(op, 3, ctx),
      closure
   };
   LLVMBuildCall(builder, llvm_fn("_convert_signal"),
                 args, ARRAY_LEN(args), "");

   llvm_lifetime_end(closure, alloca_type);
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
   case VCODE_OP_DRIVE_SIGNAL:
   case VCODE_OP_SCHED_STATIC:
      break;
   case VCODE_OP_MAP_SIGNAL:
      cgen_op_map_signal(i, ctx);
      break;
   case VCODE_OP_CONST_ARRAY:
      cgen_op_const_array(i, ctx);
      break;
   case VCODE_OP_CONST_REP:
      cgen_op_const_rep(i, ctx);
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
   case VCODE_OP_DISCONNECT:
      cgen_op_disconnect(i, ctx);
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
      cgen_op_pcall(i, ctx);
      break;
   case VCODE_OP_RESUME:
      cgen_op_resume(i, ctx);
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
   case VCODE_OP_DYNAMIC_BOUNDS:
      cgen_op_dynamic_bounds(i, ctx);
      break;
   case VCODE_OP_ARRAY_SIZE:
      cgen_op_array_size(i, ctx);
      break;
   case VCODE_OP_INDEX_CHECK:
      cgen_op_index_check(i, ctx);
      break;
   case VCODE_OP_INDEX_CHECK2:
      cgen_op_index_check2(i, ctx);
      break;
   case VCODE_OP_DEBUG_LOCUS:
      cgen_op_debug_locus(i, ctx);
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
   case VCODE_OP_RANGE_NULL:
      cgen_op_range_null(i, ctx);
      break;
   case VCODE_OP_INIT_SIGNAL:
      cgen_op_init_signal(i, ctx);
      break;
   case VCODE_OP_IMPLICIT_SIGNAL:
      cgen_op_implicit_signal(i, ctx);
      break;
   case VCODE_OP_LINK_SIGNAL:
      cgen_op_link_signal(i, ctx);
      break;
   case VCODE_OP_LINK_VAR:
      cgen_op_link_var(i, ctx);
      break;
   case VCODE_OP_LINK_PACKAGE:
      cgen_op_link_package(i, ctx);
      break;
   case VCODE_OP_RESOLUTION_WRAPPER:
      cgen_op_resolution_wrapper(i, ctx);
      break;
   case VCODE_OP_CLOSURE:
      cgen_op_closure(i, ctx);
      break;
   case VCODE_OP_PROTECTED_INIT:
      cgen_op_protected_init(i, ctx);
      break;
   case VCODE_OP_PROTECTED_FREE:
      cgen_op_protected_free(i, ctx);
      break;
   case VCODE_OP_CONTEXT_UPREF:
      cgen_op_context_upref(i, ctx);
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
      ctx->blocks[i] = llvm_append_block(ctx->fn, name);
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
   const int p0 = cgen_is_procedure() ? 1 : 0;
   const int nparams = vcode_count_params();
   for (int i = 0; i < nparams; i++)
      ctx->regs[vcode_param_reg(i)] = LLVMGetParam(ctx->fn, p0 + i);
}

static void cgen_locals(cgen_ctx_t *ctx)
{
   LLVMPositionBuilderAtEnd(builder, ctx->blocks[0]);

   const int nvars = vcode_count_vars();
   vcode_unit_t unit = vcode_active_unit();
   if (vcode_unit_child(unit) != NULL) {
      bool on_stack = true;
      for (int i = 0; i < nvars; i++) {
         if (vcode_var_flags(i) & VAR_HEAP)
            on_stack = false;
      }

      LLVMTypeRef state_type = cgen_state_type(unit);
      if (on_stack)
         ctx->state = LLVMBuildAlloca(builder, state_type, "state");
      else
         ctx->state = cgen_tmp_alloc(llvm_sizeof(state_type), state_type);

      LLVMValueRef context_ptr = LLVMBuildStructGEP(builder, ctx->state, 0, "");
      LLVMBuildStore(builder, ctx->display, context_ptr);
   }
   else {
      for (int i = 0; i < nvars; i++) {
         LLVMTypeRef lltype = cgen_type(vcode_var_type(i));

         LLVMValueRef mem;
         if (vcode_var_flags(i) & VAR_HEAP)
            mem = cgen_tmp_alloc(llvm_sizeof(lltype), lltype);
         else {
            LOCAL_TEXT_BUF name = tb_new();
            ident_str(vcode_var_name(i), name);
            mem = LLVMBuildAlloca(builder, lltype, tb_get(name));
         }

         ctx->locals[i] = mem;
      }
   }
}

static void cgen_function(void)
{
   assert(vcode_unit_kind() == VCODE_UNIT_FUNCTION);

   const int nparams = vcode_count_params();
   vcode_type_t params[nparams];
   for (int i = 0; i < nparams; i++)
      params[i] = vcode_param_type(i);

   LLVMValueRef fn = cgen_signature(vcode_unit_name(), vcode_unit_result(),
                                    VCODE_CC_VHDL, params, nparams);
   cgen_add_func_attr(fn, FUNC_ATTR_UWTABLE, -1);

   // Make sure the display type is not an opaque struct
   cgen_state_type(vcode_active_unit());

   // Do not add FUNC_ATTR_READONLY here: it can result in unexpected
   // optimisations, such as removing assertions.

   const int display_arg = vcode_unit_result() == VCODE_INVALID_TYPE ? 1 : 0;

   cgen_ctx_t ctx = {
      .fn = fn,
      .display = LLVMGetParam(fn, display_arg)
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

   LOCAL_TEXT_BUF tb = tb_new();
   ident_str(vcode_unit_name(), tb);
   tb_cat(tb, ".state");

   const char *name = tb_get(tb);

   LLVMTypeRef opaque = LLVMGetTypeByName(module, name);
   if (opaque != NULL && !LLVMIsOpaqueStruct(opaque)) {
      vcode_state_restore(&state);
      return opaque;
   }
   else if (opaque == NULL)
      opaque = LLVMStructCreateNamed(llvm_context(), name);

   const vunit_kind_t kind = vcode_unit_kind();
   const bool has_fsm = (kind == VCODE_UNIT_PROCESS || cgen_is_procedure());

   const int nvars   = vcode_count_vars();
   const int nfields = nvars + (has_fsm ? 3 : 1);

   int next_field = 0;
   LLVMTypeRef fields[nfields];

   vcode_unit_t context = vcode_unit_context();
   assert(context != unit);
   if (context != NULL)
      fields[next_field++] = LLVMPointerType(cgen_state_type(context), 0);
   else
      fields[next_field++] = llvm_void_ptr();

   if (has_fsm) {
      fields[next_field++] = llvm_int32_type();   // Current FSM state
      fields[next_field++] = llvm_void_ptr();   // Suspended pcall state
   }

   for (int i = 0; i < nvars; i++)
      fields[next_field++] = cgen_type(vcode_var_type(i));

   assert(next_field == nfields);
   LLVMStructSetBody(opaque, fields, nfields, false);

   vcode_state_restore(&state);
   return opaque;
}

static void cgen_global_offsets(LLVMTypeRef state_type)
{
   // Generate constants containing the offests of each global field in
   // the state type

   const int nvars = vcode_count_vars();
   for (int i = 0; i < nvars; i++) {
      if (vcode_var_flags(i) & VAR_GLOBAL) {
         LOCAL_TEXT_BUF symbol = safe_symbol(vcode_var_name(i));
         char *name LOCAL = xasprintf("__offset_%s", tb_get(symbol));
         LLVMValueRef global = LLVMAddGlobal(module, llvm_int32_type(), name);
#ifdef IMPLIB_REQUIRED
         LLVMSetDLLStorageClass(global, LLVMDLLExportStorageClass);
#endif
         LLVMTargetDataRef td = LLVMGetModuleDataLayout(module);
         const size_t offset =
            LLVMOffsetOfElement(td, state_type, cgen_var_offset(i));
         LLVMSetInitializer(global, llvm_int32(offset));
         LLVMSetGlobalConstant(global, true);
         LLVMSetUnnamedAddr(global, true);
      }
   }
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

      if (last_op == VCODE_OP_WAIT || last_op == VCODE_OP_PCALL)
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

static void cgen_procedure(void)
{
   assert(vcode_unit_kind() == VCODE_UNIT_PROCEDURE);

   const int nparams = vcode_count_params();
   vcode_type_t params[nparams];
   for (int i = 0; i < nparams; i++)
      params[i] = vcode_param_type(i);

   LLVMValueRef fn = cgen_signature(vcode_unit_name(), VCODE_INVALID_TYPE,
                                    VCODE_CC_VHDL, params, nparams);
   cgen_add_func_attr(fn, FUNC_ATTR_UWTABLE, -1);

   // Make sure the display type is not an opaque struct
   cgen_state_type(vcode_active_unit());

   cgen_ctx_t ctx = {
      .fn = fn,
   };

   LLVMBasicBlockRef entry_bb = llvm_append_block(fn, "entry");
   LLVMBasicBlockRef alloc_bb = llvm_append_block(fn, "alloc");
   LLVMBasicBlockRef jump_bb  = llvm_append_block(fn, "jump_table");

   cgen_debug_push_func(&ctx);
   cgen_alloc_context(&ctx);
   cgen_params(&ctx);

   LLVMPositionBuilderAtEnd(builder, entry_bb);

   LLVMTypeRef state_type = cgen_state_type(vcode_active_unit());
   LLVMTypeRef pointer_type = LLVMPointerType(state_type, 0);
   LLVMValueRef old_state =
      LLVMBuildPointerCast(builder, LLVMGetParam(fn, 0),
                           pointer_type, "old_state");

   LLVMValueRef is_null = LLVMBuildIsNull(builder, old_state, "");
   LLVMBuildCondBr(builder, is_null, alloc_bb, jump_bb);

   LLVMPositionBuilderAtEnd(builder, alloc_bb);

   LLVMValueRef new_state = LLVMBuildMalloc(builder, state_type, "new_state");

   LLVMValueRef display_ptr = LLVMBuildStructGEP(builder, new_state, 0, "");
   LLVMBuildStore(builder, LLVMGetParam(fn, 1), display_ptr);

   LLVMValueRef state_ptr = LLVMBuildStructGEP(builder, new_state, 1, "");
   LLVMBuildStore(builder, llvm_int32(0), state_ptr);

   LLVMBuildBr(builder, jump_bb);

   LLVMPositionBuilderAtEnd(builder, jump_bb);

   ctx.state = LLVMBuildPhi(builder, pointer_type, "state");

   LLVMValueRef phi_values[]   = { old_state, new_state };
   LLVMBasicBlockRef phi_bbs[] = { entry_bb,  alloc_bb  };
   LLVMAddIncoming(ctx.state, phi_values, phi_bbs, 2);

   LLVMValueRef new_display_ptr = LLVMBuildStructGEP(builder, ctx.state, 0, "");
   ctx.display = LLVMBuildLoad(builder, new_display_ptr, "display");

   cgen_jump_table(&ctx);
   cgen_code(&ctx);
   cgen_free_context(&ctx);
   cgen_pop_debug_scope();
}

static void cgen_process(void)
{
   assert(vcode_unit_kind() == VCODE_UNIT_PROCESS);

   cgen_ctx_t ctx = {};

   LLVMTypeRef display_type = cgen_state_type(vcode_unit_context());
   LLVMTypeRef state_type = cgen_state_type(vcode_active_unit());

   LLVMTypeRef pargs[] = {
      LLVMPointerType(state_type, 0),
      LLVMPointerType(display_type, 0)
   };
   LLVMTypeRef ftype = LLVMFunctionType(llvm_void_ptr(), pargs, 2, false);
   LOCAL_TEXT_BUF symbol = safe_symbol(vcode_unit_name());
   ctx.fn = LLVMAddFunction(module, tb_get(symbol), ftype);
   cgen_add_func_attr(ctx.fn, FUNC_ATTR_NOUNWIND, -1);
   cgen_add_func_attr(ctx.fn, FUNC_ATTR_DLLEXPORT, -1);
   cgen_add_func_attr(ctx.fn, FUNC_ATTR_UWTABLE, -1);

   LLVMBasicBlockRef entry_bb = llvm_append_block(ctx.fn, "entry");
   LLVMBasicBlockRef reset_bb = llvm_append_block(ctx.fn, "reset");
   LLVMBasicBlockRef jump_bb  = llvm_append_block(ctx.fn, "jump_table");

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
   LLVMTypeRef state_type = cgen_state_type(vcode_active_unit());

   if (vcode_unit_kind() == VCODE_UNIT_PACKAGE)
      cgen_global_offsets(state_type);

   LOCAL_TEXT_BUF symbol = safe_symbol(vcode_unit_name());
   char *name LOCAL = xasprintf("%s_reset", tb_get(symbol));

   vcode_unit_t context = vcode_unit_context();

   cgen_ctx_t ctx = {
      .fn = LLVMGetNamedFunction(module, name)
   };

   if (ctx.fn == NULL) {
      LLVMTypeRef args[1];
      if (context != NULL)
         args[0] = LLVMPointerType(cgen_state_type(context), 0);
      else
         args[0] = llvm_void_ptr();

      LLVMTypeRef ftype = LLVMFunctionType(llvm_void_ptr(), args, 1, false);
      ctx.fn = LLVMAddFunction(module, name, ftype);
   }

   cgen_add_func_attr(ctx.fn, FUNC_ATTR_DLLEXPORT, -1);
   cgen_add_func_attr(ctx.fn, FUNC_ATTR_UWTABLE, -1);
   cgen_add_func_attr(ctx.fn, FUNC_ATTR_OPTNONE, -1);
   cgen_add_func_attr(ctx.fn, FUNC_ATTR_NOINLINE, -1);
   cgen_add_func_attr(ctx.fn, FUNC_ATTR_COLD, -1);

   LLVMBasicBlockRef entry_bb = llvm_append_block(ctx.fn, "entry");
   LLVMPositionBuilderAtEnd(builder, entry_bb);

   cgen_debug_push_func(&ctx);
   cgen_alloc_context(&ctx);

   ctx.state = LLVMBuildMalloc(builder, state_type, "privdata");

   ctx.display = LLVMGetParam(ctx.fn, 0);
   LLVMValueRef context_ptr = LLVMBuildStructGEP(builder, ctx.state, 0, "");
   LLVMBuildStore(builder, ctx.display, context_ptr);

   if (vcode_unit_kind() == VCODE_UNIT_PACKAGE) {
      LOCAL_TEXT_BUF name = tb_new();
      ident_str(vcode_unit_name(), name);

      LLVMValueRef global = LLVMAddGlobal(module,
                                          LLVMPointerType(state_type, 0),
                                          tb_get(name));
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

static void cgen_coverage_state(tree_t t, cover_tagging_t *tagging,
                                bool external)
{
   int32_t stmt_tags, cond_tags;
   cover_count_tags(tagging, &stmt_tags, &cond_tags);

   if (stmt_tags > 0) {
      LLVMTypeRef type = LLVMArrayType(llvm_int32_type(), stmt_tags);
      LLVMValueRef var = LLVMAddGlobal(module, type, "cover_stmts");
      if (external)
         LLVMSetLinkage(var, LLVMExternalLinkage);
      else {
         LLVMSetInitializer(var, LLVMGetUndef(type));
         cgen_add_func_attr(var, FUNC_ATTR_DLLEXPORT, -1);
      }
   }

   if (cond_tags > 0) {
      LLVMTypeRef type = LLVMArrayType(llvm_int32_type(), stmt_tags);
      LLVMValueRef var = LLVMAddGlobal(module, type, "cover_conds");
      if (external)
         LLVMSetLinkage(var, LLVMExternalLinkage);
      else {
         LLVMSetInitializer(var, LLVMGetUndef(type));
         cgen_add_func_attr(var, FUNC_ATTR_DLLEXPORT, -1);
      }
   }
}

static void cgen_module_debug_info(tree_t top)
{
   llvm_add_module_flag("Debug Info Version", DEBUG_METADATA_VERSION);
#ifdef __APPLE__
   llvm_add_module_flag("Dwarf Version", 2);
#else
   llvm_add_module_flag("Dwarf Version", 4);
#endif

   const loc_t *loc = vcode_unit_loc();
   assert(!loc_invalid_p(loc));

   LLVMMetadataRef file_ref = cgen_debug_file(loc, false);

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

   LOCAL_TEXT_BUF tb = tb_new();
   ident_str(tree_ident(top), tb);

   LLVMMetadataRef mod = LLVMDIBuilderCreateModule(
      debuginfo, cu, tb_get(tb), tb_len(tb), "", 0, "", 0, "", 0);

   cgen_push_debug_scope(mod);
}

static void cgen_find_units(vcode_unit_t root, unit_list_t *units)
{
   vcode_select_unit(root);

   for (vcode_unit_t it = vcode_unit_child(root);
        it != NULL;
        it = vcode_unit_next(it)) {
      cgen_find_units(it, units);
   }

   APUSH(*units, root);
}

static void cgen_partition_jobs(unit_list_t *units, job_list_t *jobs,
                                const char *base_name, int units_per_job)
{
   int counter = 0;

   // Adjust units_per_job to ensure that each job has a roughly equal
   // number of units
   const int njobs = (units->count + units_per_job - 1) / units_per_job;
   units_per_job = (units->count + njobs - 1) / njobs;

   for (unsigned i = 0; i < units->count; i += units_per_job, counter++) {
      char *module_name = xasprintf("%s.%d", base_name, counter);
      char *obj_name LOCAL = xasprintf("_%s." LLVM_OBJ_EXT, module_name);

      char obj_path[PATH_MAX];
      lib_realpath(lib_work(), obj_name, obj_path, sizeof(obj_path));

      cgen_job_t job = {
         .module_name = module_name,
         .obj_path    = xstrdup(obj_path),
         .index       = counter,
      };

      for (unsigned j = i; j < units->count && j < i + units_per_job; j++)
         APUSH(job.units, units->items[j]);

      APUSH(*jobs, job);
   }

   assert(jobs->count == njobs);
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

   LLVMRunPassManager(mpm, module);
   LLVMDisposePassManager(mpm);
}

static LLVMValueRef cgen_support_fn(const char *name)
{
   LLVMValueRef fn = NULL;
   if (strcmp(name, "_sched_process") == 0) {
      LLVMTypeRef args[] = { llvm_int64_type() };
      fn = LLVMAddFunction(module, "_sched_process",
                           LLVMFunctionType(llvm_void_type(),
                                            args, ARRAY_LEN(args), false));
   }
   else if (strcmp(name, "_sched_waveform") == 0) {
      LLVMTypeRef args[] = {
         LLVMPointerType(llvm_signal_shared_struct(), 0),
         llvm_int32_type(),
         llvm_void_ptr(),
         llvm_int32_type(),
         llvm_int64_type(),
         llvm_int64_type()
      };
      fn = LLVMAddFunction(module, "_sched_waveform",
                           LLVMFunctionType(llvm_void_type(),
                                            args, ARRAY_LEN(args), false));
      cgen_add_func_attr(fn, FUNC_ATTR_NOCAPTURE, 1);
      cgen_add_func_attr(fn, FUNC_ATTR_NOCAPTURE, 3);
      cgen_add_func_attr(fn, FUNC_ATTR_READONLY, 3);
   }
   else if (strcmp(name, "_sched_waveform_s") == 0) {
      LLVMTypeRef args[] = {
         LLVMPointerType(llvm_signal_shared_struct(), 0),
         llvm_int32_type(),
         llvm_int64_type(),
         llvm_int64_type(),
         llvm_int64_type()
      };
      fn = LLVMAddFunction(module, "_sched_waveform_s",
                           LLVMFunctionType(llvm_void_type(),
                                            args, ARRAY_LEN(args), false));
      cgen_add_func_attr(fn, FUNC_ATTR_NOCAPTURE, 1);
   }
   else if (strcmp(name, "_sched_event") == 0) {
      LLVMTypeRef args[] = {
         LLVMPointerType(llvm_signal_shared_struct(), 0),
         llvm_int32_type(),
         llvm_int32_type()
      };
      fn = LLVMAddFunction(module, "_sched_event",
                           LLVMFunctionType(llvm_void_type(),
                                            args, ARRAY_LEN(args), false));
      cgen_add_func_attr(fn, FUNC_ATTR_NOCAPTURE, 1);
      cgen_add_func_attr(fn, FUNC_ATTR_READONLY, 1);
   }
   else if (strcmp(name, "_disconnect") == 0) {
      LLVMTypeRef args[] = {
         LLVMPointerType(llvm_signal_shared_struct(), 0),
         llvm_int32_type(),
         llvm_int32_type(),
         llvm_int64_type(),
         llvm_int64_type()
      };
      fn = LLVMAddFunction(module, "_disconnect",
                           LLVMFunctionType(llvm_void_type(),
                                            args, ARRAY_LEN(args), false));
   }
   else if (strcmp(name, "_init_signal") == 0) {
      LLVMTypeRef args[] = {
         LLVMPointerType(llvm_signal_shared_struct(), 0),
         llvm_int32_type(),
         llvm_int32_type(),
         llvm_int32_type(),
         llvm_void_ptr(),
         LLVMPointerType(llvm_resolution_type(), 0)
      };
      fn = LLVMAddFunction(module, "_init_signal",
                           LLVMFunctionType(llvm_void_type(),
                                            args, ARRAY_LEN(args), false));
      cgen_add_func_attr(fn, FUNC_ATTR_NOCAPTURE, 1);
      cgen_add_func_attr(fn, FUNC_ATTR_NOCAPTURE, 5);
      cgen_add_func_attr(fn, FUNC_ATTR_READONLY, 5);
      cgen_add_func_attr(fn, FUNC_ATTR_NOCAPTURE, 6);
      cgen_add_func_attr(fn, FUNC_ATTR_READONLY, 6);
   }
   else if (strcmp(name, "_link_signal") == 0) {
      LLVMTypeRef args[] = {
         LLVMPointerType(llvm_int8_type(), 0)
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
         LLVMPointerType(llvm_int8_type(), 0),
         llvm_int32_type(),
         llvm_int8_type(),
         llvm_int8_type(),
         LLVMPointerType(llvm_rt_loc(), 0)
      };
      fn = LLVMAddFunction(module, "_assert_fail",
                           LLVMFunctionType(llvm_void_type(),
                                            args, ARRAY_LEN(args), false));
      cgen_add_func_attr(fn, FUNC_ATTR_NOCAPTURE, 1);
      cgen_add_func_attr(fn, FUNC_ATTR_NOCAPTURE, 5);
      cgen_add_func_attr(fn, FUNC_ATTR_COLD, -1);
   }
   else if (strcmp(name, "_debug_out") == 0) {
      LLVMTypeRef args[] = {
         llvm_int32_type(),
         llvm_int32_type()
      };
      fn = LLVMAddFunction(module, "_debug_out",
                           LLVMFunctionType(llvm_void_type(),
                                            args, ARRAY_LEN(args), false));
   }
   else if (strcmp(name, "_debug_dump") == 0) {
      LLVMTypeRef args[] = {
         llvm_void_ptr(),
         llvm_int32_type()
      };
      fn = LLVMAddFunction(module, "_debug_dump",
                           LLVMFunctionType(llvm_void_type(),
                                            args, ARRAY_LEN(args), false));
   }
   else if (strcmp(name, "llvm.pow.f64") == 0) {
      LLVMTypeRef args[] = {
         llvm_double_type(),
         llvm_double_type()
      };
      fn = LLVMAddFunction(module, "llvm.pow.f64",
                           LLVMFunctionType(llvm_double_type(),
                                            args, ARRAY_LEN(args), false));
   }
   else if (strcmp(name, "llvm.round.f64") == 0) {
      LLVMTypeRef args[] = {
         llvm_double_type()
      };
      fn = LLVMAddFunction(module, "llvm.round.f64",
                           LLVMFunctionType(llvm_double_type(),
                                            args, ARRAY_LEN(args), false));
   }
   else if (strcmp(name, "llvm.memset.p0i8.i32") == 0) {
      LLVMTypeRef args[] = {
         LLVMPointerType(llvm_int8_type(), 0),
         llvm_int8_type(),
         llvm_int32_type(),
         llvm_int1_type()
      };
      fn = LLVMAddFunction(module, "llvm.memset.p0i8.i32",
                           LLVMFunctionType(llvm_void_type(),
                                            args, ARRAY_LEN(args), false));
   }
   else if (strncmp(name, "llvm.mem", 8) == 0) {
      int width;
      char kind[17];
      if (sscanf(name, "llvm.%16[^.].p0i%d", kind, &width) != 2)
         fatal("invalid memcpy intrinsic %s", name);

      LLVMTypeRef args[] = {
         LLVMPointerType(LLVMIntTypeInContext(llvm_context(), width), 0),
         LLVMPointerType(LLVMIntTypeInContext(llvm_context(), width), 0),
         llvm_int32_type(),
         llvm_int1_type()
      };
      fn = LLVMAddFunction(module, cgen_memcpy_name(kind, width),
                           LLVMFunctionType(llvm_void_type(),
                                            args, ARRAY_LEN(args), false));
   }
   else if (strcmp(name, "llvm.lifetime.start.p0i8") == 0) {
      LLVMTypeRef args[] = {
         llvm_int64_type(),
         llvm_void_ptr()
      };
      fn = LLVMAddFunction(module, "llvm.lifetime.start.p0i8",
                           LLVMFunctionType(llvm_void_type(), args, 2, false));
   }
   else if (strcmp(name, "llvm.lifetime.end.p0i8") == 0) {
      LLVMTypeRef args[] = {
         llvm_int64_type(),
         llvm_void_ptr()
      };
      fn = LLVMAddFunction(module, "llvm.lifetime.end.p0i8",
                           LLVMFunctionType(llvm_void_type(), args, 2, false));
   }
   else if (strcmp(name, "_file_open") == 0) {
      LLVMTypeRef args[] = {
         LLVMPointerType(llvm_int8_type(), 0),
         LLVMPointerType(llvm_void_ptr(), 0),
         LLVMPointerType(llvm_int8_type(), 0),
         llvm_int32_type(),
         llvm_int8_type()
      };
      fn = LLVMAddFunction(module, "_file_open",
                           LLVMFunctionType(llvm_void_type(),
                                            args, ARRAY_LEN(args), false));
   }
   else if (strcmp(name, "_file_write") == 0) {
      LLVMTypeRef args[] = {
         LLVMPointerType(llvm_void_ptr(), 0),
         llvm_void_ptr(),
         llvm_int32_type()
      };
      fn = LLVMAddFunction(module, "_file_write",
                           LLVMFunctionType(llvm_void_type(),
                                            args, ARRAY_LEN(args), false));
   }
   else if (strcmp(name, "_file_read") == 0) {
      LLVMTypeRef args[] = {
         LLVMPointerType(llvm_void_ptr(), 0),
         llvm_void_ptr(),
         llvm_int32_type(),
         llvm_int32_type(),
         LLVMPointerType(llvm_int32_type(), 0)
      };
      fn = LLVMAddFunction(module, "_file_read",
                           LLVMFunctionType(llvm_void_type(),
                                            args, ARRAY_LEN(args), false));
   }
   else if (strcmp(name, "_file_close") == 0) {
      LLVMTypeRef args[] = {
         LLVMPointerType(llvm_void_ptr(), 0)
      };
      fn = LLVMAddFunction(module, "_file_close",
                           LLVMFunctionType(llvm_void_type(),
                                            args, ARRAY_LEN(args), false));
   }
   else if (strcmp(name, "_endfile") == 0) {
      LLVMTypeRef args[] = {
         llvm_void_ptr()
      };
      fn = LLVMAddFunction(module, "_endfile",
                           LLVMFunctionType(llvm_int1_type(),
                                            args, ARRAY_LEN(args), false));
   }
   else if (strcmp(name, "_bounds_fail") == 0) {
      LLVMTypeRef args[] = {
         llvm_int32_type(),
         llvm_int32_type(),
         llvm_int32_type(),
         llvm_int32_type(),
         LLVMPointerType(llvm_rt_loc(), 0),
         llvm_char_ptr()
      };
      fn = LLVMAddFunction(module, "_bounds_fail",
                           LLVMFunctionType(llvm_void_type(),
                                            args, ARRAY_LEN(args), false));
      cgen_add_func_attr(fn, FUNC_ATTR_NORETURN, -1);
      cgen_add_func_attr(fn, FUNC_ATTR_COLD, -1);
   }
   else if (strcmp(name, "__nvc_index_fail") == 0) {
      LLVMTypeRef args[] = {
         llvm_int32_type(),
         llvm_int32_type(),
         llvm_int32_type(),
         llvm_int1_type(),
         llvm_debug_locus_type(),
      };
      fn = LLVMAddFunction(module, "__nvc_index_fail",
                           LLVMFunctionType(llvm_void_type(),
                                            args, ARRAY_LEN(args), false));
      cgen_add_func_attr(fn, FUNC_ATTR_NORETURN, -1);
      cgen_add_func_attr(fn, FUNC_ATTR_COLD, -1);
   }
   else if (strcmp(name, "_div_zero") == 0) {
      LLVMTypeRef args[] = {
         LLVMPointerType(llvm_rt_loc(), 0)
      };
      fn = LLVMAddFunction(module, "_div_zero",
                           LLVMFunctionType(llvm_void_type(),
                                            args, ARRAY_LEN(args), false));
      cgen_add_func_attr(fn, FUNC_ATTR_NORETURN, -1);
      cgen_add_func_attr(fn, FUNC_ATTR_COLD, -1);
   }
   else if (strcmp(name, "_null_deref") == 0) {
      LLVMTypeRef args[] = {
         LLVMPointerType(llvm_rt_loc(), 0)
      };
      fn = LLVMAddFunction(module, "_null_deref",
                           LLVMFunctionType(llvm_void_type(),
                                            args, ARRAY_LEN(args), false));
      cgen_add_func_attr(fn, FUNC_ATTR_NORETURN, -1);
      cgen_add_func_attr(fn, FUNC_ATTR_COLD, -1);
   }
   else if (strcmp(name, "_test_net_event") == 0) {
      LLVMTypeRef args[] = {
         LLVMPointerType(llvm_signal_shared_struct(), 0),
         llvm_int32_type(),
         llvm_int32_type()
      };
      fn = LLVMAddFunction(module, "_test_net_event",
                           LLVMFunctionType(llvm_int1_type(),
                                            args, ARRAY_LEN(args), false));
   }
   else if (strcmp(name, "_test_net_active") == 0) {
      LLVMTypeRef args[] = {
         LLVMPointerType(llvm_signal_shared_struct(), 0),
         llvm_int32_type(),
         llvm_int32_type()
      };
      fn = LLVMAddFunction(module, "_test_net_active",
                           LLVMFunctionType(llvm_int1_type(),
                                            args, ARRAY_LEN(args), false));
   }
   else if (strcmp(name, "_last_event") == 0) {
      LLVMTypeRef args[] = {
         LLVMPointerType(llvm_signal_shared_struct(), 0),
         llvm_int32_type(),
         llvm_int32_type()
      };
      fn = LLVMAddFunction(module, "_last_event",
                           LLVMFunctionType(llvm_int64_type(),
                                            args, ARRAY_LEN(args), false));
   }
   else if (strcmp(name, "_last_active") == 0) {
      LLVMTypeRef args[] = {
         LLVMPointerType(llvm_signal_shared_struct(), 0),
         llvm_int32_type(),
         llvm_int32_type()
      };
      fn = LLVMAddFunction(module, "_last_active",
                           LLVMFunctionType(llvm_int64_type(),
                                            args, ARRAY_LEN(args), false));
   }
   else if (strcmp(name, "_driving") == 0) {
      LLVMTypeRef args[] = {
         LLVMPointerType(llvm_signal_shared_struct(), 0),
         llvm_int32_type(),
         llvm_int32_type()
      };
      fn = LLVMAddFunction(module, "_driving",
                           LLVMFunctionType(llvm_int1_type(),
                                            args, ARRAY_LEN(args), false));
   }
   else if (strcmp(name, "_driving_value") == 0) {
      LLVMTypeRef args[] = {
         LLVMPointerType(llvm_signal_shared_struct(), 0),
         llvm_int32_type(),
         llvm_int32_type()
      };
      fn = LLVMAddFunction(module, "_driving_value",
                           LLVMFunctionType(llvm_void_ptr(),
                                            args, ARRAY_LEN(args), false));
   }
   else if (strcmp(name, "_convert_signal") == 0) {
      LLVMTypeRef args[] = {
         LLVMPointerType(llvm_signal_shared_struct(), 0),
         llvm_int32_type(),
         llvm_int32_type(),
         LLVMPointerType(llvm_closure_type(), 0),
      };
      fn = LLVMAddFunction(module, "_convert_signal",
                           LLVMFunctionType(llvm_void_type(),
                                            args, ARRAY_LEN(args), false));
   }
   else if (strcmp(name, "_implicit_signal") == 0) {
      LLVMTypeRef args[] = {
         LLVMPointerType(llvm_signal_shared_struct(), 0),
         llvm_int32_type(),
         LLVMPointerType(llvm_closure_type(), 0),
      };
      fn = LLVMAddFunction(module, "_implicit_signal",
                           LLVMFunctionType(llvm_void_type(),
                                            args, ARRAY_LEN(args), false));
   }
   else if (strcmp(name, "_private_stack") == 0) {
      fn = LLVMAddFunction(module, "_private_stack",
                           LLVMFunctionType(llvm_void_type(),
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
      LLVMAddGlobal(module, llvm_int32_type(), "_tmp_alloc");
   LLVMSetLinkage(_tmp_alloc, LLVMExternalLinkage);
}

static void cgen_abi_version(void)
{
   LLVMValueRef global =
      LLVMAddGlobal(module, llvm_int32_type(), "__nvc_abi_version");
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
static void cgen_find_dll_deps(ident_t unit_name, void *__ctx)
{
   ident_list_t **deps = __ctx;

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

   tree_walk_deps(unit, cgen_find_dll_deps, deps);
}
#endif  // IMPLIB_REQUIRED

static void cgen_native(LLVMTargetMachineRef tm_ref, char *obj_path)
{
   char *error;
   if (LLVMTargetMachineEmitToFile(tm_ref, module, obj_path,
                                   LLVMObjectFile, &error))
      fatal("Failed to write object file: %s", error);

#if DUMP_ASSEMBLY
   char *asm_name LOCAL = xasprintf("_%s.s", module_name);

   char asm_path[PATH_MAX];
   lib_realpath(lib_work(), asm_name, asm_path, sizeof(asm_path));

    if (LLVMTargetMachineEmitToFile(tm_ref, module, asm_path,
                                   LLVMAssemblyFile, &error))
      fatal("Failed to write assembly file: %s", error);
#endif

#if DUMP_BITCODE
    char *bc_name LOCAL = xasprintf("_%s.bc", module_name);

    char bc_path[PATH_MAX];
    lib_realpath(lib_work(), bc_name, bc_path, sizeof(bc_path));

    if (LLVMWriteBitcodeToFile(module, bc_path))
       fatal("Failed to write bitcode to file");
#endif
}

static void cgen_link(const char *module_name, char **objs, int nobjs)
{
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
#elif defined __OpenBSD__
   cgen_link_arg("-Bdynamic");
   cgen_link_arg("-shared");
   cgen_link_arg("/usr/lib/crtbeginS.o");
#else
   cgen_link_arg("-shared");
#endif

   char *fname LOCAL = xasprintf("_%s." DLL_EXT, module_name);
   char so_path[PATH_MAX];
   lib_realpath(lib_work(), fname, so_path, PATH_MAX);

   cgen_link_arg("-o");
   cgen_link_arg("%s", so_path);

   for (int i = 0; i < nobjs; i++)
      cgen_link_arg("%s", objs[i]);

#if defined LINKER_PATH && defined __OpenBSD__
   // Extra linker arguments to make constructors work on OpenBSD
   cgen_link_arg("-L/usr/lib");
   cgen_link_arg("-lcompiler_rt");
   cgen_link_arg("/usr/lib/crtendS.o");
#endif

#if IMPLIB_REQUIRED
   // Windows needs all symbols to be resolved when linking a DLL

   cgen_link_arg("-L%s", lib_path(lib_work()));

   ident_t name = ident_new(module_name);
   LOCAL_IDENT_LIST deps = NULL;
   cgen_find_dll_deps(name, &deps);

   for (const ident_list_t *it = deps; it != NULL; it = it->next) {
      if (it->ident == name)
         continue;

      lib_t lib = lib_require(ident_until(it->ident, '.'));

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
   char *cygarg LOCAL = xasprintf("-L%s", (cyglib != NULL) ? cyglib : LIBDIR);
   cgen_link_arg(cygarg);
   cgen_link_arg("-lnvcimp");
#endif

   APUSH(link_args, NULL);

   run_program((const char * const *)link_args.items, link_args.count);

   for (int i = 0; i < nobjs ; i++) {
      if (unlink(objs[i]) != 0)
         fatal_errno("unlink: %s", objs[i]);
   }

   progress("linking shared library");

   for (size_t i = 0; i < link_args.count; i++)
      free(link_args.items[i]);
   ACLEAR(link_args);
}

static void cgen_global_ctors(void)
{
   if (ctors.count == 0)
      return;

   LLVMTypeRef ctor_type = llvm_ctor_type();
   LLVMTypeRef array_type = LLVMArrayType(ctor_type, ctors.count);
   LLVMValueRef global = LLVMAddGlobal(module, array_type, "llvm.global_ctors");
   LLVMSetLinkage(global, LLVMAppendingLinkage);

   LLVMValueRef array = LLVMConstArray(ctor_type, ctors.items, ctors.count);
   LLVMSetInitializer(global, array);

   ACLEAR(ctors);
}

static void cgen_units(unit_list_t *units, tree_t top, cover_tagging_t *cover,
                       const char *module_name, LLVMTargetMachineRef tm_ref,
                       char *obj_path, bool primary)
{
   module = LLVMModuleCreateWithNameInContext(module_name, llvm_context());
   builder = LLVMCreateBuilderInContext(llvm_context());
   debuginfo = LLVMCreateDIBuilderDisallowUnresolved(module);

   char *triple = LLVMGetTargetMachineTriple(tm_ref);
   LLVMSetTarget(module, triple);
   LLVMDisposeMessage(triple);

   LLVMTargetDataRef data_ref = LLVMCreateTargetDataLayout(tm_ref);
   LLVMSetModuleDataLayout(module, data_ref);

   string_pool = shash_new(128);

   assert(units->count > 0);
   vcode_select_unit(units->items[0]);

   cgen_tmp_stack();
   cgen_module_debug_info(top);

   if (primary) {
      cgen_abi_version();
      cgen_coverage_state(top, cover, false);
   }
   else
      cgen_coverage_state(top, cover, true);

   for (unsigned i = 0; i < units->count; i++) {
      vcode_select_unit(units->items[i]);

      switch (vcode_unit_kind()) {
      case VCODE_UNIT_PROCEDURE:
         cgen_procedure();
         break;
      case VCODE_UNIT_FUNCTION:
         cgen_function();
         break;
      case VCODE_UNIT_PROCESS:
         cgen_process();
         break;
      case VCODE_UNIT_INSTANCE:
      case VCODE_UNIT_PROTECTED:
      case VCODE_UNIT_PACKAGE:
         cgen_reset_function();
         break;
      default:
         break;
      }
   }

   cgen_pop_debug_scope();
   cgen_pop_debug_scope();

   cgen_global_ctors();

   LLVMDIBuilderFinalize(debuginfo);

   const bool dump_ir = opt_get_int("dump-llvm");
   if (dump_ir) cgen_dump_module("initial");

   if (getenv("NVC_CGEN_VERBOSE") != NULL)
      LLVMDumpModule(module);

   if (LLVMVerifyModule(module, LLVMPrintMessageAction, NULL))
      fatal("LLVM verification failed");

   cgen_optimise();
   cgen_native(tm_ref, obj_path);

   if (dump_ir) cgen_dump_module("final");

   assert(debug_scopes.count == 0);

   shash_free(string_pool);
   string_pool = NULL;

   vcode_close();

   LLVMDisposeModule(module);
   LLVMDisposeBuilder(builder);
   LLVMDisposeDIBuilder(debuginfo);
   LLVMDisposeTargetData(data_ref);
}

static void *cgen_worker_thread(void *__arg)
{
   cgen_thread_params_t *params = __arg;

   thread_context = LLVMContextCreate();

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

   for (;;) {
      cgen_job_t job;
      {
         SCOPED_LOCK(params->lock);

         if (params->jobs->count == 0)
            break;

         job = APOP(*(params->jobs));
      }

      cgen_units(&(job.units), params->top, params->cover,
                 job.module_name, tm_ref, job.obj_path, job.index == 0);

      ACLEAR(job.units);
      free(job.module_name);
   }

   LLVMDisposeTargetMachine(tm_ref);
   LLVMDisposeMessage(def_triple);

   LLVMContextDispose(thread_context);
   thread_context = NULL;

   return NULL;
}

void cgen(tree_t top, vcode_unit_t vcode, cover_tagging_t *cover)
{
   ident_t name = tree_ident(top);
   if (tree_kind(top) == T_PACK_BODY)
      name = tree_ident(tree_primary(top));

   unit_list_t units = AINIT;
   cgen_find_units(vcode, &units);

   job_list_t jobs = AINIT;
   cgen_partition_jobs(&units, &jobs, istr(name), UNITS_PER_JOB);

   LLVMInitializeNativeTarget();
   LLVMInitializeNativeAsmPrinter();

   SCOPED_A(char *) objs = AINIT;
   for (unsigned i = 0; i < jobs.count; i++)
      APUSH(objs, jobs.items[i].obj_path);

   cgen_thread_params_t params = {
      .jobs   = &jobs,
      .lock   = mutex_create(),
      .top    = top,
      .cover  = cover,
   };

   const int njobs = jobs.count;

   int nprocs = 1;
   if (LLVMIsMultithreaded())
      nprocs = MIN(MAX(nvc_nprocs() - 1, 1), njobs);

   nvc_thread_t *worker[nprocs];
   for (int i = 0; i < nprocs; i++)
      worker[i] = thread_create(cgen_worker_thread, &params, "cgen worker");

   for (int i = 0; i < nprocs; i++)
      thread_join(worker[i]);

   assert(jobs.count == 0);
   ACLEAR(jobs);

   mutex_destroy(params.lock);

   progress("code generation for %d units using %d threads",
            units.count, nprocs);

   cgen_link(istr(name), objs.items, objs.count);

   for (unsigned i = 0; i < objs.count; i++)
      free(objs.items[i]);
   ACLEAR(objs);

   ACLEAR(units);
}
