//
//  Copyright (C) 2022-2023  Nick Gasson
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
#include "array.h"
#include "hash.h"
#include "ident.h"
#include "jit/jit-llvm.h"
#include "jit/jit-priv.h"
#include "lib.h"
#include "object.h"
#include "option.h"
#include "rt/rt.h"
#include "thread.h"

#include <assert.h>
#include <libgen.h>
#include <stdlib.h>
#include <limits.h>
#include <string.h>
#include <stdint.h>

#include <llvm-c/Analysis.h>
#include <llvm-c/Core.h>
#include <llvm-c/DebugInfo.h>
#include <llvm-c/Error.h>
#include <llvm-c/TargetMachine.h>

#if LLVM_HAS_PASS_BUILDER
#include <llvm-c/Transforms/PassBuilder.h>
#else
#include <llvm-c/Transforms/Scalar.h>
#include <llvm-c/Transforms/PassManagerBuilder.h>
#endif

typedef enum {
   LLVM_VOID,
   LLVM_PTR,
   LLVM_INT1,
   LLVM_INT8,
   LLVM_INT16,
   LLVM_INT32,
   LLVM_INT64,
   LLVM_INTPTR,
   LLVM_DOUBLE,

   LLVM_PAIR_I8_I1,
   LLVM_PAIR_I16_I1,
   LLVM_PAIR_I32_I1,
   LLVM_PAIR_I64_I1,

   LLVM_ENTRY_FN,
   LLVM_ANCHOR,
   LLVM_TLAB,
   LLVM_AOT_RELOC,
   LLVM_STRTAB,

   LLVM_LAST_TYPE
} llvm_type_t;

typedef enum {
   LLVM_ADD_OVERFLOW_S8,
   LLVM_ADD_OVERFLOW_S16,
   LLVM_ADD_OVERFLOW_S32,
   LLVM_ADD_OVERFLOW_S64,

   LLVM_ADD_OVERFLOW_U8,
   LLVM_ADD_OVERFLOW_U16,
   LLVM_ADD_OVERFLOW_U32,
   LLVM_ADD_OVERFLOW_U64,

   LLVM_SUB_OVERFLOW_S8,
   LLVM_SUB_OVERFLOW_S16,
   LLVM_SUB_OVERFLOW_S32,
   LLVM_SUB_OVERFLOW_S64,

   LLVM_SUB_OVERFLOW_U8,
   LLVM_SUB_OVERFLOW_U16,
   LLVM_SUB_OVERFLOW_U32,
   LLVM_SUB_OVERFLOW_U64,

   LLVM_MUL_OVERFLOW_S8,
   LLVM_MUL_OVERFLOW_S16,
   LLVM_MUL_OVERFLOW_S32,
   LLVM_MUL_OVERFLOW_S64,

   LLVM_MUL_OVERFLOW_U8,
   LLVM_MUL_OVERFLOW_U16,
   LLVM_MUL_OVERFLOW_U32,
   LLVM_MUL_OVERFLOW_U64,

   LLVM_EXP_OVERFLOW_S8,
   LLVM_EXP_OVERFLOW_S16,
   LLVM_EXP_OVERFLOW_S32,
   LLVM_EXP_OVERFLOW_S64,

   LLVM_EXP_OVERFLOW_U8,
   LLVM_EXP_OVERFLOW_U16,
   LLVM_EXP_OVERFLOW_U32,
   LLVM_EXP_OVERFLOW_U64,

   LLVM_MEMSET_U8,
   LLVM_MEMSET_U16,
   LLVM_MEMSET_U32,
   LLVM_MEMSET_U64,

   LLVM_POW_F64,
   LLVM_COPYSIGN_F64,
   LLVM_MEMSET_INLINE,
   LLVM_MEMCPY_INLINE,

   LLVM_DO_EXIT,
   LLVM_PUTPRIV,
   LLVM_MSPACE_ALLOC,
   LLVM_DO_FFICALL,
   LLVM_GET_OBJECT,
   LLVM_TLAB_ALLOC,
   LLVM_SCHED_WAVEFORM,
   LLVM_TEST_EVENT,
   LLVM_LAST_EVENT,

   LLVM_LAST_FN,
} llvm_fn_t;

typedef struct _cgen_func  cgen_func_t;
typedef struct _cgen_block cgen_block_t;

#define DEBUG_METADATA_VERSION 3
#define CLOSED_WORLD           1
#define ARGCACHE_SIZE          6
#define ENABLE_DWARF           0

#if defined __APPLE__ && defined ARCH_ARM64
#define JIT_CODE_MODEL LLVMCodeModelDefault
#else
#define JIT_CODE_MODEL LLVMCodeModelJITDefault
#endif

#if ENABLE_DWARF
#define DWARF_ONLY(x) x
#else
#define DWARF_ONLY(x)
#endif

typedef struct _llvm_obj {
   LLVMModuleRef         module;
   LLVMContextRef        context;
   LLVMTargetMachineRef  target;
   LLVMBuilderRef        builder;
#if ENABLE_DWARF
   LLVMDIBuilderRef      debuginfo;
   LLVMMetadataRef       debugcu;
#endif
   LLVMTargetDataRef     data_ref;
   LLVMTypeRef           types[LLVM_LAST_TYPE];
   LLVMValueRef          fns[LLVM_LAST_FN];
   LLVMTypeRef           fntypes[LLVM_LAST_FN];
   LLVMValueRef          strtab;
   pack_writer_t        *pack_writer;
} llvm_obj_t;

typedef struct _cgen_block {
   LLVMBasicBlockRef  bbref;
   LLVMValueRef       inflags;
   LLVMValueRef       outflags;
   LLVMValueRef      *inregs;
   LLVMValueRef      *outregs;
   jit_block_t       *source;
   cgen_func_t       *func;
} cgen_block_t;

typedef enum { CGEN_JIT, CGEN_AOT } cgen_mode_t;

typedef struct {
   reloc_kind_t kind;
   unsigned     nth;
   LLVMValueRef str;
   uintptr_t    key;
} cgen_reloc_t;

typedef struct _cgen_func {
   LLVMValueRef     llvmfn;
   LLVMValueRef     args;
   LLVMValueRef     argcache[ARGCACHE_SIZE];
   LLVMValueRef     irpos;
   LLVMValueRef     tlab;
   LLVMValueRef     anchor;
   LLVMValueRef     cpool;
   LLVMTypeRef      cpool_type;
   LLVMValueRef     descr;
   LLVMTypeRef      descr_type;
   LLVMTypeRef      reloc_type;
   LLVMMetadataRef  debugmd;
   cgen_block_t    *blocks;
   jit_func_t      *source;
   jit_cfg_t       *cfg;
   char            *name;
   loc_t            last_loc;
   bit_mask_t       ptr_mask;
   cgen_mode_t      mode;
   cgen_reloc_t    *relocs;
} cgen_func_t;

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
   FUNC_ATTR_NOALIAS,
   FUNC_ATTR_INLINE,

   // Attributes requiring special handling
   FUNC_ATTR_PRESERVE_FP,
   FUNC_ATTR_DLLEXPORT,
} func_attr_t;

#define LLVM_CHECK(op, ...) do {                        \
      LLVMErrorRef error = op(__VA_ARGS__);             \
      if (unlikely(error != LLVMErrorSuccess)) {        \
         char *msg = LLVMGetErrorMessage(error);        \
         fatal(#op " failed: %s", msg);                 \
      }                                                 \
   } while (0)

#ifdef LLVM_HAS_OPAQUE_POINTERS
#define PTR(x) x
#else
#define PTR(x) \
   LLVMBuildPointerCast(obj->builder, (x), obj->types[LLVM_PTR], "")
#endif

////////////////////////////////////////////////////////////////////////////////
// LLVM wrappers

static LLVMValueRef llvm_int1(llvm_obj_t *obj, bool b)
{
   return LLVMConstInt(obj->types[LLVM_INT1], b, false);
}

static LLVMValueRef llvm_int8(llvm_obj_t *obj, int8_t i)
{
   return LLVMConstInt(obj->types[LLVM_INT8], i, false);
}

static LLVMValueRef llvm_int32(llvm_obj_t *obj, int32_t i)
{
   return LLVMConstInt(obj->types[LLVM_INT32], i, false);
}

static LLVMValueRef llvm_int64(llvm_obj_t *obj, int64_t i)
{
   return LLVMConstInt(obj->types[LLVM_INT64], i, false);
}

static LLVMValueRef llvm_intptr(llvm_obj_t *obj, intptr_t i)
{
   return LLVMConstInt(obj->types[LLVM_INTPTR], i, false);
}

static LLVMValueRef llvm_ptr(llvm_obj_t *obj, void *ptr)
{
   return LLVMConstIntToPtr(llvm_intptr(obj, (intptr_t)ptr),
                            obj->types[LLVM_PTR]);
}

static LLVMValueRef llvm_real(llvm_obj_t *obj, double r)
{
   return LLVMConstReal(obj->types[LLVM_DOUBLE], r);
}

static void llvm_add_func_attr(llvm_obj_t *obj, LLVMValueRef fn,
                               func_attr_t attr, int param)
{
   LLVMAttributeRef ref;
   if (attr == FUNC_ATTR_DLLEXPORT) {
#ifdef IMPLIB_REQUIRED
      LLVMSetDLLStorageClass(fn, LLVMDLLExportStorageClass);
#endif
      return;
   }
   else if (attr == FUNC_ATTR_PRESERVE_FP) {
      ref = LLVMCreateStringAttribute(obj->context, "frame-pointer",
                                      13, "all", 3);
   }
   else {
      const char *names[] = {
         "nounwind", "noreturn", "readonly", "nocapture", "byval",
         "uwtable", "noinline", "writeonly", "nonnull", "cold", "optnone",
         "noalias", "inlinehint",
      };
      assert(attr < ARRAY_LEN(names));

      const unsigned kind =
         LLVMGetEnumAttributeKindForName(names[attr], strlen(names[attr]));
      if (kind == 0)
         fatal_trace("Cannot get LLVM attribute for %s", names[attr]);

#ifdef LLVM_UWTABLE_HAS_ARGUMENT
      if (attr == FUNC_ATTR_UWTABLE)
         ref = LLVMCreateEnumAttribute(obj->context, kind, 2);
      else
#endif
         ref = LLVMCreateEnumAttribute(obj->context, kind, 0);
   }

   LLVMAddAttributeAtIndex(fn, param, ref);
}

static void llvm_register_types(llvm_obj_t *obj)
{
   obj->types[LLVM_VOID]   = LLVMVoidTypeInContext(obj->context);
   obj->types[LLVM_INT1]   = LLVMInt1TypeInContext(obj->context);
   obj->types[LLVM_INT8]   = LLVMInt8TypeInContext(obj->context);
   obj->types[LLVM_INT16]  = LLVMInt16TypeInContext(obj->context);
   obj->types[LLVM_INT32]  = LLVMInt32TypeInContext(obj->context);
   obj->types[LLVM_INT64]  = LLVMInt64TypeInContext(obj->context);
   obj->types[LLVM_DOUBLE] = LLVMDoubleTypeInContext(obj->context);

   obj->types[LLVM_INTPTR] = LLVMIntPtrTypeInContext(obj->context,
                                                     obj->data_ref);

#ifdef LLVM_HAS_OPAQUE_POINTERS
   obj->types[LLVM_PTR] = LLVMPointerTypeInContext(obj->context, 0);
#else
   obj->types[LLVM_PTR] = LLVMPointerType(obj->types[LLVM_INT8], 0);
#endif

   obj->types[LLVM_STRTAB] = LLVMArrayType(obj->types[LLVM_INT8], 0);

   {
      LLVMTypeRef fields[] = {
         obj->types[LLVM_PTR],     // Mspace object
         obj->types[LLVM_PTR],     // Base pointer
         obj->types[LLVM_INT32],   // Allocation pointer
         obj->types[LLVM_INT32],   // Limit pointer
         obj->types[LLVM_PTR],     // Mptr object
      };
      obj->types[LLVM_TLAB] = LLVMStructTypeInContext(obj->context, fields,
                                                      ARRAY_LEN(fields), false);
   }

   {
      LLVMTypeRef atypes[] = {
         obj->types[LLVM_PTR],    // Function
         obj->types[LLVM_PTR],    // Anchor
#ifdef LLVM_HAS_OPAQUE_POINTERS
         obj->types[LLVM_PTR],    // Arguments
#else
         LLVMPointerType(obj->types[LLVM_INT64], 0),
#endif
#ifdef LLVM_HAS_OPAQUE_POINTERS
         obj->types[LLVM_PTR]     // TLAB pointer
#else
         LLVMPointerType(obj->types[LLVM_TLAB], 0)
#endif
      };
      obj->types[LLVM_ENTRY_FN] = LLVMFunctionType(obj->types[LLVM_VOID],
                                                   atypes, ARRAY_LEN(atypes),
                                                   false);
   }

   {
      LLVMTypeRef fields[] = {
         obj->types[LLVM_INT32],   // Kind
         obj->types[LLVM_INTPTR],  // Data pointer
      };
      obj->types[LLVM_AOT_RELOC] = LLVMStructTypeInContext(obj->context, fields,
                                                           ARRAY_LEN(fields),
                                                           false);
   }

   {
      LLVMTypeRef fields[] = {
         obj->types[LLVM_PTR],    // Caller
         obj->types[LLVM_PTR],    // Function
         obj->types[LLVM_INT32],  // IR position
         obj->types[LLVM_INT32]   // TLAB watermark
      };
      obj->types[LLVM_ANCHOR] = LLVMStructTypeInContext(obj->context, fields,
                                                        ARRAY_LEN(fields),
                                                        false);
   }

   for (jit_size_t sz = JIT_SZ_8; sz <= JIT_SZ_64; sz++) {
      LLVMTypeRef fields[] = {
         obj->types[LLVM_INT8 + sz],
         obj->types[LLVM_INT1]
      };
      llvm_fn_t which = LLVM_PAIR_I8_I1 + sz;
      obj->types[which] = LLVMStructTypeInContext(obj->context, fields,
                                                  ARRAY_LEN(fields), false);
   }
}

static void llvm_dump_module(LLVMModuleRef module, const char *tag)
{
   size_t length;
   const char *module_name = LLVMGetModuleIdentifier(module, &length);

   if (!opt_get_verbose(OPT_LLVM_VERBOSE, module_name))
      return;

   LOCAL_TEXT_BUF tb = tb_new();
   tb_printf(tb, "%s.%s.ll", module_name, tag);

   char *error;
   if (LLVMPrintModuleToFile(module, tb_get(tb), &error))
      fatal("Failed to write LLVM IR file: %s", error);

   debugf("wrote LLVM IR for %s to %s", module_name, tb_get(tb));
}

static void llvm_verify_module(LLVMModuleRef module)
{
#ifdef DEBUG
   if (LLVMVerifyModule(module, LLVMPrintMessageAction, NULL)) {
      size_t len;
      const char *name = LLVMGetModuleIdentifier(module, &len);
      fatal("LLVM verification failed for %s", name);
   }
#endif
}

static void llvm_optimise(LLVMModuleRef module, LLVMTargetMachineRef target,
                          llvm_opt_level_t olevel)
{
   assert(olevel >= LLVM_O0 && olevel <= LLVM_O3);

#if LLVM_HAS_PASS_BUILDER
   LLVMPassBuilderOptionsRef options = LLVMCreatePassBuilderOptions();
   LLVMPassBuilderOptionsSetDebugLogging(options, false);
   LLVMPassBuilderOptionsSetLoopVectorization(options, false);
   LLVMPassBuilderOptionsSetLoopInterleaving(options, false);
   LLVMPassBuilderOptionsSetSLPVectorization(options, false);
   LLVMPassBuilderOptionsSetLoopUnrolling(options, false);
   LLVMPassBuilderOptionsSetCallGraphProfile(options, false);

   const char *passes[] = {
      "default<O0>", "default<O1>", "default<O2>", "default<O3>"
   };

   LLVM_CHECK(LLVMRunPasses, module, passes[olevel], target, options);

   LLVMDisposePassBuilderOptions(options);
#else
   LLVMPassManagerRef fpm = LLVMCreateFunctionPassManagerForModule(module);
   LLVMPassManagerRef mpm = LLVMCreatePassManager();

   LLVMInitializeFunctionPassManager(fpm);

   for (LLVMValueRef fn = LLVMGetFirstFunction(module);
        fn != NULL; fn = LLVMGetNextFunction(fn))
      LLVMRunFunctionPassManager(fpm, fn);

   LLVMFinalizeFunctionPassManager(fpm);
   LLVMDisposePassManager(fpm);

   LLVMPassManagerBuilderRef builder = LLVMPassManagerBuilderCreate();
   LLVMPassManagerBuilderSetOptLevel(builder, olevel);
   LLVMPassManagerBuilderSetSizeLevel(builder, 0);

   if (olevel >= 2)
      LLVMPassManagerBuilderUseInlinerWithThreshold(builder, 50);

   LLVMPassManagerBuilderPopulateModulePassManager(builder, mpm);
   LLVMPassManagerBuilderDispose(builder);

   LLVMRunPassManager(mpm, module);
   LLVMDisposePassManager(mpm);
#endif
}

static LLVMTargetMachineRef llvm_target_machine(LLVMRelocMode reloc,
                                                LLVMCodeModel model)
{
   char *def_triple = LLVMGetDefaultTargetTriple();
   char *error;
   LLVMTargetRef target_ref;
   if (LLVMGetTargetFromTriple(def_triple, &target_ref, &error))
      fatal("failed to get LLVM target for %s: %s", def_triple, error);

   LLVMTargetMachineRef tm = LLVMCreateTargetMachine(target_ref, def_triple,
                                                     "", "",
                                                     LLVMCodeGenLevelDefault,
                                                     reloc, model);
   LLVMDisposeMessage(def_triple);

   return tm;
}

static LLVMBasicBlockRef llvm_append_block(llvm_obj_t *obj, LLVMValueRef fn,
                                           const char *name)
{
   return LLVMAppendBasicBlockInContext(obj->context, fn, name);
}

static LLVMValueRef llvm_add_fn(llvm_obj_t *obj, const char *name,
                                LLVMTypeRef type)
{
   LLVMValueRef fn = LLVMGetNamedFunction(obj->module, name);
   if (fn == NULL)
      fn = LLVMAddFunction(obj->module, name, type);

   return fn;
}

static LLVMValueRef llvm_get_fn(llvm_obj_t *obj, llvm_fn_t which)
{
   if (obj->fns[which] != NULL)
      return obj->fns[which];

   LLVMValueRef fn = NULL;
   switch (which) {
   case LLVM_ADD_OVERFLOW_S8:
   case LLVM_ADD_OVERFLOW_S16:
   case LLVM_ADD_OVERFLOW_S32:
   case LLVM_ADD_OVERFLOW_S64:
      {
         jit_size_t sz = which - LLVM_ADD_OVERFLOW_S8;
         LLVMTypeRef int_type = obj->types[LLVM_INT8 + sz];
         LLVMTypeRef pair_type = obj->types[LLVM_PAIR_I8_I1 + sz];
         LLVMTypeRef args[] = { int_type, int_type };
         obj->fntypes[which] = LLVMFunctionType(pair_type, args,
                                                ARRAY_LEN(args), false);

         static const char *names[] = {
            "llvm.sadd.with.overflow.i8",
            "llvm.sadd.with.overflow.i16",
            "llvm.sadd.with.overflow.i32",
            "llvm.sadd.with.overflow.i64"
         };
         fn = llvm_add_fn(obj, names[sz], obj->fntypes[which]);
      }
      break;

   case LLVM_ADD_OVERFLOW_U8:
   case LLVM_ADD_OVERFLOW_U16:
   case LLVM_ADD_OVERFLOW_U32:
   case LLVM_ADD_OVERFLOW_U64:
      {
         jit_size_t sz = which - LLVM_ADD_OVERFLOW_U8;
         LLVMTypeRef int_type = obj->types[LLVM_INT8 + sz];
         LLVMTypeRef pair_type = obj->types[LLVM_PAIR_I8_I1 + sz];
         LLVMTypeRef args[] = { int_type, int_type };
         obj->fntypes[which] = LLVMFunctionType(pair_type, args,
                                                ARRAY_LEN(args), false);

         static const char *names[] = {
            "llvm.uadd.with.overflow.i8",
            "llvm.uadd.with.overflow.i16",
            "llvm.uadd.with.overflow.i32",
            "llvm.uadd.with.overflow.i64"
         };
         fn = llvm_add_fn(obj, names[sz], obj->fntypes[which]);
      }
      break;

   case LLVM_SUB_OVERFLOW_S8:
   case LLVM_SUB_OVERFLOW_S16:
   case LLVM_SUB_OVERFLOW_S32:
   case LLVM_SUB_OVERFLOW_S64:
      {
         jit_size_t sz = which - LLVM_SUB_OVERFLOW_S8;
         LLVMTypeRef int_type = obj->types[LLVM_INT8 + sz];
         LLVMTypeRef pair_type = obj->types[LLVM_PAIR_I8_I1 + sz];
         LLVMTypeRef args[] = { int_type, int_type };
         obj->fntypes[which] = LLVMFunctionType(pair_type, args,
                                                ARRAY_LEN(args), false);

         static const char *names[] = {
            "llvm.ssub.with.overflow.i8",
            "llvm.ssub.with.overflow.i16",
            "llvm.ssub.with.overflow.i32",
            "llvm.ssub.with.overflow.i64"
         };
         fn = llvm_add_fn(obj, names[sz], obj->fntypes[which]);
      }
      break;

   case LLVM_SUB_OVERFLOW_U8:
   case LLVM_SUB_OVERFLOW_U16:
   case LLVM_SUB_OVERFLOW_U32:
   case LLVM_SUB_OVERFLOW_U64:
      {
         jit_size_t sz = which - LLVM_SUB_OVERFLOW_U8;
         LLVMTypeRef int_type = obj->types[LLVM_INT8 + sz];
         LLVMTypeRef pair_type = obj->types[LLVM_PAIR_I8_I1 + sz];
         LLVMTypeRef args[] = { int_type, int_type };
         obj->fntypes[which] = LLVMFunctionType(pair_type, args,
                                                ARRAY_LEN(args), false);

         static const char *names[] = {
            "llvm.usub.with.overflow.i8",
            "llvm.usub.with.overflow.i16",
            "llvm.usub.with.overflow.i32",
            "llvm.usub.with.overflow.i64"
         };
         fn = llvm_add_fn(obj, names[sz], obj->fntypes[which]);
      }
      break;

   case LLVM_MUL_OVERFLOW_S8:
   case LLVM_MUL_OVERFLOW_S16:
   case LLVM_MUL_OVERFLOW_S32:
   case LLVM_MUL_OVERFLOW_S64:
      {
         jit_size_t sz = which - LLVM_MUL_OVERFLOW_S8;
         LLVMTypeRef int_type = obj->types[LLVM_INT8 + sz];
         LLVMTypeRef pair_type = obj->types[LLVM_PAIR_I8_I1 + sz];
         LLVMTypeRef args[] = { int_type, int_type };
         obj->fntypes[which] = LLVMFunctionType(pair_type, args,
                                                ARRAY_LEN(args), false);

         static const char *names[] = {
            "llvm.smul.with.overflow.i8",
            "llvm.smul.with.overflow.i16",
            "llvm.smul.with.overflow.i32",
            "llvm.smul.with.overflow.i64"
         };
         fn = llvm_add_fn(obj, names[sz], obj->fntypes[which]);
      }
      break;

   case LLVM_MUL_OVERFLOW_U8:
   case LLVM_MUL_OVERFLOW_U16:
   case LLVM_MUL_OVERFLOW_U32:
   case LLVM_MUL_OVERFLOW_U64:
      {
         jit_size_t sz = which - LLVM_MUL_OVERFLOW_U8;
         LLVMTypeRef int_type = obj->types[LLVM_INT8 + sz];
         LLVMTypeRef pair_type = obj->types[LLVM_PAIR_I8_I1 + sz];
         LLVMTypeRef args[] = { int_type, int_type };
         obj->fntypes[which] = LLVMFunctionType(pair_type, args,
                                                ARRAY_LEN(args), false);

         static const char *names[] = {
            "llvm.umul.with.overflow.i8",
            "llvm.umul.with.overflow.i16",
            "llvm.umul.with.overflow.i32",
            "llvm.umul.with.overflow.i64"
         };
         fn = llvm_add_fn(obj, names[sz], obj->fntypes[which]);
      }
      break;

   case LLVM_EXP_OVERFLOW_S8:
   case LLVM_EXP_OVERFLOW_S16:
   case LLVM_EXP_OVERFLOW_S32:
   case LLVM_EXP_OVERFLOW_S64:
      {
         jit_size_t sz = which - LLVM_EXP_OVERFLOW_S8;
         LLVMTypeRef int_type = obj->types[LLVM_INT8 + sz];
         LLVMTypeRef pair_type = obj->types[LLVM_PAIR_I8_I1 + sz];
         LLVMTypeRef args[] = { int_type, int_type };
         obj->fntypes[which] = LLVMFunctionType(pair_type, args,
                                                ARRAY_LEN(args), false);

         static const char *names[] = {
            "nvc.sexp.with.overflow.i16",
            "nvc.sexp.with.overflow.i16",
            "nvc.sexp.with.overflow.i32",
            "nvc.sexp.with.overflow.i64"
         };
         fn = llvm_add_fn(obj, names[sz], obj->fntypes[which]);
         llvm_add_func_attr(obj, fn, FUNC_ATTR_NOUNWIND, -1);
      }
      break;

   case LLVM_EXP_OVERFLOW_U8:
   case LLVM_EXP_OVERFLOW_U16:
   case LLVM_EXP_OVERFLOW_U32:
   case LLVM_EXP_OVERFLOW_U64:
      {
         jit_size_t sz = which - LLVM_EXP_OVERFLOW_U8;
         LLVMTypeRef int_type = obj->types[LLVM_INT8 + sz];
         LLVMTypeRef pair_type = obj->types[LLVM_PAIR_I8_I1 + sz];
         LLVMTypeRef args[] = { int_type, int_type };
         obj->fntypes[which] = LLVMFunctionType(pair_type, args,
                                                ARRAY_LEN(args), false);

         static const char *names[] = {
            "nvc.uexp.with.overflow.i16",
            "nvc.uexp.with.overflow.i16",
            "nvc.uexp.with.overflow.i32",
            "nvc.uexp.with.overflow.i64"
         };
         fn = llvm_add_fn(obj, names[sz], obj->fntypes[which]);
      }
      break;

   case LLVM_MEMSET_U8:
   case LLVM_MEMSET_U16:
   case LLVM_MEMSET_U32:
   case LLVM_MEMSET_U64:
      {
         jit_size_t sz = which - LLVM_MEMSET_U8;
         LLVMTypeRef args[] = {
            obj->types[LLVM_PTR],
            obj->types[LLVM_INT8 + sz],
            obj->types[LLVM_INT64]
         };
         obj->fntypes[which] = LLVMFunctionType(obj->types[LLVM_VOID], args,
                                                ARRAY_LEN(args), false);

         static const char *names[] = {
            NULL,   // Use regular memset instead
            "nvc.memset.i16",
            "nvc.memset.i32",
            "nvc.memset.i64"
         };
         fn = llvm_add_fn(obj, names[sz], obj->fntypes[which]);
      }
      break;

   case LLVM_POW_F64:
      {
         LLVMTypeRef args[] = {
            obj->types[LLVM_DOUBLE],
            obj->types[LLVM_DOUBLE]
         };
         obj->fntypes[which] = LLVMFunctionType(obj->types[LLVM_DOUBLE],
                                                args, ARRAY_LEN(args), false);

         fn = llvm_add_fn(obj, "llvm.pow.f64", obj->fntypes[which]);
      }
      break;

   case LLVM_COPYSIGN_F64:
      {
         LLVMTypeRef args[] = {
            obj->types[LLVM_DOUBLE],
            obj->types[LLVM_DOUBLE]
         };
         obj->fntypes[which] = LLVMFunctionType(obj->types[LLVM_DOUBLE],
                                                args, ARRAY_LEN(args), false);

         fn = llvm_add_fn(obj, "llvm.copysign.f64", obj->fntypes[which]);
      }
      break;

   case LLVM_MEMSET_INLINE:
      {
         LLVMTypeRef args[] = {
            obj->types[LLVM_PTR],
            obj->types[LLVM_INT8],
            obj->types[LLVM_INT64],
            obj->types[LLVM_INT1],
         };
         obj->fntypes[which] = LLVMFunctionType(obj->types[LLVM_VOID],
                                                args, ARRAY_LEN(args), false);

         fn = llvm_add_fn(obj, "llvm.memset.inline.p0.i64",
                          obj->fntypes[which]);
      }
      break;

   case LLVM_MEMCPY_INLINE:
      {
         LLVMTypeRef args[] = {
            obj->types[LLVM_PTR],
            obj->types[LLVM_PTR],
            obj->types[LLVM_INT64],
            obj->types[LLVM_INT1],
         };
         obj->fntypes[which] = LLVMFunctionType(obj->types[LLVM_VOID],
                                                args, ARRAY_LEN(args), false);

         fn = llvm_add_fn(obj, "llvm.memcpy.inline.p0.p0.i64",
                          obj->fntypes[which]);
      }
      break;

   case LLVM_DO_EXIT:
      {
         LLVMTypeRef args[] = {
            obj->types[LLVM_INT32],
            obj->types[LLVM_PTR],
#ifdef LLVM_HAS_OPAQUE_POINTERS
            obj->types[LLVM_PTR],
#else
            LLVMPointerType(obj->types[LLVM_INT64], 0),
#endif
#ifdef LLVM_HAS_OPAQUE_POINTERS
            obj->types[LLVM_PTR],
#else
            LLVMPointerType(obj->types[LLVM_TLAB], 0),
#endif
         };
         obj->fntypes[which] = LLVMFunctionType(obj->types[LLVM_VOID], args,
                                                ARRAY_LEN(args), false);

         fn = llvm_add_fn(obj, "__nvc_do_exit", obj->fntypes[which]);
         llvm_add_func_attr(obj, fn, FUNC_ATTR_NOUNWIND, -1);
         llvm_add_func_attr(obj, fn, FUNC_ATTR_READONLY, 2);
         llvm_add_func_attr(obj, fn, FUNC_ATTR_NOCAPTURE, 2);
         llvm_add_func_attr(obj, fn, FUNC_ATTR_NOCAPTURE, 3);
         llvm_add_func_attr(obj, fn, FUNC_ATTR_NOCAPTURE, 4);
      }
      break;

   case LLVM_SCHED_WAVEFORM:
   case LLVM_TEST_EVENT:
   case LLVM_LAST_EVENT:
      {
         LLVMTypeRef args[] = {
            obj->types[LLVM_PTR],
#ifdef LLVM_HAS_OPAQUE_POINTERS
            obj->types[LLVM_PTR],
#else
            LLVMPointerType(obj->types[LLVM_INT64], 0),
#endif
#ifdef LLVM_HAS_OPAQUE_POINTERS
            obj->types[LLVM_PTR],
#else
            LLVMPointerType(obj->types[LLVM_TLAB], 0),
#endif
         };
         obj->fntypes[which] = LLVMFunctionType(obj->types[LLVM_VOID], args,
                                                ARRAY_LEN(args), false);

         const char *sym = NULL;
         switch (which) {
         case LLVM_SCHED_WAVEFORM: sym = "__nvc_sched_waveform"; break;
         case LLVM_TEST_EVENT: sym = "__nvc_test_event"; break;
         case LLVM_LAST_EVENT: sym = "__nvc_last_event"; break;
         default: break;
         }

         fn = llvm_add_fn(obj, sym, obj->fntypes[which]);
         llvm_add_func_attr(obj, fn, FUNC_ATTR_NOUNWIND, -1);
         llvm_add_func_attr(obj, fn, FUNC_ATTR_READONLY, 1);
         llvm_add_func_attr(obj, fn, FUNC_ATTR_NOCAPTURE, 1);
         llvm_add_func_attr(obj, fn, FUNC_ATTR_NOCAPTURE, 2);
         llvm_add_func_attr(obj, fn, FUNC_ATTR_NOCAPTURE, 3);
      }
      break;

   case LLVM_DO_FFICALL:
      {
         LLVMTypeRef args[] = {
            obj->types[LLVM_PTR],
            obj->types[LLVM_PTR],
#ifdef LLVM_HAS_OPAQUE_POINTERS
            obj->types[LLVM_PTR],
#else
            LLVMPointerType(obj->types[LLVM_INT64], 0),
#endif
         };
         obj->fntypes[which] = LLVMFunctionType(obj->types[LLVM_VOID], args,
                                                ARRAY_LEN(args), false);

         fn = llvm_add_fn(obj, "__nvc_do_fficall", obj->fntypes[which]);
         llvm_add_func_attr(obj, fn, FUNC_ATTR_NOUNWIND, -1);
      }
      break;

   case LLVM_PUTPRIV:
      {
         LLVMTypeRef args[] = {
            obj->types[LLVM_INT32],
            obj->types[LLVM_PTR]
         };
         obj->fntypes[which] = LLVMFunctionType(obj->types[LLVM_VOID], args,
                                                ARRAY_LEN(args), false);

         fn = llvm_add_fn(obj, "__nvc_putpriv", obj->fntypes[which]);
         llvm_add_func_attr(obj, fn, FUNC_ATTR_NOUNWIND, -1);
      }
      break;

   case LLVM_MSPACE_ALLOC:
      {
         LLVMTypeRef args[] = {
            obj->types[LLVM_INTPTR],
#ifdef LLVM_HAS_OPAQUE_POINTERS
            obj->types[LLVM_PTR],
#else
            LLVMPointerType(obj->types[LLVM_ANCHOR], 0),
#endif
         };
         obj->fntypes[which] = LLVMFunctionType(obj->types[LLVM_PTR], args,
                                                ARRAY_LEN(args), false);

         fn = llvm_add_fn(obj, "__nvc_mspace_alloc", obj->fntypes[which]);
         llvm_add_func_attr(obj, fn, FUNC_ATTR_NOUNWIND, -1);
      }
      break;

   case LLVM_GET_OBJECT:
      {
         LLVMTypeRef args[] = {
            obj->types[LLVM_PTR],
            obj->types[LLVM_INTPTR]
         };
         obj->fntypes[which] = LLVMFunctionType(obj->types[LLVM_PTR], args,
                                                ARRAY_LEN(args), false);
         fn = llvm_add_fn(obj, "__nvc_get_object", obj->fntypes[which]);
         llvm_add_func_attr(obj, fn, FUNC_ATTR_NOUNWIND, -1);
         llvm_add_func_attr(obj, fn, FUNC_ATTR_NOCAPTURE, 1);
         llvm_add_func_attr(obj, fn, FUNC_ATTR_READONLY, 1);
      }
      break;

   case LLVM_TLAB_ALLOC:
      {
         LLVMTypeRef args[] = {
#ifdef LLVM_HAS_OPAQUE_POINTERS
            obj->types[LLVM_PTR],
#else
            LLVMPointerType(obj->types[LLVM_TLAB], 0),
#endif
            obj->types[LLVM_INTPTR],
#ifdef LLVM_HAS_OPAQUE_POINTERS
            obj->types[LLVM_PTR],
#else
            LLVMPointerType(obj->types[LLVM_ANCHOR], 0),
#endif
         };
         obj->fntypes[which] = LLVMFunctionType(obj->types[LLVM_PTR], args,
                                                ARRAY_LEN(args), false);
         fn = llvm_add_fn(obj, "tlab_alloc", obj->fntypes[which]);
         llvm_add_func_attr(obj, fn, FUNC_ATTR_NOUNWIND, -1);
      }
      break;

   default:
      fatal_trace("cannot generate prototype for function %d", which);
   }

   return (obj->fns[which] = fn);
}

static LLVMValueRef llvm_call_fn(llvm_obj_t *obj, llvm_fn_t which,
                                 LLVMValueRef *args, unsigned count)
{
   LLVMValueRef fn = llvm_get_fn(obj, which);
   return LLVMBuildCall2(obj->builder, obj->fntypes[which], fn,
                         args, count, "");
}

static LLVMValueRef llvm_const_string(llvm_obj_t *obj, const char *str)
{
   const unsigned off = pack_writer_get_string(obj->pack_writer, str);

   LLVMValueRef indexes[] = {
      llvm_int32(obj, 0),
      llvm_int32(obj, off)
   };
   return LLVMBuildGEP2(obj->builder, obj->types[LLVM_STRTAB], obj->strtab,
                        indexes, ARRAY_LEN(indexes), "string");
}

#if ENABLE_DWARF
static void llvm_add_module_flag(llvm_obj_t *obj, const char *key, int value)
{
   LLVMAddModuleFlag(obj->module, LLVMModuleFlagBehaviorWarning,
                     key, strlen(key),
                     LLVMValueAsMetadata(llvm_int32(obj, value)));
}
#endif

static bool llvm_is_ptr(LLVMValueRef value)
{
   return LLVMGetTypeKind(LLVMTypeOf(value)) == LLVMPointerTypeKind;
}

#ifdef DEBUG
static void llvm_set_value_name(LLVMValueRef value, const char *name)
{
   size_t curlen;
   LLVMGetValueName2(value, &curlen);
   if (curlen == 0)
      LLVMSetValueName(value, name);
}

__attribute__((unused))
static void llvm_dump_value(const char *tag, LLVMValueRef value)
{
   fprintf(stderr, "%s: ", tag);
   fflush(stderr);
   LLVMDumpValue(value);
   fprintf(stderr, "\n");
}
#endif

////////////////////////////////////////////////////////////////////////////////
// JIT IR to LLVM lowering

static const char *cgen_reg_name(jit_reg_t reg)
{
#ifdef DEBUG
   static volatile int uniq = 0;
   static __thread char buf[32];
   checked_sprintf(buf, sizeof(buf), "R%d.%d", reg, relaxed_add(&uniq, 1));
   return buf;
#else
   return "";
#endif
}

static const char *cgen_arg_name(int nth)
{
#ifdef DEBUG
   static volatile int uniq = 0;
   static __thread char buf[32];
   checked_sprintf(buf, sizeof(buf), "A%d.%d", nth, relaxed_add(&uniq, 1));
   return buf;
#else
   return "";
#endif
}

__attribute__((noreturn))
static void cgen_abort(cgen_block_t *cgb, jit_ir_t *ir, const char *fmt, ...)
{
   va_list ap;
   va_start(ap, fmt);

   char *text LOCAL = xvasprintf(fmt, ap);
   jit_dump_with_mark(cgb->func->source, ir - cgb->func->source->irbuf, false);
   fatal_trace("%s", text);

   va_end(ap);
}

static LLVMRealPredicate cgen_real_pred(cgen_block_t *cgb, jit_ir_t *ir)
{
   switch (ir->cc) {
   case JIT_CC_EQ: return LLVMRealUEQ;
   case JIT_CC_NE: return LLVMRealUNE;
   case JIT_CC_GT: return LLVMRealUGT;
   case JIT_CC_LT: return LLVMRealULT;
   case JIT_CC_LE: return LLVMRealULE;
   case JIT_CC_GE: return LLVMRealUGE;
   default:
      cgen_abort(cgb, ir, "unhandled fcmp condition code");
   }
}

static LLVMIntPredicate cgen_int_pred(cgen_block_t *cgb, jit_ir_t *ir)
{
   switch (ir->cc) {
   case JIT_CC_EQ: return LLVMIntEQ;
   case JIT_CC_NE: return LLVMIntNE;
   case JIT_CC_GT: return LLVMIntSGT;
   case JIT_CC_LT: return LLVMIntSLT;
   case JIT_CC_LE: return LLVMIntSLE;
   case JIT_CC_GE: return LLVMIntSGE;
   default:
      cgen_abort(cgb, ir, "unhandled cmp condition code");
   }
}

static cgen_reloc_t *cgen_find_reloc(cgen_reloc_t *list, reloc_kind_t kind,
                                     int limit, uintptr_t key)
{
   for (int i = 0; i < limit; i++) {
      if (list[i].kind == kind && list[i].key == key)
         return &(list[i]);
      else if (list[i].kind == RELOC_NULL)
         break;
   }

   return NULL;
}

static LLVMValueRef cgen_load_from_reloc(llvm_obj_t *obj, cgen_func_t *func,
                                         reloc_kind_t kind, uintptr_t key)
{
   cgen_reloc_t *reloc = cgen_find_reloc(func->relocs, kind, INT_MAX, key);
   assert(reloc != NULL);

   LLVMValueRef array = LLVMBuildStructGEP2(obj->builder, func->descr_type,
                                            func->descr, 4, "");

   LLVMValueRef indexes[] = {
      llvm_intptr(obj, 0),
      llvm_intptr(obj, reloc->nth)
   };
   LLVMValueRef elem =
      LLVMBuildInBoundsGEP2(obj->builder, func->reloc_type, array,
                            indexes, ARRAY_LEN(indexes), "");
   LLVMValueRef ptr =
      LLVMBuildStructGEP2(obj->builder, obj->types[LLVM_AOT_RELOC],
                          elem, 1, "");

   return LLVMBuildLoad2(obj->builder, obj->types[LLVM_PTR], ptr, "");
}

static LLVMValueRef cgen_rematerialise_object(llvm_obj_t *obj,
                                              cgen_func_t *func,
                                              ident_t unit, ptrdiff_t offset)
{
   if (unit != func->source->module || offset < 0 || func->mode == CGEN_AOT) {
      // Locus refers to another module that may not be loaded or the
      // pointer is not stable
      LLVMValueRef unit_str;
      if (func->mode == CGEN_AOT) {
         LOCAL_TEXT_BUF tb = tb_new();
         tb_istr(tb, unit);

         object_fixup_locus(unit, &offset);

         unit_str = llvm_const_string(obj, tb_get(tb));
      }
      else
         unit_str = llvm_ptr(obj, (void *)istr(unit));

      LLVMValueRef args[] = {
         unit_str,
         llvm_intptr(obj, offset),
      };
      return llvm_call_fn(obj, LLVM_GET_OBJECT, args, ARRAY_LEN(args));
   }
   else {
      // Locus refers to the module containing the unit being compiled
      // which is guaranteed to be loaded
      return llvm_ptr(obj, object_from_locus(unit, offset, NULL));
   }
}

static LLVMValueRef cgen_rematerialise_handle(llvm_obj_t *obj,
                                              cgen_func_t *func,
                                              jit_handle_t handle)
{
   LLVMValueRef ptr =
      cgen_load_from_reloc(obj, func, RELOC_HANDLE, handle);
   LLVMValueRef intptr =
      LLVMBuildPtrToInt(obj->builder, ptr, obj->types[LLVM_INTPTR], "");

#ifdef DEBUG
   ident_t name = jit_get_name(func->source->jit, handle);
   char *valname LOCAL = xasprintf("%s.handle", istr(name));
#else
   const char *valname = "";
#endif

   return LLVMBuildTrunc(obj->builder, intptr, obj->types[LLVM_INT32], valname);
}

static LLVMValueRef cgen_get_value(llvm_obj_t *obj, cgen_block_t *cgb,
                                   jit_value_t value)
{
   switch (value.kind) {
   case JIT_VALUE_REG:
      assert(value.reg < cgb->func->source->nregs);
      assert(cgb->outregs[value.reg] != NULL);
      return cgb->outregs[value.reg];
   case JIT_VALUE_INT64:
      return llvm_int64(obj, value.int64);
   case JIT_VALUE_DOUBLE:
      return llvm_real(obj, value.dval);
   case JIT_ADDR_CPOOL:
      assert(value.int64 >= 0 && value.int64 <= cgb->func->source->cpoolsz);
      if (cgb->func->mode == CGEN_AOT) {
         LLVMValueRef indexes[] = {
            llvm_intptr(obj, 0),
            llvm_intptr(obj, value.int64)
         };
         return LLVMBuildInBoundsGEP2(obj->builder, cgb->func->cpool_type,
                                      cgb->func->cpool, indexes,
                                      ARRAY_LEN(indexes), "");
      }
      else
         return llvm_ptr(obj, cgb->func->source->cpool + value.int64);
   case JIT_ADDR_REG:
      {
         assert(value.reg < cgb->func->source->nregs);
         LLVMValueRef ptr = cgb->outregs[value.reg];

         if (value.disp == 0)
            return ptr;
         else if (llvm_is_ptr(ptr)) {
            LLVMValueRef indexes[] = { llvm_intptr(obj, value.disp) };
            return LLVMBuildGEP2(obj->builder,
                                 obj->types[LLVM_INT8],
                                 ptr, indexes, 1, "");
         }
         else {
            LLVMValueRef disp = llvm_int64(obj, value.disp);
            return LLVMBuildAdd(obj->builder, ptr, disp, "");
         }
      }
   case JIT_VALUE_EXIT:
      return llvm_int32(obj, value.exit);
   case JIT_VALUE_HANDLE:
      if (cgb->func->mode == CGEN_AOT && value.handle != JIT_HANDLE_INVALID)
         return cgen_rematerialise_handle(obj, cgb->func, value.handle);
      else
         return llvm_int32(obj, value.handle);
   case JIT_ADDR_ABS:
      return llvm_ptr(obj, (void *)(intptr_t)value.int64);
   case JIT_ADDR_COVER:
      if (cgb->func->mode == CGEN_AOT) {
         LLVMValueRef ptr =
            cgen_load_from_reloc(obj, cgb->func, RELOC_COVER, 0);
         LLVMValueRef base = LLVMBuildLoad2(obj->builder,
                                            obj->types[LLVM_PTR], ptr, "");
         LLVMValueRef indexes[] = {
            llvm_intptr(obj, value.int64)
         };
         return LLVMBuildGEP2(obj->builder, obj->types[LLVM_INT32],
                              base, indexes, 1, "");
      }
      else
         return llvm_ptr(obj, jit_get_cover_ptr(cgb->func->source->jit, value));
   case JIT_VALUE_FOREIGN:
      if (cgb->func->mode == CGEN_AOT)
         return cgen_load_from_reloc(obj, cgb->func, RELOC_FOREIGN,
                                     (uintptr_t)value.foreign);
      else
         return llvm_ptr(obj, value.foreign);
   case JIT_VALUE_LOCUS:
      return cgen_rematerialise_object(obj, cgb->func, value.ident, value.disp);
   default:
      fatal_trace("cannot handle value kind %d", value.kind);
   }
}

static LLVMValueRef cgen_coerce_value(llvm_obj_t *obj, cgen_block_t *cgb,
                                      jit_value_t value, llvm_type_t type)
{
   LLVMValueRef raw = cgen_get_value(obj, cgb, value);
   LLVMTypeRef lltype = LLVMTypeOf(raw);

   switch (type) {
   case LLVM_PTR:
      if (LLVMGetTypeKind(lltype) == LLVMIntegerTypeKind)
         return LLVMBuildIntToPtr(obj->builder, raw, obj->types[LLVM_PTR], "");
      else
         return raw;

   case LLVM_INTPTR:
   case LLVM_INT64:
   case LLVM_INT32:
   case LLVM_INT16:
   case LLVM_INT8:
   case LLVM_INT1:
      switch (LLVMGetTypeKind(lltype)) {
      case LLVMPointerTypeKind:
         return LLVMBuildIntToPtr(obj->builder, raw, obj->types[LLVM_PTR], "");
      case LLVMIntegerTypeKind:
         {
            const int bits1 = LLVMGetIntTypeWidth(lltype);
            const int bits2 = LLVMGetIntTypeWidth(obj->types[type]);

            if (bits2 == 1) {
               LLVMValueRef zero = LLVMConstInt(lltype, 0, false);
               return LLVMBuildICmp(obj->builder, LLVMIntNE, raw, zero, "");
            }
            else if (bits1 < bits2)
               return LLVMBuildSExt(obj->builder, raw, obj->types[type], "");
            else if (bits1 == bits2)
               return raw;
            else
               return LLVMBuildTrunc(obj->builder, raw, obj->types[type], "");
         }
      case LLVMDoubleTypeKind:
         return LLVMBuildBitCast(obj->builder, raw, obj->types[type], "");
      default:
         LLVMDumpType(lltype);
         fatal_trace("cannot coerce type to integer");
      }
      break;

   case LLVM_DOUBLE:
      return LLVMBuildBitCast(obj->builder, raw, obj->types[type], "");

   default:
      return raw;
   }
}

static void cgen_pointer_result(llvm_obj_t *obj, cgen_block_t *cgb,
                                jit_ir_t *ir, LLVMValueRef value)
{
   assert(llvm_is_ptr(value));

   if (mask_test(&cgb->func->ptr_mask, ir->result)) {
      DEBUG_ONLY(llvm_set_value_name(value, cgen_reg_name(ir->result)));
      cgb->outregs[ir->result] = PTR(value);
   }
   else
      cgb->outregs[ir->result] = LLVMBuildPtrToInt(obj->builder, value,
                                                   obj->types[LLVM_INT64],
                                                   cgen_reg_name(ir->result));
}

static void cgen_sext_result(llvm_obj_t *obj, cgen_block_t *cgb, jit_ir_t *ir,
                             LLVMValueRef value)
{
   LLVMTypeRef type = LLVMTypeOf(value);
   switch (LLVMGetTypeKind(type)) {
   case LLVMIntegerTypeKind:
      if (LLVMGetIntTypeWidth(type) == 64) {
         DEBUG_ONLY(llvm_set_value_name(value, cgen_reg_name(ir->result)));
         cgb->outregs[ir->result] = value;
      }
      else
         cgb->outregs[ir->result] = LLVMBuildSExt(obj->builder, value,
                                                  obj->types[LLVM_INT64],
                                                  cgen_reg_name(ir->result));
      break;

   case LLVMDoubleTypeKind:
      cgb->outregs[ir->result] = LLVMBuildBitCast(obj->builder, value,
                                                  obj->types[LLVM_INT64],
                                                  cgen_reg_name(ir->result));
      break;

   case LLVMPointerTypeKind:
      cgen_pointer_result(obj, cgb, ir, value);
      break;

   default:
      LLVMDumpType(type);
      fatal_trace("unhandled LLVM type kind in cgen_sext_result");
   }
}

static void cgen_zext_result(llvm_obj_t *obj, cgen_block_t *cgb, jit_ir_t *ir,
                             LLVMValueRef value)
{
   LLVMTypeRef type = LLVMTypeOf(value);
   switch (LLVMGetTypeKind(type)) {
   case LLVMIntegerTypeKind:
      if (LLVMGetIntTypeWidth(type) == 64) {
         DEBUG_ONLY(llvm_set_value_name(value, cgen_reg_name(ir->result)));
         cgb->outregs[ir->result] = value;
      }
      else
         cgb->outregs[ir->result] = LLVMBuildZExt(obj->builder, value,
                                                  obj->types[LLVM_INT64],
                                                  cgen_reg_name(ir->result));
      break;

   default:
      LLVMDumpType(type);
      fatal_trace("unhandled LLVM type kind in cgen_sext_result");
   }
}

static void cgen_sync_irpos(llvm_obj_t *obj, cgen_block_t *cgb, jit_ir_t *ir)
{
   const unsigned irpos = ir - cgb->func->source->irbuf;
   LLVMBuildStore(obj->builder, llvm_int32(obj, irpos), cgb->func->irpos);
}

#if ENABLE_DWARF
static void cgen_debug_loc(llvm_obj_t *obj, cgen_func_t *func, const loc_t *loc)
{
   if (loc_eq(loc, &(func->last_loc)))
      return;

   LLVMMetadataRef dloc = LLVMDIBuilderCreateDebugLocation(
      obj->context, loc->first_line, loc->first_column,
      func->debugmd, NULL);

#ifdef LLVM_HAVE_SET_CURRENT_DEBUG_LOCATION_2
   LLVMSetCurrentDebugLocation2(obj->builder, dloc);
#else
   LLVMValueRef md = LLVMMetadataAsValue(obj->context, dloc);
   LLVMSetCurrentDebugLocation(obj->builder, md);
#endif
}
#endif

static void cgen_op_recv(llvm_obj_t *obj, cgen_block_t *cgb, jit_ir_t *ir)
{
   assert(ir->arg1.kind == JIT_VALUE_INT64);
   const int nth = ir->arg1.int64;

   LLVMValueRef ptr;
   if (nth < ARGCACHE_SIZE)
      ptr = cgb->func->argcache[nth];
   else {
      assert(nth < JIT_MAX_ARGS);
      LLVMValueRef indexes[] = { llvm_intptr(obj, nth) };
      ptr = LLVMBuildInBoundsGEP2(obj->builder, obj->types[LLVM_INT64],
                                  cgb->func->args, indexes, ARRAY_LEN(indexes),
                                  cgen_arg_name(nth));
   }

   llvm_type_t type =
      mask_test(&cgb->func->ptr_mask, ir->result) ? LLVM_PTR : LLVM_INT64;

#ifdef LLVM_HAS_OPAQUE_POINTERS
   LLVMValueRef cast = ptr;
#else
   LLVMTypeRef ptr_type = LLVMPointerType(obj->types[type], 0);
   LLVMValueRef cast =
      LLVMBuildPointerCast(obj->builder, ptr, ptr_type, "");
#endif

   cgb->outregs[ir->result] = LLVMBuildLoad2(obj->builder, obj->types[type],
                                             cast, cgen_reg_name(ir->result));
   LLVMSetAlignment(cgb->outregs[ir->result], sizeof(int64_t));
}

static void cgen_op_send(llvm_obj_t *obj, cgen_block_t *cgb, jit_ir_t *ir)
{
   assert(ir->arg1.kind == JIT_VALUE_INT64);
   const int nth = ir->arg1.int64;

   LLVMValueRef value = cgen_get_value(obj, cgb, ir->arg2);

   LLVMValueRef ptr;
   if (nth < ARGCACHE_SIZE)
      ptr = cgb->func->argcache[nth];
   else {
      assert(nth < JIT_MAX_ARGS);
      LLVMValueRef indexes[] = { llvm_int32(obj, nth) };
      LLVMTypeRef int64_type = obj->types[LLVM_INT64];
#ifdef LLVM_HAS_OPAQUE_POINTERS
      LLVMValueRef args_cast = cgb->func->args;
#else
      LLVMTypeRef args_ptr_type = LLVMPointerType(int64_type, 0);
      LLVMValueRef args_cast =
         LLVMBuildPointerCast(obj->builder, cgb->func->args, args_ptr_type, "");
#endif
      ptr = LLVMBuildInBoundsGEP2(obj->builder, int64_type,
                                  args_cast, indexes, ARRAY_LEN(indexes),
                                  cgen_arg_name(nth));
   }

#ifdef LLVM_HAS_OPAQUE_POINTERS
   LLVMValueRef store = LLVMBuildStore(obj->builder, value, ptr);
#else
   LLVMTypeRef ptr_type = LLVMPointerType(LLVMTypeOf(value), 0);
   LLVMValueRef cast = LLVMBuildPointerCast(obj->builder, ptr, ptr_type, "");
   LLVMValueRef store = LLVMBuildStore(obj->builder, value, cast);
#endif

   LLVMSetAlignment(store, sizeof(int64_t));
}

static void cgen_op_store(llvm_obj_t *obj, cgen_block_t *cgb, jit_ir_t *ir)
{
   llvm_type_t type   = LLVM_INT8 + ir->size;
   LLVMValueRef value = cgen_coerce_value(obj, cgb, ir->arg1, type);
   LLVMValueRef ptr   = cgen_coerce_value(obj, cgb, ir->arg2, LLVM_PTR);

#ifdef LLVM_HAS_OPAQUE_POINTERS
   LLVMValueRef store = LLVMBuildStore(obj->builder, value, ptr);
#else
   LLVMTypeRef ptr_type = LLVMPointerType(LLVMTypeOf(value), 0);
   LLVMValueRef cast = LLVMBuildPointerCast(obj->builder, ptr, ptr_type, "");
   LLVMValueRef store = LLVMBuildStore(obj->builder, value, cast);
#endif

   LLVMSetAlignment(store, 1 << ir->size);
}

static void cgen_op_load(llvm_obj_t *obj, cgen_block_t *cgb, jit_ir_t *ir)
{
   LLVMValueRef ptr = cgen_coerce_value(obj, cgb, ir->arg1, LLVM_PTR);

   llvm_type_t type = mask_test(&cgb->func->ptr_mask, ir->result)
      ? LLVM_PTR : LLVM_INT8 + ir->size;

#ifndef LLVM_HAS_OPAQUE_POINTERS
   LLVMTypeRef ptr_type = LLVMPointerType(obj->types[type], 0);
   ptr = LLVMBuildPointerCast(obj->builder, ptr, ptr_type, "");
#endif

   if (type == LLVM_INT64) {
      LLVMValueRef value = LLVMBuildLoad2(obj->builder, obj->types[type],
                                          ptr, cgen_reg_name(ir->result));
      LLVMSetAlignment(value, 1 << ir->size);
      cgb->outregs[ir->result] = value;
   }
   else {
      LLVMValueRef tmp =
         LLVMBuildLoad2(obj->builder, obj->types[type], ptr, "");
      LLVMSetAlignment(tmp, 1 << ir->size);
      if (ir->op == J_ULOAD)
         cgen_zext_result(obj, cgb, ir, tmp);
      else
         cgen_sext_result(obj, cgb, ir, tmp);
   }
}

static void cgen_op_add(llvm_obj_t *obj, cgen_block_t *cgb, jit_ir_t *ir)
{
   llvm_fn_t fn = LLVM_LAST_FN;
   if (ir->cc == JIT_CC_O)
      fn = LLVM_ADD_OVERFLOW_S8 + ir->size;
   else if (ir->cc == JIT_CC_C)
      fn = LLVM_ADD_OVERFLOW_U8 + ir->size;

   if (fn != LLVM_LAST_FN) {
      llvm_type_t type = LLVM_INT8 + ir->size;
      LLVMValueRef arg1 = cgen_coerce_value(obj, cgb, ir->arg1, type);
      LLVMValueRef arg2 = cgen_coerce_value(obj, cgb, ir->arg2, type);

      LLVMValueRef args[] = { arg1, arg2 };
      LLVMValueRef pair = llvm_call_fn(obj, fn, args, 2);

      LLVMValueRef result = LLVMBuildExtractValue(obj->builder, pair, 0, "");
      cgb->outflags = LLVMBuildExtractValue(obj->builder, pair, 1, "FLAGS");

      if (ir->cc == JIT_CC_C)
         cgen_zext_result(obj, cgb, ir, result);
      else
         cgen_sext_result(obj, cgb, ir, result);
   }
   else {
      LLVMValueRef arg1 = cgen_get_value(obj, cgb, ir->arg1);
      LLVMValueRef arg2 = cgen_get_value(obj, cgb, ir->arg2);

      if (llvm_is_ptr(arg1)) {
         LLVMValueRef indexes[] = { arg2 };
         LLVMValueRef ptr = LLVMBuildGEP2(obj->builder, obj->types[LLVM_INT8],
                                          arg1, indexes, 1, "");
         cgen_pointer_result(obj, cgb, ir, ptr);
      }
      else if (mask_test(&cgb->func->ptr_mask, ir->result)) {
         LLVMValueRef base =
            LLVMBuildIntToPtr(obj->builder, arg1, obj->types[LLVM_PTR], "");
         LLVMValueRef indexes[] = { arg2 };
         LLVMValueRef ptr = LLVMBuildGEP2(obj->builder, obj->types[LLVM_INT8],
                                          base, indexes, 1, "");
         cgen_pointer_result(obj, cgb, ir, ptr);
      }
      else
         cgb->outregs[ir->result] = LLVMBuildAdd(obj->builder, arg1, arg2,
                                                 cgen_reg_name(ir->result));
   }
}

static void cgen_op_sub(llvm_obj_t *obj, cgen_block_t *cgb, jit_ir_t *ir)
{
   llvm_fn_t fn = LLVM_LAST_FN;
   if (ir->cc == JIT_CC_O)
      fn = LLVM_SUB_OVERFLOW_S8 + ir->size;
   else if (ir->cc == JIT_CC_C)
      fn = LLVM_SUB_OVERFLOW_U8 + ir->size;

   if (fn != LLVM_LAST_FN) {
      llvm_type_t type = LLVM_INT8 + ir->size;
      LLVMValueRef arg1 = cgen_coerce_value(obj, cgb, ir->arg1, type);
      LLVMValueRef arg2 = cgen_coerce_value(obj, cgb, ir->arg2, type);

      LLVMValueRef args[] = { arg1, arg2 };
      LLVMValueRef pair = llvm_call_fn(obj, fn, args, 2);

      LLVMValueRef result = LLVMBuildExtractValue(obj->builder, pair, 0, "");
      cgb->outflags = LLVMBuildExtractValue(obj->builder, pair, 1, "FLAGS");

      if (ir->cc == JIT_CC_C)
         cgen_zext_result(obj, cgb, ir, result);
      else
         cgen_sext_result(obj, cgb, ir, result);
   }
   else {
      LLVMValueRef arg1 = cgen_get_value(obj, cgb, ir->arg1);
      LLVMValueRef arg2 = cgen_get_value(obj, cgb, ir->arg2);
      cgb->outregs[ir->result] = LLVMBuildSub(obj->builder, arg1, arg2,
                                              cgen_reg_name(ir->result));
   }
}

static void cgen_op_mul(llvm_obj_t *obj, cgen_block_t *cgb, jit_ir_t *ir)
{
   llvm_fn_t fn = LLVM_LAST_FN;
   if (ir->cc == JIT_CC_O)
      fn = LLVM_MUL_OVERFLOW_S8 + ir->size;
   else if (ir->cc == JIT_CC_C)
      fn = LLVM_MUL_OVERFLOW_U8 + ir->size;

   if (fn != LLVM_LAST_FN) {
      llvm_type_t type = LLVM_INT8 + ir->size;
      LLVMValueRef arg1 = cgen_coerce_value(obj, cgb, ir->arg1, type);
      LLVMValueRef arg2 = cgen_coerce_value(obj, cgb, ir->arg2, type);

      LLVMValueRef args[] = { arg1, arg2 };
      LLVMValueRef pair = llvm_call_fn(obj, fn, args, 2);

      LLVMValueRef result = LLVMBuildExtractValue(obj->builder, pair, 0, "");
      cgb->outflags = LLVMBuildExtractValue(obj->builder, pair, 1, "FLAGS");

      if (ir->cc == JIT_CC_C)
         cgen_zext_result(obj, cgb, ir, result);
      else
         cgen_sext_result(obj, cgb, ir, result);
   }
   else {
      LLVMValueRef arg1 = cgen_get_value(obj, cgb, ir->arg1);
      LLVMValueRef arg2 = cgen_get_value(obj, cgb, ir->arg2);
      cgb->outregs[ir->result] = LLVMBuildMul(obj->builder, arg1, arg2,
                                              cgen_reg_name(ir->result));
   }
}

static void cgen_op_div(llvm_obj_t *obj, cgen_block_t *cgb, jit_ir_t *ir)
{
   LLVMValueRef arg1 = cgen_get_value(obj, cgb, ir->arg1);
   LLVMValueRef arg2 = cgen_get_value(obj, cgb, ir->arg2);

   cgb->outregs[ir->result] = LLVMBuildSDiv(obj->builder, arg1, arg2,
                                            cgen_reg_name(ir->result));
}

static void cgen_op_rem(llvm_obj_t *obj, cgen_block_t *cgb, jit_ir_t *ir)
{
   LLVMValueRef arg1 = cgen_get_value(obj, cgb, ir->arg1);
   LLVMValueRef arg2 = cgen_get_value(obj, cgb, ir->arg2);

   cgb->outregs[ir->result] = LLVMBuildSRem(obj->builder, arg1, arg2,
                                            cgen_reg_name(ir->result));
}

static void cgen_op_shl(llvm_obj_t *obj, cgen_block_t *cgb, jit_ir_t *ir)
{
   LLVMValueRef arg1 = cgen_get_value(obj, cgb, ir->arg1);
   LLVMValueRef arg2 = cgen_get_value(obj, cgb, ir->arg2);

   cgb->outregs[ir->result] = LLVMBuildShl(obj->builder, arg1, arg2,
                                           cgen_reg_name(ir->result));
}

static void cgen_op_asr(llvm_obj_t *obj, cgen_block_t *cgb, jit_ir_t *ir)
{
   LLVMValueRef arg1 = cgen_get_value(obj, cgb, ir->arg1);
   LLVMValueRef arg2 = cgen_get_value(obj, cgb, ir->arg2);

   cgb->outregs[ir->result] = LLVMBuildAShr(obj->builder, arg1, arg2,
                                            cgen_reg_name(ir->result));
}

static void cgen_op_fadd(llvm_obj_t *obj, cgen_block_t *cgb, jit_ir_t *ir)
{
   LLVMValueRef arg1 = cgen_coerce_value(obj, cgb, ir->arg1, LLVM_DOUBLE);
   LLVMValueRef arg2 = cgen_coerce_value(obj, cgb, ir->arg2, LLVM_DOUBLE);

   LLVMValueRef real = LLVMBuildFAdd(obj->builder, arg1, arg2, "");
   cgen_sext_result(obj, cgb, ir, real);
}

static void cgen_op_fsub(llvm_obj_t *obj, cgen_block_t *cgb, jit_ir_t *ir)
{
   LLVMValueRef arg1 = cgen_coerce_value(obj, cgb, ir->arg1, LLVM_DOUBLE);
   LLVMValueRef arg2 = cgen_coerce_value(obj, cgb, ir->arg2, LLVM_DOUBLE);

   LLVMValueRef real = LLVMBuildFSub(obj->builder, arg1, arg2, "");
   cgen_sext_result(obj, cgb, ir, real);
}

static void cgen_op_fmul(llvm_obj_t *obj, cgen_block_t *cgb, jit_ir_t *ir)
{
   LLVMValueRef arg1 = cgen_coerce_value(obj, cgb, ir->arg1, LLVM_DOUBLE);
   LLVMValueRef arg2 = cgen_coerce_value(obj, cgb, ir->arg2, LLVM_DOUBLE);

   LLVMValueRef real = LLVMBuildFMul(obj->builder, arg1, arg2, "");
   cgen_sext_result(obj, cgb, ir, real);
}

static void cgen_op_fdiv(llvm_obj_t *obj, cgen_block_t *cgb, jit_ir_t *ir)
{
   LLVMValueRef arg1 = cgen_coerce_value(obj, cgb, ir->arg1, LLVM_DOUBLE);
   LLVMValueRef arg2 = cgen_coerce_value(obj, cgb, ir->arg2, LLVM_DOUBLE);

   LLVMValueRef real = LLVMBuildFDiv(obj->builder, arg1, arg2, "");
   cgen_sext_result(obj, cgb, ir, real);
}

static void cgen_op_fneg(llvm_obj_t *obj, cgen_block_t *cgb, jit_ir_t *ir)
{
   LLVMValueRef arg1 = cgen_coerce_value(obj, cgb, ir->arg1, LLVM_DOUBLE);

   LLVMValueRef real = LLVMBuildFNeg(obj->builder, arg1, "");
   cgen_sext_result(obj, cgb, ir, real);
}

static void cgen_op_fcvtns(llvm_obj_t *obj, cgen_block_t *cgb, jit_ir_t *ir)
{
   LLVMValueRef arg1 = cgen_coerce_value(obj, cgb, ir->arg1, LLVM_DOUBLE);

   LLVMValueRef args[] = { llvm_real(obj, 0.5), arg1 };
   LLVMValueRef half = llvm_call_fn(obj, LLVM_COPYSIGN_F64, args, 2);
   LLVMValueRef rounded = LLVMBuildFAdd(obj->builder, arg1, half, "");

   cgb->outregs[ir->result] = LLVMBuildFPToSI(obj->builder, rounded,
                                              obj->types[LLVM_INT64],
                                              cgen_reg_name(ir->result));
}

static void cgen_op_scvtf(llvm_obj_t *obj, cgen_block_t *cgb, jit_ir_t *ir)
{
   LLVMValueRef arg1 = cgen_get_value(obj, cgb, ir->arg1);

   LLVMValueRef real = LLVMBuildSIToFP(obj->builder, arg1,
                                       obj->types[LLVM_DOUBLE], "");
   cgen_sext_result(obj, cgb, ir, real);
}

static void cgen_op_not(llvm_obj_t *obj, cgen_block_t *cgb, jit_ir_t *ir)
{
   LLVMValueRef arg1 = cgen_coerce_value(obj, cgb, ir->arg1, LLVM_INT1);
   LLVMValueRef logical = LLVMBuildNot(obj->builder, arg1, "");
   cgen_zext_result(obj, cgb, ir, logical);
}

static void cgen_op_and(llvm_obj_t *obj, cgen_block_t *cgb, jit_ir_t *ir)
{
   LLVMValueRef arg1 = cgen_get_value(obj, cgb, ir->arg1);
   LLVMValueRef arg2 = cgen_get_value(obj, cgb, ir->arg2);

   LLVMValueRef logical = LLVMBuildAnd(obj->builder, arg1, arg2, "");
   cgen_zext_result(obj, cgb, ir, logical);
}

static void cgen_op_or(llvm_obj_t *obj, cgen_block_t *cgb, jit_ir_t *ir)
{
   LLVMValueRef arg1 = cgen_get_value(obj, cgb, ir->arg1);
   LLVMValueRef arg2 = cgen_get_value(obj, cgb, ir->arg2);

   LLVMValueRef logical = LLVMBuildOr(obj->builder, arg1, arg2, "");
   cgen_zext_result(obj, cgb, ir, logical);
}

static void cgen_op_xor(llvm_obj_t *obj, cgen_block_t *cgb, jit_ir_t *ir)
{
   LLVMValueRef arg1 = cgen_get_value(obj, cgb, ir->arg1);
   LLVMValueRef arg2 = cgen_get_value(obj, cgb, ir->arg2);

   LLVMValueRef logical = LLVMBuildXor(obj->builder, arg1, arg2, "");
   cgen_zext_result(obj, cgb, ir, logical);
}

static void cgen_op_ret(llvm_obj_t *obj, jit_ir_t *ir)
{
   LLVMBuildRetVoid(obj->builder);
}

static void cgen_op_jump(llvm_obj_t *obj, cgen_block_t *cgb, jit_ir_t *ir)
{
   if (ir->cc == JIT_CC_NONE) {
      assert(cgb->source->out.count == 1);
      LLVMBasicBlockRef dest =
         cgb->func->blocks[jit_get_edge(&(cgb->source->out), 0)].bbref;
      LLVMBuildBr(obj->builder, dest);
   }
   else if (ir->cc == JIT_CC_T) {
      assert(cgb->source->out.count == 2);
      LLVMBasicBlockRef dest_t =
         cgb->func->blocks[jit_get_edge(&(cgb->source->out), 1)].bbref;
      LLVMBasicBlockRef dest_f = (cgb + 1)->bbref;
      LLVMBuildCondBr(obj->builder, cgb->outflags, dest_t, dest_f);
   }
   else if (ir->cc == JIT_CC_F) {
      assert(cgb->source->out.count == 2);
      LLVMBasicBlockRef dest_t =
         cgb->func->blocks[jit_get_edge(&(cgb->source->out), 1)].bbref;
      LLVMBasicBlockRef dest_f = (cgb + 1)->bbref;
      LLVMBuildCondBr(obj->builder, cgb->outflags, dest_f, dest_t);
   }
   else
      cgen_abort(cgb, ir, "unhandled jump condition code");
}

static void cgen_op_cmp(llvm_obj_t *obj, cgen_block_t *cgb, jit_ir_t *ir)
{
   LLVMValueRef arg1 = cgen_get_value(obj, cgb, ir->arg1);
   LLVMValueRef arg2 = cgen_get_value(obj, cgb, ir->arg2);

   const bool arg1_ptr = llvm_is_ptr(arg1);
   const bool arg2_ptr = llvm_is_ptr(arg2);

   if (arg1_ptr && !arg2_ptr)
      arg2 = LLVMBuildIntToPtr(obj->builder, arg2, obj->types[LLVM_PTR], "");
   else if (!arg1_ptr && arg2_ptr)
      arg1 = LLVMBuildIntToPtr(obj->builder, arg1, obj->types[LLVM_PTR], "");

   LLVMIntPredicate pred = cgen_int_pred(cgb, ir);
   cgb->outflags = LLVMBuildICmp(obj->builder, pred, arg1, arg2, "FLAGS");
}

static void cgen_op_ccmp(llvm_obj_t *obj, cgen_block_t *cgb, jit_ir_t *ir)
{
   LLVMValueRef arg1 = cgen_get_value(obj, cgb, ir->arg1);
   LLVMValueRef arg2 = cgen_get_value(obj, cgb, ir->arg2);

   const bool arg1_ptr = llvm_is_ptr(arg1);
   const bool arg2_ptr = llvm_is_ptr(arg2);

   if (arg1_ptr && !arg2_ptr)
      arg2 = LLVMBuildIntToPtr(obj->builder, arg2, obj->types[LLVM_PTR], "");
   else if (!arg1_ptr && arg2_ptr)
      arg1 = LLVMBuildIntToPtr(obj->builder, arg1, obj->types[LLVM_PTR], "");

   LLVMIntPredicate pred = cgen_int_pred(cgb, ir);
   LLVMValueRef cmp = LLVMBuildICmp(obj->builder, pred, arg1, arg2, "");

   cgb->outflags = LLVMBuildAnd(obj->builder, cgb->outflags, cmp, "FLAGS");
}

static void cgen_op_fcmp(llvm_obj_t *obj, cgen_block_t *cgb, jit_ir_t *ir)
{
   LLVMValueRef arg1 = cgen_coerce_value(obj, cgb, ir->arg1, LLVM_DOUBLE);
   LLVMValueRef arg2 = cgen_coerce_value(obj, cgb, ir->arg2, LLVM_DOUBLE);

   LLVMRealPredicate pred = cgen_real_pred(cgb, ir);
   cgb->outflags = LLVMBuildFCmp(obj->builder, pred, arg1, arg2, "FLAGS");
}

static void cgen_op_fccmp(llvm_obj_t *obj, cgen_block_t *cgb, jit_ir_t *ir)
{
   LLVMValueRef arg1 = cgen_coerce_value(obj, cgb, ir->arg1, LLVM_DOUBLE);
   LLVMValueRef arg2 = cgen_coerce_value(obj, cgb, ir->arg2, LLVM_DOUBLE);

   LLVMRealPredicate pred = cgen_real_pred(cgb, ir);
   LLVMValueRef cmp = LLVMBuildFCmp(obj->builder, pred, arg1, arg2, "");

   cgb->outflags = LLVMBuildAnd(obj->builder, cgb->outflags, cmp, "FLAGS");
}

static void cgen_op_cset(llvm_obj_t *obj, cgen_block_t *cgb, jit_ir_t *ir)
{
   cgen_zext_result(obj, cgb, ir, cgb->outflags);
}

static void cgen_op_csel(llvm_obj_t *obj, cgen_block_t *cgb, jit_ir_t *ir)
{
   LLVMValueRef arg1 = cgen_get_value(obj, cgb, ir->arg1);
   LLVMValueRef arg2 = cgen_get_value(obj, cgb, ir->arg2);

   LLVMValueRef result =
      LLVMBuildSelect(obj->builder, cgb->outflags, arg1, arg2, "");

   cgen_sext_result(obj, cgb, ir, result);
}

static void cgen_op_call(llvm_obj_t *obj, cgen_block_t *cgb, jit_ir_t *ir)
{
   cgen_sync_irpos(obj, cgb, ir);

   jit_func_t *callee = jit_get_func(cgb->func->source->jit, ir->arg1.handle);

   LLVMValueRef entry = NULL, fptr = NULL;
   if (cgb->func->mode == CGEN_AOT) {
      cgen_reloc_t *reloc = cgen_find_reloc(cgb->func->relocs, RELOC_FUNC,
                                            INT_MAX, ir->arg1.handle);
      assert(reloc != NULL);

      LLVMValueRef array =
         LLVMBuildStructGEP2(obj->builder, cgb->func->descr_type,
                             cgb->func->descr, 4, "");

      LLVMValueRef indexes[] = {
         llvm_intptr(obj, 0),
         llvm_intptr(obj, reloc->nth)
      };
      LLVMValueRef elem =
         LLVMBuildInBoundsGEP2(obj->builder, cgb->func->reloc_type, array,
                               indexes, ARRAY_LEN(indexes), "");
      LLVMValueRef ptr =
         LLVMBuildStructGEP2(obj->builder, obj->types[LLVM_AOT_RELOC],
                             elem, 1, "");

      fptr = LLVMBuildLoad2(obj->builder, obj->types[LLVM_PTR], ptr, "");

#if CLOSED_WORLD
      // Do not generate direct calls for intrinsics
      if (callee->entry == jit_interp) {
         LOCAL_TEXT_BUF symbol = safe_symbol(callee->name);
         entry = llvm_add_fn(obj, tb_get(symbol), obj->types[LLVM_ENTRY_FN]);
      }
#endif
   }
   else
      fptr = llvm_ptr(obj, callee);

   if (entry == NULL) {
      // Must have acquire semantics to synchronise with installing new code
      entry = LLVMBuildLoad2(obj->builder, obj->types[LLVM_PTR], fptr, "entry");
      LLVMSetAlignment(entry, sizeof(void *));
      LLVMSetOrdering(entry, LLVMAtomicOrderingAcquire);
   }

#ifndef LLVM_HAS_OPAQUE_POINTERS
   LLVMTypeRef ptr_type = LLVMPointerType(obj->types[LLVM_ENTRY_FN], 0);
   entry = LLVMBuildPointerCast(obj->builder, entry, ptr_type, "");
#endif

   LLVMValueRef args[] = {
      fptr,
      PTR(cgb->func->anchor),
      cgb->func->args,
      cgb->func->tlab,
   };
   LLVMBuildCall2(obj->builder, obj->types[LLVM_ENTRY_FN], entry,
                  args, ARRAY_LEN(args), "");
}

static void cgen_op_lea(llvm_obj_t *obj, cgen_block_t *cgb, jit_ir_t *ir)
{
   LLVMValueRef ptr = cgen_get_value(obj, cgb, ir->arg1);

   if (llvm_is_ptr(ptr))
      cgen_pointer_result(obj, cgb, ir, ptr);
   else
      cgen_zext_result(obj, cgb, ir, ptr);
}

static void cgen_op_mov(llvm_obj_t *obj, cgen_block_t *cgb, jit_ir_t *ir)
{
   LLVMValueRef value = cgen_get_value(obj, cgb, ir->arg1);
   cgen_sext_result(obj, cgb, ir, value);
}

static void cgen_op_neg(llvm_obj_t *obj, cgen_block_t *cgb, jit_ir_t *ir)
{
   LLVMValueRef arg1 = cgen_get_value(obj, cgb, ir->arg1);
   LLVMValueRef neg =
      LLVMBuildNeg(obj->builder, arg1, cgen_reg_name(ir->result));

   cgb->outregs[ir->result] = neg;
}

static void cgen_op_clamp(llvm_obj_t *obj, cgen_block_t *cgb, jit_ir_t *ir)
{
   LLVMValueRef arg1 = cgen_get_value(obj, cgb, ir->arg1);
   LLVMValueRef zero = llvm_int64(obj, 0);
   LLVMValueRef isneg = LLVMBuildICmp(obj->builder, LLVMIntSLT, arg1, zero, "");
   LLVMValueRef clamp = LLVMBuildSelect(obj->builder, isneg, zero, arg1, "");

   cgb->outregs[ir->result] = clamp;
}

static void cgen_macro_exp(llvm_obj_t *obj, cgen_block_t *cgb, jit_ir_t *ir)
{
   llvm_fn_t fn = LLVM_EXP_OVERFLOW_S64;
   if (ir->cc == JIT_CC_O)
      fn = LLVM_EXP_OVERFLOW_S8 + ir->size;
   else if (ir->cc == JIT_CC_C)
      fn = LLVM_EXP_OVERFLOW_U8 + ir->size;

   llvm_type_t type = LLVM_INT8 + ir->size;
   LLVMValueRef arg1 = cgen_coerce_value(obj, cgb, ir->arg1, type);
   LLVMValueRef arg2 = cgen_coerce_value(obj, cgb, ir->arg2, type);

   LLVMValueRef args[] = { arg1, arg2 };
   LLVMValueRef pair = llvm_call_fn(obj, fn, args, 2);

   LLVMValueRef result = LLVMBuildExtractValue(obj->builder, pair, 0, "");
   cgb->outflags = LLVMBuildExtractValue(obj->builder, pair, 1, "FLAGS");

   if (ir->cc == JIT_CC_C)
      cgen_zext_result(obj, cgb, ir, result);
   else
      cgen_sext_result(obj, cgb, ir, result);
}

static void cgen_macro_fexp(llvm_obj_t *obj, cgen_block_t *cgb, jit_ir_t *ir)
{
   LLVMValueRef arg1 = cgen_coerce_value(obj, cgb, ir->arg1, LLVM_DOUBLE);
   LLVMValueRef arg2 = cgen_coerce_value(obj, cgb, ir->arg2, LLVM_DOUBLE);

   LLVMValueRef args[] = { arg1, arg2 };
   LLVMValueRef real = llvm_call_fn(obj, LLVM_POW_F64, args, 2);

   cgen_sext_result(obj, cgb, ir, real);
}

static void cgen_macro_copy(llvm_obj_t *obj, cgen_block_t *cgb, jit_ir_t *ir)
{
   LLVMValueRef count = cgb->outregs[ir->result];
   LLVMValueRef dest  = cgen_coerce_value(obj, cgb, ir->arg1, LLVM_PTR);
   LLVMValueRef src   = cgen_coerce_value(obj, cgb, ir->arg2, LLVM_PTR);

#ifdef LLVM_HAS_OPAQUE_POINTERS
   if (LLVMIsConstant(count) && LLVMConstIntGetZExtValue(count) <= 64) {
      LLVMValueRef args[] = {
         dest,
         src,
         count,
         llvm_int1(obj, 0),
      };
      llvm_call_fn(obj, LLVM_MEMCPY_INLINE, args, ARRAY_LEN(args));
   }
   else
#endif
      LLVMBuildMemCpy(obj->builder, dest, 0, src, 0, count);
}

static void cgen_macro_move(llvm_obj_t *obj, cgen_block_t *cgb, jit_ir_t *ir)
{
   LLVMValueRef count = cgb->outregs[ir->result];
   LLVMValueRef dest  = cgen_coerce_value(obj, cgb, ir->arg1, LLVM_PTR);
   LLVMValueRef src   = cgen_coerce_value(obj, cgb, ir->arg2, LLVM_PTR);

   LLVMBuildMemMove(obj->builder, dest, 0, src, 0, count);
}

static void cgen_macro_bzero(llvm_obj_t *obj, cgen_block_t *cgb, jit_ir_t *ir)
{
   LLVMValueRef count = cgb->outregs[ir->result];
   LLVMValueRef dest  = cgen_coerce_value(obj, cgb, ir->arg1, LLVM_PTR);

#ifdef LLVM_HAS_OPAQUE_POINTERS
   if (LLVMIsConstant(count) && LLVMConstIntGetZExtValue(count) <= 64) {
      LLVMValueRef args[] = {
         dest,
         llvm_int8(obj, 0),
         count,
         llvm_int1(obj, 0),
      };
      llvm_call_fn(obj, LLVM_MEMSET_INLINE, args, ARRAY_LEN(args));
   }
   else
#endif
      LLVMBuildMemSet(obj->builder, PTR(dest), llvm_int8(obj, 0), count, 0);
}

static void cgen_macro_memset(llvm_obj_t *obj, cgen_block_t *cgb, jit_ir_t *ir)
{
   LLVMValueRef count = cgb->outregs[ir->result];
   LLVMValueRef dest  = cgen_coerce_value(obj, cgb, ir->arg1, LLVM_PTR);
   LLVMValueRef value = cgen_coerce_value(obj, cgb, ir->arg2,
                                          LLVM_INT8 + ir->size);

   if (ir->size == JIT_SZ_8) {
#ifdef LLVM_HAS_OPAQUE_POINTERSS
      if (LLVMIsConstant(count) && LLVMConstIntGetZExtValue(count) <= 64) {
         LLVMValueRef args[] = {
            dest,
            value,
            count,
            llvm_int1(obj, 0),
         };
         llvm_call_fn(obj, LLVM_MEMSET_INLINE, args, ARRAY_LEN(args));
      }
      else
#endif
         LLVMBuildMemSet(obj->builder, PTR(dest), value, count, 0);
   }
   else {
      LLVMValueRef args[] = { dest, value, count, };
      llvm_call_fn(obj, LLVM_MEMSET_U8 + ir->size, args, ARRAY_LEN(args));
   }
}

static void cgen_macro_exit(llvm_obj_t *obj, cgen_block_t *cgb, jit_ir_t *ir)
{
   cgen_sync_irpos(obj, cgb, ir);

   switch (ir->arg1.exit) {
   case JIT_EXIT_SCHED_WAVEFORM:
      {
         LLVMValueRef args[] = {
            PTR(cgb->func->anchor),
            cgb->func->args,
            cgb->func->tlab,
         };
         llvm_call_fn(obj, LLVM_SCHED_WAVEFORM, args, ARRAY_LEN(args));
      }
      break;

   case JIT_EXIT_TEST_EVENT:
      {
         LLVMValueRef args[] = {
            PTR(cgb->func->anchor),
            cgb->func->args,
            cgb->func->tlab,
         };
         llvm_call_fn(obj, LLVM_TEST_EVENT, args, ARRAY_LEN(args));
      }
      break;

   default:
      {
         LLVMValueRef which = cgen_get_value(obj, cgb, ir->arg1);

         LLVMValueRef args[] = {
            which,
            PTR(cgb->func->anchor),
            cgb->func->args,
            cgb->func->tlab,
         };
         llvm_call_fn(obj, LLVM_DO_EXIT, args, ARRAY_LEN(args));
      }
      break;
   }
}

static void cgen_macro_fficall(llvm_obj_t *obj, cgen_block_t *cgb, jit_ir_t *ir)
{
   cgen_sync_irpos(obj, cgb, ir);

   LLVMValueRef ffptr = cgen_get_value(obj, cgb, ir->arg1);

   LLVMValueRef args[] = {
      ffptr,
      PTR(cgb->func->anchor),
      cgb->func->args
   };
   llvm_call_fn(obj, LLVM_DO_FFICALL, args, ARRAY_LEN(args));
}

static void cgen_macro_galloc(llvm_obj_t *obj, cgen_block_t *cgb, jit_ir_t *ir)
{
   cgen_sync_irpos(obj, cgb, ir);

   LLVMValueRef size = cgen_get_value(obj, cgb, ir->arg1);

   LLVMValueRef args[] = {
      LLVMBuildTrunc(obj->builder, size, obj->types[LLVM_INTPTR], ""),
      cgb->func->anchor
   };
   LLVMValueRef ptr = llvm_call_fn(obj, LLVM_MSPACE_ALLOC, args,
                                   ARRAY_LEN(args));

   cgen_pointer_result(obj, cgb, ir, ptr);
}

static void cgen_macro_lalloc(llvm_obj_t *obj, cgen_block_t *cgb, jit_ir_t *ir)
{
   cgen_sync_irpos(obj, cgb, ir);

   LLVMValueRef size = cgen_get_value(obj, cgb, ir->arg1);

   LLVMValueRef args[] = {
      cgb->func->tlab,
      LLVMBuildTrunc(obj->builder, size, obj->types[LLVM_INTPTR], ""),
      cgb->func->anchor
   };
   LLVMValueRef ptr = llvm_call_fn(obj, LLVM_TLAB_ALLOC, args,
                                   ARRAY_LEN(args));

   cgen_pointer_result(obj, cgb, ir, ptr);
}

static void cgen_macro_salloc(llvm_obj_t *obj, cgen_block_t *cgb, jit_ir_t *ir)
{
   LLVMBasicBlockRef old_bb = LLVMGetInsertBlock(obj->builder);
   LLVMBasicBlockRef first_bb = LLVMGetFirstBasicBlock(cgb->func->llvmfn);
   LLVMPositionBuilderAtEnd(obj->builder, first_bb);

   assert(ir->arg2.kind == JIT_VALUE_INT64);

   LLVMValueRef ptr;
   if (ir->arg2.int64 <= 8)
      ptr = LLVMBuildAlloca(obj->builder, obj->types[LLVM_INT64], "");
   else
      ptr = LLVMBuildArrayAlloca(obj->builder, obj->types[LLVM_INT8],
                                 llvm_intptr(obj, ir->arg2.int64), "");

   LLVMPositionBuilderAtEnd(obj->builder, old_bb);

   cgen_pointer_result(obj, cgb, ir, ptr);
}

static void cgen_macro_getpriv(llvm_obj_t *obj, cgen_block_t *cgb, jit_ir_t *ir)
{
   LLVMValueRef ptrptr;
   if (cgb->func->mode == CGEN_JIT) {
      jit_func_t *f = jit_get_func(cgb->func->source->jit, ir->arg1.handle);
      ptrptr = llvm_ptr(obj, jit_get_privdata_ptr(f->jit, f));
   }
   else
      ptrptr = cgen_load_from_reloc(obj, cgb->func, RELOC_PRIVDATA,
                                    ir->arg1.handle);

#ifndef LLVM_HAS_OPAQUE_POINTERS
   LLVMTypeRef ptr_type = LLVMPointerType(obj->types[LLVM_PTR], 0);
   ptrptr = LLVMBuildPointerCast(obj->builder, ptrptr, ptr_type, "");
#endif

   LLVMValueRef ptr =
      LLVMBuildLoad2(obj->builder, obj->types[LLVM_PTR], ptrptr, "p2");
   LLVMSetAlignment(ptr, sizeof(void *));
   LLVMSetOrdering(ptr, LLVMAtomicOrderingAcquire);

   cgen_pointer_result(obj, cgb, ir, ptr);
}

static void cgen_macro_putpriv(llvm_obj_t *obj, cgen_block_t *cgb, jit_ir_t *ir)
{
   LLVMValueRef value = cgen_coerce_value(obj, cgb, ir->arg2, LLVM_PTR);

   if (cgb->func->mode == CGEN_AOT) {
      LLVMValueRef ptr = cgen_load_from_reloc(obj, cgb->func, RELOC_PRIVDATA,
                                              ir->arg1.handle);
#ifndef LLVM_HAS_OPAQUE_POINTERS
      LLVMTypeRef ptr_type = LLVMPointerType(obj->types[LLVM_PTR], 0);
      ptr = LLVMBuildPointerCast(obj->builder, ptr, ptr_type, "");
#endif

      LLVMValueRef store = LLVMBuildStore(obj->builder, value, ptr);
      LLVMSetAlignment(store, sizeof(void *));
      LLVMSetOrdering(store, LLVMAtomicOrderingRelease);
   }
   else {
      LLVMValueRef args[] = {
         cgen_get_value(obj, cgb, ir->arg1),
         value,
      };
      llvm_call_fn(obj, LLVM_PUTPRIV, args, ARRAY_LEN(args));
   }
}

static void cgen_macro_case(llvm_obj_t *obj, cgen_block_t *cgb, jit_ir_t *ir)
{
   jit_ir_t *first = cgb->func->source->irbuf + cgb->source->first;
   if (ir > first && (ir - 1)->op == MACRO_CASE)
      return;    // Combined into previous $CASE

   LLVMBasicBlockRef elsebb = (cgb + 1)->bbref;

   LLVMValueRef test = cgb->outregs[ir->result];
   assert(test != NULL);

   jit_ir_t *last = cgb->func->source->irbuf + cgb->source->last;

   const int numcases = last - ir + 1;
   assert(cgb->source->out.count == numcases + 1);

   LLVMValueRef stmt = LLVMBuildSwitch(obj->builder, test, elsebb, numcases);

   for (int nth = 0; nth < numcases; ir++, nth++) {
      assert(ir->op == MACRO_CASE);

      LLVMValueRef onval = cgen_get_value(obj, cgb, ir->arg1);
      LLVMBasicBlockRef dest =
         cgb->func->blocks[jit_get_edge(&(cgb->source->out), nth + 1)].bbref;

      LLVMAddCase(stmt, onval, dest);
   }
}

static void cgen_macro_trim(llvm_obj_t *obj, cgen_block_t *cgb, jit_ir_t *ir)
{
   LLVMValueRef watermark_ptr = LLVMBuildStructGEP2(obj->builder,
                                                    obj->types[LLVM_ANCHOR],
                                                    cgb->func->anchor, 3, "");
   LLVMValueRef watermark = LLVMBuildLoad2(obj->builder,
                                           obj->types[LLVM_INT32],
                                           watermark_ptr, "");
   LLVMValueRef alloc_ptr = LLVMBuildStructGEP2(obj->builder,
                                                obj->types[LLVM_TLAB],
                                                cgb->func->tlab, 2, "");
   LLVMBuildStore(obj->builder, watermark, alloc_ptr);
}

static void cgen_ir(llvm_obj_t *obj, cgen_block_t *cgb, jit_ir_t *ir)
{
   switch (ir->op) {
   case J_RECV:
      cgen_op_recv(obj, cgb, ir);
      break;
   case J_SEND:
      cgen_op_send(obj, cgb, ir);
      break;
   case J_STORE:
      cgen_op_store(obj, cgb, ir);
      break;
   case J_LOAD:
   case J_ULOAD:
      cgen_op_load(obj, cgb, ir);
      break;
   case J_ADD:
      cgen_op_add(obj, cgb, ir);
      break;
   case J_SUB:
      cgen_op_sub(obj, cgb, ir);
      break;
   case J_MUL:
      cgen_op_mul(obj, cgb, ir);
      break;
   case J_DIV:
      cgen_op_div(obj, cgb, ir);
      break;
   case J_REM:
      cgen_op_rem(obj, cgb, ir);
      break;
   case J_SHL:
      cgen_op_shl(obj, cgb, ir);
      break;
   case J_ASR:
      cgen_op_asr(obj, cgb, ir);
      break;
   case J_FADD:
      cgen_op_fadd(obj, cgb, ir);
      break;
   case J_FSUB:
      cgen_op_fsub(obj, cgb, ir);
      break;
   case J_FMUL:
      cgen_op_fmul(obj, cgb, ir);
      break;
   case J_FDIV:
      cgen_op_fdiv(obj, cgb, ir);
      break;
   case J_FNEG:
      cgen_op_fneg(obj, cgb, ir);
      break;
   case J_FCVTNS:
      cgen_op_fcvtns(obj, cgb, ir);
      break;
   case J_SCVTF:
      cgen_op_scvtf(obj, cgb, ir);
      break;
   case J_NOT:
      cgen_op_not(obj, cgb, ir);
      break;
   case J_AND:
      cgen_op_and(obj, cgb, ir);
      break;
   case J_OR:
      cgen_op_or(obj, cgb, ir);
      break;
   case J_XOR:
      cgen_op_xor(obj, cgb, ir);
      break;
   case J_RET:
      cgen_op_ret(obj, ir);
      break;
   case J_JUMP:
      cgen_op_jump(obj, cgb, ir);
      break;
   case J_CMP:
      cgen_op_cmp(obj, cgb, ir);
      break;
   case J_CCMP:
      cgen_op_ccmp(obj, cgb, ir);
      break;
   case J_FCMP:
      cgen_op_fcmp(obj, cgb, ir);
      break;
   case J_FCCMP:
      cgen_op_fccmp(obj, cgb, ir);
      break;
   case J_CSET:
      cgen_op_cset(obj, cgb, ir);
      break;
   case J_CSEL:
      cgen_op_csel(obj, cgb, ir);
      break;
   case J_CALL:
      cgen_op_call(obj, cgb, ir);
      break;
   case J_LEA:
      cgen_op_lea(obj, cgb, ir);
      break;
   case J_MOV:
      cgen_op_mov(obj, cgb, ir);
      break;
   case J_NEG:
      cgen_op_neg(obj, cgb, ir);
      break;
   case J_CLAMP:
      cgen_op_clamp(obj, cgb, ir);
      break;
   case J_DEBUG:
      DWARF_ONLY(cgen_debug_loc(obj, cgb->func, &(ir->arg1.loc)));
      break;
   case J_TRAP:
   case J_NOP:
      break;
   case MACRO_EXP:
      cgen_macro_exp(obj, cgb, ir);
      break;
   case MACRO_FEXP:
      cgen_macro_fexp(obj, cgb, ir);
      break;
   case MACRO_COPY:
      cgen_macro_copy(obj, cgb, ir);
      break;
   case MACRO_MOVE:
      cgen_macro_move(obj, cgb, ir);
      break;
   case MACRO_BZERO:
      cgen_macro_bzero(obj, cgb, ir);
      break;
   case MACRO_MEMSET:
      cgen_macro_memset(obj, cgb, ir);
      break;
   case MACRO_EXIT:
      cgen_macro_exit(obj, cgb, ir);
      break;
   case MACRO_FFICALL:
      cgen_macro_fficall(obj, cgb, ir);
      break;
   case MACRO_GALLOC:
      cgen_macro_galloc(obj, cgb, ir);
      break;
   case MACRO_LALLOC:
      cgen_macro_lalloc(obj, cgb, ir);
      break;
   case MACRO_SALLOC:
      cgen_macro_salloc(obj, cgb, ir);
      break;
   case MACRO_GETPRIV:
      cgen_macro_getpriv(obj, cgb, ir);
      break;
   case MACRO_PUTPRIV:
      cgen_macro_putpriv(obj, cgb, ir);
      break;
   case MACRO_CASE:
      cgen_macro_case(obj, cgb, ir);
      break;
   case MACRO_TRIM:
      cgen_macro_trim(obj, cgb, ir);
      break;
   default:
      cgen_abort(cgb, ir, "cannot generate LLVM for %s", jit_op_name(ir->op));
   }
}

static void cgen_basic_blocks(llvm_obj_t *obj, cgen_func_t *func,
                              jit_cfg_t *cfg)
{
   func->blocks = xcalloc_array(cfg->nblocks, sizeof(cgen_block_t));

   for (int i = 0; i < cfg->nblocks; i++) {
#ifdef DEBUG
      char name[32];
      checked_sprintf(name, sizeof(name), "BB%d", i);
#else
      const char *name = "";
#endif

      cgen_block_t *cgb = &(func->blocks[i]);
      cgb->bbref  = llvm_append_block(obj, func->llvmfn, name);
      cgb->source = &(cfg->blocks[i]);
      cgb->func   = func;

      cgb->inregs  = xcalloc_array(func->source->nregs, sizeof(LLVMValueRef));
      cgb->outregs = xcalloc_array(func->source->nregs, sizeof(LLVMValueRef));
   }
}

static void cgen_frame_anchor(llvm_obj_t *obj, cgen_func_t *func)
{
   LLVMTypeRef type = obj->types[LLVM_ANCHOR];
   func->anchor = LLVMBuildAlloca(obj->builder, type, "anchor");

   LLVMValueRef func_arg = LLVMGetParam(func->llvmfn, 0);
   LLVMSetValueName(func_arg, "func");

   LLVMValueRef caller_arg = LLVMGetParam(func->llvmfn, 1);
   LLVMSetValueName(caller_arg, "caller");

   LLVMValueRef caller_ptr = LLVMBuildStructGEP2(obj->builder, type,
                                                 func->anchor, 0, "");
   LLVMBuildStore(obj->builder, caller_arg, caller_ptr);

   LLVMValueRef func_ptr = LLVMBuildStructGEP2(obj->builder, type,
                                               func->anchor, 1, "");
   LLVMBuildStore(obj->builder, func_arg, func_ptr);

   LLVMValueRef irpos_ptr = LLVMBuildStructGEP2(obj->builder, type,
                                                func->anchor, 2, "");
   LLVMBuildStore(obj->builder, llvm_int32(obj, 0), irpos_ptr);

   LLVMValueRef watermark_ptr = LLVMBuildStructGEP2(obj->builder, type,
                                                    func->anchor, 3, "");
   LLVMValueRef alloc_ptr = LLVMBuildStructGEP2(obj->builder,
                                                obj->types[LLVM_TLAB],
                                                func->tlab, 2, "");
   LLVMValueRef alloc = LLVMBuildLoad2(obj->builder, obj->types[LLVM_INT32],
                                       alloc_ptr, "");
   LLVMBuildStore(obj->builder, alloc, watermark_ptr);
}

static void cgen_aot_cpool(llvm_obj_t *obj, cgen_func_t *func)
{
   jit_func_t *f = func->source;

   LOCAL_TEXT_BUF tb = tb_new();
   tb_istr(tb, f->name);
   tb_cat(tb, ".cpool");

   LLVMTypeRef array_type = LLVMArrayType(obj->types[LLVM_INT8], f->cpoolsz);

   LLVMValueRef global = LLVMAddGlobal(obj->module, array_type, tb_get(tb));
   LLVMSetLinkage(global, LLVMPrivateLinkage);
   LLVMSetGlobalConstant(global, true);
   LLVMSetUnnamedAddr(global, true);

   LLVMValueRef *data LOCAL = xmalloc_array(f->cpoolsz, sizeof(LLVMValueRef));
   for (int i = 0; i < f->cpoolsz; i++)
      data[i] = llvm_int8(obj, f->cpool[i]);

   LLVMValueRef init =
      LLVMConstArray(obj->types[LLVM_INT8], data, f->cpoolsz);
   LLVMSetInitializer(global, init);

   func->cpool = global;
   func->cpool_type = array_type;
}

static LLVMValueRef cgen_debug_irbuf(llvm_obj_t *obj, jit_func_t *f)
{
   LOCAL_TEXT_BUF tb = tb_new();
   tb_istr(tb, f->name);
   tb_cat(tb, ".debug");

   size_t size = 0;
   uint8_t *buf LOCAL = NULL;
   pack_writer_emit(obj->pack_writer, f->jit, f->handle, &buf, &size);

   LLVMValueRef *data LOCAL = xmalloc_array(size, sizeof(LLVMValueRef));
   for (size_t i = 0; i < size; i++)
      data[i] = llvm_int8(obj, buf[i]);

   LLVMTypeRef array_type = LLVMArrayType(obj->types[LLVM_INT8], size);

   LLVMValueRef global = LLVMAddGlobal(obj->module, array_type, tb_get(tb));
   LLVMSetLinkage(global, LLVMPrivateLinkage);
   LLVMSetGlobalConstant(global, true);
   LLVMSetUnnamedAddr(global, true);

   LLVMValueRef init = LLVMConstArray(obj->types[LLVM_INT8], data, size);
   LLVMSetInitializer(global, init);

   return global;
}

static LLVMValueRef cgen_reloc_str(llvm_obj_t *obj, const char *str)
{
   const unsigned off = pack_writer_get_string(obj->pack_writer, str);
   return llvm_intptr(obj, off);
}

static void cgen_aot_descr(llvm_obj_t *obj, cgen_func_t *func)
{
   LLVMValueRef irbuf = cgen_debug_irbuf(obj, func->source);

   A(cgen_reloc_t) relocs = AINIT;

   for (int i = 0; i < func->source->nirs; i++) {
      jit_ir_t *ir = &(func->source->irbuf[i]);
      if (ir->op == J_CALL || ir->op == MACRO_GETPRIV
          || ir->op == MACRO_PUTPRIV) {
         // Special handling of JIT_VALUE_HANDLE arguments
         reloc_kind_t kind = ir->op == J_CALL ? RELOC_FUNC : RELOC_PRIVDATA;
         if (cgen_find_reloc(relocs.items, kind, relocs.count,
                             ir->arg1.handle) == NULL) {
            jit_func_t *f = jit_get_func(func->source->jit, ir->arg1.handle);
            const cgen_reloc_t r = {
               .kind = kind,
               .str  = cgen_reloc_str(obj, istr(f->name)),
               .key  = ir->arg1.handle,
               .nth  = relocs.count,
            };
            APUSH(relocs, r);
         }
      }
      else {
         jit_value_t args[2] = { ir->arg1, ir->arg2 };
         for (int j = 0; j < ARRAY_LEN(args); j++) {
            if (args[j].kind == JIT_VALUE_HANDLE
                && args[j].handle != JIT_HANDLE_INVALID) {
               if (cgen_find_reloc(relocs.items, RELOC_HANDLE, relocs.count,
                                   args[j].handle) == NULL) {
                  ident_t name = jit_get_name(func->source->jit,
                                              args[j].handle);

                  const cgen_reloc_t r = {
                     .kind = RELOC_HANDLE,
                     .str  = cgen_reloc_str(obj, istr(name)),
                     .key  = args[j].handle,
                     .nth  = relocs.count,
                  };
                  APUSH(relocs, r);
               }
            }
            else if (args[j].kind == JIT_VALUE_FOREIGN) {
               if (cgen_find_reloc(relocs.items, RELOC_FOREIGN, relocs.count,
                                   (uintptr_t)args[j].foreign) == NULL) {
                  // Encode spec in name string
                  LOCAL_TEXT_BUF tb = tb_new();
                  ffi_spec_t spec = ffi_get_spec(args[j].foreign);
                  if (spec.count > 0)
                     tb_catn(tb, spec.embed, spec.count);
                  else
                     tb_cat(tb, spec.ext);
                  tb_append(tb, '\b');
                  tb_istr(tb, ffi_get_sym(args[j].foreign));

                  const cgen_reloc_t r = {
                     .kind = RELOC_FOREIGN,
                     .str  = cgen_reloc_str(obj, tb_get(tb)),
                     .key  = (uintptr_t)args[j].foreign,
                     .nth  = relocs.count,
                  };
                  APUSH(relocs, r);
               }
            }
            else if (args[j].kind == JIT_ADDR_COVER) {
               if (cgen_find_reloc(relocs.items, RELOC_COVER,
                                   relocs.count, 0) == NULL) {
                  const cgen_reloc_t r = {
                     .kind = RELOC_COVER,
                     .nth  = relocs.count,
                  };
                  APUSH(relocs, r);
               }
            }
         }
      }
   }

   APUSH(relocs, (cgen_reloc_t){ .kind = RELOC_NULL });
   func->relocs = relocs.items;

   func->reloc_type = LLVMArrayType(obj->types[LLVM_AOT_RELOC], relocs.count);

   LLVMValueRef *reloc_elems LOCAL =
      xmalloc_array(relocs.count, sizeof(LLVMValueRef));

   for (int i = 0; i < relocs.count; i++) {
      LLVMValueRef fields[] = {
         llvm_int32(obj, relocs.items[i].kind),
         relocs.items[i].str ?: llvm_intptr(obj, 0)
      };
      reloc_elems[i] = LLVMConstNamedStruct(obj->types[LLVM_AOT_RELOC],
                                            fields, ARRAY_LEN(fields));
   }

   LLVMValueRef reloc_array = LLVMConstArray(obj->types[LLVM_AOT_RELOC],
                                             reloc_elems, relocs.count);

   LLVMTypeRef ftypes[] = {
      obj->types[LLVM_PTR],     // Entry function
      obj->types[LLVM_PTR],     // String table
      obj->types[LLVM_PTR],     // JIT pack buffer
      obj->types[LLVM_PTR],     // Constant pool
      func->reloc_type          // Relocations list
   };
   func->descr_type = LLVMStructTypeInContext(obj->context, ftypes,
                                              ARRAY_LEN(ftypes), false);

   char *name LOCAL = xasprintf("%s.descr", func->name);
   func->descr = LLVMAddGlobal(obj->module, func->descr_type, name);
#ifdef IMPLIB_REQUIRED
   LLVMSetDLLStorageClass(func->descr, LLVMDLLExportStorageClass);
#endif

   LLVMValueRef fields[] = {
      PTR(func->llvmfn),
      PTR(obj->strtab),
      PTR(irbuf),
      PTR(func->cpool),
      reloc_array,
   };
   LLVMValueRef init = LLVMConstNamedStruct(func->descr_type,
                                            fields, ARRAY_LEN(fields));
   LLVMSetInitializer(func->descr, init);
}

#if ENABLE_DWARF
static LLVMMetadataRef cgen_debug_file(llvm_obj_t *obj, const loc_t *loc)
{
   const char *file_path = loc_file_str(loc) ?: "";

   char *basec LOCAL = xstrdup(file_path);
   char *dirc LOCAL = xstrdup(file_path);

   const char *file = basename(basec);
   const size_t file_len = strlen(file);

   const char *dir = dirname(dirc);
   const size_t dir_len = strlen(dir);

   return LLVMDIBuilderCreateFile(obj->debuginfo, file, file_len, dir, dir_len);
}
#endif

static void cgen_must_be_pointer(cgen_func_t *func, jit_value_t value)
{
   switch (value.kind) {
   case JIT_VALUE_REG:
   case JIT_ADDR_REG:
      mask_set(&(func->ptr_mask), value.reg);
      break;
   default:
      break;
   }
}

static void cgen_pointer_mask(cgen_func_t *func)
{
   mask_init(&(func->ptr_mask), func->source->nregs);

   for (int i = 0; i < func->source->nirs; i++) {
      jit_ir_t *ir = &(func->source->irbuf[i]);

      switch (ir->op) {
      case J_LOAD:
      case J_ULOAD:
      case MACRO_BZERO:
         cgen_must_be_pointer(func, ir->arg1);
         break;
      case J_STORE:
      case MACRO_PUTPRIV:
         cgen_must_be_pointer(func, ir->arg2);
         break;
      case MACRO_COPY:
      case MACRO_MOVE:
         cgen_must_be_pointer(func, ir->arg1);
         cgen_must_be_pointer(func, ir->arg2);
         break;
      case J_ADD:
      case J_MOV:
         // Propagate pointer argument to result
         if (ir->arg1.kind == JIT_VALUE_REG
             && mask_test(&(func->ptr_mask), ir->arg1.reg))
            mask_set(&(func->ptr_mask), ir->arg1.reg);
         break;
      case J_LEA:
      case MACRO_GETPRIV:
      case MACRO_LALLOC:
      case MACRO_SALLOC:
      case MACRO_GALLOC:
         mask_set(&(func->ptr_mask), ir->result);
         break;
      default:
         break;
      }
   }
}

static void cgen_cache_args(llvm_obj_t *obj, cgen_func_t *func)
{
   // Attempt to limit the number of getelementptr instructions
   // generated for the argument array

   for (int i = 0; i < ARGCACHE_SIZE; i++) {
      LLVMValueRef indexes[] = { llvm_int32(obj, i) };
      func->argcache[i] = LLVMBuildInBoundsGEP2(obj->builder,
                                                obj->types[LLVM_INT64],
                                                func->args, indexes,
                                                ARRAY_LEN(indexes),
                                                cgen_arg_name(i));
   }

   // Also cache the IR position pointer in the anchor
   func->irpos = LLVMBuildStructGEP2(obj->builder,
                                     obj->types[LLVM_ANCHOR],
                                     func->anchor, 2, "irpos");
}

static void cgen_fix_liveout_types(llvm_obj_t *obj, cgen_block_t *cgb)
{
   for (int j = 0; j < cgb->func->source->nregs; j++) {
      if (!mask_test(&cgb->source->liveout, j))
         continue;

      const bool want_ptr = mask_test(&cgb->func->ptr_mask, j);
      if (want_ptr && !llvm_is_ptr(cgb->outregs[j])) {
         LLVMValueRef last = LLVMGetBasicBlockTerminator(cgb->bbref);
         if (last != NULL)
            LLVMPositionBuilderBefore(obj->builder, last);

         cgb->outregs[j] = LLVMBuildIntToPtr(obj->builder,
                                             cgb->outregs[j],
                                             obj->types[LLVM_PTR],
                                             cgen_reg_name(j));
      }
   }
}

static void cgen_function(llvm_obj_t *obj, cgen_func_t *func)
{
   func->llvmfn = llvm_add_fn(obj, func->name, obj->types[LLVM_ENTRY_FN]);
   llvm_add_func_attr(obj, func->llvmfn, FUNC_ATTR_NOUNWIND, -1);
   llvm_add_func_attr(obj, func->llvmfn, FUNC_ATTR_UWTABLE, -1);
   llvm_add_func_attr(obj, func->llvmfn, FUNC_ATTR_DLLEXPORT, -1);
   llvm_add_func_attr(obj, func->llvmfn, FUNC_ATTR_READONLY, 1);
   llvm_add_func_attr(obj, func->llvmfn, FUNC_ATTR_NONNULL, 1);
   llvm_add_func_attr(obj, func->llvmfn, FUNC_ATTR_READONLY, 2);
   llvm_add_func_attr(obj, func->llvmfn, FUNC_ATTR_NOALIAS, 3);
   llvm_add_func_attr(obj, func->llvmfn, FUNC_ATTR_NONNULL, 3);
   llvm_add_func_attr(obj, func->llvmfn, FUNC_ATTR_NOALIAS, 4);

#ifdef PRESERVE_FRAME_POINTER
   llvm_add_func_attr(obj, func->llvmfn, FUNC_ATTR_PRESERVE_FP, -1);
#endif

#if ENABLE_DWARF
   LLVMMetadataRef file_ref =
      cgen_debug_file(obj, &(func->source->object->loc));

   if (obj->debugcu == NULL) {
      obj->debugcu = LLVMDIBuilderCreateCompileUnit(
         obj->debuginfo, LLVMDWARFSourceLanguageAda83,
         file_ref, PACKAGE, sizeof(PACKAGE) - 1,
         opt_get_int(OPT_OPTIMISE), "", 0,
         0, "", 0,
         LLVMDWARFEmissionFull, 0, false, false
#if LLVM_CREATE_CU_HAS_SYSROOT
         , "/", 1, "", 0
#endif
      );
   }

   LLVMMetadataRef dtype = LLVMDIBuilderCreateSubroutineType(
      obj->debuginfo, file_ref, NULL, 0, 0);
   const size_t namelen = strlen(func->name);

   func->debugmd = LLVMDIBuilderCreateFunction(
      obj->debuginfo, obj->debugcu, func->name, namelen,
      func->name, namelen, file_ref,
      func->source->object->loc.first_line, dtype, true, true,
      1, 0, opt_get_int(OPT_OPTIMISE));

   LLVMSetSubprogram(func->llvmfn, func->debugmd);

   cgen_debug_loc(obj, func, &(func->source->object->loc));
#endif  // ENABLE_DWARF

   if (func->mode == CGEN_AOT) {
      cgen_aot_cpool(obj, func);
      cgen_aot_descr(obj, func);
   }

   LLVMBasicBlockRef entry_bb = llvm_append_block(obj, func->llvmfn, "entry");
   LLVMPositionBuilderAtEnd(obj->builder, entry_bb);

   func->args = LLVMGetParam(func->llvmfn, 2);
   LLVMSetValueName(func->args, "args");

   func->tlab = LLVMGetParam(func->llvmfn, 3);
   LLVMSetValueName(func->tlab, "tlab");

   cgen_frame_anchor(obj, func);
   cgen_cache_args(obj, func);

   jit_cfg_t *cfg = func->cfg = jit_get_cfg(func->source);
   cgen_basic_blocks(obj, func, cfg);

   cgen_pointer_mask(func);

   cgen_block_t *cgb = func->blocks;

   LLVMValueRef zero_flag = llvm_int1(obj, false);

   int maxin = 0;
   for (int i = 0; i < func->source->nirs; i++) {
      if (i == cgb->source->first) {
         LLVMPositionBuilderAtEnd(obj->builder, cgb->bbref);

         cgen_block_t *dom = NULL;
         if (cgb->source->in.count == 1)
            dom = &(func->blocks[jit_get_edge(&cgb->source->in, 0)]);

         cgb->inflags = zero_flag;

         if (mask_test(&cgb->source->livein, func->source->nregs)) {
             if (dom != NULL && dom < cgb) {
                // Skip generating a phi instruction if there is just
                // one dominating block
                assert(dom->outflags != NULL);
                cgb->inflags = dom->outflags;
             }
             else if (cgb->source->in.count > 0) {
                LLVMTypeRef int1_type = obj->types[LLVM_INT1];
                cgb->inflags = LLVMBuildPhi(obj->builder, int1_type, "FLAGS");
             }
         }

         cgb->outflags = cgb->inflags;

         for (int j = 0; j < func->source->nregs; j++) {
            if (mask_test(&cgb->source->livein, j)) {
               if (dom != NULL && dom < cgb) {
                  assert(dom->outregs[j] != NULL);
                  cgb->inregs[j] = cgb->outregs[j] = dom->outregs[j];
                  continue;
               }

               llvm_type_t type =
                  mask_test(&func->ptr_mask, j) ? LLVM_PTR : LLVM_INT64;
               const char *name = cgen_reg_name(j);
               LLVMValueRef init = cgb->source->in.count == 0
                  ? LLVMConstNull(obj->types[type])
                  : LLVMBuildPhi(obj->builder, obj->types[type], name);
               cgb->inregs[j] = cgb->outregs[j] = init;
            }
         }

         maxin = MAX(maxin, cgb->source->in.count);
      }

      assert(i >= cgb->source->first && i <= cgb->source->last);

      cgen_ir(obj, cgb, &(func->source->irbuf[i]));

      if (i == cgb->source->last) {
         if (cgb->source->aborts)
            LLVMBuildUnreachable(obj->builder);

         cgen_fix_liveout_types(obj, cgb);

         if (LLVMGetBasicBlockTerminator(cgb->bbref) == NULL) {
            // Fall through to next block
            assert(!cgb->source->returns);
            assert(cgb + 1 < func->blocks + cfg->nblocks);
            LLVMBuildBr(obj->builder, (++cgb)->bbref);
         }
         else
            ++cgb;
      }
   }

   LLVMValueRef *phi_in LOCAL = xmalloc_array(maxin, sizeof(LLVMValueRef));
   LLVMBasicBlockRef *phi_bb LOCAL =
      xmalloc_array(maxin, sizeof(LLVMBasicBlockRef));

   for (int i = 0; i < cfg->nblocks; i++) {
      jit_block_t *bb = &(cfg->blocks[i]);
      cgen_block_t *cgb = &(func->blocks[i]);

      if (bb->in.count == 0)
         continue;

      // Flags
      if (mask_test(&cgb->source->livein, func->source->nregs)
          && LLVMIsAPHINode(cgb->inflags)
          && LLVMGetInstructionParent(cgb->inflags) == cgb->bbref) {

         for (int j = 0; j < bb->in.count; j++) {
            const int edge = jit_get_edge(&bb->in, j);
            phi_in[j] = func->blocks[edge].outflags;
            phi_bb[j] = func->blocks[edge].bbref;
         }
         LLVMAddIncoming(cgb->inflags, phi_in, phi_bb, bb->in.count);
      }

      // Live-in registers
      for (int j = 0; j < func->source->nregs; j++) {
         if (cgb->inregs[j] != NULL && LLVMIsAPHINode(cgb->inregs[j])
             && LLVMGetInstructionParent(cgb->inregs[j]) == cgb->bbref) {

            for (int k = 0; k < bb->in.count; k++) {
               const int edge = jit_get_edge(&bb->in, k);
               assert(func->blocks[edge].outregs[j] != NULL);
               phi_in[k] = func->blocks[edge].outregs[j];
               phi_bb[k] = func->blocks[edge].bbref;
            }
            LLVMAddIncoming(cgb->inregs[j], phi_in, phi_bb, bb->in.count);
         }
      }
   }

   for (int i = 0; i < cfg->nblocks; i++) {
      cgen_block_t *cgb = &(func->blocks[i]);
      free(cgb->inregs);
      free(cgb->outregs);
      cgb->inregs = cgb->outregs = NULL;
   }

   LLVMPositionBuilderAtEnd(obj->builder, entry_bb);
   LLVMBuildBr(obj->builder, func->blocks[0].bbref);

   jit_free_cfg(func->source);
   func->cfg = cfg = NULL;

   mask_free(&(func->ptr_mask));

   free(func->blocks);
   func->blocks = NULL;

   free(func->relocs);
   func->relocs = NULL;
}

static void cgen_tlab_alloc_body(llvm_obj_t *obj)
{
   LLVMValueRef fn = obj->fns[LLVM_TLAB_ALLOC];
   LLVMSetLinkage(fn, LLVMPrivateLinkage);

#ifdef PRESERVE_FRAME_POINTER
   llvm_add_func_attr(obj, fn, FUNC_ATTR_PRESERVE_FP, 0);
#endif

   LLVMBasicBlockRef entry = llvm_append_block(obj, fn, "");

   LLVMPositionBuilderAtEnd(obj->builder, entry);

   LLVMValueRef tlab = LLVMGetParam(fn, 0);
   LLVMSetValueName(tlab, "tlab");

   LLVMValueRef bytes = LLVMGetParam(fn, 1);
   LLVMSetValueName(bytes, "bytes");

   LLVMValueRef anchor = LLVMGetParam(fn, 2);
   LLVMSetValueName(anchor, "anchor");

   LLVMBasicBlockRef fast_bb = llvm_append_block(obj, fn, "");
   LLVMBasicBlockRef slow_bb = llvm_append_block(obj, fn, "");

   LLVMValueRef base_ptr =
      LLVMBuildStructGEP2(obj->builder, obj->types[LLVM_TLAB], tlab, 1, "");
   LLVMValueRef alloc_ptr =
      LLVMBuildStructGEP2(obj->builder, obj->types[LLVM_TLAB], tlab, 2, "");
   LLVMValueRef limit_ptr =
      LLVMBuildStructGEP2(obj->builder, obj->types[LLVM_TLAB], tlab, 3, "");

   LLVMValueRef alloc =
      LLVMBuildLoad2(obj->builder, obj->types[LLVM_INT32], alloc_ptr, "");
   LLVMValueRef limit =
      LLVMBuildLoad2(obj->builder, obj->types[LLVM_INT32], limit_ptr, "");

   LLVMValueRef bytes_trunc = LLVMBuildTrunc(obj->builder, bytes,
                                             obj->types[LLVM_INT32], "");

   LLVMValueRef align_mask = llvm_int32(obj, RT_ALIGN_MASK);
   LLVMValueRef align_up =
      LLVMBuildAnd(obj->builder,
                   LLVMBuildAdd(obj->builder, bytes_trunc, align_mask, ""),
                   LLVMConstNot(align_mask), "");

   LLVMValueRef next = LLVMBuildAdd(obj->builder, alloc, align_up, "");

   LLVMValueRef over = LLVMBuildICmp(obj->builder, LLVMIntUGT, next, limit, "");
   LLVMBuildCondBr(obj->builder, over, slow_bb, fast_bb);

   LLVMPositionBuilderAtEnd(obj->builder, fast_bb);

   LLVMBuildStore(obj->builder, next, alloc_ptr);

   LLVMValueRef base =
      LLVMBuildLoad2(obj->builder, obj->types[LLVM_PTR], base_ptr, "");

   LLVMValueRef indexes[] = { alloc };
   LLVMValueRef fast_ptr = LLVMBuildInBoundsGEP2(obj->builder,
                                                 obj->types[LLVM_INT8],
                                                 base, indexes,
                                                 ARRAY_LEN(indexes), "");

   LLVMBuildRet(obj->builder, fast_ptr);

   LLVMPositionBuilderAtEnd(obj->builder, slow_bb);

   LLVMValueRef args[] = { bytes, anchor };
   LLVMValueRef slow_ptr = llvm_call_fn(obj, LLVM_MSPACE_ALLOC, args,
                                        ARRAY_LEN(args));

   LLVMBuildRet(obj->builder, slow_ptr);
}

static void cgen_exp_overflow_body(llvm_obj_t *obj, llvm_fn_t which,
                                   jit_size_t sz, llvm_fn_t mulbase)
{
   LLVMValueRef fn = obj->fns[which];
   LLVMSetLinkage(fn, LLVMPrivateLinkage);

#ifdef PRESERVE_FRAME_POINTER
   llvm_add_func_attr(obj, fn, FUNC_ATTR_PRESERVE_FP, 0);
#endif

   LLVMBasicBlockRef entry = llvm_append_block(obj, fn, "entry");

   LLVMPositionBuilderAtEnd(obj->builder, entry);

   LLVMValueRef x0 = LLVMGetParam(fn, 0);
   LLVMSetValueName(x0, "x0");

   LLVMValueRef y0 = LLVMGetParam(fn, 1);
   LLVMSetValueName(y0, "y0");

   LLVMTypeRef int_type = obj->types[LLVM_INT8 + sz];
   LLVMTypeRef pair_type = obj->types[LLVM_PAIR_I8_I1 + sz];

   LLVMValueRef mulfn = llvm_get_fn(obj, mulbase + sz);
   LLVMTypeRef multype = obj->fntypes[mulbase + sz];

   LLVMValueRef zero = LLVMConstInt(int_type, 0, false);
   LLVMValueRef one = LLVMConstInt(int_type, 1, false);

   LLVMValueRef r0 = one;
   LLVMValueRef o0 = llvm_int1(obj, false);
   LLVMValueRef xo0 = llvm_int1(obj, false);

   LLVMBasicBlockRef header = llvm_append_block(obj, fn, "header");
   LLVMBasicBlockRef body = llvm_append_block(obj, fn, "body");
   LLVMBasicBlockRef accum = llvm_append_block(obj, fn, "accum");
   LLVMBasicBlockRef tail = llvm_append_block(obj, fn, "tail");
   LLVMBasicBlockRef exit = llvm_append_block(obj, fn, "exit");

   LLVMBuildBr(obj->builder, header);

   LLVMPositionBuilderAtEnd(obj->builder, header);

   LLVMValueRef x1 = LLVMBuildPhi(obj->builder, int_type, "x1");
   LLVMValueRef y1 = LLVMBuildPhi(obj->builder, int_type, "y1");
   LLVMValueRef r1 = LLVMBuildPhi(obj->builder, int_type, "r1");
   LLVMValueRef o1 = LLVMBuildPhi(obj->builder, obj->types[LLVM_INT1], "o1");
   LLVMValueRef xo1 = LLVMBuildPhi(obj->builder, obj->types[LLVM_INT1], "xo1");

   LLVMValueRef test1 = LLVMBuildICmp(obj->builder, LLVMIntEQ, y1, zero, "");
   LLVMBuildCondBr(obj->builder, test1, exit, body);

   LLVMPositionBuilderAtEnd(obj->builder, body);

   LLVMValueRef lsb = LLVMBuildAnd(obj->builder, y1, one, "");
   LLVMValueRef test2 = LLVMBuildICmp(obj->builder, LLVMIntEQ, lsb, zero, "");
   LLVMBuildCondBr(obj->builder, test2, tail, accum);

   LLVMPositionBuilderAtEnd(obj->builder, accum);

   LLVMValueRef r2_args[] = { r1, x1 };
   LLVMValueRef r2_pair = LLVMBuildCall2(obj->builder, multype, mulfn, r2_args,
                                         ARRAY_LEN(r2_args), "");
   LLVMValueRef r2 = LLVMBuildExtractValue(obj->builder, r2_pair, 0, "r2");
   LLVMValueRef r2_ovf = LLVMBuildExtractValue(obj->builder, r2_pair, 1, "");
   LLVMValueRef tmp = LLVMBuildOr(obj->builder, o1, xo1, "");
   LLVMValueRef o2 = LLVMBuildOr(obj->builder, tmp, r2_ovf, "o2");

   LLVMBuildBr(obj->builder, tail);

   LLVMPositionBuilderAtEnd(obj->builder, tail);

   LLVMValueRef r3 = LLVMBuildPhi(obj->builder, int_type, "r3");

   LLVMValueRef r3_in[] = { r1, r2 };
   LLVMBasicBlockRef r3_bb[] = { body, accum };
   LLVMAddIncoming(r3, r3_in, r3_bb, 2);

   LLVMValueRef o3 = LLVMBuildPhi(obj->builder, obj->types[LLVM_INT1], "o3");

   LLVMValueRef o3_in[] = { o1, o2 };
   LLVMBasicBlockRef o3_bb[] = { body, accum };
   LLVMAddIncoming(o3, o3_in, o3_bb, 2);

   LLVMValueRef r1_in[] = { r0, r3 };
   LLVMBasicBlockRef r1_bb[] = { entry, tail };
   LLVMAddIncoming(r1, r1_in, r1_bb, 2);

   LLVMValueRef y2 = LLVMBuildLShr(obj->builder, y1, one, "y2");

   LLVMValueRef x2_args[] = { x1, x1 };
   LLVMValueRef x2_pair = LLVMBuildCall2(obj->builder, multype, mulfn, x2_args,
                                         ARRAY_LEN(x2_args), "");
   LLVMValueRef x2 = LLVMBuildExtractValue(obj->builder, x2_pair, 0, "x2");
   LLVMValueRef xo2 = LLVMBuildExtractValue(obj->builder, x2_pair, 1, "");

   LLVMValueRef x1_in[] = { x0, x2 };
   LLVMBasicBlockRef x1_bb[] = { entry, tail };
   LLVMAddIncoming(x1, x1_in, x1_bb, 2);

   LLVMValueRef y1_in[] = { y0, y2 };
   LLVMBasicBlockRef y1_bb[] = { entry, tail };
   LLVMAddIncoming(y1, y1_in, y1_bb, 2);

   LLVMValueRef o1_in[] = { o0, o3 };
   LLVMBasicBlockRef o1_bb[] = { entry, tail };
   LLVMAddIncoming(o1, o1_in, o1_bb, 2);

   LLVMValueRef xo1_in[] = { xo0, xo2 };
   LLVMBasicBlockRef xo1_bb[] = { entry, tail };
   LLVMAddIncoming(xo1, xo1_in, xo1_bb, 2);

   LLVMBuildBr(obj->builder, header);

   LLVMPositionBuilderAtEnd(obj->builder, exit);

   LLVMValueRef pair = LLVMConstNull(pair_type);
   pair = LLVMBuildInsertValue(obj->builder, pair, r1, 0, "");
   pair = LLVMBuildInsertValue(obj->builder, pair, o1, 1, "");

   LLVMBuildRet(obj->builder, pair);
}

static void cgen_memset_body(llvm_obj_t *obj, llvm_fn_t which,
                             jit_size_t sz, llvm_fn_t mulbase)
{
   LLVMValueRef fn = obj->fns[which];
   LLVMSetLinkage(fn, LLVMPrivateLinkage);

#ifdef PRESERVE_FRAME_POINTER
   llvm_add_func_attr(obj, fn, FUNC_ATTR_PRESERVE_FP, 0);
#endif

   LLVMBasicBlockRef entry = llvm_append_block(obj, fn, "entry");

   LLVMPositionBuilderAtEnd(obj->builder, entry);

   LLVMValueRef dest = LLVMGetParam(fn, 0);
   LLVMSetValueName(dest, "dest");

   LLVMValueRef value = LLVMGetParam(fn, 1);
   LLVMSetValueName(value, "value");

   LLVMValueRef bytes = LLVMGetParam(fn, 2);
   LLVMSetValueName(bytes, "bytes");

   LLVMValueRef indexes1[] = { bytes };
   LLVMValueRef eptr = LLVMBuildGEP2(obj->builder, obj->types[LLVM_INT8],
                                     dest, indexes1, 1, "eptr");

   LLVMBasicBlockRef body = llvm_append_block(obj, fn, "body");
   LLVMBasicBlockRef exit = llvm_append_block(obj, fn, "exit");

   LLVMTypeRef type = obj->types[LLVM_INT8 + sz];

   LLVMValueRef zero = LLVMBuildIsNull(obj->builder, bytes, "");
   LLVMBuildCondBr(obj->builder, zero, exit, body);

   LLVMPositionBuilderAtEnd(obj->builder, body);

   LLVMValueRef ptr = LLVMBuildPhi(obj->builder, obj->types[LLVM_PTR], "ptr");

#ifndef LLVM_HAS_OPAQUE_POINTERS
   LLVMTypeRef ptr_type = LLVMPointerType(type, 0);
   LLVMValueRef cast = LLVMBuildPointerCast(obj->builder, ptr, ptr_type, "");
#else
   LLVMValueRef cast = ptr;
#endif

   LLVMBuildStore(obj->builder, value, cast);

   LLVMValueRef indexes2[] = { llvm_intptr(obj, 1) };
   LLVMValueRef next = LLVMBuildGEP2(obj->builder, type, cast, indexes2, 1, "");

   LLVMValueRef ptr_in[] = { dest, PTR(next) };
   LLVMBasicBlockRef ptr_bb[] = { entry, body };
   LLVMAddIncoming(ptr, ptr_in, ptr_bb, 2);

   LLVMValueRef cont =
      LLVMBuildICmp(obj->builder, LLVMIntULT, PTR(next), eptr, "");
   LLVMBuildCondBr(obj->builder, cont, body, exit);

   LLVMPositionBuilderAtEnd(obj->builder, exit);

   LLVMBuildRetVoid(obj->builder);
}

////////////////////////////////////////////////////////////////////////////////
// JIT plugin interface

typedef struct {
   code_cache_t *code;
} llvm_jit_state_t;

static void *jit_llvm_init(jit_t *jit)
{
   LLVMInitializeNativeTarget();
   LLVMInitializeNativeAsmPrinter();

   llvm_jit_state_t *state = xcalloc(sizeof(llvm_jit_state_t));
   state->code = code_cache_new();

   return state;
}

static void jit_llvm_cgen(jit_t *j, jit_handle_t handle, void *context)
{
   llvm_jit_state_t *state = context;

   jit_func_t *f = jit_get_func(j, handle);

#ifdef DEBUG
   const char *only = getenv("NVC_JIT_ONLY");
   if (only != NULL && !icmp(f->name, only))
      return;
#endif

   const uint64_t start_us = get_timestamp_us();

   LLVMTargetMachineRef tm = llvm_target_machine(LLVMRelocDefault,
                                                 JIT_CODE_MODEL);

   llvm_obj_t obj = {
      .context = LLVMContextCreate(),
      .target  = tm,
   };

   LOCAL_TEXT_BUF tb = tb_new();
   tb_istr(tb, f->name);

   obj.module    = LLVMModuleCreateWithNameInContext(tb_get(tb), obj.context);
   obj.builder   = LLVMCreateBuilderInContext(obj.context);
   obj.data_ref  = LLVMCreateTargetDataLayout(tm);

#if ENABLE_DWARF
   obj.debuginfo = LLVMCreateDIBuilderDisallowUnresolved(obj.module);
#endif

   llvm_register_types(&obj);

   cgen_func_t func = {
      .name   = tb_claim(tb),
      .source = f,
      .mode   = CGEN_JIT,
   };

   cgen_function(&obj, &func);

   llvm_obj_finalise(&obj, LLVM_O0);

   LLVMMemoryBufferRef buf;
   char *error;
   if (LLVMTargetMachineEmitToMemoryBuffer(tm, obj.module, LLVMObjectFile,
                                           &error, &buf))
     fatal("failed to generate native code: %s", error);

   const size_t objsz = LLVMGetBufferSize(buf);

   code_blob_t *blob = code_blob_new(state->code, f->name, objsz);
   if (blob == NULL)
      return;

   const uint8_t *base = blob->wptr;
   const void *entry_addr = blob->wptr;

   code_load_object(blob, LLVMGetBufferStart(buf), objsz);

   const size_t size = blob->wptr - base;
   code_blob_finalise(blob, &(f->entry));

   if (opt_get_int(OPT_JIT_LOG)) {
      const uint64_t end_us = get_timestamp_us();
      debugf("%s at %p [%zu bytes in %"PRIi64" us]", func.name,
             entry_addr, size, end_us - start_us);
   }

   LLVMDisposeMemoryBuffer(buf);
   LLVMDisposeTargetData(obj.data_ref);
   LLVMDisposeTargetMachine(tm);
   LLVMDisposeBuilder(obj.builder);
   DWARF_ONLY(LLVMDisposeDIBuilder(obj.debuginfo));
   LLVMContextDispose(obj.context);
   free(func.name);
}

static void jit_llvm_cleanup(void *context)
{
   llvm_jit_state_t *state = context;
   code_cache_free(state->code);
   free(state);
}

static const jit_plugin_t jit_llvm = {
   .init    = jit_llvm_init,
   .cgen    = jit_llvm_cgen,
   .cleanup = jit_llvm_cleanup
};

void jit_register_llvm_plugin(jit_t *j)
{
   const int threshold = opt_get_int(OPT_JIT_THRESHOLD);
   if (threshold > 0)
      jit_add_tier(j, threshold, &jit_llvm);
   else if (threshold < 0)
      warnf("invalid NVC_JIT_THRESOLD setting %d", threshold);
}


////////////////////////////////////////////////////////////////////////////////
// Ahead-of-time code generation

llvm_obj_t *llvm_obj_new(const char *name)
{
   llvm_obj_t *obj = xcalloc(sizeof(llvm_obj_t));
   obj->context     = LLVMContextCreate();
   obj->module      = LLVMModuleCreateWithNameInContext(name, obj->context);
   obj->builder     = LLVMCreateBuilderInContext(obj->context);
   obj->target      = llvm_target_machine(LLVMRelocPIC, LLVMCodeModelDefault);
   obj->data_ref    = LLVMCreateTargetDataLayout(obj->target);
   obj->pack_writer = pack_writer_new();

#if ENABLE_DWARF
   obj->debuginfo = LLVMCreateDIBuilderDisallowUnresolved(obj->module);
#endif

   char *triple = LLVMGetTargetMachineTriple(obj->target);
   LLVMSetTarget(obj->module, triple);
   LLVMDisposeMessage(triple);

   LLVMSetModuleDataLayout(obj->module, obj->data_ref);

   llvm_register_types(obj);

#if ENABLE_DWARF
   llvm_add_module_flag(obj, "Debug Info Version", DEBUG_METADATA_VERSION);
#ifdef __APPLE__
   llvm_add_module_flag(obj, "Dwarf Version", 2);
#else
   llvm_add_module_flag(obj, "Dwarf Version", 4);
#endif
#endif

   obj->strtab = LLVMAddGlobal(obj->module, obj->types[LLVM_STRTAB],
                               "placeholder_strtab");
   LLVMSetGlobalConstant(obj->strtab, true);
   LLVMSetLinkage(obj->strtab, LLVMPrivateLinkage);

   return obj;
}

void llvm_add_abi_version(llvm_obj_t *obj)
{
   LLVMValueRef abi_version =
      LLVMAddGlobal(obj->module, obj->types[LLVM_INT32], "__nvc_abi_version");
   LLVMSetInitializer(abi_version, llvm_int32(obj, RT_ABI_VERSION));
   LLVMSetGlobalConstant(abi_version, true);
#ifdef IMPLIB_REQUIRED
   LLVMSetDLLStorageClass(abi_version, LLVMDLLExportStorageClass);
#endif
}

void llvm_aot_compile(llvm_obj_t *obj, jit_t *j, jit_handle_t handle)
{
   DEBUG_ONLY(const uint64_t start_us = get_timestamp_us());

   jit_func_t *f = jit_get_func(j, handle);
   jit_fill_irbuf(f);

   LOCAL_TEXT_BUF tb = safe_symbol(f->name);

   cgen_func_t func = {
      .name   = tb_claim(tb),
      .source = f,
      .mode   = CGEN_AOT,
   };

   cgen_function(obj, &func);

#ifdef DEBUG
   const uint64_t end_us = get_timestamp_us();
   if (end_us - start_us > 100000)
      debugf("compiled %s [%"PRIi64" us]", func.name, end_us - start_us);
#endif

   free(func.name);
}

static void llvm_finalise_string_table(llvm_obj_t *obj)
{
   if (obj->pack_writer == NULL)
      return;

   const char *strtab;
   size_t len;
   pack_writer_string_table(obj->pack_writer, &strtab, &len);

   LLVMValueRef init =
      LLVMConstStringInContext(obj->context, strtab, len - 1, false);
   LLVMTypeRef array_type = LLVMArrayType(obj->types[LLVM_INT8], len);
   LLVMValueRef global = LLVMAddGlobal(obj->module, array_type, "strtab");
   LLVMSetGlobalConstant(global, true);
   LLVMSetInitializer(global, init);
   LLVMSetLinkage(global, LLVMPrivateLinkage);

   LLVMReplaceAllUsesWith(obj->strtab, global);
   LLVMDeleteGlobal(obj->strtab);
}

void llvm_obj_finalise(llvm_obj_t *obj, llvm_opt_level_t olevel)
{
   if (obj->fns[LLVM_TLAB_ALLOC] != NULL)
      cgen_tlab_alloc_body(obj);

   for (jit_size_t sz = JIT_SZ_8; sz <= JIT_SZ_64; sz++) {
      if (obj->fns[LLVM_EXP_OVERFLOW_S8 + sz] != NULL)
         cgen_exp_overflow_body(obj, LLVM_EXP_OVERFLOW_S8 + sz, sz,
                                LLVM_MUL_OVERFLOW_S8);
      if (obj->fns[LLVM_EXP_OVERFLOW_U8 + sz] != NULL)
         cgen_exp_overflow_body(obj, LLVM_EXP_OVERFLOW_U8 + sz, sz,
                                LLVM_MUL_OVERFLOW_U8);
      if (obj->fns[LLVM_MEMSET_U8 + sz] != NULL)
         cgen_memset_body(obj, LLVM_MEMSET_U8 + sz, sz,
                          LLVM_MUL_OVERFLOW_U8);
   }

   llvm_finalise_string_table(obj);

   DWARF_ONLY(LLVMDIBuilderFinalize(obj->debuginfo));

   llvm_dump_module(obj->module, "initial");
   llvm_verify_module(obj->module);
   llvm_optimise(obj->module, obj->target, olevel);
   llvm_dump_module(obj->module, "final");
}

void llvm_obj_emit(llvm_obj_t *obj, const char *path)
{
   char *error;
   if (LLVMTargetMachineEmitToFile(obj->target, obj->module, (char *)path,
                                   LLVMObjectFile, &error))
      fatal("Failed to write object file: %s", error);

   LLVMDisposeTargetData(obj->data_ref);
   LLVMDisposeTargetMachine(obj->target);
   LLVMDisposeBuilder(obj->builder);
   LLVMDisposeModule(obj->module);
   LLVMContextDispose(obj->context);

   pack_writer_free(obj->pack_writer);

   free(obj);
}
