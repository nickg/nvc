//
//  Copyright (C) 2022-2025  Nick Gasson
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

   LLVM_ADD_SAT_U8,
   LLVM_ADD_SAT_U16,
   LLVM_ADD_SAT_U32,
   LLVM_ADD_SAT_U64,

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
   LLVM_GET_OBJECT,
   LLVM_TLAB_ALLOC,
   LLVM_SCHED_WAVEFORM,
   LLVM_TEST_EVENT,
   LLVM_LAST_EVENT,
   LLVM_SCHED_PROCESS,
   LLVM_PACK,
   LLVM_UNPACK,
   LLVM_VEC4OP,

   LLVM_LAST_FN,
} llvm_fn_t;

typedef enum {
   LLVM_O0,
   LLVM_O1,
   LLVM_O2,
   LLVM_O3
} llvm_opt_level_t;

typedef struct _cgen_func  cgen_func_t;
typedef struct _cgen_block cgen_block_t;

#define DEBUG_METADATA_VERSION 3
#define ARGCACHE_SIZE          6
#define ENABLE_DWARF           0
#define INLINE_LIMIT           25

#if defined __APPLE__ && defined ARCH_ARM64
#define JIT_CODE_MODEL LLVMCodeModelSmall
#elif defined __linux__ && defined ARCH_X86_64
#define JIT_CODE_MODEL LLVMCodeModelLarge
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
   unsigned              opt_hint;
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
#ifdef LLVM_HAS_CAPTURES
   else if (attr == FUNC_ATTR_NOCAPTURE) {
      const unsigned kind =
         LLVMGetEnumAttributeKindForName("captures", 8);
      if (kind == 0)
         fatal_trace("Cannot get LLVM captures attribute");

      ref = LLVMCreateEnumAttribute(obj->context, kind, 0);
   }
#endif
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
         obj->types[LLVM_PTR],                     // Mspace object
         obj->types[LLVM_INT32],                   // Allocation pointer
         obj->types[LLVM_INT32],                   // Limit pointer
         LLVMArrayType(obj->types[LLVM_INT8], 0),  // Data
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

#ifdef NAME_MAX
   if (tb_len(tb) > NAME_MAX)
      tb_trim(tb, NAME_MAX);
#endif

   tb_replace(tb, '/', '_');

   char *error;
   if (LLVMPrintModuleToFile(module, tb_get(tb), &error))
      fatal("Failed to write LLVM IR file: %s: %s", tb_get(tb), error);

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

   case LLVM_ADD_SAT_U8:
   case LLVM_ADD_SAT_U16:
   case LLVM_ADD_SAT_U32:
   case LLVM_ADD_SAT_U64:
      {
         jit_size_t sz = which - LLVM_ADD_SAT_U8;
         LLVMTypeRef int_type = obj->types[LLVM_INT8 + sz];
         LLVMTypeRef args[] = { int_type, int_type };
         obj->fntypes[which] = LLVMFunctionType(int_type, args,
                                                ARRAY_LEN(args), false);

         static const char *names[] = {
            "llvm.uadd.sat.i8",
            "llvm.uadd.sat.i16",
            "llvm.uadd.sat.i32",
            "llvm.uadd.sat.i64"
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
   case LLVM_SCHED_PROCESS:
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
         case LLVM_SCHED_PROCESS: sym = "__nvc_sched_process"; break;
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

   case LLVM_PACK:
      {
         LLVMTypeRef args[] = {
            obj->types[LLVM_PTR],
            obj->types[LLVM_INT32],
#ifdef LLVM_HAS_OPAQUE_POINTERS
            obj->types[LLVM_PTR],
#else
            LLVMPointerType(obj->types[LLVM_INT64], 0),
#endif
         };
         obj->fntypes[which] = LLVMFunctionType(obj->types[LLVM_VOID], args,
                                                ARRAY_LEN(args), false);

         fn = llvm_add_fn(obj, "__nvc_pack", obj->fntypes[which]);
         llvm_add_func_attr(obj, fn, FUNC_ATTR_NOUNWIND, -1);
      }
      break;

   case LLVM_UNPACK:
      {
         LLVMTypeRef args[] = {
            obj->types[LLVM_INT64],
            obj->types[LLVM_INT64],
#ifdef LLVM_HAS_OPAQUE_POINTERS
            obj->types[LLVM_PTR],
#else
            LLVMPointerType(obj->types[LLVM_INT64], 0),
#endif
         };
         obj->fntypes[which] = LLVMFunctionType(obj->types[LLVM_VOID], args,
                                                ARRAY_LEN(args), false);

         fn = llvm_add_fn(obj, "__nvc_unpack", obj->fntypes[which]);
         llvm_add_func_attr(obj, fn, FUNC_ATTR_NOUNWIND, -1);
      }
      break;

   case LLVM_VEC4OP:
      {
         LLVMTypeRef args[] = {
            obj->types[LLVM_INT32],
#ifdef LLVM_HAS_OPAQUE_POINTERS
            obj->types[LLVM_PTR],
#else
            LLVMPointerType(obj->types[LLVM_ANCHOR], 0),
#endif
#ifdef LLVM_HAS_OPAQUE_POINTERS
            obj->types[LLVM_PTR],
#else
            LLVMPointerType(obj->types[LLVM_INT64], 0),
#endif
            obj->types[LLVM_INT32],
         };
         obj->fntypes[which] = LLVMFunctionType(obj->types[LLVM_VOID], args,
                                                ARRAY_LEN(args), false);

         fn = llvm_add_fn(obj, "__nvc_vec4op", obj->fntypes[which]);
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

static void llvm_native_setup(void)
{
   LLVMInitializeNativeTarget();
   LLVMInitializeNativeAsmPrinter();

   if (!LLVMIsMultithreaded())
      fatal("LLVM was built without multithreaded support");
}

////////////////////////////////////////////////////////////////////////////////
// JIT IR to LLVM lowering

static void cgen_function(llvm_obj_t *obj, cgen_func_t *func);

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
   jit_dump_with_mark(cgb->func->source, ir - cgb->func->source->irbuf);
   fatal_trace("%s", text);

   va_end(ap);
}

static LLVMRealPredicate cgen_real_pred(cgen_block_t *cgb, jit_ir_t *ir)
{
   switch (ir->cc) {
   case JIT_CC_EQ: return LLVMRealOEQ;
   case JIT_CC_NE: return LLVMRealONE;
   case JIT_CC_GT: return LLVMRealOGT;
   case JIT_CC_LT: return LLVMRealOLT;
   case JIT_CC_LE: return LLVMRealOLE;
   case JIT_CC_GE: return LLVMRealOGE;
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
      return llvm_int32(obj, value.handle);
   case JIT_ADDR_ABS:
      return llvm_ptr(obj, (void *)(intptr_t)value.int64);
   case JIT_ADDR_COVER:
      return llvm_ptr(obj, jit_get_cover_ptr(cgb->func->source, value));
   case JIT_VALUE_LOCUS:
      return llvm_ptr(obj, value.locus);
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

static LLVMValueRef cgen_maybe_inline(llvm_obj_t *obj, jit_func_t *callee)
{
   if (load_acquire(&callee->state) != JIT_FUNC_READY)
      return NULL;

   if (callee->nirs > INLINE_LIMIT)
      return NULL;

   LOCAL_TEXT_BUF tb = tb_new();
   tb_istr(tb, callee->name);
   tb_cat(tb, "!inline");

   LLVMValueRef exist = LLVMGetNamedFunction(obj->module, tb_get(tb));
   if (exist != NULL)
      return exist;

   cgen_func_t func = {
      .name   = tb_claim(tb),
      .source = callee,
   };

   LLVMBasicBlockRef old_bb = LLVMGetInsertBlock(obj->builder);
   LLVMClearInsertionPosition(obj->builder);

   cgen_function(obj, &func);

   LLVMSetLinkage(func.llvmfn, LLVMInternalLinkage);

   LLVMPositionBuilderAtEnd(obj->builder, old_bb);

   free(func.name);
   obj->opt_hint++;
   return func.llvmfn;
}

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

   llvm_type_t type = LLVM_INT64;

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

   llvm_type_t type = LLVM_INT8 + ir->size;

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

   LLVMValueRef shift = LLVMBuildShl(obj->builder, arg1, arg2,
                                     cgen_reg_name(ir->result));

   if (ir->arg2.kind == JIT_VALUE_INT64 && (uint64_t)ir->arg2.int64 < 64)
      cgb->outregs[ir->result] = shift;
   else {
      LLVMValueRef inrange = LLVMBuildICmp(obj->builder, LLVMIntULT, arg2,
                                           llvm_int64(obj, 64), "");
      cgb->outregs[ir->result] = LLVMBuildSelect(obj->builder, inrange, shift,
                                                 llvm_int64(obj, 0), "");
   }
}

static void cgen_op_shr(llvm_obj_t *obj, cgen_block_t *cgb, jit_ir_t *ir)
{
   LLVMValueRef arg1 = cgen_get_value(obj, cgb, ir->arg1);
   LLVMValueRef arg2 = cgen_get_value(obj, cgb, ir->arg2);

   LLVMValueRef shift = LLVMBuildLShr(obj->builder, arg1, arg2,
                                      cgen_reg_name(ir->result));

   if (ir->arg2.kind == JIT_VALUE_INT64 && (uint64_t)ir->arg2.int64 < 64)
      cgb->outregs[ir->result] = shift;
   else {
      LLVMValueRef inrange = LLVMBuildICmp(obj->builder, LLVMIntULT, arg2,
                                           llvm_int64(obj, 64), "");
      cgb->outregs[ir->result] = LLVMBuildSelect(obj->builder, inrange, shift,
                                                 llvm_int64(obj, 0), "");
   }
}

static void cgen_op_asr(llvm_obj_t *obj, cgen_block_t *cgb, jit_ir_t *ir)
{
   LLVMValueRef arg1 = cgen_get_value(obj, cgb, ir->arg1);
   LLVMValueRef arg2 = cgen_get_value(obj, cgb, ir->arg2);

   LLVMValueRef shift = LLVMBuildAShr(obj->builder, arg1, arg2,
                                      cgen_reg_name(ir->result));

   if (ir->arg2.kind == JIT_VALUE_INT64 && (uint64_t)ir->arg2.int64 < 64)
      cgb->outregs[ir->result] = shift;
   else {
      LLVMValueRef inrange = LLVMBuildICmp(obj->builder, LLVMIntULT, arg2,
                                           llvm_int64(obj, 64), "");
      cgb->outregs[ir->result] = LLVMBuildSelect(obj->builder, inrange, shift,
                                                 llvm_int64(obj, 0), "");
   }
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

   LLVMValueRef fptr = llvm_ptr(obj, callee);

   LLVMValueRef entry = cgen_maybe_inline(obj, callee);
   if (entry == NULL) {
      // Must have acquire semantics to synchronise with installing new code
      entry = LLVMBuildLoad2(obj->builder, obj->types[LLVM_PTR], fptr, "entry");
      LLVMSetAlignment(entry, sizeof(void *));
      LLVMSetOrdering(entry, LLVMAtomicOrderingAcquire);

#ifndef LLVM_HAS_OPAQUE_POINTERS
      LLVMTypeRef ptr_type = LLVMPointerType(obj->types[LLVM_ENTRY_FN], 0);
      entry = LLVMBuildPointerCast(obj->builder, entry, ptr_type, "");
#endif
   }

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

   case JIT_EXIT_LAST_EVENT:
      {
         LLVMValueRef args[] = {
            PTR(cgb->func->anchor),
            cgb->func->args,
            cgb->func->tlab,
         };
         llvm_call_fn(obj, LLVM_LAST_EVENT, args, ARRAY_LEN(args));
      }
      break;

   case JIT_EXIT_SCHED_PROCESS:
      {
         LLVMValueRef args[] = {
            PTR(cgb->func->anchor),
            cgb->func->args,
            cgb->func->tlab,
         };
         llvm_call_fn(obj, LLVM_SCHED_PROCESS, args, ARRAY_LEN(args));
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
   jit_func_t *f = jit_get_func(cgb->func->source->jit, ir->arg1.handle);
   LLVMValueRef ptrptr = llvm_ptr(obj, jit_get_privdata_ptr(f->jit, f));

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

   LLVMValueRef args[] = {
      cgen_get_value(obj, cgb, ir->arg1),
      value,
   };
   llvm_call_fn(obj, LLVM_PUTPRIV, args, ARRAY_LEN(args));
}

static void cgen_macro_pack(llvm_obj_t *obj, cgen_block_t *cgb, jit_ir_t *ir)
{
   LLVMValueRef args[] = {
      cgen_coerce_value(obj, cgb, ir->arg1, LLVM_PTR),
      cgen_coerce_value(obj, cgb, ir->arg2, LLVM_INT32),
      cgb->func->args,
   };
   llvm_call_fn(obj, LLVM_PACK, args, ARRAY_LEN(args));
}

static void cgen_macro_unpack(llvm_obj_t *obj, cgen_block_t *cgb, jit_ir_t *ir)
{
   LLVMValueRef args[] = {
      cgen_get_value(obj, cgb, ir->arg1),
      cgen_get_value(obj, cgb, ir->arg2),
      cgb->func->args,
   };
   llvm_call_fn(obj, LLVM_UNPACK, args, ARRAY_LEN(args));
}

static void cgen_macro_vec4op(llvm_obj_t *obj, cgen_block_t *cgb, jit_ir_t *ir)
{
   cgen_sync_irpos(obj, cgb, ir);

   LLVMValueRef op = cgen_get_value(obj, cgb, ir->arg1);
   LLVMValueRef size = cgen_get_value(obj, cgb, ir->arg2);

   LLVMValueRef args[] = {
      LLVMBuildTrunc(obj->builder, op, obj->types[LLVM_INT32], ""),
      cgb->func->anchor,
      cgb->func->args,
      LLVMBuildTrunc(obj->builder, size, obj->types[LLVM_INT32], ""),
   };
   llvm_call_fn(obj, LLVM_VEC4OP, args, ARRAY_LEN(args));
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

static void cgen_macro_reexec(llvm_obj_t *obj, cgen_block_t *cgb, jit_ir_t *ir)
{
   LLVMValueRef fptr = LLVMGetParam(cgb->func->llvmfn, 0);

   // Must have acquire semantics to synchronise with installing new code
   LLVMValueRef entry =
      LLVMBuildLoad2(obj->builder, obj->types[LLVM_PTR], fptr, "entry");
   LLVMSetAlignment(entry, sizeof(void *));
   LLVMSetOrdering(entry, LLVMAtomicOrderingAcquire);

#ifndef LLVM_HAS_OPAQUE_POINTERS
   LLVMTypeRef ptr_type = LLVMPointerType(obj->types[LLVM_ENTRY_FN], 0);
   entry = LLVMBuildPointerCast(obj->builder, entry, ptr_type, "");
#endif

   LLVMValueRef args[] = {
      fptr,
      LLVMGetParam(cgb->func->llvmfn, 1),
      cgb->func->args,
      cgb->func->tlab,
   };
   LLVMValueRef call = LLVMBuildCall2(obj->builder, obj->types[LLVM_ENTRY_FN],
                                      entry, args, ARRAY_LEN(args), "");
   LLVMSetTailCall(call, true);

   LLVMBuildRetVoid(obj->builder);
}

static void cgen_macro_sadd(llvm_obj_t *obj, cgen_block_t *cgb, jit_ir_t *ir)
{
   LLVMValueRef ptr = cgen_coerce_value(obj, cgb, ir->arg1, LLVM_PTR);
   llvm_type_t type = LLVM_INT8 + ir->size;

#ifndef LLVM_HAS_OPAQUE_POINTERS
   LLVMTypeRef ptr_type = LLVMPointerType(obj->types[type], 0);
   ptr = LLVMBuildPointerCast(obj->builder, ptr, ptr_type, "");
#endif

   LLVMValueRef cur = LLVMBuildLoad2(obj->builder, obj->types[type], ptr, "");
   LLVMValueRef addend = cgen_coerce_value(obj, cgb, ir->arg2, type);

   LLVMValueRef args[] = { cur, addend };
   LLVMValueRef sat = llvm_call_fn(obj, LLVM_ADD_SAT_U8 + ir->size, args, 2);

   LLVMBuildStore(obj->builder, sat, ptr);
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
   case J_SHR:
      cgen_op_shr(obj, cgb, ir);
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
   case MACRO_PACK:
      cgen_macro_pack(obj, cgb, ir);
      break;
   case MACRO_UNPACK:
      cgen_macro_unpack(obj, cgb, ir);
      break;
   case MACRO_VEC4OP:
      cgen_macro_vec4op(obj, cgb, ir);
      break;
   case MACRO_CASE:
      cgen_macro_case(obj, cgb, ir);
      break;
   case MACRO_TRIM:
      cgen_macro_trim(obj, cgb, ir);
      break;
   case MACRO_REEXEC:
      cgen_macro_reexec(obj, cgb, ir);
      break;
   case MACRO_SADD:
      cgen_macro_sadd(obj, cgb, ir);
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

static void cgen_reexecute_guard(llvm_obj_t *obj, cgen_func_t *func,
                                 jit_cfg_t *cfg)
{
   // Jump to the real entry if it may be modified at runtime
   // (e.g. bound to a foreign function)

   bool has_reexec = false;
   for (int i = 0; i < cfg->nblocks; i++) {
      jit_block_t *bb = &(cfg->blocks[i]);
      if (func->source->irbuf[bb->last].op == MACRO_REEXEC) {
         has_reexec = true;
         break;
      }
   }

   if (!has_reexec)
      return;

   LLVMValueRef fptr = LLVMGetParam(func->llvmfn, 0);

   // Must have acquire semantics to synchronise with installing new code
   LLVMValueRef entry =
      LLVMBuildLoad2(obj->builder, obj->types[LLVM_PTR], fptr, "entry");
   LLVMSetAlignment(entry, sizeof(void *));
   LLVMSetOrdering(entry, LLVMAtomicOrderingAcquire);

   LLVMBasicBlockRef reexec_bb = llvm_append_block(obj, func->llvmfn, "reexec");
   LLVMBasicBlockRef cont_bb = llvm_append_block(obj, func->llvmfn, "cont");

   LLVMValueRef changed = LLVMBuildICmp(obj->builder, LLVMIntNE, entry,
                                        PTR(func->llvmfn), "changed");
   LLVMBuildCondBr(obj->builder, changed, reexec_bb, cont_bb);

   LLVMPositionBuilderAtEnd(obj->builder, reexec_bb);

#ifndef LLVM_HAS_OPAQUE_POINTERS
   LLVMTypeRef ptr_type = LLVMPointerType(obj->types[LLVM_ENTRY_FN], 0);
   entry = LLVMBuildPointerCast(obj->builder, entry, ptr_type, "");
#endif

   LLVMValueRef anchor = LLVMGetParam(func->llvmfn, 1);

   LLVMValueRef args[] = {
      fptr,
      anchor,
      func->args,
      func->tlab,
   };
   LLVMValueRef call = LLVMBuildCall2(obj->builder, obj->types[LLVM_ENTRY_FN],
                                      entry, args, ARRAY_LEN(args), "");
   LLVMSetTailCall(call, true);

   LLVMBuildRetVoid(obj->builder);

   LLVMPositionBuilderAtEnd(obj->builder, cont_bb);
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

   LLVMBasicBlockRef entry_bb = llvm_append_block(obj, func->llvmfn, "entry");
   LLVMPositionBuilderAtEnd(obj->builder, entry_bb);

   func->args = LLVMGetParam(func->llvmfn, 2);
   LLVMSetValueName(func->args, "args");

   func->tlab = LLVMGetParam(func->llvmfn, 3);
   LLVMSetValueName(func->tlab, "tlab");

   cgen_frame_anchor(obj, func);
   cgen_cache_args(obj, func);

   jit_cfg_t *cfg = func->cfg = jit_get_cfg(func->source);
   cgen_reexecute_guard(obj, func, cfg);
   cgen_basic_blocks(obj, func, cfg);

   entry_bb = LLVMGetInsertBlock(obj->builder);

   cgen_block_t *cgb = func->blocks;

   LLVMValueRef zero_flag = llvm_int1(obj, false);

   int maxin = 0;
   for (int i = 0; i < func->source->nirs; i++) {
      if (i == cgb->source->first) {
         LLVMPositionBuilderAtEnd(obj->builder, cgb->bbref);

         int ndom = 0;
         cgen_block_t *dom[4];
         if (cgb->source->in.count <= ARRAY_LEN(dom)) {
            ndom = cgb->source->in.count;
            for (int j = 0; j < ndom; j++) {
               dom[j] = &(func->blocks[jit_get_edge(&cgb->source->in, j)]);
               if (dom[j] >= cgb) {
                  ndom = 0;   // Not processed this block
                  break;
               }
            }
         }

         cgb->inflags = zero_flag;

         if (mask_test(&cgb->source->livein, func->source->nregs)) {
             if (ndom == 1) {
                // Skip generating a phi instruction if there is just
                // one dominating block
                assert(dom[0]->outflags != NULL);
                cgb->inflags = dom[0]->outflags;
             }
             else if (cgb->source->in.count > 0) {
                LLVMTypeRef int1_type = obj->types[LLVM_INT1];
                cgb->inflags = LLVMBuildPhi(obj->builder, int1_type, "FLAGS");
             }
         }

         cgb->outflags = cgb->inflags;

         for (int j = 0; j < func->source->nregs; j++) {
            if (mask_test(&cgb->source->livein, j)) {
               LLVMValueRef dom_in = NULL;
               for (int k = 0; k < ndom; k++) {
                  assert(dom[k]->outregs[j] != NULL);
                  if (dom_in == NULL)
                     dom_in = dom[k]->outregs[j];
                  else if (dom[k]->outregs[j] != dom_in) {
                     // Need a phi
                     dom_in = NULL;
                     break;
                  }
               }

               if (dom_in != NULL) {
                  cgb->inregs[j] = cgb->outregs[j] = dom_in;
                  continue;
               }

               llvm_type_t type = LLVM_INT64;
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
         if (LLVMGetBasicBlockTerminator(cgb->bbref) == NULL) {
            if (cgb->source->aborts)
               LLVMBuildUnreachable(obj->builder);
            else {
               // Fall through to next block
               assert(!cgb->source->returns);
               assert(cgb + 1 < func->blocks + cfg->nblocks);
               LLVMBuildBr(obj->builder, (cgb + 1)->bbref);
            }
         }

         cgb++;
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

   jit_free_cfg(cfg);

   free(func->blocks);
   func->blocks = NULL;
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

   LLVMValueRef alloc_ptr =
      LLVMBuildStructGEP2(obj->builder, obj->types[LLVM_TLAB], tlab, 1, "");
   LLVMValueRef limit_ptr =
      LLVMBuildStructGEP2(obj->builder, obj->types[LLVM_TLAB], tlab, 2, "");

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
      LLVMBuildStructGEP2(obj->builder, obj->types[LLVM_TLAB], tlab, 3, "");

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
   llvm_native_setup();

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

   LLVMTargetMachineRef tm = llvm_target_machine(LLVMRelocStatic,
                                                 JIT_CODE_MODEL);

   llvm_obj_t obj = {
      .context = LLVMContextCreate(),
      .target  = tm,
   };

   LOCAL_TEXT_BUF tb = tb_new();
   tb_istr(tb, f->name);

   obj.module   = LLVMModuleCreateWithNameInContext(tb_get(tb), obj.context);
   obj.builder  = LLVMCreateBuilderInContext(obj.context);
   obj.data_ref = LLVMCreateTargetDataLayout(tm);

#if ENABLE_DWARF
   obj.debuginfo = LLVMCreateDIBuilderDisallowUnresolved(obj.module);
#endif

   llvm_register_types(&obj);

   cgen_func_t func = {
      .name   = tb_claim(tb),
      .source = f,
   };

   cgen_function(&obj, &func);

   if (obj.fns[LLVM_TLAB_ALLOC] != NULL)
      cgen_tlab_alloc_body(&obj);

   for (jit_size_t sz = JIT_SZ_8; sz <= JIT_SZ_64; sz++) {
      if (obj.fns[LLVM_EXP_OVERFLOW_S8 + sz] != NULL)
         cgen_exp_overflow_body(&obj, LLVM_EXP_OVERFLOW_S8 + sz, sz,
                                LLVM_MUL_OVERFLOW_S8);
      if (obj.fns[LLVM_EXP_OVERFLOW_U8 + sz] != NULL)
         cgen_exp_overflow_body(&obj, LLVM_EXP_OVERFLOW_U8 + sz, sz,
                                LLVM_MUL_OVERFLOW_U8);
      if (obj.fns[LLVM_MEMSET_U8 + sz] != NULL)
         cgen_memset_body(&obj, LLVM_MEMSET_U8 + sz, sz,
                          LLVM_MUL_OVERFLOW_U8);
   }

   DWARF_ONLY(LLVMDIBuilderFinalize(obj.debuginfo));

   if (jit_is_shutdown(f->jit))
      goto skip_emit;

   llvm_dump_module(obj.module, "initial");
   llvm_verify_module(obj.module);
   llvm_optimise(obj.module, obj.target, obj.opt_hint > 0 ? LLVM_O1 : LLVM_O0);
   llvm_dump_module(obj.module, "final");

   if (jit_is_shutdown(f->jit))
      goto skip_emit;

   LLVMMemoryBufferRef buf;
   char *error;
   if (LLVMTargetMachineEmitToMemoryBuffer(tm, obj.module, LLVMObjectFile,
                                           &error, &buf))
     fatal("failed to generate native code: %s", error);

   if (jit_is_shutdown(f->jit))
      goto skip_load;

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

 skip_load:
   LLVMDisposeMemoryBuffer(buf);

 skip_emit:
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
