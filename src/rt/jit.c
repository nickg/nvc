//
//  Copyright (C) 2011-2017  Nick Gasson
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

#include "rt.h"
#include "util.h"
#include "lib.h"
#include "tree.h"
#include "common.h"

#include <assert.h>
#include <limits.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <errno.h>
#include <dlfcn.h>
#include <stdlib.h>
#include <string.h>

#include <llvm-c/Core.h>
#include <llvm-c/BitReader.h>
#include <llvm-c/ExecutionEngine.h>

#if LLVM_HAS_ORC
#include <llvm-c/OrcBindings.h>
#endif

typedef struct module module_t;

#if LLVM_HAS_ORC
static LLVMOrcJITStackRef orc_ref = NULL;
#else
static LLVMExecutionEngineRef exec_engine = NULL;
#if !LLVM_HAS_MCJIT
static LLVMModuleRef top_module = NULL;
#endif
#endif


static void *jit_search_loaded_syms(const char *name, bool required)
{
   dlerror();   // Clear any previous error

   void *sym = dlsym(NULL, name);
   const char *error = dlerror();
   if (error != NULL) {
      sym = dlsym(RTLD_DEFAULT, name);
      error = dlerror();
      if ((error != NULL) && required)
         fatal("%s", error);
   }
   return sym;
}

void *jit_fun_ptr(const char *name, bool required)
{
#if LLVM_HAS_ORC
      return jit_var_ptr(name, required);
#else
      if (exec_engine != NULL) {
         LLVMValueRef fn;
         if (LLVMFindFunction(exec_engine, name, &fn))
            return jit_search_loaded_syms(name, required);

         return LLVMGetPointerToGlobal(exec_engine, fn);
      }
      else
         return jit_var_ptr(name, required);
#endif
}

void *jit_var_ptr(const char *name, bool required)
{
   void *ptr = NULL;

#if LLVM_HAS_ORC

   if (orc_ref != NULL) {
      char *mangled;
      LLVMOrcGetMangledSymbol(orc_ref, &mangled, name);
      ptr = (void *)LLVMOrcGetSymbolAddress(orc_ref, mangled);
      LLVMOrcDisposeMangledSymbol(mangled);
   }

#elif LLVM_HAS_MCJIT

   if (exec_engine != NULL)
      ptr = (void *)(uintptr_t)LLVMGetGlobalValueAddress(exec_engine, name);

#else

   if (exec_engine != NULL) {
      LLVMValueRef var = LLVMGetNamedGlobal(top_module, name);
      if (var != NULL)
         ptr = LLVMGetPointerToGlobal(exec_engine, var);
   }

#endif

   if (ptr == NULL)
      return jit_search_loaded_syms(name, required);
   else
      return ptr;
}

void jit_bind_fn(const char *name, void *ptr)
{
#if !LLVM_HAS_ORC
   if (exec_engine != NULL) {
      LLVMValueRef fn;
      if (LLVMFindFunction(exec_engine, name, &fn))
         return;

      LLVMAddGlobalMapping(exec_engine, fn, ptr);
   }
#endif
}

#if LLVM_HAS_ORC
static uint64_t jit_orc_sym_resolver(const char *name, void *ctx)
{
   return (uintptr_t)jit_search_loaded_syms(name, true);
}
#endif  // LLVM_HAS_ORC

static time_t jit_mod_time(const char *path)
{
   struct stat st;
   if (stat(path, &st) == -1) {
      if (errno == ENOENT)
         return 0;
      else
         fatal_errno("%s", path);
   }
   return st.st_mtime;
}

static void jit_load_module(ident_t name, LLVMModuleRef module)
{
   lib_t lib = lib_find(ident_until(name, '.'), true);

   tree_kind_t kind = lib_index_kind(lib, name);
   if (kind == T_LAST_TREE_KIND)
      fatal("Cannot find %s in library %s", istr(name), istr(lib_name(lib)));

   if (kind == T_ENTITY || kind == T_ARCH)
      return;

   const bool optional = (kind == T_PACKAGE || kind == T_PACK_BODY);

   char *bc_fname LOCAL = xasprintf("_%s.bc", istr(name));
   char *so_fname LOCAL = xasprintf("_%s.so", istr(name));

   char bc_path[PATH_MAX], so_path[PATH_MAX];
   lib_realpath(lib, bc_fname, bc_path, sizeof(bc_path));
   lib_realpath(lib, so_fname, so_path, sizeof(so_path));

   if (access(bc_path, F_OK) != 0 && access(so_path, F_OK) != 0 && optional)
      return;

   const bool use_jit = (jit_mod_time(bc_path) > jit_mod_time(so_path));

   if (use_jit) {
      notef("Load %s using JIT from %s", istr(name), bc_path);

      if (module == NULL) {
         char *error;
         LLVMMemoryBufferRef buf;
         if (LLVMCreateMemoryBufferWithContentsOfFile(bc_path, &buf, &error))
            fatal("error reading bitcode from %s: %s", bc_path, error);

         if (LLVMParseBitcode(buf, &module, &error))
            fatal("error parsing bitcode: %s", error);

         LLVMDisposeMemoryBuffer(buf);
      }

#if LLVM_HAS_ORC

      if (orc_ref == NULL) {
         LLVMInitializeNativeTarget();
         LLVMInitializeNativeAsmPrinter();
         LLVMLinkInMCJIT();

         char *def_triple = LLVMGetDefaultTargetTriple();
         char *error;
         LLVMTargetRef target_ref;
         if (LLVMGetTargetFromTriple(def_triple, &target_ref, &error))
            fatal("failed to get LLVM target for %s: %s", def_triple, error);

         if (!LLVMTargetHasJIT(target_ref))
            fatal("LLVM target %s has no JIT", LLVMGetTargetName(target_ref));

         LLVMTargetMachineRef tm_ref =
            LLVMCreateTargetMachine(target_ref, def_triple, "", "",
                                    LLVMCodeGenLevelDefault,
                                    LLVMRelocDefault,
                                    LLVMCodeModelJITDefault);
         assert(tm_ref);
         LLVMDisposeMessage(def_triple);

         orc_ref = LLVMOrcCreateInstance(tm_ref);
      }

      LLVMOrcAddLazilyCompiledIR(orc_ref, module, jit_orc_sym_resolver, NULL);

#else

      if (exec_engine == NULL) {
         LLVMInitializeNativeTarget();
         LLVMInitializeNativeAsmPrinter();

#if LLVM_HAS_MCJIT
         LLVMLinkInMCJIT();

         struct LLVMMCJITCompilerOptions options;
         LLVMInitializeMCJITCompilerOptions(&options, sizeof(options));

         char *error;
         if (LLVMCreateMCJITCompilerForModule(&exec_engine, module, &options,
                                              sizeof(options), &error))
            fatal("error creating MCJIT compiler: %s", error);
#else
         LLVMLinkInJIT();

         char *error;
         if (LLVMCreateExecutionEngineForModule(&exec_engine, module, &error))
            fatal("error creating execution engine: %s", error);

         top_module = module;
#endif
      }
      else
         LLVMAddModule(exec_engine, module);

#endif
   }
   else {
      notef("Load %s using native code from %s", istr(name), so_path);

      if (dlopen(so_path, RTLD_LAZY | RTLD_GLOBAL) == NULL)
         fatal("%s: %s", so_path, dlerror());

      // Free LLVM module as we no longer need it
      LLVMDisposeModule(module);
   }
}

void jit_init(tree_t top)
{
   const int ncontext = tree_contexts(top);
   for (int i = 0; i < ncontext; i++)
      jit_load_module(tree_ident(tree_context(top, i)), NULL);

   jit_load_module(tree_ident(top), tree_attr_ptr(top, llvm_i));
   tree_remove_attr(top, llvm_i);
}

void jit_shutdown(void)
{
#if LLVM_HAS_ORC
   if (orc_ref != NULL)
      LLVMOrcDisposeInstance(orc_ref);
#else
   if (exec_engine != NULL)
      LLVMDisposeExecutionEngine(exec_engine);
#endif
}
