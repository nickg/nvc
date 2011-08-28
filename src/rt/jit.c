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

#include "rt.h"
#include "util.h"
#include "lib.h"

#include <assert.h>
#include <limits.h>

#include <llvm-c/Core.h>
#include <llvm-c/BitReader.h>
#include <llvm-c/ExecutionEngine.h>

static LLVMModuleRef          module = NULL;
static LLVMExecutionEngineRef exec_engine = NULL;

void *jit_fun_ptr(const char *name)
{
   LLVMValueRef fn;
   if (LLVMFindFunction(exec_engine, name, &fn))
      fatal("cannot find function %s", name);

   return LLVMGetPointerToGlobal(exec_engine, fn);
}

void jit_bind_fn(const char *name, void *ptr)
{
   LLVMValueRef fn;
   if (LLVMFindFunction(exec_engine, name, &fn))
      fatal("cannot find function %s", name);

   LLVMAddGlobalMapping(exec_engine, fn, ptr);
}

void jit_init(ident_t top)
{
   char fname[128];
   snprintf(fname, sizeof(fname), "_%s.bc", istr(top));

   char path[PATH_MAX];
   lib_realpath(lib_work(), fname, path, sizeof(path));

   char *error;
   LLVMMemoryBufferRef buf;
   if (LLVMCreateMemoryBufferWithContentsOfFile(path, &buf, &error))
      fatal("error reading bitcode from %s: %s", path, error);

   if (LLVMParseBitcode(buf, &module, &error))
      fatal("error parsing bitcode: %s", error);

   LLVMDisposeMemoryBuffer(buf);

   LLVMInitializeNativeTarget();
   LLVMLinkInJIT();

   if (LLVMCreateExecutionEngineForModule(&exec_engine, module, &error))
      fatal("error creating execution engine: %s", error);
}

void jit_shutdown(void)
{
   LLVMDisposeExecutionEngine(exec_engine);
}
