//
//  Copyright (C) 2026  Nick Gasson
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
#include "jit/jit-priv.h"

#include <assert.h>
#include <setjmp.h>

#if defined __MINGW32__ && defined(__clang__)

#define WIN32_LEAN_AND_MEAN
#include <windows.h>

#define NVC_JIT_UNWIND_EXCEPTION ((DWORD)0xe0004a49u)

static int jit_seh_filter(DWORD code)
{
   return code == NVC_JIT_UNWIND_EXCEPTION
      ? EXCEPTION_EXECUTE_HANDLER
      : EXCEPTION_CONTINUE_SEARCH;
}

__attribute__((noinline))
bool jit_trampoline(jit_thread_local_t *thread, jit_func_t *f,
                    jit_scalar_t *args, tlab_t *tlab)
{
   relaxed_store(&thread->jmp_buf, thread);

   bool ok = true;
   __try {
      jit_entry_fn_t entry = load_acquire(&f->entry);
      (*entry)(f, NULL, args, tlab);
   }
   __except (jit_seh_filter(GetExceptionCode())) {
      ok = false;
   }

   relaxed_store(&thread->jmp_buf, NULL);
   thread->anchor = NULL;
   return ok;
}

__attribute__((noinline))
void jit_unwind(jit_thread_local_t *thread)
{
   assert(thread->jmp_buf != NULL);
   RaiseException(NVC_JIT_UNWIND_EXCEPTION, EXCEPTION_NONCONTINUABLE, 0, NULL);
   __builtin_unreachable();
}

#else

__attribute__((noinline))
bool jit_trampoline(jit_thread_local_t *thread, jit_func_t *f,
                    jit_scalar_t *args, tlab_t *tlab)
{
#if defined HAVE___BUILTIN_SETJMP && !defined __clang__
   void *jmp_buf[5];
   const int rc = __builtin_setjmp(jmp_buf);
#else
   sigjmp_buf jmp_buf;
   const int rc = sigsetjmp(jmp_buf, 0);
#endif

   if (likely(rc == 0)) {
      relaxed_store(&thread->jmp_buf, jmp_buf);

      jit_entry_fn_t entry = load_acquire(&f->entry);
      (*entry)(f, NULL, args, tlab);

      relaxed_store(&thread->jmp_buf, NULL);
      thread->anchor = NULL;
      return true;
   }
   else {
      relaxed_store(&thread->jmp_buf, NULL);
      thread->anchor = NULL;
      return false;
   }
}

__attribute__((noinline))
void jit_unwind(jit_thread_local_t *thread)
{
   assert(thread->jmp_buf != NULL);
#if defined HAVE___BUILTIN_SETJMP && !defined __clang__
   __builtin_longjmp(thread->jmp_buf, 1);
#else
   siglongjmp(thread->jmp_buf, 1);
#endif
}

#endif
