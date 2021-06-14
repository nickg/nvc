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
#include "rt.h"
#include "lib.h"
#include "tree.h"
#include "common.h"
#include "array.h"

#include <assert.h>
#include <limits.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <errno.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <math.h>

#ifdef HAVE_EXECINFO_H
#include <execinfo.h>
#endif

#ifdef __MINGW32__
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#else
#include <dlfcn.h>
#endif

#define TRACE_MAX 10

#ifdef __MINGW32__
#ifdef _WIN64
extern void ___chkstk_ms(void);
#else
#undef _alloca
extern void _alloca(void);
#endif

static A(HMODULE) search_modules;
#endif

void *jit_find_symbol(const char *name, bool required)
{
#if (defined __MINGW32__ || defined __CYGWIN__) && !defined _WIN64
   if (*name == '_')
      name++;   // Remove leading underscore on 32-bit Windows
#endif

   name = safe_symbol(name);

#ifdef __MINGW32__

#ifdef _WIN64
   if (strcmp(name, "___chkstk_ms") == 0)
      return (void *)(uintptr_t)___chkstk_ms;
#else
   if (strcmp(name, "_alloca") == 0)
      return (void *)(uintptr_t)_alloca;
#endif

   if (strcmp(name, "exp2") == 0)
      return (void *)(uintptr_t)exp2;

   for (size_t i = 0; i < search_modules.count; i++) {
      HMODULE h = search_modules.items[i];
      void *ptr = (void *)(uintptr_t)GetProcAddress(h, name);
      if (ptr != NULL)
         return ptr;
   }

   if (required)
      fatal("cannot find symbol %s", name);

   return NULL;

#else  // __MINGW32__

   dlerror();   // Clear any previous error

   void *sym = dlsym(NULL, name);
   const char *error = dlerror();
   if (error != NULL) {
      sym = dlsym(RTLD_DEFAULT, name);
      error = dlerror();
      if ((error != NULL) && required)
         fatal("%s: %s", name, error);
   }
   return sym;
#endif
}

static void jit_load_module(ident_t name)
{
   lib_t lib = lib_find(ident_until(name, '.'), true);

   tree_kind_t kind = lib_index_kind(lib, name);
   if (kind == T_LAST_TREE_KIND)
      fatal("Cannot find %s in library %s", istr(name), istr(lib_name(lib)));

   if (kind == T_ENTITY || kind == T_ARCH)
      return;

   const bool optional = (kind == T_PACKAGE || kind == T_PACK_BODY);

   char *so_fname LOCAL = xasprintf("_%s." DLL_EXT, istr(name));

   char so_path[PATH_MAX];
   lib_realpath(lib, so_fname, so_path, sizeof(so_path));

   if (access(so_path, F_OK) != 0 && optional)
      return;

   if (opt_get_int("rt_trace_en"))
      fprintf(stderr, "TRACE (init): load %s from %s\n", istr(name), so_path);

#ifdef __MINGW32__
   HMODULE hModule = LoadLibrary(so_path);
   if (hModule == NULL)
      fatal("failed to load %s", so_path);

   APUSH(search_modules, hModule);
#else
   if (dlopen(so_path, RTLD_LAZY | RTLD_GLOBAL) == NULL)
      fatal("%s: %s", so_path, dlerror());
#endif
}

void jit_init(tree_t top)
{
#ifdef __MINGW32__
   ACLEAR(search_modules);
   APUSH(search_modules, GetModuleHandle(NULL));
   APUSH(search_modules, GetModuleHandle("MSVCRT.DLL"));
#endif

   const int ncontext = tree_contexts(top);
   for (int i = 0; i < ncontext; i++) {
      tree_t c = tree_context(top, i);
      if (tree_kind(c) == T_USE)
         jit_load_module(tree_ident(c));
   }

   jit_load_module(tree_ident(top));
}

void jit_shutdown(void)
{

}
