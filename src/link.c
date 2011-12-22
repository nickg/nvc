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

#include "util.h"
#include "phase.h"
#include "tree.h"

#include <assert.h>
#include <stdlib.h>
#include <stdarg.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/wait.h>

#include "llvm/Config/llvm-config.h"

#define MAX_ARGS 64
#define ARG_LEN  256

static char **args = NULL;
static int  n_args = 0;

__attribute__((format(printf, 1, 2)))
static void link_arg_f(const char *fmt, ...)
{
   va_list ap;
   assert(n_args < MAX_ARGS);
   va_start(ap, fmt);
   args[n_args] = xmalloc(ARG_LEN);
   vsnprintf(args[n_args], ARG_LEN, fmt, ap);
   va_end(ap);
   args[++n_args] = NULL;
}

static void link_arg_bc(lib_t lib, ident_t name)
{
   char path[256];
   lib_realpath(lib, NULL, path, sizeof(path));

   link_arg_f("%s/_%s.bc", path, istr(name));
}

static bool link_needs_body(tree_t pack)
{
   return false;   // TODO
}

static void link_context(context_t ctx)
{
   ident_t all = ident_strip(ctx.name, ident_new(".all"));
   assert(all != NULL);
   printf("%s\n", istr(all));

   lib_t lib = lib_find(istr(ident_until(ctx.name, '.')), true, true);
   if (lib == NULL)
      fatal("cannot link library %s", istr(all));

   tree_t unit = lib_get(lib, all);
   if (unit == NULL)
      fatal("cannot find unit %s", istr(all));
   else if (tree_kind(unit) != T_PACKAGE)
      return;

   ident_t body_i = ident_prefix(all, ident_new("body"), '-');
   tree_t body = lib_get(lib, body_i);
   if (body == NULL) {
      if (link_needs_body(unit))
         fatal("missing body for package %s", istr(all));
      else
         return;
   }

   link_arg_bc(lib, body_i);
}

static void link_output(tree_t top)
{
   link_arg_f("-b");

   ident_t orig = ident_strip(tree_ident(top), ident_new(".elab"));
   ident_t final = ident_prefix(orig, ident_new("final"), '.');
   link_arg_bc(lib_work(), final);
}

void link_bc(tree_t top)
{
   args = xmalloc(MAX_ARGS * sizeof(char*));

   link_arg_f("%s/bin/llvm-ld", LLVM_PREFIX);
   link_arg_f("-v");
   link_arg_f("-r");
   link_arg_f("-stats");

   link_output(top);
   link_arg_bc(lib_work(), tree_ident(top));

   for (unsigned i = 0; i < tree_contexts(top); i++)
      link_context(tree_context(top, i));

   for (int i = 0; i < n_args; i++)
      printf("%s%c", args[i], (i + 1 == n_args ? '\n' : ' '));

   pid_t pid = fork();
   if (pid == 0) {
      execv(args[0], args);
      fatal_errno("execv");
   }
   else if (pid > 0) {
      int status;
      if (waitpid(pid, &status, 0) != pid)
         fatal_errno("waitpid");

      if (WEXITSTATUS(status) != 0)
         fatal("llvm-ld failed with status %d", WEXITSTATUS(status));
   }
   else
      fatal_errno("fork");

   for (int i = 0; i < n_args; i++)
      free(args[i]);
   free(args);
}

