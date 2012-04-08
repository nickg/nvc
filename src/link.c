//
//  Copyright (C) 2011-2012  Nick Gasson
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

#define MAX_ARGS 64
#define ARG_LEN  256

static char **args = NULL;
static int  n_args = 0;
static bool optimise = true;
static bool native = false;

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

static void link_product(lib_t lib, ident_t name, const char *ext)
{
   char path[256];
   lib_realpath(lib, NULL, path, sizeof(path));

   link_arg_f("%s/_%s.%s", path, istr(name), ext);
}

static void link_arg_bc(lib_t lib, ident_t name)
{
   link_product(lib, name, "bc");
}

static bool link_needs_body(tree_t pack)
{
   return false;   // TODO
}

static void link_context(context_t ctx)
{
   ident_t all = ident_strip(ctx.name, ident_new(".all"));
   assert(all != NULL);

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

static void link_output(tree_t top, const char *ext)
{
   ident_t orig = ident_strip(tree_ident(top), ident_new(".elab"));
   ident_t final = ident_prefix(orig, ident_new("final"), '.');
   link_product(lib_work(), final, ext);
}

static void link_args_begin(void)
{
   args = xmalloc(MAX_ARGS * sizeof(char*));
   n_args = 0;
}

static void link_args_end(void)
{
   for (int i = 0; i < n_args; i++)
      free(args[i]);
   free(args);
}

static void link_exec(void)
{
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
         fatal("%s failed with status %d", args[0], WEXITSTATUS(status));
   }
   else
      fatal_errno("fork");
}

static void link_assembly(tree_t top)
{
   link_args_begin();

   link_arg_f("%s/llc", LLVM_CONFIG_BINDIR);
   link_arg_f("-relocation-model=pic");
   link_output(top, "bc");

   link_exec();

   link_args_end();
}

static void link_shared(tree_t top)
{
   link_args_begin();

   link_arg_f("/usr/bin/cc");
   link_arg_f("-shared");
   link_arg_f("-o");
   link_output(top, "so");   // TODO: different on OS X, etc.
   link_output(top, "s");

   link_exec();

   link_args_end();
}

static void link_native(tree_t top)
{
   link_assembly(top);
   link_shared(top);
}

void link_bc(tree_t top)
{
   link_args_begin();

   link_arg_f("%s/llvm-ld", LLVM_CONFIG_BINDIR);
   link_arg_f("-r");

   if (!optimise)
      link_arg_f("--disable-opt");

   link_arg_f("-b");
   link_output(top, "bc");
   link_arg_bc(lib_work(), tree_ident(top));

   for (unsigned i = 0; i < tree_contexts(top); i++)
      link_context(tree_context(top, i));

   link_exec();

   link_args_end();

   if (native)
      link_native(top);
}

void link_optimise_en(bool en)
{
   optimise = en;
}

void link_native_en(bool en)
{
   native = en;
}
