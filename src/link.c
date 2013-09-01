//
//  Copyright (C) 2011-2013  Nick Gasson
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

#define _GNU_SOURCE

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

static char **args = NULL;
static int    n_args = 0;
static tree_t linked[MAX_ARGS];
static int    n_linked = 0;

static void link_all_context(tree_t unit);

__attribute__((format(printf, 1, 2)))
static void link_arg_f(const char *fmt, ...)
{
   va_list ap;
   assert(n_args < MAX_ARGS);
   va_start(ap, fmt);
   if (vasprintf(&args[n_args], fmt, ap) < 0)
      abort();
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

static bool link_already_have(tree_t unit)
{
   for (int i = 0; i < n_linked; i++) {
      if (linked[i] == unit)
         return true;
   }
   return false;
}

static void link_context(tree_t ctx)
{
   ident_t name = tree_ident(ctx);

   lib_t lib = lib_find(istr(ident_until(name, '.')), true, true);
   if (lib == NULL)
      fatal("cannot link library %s", istr(name));

   tree_t unit = lib_get(lib, name);
   if (unit == NULL)
      fatal("cannot find unit %s", istr(name));
   else if (tree_kind(unit) != T_PACKAGE)
      return;

   assert(n_linked < MAX_ARGS - 1);

   if (pack_needs_cgen(unit) && !link_already_have(unit)) {
      link_arg_bc(lib, name);
      linked[n_linked++] = unit;
   }

   ident_t body_i = ident_prefix(name, ident_new("body"), '-');
   tree_t body = lib_get(lib, body_i);
   if (body == NULL) {
      if (link_needs_body(unit))
         fatal("missing body for package %s", istr(name));
      else
         return;
   }

   if (!link_already_have(body)) {
      link_arg_bc(lib, body_i);
      linked[n_linked++] = body;
   }

   link_all_context(unit);
   link_all_context(body);
}

static void link_all_context(tree_t unit)
{
   const int ncontext = tree_contexts(unit);
   for (int i = 0; i < ncontext; i++)
      link_context(tree_context(unit, i));
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

   link_arg_f("%s", SYSTEM_CC);
#if defined __APPLE__
   link_arg_f("-bundle");
   link_arg_f("-flat_namespace");
   link_arg_f("-undefined");
   link_arg_f("dynamic_lookup");
#else
   link_arg_f("-shared");
#endif
   link_arg_f("-o");
#if defined __CYGWIN__
   link_output(top, "dll");
#else
   link_output(top, "so");
#endif
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

   link_arg_f("%s/llvm-link", LLVM_CONFIG_BINDIR);

   bool opt_en = opt_get_int("optimise");

   char tmp[128];
   snprintf(tmp, sizeof(tmp), "%s/%sXXXXXX", P_tmpdir,
            istr(ident_runtil(tree_ident(top), '.')));

   link_arg_f("-o");
   if (opt_en) {
      int fd;
      if ((fd = mkstemp(tmp)) < 0)
         fatal_errno("mkstemp");
      else
         close(fd);
      link_arg_f("%s", tmp);
   }
   else
      link_output(top, "bc");

   link_arg_bc(lib_work(), tree_ident(top));

   link_all_context(top);

   link_exec();
   link_args_end();

   if (opt_en) {
      link_args_begin();

      link_arg_f("%s/opt", LLVM_CONFIG_BINDIR);
      link_arg_f("-O2");
      link_arg_f("-o");
      link_output(top, "bc");
      link_arg_f("%s", tmp);

      link_exec();
      link_args_end();

      if (unlink(tmp) < 0)
         fatal_errno("unlink");
   }

   if (opt_get_int("native"))
      link_native(top);
}

bool pack_needs_cgen(tree_t t)
{
   const int ndecls = tree_decls(t);
   for (int i = 0; i < ndecls; i++) {
      tree_t decl = tree_decl(t, i);
      switch (tree_kind(decl)) {
      case T_VAR_DECL:
      case T_SIGNAL_DECL:
      case T_FILE_DECL:
         return true;
      case T_CONST_DECL:
         if (type_is_array(tree_type(decl)))
            return true;
         break;
      default:
         break;
      }
   }

   return false;
}
