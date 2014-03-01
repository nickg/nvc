//
//  Copyright (C) 2011-2014  Nick Gasson
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
#include <limits.h>
#include <string.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <sys/stat.h>

#ifdef __CYGWIN__
#include <process.h>
#endif

#define MAX_ARGS          64
#define LINK_NATIVE_BYTES (100 * 1024)

static char **args = NULL;
static int    n_args = 0;
static tree_t linked[MAX_ARGS];
static int    n_linked = 0;
static int    n_linked_bc = 0;

typedef void (*context_fn_t)(lib_t lib, tree_t unit, FILE *deps);

static void link_all_context(tree_t unit, FILE *deps, context_fn_t fn);

__attribute__((format(printf, 1, 2)))
static void link_arg_f(const char *fmt, ...)
{
   va_list ap;
   assert(n_args < MAX_ARGS);
   va_start(ap, fmt);
   args[n_args] = xvasprintf(fmt, ap);
   va_end(ap);
   args[++n_args] = NULL;
}

static void link_product(lib_t lib, ident_t name,
                         const char *prefix, const char *ext)
{
   char path[PATH_MAX];
   lib_realpath(lib, NULL, path, sizeof(path));

   link_arg_f("%s%s/_%s.%s", prefix, path, istr(name), ext);
}

static void link_arg_bc(lib_t lib, ident_t name)
{
   link_product(lib, name, "", "bc");
}

static bool link_needs_body(tree_t pack)
{
   return false;   // TODO
}

static bool link_find_native_library(lib_t lib, tree_t unit, FILE *deps)
{
#ifdef ENABLE_NATIVE
   ident_t name = tree_ident(unit);

   char *so_name = xasprintf("_%s.so", istr(name));

   lib_mtime_t so_mt;
   if (!lib_stat(lib, so_name, &so_mt)) {
      free(so_name);
      return false;
   }

   lib_mtime_t unit_mt = lib_mtime(lib, name);

   if (unit_mt > so_mt) {
      warnf("unit %s has stale native shared library", istr(name));
      free(so_name);
      return false;
   }

   if (deps != NULL) {
      char path[PATH_MAX];
      lib_realpath(lib, so_name, path, sizeof(path));

      fprintf(deps, "%s\n", path);
   }

   free(so_name);
   return true;
#else   // ENABLE_NATIVE
   return false;
#endif  // ENABLE_NATIVE
}

static bool link_already_have(tree_t unit)
{
   for (int i = 0; i < n_linked; i++) {
      if (linked[i] == unit)
         return true;
   }

   return false;
}

static void link_context_bc_fn(lib_t lib, tree_t unit, FILE *deps)
{
   if (!link_find_native_library(lib, unit, deps)) {
      link_arg_bc(lib, tree_ident(unit));
      n_linked_bc++;
   }
}

#ifdef IMPLIB_REQUIRED
static void link_context_cyg_fn(lib_t lib, tree_t unit, FILE *deps)
{
   if (link_find_native_library(lib, unit, deps))
      link_product(lib, tree_ident(unit), "", "a");
}
#endif  // IMPLIB_REQUIRED

static void link_context(tree_t ctx, FILE *deps, context_fn_t fn)
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
      (*fn)(lib, unit, deps);
      linked[n_linked++] = unit;
   }

   link_all_context(unit, deps, fn);

   ident_t body_i = ident_prefix(name, ident_new("body"), '-');
   tree_t body = lib_get(lib, body_i);
   if (body == NULL) {
      if (link_needs_body(unit))
         fatal("missing body for package %s", istr(name));
      else
         return;
   }

   link_all_context(body, deps, fn);

   if (!link_already_have(body)) {
      (*fn)(lib, body, deps);
      linked[n_linked++] = body;
   }
}

static void link_all_context(tree_t unit, FILE *deps, context_fn_t fn)
{
   if (tree_kind(unit) == T_PACK_BODY) {
      tree_t pack = lib_get(lib_work(), ident_strip(tree_ident(unit),
                                                    ident_new("-body")));
      if (pack != NULL) {
         assert(tree_kind(pack) == T_PACKAGE);

         if (pack_needs_cgen(pack) && !link_already_have(pack)) {
            (*fn)(lib_work(), pack, deps);
            linked[n_linked++] = pack;
         }

         link_all_context(pack, NULL, fn);
      }
   }

   const int ncontext = tree_contexts(unit);
   for (int i = 0; i < ncontext; i++)
      link_context(tree_context(unit, i), deps, fn);
}

static ident_t link_elab_final(tree_t top)
{
   return ident_prefix(ident_strip(tree_ident(top), ident_new(".elab")),
                       ident_new("final"), '.');
}

static void link_output(tree_t top, const char *ext)
{
   ident_t product;
   if (tree_kind(top) == T_ELAB)
      product = link_elab_final(top);
   else
      product = tree_ident(top);
   link_product(lib_work(), product, "", ext);
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
   const bool quiet = (getenv("NVC_LINK_QUIET") != NULL);

   if (!quiet) {
      for (int i = 0; i < n_args; i++)
         printf("%s%c", args[i], (i + 1 == n_args ? '\n' : ' '));
   }

#ifdef __CYGWIN__
   int status = spawnv(_P_WAIT, args[0], (const char * const *)args);
   if (status != 0)
      fatal("%s failed with status %d", args[0], status);
#else  // __CYGWIN__
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
#endif  // __CYGWIN__

   n_linked_bc = 0;
   n_linked = 0;
}

#ifdef ENABLE_NATIVE
static void link_assembly(tree_t top)
{
   link_args_begin();

   const char *extra = getenv("NVC_LLC_ARG");

   link_arg_f("%s/llc", LLVM_CONFIG_BINDIR);
   link_arg_f("-relocation-model=pic");
   if (extra != NULL)
      link_arg_f("%s", extra);
   link_output(top, "bc");
#ifdef LLVM_LLC_HAS_OBJ
   link_arg_f("-filetype=obj");
#endif

   link_exec();

   link_args_end();
}
#endif

#ifdef ENABLE_NATIVE
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

#if defined __CYGWIN__
   link_arg_f("-Wl,--export-all-symbols");
   link_product(lib_work(), tree_ident(top), "-Wl,--out-implib=", "a");
#endif

   link_arg_f("-o");

   link_output(top, "so");

#ifdef LLVM_LLC_HAS_OBJ
   link_output(top, "o");
#else
   link_output(top, "s");
#endif

   const char *obj = getenv("NVC_FOREIGN_OBJ");
   if (obj != NULL)
      link_arg_f("%s", obj);

#ifdef IMPLIB_REQUIRED
   const char *cyglib = getenv("NVC_CYG_LIB");
   link_arg_f("-L%s", (cyglib != NULL) ? cyglib : DATADIR);
   link_arg_f("-lnvcimp");

   link_all_context(top, NULL, link_context_cyg_fn);
#endif

   link_exec();

   link_args_end();
}
#endif

static void link_native(tree_t top)
{
#ifdef ENABLE_NATIVE
   link_assembly(top);
   link_shared(top);
#else
   fatal("native code generation is not available on this system");
#endif
}

static void link_opt(tree_t top, const char *input)
{
   link_args_begin();

   link_arg_f("%s/opt", LLVM_CONFIG_BINDIR);
   link_arg_f("-O2");
   link_arg_f("-o");
   link_output(top, "bc");
   if (*input == '\0')
      link_arg_bc(lib_work(), tree_ident(top));
   else
      link_arg_f("%s", input);

   link_exec();
   link_args_end();
}

static void link_tmp_name(tree_t top, char *buf, size_t len)
{
   snprintf(buf, len, "%s/%sXXXXXX", P_tmpdir,
            istr(ident_runtil(tree_ident(top), '.')));

   int fd;
   if ((fd = mkstemp(buf)) < 0)
      fatal_errno("mkstemp");
   else
      close(fd);
}

static FILE *link_deps_file(tree_t top)
{
   char *deps_name = xasprintf("_%s.deps.txt", istr(tree_ident(top)));
   FILE *fp = lib_fopen(lib_work(), deps_name, "w");
   free(deps_name);
   return fp;
}

void link_bc(tree_t top)
{
   link_args_begin();

   link_arg_f("%s/llvm-link", LLVM_CONFIG_BINDIR);

   const bool opt_en = opt_get_int("optimise");

   link_arg_bc(lib_work(), tree_ident(top));

   FILE *deps = link_deps_file(top);
   link_all_context(top, deps, link_context_bc_fn);
   fclose(deps);

   const bool linked_bc = (n_linked_bc > 0);

   char tmp[128] = "";
   if (linked_bc) {
      link_arg_f("-o");
      if (opt_en) {
         link_tmp_name(top, tmp, sizeof(tmp));
         link_arg_f("%s", tmp);
      }
      else
         link_output(top, "bc");

      link_exec();
   }

   link_args_end();

   if (opt_en) {
      link_opt(top, tmp);
      if ((*tmp != '\0') && (unlink(tmp) < 0))
         fatal_errno("unlink");
   }
   else if (!linked_bc) {
      link_args_begin();

      link_arg_f("/bin/mv");
      link_arg_bc(lib_work(), tree_ident(top));
      link_output(top, "bc");

      link_exec();
      link_args_end();
   }

   bool native = false;

   if (opt_get_int("native")) {
      if (!opt_en)
         fatal("optimisation must be enabled for native code generation");
      else
         native = true;
   }
   else if (opt_en) {
      // Use a heuristic to decide if the generated bitcode file is large
      // enough to benefit from native complilation
#ifdef ENABLE_NATIVE
      ident_t final = link_elab_final(top);

      char path[PATH_MAX];
      lib_realpath(lib_work(), NULL, path, sizeof(path));

      const size_t libpath_len = strlen(path);

      checked_sprintf(path + libpath_len, PATH_MAX - libpath_len,
                      "/_%s.bc", istr(final));

      struct stat st;
      if (stat(path, &st) != 0)
         fatal_errno("stat: %s", path);

      native = (st.st_size > LINK_NATIVE_BYTES);
#endif  // ENABLE_NATIVE
   }

   if (native)
      link_native(top);
}

void link_package(tree_t pack)
{
   char *name = xasprintf("_%s.bc", istr(tree_ident(pack)));

   char input[PATH_MAX];
   lib_realpath(lib_work(), name, input, sizeof(input));
   free(name);

   link_opt(pack, input);
   link_native(pack);
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
