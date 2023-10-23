//
//  Copyright (C) 2011-2021  Nick Gasson
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
#include "common.h"
#include "diag.h"
#include "hash.h"
#include "jit/jit-ffi.h"
#include "jit/jit-llvm.h"
#include "jit/jit.h"
#include "lib.h"
#include "lower.h"
#include "object.h"
#include "option.h"
#include "phase.h"
#include "rt/cover.h"
#include "rt/rt.h"
#include "thread.h"
#include "type.h"
#include "vcode.h"

#include <stdlib.h>
#include <string.h>
#include <limits.h>
#include <unistd.h>
#include <ctype.h>
#include <libgen.h>
#include <sys/stat.h>

#include <llvm-c/Core.h>
#include <llvm-c/ExecutionEngine.h>

typedef A(vcode_unit_t) unit_list_t;
typedef A(char *) obj_list_t;

typedef struct {
   unit_list_t      units;
   char            *obj_path;
   char            *module_name;
   unsigned         index;
   cover_data_t    *cover;
   llvm_obj_t      *obj;
} cgen_job_t;

typedef struct {
   unit_list_t     *units;
   unit_registry_t *registry;
} discover_args_t;

static A(char *) link_args;
static A(char *) cleanup_files = AINIT;

#define UNITS_PER_JOB 25

static void cgen_find_children(vcode_unit_t root, unit_list_t *units)
{
   vcode_select_unit(root);

   const vunit_kind_t kind = vcode_unit_kind();
   if (kind != VCODE_UNIT_INSTANCE && kind != VCODE_UNIT_PROCESS
       && kind != VCODE_UNIT_PROPERTY)
      return;

   for (vcode_unit_t it = vcode_unit_child(root);
        it != NULL;
        it = vcode_unit_next(it)) {
      cgen_find_children(it, units);
   }

   APUSH(*units, root);
}

static bool cgen_is_preload(ident_t name)
{
   const char *preload[] = {
      "STD.STANDARD",
      "STD.TEXTIO",
      "STD.ENV",
      "STD.REFLECTION",
      "IEEE.STD_LOGIC",
      "IEEE.NUMERIC",
      "IEEE.MATH",
      "IEEE.FLOAT",
      "IEEE.FIXED",
      "NVC."
   };
   const char *str = istr(name);
   for (int i = 0; i < ARRAY_LEN(preload); i++) {
      if (strncmp(str, preload[i], strlen(preload[i])) == 0)
         return true;
   }

   return false;
}

static void cgen_add_dependency(ident_t name, unit_registry_t *ur,
                                unit_list_t *list)
{
   vcode_unit_t vu = unit_registry_get(ur, name);
   if (vu == NULL)
      fatal("missing vcode unit %s", istr(name));

   unsigned pos = 0;
   for (; pos < list->count; pos++) {
      if (list->items[pos] == vu)
         break;
   }

   if (pos == list->count)
      APUSH(*list, vu);
}

static void cgen_dep_cb(ident_t name, void *ctx)
{
   discover_args_t *args = ctx;

   if (cgen_is_preload(name))
      return;

   cgen_add_dependency(name, args->registry, args->units);
}

static void cgen_find_units(vcode_unit_t root, unit_registry_t *ur,
                            unit_list_t *units)
{
   cgen_find_children(root, units);

   discover_args_t args = {
      .units = units,
      .registry = ur
   };

   for (unsigned i = 0; i < units->count; i++)
      vcode_walk_dependencies(units->items[i], cgen_dep_cb, &args);
}

static void cleanup_temp_dll(void)
{
   for (int i = 0; i < cleanup_files.count; i++) {
      if (remove(cleanup_files.items[i]) == -1)
         warnf("cannot remove %s: %s", cleanup_files.items[i], last_os_error());

      free(cleanup_files.items[i]);
   }

   ACLEAR(cleanup_files);
}

static void cgen_link_arg(const char *fmt, ...)
{
   va_list ap;
   va_start(ap, fmt);
   char *buf = xvasprintf(fmt, ap);
   va_end(ap);

   APUSH(link_args, buf);
}

static void cgen_linker_setup(void)
{
#ifdef LINKER_PATH
   cgen_link_arg("%s", LINKER_PATH);
   cgen_link_arg("--eh-frame-hdr");
#else
   cgen_link_arg("%s", SYSTEM_CC);
#endif

#if defined __APPLE__
   cgen_link_arg("-bundle");
   cgen_link_arg("-flat_namespace");
   cgen_link_arg("-undefined");
   cgen_link_arg("dynamic_lookup");
#ifdef HAVE_NO_FIXUP_CHAINS
   cgen_link_arg("-Wl,-no_fixup_chains");
#endif
#elif defined __OpenBSD__
   cgen_link_arg("-Bdynamic");
   cgen_link_arg("-shared");
   cgen_link_arg("/usr/lib/crtbeginS.o");
#else
   cgen_link_arg("-shared");
#endif
}

static void cgen_link(const char *module_name, char **objs, int nobjs)
{
   cgen_linker_setup();

   LOCAL_TEXT_BUF tb = tb_new();
   tb_printf(tb, "_%s", module_name);
   if (opt_get_int(OPT_NO_SAVE))
      tb_printf(tb, ".%d", getpid());
   tb_cat(tb, "." DLL_EXT);

   char so_path[PATH_MAX];
   lib_realpath(lib_work(), tb_get(tb), so_path, PATH_MAX);

   if (opt_get_int(OPT_NO_SAVE)) {
      APUSH(cleanup_files, xstrdup(so_path));
      atexit(cleanup_temp_dll);
   }

   cgen_link_arg("-o");
   cgen_link_arg("%s", so_path);

   for (int i = 0; i < nobjs; i++)
      cgen_link_arg("%s", objs[i]);

#if defined LINKER_PATH && defined __OpenBSD__
   // Extra linker arguments to make constructors work on OpenBSD
   cgen_link_arg("-L/usr/lib");
   cgen_link_arg("-lcompiler_rt");
   cgen_link_arg("/usr/lib/crtendS.o");
#endif

#ifdef IMPLIB_REQUIRED
   tb_rewind(tb);
   const char *cyglib = getenv("NVC_IMP_LIB");
   if (cyglib != NULL)
      tb_cat(tb, cyglib);
   else
      get_lib_dir(tb);

   cgen_link_arg("-L%s", tb_get(tb));
   cgen_link_arg("-L%s/nvc", LIBDIR);
   cgen_link_arg("-lnvcimp");

   const char *preload_vers[] = { "93", "93", "93", "93", "08", "19" };
   cgen_link_arg("%s/preload%s.dll", tb_get(tb), preload_vers[standard()]);
#endif

   APUSH(link_args, NULL);

   run_program((const char * const *)link_args.items);

   for (int i = 0; i < nobjs ; i++) {
      if (unlink(objs[i]) != 0)
         fatal_errno("unlink: %s", objs[i]);
   }

   progress("linking shared library");

   for (size_t i = 0; i < link_args.count; i++)
      free(link_args.items[i]);
   ACLEAR(link_args);
}

static void cgen_async_work(void *context, void *arg)
{
   jit_t *jit = context;
   cgen_job_t *job = arg;

   llvm_obj_t *obj = llvm_obj_new(job->module_name);

   if (job->index == 0)
      llvm_add_abi_version(obj);

   for (int i = 0; i < job->units.count; i++) {
      vcode_unit_t vu = job->units.items[i];
      vcode_select_unit(vu);

      jit_handle_t handle = jit_lazy_compile(jit, vcode_unit_name());
      assert(handle != JIT_HANDLE_INVALID);

      llvm_aot_compile(obj, jit, handle);
   }

   llvm_obj_finalise(obj, LLVM_O0);
   llvm_obj_emit(obj, job->obj_path);

   ACLEAR(job->units);
   free(job->module_name);
   free(job);
}

static void cgen_partition_jobs(unit_list_t *units, workq_t *wq,
                                const char *base_name, int units_per_job,
                                tree_t top, obj_list_t *objs)
{
   int counter = 0;

   // Adjust units_per_job to ensure that each job has a roughly equal
   // number of units
   const int njobs = (units->count + units_per_job - 1) / units_per_job;
   units_per_job = (units->count + njobs - 1) / njobs;

   for (unsigned i = 0; i < units->count; i += units_per_job, counter++) {
      char *module_name = xasprintf("%s.%d", base_name, counter);
      char *obj_name LOCAL =
         xasprintf("_%s.%d." LLVM_OBJ_EXT, module_name, getpid());

      char obj_path[PATH_MAX];
      lib_realpath(lib_work(), obj_name, obj_path, sizeof(obj_path));

      cgen_job_t *job = xcalloc(sizeof(cgen_job_t));
      job->module_name = module_name;
      job->obj_path    = xstrdup(obj_path);
      job->index       = counter;

      for (unsigned j = i; j < units->count && j < i + units_per_job; j++)
         APUSH(job->units, units->items[j]);

      APUSH(*objs, job->obj_path);

      workq_do(wq, cgen_async_work, job);
   }
}

void cgen(tree_t top, unit_registry_t *ur, jit_t *jit)
{
   assert(tree_kind(top) == T_ELAB);

   ident_t b0_name = tree_ident(tree_stmt(top, 0));
   ident_t work_name = lib_name(lib_work());
   ident_t unit_name = ident_prefix(work_name, b0_name, '.');
   vcode_unit_t vu = unit_registry_get(ur, unit_name);
   if (vu == NULL)
      fatal_trace("missing vcode for %s", istr(unit_name));

   unit_list_t units = AINIT;
   cgen_find_units(vu, ur, &units);

   LLVMInitializeNativeTarget();
   LLVMInitializeNativeAsmPrinter();

   if (!LLVMIsMultithreaded())
      fatal("LLVM was built without multithreaded support");

   workq_t *wq = workq_new(jit);

   ident_t name = tree_ident(top);

   obj_list_t objs = AINIT;
   cgen_partition_jobs(&units, wq, istr(name), UNITS_PER_JOB, top, &objs);

   workq_start(wq);
   workq_drain(wq);

   progress("code generation for %d units", units.count);

   cgen_link(istr(name), objs.items, objs.count);

   for (unsigned i = 0; i < objs.count; i++)
      free(objs.items[i]);
   ACLEAR(objs);

   LLVMShutdown();

   ACLEAR(units);
   workq_free(wq);
}

static void preload_walk_index(lib_t lib, ident_t ident, int kind, void *ctx)
{
   discover_args_t *args = ctx;

   if (kind != T_PACKAGE && kind != T_PACK_INST)
      return;
   else if (!cgen_is_preload(ident))
      return;

   tree_t unit = lib_get(lib, ident);

   if (is_uninstantiated_package(unit))
      return;

   cgen_add_dependency(ident, args->registry, args->units);

   ident_t helper_suffix[] = {
      ident_new("image"),
      ident_new("value"),
      ident_new("resolved"),
      ident_new("copy"),
      ident_new("new"),
   };

   const int ndecls = tree_decls(unit);
   for (int i = 0; i < ndecls; i++) {
      tree_t d = tree_decl(unit, i);
      switch (tree_kind(d)) {
      case T_FUNC_DECL:
      case T_FUNC_BODY:
      case T_FUNC_INST:
      case T_PROC_DECL:
      case T_PROC_BODY:
      case T_PROC_INST:
         {
            const subprogram_kind_t kind = tree_subkind(d);
            if (kind != S_FOREIGN && !is_open_coded_builtin(kind))
               cgen_add_dependency(tree_ident2(d), args->registry, args->units);
         }
         break;
      case T_PROT_DECL:
         {
            type_t type = tree_type(d);
            ident_t id = type_ident(type);

            cgen_add_dependency(id, args->registry, args->units);

            const int nmeth = tree_decls(d);
            for (int i = 0; i < nmeth; i++) {
               tree_t m = tree_decl(d, i);
               if (is_subprogram(m))
                  cgen_add_dependency(tree_ident2(m), args->registry,
                                      args->units);
            }
         }
         break;
      case T_TYPE_DECL:
         {
            type_t type = tree_type(d);
            ident_t id = type_ident(type);

            for (int i = 0; i < ARRAY_LEN(helper_suffix); i++) {
               ident_t func = ident_prefix(id, helper_suffix[i], '$');
               if (unit_registry_query(args->registry, func))
                  cgen_add_dependency(func, args->registry, args->units);
            }
         }
         break;
      default:
         break;
      }
   }
}

static void preload_dep_cb(ident_t name, void *ctx)
{
   discover_args_t *args = ctx;
   cgen_add_dependency(name, args->registry, args->units);
}

static void preload_do_link(const char *so_name, const char *obj_file)
{
   cgen_linker_setup();

   cgen_link_arg("-o");
   cgen_link_arg("%s", so_name);

   cgen_link_arg("%s", obj_file);

#if defined LINKER_PATH && defined __OpenBSD__
   // Extra linker arguments to make constructors work on OpenBSD
   cgen_link_arg("-L/usr/lib");
   cgen_link_arg("-lcompiler_rt");
   cgen_link_arg("/usr/lib/crtendS.o");
#endif

#ifdef IMPLIB_REQUIRED
   LOCAL_TEXT_BUF tb = tb_new();
   const char *cyglib = getenv("NVC_IMP_LIB");
   if (cyglib != NULL)
      tb_cat(tb, cyglib);
   else
      get_lib_dir(tb);

   cgen_link_arg("-L%s", tb_get(tb));
   cgen_link_arg("-lnvcimp");
#endif

   APUSH(link_args, NULL);

   run_program((const char * const *)link_args.items);

   progress("linking shared library");

   for (size_t i = 0; i < link_args.count; i++)
      free(link_args.items[i]);
   ACLEAR(link_args);
}

void aotgen(const char *outfile, char **argv, int argc)
{
   unit_list_t units = AINIT;
   unit_registry_t *ur = unit_registry_new();

   discover_args_t args = {
      .registry = ur,
      .units    = &units,
   };

   for (int i = 0; i < argc; i++) {
      for (char *p = argv[i]; *p; p++)
         *p = toupper((int)*p);

      lib_t lib = lib_require(ident_new(argv[i]));
      lib_walk_index(lib, preload_walk_index, &args);
   }

   for (unsigned i = 0; i < units.count; i++)
      vcode_walk_dependencies(units.items[i], preload_dep_cb, &args);

   LLVMInitializeNativeTarget();
   LLVMInitializeNativeAsmPrinter();

   jit_t *jit = jit_new(ur);

   progress("initialising");

   llvm_obj_t *obj = llvm_obj_new("preload");
   llvm_add_abi_version(obj);

   for (int i = 0; i < units.count; i++) {
      vcode_unit_t vu = units.items[i];
      vcode_select_unit(vu);

      jit_handle_t handle = jit_lazy_compile(jit, vcode_unit_name());
      assert(handle != JIT_HANDLE_INVALID);

      llvm_aot_compile(obj, jit, handle);
   }

   progress("code generation for %d units", units.count);

   llvm_opt_level_t olevel = opt_get_int(OPT_OPTIMISE);
   llvm_obj_finalise(obj, olevel);

   progress("LLVM module optimisation passes");

   char *objfile LOCAL = nvc_temp_file();
   llvm_obj_emit(obj, objfile);

   progress("native code generation");

   preload_do_link(outfile, objfile);

   if (remove(objfile) != 0)
      warnf("remove: %s: %s", objfile, last_os_error());

   LLVMShutdown();

   ACLEAR(units);
   jit_free(jit);
   unit_registry_free(ur);
}
