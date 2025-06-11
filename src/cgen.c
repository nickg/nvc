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
#include "mir/mir-unit.h"
#include "option.h"
#include "phase.h"
#include "thread.h"
#include "type.h"
#include "vlog/vlog-node.h"

#include <stdlib.h>
#include <string.h>
#include <limits.h>
#include <unistd.h>
#include <ctype.h>

typedef A(ident_t) unit_list_t;
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
   hset_t          *filter;
   unit_registry_t *registry;
} discover_args_t;

#ifdef ENABLE_LLVM
static A(char *) link_args;
static A(char *) cleanup_files = AINIT;
#endif

#define UNITS_PER_JOB 25

// Avoid generating excessively long linker command line
#ifdef __MINGW32__
#define MAX_JOBS 100
#else
#define MAX_JOBS 1000
#endif

static void cgen_find_dependencies(mir_context_t *mc, unit_registry_t *ur,
                                   unit_list_t *units, hset_t *seen,
                                   ident_t name)
{
   mir_unit_t *mu = mir_get_unit(mc, name);
   if (mu == NULL) {
      (void)unit_registry_get(ur, name);
      mu = mir_get_unit(mc, name);
   }

   if (mu == NULL)
      fatal_trace("missing vcode for %s", istr(name));

   const int nlink = mir_count_linkage(mu);
   for (int i = 0; i < nlink; i++) {
      ident_t link = mir_get_linkage(mu, i);
      if (hset_contains(seen, link))
         continue;
      else if (ident_char(link, 0) == '$')
         continue;   // TODO: handle VPI differently
      else {
         APUSH(*units, link);
         hset_insert(seen, link);
      }
   }
}

static void cgen_walk_hier(unit_list_t *units, hset_t *seen, tree_t block,
                           ident_t prefix)
{
   assert(tree_kind(block) == T_BLOCK);

   ident_t unit_name = ident_prefix(prefix, tree_ident(block), '.');
   APUSH(*units, unit_name);
   hset_insert(seen, unit_name);

   tree_t hier = tree_decl(block, 0);
   assert(tree_kind(hier) == T_HIER);

   const int nstmts = tree_stmts(block);
   for (int i = 0; i < nstmts; i++) {
      tree_t s = tree_stmt(block, i);
      switch (tree_kind(s)) {
      case T_BLOCK:
         cgen_walk_hier(units, seen, s, unit_name);
         break;
      case T_PROCESS:
      case T_PSL_DIRECT:
         {
            ident_t proc_name = ident_prefix(unit_name, tree_ident(s), '.');
            APUSH(*units, proc_name);
            hset_insert(seen, proc_name);
         }
         break;
      case T_VERILOG:
         {
            ident_t suffix = well_known(W_SHAPE);
            ident_t shape = ident_prefix(tree_ident2(hier), suffix, '.');
            ident_t sym = ident_prefix(shape, tree_ident(s), '.');
            APUSH(*units, sym);
            hset_insert(seen, sym);
         }
         break;
      default:
         break;
      }
   }
}

#ifdef ENABLE_LLVM
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
#if defined LINKER_PATH
   cgen_link_arg("%s", LINKER_PATH);
   cgen_link_arg("--eh-frame-hdr");
#elif defined SYSTEM_CC
   cgen_link_arg("%s", SYSTEM_CC);
#elif defined BOOTSTRAP_CC
   cgen_link_arg("%s", BOOTSTRAP_CC);
#else
   fatal_trace("configured without external C compiler or linker");
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
      jit_handle_t handle = jit_lazy_compile(jit, job->units.items[i]);
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
                                obj_list_t *objs)
{
   int counter = 0;

   // Adjust units_per_job to ensure that each job has a roughly equal
   // number of units
   const int njobs = (units->count + units_per_job - 1) / units_per_job;
   const int clamped = MIN(njobs, MAX_JOBS);
   units_per_job = (units->count + clamped - 1) / clamped;

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

static void cgen_native(ident_t name, jit_t *jit, unit_list_t *units)
{
   workq_t *wq = workq_new(jit);

   obj_list_t objs = AINIT;
   cgen_partition_jobs(units, wq, istr(name), UNITS_PER_JOB, &objs);

   workq_start(wq);
   workq_drain(wq);

   progress("code generation for %d units", units->count);

   cgen_link(istr(name), objs.items, objs.count);

   for (unsigned i = 0; i < objs.count; i++)
      free(objs.items[i]);
   ACLEAR(objs);

   workq_free(wq);
}
#endif   // ENABLE_LLVM

static void cgen_jit_pack(ident_t name, jit_t *jit, unit_list_t *units)
{
   const char *fname LOCAL = xasprintf("_%s.pack", istr(name));

   FILE *f = lib_fopen(lib_work(), fname, "wb");
   if (f == NULL)
      fatal_errno("fopen: %s", fname);

   jit_write_pack(jit, units->items, units->count, f);
   fclose(f);

   progress("writing JIT pack");
}

void cgen(tree_t top, unit_registry_t *ur, mir_context_t *mc, jit_t *jit,
          cgen_mode_t mode)
{
   assert(tree_kind(top) == T_ELAB);

   ident_t b0_name = tree_ident(tree_stmt(top, 0));
   ident_t work_name = lib_name(lib_work());
   ident_t unit_name = ident_prefix(work_name, b0_name, '.');
   vcode_unit_t vu = unit_registry_get(ur, unit_name);
   if (vu == NULL)
      fatal_trace("missing vcode for %s", istr(unit_name));

   hset_t *seen = hset_new(16);
   unit_list_t units = AINIT;

   cgen_walk_hier(&units, seen, tree_stmt(top, 0), work_name);

   for (int i = 0; i < units.count; i++)
      cgen_find_dependencies(mc, ur, &units, seen, units.items[i]);

   hset_free(seen);
   seen = NULL;

   switch (mode) {
   case CGEN_NATIVE:
#ifdef ENABLE_LLVM
      cgen_native(tree_ident(top), jit, &units);
#else
      fatal("native code generation not enabled in this build");
#endif
      break;
   case CGEN_JIT_PACK:
      cgen_jit_pack(tree_ident(top), jit, &units);
      break;
   }

   ACLEAR(units);
}
