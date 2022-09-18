//
//  Copyright (C) 2022  Nick Gasson
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
#include "diag.h"
#include "eval.h"
#include "ident.h"
#include "jit/jit.h"
#include "lib.h"
#include "opt.h"
#include "phase.h"
#include "rt/mspace.h"
#include "scan.h"
#include "thread.h"

#include <getopt.h>
#include <stdlib.h>
#include <string.h>

#define ITERATIONS 5

static double mean(double *arr, int len)
{
   double r = 0.0;
   for (int i = 0; i < len; i++)
      r += arr[i];
   return r / len;
}

static void run_benchmark(tree_t pack, tree_t proc)
{
   color_printf("$!magenta$## %s$$\n\n", istr(tree_ident(proc)));

   ident_t name = tree_ident2(proc);

   jit_t *j = jit_new();

#ifdef LLVM_HAS_LLJIT
   //extern const jit_plugin_t jit_llvm;
   //jit_add_tier(j, 100, &jit_llvm);
#endif

   jit_handle_t hpack = jit_compile(j, tree_ident(pack));
   void *context = jit_link(j, hpack);

   jit_handle_t hproc = jit_compile(j, name);
   if (hproc == JIT_HANDLE_INVALID)
      fatal("cannot compile unit %s", istr(name));

   double ops_sec[ITERATIONS + 1], usec_op[ITERATIONS + 1];

   for (int trial = 0; trial < ITERATIONS + 1; trial++) {
      if (trial == 0)
         printf("Warmup:      ");
      else
         printf("Iteration %d: ", trial);
      fflush(stdout);

      const uint64_t start = get_timestamp_us();
      uint64_t now, iters = 0;
      for (; (now = get_timestamp_us()) < start + 1000000; iters++) {
         jit_scalar_t result;
         if (!jit_try_call(j, name, context, &result, ""))
            fatal("error in benchmark subprogram");
      }

      const double elapsed = (now - start) / 1e6;
      ops_sec[trial] = iters / elapsed;
      usec_op[trial] = (double)(now - start) / iters;

      printf("%.1f ops/s; %.1f us/op\n", ops_sec[trial], usec_op[trial]);
      fflush(stdout);
   }

   color_printf("\n$!green$--> %.1f ops/s; %.1f us/op$$\n\n",
                mean(ops_sec + 1, ITERATIONS), mean(usec_op + 1, ITERATIONS));

   jit_free(j);
}

static void find_benchmarks(tree_t pack, const char *filter)
{
   ident_t test_i = ident_new("TEST_");

   const int ndecls = tree_decls(pack);
   for (int i = 0; i < ndecls; i++) {
      tree_t d = tree_decl(pack, i);
      if (tree_kind(d) != T_PROC_DECL)
         continue;

      ident_t id = tree_ident(d);
      if (ident_starts_with(id, test_i)
          && (filter == NULL || strcasestr(istr(id), filter) != NULL))
         run_benchmark(pack, d);
   }
}

static void set_default_options(void)
{
   opt_set_int(OPT_BOOTSTRAP, 0);
   opt_set_int(OPT_COVER, 0);
   opt_set_int(OPT_UNIT_TEST, 1);
   opt_set_str(OPT_DUMP_VCODE, getenv("NVC_LOWER_VERBOSE"));
   opt_set_int(OPT_IGNORE_TIME, 0);
   opt_set_int(OPT_VERBOSE, 0);
   opt_set_int(OPT_SYNTHESIS, 0);
   opt_set_int(OPT_ERROR_LIMIT, -1);
   opt_set_int(OPT_ARENA_SIZE, 1 << 20);
   opt_set_str(OPT_GC_VERBOSE, getenv("NVC_GC_VERBOSE"));
   opt_set_str(OPT_OBJECT_VERBOSE, getenv("NVC_OBJECT_VERBOSE"));
   opt_set_str(OPT_ELAB_VERBOSE, getenv("NVC_ELAB_VERBOSE"));
   opt_set_str(OPT_EVAL_VERBOSE, getenv("NVC_EVAL_VERBOSE"));
   opt_set_str(OPT_JIT_VERBOSE, getenv("NVC_JIT_VERBOSE"));
   opt_set_int(OPT_HEAP_SIZE, 128 * 1024);
   opt_set_int(OPT_GC_STRESS, getenv("NVC_GC_STRESS") != 0);
   opt_set_int(OPT_RELAXED, 0);
   opt_set_int(OPT_JIT_LOG, getenv("NVC_JIT_LOG") != NULL);
   opt_set_int(OPT_WARN_HIDDEN, 0);
   opt_set_int(OPT_RT_TRACE, 0);
   opt_set_int(OPT_STOP_DELTA, 1000);
   opt_set_int(OPT_RT_STATS, 0);
}

static void usage(void)
{
   printf("Usage: jitperf [OPTION]... [FILE]...\n"
          "\n"
          " -f PATTERN\t\t Only run tests matching PATTERN\n"
          " -L PATH\t\tAdd PATH to library search paths\n"
          "\n");

   LOCAL_TEXT_BUF tb = tb_new();
   lib_print_search_paths(tb);
   printf("Library search paths:%s\n", tb_get(tb));

   printf("\nReport bugs to %s\n", PACKAGE_BUGREPORT);
}

int main(int argc, char **argv)
{
   term_init();
   set_default_options();
   thread_init();
   register_signal_handlers();
   mspace_stack_limit(MSPACE_CURRENT_FRAME);
   intern_strings();

   static struct option long_options[] = {
      { 0, 0, 0, 0 }
   };

   opterr = 0;

   const char *filter = NULL;
   int c, index = 0;
   const char *spec = "L:hf:";
   while ((c = getopt_long(argc, argv, spec, long_options, &index)) != -1) {
      switch (c) {
      case 0:
         // Set a flag
         break;
      case 'L':
         lib_add_search_path(optarg);
         break;
      case 'h':
         usage();
         return 0;
      case 'f':
         filter = optarg;
         break;
      default:
         if (optopt == 0)
            fatal("unrecognised option $bold$%s$$", argv[optind - 1]);
         else
            fatal("unrecognised option $bold$-%c$$", optopt);
      }
   }

   if (optind == argc)
      fatal("usage: %s FILE...", argv[0]);

   lib_t work = lib_tmp("PERF");
   lib_set_work(work);

   eval_t *eval = eval_new(0);

   for (int i = optind; i < argc; i++) {
      color_printf("$!cyan$--\n-- %s\n--$$\n\n", argv[i]);

      input_from_file(argv[i]);

      tree_t unit, pack = NULL;
      while ((unit = parse())) {
         if (error_count() > 0)
            return EXIT_FAILURE;

         lib_put(work, unit);

         simplify_local(unit, eval);
         bounds_check(unit);

         if (error_count() > 0)
            return EXIT_FAILURE;

         if (unit_needs_cgen(unit)) {
            vcode_unit_t vu = lower_unit(unit, NULL);
            lib_put_vcode(work, unit, vu);
         }

         if (pack == NULL && tree_kind(unit) == T_PACKAGE)
            pack = unit;
      }

      if (pack == NULL)
         fatal("no package found in %s", argv[i]);

      find_benchmarks(pack, filter);
   }

   eval_free(eval);

   return 0;
}
