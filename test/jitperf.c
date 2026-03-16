//
//  Copyright (C) 2022-2026  Nick Gasson
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
#include "diag.h"
#include "gitsha.h"
#include "ident.h"
#include "jit/jit.h"
#include "lib.h"
#include "lower.h"
#include "mir/mir-unit.h"
#include "object.h"
#include "option.h"
#include "phase.h"
#include "printf.h"
#include "rt/mspace.h"
#include "rt/rt.h"
#include "scan.h"
#include "thread.h"

#include <assert.h>
#include <errno.h>
#include <math.h>
#include <getopt.h>
#include <stdlib.h>
#include <string.h>

#define ITERATIONS 5
#define BASELINE_FILE "baseline.txt"

typedef struct {
   ident_t  name;
   char    *file;
   double   ops_sec;
   double   usec_op;
} result_t;

typedef A(result_t) result_array_t;

const char copy_string[] = "";
const char version_string[] = "";

static double mean(double *arr, int len)
{
   double r = 0.0;
   for (int i = 0; i < len; i++)
      r += arr[i];
   return r / len;
}

static void print_result(double ops_sec, double usec_op)
{
   if (usec_op < 1.0)
      printf("%.1f ops/s; %.1f ns/op\n", ops_sec, usec_op * 1000.0);
   else
      printf("%.1f ops/s; %.1f us/op\n", ops_sec, usec_op);
}

static const result_t *find_baseline_result(const result_array_t *results,
                                            const result_t *ref)
{
   if (results == NULL)
      return NULL;

   for (int i = 0; i < results->count; i++) {
      if (results->items[i].name == ref->name
          && strcmp(results->items[i].file, ref->file) == 0)
         return &(results->items[i]);
   }

   return NULL;
}

static bool load_baseline(result_array_t *results)
{
   FILE *f = fopen(BASELINE_FILE, "r");
   if (f == NULL && errno == ENOENT)
      return false;
   else if (f == NULL)
      fatal_errno("failed to open baseline file %s", BASELINE_FILE);

   char *line = NULL;
   size_t linesz = 0;
   int line_no = 0;
   while (getline(&line, &linesz, f) != -1) {
      line_no++;

      if (line[0] == '#' || line[0] == '\n' || line[0] == '\0')
         continue;

      char *saveptr = NULL;
      char *name = strtok_r(line, " \t\r\n", &saveptr);
      char *file = strtok_r(NULL, " \t\r\n", &saveptr);
      char *ops = strtok_r(NULL, " \t\r\n", &saveptr);
      char *usec = strtok_r(NULL, " \t\r\n", &saveptr);
      if (name == NULL || file == NULL || ops == NULL || usec == NULL)
         fatal("invalid baseline file %s at line %d", BASELINE_FILE, line_no);

      char *endptr = NULL;
      const double ops_sec = strtod(ops, &endptr);
      if (endptr == NULL || *endptr != '\0')
         fatal("invalid ops/s value in baseline file %s at line %d",
               BASELINE_FILE, line_no);

      endptr = NULL;
      const double usec_op = strtod(usec, &endptr);
      if (endptr == NULL || *endptr != '\0')
         fatal("invalid us/op value in baseline file %s at line %d",
               BASELINE_FILE, line_no);

      result_t result = (result_t){
         .name    = ident_new(name),
         .file    = xstrdup(basename(file)),
         .ops_sec = ops_sec,
         .usec_op = usec_op,
      };
      APUSH(*results, result);
   }

   free(line);

   fclose(f);
   return true;
}

static void save_baseline(const result_array_t *results)
{
   FILE *f = fopen(BASELINE_FILE, "w");
   if (f == NULL)
      fatal_errno("failed to write baseline file %s", BASELINE_FILE);

   fprintf(f, "# jitperf baseline %s\n", GIT_SHA);
   for (int i = 0; i < results->count; i++) {
      const result_t *r = &(results->items[i]);
      fprintf(f, "%s %s %.1f %.1f\n", istr(r->name), r->file, r->ops_sec,
              r->usec_op);
   }

   fclose(f);
}

static void print_delta(double current, double baseline, bool higher_is_better)
{
   const double delta = ((current - baseline) / baseline) * 100.0;
   if (fabs(delta) < 1.0)
      nvc_printf("$#236$%+5.1f%%$$", delta);
   else {
      const double cmp = higher_is_better ? delta : -delta;
      if (cmp > 0.0)
         nvc_printf("$green$%+5.1f%%$$", delta);
      else
         nvc_printf("$red$%+5.1f%%$$", delta);
   }
}

static void print_summary(const result_array_t *results,
                          const result_array_t *baseline)
{
   double max = 0.0;
   int width = 0;
   for (int i = 0; i < results->count; i++) {
      width = MAX(width, ident_len(results->items[i].name));
      max = MAX(max, results->items[i].usec_op);
   }

   const int scale = max < 10.0 ? 1000 : 1;
   nvc_printf("$bold$%*s  %10s %6s %8s %6s$$\n",
              width, "", "ops/s", "", scale == 1000 ? "ns/op" : "us/op", "");

   for (int i = 0; i < results->count; i++) {
      const result_t *r = &(results->items[i]);
      const result_t *b = find_baseline_result(baseline, r);

      nvc_printf("$bold$%-*s$$  %10.1f ", width, istr(r->name), r->ops_sec);
      if (b != NULL)
         print_delta(r->ops_sec, b->ops_sec, true);
      else
         printf("%6s", "");

      printf(" %8.1f ", r->usec_op * scale);
      if (b != NULL)
         print_delta(r->usec_op, b->usec_op, false);
      else
         printf("%6s", "");
      printf("\n");
   }
}

static result_t run_benchmark(tree_t pack, tree_t proc, unit_registry_t *ur,
                              mir_context_t *mc)
{
   nvc_printf("$!magenta$## %s$$\n\n", istr(tree_ident(proc)));

   ident_t name = tree_ident2(proc);

   jit_t *j = jit_new(ur, mc);

#if defined HAVE_LLVM && 1
   jit_register_llvm_plugin(j);
#elif defined ARCH_X86_64 && 0
   jit_register_native_plugin(j);
#endif

   jit_handle_t hpack = jit_compile(j, tree_ident(pack));
   jit_scalar_t context = { .pointer = jit_link(j, hpack) };

   jit_handle_t hproc = jit_compile(j, name);
   if (hproc == JIT_HANDLE_INVALID)
      fatal("cannot compile unit %s", istr(name));

   double ops_sec[ITERATIONS + 1], usec_op[ITERATIONS + 1];

   tlab_t *tlab = tlab_acquire(jit_get_mspace(j));

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
         jit_scalar_t dummy = { .integer = 0 };
         if (!jit_fastcall(j, hproc, &result, dummy, context, tlab))
            fatal("error in benchmark subprogram");

         tlab_reset(tlab);
      }

      const double elapsed = (now - start) / 1e6;
      ops_sec[trial] = iters / elapsed;
      usec_op[trial] = (double)(now - start) / iters;

      print_result(ops_sec[trial], usec_op[trial]);
      fflush(stdout);
   }

   tlab_release(tlab);

   jit_free(j);

   printf("\n");

   const char *file = loc_file_str(tree_loc(proc));

   return (result_t){
      .name    = tree_ident(proc),
      .file    = xstrdup(basename(file)),
      .ops_sec = mean(ops_sec + 1, ITERATIONS),
      .usec_op = mean(usec_op + 1, ITERATIONS),
   };
}

static void find_benchmarks(tree_t pack, const char *filter,
                            unit_registry_t *ur, mir_context_t *mc,
                            result_array_t *results)
{
   ident_t test_i = ident_new("TEST_");

   const int ndecls = tree_decls(pack);
   for (int i = 0; i < ndecls; i++) {
      tree_t d = tree_decl(pack, i);
      if (!is_subprogram(d))
         continue;

      ident_t id = tree_ident(d);
      if (!ident_starts_with(id, test_i))
         continue;
      else if (filter != NULL && strcasestr(istr(id), filter) == NULL)
         continue;

      result_t r = run_benchmark(pack, d, ur, mc);
      APUSH(*results, r);
   }
}

static void free_results(result_array_t *results)
{
   for (int i = 0; i < results->count; i++)
      free(results->items[i].file);

   ACLEAR(*results);
}

static vhdl_standard_t parse_standard(const char *str)
{
   char *eptr = NULL;
   const int year = strtol(str, &eptr, 10);
   if ((eptr != NULL) && (*eptr == '\0')) {
      switch (year) {
      case 1987:
      case 87:
         fatal("VHDL standard 1076-1987 is not supported");
      case 1993:
      case 93:
         return STD_93;
      case 2000:
      case 0:
         return STD_00;
      case 2002:
      case 2:
         return STD_02;
      case 2008:
      case 8:
         return STD_08;
      case 2019:
      case 19:
         return STD_19;
      }
   }

   fatal("invalid standard revision: %s (allowed 1993, 2000, 2002, "
         "2008, 2019)", str);
}

static void usage(void)
{
   printf("Usage: jitperf [OPTION]... [FILE]...\n"
          "\n"
          "     --baseline\t\t Save current results as baseline\n"
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

   opt_set_str(OPT_GC_VERBOSE, getenv("NVC_GC_VERBOSE"));
   opt_set_int(OPT_RELAXED, 1);

   _std_standard_init();
   _std_env_init();
   _nvc_sim_pkg_init();

   static struct option long_options[] = {
      { "baseline", no_argument, 0, 'b' },
      { "std", required_argument, 0, 's' },
      { 0, 0, 0, 0 }
   };

   opterr = 0;

   const char *filter = NULL;
   bool baseline_loaded = false;
   bool write_baseline_file = false;
   int c, index = 0;
   const char *spec = "L:hf:i";
   while ((c = getopt_long(argc, argv, spec, long_options, &index)) != -1) {
      switch (c) {
      case 0:
         // Set a flag
         break;
      case 'b':
         write_baseline_file = true;
         break;
      case 'L':
         lib_add_search_path(optarg);
         break;
      case 'h':
         usage();
         return 0;
      case 's':
         set_standard(parse_standard(optarg));
         break;
      case 'f':
         filter = optarg;
         break;
      case 'i':
         opt_set_int(OPT_JIT_THRESHOLD, 0);
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

   mir_context_t *mc = mir_context_new();
   unit_registry_t *ur = unit_registry_new(mc);
   jit_t *jit = jit_new(ur, mc);

   result_array_t results = AINIT;
   result_array_t baseline = AINIT;

   for (int i = optind; i < argc; i++) {
      nvc_printf("$!cyan$--\n-- %s\n--$$\n\n", argv[i]);

      input_from_file(argv[i]);

      tree_t unit, pack = NULL;
      while ((unit = parse())) {
         if (error_count() > 0)
            return EXIT_FAILURE;

         lib_put(work, unit);

         simplify_local(unit, jit, ur, mc);
         bounds_check(unit);

         if (error_count() > 0)
            return EXIT_FAILURE;

         if (pack == NULL && tree_kind(unit) == T_PACKAGE)
            pack = unit;
      }

      if (pack == NULL)
         fatal("no package found in %s", argv[i]);

      freeze_global_arena();

      find_benchmarks(pack, filter, ur, mc, &results);
   }

   if (write_baseline_file)
      save_baseline(&results);
   else
      baseline_loaded = load_baseline(&baseline);

   print_summary(&results, baseline_loaded ? &baseline : NULL);

   jit_free(jit);
   unit_registry_free(ur);
   free_results(&baseline);
   free_results(&results);

   return 0;
}
