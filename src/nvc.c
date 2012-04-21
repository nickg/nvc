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
#include "parse.h"
#include "phase.h"
#include "rt/rt.h"
#include "rt/slave.h"

#include <unistd.h>
#include <getopt.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <assert.h>
#if defined HAVE_TCL_TCL_H
#include <tcl/tcl.h>
#elif defined HAVE_TCL_H
#include <tcl.h>
#endif

static const char *work_name = "work";

static void set_work_lib(void)
{
   lib_t work = lib_find(work_name, false, false);
   if (work == NULL) {
      if ((work = lib_new(work_name)) == NULL)
         exit(EXIT_FAILURE);
   }

   lib_set_work(work);
}

static ident_t to_unit_name(const char *str)
{
   char *name = strdup(str);
   for (char *p = name; *p; p++)
      *p = toupper((uint8_t)*p);

   ident_t i = ident_prefix(lib_name(lib_work()),
                            ident_new(name), '.');
   free(name);
   return i;
}

static int analyse(int argc, char **argv)
{
   set_work_lib();

   static struct option long_options[] = {
      {"bootstrap", no_argument, 0, 'b'},
      {"dump-llvm", no_argument, 0, 'd'},
      {0, 0, 0, 0}
   };

   int c, index = 0;
   const char *spec = "";
   optind = 1;
   while ((c = getopt_long(argc, argv, spec, long_options, &index)) != -1) {
      switch (c) {
      case 0:
         // Set a flag
         break;
      case '?':
         // getopt_long already printed an error message
         exit(EXIT_FAILURE);
      case 'b':
         opt_set_int("bootstrap", 1);
         break;
      case 'd':
         opt_set_int("dump-llvm", 1);
         break;
      default:
         abort();
      }
   }

   tree_t to_cgen[16];
   int n_cgen = 0;

   for (int i = optind; i < argc; i++) {
      if (!input_from_file(argv[i]))
         return EXIT_FAILURE;

      tree_t unit;
      while ((unit = parse())) {
         if (sem_check(unit)) {
            assert(sem_errors() == 0);
            simplify(unit);

            if (tree_kind(unit) == T_PACK_BODY) {
               assert(n_cgen < ARRAY_LEN(to_cgen));
               to_cgen[n_cgen++] = unit;
            }
         }
         else
            break;
      }

      tree_gc();

      if (parse_errors() + sem_errors() + simplify_errors() > 0)
         return EXIT_FAILURE;

      lib_save(lib_work());
   }

   for (int i = 0; i < n_cgen; i++)
      cgen(to_cgen[i]);

   return EXIT_SUCCESS;
}

static int elaborate(int argc, char **argv)
{
   set_work_lib();

   static struct option long_options[] = {
      {"disable-opt", no_argument, 0, 'o'},
      {"dump-llvm", no_argument, 0, 'd'},
      {"native", no_argument, 0, 'n'},
      {0, 0, 0, 0}
   };

   int c, index = 0;
   const char *spec = "";
   optind = 1;
   while ((c = getopt_long(argc, argv, spec, long_options, &index)) != -1) {
      switch (c) {
      case 'o':
         opt_set_int("optimise", 0);
         break;
      case 'd':
         opt_set_int("dump-llvm", 1);
         break;
      case 'n':
         opt_set_int("native", 1);
         break;
      case 0:
         // Set a flag
         break;
      case '?':
         // getopt_long already printed an error message
         exit(EXIT_FAILURE);
      default:
         abort();
      }
   }

   if (optind == argc)
      fatal("missing top-level unit name");

   ident_t unit_i = to_unit_name(argv[optind]);
   tree_t unit = lib_get(lib_work(), unit_i);
   if (unit == NULL)
      fatal("cannot find unit %s in library %s",
            istr(unit_i), istr(lib_name(lib_work())));

   tree_t e = elab(unit);
   if (e == NULL)
      return EXIT_FAILURE;

   opt(e);

   tree_gc();

   // Save the library now so the code generator can attach temporary
   // meta data to trees. We also want to avoid saving the signal
   // driver lists which will be very large
   lib_save(lib_work());

   driver_extract(e);
   if (driver_errors() > 0)
      return EXIT_FAILURE;

   cgen(e);
   link_bc(e);

   return EXIT_SUCCESS;
}

static uint64_t parse_time(const char *str)
{
   char     unit[4];
   unsigned base;
   uint64_t mult = 1;

   if (sscanf(str, "%u%3s", &base, unit) != 2)
      fatal("invalid time format: %s", str);

   if      (strcmp(unit, "fs") == 0) mult = 1;
   else if (strcmp(unit, "ps") == 0) mult = 1000;
   else if (strcmp(unit, "ns") == 0) mult = 1000000;
   else if (strcmp(unit, "us") == 0) mult = 1000000000;
   else if (strcmp(unit, "ms") == 0) mult = 1000000000000;
   else
      fatal("invalid unit: %s", unit);

   return base * mult;
}

static int run(int argc, char **argv)
{
   set_work_lib();

   static struct option long_options[] = {
      {"trace", no_argument, 0, 't'},
      {"batch", no_argument, 0, 'b'},
      {"command", no_argument, 0, 'c'},
      {"stop-time", required_argument, 0, 's'},
      {"vcd", required_argument, 0, 'v'},
      {"stats", no_argument, 0, 'S'},
      {0, 0, 0, 0}
   };

   enum { BATCH, COMMAND } mode = BATCH;

   uint64_t stop_time = UINT64_MAX;
   const char *vcd_fname = NULL;

   int c, index = 0;
   const char *spec = "bc";
   optind = 1;
   while ((c = getopt_long(argc, argv, spec, long_options, &index)) != -1) {
      switch (c) {
      case 0:
         // Set a flag
         break;
      case '?':
         // getopt_long already printed an error message
         exit(EXIT_FAILURE);
      case 't':
         opt_set_int("rt_trace_en", 1);
         break;
      case 'b':
         mode = BATCH;
         break;
      case 'c':
         mode = COMMAND;
         break;
      case 's':
         stop_time = parse_time(optarg);
         break;
      case 'v':
         vcd_fname = optarg;
         break;
      case 'S':
         opt_set_int("rt-stats", 1);
         break;
      default:
         abort();
      }
   }

   if (optind == argc)
      fatal("missing top-level unit name");

   ident_t top = to_unit_name(argv[optind]);
   ident_t ename = ident_prefix(top, ident_new("elab"), '.');
   tree_rd_ctx_t ctx;
   tree_t e = lib_get_ctx(lib_work(), ename, &ctx);
   if (e == NULL)
      fatal("%s not elaborated", istr(top));
   else if (tree_kind(e) != T_ELAB)
      fatal("%s not suitable top level", istr(top));

   if (vcd_fname != NULL)
      vcd_init(vcd_fname, e);

   if (mode == BATCH)
      rt_batch_exec(e, stop_time, ctx);
   else {
      bool master = slave_fork();
      if (master)
         shell_run(e);
      else
         rt_slave_exec(e, ctx);
   }

   tree_read_end(ctx);
   return EXIT_SUCCESS;
}

static int dump_cmd(int argc, char **argv)
{
   set_work_lib();

   static struct option long_options[] = {
      {"elab", no_argument, 0, 'e'},
      {"body", no_argument, 0, 'b'},
      {0, 0, 0, 0}
   };

   bool add_elab = false, add_body = false;
   int c, index = 0;
   const char *spec = "eb";
   optind = 1;
   while ((c = getopt_long(argc, argv, spec, long_options, &index)) != -1) {
      switch (c) {
      case 0:
         // Set a flag
         break;
      case '?':
         // getopt_long already printed an error message
         exit(EXIT_FAILURE);
      case 'e':
         add_elab = true;
         break;
      case 'b':
         add_body = true;
         break;
      default:
         abort();
      }
   }

   if (optind == argc)
      fatal("missing unit name");

   for (int i = optind; i < argc; i++) {
      ident_t name = to_unit_name(argv[i]);
      if (add_elab)
         name = ident_prefix(name, ident_new("elab"), '.');
      else if (add_body)
         name = ident_prefix(name, ident_new("body"), '-');
      tree_t top = lib_get(lib_work(), name);
      if (top == NULL)
         fatal("%s not analysed", istr(name));
      dump(top);
   }

   return EXIT_SUCCESS;
}

static void set_default_opts(void)
{
   opt_set_int("rt-stats", 0);
   opt_set_int("rt_trace_en", 0);
   opt_set_int("dump-llvm", 0);
   opt_set_int("optimise", 1);
   opt_set_int("native", 0);
   opt_set_int("bootstrap", 0);
}

static void usage(void)
{
   printf("Usage: %s [OPTION]... COMMAND [OPTION]...\n"
          "\n"
          "COMMAND is one of:\n"
          " -a [OPTION]... FILE...\tAnalyse FILEs into work library\n"
          " -e UNIT\t\tElaborate and generate code for UNIT\n"
          " -r UNIT\t\tExecute previously elaborated UNIT\n"
          " --dump UNIT\t\tPrint out previously analysed UNIT\n"
          "\n"
          "Global options may be placed before COMMAND:\n"
          " -v, --version\t\tDisplay version and copyright information\n"
          " -h, --help\t\tDisplay this message and exit\n"
          "     --work=NAME\tUse NAME as the work library\n"
          "\n"
          "Analyse options:\n"
          "     --bootstrap\tAllow compilation of STANDARD package\n"
          "\n"
          "Elaborate options:\n"
          "     --disable-opt\tDisable LLVM optimisations\n"
          "     --dump-llvm\tPrint generated LLVM IR\n"
          "     --native\t\tGenerate native code shared library\n"
          "\n"
          "Run options:\n"
          " -b, --batch\t\tRun in batch mode (default)\n"
          " -c, --command\t\tRun in TCL command line mode\n"
          "     --stats\t\tPrint statistics at end of run\n"
          "     --stop-time=T\tStop after simulation time T (e.g. 5ns)\n"
          "     --trace\t\tTrace simulation events\n"
          "     --vcd=FILE\t\tWrite VCD data to FILE\n"
          "\n"
          "Dump options:\n"
          " -e, --elab\t\tDump an elaborated unit\n"
          " -b, --body\t\tDump package body\n"
          "\n"
          "Report bugs to %s\n",
          PACKAGE, PACKAGE_BUGREPORT);
}

static void version(void)
{
   static const char *copy =
      "Copyright (C) 2011-2012  Nick Gasson\n"
      "This program comes with ABSOLUTELY NO WARRANTY. This is free software, and\n"
      "you are welcome to redistribute it under certain conditions. See the GNU\n"
      "General Public Licence for details.";

#ifdef HAVE_CONFIG_H
   printf("%s (llvm %s; tcl %s)\n",
          PACKAGE_STRING, LLVM_VERSION, TCL_VERSION);
#endif

   puts(copy);
}

int main(int argc, char **argv)
{
   term_init();
   register_trace_signal_handlers();
   set_default_opts();

   static struct option long_options[] = {
      {"help",    no_argument,       0, 'h'},
      {"version", no_argument,       0, 'v'},
      {"work",    required_argument, 0, 'w'},
      {"dump",    no_argument,       0, 'd'},
      {0, 0, 0, 0}
   };

   int c, index = 0;
   const char *spec = "aehr";
   while ((c = getopt_long(argc, argv, spec, long_options, &index)) != -1) {
      switch (c) {
      case 0:
         // Set a flag
         break;
      case 'h':
         usage();
         exit(EXIT_SUCCESS);
      case 'v':
         version();
         exit(EXIT_SUCCESS);
      case 'w':
         work_name = optarg;
         break;
      case 'a':
      case 'e':
      case 'd':
      case 'r':
         // Subcommand options are parsed later
         argc -= (optind - 1);
         argv += (optind - 1);
         goto getopt_out;
      case '?':
         // getopt_long already printed an error message
         exit(EXIT_FAILURE);
      default:
         abort();
      }
   }
 getopt_out:

   switch (c) {
   case 'a':
      return analyse(argc, argv);
   case 'e':
      return elaborate(argc, argv);
   case 'r':
      return run(argc, argv);
   case 'd':
      return dump_cmd(argc, argv);
   default:
      fprintf(stderr, "%s: missing command\n", PACKAGE);
      return EXIT_FAILURE;
   }
}
