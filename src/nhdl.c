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
#include "parse.h"
#include "sem.h"
#include "phase.h"
#include "rt/rt.h"

#include <unistd.h>
#include <getopt.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

static const char *work_name = "work";

static void set_work_lib(void)
{
   lib_t work = lib_find(work_name, false);
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
      *p = toupper(*p);

   ident_t i = ident_prefix(lib_name(lib_work()), ident_new(name));
   free(name);
   return i;
}

static int analyse(int argc, char **argv)
{
   set_work_lib();

   static struct option long_options[] = {
      {"bootstrap", no_argument, 0, 'b'},
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
         sem_bootstrap_en(true);
         break;
      default:
         abort();
      }
   }

   for (int i = optind; i < argc; i++) {
      if (!input_from_file(argv[i]))
         return EXIT_FAILURE;

      tree_t unit;
      while ((unit = parse())) {
         sem_check(unit);
      }
   }

   tree_gc();

   if (parse_errors() > 0 || sem_errors() > 0)
      return EXIT_FAILURE;

   lib_save(lib_work());
   return EXIT_SUCCESS;
}

static int elaborate(int argc, char **argv)
{
   set_work_lib();

   static struct option long_options[] = {
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
   cgen(e);

   lib_save(lib_work());
   return EXIT_SUCCESS;
}

static int run(int argc, char **argv)
{
   set_work_lib();

   static struct option long_options[] = {
      {"trace", no_argument, 0, 't'},
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
      case 't':
         rt_trace_en(true);
         break;
      default:
         abort();
      }
   }

   if (optind == argc)
      fatal("missing top-level unit name");

   rt_exec(to_unit_name(argv[optind]));

   return EXIT_SUCCESS;
}

static int dump(int argc, char **argv)
{
   set_work_lib();

   for (int i = 1; i < argc; i++) {
      ident_t unit_i = to_unit_name(argv[i]);
      tree_t unit = lib_get(lib_work(), unit_i);
      if (unit == NULL)
         fatal("cannot find unit %s in library %s",
               istr(unit_i), istr(lib_name(lib_work())));

      tree_dump(unit);
   }

   return EXIT_SUCCESS;
}

static void usage(void)
{
   printf("Usage: %s [OPTION]... COMMAND [OPTION]...\n"
          "\n"
          "COMMAND is one of:\n"
          " -a [OPTION]... FILE...\tAnalyse FILEs into work library\n"
          " -e UNIT\t\tElaborate and generate code for UNIT\n"
          " -r UNIT\t\tExecute previously elaborated UNIT\n"
          "\n"
          "Global options may be placed before COMMAND:\n"
          " -v, --version\t\tDisplay version and copyright information\n"
          " -h, --help\t\tDisplay this message and exit\n"
          "     --work=NAME\tUse NAME as the work library\n"
          "\n"
          "Analyse options:\n"
          "     --bootstrap\tAllow compilation of STANDARD package\n"
          "\n"
          "Run options:\n"
          "     --trace\t\tTrace simulation events\n"
          "\n"
          "Report bugs to %s\n",
          PACKAGE, PACKAGE_BUGREPORT);
}

static void version(void)
{
   static const char *copy =
      "Copyright (C) 2011  Nick Gasson\n"
      "This program comes with ABSOLUTELY NO WARRANTY. This is free software, and\n"
      "you are welcome to redistribute it under certain conditions. See the GNU\n"
      "General Public Licence for details.";

#ifdef HAVE_CONFIG_H
   puts(PACKAGE_STRING);
#endif

   puts(copy);
}

int main(int argc, char **argv)
{
   register_trace_signal_handlers();

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
   case 'd':
      return dump(argc, argv);
   case 'r':
      return run(argc, argv);
   default:
      fprintf(stderr, "%s: missing command\n", PACKAGE);
      return EXIT_FAILURE;
   }
}
