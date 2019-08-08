//
//  Copyright (C) 2011-2019  Nick Gasson
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
#include "common.h"
#include "vcode.h"
#include "rt/rt.h"

#include <unistd.h>
#include <getopt.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <assert.h>

const char *copy_string =
   "Copyright (C) 2011-2018  Nick Gasson\n"
   "This program comes with ABSOLUTELY NO WARRANTY. This is free software, "
   "and\nyou are welcome to redistribute it under certain conditions. See "
   "the GNU\nGeneral Public Licence for details.";
const char *version_string =
   PACKAGE_STRING " (Using LLVM " LLVM_VERSION ")";

static ident_t top_level = NULL;
static char *top_level_orig = NULL;

static int process_command(int argc, char **argv);

static ident_t to_unit_name(const char *str)
{
   char *name = xstrdup(str);
   for (char *p = name; *p; p++)
      *p = toupper((int)*p);

   ident_t i = ident_prefix(lib_name(lib_work()),
                            ident_new(name), '.');
   free(name);
   return i;
}

static unsigned parse_relax(const char *str)
{
   char *copy = xstrdup(str);
   assert(copy);

   unsigned mask = 0;

   char *token = strtok(copy, ",");
   while (token != NULL) {
      if (strcmp(token, "prefer-explicit") == 0)
         mask |= RELAX_PREFER_EXPLICT;
      else if (strcmp(token, "locally-static") == 0)
         mask |= RELAX_LOCALLY_STATIC;
      else if (strcmp(token, "generic-static") == 0) {
         warnf("relax option 'generic-static' is deprecated: use "
               "'locally-static' instead");
         mask |= RELAX_LOCALLY_STATIC;
      }
      else if (strcmp(token, "universal-bound") == 0)
         mask |= RELAX_UNIVERSAL_BOUND;
      else if (strcmp(token, "pure-files") == 0)
         mask |= RELAX_PURE_FILES;
      else
         fatal("invalid relax option '%s'", token);

      token = strtok(NULL, ",");
   }

   free(copy);
   return mask;
}

static int scan_cmd(int start, int argc, char **argv)
{
   const char *commands[] = {
      "-a", "-e", "-r", "--codegen", "--dump", "--make", "--syntax", "--list"
   };

   for (int i = start; i < argc; i++) {
      for (size_t j = 0; j < ARRAY_LEN(commands); j++) {
         if (strcmp(argv[i], commands[j]) == 0)
            return i;
      }
   }

   return argc;
}

static int analyse(int argc, char **argv)
{
   static struct option long_options[] = {
      { "bootstrap",       no_argument,       0, 'b' },
      { "dump-json",       required_argument, 0, 'j' },
      { "dump-llvm",       no_argument,       0, 'D' },
      { "dump-vcode",      optional_argument, 0, 'v' },
      { "parse-pragmas",   no_argument,       0, 'P' },
      { "prefer-explicit", no_argument,       0, 'p' },   // DEPRECATED
      { "relax",           required_argument, 0, 'R' },
      { 0, 0, 0, 0 }
   };

   const int next_cmd = scan_cmd(2, argc, argv);
   int c, index = 0;
   const char *spec = "";

   while ((c = getopt_long(next_cmd, argv, spec, long_options, &index)) != -1) {
      switch (c) {
      case 0:
         // Set a flag
         break;
      case '?':
         fatal("unrecognised analyse option %s", argv[optind - 1]);
      case 'b':
         opt_set_int("bootstrap", 1);
         break;
      case 'D':
         opt_set_int("dump-llvm", 1);
         break;
      case 'v':
         opt_set_str("dump-vcode", optarg ?: "");
         break;
      case 'j':
         opt_set_str("dump-json", optarg ?: "");
         break;
      case 'P':
         opt_set_int("parse-pragmas", 1);
         break;
      case 'p':
         warnf("the --prefer-explict option is deprecated: use "
               "--relax=prefer-explict instead");
         set_relax_rules(RELAX_PREFER_EXPLICT);
         break;
      case 'R':
         set_relax_rules(parse_relax(optarg));
         break;
      default:
         abort();
      }
   }

   size_t unit_list_sz = 32;
   tree_t *units LOCAL = xmalloc(sizeof(tree_t) * unit_list_sz);
   int n_units = 0;

   for (int i = optind; i < next_cmd; i++) {
      input_from_file(argv[i]);

      tree_t unit;
      while ((unit = parse()) && sem_check(unit))
         ARRAY_APPEND(units, unit, n_units, unit_list_sz);
   }

   for (int i = 0; i < n_units; i++) {
      // Delete any stale vcode to prevent problems in constant folding
      char *vcode LOCAL = vcode_file_name(tree_ident(units[i]));
      lib_delete(lib_work(), vcode);

      simplify(units[i], 0);
      bounds_check(units[i]);
   }

   if (parse_errors() + sem_errors() + bounds_errors() > 0)
      return EXIT_FAILURE;

   if (opt_get_str("dump-json")) {
      dump_json(units, n_units, opt_get_str("dump-json"));
   }

   lib_save(lib_work());

   for (int i = 0; i < n_units; i++) {
      const tree_kind_t kind = tree_kind(units[i]);
      const bool need_cgen = kind == T_PACK_BODY
         || (kind == T_PACKAGE && pack_needs_cgen(units[i]));
      if (need_cgen) {
         vcode_unit_t vu = lower_unit(units[i]);
         char *name LOCAL = vcode_file_name(tree_ident(units[i]));
         fbuf_t *fbuf = lib_fbuf_open(lib_work(), name, FBUF_OUT);
         vcode_write(vu, fbuf);
         fbuf_close(fbuf);
         cgen(units[i], vu);
      }
   }

   argc -= next_cmd - 1;
   argv += next_cmd - 1;

   return argc > 1 ? process_command(argc, argv) : EXIT_SUCCESS;
}

static void elab_verbose(bool verbose, const char *fmt, ...)
{
   if (verbose) {
      va_list ap;
      va_start(ap, fmt);
      char *msg LOCAL = xvasprintf(fmt, ap);
      va_end(ap);

      static nvc_rusage_t last_ru;

      nvc_rusage_t ru;
      nvc_rusage(&ru);
      notef("%s [%ums %+dkB]", msg, ru.ms, ru.rss - last_ru.rss);

      last_ru = ru;
   }
}

static void parse_generic(const char *str)
{
   char *copy LOCAL = xstrdup(str);

   char *split = strchr(copy, '=');
   if (split == NULL || *(split + 1) == '\0' || *copy == '\0')
      fatal("invalid generic specification '%s' (use -gNAME=VALUE)", str);

   *split = '\0';

   for (char *p = copy; *p != '\0'; p++)
      *p = toupper((int)*p);

   elab_set_generic(copy, split + 1);
}

static void set_top_level(char **argv, int next_cmd)
{
   if (optind == next_cmd) {
      if (top_level == NULL)
         fatal("missing top-level unit name");
   }
   else {
      free(top_level_orig);
      top_level_orig = xstrdup(argv[optind]);
      top_level = to_unit_name(top_level_orig);
   }
}

static int elaborate(int argc, char **argv)
{
   static struct option long_options[] = {
      { "disable-opt", no_argument,       0, 'o' },    // DEPRECATED
      { "dump-llvm",   no_argument,       0, 'd' },
      { "dump-vcode",  optional_argument, 0, 'v' },
      { "native",      no_argument,       0, 'n' },    // DEPRECATED
      { "cover",       no_argument,       0, 'c' },
      { "verbose",     no_argument,       0, 'V' },
      { 0, 0, 0, 0 }
   };

   const int next_cmd = scan_cmd(2, argc, argv);
   bool verbose = false;
   int c, index = 0;
   const char *spec = "Vg:O:";
   while ((c = getopt_long(next_cmd, argv, spec, long_options, &index)) != -1) {
      switch (c) {
      case 'o':
         warnf("The '--disable-opt' option is deprecated: use '-O0' instead");
         opt_set_int("optimise", 0);
         break;
      case 'O':
         {
            char *eptr;
            const int level = strtoul(optarg, &eptr, 10);
            if (level > 3)
               fatal("Invalid optimisation level %s", optarg);
            opt_set_int("optimise", level);
         }
         break;
      case 'd':
         opt_set_int("dump-llvm", 1);
         break;
      case 'v':
         opt_set_str("dump-vcode", optarg ?: "");
         break;
      case 'n':
         warnf("--native is now a global option: place before the -e command");
         break;
      case 'c':
         opt_set_int("cover", 1);
         break;
      case 'V':
         verbose = true;
         opt_set_int("verbose", 1);
         break;
      case 'g':
         parse_generic(optarg);
         break;
      case 0:
         // Set a flag
         break;
      case '?':
         fatal("unrecognised elaborate option %s", argv[optind - 1]);
      default:
         abort();
      }
   }

   set_top_level(argv, next_cmd);

   elab_verbose(verbose, "initialising");

   tree_t unit = lib_get(lib_work(), top_level);
   if (unit == NULL)
      fatal("cannot find unit %s in library %s",
            istr(top_level), istr(lib_name(lib_work())));

   elab_verbose(verbose, "loading top-level unit");

   tree_t e = elab(unit);
   if (e == NULL)
      return EXIT_FAILURE;

   elab_verbose(verbose, "elaborating design");

   group_nets(e);
   elab_verbose(verbose, "grouping nets");

   // Save the library now so the code generator can attach temporary
   // meta data to trees
   lib_save(lib_work());
   elab_verbose(verbose, "saving library");

   vcode_unit_t vu = lower_unit(e);
   elab_verbose(verbose, "generating intermediate code");

   cgen(e, vu);
   elab_verbose(verbose, "generating LLVM");

   argc -= next_cmd - 1;
   argv += next_cmd - 1;

   return argc > 1 ? process_command(argc, argv) : EXIT_SUCCESS;
}

// DEPRECATED
static int codegen(int argc, char **argv)
{
   warnf("The --codegen option is deprecated, pass global option --native "
         "when analysing instead");

   static struct option long_options[] = {
      { 0, 0, 0, 0 }
   };

   const int next_cmd = scan_cmd(2, argc, argv);
   int c, index = 0;
   const char *spec = "";
   while ((c = getopt_long(next_cmd, argv, spec, long_options, &index)) != -1) {
      switch (c) {
      case 0:
         // Set a flag
         break;
      case '?':
         fatal("unrecognised codegen option %s", argv[optind - 1]);
      default:
         abort();
      }
   }

   set_top_level(argv, next_cmd);

   tree_t pack = lib_get(lib_work(), top_level);
   if (pack == NULL)
      fatal("cannot find unit %s in library %s",
            istr(top_level), istr(lib_name(lib_work())));

   if (tree_kind(pack) != T_PACKAGE)
      fatal("this command can only be used with packages");

   if (pack_needs_cgen(pack))
      /* NOP */;

   ident_t body_i = ident_prefix(top_level, ident_new("body"), '-');
   tree_t body = lib_get(lib_work(), body_i);
   if (body != NULL)
      /* NOP */;

   argc -= next_cmd - 1;
   argv += next_cmd - 1;

   return argc > 1 ? process_command(argc, argv) : EXIT_SUCCESS;
}

static uint64_t parse_time(const char *str)
{
   char     unit[4];
   unsigned base;
   uint64_t mult = 1;

   if (sscanf(str, "%u%3s", &base, unit) != 2)
      fatal("invalid time format: %s", str);

   if      (strcmp(unit, "fs") == 0)  mult = 1;
   else if (strcmp(unit, "ps") == 0)  mult = 1000;
   else if (strcmp(unit, "ns") == 0)  mult = 1000000;
   else if (strcmp(unit, "us") == 0)  mult = 1000000000;
   else if (strcmp(unit, "ms") == 0)  mult = 1000000000000;
   else if (strcmp(unit, "sec") == 0) mult = 1000000000000000;
   else
      fatal("invalid unit: %s", unit);

   return base * mult;
}

static int parse_int(const char *str)
{
   char *eptr = NULL;
   int n = strtol(str, &eptr, 0);
   if ((eptr == NULL) || (*eptr != '\0'))
      fatal("invalid integer: %s", str);
   return n;
}

static rt_severity_t parse_severity(const char *str)
{
   if (strcasecmp(str, "note") == 0)
      return SEVERITY_NOTE;
   else if (strcasecmp(str, "warning") == 0)
      return SEVERITY_WARNING;
   else if (strcasecmp(str, "error") == 0)
      return SEVERITY_ERROR;
   else if (strcasecmp(str, "failure") == 0)
      return SEVERITY_FAILURE;
   else
      fatal("invalid severity level: %s", str);
}

static int run(int argc, char **argv)
{
   static struct option long_options[] = {
      { "trace",         no_argument,       0, 't' },
      { "profile",       no_argument,       0, 'p' },
      { "stop-time",     required_argument, 0, 's' },
      { "stats",         no_argument,       0, 'S' },
      { "wave",          optional_argument, 0, 'w' },
      { "stop-delta",    required_argument, 0, 'd' },
      { "format",        required_argument, 0, 'f' },
      { "include",       required_argument, 0, 'i' },
      { "exclude",       required_argument, 0, 'e' },
      { "exit-severity", required_argument, 0, 'x' },
#if ENABLE_VHPI
      { "load",          required_argument, 0, 'l' },
      { "vhpi-trace",    no_argument,       0, 'T' },
#endif
      { 0, 0, 0, 0 }
   };

   enum { LXT, FST, VCD} wave_fmt = FST;

   uint64_t stop_time = UINT64_MAX;
   const char *wave_fname = NULL;
   const char *vhpi_plugins = NULL;

   static bool have_run = false;
   if (have_run)
      fatal("multiple run commands are not supported");

   have_run = true;

   const int next_cmd = scan_cmd(2, argc, argv);

   int c, index = 0;
   const char *spec = "w::l:";
   while ((c = getopt_long(next_cmd, argv, spec, long_options, &index)) != -1) {
      switch (c) {
      case 0:
         // Set a flag
         break;
      case '?':
         fatal("unrecognised run option %s", argv[optind - 1]);
      case 't':
         opt_set_int("rt_trace_en", 1);
         break;
      case 'p':
         opt_set_int("rt_profile", 1);
         break;
      case 'T':
         opt_set_int("vhpi_trace_en", 1);
         break;
      case 's':
         stop_time = parse_time(optarg);
         break;
      case 'f':
         if (strcmp(optarg, "vcd") == 0)
            wave_fmt = VCD;
         else if (strcmp(optarg, "fst") == 0)
            wave_fmt = FST;
         else if (strcmp(optarg, "lxt") == 0)
            wave_fmt = LXT;
         else
            fatal("invalid waveform format: %s", optarg);
         break;
      case 'S':
         opt_set_int("rt-stats", 1);
         break;
      case 'w':
         if (optarg == NULL)
            wave_fname = "";
         else
            wave_fname = optarg;
         break;
      case 'd':
         opt_set_int("stop-delta", parse_int(optarg));
         break;
      case 'i':
         wave_include_glob(optarg);
         break;
      case 'e':
         wave_exclude_glob(optarg);
         break;
      case 'l':
         vhpi_plugins = optarg;
         break;
      case 'x':
         rt_set_exit_severity(parse_severity(optarg));
         break;
      default:
         abort();
      }
   }

   set_top_level(argv, next_cmd);

   ident_t ename = ident_prefix(top_level, ident_new("elab"), '.');
   tree_t e = lib_get(lib_work(), ename);
   if (e == NULL)
      fatal("%s not elaborated", istr(top_level));
   else if (tree_kind(e) != T_ELAB)
      fatal("%s not suitable top level", istr(top_level));

   if (wave_fname != NULL) {
      const char *name_map[] = { "LXT", "FST", "VCD" };
      const char *ext_map[]  = { "lxt", "fst", "vcd" };
      char *tmp LOCAL = NULL;

      if (*wave_fname == '\0') {
         tmp = xasprintf("%s.%s", top_level_orig, ext_map[wave_fmt]);
         wave_fname = tmp;
         notef("writing %s waveform data to %s", name_map[wave_fmt], tmp);
      }

      wave_include_file(argv[optind]);

      switch (wave_fmt) {
      case LXT:
         lxt_init(wave_fname, e);
         break;
      case VCD:
         vcd_init(wave_fname, e);
         break;
      case FST:
         fst_init(wave_fname, e);
         break;
      }
   }

   rt_start_of_tool(e);

   if (vhpi_plugins != NULL)
      vhpi_load_plugins(e, vhpi_plugins);

   rt_restart(e);
   rt_run_sim(stop_time);
   rt_end_of_tool(e);

   argc -= next_cmd - 1;
   argv += next_cmd - 1;

   return argc > 1 ? process_command(argc, argv) : EXIT_SUCCESS;
}

static int make_cmd(int argc, char **argv)
{
   static struct option long_options[] = {
      { "deps-only", no_argument, 0, 'd' },
      { "posix",     no_argument, 0, 'p' },
      { 0, 0, 0, 0 }
   };

   const int next_cmd = scan_cmd(2, argc, argv);
   int c, index = 0;
   const char *spec = "";
   while ((c = getopt_long(next_cmd, argv, spec, long_options, &index)) != -1) {
      switch (c) {
      case 0:
         // Set a flag
         break;
      case '?':
         fatal("unrecognised make option %s", argv[optind - 1]);
      case 'd':
         opt_set_int("make-deps-only", 1);
         break;
      case 'p':
         opt_set_int("make-posix", 1);
         break;
      default:
         abort();
      }
   }

   const int count = next_cmd - optind;
   tree_t *targets = xmalloc(count * sizeof(tree_t));

   lib_t work = lib_work();

   for (int i = optind; i < next_cmd; i++) {
      ident_t name = to_unit_name(argv[i]);
      ident_t elab = ident_prefix(name, ident_new("elab"), '.');
      if ((targets[i - optind] = lib_get(work, elab)) == NULL) {
         if ((targets[i - optind] = lib_get(work, name)) == NULL)
            fatal("cannot find unit %s in library %s",
                  istr(name), istr(lib_name(work)));
      }
   }

   make(targets, count, stdout);

   argc -= next_cmd - 1;
   argv += next_cmd - 1;

   return argc > 1 ? process_command(argc, argv) : EXIT_SUCCESS;
}

static void list_walk_fn(ident_t ident, int kind, void *context)
{
   const char *pretty = "???";
   switch (kind) {
   case T_ELAB: pretty = "Elaborated"; break;
   case T_ARCH: pretty = "Architecture"; break;
   case T_ENTITY: pretty = "Entity"; break;
   case T_PACKAGE: pretty = "Package"; break;
   case T_PACK_BODY: pretty = "Package body"; break;
   case T_CONFIGURATION: pretty = "Configuration"; break;
   }

   printf("%-30s  : %s\n", istr(ident), pretty);
}

static int list_cmd(int argc, char **argv)
{
   static struct option long_options[] = {
      { 0, 0, 0, 0 }
   };

   const int next_cmd = scan_cmd(2, argc, argv);
   int c, index = 0;
   const char *spec = "";
   while ((c = getopt_long(next_cmd, argv, spec, long_options, &index)) != -1) {
      switch (c) {
      case 0:
         // Set a flag
         break;
      case '?':
         fatal("unrecognised list option %s", argv[optind - 1]);
      default:
         abort();
      }
   }

   lib_walk_index(lib_work(), list_walk_fn, NULL);

   argc -= next_cmd - 1;
   argv += next_cmd - 1;

   return argc > 1 ? process_command(argc, argv) : EXIT_SUCCESS;
}

static int syntax_cmd(int argc, char **argv)
{
   static struct option long_options[] = {
      { 0, 0, 0, 0 }
   };

   const int next_cmd = scan_cmd(2, argc, argv);
   int c, index = 0;
   const char *spec = "";
   while ((c = getopt_long(next_cmd, argv, spec, long_options, &index)) != -1) {
      switch (c) {
      case 0:
         // Set a flag
         break;
      case '?':
         fatal("unrecognised syntax option %s", argv[optind - 1]);
      default:
         abort();
      }
   }

   for (int i = optind; i < next_cmd; i++) {
      input_from_file(argv[i]);
      (void)parse();
   }

   if (parse_errors() > 0)
      return EXIT_FAILURE;

   argc -= next_cmd - 1;
   argv += next_cmd - 1;

   return argc > 1 ? process_command(argc, argv) : EXIT_SUCCESS;
}

static int dump_cmd(int argc, char **argv)
{
   static struct option long_options[] = {
      { "elab", no_argument, 0, 'E' },
      { "body", no_argument, 0, 'b' },
      { "nets", no_argument, 0, 'n' },
      { 0, 0, 0, 0 }
   };

   const int next_cmd = scan_cmd(2, argc, argv);
   bool add_elab = false, add_body = false, nets = false;
   int c, index = 0;
   const char *spec = "Eb";
   while ((c = getopt_long(argc, argv, spec, long_options, &index)) != -1) {
      switch (c) {
      case 0:
         // Set a flag
         break;
      case '?':
         fatal("unrecognised dump option %s", argv[optind - 1]);
      case 'E':
         add_elab = true;
         break;
      case 'b':
         add_body = true;
         break;
      case 'n':
         add_elab = true;
         nets = true;
         break;
      default:
         abort();
      }
   }

   set_top_level(argv, next_cmd);

   ident_t name = top_level;
   if (add_elab)
      name = ident_prefix(name, ident_new("elab"), '.');
   else if (add_body)
      name = ident_prefix(name, ident_new("body"), '-');

   tree_t top = lib_get(lib_work(), name);
   if (top == NULL)
      fatal("%s not analysed", istr(name));
   (nets ? dump_nets : dump)(top);

   argc -= next_cmd - 1;
   argv += next_cmd - 1;

   return argc > 1 ? process_command(argc, argv) : EXIT_SUCCESS;
}

static void set_default_opts(void)
{
   opt_set_int("rt-stats", 0);
   opt_set_int("rt_trace_en", 0);
   opt_set_int("vhpi_trace_en", 0);
   opt_set_int("dump-llvm", 0);
   opt_set_int("optimise", 2);
   opt_set_int("bootstrap", 0);
   opt_set_str("dump-json", NULL);
   opt_set_int("cover", 0);
   opt_set_int("stop-delta", 1000);
   opt_set_int("unit-test", 0);
   opt_set_int("make-deps-only", 0);
   opt_set_int("make-posix", 0);
   opt_set_str("dump-vcode", NULL);
   opt_set_int("relax", 0);
   opt_set_int("ignore-time", 0);
   opt_set_int("force-init", 0);
   opt_set_int("verbose", 0);
   opt_set_int("rt_profile", 0);
   opt_set_int("synthesis", 0);
   opt_set_int("parse-pragmas", 0);
}

static void usage(void)
{
   printf("Usage: %s [OPTION]... COMMAND [OPTION]...\n"
          "\n"
          "COMMAND is one of:\n"
          " -a [OPTION]... FILE...\t\tAnalyse FILEs into work library\n"
          " -e [OPTION]... UNIT\t\tElaborate and generate code for UNIT\n"
          " -r [OPTION]... UNIT\t\tExecute previously elaborated UNIT\n"
          " --dump [OPTION]... UNIT\tPrint out previously analysed UNIT\n"
          " --list\t\t\t\tPrint all units in the library\n"
          " --make [OPTION]... [UNIT]...\tGenerate makefile to rebuild UNITs\n"
          " --syntax FILE...\t\tCheck FILEs for syntax errors only\n"
          "\n"
          "Global options may be placed before COMMAND:\n"
          "     --force-init\tCreate a library in an existing directory\n"
          " -h, --help\t\tDisplay this message and exit\n"
          "     --ignore-time\tSkip source file timestamp check\n"
          " -L PATH\t\tAdd PATH to library search paths\n"
          "     --map=LIB:PATH\tMap library LIB to PATH\n"
          "     --messages=STYLE\tSelect full or compact message format\n"
          "     --native\t\tGenerate native code shared library\n"
          "     --std=REV\t\tVHDL standard revision to use\n"
          " -v, --version\t\tDisplay version and copyright information\n"
          "     --work=NAME\tUse NAME as the work library\n"
          "\n"
          "Analyse options:\n"
          "     --bootstrap\tAllow compilation of STANDARD package\n"
          "     --parse-pragmas\tEnable parsing comments for pragmas\n"
          "     --relax=RULES\tDisable certain pedantic rule checks\n"
          "\n"
          "Elaborate options:\n"
          "     --cover\t\tEnable code coverage reporting\n"
          "     --dump-llvm\tPrint generated LLVM IR\n"
          "     --dump-vcode\tPrint generated intermediate code\n"
          " -g NAME=VALUE\t\tSet top level generic NAME to VALUE\n"
          " -O0, -O1, -O2, -O3\tSet optimisation level (default is -O2)\n"
          " -V, --verbose\t\tPrint resource usage at each step\n"
          "\n"
          "Run options:\n"
          "     --exclude=GLOB\tExclude signals matching GLOB from wave dump\n"
          "     --exit-severity=S\tExit after assertion failure of severity S\n"
          "     --format=FMT\tWaveform format is either fst or vcd\n"
          "     --include=GLOB\tInclude signals matching GLOB in wave dump\n"
#ifdef ENABLE_VHPI
          "     --load=PLUGIN\tLoad VHPI plugin at startup\n"
#endif
          "     --profile\t\tColect profiling data during run\n"
          "     --stats\t\tPrint statistics at end of run\n"
          "     --stop-delta=N\tStop after N delta cycles (default %d)\n"
          "     --stop-time=T\tStop after simulation time T (e.g. 5ns)\n"
          "     --trace\t\tTrace simulation events\n"
#ifdef ENABLE_VHPI
          "     --vhpi-trace\tTrace VHPI calls and events\n"
#endif
          " -w, --wave=FILE\tWrite waveform data; file name is optional\n"
          "\n"
          "Dump options:\n"
          " -e, --elab\t\tDump an elaborated unit\n"
          " -b, --body\t\tDump package body\n"
          "     --nets\t\tShow mapping from signals to nets\n"
          "\n"
          "Make options:\n"
          "     --deps-only\tOutput dependencies without actions\n"
          "     --posix\t\tStrictly POSIX compliant makefile\n"
          "\n",
          PACKAGE,
          opt_get_int("stop-delta"));

   printf("Library search paths:\n");
   void *token = NULL;
   const char *path;
   while ((path = lib_enum_search_paths(&token)) != NULL)
      printf("  %s\n", path);

   printf("\nReport bugs to %s\n", PACKAGE_BUGREPORT);
}

static vhdl_standard_t parse_standard(const char *str)
{
   char *eptr = NULL;
   const int year = strtol(str, &eptr, 0);
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
      }
   }

   fatal("invalid standard revision: %s (allowed 1993, 2000, 2002, 2008)", str);
}

static message_style_t parse_message_style(const char *str)
{
   if (strcmp(optarg, "full") == 0)
      return MESSAGE_FULL;
   else if (strcmp(optarg, "compact") == 0)
      return MESSAGE_COMPACT;

   fatal("invalid message style '%s' (allowed are 'full' and 'compact')", str);
}

static void parse_library_map(char *str)
{
   char *split = strchr(str, ':');
   if (split == NULL)
      fatal("invalid library map syntax '%s': use NAME:PATH", str);

   *split = '\0';

   if (strcasecmp(str, "work") == 0)
      fatal("use --work option to specify work library name and path");

   lib_add_map(str, split + 1);
}

static void parse_work_name(char *str, const char **name, const char **path)
{
   char *split = strchr(str, ':');
   if (split == NULL)
      *name = *path = str;
   else {
      *split = '\0';
      *name = str;
      *path = split + 1;
   }
}

static int process_command(int argc, char **argv)
{
   static struct option long_options[] = {
      { "dump",    no_argument, 0, 'd' },
      { "codegen", no_argument, 0, 'c' },   // DEPRECATED
      { "make",    no_argument, 0, 'm' },
      { "syntax",  no_argument, 0, 's' },
      { "list",    no_argument, 0, 'l' },
      { 0, 0, 0, 0 }
   };

   opterr = 0;
   optind = 1;

   int index = 0;
   const char *spec = "aer";
   switch (getopt_long(MIN(argc, 2), argv, spec, long_options, &index)) {
   case 'a':
      return analyse(argc, argv);
   case 'e':
      return elaborate(argc, argv);
   case 'r':
      return run(argc, argv);
   case 'd':
      return dump_cmd(argc, argv);
   case 'c':
      return codegen(argc, argv);
   case 'm':
      return make_cmd(argc, argv);
   case 's':
      return syntax_cmd(argc, argv);
   case 'l':
      return list_cmd(argc, argv);
   default:
      fatal("missing command, try %s --help for usage", PACKAGE);
      return EXIT_FAILURE;
   }
}

int main(int argc, char **argv)
{
   term_init();
   set_default_opts();
   intern_strings();

   if (getenv("NVC_GDB") != NULL)
      register_gdb_signal_handlers();
   else
      register_trace_signal_handlers();

   if (is_debugger_running())
      atexit(tree_gc);

   atexit(fbuf_cleanup);

   static struct option long_options[] = {
      { "help",        no_argument,       0, 'h' },
      { "version",     no_argument,       0, 'v' },
      { "work",        required_argument, 0, 'w' },
      { "std",         required_argument, 0, 's' },
      { "messages",    required_argument, 0, 'M' },
      { "native",      no_argument,       0, 'n' },   // DEPRECATED
      { "map",         required_argument, 0, 'p' },
      { "ignore-time", no_argument,       0, 'i' },
      { "force-init",  no_argument,       0, 'f' },
      { 0, 0, 0, 0 }
   };

   opterr = 0;

   const char *work_name = "work";
   const char *work_path = work_name;
   lib_t work = NULL;

   const int next_cmd = scan_cmd(1, argc, argv);
   int c, index = 0;
   const char *spec = "aehrvL:";
   while ((c = getopt_long(next_cmd, argv, spec, long_options, &index)) != -1) {
      switch (c) {
      case 0:
         // Set a flag
         break;
      case 'h':
         usage();
         exit(EXIT_SUCCESS);
      case 'v':
         printf("%s\n%s\n", version_string, copy_string);
         exit(EXIT_SUCCESS);
      case 'w':
         parse_work_name(optarg, &work_name, &work_path);
         break;
      case 'L':
         lib_add_search_path(optarg);
         break;
      case 's':
         set_standard(parse_standard(optarg));
         break;
      case 'M':
         set_message_style(parse_message_style(optarg));
         break;
      case 'p':
         parse_library_map(optarg);
         break;
      case 'i':
         opt_set_int("ignore-time", 1);
         break;
      case 'f':
         opt_set_int("force-init", 1);
         break;
      case 'n':
         warnf("the --native option is deprecated and has no effect");
         break;
      case '?':
         fatal("unrecognised global option %s", argv[optind]);
      default:
         abort();
      }
   }

   work = lib_new(work_name, work_path);
   lib_set_work(work);

   argc -= next_cmd - 1;
   argv += next_cmd - 1;

   return process_command(argc, argv);
}
