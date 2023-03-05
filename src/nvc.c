//
//  Copyright (C) 2011-2023  Nick Gasson
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
#include "common.h"
#include "diag.h"
#include "eval.h"
#include "jit/jit-llvm.h"
#include "jit/jit.h"
#include "lib.h"
#include "lower.h"
#include "option.h"
#include "phase.h"
#include "rt/cover.h"
#include "rt/model.h"
#include "rt/mspace.h"
#include "rt/rt.h"
#include "rt/wave.h"
#include "scan.h"
#include "thread.h"
#include "vhpi/vhpi-util.h"

#include <unistd.h>
#include <getopt.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <assert.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <dirent.h>

#if HAVE_GIT_SHA
#include "gitsha.h"
#define GIT_SHA_ONLY(x) x
#else
#define GIT_SHA_ONLY(x)
#endif

const char *copy_string =
   "Copyright (C) 2011-2023  Nick Gasson\n"
   "This program comes with ABSOLUTELY NO WARRANTY. This is free software, "
   "and\nyou are welcome to redistribute it under certain conditions. See "
   "the GNU\nGeneral Public Licence for details.";
const char *version_string =
   PACKAGE_STRING GIT_SHA_ONLY(" (" GIT_SHA ")")
   LLVM_ONLY(" (Using LLVM " LLVM_VERSION ")") DEBUG_ONLY(" [debug]");

static ident_t top_level = NULL;
static char *top_level_orig = NULL;

static int process_command(int argc, char **argv);
static int parse_int(const char *str);

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

static int scan_cmd(int start, int argc, char **argv)
{
   const char *commands[] = {
      "-a", "-e", "-r", "-c", "--dump", "--make", "--syntax", "--list",
      "--init", "--install", "--print-deps", "--aotgen"
   };

   for (int i = start; i < argc; i++) {
      for (size_t j = 0; j < ARRAY_LEN(commands); j++) {
         if (strcmp(argv[i], commands[j]) == 0)
            return i;
      }
   }

   return argc;
}

static void bad_option(const char *what, char **argv)
{
   if (optopt == 0)
      fatal("unrecognised %s option $bold$%s$$", what, argv[optind - 1]);
   else
      fatal("unrecognised %s option $bold$-%c$$", what, optopt);
}

static void missing_argument(const char *what, char **argv)
{
   fatal("%s option $bold$%s$$ requires an argument", what, argv[optind - 1]);
}

static int analyse(int argc, char **argv)
{
   static struct option long_options[] = {
      { "bootstrap",       no_argument,       0, 'b' },
      { "error-limit",     required_argument, 0, 'l' },
      { "dump-llvm",       no_argument,       0, 'D' },
      { "dump-vcode",      optional_argument, 0, 'v' },
      { "psl",             no_argument,       0, 'P' },
      { "relax",           required_argument, 0, 'X' },
      { "relaxed",         no_argument,       0, 'R' },
      { 0, 0, 0, 0 }
   };

   const int next_cmd = scan_cmd(2, argc, argv);
   int c, index = 0;
   const char *spec = ":";

   while ((c = getopt_long(next_cmd, argv, spec, long_options, &index)) != -1) {
      switch (c) {
      case 0:
         // Set a flag
         break;
      case '?':
         bad_option("analyse", argv);
      case ':':
         missing_argument("analyse", argv);
      case 'b':
         opt_set_int(OPT_BOOTSTRAP, 1);
         break;
      case 'D':
         opt_set_int(OPT_DUMP_LLVM, 1);
         break;
      case 'v':
         opt_set_str(OPT_DUMP_VCODE, optarg ?: "");
         break;
      case 'X':
         warnf("The $bold$--relax=$$ option is deprecated: use the combined "
               "$bold$--relaxed$$ option instead");
         opt_set_int(OPT_RELAXED, 1);
         break;
      case 'l':
         opt_set_int(OPT_ERROR_LIMIT, parse_int(optarg));
         break;
      case 'P':
         opt_set_int(OPT_PSL_COMMENTS, 1);
         break;
      case 'R':
         opt_set_int(OPT_RELAXED, 1);
         break;
      default:
         abort();
      }
   }

   lib_t work = lib_work();
   jit_t *jit = jit_new();
   eval_t *eval = eval_new();  // XXX: share jit

   for (int i = optind; i < next_cmd; i++) {
      input_from_file(argv[i]);

      int base_errors = 0;
      tree_t unit;
      while (base_errors = error_count(), (unit = parse())) {
         if (error_count() == base_errors) {
            lib_put(work, unit);

            simplify_local(unit, eval);
            bounds_check(unit);

            if (error_count() == base_errors && unit_needs_cgen(unit))
               lower_standalone_unit(unit);
         }
         else
            lib_put_error(work, unit);
      }
   }

   eval_free(eval);
   eval = NULL;

   jit_free(jit);

   if (error_count() > 0)
      return EXIT_FAILURE;

   lib_save(work);

   argc -= next_cmd - 1;
   argv += next_cmd - 1;

   return argc > 1 ? process_command(argc, argv) : EXIT_SUCCESS;
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

static void parse_cover_options(const char *str, cover_mask_t *mask,
                                int *array_limit)
{
   static const struct {
      const char *name;
      cover_mask_t mask;
   } options[] = {
      { "statement",             COVER_MASK_STMT                        },
      { "toggle",                COVER_MASK_TOGGLE                      },
      { "branch",                COVER_MASK_BRANCH                      },
      { "expression",            COVER_MASK_EXPRESSION                  },
      { "all",                   COVER_MASK_ALL                         },
      { "count-from-undefined",  COVER_MASK_TOGGLE_COUNT_FROM_UNDEFINED },
      { "count-from-to-z",       COVER_MASK_TOGGLE_COUNT_FROM_TO_Z      },
      { "include-mems",          COVER_MASK_TOGGLE_INCLUDE_MEMS         },
      { "exclude-unreachable",   COVER_MASK_EXCLUDE_UNREACHABLE         }
   };

   for (const char *start = str; ; str++) {
      if (*str == ',' || *str == '\0') {
         if (strncmp(start, "ignore-arrays-from-", 19) == 0)
            *array_limit = parse_int(start + 19);
         else {
            int pos = 0;
            for (; pos < ARRAY_LEN(options); pos++) {
               if (!strncmp(options[pos].name, start, str - start))
                  break;
            }

            if (pos == ARRAY_LEN(options)) {
               diag_t *d = diag_new(DIAG_FATAL, NULL);
               diag_printf(d, "unknown coverage type '%.*s'",
                           (int)(str - start), start);
               diag_hint(d, NULL, "valid coverage types are: statement, "
                         "toggle, branch, and expression");
               diag_hint(d, NULL, "selected coverage types should be "
                         "comma separated e.g $bold$--cover=toggle,branch$$");
               diag_emit(d);
               fatal_exit(EXIT_FAILURE);
            }
            else
               *mask |= options[pos].mask;
         }

         if (*str == '\0')
            break;

         start = str + 1;
      }
   }
}

static int parse_optimise_level(const char *str)
{
   char *eptr;
   const int level = strtoul(optarg, &eptr, 10);
   if (level > 3 || *eptr != '\0')
      fatal("invalid optimisation level %s", optarg);
   return level;
}

static int elaborate(int argc, char **argv)
{
   static struct option long_options[] = {
      { "dump-llvm",       no_argument,       0, 'd' },
      { "dump-vcode",      optional_argument, 0, 'v' },
      { "cover",           optional_argument, 0, 'c' },
      { "cover-spec",      required_argument, 0, 's' },
      { "verbose",         no_argument,       0, 'V' },
      { "no-save",         no_argument,       0, 'N' },
      { "jit",             no_argument,       0, 'j' },
      { 0, 0, 0, 0 }
   };

   bool use_jit = false;
   cover_mask_t cover_mask = 0;
   char *cover_spec_file = NULL;
   int cover_array_limit = 0;
   const int next_cmd = scan_cmd(2, argc, argv);
   int c, index = 0;
   const char *spec = ":Vg:O:j";
   while ((c = getopt_long(next_cmd, argv, spec, long_options, &index)) != -1) {
      switch (c) {
      case 'O':
         opt_set_int(OPT_OPTIMISE, parse_optimise_level(optarg));
         break;
      case 'd':
         opt_set_int(OPT_DUMP_LLVM, 1);
         break;
      case 'v':
         opt_set_str(OPT_DUMP_VCODE, optarg ?: "");
         break;
      case 'c':
         if (optarg)
            parse_cover_options(optarg, &(cover_mask), &(cover_array_limit));
         else
            cover_mask = COVER_MASK_ALL;
         break;
      case 'V':
         opt_set_int(OPT_VERBOSE, 1);
         break;
      case 'N':
         opt_set_int(OPT_NO_SAVE, 1);
         break;
      case 'j':
         use_jit = true;
         break;
      case 'g':
         parse_generic(optarg);
         break;
      case 's':
         cover_spec_file = optarg;
         break;
      case 0:
         // Set a flag
         break;
      case '?':
         bad_option("elaborate", argv);
      case ':':
         missing_argument("elaborate", argv);
      default:
         abort();
      }
   }

   if (use_jit && !opt_get_int(OPT_NO_SAVE))
      fatal("$bold$--jit$$ option requires $bold$--no-save$$");

   set_top_level(argv, next_cmd);

   progress("initialising");

   tree_t unit = lib_get(lib_work(), top_level);
   if (unit == NULL)
      fatal("cannot find unit %s in library %s",
            istr(top_level), istr(lib_name(lib_work())));

   progress("loading top-level unit");

   cover_tagging_t *cover = NULL;
   if (cover_mask != 0) {
      cover = cover_tags_init(cover_mask, cover_array_limit);

      if (cover_spec_file)
         cover_load_spec_file(cover, cover_spec_file);
   }

   tree_t top = elab(unit, cover);
   if (top == NULL)
      return EXIT_FAILURE;

   progress("elaborating design");

   if (cover != NULL) {
      fbuf_t *covdb =  cover_open_lib_file(top, FBUF_OUT, true);
      cover_dump_tags(cover, covdb, COV_DUMP_ELAB, NULL, NULL, NULL, NULL);
      fbuf_close(covdb, NULL);
      progress("dumping coverage data");
   }

   if (error_count() > 0)
      return EXIT_FAILURE;

   if (!opt_get_int(OPT_NO_SAVE)) {
      lib_save(lib_work());
      progress("saving library");
   }

   if (!use_jit)
      AOT_ONLY(cgen(top, cover));

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

static bool parse_on_off(const char *str)
{
   if (strcasecmp(str, "on") == 0)
      return true;
   else if (strcasecmp(str, "off") == 0)
      return false;

   fatal("specifiy 'on' or 'off' instead of '%s'", str);
}

static vhdl_severity_t parse_severity(const char *str)
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

static void ctrl_c_handler(void *arg)
{
#ifdef __SANITIZE_THREAD__
   _Exit(1);
#else
   rt_model_t *model = arg;
   model_interrupt(model);
#endif
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
      { "ieee-warnings", required_argument, 0, 'I' },
      { "exclude",       required_argument, 0, 'e' },
      { "exit-severity", required_argument, 0, 'x' },
      { "dump-arrays",   no_argument,       0, 'a' },
      { "load",          required_argument, 0, 'l' },
      { "vhpi-trace",    no_argument,       0, 'T' },
      { "gtkw",          optional_argument, 0, 'g' },
      { 0, 0, 0, 0 }
   };

   wave_format_t wave_fmt = WAVE_FORMAT_FST;
   uint64_t      stop_time = TIME_HIGH;
   const char   *wave_fname = NULL;
   const char   *gtkw_fname = NULL;
   const char   *vhpi_plugins = NULL;

   static bool have_run = false;
   if (have_run)
      fatal("multiple run commands are not supported");

   have_run = true;

   const int next_cmd = scan_cmd(2, argc, argv);

   int c, index = 0;
   const char *spec = ":w::l:g";
   while ((c = getopt_long(next_cmd, argv, spec, long_options, &index)) != -1) {
      switch (c) {
      case 0:
         // Set a flag
         break;
      case '?':
         bad_option("run", argv);
      case ':':
         missing_argument("run", argv);
      case 't':
         opt_set_int(OPT_RT_TRACE, 1);
         break;
      case 'p':
         opt_set_int(OPT_RT_PROFILE, 1);
         break;
      case 'T':
         opt_set_str(OPT_VHPI_TRACE, "1");
         break;
      case 's':
         stop_time = parse_time(optarg);
         break;
      case 'f':
         if (strcmp(optarg, "vcd") == 0)
            wave_fmt = WAVE_FORMAT_VCD;
         else if (strcmp(optarg, "fst") == 0)
            wave_fmt = WAVE_FORMAT_FST;
         else
            fatal("invalid waveform format: %s", optarg);
         break;
      case 'S':
         opt_set_int(OPT_RT_STATS, 1);
         break;
      case 'w':
         if (optarg == NULL)
            wave_fname = "";
         else
            wave_fname = optarg;
         break;
      case 'g':
         if (optarg == NULL)
            gtkw_fname = "";
         else
            gtkw_fname = optarg;
         break;
      case 'd':
         opt_set_int(OPT_STOP_DELTA, parse_int(optarg));
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
         set_exit_severity(parse_severity(optarg));
         break;
      case 'I':
         opt_set_int(OPT_IEEE_WARNINGS, parse_on_off(optarg));
         break;
      case 'a':
         opt_set_int(OPT_DUMP_ARRAYS, 1);
         break;
      default:
         abort();
      }
   }

   set_top_level(argv, next_cmd);

   ident_t ename = ident_prefix(top_level, well_known(W_ELAB), '.');
   tree_t top = lib_get(lib_work(), ename);
   if (top == NULL)
      fatal("%s not elaborated", istr(top_level));

   wave_dumper_t *dumper = NULL;
   if (wave_fname != NULL) {
      const char *name_map[] = { "FST", "VCD" };
      const char *ext_map[]  = { "fst", "vcd" };
      char *tmp LOCAL = NULL, *tmp2 LOCAL = NULL;

      if (*wave_fname == '\0') {
         tmp = xasprintf("%s.%s", top_level_orig, ext_map[wave_fmt]);
         wave_fname = tmp;
         notef("writing %s waveform data to %s", name_map[wave_fmt], tmp);
      }

      if (gtkw_fname != NULL && *gtkw_fname == '\0') {
         tmp2 = xasprintf("%s.gtkw", top_level_orig);
         gtkw_fname = tmp2;
      }

      wave_include_file(argv[optind]);
      dumper = wave_dumper_new(wave_fname, gtkw_fname, top, wave_fmt);
   }
   else if (gtkw_fname != NULL)
      warnf("$bold$--gtkw$$ option has no effect without $bold$--wave$$");

   if (opt_get_size(OPT_HEAP_SIZE) < 0x100000)
      warnf("recommended heap size is at least 1M");

   jit_t *jit = jit_new();
   jit_enable_runtime(jit, true);
   jit_preload(jit);

   AOT_ONLY(jit_load_dll(jit, tree_ident(top)));

#if defined LLVM_HAS_LLJIT && 1
   jit_register_llvm_plugin(jit);
#elif defined ARCH_X86_64 && 0
   jit_register_native_plugin(jit);
#endif

   _std_standard_init();
   _std_env_init();
   _nvc_sim_pkg_init();

   rt_model_t *model = model_new(top, jit);

   if (vhpi_plugins != NULL)
      vhpi_load_plugins(top, model, vhpi_plugins);

   set_ctrl_c_handler(ctrl_c_handler, model);

   model_reset(model);

   if (dumper != NULL)
      wave_dumper_restart(dumper, model);

   model_run(model, stop_time);

   set_ctrl_c_handler(NULL, NULL);

   const int rc = jit_exit_status(jit);

   if (dumper != NULL)
      wave_dumper_free(dumper);

   model_free(model);
   jit_free(jit);

   argc -= next_cmd - 1;
   argv += next_cmd - 1;

   return rc == 0 && argc > 1 ? process_command(argc, argv) : rc;
}

static int print_deps_cmd(int argc, char **argv)
{
   static struct option long_options[] = {
      { 0, 0, 0, 0 }
   };

   opt_set_int(OPT_MAKE_DEPS_ONLY, 1);

   const int next_cmd = scan_cmd(2, argc, argv);
   int c, index = 0;
   const char *spec = ":";
   while ((c = getopt_long(next_cmd, argv, spec, long_options, &index)) != -1) {
      switch (c) {
      case 0: break;  // Set a flag
      case '?': bad_option("make", argv);
      case ':': missing_argument("make", argv);
      default: abort();
      }
   }

   const int count = next_cmd - optind;
   tree_t *targets = xmalloc_array(count, sizeof(tree_t));

   lib_t work = lib_work();

   for (int i = optind; i < next_cmd; i++) {
      ident_t name = to_unit_name(argv[i]);
      ident_t elab = ident_prefix(name, well_known(W_ELAB), '.');
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

static int make_cmd(int argc, char **argv)
{
   static struct option long_options[] = {
      { "deps-only", no_argument, 0, 'd' },
      { "posix",     no_argument, 0, 'p' },
      { 0, 0, 0, 0 }
   };

   warnf("the $bold$--make$$ command is deprecated and may be repurposed in a "
         "future release");
   notef("use $bold$--print-deps$$ to print Makefile dependencies instead");

   const int next_cmd = scan_cmd(2, argc, argv);
   int c, index = 0;
   const char *spec = ":";
   while ((c = getopt_long(next_cmd, argv, spec, long_options, &index)) != -1) {
      switch (c) {
      case 0:
         // Set a flag
         break;
      case 'd':
         opt_set_int(OPT_MAKE_DEPS_ONLY, 1);
         break;
      case 'p':
         // Does nothing
         break;
      case '?':
         bad_option("make", argv);
      case ':':
         missing_argument("make", argv);
      default:
         abort();
      }
   }

   const int count = next_cmd - optind;
   tree_t *targets = xmalloc_array(count, sizeof(tree_t));

   lib_t work = lib_work();

   for (int i = optind; i < next_cmd; i++) {
      ident_t name = to_unit_name(argv[i]);
      ident_t elab = ident_prefix(name, well_known(W_ELAB), '.');
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

static void list_walk_fn(lib_t lib, ident_t ident, int kind, void *context)
{
   const char *pretty = "???";
   switch (kind) {
   case T_ELAB: pretty = "Elaborated"; break;
   case T_ARCH: pretty = "Architecture"; break;
   case T_ENTITY: pretty = "Entity"; break;
   case T_PACKAGE: pretty = "Package"; break;
   case T_PACK_BODY: pretty = "Package body"; break;
   case T_PACK_INST: pretty = "Instantiated package"; break;
   case T_CONFIGURATION: pretty = "Configuration"; break;
   case T_CONTEXT: pretty = "Context"; break;
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
   const char *spec = ":";
   while ((c = getopt_long(next_cmd, argv, spec, long_options, &index)) != -1) {
      switch (c) {
      case 0:
         // Set a flag
         break;
      case '?':
         bad_option("list", argv);
      case ':':
         missing_argument("list", argv);
      default:
         abort();
      }
   }

   lib_walk_index(lib_work(), list_walk_fn, NULL);

   argc -= next_cmd - 1;
   argv += next_cmd - 1;

   return argc > 1 ? process_command(argc, argv) : EXIT_SUCCESS;
}

static int init_cmd(int argc, char **argv)
{
   static struct option long_options[] = {
      { 0, 0, 0, 0 }
   };

   const int next_cmd = scan_cmd(2, argc, argv);
   int c, index = 0;
   const char *spec = ":";
   while ((c = getopt_long(next_cmd, argv, spec, long_options, &index)) != -1) {
      switch (c) {
      case 0:
         // Set a flag
         break;
      case '?':
         bad_option("init", argv);
      case ':':
         missing_argument("init", argv);
      }
   }

   if (argc != optind)
      fatal("$bold$--init$$ command takes no positional arguments");

   lib_save(lib_work());

   argc -= next_cmd - 1;
   argv += next_cmd - 1;

   return argc > 1 ? process_command(argc, argv) : EXIT_SUCCESS;
}

static void list_packages(void)
{
   LOCAL_TEXT_BUF tb = tb_new();
   get_libexec_dir(tb);

   DIR *dir = opendir(tb_get(tb));
   tb_rewind(tb);

   if (dir != NULL) {
      struct dirent *d;
      while ((d = readdir(dir))) {
         if (strncmp(d->d_name, "install-", 8))
            continue;

         const char *dot = strrchr(d->d_name, '.');
         if (dot == NULL)
            continue;

         const int nchar = dot - d->d_name - 8;
         tb_printf(tb, " %.*s", nchar, d->d_name + 8);
      }

      closedir(dir);
   }

   notef("the following packages can be installed:%s", tb_get(tb));
}

static int install_cmd(int argc, char **argv)
{
   static struct option long_options[] = {
      { "dest", required_argument, 0, 'd' },
      { 0, 0, 0, 0 }
   };

   const int next_cmd = scan_cmd(2, argc, argv);
   int c, index = 0;
   const char *spec = ":";
   while ((c = getopt_long(next_cmd, argv, spec, long_options, &index)) != -1) {
      switch (c) {
      case 0:
         // Set a flag
         break;
      case '?':
         bad_option("install", argv);
      case ':':
         missing_argument("install", argv);
      case 'd':
         setenv("NVC_INSTALL_DEST", optarg , 1);
         break;
      }
   }

   if (argc == optind) {
      errorf("missing argument to $bold$--install$$ command");
      list_packages();
      return EXIT_FAILURE;
   }

   LOCAL_TEXT_BUF tb = tb_new();
   if (get_exe_path(tb))
      setenv("NVC", tb_get(tb), 1);

   for (int i = optind; i < next_cmd; i++) {
      tb_rewind(tb);
      get_libexec_dir(tb);
      tb_printf(tb, DIR_SEP "install-%s.sh", argv[i]);

      struct stat sb;
      if (stat(tb_get(tb), &sb) != 0) {
         errorf("%s is not an executable script", tb_get(tb));
         list_packages();
         return EXIT_FAILURE;
      }

      const char *args[] = {
#ifdef __MINGW32__
         "bash",
#endif
         tb_get(tb),
         NULL
      };
      run_program(args);
   }

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
   const char *spec = ":";
   while ((c = getopt_long(next_cmd, argv, spec, long_options, &index)) != -1) {
      switch (c) {
      case 0:
         // Set a flag
         break;
      case '?':
         bad_option("syntax", argv);
      case ':':
         missing_argument("syntax", argv);
      }
   }

   for (int i = optind; i < next_cmd; i++) {
      input_from_file(argv[i]);
      while (parse())
         ;
   }

   if (error_count() > 0)
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
      { 0, 0, 0, 0 }
   };

   const int next_cmd = scan_cmd(2, argc, argv);
   bool add_elab = false, add_body = false;
   int c, index = 0;
   const char *spec = ":Eb";
   while ((c = getopt_long(next_cmd, argv, spec, long_options, &index)) != -1) {
      switch (c) {
      case 0:
         // Set a flag
         break;
      case '?':
         bad_option("dump", argv);
      case ':':
         missing_argument("dump", argv);
      case 'E':
         add_elab = true;
         break;
      case 'b':
         add_body = true;
         break;
      default:
         abort();
      }
   }

   set_top_level(argv, next_cmd);

   ident_t name = top_level;
   if (add_elab)
      name = ident_prefix(name, well_known(W_ELAB), '.');
   else if (add_body)
      name = ident_prefix(name, well_known(W_BODY), '-');

   tree_t top = lib_get(lib_work(), name);
   if (top == NULL)
      fatal("%s not analysed", istr(name));

   dump(top);

   argc -= next_cmd - 1;
   argv += next_cmd - 1;

   return argc > 1 ? process_command(argc, argv) : EXIT_SUCCESS;
}

#if ENABLE_LLVM
static int aotgen_cmd(int argc, char **argv)
{
   static struct option long_options[] = {
      { 0, 0, 0, 0 }
   };

   const char *outfile = "preload." DLL_EXT;

   const int next_cmd = scan_cmd(2, argc, argv);
   int c, index = 0;
   const char *spec = ":o:VO:";
   while ((c = getopt_long(next_cmd, argv, spec, long_options, &index)) != -1) {
      switch (c) {
      case 0: break;  // Set a flag
      case 'V':
         opt_set_int(OPT_VERBOSE, 1);
         break;
      case 'O':
         opt_set_int(OPT_OPTIMISE, parse_optimise_level(optarg));
         break;
      case 'o': outfile = optarg; break;
      case '?': bad_option("aotgen", argv);
      case ':': missing_argument("aotgen", argv);
      default: abort();
      }
   }

   const int count = next_cmd - optind;
   aotgen(outfile, argv + optind, count);

   argc -= next_cmd - 1;
   argv += next_cmd - 1;

   return argc > 1 ? process_command(argc, argv) : EXIT_SUCCESS;
}
#else
static int aotgen_cmd(int argc, char **argv)
{
   fatal("$bold$--aotgen$$ not supported without LLVM");
}
#endif

static uint32_t parse_cover_print_spec(char *str)
{
   uint32_t mask = 0;
   const char *delim = ",";
   for (char *tok = strtok(str, delim); tok; tok = strtok(NULL, delim)) {
      if (!strcmp(tok, "covered"))
         mask |= COVER_MASK_DONT_PRINT_COVERED;
      else if (!strcmp(tok, "uncovered"))
         mask |= COVER_MASK_DONT_PRINT_UNCOVERED;
      else if (!strcmp(tok, "excluded"))
         mask |= COVER_MASK_DONT_PRINT_EXCLUDED;
      else {
         diag_t *d = diag_new(DIAG_FATAL, NULL);
         diag_printf(d, "invalid option: '%s' for $bold$--dont-print$$", tok);
         diag_hint(d, NULL, "valid options are: 'covered', 'uncovered', "
                   "'excluded'");
         diag_emit(d);
         fatal_exit(EXIT_FAILURE);
      }
   }
   return mask;
}

static int coverage(int argc, char **argv)
{
   static struct option long_options[] = {
      { "report",       required_argument, 0, 'r' },
      { "exclude-file", required_argument, 0, 'e' },
      { "merge",        required_argument, 0, 'm' },
      { "dont-print",   required_argument, 0, 'd' },
      { "item-limit",   required_argument, 0, 'l' },
      { "verbose",      no_argument,       0, 'V' },
      { 0, 0, 0, 0 }
   };

   const char *out_db = NULL, *rpt_file = NULL, *exclude_file = NULL;
   int c, index;
   const char *spec = ":V";
   cover_mask_t rpt_mask = 0;
   int item_limit = 5000;

   while ((c = getopt_long(argc, argv, spec, long_options, &index)) != -1) {
      switch (c) {
      case 'r':
         rpt_file = optarg;
         break;
      case 'm':
         out_db = optarg;
         break;
      case 'e':
         exclude_file = optarg;
         break;
      case 'd':
         rpt_mask = parse_cover_print_spec(optarg);
         break;
      case 'l':
         item_limit = parse_int(optarg);
         break;
      case 'V':
         opt_set_int(OPT_VERBOSE, 1);
         break;
      case '?':
         bad_option("coverage", argv);
      case ':':
         missing_argument("coverage", argv);
      default:
         abort();
      }
   }

   progress("initialising");

   if (optind == argc)
      fatal("No input coverage database FILE specified");

   cover_tagging_t *cover = NULL;

   // Rest of inputs are coverage input files
   for (int i = optind; i < argc; i++) {
      fbuf_t *f = fbuf_open(argv[i], FBUF_IN, FBUF_CS_NONE);

      if (f != NULL) {
         progress("Loading input coverage database: %s", argv[i]);
         if (i == optind)
            cover = cover_read_tags(f, rpt_mask);
         else
            cover_merge_tags(f, cover);
      }
      else
         fatal("Could not open coverage database: %s", argv[i]);

      fbuf_close(f, NULL);
   }

   if (out_db) {
      progress("Saving merged coverage database to: %s", out_db);
      fbuf_t *f = fbuf_open(out_db, FBUF_OUT, FBUF_CS_NONE);
      cover_dump_tags(cover, f, COV_DUMP_PROCESSING, NULL, NULL, NULL, NULL);
      fbuf_close(f, NULL);
   }

   if (exclude_file && cover) {
      progress("Loading exclude file: %s", exclude_file);
      cover_load_exclude_file(exclude_file, cover);
   }

   if (rpt_file && cover) {
      progress("Generating code coverage report.");
      cover_report(rpt_file, cover, item_limit);
   }

   return 0;
}

static void usage(void)
{
   printf("Usage: %s [OPTION]... COMMAND [OPTION]...\n"
          "\n"
          "COMMAND is one of:\n"
          " -a [OPTION]... FILE...\t\tAnalyse FILEs into work library\n"
          " -e [OPTION]... UNIT\t\tElaborate and generate code for UNIT\n"
          " -r [OPTION]... UNIT\t\tExecute previously elaborated UNIT\n"
          " -c [OPTION]... FILE...\t\tProcess code coverage from FILEs\n"
          "                       \t\t'covdb' coverage databases.\n"
          " --dump [OPTION]... UNIT\tPrint out previously analysed UNIT\n"
          " --init\t\t\t\tInitialise work library directory\n"
          " --install PKG\t\t\tInstall third-party packages\n"
          " --list\t\t\t\tPrint all units in the library\n"
          " --print-deps [UNIT]...\t\tPrint dependencies in Makefile format\n"
          " --syntax FILE...\t\tCheck FILEs for syntax errors only\n"
          "\n"
          "Global options may be placed before COMMAND:\n"
          " -h, --help\t\tDisplay this message and exit\n"
          " -H SIZE\t\tSet the maximum heap size to SIZE bytes\n"
          "     --ignore-time\tSkip source file timestamp check\n"
          " -L PATH\t\tAdd PATH to library search paths\n"
          " -M SIZE\t\tLimit design unit heap space to SIZE bytes\n"
          "     --map=LIB:PATH\tMap library LIB to PATH\n"
          "     --messages=STYLE\tSelect full or compact message format\n"
          "     --native\t\tGenerate native code shared library\n"
          "     --std=REV\t\tVHDL standard revision to use\n"
          "     --stderr=SEV\tPrint messages higher than SEV to stderr\n"
          " -v, --version\t\tDisplay version and copyright information\n"
          "     --work=NAME\tUse NAME as the work library\n"
          "\n"
          "Analyse options:\n"
          "     --bootstrap\tAllow compilation of STANDARD package\n"
          "     --error-limit=NUM\tStop after NUM errors\n"
          "     --psl\t\tEnable parsing of PSL directives in comments\n"
          "     --relaxed\t\tDisable certain pedantic rule checks\n"
          "\n"
          "Elaborate options:\n"
          "     --cover[=TYPES]\tEnable code coverage collection. TYPES is a\n"
          "                    \tcomma separated list of coverage types to "
          "collect:\n"
          "                    \t  statement\n"
          "                    \t  toggle\n"
          "                    \t  branch\n"
          "                    \t  expression\n"
          "                    \tOmitting TYPES collects all coverage types.\n"
          "     --dump-llvm\tDump generated LLVM IR\n"
          "     --dump-vcode\tPrint generated intermediate code\n"
          " -g NAME=VALUE\t\tSet top level generic NAME to VALUE\n"
          " -j, --jit\t\tEnable just-in-time compilation during simulation\n"
          "     --no-save\t\tDo not save the elaborated design to disk\n"
          " -O0, -O1, -O2, -O3\tSet optimisation level (default is -O2)\n"
          " -V, --verbose\t\tPrint resource usage at each step\n"
          "\n"
          "Run options:\n"
          "     --dump-arrays\tInclude nested arrays in waveform dump\n"
          "     --exclude=GLOB\tExclude signals matching GLOB from wave dump\n"
          "     --exit-severity=\tExit after assertion failure of "
          "this severity\n"
          "     --format=FMT\tWaveform format is either fst or vcd\n"
          "     --ieee-warnings=\tEnable ('on') or disable ('off') warnings\n"
          "     \t\t\tfrom IEEE packages\n"
          "     --include=GLOB\tInclude signals matching GLOB in wave dump\n"
          "     --load=PLUGIN\tLoad VHPI plugin at startup\n"
          "     --profile\t\tDisplay detailed statistics at end of run\n"
          "     --stats\t\tPrint time and memory usage at end of run\n"
          "     --stop-delta=N\tStop after N delta cycles (default %d)\n"
          "     --stop-time=T\tStop after simulation time T (e.g. 5ns)\n"
          "     --trace\t\tTrace simulation events\n"
          "     --vhpi-trace\tTrace VHPI calls and events\n"
          " -w, --wave=FILE\tWrite waveform data; file name is optional\n"
          "\n"
          "Coverage processing options:\n"
          "     --merge=OUTPUT\tMerge all input coverage databases from FILEs\n"
          "                   \tto OUTPUT coverage database\n"
          "     --exclude-file=\tApply exclude file when generating report\n"
          "     --dont-print=\tDo not include specified tags in generated "
          "code\n"
          "                  \tcoverage report. Argument is a list of:\n"
          "                  \t  covered\n"
          "                  \t  uncovered\n"
          "                  \t  excluded\n"
          "     --report=DIR\tGenerate HTML report with code coverage results\n"
          "                    \tto DIR folder.\n"
          "\n"
          "Dump options:\n"
          " -e, --elab\t\tDump an elaborated unit\n"
          " -b, --body\t\tDump package body\n"
          "\n"
          "Install options:\n"
          "     --dest=DIR\t\tCompile libraries into directory DEST\n"
          "\n",
          PACKAGE,
          opt_get_int(OPT_STOP_DELTA));

   LOCAL_TEXT_BUF tb = tb_new();
   lib_print_search_paths(tb);
   printf("Library search paths:%s\n", tb_get(tb));

   printf("\nReport bugs to %s\n", PACKAGE_BUGREPORT);
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

static message_style_t parse_message_style(const char *str)
{
   if (strcmp(optarg, "full") == 0)
      return MESSAGE_FULL;
   else if (strcmp(optarg, "compact") == 0)
      return MESSAGE_COMPACT;

   fatal("invalid message style '%s' (allowed are 'full' and 'compact')", str);
}

static size_t parse_size(const char *str)
{
   char *eptr;
   const ssize_t size = strtoll(str, &eptr, 0);

   if (size <= 0)
      fatal("invalid size '%s' (must be positive)", str);
   else if (*eptr == '\0')
      return size;
   else if (strcasecmp(eptr, "k") == 0)
      return size * 1024;
   else if (strcasecmp(eptr, "m") == 0)
      return size * 1024 * 1024;
   else if (strcasecmp(eptr, "g") == 0)
      return size * 1024 * 1024 * 1024;

   fatal("invalid size '%s' (expected a number with optional k, m, "
         "or g suffix)", str);
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

static int process_command(int argc, char **argv)
{
   static struct option long_options[] = {
      { "dump",       no_argument, 0, 'd' },
      { "make",       no_argument, 0, 'm' },
      { "syntax",     no_argument, 0, 's' },
      { "list",       no_argument, 0, 'l' },
      { "init",       no_argument, 0, 'i' },
      { "install",    no_argument, 0, 'I' },
      { "print-deps", no_argument, 0, 'P' },
      { "aotgen",     no_argument, 0, 'A' },
      { 0, 0, 0, 0 }
   };

   opterr = 0;
   optind = 1;

   int index = 0;
   const char *spec = "aerc";
   switch (getopt_long(MIN(argc, 2), argv, spec, long_options, &index)) {
   case 'a':
      return analyse(argc, argv);
   case 'e':
      return elaborate(argc, argv);
   case 'r':
      return run(argc, argv);
   case 'c':
      return coverage(argc, argv);
   case 'd':
      return dump_cmd(argc, argv);
   case 'm':
      return make_cmd(argc, argv);
   case 's':
      return syntax_cmd(argc, argv);
   case 'l':
      return list_cmd(argc, argv);
   case 'i':
      return init_cmd(argc, argv);
   case 'I':
      return install_cmd(argc, argv);
   case 'P':
      return print_deps_cmd(argc, argv);
   case 'A':
      return aotgen_cmd(argc, argv);
   default:
      fatal("missing command, try %s --help for usage", PACKAGE);
      return EXIT_FAILURE;
   }
}

int main(int argc, char **argv)
{
   term_init();
   thread_init();
   set_default_options();
   intern_strings();
   register_signal_handlers();
   mspace_stack_limit(MSPACE_CURRENT_FRAME);

   atexit(fbuf_cleanup);

   static struct option long_options[] = {
      { "help",        no_argument,       0, 'h' },
      { "version",     no_argument,       0, 'v' },
      { "work",        required_argument, 0, 'w' },
      { "std",         required_argument, 0, 's' },
      { "messages",    required_argument, 0, 'I' },
      { "native",      no_argument,       0, 'n' },   // DEPRECATED 1.4
      { "map",         required_argument, 0, 'p' },
      { "ignore-time", no_argument,       0, 'i' },
      { "force-init",  no_argument,       0, 'f' },   // DEPRECATED 1.7
      { "stderr",      required_argument, 0, 'E' },
      { 0, 0, 0, 0 }
   };

   opterr = 0;

   const char *work_name = "work";

   const int next_cmd = scan_cmd(1, argc, argv);
   int c, index = 0;
   const char *spec = ":aehrcvL:M:P:G:H:";
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
         work_name = optarg;
         break;
      case 'L':
         lib_add_search_path(optarg);
         break;
      case 's':
         set_standard(parse_standard(optarg));
         break;
      case 'I':
         set_message_style(parse_message_style(optarg));
         break;
      case 'p':
         parse_library_map(optarg);
         break;
      case 'i':
         opt_set_int(OPT_IGNORE_TIME, 1);
         break;
      case 'f':
         warnf("the --force-init option is deprecated and has no effect");
         break;
      case 'n':
         warnf("the --native option is deprecated and has no effect");
         break;
      case 'M':
         opt_set_size(OPT_ARENA_SIZE, parse_size(optarg));
         break;
      case 'P':
      case 'G':
         warnf("the -%c option is deprecated and has no effect (the new "
               "-H option sets a unified heap size)", c);
         break;
      case 'H':
         opt_set_size(OPT_HEAP_SIZE, parse_size(optarg));
         break;
      case 'E':
         set_stderr_severity(parse_severity(optarg));
         break;
      case '?':
         bad_option("global", argv);
      case ':':
         missing_argument("global", argv);
      default:
         abort();
      }
   }

   lib_set_work(lib_new(work_name));

   argc -= next_cmd - 1;
   argv += next_cmd - 1;

   return process_command(argc, argv);
}
