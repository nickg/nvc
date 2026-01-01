//
//  Copyright (C) 2011-2026  Nick Gasson
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
#include "cov/cov-api.h"
#include "diag.h"
#include "ident.h"
#include "jit/jit.h"
#include "lib.h"
#include "lower.h"
#include "mir/mir-unit.h"
#include "option.h"
#include "phase.h"
#include "printf.h"
#include "rt/assert.h"
#include "rt/model.h"
#include "rt/mspace.h"
#include "rt/rt.h"
#include "rt/shell.h"
#include "rt/wave.h"
#include "scan.h"
#include "server.h"
#include "thread.h"
#include "vhpi/vhpi-util.h"
#include "vlog/vlog-node.h"
#include "vlog/vlog-phase.h"
#include "vpi/vpi-model.h"

#include <getopt.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <assert.h>
#include <sys/types.h>
#include <limits.h>
#include <unistd.h>
#include <dirent.h>
#include <time.h>

#if HAVE_GIT_SHA
#include "gitsha.h"
#define GIT_SHA_ONLY(x) x
#else
#define GIT_SHA_ONLY(x)
#endif

typedef struct {
   jit_t           *jit;
   unit_registry_t *registry;
   mir_context_t   *mir;
   bool             user_set_std;
   vhpi_context_t  *vhpi;
   vpi_context_t   *vpi;
   const char      *plugins;
   rt_model_t      *model;
   cover_data_t    *cover;
   ident_t          top_level;
   const char      *top_level_arg;
   lib_t            work;
} cmd_state_t;

const char copy_string[] =
   "Copyright (C) 2011-2026  Nick Gasson\n"
   "This program comes with ABSOLUTELY NO WARRANTY. This is free software, "
   "and\nyou are welcome to redistribute it under certain conditions. See "
   "the GNU\nGeneral Public Licence for details.";
const char version_string[] =
   PACKAGE_STRING GIT_SHA_ONLY(" (" GIT_SHA ")")
   LLVM_ONLY(" (Using LLVM " LLVM_VERSION ")") DEBUG_ONLY(" [debug]");

static int process_command(int argc, char **argv, cmd_state_t *state);
static int parse_int(const char *str);
static jit_t *get_jit(cmd_state_t *state);

static ident_t to_unit_name(const char *str)
{
   char *name LOCAL = xstrdup(str);
   for (char *p = name; *p; p++) {
      if (!isalnum_iso88591(*p) && (p == name || (*p != '_' && *p != '-')))
         fatal("'%s' is not a valid design unit name", str);

      *p = toupper_iso88591(*p);
   }

   return ident_prefix(lib_name(lib_work()), ident_new(name), '.');
}

static int scan_cmd(int start, int argc, char **argv)
{
   const char *commands[] = {
      "-a", "-e", "-r", "-c", "--dump", "--make", "--syntax", "--list",
      "--init", "--install", "--print-deps", "--do", "-i",
      "--cover-export", "--preprocess", "--gui", "--cover-merge",
      "--cover-report",
   };

   for (int i = start; i < argc; i++) {
      for (size_t j = 0; j < ARRAY_LEN(commands); j++) {
         if (commands[j][1] == '-') {
            if (strcmp(argv[i], commands[j]) == 0)
               return i;
         }
         else if (argv[i][0] == '-' && commands[j][1] == argv[i][1])
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

static void parse_pp_define(char *optarg)
{
   char *eq = strchr(optarg, '=');
   if (eq == NULL)
      pp_defines_add(optarg, "");
   else {
      *eq = '\0';
      pp_defines_add(optarg, eq + 1);
   }
}

static void do_file_list(const char *file, jit_t *jit, unit_registry_t *ur,
                         mir_context_t *mc)
{
   FILE *f;
   if (strcmp(file, "-") == 0)
      f = stdin;
   else if ((f = fopen(file, "r")) == NULL)
      fatal_errno("failed to open %s", file);

   LOCAL_TEXT_BUF tb = tb_new();
   char *line = NULL;
   size_t nchars, bufsz = 0;
   while ((nchars = getline(&line, &bufsz, f)) != -1) {
      char *stop = strpbrk(line, "\r\n#") ?: line + nchars;
      *stop = '\0';

      // Trim trailing whitespace
      while (stop > line && isspace_iso88591(*--stop))
         *stop = '\0';

      if (strlen(line) == 0)
         continue;

      tb_rewind(tb);

      for (char *p = line; *p;) {
         if (*p == '$') {  // Interpolate environment variable
            const char *start = ++p;
            while (isalnum_iso88591(*p) || *p == '_') p++;
            const char tail = *p;
            *p++ = '\0';

            const char *envvar = getenv(start);
            if (envvar == NULL) {
               warnf("environment variable $%s not set", start);
               tb_cat(tb, start - 1);
            }
            else
               tb_cat(tb, envvar);

            tb_append(tb, tail);
         }
         else
            tb_append(tb, *p++);
      }

      analyse_file(tb_get(tb), jit, ur, mc);
   }

   free(line);
   fclose(f);
}

static int analyse(int argc, char **argv, cmd_state_t *state)
{
   static struct option long_options[] = {
      { "bootstrap",       no_argument,       0, 'b' },
      { "error-limit",     required_argument, 0, 'l' },
      { "dump-vcode",      optional_argument, 0, 'v' },
      { "psl",             no_argument,       0, 'P' },
      { "relax",           required_argument, 0, 'X' },
      { "relaxed",         no_argument,       0, 'R' },
      { "define",          required_argument, 0, 'D' },
      { "files",           required_argument, 0, 'f' },
      { "check-synthesis", no_argument,       0, 's' },
      { "no-save",         no_argument,       0, 'N' },
      { "single-unit",     no_argument,       0, 'u' },
      { "preserve-case",   no_argument,       0, 'p' },
      { "keywords",        required_argument, 0, 'k' },
      { "relative",        required_argument, 0, 'r' },
      { 0, 0, 0, 0 }
   };

   const int next_cmd = scan_cmd(2, argc, argv);
   int c, index = 0, error_limit = 20;
   const char *file_list = NULL;
   const char *spec = ":D:f:I:";
   bool no_save = false;

   while ((c = getopt_long(next_cmd, argv, spec, long_options, &index)) != -1) {
      switch (c) {
      case 0:
         // Set a flag
         break;
      case '?':
         bad_option("analysis", argv);
      case ':':
         missing_argument("analysis", argv);
      case 'b':
         opt_set_int(OPT_BOOTSTRAP, 1);
         break;
      case 'v':
         opt_set_str(OPT_LOWER_VERBOSE, optarg ?: "");
         break;
      case 'X':
         warnf("The $bold$--relax=$$ option is deprecated: use the combined "
               "$bold$--relaxed$$ option instead");
         opt_set_int(OPT_RELAXED, 1);
         break;
      case 'l':
         error_limit = parse_int(optarg);
         break;
      case 'P':
         opt_set_int(OPT_PSL_COMMENTS, 1);
         break;
      case 'R':
         opt_set_int(OPT_RELAXED, 1);
         break;
      case 'D':
         parse_pp_define(optarg);
         break;
      case 'f':
         file_list = optarg;
         break;
      case 's':
         opt_set_int(OPT_CHECK_SYNTHESIS, 1);
         break;
      case 'N':
         no_save = true;
         break;
      case 'I':
         add_include_dir(optarg);
         break;
      case 'u':
         opt_set_int(OPT_SINGLE_UNIT, 1);
         break;
      case 'p':
         opt_set_int(OPT_PRESERVE_CASE, 1);
         break;
      case 'k':
         {
            vlog_version_t vers;
            if (parse_verilog_version(optarg, &vers))
               push_keywords(vers);
            else
               fatal("'%s' is not a valid Verilog version", optarg);
         }
         break;
      case 'r':
         opt_set_str(OPT_RELATIVE_PATH, optarg);
         break;
      default:
         should_not_reach_here();
      }
   }

   set_error_limit(error_limit);

   if (state->mir == NULL)
      state->mir = mir_context_new();

   if (state->registry == NULL)
      state->registry = unit_registry_new(state->mir);

   jit_t *jit = jit_new(state->registry, state->mir, NULL);

   if (file_list != NULL)
      do_file_list(file_list, jit, state->registry, state->mir);
   else if (optind == next_cmd)
      fatal("missing file name");

   for (int i = optind; i < next_cmd; i++) {
      if (argv[i][0] == '@')
         do_file_list(argv[i] + 1, jit, state->registry, state->mir);
      else
         analyse_file(argv[i], jit, state->registry, state->mir);
   }

   jit_free(jit);
   set_error_limit(0);

   if (error_count() > 0)
      return EXIT_FAILURE;

   if (!no_save)
      lib_save(state->work);

   if (error_count() > 0)
      return EXIT_FAILURE;   // May have errors saving library

   argc -= next_cmd - 1;
   argv += next_cmd - 1;

   return argc > 1 ? process_command(argc, argv, state) : EXIT_SUCCESS;
}

static void parse_generic(const char *str)
{
   char *copy LOCAL = xstrdup(str);

   char *split = strchr(copy, '=');
   if (split == NULL || *(split + 1) == '\0' || *copy == '\0')
      fatal("invalid generic specification '%s' (use -gNAME=VALUE)", str);

   *split = '\0';

   for (char *p = copy; *p != '\0'; p++)
      *p = toupper_iso88591(*p);

   elab_set_generic(copy, split + 1);
}

static void set_top_level(char **argv, int next_cmd, cmd_state_t *state)
{
   if (optind == next_cmd) {
      if (state->top_level == NULL)
         fatal("missing top-level unit name");
   }
   else if (optind != next_cmd - 1)
      fatal("excess positional argument '%s' following top-level unit name",
            argv[optind + 1]);
   else {
      state->top_level_arg = argv[optind];
      state->top_level = to_unit_name(argv[optind]);
   }
}

static void parse_cover_options(const char *str, cover_mask_t *mask,
                                int *array_limit, int *threshold)
{
   static const struct {
      const char *name;
      cover_mask_t mask;
   } options[] = {
      { "statement",             COVER_MASK_STMT                        },
      { "toggle",                COVER_MASK_TOGGLE                      },
      { "branch",                COVER_MASK_BRANCH                      },
      { "expression",            COVER_MASK_EXPRESSION                  },
      { "fsm-state",             COVER_MASK_STATE                       },
      { "functional",            COVER_MASK_FUNCTIONAL                  },
      { "all",                   COVER_MASK_ALL                         },
      { "count-from-undefined",  COVER_MASK_TOGGLE_COUNT_FROM_UNDEFINED },
      { "count-from-to-z",       COVER_MASK_TOGGLE_COUNT_FROM_TO_Z      },
      { "include-mems",          COVER_MASK_TOGGLE_INCLUDE_MEMS         },
      { "exclude-unreachable",   COVER_MASK_EXCLUDE_UNREACHABLE         },
      { "fsm-no-default-enums",  COVER_MASK_FSM_NO_DEFAULT_ENUMS        }
   };

   for (const char *start = str; ; str++) {
      if (*str == ',' || *str == '\0') {
         if (strncmp(start, "ignore-arrays-from-", 19) == 0)
            *array_limit = parse_int(start + 19);
         else if (strncmp(start, "threshold-", 10) == 0)
            *threshold = parse_int(start + 10);
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

static void load_jit_pack(jit_t *jit, tree_t top)
{
   char *name LOCAL = xasprintf("_%s.pack", istr(tree_ident(top)));
   FILE *f = lib_fopen(lib_work(), name, "rb");
   if (f != NULL) {
      jit_load_pack(jit, f);
      fclose(f);
   }
}

static int elaborate(int argc, char **argv, cmd_state_t *state)
{
   static struct option long_options[] = {
      { "dump-llvm",       no_argument,       0, 'd' },
      { "dump-vcode",      optional_argument, 0, 'v' },
      { "cover",           optional_argument, 0, 'c' },
      { "cover-file",      required_argument, 0, 'F' },
      { "cover-spec",      required_argument, 0, 's' },
      { "sdf",             required_argument, 0, 'f' },
      { "verbose",         no_argument,       0, 'V' },
      { "no-save",         no_argument,       0, 'N' },
      { "jit",             no_argument,       0, 'j' },
      { "no-collapse",     no_argument,       0, 'C' },
      { "stats",           no_argument,       0, 'S' },
      { "trace",           no_argument,       0, 't' },
      { 0, 0, 0, 0 }
   };

   bool no_save = false;
   unit_meta_t meta = {};
   cover_mask_t cover_mask = 0;
   const char *cover_spec_file = NULL, *sdf_args = NULL;
   int cover_array_limit = 0;
   int threshold = 1;
   const int next_cmd = scan_cmd(2, argc, argv);
   int c, index = 0;
   const char *spec = ":Vg:O:jt";
   while ((c = getopt_long(next_cmd, argv, spec, long_options, &index)) != -1) {
      switch (c) {
      case 'O':
         opt_set_int(OPT_OPTIMISE, parse_optimise_level(optarg));
         break;
      case 'd':
         opt_set_int(OPT_DUMP_LLVM, 1);
         break;
      case 'v':
         opt_set_str(OPT_LOWER_VERBOSE, optarg ?: "");
         break;
      case 'c':
         if (optarg)
            parse_cover_options(optarg, &(cover_mask), &(cover_array_limit),
                                &(threshold));
         else
            cover_mask = COVER_MASK_ALL;
         break;
      case 'V':
         opt_set_int(OPT_VERBOSE, 1);
         break;
      case 'N':
         opt_set_int(OPT_NO_SAVE, 1);
         no_save = true;
         break;
      case 'C':
         opt_set_int(OPT_NO_COLLAPSE, 1);
         break;
      case 'j':
         // No effect
         break;
      case 'g':
         parse_generic(optarg);
         break;
      case 's':
         cover_spec_file = optarg;
         break;
      case 'F':
         meta.cover_file = optarg;
         break;
      case 'f':
         sdf_args = optarg;
         break;
      case 't':
         opt_set_int(OPT_RT_TRACE, 1);
         break;
      case 'S':
         opt_set_int(OPT_ELAB_STATS, 1);
         break;
      case 0:
         // Set a flag
         break;
      case '?':
         bad_option("elaboration", argv);
      case ':':
         missing_argument("elaboration", argv);
      default:
         should_not_reach_here();
      }
   }

   set_top_level(argv, next_cmd, state);

   progress("initialising");

   object_t *obj = lib_get_generic(state->work, state->top_level, NULL);
   if (obj == NULL)
      fatal("cannot find unit %s in library %s",
            istr(state->top_level), istr(lib_name(state->work)));

   progress("loading top-level unit");

   char *cover_default LOCAL = NULL;
   if (cover_mask != 0) {
      state->cover = cover_data_init(cover_mask, cover_array_limit, threshold);

      if (cover_spec_file != NULL)
         cover_load_spec_file(state->cover, cover_spec_file);

      if (meta.cover_file == NULL) {
         cover_default = xasprintf("%s.ncdb", state->top_level_arg);
         meta.cover_file = cover_default;
      }
   }

   if (sdf_args != NULL) {
      // TODO: Pass min-max spec to underlying sdf_parse somehow
      analyse_file(sdf_args, NULL, NULL, NULL);
   }

   if (state->model != NULL) {
      model_free(state->model);
      state->model = NULL;
   }

   if (state->jit != NULL) {
      jit_free(state->jit);
      state->jit = NULL;
   }

   if (state->registry != NULL) {
      unit_registry_free(state->registry);
      state->registry = NULL;
   }

   if (state->mir != NULL) {
      mir_context_free(state->mir);
      state->mir = NULL;
   }

   state->mir = mir_context_new();
   state->registry = unit_registry_new(state->mir);
   state->jit = get_jit(state);
   state->model = model_new(state->jit, state->cover);

   if (state->vhpi == NULL)
      state->vhpi = vhpi_context_new();

   tree_t top = elab(obj, state->jit, state->registry, state->mir,
                     state->cover, NULL, state->model);

   if (top == NULL)
      return EXIT_FAILURE;

   lib_put_meta(state->work, top, &meta);

   progress("elaborating design");

   if (state->cover != NULL) {
      fbuf_t *f = fbuf_open(meta.cover_file, FBUF_OUT, FBUF_CS_NONE);
      if (f == NULL)
         fatal_errno("failed to open coverage database: %s", meta.cover_file);

      cover_dump_items(state->cover, f, COV_DUMP_ELAB);
      fbuf_close(f, NULL);

      progress("dumping coverage data");
   }

   if (error_count() > 0)
      return EXIT_FAILURE;

   const char *elab_name = istr(tree_ident(top));
   char *pack_name LOCAL = xasprintf("_%s.pack", elab_name);

   // Delete any existing generated code to avoid accidentally loading
   // the wrong version later
   lib_delete(state->work, pack_name);

   if (!no_save) {
      lib_save(state->work);
      progress("saving library");
   }

   if (!no_save)
      cgen(top, state->registry, state->mir, state->jit);

   argc -= next_cmd - 1;
   argv += next_cmd - 1;

   return argc > 1 ? process_command(argc, argv, state) : EXIT_SUCCESS;
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

static ieee_warnings_t parse_ieee_warnings(const char *str)
{
   if (strcasecmp(str, "off") == 0)
      return IEEE_WARNINGS_OFF;
   else if (strcasecmp(str, "on") == 0)
      return IEEE_WARNINGS_ON;
   else if (strcasecmp(str, "off-at-0") == 0)
      return IEEE_WARNINGS_OFF_AT_0;
   else
      fatal("specify 'on', 'off' or 'off-at-0' instead of '%s'", str);
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

static void parse_exit_severity(const char *str)
{
   const vhdl_severity_t s = parse_severity(optarg);
   set_exit_severity(s);
   set_status_severity(s);
}

static void parse_stderr_severity(const char *str)
{
   diag_level_t level;
   if (strcasecmp(str, "none") == 0)
      level = DIAG_FATAL + 1;  // No messages to stderr
   else
      level = get_diag_severity(parse_severity(optarg));

   opt_set_int(OPT_STDERR_LEVEL, level);
}

static int parse_stop_delta(const char *str)
{
   const int ival = parse_int(str);
   if (ival < 1)
      fatal("$bold$--stop-delta$$ argument must be greater than zero");
   else if (ival > DELTA_CYCLE_MAX) {
      warnf("the maxmimum number of supported delta cycles is %d",
            DELTA_CYCLE_MAX);
      return DELTA_CYCLE_MAX;
   }
   else
      return ival;
}

static void ctrl_c_handler(void *arg)
{
   rt_model_t *model = arg;
   model_interrupt(model);
}

static jit_t *get_jit(cmd_state_t *state)
{
   jit_t *jit = jit_new(state->registry, state->mir, state->cover);

#if defined HAVE_LLVM && 1
   jit_register_llvm_plugin(jit);
#elif defined ARCH_X86_64 && 0
   jit_register_native_plugin(jit);
#endif

   _std_standard_init();
   _std_env_init();
   _std_reflection_init();
   _file_io_init();
   _nvc_sim_pkg_init();

   return jit;
}

static int plusarg_cmp(const void *lptr, const void *rptr)
{
   const char *lstr = *(const char **)lptr;
   const char *rstr = *(const char **)rptr;

   if (lstr[0] == '+' && rstr[0] != '+')
      return -1;
   else if (lstr[0] != '+' && rstr[0] == '+')
      return 1;
   else
      return lptr - rptr;
}

static cover_data_t *load_coverage(const unit_meta_t *meta)
{
   if (meta->cover_file == NULL)
      return NULL;

   fbuf_t *f = fbuf_open(meta->cover_file, FBUF_IN, FBUF_CS_NONE);
   if (f == NULL)
      fatal_errno("failed to open coverage database: %s", meta->cover_file);

   cover_data_t *db = cover_read_items(f, 0);

   fbuf_close(f, NULL);
   return db;
}

static void emit_coverage(const unit_meta_t *meta, jit_t *j, cover_data_t *db)
{
   assert(meta->cover_file != NULL);

   fbuf_t *f = fbuf_open(meta->cover_file, FBUF_OUT, FBUF_CS_NONE);
   if (f == NULL)
      fatal_errno("failed to open coverage database: %s", meta->cover_file);

   cover_dump_items(db, f, COV_DUMP_RUNTIME);

   fbuf_close(f, NULL);
}

static void enable_ieee_warnings_cb(rt_model_t *m, void *ctx)
{
   cmd_state_t *state = ctx;

   assert(opt_get_int(OPT_IEEE_WARNINGS) == IEEE_WARNINGS_OFF_AT_0);
   opt_set_int(OPT_IEEE_WARNINGS, IEEE_WARNINGS_ON);

   ident_t pkg_name = ident_new("NVC.SIM_PKG");
   ident_t var_name = ident_new("IEEE_NO_WARNING");

   jit_handle_t handle = jit_lazy_compile(state->jit, pkg_name);
   jit_link(state->jit, handle);

   uint8_t *ptr = jit_get_frame_var(state->jit, handle, var_name);
   assert(ptr != NULL);
   assert(*ptr == 1);

   *ptr = 0;
}

static int run_cmd(int argc, char **argv, cmd_state_t *state)
{
   static struct option long_options[] = {
      { "trace",         no_argument,       0, 't' },
      { "profile",       no_argument,       0, 'p' },   // DEPRECATED 1.14
      { "stop-time",     required_argument, 0, 's' },
      { "stats",         no_argument,       0, 'S' },
      { "wave",          optional_argument, 0, 'w' },
      { "stop-delta",    required_argument, 0, 'd' },
      { "format",        required_argument, 0, 'f' },
      { "include",       required_argument, 0, 'i' },
      { "ieee-warnings", required_argument, 0, 'I' },   // DEPRECATED 1.16
      { "exclude",       required_argument, 0, 'e' },
      { "exit-severity", required_argument, 0, 'x' },
      { "dump-arrays",   optional_argument, 0, 'a' },
      { "load",          required_argument, 0, 'l' },
      { "vhpi-debug",    no_argument,       0, 'D' },
      { "vhpi-trace",    no_argument,       0, 'T' },
      { "gtkw",          optional_argument, 0, 'g' },
      { "shuffle",       no_argument,       0, 'H' },
      { 0, 0, 0, 0 }
   };

   wave_format_t wave_fmt = WAVE_FORMAT_FST;
   uint64_t      stop_time = TIME_HIGH;
   const char   *wave_fname = NULL;
   const char   *gtkw_fname = NULL;
   const char   *pli_plugins = NULL;

   static bool have_run = false;
   if (have_run)
      fatal("multiple run commands are not supported");

   have_run = true;

   const int next_cmd = scan_cmd(2, argc, argv);

   int c, index = 0;
   const char *spec = ":w::l:gi";
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
         warnf("the $bold$--profile$$ option is deprecated and has no effect");
         break;
      case 'T':
         opt_set_str(OPT_PLI_TRACE, "1");
         opt_set_int(OPT_PLI_DEBUG, 1);
         break;
      case 'D':
         opt_set_int(OPT_PLI_DEBUG, 1);
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
         opt_set_int(OPT_STOP_DELTA, parse_stop_delta(optarg));
         break;
      case 'i':
         wave_include_glob(optarg);
         break;
      case 'e':
         wave_exclude_glob(optarg);
         break;
      case 'l':
         pli_plugins = optarg;
         break;
      case 'x':
         parse_exit_severity(optarg);
         break;
      case 'I':
         {
            const ieee_warnings_t on = parse_ieee_warnings(optarg);

            // TODO: add an unconditional warning after 1.16
            if (state->jit != NULL && opt_get_int(OPT_IEEE_WARNINGS) != on)
               warnf("the $bold$--ieee-warnings$$ option may have no effect "
                     "as the IEEE packages have already been initialised, pass "
                     "$bold$--ieee-warnings$$ as a global option instead");

            opt_set_int(OPT_IEEE_WARNINGS, on);
         }
         break;
      case 'a':
         if (optarg == NULL)
            opt_set_int(OPT_DUMP_ARRAYS, INT_MAX);
         else
            opt_set_int(OPT_DUMP_ARRAYS, parse_int(optarg));
         break;
      case 'H':
         warnf("the $bold$--shuffle$$ option is intended for debug use only "
               "and may introduce significant performance overhead as well "
               "as non-deterministic behaviour");
         opt_set_int(OPT_SHUFFLE_PROCS, 1);
         break;
      default:
         should_not_reach_here();
      }
   }

   // Shuffle the arguments to put all the plusargs first
   qsort(argv + optind, next_cmd - optind, sizeof(char *), plusarg_cmp);

   int nplusargs = 0;
   char **plusargs = argv + optind;
   for (int i = optind; i < next_cmd; i++) {
      if (argv[i][0] == '+')
         nplusargs++, optind++;
   }

   set_top_level(argv, next_cmd, state);

   ident_t ename = ident_prefix(state->top_level, well_known(W_ELAB), '.');

   const unit_meta_t *meta = NULL;
   object_t *obj = lib_get_generic(state->work, ename, &meta);
   if (obj == NULL)
      fatal("%s not elaborated", istr(state->top_level));

   tree_t top = tree_from_object(obj);
   assert(top != NULL);

   wave_dumper_t *dumper = NULL;
   if (wave_fname != NULL) {
      const char *name_map[] = { "FST", "VCD" };
      const char *ext_map[]  = { "fst", "vcd" };
      char *tmp LOCAL = NULL, *tmp2 LOCAL = NULL;

      if (*wave_fname == '\0') {
         tmp = xasprintf("%s.%s", state->top_level_arg, ext_map[wave_fmt]);
         wave_fname = tmp;
         notef("writing %s waveform data to %s", name_map[wave_fmt], tmp);
      }

      if (gtkw_fname != NULL && *gtkw_fname == '\0') {
         tmp2 = xasprintf("%s.gtkw", state->top_level_arg);
         gtkw_fname = tmp2;
      }

      wave_include_file(argv[optind]);
      dumper = wave_dumper_new(wave_fname, gtkw_fname, top, wave_fmt);
   }
   else if (gtkw_fname != NULL)
      warnf("$bold$--gtkw$$ option has no effect without $bold$--wave$$");

   if (opt_get_size(OPT_HEAP_SIZE) < 0x100000)
      warnf("recommended heap size is at least 1M");

   if (state->cover == NULL)
      state->cover = load_coverage(meta);

   if (state->mir == NULL)
      state->mir = mir_context_new();

   if (state->registry == NULL)
      state->registry = unit_registry_new(state->mir);

   if (state->jit == NULL)
      state->jit = get_jit(state);

   load_jit_pack(state->jit, top);

   if (state->model == NULL) {
      state->model = model_new(state->jit, state->cover);
      create_scope(state->model, top, NULL);
   }

   if (state->vhpi == NULL)
      state->vhpi = vhpi_context_new();

   if (state->vpi == NULL)
      state->vpi = vpi_context_new();

   if (pli_plugins != NULL || state->plugins != NULL) {
      vhpi_context_initialise(state->vhpi, top, state->model, state->jit,
                              nplusargs, plusargs);
      vpi_context_initialise(state->vpi, top, state->model, state->jit,
                             nplusargs, plusargs);
   }
   else if (nplusargs > 0)
      warnf("found plusargs on command line but no VHPI plugin was loaded");

   if (pli_plugins != NULL)
      vhpi_load_plugins(pli_plugins);

   set_ctrl_c_handler(ctrl_c_handler, state->model);

   model_reset(state->model);

   if (dumper != NULL)
      wave_dumper_restart(dumper, state->model, state->jit);

   if (opt_get_int(OPT_IEEE_WARNINGS) == IEEE_WARNINGS_OFF_AT_0)
      model_set_phase_cb(state->model, END_TIME_STEP,
                         enable_ieee_warnings_cb, state);

   model_run(state->model, stop_time);

   set_ctrl_c_handler(NULL, NULL);

   const int rc = model_exit_status(state->model);

   if (dumper != NULL)
      wave_dumper_free(dumper);

   if (state->cover != NULL)
      emit_coverage(meta, state->jit, state->cover);

   vhpi_context_free(state->vhpi);
   state->vhpi = NULL;

   vpi_context_free(state->vpi);
   state->vpi = NULL;

   model_free(state->model);
   state->model = NULL;

   argc -= next_cmd - 1;
   argv += next_cmd - 1;

   return rc == 0 && argc > 1 ? process_command(argc, argv, state) : rc;
}

static int print_deps_cmd(int argc, char **argv, cmd_state_t *state)
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
   tree_t *targets = NULL;
   if (count > 0)
      targets = xmalloc_array(count, sizeof(tree_t));

   for (int i = optind; i < next_cmd; i++) {
      ident_t name = to_unit_name(argv[i]);
      ident_t elab = ident_prefix(name, well_known(W_ELAB), '.');
      if ((targets[i - optind] = lib_get(state->work, elab)) == NULL) {
         if ((targets[i - optind] = lib_get(state->work, name)) == NULL)
            fatal("cannot find unit %s in library %s",
                  istr(name), istr(lib_name(state->work)));
      }
   }

   make(targets, count, stdout);

   argc -= next_cmd - 1;
   argv += next_cmd - 1;

   return argc > 1 ? process_command(argc, argv, state) : EXIT_SUCCESS;
}

static int make_cmd(int argc, char **argv, cmd_state_t *state)
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

   for (int i = optind; i < next_cmd; i++) {
      ident_t name = to_unit_name(argv[i]);
      ident_t elab = ident_prefix(name, well_known(W_ELAB), '.');
      if ((targets[i - optind] = lib_get(state->work, elab)) == NULL) {
         if ((targets[i - optind] = lib_get(state->work, name)) == NULL)
            fatal("cannot find unit %s in library %s",
                  istr(name), istr(lib_name(state->work)));
      }
   }

   make(targets, count, stdout);

   argc -= next_cmd - 1;
   argv += next_cmd - 1;

   return argc > 1 ? process_command(argc, argv, state) : EXIT_SUCCESS;
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
   case T_LAST_TREE_KIND: pretty = "Verilog"; break;
   }

   printf("%-30s  : %s\n", istr(ident), pretty);
}

static int list_cmd(int argc, char **argv, cmd_state_t *state)
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

   lib_walk_index(state->work, list_walk_fn, NULL);

   argc -= next_cmd - 1;
   argv += next_cmd - 1;

   return argc > 1 ? process_command(argc, argv, state) : EXIT_SUCCESS;
}

static int init_cmd(int argc, char **argv, cmd_state_t *state)
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

   lib_save(state->work);

   argc -= next_cmd - 1;
   argv += next_cmd - 1;

   return argc > 1 ? process_command(argc, argv, state) : EXIT_SUCCESS;
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

static int install_cmd(int argc, char **argv, cmd_state_t *state)
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

   if (state->user_set_std)
      setenv("NVC_STD", standard_text(standard()), 1);

   for (int i = optind; i < next_cmd; i++) {
      tb_rewind(tb);
      get_libexec_dir(tb);
      tb_printf(tb, DIR_SEP "install-%s.sh", argv[i]);

      file_info_t info;
      if (!get_file_info(tb_get(tb), &info) || info.type != FILE_REGULAR) {
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

   return argc > 1 ? process_command(argc, argv, state) : EXIT_SUCCESS;
}

static int syntax_cmd(int argc, char **argv, cmd_state_t *state)
{
   static struct option long_options[] = {
      { 0, 0, 0, 0 }
   };

   warnf("the $bold$--syntax$$ command is deprecated, use "
         "$bold$-a --no-save$$ instead");

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

   return argc > 1 ? process_command(argc, argv, state) : EXIT_SUCCESS;
}

static void dump_one_unit(ident_t name, bool add_elab, bool add_body)
{
   if (add_elab)
      name = ident_prefix(name, well_known(W_ELAB), '.');
   else if (add_body)
      name = ident_prefix(name, well_known(W_BODY), '-');

   object_t *obj = lib_get_generic(lib_work(), name, NULL);
   if (obj == NULL)
      fatal("%s not analysed", istr(name));

   tree_t t = tree_from_object(obj);
   if (t != NULL)
      dump(t);

   vlog_node_t v = vlog_from_object(obj);
   if (v != NULL)
      vlog_dump(v, 0);
}

static int dump_cmd(int argc, char **argv, cmd_state_t *state)
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

   if (optind == next_cmd) {
      if (state->top_level == NULL)
         fatal("missing top-level unit name");
      else
         dump_one_unit(state->top_level, add_elab, add_body);
   }
   else {
      for (int i = optind; i < next_cmd; i++)
         dump_one_unit(to_unit_name(argv[i]), add_elab, add_body);
   }

   argc -= next_cmd - 1;
   argv += next_cmd - 1;

   return argc > 1 ? process_command(argc, argv, state) : EXIT_SUCCESS;
}

static int do_cmd(int argc, char **argv, cmd_state_t *state)
{
   static struct option long_options[] = {
      { 0, 0, 0, 0 }
   };

   const int next_cmd = scan_cmd(2, argc, argv);
   int c, index = 0;
   const char *spec = ":";
   while ((c = getopt_long(next_cmd, argv, spec, long_options, &index)) != -1) {
      switch (c) {
      case 0: break;  // Set a flag
      case '?': bad_option("do", argv);
      case ':': missing_argument("do", argv);
      default: abort();
      }
   }

   if (state->mir == NULL)
      state->mir = mir_context_new();

   if (state->registry == NULL)
      state->registry = unit_registry_new(state->mir);

   if (state->jit == NULL)
      state->jit = get_jit(state);

#ifdef ENABLE_TCL
   tcl_shell_t *sh = shell_new(state->jit);

   if (optind < next_cmd && strpbrk(argv[optind], "./\\") == NULL) {
      ident_t unit_name = to_unit_name(argv[optind]);
      if (lib_get(state->work, unit_name) != NULL) {
         state->top_level_arg = xstrdup(argv[optind]);
         state->top_level = unit_name;
         optind++;
      }
   }

   if (state->top_level != NULL) {
      ident_t ename = ident_prefix(state->top_level, well_known(W_ELAB), '.');
      tree_t top = lib_get(state->work, ename);
      if (top == NULL)
         fatal("%s not elaborated", istr(state->top_level));

      load_jit_pack(state->jit, top);

      shell_reset(sh, top);
   }

   if (optind == next_cmd)
      fatal("no script file specified");

   for (int i = optind; i < next_cmd; i++) {
      if (!shell_do(sh, argv[i])) {
         shell_free(sh);
         return EXIT_FAILURE;
      }
   }

   shell_free(sh);
#else
   fatal("compiled without TCL support");
#endif

   argc -= next_cmd - 1;
   argv += next_cmd - 1;

   return argc > 1 ? process_command(argc, argv, state) : EXIT_SUCCESS;
}

static int interact_cmd(int argc, char **argv, cmd_state_t *state)
{
   static struct option long_options[] = {
      { 0, 0, 0, 0 }
   };

   const int next_cmd = scan_cmd(2, argc, argv);
   int c, index = 0;
   const char *spec = ":";
   while ((c = getopt_long(next_cmd, argv, spec, long_options, &index)) != -1) {
      switch (c) {
      case 0: break;  // Set a flag
      case '?': bad_option("do", argv);
      case ':': missing_argument("do", argv);
      default: abort();
      }
   }

   if (state->mir == NULL)
      state->mir = mir_context_new();

   if (state->registry == NULL)
      state->registry = unit_registry_new(state->mir);

   if (state->jit == NULL)
      state->jit = get_jit(state);

#ifdef ENABLE_TCL
   tcl_shell_t *sh = shell_new(state->jit);

   if (optind < next_cmd && strpbrk(argv[optind], "./\\") == NULL) {
      ident_t unit_name = to_unit_name(argv[optind]);
      if (lib_get(state->work, unit_name) != NULL) {
         state->top_level_arg = xstrdup(argv[optind]);
         state->top_level = unit_name;
         optind++;
      }
   }

   if (optind != next_cmd)
      fatal("unexpected argument \"%s\"", argv[optind]);

   if (state->top_level != NULL) {
      ident_t ename = ident_prefix(state->top_level, well_known(W_ELAB), '.');
      tree_t top = lib_get(state->work, ename);
      if (top == NULL)
         fatal("%s not elaborated", istr(state->top_level));

      load_jit_pack(state->jit, top);

      shell_reset(sh, top);
   }

   shell_interact(sh);

   shell_free(sh);
#else
   fatal("compiled without TCL support");
#endif

   argc -= next_cmd - 1;
   argv += next_cmd - 1;

   return argc > 1 ? process_command(argc, argv, state) : EXIT_SUCCESS;
}

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

static merge_mode_t parse_cover_merge_mode(char *str)
{
   if (strcmp(str, "intersect") == 0)
      return MERGE_INTERSECT;
   else if (strcmp(str, "union") == 0)
      return MERGE_UNION;
   else {
      diag_t *d = diag_new(DIAG_FATAL, NULL);
      diag_printf(d, "invalid option: '%s' for $bold$--merge-mode$$", str);
      diag_hint(d, NULL, "valid options are: 'union', 'intersect'");
      diag_emit(d);
      fatal_exit(EXIT_FAILURE);
   }

   return 0;
}

static int coverage_cmd(int argc, char **argv, cmd_state_t *state)
{
   warnf("the $bold$-c$$ sub-command is deprecated, use $bold$--cover-report$$ "
         "or $bold$--cover-merge$$ instead");

   static struct option long_options[] = {
      { "report",       required_argument, 0, 'r' },
      { "exclude-file", required_argument, 0, 'e' },
      { "export",       required_argument, 0, 'E' },
      { "merge",        required_argument, 0, 'm' },
      { "dont-print",   required_argument, 0, 'd' },
      { "item-limit",   required_argument, 0, 'l' },
      { "verbose",      no_argument,       0, 'V' },
      { 0, 0, 0, 0 }
   };

   const char *out_db = NULL, *rpt_file = NULL, *exclude_file = NULL;
   const char *export_file = NULL;
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
      case 'E':
         export_file = optarg;
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
      fatal("no input coverage database FILE specified");

   cover_data_t *cover = NULL;

   // Rest of inputs are coverage input files
   for (int i = optind; i < argc; i++) {
      fbuf_t *f = fbuf_open(argv[i], FBUF_IN, FBUF_CS_NONE);

      if (f != NULL) {
         progress("Loading input coverage database: %s", argv[i]);
         if (i == optind)
            cover = cover_read_items(f, rpt_mask);
         else
            cover_merge_items(f, cover, MERGE_UNION);
      }
      else
         fatal("Could not open coverage database: %s", argv[i]);

      fbuf_close(f, NULL);
   }

   if (out_db) {
      progress("Saving merged coverage database to: %s", out_db);
      fbuf_t *f = fbuf_open(out_db, FBUF_OUT, FBUF_CS_NONE);
      cover_dump_items(cover, f, COV_DUMP_PROCESSING);
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

   if (export_file && cover) {
      progress("Exporting XML coverage report");

      FILE *f = create_file("%s", export_file);
      cover_export_cobertura(cover, f, NULL);
      fclose(f);
   }

   return 0;
}

#ifdef ENABLE_GUI
static int gui_cmd(int argc, char **argv, cmd_state_t *state)
{
   static struct option long_options[] = {
      { "init",     required_argument, 0, 'i' },
      { "port",     required_argument, 0, 'p' },
      { "protocol", required_argument, 0, 'o' },
      { 0, 0, 0, 0 }
   };

   const int next_cmd = scan_cmd(2, argc, argv);
   server_kind_t kind = SERVER_HTTP;
   int c, index = 0;
   const char *spec = ":", *init_cmd = NULL;
   while ((c = getopt_long(next_cmd, argv, spec, long_options, &index)) != -1) {
      switch (c) {
      case 0: break;  // Set a flag
      case 'i': init_cmd = optarg; break;
      case 'p':
         {
            const int port = parse_int(optarg);
            if (port < 0 || port > UINT16_MAX)
               fatal("invalid port number %d", port);

            opt_set_int(OPT_SERVER_PORT, port);
         }
         break;
      case 'o':
         {
            if (strcmp(optarg, "http") == 0)
               kind = SERVER_HTTP;
            else if (strcmp(optarg, "cxxrtl") == 0)
               kind = SERVER_CXXRTL;
            else
               fatal("invalid protocol '%s', valid choices are "
                     "'http' and 'cxxrtl'", optarg);
         }
         break;
      case '?': bad_option("gui", argv);
      case ':': missing_argument("gui", argv);
      default: abort();
      }
   }

   if (argc != optind)
      fatal("$bold$--gui$$ command takes no positional arguments");

   tree_t top = NULL;
   if (state->top_level != NULL) {
      ident_t ename = ident_prefix(state->top_level, well_known(W_ELAB), '.');
      if ((top = lib_get(state->work, ename)) == NULL)
         fatal("%s not elaborated", istr(state->top_level));
   }

   if (state->mir == NULL)
      state->mir = mir_context_new();

   if (state->jit == NULL)
      state->jit = get_jit(state);

   start_server(kind, state->jit, top, NULL, NULL, init_cmd);

   argc -= next_cmd - 1;
   argv += next_cmd - 1;

   return argc > 1 ? process_command(argc, argv, state) : EXIT_SUCCESS;
}
#endif

static cover_data_t *merge_coverage_files(int argc, int next_cmd, char **argv,
                                          cover_mask_t rpt_mask,
                                          merge_mode_t mode)
{
   // Merge all input coverage databases given on command line

   if (optind == next_cmd)
      fatal("no input coverage database specified");

   cover_data_t *cover = NULL;

   for (int i = optind; i < next_cmd; i++) {
      fbuf_t *f = fbuf_open(argv[i], FBUF_IN, FBUF_CS_NONE);
      if (f == NULL) {
         // Attempt to redirect the old file name to the new one
         // TODO: this should be removed at some point
         char *slash = strrchr(argv[i], *DIR_SEP) ?: strrchr(argv[i], '/');
         if (slash != NULL && slash[1] == '_') {
            char *tail = strstr(slash, ".covdb");
            if (tail != NULL && tail[6] == '\0') {
               ident_t unit_name = ident_new_n(slash + 2, tail - slash - 2);
               lib_t lib = lib_find(ident_until(unit_name, '.'));
               if (lib != NULL) {
                  const unit_meta_t *meta;
                  object_t *obj = lib_get_generic(lib, unit_name, &meta);
                  if (obj != NULL && meta->cover_file != NULL) {
                     warnf("redirecting %s to %s, please update your scripts",
                           argv[i], meta->cover_file);
                     f = fbuf_open(meta->cover_file, FBUF_IN, FBUF_CS_NONE);
                  }
               }
            }
         }
      }

      if (f == NULL)
         fatal_errno("could not open %s", argv[i]);

      progress("loading input coverage database %s", argv[i]);

      if (i == optind)
         cover = cover_read_items(f, rpt_mask);
      else
         cover_merge_items(f, cover, mode);

      fbuf_close(f, NULL);
   }

   return cover;
}

static int cover_export_cmd(int argc, char **argv, cmd_state_t *state)
{
   static struct option long_options[] = {
      { "format",   required_argument, 0, 'f' },
      { "output",   required_argument, 0, 'o' },
      { "relative", optional_argument, 0, 'r' },
      { 0, 0, 0, 0 }
   };

   const int next_cmd = scan_cmd(2, argc, argv);

   enum { UNSET, COBERTURA, XML } format = UNSET;
   const char *output = NULL, *relative = NULL;
   int c, index;
   const char *spec = ":o:";
   while ((c = getopt_long(next_cmd, argv, spec, long_options, &index)) != -1) {
      switch (c) {
      case 'f':
         if (strcasecmp(optarg, "cobertura") == 0)
            format = COBERTURA;
         else if (strcasecmp(optarg, "xml") == 0)
            format = XML;
         else
            fatal("unknown format '%s', valid formats are: cobertura, xml",
                  optarg);
         break;
      case 'o':
         output = optarg;
         break;
      case 'r':
         relative = optarg ?: ".";
         break;
      case '?':
         bad_option("coverage export", argv);
      case ':':
         missing_argument("coverage export", argv);
      default:
         abort();
      }
   }

   if (format == UNSET) {
      diag_t *d = diag_new(DIAG_FATAL, NULL);
      diag_printf(d, "the $bold$--format$$ option is required");
      diag_hint(d, NULL, "pass $bold$--format=cobertura$$ for Cobertura XML");
      diag_emit(d);
      return EXIT_FAILURE;
   }

   // DEPRECATED 1.14
   // Handle the old-style --cover-export which accepted a top-level unit name
   bool looks_like_file = false;
   if (argc != optind) {
      file_info_t info;
      if (get_file_info(argv[optind], &info))
         looks_like_file = true;
      else if (strstr(argv[optind], "/"))
         looks_like_file = true;
   }

   cover_data_t *cover;
   if (looks_like_file)
      cover = merge_coverage_files(argc, next_cmd, argv, 0, MERGE_UNION);
   else {
      set_top_level(argv, next_cmd, state);

      char *fname LOCAL = xasprintf("_%s.elab.covdb", istr(state->top_level));
      fbuf_t *f = lib_fbuf_open(state->work, fname, FBUF_IN, FBUF_CS_NONE);

      if (f == NULL)
         fatal("no coverage database for %s", istr(state->top_level));

      cover = cover_read_items(f, 0);
      fbuf_close(f, NULL);

      warnf("exporting the coverage database using the top-level unit name "
            "is deprecated, pass the path to the coverage database instead");
   }

   FILE *file = stdout;
   if (output != NULL)
      file = create_file("%s", output);

   switch (format) {
   case COBERTURA:
      cover_export_cobertura(cover, file, relative);
      break;
   case XML:
      cover_export_xml(cover, file, relative);
      break;
   case UNSET:
      should_not_reach_here();
   }

   if (file != stdout)
      fclose(file);

   argc -= next_cmd - 1;
   argv += next_cmd - 1;

   return argc > 1 ? process_command(argc, argv, state) : 0;
}

static int cover_report_cmd(int argc, char **argv, cmd_state_t *state)
{
   static struct option long_options[] = {
      { "report",       required_argument, 0, 'r' },   // DEPRECATED 1.14
      { "output",       required_argument, 0, 'o' },
      { "exclude-file", required_argument, 0, 'e' },
      { "dont-print",   required_argument, 0, 'd' },
      { "item-limit",   required_argument, 0, 'l' },
      { "per-file",     no_argument,       0, 'f' },
      { "verbose",      no_argument,       0, 'V' },
      { 0, 0, 0, 0 }
   };

   const int next_cmd = scan_cmd(2, argc, argv);

   const char *outdir = NULL, *exclude_file = NULL;
   int c, index;
   const char *spec = ":Vo:";
   cover_mask_t rpt_mask = 0;
   int item_limit = 5000;

   while ((c = getopt_long(next_cmd, argv, spec, long_options, &index)) != -1) {
      switch (c) {
      case 'r':
         warnf("the $bold$--report$$ option is deprecated, use "
               "$bold$--output$$ instead");
         // Fall-through
      case 'o':
         outdir = optarg;
         break;
      case 'e':
         exclude_file = optarg;
         break;
      case 'd':
         rpt_mask |= parse_cover_print_spec(optarg);
         break;
      case 'l':
         item_limit = parse_int(optarg);
         break;
      case 'f':
         rpt_mask |= COVER_MASK_PER_FILE_REPORT;
         break;
      case 'V':
         opt_set_int(OPT_VERBOSE, 1);
         break;
      case '?':
         bad_option("coverage report", argv);
      case ':':
         missing_argument("coverage report", argv);
      default:
         should_not_reach_here();
      }
   }

   if (outdir == NULL)
      fatal("the output directory must be specified with $bold$--output$$");

   progress("initialising");

   cover_data_t *cover =
      merge_coverage_files(argc, next_cmd, argv, rpt_mask, MERGE_UNION);

   if (exclude_file && cover) {
      progress("loading exclude file %s", exclude_file);
      cover_load_exclude_file(exclude_file, cover);
   }

   progress("generating code coverage report");
   cover_report(outdir, cover, item_limit);

   argc -= next_cmd - 1;
   argv += next_cmd - 1;

   return argc > 1 ? process_command(argc, argv, state) : 0;
}

static int cover_merge_cmd(int argc, char **argv, cmd_state_t *state)
{
   static struct option long_options[] = {
      { "output",       required_argument, 0, 'o' },
      { "merge-mode",   required_argument, 0, 'm' },
      { "verbose",      no_argument,       0, 'V' },
      { 0, 0, 0, 0 }
   };

   const int next_cmd = scan_cmd(2, argc, argv);

   const char *out_db = NULL;
   int c, index;
   const char *spec = ":Vo:";
   merge_mode_t mode = MERGE_UNION;

   while ((c = getopt_long(next_cmd, argv, spec, long_options, &index)) != -1) {
      switch (c) {
      case 'o':
         out_db = optarg;
         break;
      case 'm':
         mode = parse_cover_merge_mode(optarg);
         break;
      case 'V':
         opt_set_int(OPT_VERBOSE, 1);
         break;
      case '?':
         bad_option("coverage merge", argv);
      case ':':
         missing_argument("coverage merge", argv);
      default:
         should_not_reach_here();
      }
   }

   if (out_db == NULL)
      fatal("the output database must be specified with $bold$--output$$");

   progress("initialising");

   cover_data_t *cover = merge_coverage_files(argc, next_cmd, argv, 0, mode);

   progress("saving merged coverage database to %s", out_db);

   fbuf_t *f = fbuf_open(out_db, FBUF_OUT, FBUF_CS_NONE);
   cover_dump_items(cover, f, COV_DUMP_PROCESSING);
   fbuf_close(f, NULL);

   argc -= next_cmd - 1;
   argv += next_cmd - 1;

   return argc > 1 ? process_command(argc, argv, state) : 0;
}

static int preprocess_cmd(int argc, char **argv, cmd_state_t *state)
{
   static struct option long_options[] = {
      { "define",      required_argument, 0, 'D' },
      { "single-unit", no_argument,       0, 'u' },
      { 0, 0, 0, 0 }
   };

   const int next_cmd = scan_cmd(2, argc, argv);

   int c, index;
   const char *spec = ":D:I:";
   while ((c = getopt_long(argc, argv, spec, long_options, &index)) != -1) {
      switch (c) {
      case 'u':
         opt_set_int(OPT_SINGLE_UNIT, 1);
         break;
      case 'D':
         parse_pp_define(optarg);
         break;
      case 'I':
         add_include_dir(optarg);
         break;
      case ':':
         missing_argument("preprocess", argv);
      case '?':
      default:
         bad_option("preprocess", argv);
      }
   }

   if (optind == next_cmd)
      fatal("no input files");

   LOCAL_TEXT_BUF tb = tb_new();
   for (int i = optind; i < next_cmd; i++) {
      input_from_file(argv[i]);

      tb_rewind(tb);
      vlog_preprocess(tb, false);

      fputs(tb_get(tb), stdout);
   }

   argc -= next_cmd - 1;
   argv += next_cmd - 1;

   return argc > 1 ? process_command(argc, argv, state) : 0;
}

static void usage(void)
{
   nvc_printf("$!cyan$Usage:$$ $bold$%s [OPTION]... "
              "COMMAND [OPTION]...$$\n\n",  PACKAGE);

   wrapped_printf("Global options are placed before COMMAND, and "
                  "command-specific options are placed afterwards. "
                  "Multiple commands can be given and will be performed "
                  "in order.\n\n");

   static const struct {
      const char *group;
      struct {
         const char *args;
         const char *usage;
      } options[16];
   } groups[] = {
      { "Commands",
        {
           { "-a [OPTION]... FILE...", "Analyse FILEs into work library" },
           { "-e [OPTION]... TOP", "Elaborate design unit TOP" },
           { "-r [OPTION]... TOP", "Execute previously elaborated TOP" },
#ifdef ENABLE_TCL
           { "-i [TOP]", "Launch interactive TCL shell" },
#endif
           { "--cover-export FILE...",
             "Export coverage database to external format" },
           { "--cover-report FILE...",
             "Generate HTML report from coverage database" },
           { "--cover-merge FILE...", "Merge multiple coverage databases" },
#ifdef ENABLE_TCL
           { "--do [TOP] FILE...", "Evaluate TCL script" },
#endif
#ifdef ENABLE_GUI
           { "--gui", "Launch browser-based GUI" },
#endif
           { "--init", "Initialise work library directory" },
           { "--install PKG", "Install third-party packages" },
           { "--list", "Print all units in the library" },
           { "--preprocess FILE...",
             "Expand FILEs with Verilog preprocessor" },
           { "--print-deps [UNIT]...",
             "Print dependencies in Makefile format" },
        }
      },
      { "Global options",
        {
           { "-h, --help", "Display this message and exit" },
           { "-H SIZE", "Set the maximum heap size to SIZE bytes" },
           { "--ieee-warnings={on,off,off-at-0}",
             "Enable or disable warnings from IEEE packages" },
           { "--ignore-time", "Skip source file timestamp check" },
           { "--load=PLUGIN", "Load VHPI plugin at startup" },
           { "-L PATH", "Add PATH to library search paths" },
           { "-M SIZE", "Limit design unit heap space to SIZE bytes" },
           { "--map=LIB:PATH", "Map library LIB to PATH" },
           { "--messages={full,compact}",
             "Diagnostic message style, compact is less verbose" },
           { "--seed=N", "Seed for random number generation" },
           { "--std={1993,..,2019}", "VHDL standard revision to use" },
           { "--stderr={note,warning,error,failure,none}",
             "Print messages of this severity level or higher to stderr" },
           { "-v, --version", "Display version and copyright information" },
           { "--vhpi-debug", "Report VHPI errors as diagnostic messages" },
           { "--vhpi-trace", "Trace VHPI calls and events" },
           { "--work=NAME", "Use NAME as the work library" },
        }
      },
      { "Analysis options",
        {
           { "--bootstrap", "Allow compilation of STANDARD package" },
           { "--check-synthesis", "Warn on common synthesis mistakes" },
           { "-D, --define NAME=VALUE",
             "Set preprocessor symbol NAME to VALUE" },
           { "--error-limit=NUM", "Stop after NUM errors" },
           { "-f, --files=LIST", "Read files to analyse from LIST" },
           { "-I DIR", "Add DIR to list of Verilog include directories" },
           { "--keywords=VERSION",
             "Use keywords from specified Verilog version" },
           { "--no-save", "Do not save analysed design units" },
           { "--preserve-case",
             "Preserve the original case of VHDL identifiers" },
           { "--psl", "Enable parsing of PSL directives in comments" },
           { "--relative=DIR", "Store paths relative to DIR" },
           { "--relaxed", "Disable certain pedantic rule checks" },
           { "--single-unit",
             "Treat all Verilog files as a single compilation unit" },
        }
      },
      { "Elaboration options",
        {
           { "--cover[={statement,branch,expression,toggle,...}]",
             "Enable code coverage collection" },
           { "--cover-file=FILE",
             "Set the file name of the coverage database" },
           { "--cover-spec=FILE",
             "Fine-grained coverage collection specification, see the manual "
             "for details" },
           { "-g NAME=VALUE", "Set top level generic NAME to VALUE" },
           { "-j, --jit", "Enable just-in-time compilation during simulation" },
           { "-O0, -O1, -O2, -O3", "Set optimisation level (default is -O2)" },
           { "--no-collapse", "Do not collapse multiple signals into one" },
           { "--no-save", "Do not save the elaborated design to disk" },
           { "--stats", "Print statistics about instantiated design units" },
           { "-V, --verbose", "Print resource usage at each step" },
        }
      },
      { "Run options",
        {
           { "--dump-arrays[=N]",
             "Include nested arrays with up to N elements in waveform dump" },
           { "--exclude=GLOB",
             "Exclude signals matching GLOB from waveform dump" },
           { "--exit-severity={note,warning,error,failure}",
             "Exit after an assertion failure of this severity" },
           { "--format={fst,vcd}", "Waveform dump format" },
           { "--include=GLOB",
             "Include signals matching GLOB in waveform dump" },
           { "--shuffle", "Run processes in random order" },
           { "--stats", "Print time and memory usage at end of run" },
           { "--stop-delta=N", "Stop after N delta cycles (default 10000)" },
           { "--stop-time=T", "Stop after simulation time T (e.g. 5ns)" },
           { "--trace", "Trace simulation events" },
           { "-w, --wave[=FILE]", "Write waveform dump to FILE" },
        }
      },
#ifdef ENABLE_GUI
      { "GUI options",
        {
           { "--init=CMDS", "Evaluate TCL commands on startup" },
           { "--port=PORT", "Specify port for HTTP server" },
        }
      },
#endif
      { "Coverage report options",
        {
           { "-o, --output=dir", "Output directory for HTML report" },
           { "--exclude-file=FILE",
             "Apply exclude file when generating report, see manual for syntax"
           },
           { "--dont-print={covered,uncovered,excluded}",
             "Exclude specified items from coverage report" },
           { "--item-limit=LIMIT",
             "Display at most LIMIT items in a single report file "
             "(default 5000)" },
           { "--per-file",
             "Create source file code coverage report." },
        }
      },
      { "Coverage merge options",
        {
           { "--merge-mode={union,intersect}",
             "Merge hierarchies by union or intersection, default is union"
           },
           { "-o, --output=FILE", "Output database file name" },
        }
      },
      { "Coverage export options",
        {
           { "--format=FMT", "File format (must be 'cobertura')" },
           { "-o, --output=FILE", "Output file name" },
           { "--relative=PATH", "Report file names relative to PATH" },
        }
      },
      { "Install options",
        {
           { "--dest=DIR", "Compile libraries into directory DIR" }
        },
      }
   };

   const int right = MAX(60, terminal_width());

   for (int i = 0; i < ARRAY_LEN(groups); i++) {
      nvc_printf("$bold$$cyan$%s:$$\n", groups[i].group);

      for (int j = 0; j < ARRAY_LEN(groups[i].options); j++) {
         const char *args  = groups[i].options[j].args;
         const char *usage = groups[i].options[j].usage;

         if (args == NULL || usage == NULL)
            break;

         int col = 0;
         if (args[0] == '-' && args[1] == '-' && i > 0)
            col += nvc_printf("     $bold$%s$$ ", args);
         else
            col += nvc_printf(" $bold$%s$$ ", args);

         const int indent = i == 0 ? 30 : 20;
         if (col > indent)
            printf("\n%*.s", indent, "");
         else
            col += printf("%*.s", indent - col, "");

         col = indent;

         const char *p = usage, *begin = usage;
         for (; *p != '\0'; p++) {
            if (col + 1 >= right && p - begin + 1 < right - indent) {
               printf("\n%*s", indent, "");
               col = indent + p - begin;
            }
            else if (isspace_iso88591(*p)) {
               fwrite(begin, 1, p - begin + 1, stdout);
               col++;
               begin = p + 1;
            }
            else
               ++col;
         }

         if (begin < p)
            fwrite(begin, 1, p - begin, stdout);

         printf("\n");
      }

      printf("\n");
   }

   LOCAL_TEXT_BUF tb = tb_new();
   lib_print_search_paths(tb);
   nvc_printf("$!cyan$Library search paths:$$%s\n\n", tb_get(tb));

   wrapped_printf("The full manual can be read with $bold$man 1 %s$$ and "
                  "contains detailed explanations of the commands and options "
                  "above as well as examples.\n", PACKAGE_NAME);
   nvc_printf("\nReport bugs at $link:%s\07%s$\n", PACKAGE_BUGREPORT,
              PACKAGE_BUGREPORT);
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
#ifdef __MINGW32__
   char *split = strpbrk(str, ";:");
#else
   char *split = strchr(str, ':');
#endif

   if (split == NULL)
      fatal("invalid library map syntax '%s': use NAME:PATH", str);

   *split = '\0';

   if (strcasecmp(str, "work") == 0)
      fatal("use --work option to specify work library name and path");

   lib_add_map(str, split + 1);
}

static int process_command(int argc, char **argv, cmd_state_t *state)
{
   static struct option long_options[] = {
      { "dump",         no_argument, 0, 'd' },
      { "make",         no_argument, 0, 'm' },   // DEPRECATED 1.8
      { "syntax",       no_argument, 0, 's' },   // DEPRECATED 1.15
      { "list",         no_argument, 0, 'l' },
      { "init",         no_argument, 0, 'n' },
      { "install",      no_argument, 0, 'I' },
      { "print-deps",   no_argument, 0, 'P' },
      { "do",           no_argument, 0, 'D' },
      { "cover-export", no_argument, 0, 'E' },
      { "cover-merge",  no_argument, 0, 'M' },
      { "cover-report", no_argument, 0, 'p' },
      { "preprocess",   no_argument, 0, 'R' },
#ifdef ENABLE_GUI
      { "gui",          no_argument, 0, 'g' },
#endif
      { 0, 0, 0, 0 }
   };

   opterr = 0;
   optind = 1;

   int index = 0;
   const char *spec = "aerci";
   switch (getopt_long(MIN(argc, 2), argv, spec, long_options, &index)) {
   case 'a':
      return analyse(argc, argv, state);
   case 'e':
      return elaborate(argc, argv, state);
   case 'r':
      return run_cmd(argc, argv, state);
   case 'c':    // DEPRECATED 1.14
      return coverage_cmd(argc, argv, state);
   case 'd':
      return dump_cmd(argc, argv, state);
   case 'm':
      return make_cmd(argc, argv, state);
   case 's':
      return syntax_cmd(argc, argv, state);
   case 'l':
      return list_cmd(argc, argv, state);
   case 'n':
      return init_cmd(argc, argv, state);
   case 'I':
      return install_cmd(argc, argv, state);
   case 'P':
      return print_deps_cmd(argc, argv, state);
   case 'D':
      return do_cmd(argc, argv, state);
   case 'i':
      return interact_cmd(argc, argv, state);
   case 'E':
      return cover_export_cmd(argc, argv, state);
   case 'M':
      return cover_merge_cmd(argc, argv, state);
   case 'p':
      return cover_report_cmd(argc, argv, state);
   case 'R':
      return preprocess_cmd(argc, argv, state);
#ifdef ENABLE_GUI
   case 'g':
      return gui_cmd(argc, argv, state);
#endif
   default:
      fatal("missing command, try $bold$%s --help$$ for usage", PACKAGE);
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
   check_cpu_features();

   atexit(fbuf_cleanup);

   static struct option long_options[] = {
      { "help",          no_argument,       0, 'h' },
      { "version",       no_argument,       0, 'v' },
      { "work",          required_argument, 0, 'w' },
      { "std",           required_argument, 0, 's' },
      { "messages",      required_argument, 0, 'I' },
      { "map",           required_argument, 0, 'p' },
      { "ieee-warnings", required_argument, 0, 'W' },
      { "ignore-time",   no_argument,       0, 'i' },
      { "force-init",    no_argument,       0, 'f' }, // DEPRECATED 1.7
      { "stderr",        required_argument, 0, 'E' },
      { "load",          required_argument, 0, 'l' },
      { "vhpi-debug",    no_argument,       0, 'D' },
      { "vhpi-trace",    no_argument,       0, 'T' },
      { "seed",          required_argument, 0, 'S' },
      { 0, 0, 0, 0 }
   };

   opterr = 0;

   const char *work_name = "work";
   cmd_state_t state = {};

   const int next_cmd = scan_cmd(1, argc, argv);
   int c, index = 0;
   const char *spec = ":hivL:M:P:G:H:";
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
         state.user_set_std = true;
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
         parse_stderr_severity(optarg);
         break;
      case 'l':
         state.plugins = optarg;
         break;
      case 'T':
         opt_set_str(OPT_PLI_TRACE, "1");
         opt_set_int(OPT_PLI_DEBUG, 1);
         break;
      case 'D':
         opt_set_int(OPT_PLI_DEBUG, 1);
         break;
      case 'W':
         opt_set_int(OPT_IEEE_WARNINGS, parse_ieee_warnings(optarg));
         break;
      case 'S':
         opt_set_int(OPT_RANDOM_SEED, parse_int(optarg));
         break;
      case '?':
         bad_option("global", argv);
      case ':':
         missing_argument("global", argv);
      default:
         should_not_reach_here();
      }
   }

   srand(opt_get_int(OPT_RANDOM_SEED));

   state.work = lib_new(work_name);
   lib_set_work(state.work);

   argc -= next_cmd - 1;
   argv += next_cmd - 1;

   if (state.plugins != NULL) {
      state.vhpi = vhpi_context_new();
      vhpi_load_plugins(state.plugins);
   }

   const int ret = process_command(argc, argv, &state);

   if (state.model != NULL)
      model_free(state.model);

   if (state.jit != NULL)
      jit_free(state.jit);   // JIT must be shut down before exiting

   if (state.registry != NULL)
      unit_registry_free(state.registry);

   if (state.mir != NULL)
      mir_context_free(state.mir);

   if (state.cover != NULL)
      cover_data_free(state.cover);

   return ret;
}
