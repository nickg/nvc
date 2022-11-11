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

#include "opt.h"
#include "util.h"
#include "diag.h"

#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <ctype.h>

typedef enum {
   OPT_KIND_UNDEF,
   OPT_KIND_INT,
   OPT_KIND_STRING
} option_kind_t;

typedef union {
   int   i;
   char *s;
} optval_t;

typedef struct {
   option_kind_t kind;
   optval_t      value;
} option_t;

static option_t options[OPT_LAST_NAME];

static void opt_set_generic(opt_name_t name, option_kind_t kind,
                            optval_t value)
{
   assert(name < OPT_LAST_NAME);

   option_t *o = &(options[name]);

   if (o->kind == OPT_KIND_STRING)
      free(o->value.s);

   o->value = value;
   o->kind  = kind;
}

static optval_t opt_get_generic(opt_name_t name, option_kind_t kind)
{
   assert(name < OPT_LAST_NAME);

   const option_t *o = &(options[name]);
   if (o->kind == kind)
      return o->value;
   else if (o->kind == OPT_KIND_UNDEF)
      fatal_trace("initial value for option %d not set", name);
   else
      fatal_trace("wrong option kind for %d", name);
}

void opt_set_int(opt_name_t name, int val)
{
   opt_set_generic(name, OPT_KIND_INT, (optval_t)val);
}

int opt_get_int(opt_name_t name)
{
   return opt_get_generic(name, OPT_KIND_INT).i;
}

void opt_set_str(opt_name_t name, const char *val)
{
   opt_set_generic(name, OPT_KIND_STRING, (optval_t)(val ? strdup(val) : NULL));
}

const char *opt_get_str(opt_name_t name)
{
   return opt_get_generic(name, OPT_KIND_STRING).s;
}

bool opt_get_verbose(opt_name_t name, const char *filter)
{
   const char *value = opt_get_str(name);
   if (value == NULL || *value == '\0')
      return false;
   else if (isdigit((int)*value))
      return true;
   else if (filter == NULL)
      return false;
   else if (value[0] == '^')
      return strcmp(value + 1, filter) == 0;
   else
      return strstr(filter, value) != NULL;
}

int opt_parse_comma_separated(const char *opt, const char *optarg,
                              opt_separed_t *allowed, int num_allowed)
{
   assert(num_allowed > 1);

   char buf[64] = {0};
   int len = 0;
   int rv = 0;

   while (1) {
      if (*optarg == ',' || *optarg == '\0') {
         bool found = false;

         for (int i = 0; i < num_allowed; i++)
            if (!strcmp(allowed[i].opt, buf)) {
               rv |= allowed[i].mask;
               printf("Setting option:%s\n", allowed[i].opt);
               found = true;
            }

         if (!found) {
            diag_t *d = diag_new(DIAG_FATAL, NULL);
            diag_printf(d, "Invalid option '%s' for command '%s'", buf, opt);
            diag_hint(d, NULL, "valid options are:");
            for (int i = 0; i < num_allowed; i++)
               diag_hint(d, NULL, "    %s", allowed[i].opt);
            diag_hint(d, NULL, "selected options shall be "
                               "comma separated e.g. $bold$%s=%s,%s...$$",
                                opt, allowed[0].opt, allowed[1].opt);
            diag_emit(d);
            fatal_exit(EXIT_FAILURE);
         }

         if (*optarg == '\0')
            break;

         for (int i = 0; i < sizeof(buf) / sizeof(char); i++)
            buf[i] = '\0';
         len = 0;
      }
      else {
         buf[len] = *optarg;
         len++;
      }
      optarg++;
   }
   return rv;
}

void set_default_options(void)
{
   opt_set_int(OPT_RT_STATS, 0);
   opt_set_int(OPT_RT_TRACE, 0);
   opt_set_str(OPT_VHPI_TRACE, getenv("NVC_VHPI_VERBOSE"));
   opt_set_int(OPT_DUMP_LLVM, 0);
   opt_set_int(OPT_OPTIMISE, 2);
   opt_set_int(OPT_BOOTSTRAP, 0);
   opt_set_int(OPT_STOP_DELTA, 10000);
   opt_set_int(OPT_UNIT_TEST, 0);
   opt_set_int(OPT_MAKE_DEPS_ONLY, 0);
   opt_set_int(OPT_MAKE_POSIX, 0);
   opt_set_str(OPT_DUMP_VCODE, getenv("NVC_LOWER_VERBOSE"));
   opt_set_int(OPT_IGNORE_TIME, 0);
   opt_set_int(OPT_VERBOSE, 0);
   opt_set_int(OPT_RT_PROFILE, 0);
   opt_set_int(OPT_MISSING_BODY, 1);
   opt_set_int(OPT_ERROR_LIMIT, -1);
   opt_set_int(OPT_IEEE_WARNINGS, 1);
   opt_set_int(OPT_ARENA_SIZE, 1 << 24);
   opt_set_int(OPT_DUMP_ARRAYS, 0);
   opt_set_str(OPT_OBJECT_VERBOSE, getenv("NVC_OBJECT_VERBOSE"));
   opt_set_str(OPT_GC_VERBOSE, getenv("NVC_GC_VERBOSE") DEBUG_ONLY(?: "1"));
   opt_set_str(OPT_EVAL_VERBOSE, getenv("NVC_EVAL_VERBOSE"));
   opt_set_str(OPT_ELAB_VERBOSE, getenv("NVC_ELAB_VERBOSE"));
   opt_set_int(OPT_HEAP_SIZE, 16 * 1024 * 1024);
   opt_set_int(OPT_ERROR_LIMIT, 20);
   opt_set_int(OPT_GC_STRESS, 0 DEBUG_ONLY(|| getenv("NVC_GC_STRESS") != 0));
   opt_set_int(OPT_RELAXED, 0);
   opt_set_str(OPT_JIT_VERBOSE, getenv("NVC_JIT_VERBOSE"));
   opt_set_int(OPT_JIT_LOG, getenv("NVC_JIT_LOG") != NULL);
   opt_set_int(OPT_WARN_HIDDEN, 0);
   opt_set_int(OPT_NO_SAVE, 0);
   opt_set_str(OPT_LLVM_VERBOSE, getenv("NVC_LLVM_VERBOSE"));
   opt_set_int(OPT_JIT_THRESHOLD, atoi(getenv("NVC_JIT_THRESHOLD") ?: "100"));
}
