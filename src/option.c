//
//  Copyright (C) 2022-2024  Nick Gasson
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
#include "option.h"

#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <ctype.h>

typedef enum {
   OPT_KIND_UNDEF,
   OPT_KIND_INT,
   OPT_KIND_STRING,
   OPT_KIND_SIZE,
} option_kind_t;

typedef union {
   int     i;
   char   *s;
   size_t  z;
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

static inline optval_t opt_get_generic(opt_name_t name, option_kind_t kind)
{
   assert(name < OPT_LAST_NAME);

   const option_t *o = &(options[name]);
   if (likely(o->kind == kind))
      return o->value;
   else if (o->kind == OPT_KIND_UNDEF)
      fatal_trace("initial value for option %d not set", name);
   else
      fatal_trace("wrong option kind for %d", name);
}

void opt_set_int(opt_name_t name, int val)
{
   opt_set_generic(name, OPT_KIND_INT, (optval_t){ .i = val });
}

int opt_get_int(opt_name_t name)
{
   return opt_get_generic(name, OPT_KIND_INT).i;
}

void opt_set_size(opt_name_t name, size_t val)
{
   opt_set_generic(name, OPT_KIND_SIZE, (optval_t){ .z = val });
}

size_t opt_get_size(opt_name_t name)
{
   return opt_get_generic(name, OPT_KIND_SIZE).z;
}

void opt_set_str(opt_name_t name, const char *val)
{
   opt_set_generic(name, OPT_KIND_STRING,
                   (optval_t){ .s = val ? strdup(val) : NULL });
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

static int get_int_env(const char *var, int def)
{
   const char *env = getenv(var);
   if (env == NULL)
      return def;

   return atoi(env);
}

void set_default_options(void)
{
   opt_set_int(OPT_RT_STATS, 0);
   opt_set_int(OPT_RT_TRACE, 0);
   opt_set_str(OPT_PLI_TRACE, getenv("NVC_VHPI_VERBOSE"));
   opt_set_int(OPT_DUMP_LLVM, 0);
   opt_set_int(OPT_OPTIMISE, 2);
   opt_set_int(OPT_BOOTSTRAP, 0);
   opt_set_int(OPT_STOP_DELTA, 10000);
   opt_set_int(OPT_UNIT_TEST, 0);
   opt_set_int(OPT_MAKE_DEPS_ONLY, 0);
   opt_set_str(OPT_LOWER_VERBOSE, getenv("NVC_LOWER_VERBOSE"));
   opt_set_int(OPT_IGNORE_TIME, 0);
   opt_set_int(OPT_VERBOSE, 0);
   opt_set_int(OPT_MISSING_BODY, 1);
   opt_set_int(OPT_IEEE_WARNINGS, IEEE_WARNINGS_ON);
   opt_set_size(OPT_ARENA_SIZE, 1 << 24);
   opt_set_int(OPT_DUMP_ARRAYS, 0);
   opt_set_str(OPT_OBJECT_VERBOSE, getenv("NVC_OBJECT_VERBOSE"));
   opt_set_str(OPT_GC_VERBOSE, getenv("NVC_GC_VERBOSE") DEBUG_ONLY(?: "1"));
   opt_set_str(OPT_EVAL_VERBOSE, getenv("NVC_EVAL_VERBOSE"));
   opt_set_str(OPT_ELAB_VERBOSE, getenv("NVC_ELAB_VERBOSE"));
   opt_set_size(OPT_HEAP_SIZE, 16 * 1024 * 1024);
   opt_set_int(OPT_GC_STRESS, 0 DEBUG_ONLY(|| get_int_env("NVC_GC_STRESS", 0)));
   opt_set_int(OPT_RELAXED, 0);
   opt_set_str(OPT_JIT_VERBOSE, getenv("NVC_JIT_VERBOSE"));
   opt_set_int(OPT_JIT_LOG, get_int_env("NVC_JIT_LOG", 0));
   opt_set_int(OPT_NO_SAVE, 0);
   opt_set_str(OPT_LLVM_VERBOSE, getenv("NVC_LLVM_VERBOSE"));
   opt_set_int(OPT_JIT_THRESHOLD, get_int_env("NVC_JIT_THRESHOLD", 100));
   opt_set_str(OPT_ASM_VERBOSE, getenv("NVC_ASM_VERBOSE"));
   opt_set_int(OPT_JIT_ASYNC, get_int_env("NVC_JIT_ASYNC", 1));
   opt_set_int(OPT_PERF_MAP, get_int_env("NVC_PERF_MAP", 0));
   opt_set_str(OPT_LIB_VERBOSE, getenv("NVC_LIB_VERBOSE"));
   opt_set_str(OPT_PSL_VERBOSE, getenv("NVC_PSL_VERBOSE"));
   opt_set_int(OPT_PSL_COMMENTS, 0);
   opt_set_int(OPT_NO_COLLAPSE, 0);
   opt_set_int(OPT_COVER_VERBOSE, get_int_env("NVC_COVER_VERBOSE", 0));
   opt_set_int(OPT_COVER_TIMESTAMP, get_int_env("NVC_COVER_TIMESTAMP", -1));
   opt_set_str(OPT_COVER_VERSION, getenv("NVC_COVER_VERSION"));
   opt_set_int(OPT_DRIVER_VERBOSE, get_int_env("NVC_DRIVER_VERBOSE", 0));
   opt_set_int(OPT_JIT_INTRINSICS, get_int_env("NVC_JIT_INTRINSICS", 1));
   opt_set_int(OPT_VECTOR_INTRINSICS, get_int_env("NVC_VECTOR_INTRINSICS", 1));
   opt_set_int(OPT_SHUFFLE_PROCS, 0);
   opt_set_int(OPT_PLI_DEBUG, opt_get_str(OPT_PLI_TRACE) != NULL);
   opt_set_int(OPT_SERVER_PORT, 8888);
   opt_set_int(OPT_STDERR_LEVEL, DIAG_DEBUG);
   opt_set_int(OPT_CHECK_SYNTHESIS, 0);
   opt_set_int(OPT_MISSING_WAIT, 1);
   opt_set_int(OPT_LAYOUT_VERBOSE, get_int_env("NVC_LAYOUT_VERBOSE", 0));
   opt_set_int(OPT_SINGLE_UNIT, 0);
   opt_set_int(OPT_PRESERVE_CASE, 0);
   opt_set_str(OPT_GVN_VERBOSE, getenv("NVC_GVN_VERBOSE"));
   opt_set_str(OPT_DCE_VERBOSE, getenv("NVC_DCE_VERBOSE"));
   opt_set_str(OPT_CFG_VERBOSE, getenv("NVC_CFG_VERBOSE"));
   opt_set_str(OPT_RA_VERBOSE, getenv("NVC_RA_VERBOSE"));
   opt_set_int(OPT_RANDOM_SEED, get_timestamp_us());
   opt_set_int(OPT_ELAB_STATS, 0);
   opt_set_str(OPT_RELATIVE_PATH, NULL);
}
