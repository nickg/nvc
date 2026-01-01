//
//  Copyright (C) 2022-2025  Nick Gasson
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

#ifndef _OPTION_H
#define _OPTION_H

#include "prim.h"

#include <stdbool.h>

typedef enum {
   OPT_RT_STATS,
   OPT_PLI_TRACE,
   OPT_DUMP_LLVM,
   OPT_OPTIMISE,
   OPT_BOOTSTRAP,
   OPT_STOP_DELTA,
   OPT_UNIT_TEST,
   OPT_MAKE_DEPS_ONLY,
   OPT_LOWER_VERBOSE,
   OPT_IGNORE_TIME,
   OPT_VERBOSE,
   OPT_RT_TRACE,
   OPT_MISSING_BODY,
   OPT_IEEE_WARNINGS,
   OPT_ARENA_SIZE,
   OPT_DUMP_ARRAYS,
   OPT_GC_VERBOSE,
   OPT_EVAL_VERBOSE,
   OPT_ELAB_VERBOSE,
   OPT_HEAP_SIZE,
   OPT_OBJECT_VERBOSE,
   OPT_GC_STRESS,
   OPT_RELAXED,
   OPT_JIT_VERBOSE,
   OPT_JIT_LOG,
   OPT_NO_SAVE,
   OPT_LLVM_VERBOSE,
   OPT_JIT_THRESHOLD,
   OPT_ASM_VERBOSE,
   OPT_JIT_ASYNC,
   OPT_PERF_MAP,
   OPT_LIB_VERBOSE,
   OPT_PSL_VERBOSE,
   OPT_PSL_COMMENTS,
   OPT_NO_COLLAPSE,
   OPT_COVER_VERBOSE,
   OPT_COVER_TIMESTAMP,
   OPT_COVER_VERSION,
   OPT_DRIVER_VERBOSE,
   OPT_JIT_INTRINSICS,
   OPT_VECTOR_INTRINSICS,
   OPT_SHUFFLE_PROCS,
   OPT_PLI_DEBUG,
   OPT_SERVER_PORT,
   OPT_STDERR_LEVEL,
   OPT_CHECK_SYNTHESIS,
   OPT_MISSING_WAIT,
   OPT_LAYOUT_VERBOSE,
   OPT_SINGLE_UNIT,
   OPT_PRESERVE_CASE,
   OPT_GVN_VERBOSE,
   OPT_DCE_VERBOSE,
   OPT_CFG_VERBOSE,
   OPT_RANDOM_SEED,
   OPT_ELAB_STATS,
   OPT_RELATIVE_PATH,
   OPT_RA_VERBOSE,

   OPT_LAST_NAME
} opt_name_t;

typedef enum {
   IEEE_WARNINGS_OFF,
   IEEE_WARNINGS_ON,
   IEEE_WARNINGS_OFF_AT_0
} ieee_warnings_t;

void opt_set_int(opt_name_t name, int val);
void opt_set_size(opt_name_t name, size_t val);
void opt_set_str(opt_name_t name, const char *val);
int opt_get_int(opt_name_t name);
size_t opt_get_size(opt_name_t name);
const char *opt_get_str(opt_name_t name);
bool opt_get_verbose(opt_name_t name, const char *filter);

void set_default_options(void);

#endif  // _OPTION_H
