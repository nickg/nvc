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

#ifndef _OPT_H
#define _OPT_H

#include "prim.h"

typedef enum {
   OPT_ERROR_LIMIT,
   OPT_RT_STATS,
   OPT_VHPI_TRACE,
   OPT_DUMP_LLVM,
   OPT_OPTIMISE,
   OPT_BOOTSTRAP,
   OPT_DUMP_JSON,
   OPT_COVER,
   OPT_STOP_DELTA,
   OPT_UNIT_TEST,
   OPT_MAKE_DEPS_ONLY,
   OPT_MAKE_POSIX,
   OPT_DUMP_VCODE,
   OPT_RELAX,
   OPT_IGNORE_TIME,
   OPT_VERBOSE,
   OPT_RT_PROFILE,
   OPT_RT_TRACE,
   OPT_SYNTHESIS,
   OPT_MISSING_BODY,
   OPT_IEEE_WARNINGS,
   OPT_ARENA_SIZE,
   OPT_DUMP_ARRAYS,
   OPT_FORCE_INIT,

   OPT_LAST_NAME
} opt_name_t;

void opt_set_int(opt_name_t name, int val);
void opt_set_str(opt_name_t name, const char *val);
int opt_get_int(opt_name_t name);
const char *opt_get_str(opt_name_t name);

#endif  // _OPT_H
