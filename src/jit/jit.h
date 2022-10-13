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

#ifndef _JIT_H
#define _JIT_H

#include "prim.h"
#include "diag.h"
#include "util.h"

typedef int32_t jit_handle_t;
#define JIT_HANDLE_INVALID 0xffffffff

typedef union {
   int64_t  integer;
   void    *pointer;
   double   real;
} jit_scalar_t;

typedef struct {
   int offset;
   int size;
   int repeat;
   int align;
} layout_part_t;

typedef struct {
   int nparts;
   int size;
   int align;
   layout_part_t parts[0];
} jit_layout_t;

typedef vcode_unit_t (*jit_lower_fn_t)(ident_t, void *);

typedef struct {
   void *(*init)(void);
   void (*cgen)(jit_t *, jit_handle_t, void *);
   void (*cleanup)(void *);
} jit_plugin_t;

jit_t *jit_new(void);
void jit_free(jit_t *j);
jit_handle_t jit_compile(jit_t *j, ident_t name);
jit_handle_t jit_lazy_compile(jit_t *j, ident_t name);
void *jit_link(jit_t *j, jit_handle_t handle);
void *jit_get_frame_var(jit_t *j, jit_handle_t handle, uint32_t var);
void jit_set_lower_fn(jit_t *j, jit_lower_fn_t fn, void *ctx);
void jit_set_silent(jit_t *j, bool silent);
const jit_layout_t *jit_layout(jit_t *j, type_t type);
void jit_limit_backedges(jit_t *j, int limit);
void jit_enable_runtime(jit_t *j, bool enable);
mspace_t *jit_get_mspace(jit_t *j);
void jit_load_dll(jit_t *j, ident_t name);
int jit_exit_status(jit_t *j);
void jit_set_exit_status(jit_t *j, int code);
void jit_reset_exit_status(jit_t *j);
void jit_add_tier(jit_t *j, int threshold, const jit_plugin_t *plugin);
ident_t jit_get_name(jit_t *j, jit_handle_t handle);

bool jit_try_call(jit_t *j, jit_handle_t handle, jit_scalar_t *result, ...);
bool jit_try_call_packed(jit_t *j, jit_handle_t handle, jit_scalar_t context,
                         void *input, size_t insz, void *output, size_t outsz);
jit_scalar_t jit_call(jit_t *j, jit_handle_t handle, ...);
bool jit_call_thunk(jit_t *j, vcode_unit_t unit, jit_scalar_t *result);
bool jit_fastcall(jit_t *j, jit_handle_t handle, jit_scalar_t *result,
                  jit_scalar_t p1, jit_scalar_t p2);

void jit_msg(const loc_t *where, diag_level_t level, const char *fmt, ...);
void jit_abort(int code);

#endif   // _JIT_H
