//
//  Copyright (C) 2022-2023  Nick Gasson
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
   void *(*init)(jit_t *);
   void (*cgen)(jit_t *, jit_handle_t, void *);
   void (*cleanup)(void *);
} jit_plugin_t;

typedef struct {
   loc_t     loc;
   object_t *object;
   ident_t   symbol;
} jit_frame_t;

typedef struct {
   size_t      count;
   jit_frame_t frames[0];
} jit_stack_trace_t;

typedef void (*jit_irq_fn_t)(jit_t *, void *);

jit_t *jit_new(unit_registry_t *ur);
void jit_free(jit_t *j);
jit_handle_t jit_compile(jit_t *j, ident_t name);
jit_handle_t jit_lazy_compile(jit_t *j, ident_t name);
jit_handle_t jit_assemble(jit_t *j, ident_t name, const char *text);
void *jit_link(jit_t *j, jit_handle_t handle);
void *jit_get_frame_var(jit_t *j, jit_handle_t handle, ident_t name);
void jit_set_silent(jit_t *j, bool silent);
void jit_enable_runtime(jit_t *j, bool enable);
mspace_t *jit_get_mspace(jit_t *j);
void jit_load_dll(jit_t *j, ident_t name);
void jit_preload(jit_t *j);
bool jit_exit_status(jit_t *j, int *status);
void jit_reset_exit_status(jit_t *j);
void jit_add_tier(jit_t *j, int threshold, const jit_plugin_t *plugin);
ident_t jit_get_name(jit_t *j, jit_handle_t handle);
void jit_register_native_plugin(jit_t *j);
void jit_interrupt(jit_t *j, jit_irq_fn_t fn, void *ctx);
void jit_check_interrupt(jit_t *j);
void jit_reset(jit_t *j);
int32_t *jit_get_cover_mem(jit_t *j, int mintags);

void *jit_mspace_alloc(size_t size) RETURNS_NONNULL;
jit_stack_trace_t *jit_stack_trace(void);

bool jit_try_call(jit_t *j, jit_handle_t handle, jit_scalar_t *result, ...);
bool jit_try_call_packed(jit_t *j, jit_handle_t handle, jit_scalar_t context,
                         void *input, size_t insz, void *output, size_t outsz);
jit_scalar_t jit_call(jit_t *j, jit_handle_t handle, ...);
bool jit_call_thunk(jit_t *j, vcode_unit_t unit, jit_scalar_t *result,
                    void *context);
bool jit_fastcall(jit_t *j, jit_handle_t handle, jit_scalar_t *result,
                  jit_scalar_t p1, jit_scalar_t p2, tlab_t *tlab);

tlab_t jit_null_tlab(jit_t *j);

jit_pack_t *jit_pack_new(void);
void jit_pack_free(jit_pack_t *jp);
void jit_pack_encode(jit_pack_t *jp, jit_t *j, jit_handle_t handle);
void jit_pack_emit(jit_pack_t *jp, fbuf_t *fbuf, ident_wr_ctx_t ident_ctx,
                   loc_wr_ctx_t *loc_ctx);
void jit_pack_vcode(jit_pack_t *jp, jit_t *j, vcode_unit_t vu);

__attribute__((format(printf, 3, 4)))
void jit_msg(const loc_t *where, diag_level_t level, const char *fmt, ...);

__attribute__((noreturn))
void jit_abort(void);

__attribute__((noreturn))
void jit_abort_with_status(int code);

#endif   // _JIT_H
