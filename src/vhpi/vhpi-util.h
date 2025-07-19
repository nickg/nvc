//
//  Copyright (C) 2014-2025  Nick Gasson
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

#ifndef _VHPI_UTIL_H
#define _VHPI_UTIL_H

#include "prim.h"
#include "jit/jit.h"
#include "rt/rt.h"

#ifdef __MINGW32__
#define PLI_DLLISPEC __declspec(dllexport)
#define PLI_DLLESPEC
#else
#define PLI_DLLISPEC
#define PLI_DLLESPEC
#endif

#include "vhpi/vhpi_user.h"

__attribute__((format(printf, 2, 3)))
void vhpi_trace(const char *func, const char *fmt, ...);

__attribute__((format(printf, 3, 4)))
void vhpi_error(vhpiSeverityT sev, const loc_t *loc, const char *fmt, ...);

vhpi_context_t *vhpi_context_new(void);
void vhpi_context_initialise(vhpi_context_t *c, tree_t top, rt_model_t *model,
                             jit_t *jit, int argc, char **argv);
void vhpi_context_free(vhpi_context_t *c);

void vhpi_load_plugins(const char *plugins);

void vhpi_clear_error(void);
rt_event_t vhpi_get_rt_event(int reason);
vhpiFormatT vhpi_format_for_type(type_t type, const char **map_str);
uint64_t vhpi_time_to_native(const vhpiTimeT *time);
vhpiPhysT vhpi_phys_from_native(int64_t value);

vhpiHandleT vhpi_bind_foreign(const char *obj_lib, const char *model,
                              tree_t where);
void vhpi_call_foreign(vhpiHandleT handle, jit_scalar_t *args, tlab_t *tlab);

const char *vhpi_cb_reason_str(int reason);
const char *vhpi_one_to_many_str(vhpiOneToManyT kind);
const char *vhpi_one_to_one_str(vhpiOneToOneT kind);
const char *vhpi_class_str(vhpiClassKindT kind);
const char *vhpi_property_str(int property);
const char *vhpi_put_value_mode_str(vhpiPutValueModeT mode);
const char *vhpi_state_str(vhpiStateT state);
const char *vhpi_format_str(vhpiFormatT format);

#endif  // _VHPI_UTIL_H
