//
//  Copyright (C) 2014-2022  Nick Gasson
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
#include "rt/rt.h"

#if defined __MINGW32__ || defined __CYGWIN__
#define PLI_DLLISPEC __declspec(dllexport)
#define PLI_DLLESPEC __declspec(dllimport)
#endif

#include "vhpi/vhpi_user.h"

// Simulator interface to VHPI
void vhpi_build_design_model(tree_t top);

__attribute__((format(printf, 2, 3)))
void vhpi_trace(const char *func, const char *fmt, ...);

__attribute__((format(printf, 3, 4)))
void vhpi_error(vhpiSeverityT sev, const loc_t *loc, const char *fmt, ...);

void vhpi_load_plugins(tree_t top, const char *plugins);

void vhpi_clear_error(void);
rt_event_t vhpi_get_rt_event(int reason);
const char *vhpi_map_str_for_type(type_t type);
uint64_t vhpi_time_to_native(const vhpiTimeT *time);
bool vhpi_is_repetitive(vhpiEnumT reason);
vhpiPhysT vhpi_phys_from_native(int64_t value);

const char *vhpi_cb_reason_str(int reason);
const char *vhpi_one_to_many_str(vhpiOneToManyT kind);
const char *vhpi_one_to_one_str(vhpiOneToOneT kind);
const char *vhpi_class_str(vhpiClassKindT kind);
const char *vhpi_property_str(int property);
const char *vhpi_put_value_mode_str(vhpiPutValueModeT mode);

#endif  // _VHPI_UTIL_H
