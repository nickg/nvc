//
//  Copyright (C) 2024  Nick Gasson
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

#ifndef _VPI_PRIV_H
#define _VPI_PRIV_H

#include "prim.h"

#ifdef __MINGW32__
#define PLI_DLLISPEC __declspec(dllexport)
#define PLI_DLLESPEC
#else
#define PLI_DLLISPEC
#define PLI_DLLESPEC
#endif

#include "vpi/vpi_user.h"

const char *vpi_type_str(PLI_INT32 type);
const char *vpi_method_str(PLI_INT32 type);
const char *vpi_property_str(PLI_INT32 property);

__attribute__((format(printf, 3, 4)))
void vpi_error(PLI_INT32 sev, const loc_t *loc, const char *fmt, ...);

__attribute__((format(printf, 2, 3)))
void vpi_trace(const char *func, const char *fmt, ...);

void vpi_register_builtins(void);
void vpi_clear_error(void);
void vpi_format_number(number_t n, PLI_INT32 format, text_buf_t *tb);
void vpi_format_number2(int size, uint64_t abits, uint64_t bbits,
                        PLI_INT32 format, text_buf_t *tb);

#endif  // _VPI_PRIV_H
