//
//  Copyright (C) 2011  Nick Gasson
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

#ifndef _RT_H
#define _RT_H

#include "ident.h"

void rt_exec(ident_t top);
void rt_trace_en(bool en);

void jit_init(ident_t top);
void jit_shutdown(void);
void *jit_fun_ptr(const char *name);
void *jit_var_ptr(const char *name);
void jit_bind_fn(const char *name, void *ptr);

#endif  // _RT_H
