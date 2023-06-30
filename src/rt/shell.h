//
//  Copyright (C) 2023  Nick Gasson
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

#ifndef _RT_SHELL_H
#define _RT_SHELL_H

#include "prim.h"

typedef jit_t *(*jit_factory_t)(unit_registry_t *);

tcl_shell_t *shell_new(jit_factory_t make_jit);
void shell_free(tcl_shell_t *sh);
bool shell_eval(tcl_shell_t *sh, const char *script, const char **result);
bool shell_do(tcl_shell_t *sh, const char *file);
void shell_interact(tcl_shell_t *sh);
void shell_reset(tcl_shell_t *sh, tree_t top);

#endif  // _RT_SHELL_H
