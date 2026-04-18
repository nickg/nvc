//
//  Copyright (C) 2026  Nick Gasson
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

#ifndef _TCL_PRIV_H
#define _TCL_PRIV_H

#include "prim.h"

#undef DLLEXPORT
#include <tcl.h>

#if TCL_MAJOR_VERSION < 9
typedef int Tcl_Size;
#endif

void shell_add_cmd(tcl_shell_t *sh, const char *name, Tcl_ObjCmdProc fn,
                   const char *help);

void shell_add_vhpi_cmds(tcl_shell_t *sh);

#endif  // _TCL_PRIV_H
