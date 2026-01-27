//
//  Copyright (C) 2025  Nick Gasson
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

#ifndef _VHDL_UTIL_H
#define _VHDL_UTIL_H

#include "prim.h"

ident_t predef_func_name(type_t type, const char *op);

#define CANNOT_HANDLE(t) do {                                           \
      fatal_at(tree_loc(t), "cannot handle %s in %s" ,                  \
               tree_kind_str(tree_kind(t)), __FUNCTION__);              \
   } while (0)

#endif  // _VHDL_UTIL_H
