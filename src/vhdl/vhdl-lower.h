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

#ifndef _VHDL_LOWER_H
#define _VHDL_LOWER_H

#include "prim.h"

void vhdl_lower_predef(mir_unit_t *mu, object_t *obj);
void vhdl_lower_image_helper(mir_unit_t *mu, object_t *obj);

#endif  // _VHDL_LOWER_H
