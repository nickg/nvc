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

#ifndef _JIT_LAYOUT_H
#define _JIT_LAYOUT_H

#include "prim.h"

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

const jit_layout_t *layout_of(type_t type);
const jit_layout_t *signal_layout_of(type_t type);

#endif   // _JIT_LAYOUT_H
