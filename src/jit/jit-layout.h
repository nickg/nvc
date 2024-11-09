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
#include "util.h"

typedef enum {
   LC_DATA,
   LC_BOUNDS,
   LC_OFFSET,
   LC_EXTERNAL,
} layout_class_t;

typedef struct {
   uint32_t offset;
   uint32_t size;
   uint32_t repeat;
   uint16_t align;
   uint16_t class;
} layout_part_t;

STATIC_ASSERT(sizeof(layout_part_t) == 16);

typedef struct {
   int nparts;
   int size;
   int align;
   layout_part_t parts[0];
} jit_layout_t;

const jit_layout_t *layout_of(type_t type);
const jit_layout_t *signal_layout_of(type_t type);

#endif   // _JIT_LAYOUT_H
