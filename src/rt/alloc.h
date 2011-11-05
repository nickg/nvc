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

#ifndef _RT_ALLOC_H
#define _RT_ALLOC_H

#include "util.h"

typedef struct rt_alloc_stack *rt_alloc_stack_t;

rt_alloc_stack_t rt_alloc_stack_new(size_t size);
void *rt_alloc(rt_alloc_stack_t stack);
void rt_free(rt_alloc_stack_t stack, void *ptr);

#endif  // _RT_ALLOC_H
