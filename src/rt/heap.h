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

#ifndef _HEAP_H
#define _HEAP_H

#include <stddef.h>
#include <stdint.h>

typedef struct heap *heap_t;

typedef void (*heap_walk_fn_t)(uint64_t key, void *user, void *context);

heap_t heap_new(size_t init_size);
void heap_free(heap_t h);
void *heap_extract_min(heap_t h);
void *heap_min(heap_t h);
void heap_insert(heap_t h, uint64_t key, void *user);
size_t heap_size(heap_t h);
void heap_walk(heap_t h, heap_walk_fn_t fn, void *context);

#endif
