//
//  Copyright (C) 2013-2015  Nick Gasson
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

#ifndef _HASH_H
#define _HASH_H

#include "util.h"

#include <limits.h>

typedef struct hash hash_t;

typedef unsigned hash_iter_t;

#define HASH_BEGIN 0
#define HASH_END   UINT_MAX

hash_t *hash_new(int size, bool replace);
void hash_free(hash_t *h);
bool hash_put(hash_t *h, const void *key, void *value);
void *hash_get(hash_t *h, const void *key);
void *hash_get_nth(hash_t *h, const void *key, int *n);
void hash_replace(hash_t *h, void *value, void *with);
bool hash_iter(hash_t *h, hash_iter_t *now, const void **key, void **value);
unsigned hash_members(hash_t *h);

#endif  // _HASH_H
