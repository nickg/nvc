//
//  Copyright (C) 2013-2022  Nick Gasson
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
#include "prim.h"

#include <limits.h>

typedef unsigned hash_iter_t;

#define HASH_BEGIN 0
#define HASH_END   UINT_MAX

hash_t *hash_new(int size);
void hash_free(hash_t *h);
bool hash_put(hash_t *h, const void *key, void *value);
void *hash_get(hash_t *h, const void *key);
void hash_delete(hash_t *h, const void *key);
bool hash_iter(hash_t *h, hash_iter_t *now, const void **key, void **value);
unsigned hash_members(hash_t *h);

shash_t *shash_new(int size);
void shash_free(shash_t *h);
void shash_put(shash_t *h, const char *key, void *value);
void *shash_get(shash_t *h, const char *key);

ihash_t *ihash_new(int size);
void ihash_free(ihash_t *h);
void ihash_put(ihash_t *h, uint64_t key, void *value);
void *ihash_get(ihash_t *h, uint64_t key);

hset_t *hset_new(int size);
void hset_free(hset_t *h);
void hset_insert(hset_t *h, const void *key);
bool hset_contains(hset_t *h, const void *key);

#endif  // _HASH_H
