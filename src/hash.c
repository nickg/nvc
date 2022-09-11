//
//  Copyright (C) 2013-2021  Nick Gasson
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

#include "hash.h"

#include <stdlib.h>
#include <string.h>
#include <assert.h>

////////////////////////////////////////////////////////////////////////////////
// Hash table of pointers to pointers

struct _hash {
   unsigned     size;
   unsigned     members;
   void       **values;
   const void **keys;
};

static inline int hash_slot(unsigned size, const void *key)
{
   assert(key != NULL);

   uintptr_t uptr = (uintptr_t)key;

   // Bottom two bits will always be zero with 32-bit pointers
   uptr >>= 2;

   // Hash function from here:
   //   http://burtleburtle.net/bob/hash/integer.html

   uint32_t a = (uint32_t)uptr;
   a = (a ^ 61) ^ (a >> 16);
   a = a + (a << 3);
   a = a ^ (a >> 4);
   a = a * UINT32_C(0x27d4eb2d);
   a = a ^ (a >> 15);

   return a & (size - 1);
}

hash_t *hash_new(int size)
{
   hash_t *h = xmalloc(sizeof(hash_t));
   h->size    = next_power_of_2(size);
   h->members = 0;

   char *mem = xcalloc(h->size * 2 * sizeof(void *));
   h->values = (void **)mem;
   h->keys   = (const void **)(mem + (h->size * sizeof(void *)));

   return h;
}

void hash_free(hash_t *h)
{
   if (h != NULL) {
      free(h->values);
      free(h);
   }
}

bool hash_put(hash_t *h, const void *key, void *value)
{
   if (unlikely(h->members >= h->size / 2)) {
      // Rebuild the hash table with a larger size
      // This is expensive so a conservative initial size should be chosen

      const int old_size = h->size;
      h->size *= 2;

      const void **old_keys = h->keys;
      void **old_values = h->values;

      char *mem = xcalloc(h->size * 2 * sizeof(void *));
      h->values = (void **)mem;
      h->keys   = (const void **)(mem + (h->size * sizeof(void *)));

      h->members = 0;

      for (int i = 0; i < old_size; i++) {
         if (old_keys[i] != NULL)
            hash_put(h, old_keys[i], old_values[i]);
      }

      free(old_values);
   }

   int slot = hash_slot(h->size, key);

   for (; ; slot = (slot + 1) & (h->size - 1)) {
      if (h->keys[slot] == key) {
         h->values[slot] = value;
         return true;
      }
      else if (h->keys[slot] == NULL) {
         h->values[slot] = value;
         h->keys[slot] = key;
         h->members++;
         break;
      }
   }

   return false;
}

void hash_delete(hash_t *h, const void *key)
{
   int slot = hash_slot(h->size, key);

   for (; ; slot = (slot + 1) & (h->size - 1)) {
      if (h->keys[slot] == key) {
         h->values[slot] = NULL;
         return;
      }
      else if (h->keys[slot] == NULL)
         return;
   }
}

void *hash_get(hash_t *h, const void *key)
{
   int slot = hash_slot(h->size, key);

   for (; ; slot = (slot + 1) & (h->size - 1)) {
      if (h->keys[slot] == key)
         return h->values[slot];
      else if (h->keys[slot] == NULL)
         return NULL;
   }
}

bool hash_iter(hash_t *h, hash_iter_t *now, const void **key, void **value)
{
   assert(*now != HASH_END);

   while (*now < h->size) {
      const unsigned old = (*now)++;
      if (h->keys[old] != NULL) {
         *key   = h->keys[old];
         *value = h->values[old];
         return true;
      }
   }

   *now = HASH_END;
   return false;
}

unsigned hash_members(hash_t *h)
{
   return h->members;
}

////////////////////////////////////////////////////////////////////////////////
// Hash table of strings to pointers

struct _shash {
   unsigned   size;
   unsigned   members;
   void     **values;
   char     **keys;
};

static inline int shash_slot(shash_t *h, const char *key)
{
   assert(key != NULL);

   // DJB2 hash function from here:
   //   http://www.cse.yorku.ca/~oz/hash.html

   unsigned long hash = 5381;
   int c;

   while ((c = *key++))
      hash = ((hash << 5) + hash) + c;

   return hash & (h->size - 1);
}

shash_t *shash_new(int size)
{
   shash_t *h = xmalloc(sizeof(shash_t));
   h->size    = next_power_of_2(size);
   h->members = 0;

   char *mem = xcalloc(h->size * 2 * sizeof(void *));
   h->values = (void **)mem;
   h->keys   = (char **)(mem + (h->size * sizeof(void *)));

   return h;
}

void shash_free(shash_t *h)
{
   for (unsigned i = 0; i < h->size; i++) {
      if (h->keys[i] != NULL)
         free(h->keys[i]);
   }

   free(h->values);
   free(h);
}

static void shash_put_copy(shash_t *h, char *key, void *value)
{
   int slot = shash_slot(h, key);

   for (; ; slot = (slot + 1) & (h->size - 1)) {
      if (h->keys[slot] == NULL) {
         h->values[slot] = value;
         h->keys[slot] = key;
         h->members++;
         break;
      }
      else if (strcmp(h->keys[slot], key) == 0) {
         h->values[slot] = value;
         return;
      }
   }
}

void shash_put(shash_t *h, const char *key, void *value)
{
   if (unlikely(h->members >= h->size / 2)) {
      // Rebuild the hash table with a larger size

      const int old_size = h->size;
      h->size *= 2;

      char **old_keys = h->keys;
      void **old_values = h->values;

      char *mem = xcalloc(h->size * 2 * sizeof(void *));
      h->values = (void **)mem;
      h->keys   = (char **)(mem + (h->size * sizeof(void *)));

      h->members = 0;

      for (int i = 0; i < old_size; i++) {
         if (old_keys[i] != NULL)
            shash_put_copy(h, old_keys[i], old_values[i]);
      }

      free(old_values);
   }

   shash_put_copy(h, xstrdup(key), value);
}

void *shash_get(shash_t *h, const char *key)
{
   int slot = shash_slot(h, key);

   for (; ; slot = (slot + 1) & (h->size - 1)) {
      if (h->keys[slot] == NULL)
         return NULL;
      else if (strcmp(h->keys[slot], key) == 0)
         return h->values[slot];
   }
}

////////////////////////////////////////////////////////////////////////////////
// Hash of unsigned integers to pointers

struct _ihash {
   unsigned   size;
   unsigned   members;
   void     **values;
   uint64_t  *keys;
   uint64_t  *mask;
   uint64_t   cachekey;
   void      *cacheval;
};

static inline int ihash_slot(ihash_t *h, uint64_t key)
{
   key = (key ^ (key >> 30)) * UINT64_C(0xbf58476d1ce4e5b9);
   key = (key ^ (key >> 27)) * UINT64_C(0x94d049bb133111eb);
   key = key ^ (key >> 31);

   return key & (h->size - 1);
}

ihash_t *ihash_new(int size)
{
   ihash_t *h = xmalloc(sizeof(ihash_t));
   h->size    = next_power_of_2(size);
   h->members = 0;

   const size_t bytes =
      h->size * sizeof(void *) +
      h->size * sizeof(uint64_t) +
      (h->size + 63 / 64) * sizeof(uint64_t);

   char *mem = xcalloc(bytes);
   h->values = (void **)mem;
   h->keys   = (uint64_t *)(mem + (h->size * sizeof(void *)));
   h->mask   = (uint64_t *)(mem + (h->size * sizeof(void *))
                            + (h->size * sizeof(uint64_t)));

   return h;
}

void ihash_free(ihash_t *h)
{
   free(h->values);
   free(h);
}

void ihash_put(ihash_t *h, uint64_t key, void *value)
{
   if (unlikely(h->members >= h->size / 2)) {
      // Rebuild the hash table with a larger size

      const int old_size = h->size;
      h->size *= 2;

      uint64_t *old_keys = h->keys;
      uint64_t *old_mask = h->mask;
      void **old_values = h->values;

      const size_t bytes =
         h->size * sizeof(void *) +
         h->size * sizeof(uint64_t) +
         (ALIGN_UP(h->size, 64) / 64) * sizeof(uint64_t);

      char *mem = xcalloc(bytes);
      h->values = (void **)mem;
      h->keys   = (uint64_t *)(mem + (h->size * sizeof(void *)));
      h->mask   = (uint64_t *)(mem + (h->size * sizeof(void *))
                               + (h->size * sizeof(uint64_t)));

      h->members = 0;

      for (int i = 0; i < old_size; i++) {
         if (old_mask[i / 64] & (UINT64_C(1) << (i % 64)))
            ihash_put(h, old_keys[i], old_values[i]);
      }

      free(old_values);
   }

   h->cachekey = key;
   h->cacheval = value;

   int slot = ihash_slot(h, key);

   for (; ; slot = (slot + 1) & (h->size - 1)) {
      if (!(h->mask[slot / 64] & (UINT64_C(1) << (slot % 64)))) {
         h->values[slot] = value;
         h->keys[slot] = key;
         h->mask[slot / 64] |= (UINT64_C(1) << (slot % 64));
         h->members++;
         break;
      }
      else if (h->keys[slot] == key) {
         h->values[slot] = value;
         assert(h->mask[slot / 64] & (UINT64_C(1) << (slot % 64)));
         return;
      }
   }
}

void *ihash_get(ihash_t *h, uint64_t key)
{
   if (h->members > 0 && key == h->cachekey)
      return h->cacheval;

   h->cachekey = key;

   int slot = ihash_slot(h, key);

   for (; ; slot = (slot + 1) & (h->size - 1)) {
      if (!(h->mask[slot / 64] & (UINT64_C(1) << (slot % 64))))
         return (h->cacheval = NULL);
      else if (h->keys[slot] == key)
         return (h->cacheval = h->values[slot]);
   }
}

////////////////////////////////////////////////////////////////////////////////
// Set of pointers implemented as a hash table

struct _hset {
   unsigned     size;
   unsigned     members;
   const void **keys;
};

hset_t *hset_new(int size)
{
   hset_t *h = xmalloc(sizeof(hset_t));
   h->size    = next_power_of_2(size);
   h->members = 0;
   h->keys    = xcalloc_array(h->size, sizeof(void *));

   return h;
}

void hset_free(hset_t *h)
{
   free(h->keys);
   free(h);
}

void hset_insert(hset_t *h, const void *key)
{
   if (unlikely(h->members >= h->size / 2)) {
      const int old_size = h->size;
      h->size *= 2;

      const void **old_keys = h->keys;
      h->keys = xcalloc_array(h->size, sizeof(void *));

      h->members = 0;

      for (int i = 0; i < old_size; i++) {
         if (old_keys[i] != NULL)
            hset_insert(h, old_keys[i]);
      }

      free(old_keys);
   }

   int slot = hash_slot(h->size, key);

   for (; ; slot = (slot + 1) & (h->size - 1)) {
      if (h->keys[slot] == key)
         return;
      else if (h->keys[slot] == NULL) {
         h->keys[slot] = key;
         h->members++;
         break;
      }
   }
}

bool hset_contains(hset_t *h, const void *key)
{
   int slot = hash_slot(h->size, key);

   for (; ; slot = (slot + 1) & (h->size - 1)) {
      if (h->keys[slot] == key)
         return true;
      else if (h->keys[slot] == NULL)
         return false;
   }
}
