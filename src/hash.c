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
   bool         replace;
   void       **values;
   const void **keys;
};

static inline int hash_slot(hash_t *h, const void *key)
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

   return a & (h->size - 1);
}

hash_t *hash_new(int size, bool replace)
{
   hash_t *h = xmalloc(sizeof(hash_t));
   h->size    = next_power_of_2(size);
   h->members = 0;
   h->replace = replace;

   char *mem = xcalloc(h->size * 2 * sizeof(void *));
   h->values = (void **)mem;
   h->keys   = (const void **)(mem + (h->size * sizeof(void *)));

   return h;
}

void hash_free(hash_t *h)
{
   free(h->values);
   free(h);
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

   int slot = hash_slot(h, key);

   for (; ; slot = (slot + 1) & (h->size - 1)) {
      if ((h->keys[slot] == key) && h->replace) {
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

void *hash_get_nth(hash_t *h, const void *key, int *n)
{
   int slot = hash_slot(h, key);

   for (; ; slot = (slot + 1) & (h->size - 1)) {
      if (h->keys[slot] == key) {
         if (*n == 0)
            return h->values[slot];
         else
            --(*n);
      }
      else if (h->keys[slot] == NULL)
         return NULL;
   }
}

void *hash_get(hash_t *h, const void *key)
{
   int n = 0;
   return hash_get_nth(h, key, &n);
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
