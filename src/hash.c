//
//  Copyright (C) 2013-2025  Nick Gasson
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
#include "thread.h"

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
   return mix_bits_64((uintptr_t)key) & (size - 1);
}

hash_t *hash_new(int size)
{
   assert(size > 0);

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
   if (unlikely(h->members > h->size / 2)) {
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
         if (old_keys[i] != NULL && old_values[i] != NULL) {
            int slot = hash_slot(h->size, old_keys[i]);

            for (; ; slot = (slot + 1) & (h->size - 1)) {
               if (h->keys[slot] == NULL) {
                  h->values[slot] = old_values[i];
                  h->keys[slot] = old_keys[i];
                  h->members++;
                  break;
               }
               else
                  assert(h->keys[slot] != old_keys[i]);
            }
         }
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
      if (h->keys[old] != NULL && h->values[old] != NULL) {
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

   uint32_t hash = 5381;
   int c;

   while ((c = *key++))
      hash = ((hash << 5) + hash) + c;

   return mix_bits_32(hash) & (h->size - 1);
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
   if (h == NULL)
      return;

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
   if (unlikely(h->members > h->size / 2)) {
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

void shash_delete(shash_t *h, const void *key)
{
   int slot = shash_slot(h, key);

   for (; ; slot = (slot + 1) & (h->size - 1)) {
      if (h->keys[slot] == NULL)
         return;
      else if (strcmp(h->keys[slot], key) == 0) {
         h->values[slot] = NULL;
         return;
      }
   }
}

bool shash_iter(shash_t *h, hash_iter_t *now, const char **key, void **value)
{
   assert(*now != HASH_END);

   while (*now < h->size) {
      const unsigned old = (*now)++;
      if (h->keys[old] != NULL && h->values[old] != NULL) {
         *key   = h->keys[old];
         *value = h->values[old];
         return true;
      }
   }

   *now = HASH_END;
   return false;
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
   if (h != NULL) {
      free(h->values);
      free(h);
   }
}

void ihash_put(ihash_t *h, uint64_t key, void *value)
{
   if (unlikely(h->members > h->size / 2)) {
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
   if (h != NULL) {
      free(h->keys);
      free(h);
   }
}

void hset_insert(hset_t *h, const void *key)
{
   if (unlikely(h->members > h->size / 2)) {
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

////////////////////////////////////////////////////////////////////////////////
// Hash table that supports concurrent updates

typedef struct _chash_node chash_node_t;

typedef struct {
   void *tagged_key;
   void *value;
} chash_slot_t;

typedef struct {
   size_t       size;
   chash_slot_t slots[0];
} chash_tab_t;

struct _chash_node {
   chash_node_t *chain;
   const void   *key;
   void         *value;
};

struct _chash {
   chash_tab_t *tab;
   chash_tab_t *resizing;
   size_t       members;
};

typedef enum {
   FREE_MARKER,
   BUSY_MARKER,
   INUSE_MARKER,
   MOVED_MARKER,
} chash_marker_t;

chash_t *chash_new(int size)
{
   const int roundup = next_power_of_2(size);

   chash_tab_t *tab =
      xcalloc_flex(sizeof(chash_tab_t), roundup, sizeof(chash_slot_t));
   tab->size = roundup;

   chash_t *h = xcalloc(sizeof(chash_t));
   store_release(&h->resizing, tab);
   store_release(&h->tab, tab);

   return h;
}

static void chash_wait_for_resize(chash_t *h)
{
   for (;;) {
      chash_tab_t *from = atomic_load(&h->tab);
      chash_tab_t *to = atomic_load(&h->resizing);

      if (from == to)
         break;

      thread_sleep(10);
   }
}

void chash_free(chash_t *h)
{
   if (h == NULL)
      return;

   chash_wait_for_resize(h);

   free(h->tab);
   free(h);
}

static void chash_copy_table(chash_tab_t *from, chash_tab_t *to)
{
   for (int i = 0; i < from->size; i++) {
      for (;;) {
         void *const tagged_key = load_acquire(&from->slots[i].tagged_key);
         const chash_marker_t marker = pointer_tag(tagged_key);
         const void *key = untag_pointer(tagged_key, void);
         switch (marker) {
         case BUSY_MARKER:
            spin_wait();
            continue;
         case INUSE_MARKER:
            {
               for (int j = hash_slot(to->size, key);;
                    j = (j + 1) & (to->size - 1)) {
                  void *exist = load_acquire(&(to->slots[j].tagged_key));
                  if (exist == NULL) {
                     to->slots[j].value = from->slots[i].value;
                     store_release(&(to->slots[j].tagged_key), tagged_key);
                     break;
                  }
                  else
                     assert(exist != tagged_key);
               }
            }
            break;
         case FREE_MARKER:
            break;
         default:
            should_not_reach_here();
         }

         void *moved = tag_pointer(key, MOVED_MARKER);
         if (atomic_cas(&(from->slots[i].tagged_key), tagged_key, moved))
            break;
         else
            assert(marker == FREE_MARKER);   // Raced to move free slot
      }
   }
}

static void chash_grow_table(chash_t *h, chash_tab_t *cur)
{
   chash_tab_t *newtab =
      xcalloc_flex(sizeof(chash_tab_t), cur->size * 2, sizeof(chash_slot_t));
   newtab->size = cur->size * 2;

   if (atomic_cas(&h->resizing, cur, newtab)) {
      chash_copy_table(cur, newtab);

      assert(atomic_load(&h->resizing) == newtab);

      if (!atomic_cas(&h->tab, cur, newtab))
         should_not_reach_here();

      async_free(cur);
   }
   else {
      free(newtab);
      chash_wait_for_resize(h);
   }
}

static bool chash_try_put(chash_t *h, const void *key, void *value)
{
   chash_tab_t *tab = load_acquire(&h->tab);
   assert(is_power_of_2(tab->size));

   if (relaxed_load(&h->members) > tab->size / 2) {
      chash_grow_table(h, tab);
      return false;
   }

   for (int i = hash_slot(tab->size, key);; i = (i + 1) & (tab->size - 1)) {
      void *const tagged_key = load_acquire(&tab->slots[i].tagged_key);
      const chash_marker_t marker = pointer_tag(tagged_key);

      switch (marker) {
      case BUSY_MARKER:
         spin_wait();
         return false;
      case FREE_MARKER:
         {
            void *busy = tag_pointer(NULL, BUSY_MARKER);
            if (atomic_cas(&tab->slots[i].tagged_key, NULL, busy)) {
               tab->slots[i].value = value;
               void *inuse = tag_pointer(key, INUSE_MARKER);
               store_release(&tab->slots[i].tagged_key, inuse);
               relaxed_add(&h->members, 1);
               return true;
            }
            else {
               spin_wait();
               return false;
            }
         }
      case INUSE_MARKER:
         if (untag_pointer(tagged_key, void) == key) {
            // Must CAS to busy here to avoid lost update due to
            // concurrent move
            void *busy = tag_pointer(key, BUSY_MARKER);
            if (atomic_cas(&tab->slots[i].tagged_key, tagged_key, busy)) {
               tab->slots[i].value = value;
               store_release(&tab->slots[i].tagged_key, tagged_key);
               return true;
            }
            else {
               spin_wait();
               return false;
            }
         }
         else
            break;
      case MOVED_MARKER:
         chash_wait_for_resize(h);
         return false;
      }
   }
}

void chash_put(chash_t *h, const void *key, void *value)
{
   while (!chash_try_put(h, key, value));
}

static bool chash_try_cas(chash_t *h, const void *key, void *cmp, void **value)
{
   chash_tab_t *tab = load_acquire(&h->tab);
   assert(is_power_of_2(tab->size));

   if (relaxed_load(&h->members) > tab->size / 2) {
      chash_grow_table(h, tab);
      return false;
   }

   for (int i = hash_slot(tab->size, key);; i = (i + 1) & (tab->size - 1)) {
      void *const tagged_key = load_acquire(&tab->slots[i].tagged_key);
      const chash_marker_t marker = pointer_tag(tagged_key);

      switch (marker) {
      case BUSY_MARKER:
         spin_wait();
         return false;
      case INUSE_MARKER:
         if (untag_pointer(tagged_key, void) != key)
            break;
         // Fall-through
      case FREE_MARKER:
         {
            void *busy = tag_pointer(key, BUSY_MARKER);
            if (atomic_cas(&tab->slots[i].tagged_key, tagged_key, busy)) {
               void *new = *value;
               if ((*value = tab->slots[i].value) == cmp)
                  tab->slots[i].value = new;
               void *inuse = tag_pointer(key, INUSE_MARKER);
               store_release(&tab->slots[i].tagged_key, inuse);
               return true;
            }
            else {
               spin_wait();
               return false;
            }
         }
      case MOVED_MARKER:
         chash_wait_for_resize(h);
         return false;
      }
   }
}

void *chash_cas(chash_t *h, const void *key, void *cmp, void *value)
{
   while (!chash_try_cas(h, key, cmp, &value));
   return value;
}

static bool chash_try_get(chash_t *h, const void *key, void **value)
{
   chash_tab_t *tab = load_acquire(&h->tab);
   assert(is_power_of_2(tab->size));

   for (int i = hash_slot(tab->size, key);; i = (i + 1) & (tab->size - 1)) {
      void *const tagged_key = load_acquire(&tab->slots[i].tagged_key);
      const chash_marker_t marker = pointer_tag(tagged_key);

      switch (marker) {
      case BUSY_MARKER:
         spin_wait();
         return false;
      case FREE_MARKER:
         *value = NULL;
         return true;
      case INUSE_MARKER:
         if (untag_pointer(tagged_key, void) == key) {
            *value = tab->slots[i].value;
            return true;
         }
         else
            break;
      case MOVED_MARKER:
         chash_wait_for_resize(h);
         return false;
      }
   }
}

void *chash_get(chash_t *h, const void *key)
{
   void *value;
   while (!chash_try_get(h, key, &value));
   return value;
}

void chash_iter(chash_t *h, hash_iter_fn_t fn)
{
   chash_tab_t *tab = load_acquire(&h->tab);
   for (int pos = 0; pos < tab->size;) {
      void *const tagged_key = load_acquire(&tab->slots[pos].tagged_key);
      const chash_marker_t marker = pointer_tag(tagged_key);

      switch (marker) {
      case BUSY_MARKER:
         spin_wait();
         continue;
      case FREE_MARKER:
         break;
      case MOVED_MARKER:
      case INUSE_MARKER:
         (*fn)(untag_pointer(tagged_key, void), tab->slots[pos].value);
         break;
      }

      pos++;
   }
}

////////////////////////////////////////////////////////////////////////////////
// Generic hash table

struct _ghash {
   unsigned          size;
   unsigned          members;
   ghash_hash_fn_t   hash_fn;
   ghash_cmp_fn_t    cmp_fn;
   void            **values;
   const void      **keys;
   uint32_t         *hashes;
};

static void ghash_alloc(ghash_t *h)
{
   size_t size = h->size * 2 * sizeof(void *) + h->size * sizeof(uint32_t);
   char *mem = xcalloc(size);
   h->values = (void **)mem;
   h->keys   = (const void **)(mem + (h->size * sizeof(void *)));
   h->hashes = (uint32_t *)(mem + (2 * h->size * sizeof(void *)));
}

static inline uint32_t ghash_hash_fn(ghash_t *h, const void *key)
{
   assert(key != NULL);
   return mix_bits_32((*h->hash_fn)(key));
}

ghash_t *ghash_new(int size, ghash_hash_fn_t hash_fn, ghash_cmp_fn_t cmp_fn)
{
   ghash_t *h = xmalloc(sizeof(ghash_t));
   h->size    = next_power_of_2(size);
   h->members = 0;
   h->hash_fn = hash_fn;
   h->cmp_fn  = cmp_fn;

   ghash_alloc(h);

   return h;
}

void ghash_free(ghash_t *h)
{
   if (h != NULL) {
      free(h->values);
      free(h);
   }
}

void ghash_put(ghash_t *h, const void *key, void *value)
{
   if (unlikely(h->members > h->size / 2)) {
      // Rebuild the hash table with a larger size

      const int old_size = h->size;
      h->size *= 2;

      const void **old_keys = h->keys;
      void **old_values = h->values;
      uint32_t *old_hashes = h->hashes;

      ghash_alloc(h);

      h->members = 0;

      for (int i = 0; i < old_size; i++) {
         if (old_keys[i] != NULL && old_values[i] != NULL) {
            int slot = old_hashes[i] & (h->size - 1);

            for (; ; slot = (slot + 1) & (h->size - 1)) {
               if (h->keys[slot] == NULL) {
                  h->values[slot] = old_values[i];
                  h->keys[slot] = old_keys[i];
                  h->hashes[slot] = old_hashes[i];
                  h->members++;
                  break;
               }
               else
                  assert(h->keys[slot] != old_keys[i]);
            }
         }
      }

      free(old_values);
   }

   uint32_t hash = ghash_hash_fn(h, key);
   int slot = hash & (h->size - 1);

   for (; ; slot = (slot + 1) & (h->size - 1)) {
      if (h->keys[slot] == NULL) {
         h->values[slot] = value;
         h->keys[slot] = key;
         h->hashes[slot] = hash;
         h->members++;
         break;
      }
      else if (h->hashes[slot] != hash)
         continue;
      else if (h->keys[slot] == key || (*h->cmp_fn)(h->keys[slot], key)) {
         h->values[slot] = value;
         break;
      }
   }
}

void *ghash_get(ghash_t *h, const void *key)
{
   uint32_t hash = ghash_hash_fn(h, key);
   int slot = hash & (h->size - 1);

   for (; ; slot = (slot + 1) & (h->size - 1)) {
      if (h->keys[slot] == NULL)
         return NULL;
      else if (h->hashes[slot] != hash)
         continue;
      else if (h->keys[slot] == key || (*h->cmp_fn)(h->keys[slot], key))
         return h->values[slot];
   }
}

void ghash_delete(ghash_t *h, const void *key)
{
   uint32_t hash = ghash_hash_fn(h, key);
   int slot = hash & (h->size - 1);

   for (; ; slot = (slot + 1) & (h->size - 1)) {
      if (h->keys[slot] == NULL)
         return;
      else if (h->hashes[slot] != hash)
         continue;
      else if (h->keys[slot] == key || (*h->cmp_fn)(h->keys[slot], key)) {
         h->values[slot] = NULL;
         h->hashes[slot] = 0;
         return;
      }
   }
}
