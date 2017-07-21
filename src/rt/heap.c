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

#include "util.h"
#include "heap.h"

#include <inttypes.h>
#include <stdlib.h>

#define PARENT(i) (i >> 1)
#define LEFT(i)   (i << 1)
#define RIGHT(i)  ((i << 1) + 1)

struct node {
   void     *user;
   uint64_t key;
};

struct heap {
   struct node *nodes;
   size_t    size;
   size_t    max_size;
};

#define NODE(h, i) (h->nodes[i - 1])
#define KEY(h, i)  (NODE(h, i).key)
#define USER(h, i) (NODE(h, i).user)

static inline void exchange(heap_t h, size_t i, size_t j)
{
   struct node tmp = NODE(h, j);
   NODE(h, j) = NODE(h, i);
   NODE(h, i) = tmp;
}

static void min_heapify(heap_t h, size_t i)
{
   for (;;) {
      const size_t l = LEFT(i);
      const size_t r = RIGHT(i);

      size_t smallest;
      if (l <= h->size && KEY(h, l) < KEY(h, i))
         smallest = l;
      else
         smallest = i;

      if (r <= h->size && KEY(h, r) < KEY(h, smallest))
         smallest = r;

      if (smallest == i)
         break;

      exchange(h, i, smallest);
      i = smallest;
   }
}

static inline void heap_decrease_key(heap_t h, size_t i, uint64_t key)
{
   if (unlikely(key > KEY(h, i)))
      fatal("new key is larger than current key") LCOV_EXCL_LINE;

   KEY(h, i) = key;
   while (i > 1 && KEY(h, PARENT(i)) > KEY(h, i)) {
      exchange(h, i, PARENT(i));
      i = PARENT(i);
   }
}

heap_t heap_new(size_t init_size)
{
   struct heap *h = xmalloc(sizeof(struct heap));
   h->nodes    = xmalloc(init_size * sizeof(struct node));
   h->max_size = init_size;
   h->size     = 0;
   return h;
}

void heap_free(heap_t h)
{
   free(h->nodes);
   free(h);
}

void *heap_extract_min(heap_t h)
{
   if (unlikely(h->size < 1))
      fatal_trace("heap underflow") LCOV_EXCL_LINE;

   void *min = USER(h, 1);
   NODE(h, 1) = NODE(h, h->size);
   --(h->size);
   min_heapify(h, 1);
   return min;
}

void *heap_min(heap_t h)
{
   if (unlikely(h->size < 1))
      fatal_trace("heap underflow") LCOV_EXCL_LINE;

   return USER(h, 1);
}

void heap_insert(heap_t h, uint64_t key, void *user)
{
   if (unlikely(h->size == h->max_size)) {
      h->max_size *= 2;
      h->nodes = xrealloc(h->nodes, h->max_size * sizeof(struct node));
   }

   ++(h->size);

   KEY(h, h->size) = UINT64_MAX;
   USER(h, h->size) = user;

   heap_decrease_key(h, h->size, key);
}

size_t heap_size(heap_t h)
{
   return h->size;
}

void heap_walk(heap_t h, heap_walk_fn_t fn, void *context)
{
   for (size_t i = 1; i <= h->size; i++)
      (*fn)(KEY(h, i), USER(h, i), context);
}
