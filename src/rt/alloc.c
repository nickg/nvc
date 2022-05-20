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

#include "alloc.h"

#include <assert.h>
#include <stdlib.h>

// Profiling shows a large proportion of simulation time is spent in
// malloc and free. These routines provide a stack-based fixed-size
// allocator that is faster and has better cache locality.

#define INIT_ITEMS 128

struct rt_chunk {
   rt_chunk_t *next;
   char        data[0];
};

static void rt_alloc_add_objects(rt_alloc_stack_t s, size_t n)
{
   rt_chunk_t *c = xmalloc_flex(sizeof(rt_chunk_t), n, s->item_sz);
   c->next = s->chunks;

   char *p = c->data;
   for (int i = 0; i < n; i++, p += s->item_sz)
      rt_free(s, p);

   s->chunks = c;
}

rt_alloc_stack_t rt_alloc_stack_new(size_t size, const char *name)
{
   struct rt_alloc_stack *s = xmalloc(sizeof(struct rt_alloc_stack));
   s->stack     = xmalloc_array(INIT_ITEMS, sizeof(void *));
   s->stack_sz  = INIT_ITEMS;
   s->stack_top = 0;
   s->item_sz   = size;
   s->name      = name;
   s->chunks    = NULL;

   rt_alloc_add_objects(s, INIT_ITEMS);

   return s;
}

void rt_alloc_stack_destroy(rt_alloc_stack_t s)
{
#ifndef NDEBUG
   if (s->stack_top != s->stack_sz)
      fatal("memory leak of %zu items from %s stack",
            s->stack_sz - s->stack_top, s->name);
#endif

   while (s->chunks != NULL) {
      rt_chunk_t *tmp = s->chunks->next;
      free(s->chunks);
      s->chunks = tmp;
   }

   free(s->stack);
   free(s);
}

void *rt_alloc_slow(rt_alloc_stack_t s)
{
   if (s->stack_top == 0) {
      s->stack_sz *= 2;
      s->stack = xrealloc(s->stack, sizeof(void *) * s->stack_sz);

      rt_alloc_add_objects(s, s->stack_sz / 2);
   }

   return s->stack[--s->stack_top];
}
