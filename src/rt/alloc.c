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

rt_alloc_stack_t rt_alloc_stack_new(size_t size)
{
   struct rt_alloc_stack *s = xmalloc(sizeof(struct rt_alloc_stack));
   s->stack     = xmalloc(sizeof(void *) * INIT_ITEMS);
   s->stack_sz  = INIT_ITEMS;
   s->stack_top = 0;
   s->item_sz   = size;

   for (int i = 0; i < INIT_ITEMS; i++)
      rt_free(s, xmalloc(size));

   return s;
}

void rt_alloc_stack_destroy(rt_alloc_stack_t s)
{
   if (s->stack_top != s->stack_sz)
      fatal("memory leak of %zu items from %zu byte stack",
            s->stack_sz - s->stack_top, s->item_sz);

   for (size_t i = 0; i < s->stack_sz; i++)
      free(s->stack[i]);

   free(s->stack);
   free(s);
}

void *rt_alloc_slow(rt_alloc_stack_t s)
{
   if (s->stack_top == 0) {
      s->stack_sz *= 2;
      s->stack = xrealloc(s->stack, sizeof(void *) * s->stack_sz);

      for (size_t i = 0; i < s->stack_sz / 2; i++)
         rt_free(s, xmalloc(s->item_sz));
   }

   return s->stack[--s->stack_top];
}

