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

#include <assert.h>

struct rt_alloc_stack {
   void   **stack;
   size_t stack_sz;
   size_t stack_top;
   size_t item_sz;
};

typedef struct rt_alloc_stack *rt_alloc_stack_t;

rt_alloc_stack_t rt_alloc_stack_new(size_t size);
void rt_alloc_stack_destroy(rt_alloc_stack_t stack);
void *rt_alloc_slow(rt_alloc_stack_t stack);

static inline void *rt_alloc(rt_alloc_stack_t s)
{
   if (unlikely(s->stack_top == 0))
      return rt_alloc_slow(s);
   else
      return s->stack[--s->stack_top];
}

static inline void rt_free(rt_alloc_stack_t s, void *ptr)
{
   assert(s->stack_top < s->stack_sz);

   s->stack[s->stack_top++] = ptr;
}

#endif  // _RT_ALLOC_H
