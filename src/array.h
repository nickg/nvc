//
//  Copyright (C) 2012  Nick Gasson
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

#ifndef _ARRAY_H
#define _ARRAY_H

#define ARRAY_BASE_SZ 8

// Generic array template used in type and tree code
#define DEFINE_ARRAY(what)                                     \
   typedef struct {                                            \
      uint32_t  count;                                         \
      uint32_t  _max;                                          \
      what##_t *items;                                         \
   } what##_array_t;                                           \
                                                               \
   static void what##_array_add(what##_array_t *a, what##_t t) \
   {                                                           \
      if (a->_max == 0) {                                      \
         a->items = xmalloc(sizeof(what##_t) * ARRAY_BASE_SZ); \
         a->_max   = ARRAY_BASE_SZ;                            \
      }                                                        \
      else if (a->count == a->_max) {                          \
         a->_max *= 2;                                         \
         a->items = xrealloc(a->items,                         \
                             sizeof(what##_t) * a->_max);      \
      }                                                        \
                                                               \
      a->items[a->count++] = t;                                \
   }                                                           \
                                                               \
   static inline what##_t what##_array_nth(what##_array_t *a,  \
                                           unsigned n)         \
   {                                                           \
      assert(n < a->count);                                    \
      return a->items[n];                                      \
   }                                                           \
                                                               \
   __attribute__((unused))                                     \
   static void what##_array_resize(what##_array_t *a,          \
                                   size_t n)                   \
   {                                                           \
      a->count = a->_max = n;                                  \
      a->items = xrealloc(a->items, n * sizeof(what##_t));     \
   }                                                           \

#endif  // _ARRAY_H
