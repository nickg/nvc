//
//  Copyright (C) 2022-2024  Nick Gasson
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

#ifndef _MASK_H
#define _MASK_H

#include "prim.h"

#include <stdbool.h>
#include <stddef.h>
#include <sys/types.h>

typedef struct _bit_mask {
   size_t size;
   union {
      uint64_t bits;
      uint64_t *ptr;
   };
} bit_mask_t;

#define mask_test(m, bit) ({                                            \
         const size_t _bit = (bit);                                     \
         assert(_bit < (m)->size);                                      \
         (m)->size > 64                                                 \
            ? !!((m)->ptr[_bit / 64] & (UINT64_C(1) << (_bit % 64)))    \
            : !!((m)->bits & (UINT64_C(1) << (_bit % 64)));             \
      })

#define mask_set(m, bit) do {                                   \
      const size_t _bit = (bit);                                \
      assert(_bit < (m)->size);                                 \
      if ((m)->size > 64)                                       \
         (m)->ptr[_bit / 64] |= (UINT64_C(1) << (_bit % 64));   \
      else                                                      \
         (m)->bits |= UINT64_C(1) << (_bit % 64);               \
   } while (0)

#define mask_clear(m, bit) do {                                 \
      const size_t _bit = (bit);                                \
      assert(_bit < (m)->size);                                 \
      if ((m)->size > 64)                                       \
         (m)->ptr[_bit / 64] &= ~(UINT64_C(1) << (_bit % 64));  \
      else                                                      \
         (m)->bits &= ~(UINT64_C(1) << (_bit % 64));            \
   } while (0)

#define LOCAL_BIT_MASK __attribute__((cleanup(mask_free))) bit_mask_t

void mask_init(bit_mask_t *m, size_t size);
void mask_free(bit_mask_t *m);
void mask_clear_range(bit_mask_t *m, size_t start, size_t count);
void mask_set_range(bit_mask_t *m, size_t start, size_t count);
bool mask_test_range(bit_mask_t *m, size_t start, size_t count);
size_t mask_popcount(const bit_mask_t *m);
void mask_setall(bit_mask_t *m);
void mask_clearall(bit_mask_t *m);
bool mask_test_and_set(bit_mask_t *m, size_t bit);
ssize_t mask_scan_backwards(bit_mask_t *m, size_t bit);
size_t mask_count_clear(bit_mask_t *m, size_t bit);
void mask_subtract(bit_mask_t *m, const bit_mask_t *m2);
void mask_union(bit_mask_t *m, const bit_mask_t *m2);
void mask_intersect(bit_mask_t *m, const bit_mask_t *m2);
void mask_copy(bit_mask_t *m, const bit_mask_t *m2);
bool mask_eq(const bit_mask_t *m1, const bit_mask_t *m2);
bool mask_iter(const bit_mask_t *m, size_t *bit);

#endif  // _MASK_H
