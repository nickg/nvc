//
//  Copyright (C) 2022  Nick Gasson
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

typedef struct {
   size_t size;
   union {
      uint64_t bits;
      uint64_t *ptr;
   };
} bit_mask_t;

#define mask_test(m, bit) ({                                            \
         const int _bit = (bit);                                        \
         assert(_bit < (m)->size);                                      \
         (m)->size > 64                                                 \
            ? !!((m)->ptr[_bit / 64] & (UINT64_C(1) << (_bit % 64)))    \
            : !!((m)->bits & (UINT64_C(1) << (_bit % 64)));             \
      })

#define mask_set(m, bit) do {                                   \
      const int _bit = (bit);                                   \
      assert(_bit < (m)->size);                                 \
      if ((m)->size > 64)                                       \
         (m)->ptr[_bit / 64] |= (UINT64_C(1) << (_bit % 64));   \
      else                                                      \
         (m)->bits |= UINT64_C(1) << (_bit % 64);               \
   } while (0)

#define mask_clear(m, bit) do {                                 \
      const int _bit = (bit);                                   \
      assert(_bit < (m)->size);                                 \
      if ((m)->size > 64)                                       \
         (m)->ptr[_bit / 64] &= ~(UINT64_C(1) << (_bit % 64));  \
      else                                                      \
         (m)->bits &= ~(UINT64_C(1) << (_bit % 64));            \
   } while (0)

void mask_init(bit_mask_t *m, size_t size);
void mask_free(bit_mask_t *m);
void mask_clear_range(bit_mask_t *m, int start, int count);
void mask_set_range(bit_mask_t *m, int start, int count);
int mask_popcount(bit_mask_t *m);
void mask_setall(bit_mask_t *m);
void mask_clearall(bit_mask_t *m);
int mask_scan_backwards(bit_mask_t *m, int bit);
int mask_count_clear(bit_mask_t *m, int bit);
void mask_subtract(bit_mask_t *m, const bit_mask_t *m2);
void mask_union(bit_mask_t *m, const bit_mask_t *m2);
void mask_copy(bit_mask_t *m, const bit_mask_t *m2);
bool mask_eq(const bit_mask_t *m1, const bit_mask_t *m2);

#endif  // _MASK_H
