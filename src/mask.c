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

#include "util.h"
#include "mask.h"

#include <assert.h>
#include <stdlib.h>
#include <string.h>

void mask_init(bit_mask_t *m, size_t size)
{
   m->size = size;
   if (size > 64)
      m->ptr = xcalloc_array((size + 63) / 64, sizeof(uint64_t));
   else
      m->bits = 0;
}

void mask_free(bit_mask_t *m)
{
   if (m->size > 64)
      free(m->ptr);

   m->bits = 0;
   m->size = 0;
}

static inline uint64_t mask_for_range(int low, int high)
{
   assert(low >= 0 && low < 64);
   assert(high >= 0 && high < 64);
   assert(low <= high);

   uint64_t mask = ~UINT64_C(0);
   if (high < 63)
      mask &= (UINT64_C(1) << (high + 1)) - 1;
   if (low > 0)
      mask &= ~((UINT64_C(1) << low) - 1);

   assert(__builtin_popcountll(mask) == high - low + 1);
   return mask;
}

void mask_clear_range(bit_mask_t *m, int start, int count)
{
   if (m->size <= 64) {
      m->bits &= ~mask_for_range(start, start + count - 1);
      return;
   }

   if (count > 0 && start % 64 != 0) {
      // Pre-loop: clear range of bits in first 64-bit word
      const int low = start % 64;
      const int high = MIN(low + count - 1, 63);
      const int nbits = high - low + 1;
      m->ptr[start / 64] &= ~mask_for_range(low, high);
      start += nbits;
      count -= nbits;
   }

   if (count > 0) {
      // Main loop: clear complete 64-bit words
      for (; count >= 64; count -= 64, start += 64)
         m->ptr[start / 64] = 0;
   }

   if (count > 0) {
      // Post-loop: clear range of bits in last 64-bit word
      assert(start % 64 == 0);
      const int high = MIN(count - 1, 63);
      m->ptr[start / 64] &= ~mask_for_range(0, high);
      assert(count == high + 1);
   }
}

void mask_set_range(bit_mask_t *m, int start, int count)
{
   if (m->size <= 64) {
      m->bits |= mask_for_range(start, start + count - 1);
      return;
   }

   if (count > 0 && start % 64 != 0) {
      // Pre-loop: set range of bits in first 64-bit word
      const int low = start % 64;
      const int high = MIN(low + count - 1, 63);
      const int nbits = high - low + 1;
      m->ptr[start / 64] |= mask_for_range(low, high);
      start += nbits;
      count -= nbits;
   }

   if (count > 0) {
      // Main loop: set complete 64-bit words
      for (; count >= 64; count -= 64, start += 64)
         m->ptr[start / 64] = ~UINT64_C(0);
   }

   if (count > 0) {
      // Post-loop: set range of bits in last 64-bit word
      assert(start % 64 == 0);
      const int high = MIN(count - 1, 63);
      m->ptr[start / 64] |= mask_for_range(0, high);
      assert(count == high + 1);
   }
}

int mask_popcount(bit_mask_t *m)
{
   if (m->size > 64) {
      int sum = 0;
      for (int i = 0; i < (m->size + 63) / 64; i++)
         sum += __builtin_popcountll(m->ptr[i]);
      return sum;
   }
   else
      return __builtin_popcountll(m->bits);
}

void mask_setall(bit_mask_t *m)
{
   mask_set_range(m, 0, m->size);
}

void mask_clearall(bit_mask_t *m)
{
   mask_clear_range(m, 0, m->size);
}

int mask_scan_backwards(bit_mask_t *m, int bit)
{
   if (m->size <= 64) {
      uint64_t word0 = m->bits & mask_for_range(0, bit);
      return word0 == 0 ? -1 : 63 - __builtin_clzll(word0);
   }

   uint64_t word0 = m->ptr[bit / 64];
   word0 &= mask_for_range(0, bit % 64);

   if (word0 != 0)
      return (bit | 63) - __builtin_clzll(word0);

   bit -= bit % 64 + 1;

   for (; bit > 0 && m->ptr[bit / 64] == 0; bit -= 64);

   if (bit > 0)
      return (bit | 63) - __builtin_clzll(m->ptr[bit / 64]);

   return -1;
}

int mask_count_clear(bit_mask_t *m, int bit)
{
   assert(bit < m->size);

   if (m->size <= 64) {
      const int fs = __builtin_ffsll(m->bits & mask_for_range(bit, 63));
      return fs > 0 ? fs - 1 - bit : m->size - bit;
   }

   int count = 0;

   const int modbits = bit % 64, maxbits = MIN(64, m->size - (bit & ~63));
   if (modbits > 0) {
      uint64_t word0 = m->ptr[bit / 64];
      word0 &= mask_for_range(modbits, maxbits - 1);

      const int fs = __builtin_ffsll(word0);
      if (fs > 0)
         return fs - 1 - modbits;

      count = maxbits - modbits;
      bit = bit + maxbits - modbits;
   }

   if (bit == m->size)
      return count;

   assert(bit % 64 == 0);

   for (; bit + 64 < m->size && m->ptr[bit / 64] == 0; bit += 64, count += 64);

   const int fs = __builtin_ffsll(m->ptr[bit / 64]);
   if (fs > 0)
      return count + fs - 1;

   return count + m->size - bit;
}

void mask_subtract(bit_mask_t *m, const bit_mask_t *m2)
{
   assert(m->size == m2->size);

   if (m->size > 64) {
      for (int i = 0; i < (m->size + 63) / 64; i++)
         m->ptr[i] &= ~m2->ptr[i];
   }
   else
      m->bits &= ~m2->bits;
}

void mask_union(bit_mask_t *m, const bit_mask_t *m2)
{
   assert(m->size == m2->size);

   if (m->size > 64) {
      for (int i = 0; i < (m->size + 63) / 64; i++)
         m->ptr[i] |= m2->ptr[i];
   }
   else
      m->bits |= m2->bits;
}

void mask_copy(bit_mask_t *m, const bit_mask_t *m2)
{
   assert(m->size == m2->size);

   if (m->size > 64) {
      for (int i = 0; i < (m->size + 63) / 64; i++)
         m->ptr[i] = m2->ptr[i];
   }
   else
      m->bits = m2->bits;
}

bool mask_eq(const bit_mask_t *m1, const bit_mask_t *m2)
{
   assert(m1->size == m2->size);

   if (m1->size > 64) {
      for (int i = 0; i < (m1->size + 63) / 64; i++) {
         if (m1->ptr[i] != m2->ptr[i])
            return false;
      }

      return true;
   }
   else
      return m1->bits == m2->bits;
}
