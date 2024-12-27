//
//  Copyright (C) 2024  Nick Gasson
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
#include "copy.h"

#include <string.h>

#ifdef ARCH_X86_64
#include <x86intrin.h>
#endif

#if defined __GNUC__ && !defined __clang__
#pragma GCC optimize ("O2")
#endif

void _copy2(void *p1, void *p2, const void *src, size_t len)
{
#ifdef ARCH_X86_64
   for (; len > 15; len -= 16, p1 += 16, p2 += 16, src += 16) {
      __m128i v2   = _mm_loadu_si128((const __m128i *)p2);
      __m128i vsrc = _mm_loadu_si128((const __m128i *)src);
      _mm_storeu_si128((__m128i *)p1, v2);
      _mm_storeu_si128((__m128i *)p2, vsrc);
   }
#endif

   for (; len > 7; len -= 8, p1 += 8, p2 += 8, src += 8) {
      memcpy(p1, p2, 8);
      memcpy(p2, src, 8);
   }

   for (; len > 3; len -= 4, p1 += 4, p2 += 4, src += 4) {
      memcpy(p1, p2, 4);
      memcpy(p2, src, 4);
   }

   for (; len > 0; len--, p1++, p2++, src++) {
      *(unsigned char *)p1 = *(unsigned char *)p2;
      *(unsigned char *)p2 = *(unsigned char *)src;
   }
}

#if defined HAVE_SSE41
__attribute__((aligned(16)))
static const uint8_t lane_iota[16] = {
   0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15
};
#endif

#ifdef HAVE_SSE41
__attribute__((target("sse4.1"), always_inline))
static inline bool cmp_bytes_sse41(const void *a, const void *b, size_t size)
{
   __m128i allmask = _mm_set1_epi8(0xff);

   int pos = 0;
   for (; pos + 15 < size; pos += 16) {
      __m128i left1  = _mm_loadu_si128((const __m128i *)(a + pos));
      __m128i right1 = _mm_loadu_si128((const __m128i *)(b + pos));
      __m128i xor    = _mm_xor_si128(left1, right1);
      if (!_mm_test_all_zeros(xor, allmask))
         return false;
   }

   if (pos < size) {
      __m128i iota   = _mm_load_si128((const __m128i *)lane_iota);
      __m128i mask   = _mm_cmplt_epi8(iota, _mm_set1_epi8(size - pos));
      __m128i left1  = _mm_loadu_si128((const __m128i *)(a + pos));
      __m128i right1 = _mm_loadu_si128((const __m128i *)(b + pos));
      __m128i xor    = _mm_xor_si128(left1, right1);
      if (!_mm_test_all_zeros(xor, mask))
         return false;
   }

   return true;
}
#endif

#ifdef HAVE_SSE41
__attribute__((target("sse4.1")))
#endif
bool _cmp_bytes(const void *a, const void *b, size_t size)
{
#if defined HAVE_SSE41 && !ASAN_ENABLED
   if (likely(__builtin_cpu_supports("sse4.1")))
      return cmp_bytes_sse41(a, b, size);
#endif

   return memcmp(a, b, size) == 0;
}
