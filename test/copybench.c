//
//  Copyright (C) 2023  Nick Gasson
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

#include <stdint.h>
#include <stdbool.h>
#include <stdio.h>
#include <stddef.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#define ITERS 10000000

static inline void copy_bytes(void *restrict dst, const void *restrict src,
                              size_t size)
{
   if (__builtin_expect(size <= 32, 1)) {
#if 0
      asm volatile ("rep movsb"
          : "=D" (dst),
            "=S" (src),
            "=c" (size)
          : "0" (dst),
            "1" (src),
            "2" (size)
           : "memory");
#else
      for (; size > 7; size -= 8, dst += 8, src += 8)
         *(uint64_t *)dst = *(const uint64_t *)src;

      for (; size > 0; size--, dst++, src++)
         *(uint8_t *)dst = *(const uint8_t *)src;
#endif
   }
   else
      memcpy(dst, src, size);
}

static inline bool cmp_bytes(const void *a, const void *b, size_t size)
{
   if (__builtin_expect(size <= 128, 1)) {
      for (; size > 7; size -= 8, a += 8, b += 8) {
         if (*(const uint64_t *)a != *(const uint64_t *)b)
            return false;
      }
      for (; size > 0; size--, a++, b++) {
         if (*(const uint8_t *)a != *(const uint8_t *)b)
            return false;
      }
      return true;
   }
   else
      return memcmp(a, b, size) == 0;
}

__attribute__((noinline))
uint64_t get_timestamp_ns(void)
{
   struct timespec ts;
   if (clock_gettime(CLOCK_MONOTONIC, &ts) != 0)
      abort();
   return ts.tv_nsec + (ts.tv_sec * 1000000000ull);
}

__attribute__((noinline))
static void bench_copy(void *dst, const void *src, int size)
{
   {
      const uint64_t start = get_timestamp_ns();

      for (int i = 0; i < ITERS; i++)
         memcpy(dst, src, size);

      const uint64_t end = get_timestamp_ns();

      printf("%-7.1f ", (double)(end - start) / ITERS);
   }

   {
      const uint64_t start = get_timestamp_ns();

      for (int i = 0; i < ITERS; i++)
         copy_bytes(dst, src, size);

      const uint64_t end = get_timestamp_ns();

      printf("%-7.1f ", (double)(end - start) / ITERS);
   }
}

__attribute__((noinline))
static int bench_cmp(uint8_t *a, uint8_t *b, int size)
{
   int sum = 0;

   memset(a, '\0', size);
   memset(b, '\0', size);

   {
      const uint64_t start = get_timestamp_ns();

      for (int i = 0; i < ITERS/2; i++) {
         a[ITERS%size] = 0;
         sum += memcmp(a, b, size) == 0;
      }

      for (int i = 0; i < ITERS/2; i++) {
         a[ITERS%size] = 1;
         sum += memcmp(a, b, size) == 0;
         a[ITERS%size] = 0;
      }

      const uint64_t end = get_timestamp_ns();

      printf("%-7.1f ", (double)(end - start) / ITERS);
   }

   {
      const uint64_t start = get_timestamp_ns();

      for (int i = 0; i < ITERS/2; i++) {
         a[ITERS%size] = 0;
         sum += cmp_bytes(a, b, size) == 0;
      }

      for (int i = 0; i < ITERS/2; i++) {
         a[ITERS%size] = 1;
         sum += cmp_bytes(a, b, size) == 0;
         a[ITERS%size] = 0;
      }

      const uint64_t end = get_timestamp_ns();

      printf("%-7.1f ", (double)(end - start) / ITERS);
   }

   printf("   (%d)", sum);
   return sum;
}

int main(int argc, char **argv)
{
   const int sizes[] = {
      1, 2, 5, 7, 8, 10, 15, 16, 20, 32, 35, 40,
      50, 100, 200, 1000, 2048, 3000, 10000
   };

   printf("SIZE   MEMCPY  COPY_BYTES  MEMCMP  CMP_BYTES\n");

   for (unsigned i = 0; i < sizeof(sizes)/sizeof(int); i++) {
      void *src = malloc(sizes[i]);
      void *dst = malloc(sizes[i]);

      printf("%-6d ", sizes[i]);

      bench_copy(dst, src, sizes[i]);

      printf("    ");

      bench_cmp(dst, src, sizes[i]);

      printf("\n");

      free(src);
      free(dst);
   }

   return 0;
}
