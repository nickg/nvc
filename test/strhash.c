#include "../src/util.h"

#include <stdio.h>
#include <math.h>
#include <assert.h>
#include <string.h>

#include <x86intrin.h>

#define WORDS 100000

static uint32_t djb2_mix(const char *key)
{
   uint32_t hash = 5381;
   int c;

   while ((c = *key++))
      hash = ((hash << 5) + hash) + c;

   return mix_bits_32(hash);
}

static uint32_t djb2_orig(const char *key)
{
   uint32_t hash = 5381;
   int c;

   while ((c = *key++))
      hash = ((hash << 5) + hash) + c;

   return hash;
}

__attribute__((target("sse4.1")))
static uint32_t simd_mix_32(const char *key)
{
   __m128i zero = _mm_setzero_si128();
   __m128i prime1 = _mm_set1_epi32(0xcc9e2d51);
   __m128i prime2 = _mm_set1_epi32(0x1b873593);
   __m128i shift1 = _mm_set1_epi64x(15);
   __m128i shift2 = _mm_set1_epi64x(17);

   uint32_t hash = 0;
   for (int pos = 0;; pos += 16) {
      __m128i n = _mm_loadu_si128((const __m128i *)(key + pos));
      __m128i cmp = _mm_cmpeq_epi8(n, zero);
      n = _mm_mullo_epi32(n, prime1);
      n = _mm_or_si128(_mm_sll_epi32(n, shift1), _mm_srl_epi32(n, shift2));
      n = _mm_mullo_epi32(n, prime2);

      __m128i mix1 = _mm_xor_si128(n, _mm_srli_si128(n, 8));
      __m128i mix2 = _mm_xor_si128(mix1, _mm_srli_si128(mix1, 4));
      hash ^= _mm_cvtsi128_si32(mix2);

      if (_mm_movemask_epi8(cmp))
         break;
   }

   return hash;
}

uint64_t get_timestamp_ns(void)
{
   struct timespec ts;
   clock_gettime(CLOCK_MONOTONIC, &ts);
   return ts.tv_nsec + (ts.tv_sec * UINT64_C(1000000000));
}

static void run_test(const char *name, uint32_t (*fn)(const char *), char *mem)
{
   int tab[1024] = {};

   uint64_t start = get_timestamp_ns();

   for (int n = 0; n < 100; n++) {
      for (int i = 0; i < WORDS; i++)
         tab[(*fn)(mem + i*32) & (ARRAY_LEN(tab) - 1)]++;
   }

   uint64_t end = get_timestamp_ns();

   int sum = 0;
   for (int i = 0; i < ARRAY_LEN(tab); i++)
      sum += tab[i];

   double avg = (double)sum / ARRAY_LEN(tab), sumsq = 0.0;

   for (int i = 0; i < ARRAY_LEN(tab); i++)
      sumsq += pow(tab[i] - avg, 2);

   double std = sqrt(sumsq / ARRAY_LEN(tab));

   printf("%-10s : %.1f ± %.2f%% (%.3f ms)\n", name, avg, 100.0 * (std / avg),
          (double)(end - start) / 1.0e6);
}

int main(int argc, char **argv)
{
   FILE *f = fopen("/usr/share/dict/words", "r");
   assert(f);

   char *mem = calloc(32, WORDS);

   char buf[256];
   for (int i = 0; i < WORDS && fgets(buf, sizeof(buf), f); i++)
      strncpy(mem + i*32, buf, 31);

   fclose(f);

   run_test("djb2_mix", djb2_mix, mem);
   run_test("djb2_orig", djb2_orig, mem);
   run_test("simd_mix_32", simd_mix_32, mem);

   return 0;
}
