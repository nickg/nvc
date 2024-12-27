#include <stdio.h>
#include <assert.h>
#include <stdlib.h>
#include <time.h>
#include <string.h>

#include "rt/copy.h"

typedef struct {
   size_t off1, size, off2;
} case_t;

uint64_t get_timestamp_ns(void)
{
   struct timespec ts;
   clock_gettime(CLOCK_MONOTONIC, &ts);
   return ts.tv_nsec + (ts.tv_sec * 1000000000ul);
}

int main(int argc, char **argv)
{
   FILE *f = fopen("cases.txt", "r");
   assert(f);

   int ncases = 0, maxmem = 0, maxlen = 0;
   while (!feof(f)) {
      size_t off1, size, off2;
      int n = fscanf(f, "%zu %zu %ld\n", &off1, &size, &off2);
      assert(n == 3);

      ncases++;
      if (off2 + size > maxmem)
         maxmem = off2 + size;
      if (size > maxlen)
         maxlen = size;
   }

   printf("%d cases, %d bytes max offset; %d bytes max length\n",
          ncases, maxmem, maxlen);

   rewind(f);

   case_t *cases = malloc(ncases * sizeof(case_t));
   assert(cases);

   for (int i = 0; i < ncases; i++) {
      int n = fscanf(f, "%zu %zu %ld\n", &cases[i].off1, &cases[i].size,
                     &cases[i].off2);
      assert(n == 3);
   }

   unsigned char *mem = malloc(maxmem);
   for (int i = 0; i < maxmem; i++)
      mem[i] = i;

   unsigned char *src = malloc(maxlen);
   for (int i = 0; i < maxlen; i++)
      src[i] = i;

   const unsigned long start = get_timestamp_ns();

   for (int n = 0; n < 1000; n++) {
      for (int i = 0; i < ncases; i++)
         copy2(mem + cases[i].off1, mem + cases[i].off2, src, cases[i].size);
   }

   const unsigned long end = get_timestamp_ns();

   printf("===> %lu us\n", (end - start) / 1000);

   return 0;
}
