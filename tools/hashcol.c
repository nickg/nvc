//
// Tool to find hash collisions for test/regress/case8.vhd
//

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <inttypes.h>

static const char slv_map[] = "UX01ZWLH-";

static int rand_slv(void)
{
   return random() % 9;
}

static int64_t hash_slv(int array[], int length)
{
   uint64_t result = 0;
   for (int i = 0; i < length; i++) {
      result *= 0x27d4eb2d;
      result += array[i];
   }
   return result;
}

static void slv_to_str(char *buf, int array[], int length)
{
   for (int i = 0; i < length; i++)
      buf[i] = slv_map[array[i]];
   buf[length] = '\0';
}

int main(void)
{
   int array[32];
   char buf[33];

   static const int max_cases = 256;

   for (;;) {
      for (int i = 0; i < 32; i++)
         array[i] = rand_slv();

      const int64_t hash = hash_slv(array, 32);
      if (hash % max_cases == 0) {
         slv_to_str(buf, array, 32);
         printf("%s ==> %"PRIi64" %% %d = 0\n", buf, hash, max_cases);
      }
   }

   return 0;
}
