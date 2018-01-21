#include "hash.h"

#include <check.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <time.h>

#define VOIDP(x) ((void *)(uintptr_t)x)

START_TEST(test_basic)
{
   hash_t *h = hash_new(8, true);

   hash_put(h, VOIDP(1516), VOIDP(6));
   hash_put(h, VOIDP(151670), VOIDP(4));
   hash_put(h, VOIDP(61), VOIDP(1));

   fail_unless(hash_get(h, VOIDP(1516)) == VOIDP(6));
   fail_unless(hash_get(h, VOIDP(151670)) == VOIDP(4));
   fail_unless(hash_get(h, VOIDP(61)) == VOIDP(1));

   hash_free(h);
}
END_TEST;

START_TEST(test_rand)
{
  hash_t *h = hash_new(32, true);

  static const int N = 1024;

  void *keys[N];
  void *values[N];

  for (int i = 0; i < N; i++) {
     do {
        keys[i] = VOIDP(((i << 16) | (rand() & 0xffff)));
     } while (keys[i] == NULL);
     values[i] = VOIDP(rand());
  }

  for (int i = 0; i < N; i++)
     hash_put(h, keys[i], values[i]);

  for (int i = 0; i < N; i++)
     fail_unless(hash_get(h, keys[i]) == values[i]);
}
END_TEST;

START_TEST(test_replace)
{
   hash_t *h = hash_new(8, false);

   hash_put(h, VOIDP(10), VOIDP(6));
   hash_put(h, VOIDP(10), VOIDP(4));
   hash_put(h, VOIDP(10), VOIDP(1));

   int n;
   fail_unless(hash_get(h, VOIDP(10)) == VOIDP(6));
   n = 1;
   fail_unless(hash_get_nth(h, VOIDP(10), &n) == VOIDP(4));
   fail_unless(n == 0);
   n = 2;
   fail_unless(hash_get_nth(h, VOIDP(10), &n) == VOIDP(1));
   fail_unless(n == 0);

   hash_free(h);
}
END_TEST;

Suite *get_hash_tests(void)
{
   Suite *s = suite_create("hash");

   TCase *tc_core = tcase_create("Core");
   tcase_add_test(tc_core, test_basic);
   tcase_add_test(tc_core, test_rand);
   tcase_add_test(tc_core, test_replace);
   suite_add_tcase(s, tc_core);

   return s;
}
