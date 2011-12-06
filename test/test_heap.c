#include "rt/heap.h"

#include <check.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <time.h>

static heap_t h = NULL;

static void setup(void)
{
   h = heap_new(128);
}

static void teardown(void)
{
   heap_free(h);
   h = NULL;
}

static int greater_than(const void *a, const void *b)
{
   return *(const uint64_t*)a > *(const uint64_t*)b;
}

static void walk_fn(uint64_t key, void *user, void *context)
{
   uint64_t *last = context;

   fail_if(key != (uint64_t)user);
   fail_if(*last > key);

   *last = key;
}

START_TEST(test_basic)
{
   heap_insert(h, 5, (void*)5);
   heap_insert(h, 2, (void*)2);
   heap_insert(h, 62, (void*)62);

   fail_unless(heap_size(h) == 3);

   fail_unless(heap_min(h) == (void*)2);

   fail_unless(heap_extract_min(h) == (void*)2);
   fail_unless(heap_extract_min(h) == (void*)5);
   fail_unless(heap_extract_min(h) == (void*)62);

   fail_unless(heap_size(h) == 0);
}
END_TEST

START_TEST(test_walk)
{
   heap_insert(h, 5, (void*)5);
   heap_insert(h, 2, (void*)2);
   heap_insert(h, 62, (void*)62);

   uint64_t last = 0;
   heap_walk(h, walk_fn, &last);

   fail_unless(last == 62);
}
END_TEST

START_TEST(test_rand)
{
   static const int N = 1024;
   uint64_t keys[N];

   for (int i = 0; i < N; i++) {
      keys[i] = random();
      heap_insert(h, keys[i], (void*)keys[i]);
   }

   qsort(keys, N, sizeof(uint64_t), greater_than);

   for (int i = 0; i < N; i++)
      fail_unless(heap_extract_min(h) == (void*)keys[i]);
}
END_TEST

int main(void)
{
   srandom((unsigned)time(NULL));

   Suite *s = suite_create("heap");

   TCase *tc_core = tcase_create("Core");
   tcase_add_checked_fixture(tc_core, setup, teardown);
   tcase_add_test(tc_core, test_basic);
   tcase_add_test(tc_core, test_rand);
   tcase_add_test(tc_core, test_walk);
   suite_add_tcase(s, tc_core);

   SRunner *sr = srunner_create(s);
   srunner_run_all(sr, CK_NORMAL);

   int nfail = srunner_ntests_failed(sr);

   srunner_free(sr);

   return nfail == 0 ? EXIT_SUCCESS : EXIT_FAILURE;
}

