#include "hash.h"
#include "rt/heap.h"

#include <check.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <time.h>

#define VOIDP(x) ((void *)(uintptr_t)x)

START_TEST(test_hash_basic)
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

START_TEST(test_hash_rand)
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

   hash_free(h);
}
END_TEST;

START_TEST(test_hash_replace)
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

START_TEST(test_safe_symbol)
{
  const char *orig = "foo[]()+*\"=bar";
  char *enc = safe_symbol(orig);
  char *dec = unsafe_symbol(enc);

  ck_assert_str_eq(orig, dec);

  if (enc != orig) free(enc);
  if (dec != orig) free(dec);
}
END_TEST

static int magnitude_compar(const void *a, const void *b)
{
   return *(const uintptr_t*)a - *(const uintptr_t*)b;
}

static void walk_fn(uint64_t key, void *user, void *context)
{
   uint64_t *last = context;

   fail_if(key != (uintptr_t)user);
   fail_if(*last > key);

   *last = key;
}

START_TEST(test_heap_basic)
{
   heap_t *h = heap_new(128);

   heap_insert(h, 5, (void*)5);
   heap_insert(h, 2, (void*)2);
   heap_insert(h, 62, (void*)62);

   fail_unless(heap_size(h) == 3);

   fail_unless(heap_min(h) == (void*)2);

   fail_unless(heap_extract_min(h) == (void*)2);
   fail_unless(heap_extract_min(h) == (void*)5);
   fail_unless(heap_extract_min(h) == (void*)62);

   fail_unless(heap_size(h) == 0);

   heap_free(h);
}
END_TEST

START_TEST(test_heap_walk)
{
   heap_t *h = heap_new(128);

   heap_insert(h, 5, (void*)5);
   heap_insert(h, 2, (void*)2);
   heap_insert(h, 62, (void*)62);

   uint64_t last = 0;
   heap_walk(h, walk_fn, &last);

   fail_unless(last == 62);

   heap_free(h);
}
END_TEST

START_TEST(test_heap_rand)
{
   heap_t *h = heap_new(128);

   static const int N = 1024;
   uintptr_t keys[N];

   for (int i = 0; i < N; i++) {
      keys[i] = rand();
      heap_insert(h, keys[i], (void*)keys[i]);
   }

   qsort(keys, N, sizeof(uintptr_t), magnitude_compar);

   for (int i = 0; i < N; i++)
      fail_unless(heap_extract_min(h) == (void*)keys[i]);

   heap_free(h);
}
END_TEST

START_TEST(test_color_printf)
{
   setenv("NVC_COLORS", "always", 1);
   term_init();

   char *LOCAL str1 = color_asprintf("$red$hello$$");
   ck_assert_str_eq(str1, "\033[31mhello\033[0m");

   char *LOCAL str2 = color_asprintf("$#42$world$$");
   ck_assert_str_eq(str2, "\033[38;5;42mworld\033[0m");

   char *LOCAL str3 = color_asprintf("$!blue$bold$$ normal");
   ck_assert_str_eq(str3, "\033[1;34mbold\033[0m normal");

   char *LOCAL str4 = color_asprintf("a $bad$ color");
   ck_assert_str_eq(str4, "a $bad$ color");

   char *LOCAL str5 = color_asprintf("$!wrong$ end");
   ck_assert_str_eq(str5, "$!wrong$ end");

   char *LOCAL str6 = color_asprintf("$missing");
   ck_assert_str_eq(str6, "$missing");

   char *LOCAL str7 = color_asprintf("$<$$red$override$$$>$");
   ck_assert_str_eq(str7, "override");
}
END_TEST

Suite *get_misc_tests(void)
{
   Suite *s = suite_create("misc");

   TCase *tc_hash = tcase_create("hash");
   tcase_add_test(tc_hash, test_hash_basic);
   tcase_add_test(tc_hash, test_hash_rand);
   tcase_add_test(tc_hash, test_hash_replace);
   suite_add_tcase(s, tc_hash);

   TCase *tc_sym = tcase_create("safe_symbol");
   tcase_add_test(tc_sym, test_safe_symbol);
   suite_add_tcase(s, tc_sym);

   TCase *tc_heap = tcase_create("heap");
   tcase_add_test(tc_heap, test_heap_basic);
   tcase_add_test(tc_heap, test_heap_rand);
   tcase_add_test(tc_heap, test_heap_walk);
   suite_add_tcase(s, tc_heap);

   TCase *tc_util = tcase_create("util");
   tcase_add_test(tc_heap, test_color_printf);
   suite_add_tcase(s, tc_util);

   return s;
}
