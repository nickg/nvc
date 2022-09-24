//
//  Copyright (C) 2021-2022  Nick Gasson
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

#include "test_util.h"
#include "hash.h"
#include "mask.h"
#include "ident.h"
#include "rt/heap.h"
#include "thread.h"

#include <assert.h>
#include <check.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <time.h>

#define VOIDP(x) ((void *)(uintptr_t)x)

START_TEST(test_hash_basic)
{
   hash_t *h = hash_new(8);

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
   hash_t *h = hash_new(32);

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

START_TEST(test_hash_delete)
{
   hash_t *h = hash_new(8);

   hash_put(h, VOIDP(10), VOIDP(6));
   hash_put(h, VOIDP(11), VOIDP(4));
   hash_put(h, VOIDP(2), VOIDP(1));

   fail_unless(hash_get(h, VOIDP(10)) == VOIDP(6));
   hash_delete(h, VOIDP(10));
   fail_unless(hash_get(h, VOIDP(10)) == NULL);
   fail_unless(hash_get(h, VOIDP(11)) == VOIDP(4));

   hash_free(h);
}
END_TEST

START_TEST(test_shash_basic)
{
   shash_t *h = shash_new(8);

   shash_put(h, "hello", VOIDP(6));
   shash_put(h, "world", VOIDP(4));
   shash_put(h, "hell", VOIDP(1));

   fail_unless(shash_get(h, "hello") == VOIDP(6));
   fail_unless(shash_get(h, "world") == VOIDP(4));
   fail_unless(shash_get(h, "hell") == VOIDP(1));

   char *tmp LOCAL = xstrdup("hello");
   fail_unless(shash_get(h, tmp) == VOIDP(6));

   shash_free(h);
}
END_TEST;

START_TEST(test_shash_rand)
{
   shash_t *h = shash_new(8);

   static const int N = 1000;
   char *strs[N];

   for (int i = 0; i < N; i++) {
      size_t len = 5 + rand() % 40;
      strs[i] = xmalloc(len + 1);

      for (size_t j = checked_sprintf(strs[i], len, "%d", i); j < len; j++)
         strs[i][j] = ' ' + rand() % 80;
      strs[i][len] = '\0';

      shash_put(h, strs[i], VOIDP(i));
   }

   for (int i = 0; i < N; i++) {
      fail_unless(shash_get(h, strs[i]) == VOIDP(i));
      free(strs[i]);
   }

   shash_free(h);
}
END_TEST;

START_TEST(test_ihash_rand)
{
   ihash_t *h = ihash_new(32);

   static const int N = 1024;

   uint64_t keys[N];
   void *values[N];

   for (int i = 0; i < N; i++) {
      keys[i] = (i << 16) | (rand() & 0xffff);
      values[i] = VOIDP(rand());
   }

   for (int i = 0; i < N; i++)
      ihash_put(h, keys[i], values[i]);

   for (int i = 0; i < N; i++)
      ck_assert_ptr_eq(ihash_get(h, keys[i]), values[i]);

   ihash_free(h);
}
END_TEST;

START_TEST(test_hset_rand)
{
   hset_t *h = hset_new(32);

   static const int N = 1024;

   void *keys[N];

   for (int i = 0; i < N; i++)
      keys[i] = VOIDP((((i + 1) << 16) | (rand() & 0xffff)));

   for (int i = 0; i < N; i++)
      hset_insert(h, keys[i]);

   for (int i = 0; i < N; i++)
      ck_assert(hset_contains(h, keys[i]));

   hset_free(h);
}
END_TEST;

START_TEST(test_safe_symbol)
{
  const char *orig = "foo[]()+*\"=bar";
  LOCAL_TEXT_BUF enc = safe_symbol(ident_new(orig));
  LOCAL_TEXT_BUF dec = unsafe_symbol(tb_get(enc));

  ck_assert_str_eq(orig, tb_get(dec));
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

static const int mask_size[] = { 15, 64, 101, 160, 256 };

START_TEST(test_mask)
{
   bit_mask_t m;
   mask_init(&m, mask_size[_i]);

   fail_if(mask_test(&m, 0));
   fail_if(mask_test(&m, 5));

   ck_assert_int_eq(mask_popcount(&m), 0);

   mask_set(&m, 4);
   fail_unless(mask_test(&m, 4));
   fail_if(mask_test(&m, 5));

   ck_assert_int_eq(mask_popcount(&m), 1);

   mask_clear(&m, 4);
   fail_if(mask_test(&m, 4));

   mask_setall(&m);
   ck_assert_int_eq(mask_popcount(&m), mask_size[_i]);

   mask_clearall(&m);
   ck_assert_int_eq(mask_popcount(&m), 0);

   mask_free(&m);
}
END_TEST

START_TEST(test_set_clear_range)
{
   bit_mask_t m;
   mask_init(&m, mask_size[_i]);

   mask_set_range(&m, 1, 2);

   fail_if(mask_test(&m, 0));
   fail_unless(mask_test(&m, 1));
   fail_unless(mask_test(&m, 2));
   fail_if(mask_test(&m, 3));
   ck_assert_int_eq(mask_popcount(&m), 2);

   if (mask_size[_i] > 64) {
      mask_set_range(&m, 70, 3);

      fail_if(mask_test(&m, 69));
      fail_unless(mask_test(&m, 70));
      fail_unless(mask_test(&m, 71));
      fail_unless(mask_test(&m, 72));
      fail_if(mask_test(&m, 73));
      ck_assert_int_eq(mask_popcount(&m), 5);

      mask_clear_range(&m, 71, 2);

      fail_if(mask_test(&m, 71));
      ck_assert_int_eq(mask_popcount(&m), 3);
   }

   mask_free(&m);
}
END_TEST

START_TEST(test_count_clear)
{
   bit_mask_t m;
   mask_init(&m, mask_size[_i]);

   mask_set_range(&m, 3, 2);

   ck_assert_int_eq(mask_count_clear(&m, 0), 3);
   ck_assert_int_eq(mask_count_clear(&m, 1), 2);
   ck_assert_int_eq(mask_count_clear(&m, 2), 1);
   ck_assert_int_eq(mask_count_clear(&m, 3), 0);
   ck_assert_int_eq(mask_count_clear(&m, 4), 0);
   ck_assert_int_eq(mask_count_clear(&m, 5), mask_size[_i] - 5);

   if (mask_size[_i] > 64) {
      mask_clearall(&m);
      mask_set_range(&m, 70, 3);

      ck_assert_int_eq(mask_count_clear(&m, 0), 70);
      ck_assert_int_eq(mask_count_clear(&m, 64), 6);
      ck_assert_int_eq(mask_count_clear(&m, mask_size[_i] - 1), 1);
   }

   mask_free(&m);
}
END_TEST

START_TEST(test_scan_backwards)
{
   bit_mask_t m;
   mask_init(&m, mask_size[_i]);

   mask_set_range(&m, 3, 2);

   ck_assert_int_eq(mask_scan_backwards(&m, 0), -1);
   ck_assert_int_eq(mask_scan_backwards(&m, 5), 4);
   ck_assert_int_eq(mask_scan_backwards(&m, 4), 4);
   ck_assert_int_eq(mask_scan_backwards(&m, 3), 3);
   ck_assert_int_eq(mask_scan_backwards(&m, 2), -1);
   ck_assert_int_eq(mask_scan_backwards(&m, mask_size[_i] - 1), 4);

   if (mask_size[_i] > 64) {
      mask_set_range(&m, 70, 3);

      ck_assert_int_eq(mask_scan_backwards(&m, 0), -1);
      ck_assert_int_eq(mask_scan_backwards(&m, 75), 72);
      ck_assert_int_eq(mask_scan_backwards(&m, 72), 72);
      ck_assert_int_eq(mask_scan_backwards(&m, 71), 71);
      ck_assert_int_eq(mask_scan_backwards(&m, 69), 4);
      ck_assert_int_eq(mask_scan_backwards(&m, mask_size[_i] - 1), 72);
   }

   mask_free(&m);
}
END_TEST

START_TEST(test_subtract)
{
   bit_mask_t m1, m2;
   mask_init(&m1, mask_size[_i]);
   mask_init(&m2, mask_size[_i]);

   mask_setall(&m1);
   mask_setall(&m2);
   mask_subtract(&m1, &m2);
   ck_assert_int_eq(mask_popcount(&m1), 0);
   ck_assert_int_eq(mask_popcount(&m2), mask_size[_i]);

   mask_setall(&m1);
   mask_clearall(&m2);
   mask_set(&m2, 5);
   mask_set(&m2, 1);
   mask_subtract(&m1, &m2);
   ck_assert_int_eq(mask_popcount(&m1), mask_size[_i] - 2);
   fail_if(mask_test(&m1, 5));
   fail_if(mask_test(&m1, 1));
   fail_unless(mask_test(&m1, 4));

   mask_free(&m1);
   mask_free(&m2);
}
END_TEST

static volatile int counter = 0;
static nvc_lock_t   lock = 0;

static void *thread_fn(void *__arg)
{
   for (int i = 0; i < 10000; i++) {
      nvc_lock(&lock);
      counter++;
      nvc_unlock(&lock);
   }

   return NULL;
}

START_TEST(test_threads)
{
   static const int N = 5;
   nvc_thread_t *threads[N];
   for (int i = 0; i < N; i++)
      threads[i] = thread_create(thread_fn, NULL, "t%d", i);

   for (int i = 0; i < N; i++)
      thread_join(threads[i]);

   ck_assert_int_eq(counter, N * 10000);

}
END_TEST

Suite *get_misc_tests(void)
{
   Suite *s = suite_create("misc");

   TCase *tc_hash = tcase_create("hash");
   tcase_add_test(tc_hash, test_hash_basic);
   tcase_add_test(tc_hash, test_hash_rand);
   tcase_add_test(tc_hash, test_hash_delete);
   tcase_add_test(tc_hash, test_shash_basic);
   tcase_add_test(tc_hash, test_shash_rand);
   tcase_add_test(tc_hash, test_ihash_rand);
   tcase_add_test(tc_hash, test_hset_rand);
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
   tcase_add_test(tc_util, test_color_printf);
   suite_add_tcase(s, tc_util);

   TCase *tc_mask = tcase_create("mask");
   tcase_add_loop_test(tc_mask, test_mask, 0, ARRAY_LEN(mask_size));
   tcase_add_loop_test(tc_mask, test_set_clear_range, 0, ARRAY_LEN(mask_size));
   tcase_add_loop_test(tc_mask, test_count_clear, 0, ARRAY_LEN(mask_size));
   tcase_add_loop_test(tc_mask, test_scan_backwards, 0, ARRAY_LEN(mask_size));
   tcase_add_loop_test(tc_mask, test_subtract, 0, ARRAY_LEN(mask_size));
   suite_add_tcase(s, tc_mask);

   TCase *tc_thread = tcase_create("thread");
   tcase_add_test(tc_heap, test_threads);
   suite_add_tcase(s, tc_thread);

   return s;
}
