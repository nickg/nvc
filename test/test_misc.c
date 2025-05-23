//
//  Copyright (C) 2021-2024  Nick Gasson
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
#include "fbuf.h"
#include "hash.h"
#include "ident.h"
#include "mask.h"
#include "option.h"
#include "rt/copy.h"
#include "rt/heap.h"
#include "thread.h"
#include "util.h"

#include <assert.h>
#include <check.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <time.h>
#include <unistd.h>

#define VOIDP(x) ((void *)(uintptr_t)(x))

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

START_TEST(test_chash_rand)
{
   chash_t *h = chash_new(32);

   static const int N = 1024;

   void *keys[N];
   void *values[N];

   for (int i = 0; i < N; i++) {
      do {
         keys[i] = VOIDP((i << 20) | (rand() & 0xffff0));
      } while (keys[i] == NULL);
      values[i] = VOIDP(rand());
   }

   for (int i = 0; i < N; i++)
      chash_put(h, keys[i], values[i]);

   for (int i = 0; i < N; i++)
      ck_assert_ptr_eq(chash_get(h, keys[i]), values[i]);

   chash_free(h);
}
END_TEST;

START_TEST(test_chash_cas)
{
   chash_t *h = chash_new(32);

   ck_assert_ptr_eq(chash_cas(h, VOIDP(0x1000), NULL, VOIDP(0x123)), NULL);
   ck_assert_ptr_eq(chash_cas(h, VOIDP(0x1000), NULL, VOIDP(0x123)),
                    VOIDP(0x123));
   ck_assert_ptr_eq(chash_cas(h, VOIDP(0x1000), VOIDP(0x123), VOIDP(0x456)),
                    VOIDP(0x123));
   ck_assert_ptr_eq(chash_get(h, VOIDP(0x1000)), VOIDP(0x456));

   chash_free(h);
}
END_TEST;

struct point {
   double x, y;
};

static uint32_t hash_point(const void *ptr)
{
   const struct point *p = ptr;
   return mix_bits_32(FLOAT_BITS(p->x) + FLOAT_BITS(p->y));
}

static bool cmp_point(const void *a, const void *b)
{
   const struct point *pa = a, *pb = b;
   return pa->x == pb->x && pa->y == pb->y;
}

START_TEST(test_ghash_basic)
{
   ghash_t *h = ghash_new(4, hash_point, cmp_point);

   static const struct point data[] = {
      { 1.0, 5.0 },
      { 2.0, 3.5 },
      { 1.9, 3.6 },
      { 2.2, 5.7 },
      { 7.9, 0.1 },
   };

   for (int i = 0; i < ARRAY_LEN(data); i++)
      ghash_put(h, &data[i], VOIDP(i + 1));

   for (int i = 0; i < ARRAY_LEN(data); i++)
      ck_assert_ptr_eq(ghash_get(h, &data[i]), VOIDP(i + 1));

   const struct point dummy = { 5.5, 1.0 };
   ck_assert_ptr_null(ghash_get(h, &dummy));

   const struct point elem = { 1.0, 5.0 };
   ck_assert_ptr_nonnull(ghash_get(h, &elem));
   ghash_delete(h, &elem);
   ck_assert_ptr_null(ghash_get(h, &elem));

   ghash_free(h);
}
END_TEST;

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

static bool heap_delete_cb(uint64_t key, void *value, void *context)
{
   ck_assert_int_eq(key, (uintptr_t)value);
   return value == context;
}

START_TEST(test_heap_delete)
{
   heap_t *h = heap_new(128);

   static const int N = 1024;
   uintptr_t keys[N];

   for (int i = 0; i < N; i++) {
      keys[i] = 1 + rand() % 10000;
      heap_insert(h, keys[i], (void*)keys[i]);
   }

   int deleted = 0;
   for (int i = 0; i < N; i++) {
      if (rand() % 20 == 0) {
         ck_assert(heap_delete(h, heap_delete_cb, (void*)keys[i]));
         keys[i] = 0;
         deleted++;
      }
   }

   ck_assert_int_eq(heap_size(h), N - deleted);

   qsort(keys, N, sizeof(uintptr_t), magnitude_compar);

   for (int i = 0; i < deleted; i++)
      ck_assert_int_eq(keys[i], 0);

   for (int i = deleted; i < N; i++)
      ck_assert_ptr_eq(heap_extract_min(h), (void*)keys[i]);

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

START_TEST(test_strip)
{
   LOCAL_TEXT_BUF tb = tb_new();
   tb_cat(tb, " hello world    \r\n");
   tb_strip(tb);

   ck_assert_str_eq(tb_get(tb), " hello world");
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

   ck_assert(!mask_test_and_set(&m, 5));
   ck_assert(mask_test_and_set(&m, 5));

   mask_free(&m);
}
END_TEST

START_TEST(test_set_clear_range)
{
   bit_mask_t m;
   mask_init(&m, mask_size[_i]);

   fail_if(mask_test_range(&m, 0, mask_size[_i]));

   mask_set_range(&m, 1, 2);

   fail_if(mask_test(&m, 0));
   fail_unless(mask_test(&m, 1));
   fail_unless(mask_test(&m, 2));
   fail_if(mask_test(&m, 3));
   ck_assert_int_eq(mask_popcount(&m), 2);
   fail_unless(mask_test_range(&m, 0, 5));

   if (mask_size[_i] > 64) {
      mask_set_range(&m, 70, 3);

      fail_if(mask_test(&m, 69));
      fail_unless(mask_test(&m, 70));
      fail_unless(mask_test(&m, 71));
      fail_unless(mask_test(&m, 72));
      fail_if(mask_test(&m, 73));
      ck_assert_int_eq(mask_popcount(&m), 5);
      fail_unless(mask_test_range(&m, 60, 20));

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

START_TEST(test_empty_mask)
{
   bit_mask_t m;
   mask_init(&m, 0);

   mask_clearall(&m);

   mask_free(&m);
}
END_TEST

START_TEST(test_mask_iter)
{
   bit_mask_t m;
   mask_init(&m, 32);

   mask_set(&m, 1);
   mask_set(&m, 6);
   mask_set(&m, 17);

   size_t bit = -1;
   fail_unless(mask_iter(&m, &bit));
   fail_unless(bit == 1);
   fail_unless(mask_iter(&m, &bit));
   fail_unless(bit == 6);
   fail_unless(mask_iter(&m, &bit));
   fail_unless(bit == 17);
   fail_if(mask_iter(&m, &bit));

   mask_free(&m);

   mask_init(&m, 600);

   mask_set(&m, 0);
   mask_set(&m, 6);
   mask_set(&m, 93);
   mask_set(&m, 422);
   mask_set(&m, 599);

   bit = -1;
   fail_unless(mask_iter(&m, &bit));
   fail_unless(bit == 0);
   fail_unless(mask_iter(&m, &bit));
   fail_unless(bit == 6);
   fail_unless(mask_iter(&m, &bit));
   fail_unless(bit == 93);
   fail_unless(mask_iter(&m, &bit));
   fail_unless(bit == 422);
   fail_unless(mask_iter(&m, &bit));
   fail_unless(bit == 599);
   fail_if(mask_iter(&m, &bit));

   mask_free(&m);

   mask_init(&m, 85);

   mask_set(&m, 64);
   mask_set(&m, 70);

   bit = -1;
   fail_unless(mask_iter(&m, &bit));
   fail_unless(bit == 64);
   fail_unless(mask_iter(&m, &bit));
   fail_unless(bit == 70);
   fail_if(mask_iter(&m, &bit));

   mask_free(&m);

   mask_init(&m, 124);

   mask_set(&m, 56);
   mask_set(&m, 63);
   mask_set(&m, 69);
   mask_set(&m, 123);

   bit = -1;
   fail_unless(mask_iter(&m, &bit));
   fail_unless(bit == 56);
   fail_unless(mask_iter(&m, &bit));
   fail_unless(bit == 63);
   fail_unless(mask_iter(&m, &bit));
   fail_unless(bit == 69);
   fail_unless(mask_iter(&m, &bit));
   fail_unless(bit == 123);
   fail_if(mask_iter(&m, &bit));

   mask_free(&m);
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

static void async_add_cb(void *context, void *arg)
{
   (*(int *)arg)++;
}

START_TEST(test_async)
{
   int numbers[100];
   for (int i = 0; i < ARRAY_LEN(numbers); i++)
      numbers[i] = i;

   for (int i = 0; i < ARRAY_LEN(numbers); i++)
      async_do(async_add_cb, NULL, &(numbers[i]));

   async_barrier();

   for (int i = 0; i < ARRAY_LEN(numbers); i++)
      ck_assert_int_eq(numbers[i], i + 1);
}
END_TEST

static void stop_world_cb(int thread_id, struct cpu_state *cpu, void *arg)
{
   // Avoid ck_assert* here as it does I/O
   assert(arg == (void *)0xdeadbeef);
}

static void *stop_world_thread_fn(void *__arg)
{
   for (int i = 0; i < 100; i++) {
      stop_world(stop_world_cb, (void *)0xdeadbeef);
      start_world();
   }

   return NULL;
}

START_TEST(test_stop_world)
{
   static const int N = 5;
   nvc_thread_t *threads[N];
   for (int i = 0; i < N; i++)
      threads[i] = thread_create(stop_world_thread_fn, NULL, "t%d", i);

   for (int i = 0; i < N; i++)
      thread_join(threads[i]);
}
END_TEST

static void *barrier_fn(void *__arg)
{
   barrier_t *b = __arg;

   static volatile int ctr = 0;

   atomic_add(&ctr, 1);
   barrier_wait(b);

   const int nthreads = atomic_load(&ctr);
   barrier_wait(b);

   for (int i = 0; i < 10; i++) {
      atomic_add(&ctr, 1);
      barrier_wait(b);
      ck_assert_int_eq(atomic_load(&ctr), nthreads * (i + 2));
      barrier_wait(b);
   }

   return NULL;
}

START_TEST(test_barrier)
{
   static const int N = 4;
   barrier_t *b = barrier_new(N);
   nvc_thread_t *threads[N];
   for (int i = 0; i < N; i++)
      threads[i] = thread_create(barrier_fn, b, "t%d", i);

   for (int i = 0; i < N; i++)
      thread_join(threads[i]);

   barrier_free(b);
}
END_TEST

START_TEST(test_pool_basic)
{
   mem_pool_t *mp = pool_new();

   uint8_t *p1 = pool_malloc(mp, 13);
   ck_assert_ptr_nonnull(p1);
   memset(p1, 42, 13);

   uint8_t *p2 = pool_calloc(mp, 3);
   ck_assert_ptr_nonnull(p2);
   ck_assert_int_eq(p2[0], 0);
   memset(p2, 66, 3);

   ck_assert_int_eq(p1[0], 42);
   ck_assert_int_eq(p1[12], 42);
   ck_assert_int_eq(p2[0], 66);
   ck_assert_int_eq(p2[2], 66);

   pool_free(mp);
}
END_TEST

START_TEST(test_pool_stats)
{
   mem_pool_t *mp = pool_new();

   for (int i = 0; i < 100; i++)
      pool_malloc(mp, i);

   size_t alloc, npages;
   pool_stats(mp, &alloc, &npages);

   // Will vary with host word size and address sanitiser
   ck_assert_int_gt(alloc, 5000);
   ck_assert_int_lt(alloc, 7000);

   ck_assert_int_eq(npages, 2);

   pool_free(mp);
}
END_TEST

START_TEST(test_cmp_bytes)
{
   unsigned char a[50 + 15], b[50 + 15];

   for (int i = 0; i < ARRAY_LEN(a); i++)
      a[i] = b[i] = i;

   for (int size = 0; size < 50; size++) {
      a[size + 1] = 200;
      b[size + 1] = 201;
      ck_assert(cmp_bytes(a, b, size));

      for (int diffpos = 0; diffpos < size; diffpos++) {
         a[diffpos] = 255;
         ck_assert(!cmp_bytes(a, b, size));
         a[diffpos] = diffpos;

         b[diffpos] = 255;
         ck_assert(!cmp_bytes(a, b, size));
         b[diffpos] = diffpos;
      }

      a[size + 1] = b[size + 1] = size + 1;
   }
}
END_TEST

START_TEST(test_copy2)
{
   unsigned char a[50 + 15], b[50 + 15], c[50 + 15];

   for (int i = 0; i < ARRAY_LEN(a); i++)
      c[i] = 100 + i;

   for (int size = 0; size < 50; size++) {
      for (int i = 0; i < ARRAY_LEN(a); i++) {
         a[i] = 255;
         b[i] = i;
      }

      copy2(a, b, c, size);

      for (int i = 0; i < ARRAY_LEN(a); i++) {
         if (i < size) {
            ck_assert_int_eq(a[i], i);
            ck_assert_int_eq(b[i], 100 + i);
         }
         else {
            ck_assert_int_eq(a[i], 255);
            ck_assert_int_eq(b[i], i);
         }
      }
   }
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
   tcase_add_test(tc_hash, test_chash_rand);
   tcase_add_test(tc_hash, test_chash_cas);
   tcase_add_test(tc_hash, test_ghash_basic);
   suite_add_tcase(s, tc_hash);

   TCase *tc_heap = tcase_create("heap");
   tcase_add_test(tc_heap, test_heap_basic);
   tcase_add_test(tc_heap, test_heap_rand);
   tcase_add_test(tc_heap, test_heap_walk);
   tcase_add_test(tc_heap, test_heap_delete);
   suite_add_tcase(s, tc_heap);

   TCase *tc_util = tcase_create("util");
   tcase_add_test(tc_util, test_color_printf);
   tcase_add_test(tc_util, test_strip);
   suite_add_tcase(s, tc_util);

   TCase *tc_mask = tcase_create("mask");
   tcase_add_loop_test(tc_mask, test_mask, 0, ARRAY_LEN(mask_size));
   tcase_add_loop_test(tc_mask, test_set_clear_range, 0, ARRAY_LEN(mask_size));
   tcase_add_loop_test(tc_mask, test_count_clear, 0, ARRAY_LEN(mask_size));
   tcase_add_loop_test(tc_mask, test_scan_backwards, 0, ARRAY_LEN(mask_size));
   tcase_add_loop_test(tc_mask, test_subtract, 0, ARRAY_LEN(mask_size));
   tcase_add_test(tc_mask, test_empty_mask);
   tcase_add_test(tc_mask, test_mask_iter);
   suite_add_tcase(s, tc_mask);

   TCase *tc_thread = tcase_create("thread");
   tcase_add_test(tc_thread, test_threads);
   tcase_add_test(tc_thread, test_async);
#ifndef __SANITIZE_THREAD__
   tcase_add_test(tc_thread, test_stop_world);
#endif
   tcase_add_test(tc_thread, test_barrier);
   suite_add_tcase(s, tc_thread);

   TCase *tc_pool = tcase_create("pool");
   tcase_add_test(tc_pool, test_pool_basic);
   tcase_add_test(tc_pool, test_pool_stats);
   suite_add_tcase(s, tc_pool);

   TCase *tc_copy = tcase_create("copy");
   tcase_add_test(tc_pool, test_cmp_bytes);
   tcase_add_test(tc_pool, test_copy2);
   suite_add_tcase(s, tc_copy);

   return s;
}
