//
//  Copyright (C) 2022-2024  Nick Gasson
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
#include "hash.h"
#include "ident.h"
#include "mir/mir-node.h"
#include "mir/mir-unit.h"
#include "option.h"
#include "rt/mspace.h"
#include "thread.h"

#include <assert.h>
#include <check.h>
#include <ctype.h>
#include <stdlib.h>
#include <stdio.h>
#include <time.h>

#define CHECK(expr) do {                        \
      if (unlikely(!(expr)))                    \
         fatal_trace("check " #expr " failed"); \
} while (0)

const char copy_string[] = "";
const char version_string[] = "";

static volatile int start = 0;

static void run_test(thread_fn_t fn, void *arg)
{
   const int nproc = MIN(nvc_nprocs(), MAX_THREADS - 1);
   nvc_thread_t *handles[MAX_THREADS];

   full_barrier();

   for (int i = 0; i < nproc; i++)
      handles[i] = thread_create(fn, arg, "test thread %d", i);

   store_release(&start, 1);

   for (int i = 0; i < nproc; i++)
      thread_join(handles[i]);

   store_release(&start, 0);
}

static uint32_t fast_rand(uint32_t *state)
{
   uint32_t tmp = *state;
   tmp ^= (tmp << 13);
   tmp ^= (tmp >> 17);
   tmp ^= (tmp << 5);
   return (*state = tmp);
}

////////////////////////////////////////////////////////////////////////////////
// Concurrent calls to ident_new

#define NWORDS 1000000
static char *words[NWORDS];
static ident_t idents[NWORDS];

static void *test_ident_thread(void *arg)
{
   const int nproc = nvc_nprocs();

   while (load_acquire(&start) == 0)
      spin_wait();

   for (int i = 0; i < NWORDS / 2 / nproc; i++) {
      const int pos = rand() % NWORDS;
      ident_t id = ident_new(words[pos]), exist;

   again:
      exist = load_acquire(&(idents[pos]));
      if (exist != NULL)
         ck_assert_ptr_eq(exist, id);
      else if (!atomic_cas(&(idents[pos]), NULL, id))
         goto again;
   }

   return NULL;
}

START_TEST(test_ident_new)
{
   FILE *f = fopen("/usr/share/dict/words", "r");
   ck_assert_ptr_nonnull(f);

   char *line LOCAL = NULL;
   size_t bufsz = 0;
   int pos = 0;
   for (; pos < NWORDS; ) {
      int nchars = getline(&line, &bufsz, f);
      if (nchars == -1)
         break;

      ck_assert_int_ge(nchars, 2);

      if (islower(line[0]) && nchars > 2 && line[nchars - 3] != '\'') {
         line[nchars - 1] = '\0';
         words[pos++] = line;
         line = NULL;
         bufsz = 0;
      }
   }

   for (; pos < NWORDS; pos++)
      words[pos] = words[rand() % pos];

   fclose(f);

   run_test(test_ident_thread, NULL);
}
END_TEST

////////////////////////////////////////////////////////////////////////////////
// Concurrent hash table updates

#define CHASH_NVALUES 1024
#define CHASH_ITERS   10000

static void *chash_keys[CHASH_NVALUES];
static void *chash_values[CHASH_NVALUES];

#define VOIDP(x) ((void *)(uintptr_t)x)

static void *test_chash_thread(void *arg)
{
   chash_t *h = arg;

   while (load_acquire(&start) == 0)
      spin_wait();

   for (int i = 0; i < CHASH_ITERS; i++) {
      int nth = rand() % CHASH_NVALUES;

      if (rand() % 3 == 0)
         chash_put(h, chash_keys[nth], chash_values[nth]);
      else {
         void *value = chash_get(h, chash_keys[nth]);
         if (value != NULL)
            ck_assert_ptr_eq(value, chash_values[nth]);
      }
   }

   return NULL;
}

START_TEST(test_chash_rand)
{
   for (int i = 0; i < CHASH_NVALUES; i++) {
      do {
         chash_keys[i] = VOIDP(((i << 16) | (rand() & 0xffff)));
      } while (chash_keys[i] == NULL);
      chash_values[i] = VOIDP(rand());
   }

   chash_t *h = chash_new(CHASH_NVALUES / 4);

   run_test(test_chash_thread, h);

   chash_free(h);
}
END_TEST

////////////////////////////////////////////////////////////////////////////////
// Stop/start world

static void stop_world_cb(int thread_id, struct cpu_state *cpu, void *arg)
{
}

static void *stop_world_thread_fn(void *__arg)
{
   while (load_acquire(&start) == 0)
      spin_wait();

   for (int i = 0; i < 10000; i++) {
      stop_world(stop_world_cb, NULL);
      start_world();
   }

   return NULL;
}

START_TEST(test_stop_world)
{
   run_test(stop_world_thread_fn, NULL);
}
END_TEST

////////////////////////////////////////////////////////////////////////////////
// Concurrent GC

__attribute__((noinline))
static void gc_alloc_loop(mspace_t *m)
{
   int32_t *saved[5];
   int nsaved = 0;

   for (int i = 0; i < 100000; i++) {
      int32_t *mem = mspace_alloc(m, 4 + (rand() % 1000));
      CHECK(mem != NULL);

      if (nsaved < ARRAY_LEN(saved) && (rand() % 2 == 0)) {
         saved[nsaved++] = mem;
         *mem = nsaved | (thread_id() << 16);
      }
   }

   for (int i = 0; i < ARRAY_LEN(saved); i++)
      CHECK(*saved[i] == ((i + 1) | (thread_id() << 16)));
}

static void *test_gc_thread(void *arg)
{
   mspace_t *m = arg;
   mspace_stack_limit(MSPACE_CURRENT_FRAME);
   gc_alloc_loop(m);

   return NULL;
}

START_TEST(test_gc)
{
   mspace_t *m = mspace_new(0x100000);

   run_test(test_gc_thread, m);

   mspace_destroy(m);
}
END_TEST

////////////////////////////////////////////////////////////////////////////////
// MIR concurrent access

static void *test_mir_types_thread(void *arg)
{
   mir_context_t *mc = arg;
   const uint32_t rng_init = knuth_hash(thread_id());

   while (load_acquire(&start) == 0)
      spin_wait();

   ident_t name = ident_sprintf("func%d", thread_id());
   mir_unit_t *mu = mir_unit_new(mc, name, MIR_UNIT_FUNCTION, NULL);

   uint32_t rng = rng_init;

   mir_type_t types[4000];

   for (int i = 0; i < ARRAY_LEN(types); i++) {
      types[i] = mir_int_type(mu, 0, fast_rand(&rng) % (i + 1));
      CHECK(!mir_is_null(types[i]));
   }

   rng = rng_init;

   for (int i = 0; i < ARRAY_LEN(types); i++) {
      const uint32_t max = fast_rand(&rng) % (i + 1);
      const mir_repr_t repr = mir_get_repr(mu, types[i]);

      CHECK(mir_get_class(mu, types[i]) == MIR_TYPE_INT);

      if (max <= 1)
         CHECK(repr == MIR_REPR_U1);
      else if (max <= UINT8_MAX)
         CHECK(repr == MIR_REPR_U8);
      else if (max <= UINT16_MAX)
         CHECK(repr == MIR_REPR_U16);
   }

   mir_unit_free(mu);

   return NULL;
}

START_TEST(test_mir_types)
{
   mir_context_t *mc = mir_context_new();

   run_test(test_mir_types_thread, mc);

   mir_context_free(mc);
}
END_TEST

////////////////////////////////////////////////////////////////////////////////

int main(int argc, char **argv)
{
   srand((unsigned)time(NULL));

   term_init();
   thread_init();
   register_signal_handlers();
   set_default_options();
   mspace_stack_limit(MSPACE_CURRENT_FRAME);

   setenv("NVC_LIBPATH", "./lib", 1);

   Suite *s = suite_create("mtstress");

   TCase *tc_ident = tcase_create("ident");
   tcase_add_test(tc_ident, test_ident_new);
   suite_add_tcase(s, tc_ident);

   TCase *tc_chash = tcase_create("chash");
   tcase_add_test(tc_chash, test_chash_rand);
   tcase_set_timeout(tc_chash, 10.0);
   suite_add_tcase(s, tc_chash);

#ifndef __SANITIZE_THREAD__
   TCase *tc_gc = tcase_create("gc");
   tcase_add_test(tc_gc, test_gc);
   tcase_set_timeout(tc_gc, 20.0);
   suite_add_tcase(s, tc_gc);

   TCase *tc_stop_world = tcase_create("stop_world");
   tcase_add_test(tc_stop_world, test_stop_world);
   tcase_set_timeout(tc_stop_world, 20.0);
   suite_add_tcase(s, tc_stop_world);
#endif

   TCase *tc_mir = tcase_create("mir");
   tcase_add_test(tc_mir, test_mir_types);
   tcase_set_timeout(tc_mir, 20.0);
   suite_add_tcase(s, tc_mir);

   SRunner *sr = srunner_create(s);
   srunner_run_all(sr, CK_NORMAL);

   return srunner_ntests_failed(sr) == 0 ? EXIT_SUCCESS : EXIT_FAILURE;
}
