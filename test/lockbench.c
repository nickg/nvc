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
#include "option.h"
#include "thread.h"

#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <limits.h>
#include <assert.h>
#include <time.h>
#include <math.h>

#ifdef __MINGW32__
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#else
#include <pthread.h>
#endif

#define USE_NVC_LOCK 1

#if USE_NVC_LOCK
#define LOCK_TYPE nvc_lock_t
#define LOCK_INIT(m) m = 0
#define LOCK(m) nvc_lock(&m);
#define UNLOCK(m) nvc_unlock(&m);
#elif defined __MINGW32__
#define LOCK_TYPE CRITICAL_SECTION
#define LOCK_INIT(m) InitializeCriticalSectionAndSpinCount(&m, 10);
#define LOCK(m) EnterCriticalSection(&m)
#define UNLOCK(m) LeaveCriticalSection(&m)
#else
#define LOCK_TYPE pthread_mutex_t
#define LOCK_INIT(m) pthread_mutex_init(&m, NULL)
#define LOCK(m) pthread_mutex_lock(&m)
#define UNLOCK(m) pthread_mutex_unlock(&m)
#endif

typedef struct {
   int       value;
   LOCK_TYPE lock;
} __attribute__((aligned(64))) counter_t;

#define N_COUNTERS 8

static counter_t counter[N_COUNTERS];

STATIC_ASSERT(sizeof(counter) == N_COUNTERS * 64);

typedef struct {
   nvc_thread_t *thread;
   uint32_t      rng;
   uint64_t      iters;
   uint64_t      last;
   int           running;
} __attribute__((aligned(64))) thread_state_t;

STATIC_ASSERT(sizeof(thread_state_t) == 64);

static inline uint32_t fast_rand(thread_state_t *t)
{
   uint32_t state = t->rng;
   state ^= (state << 13);
   state ^= (state >> 17);
   state ^= (state << 5);
   return (t->rng = state);
}

static void *worker_thread(void *arg)
{
   thread_state_t *t = arg;

   while (relaxed_load(&(t->running))) {
      int n = fast_rand(t) % N_COUNTERS;

      LOCK(counter[n].lock);
      counter[n].value++;
      UNLOCK(counter[n].lock);

      relaxed_add(&(t->iters), 1);
   }

   return NULL;
}

int main(int argc, char **argv)
{
   const int nproc = nvc_nprocs();

   srand((unsigned)time(NULL));

   term_init();
   thread_init();
   register_signal_handlers();
   set_default_options();

   thread_state_t *threads = xcalloc_array(nproc, sizeof(thread_state_t));

   for (int i = 0; i < N_COUNTERS; i++)
      LOCK_INIT(counter[i].lock);

   for (int i = 0; i < nproc; i++) {
      threads[i].running = 1;
      threads[i].rng = rand();
      threads[i].thread = thread_create(worker_thread, &(threads[i]),
                                        "worker %d", i);
   }

   printf("THREADS   OPS/MS      FAIRNESS\n");

   for (int i = 0; i < 10; i++) {
      const uint64_t start = get_timestamp_ns();

      sleep(1);

      const uint64_t end = get_timestamp_ns();
      const double secs = (end - start) / 1.0e9;

      int total = 0;
      for (int j = 0; j < N_COUNTERS; j++) {
         LOCK(counter[j].lock);
         total += counter[j].value;
         counter[j].value = 0;
         UNLOCK(counter[j].lock);
      }

      double geo = 1.0, best = 0.0;
      for (int j = 0; j < nproc; j++) {
         uint64_t iters = relaxed_load(&(threads[j].iters));
         uint64_t delta = iters - threads[j].last;
         const double result = (double)delta / secs;
         geo *= result;
         if (result > best)
            best = result;
         threads[j].last = iters;
      }

      const double fair = pow(geo, 1.0 / nproc) / best;
      printf("%-8d  %-11.1f %.2f\n", nproc, (total / secs) / 1000.0, fair);
   }

   for (int i = 0; i < nproc; i++)
      relaxed_store(&(threads[i].running), 0);

   for (int i = 0; i < nproc; i++)
      thread_join(threads[i].thread);

#if USE_NVC_LOCK
   for (int i = 0; i < N_COUNTERS; i++)
      assert(counter[i].lock == 0);
#endif

   return 0;
}
