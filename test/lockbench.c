//
//  Copyright (C) 2022  Nick Gasson
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
#include "thread.h"

#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <pthread.h>
#include <limits.h>
#include <assert.h>

#define USE_NVC_LOCK

#ifdef USE_NVC_LOCK
#define LOCK_TYPE nvc_lock_t
#define LOCK_INIT(m) m = 0
#define LOCK(m) nvc_lock(&m);
#define UNLOCK(m) nvc_unlock(&m);
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

typedef struct __attribute__((aligned(64))) {
   nvc_thread_t *thread;
   int iters;
   int last;
   int running;
} thread_state_t;

STATIC_ASSERT(sizeof(thread_state_t) == 64);

static void *worker_thread(void *arg)
{
   thread_state_t *t = arg;

   while (relaxed_load(&(t->running))) {
      int iters = relaxed_load(&(t->iters));

      int n = random() % N_COUNTERS;

      LOCK(counter[n].lock);
      counter[n].value++;
      UNLOCK(counter[n].lock);

      relaxed_store(&(t->iters), iters + 1);
   }

   return NULL;
}

int main(int argc, char **argv)
{
   const int nproc = nvc_nprocs();

   srandom((unsigned)time(NULL));

   term_init();
   thread_init();
   register_signal_handlers();

   thread_state_t *threads = xcalloc_array(nproc, sizeof(thread_state_t));

   for (int i = 0; i < N_COUNTERS; i++)
      LOCK_INIT(counter[i].lock);

   for (int i = 0; i < nproc; i++) {
      threads[i].running = 1;
      threads[i].thread = thread_create(worker_thread, &(threads[i]),
                                        "worker %d", i);
   }

   for (int i = 0; i < 10; i++) {
      sleep(1);

      int total = 0;
      for (int j = 0; j < N_COUNTERS; j++) {
         LOCK(counter[j].lock);
         total += counter[j].value;
         counter[j].value = 0;
         UNLOCK(counter[j].lock);
      }

      int min = INT_MAX, max = INT_MIN;
      for (int j = 0; j < nproc; j++) {
         int iters = relaxed_load(&(threads[j].iters));
         int delta = iters - threads[j].last;
         min = MIN(min, delta);
         max = MAX(max, delta);
         threads[j].last = iters;
      }

      printf("%d threads; avg:%d min:%d max:%d\n",
             nproc, total / nproc, min, max);
   }

   for (int i = 0; i < nproc; i++)
      relaxed_store(&(threads[i].running), 0);

   for (int i = 0; i < nproc; i++)
      thread_join(threads[i].thread);

   return 0;
}
