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

#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <pthread.h>
#include <limits.h>
#include <assert.h>

#define PTHREAD   1
#define NVC_MUTEX 2
#define NVC_LOCK  3

#define IMPL NVC_LOCK

#if IMPL == NVC_MUTEX
#define LOCK_TYPE nvc_mutex_t *
#define LOCK_INIT(m) m = mutex_create()
#define LOCK(m) mutex_lock(m);
#define UNLOCK(m) mutex_unlock(m);
#elif IMPL == NVC_LOCK
#define LOCK_TYPE nvc_lock_t
#define LOCK_INIT(m) m = 0
#define LOCK(m) nvc_lock(&m);
#define UNLOCK(m) nvc_unlock(&m);

typedef int nvc_lock_t;

void nvc_lock(nvc_lock_t *lock);
void nvc_unlock(nvc_lock_t *lock);

void nvc_lock(nvc_lock_t *lock)
{
   while (!atomic_cas(lock, 0, 1))
      spin_wait();
}

void nvc_unlock(nvc_lock_t *lock)
{
   assert(relaxed_load(lock) == 1);
   atomic_store(lock, 0);
}

#elif IMPL == PTHREAD
#define LOCK_TYPE pthread_mutex_t
#define LOCK_INIT(m) pthread_mutex_init(&m, NULL)
#define LOCK(m) pthread_mutex_lock(&m)
#define UNLOCK(m) pthread_mutex_unlock(&m)
#else
#error invalid IMPL value
#endif

static LOCK_TYPE mtx __attribute__((aligned(64)));
static int counter __attribute__((aligned(64)));

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

      LOCK(mtx);
      counter++;
      UNLOCK(mtx);

      relaxed_store(&(t->iters), iters + 1);
   }

   return NULL;
}

int main(int argc, char **argv)
{
   const int nproc = nvc_nprocs();

   thread_state_t *threads = xcalloc_array(nproc, sizeof(thread_state_t));

   LOCK_INIT(mtx);

   for (int i = 0; i < nproc; i++) {
      threads[i].running = 1;
      threads[i].thread = thread_create(worker_thread, &(threads[i]),
                                        "worker %d", i);
   }

   for (int i = 0; i < 10; i++) {
      sleep(1);

      LOCK(mtx);
      int old = counter;
      counter = 0;
      UNLOCK(mtx);

      int min = INT_MAX, max = INT_MIN;
      for (int j = 0; j < nproc; j++) {
         int iters = relaxed_load(&(threads[j].iters));
         int delta = iters - threads[j].last;
         min = MIN(min, delta);
         max = MAX(max, delta);
         threads[j].last = iters;
      }

      printf("%d threads; avg:%d min:%d max:%d\n",
             nproc, old / nproc, min, max);
   }

   for (int i = 0; i < nproc; i++)
      relaxed_store(&(threads[i].running), 0);

   for (int i = 0; i < nproc; i++)
      thread_join(threads[i].thread);

   return 0;
}
