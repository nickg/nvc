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

#include "util.h"
#include "thread.h"

#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include <inttypes.h>

#ifdef HAVE_PTHREAD
#include <pthread.h>
#else
#error missing pthread support
#endif

#ifdef __SANITIZE_THREAD__
#include <sanitizer/tsan_interface.h>
#endif

#define MAX_THREADS  64
#define MAX_SPINS    15
#define PARKING_BAYS 16

struct _nvc_thread {
   unsigned     id;
   char        *name;
   pthread_t    handle;
   thread_fn_t  fn;
   void        *arg;
};

typedef struct {
   int64_t locks;
   int64_t spins;
   int64_t contended;
   int64_t parks;
   int64_t spurious;
   int64_t retries;
} __attribute__((aligned(64))) lock_stats_t;

STATIC_ASSERT(sizeof(lock_stats_t) == 64)

#ifdef DEBUG
#define LOCK_EVENT(what, n) do {                \
      lock_stats[my_thread->id].what += (n);    \
   } while (0)
#else
#define LOCK_EVENT(what, n)
#endif

#ifdef __SANITIZE_THREAD__
#define TSAN_PRE_LOCK(addr) \
   __tsan_mutex_pre_lock(addr, __tsan_mutex_linker_init);
#define TSAN_POST_LOCK(addr) \
   __tsan_mutex_post_lock(addr, __tsan_mutex_linker_init, 0);
#define TSAN_PRE_UNLOCK(addr) \
   __tsan_mutex_pre_unlock(addr, __tsan_mutex_linker_init);
#define TSAN_POST_UNLOCK(addr) \
   __tsan_mutex_post_unlock(addr, __tsan_mutex_linker_init);
#else
#define TSAN_PRE_LOCK(addr)
#define TSAN_POST_LOCK(addr)
#define TSAN_PRE_UNLOCK(addr)
#define TSAN_POST_UNLOCK(addr)
#endif

#define PTHREAD_CHECK(op, ...) do {             \
      if (unlikely(op(__VA_ARGS__) != 0))       \
         fatal_errno(#op);                      \
   } while (0)

// Lock implementation based on WTF::Lock in WebKit
//   https://webkit.org/blog/6161/locking-in-webkit/

typedef enum {
   IS_LOCKED  = (1 << 0),
   HAS_PARKED = (1 << 1)
} lock_bits_t;

typedef struct {
   pthread_mutex_t mutex;
   pthread_cond_t  cond;
   int             parked;
} __attribute__((aligned(64))) parking_bay_t;

typedef bool (*park_fn_t)(parking_bay_t *, void *);
typedef void (*unpark_fn_t)(parking_bay_t *, void *);

static parking_bay_t parking_bays[PARKING_BAYS] = {
   [0 ... PARKING_BAYS - 1] = {
      PTHREAD_MUTEX_INITIALIZER,
      PTHREAD_COND_INITIALIZER
   }
};

#ifdef DEBUG
static lock_stats_t lock_stats[MAX_THREADS];
#endif

static unsigned max_workers = 0;
static int      last_thread_id = -1;

static __thread nvc_thread_t *my_thread = NULL;

#ifdef DEBUG
static void print_lock_stats(void)
{
   lock_stats_t s = {};
   //  workq_stats_t t = {};
   for (int i = 0; i <= last_thread_id; i++) {
      s.locks     += relaxed_load(&(lock_stats[i].locks));
      s.contended += relaxed_load(&(lock_stats[i].contended));
      s.parks     += relaxed_load(&(lock_stats[i].parks));
      s.spins     += relaxed_load(&(lock_stats[i].spins));
      s.spurious  += relaxed_load(&(lock_stats[i].spurious));
      s.retries   += relaxed_load(&(lock_stats[i].retries));

      // t.comp   += relaxed_load(&(workq_stats[i].comp));
      //t.steals += relaxed_load(&(workq_stats[i].steals));
   }

   printf("\nLock statistics:\n");
   printf("\tTotal locks      : %"PRIi64"\n", s.locks);
   printf("\tContended        : %"PRIi64" (%.1f%%)\n",
          s.contended, 100.0 * ((double)s.contended / (double)s.locks));
   printf("\tParked           : %"PRIi64" (%.1f%%)\n",
          s.parks, 100.0 * ((double)s.parks / (double)s.locks));
   printf("\tAverage spins    : %.1f\n", (double)s.spins / (double)s.retries);
   printf("\tSpurious wakeups : %"PRIi64"\n", s.spurious);

   printf("\nWork queue statistics:\n");
   //   printf("\tCompleted tasks  : %"PRIi64"\n", t.comp);
   //printf("\tSteals           : %"PRIi64"\n", t.steals);
}
#endif

void thread_init(void)
{
   assert(my_thread == NULL);

   my_thread = xcalloc(sizeof(nvc_thread_t));
   my_thread->name   = xstrdup("main thread");
   my_thread->id     = atomic_add(&last_thread_id, 1);
   my_thread->handle = pthread_self();

   assert(my_thread->id == 0);

   max_workers = MIN(nvc_nprocs(), MAX_THREADS);

#ifdef DEBUG
   if (getenv("NVC_THREAD_VERBOSE") != NULL)
      atexit(print_lock_stats);
#endif
}

int thread_id(void)
{
   assert(my_thread != NULL);
   return my_thread->id;
}

static void *thread_wrapper(void *arg)
{
   assert(my_thread == NULL);
   my_thread = arg;

   return (*my_thread->fn)(my_thread->arg);
}

nvc_thread_t *thread_create(thread_fn_t fn, void *arg, const char *fmt, ...)
{
   nvc_thread_t *thread = xcalloc(sizeof(nvc_thread_t));

   va_list ap;
   va_start(ap, fmt);
   thread->name = xvasprintf(fmt, ap);
   va_end(ap);

   thread->id = atomic_add(&last_thread_id, 1);
   assert(thread->id > 0);

   thread->fn  = fn;
   thread->arg = arg;

   if (thread->id >= MAX_THREADS)
      fatal_trace("cannot create more than %d threads", MAX_THREADS);

   PTHREAD_CHECK(pthread_create, &(thread->handle), NULL,
                 thread_wrapper, thread);

   return thread;
}

void *thread_join(nvc_thread_t *thread)
{
   if (thread == my_thread || thread->id == 0)
      fatal_trace("cannot join self or main thread");

   void *retval = NULL;
   PTHREAD_CHECK(pthread_join, thread->handle, &retval);

   free(thread->name);
   free(thread);

   return retval;
}

static parking_bay_t *parking_bay_for(void *cookie)
{
   uint32_t a = (uint32_t)((uintptr_t)cookie >> 2);
   a = (a ^ 61) ^ (a >> 16);
   a = a + (a << 3);
   a = a ^ (a >> 4);
   a = a * UINT32_C(0x27d4eb2d);
   a = a ^ (a >> 15);

   return &(parking_bays[a % PARKING_BAYS]);
}

static void thread_park(void *cookie, park_fn_t fn)
{
   parking_bay_t *bay = parking_bay_for(cookie);

   PTHREAD_CHECK(pthread_mutex_lock, &(bay->mutex));
   {
      if ((*fn)(bay, cookie)) {
         bay->parked++;
         PTHREAD_CHECK(pthread_cond_wait, &(bay->cond), &(bay->mutex));
         assert(bay->parked > 0);
         bay->parked--;
      }
   }
   PTHREAD_CHECK(pthread_mutex_unlock, &(bay->mutex));
}

static void thread_unpark(void *cookie, unpark_fn_t fn)
{
   parking_bay_t *bay = parking_bay_for(cookie);

   PTHREAD_CHECK(pthread_mutex_lock, &(bay->mutex));
   {
      (*fn)(bay, cookie);
   }
   PTHREAD_CHECK(pthread_mutex_unlock, &(bay->mutex));

   PTHREAD_CHECK(pthread_cond_signal, &(bay->cond));
}

void spin_wait(void)
{
#ifdef __x86_64__
   __asm__ volatile ("pause");
#elif defined __aarch64__
   // YIELD is a no-op on most AArch64 cores so also do an ISB to stall
   // the pipeline for a bit
   __asm__ volatile ("yield; isb");
#endif
}

static bool lock_park_cb(parking_bay_t *bay, void *cookie)
{
   nvc_lock_t *lock = cookie;

   // This is called with the park mutex held: check the lock is still
   // owned by someone and the park bit is still set
   return relaxed_load(lock) == (IS_LOCKED | HAS_PARKED);
}

static void lock_unpark_cb(parking_bay_t *bay, void *cookie)
{
   nvc_lock_t *lock = cookie;

   assert(relaxed_load(lock) == (IS_LOCKED | HAS_PARKED));

   // Unlock must have release semantics
   atomic_store(lock, (bay->parked > 0 ? HAS_PARKED : 0));
}

void nvc_lock(nvc_lock_t *lock)
{
   LOCK_EVENT(locks, 1);
   TSAN_PRE_LOCK(lock);

   int8_t state = relaxed_load(lock);
   if (state & IS_LOCKED)
      LOCK_EVENT(contended, 1);
   else if (likely(atomic_cas(lock, state, state | IS_LOCKED)))
      goto locked;  // Fast path: acquired the lock without contention

   for (;;) {
      LOCK_EVENT(retries, 1);

      // Spin a few times waiting for the owner to release the lock
      // before parking
      int spins = 0;
      for (; (state & IS_LOCKED) && spins < MAX_SPINS;
           spins++, state = relaxed_load(lock))
         spin_wait();

      if (spins == MAX_SPINS) {
         // Ignore failures here as we will check the lock state again
         // in the callback with the park mutex held
         atomic_cas(lock, IS_LOCKED, IS_LOCKED | HAS_PARKED);

         LOCK_EVENT(parks, 1);
         thread_park(lock, lock_park_cb);

         if ((state = relaxed_load(lock)) & IS_LOCKED) {
            // Someone else grabbed the lock before our thread was unparked
            LOCK_EVENT(spurious, 1);
            continue;
         }
      }
      else
         LOCK_EVENT(spins, spins);

      assert(!(state & IS_LOCKED));

      // If we get here then we've seen the lock in an unowned state:
      // attempt to grab it with a CAS
      if (__atomic_cas(lock, &state, state | IS_LOCKED))
         goto locked;
   }

 locked:
   TSAN_POST_LOCK(lock);
}

void nvc_unlock(nvc_lock_t *lock)
{
   TSAN_PRE_UNLOCK(lock);

   // Fast path: unlock assuming no parked waiters
   if (likely(atomic_cas(lock, IS_LOCKED, 0)))
      goto unlocked;

   // If we get here then we must own the lock with at least one parked
   // waiting thread
   assert(relaxed_load(lock) == (IS_LOCKED | HAS_PARKED));

   // Lock released in callback
   thread_unpark(lock, lock_unpark_cb);

 unlocked:
   TSAN_POST_UNLOCK(lock);
}

void __scoped_unlock(nvc_lock_t **plock)
{
   nvc_unlock(*plock);
}
