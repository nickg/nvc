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
#include "rt/mspace.h"
#include "thread.h"

#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include <inttypes.h>
#include <unistd.h>

#ifdef HAVE_PTHREAD
#include <pthread.h>
#else
#error missing pthread support
#endif

#ifdef __SANITIZE_THREAD__
#include <sanitizer/tsan_interface.h>
#endif

#define MAX_THREADS  64
#define LOCK_SPINS   15
#define MAX_ACTIVEQS 16
#define MIN_TAKE     8
#define PARKING_BAYS 64

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

#define WORKQ_EVENT(what, n) do {               \
      workq_stats[my_thread->id].what += (n);   \
   } while (0)
#else
#define LOCK_EVENT(what, n)
#define WORKQ_EVENT(what, n)
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

typedef struct {
   task_fn_t  fn;
   void      *context;
   void      *arg;
   workq_t   *workq;
} task_t;

// Work sealing task queue is based on:
//   Arora, N. S., Blumofe, R. D., and Plaxton, C. G.
//   Thread scheduling for multiprogrammed multiprocessors.
//   Theory of Computing Systems 34, 2 (2001), 115-144.

typedef uint32_t abp_idx_t;
typedef uint32_t abp_tag_t;

typedef union {
   struct {
      abp_idx_t top;
      abp_tag_t tag;
   };
   uint64_t bits;
} abp_age_t;

STATIC_ASSERT(sizeof(abp_age_t) <= 8);

#define THREADQ_SIZE 256

typedef struct {
   task_t    deque[THREADQ_SIZE];
   abp_age_t age;
   abp_idx_t bot;
} __attribute__((aligned(64))) threadq_t;

typedef enum {
   IDLE, START, DEAD,
} workq_state_t;

struct _workq {
   workq_t       *next;
   workq_state_t  state;
   nvc_lock_t     lock;
   task_t        *entryq;
   unsigned       queuesz;
   unsigned       wptr;
   unsigned       rptr;
   unsigned       comp;
   int            activeidx;
   void          *context;
   bool           parallel;
};

typedef struct {
   int64_t comp;
   int64_t steals;
   int64_t wakes;
} __attribute__((aligned(64))) workq_stats_t;

STATIC_ASSERT(sizeof(workq_stats_t) == 64)

typedef enum {
   MAIN_THREAD,
   USER_THREAD,
   WORKER_THREAD,
} thread_kind_t;

struct _nvc_thread {
   unsigned        id;
   thread_kind_t   kind;
   unsigned        spins;
   threadq_t       queue;
   char           *name;
   pthread_t       handle;
   thread_fn_t     fn;
   void           *arg;
};

static parking_bay_t parking_bays[PARKING_BAYS] = {
   [0 ... PARKING_BAYS - 1] = {
      PTHREAD_MUTEX_INITIALIZER,
      PTHREAD_COND_INITIALIZER
   }
};

static nvc_thread_t *threads[MAX_THREADS];
static unsigned      max_workers = 0;
static int           running_threads = 0;
static bool          should_stop = false;
static workq_t      *activeqs[MAX_ACTIVEQS];

#ifdef DEBUG
static lock_stats_t  lock_stats[MAX_THREADS];
static workq_stats_t workq_stats[MAX_THREADS];
#endif

static __thread nvc_thread_t *my_thread = NULL;

static parking_bay_t *parking_bay_for(void *cookie);

#ifdef DEBUG
static void print_lock_stats(void)
{
   lock_stats_t s = {};
   workq_stats_t t = {};
   for (int i = 0; i < MAX_THREADS; i++) {
      s.locks     += relaxed_load(&(lock_stats[i].locks));
      s.contended += relaxed_load(&(lock_stats[i].contended));
      s.parks     += relaxed_load(&(lock_stats[i].parks));
      s.spins     += relaxed_load(&(lock_stats[i].spins));
      s.spurious  += relaxed_load(&(lock_stats[i].spurious));
      s.retries   += relaxed_load(&(lock_stats[i].retries));

      t.comp   += relaxed_load(&(workq_stats[i].comp));
      t.steals += relaxed_load(&(workq_stats[i].steals));
      t.wakes  += relaxed_load(&(workq_stats[i].wakes));
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
   printf("\tCompleted tasks  : %"PRIi64"\n", t.comp);
   printf("\tSteals           : %"PRIi64"\n", t.steals);
   printf("\tWakeups          : %"PRIi64"\n", t.wakes);
}
#endif

static void join_worker_threads(void)
{
   atomic_store(&should_stop, true);

   for (int i = 1; i < MAX_THREADS; i++) {
      nvc_thread_t *t = atomic_load(&threads[i]);
      if (t == NULL)
         continue;

      switch (relaxed_load(&t->kind)) {
      case WORKER_THREAD:
         thread_join(t);
         continue;  // Freed thread struct
      case USER_THREAD:
      case MAIN_THREAD:
         fatal_trace("leaked a user thread: %s", t->name);
      }
   }

   assert(atomic_load(&running_threads) == 1);
}

void stop_workers(void)
{
   // Temporary until runtime is thread-safe
   join_worker_threads();
}

static nvc_thread_t *thread_new(thread_fn_t fn, void *arg,
                                thread_kind_t kind, char *name)
{
   nvc_thread_t *thread = xcalloc(sizeof(nvc_thread_t));
   thread->name  = name;
   thread->fn    = fn;
   thread->arg   = arg;
   thread->kind  = kind;

   atomic_store(&thread->queue.age.bits, 0);
   atomic_store(&thread->queue.bot, 0);

   int id = 0;
   for (; id < MAX_THREADS; id++) {
      if (relaxed_load(&(threads[id])) != NULL)
         continue;
      else if (atomic_cas(&(threads[id]), NULL, thread))
         break;
   }

   if (id == MAX_THREADS)
      fatal_trace("cannot create more than %d threads", MAX_THREADS);

   thread->id = id;

   atomic_add(&running_threads, 1);
   return thread;
}

void thread_init(void)
{
   assert(my_thread == NULL);

   my_thread = thread_new(NULL, NULL, MAIN_THREAD, xstrdup("main thread"));
   my_thread->handle = pthread_self();

   assert(my_thread->id == 0);

   max_workers = MIN(nvc_nprocs(), MAX_THREADS);
   assert(max_workers >= 0);

#ifdef DEBUG
   if (getenv("NVC_THREAD_VERBOSE") != NULL)
      atexit(print_lock_stats);
#endif

   atexit(join_worker_threads);
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

   void *result = (*my_thread->fn)(my_thread->arg);

   atomic_add(&running_threads, -1);
   return result;
}

nvc_thread_t *thread_create(thread_fn_t fn, void *arg, const char *fmt, ...)
{
   va_list ap;
   va_start(ap, fmt);
   char *name = xvasprintf(fmt, ap);
   va_end(ap);

   nvc_thread_t *thread = thread_new(fn, arg, USER_THREAD, name);

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

   assert(threads[thread->id] == thread);
   atomic_store(&(threads[thread->id]),  NULL);

   //free(thread->name);
   //free(thread);
   // TODO: free at safe point

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

   if (fn != NULL) {
      PTHREAD_CHECK(pthread_mutex_lock, &(bay->mutex));
      {
         (*fn)(bay, cookie);
      }
      PTHREAD_CHECK(pthread_mutex_unlock, &(bay->mutex));
   }

   // Do not use pthread_cond_signal here as multiple threads parked in
   // this bay here may be waiting on different cookies
   PTHREAD_CHECK(pthread_cond_broadcast, &(bay->cond));
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
      for (; (state & IS_LOCKED) && spins < LOCK_SPINS;
           spins++, state = relaxed_load(lock))
         spin_wait();

      if (spins == LOCK_SPINS) {
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

static void push_bot(threadq_t *tq, const task_t *tasks, size_t count)
{
   const abp_idx_t bot = atomic_load(&tq->bot);
   assert(bot + count <= THREADQ_SIZE);

   memcpy(tq->deque + bot, tasks, count * sizeof(task_t));
   atomic_store(&tq->bot, bot + count);
}

static bool pop_bot(threadq_t *tq, task_t *task)
{
   const abp_idx_t old_bot = atomic_load(&tq->bot);
   if (old_bot == 0)
      return false;

   const abp_idx_t new_bot = old_bot - 1;
   atomic_store(&tq->bot, new_bot);

   *task = tq->deque[new_bot];

   const abp_age_t old_age = { .bits = atomic_load(&tq->age.bits) };
   if (new_bot > old_age.top)
      return true;

   atomic_store(&tq->bot, 0);

   const abp_age_t new_age = { .top = 0, .tag = old_age.tag + 1 };
   if (new_bot == old_age.top) {
      if (atomic_cas(&tq->age.bits, old_age.bits, new_age.bits))
         return true;
   }

   atomic_store(&tq->age.bits, new_age.bits);
   return false;
}

static bool pop_top(threadq_t *tq, task_t *task)
{
   const abp_age_t old_age = { .bits = atomic_load(&tq->age.bits) };
   const abp_idx_t bot = atomic_load(&tq->bot);

   if (bot <= old_age.top)
      return false;

   *task = tq->deque[old_age.top];

   const abp_age_t new_age = {
      .tag = old_age.tag,
      .top = old_age.top + 1
   };

   return atomic_cas(&tq->age.bits, old_age.bits, new_age.bits);
}

workq_t *workq_new(void *context)
{
   if (my_thread->kind != MAIN_THREAD)
      fatal_trace("work queues can only be created by the main thread");

   workq_t *wq = xcalloc(sizeof(workq_t));
   wq->state     = IDLE;
   wq->activeidx = -1;
   wq->context   = context;

   return wq;
}

void workq_free(workq_t *wq)
{
   if (my_thread->kind != MAIN_THREAD)
      fatal_trace("work queues can only be freed by the main thread");

   {
      SCOPED_LOCK(wq->lock);
      assert(wq->state == IDLE);
      wq->state = DEAD;
   }

   // TODO: free at safe point
}

void workq_do(workq_t *wq, task_fn_t fn, void *arg)
{
   if (my_thread->kind != MAIN_THREAD)
      fatal_trace("workq_do can only be called from the main thread");

   assert(wq->state == IDLE);

   if (wq->wptr == wq->queuesz) {
      wq->queuesz = MAX(wq->queuesz * 2, 64);
      wq->entryq = xrealloc_array(wq->entryq, wq->queuesz, sizeof(task_t));
   }

   wq->entryq[wq->wptr++] = (task_t){ fn, wq->context, arg, wq };
}

void workq_scan(workq_t *wq, scan_fn_t fn, void *arg)
{
   SCOPED_LOCK(wq->lock);

   for (int i = wq->rptr; i < wq->wptr; i++)
      (*fn)(wq->context, wq->entryq[i].arg, arg);
}

static size_t workq_take(workq_t *wq, threadq_t *tq)
{
   int from, take;
   {
      SCOPED_LOCK(wq->lock);

      if (wq->state != START)
         return 0;
      else if (wq->wptr == wq->rptr) {
         assert(wq->activeidx != -1);
         atomic_cas(&(activeqs[wq->activeidx]), wq, NULL);
         return 0;
      }

      const int remain = wq->wptr - wq->rptr;
      const int share = wq->wptr / relaxed_load(&running_threads);
      take = MIN(remain, MAX(MIN_TAKE, MIN(THREADQ_SIZE, share)));
      from = wq->rptr;

      wq->rptr += take;
   }

   push_bot(tq, wq->entryq + from, take);
   return take;
}

static int estimate_depth(threadq_t *tq)
{
   if (tq == NULL) return 0;

   const abp_age_t age = { .bits = relaxed_load(&tq->age.bits) };
   const abp_idx_t bot = relaxed_load(&tq->bot);

   return bot <= age.top ? 0 : bot - age.top;
}

static threadq_t *get_thread_queue(int id)
{
   assert(id < MAX_THREADS);

   nvc_thread_t *t = atomic_load(&(threads[id]));
   if (t == NULL)
      return NULL;

   return &(t->queue);
}

static bool workq_poll(workq_t *wq, threadq_t *tq)
{
   int ntasks;
   if ((ntasks = workq_take(wq, tq))) {
      task_t task;
      int comp = 0;
      for (; pop_bot(tq, &task); comp++)
         (*task.fn)(task.context, task.arg);

      WORKQ_EVENT(comp, comp);
      atomic_add(&(wq->comp), comp);
      return true;
   }
   else
      return false;
}

static bool steal_task(void)
{
   int nthreads = relaxed_load(&running_threads), victim;
   if (nthreads > 2) {
      // Pick two threads at random and steal from the one with the
      // longest queue
      int t1, t2;
      do {
         t1 = rand() % nthreads;
      } while (t1 == my_thread->id);
      do {
         t2 = rand() % nthreads;
      } while (t2 == my_thread->id || t1 == t2);

      threadq_t *q1 = get_thread_queue(t1);
      threadq_t *q2 = get_thread_queue(t2);

      if (estimate_depth(q1) > estimate_depth(q2))
         victim = t1;
      else
         victim = t2;
   }
   else if (nthreads == 1)
      victim = my_thread->id ^ 1;  // Pick the only other thread
   else
      return false;   // No one to steal from

   threadq_t *tq = get_thread_queue(victim);

   task_t task;
   if (tq != NULL && pop_top(tq, &task)) {
      WORKQ_EVENT(steals, 1);
      (*task.fn)(task.context, task.arg);
      WORKQ_EVENT(comp, 1);
      atomic_add(&(task.workq->comp), 1);
      return true;
   }
   else
      return false;
}

static void maybe_backoff(void)
{
   const int spins = my_thread->spins++;

   if (spins < 10)
      spin_wait();
   else if (spins < 20) {
      for (int i = 0; i < 50; i++)
         spin_wait();
   }
   else if (spins < 30)
      sched_yield();
   else if (spins < 40)
      usleep(1000);
   else
      usleep(10000);
}

static void *worker_thread(void *arg)
{
   mspace_stack_limit(MSPACE_CURRENT_FRAME);

   do {
      const int bias = rand();
      bool did_work = false;
      for (int i = 0; i < MAX_ACTIVEQS; i++) {
         const int idx = (i + bias) % MAX_ACTIVEQS;
         workq_t *wq = atomic_load(&(activeqs[idx]));
         if (wq == NULL)
            continue;

         did_work |= workq_poll(wq, &(my_thread->queue));
      }

      if (!did_work)
         did_work |= steal_task();

      if (did_work)
         my_thread->spins = 0;
      else
         maybe_backoff();

   } while (likely(!relaxed_load(&should_stop)));

   return NULL;
}

static void create_workers(int needed)
{
   assert(my_thread->kind == MAIN_THREAD);

   if (relaxed_load(&should_stop))
      return;

   while (relaxed_load(&running_threads) < MIN(max_workers, needed)) {
      static int counter = 0;
      char *name = xasprintf("worker thread %d", atomic_add(&counter, 1));
      nvc_thread_t *thread =
         thread_new(worker_thread, NULL, WORKER_THREAD, name);

      PTHREAD_CHECK(pthread_create, &(thread->handle), NULL,
                    thread_wrapper, thread);
   }
}

void workq_start(workq_t *wq)
{
   if (my_thread->kind != MAIN_THREAD)
      fatal_trace("workq_start can only be called from the main thread");

   wq->parallel = max_workers > 0 && !relaxed_load(&should_stop);

   if (wq->parallel) {
      create_workers(wq->wptr);

      SCOPED_LOCK(wq->lock);

      int idx = 0;
      for (; idx < MAX_ACTIVEQS; idx++) {
         if (relaxed_load(&activeqs[idx]) != NULL)
            continue;
         else if (atomic_cas(&activeqs[idx], NULL, wq))
            break;
      }

      if (unlikely(idx >= MAX_ACTIVEQS))
         fatal_trace("too many active work queues");

      assert(wq->state == IDLE);
      wq->state = START;

      assert(wq->activeidx == -1);
      wq->activeidx = idx;
   }
   else {
      assert(wq->state == IDLE);
      wq->state = START;

      assert(wq->rptr == 0);
      for (int i = 0; i < wq->wptr; i++)
         (*wq->entryq[i].fn)(wq->context, wq->entryq[i].arg);

      wq->rptr = wq->comp = wq->wptr;
   }
}

static void workq_parallel_drain(workq_t *wq)
{
   while (workq_poll(wq, &(my_thread->queue)))
      ;

   for (;;) {
      {
         SCOPED_LOCK(wq->lock);
         assert(wq->state == START);

         if (atomic_load(&wq->comp) == wq->wptr) {
            wq->state = IDLE;
            wq->wptr = wq->rptr = wq->comp = 0;

            assert(wq->activeidx != -1);
            assert(relaxed_load(&activeqs[wq->activeidx]) != wq);

            wq->activeidx = -1;
            break;
         }
      }

      if (!steal_task())
         spin_wait();
   }
}

void workq_drain(workq_t *wq)
{
   if (my_thread->kind != MAIN_THREAD)
      fatal_trace("workq_drain can only be called from the main thread");

   if (wq->parallel)
      workq_parallel_drain(wq);
   else {
      assert(wq->state == START);
      wq->state = IDLE;
      wq->wptr = wq->rptr = wq->comp = 0;
   }
}
