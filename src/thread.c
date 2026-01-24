//
//  Copyright (C) 2021-2023  Nick Gasson
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
#include "array.h"
#include "cpustate.h"
#include "rt/mspace.h"
#include "thread.h"

#include <assert.h>
#include <errno.h>
#include <math.h>
#include <signal.h>
#include <stdlib.h>
#include <string.h>
#include <inttypes.h>
#include <unistd.h>
#include <semaphore.h>

#ifdef HAVE_PTHREAD
#include <pthread.h>
#elif !defined __MINGW32__
#error missing pthread support
#endif

#ifdef __SANITIZE_THREAD__
#include <sanitizer/tsan_interface.h>
#endif

#if defined __MINGW32__
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#elif defined __APPLE__
#define task_t __task_t
#include <mach/thread_act.h>
#include <mach/machine.h>
#undef task_t
#endif

#define LOCK_SPINS      64
#define YIELD_SPINS     32
#define MIN_TAKE        8
#define PARKING_BAYS    64
#define SUSPEND_TIMEOUT 1
#define THREAD_NAME_LEN 16

#if !defined __MINGW32__ && !defined __APPLE__
#define POSIX_SUSPEND 1
#define SIGSUSPEND    SIGRTMIN
#define SIGRESUME     (SIGRTMIN + 1)
#endif

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
#define LOCK_EVENT(what, n) do {                  \
      if (likely(my_thread != NULL))              \
         lock_stats[my_thread->id].what += (n);   \
   } while (0)

#define WORKQ_EVENT(what, n) do {                 \
      if (likely(my_thread != NULL))              \
         workq_stats[my_thread->id].what += (n);  \
   } while (0)
#else
#define LOCK_EVENT(what, n) (void)(n)
#define WORKQ_EVENT(what, n) (void)(n)
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

#ifndef PTHREAD_ADAPTIVE_MUTEX_INITIALIZER_NP
#define PTHREAD_ADAPTIVE_MUTEX_INITIALIZER_NP PTHREAD_MUTEX_INITIALIZER
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
#ifdef __MINGW32__
   CRITICAL_SECTION   mutex;
   CONDITION_VARIABLE cond;
#else
   pthread_mutex_t    mutex;
   pthread_cond_t     cond;
#endif
   int                parked;
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
} threadq_t;

typedef enum { IDLE, START } workq_state_t;

typedef union {
   struct {
      uint32_t count;
      uint32_t epoch;
   };
   uint64_t bits;
} entryq_ptr_t;

STATIC_ASSERT(sizeof(entryq_ptr_t) <= 8);

typedef struct {
   task_t       *tasks;
   unsigned      queuesz;
   entryq_ptr_t  wptr;
   entryq_ptr_t  comp;
} __attribute__((aligned(64))) entryq_t;

STATIC_ASSERT(sizeof(entryq_t) == 64);

struct _workq {
   void          *context;
   workq_state_t  state;
   unsigned       epoch;
   unsigned       maxthread;
   bool           parallel;
   entryq_t       entryqs[MAX_THREADS];
};

typedef struct {
   int64_t comp;
   int64_t steals;
   int64_t wakes;
} __attribute__((aligned(64))) workq_stats_t;

STATIC_ASSERT(sizeof(workq_stats_t) == 64);

typedef enum {
   MAIN_THREAD,
   USER_THREAD,
   WORKER_THREAD,
} thread_kind_t;

typedef char thread_name_t[THREAD_NAME_LEN];

struct _nvc_thread {
   unsigned        id;
   thread_kind_t   kind;
   unsigned        spins;
   threadq_t       queue;
   thread_name_t   name;
   thread_fn_t     fn;
   void           *arg;
   int             victim;
   uint32_t        rngstate;
#ifdef __MINGW32__
   HANDLE          handle;
   void           *retval;
#else
   pthread_t       handle;
#endif
#ifdef __APPLE__
   thread_port_t   port;
#endif
};

typedef struct {
   nvc_lock_t   lock;
   task_t      *tasks;
   unsigned     wptr;
   unsigned     rptr;
   unsigned     max;
} globalq_t;

typedef struct _barrier {
   unsigned count;
   unsigned reached;
   unsigned passed;
} __attribute__((aligned(64))) barrier_t;

static parking_bay_t parking_bays[PARKING_BAYS] = {
#ifndef __MINGW32__
   [0 ... PARKING_BAYS - 1] = {
      PTHREAD_ADAPTIVE_MUTEX_INITIALIZER_NP,
      PTHREAD_COND_INITIALIZER
   }
#endif
};

static nvc_thread_t    *threads[MAX_THREADS];
static unsigned         max_workers = 0;
static int              running_threads = 0;
static unsigned         max_thread_id = 0;
static bool             should_stop = false;
static globalq_t        globalq __attribute__((aligned(64)));
static int              async_pending __attribute__((aligned(64))) = 0;
static nvc_lock_t       stop_lock = 0;
static stop_world_fn_t  stop_callback = NULL;
static void            *stop_arg = NULL;

#ifdef __MINGW32__
static CONDITION_VARIABLE wake_workers = CONDITION_VARIABLE_INIT;
static CRITICAL_SECTION   wakelock;
#else
static pthread_cond_t     wake_workers = PTHREAD_COND_INITIALIZER;
static pthread_mutex_t    wakelock = PTHREAD_ADAPTIVE_MUTEX_INITIALIZER_NP;
#endif

#ifdef POSIX_SUSPEND
static sem_t stop_sem;
#endif

#ifdef DEBUG
static lock_stats_t  lock_stats[MAX_THREADS];
static workq_stats_t workq_stats[MAX_THREADS];
#endif

static __thread nvc_thread_t *my_thread = NULL;

static parking_bay_t *parking_bay_for(void *cookie);

#ifdef POSIX_SUSPEND
static void suspend_handler(int sig, siginfo_t *info, void *context);
#endif

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

#ifdef __MINGW32__
static inline void platform_mutex_lock(LPCRITICAL_SECTION lpcs)
{
   EnterCriticalSection(lpcs);
}

static inline void platform_mutex_unlock(LPCRITICAL_SECTION lpcs)
{
   LeaveCriticalSection(lpcs);
}

static inline void platform_cond_broadcast(PCONDITION_VARIABLE pcv)
{
   WakeAllConditionVariable(pcv);
}

static inline void platform_cond_wait(PCONDITION_VARIABLE pcv,
                                      LPCRITICAL_SECTION lpcs)
{
   SleepConditionVariableCS(pcv, lpcs, INFINITE);
}
#else
static inline void platform_mutex_lock(pthread_mutex_t *mtx)
{
   PTHREAD_CHECK(pthread_mutex_lock, mtx);
}

static inline void platform_mutex_unlock(pthread_mutex_t *mtx)
{
   PTHREAD_CHECK(pthread_mutex_unlock, mtx);
}

static inline void platform_cond_broadcast(pthread_cond_t *cond)
{
   PTHREAD_CHECK(pthread_cond_broadcast, cond);
}

static inline void platform_cond_wait(pthread_cond_t *cond,
                                      pthread_mutex_t *mtx)
{
   PTHREAD_CHECK(pthread_cond_wait, cond, mtx);
}
#endif

static void join_worker_threads(void)
{
   SCOPED_A(nvc_thread_t *) join_list = AINIT;

   for (int i = 1; i < MAX_THREADS; i++) {
      nvc_thread_t *t = atomic_load(&threads[i]);
      if (t != NULL)
         APUSH(join_list, t);
   }

   // Lock the wake mutex here to avoid races with workers sleeping
   platform_mutex_lock(&wakelock);
   {
      atomic_store(&should_stop, true);
      platform_cond_broadcast(&wake_workers);
   }
   platform_mutex_unlock(&wakelock);

   for (int i = 0; i < join_list.count; i++) {
      nvc_thread_t *t = join_list.items[i];

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

   for (int i = 0; i < join_list.count; i++)
      free(join_list.items[i]);

#ifdef DEBUG
   for (int i = 0; i < PARKING_BAYS; i++)
      assert(parking_bays[i].parked == 0);
#endif
}

static nvc_thread_t *thread_new(thread_fn_t fn, void *arg, thread_kind_t kind,
                                const thread_name_t name)
{
   nvc_thread_t *thread = xcalloc(sizeof(nvc_thread_t));
   thread->fn       = fn;
   thread->arg      = arg;
   thread->kind     = kind;
   thread->rngstate = rand();
   memcpy(thread->name, name, THREAD_NAME_LEN);

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

   unsigned max = relaxed_load(&max_thread_id);
   while (max < id) {
      if (__atomic_cas(&max_thread_id, &max, id))
         break;
   }

   atomic_add(&running_threads, 1);
   return thread;
}

#ifdef POSIX_SUSPEND
static void unmask_fatal_signals(sigset_t *mask)
{
   sigdelset(mask, SIGQUIT);
   sigdelset(mask, SIGABRT);
   sigdelset(mask, SIGTERM);
}
#endif

#ifdef __APPLE__
static void reset_mach_ports(void)
{
   for (int i = 0; i < MAX_THREADS; i++) {
      nvc_thread_t *t = atomic_load(&(threads[i]));
      if (t == NULL)
         continue;

      // Mach ports are not valid after fork
      t->port = pthread_mach_thread_np(t->handle);
   }
}
#endif

void thread_init(void)
{
   assert(my_thread == NULL);

   const thread_name_t name = "main thread";
   my_thread = thread_new(NULL, NULL, MAIN_THREAD, name);

#ifdef __MINGW32__
   my_thread->handle = GetCurrentThread();
#else
   my_thread->handle = pthread_self();
#endif

#ifdef __APPLE__
   my_thread->port = pthread_mach_thread_np(my_thread->handle);
   pthread_atfork(NULL, NULL, reset_mach_ports);
#endif

#ifdef __MINGW32__
   InitializeCriticalSectionAndSpinCount(&wakelock, LOCK_SPINS);
   InitializeConditionVariable(&wake_workers);

   for (int i = 0; i < PARKING_BAYS; i++) {
      parking_bay_t *bay = &(parking_bays[i]);
      InitializeCriticalSectionAndSpinCount(&(bay->mutex), LOCK_SPINS);
      InitializeConditionVariable(&(bay->cond));
   }
#endif

   assert(my_thread->id == 0);

   const char *max_env = getenv("NVC_MAX_THREADS");
   if (max_env != NULL)
      max_workers = MAX(1, MIN(atoi(max_env), MAX_THREADS));
   else
      max_workers = DEFAULT_THREADS;

   const int num_cpus = nvc_nprocs();
   max_workers = MIN(num_cpus, max_workers);

   const char *jobs_env = getenv("NVC_CONCURRENT_JOBS");
   if (jobs_env != NULL) {
      const int num_jobs = MAX(1, atoi(jobs_env));
      const int limit = (int)round((double)num_cpus / (double)num_jobs);
      max_workers = MAX(1, MIN(max_workers, limit));
   }

   assert(max_workers > 0);

#ifdef DEBUG
   if (getenv("NVC_THREAD_VERBOSE") != NULL)
      atexit(print_lock_stats);
#endif

   atexit(join_worker_threads);

#ifdef POSIX_SUSPEND
   sem_init(&stop_sem, 0, 0);

   struct sigaction sa = {
      .sa_sigaction = suspend_handler,
      .sa_flags = SA_RESTART | SA_SIGINFO
   };
   sigfillset(&sa.sa_mask);
   unmask_fatal_signals(&sa.sa_mask);

   sigaction(SIGSUSPEND, &sa, NULL);
   sigaction(SIGRESUME, &sa, NULL);

   sigset_t mask;
   PTHREAD_CHECK(pthread_sigmask, SIG_SETMASK, NULL, &mask);

   sigdelset(&mask, SIGSUSPEND);
   sigaddset(&mask, SIGRESUME);

   PTHREAD_CHECK(pthread_sigmask, SIG_SETMASK, &mask, NULL);
#endif
}

int thread_id(void)
{
   assert(my_thread != NULL);
   return my_thread->id;
}

bool thread_attached(void)
{
   return my_thread != NULL;
}

void thread_sleep(int usec)
{
   usleep(usec);
}

static void *thread_wrapper(void *arg)
{
   assert(my_thread == NULL);
   my_thread = arg;

   void *result = (*my_thread->fn)(my_thread->arg);

   // Avoid races with stop_world
   SCOPED_LOCK(stop_lock);

   assert(threads[my_thread->id] == my_thread);
   atomic_store(&(threads[my_thread->id]),  NULL);

   atomic_add(&running_threads, -1);
   return result;
}

#ifdef __MINGW32__
static DWORD win32_thread_wrapper(LPVOID param)
{
   void *ret = thread_wrapper(param);
   atomic_store(&(my_thread->retval), ret);
   return 0;
}
#endif

static void thread_start(nvc_thread_t *thread)
{
   assert_lock_held(&stop_lock);

#ifdef __MINGW32__
   if ((thread->handle = CreateThread(NULL, 0, win32_thread_wrapper,
                                      thread, 0, NULL)) == NULL)
      fatal_errno("CreateThread");
#else
   PTHREAD_CHECK(pthread_create, &(thread->handle), NULL,
                 thread_wrapper, thread);
#endif

#ifdef __APPLE__
   thread->port = pthread_mach_thread_np(thread->handle);
#endif
}

nvc_thread_t *thread_create(thread_fn_t fn, void *arg, const char *fmt, ...)
{
   va_list ap;
   va_start(ap, fmt);

   thread_name_t name;
   checked_vsprintf(name, THREAD_NAME_LEN, fmt, ap);

   va_end(ap);

   // Avoid races with stop_world
   SCOPED_LOCK(stop_lock);

   nvc_thread_t *thread = thread_new(fn, arg, USER_THREAD, name);
   thread_start(thread);

   return thread;
}

void *thread_join(nvc_thread_t *thread)
{
   if (thread == my_thread || thread->kind == MAIN_THREAD)
      fatal_trace("cannot join self or main thread");

   void *retval = NULL;
#ifdef __MINGW32__
   if (WaitForSingleObject(thread->handle, INFINITE) == WAIT_FAILED)
      fatal_errno("WaitForSingleObject failed for thread %s", thread->name);

   retval = atomic_load(&(thread->retval));
#else
   PTHREAD_CHECK(pthread_join, thread->handle, &retval);
#endif

   async_free(thread->name);
   async_free(thread);

   return retval;
}

nvc_thread_t *get_thread(int id)
{
   assert(id >= 0 && id < MAX_THREADS);
   return atomic_load(&threads[id]);
}

static inline parking_bay_t *parking_bay_for(void *cookie)
{
   return &(parking_bays[mix_bits_64(cookie) % PARKING_BAYS]);
}

static void thread_park(void *cookie, park_fn_t fn)
{
   parking_bay_t *bay = parking_bay_for(cookie);

   platform_mutex_lock(&(bay->mutex));
   {
      if ((*fn)(bay, cookie)) {
         bay->parked++;
         platform_cond_wait(&(bay->cond), &(bay->mutex));
         assert(bay->parked > 0);
         bay->parked--;
      }
   }
   platform_mutex_unlock(&(bay->mutex));
}

static void thread_unpark(void *cookie, unpark_fn_t fn)
{
   parking_bay_t *bay = parking_bay_for(cookie);

   if (fn != NULL) {
      platform_mutex_lock(&(bay->mutex));
      {
         (*fn)(bay, cookie);
      }
      platform_mutex_unlock(&(bay->mutex));
   }

   // Do not use pthread_cond_signal here as multiple threads parked in
   // this bay may be waiting on different cookies
   platform_cond_broadcast(&(bay->cond));
}

void spin_wait(void)
{
#if defined ARCH_X86_64
   __asm__ volatile ("pause");
#elif defined ARCH_ARM64
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

   int8_t state = 0;
   if (likely(__atomic_cas(lock, &state, state | IS_LOCKED)))
      goto locked;  // Fast path: acquired the lock without contention

   LOCK_EVENT(contended, 1);

   for (;;) {
      LOCK_EVENT(retries, 1);

      // Spin a few times waiting for the owner to release the lock
      // before parking
      int spins = 0;
      for (; (state & IS_LOCKED) && spins < LOCK_SPINS;
           spins++, state = relaxed_load(lock))
         spin_wait();

      if (state & IS_LOCKED) {
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

#ifdef DEBUG
void assert_lock_held(nvc_lock_t *lock)
{
   int8_t state = relaxed_load(lock);
   if (unlikely(!(state & IS_LOCKED)))
      fatal_trace("expected lock at %p to be held", lock);
}
#endif

void __scoped_unlock(nvc_lock_t **plock)
{
   nvc_unlock(*plock);
}

static void push_bot(threadq_t *tq, const task_t *tasks, size_t count)
{
   const abp_idx_t bot = relaxed_load(&tq->bot);
   assert(bot + count <= THREADQ_SIZE);

   memcpy(tq->deque + bot, tasks, count * sizeof(task_t));
   store_release(&tq->bot, bot + count);
}

static bool pop_bot(threadq_t *tq, task_t *task)
{
   const abp_idx_t old_bot = relaxed_load(&tq->bot);
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

__attribute__((no_sanitize("thread")))
static bool pop_top(threadq_t *tq, task_t *task)
{
   const abp_age_t old_age = { .bits = atomic_load(&tq->age.bits) };
   const abp_idx_t bot = atomic_load(&tq->bot);

   if (bot <= old_age.top)
      return false;

   // This triggers a TSAN false-positive: we will never read from *task
   // if the CAS fails below, so it's safe to ignore
   *task = tq->deque[old_age.top];

   const abp_age_t new_age = {
      .tag = old_age.tag,
      .top = old_age.top + 1
   };

   return atomic_cas(&tq->age.bits, old_age.bits, new_age.bits);
}

static void globalq_put(globalq_t *gq, const task_t *tasks, size_t count)
{
   assert_lock_held(&gq->lock);

   if (gq->wptr == gq->rptr)
      gq->wptr = gq->rptr = 0;

   if (gq->wptr + count > gq->max) {
      gq->max = next_power_of_2(gq->wptr + count);
      gq->tasks = xrealloc_array(gq->tasks, gq->max, sizeof(task_t));
   }

   memcpy(gq->tasks + gq->wptr, tasks, count * sizeof(task_t));
   gq->wptr += count;
}

__attribute__((no_sanitize("thread")))
static bool globalq_unlocked_empty(globalq_t *gq)
{
   return relaxed_load(&gq->wptr) == relaxed_load(&gq->rptr);
}

static size_t globalq_take(globalq_t *gq, threadq_t *tq)
{
   if (globalq_unlocked_empty(gq))
      return 0;

   const int nthreads = relaxed_load(&running_threads);

   SCOPED_LOCK(gq->lock);

   if (gq->wptr == gq->rptr)
      return 0;

   const int remain = gq->wptr - gq->rptr;
   const int share = gq->wptr / nthreads;
   const int take = MIN(remain, MAX(MIN_TAKE, MIN(THREADQ_SIZE, share)));
   const int from = gq->rptr;

   gq->rptr += take;

   push_bot(tq, gq->tasks + from, take);
   return take;
}

static void execute_task(task_t *task)
{
   (*task->fn)(task->context, task->arg);

   if (task->workq != NULL) {
      entryq_t *eq = &(task->workq->entryqs[my_thread->id]);

      const entryq_ptr_t cur = { .bits = relaxed_load(&eq->comp.bits) };
      const int epoch = atomic_load(&task->workq->epoch);
      const int count = cur.epoch == epoch ? cur.count : 0;
      const entryq_ptr_t next = { .count = count + 1, .epoch = epoch };
      store_release(&eq->comp.bits, next.bits);
   }
   else
      atomic_add(&async_pending, -1);
}

static bool globalq_poll(globalq_t *gq, threadq_t *tq)
{
   int ntasks;
   if ((ntasks = globalq_take(gq, tq))) {
      task_t task;
      int comp = 0;
      for (; pop_bot(tq, &task); comp++)
         execute_task(&task);

      WORKQ_EVENT(comp, comp);
      return true;
   }
   else
      return false;
}

workq_t *workq_new(void *context)
{
   if (my_thread->kind != MAIN_THREAD)
      fatal_trace("work queues can only be created by the main thread");

   workq_t *wq = xcalloc(sizeof(workq_t));
   wq->state    = IDLE;
   wq->context  = context;
   wq->parallel = max_workers > 1;
   wq->epoch    = 1;

   return wq;
}

void workq_not_thread_safe(workq_t *wq)
{
   wq->parallel = false;
}

void workq_free(workq_t *wq)
{
   if (my_thread->kind != MAIN_THREAD)
      fatal_trace("work queues can only be freed by the main thread");

   assert(wq->state == IDLE);

   for (int i = 0; i < MAX_THREADS; i++)
      free(wq->entryqs[i].tasks);

   free(wq);
}

void workq_do(workq_t *wq, task_fn_t fn, void *arg)
{
   assert(wq->state == IDLE);

   entryq_t *eq = &(wq->entryqs[my_thread->id]);

   const entryq_ptr_t cur = { .bits = relaxed_load(&eq->wptr.bits) };
   const int epoch = atomic_load(&wq->epoch);
   const int wptr = cur.epoch == epoch ? cur.count : 0;

   if (wptr == eq->queuesz) {
      eq->queuesz = MAX(eq->queuesz * 2, 64);
      eq->tasks = xrealloc_array(eq->tasks, eq->queuesz, sizeof(task_t));
   }

   eq->tasks[wptr] = (task_t){ fn, wq->context, arg, wq };

   const entryq_ptr_t next = { .count = wptr + 1, .epoch = epoch };
   store_release(&eq->wptr.bits, next.bits);

   unsigned maxthread = relaxed_load(&wq->maxthread);
   while (maxthread < my_thread->id) {
      if (__atomic_cas(&wq->maxthread, &maxthread, my_thread->id))
         break;
   }
}

void workq_scan(workq_t *wq, scan_fn_t fn, void *arg)
{
   const int maxthread = relaxed_load(&wq->maxthread);
   for (int i = 0; i <= maxthread; i++) {
      entryq_t *eq = &(wq->entryqs[my_thread->id]);
      const entryq_ptr_t wptr = { .bits = load_acquire(&eq->wptr.bits) };
      for (int j = 0; j < wptr.count; j++)
         (*fn)(wq->context, eq->tasks[j].arg, arg);
   }
}

static int estimate_depth(threadq_t *tq)
{
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

static uint32_t fast_rand(void)
{
   uint32_t state = my_thread->rngstate;
   state ^= (state << 13);
   state ^= (state >> 17);
   state ^= (state << 5);
   return (my_thread->rngstate = state);
}

static threadq_t *find_victim(void)
{
   threadq_t *last = get_thread_queue(my_thread->victim);
   if (last != NULL && estimate_depth(last) > 0)
      return last;

   const int maxthread = relaxed_load(&max_thread_id);
   const int start = fast_rand() % (maxthread + 1);
   int idx = start;
   do {
      if (idx != my_thread->id) {
         threadq_t *q = get_thread_queue(idx);
         if (q != NULL && estimate_depth(q) > 0) {
            my_thread->victim = idx;
            return q;
         }
      }
   } while ((idx = (idx + 1) % (maxthread + 1)) != start);

   return NULL;
}

static bool steal_task(void)
{
   threadq_t *tq = find_victim();
   if (tq == NULL)
      return false;

   task_t task;
   if (pop_top(tq, &task)) {
      WORKQ_EVENT(steals, 1);
      execute_task(&task);
      WORKQ_EVENT(comp, 1);
      return true;
   }

   return false;
}

static void progressive_backoff(void)
{
   if (my_thread->spins++ < YIELD_SPINS)
      spin_wait();
   else {
#ifdef __MINGW32__
      SwitchToThread();
#else
      sched_yield();
#endif
      my_thread->spins = 0;
   }
}

static void *worker_thread(void *arg)
{
   mspace_stack_limit(MSPACE_CURRENT_FRAME);

   do {
      if (globalq_poll(&globalq, &(my_thread->queue)) || steal_task())
         my_thread->spins = 0;  // Did work
      else if (my_thread->spins++ < 2)
         spin_wait();
      else {
         platform_mutex_lock(&wakelock);
         {
            if (!relaxed_load(&should_stop))
               platform_cond_wait(&wake_workers, &wakelock);
         }
         platform_mutex_unlock(&wakelock);
      }
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
      thread_name_t name;
      checked_sprintf(name, THREAD_NAME_LEN, "worker thread %d",
                      atomic_add(&counter, 1));
      SCOPED_LOCK(stop_lock);   // Avoid races with stop_world
      nvc_thread_t *thread =
         thread_new(worker_thread, NULL, WORKER_THREAD, name);
      thread_start(thread);
   }

   platform_cond_broadcast(&wake_workers);
}

void workq_start(workq_t *wq)
{
   assert(my_thread->kind == MAIN_THREAD);

   const int epoch = relaxed_load(&wq->epoch);
   const int maxthread = relaxed_load(&wq->maxthread);

   assert(wq->state == IDLE);
   wq->state = START;

   int nserial = 0, nparallel = 0;
   for (int i = 0; i <= maxthread; i++) {
      entryq_t *eq = &wq->entryqs[i];
      const entryq_ptr_t wptr = { .bits = load_acquire(&eq->wptr.bits) };
      if (wptr.epoch == epoch) {
         // Only bump epoch if there are tasks to run
         if (nserial + nparallel == 0)
            atomic_add(&wq->epoch, 1);

         assert(wptr.count > 0);

         if (i == maxthread && nserial + nparallel == 0 && wptr.count == 1) {
            execute_task(&(eq->tasks[0]));   // Only one task in total
            nserial++;
         }
         else if (wq->parallel) {
            if (nparallel == 0)
               nvc_lock(&globalq.lock);   // Lazily acquire lock
            globalq_put(&globalq, eq->tasks, wptr.count);
            nparallel += wptr.count;
         }
         else {
            for (int j = 0; j < wptr.count; j++)
               execute_task(&(eq->tasks[j]));
            nserial += wptr.count;
         }
      }
   }

   if (wq->parallel && nparallel > 0) {
      nvc_unlock(&globalq.lock);
      create_workers(nparallel);
   }
}

static int workq_outstanding(workq_t *wq)
{
   assert(wq->state == START);

   const int epoch = atomic_load(&wq->epoch);
   const int maxthread = relaxed_load(&max_thread_id);

   int pending = 0;
   for (int i = 0; i <= maxthread; i++) {
      entryq_t *eq = &(wq->entryqs[i]);

      const entryq_ptr_t wptr = { .bits = load_acquire(&eq->wptr.bits) };
      if (wptr.epoch == epoch - 1)
         pending += wptr.count;

      const entryq_ptr_t comp = { .bits = load_acquire(&eq->comp.bits) };
      if (comp.epoch == epoch)
         pending -= comp.count;
   }

   assert(pending >= 0);
   return pending;
}

static void workq_parallel_drain(workq_t *wq)
{
   if (workq_outstanding(wq) > 0) {
      while (globalq_poll(&globalq, &(my_thread->queue)));
      while (steal_task());

      while (workq_outstanding(wq) > 0)
         progressive_backoff();
   }

   wq->state = IDLE;
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
   }
}

void async_do(task_fn_t fn, void *context, void *arg)
{
   if (max_workers == 1)
      (*fn)(context, arg);   // Single CPU
   else {
      const int npending = atomic_add(&async_pending, 1);
      create_workers(npending + 1 /* Do not count main thread */);

      task_t tasks[1] = {
         { fn, context, arg, NULL }
      };
      SCOPED_LOCK(globalq.lock);
      globalq_put(&globalq, tasks, 1);
   }
}

void async_barrier(void)
{
   while (atomic_load(&async_pending) > 0) {
      if (!globalq_poll(&globalq, &(my_thread->queue)))
         progressive_backoff();
   }
}

void async_free(void *ptr)
{
   if (ptr == NULL)
      return;

   // TODO: free when all threads in quiescent state
}

#ifdef POSIX_SUSPEND
static void suspend_handler(int sig, siginfo_t *info, void *context)
{
   if (info->si_pid != getpid())
      return;   // Not sent by us, ignore it
   else if (sig == SIGRESUME)
      return;

   const int olderrno = errno;

   if (my_thread != NULL) {
      struct cpu_state cpu;
      fill_cpu_state(&cpu, (ucontext_t *)context);

      stop_world_fn_t callback = atomic_load(&stop_callback);
      void *arg = atomic_load(&stop_arg);

      (*callback)(my_thread->id, &cpu, arg);
   }

   sem_post(&stop_sem);

   sigset_t mask;
   sigfillset(&mask);
   unmask_fatal_signals(&mask);
   sigdelset(&mask, SIGRESUME);

   sigsuspend(&mask);

   sem_post(&stop_sem);

   errno = olderrno;
}
#endif

void stop_world(stop_world_fn_t callback, void *arg)
{
   nvc_lock(&stop_lock);

   atomic_store(&stop_callback, callback);
   atomic_store(&stop_arg, arg);

#ifdef __MINGW32__
   const int maxthread = relaxed_load(&max_thread_id);
   for (int i = 0; i <= maxthread; i++) {
      nvc_thread_t *thread = atomic_load(&threads[i]);
      if (thread == NULL || thread == my_thread)
         continue;

      if (SuspendThread(thread->handle) != 0)
         fatal_errno("SuspendThread");

      CONTEXT context;
      context.ContextFlags = CONTEXT_INTEGER | CONTEXT_CONTROL;
      if (!GetThreadContext(thread->handle, &context))
         fatal_errno("GetThreadContext");

      struct cpu_state cpu;
      fill_cpu_state(&cpu, &context);

      (*callback)(thread->id, &cpu, arg);
   }
#elif defined __APPLE__
   const int maxthread = relaxed_load(&max_thread_id);
   for (int i = 0; i <= maxthread; i++) {
      nvc_thread_t *thread = atomic_load(&threads[i]);
      if (thread == NULL || thread == my_thread)
         continue;

      assert(thread->port != MACH_PORT_NULL);

      kern_return_t kern_result;
      do {
         kern_result = thread_suspend(thread->port);
      } while (kern_result == KERN_ABORTED);

      if (kern_result != KERN_SUCCESS)
         fatal_trace("failed to suspend thread %d (%d)", i, kern_result);

#ifdef ARCH_ARM64
      arm_thread_state64_t state;
      mach_msg_type_number_t count = ARM_THREAD_STATE64_COUNT;
      thread_state_flavor_t flavor = ARM_THREAD_STATE64;
#else
      x86_thread_state64_t state;
      mach_msg_type_number_t count = x86_THREAD_STATE64_COUNT;
      thread_state_flavor_t flavor = x86_THREAD_STATE64;
#endif
      kern_result = thread_get_state(thread->port, flavor,
                                     (natural_t *)&state, &count);
      if (kern_result != KERN_SUCCESS)
         fatal_trace("failed to get thread %d state (%d)", i, kern_result);

      // Fake a ucontext_t that we can pass to fill_cpu_state
      ucontext_t uc;
      typeof(*uc.uc_mcontext) mc;
      uc.uc_mcontext = &mc;
      mc.__ss = state;

      struct cpu_state cpu;
      fill_cpu_state(&cpu, &uc);

      (*callback)(thread->id, &cpu, arg);
   }
#elif defined __SANITIZE_THREAD__
   // https://github.com/google/sanitizers/issues/1179
   fatal_trace("stop_world is not supported with tsan");
#else
   assert(sem_trywait(&stop_sem) == -1 && errno == EAGAIN);

   int signalled = 0;
   const int maxthread = relaxed_load(&max_thread_id);
   for (int i = 0; i <= maxthread; i++) {
      nvc_thread_t *thread = atomic_load(&threads[i]);
      if (thread == NULL || thread == my_thread)
         continue;

      PTHREAD_CHECK(pthread_kill, thread->handle, SIGSUSPEND);
      signalled++;
   }

   struct timespec ts;
   if (clock_gettime(CLOCK_REALTIME, &ts) != 0)
      fatal_errno("clock_gettime");

   ts.tv_sec += SUSPEND_TIMEOUT;

   for (; signalled > 0; signalled--) {
      if (sem_timedwait(&stop_sem, &ts) != 0)
         fatal_trace("timeout waiting for %d threads to suspend", signalled);
   }

   assert(sem_trywait(&stop_sem) == -1 && errno == EAGAIN);
#endif

   struct cpu_state cpu;
   capture_registers(&cpu);

   (*callback)(my_thread->id, &cpu, arg);
}

void start_world(void)
{
   assert_lock_held(&stop_lock);

   const int maxthread = relaxed_load(&max_thread_id);

#ifdef __MINGW32__
   for (int i = 0; i <= maxthread; i++) {
      nvc_thread_t *thread = atomic_load(&threads[i]);
      if (thread == NULL || thread == my_thread)
         continue;

      if (ResumeThread(thread->handle) != 1)
         fatal_errno("ResumeThread");
   }
#elif defined __APPLE__
   for (int i = 0; i <= maxthread; i++) {
      nvc_thread_t *thread = atomic_load(&threads[i]);
      if (thread == NULL || thread == my_thread)
         continue;

      kern_return_t kern_result;
      do {
         kern_result = thread_resume(thread->port);
      } while (kern_result == KERN_ABORTED);

      if (kern_result != KERN_SUCCESS)
         fatal_trace("failed to resume thread %d (%d)", i, kern_result);
   }
#else
   assert(sem_trywait(&stop_sem) == -1 && errno == EAGAIN);

   int signalled = 0;
   for (int i = 0; i <= maxthread; i++) {
      nvc_thread_t *thread = atomic_load(&threads[i]);
      if (thread == NULL || thread == my_thread)
         continue;

      PTHREAD_CHECK(pthread_kill, thread->handle, SIGRESUME);
      signalled++;
   }

   struct timespec ts;
   if (clock_gettime(CLOCK_REALTIME, &ts) != 0)
      fatal_errno("clock_gettime");

   ts.tv_sec += SUSPEND_TIMEOUT;

   for (; signalled > 0; signalled--) {
      if (sem_timedwait(&stop_sem, &ts) != 0)
         fatal_trace("timeout waiting for %d threads to resume", signalled);
   }

   assert(sem_trywait(&stop_sem) == -1 && errno == EAGAIN);
#endif

   nvc_unlock(&stop_lock);
}

void thread_wx_mode(wx_mode_t mode)
{
#ifdef __APPLE__
   pthread_jit_write_protect_np(mode == WX_EXECUTE);
#else
   // Could use Intel memory protection keys here
#endif
}

barrier_t *barrier_new(int count)
{
   barrier_t *b = xcalloc(sizeof(barrier_t));
   b->count = count;
   return b;
}

void barrier_free(barrier_t *b)
{
   free(b);
}

void barrier_wait(barrier_t *b)
{
   const int count = relaxed_load(&b->count);
   const int passed = relaxed_load(&b->passed);

   if (atomic_fetch_add(&b->reached, 1) == count - 1) {
      // Last thread to pass barrier
      relaxed_store(&b->reached, 0);
      store_release(&b->passed, passed + 1);
   }
   else {
      while (load_acquire(&b->passed) == passed)
         progressive_backoff();
   }
}
