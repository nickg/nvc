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

#ifndef _THREAD_H
#define _THREAD_H

#include <stdint.h>

#define atomic_add(p, n) __atomic_add_fetch((p), (n), __ATOMIC_SEQ_CST)
#define atomic_load(p) __atomic_load_n((p), __ATOMIC_SEQ_CST)
#define atomic_store(p, v) __atomic_store_n((p), (v), __ATOMIC_SEQ_CST)
#define atomic_xchg(p, v) __atomic_exchange_n((p), (v), __ATOMIC_SEQ_CST)

#define __atomic_cas(p, old, new)                                       \
   __atomic_compare_exchange_n((p), (old), (new), false,                \
                               __ATOMIC_SEQ_CST, __ATOMIC_RELAXED)

#define atomic_cas(p, old, new) ({                                      \
      typeof(*p) __cmp = (old);                                         \
      __atomic_cas((p), &__cmp, (new));                                 \
    })

#define relaxed_add(p, n) __atomic_add_fetch((p), (n), __ATOMIC_RELAXED)
#define relaxed_load(p) __atomic_load_n((p), __ATOMIC_RELAXED)
#define relaxed_store(p, v) __atomic_store_n((p), (v), __ATOMIC_RELAXED)

typedef struct _nvc_thread nvc_thread_t;

void thread_init(void);
int thread_id(void);

typedef void *(*thread_fn_t)(void *);

nvc_thread_t *thread_create(thread_fn_t fn, void *arg, const char *fmt, ...)
  __attribute__((format(printf, 3, 4)));
void *thread_join(nvc_thread_t *thread);

void spin_wait(void);

typedef int8_t nvc_lock_t;

void nvc_lock(nvc_lock_t *lock);
void nvc_unlock(nvc_lock_t *lock);

void __scoped_unlock(nvc_lock_t **plock);

#define SCOPED_LOCK(lock)                               \
  __attribute__((cleanup(__scoped_unlock), unused))     \
  nvc_lock_t *__lock = &(lock);                         \
  nvc_lock(&(lock));

#endif  // _THREAD_H
