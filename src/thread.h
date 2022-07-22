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

#define atomic_add(p, n) __atomic_add_fetch((p), (n), __ATOMIC_ACQ_REL)
#define atomic_load(p) __atomic_load_n((p), __ATOMIC_ACQUIRE)
#define atomic_store(p, v) __atomic_store_n((p), (v), __ATOMIC_RELEASE)
#define atomic_xchg(p, v) __atomic_exchange_n((p), (v), __ATOMIC_ACQ_REL)

#define atomic_cas(p, old, new) ({                                      \
      typeof(*p) __cmp = (old);                                         \
      __atomic_compare_exchange_n((p), &__cmp, (new), false,            \
                                  __ATOMIC_ACQ_REL,                     \
                                  __ATOMIC_ACQUIRE);                    \
    })

#define relaxed_add(p, n) __atomic_add_fetch((p), (n), __ATOMIC_RELAXED)
#define relaxed_load(p) __atomic_load_n((p), __ATOMIC_RELAXED)
#define relaxed_store(p, v) __atomic_store_n((p), (v), __ATOMIC_RELAXED)

typedef struct _nvc_thread nvc_thread_t;
typedef struct _nvc_mutex  nvc_mutex_t;

nvc_thread_t *thread_create(void *(*fn)(void *), void *arg,
                            const char *fmt, ...)
  __attribute__((format(printf, 3, 4)));
void *thread_join(nvc_thread_t *thread);

nvc_mutex_t *mutex_create(void);
void mutex_lock(nvc_mutex_t *mtx);
void mutex_unlock(nvc_mutex_t *mtx);
void mutex_destroy(nvc_mutex_t *mtx);

void spin_wait(void);

void __scoped_unlock(nvc_mutex_t **pmtx);

#define SCOPED_LOCK(mtx)                                \
  __attribute__((cleanup(__scoped_unlock), unused))     \
  nvc_mutex_t *__lock = mtx;                            \
  mutex_lock(mtx);

#endif  // _THREAD_H
