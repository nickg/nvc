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

#include <stdlib.h>
#include <string.h>

#ifdef HAVE_PTHREAD
#include <pthread.h>
#else
#error missing pthread support
#endif

struct _nvc_thread {
   unsigned   id;
   char      *name;
   pthread_t  handle;
};

struct _nvc_mutex {
   pthread_mutex_t mutex;
};

nvc_thread_t *thread_create(void *(*fn)(void *), void *arg,
                            const char *fmt, ...)
{
   nvc_thread_t *thread = xcalloc(sizeof(nvc_thread_t));

   va_list ap;
   va_start(ap, fmt);
   thread->name = xvasprintf(fmt, ap);
   va_end(ap);

   if (pthread_create(&(thread->handle), NULL, fn, arg) != 0)
      fatal_errno("pthread_create");

   return thread;
}

void *thread_join(nvc_thread_t *thread)
{
   void *retval = NULL;

   if (pthread_join(thread->handle, &retval) != 0)
      fatal_errno("pthread_join");

   free(thread->name);
   free(thread);

   return retval;
}

nvc_mutex_t *mutex_create(void)
{
   nvc_mutex_t *mtx = xcalloc(sizeof(nvc_mutex_t));

   if (pthread_mutex_init(&(mtx->mutex), NULL) != 0)
      fatal_errno("pthread_mutex_init");

   return mtx;
}

void mutex_lock(nvc_mutex_t *mtx)
{
   if (unlikely(pthread_mutex_lock(&(mtx->mutex)) != 0))
      fatal_errno("pthread_mutex_lock");
}

void mutex_unlock(nvc_mutex_t *mtx)
{
   if (unlikely(pthread_mutex_unlock(&(mtx->mutex)) != 0))
      fatal_errno("pthread_mutex_unlock");
}

void mutex_destroy(nvc_mutex_t *mtx)
{
   if (pthread_mutex_destroy(&(mtx->mutex)) != 0)
      fatal_errno("pthread_mutex_destroy");

   free(mtx);
}

void spin_wait(void)
{
#ifdef __x86_64__
   __asm__ ("pause");
#endif
}

void __scoped_unlock(nvc_mutex_t **pmtx)
{
   mutex_unlock(*pmtx);
}
