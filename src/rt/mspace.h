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

#ifndef _RT_MSPACE_H
#define _RT_MSPACE_H

#include "prim.h"

typedef uint32_t mptr_t;
#define MPTR_INVALID 0

typedef void (*mspace_oom_fn_t)(mspace_t *, size_t);

#define TLAB_SIZE (64 * 1024)

// The code generator knows the layout of this struct
typedef struct _tlab {
   mspace_t *mspace;
   char     *base;
   char     *alloc;
   char     *limit;
   mptr_t    mptr;
} tlab_t;

#define tlab_valid(t) ((t).base != NULL)

#define tlab_move(from, to) do {                \
      assert(!tlab_valid((to)));                \
      (to) = (from);                            \
      (from) = (tlab_t){};                      \
   } while (0)

#define tlab_reset(t) do {                      \
      assert(tlab_valid((t)));                  \
      (t).alloc = (t).base;                     \
   } while (0)

mspace_t *mspace_new(size_t size);
void mspace_destroy(mspace_t *m);
void *mspace_alloc(mspace_t *m, size_t size);
void mspace_set_oom_handler(mspace_t *m, mspace_oom_fn_t fn);

void tlab_acquire(mspace_t *m, tlab_t *t);
void tlab_release(tlab_t *t);
void *tlab_alloc(tlab_t *t, size_t size);

mptr_t mptr_new(mspace_t *m);
void mptr_free(mspace_t *m, mptr_t *ptr);
void *mptr_get(mspace_t *m, mptr_t ptr);
void mptr_put(mspace_t *m, mptr_t ptr, void *value);

#define MSPACE_CURRENT_FRAME __builtin_frame_address(0)

void mspace_stack_limit(void *limit);

#endif   // _RT_MSPACE_H
