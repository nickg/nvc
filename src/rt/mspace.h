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

// Extra padding at the end of heap regions to allow vectorised
// intrinsics to read past the end of an array
#define OVERRUN_MARGIN 32    // AVX2 has 32-byte vectors

#define MPTR_INVALID NULL
typedef struct _mptr *mptr_t;

typedef void *UNSAFE_MPTR;

#define tlab_reset(t) do {                      \
      assert((t)->alloc <= (t)->limit);         \
      (t)->alloc = 0;                           \
   } while (0)

#define tlab_mark(t) (t)->alloc                 \

#define tlab_trim(t, mark) do {                 \
      assert((t)->alloc >= (mark));             \
      (t)->alloc = (mark);                      \
   } while (0)

typedef struct {
   void (*mark)(mspace_t *m, void *cookie, void *ctx);
   void (*oom)(mspace_t *m, size_t size, void *ctx);
   void *context;
} mspace_handler_t;

mspace_t *mspace_new(size_t size, const mspace_handler_t *h);
void mspace_destroy(mspace_t *m);
void *mspace_alloc(mspace_t *m, size_t size);
void *mspace_alloc_array(mspace_t *m, int nelems, size_t size);
void *mspace_alloc_flex(mspace_t *m, size_t fixed, int nelems, size_t size);
void *mspace_find(mspace_t *m, void *ptr, size_t *size);

void mspace_user_mark(mspace_t *m, void *cookie, const void *ptr, size_t size);

mptr_t mptr_new(mspace_t *m, const char *name);
void mptr_free(mspace_t *m, mptr_t *ptr);
void **mptr_get(mptr_t ptr);

#define MSPACE_CURRENT_FRAME __builtin_frame_address(0)

void mspace_stack_limit(void *limit);

#endif   // _RT_MSPACE_H
