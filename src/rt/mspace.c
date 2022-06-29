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
#include "array.h"
#include "diag.h"
#include "mask.h"
#include "opt.h"
#include "rt/mspace.h"

#include <assert.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#if __SANITIZE_ADDRESS__
#include <sanitizer/asan_interface.h>

#define MSPACE_POISON(addr, size) ASAN_POISON_MEMORY_REGION(addr, size)
#define MSPACE_UNPOISON(addr, size) ASAN_UNPOISON_MEMORY_REGION(addr, size)
#else
#define MSPACE_POISON(addr, size)
#define MSPACE_UNPOISON(addr, size)
#endif

#define LINE_SIZE  32
#define LINE_WORDS (LINE_SIZE / sizeof(intptr_t))

typedef A(mptr_t)       mptr_list_t;
typedef A(void *)       root_list_t;
typedef A(uint64_t)     work_list_t;
typedef A(const char *) str_list_t;

typedef struct {
   bit_mask_t  markmask;
   work_list_t worklist;
} gc_state_t;

typedef struct _free_list free_list_t;

struct _free_list {
   free_list_t *next;
   char        *ptr;
   size_t       size;
};

struct _mspace {
   size_t           maxsize;
   unsigned         maxlines;
   char            *space;
   bit_mask_t       headmask;
   root_list_t      roots;
   mptr_list_t      free_mptrs;
   mspace_oom_fn_t  oomfn;
   free_list_t     *free_list;
   uint64_t         create_us;
   unsigned         total_gc;
   unsigned         num_cycles;
#ifdef DEBUG
   str_list_t       mptr_names;
   bool             stress;
#endif
};

static __thread intptr_t *stack_limit = NULL;

static void mspace_gc(mspace_t *m);
static bool is_mspace_ptr(mspace_t *m, char *p);

mspace_t *mspace_new(size_t size)
{
   mspace_t *m = xcalloc(sizeof(mspace_t));
   m->maxsize  = ALIGN_UP(size, LINE_SIZE);
   m->maxlines = m->maxsize / LINE_SIZE;

   DEBUG_ONLY(m->stress = opt_get_int(OPT_GC_STRESS));

   m->space = nvc_memalign(LINE_SIZE, m->maxsize);

   MSPACE_POISON(m->space, m->maxsize);

   mask_init(&(m->headmask), m->maxlines);
   mask_setall(&(m->headmask));

   APUSH(m->roots, NULL);    // Dummy MPTR_INVALID root
   DEBUG_ONLY(APUSH(m->mptr_names, NULL));

   free_list_t *f = xmalloc(sizeof(free_list_t));
   f->next = NULL;
   f->ptr  = m->space;
   f->size = m->maxsize;

   m->free_list = f;

   m->create_us = get_timestamp_us();
   return m;
}

void mspace_destroy(mspace_t *m)
{
#ifndef NDEBUG
   if (m->free_mptrs.count != m->roots.count - 1) {
      LOCAL_TEXT_BUF tb = tb_new();
      for (int i = 0, n = 0; i < m->mptr_names.count; i++) {
         if (m->mptr_names.items[i] != NULL)
            tb_printf(tb, "%s%s", n++ > 0 ? ", " : "", m->mptr_names.items[i]);
      }
      fatal_trace("destroying mspace with %d live mptrs: %s",
                  m->roots.count - m->free_mptrs.count - 1, tb_get(tb));
   }
#endif

   if (opt_get_verbose(OPT_GC_VERBOSE, NULL) && m->num_cycles > 0) {
      const uint64_t destroy_us = get_timestamp_us();
      const double gc_frac = m->total_gc / (double)(destroy_us - m->create_us);
      debugf("GC: %d collection cycles; %d us total; %.1f%% of overall "
             "run time", m->num_cycles, m->total_gc, gc_frac * 100.0);
   }

   for (free_list_t *it = m->free_list, *tmp; it; it = tmp) {
      tmp = it->next;
      free(it);
   }

   ACLEAR(m->roots);
   DEBUG_ONLY(ACLEAR(m->mptr_names));
   ACLEAR(m->free_mptrs);
   mask_free(&(m->headmask));
   nvc_munmap(m->space, m->maxsize);
   free(m);
}

void mspace_stack_limit(void *limit)
{
   assert(limit != NULL);
   stack_limit = limit;
}

void *mspace_alloc(mspace_t *m, size_t size)
{
   if (size == 0)
      return NULL;

#ifdef DEBUG
   if (m->stress)
      mspace_gc(m);
#endif

   // Add one to size before rounding up to LINE_SIZE to allow a valid
   // pointer to point at one element past the end of an array
   const int nlines = (size + LINE_SIZE) / LINE_SIZE;
   const size_t asize = nlines * LINE_SIZE;

   int retry = 1;
   do {
      for (free_list_t **it = &(m->free_list); *it; it = &((*it)->next)) {
         assert((*it)->size % LINE_SIZE == 0);
         if ((*it)->size >= asize) {
            char *base = (*it)->ptr;
            assert((uintptr_t)base % LINE_SIZE == 0);

            MSPACE_UNPOISON(base, size);

            const int line = (base - m->space) / LINE_SIZE;
            mask_set(&(m->headmask), line);
            if (nlines > 1)
               mask_clear_range(&(m->headmask), line + 1, nlines - 1);

            if ((*it)->size == asize) {
               free_list_t *next = (*it)->next;
               free(*it);
               *it = next;
            }
            else {
               (*it)->size -= asize;
               (*it)->ptr += asize;
            }

            return base;
         }
      }

      mspace_gc(m);
   } while (retry--);

   if (m->oomfn) {
      (*m->oomfn)(m, size);
      return NULL;
   }
   else
      fatal_trace("out of memory attempting to allocate %zu byte object", size);
}

void *mspace_alloc_array(mspace_t *m, int nelems, size_t size)
{
   return mspace_alloc(m, nelems * size);
}

void *mspace_alloc_flex(mspace_t *m, size_t fixed, int nelems, size_t size)
{
   return mspace_alloc(m, fixed + nelems * size);
}

void mspace_set_oom_handler(mspace_t *m, mspace_oom_fn_t fn)
{
   m->oomfn = fn;
}

mptr_t mptr_new(mspace_t *m, const char *name)
{
   if (m->free_mptrs.count > 0) {
      mptr_t ptr = APOP(m->free_mptrs);
      assert(AGET(m->roots, ptr) == NULL);
      assert(AGET(m->mptr_names, ptr) == NULL);
      DEBUG_ONLY(m->mptr_names.items[ptr] = name);
      return ptr;
   }
   else {
      mptr_t ptr = m->roots.count;
      APUSH(m->roots, NULL);
      DEBUG_ONLY(APUSH(m->mptr_names, name));
      return ptr;
   }
}

void mptr_free(mspace_t *m, mptr_t *ptr)
{
   if (*ptr == MPTR_INVALID)
      return;

   assert(*ptr < m->roots.count);
   assert(*ptr < m->mptr_names.count);
   assert(m->free_mptrs.count < m->roots.count);

   m->roots.items[*ptr] = NULL;
   DEBUG_ONLY(m->mptr_names.items[*ptr] = NULL);
   APUSH(m->free_mptrs, *ptr);
   *ptr = MPTR_INVALID;
}

void *mptr_get(mspace_t *m, mptr_t ptr)
{
   assert(ptr != MPTR_INVALID);
   assert(ptr < m->roots.count);

   return m->roots.items[ptr];
}

void mptr_put(mspace_t *m, mptr_t ptr, void *value)
{
   assert(ptr != MPTR_INVALID);
   assert(ptr < m->roots.count);
   assert(value == NULL || is_mspace_ptr(m, value));

   m->roots.items[ptr] = value;
}

void tlab_acquire(mspace_t *m, tlab_t *t)
{
   assert(!tlab_valid(*t));

   t->mspace = m;
   t->base   = mspace_alloc(m, TLAB_SIZE);
   t->limit  = t->base + TLAB_SIZE;
   t->alloc  = t->base;
   t->mptr   = mptr_new(m, "tlab");

   // This ensures the TLAB is kept alive over GCs
   mptr_put(m, t->mptr, t->base);
}

void tlab_release(tlab_t *t)
{
   if (!tlab_valid(*t))
      return;

   assert(t->alloc <= t->limit);

   free_list_t *f = xmalloc(sizeof(free_list_t));
   f->next = t->mspace->free_list;
   f->size = TLAB_SIZE;
   f->ptr  = t->base;

   t->mspace->free_list = f;

   MSPACE_POISON(t->base, t->limit - t->base);

   int line = ((char *)t->base - t->mspace->space) / LINE_SIZE;
   mask_set_range(&(t->mspace->headmask), line, TLAB_SIZE / LINE_SIZE);

   mptr_free(t->mspace, &(t->mptr));
   *t = (tlab_t){};
}

void *tlab_alloc(tlab_t *t, size_t size)
{
   assert(t->alloc <= t->limit);
   assert(((uintptr_t)t->alloc & (sizeof(double) - 1)) == 0);

   if (t->alloc + size <= t->limit) {
      void *p = t->alloc;
      t->alloc += ALIGN_UP(size, sizeof(double));
      return p;
   }
   else
      return mspace_alloc(t->mspace, size);
}

static bool is_mspace_ptr(mspace_t *m, char *p)
{
   return p >= m->space && p < m->space + m->maxsize;
}

static void mspace_mark_root(mspace_t *m, intptr_t p, gc_state_t *state)
{
   if (is_mspace_ptr(m, (char *)p)) {
      int line = ((char *)p - m->space) / LINE_SIZE;

      // Scan backwards to the start of the object
      line = mask_scan_backwards(&(m->headmask), line);
      assert(line != -1);

      int objlen = 1;
      if (line + 1 < m->maxlines)
         objlen += mask_count_clear(&(m->headmask), line + 1);

      if (!mask_test(&(state->markmask), line)) {
         mask_set_range(&(state->markmask), line, objlen);

         uint64_t enc = ((uint64_t)line << 32) | objlen;
         APUSH(state->worklist, enc);
      }
   }
}

__attribute__((noinline))
static intptr_t *current_stack_pointer(void)
{
   return MSPACE_CURRENT_FRAME;
}

__attribute__((no_sanitize_address, noinline))
static void mspace_gc(mspace_t *m)
{
   const uint64_t start_ticks = get_timestamp_us();

   if (stack_limit == NULL)
      fatal_trace("GC requested without setting stack limit");

   intptr_t *stack_top = current_stack_pointer();
   assert(stack_top <= stack_limit);   // Stack must grow down

   gc_state_t state = {};
   mask_init(&(state.markmask), m->maxlines);

   for (int i = 0; i < m->roots.count; i++)
      mspace_mark_root(m, (intptr_t)m->roots.items[i], &state);

   uintptr_t regs[MAX_CPU_REGS];
   capture_registers(regs);

   for (int i = 0; i < MAX_CPU_REGS; i++)
      mspace_mark_root(m, regs[i], &state);

   for (intptr_t *p = stack_top; p < stack_limit; p++)
      mspace_mark_root(m, *p, &state);

   while (state.worklist.count > 0) {
      const uint64_t enc = APOP(state.worklist);
      const int line = enc >> 32;
      const int objlen = enc & 0xffffffff;

      for (int i = 0; i < objlen; i++) {
         intptr_t *words = (intptr_t *)(m->space + (line + i) * LINE_SIZE);
         for (int j = 0; j < LINE_WORDS; j++)
            mspace_mark_root(m, words[j], &state);
      }
   }

#if __SANITIZE_ADDRESS__
   for (int i = 0; i < m->maxlines; i++) {
      if (!mask_test(&(state.markmask), i))
         MSPACE_POISON(m->space + i * LINE_SIZE, LINE_SIZE);
   }
#endif

   for (free_list_t *it = m->free_list, *tmp; it; it = tmp) {
      tmp = it->next;
      free(it);
   }
   m->free_list = NULL;

   int freefrags = 0, freelines = 0;
   free_list_t **tail = &(m->free_list);
   for (int line = 0; line < m->maxlines;) {
      const int clear = mask_count_clear(&(state.markmask), line);
      if (clear == 0)
         line++;
      else {
         free_list_t *f = xmalloc(sizeof(free_list_t));
         f->next = NULL;
         f->ptr  = m->space + line * LINE_SIZE;
         f->size = clear * LINE_SIZE;

         *tail = f;
         tail = &(f->next);

         mask_set_range(&(m->headmask), line, clear);

         freefrags++;
         freelines += clear;

         line += clear;
      }
   }

   if (opt_get_verbose(OPT_GC_VERBOSE, NULL)) {
      const int ticks = get_timestamp_us() - start_ticks;
      debugf("GC: allocated %d/%zu; fragmentation %.2g%% [%d us]",
             mask_popcount(&(state.markmask)) * LINE_SIZE, m->maxsize,
             ((double)(freefrags - 1) / (double)freelines) * 100.0, ticks);

      m->total_gc += ticks;
      m->num_cycles++;
   }

   mask_free(&(state.markmask));

   assert(state.worklist.count == 0);
   ACLEAR(state.worklist);
}

void *mspace_find(mspace_t *m, void *ptr, size_t *size)
{
   if (!is_mspace_ptr(m, ptr)) {
      *size = 0;
      return NULL;
   }

   int line = ((char *)ptr - m->space) / LINE_SIZE;

   // Scan backwards to the start of the object
   line = mask_scan_backwards(&(m->headmask), line);
   assert(line != -1);

   int objlen = 1;
   if (line + 1 < m->maxlines)
      objlen += mask_count_clear(&(m->headmask), line + 1);

   *size = objlen * LINE_SIZE;
   return m->space + line * LINE_SIZE;
}
