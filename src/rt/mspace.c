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
#include "cpustate.h"
#include "diag.h"
#include "mask.h"
#include "option.h"
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

typedef A(uint64_t)     work_list_t;

struct _mptr {
   void       *ptr;
   mptr_t      prev;
   mptr_t      next;
#ifdef DEBUG
   const char *name;
#endif
};

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
   mptr_t           roots;
   mptr_t           free_mptrs;
   mspace_oom_fn_t  oomfn;
   free_list_t     *free_list;
   uint64_t         create_us;
   unsigned         total_gc;
   unsigned         num_cycles;
#ifdef DEBUG
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

   m->space = map_huge_pages(LINE_SIZE, m->maxsize);

   MSPACE_POISON(m->space, m->maxsize);

   mask_init(&(m->headmask), m->maxlines);
   mask_setall(&(m->headmask));

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
#ifdef DEBUG
   if (m->roots != NULL) {
      LOCAL_TEXT_BUF tb = tb_new();
      int n = 0;
      for (mptr_t p = m->roots; p; p = p->next)
         tb_printf(tb, "%s%s", n++ > 0 ? ", " : "", p->name);
      fatal_trace("destroying mspace with %d live mptrs: %s", n, tb_get(tb));
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

   for (mptr_t p = m->free_mptrs, tmp; p; p = tmp) {
      tmp = p->next;
      free(p);
   }

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

            // Make sure the first fault to the page is a write to
            // allocate THP on Linux
            *(volatile char *)base = 0;

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

static void mspace_return_memory(mspace_t *m, char *ptr, size_t size)
{
   assert(is_mspace_ptr(m, ptr));
   assert((uintptr_t)ptr % LINE_SIZE == 0);

   // Same rounding-up as mspace_alloc
   const int nlines = (size + LINE_SIZE) / LINE_SIZE;
   const size_t asize = nlines * LINE_SIZE;

   free_list_t **tail;
   for (tail = &(m->free_list); *tail; tail = &((*tail)->next)) {
      if ((*tail)->ptr + (*tail)->size == ptr) {
         // Coalese after this block
         (*tail)->size += asize;
         break;
      }
      else if (ptr + asize == (*tail)->ptr) {
         // Coalese before this block
         (*tail)->ptr = ptr;
         (*tail)->size += asize;
         break;
      }
   }

   if (*tail == NULL) {
      free_list_t *f = xmalloc(sizeof(free_list_t));
      f->next = NULL;
      f->size = asize;
      f->ptr  = ptr;

      *tail = f;
   }

   MSPACE_POISON(ptr, asize);

   int line = (ptr - m->space) / LINE_SIZE;
   mask_set_range(&(m->headmask), line, nlines);
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
   mptr_t ptr;
   if (m->free_mptrs != NULL) {
      ptr = m->free_mptrs;
      m->free_mptrs = ptr->next;
   }
   else
      ptr = xmalloc(sizeof(struct _mptr));

   if (m->roots != NULL) {
      assert(m->roots->prev == NULL);
      m->roots->prev = ptr;
   }

   ptr->ptr = NULL;
   ptr->next = m->roots;
   ptr->prev = NULL;
   DEBUG_ONLY(ptr->name = name);

   return (m->roots = ptr);
}

void mptr_free(mspace_t *m, mptr_t *ptr)
{
   if (*ptr == MPTR_INVALID)
      return;

   if ((*ptr)->next != NULL)
      (*ptr)->next->prev = (*ptr)->prev;

   if ((*ptr)->prev != NULL)
      (*ptr)->prev->next = (*ptr)->next;
   else {
      assert(m->roots == *ptr);
      m->roots = (*ptr)->next;
   }

   (*ptr)->prev = NULL;
   (*ptr)->ptr  = NULL;
   DEBUG_ONLY((*ptr)->name = NULL);

   (*ptr)->next = m->free_mptrs;
   m->free_mptrs = *ptr;

   *ptr = NULL;
}

void **mptr_get(mptr_t ptr)
{
   assert(ptr != MPTR_INVALID);
   return &(ptr->ptr);
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
   *mptr_get(t->mptr) = t->base;
}

void tlab_release(tlab_t *t)
{
   if (!tlab_valid(*t))
      return;

   assert(t->alloc <= t->limit);

   mspace_return_memory(t->mspace, t->base, TLAB_SIZE);

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

static void mspace_mark_mptr(mspace_t *m, mptr_t ptr, gc_state_t *state)
{
#ifdef DEBUG
   if (unlikely(ptr->ptr != NULL && !is_mspace_ptr(m, ptr->ptr)))
      fatal_trace("mptr %s points to unknown address %p", ptr->name, ptr->ptr);
#endif

   mspace_mark_root(m, (intptr_t)ptr->ptr, state);
}

__attribute__((no_sanitize_address, noinline))
static void mspace_gc(mspace_t *m)
{
   const uint64_t start_ticks = get_timestamp_us();

   if (stack_limit == NULL)
      fatal_trace("GC requested without setting stack limit");

   gc_state_t state = {};
   mask_init(&(state.markmask), m->maxlines);

   for (mptr_t p = m->roots; p; p = p->next)
      mspace_mark_mptr(m, p, &state);

   struct cpu_state cpu;
   capture_registers(&cpu);

   for (int i = 0; i < MAX_CPU_REGS; i++)
      mspace_mark_root(m, cpu.regs[i], &state);

   intptr_t *stack_top = (intptr_t *)cpu.sp;
   assert(stack_top <= stack_limit);   // Stack must grow down

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
