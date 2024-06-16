//
//  Copyright (C) 2022-2024  Nick Gasson
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
#include "thread.h"

#include <assert.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <inttypes.h>

#define LINE_SIZE  32
#define LINE_WORDS (LINE_SIZE / sizeof(intptr_t))
#define MAX_HEAP   (UINT64_C(0x100000000) * LINE_SIZE)

// Extra padding at the end of heap regions to allow vectorised
// intrinsics to read past the end of an array
#define OVERRUN_MARGIN 32    // AVX2 has 32-byte vectors

STATIC_ASSERT(OVERRUN_MARGIN % LINE_SIZE == 0);

typedef A(uint64_t) work_list_t;
typedef struct _linked_tlab linked_tlab_t;

typedef struct _linked_tlab {
   linked_tlab_t *next;
   linked_tlab_t *prev;
   tlab_t         tlab;
} linked_tlab_t;

struct _mptr {
   void       *ptr;
   mptr_t      prev;
   mptr_t      next;
#ifdef DEBUG
   const char *name;
#endif
};

typedef struct {
   bit_mask_t        markmask;
   work_list_t       worklist;
   struct cpu_state  cpu[MAX_THREADS];
#if __SANITIZE_ADDRESS__
   void             *fake_stack[MAX_THREADS];
#endif
} gc_state_t;

typedef struct _free_list free_list_t;

struct _free_list {
   free_list_t *next;
   char        *ptr;
   size_t       size;
};

struct _mspace {
   nvc_lock_t       lock;
   size_t           maxsize;
   size_t           maxlines;
   char            *space;
   bit_mask_t       headmask;
   mptr_t           roots;
   mptr_t           free_mptrs;
   mspace_oom_fn_t  oomfn;
   free_list_t     *free_list;
   uint64_t         create_us;
   linked_tlab_t   *live_tlabs;
   linked_tlab_t   *free_tlabs;
   unsigned         total_gc;
   unsigned         num_cycles;
#ifdef DEBUG
   bool             stress;
#endif
};

static intptr_t *stack_limit[MAX_THREADS];

static void mspace_gc(mspace_t *m);
static bool is_mspace_ptr(mspace_t *m, char *p);

mspace_t *mspace_new(size_t size)
{
   mspace_t *m = xcalloc(sizeof(mspace_t));
   m->maxsize  = ALIGN_UP(size, LINE_SIZE);
   m->maxlines = (m->maxsize - OVERRUN_MARGIN) / LINE_SIZE;

   if (m->maxsize > MAX_HEAP)
      fatal("the maximum supported heap size is %"PRIu64"g",
            MAX_HEAP / 1024 / 1024 / 1024);

   DEBUG_ONLY(m->stress = opt_get_int(OPT_GC_STRESS));

   m->space = map_huge_pages(LINE_SIZE, m->maxsize);

   ASAN_POISON(m->space, m->maxsize);

   mask_init(&(m->headmask), m->maxlines);
   mask_setall(&(m->headmask));

   free_list_t *f = xmalloc(sizeof(free_list_t));
   f->next = NULL;
   f->ptr  = m->space;
   f->size = m->maxsize - OVERRUN_MARGIN;

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

   assert(m->live_tlabs == NULL);
   for (linked_tlab_t *lt = m->free_tlabs, *tmp; lt; lt = tmp) {
      tmp = lt->next;
      free(lt);
   }

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
   atomic_store(&(stack_limit[thread_id()]), limit);
}

static void *mspace_try_alloc(mspace_t *m, size_t size)
{
   // Add one to size before rounding up to LINE_SIZE to allow a valid
   // pointer to point at one element past the end of an array
   const size_t nlines = (size + LINE_SIZE) / LINE_SIZE;
   const size_t asize = nlines * LINE_SIZE;

   SCOPED_LOCK(m->lock);

   for (free_list_t **it = &(m->free_list); *it; it = &((*it)->next)) {
      assert((*it)->size % LINE_SIZE == 0);
      if ((*it)->size >= asize) {
         char *base = (*it)->ptr;
         assert((uintptr_t)base % LINE_SIZE == 0);
         assert(base >= m->space);
         assert(base < m->space + m->maxsize);

         ASAN_UNPOISON(base, size);

         const ptrdiff_t line = (base - m->space) / LINE_SIZE;
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

   return NULL;
}

void *mspace_alloc(mspace_t *m, size_t size)
{
   if (size == 0)
      return NULL;

#ifdef DEBUG
   if (stack_limit[thread_id()] == NULL)
      fatal_trace("cannot allocate without setting stack limit");
   else if (m->stress)
      mspace_gc(m);
#endif

   int retry = 1;
   do {
      void *ptr = mspace_try_alloc(m, size);
      if (ptr != NULL)
         return ptr;

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
   SCOPED_LOCK(m->lock);

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

   SCOPED_LOCK(m->lock);

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

#ifdef DEBUG
static bool tlab_on_list(linked_tlab_t *lt, linked_tlab_t *list)
{
   for (linked_tlab_t *it = list; it; it = it->next) {
      if (it == lt)
         return true;
   }

   return false;
}
#endif

tlab_t *tlab_acquire(mspace_t *m)
{
   SCOPED_LOCK(m->lock);

   linked_tlab_t *lt = m->free_tlabs;
   if (lt == NULL) {
      lt = xmalloc(sizeof(linked_tlab_t) + TLAB_SIZE);
      lt->tlab.mspace = m;
   }
   else {
      assert(!tlab_on_list(lt, m->live_tlabs));
      assert(lt->prev == NULL);
      m->free_tlabs = lt->next;
   }

   lt->next = m->live_tlabs;
   lt->prev = NULL;

   if (m->live_tlabs != NULL) {
      assert(m->live_tlabs->prev == NULL);
      m->live_tlabs->prev = lt;
   }

   m->live_tlabs = lt;

   // Ensure proper starting alignment on 32-bit systems
   const size_t data_off = offsetof(tlab_t, data);
   lt->tlab.alloc = ALIGN_UP(data_off, sizeof(double)) - data_off;
   lt->tlab.limit = TLAB_SIZE - OVERRUN_MARGIN;

   return &(lt->tlab);
}

void tlab_release(tlab_t *t)
{
   if (t == NULL)
      return;

   SCOPED_LOCK(t->mspace->lock);

   linked_tlab_t *lt = container_of(t, linked_tlab_t, tlab);

   assert(t->alloc <= t->limit);
   assert(!tlab_on_list(lt, t->mspace->free_tlabs));
   assert(tlab_on_list(lt, t->mspace->live_tlabs));

   if (lt->prev == NULL)
      t->mspace->live_tlabs = lt->next;
   else {
      assert(lt->prev->next == lt);
      lt->prev->next = lt->next;
   }

   if (lt->next != NULL)
      lt->next->prev = lt->prev;

   assert(!tlab_on_list(lt, t->mspace->live_tlabs));

   lt->prev = NULL;
   lt->next = t->mspace->free_tlabs;
   t->mspace->free_tlabs = lt;
}

void *tlab_alloc(tlab_t *t, size_t size)
{
   assert(t->alloc <= t->limit);
   assert((t->alloc & (sizeof(double) - 1)) == 0);

   if (t->alloc + size <= t->limit) {
      void *p = t->data + t->alloc;
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
      ptrdiff_t line = ((char *)p - m->space) / LINE_SIZE;
      assert(line < UINT32_MAX);   // Enforced by MAX_HEAP

      // Scan backwards to the start of the object
      line = mask_scan_backwards(&(m->headmask), line);
      assert(line != -1);

      size_t objlen = 1;
      if (line + 1 < m->maxlines)
         objlen += mask_count_clear(&(m->headmask), line + 1);
      assert(objlen < UINT32_MAX);

      if (!mask_test(&(state->markmask), line)) {
         mask_set_range(&(state->markmask), line, objlen);

         uint64_t enc = ((uint64_t)line << 32) | objlen;
         APUSH(state->worklist, enc);
      }
   }
}

static void mspace_suspend_cb(int thread_id, struct cpu_state *cpu, void *arg)
{
   gc_state_t *state = arg;
   assert(thread_id < MAX_THREADS);
   state->cpu[thread_id] = *cpu;
#if __SANITIZE_ADDRESS__
   state->fake_stack[thread_id] = __asan_get_current_fake_stack();
#endif
}

__attribute__((no_sanitize_address, noinline))
static void mspace_gc(mspace_t *m)
{
   const uint64_t start_ticks = get_timestamp_us();

#ifdef DEBUG
   if (stack_limit[thread_id()] == NULL)
      fatal_trace("GC requested without setting stack limit");
#endif

#if defined __SANITIZE_THREAD__ && !defined __APPLE__
   return;   // Cannot reliably suspend threads with tsan
#endif

   gc_state_t state = {};
   mask_init(&(state.markmask), m->maxlines);

   SCOPED_LOCK(m->lock);

   stop_world(mspace_suspend_cb, &state);

   for (int i = 0; i < MAX_THREADS; i++) {
      if (get_thread(i) == NULL)
         continue;

      intptr_t *limit = atomic_load(&(stack_limit[i]));
      if (limit == NULL)
         continue;

      for (int j = 0; j < MAX_CPU_REGS; j++)
         mspace_mark_root(m, state.cpu[i].regs[j], &state);

      intptr_t *stack_top = (intptr_t *)state.cpu[i].sp;
      assert(stack_top <= limit);   // Stack must grow down

      for (intptr_t *p = stack_top; p < limit; p++) {
         mspace_mark_root(m, *p, &state);

#if __SANITIZE_ADDRESS__
         // Address sanitiser relocates possibly-escaping stack
         // allocations to a "fake stack" on the heap which we also need
         // to scan to find roots
         if (state.fake_stack[i] != NULL) {
            void *beg, *end;
            if (__asan_addr_is_in_fake_stack(state.fake_stack[i], (void *)*p,
                                             &beg, &end)) {
               for (intptr_t *p2 = beg; p2 < (intptr_t *)end; p2++)
                  mspace_mark_root(m, *p2, &state);
            }
         }
#endif
      }
   }

   for (mptr_t p = m->roots; p; p = p->next)
      mspace_mark_root(m, (intptr_t)p->ptr, &state);

   for (linked_tlab_t *lt = m->live_tlabs; lt; lt = lt->next) {
      for (char *p = lt->tlab.data; p < lt->tlab.data + lt->tlab.alloc;
           p += sizeof(intptr_t))
         mspace_mark_root(m, *(intptr_t *)p, &state);
   }

   while (state.worklist.count > 0) {
      const uint64_t enc = APOP(state.worklist);
      const uint32_t line = enc >> 32;
      const uint32_t objlen = enc & 0xffffffff;

      for (size_t i = 0; i < objlen; i++) {
         const ptrdiff_t off = (uintptr_t)(line + i) * LINE_SIZE;
         intptr_t *words = (intptr_t *)(m->space + off);
         for (int j = 0; j < LINE_WORDS; j++)
            mspace_mark_root(m, words[j], &state);
      }
   }

#if __SANITIZE_ADDRESS__
   for (int i = 0; i < m->maxlines; i++) {
      if (!mask_test(&(state.markmask), i))
         ASAN_POISON(m->space + i * LINE_SIZE, LINE_SIZE);
   }
#endif

   for (free_list_t *it = m->free_list, *tmp; it; it = tmp) {
      tmp = it->next;
      free(it);
   }
   m->free_list = NULL;

   int freefrags = 0, freelines = 0;
   free_list_t **tail = &(m->free_list);
   for (size_t line = 0; line < m->maxlines;) {
      const size_t clear = mask_count_clear(&(state.markmask), line);
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

   start_world();

   if (opt_get_verbose(OPT_GC_VERBOSE, NULL)) {
      const int ticks = get_timestamp_us() - start_ticks;
      debugf("GC: allocated %zd/%zu; fragmentation %.2g%% [%d us]",
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

   ptrdiff_t line = ((char *)ptr - m->space) / LINE_SIZE;

   // Scan backwards to the start of the object
   line = mask_scan_backwards(&(m->headmask), line);
   assert(line != -1);

   size_t objlen = 1;
   if (line + 1 < m->maxlines)
      objlen += mask_count_clear(&(m->headmask), line + 1);

   *size = objlen * LINE_SIZE;
   return m->space + line * LINE_SIZE;
}
