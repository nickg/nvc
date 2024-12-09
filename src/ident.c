//
//  Copyright (C) 2011-2023  Nick Gasson
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
#include "fbuf.h"
#include "ident.h"
#include "thread.h"

#include <assert.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <ctype.h>
#include <limits.h>

#define INITIAL_SIZE  1024
#define REPROBE_LIMIT 20
#define MOVED_TAG     1

struct ident_rd_ctx {
   fbuf_t  *file;
   size_t   cache_sz;
   size_t   cache_alloc;
   ident_t *cache;
   char    *scratch;
   size_t   scratch_sz;
   bool     new_format;   // Added in 1.13
};

struct ident_wr_ctx {
   fbuf_t   *file;
   uint32_t  next_index;
   uint16_t  generation;
};

struct _ident {
   hash_state_t hash;
   uint32_t     write_index;
   uint16_t     write_gen;
   uint16_t     length;
   char         bytes[0];
};

typedef struct {
   size_t  size;
   ident_t slots[0];
} ident_tab_t;

static ident_tab_t *table = NULL;
static ident_tab_t *resizing = NULL;

static ident_t ident_alloc(size_t len, hash_state_t hash)
{
   ident_t id = xmalloc_flex(sizeof(struct _ident), len + 1, sizeof(char));
   id->length      = len;
   id->write_gen   = 0;
   id->write_index = 0;
   id->hash        = hash;
   id->bytes[len]  = '\0';

   return id;
}

static inline bool ident_install(ident_t *where, ident_t new, size_t len)
{
   if (unlikely(len >= UINT16_MAX))
      fatal("identifier '%s' too long", new->bytes);

   if (atomic_cas(where, NULL, new))
      return true;
   else {
      free(new);
      return false;
   }
}

static void copy_table(ident_tab_t *from, ident_tab_t *to)
{
   for (int i = 0; i < from->size; i++) {
      for (;;) {
         ident_t id = load_acquire(&(from->slots[i]));
         assert(pointer_tag(id) != MOVED_TAG);

         if (id != NULL) {
            for (int slot = id->hash & (to->size - 1), i = 1;;
                 slot = (slot + i) & (to->size - 1), i++) {
               ident_t exist = load_acquire(&(to->slots[slot]));
               if (exist == NULL) {
                  store_release(&(to->slots[slot]), id);
                  break;
               }
               else
                  assert(exist != id);
            }
         }

         if (atomic_cas(&(from->slots[i]), id, tag_pointer(id, MOVED_TAG)))
            break;
      }
   }
}

static void wait_for_resize(void)
{
   for (;;) {
      ident_tab_t *from = atomic_load(&table);
      ident_tab_t *to = atomic_load(&resizing);

      if (from == to)
         break;

      // We could help to resize the table here but the extra complexity
      // doesn't seem worth it
      thread_sleep(10);
   }
}

static void finish_resizing(ident_tab_t *old, ident_tab_t *new)
{
   assert(atomic_load(&resizing) == new);

   if (!atomic_cas(&table, old, new))
      fatal_trace("bug in concurrent resize protocol");

   async_free(old);
}

static ident_tab_t *get_table(void)
{
   ident_tab_t *cur = atomic_load(&table);
   if (unlikely(cur == NULL)) {
      ident_tab_t *newtab =
         xcalloc_flex(sizeof(ident_tab_t), INITIAL_SIZE, sizeof(ident_t));
      newtab->size = INITIAL_SIZE;

      if (atomic_cas(&resizing, NULL, newtab)) {
         finish_resizing(NULL, newtab);
         return newtab;
      }
      else {
         free(newtab);
         wait_for_resize();
         return atomic_load(&table);
      }
   }
   else
      return cur;
}

static void grow_table(ident_tab_t *cur)
{
   ident_tab_t *newtab =
      xcalloc_flex(sizeof(ident_tab_t), cur->size * 2, sizeof(ident_t));
   newtab->size = cur->size * 2;

   if (atomic_cas(&resizing, cur, newtab)) {
      copy_table(cur, newtab);
      finish_resizing(cur, newtab);
   }
   else {
      free(newtab);
      wait_for_resize();
   }
}

static ident_t ident_from_bytes(const char *str, hash_state_t hash, size_t len)
{
   hash = mix_bits_32(hash);

   for (;;) {
      ident_tab_t *cur = get_table();

      for (int reprobe = 0, i = 1, slot = hash & (cur->size - 1);;
           slot = (slot + i) & (cur->size - 1), i++) {
         ident_t *ptr = &(cur->slots[slot]);

         ident_t id = load_acquire(ptr);
         if (pointer_tag(id) == MOVED_TAG) {
            wait_for_resize();
            break;
         }
         else if (id == NULL) {
            ident_t new = ident_alloc(len, hash);
            memcpy(new->bytes, str, len);

            if (ident_install(ptr, new, len))
               return new;
            else
               break;
         }
         else if (id->length == len && memcmp(id->bytes, str, len) == 0)
            return id;

         if (++reprobe == REPROBE_LIMIT) {
            grow_table(cur);
            break;
         }
      }
   }
}

static ident_t ident_from_byte_vec(hash_state_t hash, bool uniq, int nparts,
                                   const char *str_vec[nparts],
                                   const size_t len_vec[nparts])
{
   hash = mix_bits_32(hash);

   size_t len = 0;
   for (int i = 0; i < nparts; i++)
      len += len_vec[i];

   for (;;) {
      ident_tab_t *cur = get_table();

      for (int reprobe = 0, i = 1, slot = hash & (cur->size - 1);;
           slot = (slot + i) & (cur->size - 1), i++) {
         ident_t *ptr = &(cur->slots[slot]);

         ident_t id = load_acquire(ptr);
         if (pointer_tag(id) == MOVED_TAG) {
            wait_for_resize();
            break;
         }
         else if (id == NULL) {
            ident_t new = ident_alloc(len, hash);
            char *p = new->bytes;
            for (int i = 0; i < nparts; p += len_vec[i++])
               memcpy(p, str_vec[i], len_vec[i]);

            if (ident_install(ptr, new, len))
               return new;
            else
               break;
         }
         else if (id->length == len) {
            int pos = 0;
            for (const char *p = id->bytes;
                 pos < nparts && memcmp(p, str_vec[pos], len_vec[pos]) == 0;
                 p += len_vec[pos++]);

            if (pos == nparts)
               return uniq ? NULL : id;
         }

         if (++reprobe == REPROBE_LIMIT) {
            grow_table(cur);
            break;
         }
      }
   }
}

ident_t ident_new(const char *str)
{
   assert(str != NULL);
   assert(*str != '\0');

   hash_state_t hash = HASH_INIT;
   int len = hash_update(&hash, str, INT_MAX);

   return ident_from_bytes(str, hash, len);
}

ident_t ident_new_n(const char *str, size_t len)
{
   assert(str != NULL);
   assert(len > 0);

   hash_state_t hash = HASH_INIT;
   hash_update(&hash, str, len);

   return ident_from_bytes(str, hash, len);
}

const char *istr(ident_t ident)
{
   return ident ? ident->bytes : NULL;
}

ident_wr_ctx_t ident_write_begin(fbuf_t *f)
{
   static uint16_t ident_wr_gen = 1;
   assert(ident_wr_gen > 0);

   struct ident_wr_ctx *ctx = xcalloc(sizeof(struct ident_wr_ctx));
   ctx->file         = f;
   ctx->generation   = ident_wr_gen++;
   ctx->next_index   = 1;   // Skip over null ident

   return ctx;
}

void ident_write_end(ident_wr_ctx_t ctx)
{
   free(ctx);
}

void ident_write(ident_t ident, ident_wr_ctx_t ctx)
{
   if (ident == NULL)
      fbuf_put_int(ctx->file, 1);   // TODO: change this to zero
   else if (ident->write_gen == ctx->generation)
      fbuf_put_int(ctx->file, ident->write_index + 1);
   else {
      fbuf_put_int(ctx->file, -ident->length);

      assert(ident->bytes[ident->length] == '\0');
      write_raw(ident->bytes, ident->length, ctx->file);

      ident->write_gen   = ctx->generation;
      ident->write_index = ctx->next_index++;

      assert(ctx->next_index != UINT32_MAX);
   }
}

ident_rd_ctx_t ident_read_begin(fbuf_t *f)
{
   struct ident_rd_ctx *ctx = xcalloc(sizeof(struct ident_rd_ctx));
   ctx->file        = f;
   ctx->cache_alloc = 256;
   ctx->cache_sz    = 0;
   ctx->cache       = xmalloc_array(ctx->cache_alloc, sizeof(ident_t));
   ctx->scratch_sz  = 128;
   ctx->scratch     = xmalloc(ctx->scratch_sz);
   ctx->new_format  = true;

   // First index is implicit null
   ctx->cache[ctx->cache_sz++] = NULL;

   return ctx;
}

void ident_read_end(ident_rd_ctx_t ctx)
{
   free(ctx->scratch);
   free(ctx->cache);
   free(ctx);
}

ident_t ident_read(ident_rd_ctx_t ctx)
{
   const int32_t index =
      ctx->new_format ? fbuf_get_int(ctx->file) : fbuf_get_uint(ctx->file);
   if (index < 0) {
      if (unlikely(-index + 1 > ctx->scratch_sz)) {
         ctx->scratch_sz = MAX(-index + 1, (ctx->scratch_sz * 3) / 2);
         ctx->scratch = xrealloc(ctx->scratch, ctx->scratch_sz);
      }

      read_raw(ctx->scratch, -index, ctx->file);
      ctx->scratch[-index] = '\0';

      if (unlikely(ctx->cache_sz == ctx->cache_alloc)) {
         ctx->cache_alloc *= 2;
         ctx->cache = xrealloc(ctx->cache, ctx->cache_alloc * sizeof(ident_t));
      }

      return (ctx->cache[ctx->cache_sz++] = ident_new(ctx->scratch));
   }
   else if (index == 0) {
      // Format prior to 1.13
      ctx->new_format = false;

      int num = 0;
      char ch;
      do {
         if (num == ctx->scratch_sz) {
            ctx->scratch_sz = (ctx->scratch_sz * 3) / 2;
            ctx->scratch = xrealloc(ctx->scratch, ctx->scratch_sz);
         }

         ch = ctx->scratch[num++] = read_u8(ctx->file);
      } while (ch);

      if (unlikely(ctx->cache_sz == ctx->cache_alloc)) {
         ctx->cache_alloc *= 2;
         ctx->cache = xrealloc(ctx->cache, ctx->cache_alloc * sizeof(ident_t));
      }

      return (ctx->cache[ctx->cache_sz++] = ident_new(ctx->scratch));
   }
   else if (likely(index - 1 < ctx->cache_sz))
      return ctx->cache[index - 1];
   else
      fatal("ident index in %s is corrupt: index=%d cache_sz=%d",
            fbuf_file_name(ctx->file), index, (int)ctx->cache_sz);
}

ident_t ident_uniq(const char *prefix)
{
   hash_state_t base_hash = HASH_INIT;
   int len = hash_update(&base_hash, prefix, INT_MAX);

   static volatile int counter = 0;
   char suffix[16] = "";
   for (;;) {
      hash_state_t hash = base_hash;
      int sufflen = hash_update(&hash, suffix, ARRAY_LEN(suffix));

      const char *str_vec[] = { prefix, suffix };
      const size_t len_vec[] = { len, sufflen };

      ident_t new = ident_from_byte_vec(hash, true, 2, str_vec, len_vec);
      if (new != NULL)
         return new;

      checked_sprintf(suffix, sizeof(suffix), "%d", relaxed_add(&counter, 1));
   }
}

ident_t ident_sprintf(const char *fmt, ...)
{
   va_list ap, ap2;
   va_start(ap, fmt);
   va_copy(ap2, ap);

   static char buf[64];
   size_t req = vsnprintf(buf, sizeof(buf), fmt, ap);

   ident_t result;
   if (req + 1 > sizeof(buf)) {
      char *buf2 = xmalloc(req + 1);
      vsnprintf(buf2, req + 1, fmt, ap2);
      result = ident_new(buf2);
      free(buf2);
   }
   else
      result = ident_new(buf);

   va_end(ap);
   va_end(ap2);

   return result;
}

ident_t ident_prefix(ident_t a, ident_t b, char sep)
{
   if (a == NULL)
      return b;
   else if (b == NULL)
      return a;
   else if (sep == '\0') {
      hash_state_t hash = HASH_INIT;
      hash_update(&hash, a->bytes, a->length);
      hash_update(&hash, b->bytes, b->length);

      const char *str_vec[] = { a->bytes, b->bytes };
      const size_t len_vec[] = { a->length, b->length };
      return ident_from_byte_vec(hash, false, 2, str_vec, len_vec);
   }
   else {
      hash_state_t hash = HASH_INIT;
      hash_update(&hash, a->bytes, a->length);
      hash_update(&hash, &sep, 1);
      hash_update(&hash, b->bytes, b->length);

      const char *str_vec[] = { a->bytes, &sep, b->bytes };
      const size_t len_vec[] = { a->length, 1, b->length };
      return ident_from_byte_vec(hash, false, 3, str_vec, len_vec);
   }
}

bool ident_starts_with(ident_t a, ident_t b)
{
   return b != NULL && (a == b || strncmp(a->bytes, b->bytes, b->length) == 0);
}

char ident_char(ident_t i, unsigned n)
{
   if (i == NULL)
      return '\0';
   else {
      assert(n < i->length);
      return i->bytes[n];
   }
}

int ident_pos(ident_t i, char ch)
{
   assert(i != NULL);

   for (int pos = 0; pos < i->length; pos++) {
      if (i->bytes[pos] == ch)
         return pos;
   }

   return -1;
}

size_t ident_len(ident_t i)
{
   return i == NULL ? 0 : i->length;
}

ident_t ident_until(ident_t i, char c)
{
   assert(i != NULL);

   int pos = 0;
   for (; pos < i->length && i->bytes[pos] != c; pos++);

   if (pos == i->length) return i;

   hash_state_t hash = HASH_INIT;
   hash_update(&hash, i->bytes, pos);

   return ident_from_bytes(i->bytes, hash, pos);
}

ident_t ident_runtil(ident_t i, char c)
{
   assert(i != NULL);

   int pos = i->length;
   for (; pos >= 0 && i->bytes[pos] != c; pos--);

   if (pos < 0) return i;

   hash_state_t hash = HASH_INIT;
   hash_update(&hash, i->bytes, pos);

   return ident_from_bytes(i->bytes, hash, pos);
}

ident_t ident_from(ident_t i, char c)
{
   assert(i != NULL);

   int pos = 0;
   for (; pos < i->length && i->bytes[pos] != c; pos++);

   if (pos == i->length) return NULL;

   hash_state_t hash = HASH_INIT;
   hash_update(&hash, i->bytes + pos + 1, i->length - pos - 1);

   return ident_from_bytes(i->bytes + pos + 1, hash, i->length - pos - 1);
}

ident_t ident_rfrom(ident_t i, char c)
{
   assert(i != NULL);

   int pos = i->length;
   for (; pos >= 0 && i->bytes[pos] != c; pos--);

   if (pos < 0) return i;

   hash_state_t hash = HASH_INIT;
   hash_update(&hash, i->bytes + pos + 1, i->length - pos - 1);

   return ident_from_bytes(i->bytes + pos + 1, hash, i->length - pos - 1);
}

bool icmp(ident_t i, const char *s)
{
   if (i == NULL || s == NULL)
      return i == NULL && s == NULL;

   return strcmp(i->bytes, s) == 0;
}

int ident_compare(ident_t a, ident_t b)
{
   return strcmp(a->bytes, b->bytes);
}

static bool ident_glob_walk(const char *str, const char *g)
{
   if (*str == '\0')
      return *g == '\0';
   else if (*g == '\0')
      return false;
   else if (*g == '*')
      return ident_glob_walk(str + 1, g)
         || ident_glob_walk(str + 1, g + 1);
   else if (*str == *g)
      return ident_glob_walk(str + 1, g + 1);
   else
      return false;
}

bool ident_glob(ident_t i, const char *glob, int length)
{
   assert(i != NULL);

   return ident_glob_walk(i->bytes, glob);
}

ident_t ident_downcase(ident_t i)
{
   assert(i != NULL);

   char small[64], *big = NULL, *buf = small;
   if (i->length >= ARRAY_LEN(small))
      buf = big = xmalloc(i->length + 1);

   for (int pos = 0; pos < i->length; pos++)
      buf[pos] = tolower_iso88591(i->bytes[pos]);
   buf[i->length] = '\0';

   hash_state_t hash = HASH_INIT;
   hash_update(&hash, buf, i->length);

   ident_t result = ident_from_bytes(buf, hash, i->length);
   if (big != NULL) free(big);
   return result;
}

ident_t ident_walk_selected(ident_t *it)
{
   if (*it == NULL)
      return NULL;

   ident_t i = *it;
   char escape = '\0';
   int pos = 0;
   for (; pos < i->length; pos++) {
      if (i->bytes[pos] == '.' && escape == '\0')
         break;
      else if (i->bytes[pos] == '\'' && escape != '\\')
         escape = escape == '\'' ? '\0' : '\'';
      else if (i->bytes[pos] == '\\' && escape != '\'')
         escape = escape == '\\' ? '\0' : '\\';
   }

   if (pos == i->length) {
      *it = NULL;
      return i;
   }

   hash_state_t hash = HASH_INIT;
   hash_update(&hash, i->bytes + pos + 1, i->length - pos - 1);

   *it = ident_from_bytes(i->bytes + pos + 1, hash, i->length - pos - 1);

   hash = HASH_INIT;
   hash_update(&hash, i->bytes, pos);

   return ident_from_bytes(i->bytes, hash, pos);
}

int ident_distance(ident_t a, ident_t b)
{
   const int n = b->length;
   const int m = a->length;

   const char *s = a->bytes, *t = b->bytes;

   int mem[2 * (n + 1)], *v0 = mem, *v1 = mem + n + 1;

   for (int i = 0; i <= n; i++)
      v0[i] = i;

   for (int i = 0; i < m; i++) {
      v1[0] = i + 1;

      for (int j = 0; j < n; j++) {
         const int dc = v0[j + 1] + 1;
         const int ic = v1[j] + 1;
         const int sc = (s[i] == t[j] ? v0[j] : v0[j] + 1);

         v1[j + 1] = MIN(dc, MIN(ic, sc));
      }

      int *tmp = v0;
      v0 = v1;
      v1 = tmp;
   }

   return v0[n];
}
