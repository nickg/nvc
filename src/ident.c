//
//  Copyright (C) 2011-2020  Nick Gasson
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

#define HASH_INIT 5381;
typedef unsigned long hash_state_t;

typedef A(char) char_array_t;

struct ident_rd_ctx {
   fbuf_t       *file;
   size_t        cache_sz;
   size_t        cache_alloc;
   ident_t      *cache;
   char_array_t  scratch;
};

struct ident_wr_ctx {
   fbuf_t   *file;
   uint32_t  next_index;
   uint16_t  generation;
};

struct _ident {
   ident_t  chain;
   uint32_t write_index;
   uint16_t write_gen;
   uint16_t length;
   char     bytes[0];
};

#define TABLE_SZ 1024
static ident_t table[TABLE_SZ];

static inline int hash_update(hash_state_t *state, const char *key, int nchars)
{
   // DJB2 hash function from here:
   //   http://www.cse.yorku.ca/~oz/hash.html

   hash_state_t hash = *state;
   const char *p = key;
   for (; p < key + nchars && *p; p++)
      hash = ((hash << 5) + hash) + *p;
   *state = hash;
   return p - key;
}

static bool ident_install(ident_t *where, ident_t new, int len)
{
   if (unlikely(len >= UINT16_MAX))
      fatal("identifier '%s' too long", new->bytes);

   new->length      = len;
   new->chain       = NULL;
   new->write_gen   = 0;
   new->write_index = 0;

   if (atomic_cas(where, NULL, new))
      return true;
   else {
      free(new);
      return false;
   }
}

static ident_t ident_from_bytes(const char *str, hash_state_t hash, int len)
{
   const int slot = hash & (TABLE_SZ - 1);
   for (;;) {
      ident_t *ptr = &(table[slot]);
      for (ident_t it; (it = load_acquire(ptr)); ptr = &(it->chain)) {
         if (it->length == len && memcmp(it->bytes, str, len) == 0)
            return it;
      }

      ident_t new = xmalloc_flex(sizeof(struct _ident), len + 1, sizeof(char));
      memcpy(new->bytes, str, len);
      new->bytes[len] = '\0';

      if (ident_install(ptr, new, len))
         return new;
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
      fbuf_put_uint(ctx->file, 1);
   else if (ident->write_gen == ctx->generation)
      fbuf_put_uint(ctx->file, ident->write_index + 1);
   else {
      fbuf_put_uint(ctx->file, 0);

      assert(ident->bytes[ident->length] == '\0');
      write_raw(ident->bytes, ident->length + 1, ctx->file);

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

   // First index is implicit null
   ctx->cache[ctx->cache_sz++] = NULL;

   return ctx;
}

void ident_read_end(ident_rd_ctx_t ctx)
{
   ACLEAR(ctx->scratch);
   free(ctx->cache);
   free(ctx);
}

ident_t ident_read(ident_rd_ctx_t ctx)
{
   const uint32_t index = fbuf_get_uint(ctx->file);
   if (index == 0) {
      if (ctx->cache_sz == ctx->cache_alloc) {
         ctx->cache_alloc *= 2;
         ctx->cache = xrealloc(ctx->cache, ctx->cache_alloc * sizeof(ident_t));
      }

      ARESIZE(ctx->scratch, 0);
      char ch;
      do {
         ch = read_u8(ctx->file);
         APUSH(ctx->scratch, ch);
      } while (ch);

      return (ctx->cache[ctx->cache_sz++] = ident_new(ctx->scratch.items));
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

      const int slot = hash & (TABLE_SZ - 1);
      for (;;) {
         ident_t *ptr = &(table[slot]);
         for (ident_t it; (it = load_acquire(ptr)); ptr = &(it->chain)) {
            if (it->length == len + sufflen
                && memcmp(it->bytes, prefix, len) == 0
                && memcmp(it->bytes + len, suffix, sufflen) == 0)
               goto exist;
         }

         ident_t new = xmalloc_flex(sizeof(struct _ident), len + sufflen + 1,
                                    sizeof(char));
         memcpy(new->bytes, prefix, len);
         memcpy(new->bytes + len, suffix, sufflen);
         new->bytes[len + sufflen] = '\0';

         if (ident_install(ptr, new, len + sufflen))
            return new;
      }

   exist:
      checked_sprintf(suffix, sizeof(suffix), "%d", relaxed_add(&counter, 1));
   }
}

ident_t ident_prefix(ident_t a, ident_t b, char sep)
{
   if (a == NULL)
      return b;
   else if (b == NULL)
      return a;

   hash_state_t hash = HASH_INIT;
   hash_update(&hash, a->bytes, a->length);
   hash_update(&hash, &sep, 1);
   hash_update(&hash, b->bytes, b->length);

   const int len = a->length + b->length + (sep != '\0');
   const int slot = hash & (TABLE_SZ - 1);

   for (;;) {
      ident_t *ptr = &(table[slot]);
      for (ident_t it; (it = load_acquire(ptr)); ptr = &(it->chain)) {
         if (it->length == len
             && memcmp(it->bytes, a->bytes, a->length) == 0
             && (sep == '\0' || it->bytes[a->length] == sep)
             && memcmp(it->bytes + a->length + (sep != '\0'),
                       b->bytes, b->length) == 0)
            return it;
      }

      ident_t new = xmalloc_flex(sizeof(struct _ident), len + 1, sizeof(char));
      memcpy(new->bytes, a->bytes, a->length);
      if (sep != '\0') new->bytes[a->length] = sep;
      memcpy(new->bytes + a->length + (sep != '\0'), b->bytes, b->length + 1);

      if (ident_install(ptr, new, len))
         return new;
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
      buf[pos] = tolower((int)i->bytes[pos]);
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
