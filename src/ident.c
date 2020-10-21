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
#include "fbuf.h"
#include "ident.h"

#include <assert.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <ctype.h>

#define MAP_DEPTH 3

typedef struct clist clist_t;
typedef struct trie  trie_t;

struct clist {
   char     value;
   trie_t  *down;
   clist_t *left;
   clist_t *right;
};

struct trie {
   char      value;
   uint16_t  write_gen;
   uint16_t  depth;
   uint32_t  write_index;
   trie_t   *up;
   clist_t  *list;
   trie_t   *map[0];
};

struct ident_rd_ctx {
   fbuf_t  *file;
   size_t   cache_sz;
   size_t   cache_alloc;
   ident_t *cache;
};

struct ident_wr_ctx {
   fbuf_t   *file;
   uint32_t  next_index;
   uint16_t  generation;
};

typedef struct {
   trie_t  trie;
   trie_t *map[256];
} root_t;

static root_t root = {
   {
      .value       = '\0',
      .write_gen   = 0,
      .write_index = 0,
      .depth       = 1,
      .up          = NULL
   }
};

static trie_t *alloc_node(char ch, trie_t *prev)
{
   const size_t mapsz = (prev->depth < MAP_DEPTH) ? 256 * sizeof(trie_t *) : 0;

   trie_t *t = xmalloc(sizeof(trie_t) + mapsz);
   t->value     = ch;
   t->depth     = prev->depth + 1;
   t->up        = prev;
   t->write_gen = 0;
   t->list      = NULL;

   if (mapsz > 0)
      memset(t->map, '\0', mapsz);

   if (prev->depth <= MAP_DEPTH)
      prev->map[(unsigned char)ch] = t;
   else {
      clist_t *c = xmalloc(sizeof(clist_t));
      c->value    = ch;
      c->down     = t;
      c->left     = NULL;
      c->right    = NULL;

      clist_t *it, **where;
      for (it = prev->list, where = &(prev->list);
           it != NULL;
           where = (ch < it->value ? &(it->left) : &(it->right)),
              it = *where)
         ;

      *where = c;
   }

   return t;
}

static void build_trie(const char *str, trie_t *prev, trie_t **end)
{
   assert(*str != '\0');
   assert(prev != NULL);

   trie_t *t = alloc_node(*str, prev);

   if (*(++str) == '\0')
      *end = t;
   else
      build_trie(str, t, end);
}

static clist_t *search_node(trie_t *t, char ch)
{
   clist_t *it;
   for (it = t->list;
        (it != NULL) && (it->value != ch);
        it = (ch < it->value ? it->left : it->right))
      ;

   return it;
}

static bool search_trie(const char **str, trie_t *t, trie_t **end)
{
   assert(**str != '\0');
   assert(t != NULL);

   trie_t *next = NULL;

   if (t->depth <= MAP_DEPTH)
      next = t->map[(unsigned char)**str];
   else {
      clist_t *it = search_node(t, **str);
      next = (it != NULL) ? it->down : NULL;
   }

   if (next == NULL) {
      *end = t;
      return false;
   }
   else {
      (*str)++;

      if (**str == '\0') {
         *end = next;
         return true;
      }
      else
         return search_trie(str, next, end);
   }
}

ident_t ident_new(const char *str)
{
   assert(str != NULL);
   assert(*str != '\0');

   trie_t *result;
   if (!search_trie(&str, &(root.trie), &result))
      build_trie(str, result, &result);

   return result;
}

bool ident_interned(const char *str)
{
   assert(str != NULL);
   assert(*str != '\0');

   trie_t *result;
   return search_trie(&str, &(root.trie), &result);
}

const char *istr(ident_t ident)
{
   if (ident == NULL)
      return NULL;

   char *p = get_fmt_buf(ident->depth) + ident->depth - 1;
   *p = '\0';

   trie_t *it;
   for (it = ident; it->value != '\0'; it = it->up)
      *(--p) = it->value;

   return p;
}

ident_wr_ctx_t ident_write_begin(fbuf_t *f)
{
   static uint16_t ident_wr_gen = 1;
   assert(ident_wr_gen > 0);

   struct ident_wr_ctx *ctx = xmalloc(sizeof(struct ident_wr_ctx));
   ctx->file       = f;
   ctx->generation = ident_wr_gen++;
   ctx->next_index = 0;

   return ctx;
}

void ident_write_end(ident_wr_ctx_t ctx)
{
   free(ctx);
}

void ident_write(ident_t ident, ident_wr_ctx_t ctx)
{
   if (ident == NULL) {
      write_u32(UINT32_MAX, ctx->file);
      write_u8(0, ctx->file);
   }
   else if (ident->write_gen == ctx->generation)
      write_u32(ident->write_index, ctx->file);
   else {
      write_u32(UINT32_MAX, ctx->file);
      write_raw(istr(ident), ident->depth, ctx->file);

      ident->write_gen   = ctx->generation;
      ident->write_index = ctx->next_index++;

      assert(ctx->next_index != UINT32_MAX);
   }
}

ident_rd_ctx_t ident_read_begin(fbuf_t *f)
{
   struct ident_rd_ctx *ctx = xmalloc(sizeof(struct ident_rd_ctx));
   ctx->file        = f;
   ctx->cache_alloc = 256;
   ctx->cache_sz    = 0;
   ctx->cache       = xmalloc(ctx->cache_alloc * sizeof(ident_t));

   return ctx;
}

void ident_read_end(ident_rd_ctx_t ctx)
{
   free(ctx->cache);
   free(ctx);
}

ident_t ident_read(ident_rd_ctx_t ctx)
{
   const uint32_t index = read_u32(ctx->file);
   if (index == UINT32_MAX) {
      if (ctx->cache_sz == ctx->cache_alloc) {
         ctx->cache_alloc *= 2;
         ctx->cache = xrealloc(ctx->cache, ctx->cache_alloc * sizeof(ident_t));
      }

      trie_t *p = &(root.trie);
      char ch;
      while ((ch = read_u8(ctx->file)) != '\0') {
         trie_t *next = NULL;
         if (p->depth <= MAP_DEPTH)
            next = p->map[(unsigned char)ch];
         else {
            clist_t *it = search_node(p, ch);
            next = (it != NULL) ? it->down : NULL;
         }

         if (next != NULL)
            p = next;
         else
            p = alloc_node(ch, p);
      }

      if (p == &(root.trie))
         return NULL;
      else {
         ctx->cache[ctx->cache_sz++] = p;
         return p;
      }
   }
   else if (likely(index < ctx->cache_sz))
      return ctx->cache[index];
   else
      fatal("ident index in %s is corrupt: index=%d cache_sz=%d",
            fbuf_file_name(ctx->file), index, (int)ctx->cache_sz);
}

ident_t ident_uniq(const char *prefix)
{
   static int counter = 0;

   const char *start = prefix;
   trie_t *end;
   if (search_trie(&start, &(root.trie), &end)) {
      const size_t len = strlen(prefix) + 16;
      char buf[len];
      snprintf(buf, len, "%s%d", prefix, counter++);

      return ident_new(buf);
   }
   else {
      trie_t *result;
      build_trie(start, end, &result);
      return result;
   }
}

ident_t ident_prefix(ident_t a, ident_t b, char sep)
{
   if (a == NULL)
      return b;
   else if (b == NULL)
      return a;

   trie_t *result;

   if (sep != '\0') {
      // Append separator
      const char sep_str[] = { sep, '\0' };
      const char *p_sep_str = sep_str;
      if (!search_trie(&p_sep_str, a, &result))
         build_trie(p_sep_str, result, &result);
   }
   else
      result = a;

   // Append b
   const char *bstr = istr(b);
   if (!search_trie(&bstr, result, &result))
      build_trie(bstr, result, &result);

   return result;
}

ident_t ident_strip(ident_t a, ident_t b)
{
   assert(a != NULL);
   assert(b != NULL);

   while (a->value == b->value && b->value != '\0') {
      a = a->up;
      b = b->up;
   }

   return (b->value == '\0' ? a : NULL);
}

char ident_char(ident_t i, unsigned n)
{
   if (i == NULL)
      return '\0';
   else if (n == 0)
      return i->value;
   else
      return ident_char(i->up, n - 1);
}

size_t ident_len(ident_t i)
{
   if (i == NULL || i->value == '\0')
      return 0;
   else
      return ident_len(i->up) + 1;
}

ident_t ident_suffix_until(ident_t i, char c, ident_t shared, char escape)
{
   assert(i != NULL);

   bool escaping = false;
   ident_t r = i;
   while (i->value != '\0' && i->up != shared) {
      if (!escaping && i->value == c)
         r = i->up;
      else if (i->value == escape)
         escaping = !escaping;
      i = i->up;
   }

   return r;
}

ident_t ident_until(ident_t i, char c)
{
   return ident_suffix_until(i, c, NULL, '\0');
}

ident_t ident_runtil(ident_t i, char c)
{
   assert(i != NULL);

   for (ident_t r = i; r->value != '\0'; r = r->up) {
      if (r->value == c)
         return r->up;
   }

   return i;
}

ident_t ident_from(ident_t i, char c)
{
   assert(i != NULL);

   char buf[i->depth + 1];
   char *p = buf + i->depth;
   *p-- = '\0';

   char *from = NULL;
   while (i->value != '\0') {
      if (i->value == c)
         from = p + 1;
      *p-- = i->value;
      i = i->up;
   }

   return (from == NULL) ? NULL : ident_new(from);
}

ident_t ident_rfrom(ident_t i, char c)
{
   assert(i != NULL);

   char buf[i->depth + 1];
   char *p = buf + i->depth;
   *p-- = '\0';

   while (i->value != '\0') {
      if (i->value == c)
         return ident_new(p + 1);
      *p-- = i->value;
      i = i->up;
   }

   return NULL;
}

bool icmp(ident_t i, const char *s)
{
   if (i == NULL || s == NULL)
      return i == NULL && s == NULL;

   trie_t *result;
   if (!search_trie(&s, &(root.trie), &result))
      return false;
   else
      return result == i;
}

static bool ident_glob_walk(const trie_t *i, const char *g,
                            const char *const end)
{
   if (i->value == '\0')
      return (g < end);
   else if (g < end)
      return false;
   else if (*g == '*')
      return ident_glob_walk(i->up, g, end)
         || ident_glob_walk(i->up, g - 1, end);
   else if (i->value == *g)
      return ident_glob_walk(i->up, g - 1, end);
   else
      return false;
}

bool ident_glob(ident_t i, const char *glob, int length)
{
   assert(i != NULL);

   if (length < 0)
      length = strlen(glob);

   return ident_glob_walk(i, glob + length - 1, glob);
}

bool ident_contains(ident_t i, const char *search)
{
   assert(i != NULL);

   for (ident_t r = i; r->value != '\0'; r = r->up) {
      for (const char *p = search; *p != '\0'; p++) {
         if (r->value == *p)
            return true;
      }
   }

   return false;
}

ident_t ident_downcase(ident_t i)
{
   // TODO: this could be implemented more efficiently

   if (i == NULL)
      return NULL;

   char *p = get_fmt_buf(i->depth) + i->depth - 1;
   *p = '\0';

   trie_t *it;
   for (it = i; it->value != '\0'; it = it->up)
      *(--p) = tolower((int)it->value);

   return ident_new(p);
}

void ident_list_add(ident_list_t **list, ident_t i)
{
   ident_list_t *c = xmalloc(sizeof(ident_list_t));
   c->ident = i;
   c->next  = *list;

   *list = c;
}

void ident_list_push(ident_list_t **list, ident_t i)
{
   ident_list_t *c = xmalloc(sizeof(ident_list_t));
   c->ident = i;
   c->next  = NULL;

   if (*list == NULL)
      *list = c;
   else {
      ident_list_t *it;
      for (it = *list; it->next != NULL; it = it->next)
         ;
      it->next = c;
   }
}

void ident_list_free(ident_list_t *list)
{
   ident_list_t *it = list;
   while (it != NULL) {
      ident_list_t *next = it->next;
      free(it);
      it = next;
   }
}

void _ident_list_cleanup(ident_list_t **list)
{
   ident_list_free(*list);
   *list = NULL;
}

bool ident_list_find(const ident_list_t *list, ident_t i)
{
   for (; list != NULL; list = list->next) {
      if (list->ident == i)
         return true;
   }

   return false;
}
