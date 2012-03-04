#include "ident.h"
#include "util.h"

#include <assert.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>

struct clist {
   char         value;
   struct trie  *down;
   struct clist *next;
};

struct trie {
   char         value;
   unsigned     depth;
   struct trie  *up;
   struct clist *children;
};

struct ident_rd_ctx {
   FILE *file;
};

struct ident_wr_ctx {
   FILE *file;
};

static struct trie root = {
   .value    = '\0',
   .depth    = 1,
   .up       = NULL,
   .children = NULL
};

static struct trie *alloc_node(char ch, struct trie *prev)
{
   struct trie *t = xmalloc(sizeof(struct trie));
   t->value    = ch;
   t->depth    = prev->depth + 1;
   t->up       = prev;
   t->children = NULL;

   struct clist *c = xmalloc(sizeof(struct clist));
   c->value    = ch;
   c->down     = t;
   c->next     = prev->children;

   prev->children = c;

   return t;
}

static void build_trie(const char *str, struct trie *prev, struct trie **end)
{
   assert(*str != '\0');
   assert(prev != NULL);

   struct trie *t = alloc_node(*str, prev);

   if (*(++str) == '\0')
      *end = t;
   else
      build_trie(str, t, end);
}

static struct clist *search_node(struct trie *t, char ch)
{
   struct clist *it;
   for (it = t->children;
        it != NULL && it->value != ch;
        it = it->next)
      ;
   return it;
}

static bool search_trie(const char **str, struct trie *t, struct trie **end)
{
   assert(**str != '\0');
   assert(t != NULL);

   struct clist *it = search_node(t, **str);

   if (it == NULL) {
      *end = t;
      return false;
   }
   else {
      (*str)++;

      if (**str == '\0') {
         *end = it->down;
         return true;
      }
      else
         return search_trie(str, it->down, end);
   }
}

ident_t ident_new(const char *str)
{
   assert(str != NULL);
   assert(*str != '\0');

   struct trie *result;
   if (!search_trie(&str, &root, &result))
      build_trie(str, result, &result);

   return result;
}

const char *istr(ident_t ident)
{
   assert(ident != NULL);

   // This is a bit of a kludge but keeping a sufficient number
   // of static buffers allows us to use istr multiple times in printf
   static char   *buf_set[ISTR_MAX_BUFS];
   static size_t buflen[ISTR_MAX_BUFS];
   static int    next_buf = 0;

   char **bufp = &buf_set[next_buf];
   size_t *blenp = &buflen[next_buf];
   next_buf = (next_buf + 1) % ISTR_MAX_BUFS;

   if (*bufp == NULL) {
      *bufp = xmalloc(ident->depth);
      *blenp = ident->depth;
   }

   while (ident->depth > *blenp) {
      *blenp *= 2;
      *bufp = xrealloc(*bufp, *blenp);
   }

   char *p = *bufp + ident->depth - 1;
   *p = '\0';

   struct trie *it;
   for (it = ident; it->value != '\0'; it = it->up) {
      assert(it != NULL);
      *(--p) = it->value;
   }
   assert(p == *bufp);

   return *bufp;
}

ident_wr_ctx_t ident_write_begin(FILE *f)
{
   struct ident_wr_ctx *ctx = xmalloc(sizeof(struct ident_wr_ctx));
   ctx->file = f;
   return ctx;
}

void ident_write_end(ident_wr_ctx_t ctx)
{
   free(ctx);
}

void ident_write(ident_t ident, ident_wr_ctx_t ctx)
{
   assert(ident != NULL);

   if (fwrite(istr(ident), ident->depth, 1, ctx->file) != 1)
      fatal("fwrite failed");
}

ident_rd_ctx_t ident_read_begin(FILE *f)
{
   struct ident_rd_ctx *ctx = xmalloc(sizeof(struct ident_rd_ctx));
   ctx->file = f;
   return ctx;
}

void ident_read_end(ident_rd_ctx_t ctx)
{
   free(ctx);
}

ident_t ident_read(ident_rd_ctx_t ctx)
{
   struct trie *p = &root;
   char ch;
   while ((ch = fgetc(ctx->file)) != '\0') {
      struct clist *it = search_node(p, ch);
      if (it != NULL)
         p = it->down;
      else
         p = alloc_node(ch, p);
   }

   return p;
}

ident_t ident_uniq(const char *prefix)
{
   static int counter = 0;

   const char *start = prefix;
   struct trie *end;
   if (search_trie(&start, &root, &end)) {
      const size_t len = strlen(prefix) + 16;
      char buf[len];
      snprintf(buf, len, "%s%d", prefix, counter++);

      return ident_new(buf);
   }
   else {
      struct trie *result;
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

   struct trie *result;

   // Append separator
   const char sep_str[] = { sep, '\0' };
   const char *p_sep_str = sep_str;
   if (!search_trie(&p_sep_str, a, &result))
      build_trie(p_sep_str, result, &result);

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
   assert(i != NULL);

   if (n == 0)
      return i->value;
   else
      return ident_char(i->up, n - 1);
}

ident_t ident_until(ident_t i, char c)
{
   assert(i != NULL);

   ident_t r = i;
   while (i->value != '\0') {
      if (i->value == c)
         r = i->up;
      i = i->up;
   }

   return r;
}
