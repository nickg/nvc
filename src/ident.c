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
   struct clist *left;
   struct clist *right;
};

struct trie {
   char         value;
   uint8_t      write_gen;
   uint16_t     write_index;
   uint16_t     depth;
   struct trie  *up;
   struct clist *children;
};

struct ident_rd_ctx {
   FILE    *file;
   size_t  n_idents;
   ident_t *idents;
};

struct ident_wr_ctx {
   FILE    *file;
   long    start_off;
   ident_t *pending;
   size_t  n_pending;
   size_t  pend_alloc;
   uint8_t generation;
};

static struct trie root = {
   .value       = '\0',
   .write_gen   = 0,
   .write_index = 0,
   .depth       = 1,
   .up          = NULL,
   .children    = NULL
};

static struct trie *alloc_node(char ch, struct trie *prev)
{
   struct trie *t = xmalloc(sizeof(struct trie));
   t->value     = ch;
   t->depth     = prev->depth + 1;
   t->up        = prev;
   t->children  = NULL;
   t->write_gen = 0;

   struct clist *c = xmalloc(sizeof(struct clist));
   c->value    = ch;
   c->down     = t;
   c->left     = NULL;
   c->right    = NULL;

   struct clist *it, **where;
   for (it = prev->children, where = &(prev->children);
        it != NULL;
        where = (ch < it->value ? &(it->left) : &(it->right)),
           it = *where)
      ;

   *where = c;

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
        (it != NULL) && (it->value != ch);
        it = (ch < it->value ? it->left : it->right))
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

   char *p = get_fmt_buf(ident->depth) + ident->depth - 1;
   *p = '\0';

   struct trie *it;
   for (it = ident; it->value != '\0'; it = it->up) {
      assert(it != NULL);
      *(--p) = it->value;
   }

   return p;
}

ident_wr_ctx_t ident_write_begin(FILE *f)
{
   static uint8_t ident_wr_gen = 1;
   assert(ident_wr_gen > 0);

   struct ident_wr_ctx *ctx = xmalloc(sizeof(struct ident_wr_ctx));
   ctx->file       = f;
   ctx->start_off  = ftell(f);
   ctx->pend_alloc = 512;
   ctx->pending    = xmalloc(sizeof(ident_t) * ctx->pend_alloc);
   ctx->n_pending  = 0;
   ctx->generation = ident_wr_gen++;

   // Write a placeholder that will later be overwritten with the
   // offset of the identifier table
   write_u32(UINT32_MAX, ctx->file);

   return ctx;
}

void ident_write_end(ident_wr_ctx_t ctx)
{
   long table_off = ftell(ctx->file);

   write_u16(ctx->n_pending, ctx->file);

   for (size_t i = 0; i < ctx->n_pending; i++) {
      ident_t ident = ctx->pending[i];
      if (fwrite(istr(ident), ident->depth, 1, ctx->file) != 1)
         fatal("fwrite failed");
   }

   fseek(ctx->file, ctx->start_off, SEEK_SET);
   write_u32(table_off, ctx->file);

   free(ctx->pending);
   free(ctx);
}

void ident_write(ident_t ident, ident_wr_ctx_t ctx)
{
   assert(ident != NULL);

   uint16_t index;
   if (ident->write_gen == ctx->generation)
      index = ident->write_index;
   else {
      index = ctx->n_pending;

      if (ctx->n_pending == ctx->pend_alloc) {
         ctx->pend_alloc *= 2;
         ctx->pending = xrealloc(
            ctx->pending, sizeof(ident_t) * ctx->pend_alloc);
      }

      ctx->pending[ctx->n_pending++] = ident;
      assert(ctx->n_pending != UINT16_MAX);

      ident->write_gen   = ctx->generation;
      ident->write_index = index;
   }

   write_u16(index, ctx->file);
}

ident_rd_ctx_t ident_read_begin(FILE *f)
{
   long ident_off = read_u32(f);
   long save_off = ftell(f);

   fseek(f, ident_off, SEEK_SET);

   struct ident_rd_ctx *ctx = xmalloc(sizeof(struct ident_rd_ctx));
   ctx->file     = f;
   ctx->n_idents = read_u16(ctx->file);
   ctx->idents   = xmalloc(sizeof(ident_t) * ctx->n_idents);

   for (size_t i = 0; i < ctx->n_idents; i++) {
      struct trie *p = &root;
      char ch;
      while ((ch = fgetc(ctx->file)) != '\0') {
         struct clist *it = search_node(p, ch);
         if (it != NULL)
            p = it->down;
         else
            p = alloc_node(ch, p);
      }
      ctx->idents[i] = p;
   }

   fseek(ctx->file, save_off, SEEK_SET);
   return ctx;
}

void ident_read_end(ident_rd_ctx_t ctx)
{
   free(ctx->idents);
   free(ctx);
}

ident_t ident_read(ident_rd_ctx_t ctx)
{
   uint16_t index = read_u16(ctx->file);
   assert(index < ctx->n_idents);
   return ctx->idents[index];
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

ident_t ident_runtil(ident_t i, char c)
{
   assert(i != NULL);

   ident_t r = i;
   while (i->value != '\0') {
      if (i->value == c)
         return i->up;
      i = i->up;
   }

   return r;
}

bool icmp(ident_t i, const char *s)
{
   assert(i != NULL);

   struct trie *result;
   if (!search_trie(&s, &root, &result))
      return false;
   else
      return result == i;
}
