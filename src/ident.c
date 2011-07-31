#include "ident.h"
#include "util.h"

#include <assert.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>

#define IDENT_MAX_LEN (1 << 16)

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

static struct trie root = {
   .value    = '\0',
   .depth    = 1,
   .up       = NULL,
   .children = NULL
};

static void build_trie(const char *str, struct trie *prev, struct trie **end)
{
   assert(*str != '\0');
   assert(prev != NULL);

   struct trie *t = xmalloc(sizeof(struct trie));
   t->value    = *str;
   t->depth    = prev->depth + 1;
   t->up       = prev;
   t->children = NULL;

   struct clist *c = xmalloc(sizeof(struct clist));
   c->value    = *str;
   c->down     = t;
   c->next     = prev->children;

   prev->children = c;
   
   if (*(++str) == '\0')
      *end = t;
   else
      build_trie(str, t, end);
}

static bool search_trie(const char **str, struct trie *t, struct trie **end)
{
   assert(**str != '\0');
   assert(t != NULL);

   struct clist *it;
   for (it = t->children;
        it != NULL && it->value != **str;
        it = it->next)
      ;

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

void ident_write(ident_t ident, FILE *f)
{
   assert(ident != NULL);

   fwrite(&ident->depth, sizeof(unsigned), 1, f);
   fwrite(istr(ident), ident->depth, 1, f);
}

ident_t ident_read(FILE *f)
{
   unsigned len;
   if (fread(&len, sizeof(unsigned), 1, f) != 1)
      fatal("failed to read identifier length from file");

   if (len > IDENT_MAX_LEN)
      fatal("identifier too long %u", len);

   char *buf = xmalloc(len);
   if (fread(buf, len, 1, f) != 1)
      fatal("failed to read identifier data from file");
   ident_t i = ident_new(buf);
   assert(i->depth == len);
   free(buf);

   return i;
}

ident_t ident_uniq(const char *prefix)
{
   static int counter = 0;

   const size_t len = strlen(prefix) + 16;
   char buf[len];
   snprintf(buf, len, "%s%d", prefix, counter++);

   return ident_new(buf);
}

ident_t ident_prefix(ident_t a, ident_t b)
{
   assert(a != NULL);
   assert(b != NULL);
   
   struct trie *result;

   // Append dot
   const char *dot = ".";
   if (!search_trie(&dot, a, &result))
      build_trie(dot, result, &result);

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
