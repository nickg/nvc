#include "ident.h"
#include "util.h"

#include <assert.h>
#include <stdbool.h>
#include <ctype.h>
#include <stdlib.h>

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
        it != NULL && toupper(it->value) != toupper(**str);
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

ident_t make_ident(const char *str)
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

   static char *buf = NULL;
   static size_t buflen = 0;

   if (buf == NULL) {
      buf = xmalloc(ident->depth);
      buflen = ident->depth;
   }
   
   while (ident->depth > buflen) {
      buflen *= 2;
      buf = xrealloc(buf, buflen);
   }

   char *p = buf + ident->depth - 1;
   *p = '\0';

   struct trie *it;
   for (it = ident; it->value != '\0'; it = it->up) {
      assert(it != NULL);
      *(--p) = it->value;
   }
   assert(p == buf);

   return buf;
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
   ident_t i = make_ident(buf);
   assert(i->depth == len);
   free(buf);

   return i;
}

