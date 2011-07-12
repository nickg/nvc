#include "util.h"

#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include <errno.h>
#include <string.h>
#include <stdbool.h>

void *xmalloc(size_t size)
{
   void *p = malloc(size);
   if (p == NULL)
      abort();
   return p;
}

void *xrealloc(void *ptr, size_t size)
{
   ptr = realloc(ptr, size);
   if (ptr == NULL)
      abort();
   return ptr;
}

void errorf(const char *fmt, ...)
{
   va_list ap;
   va_start(ap, fmt);

   fprintf(stderr, "error: ");
   vfprintf(stderr, fmt, ap);
   fprintf(stderr, "\n");
   
   va_end(ap);
}

void fatal(const char *fmt, ...)
{
   va_list ap;
   va_start(ap, fmt);

   fprintf(stderr, "fatal: ");
   vfprintf(stderr, fmt, ap);
   fprintf(stderr, "\n");
   
   va_end(ap);

   exit(EXIT_FAILURE);
}

void fmt_loc(FILE *f, const struct loc *loc)
{
   if (loc->first_line == -1)
      return;
   
   const char *lb = loc->linebuf;
   char buf[80];
   size_t i = 0;
   while (i < sizeof(buf) - 4 && *lb != '\0' && *lb != '\n') {
      // TODO: expand tabs?
      buf[i++] = *lb++;
   }

   if (i == sizeof(buf) - 4) {
      buf[i++] = '.';
      buf[i++] = '.';
      buf[i++] = '.';
   }

   buf[i] = '\0';

   // Print ... if error location spans multiple lines
   bool many_lines = (loc->first_line != loc->last_line);
   int last_col = many_lines ? strlen(buf) + 4 : loc->last_column;

   fprintf(stderr, "    %s%s\n", buf, many_lines ? " ..." : "");
   for (int i = 0; i < loc->first_column + 4; i++)
      fprintf(stderr, " ");
   for (int i = 0; i < last_col - loc->first_column + 1; i++)
      fprintf(stderr, "^");
   fprintf(stderr, "\n");
}

