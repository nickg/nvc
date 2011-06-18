#include "util.h"

#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include <errno.h>
#include <string.h>

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
