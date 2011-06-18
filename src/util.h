#ifndef _UTIL_H
#define _UTIL_H

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stddef.h>

void *xmalloc(size_t size);
void *xrealloc(void *ptr, size_t size);

void errorf(const char *fmt, ...)
   __attribute__((format(printf, 1, 2)));
void fatal(const char *fmt, ...)
   __attribute__((format(printf, 1, 2)));

#endif // _UTIL_H
