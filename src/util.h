#ifndef _UTIL_H
#define _UTIL_H

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stddef.h>
#include <stdio.h>
#include <stdbool.h>

#include "loc.h"

void *xmalloc(size_t size);
void *xrealloc(void *ptr, size_t size);

void errorf(const char *fmt, ...)
   __attribute__((format(printf, 1, 2)));
void fatal(const char *fmt, ...)
   __attribute__((format(printf, 1, 2), noreturn));

void fmt_loc(FILE *f, const loc_t *loc);

void show_stacktrace(void);
void register_trace_signal_handlers(void);

void write_u(unsigned u, FILE *f);
void write_s(unsigned short s, FILE *f);
bool write_b(bool b, FILE *f);

unsigned read_u(FILE *f);
unsigned short read_s(FILE *f);
bool read_b(FILE *f);

#endif // _UTIL_H
