//
//  Copyright (C) 2011-2013  Nick Gasson
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

#ifndef _UTIL_H
#define _UTIL_H

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stddef.h>
#include <stdio.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdarg.h>

#include "prim.h"

#define ARRAY_LEN(a) (sizeof(a) / sizeof(a[0]))

#define MAX(x, y) ((x) > (y) ? (x) : (y))
#define MIN(x, y) ((x) < (y) ? (x) : (y))

void *xmalloc(size_t size);
void *xrealloc(void *ptr, size_t size);

void errorf(const char *fmt, ...)
   __attribute__((format(printf, 1, 2)));
void warnf(const char *fmt, ...)
   __attribute__((format(printf, 1, 2)));
void notef(const char *fmt, ...)
   __attribute__((format(printf, 1, 2)));
void fatal(const char *fmt, ...)
   __attribute__((format(printf, 1, 2), noreturn));
void fatal_trace(const char *fmt, ...)
   __attribute__((format(printf, 1, 2), noreturn));
void fatal_errno(const char *fmt, ...)
   __attribute__((format(printf, 1, 2), noreturn));

#define likely(x) __builtin_expect(x, 1)
#define unlikely(x) __builtin_expect(x, 0)

// Error callback for use in unit tests.
typedef void (*error_fn_t)(const char *msg, const loc_t *loc);
error_fn_t set_error_fn(error_fn_t fn);

typedef void (*fatal_fn_t)(void);
void set_fatal_fn(fatal_fn_t fn);

void error_at(const loc_t *loc, const char *fmt, ...)
   __attribute__((format(printf, 2, 3)));
void warn_at(const loc_t *loc, const char *fmt, ...)
   __attribute__((format(printf, 2, 3)));
void note_at(const loc_t *loc, const char *fmt, ...)
   __attribute__((format(printf, 2, 3)));
void fatal_at(const loc_t *loc, const char *fmt, ...)
   __attribute__((format(printf, 2, 3), noreturn));
void error_at_v(const loc_t *loc, const char *fmt, va_list ap);

void fmt_loc(FILE *f, const loc_t *loc);

void show_stacktrace(void);
void register_trace_signal_handlers(void);
void register_gdb_signal_handlers(void);

void term_init(void);

void opt_set_int(const char *name, int val);
int opt_get_int(const char *name);

char *get_fmt_buf(size_t len);

void static_printf_begin(char *buf, size_t len);
void static_printf(char *buf, const char *fmt, ...)
   __attribute__((format(printf, 2, 3)));

int next_power_of_2(int n);

#endif // _UTIL_H
