//
//  Copyright (C) 2011  Nick Gasson
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

#include "loc.h"

#define ARRAY_LEN(a) (sizeof(a) / sizeof(a[0]))

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
void write_i(int i, FILE *f);

unsigned read_u(FILE *f);
unsigned short read_s(FILE *f);
bool read_b(FILE *f);
int read_i(FILE *f);

#endif // _UTIL_H
