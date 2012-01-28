//
//  Copyright (C) 2011-2012  Nick Gasson
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

#include "loc.h"

#define ARRAY_LEN(a) (sizeof(a) / sizeof(a[0]))

void *xmalloc(size_t size);
void *xrealloc(void *ptr, size_t size);

void errorf(const char *fmt, ...)
   __attribute__((format(printf, 1, 2)));
void fatal(const char *fmt, ...)
   __attribute__((format(printf, 1, 2), noreturn));
void fatal_errno(const char *fmt, ...)
   __attribute__((format(printf, 1, 2), noreturn));

#define likely(x) __builtin_expect(x, 1)
#define unlikely(x) __builtin_expect(x, 0)

// Error callback for use in unit tests.
typedef void (*error_fn_t)(const char *msg, const loc_t *loc);
error_fn_t set_error_fn(error_fn_t fn);

void error_at(const loc_t *loc, const char *fmt, ...)
   __attribute__((format(printf, 2, 3)));

void fmt_loc(FILE *f, const loc_t *loc);

void show_stacktrace(void);
void register_trace_signal_handlers(void);

void write_u32(uint32_t u, FILE *f);
void write_u16(uint16_t s, FILE *f);
bool write_b(bool b, FILE *f);
void write_i32(int32_t i, FILE *f);
void write_i64(int64_t i, FILE *f);
void write_u8(uint8_t u, FILE *f);

uint32_t read_u32(FILE *f);
uint16_t read_u16(FILE *f);
bool read_b(FILE *f);
int32_t read_i32(FILE *f);
int64_t read_i64(FILE *f);
uint8_t read_u8(FILE *f);

#endif // _UTIL_H
