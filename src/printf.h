//
//  Copyright (C) 2025  Nick Gasson
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

#ifndef _PRINTF_H
#define _PRINTF_H

#include "prim.h"

#include <stdarg.h>

typedef void (*write_fn_t)(const char *buf, size_t len, void *ctx);

typedef enum {
   CHARSET_ISO88591,
   CHARSET_UTF8,
} charset_t;

typedef struct {
   write_fn_t  callback;
   void       *context;
   charset_t   charset;
   bool        terminal;
} ostream_t;

int nvc_vfprintf(ostream_t *os, const char *fmt, va_list ap);
int nvc_vprintf(const char *fmt, va_list ap);
int nvc_printf(const char *fmt, ...)
   __attribute__((format(printf, 1, 2)));
int nvc_fprintf(ostream_t *os, const char *fmt, ...)
   __attribute__((format(printf, 2, 3)));

int ostream_write(ostream_t *os, const char *buf, size_t len);
int ostream_putc(ostream_t *os, char ch);
int ostream_puts(ostream_t *os, const char *str);

void stdio_ostream_write(const char *buf, size_t len, void *ctx);

ostream_t *nvc_stdout(void);
ostream_t *nvc_stderr(void);

char *color_asprintf(const char *fmt, ...);

#endif  // _PRINTF_H
