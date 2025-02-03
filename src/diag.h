//
//  Copyright (C) 2011-2024  Nick Gasson
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

#ifndef _DIAG_H
#define _DIAG_H

#include "prim.h"
#include "util.h"
#include "common.h"

#include <stdio.h>
#include <stdarg.h>
#include <stdbool.h>

#ifdef __MINGW32__
#undef SEVERITY_ERROR
#endif

typedef uint16_t file_ref_t;

struct loc {
   unsigned first_line : 20;
   unsigned first_column : 12;
   unsigned line_delta : 8;
   unsigned column_delta : 8;
   unsigned file_ref : 16;
};

STATIC_ASSERT(sizeof(loc_t) == 8);

#define LINE_INVALID   0xfffff
#define COLUMN_INVALID 0xfff
#define FILE_INVALID   0xffff
#define DELTA_INVALID  0xff

#define LOC_INVALID ((loc_t) {      \
      LINE_INVALID, COLUMN_INVALID, \
      DELTA_INVALID, DELTA_INVALID, \
      FILE_INVALID                  \
   })

void fmt_loc(FILE *f, const loc_t *loc);
const char *loc_file_str(const loc_t *loc);
loc_t get_loc(unsigned first_line, unsigned first_column,
              unsigned last_line, unsigned last_column,
              file_ref_t file_ref);
bool loc_invalid_p(const loc_t *loc);
file_ref_t loc_file_ref(const char *name, const char *linebuf);
bool loc_eq(const loc_t *a, const loc_t *b);
const char *loc_get_source(const loc_t *loc);

loc_wr_ctx_t *loc_write_begin(fbuf_t *f);
void loc_write(const loc_t *loc, loc_wr_ctx_t *ctx);
void loc_write_end(loc_wr_ctx_t *ctx);

loc_rd_ctx_t *loc_read_begin(fbuf_t *f);
void loc_read(loc_t *loc, loc_rd_ctx_t *ctx);
void loc_read_end(loc_rd_ctx_t *ctx);

typedef enum {
   DIAG_DEBUG,
   DIAG_NOTE,
   DIAG_WARN,
   DIAG_ERROR,
   DIAG_FAILURE,
   DIAG_FATAL,
} diag_level_t;

// Error callback for use in unit tests
typedef void (*diag_consumer_t)(diag_t *, void *);
void diag_set_consumer(diag_consumer_t fn, void *);

typedef void (*diag_hint_fn_t)(diag_t *, void *);
void diag_add_hint_fn(diag_hint_fn_t fn, void *context);
void diag_remove_hint_fn(diag_hint_fn_t fn);

diag_t *diag_new(diag_level_t level, const loc_t *loc);
void diag_printf(diag_t *d, const char *fmt, ...)
   __attribute__((format(printf, 2, 3)));
void diag_vprintf(diag_t *d, const char *fmt, va_list ap);
void diag_write(diag_t *d, const char *str, size_t len);
text_buf_t *diag_text_buf(diag_t *d);
void diag_hint(diag_t *d, const loc_t *loc, const char *fmt, ...)
   __attribute__((format(printf, 3, 4)));
void diag_vhint(diag_t *d, const loc_t *loc, const char *fmt, va_list ap);
void diag_trace(diag_t *d, const loc_t *loc, const char *fmt, ...)
   __attribute__((format(printf, 3, 4)));
void diag_lrm(diag_t *d, vhdl_standard_t std, const char *section);
void diag_show_source(diag_t *d, bool show);
void diag_emit(diag_t *d);
void diag_femit(diag_t *d, FILE *f);
void diag_suppress(diag_t *d, bool suppress);
void diag_stacktrace(diag_t *d, bool stacktrace);
void diag_clear(diag_t *d);
unsigned diag_count(diag_level_t level);

unsigned error_count(void);
void reset_error_count(void);
unsigned set_error_limit(unsigned limit);

void wrapped_printf(const char *fmt, ...);
void wrapped_vprintf(const char *fmt, va_list ap);

#define pedantic_diag(loc) ({                          \
         static int _warned = 0;                       \
         _pedantic_diag(loc, &_warned, NULL);          \
      })

diag_t *_pedantic_diag(const loc_t *loc, int *warned, bool *error);

// Accessors for use in unit tests
const char *diag_get_text(diag_t *d);
const char *diag_get_hint(diag_t *d, int nth);
const char *diag_get_trace(diag_t *d, int nth);
const loc_t *diag_get_loc(diag_t *d);
int diag_hints(diag_t *d);
int diag_traces(diag_t *d);

#endif  // _DIAG_H
