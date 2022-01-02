//
//  Copyright (C) 2011-2021  Nick Gasson
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

#ifndef _LOC_H
#define _LOC_H

#include "util.h"
#include "ident.h"
#include "prim.h"

#include <stdint.h>

typedef uint16_t loc_file_ref_t;

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
const char *loc_linebuf(const loc_t *loc);
loc_t get_loc(unsigned first_line, unsigned first_column,
              unsigned last_line, unsigned last_column,
              loc_file_ref_t file_ref);
bool loc_invalid_p(const loc_t *loc);
loc_file_ref_t loc_file_ref(const char *name, const char *linebuf);
bool loc_eq(const loc_t *a, const loc_t *b);
bool loc_contains(const loc_t *outer, const loc_t *inner);

loc_wr_ctx_t *loc_write_begin(fbuf_t *f);
void loc_write(const loc_t *loc, loc_wr_ctx_t *ctx);
void loc_write_end(loc_wr_ctx_t *ctx);

loc_rd_ctx_t *loc_read_begin(fbuf_t *f);
void loc_read(loc_t *loc, loc_rd_ctx_t *ctx);
void loc_read_end(loc_rd_ctx_t *ctx);

#endif // _LOC_H
