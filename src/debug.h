//
//  Copyright (C) 2021-2023  Nick Gasson
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

#ifndef _DEBUG_H
#define _DEBUG_H

#include "util.h"
#include "ident.h"

#include <stdint.h>

typedef struct debug_info   debug_info_t;
typedef struct debug_inline debug_inline_t;
typedef struct debug_src    debug_src_t;

typedef enum {
   FRAME_PROG,
   FRAME_LIB,
   FRAME_VHDL
} frame_kind_t;

typedef struct debug_inline {
   const char     *symbol;
   const char     *srcfile;
   ident_t         vhdl_unit;
   unsigned        lineno;
   unsigned        colno;
   debug_inline_t *next;
} debug_inline_t;

typedef struct {
   frame_kind_t    kind;
   uintptr_t       pc;
   const char     *symbol;
   const char     *srcfile;
   const char     *module;
   ident_t         vhdl_unit;
   unsigned        lineno;
   unsigned        colno;
   ptrdiff_t       disp;
   debug_inline_t *inlined;
} debug_frame_t;

typedef void (*debug_unwind_fn_t)(uintptr_t, debug_frame_t *, void *);

debug_info_t *debug_capture(void);
void debug_free(debug_info_t *di);
unsigned debug_count_frames(debug_info_t *di);
const debug_frame_t *debug_get_frame(debug_info_t *di, unsigned n);
uint32_t debug_hash(debug_info_t *di);

void debug_add_unwinder(void *start, size_t len, debug_unwind_fn_t fn,
                        void *context);
void debug_remove_unwinder(void *start);

const char *debug_symbol_name(void *addr);

#endif   // _DEBUG_H
