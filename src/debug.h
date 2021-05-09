//
//  Copyright (C) 2021  Nick Gasson
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

typedef struct debug_info debug_info_t;

typedef enum {
   FRAME_PROG,
   FRAME_LIB,
   FRAME_VHDL
} frame_kind_t;

typedef struct {
   frame_kind_t  kind;
   uintptr_t     pc;
   const char   *symbol;
   const char   *srcfile;
   const char   *module;
   ident_t       vhdl_unit;
   unsigned      lineno;
   unsigned      colno;
   ptrdiff_t     disp;
} debug_frame_t;

debug_info_t *debug_capture(void);
void debug_free(debug_info_t *di);
unsigned debug_count_frames(debug_info_t *di);
const debug_frame_t *debug_get_frame(debug_info_t *di, unsigned n);

#endif   // _DEBUG_H
