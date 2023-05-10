//
//  Copyright (C) 2023  Nick Gasson
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

#ifndef _VLOG_NUMBER_H
#define _VLOG_NUMBER_H

#include "prim.h"
#include "util.h"

#include <stdint.h>

typedef enum {
   LOGIC_0,
   LOGIC_1,
   LOGIC_Z,
   LOGIC_X,
} vlog_logic_t;

typedef struct _bignum bignum_t;

// Packed representation of Verilog's four-state logic type
typedef union _number {
   bignum_t *ext;
   uint64_t  bits;
   struct {
      unsigned tag : 1;
      unsigned width : 6;
      unsigned issigned : 1;
      uint64_t packed : 56;
   };
} number_t;

STATIC_ASSERT(sizeof(number_t) == 8);

number_t number_new(const char *str);
void number_free(number_t val);
void number_print(number_t val, text_buf_t *tb);
bool number_is_defined(number_t val);
int64_t number_to_integer(number_t val);
unsigned number_width(number_t val);

#endif  // _VLOG_NUMBER_H
