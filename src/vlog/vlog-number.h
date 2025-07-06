//
//  Copyright (C) 2023-2025  Nick Gasson
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
   LOGIC_0 = 0b00,
   LOGIC_1 = 0b01,
   LOGIC_X = 0b11,
   LOGIC_Z = 0b10,
} vlog_logic_t;

typedef struct _bignum bignum_t;

typedef enum {
   TAG_BIGNUM = 0x0,
} number_tag_t;

// Packed representation of Verilog's four-state logic type
typedef union _number {
   bignum_t *big;
   uint64_t  bits;
   struct {
      uint64_t tag : 2;
   } common;
} number_t;

STATIC_ASSERT(sizeof(number_t) == 8);

number_t number_new(const char *str, const loc_t *loc);
void number_free(number_t *val);
void number_print(number_t val, text_buf_t *tb);
bool number_is_defined(number_t val);
int64_t number_integer(number_t val);
unsigned number_width(number_t val);
vlog_logic_t number_bit(number_t val, unsigned n);
uint8_t number_byte(number_t val, unsigned n);
number_t number_pack(const uint8_t *bits, unsigned width);
bool number_equal(number_t a, number_t b);
bool number_truthy(number_t a);

void number_write(number_t val, fbuf_t *f);
number_t number_read(fbuf_t *f);

number_t number_add(number_t a, number_t b);
number_t number_sub(number_t a, number_t b);
number_t number_logical_equal(number_t a, number_t b);

void vec2_add(uint64_t *a, size_t asize, const uint64_t *b, size_t bsize);
void vec2_mul(uint64_t *a, size_t asize, const uint64_t *b, size_t bsize);

#endif  // _VLOG_NUMBER_H
