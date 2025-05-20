//
//  Copyright (C) 2023-2024  Nick Gasson
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

#define STRIP_STRENGTH(x) (x & 3)

typedef struct _bignum bignum_t;

typedef enum {
   TAG_BIGNUM = 0x0,
   TAG_SMALLNUM = 0x01,
   TAG_INTEGER = 0x02,
} number_tag_t;

#define SMALLNUM_PACKED_BITS 56
#define SMALLNUM_WIDTH_BITS  5
#define SMALLNUM_WIDTH_MAX  ((1 << SMALLNUM_WIDTH_BITS) - 1)
#define SMALLNUM_ENCODE_MAX ((UINT64_C(1) << SMALLNUM_PACKED_BITS/2) - 1)

STATIC_ASSERT(SMALLNUM_ENCODE_MAX <= (UINT64_C(1) << SMALLNUM_WIDTH_MAX) - 1);

#define INTEGER_PACKED_BITS 32
#define INTEGER_WIDTH_BITS  6
#define INTEGER_WIDTH_MAX  (1 << (INTEGER_WIDTH_BITS - 1))
#define INTEGER_ENCODE_MAX  ((UINT64_C(1) << INTEGER_PACKED_BITS) - 1)

STATIC_ASSERT(INTEGER_ENCODE_MAX <= (UINT64_C(1) << INTEGER_WIDTH_MAX) - 1);

// Packed representation of Verilog's four-state logic type
typedef union _number {
   bignum_t *big;
   uint64_t  bits;
   struct {
      uint64_t tag : 2;
   } common;
   struct {
      uint64_t tag : 2;
      uint64_t issigned : 1;
      uint64_t width : SMALLNUM_WIDTH_BITS;
      uint64_t packed : SMALLNUM_PACKED_BITS;
   } small;
   struct {
      uint64_t tag : 2;
      uint64_t issigned : 1;
      uint64_t width : INTEGER_WIDTH_BITS;
      uint64_t packed : INTEGER_PACKED_BITS;
   } intg;
   struct {
      uint64_t tag : 2;
      uint64_t bit : 2;
      uint64_t width : 32;
   } rep;
} number_t;

STATIC_ASSERT(sizeof(number_t) == 8);

number_t number_new(const char *str);
void number_free(number_t *val);
void number_print(number_t val, text_buf_t *tb);
bool number_is_defined(number_t val);
int64_t number_integer(number_t val);
unsigned number_width(number_t val);
vlog_logic_t number_bit(number_t val, unsigned n);
number_t number_pack(const uint8_t *bits, unsigned width);
bool number_equal(number_t a, number_t b);

void number_write(number_t val, fbuf_t *f);
number_t number_read(fbuf_t *f);

#endif  // _VLOG_NUMBER_H
