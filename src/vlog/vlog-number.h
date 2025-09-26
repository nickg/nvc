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
number_t number_from_string(const char *bytes, size_t len);
number_t number_from_int(int64_t value);
number_t number_from_bool(bool value);
void number_print(number_t val, text_buf_t *tb);
bool number_is_defined(number_t val);
int64_t number_integer(number_t val);
unsigned number_width(number_t val);
vlog_logic_t number_bit(number_t val, unsigned n);
uint8_t number_byte(number_t val, unsigned n);
void number_get(number_t val, const uint64_t **abits, const uint64_t **bbits);
bool number_equal(number_t a, number_t b);
bool number_truthy(number_t a);
bool number_signed(number_t a);
uint32_t number_hash(number_t n);

void number_write(number_t val, fbuf_t *f);
number_t number_read(fbuf_t *f);

number_t number_add(number_t a, number_t b);
number_t number_sub(number_t a, number_t b);
number_t number_mul(number_t a, number_t b);
number_t number_div(number_t a, number_t b);
number_t number_exp(number_t a, number_t b);
number_t number_shl(number_t a, number_t b);
number_t number_negate(number_t a);
number_t number_logical_equal(number_t a, number_t b);
number_t number_not(number_t a);
number_t number_greater(number_t a, number_t b);
number_t number_greater_equal(number_t a, number_t b);
number_t number_less(number_t a, number_t b);
number_t number_less_equal(number_t a, number_t b);

void vec2_add(int size, uint64_t *a, const uint64_t *b);
void vec2_sub(int size, uint64_t *a, const uint64_t *b);
void vec2_mul(int size, uint64_t *a, const uint64_t *b);
void vec2_div(int size, uint64_t *a, const uint64_t *b);
void vec2_mod(int size, uint64_t *a, const uint64_t *b);
void vec2_exp(int size, uint64_t *a, const uint64_t *b);
void vec2_shl(int size, uint64_t *a, const uint64_t *b);
void vec2_shr(int size, uint64_t *a, const uint64_t *b);
void vec2_neg(int size, uint64_t *a);
void vec2_inv(int size, uint64_t *a);
int vec2_and1(int size, const uint64_t *a);
int vec2_or1(int size, const uint64_t *a);

int vec2_sgt(int size, const uint64_t *a, const uint64_t *b);
int vec2_gt(int size, const uint64_t *a, const uint64_t *b);
int vec2_slt(int size, const uint64_t *a, const uint64_t *b);
int vec2_lt(int size, const uint64_t *a, const uint64_t *b);
int vec2_sge(int size, const uint64_t *a, const uint64_t *b);
int vec2_ge(int size, const uint64_t *a, const uint64_t *b);
int vec2_sle(int size, const uint64_t *a, const uint64_t *b);
int vec2_le(int size, const uint64_t *a, const uint64_t *b);

void vec4_add(int size, uint64_t *a1, uint64_t *b1, const uint64_t *a2,
              const uint64_t *b2);
void vec4_mul(int size, uint64_t *a1, uint64_t *b1, const uint64_t *a2,
              const uint64_t *b2);
void vec4_div(int size, uint64_t *a1, uint64_t *b1, const uint64_t *a2,
              const uint64_t *b2);
void vec4_mod(int size, uint64_t *a1, uint64_t *b1, const uint64_t *a2,
              const uint64_t *b2);
void vec4_exp(int size, uint64_t *a1, uint64_t *b1, const uint64_t *a2,
              const uint64_t *b2);
void vec4_shl(int size, uint64_t *a1, uint64_t *b1, const uint64_t *a2,
              const uint64_t *b2);
void vec4_shr(int size, uint64_t *a1, uint64_t *b1, const uint64_t *a2,
              const uint64_t *b2);
void vec4_inv(int size, uint64_t *a, uint64_t *b);

#endif  // _VLOG_NUMBER_H
