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

#ifndef _TEST_MIR_H
#define _TEST_MIR_H

#include "test_util.h"
#include "mir/mir-node.h"
#include "mir/mir-unit.h"

#include <inttypes.h>

typedef union {
   struct {
      uint64_t data : 60;
      uint64_t tag : 4;
   };
   struct {
      float    real;
      uint32_t ones : 28;
      uint32_t zero : 4;
   };
   const void *ptr;
   uint64_t    bits;
} mir_pattern_t;

STATIC_ASSERT(sizeof(mir_pattern_t) == 8);

#define _ (UINT32_MAX + 1)
#define VAR(name) ((mir_pattern_t){ .ptr = ("\x8" name) })
#define PARAM(name) ((mir_pattern_t){ .ptr = ("\x2" name) })
#define LINK(name) ((mir_pattern_t){ .ptr = ("\xa" name) })
#define NODE(n) ((mir_pattern_t){ .tag = MIR_TAG_NODE, .data = n })
#define BLOCK(n) ((mir_pattern_t){ .tag = MIR_TAG_BLOCK, .data = n })
#define CONST(n) ((mir_pattern_t){ .tag = MIR_TAG_CONST, .data = n })
#define ENUM(n) ((mir_pattern_t){ .tag = MIR_TAG_ENUM, .data = n })
#define REAL(f) ((mir_pattern_t){ .ones = ~0, .real = (float)(f) })

#define mir_match(mu, b, pat) _mir_match((mu), (b), (pat), ARRAY_LEN((pat)))

#define mir_assert_const_eq(mu, value, exp) do {          \
      int64_t __cval;                                     \
      ck_assert(mir_get_const((mu), (value), &__cval));   \
      ck_assert_int_eq(__cval, (exp));                    \
   } while (0)

typedef struct {
   mir_op_t      op;
   mir_pattern_t arg0;
   mir_pattern_t arg1;
   mir_pattern_t arg2;
} mir_match_t;

STATIC_ASSERT(sizeof(mir_match_t) <= 32);

void _mir_match(mir_unit_t *mu, int nth, const mir_match_t *mm, size_t length);

#endif  // _TEST_MIR_H
