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

#include "util.h"
#include "ident.h"
#include "jit/jit-priv.h"
#include "jit/jit.h"
#include "option.h"
#include "thread.h"

#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

#ifdef HAVE_AVX2
#include <x86intrin.h>
#endif

typedef enum {
   CPU_AVX2 = 0x1,
   CPU_SSE41 = 0x2,
} cpu_feature_t;

typedef struct {
   const char     *name;
   jit_entry_fn_t  entry;
   cpu_feature_t   feature;
   ident_t         ident;
} jit_intrinsic_t;

typedef enum {
   _U  = 0x0,
   _X  = 0x1,
   _0  = 0x2,
   _1  = 0x3,
   _Z  = 0x4,
   _W  = 0x5,
   _L  = 0x6,
   _H  = 0x7,
   _DC = 0x8
} std_ulogic_t;

#define IS_01(x) \
   _Generic((x),                                                        \
            uint64_t: (((x) & UINT64_C(0x0e0e0e0e0e0e0e0e))             \
                       == UINT64_C(0x0202020202020202)),                \
            uint8_t: ((x) & 0xe) == 0x02)

static const uint8_t cvt_to_x01[16] = {
   _X, _X, _0, _1, _X, _X, _0, _1, _X, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff
};

static const uint8_t xor_table[16][16] = {
   // --------------------------------------------------
   // | U   X   0   1   Z   W   L   H   -          |   |
   // --------------------------------------------------
   {   _U, _U, _U, _U, _U, _U, _U, _U, _U   },  // | U |
   {   _U, _X, _X, _X, _X, _X, _X, _X, _X   },  // | X |
   {   _U, _X, _0, _1, _X, _X, _0, _1, _X   },  // | 0 |
   {   _U, _X, _1, _0, _X, _X, _1, _0, _X   },  // | 1 |
   {   _U, _X, _X, _X, _X, _X, _X, _X, _X   },  // | Z |
   {   _U, _X, _X, _X, _X, _X, _X, _X, _X   },  // | W |
   {   _U, _X, _0, _1, _X, _X, _0, _1, _X   },  // | L |
   {   _U, _X, _1, _0, _X, _X, _1, _0, _X   },  // | H |
   {   _U, _X, _X, _X, _X, _X, _X, _X, _X   },  // | - |
};

static const uint8_t and_table[16][16] = {
   // ---------------------------------------------------
   // |  U   X   0   1   Z   W   L   H   -          |   |
   // ---------------------------------------------------
   {    _U, _U, _0, _U, _U, _U, _0, _U, _U   },  // | U |
   {    _U, _X, _0, _X, _X, _X, _0, _X, _X   },  // | X |
   {    _0, _0, _0, _0, _0, _0, _0, _0, _0   },  // | 0 |
   {    _U, _X, _0, _1, _X, _X, _0, _1, _X   },  // | 1 |
   {    _U, _X, _0, _X, _X, _X, _0, _X, _X   },  // | Z |
   {    _U, _X, _0, _X, _X, _X, _0, _X, _X   },  // | W |
   {    _0, _0, _0, _0, _0, _0, _0, _0, _0   },  // | L |
   {    _U, _X, _0, _1, _X, _X, _0, _1, _X   },  // | H |
   {    _U, _X, _0, _X, _X, _X, _0, _X, _X   },  // | - |
};

static const uint8_t or_table[16][16] = {
   // ---------------------------------------------------
   // |  U   X   0   1   Z   W   L   H   -          |   |
   // ---------------------------------------------------
   {    _U, _U, _U, _1, _U, _U, _U, _1, _U   },  // | U |
   {    _U, _X, _X, _1, _X, _X, _X, _1, _X   },  // | X |
   {    _U, _X, _0, _1, _X, _X, _0, _1, _X   },  // | 0 |
   {    _1, _1, _1, _1, _1, _1, _1, _1, _1   },  // | 1 |
   {    _U, _X, _X, _1, _X, _X, _X, _1, _X   },  // | Z |
   {    _U, _X, _X, _1, _X, _X, _X, _1, _X   },  // | W |
   {    _U, _X, _0, _1, _X, _X, _0, _1, _X   },  // | L |
   {    _1, _1, _1, _1, _1, _1, _1, _1, _1   },  // | H |
   {    _U, _X, _X, _1, _X, _X, _X, _1, _X   },  // | - |
};

__attribute__((always_inline))
static inline void *__tlab_alloc(tlab_t *t, size_t size)
{
   assert(t->alloc <= t->limit);
   assert((t->alloc & (sizeof(double) - 1)) == 0);

   if (likely(t->alloc + size <= t->limit)) {
      void *p = t->base + t->alloc;
      t->alloc += ALIGN_UP(size, sizeof(double));
      return p;
   }
   else
      return mspace_alloc(t->mspace, size);
}

__attribute__((always_inline))
static inline uint32_t __tlab_mark(tlab_t *t)
{
   return t->alloc;
}

__attribute__((always_inline))
static inline void __tlab_restore(tlab_t *t, uint32_t mark)
{
   assert(mark <= t->alloc);
   t->alloc = mark;
}

#if 0
static void print_bits(const char *tag, const uint8_t *bits, size_t size)
{
   static const char map[] = "UX01ZWLH-";

   printf("%s: ", tag);
   for (int i = 0; i < size; i++)
      printf("%c", map[bits[i]]);
   printf("\n");
}
#endif

__attribute__((cold))
static void __ieee_warn(jit_func_t *func, jit_anchor_t *caller, const char *msg)
{
   if (!opt_get_int(OPT_IEEE_WARNINGS))
      return;

   jit_anchor_t frame = {
      .caller = caller,
      .func = func
   };

   jit_thread_local_t *thread = jit_thread_local();
   thread->anchor = &frame;

   diag_t *d = diag_new(DIAG_WARN, NULL);
   diag_printf(d, "Assertion Warning: %s", msg);
   diag_show_source(d, false);
   diag_emit(d);

   thread->anchor = NULL;
}

__attribute__((cold, noreturn))
static void __ieee_failure(jit_func_t *func, jit_anchor_t *caller,
                           const char *msg)
{
   jit_anchor_t frame = {
      .caller = caller,
      .func = func
   };

   jit_thread_local_t *thread = jit_thread_local();
   thread->anchor = &frame;

   diag_t *d = diag_new(DIAG_FATAL, NULL);
   diag_printf(d, "Assertion Failure: %s", msg);
   diag_show_source(d, false);
   diag_emit(d);

   jit_abort_with_status(EXIT_FAILURE);
}

__attribute__((always_inline))
static inline void __invert_bits(const uint8_t *input, int size,
                                 uint8_t *result)
{
   for (int i = 0; i < size; i++) {
      assert(input[i] == _0 || input[i] == _1);
      result[i] = input[i] ^ 1;
   }
}

__attribute__((always_inline))
static inline void __add_unsigned(const uint8_t *left, const uint8_t *right,
                                  uint8_t cbit, int size, uint8_t *result)
{
   for (int i = size - 1; i >= 0; i--) {
      // RESULT(I) := CBIT xor XL(I) xor XR(I);
      result[i] = xor_table[xor_table[cbit][left[i]]][right[i]];

      // CBIT := (CBIT and XL(I)) or (CBIT and XR(I)) or (XL(I) and XR(I));
      const uint8_t tmp1 = and_table[cbit][left[i]];
      const uint8_t tmp2 = and_table[cbit][right[i]];
      const uint8_t tmp3 = and_table[left[i]][right[i]];
      cbit = or_table[or_table[tmp1][tmp2]][tmp3];
   }
}

static void ieee_add_unsigned(jit_func_t *func, jit_anchor_t *anchor,
                              jit_scalar_t *args, tlab_t *tlab)
{
   const int size = args[3].integer ^ (args[3].integer >> 63);
   assert(size == (args[6].integer ^ (args[6].integer >> 63)));

   const uint8_t *left = args[1].pointer;
   const uint8_t *right = args[4].pointer;
   uint8_t cbit = args[7].integer;

   uint8_t *result = __tlab_alloc(tlab, size);

   __add_unsigned(left, right, cbit, size, result);

   args[0].pointer = result;
   args[1].integer = size - 1;
   args[2].integer = ~size;
}

#ifdef HAVE_SSE41
__attribute__((target("sse4.1")))
static void std_to_x01_sse41(jit_func_t *func, jit_anchor_t *anchor,
                             jit_scalar_t *args, tlab_t *tlab)
{
   const int size = args[3].integer ^ (args[3].integer >> 63);
   const uint8_t *input = args[1].pointer;

   uint8_t *result = __tlab_alloc(tlab, size);

   __m128i lookup = _mm_loadu_si128((const __m128i *)cvt_to_x01);

   int pos = 0;
   for (; pos + 15 < size; pos += 16) {
      __m128i in = _mm_loadu_si128((const __m128i *)(input + pos));
      __m128i out = _mm_shuffle_epi8(lookup, in);
      _mm_storeu_si128((__m128i *)(result + pos), out);
   }

   for (; pos < size; pos++)
      result[pos] = cvt_to_x01[input[pos]];

   args[0].pointer = result;
   args[1].integer = size - 1;
   args[2].integer = ~size;
}
#endif

static void std_to_x01(jit_func_t *func, jit_anchor_t *anchor,
                       jit_scalar_t *args, tlab_t *tlab)
{
   const int size = args[3].integer ^ (args[3].integer >> 63);
   const uint8_t *input = args[1].pointer;

   uint8_t *result = __tlab_alloc(tlab, size);

   for (int i = 0; i < size; i++)
      result[i] = cvt_to_x01[input[i]];

   args[0].pointer = result;
   args[1].integer = size - 1;
   args[2].integer = ~size;
}

__attribute__((always_inline))
static inline bool __all_01(const void *vec, int size)
{
   int pos = 0;
   for (; pos + 7 < size; pos += 8) {
      const uint64_t u64 = unaligned_load(vec + pos, uint64_t);
      if (!IS_01(u64))
         return false;
   }

   for (; pos < size; pos++) {
      const uint8_t u8 = *(const uint8_t *)(vec + pos);
      if (!IS_01(u8))
         return false;
   }

   return true;
}

__attribute__((always_inline))
static inline uint8_t *__to_01(tlab_t *tlab, const uint8_t *input,
                               int size, uint8_t xmap)
{
   assert(size > 0);

   if (__all_01(input, size))
      return (uint8_t *)input;
   else {
      uint8_t *result = __tlab_alloc(tlab, size);

      bool bad = false;
      for (int i = 0; i < size; i++) {
         const uint8_t elt = input[i];
         if (elt == _1 || elt == _H)
            result[i] = _1;
         else if (elt == _0 || elt == _L)
            result[i] = _0;
         else
            bad = true;
      }

      if (bad)
         memset(result, xmap, size);

      return result;
   }
}

static void ieee_to_01(jit_func_t *func, jit_anchor_t *anchor,
                       jit_scalar_t *args, tlab_t *tlab)
{
   const int size = args[3].integer ^ (args[3].integer >> 63);
   const uint8_t *input = args[1].pointer;
   const uint8_t xmap = args[4].integer;

   if (size == 0) {
      __ieee_warn(func, anchor,
                  "NUMERIC_STD.TO_01: null detected, returning NAU");

      args[0].pointer = NULL;
      args[1].integer = 0;
      args[2].integer = -1;
   }
   else {
      args[0].pointer = __to_01(tlab, input, size, xmap);
      args[1].integer = size - 1;
      args[2].integer = ~size;
   }
}

__attribute__((always_inline))
static inline uint8_t *__resize_unsigned(tlab_t *tlab, const void *input,
                                         int size, int newsize)
{
   if (newsize < 1)
      return NULL;
   else if (size >= newsize)
      return (uint8_t *)input + size - newsize;
   else {
      uint8_t *result = __tlab_alloc(tlab, newsize);

      const int pad = newsize - size;
      memset(result, _0, pad);
      memcpy(result + pad, input, size);

      return result;
   }
}

__attribute__((always_inline))
static inline uint8_t *__resize_signed(tlab_t *tlab, const void *input,
                                       int size, int newsize)
{
   if (newsize < 1)
      return NULL;
   else if (size == newsize)
      return (uint8_t *)input;
   else if (size == 0) {
      uint8_t *result = __tlab_alloc(tlab, newsize);
      memset(result, _0, newsize);
      return result;
   }
   else {
      const int bound = MIN(size, newsize) - 1;
      assert(bound >= 0);

      uint8_t *result = __tlab_alloc(tlab, newsize);
      memset(result, *(uint8_t *)input, newsize - bound);
      memcpy(result + newsize - bound, input + size - bound, bound);

      return result;
   }
}

static void ieee_resize_unsigned(jit_func_t *func, jit_anchor_t *anchor,
                                 jit_scalar_t *args, tlab_t *tlab)
{
   const int size = args[3].integer ^ (args[3].integer >> 63);
   const int newsize = args[4].integer;
   const uint8_t *input = args[1].pointer;

   args[0].pointer = __resize_unsigned(tlab, input, size, newsize);
   args[1].integer = newsize - 1;
   args[2].integer = ~newsize;
}

static void ieee_resize_signed(jit_func_t *func, jit_anchor_t *anchor,
                               jit_scalar_t *args, tlab_t *tlab)
{
   const int size = args[3].integer ^ (args[3].integer >> 63);
   const int newsize = args[4].integer;
   const uint8_t *input = args[1].pointer;

   args[0].pointer = __resize_signed(tlab, input, size, newsize);
   args[1].integer = newsize - 1;
   args[2].integer = ~newsize;
}

__attribute__((always_inline))
static inline uint8_t __pack_low_bits(const void* vec)
{
   uint64_t bits = unaligned_load(vec, uint64_t);
   bits &= UINT64_C(0x0101010101010101);
   bits *= UINT64_C(0x8040201008040201);
   return bits >> 56;
}

__attribute__((always_inline))
static inline void __spread_bits(void* vec, uint8_t packed)
{
   const uint64_t spread1 = packed * UINT64_C(0x0000040010004001);
   const uint64_t mask1 = spread1 & UINT64_C(0x0001000100010001);

   const uint64_t spread2 = packed * UINT64_C(0x0002000800200080);
   const uint64_t mask2 = spread2 & UINT64_C(0x0100010001000100);

   const uint64_t bits = mask1 | mask2 | UINT64_C(0x0202020202020202);
   const uint64_t swap = __builtin_bswap64(bits);

   memcpy(vec, &swap, sizeof(swap));
}

__attribute__((always_inline))
static inline void __ieee_packed_add(uint8_t *left, uint8_t *right, int size,
                                     int carry, uint8_t *result)
{
   int pos = size - 8;
   for (; pos > 0; pos -= 8) {
      const unsigned lbyte = __pack_low_bits(left + pos);
      const unsigned rbyte = __pack_low_bits(right + pos);
      const unsigned sum = lbyte + rbyte + carry;

      __spread_bits(result + pos, sum);
      carry = !!(sum & 0x100);
   }

   for (; pos + 8 > 0; pos--) {
      const unsigned lbit = left[pos + 7] & 1;
      const unsigned rbit = right[pos + 7] & 1;
      const unsigned sum = lbit + rbit + carry;
      result[pos + 7] = (sum & 1) | 0x02;
      carry = !!(sum & 2);
   }
}

static void ieee_plus_unsigned(jit_func_t *func, jit_anchor_t *anchor,
                               jit_scalar_t *args, tlab_t *tlab)
{
   const int lsize = args[3].integer ^ (args[3].integer >> 63);
   const int rsize = args[6].integer ^ (args[6].integer >> 63);
   uint8_t *left = args[1].pointer;
   uint8_t *right = args[4].pointer;

   const int size = MAX(lsize, rsize);

   if (lsize == 0 || rsize == 0) {
      args[0].pointer = NULL;
      args[1].integer = 0;
      args[2].integer = -1;
   }
   else {
      left = __resize_unsigned(tlab, left, lsize, size);
      right = __resize_unsigned(tlab, right, rsize, size);

      left = __to_01(tlab, left, size, _X);
      right = __to_01(tlab, right, size, _X);

      if (left[0] == _X)
         args[0].pointer = left;
      else if (right[0] == _X)
         args[0].pointer = right;
      else {
         uint8_t *result = __tlab_alloc(tlab, size);
         __ieee_packed_add(left, right, size, 0, result);
         args[0].pointer = result;
      }

      args[1].integer = size - 1;
      args[2].integer = ~size;
   }
}

static void ieee_plus_signed(jit_func_t *func, jit_anchor_t *anchor,
                             jit_scalar_t *args, tlab_t *tlab)
{
   const int lsize = args[3].integer ^ (args[3].integer >> 63);
   const int rsize = args[6].integer ^ (args[6].integer >> 63);
   uint8_t *left = args[1].pointer;
   uint8_t *right = args[4].pointer;

   const int size = MAX(lsize, rsize);

   if (lsize == 0 || rsize == 0) {
      args[0].pointer = NULL;
      args[1].integer = 0;
      args[2].integer = -1;
   }
   else {
      left = __resize_signed(tlab, left, lsize, size);
      right = __resize_signed(tlab, right, rsize, size);

      left = __to_01(tlab, left, size, _X);
      right = __to_01(tlab, right, size, _X);

      if (left[0] == _X)
         args[0].pointer = left;
      else if (right[0] == _X)
         args[0].pointer = right;
      else {
         uint8_t *result = __tlab_alloc(tlab, size);
         __ieee_packed_add(left, right, size, 0, result);
         args[0].pointer = result;
      }

      args[1].integer = size - 1;
      args[2].integer = ~size;
   }
}

static void ieee_minus_unsigned(jit_func_t *func, jit_anchor_t *anchor,
                                jit_scalar_t *args, tlab_t *tlab)
{
   const int lsize = ffi_abs_length(args[3].integer);
   const int rsize = ffi_abs_length(args[6].integer);
   uint8_t *left = args[1].pointer;
   uint8_t *right = args[4].pointer;

   const int size = MAX(lsize, rsize);

   if (lsize == 0 || rsize == 0) {
      args[0].pointer = NULL;
      args[1].integer = 0;
      args[2].integer = -1;
   }
   else {
      left = __resize_unsigned(tlab, left, lsize, size);
      right = __resize_unsigned(tlab, right, rsize, size);

      left = __to_01(tlab, left, size, _X);
      right = __to_01(tlab, right, size, _X);

      if (left[0] == _X)
         args[0].pointer = left;
      else if (right[0] == _X)
         args[0].pointer = right;
      else {
         uint8_t *result = __tlab_alloc(tlab, size);
         __invert_bits(right, size, result);
         __ieee_packed_add(left, result, size, 1, result);
         args[0].pointer = result;
      }

      args[1].integer = size - 1;
      args[2].integer = ~size;
   }
}

static void ieee_minus_signed(jit_func_t *func, jit_anchor_t *anchor,
                              jit_scalar_t *args, tlab_t *tlab)
{
   const int lsize = ffi_abs_length(args[3].integer);
   const int rsize = ffi_abs_length(args[6].integer);
   uint8_t *left = args[1].pointer;
   uint8_t *right = args[4].pointer;

   const int size = MAX(lsize, rsize);

   if (lsize == 0 || rsize == 0) {
      args[0].pointer = NULL;
      args[1].integer = 0;
      args[2].integer = -1;
   }
   else {
      left = __resize_signed(tlab, left, lsize, size);
      right = __resize_signed(tlab, right, rsize, size);

      left = __to_01(tlab, left, size, _X);
      right = __to_01(tlab, right, size, _X);

      if (left[0] == _X)
         args[0].pointer = left;
      else if (right[0] == _X)
         args[0].pointer = right;
      else {
         uint8_t *result = __tlab_alloc(tlab, size);
         __invert_bits(right, size, result);
         __ieee_packed_add(left, result, size, 1, result);
         args[0].pointer = result;
      }

      args[1].integer = size - 1;
      args[2].integer = ~size;
   }
}

static void ieee_mul_unsigned(jit_func_t *func, jit_anchor_t *anchor,
                              jit_scalar_t *args, tlab_t *tlab)
{
   const int lsize = ffi_abs_length(args[3].integer);
   const int rsize = ffi_abs_length(args[6].integer);
   uint8_t *left = args[1].pointer;
   uint8_t *right = args[4].pointer;

   const int size = lsize + rsize;

   if (lsize == 0 || rsize == 0) {
      args[0].pointer = NULL;
      args[1].integer = 0;
      args[2].integer = -1;
   }
   else {
      uint8_t *result = __tlab_alloc(tlab, size);
      const uint32_t mark = __tlab_mark(tlab);

      left = __to_01(tlab, left, lsize, _X);
      right = __to_01(tlab, right, rsize, _X);

      if (left[0] == _X || right[0] == _X)
         memset(result, _X, size);
      else {
         memset(result, _0, size);

         uint8_t *adval = __resize_unsigned(tlab, right, rsize, size);
         for (int i = lsize - 1, shift = 0; i >= 0; i--, shift++) {
            if (left[i] == _1) {
               // Delay left-shift until value needed
               memmove(adval, adval + shift, size - shift);
               memset(adval + size - shift, _0, shift);
               shift = 0;

               __ieee_packed_add(result, adval, size, 0, result);
            }
         }
      }

      __tlab_restore(tlab, mark);

      args[0].pointer = result;
      args[1].integer = size - 1;
      args[2].integer = ~size;
   }
}

static void ieee_mul_signed(jit_func_t *func, jit_anchor_t *anchor,
                            jit_scalar_t *args, tlab_t *tlab)
{
   const int lsize = ffi_abs_length(args[3].integer);
   const int rsize = ffi_abs_length(args[6].integer);
   uint8_t *left = args[1].pointer;
   uint8_t *right = args[4].pointer;

   const int size = lsize + rsize;

   if (lsize == 0 || rsize == 0) {
      args[0].pointer = NULL;
      args[1].integer = 0;
      args[2].integer = -1;
   }
   else {
      uint8_t *result = __tlab_alloc(tlab, size);
      const uint32_t mark = __tlab_mark(tlab);

      left = __to_01(tlab, left, lsize, _X);
      right = __to_01(tlab, right, rsize, _X);

      if (left[0] == _X || right[0] == _X)
         memset(result, _X, size);
      else {
         memset(result, _0, size);

         uint8_t *adval = __resize_signed(tlab, right, rsize, size);
         for (int i = lsize - 1, shift = 0; i >= 0; i--, shift++) {
            if (left[i] == _1) {
               // Delay left-shift until value needed
               memmove(adval, adval + shift, size - shift);
               memset(adval + size - shift, _0, shift);
               shift = 0;

               if (i == 0)
                  __invert_bits(adval, size, adval);

               __ieee_packed_add(result, adval, size, i == 0, result);
            }
         }
      }

      __tlab_restore(tlab, mark);

      args[0].pointer = result;
      args[1].integer = size - 1;
      args[2].integer = ~size;
   }
}

static void ieee_and_vector(jit_func_t *func, jit_anchor_t *anchor,
                            jit_scalar_t *args, tlab_t *tlab)
{
   const int lsize = args[3].integer ^ (args[3].integer >> 63);
   const int rsize = args[6].integer ^ (args[6].integer >> 63);
   uint8_t *left = args[1].pointer;
   uint8_t *right = args[4].pointer;

   if (unlikely(lsize != rsize))
      __ieee_failure(func, anchor, "STD_LOGIC_1164.\"and\": arguments of "
                     "overloaded 'and' operator are not of the same length");
   else {
      uint8_t *result = __tlab_alloc(tlab, lsize);

      int pos = 0;
      for (; pos + 7 < lsize; pos += 8) {
         const uint64_t l64 = unaligned_load(left + pos, uint64_t);
         const uint64_t r64 = unaligned_load(right + pos, uint64_t);
         if (!IS_01(l64) || !IS_01(r64))
            goto slow_path;

         const uint64_t or = (l64 & r64) | UINT64_C(0x0202020202020202);
         memcpy(result + pos, &or, sizeof(or));
      }

      for (; pos < lsize; pos++) {
         const uint8_t l8 = left[pos];
         const uint8_t r8 = right[pos];
         if (!IS_01(l8) || !IS_01(r8))
            goto slow_path;

         result[pos] = (l8 & r8) | 0x02;
      }

   slow_path:
      for (; pos < lsize; pos++)
         result[pos] = and_table[left[pos]][right[pos]];

      args[0].pointer = result;
      args[1].integer = 1;
      args[2].integer = lsize;
   }
}

static void ieee_or_vector(jit_func_t *func, jit_anchor_t *anchor,
                           jit_scalar_t *args, tlab_t *tlab)
{
   const int lsize = args[3].integer ^ (args[3].integer >> 63);
   const int rsize = args[6].integer ^ (args[6].integer >> 63);
   uint8_t *left = args[1].pointer;
   uint8_t *right = args[4].pointer;

   if (unlikely(lsize != rsize))
      __ieee_failure(func, anchor, "STD_LOGIC_1164.\"or\": arguments of "
                     "overloaded 'or' operator are not of the same length");
   else {
      uint8_t *result = __tlab_alloc(tlab, lsize);

      int pos = 0;
      for (; pos + 7 < lsize; pos += 8) {
         const uint64_t l64 = unaligned_load(left + pos, uint64_t);
         const uint64_t r64 = unaligned_load(right + pos, uint64_t);
         if (!IS_01(l64) || !IS_01(r64))
            goto slow_path;

         const uint64_t or = (l64 | r64) | UINT64_C(0x0202020202020202);
         memcpy(result + pos, &or, sizeof(or));
      }

      for (; pos < lsize; pos++) {
         const uint8_t l8 = left[pos];
         const uint8_t r8 = right[pos];
         if (!IS_01(l8) || !IS_01(r8))
            goto slow_path;

         result[pos] = (l8 | r8) | 0x02;
      }

   slow_path:
      for (; pos < lsize; pos++)
         result[pos] = or_table[left[pos]][right[pos]];

      args[0].pointer = result;
      args[1].integer = 1;
      args[2].integer = lsize;
   }
}

static void std_xor_vector(jit_func_t *func, jit_anchor_t *anchor,
                           jit_scalar_t *args, tlab_t *tlab)
{
   const int lsize = args[3].integer ^ (args[3].integer >> 63);
   const int rsize = args[6].integer ^ (args[6].integer >> 63);
   uint8_t *left = args[1].pointer;
   uint8_t *right = args[4].pointer;

   if (unlikely(lsize != rsize))
      __ieee_failure(func, anchor, "STD_LOGIC_1164.\"xor\": arguments of "
                     "overloaded 'xor' operator are not of the same length");
   else {
      uint8_t *result = __tlab_alloc(tlab, lsize);

      int pos = 0;
      for (; pos + 7 < lsize; pos += 8) {
         const uint64_t l64 = unaligned_load(left + pos, uint64_t);
         const uint64_t r64 = unaligned_load(right + pos, uint64_t);
         if (!IS_01(l64) || !IS_01(r64))
            goto slow_path;

         const uint64_t xor = (l64 ^ r64) | UINT64_C(0x0202020202020202);
         memcpy(result + pos, &xor, sizeof(xor));
      }

      for (; pos < lsize; pos++) {
         const uint8_t l8 = left[pos];
         const uint8_t r8 = right[pos];
         if (!IS_01(l8) || !IS_01(r8))
            goto slow_path;

         result[pos] = (l8 ^ r8) | 0x02;
      }

   slow_path:
      for (; pos < lsize; pos++)
         result[pos] = xor_table[left[pos]][right[pos]];

      args[0].pointer = result;
      args[1].integer = 1;
      args[2].integer = lsize;
   }
}

static void ieee_to_unsigned(jit_func_t *func, jit_anchor_t *anchor,
                             jit_scalar_t *args, tlab_t *tlab)
{
   const int64_t size = args[2].integer;
   uint64_t arg = args[1].integer;

   if (size < 1) {
      args[0].pointer = NULL;
      args[1].integer = 0;
      args[2].integer = -1;
   }
   else {
      const int roundup = (size + 7) & ~7;
      uint8_t *result = __tlab_alloc(tlab, roundup), last = 0;
      for (int pos = roundup - 8; pos >= 0; pos -= 8, arg >>= 8)
         __spread_bits(result + pos, (last = (arg & 0xff)));

      const uint8_t spill_mask = ~((1 << (8 - roundup + size)) - 1);
      if (unlikely(arg != 0 || (last & spill_mask)))
         __ieee_warn(func, anchor, "NUMERIC_STD.TO_UNSIGNED: vector truncated");

      args[0].pointer = result + roundup - size;
      args[1].integer = size - 1;
      args[2].integer = ~size;
   }
}

static void ieee_to_signed(jit_func_t *func, jit_anchor_t *anchor,
                           jit_scalar_t *args, tlab_t *tlab)
{
   const int64_t size = args[2].integer;
   int64_t arg = args[1].integer;

   if (size < 1) {
      args[0].pointer = NULL;
      args[1].integer = 0;
      args[2].integer = -1;
   }
   else {
      const int roundup = (size + 7) & ~7;
      uint8_t *result = __tlab_alloc(tlab, roundup), last = 0;
      for (int pos = roundup - 8; pos >= 0; pos -= 8, arg >>= 8)
         __spread_bits(result + pos, (last = (arg & 0xff)));

      const uint8_t spill_mask = ~((1 << (8 - roundup + size)) - 1);
      if (unlikely((arg != 0 && arg != -1)
                   || (arg == 0 && result[roundup - size] == _1)
                   || (arg == -1 && result[roundup - size] == _0)
                   || (arg == 0 && (last & spill_mask) != 0)
                   || (arg == -1 && (last & spill_mask) != spill_mask)))
         __ieee_warn(func, anchor, "NUMERIC_STD.TO_SIGNED: vector truncated");

      args[0].pointer = result + roundup - size;
      args[1].integer = size - 1;
      args[2].integer = ~size;
   }
}

static void byte_vector_equal(jit_func_t *func, jit_anchor_t *anchor,
                              jit_scalar_t *args, tlab_t *tlab)
{
   const int lsize = ffi_abs_length(args[3].integer);
   const int rsize = ffi_abs_length(args[6].integer);
   uint8_t *left = args[1].pointer;
   uint8_t *right = args[4].pointer;

   args[0].integer = 0;

   if (lsize != rsize)
      return;

   int size = lsize;
   if (likely(size <= 128)) {
      for (; size > 7; size -= 8, left += 8, right += 8) {
         const uint64_t l64 = unaligned_load(left, uint64_t);
         const uint64_t r64 = unaligned_load(right, uint64_t);
         if (l64 != r64)
            return;
      }

      for (; size > 3; size -= 4, left += 4, right += 4) {
         const uint32_t l32 = unaligned_load(left, uint32_t);
         const uint32_t r32 = unaligned_load(right, uint32_t);
         if (l32 != r32)
            return;
      }

      switch (size) {
      case 3: if (*left++ != *right++) return;
      case 2: if (*left++ != *right++) return;
      case 1: if (*left++ != *right++) return;
      }

      args[0].integer = 1;
   }
   else
      args[0].integer = (memcmp(left, right, size) == 0);
}

static void ieee_math_sin(jit_func_t *func, jit_anchor_t *anchor,
                          jit_scalar_t *args, tlab_t *tlab)
{
   args[0].real = sin(args[1].real);
}

static void ieee_math_cos(jit_func_t *func, jit_anchor_t *anchor,
                          jit_scalar_t *args, tlab_t *tlab)
{
   args[0].real = cos(args[1].real);
}

static void ieee_math_log2(jit_func_t *func, jit_anchor_t *anchor,
                          jit_scalar_t *args, tlab_t *tlab)
{
   args[0].real = log2(args[1].real);
}

#define UU "36IEEE.NUMERIC_STD.UNRESOLVED_UNSIGNED"
#define U "25IEEE.NUMERIC_STD.UNSIGNED"
#define US "34IEEE.NUMERIC_STD.UNRESOLVED_SIGNED"
#define S "23IEEE.NUMERIC_STD.SIGNED"
#define NS "IEEE.NUMERIC_STD."
#define SL "IEEE.STD_LOGIC_1164."
#define MR "IEEE.MATH_REAL."
#define ST "STD.STANDARD."

static jit_intrinsic_t intrinsic_list[] = {
   { NS "ADD_UNSIGNED(" U U "L)" U, ieee_add_unsigned },
   { NS "ADD_UNSIGNED(" UU UU "L)" UU, ieee_add_unsigned },
   { NS "\"+\"(" U U ")" U, ieee_plus_unsigned },
   { NS "\"+\"(" UU UU ")" UU, ieee_plus_unsigned },
   { NS "\"+\"(" S S ")" S, ieee_plus_signed },
   { NS "\"+\"(" US US ")" US, ieee_plus_signed },
   { NS "\"-\"(" U U ")" U, ieee_minus_unsigned },
   { NS "\"-\"(" UU UU ")" UU, ieee_minus_unsigned },
   { NS "\"-\"(" S S ")" S, ieee_minus_signed },
   { NS "\"-\"(" US US ")" US, ieee_minus_signed },
   { NS "\"*\"(" U U ")" U, ieee_mul_unsigned },
   { NS "\"*\"(" UU UU ")" UU, ieee_mul_unsigned },
   { NS "\"*\"(" S S ")" S, ieee_mul_signed },
   { NS "\"*\"(" US US ")" US, ieee_mul_signed },
#ifdef HAVE_SSE41
   { SL "TO_X01(V)V", std_to_x01_sse41, CPU_SSE41 },
   { SL "TO_X01(Y)Y", std_to_x01_sse41, CPU_SSE41 },
#endif
   { SL "TO_X01(V)V", std_to_x01 },
   { SL "TO_X01(Y)Y", std_to_x01 },
   { NS "TO_01(" U "L)" U, ieee_to_01 },
   { NS "TO_01(" UU "U)" UU, ieee_to_01 },
   { NS "TO_01(" S "L)" U, ieee_to_01 },
   { NS "TO_01(" US "U)" UU, ieee_to_01 },
   { NS "RESIZE(" U "N)" U, ieee_resize_unsigned },
   { NS "RESIZE(" UU "N)" UU, ieee_resize_unsigned },
   { NS "RESIZE(" S "N)" S, ieee_resize_signed },
   { NS "RESIZE(" US "N)" US, ieee_resize_signed },
   { SL "\"and\"(VV)V", ieee_and_vector },
   { SL "\"and\"(YY)Y", ieee_and_vector },
   { SL "\"or\"(VV)V", ieee_or_vector },
   { SL "\"or\"(YY)Y", ieee_or_vector },
   { SL "\"xor\"(VV)V", std_xor_vector },
   { SL "\"xor\"(YY)Y", std_xor_vector },
   { NS "TO_UNSIGNED(NN)" U, ieee_to_unsigned },
   { NS "TO_UNSIGNED(NN)" UU, ieee_to_unsigned },
   { NS "TO_SIGNED(IN)" S, ieee_to_signed },
   { NS "TO_SIGNED(IN)" US, ieee_to_signed },
   { SL "\"=\"(VV)B", byte_vector_equal },
   { SL "\"=\"(YY)B", byte_vector_equal },
   { ST "\"=\"(QQ)B", byte_vector_equal },
   { ST "\"=\"(SS)B", byte_vector_equal },
   { MR "SIN(R)R", ieee_math_sin },
   { MR "COS(R)R", ieee_math_cos },
   { MR "LOG2(R)R", ieee_math_log2 },
   { NULL, NULL }
};

jit_entry_fn_t jit_bind_intrinsic(ident_t name)
{
   INIT_ONCE({
         const bool want_intrinsics = !!opt_get_int(OPT_JIT_INTRINSICS);
         const bool want_vector = !!opt_get_int(OPT_VECTOR_INTRINSICS);

         cpu_feature_t mask = 0;
#if HAVE_AVX2
         if (want_vector && __builtin_cpu_supports("avx2"))
            mask |= CPU_AVX2;
#endif
#ifdef HAVE_SSE41
         if (want_vector && __builtin_cpu_supports("sse4.1"))
            mask |= CPU_SSE41;
#endif

         for (jit_intrinsic_t *it = intrinsic_list; it->name; it++) {
            if (it->feature && !(it->feature & mask))
               continue;
            else if (want_intrinsics)
               it->ident = ident_new(it->name);
         }
      });

   for (const jit_intrinsic_t *it = intrinsic_list; it->name; it++) {
      if (it->ident == name)
         return it->entry;
   }

   return NULL;
}
