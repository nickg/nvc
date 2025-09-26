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

#include "util.h"
#include "diag.h"
#include "fbuf.h"
#include "hash.h"
#include "vlog/vlog-number.h"
#include "thread.h"

#include <assert.h>
#include <inttypes.h>
#include <string.h>
#include <stdlib.h>

typedef enum {
   RADIX_BIN = 2,
   RADIX_OCT = 8,
   RADIX_DEC = 10,
   RADIX_HEX = 16,
} vlog_radix_t;

typedef struct _bignum {
   unsigned width;
   bool     issigned;
   bool     isscratch;
   uint64_t words[];
} bignum_t;

#define BIGNUM_WORDS(w) (((w) + 63) / 64)

__attribute__((always_inline))
static inline int bignum_words(const bignum_t *bn)
{
   return BIGNUM_WORDS(bn->width);
}

__attribute__((always_inline))
static inline uint64_t *bignum_abits(bignum_t *bn)
{
   return bn->words;
}

__attribute__((always_inline))
static inline uint64_t *bignum_bbits(bignum_t *bn)
{
   return bn->words + bignum_words(bn);
}

__attribute__((always_inline))
static inline int bignum_abit(bignum_t *bn, unsigned n)
{
   assert(n < bn->width);
   return (bignum_abits(bn)[n / 64] >> (n % 64)) & 1;
}

__attribute__((always_inline))
static inline int bignum_bbit(bignum_t *bn, unsigned n)
{
   assert(n < bn->width);
   return (bignum_bbits(bn)[n / 64] >> (n % 64)) & 1;
}

static inline void bignum_set_abit(bignum_t *bn, unsigned n, int b)
{
   if (n < bn->width)
      bignum_abits(bn)[n / 64] |= (uint64_t)(b & 1) << (n % 64);
}

static inline void bignum_set_bbit(bignum_t *bn, unsigned n, int b)
{
   if (n < bn->width)
      bignum_bbits(bn)[n / 64] |= (uint64_t)(b & 1) << (n % 64);
}

static inline void bignum_set_nibble(bignum_t *bn, unsigned n, int value)
{
   for (int i = 0; i < 4; i++)
      bignum_set_abit(bn, n + i, value >> i);
}

static uint32_t bignum_hash(const void *p)
{
   const bignum_t *bn = p;

   uint64_t hash = knuth_hash(bn->width) ^ bn->issigned;

   for (int i = 0; i < bignum_words(bn) * 2; i++)
      hash ^= mix_bits_64(bn->words[i]);

   return hash;
}

static bool bignum_cmp(const void *pa, const void *pb)
{
   const bignum_t *a = pa, *b = pb;

   if (a->width != b->width || a->issigned != b->issigned)
      return false;

   for (int i = 0; i < bignum_words(a) * 2; i++) {
      if (a->words[i] != b->words[i])
         return false;
   }

   return true;
}

static void bignum_scratch(int width, bool issigned, int count,
                           bignum_t *out[count])
{
   static __thread void *cache = NULL;
   static __thread size_t cachesz = 0;

   const int nwords = MAX(1, BIGNUM_WORDS(width));
   const size_t size = sizeof(bignum_t) + nwords * 2 * sizeof(uint64_t);
   const size_t total_size = count * size;

   if (total_size > cachesz) {
      cache = xrealloc(cache, total_size);
      cachesz = total_size;
   }

   ASAN_POISON(cache + total_size, cachesz - total_size);
   ASAN_UNPOISON(cache, total_size);

   for (int i = 0; i < count; i++) {
      bignum_t *big = out[i] = cache + i * size;
      big->width     = width;
      big->issigned  = issigned;
      big->isscratch = true;

      for (int j = 0; j < nwords * 2; j++)
         big->words[j] = 0;
   }
}

static void bignum_for_binary(number_t in_left, number_t in_right,
                              bignum_t **out_left, bignum_t **out_right)
{
   const int width = MAX(in_left.big->width, in_right.big->width);
   const bool issigned = in_left.big->issigned && in_right.big->issigned;

   bignum_t *big[2];
   bignum_scratch(width, issigned, 2, big);

   const int out_nwords = BIGNUM_WORDS(width);

   const number_t in[2] = { in_left, in_right };

   // XXX: sign extend
   for (int i = 0; i < 2; i++) {
      const int in_nwords = BIGNUM_WORDS(in[i].big->width);

      for (int j = 0; j < in_nwords; j++) {
         big[i]->words[j] = in[i].big->words[j];
         big[i]->words[j + out_nwords] = in[i].big->words[j + in_nwords];
      }

      for (int j = in_nwords; j < out_nwords; j++) {
         big[i]->words[j] = 0;
         big[i]->words[j + out_nwords] = 0;
      }
   }

   *out_left = big[0];
   *out_right = big[1];
}

static bignum_t *bignum_for_unary(number_t in)
{
   bignum_t *big;
   bignum_scratch(in.big->width, in.big->issigned, 1, &big);

   const int nwords = BIGNUM_WORDS(in.big->width);
   memcpy(big->words, in.big->words, nwords * 2);

   return big;
}

static bool bignum_is_defined(bignum_t *bn)
{
   const int nwords = bignum_words(bn);
   const uint64_t *bbits = bignum_bbits(bn);

   for (int i = 0; i < nwords; i++) {
      if (bbits[i] != 0)
         return false;
   }

   return true;
}

static number_t number_intern(const bignum_t *in)
{
   static mem_pool_t *pool = NULL;
   static ghash_t *hash = NULL;

   INIT_ONCE(
      pool = pool_new();
      hash = ghash_new(32, bignum_hash, bignum_cmp);
   );

   assert(in->isscratch);

   bignum_t *bn = ghash_get(hash, in);
   if (bn == NULL) {
      const int nwords = bignum_words(in) * 2;
      bn = pool_malloc_flex(pool, sizeof(bignum_t), nwords, sizeof(uint64_t));
      bn->width     = in->width;
      bn->issigned  = in->issigned;
      bn->isscratch = false;

      for (int i = 0; i < nwords; i++)
         bn->words[i] = in->words[i];

      ghash_put(hash, bn, bn);
   }

   return (number_t){ .big = bn };
}

number_t number_new(const char *str, const loc_t *loc)
{
   int width = 32;
   bool issigned = true;
   const char *p = str;
   if (*p == '"')
      width = 8 * (strlen(str) - 2);
   else {
      const char *tick = strchr(str, '\'');
      if (tick == str)
         p++;
      else if (tick != NULL) {
         char *eptr;
         width = strtol(str, &eptr, 10);

         p = eptr;
         while (isspace_iso88591(*p)) p++;
         assert(p == tick);
         p++;
         issigned = false;
      }

      if (*p == 's') {
         issigned = true;
         p++;
      }
   }

   bignum_t *result;
   bignum_scratch(width, issigned, 1, &result);

   vlog_radix_t radix = RADIX_DEC;
   switch (*p) {
   case 'B':
   case 'b': radix = RADIX_BIN; p++; break;
   case 'O':
   case 'o': radix = RADIX_OCT; p++; break;
   case 'H':
   case 'h': radix = RADIX_HEX; p++; break;
   case 'D':
   case 'd': radix = RADIX_DEC; p++; break;
   default: result->issigned = true; break;
   }

   if (*p == '_')
      error_at(loc, "number cannot start with an underscore");

   // Skip optional spaces after the radix
   while (isspace_iso88591(*p))
      p++;

   if (radix == RADIX_DEC) {
      bool is_x_digit = false;
      switch (*p) {
      case 'X':
      case 'x':
         is_x_digit = true;
      case '?':
      case 'Z':
      case 'z':
         p++;
         for (int bit = 0; bit < width; bit++) {
            if (is_x_digit)
               bignum_set_abit(result, bit, 1);
            bignum_set_bbit(result, bit, 1);
         }
         if (*p == '_')
            p++;
         if (*p != '\0')
            error_at(loc, "invalid character '%c' in decimal number %s",
                     *p, str);
         break;
      default:
         {
            uint64_t ten_small[1], *ten = ten_small;
            uint64_t digit_small[1], *digit = digit_small;

            if (width > 64) {
               ten = xcalloc_array(BIGNUM_WORDS(width), sizeof(uint64_t));
               digit = xcalloc_array(BIGNUM_WORDS(width), sizeof(uint64_t));
            }

            for (; *p; p++) {
               switch (*p) {
               case '0'...'9':
                  {
                     ten[0] = 10;
                     digit[0] = *p - '0';

                     vec2_mul(width, result->words, ten);
                     vec2_add(width, result->words, digit);
                  }
                  break;
               case '_':
                  continue;
               default:
                  error_at(loc, "invalid character '%c' in decimal number %s",
                           *p, str);
               }
            }

            if (width > 64) {
               free(ten);
               free(digit);
            }
         }
      }
   }
   else {
      const char *start = p;
      for (; *(p + 1); p++);

      int bit = 0;
      for (; p >= start; p--) {
         switch (radix) {
         case RADIX_BIN:
            {
               switch (*p) {
               case '0':
                  break;
               case '1':
                  bignum_set_abit(result, bit, 1);
                  break;
               case 'X':
               case 'x':
                  bignum_set_abit(result, bit, 1);
                  bignum_set_bbit(result, bit, 1);
                  break;
               case '?':
               case 'Z':
               case 'z':
                  bignum_set_bbit(result, bit, 1);
                  break;
               case '_':
                  continue;
               default:
                  error_at(loc, "invalid character '%c' in binary number %s",
                           *p, str);
               }

               if (bit >= width) {
                  warn_at(loc, "excess digits in binary constant %s", str);
                  return number_intern(result);
               }

               bit++;
            }
            break;

         case RADIX_OCT:
            {
               switch (*p) {
               case 'X':
               case 'x':
                  for (int i = 0; i < 3; i++) {
                     bignum_set_abit(result, bit + i, 1);
                     bignum_set_bbit(result, bit + i, 1);
                  }
                  break;
               case '?':
               case 'Z':
               case 'z':
                  for (int i = 0; i < 3; i++)
                     bignum_set_bbit(result, bit + i, 1);
                  break;
               case '0'...'7':
                  for (int i = 0; i < 3; i++)
                     bignum_set_abit(result, bit + i, (*p - '0') >> i);
                  break;
               case '_':
                  continue;
               default:
                  error_at(loc, "invalid character '%c' in octal number %s",
                           *p, str);
               }

               if (bit >= width) {
                  warn_at(loc, "excess digits in octal constant %s", str);
                  return number_intern(result);
               }

               bit += 3;
            }
            break;

         case RADIX_HEX:
            {
               switch (*p) {
               case 'X':
               case 'x':
                  for (int i = 0; i < 4; i++) {
                     bignum_set_abit(result, bit + i, 1);
                     bignum_set_bbit(result, bit + i, 1);
                  }
                  break;
               case '?':
               case 'Z':
               case 'z':
                  for (int i = 0; i < 4; i++)
                     bignum_set_bbit(result, bit + i, 1);
                  break;
               case '0'...'9':
                  bignum_set_nibble(result, bit, *p - '0');
                  break;
               case 'a'...'f':
                  bignum_set_nibble(result, bit, 10 + *p - 'a');
                  break;
               case 'A'...'F':
                  bignum_set_nibble(result, bit, 10 + *p - 'A');
                  break;
               case '_':
                  continue;
               default:
                  error_at(loc, "invalid character '%c' in hex number %s",
                           *p, str);
               }

               if (bit >= width) {
                  warn_at(loc, "excess digits in hex constant %s", str);
                  return number_intern(result);
               }

               bit += 4;
            }
            break;

         default:
            should_not_reach_here();
         }
      }

      for (; bit < width; bit++) {
         switch (*start) {
         case 'x':
         case 'X':
            bignum_set_abit(result, bit, 1);
            bignum_set_bbit(result, bit, 1);
            break;
         case 'z':
         case 'Z':
            bignum_set_abit(result, bit, 0);
            bignum_set_bbit(result, bit, 1);
            break;
         default:
            break;
         }
      }
   }

   return number_intern(result);
}

number_t number_from_string(const char *bytes, size_t len)
{
   const int width = 8 * len;
   bignum_t *result;
   bignum_scratch(width, false, 1, &result);

   const char *p = bytes;
   for (int bit = width - 8; p < bytes + len; p++, bit -= 8) {
      const uint8_t byte = *p;
      bignum_set_nibble(result, bit, byte);
      bignum_set_nibble(result, bit + 4, byte >> 4);
   }

   return number_intern(result);
}

number_t number_from_int(int64_t value)
{
   int width = 32;
   if (value < INT32_MIN || value > INT32_MAX)
      width = ilog2(llabs(value)) + 1;

   bignum_t *result;
   bignum_scratch(width, true, 1, &result);

   bignum_abits(result)[0] = value;
   bignum_bbits(result)[0] = 0;

   return number_intern(result);
}

number_t number_from_bool(bool value)
{
   bignum_t *result;
   bignum_scratch(1, false, 1, &result);

   bignum_abits(result)[0] = value;
   bignum_bbits(result)[0] = 0;

   return number_intern(result);
}

number_t number_from_logic(vlog_logic_t value)
{
   bignum_t *result;
   bignum_scratch(1, false, 1, &result);

   bignum_abits(result)[0] = value & 1;
   bignum_bbits(result)[0] = value >> 1;

   return number_intern(result);
}

void number_print(number_t val, text_buf_t *tb)
{
   if (number_is_defined(val)) {
      if (val.big->width == 32 && val.big->issigned) {
         tb_printf(tb, "%"PRIi64, number_integer(val));
         return;
      }
      else if (val.big->width > 1 && val.big->width <= 32) {
         tb_printf(tb, "%u'%sd%"PRIu64, val.big->width,
                   val.big->issigned ? "s" : "", number_integer(val));
         return;
      }
   }

   tb_printf(tb, "%u'b", number_width(val));

   static const char map[] = "01zx";

   if (val.big->width > 0) {
      const vlog_logic_t bit0 = number_bit(val, val.big->width - 1);
      if (bit0 == LOGIC_X || bit0 == LOGIC_Z) {
         bool all_undef = true;
         for (int i = 0; all_undef && i < val.big->width - 1; i++)
            all_undef &= number_bit(val, i) == bit0;

         if (all_undef) {
            tb_append(tb, map[bit0]);
            return;
         }
      }
   }

   bool leading = true;
   for (int i = val.big->width - 1; i >= 0; i--) {
      const vlog_logic_t bit = number_bit(val, i);
      if (leading && bit != LOGIC_0)
         leading = false;
      else if (leading)
         continue;
      tb_append(tb, map[bit]);
   }

   if (leading)
      tb_append(tb, '0');
}

bool number_is_defined(number_t val)
{
   return bignum_is_defined(val.big);
}

int64_t number_integer(number_t val)
{
   assert(number_width(val) <= 64);
   assert(number_is_defined(val));

   const uint64_t w0 = bignum_abits(val.big)[0];
   if (val.big->issigned)
      return (int64_t)(w0 << (64 - val.big->width)) >> (64 - val.big->width);
   else
      return w0;
}

unsigned number_width(number_t val)
{
   return val.big->width;
}

vlog_logic_t number_bit(number_t val, unsigned n)
{
   return bignum_abit(val.big, n) | (bignum_bbit(val.big, n) << 1);
}

uint8_t number_byte(number_t val, unsigned n)
{
   assert(number_is_defined(val));
   assert(n < bignum_words(val.big) * 8);

   const uint64_t *abits = bignum_abits(val.big);
   return (abits[n / 8] >> (n * 8) % 64) & 0xff;
}

uint32_t number_hash(number_t n)
{
   return bignum_hash(n.big);
}

bool number_equal(number_t a, number_t b)
{
   if (a.big->width != b.big->width)
      return false;
   else if (a.big->issigned != b.big->issigned)
      return false;

   const int nwords = bignum_words(a.big);
   for (int i = 0; i < nwords; i++) {
      if (a.big->words[i] != b.big->words[i])
         return false;
   }

   return true;
}

bool number_truthy(number_t a)
{
   const int nwords = bignum_words(a.big);
   for (int i = 0; i < nwords; i++) {
      if (bignum_bbits(a.big)[i] != 0)
         return false;
      else if (bignum_abits(a.big)[i] != 0)
         return true;
   }

   return false;
}

bool number_signed(number_t a)
{
   return a.big->issigned;
}

void number_get(number_t val, const uint64_t **abits, const uint64_t **bbits)
{
   *abits = bignum_abits(val.big);
   *bbits = bignum_bbits(val.big);
}

void number_write(number_t val, fbuf_t *f)
{
   const int64_t width = val.big->width;  // Widen
   fbuf_put_int(f, val.big->issigned ? -width : width);

   const int nwords = bignum_words(val.big);
   write_raw(val.big->words, nwords * 2  * sizeof(uint64_t), f);
}

number_t number_read(fbuf_t *f)
{
   const int64_t enc = fbuf_get_int(f);
   const unsigned width = llabs(enc);
   const int nwords = BIGNUM_WORDS(width);

   bignum_t *result;
   bignum_scratch(width, enc < 0, 1, &result);

   read_raw(result->words, nwords * 2 * sizeof(uint64_t), f);

   return number_intern(result);
}

number_t number_add(number_t a, number_t b)
{
   bignum_t *left, *right;
   bignum_for_binary(a, b, &left, &right);

   assert(bignum_words(left) == 1);  // TODO

   bignum_abits(left)[0] += bignum_abits(right)[0];
   bignum_bbits(right)[0] |= bignum_bbits(right)[0];

   return number_intern(left);
}

number_t number_sub(number_t a, number_t b)
{
   bignum_t *left, *right;
   bignum_for_binary(a, b, &left, &right);

   assert(bignum_words(left) == 1);  // TODO

   bignum_abits(left)[0] -= bignum_abits(right)[0];
   bignum_bbits(left)[0] |= bignum_bbits(right)[0];

   return number_intern(left);
}

number_t number_mul(number_t a, number_t b)
{
   bignum_t *left, *right;
   bignum_for_binary(a, b, &left, &right);

   assert(bignum_words(left) == 1);  // TODO

   bignum_abits(left)[0] *= bignum_abits(right)[0];
   bignum_bbits(left)[0] |= bignum_bbits(right)[0];

   return number_intern(left);
}

number_t number_div(number_t a, number_t b)
{
   bignum_t *left, *right;
   bignum_for_binary(a, b, &left, &right);

   assert(bignum_words(left) == 1);  // TODO

   bignum_abits(left)[0] /= bignum_abits(right)[0];
   bignum_bbits(left)[0] |= bignum_bbits(right)[0];

   return number_intern(left);
}

number_t number_exp(number_t a, number_t b)
{
   bignum_t *left, *right;
   bignum_for_binary(a, b, &left, &right);

   vec4_exp(left->width, bignum_abits(left), bignum_bbits(left),
            bignum_abits(right), bignum_bbits(right));

   return number_intern(left);
}

number_t number_shl(number_t a, number_t b)
{
   bignum_t *left, *right;
   bignum_for_binary(a, b, &left, &right);

   vec2_shl(left->width, left->words, right->words);

   return number_intern(left);
}

number_t number_negate(number_t a)
{
   bignum_t *result = bignum_for_unary(a);

   vec2_neg(result->width, result->words);

   return number_intern(result);
}

number_t number_logical_equal(number_t a, number_t b)
{
   bignum_t *result;
   bignum_scratch(1, false, 1, &result);

   assert(bignum_words(a.big) == 1);  // TODO
   assert(bignum_words(b.big) == 1);  // TODO

   bignum_abits(result)[0] =
      bignum_abits(a.big)[0] == bignum_abits(b.big)[0];
   bignum_bbits(result)[0] =
      bignum_bbits(a.big)[0] | bignum_bbits(b.big)[0];

   return number_intern(result);
}

number_t number_not(number_t a)
{
   bignum_t *result = bignum_for_unary(a);

   assert(bignum_words(result) == 1);  // TODO

   bignum_abits(result)[0] = !bignum_abits(result)[0];

   return number_intern(result);
}

number_t number_greater(number_t a, number_t b)
{
   bignum_t *left, *right;
   bignum_for_binary(a, b, &left, &right);

   if (number_is_defined(a) && number_is_defined(b)) {
      int cmp;
      if (a.big->issigned && b.big->issigned)
         cmp = vec2_sgt(left->width, left->words, right->words);
      else
         cmp = vec2_gt(left->width, left->words, right->words);

      return number_from_bool(cmp);
   }
   else
      return number_from_logic(LOGIC_X);
}

number_t number_greater_equal(number_t a, number_t b)
{
   bignum_t *left, *right;
   bignum_for_binary(a, b, &left, &right);

   if (number_is_defined(a) && number_is_defined(b)) {
      int cmp;
      if (a.big->issigned && b.big->issigned)
         cmp = vec2_sge(left->width, left->words, right->words);
      else
         cmp = vec2_ge(left->width, left->words, right->words);

      return number_from_bool(cmp);
   }
   else
      return number_from_logic(LOGIC_X);
}

number_t number_less(number_t a, number_t b)
{
   bignum_t *left, *right;
   bignum_for_binary(a, b, &left, &right);

   if (number_is_defined(a) && number_is_defined(b)) {
      int cmp;
      if (a.big->issigned && b.big->issigned)
         cmp = vec2_slt(left->width, left->words, right->words);
      else
         cmp = vec2_lt(left->width, left->words, right->words);

      return number_from_bool(cmp);
   }
   else
      return number_from_logic(LOGIC_X);
}

number_t number_less_equal(number_t a, number_t b)
{
   bignum_t *left, *right;
   bignum_for_binary(a, b, &left, &right);

   if (number_is_defined(a) && number_is_defined(b)) {
      int cmp;
      if (a.big->issigned && b.big->issigned)
         cmp = vec2_sle(left->width, left->words, right->words);
      else
         cmp = vec2_le(left->width, left->words, right->words);

      return number_from_bool(cmp);
   }
   else
      return number_from_logic(LOGIC_X);
}

static void vec2_mask(int size, uint64_t *a)
{
   uint64_t mask = ~UINT64_C(0);
   if (size % 64 != 0) mask >>= 64 - size % 64;

   a[BIGNUM_WORDS(size) - 1] &= mask;
}

static bool vec2_is_zero(int size, const uint64_t *a)
{
   for (int i = 0; i < BIGNUM_WORDS(size); i++) {
      if (a[i] != 0)
         return false;
   }

   return true;
}

void vec2_add(int size, uint64_t *a, const uint64_t *b)
{
   uint64_t carry = 0;
   for (size_t i = 0; i < BIGNUM_WORDS(size); i++) {
      uint64_t sum = a[i] + b[i];
      uint64_t c1 = (sum < a[i]);
      uint64_t sum2 = sum + carry;
      uint64_t c2 = (sum2 < sum);
      a[i] = sum2;
      carry = (c1 | c2);
   }

   vec2_mask(size, a);
}

void vec2_sub(int size, uint64_t *a, const uint64_t *b)
{
    uint64_t borrow = 0;
    for (size_t i = 0; i < BIGNUM_WORDS(size); i++) {
        uint64_t bi = b[i] + borrow;
        uint64_t new_borrow = (bi < b[i]) || (a[i] < bi);
        uint64_t diff = a[i] - bi;
        a[i] = diff;
        borrow = new_borrow;
    }

    vec2_mask(size, a);
}

void vec2_mul(int size, uint64_t *a, const uint64_t *b)
{
   if (size > 0 && size <= 64)
      a[0] *= b[0];
   else if (size > 64) {
      const int n = BIGNUM_WORDS(size);
      uint64_t *tmp LOCAL = xcalloc_array(2 * n, sizeof(uint64_t));

      for (int i = 0; i < n; i++) {
         unsigned __int128 carry = 0;
         for (int j = 0; j < n; j++) {
            unsigned __int128 t =
               (unsigned __int128)tmp[i + j] +
               (unsigned __int128)a[i] * (unsigned __int128)b[j] +
               carry;
            tmp[i + j] = (uint64_t)t;
            carry = t >> 64;
         }
         int k = i + n;
         while (carry) {
            unsigned __int128 t = (unsigned __int128)tmp[k] + (uint64_t)carry;
            tmp[k] = (uint64_t)t;
            carry = t >> 64;
            k++;
         }
      }

      memcpy(a, tmp, n * sizeof(uint64_t));
      vec2_mask(size, a);
   }
}

static void vec2_divmod(int size, uint64_t *q, uint64_t *r,
                        const uint64_t *a, const uint64_t *b)
{
   uint64_t dividend[BIGNUM_WORDS(size)];
   memcpy(dividend, a, sizeof(dividend));

   uint64_t divisor[BIGNUM_WORDS(size)];
   memcpy(divisor, b, sizeof(divisor));

   uint64_t one[BIGNUM_WORDS(size)];
   memset(one, 0, sizeof(one));
   one[0] = 1;

   memset(q, 0, BIGNUM_WORDS(size) * sizeof(uint64_t));
   memset(r, 0, BIGNUM_WORDS(size) * sizeof(uint64_t));

   for (int i = size - 1; i >= 0; i--) {
      const int iword = i / 64;
      const uint64_t ibit = UINT64_C(1) << (i % 64);

      vec2_shl(size, r, one);
      if (dividend[iword] & ibit)
         r[0] |= 1;

      if (!vec2_gt(size, divisor, r)) {
         vec2_sub(size, r, divisor);
         q[iword] |= ibit;
      }
   }
}

void vec2_div(int size, uint64_t *a, const uint64_t *b)
{
   if (size > 0 && size <= 64)
      a[0] /= b[0];
   else {
      uint64_t q[BIGNUM_WORDS(size)], r[BIGNUM_WORDS(size)];
      vec2_divmod(size, q, r, a, b);
      memcpy(a, q, sizeof(q));
   }
}

void vec2_mod(int size, uint64_t *a, const uint64_t *b)
{
   if (size > 0 && size <= 64)
      a[0] %= b[0];
   else {
      uint64_t q[BIGNUM_WORDS(size)], r[BIGNUM_WORDS(size)];
      vec2_divmod(size, q, r, a, b);
      memcpy(a, r, sizeof(r));
   }
}

void vec2_exp(int size, uint64_t *a, const uint64_t *b)
{
   uint64_t base[BIGNUM_WORDS(size)];
   memcpy(base, a, sizeof(base));

   uint64_t exp[BIGNUM_WORDS(size)];
   memcpy(exp, b, sizeof(exp));

   uint64_t one[BIGNUM_WORDS(size)];
   memset(one, 0, sizeof(one));
   one[0] = 1;

   memset(a, 0, BIGNUM_WORDS(size));
   a[0] = 1;

   while (!vec2_is_zero(size, exp)) {
      if (exp[0] & 1)
         vec2_mul(size, a, base);
      vec2_mul(size, base, base);
      vec2_shr(size, exp, one);
   }

   vec2_mask(size, a);
}

static uint64_t get_shift_amount(int size, const uint64_t *a)
{
   for (int i = 1; i < BIGNUM_WORDS(size); i++) {
      if (a[i] != 0)
         return UINT64_MAX;
   }

   return a[0];
}

void vec2_shl(int size, uint64_t *a, const uint64_t *b)
{
   const int n = BIGNUM_WORDS(size);
   if (n == 0)
      return;

   const uint64_t k = get_shift_amount(size, b);
   if (k == 0) {
      vec2_mask(size, a);
      return;
   }

   if (k >= size) {
      for (int i = 0; i < n; i++)
         a[i] = 0;
      return;
   }

   const int s = k / 64;
   const int r = k % 64;

   if (s) {
      for (int i = n - 1; i >= 0; --i)
         a[i] = (i - s >= 0) ? a[i - s] : 0;
   }

   if (r) {
      uint64_t carry = 0;
      for (int i = s; i < n; ++i) {
         const uint64_t cur = a[i];
         a[i] = (cur << r) | carry;
         carry = cur >> (64 - r);
      }
   }

   for (int i = 0; i < s && i < n; ++i)
      a[i] = 0;

   vec2_mask(size, a);
}

void vec2_shr(int size, uint64_t *a, const uint64_t *b)
{
   const int n = BIGNUM_WORDS(size);
   if (n == 0)
      return;

   const uint64_t k = get_shift_amount(size, b);
   if (k == 0) {
      vec2_mask(size, a);
      return;
   }

   if (k >= size) {
      for (int i = 0; i < n; i++)
         a[i] = 0;
      return;
   }

   const int s = k / 64;
   const int r = k % 64;

   if (s) {
      for (int i = 0; i < n; i++)
         a[i] = (i + s < n) ? a[i + s] : 0;
   }

   if (r) {
      uint64_t carry = 0;
      for (int i = n - 1; i >= 0; --i) {
         const uint64_t cur = a[i];
         a[i] = (cur >> r) | carry;
         carry = cur << (64 - r);
      }
   }

   vec2_mask(size, a);
}

void vec2_neg(int size, uint64_t *a)
{
   if (size <= 64)
      a[0] = -a[0];
   else
      should_not_reach_here();   // TODO

   vec2_mask(size, a);
}

void vec2_inv(int size, uint64_t *a)
{
   for (int i = 0; i < BIGNUM_WORDS(size); i++)
      a[i] = ~a[i];

   vec2_mask(size, a);
}

int vec2_and1(int size, const uint64_t *a)
{
   int result = 1;
   for (int i = 0; i < size / 64; i++)
      result &= (a[i] == ~UINT64_C(0));

   if (size % 64 != 0)
      result &= (a[BIGNUM_WORDS(size) - 1] == ~UINT64_C(0) >> (-size & 63));

   return result;
}

int vec2_or1(int size, const uint64_t *a)
{
   int result = 0;
   for (int i = 0; i < BIGNUM_WORDS(size); i++)
      result |= (a[i] != 0);

   return result;
}

int vec2_gt(int size, const uint64_t *a, const uint64_t *b)
{
   if (size <= 64)
      return a[0] > b[0];
   else {
      for (int i = BIGNUM_WORDS(size) - 1; i >= 0; i--) {
         if (a[i] > b[i])
            return 1;
         else if (a[i] < b[i])
            return 0;
      }

      return 0;
   }
}

int vec2_sgt(int size, const uint64_t *a, const uint64_t *b)
{
   if (size <= 64)
      return (int64_t)a[0] > (int64_t)b[0];
   else
      should_not_reach_here();  // TODO
}

#define VEC2_CMP_OP(name, op)                                           \
   int vec2_##name(int size, const uint64_t *a, const uint64_t *b)      \
   {                                                                    \
      if (size <= 64)                                                   \
         return a[0] op b[0] ? LOGIC_1 : LOGIC_0;                       \
      else                                                              \
         should_not_reach_here();   /* TODO */                          \
   }                                                                    \
                                                                        \
   int vec2_s##name(int size, const uint64_t *a, const uint64_t *b)     \
   {                                                                    \
      if (size <= 64)                                                   \
         return (int64_t)a[0] op (int64_t)b[0] ? LOGIC_1 : LOGIC_0;     \
      else                                                              \
         should_not_reach_here();   /* TODO */                          \
   }

VEC2_CMP_OP(lt, <);
VEC2_CMP_OP(le, <=);
VEC2_CMP_OP(ge, >=);

static void vec4_make_undef(int size, uint64_t *a, uint64_t *b)
{
  for (int i = 0; i < BIGNUM_WORDS(size); i++) {
      b[i] = a[i] = ~UINT64_C(0);
      if ((i + 1) * 64 > size) {
         a[i] >>= 64 - size % 64;
         b[i] >>= 64 - size % 64;
      }
   }
}

static bool vec4_arith_defined(int size, uint64_t *a1, uint64_t *b1,
                               const uint64_t *a2, const uint64_t *b2)
{
   bool is_defined = true;
   for (int i = 0; i < BIGNUM_WORDS(size); i++)
      is_defined &= b1[i] == 0 && b2[i] == 0;

   if (is_defined)
      return true;

   vec4_make_undef(size, a1, b1);
   return false;
}

void vec4_add(int size, uint64_t *a1, uint64_t *b1, const uint64_t *a2,
              const uint64_t *b2)
{
   if (vec4_arith_defined(size, a1, b1, a2, b2))
      vec2_add(size, a1, a2);
}

void vec4_mul(int size, uint64_t *a1, uint64_t *b1, const uint64_t *a2,
              const uint64_t *b2)
{
   if (vec4_arith_defined(size, a1, b1, a2, b2))
      vec2_mul(size, a1, a2);
}

void vec4_div(int size, uint64_t *a1, uint64_t *b1, const uint64_t *a2,
              const uint64_t *b2)
{
   if (vec4_arith_defined(size, a1, b1, a2, b2)) {
      if (vec2_is_zero(size, a2))
         vec4_make_undef(size, a1, b1);
      else
         vec2_div(size, a1, a2);
   }
}

void vec4_mod(int size, uint64_t *a1, uint64_t *b1, const uint64_t *a2,
              const uint64_t *b2)
{
   if (vec4_arith_defined(size, a1, b1, a2, b2)) {
      if (vec2_is_zero(size, a2))
         vec4_make_undef(size, a1, b1);
      else
         vec2_mod(size, a1, a2);
   }
}

void vec4_exp(int size, uint64_t *a1, uint64_t *b1, const uint64_t *a2,
              const uint64_t *b2)
{
   if (vec4_arith_defined(size, a1, b1, a2, b2))
      vec2_exp(size, a1, a2);
}

void vec4_shl(int size, uint64_t *a1, uint64_t *b1, const uint64_t *a2,
              const uint64_t *b2)
{
   vec2_shl(size, a1, a2);
   vec2_shl(size, b1, b2);
}

void vec4_shr(int size, uint64_t *a1, uint64_t *b1, const uint64_t *a2,
              const uint64_t *b2)
{
   vec2_shr(size, a1, a2);
   vec2_shr(size, b1, b2);
}

void vec4_inv(int size, uint64_t *a, uint64_t *b)
{
   vec2_inv(size, a);
}
