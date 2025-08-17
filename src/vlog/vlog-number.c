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
   RADIX_STR = 0,
   RADIX_BIN = 2,
   RADIX_OCT = 8,
   RADIX_DEC = 10,
   RADIX_HEX = 16,
} vlog_radix_t;

typedef struct _bignum {
   unsigned width;
   bool     issigned;
   uint64_t words[];
} bignum_t;

#define BIGNUM_WORDS(w) (((w) + 63) / 64)

#ifdef DEBUG
#define SCRATCH_NUMBER number_t __attribute__((cleanup(_number_zap)))
#else
#define SCRATCH_NUMBER number_t
#endif

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

static number_t number_intern(number_t n)
{
   static mem_pool_t *pool = NULL;
   static ghash_t *hash = NULL;

   INIT_ONCE(
      pool = pool_new();
      hash = ghash_new(32, bignum_hash, bignum_cmp);
   );

   LOCAL_TEXT_BUF tb = tb_new();
   number_print(n, tb);

   bignum_t *bn = ghash_get(hash, n.big);
   if (bn == NULL) {
      const int nwords = bignum_words(n.big) * 2;
      bn = pool_malloc_flex(pool, sizeof(bignum_t), nwords, sizeof(uint64_t));
      bn->width    = n.big->width;
      bn->issigned = n.big->issigned;

      for (int i = 0; i < nwords; i++)
         bn->words[i] = n.big->words[i];

      ghash_put(hash, bn, bn);
   }

   return (number_t){ .big = bn };
}

static number_t number_scratch(int width, bool issigned)
{
   static __thread bignum_t *cache = NULL;
   static __thread int max_words = -1;

   const int nwords = MAX(1, BIGNUM_WORDS(width));

   if (nwords > max_words) {
      cache = xrealloc_flex(cache, sizeof(bignum_t), nwords * 2,
                            sizeof(uint64_t));
      max_words = nwords;
   }

   ASAN_UNPOISON(cache, sizeof(bignum_t) + nwords * 2 * sizeof(uint64_t));

   cache->width = width;
   cache->issigned = issigned;

   for (int i = 0; i < nwords * 2; i++)
      cache->words[i] = 0;

   return (number_t){ .big = cache };
}

static number_t number_scratch_copy(number_t n)
{
   number_t copy = number_scratch(n.big->width, n.big->issigned);

   memcpy(copy.big->words, n.big->words,
          bignum_words(n.big) * 2 * sizeof(uint64_t));

   return copy;
}

#ifdef DEBUG
static void _number_zap(number_t *p)
{
#if ASAN_ENABLED
   const int nwords = bignum_words(p->big);
   ASAN_POISON(p->big, sizeof(bignum_t) + nwords * 2 * sizeof(uint64_t));
#else
   p->big->width = 0;
   p->big->issigned = false;
   p->bits = 0;
#endif
}
#endif

number_t number_new(const char *str, const loc_t *loc)
{
   int width = 32;
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
         if (eptr != tick)
            should_not_reach_here();

         p = tick + 1;
      }
   }

   const bool issigned = (*p == 's');
   if (issigned) p++;

   SCRATCH_NUMBER result = number_scratch(width, issigned);

   vlog_radix_t radix = RADIX_DEC;
   switch (*p) {
   case 'b': radix = RADIX_BIN; p++; break;
   case 'o': radix = RADIX_OCT; p++; break;
   case 'h': radix = RADIX_HEX; p++; break;
   case 'd': radix = RADIX_DEC; p++; break;
   case '"': radix = RADIX_STR; p++; break;
   default: result.big->issigned = true; break;
   }

   if (*p == '_' && radix != RADIX_STR)
      error_at(loc, "number cannot start with an underscore");

   if (radix == RADIX_STR) {
      for (int bit = width - 8; !(*p == '"' && *(p + 1) == '\0');
           p++, bit -= 8) {
         const uint8_t byte = *p;
         bignum_set_nibble(result.big, bit, byte);
         bignum_set_nibble(result.big, bit + 4, byte >> 4);
      }
   }
   else if (radix == RADIX_DEC) {
      for (; *p; p++) {
         switch (*p) {
         case '0'...'9':
            {
               const uint64_t ten[] = { 10 };
               const uint64_t digit[] = { *p - '0' };

               vec2_mul(result.big->words, width, ten, 64);
               vec2_add(width, result.big->words, digit);
            }
            break;
         case '_':
            continue;
         default:
            error_at(loc, "invalid character '%c' in number %s", *p, str);
         }
      }
   }
   else {
      const char *start = p;
      for (; *(p + 1); p++);

      for (int bit = 0; p >= start; p--) {
         switch (radix) {
         case RADIX_BIN:
            {
               switch (*p) {
               case '0':
                  break;
               case '1':
                  bignum_set_abit(result.big, bit, 1);
                  break;
               case 'x':
                  bignum_set_abit(result.big, bit, 1);
                  bignum_set_bbit(result.big, bit, 1);
                  break;
               case 'z':
                  bignum_set_bbit(result.big, bit, 1);
                  break;
               case '_':
                  continue;
               default:
                  error_at(loc, "invalid character '%c' in number %s", *p, str);
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
               case 'x':
                  for (int i = 0; i < 3; i++) {
                     bignum_set_abit(result.big, bit + i, 1);
                     bignum_set_bbit(result.big, bit + i, 1);
                  }
                  break;
               case 'z':
                  for (int i = 0; i < 3; i++)
                     bignum_set_bbit(result.big, bit + i, 1);
                  break;
               case '0'...'7':
                  for (int i = 0; i < 3; i++)
                     bignum_set_abit(result.big, bit + i, (*p - '0') >> i);
                  break;
               case '_':
                  continue;
               default:
                  error_at(loc, "invalid character '%c' in number %s", *p, str);
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
               case 'x':
                  for (int i = 0; i < 4; i++) {
                     bignum_set_abit(result.big, bit + i, 1);
                     bignum_set_bbit(result.big, bit + i, 1);
                  }
                  break;
               case 'z':
                  for (int i = 0; i < 4; i++)
                     bignum_set_bbit(result.big, bit + i, 1);
                  break;
               case '0'...'9':
                  bignum_set_nibble(result.big, bit, *p - '0');
                  break;
               case 'a'...'f':
                  bignum_set_nibble(result.big, bit, 10 + *p - 'a');
                  break;
               case 'A'...'F':
                  bignum_set_nibble(result.big, bit, 10 + *p - 'A');
                  break;
               case '_':
                  continue;
               default:
                  error_at(loc, "invalid character '%c' in number %s", *p, str);
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
   }

   return number_intern(result);
}

number_t number_from_int(int64_t value)
{
   int width = 32;
   if (value < INT32_MIN || value > INT32_MAX)
      width = ilog2(llabs(value)) + 1;

   SCRATCH_NUMBER result = number_scratch(width, true);

   bignum_abits(result.big)[0] = value;
   bignum_bbits(result.big)[0] = 0;

   return number_intern(result);
}

number_t number_from_bool(bool value)
{
   SCRATCH_NUMBER result = number_scratch(1, false);

   bignum_abits(result.big)[0] = value;
   bignum_bbits(result.big)[0] = 0;

   return number_intern(result);
}

void number_print(number_t val, text_buf_t *tb)
{
   if (number_is_defined(val)) {
      if (val.big->width == 32 && val.big->issigned) {
         tb_printf(tb, "%"PRIi64, bignum_abits(val.big)[0]);
         return;
      }
      else if (val.big->width > 1 && val.big->width <= 32) {
         tb_printf(tb, "%u'%sd%"PRIu64, val.big->width,
                   val.big->issigned ? "s" : "", bignum_abits(val.big)[0]);
         return;
      }
   }

   tb_printf(tb, "%u'b", number_width(val));

   static const char map[] = "01zx";

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
   const int nwords = bignum_words(val.big);
   const uint64_t *bbits = bignum_bbits(val.big);

   for (int i = 0; i < nwords; i++) {
      if (bbits[i] != 0)
         return false;
   }

   return true;
}

int64_t number_integer(number_t val)
{
   assert(number_width(val) <= 64);
   assert(number_is_defined(val));

   return bignum_abits(val.big)[0];
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

   SCRATCH_NUMBER result = number_scratch(width, enc < 0);

   read_raw(result.big->words, nwords * 2 * sizeof(uint64_t), f);

   return number_intern(result);
}

number_t number_add(number_t a, number_t b)
{
   const int width = MAX(a.big->width, b.big->width) + 1;
   const bool issigned = a.big->issigned && b.big->issigned;

   SCRATCH_NUMBER result = number_scratch(width, issigned);

   assert(bignum_words(result.big) == 1);  // TODO

   bignum_abits(result.big)[0] =
      bignum_abits(a.big)[0] + bignum_abits(b.big)[0];
   bignum_bbits(result.big)[0] =
      bignum_bbits(a.big)[0] | bignum_bbits(b.big)[0];

   return number_intern(result);
}

number_t number_sub(number_t a, number_t b)
{
   const int width = MAX(a.big->width, b.big->width) + 1;
   const bool issigned = a.big->issigned && b.big->issigned;

   SCRATCH_NUMBER result = number_scratch(width, issigned);

   assert(bignum_words(result.big) == 1);  // TODO

   bignum_abits(result.big)[0] =
      bignum_abits(a.big)[0] - bignum_abits(b.big)[0];
   bignum_bbits(result.big)[0] =
      bignum_bbits(a.big)[0] | bignum_bbits(b.big)[0];

   return number_intern(result);
}

number_t number_mul(number_t a, number_t b)
{
   const int width = MAX(a.big->width, b.big->width);
   const bool issigned = a.big->issigned && b.big->issigned;

   SCRATCH_NUMBER result = number_scratch(width, issigned);

   assert(bignum_words(result.big) == 1);  // TODO

   bignum_abits(result.big)[0] =
      bignum_abits(a.big)[0] * bignum_abits(b.big)[0];
   bignum_bbits(result.big)[0] =
      bignum_bbits(a.big)[0] | bignum_bbits(b.big)[0];

   return number_intern(result);
}

number_t number_div(number_t a, number_t b)
{
   const int width = MAX(a.big->width, b.big->width);
   const bool issigned = a.big->issigned && b.big->issigned;

   SCRATCH_NUMBER result = number_scratch(width, issigned);

   assert(bignum_words(result.big) == 1);  // TODO

   bignum_abits(result.big)[0] =
      bignum_abits(a.big)[0] / bignum_abits(b.big)[0];
   bignum_bbits(result.big)[0] =
      bignum_bbits(a.big)[0] | bignum_bbits(b.big)[0];

   return number_intern(result);
}

number_t number_shl(number_t a, number_t b)
{
   const int width = a.big->width;
   const bool issigned = a.big->issigned;

   SCRATCH_NUMBER result = number_scratch(width, issigned);

   assert(bignum_words(result.big) == 1);  // TODO

   const uint64_t amount = bignum_abits(b.big)[0];

   if (amount < 64) {
      bignum_abits(result.big)[0] = bignum_abits(a.big)[0] << amount;
      bignum_bbits(result.big)[0] = bignum_bbits(a.big)[0] << amount;
   }
   else {
      bignum_abits(result.big)[0] = 0;
      bignum_bbits(result.big)[0] = 0;
   }

   return number_intern(result);
}

number_t number_negate(number_t a)
{
   SCRATCH_NUMBER result = number_scratch_copy(a);

   vec2_negate(result.big->words, result.big->width);

   return number_intern(result);
}

number_t number_logical_equal(number_t a, number_t b)
{
   SCRATCH_NUMBER result = number_scratch(1, false);

   assert(bignum_words(a.big) == 1);  // TODO
   assert(bignum_words(b.big) == 1);  // TODO

   bignum_abits(result.big)[0] =
      bignum_abits(a.big)[0] == bignum_abits(b.big)[0];
   bignum_bbits(result.big)[0] =
      bignum_bbits(a.big)[0] | bignum_bbits(b.big)[0];

   return number_intern(result);
}

number_t number_not(number_t a)
{
   SCRATCH_NUMBER result = number_scratch(1, false);

   assert(bignum_words(a.big) == 1);  // TODO

   bignum_abits(result.big)[0] = !bignum_abits(a.big)[0];
   bignum_bbits(result.big)[0] = bignum_bbits(a.big)[0];

   return number_intern(result);
}

number_t number_greater(number_t a, number_t b)
{
   SCRATCH_NUMBER result = number_scratch(1, false);

   if (number_is_defined(a) && number_is_defined(b)) {
      vlog_logic_t cmp;
      if (a.big->issigned && b.big->issigned)
         cmp = vec2_sgt(a.big->words, a.big->width, b.big->words, b.big->width);
      else
         cmp = vec2_gt(a.big->words, a.big->width, b.big->words, b.big->width);

      bignum_abits(result.big)[0] = (cmp == LOGIC_1);
      bignum_bbits(result.big)[0] = 0;
   }
   else
      bignum_abits(result.big)[0] = bignum_bbits(result.big)[0] = 1;

   return number_intern(result);
}

number_t number_greater_equal(number_t a, number_t b)
{
   SCRATCH_NUMBER result = number_scratch(1, false);

   if (number_is_defined(a) && number_is_defined(b)) {
      vlog_logic_t cmp;
      if (a.big->issigned && b.big->issigned)
         cmp = vec2_sge(a.big->words, a.big->width, b.big->words, b.big->width);
      else
         cmp = vec2_ge(a.big->words, a.big->width, b.big->words, b.big->width);

      bignum_abits(result.big)[0] = (cmp == LOGIC_1);
      bignum_bbits(result.big)[0] = 0;
   }
   else
      bignum_abits(result.big)[0] = bignum_bbits(result.big)[0] = 1;

   return number_intern(result);
}

number_t number_less(number_t a, number_t b)
{
   SCRATCH_NUMBER result = number_scratch(1, false);

   if (number_is_defined(a) && number_is_defined(b)) {
      vlog_logic_t cmp;
      if (a.big->issigned && b.big->issigned)
         cmp = vec2_slt(a.big->words, a.big->width, b.big->words, b.big->width);
      else
         cmp = vec2_lt(a.big->words, a.big->width, b.big->words, b.big->width);

      bignum_abits(result.big)[0] = (cmp == LOGIC_1);
      bignum_bbits(result.big)[0] = 0;
   }
   else
      bignum_abits(result.big)[0] = bignum_bbits(result.big)[0] = 1;

   return number_intern(result);
}

number_t number_less_equal(number_t a, number_t b)
{
   SCRATCH_NUMBER result = number_scratch(1, false);

   if (number_is_defined(a) && number_is_defined(b)) {
      vlog_logic_t cmp;
      if (a.big->issigned && b.big->issigned)
         cmp = vec2_sle(a.big->words, a.big->width, b.big->words, b.big->width);
      else
         cmp = vec2_le(a.big->words, a.big->width, b.big->words, b.big->width);

      bignum_abits(result.big)[0] = (cmp == LOGIC_1);
      bignum_bbits(result.big)[0] = 0;
   }
   else
      bignum_abits(result.big)[0] = bignum_bbits(result.big)[0] = 1;

   return number_intern(result);
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
}

void vec2_mul(uint64_t *a, size_t asize, const uint64_t *b, size_t bsize)
{
   if (asize <= 64 && bsize <= 64)
      a[0] *= b[0];
   else
      should_not_reach_here();   // TODO
}

void vec2_negate(uint64_t *a, size_t asize)
{
   if (asize <= 64)
      a[0] = -a[0];
   else
      should_not_reach_here();   // TODO
}

#define VEC2_CMP_OP(name, op)                                           \
   vlog_logic_t vec2_##name(const uint64_t *a, size_t asize,            \
                            const uint64_t *b, size_t bsize)            \
   {                                                                    \
      if (asize <= 64 && bsize <= 64)                                   \
         return a[0] op b[0] ? LOGIC_1 : LOGIC_0;                       \
      else                                                              \
         should_not_reach_here();   /* TODO */                          \
   }                                                                    \
                                                                        \
   vlog_logic_t vec2_s##name(const uint64_t *a, size_t asize,           \
                             const uint64_t *b, size_t bsize)           \
   {                                                                    \
      if (asize <= 64 && bsize <= 64)                                   \
         return (int64_t)a[0] op (int64_t)b[0] ? LOGIC_1 : LOGIC_0;     \
      else                                                              \
         should_not_reach_here();   /* TODO */                          \
   }

VEC2_CMP_OP(lt, <);
VEC2_CMP_OP(gt, >);
VEC2_CMP_OP(le, <=);
VEC2_CMP_OP(ge, >=);

static bool vec4_arith_defined(int size, uint64_t *a1, uint64_t *b1,
                               const uint64_t *a2, const uint64_t *b2)
{
   bool is_defined = true;
   for (int i = 0; i < BIGNUM_WORDS(size); i++)
      is_defined &= b1[i] == 0 && b2[i] == 0;

   if (is_defined)
      return true;

   for (int i = 0; i < BIGNUM_WORDS(size); i++) {
      b1[i] = a1[i] = ~UINT64_C(0);
      if ((i + 1) * 64 > size) {
         a1[i] >>= 64 - size % 64;
         b1[i] >>= 64 - size % 64;
      }
   }

   return false;
}

void vec4_add(int size, uint64_t *a1, uint64_t *b1, const uint64_t *a2,
              const uint64_t *b2)
{
   if (vec4_arith_defined(size, a1, b1, a2, b2))
      vec2_add(size, a1, a2);
}
