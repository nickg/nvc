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
#include "vlog/vlog-number.h"

#include <assert.h>
#include <inttypes.h>
#include <string.h>
#include <stdlib.h>

typedef enum {
   RADIX_STR = 0,
   RADIX_BIN = 2,
   RADIX_DEC = 10,
   RADIX_HEX = 16,
} vlog_radix_t;

typedef struct _bignum {
   unsigned width;
   bool     issigned;
   uint64_t words[];
} bignum_t;

#define BIGNUM_WORDS(w) (((w) + 63) / 64)

__attribute__((always_inline))
static inline int bignum_words(bignum_t *bn)
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

   vlog_radix_t radix = RADIX_DEC;
   switch (*p) {
   case 'b': radix = RADIX_BIN; p++; break;
   case 'h': radix = RADIX_HEX; p++; break;
   case 'd': radix = RADIX_DEC; p++; break;
   case '"': radix = RADIX_STR; p++; break;
   default: break;
   }

   number_t result = { .bits = 0 };
   const int nwords = MAX(1, BIGNUM_WORDS(width));
   result.big = xmalloc_flex(sizeof(bignum_t), nwords * 2, sizeof(uint64_t));
   result.big->width = width;
   result.big->issigned = false;

   for (int i = 0; i < nwords * 2; i++)
      result.big->words[i] = 0;

   if (radix == RADIX_STR) {
      for (int bit = width - 8; !(*p == '"' && *(p + 1) == '\0');
           p++, bit -= 8) {
         const uint8_t byte = *p;
         bignum_set_nibble(result.big, bit, byte);
         bignum_set_nibble(result.big, bit + 4, byte >> 4);
      }
   }
   else if (radix == RADIX_DEC) {
      char *eptr;
      result.big->words[0] = strtoull(p, &eptr, 10);
      if (*eptr != '\0')
         error_at(loc, "invalid character '%c' in number %s", *eptr, str);
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
                  return result;
               }

               bit++;
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
                  return result;
               }

               bit += 4;
            }
            break;

         default:
            should_not_reach_here();
         }
      }
   }

   return result;
}

void number_free(number_t *val)
{
   if (val->common.tag == TAG_BIGNUM)
      free(val->big);

   val->bits = 0;
}

void number_print(number_t val, text_buf_t *tb)
{
   if (number_is_defined(val)) {
      if (val.big->width == 32 && !val.big->issigned) {
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

number_t number_pack(const uint8_t *bits, unsigned width)
{
   const int nwords = BIGNUM_WORDS(width);
   bignum_t *bn = xmalloc_flex(sizeof(bignum_t), nwords * 2, sizeof(uint64_t));
   bn->width = width;
   bn->issigned = 0;

   for (int i = 0; i < nwords * 2; i++)
      bn->words[i] = 0;

   for (int i = 0; i < width; i++) {
      bignum_set_abit(bn, i, bits[width - 1 - i]);
      bignum_set_bbit(bn, i, bits[width - 1 - i] >> 1);
   }

   return (number_t){ .big = bn };
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

void number_write(number_t val, fbuf_t *f)
{
   fbuf_put_int(f, val.big->issigned ? -val.big->width : val.big->width);

   const int nwords = bignum_words(val.big);
   write_raw(val.big->words, nwords * 2  * sizeof(uint64_t), f);
}

number_t number_read(fbuf_t *f)
{
   const int64_t enc = fbuf_get_int(f);
   const unsigned width = llabs(enc);
   const int nwords = BIGNUM_WORDS(width);

   number_t result = { .bits = 0 };
   result.big = xmalloc_flex(sizeof(bignum_t), nwords * 2, sizeof(uint64_t));
   result.big->width = width;
   result.big->issigned = enc < 0;

   read_raw(result.big->words, nwords * 2 * sizeof(uint64_t), f);

   return result;
}

number_t number_add(number_t a, number_t b)
{
   const int width = MAX(a.big->width, b.big->width) + 1;
   const int nwords = BIGNUM_WORDS(width);

   number_t result = { .bits = 0 };
   result.big = xmalloc_flex(sizeof(bignum_t), nwords * 2, sizeof(uint64_t));
   result.big->width = width;
   result.big->issigned = a.big->issigned || b.big->issigned;

   assert(nwords == 1);  // TODO

   bignum_abits(result.big)[0] =
      bignum_abits(a.big)[0] + bignum_abits(b.big)[0];
   bignum_bbits(result.big)[0] =
      bignum_bbits(a.big)[0] | bignum_bbits(b.big)[0];

   return result;
}

number_t number_sub(number_t a, number_t b)
{
   const int width = MAX(a.big->width, b.big->width) + 1;
   const int nwords = BIGNUM_WORDS(width);

   number_t result = { .bits = 0 };
   result.big = xmalloc_flex(sizeof(bignum_t), nwords * 2, sizeof(uint64_t));
   result.big->width = width;
   result.big->issigned = a.big->issigned || b.big->issigned;

   assert(nwords == 1);  // TODO

   bignum_abits(result.big)[0] =
      bignum_abits(a.big)[0] - bignum_abits(b.big)[0];
   bignum_bbits(result.big)[0] =
      bignum_bbits(a.big)[0] | bignum_bbits(b.big)[0];

   return result;
}

number_t number_logical_equal(number_t a, number_t b)
{
   const int width = 1;
   const int nwords = BIGNUM_WORDS(width);

   number_t result = { .bits = 0 };
   result.big = xmalloc_flex(sizeof(bignum_t), nwords * 2, sizeof(uint64_t));
   result.big->width = width;
   result.big->issigned = false;

   assert(bignum_words(a.big) == 1);  // TODO
   assert(bignum_words(b.big) == 1);  // TODO

   bignum_abits(result.big)[0] =
      bignum_abits(a.big)[0] == bignum_abits(b.big)[0];
   bignum_bbits(result.big)[0] =
      bignum_bbits(a.big)[0] | bignum_bbits(b.big)[0];

   return result;

}
