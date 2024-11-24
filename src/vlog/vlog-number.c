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

#include "util.h"
#include "diag.h"
#include "fbuf.h"
#include "vlog/vlog-number.h"

#include <assert.h>
#include <inttypes.h>
#include <string.h>
#include <stdlib.h>

typedef enum {
   RADIX_BIN = 2,
   RADIX_DEC = 10,
   RADIX_HEX = 16,
} vlog_radix_t;

typedef struct _bignum {
   unsigned width;
   bool     issigned;
   uint64_t packed[0];
} bignum_t;

#define SMALL_PACKED_ZERO UINT64_C(0xaaaaaaaaaaaaaa)
#define BIG_PACKED_ZERO UINT64_C(0xaaaaaaaaaaaaaaaa)
#define BIGNUM_WORDS(w) (((w) + 31) / 32)

static void number_shift_left(number_t *n, unsigned count, uint64_t carry_in)
{
   switch (n->common.tag) {
   case TAG_BIGNUM:
      {
         assert(count < 32);
         for (int i = BIGNUM_WORDS(n->big->width) - 1; i >= 0; i--) {
            const uint64_t carry_out = n->big->packed[i] >> (64 - count * 2);
            n->big->packed[i] <<= count * 2;
            n->big->packed[i] |= carry_in;
            carry_in = carry_out;
         }
      }
      break;
   case TAG_SMALLNUM:
      assert(count < n->small.width);
      n->small.packed <<= count * 2;
      break;
   default:
      DEBUG_ONLY(fatal_trace("invalid number tag %x", n->common.tag));
   }
}

static void strip_underscores(char *s)
{
   char *p;
   for (p = s; *s; s++) {
      if (*s != '_') *p++ = *s;
   }
   *p = '\0';
}

number_t number_new(const char *str)
{
   int width = 32;
   const char *tick = strchr(str, '\''), *p = str;
   if (tick == str)
      p++;
   else if (tick != NULL) {
      char *eptr;
      width = strtol(str, &eptr, 10);
      if (eptr != tick)
         abort();

      p = tick + 1;
   }

   vlog_radix_t radix = RADIX_DEC;
   switch (*p) {
   case 'b': radix = RADIX_BIN; p++; break;
   case 'h': radix = RADIX_HEX; p++; break;
   case 'd': radix = RADIX_DEC; p++; break;
   default: break;
   }

   bool has_xz = false;
   for (const char *pp = p; *pp; pp++)
      has_xz |= (*pp == 'x' || *pp == 'z');

   if (!has_xz && width <= INTEGER_WIDTH_MAX) {
      char *eptr, *copy LOCAL = NULL;
      int64_t bits = strtoll(p, &eptr, radix);
      if (*eptr == '_') {
         copy = xstrdup(p);
         strip_underscores(copy);
         bits = strtoll(copy, &eptr, radix);
      }
      if (*eptr != '\0')
         errorf("invalid character '%c' in number %s", *eptr, str);

      return (number_t) {
         .intg = {
            .tag      = TAG_INTEGER,
            .width    = width,
            .issigned = 0,
            .packed   = bits
         }
      };
   }
   else if (radix == RADIX_DEC) {
      char *eptr;
      const int64_t bits = strtoll(p, &eptr, 10);
      if (*eptr != '\0')
         errorf("invalid character '%c' in number %s", *eptr, str);

      assert(bits >= 0);

      if (width <= INTEGER_WIDTH_MAX) {
         return (number_t) {
            .intg = {
               .tag      = TAG_INTEGER,
               .width    = width,
               .issigned = 0,
               .packed   = bits
            }
         };
      }
      else {
         assert(false);

         uint64_t packed = SMALL_PACKED_ZERO;
         for (int i = width; i >= 0; i--) {
            packed <<= 2;
            if (bits & (1 << i))
               packed |= LOGIC_1;
            else
               packed |= LOGIC_0;
         }

         return (number_t) {
            .small = {
               .tag      = TAG_SMALLNUM,
               .width    = width,
               .issigned = 0,
               .packed   = packed
            }
         };
      }
   }
   else {
      number_t result = { .bits = 0 };
      if (width <= SMALLNUM_WIDTH_MAX) {
         result.small.tag      = TAG_SMALLNUM;
         result.small.width    = width;
         result.small.issigned = 0;
         result.small.packed   = SMALL_PACKED_ZERO;
      }
      else {
         const int nwords = BIGNUM_WORDS(width);
         result.big = xmalloc_flex(sizeof(bignum_t), nwords, sizeof(uint64_t));
         result.big->width = width;
         result.big->issigned = 0;

         for (int i = 0; i < nwords; i++)
            result.big->packed[i] = BIG_PACKED_ZERO;
      }

      for (; *p; p++) {
         switch (radix) {
         case RADIX_BIN:
            {
               uint64_t carry = LOGIC_X;
               switch (*p) {
               case '0': carry = LOGIC_0; break;
               case '1': carry = LOGIC_1; break;
               case 'x': carry = LOGIC_X; break;
               case 'z': carry = LOGIC_Z; break;
               default:
                  errorf("invalid character '%c' in number %s", *p, str);
               }

               number_shift_left(&result, 1, carry);
            }
            break;

         case RADIX_HEX:
            abort();

         case RADIX_DEC:
            break;
         }
      }

      return result;
   }
}

void number_free(number_t *val)
{
   if (val->common.tag == TAG_BIGNUM)
      free(val->big);

   val->bits = 0;
}

void number_print(number_t val, text_buf_t *tb)
{
   switch (val.common.tag) {
   case TAG_SMALLNUM:
   case TAG_BIGNUM:
      {
         tb_printf(tb, "%u'b", number_width(val));

         static const char map[] = "xz01";

         bool leading = true;
         for (int i = number_width(val) - 1; i >= 0; i--) {
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
      break;
   case TAG_INTEGER:
      if (val.intg.width == 1)
         tb_printf(tb, "1'b%u", (unsigned char)val.intg.packed);
      else if (val.intg.width == 32 && !val.intg.issigned) {
         tb_printf(tb, "%"PRIi64, (int64_t)val.intg.packed);
      }
      else
         tb_printf(tb, "%u'%sd%"PRIi64, val.intg.width,
                   val.intg.issigned ? "s" : "", (int64_t)val.intg.packed);
      break;
   default:
      DEBUG_ONLY(fatal_trace("invalid number tag %x", val.common.tag));
      break;
   }
}

bool number_is_defined(number_t val)
{
   switch (val.common.tag) {
   case TAG_SMALLNUM:
      return (val.small.packed & SMALL_PACKED_ZERO) == SMALL_PACKED_ZERO;
   case TAG_INTEGER:
      return true;
   case TAG_BIGNUM:
      for (int i = 0; i < BIGNUM_WORDS(val.big->width); i++) {
         if ((val.big->packed[i] & BIG_PACKED_ZERO) != BIG_PACKED_ZERO)
            return false;
      }
      return true;
   default:
      DEBUG_ONLY(fatal_trace("invalid number tag %x", val.common.tag));
      return 0;
   }
}

int64_t number_integer(number_t val)
{
   assert(number_width(val) <= 64);
   assert(number_is_defined(val));

   switch (val.common.tag) {
   case TAG_SMALLNUM:
   case TAG_BIGNUM:
      {
         // TODO: signed vs unsigned

         uint64_t result = 0;
         for (int i = number_width(val) - 1; i >= 0; i--) {
            const vlog_logic_t bit = number_bit(val, i);
            result <<= 1;
            result |= bit & 1;
         }

         return result;
      }
      break;
   case TAG_INTEGER:
      return val.intg.packed;
   default:
      DEBUG_ONLY(fatal_trace("invalid number tag %x", val.common.tag));
      return 0;
   }
}

unsigned number_width(number_t val)
{
   switch (val.common.tag) {
   case TAG_SMALLNUM:
      return val.small.width;
   case TAG_INTEGER:
      return val.intg.width;
   case TAG_BIGNUM:
      return val.big->width;
   default:
      DEBUG_ONLY(fatal_trace("invalid number tag %x", val.common.tag));
      return 0;
   }
}

vlog_logic_t number_bit(number_t val, unsigned n)
{
   switch (val.common.tag) {
   case TAG_SMALLNUM:
      assert(n < val.small.width);
      return (val.small.packed >> (n * 2)) & 3;
   case TAG_INTEGER:
      assert(n < val.intg.width);
      return ((val.intg.packed >> n) & 1) | LOGIC_0;
   case TAG_BIGNUM:
      assert(n < val.big->width);
      return (val.big->packed[(val.big->width - n - 1) / 32]
              >> ((n % 32) * 2)) & 3;
   default:
      DEBUG_ONLY(fatal_trace("invalid number tag %x", val.common.tag));
      return 0;
   }
}

number_t number_pack(const uint8_t *bits, unsigned width)
{
   bool has_xz = false;
   for (int i = 0; i < width; i++) {
      const vlog_logic_t log = STRIP_STRENGTH(bits[i]);
      has_xz |= (log == LOGIC_X || log == LOGIC_Z);
   }

   if (!has_xz && width <= INTEGER_WIDTH_MAX) {
      uint64_t packed = 0;
      for (int i = 0; i < width; i++) {
         packed <<= 1;
         packed |= (STRIP_STRENGTH(bits[i]) & 1);
      }

      return (number_t){
         .intg = {
            .tag      = TAG_INTEGER,
            .width    = width,
            .issigned = 0,
            .packed   = packed,
         }
      };
   }
   else if (width <= SMALLNUM_WIDTH_MAX) {
      uint64_t packed = SMALL_PACKED_ZERO;
      for (int i = 0; i < width; i++) {
         packed <<= 2;
         packed |= STRIP_STRENGTH(bits[i]);
      }

      return (number_t){
         .small = {
            .tag      = TAG_SMALLNUM,
            .width    = width,
            .issigned = 0,
            .packed   = packed,
         }
      };
   }
   else {
      const int nwords = BIGNUM_WORDS(width);
      bignum_t *bn = xmalloc_flex(sizeof(bignum_t), nwords, sizeof(uint64_t));
      bn->width = width;
      bn->issigned = 0;

      for (int i = 0; i < nwords; i++) {
         bn->packed[i] = BIG_PACKED_ZERO;

         for (int j = 0; j < 32 && i*32 + j < width; j++) {
            bn->packed[i] <<= 2;
            bn->packed[i] |= STRIP_STRENGTH(bits[i*32 + j]);
         }
      }

      return (number_t){ .big = bn };
   }
}

bool number_equal(number_t a, number_t b)
{
   assert(a.common.tag == b.common.tag);   // TODO

   switch (a.common.tag) {
   case TAG_INTEGER:
      return a.intg.packed == b.intg.packed;
   case TAG_SMALLNUM:
      return a.small.packed == b.small.packed;
   default:
      DEBUG_ONLY(fatal_trace("invalid number tag %x", a.common.tag));
      return false;
   }
}

void number_write(number_t val, fbuf_t *f)
{
   assert(val.common.tag != TAG_BIGNUM);
   fbuf_put_uint(f, val.bits);
}

number_t number_read(fbuf_t *f)
{
   return (number_t){ .bits = fbuf_get_uint(f) };
}
