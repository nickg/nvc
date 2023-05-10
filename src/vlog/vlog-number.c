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
#include "diag.h"
#include "vlog/vlog-number.h"

#include <assert.h>
#include <string.h>
#include <stdlib.h>

#define EMBED_WIDTH 28

typedef enum {
   RADIX_BIN,
   RADIX_DEC,
   RADIX_HEX,
} vlog_radix_t;

typedef struct _bignum {
   unsigned width;
   bool     issigned;
   uint64_t packed[0];
} bignum_t;

number_t number_new(const char *str)
{
   int width = -1;
   const char *tick = strchr(str, '\''), *p = str;
   if (tick != NULL) {
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

   if (width < 0) {
      switch (radix) {
      case RADIX_BIN: width = strlen(p); break;
      case RADIX_HEX: width = strlen(p) / 16; break;
      case RADIX_DEC: width = ilog2(ipow(10, strlen(p)) - 1) + 1; break;
      }
   }

   if (radix == RADIX_DEC) {
      char *eptr;
      const int64_t bits = strtoll(p, &eptr, 10);
      if (*eptr != '\0')
         errorf("invalid character '%c' in number %s", *eptr, str);

      assert(width <= EMBED_WIDTH);

      uint64_t packed = 0;
      for (int i = width; i >= 0; i--) {
         packed <<= 2;
         if (bits & (1 << i))
            packed |= LOGIC_1;
         else
            packed |= LOGIC_0;
      }

      return (number_t) {
         .tag      = 1,
         .width    = width,
         .issigned = 0,
         .packed   = packed
      };
   }
   else if (width <= EMBED_WIDTH) {
      uint64_t packed = 0;
      for (; *p; p++) {
         switch (radix) {
         case RADIX_BIN:
            {
               packed <<= 2;
               switch (*p) {
               case '0': packed |= LOGIC_0; break;
               case '1': packed |= LOGIC_1; break;
               case 'x': packed |= LOGIC_X; break;
               case 'z': packed |= LOGIC_Z; break;
               default:
                  errorf("invalid character '%c' in number %s", *p, str);
               }
            }
            break;

         case RADIX_HEX:
            abort();

         case RADIX_DEC:
            break;
         }
      }

      return (number_t) {
         .tag      = 1,
         .width    = width,
         .issigned = 0,
         .packed   = packed
      };
   }
   else
      abort();
}

void number_free(number_t val)
{
   if (!val.tag)
      abort();
}

void number_print(number_t val, text_buf_t *tb)
{
   tb_printf(tb, "%u'b", val.width);

   bool leading = true;
   for (int i = val.width - 1; i >= 0; i--) {
      const int bit = (val.packed >> (i * 2)) & 3;
      if (leading && bit > 0)
         leading = false;
      else if (leading)
         continue;
      tb_printf(tb, "%d", bit);
   }
}

bool number_is_defined(number_t val)
{
   return !(val.packed & UINT64_C(0xaaaaaaaaaaaaaa));
}

int64_t number_to_integer(number_t val)
{
   assert(number_width(val) <= 64);
   assert(number_is_defined(val));

   // TODO: signed vs unsigned

   uint64_t result = 0;
   for (int i = val.width - 1; i >= 0; i--) {
      const int bit = val.packed >> (i * 2);
      result <<= 1;
      result |= bit & 1;

   }

   /*
   for (uint64_t p = val.packed; p; p >>= 2) {
      result <<= 1;
      result |= p & 1;
      }*/

   return result;
}

unsigned number_width(number_t val)
{
   return val.tag ? val.width : 0;
}
