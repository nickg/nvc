//
//  Copyright (C) 2011-2022  Nick Gasson
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
#include "jit/jit.h"
#include "jit/jit-exits.h"
#include "jit/jit-ffi.h"
#include "rt/rt.h"

#include <assert.h>
#include <stdlib.h>
#include <inttypes.h>
#include <string.h>
#include <float.h>

static ffi_uarray_t bit_vec_to_string(const uint8_t *vec, size_t vec_len,
                                      int log_base)
{
   const size_t result_len = (vec_len + log_base - 1) / log_base;
   const int left_pad = (log_base - (vec_len % log_base)) % log_base;
   char *buf = rt_tlab_alloc(result_len);

   for (int i = 0; i < result_len; i++) {
      unsigned nibble = 0;
      for (int j = 0; j < log_base; j++) {
         if (i > 0 || j >= left_pad) {
            nibble <<= 1;
            nibble |= !!(vec[i*log_base + j - left_pad]);
         }
      }

      static const char map[16] = "0123456789ABCDEF";
      buf[i] = map[nibble];
   }

   return ffi_wrap(buf, 1, result_len);
}

DLLEXPORT
int64_t _std_standard_now(void)
{
   return x_now();
}

DLLEXPORT
void _std_to_string_time(int64_t value, int64_t unit, ffi_uarray_t *u)
{
   const char *unit_str = "";
   switch (unit) {
   case 1ll: unit_str = "fs"; break;
   case 1000ll: unit_str = "ps"; break;
   case 1000000ll: unit_str = "ns"; break;
   case 1000000000ll: unit_str = "us"; break;
   case 1000000000000ll: unit_str = "ms"; break;
   case 1000000000000000ll: unit_str = "sec"; break;
   case 60000000000000000ll: unit_str = "min"; break;
   case 3600000000000000000ll: unit_str = "hr"; break;
   default:
      jit_msg(NULL, DIAG_FATAL, "invalid UNIT argument %"PRIi64" in TO_STRING",
              unit);
   }

   size_t max_len = 32 + strlen(unit_str) + 1;
   char *buf = rt_tlab_alloc(max_len);

   size_t len;
   if (value % unit == 0)
      len = checked_sprintf(buf, max_len, "%"PRIi64" %s",
                            value / unit, unit_str);
   else
      len = checked_sprintf(buf, max_len, "%.*g %s", DBL_DIG,
                            (double)value / (double)unit, unit_str);

   *u = ffi_wrap(buf, 1, len);
}

DLLEXPORT
void _std_to_string_real_digits(double value, int32_t digits, ffi_uarray_t *u)
{
   size_t max_len = 32;
   char *buf = rt_tlab_alloc(max_len);

   size_t len;
   if (digits == 0)
      len = checked_sprintf(buf, max_len, "%.17g", value);
   else
      len = checked_sprintf(buf, max_len, "%.*f", digits, value);

   *u = ffi_wrap(buf, 1, len);
}

DLLEXPORT
void _std_to_string_real_format(double value, const void *fmt_ptr,
                                int64_t fmt_length, ffi_uarray_t *u)
{
   char *fmt_cstr LOCAL = null_terminate(fmt_ptr, fmt_length);

   if (fmt_cstr[0] != '%')
      jit_msg(NULL, DIAG_FATAL, "conversion specification must "
              "start with '%%'");

   for (const char *p = fmt_cstr + 1; *p; p++) {
      switch (*p) {
      case 'e': case 'E': case 'f': case 'F': case 'g': case 'G':
      case 'a': case 'A':
         continue;
      case '0'...'9':
         continue;
      case '.': case '-':
         continue;
      default:
         jit_msg(NULL, DIAG_FATAL, "illegal character '%c' in format \"%s\"",
                 *p, fmt_cstr + 1);
      }
   }

   size_t max_len = 64;
   char *buf = rt_tlab_alloc(max_len);
   size_t len = checked_sprintf(buf, max_len, fmt_cstr, value);
   *u = ffi_wrap(buf, 1, len);
}

DLLEXPORT
void _std_to_hstring_bit_vec(const uint8_t *vec_ptr, int64_t vec_len,
                             ffi_uarray_t *u)
{
   *u = bit_vec_to_string(vec_ptr, vec_len, 4);
}

DLLEXPORT
void _std_to_ostring_bit_vec(const uint8_t *vec_ptr, int64_t vec_len,
                             ffi_uarray_t *u)
{
   *u = bit_vec_to_string(vec_ptr, vec_len, 3);
}

void _std_standard_init(void)
{
   // Dummy function to force linking
}
