//
//  Copyright (C) 2011-2024  Nick Gasson
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
#include "rt/model.h"
#include "rt/mspace.h"
#include "rt/rt.h"

#include <assert.h>
#include <stdlib.h>
#include <inttypes.h>
#include <string.h>
#include <float.h>

static void bit_vec_to_string(jit_scalar_t *args, tlab_t *tlab, int log_base)
{
   const uint8_t *vec_ptr = args[1].pointer;
   int64_t vec_len = ffi_array_length(args[3].integer);

   const size_t result_len = (vec_len + log_base - 1) / log_base;
   const int left_pad = (log_base - (vec_len % log_base)) % log_base;
   char *buf = tlab_alloc(tlab, result_len);

   printf("result_len=%zu\n", result_len);

   for (int i = 0; i < result_len; i++) {
      unsigned nibble = 0;
      for (int j = 0; j < log_base; j++) {
         if (i > 0 || j >= left_pad) {
            nibble <<= 1;
            nibble |= !!(vec_ptr[i*log_base + j - left_pad]);
         }
      }

      static const char map[16] = "0123456789ABCDEF";
      buf[i] = map[nibble];
   }

   args[0].pointer = buf;
   args[1].integer = 1;
   args[2].integer = result_len;
}

DLLEXPORT
void _std_standard_now(jit_scalar_t *args)
{
   rt_model_t *m = get_model_or_null();
   args[0].integer = m ? model_now(m, NULL) : 0;
}

DLLEXPORT
void _std_to_string_time(jit_scalar_t *args, tlab_t *tlab)
{
   int64_t value = args[1].integer;
   int64_t unit = args[2].integer;

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

   static char buf[64];
   if (value % unit == 0)
      checked_sprintf(buf, sizeof(buf), "%"PRIi64" %s",
                      value / unit, unit_str);
   else
      checked_sprintf(buf, sizeof(buf), "%.*g %s", DBL_DIG,
                      (double)value / (double)unit, unit_str);

   ffi_return_string(buf, args, tlab);
}

DLLEXPORT
void _std_to_string_real_digits(jit_scalar_t *args, tlab_t *tlab)
{
   double value = args[1].real;
   int32_t digits = args[2].integer;

   char buf[32];
   if (digits == 0)
      checked_sprintf(buf, sizeof(buf), "%.17g", value);
   else
      checked_sprintf(buf, sizeof(buf), "%.*f", digits, value);

   ffi_return_string(buf, args, tlab);
}

DLLEXPORT
void _std_to_string_real_format(jit_scalar_t *args, tlab_t *tlab)
{
   double value = args[1].real;
   const void *fmt_ptr = args[2].pointer;
   size_t fmt_length = ffi_array_length(args[4].integer);

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

   char buf[64];
   checked_sprintf(buf, sizeof(buf), fmt_cstr, value);

   ffi_return_string(buf, args, tlab);
}

DLLEXPORT
void _std_to_hstring_bit_vec(jit_scalar_t *args, tlab_t *tlab)
{
   bit_vec_to_string(args, tlab, 4);
}

DLLEXPORT
void _std_to_ostring_bit_vec(jit_scalar_t *args, tlab_t *tlab)
{
   bit_vec_to_string(args, tlab, 3);
}

DLLEXPORT
void _std_to_string_real(jit_scalar_t *args, tlab_t *tlab)
{
   const size_t max = 32;
   char *buf = tlab_alloc(tlab, max);
   size_t len = checked_sprintf(buf, max, "%.*g", DBL_DIG, args[1].real);

   args[0].pointer = buf;
   args[1].integer = 1;
   args[2].integer = len;
}

void _std_standard_init(void)
{
   // Dummy function to force linking
}
