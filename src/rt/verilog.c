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
#include "jit/jit.h"
#include "jit/jit-ffi.h"
#include "rt/model.h"
#include "vlog/vlog-number.h"

#include <assert.h>
#include <inttypes.h>
#include <stdlib.h>
#include <stdio.h>

static const void *next_arg(jit_scalar_t **args, unsigned *length)
{
   void *ptr = (*args)[0].pointer;
   *length = ffi_array_length((*args)[2].integer);
   *args += 3;
   return ptr;
}

static int calc_dec_size(int nr_bits, bool is_signed)
{
   // From Icarus Verilog src/vpi/sys_display.c
   if (is_signed) --nr_bits;
   int r = (nr_bits * 146L + 484) / 485;
   if (is_signed) ++r;
   return r;
}

static void verilog_printf(jit_scalar_t *args)
{
   unsigned fmtlen;
   const char *fmt = next_arg(&args, &fmtlen), *start = fmt, *p = fmt;

   for (; *p && p < fmt + fmtlen; p++) {
      if (*p == '%') {
         if (start < p)
            fwrite(start, 1, p - start, stdout);

         switch (*++p) {
         case 's':
            {
               unsigned len;
               const char *str = next_arg(&args, &len);
               fputs(str, stdout);
            }
            break;
         case 'd':
         case 'x':
            {
               unsigned width;
               const uint8_t *bits = next_arg(&args, &width);
               number_t num = number_pack(bits, width);

               switch (*p) {
               case 'd':
                  {
                     const int dmax = calc_dec_size(width, false);
                     if (number_is_defined(num))
                        printf("%*"PRIi64, dmax, number_integer(num));
                     else
                        printf("%*s", dmax, "x");
                  }
                  break;
               case 'x':
                  if (number_is_defined(num))
                     printf("%0*"PRIx64, width / 4, number_integer(num));
                  else
                     printf("%*s", width / 4, "x");
                  break;
               }

               number_free(&num);
            }
            break;
         default:
            jit_msg(NULL, DIAG_FATAL, "unknown format specifier '%c'", *p);
         }

         start = p + 1;
      }
   }

   if (start < p)
      fwrite(start, 1, p - start, stdout);
}

DLLEXPORT
void __nvc_sys_finish(void)
{
   notef("$finish called");
   jit_abort();
}

DLLEXPORT
void __nvc_sys_display(jit_scalar_t *args)
{
   verilog_printf(args + 2);
   fputc('\n', stdout);
   fflush(stdout);
}

DLLEXPORT
void __nvc_sys_write(jit_scalar_t *args)
{
   verilog_printf(args + 2);
   fflush(stdout);
}

void _verilog_init(void)
{
   // Dummy function to force linking
}
