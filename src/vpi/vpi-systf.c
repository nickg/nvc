//
//  Copyright (C) 2024  Nick Gasson
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
#include "jit/jit.h"
#include "option.h"
#include "rt/model.h"
#include "svrand.h"
#include "thread.h"
#include "vpi/vpi-priv.h"

#include <assert.h>
#include <inttypes.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

static void format_string(vpiHandle it, int fwidth)
{
   vpiHandle arg = vpi_scan(it);
   if (arg == NULL)
      return;

   s_vpi_value argval = { .format = vpiStringVal };
   vpi_get_value(arg, &argval);

   fputs(argval.value.str, stdout);

   vpi_release_handle(arg);
}

static int calc_dec_size(int nr_bits, bool is_signed)
{
   // From Icarus Verilog vpi/sys_display.c
   if (is_signed) --nr_bits;
   int r = (nr_bits * 146L + 484) / 485;
   if (is_signed) ++r;
   return r;
}

static void format_number(vpiHandle it, char radix, int fwidth)
{
   vpiHandle arg = vpi_scan(it);
   if (arg == NULL)
      return;

   switch (radix) {
   case 'd':
   case 't':
      {
         s_vpi_value argval = { .format = vpiDecStrVal };
         vpi_get_value(arg, &argval);

         if (!vpi_chk_error(NULL)) {
            const int nbits = vpi_get(vpiSize, arg);
            const int dmax = calc_dec_size(nbits, false);

            if (dmax > strlen(argval.value.str))
               printf("%*s", dmax, argval.value.str);
            else
               fputs(argval.value.str, stdout);
         }
      }
      break;
   case 'x':
   case 'h':
      {
         s_vpi_value argval = { .format = vpiHexStrVal };
         vpi_get_value(arg, &argval);

         if (!vpi_chk_error(NULL))
            fputs(argval.value.str, stdout);
      }
      break;
   case 'b':
      {
         s_vpi_value argval = { .format = vpiBinStrVal };
         vpi_get_value(arg, &argval);

         if (!vpi_chk_error(NULL))
            fputs(argval.value.str, stdout);
      }
      break;
   case 'f':
      {
         s_vpi_value argval = { .format = vpiRealVal };
         vpi_get_value(arg, &argval);

         if (!vpi_chk_error(NULL))
            printf("%f", argval.value.real);
      }
      break;
   }

   vpi_release_handle(arg);
}

static void format_char(vpiHandle it, int fwidth)
{
   vpiHandle arg = vpi_scan(it);
   if (arg == NULL)
      return;

   s_vpi_value argval = { .format = vpiDecStrVal };
   vpi_get_value(arg, &argval);

   s_vpi_error_info ei;
   vpi_chk_error(&ei);
   const char ch = atoi(argval.value.str);
   fputc(ch, stdout);

   vpi_release_handle(arg);
}

static void interpret_format(const char *fmt, vpiHandle it)
{
   const char *start = fmt, *p = fmt;

   for (; *p; p++) {
      if (*p == '%') {
         if (start < p)
            fwrite(start, 1, p - start, stdout);

         p++;   // Skip over '%'

         int fwidth = 0;
         if (isdigit_iso88591(*p))
            fwidth = strtol(p + 1, (char **)&p, 10);

         switch (*p) {
         case 's':
            format_string(it, fwidth);
            break;
         case 'd':
         case 'b':
         case 'x':
         case 'h':
         case 't':
         case 'f':
            format_number(it, *p, fwidth);
            break;
         case 'c':
            format_char(it, fwidth);
            break;
         case '%':
            fputc('%', stdout);
            break;
         default:
            jit_msg(NULL, DIAG_WARN, "unknown format specifier '%c'", *p);
         }

         start = p + 1;
      }
   }

   if (start < p)
      fwrite(start, 1, p - start, stdout);
}

static void verilog_printf(void)
{
   vpiHandle call = vpi_handle(vpiSysTfCall, NULL);
   assert(call != NULL);

   vpiHandle it = vpi_iterate(vpiArgument, call);
   vpiHandle arg = vpi_scan(it);

   if (arg == NULL)
      goto release_handles;

   const bool has_format =
      vpi_get(vpiType, arg) == vpiConstant
      && vpi_get(vpiConstType, arg) == vpiStringConst;

   if (has_format) {
      s_vpi_value argval = { .format = vpiStringVal };
      vpi_get_value(arg, &argval);

      char *copy = xstrdup(argval.value.str);
      interpret_format(copy, it);
      free(copy);

      vpi_release_handle(arg);
      arg = vpi_scan(it);
   }

   while (arg != NULL) {
      const bool is_null = vpi_get(vpiType, arg) == vpiOperation
         && vpi_get(vpiOpType, arg) == vpiNullOp;

      if (is_null)
         fputc(' ', stdout);
      else {
         s_vpi_value argval = { .format = vpiDecStrVal };
         vpi_get_value(arg, &argval);

         const int nbits = vpi_get(vpiSize, arg);
         const int dmax = calc_dec_size(nbits, false);

         if (dmax > strlen(argval.value.str))
            printf("%*s", dmax, argval.value.str);
         else
            fputs(argval.value.str, stdout);
      }

      vpi_release_handle(arg);
      arg = vpi_scan(it);
   }

 release_handles:
   vpi_release_handle(call);
}

static PLI_INT32 display_tf(PLI_BYTE8 *userdata)
{
   verilog_printf();
   printf("\n");
   return 0;
}

static PLI_INT32 write_tf(PLI_BYTE8 *userdata)
{
   verilog_printf();
   return 0;
}

static PLI_INT32 finish_tf(PLI_BYTE8 *userdata)
{
   notef("$finish called");
   jit_abort();
}

static PLI_INT32 fatal_tf(PLI_BYTE8 *userdata)
{
   diag_t *d = diag_new(DIAG_FATAL, NULL);
   diag_printf(d, "$fatal called");
   diag_emit(d);

   jit_abort_with_status(1);
}

static PLI_INT32 monitor_tf(PLI_BYTE8 *userdata)
{
   // TODO
   return 0;
}

static PLI_INT32 readmemh_tf(PLI_BYTE8 *userdata)
{
   vpiHandle callh = vpi_handle(vpiSysTfCall, 0);
   vpiHandle argv = vpi_iterate(vpiArgument, callh);

   vpiHandle file_arg = vpi_scan(argv);
   vpiHandle mem_arg = vpi_scan(argv);

   s_vpi_value file = { .format = vpiStringVal };
   vpi_get_value(file_arg, &file);
   vpi_release_handle(file_arg);

   FILE *f = fopen(file.value.str, "r");
   if (f == NULL)
      jit_msg(NULL, DIAG_FATAL, "failed to open %s: %s",
              file.value.str, last_os_error());

   char *line LOCAL = NULL;
   size_t line_len = 0, index = 0;
   while (getline(&line, &line_len, f) != -1) {
      char *savep = NULL;
      char *tok = strtok_r(line, " \t\r\n", &savep);
      if (tok == NULL)
         continue;

      vpiHandle elem = vpi_handle_by_index(mem_arg, index++);
      if (elem == NULL)
         break;

      s_vpi_value val = { .format = vpiHexStrVal, .value.str = tok };
      vpi_put_value(elem, &val, NULL, vpiNoDelay);

      vpi_release_handle(elem);
   }

   fclose(f);

   vpi_release_handle(mem_arg);
   vpi_release_handle(argv);
   vpi_release_handle(callh);
   return 0;
}

static PLI_INT32 time_tf(PLI_BYTE8 *userdata)
{
   rt_model_t *m = get_model();
   const int64_t now = model_now(m, NULL);

   s_vpi_time timeval = {
      .type = vpiSimTime,
      .high = now >> 32,
      .low  = now & 0xffffffff
   };

   s_vpi_value result = {
      .format = vpiTimeVal,
      .value = { .time = &timeval },
   };

   vpiHandle call = vpi_handle(vpiSysTfCall, NULL);
   assert(call != NULL);

   vpi_put_value(call, &result, NULL, 0);

   vpi_release_handle(call);
   return 0;
}

static PLI_INT32 random_tf(PLI_BYTE8 *userdata)
{
   static __thread int32_t i_seed;
   INIT_ONCE(i_seed = opt_get_int(OPT_RANDOM_SEED));

   int32_t a_seed = i_seed;

   vpiHandle call = vpi_handle(vpiSysTfCall, NULL);
   assert(call != NULL);

   vpiHandle argv = vpi_iterate(vpiArgument, call), seed = NULL;
   if (argv != NULL) {
      seed = vpi_scan(argv);
      vpi_release_handle(argv);

      s_vpi_value val = { .format = vpiIntVal };
      vpi_get_value(seed, &val);

      a_seed = val.value.integer;
   }

   s_vpi_value result = {
      .format = vpiIntVal,
      .value = { .integer = rtl_dist_uniform(&a_seed, INT32_MIN, INT32_MAX) },
   };

   vpi_put_value(call, &result, NULL, 0);

   if (seed != NULL) {
      s_vpi_value next_seed = {
         .format = vpiIntVal,
         .value = { .integer = a_seed },
      };
      vpi_put_value(seed, &next_seed, NULL, 0);
      vpi_release_handle(seed);
   }
   else
      i_seed = a_seed;

   vpi_release_handle(call);
   return 0;
}

static s_vpi_systf_data builtins[] = {
   {
      .type   = vpiSysTask,
      .tfname = "$display",
      .calltf = display_tf
   },
   {
      .type   = vpiSysTask,
      .tfname = "$write",
      .calltf = write_tf
   },
   {
      .type   = vpiSysTask,
      .tfname = "$finish",
      .calltf = finish_tf
   },
   {
      .type   = vpiSysTask,
      .tfname = "$fatal",
      .calltf = fatal_tf
   },
   {
      .type   = vpiSysTask,
      .tfname = "$monitor",
      .calltf = monitor_tf
   },
   {
      .type   = vpiSysTask,
      .tfname = "$readmemh",
      .calltf = readmemh_tf
   },
   {
      .type        = vpiSysFunc,
      .tfname      = "$time",
      .sysfunctype = vpiTimeFunc,
      .calltf      = time_tf
   },
   {
      .type        = vpiSysFunc,
      .tfname      = "$random",
      .sysfunctype = vpiIntFunc,
      .calltf      = random_tf
   }
};

void vpi_register_builtins(void)
{
   for (int i = 0; i < ARRAY_LEN(builtins); i++) {
      vpiHandle handle = vpi_register_systf(&builtins[i]);
      if (handle == NULL)
         fatal("failed to register system task %s", builtins[i].tfname);
   }
}
