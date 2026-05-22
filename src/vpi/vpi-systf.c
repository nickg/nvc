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
#include "rt/fileio.h"
#include "option.h"
#include "rt/model.h"
#include "svrand.h"
#include "thread.h"
#include "vpi/vpi-priv.h"

#include <assert.h>
#include <inttypes.h>
#include <math.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

typedef struct {
   FILE       *file;
   const char *str;
   size_t      pos;
   bool        eof;
} scan_src_t;

static void format_string(FILE *out, vpiHandle it, int fwidth)
{
   vpiHandle arg = vpi_scan(it);
   if (arg == NULL)
      return;

   s_vpi_value argval = { .format = vpiStringVal };
   vpi_get_value(arg, &argval);

   if (fwidth >= 0)
      fprintf(out, "%*s", fwidth, argval.value.str);
   else
      fputs(argval.value.str, out);

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

static void format_radix_string(FILE *out, const char *str, int fwidth)
{
   if (fwidth == 0) {
      while (str[0] == '0' && str[1] != '\0')
         str++;
   }

   if (fwidth > 0)
      fprintf(out, "%*s", fwidth, str);
   else
      fputs(str, out);
}

static void format_radix(FILE *out, vpiHandle arg, char radix, int fwidth,
                         int precision)
{
   switch (radix) {
   case 'd':
   case 't':
      {
         s_vpi_value argval = { .format = vpiDecStrVal };
         vpi_get_value(arg, &argval);

         if (!vpi_chk_error(NULL)) {
            const int nbits = vpi_get(vpiSize, arg);
            const bool is_signed = vpi_get(vpiSigned, arg);
            const int dmax = calc_dec_size(nbits, is_signed);
            const int pad_width =
               nbits >= 64 ? MAX(fwidth, dmax) : fwidth;

            if (pad_width >= 0)
               fprintf(out, "%*s", pad_width, argval.value.str);
            else if (dmax > strlen(argval.value.str))
               fprintf(out, "%*s", dmax, argval.value.str);
            else
               fputs(argval.value.str, out);
         }
      }
      break;
   case 'x':
   case 'h':
      {
         s_vpi_value argval = { .format = vpiHexStrVal };
         vpi_get_value(arg, &argval);

         if (!vpi_chk_error(NULL))
            format_radix_string(out, argval.value.str, fwidth);
      }
      break;
   case 'b':
      {
         s_vpi_value argval = { .format = vpiBinStrVal };
         vpi_get_value(arg, &argval);

         if (!vpi_chk_error(NULL))
            format_radix_string(out, argval.value.str, fwidth);
      }
      break;
   case 'o':
      {
         s_vpi_value argval = { .format = vpiOctStrVal };
         vpi_get_value(arg, &argval);

         if (!vpi_chk_error(NULL))
            format_radix_string(out, argval.value.str, fwidth);
      }
      break;
   case 'f':
      {
         s_vpi_value argval = { .format = vpiRealVal };
         vpi_get_value(arg, &argval);

         if (!vpi_chk_error(NULL)) {
            if (precision >= 0)
               fprintf(out, "%*.*f", fwidth < 0 ? 0 : fwidth, precision,
                       argval.value.real);
            else if (fwidth >= 0)
               fprintf(out, "%*f", fwidth, argval.value.real);
            else
               fprintf(out, "%f", argval.value.real);
         }
      }
      break;
   }
}

static void format_number(FILE *out, vpiHandle it, char radix, int fwidth,
                          int precision)
{
   vpiHandle arg = vpi_scan(it);
   if (arg == NULL)
      return;

   format_radix(out, arg, radix, fwidth, precision);
   vpi_release_handle(arg);
}

static void format_char(FILE *out, vpiHandle it, int fwidth)
{
   vpiHandle arg = vpi_scan(it);
   if (arg == NULL)
      return;

   s_vpi_value argval = { .format = vpiDecStrVal };
   vpi_get_value(arg, &argval);

   s_vpi_error_info ei;
   vpi_chk_error(&ei);
   const char ch = atoi(argval.value.str);
   if (fwidth >= 0)
      fprintf(out, "%*c", fwidth, ch);
   else
      fputc(ch, out);

   vpi_release_handle(arg);
}

static void interpret_format(FILE *out, const char *fmt, vpiHandle it)
{
   const char *start = fmt, *p = fmt;

   for (; *p; p++) {
      if (*p == '%') {
         if (start < p)
            fwrite(start, 1, p - start, out);

         p++;   // Skip over '%'

         int fwidth = -1;
         if (isdigit_iso88591(*p))
            fwidth = strtol(p, (char **)&p, 10);

         int precision = -1;
         if (*p == '.') {
            p++;
            precision = strtol(p, (char **)&p, 10);
         }

         switch (*p) {
         case 's':
            format_string(out, it, fwidth);
            break;
         case 'd':
         case 'b':
         case 'x':
         case 'h':
         case 't':
         case 'f':
            format_number(out, it, *p, fwidth, precision);
            break;
         case 'c':
            format_char(out, it, fwidth);
            break;
         case '%':
            fputc('%', out);
            break;
         default:
            jit_msg(NULL, DIAG_WARN, "unknown format specifier '%c'", *p);
         }

         start = p + 1;
      }
   }

   if (start < p)
      fwrite(start, 1, p - start, out);
}

static void verilog_printf(FILE *out, char default_radix, vpiHandle it)
{
   vpiHandle arg = vpi_scan(it);
   while (arg != NULL) {
      const bool is_null = vpi_get(vpiType, arg) == vpiOperation
         && vpi_get(vpiOpType, arg) == vpiNullOp;
      const bool has_format =
         vpi_get(vpiType, arg) == vpiConstant
         && vpi_get(vpiConstType, arg) == vpiStringConst;

      if (is_null)
         fputc(' ', out);
      else if (has_format) {
         s_vpi_value argval = { .format = vpiStringVal };
         vpi_get_value(arg, &argval);

         char *copy = xstrdup(argval.value.str);
         interpret_format(out, copy, it);
         free(copy);
      }
      else
         format_radix(out, arg, default_radix, -1, -1);

      vpi_release_handle(arg);
      arg = vpi_scan(it);
   }
}

static PLI_INT32 display_tf(PLI_BYTE8 *userdata)
{
   vpiHandle call = vpi_handle(vpiSysTfCall, NULL);
   assert(call != NULL);

   vpiHandle it = vpi_iterate(vpiArgument, call);

   verilog_printf(stdout, *(char *)userdata, it);
   printf("\n");
   return 0;
}

static PLI_INT32 write_tf(PLI_BYTE8 *userdata)
{
   vpiHandle call = vpi_handle(vpiSysTfCall, NULL);
   assert(call != NULL);

   vpiHandle it = vpi_iterate(vpiArgument, call);

   verilog_printf(stdout, *(char *)userdata, it);
   return 0;
}

static void put_int_result(PLI_INT32 value)
{
   s_vpi_value result = {
      .format = vpiIntVal,
      .value = { .integer = value },
   };

   vpiHandle call = vpi_handle(vpiSysTfCall, NULL);
   assert(call != NULL);

   vpi_put_value(call, &result, NULL, 0);
   vpi_release_handle(call);
}

static PLI_INT32 get_int_arg(vpiHandle arg)
{
   s_vpi_value value = { .format = vpiIntVal };
   vpi_get_value(arg, &value);
   return value.value.integer;
}

static FILE *get_file_stream(vpiHandle arg)
{
   s_vpi_value value = { .format = vpiIntVal };
   vpi_get_value(arg, &value);

   PLI_INT32 fd = value.value.integer;

   switch (fd) {
   case 0: return stdin;
   case 1: return stdout;
   case 2: return stderr;
   default:
      {
         FILE *f = file_stream(fd);
         if (f == NULL)
            jit_msg(NULL, DIAG_FATAL, "invalid file descriptor %d", fd);

         return f;
      }
   }
}

static PLI_INT32 fopen_tf(PLI_BYTE8 *userdata)
{
   vpiHandle call = vpi_handle(vpiSysTfCall, NULL);
   assert(call != NULL);

   vpiHandle argv = vpi_iterate(vpiArgument, call);
   vpiHandle file_arg = vpi_scan(argv);
   if (file_arg == NULL) {
      put_int_result(0);
      vpi_release_handle(argv);
      vpi_release_handle(call);
      return 0;
   }

   s_vpi_value file = { .format = vpiStringVal };
   vpi_get_value(file_arg, &file);
   char *file_name LOCAL = xstrdup(file.value.str);

   const char *mode_str = "w";
   char *mode_copy LOCAL = NULL;
   vpiHandle mode_arg = vpi_scan(argv);
   if (mode_arg != NULL) {
      s_vpi_value mode = { .format = vpiStringVal };
      vpi_get_value(mode_arg, &mode);
      mode_copy = xstrdup(mode.value.str);
      mode_str = mode_copy;
      vpi_release_handle(mode_arg);
   }

   const bool has_plus = strchr(mode_str, '+') != NULL;
   file_mode_t fmode = FILE_READ;
   switch (mode_str[0]) {
   case 'r': fmode = has_plus ? FILE_READ_WRITE : FILE_READ; break;
   case 'w': fmode = has_plus ? FILE_READ_WRITE : FILE_WRITE; break;
   case 'a': fmode = has_plus ? FILE_READ_WRITE : FILE_APPEND; break;
   default:
      jit_msg(NULL, DIAG_FATAL, "invalid file mode \"%s\"", mode_str);
   }

   file_handle_t fh = file_open(file_name, fmode);
   put_int_result(fh);

   vpi_release_handle(file_arg);
   vpi_release_handle(argv);
   vpi_release_handle(call);
   return 0;
}

static PLI_INT32 fclose_tf(PLI_BYTE8 *userdata)
{
   vpiHandle call = vpi_handle(vpiSysTfCall, NULL);
   assert(call != NULL);

   vpiHandle argv = vpi_iterate(vpiArgument, call);
   vpiHandle fd_arg = vpi_scan(argv);
   if (fd_arg != NULL) {
      const PLI_INT32 fd = get_int_arg(fd_arg);
      if (fd != 1 && fd != 2 && !file_close(fd))
         jit_msg(NULL, DIAG_FATAL, "$fclose called with invalid file "
                 "descriptor %d", fd);
      vpi_release_handle(fd_arg);
   }

   vpi_release_handle(argv);
   vpi_release_handle(call);
   return 0;
}

static PLI_INT32 fflush_tf(PLI_BYTE8 *userdata)
{
   vpiHandle call = vpi_handle(vpiSysTfCall, NULL);
   assert(call != NULL);

   vpiHandle argv = vpi_iterate(vpiArgument, call);
   vpiHandle fd_arg = vpi_scan(argv);
   if (fd_arg == NULL)
      fflush(NULL);
   else {
      FILE *f = get_file_stream(fd_arg);
      fflush(f);
      vpi_release_handle(fd_arg);
   }

   vpi_release_handle(argv);
   vpi_release_handle(call);
   return 0;
}

static PLI_INT32 feof_tf(PLI_BYTE8 *userdata)
{
   vpiHandle call = vpi_handle(vpiSysTfCall, NULL);
   assert(call != NULL);

   vpiHandle argv = vpi_iterate(vpiArgument, call);
   vpiHandle fd_arg = vpi_scan(argv);
   PLI_INT32 result = 1;
   if (fd_arg != NULL) {
      FILE *f = get_file_stream(fd_arg);
      result = feof(f);
      vpi_release_handle(fd_arg);
   }

   put_int_result(result);
   vpi_release_handle(argv);
   vpi_release_handle(call);
   return 0;
}

static PLI_INT32 fdisplay_tf(PLI_BYTE8 *userdata)
{
   const char *fmt = (const char *)userdata;
   const bool newline = fmt[1] == 'n';

   vpiHandle call = vpi_handle(vpiSysTfCall, NULL);
   assert(call != NULL);

   vpiHandle argv = vpi_iterate(vpiArgument, call);
   vpiHandle fd_arg = vpi_scan(argv);
   if (fd_arg != NULL) {
      FILE *out = get_file_stream(fd_arg);
      verilog_printf(out, fmt[0], argv);
      if (newline)
         fputc('\n', out);
      vpi_release_handle(fd_arg);
   }

   vpi_release_handle(argv);
   vpi_release_handle(call);
   return 0;
}

static int scan_getc(scan_src_t *src)
{
   int ch;
   if (src->file != NULL)
      ch = fgetc(src->file);
   else
      ch = src->str[src->pos] ? src->str[src->pos++] : EOF;

   if (ch == EOF)
      src->eof = true;

   return ch;
}

static void scan_ungetc(scan_src_t *src, int ch)
{
   if (ch == EOF)
      return;

   src->eof = false;
   if (src->file != NULL)
      ungetc(ch, src->file);
   else
      src->pos--;
}

static void scan_skip_ws(scan_src_t *src)
{
   int ch;
   while ((ch = scan_getc(src)) != EOF) {
      if (!isspace_iso88591(ch)) {
         scan_ungetc(src, ch);
         break;
      }
   }
}

static bool scan_token(scan_src_t *src, char *buf, size_t size)
{
   scan_skip_ws(src);

   size_t len = 0;
   int ch;
   while ((ch = scan_getc(src)) != EOF) {
      if (isspace_iso88591(ch)) {
         scan_ungetc(src, ch);
         break;
      }
      else if (len + 1 < size)
         buf[len++] = ch;
   }

   buf[len] = '\0';
   return len > 0;
}

static bool scan_assign(vpiHandle arg, char conv, const char *tok)
{
   s_vpi_value value = { .value = { .str = (PLI_BYTE8 *)tok } };

   switch (conv) {
   case 'd': value.format = vpiDecStrVal; break;
   case 'b': value.format = vpiBinStrVal; break;
   case 'o': value.format = vpiOctStrVal; break;
   case 'h':
   case 'x': value.format = vpiHexStrVal; break;
   case 's': value.format = vpiStringVal; break;
   case 'c':
      value.format = vpiIntVal;
      value.value.integer = (unsigned char)tok[0];
      break;
   default:
      return false;
   }

   vpi_put_value(arg, &value, NULL, vpiNoDelay);
   return !vpi_chk_error(NULL);
}

static PLI_INT32 scan_format(scan_src_t *src, const char *fmt, vpiHandle argv)
{
   int assigned = 0;

   for (const char *p = fmt; *p; p++) {
      if (isspace_iso88591(*p)) {
         scan_skip_ws(src);
         continue;
      }

      if (*p != '%') {
         const int ch = scan_getc(src);
         if (ch == EOF)
            break;
         else if (ch != *p) {
            scan_ungetc(src, ch);
            break;
         }
         continue;
      }

      p++;
      if (*p == '%') {
         const int ch = scan_getc(src);
         if (ch != '%') {
            scan_ungetc(src, ch);
            break;
         }
         continue;
      }

      while (isdigit_iso88591(*p))
         p++;

      char tok[1024];
      if (*p == 'c') {
         const int ch = scan_getc(src);
         if (ch == EOF)
            break;
         tok[0] = ch;
         tok[1] = '\0';
      }
      else if (*p == 'd' || *p == 'b' || *p == 'o' || *p == 'h'
               || *p == 'x' || *p == 's') {
         if (!scan_token(src, tok, sizeof(tok)))
            break;
      }
      else
         jit_msg(NULL, DIAG_WARN, "unknown scanf format specifier '%c'", *p);

      vpiHandle arg = vpi_scan(argv);
      if (arg == NULL)
         break;

      if (scan_assign(arg, *p, tok))
         assigned++;

      vpi_release_handle(arg);
   }

   return assigned == 0 && src->eof ? -1 : assigned;
}

static PLI_INT32 fscanf_tf(PLI_BYTE8 *userdata)
{
   vpiHandle call = vpi_handle(vpiSysTfCall, NULL);
   assert(call != NULL);

   vpiHandle argv = vpi_iterate(vpiArgument, call);
   vpiHandle fd_arg = vpi_scan(argv);
   vpiHandle fmt_arg = vpi_scan(argv);

   PLI_INT32 result = -1;
   if (fd_arg != NULL && fmt_arg != NULL) {
      FILE *f = get_file_stream(fd_arg);

      s_vpi_value fmt = { .format = vpiStringVal };
      vpi_get_value(fmt_arg, &fmt);

      scan_src_t src = { .file = f };
      result = scan_format(&src, fmt.value.str, argv);
   }

   put_int_result(result);

   if (fmt_arg != NULL) vpi_release_handle(fmt_arg);
   if (fd_arg != NULL) vpi_release_handle(fd_arg);
   vpi_release_handle(argv);
   vpi_release_handle(call);
   return 0;
}

static PLI_INT32 sscanf_tf(PLI_BYTE8 *userdata)
{
   vpiHandle call = vpi_handle(vpiSysTfCall, NULL);
   assert(call != NULL);

   vpiHandle argv = vpi_iterate(vpiArgument, call);
   vpiHandle str_arg = vpi_scan(argv);
   vpiHandle fmt_arg = vpi_scan(argv);

   PLI_INT32 result = -1;
   if (str_arg != NULL && fmt_arg != NULL) {
      s_vpi_value str = { .format = vpiStringVal };
      s_vpi_value fmt = { .format = vpiStringVal };
      vpi_get_value(str_arg, &str);
      char *copy LOCAL = xstrdup(str.value.str);
      vpi_get_value(fmt_arg, &fmt);

      scan_src_t src = { .str = copy };
      result = scan_format(&src, fmt.value.str, argv);
   }

   put_int_result(result);

   if (fmt_arg != NULL) vpi_release_handle(fmt_arg);
   if (str_arg != NULL) vpi_release_handle(str_arg);
   vpi_release_handle(argv);
   vpi_release_handle(call);
   return 0;
}

static PLI_INT32 fgets_tf(PLI_BYTE8 *userdata)
{
   vpiHandle call = vpi_handle(vpiSysTfCall, NULL);
   assert(call != NULL);

   vpiHandle argv = vpi_iterate(vpiArgument, call);
   vpiHandle str_arg = vpi_scan(argv);
   vpiHandle fd_arg = vpi_scan(argv);

   PLI_INT32 result = 0;
   if (str_arg != NULL && fd_arg != NULL) {
      FILE *f = get_file_stream(fd_arg);

      char *line LOCAL = NULL;
      size_t line_len = 0;
      ssize_t nread = getline(&line, &line_len, f);
      if (nread != -1) {
         result = nread;
         s_vpi_value value = {
            .format = vpiStringVal,
            .value = { .str = (PLI_BYTE8 *)line },
         };
         vpi_put_value(str_arg, &value, NULL, vpiNoDelay);
      }
   }

   put_int_result(result);

   if (fd_arg != NULL) vpi_release_handle(fd_arg);
   if (str_arg != NULL) vpi_release_handle(str_arg);
   vpi_release_handle(argv);
   vpi_release_handle(call);
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

static PLI_INT32 readmem_tf(PLI_BYTE8 *userdata)
{
   vpiHandle callh = vpi_handle(vpiSysTfCall, 0);
   vpiHandle argv = vpi_iterate(vpiArgument, callh);

   vpiHandle file_arg = vpi_scan(argv);
   vpiHandle mem_arg = vpi_scan(argv);

   s_vpi_value file = { .format = vpiStringVal };
   vpi_get_value(file_arg, &file);
   char *file_name LOCAL = xstrdup(file.value.str);
   vpi_release_handle(file_arg);

   file_handle_t fh = file_open(file_name, FILE_READ);
   FILE *f = file_stream(fh);
   if (f == NULL)
      jit_msg(NULL, DIAG_FATAL, "failed to open %s: %s",
              file_name, last_os_error());

   const bool binary = *(char *)userdata == 'b';
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

      s_vpi_value val = {
         .format = binary ? vpiBinStrVal : vpiHexStrVal,
         .value.str = tok,
      };
      vpi_put_value(elem, &val, NULL, vpiNoDelay);

      vpi_release_handle(elem);
   }

   file_close(fh);

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

static PLI_INT32 stime_tf(PLI_BYTE8 *userdata)
{
   rt_model_t *m = get_model();
   const int64_t now = model_now(m, NULL);

   s_vpi_value result = {
      .format = vpiIntVal,
      .value = { .integer = now & 0xffffffff },
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

   vpiHandle argv = vpi_iterate(vpiArgument, call);

   vpiHandle seed = vpi_scan(argv);
   if (seed != NULL) {
      s_vpi_value val = { .format = vpiIntVal };
      vpi_get_value(seed, &val);

      a_seed = val.value.integer;
   }

   vpi_release_handle(argv);

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

static PLI_INT32 clog2_tf(PLI_BYTE8 *userdata)
{
   vpiHandle call = vpi_handle(vpiSysTfCall, NULL);
   assert(call != NULL);

   vpiHandle argv = vpi_iterate(vpiArgument, call);
   vpiHandle arg = vpi_scan(argv);
   assert(arg != NULL);

   s_vpi_value value = { .format = vpiIntVal };
   vpi_get_value(arg, &value);

   vpi_release_handle(arg);
   vpi_release_handle(argv);

   s_vpi_value result = {
      .format = vpiIntVal,
      .value = { .integer = ilog2(value.value.integer) },
   };

   vpi_put_value(call, &result, NULL, 0);

   vpi_release_handle(call);
   return 0;
}

static PLI_INT32 rtoi_tf(PLI_BYTE8 *userdata)
{
   vpiHandle call = vpi_handle(vpiSysTfCall, NULL);
   assert(call != NULL);

   vpiHandle argv = vpi_iterate(vpiArgument, call);
   vpiHandle arg = vpi_scan(argv);
   assert(arg != NULL);

   s_vpi_value value = { .format = vpiRealVal };
   vpi_get_value(arg, &value);

   vpi_release_handle(arg);
   vpi_release_handle(argv);

   s_vpi_value result = {
      .format = vpiIntVal,
      .value = { .integer = value.value.real },
   };

   vpi_put_value(call, &result, NULL, 0);

   vpi_release_handle(call);
   return 0;
}

static PLI_INT32 real_unary_tf(double (*fn)(double))
{
   vpiHandle call = vpi_handle(vpiSysTfCall, NULL);
   assert(call != NULL);

   vpiHandle argv = vpi_iterate(vpiArgument, call);
   vpiHandle arg = vpi_scan(argv);
   assert(arg != NULL);

   s_vpi_value value = { .format = vpiRealVal };
   vpi_get_value(arg, &value);

   vpi_release_handle(arg);
   vpi_release_handle(argv);

   s_vpi_value result = {
      .format = vpiRealVal,
      .value = { .real = (*fn)(value.value.real) },
   };

   vpi_put_value(call, &result, NULL, 0);

   vpi_release_handle(call);
   return 0;
}

static PLI_INT32 pow_tf(PLI_BYTE8 *userdata)
{
   vpiHandle call = vpi_handle(vpiSysTfCall, NULL);
   assert(call != NULL);

   vpiHandle argv = vpi_iterate(vpiArgument, call);
   vpiHandle left = vpi_scan(argv);
   vpiHandle right = vpi_scan(argv);
   assert(left != NULL);
   assert(right != NULL);

   s_vpi_value lval = { .format = vpiRealVal };
   s_vpi_value rval = { .format = vpiRealVal };
   vpi_get_value(left, &lval);
   vpi_get_value(right, &rval);

   vpi_release_handle(left);
   vpi_release_handle(right);
   vpi_release_handle(argv);

   s_vpi_value result = {
      .format = vpiRealVal,
      .value = { .real = pow(lval.value.real, rval.value.real) },
   };

   vpi_put_value(call, &result, NULL, 0);

   vpi_release_handle(call);
   return 0;
}

static PLI_INT32 sqrt_tf(PLI_BYTE8 *userdata)
{
   return real_unary_tf(sqrt);
}

static PLI_INT32 sin_tf(PLI_BYTE8 *userdata)
{
   return real_unary_tf(sin);
}

static PLI_INT32 cos_tf(PLI_BYTE8 *userdata)
{
   return real_unary_tf(cos);
}

static PLI_INT32 tan_tf(PLI_BYTE8 *userdata)
{
   return real_unary_tf(tan);
}

static PLI_INT32 ln_tf(PLI_BYTE8 *userdata)
{
   return real_unary_tf(log);
}

static PLI_INT32 log10_tf(PLI_BYTE8 *userdata)
{
   return real_unary_tf(log10);
}

static PLI_INT32 exp_tf(PLI_BYTE8 *userdata)
{
   return real_unary_tf(exp);
}

static PLI_INT32 ceil_tf(PLI_BYTE8 *userdata)
{
   return real_unary_tf(ceil);
}

static PLI_INT32 floor_tf(PLI_BYTE8 *userdata)
{
   return real_unary_tf(floor);
}

static s_vpi_systf_data builtins[] = {
   {
      .type      = vpiSysTask,
      .tfname    = "$display",
      .calltf    = display_tf,
      .user_data = "d"
   },
   {
      .type      = vpiSysTask,
      .tfname    = "$displayb",
      .calltf    = display_tf,
      .user_data = "b"
   },
   {
      .type      = vpiSysTask,
      .tfname    = "$displayh",
      .calltf    = display_tf,
      .user_data = "h"
   },
   {
      .type      = vpiSysTask,
      .tfname    = "$displayo",
      .calltf    = display_tf,
      .user_data = "o"
   },
   {
      .type      = vpiSysTask,
      .tfname    = "$write",
      .calltf    = write_tf,
      .user_data = "d"
   },
   {
      .type      = vpiSysTask,
      .tfname    = "$writeb",
      .calltf    = write_tf,
      .user_data = "b"
   },
   {
      .type      = vpiSysTask,
      .tfname    = "$writeh",
      .calltf    = write_tf,
      .user_data = "h"
   },
   {
      .type      = vpiSysTask,
      .tfname    = "$writeo",
      .calltf    = write_tf,
      .user_data = "o"
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
      .calltf = readmem_tf,
      .user_data = "h"
   },
   {
      .type   = vpiSysTask,
      .tfname = "$readmemb",
      .calltf = readmem_tf,
      .user_data = "b"
   },
   {
      .type      = vpiSysTask,
      .tfname    = "$fdisplay",
      .calltf    = fdisplay_tf,
      .user_data = "dn"
   },
   {
      .type      = vpiSysTask,
      .tfname    = "$fdisplayb",
      .calltf    = fdisplay_tf,
      .user_data = "bn"
   },
   {
      .type      = vpiSysTask,
      .tfname    = "$fdisplayh",
      .calltf    = fdisplay_tf,
      .user_data = "hn"
   },
   {
      .type      = vpiSysTask,
      .tfname    = "$fdisplayo",
      .calltf    = fdisplay_tf,
      .user_data = "on"
   },
   {
      .type      = vpiSysTask,
      .tfname    = "$fwrite",
      .calltf    = fdisplay_tf,
      .user_data = "d"
   },
   {
      .type      = vpiSysTask,
      .tfname    = "$fwriteb",
      .calltf    = fdisplay_tf,
      .user_data = "b"
   },
   {
      .type      = vpiSysTask,
      .tfname    = "$fwriteh",
      .calltf    = fdisplay_tf,
      .user_data = "h"
   },
   {
      .type      = vpiSysTask,
      .tfname    = "$fwriteo",
      .calltf    = fdisplay_tf,
      .user_data = "o"
   },
   {
      .type   = vpiSysTask,
      .tfname = "$fclose",
      .calltf = fclose_tf
   },
   {
      .type   = vpiSysTask,
      .tfname = "$fflush",
      .calltf = fflush_tf
   },
   {
      .type        = vpiSysFunc,
      .tfname      = "$fopen",
      .sysfunctype = vpiIntFunc,
      .calltf      = fopen_tf
   },
   {
      .type        = vpiSysFunc,
      .tfname      = "$feof",
      .sysfunctype = vpiIntFunc,
      .calltf      = feof_tf
   },
   {
      .type        = vpiSysFunc,
      .tfname      = "$fscanf",
      .sysfunctype = vpiIntFunc,
      .calltf      = fscanf_tf
   },
   {
      .type        = vpiSysFunc,
      .tfname      = "$sscanf",
      .sysfunctype = vpiIntFunc,
      .calltf      = sscanf_tf
   },
   {
      .type        = vpiSysFunc,
      .tfname      = "$fgets",
      .sysfunctype = vpiIntFunc,
      .calltf      = fgets_tf
   },
   {
      .type        = vpiSysFunc,
      .tfname      = "$time",
      .sysfunctype = vpiTimeFunc,
      .calltf      = time_tf
   },
   {
      .type        = vpiSysFunc,
      .tfname      = "$stime",
      .sysfunctype = vpiIntFunc,
      .calltf      = stime_tf
   },
   {
      .type        = vpiSysFunc,
      .tfname      = "$random",
      .sysfunctype = vpiIntFunc,
      .calltf      = random_tf
   },
   {
      .type        = vpiSysFunc,
      .tfname      = "$clog2",
      .sysfunctype = vpiIntFunc,
      .calltf      = clog2_tf
   },
   {
      .type        = vpiSysFunc,
      .tfname      = "$rtoi",
      .sysfunctype = vpiIntFunc,
      .calltf      = rtoi_tf
   },
   {
      .type        = vpiSysFunc,
      .tfname      = "$pow",
      .sysfunctype = vpiRealFunc,
      .calltf      = pow_tf
   },
   {
      .type        = vpiSysFunc,
      .tfname      = "$sqrt",
      .sysfunctype = vpiRealFunc,
      .calltf      = sqrt_tf
   },
   {
      .type        = vpiSysFunc,
      .tfname      = "$sin",
      .sysfunctype = vpiRealFunc,
      .calltf      = sin_tf
   },
   {
      .type        = vpiSysFunc,
      .tfname      = "$cos",
      .sysfunctype = vpiRealFunc,
      .calltf      = cos_tf
   },
   {
      .type        = vpiSysFunc,
      .tfname      = "$tan",
      .sysfunctype = vpiRealFunc,
      .calltf      = tan_tf
   },
   {
      .type        = vpiSysFunc,
      .tfname      = "$ln",
      .sysfunctype = vpiRealFunc,
      .calltf      = ln_tf
   },
   {
      .type        = vpiSysFunc,
      .tfname      = "$log10",
      .sysfunctype = vpiRealFunc,
      .calltf      = log10_tf
   },
   {
      .type        = vpiSysFunc,
      .tfname      = "$exp",
      .sysfunctype = vpiRealFunc,
      .calltf      = exp_tf
   },
   {
      .type        = vpiSysFunc,
      .tfname      = "$ceil",
      .sysfunctype = vpiRealFunc,
      .calltf      = ceil_tf
   },
   {
      .type        = vpiSysFunc,
      .tfname      = "$floor",
      .sysfunctype = vpiRealFunc,
      .calltf      = floor_tf
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
