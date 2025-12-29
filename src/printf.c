//
//  Copyright (C) 2025  Nick Gasson
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
#include "ident.h"
#include "printf.h"
#include "thread.h"
#include "type.h"

#include <assert.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>
#include <limits.h>

#define ANSI_RESET      0
#define ANSI_BOLD       1
#define ANSI_FG_BLACK   30
#define ANSI_FG_RED     31
#define ANSI_FG_GREEN   32
#define ANSI_FG_YELLOW  33
#define ANSI_FG_BLUE    34
#define ANSI_FG_MAGENTA 35
#define ANSI_FG_CYAN    36
#define ANSI_FG_WHITE   37

#define MAX_ARGS 25

typedef struct _printf_state printf_state_t;
typedef struct _printf_arg printf_arg_t;

typedef union {
   long long  ll;
   long       l;
   size_t     z;
   double     f;
   int        i;
   void      *p;
} printf_value_t;

typedef int (*fmt_fn_t)(ostream_t *, printf_state_t *, printf_arg_t *);

typedef struct _printf_arg {
   const char     *start;
   size_t          len;
   fmt_fn_t        fn;
   printf_value_t  value;
   int             precision;
} printf_arg_t;

typedef struct _printf_state {
   printf_arg_t args[MAX_ARGS];
   unsigned     nargs;
   unsigned     pos;
} printf_state_t;

typedef struct {
   const char *name;
   int         value;
} color_escape_t;

static const color_escape_t escapes[] = {
   { "",        ANSI_RESET },
   { "bold",    ANSI_BOLD },
   { "black",   ANSI_FG_BLACK },
   { "red",     ANSI_FG_RED },
   { "green",   ANSI_FG_GREEN },
   { "yellow",  ANSI_FG_YELLOW },
   { "blue",    ANSI_FG_BLUE },
   { "magenta", ANSI_FG_MAGENTA },
   { "cyan",    ANSI_FG_CYAN },
   { "white",   ANSI_FG_WHITE },
};

static int printf_interpret(ostream_t *os, printf_state_t *state,
                            const char *fmt, const char *end);

int ostream_write(ostream_t *os, const char *buf, size_t len)
{
   (*os->callback)(buf, len, os->context);
   return len;
}

int ostream_putc(ostream_t *os, char ch)
{
   char buf[1] = { ch };
   return ostream_write(os, buf, 1);
}

int ostream_puts(ostream_t *os, const char *str)
{
   return ostream_write(os, str, strlen(str));
}

static int format_ident(ostream_t *os, printf_state_t *state, printf_arg_t *arg)
{
   ident_t id = arg->value.p;
   return ostream_write(os, istr(id), ident_len(id));
}

static int format_ident_toupper(ostream_t *os, printf_state_t *state,
                                printf_arg_t *arg)
{
   ident_t id = arg->value.p;
   size_t len = ident_len(id);
   const char *str = istr(id);
   int nchars = 0;
   char chunk[32];

   for (int i = 0; i < len; i += sizeof(chunk)) {
      const int tocopy = MIN(sizeof(chunk), len - i);
      for (int j = 0; j < tocopy; j++)
         chunk[j] = toupper_iso88591(str[i + j]);

      nchars += ostream_write(os, chunk, tocopy);
   }

   return nchars;
}

static int format_type(ostream_t *os, printf_state_t *state, printf_arg_t *arg)
{
   type_t type = arg->value.p;

   // Print a fully qualified name if there is another type in the
   // argument list with the same simple name

   for (int i = 0; i < state->nargs; i++) {
      const printf_arg_t *other = &(state->args[i]);
      if (other != arg && other->fn == format_type)
         return ostream_puts(os, type_pp2(type, other->value.p));
   }

   return ostream_puts(os, type_pp(type));
}

static int delegate(ostream_t *os, printf_state_t *s, printf_arg_t *arg, ...)
{
   char spec[32];
   assert(arg->len + 1 < sizeof(spec));
   memcpy(spec, arg->start, arg->len);
   spec[arg->len] = '\0';

   va_list ap, ap2;
   va_start(ap, arg);
   va_copy(ap2, ap);

   char small[64];
   int req = vsnprintf(small, sizeof(small), spec, ap);

   if (req + 1 > sizeof(small)) {
      char *large = xmalloc(req + 1);
      vsnprintf(large, req + 1, spec, ap2);
      ostream_write(os, large, req);
      free(large);
   }
   else
      ostream_write(os, small, req);

   va_end(ap);
   va_end(ap2);
   return req;
}

static fmt_fn_t get_pointer_formatter(char ch)
{
   switch (ch) {
   case 'i': return format_ident;
   case 'I': return format_ident_toupper;
   case 'T': return format_type;
   default: return NULL;
   }
}

static int ansi_escape(ostream_t *os, printf_state_t *state, const char **fmt)
{
   bool has_format = false;
   const char *start = *fmt;
   do {
      has_format |= (**fmt == '%');
      (*fmt)++;
   } while (**fmt != '\0' && **fmt != '$');

   if (**fmt == '\0')
      return ostream_write(os, start, *fmt - start);

   const char *end = *fmt;
   (*fmt)++;   // Advance past final '$'

   size_t len = len = *fmt - start - 2;
   const char *e = e = start + 1;
   LOCAL_TEXT_BUF tb = NULL;

   if (has_format) {
      // Expand any embedded formatting inside the ANSI escape
      tb = tb_new();

      ostream_t aos = {
         tb_ostream_write,
         tb,
         os->charset,
         false,
      };

      printf_interpret(&aos, state, start + 1, end);

      e = tb_get(tb);
      len = tb_len(tb);
   }

   bool bold;
   if ((bold = (*e == '!')))
      ++e, --len;

   bool bright;
   if ((bright = (*e == '+')))
      ++e, --len;

   if (*e == '#') {
      char *eptr;
      int code = strtoul(e + 1, &eptr, 10);
      if (eptr == e + len) {
         char buf[16];
         if (bold)
            checked_sprintf(buf, sizeof(buf), "\033[1;38;5;%dm", code);
         else
            checked_sprintf(buf, sizeof(buf), "\033[38;5;%dm", code);

         if (os->terminal)
            ostream_puts(os, buf);

         return 0;
      }
   }

   if (strncmp(e, "link:", 5) == 0) {
      const char *bel = strchr(e, '\07');

#ifndef __MINGW32__    // Winpty doesn't recognise these
      if (os->terminal) {
         ostream_puts(os, "\033]8;;");
         ostream_write(os, e + 5, len - 5);
         ostream_puts(os, "\033]8;;\07");
         return e + len - bel - 1;
      }
#endif

      return ostream_write(os, bel + 1, e + len - bel - 1);
   }

   for (int i = 0; i < ARRAY_LEN(escapes); i++) {
      if (strncmp(e, escapes[i].name, len) == 0) {
         int code = escapes[i].value + (bright ? 60 : 0);
         char buf[16];
         if (bold)
            checked_sprintf(buf, sizeof(buf), "\033[1;%dm", code);
         else
            checked_sprintf(buf, sizeof(buf), "\033[%dm", code);

         if (os->terminal)
            ostream_puts(os, buf);

         return 0;
      }
   }

   return ostream_write(os, start, *fmt - start);
}

static int format_z(ostream_t *os, printf_state_t *s, printf_arg_t *arg)
{
   assert(arg->precision == INT_MIN);
   return delegate(os, s, arg, arg->value.z);
}

static int format_ll(ostream_t *os, printf_state_t *s, printf_arg_t *arg)
{
   if (arg->precision != INT_MIN)
      return delegate(os, s, arg, arg->precision, arg->value.ll);
   else
      return delegate(os, s, arg, arg->value.ll);
}

static int format_l(ostream_t *os, printf_state_t *s, printf_arg_t *arg)
{
   if (arg->precision != INT_MIN)
      return delegate(os, s, arg, arg->precision, arg->value.l);
   else
      return delegate(os, s, arg, arg->value.l);
}

static int format_i(ostream_t *os, printf_state_t *s, printf_arg_t *arg)
{
   if (arg->precision != INT_MIN)
      return delegate(os, s, arg, arg->precision, arg->value.i);
   else
      return delegate(os, s, arg, arg->value.i);
}

static int format_f(ostream_t *os, printf_state_t *s, printf_arg_t *arg)
{
   if (arg->precision != INT_MIN)
      return delegate(os, s, arg, arg->precision, arg->value.f);
   else
      return delegate(os, s, arg, arg->value.f);
}

static int format_s(ostream_t *os, printf_state_t *s, printf_arg_t *arg)
{
   if (arg->precision != INT_MIN)
      return delegate(os, s, arg, arg->precision, arg->value.p);
   else
      return delegate(os, s, arg, arg->value.p);
}

static int format_p(ostream_t *os, printf_state_t *s, printf_arg_t *arg)
{
   return delegate(os, s, arg, arg->value.p);
}

static int printf_interpret(ostream_t *os, printf_state_t *state,
                            const char *fmt, const char *end)
{
   int nchars = 0;
   for (const char *p = fmt;;) {
      const char *start = p;
      while (p < end && *p != '%' && *p != '$')
         p++;

      if (start < p)
         nchars += ostream_write(os, start, p - start);

      if (p == end)
         return nchars;
      else if (*p == '$') {
         nchars += ansi_escape(os, state, &p);
         continue;
      }
      else if (*p == '%' && *(p + 1) == '%') {
         nchars += ostream_write(os, "%", 1);
         p += 2;
         continue;
      }

      assert(state->pos < state->nargs);

      printf_arg_t *arg = &(state->args[state->pos]);
      nchars += (*arg->fn)(os, state, arg);
      p += arg->len;

      state->pos++;
   }
}

int nvc_vfprintf(ostream_t *os, const char *fmt, va_list ap)
{
   printf_state_t state = {};

   const char *end = NULL;
   for (const char *p = fmt;;) {
      while (*p != '\0' && *p != '%')
         p++;

      if (*p == '\0') {
         end = p;
         break;
      }

      printf_arg_t arg = { .start = p, .precision = INT_MIN };
      bool z_mod = false;
      int l_mod = 0;
   again:
      switch (*++p) {
      case 'l':
         l_mod++;
         goto again;
      case 'z':
         z_mod = true;
         goto again;
      case '-':
      case '+':
      case '.':
      case '0'...'9':
         goto again;
      case '*':
         arg.precision = va_arg(ap, int);
         goto again;
      case 'd':
      case 'i':
      case 'x':
      case 'u':
         if (z_mod) {
            arg.value.z = va_arg(ap, size_t);
            arg.fn = format_z;
         }
         else if (l_mod >= 2) {
            arg.value.ll = va_arg(ap, long long);
            arg.fn = format_ll;
         }
         else if (l_mod == 1) {
            arg.value.l = va_arg(ap, long);
            arg.fn = format_l;
         }
         else {
            arg.value.i = va_arg(ap, int);
            arg.fn = format_i;
         }
         break;
      case 'c':
         arg.value.i = va_arg(ap, int);
         arg.fn = format_i;
         break;
      case 'e':
      case 'f':
      case 'g':
         arg.value.f = va_arg(ap, double);
         arg.fn = format_f;
         break;
      case 's':
         arg.value.p = va_arg(ap, char *);
         arg.fn = format_s;
         break;
      case 'p':
         arg.value.p = va_arg(ap, void *);
         if ((arg.fn = get_pointer_formatter(p[1])))
            p++;
         else
            arg.fn = format_p;
         break;
      case '%':
         p++;
         continue;
      default:
         fatal_trace("unhandled character '%c' in format", *p);
      }

      arg.len = ++p - arg.start;

      if (state.nargs == MAX_ARGS)
         fatal_trace("maximum of %d printf arguments supported", MAX_ARGS);
      else
         state.args[state.nargs++] = arg;
   }

   return printf_interpret(os, &state, fmt, end);
}

int nvc_vprintf(const char *fmt, va_list ap)
{
   return nvc_vfprintf(nvc_stdout(), fmt, ap);
}

int nvc_printf(const char *fmt, ...)
{
   va_list ap;
   va_start(ap, fmt);
   const int nchars = nvc_vprintf(fmt, ap);
   va_end(ap);
   return nchars;
}

int nvc_fprintf(ostream_t *os, const char *fmt, ...)
{
   va_list ap;
   va_start(ap, fmt);
   const int nchars = nvc_vfprintf(os, fmt, ap);
   va_end(ap);
   return nchars;
}

void stdio_ostream_write(const char *buf, size_t len, void *ctx)
{
   FILE *f = ctx;
   fwrite(buf, len, 1, f);
}

static void init_terminal_ostream(ostream_t *os, FILE *f)
{
   os->callback = stdio_ostream_write;
   os->context = f;

   if (isatty(fileno(f))) {
      os->charset = utf8_terminal() ? CHARSET_UTF8 : CHARSET_ISO88591;
      os->terminal = color_terminal();
   }
   else {
      os->charset = CHARSET_ISO88591;
      os->terminal = false;
   }
}

ostream_t *nvc_stdout(void)
{
   static ostream_t os;
   INIT_ONCE(init_terminal_ostream(&os, stdout));
   return &os;
}

ostream_t *nvc_stderr(void)
{
   static ostream_t os;
   INIT_ONCE(init_terminal_ostream(&os, stderr));
   return &os;
}

char *color_asprintf(const char *fmt, ...)
{
   LOCAL_TEXT_BUF tb = tb_new();
   ostream_t os = { tb_ostream_write, tb, CHARSET_ISO88591, color_terminal() };

   va_list ap;
   va_start(ap, fmt);
   nvc_vfprintf(&os, fmt, ap);
   va_end(ap);

   return tb_claim(tb);
}
