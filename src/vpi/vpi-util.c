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
#include "debug.h"
#include "diag.h"
#include "option.h"
#include "vlog/vlog-number.h"
#include "vpi/vpi-priv.h"

#include <inttypes.h>
#include <string.h>
#include <stdlib.h>

static __thread s_vpi_error_info last_error;

void vpi_trace(const char *func, const char *fmt, ...)
{
   va_list ap;
   va_start(ap, fmt);

   fprintf(stderr, "VPI: %s ", func);
   vfprintf(stderr, fmt, ap);
   fprintf(stderr, "\n");

   va_end(ap);
}

void vpi_error(PLI_INT32 sev, const loc_t *loc, const char *fmt, ...)
{
   last_error.level = sev;
   last_error.state = vpiRun;

   if (!loc_invalid_p(loc)) {
      last_error.file = (char *)loc_file_str(loc);
      last_error.line = loc->first_line;
   }

   free(last_error.message);

   va_list ap;
   va_start(ap, fmt);
   last_error.message = xvasprintf(fmt, ap);
   va_end(ap);

   static const diag_level_t map[] = {
      [vpiNotice] = DIAG_NOTE,
      [vpiWarning] = DIAG_WARN,
      [vpiError] = DIAG_ERROR,
      [vpiSystem] = DIAG_ERROR,
      [vpiInternal] = DIAG_FATAL,
   };

   if (!opt_get_int(OPT_PLI_DEBUG))
      return;

   // Also report the error as a diagnostic for debugging

   diag_t *d = diag_new(sev < ARRAY_LEN(map) ? map[sev] : DIAG_ERROR, loc);
   diag_printf(d, "%s", last_error.message);

   debug_info_t *trace = debug_capture();

   const debug_frame_t *last_prog = NULL, *first_lib = NULL;
   const int nframes = debug_count_frames(trace);
   for (int i = 0; i < nframes; i++) {
      const debug_frame_t *f = debug_get_frame(trace, i);
      if (f->kind == FRAME_PROG)
         last_prog = f;
      else if (f->kind == FRAME_LIB) {
         first_lib = f;
         break;
      }
   }

   if (last_prog != NULL && strncmp(last_prog->symbol, "vhpi", 4) == 0)
      diag_hint(d, NULL, "in call to VHPI function $bold$%s$$",
                last_prog->symbol);

   if (first_lib != NULL)
      diag_hint(d, NULL, "called from user function $bold$%s$$ "
                "at %s:%d", first_lib->symbol,
                first_lib->srcfile, first_lib->lineno);

   debug_free(trace);

   diag_emit(d);
}

void vpi_clear_error(void)
{
   last_error.level = 0;

   free(last_error.message);
   last_error.message = 0;
}

void vpi_format_number(number_t n, PLI_INT32 format, text_buf_t *tb)
{
   switch (format) {
   case vpiBinStrVal:
      number_print(n, tb);
      break;

   case vpiDecStrVal:
      if (number_is_defined(n))
         tb_printf(tb, "%"PRIi64, number_integer(n));
      else
         tb_cat(tb, "x");
      break;

   case vpiHexStrVal:
      if (number_is_defined(n))
         tb_printf(tb, "%0*"PRIx64, number_width(n) / 4, number_integer(n));
      else
         tb_printf(tb, "%*s", number_width(n) / 4, "x");
      break;
   }
}

void vpi_format_number2(int size, uint64_t abits, uint64_t bbits,
                        PLI_INT32 format, text_buf_t *tb)
{
   switch (format) {
   case vpiBinStrVal:
      should_not_reach_here();
      break;

   case vpiDecStrVal:
      if (bbits == 0)
         tb_printf(tb, "%"PRIi64, abits);
      else
         tb_cat(tb, "x");
      break;

   case vpiHexStrVal:
      if (bbits == 0)
         tb_printf(tb, "%0*"PRIx64, size / 4, abits);
      else
         tb_printf(tb, "%*s", size / 4, "x");
      break;
   }
}
