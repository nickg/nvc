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

#include <assert.h>
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

void vpi_format_number(int size, const uint64_t *abits, const uint64_t *bbits,
                       s_vpi_value *val, text_buf_t *tb)
{
   const int nwords = (size + 63) / 64;
   bool is_defined = true;
   for (int i = 0; i < nwords; i++)
      is_defined &= (bbits[i] == 0);

   switch (val->format) {
   case vpiBinStrVal:
      for (int i = size - 1; i >= 0; i--) {
         const vlog_logic_t bit =
            ((bbits[i / 64] >> (i % 64)) & 1) << 1
            | ((abits[i / 64] >> (i % 64)) & 1);
         static const char map[] = "01zx";
         tb_append(tb, map[bit]);
      }
      val->value.str = (PLI_BYTE8 *)tb_get(tb);
      break;

   case vpiDecStrVal:
      if (is_defined)
         vec2_itoa(size, abits, tb);
      else
         tb_cat(tb, "x");
      val->value.str = (PLI_BYTE8 *)tb_get(tb);
      break;

   case vpiHexStrVal:
      if (is_defined) {
         for (int i = nwords - 1, field = ((size % 64) + 3) / 4;
              i >= 0; i--, field = 16)
            tb_printf(tb, "%0*"PRIx64, field, abits[i]);
      }
      else {
         for (int i = 0; i < (size + 3) / 4; i++)
            tb_append(tb, 'x');
      }
      val->value.str = (PLI_BYTE8 *)tb_get(tb);
      break;

   case vpiStringVal:
      if (is_defined) {
         for (int i = (size - 7) & ~7; i >= 0; i -= 8)
            tb_append(tb, abits[i / 64] >> (i % 64));
      }
      val->value.str = (PLI_BYTE8 *)tb_get(tb);
      break;

   case vpiIntVal:
      val->value.integer = abits[0];
      break;

   default:
      vpi_error(vpiError, NULL, "unsupported number format %d", val->format);
      break;
   }
}

PLI_INT32 vpi_chk_error(p_vpi_error_info error_info_p)
{
   if (error_info_p != NULL)
      *error_info_p = last_error;

   return last_error.level;
}
