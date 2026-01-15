//
//  Copyright (C) 2014-2023  Nick Gasson
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
#include "jit/jit-ffi.h"
#include "option.h"
#include "rt/assert.h"
#include "type.h"
#include "vhpi/vhpi-macros.h"
#include "vhpi/vhpi-model.h"
#include "vhpi/vhpi-priv.h"

#include <assert.h>
#include <inttypes.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static __thread vhpiErrorInfoT last_error;

////////////////////////////////////////////////////////////////////////////////
// Public API

DLLEXPORT const vhpiPhysT vhpiFS = { 0, 1 };
DLLEXPORT const vhpiPhysT vhpiPS = { 0, 0x3e8 };
DLLEXPORT const vhpiPhysT vhpiNS = { 0, 0xf4240 };
DLLEXPORT const vhpiPhysT vhpiUS = { 0, 0x3b9aca00 };
DLLEXPORT const vhpiPhysT vhpiMS = { 0xe8, 0xd4a51000 };
DLLEXPORT const vhpiPhysT vhpiS = { 0x38d7e, 0xa4c68000 };
DLLEXPORT const vhpiPhysT vhpiMN = { 0xd529ae, 0x9e860000 };
DLLEXPORT const vhpiPhysT vhpiHR = { 0x31f5c4ed, 0x27680000 };

DLLEXPORT
int vhpi_printf(const char *format, ...)
{
   vhpi_clear_error();

   va_list ap;
   va_start(ap, format);

   const int ret = vhpi_vprintf(format, ap);

   va_end(ap);
   return ret;
}

DLLEXPORT
int vhpi_vprintf(const char *format, va_list args)
{
   vhpi_clear_error();

   char *buf LOCAL = xvasprintf(format, args);
   size_t len = strlen(buf);

   for (char *eptr = buf + len - 1; eptr >= buf && *eptr == '\n'; eptr--, len--)
      *eptr = '\0';

   diag_t *d = diag_new(DIAG_NOTE, NULL);
   diag_write(d, buf, len);
   diag_emit(d);

   return len;
}

DLLEXPORT
int vhpi_check_error(vhpiErrorInfoT *error_info_p)
{
   if (last_error.severity == 0)
      return 0;
   else {
      *error_info_p = last_error;
      return last_error.severity;
   }
}

DLLEXPORT
int vhpi_assert(vhpiSeverityT severity, char *formatmsg,  ...)
{
   vhpi_clear_error();

   VHPI_TRACE("severity=%d formatmsg=\"%s\"", severity, formatmsg);

   vhdl_severity_t vhdl = SEVERITY_ERROR;
   switch (severity) {
   case vhpiNote:
      vhdl = SEVERITY_NOTE;
      break;
   case vhpiWarning:
      vhdl = SEVERITY_WARNING;
      break;
   case vhpiError:
      vhdl = SEVERITY_ERROR;
      break;
   case vhpiFailure:
   case vhpiSystem:
   case vhpiInternal:
      vhdl = SEVERITY_FAILURE;
      break;
   }

   va_list ap;
   va_start(ap, formatmsg);

   diag_t *d = diag_new(get_diag_severity(vhdl), NULL);
   diag_vprintf(d, formatmsg, ap);

   va_end(ap);

   emit_vhdl_diag(d, vhdl);
   return 0;
}

DLLEXPORT
int vhpi_is_printable(char ch)
{
   return isprint_iso88591(ch);
}

////////////////////////////////////////////////////////////////////////////////
// Internal interface

int _vhpi_trace_on(void)
{
   static int trace_on = -1;

   if (trace_on == -1)
      trace_on = opt_get_verbose(OPT_PLI_TRACE, NULL);

   return trace_on;
}

void vhpi_trace(const char *func, const char *fmt, ...)
{
   va_list ap;
   va_start(ap, fmt);

   fprintf(stderr, "VHPI: %s ", func);
   vfprintf(stderr, fmt, ap);
   fprintf(stderr, "\n");

   va_end(ap);
}

void vhpi_clear_error(void)
{
   last_error.severity = 0;
}

void vhpi_error(vhpiSeverityT sev, const loc_t *loc, const char *fmt, ...)
{
   vhpi_clear_error();

   last_error.severity = sev;
   last_error.str = NULL;

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
      [vhpiNote] = DIAG_NOTE,
      [vhpiWarning] = DIAG_WARN,
      [vhpiError] = DIAG_ERROR,
      [vhpiFailure] = DIAG_FATAL,
      [vhpiSystem] = DIAG_ERROR,
      [vhpiInternal] = DIAG_FATAL,
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

model_phase_t vhpi_get_phase(int reason)
{
   switch (reason){
   case vhpiCbNextTimeStep:
   case vhpiCbRepNextTimeStep:
      return NEXT_TIME_STEP;
   case vhpiCbStartOfNextCycle:
   case vhpiCbRepStartOfNextCycle:
      return NEXT_CYCLE;
   case vhpiCbEndOfTimeStep:
   case vhpiCbRepEndOfTimeStep:
      return END_TIME_STEP;
   case vhpiCbRepEndOfProcesses:
   case vhpiCbEndOfProcesses:
      return END_OF_PROCESSES;
   case vhpiCbStartOfSimulation:
      return START_OF_SIMULATION;
   case vhpiCbEndOfSimulation:
      return END_OF_SIMULATION;
   case vhpiCbRepLastKnownDeltaCycle:
   case vhpiCbLastKnownDeltaCycle:
      return LAST_KNOWN_DELTA_CYCLE;
   case vhpiCbEndOfInitialization:
      return END_OF_INITIALISATION;
   case vhpiCbStartOfProcesses:
   case vhpiCbRepStartOfProcesses:
      return START_OF_PROCESSES;
   default:
      fatal_trace("unhandled value %d in vhpi_get_rt_event", reason);
   }
}

vhpiFormatT vhpi_format_for_type(type_t type, const char **map_str)
{
   type_t base = type_base_recur(type);

   *map_str = NULL;

   switch (type_kind(base)) {
   case T_ENUM:
      switch (is_well_known(type_ident(base))) {
      case W_IEEE_LOGIC:
      case W_IEEE_ULOGIC:
         *map_str = "UX01ZWLH-";
         return vhpiLogicVal;
      case W_STD_BIT:
         *map_str = "01";
         return vhpiLogicVal;
      case W_STD_CHAR:
         return vhpiCharVal;
      default:
         if (type_enum_literals(base) <= 256)
            return vhpiSmallEnumVal;
         else
            return vhpiEnumVal;
      }
      break;

   case T_INTEGER:
      return vhpiIntVal;

   case T_REAL:
      return vhpiRealVal;

   case T_ARRAY:
      {
         type_t elem = type_base_recur(type_elem(base));
         switch (type_kind(elem)) {
         case T_ENUM:
            {
               switch (is_well_known(type_ident(elem))) {
               case W_IEEE_LOGIC:
               case W_IEEE_ULOGIC:
                  *map_str = "UX01ZWLH-";
                  return vhpiLogicVecVal;
               case W_STD_BIT:
                  *map_str = "01";
                  return vhpiLogicVecVal;
               case W_STD_CHAR:
                  return vhpiStrVal;
               default:
                  if (type_enum_literals(elem) <= 256)
                     return vhpiSmallEnumVecVal;
                  else
                     return vhpiEnumVecVal;
               }
               break;
            }

         case T_REAL:
            return vhpiRealVecVal;

         case T_INTEGER:
            return vhpiIntVecVal;

         default:
            break;
         }
      }
      break;

   default:
      break;
   }

   return (vhpiFormatT)-1;   // Not supported
}

vhpiPhysT vhpi_phys_from_native(int64_t value)
{
   const vhpiPhysT result = {
      .low  = value & 0xffffffff,
      .high = value >> 32
   };
   return result;
}

uint64_t vhpi_time_to_native(const vhpiTimeT *time)
{
   return ((uint64_t)time->high << 32) | (uint64_t)time->low;
}

void vhpi_load_plugins(const char *plugins)
{
   char *plugins_copy LOCAL = xstrdup(plugins);

   char *tok = strtok(plugins_copy, ",");
   do {
      notef("loading VHPI plugin %s", tok);

      jit_dll_t *dll = ffi_load_dll(tok);

      void (**startup_funcs)() = ffi_find_symbol(dll, "vhpi_startup_routines");

      if (startup_funcs != NULL) {
         while (*startup_funcs)
            (*startup_funcs++)();
      }
   } while ((tok = strtok(NULL, ",")));
}
