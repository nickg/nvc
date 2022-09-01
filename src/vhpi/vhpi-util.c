//
//  Copyright (C) 2014-2022  Nick Gasson
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
#include "opt.h"
#include "type.h"
#include "vhpi/vhpi-macros.h"
#include "vhpi/vhpi-util.h"

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifdef __MINGW32__
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#else
#include <dlfcn.h>
#endif

static vhpiErrorInfoT last_error;

////////////////////////////////////////////////////////////////////////////////
// Public API

const vhpiPhysT vhpiFS = { 0, 1 };
const vhpiPhysT vhpiPS = { 0, 0x3e8 };
const vhpiPhysT vhpiNS = { 0, 0xf4240 };
const vhpiPhysT vhpiUS = { 0, 0x3b9aca00 };
const vhpiPhysT vhpiMS = { 0xe8, 0xd4a51000 };
const vhpiPhysT vhpiS = { 0x38d7e, 0xa4c68000 };
const vhpiPhysT vhpiMN = { 0xd529ae, 0x9e860000 };
const vhpiPhysT vhpiHR = { 0x31f5c4ed, 0x27680000 };

int vhpi_printf(const char *format, ...)
{
   vhpi_clear_error();

   va_list ap;
   va_start(ap, format);

   const int ret = vhpi_vprintf(format, ap);

   va_end(ap);
   return ret;
}

int vhpi_vprintf(const char *format, va_list args)
{
   vhpi_clear_error();

   char *buf LOCAL = xvasprintf(format, args);
   notef("VHPI printf $green$%s$$", buf);
   return strlen(buf);
}

int vhpi_compare_handles(vhpiHandleT handle1, vhpiHandleT handle2)
{
   vhpi_clear_error();

   VHPI_TRACE("vhpi_compare_handles handle1=%p handle2=%p", handle1, handle2);

   return handle1 == handle2;
}

int vhpi_check_error(vhpiErrorInfoT *error_info_p)
{
   if (last_error.severity == 0)
      return 0;
   else {
      *error_info_p = last_error;
      return last_error.severity;
   }
}

int vhpi_assert(vhpiSeverityT severity, char *formatmsg,  ...)
{
   vhpi_clear_error();

   VHPI_TRACE("severity=%d formatmsg=\"%s\"", severity, formatmsg);

   va_list ap;
   va_start(ap, formatmsg);
   char *buf LOCAL = xvasprintf(formatmsg, ap);
   va_end(ap);

   switch (severity) {
   case vhpiNote:
      notef("%s", buf);
      break;

   case vhpiWarning:
      warnf("%s", buf);
      break;

   case vhpiError:
      errorf("%s", buf);
      break;

   case vhpiFailure:
   case vhpiSystem:
   case vhpiInternal:
      fatal("%s", buf);
      break;
   }

   return 0;
}

int vhpi_is_printable(char ch)
{
   if (ch < 32)
      return 0;
   else if (ch < 127)
      return 1;
   else if (ch == 127)
      return 0;
   else if ((unsigned char)ch < 160)
      return 0;
   else
      return 1;
}

////////////////////////////////////////////////////////////////////////////////
// Internal interface

int _vhpi_trace_on(void)
{
   static int trace_on = -1;

   if (trace_on == -1)
      trace_on = opt_get_verbose(OPT_VHPI_TRACE, NULL);

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

   if (loc != NULL) {
      last_error.file = (char *)loc_file_str(loc);
      last_error.line = loc->first_line;
   }

   free(last_error.message);

   va_list ap;
   va_start(ap, fmt);
   last_error.message = xvasprintf(fmt, ap);
   va_end(ap);

   if (loc != NULL)
      error_at(loc, "%s", last_error.message);
   else
      errorf("%s", last_error.message);
}

rt_event_t vhpi_get_rt_event(int reason)
{
   switch (reason){
   case vhpiCbNextTimeStep:
   case vhpiCbRepNextTimeStep:
      return RT_NEXT_TIME_STEP;
   case vhpiCbRepEndOfProcesses:
   case vhpiCbEndOfProcesses:
      return RT_END_OF_PROCESSES;
   case vhpiCbStartOfSimulation:
      return RT_START_OF_SIMULATION;
   case vhpiCbEndOfSimulation:
      return RT_END_OF_SIMULATION;
   case vhpiCbRepLastKnownDeltaCycle:
   case vhpiCbLastKnownDeltaCycle:
      return RT_LAST_KNOWN_DELTA_CYCLE;
   default:
      fatal_trace("unhandled value %d in vhpi_get_rt_event", reason);
   }
}

const char *vhpi_map_str_for_type(type_t type)
{
   ident_t type_name;
   if (type_is_array(type))
      type_name = type_ident(type_elem(type));
   else
      type_name = type_ident(type);

   switch (is_well_known(type_name)) {
   case W_IEEE_LOGIC:
   case W_IEEE_ULOGIC:
      return "UX01ZWLH-";
   case W_STD_BIT:
      return "01";
   default:
      fatal_trace("vhpi_map_type_for_str not supported for %s",
                  type_pp(type));
   }
}

bool vhpi_is_repetitive(vhpiEnumT reason)
{
   switch (reason) {
   case vhpiCbValueChange:
   case vhpiCbRepEndOfProcesses:
   case vhpiCbRepLastKnownDeltaCycle:
   case vhpiCbRepNextTimeStep:
      return true;
   default:
      return false;
   }
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

void vhpi_load_plugins(tree_t top, rt_model_t *model, const char *plugins)
{
   vhpi_clear_error();

   vhpi_build_design_model(top, model);

   char *plugins_copy LOCAL = xstrdup(plugins);

   char *tok = strtok(plugins_copy, ",");
   do {
      notef("loading VHPI plugin %s", tok);

#ifdef __MINGW32__
      HMODULE hModule = LoadLibrary(tok);
      if (hModule == NULL)
         fatal_errno("failed to load %s", tok);

      void (**startup_funcs)() =
         (void (**)())GetProcAddress(hModule, "vhpi_startup_routines");
#else
      void *handle = dlopen(tok, RTLD_LAZY | RTLD_GLOBAL);
      if (handle == NULL)
         fatal("%s", dlerror());

      void (**startup_funcs)() = dlsym(handle, "vhpi_startup_routines");
#endif

      if (startup_funcs != NULL) {
         while (*startup_funcs)
            (*startup_funcs++)();
      }
   } while ((tok = strtok(NULL, ",")));
}
