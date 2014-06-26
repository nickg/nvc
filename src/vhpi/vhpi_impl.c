//
//  Copyright (C) 2014  Nick Gasson
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
//  Additional permission under GNU GPL version 3 section 7
//
//  If you modify this Program, or any covered work, by linking or
//  combining it with the IEEE VHPI reference code (or a modified version
//  of that library), containing parts covered by the terms of the IEEE's
//  license, the licensors of this Program grant you additional permission
//  to convey the resulting work. Corresponding Source for a non-source
//  form of such a combination shall include the source code for the parts
//  of the IEEE VHPI reference code used as well as that of the covered work.
//

#include "vhpi_user.h"
#include "util.h"

#include <string.h>
#include <assert.h>
#include <dlfcn.h>
#include <stdarg.h>

#define VHPI_MISSING fatal("VHPI function %s not implemented", __func__)

int vhpi_assert(vhpiSeverityT severity, char *formatmsg,  ...)
{
   VHPI_MISSING;
}

vhpiHandleT vhpi_register_cb(vhpiCbDataT *cb_data_p, int32_t flags)
{
   VHPI_MISSING;
}

int vhpi_remove_cb(vhpiHandleT cb_obj)
{
   VHPI_MISSING;
}

int vhpi_disable_cb(vhpiHandleT cb_obj)
{
   VHPI_MISSING;
}

int vhpi_enable_cb(vhpiHandleT cb_obj)
{
   VHPI_MISSING;
}

int vhpi_get_cb_info(vhpiHandleT object, vhpiCbDataT *cb_data_p)
{
   VHPI_MISSING;
}

vhpiHandleT vhpi_handle_by_name(const char *name, vhpiHandleT scope)
{
   VHPI_MISSING;
}

vhpiHandleT vhpi_handle_by_index(vhpiOneToManyT itRel,
                                 vhpiHandleT parent,
                                 int32_t indx)
{
   VHPI_MISSING;
}

vhpiHandleT vhpi_handle(vhpiOneToOneT type, vhpiHandleT referenceHandle)
{
   VHPI_MISSING;
}

vhpiHandleT vhpi_iterator(vhpiOneToManyT type, vhpiHandleT referenceHandle)
{
   VHPI_MISSING;
}

vhpiHandleT vhpi_scan(vhpiHandleT iterator)
{
   VHPI_MISSING;
}

vhpiIntT vhpi_get(vhpiIntPropertyT property, vhpiHandleT object)
{
   VHPI_MISSING;
}

const vhpiCharT * vhpi_get_str(vhpiStrPropertyT property,
                               vhpiHandleT object)
{
   VHPI_MISSING;
}

vhpiRealT vhpi_get_real(vhpiRealPropertyT property,
                        vhpiHandleT object)
{
   VHPI_MISSING;
}

vhpiPhysT vhpi_get_phys(vhpiPhysPropertyT property,
                        vhpiHandleT object)
{
   VHPI_MISSING;
}

int vhpi_protected_call(vhpiHandleT varHdl,
                        vhpiUserFctT userFct,
                        void *userData)
{
   VHPI_MISSING;
}

int vhpi_get_value(vhpiHandleT expr, vhpiValueT *value_p)
{
   VHPI_MISSING;
}

int vhpi_put_value(vhpiHandleT object,
                   vhpiValueT *value_p,
                   vhpiPutValueModeT mode)
{
   VHPI_MISSING;
}

int vhpi_schedule_transaction(vhpiHandleT drivHdl,
                              vhpiValueT *value_p,
                              uint32_t numValues,
                              vhpiTimeT *delayp,
                              vhpiDelayModeT delayMode,
                              vhpiTimeT *pulseRejp)
{
   VHPI_MISSING;
}

int vhpi_format_value(const vhpiValueT *in_value_p,
                      vhpiValueT *out_value_p)
{
   VHPI_MISSING;
}

void vhpi_get_time(vhpiTimeT *time_p, long *cycles)
{
   VHPI_MISSING;
}

int vhpi_get_next_time(vhpiTimeT *time_p)
{
   VHPI_MISSING;
}

int vhpi_control(vhpiSimControlT command, ...)
{
   VHPI_MISSING;
}

int vhpi_printf(const char *format, ...)
{
   va_list ap;
   va_start(ap, format);

   const int ret = vhpi_vprintf(format, ap);

   va_end(ap);
   return ret;
}

int vhpi_vprintf(const char *format, va_list args)
{
   char *buf LOCAL = xvasprintf(format, args);
   notef("VHPI printf $green$%s$$", buf);
   return strlen(buf);
}

int vhpi_compare_handles(vhpiHandleT handle1, vhpiHandleT handle2)
{
   VHPI_MISSING;
}

int vhpi_check_error(vhpiErrorInfoT *error_info_p)
{
   VHPI_MISSING;
}

int vhpi_release_handle(vhpiHandleT object)
{
   VHPI_MISSING;
}

vhpiHandleT vhpi_create(vhpiClassKindT kind,
                        vhpiHandleT handle1,
                        vhpiHandleT handle2)
{
   VHPI_MISSING;
}

int vhpi_get_foreignf_info(vhpiHandleT hdl, vhpiForeignDataT *foreignDatap)
{
   VHPI_MISSING;
}

size_t vhpi_get_data(int32_t id, void *dataLoc, size_t numBytes)
{
   VHPI_MISSING;
}

size_t vhpi_put_data(int32_t id, void *dataLoc, size_t numBytes)
{
   VHPI_MISSING;
}

int vhpi_is_printable(char ch)
{
   if (ch < 32)
      return 0;
   else if (ch < 127)
      return 1;
   else if (ch == 127)
      return 0;
   else if (ch < 160)
      return 0;
   else
      return 1;
}

void vhpi_load_plugins(const char *plugins)
{
   char *plugins_copy LOCAL = strdup(plugins);
   assert(plugins_copy);

   char *tok = strtok(plugins_copy, ",");
   do {
      notef("loading VHPI plugin %s", tok);

      void *handle = dlopen(tok, RTLD_NOW);
      if (handle == NULL)
         fatal("%s", dlerror());

      (void)dlerror();
      void (**startup_funcs)() = dlsym(handle, "vhpi_startup_routines");
      const char *error = dlerror();
      if (error != NULL) {
         warnf("%s", error);
         dlclose(handle);
         continue;
      }

      while (*startup_funcs)
         (*startup_funcs++)();

   } while ((tok = strtok(NULL, ",")));
}
