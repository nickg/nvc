//
//  Copyright (C) 2022  Nick Gasson
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
#include "common.h"
#include "rt/ffi.h"
#include "rt/rt.h"

#include <math.h>
#include <stdlib.h>
#include <string.h>
#include <sys/time.h>
#include <time.h>

typedef struct {
   int32_t microsecond;
   int32_t second;
   int32_t minute;
   int32_t hour;
   int32_t day;
   int32_t month;
   int32_t year;
   int8_t  weekday;
   int32_t dayofyear;
} time_record_t;

DLLEXPORT
void _std_env_getenv(EXPLODED_UARRAY(name), ffi_uarray_t *u)
{
   char *LOCAL cstr = xmalloc(name_length + 1);
   memcpy(cstr, name_ptr, name_length);
   cstr[name_length] = '\0';

   const char *env = getenv(cstr);

   if (env == NULL)
      *u = ffi_wrap_str(NULL, 0);
   else {
      const size_t len = strlen(env);
      char *buf = rt_tlab_alloc(len);
      memcpy(buf, env, len);
      *u = ffi_wrap_str(buf, len);
   }
}

DLLEXPORT
void _std_env_vhdl_version(ffi_uarray_t *u)
{
   const char *str = standard_text(standard());
   *u = ffi_wrap_str((char *)str, strlen(str));
}

DLLEXPORT
void _std_env_tool_version(ffi_uarray_t *u)
{
   const char *str = PACKAGE_VERSION;
   *u = ffi_wrap_str((char *)str, strlen(str));
}

DLLEXPORT
double _std_env_epoch(void)
{
   struct timeval tz = {};
   if (gettimeofday(&tz, NULL) == -1)
      warnf("gettimeofday: %s", last_os_error());

   return (double)tz.tv_sec + (double)tz.tv_usec / 1e6;
}

DLLEXPORT
void _std_env_localtime(double time, time_record_t *tr)
{
   double fsecs = floor(time);
   time_t secs = (int)fsecs;

   struct tm tm;
#ifdef __MINGW32__
   localtime_s(&tm, &secs);
#else
   localtime_r(&secs, &tm);
#endif

   tr->microsecond = (int)(1e6 * (time - fsecs));
   tr->second      = tm.tm_sec;
   tr->minute      = tm.tm_min;
   tr->hour        = tm.tm_hour;
   tr->day         = tm.tm_mday;
   tr->month       = tm.tm_mon;
   tr->year        = 1900 + tm.tm_year;
   tr->weekday     = tm.tm_wday;
   tr->dayofyear   = tm.tm_yday;
}

void _std_env_init(void)
{
   // Dummy function to force linking
}
