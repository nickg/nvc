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
#include "jit/jit.h"
#include "jit/jit-ffi.h"
#include "rt/rt.h"

#include <errno.h>
#include <limits.h>
#include <math.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <time.h>
#include <unistd.h>

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

typedef enum {
   DIR_OPEN_STATUS_OK = 0,
   DIR_OPEN_STATUS_NOT_FOUND = 1,
   DIR_OPEN_STATUS_NO_DIRECTORY = 2,
   DIR_OPEN_STATUS_ACCESS_DENIED = 3,
   DIR_OPEN_STATUS_ERROR = 4
} dir_open_status_t;

typedef enum {
   DIR_CREATE_STATUS_OK = 0,
   DIR_CREATE_STATUS_ITEM_EXISTS = 1,
   DIR_CREATE_STATUS_ACCESS_DENIED = 2,
   DIR_CREATE_STATUS_ERROR = 3
} dir_create_status_t;

static void copy_str(const char *str, ffi_uarray_t *u)
{
   const size_t len = strlen(str);
   char *buf = rt_tlab_alloc(len);
   memcpy(buf, str, len);
   *u = ffi_wrap_str(buf, len);
}

static char *to_cstring(const char *data, int len)
{
   len = abs(len);
   char *cstr = xmalloc(len + 1);
   memcpy(cstr, data, len);
   cstr[len] = '\0';
   return cstr;
}

static int8_t errno_to_dir_open_status(void)
{
   switch (errno) {
   case 0: return DIR_OPEN_STATUS_OK;
   case ENOENT: return DIR_OPEN_STATUS_NOT_FOUND;
   case ENOTDIR: return DIR_OPEN_STATUS_NO_DIRECTORY;
   case EACCES: return DIR_OPEN_STATUS_ACCESS_DENIED;
   default: return DIR_OPEN_STATUS_ERROR;
   }
}

static int8_t errno_to_dir_create_status(void)
{
   switch (errno) {
   case 0: return DIR_CREATE_STATUS_OK;
   case EEXIST: return DIR_CREATE_STATUS_ITEM_EXISTS;
   case EACCES: return DIR_CREATE_STATUS_ACCESS_DENIED;
   default: return DIR_CREATE_STATUS_ERROR;
   }
}

DLLEXPORT
void _std_env_stop(int32_t finish, int32_t have_status, int32_t status)
{
   if (have_status)
      notef("%s called with status %d", finish ? "FINISH" : "STOP", status);
   else
      notef("%s called", finish ? "FINISH" : "STOP");

   jit_abort(status);
}

DLLEXPORT
void _std_env_getenv(EXPLODED_UARRAY(name), ffi_uarray_t *u)
{
   char *LOCAL cstr = to_cstring(name_ptr, name_length);
   const char *env = getenv(cstr);

   if (env == NULL)
      *u = ffi_wrap_str(NULL, 0);
   else
      copy_str(env, u);
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

static void to_time_record(const struct tm *tm, int us, time_record_t *tr)
{
   tr->microsecond = us;
   tr->second      = tm->tm_sec;
   tr->minute      = tm->tm_min;
   tr->hour        = tm->tm_hour;
   tr->day         = tm->tm_mday;
   tr->month       = tm->tm_mon;
   tr->year        = 1900 + tm->tm_year;
   tr->weekday     = tm->tm_wday;
   tr->dayofyear   = tm->tm_yday;
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

   to_time_record(&tm, (int)(1e6 * (time - fsecs)), tr);
}

DLLEXPORT
void _std_env_gmtime(double time, time_record_t *tr)
{
   double fsecs = floor(time);
   time_t secs = (int)fsecs;

   struct tm tm;
#ifdef __MINGW32__
   gmtime_s(&tm, &secs);
#else
   gmtime_r(&secs, &tm);
#endif

   to_time_record(&tm, (int)(1e6 * (time - fsecs)), tr);
}

DLLEXPORT
void _std_env_get_workingdir(ffi_uarray_t *u)
{
   char buf[PATH_MAX];
   if (getcwd(buf, sizeof(buf)) == NULL)
      jit_msg(NULL, DIAG_FATAL, "getcwd failed: %s", strerror(errno));

   copy_str(buf, u);
}

DLLEXPORT
void _std_env_set_workingdir(EXPLODED_UARRAY(dir), int8_t *status)
{
   char *cstr LOCAL = to_cstring(dir_ptr, dir_length);

   if (chdir(cstr) == -1)
      *status = errno_to_dir_open_status();
   else
      *status = DIR_OPEN_STATUS_OK;
}

DLLEXPORT
void _std_env_createdir(EXPLODED_UARRAY(path), int8_t parents, int8_t *status)
{
   char *cstr LOCAL = to_cstring(path_ptr, path_length);

#ifdef __MINGW32__
   if (mkdir(cstr) == -1)
#else
   if (mkdir(cstr, 0777) == -1)
#endif
      *status = errno_to_dir_create_status();
   else
      *status = DIR_CREATE_STATUS_OK;
}

DLLEXPORT
bool _std_env_itemexists(EXPLODED_UARRAY(path))
{
   char *path LOCAL = to_cstring(path_ptr, path_length);

   struct stat sb;
   return stat(path, &sb) == 0;
}

DLLEXPORT
bool _std_env_itemisfile(EXPLODED_UARRAY(path))
{
   char *path LOCAL = to_cstring(path_ptr, path_length);

   struct stat sb;
   if (stat(path, &sb) != 0)
      return false;

   return (sb.st_mode & S_IFMT) == S_IFREG;
}

DLLEXPORT
bool _std_env_itemisdir(EXPLODED_UARRAY(path))
{
   char *path LOCAL = to_cstring(path_ptr, path_length);

   struct stat sb;
   if (stat(path, &sb) != 0)
      return false;

   return (sb.st_mode & S_IFMT) == S_IFDIR;
}

void _std_env_init(void)
{
   // Dummy function to force linking
}
