//
//  Copyright (C) 2022-2023  Nick Gasson
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

#include <assert.h>
#include <ctype.h>
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

typedef struct {
   ffi_uarray_t *name;
   ffi_uarray_t *file_name;
   ffi_uarray_t *file_path;
   int32_t       file_line;
} call_path_element_t;

static void copy_str(const char *str, ffi_uarray_t *u)
{
   const size_t len = strlen(str);
   char *buf = rt_tlab_alloc(len);
   memcpy(buf, str, len);
   *u = ffi_wrap(buf, 1, len);
}

static char *to_cstring(const uint8_t *data, int len)
{
   len = abs(len);
   char *cstr = xmalloc(len + 1);
   memcpy(cstr, data, len);
   cstr[len] = '\0';
   return cstr;
}

static ffi_uarray_t *to_line_n(const char *str, size_t len)
{
   char *buf = jit_mspace_alloc(len);
   memcpy(buf, str, len);

   ffi_uarray_t *u = jit_mspace_alloc(sizeof(ffi_uarray_t));
   *u = ffi_wrap(buf, 1, len);
   return u;
}

static ffi_uarray_t *to_line(const char *str)
{
   return to_line_n(str, strlen(str));
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

static const char *find_dir_separator(const char *str)
{
   // Forward slash is a valid separator even on Windows
   const char *fwd = strrchr(str, '/');
   if (fwd == NULL && DIR_SEP[0] != '/')
      return strrchr(str, DIR_SEP[0]);
   else
      return fwd;
}

static ffi_uarray_t *to_absolute_path(const char *input, size_t len)
{
   if (input[0] == DIR_SEP[0] || input[0] == '/')
      return to_line_n(input, len);

#ifdef __MINGW32__
   if (isalpha((int)input[0]) && input[1] == ':')
      return to_line_n(input, len);
#endif

   char buf[PATH_MAX];
   if (realpath(input, buf) == NULL)
      return to_line(input);
   else
      return to_line(buf);
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
void _std_env_getenv(const uint8_t *name_ptr, int64_t name_len, ffi_uarray_t *u)
{
   char *LOCAL cstr = to_cstring(name_ptr, name_len);
   const char *env = getenv(cstr);

   if (env == NULL)
      *u = ffi_wrap(NULL, 1, 0);
   else
      copy_str(env, u);
}

DLLEXPORT
void _std_env_vhdl_version(ffi_uarray_t *u)
{
   const char *str = standard_text(standard());
   *u = ffi_wrap((char *)str, 1, strlen(str));
}

DLLEXPORT
void _std_env_tool_version(ffi_uarray_t *u)
{
   const char *str = PACKAGE_VERSION;
   *u = ffi_wrap((char *)str, 1, strlen(str));
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
void _std_env_set_workingdir(const uint8_t *dir_ptr, int64_t dir_len,
                             int8_t *status)
{
   char *cstr LOCAL = to_cstring(dir_ptr, dir_len);

   if (chdir(cstr) == -1)
      *status = errno_to_dir_open_status();
   else
      *status = DIR_OPEN_STATUS_OK;
}

DLLEXPORT
void _std_env_createdir(const uint8_t *path_ptr, int64_t path_len,
                        int8_t parents, int8_t *status)
{
   char *cstr LOCAL = to_cstring(path_ptr, path_len);

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
bool _std_env_itemexists(const uint8_t *path_ptr, int64_t path_len)
{
   char *path LOCAL = to_cstring(path_ptr, path_len);

   struct stat sb;
   return stat(path, &sb) == 0;
}

DLLEXPORT
bool _std_env_itemisfile(const uint8_t *path_ptr, int64_t path_len)
{
   char *path LOCAL = to_cstring(path_ptr, path_len);

   struct stat sb;
   if (stat(path, &sb) != 0)
      return false;

   return (sb.st_mode & S_IFMT) == S_IFREG;
}

DLLEXPORT
bool _std_env_itemisdir(const uint8_t *path_ptr, int64_t path_len)
{
   char *path LOCAL = to_cstring(path_ptr, path_len);

   struct stat sb;
   if (stat(path, &sb) != 0)
      return false;

   return (sb.st_mode & S_IFMT) == S_IFDIR;
}

DLLEXPORT
void _std_env_get_call_path(ffi_uarray_t **ptr)
{
   jit_stack_trace_t *stack LOCAL = jit_stack_trace();

   call_path_element_t *array =
      jit_mspace_alloc((stack->count - 1) * sizeof(call_path_element_t));

   for (int i = 1; i < stack->count; i++) {
      jit_frame_t *frame = &(stack->frames[i]);
      call_path_element_t *cpe = &(array[i - 1]);

      cpe->name = to_line(istr(tree_ident(frame->decl)));
      cpe->file_line = frame->loc.first_line;

      const char *file = loc_file_str(&frame->loc);
      const char *sep = find_dir_separator(file);

      if (sep != NULL) {
         cpe->file_name = to_line(sep + 1);
         cpe->file_path = to_absolute_path(file, sep - file);
      }
      else {
         cpe->file_name = to_line(file);
         cpe->file_path = to_absolute_path(".", 1);
      }
   }

   ffi_uarray_t *u = jit_mspace_alloc(sizeof(ffi_uarray_t));
   *u = ffi_wrap(array, 0, stack->count - 2);
   *ptr = u;
}

DLLEXPORT
void _std_env_file_name(ffi_uarray_t **ptr)
{
   jit_stack_trace_t *stack LOCAL = jit_stack_trace();
   assert(stack->count > 1);

   const char *file = loc_file_str(&(stack->frames[1].loc));
   const char *sep = find_dir_separator(file);
   *ptr = to_line(sep ? sep + 1 : file);
}

DLLEXPORT
void _std_env_file_path(ffi_uarray_t **ptr)
{
   jit_stack_trace_t *stack LOCAL = jit_stack_trace();
   assert(stack->count > 1);

   const char *file = loc_file_str(&(stack->frames[1].loc));
   const char *sep = find_dir_separator(file);

   if (sep == NULL)
      *ptr = to_absolute_path(".", 1);
   else
      *ptr = to_absolute_path(file, sep - file);
}

DLLEXPORT
int32_t _std_env_file_line(void)
{
   jit_stack_trace_t *stack LOCAL = jit_stack_trace();
   assert(stack->count > 1);

   return stack->frames[1].loc.first_line;
}

void _std_env_init(void)
{
   // Dummy function to force linking
}
