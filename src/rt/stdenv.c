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
#include "scan.h"
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
#include <dirent.h>
#include <time.h>
#include <unistd.h>

#ifdef __MINGW32__
#include <timezoneapi.h>

#define gmtime_r(t, tm) gmtime_s(tm, t);
#define localtime_r(t, tm) localtime_s(tm, t);
#endif

typedef struct {
   int64_t microsecond;
   int64_t second;
   int64_t minute;
   int64_t hour;
   int64_t day;
   int64_t month;
   int64_t year;
   int8_t  weekday;
   int64_t dayofyear;
} time_record_t;

typedef struct {
   ffi_uarray_t *name;
   ffi_uarray_t *items;
} directory_t;

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

typedef enum {
   FILE_DELETE_STATUS_OK = 0,
   FILE_DELETE_STATUS_NO_FILE = 1,
   FILE_DELETE_STATUS_ACCESS_DENIED = 2,
   FILE_DELETE_STATUS_ERROR = 3
} file_delete_status_t;

typedef enum {
   DIR_DELETE_STATUS_OK = 0,
   DIR_DELETE_STATUS_NO_DIRECTORY = 1,
   DIR_DELETE_STATUS_NOT_EMPTY = 2,
   DIR_DELETE_STATUS_ACCESS_DENIED = 3,
   DIR_DELETE_STATUS_ERROR = 4
} dir_delete_status_t;

typedef struct {
   ffi_uarray_t *name;
   ffi_uarray_t *file_name;
   ffi_uarray_t *file_path;
   int64_t       file_line;
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

static int8_t errno_to_file_delete_status(void)
{
   switch (errno) {
   case 0: return FILE_DELETE_STATUS_OK;
   case ENOENT: return FILE_DELETE_STATUS_NO_FILE;
   case EACCES: return FILE_DELETE_STATUS_ACCESS_DENIED;
   default: return FILE_DELETE_STATUS_ERROR;
   }
}

static int8_t errno_to_dir_delete_status(void)
{
   switch (errno) {
   case 0: return DIR_DELETE_STATUS_OK;
   case ENOENT:
   case ENOTDIR: return DIR_DELETE_STATUS_NO_DIRECTORY;
   case ENOTEMPTY: return DIR_DELETE_STATUS_NOT_EMPTY;
   case EACCES: return DIR_DELETE_STATUS_ACCESS_DENIED;
   default: return DIR_DELETE_STATUS_ERROR;
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
   if (is_absolute_path(input))
      return to_line_n(input, len);

   char buf[PATH_MAX];
   if (realpath(input, buf) == NULL)
      return to_line(input);
   else
      return to_line(buf);
}

static void to_time_record(const struct tm *tm, int us, time_record_t *tr)
{
   assert(us >= 0);
   assert(us < 1000000);

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

static time_t to_time_t(const time_record_t *tr)
{
   const int adj_year = tr->year - 1900;
   return tr->second + tr->minute*60 + tr->hour*3600 + tr->dayofyear*86400
      + (adj_year - 70) * 31536000 + ((adj_year - 69) / 4) * 86400
      - ((adj_year - 1) / 100)*86400 + ((adj_year + 299) / 400) * 86400;
}

DLLEXPORT
void _std_env_stop(int32_t finish, int32_t have_status, int32_t status)
{
   if (have_status) {
      notef("%s called with status %d", finish ? "FINISH" : "STOP", status);
      jit_abort_with_status(status);
   }
   else {
      notef("%s called", finish ? "FINISH" : "STOP");
      jit_abort();
   }
}

DLLEXPORT
void _std_env_getenv(const uint8_t *name_ptr, int64_t name_len, ffi_uarray_t *u)
{
   char *LOCAL cstr = to_cstring(name_ptr, name_len);

   // LRM19 section 16.5.6: conditional analysis identifiers are part of
   // the queried environment and take precedence over possibly
   // inherited environment variables of identical names.
   const char *pp = pp_defines_get(cstr);
   if (pp != NULL)
      copy_str(pp, u);
   else {
      const char *env = getenv(cstr);
      if (env == NULL)
         *u = ffi_wrap(NULL, 1, 0);
      else
         copy_str(env, u);
   }
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
   const uint64_t nanos = get_real_time();

   // Be careful to avoid loss of precision here
   double real = nanos / UINT64_C(1000000000);
   real += 1.0e9 / (nanos % UINT64_C(1000000000));

   return real;
}

DLLEXPORT
double _std_env_epoch_trec(const time_record_t *tr)
{
   const time_t secs = to_time_t(tr);
   return (double)secs + (double)tr->microsecond / 1e6;
}

DLLEXPORT
void _std_env_localtime(double time, time_record_t *tr)
{
   double fsecs = floor(time);
   time_t secs = (time_t)fsecs;

   struct tm tm;
   localtime_r(&secs, &tm);

   to_time_record(&tm, (int)(1e6 * (time - fsecs)), tr);
}

DLLEXPORT
void _std_env_localtime_trec(const time_record_t *tr, time_record_t *result)
{
   time_t time = to_time_t(tr);

   struct tm tm = {};
   localtime_r(&time, &tm);

   to_time_record(&tm, tr->microsecond, result);
}

DLLEXPORT
void _std_env_gmtime(double time, time_record_t *tr)
{
   double fsecs = floor(time);
   time_t secs = (int)fsecs;

   struct tm tm;
   gmtime_r(&secs, &tm);

   to_time_record(&tm, (int)(1e6 * (time - fsecs)), tr);
}

DLLEXPORT
void _std_env_gmtime_trec(const time_record_t *tr, time_record_t *result)
{
   struct tm tm = {};
   time_t time = to_time_t(tr);

#ifdef __MINGW32__
   TIME_ZONE_INFORMATION tz;
   switch (GetTimeZoneInformation(&tz)) {
   case TIME_ZONE_ID_UNKNOWN:
      time += tz.Bias * 60;
      break;
   case TIME_ZONE_ID_STANDARD:
      time += (tz.Bias + tz.StandardBias) * 60;
      break;
   case TIME_ZONE_ID_DAYLIGHT:
      time += (tz.Bias + tz.DaylightBias) * 60;
      break;
   }
#else
   // Call localtime to get GMT offset
   localtime_r(&time, &tm);
   time -= tm.tm_gmtoff;
#endif

   gmtime_r(&time, &tm);

   to_time_record(&tm, tr->microsecond, result);
}

DLLEXPORT
void _std_env_add_trec_real(const time_record_t *tr, double delta,
                            time_record_t *result)
{
   const int micro = (int)(fmod(delta, 1.0) * 1e6);
   const time_t secs = (time_t)trunc(delta);

   time_t newsecs = to_time_t(tr) + secs;
   int newmicro = tr->microsecond + micro;

   if (newmicro >= 1000000) {
      newmicro %= 1000000;
      newsecs++;
   }
   else if (newmicro < 0) {
      newmicro = 1000000 + newmicro % 100000;
      newsecs--;
   }

   struct tm tm = {};
   gmtime_r(&newsecs, &tm);

   to_time_record(&tm, newmicro, result);
}

DLLEXPORT
double _std_env_diff_trec(const time_record_t *tr1, const time_record_t *tr2)
{
   const time_t diff = to_time_t(tr1) - to_time_t(tr2);
   const int microdiff = tr1->microsecond - tr2->microsecond;

   return (double)diff + (double)microdiff / 1e6;
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

   file_info_t info;
   return get_file_info(path, &info);
}

DLLEXPORT
bool _std_env_itemisfile(const uint8_t *path_ptr, int64_t path_len)
{
   char *path LOCAL = to_cstring(path_ptr, path_len);

   file_info_t info;
   if (!get_file_info(path, &info))
      return false;

   return info.type == FILE_REGULAR;
}

DLLEXPORT
bool _std_env_itemisdir(const uint8_t *path_ptr, int64_t path_len)
{
   char *path LOCAL = to_cstring(path_ptr, path_len);

   file_info_t info;
   if (!get_file_info(path, &info))
      return false;

   return info.type == FILE_DIR;
}

DLLEXPORT
void _std_env_deletefile(const uint8_t *path_ptr, int64_t path_len,
                         int8_t *status)
{
   char *path LOCAL = to_cstring(path_ptr, path_len);

   if (remove(path) == -1)
      *status = errno_to_file_delete_status();
   else
      *status = FILE_DELETE_STATUS_OK;
}

static int8_t delete_tree(const char *path)
{
   DIR *d = opendir(path);
   if (d == NULL)
      return errno_to_dir_delete_status();

   LOCAL_TEXT_BUF tb = tb_new();
   int8_t status = DIR_DELETE_STATUS_OK;

   struct dirent *e;
   while (status == DIR_DELETE_STATUS_OK && (e = readdir(d))) {
      if (strcmp(e->d_name, ".") == 0 || strcmp(e->d_name, "..") == 0)
         continue;

      tb_rewind(tb);
      tb_cat(tb, path);
      tb_cat(tb, DIR_SEP);
      tb_cat(tb, e->d_name);

#if defined __MINGW32__ || !defined DT_DIR
      file_info_t info;
      get_file_info(tb_get(tb), &info);

      const bool is_dir = (info.type == FILE_DIR);
#else
      const bool is_dir = (e->d_type == DT_DIR);
#endif

      if (is_dir)
         status = delete_tree(tb_get(tb));
      else if (remove(tb_get(tb)) == -1)
         status = errno_to_dir_delete_status();
   }

   closedir(d);

   if (status == DIR_DELETE_STATUS_OK && rmdir(path) == -1)
      return errno_to_dir_delete_status();

   return status;
}

DLLEXPORT
void _std_env_deletedir(const uint8_t *path_ptr, int64_t path_len,
                        int8_t recursive, int8_t *status)
{
   char *path LOCAL = to_cstring(path_ptr, path_len);

   if (recursive)
      *status = delete_tree(path);
   else if (rmdir(path) == -1)
      *status = errno_to_dir_delete_status();
   else
      *status = DIR_DELETE_STATUS_OK;
}

DLLEXPORT
void _std_env_dir_open(const uint8_t *path_ptr, int64_t path_len,
                       directory_t *dir, int8_t *status)
{
   char *path LOCAL = to_cstring(path_ptr, path_len);

   DIR *d = opendir(path);
   if (d == NULL) {
      *status = errno_to_dir_open_status();
      return;
   }

   char resolved[PATH_MAX];
   realpath(path, resolved);

   const size_t resolvedsz = strlen(resolved);
   size_t memsz = sizeof(ffi_uarray_t)*2 + resolvedsz;
   struct dirent *e;
   int count = 0;
   while ((e = readdir(d))) {
      const size_t nchars = strlen(e->d_name);
      memsz += sizeof(ffi_uarray_t) + ALIGN_UP(nchars, 8);
      count++;
   }
   memsz += count * sizeof(ffi_uarray_t *);

   rewinddir(d);

   void *mem = jit_mspace_alloc(memsz), *next = mem;
   dir->items = next;
   dir->items->dims[0].left = 0;
   dir->items->dims[0].length = count;
   dir->items->ptr = dir->items + 1;

   next += sizeof(ffi_uarray_t) + count * sizeof(ffi_uarray_t *);

   for (int nth = 0; (e = readdir(d)); nth++) {
      const size_t nchars = strlen(e->d_name);

      ffi_uarray_t *u = next;
      u->ptr = u + 1;
      u->dims[0].left = 1;
      u->dims[0].length = nchars;
      memcpy(u->ptr, e->d_name, nchars);

      *((ffi_uarray_t **)dir->items->ptr + nth) = u;

      next += sizeof(ffi_uarray_t) + ALIGN_UP(nchars, 8);
   }

   closedir(d);

   dir->name = next;
   dir->name->ptr = dir->name + 1;
   dir->name->dims[0].left = 1;
   dir->name->dims[0].length = resolvedsz;

   memcpy(dir->name->ptr, resolved, resolvedsz);

   next += sizeof(ffi_uarray_t) + resolvedsz;
   assert(next == mem + memsz);

   *status = 0;
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

DLLEXPORT
int64_t _std_env_seconds_to_time(double real)
{
   double whole, frac = modf(real, &whole);
   return (int64_t)whole * UINT64_C(1000000000000000) + (int64_t)(frac * 1e15);
}

void _std_env_init(void)
{
   // Dummy function to force linking
}
