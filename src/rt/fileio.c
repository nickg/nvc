//
//  Copyright (C) 2023  Nick Gasson
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
#include "jit/jit.h"
#include "jit/jit-exits.h"
#include "jit/jit-ffi.h"
#include "rt/rt.h"

#include <assert.h>
#include <stdio.h>
#include <errno.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/stat.h>

#ifdef __MINGW32__
#include <fileapi.h>
#endif

#ifdef HAVE_STDIO_EXT_H
#include <stdio_ext.h>
#endif

#ifndef HAVE_FTELLO
#define ftello(f) (off_t)ftell(f)
#endif

#ifndef HAVE_FSEEKO
#define fseeko(f, o, w) (off_t)fseek(f, o, w)
#endif

typedef enum {
   FILE_ORIGIN_BEGIN,
   FILE_ORIGIN_CURRENT,
   FILE_ORIGIN_END
} file_origin_kind_t;

typedef enum {
   STATE_OPEN,
   STATE_CLOSED
} file_open_state_t;

typedef enum {
   READ_MODE,
   WRITE_MODE,
   APPEND_MODE,
   READ_WRITE_MODE
} file_open_kind_t;

DLLEXPORT
void __nvc_flush(FILE **fp)
{
   if (*fp == NULL)
      jit_msg(NULL, DIAG_FATAL, "FLUSH called on closed file");

   fflush(*fp);
}

DLLEXPORT
void __nvc_rewind(FILE **fp)
{
   if (*fp == NULL)
      jit_msg(NULL, DIAG_FATAL, "FILE_REWIND called on closed file");

   rewind(*fp);
}

DLLEXPORT
void __nvc_seek(FILE **fp, int32_t offset, int8_t origin)
{
   if (*fp == NULL)
      jit_msg(NULL, DIAG_FATAL, "FILE_SEEK called on closed file");

   const int whence[3] = { SEEK_SET, SEEK_CUR, SEEK_END };
   assert(origin >= 0 && origin < ARRAY_LEN(whence));

   if (fseek(*fp, offset, whence[origin]) < 0)
      jit_msg(NULL, DIAG_FATAL, "FILE_SEEK failed: %s", strerror(errno));
}

DLLEXPORT
int8_t __nvc_open3(FILE **fp, const uint8_t *name_ptr, int64_t name_len,
                   int8_t open_kind)
{
   int8_t status;
   x_file_open(&status, (void **)fp, name_ptr, name_len, open_kind);
   return status;
}

DLLEXPORT
void __nvc_truncate(FILE **fp, int32_t size, int8_t origin)
{
   if (*fp == NULL)
      jit_msg(NULL, DIAG_FATAL, "FILE_TRUNCATE called on closed file");

   fflush(*fp);

#ifdef __MINGW32__
   HANDLE handle = (HANDLE)_get_osfhandle(_fileno(*fp));
   LARGE_INTEGER distance = { .QuadPart = size };
   if (!SetFilePointerEx(handle, distance, NULL, origin))
      goto failed;

   if (!SetEndOfFile(handle))
      goto failed;
#else  // !__MINGW32__
   off_t asize = size, oldpos = ftello(*fp);
   switch (origin) {
   case FILE_ORIGIN_END:
      fseek(*fp, 0, SEEK_END);
      asize = ftello(*fp) + size;
      break;
   case FILE_ORIGIN_CURRENT:
      asize = oldpos + size;
      break;
   }

   if (ftruncate(fileno(*fp), asize) != 0)
      goto failed;

   if (oldpos > asize || origin == FILE_ORIGIN_END) {
      if (fseeko(*fp, asize, SEEK_SET) != 0)
         goto failed;
   }

#if defined HAVE_FPURGE
   if (fpurge(*fp) != 0)
      goto failed;
#elif defined HAVE___FPURGE
   __fpurge(*fp);
#endif

#endif  // !__MINGW32__

   return;

 failed:
   jit_msg(NULL, DIAG_FATAL, "FILE_TRUNCATE failed: %s", strerror(errno));
}

DLLEXPORT
int8_t __nvc_file_state(FILE **fp)
{
   return *fp == NULL ? STATE_CLOSED : STATE_OPEN;
}

DLLEXPORT
int8_t __nvc_file_mode(FILE **fp)
{
   if (*fp == NULL)
      jit_msg(NULL, DIAG_FATAL, "FILE_MODE called on closed file");

#ifdef __MINGW32__
   fflush(*fp);

   HANDLE handle = (HANDLE)_get_osfhandle(_fileno(*fp));
   const bool can_read = ReadFile(handle, NULL, 0, NULL, NULL);
   const bool can_write = WriteFile(handle, NULL, 0, NULL, NULL);

   if (can_read && can_write)
      return READ_WRITE_MODE;
   else if (can_read)
      return READ_MODE;
   else if (can_write)
      return WRITE_MODE;
#else
   const int mode = fcntl(fileno(*fp), F_GETFL);
   if (mode < 0)
      jit_msg(NULL, DIAG_FATAL, "FILE_MODE failed: %s", strerror(errno));

   switch (mode & O_ACCMODE) {
   case O_RDONLY: return READ_MODE;
   case O_WRONLY: return (mode & O_APPEND) ? APPEND_MODE : WRITE_MODE;
   case O_RDWR: return READ_WRITE_MODE;
   }
#endif

   jit_msg(NULL, DIAG_WARN, "cannot determine file mode");
   return READ_MODE;
}

DLLEXPORT
int64_t __nvc_file_position(FILE **fp, int8_t origin)
{
   if (*fp == NULL)
      jit_msg(NULL, DIAG_FATAL, "FILE_POSITION called on closed file");

   if (origin == FILE_ORIGIN_CURRENT)
      return 0;

   off_t off = ftello(*fp);
   if (off < 0)
      jit_msg(NULL, DIAG_FATAL, "FILE_POSITION failed: %s", strerror(errno));

   if (origin == FILE_ORIGIN_END) {
      fseeko(*fp, 0, SEEK_END);
      off_t end = ftello(*fp);
      fseeko(*fp, off, SEEK_SET);
      return end - off;
   }
   else
      return off;
}

DLLEXPORT
int64_t __nvc_file_size(FILE **fp, int8_t origin)
{
   if (*fp == NULL)
      jit_msg(NULL, DIAG_FATAL, "FILE_SIZE called on closed file");

   fflush(*fp);

   struct stat st;
   if (fstat(fileno(*fp), &st) < 0)
      jit_msg(NULL, DIAG_FATAL, "FILE_SIZE failed: %s", strerror(errno));

   return st.st_size;
}

DLLEXPORT
int8_t __nvc_file_canseek(FILE **fp)
{
   if (*fp == NULL)
      jit_msg(NULL, DIAG_FATAL, "FILE_CANSEEK called on closed file");

   return fseek(*fp, 0, SEEK_CUR) == 0;
}

void _file_io_init(void)
{
   // Dummy function to force linking
}
