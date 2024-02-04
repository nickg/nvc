//
//  Copyright (C) 2023-2024  Nick Gasson
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
void __nvc_flush(jit_scalar_t *args)
{
   FILE **fp = args[0].pointer;

   if (*fp == NULL)
      jit_msg(NULL, DIAG_FATAL, "FLUSH called on closed file");

   fflush(*fp);
}

DLLEXPORT
void __nvc_rewind(jit_scalar_t *args)
{
   FILE **fp = args[0].pointer;

   if (*fp == NULL)
      jit_msg(NULL, DIAG_FATAL, "FILE_REWIND called on closed file");

   rewind(*fp);
}

DLLEXPORT
void __nvc_seek(jit_scalar_t *args)
{
   FILE **fp = args[0].pointer;
   off_t offset = args[1].integer;
   int8_t origin = args[2].integer;

   if (*fp == NULL)
      jit_msg(NULL, DIAG_FATAL, "FILE_SEEK called on closed file");

   const int whence[3] = { SEEK_SET, SEEK_CUR, SEEK_END };
   assert(origin >= 0 && origin < ARRAY_LEN(whence));

   if (fseeko(*fp, offset, whence[origin]) < 0)
      jit_msg(NULL, DIAG_FATAL, "FILE_SEEK failed: %s", strerror(errno));
}

DLLEXPORT
void __nvc_truncate(jit_scalar_t *args)
{
   FILE **fp = args[0].pointer;
   int64_t size = args[1].integer;
   int8_t origin = args[2].integer;

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
void __nvc_file_state(jit_scalar_t *args)
{
   FILE **fp = args[0].pointer;
   int8_t *result = args[1].pointer;

   *result = (*fp == NULL ? STATE_CLOSED : STATE_OPEN);
}

DLLEXPORT
void __nvc_file_mode(jit_scalar_t *args)
{
   FILE **fp = args[0].pointer;
   int8_t *result = args[1].pointer;

   if (*fp == NULL)
      jit_msg(NULL, DIAG_FATAL, "FILE_MODE called on closed file");

   fflush(*fp);

   file_mode_t mode;
   if (!get_handle_mode(fileno(*fp), &mode)) {
      jit_msg(NULL, DIAG_WARN, "cannot determine file mode");
      *result = READ_MODE;
   }
   else
      *result = mode;
}

DLLEXPORT
void __nvc_file_position(jit_scalar_t *args)
{
   FILE **fp = args[0].pointer;
   int8_t origin = args[1].integer;
   int64_t *result = args[2].pointer;

   if (*fp == NULL)
      jit_msg(NULL, DIAG_FATAL, "FILE_POSITION called on closed file");

   off_t off = ftello(*fp);
   if (off < 0)
      jit_msg(NULL, DIAG_FATAL, "FILE_POSITION failed: %s", strerror(errno));

   switch (origin) {
   case FILE_ORIGIN_BEGIN:
      *result = off;
      break;
   case FILE_ORIGIN_END:
      {
         fseeko(*fp, 0, SEEK_END);
         off_t end = ftello(*fp);
         fseeko(*fp, off, SEEK_SET);
         *result = end - off;
      }
      break;
   case FILE_ORIGIN_CURRENT:
      *result = 0;
      break;
   }
}

DLLEXPORT
void __nvc_file_size(jit_scalar_t *args)
{
   FILE **fp = args[0].pointer;
   int64_t *result = args[1].pointer;

   if (*fp == NULL)
      jit_msg(NULL, DIAG_FATAL, "FILE_SIZE called on closed file");

   fflush(*fp);

   file_info_t info;
   if (!get_handle_info(fileno(*fp), &info))
      jit_msg(NULL, DIAG_FATAL, "FILE_SIZE failed: %s", strerror(errno));

   *result = info.size;
}

DLLEXPORT
void __nvc_file_canseek(jit_scalar_t *args)
{
   FILE **fp = args[0].pointer;
   int8_t *result = args[1].pointer;

   if (*fp == NULL)
      jit_msg(NULL, DIAG_FATAL, "FILE_CANSEEK called on closed file");

   *result = fseek(*fp, 0, SEEK_CUR) == 0;
}

void _file_io_init(void)
{
   // Dummy function to force linking
}
