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
#include "jit/jit-exits.h"
#include "jit/jit-ffi.h"
#include "jit/jit.h"
#include "rt/fileio.h"
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
#include <share.h>
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
   OPEN_OK      = 0,
   STATUS_ERROR = 1,
   NAME_ERROR   = 2,
   MODE_ERROR   = 3,
} file_open_status_t;

typedef enum {
   READ_MODE,
   WRITE_MODE,
   APPEND_MODE,
   READ_WRITE_MODE
} file_open_kind_t;

typedef enum {
   FILE_USER,
   FILE_PREDEF,
} file_kind_t;

typedef struct {
   FILE        *file;
   char        *name;
   file_kind_t  kind;
   file_mode_t  mode;
   uint16_t     generation;
} file_slot_t;

#define HANDLE_BITS      (sizeof(file_handle_t) * 8)
#define HANDLE_MAX_INDEX ((UINT64_C(1) << (HANDLE_BITS / 2)) - 1)

static file_slot_t *handles;
static unsigned     num_handles;
static unsigned     free_hint;

static file_slot_t *decode_handle(file_handle_t handle)
{
   const uintptr_t bits = (uintptr_t)handle;
   const uint32_t index = bits & HANDLE_MAX_INDEX;
   const uint32_t generation = bits >> HANDLE_BITS/2;

   if (handle == 0 || index >= num_handles)
      return NULL;

   file_slot_t *slot = &(handles[index]);
   if (slot->file == NULL)
      return NULL;
   else if (slot->generation != generation)
      return NULL;   // Use-after-free

   return slot;
}

static file_handle_t handle_for(FILE *f, const char *name, file_kind_t kind,
                                file_mode_t mode)
{
   assert(f != NULL);

   uint32_t index = free_hint;
   if (index >= num_handles || handles[index].file != NULL) {
      for (index = 0; index < num_handles; index++) {
         if (handles[index].file == NULL
             && handles[index].generation < HANDLE_MAX_INDEX)
            break;
      }
   }

   if (index > HANDLE_MAX_INDEX)
      fatal("too many active file handles");
   else if (index == num_handles) {
      const int new_size = MAX(num_handles * 2, 16);
      handles = xrealloc_array(handles, new_size, sizeof(file_slot_t));
      num_handles = new_size;

      for (int i = index; i < new_size; i++) {
         handles[i].file = NULL;
         handles[i].generation = 1;
      }
   }

   file_slot_t *slot = &(handles[index]);
   slot->file = f;
   slot->name = xstrdup(name);
   slot->kind = kind;
   slot->mode = mode;

   free_hint = index + 1;

   return (file_handle_t)slot->generation << HANDLE_BITS/2 | index;
}

static void drop_handle(file_handle_t handle)
{
   file_slot_t *slot = decode_handle(handle);
   if (slot == NULL)
      return;

   if (slot->kind == FILE_USER)
      fclose(slot->file);

   free(slot->name);

   slot->file = NULL;
   slot->generation++;

   free_hint = slot - handles;
}

bool file_mode(file_handle_t fh, file_mode_t *mode)
{
   file_slot_t *slot = decode_handle(fh);
   if (slot == NULL)
      return false;

   *mode = slot->mode;
   return true;
}

bool file_logical_name(file_handle_t fh, const char **name)
{
   file_slot_t *slot = decode_handle(fh);
   if (slot == NULL)
      return false;

   *name = slot->name;
   return true;
}

void clear_file_handles(void)
{
   for (int i = 0; i < num_handles; i++) {

   }
}

DLLEXPORT
void __nvc_file_close(jit_scalar_t *args)
{
   file_handle_t *handle = args[2].pointer;
   drop_handle(*handle);
   *handle = 0;
}

DLLEXPORT
void __nvc_endfile(jit_scalar_t *args)
{
   file_handle_t *handle = args[1].pointer;

   file_slot_t *slot = decode_handle(*handle);
   if (slot == NULL)
      jit_msg(NULL, DIAG_FATAL, "ENDFILE called on closed file");

   int c = fgetc(slot->file);
   if (c == EOF)
      args[0].integer = 1;
   else {
      ungetc(c, slot->file);
      args[0].integer = 0;
   }
}

DLLEXPORT
void __nvc_flush(jit_scalar_t *args)
{
   file_handle_t *handle = args[2].pointer;

   file_slot_t *slot = decode_handle(*handle);
   if (slot == NULL)
      jit_msg(NULL, DIAG_FATAL, "FLUSH called on closed file");

   fflush(slot->file);
}

DLLEXPORT
void __nvc_rewind(jit_scalar_t *args)
{
   file_handle_t *handle = args[2].pointer;

   file_slot_t *slot = decode_handle(*handle);
   if (slot == NULL)
      jit_msg(NULL, DIAG_FATAL, "FILE_REWIND called on closed file");

   rewind(slot->file);
}

DLLEXPORT
void __nvc_seek(jit_scalar_t *args)
{
   file_handle_t *handle = args[2].pointer;
   off_t offset = args[3].integer;
   int8_t origin = args[4].integer;

   file_slot_t *slot = decode_handle(*handle);
   if (slot == NULL)
      jit_msg(NULL, DIAG_FATAL, "FILE_SEEK called on closed file");

   const int whence[3] = { SEEK_SET, SEEK_CUR, SEEK_END };
   assert(origin >= 0 && origin < ARRAY_LEN(whence));

   if (fseeko(slot->file, offset, whence[origin]) < 0)
      jit_msg(NULL, DIAG_FATAL, "FILE_SEEK failed: %s", strerror(errno));
}

DLLEXPORT
void __nvc_truncate(jit_scalar_t *args)
{
   file_handle_t *handle = args[2].pointer;
   int64_t size = args[3].integer;
   int8_t origin = args[4].integer;

   file_slot_t *slot = decode_handle(*handle);
   if (slot == NULL)
      jit_msg(NULL, DIAG_FATAL, "FILE_TRUNCATE called on closed file");

   fflush(slot->file);

#ifdef __MINGW32__
   HANDLE os_handle = (HANDLE)_get_osfhandle(_fileno(slot->file));
   LARGE_INTEGER distance = { .QuadPart = size };
   if (!SetFilePointerEx(os_handle, distance, NULL, origin))
      goto failed;

   if (!SetEndOfFile(os_handle))
      goto failed;
#else  // !__MINGW32__
   off_t asize = size, oldpos = ftello(slot->file);
   switch (origin) {
   case FILE_ORIGIN_END:
      fseek(slot->file, 0, SEEK_END);
      asize = ftello(slot->file) + size;
      break;
   case FILE_ORIGIN_CURRENT:
      asize = oldpos + size;
      break;
   }

   if (ftruncate(fileno(slot->file), asize) != 0)
      goto failed;

   if (oldpos > asize || origin == FILE_ORIGIN_END) {
      if (fseeko(slot->file, asize, SEEK_SET) != 0)
         goto failed;
   }

#if defined HAVE_FPURGE
   if (fpurge(slot->file) != 0)
      goto failed;
#elif defined HAVE___FPURGE
   __fpurge(slot->file);
#endif

#endif  // !__MINGW32__

   return;

 failed:
   jit_msg(NULL, DIAG_FATAL, "FILE_TRUNCATE failed: %s", strerror(errno));
}

DLLEXPORT
void __nvc_file_state(jit_scalar_t *args)
{
   file_handle_t *handle = args[1].pointer;

   file_slot_t *slot = decode_handle(*handle);
   args[0].integer = (slot == NULL ? STATE_CLOSED : STATE_OPEN);
}

DLLEXPORT
void __nvc_file_mode(jit_scalar_t *args)
{
   file_handle_t *handle = args[1].pointer;

   file_slot_t *slot = decode_handle(*handle);
   if (slot == NULL)
      jit_msg(NULL, DIAG_FATAL, "FILE_MODE called on closed file");

   args[0].integer = slot->mode;
}

DLLEXPORT
void __nvc_file_position(jit_scalar_t *args)
{
   file_handle_t *handle = args[1].pointer;
   int8_t origin = args[2].integer;

   file_slot_t *slot = decode_handle(*handle);
   if (slot == NULL)
      jit_msg(NULL, DIAG_FATAL, "FILE_POSITION called on closed file");

   off_t off = ftello(slot->file);
   if (off < 0)
      jit_msg(NULL, DIAG_FATAL, "FILE_POSITION failed: %s", strerror(errno));

   switch (origin) {
   case FILE_ORIGIN_BEGIN:
      args[0].integer = off;
      break;
   case FILE_ORIGIN_END:
      {
         fseeko(slot->file, 0, SEEK_END);
         off_t end = ftello(slot->file);
         fseeko(slot->file, off, SEEK_SET);
         args[0].integer = end - off;
      }
      break;
   case FILE_ORIGIN_CURRENT:
      args[0].integer = 0;
      break;
   }
}

DLLEXPORT
void __nvc_file_size(jit_scalar_t *args)
{
   file_handle_t *handle = args[1].pointer;

   file_slot_t *slot = decode_handle(*handle);
   if (slot->file == NULL)
      jit_msg(NULL, DIAG_FATAL, "FILE_SIZE called on closed file");

   fflush(slot->file);

   file_info_t info;
   if (!get_handle_info(fileno(slot->file), &info))
      jit_msg(NULL, DIAG_FATAL, "FILE_SIZE failed: %s", strerror(errno));

   args[0].integer = info.size;
}

DLLEXPORT
void __nvc_file_canseek(jit_scalar_t *args)
{
   file_handle_t *handle = args[1].pointer;

   file_slot_t *slot = decode_handle(*handle);
   if (slot->file == NULL)
      jit_msg(NULL, DIAG_FATAL, "FILE_CANSEEK called on closed file");

   args[0].integer = fseek(slot->file, 0, SEEK_CUR) == 0;
}

void x_file_open(int8_t *status, void **_fp, const uint8_t *name_bytes,
                 int32_t name_len, int8_t mode)
{
   file_handle_t *handle = (file_handle_t *)_fp;

   char *fname LOCAL = xmalloc(name_len + 1);
   memcpy(fname, name_bytes, name_len);
   fname[name_len] = '\0';

   const char *mode_str[] = {
      "rb", "wb", "ab", "r+"
   };
   assert(mode < ARRAY_LEN(mode_str));

   if (status != NULL)
      *status = OPEN_OK;

   if (*handle != 0) {
      if (status == NULL)
         jit_msg(NULL, DIAG_FATAL, "file object already associated "
                 "with an external file");
      else
         *status = STATUS_ERROR;
   }
   else if (name_len == 0) {
      if (status == NULL)
         jit_msg(NULL, DIAG_FATAL, "empty file name in FILE_OPEN");
      else
         *status = NAME_ERROR;
   }
   else if (strcmp(fname, "STD_INPUT") == 0)
      *handle = handle_for(stdin, fname, FILE_PREDEF, mode);
   else if (strcmp(fname, "STD_OUTPUT") == 0)
      *handle = handle_for(stdout, fname, FILE_PREDEF, mode);
   else {
#ifdef __MINGW32__
      FILE *fp = _fsopen(fname, mode_str[mode], _SH_DENYNO);
#else
      FILE *fp = fopen(fname, mode_str[mode]);
#endif
      if (fp == NULL) {
         if (status == NULL)
            jit_msg(NULL, DIAG_FATAL, "failed to open %s: %s", fname,
                    strerror(errno));
         else {
            switch (errno) {
            case ENOENT: *status = NAME_ERROR; break;
            case EACCES: *status = MODE_ERROR; break;
            default:     *status = NAME_ERROR; break;
            }
         }
      }
      else
         *handle = handle_for(fp, fname, FILE_USER, mode);
   }
}

void x_file_write(void **_fp, void *data, int64_t size, int64_t count)
{
   file_handle_t *handle = (file_handle_t *)_fp;

   file_slot_t *slot = decode_handle(*handle);
   if (slot == NULL)
      jit_msg(NULL, DIAG_FATAL, "write to closed file");

   if (fwrite(data, size, count, slot->file) != count)
      jit_msg(NULL, DIAG_FATAL, "write to file failed");
}

int64_t x_file_read(void **_fp, void *data, int64_t size, int64_t count)
{
   file_handle_t *handle = (file_handle_t *)_fp;

   file_slot_t *slot = decode_handle(*handle);
   if (slot == NULL)
      jit_msg(NULL, DIAG_FATAL, "read from closed file");

   const unsigned long actual = fread(data, size, count, slot->file);
   if (actual != count && ferror(slot->file))
      jit_msg(NULL, DIAG_FATAL, "read from file failed");

   return actual;
}

void _file_io_init(void)
{
   // Dummy function to force linking
}
