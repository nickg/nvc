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
#include "diag.h"
#include "jit/jit.h"
#include "jit/jit-exits.h"
#include "lib.h"
#include "rt/rt.h"

#include <assert.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void x_file_open(int8_t *status, void **_fp, uint8_t *name_bytes,
                 int32_t name_len, int8_t mode, tree_t where)
{
   FILE **fp = (FILE **)_fp;

   char *fname LOCAL = xmalloc(name_len + 1);
   memcpy(fname, name_bytes, name_len);
   fname[name_len] = '\0';

   const char *mode_str[] = {
      "rb", "wb", "w+b"
   };
   assert(mode < ARRAY_LEN(mode_str));

   if (status != NULL)
      *status = OPEN_OK;

   if (*fp != NULL) {
      if (status == NULL)
         jit_msg(tree_loc(where), DIAG_FATAL, "file object already associated "
                 "with an external file");
      else
         *status = STATUS_ERROR;
   }
   else if (name_len == 0) {
      if (status == NULL)
         jit_msg(tree_loc(where), DIAG_FATAL, "empty file name in FILE_OPEN");
      else
         *status = NAME_ERROR;
   }
   else if (strcmp(fname, "STD_INPUT") == 0)
      *fp = stdin;
   else if (strcmp(fname, "STD_OUTPUT") == 0)
      *fp = stdout;
   else {
#ifdef __MINGW32__
      const bool failed = (fopen_s(fp, fname, mode_str[mode]) != 0);
#else
      const bool failed = ((*fp = fopen(fname, mode_str[mode])) == NULL);
#endif
      if (failed) {
         if (status == NULL)
            jit_msg(tree_loc(where), DIAG_FATAL, "failed to open %s: %s", fname,
                    strerror(errno));
         else {
            switch (errno) {
            case ENOENT: *status = NAME_ERROR; break;
            case EPERM:  *status = MODE_ERROR; break;
            default:     *status = NAME_ERROR; break;
            }
         }
      }
   }
}

void x_file_write(void **_fp, uint8_t *data, int32_t len)
{
   FILE **fp = (FILE **)_fp;

   if (*fp == NULL)
      jit_msg(NULL, DIAG_FATAL, "write to closed file");

   fwrite(data, 1, len, *fp);
}

void x_file_read(void **_fp, uint8_t *data, int32_t size, int32_t count,
                 int32_t *out)
{
   FILE **fp = (FILE **)_fp;

   if (*fp == NULL)
      jit_msg(NULL, DIAG_FATAL, "read from closed file");

   size_t n = fread(data, size, count, *fp);
   if (out != NULL)
      *out = n;
}

void x_file_close(void **_fp)
{
   FILE **fp = (FILE **)_fp;

   if (*fp != NULL) {
      fclose(*fp);
      *fp = NULL;
   }
}

int8_t x_endfile(void *_f)
{
   FILE *f = _f;

   if (f == NULL)
      jit_msg(NULL, DIAG_FATAL, "ENDFILE called on closed file");

   int c = fgetc(f);
   if (c == EOF)
      return 1;
   else {
      ungetc(c, f);
      return 0;
   }
}

void x_file_flush(void *_f)
{
   if (_f == NULL)
      jit_msg(NULL, DIAG_FATAL, "FLUSH called on closed file");

   fflush(_f);
}

////////////////////////////////////////////////////////////////////////////////
// Entry points from compiled code

// Helper macro for passing debug loci from LLVM
#define DEBUG_LOCUS(name) \
   const char *name##_unit, uint32_t name##_offset

static inline tree_t locus_to_tree(const char *unit, unsigned offset)
{
   if (unit == NULL)
      return NULL;
   else
      return tree_from_locus(ident_new(unit), offset, lib_get_qualified);
}

DLLEXPORT
void _file_open(int8_t *status, void **_fp, uint8_t *name_bytes,
                int32_t name_len, int8_t mode, DEBUG_LOCUS(locus))
{
   tree_t where = locus_to_tree(locus_unit, locus_offset);

   x_file_open(status, _fp, name_bytes, name_len, mode, where);
}

DLLEXPORT
void _file_write(void **_fp, uint8_t *data, int32_t len)
{
   x_file_write(_fp, data, len);
}

DLLEXPORT
void _file_read(void **_fp, uint8_t *data, int32_t size, int32_t count,
                int32_t *out)
{
   x_file_read(_fp, data, size, count, out);
}

DLLEXPORT
void _file_close(void **_fp)
{
   x_file_close(_fp);
}

DLLEXPORT
int8_t _endfile(void *_f)
{
   return x_endfile(_f);
}

DLLEXPORT
void __nvc_flush(FILE *f)
{
   x_file_flush(f);
}
