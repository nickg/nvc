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
#include "scan.h"

#include <assert.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <string.h>

static const char    *file_start;
static size_t         file_sz;
static const char    *read_ptr;
static source_kind_t  src_kind;
static loc_file_ref_t file_ref = FILE_INVALID;
static int            colno;
static int            lineno;

void input_from_file(const char *file)
{
   int fd = open(file, O_RDONLY);
   if (fd < 0)
      fatal_errno("opening %s", file);

   struct stat buf;
   if (fstat(fd, &buf) != 0)
      fatal_errno("fstat");

   if (!S_ISREG(buf.st_mode))
      fatal("opening %s: not a regular file", file);

   file_sz = buf.st_size;

   if (file_sz > 0)
      file_start = map_file(fd, file_sz);
   else
      file_start = NULL;

   close(fd);

   size_t len = strlen(file);
   if (len > 2 && file[len - 2] == '.' && file[len - 1] == 'v') {
      src_kind = SOURCE_VERILOG;
      fatal("sorry, Verilog is not currently supported");
   }
   else {
      src_kind = SOURCE_VHDL;
      reset_vhdl_parser();
   }

   read_ptr = file_start;
   file_ref = loc_file_ref(file, file_start);
   lineno   = 1;
   colno    = 0;
}

source_kind_t source_kind(void)
{
   return src_kind;
}

int get_next_char(char *b, int max_buffer)
{
   const ptrdiff_t navail = file_start + file_sz - read_ptr;
   assert(navail >= 0);

   const int nchars = MIN(navail, max_buffer);

   memcpy(b, read_ptr, nchars);
   read_ptr += nchars;

   return nchars;
}

void begin_token(char *tok, int length)
{
   // Newline must match as a single token for the logic below to work
   assert(strchr(tok, '\n') == NULL || length == 1);

   const int first_col = colno;
   if (*tok == '\n') {
      colno = 0;
      lineno += 1;
   }
   else
      colno += length;

   const int last_col = first_col + length - 1;

   extern loc_t yylloc;
   yylloc = get_loc(lineno, first_col, lineno, last_col, file_ref);
}
