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

#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <string.h>

static const char    *file_start;
static size_t         file_sz;
static bool           last_was_newline = true;
static const char    *read_ptr;
static source_kind_t  src_kind;
static loc_file_ref_t file_ref = FILE_INVALID;
static int            n_token_next_start = 0;
static int            n_row = 0;

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

   read_ptr           = file_start;
   last_was_newline   = true;
   file_ref           = loc_file_ref(file, file_start);
   n_row              = 0;
   n_token_next_start = 0;
}

source_kind_t source_kind(void)
{
   return src_kind;
}

int get_next_char(char *b, int max_buffer)
{
   if (last_was_newline) {
      n_row += 1;
      last_was_newline = false;
   }

   const bool eof = read_ptr >= file_start + file_sz;
   if (eof)
      return 0;
   else
      *b = *read_ptr++;

   if (*b == '\n')
      last_was_newline = true;

   return *b == 0 ? 0 : 1;
}

void begin_token(char *tok)
{
   const char *newline = strrchr(tok, '\n');
   int n_token_start, n_token_length;
   if (newline != NULL) {
      n_token_start = 0;
      n_token_length = strlen(tok) - (newline - tok);
      n_token_next_start = n_token_length - 1;
   }
   else {
      n_token_start = n_token_next_start;
      n_token_length = strlen(tok);
      n_token_next_start += n_token_length;
   }

   const int last_col = n_token_start + n_token_length - 1;

   extern loc_t yylloc;
   yylloc = get_loc(MIN(n_row, LINE_INVALID),
                    MIN(n_token_start, COLUMN_INVALID),
                    MIN(n_row, LINE_INVALID),
                    MIN(last_col, COLUMN_INVALID),
                    file_ref);
}
