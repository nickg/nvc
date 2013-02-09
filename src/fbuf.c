//
//  Copyright (C) 2013  Nick Gasson
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
#include "fbuf.h"

#include <stdlib.h>
#include <string.h>

struct fbuf {
   FILE *file;
};

fbuf_t *fbuf_open(const char *file, fbuf_mode_t mode)
{
   FILE *h = fopen(file, (mode == FBUF_IN) ? "r" : "w");
   if (h == NULL)
      return NULL;

   fbuf_t *f = xmalloc(sizeof(struct fbuf));
   f->file = h;

   return f;
}

void fbuf_close(fbuf_t *f)
{
   fclose(f->file);
   free(f);
}

void write_u32(uint32_t u, fbuf_t *f)
{
   if (fwrite(&u, sizeof(uint32_t), 1, f->file) != 1)
      fatal("fwrite failed");
}

void write_i32(int32_t i, fbuf_t *f)
{
   if (fwrite(&i, sizeof(int32_t), 1, f->file) != 1)
      fatal("fwrite failed");
}

void write_i64(int64_t i, fbuf_t *f)
{
   if (fwrite(&i, sizeof(int64_t), 1, f->file) != 1)
      fatal("fwrite failed");
}

void write_u16(uint16_t s, fbuf_t *f)
{
   if (fwrite(&s, sizeof(uint16_t), 1, f->file) != 1)
      fatal("fwrite failed");
}

void write_u8(uint8_t u, fbuf_t *f)
{
   if (fwrite(&u, sizeof(uint8_t), 1, f->file) != 1)
      fatal("fwrite failed");
}

bool write_b(bool b, fbuf_t *f)
{
   uint8_t c = b;
   if (fwrite(&c, 1, 1, f->file) != 1)
      fatal("fwrite failed");
   return b;
}

void write_raw(const void *buf, size_t len, fbuf_t *f)
{
   if (fwrite(buf, len, 1, f->file) != 1)
      fatal("fwrite failed");
}

uint32_t read_u32(fbuf_t *f)
{
   uint32_t u;
   if (fread(&u, sizeof(uint32_t), 1, f->file) != 1)
      fatal("premature end of file");
   return u;
}

uint16_t read_u16(fbuf_t *f)
{
   uint16_t u;
   if (fread(&u, sizeof(uint16_t), 1, f->file) != 1)
      fatal("premature end of file");
   return u;
}

uint8_t read_u8(fbuf_t *f)
{
   uint8_t u;
   if (fread(&u, sizeof(uint8_t), 1, f->file) != 1)
      fatal("premature end of file");
   return u;
}

bool read_b(fbuf_t *f)
{
   uint8_t u;
   if (fread(&u, sizeof(uint8_t), 1, f->file) != 1)
      fatal("premature end of file");
   return u;
}

int32_t read_i32(fbuf_t *f)
{
   int32_t i;
   if (fread(&i, sizeof(int32_t), 1, f->file) != 1)
      fatal("premature end of file");
   return i;
}

int64_t read_i64(fbuf_t *f)
{
   int64_t i;
   if (fread(&i, sizeof(int64_t), 1, f->file) != 1)
      fatal("premature end of file");
   return i;
}

void read_raw(void *buf, size_t len, fbuf_t *f)
{
   if (fread(buf, len, 1, f->file) != 1)
      fatal("premature end of file");
}
