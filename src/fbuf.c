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
#include <assert.h>
#include <sys/types.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>

struct fbuf {
   fbuf_mode_t    mode;
   FILE          *file;
   const uint8_t *rbuf;
   size_t         maplen;
};

fbuf_t *fbuf_open(const char *file, fbuf_mode_t mode)
{
   switch (mode) {
   case FBUF_OUT:
      {
         FILE *h = fopen(file, "w");
         if (h == NULL)
            return NULL;

         fbuf_t *f = xmalloc(sizeof(struct fbuf));
         f->file = h;
         f->mode = mode;
         f->rbuf = NULL;

         return f;
      }

   case FBUF_IN:
      {
         int fd = open(file, O_RDONLY);
         if (fd < 0)
            return NULL;

         struct stat buf;
         if (fstat(fd, &buf) != 0)
            fatal_errno("fstat");

         void *rbuf = mmap(NULL, buf.st_size, PROT_READ, MAP_PRIVATE, fd, 0);
         if (rbuf == MAP_FAILED)
            fatal_errno("mmap");

         close(fd);

         fbuf_t *f = xmalloc(sizeof(struct fbuf));
         f->file   = NULL;
         f->mode   = mode;
         f->rbuf   = rbuf;
         f->maplen = buf.st_size;

         return f;
      }

   default:
      assert(false);
   }
}

void fbuf_close(fbuf_t *f)
{
   if (f->file != NULL)
      fclose(f->file);

   if (f->rbuf != NULL)
      munmap((void *)f->rbuf, f->maplen);

   free(f);
}

void write_u32(uint32_t u, fbuf_t *f)
{
   if (fwrite(&u, sizeof(uint32_t), 1, f->file) != 1)
      fatal("fwrite failed");
}

void write_u64(uint64_t i, fbuf_t *f)
{
   if (fwrite(&i, sizeof(uint64_t), 1, f->file) != 1)
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

void write_raw(const void *buf, size_t len, fbuf_t *f)
{
   if (fwrite(buf, len, 1, f->file) != 1)
      fatal("fwrite failed");
}

uint32_t read_u32(fbuf_t *f)
{
   uint32_t val = 0;
   val |= (uint32_t)*(f->rbuf++) << 0;
   val |= (uint32_t)*(f->rbuf++) << 8;
   val |= (uint32_t)*(f->rbuf++) << 16;
   val |= (uint32_t)*(f->rbuf++) << 24;
   return val;
}

uint16_t read_u16(fbuf_t *f)
{
   uint16_t val = 0;
   val |= (uint16_t)*(f->rbuf++) << 0;
   val |= (uint16_t)*(f->rbuf++) << 8;
   return val;
}

uint8_t read_u8(fbuf_t *f)
{
   return *(f->rbuf++);
}

uint64_t read_u64(fbuf_t *f)
{
   uint64_t val = 0;
   val |= (uint64_t)*(f->rbuf++) << 0;
   val |= (uint64_t)*(f->rbuf++) << 8;
   val |= (uint64_t)*(f->rbuf++) << 16;
   val |= (uint64_t)*(f->rbuf++) << 24;
   val |= (uint64_t)*(f->rbuf++) << 32;
   val |= (uint64_t)*(f->rbuf++) << 40;
   val |= (uint64_t)*(f->rbuf++) << 48;
   val |= (uint64_t)*(f->rbuf++) << 56;
   return val;
}

void read_raw(void *buf, size_t len, fbuf_t *f)
{
   memcpy(buf, f->rbuf, len);
   f->rbuf += len;
}
