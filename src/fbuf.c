//
//  Copyright (C) 2014  Nick Gasson
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
#include "fastlz.h"

#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <sys/types.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>

#define SPILL_SIZE 65536
#define BLOCK_SIZE (SPILL_SIZE - (SPILL_SIZE / 16))

struct fbuf {
   fbuf_mode_t  mode;
   char        *fname;
   FILE        *file;
   uint8_t     *wbuf;
   size_t       wpend;
   uint8_t     *rbuf;
   size_t       rptr;
   size_t       ravail;
   size_t       roff;
   uint8_t     *rmap;
   size_t       maplen;
   fbuf_t      *next;
   fbuf_t      *prev;
};

static fbuf_t *open_list = NULL;

void fbuf_cleanup(void)
{
   for (fbuf_t *it = open_list; it != NULL; it = it->next) {
      if (it->mode == FBUF_OUT) {
         fclose(it->file);
         remove(it->fname);
      }
   }
}

fbuf_t *fbuf_open(const char *file, fbuf_mode_t mode)
{
   fbuf_t *f = NULL;

   switch (mode) {
   case FBUF_OUT:
      {
         FILE *h = fopen(file, "w");
         if (h == NULL)
            return NULL;

         f = xmalloc(sizeof(struct fbuf));

         f->file  = h;
         f->rmap  = NULL;
         f->rbuf  = NULL;
         f->wbuf  = xmalloc(SPILL_SIZE);
         f->wpend = 0;
      }
      break;

   case FBUF_IN:
      {
         int fd = open(file, O_RDONLY);
         if (fd < 0)
            return NULL;

         struct stat buf;
         if (fstat(fd, &buf) != 0)
            fatal_errno("fstat");

         void *rmap = mmap(NULL, buf.st_size, PROT_READ, MAP_PRIVATE, fd, 0);
         if (rmap == MAP_FAILED)
            fatal_errno("mmap");

         close(fd);

         f = xmalloc(sizeof(struct fbuf));

         f->file   = NULL;
         f->rmap   = rmap;
         f->rbuf   = xmalloc(SPILL_SIZE);
         f->rptr   = 0;
         f->roff   = 0;
         f->ravail = 0;
         f->maplen = buf.st_size;
         f->wbuf   = NULL;
      }
      break;
   }

   f->fname = strdup(file);
   f->mode  = mode;
   f->next  = open_list;
   f->prev  = NULL;

   if (open_list != NULL)
      open_list->prev = f;

   return (open_list = f);
}

const char *fbuf_file_name(fbuf_t *f)
{
   return f->fname;
}

static void fbuf_maybe_flush(fbuf_t *f, size_t more, bool finish)
{
   assert(more <= BLOCK_SIZE);
   if (f->wpend + more > BLOCK_SIZE) {
      if (f->wpend < 16) {
         // Write dummy bytes at end to meet fastlz block size requirement
         assert(finish);
         f->wpend = 16;
      }

      uint8_t out[SPILL_SIZE];
      const int ret = fastlz_compress_level(2, f->wbuf, f->wpend, out);

      assert((ret > 0) && (ret < SPILL_SIZE));

      const uint8_t blksz[4] = {
         (ret >> 24) & 0xff,
         (ret >> 16) & 0xff,
         (ret >> 8) & 0xff,
         ret & 0xff
      };

      if (fwrite(blksz, 4, 1, f->file) != 1)
         fatal("fwrite failed");

      if (fwrite(out, ret, 1, f->file) != 1)
         fatal("fwrite failed");

      f->wpend = 0;
   }
}

static void fbuf_maybe_read(fbuf_t *f, size_t more)
{
   assert(more <= BLOCK_SIZE);
   if (f->rptr + more > f->ravail) {
      const size_t overlap = f->ravail - f->rptr;
      memcpy(f->rbuf, f->rbuf + f->rptr, overlap);

      const uint8_t *blksz_raw = f->rmap + f->roff;

      const uint32_t blksz =
         (uint32_t)(blksz_raw[0] << 24)
         | (uint32_t)(blksz_raw[1] << 16)
         | (uint32_t)(blksz_raw[2] << 8)
         | (uint32_t)blksz_raw[3];

      if (blksz > SPILL_SIZE)
         fatal("file %s has invalid compression format", f->fname);

      f->roff += sizeof(uint32_t);

      if (f->roff + blksz > f->maplen)
         fatal_trace("read past end of compressed file %s", f->fname);

      const int ret = fastlz_decompress(f->rmap + f->roff,
                                        blksz,
                                        f->rbuf + overlap,
                                        SPILL_SIZE - overlap);

      if (ret == 0)
         fatal("file %s has invalid compression format", f->fname);

      f->roff  += blksz;
      f->ravail = overlap + ret;
      f->rptr   = 0;
   }
}

void fbuf_close(fbuf_t *f)
{
   if (f->rmap != NULL) {
      munmap((void *)f->rmap, f->maplen);
      free(f->rbuf);
   }

   if (f->wbuf != NULL) {
      fbuf_maybe_flush(f, BLOCK_SIZE, true);
      free(f->wbuf);
   }

   if (f->file != NULL)
      fclose(f->file);

   if (f->prev == NULL) {
      assert(f == open_list);
      if (f->next != NULL)
         f->next->prev = NULL;
      open_list = f->next;
   }
   else {
      f->prev->next = f->next;
      if (f->next != NULL)
         f->next->prev = f->prev;
   }

   free(f->fname);
   free(f);
}

void write_u32(uint32_t u, fbuf_t *f)
{
   fbuf_maybe_flush(f, 4, false);
   *(f->wbuf + f->wpend++) = (u >>  0) & UINT32_C(0xff);
   *(f->wbuf + f->wpend++) = (u >>  8) & UINT32_C(0xff);
   *(f->wbuf + f->wpend++) = (u >> 16) & UINT32_C(0xff);
   *(f->wbuf + f->wpend++) = (u >> 24) & UINT32_C(0xff);
}

void write_u64(uint64_t u, fbuf_t *f)
{
   fbuf_maybe_flush(f, 8, false);
   *(f->wbuf + f->wpend++) = (u >>  0) & UINT64_C(0xff);
   *(f->wbuf + f->wpend++) = (u >>  8) & UINT64_C(0xff);
   *(f->wbuf + f->wpend++) = (u >> 16) & UINT64_C(0xff);
   *(f->wbuf + f->wpend++) = (u >> 24) & UINT64_C(0xff);
   *(f->wbuf + f->wpend++) = (u >> 32) & UINT64_C(0xff);
   *(f->wbuf + f->wpend++) = (u >> 40) & UINT64_C(0xff);
   *(f->wbuf + f->wpend++) = (u >> 48) & UINT64_C(0xff);
   *(f->wbuf + f->wpend++) = (u >> 56) & UINT64_C(0xff);
}

void write_u16(uint16_t s, fbuf_t *f)
{
   fbuf_maybe_flush(f, 2, false);
   *(f->wbuf + f->wpend++) = (s >> 0) & UINT16_C(0xff);
   *(f->wbuf + f->wpend++) = (s >> 8) & UINT16_C(0xff);
}

void write_u8(uint8_t u, fbuf_t *f)
{
   fbuf_maybe_flush(f, 1, false);
   *(f->wbuf + f->wpend++) = u;
}

void write_raw(const void *buf, size_t len, fbuf_t *f)
{
   fbuf_maybe_flush(f, len, false);
   memcpy(f->wbuf + f->wpend, buf, len);
   f->wpend += len;
}

void write_double(double d, fbuf_t *f)
{
   union { double d; uint64_t i; } u;
   u.d = d;
   write_u64(u.i, f);
}

uint32_t read_u32(fbuf_t *f)
{
   fbuf_maybe_read(f, 4);

   uint32_t val = 0;
   val |= (uint32_t)*(f->rbuf + f->rptr++) << 0;
   val |= (uint32_t)*(f->rbuf + f->rptr++) << 8;
   val |= (uint32_t)*(f->rbuf + f->rptr++) << 16;
   val |= (uint32_t)*(f->rbuf + f->rptr++) << 24;
   return val;
}

uint16_t read_u16(fbuf_t *f)
{
   fbuf_maybe_read(f, 2);

   uint16_t val = 0;
   val |= (uint16_t)*(f->rbuf + f->rptr++) << 0;
   val |= (uint16_t)*(f->rbuf + f->rptr++) << 8;
   return val;
}

uint8_t read_u8(fbuf_t *f)
{
   fbuf_maybe_read(f, 1);
   return *(f->rbuf + f->rptr++);
}

uint64_t read_u64(fbuf_t *f)
{
   fbuf_maybe_read(f, 8);

   uint64_t val = 0;
   val |= (uint64_t)*(f->rbuf + f->rptr++) << 0;
   val |= (uint64_t)*(f->rbuf + f->rptr++) << 8;
   val |= (uint64_t)*(f->rbuf + f->rptr++) << 16;
   val |= (uint64_t)*(f->rbuf + f->rptr++) << 24;
   val |= (uint64_t)*(f->rbuf + f->rptr++) << 32;
   val |= (uint64_t)*(f->rbuf + f->rptr++) << 40;
   val |= (uint64_t)*(f->rbuf + f->rptr++) << 48;
   val |= (uint64_t)*(f->rbuf + f->rptr++) << 56;
   return val;
}

void read_raw(void *buf, size_t len, fbuf_t *f)
{
   fbuf_maybe_read(f, len);
   memcpy(buf, f->rbuf + f->rptr, len);
   f->rptr += len;
}

double read_double(fbuf_t *f)
{
   union { uint64_t i; double d; } u;
   u.i = read_u64(f);
   return u.d;
}
