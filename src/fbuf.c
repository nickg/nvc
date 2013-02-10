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
#include <zlib.h>

#define WBUF_SIZE     8192
#define RBUF_SIZE     8192
#define DEFLATE_LEVEL 4

struct fbuf {
   fbuf_mode_t  mode;
   char        *fname;
   FILE        *file;
   uint8_t     *wbuf;
   size_t       wpend;
   uint8_t     *rbuf;
   size_t       rptr;
   uint8_t     *rmap;
   size_t       maplen;
   z_stream     strm;
   fbuf_t      *next;
   fbuf_t      *prev;
};

static fbuf_t *open_list = NULL;

void fbuf_cleanup(void)
{
   while (open_list != NULL) {
      fbuf_t *next = open_list->next;
      if (open_list->mode == FBUF_OUT)
         remove(open_list->fname);
      open_list = next;
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
         f->wbuf  = xmalloc(WBUF_SIZE);
         f->wpend = 0;

         f->strm.zalloc = Z_NULL;
         f->strm.zfree  = Z_NULL;
         f->strm.opaque = Z_NULL;
         if (deflateInit(&(f->strm), DEFLATE_LEVEL) != Z_OK)
            fatal("deflateInit failed");
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
         f->rbuf   = xmalloc(RBUF_SIZE);
         f->rptr   = RBUF_SIZE;
         f->maplen = buf.st_size;
         f->wbuf   = NULL;

         f->strm.zalloc   = Z_NULL;
         f->strm.zfree    = Z_NULL;
         f->strm.opaque   = Z_NULL;
         f->strm.avail_in = buf.st_size;
         f->strm.next_in  = rmap;
         if (inflateInit(&(f->strm)) != Z_OK)
            fatal("inflateInit failed");
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

static void fbuf_maybe_flush(fbuf_t *f, size_t more, bool finish)
{
   assert(more <= WBUF_SIZE);
   if (f->wpend + more > WBUF_SIZE) {
      const int zflush = finish ? Z_FINISH : Z_NO_FLUSH;
      f->strm.avail_in = f->wpend;
      f->strm.next_in  = f->wbuf;

      do {
         uint8_t out[WBUF_SIZE];
         f->strm.avail_out = sizeof(out);
         f->strm.next_out  = out;

         const int ret = deflate(&(f->strm), zflush);
         assert(ret != Z_STREAM_ERROR);

         const int have = sizeof(out) - f->strm.avail_out;
         if ((have > 0) && (fwrite(out, have, 1, f->file) != 1))
            fatal("fwrite failed");

      } while (f->strm.avail_out == 0);

      f->wpend = 0;
   }
}

static void fbuf_maybe_read(fbuf_t *f, size_t more)
{
   assert(more <= RBUF_SIZE);
   if (f->rptr + more > RBUF_SIZE) {
      const size_t overlap = RBUF_SIZE - f->rptr;
      memcpy(f->rbuf, f->rbuf + f->rptr, overlap);

      f->strm.avail_out = RBUF_SIZE - overlap;
      f->strm.next_out  = f->rbuf + overlap;

      do {
         const int ret = inflate(&(f->strm), Z_NO_FLUSH);
         if (ret == Z_STREAM_END)
            break;
         else if (ret == Z_DATA_ERROR)
            fatal("file is not compressed");
         else if (ret != Z_OK)
            fatal("inflate failed %d", ret);
      } while (f->strm.avail_out != 0);

      f->rptr = 0;
   }
}

void fbuf_close(fbuf_t *f)
{
   if (f->rmap != NULL) {
      munmap((void *)f->rmap, f->maplen);
      inflateEnd(&(f->strm));
      free(f->rbuf);
   }

   if (f->wbuf != NULL) {
      fbuf_maybe_flush(f, WBUF_SIZE, true);
      deflateEnd(&(f->strm));
      free(f->wbuf);
   }

   if (f->file != NULL)
      fclose(f->file);

   if (f->prev == NULL) {
      assert(f == open_list);
      open_list = f->next;
   }
   else
      f->prev->next = f->next;

   free(f->fname);
   free(f);
}

void write_u32(uint32_t u, fbuf_t *f)
{
   fbuf_maybe_flush(f, 4, false);
   *(f->wbuf + f->wpend++) = (u >> 0) & 0xff;
   *(f->wbuf + f->wpend++) = (u >> 8) & 0xff;
   *(f->wbuf + f->wpend++) = (u >> 16) & 0xff;
   *(f->wbuf + f->wpend++) = (u >> 24) & 0xff;
}

void write_u64(uint64_t u, fbuf_t *f)
{
   fbuf_maybe_flush(f, 8, false);
   *(f->wbuf + f->wpend++) = (u >> 0) & 0xff;
   *(f->wbuf + f->wpend++) = (u >> 8) & 0xff;
   *(f->wbuf + f->wpend++) = (u >> 16) & 0xff;
   *(f->wbuf + f->wpend++) = (u >> 24) & 0xff;
   *(f->wbuf + f->wpend++) = (u >> 32) & 0xff;
   *(f->wbuf + f->wpend++) = (u >> 40) & 0xff;
   *(f->wbuf + f->wpend++) = (u >> 48) & 0xff;
   *(f->wbuf + f->wpend++) = (u >> 56) & 0xff;
}

void write_u16(uint16_t s, fbuf_t *f)
{
   fbuf_maybe_flush(f, 2, false);
   *(f->wbuf + f->wpend++) = (s >> 0) & 0xff;
   *(f->wbuf + f->wpend++) = (s >> 8) & 0xff;
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
