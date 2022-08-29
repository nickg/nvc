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
#include "fbuf.h"
#include "fastlz.h"

#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>

#define SPILL_SIZE 65536
#define BLOCK_SIZE (SPILL_SIZE - (SPILL_SIZE / 16))

#define UNPACK_BE32(b)                                  \
   ((uint32_t)((b)[0] << 24) | (uint32_t)((b)[1] << 16) \
    | (uint32_t)((b)[2] << 8) | (uint32_t)(b)[3])

#define PACK_BE32(u)                            \
   ((u) >> 24) & 0xff, ((u) >> 16) & 0xff,      \
      ((u) >> 8) & 0xff, (u) & 0xff

typedef struct {
   unsigned long s1;
   unsigned long s2;
} adler32_t;

typedef struct {
   fbuf_cs_t algo;
   uint32_t  expect;
   union {
      adler32_t adler32;
   } u;
} cs_state_t;

struct _fbuf {
   fbuf_mode_t  mode;
   char        *fname;
   FILE        *file;
   uint8_t     *wbuf;
   size_t       wpend;
   size_t       wtotal;
   uint8_t     *rbuf;
   size_t       rptr;
   size_t       ravail;
   size_t       roff;
   uint8_t     *rmap;
   size_t       maplen;
   fbuf_t      *next;
   fbuf_t      *prev;
   cs_state_t   checksum;
};

static fbuf_t *open_list = NULL;

static void adler32_update(adler32_t *state, uint8_t *input, size_t length)
{
   // Public domain implementation from
   //   https://github.com/weidai11/cryptopp/blob/master/adler32.cpp

   const unsigned long BASE = 65521;

   unsigned long s1 = state->s1;
   unsigned long s2 = state->s2;

   if (length % 8 != 0) {
      do {
         s1 += *input++;
         s2 += s1;
         length--;
      } while (length % 8 != 0);

      if (s1 >= BASE)
         s1 -= BASE;
      s2 %= BASE;
   }

   while (length > 0) {
      s1 += input[0]; s2 += s1;
      s1 += input[1]; s2 += s1;
      s1 += input[2]; s2 += s1;
      s1 += input[3]; s2 += s1;
      s1 += input[4]; s2 += s1;
      s1 += input[5]; s2 += s1;
      s1 += input[6]; s2 += s1;
      s1 += input[7]; s2 += s1;

      length -= 8;
      input += 8;

      if (s1 >= BASE)
         s1 -= BASE;
      if (length % 0x8000 == 0)
         s2 %= BASE;
   }

   assert(s1 < BASE);
   assert(s2 < BASE);

   state->s1 = s1;
   state->s2 = s2;
}

static void checksum_init(cs_state_t *state, fbuf_cs_t algo)
{
   state->expect = 0;

   switch ((state->algo = algo)) {
   case FBUF_CS_NONE:
      break;
   case FBUF_CS_ADLER32:
      state->u.adler32.s1 = 1;
      state->u.adler32.s2 = 0;
      break;
   }
}

static void checksum_update(cs_state_t *state, uint8_t *input, size_t length)
{
   switch (state->algo) {
   case FBUF_CS_NONE:
      break;
   case FBUF_CS_ADLER32:
      adler32_update(&(state->u.adler32), input, length);
      break;
   }
}

static uint32_t checksum_finish(cs_state_t *state)
{
   switch (state->algo) {
   case FBUF_CS_ADLER32:
      return (state->u.adler32.s1 << 16) | state->u.adler32.s2;
   default:
      return 0;
   }
}

void fbuf_cleanup(void)
{
   for (fbuf_t *it = open_list; it != NULL; it = it->next) {
      if (it->mode == FBUF_OUT) {
         fclose(it->file);
         remove(it->fname);
      }
   }
}

static void fbuf_write_raw(fbuf_t *f, const uint8_t *bytes, size_t count)
{
   if (fwrite(bytes, count, 1, f->file) != 1)
      fatal_errno("%s: fwrite", f->fname);
}

static void fbuf_write_header(fbuf_t *f)
{
   const uint8_t header[16] = {
      'F', 'B', 'U', 'F',     // Magic number "FBUF"
      'F',                    // Compression format (FastLZ)
      f->checksum.algo,       // Checksum algorithm
      0, 0,                   // Unused
      0, 0, 0, 0,             // Decompressed length
      0, 0, 0, 0,             // Checksum
   };
   fbuf_write_raw(f, header, sizeof(header));
}

static void fbuf_update_header(fbuf_t *f, uint32_t checksum)
{
   if (fseek(f->file, 8, SEEK_SET) != 0)
      fatal_errno("%s: fseek", f->fname);

   const uint8_t bytes[8] = { PACK_BE32(f->wtotal), PACK_BE32(checksum) };
   fbuf_write_raw(f, bytes, 8);
}

static void fbuf_read_header(fbuf_t *f)
{
   uint8_t *header = f->rmap;

   if (memcmp(header, "FBUF", 4)) {
      // TEMPORARY: assume it's from an old version
      //warnf("%s: file created with an older version of NVC", f->fname);
      return;
   }

   if (header[4] != 'F')
      fatal("%s has was created with unexpected compression algorithm %c",
            f->fname, header[4]);

   if (header[5] != f->checksum.algo)
      fatal("%s has was created with unexpected checksum algorithm %c",
            f->fname, header[5]);

   const uint32_t len = UNPACK_BE32(header + 8);
   const uint32_t checksum = UNPACK_BE32(header + 12);

   (void)len;   // Not used

   f->checksum.expect = checksum;
   f->roff = 16;
}

fbuf_t *fbuf_open(const char *file, fbuf_mode_t mode, fbuf_cs_t csum)
{
   fbuf_t *f = NULL;

   switch (mode) {
   case FBUF_OUT:
      {
         FILE *h = fopen(file, "wb");
         if (h == NULL)
            return NULL;

         f = xmalloc(sizeof(struct _fbuf));

         f->file   = h;
         f->rmap   = NULL;
         f->rbuf   = NULL;
         f->wbuf   = xmalloc(SPILL_SIZE);
         f->wpend  = 0;
	 f->wtotal = 0;
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

         void *rmap = map_file(fd, buf.st_size);

         close(fd);

         f = xmalloc(sizeof(struct _fbuf));

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

   f->fname = xstrdup(file);
   f->mode  = mode;
   f->next  = open_list;
   f->prev  = NULL;

   checksum_init(&(f->checksum), csum);

   if (mode == FBUF_OUT)
      fbuf_write_header(f);
   else
      fbuf_read_header(f);

   if (open_list != NULL)
      open_list->prev = f;

   return (open_list = f);
}

const char *fbuf_file_name(fbuf_t *f)
{
   return f->fname;
}

static void fbuf_maybe_flush(fbuf_t *f, size_t more)
{
   assert(more <= BLOCK_SIZE);
   if (f->wpend + more > BLOCK_SIZE) {
      if (f->wpend < 16) {
         // Write dummy bytes at end to meet fastlz block size requirement
         memset(f->wbuf + f->wpend, '\0', 16 - f->wpend);
         f->wpend = 16;
      }

      checksum_update(&(f->checksum), f->wbuf, f->wpend);

      uint8_t out[SPILL_SIZE];
      const int ret = fastlz_compress_level(2, f->wbuf, f->wpend, out);

      assert((ret > 0) && (ret < SPILL_SIZE));

      const uint8_t blksz[4] = { PACK_BE32(ret) };
      fbuf_write_raw(f, blksz, 4);

      fbuf_write_raw(f, out, ret);

      f->wtotal += f->wpend;
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

      const uint32_t blksz = UNPACK_BE32(blksz_raw);
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

      checksum_update(&(f->checksum), f->rbuf + overlap, ret);

      f->roff  += blksz;
      f->ravail = overlap + ret;
      f->rptr   = 0;
   }
}

void fbuf_close(fbuf_t *f, uint32_t *checksum)
{
   if (f->wbuf != NULL)
      fbuf_maybe_flush(f, BLOCK_SIZE);

   const uint32_t cs = checksum_finish(&(f->checksum));

   if (f->mode == FBUF_IN && cs != f->checksum.expect && f->checksum.expect != 0 /* TODO: REMOVE */)
      fatal("%s: incorrect checksum %08x, expected %08x",
            f->fname, cs, f->checksum.expect);

   if (checksum != NULL)
      *checksum = cs;

   if (f->rmap != NULL) {
      unmap_file((void *)f->rmap, f->maplen);
      free(f->rbuf);
   }

   if (f->wbuf != NULL) {
      fbuf_update_header(f, cs);
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

   if (checksum != NULL)
      *checksum = checksum_finish(&(f->checksum));

   free(f->fname);
   free(f);
}

void fbuf_put_uint(fbuf_t *f, uint64_t val)
{
   uint8_t enc[10];
   int nbytes = 0;

   do {
      enc[nbytes] = val & 0x7f;
      val >>= 7;
      if (val) enc[nbytes] |= 0x80;
      nbytes++;
   } while (val);

   fbuf_maybe_flush(f, nbytes);
   for (int i = 0; i < nbytes; i++)
      *(f->wbuf + f->wpend++) = enc[i];
}

void fbuf_put_int(fbuf_t *f, int64_t val)
{
   uint64_t zz = (val << 1) ^ (val >> 63);   // Zig-zag encoding
   fbuf_put_uint(f, zz);
}

void write_u32(uint32_t u, fbuf_t *f)
{
   fbuf_maybe_flush(f, 4);
   *(f->wbuf + f->wpend++) = (u >>  0) & UINT32_C(0xff);
   *(f->wbuf + f->wpend++) = (u >>  8) & UINT32_C(0xff);
   *(f->wbuf + f->wpend++) = (u >> 16) & UINT32_C(0xff);
   *(f->wbuf + f->wpend++) = (u >> 24) & UINT32_C(0xff);
}

void write_u64(uint64_t u, fbuf_t *f)
{
   fbuf_maybe_flush(f, 8);
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
   fbuf_maybe_flush(f, 2);
   *(f->wbuf + f->wpend++) = (s >> 0) & UINT16_C(0xff);
   *(f->wbuf + f->wpend++) = (s >> 8) & UINT16_C(0xff);
}

void write_u8(uint8_t u, fbuf_t *f)
{
   fbuf_maybe_flush(f, 1);
   *(f->wbuf + f->wpend++) = u;
}

void write_raw(const void *buf, size_t len, fbuf_t *f)
{
   fbuf_maybe_flush(f, len);
   memcpy(f->wbuf + f->wpend, buf, len);
   f->wpend += len;
}

void write_double(double d, fbuf_t *f)
{
   union { double d; uint64_t i; } u;
   u.d = d;
   write_u64(u.i, f);
}

uint64_t fbuf_get_uint(fbuf_t *f)
{
   uint8_t dec[10];
   int nbytes = 0;

   uint8_t byte;
   do {
      byte = read_u8(f);
      dec[nbytes++] = byte & 0x7f;
   } while (byte & 0x80);

   uint64_t val = 0;
   for (int i = nbytes - 1; i >= 0; i--) {
      val <<= 7;
      val |= dec[i];
   }

   return val;
}

int64_t fbuf_get_int(fbuf_t *f)
{
   uint64_t zz = fbuf_get_uint(f);
   return (zz >> 1) ^ -(zz & 1);
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
