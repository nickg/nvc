//
//  Copyright (C) 2022-2023  Nick Gasson
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
#include "array.h"
#include "fbuf.h"
#include "hash.h"
#include "ident.h"
#include "jit/jit-priv.h"
#include "lib.h"
#include "object.h"

#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include <zstd.h>

typedef struct {
   ident_t        name;
   const char    *strtab;
   const uint8_t *buf;
   const uint8_t *rptr;
   const uint8_t *cpool;
   loc_t          last_loc;
} pack_func_t;

struct _jit_pack {
   chash_t    *funcs;
   ZSTD_DCtx  *zstd;
   const void *mmap;
   size_t      map_size;
   nvc_lock_t  zlock;
};

typedef struct _pack_writer {
   text_buf_t *strtab;
   shash_t    *strhash;
   size_t      bufsz;
   uint8_t    *wptr;
   uint8_t    *buf;
   ZSTD_CCtx  *zstd;
   loc_t       last_loc;
} pack_writer_t;

////////////////////////////////////////////////////////////////////////////////
// JIT bytecode serialisation

static inline int encode_number(uint64_t value, uint8_t buf[10])
{
   int num = 0;
   do {
      uint8_t byte = value & 0x7f;
      value >>= 7;
      if (value) byte |= 0x80;
      buf[num++] = byte;
   } while (value);

   assert(num <= 10);
   return num;
}

static inline uint64_t decode_number(const uint8_t **buf)
{
   const uint8_t b0 = *((*buf)++);
   if (!(b0 & 0x80))
      return b0;

   uint64_t val = b0 & 0x7f;
   int shift = 7;

   uint8_t byte;
   do {
      byte = *((*buf)++);
      val |= (uint64_t)(byte & 0x7f) << shift;
      shift += 7;
   } while (byte & 0x80);

   return val;
}

static inline void pack_grow(pack_writer_t *pw, size_t minsz)
{
   const size_t used = pw->wptr - pw->buf;
   if (unlikely(used + minsz > pw->bufsz)) {
      pw->bufsz = MAX(pw->bufsz + minsz, (3 * pw->bufsz) / 2);
      pw->buf = xrealloc(pw->buf, pw->bufsz);
      pw->wptr = pw->buf + used;
   }
}

static inline void pack_u8(pack_writer_t *pw, uint8_t value)
{
   pack_grow(pw, 1);
   *pw->wptr++ = value;
}

static inline void pack_u16(pack_writer_t *pw, uint16_t value)
{
   pack_grow(pw, 2);
   *pw->wptr++ = value & 0xff;
   *pw->wptr++ = value >> 8;
}

static void pack_uint(pack_writer_t *pw, uint64_t value)
{
   pack_grow(pw, 10);
   pw->wptr += encode_number(value, pw->wptr);
}

static inline void pack_int(pack_writer_t *pw, int64_t value)
{
   uint64_t zz = ((uint64_t)value << 1) ^ (value >> 63);   // Zig-zag encoding
   pack_uint(pw, zz);
}

static void pack_str(pack_writer_t *pw, const char *str)
{
   pack_uint(pw, pack_writer_get_string(pw, str));
}

static void pack_loc(pack_writer_t *pw, const loc_t *loc)
{
   if (loc_invalid_p(&pw->last_loc) || loc->file_ref != pw->last_loc.file_ref) {
      pack_str(pw, loc_file_str(loc));
      pack_uint(pw, loc->first_line);
      pack_uint(pw, loc->first_column);
   }
   else {
      const int line_delta = loc->first_line - pw->last_loc.first_line;
      const int column_delta = loc->first_column - pw->last_loc.first_column;

      pack_u8(pw, 0);
      pack_int(pw, line_delta);
      pack_int(pw, column_delta);
   }

   pw->last_loc = *loc;
}

static inline void pack_reg(pack_writer_t *pw, jit_reg_t reg)
{
   pack_uint(pw, reg == JIT_REG_INVALID ? 0 : reg + 1);
}

static inline void pack_double(pack_writer_t *pw, double value)
{
   jit_scalar_t u = { .real = value };
   pack_uint(pw, u.integer);
}

static void pack_handle(pack_writer_t *pw, jit_t *j, jit_handle_t handle)
{
   if (handle == JIT_HANDLE_INVALID)
      pack_str(pw, NULL);
   else
      pack_str(pw, istr(jit_get_func(j, handle)->name));
}

static void pack_locus(pack_writer_t *pw, object_t *obj)
{
   ident_t module;
   ptrdiff_t offset;
   object_locus(obj, &module, &offset);

   pack_str(pw, istr(module));
   pack_uint(pw, offset);
}

static void pack_value(pack_writer_t *pw, jit_t *j, jit_value_t value)
{
   if (value.kind == JIT_VALUE_VPOS) {
      // Not needed anymore
      pack_u8(pw, JIT_VALUE_INVALID);
      return;
   }

   pack_u8(pw, value.kind);

   switch (value.kind) {
   case JIT_VALUE_INVALID:
      break;
   case JIT_VALUE_REG:
      pack_reg(pw, value.reg);
      break;
   case JIT_VALUE_INT64:
      pack_int(pw, value.int64);
      break;
   case JIT_VALUE_DOUBLE:
      pack_double(pw, value.dval);
      break;
   case JIT_ADDR_REG:
      pack_reg(pw, value.reg);
      pack_uint(pw, value.disp);
      break;
   case JIT_ADDR_CPOOL:
      pack_uint(pw, value.int64);
      break;
   case JIT_ADDR_ABS:
   case JIT_ADDR_COVER:
      pack_uint(pw, value.int64);
      break;
   case JIT_VALUE_LABEL:
      pack_uint(pw, value.label);
      break;
   case JIT_VALUE_HANDLE:
      pack_handle(pw, j, value.handle);
      break;
   case JIT_VALUE_EXIT:
      pack_u8(pw, value.exit);
      break;
   case JIT_VALUE_LOC:
      pack_loc(pw, &(value.loc));
      break;
   case JIT_VALUE_LOCUS:
      pack_locus(pw, value.locus);
      break;
   default:
      fatal_trace("cannot handle value kind %d in pack_value", value.kind);
   }
}

static void pack_func(pack_writer_t *pw, jit_t *j, jit_func_t *f)
{
   pw->last_loc = LOC_INVALID;

   pack_uint(pw, f->nirs);
   pack_uint(pw, f->nregs);
   pack_uint(pw, f->nvars);
   pack_uint(pw, f->cpoolsz);
   pack_uint(pw, f->spec.bits);
   pack_uint(pw, f->framesz);

   for (int i = 0; i < f->nvars; i++) {
      pack_str(pw, istr(f->linktab[i].name));
      pack_uint(pw, f->linktab[i].offset);
   }

   ident_t module;
   ptrdiff_t offset;
   object_locus(f->object, &module, &offset);

   pack_str(pw, istr(module));
   pack_uint(pw, offset);

   for (int i = 0; i < f->nirs; i++) {
      jit_ir_t *ir = &(f->irbuf[i]);

      const uint16_t enc =
         (ir->op << 8) | (ir->size << 5) | (ir->target << 4) | ir->cc;
      pack_u16(pw, enc);

      pack_reg(pw, ir->result);
      pack_value(pw, j, ir->arg1);
      pack_value(pw, j, ir->arg2);
   }
}

pack_writer_t *pack_writer_new(void)
{
   pack_writer_t *pw = xcalloc(sizeof(pack_writer_t));
   pw->strhash = shash_new(16);
   pw->strtab  = tb_new();
   pw->bufsz   = 512;
   pw->buf     = xmalloc(pw->bufsz);
   pw->wptr    = pw->buf;

   if ((pw->zstd = ZSTD_createCCtx()) == NULL)
      fatal_trace("ZSTD_createCCtx failed");

   tb_append(pw->strtab, '\0');    // Null string is index zero

   return pw;
}

unsigned pack_writer_get_string(pack_writer_t *pw, const char *str)
{
   if (str == NULL)
      return 0;

   const uintptr_t exist = (uintptr_t)shash_get(pw->strhash, str);
   if (exist != 0)
      return exist;

   const unsigned off = tb_len(pw->strtab);
   shash_put(pw->strhash, str, (void *)(uintptr_t)off);
   tb_catn(pw->strtab, str, strlen(str) + 1);
   return off;
}

void pack_writer_emit(pack_writer_t *pw, jit_t *j, jit_handle_t handle,
                      uint8_t **buf, size_t *size)
{
   jit_func_t *f = jit_get_func(j, handle);
   jit_fill_irbuf(f);

   assert(pw->wptr == pw->buf);

   pack_func(pw, j, f);

   const size_t ubufsz = pw->wptr - pw->buf;
   const size_t zbufsz = ZSTD_compressBound(ubufsz) + 10;
   uint8_t *zbuf = xmalloc(zbufsz);

   int start = 0;
   size_t encsz = ubufsz;
   do {
      uint8_t byte = encsz & 0x7f;
      encsz >>= 7;
      if (encsz) byte |= 0x80;
      zbuf[start++] = byte;
   } while (encsz);

   const size_t csize = ZSTD_compressCCtx(pw->zstd, zbuf + start,
                                          zbufsz - start, pw->buf, ubufsz, 3);
   if (ZSTD_isError(csize))
      fatal("ZSTD compress failed: %s: %s", istr(f->name),
            ZSTD_getErrorName(csize));

   pw->wptr = pw->buf;

   *buf = zbuf;
   *size = csize + start;
}

void pack_writer_string_table(pack_writer_t *pw, const char **tab, size_t *size)
{
   *tab = tb_get(pw->strtab);
   *size = tb_len(pw->strtab);
}

void pack_writer_free(pack_writer_t *pw)
{
   ZSTD_freeCCtx(pw->zstd);
   free(pw->buf);
   tb_free(pw->strtab);
   shash_free(pw->strhash);
   free(pw);
}

////////////////////////////////////////////////////////////////////////////////
// JIT bytecode loader

jit_pack_t *jit_pack_new(void)
{
   jit_pack_t *jp = xcalloc(sizeof(struct _jit_pack));
   jp->funcs = chash_new(256);

   if ((jp->zstd = ZSTD_createDCtx()) == NULL)
      fatal_trace("ZSTD_createDCtx failed");

   return jp;
}

static void pack_func_free(const void *key, void *value)
{
   pack_func_t *pf = value;
   free(pf);
}

void jit_pack_free(jit_pack_t *jp)
{
   if (jp->mmap != NULL)
      unmap_file((void *)jp->mmap, jp->map_size);

   ZSTD_freeDCtx(jp->zstd);
   chash_iter(jp->funcs, pack_func_free);
   chash_free(jp->funcs);
   free(jp);
}

void jit_pack_put(jit_pack_t *jp, ident_t name, const uint8_t *cpool,
                  const char *strtab, const uint8_t *buf)
{
   assert(chash_get(jp->funcs, name) == NULL);

   pack_func_t *pf = xcalloc(sizeof(pack_func_t));
   pf->buf    = buf;
   pf->cpool  = cpool;
   pf->strtab = strtab;

   chash_put(jp->funcs, name, pf);
}

static uint64_t unpack_uint(pack_func_t *pf)
{
   return decode_number(&pf->rptr);
}

static int64_t unpack_int(pack_func_t *pf)
{
   uint64_t zz = unpack_uint(pf);
   return (zz >> 1) ^ -(zz & 1);
}

static inline uint16_t unpack_u16(pack_func_t *pf)
{
   uint16_t value = 0;
   value |= *pf->rptr++;
   value |= *pf->rptr++ << 8;
   return value;
}

static inline jit_reg_t unpack_reg(pack_func_t *pf)
{
   const uint64_t enc = unpack_uint(pf);
   assert(enc < JIT_REG_INVALID - 1);
   return enc == 0 ? JIT_REG_INVALID : enc - 1;
}

static inline double unpack_double(pack_func_t *pf)
{
   jit_scalar_t value = { .integer = unpack_uint(pf) };
   return value.real;
}

static const char *unpack_str(pack_func_t *pf)
{
   const unsigned off = unpack_uint(pf);
   return off == 0 ? NULL : pf->strtab + off;
}

static loc_t unpack_loc(pack_func_t *pf)
{
   loc_t loc = pf->last_loc;

   const char *file = unpack_str(pf);
   if (file != NULL) {
      loc.file_ref = loc_file_ref(file, NULL);
      loc.first_line = unpack_uint(pf);
      loc.first_column = unpack_uint(pf);
      loc.column_delta = 0;
      loc.line_delta = 0;
   }
   else {
      loc.first_line += unpack_int(pf);
      loc.first_column += unpack_int(pf);
   }

   return (pf->last_loc = loc);
}

static jit_handle_t unpack_handle(pack_func_t *pf, jit_t *j)
{
   const char *str = unpack_str(pf);
   if (str == NULL)
      return JIT_HANDLE_INVALID;
   else
      return jit_lazy_compile(j, ident_new(str));
}

static object_t *unpack_locus(pack_func_t *pf)
{
   ident_t module = ident_new(unpack_str(pf));
   ptrdiff_t disp = unpack_uint(pf);
   return object_from_locus(module, disp, lib_load_handler);
}

static jit_value_t unpack_value(pack_func_t *pf, jit_t *j)
{
   jit_value_t value;
   value.kind = *pf->rptr++;

   switch (value.kind) {
   case JIT_VALUE_INVALID:
      break;
   case JIT_VALUE_REG:
      value.reg = unpack_reg(pf);
      break;
   case JIT_VALUE_INT64:
      value.int64 = unpack_int(pf);
      break;
   case JIT_VALUE_DOUBLE:
      value.dval = unpack_double(pf);
      break;
   case JIT_ADDR_REG:
      value.reg = unpack_reg(pf);
      value.disp = unpack_uint(pf);
      break;
   case JIT_ADDR_CPOOL:
      value.int64 = unpack_uint(pf);
      break;
   case JIT_ADDR_ABS:
   case JIT_ADDR_COVER:
      value.int64 = unpack_uint(pf);
      break;
   case JIT_VALUE_LABEL:
      value.label = unpack_uint(pf);
      break;
   case JIT_VALUE_HANDLE:
      value.handle = unpack_handle(pf, j);
      break;
   case JIT_VALUE_EXIT:
      value.exit = *pf->rptr++;
      break;
   case JIT_VALUE_LOC:
      value.loc = unpack_loc(pf);
      break;
   case JIT_VALUE_LOCUS:
      value.locus = unpack_locus(pf);
      break;
   default:
      fatal_trace("cannot handle value kind %d in unpack_value", value.kind);
   }

   return value;
}

bool jit_pack_fill(jit_pack_t *jp, jit_t *j, jit_func_t *f)
{
   pack_func_t *pf = chash_get(jp->funcs, f->name);
   if (pf == NULL)
      return false;

   assert(load_acquire(&f->state) == JIT_FUNC_COMPILING);

   size_t ubufsz = 0;
   int shift = 0, start = 0;
   uint8_t byte;
   do {
      byte = pf->buf[start++];
      ubufsz |= (uint64_t)(byte & 0x7f) << shift;
      shift += 7;
   } while (byte & 0x80);

   uint8_t *ubuf LOCAL = xmalloc(ubufsz);

   size_t framesz = ZSTD_findFrameCompressedSize(pf->buf + start, SIZE_MAX);
   if (ZSTD_isError(framesz))
      fatal("cannot get ZSTD compressed frame size: %s: %s", istr(f->name),
            ZSTD_getErrorName(framesz));

   {
      SCOPED_LOCK(jp->zlock);

      size_t dsize = ZSTD_decompressDCtx(jp->zstd, ubuf, ubufsz,
                                         pf->buf + start, framesz);
      if (ZSTD_isError(dsize))
         fatal("ZSTD decompress failed: %s: %s", istr(f->name),
               ZSTD_getErrorName(dsize));

      assert(dsize == ubufsz);
   }

   pf->rptr = ubuf;
   pf->last_loc = LOC_INVALID;

   f->nirs      = unpack_uint(pf);
   f->nregs     = unpack_uint(pf);
   f->nvars     = unpack_uint(pf);
   f->cpoolsz   = unpack_uint(pf);
   f->spec.bits = unpack_uint(pf);
   f->framesz   = unpack_uint(pf);

   f->cpool = (unsigned char *)pf->cpool;
   f->irbuf = xmalloc_array(f->nirs, sizeof(jit_ir_t));

   if (f->nvars > 0) {
      f->linktab = xmalloc_array(f->nvars, sizeof(link_tab_t));

      for (int i = 0; i < f->nvars; i++) {
         f->linktab[i].name = ident_new(unpack_str(pf));
         f->linktab[i].offset = unpack_uint(pf);
      }
   }

   ident_t module = ident_new(unpack_str(pf));
   ptrdiff_t offset = unpack_uint(pf);
   f->object = object_from_locus(module, offset, lib_load_handler);

   for (int i = 0; i < f->nirs; i++) {
      jit_ir_t *ir = &(f->irbuf[i]);

      const uint16_t enc = unpack_u16(pf);
      ir->op = enc >> 8;
      ir->size = (enc >> 5) & 7;
      ir->target = (enc >> 4) & 1;
      ir->cc = enc & 0xf;

      ir->result = unpack_reg(pf);
      ir->arg1 = unpack_value(pf, j);
      ir->arg2 = unpack_value(pf, j);
   }

   pf->rptr = NULL;

   store_release(&(f->state), JIT_FUNC_READY);
   return true;
}

////////////////////////////////////////////////////////////////////////////////
// Disk storage

typedef struct {
   char     magic[4];
   uint32_t strtab;
} pack_header_t;

#define PACK_MAGIC "JIT1"

static void write_fully(const void *buf, size_t size, FILE *f)
{
   if (fwrite(buf, size, 1, f) != 1)
      fatal_errno("fwrite");
}

void jit_write_pack(jit_t *j, const ident_t *units, size_t count, FILE *f)
{
   pack_writer_t *pw = pack_writer_new();

   pack_header_t header = {};
   write_fully(&header, sizeof(header), f);

   for (size_t i = 0; i < count; i++) {
      jit_handle_t handle = jit_compile(j, units[i]);

      uint8_t bytes[10];

      const uint32_t name = pack_writer_get_string(pw, istr(units[i]));
      const int name_nbytes = encode_number(name, bytes);
      write_fully(bytes, name_nbytes, f);

      uint8_t *buf;
      size_t size;
      pack_writer_emit(pw, j, handle, &buf, &size);

      const int size_nbytes = encode_number(size, bytes);
      write_fully(bytes, size_nbytes, f);

      jit_func_t *func = jit_get_func(j, handle);

      const int cpool_nbytes = encode_number(func->cpoolsz, bytes);
      write_fully(bytes, cpool_nbytes, f);

      write_fully(buf, size, f);

      if (func->cpoolsz > 0)
         write_fully(func->cpool, func->cpoolsz, f);
   }

   memcpy(header.magic, PACK_MAGIC, sizeof(header.magic));
   header.strtab = ftell(f);

   const char *tab;
   size_t size;
   pack_writer_string_table(pw, &tab, &size);

   write_fully(tab, size, f);

   rewind(f);

   write_fully(&header, sizeof(header), f);

   pack_writer_free(pw);
}

jit_pack_t *jit_read_pack(FILE *f)
{
   jit_pack_t *jp = jit_pack_new();

   file_info_t info;
   if (!get_handle_info(fileno(f), &info))
      fatal("cannot get info for pack file");

   jp->mmap = map_file(fileno(f), info.size);
   jp->map_size = info.size;

   const pack_header_t *header = jp->mmap;
   if (memcmp(header->magic, PACK_MAGIC, sizeof(header->magic)) != 0)
      fatal("bad JIT pack magic");

   const char *strtab = jp->mmap + header->strtab;

   const uint8_t *rptr = jp->mmap + sizeof(pack_header_t);
   while (rptr < (uint8_t *)jp->mmap + header->strtab) {
      const uint32_t str = decode_number(&rptr);
      const uint32_t size = decode_number(&rptr);
      const uint32_t cpool = decode_number(&rptr);

      ident_t ident = ident_new(strtab + str);
      jit_pack_put(jp, ident, rptr + size, strtab, rptr);

      rptr += size + cpool;
   }

   return jp;
}
