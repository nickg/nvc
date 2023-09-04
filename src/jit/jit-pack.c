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
#include "vcode.h"

#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include <zstd.h>

typedef struct {
   ident_t      name;
   const char  *strtab;
   uint8_t     *buf;
   uint8_t     *rptr;
   uint8_t     *cpool;
   loc_t        last_loc;
} pack_func_t;

struct _jit_pack {
   chash_t   *funcs;
   ZSTD_DCtx *zstd;
};

typedef struct _pack_writer {
   text_buf_t *strtab;
   size_t      bufsz;
   uint8_t    *wptr;
   uint8_t    *buf;
   ZSTD_CCtx  *zstd;
   loc_t       last_loc;
} pack_writer_t;

////////////////////////////////////////////////////////////////////////////////
// JIT bytecode serialisation

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
   do {
      uint8_t byte = value & 0x7f;
      value >>= 7;
      if (value) byte |= 0x80;
      *pw->wptr++ = byte;
   } while (value);
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

static void pack_locus(pack_writer_t *pw, ident_t unit, ptrdiff_t offset)
{
   object_fixup_locus(unit, &offset);

   pack_str(pw, istr(unit));
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
      pack_uint(pw, value.disp);
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
      pack_locus(pw, value.ident, value.disp);
      break;
   case JIT_VALUE_FOREIGN:
      pack_str(pw, istr(ffi_get_sym(value.foreign)));
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
   pack_uint(pw, f->spec.bits);   // XXX: need a function to pack jit_foreign_t
   pack_uint(pw, f->framesz);

   for (int i = 0; i < f->nvars; i++) {
      pack_str(pw, istr(f->linktab[i].name));
      pack_uint(pw, f->linktab[i].offset);
   }

   ident_t unit;
   ptrdiff_t offset;
   object_locus(f->object, &unit, &offset);

   pack_str(pw, istr(unit));
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
   pw->strtab = tb_new();
   pw->bufsz  = 512;
   pw->buf    = xmalloc(pw->bufsz);
   pw->wptr   = pw->buf;

   if ((pw->zstd = ZSTD_createCCtx()) == NULL)
      fatal_trace("ZSTD_createCCtx failed");

   tb_append(pw->strtab, '\0');    // Null string is index zero

   return pw;
}

unsigned pack_writer_get_string(pack_writer_t *pw, const char *str)
{
   if (str == NULL)
      return 0;
   else {
      const char *strtab = tb_get(pw->strtab);
      const size_t tlen = tb_len(pw->strtab);
      const size_t slen = strlen(str) + 1;
      const char *exist = memmem(strtab, tlen, str, slen);
      if (exist != NULL)
         return exist - strtab;
      else {
         tb_catn(pw->strtab, str, slen);
         return tlen;
      }
   }
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
   pf->buf    = (uint8_t *)buf;
   pf->cpool  = (uint8_t *)cpool;
   pf->strtab = strtab;

   chash_put(jp->funcs, name, pf);
}

static uint64_t unpack_uint(pack_func_t *pf)
{
   const uint8_t b0 = *pf->rptr++;
   if (!(b0 & 0x80))
      return b0;

   uint64_t val = b0 & 0x7f;
   int shift = 7;

   uint8_t byte;
   do {
      byte = *pf->rptr++;
      val |= (uint64_t)(byte & 0x7f) << shift;
      shift += 7;
   } while (byte & 0x80);

   return val;
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
   return enc == 0 ? JIT_REG_INVALID : enc + 1;
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
      value.disp = unpack_uint(pf);
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
      value.ident = ident_new(unpack_str(pf));
      value.disp = unpack_uint(pf);
      break;
   case JIT_VALUE_FOREIGN:
      value.foreign = jit_ffi_get(ident_new(unpack_str(pf)));
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

   size_t dsize = ZSTD_decompressDCtx(jp->zstd, ubuf, ubufsz,
                                      pf->buf + start, framesz);
   if (ZSTD_isError(dsize))
      fatal("ZSTD decompress failed: %s: %s", istr(f->name),
            ZSTD_getErrorName(dsize));

   assert(dsize == ubufsz);

   pf->rptr = ubuf;
   pf->last_loc = LOC_INVALID;

   f->nirs      = unpack_uint(pf);
   f->nregs     = unpack_uint(pf);
   f->nvars     = unpack_uint(pf);
   f->cpoolsz   = unpack_uint(pf);
   f->spec.bits = unpack_uint(pf);
   f->framesz   = unpack_uint(pf);

   f->cpool = pf->cpool;
   f->irbuf = xmalloc_array(f->nirs, sizeof(jit_ir_t));

   if (f->nvars > 0) {
      f->linktab = xmalloc_array(f->nvars, sizeof(link_tab_t));

      for (int i = 0; i < f->nvars; i++) {
         f->linktab[i].name = ident_new(unpack_str(pf));
         f->linktab[i].offset = unpack_uint(pf);
      }
   }

   ident_t unit = ident_new(unpack_str(pf));
   ptrdiff_t offset = unpack_uint(pf);

   f->object = object_from_locus(unit, offset, lib_load_handler);

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

   f->cpool = pf->cpool;

   pf->rptr = NULL;

   store_release(&(f->state), JIT_FUNC_READY);
   return true;
}
