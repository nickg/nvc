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
#include <zlib.h>

typedef enum {
   PF_OWNED,
   PF_BORROWED
} owner_kind_t;

typedef A(const char *) string_list_t;

typedef struct {
   ident_t        name;
   owner_kind_t   kind;
   shash_t       *strtab;
   string_list_t  strings;
   unsigned       nextstr;
   size_t         cpoolsz;
   uint8_t       *cpool;
   size_t         bufsz;
   uint8_t       *wptr;
   uint8_t       *buf;
} pack_func_t;

struct _jit_pack {
   chash_t *funcs;
};

jit_pack_t *jit_pack_new(void)
{
   jit_pack_t *jp = xcalloc(sizeof(struct _jit_pack));
   jp->funcs = chash_new(256);

   return jp;
}

static void pack_func_free(const void *key, void *value)
{
   pack_func_t *pf = value;

   if (pf->kind == PF_OWNED) {
      free(pf->buf);
      free(pf->cpool);
   }

   free(pf);
}

void jit_pack_free(jit_pack_t *jp)
{
   chash_iter(jp->funcs, pack_func_free);
   chash_free(jp->funcs);
   free(jp);
}

static inline void pack_grow(pack_func_t *pf, size_t minsz)
{
   const size_t used = pf->wptr - pf->buf;
   if (unlikely(used + minsz > pf->bufsz)) {
      pf->bufsz = MAX(pf->bufsz + minsz, (3 * pf->bufsz) / 2);
      pf->buf = xrealloc(pf->buf, pf->bufsz);
      pf->wptr = pf->buf + used;
   }
}

static inline void pack_u8(pack_func_t *pf, uint8_t value)
{
   pack_grow(pf, 1);
   *pf->wptr++ = value;
}

static inline void pack_u16(pack_func_t *pf, uint16_t value)
{
   pack_grow(pf, 2);
   *pf->wptr++ = value & 0xff;
   *pf->wptr++ = value >> 8;
}

static void pack_uint(pack_func_t *pf, uint64_t value)
{
   pack_grow(pf, 10);
   do {
      uint8_t byte = value & 0x7f;
      value >>= 7;
      if (value) byte |= 0x80;
      *pf->wptr++ = byte;
   } while (value);
}

static inline void pack_int(pack_func_t *pf, int64_t value)
{
   uint64_t zz = (value << 1) ^ (value >> 63);   // Zig-zag encoding
   pack_uint(pf, zz);
}

static void pack_str(pack_func_t *pf, const char *str)
{
   if (str == NULL)
      pack_int(pf, 0);
   else {
      const uintptr_t idx = (uintptr_t)shash_get(pf->strtab, str);
      if (idx == 0) {
         const int len = strlen(str);
         pack_int(pf, -len);
         pack_grow(pf, len + 1);
         memcpy(pf->wptr, str, len + 1);
         pf->wptr += len + 1;

         shash_put(pf->strtab, str, (void *)(uintptr_t)pf->nextstr++);
      }
      else
         pack_int(pf, idx);
   }
}

static void pack_loc(pack_func_t *pf, const loc_t *loc)
{
   pack_str(pf, loc_file_str(loc));
   pack_uint(pf, loc->first_line);
   pack_uint(pf, loc->first_column);
   pack_uint(pf, loc->line_delta);
   pack_uint(pf, loc->column_delta);
}

static inline void pack_reg(pack_func_t *pf, jit_reg_t reg)
{
   pack_uint(pf, reg == JIT_REG_INVALID ? 0 : reg + 1);
}

static inline void pack_double(pack_func_t *pf, double value)
{
   jit_scalar_t u = { .real = value };
   pack_uint(pf, u.integer);
}

static void pack_tree(pack_func_t *pf, tree_t tree)
{
   ident_t unit;
   ptrdiff_t offset;
   tree_locus(tree, &unit, &offset);

   pack_str(pf, istr(unit));
   pack_uint(pf, offset);
}

static void pack_handle(pack_func_t *pf, jit_t *j, jit_handle_t handle)
{
   if (handle == JIT_HANDLE_INVALID)
      pack_str(pf, NULL);
   else
      pack_str(pf, istr(jit_get_func(j, handle)->name));
}

static void pack_value(pack_func_t *pf, jit_t *j, jit_value_t value)
{
   pack_u8(pf, value.kind);

   switch (value.kind) {
   case JIT_VALUE_INVALID:
      break;
   case JIT_VALUE_REG:
      pack_reg(pf, value.reg);
      break;
   case JIT_VALUE_INT64:
      pack_int(pf, value.int64);
      break;
   case JIT_VALUE_DOUBLE:
      pack_double(pf, value.dval);
      break;
   case JIT_ADDR_REG:
      pack_reg(pf, value.reg);
      pack_uint(pf, value.disp);
      break;
   case JIT_ADDR_CPOOL:
      pack_uint(pf, value.disp);
      break;
   case JIT_ADDR_ABS:
   case JIT_ADDR_COVER:
      pack_uint(pf, value.int64);
      break;
   case JIT_VALUE_LABEL:
      pack_uint(pf, value.label);
      break;
   case JIT_VALUE_HANDLE:
      pack_handle(pf, j, value.handle);
      break;
   case JIT_VALUE_EXIT:
      pack_u8(pf, value.exit);
      break;
   case JIT_VALUE_LOC:
      pack_loc(pf, &(value.loc));
      break;
   case JIT_VALUE_TREE:
      pack_tree(pf, value.tree);
      break;
   case JIT_VALUE_FOREIGN:
      pack_str(pf, istr(ffi_get_sym(value.foreign)));
      break;
   default:
      fatal_trace("cannot handle value kind %d in pack_value", value.kind);
   }
}

static void pack_func(pack_func_t *pf, jit_t *j, jit_func_t *f)
{
   pf->name    = f->name;
   pf->cpoolsz = f->cpoolsz;

   pf->cpool = xmalloc(f->cpoolsz);
   memcpy(pf->cpool, f->cpool, f->cpoolsz);

   pack_uint(pf, f->nirs);
   pack_uint(pf, f->nregs);
   pack_uint(pf, f->nvars);
   pack_uint(pf, f->cpoolsz);
   pack_uint(pf, f->spec);
   pack_uint(pf, f->framesz);

   for (int i = 0; i < f->nvars; i++) {
      pack_str(pf, istr(f->linktab[i].name));
      pack_uint(pf, f->linktab[i].offset);
   }

   ident_t unit;
   ptrdiff_t offset;
   object_locus(f->object, &unit, &offset);

   pack_str(pf, istr(unit));
   pack_uint(pf, offset);

   for (int i = 0; i < f->nirs; i++) {
      jit_ir_t *ir = &(f->irbuf[i]);

      const uint16_t enc =
         (ir->op << 8) | (ir->size << 5) | (ir->target << 4) | ir->cc;
      pack_u16(pf, enc);

      pack_reg(pf, ir->result);
      pack_value(pf, j, ir->arg1);
      pack_value(pf, j, ir->arg2);
   }
}

void jit_pack_encode(jit_pack_t *jp, jit_t *j, jit_handle_t handle)
{
   jit_func_t *f = jit_get_func(j, handle);
   jit_fill_irbuf(f);

   pack_func_t *pf = xcalloc(sizeof(pack_func_t));
   pf->kind    = PF_OWNED;
   pf->bufsz   = f->nirs * 10;
   pf->buf     = xmalloc(pf->bufsz);
   pf->wptr    = pf->buf;
   pf->strtab  = shash_new(128);
   pf->nextstr = 1;

   pack_func(pf, j, f);

   shash_free(pf->strtab);
   pf->strtab = NULL;

   z_stream strm;
   strm.zalloc = Z_NULL;
   strm.zfree  = Z_NULL;
   strm.opaque = Z_NULL;

   if (deflateInit(&strm, Z_DEFAULT_COMPRESSION) != Z_OK)
      fatal_trace("deflateInit failed");

   const size_t ubufsz = pf->wptr - pf->buf;
   const size_t zbufsz = deflateBound(&strm, ubufsz) + 4;
   uint8_t *zbuf = xmalloc(zbufsz);

   zbuf[0] = ubufsz & 0xff;
   zbuf[1] = (ubufsz >> 8) & 0xff;
   zbuf[2] = (ubufsz >> 16) & 0xff;
   zbuf[3] = (ubufsz >> 24) & 0xff;

   strm.next_in   = pf->buf;
   strm.avail_in  = pf->wptr - pf->buf;
   strm.avail_out = zbufsz - 4;
   strm.next_out  = zbuf + 4;

   if (deflate(&strm, Z_FINISH) != Z_STREAM_END)
      fatal_trace("deflate failed");

   deflateEnd(&strm);

   free(pf->buf);
   pf->buf = zbuf;
   pf->bufsz = zbufsz - strm.avail_out + 4;

   chash_put(jp->funcs, f->name, pf);
}

void jit_pack_vcode(jit_pack_t *jp, jit_t *j, vcode_unit_t vu)
{
   vcode_select_unit(vu);
   jit_handle_t h = jit_lazy_compile(j, vcode_unit_name());
   jit_pack_encode(jp, j, h);

   for (vcode_unit_t it = vcode_unit_child(vu);
        it != NULL;
        it = vcode_unit_next(it)) {
      jit_pack_vcode(jp, j, it);
   }
}

const uint8_t *jit_pack_get(jit_pack_t *jp, ident_t name, size_t *size)
{
   pack_func_t *pf = chash_get(jp->funcs, name);
   if (pf == NULL)
      return NULL;

   *size = pf->bufsz;
   return pf->buf;
}

void jit_pack_put(jit_pack_t *jp, ident_t name, const uint8_t *cpool,
                  const uint8_t *buf)
{
   assert(chash_get(jp->funcs, name) == NULL);

   pack_func_t *pf = xcalloc(sizeof(pack_func_t));
   pf->kind  = PF_BORROWED;
   pf->buf   = (uint8_t *)buf;
   pf->cpool = (uint8_t *)cpool;

   chash_put(jp->funcs, name, pf);
}

static uint64_t unpack_uint(pack_func_t *pf)
{
   uint8_t dec[10];
   int nbytes = 0;

   uint8_t byte;
   do {
      byte = *pf->wptr++;
      dec[nbytes++] = byte & 0x7f;
   } while (byte & 0x80);

   uint64_t val = 0;
   for (int i = nbytes - 1; i >= 0; i--) {
      val <<= 7;
      val |= dec[i];
   }

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
   value |= *pf->wptr++;
   value |= *pf->wptr++ << 8;
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
   const int64_t enc = unpack_int(pf);
   if (enc == 0)
      return NULL;
   else if (enc < 0) {
      const char *str = (char *)pf->wptr;
      pf->wptr += -enc + 1;
      APUSH(pf->strings, str);
      return str;
   }
   else
      return AGET(pf->strings, enc - 1);
}

static loc_t unpack_loc(pack_func_t *pf)
{
   loc_t loc;
   loc.file_ref = loc_file_ref(unpack_str(pf), NULL);
   loc.first_line = unpack_uint(pf);
   loc.first_column = unpack_uint(pf);
   loc.line_delta = unpack_uint(pf);
   loc.column_delta = unpack_uint(pf);
   return loc;
}

static tree_t unpack_tree(pack_func_t *pf)
{
   ident_t unit = ident_new(unpack_str(pf));
   ptrdiff_t offset = unpack_uint(pf);

   return tree_from_locus(unit, offset, lib_get_qualified);
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
   value.kind = *pf->wptr++;

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
      value.exit = *pf->wptr++;
      break;
   case JIT_VALUE_LOC:
      value.loc = unpack_loc(pf);
      break;
   case JIT_VALUE_TREE:
      value.tree = unpack_tree(pf);
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

   const size_t ubufsz = pf->buf[0] | (pf->buf[1] << 8)
      | (pf->buf[2] << 16) | (pf->buf[3] << 24);

   uint8_t *ubuf LOCAL = xmalloc(ubufsz);

   z_stream strm;
   strm.zalloc = Z_NULL;
   strm.zfree  = Z_NULL;
   strm.opaque = Z_NULL;

   if (inflateInit(&strm) != Z_OK)
      fatal_trace("inflateInit failed");

   strm.next_in   = pf->buf + 4;
   strm.avail_in  = INT_MAX;
   strm.avail_out = ubufsz;
   strm.next_out  = ubuf;

   if (inflate(&strm, Z_FINISH) != Z_STREAM_END)
      fatal_trace("inflate failed");

   pf->wptr = ubuf;

   f->nirs    = unpack_uint(pf);
   f->nregs   = unpack_uint(pf);
   f->nvars   = unpack_uint(pf);
   f->cpoolsz = unpack_uint(pf);
   f->spec    = unpack_uint(pf);
   f->framesz = unpack_uint(pf);

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

   f->object = object_from_locus(unit, offset,
                                 (object_load_fn_t)lib_get_qualified);

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

   ACLEAR(pf->strings);
   pf->wptr = NULL;

   store_release(&(f->state), JIT_FUNC_READY);
   return true;
}
