//
//  Copyright (C) 2026  Nick Gasson
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
#include "rt/wave-live.h"

#include <assert.h>
#include <inttypes.h>
#include <stdio.h>
#include <string.h>

typedef struct {
   char     kind;   // 'L', 'R' or 'S'
   uint32_t width;  // bits for 'L'
} lvw_handle_t;

typedef struct _lvw {
   FILE         *file;
   lvw_handle_t *handles;   // indexed by handle - 1
   unsigned      nhandles;
   unsigned      maxhandles;
   bool          defs_ended;
} lvw_t;

lvw_t *lvw_create(const char *path)
{
   lvw_t *lw = xcalloc(sizeof(lvw_t));
   lw->file = fopen(path, "wb");
   if (lw->file == NULL)
      fatal_errno("%s", path);

   fputs("LVW 1\n", lw->file);
   return lw;
}

void lvw_flush(lvw_t *lw)
{
   fflush(lw->file);
}

void lvw_close(lvw_t *lw)
{
   fclose(lw->file);
   free(lw->handles);
   free(lw);
}

static void lvw_end_defs(lvw_t *lw)
{
   if (!lw->defs_ended) {
      fputs("E\n", lw->file);
      lw->defs_ended = true;
   }
}

void lvw_SetTimescale(lvw_t *lw, int exponent)
{
   fprintf(lw->file, "TS %d\n", exponent);
}

void lvw_SetScope(lvw_t *lw, enum fstScopeType type, const char *name,
                  const char *component)
{
   fprintf(lw->file, "S %d %s\n", (int)type, name);
}

void lvw_SetUpscope(lvw_t *lw)
{
   fputs("U\n", lw->file);
}

void lvw_SetAttrEnd(lvw_t *lw)
{
   // Attributes (array groupings) carry no information the viewer needs
}

void lvw_SetSourceStem(lvw_t *lw, const char *path, unsigned int line,
                       unsigned int use_realpath)
{
}

fstHandle lvw_CreateVar2(lvw_t *lw, enum fstVarType vt, enum fstVarDir vd,
                         uint32_t len, const char *name, fstHandle alias,
                         const char *type, enum fstSupplementalVarType svt,
                         enum fstSupplementalDataType sdt)
{
   char kind;
   switch (vt) {
   case FST_VT_VCD_REAL:
   case FST_VT_VCD_REAL_PARAMETER:
   case FST_VT_SV_SHORTREAL:
      kind = 'R';
      break;
   case FST_VT_GEN_STRING:
      kind = 'S';
      break;
   default:
      kind = 'L';
      break;
   }

   fstHandle h = alias;
   if (h == 0) {
      if (lw->nhandles == lw->maxhandles) {
         lw->maxhandles = MAX(64, lw->maxhandles * 2);
         lw->handles = xrealloc_array(lw->handles, lw->maxhandles,
                                      sizeof(lvw_handle_t));
      }
      h = ++lw->nhandles;
      lw->handles[h - 1].kind  = kind;
      lw->handles[h - 1].width = len;
   }

   fprintf(lw->file, "V %u %d %u %c %s\n", (unsigned)h, (int)vt,
           (unsigned)len, kind, name);
   return h;
}

fstEnumHandle lvw_CreateEnumTable(lvw_t *lw, const char *name,
                                  uint32_t elem_count, unsigned int min_valbits,
                                  const char **literal_arr,
                                  const char **val_arr)
{
   return 0;
}

void lvw_EmitEnumTableRef(lvw_t *lw, fstEnumHandle handle)
{
}

void lvw_EmitTimeChange(lvw_t *lw, uint64_t tim)
{
   lvw_end_defs(lw);
   fprintf(lw->file, "#%" PRIu64 "\n", tim);
}

void lvw_EmitValueChange(lvw_t *lw, fstHandle handle, const void *val)
{
   lvw_end_defs(lw);
   assert(handle >= 1 && handle <= lw->nhandles);
   const lvw_handle_t *h = &(lw->handles[handle - 1]);

   if (h->kind == 'R') {
      double d;
      memcpy(&d, val, sizeof(d));
      fprintf(lw->file, "%u R %.17g\n", (unsigned)handle, d);
   }
   else {
      // The value is exactly 'width' characters and is not always
      // NUL terminated
      fprintf(lw->file, "%u L ", (unsigned)handle);
      fwrite(val, 1, h->width, lw->file);
      fputc('\n', lw->file);
   }
}

void lvw_EmitVariableLengthValueChange(lvw_t *lw, fstHandle handle,
                                       const void *val, uint32_t len)
{
   lvw_end_defs(lw);
   assert(handle >= 1 && handle <= lw->nhandles);

   fprintf(lw->file, "%u S ", (unsigned)handle);
   const char *p = val;
   for (uint32_t i = 0; i < len; i++) {
      switch (p[i]) {
      case '\n': fputs("\\n", lw->file); break;
      case '\r': fputs("\\r", lw->file); break;
      case '\\': fputs("\\\\", lw->file); break;
      default: fputc(p[i], lw->file); break;
      }
   }
   fputc('\n', lw->file);
}
