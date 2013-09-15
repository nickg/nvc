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
#include "rt.h"
#include "tree.h"
#include "common.h"
#include "fstapi.h"

static tree_t  fst_top;
static void   *fst_ctx;
static ident_t fst_data_i;

typedef struct fst_data fst_data_t;

typedef void (*fst_fmt_fn_t)(tree_t, fst_data_t *);

struct fst_data {
   fstHandle     handle;
   fst_fmt_fn_t  fmt;
   range_kind_t  dir;
   const char   *map;
};

static const char std_logic_map[] = "UX01ZWLH-";
static const char bit_map[]       = "01";
static uint64_t   last_time;

static void fst_close(void)
{
   //fstWriterEmitTimeChange(fst_ctx, rt_now());
   fstWriterClose(fst_ctx);
}

static void fst_fmt_int(tree_t decl, fst_data_t *data)
{
   uint64_t val;
   rt_signal_value(decl, &val, 1, false);

   char buf[33];
   for (int i = 0; i < 32; i++)
      buf[31 - i] = (val & (1 << i)) ? '1' : '0';
   buf[32] = '\0';

   printf("emit %s\n", buf);

   fstWriterEmitValueChange(fst_ctx, data->handle, buf);
}

static void fst_event_cb(uint64_t now, tree_t decl)
{
   if (now != last_time) {
      fstWriterEmitTimeChange(fst_ctx, now);
      last_time = now;
   }

   fst_data_t *data = tree_attr_ptr(decl, fst_data_i);
   (*data->fmt)(decl, data);
}

void fst_restart(void)
{
   if (fst_ctx == NULL)
      return;

   const int ndecls = tree_decls(fst_top);
   for (int i = 0; i < ndecls; i++) {
      tree_t d = tree_decl(fst_top, i);
      if (tree_kind(d) != T_SIGNAL_DECL)
         continue;

      type_t type = tree_type(d);

      fst_data_t *data = xmalloc(sizeof(fst_data_t));
      memset(data, '\0', sizeof(fst_data_t));

      enum fstVarType vt;
      enum fstSupplimentalDataType sdt;
      int size;
      if (type_is_array(type)) {
         type_t elem = type_base_recur(type_elem(type));
         warn_at(tree_loc(d), "cannot represent arrays of type %s "
                 "in LXT format", type_pp(elem));
         free(data);
         continue;

         data->dir = type_dim(type, 0).kind;
      }
      else {
         type_t base = type_base_recur(type);
         switch (type_kind(base)) {
         case T_INTEGER:
            sdt  = FST_SDT_VHDL_INTEGER;
            vt   = FST_VT_VCD_INTEGER;
            size = 32;
            data->fmt = fst_fmt_int;
            break;

         case T_ENUM:
            // TODO

         default:
            warn_at(tree_loc(d), "cannot represent type %s in FST format",
                    type_pp(type));
            free(data);
            continue;
         }
      }

      data->handle = fstWriterCreateVar2(
         fst_ctx,
         vt,
         FST_VD_IMPLICIT,
         size,
         istr(tree_ident(d)),
         0,
         type_pp(type),
         FST_SVT_VHDL_SIGNAL,
         sdt);

      tree_add_attr_ptr(d, fst_data_i, data);

      rt_set_event_cb(d, fst_event_cb);
   }

   last_time = UINT64_MAX;
}

void fst_init(const char *file, tree_t top)
{
   fst_data_i = ident_new("fst_data");

   if ((fst_ctx = fstWriterCreate(file, 1)) == NULL)
      fatal("fstWriterCreate failed");

   fstWriterSetTimescale(fst_ctx, -15);

   atexit(fst_close);

   fst_top = top;
}
