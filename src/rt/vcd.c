//
//  Copyright (C) 2011-2013  Nick Gasson
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

#include <time.h>
#include <inttypes.h>
#include <string.h>
#include <assert.h>
#include <stdlib.h>

typedef struct vcd_data vcd_data_t;

typedef void (*vcd_fmt_fn_t)(tree_t, watch_t *, vcd_data_t *);

struct vcd_data {
   char          key[64];
   vcd_fmt_fn_t  fmt;
   range_kind_t  dir;
   const char   *map;
   size_t        size;
   watch_t      *watch;
};

static FILE    *vcd_file;
static tree_t   vcd_top;
static ident_t  vcd_data_i;
static ident_t  std_bit_i;
static ident_t  std_ulogic_i;
static uint64_t last_time;

static void vcd_fmt_int(tree_t decl, watch_t *w, vcd_data_t *data)
{
   uint64_t val;
   rt_watch_value(w, &val, 1, false);

   char buf[data->size + 1];
   for (size_t i = 0; i < data->size; i++)
      buf[data->size - 1 - i] = (val & (1 << i)) ? '1' : '0';
   buf[data->size] = '\0';

   fprintf(vcd_file, "b%s %s\n", buf, data->key);
}

static void vcd_fmt_chars(tree_t decl, watch_t *w, vcd_data_t *data)
{
   const int nvals = data->size;
   char buf[nvals + 1];
   rt_watch_string(w, data->map, buf, nvals + 1);

   fprintf(vcd_file, "b%s %s\n", buf, data->key);
}

static void vcd_event_cb(uint64_t now, tree_t decl, watch_t *w, void *user)
{
   if (now != last_time) {
      fprintf(vcd_file, "#%"PRIu64"\n", now);
      last_time = now;
   }

   vcd_data_t *data = user;
   if (likely(data != NULL))
      (*data->fmt)(decl, w, data);
}

static void vcd_key_fmt(int key, char *buf)
{
   char *p = buf;
   do {
      *p++ = 33 + (key % (126 - 33));
      key /= (126 - 33);
   } while (key > 0);
   *p = '\0';
}

static void vcd_emit_header(void)
{
   rewind(vcd_file);

   char tmbuf[64];
   time_t t = time(NULL);
   struct tm *tm = localtime(&t);
   strftime(tmbuf, sizeof(tmbuf), "%a, %d %b %Y %T %z", tm);
   fprintf(vcd_file, "$date\n  %s\n$end\n", tmbuf);

   fprintf(vcd_file, "$version\n  "PACKAGE_STRING"\n$end\n");
   fprintf(vcd_file, "$timescale\n  1 fs\n$end\n");
}

static bool vcd_can_fmt_chars(type_t type, vcd_data_t *data)
{
   type_t base = type_base_recur(type);
   ident_t name = type_ident(base);
   if (name == std_ulogic_i) {
      data->fmt = vcd_fmt_chars;
      data->map = "xx01zx01x";
      return true;
   }
   else if (name == std_bit_i) {
      data->fmt = vcd_fmt_chars;
      data->map = "01";
      return true;
   }
   else
      return false;
}

static void vcd_process_signal(tree_t d, int *next_key)
{
   type_t type = tree_type(d);
   type_t base = type_base_recur(type);

   vcd_data_t *data = xmalloc(sizeof(vcd_data_t));
   memset(data, '\0', sizeof(vcd_data_t));

   int msb = 0, lsb = 0;

   if (type_is_array(type)) {
      if (type_dims(type) > 1) {
         warn_at(tree_loc(d), "cannot represent multidimensional arrays "
                 "in VCD format");
         free(data);
         return;
      }

      range_t r = type_dim(type, 0);

      int64_t low, high;
      range_bounds(r, &low, &high);

      data->dir  = r.kind;
      data->size = high - low + 1;

      msb = assume_int(r.left);
      lsb = assume_int(r.right);

      type_t elem = type_elem(type);
      if (!vcd_can_fmt_chars(elem, data)) {
         warn_at(tree_loc(d), "cannot represent arrays of type %s "
                 "in VCD format", type_pp(elem));
         free(data);
         return;
      }
   }
   else {
      switch (type_kind(base)) {
      case T_INTEGER:
         {
            int64_t low, high;
            range_bounds(type_dim(type, 0), &low, &high);

            data->size = ilog2(high - low + 1);
            data->fmt  = vcd_fmt_int;
         }
         break;

      case T_ENUM:
         if (vcd_can_fmt_chars(type, data)) {
            data->size = 1;
            break;
         }
         // Fall-through

      default:
         warn_at(tree_loc(d), "cannot represent type %s in VCD format",
                 type_pp(type));
         free(data);
         return;
      }
   }

   const char *name_base = strrchr(istr(tree_ident(d)), ':') + 1;
   const size_t base_len = strlen(name_base);
   char name[base_len + 64];
   strncpy(name, name_base, base_len + 64);
   if (type_is_array(type))
      snprintf(name + base_len, 64, "[%d:%d]\n", msb, lsb);

   tree_add_attr_ptr(d, vcd_data_i, data);

   data->watch = rt_set_event_cb(d, vcd_event_cb, data);

   vcd_key_fmt(*next_key, data->key);

   fprintf(vcd_file, "$var reg %d %s %s $end\n",
           (int)data->size, data->key, name);

   ++(*next_key);
}

void vcd_restart(void)
{
   if (vcd_file == NULL)
      return;

   vcd_emit_header();

   int next_key = 0;
   const int ndecls = tree_decls(vcd_top);
   for (int i = 0; i < ndecls; i++) {
      tree_t d = tree_decl(vcd_top, i);
      switch (tree_kind(d)) {
      case T_HIER:
         fprintf(vcd_file, "$scope module %s $end\n", istr(tree_ident(d)));
         break;
      case T_SIGNAL_DECL:
         if (wave_should_dump(d))
            vcd_process_signal(d, &next_key);
         break;
      default:
         break;
      }

      int npop = tree_attr_int(d, ident_new("scope_pop"), 0);
      while (npop-- > 0)
         fprintf(vcd_file, "$upscope $end\n");
   }

   fprintf(vcd_file, "$enddefinitions $end\n");

   fprintf(vcd_file, "$dumpvars\n");

   last_time = UINT64_MAX;

   for (int i = 0; i < ndecls; i++) {
      tree_t d = tree_decl(vcd_top, i);
      if (tree_kind(d) == T_SIGNAL_DECL) {
         vcd_data_t *data = tree_attr_ptr(d, vcd_data_i);
         if (likely(data != NULL))
            vcd_event_cb(0, d, data->watch, data);
      }
   }

   fprintf(vcd_file, "$end\n");
}

void vcd_init(const char *filename, tree_t top)
{
   vcd_data_i   = ident_new("vcd_data");
   std_ulogic_i = ident_new("IEEE.STD_LOGIC_1164.STD_ULOGIC");
   std_bit_i    = ident_new("STD.STANDARD.BIT");

   vcd_top = top;

   warnf("Use of the VCD file format is discouraged as it cannot fully "
         "represent many VHDL types and the performance is poor for large "
         "designs. If you are using GtkWave the --wave option will generate "
         "an FST file that overcomes these limitations.");

   vcd_file = fopen(filename, "w");
   if (vcd_file == NULL)
      fatal_errno("failed to open VCD output %s", filename);
}
