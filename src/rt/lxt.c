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
#include "lxt_write.h"

#include <time.h>
#include <inttypes.h>
#include <string.h>
#include <assert.h>
#include <stdlib.h>

typedef void (*lxt_fmt_fn_t)(tree_t, struct lt_symbol *);

static struct lt_trace *trace = NULL;
static tree_t           lxt_top;
static ident_t          lxt_symbol_i;
static ident_t          lxt_fmt_fn_i;
static lxttime_t        last_time;

static void lxt_shutdown(void)
{
   assert(trace != NULL);
   lt_close(trace);
}

static void lxt_fmt_int(tree_t decl, struct lt_symbol *s)
{
   uint64_t val;
   rt_signal_value(decl, &val, 1, false);

   lt_emit_value_int(trace, s, 0, val);
}

static void lxt_event_cb(uint64_t now, tree_t decl)
{
   if (now != last_time)
      lt_set_time64(trace, now);

   struct lt_symbol *s = tree_attr_ptr(decl, lxt_symbol_i);
   lxt_fmt_fn_t fmt = tree_attr_ptr(decl, lxt_fmt_fn_i);

   (*fmt)(decl, s);
}

static int lxt_symbol_kind(tree_t decl)
{
   type_t type = tree_type(decl);

   switch (type_kind(type)) {
   case T_INTEGER:
      tree_add_attr_ptr(decl, lxt_fmt_fn_i, lxt_fmt_int);
      return LT_SYM_F_INTEGER;

   default:
      warn_at(tree_loc(decl), "cannot represent type %s in LXT format",
              type_pp(type));
      return -1;
   }
}

static char *lxt_fmt_name(tree_t decl)
{
   char *s = strdup(istr(tree_ident(decl)) + 1);
   for (char *p = s; *p != '\0'; p++) {
      if (*p == ':')
         *p = '.';
   }
   return s;
}

void lxt_restart(void)
{
   if (trace == NULL)
      return;

   lt_set_timescale(trace, -12);

   const int ndecls = tree_decls(lxt_top);
   for (int i = 0; i < ndecls; i++) {
      tree_t d = tree_decl(lxt_top, i);
      if (tree_kind(d) != T_SIGNAL_DECL)
         continue;

      type_t type = tree_type(d);

      int rows, msb;
      if (type_is_array(type)) {
         rows = type_dims(type);
         if ((rows > 1) || type_is_array(type_elem(type))) {
            warn_at(tree_loc(d), "cannot emit arrays of greater than one "
                    "dimension or arrays of arrays in LXT yet");
            continue;
         }

         int64_t low, high;
         range_bounds(type_dim(type, 0), &low, &high);
         msb = high - low;
      }
      else
         msb = rows = 0;

      const int kind = lxt_symbol_kind(d);
      if (kind == -1)
         continue;

      char *name = lxt_fmt_name(d);
      struct lt_symbol *s = lt_symbol_add(trace, name, rows, msb, 0, kind);
      free(name);
      tree_add_attr_ptr(d, lxt_symbol_i, s);

      lt_emit_value_int(trace, s, 0, 0);

      rt_set_event_cb(d, lxt_event_cb);
   }

   last_time = (lxttime_t)-1;
}

void lxt_init(const char *filename, tree_t top)
{
   lxt_symbol_i = ident_new("lxt_symbol");
   lxt_fmt_fn_i = ident_new("lxt_fmt_fn");

   if ((trace = lt_init(filename)) == NULL)
      fatal("lt_init failed");

   atexit(lxt_shutdown);

   lxt_top = top;
}
