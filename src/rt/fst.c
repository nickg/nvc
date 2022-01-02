//
//  Copyright (C) 2013-2021  Nick Gasson
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
#include "enode.h"
#include "type.h"

#include <assert.h>
#include <unistd.h>
#include <limits.h>

#if !defined __CYGWIN__ && !defined __MINGW32__
#include <libgen.h>
#endif

static tree_t    fst_top;
static void     *fst_ctx;
static uint64_t  last_time;
static FILE     *vcdfile;
static char     *tmpfst;

typedef struct fst_data fst_data_t;

typedef void (*fst_fmt_fn_t)(rt_watch_t *, fst_data_t *);

typedef struct {
   int64_t  mult;
   char    *name;
} fst_unit_t;

typedef union {
   const char *map;
   fst_unit_t *units;
} fst_type_t;

struct fst_data {
   fstHandle     handle;
   fst_fmt_fn_t  fmt;
   range_kind_t  dir;
   fst_type_t    type;
   size_t        size;
   rt_watch_t   *watch;
   tree_t        decl;
   rt_signal_t  *signal;
};

static void fst_close(void)
{
   fstWriterEmitTimeChange(fst_ctx, rt_now(NULL));
   fstWriterClose(fst_ctx);

   if (vcdfile) {
      void *xc = fstReaderOpen(tmpfst);
      if (xc == NULL)
         fatal("fstReaderOpen failed for temporary FST file");

      fstReaderSetVcdExtensions(xc, 1);
      if (!fstReaderProcessHier(xc, vcdfile))
         fatal("fstReaderProcessHier failed");

      fstReaderSetFacProcessMaskAll(xc);
      fstReaderIterBlocks(xc, NULL, NULL, vcdfile);

      fstReaderClose(xc);
      fclose(vcdfile);
      vcdfile = NULL;

      if (unlink(tmpfst) != 0)
         fatal_errno("unlink: %s", tmpfst);

#if !defined __CYGWIN__ && !defined __MINGW32__
      char *tmpdir = dirname(tmpfst);
      if (rmdir(tmpdir) != 0)
         fatal_errno("unlink: %s", tmpdir);
#endif

      free(tmpfst);
      tmpfst = NULL;
   }
}

static void fst_fmt_int(rt_watch_t *w, fst_data_t *data)
{
   uint64_t val;
   rt_watch_value(w, &val, 1);

   char buf[data->size + 1];
   for (size_t i = 0; i < data->size; i++)
      buf[data->size - 1 - i] = (val & (1 << i)) ? '1' : '0';
   buf[data->size] = '\0';

   fstWriterEmitValueChange(fst_ctx, data->handle, buf);
}

static void fst_fmt_physical(rt_watch_t *w, fst_data_t *data)
{
   uint64_t val;
   rt_watch_value(w, &val, 1);

   fst_unit_t *unit = data->type.units;
   while ((val % unit->mult) != 0)
      ++unit;

   char buf[128];
   checked_sprintf(buf, sizeof(buf), "%"PRIi64" %s",
                   val / unit->mult, unit->name);

   fstWriterEmitVariableLengthValueChange(
      fst_ctx, data->handle, buf, strlen(buf));
}

static void fst_fmt_chars(rt_watch_t *w, fst_data_t *data)
{
   const int nvals = data->size;
   char buf[nvals + 1];
   rt_watch_string(w, data->type.map, buf, nvals + 1);
   if (likely(data->type.map != NULL))
      fstWriterEmitValueChange(fst_ctx, data->handle, buf);
   else
      fstWriterEmitVariableLengthValueChange(
         fst_ctx, data->handle, buf, data->size);
}

static void fst_fmt_enum(rt_watch_t *w, fst_data_t *data)
{
   uint64_t val;
   rt_watch_value(w, &val, 1);

   tree_t lit = type_enum_literal(tree_type(data->decl), val);
   const char *str = istr(tree_ident(lit));

   fstWriterEmitVariableLengthValueChange(
      fst_ctx, data->handle, str, strlen(str));
}

static void fst_event_cb(uint64_t now, rt_signal_t *s, rt_watch_t *w,
                         void *user)
{
   if (now != last_time) {
      fstWriterEmitTimeChange(fst_ctx, now);
      last_time = now;
   }

   fst_data_t *data = user;
   if (likely(data != NULL))
      (*data->fmt)(w, data);
}

static fst_unit_t *fst_make_unit_map(type_t type)
{
   type_t base = type_base_recur(type);

   const int nunits = type_units(base);

   fst_unit_t *map = xmalloc(nunits * sizeof(fst_unit_t));
   for (int i = 0; i < nunits; i++) {
      tree_t unit = type_unit(base, nunits - 1 - i);
      map[i].mult = assume_int(tree_value(unit));
      map[i].name = strdup(istr(tree_ident(unit)));
   }

   return map;
}

static bool fst_can_fmt_chars(type_t type, fst_data_t *data,
                              enum fstVarType *vt,
                              enum fstSupplementalDataType *sdt)
{
   type_t base = type_base_recur(type);
   ident_t name = type_ident(base);
   if (name == std_ulogic_i) {
      if (type_ident(type) == std_logic_i)
         *sdt = (data->size > 1) ?
            FST_SDT_VHDL_STD_LOGIC_VECTOR : FST_SDT_VHDL_STD_LOGIC;
      else
         *sdt = (data->size > 1) ?
            FST_SDT_VHDL_STD_ULOGIC_VECTOR : FST_SDT_VHDL_STD_ULOGIC;
      *vt = FST_VT_SV_LOGIC;
      data->fmt = fst_fmt_chars;
      data->type.map = "UX01ZWLH-";
      return true;
   }
   else if (name == std_bit_i) {
      *sdt = FST_SDT_VHDL_BIT;
      *vt  = FST_VT_SV_LOGIC;
      data->fmt = fst_fmt_chars;
      data->type.map = "01";
      return true;
   }
   else if ((name == std_char_i) && (data->size > 0)) {
      *sdt = FST_SDT_VHDL_STRING;
      *vt  = FST_VT_GEN_STRING;
      data->fmt = fst_fmt_chars;
      data->type.map = NULL;
      return true;
   }
   else
      return false;
}

static void fst_process_signal(tree_t d, e_node_t e)
{
   assert(tree_ident(d) == e_ident(e));

   type_t type = tree_type(d);
   type_t base = type_base_recur(type);

   fst_data_t *data = xcalloc(sizeof(fst_data_t));

   int msb = 0, lsb = 0;

   enum fstVarType vt;
   enum fstSupplementalDataType sdt;
   if (type_is_array(type)) {
      if (dimension_of(type) > 1) {
         warn_at(tree_loc(d), "cannot represent multidimensional arrays "
                 "in FST format");
         free(data);
         return;
      }

      tree_t r = range_of(type, 0);

      int64_t low, high;
      range_bounds(r, &low, &high);

      data->dir  = tree_subkind(r);
      data->size = high - low + 1;

      msb = assume_int(tree_left(r));
      lsb = assume_int(tree_right(r));

      type_t elem = type_elem(type);
      if (!fst_can_fmt_chars(elem, data, &vt, &sdt)) {
         warn_at(tree_loc(d), "cannot represent arrays of type %s "
                 "in FST format", type_pp(elem));
         free(data);
         return;
      }
      else {
         ident_t ident = type_ident(base);
         if (ident == unsigned_i)
            sdt = FST_SDT_VHDL_UNSIGNED;
         else if (ident == signed_i)
            sdt = FST_SDT_VHDL_SIGNED;
      }
   }
   else {
      switch (type_kind(base)) {
      case T_INTEGER:
         {
            ident_t ident = type_ident(type);
            if (ident == natural_i)
               sdt = FST_SDT_VHDL_NATURAL;
            else if (ident == positive_i)
               sdt = FST_SDT_VHDL_POSITIVE;
            else
               sdt = FST_SDT_VHDL_INTEGER;

            int64_t low, high;
            range_bounds(range_of(type, 0), &low, &high);

            vt = FST_VT_VCD_INTEGER;
            data->size = ilog2(high - low + 1);
            data->fmt  = fst_fmt_int;
         }
         break;

      case T_ENUM:
         if (!fst_can_fmt_chars(type, data, &vt, &sdt)) {
            ident_t ident = type_ident(base);
            if (ident == std_bool_i)
               sdt = FST_SDT_VHDL_BOOLEAN;
            else if (ident == std_char_i)
               sdt = FST_SDT_VHDL_CHARACTER;
            else
               sdt = FST_SDT_NONE;

            vt = FST_VT_GEN_STRING;
            data->size = 0;
            data->fmt  = fst_fmt_enum;
         }
         else
            data->size = 1;
         break;

      case T_PHYSICAL:
         {
            sdt = FST_SDT_NONE;
            vt  = FST_VT_GEN_STRING;
            data->size = 0;
            data->type.units = fst_make_unit_map(type);
            data->fmt = fst_fmt_physical;
         }
         break;

      default:
         warn_at(tree_loc(d), "cannot represent type %s in FST format",
                 type_pp(type));
         free(data);
         return;
      }
   }

   enum fstVarDir dir = FST_VD_IMPLICIT;

   if (tree_kind(d) == T_PORT_DECL) {
      switch (tree_subkind(d)) {
      case PORT_IN:     dir = FST_VD_INPUT; break;
      case PORT_OUT:    dir = FST_VD_OUTPUT; break;
      case PORT_INOUT:  dir = FST_VD_INOUT; break;
      case PORT_BUFFER: dir = FST_VD_BUFFER; break;
      }
   }

   const char *name_base = istr(e_ident(e));
   const size_t base_len = strlen(name_base);
   char name[base_len + 64];
   if (type_is_array(type))
      checked_sprintf(name, sizeof(name), "%s[%d:%d]\n", name_base, msb, lsb);
   else
      checked_sprintf(name, sizeof(name), "%s", name_base);

   data->handle = fstWriterCreateVar2(
      fst_ctx,
      vt,
      dir,
      data->size,
      name,
      0,
      type_pp(type),
      FST_SVT_VHDL_SIGNAL,
      sdt);

   data->decl   = d;
   data->signal = rt_find_signal(e);
   data->watch  = rt_set_event_cb(data->signal, fst_event_cb, data, true);
}

static void fst_process_hier(tree_t h, tree_t block)
{
   const tree_kind_t scope_kind = tree_subkind(h);

   enum fstScopeType st;
   switch (scope_kind) {
   case T_ARCH: st = FST_ST_VHDL_ARCHITECTURE; break;
   case T_BLOCK: st = FST_ST_VHDL_BLOCK; break;
   case T_FOR_GENERATE: st = FST_ST_VHDL_FOR_GENERATE; break;
   case T_PACKAGE: st = FST_ST_VHDL_PACKAGE; break;
   default:
      st = FST_ST_VHDL_ARCHITECTURE;
      warn_at(tree_loc(h), "no FST scope type for %s",
              tree_kind_str(scope_kind));
      break;
   }

   const loc_t *loc = tree_loc(h);
   fstWriterSetSourceStem(fst_ctx, loc_file_str(loc), loc->first_line, 1);

   // TODO: store the component name in T_HIER somehow?
   fstWriterSetScope(fst_ctx, st, istr(ident_downcase(tree_ident(block))), "");
}

static void fst_walk_design(tree_t block, e_node_t scope)
{
   int nsignal = 0;

   tree_t h = tree_decl(block, 0);
   assert(tree_kind(h) == T_HIER);
   fst_process_hier(h, block);

   const int nports = tree_ports(block);
   for (int i = 0; i < nports; i++) {
      tree_t p = tree_port(block, i);
      e_node_t e = e_signal(scope, nsignal++);
      if (wave_should_dump(p))
         fst_process_signal(p, e);
   }

   const int ndecls = tree_decls(block);
   for (int i = 0; i < ndecls; i++) {
      tree_t d = tree_decl(block, i);
      if (tree_kind(d) == T_SIGNAL_DECL) {
         e_node_t e = e_signal(scope, nsignal++);
         if (wave_should_dump(d))
            fst_process_signal(d, e);
      }
   }

   int nscope = 0;
   const int nstmts = tree_stmts(block);
   for (int i = 0; i < nstmts; i++) {
      tree_t s = tree_stmt(block, i);
      switch (tree_kind(s)) {
      case T_BLOCK:
         fst_walk_design(s, e_scope(scope, nscope++));
         break;
      case T_PROCESS:
         break;
      default:
         fatal_trace("cannot handle tree kind %s in fst_walk_design",
                     tree_kind_str(tree_kind(s)));
      }
   }

   fstWriterSetUpscope(fst_ctx);
}

void fst_restart(void)
{
   if (fst_ctx == NULL)
      return;

   e_node_t e_root = lib_get_eopt(lib_work(), fst_top);
   fst_walk_design(tree_stmt(fst_top, 0),
                   e_scope(e_root, e_scopes(e_root) - 1));

   last_time = UINT64_MAX;
}

void fst_init(const char *file, tree_t top, fst_output_t output)
{
   if (output == FST_OUTPUT_VCD) {
#if defined __CYGWIN__ || defined __MINGW32__
      const char *tmpdir = ".";
#else
      char tmpdir[PATH_MAX] = "/tmp/vcdXXXXXX";
      checked_sprintf(tmpdir, PATH_MAX, "%s/vcdXXXXXX",
                      getenv("TMPDIR") ?: "/tmp");
      if (mkdtemp(tmpdir) == NULL)
         fatal_errno("mkdtemp");
#endif

      vcdfile = fopen(file, "wb");
      if (vcdfile == NULL)
         fatal_errno("%s", file);

      tmpfst  = xasprintf("%s" DIR_SEP "temp.fst", tmpdir);
      fst_ctx = fstWriterCreate(tmpfst, 1);
   }
   else {
      vcdfile = NULL;
      tmpfst  = NULL;
      fst_ctx = fstWriterCreate(file, 1);
   }

   if (fst_ctx == NULL)
      fatal("fstWriterCreate failed");

   fstWriterSetFileType(fst_ctx, FST_FT_VHDL);
   fstWriterSetTimescale(fst_ctx, -15);
   fstWriterSetVersion(fst_ctx, PACKAGE_STRING);
   fstWriterSetPackType(fst_ctx, 0);
   fstWriterSetRepackOnClose(fst_ctx, 1);
   fstWriterSetParallelMode(fst_ctx, 0);

   atexit(fst_close);

   fst_top = top;
}
