//
//  Copyright (C) 2013-2022  Nick Gasson
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
#include "common.h"
#include "diag.h"
#include "fstapi.h"
#include "opt.h"
#include "rt.h"
#include "tree.h"
#include "type.h"

#include <assert.h>
#include <unistd.h>
#include <limits.h>
#include <string.h>

#if !defined __CYGWIN__ && !defined __MINGW32__
#include <libgen.h>
#endif

typedef struct {
   char  *text;
   size_t len;
} glob_t;

typedef A(glob_t) glob_array_t;

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
   fst_fmt_fn_t  fmt;
   fst_type_t    type;
   rt_watch_t   *watch;
   tree_t        decl;
   rt_signal_t  *signal;
   range_kind_t  dir;
   unsigned      size;
   unsigned      count;
   fstHandle     handle[];
};

static glob_array_t incl;
static glob_array_t excl;

static tree_t    fst_top;
static void     *fst_ctx;
static uint64_t  last_time = UINT64_MAX;
static FILE     *vcdfile;
static char     *tmpfst;

static void fst_process_signal(rt_scope_t *scope, tree_t d, tree_t cons);

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
   rt_signal_expand(data->signal, 0, &val, 1);

   char buf[data->size + 1];
   for (size_t i = 0; i < data->size; i++)
      buf[data->size - 1 - i] = (val & (1 << i)) ? '1' : '0';
   buf[data->size] = '\0';

   fstWriterEmitValueChange(fst_ctx, data->handle[0], buf);
}

static void fst_fmt_physical(rt_watch_t *w, fst_data_t *data)
{
   uint64_t val;
   rt_signal_expand(data->signal, 0, &val, 1);

   fst_unit_t *unit = data->type.units;
   while ((val % unit->mult) != 0)
      ++unit;

   char buf[128];
   checked_sprintf(buf, sizeof(buf), "%"PRIi64" %s",
                   val / unit->mult, unit->name);

   fstWriterEmitVariableLengthValueChange(
      fst_ctx, data->handle[0], buf, strlen(buf));
}

static void fst_fmt_chars(rt_watch_t *w, fst_data_t *data)
{
   const uint8_t *p = rt_signal_value(data->signal, 0);
   for (int i = 0; i < data->count; i++, p += data->size) {
      if (likely(data->type.map != NULL)) {
         char buf[data->size];
         for (int j = 0; j < data->size; j++)
            buf[j] = data->type.map[p[j]];
         fstWriterEmitValueChange(fst_ctx, data->handle[i], buf);
      }
      else
         fstWriterEmitVariableLengthValueChange(
            fst_ctx, data->handle[i], p, data->size);
   }
}

static void fst_fmt_enum(rt_watch_t *w, fst_data_t *data)
{
   uint64_t val;
   rt_signal_expand(data->signal, 0, &val, 1);

   type_t base = type_base_recur(tree_type(data->decl));
   tree_t lit = type_enum_literal(base, val);
   const char *str = istr(tree_ident(lit));

   fstWriterEmitVariableLengthValueChange(
      fst_ctx, data->handle[0], str, strlen(str));
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

   switch (is_well_known(type_ident(base))) {
   case W_STD_ULOGIC:
      if (type_ident(type) == well_known(W_STD_LOGIC))
         *sdt = (data->size > 1) ?
            FST_SDT_VHDL_STD_LOGIC_VECTOR : FST_SDT_VHDL_STD_LOGIC;
      else
         *sdt = (data->size > 1) ?
            FST_SDT_VHDL_STD_ULOGIC_VECTOR : FST_SDT_VHDL_STD_ULOGIC;
      *vt = FST_VT_SV_LOGIC;
      data->fmt = fst_fmt_chars;
      data->type.map = "UX01ZWLH-";
      return true;

   case W_STD_BIT:
      *sdt = FST_SDT_VHDL_BIT;
      *vt  = FST_VT_SV_LOGIC;
      data->fmt = fst_fmt_chars;
      data->type.map = "01";
      return true;

   case W_STD_CHAR:
      if (data->size > 0) {
         *sdt = FST_SDT_VHDL_STRING;
         *vt  = FST_VT_GEN_STRING;
         data->fmt = fst_fmt_chars;
         data->type.map = NULL;
         return true;
      }
      else
         return false;

   default:
      return false;
   }
}

static void fst_create_array_var(tree_t d, rt_signal_t *s, type_t type,
                                 tree_t cons)
{
   fst_data_t *data = NULL;

   enum fstVarType vt;
   enum fstSupplementalDataType sdt;

   if (dimension_of(type) > 1) {
      warn_at(tree_loc(d), "cannot represent multidimensional arrays "
              "in FST format");
      return;
   }

   tree_t r = cons ? tree_range(cons, 0) : range_of(type, 0);

   int64_t low, high;
   range_bounds(r, &low, &high);

   enum fstVarDir dir = FST_VD_IMPLICIT;

   if (tree_kind(d) == T_PORT_DECL) {
      switch (tree_subkind(d)) {
      case PORT_IN:     dir = FST_VD_INPUT; break;
      case PORT_OUT:    dir = FST_VD_OUTPUT; break;
      case PORT_INOUT:  dir = FST_VD_INOUT; break;
      case PORT_BUFFER: dir = FST_VD_BUFFER; break;
      }
   }

   type_t elem = type_elem(type);
   if (type_is_array(elem)) {
      // Dumping memories and nested arrays can be slow
      if (!opt_get_int(OPT_DUMP_ARRAYS))
         return;

      const int length = MAX(high - low + 1, 0);
      data = xcalloc_flex(sizeof(fst_data_t), length, sizeof(fstHandle));
      data->count = length;
      data->dir   = tree_subkind(r);

      type_t elem2 = type_elem(elem);
      tree_t elem_r = range_of(elem, 0);

      int64_t e_low, e_high;
      range_bounds(elem_r, &e_low, &e_high);

      data->size = e_high - e_low + 1;

      if (!fst_can_fmt_chars(elem2, data, &vt, &sdt)) {
         warn_at(tree_loc(d), "cannot represent arrays of array of type %s "
                 "in FST format", type_pp(elem2));
         free(data);
         return;
      }
      else {
         switch (is_well_known(type_ident(type_base_recur(elem)))) {
         case W_IEEE_UNSIGNED: sdt = FST_SDT_VHDL_UNSIGNED; break;
         case W_IEEE_SIGNED:   sdt = FST_SDT_VHDL_SIGNED; break;
         default: break;
         }
      }

      const int msb = assume_int(tree_left(elem_r));
      const int lsb = assume_int(tree_right(elem_r));

      for (int i = 0; i < length; i++) {
         LOCAL_TEXT_BUF tb = tb_new();
         tb_istr(tb, tree_ident(d));
         tb_printf(tb, "[%"PRIi64"][%d:%d]", low + i, msb, lsb);

         data->handle[i] = fstWriterCreateVar2(
            fst_ctx,
            vt,
            dir,
            data->size,
            tb_get(tb),
            0,
            type_pp(elem),
            FST_SVT_VHDL_SIGNAL,
            sdt);
      }

      fstWriterSetAttrEnd(fst_ctx);
   }
   else {
      data = xcalloc(sizeof(fst_data_t) + sizeof(fstHandle));
      data->count = 1;
      data->dir   = tree_subkind(r);
      data->size  = high - low + 1;

      if (!fst_can_fmt_chars(elem, data, &vt, &sdt)) {
         warn_at(tree_loc(d), "cannot represent arrays of type %s "
                 "in FST format", type_pp(elem));
         free(data);
         return;
      }
      else {
         switch (is_well_known(type_ident(type_base_recur(type)))) {
         case W_IEEE_UNSIGNED: sdt = FST_SDT_VHDL_UNSIGNED; break;
         case W_IEEE_SIGNED:   sdt = FST_SDT_VHDL_SIGNED; break;
         default: break;
         }
      }

      const int msb = assume_int(tree_left(r));
      const int lsb = assume_int(tree_right(r));

      LOCAL_TEXT_BUF tb = tb_new();
      tb_istr(tb, tree_ident(d));
      tb_printf(tb, "[%d:%d]", msb, lsb);

      data->handle[0] = fstWriterCreateVar2(
         fst_ctx,
         vt,
         dir,
         data->size,
         tb_get(tb),
         0,
         type_pp(type),
         FST_SVT_VHDL_SIGNAL,
         sdt);

   }

   data->decl   = d;
   data->signal = s;
   data->watch  = rt_set_event_cb(data->signal, fst_event_cb, data, true);

   fst_event_cb(0, data->signal, data->watch, data);
}

static void fst_create_scalar_var(tree_t d, rt_signal_t *s, type_t type)
{
   type_t base = type_base_recur(type);

   fst_data_t *data = xcalloc(sizeof(fst_data_t) + sizeof(fstHandle));
   data->count = 1;

   enum fstVarType vt;
   enum fstSupplementalDataType sdt;

   switch (type_kind(base)) {
   case T_INTEGER:
      {
         switch (is_well_known(type_ident(type))) {
         case W_STD_NATURAL:  sdt = FST_SDT_VHDL_NATURAL; break;
         case W_STD_POSITIVE: sdt = FST_SDT_VHDL_POSITIVE; break;
         default:             sdt = FST_SDT_VHDL_INTEGER; break;
         }

         int64_t low, high;
         range_bounds(range_of(type, 0), &low, &high);

         vt = FST_VT_VCD_INTEGER;
         data->size = ilog2(high - low + 1);
         data->fmt  = fst_fmt_int;
      }
      break;

   case T_ENUM:
      if (!fst_can_fmt_chars(type, data, &vt, &sdt)) {
         switch (is_well_known(type_ident(base))) {
         case W_STD_BOOL: sdt = FST_SDT_VHDL_BOOLEAN; break;
         case W_STD_CHAR: sdt = FST_SDT_VHDL_CHARACTER; break;
         default:         sdt = FST_SDT_NONE; break;
         }

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

   enum fstVarDir dir = FST_VD_IMPLICIT;

   if (tree_kind(d) == T_PORT_DECL) {
      switch (tree_subkind(d)) {
      case PORT_IN:     dir = FST_VD_INPUT; break;
      case PORT_OUT:    dir = FST_VD_OUTPUT; break;
      case PORT_INOUT:  dir = FST_VD_INOUT; break;
      case PORT_BUFFER: dir = FST_VD_BUFFER; break;
      }
   }

   data->handle[0] = fstWriterCreateVar2(
      fst_ctx,
      vt,
      dir,
      data->size,
      istr(tree_ident(d)),
      0,
      type_pp(type),
      FST_SVT_VHDL_SIGNAL,
      sdt);

   data->decl   = d;
   data->signal = s;
   data->watch  = rt_set_event_cb(data->signal, fst_event_cb, data, true);

   fst_event_cb(0, data->signal, data->watch, data);
}

static void fst_create_record_var(tree_t d, rt_scope_t *scope, type_t type)
{
   fstWriterSetScope(fst_ctx, FST_ST_VHDL_RECORD, istr(tree_ident(d)), NULL);

   const int nfields = type_fields(type);
   for (int i = 0; i < nfields; i++) {
      tree_t f = type_field(type, i);
      tree_t cons = type_constraint_for_field(type, f);
      fst_process_signal(scope, f, cons);
   }

   fstWriterSetUpscope(fst_ctx);
}

static void fst_process_signal(rt_scope_t *scope, tree_t d, tree_t cons)
{
   type_t type = tree_type(d);
   if (type_is_record(type)) {
      rt_scope_t *sub = rt_child_scope(scope, d);
      if (sub == NULL)
         ;    // Signal was optimised out
      else
         fst_create_record_var(d, sub, type);
   }
   else {
      rt_signal_t *s = rt_find_signal(scope, d);
      if (s == NULL)
         ;    // Signal was optimised out
      else if (type_is_array(type))
         fst_create_array_var(d, s, type, cons);
      else
         fst_create_scalar_var(d, s, type);
   }
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

static void fst_walk_design(tree_t block)
{
   tree_t h = tree_decl(block, 0);
   assert(tree_kind(h) == T_HIER);
   fst_process_hier(h, block);

   ident_t hpath = tree_ident(h);

   rt_scope_t *scope = rt_find_scope(block);
   if (scope == NULL)
      fatal_trace("missing scope for %s", istr(hpath));

   const int nports = tree_ports(block);
   for (int i = 0; i < nports; i++) {
      tree_t p = tree_port(block, i);
      ident_t path = ident_prefix(hpath, ident_downcase(tree_ident(p)), ':');
      if (wave_should_dump(path))
         fst_process_signal(scope, p, NULL);
   }

   const int ndecls = tree_decls(block);
   for (int i = 0; i < ndecls; i++) {
      tree_t d = tree_decl(block, i);
      if (tree_kind(d) == T_SIGNAL_DECL) {
         ident_t path = ident_prefix(hpath, ident_downcase(tree_ident(d)), ':');
         if (wave_should_dump(path))
            fst_process_signal(scope, d, NULL);
      }
   }

   const int nstmts = tree_stmts(block);
   for (int i = 0; i < nstmts; i++) {
      tree_t s = tree_stmt(block, i);
      switch (tree_kind(s)) {
      case T_BLOCK:
         fst_walk_design(s);
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

void wave_restart(void)
{
   if (fst_ctx == NULL)
      return;

   last_time = UINT64_MAX;

   fst_walk_design(tree_stmt(fst_top, 0));
}

void wave_init(const char *file, tree_t top, wave_output_t output)
{
   if (output == WAVE_OUTPUT_VCD) {
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

void wave_include_glob(const char *glob)
{
   APUSH(incl, ((glob_t){ .text = strdup(glob), .len = strlen(glob) }));
}

void wave_exclude_glob(const char *glob)
{
   APUSH(excl, ((glob_t){ .text = strdup(glob), .len = strlen(glob) }));
}

static void wave_process_file(const char *fname, bool include)
{
   FILE *f = fopen(fname, "r");
   if (f == NULL)
      return;

   notef("%s signals from %s", include ? "including" : "excluding", fname);

   int lineno = 0;
   char line[1024];
   while (!feof(f) && (lineno++, fgets(line, sizeof(line), f) != NULL)) {
      // Erase comments
      bool comment = false;
      for (char *p = line; *p != '\0'; p++) {
         if (*p == '#')
            comment = true;
         if (comment || (*p == '\r') || (*p == '\n'))
            *p = '\0';
      }

      char glob[1024];
      if (sscanf(line, " %1023s ", glob) == 1) {
         if (include)
            wave_include_glob(glob);
         else
            wave_exclude_glob(glob);
      }
   }

   fclose(f);
}

void wave_include_file(const char *base)
{
   char *inclf LOCAL = xasprintf("%s.include", base);
   wave_process_file(inclf, true);

   char *exclf LOCAL = xasprintf("%s.exclude", base);
   wave_process_file(exclf, false);
}

bool wave_should_dump(ident_t name)
{
   for (int i = 0; i < excl.count; i++) {
      if (ident_glob(name, excl.items[i].text, excl.items[i].len))
         return false;
   }

   for (int i = 0; i < incl.count; i++) {
      if (ident_glob(name, incl.items[i].text, incl.items[i].len))
         return true;
   }

   return (incl.count == 0);
}
