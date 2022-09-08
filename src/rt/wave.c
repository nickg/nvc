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
#include "hash.h"
#include "opt.h"
#include "rt/model.h"
#include "rt/rt.h"
#include "rt/wave.h"
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

typedef struct _fst_data fst_data_t;

typedef void (*fst_fmt_fn_t)(rt_watch_t *, fst_data_t *);

typedef struct {
   int64_t  mult;
   char    *name;
} fst_unit_t;

typedef struct {
   unsigned  count;
   unsigned  size;
   char     *strings;
} fst_enum_t;

typedef struct {
   fst_fmt_fn_t                 fn;
   enum fstVarType              vartype;
   enum fstSupplementalDataType sdt;
   unsigned                     size;
   union {
      const char  *map;
      fst_unit_t  *units;
      fst_enum_t   literals;
   } u;
} fst_type_t;

typedef struct _fst_data {
   wave_dumper_t *dumper;
   fst_type_t    *type;
   rt_watch_t    *watch;
   tree_t         decl;
   rt_signal_t   *signal;
   range_kind_t   dir;
   unsigned       size;
   unsigned       count;
   fstHandle      handle[];
} fst_data_t;

typedef struct {
   FILE       *file;
   int         colour;
   text_buf_t *hier;
} gtkw_writer_t;

typedef struct _wave_dumper {
   tree_t         top;
   void          *fst_ctx;
   rt_model_t    *model;
   gtkw_writer_t *gtkw;
   FILE          *vcdfile;
   char          *tmpfst;
   uint64_t       last_time;
} wave_dumper_t;

static glob_array_t incl;
static glob_array_t excl;

static void fst_process_signal(wave_dumper_t *wd, rt_scope_t *scope, tree_t d,
                               tree_t cons, text_buf_t *tb);
static bool wave_should_dump(ident_t name);

static void fst_close(rt_model_t *m, void *arg)
{
   wave_dumper_t *wd = arg;

   fstWriterEmitTimeChange(wd->fst_ctx, model_now(m, NULL));
   fstWriterClose(wd->fst_ctx);

   if (wd->vcdfile) {
      void *xc = fstReaderOpen(wd->tmpfst);
      if (xc == NULL)
         fatal("fstReaderOpen failed for temporary FST file");

      fstReaderSetVcdExtensions(xc, 1);
      if (!fstReaderProcessHier(xc, wd->vcdfile))
         fatal("fstReaderProcessHier failed");

      fstReaderSetFacProcessMaskAll(xc);
      fstReaderIterBlocks(xc, NULL, NULL, wd->vcdfile);

      fstReaderClose(xc);
      fclose(wd->vcdfile);
      wd->vcdfile = NULL;

      if (unlink(wd->tmpfst) != 0)
         fatal_errno("unlink: %s", wd->tmpfst);

#if !defined __CYGWIN__ && !defined __MINGW32__
      char *tmpdir = dirname(wd->tmpfst);
      if (rmdir(tmpdir) != 0)
         fatal_errno("unlink: %s", tmpdir);
#endif

      free(wd->tmpfst);
      wd->tmpfst = NULL;
   }

   wd->fst_ctx = NULL;
   wd->model   = NULL;
}

static void fst_fmt_int(rt_watch_t *w, fst_data_t *data)
{
   uint64_t val;
   signal_expand(data->signal, 0, &val, 1);

   char buf[data->type->size + 1];
   for (size_t i = 0; i < data->type->size; i++)
      buf[data->type->size - 1 - i] = (val & (1 << i)) ? '1' : '0';
   buf[data->type->size] = '\0';

   fstWriterEmitValueChange(data->dumper->fst_ctx, data->handle[0], buf);
}

static void fst_fmt_real(rt_watch_t *w, fst_data_t *data)
{
   const void *buf = signal_value(data->signal);
   fstWriterEmitValueChange(data->dumper->fst_ctx, data->handle[0], buf);
}

static void fst_fmt_physical(rt_watch_t *w, fst_data_t *data)
{
   uint64_t val;
   signal_expand(data->signal, 0, &val, 1);

   fst_unit_t *unit = data->type->u.units;
   while ((val % unit->mult) != 0)
      ++unit;

   char buf[128];
   checked_sprintf(buf, sizeof(buf), "%"PRIi64" %s",
                   val / unit->mult, unit->name);

   fstWriterEmitVariableLengthValueChange(
      data->dumper->fst_ctx, data->handle[0], buf, strlen(buf));
}

static void fst_fmt_chars(rt_watch_t *w, fst_data_t *data)
{
   const uint8_t *p = signal_value(data->signal);
   for (int i = 0; i < data->count; i++, p += data->size) {
      if (likely(data->type->u.map != NULL)) {
         char buf[data->size];
         for (int j = 0; j < data->size; j++)
            buf[j] = data->type->u.map[p[j]];
         fstWriterEmitValueChange(data->dumper->fst_ctx, data->handle[i], buf);
      }
      else
         fstWriterEmitVariableLengthValueChange(
            data->dumper->fst_ctx, data->handle[i], p, data->size);
   }
}

static void fst_fmt_enum(rt_watch_t *w, fst_data_t *data)
{
   uint64_t val;
   signal_expand(data->signal, 0, &val, 1);

   fst_enum_t *e = &(data->type->u.literals);
   assert(val < e->count);

   const char *literal = e->strings + val * e->size;
   fstWriterEmitVariableLengthValueChange(data->dumper->fst_ctx,
                                          data->handle[0],
                                          literal,
                                          strnlen(literal, e->size));
}

static void fst_event_cb(uint64_t now, rt_signal_t *s, rt_watch_t *w,
                         void *user)
{
   fst_data_t *data = user;

   if (now != data->dumper->last_time) {
      fstWriterEmitTimeChange(data->dumper->fst_ctx, now);
      data->dumper->last_time = now;
   }

   if (likely(data != NULL))
      (*data->type->fn)(w, data);
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

static fst_type_t *fst_type_for(type_t type, const loc_t *loc)
{
   const type_kind_t kind = type_kind(type);
   if (kind == T_SUBTYPE && !type_has_ident(type)) {
      // Do not cache anonymous subtypes
      return fst_type_for(type_base(type), loc);
   }

   static hash_t *cache = NULL;
   if (cache == NULL)
      cache = hash_new(128);

   fst_type_t *ft = hash_get(cache, type);
   if (ft == (void *)-1)
      return NULL;   // Failed for this type earlier
   else if (ft != NULL)
      return ft;

   ft = xcalloc(sizeof(fst_type_t));

   switch (type_kind(type)) {
   case T_SUBTYPE:
      {
         *ft = *fst_type_for(type_base(type), loc);

         switch (is_well_known(type_ident(type))) {
         case W_STD_NATURAL:  ft->sdt = FST_SDT_VHDL_NATURAL; break;
         case W_STD_POSITIVE: ft->sdt = FST_SDT_VHDL_POSITIVE; break;
         case W_IEEE_LOGIC:   ft->sdt = FST_SDT_VHDL_STD_LOGIC; break;
         default: break;
         }
      }
      break;

   case T_INTEGER:
      {
         int64_t low, high;
         range_bounds(range_of(type, 0), &low, &high);

         ft->vartype = FST_VT_VCD_INTEGER;
         ft->fn      = fst_fmt_int;
         ft->size    = bits_for_range(low, high);
         ft->sdt     = FST_SDT_VHDL_INTEGER;
      }
      break;

   case T_REAL:
      ft->vartype = FST_VT_VCD_REAL;
      ft->fn      = fst_fmt_real;
      ft->sdt     = FST_SDT_VHDL_REAL;
      break;

   case T_ENUM:
      {
         switch (is_well_known(type_ident(type))) {
         case W_IEEE_ULOGIC:
            ft->sdt     = FST_SDT_VHDL_STD_ULOGIC;
            ft->vartype = FST_VT_SV_LOGIC;
            ft->fn      = fst_fmt_chars;
            ft->u.map   = "UX01ZWLH-";
            ft->size    = 1;
            break;

         case W_STD_BIT:
            ft->sdt     = FST_SDT_VHDL_BIT;
            ft->vartype = FST_VT_SV_LOGIC;
            ft->fn      = fst_fmt_chars;
            ft->u.map   = "01";
            ft->size    = 1;
            break;

         case W_STD_CHAR: ft->sdt = FST_SDT_VHDL_CHARACTER; break;
         case W_STD_BOOL: ft->sdt = FST_SDT_VHDL_BOOLEAN; break;
         default:         ft->sdt = FST_SDT_NONE; break;
         }
      }

      if (ft->fn == NULL) {
         ft->vartype = FST_VT_GEN_STRING;
         ft->size    = 0;
         ft->fn      = fst_fmt_enum;

         const int nlits = type_enum_literals(type);
         int maxsize = 0;
         for (int i = 0; i < nlits; i++) {
            ident_t id = tree_ident(type_enum_literal(type, i));
            maxsize = MAX(maxsize, ident_len(id) + 1);
         }

         ft->u.literals.count = nlits;
         ft->u.literals.size  = maxsize;

         ft->u.literals.strings = xmalloc(maxsize * nlits);
         for (int i = 0; i < nlits; i++) {
            char *p = ft->u.literals.strings + i*maxsize;
            istr_r(tree_ident(type_enum_literal(type, i)), p, maxsize);
            for (; *p; p++)
               *p = tolower((int)*p);
         }
      }
      break;

   case T_PHYSICAL:
      ft->sdt     = FST_SDT_NONE;
      ft->vartype = FST_VT_GEN_STRING;
      ft->size    = 0;
      ft->u.units = fst_make_unit_map(type);
      ft->fn      = fst_fmt_physical;
      break;

   case T_ARRAY:
      {
         if (dimension_of(type) > 1) {
            warn_at(loc, "cannot represent multidimensional arrays "
                    "in FST format");
            goto poison;
         }

         type_t elem = type_elem(type);
         fst_type_t *elemft = fst_type_for(elem, loc);
         if (elemft == NULL) {
            warn_at(loc, "cannot represent arrays of array of type %s "
                    "in FST format", type_pp(elem));
            goto poison;
         }

         *ft = *elemft;

         switch (is_well_known(type_ident(type))) {
         case W_IEEE_ULOGIC_VECTOR:
            ft->sdt = FST_SDT_VHDL_STD_ULOGIC_VECTOR;
            break;
         case W_IEEE_LOGIC_VECTOR:
            ft->sdt = FST_SDT_VHDL_STD_LOGIC_VECTOR;
            break;
         case W_IEEE_SIGNED:
            ft->sdt = FST_SDT_VHDL_SIGNED;
            break;
         case W_IEEE_UNSIGNED:
            ft->sdt = FST_SDT_VHDL_UNSIGNED;
            break;
         case W_STD_STRING:
            ft->sdt   = FST_SDT_VHDL_STRING;
            ft->fn    = fst_fmt_chars;
            ft->u.map = NULL;
            ft->size  = 1;
            break;
         default:
            break;
         }
      }
      break;

   default:
      warn_at(loc, "cannot represent type %s in FST format", type_pp(type));
      goto poison;
   }

   hash_put(cache, type, ft);
   return ft;

 poison:
   hash_put(cache, type, (void *)-1);
   free(ft);
   return NULL;
}

static void fst_create_array_var(wave_dumper_t *wd, tree_t d, rt_signal_t *s,
                                 type_t type, tree_t cons, text_buf_t *tb)
{
   tree_t r;
   if (cons != NULL && tree_kind(cons) == T_ELEM_CONSTRAINT)
      r = range_of(tree_type(cons), 0);
   else if (cons != NULL)
      r = tree_range(cons, 0);
   else
      r = range_of(type, 0);

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

   const int msb = assume_int(tree_left(r));
   const int lsb = assume_int(tree_right(r));

   tb_rewind(tb);
   tb_istr(tb, tree_ident(d));
   tb_printf(tb, "[%d:%d]", msb, lsb);
   tb_downcase(tb);

   fst_data_t *data;

   type_t elem = type_elem(type);
   if (type_is_array(elem)) {
      // Dumping memories and nested arrays can be slow
      if (!opt_get_int(OPT_DUMP_ARRAYS))
         return;

      fst_type_t *ft = fst_type_for(elem, tree_loc(d));
      if (ft == NULL)
         return;

      tree_t elem_r = range_of(elem, 0);
      int64_t e_low, e_high;
      range_bounds(elem_r, &e_low, &e_high);

      const int length = MAX(high - low + 1, 0);
      data = xcalloc_flex(sizeof(fst_data_t), length, sizeof(fstHandle));
      data->count = length;
      data->size  = (e_high - e_low + 1) * ft->size;
      data->type  = ft;

      const int msb = assume_int(tree_left(elem_r));
      const int lsb = assume_int(tree_right(elem_r));

      for (int i = 0; i < length; i++) {
         tb_rewind(tb);
         tb_istr(tb, tree_ident(d));
         tb_printf(tb, "[%"PRIi64"][%d:%d]", low + i, msb, lsb);
         tb_downcase(tb);

         data->handle[i] = fstWriterCreateVar2(
            wd->fst_ctx,
            ft->vartype,
            dir,
            data->size,
            tb_get(tb),
            0,
            type_pp(elem),
            FST_SVT_VHDL_SIGNAL,
            ft->sdt);
      }

      fstWriterSetAttrEnd(wd->fst_ctx);
   }
   else {
      fst_type_t *ft = fst_type_for(type, tree_loc(d));
      if (ft == NULL)
         return;

      data = xcalloc(sizeof(fst_data_t) + sizeof(fstHandle));
      data->type  = ft;
      data->size  = (high - low + 1) * ft->size;
      data->count = 1;

      data->handle[0] = fstWriterCreateVar2(
         wd->fst_ctx,
         ft->vartype,
         dir,
         data->size,
         tb_get(tb),
         0,
         type_pp(type),
         FST_SVT_VHDL_SIGNAL,
         ft->sdt);

      if (wd->gtkw != NULL)
         fprintf(wd->gtkw->file, "%s.%s\n", tb_get(wd->gtkw->hier), tb_get(tb));
   }

   data->decl   = d;
   data->signal = s;
   data->dir    = tree_subkind(r);
   data->dumper = wd;
   data->watch  = model_set_event_cb(wd->model, data->signal,
                                     fst_event_cb, data, true);

   fst_event_cb(0, data->signal, data->watch, data);
}

static void fst_create_scalar_var(wave_dumper_t *wd, tree_t d, rt_signal_t *s,
                                  type_t type, text_buf_t *tb)
{
   fst_type_t *ft = fst_type_for(type, tree_loc(d));
   if (ft == NULL)
      return;

   fst_data_t *data = xcalloc(sizeof(fst_data_t) + sizeof(fstHandle));
   data->type   = ft;
   data->count  = 1;
   data->size   = ft->size;
   data->dumper = wd;

   enum fstVarDir dir = FST_VD_IMPLICIT;

   if (tree_kind(d) == T_PORT_DECL) {
      switch (tree_subkind(d)) {
      case PORT_IN:     dir = FST_VD_INPUT; break;
      case PORT_OUT:    dir = FST_VD_OUTPUT; break;
      case PORT_INOUT:  dir = FST_VD_INOUT; break;
      case PORT_BUFFER: dir = FST_VD_BUFFER; break;
      }
   }

   tb_rewind(tb);
   tb_istr(tb, tree_ident(d));
   tb_downcase(tb);

   data->handle[0] = fstWriterCreateVar2(wd->fst_ctx, ft->vartype, dir,
                                         ft->size, tb_get(tb), 0, type_pp(type),
                                         FST_SVT_VHDL_SIGNAL, ft->sdt);

   data->decl   = d;
   data->signal = s;
   data->watch  = model_set_event_cb(wd->model, data->signal, fst_event_cb,
                                     data, true);

   fst_event_cb(0, data->signal, data->watch, data);

   if (wd->gtkw != NULL)
      fprintf(wd->gtkw->file, "%s.%s\n", tb_get(wd->gtkw->hier), tb_get(tb));
}

static void fst_create_record_var(wave_dumper_t *wd, tree_t d,
                                  rt_scope_t *scope, type_t type,
                                  text_buf_t *tb)
{
   tb_rewind(tb);
   tb_istr(tb, tree_ident(d));
   tb_downcase(tb);

   fstWriterSetScope(wd->fst_ctx, FST_ST_VHDL_RECORD, tb_get(tb), NULL);

   size_t hlen = 0;
   if (wd->gtkw != NULL) {
      hlen = tb_len(wd->gtkw->hier);
      tb_printf(wd->gtkw->hier, ".%s", tb_get(tb));
      fprintf(wd->gtkw->file, "-%s\n", tb_get(wd->gtkw->hier));
   }

   const int nfields = type_fields(type);
   for (int i = 0; i < nfields; i++) {
      tree_t f = type_field(type, i);
      tree_t cons = type_constraint_for_field(type, f);
      fst_process_signal(wd, scope, f, cons, tb);
   }

   fstWriterSetUpscope(wd->fst_ctx);

   if (wd->gtkw != NULL)
      tb_trim(wd->gtkw->hier, hlen);
}

static void fst_process_signal(wave_dumper_t *wd, rt_scope_t *scope, tree_t d,
                               tree_t cons, text_buf_t *tb)
{
   type_t type = tree_type(d);
   if (type_is_record(type)) {
      rt_scope_t *sub = child_scope(scope, d);
      if (sub == NULL)
         ;    // Signal was optimised out
      else
         fst_create_record_var(wd, d, sub, type, tb);
   }
   else {
      if (wd->gtkw != NULL)
         fprintf(wd->gtkw->file, "[color] %d\n", wd->gtkw->colour);

      rt_signal_t *s = find_signal(scope, d);
      if (s == NULL)
         ;    // Signal was optimised out
      else if (type_is_array(type))
         fst_create_array_var(wd, d, s, type, cons, tb);
      else
         fst_create_scalar_var(wd, d, s, type, tb);
   }
}

static void fst_process_hier(wave_dumper_t *wd, tree_t h, tree_t block)
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
   fstWriterSetSourceStem(wd->fst_ctx, loc_file_str(loc), loc->first_line, 1);

   LOCAL_TEXT_BUF tb = tb_new();
   tb_istr(tb, tree_ident(block));
   tb_downcase(tb);

   // TODO: store the component name in T_HIER somehow?
   fstWriterSetScope(wd->fst_ctx, st, tb_get(tb), "");

   if (wd->gtkw != NULL) {
      if (tb_len(wd->gtkw->hier) > 0)
         tb_append(wd->gtkw->hier, '.');
      tb_cat(wd->gtkw->hier, tb_get(tb));

      fprintf(wd->gtkw->file, "-%s\n", tb_get(wd->gtkw->hier));
      wd->gtkw->colour = (wd->gtkw->colour % 7) + 1;
   }
}

static void fst_walk_design(wave_dumper_t *wd, tree_t block)
{
   tree_t h = tree_decl(block, 0);
   assert(tree_kind(h) == T_HIER);
   fst_process_hier(wd, h, block);

   ident_t hpath = tree_ident(h);

   rt_scope_t *scope = find_scope(wd->model, block);
   if (scope == NULL)
      fatal_trace("missing scope for %s", istr(hpath));

   LOCAL_TEXT_BUF tb = tb_new();

   const int nports = tree_ports(block);
   for (int i = 0; i < nports; i++) {
      tree_t p = tree_port(block, i);
      ident_t path = ident_prefix(hpath, ident_downcase(tree_ident(p)), ':');
      if (wave_should_dump(path))
         fst_process_signal(wd, scope, p, NULL, tb);
   }

   const int ndecls = tree_decls(block);
   for (int i = 0; i < ndecls; i++) {
      tree_t d = tree_decl(block, i);
      if (tree_kind(d) == T_SIGNAL_DECL) {
         ident_t path = ident_prefix(hpath, ident_downcase(tree_ident(d)), ':');
         if (wave_should_dump(path))
            fst_process_signal(wd, scope, d, NULL, tb);
      }
   }

   const int nstmts = tree_stmts(block);
   for (int i = 0; i < nstmts; i++) {
      tree_t s = tree_stmt(block, i);
      switch (tree_kind(s)) {
      case T_BLOCK:
         fst_walk_design(wd, s);
         break;
      case T_PROCESS:
         break;
      default:
         fatal_trace("cannot handle tree kind %s in fst_walk_design",
                     tree_kind_str(tree_kind(s)));
      }
   }

   fstWriterSetUpscope(wd->fst_ctx);

   if (wd->gtkw != NULL) {
      const char *h = tb_get(wd->gtkw->hier);
      const char *prev = strrchr(h, '.');
      if (prev != NULL)
         tb_trim(wd->gtkw->hier, prev - h);
   }
}

void wave_dumper_restart(wave_dumper_t *wd, rt_model_t *m)
{
   wd->last_time = UINT64_MAX;
   wd->model     = m;

   fst_walk_design(wd, tree_stmt(wd->top, 0));

   if (wd->gtkw != NULL) {
      fclose(wd->gtkw->file);
      tb_free(wd->gtkw->hier);
      free(wd->gtkw);
      wd->gtkw = NULL;
   }

   model_set_global_cb(m, RT_END_OF_SIMULATION, fst_close, wd);
}

wave_dumper_t *wave_dumper_new(const char *file, const char *gtkw_file,
                               tree_t top, wave_format_t format)
{
   wave_dumper_t *wd = xcalloc(sizeof(wave_dumper_t));
   wd->top       = top;
   wd->last_time = UINT64_MAX;

   if (format == WAVE_FORMAT_VCD) {
#if defined __CYGWIN__ || defined __MINGW32__
      const char *tmpdir = ".";
#else
      char tmpdir[PATH_MAX] = "/tmp/vcdXXXXXX";
      checked_sprintf(tmpdir, PATH_MAX, "%s/vcdXXXXXX",
                      getenv("TMPDIR") ?: "/tmp");
      if (mkdtemp(tmpdir) == NULL)
         fatal_errno("mkdtemp");
#endif

      wd->vcdfile = fopen(file, "wb");
      if (wd->vcdfile == NULL)
         fatal_errno("%s", file);

      wd->tmpfst  = xasprintf("%s" DIR_SEP "temp.fst", tmpdir);
      wd->fst_ctx = fstWriterCreate(wd->tmpfst, 1);
   }
   else {
      wd->vcdfile = NULL;
      wd->tmpfst  = NULL;
      wd->fst_ctx = fstWriterCreate(file, 1);
   }

   if (wd->fst_ctx == NULL)
      fatal("fstWriterCreate failed");

   fstWriterSetFileType(wd->fst_ctx, FST_FT_VHDL);
   fstWriterSetTimescale(wd->fst_ctx, -15);
   fstWriterSetVersion(wd->fst_ctx, PACKAGE_STRING);
   fstWriterSetPackType(wd->fst_ctx, 0);
   fstWriterSetRepackOnClose(wd->fst_ctx, 1);
   fstWriterSetParallelMode(wd->fst_ctx, 0);

   if (gtkw_file != NULL) {
      wd->gtkw = xcalloc(sizeof(gtkw_writer_t));
      if ((wd->gtkw->file = fopen(gtkw_file, "w")) == NULL)
         fatal_errno("%s", gtkw_file);

      wd->gtkw->hier   = tb_new();
      wd->gtkw->colour = 1;
   }

   return wd;
}

void wave_dumper_free(wave_dumper_t *wd)
{
   if (wd->model != NULL)
      fatal_trace("wave_dumper_free called before end of simulation");

   free(wd);
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

static bool wave_should_dump(ident_t name)
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
