//
//  Copyright (C) 2013-2024  Nick Gasson
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
#include "jit/jit-layout.h"
#include "option.h"
#include "rt/model.h"
#include "rt/rt.h"
#include "rt/structs.h"
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

#define USE_FST_ENUMS 0

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
      const char    *map;
      fst_unit_t    *units;
      fst_enum_t     literals;
      fstEnumHandle  enumh;
   } u;
} fst_type_t;

typedef struct _fst_data {
   wave_dumper_t *dumper;
   fst_type_t    *type;
   rt_watch_t    *watch;
   tree_t         decl;
   rt_signal_t   *signal;
   unsigned       size;
   unsigned       count;
   fstHandle      handle[];
} fst_data_t;

typedef struct {
   FILE       *file;
   int         colour;
   text_buf_t *hier;
   bool        end_of_record;
} gtkw_writer_t;

typedef struct _wave_dumper {
   tree_t         top;
   void          *fst_ctx;
   rt_model_t    *model;
   gtkw_writer_t *gtkw;
   FILE          *vcdfile;
   char          *tmpfst;
   uint64_t       last_time;
   jit_t         *jit;
   hash_t        *typecache;
} wave_dumper_t;

static glob_array_t incl;
static glob_array_t excl;

static void fst_process_signal(wave_dumper_t *wd, rt_scope_t *scope, tree_t d,
                               type_t type, text_buf_t *tb);
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

static inline void fst_write_binary(uint64_t val, size_t size, char *buf)
{
   for (size_t j = 0; j < size; j++)
      buf[size - 1 - j] = (val & (UINT64_C(1) << j)) ? '1' : '0';
   buf[size] = '\0';
}

static void fst_fmt_int(rt_watch_t *w, fst_data_t *data)
{
   uint64_t val[data->count];
   signal_expand(data->signal, val, data->count);

   for (int i = 0; i < data->count; i++) {
      char buf[data->type->size + 1];
      fst_write_binary(val[i], data->type->size, buf);

      fstWriterEmitValueChange(data->dumper->fst_ctx, data->handle[i], buf);
   }
}

static void fst_fmt_real(rt_watch_t *w, fst_data_t *data)
{
   const void *buf = signal_value(data->signal);
   fstWriterEmitValueChange(data->dumper->fst_ctx, data->handle[0], buf);
}

static void fst_fmt_physical(rt_watch_t *w, fst_data_t *data)
{
   uint64_t val;
   signal_expand(data->signal, &val, 1);

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

#if !USE_FST_ENUMS
static void fst_fmt_enum(rt_watch_t *w, fst_data_t *data)
{
   uint64_t val;
   signal_expand(data->signal, &val, 1);

   fst_enum_t *e = &(data->type->u.literals);
   assert(val < e->count);

   const char *literal = e->strings + val * e->size;
   fstWriterEmitVariableLengthValueChange(data->dumper->fst_ctx,
                                          data->handle[0],
                                          literal,
                                          strnlen(literal, e->size));
}
#endif

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

static fst_type_t *fst_type_for(wave_dumper_t *wd, type_t type,
                                const loc_t *loc)
{
   if (is_anonymous_subtype(type)) {
      // Do not cache anonymous subtypes
      return fst_type_for(wd, type_base(type), loc);
   }

   fst_type_t *ft = hash_get(wd->typecache, type);
   if (ft == (void *)-1)
      return NULL;   // Failed for this type earlier
   else if (ft != NULL)
      return ft;

   ft = xcalloc(sizeof(fst_type_t));

   switch (type_kind(type)) {
   case T_SUBTYPE:
      {
         fst_type_t *baseft = fst_type_for(wd, type_base(type), loc);
         if (baseft == NULL)
            goto poison;

         *ft = *baseft;

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
         const int nlits = type_enum_literals(type);
         int maxsize = 0;
         for (int i = 0; i < nlits; i++) {
            ident_t id = tree_ident(type_enum_literal(type, i));
            maxsize = MAX(maxsize, ident_len(id) + 1);
         }

#if USE_FST_ENUMS
         const int nbits = bits_for_range(0, nlits - 1);
         char *lit_mem LOCAL = xmalloc((nbits + 1) * nlits);
         char *name_mem LOCAL = xmalloc_array(maxsize, nlits);
         const char **lits LOCAL = xmalloc_array(nlits, sizeof(char *));
         const char **names LOCAL = xmalloc_array(nlits, sizeof(char *));

         char *namep = name_mem;
         for (int i = 0; i < nlits; i++) {
            ident_t id = tree_ident(type_enum_literal(type, i));
            size_t len = ident_len(id) + 1;
            memcpy(namep, istr(id), len);
            names[i] = namep;
            namep += len;

            char *litp = lit_mem + i * (nbits + 1);
            fst_write_binary(i, nbits, litp);
            lits[i] = litp;
         }

         ft->vartype = FST_VT_SV_ENUM;
         ft->size    = nbits;
         ft->fn      = fst_fmt_int;

         ft->u.enumh = fstWriterCreateEnumTable(wd->fst_ctx, type_pp(type),
                                                nlits, nbits, names, lits);
#else
         ft->vartype = FST_VT_GEN_STRING;
         ft->size    = 0;
         ft->fn      = fst_fmt_enum;

         ft->u.literals.count = nlits;
         ft->u.literals.size  = maxsize;

         ft->u.literals.strings = xmalloc(maxsize * nlits);
         for (int i = 0; i < nlits; i++) {
            char *p = ft->u.literals.strings + i*maxsize;
            strncpy(p, istr(tree_ident(type_enum_literal(type, i))), maxsize);
            for (; *p; p++)
               *p = tolower_iso88591(*p);
         }
#endif
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
         fst_type_t *elemft = fst_type_for(wd, elem, loc);
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
            ft->sdt     = FST_SDT_VHDL_STRING;
            ft->vartype = FST_VT_GEN_STRING;
            ft->fn      = fst_fmt_chars;
            ft->u.map   = NULL;
            ft->size    = 1;
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

   hash_put(wd->typecache, type, ft);
   return ft;

 poison:
   hash_put(wd->typecache, type, (void *)-1);
   free(ft);
   return NULL;
}

static void *fst_get_ptr(wave_dumper_t *wd, rt_scope_t *scope, tree_t where)
{
   if (tree_kind(where) == T_FIELD_DECL) {
      assert(scope->kind == SCOPE_SIGNAL);

      type_t rtype = tree_type(scope->where);
      const jit_layout_t *l = signal_layout_of(rtype);

      if (type_is_array(rtype))
         return fst_get_ptr(wd, scope->parent, scope->where);
      else {
         assert(type_is_record(rtype));
         assert(type_field(rtype, tree_pos(where)) == where);
         assert(l->nparts == type_fields(rtype));

         const ptrdiff_t offset = l->parts[tree_pos(where)].offset;
         return fst_get_ptr(wd, scope->parent, scope->where) + offset;
      }
   }
   else {
      assert(scope->kind == SCOPE_INSTANCE);
      jit_handle_t handle = jit_lazy_compile(wd->jit, scope->name);
      return jit_get_frame_var(wd->jit, handle, tree_ident(where));
   }
}

static void fst_get_array_range(wave_dumper_t *wd, type_t type,
                                rt_scope_t *scope, tree_t where, int dim,
                                int64_t *msb, int64_t *lsb, int64_t *length)
{
   if (!type_is_unconstrained(type)) {
      tree_t r = range_of(type, 0);
      if (folded_length(r, length)) {
         *msb = assume_int(tree_left(r));
         *lsb = assume_int(tree_right(r));
         return;
      }
   }

   ffi_dim_t *dims = fst_get_ptr(wd, scope, where) + 2*sizeof(int64_t);

   *lsb    = ffi_array_right(dims[dim].left, dims[dim].length);
   *msb    = dims[dim].left;
   *length = ffi_array_length(dims[dim].length);
}

static fstHandle fst_create_handle(wave_dumper_t *wd, fst_data_t *data,
                                   const char *name, enum fstVarDir dir,
                                   type_t type, fstHandle alias)
{
   if (data->type->vartype == FST_VT_SV_ENUM)
      fstWriterEmitEnumTableRef(wd->fst_ctx, data->type->u.enumh);

   return fstWriterCreateVar2(
      wd->fst_ctx,
      data->type->vartype,
      dir,
      data->size,
      name,
      alias,
      type_pp(type),
      FST_SVT_VHDL_SIGNAL,
      data->type->sdt);
}

static void fst_create_array_var(wave_dumper_t *wd, tree_t d, rt_signal_t *s,
                                 type_t type, text_buf_t *tb)
{
   enum fstVarDir dir = FST_VD_IMPLICIT;
   if (tree_kind(d) == T_PORT_DECL) {
      switch (tree_subkind(d)) {
      case PORT_IN:     dir = FST_VD_INPUT; break;
      case PORT_OUT:    dir = FST_VD_OUTPUT; break;
      case PORT_INOUT:  dir = FST_VD_INOUT; break;
      case PORT_BUFFER: dir = FST_VD_BUFFER; break;
      }
   }

   int64_t lsb, msb, length;
   fst_get_array_range(wd, type, s->parent, s->where, 0, &msb, &lsb, &length);

   tb_rewind(tb);
   tb_istr(tb, tree_ident(d));
   tb_printf(tb, "[%"PRIi64":%"PRIi64"]", msb, lsb);
   tb_downcase(tb);

   fst_data_t *data;

   type_t elem = type_elem(type);
   if (type_is_enum(elem)) {
      fst_type_t *ft = fst_type_for(wd, type, tree_loc(d));
      if (ft == NULL)
         return;

      data = xcalloc(sizeof(fst_data_t) + sizeof(fstHandle));
      data->type  = ft;
      data->size  = length * ft->size;
      data->count = 1;

      data->handle[0] = fst_create_handle(wd, data, tb_get(tb), dir, type, 0);

      if (wd->gtkw != NULL)
         fprintf(wd->gtkw->file, "%s.%s\n", tb_get(wd->gtkw->hier), tb_get(tb));
   }
   else if (!opt_get_int(OPT_DUMP_ARRAYS))
      return;   // Dumping memories and nested arrays can be slow
   else if (type_is_record(elem))
      return;   // Not yet supported
   else {
      fst_type_t *ft = fst_type_for(wd, elem, tree_loc(d));
      if (ft == NULL)
         return;

      const bool is_memory = type_is_array(elem);

      int64_t e_low = 1, e_high = 1;
      int e_msb = 0, e_lsb = 0;
      if (is_memory) {
         tree_t elem_r = range_of(elem, 0);
         range_bounds(elem_r, &e_low, &e_high);

         e_msb = assume_int(tree_left(elem_r));
         e_lsb = assume_int(tree_right(elem_r));
      }

      data = xcalloc_flex(sizeof(fst_data_t), length, sizeof(fstHandle));
      data->count = length;
      data->size  = (e_high - e_low + 1) * ft->size;
      data->type  = ft;

      for (int i = 0; i < length; i++) {
         tb_rewind(tb);
         tb_istr(tb, tree_ident(d));
         tb_printf(tb, "[%"PRIi64"]", MIN(msb, lsb) + i);
         if (is_memory)
            tb_printf(tb, "[%d:%d]", e_msb, e_lsb);
         tb_downcase(tb);

         data->handle[i] =
            fst_create_handle(wd, data, tb_get(tb), dir, elem, 0);
      }

      fstWriterSetAttrEnd(wd->fst_ctx);
   }

   assert(find_watch(&(s->nexus), fst_event_cb) == NULL);

   data->decl   = d;
   data->signal = s;
   data->dumper = wd;
   data->watch  = model_set_event_cb(wd->model, data->signal,
                                     fst_event_cb, data, true);

   fst_event_cb(0, data->signal, data->watch, data);
}

static void fst_create_scalar_var(wave_dumper_t *wd, tree_t d, rt_signal_t *s,
                                  type_t type, text_buf_t *tb)
{
   fst_type_t *ft = fst_type_for(wd, type, tree_loc(d));
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

   data->handle[0] = fst_create_handle(wd, data, tb_get(tb), dir, type, 0);

   assert(find_watch(&(s->nexus), fst_event_cb) == NULL);

   data->decl   = d;
   data->signal = s;
   data->watch  = model_set_event_cb(wd->model, data->signal, fst_event_cb,
                                     data, true);

   fst_event_cb(0, data->signal, data->watch, data);

   if (wd->gtkw != NULL)
      fprintf(wd->gtkw->file, "%s.%s\n", tb_get(wd->gtkw->hier), tb_get(tb));
}

static void gtkw_print_scope_comment(gtkw_writer_t *gtkw, rt_scope_t *scope,
                                     rt_scope_kind_t kind, text_buf_t *tb,
                                     bool leaf)
{
   if (scope->parent != NULL && scope->parent->kind == kind) {
      gtkw_print_scope_comment(gtkw, scope->parent, kind, tb, false);
      fputc(kind == SCOPE_INSTANCE ? '/' : '.', gtkw->file);
   }
   else
      fputc('-', gtkw->file);

   if (scope->kind == SCOPE_INSTANCE && scope->parent->kind == SCOPE_ROOT) {
      // Do not emit the root scope label
   }
   else if (scope->kind == kind) {
      tb_rewind(tb);
      tb_istr(tb, tree_ident(scope->where));
      tb_downcase(tb);
      fputs(tb_get(tb), gtkw->file);
   }

   if (leaf)
      fprintf(gtkw->file, "%c\n", kind == SCOPE_SIGNAL ? ':' : '/');

   gtkw->end_of_record = false;
}

static void fst_create_record_var(wave_dumper_t *wd, tree_t d,
                                  rt_scope_t *scope, type_t type,
                                  const char *suffix, text_buf_t *tb)
{
   tb_rewind(tb);
   tb_istr(tb, tree_ident(d));
   tb_cat(tb, suffix);
   tb_downcase(tb);

   fstWriterSetScope(wd->fst_ctx, FST_ST_VHDL_RECORD, tb_get(tb), NULL);

   size_t hlen = 0;
   if (wd->gtkw != NULL) {
      hlen = tb_len(wd->gtkw->hier);
      tb_printf(wd->gtkw->hier, ".%s", tb_get(tb));
      gtkw_print_scope_comment(wd->gtkw, scope, SCOPE_SIGNAL, tb, true);
   }

   const int nfields = type_fields(type);
   for (int i = 0; i < nfields; i++) {
      tree_t f = type_field(type, i);
      tree_t cons = type_constraint_for_field(type, f);
      fst_process_signal(wd, scope, f, tree_type(cons ?: f), tb);
   }

   fstWriterSetUpscope(wd->fst_ctx);

   if (wd->gtkw != NULL) {
      tb_trim(wd->gtkw->hier, hlen);
      wd->gtkw->end_of_record = true;
   }
}

static void fst_create_record_array_var(wave_dumper_t *wd, tree_t d,
                                        rt_scope_t *scope, type_t type,
                                        int dim, int start, int count,
                                        const char *prefix, text_buf_t *tb)
{
   if (dimension_of(type) > 1)
      return;   // Not supported

   int64_t lsb, msb, length;
   fst_get_array_range(wd, type, scope->parent, d, dim, &msb, &lsb, &length);

   type_t elem = type_elem(type);
   const bool nested = type_is_array(elem);

   assert(count % length == 0);
   const int stride = count / length;

   for (int i = start, index = MIN(msb, lsb); i < start + count; i += stride) {
      rt_scope_t *sub = AGET(scope->children, i);

      char suffix[64];
      checked_sprintf(suffix, sizeof(suffix), "%s[%d]", prefix, index++);

      if (nested)
         fst_create_record_array_var(wd, d, scope, elem, dim + 1, i,
                                     stride, suffix, tb);
      else
         fst_create_record_var(wd, d, sub, elem, suffix, tb);
   }
}

static void fst_alias_var(wave_dumper_t *wd, tree_t d, rt_signal_t *s,
                          text_buf_t *tb)
{
   rt_watch_t *w = find_watch(&(s->nexus), fst_event_cb);
   if (w == NULL)
      return;   // Did not dump the primary signal

   fst_data_t *data = w->user_data;
   if (data->count != 1)
      return;   // Cannot handle for now

   type_t type = tree_type(d);

   tb_rewind(tb);
   tb_istr(tb, tree_ident(d));
   if (type_is_array(type)) {
      int64_t lsb, msb, length;
      fst_get_array_range(wd, type, s->parent, s->where, 0,
                          &msb, &lsb, &length);

      tb_printf(tb, "[%"PRIi64":%"PRIi64"]", msb, lsb);
   }
   tb_downcase(tb);

   fst_create_handle(wd, data, tb_get(tb), FST_VD_INPUT, type, data->handle[0]);

   if (wd->gtkw != NULL)
      fprintf(wd->gtkw->file, "%s.%s\n", tb_get(wd->gtkw->hier), tb_get(tb));
}

static void fst_process_signal(wave_dumper_t *wd, rt_scope_t *scope, tree_t d,
                               type_t type, text_buf_t *tb)
{
   if (type_is_homogeneous(type)) {
      if (wd->gtkw != NULL) {
         if (wd->gtkw->end_of_record) {
            fputs("-\n", wd->gtkw->file);  // Blank line after record
            wd->gtkw->end_of_record = false;
         }
         fprintf(wd->gtkw->file, "[color] %d\n", wd->gtkw->colour);
      }

      rt_signal_t *s = find_signal(scope, d);
      if (s == NULL)
         return;
      else if (s->where != d)
         fst_alias_var(wd, d, s, tb);  // Collapsed with another signal
      else if (type_is_array(type))
         fst_create_array_var(wd, d, s, type, tb);
      else
         fst_create_scalar_var(wd, d, s, type, tb);
   }
   else if (type_is_record(type)) {
      rt_scope_t *sub = child_scope(scope, d);
      if (sub != NULL)   // NULL means signal was optimised out
         fst_create_record_var(wd, d, sub, type, "", tb);
   }
   else if (opt_get_int(OPT_DUMP_ARRAYS)) {
      rt_scope_t *sub = child_scope(scope, d);
      if (sub != NULL)   // NULL means signal was optimised out
         fst_create_record_array_var(wd, d, sub, type, 0, 0,
                                     sub->children.count, "", tb);
   }
}

static void fst_enter_scope(wave_dumper_t *wd, tree_t unit, rt_scope_t *scope,
                            text_buf_t *tb)
{
   enum fstScopeType st;
   switch (tree_kind(unit)) {
   case T_ARCH: st = FST_ST_VHDL_ARCHITECTURE; break;
   case T_BLOCK: st = FST_ST_VHDL_BLOCK; break;
   case T_FOR_GENERATE: st = FST_ST_VHDL_FOR_GENERATE; break;
   case T_IF_GENERATE: st = FST_ST_VHDL_IF_GENERATE; break;
   case T_PACKAGE: st = FST_ST_VHDL_PACKAGE; break;
   case T_COMPONENT: st = FST_ST_VHDL_ARCHITECTURE; break;
   case T_VERILOG: st = FST_ST_VCD_MODULE; break;
   default:
      st = FST_ST_VHDL_ARCHITECTURE;
      warn_at(tree_loc(unit), "no FST scope type for %s",
              tree_kind_str(tree_kind(unit)));
      break;
   }

   const loc_t *loc = tree_loc(unit);
   fstWriterSetSourceStem(wd->fst_ctx, loc_file_str(loc), loc->first_line, 1);

   tb_rewind(tb);
   tb_istr(tb, tree_ident(scope->where));
   tb_downcase(tb);

   // TODO: store the component name in T_HIER somehow?
   fstWriterSetScope(wd->fst_ctx, st, tb_get(tb), "");

   if (wd->gtkw != NULL) {
      if (scope->kind == SCOPE_INSTANCE && tb_len(wd->gtkw->hier) > 0)
         tb_append(wd->gtkw->hier, '.');
      tb_cat(wd->gtkw->hier, tb_get(tb));

      gtkw_print_scope_comment(wd->gtkw, scope, scope->kind, tb, true);

      wd->gtkw->colour = (wd->gtkw->colour % 7) + 1;
   }
}

static void fst_leave_scope(wave_dumper_t *wd)
{
   fstWriterSetUpscope(wd->fst_ctx);

   if (wd->gtkw != NULL) {
      const char *h = tb_get(wd->gtkw->hier);
      const char *prev = strrchr(h, '.');
      if (prev != NULL)
         tb_trim(wd->gtkw->hier, prev - h);
   }
}

static void fst_walk_design(wave_dumper_t *wd, tree_t block)
{
   tree_t h = tree_decl(block, 0);
   assert(tree_kind(h) == T_HIER);

   ident_t hinst = tree_ident(h);

   LOCAL_TEXT_BUF tb = tb_new();
   instance_name_to_path(tb, istr(hinst));

   ident_t hpath = ident_new(tb_get(tb));

   rt_scope_t *scope = find_scope(wd->model, block);
   if (scope == NULL)
      fatal_trace("missing scope for %s", istr(hinst));

   fst_enter_scope(wd, tree_ref(h), scope, tb);

   if (tree_subkind(h) == T_COMPONENT && tree_stmts(block) > 0) {
      // Skip over implicit block statement created for component
      // instantiation
      block = tree_stmt(block, 0);
      assert(tree_kind(block) == T_BLOCK);

      assert(scope->children.count == 1);
      scope = scope->children.items[0];
   }

   const int nports = tree_ports(block);
   for (int i = 0; i < nports; i++) {
      tree_t p = tree_port(block, i);
      ident_t path = ident_prefix(hpath, ident_downcase(tree_ident(p)), ':');
      if (wave_should_dump(path))
         fst_process_signal(wd, scope, p, tree_type(p), tb);
   }

   const int ndecls = tree_decls(block);
   for (int i = 0; i < ndecls; i++) {
      tree_t d = tree_decl(block, i);
      if (tree_kind(d) == T_SIGNAL_DECL) {
         ident_t path = ident_prefix(hpath, ident_downcase(tree_ident(d)), ':');
         if (wave_should_dump(path))
            fst_process_signal(wd, scope, d, tree_type(d), tb);
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
      case T_VERILOG:
         break;
      case T_PSL:
         break;   // TODO: consider emitting to FST
      default:
         fatal_trace("cannot handle tree kind %s in fst_walk_design",
                     tree_kind_str(tree_kind(s)));
      }
   }

   fst_leave_scope(wd);
}

static void fst_walk_packages(wave_dumper_t *wd)
{
   rt_scope_t *root = root_scope(wd->model);
   assert(root != NULL);
   assert(root->kind == SCOPE_ROOT);

   if (wd->gtkw != NULL)
      tb_rewind(wd->gtkw->hier);

   LOCAL_TEXT_BUF tb = tb_new();

   for (int i = 0; i < root->children.count; i++) {
      rt_scope_t *s = root->children.items[i];
      if (s->kind != SCOPE_PACKAGE)
         continue;

      fst_enter_scope(wd, s->where, s, tb);

      const int ndecls = tree_decls(s->where);
      for (int j = 0; j < ndecls; j++) {
         tree_t d = tree_decl(s->where, j);
         if (tree_kind(d) == T_SIGNAL_DECL)
            fst_process_signal(wd, s, d, tree_type(d), tb);
      }

      fst_leave_scope(wd);
   }
}

void wave_dumper_restart(wave_dumper_t *wd, rt_model_t *m, jit_t *jit)
{
   wd->last_time = UINT64_MAX;
   wd->model     = m;
   wd->jit       = jit;

   fst_walk_design(wd, tree_stmt(wd->top, 0));
   fst_walk_packages(wd);

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
   wd->typecache = hash_new(128);

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
      wd->gtkw->colour = 3;
   }

   return wd;
}

void wave_dumper_free(wave_dumper_t *wd)
{
   hash_free(wd->typecache);
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

   char line[1024];
   while (!feof(f) && (fgets(line, sizeof(line), f) != NULL)) {
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
