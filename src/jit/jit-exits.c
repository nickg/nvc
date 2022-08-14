//
//  Copyright (C) 2022  Nick Gasson
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
#include "diag.h"
#include "jit/jit-exits.h"
#include "jit/jit.h"
#include "lib.h"
#include "rt/rt.h"
#include "type.h"

#include <assert.h>
#include <errno.h>
#include <inttypes.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void x_file_open(int8_t *status, void **_fp, uint8_t *name_bytes,
                 int32_t name_len, int8_t mode, tree_t where)
{
   FILE **fp = (FILE **)_fp;

   char *fname LOCAL = xmalloc(name_len + 1);
   memcpy(fname, name_bytes, name_len);
   fname[name_len] = '\0';

   const char *mode_str[] = {
      "rb", "wb", "w+b"
   };
   assert(mode < ARRAY_LEN(mode_str));

   if (status != NULL)
      *status = OPEN_OK;

   if (*fp != NULL) {
      if (status == NULL)
         jit_msg(tree_loc(where), DIAG_FATAL, "file object already associated "
                 "with an external file");
      else
         *status = STATUS_ERROR;
   }
   else if (name_len == 0) {
      if (status == NULL)
         jit_msg(tree_loc(where), DIAG_FATAL, "empty file name in FILE_OPEN");
      else
         *status = NAME_ERROR;
   }
   else if (strcmp(fname, "STD_INPUT") == 0)
      *fp = stdin;
   else if (strcmp(fname, "STD_OUTPUT") == 0)
      *fp = stdout;
   else {
#ifdef __MINGW32__
      const bool failed = (fopen_s(fp, fname, mode_str[mode]) != 0);
#else
      const bool failed = ((*fp = fopen(fname, mode_str[mode])) == NULL);
#endif
      if (failed) {
         if (status == NULL)
            jit_msg(tree_loc(where), DIAG_FATAL, "failed to open %s: %s", fname,
                    strerror(errno));
         else {
            switch (errno) {
            case ENOENT: *status = NAME_ERROR; break;
            case EPERM:  *status = MODE_ERROR; break;
            default:     *status = NAME_ERROR; break;
            }
         }
      }
   }
}

void x_file_write(void **_fp, uint8_t *data, int32_t len)
{
   FILE **fp = (FILE **)_fp;

   if (*fp == NULL)
      jit_msg(NULL, DIAG_FATAL, "write to closed file");

   fwrite(data, 1, len, *fp);
}

void x_file_read(void **_fp, uint8_t *data, int32_t size, int32_t count,
                 int32_t *out)
{
   FILE **fp = (FILE **)_fp;

   if (*fp == NULL)
      jit_msg(NULL, DIAG_FATAL, "read from closed file");

   size_t n = fread(data, size, count, *fp);
   if (out != NULL)
      *out = n;
}

void x_file_close(void **_fp)
{
   FILE **fp = (FILE **)_fp;

   if (*fp != NULL) {
      fclose(*fp);
      *fp = NULL;
   }
}

int8_t x_endfile(void *_f)
{
   FILE *f = _f;

   if (f == NULL)
      jit_msg(NULL, DIAG_FATAL, "ENDFILE called on closed file");

   int c = fgetc(f);
   if (c == EOF)
      return 1;
   else {
      ungetc(c, f);
      return 0;
   }
}

void x_file_flush(void *_f)
{
   if (_f == NULL)
      jit_msg(NULL, DIAG_FATAL, "FLUSH called on closed file");

   fflush(_f);
}

void x_index_fail(int32_t value, int32_t left, int32_t right, int8_t dir,
                  tree_t where, tree_t hint)
{
   type_t type = tree_type(hint);

   LOCAL_TEXT_BUF tb = tb_new();
   tb_cat(tb, "index ");
   to_string(tb, type, value);
   tb_printf(tb, " outside of %s range ", type_pp(type));
   to_string(tb, type, left);
   tb_cat(tb, dir == RANGE_TO ? " to " : " downto ");
   to_string(tb, type, right);

   jit_msg(tree_loc(where), DIAG_FATAL, "%s", tb_get(tb));
}

void x_length_fail(int32_t left, int32_t right, int32_t dim, tree_t where)
{
   const tree_kind_t kind = tree_kind(where);

   LOCAL_TEXT_BUF tb = tb_new();
   if (kind == T_PORT_DECL || kind == T_GENERIC_DECL || kind == T_PARAM_DECL)
      tb_cat(tb, "actual");
   else if (kind == T_CASE || kind == T_MATCH_CASE)
      tb_cat(tb, "expression");
   else if (kind == T_ASSOC)
      tb_cat(tb, "choice");
   else
      tb_cat(tb, "value");
   tb_printf(tb, " length %d", right);
   if (dim > 0)
      tb_printf(tb, " for dimension %d", dim);
   tb_cat(tb, " does not match ");

   switch (kind) {
   case T_PORT_DECL:
      tb_printf(tb, "port %s", istr(tree_ident(where)));
      break;
   case T_PARAM_DECL:
      tb_printf(tb, "parameter %s", istr(tree_ident(where)));
      break;
   case T_GENERIC_DECL:
      tb_printf(tb, "generic %s", istr(tree_ident(where)));
      break;
   case T_VAR_DECL:
      tb_printf(tb, "variable %s", istr(tree_ident(where)));
      break;
   case T_SIGNAL_DECL:
      tb_printf(tb, "signal %s", istr(tree_ident(where)));
      break;
   case T_REF:
      tb_printf(tb, "%s %s", class_str(class_of(where)),
                istr(tree_ident(where)));
      break;
   case T_FIELD_DECL:
      tb_printf(tb, "field %s", istr(tree_ident(where)));
      break;
   case T_ALIAS:
      tb_printf(tb, "alias %s", istr(tree_ident(where)));
      break;
   case T_CASE:
   case T_MATCH_CASE:
      tb_cat(tb, "case choice");
      break;
   case T_ASSOC:
      tb_cat(tb, "expected");
      break;
   default:
      tb_cat(tb, "target");
      break;
   }

   tb_printf(tb, " length %d", left);

   jit_msg(tree_loc(where), DIAG_FATAL, "%s", tb_get(tb));
}

void x_range_fail(int64_t value, int64_t left, int64_t right, int8_t dir,
                  tree_t where, tree_t hint)
{
   type_t type = tree_type(hint);

   LOCAL_TEXT_BUF tb = tb_new();
   tb_cat(tb, "value ");
   to_string(tb, type, value);
   tb_printf(tb, " outside of %s range ", type_pp(type));
   to_string(tb, type, left);
   tb_cat(tb, dir == RANGE_TO ? " to " : " downto ");
   to_string(tb, type, right);

   switch (tree_kind(hint)) {
   case T_SIGNAL_DECL:
   case T_CONST_DECL:
   case T_VAR_DECL:
   case T_REF:
      tb_printf(tb, " for %s %s", class_str(class_of(hint)),
                istr(tree_ident(hint)));
      break;
   case T_PORT_DECL:
      tb_printf(tb, " for port %s", istr(tree_ident(hint)));
      break;
   case T_PARAM_DECL:
      tb_printf(tb, " for parameter %s", istr(tree_ident(hint)));
      break;
   case T_GENERIC_DECL:
      tb_printf(tb, " for generic %s", istr(tree_ident(hint)));
      break;
   case T_ATTR_REF:
      tb_printf(tb, " for attribute '%s", istr(tree_ident(hint)));
      break;
   default:
      break;
   }

   jit_msg(tree_loc(where), DIAG_FATAL, "%s", tb_get(tb));
}

void x_exponent_fail(int32_t value, tree_t where)
{
   jit_msg(tree_loc(where), DIAG_FATAL, "negative exponent %d only "
           "allowed for floating-point types", value);
}

void x_overflow(int64_t lhs, int64_t rhs, tree_t where)
{
   const char *op = "??";
   if (tree_kind(where) == T_FCALL) {
      switch (tree_subkind(tree_ref(where))) {
      case S_ADD: op = "+"; break;
      case S_MUL: op = "*"; break;
      case S_SUB: op = "-"; break;
      }
   }

   jit_msg(tree_loc(where), DIAG_FATAL, "result of %"PRIi64" %s %"PRIi64
           " cannot be represented as %s", lhs, op, rhs,
           type_pp(tree_type(where)));
}

void x_null_deref(tree_t where)
{
   jit_msg(tree_loc(where), DIAG_FATAL, "null access dereference");
}

void x_div_zero(tree_t where)
{
   jit_msg(tree_loc(where), DIAG_FATAL, "division by zero");
}

////////////////////////////////////////////////////////////////////////////////
// Entry points from compiled code

// Helper macro for passing debug loci from LLVM
#define DEBUG_LOCUS(name) \
   const char *name##_unit, uint32_t name##_offset

static inline tree_t locus_to_tree(const char *unit, unsigned offset)
{
   if (unit == NULL)
      return NULL;
   else
      return tree_from_locus(ident_new(unit), offset, lib_get_qualified);
}

DLLEXPORT
void _file_open(int8_t *status, void **_fp, uint8_t *name_bytes,
                int32_t name_len, int8_t mode, DEBUG_LOCUS(locus))
{
   tree_t where = locus_to_tree(locus_unit, locus_offset);

   x_file_open(status, _fp, name_bytes, name_len, mode, where);
}

DLLEXPORT
void _file_write(void **_fp, uint8_t *data, int32_t len)
{
   x_file_write(_fp, data, len);
}

DLLEXPORT
void _file_read(void **_fp, uint8_t *data, int32_t size, int32_t count,
                int32_t *out)
{
   x_file_read(_fp, data, size, count, out);
}

DLLEXPORT
void _file_close(void **_fp)
{
   x_file_close(_fp);
}

DLLEXPORT
int8_t _endfile(void *_f)
{
   return x_endfile(_f);
}

DLLEXPORT
void __nvc_flush(FILE *f)
{
   x_file_flush(f);
}

DLLEXPORT
void __nvc_index_fail(int32_t value, int32_t left, int32_t right, int8_t dir,
                      DEBUG_LOCUS(locus), DEBUG_LOCUS(hint))
{
   tree_t where = locus_to_tree(locus_unit, locus_offset);
   tree_t hint = locus_to_tree(hint_unit, hint_offset);

   x_index_fail(value, left, right, dir, where, hint);
}

DLLEXPORT
void __nvc_length_fail(int32_t left, int32_t right, int32_t dim,
                       DEBUG_LOCUS(locus))
{
   tree_t where = locus_to_tree(locus_unit, locus_offset);

   x_length_fail(left, right, dim, where);
}

DLLEXPORT
void __nvc_range_fail(int64_t value, int64_t left, int64_t right, int8_t dir,
                      DEBUG_LOCUS(locus), DEBUG_LOCUS(hint))
{
   tree_t where = locus_to_tree(locus_unit, locus_offset);
   tree_t hint = locus_to_tree(hint_unit, hint_offset);

   x_range_fail(value, left, right, dir, where, hint);
}

DLLEXPORT
void __nvc_exponent_fail(int32_t value, DEBUG_LOCUS(locus))
{
   tree_t where = locus_to_tree(locus_unit, locus_offset);

   x_exponent_fail(value, where);
}

DLLEXPORT
void __nvc_overflow(int64_t lhs, int64_t rhs, DEBUG_LOCUS(locus))
{
   tree_t where = locus_to_tree(locus_unit, locus_offset);

   x_overflow(lhs, rhs, where);
}

DLLEXPORT
void __nvc_null_deref(DEBUG_LOCUS(locus))
{
   tree_t where = locus_to_tree(locus_unit, locus_offset);

   x_null_deref(where);
}

DLLEXPORT
void __nvc_div_zero(DEBUG_LOCUS(locus))
{
   tree_t where = locus_to_tree(locus_unit, locus_offset);

   x_div_zero(where);
}