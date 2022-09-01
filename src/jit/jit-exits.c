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
#include "rt/ffi.h"
#include "rt/rt.h"
#include "type.h"

#include <assert.h>
#include <ctype.h>
#include <errno.h>
#include <float.h>
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

int64_t x_string_to_int(const uint8_t *raw_str, int32_t str_len, int32_t *used)
{
   const char *p = (const char *)raw_str;
   const char *endp = p + str_len;

   for (; p < endp && isspace((int)*p); p++)
      ;

   const bool is_negative = p < endp && *p == '-';
   if (is_negative) p++;

   int64_t value = INT64_MIN;
   int num_digits = 0;
   while (p < endp && (isdigit((int)*p) || *p == '_')) {
      if (*p != '_') {
         value *= 10;
         value += (*p - '0');
         num_digits++;
      }
      ++p;
   }

   if (is_negative) value = -value;

   if (num_digits == 0)
      jit_msg(NULL, DIAG_FATAL, "invalid integer value "
              "\"%.*s\"", str_len, (const char *)raw_str);

   if (used != NULL)
      *used = p - (const char *)raw_str;
   else {
      for (; p < endp && *p != '\0'; p++) {
         if (!isspace((int)*p)) {
            jit_msg(NULL, DIAG_FATAL, "found invalid characters \"%.*s\" after "
                    "value \"%.*s\"", (int)(endp - p), p, str_len,
                    (const char *)raw_str);
         }
      }
   }

   return value;
}

double x_string_to_real(const uint8_t *raw_str, int32_t str_len)
{
   char *null LOCAL = xmalloc(str_len + 1);
   memcpy(null, raw_str, str_len);
   null[str_len] = '\0';

   char *p = null;
   for (; p < p + str_len && isspace((int)*p); p++)
      ;

   double value = strtod(p, &p);

   if (*p != '\0' && !isspace((int)*p))
      jit_msg(NULL, DIAG_FATAL, "invalid real value "
              "\"%.*s\"", str_len, (const char *)raw_str);
   else {
      for (; p < null + str_len && *p != '\0'; p++) {
         if (!isspace((int)*p)) {
            jit_msg(NULL, DIAG_FATAL, "found invalid characters \"%.*s\" after "
                    "value \"%.*s\"", (int)(null + str_len - p), p, str_len,
                    (const char *)raw_str);
         }
      }
   }

   return value;
}

ffi_uarray_t x_canon_value(const uint8_t *raw_str, int32_t str_len, char *buf)
{
   char *p = buf;
   int pos = 0;

   for (; pos < str_len && isspace((int)raw_str[pos]); pos++)
      ;

   bool upcase = true;
   for (; pos < str_len && !isspace((int)raw_str[pos]); pos++) {
      if (raw_str[pos] == '\'')
         upcase = !upcase;

      *p++ = upcase ? toupper((int)raw_str[pos]) : raw_str[pos];
   }

   for (; pos < str_len; pos++) {
      if (!isspace((int)raw_str[pos])) {
         jit_msg(NULL, DIAG_FATAL, "found invalid characters \"%.*s\" after "
                 "value \"%.*s\"", (int)(str_len - pos), raw_str + pos, str_len,
                 (const char *)raw_str);
      }
   }

   return ffi_wrap_str(buf, p - buf);
}

ffi_uarray_t x_int_to_string(int64_t value, char *buf, size_t max)
{
   size_t len = checked_sprintf(buf, max, "%"PRIi64, value);
   return ffi_wrap_str(buf, len);
}

ffi_uarray_t x_real_to_string(double value, char *buf, size_t max)
{
   size_t len = checked_sprintf(buf, max, "%.*g", DBL_DIG, value);
   return ffi_wrap_str(buf, len);
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

DLLEXPORT
int64_t _string_to_int(const uint8_t *raw_str, int32_t str_len, int32_t *used)
{
   return x_string_to_int(raw_str, str_len, used);
}

DLLEXPORT
double _string_to_real(const uint8_t *raw_str, int32_t str_len)
{
   return x_string_to_real(raw_str, str_len);
}

DLLEXPORT
void _canon_value(const uint8_t *raw_str, int32_t str_len, ffi_uarray_t *u)
{
   char *buf = rt_tlab_alloc(str_len);
   *u = x_canon_value(raw_str, str_len, buf);
}

DLLEXPORT
void _int_to_string(int64_t value, ffi_uarray_t *u)
{
   char *buf = rt_tlab_alloc(20);
   *u = x_int_to_string(value, buf, 20);
}

DLLEXPORT
void _real_to_string(double value, ffi_uarray_t *u)
{
   char *buf = rt_tlab_alloc(32);
   *u = x_real_to_string(value, buf, 32);
}

DLLEXPORT
sig_shared_t *_init_signal(uint32_t count, uint32_t size, const uint8_t *values,
                           int32_t flags, DEBUG_LOCUS(locus), int32_t offset)
{
   tree_t where = locus_to_tree(locus_unit, locus_offset);
   return x_init_signal(count, size, values, flags, where, offset);
}

DLLEXPORT
void __nvc_drive_signal(sig_shared_t *ss, uint32_t offset, int32_t count)
{
   x_drive_signal(ss, offset, count);
}

DLLEXPORT
void _sched_process(int64_t delay)
{
   x_sched_process(delay);
}

DLLEXPORT
void _sched_waveform_s(sig_shared_t *ss, uint32_t offset, uint64_t scalar,
                       int64_t after, int64_t reject)
{
   x_sched_waveform_s(ss, offset, scalar, after, reject);
}

DLLEXPORT
void _sched_waveform(sig_shared_t *ss, uint32_t offset, void *values,
                     int32_t count, int64_t after, int64_t reject)
{
   x_sched_waveform(ss, offset, values, count, after, reject);
}

DLLEXPORT
void _sched_event(sig_shared_t *ss, uint32_t offset, int32_t count, bool recur,
                  sig_shared_t *wake_ss)
{
   x_sched_event(ss, offset, count, recur, wake_ss);
}

DLLEXPORT
void __nvc_alias_signal(sig_shared_t *ss, DEBUG_LOCUS(locus))
{
   tree_t where = locus_to_tree(locus_unit, locus_offset);
   x_alias_signal(ss, where);
}

DLLEXPORT
void __nvc_assert_fail(const uint8_t *msg, int32_t msg_len, int8_t severity,
                       int64_t hint_left, int64_t hint_right, int8_t hint_valid,
                       DEBUG_LOCUS(locus))
{
   tree_t where = locus_to_tree(locus_unit, locus_offset);

   x_assert_fail(msg, msg_len, severity, hint_left, hint_right,
                 hint_valid, where);
}

DLLEXPORT
void __nvc_report(const uint8_t *msg, int32_t msg_len, int8_t severity,
                  DEBUG_LOCUS(locus))
{
   tree_t where = locus_to_tree(locus_unit, locus_offset);

   x_report(msg, msg_len, severity, where);
}

DLLEXPORT
void __nvc_claim_tlab(void)
{
   x_claim_tlab();
}

DLLEXPORT
int64_t _last_event(sig_shared_t *ss, uint32_t offset, int32_t count)
{
   return x_last_event(ss, offset, count);
}

DLLEXPORT
int64_t _last_active(sig_shared_t *ss, uint32_t offset, int32_t count)
{
   return x_last_active(ss, offset, count);
}

DLLEXPORT
int32_t _test_net_active(sig_shared_t *ss, uint32_t offset, int32_t count)
{
   return x_test_net_active(ss, offset, count);
}

DLLEXPORT
int32_t _test_net_event(sig_shared_t *ss, uint32_t offset, int32_t count)
{
   return x_test_net_event(ss, offset, count);
}

DLLEXPORT
void _debug_out(intptr_t val, int32_t reg)
{
   printf("DEBUG: r%d val=%"PRIxPTR"\n", reg, val);
   fflush(stdout);
}

DLLEXPORT
void _debug_dump(const uint8_t *ptr, int32_t len)
{
   printf("---- %p ----\n", ptr);

   if (ptr != NULL) {
      for (int i = 0; i < len; i++)
         printf("%02x%c", ptr[i], (i % 8 == 7) ? '\n' : ' ');
      if (len % 8 != 0)
         printf("\n");
   }

   fflush(stdout);
}

DLLEXPORT
void __nvc_map_signal(sig_shared_t *src_ss, uint32_t src_offset,
                      sig_shared_t *dst_ss, uint32_t dst_offset,
                      uint32_t src_count, uint32_t dst_count,
                      ffi_closure_t *closure)
{
   x_map_signal(src_ss, src_offset, dst_ss, dst_offset, src_count,
                dst_count, closure);
}

DLLEXPORT
void __nvc_map_const(sig_shared_t *ss, uint32_t offset,
                     const uint8_t *values, uint32_t count)
{
   x_map_const(ss, offset, values, count);
}

DLLEXPORT
void __nvc_push_scope(DEBUG_LOCUS(locus), int32_t size)
{
   tree_t where = locus_to_tree(locus_unit, locus_offset);
   x_push_scope(where, size);
}

DLLEXPORT
void __nvc_pop_scope(void)
{
   x_pop_scope();
}

DLLEXPORT
bool _driving(sig_shared_t *ss, uint32_t offset, int32_t count)
{
   return x_driving(ss, offset, count);
}

DLLEXPORT
void *_driving_value(sig_shared_t *ss, uint32_t offset, int32_t count)
{
   return x_driving_value(ss, offset, count);
}

DLLEXPORT
sig_shared_t *_implicit_signal(uint32_t count, uint32_t size,
                               DEBUG_LOCUS(locus), uint32_t kind,
                               ffi_closure_t *closure)
{
   tree_t where = locus_to_tree(locus_unit, locus_offset);
   return x_implicit_signal(count, size, where, kind, closure);
}

DLLEXPORT
void _disconnect(sig_shared_t *ss, uint32_t offset, int32_t count,
                 int64_t after, int64_t reject)
{
   x_disconnect(ss, offset, count, after, reject);
}

DLLEXPORT
void __nvc_force(sig_shared_t *ss, uint32_t offset, int32_t count, void *values)
{
   x_force(ss, offset, count, values);
}

DLLEXPORT
void __nvc_release(sig_shared_t *ss, uint32_t offset, int32_t count)
{
   x_release(ss, offset, count);
}

DLLEXPORT
void __nvc_resolve_signal(sig_shared_t *ss, rt_resolution_t *resolution)
{
   x_resolve_signal(ss, resolution);
}

DLLEXPORT
void __nvc_elab_order_fail(DEBUG_LOCUS(locus))
{
   tree_t where = locus_to_tree(locus_unit, locus_offset);
   x_elab_order_fail(where);
}

DLLEXPORT
void __nvc_unreachable(DEBUG_LOCUS(locus))
{
   tree_t where = locus_to_tree(locus_unit, locus_offset);
   x_unreachable(where);
}

DLLEXPORT
void *__nvc_mspace_alloc(uint32_t size, uint32_t nelems)
{
   return x_mspace_alloc(size, nelems);
}
