//
//  Copyright (C) 2013-2016  Nick Gasson
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

#define COMMON_IMPL

#include "util.h"
#include "common.h"
#include "phase.h"

#include <assert.h>
#include <ctype.h>
#include <string.h>
#include <stdlib.h>
#include <inttypes.h>

static vhdl_standard_t current_std = STD_93;

int64_t assume_int(tree_t t)
{
   switch (tree_kind(t)) {
   case T_LITERAL:
      assert(tree_subkind(t) == L_INT);
      return tree_ival(t);

   case T_REF:
      {
         tree_t ref = tree_ref(t);
         assert(tree_kind(ref) == T_ENUM_LIT);
         return tree_pos(ref);
      }

   case T_FCALL:
      {
         const eval_flags_t flags =
            EVAL_FCALL | EVAL_BOUNDS | EVAL_WARN | EVAL_REPORT;
         tree_t new = eval(t, flags);
         const tree_kind_t new_kind = tree_kind(new);
         if (new_kind == T_LITERAL || new_kind == T_REF)
            return assume_int(new);
      }
      // Fall-through

   default:
      fatal_at(tree_loc(t), "expression cannot be folded to "
               "an integer constant");
   }
}

void range_bounds(range_t r, int64_t *low, int64_t *high)
{
   const int64_t left = assume_int(r.left);
   const int64_t right = assume_int(r.right);

   *low  = r.kind == RANGE_TO ? left : right;
   *high = r.kind == RANGE_TO ? right : left;
}

tree_t call_builtin(const char *builtin, type_t type, ...)
{
   struct decl_cache {
      struct decl_cache *next;
      ident_t bname;
      tree_t  decl;
   };

   char *name = xasprintf("NVC.BUILTIN.%s", builtin);
   for (char *p = name; *p != '\0'; p++)
      *p = toupper((int)*p);

   static struct decl_cache *cache = NULL;

   ident_t bname = ident_new(builtin);
   ident_t name_i = ident_new(name);
   free(name);

   struct decl_cache *it;
   tree_t decl = NULL;
   for (it = cache; it != NULL; it = it->next) {
      if (it->bname == bname) {
         decl = it->decl;
         break;
      }
   }

   if (decl == NULL) {
      decl = tree_new(T_FUNC_DECL);
      tree_set_ident(decl, name_i);
      tree_add_attr_str(decl, builtin_i, ident_new(builtin));
   }

   struct decl_cache *c = xmalloc(sizeof(struct decl_cache));
   c->next  = cache;
   c->bname = bname;
   c->decl  = decl;

   cache = c;

   tree_t call = tree_new(T_FCALL);
   tree_set_ident(call, name_i);
   tree_set_ref(call, decl);
   if (type != NULL)
      tree_set_type(call, type);

   va_list ap;
   va_start(ap, type);
   tree_t arg;
   while ((arg = va_arg(ap, tree_t))) {
      tree_t p = tree_new(T_PARAM);
      tree_set_value(p, arg);
      tree_set_loc(p, tree_loc(arg));
      tree_set_subkind(p, P_POS);

      tree_add_param(call, p);
   }
   va_end(ap);

   return call;
}

bool folded_int(tree_t t, int64_t *l)
{
   if ((tree_kind(t) == T_LITERAL) && (tree_subkind(t) == L_INT)) {
      *l = tree_ival(t);
      return true;
   }
   else
      return false;
}

bool folded_real(tree_t t, double *l)
{
   if ((tree_kind(t) == T_LITERAL) && (tree_subkind(t) == L_REAL)) {
      *l = tree_dval(t);
      return true;
   }
   else
      return false;
}

bool folded_length(range_t r, int64_t *l)
{
   int64_t low, high;
   if (folded_bounds(r, &low, &high)) {
      *l = MAX(high - low + 1, 0);
      return true;
   }
   else
      return false;
}

bool folded_bounds(range_t r, int64_t *low, int64_t *high)
{
   int64_t left, right;
   unsigned leftu, rightu;
   if (folded_int(r.left, &left) && folded_int(r.right, &right))
       ;
   else if (folded_enum(r.left, &leftu) && folded_enum(r.right, &rightu)) {
      left  = leftu;
      right = rightu;
   }
   else
      return false;

   switch (r.kind) {
   case RANGE_TO:
      *low  = left;
      *high = right;
      return true;
   case RANGE_DOWNTO:
      *low  = right;
      *high = left;
      return true;
   default:
      return false;
   }
}

bool folded_bounds_real(range_t r, double *low, double *high)
{
   double left, right;
   if (folded_real(r.left, &left) && folded_real(r.right, &right)) {
      switch (r.kind) {
      case RANGE_TO:
         *low  = left;
         *high = right;
         return true;
      case RANGE_DOWNTO:
         *low  = right;
         *high = left;
         return true;
      default:
         return false;
      }
   }
   else
      return false;
}

bool folded_enum(tree_t t, unsigned *pos)
{
   if (tree_kind(t) == T_REF) {
      tree_t decl = tree_ref(t);
      if (tree_kind(decl) == T_ENUM_LIT) {
         *pos = tree_pos(decl);
         return true;
      }
   }

   return false;
}

bool folded_bool(tree_t t, bool *b)
{
   if (tree_kind(t) == T_REF) {
      tree_t decl = tree_ref(t);
      if (tree_kind(decl) == T_ENUM_LIT
          && type_ident(tree_type(decl)) == std_bool_i) {
         *b = (tree_pos(decl) == 1);
         return true;
      }
   }

   return false;
}

tree_t get_enum_lit(tree_t t, int pos)
{
   type_t enum_type = type_base_recur(tree_type(t));
   tree_t lit = type_enum_literal(enum_type, pos);

   tree_t b = tree_new(T_REF);
   tree_set_loc(b, tree_loc(t));
   tree_set_ref(b, lit);
   tree_set_type(b, enum_type);
   tree_set_ident(b, tree_ident(lit));

   return b;
}

tree_t get_int_lit(tree_t t, int64_t i)
{
   tree_t f = tree_new(T_LITERAL);
   tree_set_subkind(f, L_INT);
   tree_set_ival(f, i);
   tree_set_loc(f, tree_loc(t));
   tree_set_type(f, tree_type(t));

   return f;
}

tree_t get_real_lit(tree_t t, double r)
{
   tree_t f = tree_new(T_LITERAL);
   tree_set_loc(f, tree_loc(t));
   tree_set_subkind(f, L_REAL);
   tree_set_dval(f, r);
   tree_set_type(f, tree_type(t));

   return f;
}

const char *package_signal_path_name(ident_t i)
{
   const char *str = istr(i);
   char *buf = get_fmt_buf(strlen(str) + 3);
   char *p = buf;

   *p++ = ':';
   while (*str != '\0') {
      if (*str == '.') {
         *p++ = ':';
         str++;
      }
      else {
         *p++ = tolower((int)*str);
         str++;
      }
   }
   *p = '\0';

   return buf;
}

bool parse_value(type_t type, const char *str, int64_t *value)
{
   while (isspace((int)*str))
      ++str;

   switch (type_kind(type_base_recur(type))) {
   case T_INTEGER:
      {
         int64_t sum = 0;
         while (isdigit((int)*str) || (*str == '_')) {
            if (*str != '_') {
               sum *= 10;
               sum += (*str - '0');
            }
            ++str;
         }

         *value = sum;
      }
      break;

   case T_ENUM:
      {
         bool upcase = true;
         char *copy = xstrdup(str), *p;
         for (p = copy; (*p != '\0') && !isspace((int)*p); p++, str++) {
            if (*p == '\'')
               upcase = false;
            if (upcase)
               *p = toupper((int)*p);
         }
         *p = '\0';

         ident_t id = ident_new(copy);
         free(copy);

         *value = -1;

         const int nlits = type_enum_literals(type);
         for (int i = 0; (*value == -1) && (i < nlits); i++) {
            if (tree_ident(type_enum_literal(type, i)) == id)
               *value = i;
         }

         if (*value == -1)
            return false;
      }
      break;

   default:
      break;
   }

   while (*str != '\0') {
      if (!isspace((int)*str)) {
         str++;
         return false;
      }
      else {
         str++;
      }
   }

   return true;
}

tree_t make_ref(tree_t to)
{
   tree_t t = tree_new(T_REF);
   tree_set_ident(t, tree_ident(to));
   tree_set_ref(t, to);
   tree_set_type(t, tree_type(to));
   return t;
}

vhdl_standard_t standard(void)
{
   return current_std;
}

void set_standard(vhdl_standard_t s)
{
   current_std = s;
}

const char *standard_text(vhdl_standard_t s)
{
   static const char *text[] = {
      "1987", "1993", "2000", "2002", "2008"
   };

   if ((unsigned)s < ARRAY_LEN(text))
      return text[s];
   else
      return "????";
}

int record_field_to_net(type_t type, ident_t name)
{
   int offset = 0;

   const int nfields = type_fields(type);
   for (int i = 0; i < nfields; i++) {
      tree_t field = type_field(type, i);
      if (tree_ident(field) == name)
         return offset;
      else
         offset += type_width(tree_type(field));
   }

   assert(false);
}

tree_t find_record_field(tree_t rref)
{
   ident_t fname = tree_ident(rref);
   type_t value_type = tree_type(tree_value(rref));

   const int nfields = type_fields(value_type);
   for (int i = 0; i < nfields; i++) {
      tree_t field = type_field(value_type, i);
      if (tree_ident(field) == fname)
         return field;
   }

   return NULL;
}

class_t class_of(tree_t t)
{
   switch (tree_kind(t)) {
   case T_VAR_DECL:
      return C_VARIABLE;
   case T_SIGNAL_DECL:
      return C_SIGNAL;
   case T_CONST_DECL:
      return C_CONSTANT;
   case T_PORT_DECL:
      return tree_class(t);
   case T_ENUM_LIT:
   case T_GENVAR:
   case T_ALIAS:
   case T_FIELD_DECL:
      return C_DEFAULT;
   case T_UNIT_DECL:
      return C_UNITS;
   case T_ARCH:
      return C_ARCHITECTURE;
   case T_FUNC_DECL:
   case T_FUNC_BODY:
      return C_FUNCTION;
   case T_PROC_DECL:
   case T_PROC_BODY:
      return C_PROCEDURE;
   case T_ENTITY:
      return C_ENTITY;
   case T_TYPE_DECL:
      return C_TYPE;
   case T_FILE_DECL:
      return C_FILE;
   case T_PROCESS:
   case T_BLOCK:
   case T_FOR:
      return C_LABEL;
   case T_COMPONENT:
      return C_COMPONENT;
   case T_REF:
      return class_of(tree_ref(t));
   case T_ARRAY_REF:
   case T_ARRAY_SLICE:
   case T_RECORD_REF:
      return class_of(tree_value(t));
   case T_PACKAGE:
      return C_PACKAGE;
   case T_LIBRARY:
      return C_LIBRARY;
   case T_ELAB:
      return C_ELAB;
   default:
      fatal("missing class_of for %s", tree_kind_str(tree_kind(t)));
   }
}

bool class_has_type(class_t c)
{
   switch (c) {
   case C_LABEL:
   case C_ENTITY:
   case C_ARCHITECTURE:
   case C_COMPONENT:
   case C_CONFIGURATION:
   case C_PACKAGE:
   case C_LIBRARY:
   case C_ELAB:
      return false;
   default:
      return true;
   }
}

const char *class_str(class_t c)
{
   static const char *strs[] = {
      "default", "signal", "variable", "constant", "file", "entity",
      "component", "configuration", "architecture", "function", "package",
      "type", "subtype", "label", "procedure", "literal", "units"
   };
   assert(c < ARRAY_LEN(strs));
   return strs[c];
}

tree_t add_param(tree_t call, tree_t value, param_kind_t kind, tree_t name)
{
   tree_t p = tree_new(T_PARAM);
   tree_set_loc(p, tree_loc(value));
   tree_set_subkind(p, kind);
   tree_set_value(p, value);

   if (kind == P_NAMED) {
      assert(name != NULL);
      tree_set_name(p, name);
   }

   tree_add_param(call, p);
   return p;
}


type_t array_aggregate_type(type_t array, int from_dim)
{
   if (type_is_unconstrained(array)) {
      const int nindex = type_index_constrs(array);
      assert(from_dim < nindex);

      type_t type = type_new(T_UARRAY);
      type_set_ident(type, type_ident(array));
      type_set_elem(type, type_elem(array));

      for (int i = from_dim; i < nindex; i++)
         type_add_index_constr(type, type_index_constr(array, i));

      return type;
   }
   else {
      const int ndims = type_dims(array);
      assert(from_dim < ndims);

      type_t type = type_new(T_CARRAY);
      type_set_ident(type, type_ident(array));
      type_set_elem(type, type_elem(array));

      for (int i = from_dim; i < ndims; i++)
         type_add_dim(type, type_dim(array, i));

      return type;
   }
}

tree_t make_default_value(type_t type, const loc_t *loc)
{
   type_t base = type_base_recur(type);

   switch (type_kind(base)) {
   case T_UARRAY:
      assert(type_kind(type) == T_SUBTYPE);
      // Fall-through

   case T_CARRAY:
      {
         tree_t def = NULL;
         const int ndims = type_dims(type);
         for (int i = ndims - 1; i >= 0; i--) {
            tree_t val = (def ? def : make_default_value(type_elem(base), loc));
            def = tree_new(T_AGGREGATE);
            tree_set_type(def, array_aggregate_type(type, i));

            tree_t a = tree_new(T_ASSOC);
            tree_set_subkind(a, A_OTHERS);
            tree_set_value(a, val);

            tree_add_assoc(def, a);
         }
         tree_set_type(def, type);
         tree_set_loc(def, loc);
         return def;
      }

   case T_INTEGER:
   case T_PHYSICAL:
   case T_REAL:
      return type_dim(type, 0).left;

   case T_ENUM:
      {
         int64_t val = 0;
         const bool folded = folded_int(type_dim(type, 0).left, &val);
         if (folded)
            return make_ref(type_enum_literal(base, (unsigned) val));
         else
            return type_dim(type, 0).left;
      }

   case T_RECORD:
      {
         tree_t def = tree_new(T_AGGREGATE);
         tree_set_loc(def, loc);
         const int nfields = type_fields(base);
         for (int i = 0; i < nfields; i++) {
            tree_t field = type_field(base, i);

            tree_t a = tree_new(T_ASSOC);
            tree_set_subkind(a, A_POS);
            tree_set_value(a, make_default_value(tree_type(field),
                                                 tree_loc(field)));

            tree_add_assoc(def, a);
         }
         tree_set_type(def, type);
         return def;
      }

   case T_ACCESS:
      {
         tree_t null = tree_new(T_LITERAL);
         tree_set_loc(null, loc);
         tree_set_subkind(null, L_NULL);
         tree_set_type(null, type);
         return null;
      }

   case T_UNRESOLVED:
      return NULL;

   default:
      fatal_trace("cannot handle type %s in %s",
                  type_kind_str(type_kind(base)), __func__);
   }
}

const char *fmt_time_r(char *buf, size_t len, uint64_t t)
{
   static const struct {
      uint64_t time;
      const char *unit;
   } units[] = {
      { UINT64_C(1), "fs" },
      { UINT64_C(1000), "ps" },
      { UINT64_C(1000000), "ns" },
      { UINT64_C(1000000000), "us" },
      { UINT64_C(1000000000000), "ms" },
      { 0, NULL }
   };

   int u = 0;
   while (units[u + 1].unit && (t % units[u + 1].time == 0))
      ++u;

   snprintf(buf, len, "%"PRIu64"%s",
            t / units[u].time, units[u].unit);

   return buf;
}

const char *fmt_time(uint64_t t)
{
   static const int BUF_SZ = 64;
   return fmt_time_r(get_fmt_buf(BUF_SZ), BUF_SZ, t);
}

unsigned bits_for_range(int64_t low, int64_t high)
{
   assert(low <= high);

   if (low < 0) {
      // Signed integers
      if (low >= INT8_MIN && high <= INT8_MAX)
         return 8;
      else if (low >= INT16_MIN && high <= INT16_MAX)
         return 16;
      else if (low >= INT32_MIN && high <= INT32_MAX)
         return 32;
      else
         return 64;
   }
   else {
      // Unsigned integers
      if (high <= 1)
         return 1;
      else if (high <= UINT8_MAX)
         return 8;
      else if (high <= UINT16_MAX)
         return 16;
      else if (high <= UINT32_MAX)
         return 32;
      else
         return 64;
   }
}

unsigned array_dimension(type_t a)
{
   return (type_is_unconstrained(a)
           ? type_index_constrs(type_base_recur(a))
           : type_dims(a));
}

type_t index_type_of(type_t type, int dim)
{
   if (type_is_unconstrained(type))
      return type_index_constr(type_base_recur(type), dim);
   else if (type_kind(type) == T_ENUM)
      return type;
   else {
      tree_t left = type_dim(type, dim).left;

      // If the left bound has not been assigned a type then there is some
      // error with it so just return a dummy type here
      return tree_has_type(left) ? tree_type(left) : type;
   }
}

int64_t rebase_index(type_t array_type, int dim, int64_t value)
{
   // Convert value which is in the range of array_type to a zero-based index
   range_t r = type_dim(array_type, dim);
   const int64_t left = assume_int(r.left);
   return (r.kind == RANGE_TO) ? value - left : left - value;
}

tree_t str_to_literal(const char *start, const char *end, type_t type)
{
   tree_t t = tree_new(T_LITERAL);
   tree_set_subkind(t, L_STRING);

   type_t elem = NULL;
   if (type != NULL) {
      tree_set_type(t, type);
      elem = type_elem(type);
   }

   for (const char *p = start; *p != '\0' && p != end; p++) {
      if (*(const unsigned char *)p == 0x81)
         continue;   // Allow UTF-8 encoded ASCII characters

      const char ch[] = { '\'', *p, '\'', '\0' };
      ident_t id = ident_new(ch);
      tree_t ref = tree_new(T_REF);
      tree_set_ident(ref, id);
      tree_add_char(t, ref);

      if (elem != NULL) {
         const int nlit = type_enum_literals(elem);
         for (int i = 0; i < nlit; i++) {
            tree_t lit = type_enum_literal(elem, i);
            if (tree_ident(lit) == id) {
               tree_set_ref(ref, lit);
               break;
            }
         }
      }
   }

   return t;
}

bool loc_eq(const loc_t *a, const loc_t *b)
{
   return a->first_line == b->first_line
      && a->first_column == b->first_column
      && a->last_line == b->last_line
      && a->last_column == b->last_column
      && a->file == b->file;
}

void loc_write(const loc_t *loc, fbuf_t *f, ident_wr_ctx_t ctx)
{
   ident_write(loc->file, ctx);

   const uint64_t merged =
      ((uint64_t)loc->first_line << 44)
      | ((uint64_t)loc->first_column << 32)
      | ((uint64_t)loc->last_line << 12)
      | (uint64_t)loc->last_column;

   write_u64(merged, f);
}

void loc_read(loc_t *loc, fbuf_t *f, ident_rd_ctx_t ctx)
{
   loc->file = ident_read(ctx);
   loc->linebuf = NULL;

   const uint64_t merged = read_u64(f);

   loc->first_line   = (merged >> 44) & 0xfffff;
   loc->first_column = (merged >> 32) & 0xfff;
   loc->last_line    = (merged >> 12) & 0xfffff;
   loc->last_column  = merged & 0xfff;
}

void intern_strings(void)
{
   builtin_i        = ident_new("builtin");
   std_standard_i   = ident_new("STD.STANDARD");
   formal_i         = ident_new("formal");
   elab_copy_i      = ident_new("elab_copy");
   all_i            = ident_new("all");
   protected_i      = ident_new("protected");
   inst_name_i      = ident_new("INSTANCE_NAME");
   fst_dir_i        = ident_new("fst_dir");
   scope_pop_i      = ident_new("scope_pop");
   partial_map_i    = ident_new("partial_map");
   fst_data_i       = ident_new("fst_data");
   std_logic_i      = ident_new("IEEE.STD_LOGIC_1164.STD_LOGIC");
   std_ulogic_i     = ident_new("IEEE.STD_LOGIC_1164.STD_ULOGIC");
   std_bit_i        = ident_new("STD.STANDARD.BIT");
   std_bool_i       = ident_new("STD.STANDARD.BOOLEAN");
   std_char_i       = ident_new("STD.STANDARD.CHARACTER");
   natural_i        = ident_new("STD.STANDARD.NATURAL");
   positive_i       = ident_new("STD.STANDARD.POSITIVE");
   signed_i         = ident_new("IEEE.NUMERIC_STD.SIGNED");
   unsigned_i       = ident_new("IEEE.NUMERIC_STD.UNSIGNED");
   foreign_i        = ident_new("FOREIGN");
   nested_i         = ident_new("nested");
   drives_all_i     = ident_new("drives_all");
   driver_init_i    = ident_new("driver_init");
   static_i         = ident_new("static");
   mangled_i        = ident_new("mangled");
   null_range_i     = ident_new("null_range");
   deferred_i       = ident_new("deferred");
   prot_field_i     = ident_new("prot_field");
   stmt_tag_i       = ident_new("stmt_tag");
   cond_tag_i       = ident_new("cond_tag");
   sub_cond_i       = ident_new("sub_cond");
   range_var_i      = ident_new("range_var");
   work_i           = ident_new("WORK");
   llvm_i           = ident_new("llvm");
   wait_level_i     = ident_new("wait_level");
   impure_io_i      = ident_new("impure_io");
   simple_name_i    = ident_new("simple_name");
   std_i            = ident_new("STD");
   nnets_i          = ident_new("nnets");
}
