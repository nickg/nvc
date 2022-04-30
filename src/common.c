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
#include "common.h"
#include "ident.h"
#include "opt.h"
#include "type.h"

#include <assert.h>
#include <ctype.h>
#include <string.h>
#include <stdlib.h>
#include <inttypes.h>

static vhdl_standard_t current_std = STD_93;
static int             relax = 0;
static ident_t         id_cache[NUM_WELL_KNOWN];

int64_t assume_int(tree_t t)
{
   switch (tree_kind(t)) {
   case T_LITERAL:
      switch (tree_subkind(t)) {
      case L_INT:
      case L_PHYSICAL:
         return tree_ival(t);
      }
      break;

   case T_REF:
      {
         tree_t decl = tree_ref(t);
         if (tree_kind(decl) == T_CONST_DECL) {
            if (tree_has_value(decl))
               return assume_int(tree_value(decl));
            else {
               // Deferred constant
               tree_t pack = tree_container(decl);
               assert(tree_kind(pack) == T_PACKAGE);

               tree_t body = body_of(pack);
               ident_t id = tree_ident(decl);

               if (body != NULL && (decl = search_decls(body, id, 0))) {
                  assert(tree_kind(decl) == T_CONST_DECL);
                  assert(tree_has_value(decl));
                  return assume_int(tree_value(decl));
               }
            }
         }
         else {
            assert(tree_kind(decl) == T_ENUM_LIT);
            return tree_pos(decl);
         }
      }

   default:
      break;
   }

   fatal_at(tree_loc(t), "expression cannot be folded to "
            "an integer constant");
}

void range_bounds(tree_t r, int64_t *low, int64_t *high)
{
   assert(tree_kind(r) == T_RANGE);

   const int64_t left = assume_int(tree_left(r));
   const int64_t right = assume_int(tree_right(r));

   *low  = tree_subkind(r) == RANGE_TO ? left : right;
   *high = tree_subkind(r) == RANGE_TO ? right : left;
}

bool folded_int(tree_t t, int64_t *l)
{
   switch (tree_kind(t)) {
   case T_LITERAL:
      switch (tree_subkind(t)) {
      case L_PHYSICAL:
         if (tree_has_ref(t))
            return false;
         // Fall-through
      case L_INT:
         *l = tree_ival(t);
         return true;
      default:
         return false;
      }
   case T_QUALIFIED:
      return folded_int(tree_value(t), l);
   default:
      return false;
   }
}

bool folded_real(tree_t t, double *l)
{
   switch (tree_kind(t)) {
   case T_LITERAL:
      if (tree_subkind(t) == L_REAL) {
         *l = tree_dval(t);
         return true;
      }
      else
         return false;
   case T_QUALIFIED:
      return folded_real(tree_value(t), l);
   default:
      return false;
   }
}

bool folded_length(tree_t r, int64_t *l)
{
   int64_t low, high;
   if (folded_bounds(r, &low, &high)) {
      *l = MAX(high - low + 1, 0);
      return true;
   }
   else
      return false;
}

bool folded_bounds(tree_t r, int64_t *low, int64_t *high)
{
   assert(tree_kind(r) == T_RANGE);

   const range_kind_t rkind = tree_subkind(r);

   if (rkind != RANGE_TO && rkind != RANGE_DOWNTO)
      return false;

   int64_t left, right;
   unsigned leftu, rightu;
   if (folded_int(tree_left(r), &left) && folded_int(tree_right(r), &right))
       ;
   else if (folded_enum(tree_left(r), &leftu)
            && folded_enum(tree_right(r), &rightu)) {
      left  = leftu;
      right = rightu;
   }
   else
      return false;

   switch (rkind) {
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

bool folded_bounds_real(tree_t r, double *low, double *high)
{
   assert(tree_kind(r) == T_RANGE);

   const range_kind_t rkind = tree_subkind(r);

   if (rkind != RANGE_TO && rkind != RANGE_DOWNTO)
      return false;

   double left, right;
   if (folded_real(tree_left(r), &left) && folded_real(tree_right(r), &right)) {
      switch (rkind) {
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
          && type_ident(tree_type(decl)) == well_known(W_STD_BOOL)) {
         *b = (tree_pos(decl) == 1);
         return true;
      }
   }

   return false;
}

tree_t get_enum_lit(tree_t t, type_t type, int pos)
{
   type_t enum_type = type_base_recur(type ?: tree_type(t));
   tree_t lit = type_enum_literal(enum_type, pos);

   tree_t b = tree_new(T_REF);
   tree_set_loc(b, tree_loc(t));
   tree_set_ref(b, lit);
   tree_set_type(b, enum_type);
   tree_set_ident(b, tree_ident(lit));

   return b;
}

tree_t get_int_lit(tree_t t, type_t type, int64_t i)
{
   tree_t f = tree_new(T_LITERAL);
   tree_set_subkind(f, L_INT);
   tree_set_ival(f, i);
   tree_set_loc(f, tree_loc(t));
   tree_set_type(f, type ?: tree_type(t));

   return f;
}

tree_t get_real_lit(tree_t t, type_t type, double r)
{
   tree_t f = tree_new(T_LITERAL);
   tree_set_loc(f, tree_loc(t));
   tree_set_subkind(f, L_REAL);
   tree_set_dval(f, r);
   tree_set_type(f, type ?: tree_type(t));

   return f;
}

const char *package_signal_path_name(ident_t i)
{
   const char *str = istr(i);
   if (str[0] == ':')
      return str;

   char *buf = get_fmt_buf(strlen(str) + 3);
   char *p = buf;

   // XXX: GHDL gives something like :work:p:f[bit return integer]: for
   // objects inside package subprograms. The algorithm below is wrong
   // because overloads may create duplicate paths names.

   bool in_params = false, in_return = false;
   *p++ = ':';
   while (*str != '\0') {
      if (*str == '.') {
         *p++ = ':';
         in_return = false;
      }
      else if (*str == '(')
         in_params = true;
      else if (*str == ')') {
         in_params = false;
         in_return = true;
      }
      else if (!in_params && !in_return) {
         *p++ = tolower((int)*str);
      }
      str++;
   }
   *p = '\0';

   return buf;
}

bool parse_value(type_t type, const char *str, int64_t *value)
{
   while (isspace((int)*str))
      ++str;

   type_t base = type_base_recur(type);

   switch (type_kind(base)) {
   case T_INTEGER:
      {
         bool is_negative = *str == '-';
         int num_digits = 0;

         if (is_negative) {
            ++str;
         }
         int64_t sum = 0;
         while (isdigit((int)*str) || (*str == '_')) {
            if (*str != '_') {
               sum *= 10;
               sum += (*str - '0');
               num_digits++;
            }
            ++str;
         }

         if (is_negative) {
            *value = -sum;
         }
         else {
            *value = sum;
         }

         if (num_digits == 0) {
            return false;
         }
      }
      break;

   case T_ENUM:
      {
         bool upcase = true;
         char *copy LOCAL = xstrdup(str), *p;
         for (p = copy; (*p != '\0') && !isspace((int)*p); p++, str++) {
            if (*p == '\'')
               upcase = false;
            if (upcase)
               *p = toupper((int)*p);
         }
         *p = '\0';

         ident_t id = ident_new(copy);

         *value = -1;

         const int nlits = type_enum_literals(base);
         for (int i = 0; (*value == -1) && (i < nlits); i++) {
            if (tree_ident(type_enum_literal(base, i)) == id)
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

int record_field_to_net(type_t type, unsigned pos)
{
   int offset = 0;
   for (int i = 0; i < pos; i++)
      offset += type_width(tree_type(type_field(type, i)));

   return offset;
}

tree_t find_record_field(tree_t rref)
{
   ident_t fname = tree_ident(rref);
   type_t value_type = tree_type(tree_value(rref));

   if (!type_is_record(value_type))
      return NULL;

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
   case T_IMPLICIT_SIGNAL:
      return C_SIGNAL;
   case T_CONST_DECL:
      return C_CONSTANT;
   case T_PORT_DECL:
   case T_GENERIC_DECL:
   case T_PARAM_DECL:
   case T_EXTERNAL_NAME:
      return tree_class(t);
   case T_ENUM_LIT:
   case T_LITERAL:
      return C_LITERAL;
   case T_FIELD_DECL:
   case T_ATTR_DECL:
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
   case T_SUBTYPE_DECL:
      return C_SUBTYPE;
   case T_TYPE_DECL:
      return C_TYPE;
   case T_FILE_DECL:
      return C_FILE;
   case T_PROCESS:
   case T_BLOCK:
   case T_FOR:
   case T_INSTANCE:
      return C_LABEL;
   case T_COMPONENT:
      return C_COMPONENT;
   case T_REF:
      return tree_has_ref(t) ? class_of(tree_ref(t)) : C_DEFAULT;
   case T_ARRAY_REF:
   case T_ARRAY_SLICE:
   case T_RECORD_REF:
   case T_ALIAS:
      return class_of(tree_value(t));
   case T_PACKAGE:
   case T_PACK_BODY:
   case T_PACK_INST:
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

const char *assoc_kind_str(assoc_kind_t akind)
{
   switch (akind) {
   case A_NAMED:  return "named";
   case A_POS:    return "positional";
   case A_OTHERS: return "others";
   case A_RANGE:  return "range";
   default:       return "??";
   }
}

bool is_subprogram(tree_t t)
{
   switch (tree_kind(t)) {
   case T_FUNC_DECL:
   case T_FUNC_BODY:
   case T_PROC_DECL:
   case T_PROC_BODY:
      return true;
   case T_GENERIC_DECL:
      {
         const class_t class = tree_class(t);
         return class == C_FUNCTION || class == C_PROCEDURE;
      }
   default:
      return false;
   }
}

bool is_loop_stmt(tree_t t)
{
   const tree_kind_t kind = tree_kind(t);
   return kind == T_WHILE || kind == T_FOR;
}

bool is_container(tree_t t)
{
   switch (tree_kind(t)) {
   case T_FUNC_BODY:
   case T_PROC_BODY:
   case T_ENTITY:
   case T_ARCH:
   case T_PACKAGE:
   case T_PACK_BODY:
   case T_CONFIGURATION:
   case T_BLOCK:
   case T_PROT_BODY:
   case T_ELAB:
      return true;
   default:
      return false;
   }
}

bool is_package(tree_t t)
{
   switch (tree_kind(t)) {
   case T_PACKAGE:
   case T_PACK_BODY:
   case T_PACK_INST:
      return true;
   default:
      return false;
   }
}

bool is_design_unit(tree_t t)
{
   switch (tree_kind(t)) {
   case T_ENTITY:
   case T_ARCH:
   case T_PACKAGE:
   case T_PACK_BODY:
   case T_CONFIGURATION:
   case T_CONTEXT:
   case T_PACK_INST:
      return true;
   default:
      return false;
   }
}

bool is_guarded_signal(tree_t decl)
{
   return !!(tree_flags(decl) & (TREE_F_BUS | TREE_F_REGISTER));
}

bool is_type_decl(tree_t t)
{
   switch (tree_kind(t)) {
   case T_TYPE_DECL:
   case T_SUBTYPE_DECL:
      return true;
   default:
      return false;
   }
}

tree_t add_param(tree_t call, tree_t value, param_kind_t kind, tree_t name)
{
   tree_t p = tree_new(T_PARAM);
   tree_set_loc(p, tree_loc(value));
   tree_set_subkind(p, kind);
   tree_set_value(p, value);

   switch (kind) {
   case P_NAMED:
      assert(name != NULL);
      tree_set_name(p, name);
      break;
   case P_POS:
      tree_set_pos(p, tree_params(call));
      break;
   }

   tree_add_param(call, p);
   return p;
}

type_t array_aggregate_type(type_t array, int from_dim)
{
   if (type_is_none(array))
      return type_new(T_NONE);

   if (type_is_unconstrained(array)) {
      const int nindex = type_index_constrs(array);
      assert(from_dim < nindex);

      type_t type = type_new(T_ARRAY);
      type_set_ident(type, type_ident(array));
      type_set_elem(type, type_elem(array));

      for (int i = from_dim; i < nindex; i++)
         type_add_index_constr(type, type_index_constr(array, i));

      return type;
   }
   else {
      const int ndims = dimension_of(array);
      assert(from_dim < ndims);

      type_t base = type_new(T_ARRAY);
      type_set_ident(base, type_ident(array));
      type_set_elem(base, type_elem(array));

      tree_t constraint = tree_new(T_CONSTRAINT);
      tree_set_subkind(constraint, C_INDEX);

      type_t sub = type_new(T_SUBTYPE);
      type_set_base(sub, base);
      type_add_constraint(sub, constraint);

      for (int i = from_dim; i < ndims; i++) {
         tree_t r = range_of(array, i);
         type_add_index_constr(base, tree_type(r));
         tree_add_range(constraint, r);
      }

      return sub;
   }
}

int fmt_time_r(char *buf, size_t len, uint64_t t)
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

   return checked_sprintf(buf, len, "%"PRIu64"%s",
                          t / units[u].time, units[u].unit);
}

const char *fmt_time(uint64_t t)
{
   static const int BUF_SZ = 64;
   char *buf = get_fmt_buf(BUF_SZ);
   fmt_time_r(buf, BUF_SZ, t);
   return buf;
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

unsigned dimension_of(type_t type)
{
   switch (type_kind(type)) {
   case T_SUBTYPE:
      if (type_constraints(type) > 0)
         return tree_ranges(type_constraint(type, 0));
      else
         return dimension_of(type_base(type));
   case T_ARRAY:
      return type_index_constrs(type);
   case T_NONE:
   case T_ACCESS:
   case T_RECORD:
      return 0;
   case T_INTEGER:
   case T_REAL:
   case T_PHYSICAL:
   case T_ENUM:
      return type_dims(type);
   default:
      fatal_trace("invalid type kind %s in dimension_of",
                  type_kind_str(type_kind(type)));
   }
}

tree_t range_of(type_t type, unsigned dim)
{
   switch (type_kind(type)) {
   case T_SUBTYPE:
      if (type_constraints(type) > 0)
         return tree_range(type_constraint(type, 0), dim);
      else
         return range_of(type_base(type), dim);
   case T_INTEGER:
   case T_REAL:
   case T_PHYSICAL:
   case T_ENUM:
      return type_dim(type, dim);
   default:
      fatal_trace("invalid type kind %s in range_of",
                  type_kind_str(type_kind(type)));
   }
}

range_kind_t direction_of(type_t type, unsigned dim)
{
   switch (type_kind(type)) {
   case T_ENUM:
      return RANGE_TO;
   case T_NONE:
      return RANGE_ERROR;
   case T_INTEGER:
   case T_REAL:
   case T_PHYSICAL:
   case T_SUBTYPE:
      {
         tree_t r = range_of(type, dim);
         const range_kind_t rkind = tree_subkind(r);
         if (rkind == RANGE_EXPR) {
            // Return a fixed direction if possible
            tree_t value = tree_value(r);
            assert(tree_kind(value) == T_ATTR_REF);

            DEBUG_ONLY({
                  const attr_kind_t attr = tree_subkind(value);
                  assert(attr == ATTR_RANGE || attr == ATTR_REVERSE_RANGE);
               });

            tree_t name = tree_name(value);
            assert(tree_kind(name) == T_REF);

            tree_t decl = tree_ref(name);
            if (is_type_decl(decl))
               return direction_of(tree_type(decl), 0);
         }

         return rkind;
      }
   default:
      fatal_trace("invalid type kind %s in direction_of",
                  type_kind_str(type_kind(type)));
   }
}

type_t index_type_of(type_t type, unsigned dim)
{
   if (dim >= dimension_of(type))
      return NULL;

   type_t base = type_base_recur(type);
   type_kind_t base_kind = type_kind(base);
   if (base_kind == T_ARRAY)
      return type_index_constr(base, dim);
   else if (base_kind == T_ENUM || base_kind == T_NONE)
      return type;
   else if (base_kind == T_RECORD)
      return NULL;
   else
      return tree_type(range_of(base, dim));
}

int64_t rebase_index(type_t array_type, int dim, int64_t value)
{
   // Convert value which is in the range of array_type to a zero-based index
   tree_t r = range_of(array_type, dim);
   const int64_t left = assume_int(tree_left(r));
   return (tree_subkind(r) == RANGE_TO) ? value - left : left - value;
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

ident_t well_known(well_known_t id)
{
   assert(id < NUM_WELL_KNOWN);
   return id_cache[id];
}

well_known_t is_well_known(ident_t ident)
{
   well_known_t pos = 0;
   for (; pos < NUM_WELL_KNOWN; pos++) {
      if (id_cache[pos] == ident)
         break;
   }

   return pos;
}

void intern_strings(void)
{
   id_cache[W_STD_STANDARD]  = ident_new("STD.STANDARD");
   id_cache[W_ALL]           = ident_new("all");
   id_cache[W_STD_LOGIC]     = ident_new("IEEE.STD_LOGIC_1164.STD_LOGIC");
   id_cache[W_STD_ULOGIC]    = ident_new("IEEE.STD_LOGIC_1164.STD_ULOGIC");
   id_cache[W_STD_BIT]       = ident_new("STD.STANDARD.BIT");
   id_cache[W_STD_BOOL]      = ident_new("STD.STANDARD.BOOLEAN");
   id_cache[W_STD_CHAR]      = ident_new("STD.STANDARD.CHARACTER");
   id_cache[W_STD_NATURAL]   = ident_new("STD.STANDARD.NATURAL");
   id_cache[W_STD_POSITIVE]  = ident_new("STD.STANDARD.POSITIVE");
   id_cache[W_IEEE_SIGNED]   = ident_new("IEEE.NUMERIC_STD.SIGNED");
   id_cache[W_IEEE_UNSIGNED] = ident_new("IEEE.NUMERIC_STD.UNSIGNED");
   id_cache[W_FOREIGN]       = ident_new("FOREIGN");
   id_cache[W_WORK]          = ident_new("WORK");
   id_cache[W_STD]           = ident_new("STD");
   id_cache[W_THUNK]         = ident_new("thunk");
   id_cache[W_BODY]          = ident_new("body");
   id_cache[W_CARET]         = ident_new("^");
}

bool is_uninstantiated_package(tree_t pack)
{
   return tree_kind(pack) == T_PACKAGE
      && tree_generics(pack) > 0
      && tree_genmaps(pack) == 0;
}

bool unit_needs_cgen(tree_t unit)
{
   switch (tree_kind(unit)) {
   case T_ELAB:
   case T_PACK_INST:
      return true;
   case T_PACK_BODY:
      {
         tree_t pack = tree_primary(unit);
         return package_needs_body(pack) && !is_uninstantiated_package(pack);
      }
   case T_PACKAGE:
      return !package_needs_body(unit) && !is_uninstantiated_package(unit);
   default:
      return false;
   }
}

bool package_needs_body(tree_t pack)
{
   assert(tree_kind(pack) == T_PACKAGE);

   const int ndecls = tree_decls(pack);
   for (int i = 0; i < ndecls; i++) {
      tree_t d = tree_decl(pack, i);

      switch (tree_kind(d)) {
      case T_FUNC_DECL:
      case T_PROC_DECL:
         if (tree_flags(d) & (TREE_F_PREDEFINED | TREE_F_FOREIGN))
            continue;
         return true;

      case T_CONST_DECL:
         if (!tree_has_value(d))
            return true;
         continue;

      case T_TYPE_DECL:
         if (type_is_protected(tree_type(d)))
            return true;
         continue;

      default:
         continue;
      }
   }

   return false;
}

int relax_rules(void)
{
   return relax;
}

void set_relax_rules(int mask)
{
   relax = mask;
}

tree_t search_decls(tree_t container, ident_t name, int nth)
{
   type_t type;
   tree_kind_t kind = tree_kind(container);
   if (kind == T_LIBRARY) {
      lib_t lib = lib_require(tree_ident(container));
      return lib_get(lib, name);
   }
   else if ((kind == T_VAR_DECL || kind == T_PARAM_DECL)
            && type_is_protected((type = tree_type(container)))) {
      const int ndecls = type_decls(type);
      for (int i = 0; i < ndecls; i++) {
         tree_t d = type_decl(type, i);
         if (tree_ident(d) == name && nth-- == 0)
            return d;
      }
      return NULL;
   }
   else if (kind == T_ENTITY || kind == T_BLOCK) {
      const int nports = tree_ports(container);
      for (int i = 0; i < nports; i++) {
         tree_t p = tree_port(container, i);
         if (tree_ident(p) == name)
            return p;
      }
   }

   // TODO: how to improve this?
   const int ndecls = tree_decls(container);
   tree_t best = NULL;

   for (int i = 0; i < ndecls; i++) {
      tree_t d = tree_decl(container, i);
      if (tree_ident(d) == name) {
         if (tree_kind(d) == T_TYPE_DECL
             && type_kind(tree_type(d)) == T_INCOMPLETE)
               best = d;
         else if (nth-- == 0)
            return d;
      }
      else if (tree_kind(d) == T_TYPE_DECL) {
         type_t type = tree_type(d);
         switch (type_kind(type)) {
         case T_ENUM:
            {
               const int nlits = type_enum_literals(type);
               for (int j = 0; j < nlits; j++) {
                  tree_t lit = type_enum_literal(type, j);
                  if (tree_ident(lit) == name && nth-- == 0)
                     return lit;
               }
            }
            break;
         default:
            break;
         }
      }
   }

   return best;
}

static tree_t cached_std(tree_t hint)
{
   static tree_t standard_cache[STD_08 + 1] = {};

   const vhdl_standard_t curr = standard();
   assert(curr < ARRAY_LEN(standard_cache));

   if (standard_cache[curr] == NULL) {
      if (hint != NULL)
         standard_cache[curr] = hint;
      else {
         lib_t std = lib_require(well_known(W_STD));
         standard_cache[curr] = lib_get(std, well_known(W_STD_STANDARD));
         assert(standard_cache[curr] != NULL);
      }
   }

   assert(hint == NULL || hint == standard_cache[curr]);
   return standard_cache[curr];
}

type_t std_type(tree_t std, std_type_t which)
{
   static type_t cache[STD_SEVERITY_LEVEL + 1] = {};
   assert(which < ARRAY_LEN(cache));

   if (cache[which] == NULL) {
      const char *names[] = {
         "universal_integer",
         "universal_real",
         "INTEGER",
         "REAL",
         "BOOLEAN",
         "STRING",
         "TIME",
         "BIT",
         "FILE_OPEN_KIND",
         "FILE_OPEN_STATUS",
         "NATURAL",
         "BIT_VECTOR",
         "SEVERITY_LEVEL",
      };

      tree_t d = search_decls(cached_std(std), ident_new(names[which]), 0);
      if (d == NULL)
         fatal_trace("cannot find standard type %s", names[which]);

      // Do not cache standard types while bootstrapping as the GC will
      // move the objects after parsing
      static int can_cache = -1;
      if (can_cache == -1) can_cache = !opt_get_int(OPT_BOOTSTRAP);

      if (can_cache)
         return (cache[which] = tree_type(d));
      else
         return tree_type(d);
   }
   else
      return cache[which];
}

bool is_builtin(subprogram_kind_t kind)
{
   return kind != S_USER && kind != S_FOREIGN && kind != S_VHPIDIRECT;
}

bool is_open_coded_builtin(subprogram_kind_t kind)
{
   switch (kind) {
   case S_USER:
   case S_FOREIGN:
   case S_VHPIDIRECT:
   case S_ARRAY_EQ:
   case S_ARRAY_NEQ:
   case S_ARRAY_LT:
   case S_ARRAY_LE:
   case S_ARRAY_GT:
   case S_ARRAY_GE:
   case S_RECORD_EQ:
   case S_RECORD_NEQ:
   case S_TO_STRING:
   case S_SLL:
   case S_SRL:
   case S_SLA:
   case S_SRA:
   case S_ROL:
   case S_ROR:
   case S_ARRAY_NOT:
   case S_ARRAY_AND:
   case S_ARRAY_OR:
   case S_ARRAY_XOR:
   case S_ARRAY_XNOR:
   case S_ARRAY_NAND:
   case S_ARRAY_NOR:
   case S_MIXED_AND:
   case S_MIXED_OR:
   case S_MIXED_XOR:
   case S_MIXED_XNOR:
   case S_MIXED_NAND:
   case S_MIXED_NOR:
   case S_REDUCE_OR:
   case S_REDUCE_AND:
   case S_REDUCE_NAND:
   case S_REDUCE_NOR:
   case S_REDUCE_XOR:
   case S_REDUCE_XNOR:
   case S_MATCH_EQ:
   case S_MATCH_NEQ:
   case S_MATCH_LT:
   case S_MATCH_LE:
   case S_MATCH_GT:
   case S_MATCH_GE:
   case S_MINIMUM:
   case S_MAXIMUM:
      return false;
   default:
      return true;
   }
}

tree_t find_mangled_decl(tree_t container, ident_t name)
{
   const int ndecls = tree_decls(container);
   for (int i = 0; i < ndecls; i++) {
      tree_t d = tree_decl(container, i);
      if (is_subprogram(d) && tree_has_ident2(d) && tree_ident2(d) == name)
         return d;
      else if (tree_kind(d) == T_TYPE_DECL
               && type_ident(tree_type(d)) == ident_until(name, '$'))
         return d;
      else if (is_container(d) && (d = find_mangled_decl(d, name)))
         return d;
   }

   const tree_kind_t kind = tree_kind(container);
   if (kind == T_ARCH || kind == T_ELAB || kind == T_BLOCK) {
      ident_t leaf = ident_rfrom(name, '.');
      const int nstmts = tree_stmts(container);
      for (int i = 0; i < nstmts; i++) {
         tree_t s = tree_stmt(container, i);
         const tree_kind_t skind = tree_kind(s);
         if (skind == T_PROCESS || skind == T_BLOCK) {
            if (tree_ident(s) == leaf)
               return s;

            tree_t d = find_mangled_decl(s, name);
            if (d != NULL)
               return d;
         }
      }
   }

   return NULL;
}

tree_t std_func(ident_t mangled)
{
   return find_mangled_decl(cached_std(NULL), mangled);
}

tree_t name_to_ref(tree_t name)
{
   tree_kind_t kind;
   while ((kind = tree_kind(name)) != T_REF) {
      switch (kind) {
      case T_ARRAY_REF:
      case T_ARRAY_SLICE:
      case T_RECORD_REF:
      case T_ALL:
         name = tree_value(name);
         break;
      default:
         return NULL;
      }
   }

   return name;
}

const char *port_mode_str(port_mode_t mode)
{
   const char *mode_str[] = {
      "INVALID", "IN", "OUT", "INOUT", "BUFFER", "LINKAGE"
   };
   assert(mode < ARRAY_LEN(mode_str));
   return mode_str[mode];
}

const char *ename_kind_str(ename_kind_t kind)
{
   const char *kind_str[] = { ".", "", "@", "!" };
   assert(kind < ARRAY_LEN(kind_str));
   return kind_str[kind];
}

void mangle_one_type(text_buf_t *buf, type_t type)
{
   ident_t ident = type_ident(type);

   if (icmp(ident, "STD.STANDARD.INTEGER"))
      tb_printf(buf, "I");
   else if (icmp(ident, "STD.STANDARD.STRING"))
      tb_printf(buf, "S");
   else if (icmp(ident, "STD.STANDARD.REAL"))
      tb_printf(buf, "R");
   else if (icmp(ident, "STD.STANDARD.BOOLEAN"))
      tb_printf(buf, "B");
   else if (icmp(ident, "STD.STANDARD.CHARACTER"))
      tb_printf(buf, "C");
   else if (icmp(ident, "STD.STANDARD.TIME"))
      tb_printf(buf, "T");
   else if (icmp(ident, "STD.STANDARD.NATURAL"))
      tb_printf(buf, "N");
   else if (icmp(ident, "STD.STANDARD.POSITIVE"))
      tb_printf(buf, "P");
   else if (icmp(ident, "STD.STANDARD.BIT"))
      tb_printf(buf, "J");
   else if (icmp(ident, "STD.STANDARD.BIT_VECTOR"))
      tb_printf(buf, "Q");
   else if (icmp(ident, "IEEE.STD_LOGIC_1164.STD_LOGIC"))
      tb_printf(buf, "L");
   else if (icmp(ident, "IEEE.STD_LOGIC_1164.STD_ULOGIC"))
      tb_printf(buf, "U");
   else if (icmp(ident, "IEEE.STD_LOGIC_1164.STD_LOGIC_VECTOR"))
      tb_printf(buf, "V");
   else if (icmp(ident, "IEEE.STD_LOGIC_1164.STD_ULOGIC_VECTOR"))
      tb_printf(buf, "Y");
   else {
      const char *ident_str = istr(ident);
      tb_printf(buf, "%d%s", (int)strlen(ident_str), ident_str);
   }
}

tree_t primary_unit_of(tree_t unit)
{
   switch (tree_kind(unit)) {
   case T_ENTITY:
   case T_COMPONENT:
   case T_PACKAGE:
      return unit;
   case T_ARCH:
   case T_CONFIGURATION:
   case T_PACK_BODY:
      return tree_primary(unit);
   default:
      fatal_trace("invalid kind %s in primary_unit_of",
                  tree_kind_str(tree_kind(unit)));
   }
}

static unsigned encode_case_choice_at_depth(tree_t value, int depth)
{
   switch (tree_kind(value)) {
   case T_LITERAL:
      {
         assert(tree_subkind(value) == L_STRING);
         tree_t ch = tree_char(value, depth);
         return tree_pos(tree_ref(ch));
      }
      break;

   case T_AGGREGATE:
      {
         const int nassocs = tree_assocs(value);
         type_t type = tree_type(value);

         for (int i = 0; i < nassocs; i++) {
            tree_t a = tree_assoc(value, i);
            switch (tree_subkind(a)) {
            case A_NAMED:
               if (rebase_index(type, 0, assume_int(tree_name(a))) == depth)
                  return assume_int(tree_value(a));
               break;

            case A_POS:
               if (tree_pos(a) == (unsigned)depth)
                  return assume_int(tree_value(a));
               break;

            case A_OTHERS:
               return assume_int(tree_value(a));
            }
         }
      }
      break;

   case T_REF:
      {
         tree_t decl = tree_ref(value);
         assert(tree_kind(decl) == T_CONST_DECL);
         assert(tree_has_value(decl));
         return encode_case_choice_at_depth(tree_value(decl), depth);
      }
      break;

   case T_ARRAY_SLICE:
      {
         tree_t base = tree_value(value);
         tree_t r = tree_range(value, 0);
         const int64_t rleft = assume_int(tree_left(r));
         const int64_t offset = rebase_index(tree_type(base), 0, rleft);
         return encode_case_choice_at_depth(base, depth + offset);
      }

   case T_FCALL:
      if (tree_subkind(tree_ref(value)) == S_CONCAT) {
         const int nparams = tree_params(value);
         for (int i = 0; i < nparams; i++) {
            tree_t left = tree_value(tree_param(value, i));

            tree_t lr = range_of(tree_type(left), 0);
            int64_t left_len;
            if (!folded_length(lr, &left_len))
               fatal_at(tree_loc(left), "cannot determine length of left hand "
                        "side of concatenation");

            if (depth < left_len || i + 1 == nparams)
               return encode_case_choice_at_depth(left, depth);

            depth -= left_len;
         }
      }
      // Fall-through

   default:
      fatal_at(tree_loc(value), "unsupported tree type %s in case choice",
               tree_kind_str(tree_kind(value)));
   }

   fatal_at(tree_loc(value), "cannot find element %d in choice", depth);
}

int64_t encode_case_choice(tree_t value, int length, int bits)
{
   uint64_t enc = 0;
   for (int i = 0; i < length; i++) {
      if (bits > 0) {
         enc <<= bits;
         enc |= encode_case_choice_at_depth(value, i);
      }
      else {
         enc *= 0x27d4eb2d;
         enc += encode_case_choice_at_depth(value, i);
      }
   }

   return enc;
}

void to_string(text_buf_t *tb, type_t type, int64_t value)
{
   if (type_is_integer(type))
      tb_printf(tb, "%"PRIi64, value);
   else if (type_is_enum(type)) {
      type_t base = type_base_recur(type);
      if (value < 0 || value >= type_enum_literals(base))
         tb_printf(tb, "%"PRIi64, value);
      else
         tb_cat(tb, istr(tree_ident(type_enum_literal(base, value))));
   }
   else if (type_is_physical(type)) {
      type_t base = type_base_recur(type);
      const unsigned nunits = type_units(base);
      tree_t max_unit = NULL;
      int64_t max_unit_value = 0;

      // Find the largest unit that evenly divides the given value
      for (unsigned u = 0; u < nunits; u++) {
         tree_t unit = type_unit(base, u);
         const int64_t unit_value = assume_int(tree_value(unit));
         if ((unit_value > max_unit_value) && (value % unit_value == 0)) {
            max_unit = unit;
            max_unit_value = unit_value;
         }
      }
      assert(max_unit);

      tb_printf(tb, "%"PRIi64" %s", value / max_unit_value,
                istr(tree_ident(max_unit)));
   }
   else if (type_is_real(type)) {
      union { int64_t i; double r; } u = { .i = value };
      tb_printf(tb, "%.17g", u.r);
   }
   else if (type_is_access(type)) {
      if (value == 0)
         tb_cat(tb, "NULL");
      else
         tb_printf(tb, "%p", (void *)value);
   }
}

static bool is_static(tree_t expr)
{
   switch (tree_kind(expr)) {
   case T_REF:
      {
         tree_t decl = tree_ref(expr);
         switch (tree_kind(decl)) {
         case T_CONST_DECL:
         case T_UNIT_DECL:
         case T_ENUM_LIT:
         case T_GENERIC_DECL:
            return true;
         case T_ALIAS:
            return is_static(tree_value(decl));
         default:
            return false;
         }
      }

   case T_LITERAL:
      return true;

   default:
      return false;
   }
}

tree_t longest_static_prefix(tree_t expr)
{
   switch (tree_kind(expr)) {
   case T_ARRAY_REF:
      {
         tree_t value = tree_value(expr);
         tree_t prefix = longest_static_prefix(tree_value(expr));

         if (prefix != value)
            return prefix;

         const int nparams = tree_params(expr);
         for (int i = 0; i < nparams; i++) {
            if (!is_static(tree_value(tree_param(expr, i))))
               return prefix;
         }

         return expr;
      }

   case T_ARRAY_SLICE:
      {
         tree_t value = tree_value(expr);
         tree_t prefix = longest_static_prefix(tree_value(expr));

         if (prefix != value)
            return prefix;

         const int nranges = tree_ranges(expr);
         for (int i = 0; i < nranges; i++) {
            tree_t r = tree_range(expr, i);
            if (!is_static(tree_left(r)) || !is_static(tree_right(r)))
               return prefix;
         }

         return expr;
      }

   case T_RECORD_REF:
      {
         tree_t value = tree_value(expr);
         tree_t prefix = longest_static_prefix(tree_value(expr));

         if (prefix != value)
            return prefix;

         return expr;
      }

   default:
      return expr;
   }
}

tree_t body_of(tree_t pack)
{
   const tree_kind_t kind = tree_kind(pack);
   if (kind == T_PACK_INST)
      return NULL;

   assert(tree_kind(pack) == T_PACKAGE);

   ident_t body_i = well_known(W_BODY);
   ident_t body_name = ident_prefix(tree_ident(pack), body_i, '-');
   return lib_get_qualified(body_name);
}
