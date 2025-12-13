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
#include "common.h"
#include "diag.h"
#include "hash.h"
#include "ident.h"
#include "lib.h"
#include "lower.h"
#include "option.h"
#include "phase.h"
#include "scan.h"
#include "thread.h"
#include "type.h"
#include "vlog/vlog-phase.h"
#include "sdf/sdf-phase.h"
#include "sdf/sdf-util.h"

#include <assert.h>
#include <ctype.h>
#include <string.h>
#include <stdlib.h>
#include <inttypes.h>

static vhdl_standard_t  current_std  = STD_08;
static bool             have_set_std = false;
static ident_t          id_cache[NUM_WELL_KNOWN];
static text_buf_t      *syntax_buf = NULL;

int64_t assume_int(tree_t t)
{
   int64_t value;
   if (folded_int(t, &value))
      return value;

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
   case T_REF:
      if (tree_has_ref(t)) {
         tree_t decl = tree_ref(t);
         switch (tree_kind(decl)) {
         case T_CONST_DECL:
            if (tree_has_value(decl))
               return folded_int(tree_value(decl), l);
            else
               return false;
         case T_ENUM_LIT:
            *l = tree_pos(decl);
            return true;
         case T_ALIAS:
            return folded_int(tree_value(decl), l);
         default:
            return false;
         }
      }
      // Fall-through
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
   if (!folded_int(tree_left(r), &left))
      return false;
   else if (!folded_int(tree_right(r), &right))
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

tree_t get_discrete_lit(tree_t t, type_t type, int64_t i)
{
   if (type_is_enum(type)) {
      type_t base = type_base_recur(type);
      const int maxlit = type_enum_literals(base);
      if (i >= maxlit)
         return NULL;
      else
         return get_enum_lit(t, type, i);
   }
   else
      return get_int_lit(t, type, i);
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

bool parse_value(type_t type, const char *str, parsed_value_t *value)
{
   type_t base = type_base_recur(type);
   const type_kind_t basek = type_kind(base);

   if (basek == T_ARRAY && type_is_character_array(base)) {
      value->enums = NULL;

      int map[256];
      for (int i = 0; i < ARRAY_LEN(map); i++)
         map[i] = INT_MAX;

      type_t elem = type_elem(base);

      const int nlits = type_enum_literals(elem);
      for (int i = 0; i < nlits; i++) {
         ident_t id = tree_ident(type_enum_literal(elem, i));
         if (ident_char(id, 0) == '\'')
            map[(uint8_t)ident_char(id, 1)] = i;
      }

      while (map[(uint8_t)*str] == INT_MAX && isspace_iso88591(*str))
         ++str;

      int base = 0, dbits = 1;
      bool quoted = false;
      if (map['\"'] == INT_MAX) {
         if (str[0] == '\"') {
            quoted = true;
            str++;
         }
         else if (str[1] == '\"' && toupper_iso88591(str[0]) == 'X') {
            quoted = true;
            base = 16;
            dbits = 4;
            str += 2;
         }
      }

      const size_t max = strlen(str) * dbits;
      enum_array_t *array = xmalloc_flex(sizeof(enum_array_t), max, 1);

      int n = 0, m;
      for (; *str != '\0'; str++, n += dbits) {
         if (base > 0) {
            const bool extended = (isdigit_iso88591(*str) && *str < '0' + base)
               || (base > 10 && *str >= 'A' && *str < 'A' + base - 10)
               || (base > 10 && *str >= 'a' && *str < 'a' + base - 10);

            if (!extended)
               break;

            int digit = isdigit_iso88591(*str)
               ? (*str - '0')
               : 10 + (toupper_iso88591(*str) - 'A');

            bool valid = true;
            for (int i = dbits - 1; i >= 0; i--) {
               const uint8_t bit = (digit >> i) & 1 ? '1' : '0';
               valid &= (m = map[bit]) != INT_MAX;
               array->values[n + dbits - i - 1] = m;
            }

            if (!valid)
               break;
         }
         else if ((m = map[(uint8_t)*str]) == INT_MAX)
            break;
         else
            array->values[n] = m;
      }

      assert(n <= max);
      array->count = n;

      if (quoted && *str++ != '\"') {
         free(array);
         return false;
      }

      for (; *str; str++) {
         if (!isspace_iso88591(*str)) {
            free(array);
            return false;
         }
      }

      value->enums = array;
      return true;
   }

   while (isspace_iso88591(*str))
      ++str;

   switch (basek) {
   case T_INTEGER:
      {
         const bool is_negative = *str == '-';
         int num_digits = 0;

         if (is_negative) ++str;

         int64_t sum = 0;
         for (; isdigit_iso88591(*str) || (*str == '_'); str++) {
            if (*str != '_') {
               sum *= 10;
               sum += (*str - '0');
               num_digits++;
            }
         }

         value->integer = is_negative ? -sum : sum;

         if (num_digits == 0)
            return false;
      }
      break;

   case T_ENUM:
      {
         bool upcase = true;
         char *copy LOCAL = xstrdup(str), *p;
         for (p = copy; (*p != '\0') && !isspace_iso88591(*p); p++, str++) {
            if (*p == '\'')
               upcase = false;
            if (upcase)
               *p = toupper_iso88591(*p);
         }
         *p = '\0';

         ident_t id = ident_new(copy);

         value->integer = -1;

         const int nlits = type_enum_literals(base);
         for (int i = 0; i < nlits; i++) {
            if (tree_ident(type_enum_literal(base, i)) == id) {
               value->integer = i;
               break;
            }
         }

         if (value->integer == -1)
            return false;
      }
      break;

   case T_REAL:
      {
         char *eptr = NULL;
         value->real = strtod(str, &eptr);
         str = eptr;
      }
      break;

   case T_PHYSICAL:
      {
         char *eptr = NULL;
         double scale = strtod(str, &eptr);
         str = eptr;

         while (isspace_iso88591(*str)) ++str;

         char *copy LOCAL = xstrdup(str), *p;
         for (p = copy; *p && !isspace_iso88591(*p); p++, str++)
            *p = toupper_iso88591(*p);
         *p = '\0';

         if (p == copy)
            return false;

         ident_t id = ident_new(copy);

         value->integer = -1;

         const int nunits = type_units(base);
         for (int i = 0; i < nunits; i++) {
            tree_t u = type_unit(base, i);
            if (tree_ident(u) == id) {
               value->integer = scale * assume_int(tree_value(u));
               break;
            }
         }

         if (value->integer == -1)
            return false;
      }
      break;

   default:
      return false;
   }

   for (; *str; str++) {
      if (!isspace_iso88591(*str))
         return false;
   }

   return true;
}

tree_t make_ref(tree_t to)
{
   tree_t t = tree_new(T_REF);
   tree_set_ident(t, tree_ident(to));
   tree_set_ref(t, to);
   tree_set_type(t, tree_type(to));
   tree_set_loc(t, tree_loc(to));
   return t;
}

vhdl_standard_t standard(void)
{
   return current_std;
}

void set_standard(vhdl_standard_t s)
{
   current_std = s;
   have_set_std = true;
}

void set_default_standard(vhdl_standard_t s)
{
   if (!have_set_std)
      set_standard(s);
}

const char *standard_text(vhdl_standard_t s)
{
   static const char *text[] = {
      "1987", "1993", "2000", "2002", "2008", "2019"
   };

   if ((unsigned)s < ARRAY_LEN(text))
      return text[s];
   else
      return "????";
}

tree_t find_element_mode_indication(tree_t view, tree_t field, bool *converse)
{
   switch (tree_kind(view)) {
   case T_REF:
      return find_element_mode_indication(tree_ref(view), field, converse);

   case T_ALIAS:
      return find_element_mode_indication(tree_value(view), field, converse);

   case T_ATTR_REF:
      assert(tree_subkind(view) == ATTR_CONVERSE);
      *converse = !*converse;
      return find_element_mode_indication(tree_name(view), field, converse);

   case T_VIEW_DECL:
      {
         type_t view_type = tree_type(view);
         assert(type_kind(view_type) == T_VIEW);

         const int nelems = type_fields(view_type);
         for (int i = 0; i < nelems; i++) {
            tree_t e = type_field(view_type, i);
            if (tree_ref(e) == field)
               return e;
         }

         return NULL;
      }

   default:
      fatal_trace("unhandled tree kind %s in find_element_mode_indication",
                  tree_kind_str(tree_kind(view)));
   }
}

port_mode_t converse_mode(tree_t port, bool converse)
{
   const port_mode_t mode = tree_subkind(port);
   switch (mode) {
   case PORT_IN: return converse ? PORT_OUT : PORT_IN;
   case PORT_OUT: return converse ? PORT_IN : PORT_OUT;
   default: return mode;
   }
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
   case T_STRING:
      return C_LITERAL;
   case T_FIELD_DECL:
   case T_ATTR_DECL:
      return C_DEFAULT;
   case T_VIEW_DECL:
      return C_VIEW;
   case T_UNIT_DECL:
      return C_UNITS;
   case T_ARCH:
      return C_ARCHITECTURE;
   case T_FUNC_DECL:
   case T_FUNC_BODY:
   case T_FUNC_INST:
   case T_FCALL:
   case T_PROT_FCALL:
      return C_FUNCTION;
   case T_PROC_DECL:
   case T_PROC_BODY:
   case T_PROC_INST:
   case T_PCALL:
   case T_PROT_PCALL:
      return C_PROCEDURE;
   case T_ENTITY:
      return C_ENTITY;
   case T_SUBTYPE_DECL:
      return C_SUBTYPE;
   case T_TYPE_DECL:
   case T_PROT_DECL:
   case T_PROT_BODY:
      return C_TYPE;
   case T_FILE_DECL:
      return C_FILE;
   case T_PROCESS:
   case T_BLOCK:
   case T_FOR:
   case T_INSTANCE:
   case T_CONCURRENT:
   case T_ELAB:
   case T_PSL_DECL:
   case T_PSL_DIRECT:
   case T_FOR_GENERATE:
   case T_IF_GENERATE:
   case T_CASE_GENERATE:
      return C_LABEL;
   case T_COMPONENT:
      return C_COMPONENT;
   case T_REF:
   case T_PROT_REF:
      return tree_has_ref(t) ? class_of(tree_ref(t)) : C_DEFAULT;
   case T_ARRAY_REF:
   case T_ARRAY_SLICE:
   case T_RECORD_REF:
   case T_ALL:
   case T_ALIAS:
   case T_QUALIFIED:
      return class_of(tree_value(t));
   case T_PACKAGE:
   case T_PACK_BODY:
   case T_PACK_INST:
      return C_PACKAGE;
   case T_CONFIGURATION:
      return C_CONFIGURATION;
   case T_LIBRARY:
      return C_LIBRARY;
   case T_ATTR_REF:
      switch (tree_subkind(t)) {
      case ATTR_DELAYED:
      case ATTR_STABLE:
      case ATTR_QUIET:
      case ATTR_TRANSACTION:
         return C_SIGNAL;
      default:
         return C_DEFAULT;
      }
   case T_CONTEXT:
      return C_CONTEXT;
   default:
      fatal_trace("missing class_of for %s", tree_kind_str(tree_kind(t)));
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
      "type", "subtype", "label", "procedure", "literal", "units", "library",
      "context", "view",
   };
   assert(c < ARRAY_LEN(strs));
   return strs[c];
}

const char *assoc_kind_str(assoc_kind_t akind)
{
   switch (akind) {
   case A_NAMED:  return "named";
   case A_CONCAT:
   case A_POS:    return "positional";
   case A_OTHERS: return "others";
   case A_SLICE:
   case A_RANGE:  return "range";
   default:       return "??";
   }
}

bool is_subprogram(tree_t t)
{
   switch (tree_kind(t)) {
   case T_FUNC_DECL:
   case T_FUNC_BODY:
   case T_FUNC_INST:
   case T_PROC_DECL:
   case T_PROC_BODY:
   case T_PROC_INST:
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
   return kind == T_WHILE || kind == T_FOR || kind == T_LOOP;
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
   case T_FOR:
   case T_PROCESS:
   case T_PACK_INST:
      return true;
   default:
      return false;
   }
}

bool is_concurrent_block(tree_t t)
{
   switch (tree_kind(t)) {
   case T_ARCH:
   case T_ENTITY:
   case T_BLOCK:
   case T_IF_GENERATE:
   case T_FOR_GENERATE:
   case T_CASE_GENERATE:
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

bool is_literal(tree_t t)
{
   switch (tree_kind(t)) {
   case T_REF:
      return tree_has_ref(t) && tree_kind(tree_ref(t)) == T_ENUM_LIT;
   case T_LITERAL:
      return true;
   case T_STRING:
   default:
      return false;
   }
}

bool is_body(tree_t t)
{
   switch (tree_kind(t)) {
   case T_FUNC_BODY:
   case T_PROC_BODY:
   case T_PACK_BODY:
   case T_PROT_BODY:
      return true;
   default:
      return false;
   }
}

bool is_guarded_signal(tree_t decl)
{
   switch (tree_kind(decl)) {
   case T_PORT_DECL:
   case T_SIGNAL_DECL:
      return !!(tree_flags(decl) & (TREE_F_BUS | TREE_F_REGISTER));
   default:
      return false;
   }
}

bool is_type_decl(tree_t t)
{
   switch (tree_kind(t)) {
   case T_TYPE_DECL:
   case T_SUBTYPE_DECL:
   case T_PROT_DECL:
   case T_PROT_BODY:
      return true;
   default:
      return false;
   }
}

tree_t aliased_type_decl(tree_t decl)
{
   switch (tree_kind(decl)) {
   case T_ALIAS:
      {
         tree_t value = tree_value(decl);
         const tree_kind_t kind = tree_kind(value);
         if (kind == T_REF && tree_has_ref(value))
            return aliased_type_decl(tree_ref(value));
         else if (kind == T_ATTR_REF && is_type_attribute(tree_subkind(value)))
             return value;
         else
            return NULL;
      }
   case T_TYPE_DECL:
   case T_SUBTYPE_DECL:
   case T_PROT_DECL:
   case T_PROT_BODY:
      return decl;
   case T_GENERIC_DECL:
      if (tree_class(decl) == C_TYPE)
         return decl;
      else
         return NULL;
   default:
      return NULL;
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
      const int nindex = type_indexes(array);
      assert(from_dim < nindex);

      type_t type = type_new(T_ARRAY);
      type_set_ident(type, type_ident(array));
      type_set_elem(type, type_elem(array));

      for (int i = from_dim; i < nindex; i++)
         type_add_index(type, type_index(array, i));

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
      type_set_constraint(sub, constraint);

      for (int i = from_dim; i < ndims; i++) {
         tree_t r = range_of(array, i);
         if (r == NULL) {
            // Not enough constraints were provided for the type
            r = tree_new(T_RANGE);
            tree_set_subkind(r, RANGE_ERROR);
            tree_set_type(r, type_new(T_NONE));
         }

         type_add_index(base, tree_type(r));
         tree_add_range(constraint, r);
      }

      return sub;
   }
}

unsigned bits_for_range(int64_t low, int64_t high)
{
   if (low > high)
      return 0;   // Null range
   else if (low < 0) {
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
      return dimension_of(type_base(type));
   case T_GENERIC:
      switch (type_subkind(type)) {
      case GTYPE_ARRAY:
         return type_indexes(type);
      case GTYPE_ACCESS:
         return dimension_of(type_designated(type));
      case GTYPE_FILE:
      case GTYPE_PRIVATE:
         return 0;
      default:
         return 1;
      }
   case T_ARRAY:
      return type_indexes(type);
   case T_NONE:
   case T_RECORD:
   case T_INCOMPLETE:
   case T_FILE:
   case T_SIGNATURE:
      return 0;
   case T_INTEGER:
   case T_REAL:
   case T_PHYSICAL:
   case T_ENUM:
      return type_dims(type);
   case T_ACCESS:
      return dimension_of(type_designated(type));
   default:
      fatal_trace("invalid type kind %s in dimension_of",
                  type_kind_str(type_kind(type)));
   }
}

tree_t range_of(type_t type, unsigned dim)
{
   switch (type_kind(type)) {
   case T_SUBTYPE:
      if (type_has_constraint(type)) {
         tree_t c = type_constraint(type);
         switch (tree_subkind(c)) {
         case C_INDEX:
         case C_RANGE:
            if (dim < tree_ranges(c))
               return tree_range(c, dim);
            else
               return NULL;   // Must be an error
         default:
            should_not_reach_here();
         }
      }
      else
         return range_of(type_base(type), dim);
   case T_INTEGER:
   case T_REAL:
   case T_PHYSICAL:
   case T_ENUM:
      return type_dim(type, dim);
   default:
      fatal_trace("invalid type kind %s for %s in range_of",
                  type_kind_str(type_kind(type)), type_pp(type));
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
            if (tree_kind(name) == T_REF && tree_has_ref(name)) {
               tree_t decl = tree_ref(name);
               if (is_type_decl(decl))
                  return direction_of(tree_type(decl), 0);
            }
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
      return type_index(base, dim);
   else if (base_kind == T_ENUM || base_kind == T_NONE)
      return type;
   else if (base_kind == T_GENERIC && type_subkind(base) == GTYPE_ARRAY)
      return type_index(base, dim);
   else if (base_kind == T_RECORD || base_kind == T_GENERIC)
      return NULL;
   else if (base_kind == T_ACCESS)
      return index_type_of(type_designated(type), dim);
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
   id_cache[W_STD_STANDARD]    = ident_new("STD.STANDARD");
   id_cache[W_ALL]             = ident_new("all");
   id_cache[W_STD_BIT]         = ident_new("STD.STANDARD.BIT");
   id_cache[W_STD_BOOL]        = ident_new("STD.STANDARD.BOOLEAN");
   id_cache[W_STD_CHAR]        = ident_new("STD.STANDARD.CHARACTER");
   id_cache[W_STD_NATURAL]     = ident_new("STD.STANDARD.NATURAL");
   id_cache[W_STD_POSITIVE]    = ident_new("STD.STANDARD.POSITIVE");
   id_cache[W_STD_INTEGER]     = ident_new("STD.STANDARD.INTEGER");
   id_cache[W_STD_STRING]      = ident_new("STD.STANDARD.STRING");
   id_cache[W_STD_REAL]        = ident_new("STD.STANDARD.REAL");
   id_cache[W_STD_TIME]        = ident_new("STD.STANDARD.TIME");
   id_cache[W_STD_BIT_VECTOR]  = ident_new("STD.STANDARD.BIT_VECTOR");
   id_cache[W_IEEE_SIGNED]     = ident_new("IEEE.NUMERIC_STD.SIGNED");
   id_cache[W_IEEE_UNSIGNED]   = ident_new("IEEE.NUMERIC_STD.UNSIGNED");
   id_cache[W_IEEE_LOGIC]      = ident_new("IEEE.STD_LOGIC_1164.STD_LOGIC");
   id_cache[W_IEEE_ULOGIC]     = ident_new("IEEE.STD_LOGIC_1164.STD_ULOGIC");
   id_cache[W_IEEE_1164_AND]   = ident_new("IEEE.STD_LOGIC_1164.\"and\"");
   id_cache[W_IEEE_1164_NAND]  = ident_new("IEEE.STD_LOGIC_1164.\"nand\"");
   id_cache[W_IEEE_1164_OR]    = ident_new("IEEE.STD_LOGIC_1164.\"or\"");
   id_cache[W_IEEE_1164_NOR]   = ident_new("IEEE.STD_LOGIC_1164.\"nor\"");
   id_cache[W_IEEE_1164_XOR]   = ident_new("IEEE.STD_LOGIC_1164.\"xor\"");
   id_cache[W_IEEE_1164_XNOR]  = ident_new("IEEE.STD_LOGIC_1164.\"xnor\"");
   id_cache[W_FOREIGN]         = ident_new("FOREIGN");
   id_cache[W_WORK]            = ident_new("WORK");
   id_cache[W_STD]             = ident_new("STD");
   id_cache[W_THUNK]           = ident_new("thunk");
   id_cache[W_BODY]            = ident_new("body");
   id_cache[W_CARET]           = ident_new("^");
   id_cache[W_IEEE]            = ident_new("IEEE");
   id_cache[W_IEEE_1164]       = ident_new("IEEE.STD_LOGIC_1164");
   id_cache[W_ERROR]           = ident_new("$error");
   id_cache[W_ELAB]            = ident_new("elab");
   id_cache[W_NUMERIC_STD]     = ident_new("IEEE.NUMERIC_STD");
   id_cache[W_NUMERIC_BIT]     = ident_new("IEEE.NUMERIC_BIT");
   id_cache[W_NVC]             = ident_new("NVC");
   id_cache[W_DEFAULT_CLOCK]   = ident_new("default clock");
   id_cache[W_STD_REFLECTION]  = ident_new("STD.REFLECTION");
   id_cache[W_NEVER_WAITS]     = ident_new("NEVER_WAITS");
   id_cache[W_NVC_VERILOG]     = ident_new("NVC.VERILOG");
   id_cache[W_NVC_PSL_SUPPORT] = ident_new("NVC.PSL_SUPPORT");
   id_cache[W_INSTANCE_NAME]   = ident_new("instance_name");
   id_cache[W_PATH_NAME]       = ident_new("path_name");
   id_cache[W_VITAL]           = ident_new("VITAL");
   id_cache[W_RESOLUTION]      = ident_new("resolution");
   id_cache[W_TEXT_UTIL]       = ident_new("NVC.TEXT_UTIL");
   id_cache[W_VERILOG_LOGIC]   = ident_new("NVC.VERILOG.T_LOGIC");
   id_cache[W_DLR_SIGNED]      = ident_new("$signed");
   id_cache[W_DLR_CLOG2]       = ident_new("$clog2");

   id_cache[W_IEEE_LOGIC_VECTOR] =
      ident_new("IEEE.STD_LOGIC_1164.STD_LOGIC_VECTOR");
   id_cache[W_IEEE_ULOGIC_VECTOR] =
      ident_new("IEEE.STD_LOGIC_1164.STD_ULOGIC_VECTOR");
   id_cache[W_IEEE_1164_RISING_EDGE] =
      ident_new("IEEE.STD_LOGIC_1164.RISING_EDGE(sU)B");
   id_cache[W_IEEE_1164_FALLING_EDGE] =
      ident_new("IEEE.STD_LOGIC_1164.FALLING_EDGE(sU)B");

   id_cache[W_NUMERIC_STD_UNSIGNED] = ident_new("IEEE.NUMERIC_STD_UNSIGNED");
   id_cache[W_NUMERIC_BIT_UNSIGNED] = ident_new("IEEE.NUMERIC_BIT_UNSIGNED");
   id_cache[W_VERILOG_NET_VALUE]    = ident_new("NVC.VERILOG.T_NET_VALUE");
   id_cache[W_VERILOG_WIRE_ARRAY]   = ident_new("NVC.VERILOG.T_WIRE_ARRAY");

   id_cache[W_OP_CCONV]               = ident_new("\"??\"");
   id_cache[W_OP_AND]                 = ident_new("\"and\"");
   id_cache[W_OP_OR]                  = ident_new("\"or\"");
   id_cache[W_OP_NAND]                = ident_new("\"nand\"");
   id_cache[W_OP_NOR]                 = ident_new("\"nor\"");
   id_cache[W_OP_XOR]                 = ident_new("\"xor\"");
   id_cache[W_OP_XNOR]                = ident_new("\"xnor\"");
   id_cache[W_OP_EQUAL]               = ident_new("\"=\"");
   id_cache[W_OP_NOT_EQUAL]           = ident_new("\"/=\"");
   id_cache[W_OP_LESS_THAN]           = ident_new("\"<\"");
   id_cache[W_OP_LESS_EQUAL]          = ident_new("\"<=\"");
   id_cache[W_OP_GREATER_THAN]        = ident_new("\">\"");
   id_cache[W_OP_GREATER_EQUAL]       = ident_new("\">=\"");
   id_cache[W_OP_MATCH_EQUAL]         = ident_new("\"?=\"");
   id_cache[W_OP_MATCH_NOT_EQUAL]     = ident_new("\"?/=\"");
   id_cache[W_OP_MATCH_LESS_THAN]     = ident_new("\"?<\"");
   id_cache[W_OP_MATCH_LESS_EQUAL]    = ident_new("\"?<=\"");
   id_cache[W_OP_MATCH_GREATER_THAN]  = ident_new("\"?>\"");
   id_cache[W_OP_MATCH_GREATER_EQUAL] = ident_new("\"?>=\"");
   id_cache[W_OP_SLL]                 = ident_new("\"sll\"");
   id_cache[W_OP_SRL]                 = ident_new("\"srl\"");
   id_cache[W_OP_SLA]                 = ident_new("\"sla\"");
   id_cache[W_OP_SRA]                 = ident_new("\"sra\"");
   id_cache[W_OP_ROL]                 = ident_new("\"rol\"");
   id_cache[W_OP_ROR]                 = ident_new("\"ror\"");
   id_cache[W_OP_ADD]                 = ident_new("\"+\"");
   id_cache[W_OP_MINUS]               = ident_new("\"-\"");
   id_cache[W_OP_CONCAT]              = ident_new("\"&\"");
   id_cache[W_OP_TIMES]               = ident_new("\"*\"");
   id_cache[W_OP_DIVIDE]              = ident_new("\"/\"");
   id_cache[W_OP_MOD]                 = ident_new("\"mod\"");
   id_cache[W_OP_REM]                 = ident_new("\"rem\"");
   id_cache[W_OP_EXPONENT]            = ident_new("\"**\"");
   id_cache[W_OP_ABS]                 = ident_new("\"abs\"");
   id_cache[W_OP_NOT]                 = ident_new("\"not\"");
}

bool is_uninstantiated_package(tree_t pack)
{
   return tree_kind(pack) == T_PACKAGE
      && tree_generics(pack) > 0
      && tree_genmaps(pack) == 0;
}

bool is_uninstantiated_subprogram(tree_t decl)
{
   switch (tree_kind(decl)) {
   case T_FUNC_DECL:
   case T_FUNC_BODY:
   case T_PROC_DECL:
   case T_PROC_BODY:
      return tree_generics(decl) > 0;
   default:
      return false;
   }
}

bool is_anonymous_subtype(type_t type)
{
   return type_kind(type) == T_SUBTYPE && !type_has_ident(type);
}

bool unit_needs_cgen(tree_t unit)
{
   switch (tree_kind(unit)) {
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
      const tree_kind_t dkind = tree_kind(d);
      if ((dkind == T_FUNC_DECL || dkind == T_PROC_DECL)
          && !(tree_flags(d) & TREE_F_PREDEFINED))
         return true;
      else if (dkind == T_CONST_DECL && !tree_has_value(d))
         return true;
      else if (dkind == T_PROT_DECL)
         return true;
   }

   return false;
}

static tree_t cached_unit(tree_t hint, tree_t *cache, well_known_t lib_name,
                          well_known_t unit_name)
{
   const vhdl_standard_t curr = standard();

   if (cache[curr] == NULL) {
      if (hint != NULL)
         cache[curr] = hint;
      else {
         lib_t std = lib_require(well_known(lib_name));
         cache[curr] = lib_get(std, well_known(unit_name));
         assert(cache[curr] != NULL);
      }
   }

   assert(hint == NULL || hint == cache[curr]);
   return cache[curr];
}

static tree_t cached_std(tree_t hint)
{
   static tree_t standard_cache[STD_19 + 1] = {};
   return cached_unit(hint, standard_cache, W_STD, W_STD_STANDARD);
}

static tree_t search_type_decls(tree_t container, ident_t name)
{
   const int ndecls = tree_decls(container);

   for (int i = 0; i < ndecls; i++) {
      tree_t d = tree_decl(container, i);
      if (is_type_decl(d) && tree_ident(d) == name)
         return d;
   }

   return NULL;
}

type_t std_type(tree_t std, std_type_t which)
{
   static type_t cache[STD_FILE_OPEN_STATE + 1] = {};
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
         "FILE_ORIGIN_KIND",
         "FILE_OPEN_STATE",
      };

      tree_t d = search_type_decls(cached_std(std), ident_new(names[which]));
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

type_t ieee_type(ieee_type_t which)
{
   static type_t cache[IEEE_STD_LOGIC_VECTOR + 1] = {};
   assert(which < ARRAY_LEN(cache));

   if (cache[which] == NULL) {
      static const char *const names[] = {
         [IEEE_STD_ULOGIC] = "STD_ULOGIC",
         [IEEE_STD_LOGIC] = "STD_LOGIC",
         [IEEE_STD_ULOGIC_VECTOR] = "STD_ULOGIC_VECTOR",
         [IEEE_STD_LOGIC_VECTOR] = "STD_LOGIC_VECTOR",
      };

      static tree_t ieee_cache[STD_19 + 1] = {};
      tree_t unit = cached_unit(NULL, ieee_cache, W_IEEE, W_IEEE_1164);

      tree_t d = search_type_decls(unit, ident_new(names[which]));
      if (d == NULL)
         fatal_trace("cannot find IEEE type %s", names[which]);

      // STD.STANDARD cannot depend on IEEE
      assert(!opt_get_int(OPT_BOOTSTRAP));

      return (cache[which] = tree_type(d));
   }
   else
      return cache[which];
}

static tree_t cached_verilog(void)
{
   static tree_t verilog_cache[STD_19 + 1] = {};
   return cached_unit(NULL, verilog_cache, W_NVC, W_NVC_VERILOG);
}

type_t verilog_type(verilog_type_t which)
{
   static type_t cache[VERILOG_WIRE_ARRAY + 1] = {};
   assert(which < ARRAY_LEN(cache));

   if (cache[which] == NULL) {
      static const char *const names[] = {
         [VERILOG_LOGIC] = "T_LOGIC",
         [VERILOG_LOGIC_ARRAY] = "T_LOGIC_ARRAY",
         [VERILOG_INT64] = "T_INT64",
         [VERILOG_NET_VALUE] = "T_NET_VALUE",
         [VERILOG_NET_ARRAY] = "T_NET_ARRAY",
         [VERILOG_WIRE] = "T_WIRE",
         [VERILOG_WIRE_ARRAY] = "T_WIRE_ARRAY",
      };

      tree_t d = search_type_decls(cached_verilog(), ident_new(names[which]));
      if (d == NULL)
         fatal_trace("cannot find NVC.VERILOG type %s", names[which]);

      // STD.STANDARD cannot depend on NVC.VERILOG
      assert(!opt_get_int(OPT_BOOTSTRAP));

      return (cache[which] = tree_type(d));
   }
   else
      return cache[which];
}

type_t reflection_type(reflect_type_t which)
{
   static type_t cache[REFLECT_SUBTYPE_MIRROR + 1] = {};
   assert(which < ARRAY_LEN(cache));

   if (cache[which] == NULL) {
      static const char *const names[] = {
         [REFLECT_VALUE_MIRROR] = "VALUE_MIRROR",
         [REFLECT_SUBTYPE_MIRROR] = "SUBTYPE_MIRROR",
      };

      static tree_t reflect_cache[STD_19 + 1] = {};
      tree_t unit = cached_unit(NULL, reflect_cache, W_STD, W_STD_REFLECTION);

      tree_t d = search_type_decls(unit, ident_new(names[which]));
      if (d == NULL)
         fatal_trace("cannot find REFLECTION type %s", names[which]);

      // STD.STANDARD cannot depend on REFLECTION
      assert(!opt_get_int(OPT_BOOTSTRAP));

      return (cache[which] = tree_type(d));
   }
   else
      return cache[which];
}

bool is_open_coded_builtin(subprogram_kind_t kind)
{
   switch (kind) {
   case S_ADD:
   case S_SUB:
   case S_DIV:
   case S_MUL:
   case S_MUL_PR:
   case S_MUL_RP:
   case S_MUL_PI:
   case S_MUL_IP:
   case S_DIV_PR:
   case S_DIV_PP:
   case S_DIV_PI:
   case S_IDENTITY:
   case S_NEGATE:
   case S_SCALAR_LT:
   case S_SCALAR_LE:
   case S_SCALAR_GT:
   case S_SCALAR_GE:
   case S_SCALAR_EQ:
   case S_SCALAR_NEQ:
   case S_ABS:
   case S_MOD:
   case S_REM:
   case S_EXP:
   case S_MUL_RI:
   case S_MUL_IR:
   case S_DIV_RI:
   case S_CONCAT:
   case S_SCALAR_AND:
   case S_SCALAR_OR:
   case S_SCALAR_NOT:
   case S_SCALAR_NAND:
   case S_SCALAR_NOR:
   case S_SCALAR_XOR:
   case S_SCALAR_XNOR:
   case S_FILE_OPEN1:
   case S_FILE_OPEN2:
   case S_FILE_READ:
   case S_FILE_WRITE:
   case S_DEALLOCATE:
      return true;
   default:
      return false;
   }
}

tree_t std_func(ident_t mangled)
{
   tree_t std = cached_std(NULL);

   const int ndecls = tree_decls(std);
   for (int i = 0; i < ndecls; i++) {
      tree_t d = tree_decl(std, i);
      if (is_subprogram(d) && tree_has_ident2(d) && tree_ident2(d) == mangled)
         return d;
   }

   return NULL;
}

tree_t verilog_func(ident_t mangled)
{
   tree_t pack = cached_verilog();

   const int ndecls = tree_decls(pack);
   for (int i = 0; i < ndecls; i++) {
      tree_t d = tree_decl(pack, i);
      if (is_subprogram(d) && tree_ident2(d) == mangled)
         return d;
   }

   fatal_trace("missing Verilog helper function %s", istr(mangled));
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
      "INVALID", "IN", "OUT", "INOUT", "BUFFER", "LINKAGE", "VIEW", "VIEW"
   };
   assert(mode < ARRAY_LEN(mode_str));
   return mode_str[mode];
}

void mangle_one_type(text_buf_t *buf, type_t type)
{
   ident_t ident = type_ident(type);

   char code = 0;
   switch (is_well_known(ident)) {
   case W_STD_INTEGER:        code = 'I'; break;
   case W_STD_STRING:         code = 'S'; break;
   case W_STD_REAL:           code = 'R'; break;
   case W_STD_BOOL:           code = 'B'; break;
   case W_STD_CHAR:           code = 'C'; break;
   case W_STD_TIME:           code = 'T'; break;
   case W_STD_NATURAL:        code = 'N'; break;
   case W_STD_POSITIVE:       code = 'P'; break;
   case W_STD_BIT:            code = 'J'; break;
   case W_STD_BIT_VECTOR:     code = 'Q'; break;
   case W_IEEE_LOGIC:         code = 'L'; break;
   case W_IEEE_ULOGIC:        code = 'U'; break;
   case W_IEEE_LOGIC_VECTOR:  code = 'V'; break;
   case W_IEEE_ULOGIC_VECTOR: code = 'Y'; break;
   default: break;
   }

   if (code)
      tb_append(buf, code);
   else {
      tb_printf(buf, "%zu", ident_len(ident));
      tb_istr(buf, ident);
   }
}

ident_t get_call_context(ident_t mangled)
{
   const char *str = istr(mangled), *p = str, *end = NULL;
   for (; *p; p++) {
      if (*p == '(') break;
      if (*p == '.') end = p;
   }
   assert(end != NULL);

   return ident_new_n(str, end - str);
}

tree_t primary_unit_of(tree_t unit)
{
   switch (tree_kind(unit)) {
   case T_ENTITY:
   case T_COMPONENT:
   case T_PACKAGE:
   case T_BLOCK:
   case T_ELAB:
   case T_PACK_INST:
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

unsigned get_case_choice_char(tree_t value, int depth)
{
   switch (tree_kind(value)) {
   case T_STRING:
      if (depth < tree_chars(value))
         return assume_int(tree_char(value, depth));
      else
         return ~0;   // Out of bounds

   case T_AGGREGATE:
      {
         const int nassocs = tree_assocs(value);
         type_t type = tree_type(value);

         for (int i = 0, pos = 0; i < nassocs; i++) {
            tree_t a = tree_assoc(value, i);
            switch (tree_subkind(a)) {
            case A_NAMED:
               if (rebase_index(type, 0, assume_int(tree_name(a))) == depth)
                  return assume_int(tree_value(a));
               break;

            case A_POS:
               if (pos++ == (unsigned)depth)
                  return assume_int(tree_value(a));
               break;

            case A_CONCAT:
               {
                  tree_t left = tree_value(a);

                  type_t left_type = tree_type(left);
                  if (type_is_unconstrained(left_type))
                     fatal_at(tree_loc(left), "sorry, this expression is not "
                              "currently supported in a case choice");

                  tree_t lr = range_of(tree_type(left), 0);
                  int64_t left_len;
                  if (!folded_length(lr, &left_len))
                     fatal_at(tree_loc(left), "cannot determine length of "
                              "aggregate element");

                  if (depth < pos + left_len)
                     return get_case_choice_char(left, depth - pos);

                  pos += left_len;
               }
               break;

            case A_OTHERS:
               return assume_int(tree_value(a));

            case A_SLICE:
               {
                  tree_t base = tree_value(a);
                  tree_t r = tree_range(a, 0);

                  const int64_t rleft = assume_int(tree_left(r));
                  const int64_t rright = assume_int(tree_right(r));

                  const int64_t loffset = rebase_index(type, 0, rleft);
                  const int64_t roffset = rebase_index(type, 0, rright);

                  if (depth >= loffset && depth <= roffset)
                     return get_case_choice_char(base, depth - loffset);
               }
            }
         }

         // This will produce an error during bounds checking
         return ~0;
      }

   case T_REF:
      {
         tree_t decl = tree_ref(value);
         assert(tree_kind(decl) == T_CONST_DECL || tree_kind(decl) == T_ALIAS);
         assert(tree_has_value(decl));
         return get_case_choice_char(tree_value(decl), depth);
      }

   case T_ARRAY_SLICE:
      {
         tree_t base = tree_value(value);
         tree_t r = tree_range(value, 0);
         const int64_t rleft = assume_int(tree_left(r));
         const int64_t offset = rebase_index(tree_type(base), 0, rleft);
         return get_case_choice_char(base, depth + offset);
      }

   default:
      fatal_at(tree_loc(value), "unsupported tree type %s in case choice",
               tree_kind_str(tree_kind(value)));
   }
}

int64_t encode_case_choice(tree_t value, int length, int bits)
{
   uint64_t enc = 0;
   for (int i = 0; i < length; i++) {
      if (bits > 0) {
         enc <<= bits;
         enc |= get_case_choice_char(value, i);
      }
      else {
         enc *= 0x27d4eb2d;
         enc += get_case_choice_char(value, i);
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
            return !!(tree_flags(decl) & TREE_F_GLOBALLY_STATIC);
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
   case T_STRING:
      return true;

   case T_FCALL:
      return !!(tree_flags(expr) & (TREE_F_LOCALLY_STATIC
                                    | TREE_F_GLOBALLY_STATIC));

   case T_RECORD_REF:
      return is_static(tree_value(expr));

   case T_ARRAY_REF:
      {
         if (!is_static(tree_value(expr)))
            return false;

         const int nparams = tree_params(expr);
         for (int i = 0; i < nparams; i++) {
            if (!is_static(tree_value(tree_param(expr, i))))
               return false;
         }

         return true;
      }

   case T_ARRAY_SLICE:
      {
         if (!is_static(tree_value(expr)))
            return false;

         assert(tree_ranges(expr) == 1);

         tree_t r = tree_range(expr, 0);
         if (!is_static(tree_left(r)) || !is_static(tree_right(r)))
            return false;

         return true;
      }

   case T_ATTR_REF:
      {
         switch (tree_subkind(expr)) {
         case ATTR_EVENT:
         case ATTR_ACTIVE:
         case ATTR_LAST_EVENT:
         case ATTR_LAST_ACTIVE:
         case ATTR_LAST_VALUE:
         case ATTR_DRIVING:
         case ATTR_DRIVING_VALUE:
         case ATTR_STABLE:
         case ATTR_QUIET:
            return false;
         case ATTR_POS:
         case ATTR_VAL:
         case ATTR_LEFTOF:
         case ATTR_RIGHTOF:
         case ATTR_SUCC:
         case ATTR_PRED:
         case ATTR_VALUE:
         case ATTR_IMAGE:
            assert(tree_params(expr) == 1);
            return is_static(tree_value(tree_param(expr, 0)));
         case ATTR_LENGTH:
         case ATTR_LEFT:
         case ATTR_RIGHT:
         case ATTR_LOW:
         case ATTR_HIGH:
            {
               tree_t ref = name_to_ref(tree_name(expr));
               if (ref == NULL)
                  return false;

               switch (tree_kind(tree_ref(ref))) {
               case T_GENERIC_DECL:
               case T_PORT_DECL:
               case T_SIGNAL_DECL:
                  return true;
               default:
                  return false;
               }
            }
         default:
            return true;
         }
      }

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
         tree_t prefix = longest_static_prefix(value);

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
         tree_t prefix = longest_static_prefix(value);

         if (prefix != value)
            return prefix;

         assert(tree_ranges(expr) == 1);

         tree_t r = tree_range(expr, 0);
         if (tree_subkind(r) == RANGE_EXPR)
            return prefix;
         else if (!is_static(tree_left(r)) || !is_static(tree_right(r)))
            return prefix;

         return expr;
      }

   case T_RECORD_REF:
      {
         tree_t value = tree_value(expr);
         tree_t prefix = longest_static_prefix(value);

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

tree_t find_generic_map(tree_t unit, int pos, tree_t g)
{
   const int ngenmaps = tree_genmaps(unit);

   if (pos < ngenmaps) {
      tree_t m = tree_genmap(unit, pos);
      if (tree_subkind(m) == P_POS && tree_pos(m) == pos)
         return tree_value(m);
   }

   for (int j = 0; j < ngenmaps; j++) {
      tree_t m = tree_genmap(unit, j);
      switch (tree_subkind(m)) {
      case P_NAMED:
         {
            tree_t name = tree_name(m);
            assert(tree_kind(name) == T_REF);

            if (tree_has_ref(name) && tree_ref(name) == g)
               return tree_value(m);
         }
         break;

      case P_POS:
         if (tree_pos(m) == pos)
            return tree_value(m);
         break;

      default:
         break;
      }
   }

   return NULL;
}

bool relaxed_rules(void)
{
   return opt_get_int(OPT_RELAXED);
}

bool is_type_attribute(attr_kind_t kind)
{
   switch (kind) {
   case ATTR_SUBTYPE:
   case ATTR_BASE:
   case ATTR_ELEMENT:
   case ATTR_DESIGNATED_SUBTYPE:
   case ATTR_INDEX:
      return true;
   default:
      return false;
   }
}

bool attribute_has_param(attr_kind_t attr)
{
   switch (attr) {
   case ATTR_IMAGE:
   case ATTR_SUCC:
   case ATTR_PRED:
   case ATTR_DELAYED:
   case ATTR_LEFTOF:
   case ATTR_RIGHTOF:
   case ATTR_VALUE:
   case ATTR_POS:
   case ATTR_LOW:
   case ATTR_HIGH:
   case ATTR_LEFT:
   case ATTR_RIGHT:
   case ATTR_LENGTH:
   case ATTR_RANGE:
   case ATTR_REVERSE_RANGE:
   case ATTR_VAL:
   case ATTR_QUIET:
   case ATTR_STABLE:
   case ATTR_INDEX:
   case ATTR_ASCENDING:
      return true;
   default:
      return false;
   }
}

type_t get_type_or_null(tree_t t)
{
   switch (tree_kind(t)) {
   case T_LIBRARY:
   case T_ATTR_SPEC:
   case T_PACKAGE:
   case T_PACK_INST:
   case T_PACK_BODY:
   case T_ENTITY:
   case T_ARCH:
   case T_PROCESS:
   case T_COMPONENT:
   case T_INSTANCE:
   case T_CONCURRENT:
   case T_BLOCK:
   case T_WHILE:
   case T_FOR:
   case T_LOOP:
   case T_GROUP_TEMPLATE:
   case T_CONFIGURATION:
   case T_GROUP:
   case T_FOR_GENERATE:
   case T_IF_GENERATE:
   case T_CASE_GENERATE:
   case T_USE:
   case T_CONTEXT:
   case T_PSL_DECL:
   case T_PSL_DIRECT:
   case T_WAVEFORM:
      return NULL;
   default:
      if (tree_has_type(t))
         return tree_type(t);
      else
         return NULL;
   }
}

type_t subtype_for_string(tree_t str, type_t base)
{
   if (type_const_bounds(base))
      return base;    // Can be checked statically
   else if (!type_is_unconstrained(base))
      base = type_base_recur(base);

   // Construct a new constrained array subtype: the direction and
   // bounds are the same as those for a positional array aggregate
   type_t sub = type_new(T_SUBTYPE);
   type_set_base(sub, base);

   type_t index_type = index_type_of(base, 0);
   const bool is_enum = type_is_enum(index_type);

   // The direction is determined by the index type
   range_kind_t dir = direction_of(index_type, 0);
   tree_t index_r = range_of(index_type, 0);

   // The left bound is the left of the index type and the right bound
   // is determined by the number of elements

   tree_t left = NULL, right = NULL;
   const int nchars = tree_chars(str);

   if (is_enum) {
      const int nlits = type_enum_literals(type_base_recur(index_type));
      int64_t index_left = assume_int(tree_left(index_r));

      int64_t iright, ileft;
      if (nchars == 0) {
         iright = index_left;
         ileft = assume_int(tree_right(index_r));
      }
      else if (dir == RANGE_DOWNTO) {
         ileft = index_left;
         iright = MIN(nlits - 1, MAX(0, index_left - nchars + 1));
      }
      else {
         ileft = index_left;
         iright = MIN(nlits - 1, MAX(0, index_left + nchars - 1));
      }

      left = get_enum_lit(str, index_type, ileft);
      right = get_enum_lit(str, index_type, iright);
   }
   else {
      left = tree_left(index_r);

      int64_t iright;
      if (dir == RANGE_DOWNTO)
         iright = assume_int(left) - nchars + 1;
      else
         iright = assume_int(left) + nchars - 1;

      right = get_int_lit(str, index_type, iright);
   }

   tree_t r = tree_new(T_RANGE);
   tree_set_subkind(r, dir);
   tree_set_left(r, left);
   tree_set_right(r, right);
   tree_set_loc(r, tree_loc(str));
   tree_set_type(r, index_type);

   tree_t c = tree_new(T_CONSTRAINT);
   tree_set_subkind(c, C_INDEX);
   tree_add_range(c, r);
   tree_set_loc(c, tree_loc(str));

   type_set_constraint(sub, c);

   return sub;
}

tree_t change_ref(tree_t name, tree_t new)
{
   switch (tree_kind(name)) {
   case T_REF:
      {
         tree_t ref = make_ref(new);
         tree_set_loc(ref, tree_loc(name));
         return ref;
      }

   case T_ARRAY_REF:
      {
         tree_t value = change_ref(tree_value(name), new);

         tree_t t = tree_new(T_ARRAY_REF);
         tree_set_loc(t, tree_loc(name));
         tree_set_value(t, value);
         tree_set_type(t, type_elem(tree_type(value)));

         const int nparams = tree_params(name);
         for (int i = 0; i < nparams; i++)
            tree_add_param(t, tree_param(name, i));

         return t;
      }

   case T_ARRAY_SLICE:
      {
         tree_t value = change_ref(tree_value(name), new);
         tree_t r = tree_range(name, 0);

         tree_t constraint = tree_new(T_CONSTRAINT);
         tree_set_subkind(constraint, C_INDEX);
         tree_add_range(constraint, r);

         type_t slice_type = type_new(T_SUBTYPE);
         type_set_constraint(slice_type, constraint);
         type_set_base(slice_type, tree_type(value));

         tree_t t = tree_new(T_ARRAY_SLICE);
         tree_set_loc(t, tree_loc(name));
         tree_set_value(t, value);
         tree_set_type(t, slice_type);
         tree_add_range(t, r);

         return t;
      }

   case T_RECORD_REF:
      {
         tree_t t = tree_new(T_RECORD_REF);
         tree_set_loc(t, tree_loc(name));
         tree_set_value(t, change_ref(tree_value(name), new));
         tree_set_type(t, tree_type(name));
         tree_set_ident(t, tree_ident(name));
         tree_set_ref(t, tree_ref(name));

         return t;
      }

   case T_CONV_FUNC:
      {
         tree_t t = tree_new(T_CONV_FUNC);
         tree_set_loc(t, tree_loc(name));
         tree_set_value(t, change_ref(tree_value(name), new));
         tree_set_ident(t, tree_ident(name));
         tree_set_type(t, tree_type(name));
         tree_set_ref(t, tree_ref(name));

         return t;
      }

   case T_TYPE_CONV:
      {
         tree_t t = tree_new(T_TYPE_CONV);
         tree_set_loc(t, tree_loc(name));
         tree_set_type(t, tree_type(name));
         tree_set_value(t, change_ref(tree_value(name), new));

         return t;
      }

   default:
      fatal_trace("cannot handle tree kind %s in elab_change_ref",
                  tree_kind_str(tree_kind(name)));
   }
}

static void build_wait_for_target(tree_t expr, build_wait_fn_t fn, void *ctx)
{
   switch (tree_kind(expr)) {
   case T_ARRAY_SLICE:
      build_wait(tree_range(expr, 0), fn, ctx);
      break;

   case T_ARRAY_REF:
      {
         const int nparams = tree_params(expr);
         for (int i = 0; i < nparams; i++)
            build_wait(tree_value(tree_param(expr, i)), fn, ctx);
      }
      break;

   default:
      break;
   }
}

void build_wait(tree_t expr, build_wait_fn_t fn, void *ctx)
{
   // LRM 08 section 10.2 has rules for building a wait statement from a
   // sensitivity list. LRM 08 section 11.3 extends these rules to
   // all-sensitised processes.

   switch (tree_kind(expr)) {
   case T_REF:
      if (class_of(tree_ref(expr)) == C_SIGNAL)
         (*fn)(expr, ctx);
      break;

   case T_EXTERNAL_NAME:
      if (tree_class(expr) == C_SIGNAL)
         (*fn)(expr, ctx);
      break;

   case T_WAVEFORM:
   case T_QUALIFIED:
   case T_TYPE_CONV:
   case T_INERTIAL:
      if (tree_has_value(expr))
         build_wait(tree_value(expr), fn, ctx);
      break;

   case T_ASSERT:
      build_wait(tree_value(expr), fn, ctx);
      // Fall-through
   case T_REPORT:
      if (tree_has_message(expr))
         build_wait(tree_message(expr), fn, ctx);
      break;

   case T_ARRAY_REF:
   case T_ARRAY_SLICE:
   case T_RECORD_REF:
      {
         tree_t ref = name_to_ref(expr);
         if (ref != NULL && class_of(ref) == C_SIGNAL
             && longest_static_prefix(expr) == expr)
            (*fn)(expr, ctx);
         else {
            build_wait(tree_value(expr), fn, ctx);
            build_wait_for_target(expr, fn, ctx);
         }
      }
      break;

   case T_FCALL:
   case T_PCALL:
   case T_PROT_FCALL:
   case T_PROT_PCALL:
      {
         tree_t decl = tree_ref(expr);
         const int nparams = tree_params(expr);
         for (int i = 0; i < nparams; i++) {
            tree_t p = tree_param(expr, i), port;
            switch (tree_subkind(p)) {
            case P_POS:
               port = tree_port(decl, tree_pos(p));
               break;
            case P_NAMED:
               port = tree_ref(name_to_ref(tree_name(p)));
               break;
            default:
               should_not_reach_here();
            }
            assert(tree_kind(port) == T_PARAM_DECL);

            switch (tree_subkind(port)) {
            case PORT_IN:
            case PORT_INOUT:
            case PORT_ARRAY_VIEW:
            case PORT_RECORD_VIEW:
               build_wait(tree_value(p), fn, ctx);
               break;
            default:
               break;
            }
         }
      }
      break;

   case T_AGGREGATE:
      {
         const int nassocs = tree_assocs(expr);
         for (int i = 0; i < nassocs; i++) {
            tree_t a = tree_assoc(expr, i);
            build_wait(tree_value(a), fn, ctx);

            switch (tree_subkind(a)) {
            case A_RANGE:
            case A_SLICE:
               build_wait(tree_range(a, 0), fn, ctx);
               break;
            case A_NAMED:
               build_wait(tree_name(a), fn, ctx);
               break;
            }
         }
      }
      break;

   case T_ATTR_REF:
      {
         const attr_kind_t predef = tree_subkind(expr);
         if (predef == ATTR_EVENT || predef == ATTR_ACTIVE)
            build_wait(tree_name(expr), fn, ctx);

         const int nparams = tree_params(expr);
         for (int i = 0; i < nparams; i++)
            build_wait(tree_value(tree_param(expr, i)), fn, ctx);
      }
      break;

   case T_LITERAL:
   case T_STRING:
   case T_DUMMY_DRIVER:
      break;

   case T_IF:
      {
         const int nconds = tree_conds(expr);
         for (int i = 0; i < nconds; i++)
            build_wait(tree_cond(expr, i), fn, ctx);
      }
      break;

   case T_COND_STMT:
      {
         if (tree_has_value(expr))
            build_wait(tree_value(expr), fn, ctx);

         const int nstmts = tree_stmts(expr);
         for (int i = 0; i < nstmts; i++)
            build_wait(tree_stmt(expr, i), fn, ctx);
      }
      break;

   case T_COND_VALUE:
      {
         const int nconds = tree_conds(expr);
         for (int i = 0; i < nconds; i++)
            build_wait(tree_cond(expr, i), fn, ctx);
         break;
      }

   case T_COND_EXPR:
      {
         if (tree_has_value(expr))
            build_wait(tree_value(expr), fn, ctx);

         build_wait(tree_result(expr), fn, ctx);
         break;
      }

   case T_PROCESS:
   case T_SEQUENCE:
   case T_PROC_BODY:
      {
         const int ndecls = tree_decls(expr);
         for (int i = 0; i < ndecls; i++) {
            tree_t d = tree_decl(expr, i);
            if (tree_kind(d) == T_PROC_BODY)
               build_wait(d, fn, ctx);
         }

         const int nstmts = tree_stmts(expr);
         for (int i = 0; i < nstmts; i++)
            build_wait(tree_stmt(expr, i), fn, ctx);
      }
      break;

   case T_SIGNAL_ASSIGN:
      {
         build_wait_for_target(tree_target(expr), fn, ctx);

         const int nwaves = tree_waveforms(expr);
         for (int i = 0; i < nwaves; i++)
            build_wait(tree_waveform(expr, i), fn, ctx);
      }
      break;

   case T_VAR_ASSIGN:
   case T_FORCE:
      build_wait_for_target(tree_target(expr), fn, ctx);
      build_wait(tree_value(expr), fn, ctx);
      break;

   case T_RELEASE:
      build_wait_for_target(tree_target(expr), fn, ctx);
      break;

   case T_CASE:
   case T_MATCH_CASE:
      {
         build_wait(tree_value(expr), fn, ctx);

         const int nstmts = tree_stmts(expr);
         for (int i = 0; i < nstmts; i++) {
            tree_t alt = tree_stmt(expr, i);

            const int nstmts = tree_stmts(alt);
            for (int j = 0; j < nstmts; j++)
               build_wait(tree_stmt(alt, j), fn, ctx);
         }
      }
      break;

   case T_FOR:
      {
         build_wait(tree_range(expr, 0), fn, ctx);

         const int nstmts = tree_stmts(expr);
         for (int i = 0; i < nstmts; i++)
            build_wait(tree_stmt(expr, i), fn, ctx);
      }
      break;

   case T_WHILE:
      build_wait(tree_value(expr), fn, ctx);
      // Fall-through
   case T_LOOP:
      {
         const int nstmts = tree_stmts(expr);
         for (int i = 0; i < nstmts; i++)
            build_wait(tree_stmt(expr, i), fn, ctx);
      }
      break;

   case T_NEXT:
   case T_EXIT:
      if (tree_has_value(expr))
         build_wait(tree_value(expr), fn, ctx);
      break;

   case T_RANGE:
      if (tree_subkind(expr) == RANGE_EXPR)
         build_wait(tree_value(expr), fn, ctx);
      else {
         build_wait(tree_left(expr), fn, ctx);
         build_wait(tree_right(expr), fn, ctx);
      }
      break;

   case T_OPEN:
      break;

   default:
      fatal_trace("Cannot handle tree kind %s in wait expression",
                  tree_kind_str(tree_kind(expr)));
   }
}

void print_syntax(const char *fmt, ...)
{
   LOCAL_TEXT_BUF tb = tb_new();
   bool highlighting = false;
   static bool comment = false, last_was_newline = false;
   for (const char *p = fmt; *p != '\0'; p++) {
      if (comment) {
         if (*p == '\n' || *p == '\r') {
            comment = false;
            last_was_newline = true;
            tb_printf(tb, "$$\n");
         }
         else if (*p != '~' && *p != '#') {
            tb_append(tb, *p);
            last_was_newline = false;
         }
         if (p > fmt && *p == '/' && *(p - 1) == '*') {
            tb_printf(tb, "$$");
            comment = false;
            last_was_newline = false;
         }
      }
      else if (*p == '\r') {
         if (!last_was_newline) {
            tb_append(tb, '\n');
            last_was_newline = true;
         }
      }
      else if (*p == '#' && *(p + 1) != '#') {
         tb_printf(tb, "$bold$$cyan$");
         last_was_newline = false;
         highlighting = true;
      }
      else if (*p == '~' && *(p + 1) != '~') {
         tb_printf(tb, "$yellow$");
         last_was_newline = false;
         highlighting = true;
      }
      else if ((*p == '-' && *(p + 1) == '-')
               || (*p == '/' && *(p + 1) == '/')
               || (*p == '/' && *(p + 1) == '*')) {
         tb_printf(tb, "$red$%c", *p);
         last_was_newline = false;
         comment = true;
      }
      else if (!isalnum_iso88591(*p) && *p != '_'
               && *p != '%' && highlighting) {
         tb_printf(tb, "$$%c", *p);
         last_was_newline = false;
         highlighting = false;
      }
      else {
         tb_append(tb, *p);
         last_was_newline = (*p == '\n');
      }
   }

   if (highlighting)
      tb_cat(tb, "$$");

   va_list ap;
   va_start(ap, fmt);

   if (syntax_buf != NULL) {
      char *stripped LOCAL = strip_color(tb_get(tb), ap);
      tb_cat(syntax_buf, stripped);
   }
   else
      color_vprintf(tb_get(tb), ap);

   va_end(ap);
}

void capture_syntax(text_buf_t *tb)
{
   assert(tb == NULL || syntax_buf == NULL);
   syntax_buf = tb;
}

void analyse_file(const char *file, jit_t *jit, unit_registry_t *ur,
                  mir_context_t *mc)
{
   input_from_file(file);

   switch (source_kind()) {
   case SOURCE_VHDL:
      {
         lib_t work = lib_work();
         int base_errors = 0;
         tree_t unit;
         while (base_errors = error_count(), (unit = parse())) {
            if (error_count() == base_errors) {
               lib_put(work, unit);
               unit_registry_purge(ur, tree_ident(unit));

               simplify_local(unit, jit, ur, mc);
               bounds_check(unit);
            }
            else
               lib_put_error(work, unit);
         }
      }
      break;

   case SOURCE_VERILOG:
      {
         LOCAL_TEXT_BUF tb = tb_new();
         vlog_preprocess(tb, true);

         file_ref_t file_ref = loc_file_ref(file, NULL);
         input_from_buffer(tb_get(tb), tb_len(tb), file_ref, SOURCE_VERILOG);

         lib_t work = lib_work();
         vlog_node_t module;
         while ((module = vlog_parse())) {
            if (error_count() == 0) {
               vlog_check(module);

               if (error_count() == 0) {
                  vlog_simp(module);
                  lib_put_vlog(work, module);
               }
            }
         }
      }
      break;

   case SOURCE_SDF:
      {
         sdf_file_t *sdf_file = sdf_parse(file, 0);
         progress("analysed SDF file: %s", file);

         if (sdf_file != NULL) {
            warnf("SDF is not yet supported");
            sdf_file_free(sdf_file);
         }
      }
      break;
   }
}

bool all_character_literals(type_t type)
{
   assert(type_is_enum(type));

   type_t base = type_base_recur(type);
   const int nlits = type_enum_literals(base);
   for (int i = 0; i < nlits; i++) {
      if (ident_char(tree_ident(type_enum_literal(base, i)), 0) != '\'')
         return false;
   }

   return true;
}

bool is_operator_symbol(ident_t ident)
{
   const int len = ident_len(ident);
   if (len < 3)
      return false;
   else if (ident_char(ident, 0) != '"')
      return false;
   else if (ident_char(ident, len - 1) != '"')
      return false;

   const well_known_t wk = is_well_known(ident);

   if (standard() < STD_08)
      return wk >= W_OP_AND && wk <= W_OP_NOT;
   else
      return wk >= W_OP_AND && wk <= W_OP_MATCH_GREATER_EQUAL;
}

bool same_tree(tree_t a, tree_t b)
{
   const tree_kind_t akind = tree_kind(a);
   if (akind != tree_kind(b))
      return false;

   switch (akind) {
   case T_REF:
      return tree_ref(a) == tree_ref(b);
   case T_ARRAY_REF:
      {
         if (!same_tree(tree_value(a), tree_value(b)))
            return false;

         const int nparams = tree_params(a);
         assert(nparams == tree_params(b));

         for (int i = 0; i < nparams; i++) {
            tree_t pa = tree_value(tree_param(a, i));
            tree_t pb = tree_value(tree_param(b, i));
            if (!same_tree(pa, pb))
               return false;
         }

         return true;
      }
   case T_ARRAY_SLICE:
      {
         if (!same_tree(tree_value(a), tree_value(b)))
            return false;

         tree_t ra = tree_range(a, 0);
         tree_t rb = tree_range(b, 0);

         const range_kind_t rakind = tree_subkind(ra);
         if (rakind != tree_subkind(rb) || rakind == RANGE_EXPR)
            return false;

         return same_tree(tree_left(ra), tree_left(rb))
            && same_tree(tree_right(ra), tree_right(rb));
      }
   case T_RECORD_REF:
      return ident_casecmp(tree_ident(a), tree_ident(b))
         && same_tree(tree_value(a), tree_value(b));
   case T_LITERAL:
      {
         const literal_kind_t lkind = tree_subkind(a);
         if (lkind != tree_subkind(b))
            return false;

         switch (lkind) {
         case L_PHYSICAL:
            {
               ident_t aid = tree_has_ident(a) ? tree_ident(a) : NULL;
               ident_t bid = tree_has_ident(b) ? tree_ident(b) : NULL;
               if (aid == bid)
                  return tree_ival(a) == tree_ival(b);
               else
                  return false;
            }
         case L_INT:
            return tree_ival(a) == tree_ival(b);
         case L_REAL:
            return tree_dval(a) == tree_dval(b);
         case L_NULL:
            return true;
         default:
            return false;
         }
      }
   case T_STRING:
      {
         const int nchars = tree_chars(a);
         if (tree_chars(b) != nchars)
            return false;

         for (int i = 0; i < nchars; i++) {
            if (!same_tree(tree_char(a, i), tree_char(b, i)))
               return false;
         }

         return true;
      }
   case T_OPEN:
      return true;
   default:
      return false;
   }
}

static range_kind_t get_range_direction(tree_t r)
{
   assert(tree_kind(r) == T_RANGE);

   const range_kind_t dir = tree_subkind(r);
   if (dir != RANGE_EXPR)
      return dir;

   // Handle ranges like X'RANGE where X has known direction

   tree_t aref = tree_value(r);
   assert(tree_kind(aref) == T_ATTR_REF);

   const attr_kind_t kind = tree_subkind(aref);
   assert(kind == ATTR_RANGE || kind == ATTR_REVERSE_RANGE);

   type_t prefix_type = tree_type(tree_name(aref));
   if (type_is_unconstrained(prefix_type))
      return RANGE_EXPR;

   tree_t prefix_r = range_of(prefix_type, 0);

   const range_kind_t prefix_dir = get_range_direction(prefix_r);
   if (prefix_dir != RANGE_TO && prefix_dir != RANGE_DOWNTO)
      return prefix_dir;
   else if (kind == ATTR_REVERSE_RANGE)
      return prefix_dir == RANGE_TO ? RANGE_DOWNTO : RANGE_TO;
   else
      return prefix_dir;
}

bool calculate_aggregate_bounds(tree_t expr, range_kind_t *kind,
                                int64_t *left, int64_t *right)
{
   // Calculate the direction and bounds of an array aggregate using the
   // rules in LRM 93 7.3.2.2

   type_t type = tree_type(expr);
   type_t index_type = index_type_of(type, 0);
   if (index_type == NULL || type_is_none(index_type))
      return false;

   tree_t index_r = range_of(index_type, 0), base_r = index_r;

   int64_t low, high;
   if (!folded_bounds(index_r, &low, &high))
      return false;

   int64_t clow = INT64_MAX, chigh = INT64_MIN;  // Actual bounds computed below

   range_kind_t dir;
   if (type_is_unconstrained(type))
      dir = tree_subkind(index_r);
   else {
      base_r = range_of(type, 0);
      dir = get_range_direction(base_r);
   }

   const int nassocs = tree_assocs(expr);

   if (standard() >= STD_08) {
      // VHDL-2008 range association determines index direction for
      // unconstrained aggregate when the expression type matches the
      // array type
      for (int i = 0; i < nassocs; i++) {
         tree_t a = tree_assoc(expr, i);
         if (tree_subkind(a) == A_SLICE)
            dir = tree_subkind(tree_range(a, 0));
      }
   }

   if (dir != RANGE_TO && dir != RANGE_DOWNTO)
      return false;

   int64_t pos = 0;
   for (int i = 0; i < nassocs; i++) {
      tree_t a = tree_assoc(expr, i);
      int64_t ilow = 0, ihigh = 0;
      const assoc_kind_t akind = tree_subkind(a);

      switch (akind) {
      case A_NAMED:
         {
            tree_t name = tree_name(a);
            if (folded_int(name, &ilow))
               ihigh = ilow;
            else
               return false;
         }
         break;

      case A_RANGE:
      case A_SLICE:
         {
            tree_t r = tree_range(a, 0);
            const range_kind_t rkind = tree_subkind(r);
            if (rkind == RANGE_TO || rkind == RANGE_DOWNTO) {
               tree_t left = tree_left(r), right = tree_right(r);

               int64_t ileft, iright;
               if (folded_int(left, &ileft) && folded_int(right, &iright)) {
                  ilow = (rkind == RANGE_TO ? ileft : iright);
                  ihigh = (rkind == RANGE_TO ? iright : ileft);
               }
               else
                  return false;
            }
            else
               return false;
         }
         break;

      case A_OTHERS:
         return false;

      case A_POS:
         if (i == 0) {
            int64_t ileft;
            if (folded_int(tree_left(base_r), &ileft))
               ilow = ihigh = ileft;
            else
               return false;
         }
         else if (dir == RANGE_TO)
            ilow = ihigh = clow + pos;
         else
            ilow = ihigh = chigh - pos;
         pos++;
         break;

      case A_CONCAT:
         {
            type_t value_type = tree_type(tree_value(a));

            int64_t length;
            if (type_is_unconstrained(value_type))
               return false;
            else if (folded_length(range_of(value_type, 0), &length)) {
               if (i == 0) {
                  int64_t ileft;
                  if (folded_int(tree_left(base_r), &ileft))
                     ilow = ihigh = ileft;
                  else
                     return false;
               }
               else if (dir == RANGE_TO) {
                  ilow = clow + pos;
                  ihigh = ilow + length - 1;
               }
               else {
                  ihigh = chigh - pos;
                  ilow = ihigh - length + 1;
               }
               pos += length;
            }
            else
               return false;
         }
         break;
      }

      clow = MIN(clow, ilow);
      chigh = MAX(chigh, ihigh);
   }

   if (clow < low || chigh > high)
      return false;   // Will raise a bounds check error later

   *kind = dir;
   *left = dir == RANGE_TO ? clow : chigh;
   *right = dir == RANGE_TO ? chigh : clow;

   return true;
}

type_t calculate_aggregate_subtype(tree_t expr)
{
   range_kind_t dir;
   int64_t ileft, iright;
   if (!calculate_aggregate_bounds(expr, &dir, &ileft, &iright))
      return NULL;

   type_t type = tree_type(expr);

   const int ndims = dimension_of(type);
   type_t a0_type = NULL;
   if (ndims > 1) {
      a0_type = tree_type(tree_value(tree_assoc(expr, 0)));
      if (type_is_unconstrained(a0_type))
         return NULL;

      assert(dimension_of(a0_type) == ndims - 1);
   }

   type_t index_type = index_type_of(type, 0);

   tree_t left = get_discrete_lit(expr, index_type, ileft);
   tree_t right = get_discrete_lit(expr, index_type, iright);
   assert(left != NULL && right != NULL);

   type_t sub = type_new(T_SUBTYPE);
   type_set_base(sub, type_base_recur(type));

   type_t elem = type_elem(type);
   if (type_is_unconstrained(elem)) {
      tree_t a0 = tree_assoc(expr, 0);
      switch (tree_subkind(a0)) {
      case A_CONCAT:
      case A_SLICE:
         a0_type = type_elem(tree_type(tree_value(a0)));
         break;
      default:
         a0_type = tree_type(tree_value(a0));
         break;
      }

      if (!type_is_unconstrained(a0_type))
         elem = a0_type;
   }

   type_set_elem(sub, elem);

   tree_t cons = tree_new(T_CONSTRAINT);
   tree_set_subkind(cons, C_INDEX);

   tree_t r = tree_new(T_RANGE);
   tree_set_subkind(r, dir);
   tree_set_type(r, index_type);
   tree_set_left(r, left);
   tree_set_right(r, right);

   tree_add_range(cons, r);

   for (int i = 1; i < ndims; i++)
      tree_add_range(cons, range_of(a0_type, i - 1));

   type_set_constraint(sub, cons);

   return sub;
}

bool can_be_signal(type_t type)
{
   switch (type_kind(type)) {
   case T_RECORD:
      {
         const int nfields = type_fields(type);
         for (int i = 0; i < nfields; i++) {
            if (!can_be_signal(tree_type(type_field(type, i))))
               return false;
         }

         return true;
      }
   case T_ARRAY:
      return can_be_signal(type_elem(type));
   case T_SUBTYPE:
      return can_be_signal(type_base(type));
   case T_ACCESS:
   case T_FILE:
   case T_PROTECTED:
   case T_INCOMPLETE:
      return false;
   default:
      return true;
   }
}

type_t merge_constraints(type_t to, type_t from)
{
   assert(type_is_unconstrained(to));
   assert(type_eq(to, from));

   tree_t cto = NULL;
   if (type_kind(to) == T_SUBTYPE && type_has_constraint(to))
      cto = type_constraint(to);

   tree_t cfrom = NULL;
   if (type_kind(from) == T_SUBTYPE && type_has_constraint(from))
      cfrom = type_constraint(from);

   if (cfrom == NULL)
      return to;

   type_t sub = type_new(T_SUBTYPE);
   type_set_base(sub, type_base_recur(to));

   if (type_is_array(to)) {
      type_set_constraint(sub, cto ?: cfrom);

      type_t elem = type_elem(to);
      if (type_is_unconstrained(elem))
         type_set_elem(sub, merge_constraints(elem, type_elem(from)));
      else
         type_set_elem(sub, elem);
   }
   else {
      tree_t cnew = tree_new(T_CONSTRAINT);
      tree_set_subkind(cnew, C_RECORD);
      tree_set_loc(cnew, tree_loc(cto ?: cfrom));

      type_set_constraint(sub, cnew);

      if (cto != NULL) {
         const int nto = tree_ranges(cto);
         for (int i = 0; i < nto; i++)
            tree_add_range(cnew, tree_range(cto, i));
      }

      const int nfrom = tree_ranges(cfrom), base = tree_ranges(cnew);
      for (int i = 0; i < nfrom; i++) {
         tree_t ec = tree_range(cfrom, i);
         assert(tree_kind(ec) == T_ELEM_CONSTRAINT);

         ident_t id = tree_ident(ec);
         bool found = false;
         for (int j = 0; j < base && !found; j++)
            found |= tree_ident(tree_range(cnew, j)) == id;

         if (!found)
            tree_add_range(cnew, ec);
      }
   }

   return sub;
}
