//
//  Copyright (C) 2013-2023  Nick Gasson
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
#include "type.h"
#include "vlog/vlog-phase.h"

#include <assert.h>
#include <ctype.h>
#include <string.h>
#include <stdlib.h>
#include <inttypes.h>

typedef struct {
   ptr_list_t copied_subs;
   ptr_list_t copied_types;
} copy_ctx_t;

static vhdl_standard_t  current_std  = STD_02;
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
      {
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
         default:
            return false;
         }
      }
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

tree_t get_real_lit(tree_t t, type_t type, double r)
{
   tree_t f = tree_new(T_LITERAL);
   tree_set_loc(f, tree_loc(t));
   tree_set_subkind(f, L_REAL);
   tree_set_dval(f, r);
   tree_set_type(f, type ?: tree_type(t));

   return f;
}

bool parse_value(type_t type, const char *str, scalar_value_t *value)
{
   while (isspace_iso88591(*str))
      ++str;

   type_t base = type_base_recur(type);
   const type_kind_t basek = type_kind(base);

   switch (basek) {
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

   default:
      break;
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
      return C_FUNCTION;
   case T_PROC_DECL:
   case T_PROC_BODY:
   case T_PROC_INST:
   case T_PCALL:
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
   case T_CONCURRENT:
   case T_ELAB:
      return C_LABEL;
   case T_COMPONENT:
      return C_COMPONENT;
   case T_REF:
      return tree_has_ref(t) ? class_of(tree_ref(t)) : C_DEFAULT;
   case T_ARRAY_REF:
   case T_ARRAY_SLICE:
   case T_RECORD_REF:
   case T_ALL:
   case T_ALIAS:
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

tree_t aliased_type_decl(tree_t decl)
{
   switch (tree_kind(decl)) {
   case T_ALIAS:
      {
         tree_t value = tree_value(decl);
         if (tree_kind(value) == T_REF && tree_has_ref(value))
            return aliased_type_decl(tree_ref(value));
         else
            return NULL;
      }
   case T_TYPE_DECL:
   case T_SUBTYPE_DECL:
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
      if (type_constraints(type) > 0) {
         tree_t c0 = type_constraint(type, 0);
         switch (tree_subkind(c0)) {
         case C_OPEN:
            return range_of(type_base(type), dim);
         case C_INDEX:
         case C_RANGE:
            return tree_range(type_constraint(type, 0), dim);
         default:
            fatal_trace("invalid constraint in range_of");
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
   id_cache[W_STD_STANDARD]   = ident_new("STD.STANDARD");
   id_cache[W_ALL]            = ident_new("all");
   id_cache[W_STD_BIT]        = ident_new("STD.STANDARD.BIT");
   id_cache[W_STD_BOOL]       = ident_new("STD.STANDARD.BOOLEAN");
   id_cache[W_STD_CHAR]       = ident_new("STD.STANDARD.CHARACTER");
   id_cache[W_STD_NATURAL]    = ident_new("STD.STANDARD.NATURAL");
   id_cache[W_STD_POSITIVE]   = ident_new("STD.STANDARD.POSITIVE");
   id_cache[W_STD_INTEGER]    = ident_new("STD.STANDARD.INTEGER");
   id_cache[W_STD_STRING]     = ident_new("STD.STANDARD.STRING");
   id_cache[W_STD_REAL]       = ident_new("STD.STANDARD.REAL");
   id_cache[W_STD_TIME]       = ident_new("STD.STANDARD.TIME");
   id_cache[W_STD_BIT_VECTOR] = ident_new("STD.STANDARD.BIT_VECTOR");
   id_cache[W_IEEE_SIGNED]    = ident_new("IEEE.NUMERIC_STD.SIGNED");
   id_cache[W_IEEE_UNSIGNED]  = ident_new("IEEE.NUMERIC_STD.UNSIGNED");
   id_cache[W_IEEE_LOGIC]     = ident_new("IEEE.STD_LOGIC_1164.STD_LOGIC");
   id_cache[W_IEEE_ULOGIC]    = ident_new("IEEE.STD_LOGIC_1164.STD_ULOGIC");
   id_cache[W_IEEE_1164_AND]  = ident_new("IEEE.STD_LOGIC_1164.\"and\"");
   id_cache[W_IEEE_1164_NAND] = ident_new("IEEE.STD_LOGIC_1164.\"nand\"");
   id_cache[W_IEEE_1164_OR]   = ident_new("IEEE.STD_LOGIC_1164.\"or\"");
   id_cache[W_IEEE_1164_NOR]  = ident_new("IEEE.STD_LOGIC_1164.\"nor\"");
   id_cache[W_IEEE_1164_XOR]  = ident_new("IEEE.STD_LOGIC_1164.\"xor\"");
   id_cache[W_IEEE_1164_XNOR] = ident_new("IEEE.STD_LOGIC_1164.\"xnor\"");
   id_cache[W_FOREIGN]        = ident_new("FOREIGN");
   id_cache[W_WORK]           = ident_new("WORK");
   id_cache[W_STD]            = ident_new("STD");
   id_cache[W_THUNK]          = ident_new("thunk");
   id_cache[W_BODY]           = ident_new("body");
   id_cache[W_CARET]          = ident_new("^");
   id_cache[W_IEEE]           = ident_new("IEEE");
   id_cache[W_IEEE_1164]      = ident_new("IEEE.STD_LOGIC_1164");
   id_cache[W_ERROR]          = ident_new("error");
   id_cache[W_CCONV]          = ident_new("\"??\"");
   id_cache[W_ELAB]           = ident_new("elab");
   id_cache[W_NUMERIC_STD]    = ident_new("IEEE.NUMERIC_STD");
   id_cache[W_NUMERIC_BIT]    = ident_new("IEEE.NUMERIC_BIT");
   id_cache[W_NVC]            = ident_new("NVC");
   id_cache[W_DEFAULT_CLOCK]  = ident_new("default clock");
   id_cache[W_DOLLAR_DISPLAY] = ident_new("$display");
   id_cache[W_DOLLAR_FINISH]  = ident_new("$finish");
   id_cache[W_DOLLAR_WRITE]   = ident_new("$write");
   id_cache[W_STD_REFLECTION] = ident_new("STD.REFLECTION");

   id_cache[W_NVC_PSL_SUPPORT] = ident_new("NVC.PSL_SUPPORT");

   id_cache[W_IEEE_LOGIC_VECTOR] =
      ident_new("IEEE.STD_LOGIC_1164.STD_LOGIC_VECTOR");
   id_cache[W_IEEE_ULOGIC_VECTOR] =
      ident_new("IEEE.STD_LOGIC_1164.STD_ULOGIC_VECTOR");

   id_cache[W_NUMERIC_STD_UNSIGNED] = ident_new("IEEE.NUMERIC_STD_UNSIGNED");
   id_cache[W_NUMERIC_BIT_UNSIGNED] = ident_new("IEEE.NUMERIC_BIT_UNSIGNED");
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

      switch (tree_kind(d)) {
      case T_FUNC_DECL:
      case T_PROC_DECL:
         if (tree_flags(d) & TREE_F_PREDEFINED)
            continue;
         else if (tree_subkind(d) == S_FOREIGN)
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

tree_t search_decls(tree_t container, ident_t name, int nth)
{
   type_t type;
   tree_kind_t kind = tree_kind(container);
   if (kind == T_LIBRARY) {
      if (nth == 0) {
         lib_t lib = lib_require(tree_ident(container));
         return lib_get(lib, name);
      }
      else
         return NULL;
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
         if (tree_ident(p) == name && nth-- == 0)
            return p;
      }
   }
   else if (!is_container(container))
      return NULL;

   // TODO: how to improve this?
   const int ndecls = tree_decls(container);
   tree_t best = NULL;

   for (int i = 0; i < ndecls; i++) {
      tree_t d = tree_decl(container, i);
      if (!tree_has_ident(d))
         continue;
      else if (tree_ident(d) == name) {
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

type_t ieee_type(ieee_type_t which)
{
   static type_t cache[IEEE_STD_LOGIC + 1] = {};
   assert(which < ARRAY_LEN(cache));

   if (cache[which] == NULL) {
      const char *names[] = {
         "STD_ULOGIC",
         "STD_LOGIC",
      };

      static tree_t ieee_cache[STD_19 + 1] = {};
      tree_t unit = cached_unit(NULL, ieee_cache, W_IEEE, W_IEEE_1164);

      tree_t d = search_decls(unit, ident_new(names[which]), 0);
      if (d == NULL)
         fatal_trace("cannot find IEEE type %s", names[which]);

      // STD.STANDARD cannot depend on IEEE
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
      const char *names[] = {
         "VALUE_MIRROR",
         "SUBTYPE_MIRROR",
      };

      static tree_t reflect_cache[STD_19 + 1] = {};
      tree_t unit = cached_unit(NULL, reflect_cache, W_STD, W_STD_REFLECTION);

      tree_t d = search_decls(unit, ident_new(names[which]), 0);
      if (d == NULL)
         fatal_trace("cannot find REFLECTION type %s", names[which]);

      // STD.STANDARD cannot depend on REFLECTION
      assert(!opt_get_int(OPT_BOOTSTRAP));

      return (cache[which] = tree_type(d));
   }
   else
      return cache[which];
}

bool is_builtin(subprogram_kind_t kind)
{
   return kind != S_USER && kind != S_FOREIGN;
}

bool is_open_coded_builtin(subprogram_kind_t kind)
{
   switch (kind) {
   case S_USER:
   case S_FOREIGN:
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

unsigned get_case_choice_char(tree_t value, int depth)
{
   switch (tree_kind(value)) {
   case T_STRING:
      if (depth < tree_chars(value)) {
         tree_t ch = tree_char(value, depth);
         return tree_pos(tree_ref(ch));
      }
      else
         return ~0;   // Out of bounds

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
               return get_case_choice_char(left, depth);

            depth -= left_len;
         }
      }
      // Fall-through

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

         const int nranges = tree_ranges(expr);
         for (int i = 0; i < nranges; i++) {
            tree_t r = tree_range(expr, i);
            if (tree_subkind(r) == RANGE_EXPR)
               return prefix;
            else if (!is_static(tree_left(r)) || !is_static(tree_right(r)))
               return prefix;
         }

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

            if (tree_ref(name) == g)
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
   case T_GROUP_TEMPLATE:
   case T_CONFIGURATION:
   case T_GROUP:
   case T_FOR_GENERATE:
   case T_IF_GENERATE:
   case T_USE:
   case T_CONTEXT:
   case T_PSL:
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
   if (!type_is_unconstrained(base))
      return base;

   // Construct a new constrained array subtype: the direction and
   // bounds are the same as those for a positional array aggregate
   type_t sub = type_new(T_SUBTYPE);
   type_set_base(sub, base);

   type_t index_type = index_type_of(base, 0);
   const bool is_enum = type_is_enum(index_type);

   // The direction is determined by the index type
   range_kind_t dir = direction_of(index_type, 0);

   // The left bound is the left of the index type and the right bound
   // is determined by the number of elements

   tree_t left = NULL, right = NULL;

   if (is_enum)
      left = make_ref(type_enum_literal(type_base_recur(index_type), 0));
   else
      left = tree_left(range_of(index_type, 0));

   const int nchars = tree_chars(str);
   int64_t iright;
   if (dir == RANGE_DOWNTO)
      iright = assume_int(left) - nchars + 1;
   else
      iright = assume_int(left) + nchars - 1;

   if (is_enum)
      right = get_enum_lit(str, index_type, iright);
   else
      right = get_int_lit(str, index_type, iright);

   tree_t r = tree_new(T_RANGE);
   tree_set_subkind(r, dir);
   tree_set_left(r, left);
   tree_set_right(r, right);
   tree_set_loc(r, tree_loc(str));

   tree_t c = tree_new(T_CONSTRAINT);
   tree_set_subkind(c, C_INDEX);
   tree_add_range(c, r);
   tree_set_loc(c, tree_loc(str));

   type_add_constraint(sub, c);

   return sub;
}

tree_t change_ref(tree_t name, tree_t new)
{
   switch (tree_kind(name)) {
   case T_REF:
      return make_ref(new);

   case T_ARRAY_REF:
      {
         tree_t t = tree_new(T_ARRAY_REF);
         tree_set_loc(t, tree_loc(name));
         tree_set_value(t, change_ref(tree_value(name), new));
         tree_set_type(t, tree_type(name));

         const int nparams = tree_params(name);
         for (int i = 0; i < nparams; i++)
            tree_add_param(t, tree_param(name, i));

         return t;
      }

   case T_ARRAY_SLICE:
      {
         tree_t t = tree_new(T_ARRAY_SLICE);
         tree_set_loc(t, tree_loc(name));
         tree_set_value(t, change_ref(tree_value(name), new));
         tree_set_type(t, tree_type(name));
         tree_add_range(t, tree_range(name, 0));

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
   case T_ASSERT:
      if (tree_has_value(expr))
         build_wait(tree_value(expr), fn, ctx);
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
            tree_t p = tree_param(expr, i);
            assert(tree_subkind(p) == P_POS);
            const port_mode_t mode = tree_subkind(tree_port(decl, tree_pos(p)));
            if (mode == PORT_IN || mode == PORT_INOUT)
               build_wait(tree_value(p), fn, ctx);
         }
      }
      break;

   case T_AGGREGATE:
      {
         const int nassocs = tree_assocs(expr);
         for (int i = 0; i < nassocs; i++)
            build_wait(tree_value(tree_assoc(expr, i)), fn, ctx);
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
      build_wait_for_target(tree_target(expr), fn, ctx);
      build_wait(tree_value(expr), fn, ctx);
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
      {
         build_wait(tree_value(expr), fn, ctx);

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
      else if (*p == '#') {
         tb_printf(tb, "$bold$$cyan$");
         last_was_newline = false;
         highlighting = true;
      }
      else if (*p == '~') {
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
      else if (!isalnum((int)*p) && *p != '_' && *p != '%' && highlighting) {
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

void analyse_file(const char *file, jit_t *jit, unit_registry_t *ur)
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

               simplify_local(unit, jit, ur);
               bounds_check(unit);

               if (error_count() == base_errors && unit_needs_cgen(unit))
                  lower_standalone_unit(ur, unit);
            }
            else
               lib_put_error(work, unit);
         }
      }
      break;

   case SOURCE_VERILOG:
#ifdef ENABLE_VERILOG
      {
         lib_t work = lib_work();
         vlog_node_t module;
         while ((module = vlog_parse())) {
            if (error_count() == 0) {
               vlog_check(module);

               if (error_count() == 0)
                  lib_put_vlog(work, module);
            }
         }
      }
#else
      fatal("Verilog is not currently supported");
#endif
      break;
   }
}

static void tree_copy_cb(tree_t t, void *__ctx)
{
   copy_ctx_t *ctx = __ctx;

   if (is_subprogram(t))
      list_add(&ctx->copied_subs, t);
}

static void type_copy_cb(type_t type, void *__ctx)
{
   copy_ctx_t *ctx = __ctx;

   if (type_has_ident(type))
      list_add(&ctx->copied_types, type);
}

void copy_with_renaming(tree_t *roots, int nroots, tree_copy_pred_t tree_pred,
                        type_copy_pred_t type_pred, void *context,
                        ident_t dotted, ident_t *prefixes, int nprefix)
{
   copy_ctx_t copy_ctx = {};

   tree_copy(roots, nroots, tree_pred, type_pred, context,
             tree_copy_cb, type_copy_cb, &copy_ctx);

   // Change the name of any copied types to reflect the new hiearchy
   for (list_iter(type_t, type, copy_ctx.copied_types)) {
      ident_t orig = type_ident(type);
      for (int j = 0; j < nprefix; j++) {
         if (ident_starts_with(orig, prefixes[j])) {
            LOCAL_TEXT_BUF tb = tb_new();
            tb_cat(tb, istr(dotted));
            tb_cat(tb, istr(orig) + ident_len(prefixes[j]));

            type_set_ident(type, ident_new(tb_get(tb)));
            break;
         }
      }
   }
   list_free(&copy_ctx.copied_types);

   // Change the mangled name of copied subprograms so that copies in
   // different instances do not collide
   for (list_iter(tree_t, decl, copy_ctx.copied_subs)) {
      if (tree_kind(decl) == T_GENERIC_DECL)
         continue;   // Does not yet have mangled name

      ident_t orig = tree_ident2(decl);
      for (int j = 0; j < nprefix; j++) {
         if (ident_starts_with(orig, prefixes[j])) {
            ident_t prefix = ident_runtil(orig, '(');

            LOCAL_TEXT_BUF tb = tb_new();
            tb_cat(tb, istr(dotted));
            tb_cat(tb, istr(prefix) + ident_len(prefixes[j]));

            const tree_kind_t kind = tree_kind(decl);
            const bool is_func = kind == T_FUNC_BODY || kind == T_FUNC_DECL;
            const int nports = tree_ports(decl);
            if (nports > 0 || is_func)
               tb_append(tb, '(');

            for (int k = 0; k < nports; k++) {
               tree_t p = tree_port(decl, k);
               if (tree_class(p) == C_SIGNAL)
                  tb_printf(tb, "s");
               mangle_one_type(tb, tree_type(p));
            }

            if (nports > 0 || is_func)
               tb_printf(tb, ")");

            if (is_func)
               mangle_one_type(tb, type_result(tree_type(decl)));

            ident_t mangled = ident_new(tb_get(tb));
            tree_set_ident2(decl, mangled);

            break;
         }
      }
   }
   list_free(&copy_ctx.copied_subs);
}
