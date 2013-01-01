//
//  Copyright (C) 2012  Nick Gasson
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
#include "tree.h"
#include "type.h"

#include <inttypes.h>
#include <stdio.h>
#include <assert.h>
#include <stdarg.h>
#include <stdlib.h>
#include <string.h>

static bool pp_char_enum(type_t type)
{
   if (type_kind(type) != T_ENUM)
      return false;

   bool all_char = true;
   unsigned nlit = type_enum_literals(type);
   for (unsigned i = 0; i < nlit; i++) {
      tree_t lit = type_enum_literal(type, i);
      all_char = all_char && (ident_char(tree_ident(lit), 0) == '\'');
   }
   return all_char;
}

static void pp_one(char *buf, type_t type, uint64_t value)
{
   switch (type_kind(type)) {
   case T_INTEGER:
      static_printf(buf, "%"PRIu64, value);
      break;

   case T_ENUM:
      {
         assert(value < type_enum_literals(type));
         const char *s = istr(tree_ident(type_enum_literal(type, value)));
         if (*s == '\'')
            static_printf(buf, "%c", *(s + 1));
         else
            static_printf(buf, "%s", s);
      }
      break;

   default:
      static_printf(buf, "%"PRIx64, value);
   }
}

static const uint64_t *pp_array(char *buf, type_t type, const uint64_t *values)
{
   type_t elem = type_base_recur(type_elem(type));
   bool all_char = pp_char_enum(elem);

   static_printf(buf, all_char ? "\"" : "(");

   range_t r = type_dim(type, 0);
   const int left  = assume_int(r.left);
   const int right = assume_int(r.right);
   const int step  = (r.kind == RANGE_TO) ? 1 : -1;

   for (int i = left; i != right + step; i += step) {
      if (!all_char && (i != left))
         static_printf(buf, ",");
      if (type_is_array(elem))
         values = pp_array(buf, elem, values);
      else
         pp_one(buf, elem, *values++);
   }

   static_printf(buf, all_char ? "\"" : ")");

   return values;
}

const char *pprint(tree_t t, const uint64_t *values, size_t len)
{
   static char buf[1024];

   static_printf_begin(buf, sizeof(buf));

   type_t type = tree_type(t);

   if (type_is_array(type))
      pp_array(buf, type, values);
   else
      pp_one(buf, type_base_recur(type), values[0]);

   for (int i = 0; i < 3; i++)
      *(buf + sizeof(buf) - 2 - i) = '.';

   return buf;
}
