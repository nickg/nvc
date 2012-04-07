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

static bool pp_char_enum(type_t type)
{
   bool all_char = true;
   unsigned nlit = type_enum_literals(type);
   for (unsigned i = 0; i < nlit; i++) {
      tree_t lit = type_enum_literal(type, i);
      all_char = all_char && (ident_char(tree_ident(lit), 0) == '\'');
   }
   return all_char;
}

static size_t pp_one(char *p, size_t len, type_t type, uint64_t value)
{
   switch (type_kind(type)) {
   case T_INTEGER:
      return snprintf(p, len, "%"PRIi64, value);

   case T_ENUM:
      {
         assert(value < type_enum_literals(type));
         const char *s = istr(tree_ident(type_enum_literal(type, value)));
         if (*s == '\'')
            return snprintf(p, len, "%c", *(s + 1));
         else
            return snprintf(p, len, "%s", s);
      }

   default:
      return snprintf(p, len, "%"PRIx64, value);
   }
}

const char *pprint(tree_t t, uint64_t *values, unsigned len)
{
   static char buf[1024];

   char *p = buf;
   const char *end = buf + sizeof(buf);

   type_t type = tree_type(t);

   if (type_is_array(type)) {
      type_t elem = type_base_recur(type_elem(type));
      bool all_char = pp_char_enum(elem);

      p += snprintf(p, end - p, all_char ? "\"" : "(");

      unsigned left = 0, right = len - 1, step = 1;
      if (type_dim(type, 0).kind == RANGE_DOWNTO) {
         left  = len - 1;
         right = 0;
         step  = -1;
      }

      for (unsigned i = left; i != right + step; i += step) {
         if (!all_char && (i != left))
            p += snprintf(p, end - p, ",");
         p += pp_one(p, end - p, elem, values[i]);
      }

      p += snprintf(p, end - p, all_char ? "\"" : ")");
   }
   else
      pp_one(p, end - p, type_base_recur(type), values[0]);

   return buf;
}
