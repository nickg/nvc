//
//  Copyright (C) 2014-2025  Nick Gasson
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
#include "mir/mir-unit.h"
#include "tree.h"
#include "type.h"
#include "vhdl/vhdl-lower.h"
#include "vhdl/vhdl-priv.h"

static mir_type_t lower_type(mir_unit_t *mu, type_t type)
{
   switch (type_kind(type)) {
   case T_SUBTYPE:
      if (type_is_array(type))
         should_not_reach_here();
      else
         return lower_type(mu, type_base(type));

   case T_ARRAY:
      {
         const type_info_t *elem = type_info(mu, type_elem_recur(type));

         if (type_const_bounds(type))
            should_not_reach_here();
         else
            return mir_uarray_type(mu, 1 /*XXX*/, elem->type);
      }

   case T_PHYSICAL:
   case T_INTEGER:
      {
         tree_t r = type_dim(type, 0);
         int64_t low, high;
         const bool folded = folded_bounds(r, &low, &high);
         if (folded)
            return mir_int_type(mu, low, high);
         else
            return mir_int_type(mu, INT64_MIN, INT64_MAX);
      }

   case T_ENUM:
      return mir_int_type(mu, 0, type_enum_literals(type) - 1);

   case T_RECORD:
      should_not_reach_here();

   case T_PROTECTED:
      return mir_context_type(mu, type_ident(type));

   case T_FILE:
      return mir_file_type(mu, lower_type(mu, type_designated(type)));

   case T_ACCESS:
      should_not_reach_here();

   case T_REAL:
      should_not_reach_here();

   case T_INCOMPLETE:
      return mir_opaque_type(mu);

   default:
      fatal_trace("cannot lower type kind %s", type_kind_str(type_kind(type)));
   }
}

static mir_stamp_t lower_bounds(mir_unit_t *mu, type_t type)
{
   if (type_kind(type) == T_SUBTYPE) {
      if (type_is_integer(type) || type_is_enum(type)) {
         tree_t r = range_of(type, 0);
         int64_t low, high;
         if (folded_bounds(r, &low, &high))
            return mir_int_stamp(mu, low, high);
      }
      else if (type_is_real(type)) {
         tree_t r = range_of(type, 0);
         double low, high;
         if (folded_bounds_real(r, &low, &high))
            return mir_real_stamp(mu, low, high);
      }
   }

   if (type_is_array(type))
      return lower_bounds(mu, type_elem(type));

   return MIR_NULL_STAMP;
}

const type_info_t *type_info(mir_unit_t *mu, type_t type)
{
   type_info_t *ti = mir_get_priv(mu, type);
   if (ti != NULL)
      return ti;

   ti = mir_malloc(mu, sizeof(type_info_t));
   ti->type  = lower_type(mu, type);
   ti->stamp = lower_bounds(mu, type);

   mir_put_priv(mu, type, ti);
   return ti;
}
