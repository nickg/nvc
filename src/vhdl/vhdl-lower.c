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

static void fill_array_type_info(mir_unit_t *mu, type_t type, type_info_t *ti)
{
   ti->ndims = ti->udims = dimension_of(type);

   for (type_t e = type_elem(type);
        type_is_array(e) && !type_const_bounds(e);
        e = type_elem(e))
      ti->udims += dimension_of(e);

   const type_info_t *elem = type_info(mu, type_elem_recur(type));

   if (type_const_bounds(type)) {
      ti->size = elem->size;
      for (int i = 0; i < ti->ndims; i++) {
         tree_t r = range_of(type, i);
         int64_t low, high;
         range_bounds(r, &low, &high);
         ti->size *= MAX(high - low + 1, 0);
      }

      ti->type = mir_carray_type(mu, ti->size, elem->type);
   }
   else
      ti->type = mir_uarray_type(mu, ti->ndims, elem->type);

   ti->stamp = elem->stamp;
}

const type_info_t *type_info(mir_unit_t *mu, type_t type)
{
   type_info_t *ti = mir_get_priv(mu, type);
   if (ti != NULL)
      return ti;

   ti = mir_malloc(mu, sizeof(type_info_t));
   ti->type  = MIR_NULL_TYPE;
   ti->stamp = MIR_NULL_STAMP;
   ti->kind  = type_base_kind(type);
   ti->ndims = 0;
   ti->udims = 0;
   ti->size  = 1;

   switch (type_kind(type)) {
   case T_SUBTYPE:
      if (type_is_array(type))
         fill_array_type_info(mu, type, ti);
      else {
         ti->type = type_info(mu, type_base(type))->type;

         if (type_is_integer(type) || type_is_enum(type)) {
            tree_t r = range_of(type, 0);
            int64_t low, high;
            if (folded_bounds(r, &low, &high))
               ti->stamp = mir_int_stamp(mu, low, high);
         }
         else if (type_is_real(type)) {
            tree_t r = range_of(type, 0);
            double low, high;
            if (folded_bounds_real(r, &low, &high))
               ti->stamp = mir_real_stamp(mu, low, high);
         }
      }
      break;

   case T_ARRAY:
      fill_array_type_info(mu, type, ti);
      break;

   case T_PHYSICAL:
   case T_INTEGER:
      {
         tree_t r = type_dim(type, 0);
         int64_t low, high;
         const bool folded = folded_bounds(r, &low, &high);
         if (folded)
            ti->type = mir_int_type(mu, low, high);
         else
            ti->type = mir_int_type(mu, INT64_MIN, INT64_MAX);
      }
      break;

   case T_ENUM:
      ti->type = mir_int_type(mu, 0, type_enum_literals(type) - 1);
      break;

   case T_RECORD:
      {
         const int nfields = type_fields(type);
         mir_type_t *fields = xmalloc_array(nfields, sizeof(mir_type_t));
         for (int i = 0; i < nfields; i++)
            fields[i] = type_info(mu, tree_type(type_field(type, i)))->type;

         ti->type = mir_record_type(mu, type_ident(type), fields, nfields);
      }
      break;

   case T_PROTECTED:
      ti->type = mir_context_type(mu, type_ident(type));
      break;

   case T_FILE:
      ti->type = mir_file_type(mu, type_info(mu, type_designated(type))->type);
      break;

   case T_ACCESS:
      should_not_reach_here();

   case T_REAL:
      {
         tree_t r = type_dim(type, 0);
         double low, high;
         const bool folded = folded_bounds_real(r, &low, &high);
         if (folded)
            ti->type = mir_real_type(mu, low, high);
         else
            ti->type = mir_double_type(mu);
      }
      break;

   case T_INCOMPLETE:
      ti->type = mir_opaque_type(mu);
      break;

   default:
      fatal_trace("cannot lower type kind %s", type_kind_str(type_kind(type)));
   }

   mir_put_priv(mu, type, ti);
   return ti;
}
