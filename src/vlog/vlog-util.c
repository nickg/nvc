//
//  Copyright (C) 2024 Nick Gasson
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
#include "ident.h"
#include "vlog/vlog-node.h"
#include "vlog/vlog-number.h"
#include "vlog/vlog-util.h"

#include <assert.h>

bool vlog_is_net(vlog_node_t v)
{
   switch (vlog_kind(v)) {
   case V_REF:
      return vlog_has_ref(v) ? vlog_is_net(vlog_ref(v)) : false;
   case V_PORT_DECL:
      return vlog_has_ref(v) ? vlog_is_net(vlog_ref(v)) : true;
   case V_NET_DECL:
      return true;
   default:
      return false;
   }
}

unsigned vlog_dimensions(vlog_node_t v)
{
   return vlog_ranges(vlog_type(v)) + vlog_ranges(v);
}

int64_t vlog_get_const(vlog_node_t v)
{
   switch (vlog_kind(v)) {
   case V_NUMBER:
      return number_integer(vlog_number(v));
   case V_REF:
      return vlog_get_const(vlog_ref(v));
   case V_LOCALPARAM:
      return vlog_get_const(vlog_value(v));
   default:
      fatal_at(vlog_loc(v), "expression is not constant");
   }
}

bool vlog_is_const(vlog_node_t v)
{
   switch (vlog_kind(v)) {
   case V_NUMBER:
      return true;
   case V_REF:
      return vlog_is_const(vlog_ref(v));
   case V_LOCALPARAM:
      return vlog_is_const(vlog_value(v));
   default:
      return false;
   }
}

bool vlog_is_up(vlog_node_t v)
{
   assert(vlog_kind(v) == V_DIMENSION);

   int64_t left, right;
   vlog_bounds(v, &left, &right);

   return left <= right;
}

void vlog_bounds(vlog_node_t v, int64_t *left, int64_t *right)
{
   *left = vlog_get_const(vlog_left(v));
   *right = vlog_get_const(vlog_right(v));
}

unsigned vlog_size(vlog_node_t v)
{
   switch (vlog_kind(v)) {
   case V_DATA_TYPE:
   case V_VAR_DECL:
   case V_NET_DECL:
   case V_PORT_DECL:
   case V_TF_PORT_DECL:
      {
         unsigned size = 1;

         const int nranges = vlog_ranges(v);
         for (int i = 0; i < nranges; i++) {
            vlog_node_t r = vlog_range(v, i);

            int64_t left, right;
            vlog_bounds(r, &left, &right);

            if (left < right)
               size *= right - left + 1;
            else
               size *= left - right + 1;
         }

         return size;
      }
   case V_PART_SELECT:
      if (vlog_subkind(v) != V_RANGE_CONST)
         return vlog_get_const(vlog_right(v));
      // Fall-through
   case V_DIMENSION:
      {
         int64_t left, right;
         vlog_bounds(v, &left, &right);

         if (left < right)
            return right - left + 1;
         else
            return left - right + 1;
      }
   default:
      CANNOT_HANDLE(v);
   }
}

bool is_top_level(vlog_node_t v)
{
   switch (vlog_kind(v)) {
   case V_MODULE:
   case V_PRIMITIVE:
   case V_INST_BODY:
      return true;
   default:
      return false;
   }
}

bool is_data_type(vlog_node_t v)
{
   switch (vlog_kind(v)) {
   case V_DATA_TYPE:
   case V_ENUM_DECL:
   case V_STRUCT_DECL:
   case V_UNION_DECL:
   case V_TYPE_DECL:
      return true;
   default:
      return false;
   }
}

vlog_node_t vlog_longest_static_prefix(vlog_node_t v)
{
   switch (vlog_kind(v)) {
   case V_REF:
   case V_HIER_REF:
      return v;
   case V_BIT_SELECT:
      {
         vlog_node_t value = vlog_value(v);
         vlog_node_t prefix = vlog_longest_static_prefix(value);

         if (prefix != value)
            return prefix;

         const int nparams = vlog_params(v);
         for (int i = 0; i < nparams; i++) {
            if (!vlog_is_const(vlog_param(v, i)))
               return prefix;
         }

         return v;
      }
   case V_PART_SELECT:
      {
         vlog_node_t value = vlog_value(v);
         vlog_node_t prefix = vlog_longest_static_prefix(value);

         if (prefix != value)
            return prefix;

         if (!vlog_is_const(vlog_left(v)))
            return prefix;

         return v;
      }
   default:
      fatal_at(vlog_loc(v), "cannot calculate longest static prefix");
   }
}
