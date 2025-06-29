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

void vlog_bounds(vlog_node_t v, int64_t *left, int64_t *right)
{
   assert(vlog_subkind(v) == V_DIM_PACKED);

   vlog_node_t left_node = vlog_left(v);
   vlog_node_t right_node = vlog_right(v);

   if (vlog_kind(left_node) != V_NUMBER || vlog_kind(right_node) != V_NUMBER)
      fatal_at(vlog_loc(v), "packed dimensions are not constant");

   *left = number_integer(vlog_number(left_node));
   *right = number_integer(vlog_number(right_node));
}

unsigned vlog_size(vlog_node_t v)
{
   unsigned size = 1;

   const int nranges = vlog_ranges(v);
   for (int i = 0; i < nranges; i++) {
      vlog_node_t r = vlog_range(v, i);
      assert(vlog_subkind(r) == V_DIM_PACKED);

      int64_t left, right;
      vlog_bounds(r, &left, &right);

      if (left < right)
         size *= right - left + 1;
      else
         size *= left - right + 1;
   }

   return size;
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
