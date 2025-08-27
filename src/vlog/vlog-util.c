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

bool vlog_get_const(vlog_node_t v, int64_t *value)
{
   switch (vlog_kind(v)) {
   case V_NUMBER:
      {
         number_t n = vlog_number(v);
         if (number_is_defined(n)) {
            *value = number_integer(n);
            return true;
         }
         else
            return false;
      }
   case V_REF:
      return vlog_get_const(vlog_ref(v), value);
   case V_LOCALPARAM:
      return vlog_get_const(vlog_value(v), value);
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

bool vlog_bounds(vlog_node_t v, int64_t *left, int64_t *right)
{
   bool is_defined = true;
   is_defined &= vlog_get_const(vlog_left(v), left);
   is_defined &= vlog_get_const(vlog_right(v), right);
   return is_defined;
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
      if (vlog_subkind(v) != V_RANGE_CONST) {
         int64_t width;
         if (vlog_get_const(vlog_right(v), &width))
            return width;
         else
            return 0;  // Undefined
      }
      // Fall-through
   case V_DIMENSION:
      {
         int64_t left, right;
         if (!vlog_bounds(v, &left, &right))
            return 0;  // Undefined

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

bool vlog_equal_node(vlog_node_t a, vlog_node_t b)
{
   if (a == b)
      return true;

   const vlog_kind_t kind = vlog_kind(a);
   if (kind != vlog_kind(b))
      return false;

   switch (kind) {
   case V_NUMBER:
      {
         number_t an = vlog_number(a);
         number_t bn = vlog_number(b);

         return number_equal(an, bn);
      }
   case V_BINARY:
      return vlog_subkind(a) == vlog_subkind(b)
         && vlog_equal_node(vlog_left(a), vlog_left(b))
         && vlog_equal_node(vlog_right(a), vlog_right(b));
   case V_REF:
      if (!vlog_has_ref(a) || !vlog_has_ref(b))
         return true;   // Suppress cascading errors
      else
         return vlog_ref(a) == vlog_ref(b);
   case V_PARAM_ASSIGN:
      {
         ident_t ia = vlog_has_ident(a) ? vlog_ident(a) : NULL;
         ident_t ib = vlog_has_ident(b) ? vlog_ident(b) : NULL;
         return ia == ib && vlog_equal_node(vlog_value(a), vlog_value(b));
      }
   default:
      return false;
   }
}

uint32_t vlog_hash_node(vlog_node_t v)
{
   switch (vlog_kind(v)) {
   case V_NUMBER:
      return number_hash(vlog_number(v));
   case V_REF:
      return vlog_hash_node(vlog_ref(v));
   case V_PARAM_ASSIGN:
      {
         uint32_t h = vlog_hash_node(vlog_value(v));
         if (vlog_has_ident(v))
            h ^= ident_hash(vlog_ident(v));
         return h;
      }
   default:
      return mix_bits_64((uintptr_t)v);
   }
}
