//
//  Copyright (C) 2024-2025 Nick Gasson
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
#include "tree.h"
#include "type.h"
#include "vlog/vlog-defs.h"
#include "vlog/vlog-node.h"
#include "vlog/vlog-number.h"
#include "vlog/vlog-phase.h"
#include "vlog/vlog-util.h"

#include <assert.h>

#define CANNOT_HANDLE(v) do {                                           \
      fatal_at(vlog_loc(v), "cannot handle %s in %s" ,                  \
               vlog_kind_str(vlog_kind(v)), __FUNCTION__);              \
   } while (0)

typedef struct {
   tree_t  out;
   tree_t  chars[256];
} trans_gen_t;

static tree_t trans_expr(trans_gen_t *g, vlog_node_t v)
{
   switch (vlog_kind(v)) {
   case V_NUMBER:
      {
         number_t n = vlog_number(v);
         type_t std_int = std_type(NULL, STD_INTEGER);

         tree_t lit = tree_new(T_LITERAL);
         tree_set_subkind(lit, L_INT);
         tree_set_type(lit, std_int);

         if (number_is_defined(n))
            tree_set_ival(lit, number_integer(n));
         else
            tree_set_ival(lit, 0);

         return lit;
      }
   case V_STRING:
      {
         number_t n = vlog_number(v);
         type_t std_string = std_type(NULL, STD_STRING);
         type_t std_char = type_elem(std_string);

         tree_t str = tree_new(T_STRING);
         tree_set_type(str, std_string);

         const int width = number_width(n);
         for (int i = 0; i < width / 8; i++) {
            const int byte = number_byte(n, width/8 - i - 1);
            if (g->chars[byte] == NULL)
               g->chars[byte] = make_ref(type_enum_literal(std_char, byte));

            tree_add_char(str, g->chars[byte]);
         }

         return str;
      }
   default:
      return NULL;
   }
}

static type_t trans_sized_type(vlog_node_t decl, verilog_type_t packed_type,
                               int width)
{
   type_t packed = verilog_type(packed_type);

   tree_t c = tree_new(T_CONSTRAINT);
   tree_set_subkind(c, C_INDEX);
   tree_set_loc(c, vlog_loc(decl));

   type_t std_int = std_type(NULL, STD_INTEGER);

   tree_t left = tree_new(T_LITERAL);
   tree_set_subkind(left, L_INT);
   tree_set_type(left, std_int);
   tree_set_ival(left, width - 1);

   tree_t right = tree_new(T_LITERAL);
   tree_set_subkind(right, L_INT);
   tree_set_type(right, std_int);
   tree_set_ival(right, 0);

   tree_t r = tree_new(T_RANGE);
   tree_set_subkind(r, RANGE_DOWNTO);
   tree_set_left(r, left);
   tree_set_right(r, right);
   tree_set_loc(r, vlog_loc(decl));
   tree_set_type(r, std_int);

   tree_add_range(c, r);

   type_t sub = type_new(T_SUBTYPE);
   type_set_base(sub, packed);
   type_set_constraint(sub, c);

   return sub;
}

static type_t trans_type(trans_gen_t *g, vlog_node_t decl,
                         verilog_type_t scalar_type,
                         verilog_type_t packed_type)
{
   // TODO: cache decl ^ scalar_type ^ packed_type

   switch (vlog_kind(decl)) {
   case V_TYPE_DECL:
   case V_ENUM_DECL:
      return trans_type(g, vlog_type(decl), scalar_type, packed_type);
   case V_CLASS_DECL:
      return NULL;
   case V_DATA_TYPE:
      break;
   default:
      CANNOT_HANDLE(decl);
   }

   switch (vlog_subkind(decl)) {
   case DT_IMPLICIT:
   case DT_LOGIC:
   case DT_BIT:
      {
         const int nranges = vlog_ranges(decl);
         if (nranges == 0)
            return verilog_type(scalar_type);

         type_t packed = verilog_type(packed_type);

         tree_t c = tree_new(T_CONSTRAINT);
         tree_set_subkind(c, C_INDEX);
         tree_set_loc(c, vlog_loc(decl));

         for (int i = 0; i < nranges; i++) {
            type_t index_type = index_type_of(packed, i);
            vlog_node_t vr = vlog_range(decl, i);

            tree_t left = trans_expr(g, vlog_left(vr));
            tree_t right = trans_expr(g, vlog_right(vr));
            if (left == NULL || right == NULL)
               return packed;   // Non-literal bounds

            int64_t ileft, iright;
            const bool left_is_const = folded_int(left, &ileft);
            const bool right_is_const = folded_int(right, &iright);

            range_kind_t dir;
            if (left_is_const && right_is_const)
               dir = ileft < iright ? RANGE_TO : RANGE_DOWNTO;
            else if (left_is_const)
               dir = RANGE_TO;   // Heuristic
            else
               dir = RANGE_DOWNTO;   // Heuristic

            tree_t r = tree_new(T_RANGE);
            tree_set_subkind(r, dir);
            tree_set_left(r, left);
            tree_set_right(r, right);
            tree_set_loc(r, vlog_loc(decl));
            tree_set_type(r, index_type);

            tree_add_range(c, r);
         }

         type_t sub = type_new(T_SUBTYPE);
         type_set_base(sub, packed);
         type_set_constraint(sub, c);

         return sub;
      }

   case DT_INT:
   case DT_INTEGER:
      return trans_sized_type(decl, packed_type, 32);

   case DT_BYTE:
      return trans_sized_type(decl, packed_type, 8);

   case DT_SHORTINT:
      return trans_sized_type(decl, packed_type, 16);

   case DT_LONGINT:
      return trans_sized_type(decl, packed_type, 64);

   case DT_REAL:
      return std_type(NULL, STD_REAL);

   default:
      CANNOT_HANDLE(decl);
   }
}

static type_t trans_var_type(trans_gen_t *g, vlog_node_t v)
{
   vlog_node_t type = vlog_type(v);
   return trans_type(g, type, VERILOG_LOGIC, VERILOG_LOGIC_ARRAY);
}

static type_t trans_net_type(trans_gen_t *g, vlog_node_t v)
{
   vlog_node_t type = vlog_type(v);
   return trans_type(g, type, VERILOG_WIRE, VERILOG_WIRE_ARRAY);
}

static void trans_port_decl(trans_gen_t *g, vlog_node_t v)
{
   static const port_mode_t map[] = {
      [V_PORT_INPUT] = PORT_IN,
      [V_PORT_OUTPUT] = PORT_OUT,
      [V_PORT_INOUT] = PORT_INOUT,
   };

   const v_port_kind_t kind = vlog_subkind(v);

   tree_t t = tree_new(T_PORT_DECL);
   tree_set_ident(t, vlog_ident(v));
   tree_set_subkind(t, map[kind]);
   tree_set_class(t, C_SIGNAL);
   tree_set_loc(t, vlog_loc(v));

   vlog_node_t net = vlog_ref(v);
   if (vlog_is_net(net)) {
      type_t type = trans_net_type(g, net);
      tree_set_type(t, type);
   }
   else {
      type_t type = trans_var_type(g, net);
      tree_set_type(t, type);
   }

   tree_add_port(g->out, t);
}

static void trans_param_decl(trans_gen_t *g, vlog_node_t v)
{
   tree_t t = tree_new(T_GENERIC_DECL);
   tree_set_ident(t, vlog_ident(v));
   tree_set_class(t, C_CONSTANT);
   tree_set_subkind(t, PORT_IN);

   // XXX: should be integer for untyped only
   /// tree_set_type(t, trans_var_type(g, v));
   tree_set_type(t, std_type(NULL, STD_INTEGER));

   tree_add_generic(g->out, t);
}

static void trans_generic(trans_gen_t *g, vlog_node_t decl)
{
   tree_t wrap = tree_new(T_VERILOG);
   tree_set_vlog(wrap, decl);
   tree_set_ident(wrap, vlog_ident(decl));
   tree_set_loc(wrap, vlog_loc(decl));

   tree_add_decl(g->out, wrap);
}

void vlog_trans(vlog_node_t mod, tree_t out)
{
   trans_gen_t gen = {
      .out = out,
   };

   const int ndecls = vlog_decls(mod);
   for (int i = 0; i < ndecls; i++) {
      vlog_node_t d = vlog_decl(mod, i);
      switch (vlog_kind(d)) {
      case V_PORT_DECL:
         break;   // Translated below
      case V_PARAM_DECL:
         trans_param_decl(&gen, d);
         break;
      case V_FUNC_DECL:
      case V_TASK_DECL:
      case V_NET_DECL:
      case V_VAR_DECL:
      case V_LOCALPARAM:
         trans_generic(&gen, d);
         break;
      case V_GENVAR_DECL:
      case V_TYPE_DECL:
      case V_CLASS_DECL:
      case V_STRUCT_DECL:
         break;
      default:
         CANNOT_HANDLE(d);
      }
   }

   if (vlog_kind(mod) != V_BLOCK) {
      const int nports = vlog_ports(mod);

      for (int i = 0; i < nports; i++) {
         vlog_node_t ref = vlog_port(mod, i);
         assert(vlog_kind(ref) == V_REF);

         vlog_node_t port = vlog_ref(ref);
         assert(vlog_kind(port) == V_PORT_DECL);
         trans_port_decl(&gen, port);
      }
   }
}
