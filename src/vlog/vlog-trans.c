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
#include "common.h"
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

static tree_t trans_expr(vlog_node_t expr)
{
   switch (vlog_kind(expr)) {
   case V_NUMBER:
      {
         number_t n = vlog_number(expr);
         type_t std_int = std_type(NULL, STD_INTEGER);

         tree_t lit = tree_new(T_LITERAL);
         tree_set_subkind(lit, L_INT);
         tree_set_ival(lit, number_integer(n));
         tree_set_type(lit, std_int);

         return lit;
      }
   default:
      CANNOT_HANDLE(expr);
   }
}

static type_t trans_type(vlog_node_t decl, verilog_type_t scalar_type,
                         verilog_type_t packed_type)
{
   const int nranges = vlog_ranges(decl);
   if (nranges > 0) {
      type_t packed = verilog_type(packed_type);

      tree_t c = tree_new(T_CONSTRAINT);
      tree_set_subkind(c, C_INDEX);
      tree_set_loc(c, vlog_loc(decl));

      for (int i = 0; i < nranges; i++) {
         type_t index_type = index_type_of(packed, i);
         vlog_node_t vr = vlog_range(decl, i);

         tree_t left = trans_expr(vlog_left(vr));
         tree_t right = trans_expr(vlog_right(vr));

         const range_kind_t dir =
            assume_int(left) < assume_int(right) ? RANGE_TO : RANGE_DOWNTO;

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
      type_add_constraint(sub, c);

      return sub;
   }
   else
      return verilog_type(scalar_type);
}

static type_t trans_var_type(vlog_node_t decl)
{
   return trans_type(decl, VERILOG_LOGIC, VERILOG_PACKED_LOGIC);
}

static type_t trans_net_type(vlog_node_t decl)
{
   return trans_type(decl, VERILOG_RESOLVED_NET, VERILOG_RESOLVED_NET_ARRAY);
}

static void trans_port_decl(vlog_node_t decl, tree_t out)
{
   static const port_mode_t map[] = {
      [V_PORT_INPUT] = PORT_IN,
      [V_PORT_OUTPUT] = PORT_OUT,
      [V_PORT_INOUT] = PORT_INOUT,
   };

   const v_port_kind_t kind = vlog_subkind(decl);

   tree_t t = tree_new(T_PORT_DECL);
   tree_set_ident(t, vlog_ident(decl));
   tree_set_subkind(t, map[kind]);
   tree_set_class(t, C_SIGNAL);

   vlog_node_t net = vlog_ref(decl);
   if (vlog_is_net(net))
      tree_set_type(t, trans_net_type(net));
   else
      tree_set_type(t, trans_var_type(net));

   tree_add_port(out, t);
}

static void trans_var_decl(vlog_node_t decl, tree_t out)
{
   tree_t t = tree_new(T_SIGNAL_DECL);
   tree_set_ident(t, vlog_ident(decl));
   tree_set_type(t, trans_var_type(decl));

   tree_add_decl(out, t);
}

static void trans_net_decl(vlog_node_t decl, tree_t out)
{
   type_t type = trans_net_type(decl);

   tree_t t = tree_new(T_SIGNAL_DECL);
   tree_set_ident(t, vlog_ident(decl));
   tree_set_type(t, type);

   tree_add_decl(out, t);

   const vlog_net_kind_t kind = vlog_subkind(decl);
   if (kind == V_NET_SUPPLY0 || kind == V_NET_SUPPLY1) {
      type_t base = type_base_recur(type_elem_recur(type));
      assert(type_kind(base) == T_ENUM);

      const net_value_t init = (kind == V_NET_SUPPLY0 ? _SUPPLY0 : _SUPPLY1);
      tree_set_value(t, make_ref(type_enum_literal(base, init)));
   }
}

void vlog_trans(vlog_node_t mod, tree_t out)
{
   assert(is_top_level(mod));

   hset_t *ports = hset_new(16);
   const int nports = vlog_ports(mod);
   for (int i = 0; i < nports; i++) {
      vlog_node_t ref = vlog_port(mod, i);
      assert(vlog_kind(ref) == V_REF);

      vlog_node_t port = vlog_ref(ref);
      assert(vlog_kind(port) == V_PORT_DECL);
      trans_port_decl(port, out);

      // Do not translate the associated var/net declaration twice
      hset_insert(ports, vlog_ref(port));
   }

   const int ndecls = vlog_decls(mod);
   for (int i = 0; i < ndecls; i++) {
      vlog_node_t d = vlog_decl(mod, i);
      switch (vlog_kind(d)) {
      case V_PORT_DECL:
         break;   // Translated above
      case V_VAR_DECL:
         if (!hset_contains(ports, d))
            trans_var_decl(d, out);
         break;
      case V_NET_DECL:
         if (!hset_contains(ports, d))
            trans_net_decl(d, out);
         break;
      default:
         CANNOT_HANDLE(d);
      }
   }

   const int nstmts = vlog_stmts(mod);
   for (int i = 0; i < nstmts; i++) {
      vlog_node_t s = vlog_stmt(mod, i);

      tree_t w = tree_new(T_VERILOG);
      tree_set_ident(w, vlog_ident(s));
      tree_set_loc(w, vlog_loc(s));
      tree_set_vlog(w, s);

      tree_add_stmt(out, w);
   }

   hset_free(ports);
}
