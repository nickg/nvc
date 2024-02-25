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
#include "diag.h"
#include "ident.h"
#include "vlog/vlog-node.h"
#include "vlog/vlog-number.h"
#include "vlog/vlog-phase.h"

#include <assert.h>
#include <string.h>
#include <stdlib.h>

static vlog_node_t simp_net_decl(vlog_node_t decl, vlog_node_t mod)
{
   const vlog_net_kind_t kind = vlog_subkind(decl);
   if (kind == V_NET_SUPPLY0 || kind == V_NET_SUPPLY1) {
      vlog_node_t g = vlog_new(V_GATE_INST);
      vlog_set_loc(g, vlog_loc(decl));

      ident_t name = vlog_ident(decl);
      ident_t id = ident_prefix(name, ident_new("pull"), '#');
      vlog_set_ident(g, id);

      vlog_node_t s = vlog_new(V_STRENGTH);
      vlog_add_param(g, s);

      if (kind == V_NET_SUPPLY0) {
         vlog_set_subkind(g, V_GATE_PULLDOWN);
         vlog_set_subkind(s, V_STRENGTH_SUPPLY0);
      }
      else {
         vlog_set_subkind(g, V_GATE_PULLUP);
         vlog_set_subkind(s, V_STRENGTH_SUPPLY1);
      }

      vlog_node_t target = vlog_new(V_REF);
      vlog_set_loc(target, vlog_loc(decl));
      vlog_set_ident(target, name);
      vlog_set_ref(target, decl);

      vlog_set_target(g, target);

      vlog_add_stmt(mod, g);
   }

   return decl;
}

static vlog_node_t simp_port_decl(vlog_node_t decl, vlog_node_t mod)
{
   if (vlog_has_ref(decl))
      return decl;

   vlog_node_t wire = vlog_new(V_NET_DECL);
   vlog_set_subkind(wire, V_NET_WIRE);
   vlog_set_loc(wire, vlog_loc(decl));
   vlog_set_ident(wire, vlog_ident(decl));

   vlog_set_ref(decl, wire);
   vlog_add_decl(mod, wire);

   return decl;
}

static vlog_node_t vlog_simp_cb(vlog_node_t v, void *context)
{
   switch (vlog_kind(v)) {
   case V_NET_DECL:
      return simp_net_decl(v, context);
   case V_PORT_DECL:
      return simp_port_decl(v, context);
   default:
      return v;
   }
}

void vlog_simp(vlog_node_t mod)
{
   assert(vlog_kind(mod) == V_MODULE);

   vlog_rewrite(mod, vlog_simp_cb, mod);
}
