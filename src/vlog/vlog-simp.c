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
#include "diag.h"
#include "ident.h"
#include "vlog/vlog-node.h"
#include "vlog/vlog-number.h"
#include "vlog/vlog-phase.h"
#include "vlog/vlog-util.h"

#include <assert.h>
#include <inttypes.h>
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
      vlog_set_subkind(s, ST_SUPPLY);

      vlog_add_param(g, s);

      if (kind == V_NET_SUPPLY0)
         vlog_set_subkind(g, V_GATE_PULLDOWN);
      else
         vlog_set_subkind(g, V_GATE_PULLUP);

      vlog_node_t target = vlog_new(V_REF);
      vlog_set_loc(target, vlog_loc(decl));
      vlog_set_ident(target, name);
      vlog_set_ref(target, decl);

      vlog_set_target(g, target);

      vlog_add_stmt(mod, g);
   }

   if (vlog_has_value(decl)) {
      vlog_node_t value = vlog_value(decl);
      vlog_set_value(decl, NULL);

      ident_t id = vlog_ident(decl);
      const loc_t *loc = vlog_loc(decl);

      vlog_node_t ref = vlog_new(V_REF);
      vlog_set_ref(ref, decl);
      vlog_set_ident(ref, id);
      vlog_set_loc(ref, loc);

      vlog_node_t a = vlog_new(V_ASSIGN);
      vlog_set_target(a, ref);
      vlog_set_value(a, value);
      vlog_set_loc(a, loc);
      vlog_set_ident(a, ident_uniq("__assign#%s", istr(id)));

      vlog_add_stmt(mod, a);
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
   vlog_set_type(wire, vlog_type(decl));

   vlog_set_ref(decl, wire);
   vlog_add_decl(mod, wire);

   return decl;
}

static void build_sensitivity(vlog_node_t ctrl, vlog_node_t v)
{
   switch (vlog_kind(v)) {
   case V_NBASSIGN:
   case V_BASSIGN:
   case V_UNARY:
      build_sensitivity(ctrl, vlog_value(v));
      break;
   case V_BINARY:
      build_sensitivity(ctrl, vlog_left(v));
      build_sensitivity(ctrl, vlog_right(v));
      break;
   case V_REF:
   case V_BIT_SELECT:
   case V_PART_SELECT:
      {
         vlog_node_t e = vlog_new(V_EVENT);
         vlog_set_subkind(e, V_EVENT_LEVEL);
         vlog_set_value(e, v);
         vlog_set_loc(e, vlog_loc(ctrl));

         vlog_add_param(ctrl, e);
      }
      break;
   case V_NUMBER:
   case V_STRING:
      break;
   case V_IF:
      {
         const int nconds = vlog_conds(v);
         for (int i = 0; i < nconds; i++)
            build_sensitivity(ctrl, vlog_cond(v, i));
      }
      break;
   case V_COND:
      {
         if (vlog_has_value(v))
            build_sensitivity(ctrl, vlog_value(v));

         const int nstmts = vlog_stmts(v);
         for (int i = 0; i < nstmts; i++)
            build_sensitivity(ctrl, vlog_stmt(v, i));
      }
      break;
   case V_CASE:
      build_sensitivity(ctrl, vlog_value(v));
      // Fall-through
   case V_CASE_ITEM:
   case V_BLOCK:
      {
         const int nstmts = vlog_stmts(v);
         for (int i = 0; i < nstmts; i++)
            build_sensitivity(ctrl, vlog_stmt(v, i));
      }
      break;
   case V_CONCAT:
   case V_SYS_FCALL:
   case V_SYS_TCALL:
   case V_USER_FCALL:
   case V_USER_TCALL:
      {
         const int nparams = vlog_params(v);
         for (int i = 0; i < nparams; i++)
            build_sensitivity(ctrl, vlog_param(v, i));
      }
      break;
   case V_COND_EXPR:
      build_sensitivity(ctrl, vlog_value(v));
      build_sensitivity(ctrl, vlog_left(v));
      build_sensitivity(ctrl, vlog_right(v));
      break;
   default:
      fatal_at(vlog_loc(v), "cannot handle %s in build_sensitivity",
               vlog_kind_str(vlog_kind(v)));
   }
}

static vlog_node_t simp_timing(vlog_node_t v)
{
   vlog_node_t ctrl = vlog_value(v);

   if (vlog_kind(ctrl) == V_EVENT_CONTROL && vlog_params(ctrl) == 0) {
      const int nstmts = vlog_stmts(v);
      for (int i = 0; i < nstmts; i++)
         build_sensitivity(ctrl, vlog_stmt(v, i));

      if (vlog_params(ctrl) == 0)
         warn_at(vlog_loc(v), "timing statement has no sensitivities and "
                 "so will never trigger");
   }

   return v;
}

static vlog_node_t simp_data_type(vlog_node_t v)
{
   switch (vlog_subkind(v)) {
   case DT_INTEGER:
      {
         const loc_t *loc = vlog_loc(v);

         vlog_node_t left = vlog_new(V_NUMBER);
         vlog_set_number(left, number_new("31", NULL));
         vlog_set_loc(left, loc);

         vlog_node_t right = vlog_new(V_NUMBER);
         vlog_set_number(right, number_new("0", NULL));
         vlog_set_loc(right, loc);

         vlog_node_t r = vlog_new(V_DIMENSION);
         vlog_set_subkind(r, V_DIM_PACKED);
         vlog_set_left(r, left);
         vlog_set_right(r, right);
         vlog_set_loc(r, loc);

         vlog_set_subkind(v, DT_LOGIC);
         vlog_add_range(v, r);
      }
      break;
   }

   return v;
}

static vlog_node_t simp_ref(vlog_node_t v)
{
   vlog_node_t decl = vlog_ref(v);
   switch (vlog_kind(decl)) {
   case V_LOCALPARAM:
      {
         vlog_node_t value = vlog_value(decl);
         if (vlog_kind(value) == V_NUMBER)
            return value;
         else
            return v;
      }
   default:
      return v;
   }
}

static vlog_node_t simp_binary(vlog_node_t v)
{
   vlog_node_t left = vlog_left(v);
   vlog_node_t right = vlog_right(v);

   if (vlog_kind(left) != V_NUMBER || vlog_kind(right) != V_NUMBER)
      return v;

   number_t nleft = vlog_number(left);
   number_t nright = vlog_number(right);

   number_t result;
   switch (vlog_subkind(v)) {
   case V_BINARY_PLUS:
      result = number_add(nleft, nright);
      break;
   case V_BINARY_MINUS:
      result = number_sub(nleft, nright);
      break;
   case V_BINARY_LOG_EQ:
      result = number_logical_equal(nleft, nright);
      break;
   case V_BINARY_GT:
      result = number_greater(nleft, nright);
      break;
   default:
      return v;
   }

   vlog_node_t new = vlog_new(V_NUMBER);
   vlog_set_loc(new, vlog_loc(v));
   vlog_set_number(new, result);
   return new;
}

static vlog_node_t simp_if_generate(vlog_node_t v)
{
   const int nconds = vlog_conds(v);
   for (int i = 0; i < nconds; i++) {
      vlog_node_t c = vlog_cond(v, i);
      if (vlog_has_value(c)) {
         vlog_node_t value = vlog_value(c);
         if (vlog_kind(value) != V_NUMBER)
            return v;
         else if (!number_truthy(vlog_number(value)))
            continue;
      }

      assert(vlog_stmts(c) == 1);
      return vlog_stmt(c, 0);
   }

   return v;
}

static vlog_node_t vlog_simp_cb(vlog_node_t v, void *context)
{
   switch (vlog_kind(v)) {
   case V_NET_DECL:
      return simp_net_decl(v, context);
   case V_PORT_DECL:
      return simp_port_decl(v, context);
   case V_TIMING:
      return simp_timing(v);
   case V_DATA_TYPE:
      return simp_data_type(v);
   case V_BINARY:
      return simp_binary(v);
   case V_REF:
      return simp_ref(v);
   case V_IF_GENERATE:
      return simp_if_generate(v);
   default:
      return v;
   }
}

void vlog_simp(vlog_node_t mod)
{
   assert(is_top_level(mod));
   vlog_rewrite(mod, vlog_simp_cb, mod);
}
