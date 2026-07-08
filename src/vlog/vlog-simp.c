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
#include "hash.h"
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

   return decl;
}

static vlog_node_t simp_port_decl(vlog_node_t v, vlog_node_t mod)
{
   if (vlog_has_ref(v))
      return v;

   vlog_node_t dt = vlog_type(v), decl;
   if (is_implicit_data_type(dt)) {
      decl = vlog_new(V_NET_DECL);
      vlog_set_subkind(decl, V_NET_WIRE);
   }
   else
      decl = vlog_new(V_VAR_DECL);

   vlog_set_loc(decl, vlog_loc(v));
   vlog_set_ident(decl, vlog_ident(v));
   vlog_set_type(decl, vlog_type(v));

   if (vlog_has_value(v))
      vlog_set_value(decl, vlog_value(v));

   vlog_set_ref(v, decl);
   vlog_add_decl(mod, decl);

   return v;
}

static void build_sensitivity(vlog_node_t ctrl, vlog_node_t v, hset_t *set,
                              bool is_comb)
{
   switch (vlog_kind(v)) {
   case V_NBASSIGN:
   case V_BASSIGN:
   case V_UNARY:
   case V_OP_ASSIGN:
      build_sensitivity(ctrl, vlog_value(v), set, is_comb);
      break;
   case V_BINARY:
      build_sensitivity(ctrl, vlog_left(v), set, is_comb);
      build_sensitivity(ctrl, vlog_right(v), set, is_comb);
      break;
   case V_REF:
      {
         vlog_node_t d = vlog_ref(v);
         if (hset_contains(set, d))
            break;

         hset_insert(set, d);
      }
      // Fall-through
   case V_BIT_SELECT:
   case V_PART_SELECT:
   case V_MEMBER_REF:
   case V_HIER_REF:
      {
         vlog_node_t lsp = vlog_longest_static_prefix(v);
         if (lsp == NULL)
            break;
         else if (lsp == v) {
            vlog_node_t e = vlog_new(V_EVENT);
            vlog_set_subkind(e, V_EVENT_LEVEL);
            vlog_set_value(e, v);
            vlog_set_loc(e, vlog_loc(ctrl));

            vlog_add_param(ctrl, e);
        }
        else {
            build_sensitivity(ctrl, vlog_value(v), set, is_comb);

            switch (vlog_kind(v)) {
            case V_BIT_SELECT:
               {
                  const int nparams = vlog_params(v);
                  for (int i = 0; i < nparams; i++)
                     build_sensitivity(ctrl, vlog_param(v, i), set, is_comb);
               }
               break;
            case V_PART_SELECT:
               build_sensitivity(ctrl, vlog_left(v), set, is_comb);
               build_sensitivity(ctrl, vlog_right(v), set, is_comb);
               break;
            case V_MEMBER_REF:
               break;
            default:
               should_not_reach_here();
            }
         }
      }
      break;
   case V_NUMBER:
   case V_STRING:
      break;
   case V_IF:
      {
         const int nconds = vlog_conds(v);
         for (int i = 0; i < nconds; i++)
            build_sensitivity(ctrl, vlog_cond(v, i), set, is_comb);
      }
      break;
   case V_COND:
      {
         if (vlog_has_value(v))
            build_sensitivity(ctrl, vlog_value(v), set, is_comb);

         const int nstmts = vlog_stmts(v);
         for (int i = 0; i < nstmts; i++)
            build_sensitivity(ctrl, vlog_stmt(v, i), set, is_comb);
      }
      break;
   case V_CASE:
      build_sensitivity(ctrl, vlog_value(v), set, is_comb);
      // Fall-through
   case V_CASE_ITEM:
   case V_TIMING:
      {
         const int nstmts = vlog_stmts(v);
         for (int i = 0; i < nstmts; i++)
            build_sensitivity(ctrl, vlog_stmt(v, i), set, is_comb);
      }
      break;
   case V_FUNC_DECL:
   case V_TASK_DECL:
      {
         // Ignore task/function ports
         const int nports = vlog_ports(v);
         for (int i = 0; i < nports; i++)
            hset_insert(set, vlog_port(v, i));
      }
      // Fall-through
   case V_SEQ_BLOCK:
      {
         // Ignore local variables
         const int ndecls = vlog_decls(v);
         for (int i = 0; i < ndecls; i++)
            hset_insert(set, vlog_decl(v, i));

         const int nstmts = vlog_stmts(v);
         for (int i = 0; i < nstmts; i++)
            build_sensitivity(ctrl, vlog_stmt(v, i), set, is_comb);
      }
      break;
   case V_USER_FCALL:
   case V_USER_TCALL:
      if (is_comb)
         build_sensitivity(ctrl, vlog_ref(v), set, is_comb);
      // Fall-through
   case V_CONCAT:
   case V_SYS_FCALL:
   case V_SYS_TCALL:
      {
         const int nparams = vlog_params(v);
         for (int i = 0; i < nparams; i++)
            build_sensitivity(ctrl, vlog_param(v, i), set, is_comb);
      }
      break;
   case V_COND_EXPR:
      build_sensitivity(ctrl, vlog_value(v), set, is_comb);
      build_sensitivity(ctrl, vlog_left(v), set, is_comb);
      build_sensitivity(ctrl, vlog_right(v), set, is_comb);
      break;
   case V_FOR_LOOP:
      {
         build_sensitivity(ctrl, vlog_left(v), set, is_comb);
         if (vlog_has_value(v))
            build_sensitivity(ctrl, vlog_value(v), set, is_comb);
         build_sensitivity(ctrl, vlog_right(v), set, is_comb);
         const int nstmts = vlog_stmts(v);
         for (int i = 0; i < nstmts; i++)
            build_sensitivity(ctrl, vlog_stmt(v, i), set, is_comb);
      }
      break;
   case V_FOR_INIT:
   case V_FOR_STEP:
      {
         const int nstmts = vlog_stmts(v);
         for (int i = 0; i < nstmts; i++)
            build_sensitivity(ctrl, vlog_stmt(v, i), set, is_comb);
      }
      break;
   case V_POSTFIX:
   case V_PREFIX:
      build_sensitivity(ctrl, vlog_target(v), set, is_comb);
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
      hset_t *set = hset_new(16);

      const int nstmts = vlog_stmts(v);
      for (int i = 0; i < nstmts; i++)
         build_sensitivity(ctrl, vlog_stmt(v, i), set, false);

      if (vlog_params(ctrl) == 0)
         warn_at(vlog_loc(v), "timing statement has no sensitivities and "
                 "so will never trigger");

      hset_free(set);
   }

   return v;
}

static vlog_node_t simp_always(vlog_node_t v)
{
   switch (vlog_subkind(v)) {
   case V_ALWAYS_COMB:
   case V_ALWAYS_LATCH:
      {
         // See 1800-2023 section 9.2.2.2.1
         vlog_node_t ctrl = vlog_new(V_EVENT_CONTROL);
         vlog_set_loc(ctrl, vlog_loc(v));

         hset_t *set = hset_new(16);

         vlog_node_t s0 = vlog_stmt(v, 0);
         build_sensitivity(ctrl, s0, set, true);

         hset_free(set);

         vlog_node_t timing = vlog_new(V_TIMING);
         vlog_set_loc(timing, vlog_loc(v));
         vlog_set_value(timing, ctrl);

         vlog_node_t b = vlog_new(V_SEQ_BLOCK);
         vlog_set_loc(b, vlog_loc(v));
         vlog_add_stmt(b, s0);
         vlog_add_stmt(b, timing);

         vlog_node_t new = vlog_new(V_ALWAYS);
         vlog_set_loc(new, vlog_loc(v));
         vlog_set_subkind(new, V_ALWAYS_PLAIN);
         vlog_set_ident(new, vlog_ident(v));
         vlog_add_stmt(new, b);

         return new;
      }
   default:
      return v;
   }
}

static vlog_node_t simp_enum_decl(vlog_node_t v)
{
   vlog_node_t last = NULL;
   const int ndecls = vlog_decls(v);
   for (int i = 0; i < ndecls; i++) {
      vlog_node_t d = vlog_decl(v, i);
      if (!vlog_has_value(d)) {
         if (last == NULL) {
            vlog_node_t n = vlog_new(V_NUMBER);
            vlog_set_loc(n, vlog_loc(d));
            vlog_set_number(n, number_from_int(i));

            vlog_set_value(d, n);
         }
         else if (vlog_kind(last) == V_NUMBER) {
            number_t next = number_add(vlog_number(last), number_from_int(1));

            vlog_node_t n = vlog_new(V_NUMBER);
            vlog_set_loc(n, vlog_loc(d));
            vlog_set_number(n, next);

            vlog_set_value(d, n);
         }
         else {
            vlog_node_t one = vlog_new(V_NUMBER);
            vlog_set_number(one, number_from_int(1));

            vlog_node_t plus = vlog_new(V_BINARY);
            vlog_set_loc(plus, vlog_loc(d));
            vlog_set_subkind(plus, V_BINARY_PLUS);
            vlog_set_left(plus, last);
            vlog_set_right(plus, one);

            vlog_set_value(d, plus);
         }
      }
      else
         last = vlog_value(d);
   }

   return v;
}

static vlog_node_t simp_concat(vlog_node_t v)
{
   if (vlog_params(v) != 1 || vlog_has_value(v))
      return v;

   vlog_node_t p0 = vlog_param(v, 0);
   if (vlog_kind(p0) == V_NUMBER)
      return p0;

   return v;
}

static vlog_node_t simp_call_args(vlog_node_t v)
{
   vlog_node_t sub = vlog_ref(v);

   const int nparams = vlog_params(v);
   const int nports = vlog_ports(sub);

   for (int i = 0; i < nports; i++) {
      if (i < nparams) {
         vlog_node_t p = vlog_param(v, i);
         if (vlog_kind(p) == V_EMPTY)
            vlog_set_param(v, i, vlog_value(vlog_port(sub, i)));
      }
      else
         vlog_add_param(v, vlog_value(vlog_port(sub, i)));
   }

   return v;
}

static vlog_node_t simp_wait(vlog_node_t v)
{
   vlog_node_t expr = vlog_value(v);

   vlog_node_t not = vlog_new(V_UNARY);
   vlog_set_loc(not, vlog_loc(expr));
   vlog_set_subkind(not, V_UNARY_NOT);
   vlog_set_value(not, expr);

   vlog_node_t wh = vlog_new(V_WHILE);
   vlog_set_loc(wh, vlog_loc(v));
   vlog_set_value(wh, not);

   vlog_node_t ctrl = vlog_new(V_EVENT_CONTROL);
   vlog_set_loc(ctrl, vlog_loc(v));

   hset_t *set = hset_new(16);
   build_sensitivity(ctrl, expr, set, false);
   hset_free(set);

   vlog_node_t timing = vlog_new(V_TIMING);
   vlog_set_loc(timing, vlog_loc(v));
   vlog_set_value(timing, ctrl);
   vlog_add_stmt(wh, timing);

   const int nstmts = vlog_stmts(v);
   if (nstmts == 0)
      return wh;

   vlog_node_t block = vlog_new(V_SEQ_BLOCK);
   vlog_set_loc(block, vlog_loc(v));
   vlog_add_stmt(block, wh);

   for (int i = 0; i < nstmts; i++)
      vlog_add_stmt(block, vlog_stmt(v, i));

   return block;
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
   case V_ALWAYS:
      return simp_always(v);
   case V_ENUM_DECL:
      return simp_enum_decl(v);
   case V_CONCAT:
      return simp_concat(v);
   case V_WAIT:
      return simp_wait(v);
   case V_USER_FCALL:
   case V_USER_TCALL:
      return simp_call_args(v);
   default:
      return v;
   }
}

void vlog_simp(vlog_node_t mod)
{
   vlog_rewrite(mod, NULL, vlog_simp_cb, mod);
}
