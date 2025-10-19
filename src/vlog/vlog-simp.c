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
   case V_BLOCK:
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

         vlog_node_t b = vlog_new(V_BLOCK);
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
   case V_BINARY_TIMES:
      result = number_mul(nleft, nright);
      break;
   case V_BINARY_DIVIDE:
      result = number_div(nleft, nright);
      break;
   case V_BINARY_EXP:
      result = number_exp(nleft, nright);
      break;
   case V_BINARY_SHIFT_LL:
      result = number_shl(nleft, nright);
      break;
   case V_BINARY_LOG_EQ:
      result = number_logical_equal(nleft, nright);
      break;
   case V_BINARY_LOG_NEQ:
      result = number_not(number_logical_equal(nleft, nright));
      break;
   case V_BINARY_GT:
      result = number_greater(nleft, nright);
      break;
   case V_BINARY_GEQ:
      result = number_greater_equal(nleft, nright);
      break;
   case V_BINARY_LT:
      result = number_less(nleft, nright);
      break;
   case V_BINARY_LEQ:
      result = number_less_equal(nleft, nright);
      break;
   case V_BINARY_LOG_AND:
      result = number_from_bool(number_truthy(nleft) && number_truthy(nright));
      break;
   case V_BINARY_LOG_OR:
      result = number_from_bool(number_truthy(nleft) || number_truthy(nright));
      break;
   case V_BINARY_AND:
      result = number_and2(nleft, nright);
      break;
   case V_BINARY_OR:
      result = number_or2(nleft, nright);
      break;
   case V_BINARY_XOR:
      result = number_xor2(nleft, nright);
      break;
   default:
      return v;
   }

   vlog_node_t new = vlog_new(V_NUMBER);
   vlog_set_loc(new, vlog_loc(v));
   vlog_set_number(new, result);
   return new;
}

static vlog_node_t simp_unary(vlog_node_t v)
{
   vlog_node_t value = vlog_value(v);

   if (vlog_kind(value) != V_NUMBER)
      return v;

   number_t n = vlog_number(value);

   number_t result;
   switch (vlog_subkind(v)) {
   case V_UNARY_NEG:
      result = number_negate(n);
      break;
   case V_UNARY_IDENTITY:
      result = n;
      break;
   case V_UNARY_NOT:
      result = number_from_bool(!number_truthy(n));
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

      return vlog_stmts(c) == 1 ? vlog_stmt(c, 0) : NULL;
   }

   return NULL;   // None of the conditions are true
}

static vlog_node_t simp_sys_fcall(vlog_node_t v)
{
   switch (is_well_known(vlog_ident(v))) {
   case W_DLR_CLOG2:
      {
         vlog_node_t arg = vlog_param(v, 0);
         if (vlog_kind(arg) == V_NUMBER) {
            number_t n = vlog_number(arg);
            if (number_is_defined(n)) {
               vlog_node_t new = vlog_new(V_NUMBER);
               vlog_set_loc(new, vlog_loc(v));
               vlog_set_number(new, number_from_int(ilog2(number_integer(n))));
               return new;
            }
         }

         return v;
      }
   default:
      return v;
   }
}

static vlog_node_t simp_cond_expr(vlog_node_t v)
{
   vlog_node_t test = vlog_value(v);
   if (vlog_kind(test) != V_NUMBER)
      return v;

   number_t n = vlog_number(test);
   return number_truthy(n) ? vlog_left(v) : vlog_right(v);
}

static vlog_node_t simp_enum_decl(vlog_node_t v)
{
   if (!vlog_has_type(v)) {
      vlog_node_t dt = vlog_new(V_DATA_TYPE);
      vlog_set_subkind(dt, DT_INT);
      vlog_set_loc(dt, vlog_loc(v));

      vlog_set_type(v, dt);
   }

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

static vlog_node_t vlog_simp_cb(vlog_node_t v, void *context)
{
   switch (vlog_kind(v)) {
   case V_NET_DECL:
      return simp_net_decl(v, context);
   case V_PORT_DECL:
      return simp_port_decl(v, context);
   case V_TIMING:
      return simp_timing(v);
   case V_BINARY:
      return simp_binary(v);
   case V_UNARY:
      return simp_unary(v);
   case V_REF:
      return simp_ref(v);
   case V_IF_GENERATE:
      return simp_if_generate(v);
   case V_SYS_FCALL:
      return simp_sys_fcall(v);
   case V_COND_EXPR:
      return simp_cond_expr(v);
   case V_ALWAYS:
      return simp_always(v);
   case V_ENUM_DECL:
      return simp_enum_decl(v);
   default:
      return v;
   }
}

void vlog_simp(vlog_node_t mod)
{
   vlog_rewrite(mod, vlog_simp_cb, mod);
}
