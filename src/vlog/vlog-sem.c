//
//  Copyright (C) 2022-2025  Nick Gasson
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
#include <stdlib.h>

static void name_for_diag(diag_t *d, vlog_node_t v, const char *alt)
{
   switch (vlog_kind(v)) {
   case V_REF:
      diag_printf(d, "'%s'", istr(vlog_ident(v)));
      break;
   default:
      diag_printf(d, "%s", alt);
      break;
   }
}

static bool has_error(vlog_node_t v)
{
   switch (vlog_kind(v)) {
   case V_REF:
   case V_BIT_SELECT:
   case V_USER_FCALL:
      if (vlog_has_ref(v))
         return false;
      else {
         assert(error_count() > 0);
         return true;
      }
   case V_PORT_DECL:
      if (vlog_has_ref(v))
         return false;
      else
         return true; // XXX: check this
   default:
      return false;
   }
}

static bool constant_equal(vlog_node_t a, vlog_node_t b)
{
   if (vlog_kind(a) != V_NUMBER || vlog_kind(b) != V_NUMBER)
      return false;

   number_t an = vlog_number(a);
   number_t bn = vlog_number(b);

   return number_equal(an, bn);
}

static void vlog_check_const_expr(vlog_node_t expr)
{
   switch (vlog_kind(expr)) {
   case V_NUMBER:
      break;
   case V_REF:
      if (vlog_has_ref(expr)) {
         vlog_node_t decl = vlog_ref(expr);
         switch (vlog_kind(decl)) {
         case V_PARAM_DECL:
         case V_LOCALPARAM:
            break;
         default:
            {
               diag_t *d = diag_new(DIAG_ERROR, vlog_loc(expr));
               diag_printf(d, "cannot reference %s '%s' in constant expression",
                           vlog_is_net(decl) ? "net" : "variable",
                           istr(vlog_ident(decl)));
               diag_hint(d, vlog_loc(decl), "%s declared here",
                         istr(vlog_ident(decl)));
               diag_emit(d);
            }
            break;
         }
      }
      break;
   case V_COND_EXPR:
      vlog_check_const_expr(vlog_value(expr));
      // Fall-through
   case V_BINARY:
      vlog_check_const_expr(vlog_left(expr));
      vlog_check_const_expr(vlog_right(expr));
      break;
   case V_UNARY:
      vlog_check_const_expr(vlog_value(expr));
      break;
   case V_BIT_SELECT:
      {
         const int nparams = vlog_params(expr);
         for (int i = 0; i < nparams; i++)
            vlog_check_const_expr(vlog_param(expr, i));
      }
      break;
   default:
      error_at(vlog_loc(expr), "expression is not a constant");
      break;
   }
}

static void vlog_check_variable_target(vlog_node_t target)
{
   if (vlog_is_net(target)) {
      diag_t *d = diag_new(DIAG_ERROR, vlog_loc(target));
      name_for_diag(d, target, "target");
      diag_printf(d, " cannot be assigned in a procedural block");
      diag_emit(d);
   }
}

static void vlog_check_nbassign(vlog_node_t stmt)
{
   vlog_node_t target = vlog_target(stmt);
   vlog_check_variable_target(target);
}

static void vlog_check_bassign(vlog_node_t stmt)
{
   vlog_node_t target = vlog_target(stmt);
   vlog_check_variable_target(target);
}

static void vlog_check_net_lvalue(vlog_node_t v, vlog_node_t where)
{
   switch (vlog_kind(v)) {
   case V_NET_DECL:
      break;
   case V_PORT_DECL:
      if (vlog_has_ref(v))
         vlog_check_net_lvalue(vlog_ref(v), where);
      break;
   case V_BIT_SELECT:
   case V_REF:
      if (vlog_has_ref(v))
         vlog_check_net_lvalue(vlog_ref(v), v);
      break;
   case V_CONCAT:
      {
         const int nparams = vlog_params(v);
         for (int i = 0; i < nparams; i++) {
            vlog_node_t p = vlog_param(v, i);
            vlog_check_net_lvalue(p, p);
         }
      }
      break;
   default:
      {
         diag_t *d = diag_new(DIAG_ERROR, vlog_loc(where));
         name_for_diag(d, where, "target");
         diag_suppress(d, has_error(where));
         diag_printf(d, " cannot be driven by continuous assignment");
         diag_emit(d);
      }
   }
}

static void vlog_check_assign(vlog_node_t stmt)
{
   vlog_node_t target = vlog_target(stmt);
   vlog_check_net_lvalue(target, target);
}

static void vlog_check_consistent(vlog_node_t a, vlog_node_t b)
{
   vlog_node_t at = vlog_type(a);
   vlog_node_t bt = vlog_type(b);

   if (at == bt)
      return;

   const int aranges = vlog_ranges(at);
   assert(aranges == vlog_ranges(bt));

   for (int i = 0; i < aranges; i++) {
      vlog_node_t ar = vlog_range(at, i);
      vlog_node_t br = vlog_range(bt, i);

      vlog_node_t aleft = vlog_left(ar);
      vlog_node_t bleft = vlog_left(br);

      vlog_node_t aright = vlog_right(ar);
      vlog_node_t bright = vlog_right(br);

      if (!constant_equal(aleft, bleft) || !constant_equal(aright, bright)) {
         diag_t *d = diag_new(DIAG_ERROR, vlog_loc(br));
         diag_printf(d, "inconsistent dimensions for '%s'",
                     istr(vlog_ident(b)));
         diag_hint(d, vlog_loc(a), "earlier declaration here");
         diag_emit(d);
      }
   }
}

static void vlog_check_port_decl(vlog_node_t port)
{
   vlog_check_consistent(port, vlog_ref(port));
}

static void vlog_check_primitive(vlog_node_t udp)
{
   const int nports = vlog_ports(udp);
   for (int i = 0; i < nports; i++) {
      vlog_node_t p = vlog_port(udp, i);

      if (vlog_has_ref(p)) {
         vlog_node_t decl = vlog_ref(p);
         assert(vlog_kind(decl) == V_PORT_DECL);

         if (i == 0 && vlog_subkind(decl) != V_PORT_OUTPUT) {
            diag_t *d = diag_new(DIAG_ERROR, vlog_loc(p));
            diag_printf(d, "the first port of a primitive must be an output");
            diag_hint(d, vlog_loc(decl), "port declaration here");
            diag_emit(d);
         }
         else if (i > 0 && vlog_subkind(decl) != V_PORT_INPUT) {
            diag_t *d = diag_new(DIAG_ERROR, vlog_loc(p));
            diag_printf(d, "all ports of a primitive except the first must "
                        "be inputs");
            diag_hint(d, vlog_loc(decl), "port declaration here");
            diag_emit(d);
         }
      }
   }

   assert(vlog_stmts(udp) == 1);

   vlog_node_t table = vlog_stmt(udp, 0);
   assert(vlog_kind(table) == V_UDP_TABLE);

   const vlog_udp_kind_t kind = vlog_subkind(table);
   const int expect = kind == V_UDP_SEQ ? nports + 1 : nports;

   const int nparams = vlog_params(table);
   for (int i = 0; i < nparams; i++) {
      vlog_node_t row = vlog_param(table, i);
      assert(vlog_kind(row) == V_UDP_ENTRY);

      const int nsymbols = vlog_params(row);
      if (nsymbols != expect) {
         error_at(vlog_loc(row), "expected %d symbols in UDP table entry but "
                  "have %d", expect, nsymbols);
         break;
      }
   }
}

static void vlog_check_dimension(vlog_node_t v)
{
   vlog_node_t left = vlog_left(v);
   vlog_check_const_expr(left);

   vlog_node_t right = vlog_right(v);
   vlog_check_const_expr(right);
}

static void vlog_check_bit_select(vlog_node_t v)
{
   assert(vlog_has_ref(v) || error_count() > 0);
}

static void vlog_check_part_select(vlog_node_t v)
{
   vlog_node_t left = vlog_left(v);
   vlog_check_const_expr(left);

   vlog_node_t right = vlog_right(v);
   vlog_check_const_expr(right);
}

static void vlog_check_localparam(vlog_node_t decl)
{
   if (!vlog_has_value(decl))
      error_at(vlog_loc(decl), "local parameter declaration must have a "
               "default value");
}

static void vlog_check_case(vlog_node_t v)
{
   bool seen_default = false;
   const int nstmts = vlog_stmts(v);
   for (int i = 0; i < nstmts; i++) {
      vlog_node_t item = vlog_stmt(v, i);
      assert(vlog_kind(item) == V_CASE_ITEM);

      const int nparams = vlog_params(item);
      if (nparams == 0 && !seen_default)
         seen_default = true;
      else if (nparams == 0)
         error_at(vlog_loc(item), "multiple default statements within a single "
                  "case statement");
   }
}

static void vlog_check_if_generate(vlog_node_t v)
{
   const int nconds = vlog_conds(v);
   for (int i = 0; i < nconds; i++) {
      vlog_node_t c = vlog_cond(v, i);
      assert(vlog_kind(c) == V_COND);

      if (vlog_has_value(c)) {
         vlog_node_t value = vlog_value(c);
         vlog_check_const_expr(value);
      }
   }
}

static void vlog_check_call_args(vlog_node_t v, vlog_node_t sub)
{
   const int nparams = vlog_params(v);
   const int nports = vlog_ports(sub);

   if (nparams != nports) {
      diag_t *d = diag_new(DIAG_ERROR, vlog_loc(v));
      diag_printf(d, "expected %d argument%s for '%s' but have %d", nports,
                  nports != 1 ? "s" : "", istr(vlog_ident(sub)), nparams);
      diag_hint(d, vlog_loc(sub), "'%s' declared here", istr(vlog_ident(sub)));
      diag_emit(d);
      return;
   }
}

static void vlog_check_user_fcall(vlog_node_t v)
{
   vlog_node_t func = vlog_ref(v);
   if (vlog_kind(func) != V_FUNC_DECL) {
      diag_t *d = diag_new(DIAG_ERROR, vlog_loc(v));
      diag_printf(d, "'%s' is not a function", istr(vlog_ident(func)));
      diag_hint(d, vlog_loc(func), "'%s' declared here",
                istr(vlog_ident(func)));
      diag_emit(d);
      return;
   }

   vlog_check_call_args(v, func);
}

static vlog_node_t vlog_check_cb(vlog_node_t v, void *ctx)
{
   if (has_error(v))
      return v;

   switch (vlog_kind(v)) {
   case V_MODULE:
      break;
   case V_PRIMITIVE:
      vlog_check_primitive(v);
      break;
   case V_NBASSIGN:
      vlog_check_nbassign(v);
      break;
   case V_BASSIGN:
      vlog_check_bassign(v);
      break;
   case V_ASSIGN:
      vlog_check_assign(v);
      break;
   case V_PORT_DECL:
      vlog_check_port_decl(v);
      break;
   case V_DIMENSION:
      vlog_check_dimension(v);
      break;
   case V_BIT_SELECT:
      vlog_check_bit_select(v);
      break;
   case V_PART_SELECT:
      vlog_check_part_select(v);
      break;
   case V_LOCALPARAM:
      vlog_check_localparam(v);
      break;
   case V_CASE:
      vlog_check_case(v);
      break;
   case V_IF_GENERATE:
      vlog_check_if_generate(v);
      break;
   case V_USER_FCALL:
      vlog_check_user_fcall(v);
      break;
   case V_CASE_ITEM:
   case V_UDP_LEVEL:
   case V_UDP_EDGE:
   case V_UDP_ENTRY:
   case V_UDP_TABLE:
   case V_GATE_INST:
   case V_ENUM_NAME:
   case V_ENUM_DECL:
   case V_STRUCT_DECL:
   case V_UNION_DECL:
   case V_WAIT:
   case V_PARAM_DECL:
   case V_FOREVER:
   case V_TYPE_DECL:
   case V_DATA_TYPE:
   case V_SPECIFY:
   case V_STRENGTH:
   case V_NET_DECL:
   case V_VAR_DECL:
   case V_PORT_CONN:
   case V_PARAM_ASSIGN:
   case V_FUNC_DECL:
   case V_EMPTY:
   case V_COND_EXPR:
   case V_CONCAT:
   case V_FOR_LOOP:
   case V_FOR_INIT:
   case V_FOR_STEP:
   case V_MOD_INST:
   case V_INST_LIST:
   case V_ALWAYS:
   case V_INITIAL:
   case V_TIMING:
   case V_EVENT:
   case V_EVENT_CONTROL:
   case V_DELAY_CONTROL:
   case V_BLOCK:
   case V_SYS_TCALL:
   case V_SYS_FCALL:
   case V_NUMBER:
   case V_STRING:
   case V_IF:
   case V_BINARY:
   case V_UNARY:
   case V_REF:
   case V_COND:
      break;
   default:
      fatal_at(vlog_loc(v), "cannot check verilog node %s",
               vlog_kind_str(vlog_kind(v)));
   }

   return v;
}

void vlog_check(vlog_node_t v)
{
   assert(is_top_level(v));
   vlog_rewrite(v, vlog_check_cb, NULL);
}
