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
#include "common.h"
#include "diag.h"
#include "hash.h"
#include "ident.h"
#include "vlog/vlog-node.h"
#include "vlog/vlog-number.h"
#include "vlog/vlog-phase.h"
#include "vlog/vlog-util.h"

#include <assert.h>
#include <stdlib.h>

typedef struct _vlog_scope vlog_scope_t;

struct _vlog_scope {
   vlog_scope_t *parent;
   vlog_node_t   container;
   hash_t       *symbols;
};

static vlog_scope_t *top_scope = NULL;

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

static void push_scope(vlog_node_t container)
{
   vlog_scope_t *s = xcalloc(sizeof(vlog_scope_t));
   s->symbols   = hash_new(128);
   s->container = container;
   s->parent    = top_scope;

   top_scope = s;
}

static void pop_scope(void)
{
   assert(top_scope != NULL);

   vlog_scope_t *tmp = top_scope;
   top_scope = top_scope->parent;

   hash_free(tmp->symbols);
   free(tmp);
}

static bool has_error(vlog_node_t v)
{
   switch (vlog_kind(v)) {
   case V_REF:
      return !vlog_has_ref(v);
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

static void vlog_insert_decl(vlog_node_t v)
{
   ident_t id = vlog_ident(v);

   vlog_node_t exist = hash_get(top_scope->symbols, id);
   if (exist != NULL) {
      diag_t *d = diag_new(DIAG_ERROR, vlog_loc(v));
      diag_printf(d, "duplicate declaration of %s", istr(id));
      diag_hint(d, vlog_loc(exist), "%s was previously declared here",
                istr(id));
      diag_hint(d, vlog_loc(v), "duplicate declaration");
      diag_emit(d);
      return;
   }

   hash_put(top_scope->symbols, id, v);
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

static void vlog_check_ref(vlog_node_t ref)
{
   ident_t id = vlog_ident(ref);

   vlog_node_t decl = hash_get(top_scope->symbols, id);
   if (decl == NULL) {
      error_at(vlog_loc(ref), "no visible declaration for %s", istr(id));
      return;
   }

   vlog_set_ref(ref, decl);
}

static void vlog_check_implicit_net(vlog_node_t v)
{
   switch (vlog_kind(v)) {
   case V_REF:
      {
         ident_t id = vlog_ident(v);

         vlog_node_t decl = hash_get(top_scope->symbols, id);
         if (decl == NULL) {
            // See 1800-2017 section 6.10 "Implicit declarations"
            decl = vlog_new(V_NET_DECL);
            vlog_set_ident(decl, id);
            vlog_set_loc(decl, vlog_loc(v));
            vlog_set_subkind(decl, V_NET_WIRE);

            vlog_node_t dt = vlog_new(V_DATA_TYPE);
            vlog_set_subkind(dt, DT_LOGIC);

            vlog_set_type(decl, dt);

            assert(vlog_kind(top_scope->container) == V_MODULE);
            vlog_add_decl(top_scope->container, decl);

            hash_put(top_scope->symbols, id, decl);
         }

         vlog_set_ref(v, decl);
      }
      break;

   default:
      vlog_check(v);
      break;
   }
}

static void vlog_check_number(vlog_node_t num)
{

}

static void vlog_check_string(vlog_node_t num)
{

}

static void vlog_check_binary(vlog_node_t op)
{
   vlog_check(vlog_left(op));
   vlog_check(vlog_right(op));
}

static void vlog_check_unary(vlog_node_t op)
{
   vlog_check(vlog_value(op));
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
   vlog_check(target);

   if (vlog_has_delay(stmt))
      vlog_check(vlog_delay(stmt));

   vlog_node_t value = vlog_value(stmt);
   vlog_check(value);

   vlog_check_variable_target(target);
}

static void vlog_check_bassign(vlog_node_t stmt)
{
   vlog_node_t target = vlog_target(stmt);
   vlog_check(target);

   if (vlog_has_delay(stmt))
      vlog_check(vlog_delay(stmt));

   vlog_node_t value = vlog_value(stmt);
   vlog_check(value);

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
   vlog_check_implicit_net(target);

   vlog_node_t value = vlog_value(stmt);
   vlog_check(value);

   vlog_check_net_lvalue(target, target);
}

static void vlog_check_timing(vlog_node_t timing)
{
   vlog_check(vlog_value(timing));

   const int nstmts = vlog_stmts(timing);
   for (int i = 0; i < nstmts; i++)
      vlog_check(vlog_stmt(timing, i));
}

static void vlog_check_event(vlog_node_t event)
{
   vlog_check(vlog_value(event));
}

static void vlog_check_event_control(vlog_node_t ctrl)
{
   const int nparams = vlog_params(ctrl);
   for (int i = 0; i < nparams; i++)
      vlog_check(vlog_param(ctrl, i));
}

static void vlog_check_delay_control(vlog_node_t delay)
{
   vlog_check(vlog_value(delay));
}

static void vlog_check_always(vlog_node_t always)
{
   const int nstmts = vlog_stmts(always);
   for (int i = 0; i < nstmts; i++)
      vlog_check(vlog_stmt(always, i));
}

static void vlog_check_initial(vlog_node_t initial)
{
   const int nstmts = vlog_stmts(initial);
   for (int i = 0; i < nstmts; i++)
      vlog_check(vlog_stmt(initial, i));
}

static void vlog_check_seq_block(vlog_node_t block)
{
   const int nstmts = vlog_stmts(block);
   for (int i = 0; i < nstmts; i++)
      vlog_check(vlog_stmt(block, i));
}

static void vlog_check_sys_tcall(vlog_node_t call)
{
   const int nparams = vlog_params(call);
   for (int i = 0; i < nparams; i++)
      vlog_check(vlog_param(call, i));
}

static void vlog_check_sys_fcall(vlog_node_t call)
{
   const int nparams = vlog_params(call);
   for (int i = 0; i < nparams; i++)
      vlog_check(vlog_param(call, i));
}

static void vlog_check_if(vlog_node_t stmt)
{
   const int nconds = vlog_conds(stmt);
   for (int i = 0; i < nconds; i++) {
      vlog_node_t c = vlog_cond(stmt, i);

      if (vlog_has_value(c))
         vlog_check(vlog_value(c));
      else
         assert(i == nconds - 1);

      const int nstmts = vlog_stmts(c);
      for (int i = 0; i < nstmts; i++)
         vlog_check(vlog_stmt(c, i));
   }
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

static void vlog_check_data_type(vlog_node_t dt)
{
   const int nranges = vlog_ranges(dt);
   for (int i = 0; i < nranges; i++)
      vlog_check(vlog_range(dt, i));
}

static void vlog_check_port_decl(vlog_node_t port)
{
   vlog_check(vlog_type(port));

   ident_t id = vlog_ident(port);
   vlog_node_t exist = hash_get(top_scope->symbols, id);
   if (exist != NULL) {
      const vlog_kind_t kind = vlog_kind(exist);
      if (kind == V_VAR_DECL || kind == V_NET_DECL) {
         vlog_set_ref(port, exist);
         vlog_check_consistent(exist, port);
         hash_put(top_scope->symbols, id, port);
         return;
      }
   }

   vlog_insert_decl(port);
}

static void vlog_check_net_decl(vlog_node_t net)
{
   vlog_check(vlog_type(net));

   vlog_node_t exist = hash_get(top_scope->symbols, vlog_ident(net));
   if (exist != NULL && vlog_kind(exist) == V_PORT_DECL
       && !vlog_has_ref(exist)) {
      vlog_set_ref(exist, net);
      vlog_check_consistent(exist, net);
   }
   else
      vlog_insert_decl(net);
}

static void vlog_check_var_decl(vlog_node_t var)
{
   vlog_check(vlog_type(var));

   vlog_node_t exist = hash_get(top_scope->symbols, vlog_ident(var));
   if (exist != NULL && vlog_kind(exist) == V_PORT_DECL
       && !vlog_has_ref(exist)) {
      vlog_set_ref(exist, var);
      vlog_check_consistent(exist, var);
   }
   else
      vlog_insert_decl(var);
}

static void vlog_check_module(vlog_node_t module)
{
   assert(top_scope == NULL);
   push_scope(module);

   const int ndecls = vlog_decls(module);
   for (int i = 0; i < ndecls; i++)
      vlog_check(vlog_decl(module, i));

   const int nports = vlog_ports(module);
   for (int i = 0; i < nports; i++)
      vlog_check(vlog_port(module, i));

   const int nstmts = vlog_stmts(module);
   for (int i = 0; i < nstmts; i++)
      vlog_check(vlog_stmt(module, i));

   pop_scope();
}

static void vlog_check_primitive(vlog_node_t udp)
{
   assert(top_scope == NULL);
   push_scope(udp);

   const int ndecls = vlog_decls(udp);
   for (int i = 0; i < ndecls; i++)
      vlog_check(vlog_decl(udp, i));

   const int nports = vlog_ports(udp);
   for (int i = 0; i < nports; i++) {
      vlog_node_t p = vlog_port(udp, i);
      vlog_check(p);

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

   const int nstmts = vlog_stmts(table);
   for (int i = 0; i < nstmts; i++)
      vlog_check(vlog_stmt(table, i));

   const int nparams = vlog_params(table);
   for (int i = 0; i < nparams; i++) {
      vlog_node_t row = vlog_param(table, i);
      assert(vlog_kind(row) == V_UDP_ENTRY);

      const char *spec = vlog_text(row), *sp = spec;

      for (int pos = 0; pos < nports - 1; pos++) {
         if (*sp == ':') {
            error_at(vlog_loc(row), "missing symbol for input %s",
                     istr(vlog_ident(vlog_port(udp, pos))));
            break;
         }
         else if (*sp == '(') {
            assert(sp[3] == ')');
            sp += 4;
         }
         else
            sp++;
      }

      if (*sp != ':')
         error_at(vlog_loc(row), "too many symbols in UDP table entry");
   }

   pop_scope();
}

static void vlog_check_gate_inst(vlog_node_t g)
{
   vlog_node_t target = vlog_target(g);
   vlog_check_implicit_net(target);

   const int nparams = vlog_params(g);
   for (int i = 0; i < nparams; i++)
      vlog_check_implicit_net(vlog_param(g, i));

   if (vlog_has_ident(g))
      vlog_insert_decl(g);
   else
      vlog_set_ident(g, ident_uniq("#gate"));
}

static void vlog_check_mod_inst(vlog_node_t inst)
{
   const int nparams = vlog_params(inst);
   for (int i = 0; i < nparams; i++)
      vlog_check_implicit_net(vlog_param(inst, i));

   vlog_insert_decl(inst);
}

static void vlog_check_inst_list(vlog_node_t v)
{
   const int nstmts = vlog_stmts(v);
   for (int i = 0; i < nstmts; i++)
      vlog_check(vlog_stmt(v, i));
}

static void vlog_check_dimension(vlog_node_t dim)
{
   vlog_node_t left = vlog_left(dim);
   vlog_check(left);
   vlog_check_const_expr(left);

   vlog_node_t right = vlog_right(dim);
   vlog_check(right);
   vlog_check_const_expr(right);
}

static void vlog_check_bit_select(vlog_node_t bsel)
{
   vlog_check_ref(bsel);

   const int nparams = vlog_params(bsel);
   for (int i = 0; i < nparams; i++)
      vlog_check(vlog_param(bsel, i));
}

static void vlog_check_forever(vlog_node_t stmt)
{
   const int nstmts = vlog_stmts(stmt);
   for (int i = 0; i < nstmts; i++)
      vlog_check(vlog_stmt(stmt, i));
}

static void vlog_check_specify(vlog_node_t spec)
{

}

static void vlog_check_type_decl(vlog_node_t spec)
{

}

static void vlog_check_enum_decl(vlog_node_t spec)
{

}

static void vlog_check_struct_union_decl(vlog_node_t decl)
{
   push_scope(decl);

   const int ndecls = vlog_decls(decl);
   for (int i = 0; i < ndecls; i++) {
      vlog_node_t d = vlog_decl(decl, i);
      vlog_check(d);
   }

   pop_scope();
}

static void vlog_check_wait(vlog_node_t stmt)
{
   vlog_check(vlog_value(stmt));

   const int nstmts = vlog_stmts(stmt);
   for (int i = 0; i < nstmts; i++)
      vlog_check(vlog_stmt(stmt, i));
}

static void vlog_check_param_decl(vlog_node_t decl)
{
   vlog_insert_decl(decl);

   if (vlog_has_value(decl))
      vlog_check(vlog_value(decl));
   else if (vlog_kind(decl) == V_LOCALPARAM)
      error_at(vlog_loc(decl), "local parameter declaration must have a "
               "default value");
}

static void vlog_check_cond_expr(vlog_node_t expr)
{
   vlog_check(vlog_value(expr));
   vlog_check(vlog_left(expr));
   vlog_check(vlog_right(expr));
}

static void vlog_check_concat(vlog_node_t expr)
{
   const int nparams = vlog_params(expr);
   for (int i = 0; i < nparams; i++) {
      vlog_node_t p = vlog_param(expr, i);
      vlog_check(p);
   }
}

static void vlog_check_for_loop(vlog_node_t v)
{
   vlog_check(vlog_left(v));
   vlog_check(vlog_right(v));

   const int nstmts = vlog_stmts(v);
   for (int i = 0; i < nstmts; i++)
      vlog_check(vlog_stmt(v, i));
}

static void vlog_check_for_init(vlog_node_t v)
{
   const int ndecls = vlog_decls(v);
   for (int i = 0; i < ndecls; i++)
      vlog_check(vlog_decl(v, i));

   const int nstmts = vlog_stmts(v);
   for (int i = 0; i < nstmts; i++)
      vlog_check(vlog_stmt(v, i));
}

static void vlog_check_for_step(vlog_node_t v)
{
   const int nstmts = vlog_stmts(v);
   for (int i = 0; i < nstmts; i++)
      vlog_check(vlog_stmt(v, i));
}

static void vlog_check_case(vlog_node_t v)
{
   vlog_check(vlog_value(v));

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

      for (int i = 0; i < nparams; i++)
         vlog_check(vlog_param(item, i));

      const int nstmts = vlog_stmts(item);
      for (int i = 0; i < nstmts; i++)
         vlog_check(vlog_stmt(item, i));
   }
}

void vlog_check(vlog_node_t v)
{
   switch (vlog_kind(v)) {
   case V_MODULE:
      vlog_check_module(v);
      break;
   case V_PRIMITIVE:
      vlog_check_primitive(v);
      break;
   case V_ALWAYS:
      vlog_check_always(v);
      break;
   case V_INITIAL:
      vlog_check_initial(v);
      break;
   case V_TIMING:
      vlog_check_timing(v);
      break;
   case V_EVENT:
      vlog_check_event(v);
      break;
   case V_EVENT_CONTROL:
      vlog_check_event_control(v);
      break;
   case V_DELAY_CONTROL:
      vlog_check_delay_control(v);
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
   case V_REF:
      vlog_check_ref(v);
      break;
   case V_PORT_DECL:
      vlog_check_port_decl(v);
      break;
   case V_NET_DECL:
      vlog_check_net_decl(v);
      break;
   case V_VAR_DECL:
      vlog_check_var_decl(v);
      break;
   case V_SEQ_BLOCK:
      vlog_check_seq_block(v);
      break;
   case V_SYS_TCALL:
      vlog_check_sys_tcall(v);
      break;
   case V_SYS_FCALL:
      vlog_check_sys_fcall(v);
      break;
   case V_NUMBER:
      vlog_check_number(v);
      break;
   case V_STRING:
      vlog_check_string(v);
      break;
   case V_IF:
      vlog_check_if(v);
      break;
   case V_BINARY:
      vlog_check_binary(v);
      break;
   case V_UNARY:
      vlog_check_unary(v);
      break;
   case V_GATE_INST:
      vlog_check_gate_inst(v);
      break;
   case V_MOD_INST:
      vlog_check_mod_inst(v);
      break;
   case V_INST_LIST:
      vlog_check_inst_list(v);
      break;
   case V_DIMENSION:
      vlog_check_dimension(v);
      break;
   case V_BIT_SELECT:
      vlog_check_bit_select(v);
      break;
   case V_FOREVER:
      vlog_check_forever(v);
      break;
   case V_SPECIFY:
      vlog_check_specify(v);
      break;
   case V_STRENGTH:
      break;
   case V_TYPE_DECL:
      vlog_check_type_decl(v);
      break;
   case V_DATA_TYPE:
      vlog_check_data_type(v);
      break;
   case V_ENUM_DECL:
      vlog_check_enum_decl(v);
      break;
   case V_STRUCT_DECL:
   case V_UNION_DECL:
      vlog_check_struct_union_decl(v);
      break;
   case V_WAIT:
      vlog_check_wait(v);
      break;
   case V_PARAM_DECL:
   case V_LOCALPARAM:
      vlog_check_param_decl(v);
      break;
   case V_EMPTY:
      break;
   case V_COND_EXPR:
      vlog_check_cond_expr(v);
      break;
   case V_CONCAT:
      vlog_check_concat(v);
      break;
   case V_FOR_LOOP:
      vlog_check_for_loop(v);
      break;
   case V_FOR_INIT:
      vlog_check_for_init(v);
      break;
   case V_FOR_STEP:
      vlog_check_for_step(v);
      break;
   case V_CASE:
      vlog_check_case(v);
      break;
   default:
      fatal_at(vlog_loc(v), "cannot check verilog node %s",
               vlog_kind_str(vlog_kind(v)));
   }
}
