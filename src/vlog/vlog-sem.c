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

typedef uint32_t type_mask_t;
#define TM(x) (1 << (x))
#define TM_ERROR  0
#define TM_ANY    ~0
#define TM_CLASS  TM(31)
#define TM_STRUCT TM(30)
#define TM_ENUM   TM(29)
#define TM_INTEGRAL \
   (TM(DT_LOGIC) | TM(DT_INTEGER) | TM(DT_BYTE) | TM(DT_SHORTINT) \
    | TM(DT_INT) | TM(DT_LONGINT) | TM(DT_TIME) | TM(DT_BIT) | TM_ENUM)

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
      if (vlog_has_ref(v))
         return false;
      else {
         assert(error_count() > 0);
         return true;
      }
   case V_USER_FCALL:
   case V_USER_TCALL:
   case V_HIER_REF:
      // May not have reported error yet
      return !vlog_has_ref(v);
   case V_PORT_DECL:
   default:
      return false;
   }
}

static type_mask_t intersect_type_mask(type_mask_t a, type_mask_t b)
{
   if (a & b)
      return a & b;
   else if ((a & TM_INTEGRAL) && (b & TM_INTEGRAL))
      return (a & TM_INTEGRAL) | (b & TM_INTEGRAL);
   else
      return TM_ERROR;
}

static type_mask_t get_type_mask(vlog_node_t v)
{
   switch (vlog_kind(v)) {
   case V_DATA_TYPE:
      return TM(vlog_subkind(v));
   case V_STRUCT_DECL:
      return TM_STRUCT;
   case V_CLASS_DECL:
      return TM_CLASS;
   case V_ENUM_DECL:
      return TM_ENUM;
   case V_REF:
   case V_USER_FCALL:
   case V_MEMBER_REF:
      if (vlog_has_ref(v))
         return get_type_mask(vlog_ref(v));
      else
         return TM_ERROR;
   case V_NUMBER:
   case V_CONCAT:
   case V_SYS_FCALL:
      return TM_INTEGRAL;
   case V_REAL:
      return TM(DT_REAL);
   case V_NULL:
      return TM_CLASS;
   case V_ENUM_NAME:
      return TM_ENUM;
   case V_PARAM_DECL:
   case V_LOCALPARAM:
   case V_GENVAR_DECL:
   case V_VAR_DECL:
   case V_NET_DECL:
   case V_PORT_DECL:
   case V_TF_PORT_DECL:
   case V_TYPE_DECL:
   case V_FUNC_DECL:
      return get_type_mask(vlog_type(v));
   case V_BIT_SELECT:
   case V_PART_SELECT:
   case V_UNARY:
      return get_type_mask(vlog_value(v));
   case V_BINARY:
   case V_COND_EXPR:
      return intersect_type_mask(get_type_mask(vlog_left(v)),
                                 get_type_mask(vlog_right(v)));
   default:
      CANNOT_HANDLE(v);
   }
}

static const char *type_mask_str(type_mask_t tm)
{
   switch (tm) {
   case TM(DT_LOGIC): return "logic";
   case TM(DT_BIT): return "bit";
   case TM(DT_BYTE): return "byte";
   case TM(DT_INT): return "int";
   case TM(DT_SHORTINT): return "shortint";
   case TM_CLASS: return "class";
   case TM_ENUM: return "enum";
   default: return (tm & TM_INTEGRAL) ? "integral" : "unknown";
   }
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
         case V_GENVAR_DECL:
         case V_ENUM_NAME:
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

static void vlog_check_variable_lvalue(vlog_node_t v, vlog_node_t where)
{
   switch (vlog_kind(v)) {
   case V_VAR_DECL:
   case V_FUNC_DECL:
   case V_STRUCT_DECL:
   case V_ENUM_DECL:
   case V_UNION_DECL:
   case V_GENVAR_DECL:
      break;
   case V_REF:
      if (vlog_has_ref(v))
         vlog_check_variable_lvalue(vlog_ref(v), v);
      break;
   case V_BIT_SELECT:
   case V_PART_SELECT:
   case V_MEMBER_REF:
      vlog_check_variable_lvalue(vlog_value(v), v);
      break;
   case V_CONCAT:
      {
         const int nparams = vlog_params(v);
         for (int i = 0; i < nparams; i++) {
            vlog_node_t p = vlog_param(v, i);
            vlog_check_variable_lvalue(p, p);
         }
      }
      break;
   case V_PORT_DECL:
      if (vlog_has_ref(v)) {
         vlog_check_variable_lvalue(vlog_ref(v), where);
         break;
      }
      // Fall-through
   default:
      {
         diag_t *d = diag_new(DIAG_ERROR, vlog_loc(where));
         name_for_diag(d, where, "target");
         diag_suppress(d, has_error(where));
         diag_printf(d, " cannot be assigned in a procedural block");
         diag_emit(d);
      }
      break;
   }
}

static void vlog_check_nbassign(vlog_node_t stmt)
{
   vlog_node_t target = vlog_target(stmt);
   vlog_check_variable_lvalue(target, target);
}

static void vlog_check_bassign(vlog_node_t stmt)
{
   vlog_node_t target = vlog_target(stmt);
   vlog_check_variable_lvalue(target, target);
}

static void vlog_check_op_assign(vlog_node_t stmt)
{
   vlog_node_t target = vlog_target(stmt);
   vlog_check_variable_lvalue(target, target);
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
   case V_REF:
      if (vlog_has_ref(v))
         vlog_check_net_lvalue(vlog_ref(v), v);
      break;
   case V_BIT_SELECT:
   case V_PART_SELECT:
      vlog_check_net_lvalue(vlog_value(v), v);
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
      break;
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

      if (!vlog_equal_node(aleft, bleft) || !vlog_equal_node(aright, bright)) {
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
   if (vlog_has_ref(port))
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

static void vlog_check_part_select(vlog_node_t v)
{
   if (vlog_subkind(v) == V_RANGE_CONST) {
      vlog_node_t left = vlog_left(v);
      vlog_check_const_expr(left);
   }

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

static void vlog_check_user_tcall(vlog_node_t v)
{
   vlog_node_t func = vlog_ref(v);
   if (vlog_kind(func) != V_TASK_DECL) {
      diag_t *d = diag_new(DIAG_ERROR, vlog_loc(v));
      diag_printf(d, "'%s' is not a task", istr(vlog_ident(func)));
      diag_hint(d, vlog_loc(func), "'%s' declared here",
                istr(vlog_ident(func)));
      diag_emit(d);
      return;
   }

   vlog_check_call_args(v, func);
}

static void vlog_check_deassign(vlog_node_t v)
{
   error_at(vlog_loc(v), "procedural deassign statements are not supported "
            "as they are being considered for removal from the System Verilog "
            "standard");
}

static void vlog_check_return(vlog_node_t v)
{
   if (!vlog_has_ref(v))
      return;   // Was earlier error

   vlog_node_t subr = vlog_ref(v);
   const vlog_kind_t kind = vlog_kind(subr);

   if (kind == V_FUNC_DECL && !vlog_has_value(v))
      error_at(vlog_loc(v), "return statement in a non-void function must "
               "have an expression");
   else if (kind == V_TASK_DECL && vlog_has_value(v))
      error_at(vlog_loc(v), "return statement in a task cannot have an "
               "expression");
}

static void vlog_check_concat(vlog_node_t v)
{
   if (vlog_has_value(v))
      vlog_check_const_expr(vlog_value(v));
}

static void vlog_check_module(vlog_node_t v)
{
   const int nports = vlog_ports(v);
   for (int i = 0; i < nports; i++) {
      vlog_node_t ref = vlog_port(v, i);
      if (!vlog_has_ref(ref))
         error_at(vlog_loc(ref), "missing port declaration for '%s'",
                  istr(vlog_ident(ref)));
   }
}

static void vlog_check_enum_name(vlog_node_t v)
{
   if (vlog_has_value(v))
      vlog_check_const_expr(vlog_value(v));
}

static void vlog_check_hier_ref(vlog_node_t v)
{
   vlog_node_t inst = vlog_ref(v);
   if (vlog_kind(inst) != V_MOD_INST)
      error_at(vlog_loc(v), "prefix of hierarchical identifier is not an "
               "instance");
}

static void vlog_check_binary(vlog_node_t v)
{
   const type_mask_t lmask = get_type_mask(vlog_left(v));
   const type_mask_t rmask = get_type_mask(vlog_right(v));
   const type_mask_t comb = intersect_type_mask(lmask, rmask);

   type_mask_t allow = TM_ANY;

   if (comb & allow)
      return;

   diag_t *d = diag_new(DIAG_ERROR, vlog_loc(v));
   diag_printf(d, "invalid operands for binary expression");
   diag_hint(d, vlog_loc(v), "have '%s' and '%s'", type_mask_str(lmask),
             type_mask_str(rmask));
   diag_emit(d);
}

static vlog_node_t vlog_check_cb(vlog_node_t v, void *ctx)
{
   if (has_error(v))
      return v;

   switch (vlog_kind(v)) {
   case V_MODULE:
      vlog_check_module(v);
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
   case V_OP_ASSIGN:
      vlog_check_op_assign(v);
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
   case V_USER_TCALL:
      vlog_check_user_tcall(v);
      break;
   case V_DEASSIGN:
      vlog_check_deassign(v);
      break;
   case V_RETURN:
      vlog_check_return(v);
      break;
   case V_CONCAT:
      vlog_check_concat(v);
      break;
   case V_ENUM_NAME:
      vlog_check_enum_name(v);
      break;
   case V_HIER_REF:
      vlog_check_hier_ref(v);
      break;
   case V_BINARY:
      vlog_check_binary(v);
      break;
   case V_CASE_ITEM:
   case V_UDP_LEVEL:
   case V_UDP_EDGE:
   case V_UDP_ENTRY:
   case V_UDP_TABLE:
   case V_GATE_INST:
   case V_ENUM_DECL:
   case V_STRUCT_DECL:
   case V_UNION_DECL:
   case V_WAIT:
   case V_PARAM_DECL:
   case V_SPECPARAM:
   case V_FOREVER:
   case V_REPEAT:
   case V_TYPE_DECL:
   case V_DATA_TYPE:
   case V_SPECIFY:
   case V_STRENGTH:
   case V_NET_DECL:
   case V_VAR_DECL:
   case V_PORT_CONN:
   case V_PARAM_ASSIGN:
   case V_FUNC_DECL:
   case V_TASK_DECL:
   case V_EMPTY:
   case V_COND_EXPR:
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
   case V_FORK:
   case V_SYS_TCALL:
   case V_SYS_FCALL:
   case V_NUMBER:
   case V_STRING:
   case V_REAL:
   case V_IF:
   case V_UNARY:
   case V_REF:
   case V_COND:
   case V_PREFIX:
   case V_POSTFIX:
   case V_BIT_SELECT:
   case V_VOID_CALL:
   case V_GENVAR_DECL:
   case V_FOR_GENERATE:
   case V_FORCE:
   case V_RELEASE:
   case V_TF_PORT_DECL:
   case V_WHILE:
   case V_DO_WHILE:
   case V_MEMBER_REF:
   case V_CLASS_DECL:
   case V_PROGRAM:
   case V_NULL:
   case V_CLASS_NEW:
   case V_MIN_TYP_MAX:
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
