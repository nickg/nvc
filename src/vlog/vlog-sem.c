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
#define TM_EMPTY  TM(31)
#define TM_CONST  TM(30)
#define TM_CLASS  TM(29)
#define TM_STRUCT TM(28)
#define TM_ENUM   TM(27)
#define TM_INTEGRAL \
   (TM(DT_LOGIC) | TM(DT_INTEGER) | TM(DT_BYTE) | TM(DT_SHORTINT) \
    | TM(DT_INT) | TM(DT_LONGINT) | TM(DT_TIME) | TM(DT_BIT) | TM_ENUM)
#define TM_REAL (TM(DT_REAL) | TM(DT_SHORTREAL))

static void vlog_check_decls(vlog_node_t v);
static void vlog_check_stmts(vlog_node_t v);
static void vlog_check_ranges(vlog_node_t v);
static type_mask_t vlog_check_expr(vlog_node_t v);
static type_mask_t vlog_check_const_expr(vlog_node_t v);

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
   case V_ENUM_NAME:
      return TM_ENUM | TM_CONST;
   case V_PARAM_DECL:
   case V_LOCALPARAM:
   case V_GENVAR_DECL:
      return get_type_mask(vlog_type(v)) | TM_CONST;
   case V_VAR_DECL:
   case V_NET_DECL:
   case V_PORT_DECL:
   case V_TF_PORT_DECL:
   case V_TYPE_DECL:
   case V_FUNC_DECL:
      return get_type_mask(vlog_type(v));
   default:
      CANNOT_HANDLE(v);
   }
}

static const char *type_mask_str(type_mask_t tm)
{
   switch (tm & ~(TM_CONST)) {
   case TM(DT_LOGIC): return "logic";
   case TM(DT_BIT): return "bit";
   case TM(DT_BYTE): return "byte";
   case TM(DT_INT): return "int";
   case TM(DT_SHORTINT): return "shortint";
   case TM(DT_REAL): return "real";
   case TM(DT_TIME): return "time";
   case TM_CLASS: return "class";
   case TM_ENUM: return "enum";
   default: return (tm & TM_INTEGRAL) ? "integral" : "unknown";
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
      return;
   case V_REF:
      if (vlog_has_ref(v))
         vlog_check_variable_lvalue(vlog_ref(v), v);
      return;
   case V_BIT_SELECT:
   case V_PART_SELECT:
   case V_MEMBER_REF:
      vlog_check_variable_lvalue(vlog_value(v), v);
      return;
   case V_CONCAT:
      {
         const int nparams = vlog_params(v);
         for (int i = 0; i < nparams; i++) {
            vlog_node_t p = vlog_param(v, i);
            vlog_check_variable_lvalue(p, p);
         }
      }
      return;
   case V_TF_PORT_DECL:
      if (vlog_subkind(v) != V_PORT_INPUT)
         return;
      break;
   case V_PORT_DECL:
      if (vlog_has_ref(v)) {
         vlog_check_variable_lvalue(vlog_ref(v), where);
         return;
      }
      break;
   default:
      break;
   }

   diag_t *d = diag_new(DIAG_ERROR, vlog_loc(where));
   name_for_diag(d, where, "target");
   diag_printf(d, " cannot be assigned in a procedural block");
   diag_emit(d);
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
         diag_printf(d, " cannot be driven by continuous assignment");
         diag_emit(d);
      }
      break;
   }
}

static void vlog_check_decls(vlog_node_t v)
{
   const int ndecls = vlog_decls(v);
   for (int i = 0; i < ndecls; i++)
      vlog_check(vlog_decl(v, i));
}

static void vlog_check_stmts(vlog_node_t v)
{
   const int nstmts = vlog_stmts(v);
   for (int i = 0; i < nstmts; i++)
      vlog_check(vlog_stmt(v, i));
}

static void vlog_check_ranges(vlog_node_t v)
{
   const int nranges = vlog_ranges(v);
   for (int i = 0; i < nranges; i++)
      vlog_check(vlog_range(v, i));
}

static void vlog_check_params(vlog_node_t v)
{
   const int nparams = vlog_params(v);
   for (int i = 0; i < nparams; i++)
      vlog_check_expr(vlog_param(v, i));
}

static void vlog_check_nbassign(vlog_node_t stmt)
{
   vlog_node_t target = vlog_target(stmt);
   vlog_check_expr(target);

   vlog_check_variable_lvalue(target, target);

   vlog_check_expr(vlog_value(stmt));
}

static void vlog_check_bassign(vlog_node_t stmt)
{
   vlog_node_t target = vlog_target(stmt);
   vlog_check_expr(target);

   vlog_check_variable_lvalue(target, target);

   vlog_check_expr(vlog_value(stmt));
}

static void vlog_check_assign(vlog_node_t v)
{
   vlog_node_t target = vlog_target(v);
   vlog_check_expr(target);

   vlog_check_net_lvalue(target, target);

   vlog_check_expr(vlog_value(v));
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

static void vlog_check_port_decl(vlog_node_t v)
{
   if (vlog_has_ref(v))
      vlog_check_consistent(v, vlog_ref(v));

   if (vlog_has_value(v))
      vlog_check_expr(vlog_value(v));
}

static void vlog_check_net_decl(vlog_node_t v)
{
   vlog_check(vlog_type(v));
   vlog_check_ranges(v);

   if (vlog_has_value(v))
      vlog_check_expr(vlog_value(v));
}

static void vlog_check_var_decl(vlog_node_t v)
{
   vlog_check(vlog_type(v));
   vlog_check_ranges(v);

   if (vlog_has_value(v))
      vlog_check_expr(vlog_value(v));
}

static void vlog_check_param_decl(vlog_node_t v)
{
   if (vlog_has_value(v))
      vlog_check_expr(vlog_value(v));
}

static void vlog_check_genvar_decl(vlog_node_t v)
{
}

static void vlog_check_type_decl(vlog_node_t v)
{
   vlog_check(vlog_type(v));
}

static void vlog_check_data_type(vlog_node_t v)
{
   vlog_check_ranges(v);
}

static void vlog_check_enum_decl(vlog_node_t v)
{
   vlog_check_decls(v);
}

static void vlog_check_struct_decl(vlog_node_t v)
{
   vlog_check_decls(v);
}

static void vlog_check_union_decl(vlog_node_t v)
{
   vlog_check_decls(v);
}

static void vlog_check_tf_decl(vlog_node_t v)
{
   vlog_check_decls(v);
   vlog_check_stmts(v);
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

static void vlog_check_localparam(vlog_node_t v)
{
   if (vlog_has_value(v))
      vlog_check_expr(vlog_value(v));
   else
      error_at(vlog_loc(v), "local parameter declaration must have a "
               "default value");
}

static void vlog_check_case(vlog_node_t v)
{
   vlog_check_expr(vlog_value(v));

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

      vlog_check_stmts(item);
   }
}

static void vlog_check_for_loop(vlog_node_t v)
{
   vlog_node_t init = vlog_left(v);
   assert(vlog_kind(init) == V_FOR_INIT);
   vlog_check_decls(init);
   vlog_check_stmts(init);

   vlog_check_expr(vlog_value(v));

   vlog_node_t step = vlog_right(v);
   assert(vlog_kind(step) == V_FOR_STEP);
   vlog_check_stmts(step);

   vlog_check_stmts(v);
}

static void vlog_check_repeat(vlog_node_t v)
{
   vlog_check_expr(vlog_value(v));

   vlog_check_stmts(v);
}

static void vlog_check_while(vlog_node_t v)
{
   vlog_check_expr(vlog_value(v));

   vlog_check_stmts(v);
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

   for (int i = 0; i < nparams; i++)
      vlog_check_expr(vlog_param(v, i));
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

static void vlog_check_super_call(vlog_node_t v)
{
   // TODO: vlog_check_call_args()
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

static void vlog_check_sys_tcall(vlog_node_t v)
{
   vlog_check_params(v);
}

static void vlog_check_timing(vlog_node_t v)
{
   vlog_check(vlog_value(v));
   vlog_check_stmts(v);
}

static void vlog_check_if(vlog_node_t v)
{
   const int nconds = vlog_conds(v);
   for (int i = 0; i < nconds; i++)
      vlog_check(vlog_cond(v, i));
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

      vlog_check_stmts(c);
   }
}

static void vlog_check_cond(vlog_node_t v)
{
   if (vlog_has_value(v))
      vlog_check_expr(vlog_value(v));

   vlog_check_stmts(v);
}

static void vlog_check_event_control(vlog_node_t v)
{
   const int nparams = vlog_params(v);
   for (int i = 0; i < nparams; i++)
      vlog_check_expr(vlog_param(v, i));
}

static void vlog_check_delay_control(vlog_node_t v)
{
   vlog_check_expr(vlog_value(v));
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

   vlog_check_decls(v);
   vlog_check_stmts(v);
}

static void vlog_check_gate_inst(vlog_node_t v)
{
   const int nparams = vlog_params(v);
   for (int i = 0; i < nparams; i++)
      vlog_check_expr(vlog_param(v, i));
}

static void vlog_check_inst_list(vlog_node_t v)
{
   const int nparams = vlog_params(v);
   for (int i = 0; i < nparams; i++)
      vlog_check(vlog_param(v, i));

   vlog_check_stmts(v);
}

static void vlog_check_mod_inst(vlog_node_t v)
{
   const int nparams = vlog_params(v);
   for (int i = 0; i < nparams; i++)
      vlog_check(vlog_param(v, i));
}

static void vlog_check_port_conn(vlog_node_t v)
{
   if (vlog_has_value(v))
      vlog_check_expr(vlog_value(v));
}

static void vlog_check_param_assign(vlog_node_t v)
{
   if (vlog_has_value(v))
      vlog_check_const_expr(vlog_value(v));
}

static void vlog_check_enum_name(vlog_node_t v)
{
   if (vlog_has_value(v))
      vlog_check_const_expr(vlog_value(v));
}

static void vlog_check_wait(vlog_node_t v)
{
   // TODO
}

static type_mask_t vlog_check_hier_ref(vlog_node_t v)
{
   vlog_node_t inst = vlog_ref(v);
   if (vlog_kind(inst) != V_MOD_INST)
      error_at(vlog_loc(v), "prefix of hierarchical identifier is not an "
               "instance");

   return TM_INTEGRAL;
}

static type_mask_t vlog_check_member_ref(vlog_node_t v)
{
   vlog_check_expr(vlog_value(v));

   return get_type_mask(vlog_ref(v));
}

static type_mask_t vlog_check_ref(vlog_node_t v)
{
   return get_type_mask(vlog_ref(v));
}

static type_mask_t vlog_check_bit_select(vlog_node_t v)
{
   vlog_check_params(v);

   return vlog_check_expr(vlog_value(v));
}

static type_mask_t vlog_check_part_select(vlog_node_t v)
{
   if (vlog_subkind(v) == V_RANGE_CONST) {
      vlog_node_t left = vlog_left(v);
      vlog_check_const_expr(left);
   }

   vlog_node_t right = vlog_right(v);
   vlog_check_const_expr(right);

   return vlog_check_expr(vlog_value(v));
}

static type_mask_t vlog_check_user_fcall(vlog_node_t v)
{
   vlog_node_t func = vlog_ref(v);
   if (vlog_kind(func) != V_FUNC_DECL) {
      diag_t *d = diag_new(DIAG_ERROR, vlog_loc(v));
      diag_printf(d, "'%s' is not a function", istr(vlog_ident(func)));
      diag_hint(d, vlog_loc(func), "'%s' declared here",
                istr(vlog_ident(func)));
      diag_emit(d);
      return TM_ERROR;
   }

   vlog_check_call_args(v, func);

   return get_type_mask(func);
}

static type_mask_t vlog_check_concat(vlog_node_t v)
{
   if (vlog_has_value(v)) {
      vlog_node_t repeat = vlog_value(v);
      vlog_check_const_expr(repeat);
   }

   const int nparams = vlog_params(v);
   for (int i = 0; i < nparams; i++)
      vlog_check_expr(vlog_param(v, i));

   return TM(DT_LOGIC);
}

static type_mask_t vlog_check_event(vlog_node_t v)
{
   vlog_check_expr(vlog_value(v));
   return TM(DT_LOGIC);
}

static type_mask_t vlog_check_binary(vlog_node_t v)
{
   type_mask_t lmask = vlog_check_expr(vlog_left(v));
   type_mask_t rmask = vlog_check_expr(vlog_right(v));

   if ((lmask & TM_REAL) && (rmask & TM_INTEGRAL))
      rmask |= TM_REAL;

   if ((rmask & TM_REAL) && (lmask & TM_INTEGRAL))
      lmask |= TM_REAL;

   if ((lmask & TM_INTEGRAL) && (rmask & TM_INTEGRAL)) {
      lmask |= TM_INTEGRAL;
      rmask |= TM_INTEGRAL;
   }

   // See table 11-1 in 1800-2023 section 11.3 for allowed operand types
   type_mask_t allow = TM_ANY, result = lmask & rmask;
   switch (vlog_subkind(v)) {
   case V_BINARY_PLUS:
   case V_BINARY_MINUS:
   case V_BINARY_TIMES:
   case V_BINARY_DIVIDE:
   case V_BINARY_EXP:
   case V_BINARY_LOG_OR:
   case V_BINARY_LOG_AND:
      allow = TM_INTEGRAL | TM_REAL | TM_CONST;
      break;
   case V_BINARY_MOD:
   case V_BINARY_OR:
   case V_BINARY_AND:
   case V_BINARY_XOR:
   case V_BINARY_XNOR:
      allow = TM_INTEGRAL | TM_CONST;
      break;
   case V_BINARY_CASE_EQ:
   case V_BINARY_CASE_NEQ:
      allow = TM_ANY & ~TM_REAL;
      result = TM(DT_BIT) | (lmask & rmask & TM_CONST);
      break;
   }

   if (lmask & rmask & allow)
      return result;

   diag_t *d = diag_new(DIAG_ERROR, vlog_loc(v));
   diag_printf(d, "invalid operands for binary expression");
   diag_hint(d, vlog_loc(v), "have '%s' and '%s'", type_mask_str(lmask),
             type_mask_str(rmask));
   diag_emit(d);

   return TM_ERROR;
}

static type_mask_t vlog_check_unary(vlog_node_t v)
{
   return vlog_check_expr(vlog_value(v));
}

static type_mask_t vlog_check_cond_expr(vlog_node_t v)
{
   type_mask_t vmask = vlog_check_expr(vlog_value(v));

   type_mask_t lmask = vlog_check_expr(vlog_left(v));
   type_mask_t rmask = vlog_check_expr(vlog_right(v));

   return TM_INTEGRAL | (vmask & lmask & rmask & TM_CONST);
}

static type_mask_t vlog_check_sys_fcall(vlog_node_t v)
{
   vlog_check_params(v);

   // See 1800-2023 section 11.2.1 for list of constant system functions
   switch (is_well_known(vlog_ident(v))) {
   case W_DLR_CLOG2:
      return TM_INTEGRAL | TM_CONST;
   default:
      return TM_INTEGRAL;
   }
}

static type_mask_t vlog_check_class_new(vlog_node_t v)
{
   const int nparams = vlog_params(v);
   for (int i = 0; i < nparams; i++)
      vlog_check_expr(vlog_param(v, i));

   return TM_CLASS;
}

static type_mask_t vlog_check_op_assign(vlog_node_t v)
{
   vlog_node_t target = vlog_target(v);
   type_mask_t tmask = vlog_check_expr(target);

   vlog_check_variable_lvalue(target, target);

   vlog_check_expr(vlog_value(v));

   return tmask;
}

static type_mask_t vlog_check_prefix_postfix(vlog_node_t v)
{
   vlog_node_t target = vlog_target(v);
   type_mask_t tmask = vlog_check_expr(target);

   vlog_check_variable_lvalue(target, target);

   return tmask;
}

static void vlog_non_const_diag_cb(vlog_node_t v, void *ctx)
{
   vlog_node_t *pdecl = ctx;

   vlog_node_t decl = vlog_ref(v);
   switch (vlog_kind(decl)) {
   case V_PARAM_DECL:
   case V_LOCALPARAM:
   case V_GENVAR_DECL:
   case V_ENUM_NAME:
      break;
   default:
      *pdecl = decl;
      break;
   }
}

static type_mask_t vlog_check_const_expr(vlog_node_t v)
{
   type_mask_t tmask = vlog_check_expr(v);

   if (!(tmask & TM_CONST)) {
      vlog_node_t decl = NULL;
      vlog_visit_only(v, vlog_non_const_diag_cb, &decl, V_REF);

      if (decl == NULL)
         error_at(vlog_loc(v), "expression is not a constant");
      else {
         diag_t *d = diag_new(DIAG_ERROR, vlog_loc(v));
         diag_printf(d, "cannot reference %s '%s' in constant expression",
                     vlog_is_net(decl) ? "net" : "variable",
                     istr(vlog_ident(decl)));
         diag_hint(d, vlog_loc(decl), "%s declared here",
                   istr(vlog_ident(decl)));
         diag_emit(d);
      }
   }

   return tmask;
}

static type_mask_t vlog_check_expr(vlog_node_t v)
{
   switch (vlog_kind(v)) {
   case V_BINARY:
      return vlog_check_binary(v);
   case V_UNARY:
      return vlog_check_unary(v);
   case V_COND_EXPR:
      return vlog_check_cond_expr(v);
   case V_SYS_FCALL:
      return vlog_check_sys_fcall(v);
   case V_USER_FCALL:
      return vlog_check_user_fcall(v);
   case V_REF:
      return vlog_check_ref(v);
   case V_HIER_REF:
      return vlog_check_hier_ref(v);
   case V_MEMBER_REF:
      return vlog_check_member_ref(v);
   case V_BIT_SELECT:
      return vlog_check_bit_select(v);
   case V_PART_SELECT:
      return vlog_check_part_select(v);
   case V_EVENT:
      return vlog_check_event(v);
   case V_CLASS_NEW:
      return vlog_check_class_new(v);
   case V_CONCAT:
      return vlog_check_concat(v);
   case V_PREFIX:
   case V_POSTFIX:
      return vlog_check_prefix_postfix(v);
   case V_NUMBER:
   case V_STRENGTH:
      return TM_INTEGRAL | TM_CONST;
   case V_REAL:
      return TM_REAL | TM_CONST;
   case V_STRING:
      return TM(DT_LOGIC) | TM_CONST;
   case V_EMPTY:
      return TM_EMPTY;
   case V_NULL:
      return TM_CLASS | TM_CONST;
   default:
      CANNOT_HANDLE(v);
   }
}

void vlog_check(vlog_node_t v)
{
   switch (vlog_kind(v)) {
   case V_MODULE:
      vlog_check_module(v);
      break;
   case V_PACKAGE:
   case V_CLASS_DECL:
      vlog_check_decls(v);
      break;
   case V_NET_DECL:
      vlog_check_net_decl(v);
      break;
   case V_VAR_DECL:
      vlog_check_var_decl(v);
      break;
   case V_PORT_DECL:
      vlog_check_port_decl(v);
      break;
   case V_PARAM_DECL:
      vlog_check_param_decl(v);
      break;
   case V_LOCALPARAM:
      vlog_check_localparam(v);
      break;
   case V_GENVAR_DECL:
      vlog_check_genvar_decl(v);
      break;
   case V_TYPE_DECL:
      vlog_check_type_decl(v);
      break;
   case V_ENUM_DECL:
      vlog_check_enum_decl(v);
      break;
   case V_STRUCT_DECL:
      vlog_check_struct_decl(v);
      break;
   case V_UNION_DECL:
      vlog_check_union_decl(v);
      break;
   case V_ENUM_NAME:
      vlog_check_enum_name(v);
      break;
   case V_INITIAL:
   case V_ALWAYS:
   case V_FOREVER:
      vlog_check_stmts(v);
      break;
   case V_BLOCK:
   case V_PROGRAM:
   case V_CONSTRUCTOR:
      vlog_check_decls(v);
      vlog_check_stmts(v);
      break;
   case V_INST_LIST:
      vlog_check_inst_list(v);
      break;
   case V_MOD_INST:
      vlog_check_mod_inst(v);
      break;
   case V_PORT_CONN:
      vlog_check_port_conn(v);
      break;
   case V_PARAM_ASSIGN:
      vlog_check_param_assign(v);
      break;
   case V_FUNC_DECL:
   case V_TASK_DECL:
      vlog_check_tf_decl(v);
      break;
   case V_TIMING:
      vlog_check_timing(v);
      break;
   case V_IF:
      vlog_check_if(v);
      break;
   case V_IF_GENERATE:
      vlog_check_if_generate(v);
      break;
   case V_COND:
      vlog_check_cond(v);
      break;
   case V_CASE:
      vlog_check_case(v);
      break;
   case V_FOR_LOOP:
   case V_FOR_GENERATE:
      vlog_check_for_loop(v);
      break;
   case V_REPEAT:
      vlog_check_repeat(v);
      break;
   case V_WHILE:
      vlog_check_while(v);
      break;
   case V_BASSIGN:
      vlog_check_bassign(v);
      break;
   case V_NBASSIGN:
      vlog_check_nbassign(v);
      break;
   case V_RETURN:
      vlog_check_return(v);
      break;
   case V_SYS_TCALL:
      vlog_check_sys_tcall(v);
      break;
   case V_USER_TCALL:
      vlog_check_user_tcall(v);
      break;
   case V_SUPER_CALL:
      vlog_check_super_call(v);
      break;
   case V_VOID_CALL:
      vlog_check_expr(vlog_value(v));
      break;
   case V_EVENT_CONTROL:
      vlog_check_event_control(v);
      break;
   case V_DELAY_CONTROL:
      vlog_check_delay_control(v);
      break;
   case V_GATE_INST:
      vlog_check_gate_inst(v);
      break;
   case V_ASSIGN:
      vlog_check_assign(v);
      break;
   case V_OP_ASSIGN:
      vlog_check_op_assign(v);
      break;
   case V_PREFIX:
   case V_POSTFIX:
      vlog_check_prefix_postfix(v);
      break;
   case V_DEASSIGN:
      vlog_check_deassign(v);
      break;
   case V_PRIMITIVE:
      vlog_check_primitive(v);
      break;
   case V_DATA_TYPE:
      vlog_check_data_type(v);
      break;
   case V_DIMENSION:
      vlog_check_dimension(v);
      break;
   case V_WAIT:
      vlog_check_wait(v);
      break;
   case V_SPECIFY:
   case V_IMPORT_DECL:
      break;
   default:
      CANNOT_HANDLE(v);
   }
}
