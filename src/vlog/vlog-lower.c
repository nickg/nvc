//
//  Copyright (C) 2023 Nick Gasson
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
#include "lib.h"
#include "lower.h"
#include "tree.h"
#include "vcode.h"
#include "vlog/vlog-node.h"
#include "vlog/vlog-phase.h"

#include <assert.h>

static inline vcode_type_t vlog_logic_type(void)
{
   return vtype_int(0, 3);
}

static vcode_reg_t vlog_debug_locus(vlog_node_t v)
{
   ident_t unit;
   ptrdiff_t offset;
   vlog_locus(v, &unit, &offset);

   return emit_debug_locus(unit, offset);
}

static void vlog_lower_port_decl(lower_unit_t *lu, vlog_node_t port)
{
   vcode_type_t vlogic = vlog_logic_type();
   vcode_type_t vsignal = vtype_signal(vlogic);
   vcode_type_t voffset = vtype_offset();

   vcode_var_t var = emit_var(vsignal, vlogic, vlog_ident(port), VAR_SIGNAL);
   lower_put_vcode_obj(port, var, lu);

   vcode_reg_t size = emit_const(voffset, 1);
   vcode_reg_t count = emit_const(voffset, 1);
   vcode_reg_t init = emit_const(vlogic, 3);
   vcode_reg_t flags = emit_const(voffset, 0);
   vcode_reg_t locus = vlog_debug_locus(port);

   emit_init_signal(vlogic, size, count, init, flags, locus, VCODE_INVALID_REG);
}

static void vlog_lower_decls(lower_unit_t *lu, vlog_node_t scope)
{
   const int ndecls = vlog_decls(scope);
   for (int i = 0; i < ndecls; i++) {
      vlog_node_t d = vlog_decl(scope, i);
      switch (vlog_kind(d)) {
      case V_PORT_DECL:
         vlog_lower_port_decl(lu, d);
         break;
      default:
         fatal_trace("cannot lower Verilog decl kind %s",
                     vlog_kind_str(vlog_kind(d)));
      }
   }
}

static void vlog_lower_port_map(lower_unit_t *lu, vlog_node_t root, tree_t wrap)
{
   const int nparams = tree_params(wrap);

   for (int i = 0, pos = 0; i < nparams; i++) {
      tree_t map = tree_param(wrap, i);
      assert(tree_subkind(map) == P_POS);

      vlog_node_t port = NULL;
      while (vlog_kind((port = vlog_decl(root, pos++))) != V_PORT_DECL)
         ;

      int hops;
      vcode_var_t var = lower_search_vcode_obj(port, lu, &hops);
      assert(var != VCODE_INVALID_VAR);
      assert(hops == 0);

      vcode_reg_t vhdl_reg = lower_lvalue(lu, tree_value(map));
      vcode_reg_t vlog_reg = emit_load(var);

      vcode_reg_t count_reg = emit_const(vtype_offset(), 1);

      if (vlog_subkind(port) == V_PORT_INPUT) {
         vcode_reg_t conv_reg = VCODE_INVALID_REG;   // TODO
         emit_map_signal(vhdl_reg, vlog_reg, count_reg, count_reg, conv_reg);
      }
      else {
         vcode_reg_t conv_reg = VCODE_INVALID_REG;   // TODO
         emit_map_signal(vlog_reg, vhdl_reg, count_reg, count_reg, conv_reg);
      }
   }
}

static void vlog_lower_always(lower_unit_t *parent, vlog_node_t stmt)
{
   vcode_unit_t context = get_vcode(parent);
   vcode_select_unit(context);

   ident_t label = ident_uniq("always");
   ident_t name = ident_prefix(vcode_unit_name(), label, '.');
   vcode_unit_t vu = emit_process(name, vlog_to_object(stmt), context);

   vcode_block_t start_bb = emit_block();
   assert(start_bb == 1);

   lower_unit_t *lu = lower_unit_new(parent, vu, NULL, NULL);

   emit_return(VCODE_INVALID_REG);

   vcode_select_block(start_bb);

   emit_wait(start_bb, VCODE_INVALID_REG);

   lower_unit_free(lu);
}

static void vlog_lower_concurrent(lower_unit_t *parent, vlog_node_t scope)
{
   const int nstmts = vlog_stmts(scope);
   for (int i = 0; i < nstmts; i++) {
      vlog_node_t s = vlog_stmt(scope, i);
      switch (vlog_kind(s)) {
      case V_ALWAYS:
         vlog_lower_always(parent, s);
         break;
      default:
         fatal_trace("cannot handle concurrent statement kind %s",
                     vlog_kind_str(vlog_kind(s)));
      }
   }
}

void vlog_lower(tree_t wrap, lower_unit_t *parent)
{
   assert(tree_kind(wrap) == T_VERILOG);

   vlog_node_t root = tree_vlog(wrap);
   assert(vlog_kind(root) == V_ROOT);

   vcode_unit_t context = parent ? get_vcode(parent) : NULL;
   vcode_select_unit(context);

   ident_t prefix = parent ? vcode_unit_name() : lib_name(lib_work());
   ident_t label = tree_ident(wrap);
   ident_t name = ident_prefix(prefix, label, '.');

   vcode_unit_t vu = emit_instance(name, tree_to_object(wrap), context);

   lower_unit_t *lu = lower_unit_new(parent, vu, NULL, NULL);

   vlog_lower_decls(lu, root);
   vlog_lower_port_map(lu, root, wrap);

   emit_return(VCODE_INVALID_REG);

   lower_finished(lu);

   vlog_lower_concurrent(lu, root);

   lower_unit_free(lu);
}
