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
#include "diag.h"
#include "lib.h"
#include "lower.h"
#include "tree.h"
#include "vcode.h"
#include "vlog/vlog-node.h"
#include "vlog/vlog-number.h"
#include "vlog/vlog-phase.h"

#include <assert.h>
#include <string.h>
#include <stdlib.h>

#define CANNOT_HANDLE(v) do {                                           \
      fatal_at(vlog_loc(v), "cannot handle %s in %s" ,                  \
               vlog_kind_str(vlog_kind(v)), __FUNCTION__);              \
   } while (0)

#define PUSH_DEBUG_INFO(v)                               \
   __attribute__((cleanup(emit_debug_info), unused))     \
   const loc_t _old_loc = *vcode_last_loc();             \
   emit_debug_info(vlog_loc((v)));                       \

static void vlog_lower_stmts(lower_unit_t *lu, vlog_node_t v);

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

static vcode_reg_t vlog_lower_width(vlog_node_t v)
{
   vcode_type_t voffset = vtype_offset();

   switch (vlog_kind(v)) {
   case V_STRING:
      return emit_const(voffset, strlen(vlog_text(v)));
   case V_NUMBER:
      return emit_const(voffset, number_width(vlog_number(v)));
   default:
      CANNOT_HANDLE(v);
   }
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

   vcode_reg_t nets_reg = emit_init_signal(vlogic, size, count, init, flags,
                                           locus, VCODE_INVALID_REG);
   emit_store(nets_reg, var);
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
         CANNOT_HANDLE(d);
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

static vcode_reg_t vlog_lower_lvalue(lower_unit_t *lu, vlog_node_t v)
{
   PUSH_DEBUG_INFO(v);

   switch (vlog_kind(v)) {
   case V_REF:
      {
         vlog_node_t decl = vlog_ref(v);

         int hops;
         vcode_var_t var = lower_search_vcode_obj(decl, lu, &hops);
         assert(var != VCODE_INVALID_VAR);

         if (hops == 0)
            return emit_load(var);
         else
            return emit_load_indirect(emit_var_upref(hops, var));
      }
   default:
      CANNOT_HANDLE(v);
   }
}

static vcode_reg_t vlog_lower_rvalue(lower_unit_t *lu, vlog_node_t v)
{
   switch (vlog_kind(v)) {
   case V_REF:
      {
         vlog_node_t decl = vlog_ref(v);

         int hops;
         vcode_var_t var = lower_search_vcode_obj(decl, lu, &hops);
         assert(var != VCODE_INVALID_VAR);

         vcode_reg_t nets_reg;
         if (hops == 0)
            nets_reg = emit_load(var);
         else
            nets_reg = emit_load_indirect(emit_var_upref(hops, var));

         return emit_load_indirect(emit_resolved(nets_reg));
      }
   case V_EVENT:
      {
         vlog_node_t value = vlog_value(v);
         vcode_reg_t nets_reg = vlog_lower_lvalue(lu, value);

         vcode_reg_t count_reg = emit_const(vtype_offset(), 1);
         vcode_reg_t event_reg = emit_event_flag(nets_reg, count_reg);

         const v_event_kind_t ekind = vlog_subkind(v);
         if (ekind == V_EVENT_LEVEL)
            return event_reg;

         vcode_type_t vlogic = vlog_logic_type();

         vcode_reg_t const_reg = emit_const(vlogic, ekind == V_EVENT_POSEDGE);
         vcode_reg_t value_reg = vlog_lower_rvalue(lu, value);
         vcode_reg_t cmp_reg = emit_cmp(VCODE_CMP_EQ, value_reg, const_reg);

         return emit_and(event_reg, cmp_reg);
      }
   case V_STRING:
      {
         const char *text = vlog_text(v);
         size_t len = strlen(text) + 1;
         vcode_reg_t *chars LOCAL = xmalloc_array(len, sizeof(vcode_reg_t));
         vcode_type_t vchar = vtype_char();

         for (int i = 0; i < len; i++)
            chars[i] = emit_const(vchar, text[i]);

         vcode_type_t vtype = vtype_carray(len, vchar, vchar);
         vcode_reg_t array = emit_const_array(vtype, chars, len);
         return emit_address_of(array);
      }
   case V_NUMBER:
      {
         vcode_type_t vlogic = vlog_logic_type();

         number_t num = vlog_number(v);
         const int width = number_width(num);

         if (width == 1) {
            abort();
         }
         else {
            vcode_reg_t *bits LOCAL = xmalloc_array(width, sizeof(vcode_reg_t));
            for (int i = 0; i < width; i++)
               bits[i] = emit_const(vlogic, number_bit(num, width - i - 1 ));

            vcode_type_t varray = vtype_carray(width, vlogic, vlogic);
            return emit_const_array(varray, bits, width);
         }
      }
   default:
      CANNOT_HANDLE(v);
   }
}

static void vlog_lower_sensitivity(lower_unit_t *lu, vlog_node_t v)
{
   switch (vlog_kind(v)) {
   case V_REF:
      {
         vcode_reg_t nets_reg = vlog_lower_lvalue(lu, v);
         vcode_reg_t count_reg = emit_const(vtype_offset(), 1);
         emit_sched_event(nets_reg, count_reg);
      }
      break;
   case V_EVENT:
      vlog_lower_sensitivity(lu, vlog_value(v));
      break;
   default:
      CANNOT_HANDLE(v);
   }
}

static void vlog_lower_timing(lower_unit_t *lu, vlog_node_t v, bool is_static)
{
   vcode_reg_t test_reg = vlog_lower_rvalue(lu, vlog_value(v));

   vcode_block_t true_bb = emit_block();
   vcode_block_t false_bb = emit_block();

   emit_cond(test_reg, true_bb, false_bb);

   vcode_select_block(true_bb);

   vlog_lower_stmts(lu, v);

   if (!vcode_block_finished())
      emit_jump(false_bb);

   vcode_select_block(false_bb);
}

static void vlog_lower_nbassign(lower_unit_t *lu, vlog_node_t v)
{
   vcode_reg_t nets_reg = vlog_lower_lvalue(lu, vlog_target(v));
   vcode_reg_t count_reg = emit_const(vtype_offset(), 1);
   vcode_reg_t value_reg = vlog_lower_rvalue(lu, vlog_value(v));

   vcode_type_t vtime = vtype_time();
   vcode_reg_t reject_reg = emit_const(vtime, 0);
   vcode_reg_t after_reg = emit_const(vtime, 0);

   emit_sched_waveform(nets_reg, count_reg, value_reg, reject_reg, after_reg);
}

static void vlog_lower_systask(lower_unit_t *lu, vlog_node_t v)
{
   const v_systask_kind_t kind = vlog_subkind(v);
   const char *fns[] = {
      "__nvc_sys_display",
      "__nvc_sys_write",
      "__nvc_sys_finish"
   };
   assert(kind < ARRAY_LEN(fns));

   switch (kind) {
   case V_SYS_DISPLAY:
   case V_SYS_WRITE:
      {
         const int nparams = vlog_params(v);
         vcode_reg_t *args LOCAL =
            xmalloc_array(nparams * 2, sizeof(vcode_reg_t));
         for (int i = 0; i < nparams; i++) {
            vlog_node_t p = vlog_param(v, i);
            args[i*2] = vlog_lower_width(p);
            args[i*2 + 1] = vlog_lower_rvalue(lu, p);
         }

         emit_fcall(ident_new(fns[kind]), VCODE_INVALID_TYPE,
                    VCODE_INVALID_TYPE, VCODE_CC_VARIADIC, args, nparams * 2);
      }
      break;

   case V_SYS_FINISH:
      emit_fcall(ident_new(fns[kind]), VCODE_INVALID_TYPE,
                 VCODE_INVALID_TYPE, VCODE_CC_FOREIGN, NULL, 0);
      break;

   default:
      CANNOT_HANDLE(v);
   }
}

static void vlog_lower_stmts(lower_unit_t *lu, vlog_node_t v)
{
   const int nstmts = vlog_stmts(v);
   for (int i = 0; i < nstmts; i++) {
      vlog_node_t s = vlog_stmt(v, i);
      emit_debug_info(vlog_loc(s));

      switch (vlog_kind(s)) {
      case V_TIMING:
         vlog_lower_timing(lu, s, false);
         break;
      case V_NBASSIGN:
         vlog_lower_nbassign(lu, s);
         break;
      case V_SEQ_BLOCK:
         vlog_lower_stmts(lu, s);
         break;
      case V_SYSTASK:
         vlog_lower_systask(lu, s);
         break;
      default:
         CANNOT_HANDLE(s);
      }
   }
}

static void vlog_lower_driver(lower_unit_t *lu, vlog_node_t v)
{
   vcode_reg_t nets_reg = vlog_lower_lvalue(lu, v);
   emit_drive_signal(nets_reg, emit_const(vtype_offset(), 1));
}

static void vlog_driver_cb(vlog_node_t v, void *context)
{
   lower_unit_t *lu = context;

   switch (vlog_kind(v)) {
   case V_NBASSIGN:
   case V_ASSIGN:
      vlog_lower_driver(lu, vlog_target(v));
      break;
   default:
      break;
   }
}

static void vlog_lower_always(lower_unit_t *parent, vlog_node_t stmt)
{
   vcode_unit_t context = get_vcode(parent);
   vcode_select_unit(context);

   ident_t name = ident_prefix(vcode_unit_name(), vlog_ident(stmt), '.');
   vcode_unit_t vu = emit_process(name, vlog_to_object(stmt), context);

   vcode_block_t start_bb = emit_block();
   assert(start_bb == 1);

   lower_unit_t *lu = lower_unit_new(NULL, parent, vu, NULL, NULL);

   vlog_visit(stmt, vlog_driver_cb, lu);

   vlog_node_t timing = NULL, s0 = vlog_stmt(stmt, 0);
   if (vlog_kind(s0) == V_TIMING) {
      timing = s0;
      vlog_lower_sensitivity(lu, vlog_value(timing));
   }

   emit_return(VCODE_INVALID_REG);

   vcode_select_block(start_bb);

   if (timing != NULL)
      vlog_lower_timing(lu, timing, true);
   else
      vlog_lower_stmts(lu, stmt);

   emit_wait(start_bb, VCODE_INVALID_REG);

   lower_unit_free(lu);
}

static void vlog_lower_initial(lower_unit_t *parent, vlog_node_t stmt)
{
   vcode_unit_t context = get_vcode(parent);
   vcode_select_unit(context);

   ident_t name = ident_prefix(vcode_unit_name(), vlog_ident(stmt), '.');
   vcode_unit_t vu = emit_process(name, vlog_to_object(stmt), context);

   vcode_block_t start_bb = emit_block();
   assert(start_bb == 1);

   lower_unit_t *lu = lower_unit_new(NULL, parent, vu, NULL, NULL);

   emit_return(VCODE_INVALID_REG);

   vcode_select_block(start_bb);

   vlog_lower_stmts(lu, stmt);

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
      case V_INITIAL:
         vlog_lower_initial(parent, s);
         break;
      default:
         CANNOT_HANDLE(s);
      }
   }
}

void vlog_lower(unit_registry_t *ur, tree_t wrap, lower_unit_t *parent)
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

   lower_unit_t *lu = lower_unit_new(ur, parent, vu, NULL, NULL);

   vlog_lower_decls(lu, root);
   vlog_lower_port_map(lu, root, wrap);

   emit_return(VCODE_INVALID_REG);

   lower_finished(lu);

   vlog_lower_concurrent(lu, root);

   lower_unit_free(lu);
}
