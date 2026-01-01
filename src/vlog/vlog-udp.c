//
//  Copyright (C) 2023-2025  Nick Gasson
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
#include "ident.h"
#include "mir/mir-node.h"
#include "mir/mir-unit.h"
#include "vlog/vlog-node.h"
#include "vlog/vlog-number.h"

#include <assert.h>
#include <stdlib.h>

#define CANNOT_HANDLE(v) do {                                           \
      fatal_at(vlog_loc(v), "cannot handle %s in %s" ,                  \
               vlog_kind_str(vlog_kind(v)), __FUNCTION__);              \
   } while (0)

static mir_value_t vlog_lower_rvalue(mir_unit_t *mu, vlog_node_t v)
{
   switch (vlog_kind(v)) {
   case V_NUMBER:
      {
         number_t num = vlog_number(v);
         assert(number_width(num) == 1);

         mir_type_t t_logic = mir_vec4_type(mu, 1, false);
         return mir_const_vec(mu, t_logic, number_bit(num, 0), 1);
      }
   default:
      CANNOT_HANDLE(v);
   }
}

static mir_value_t vlog_udp_cmp(mir_unit_t *mu, mir_value_t left,
                                mir_value_t right)
{
   mir_type_t t_logic = mir_vec4_type(mu, 1, false);
   mir_value_t eq = mir_build_binary(mu, MIR_VEC_CASE_EQ, t_logic, left, right);
   return mir_build_test(mu, eq);
}

void vlog_lower_udp(mir_unit_t *mu, object_t *obj)
{
   vlog_node_t udp = vlog_from_object(obj);
   assert(vlog_kind(udp) == V_INST_BODY);

   vlog_node_t table = vlog_stmt(udp, 0);
   assert(vlog_kind(table) == V_UDP_TABLE);

   const vlog_udp_kind_t kind = vlog_subkind(table);

   mir_block_t start_bb = mir_add_block(mu);
   assert(start_bb.id == 1);

   vlog_node_t out_decl = vlog_ref(vlog_port(udp, 0));
   assert(vlog_kind(out_decl) == V_PORT_DECL);
   assert(vlog_subkind(out_decl) == V_PORT_OUTPUT);

   int hops;
   mir_value_t out_var = mir_search_object(mu, out_decl, &hops);
   assert(!mir_is_null(out_var));

   const int nports = vlog_ports(udp);
   mir_value_t *in_vars LOCAL = xmalloc_array(nports - 1, sizeof(mir_value_t));
   for (int i = 1; i < nports; i++) {
      vlog_node_t decl = vlog_ref(vlog_port(udp, i));
      assert(vlog_kind(decl) == V_PORT_DECL);
      assert(vlog_subkind(decl) == V_PORT_INPUT);

      int hops;
      in_vars[i - 1] = mir_search_object(mu, decl, &hops);
      assert(!mir_is_null(in_vars[i - 1]));
   }

   mir_type_t t_offset = mir_offset_type(mu);
   mir_type_t t_logic = mir_vec4_type(mu, 1, false);

   mir_value_t result_var = mir_add_var(mu, t_logic, MIR_NULL_STAMP,
                                        ident_new("result"), MIR_VAR_TEMP);

   {
      mir_value_t upref = mir_build_var_upref(mu, hops, out_var.id);
      mir_value_t out = mir_build_load(mu, upref);
      mir_value_t one = mir_const(mu, t_offset, 1);
      mir_build_drive_signal(mu, out, one);

      for (int i = 1; i < nports; i++) {
         mir_value_t upref = mir_build_var_upref(mu, hops, in_vars[i - 1].id);
         mir_value_t nets = mir_build_load(mu, upref);
         mir_build_sched_event(mu, nets, one);
      }

      if (vlog_stmts(table) > 0) {
         vlog_node_t init = vlog_value(vlog_stmt(table, 0));
         mir_build_map_const(mu, vlog_lower_rvalue(mu, init), out, one);
      }

      mir_build_return(mu, MIR_NULL_VALUE);
   }

   mir_block_t wait_bb = mir_add_block(mu);

   mir_set_cursor(mu, start_bb, MIR_APPEND);

   mir_build_store(mu, result_var, mir_const_vec(mu, t_logic, 1, 1));

   {
      mir_value_t one = mir_const(mu, t_offset, 1);
      mir_value_t logic0 = mir_const_vec(mu, t_logic, 0, 0);
      mir_value_t logic1 = mir_const_vec(mu, t_logic, 1, 0);
      mir_value_t logicX = mir_const_vec(mu, t_logic, 1, 1);

      mir_value_t level_map[127];
      level_map['0'] = logic0;
      level_map['1'] = logic1;
      level_map['x'] = level_map['X'] = logicX;

      mir_value_t *mem LOCAL = xmalloc_array(nports * 2, sizeof(mir_value_t));
      mir_value_t *in_regs = mem, *in_nets = mem + nports;

      for (int i = 1; i < nports; i++) {
         mir_value_t upref = mir_build_var_upref(mu, hops, in_vars[i - 1].id);
         mir_value_t nets = mir_build_load(mu, upref);
         mir_value_t value = mir_build_load(mu, mir_build_resolved(mu, nets));
         in_regs[i - 1] = mir_build_pack(mu, t_logic, value);
         in_nets[i - 1] = nets;
      }

      mir_block_t test_bb = start_bb;

      const int nentries = vlog_params(table);
      for (int i = 0; i < nentries; i++) {
         vlog_node_t entry = vlog_param(table, i);
         assert(vlog_kind(entry) == V_UDP_ENTRY);

         mir_block_t hit_bb = mir_add_block(mu);
         mir_value_t and = MIR_NULL_VALUE;

         int pos = 0;
         for (int j = 0; j < nports - 1; j++) {
            mir_value_t cmp = MIR_NULL_VALUE;
            vlog_node_t sym = vlog_param(entry, pos++);
            switch (vlog_kind(sym)) {
            case V_UDP_LEVEL:
               switch (vlog_ival(sym)) {
               case '0':
               case '1':
               case 'x':
               case 'X':
                  cmp = vlog_udp_cmp(mu, in_regs[j], level_map[vlog_ival(sym)]);
                  break;
               case '*':
                  cmp = mir_build_event_flag(mu, in_nets[j], one);
                  break;
               case 'b':
                  {
                     mir_value_t is0 = vlog_udp_cmp(mu, in_regs[j], logic0);
                     mir_value_t is1 = vlog_udp_cmp(mu, in_regs[j], logic1);
                     cmp = mir_build_and(mu, is0, is1);
                  }
                  break;
               case '?':
                  break;
               default:
                  CANNOT_HANDLE(sym);
               }
               break;

            case V_UDP_EDGE:
               {
                  const char left = vlog_ival(vlog_left(sym));
                  const char right = vlog_ival(vlog_right(sym));

                  cmp = mir_build_event_flag(mu, in_nets[j], one);

                  if (left != '?') {
                     mir_value_t last_ptr =
                        mir_build_last_value(mu, in_nets[j]);
                     mir_value_t last = mir_build_load(mu, last_ptr);
                     mir_value_t packed = mir_build_pack(mu, t_logic, last);
                     mir_value_t eq = vlog_udp_cmp(mu, packed,
                                                   level_map[(unsigned)left]);
                     cmp = mir_build_and(mu, cmp, eq);
                  }

                  if (right != '?') {
                     mir_value_t eq = vlog_udp_cmp(mu, in_regs[j],
                                                   level_map[(unsigned)right]);
                     cmp = mir_build_and(mu, cmp, eq);
                  }
               }
               break;

            default:
               CANNOT_HANDLE(entry);
            }

            if (mir_is_null(and))
               and = cmp;
            else if (!mir_is_null(cmp))
               and = mir_build_and(mu, and, cmp);
         }

         if (kind == V_UDP_SEQ) {
            mir_value_t upref = mir_build_var_upref(mu, hops, out_var.id);
            mir_value_t out = mir_build_load(mu, upref);
            mir_value_t resolved = mir_build_resolved(mu, out);
            mir_value_t cur = mir_build_load(mu, resolved);
            mir_value_t packed = mir_build_pack(mu, t_logic, cur);

            vlog_node_t sym = vlog_param(entry, pos++);
            assert(vlog_kind(sym) == V_UDP_LEVEL);

            mir_value_t cmp = MIR_NULL_VALUE;
            switch (vlog_ival(sym)) {
            case '0':
            case '1':
            case 'x':
            case 'X':
               cmp = vlog_udp_cmp(mu, packed, level_map[vlog_ival(sym)]);
               break;
            case '?':
               break;
            default:
               CANNOT_HANDLE(sym);
            }

            if (mir_is_null(and))
               and = cmp;
            else if (!mir_is_null(cmp))
               and = mir_build_and(mu, and, cmp);
         }

         if (mir_is_null(and)) {
            mir_build_jump(mu, hit_bb);
            break;
         }
         else {
            test_bb = mir_add_block(mu);
            mir_build_cond(mu, and, hit_bb, test_bb);
         }

         mir_set_cursor(mu, hit_bb, MIR_APPEND);

         vlog_node_t sym = vlog_param(entry, pos++);
         assert(vlog_kind(sym) == V_UDP_LEVEL);
         assert(pos == vlog_params(entry));

         mir_value_t drive;
         switch (vlog_ival(sym)) {
         case '0':
         case '1':
         case 'x':
         case 'X':
            drive = level_map[vlog_ival(sym)];
            break;
         case '-':
            // No change, skip assignment to output
            drive = MIR_NULL_VALUE;
            mir_build_wait(mu, start_bb);
            mir_set_cursor(mu, test_bb, MIR_APPEND);
            continue;
         default:
            CANNOT_HANDLE(entry);
         }

         mir_build_store(mu, result_var, drive);
         mir_build_jump(mu, wait_bb);

         mir_set_cursor(mu, test_bb, MIR_APPEND);
      }

      mir_set_cursor(mu, test_bb, MIR_APPEND);

      if (!mir_block_finished(mu, test_bb)) {
         mir_build_store(mu, result_var, logicX);
         mir_build_jump(mu, wait_bb);
      }

      mir_set_cursor(mu, wait_bb, MIR_APPEND);

      mir_value_t result = mir_build_load(mu, result_var);
      const uint8_t strength = kind == V_UDP_COMB ? ST_STRONG : 0;
      mir_value_t drive =
         mir_build_unpack(mu, result, strength, MIR_NULL_VALUE);

      mir_value_t upref = mir_build_var_upref(mu, hops, out_var.id);
      mir_value_t out = mir_build_load(mu, upref);
      mir_build_put_driver(mu, out, one, drive);

      mir_build_wait(mu, start_bb);
   }

   mir_optimise(mu, MIR_PASS_O0);
}
