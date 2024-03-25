//
//  Copyright (C) 2024  Nick Gasson
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

#include "vhpi_test.h"

static vhpiHandleT h_sos;
static vhpiHandleT h_clk;
static vhpiHandleT h_d;
static vhpiHandleT h_q;
static vhpiHandleT h_timeout;

static void register_timeout(void);

static void set_clk(vhpiSmallEnumT bit)
{
   vhpiValueT val = {
      .format = vhpiLogicVal,
   };

   val.value.enumv = bit;
   vhpi_put_value(h_clk, &val, vhpiDepositPropagate);
   check_error();
}

static vhpiSmallEnumT get_clk(void)
{
   vhpiValueT val = {
      .format = vhpiLogicVal,
   };

   vhpi_get_value(h_clk, &val);
   check_error();

   return val.value.enumv;
}

static void set_d(vhpiSmallEnumT bit)
{
   vhpiValueT val = {
      .format = vhpiLogicVal,
   };

   val.value.enumv = bit;
   vhpi_put_value(h_d, &val, vhpiDepositPropagate);
   check_error();
}

static vhpiSmallEnumT get_q(void)
{
   vhpiValueT val = {
      .format = vhpiLogicVal,
   };

   vhpi_get_value(h_q, &val);
   check_error();

   return val.value.enumv;
}

static void check_q_2(const vhpiCbDataT *cb_data)
{
   vhpi_printf("check q 2");

   vhpiSmallEnumT q = get_q();
   vhpi_printf("q value %x", q);
   fail_unless(q == vhpiH);  // XXX: vhpi1
}

static void check_q_1(const vhpiCbDataT *cb_data)
{
   vhpi_printf("check q 1");

   vhpiSmallEnumT q = get_q();
   vhpi_printf("q value %x", q);
   fail_unless(q == vhpi1);  // XXX: vhpiH

   set_clk(vhpi1);

   vhpiCbDataT next_cb = {
      .reason = vhpiCbStartOfNextCycle,
      .cb_rtn = check_q_2,
   };
   vhpi_register_cb(&next_cb, 0);
   check_error();
}

static void toggle_clock(const vhpiCbDataT *cb_data)
{
   vhpi_printf("toggle clock");

   if (get_clk() == vhpi1) {
      set_clk(vhpi0);

      vhpiSmallEnumT q = get_q();
      vhpi_printf("q value %x", q);
      fail_unless(q == vhpi1);
   }
   else
      set_clk(vhpi1);

   vhpi_release_handle(h_timeout);

   static int rep = 11;
   if (rep-- > 0)
      register_timeout();
   else {
      vhpiValueT val = {
         .format = vhpiLogicVal,
         .value = { .enumv = vhpiH },
      };

      vhpi_put_value(h_q, &val, vhpiDepositPropagate);

      vhpiCbDataT next_cb = {
         .reason = vhpiCbStartOfNextCycle,
         .cb_rtn = check_q_1,
      };
      vhpi_register_cb(&next_cb, 0);
      check_error();
   }
}

static void register_timeout(void)
{
   vhpiTimeT time = {
      .low = 1000000
   };

   vhpiCbDataT delay_cb = {
      .reason = vhpiCbAfterDelay,
      .cb_rtn = toggle_clock,
      .time   = &time
   };
   h_timeout = vhpi_register_cb(&delay_cb, vhpiReturnCb);
   check_error();
}

static void start_of_sim(const vhpiCbDataT *cb_data)
{
   vhpiHandleT root = vhpi_handle(vhpiRootInst, NULL);
   check_error();
   fail_if(root == NULL);
   vhpi_printf("root handle %p", root);

   h_clk = vhpi_handle_by_name("clk", root);
   check_error();
   fail_if(h_clk == NULL);
   vhpi_printf("clk handle %p", h_clk);

   h_d = vhpi_handle_by_name("d", root);
   check_error();
   fail_if(h_d == NULL);
   vhpi_printf("d handle %p", h_d);

   h_q = vhpi_handle_by_name("q", root);
   check_error();
   fail_if(h_q == NULL);
   vhpi_printf("q handle %p", h_q);

   set_clk(vhpi0);
   set_d(vhpi1);

   set_d(vhpi1);   // Should not crash if called twice

   register_timeout();

   vhpi_release_handle(h_sos);
}

void vhpi12_startup(void)
{
   vhpiCbDataT cb_data1 = {
      .reason    = vhpiCbStartOfSimulation,
      .cb_rtn    = start_of_sim,
   };
   h_sos = vhpi_register_cb(&cb_data1, vhpiReturnCb);
   check_error();
}
