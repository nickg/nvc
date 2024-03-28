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

#include <stdbool.h>
#include <stdlib.h>

static bool end_of_sim_called = false;
static int num_next_time_step = 0;
static vhpiHandleT never_call_handle_1 = NULL;
static vhpiHandleT never_call_handle_2 = NULL;

static void rep_next_time_step(const vhpiCbDataT *cb_data)
{
   vhpi_printf("rep_next_time_step");
   num_next_time_step++;

   if (num_next_time_step == 2) {
      vhpi_enable_cb(never_call_handle_1);
      vhpi_release_handle(never_call_handle_1);
   }
}

static void start_of_sim(const vhpiCbDataT *cb_data)
{
   vhpi_printf("start of sim");

   vhpi_disable_cb(never_call_handle_1);
   vhpi_disable_cb(never_call_handle_2);
}

static void end_of_sim(const vhpiCbDataT *cb_data)
{
   vhpi_printf("end of sim");
   end_of_sim_called = true;

   vhpi_printf("num_next_time_step = %d", num_next_time_step);

   fail_unless(num_next_time_step == 4);

   vhpi_remove_cb(never_call_handle_2);
}

static void never_call(const vhpiCbDataT *cb_data)
{
   vhpi_printf("callback should never be called!");
   fail_if(1);
}

static void check_end_of_sim_called(void)
{
   fail_unless(end_of_sim_called);
}

void vhpi13_startup(void)
{
   vhpiCbDataT cb_data1 = {
      .reason    = vhpiCbStartOfSimulation,
      .cb_rtn    = start_of_sim,
   };
   vhpi_register_cb(&cb_data1, 0);
   check_error();

   vhpiCbDataT cb_data2 = {
      .reason    = vhpiCbEndOfSimulation,
      .cb_rtn    = end_of_sim,
   };
   vhpi_register_cb(&cb_data2, 0);
   check_error();

   vhpiCbDataT cb_data3 = {
      .reason    = vhpiCbRepNextTimeStep,
      .cb_rtn    = rep_next_time_step,
   };
   vhpi_register_cb(&cb_data3, 0);
   check_error();

   vhpiCbDataT cb_data4 = {
      .reason    = vhpiCbNextTimeStep,
      .cb_rtn    = never_call,
   };
   never_call_handle_1 = vhpi_register_cb(&cb_data4, vhpiReturnCb);
   check_error();

   vhpiCbDataT cb_data5 = {
      .reason    = vhpiCbRepNextTimeStep,
      .cb_rtn    = never_call,
   };
   never_call_handle_2 = vhpi_register_cb(&cb_data5, vhpiReturnCb);
   check_error();

   atexit(check_end_of_sim_called);
}
