//
//  Copyright (C) 2023  Nick Gasson
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

#include <string.h>

static vhpiHandleT handle_sos;

static void start_of_sim(const vhpiCbDataT *cb_data)
{
   vhpiHandleT root = vhpi_handle(vhpiRootInst, NULL);
   check_error();
   fail_if(root == NULL);
   vhpi_printf("root handle %p", root);

   vhpiHandleT delay_signal = vhpi_handle_by_name("delay", root);
   check_error();
   fail_if(delay_signal == NULL);
   vhpi_printf("x handle %p", delay_signal);
   fail_unless(vhpi_get(vhpiKindP, delay_signal) == vhpiSigDeclK);

   vhpiHandleT delay_type = vhpi_handle(vhpiType, delay_signal);
   check_error();
   fail_if(delay_type == NULL);
   vhpi_printf("delay type handle %p", delay_type);
   vhpi_printf("delay type name is %s", vhpi_get_str(vhpiNameP, delay_type));
   vhpi_printf("delay type full name is %s",
               vhpi_get_str(vhpiFullNameP, delay_type));
   fail_unless(vhpi_get(vhpiNumDimensionsP, delay_type) == 1);
   fail_if(vhpi_get(vhpiIsUnconstrainedP, delay_type));

   vhpiHandleT delay_constrs = vhpi_iterator(vhpiConstraints, delay_type);
   check_error();
   fail_if(delay_constrs == NULL);

   vhpiHandleT delay_range = vhpi_scan(delay_constrs);
   check_error();
   fail_if(delay_range == NULL);
   fail_unless(vhpi_scan(delay_constrs) == NULL);
   vhpi_printf("delay type range handle %p", delay_range);
   vhpi_printf("delay left bound %d", vhpi_get(vhpiLeftBoundP, delay_range));
   vhpi_printf("delay right bound %d", vhpi_get(vhpiRightBoundP, delay_range));
   fail_unless(vhpi_get(vhpiLeftBoundP, delay_range) == 9);
   fail_unless(vhpi_get(vhpiRightBoundP, delay_range) == 0);
   fail_if(vhpi_get(vhpiIsUpP, delay_range));

   vhpi_release_handle(handle_sos);
}

void issue762_startup(void)
{
   vhpiCbDataT cb_data1 = {
      .reason    = vhpiCbStartOfSimulation,
      .cb_rtn    = start_of_sim,
   };
   handle_sos = vhpi_register_cb(&cb_data1, vhpiReturnCb);
   check_error();
   fail_unless(vhpi_get(vhpiStateP, handle_sos) == vhpiEnable);
}
