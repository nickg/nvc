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

   vhpiHandleT g0x = vhpi_handle_by_name("g0.x", root);
   check_error();

   vhpiValueT value = {
      .format = vhpiObjTypeVal
   };
   vhpi_get_value(g0x, &value);
   check_error();
   fail_unless(value.format == vhpiIntVal);
   vhpi_printf("g0.x value=%d", value.value.intg);
   fail_unless(value.value.intg == 55);
   fail_unless(value.numElems == 1);

   vhpiHandleT g0y = vhpi_handle_by_name("g0.y", root);
   check_error();

   value.format = vhpiObjTypeVal;
   value.bufSize = 0;
   value.value.str = NULL;

   const int bufsz = vhpi_get_value(g0y, &value);
   check_error();
   fail_unless(value.format == vhpiStrVal);
   fail_unless(bufsz == 6);

   vhpiCharT str[6];
   value.value.str = str;
   value.bufSize = sizeof(str);
   vhpi_get_value(g0y, &value);
   check_error();
   fail_unless(value.numElems == 5);
   vhpi_printf("g0.y value '%s'", (char *)str);
   fail_unless(strcmp((char *)str, "hello") == 0);
   fail_unless(vhpi_get(vhpiSizeP, g0y) == 5);

   vhpi_release_handle(handle_sos);
}

void vhpi11_startup(void)
{
   vhpiHandleT root = vhpi_handle(vhpiRootInst, NULL);
   check_error();
   fail_if(root == NULL);
   vhpi_printf("root handle %p", root);

   vhpiHandleT g0 = vhpi_handle_by_name("g0", root);
   check_error();
   fail_if(g0 == NULL);
   vhpi_printf("g0 handle %p", g0);
   fail_unless(vhpi_get(vhpiKindP, g0) == vhpiGenericDeclK);
   fail_unless(vhpi_get(vhpiModeP, g0) == vhpiInMode);
   fail_unless(vhpi_get(vhpiIsLocalP, g0) == vhpiFalse);

   vhpiHandleT g0x = vhpi_handle_by_name("g0.x", root);
   check_error();
   fail_if(g0x == NULL);
   vhpi_printf("g0x handle %p", g0x);
   fail_unless(vhpi_get(vhpiKindP, g0x) == vhpiSelectedNameK);

   vhpiCbDataT cb_data1 = {
      .reason    = vhpiCbStartOfSimulation,
      .cb_rtn    = start_of_sim,
   };
   handle_sos = vhpi_register_cb(&cb_data1, vhpiReturnCb);
   check_error();
   fail_unless(vhpi_get(vhpiStateP, handle_sos) == vhpiEnable);
}
