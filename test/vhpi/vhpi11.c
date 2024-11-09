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

   vhpiHandleT g0y2 = vhpi_handle_by_index(vhpiIndexedNames, g0y, 2);
   check_error();
   fail_if(g0y2 == NULL);

   value.format = vhpiCharVal;
   VHPI_CHECK(vhpi_get_value(g0y2, &value));
   vhpi_printf("g0.y(2) value=%c", value.value.ch);
   fail_unless(value.value.ch == 'l');
   fail_unless(value.numElems == 1);

   vhpiHandleT t_sig = VHPI_CHECK(vhpi_handle_by_name("t", root));
   vhpi_printf("t_sig handle %p", t_sig);

   vhpiHandleT t_base = VHPI_CHECK(vhpi_handle(vhpiBaseType, t_sig));
   fail_unless(vhpi_get(vhpiKindP, t_base) == vhpiArrayTypeDeclK);

   {
      vhpiHandleT it = VHPI_CHECK(vhpi_iterator(vhpiConstraints, t_base));

      vhpiHandleT range = VHPI_CHECK(vhpi_scan(it));
      vhpi_printf("t_base left bound %d", vhpi_get(vhpiLeftBoundP, range));
      vhpi_printf("t_base right bound %d", vhpi_get(vhpiRightBoundP, range));
      // XXX: should be undefined/unconstrained!
      fail_unless(vhpi_get(vhpiLeftBoundP, range) == 1);
      fail_unless(vhpi_get(vhpiRightBoundP, range) == 3);
      fail_unless(vhpi_get(vhpiIsUpP, range));
      vhpi_release_handle(range);

      fail_unless(vhpi_scan(it) == NULL);
      vhpi_release_handle(it);
   }

   vhpiHandleT t_elem = VHPI_CHECK(vhpi_handle(vhpiElemType, t_base));
   // XXX: should be vhpiSubtypeDeclk
   fail_unless(vhpi_get(vhpiKindP, t_elem) == vhpiArrayTypeDeclK);

   {
      vhpiHandleT it = VHPI_CHECK(vhpi_iterator(vhpiConstraints, t_elem));

      vhpiHandleT range = VHPI_CHECK(vhpi_scan(it));
      vhpi_printf("t_elem left bound %d", vhpi_get(vhpiLeftBoundP, range));
      vhpi_printf("t_elem right bound %d", vhpi_get(vhpiRightBoundP, range));
      fail_unless(vhpi_get(vhpiLeftBoundP, range) == 7);
      fail_unless(vhpi_get(vhpiRightBoundP, range) == 0);
      fail_if(vhpi_get(vhpiIsUpP, range));
      vhpi_release_handle(range);

      fail_unless(vhpi_scan(it) == NULL);
      vhpi_release_handle(it);
   }

   vhpi_release_handle(t_elem);
   vhpi_release_handle(t_base);
   vhpi_release_handle(t_sig);

   vhpiHandleT u_sig = VHPI_CHECK(vhpi_handle_by_name("u", root));
   vhpi_printf("u_sig handle %p", u_sig);

   vhpiHandleT u_base = VHPI_CHECK(vhpi_handle(vhpiBaseType, u_sig));
   fail_unless(vhpi_get(vhpiKindP, u_base) == vhpiArrayTypeDeclK);

   vhpiHandleT u_elem = VHPI_CHECK(vhpi_handle(vhpiElemType, u_base));
   fail_unless(vhpi_get(vhpiKindP, u_elem) == vhpiSubtypeDeclK);

   {
      vhpiHandleT it = VHPI_CHECK(vhpi_iterator(vhpiConstraints, u_elem));

      vhpiHandleT range = VHPI_CHECK(vhpi_scan(it));
      vhpi_printf("u_elem left bound %d", vhpi_get(vhpiLeftBoundP, range));
      vhpi_printf("u_elem right bound %d", vhpi_get(vhpiRightBoundP, range));
      fail_unless(vhpi_get(vhpiLeftBoundP, range) == 3);
      fail_unless(vhpi_get(vhpiRightBoundP, range) == 0);
      fail_if(vhpi_get(vhpiIsUpP, range));
      vhpi_release_handle(range);

      fail_unless(vhpi_scan(it) == NULL);
      vhpi_release_handle(it);
   }

   vhpi_release_handle(u_elem);
   vhpi_release_handle(u_base);
   vhpi_release_handle(u_sig);

   vhpi_release_handle(handle_sos);
}

void vhpi11_startup(void)
{
   vhpiCbDataT cb_data1 = {
      .reason    = vhpiCbStartOfSimulation,
      .cb_rtn    = start_of_sim,
   };
   handle_sos = vhpi_register_cb(&cb_data1, vhpiReturnCb);
   check_error();
   fail_unless(vhpi_get(vhpiStateP, handle_sos) == vhpiEnable);
}
