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
   check_string(vhpi_get_str(vhpiNameP, g0x), "G0.X");
   check_string(vhpi_get_str(vhpiCaseNameP, g0x), "g0.x");
   check_string(vhpi_get_str(vhpiFullNameP, g0x), ":VHPI11:G0.X");
   check_string(vhpi_get_str(vhpiFullCaseNameP, g0x), ":vhpi11:g0.x");

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

   vhpiHandleT s_sig = VHPI_CHECK(vhpi_handle_by_name("s", root));
   vhpi_printf("s_sig handle %p", s_sig);

   vhpiHandleT s_type = VHPI_CHECK(vhpi_handle(vhpiBaseType, s_sig));
   fail_unless(vhpi_get(vhpiKindP, s_type) == vhpiRecordTypeDeclK);

   {
      vhpiHandleT it = VHPI_CHECK(vhpi_iterator(vhpiRecordElems, s_type));

      vhpiHandleT x_elem = VHPI_CHECK(vhpi_scan(it));
      fail_if(x_elem == NULL);
      fail_unless(vhpi_get(vhpiKindP, x_elem) == vhpiElemDeclK);
      fail_unless(strcmp((char *)vhpi_get_str(vhpiNameP, x_elem), "X") == 0);
      fail_unless(vhpi_get(vhpiPositionP, x_elem) == 0);

      vhpiHandleT x_type = VHPI_CHECK(vhpi_handle(vhpiType, x_elem));
      fail_unless(vhpi_get(vhpiKindP, x_type) == vhpiSubtypeDeclK);

      vhpiHandleT x_base = VHPI_CHECK(vhpi_handle(vhpiBaseType, x_elem));
      fail_unless(vhpi_get(vhpiKindP, x_base) == vhpiIntTypeDeclK);

      fail_if(vhpi_compare_handles(x_type, x_base));

      vhpiHandleT y_elem = VHPI_CHECK(vhpi_scan(it));
      fail_if(y_elem == NULL);
      fail_unless(vhpi_get(vhpiKindP, y_elem) == vhpiElemDeclK);
      fail_unless(strcmp((char *)vhpi_get_str(vhpiNameP, y_elem), "Y") == 0);
      fail_unless(vhpi_get(vhpiPositionP, y_elem) == 1);

      vhpiHandleT y_type = VHPI_CHECK(vhpi_handle(vhpiType, y_elem));
      fail_unless(vhpi_get(vhpiKindP, y_type) == vhpiArrayTypeDeclK);

      vhpiHandleT y_base = VHPI_CHECK(vhpi_handle(vhpiBaseType, y_elem));
      fail_unless(vhpi_get(vhpiKindP, y_base) == vhpiArrayTypeDeclK);

      fail_unless(vhpi_compare_handles(y_type, y_base));

      fail_unless(vhpi_scan(it) == NULL);
      vhpi_release_handle(it);
   }

   vhpi_release_handle(s_type);
   vhpi_release_handle(s_sig);

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
   }

   vhpi_release_handle(u_elem);
   vhpi_release_handle(u_base);
   vhpi_release_handle(u_sig);

   vhpiHandleT v_sig = VHPI_CHECK(vhpi_handle_by_name("v", root));
   vhpi_printf("v_sig handle %p", u_sig);

   vhpiHandleT v_base = VHPI_CHECK(vhpi_handle(vhpiBaseType, v_sig));
   fail_unless(vhpi_get(vhpiKindP, v_base) == vhpiIntTypeDeclK);

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
