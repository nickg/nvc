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

#include <string.h>
#include <stdbool.h>

static void start_of_sim(const vhpiCbDataT *cb_data)
{
   vhpiHandleT root = vhpi_handle(vhpiRootInst, NULL);
   check_error();
   fail_if(root == NULL);
   vhpi_printf("root handle %p", root);

   vhpiHandleT bad1 = vhpi_handle_by_name(":bad", NULL);
   fail_unless(bad1 == NULL);

   vhpiHandleT root2 = vhpi_handle_by_name(":vhpi15", NULL);
   fail_unless(root2 == root);
   vhpi_release_handle(root2);

   vhpiHandleT pack1 = vhpi_handle_by_name(":pack1", NULL);
   check_handle(pack1);
   vhpi_printf("pack1 handle %p", pack1);

   vhpiHandleT du = vhpi_handle(vhpiDesignUnit, pack1);
   check_handle(du);
   fail_unless(vhpi_get(vhpiKindP, du) == vhpiPackDeclK);

   vhpiHandleT su = vhpi_handle(vhpiPrimaryUnit, du);
   fail_unless(su == NULL);

   vhpiHandleT c1 = vhpi_handle_by_name("c1", pack1);
   check_handle(c1);
   vhpi_printf("c1 full name is %s", vhpi_get_str(vhpiFullNameP, c1));
   fail_unless(vhpi_get(vhpiKindP, c1) == vhpiConstDeclK);

   vhpiValueT c1_value = {
      .format = vhpiIntVal,
   };
   vhpi_get_value(c1, &c1_value);
   check_error();
   vhpi_printf("c1 value is %d", c1_value.value.intg);
   fail_unless(c1_value.value.intg == 42);

   vhpiHandleT c2 = vhpi_handle_by_name("c2", pack1);
   check_handle(c2);
   vhpi_printf("c2 full name is %s", vhpi_get_str(vhpiFullNameP, c2));
   fail_unless(vhpi_get(vhpiKindP, c2) == vhpiConstDeclK);

   vhpiCharT c2_str[6];
   vhpiValueT c2_value = {
      .format    = vhpiStrVal,
      .bufSize   = sizeof(c2_str),
      .value.str = c2_str
   };
   vhpi_get_value(c2, &c2_value);
   check_error();
   vhpi_printf("c2 value is %s", c2_str);
   fail_unless(strcmp((char *)c2_str, "hello") == 0);

   vhpiHandleT it = vhpi_iterator(vhpiPackInsts, NULL);
   fail_if(it == NULL);

   bool found = false;
   int i = 0;
   for (vhpiHandleT h = vhpi_scan(it); h != NULL; h = vhpi_scan(it), i++) {
      vhpi_printf("found package %s", vhpi_get_str(vhpiNameP, h));

      found |= vhpi_compare_handles(h, pack1);

      vhpiHandleT h2 = vhpi_handle_by_index(vhpiPackInsts, NULL, i);
      fail_unless(vhpi_compare_handles(h, h2));
      vhpi_release_handle(h);
      vhpi_release_handle(h2);
   }
   vhpi_release_handle(it);
   fail_unless(found);
   fail_unless(i == 2);

   vhpi_release_handle(root);
   vhpi_release_handle(pack1);
   vhpi_release_handle(c1);
   vhpi_release_handle(c2);
   vhpi_release_handle(du);
}

void vhpi15_startup(void)
{
   vhpiCbDataT cb_data1 = {
      .reason = vhpiCbStartOfSimulation,
      .cb_rtn = start_of_sim,
   };
   vhpi_register_cb(&cb_data1, 0);
   check_error();
}
