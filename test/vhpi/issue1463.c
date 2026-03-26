#include "vhpi_test.h"

#include <stdio.h>
#include <assert.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>

static void start_of_sim(const vhpiCbDataT *cb_data)
{
   vhpiHandleT root = VHPI_CHECK(vhpi_handle(vhpiRootInst, NULL));
   fail_if(root == NULL);
   vhpi_printf("root handle %p", root);

   const vhpiCharT *root_name = vhpi_get_str(vhpiNameP, root);
   vhpi_printf("root name is %s", root_name);

   vhpiHandleT sig = VHPI_CHECK(vhpi_handle_by_name("sig", root));

   {
      vhpiHandleT sub = VHPI_CHECK(vhpi_handle(vhpiType, sig));
      vhpiHandleT it = VHPI_CHECK(vhpi_iterator(vhpiConstraints, sub));
      vhpiHandleT range = VHPI_CHECK(vhpi_scan(it));

      fail_unless(vhpi_get(vhpiLeftBoundP, range) == 0);
      fail_unless(vhpi_get(vhpiRightBoundP, range) == 1);
      fail_unless(vhpi_get(vhpiIsUpP, range) == 1);
   }

   vhpiHandleT sig0 =
      VHPI_CHECK(vhpi_handle_by_index(vhpiIndexedNames, sig, 0));
   vhpiHandleT data = VHPI_CHECK(vhpi_handle_by_name("data", sig0));

   fail_unless(vhpi_get(vhpiSizeP, data) == 2);

   {
      vhpiHandleT sub = VHPI_CHECK(vhpi_handle(vhpiType, data));
      vhpiHandleT it = VHPI_CHECK(vhpi_iterator(vhpiConstraints, sub));
      vhpiHandleT range = VHPI_CHECK(vhpi_scan(it));

      fail_unless(vhpi_get(vhpiLeftBoundP, range) == 0);
      fail_unless(vhpi_get(vhpiRightBoundP, range) == 1);
      fail_unless(vhpi_get(vhpiIsUpP, range) == 1);
   }

   vhpiHandleT data0 =
      VHPI_CHECK(vhpi_handle_by_index(vhpiIndexedNames, data, 0));

   const vhpiCharT *name = VHPI_CHECK(vhpi_get_str(vhpiNameP, data0));
   vhpi_printf("data0 name %s", name);
   fail_unless(strcmp((const char *)name, "SIG(0).DATA(0)") == 0);

   vhpi_release_handle(root);
}

void issue1463_startup(void)
{
   vhpiCbDataT cb_data = {
      .reason = vhpiCbStartOfSimulation,
      .cb_rtn = start_of_sim,
   };
   VHPI_CHECK(vhpi_register_cb(&cb_data, 0));
}
