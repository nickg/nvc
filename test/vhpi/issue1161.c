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

   vhpiHandleT datain = VHPI_CHECK(vhpi_handle_by_name("b.datain", root));

   vhpiHandleT type = VHPI_CHECK(vhpi_handle(vhpiType, datain));
   fail_unless(vhpi_get(vhpiKindP, type) == vhpiSubtypeDeclK);

   vhpiHandleT e = VHPI_CHECK(vhpi_handle_by_name("data", datain));
   vhpi_printf("elem %s", vhpi_get_str(vhpiNameP, e));
   vhpi_printf("size %d", vhpi_get(vhpiSizeP, e));

   fail_unless(vhpi_get(vhpiSizeP, e) == 128);

   vhpiHandleT etype = VHPI_CHECK(vhpi_handle(vhpiType, e));
   fail_unless(vhpi_get(vhpiKindP, etype) == vhpiArrayTypeDeclK);

   vhpi_release_handle(e);
   vhpi_release_handle(datain);
   vhpi_release_handle(root);
}

void issue1161_startup(void)
{
   vhpiCbDataT cb_data = {
      .reason = vhpiCbStartOfSimulation,
      .cb_rtn = start_of_sim,
   };
   VHPI_CHECK(vhpi_register_cb(&cb_data, 0));
}
