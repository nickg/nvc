#include "vhpi_test.h"

#include <stdio.h>
#include <assert.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>

static void start_of_sim(const vhpiCbDataT *cb_data)
{
   vhpiHandleT t1 = VHPI_CHECK(vhpi_handle_by_name("issue1240.t1", NULL));
   vhpi_printf("tt1 handle %p", t1);

   fail_unless(vhpi_get(vhpiKindP, t1) == vhpiArrayTypeDeclK);

   vhpiHandleT t1e = VHPI_CHECK(vhpi_handle(vhpiElemType, t1));
   vhpi_printf("elem handle %p", t1e);

   fail_unless(vhpi_get(vhpiKindP, t1e) == vhpiArrayTypeDeclK);  // XXX: vhpiSubtypeDeclK

   vhpi_release_handle(t1e);
   vhpi_release_handle(t1);

   vhpiHandleT t2 = VHPI_CHECK(vhpi_handle_by_name("issue1240.t2", NULL));
   vhpi_printf("tt2 handle %p", t2);

   fail_unless(vhpi_get(vhpiKindP, t2) == vhpiArrayTypeDeclK);

   vhpiHandleT t2e = VHPI_CHECK(vhpi_handle(vhpiElemType, t2));
   vhpi_printf("elem handle %p", t2e);

   fail_unless(vhpi_get(vhpiKindP, t2e) == vhpiArrayTypeDeclK);  // XXX: vhpiSubtypeDeclK

   vhpi_release_handle(t2e);
   vhpi_release_handle(t2);
}

void issue1240_startup(void)
{
   vhpiCbDataT cb_data = {
      .reason = vhpiCbStartOfSimulation,
      .cb_rtn = start_of_sim,
   };
   VHPI_CHECK(vhpi_register_cb(&cb_data, 0));
}
