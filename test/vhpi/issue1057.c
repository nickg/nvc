#include "vhpi_test.h"

#include <stdio.h>
#include <assert.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>

static void start_of_sim(const vhpiCbDataT *cb_data)
{
   vhpiHandleT sigA = VHPI_CHECK(vhpi_handle_by_name("issue1057.sigA", NULL));
   vhpi_printf("sigA handle %p", sigA);

   fail_unless(vhpi_get(vhpiSizeP, sigA) == 8);

   vhpiHandleT sub = VHPI_CHECK(vhpi_handle(vhpiType, sigA));
   fail_unless(vhpi_get(vhpiKindP, sub) == vhpiArrayTypeDeclK);  // XX: subtype?

   vhpiHandleT it1 = VHPI_CHECK(vhpi_iterator(vhpiConstraints, sub));

   vhpiHandleT range1 = VHPI_CHECK(vhpi_scan(it1));
   vhpi_printf("sub left bound %d", vhpi_get(vhpiLeftBoundP, range1));
   vhpi_printf("sub right bound %d", vhpi_get(vhpiRightBoundP, range1));
   fail_unless(vhpi_get(vhpiLeftBoundP, range1) == 7);
   fail_unless(vhpi_get(vhpiRightBoundP, range1) == 0);

   fail_unless(vhpi_scan(it1) == NULL);
   vhpi_release_handle(it1);

   vhpiHandleT elem = VHPI_CHECK(vhpi_handle(vhpiElemType, sub));
   fail_unless(vhpi_get(vhpiKindP, elem) == vhpiArrayTypeDeclK);  // XX: subtype?

   vhpiHandleT it2 = VHPI_CHECK(vhpi_iterator(vhpiConstraints, elem));

   vhpiHandleT range2 = VHPI_CHECK(vhpi_scan(it2));
   vhpi_printf("elem left bound %d", vhpi_get(vhpiLeftBoundP, range2));
   vhpi_printf("elem right bound %d", vhpi_get(vhpiRightBoundP, range2));
   fail_unless(vhpi_get(vhpiLeftBoundP, range2) == 0);
   fail_unless(vhpi_get(vhpiRightBoundP, range2) == 0);

   fail_unless(vhpi_scan(it2) == NULL);
   vhpi_release_handle(it2);

   vhpi_release_handle(elem);
   vhpi_release_handle(sub);
   vhpi_release_handle(sigA);
}

void issue1057_startup(void)
{
   vhpiCbDataT cb_data = {
      .reason = vhpiCbStartOfSimulation,
      .cb_rtn = start_of_sim,
   };
   VHPI_CHECK(vhpi_register_cb(&cb_data, 0));
}
