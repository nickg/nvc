#include "vhpi_test.h"

static void start_of_sim(const vhpiCbDataT *cb_data)
{
   vhpiHandleT root = VHPI_CHECK(vhpi_handle(vhpiRootInst, NULL));

   // Bound through a component declaration
   vhpiHandleT i0 = VHPI_CHECK(vhpi_handle_by_name("i0", root));
   fail_unless(vhpi_get(vhpiKindP, i0) == vhpiCompInstStmtK);
   check_string(vhpi_get_str(vhpiCompNameP, i0), "sub");
   vhpi_release_handle(i0);

   // Unbound: no design unit, but the component name is still known
   vhpiHandleT i1 = VHPI_CHECK(vhpi_handle_by_name("i1", root));
   fail_unless(vhpi_get(vhpiKindP, i1) == vhpiCompInstStmtK);
   check_string(vhpi_get_str(vhpiCompNameP, i1), "nothere");
   vhpi_release_handle(i1);

   // Direct entity instantiation has no component
   vhpiHandleT i2 = VHPI_CHECK(vhpi_handle_by_name("i2", root));
   fail_unless(vhpi_get(vhpiKindP, i2) == vhpiCompInstStmtK);
   fail_unless(vhpi_get_str(vhpiCompNameP, i2) == NULL);

   vhpiErrorInfoT info;
   fail_unless(vhpi_check_error(&info));
   vhpi_release_handle(i2);

   vhpi_release_handle(root);
}

void vhpi24_startup(void)
{
   vhpiCbDataT cb_data = {
      .reason = vhpiCbStartOfSimulation,
      .cb_rtn = start_of_sim,
   };
   VHPI_CHECK(vhpi_register_cb(&cb_data, 0));
}
