#include "vhpi_test.h"

#include <string.h>

static void start_of_sim(const vhpiCbDataT *cb_data)
{
   vhpiHandleT root = VHPI_CHECK(vhpi_handle(vhpiRootInst, NULL));

   vhpiHandleT x = VHPI_CHECK(vhpi_handle_by_name("x", root));

   // Drill into x(1)(1)(1).r — exercises flat scope index 7 of 8 children
   vhpiHandleT x1 = VHPI_CHECK(vhpi_handle_by_index(vhpiIndexedNames, x, 1));
   vhpiHandleT x11 = VHPI_CHECK(vhpi_handle_by_index(vhpiIndexedNames, x1, 1));
   vhpiHandleT x111 = VHPI_CHECK(vhpi_handle_by_index(vhpiIndexedNames, x11, 1));

   vhpiHandleT x111r = VHPI_CHECK(vhpi_handle_by_name("r", x111));

   vhpiValueT value = {
      .format = vhpiBinStrVal,
   };
   vhpiCharT buf[16];
   value.bufSize = sizeof(buf);
   value.value.str = buf;
   VHPI_CHECK(vhpi_get_value(x111r, &value));
   vhpi_printf("x(1)(1)(1).r = '%s'", (char *)buf);

   vhpiEnumT bits[] = { 1, 0, 1, 0, 1, 0, 1, 0 };
   value.format = vhpiLogicVecVal;
   value.value.enumvs = bits;
   value.bufSize = sizeof(bits);
   VHPI_CHECK(vhpi_put_value(x111r, &value, vhpiDepositPropagate));

   // And x(0)(1)(0).r — exercises flat scope index 2
   vhpiHandleT x0 = VHPI_CHECK(vhpi_handle_by_index(vhpiIndexedNames, x, 0));
   vhpiHandleT x01 = VHPI_CHECK(vhpi_handle_by_index(vhpiIndexedNames, x0, 1));
   vhpiHandleT x010 = VHPI_CHECK(vhpi_handle_by_index(vhpiIndexedNames, x01, 0));
   vhpiHandleT x010r = VHPI_CHECK(vhpi_handle_by_name("r", x010));

   VHPI_CHECK(vhpi_get_value(x010r, &value));
   vhpi_printf("x(0)(1)(0).r = '%s'", (char *)buf);

   vhpi_release_handle(x010r);
   vhpi_release_handle(x010);
   vhpi_release_handle(x01);
   vhpi_release_handle(x0);
   vhpi_release_handle(x111r);
   vhpi_release_handle(x111);
   vhpi_release_handle(x11);
   vhpi_release_handle(x1);
   vhpi_release_handle(x);
   vhpi_release_handle(root);
}

void vhpi20_startup(void)
{
   vhpiCbDataT cb_data = {
      .reason = vhpiCbStartOfSimulation,
      .cb_rtn = start_of_sim,
   };
   VHPI_CHECK(vhpi_register_cb(&cb_data, 0));
}
