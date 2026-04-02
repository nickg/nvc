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

   vhpiHandleT comp_i = VHPI_CHECK(vhpi_handle_by_name("uut.i", root));

   vhpiHandleT i_type = VHPI_CHECK(vhpi_handle(vhpiType, comp_i));

   const vhpiCharT *name = VHPI_CHECK(vhpi_get_str(vhpiNameP, i_type));
   vhpi_printf("i type name %s", name);
   fail_unless(strcmp((const char *)name, "T_WIRE") == 0);

   vhpi_release_handle(comp_i);
   vhpi_release_handle(i_type);
   vhpi_release_handle(root);
}

void issue1473_startup(void)
{
   vhpiCbDataT cb_data = {
      .reason = vhpiCbStartOfSimulation,
      .cb_rtn = start_of_sim,
   };
   VHPI_CHECK(vhpi_register_cb(&cb_data, 0));
}
