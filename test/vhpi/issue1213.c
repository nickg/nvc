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

   vhpiHandleT ifgen = VHPI_CHECK(vhpi_handle_by_name("ifgen_label", root));
   vhpi_printf("ifgen handle %p", ifgen);
   fail_unless(vhpi_get(vhpiKindP, ifgen) == vhpiIfGenerateK);

   vhpiHandleT inst = VHPI_CHECK(vhpi_handle_by_name("and_gate_inst", ifgen));
   vhpi_printf("and gate inst handle %p", inst);
   fail_unless(vhpi_get(vhpiKindP, inst) == vhpiCompInstStmtK);

   vhpiHandleT du = VHPI_CHECK(vhpi_handle(vhpiDesignUnit, inst));
   fail_unless(du == NULL);

   vhpiHandleT casegen = VHPI_CHECK(vhpi_handle_by_name("casegen_label", root));
   vhpi_printf("casgen handle %p", casegen);

   // No VHPI kind for this
   fail_unless(vhpi_get(vhpiKindP, casegen) == vhpiIfGenerateK);

   vhpi_release_handle(casegen);
   vhpi_release_handle(inst);
   vhpi_release_handle(ifgen);
   vhpi_release_handle(root);
}

void issue1213_startup(void)
{
   vhpiCbDataT cb_data = {
      .reason = vhpiCbStartOfSimulation,
      .cb_rtn = start_of_sim,
   };
   VHPI_CHECK(vhpi_register_cb(&cb_data, 0));
}
