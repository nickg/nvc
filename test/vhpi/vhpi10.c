#include "vhpi_test.h"

#include <string.h>

static vhpiHandleT handle_sos;

static void start_of_sim(const vhpiCbDataT *cb_data)
{
   vhpiHandleT root = vhpi_handle(vhpiRootInst, NULL);
   check_error();

   vhpiHandleT g0 = vhpi_handle_by_name("g0", root);
   check_error();

   vhpiValueT value = {
      .format = vhpiObjTypeVal
   };
   vhpi_get_value(g0, &value);
   check_error();
   fail_unless(value.format == vhpiIntVal);
   vhpi_printf("value=%d", value.value.intg);
   fail_unless(value.value.intg == 42);
   fail_unless(value.numElems == 0);

   vhpi_release_handle(handle_sos);
}

void vhpi10_startup(void)
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

   vhpiCbDataT cb_data1 = {
      .reason    = vhpiCbStartOfSimulation,
      .cb_rtn    = start_of_sim,
      .user_data = (char *)"some user data",
   };
   handle_sos = vhpi_register_cb(&cb_data1, vhpiReturnCb);
   check_error();
   fail_unless(vhpi_get(vhpiStateP, handle_sos) == vhpiEnable);

   vhpiValueT value = {
      .format = vhpiObjTypeVal
   };
   vhpi_get_value(g0, &value);

   vhpiErrorInfoT info;
   fail_unless(vhpi_check_error(&info));
}
