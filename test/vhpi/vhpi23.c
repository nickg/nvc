#include "vhpi_test.h"

#include <stdbool.h>

static bool called;

static void check(const vhpiCbDataT *cb_data)
{
   vhpiHandleT param = VHPI_CHECK(vhpi_handle_by_index(vhpiParamDecls,
                                                         cb_data->obj, 0));
   vhpiValueT value = {
      .format = vhpiIntVal,
   };
   fail_unless(VHPI_CHECK(vhpi_get_value(param, &value)) == 0);
   fail_unless(value.value.intg == 42);

   vhpi_release_handle(param);
   called = true;
}

static void end_of_simulation(const vhpiCbDataT *cb_data)
{
   fail_unless(called);
}

void vhpi23_startup(void)
{
   vhpiForeignDataT data = {
      .kind = vhpiProcF,
      .libraryName = "vhpi23",
      .modelName = "check",
      .execf = check,
   };
   vhpiHandleT h = VHPI_CHECK(vhpi_register_foreignf(&data));
   vhpi_release_handle(h);

   vhpiCbDataT end_data = {
      .reason = vhpiCbEndOfSimulation,
      .cb_rtn = end_of_simulation,
   };
   VHPI_CHECK(vhpi_register_cb(&end_data, 0));
}
