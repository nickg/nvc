#include "vhpi_test.h"

#include <stdbool.h>
#include <stdlib.h>

static bool called;

static void check_vec(const vhpiCbDataT *cb_data)
{
   vhpiHandleT param = VHPI_CHECK(vhpi_handle_by_index(vhpiParamDecls,
                                                         cb_data->obj, 0));
   vhpiValueT value = {
      .format = vhpiLogicVecVal,
   };

   size_t need = VHPI_CHECK(vhpi_get_value(param, &value));
   fail_unless(need == 2 * sizeof(vhpiEnumT));

   value.bufSize = need;
   value.value.enumvs = malloc(need);
   fail_unless(value.value.enumvs != NULL);
   fail_unless(VHPI_CHECK(vhpi_get_value(param, &value)) == 0);
   fail_unless(value.numElems == 2);

   free(value.value.enumvs);
   vhpi_release_handle(param);
   called = true;
}

static void end_of_simulation(const vhpiCbDataT *cb_data)
{
   fail_unless(called);
}

void issue1621_startup(void)
{
   vhpiForeignDataT data = {
      .kind = vhpiProcF,
      .libraryName = "issue1621",
      .modelName = "check_vec",
      .execf = check_vec,
   };
   vhpiHandleT h = VHPI_CHECK(vhpi_register_foreignf(&data));
   vhpi_release_handle(h);

   vhpiCbDataT end_data = {
      .reason = vhpiCbEndOfSimulation,
      .cb_rtn = end_of_simulation,
   };
   VHPI_CHECK(vhpi_register_cb(&end_data, 0));
}
