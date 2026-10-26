#include "vhpi_test.h"

static void get_one(const vhpiCbDataT *cb_data)
{
   vhpiValueT value = { .format = vhpiIntVal, .value.intg = 1 };
   VHPI_CHECK(vhpi_put_value(cb_data->obj, &value, vhpiDeposit));
}

void vhpi26_startup(void)
{
   vhpiForeignDataT data = {
      .kind = vhpiFuncF,
      .libraryName = "vhpi26",
      .modelName = "get_one",
      .execf = get_one,
   };
   vhpiHandleT h = VHPI_CHECK(vhpi_register_foreignf(&data));
   vhpi_release_handle(h);
}
