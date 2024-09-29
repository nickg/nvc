#include "vhpi_test.h"

#include <stdbool.h>

static void init(const vhpiCbDataT *cb_data_p)
{
   vhpiValueT result = {
      .format = vhpiIntVal,
      .value = { .intg = 42 }
   };
   vhpi_put_value(cb_data_p->obj, &result, vhpiDeposit);
   check_error();
}

int32_t __vhpi_init_direct(void)
{
   return 66;
}

void issue988_startup(void)
{
   vhpiForeignDataT init_data = {
      .kind = vhpiFuncF,
      .libraryName = "issue988",
      .modelName = "init",
      .execf = init,
   };
   vhpiHandleT h = vhpi_register_foreignf(&init_data);
   check_error();

   vhpi_release_handle(h);
}
