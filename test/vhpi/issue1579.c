#include "vhpi_test.h"

static void add2(const vhpiCbDataT *cb_data)
{
   fail_unless(VHPI_CHECK(vhpi_get(vhpiKindP, cb_data->obj)) ==
               vhpiFuncDeclK);
   check_string(VHPI_CHECK(vhpi_get_str(vhpiNameP, cb_data->obj)), "ADD2");
   check_string(VHPI_CHECK(vhpi_get_str(vhpiFullNameP, cb_data->obj)),
                ":ISSUE1579:ADD2");

   vhpiHandleT param = VHPI_CHECK(vhpi_handle_by_index(vhpiParamDecls,
                                                        cb_data->obj, 0));
   vhpiValueT value = { .format = vhpiIntVal };
   fail_unless(VHPI_CHECK(vhpi_get_value(param, &value)) == 0);
   value.value.intg += 2;
   VHPI_CHECK(vhpi_put_value(cb_data->obj, &value, vhpiDeposit));
   vhpi_release_handle(param);
}

void issue1579_startup(void)
{
   vhpiForeignDataT data = {
      .kind = vhpiFuncF,
      .libraryName = "issue1579",
      .modelName = "add2",
      .execf = add2,
   };
   vhpiHandleT h = VHPI_CHECK(vhpi_register_foreignf(&data));
   vhpi_release_handle(h);
}
