#include "vhpi_test.h"

static void vhpi_intout(const vhpiCbDataT *cb_data)
{
    vhpiHandleT i_h = vhpi_handle_by_index(vhpiParamDecls, cb_data->obj, 0);
    vhpiValueT int_v = {.format = vhpiIntVal};
    int_v.value.intg = 1;
    vhpi_put_value(i_h, &int_v, vhpiDepositPropagate);
    vhpi_release_handle(i_h);
}

void issue1233_startup(void)
{
    vhpiHandleT cb_h;
    vhpiForeignDataT foreign_intout = {
        vhpiProcF, "vhpi.so", "vhpi_intout", NULL, vhpi_intout};
    cb_h = vhpi_register_foreignf(&foreign_intout);
    vhpi_release_handle(cb_h);
    vhpiForeignDataT foreign_rangedintout = {
        vhpiProcF, "vhpi.so", "vhpi_rangedintout", NULL, vhpi_intout};
    cb_h = vhpi_register_foreignf(&foreign_rangedintout);
    vhpi_release_handle(cb_h);
}
