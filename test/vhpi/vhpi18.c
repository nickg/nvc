#include "vhpi_test.h"

#include <string.h>
#include <stdlib.h>

static int64_t phys_to_i64(vhpiPhysT phys)
{
   return ((uint64_t)phys.high) << 32 | phys.low;
}

static void check_phys_scalar(vhpiHandleT parent, const char *name,
                              int64_t expect)
{
   vhpiHandleT sig = VHPI_CHECK(vhpi_handle_by_name(name, parent));
   vhpi_printf("%s handle %p", name, sig);

   vhpiValueT val = { .format = vhpiObjTypeVal };
   VHPI_CHECK(vhpi_get_value(sig, &val));
   fail_unless(val.format == vhpiPhysVal);
   fail_unless(phys_to_i64(val.value.phys) == expect);
   fail_unless(val.numElems == 1);

   vhpi_release_handle(sig);
}

static void check_phys_array(vhpiHandleT parent, const char *name,
                             const int64_t *expect, size_t len)
{
   vhpiHandleT sig = VHPI_CHECK(vhpi_handle_by_name(name, parent));
   vhpi_printf("%s handle %p", name, sig);

   vhpiValueT val = {
      .format = vhpiPhysVecVal,
      .bufSize = len * sizeof(vhpiPhysT),
   };
   val.value.physs = malloc(len * sizeof(vhpiPhysT));

   VHPI_CHECK(vhpi_get_value(sig, &val));
   fail_unless(val.format == vhpiPhysVecVal);
   fail_unless(val.numElems == len);

   for (size_t i = 0; i < len; i++)
      fail_unless(phys_to_i64(val.value.physs[i]) == expect[i]);

   free(val.value.physs);
   vhpi_release_handle(sig);
}

static void end_of_sim(const vhpiCbDataT *cb_data)
{
   vhpiHandleT root = VHPI_CHECK(vhpi_handle(vhpiRootInst, NULL));
   fail_if(root == NULL);
   vhpi_printf("root handle %p", root);

   check_phys_scalar(root, "s_freq", INT64_C(100000000000));
   check_phys_scalar(root, "s_voltage", INT64_C(3300));
   check_phys_scalar(root, "s_time", INT64_C(100000000));

   const int64_t expect_freq[] = { 50000000, 100000000, 150000000 };
   const int64_t expect_voltage[] = { 1500, -3300, 5000 };
   const int64_t expect_time[] = { 10000000, 50000000, 100000000 };

   check_phys_array(root, "v_freq_arr", expect_freq, 3);
   check_phys_array(root, "v_voltage_arr", expect_voltage, 3);
   check_phys_array(root, "v_time_arr", expect_time, 3);

   vhpi_release_handle(root);
}

void vhpi18_startup(void)
{
   vhpiCbDataT cb_data = {
      .reason = vhpiCbEndOfSimulation,
      .cb_rtn = end_of_sim,
   };
   VHPI_CHECK(vhpi_register_cb(&cb_data, 0));
}
