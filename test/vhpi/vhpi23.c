#include "vhpi_test.h"

static void check_int(vhpiHandleT root, const char *name, vhpiIntT expect)
{
   vhpiHandleT h = VHPI_CHECK(vhpi_handle_by_name(name, root));

   vhpiValueT value = { .format = vhpiObjTypeVal };
   VHPI_CHECK(vhpi_get_value(h, &value));
   vhpi_printf("%s = %d", name, value.value.intg);
   fail_unless(value.format == vhpiIntVal);
   fail_unless(value.value.intg == expect);

   vhpi_release_handle(h);
}

static void check_int_vec(vhpiHandleT root, const char *name,
                          const vhpiIntT *expect, int len)
{
   vhpiHandleT h = VHPI_CHECK(vhpi_handle_by_name(name, root));

   vhpiIntT buf[4];
   vhpiValueT value = {
      .format = vhpiObjTypeVal,
      .bufSize = sizeof(buf),
      .value.intgs = buf,
   };
   VHPI_CHECK(vhpi_get_value(h, &value));
   fail_unless(value.format == vhpiIntVecVal);
   fail_unless(value.numElems == len);

   for (int i = 0; i < len; i++) {
      vhpiHandleT elem =
         VHPI_CHECK(vhpi_handle_by_index(vhpiIndexedNames, h, i));

      vhpiValueT elem_value = { .format = vhpiIntVal };
      VHPI_CHECK(vhpi_get_value(elem, &elem_value));

      vhpi_printf("%s(%d) = %d, %d", name, i, buf[i], elem_value.value.intg);
      fail_unless(buf[i] == expect[i]);
      fail_unless(elem_value.value.intg == expect[i]);

      vhpi_release_handle(elem);
   }

   vhpi_release_handle(h);
}

static void start_of_sim(const vhpiCbDataT *cb_data)
{
   vhpiHandleT root = VHPI_CHECK(vhpi_handle(vhpiRootInst, NULL));

   check_int(root, "s1", 200);
   check_int(root, "s2", -5);
   check_int(root, "s3", 40000);
   check_int(root, "s4", 150);
   check_int(root, "s5", 1);

   const vhpiIntT expect_v1[] = { 11, 22, 200, 44 };
   check_int_vec(root, "v1", expect_v1, 4);

   const vhpiIntT expect_v2[] = { -1, 2, -3 };
   check_int_vec(root, "v2", expect_v2, 3);

   const vhpiIntT expect_v3[] = { 1, 200, 3 };
   check_int_vec(root, "v3", expect_v3, 3);

   const vhpiIntT expect_v4[] = { 1, -2, 300000 };
   check_int_vec(root, "v4", expect_v4, 3);

   vhpi_release_handle(root);
}

void vhpi23_startup(void)
{
   vhpiCbDataT cb_data = {
      .reason = vhpiCbStartOfSimulation,
      .cb_rtn = start_of_sim,
   };
   VHPI_CHECK(vhpi_register_cb(&cb_data, 0));
}
