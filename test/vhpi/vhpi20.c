#include "vhpi_test.h"

#include <string.h>

static void start_of_sim(const vhpiCbDataT *cb_data)
{
   vhpiHandleT root = VHPI_CHECK(vhpi_handle(vhpiRootInst, NULL));

   // -----------------------------------------------------------------------
   // x: pre-VHDL-2008 nested 3-D array of record (cplx_3d_t)
   // Exercises multi-level indexedName chaining for non-homogeneous arrays.
   // -----------------------------------------------------------------------
   {
      vhpiHandleT x = VHPI_CHECK(vhpi_handle_by_name("x", root));

      // x(1)(1)(1).r — flat leaf index 7 of 8
      vhpiHandleT x1   = VHPI_CHECK(vhpi_handle_by_index(vhpiIndexedNames, x, 1));
      vhpiHandleT x11  = VHPI_CHECK(vhpi_handle_by_index(vhpiIndexedNames, x1, 1));
      vhpiHandleT x111 = VHPI_CHECK(vhpi_handle_by_index(vhpiIndexedNames, x11, 1));
      vhpiHandleT x111r = VHPI_CHECK(vhpi_handle_by_name("r", x111));

      vhpiValueT value = { .format = vhpiBinStrVal };
      vhpiCharT buf[16];
      value.bufSize = sizeof(buf);
      value.value.str = buf;
      VHPI_CHECK(vhpi_get_value(x111r, &value));
      vhpi_printf("x(1)(1)(1).r = '%s'", (char *)buf);

      vhpiEnumT bits[] = { 1, 0, 1, 0, 1, 0, 1, 0 };
      value.format = vhpiLogicVecVal;
      value.value.enumvs = bits;
      value.bufSize = sizeof(bits);
      VHPI_CHECK(vhpi_put_value(x111r, &value, vhpiDepositPropagate));

      // x(0)(1)(0).r — flat leaf index 2
      vhpiHandleT x0   = VHPI_CHECK(vhpi_handle_by_index(vhpiIndexedNames, x, 0));
      vhpiHandleT x01  = VHPI_CHECK(vhpi_handle_by_index(vhpiIndexedNames, x0, 1));
      vhpiHandleT x010 = VHPI_CHECK(vhpi_handle_by_index(vhpiIndexedNames, x01, 0));
      vhpiHandleT x010r = VHPI_CHECK(vhpi_handle_by_name("r", x010));

      value.format = vhpiBinStrVal;
      value.bufSize = sizeof(buf);
      value.value.str = buf;
      VHPI_CHECK(vhpi_get_value(x010r, &value));
      vhpi_printf("x(0)(1)(0).r = '%s'", (char *)buf);

      vhpi_release_handle(x010r);
      vhpi_release_handle(x010);
      vhpi_release_handle(x01);
      vhpi_release_handle(x0);
      vhpi_release_handle(x111r);
      vhpi_release_handle(x111);
      vhpi_release_handle(x11);
      vhpi_release_handle(x1);
      vhpi_release_handle(x);
   }

   // -----------------------------------------------------------------------
   // y: native VHDL-2008 2-D array of record (cplx_mat_t(0 to 1, 0 to 1))
   // vhpiIndexedNames expands all dimension combinations into a flat list;
   // a single vhpi_handle_by_index call reaches any element directly.
   // -----------------------------------------------------------------------
   {
      vhpiHandleT y = VHPI_CHECK(vhpi_handle_by_name("y", root));

      // Flat list order (row-major): (0,0)=0  (0,1)=1  (1,0)=2  (1,1)=3
      // y(0,1).i — list position 1
      vhpiHandleT y01  = VHPI_CHECK(vhpi_handle_by_index(vhpiIndexedNames, y, 1));
      vhpiHandleT y01i = VHPI_CHECK(vhpi_handle_by_name("i", y01));

      vhpiValueT value = { .format = vhpiBinStrVal };
      vhpiCharT buf[16];
      value.bufSize = sizeof(buf);
      value.value.str = buf;
      VHPI_CHECK(vhpi_get_value(y01i, &value));
      vhpi_printf("y(0,1).i = '%s'", (char *)buf);

      vhpiEnumT bits[] = { 0, 0, 0, 0, 1, 1, 1, 1 };
      value.format = vhpiLogicVecVal;
      value.value.enumvs = bits;
      value.bufSize = sizeof(bits);
      VHPI_CHECK(vhpi_put_value(y01i, &value, vhpiDepositPropagate));

      // y(1,0).r — list position 2
      vhpiHandleT y10  = VHPI_CHECK(vhpi_handle_by_index(vhpiIndexedNames, y, 2));
      vhpiHandleT y10r = VHPI_CHECK(vhpi_handle_by_name("r", y10));

      value.format = vhpiBinStrVal;
      value.bufSize = sizeof(buf);
      value.value.str = buf;
      VHPI_CHECK(vhpi_get_value(y10r, &value));
      vhpi_printf("y(1,0).r = '%s'", (char *)buf);

      vhpi_release_handle(y10r);
      vhpi_release_handle(y10);
      vhpi_release_handle(y01i);
      vhpi_release_handle(y01);
      vhpi_release_handle(y);
   }

   // -----------------------------------------------------------------------
   // z: pre-VHDL-2008 1-D array of raw bit_vector (slv_arr_t(0 to 3))
   // Homogeneous: the whole signal is one flat signal; offset arithmetic
   // selects the element without a scope lookup.
   // -----------------------------------------------------------------------
   {
      vhpiHandleT z = VHPI_CHECK(vhpi_handle_by_name("z", root));

      // z(2) — list position 2
      vhpiHandleT z2 = VHPI_CHECK(vhpi_handle_by_index(vhpiIndexedNames, z, 2));

      vhpiValueT value = { .format = vhpiBinStrVal };
      vhpiCharT buf[16];
      value.bufSize = sizeof(buf);
      value.value.str = buf;
      VHPI_CHECK(vhpi_get_value(z2, &value));
      vhpi_printf("z(2) = '%s'", (char *)buf);

      vhpiEnumT bits[] = { 1, 1, 0, 0, 0, 0, 1, 1 };
      value.format = vhpiLogicVecVal;
      value.value.enumvs = bits;
      value.bufSize = sizeof(bits);
      VHPI_CHECK(vhpi_put_value(z2, &value, vhpiDepositPropagate));

      vhpi_release_handle(z2);
      vhpi_release_handle(z);
   }

   // -----------------------------------------------------------------------
   // w: native VHDL-2008 2-D array of raw bit_vector (slv_mat_t(0 to 1, 0 to 1))
   // Combines native multi-dim indexing with homogeneous (SLV) element access.
   // -----------------------------------------------------------------------
   {
      vhpiHandleT w = VHPI_CHECK(vhpi_handle_by_name("w", root));

      // Flat list order: (0,0)=0  (0,1)=1  (1,0)=2  (1,1)=3
      // w(1,1) — list position 3
      vhpiHandleT w11 = VHPI_CHECK(vhpi_handle_by_index(vhpiIndexedNames, w, 3));

      vhpiValueT value = { .format = vhpiBinStrVal };
      vhpiCharT buf[16];
      value.bufSize = sizeof(buf);
      value.value.str = buf;
      VHPI_CHECK(vhpi_get_value(w11, &value));
      vhpi_printf("w(1,1) = '%s'", (char *)buf);

      vhpiEnumT bits[] = { 0, 1, 0, 1, 0, 1, 0, 1 };
      value.format = vhpiLogicVecVal;
      value.value.enumvs = bits;
      value.bufSize = sizeof(bits);
      VHPI_CHECK(vhpi_put_value(w11, &value, vhpiDepositPropagate));

      vhpi_release_handle(w11);
      vhpi_release_handle(w);
   }

   vhpi_release_handle(root);
}

void vhpi20_startup(void)
{
   vhpiCbDataT cb_data = {
      .reason = vhpiCbStartOfSimulation,
      .cb_rtn = start_of_sim,
   };
   VHPI_CHECK(vhpi_register_cb(&cb_data, 0));
}
