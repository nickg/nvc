#include "vhpi_test.h"

#include <string.h>
#include <stdbool.h>

static void x_value_change(const vhpiCbDataT *cb_data)
{
   const vhpiCharT *name =
      VHPI_CHECK(vhpi_get_str(vhpiFullNameP, cb_data->obj));
   vhpi_printf("%s value change", name);

   // VHPI spec says releasing the passed-in handle is erroneous
   vhpi_release_handle(cb_data->obj);

   vhpiErrorInfoT info;
   fail_unless(vhpi_check_error(&info));
}

static void start_of_sim(const vhpiCbDataT *cb_data)
{
   vhpiHandleT root = VHPI_CHECK(vhpi_handle(vhpiRootInst, NULL));
   fail_if(root == NULL);
   vhpi_printf("root handle %p", root);

   vhpiHandleT x = VHPI_CHECK(vhpi_handle_by_name("x", root));

   vhpiCbDataT x_cb_data = {
      .obj = x,
      .cb_rtn = x_value_change,
      .reason = vhpiCbValueChange,
   };
   VHPI_CHECK(vhpi_register_cb(&x_cb_data, 0));

   vhpi_release_handle(root);

   // This should not invalidate the handle passed to the callback
   vhpi_release_handle(x);
}

void vhpi16_startup(void)
{
   vhpiHandleT root = vhpi_handle(vhpiRootInst, NULL);
   fail_unless(root == NULL);

   vhpiCbDataT cb_data = {
      .reason = vhpiCbStartOfSimulation,
      .cb_rtn = start_of_sim,
   };
   VHPI_CHECK(vhpi_register_cb(&cb_data, 0));
}
