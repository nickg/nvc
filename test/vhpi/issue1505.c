#include "vhpi_test.h"

static vhpiHandleT h_entity1_a;
static vhpiHandleT h_entity2_a;
static vhpiHandleT h_b1;
static vhpiHandleT h_b2;
static vhpiHandleT h_check_cb;

static void check_outputs(const vhpiCbDataT *cb_data)
{
   static int attempts = 0;

   vhpiValueT value = {
      .format = vhpiLogicVal,
   };

   VHPI_CHECK(vhpi_get_value(h_b1, &value));
   if (value.value.enumv != vhpi1) {
      fail_unless(attempts++ < 4);
      return;
   }

   VHPI_CHECK(vhpi_get_value(h_b2, &value));
   fail_unless(value.value.enumv == vhpi1);

   VHPI_CHECK(vhpi_disable_cb(h_check_cb));
   vhpi_release_handle(h_entity1_a);
   vhpi_release_handle(h_entity2_a);
   vhpi_release_handle(h_b1);
   vhpi_release_handle(h_b2);
   vhpi_release_handle(h_check_cb);
}

static void start_of_sim(const vhpiCbDataT *cb_data)
{
   vhpiHandleT root = VHPI_CHECK(vhpi_handle(vhpiRootInst, NULL));

   h_entity1_a = VHPI_CHECK(vhpi_handle_by_name("entity_1.a", root));
   h_entity2_a = VHPI_CHECK(vhpi_handle_by_name("entity_2.a", root));
   h_b1 = VHPI_CHECK(vhpi_handle_by_name("b1", root));
   h_b2 = VHPI_CHECK(vhpi_handle_by_name("b2", root));

   vhpiValueT value = {
      .format = vhpiLogicVal,
   };
   value.value.enumv = vhpi1;

   VHPI_CHECK(vhpi_put_value(h_entity1_a, &value, vhpiDepositPropagate));
   VHPI_CHECK(vhpi_put_value(h_entity2_a, &value, vhpiDepositPropagate));

   vhpiCbDataT cb = {
      .reason = vhpiCbRepEndOfTimeStep,
      .cb_rtn = check_outputs,
   };
   h_check_cb = VHPI_CHECK(vhpi_register_cb(&cb, vhpiReturnCb));

   vhpi_release_handle(root);
}

void issue1505_startup(void)
{
   vhpiCbDataT cb_data = {
      .reason = vhpiCbStartOfSimulation,
      .cb_rtn = start_of_sim,
   };
   VHPI_CHECK(vhpi_register_cb(&cb_data, 0));
}
