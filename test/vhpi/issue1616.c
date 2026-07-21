#include "vhpi_test.h"

static vhpiHandleT callback;
static unsigned count;

static void end_of_simulation(const vhpiCbDataT *cb_data)
{
   fail_unless(count == 3);
}

static void after_delay(const vhpiCbDataT *cb_data)
{
   vhpiTimeT now;
   vhpi_get_time(&now, NULL);
   fail_unless(now.high == 0);
   fail_unless(now.low == ++count * 1000000);

   if (count == 3) {
      fail_if(vhpi_remove_cb(callback));
      VHPI_CHECK(vhpi_control(vhpiFinish));
   }
}

static void start_of_simulation(const vhpiCbDataT *cb_data)
{
   vhpiTimeT delay = { .low = 1000000 };
   vhpiCbDataT data = {
      .reason = vhpiCbRepAfterDelay,
      .cb_rtn = after_delay,
      .time   = &delay,
   };

   callback = VHPI_CHECK(vhpi_register_cb(&data, vhpiReturnCb));
}

void issue1616_startup(void)
{
   vhpiCbDataT start_data = {
      .reason = vhpiCbStartOfSimulation,
      .cb_rtn = start_of_simulation,
   };
   VHPI_CHECK(vhpi_register_cb(&start_data, 0));

   vhpiCbDataT end_data = {
      .reason = vhpiCbEndOfSimulation,
      .cb_rtn = end_of_simulation,
   };
   VHPI_CHECK(vhpi_register_cb(&end_data, 0));
}
