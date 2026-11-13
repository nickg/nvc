#include "vhpi_test.h"

static int starts, ends;

static void start_of_sim(const vhpiCbDataT *cb_data)
{
   starts++;
}

static void end_of_sim(const vhpiCbDataT *cb_data)
{
   vhpiTimeT now;
   vhpi_get_time(&now, NULL);

   ends++;
   vhpi_printf("end of simulation at %d fs after %d start(s), %d end(s)",
               now.low, starts, ends);
}

void vhpi22_startup(void)
{
   vhpiCbDataT start_data = {
      .reason = vhpiCbStartOfSimulation,
      .cb_rtn = start_of_sim,
   };
   VHPI_CHECK(vhpi_register_cb(&start_data, 0));

   vhpiCbDataT end_data = {
      .reason = vhpiCbEndOfSimulation,
      .cb_rtn = end_of_sim,
   };
   VHPI_CHECK(vhpi_register_cb(&end_data, 0));
}
