#include "vhpi_user.h"

#include <stdio.h>
#include <assert.h>

static void start_of_sim(const vhpiCbDataT *cb_data)
{
   vhpi_printf("start of sim callback! user data is '%s'",
               (char *)cb_data->user_data);

   long cycles;
   vhpiTimeT now;
   vhpi_get_time(&now, &cycles);

   assert((now.low == 0) && (now.high == 0) && (cycles == 0));
}

static void startup()
{
   vhpi_printf("hello, world!");

   vhpiCbDataT cb_data_s = {
      .reason    = vhpiCbStartOfSimulation,
      .cb_rtn    = start_of_sim,
      .user_data = (char *)"some user data",
   };
   vhpiHandleT h = vhpi_register_cb(&cb_data_s, vhpiReturnCb);

   assert(vhpi_get(vhpiStateP, h) == vhpiEnable);

   vhpi_printf("tool is %s", vhpi_get_str(vhpiNameP, NULL));

   vhpiHandleT root = vhpi_handle(vhpiRootInst, NULL);
   vhpi_printf("root handle %p", root);
}

void (*vhpi_startup_routines[])() = {
   startup,
   NULL
};
