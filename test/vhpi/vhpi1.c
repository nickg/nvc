#include "vhpi_user.h"

#include <stdio.h>

static void start_of_sim(const vhpiCbDataT *cb_data)
{
   vhpi_printf("start of sim callback! user data is '%s'",
               (char *)cb_data->user_data);
}

static void startup()
{
   vhpi_printf("hello, world!");

   vhpiCbDataT cb_data_s = {
      .reason    = vhpiCbStartOfSimulation,
      .cb_rtn    = start_of_sim,
      .user_data = (char *)"some user data",
   };
   vhpi_register_cb(&cb_data_s, vhpiReturnCb);
}

void (*vhpi_startup_routines[])() = {
   startup,
   NULL
};
