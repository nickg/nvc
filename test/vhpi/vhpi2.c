#include "vhpi_user.h"

#include <stdio.h>
#include <assert.h>
#include <stdlib.h>
#include <string.h>

#define fail_if(x)                                                      \
   if (x) vhpi_assert(vhpiFailure, "assertion '%s' failed at %s:%d",    \
                      #x, __FILE__, __LINE__)
#define fail_unless(x) fail_if(!(x))

static vhpiHandleT handle_x;
static vhpiHandleT handle_y;
static vhpiHandleT handle_sos;
static int         sequence = 0;

static void check_error(void)
{
   vhpiErrorInfoT info;
   if (vhpi_check_error(&info))
      vhpi_assert(vhpiFailure, "unexpected error '%s'", info.message);
}

static void end_of_processes(const vhpiCbDataT *cb_data)
{
   vhpi_printf("end_of_processes");

   fail_unless(sequence++ == 0);
}

static void last_known_delta_cycle(const vhpiCbDataT *cb_data)
{
   vhpi_printf("last_known_delta_cycle");

   fail_unless(sequence++ == 1);

   vhpiValueT value = {
      .format = vhpiObjTypeVal
   };
   vhpi_get_value(handle_x, &value);
   check_error();
   fail_unless(value.format == vhpiIntVal);
   fail_unless(value.value.intg == 1);

   value.value.intg = 1;
   vhpi_put_value(handle_x, &value, vhpiForcePropagate);

   vhpiErrorInfoT info;
   fail_unless(vhpi_check_error(&info));
}

static void start_of_sim(const vhpiCbDataT *cb_data)
{
   vhpi_printf("start_of_sim");

   vhpiValueT value = {
      .format = vhpiObjTypeVal
   };
   vhpi_get_value(handle_x, &value);
   check_error();
   fail_unless(value.format == vhpiIntVal);
   fail_unless(value.value.intg == 0);

   value.value.intg = 1;
   vhpi_put_value(handle_x, &value, vhpiForcePropagate);
   check_error();

   vhpiCbDataT cb_data1 = {
      .reason = vhpiCbEndOfProcesses,
      .cb_rtn = end_of_processes,
   };
   vhpi_register_cb(&cb_data1, 0);
   check_error();

   vhpiCbDataT cb_data2 = {
      .reason = vhpiCbLastKnownDeltaCycle,
      .cb_rtn = last_known_delta_cycle,
   };
   vhpi_register_cb(&cb_data2, 0);
   check_error();
}

static void startup()
{
   vhpi_printf("hello, world!");

   vhpiCbDataT cb_data1 = {
      .reason    = vhpiCbStartOfSimulation,
      .cb_rtn    = start_of_sim,
      .user_data = (char *)"some user data",
   };
   handle_sos = vhpi_register_cb(&cb_data1, vhpiReturnCb);
   check_error();
   fail_unless(vhpi_get(vhpiStateP, handle_sos) == vhpiEnable);

   vhpiHandleT root = vhpi_handle(vhpiRootInst, NULL);
   check_error();
   fail_if(root == NULL);
   vhpi_printf("root handle %p", root);

   handle_x = vhpi_handle_by_name("x", root);
   check_error();
   fail_if(handle_x == NULL);
   vhpi_printf("x handle %p", handle_x);

   handle_y = vhpi_handle_by_name("y", root);
   check_error();
   fail_if(handle_y == NULL);
   vhpi_printf("y handle %p", handle_y);

   vhpi_release_handle(root);
}

void (*vhpi_startup_routines[])() = {
   startup,
   NULL
};
