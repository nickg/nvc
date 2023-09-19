#include "vhpi_test.h"

#include <stdio.h>
#include <assert.h>
#include <stdlib.h>
#include <string.h>

static vhpiHandleT handle_x;
static vhpiHandleT handle_y;
static vhpiHandleT handle_sos;
static vhpiHandleT end_of_timestep_cb;
static int         sequence = 0;

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
   check_error();
}

static void end_of_timestep(const vhpiCbDataT *cb_data)
{
   vhpi_printf("end_of_timestep");

   fail_unless(sequence++ == 2);

   vhpiValueT value = {
      .format = vhpiIntVal,
      .value.intg = 0
   };
   fail_unless(vhpi_put_value(handle_x, &value, vhpiForcePropagate));

   vhpi_remove_cb(end_of_timestep_cb);
   check_error();
}

static void start_of_sim(const vhpiCbDataT *cb_data)
{
   vhpi_printf("start_of_sim");

   vhpiHandleT root = vhpi_handle(vhpiRootInst, NULL);
   check_error();
   fail_if(root == NULL);
   vhpi_printf("root handle %p", root);

   fail_unless(vhpi_get(vhpiKindP, root) == vhpiRootInstK);

   handle_x = vhpi_handle_by_name("x", root);
   check_error();
   fail_if(handle_x == NULL);
   vhpi_printf("x handle %p", handle_x);

   handle_y = vhpi_handle_by_name("y", root);
   check_error();
   fail_if(handle_y == NULL);
   vhpi_printf("y handle %p", handle_y);

   vhpiHandleT y_type = vhpi_handle(vhpiBaseType, handle_y);
   check_error();
   fail_if(vhpi_get(vhpiIsCompositeP, y_type));
   check_error();
   fail_unless(vhpi_get(vhpiIsScalarP, y_type));
   check_error();
   vhpi_release_handle(y_type);

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

   vhpiCbDataT cb_data3 = {
      .reason = vhpiCbRepEndOfTimeStep,
      .cb_rtn = end_of_timestep,
   };
   end_of_timestep_cb = vhpi_register_cb(&cb_data3, vhpiReturnCb);
   check_error();

   vhpi_release_handle(root);
}

void vhpi2_startup(void)
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
}
