#include "vhpi_test.h"

#include <stdio.h>
#include <assert.h>
#include <stdlib.h>
#include <string.h>

static vhpiHandleT handle_x;
static vhpiHandleT handle_y;
static vhpiHandleT end_of_timestep_cb;
static int         sequence = 0;
static int         start_of_processes_count = 0;
static int         rep_start_count = 0;
static int         rep_end_count = 0;

static void start_of_processes(const vhpiCbDataT *cb_data)
{
   vhpi_printf("start_of_processes");

   fail_unless(sequence == 0);
   fail_unless(start_of_processes_count++ == 0);
}

// In every cycle, the start of the process phase, then its end
static void rep_start_of_processes(const vhpiCbDataT *cb_data)
{
   fail_unless(rep_start_count == rep_end_count);
   rep_start_count++;
}

static void rep_end_of_processes(const vhpiCbDataT *cb_data)
{
   fail_unless(rep_end_count + 1 == rep_start_count);
   rep_end_count++;
}

static void end_of_simulation(const vhpiCbDataT *cb_data)
{
   fail_unless(start_of_processes_count == 1);
   fail_unless(rep_start_count > 1);
   fail_unless(rep_start_count == rep_end_count);
}

static void end_of_processes(const vhpiCbDataT *cb_data)
{
   vhpi_printf("end_of_processes");

   // The process phase of this cycle started after the callbacks were
   // registered
   fail_unless(start_of_processes_count == 1);
   fail_unless(rep_start_count == 1);

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
   vhpi_release_handle(end_of_timestep_cb);
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

   vhpiHandleT y_base = vhpi_handle(vhpiBaseType, y_type);
   fail_unless(vhpi_compare_handles(y_base, y_type));

   vhpi_release_handle(y_type);
   vhpi_release_handle(y_base);

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

   vhpiCbDataT cb_data0 = {
      .reason = vhpiCbStartOfProcesses,
      .cb_rtn = start_of_processes,
   };
   vhpi_register_cb(&cb_data0, 0);
   check_error();

   vhpiCbDataT cb_data0r = {
      .reason = vhpiCbRepStartOfProcesses,
      .cb_rtn = rep_start_of_processes,
   };
   vhpi_register_cb(&cb_data0r, 0);
   check_error();

   vhpiCbDataT cb_data0e = {
      .reason = vhpiCbRepEndOfProcesses,
      .cb_rtn = rep_end_of_processes,
   };
   vhpi_register_cb(&cb_data0e, 0);
   check_error();

   vhpiCbDataT cb_data0s = {
      .reason = vhpiCbEndOfSimulation,
      .cb_rtn = end_of_simulation,
   };
   vhpi_register_cb(&cb_data0s, 0);
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
   vhpiHandleT handle_sos = vhpi_register_cb(&cb_data1, vhpiReturnCb);
   check_error();
   fail_unless(vhpi_get(vhpiStateP, handle_sos) == vhpiEnable);
   vhpi_release_handle(handle_sos);
}
