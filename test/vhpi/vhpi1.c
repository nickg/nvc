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

static void check_error(void)
{
   vhpiErrorInfoT info;
   if (vhpi_check_error(&info))
      vhpi_assert(vhpiFailure, "unexpected error '%s'", info.message);
}

static void test_bin_str(void)
{
   vhpiHandleT root = vhpi_handle(vhpiRootInst, NULL);
   check_error();

   vhpiHandleT hb = vhpi_handle_by_name("b", root);
   check_error();

   vhpiHandleT hv = vhpi_handle_by_name("v", root);
   check_error();

   char b_str[2] = { 0xff, 0xff };
   vhpiValueT b_value = {
      .format    = vhpiBinStrVal,
      .bufSize   = sizeof(b_str),
      .value.str = (vhpiCharT *)b_str
   };
   vhpi_get_value(hb, &b_value);
   check_error();

   vhpi_printf("b bit string '%s' %x", b_str);
   fail_unless(strcmp(b_str, "0") == 0);

   vhpiValueT v_value = {
      .format    = vhpiBinStrVal,
      .bufSize   = 0,
      .value.str = NULL
   };
   const int need = vhpi_get_value(hv, &v_value);
   check_error();

   vhpi_printf("need %d bytes for v string", need);

   v_value.value.str = malloc(need);
   v_value.bufSize   = need;
   fail_if(v_value.value.str == NULL);
   fail_unless(vhpi_get_value(hv, &v_value) == 0);
   check_error();

   vhpi_printf("v bit string '%s'", v_value.value.str);
   fail_unless(strcmp((char *)v_value.value.str, "0011") == 0);
   free(v_value.value.str);

   vhpi_release_handle(root);
   vhpi_release_handle(hb);
   vhpi_release_handle(hv);
}

static void y_value_change(const vhpiCbDataT *cb_data)
{
   vhpiValueT value = {
      .format = vhpiObjTypeVal
   };
   vhpi_get_value(handle_y, &value);
   check_error();
   fail_unless(value.format == vhpiIntVal);

   vhpi_printf("y value changed to %d", value.value.intg);

   if (value.value.intg == 75) {
      test_bin_str();

      vhpi_control(vhpiFinish);
      check_error();
   }
   else {
      value.value.intg++;
      vhpi_put_value(handle_x, &value, vhpiForcePropagate);
      check_error();
   }
}

static void after_5ns(const vhpiCbDataT *cb_data)
{
   vhpi_printf("after_5ns callback!");

   long cycles;
   vhpiTimeT now;
   vhpi_get_time(&now, &cycles);

   fail_unless(now.low == 5000000);
   fail_unless(now.high == 0);
   fail_unless(cycles == 0);

   vhpiValueT value = {
      .format = vhpiObjTypeVal
   };
   vhpi_get_value(handle_y, &value);
   check_error();
   fail_unless(value.format == vhpiIntVal);
   fail_unless(value.value.intg == 6);

   value.value.intg = 70;
   vhpi_put_value(handle_x, &value, vhpiForcePropagate);
   check_error();

   vhpiCbDataT cb_data2 = {
      .reason = vhpiCbValueChange,
      .cb_rtn = y_value_change,
      .obj    = handle_y
   };
   vhpi_register_cb(&cb_data2, 0);
   check_error();
}

static void start_of_sim(const vhpiCbDataT *cb_data)
{
   vhpi_printf("start of sim callback! user data is '%s'",
               (char *)cb_data->user_data);

   long cycles;
   vhpiTimeT now;
   vhpi_get_time(&now, &cycles);

   fail_unless(now.low == 0);
   fail_unless(now.high == 0);
   fail_unless(cycles == 0);

   vhpiValueT value = {
      .format = vhpiObjTypeVal
   };
   vhpi_get_value(handle_x, &value);
   check_error();
   fail_unless(value.format == vhpiIntVal);
   fail_unless(value.value.intg == 0);

   value.value.intg = 5;
   vhpi_put_value(handle_x, &value, vhpiForcePropagate);
   check_error();

   vhpiTimeT time_5ns = {
      .low = 5000000
   };

   vhpiCbDataT cb_data2 = {
      .reason = vhpiCbAfterDelay,
      .cb_rtn = after_5ns,
      .time   = &time_5ns
   };
   vhpi_register_cb(&cb_data2, 0);
   check_error();
}

static void end_of_sim(const vhpiCbDataT *cb_data)
{
   vhpi_printf("end of sim callback");

   vhpiValueT value = {
      .format = vhpiObjTypeVal
   };
   vhpi_get_value(handle_y, &value);
   check_error();
   fail_unless(value.format == vhpiIntVal);
   fail_unless(value.value.intg == 75);

   vhpi_release_handle(handle_x);
   vhpi_release_handle(handle_y);
   vhpi_release_handle(handle_sos);
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

   vhpiCbDataT cb_data2 = {
      .reason    = vhpiCbEndOfSimulation,
      .cb_rtn    = end_of_sim
   };
   vhpi_register_cb(&cb_data2, 0);
   check_error();

   vhpi_printf("tool is %s", vhpi_get_str(vhpiNameP, NULL));

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
