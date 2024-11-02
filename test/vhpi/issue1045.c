#include "vhpi_test.h"

#include <stdio.h>
#include <assert.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>

static vhpiHandleT s_cb, i_cb;

static void s_value_change(const vhpiCbDataT *cb_data)
{
   static int state = 0;

   vhpiHandleT handle_i = VHPI_CHECK(vhpi_handle_by_name("i", cb_data->obj));
   vhpi_printf("i handle %p", handle_i);

   vhpiValueT buf = {
      .format = vhpiIntVal,
   };
   VHPI_CHECK(vhpi_get_value(handle_i, &buf));

   vhpi_release_handle(handle_i);

   vhpi_printf("s value change --> %d", buf.value.intg);

   switch (state++) {
   case 0:
      fail_unless(buf.value.intg == 5);
      break;
   case 1:
      fail_unless(buf.value.intg == 5);
      break;
   case 2:
      fail_unless(buf.value.intg == 6);
      vhpi_disable_cb(s_cb);
      break;
   default:
      vhpi_assert(vhpiFailure, "unexpected value change on signal S");
   }
}

static void i_value_change(const vhpiCbDataT *cb_data)
{
   static int state = 0;

   vhpiValueT buf = {
      .format = vhpiIntVal,
   };
   VHPI_CHECK(vhpi_get_value(cb_data->obj, &buf));

   vhpi_printf("i value change --> %d", buf.value.intg);

   switch (state++) {
   case 0:
      fail_unless(buf.value.intg == 5);
      break;
   case 1:
      fail_unless(buf.value.intg == 6);
      vhpi_disable_cb(i_cb);
      break;
   default:
      vhpi_assert(vhpiFailure, "unexpected value change on signal I");
   }
}

static void start_of_sim(const vhpiCbDataT *cb_data)
{
   vhpiHandleT handle_s = VHPI_CHECK(vhpi_handle_by_name("issue1045.s", NULL));
   vhpi_printf("s handle %p", handle_s);

   vhpiHandleT handle_i = VHPI_CHECK(vhpi_handle_by_name("i", handle_s));
   vhpi_printf("i handle %p", handle_i);

   vhpiCbDataT s_change_cb = {
      .reason = vhpiCbValueChange,
      .cb_rtn = s_value_change,
      .obj    = handle_s,
   };
   s_cb = VHPI_CHECK(vhpi_register_cb(&s_change_cb, vhpiReturnCb));

   vhpiCbDataT i_change_cb = {
      .reason = vhpiCbValueChange,
      .cb_rtn = i_value_change,
      .obj    = handle_i,
   };
   i_cb = VHPI_CHECK(vhpi_register_cb(&i_change_cb, vhpiReturnCb));
}

void issue1045_startup(void)
{
   vhpiCbDataT cb_data = {
      .reason = vhpiCbStartOfSimulation,
      .cb_rtn = start_of_sim,
   };
   vhpi_register_cb(&cb_data, 0);
   check_error();
}
