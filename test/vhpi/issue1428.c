#include "vhpi_test.h"

#include <stdio.h>
#include <assert.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>

static void s1_value_change(const vhpiCbDataT *cb_data)
{
   static int state = 0;

   vhpiValueT buf = {
      .format = vhpiEnumVal,
   };
   VHPI_CHECK(vhpi_get_value(cb_data->obj, &buf));

   vhpi_printf("s(1) value change --> %d", buf.value.enumv);

   vhpiTimeT now;
   long cycles;
   vhpi_get_time(&now, &cycles);

   switch (state++) {
   case 0:
      fail_unless(buf.value.enumv == 1);
      fail_unless(now.low == 2);
      fail_unless(now.high == 0);
      fail_unless(cycles == 1);
      break;
   default:
      vhpi_assert(vhpiFailure, "invalid s(1) state %d", state);
      break;
   }
}

static void r_x_value_change(const vhpiCbDataT *cb_data)
{
   static int state = 0;

   vhpiValueT buf = {
      .format = vhpiIntVal,
   };
   VHPI_CHECK(vhpi_get_value(cb_data->obj, &buf));

   vhpi_printf("r.x value change --> %d", buf.value.intg);

   vhpiTimeT now;
   long cycles;
   vhpi_get_time(&now, &cycles);

   switch (state++) {
   case 0:
      fail_unless(buf.value.intg == 5);
      fail_unless(now.low == 0);
      fail_unless(now.high == 0);
      fail_unless(cycles == 1);
      break;
   default:
      vhpi_assert(vhpiFailure, "invalid r.x state %d", state);
      break;
   }
}

static void a1_value_change(const vhpiCbDataT *cb_data)
{
   static int state = 0;

   vhpiHandleT x = VHPI_CHECK(vhpi_handle_by_name("x", cb_data->obj));
   vhpi_printf("a(1).x handle %p", x);

   vhpiValueT buf = {
      .format = vhpiIntVal,
   };
   VHPI_CHECK(vhpi_get_value(x, &buf));

   vhpi_printf("a(1).x value change --> %d", buf.value.intg);

   vhpiTimeT now;
   long cycles;
   vhpi_get_time(&now, &cycles);

   switch (state++) {
   case 0:
      fail_unless(buf.value.intg == 7);
      fail_unless(now.low == 0);
      fail_unless(now.high == 0);
      fail_unless(cycles == 1);
      break;
   default:
      vhpi_assert(vhpiFailure, "invalid a(1) state %d", state);
      break;
   }

   vhpi_release_handle(x);
}

static void start_of_sim(const vhpiCbDataT *cb_data)
{
   vhpiHandleT root = VHPI_CHECK(vhpi_handle(vhpiRootInst, NULL));
   vhpi_printf("root handle %p", root);

   vhpiHandleT s = VHPI_CHECK(vhpi_handle_by_name("s", root));
   vhpi_printf("s handle %p", s);

   vhpiHandleT s1 = VHPI_CHECK(vhpi_handle_by_index(vhpiIndexedNames, s, 1));
   vhpi_printf("s1 handle %p", s1);

   vhpiCbDataT s1_change_cb = {
      .reason = vhpiCbValueChange,
      .cb_rtn = s1_value_change,
      .obj    = s1,
   };
   VHPI_CHECK(vhpi_register_cb(&s1_change_cb, 0));

   vhpi_release_handle(s);
   vhpi_release_handle(s1);

   vhpiHandleT r = VHPI_CHECK(vhpi_handle_by_name("r", root));
   vhpi_printf("r handle %p", s);

   vhpiHandleT r_x = VHPI_CHECK(vhpi_handle_by_name("x", r));
   vhpi_printf("r.x handle %p", s1);

   vhpiCbDataT r_x_change_cb = {
      .reason = vhpiCbValueChange,
      .cb_rtn = r_x_value_change,
      .obj    = r_x,
   };
   VHPI_CHECK(vhpi_register_cb(&r_x_change_cb, 0));

   vhpi_release_handle(r);
   vhpi_release_handle(r_x);

   vhpiHandleT a = VHPI_CHECK(vhpi_handle_by_name("a", root));
   vhpi_printf("a handle %p", a);

   vhpiHandleT a1 = VHPI_CHECK(vhpi_handle_by_index(vhpiIndexedNames, a, 0));
   vhpi_printf("a1 handle %p", a1);

   vhpiCbDataT a1_change_cb = {
      .reason = vhpiCbValueChange,
      .cb_rtn = a1_value_change,
      .obj    = a1,
   };
   VHPI_CHECK(vhpi_register_cb(&a1_change_cb, 0));

   vhpi_release_handle(a);
   vhpi_release_handle(a1);

   vhpi_release_handle(root);
}

void issue1428_startup(void)
{
   vhpiCbDataT cb_data = {
      .reason = vhpiCbStartOfSimulation,
      .cb_rtn = start_of_sim,
   };
   VHPI_CHECK(vhpi_register_cb(&cb_data, 0));
}
