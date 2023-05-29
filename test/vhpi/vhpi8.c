#include "vhpi_test.h"

#include <stdio.h>
#include <string.h>

static vhpiHandleT lv, sv, ev;

static void last_delta(const vhpiCbDataT *cb_data)
{
   vhpiSmallEnumT s[4];
   vhpiValueT sval = {
      .format = vhpiSmallEnumVecVal,
      .bufSize = sizeof(s),
      .value.smallenumvs = s,
   };

   vhpi_get_value(sv, &sval);
   check_error();
   fail_unless(s[0] == 3);
   fail_unless(s[1] == 2);
   fail_unless(s[2] == 1);
   fail_unless(s[3] == 0);

   vhpiEnumT l[4];
   vhpiValueT lval = {
      .format = vhpiEnumVecVal,
      .bufSize = sizeof(l),
      .value.enumvs = l,
   };

   vhpi_get_value(ev, &lval);
   check_error();
   fail_unless(l[0] == 299);
   fail_unless(l[1] == 298);
   fail_unless(l[2] == 297);
   fail_unless(l[3] == 296);

   lval.format = vhpiLogicVecVal;
   vhpi_get_value(lv, &lval);
   check_error();
   fail_unless(l[0] == 8);
   fail_unless(l[1] == 7);
   fail_unless(l[2] == 6);
   fail_unless(l[3] == 5);
}

static void start_of_sim(const vhpiCbDataT *cb_data)
{
   vhpiSmallEnumT s[4];
   vhpiValueT sval = {
      .format     = vhpiSmallEnumVecVal,
      .bufSize    = sizeof(s),
      .value.smallenumvs = s,
   };

   vhpi_get_value(sv, &sval);
   check_error();
   fail_unless(s[0] == 0);
   fail_unless(s[1] == 1);
   fail_unless(s[2] == 2);
   fail_unless(s[3] == 3);

   s[0] = 3;
   s[1] = 2;
   s[2] = 1;
   s[3] = 0;
   vhpi_put_value(sv, &sval, vhpiDepositPropagate);
   check_error();

   vhpiEnumT l[4];
   vhpiValueT lval = {
      .format     = vhpiEnumVecVal,
      .bufSize    = sizeof(l),
      .value.enumvs = l,
   };

   vhpi_get_value(ev, &lval);
   check_error();
   fail_unless(l[0] == 0);
   fail_unless(l[1] == 1);
   fail_unless(l[2] == 2);
   fail_unless(l[3] == 3);

   l[0] = 299;
   l[1] = 298;
   l[2] = 297;
   l[3] = 296;
   vhpi_put_value(ev, &lval, vhpiDepositPropagate);
   check_error();

   lval.format = vhpiLogicVecVal;
   vhpi_get_value(lv, &lval);
   check_error();
   fail_unless(l[0] == 0);
   fail_unless(l[1] == 1);
   fail_unless(l[2] == 2);
   fail_unless(l[3] == 3);

   l[0] = 8;
   l[1] = 7;
   l[2] = 6;
   l[3] = 5;
   vhpi_put_value(lv, &lval, vhpiDepositPropagate);
   check_error();
}

void vhpi8_startup(void)
{
   vhpiCbDataT cb_data = {
      .reason    = vhpiCbStartOfSimulation,
      .cb_rtn    = start_of_sim,
   };
   vhpi_register_cb(&cb_data, 0);
   check_error();

   cb_data.reason = vhpiCbLastKnownDeltaCycle;
   cb_data.cb_rtn = last_delta;
   vhpi_register_cb(&cb_data, 0);
   check_error();

   vhpiHandleT root = vhpi_handle(vhpiRootInst, NULL);
   check_error();
   fail_if(root == NULL);

   lv = vhpi_handle_by_name("lv", root);
   check_error();
   fail_if(lv == NULL);

   sv = vhpi_handle_by_name("sv", root);
   check_error();
   fail_if(sv == NULL);

   ev = vhpi_handle_by_name("ev", root);
   check_error();
   fail_if(ev == NULL);

   vhpi_release_handle(root);
}
