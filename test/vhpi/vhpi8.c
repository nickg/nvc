#include "vhpi_test.h"

#include <float.h>
#include <math.h>
#include <stdio.h>
#include <string.h>

static vhpiHandleT lv, sv, ev, bv, cv, rv, lv2, sv2, ev2, cv2, rv2;

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

   vhpiValueT sval2 = {
      .format = vhpiSmallEnumVal,
   };

   vhpi_get_value(sv2, &sval2);
   check_error();
   fail_unless(sval2.value.smallenumv == 1);

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

   vhpiValueT lval2 = {
      .format = vhpiEnumVal,
   };

   vhpi_get_value(ev2, &lval2);
   check_error();
   fail_unless(lval2.value.enumv == 297);

   lval.format = vhpiLogicVecVal;
   vhpi_get_value(lv, &lval);
   check_error();
   fail_unless(l[0] == 8);
   fail_unless(l[1] == 7);
   fail_unless(l[2] == 6);
   fail_unless(l[3] == 5);

   lval2.format = vhpiLogicVal,
   vhpi_get_value(lv2, &lval2);
   check_error();
   fail_unless(lval2.value.enumv == 6);

   vhpiCharT c[5];
   vhpiValueT cval = {
      .format = vhpiStrVal,
      .bufSize = sizeof(c),
      .value.str = c,
   };

   vhpi_get_value(cv, &cval);
   check_error();
   fail_unless(c[0] == 0);
   fail_unless(c[1] == 1);
   fail_unless(c[2] == 254);
   fail_unless(c[3] == 255);
   fail_unless(c[4] == 0);

   vhpiValueT cval2 = {
      .format = vhpiCharVal,
   };

   vhpi_get_value(cv2, &cval2);
   check_error();
   fail_unless(cval2.value.ch == 254);

   vhpiRealT r[4];
   vhpiValueT rval = {
      .format = vhpiRealVecVal,
      .bufSize = sizeof(r),
      .value.reals = r,
   };

   vhpi_get_value(rv, &rval);
   check_error();
   fail_unless(isnan(r[0]));
   fail_unless(r[1] == DBL_TRUE_MIN);
   fail_unless(r[2] == DBL_MAX);
   fail_unless(r[3] == HUGE_VAL);

   vhpiValueT rval2 = {
      .format = vhpiRealVal,
   };

   vhpi_get_value(rv2, &rval2);
   check_error();
   fail_unless(rval2.value.real == DBL_MAX);
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

   vhpiValueT sval2 = {
      .format = vhpiSmallEnumVal,
   };

   vhpi_get_value(sv2, &sval2);
   check_error();
   fail_unless(sval2.value.smallenumv == 2);

   s[0] = 3;
   s[1] = 2;
   s[2] = 1;
   s[3] = 0;
   vhpi_put_value(sv, &sval, vhpiDepositPropagate);
   check_error();

   vhpiEnumT l[5];
   vhpiValueT lval = {
      .format     = vhpiEnumVecVal,
      .bufSize    = sizeof(vhpiEnumT) * 4,
      .value.enumvs = l,
   };

   vhpi_get_value(ev, &lval);
   check_error();
   fail_unless(l[0] == 0);
   fail_unless(l[1] == 1);
   fail_unless(l[2] == 2);
   fail_unless(l[3] == 3);

   vhpiValueT lval2 = {
      .format = vhpiEnumVal,
   };

   vhpi_get_value(ev2, &lval2);
   check_error();
   fail_unless(lval2.value.enumv == 2);

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

   lval.format = vhpiLogicVecVal;
   vhpi_get_value(bv, &lval);
   check_error();
   fail_unless(lval.numElems == 0);

   fail_unless(vhpi_put_value(bv, &lval, vhpiDepositPropagate));

   lval.bufSize = 0;
   vhpi_put_value(bv, &lval, vhpiDepositPropagate);
   check_error();
   lval.bufSize = sizeof(vhpiEnumT) * 4;

   lval2.format = vhpiLogicVal,
   vhpi_get_value(lv2, &lval2);
   check_error();
   fail_unless(lval2.value.enumv == 2);

   l[0] = 8;
   l[1] = 7;
   l[2] = 6;
   l[3] = 5;
   vhpi_put_value(lv, &lval, vhpiDepositPropagate);
   check_error();

   lval.bufSize = sizeof(l);
   fail_unless(vhpi_put_value(lv, &lval, vhpiDepositPropagate));

   vhpiCharT c[5];
   vhpiValueT cval = {
      .format     = vhpiStrVal,
      .bufSize    = sizeof(c),
      .value.str  = c,
   };

   vhpi_get_value(cv, &cval);
   check_error();
   fail_unless(c[0] == 'N');
   fail_unless(c[1] == 'V');
   fail_unless(c[2] == 'C');
   fail_unless(c[3] == '!');
   fail_unless(c[4] == '\0');

   vhpiValueT cval2 = {
      .format = vhpiCharVal,
   };

   vhpi_get_value(cv2, &cval2);
   check_error();
   fail_unless(cval2.value.ch == 'C');

   c[0] = 0;
   c[1] = 1;
   c[2] = 254;
   c[3] = 255;
   vhpi_put_value(cv, &cval, vhpiDepositPropagate);
   check_error();

   vhpiRealT r[4];
   vhpiValueT rval = {
      .format      = vhpiRealVecVal,
      .bufSize     = sizeof(r),
      .value.reals = r,
   };

   vhpi_get_value(rv, &rval);
   check_error();
   fail_unless(r[0] == 0.0);
   fail_unless(r[1] == 0.5);
   fail_unless(r[2] == 1.0);
   fail_unless(r[3] == -1.0);

   vhpiValueT rval2 = {
      .format = vhpiRealVal,
   };

   vhpi_get_value(rv2, &rval2);
   check_error();
   fail_unless(rval2.value.real == 1.0);

   r[0] = NAN;
   r[1] = DBL_TRUE_MIN;
   r[2] = DBL_MAX;
   r[3] = HUGE_VAL;
   vhpi_put_value(rv, &rval, vhpiDepositPropagate);
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

   lv2 = vhpi_handle_by_index(vhpiIndexedNames, lv, 2);
   check_error();
   fail_if(lv2 == NULL);

   sv = vhpi_handle_by_name("sv", root);
   check_error();
   fail_if(sv == NULL);

   sv2 = vhpi_handle_by_index(vhpiIndexedNames, sv, 2);
   check_error();
   fail_if(sv2 == NULL);

   ev = vhpi_handle_by_name("ev", root);
   check_error();
   fail_if(ev == NULL);

   ev2 = vhpi_handle_by_index(vhpiIndexedNames, ev, 2);
   check_error();
   fail_if(ev2 == NULL);

   bv = vhpi_handle_by_name("bv", root);
   check_error();
   fail_if(bv == NULL);

   fail_unless(vhpi_get(vhpiSizeP, bv) == 0);
   vhpiHandleT bv_names = vhpi_iterator(vhpiIndexedNames, bv);
   check_error();
   fail_if(bv_names == NULL);
   fail_unless(vhpi_scan(bv_names) == NULL);

   cv = vhpi_handle_by_name("cv", root);
   check_error();
   fail_if(cv == NULL);

   cv2 = vhpi_handle_by_index(vhpiIndexedNames, cv, 2);
   check_error();
   fail_if(cv2 == NULL);

   rv = vhpi_handle_by_name("rv", root);
   check_error();
   fail_if(rv == NULL);

   rv2 = vhpi_handle_by_index(vhpiIndexedNames, rv, 2);
   check_error();
   fail_if(rv2 == NULL);

   vhpi_release_handle(root);
}
