#include "vhpi_test.h"

#include <stdio.h>
#include <assert.h>
#include <stdlib.h>
#include <string.h>

static vhpiHandleT handle_x;
static vhpiHandleT handle_y;
static vhpiHandleT handle_sos;

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
   int need = vhpi_get_value(hv, &v_value);
   check_error();

   vhpi_printf("need %d bytes for v string", need);

   v_value.value.str = malloc(need);
   v_value.bufSize   = need;
   fail_if(v_value.value.str == NULL);
   fail_unless(vhpi_get_value(hv, &v_value) == 0);
   check_error();

   vhpi_printf("v bit string '%s'", v_value.value.str);
   fail_unless(strcmp((char *)v_value.value.str, "0011") == 0);
   fail_unless(v_value.numElems == 4);
   free(v_value.value.str);

   v_value.bufSize = 0;
   v_value.value.enumvs = NULL;
   v_value.format = vhpiLogicVecVal;

   need = vhpi_get_value(hv, &v_value);
   check_error();

   fail_unless(need = 4 * sizeof(vhpiEnumT));

   v_value.bufSize = 4 * sizeof(vhpiEnumT);
   v_value.value.enumvs = malloc(v_value.bufSize);
   fail_unless(vhpi_get_value(hv, &v_value) == 0);

   fail_unless(v_value.numElems == 4);

   fail_unless(v_value.value.enumvs[0] == 0);
   fail_unless(v_value.value.enumvs[1] == 0);
   fail_unless(v_value.value.enumvs[2] == 1);
   fail_unless(v_value.value.enumvs[3] == 1);

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

static void after_after_5ns(const vhpiCbDataT *cb_data)
{
   vhpi_printf("after_after_5ns callback!");
   fail_if(1);
}

static void next_timestep(const vhpiCbDataT *cb_data)
{
   vhpi_printf("next_timestep callback!");
   fail_if(1);
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
   vhpi_printf("value=%d", value.value.intg);
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

   vhpiTimeT time_1fs = {
      .low = 1
   };

   vhpiCbDataT cb_data3 = {
      .reason = vhpiCbAfterDelay,
      .cb_rtn = after_after_5ns,
      .time   = &time_1fs
   };
   vhpiHandleT cb3 = vhpi_register_cb(&cb_data3, vhpiReturnCb);
   check_error();
   fail_if(vhpi_disable_cb(cb3));

   vhpiHandleT cb4 = vhpi_register_cb(&cb_data3, vhpiReturnCb);
   check_error();
   fail_if(vhpi_remove_cb(cb4));

   vhpiCbDataT cb_data5 = {
      .reason = vhpiCbNextTimeStep,
      .cb_rtn = next_timestep,
   };
   vhpiHandleT cb5 = vhpi_register_cb(&cb_data5, vhpiReturnCb);
   check_error();
   fail_if(vhpi_remove_cb(cb5));
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

void vhpi1_startup(void)
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

   vhpiHandleT tool = vhpi_handle(vhpiTool, NULL);
   check_error();
   fail_if(tool == NULL);
   vhpi_printf("tool is %s", vhpi_get_str(vhpiNameP, tool));
   vhpi_printf("tool version is %s", vhpi_get_str(vhpiToolVersionP, tool));
   vhpi_release_handle(tool);

   vhpiHandleT root = vhpi_handle(vhpiRootInst, NULL);
   check_error();
   fail_if(root == NULL);
   vhpi_printf("root handle %p", root);

   const vhpiCharT *root_name = vhpi_get_str(vhpiNameP, root);
   vhpi_printf("root name is %s", root_name);
   vhpi_printf("root full name is %s", vhpi_get_str(vhpiFullNameP, root));

   vhpiHandleT root_ports = vhpi_iterator(vhpiPortDecls, root);
   fail_if(root_ports == NULL);
   for (vhpiHandleT port = vhpi_scan(root_ports); port != NULL; port = vhpi_scan(root_ports))
      vhpi_printf("root port is %s", vhpi_get_str(vhpiNameP, port));

   vhpiHandleT root_decls = vhpi_iterator(vhpiDecls, root);
   fail_if(root_decls == NULL);
   for (vhpiHandleT decl = vhpi_scan(root_decls); decl != NULL; decl = vhpi_scan(root_decls))
      vhpi_printf("root decl is %s", vhpi_get_str(vhpiNameP, decl));

   vhpiHandleT arch = vhpi_handle(vhpiDesignUnit, root);
   check_error();
   fail_if(arch == NULL);
   vhpi_printf("arch handle %p", arch);

   vhpi_printf("arch name is %s", vhpi_get_str(vhpiNameP, arch));
   vhpi_printf("arch unit name is %s", vhpi_get_str(vhpiUnitNameP, arch));
   vhpi_release_handle(arch);

   vhpiHandleT entity = vhpi_handle(vhpiPrimaryUnit, arch);
   check_error();
   fail_if(entity == NULL);
   vhpi_printf("entity handle %p", entity);

   vhpi_printf("entity name is %s", vhpi_get_str(vhpiNameP, entity));
   vhpi_printf("entity unit name is %s", vhpi_get_str(vhpiUnitNameP, entity));
   vhpi_release_handle(entity);

   handle_x = vhpi_handle_by_name("x", root);
   check_error();
   fail_if(handle_x == NULL);
   vhpi_printf("x handle %p", handle_x);
   vhpi_printf("x name %s", vhpi_get_str(vhpiNameP, handle_x));
   vhpi_printf("x full name is %s", vhpi_get_str(vhpiFullNameP, handle_x));

   handle_y = vhpi_handle_by_name("y", root);
   check_error();
   fail_if(handle_y == NULL);
   vhpi_printf("y handle %p", handle_y);
   vhpi_printf("y name %s", vhpi_get_str(vhpiNameP, handle_y));
   vhpi_printf("y full name is %s", vhpi_get_str(vhpiFullNameP, handle_y));

   vhpiHandleT handle_y2 = vhpi_handle_by_name(":y", NULL);
   check_error();
   fail_unless(handle_y == handle_y2);
   vhpi_release_handle(handle_y2);

   vhpiHandleT handle_y3 = vhpi_handle_by_name("vhpi1.y", NULL);
   check_error();
   fail_unless(handle_y == handle_y3);
   vhpi_release_handle(handle_y3);

   fail_unless(vhpi_get(vhpiKindP, handle_x) == vhpiPortDeclK);

   vhpi_release_handle(root);
}
