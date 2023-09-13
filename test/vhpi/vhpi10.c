#include "vhpi_test.h"

#include <string.h>

static vhpiHandleT handle_sos;

static void start_of_sim(const vhpiCbDataT *cb_data)
{
   vhpiHandleT root = vhpi_handle(vhpiRootInst, NULL);
   check_error();

   vhpiHandleT g0 = vhpi_handle_by_name("g0", root);
   check_error();

   vhpiValueT value = {
      .format = vhpiObjTypeVal
   };
   vhpi_get_value(g0, &value);
   check_error();
   fail_unless(value.format == vhpiIntVal);
   vhpi_printf("value=%d", value.value.intg);
   fail_unless(value.value.intg == 42);
   fail_unless(value.numElems == 1);

   vhpiHandleT c0 = vhpi_handle_by_name("c0", root);
   check_error();

   value.format = vhpiObjTypeVal;
   vhpi_get_value(c0, &value);
   check_error();
   fail_unless(value.format == vhpiIntVal);
   vhpi_printf("value=%d", value.value.intg);
   fail_unless(value.value.intg == 5);
   fail_unless(value.numElems == 1);

   vhpiHandleT c1 = vhpi_handle_by_name("c1", root);
   check_error();

   value.format = vhpiObjTypeVal;
   vhpi_get_value(c1, &value);
   check_error();
   fail_unless(value.format == vhpiRealVal);
   vhpi_printf("value=%f", value.value.real);
   fail_unless(value.value.real == 1.5);
   fail_unless(value.numElems == 1);

   vhpiHandleT i0g0 = vhpi_handle_by_name("i0.g0", root);
   check_error();
   fail_if(i0g0 == NULL);
   vhpi_printf("i0g0 handle %p", i0g0);
   fail_unless(vhpi_get(vhpiKindP, i0g0) == vhpiGenericDeclK);

   value.format = vhpiObjTypeVal;
   vhpi_get_value(i0g0, &value);
   check_error();
   fail_unless(value.format == vhpiIntVal);
   vhpi_printf("value=%d", value.value.intg);
   fail_unless(value.value.intg == 100);
   fail_unless(value.numElems == 1);

   vhpiHandleT g1 = vhpi_handle_by_name("g1", root);
   check_error();

   value.format = vhpiObjTypeVal;
   value.bufSize = 0;
   value.value.str = NULL;

   const int bufsz = vhpi_get_value(g1, &value);
   check_error();
   fail_unless(value.format == vhpiStrVal);
   fail_unless(bufsz == 6);

   vhpiCharT str[6];
   value.value.str = str;
   value.bufSize = sizeof(str);
   vhpi_get_value(g1, &value);
   check_error();
   fail_unless(value.numElems == 5);
   vhpi_printf("g1 value '%s'", (char *)str);
   fail_unless(strcmp((char *)str, "hello") == 0);
   fail_unless(vhpi_get(vhpiSizeP, g1) == 5);

   vhpi_release_handle(handle_sos);
}

void vhpi10_startup(void)
{
   vhpiHandleT root = vhpi_handle(vhpiRootInst, NULL);
   check_error();
   fail_if(root == NULL);
   vhpi_printf("root handle %p", root);

   vhpiHandleT g0 = vhpi_handle_by_name("g0", root);
   check_error();
   fail_if(g0 == NULL);
   vhpi_printf("g0 handle %p", g0);
   fail_unless(vhpi_get(vhpiKindP, g0) == vhpiGenericDeclK);
   fail_unless(vhpi_get(vhpiModeP, g0) == vhpiInMode);
   fail_unless(vhpi_get(vhpiIsLocalP, g0) == vhpiFalse);

   vhpiHandleT it1 = vhpi_iterator(vhpiGenericDecls, root);
   fail_if(it1 == NULL);
   fail_unless(vhpi_scan(it1) == g0);

   vhpiHandleT g1 = vhpi_scan(it1);
   fail_if(g1 == NULL);

   fail_unless(vhpi_scan(it1) == NULL);
   vhpi_release_handle(it1);

   vhpiHandleT c0 = vhpi_handle_by_name("c0", root);
   check_error();
   fail_if(c0 == NULL);
   vhpi_printf("c0 handle %p", c0);
   fail_unless(vhpi_get(vhpiKindP, c0) == vhpiConstDeclK);

   vhpiHandleT it2 = vhpi_iterator(vhpiConstDecls, root);
   fail_if(it2 == NULL);
   fail_unless(vhpi_scan(it2) == c0);

   vhpiHandleT c1 = vhpi_scan(it2);
   fail_if(c1 == NULL);
   fail_unless(vhpi_get(vhpiKindP, c1) == vhpiConstDeclK);

   fail_unless(vhpi_scan(it2) == NULL);
   vhpi_release_handle(it2);

   vhpiHandleT b0 = vhpi_handle_by_name("b0", root);
   check_error();
   fail_if(b0 == NULL);
   vhpi_printf("b0 handle %p", b0);
   fail_unless(vhpi_get(vhpiKindP, b0) == vhpiBlockStmtK);
   fail_if(vhpi_get(vhpiIsGuardedP, b0));
   fail_if(vhpi_get(vhpiIsSeqStmtP, b0));
   vhpi_printf("b0 label %s", vhpi_get_str(vhpiLabelNameP, b0));
   fail_unless(strcmp((char *)vhpi_get_str(vhpiLabelNameP, b0), "B0") == 0);

   vhpiHandleT b0s0 = vhpi_handle_by_name("s0", b0);
   check_error();
   fail_if(b0s0 == NULL);
   fail_unless(vhpi_get(vhpiKindP, b0s0) == vhpiSigDeclK);

   vhpiHandleT it3 = vhpi_iterator(vhpiBlockStmts, root);
   fail_if(it3 == NULL);
   fail_unless(vhpi_scan(it3) == b0);

   vhpiHandleT b1 = vhpi_scan(it3);
   fail_if(b1 == NULL);
   fail_unless(vhpi_get(vhpiIsGuardedP, b1));

   fail_unless(vhpi_scan(it3) == NULL);
   vhpi_release_handle(it3);

   vhpiHandleT b1s0 = vhpi_handle_by_name("s0", b1);
   check_error();
   fail_if(b1s0 == NULL);
   fail_unless(vhpi_get(vhpiKindP, b1s0) == vhpiSigDeclK);

   vhpiHandleT i0 = vhpi_handle_by_name("i0", root);
   check_error();
   fail_if(i0 == NULL);
   vhpi_printf("i0 handle %p", i0);
   fail_unless(vhpi_get(vhpiKindP, i0) == vhpiCompInstStmtK);
   fail_if(vhpi_get(vhpiIsSeqStmtP, i0));
   vhpi_printf("io label %s", vhpi_get_str(vhpiLabelNameP, i0));
   fail_unless(strcmp((char *)vhpi_get_str(vhpiLabelNameP, i0), "I0") == 0);

   vhpiHandleT it4 = vhpi_iterator(vhpiCompInstStmts, root);
   fail_if(it4 == NULL);
   fail_unless(vhpi_scan(it4) == i0);

   fail_unless(vhpi_scan(it4) == NULL);
   vhpi_release_handle(it4);

   vhpiCbDataT cb_data1 = {
      .reason    = vhpiCbStartOfSimulation,
      .cb_rtn    = start_of_sim,
      .user_data = (char *)"some user data",
   };
   handle_sos = vhpi_register_cb(&cb_data1, vhpiReturnCb);
   check_error();
   fail_unless(vhpi_get(vhpiStateP, handle_sos) == vhpiEnable);

   vhpiValueT value = {
      .format = vhpiObjTypeVal
   };
   vhpi_get_value(g0, &value);

   vhpiErrorInfoT info;
   fail_unless(vhpi_check_error(&info));
}
