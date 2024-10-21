#include "vhpi_test.h"

#include <stdio.h>
#include <assert.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>

static void check_record(vhpiHandleT uRec, const char *prefix)
{
   vhpiHandleT type = vhpi_handle(vhpiType, uRec);
   check_error();
   fail_if(type == NULL);
   fail_unless(vhpi_get(vhpiIsCompositeP, type));
   check_error();
   fail_if(vhpi_get(vhpiIsScalarP, type));
   check_error();

   vhpi_release_handle(type);

   static const struct {
      const char *name;
      vhpiIntT size;
   } expect[] = {
      { "A", 1 }, { "B", 2 }
   };

   vhpiHandleT names = vhpi_iterator(vhpiSelectedNames, uRec);
   for (int i = 0; i < sizeof(expect) / sizeof(expect[0]); i++) {
      vhpiHandleT e = vhpi_scan(names);
      fail_if(e == NULL);
      vhpi_printf("elem %s", vhpi_get_str(vhpiNameP, e));
      vhpi_printf("size %d", vhpi_get(vhpiSizeP, e));

      char buf[32];
      snprintf(buf, sizeof(buf), "%s.%s", prefix, expect[i].name);
      fail_unless(strcmp((const char *)vhpi_get_str(vhpiNameP, e), buf) == 0);
     fail_unless(vhpi_get(vhpiSizeP, e) == expect[i].size);

      vhpi_release_handle(e);
   }
   vhpi_release_handle(names);
}

static void start_of_sim(const vhpiCbDataT *cb_data)
{
   vhpiHandleT root = vhpi_handle(vhpiRootInst, NULL);
   check_error();
   fail_if(root == NULL);
   vhpi_printf("root handle %p", root);

   const vhpiCharT *root_name = vhpi_get_str(vhpiNameP, root);
   vhpi_printf("root name is %s", root_name);

   vhpiHandleT uRec = vhpi_handle_by_name("uRec", root);
   check_error();
   fail_if(uRec == NULL);
   vhpi_printf("uRec handle %p", uRec);

   check_record(uRec, "UREC");

   vhpi_release_handle(uRec);

   vhpiHandleT uRecC = vhpi_handle_by_name("uRecC", root);
   check_error();
   fail_if(uRecC == NULL);
   vhpi_printf("uRecC handle %p", uRecC);

   check_record(uRecC, "URECC");

   vhpi_release_handle(uRecC);

   vhpi_release_handle(root);
}

void issue1035_startup(void)
{
   vhpi_printf("hello, world!");

   vhpiCbDataT cb_data = {
      .reason = vhpiCbStartOfSimulation,
      .cb_rtn = start_of_sim,
   };
   vhpi_register_cb(&cb_data, 0);
   check_error();
}
