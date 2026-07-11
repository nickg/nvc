#include "vhpi_test.h"

#include <stdio.h>
#include <assert.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>

static void check_record(vhpiHandleT uRec, const char *prefix)
{
   vhpiHandleT type = VHPI_CHECK(vhpi_handle(vhpiType, uRec));
   fail_if(type == NULL);
   fail_unless(VHPI_CHECK(vhpi_get(vhpiIsCompositeP, type)));
   fail_if(VHPI_CHECK(vhpi_get(vhpiIsScalarP, type)));

   vhpi_release_handle(type);

   static const struct {
      const char *name;
      vhpiIntT size;
      vhpiClassKindT typ;
      vhpiClassKindT base;
   } expect[] = {
      { "A", 1, vhpiEnumTypeDeclK, vhpiEnumTypeDeclK },
      { "B", 2, vhpiSubtypeDeclK, vhpiArrayTypeDeclK }
   };

   vhpiHandleT names = vhpi_iterator(vhpiSelectedNames, uRec);
   for (int i = 0; i < sizeof(expect) / sizeof(expect[0]); i++) {
      vhpiHandleT e = vhpi_scan(names);
      fail_if(e == NULL);
      vhpi_printf("elem %s", vhpi_get_str(vhpiNameP, e));
      vhpi_printf("size %d", vhpi_get(vhpiSizeP, e));

      char buf[32];
      snprintf(buf, sizeof(buf), "%s.%s", prefix, expect[i].name);
      check_string(vhpi_get_str(vhpiNameP, e), buf);
      fail_unless(vhpi_get(vhpiSizeP, e) == expect[i].size);

      vhpiHandleT typ = VHPI_CHECK(vhpi_handle(vhpiType, e));
      vhpiHandleT base = VHPI_CHECK(vhpi_handle(vhpiBaseType, e));

      fail_unless(vhpi_get(vhpiKindP, typ) == expect[i].typ);
      fail_unless(vhpi_get(vhpiKindP, base) == expect[i].base);

      if (expect[i].typ == expect[i].base)
         fail_unless(vhpi_compare_handles(typ, base));

      vhpi_release_handle(typ);
      vhpi_release_handle(base);

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
