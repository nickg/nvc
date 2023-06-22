#include "vhpi_test.h"

#include <stdio.h>
#include <assert.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>

static vhpiHandleT m, n, o;

static vhpiIntT swan(vhpiIntT x)
{
   return (x & 0xf000) >> 12 |
          (x & 0x0f00) >> 4 |
          (x & 0x00f0) << 4 |
          (x & 0x000f) << 12;
}

void delta_recursive(vhpiHandleT parent, int base, int scale)
{
   int i = 0;

   vhpiHandleT children = vhpi_iterator(vhpiSelectedNames, parent);
   for (vhpiHandleT child = vhpi_scan(children);
        child;
        child = vhpi_scan(children), i++)
      delta_recursive(child, base + i * scale, scale / 16);

   children = vhpi_iterator(vhpiIndexedNames, parent);
   for (vhpiHandleT child = vhpi_scan(children);
        child;
        child = vhpi_scan(children), i++) {
      if (parent == n)
         delta_recursive(child, base + i / 5 * scale + i % 5 * scale / 16,
                         scale / 256);
      else
         delta_recursive(child, base + i * scale, scale / 16);
   }

   if (!i) {
      vhpiValueT val = {
         .format = vhpiIntVal,
      };

      vhpi_get_value(parent, &val);
      check_error();
      vhpi_printf("%-14s = %.04x", vhpi_get_str(vhpiNameP, parent),
                  val.value.intg);
      fail_unless(val.value.intg == swan(base));
   }
}

static void last_delta(const vhpiCbDataT *cb_data)
{
   delta_recursive(m, 0, 16);
   delta_recursive(n, 0, 4096);
   delta_recursive(o, 0, 4096);
}

void start_recursive(vhpiHandleT parent, int base, int scale, bool by_name)
{
   int i = 0;
   char name[64];
   const vhpiCharT *parent_name = vhpi_get_str(vhpiNameP, parent);

   vhpiHandleT children = vhpi_iterator(vhpiSelectedNames, parent);
   for (vhpiHandleT child = vhpi_scan(children);
        child;
        child = vhpi_scan(children), i++) {
      vhpiHandleT prefix = vhpi_handle(vhpiPrefix, child);
      fail_unless(parent == prefix);

      vhpiHandleT suffix = vhpi_handle(vhpiSuffix, child);
      fail_unless(vhpi_get(vhpiPositionP, suffix) == i);
      snprintf(name, sizeof(name), "%s.%s", parent_name,
               vhpi_get_str(vhpiNameP, suffix));
      fail_if(strcmp(name, (char *)vhpi_get_str(vhpiNameP, child)));
      if (by_name)
         fail_unless(vhpi_handle_by_name(name, NULL) == child);

      start_recursive(child, base + i * scale, scale / 16, by_name);
   }

   children = vhpi_iterator(vhpiIndexedNames, parent);
   for (vhpiHandleT child = vhpi_scan(children);
        child;
        child = vhpi_scan(children), i++) {
      if (parent == n)
         start_recursive(child, base + i / 5 * scale + i % 5 * scale / 16,
                         scale / 256, false);
      else
         start_recursive(child, base + i * scale, scale / 16, false);
   }

   if (!i) {
      vhpiValueT val = {
         .format = vhpiIntVal,
      };

      vhpi_get_value(parent, &val);
      check_error();
      vhpi_printf("%-14s = %.04x", parent_name, val.value.intg);
      fail_unless(val.value.intg == base);

      val.value.intg = swan(val.value.intg);
      vhpi_put_value(parent, &val, vhpiDepositPropagate);
      check_error();
   }
}

static void start_of_sim(const vhpiCbDataT *cb_data)
{
   start_recursive(m, 0, 16, true);
   start_recursive(n, 0, 4096, true);
   start_recursive(o, 0, 4096, true);
}

void vhpi5_startup(void)
{
   vhpi_printf("hello, world!");
   vhpi_printf("tool is %s", vhpi_get_str(vhpiNameP, NULL));

   vhpiHandleT root = vhpi_handle(vhpiRootInst, NULL);
   check_error();
   fail_if(root == NULL);
   vhpi_printf("root handle %p", root);

   const vhpiCharT *root_name = vhpi_get_str(vhpiNameP, root);
   vhpi_printf("root name is %s", root_name);

   m = vhpi_handle_by_name("m", root);
   check_error();
   fail_if(m == NULL);
   vhpi_printf("m handle %p", m);

   vhpiHandleT m_type = vhpi_handle(vhpiType, m);
   check_error();
   fail_if(m_type == NULL);
   fail_unless(vhpi_get(vhpiIsCompositeP, m_type));
   check_error();
   fail_if(vhpi_get(vhpiIsScalarP, m_type));
   check_error();

   int i = 0;
   vhpiHandleT elems = vhpi_iterator(vhpiRecordElems, m_type);
   for (vhpiHandleT elem = vhpi_scan(elems); elem; elem = vhpi_scan(elems), i++) {
      vhpi_printf("m elem %d is %s", i, vhpi_get_str(vhpiNameP, elem));
      fail_unless(vhpi_get(vhpiPositionP, elem) == i);
   }
   fail_unless(vhpi_get(vhpiNumFieldsP, m_type) == i);

   n = vhpi_handle_by_name("n", root);
   check_error();
   fail_if(n == NULL);

   o = vhpi_handle_by_name("o", root);
   check_error();
   fail_if(n == NULL);

   vhpiCbDataT cb_data = {
      .reason = vhpiCbStartOfSimulation,
      .cb_rtn = start_of_sim,
   };
   vhpi_register_cb(&cb_data, 0);
   check_error();

   cb_data.reason = vhpiCbLastKnownDeltaCycle;
   cb_data.cb_rtn = last_delta;
   vhpi_register_cb(&cb_data, 0);
   check_error();

   vhpi_release_handle(root);
}
