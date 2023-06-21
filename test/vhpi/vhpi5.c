#include "vhpi_test.h"

#include <stdio.h>
#include <assert.h>
#include <stdlib.h>
#include <string.h>

static vhpiHandleT m;

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

   vhpi_release_handle(root);
}
