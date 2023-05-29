#include "vhpi_test.h"

#include <stdio.h>
#include <assert.h>
#include <stdlib.h>
#include <string.h>

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

   vhpiHandleT s1 = vhpi_handle_by_name("s1", root);
   check_error();
   fail_if(s1 == NULL);
   vhpi_printf("s1 handle %p", s1);

   vhpiHandleT s1_type = vhpi_handle(vhpiType, s1);
   check_error();
   fail_if(s1_type == NULL);
   fail_unless(vhpi_get(vhpiIsCompositeP, s1_type));
   check_error();
   fail_if(vhpi_get(vhpiIsScalarP, s1_type));
   check_error();

   // TODO: check fields, etc.

   vhpi_release_handle(root);
}
