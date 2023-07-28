#include "vhpi_test.h"

#include <string.h>

void issue744_startup(void)
{
   vhpiHandleT root = vhpi_handle(vhpiRootInst, NULL);
   check_error();
   fail_if(root == NULL);
   vhpi_printf("root handle %p", root);

   vhpiHandleT b_signal = vhpi_handle_by_name("b", root);
   check_error();
   fail_if(b_signal == NULL);
   vhpi_printf("x handle %p", b_signal);
   fail_unless(vhpi_get(vhpiKindP, b_signal) == vhpiSigDeclK);

   vhpiHandleT b_type = vhpi_handle(vhpiType, b_signal);
   check_error();
   fail_if(b_type == NULL);
   vhpi_printf("b type handle %p", b_type);
   vhpi_printf("b type name is %s", vhpi_get_str(vhpiNameP, b_type));
   vhpi_printf("b type full name is %s", vhpi_get_str(vhpiFullNameP, b_type));
   fail_unless(vhpi_get(vhpiNumDimensionsP, b_type) == 1);
   fail_if(vhpi_get(vhpiIsUnconstrainedP, b_type));

   vhpiHandleT b_constrs = vhpi_iterator(vhpiConstraints, b_type);
   check_error();
   fail_if(b_constrs == NULL);

   vhpiHandleT b_range = vhpi_scan(b_constrs);
   check_error();
   fail_if(b_range == NULL);
   fail_unless(vhpi_scan(b_constrs) == NULL);
   vhpi_printf("v type range handle %p", b_range);
   vhpi_printf("v left bound %d", vhpi_get(vhpiLeftBoundP, b_range));
   vhpi_printf("v right bound %d", vhpi_get(vhpiRightBoundP, b_range));
   fail_unless(vhpi_get(vhpiLeftBoundP, b_range) == 5);
   fail_unless(vhpi_get(vhpiRightBoundP, b_range) == 0);
}
