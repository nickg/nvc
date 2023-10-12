#include "vhpi_test.h"

#include <string.h>

static void end_of_init(const vhpiCbDataT *cb_data)
{
   vhpiHandleT root = vhpi_handle(vhpiRootInst, NULL);
   check_error();
   fail_if(root == NULL);
   vhpi_printf("root handle %p", root);

   vhpiHandleT s_signal = vhpi_handle_by_name("s", root);
   check_error();
   fail_if(s_signal == NULL);
   vhpi_printf("x handle %p", s_signal);
   fail_unless(vhpi_get(vhpiKindP, s_signal) == vhpiSigDeclK);

   vhpiHandleT s_type = vhpi_handle(vhpiType, s_signal);
   check_error();
   fail_if(s_type == NULL);
   vhpi_printf("s type handle %p", s_type);
   vhpi_printf("s type name is %s", vhpi_get_str(vhpiNameP, s_type));
   vhpi_printf("s type full name is %s", vhpi_get_str(vhpiFullNameP, s_type));
   fail_unless(vhpi_get(vhpiNumDimensionsP, s_type) == 1);
   fail_if(vhpi_get(vhpiIsUnconstrainedP, s_type));

   vhpiHandleT s_constrs = vhpi_iterator(vhpiConstraints, s_type);
   check_error();
   fail_if(s_constrs == NULL);

   vhpiHandleT s_range = vhpi_scan(s_constrs);
   check_error();
   fail_if(s_range == NULL);
   fail_unless(vhpi_scan(s_constrs) == NULL);
   vhpi_printf("s type range handle %p", s_range);
   vhpi_printf("s left bound %d", vhpi_get(vhpiLeftBoundP, s_range));
   vhpi_printf("s right bound %d", vhpi_get(vhpiRightBoundP, s_range));
   fail_unless(vhpi_get(vhpiLeftBoundP, s_range) == 1);
   fail_unless(vhpi_get(vhpiRightBoundP, s_range) == 5);

   vhpiHandleT s_elem = vhpi_handle(vhpiElemType, s_type);
   check_error();
   fail_if(s_elem == NULL);
   vhpi_printf("s elem type handle %p", s_elem);
   vhpi_printf("s elem type name is %s", vhpi_get_str(vhpiNameP, s_elem));

   const char *s_elem_name = (char *)vhpi_get_str(vhpiFullNameP, s_elem);
   vhpi_printf("s elem type full name is %s", s_elem_name);
   fail_unless(strcmp(s_elem_name, "@STD:STANDARD:BIT_VECTOR") == 0);

   vhpiHandleT s_elem_constrs = vhpi_iterator(vhpiConstraints, s_elem);
   check_error();
   fail_if(s_elem_constrs == NULL);

   vhpiHandleT s_elem_range = vhpi_scan(s_elem_constrs);
   check_error();
   fail_if(s_elem_range == NULL);
   fail_unless(vhpi_scan(s_constrs) == NULL);
   vhpi_printf("s type range handle %p", s_elem_range);
   vhpi_printf("s left bound %d", vhpi_get(vhpiLeftBoundP, s_elem_range));
   vhpi_printf("s right bound %d", vhpi_get(vhpiRightBoundP, s_elem_range));
   fail_unless(vhpi_get(vhpiLeftBoundP, s_elem_range) == 3);
   fail_unless(vhpi_get(vhpiRightBoundP, s_elem_range) == 0);

   vhpiHandleT s3_signal = vhpi_handle_by_name("s3", root);
   check_error();
   fail_if(s3_signal == NULL);
   vhpi_printf("s3 handle %p", s3_signal);
   fail_unless(vhpi_get(vhpiKindP, s3_signal) == vhpiSigDeclK);

   vhpiHandleT s3f = vhpi_handle_by_name("s3.f", root);
   check_error();
   fail_if(s3f == NULL);
   fail_unless(vhpi_get(vhpiKindP, s3f) == vhpiSelectedNameK);

   vhpi_printf("s3.f size %d", vhpi_get(vhpiSizeP, s3f));
   fail_unless(vhpi_get(vhpiSizeP, s3f) == 4);

   vhpiHandleT s3f_type = vhpi_handle(vhpiBaseType, s3f);
   check_error();
   fail_if(s3f_type == NULL);
   vhpi_printf("s3.f type handle %p", s3f_type);
   vhpi_printf("s3.f type name is %s", vhpi_get_str(vhpiNameP, s3f_type));

   vhpiHandleT s3f_constrs = vhpi_iterator(vhpiConstraints, s3f_type);
   check_error();
   fail_if(s3f_constrs == NULL);

   vhpiHandleT s3f_range = vhpi_scan(s3f_constrs);
   check_error();
   fail_if(s3f_range == NULL);
   fail_unless(vhpi_scan(s_constrs) == NULL);
   vhpi_printf("s3.f type range handle %p", s3f_range);
   vhpi_printf("s3.f left bound %d", vhpi_get(vhpiLeftBoundP, s3f_range));
   vhpi_printf("s3.f right bound %d", vhpi_get(vhpiRightBoundP, s3f_range));
   fail_unless(vhpi_get(vhpiLeftBoundP, s3f_range) == 1);
   fail_unless(vhpi_get(vhpiRightBoundP, s3f_range) == 4);
}

void vhpi9_startup(void)
{
   vhpiCbDataT cb_data = {
      .reason    = vhpiCbEndOfInitialization,
      .cb_rtn    = end_of_init,
   };
   vhpi_register_cb(&cb_data, 0);
   check_error();
}
