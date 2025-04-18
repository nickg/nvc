#include "vhpi_test.h"

#include <stdio.h>
#include <assert.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>

static void start_of_sim(const vhpiCbDataT *cb_data)
{
   vhpiHandleT root = VHPI_CHECK(vhpi_handle(vhpiRootInst, NULL));
   fail_if(root == NULL);
   vhpi_printf("root handle %p", root);

   const vhpiCharT *root_name = vhpi_get_str(vhpiNameP, root);
   vhpi_printf("root name is %s", root_name);

   vhpiHandleT t_int_ptr = VHPI_CHECK(vhpi_handle_by_name("t_int_ptr", root));
   fail_unless(vhpi_get(vhpiKindP, t_int_ptr) == vhpiAccessTypeDeclK);

   vhpiHandleT int_valtyp = VHPI_CHECK(vhpi_handle(vhpiValType, t_int_ptr));
   fail_unless(vhpi_get(vhpiKindP, int_valtyp) == vhpiIntTypeDeclK);

   vhpiHandleT t_rec_ptr = VHPI_CHECK(vhpi_handle_by_name("t_rec_ptr", root));
   fail_unless(vhpi_get(vhpiKindP, t_rec_ptr) == vhpiAccessTypeDeclK);

   vhpiHandleT rec_valtyp = VHPI_CHECK(vhpi_handle(vhpiValType, t_rec_ptr));
   fail_unless(rec_valtyp == NULL);   // Not currently supported

   vhpiHandleT t_file = VHPI_CHECK(vhpi_handle_by_name("t_file", root));
   fail_unless(vhpi_get(vhpiKindP, t_file) == vhpiFileTypeDeclK);

   vhpiHandleT file_valtyp = VHPI_CHECK(vhpi_handle(vhpiValType, t_file));
   fail_unless(vhpi_get(vhpiKindP, file_valtyp) == vhpiIntTypeDeclK);

   vhpi_release_handle(file_valtyp);
   vhpi_release_handle(int_valtyp);
   vhpi_release_handle(t_int_ptr);
   vhpi_release_handle(t_rec_ptr);
   vhpi_release_handle(t_file);
   vhpi_release_handle(root);
}

void issue1192_startup(void)
{
   vhpiCbDataT cb_data = {
      .reason = vhpiCbStartOfSimulation,
      .cb_rtn = start_of_sim,
   };
   VHPI_CHECK(vhpi_register_cb(&cb_data, 0));
}
