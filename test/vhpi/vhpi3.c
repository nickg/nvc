#include "vhpi_test.h"

#include <stdio.h>
#include <assert.h>
#include <stdlib.h>
#include <string.h>

static int64_t phys_to_i64(vhpiPhysT phys)
{
   return ((uint64_t)phys.high) << 32 | phys.low;
}

static void start_of_sim(const vhpiCbDataT *cb_data)
{
   vhpi_printf("start_of_sim");

   fail_unless(phys_to_i64(vhpiFS) == 1ll);
   fail_unless(phys_to_i64(vhpiPS) == 1000ll);
   fail_unless(phys_to_i64(vhpiNS) == 1000000ll);
   fail_unless(phys_to_i64(vhpiUS) == 1000000000ll);
   fail_unless(phys_to_i64(vhpiMS) == 1000000000000ll);
   fail_unless(phys_to_i64(vhpiS) == 1000000000000000ll);
   fail_unless(phys_to_i64(vhpiMN) == 1000000000000000ll * 60);
   fail_unless(phys_to_i64(vhpiHR) == 1000000000000000ll * 60 * 60);

   vhpiPhysT res_limit = vhpi_get_phys(vhpiResolutionLimitP, NULL);
   check_error();
   fail_unless(phys_to_i64(res_limit) == phys_to_i64(vhpiFS));

   vhpiHandleT root = vhpi_handle(vhpiRootInst, NULL);
   check_error();
   fail_if(root == NULL);

   vhpiHandleT handle_x = vhpi_handle_by_name("x", root);
   check_error();
   fail_if(handle_x == NULL);
   vhpi_printf("x handle %p", handle_x);

   vhpiPhysT x_val = vhpi_get_phys(vhpiPhysValP, handle_x);
   check_error();
   fail_unless(phys_to_i64(x_val) == 2);

   vhpiHandleT handle_weight_type = vhpi_handle(vhpiType, handle_x);
   check_error();
   fail_if(handle_weight_type == NULL);

   const char *weight_name = (char *)vhpi_get_str(vhpiNameP, handle_weight_type);
   fail_if(strcmp("WEIGHT", weight_name));
   const char *weight_fullname = (char *)vhpi_get_str(vhpiFullNameP, handle_weight_type);
   fail_if(strcmp("@WORK:VHPI3-TEST:WEIGHT", weight_fullname));

   vhpiHandleT handle_weight_cons =
      vhpi_handle_by_index(vhpiConstraints, handle_weight_type, 0);
   check_error();
   fail_if(handle_weight_cons == NULL);

   vhpiHandleT handle_weight_cons_iter =
      vhpi_iterator(vhpiConstraints, handle_weight_type);
   check_error();
   fail_unless(vhpi_scan(handle_weight_cons_iter) == handle_weight_cons);
   check_error();
   fail_unless(vhpi_scan(handle_weight_cons_iter) == NULL);

   vhpiPhysT weight_left = vhpi_get_phys(vhpiPhysLeftBoundP,
                                         handle_weight_cons);
   check_error();
   fail_unless(phys_to_i64(weight_left) == -100);

   vhpiPhysT weight_right = vhpi_get_phys(vhpiPhysRightBoundP,
                                          handle_weight_cons);
   check_error();
   fail_unless(phys_to_i64(weight_right) == 4000);

   vhpi_release_handle(handle_weight_cons_iter);
   vhpi_release_handle(handle_weight_cons);
   vhpi_release_handle(handle_weight_type);
   vhpi_release_handle(handle_x);
   vhpi_release_handle(root);
}

void vhpi3_startup(void)
{
   vhpi_printf("hello, world!");

   vhpiCbDataT cb_data1 = {
      .reason    = vhpiCbStartOfSimulation,
      .cb_rtn    = start_of_sim,
      .user_data = NULL,
   };
   vhpi_register_cb(&cb_data1, 0);
   check_error();
}
