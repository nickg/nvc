#include "vhpi_user.h"

#include <stdio.h>
#include <assert.h>
#include <stdlib.h>
#include <string.h>

#define fail_if(x)                                                      \
   if (x) vhpi_assert(vhpiFailure, "assertion '%s' failed at %s:%d",    \
                      #x, __FILE__, __LINE__)
#define fail_unless(x) fail_if(!(x))

static void check_error(void)
{
   vhpiErrorInfoT info;
   if (vhpi_check_error(&info))
      vhpi_assert(vhpiFailure, "unexpected error '%s'", info.message);
}

static int64_t phys_to_i64(vhpiPhysT phys)
{
   return ((int64_t)phys.high) << 32 | phys.low;
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

   vhpiHandleT handle_weight_cons =
      vhpi_handle_by_index(vhpiConstraints, handle_weight_type, 0);
   check_error();
   fail_if(handle_weight_cons == NULL);

   vhpiPhysT weight_left = vhpi_get_phys(vhpiPhysLeftBoundP,
                                         handle_weight_cons);
   check_error();
   fail_unless(phys_to_i64(weight_left) == -100);

   vhpiPhysT weight_right = vhpi_get_phys(vhpiPhysRightBoundP,
                                          handle_weight_cons);
   check_error();
   fail_unless(phys_to_i64(weight_right) == 4000);

   vhpi_release_handle(handle_weight_cons);
   vhpi_release_handle(handle_weight_type);
   vhpi_release_handle(handle_x);
   vhpi_release_handle(root);
}

static void startup()
{
   vhpi_printf("hello, world!");

   vhpiCbDataT cb_data1 = {
      .reason    = vhpiCbStartOfSimulation,
      .cb_rtn    = start_of_sim,
      .user_data = NULL,
   };
   (void)vhpi_register_cb(&cb_data1, vhpiReturnCb);
   check_error();
}

void (*vhpi_startup_routines[])() = {
   startup,
   NULL
};
