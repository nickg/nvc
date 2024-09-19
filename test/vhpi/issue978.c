#include "vhpi_test.h"

#include <string.h>

static void start_of_sim(const vhpiCbDataT *cb_data)
{
   vhpiHandleT period = vhpi_handle_by_name("issue978.period", NULL);
   check_error();

   vhpiHandleT delay_length = vhpi_handle(vhpiType, period);
   check_error();
   fail_if(delay_length == NULL);
   fail_unless(strcmp((char *)vhpi_get_str(vhpiNameP, delay_length), "DELAY_LENGTH") == 0);
   fail_unless(vhpi_get(vhpiKindP, delay_length) == vhpiSubtypeDeclK);
   fail_if(vhpi_get(vhpiIsUnconstrainedP, delay_length));

   vhpiHandleT constrs = vhpi_iterator(vhpiConstraints, delay_length);
   check_error();
   fail_if(constrs == NULL);

   vhpiHandleT range = vhpi_scan(constrs);

   vhpiPhysT left = vhpi_get_phys(vhpiPhysLeftBoundP, range);
   fail_unless(left.low == 0);
   fail_unless(left.high == 0);

   vhpiPhysT right = vhpi_get_phys(vhpiPhysRightBoundP, range);
   fail_unless(right.low == (uint32_t)INT64_MAX);
   fail_unless(right.high == (int32_t)(INT64_MAX >> 32));
}

void issue978_startup(void)
{
   vhpiCbDataT cb_data = {
      .reason = vhpiCbStartOfSimulation,
      .cb_rtn = start_of_sim,
   };
   vhpi_register_cb(&cb_data, 0);
   check_error();
}
