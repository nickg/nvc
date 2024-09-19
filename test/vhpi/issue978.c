#include "vhpi_test.h"

#include <string.h>

static void test_period(void)
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

static void test_one(void)
{
   vhpiHandleT one = vhpi_handle_by_name("issue978.one", NULL);
   check_error();

   vhpiHandleT t_int64 = vhpi_handle(vhpiType, one);
   check_error();
   fail_if(t_int64 == NULL);
   fail_unless(strcmp((char *)vhpi_get_str(vhpiNameP, t_int64), "T_INT64") == 0);
   fail_unless(vhpi_get(vhpiKindP, t_int64) == vhpiIntTypeDeclK);
   fail_if(vhpi_get(vhpiIsUnconstrainedP, t_int64));

   vhpiHandleT constrs = vhpi_iterator(vhpiConstraints, t_int64);
   check_error();
   fail_if(constrs == NULL);

   vhpiHandleT range = vhpi_scan(constrs);
   fail_unless(vhpi_get(vhpiLeftBoundP, range) == 0);  // XXX: VHPI limitation
   fail_unless(vhpi_get(vhpiRightBoundP, range) == -1);  // XXX: VHPI limitation
}

static void test_two(void)
{
   vhpiHandleT two = vhpi_handle_by_name("issue978.two", NULL);
   check_error();

   vhpiHandleT t_sub = vhpi_handle(vhpiType, two);
   check_error();
   fail_if(t_sub == NULL);
   fail_unless(strcmp((char *)vhpi_get_str(vhpiNameP, t_sub), "T_SUB") == 0);
   fail_unless(vhpi_get(vhpiKindP, t_sub) == vhpiSubtypeDeclK);
   fail_if(vhpi_get(vhpiIsUnconstrainedP, t_sub));

   vhpiHandleT constrs = vhpi_iterator(vhpiConstraints, t_sub);
   check_error();
   fail_if(constrs == NULL);

   vhpiHandleT range = vhpi_scan(constrs);
   fail_unless(vhpi_get(vhpiLeftBoundP, range) == 1);
   fail_unless(vhpi_get(vhpiRightBoundP, range) == 100);

   vhpiValueT value = {
      .format = vhpiLongIntVal,
   };
   vhpi_get_value(two, &value);
   check_error();

   fail_unless(value.value.longintg == 42);
}

static void start_of_sim(const vhpiCbDataT *cb_data)
{
   test_period();
   test_one();
   test_two();
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
