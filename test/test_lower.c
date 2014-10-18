#include "test_util.h"
#include "phase.h"

START_TEST(test_wait1)
{
   input_from_file(TESTDIR "/lower/wait1.vhd");

   const error_t expect[] = {
      { -1, NULL }
   };
   expect_errors(expect);

   tree_t e = run_elab();
   lower_unit(e);
}
END_TEST

int main(void)
{
   Suite *s = suite_create("lower");

   TCase *tc = nvc_unit_test();
   tcase_add_test(tc, test_wait1);
   suite_add_tcase(s, tc);

   return nvc_run_test(s);
}
