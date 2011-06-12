#include "lib.h"

#include <check.h>
#include <stdlib.h>

START_TEST(test_make_lib)
{
   lib_t l = make_lib("test");
   fail_if(l == NULL);
}
END_TEST

int main(void)
{
   Suite *s = suite_create("lib");

   TCase *tc_core = tcase_create("Core");
   tcase_add_test(tc_core, test_make_lib);
   suite_add_tcase(s, tc_core);
   
   SRunner *sr = srunner_create(s);
   srunner_run_all(sr, CK_NORMAL);

   int nfail = srunner_ntests_failed(sr);

   srunner_free(sr);
   
   return nfail == 0 ? EXIT_SUCCESS : EXIT_FAILURE;
}

