#include "parse.h"
#include "type.h"
#include "common.h"

#include <check.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

START_TEST(test_integer)
{
   type_t t = type_new(T_INTEGER);
   int64_t v;

   fail_unless(parse_value(t, "0", &v));
   fail_unless(v == 0);

   fail_unless(parse_value(t, "1", &v));
   fail_unless(v == 1);

   fail_unless(parse_value(t, "  14124  ", &v));
   fail_unless(v == 14124);

   fail_unless(parse_value(t, "25252781781981", &v));
   fail_unless(v == 25252781781981ll);

   fail_unless(parse_value(t, "1_2_3", &v));
   fail_unless(v == 123);

   fail_if(parse_value(t, "-5", &v));
}
END_TEST

int main(void)
{
   register_trace_signal_handlers();

   Suite *s = suite_create("value");

   TCase *tc_core = tcase_create("Core");
   tcase_add_test(tc_core, test_integer);
   suite_add_tcase(s, tc_core);

   SRunner *sr = srunner_create(s);
   srunner_run_all(sr, CK_NORMAL);

   int nfail = srunner_ntests_failed(sr);

   srunner_free(sr);

   return nfail == 0 ? EXIT_SUCCESS : EXIT_FAILURE;
}
