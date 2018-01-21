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

   fail_unless(parse_value(t, "-1", &v));
   fail_unless(v == -1);

   fail_unless(parse_value(t, "2147483648", &v));
   fail_unless(v == 2147483648);

   fail_unless(parse_value(t, "-2147483648", &v));
   fail_unless(v == -2147483648);

   fail_unless(parse_value(t, "  14124  ", &v));
   fail_unless(v == 14124);

   fail_unless(parse_value(t, "25252781781981", &v));
   fail_unless(v == INT64_C(25252781781981));

   fail_unless(parse_value(t, "1_2_3", &v));
   fail_unless(v == 123);
}
END_TEST

START_TEST(test_enum)
{
   int64_t v;
   type_t t = type_new(T_ENUM);
   {
      tree_t lit1 = tree_new(T_ENUM_LIT);
      tree_set_ident(lit1, ident_new("'x'"));

      tree_t lit2 = tree_new(T_ENUM_LIT);
      tree_set_ident(lit2, ident_new("HELLO"));

      tree_t lit3 = tree_new(T_ENUM_LIT);
      tree_set_ident(lit3, ident_new("A_B_C"));

      type_enum_add_literal(t, lit1);
      type_enum_add_literal(t, lit2);
      type_enum_add_literal(t, lit3);
   }

   fail_unless(parse_value(t, "HELLO", &v));
   fail_unless(v == 1);

   fail_unless(parse_value(t, "'x'  ", &v));
   fail_unless(v == 0);

   fail_unless(parse_value(t, " \tA_B_C  ", &v));
   fail_unless(v == 2);
}
END_TEST

Suite *get_value_tests(void)
{
   Suite *s = suite_create("value");

   TCase *tc_core = tcase_create("Core");
   tcase_add_test(tc_core, test_integer);
   tcase_add_test(tc_core, test_enum);
   suite_add_tcase(s, tc_core);

   return s;
}
