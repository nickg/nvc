#include "type.h"
#include "util.h"
#include "phase.h"
#include "test_util.h"

#include <check.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

START_TEST(test_bounds)
{
   const error_t expect[] = {
      {  26, "left index 0 violates constraint POSITIVE" },
      {  27, "right index 60 violates constraint FOO" },
      {  31, "array S index -52 out of bounds 1 to 10" },
      {  32, "slice right index 11 out of bounds 1 to 10" },
      {  33, "slice left index 0 out of bounds 1 to 10" },
      {  37, "aggregate index 0 out of bounds 1 to 2147483647" },
      {  46, "actual length 4 does not match formal length 8 for parameter X" },
      {  47, "actual length 4 does not match formal length 8 for parameter X" },
      {  50, "actual length 4 for dimension 2 does not" },
      {  53, "value 9 does not match length of target 10 for signal S" },
      {  54, "value 2 does not match length of target 0 for signal N" },
      {  55, "expected at most 0 positional associations in MY_VEC1 " },
      {  60, "length of value 10 does not match length of target 3" },
      {  66, "array S index 11 out of bounds 1 to 10" },
      {  67, "array S index -1 out of bounds 1 to 10" },
      {  74, "aggregate index 5 out of bounds 1 to 3" },
      {  74, "aggregate index 0 out of bounds 1 to 3" },
      {  83, "value '1' out of target bounds 'a' to 'z'" },
      {  84, "value 0 out of target bounds 1 to 2147483647" },
      {  89, "invalid dimension 5 for type MY_VEC1" },
      {  94, "value -1 out of bounds 0 to 2147483647 for parameter X" },
      { 107, "aggregate index 5 out of bounds 1 to 3" },
      { 116, "length of sub-aggregate 2 does not match expected length 4" },
      { 137, "array index 14 out of bounds 0 to 2" },
      { 155, "value 2.000000 out of bounds 0.000000 to 1.000000 for parameter"},
      { 164, "missing choice for element FOUR of T_ARR with index type SE"},
      { 165, "expected at most 3 positional associations in T_ARR aggregate "
        "with index type SE range TWO to FOUR"},
      { 175, "value ONE out of bounds THREE downto TWO for parameter ARG2"},
      { 176, "value FOUR out of bounds THREE downto TWO for parameter ARG2"},
      { 177, "value ONE out of bounds TWO to FOUR for parameter ARG1"},
      { 178, "value FIVE out of bounds TWO to FOUR for parameter ARG1"},
      { 188, "aggregate index ONE out of bounds TWO to FOUR"},
      { 190, "aggregate index ONE out of bounds TWO to FOUR"},
      { 190, "aggregate index FIVE out of bounds TWO to FOUR"},
      { 198, "length of sub-aggregate 3 does not match expected length 4" },
      { 206, "left index ONE violates constraint SE" },
      { 206, "right index FOUR violates constraint SE" },
      { 219, "value -5 NS out of bounds 0 HR to 10 NS for parameter A"},
      { 221, "value 5 NS out of bounds 10 SEC downto 20 US for parameter A"},
      { 227, "value 200 NS out of target bounds -10 NS to 10 NS"},
      { 228, "value -200 NS out of target bounds -10 NS to 10 NS"},
      { -1, NULL }
   };
   expect_errors(expect);

   input_from_file(TESTDIR "/bounds/bounds.vhd");

   tree_t a = parse_and_check(T_ENTITY, T_ARCH);
   fail_unless(error_count() == 0);

   simplify_global(a, NULL);   // Global to fold TIME expressions
   bounds_check(a);

   check_expected_errors();
}
END_TEST

START_TEST(test_bounds2)
{
   const error_t expect[] = {
      {  13, "assignment delay may not be negative"},
      {  20, "assignment delay may not be negative"},
      {  24, "assignment delay may not be negative"},
      {  25, "assignment delay may not be negative"},
      {  33, "rejection limit may not be negative"},
      {  34, "rejection limit may not be greater than first assignment delay"},
      {  39, "wait timeout may not be negative"},
      {  52, "value 20 out of target bounds 0 to 9"},
      {  53, "value 'Z' out of target bounds 'a' to 'z'"},
      {  54, "value 10.000000 out of target bounds 0.000000 to 5.000000"},
      {  55, "value 0 HR out of target bounds 10 NS to 10 US"},
      {  56, "value 10 out of target bounds 0 to 1"},
      {  59, "value 30 out of target bounds 1 to 10"},
      {  63, "value 'c' out of target bounds 'a' to 'b'"},
      {  73, "value 2 out of target bounds 0 to 1"},
      {  70, "value 20 out of target bounds 10 downto 0"},
      {  79, "value 5.100000 out of target bounds 0.000000 to 5.000000"},
      {  95, "assignment delays must be in ascending time order"},
      {  96, "assignment delays must be in ascending time order"},
      {  97, "assignment delays must be in ascending time order"},
      {  98, "assignment delays must be in ascending time order"},
      {  99, "assignment delays must be in ascending time order"},
      { -1, NULL }
   };
   expect_errors(expect);

   input_from_file(TESTDIR "/bounds/bounds2.vhd");

   tree_t a = parse_and_check(T_ENTITY, T_ARCH);
   fail_unless(error_count() == 0);

   simplify_global(a, NULL);   // Global to fold TIME expressions
   bounds_check(a);

   check_expected_errors();
}
END_TEST

START_TEST(test_case)
{
   const error_t expect[] = {
      {  13, "missing choice C in case statement" },
      {  19, "missing choice B in case statement" },
      {  30, "10 to 19" },
      {  36, "4 to 2147483647" },
      {  44, "2147483647" },
      {  51, "value 50 is already covered" },
      {  53, "range 60 to 64 is already covered" },
      {  59, "value -1 outside NATURAL bounds" },
      {  58, "0 to 2147483647" },
      {  79, "choices cover only 2 of 8 possible values" },
      {  84, "missing choice for element 3 of BIT_VECTOR with index type "
         "NATURAL range 1 to 3" },
      {  86, "expected at most 3 positional associations in BIT_VECTOR "
         "aggregate with index type NATURAL range 1 to 3" },
      {  88, "expected 3 elements in string literal but have 2" },
      {  90, "expected 3 elements in string literal but have 4" },
      {  95, "choices cover only 2 of 65536 possible values" },
      { 101, "choices cover only 2 of 121 possible values" },
      { -1, NULL }
   };
   expect_errors(expect);

   input_from_file(TESTDIR "/bounds/case.vhd");

   tree_t a = parse_and_check(T_ENTITY, T_ARCH);
   fail_unless(error_count() == 0);

   simplify_local(a);
   bounds_check(a);

   check_expected_errors();
}
END_TEST

START_TEST(test_issue36)
{
   input_from_file(TESTDIR "/bounds/issue36.vhd");

   tree_t e = parse_and_check(T_ENTITY);
   fail_unless(error_count() == 0);

   simplify_local(e);
   bounds_check(e);

   fail_if_errors();
}
END_TEST

START_TEST(test_issue54)
{
   const error_t expect[] = {
      { 10, "aggregate index 7 out of bounds 3 downto 0" },
      { 10, "aggregate index 4 out of bounds 3 downto 0" },
      { 11, "aggregate index 3 out of bounds 7 downto 4" },
      { 11, "aggregate index 0 out of bounds 7 downto 4" },
      { 12, "aggregate index 3 out of bounds 7 downto 4" },
      { 12, "aggregate index 0 out of bounds 7 downto 4" },
      { -1, NULL }
   };
   expect_errors(expect);

   input_from_file(TESTDIR "/bounds/issue54.vhd");

   tree_t a = parse_and_check(T_ENTITY, T_ARCH);
   fail_unless(error_count() == 0);

   simplify_local(a);
   bounds_check(a);

   check_expected_errors();
}
END_TEST

START_TEST(test_issue99)
{
   const error_t expect[] = {
      {  7, "type conversion argument -1.5 out of bounds 0 to 2147483647" },
      {  8, "type conversion argument -1 out of bounds 1 to 5" },
      { -1, NULL }
   };
   expect_errors(expect);

   input_from_file(TESTDIR "/bounds/issue99.vhd");

   tree_t a = parse_and_check(T_ENTITY, T_ARCH);
   fail_unless(error_count() == 0);

   simplify_local(a);
   bounds_check(a);

   check_expected_errors();
}
END_TEST

START_TEST(test_issue150)
{
   const error_t expect[] = {
      {  7, "missing choices for elements 6 to 7 of T_LUT8X8 with index "
         "type INTEGER range 0 to 7" },
      { -1, NULL }
   };
   expect_errors(expect);

   input_from_file(TESTDIR "/bounds/issue150.vhd");

   tree_t a = parse_and_check(T_ENTITY, T_ARCH);
   fail_unless(error_count() == 0);

   simplify_local(a);
   bounds_check(a);

   check_expected_errors();
}
END_TEST

START_TEST(test_issue200)
{
   input_from_file(TESTDIR "/bounds/issue200.vhd");

   tree_t a = parse_and_check(T_ENTITY, T_ARCH);
   fail_unless(error_count() == 0);

   simplify_local(a);
   bounds_check(a);

   fail_if_errors();
}
END_TEST

START_TEST(test_issue208)
{
   const error_t expect[] = {
      { 20, "missing choice for element 1 of type NATURAL range 1 downto 0" },
      { -1, NULL }
   };
   expect_errors(expect);

   input_from_file(TESTDIR "/bounds/issue208.vhd");

   tree_t a = parse_and_check(T_ENTITY, T_ARCH);
   fail_unless(error_count() == 0);

   simplify_local(a);
   bounds_check(a);

   check_expected_errors();
}
END_TEST

START_TEST(test_issue247)
{
   const error_t expect[] = {
      { -1, NULL }
   };
   expect_errors(expect);

   input_from_file(TESTDIR "/bounds/issue247.vhd");

   tree_t a = parse_and_check(T_PACKAGE);
   fail_unless(error_count() == 0);

   simplify_local(a);
   bounds_check(a);

   check_expected_errors();
}
END_TEST

START_TEST(test_issue269)
{
   const error_t expect[] = {
      { -1, NULL }
   };
   expect_errors(expect);

   input_from_file(TESTDIR "/bounds/issue269.vhd");

   tree_t a = parse_and_check(T_PACKAGE);
   fail_unless(error_count() == 0);

   simplify_local(a);
   bounds_check(a);

   check_expected_errors();
}
END_TEST

START_TEST(test_issue307b)
{
   input_from_file(TESTDIR "/bounds/issue307b.vhd");

   tree_t a = parse_check_and_simplify(T_ENTITY, T_ARCH);
   fail_unless(error_count() == 0);

   bounds_check(a);
   fail_if_errors();
}
END_TEST

START_TEST(test_issue356)
{
   input_from_file(TESTDIR "/bounds/issue356.vhd");

   tree_t a = parse_check_and_simplify(T_ENTITY, T_ARCH);
   fail_unless(error_count() == 0);

   bounds_check(a);
   fail_if_errors();
}
END_TEST

START_TEST(test_issue98)
{
   input_from_file(TESTDIR "/bounds/issue98.vhd");

   const error_t expect[] = {
      {  5, "value 9223372036854775807 out of target bounds -2147483648 "
         "to 2147483647 for signal I" },
      { -1, NULL }
   };
   expect_errors(expect);

   tree_t a = parse_check_and_simplify(T_ENTITY, T_ARCH);
   fail_unless(error_count() == 0);

   bounds_check(a);
   check_expected_errors();
}
END_TEST

START_TEST(test_tc1147)
{
   input_from_file(TESTDIR "/bounds/tc1147.vhd");

   tree_t a = parse_check_and_simplify(T_ENTITY, T_ARCH);
   fail_unless(error_count() == 0);

   bounds_check(a);
   fail_if_errors();
}
END_TEST

START_TEST(test_aggregate)
{
   input_from_file(TESTDIR "/bounds/aggregate.vhd");

   const error_t expect[] = {
      {  8, "duplicate choice for A" },
      {  8, "for element B of MY_ENUM_MAP with index type MY_ENUM" },
      {  9, "for element C of MY_ENUM_MAP with index type MY_ENUM" },
      { 10, "duplicate choice for B" },
      { -1, NULL }
   };
   expect_errors(expect);

   tree_t a = parse_check_and_simplify(T_ENTITY, T_ARCH);
   fail_unless(error_count() == 0);

   bounds_check(a);
   check_expected_errors();
}
END_TEST

Suite *get_bounds_tests(void)
{
   Suite *s = suite_create("bounds");

   TCase *tc_core = nvc_unit_test();
   tcase_add_test(tc_core, test_bounds);
   tcase_add_test(tc_core, test_bounds2);
   tcase_add_test(tc_core, test_case);
   tcase_add_test(tc_core, test_issue36);
   tcase_add_test(tc_core, test_issue54);
   tcase_add_test(tc_core, test_issue99);
   tcase_add_test(tc_core, test_issue150);
   tcase_add_test(tc_core, test_issue200);
   tcase_add_test(tc_core, test_issue208);
   tcase_add_test(tc_core, test_issue247);
   tcase_add_test(tc_core, test_issue269);
   tcase_add_test(tc_core, test_issue307b);
   tcase_add_test(tc_core, test_issue356);
   tcase_add_test(tc_core, test_issue98);
   tcase_add_test(tc_core, test_tc1147);
   tcase_add_test(tc_core, test_aggregate);
   suite_add_tcase(s, tc_core);

   return s;
}
