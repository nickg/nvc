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
      {  31, "array S index -52 outside of POSITIVE range 1 to 10" },
      {  32, "array S index 11 outside of POSITIVE range 1 to 10" },
      {  33, "array S index 0 outside of POSITIVE range 1 to 10" },
      {  37, "aggregate choice index 0 outside of POSITIVE range "
         "1 to 2147483647" },
      {  46, "actual length 4 does not match formal length 8 for parameter X" },
      {  47, "actual length 4 does not match formal length 8 for parameter X" },
      {  50, "actual length 4 for dimension 2 does not" },
      {  53, "value 9 does not match length of target 10 for signal S" },
      {  54, "value 2 does not match length of target 0 for signal N" },
      {  55, "expected at most 0 positional associations in MY_VEC1 " },
      {  60, "length of value 10 does not match length of target 3" },
      {  66, "array S index 11 outside of POSITIVE range 1 to 10" },
      {  67, "array S index -1 outside of POSITIVE range 1 to 10" },
      {  74, "aggregate choice index 5 outside of POSITIVE range 1 to 3" },
      {  74, "aggregate choice index 0 outside of POSITIVE range 1 to 3" },
      {  83, "value '1' outside of ALPHA range 'a' to 'z' for variable A" },
      {  84, "0 outside of POSITIVE range 1 to 2147483647 for variable P" },
      {  89, "invalid dimension 5 for type MY_VEC1" },
      {  94, "value -1 outside of NATURAL range 0 to 2147483647 for "
         "parameter X" },
      { 107, "aggregate choice index 5 outside of POSITIVE range 1 to 3" },
      { 116, "length of sub-aggregate 2 does not match expected length 4" },
      { 137, "array index 14 outside of NATURAL range 0 to 2" },
      { 155, "value 2 outside of REAL range 0 to 1 for parameter X"},
      { 164, "missing choice for element FOUR of T_ARR with index type SE"},
      { 165, "expected at most 3 positional associations in T_ARR aggregate "
        "with index type SE range TWO to FOUR"},
      { 175, "ONE outside of E range THREE downto TWO for parameter ARG2" },
      { 176, "FOUR outside of E range THREE downto TWO for parameter ARG2"},
      { 177, "ONE outside of E range TWO to FOUR for parameter ARG1"},
      { 178, "FIVE outside of E range TWO to FOUR for parameter ARG1"},
      { 188, "aggregate choice index ONE outside of E range TWO to FOUR"},
      { 190, "aggregate choice index ONE outside of E range TWO to FOUR"},
      { 190, "aggregate choice index FIVE outside of E range TWO to FOUR"},
      { 198, "length of sub-aggregate 3 does not match expected length 4" },
      { 206, "left index ONE violates constraint SE" },
      { 206, "right index FOUR violates constraint SE" },
      { 219, "-5 NS outside of TIME range 0 HR to 10 NS for parameter A"},
      { 221, "5 NS outside of TIME range 10 SEC downto 20 US for parameter A"},
      { 227, "200 NS outside of TIME range -10 NS to 10 NS"},
      { 228, "-200 NS outside of TIME range -10 NS to 10 NS"},
      { 236, "array M index 'A' outside of CHARACTER range 'a' to 'z'" },
      { 237, "array M index '1' outside of CHARACTER range 'a' to 'z'" },
      { 237, "array M index '3' outside of CHARACTER range 'a' to 'z'" },
      { -1, NULL }
   };
   expect_errors(expect);

   input_from_file(TESTDIR "/bounds/bounds.vhd");

   tree_t a = parse_and_check(T_ENTITY, T_ARCH);
   fail_unless(error_count() == 0);

   simplify_global(a, NULL, NULL);   // Global to fold TIME expressions
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
      {  52, "value 20 outside of INTEGER range 0 to 9 for signal S"},
      {  53, "'Z' outside of CHARACTER range 'a' to 'z' for constant C1"},
      {  54, "value 10 outside of REAL range 0 to 5"},
      {  55, "value 0 HR outside of TIME range 10 NS to 10 US for constant T"},
      {  56, "value 10 outside of R range 0 to 1 for constant C2"},
      {  59, "value 30 outside of SUBINT range 1 to 10 for parameter A"},
      {  63, "'c' outside of CHARACTER range 'a' to 'b' for parameter A" },
      {  73, "value 2 outside of INTEGER range 0 to 1 for parameter P2"},
      {  70, "value 20 outside of INTEGER range 10 downto 0 for parameter G2"},
      {  79, "value 5.1 outside of REAL range 0 to 5 for variable V2"},
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

   simplify_global(a, NULL, NULL);   // Global to fold TIME expressions
   bounds_check(a);

   check_expected_errors();
}
END_TEST

START_TEST(test_case)
{
   const error_t expect[] = {
      {  13, "missing choice for element C of type LETTER" },
      {  19, "missing choice for element B of type AB" },
      {  30, "10 to 19" },
      {  36, "4 to 2147483647" },
      {  44, "2147483647" },
      {  51, "value 50 is already covered" },
      {  53, "range 60 to 64 is already covered" },
      {  59, "case choice index -1 outside of NATURAL range 0 to 2147483647" },
      {  79, "choices cover only 2 of 8 possible values" },
      {  84, "missing choice for element 3 of BIT_VECTOR with index type "
         "NATURAL range 1 to 3" },
      {  86, "expected at most 3 positional associations in BIT_VECTOR "
         "aggregate with index type NATURAL range 1 to 3" },
      {  88, "expected 3 elements in string literal but have 2" },
      {  90, "expected 3 elements in string literal but have 4" },
      {  95, "choices cover only 2 of 65536 possible values" },
      { 101, "choices cover only 2 of 121 possible values" },
      { 113, "missing choices for elements 3 downto 1 of type MY_INT" },
      { 117, "case choice index 11 outside of MY_INT range 10 downto 1" },
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
      { 10, "index 7 outside of NATURAL range 3 downto 0" },
      { 10, "index 4 outside of NATURAL range 3 downto 0" },
      { 11, "index 3 outside of NATURAL range 7 downto 4" },
      { 11, "index 0 outside of NATURAL range 7 downto 4" },
      { 12, "index 3 outside of NATURAL range 7 downto 4" },
      { 12, "index 0 outside of NATURAL range 7 downto 4" },
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
      {  5, "value 9223372036854775807 outside of INTEGER range -2147483648 "
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
