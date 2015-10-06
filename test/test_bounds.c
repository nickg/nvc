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
      {  26, "left index 0 violates constraint STD.STANDARD.POSITIVE" },
      {  27, "right index 60 violates constraint FOO" },
      {  31, "array S index -52 out of bounds 1 to 10" },
      {  32, "slice right index 11 out of bounds 1 to 10" },
      {  33, "slice left index 0 out of bounds 1 to 10" },
      {  37, "aggregate index 0 out of bounds 1 to 2147483647" },
      {  46, "actual length 8 does not match formal length 4" },
      {  47, "actual length 8 does not match formal length 4" },
      {  50, "actual length 3 for dimension 2 does not" },
      {  53, "length of value 9 does not match length of target 10" },
      {  54, "length of value 2 does not match length of target 0" },
      {  55, "expected 0 elements in aggregate but have 3" },
      {  60, "length of value 10 does not match length of target 3" },
      {  66, "array S index 11 out of bounds 1 to 10" },
      {  67, "array S index -1 out of bounds 1 to 10" },
      {  74, "expected 6 elements in aggregate but have 3" },
      {  74, "length of value 6 does not match length of target 3" },
      {  83, "value '1' out of target bounds 'a' to 'z'" },
      {  84, "value 0 out of target bounds 1 to 2147483647" },
      {  89, "invalid dimension 5 for type MY_VEC1" },
      {  94, "value -1 out of bounds 0 to 2147483647 for parameter X" },
      { 107, "aggregate index 5 out of bounds 1 to 3" },
      { 116, "length of sub-aggregate 2 does not match expected length 4" },
      { 137, "array index 14 out of bounds 0 to 2" },
      { 155, "value 2.000000 out of bounds 0.000000 to 1.000000 for parameter"},
      { -1, NULL }
   };
   expect_errors(expect);

   input_from_file(TESTDIR "/bounds/bounds.vhd");

   tree_t a = parse_and_check(T_ENTITY, T_ARCH);
   fail_unless(sem_errors() == 0);

   simplify(a);
   bounds_check(a);

   fail_unless(bounds_errors() == (sizeof(expect) / sizeof(error_t)) - 1);
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
      {  59, "value -1 outside STD.STANDARD.NATURAL bounds" },
      {  58, "0 to 2147483647" },
      {  79, "choices cover only 2 of 8 possible values" },
      {  84, "expected 3 elements in aggregate but have 2" },
      {  86, "expected 3 elements in aggregate but have 4" },
      {  88, "expected 3 elements in string literal but have 2" },
      {  90, "expected 3 elements in string literal but have 4" },
      {  95, "choices do not cover all possible values" },
      { 101, "choices cover only 2 of 100 possible values" },
      { -1, NULL }
   };
   expect_errors(expect);

   input_from_file(TESTDIR "/bounds/case.vhd");

   tree_t a = parse_and_check(T_ENTITY, T_ARCH);
   fail_unless(sem_errors() == 0);

   simplify(a);
   bounds_check(a);

   fail_unless(bounds_errors() == (sizeof(expect) / sizeof(error_t)) - 1);
}
END_TEST

START_TEST(test_issue36)
{
   input_from_file(TESTDIR "/bounds/issue36.vhd");

   tree_t e = parse_and_check(T_ENTITY);
   fail_unless(sem_errors() == 0);

   simplify(e);
   bounds_check(e);

   fail_unless(bounds_errors() == 0);
}
END_TEST

START_TEST(test_issue54)
{
   const error_t expect[] = {
      { 12, "aggregate index 3 out of bounds 7 downto 4" },
      { 12, "aggregate index 0 out of bounds 7 downto 4" },
      { -1, NULL }
   };
   expect_errors(expect);

   input_from_file(TESTDIR "/bounds/issue54.vhd");

   tree_t a = parse_and_check(T_ENTITY, T_ARCH);
   fail_unless(sem_errors() == 0);

   simplify(a);
   bounds_check(a);

   fail_unless(bounds_errors() == (sizeof(expect) / sizeof(error_t)) - 1);
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
   fail_unless(sem_errors() == 0);

   simplify(a);
   bounds_check(a);

   fail_unless(bounds_errors() == (sizeof(expect) / sizeof(error_t)) - 1);
}
END_TEST

START_TEST(test_issue150)
{
   const error_t expect[] = {
      { 10, "expected 8 elements in aggregate but have 6" },
      { -1, NULL }
   };
   expect_errors(expect);

   input_from_file(TESTDIR "/bounds/issue150.vhd");

   tree_t a = parse_and_check(T_ENTITY, T_ARCH);
   fail_unless(sem_errors() == 0);

   simplify(a);
   bounds_check(a);

   fail_unless(bounds_errors() == (sizeof(expect) / sizeof(error_t)) - 1);
}
END_TEST

START_TEST(test_issue200)
{
   input_from_file(TESTDIR "/bounds/issue200.vhd");

   tree_t a = parse_and_check(T_ENTITY, T_ARCH);
   fail_unless(sem_errors() == 0);

   simplify(a);
   bounds_check(a);

   fail_unless(bounds_errors() == 0);
}
END_TEST

START_TEST(test_issue208)
{
   const error_t expect[] = {
      { 20, "case choices do not cover the following values of " },
      { -1, NULL }
   };
   expect_errors(expect);

   input_from_file(TESTDIR "/bounds/issue208.vhd");

   tree_t a = parse_and_check(T_ENTITY, T_ARCH);
   fail_unless(sem_errors() == 0);

   simplify(a);
   bounds_check(a);

   fail_unless(bounds_errors() == ARRAY_LEN(expect) - 1);
}
END_TEST

int main(void)
{
   Suite *s = suite_create("bounds");

   TCase *tc_core = nvc_unit_test();
   tcase_add_test(tc_core, test_bounds);
   tcase_add_test(tc_core, test_case);
   tcase_add_test(tc_core, test_issue36);
   tcase_add_test(tc_core, test_issue54);
   tcase_add_test(tc_core, test_issue99);
   tcase_add_test(tc_core, test_issue150);
   tcase_add_test(tc_core, test_issue200);
   tcase_add_test(tc_core, test_issue208);
   suite_add_tcase(s, tc_core);

   return nvc_run_test(s);
}
