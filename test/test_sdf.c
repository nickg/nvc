//
//  Copyright (C) 2022-2024  Nick Gasson
//
//  This program is free software: you can redistribute it and/or modify
//  it under the terms of the GNU General Public License as published by
//  the Free Software Foundation, either version 3 of the License, or
//  (at your option) any later version.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.
//
//  You should have received a copy of the GNU General Public License
//  along with this program.  If not, see <http://www.gnu.org/licenses/>.
//

#include "test_util.h"
#include "common.h"
#include "lib.h"
#include "option.h"
#include "phase.h"
#include "sdf/sdf-phase.h"
#include "scan.h"
#include "type.h"

#include <math.h>

#define fail_unless_floats_equal(a, b) fail_unless(fabs(a - b) < 0.00001);

START_TEST(test_parse1)
{
   input_from_file(TESTDIR "/sdf/parse1.sdf");

   sdf_file_t *file = sdf_parse("dummy.sdf", S_F_MIN_MAX_SPEC_ALL);
   ck_assert_ptr_nonnull(file);

   fail_if_errors();
}
END_TEST

START_TEST(test_parse2)
{
   input_from_file(TESTDIR "/sdf/parse2.sdf");

   const error_t expect[] = {
      { 2, "Invalid SDF version: \"FOR_SURE_INVALID_SDF_VERSION\". "
           "SDF version shall contain one of: \"1.0\", \"2.0\", \"2.1\", \"3.0\" or \"4.0\"" },
      { -1, NULL }
   };
   expect_errors(expect);

   sdf_file_t *file = sdf_parse("dummy.sdf", S_F_MIN_MAX_SPEC_ALL);
   ck_assert_ptr_nonnull(file);

   check_expected_errors();
}
END_TEST

START_TEST(test_parse3)
{
   input_from_file(TESTDIR "/sdf/parse3.sdf");

   sdf_file_t *file = sdf_parse("dummy.sdf", S_F_MIN_MAX_SPEC_ALL);
   ck_assert_ptr_nonnull(file);

   fail_if_errors();
}
END_TEST

START_TEST(test_parse4)
{
   input_from_file(TESTDIR "/sdf/parse4.sdf");

   const error_t expect[] = {
      { 6, "Used hierarchy separator: / but hierarchy separator defined in SDF header is: ." },
      { -1, NULL }
   };
   expect_errors(expect);

   sdf_file_t *file = sdf_parse("dummy.sdf", S_F_MIN_MAX_SPEC_ALL);
   ck_assert_ptr_nonnull(file);

   check_expected_errors();
}
END_TEST

START_TEST(test_parse5)
{
   input_from_file(TESTDIR "/sdf/parse5.sdf");

   sdf_file_t *file = sdf_parse("dummy.sdf", S_F_MIN_MAX_SPEC_ALL);
   ck_assert_ptr_nonnull(file);

   fail_if_errors();
}
END_TEST

START_TEST(test_parse6)
{
   input_from_file(TESTDIR "/sdf/parse6.sdf");

   sdf_file_t *file = sdf_parse("dummy.sdf", S_F_MIN_MAX_SPEC_ALL);
   ck_assert_ptr_nonnull(file);

   fail_if_errors();
}
END_TEST

START_TEST(test_parse7)
{
   input_from_file(TESTDIR "/sdf/parse7.sdf");

   sdf_file_t *file = sdf_parse("dummy.sdf", S_F_MIN_MAX_SPEC_ALL);
   ck_assert_ptr_nonnull(file);

   fail_if_errors();
}
END_TEST

START_TEST(test_parse8)
{
   input_from_file(TESTDIR "/sdf/parse8.sdf");

   sdf_file_t *file = sdf_parse("dummy.sdf", S_F_MIN_MAX_SPEC_ALL);
   ck_assert_ptr_nonnull(file);

   fail_if_errors();
}
END_TEST

START_TEST(test_parse9)
{
   input_from_file(TESTDIR "/sdf/parse9.sdf");

   sdf_file_t *file = sdf_parse("dummy.sdf", S_F_MIN_MAX_SPEC_ALL);
   ck_assert_ptr_nonnull(file);

   fail_if_errors();
}
END_TEST

START_TEST(test_parse10)
{
   input_from_file(TESTDIR "/sdf/parse10.sdf");

   sdf_file_t *file = sdf_parse("dummy.sdf", S_F_MIN_MAX_SPEC_ALL);
   ck_assert_ptr_nonnull(file);

   fail_if_errors();
}
END_TEST

START_TEST(test_parse11)
{
   input_from_file(TESTDIR "/sdf/parse11.sdf");

   sdf_file_t *file = sdf_parse("dummy.sdf", S_F_MIN_MAX_SPEC_ALL);
   ck_assert_ptr_nonnull(file);

   fail_if_errors();
}
END_TEST

START_TEST(test_parse12)
{
   input_from_file(TESTDIR "/sdf/parse12.sdf");

   const error_t expect[] = {
      //      { 9, "'delval_list' shall have at most 12 'delval' entries" },
      { -1, NULL }
   };
   expect_errors(expect);

   sdf_file_t *file = sdf_parse("dummy.sdf", S_F_MIN_MAX_SPEC_ALL);
   ck_assert_ptr_nonnull(file);

   check_expected_errors();
}
END_TEST

START_TEST(test_parse13)
{
   input_from_file(TESTDIR "/sdf/parse13.sdf");

   const error_t expect[] = {
      { 4, "unexpected ( while parsing delay file, expecting )" },
      // { 7, "Duplicit header item" },
      { -1, NULL }
   };

   expect_errors(expect);

   sdf_file_t *file = sdf_parse("dummy.sdf", S_F_MIN_MAX_SPEC_ALL);
   ck_assert_ptr_nonnull(file);

   check_expected_errors();
}
END_TEST

START_TEST(test_parse14)
{
   input_from_file(TESTDIR "/sdf/parse14.sdf");

   sdf_file_t *file = sdf_parse("dummy.sdf", S_F_MIN_MAX_SPEC_ALL);
   ck_assert_ptr_nonnull(file);

   fail_if_errors();
}
END_TEST

START_TEST(test_parse15)
{
   input_from_file(TESTDIR "/sdf/parse15.sdf");

   sdf_file_t *file = sdf_parse("dummy.sdf", S_F_MIN_MAX_SPEC_ALL);
   ck_assert_ptr_nonnull(file);

   fail_if_errors();
}
END_TEST

START_TEST(test_parse16)
{
   input_from_file(TESTDIR "/sdf/parse16.sdf");

   sdf_file_t *file = sdf_parse("dummy.sdf", S_F_MIN_MAX_SPEC_ALL);
   ck_assert_ptr_nonnull(file);

   fail_if_errors();
}
END_TEST

START_TEST(test_parse17)
{
   input_from_file(TESTDIR "/sdf/parse17.sdf");

   sdf_file_t *file = sdf_parse("dummy.sdf", S_F_MIN_MAX_SPEC_ALL);
   ck_assert_ptr_nonnull(file);

   fail_if_errors();
}
END_TEST

START_TEST(test_parse18)
{
   input_from_file(TESTDIR "/sdf/parse18.sdf");

   sdf_file_t *file = sdf_parse("dummy.sdf", S_F_MIN_MAX_SPEC_ALL);
   ck_assert_ptr_nonnull(file);

   fail_if_errors();
}
END_TEST

START_TEST(test_parse19)
{
   input_from_file(TESTDIR "/sdf/parse19.sdf");

   sdf_file_t *file = sdf_parse("dummy.sdf", S_F_MIN_MAX_SPEC_ALL);
   ck_assert_ptr_nonnull(file);

   fail_if_errors();
}
END_TEST

START_TEST(test_parse20)
{
   input_from_file(TESTDIR "/sdf/parse20.sdf");

   sdf_file_t *file = sdf_parse("dummy.sdf", S_F_MIN_MAX_SPEC_ALL);
   ck_assert_ptr_nonnull(file);

   fail_if_errors();
}
END_TEST

START_TEST(test_parse21)
{
   input_from_file(TESTDIR "/sdf/parse21.sdf");

   sdf_file_t *file = sdf_parse("dummy.sdf", S_F_MIN_MAX_SPEC_ALL);
   ck_assert_ptr_nonnull(file);

   fail_if_errors();
}
END_TEST

START_TEST(test_parse22)
{
   input_from_file(TESTDIR "/sdf/parse22.sdf");

   sdf_file_t *file = sdf_parse("dummy.sdf", S_F_MIN_MAX_SPEC_ALL);
   ck_assert_ptr_nonnull(file);

   fail_if_errors();
}
END_TEST

START_TEST(test_parse23)
{
   input_from_file(TESTDIR "/sdf/parse23.sdf");

   sdf_file_t *file = sdf_parse("dummy.sdf", S_F_MIN_MAX_SPEC_ALL);
   ck_assert_ptr_nonnull(file);

   fail_if_errors();
}
END_TEST

START_TEST(test_parse24)
{
   input_from_file(TESTDIR "/sdf/parse24.sdf");

   const error_t expect[] = {
      { 8, "'rtripple' shall have at least one number specified"},
      { 12, "'tripple' shall have at least one number specified"},
      { -1, NULL }
   };
   expect_errors(expect);

   sdf_file_t *file = sdf_parse("dummy.sdf", S_F_MIN_MAX_SPEC_ALL);
   ck_assert_ptr_nonnull(file);

   check_expected_errors();
}
END_TEST

Suite *get_sdf_tests(void)
{
   Suite *s = suite_create("sdf");

   TCase *tc_core = nvc_unit_test();
   tcase_add_test(tc_core, test_parse1);
   tcase_add_test(tc_core, test_parse2);
   tcase_add_test(tc_core, test_parse3);
   tcase_add_test(tc_core, test_parse4);
   tcase_add_test(tc_core, test_parse5);
   tcase_add_test(tc_core, test_parse6);
   tcase_add_test(tc_core, test_parse7);
   tcase_add_test(tc_core, test_parse8);
   tcase_add_test(tc_core, test_parse9);
   tcase_add_test(tc_core, test_parse10);
   tcase_add_test(tc_core, test_parse11);
   tcase_add_test(tc_core, test_parse12);
   tcase_add_test(tc_core, test_parse13);
   tcase_add_test(tc_core, test_parse14);
   tcase_add_test(tc_core, test_parse15);
   tcase_add_test(tc_core, test_parse16);
   tcase_add_test(tc_core, test_parse17);
   tcase_add_test(tc_core, test_parse18);
   tcase_add_test(tc_core, test_parse19);
   tcase_add_test(tc_core, test_parse20);
   tcase_add_test(tc_core, test_parse21);
   tcase_add_test(tc_core, test_parse22);
   tcase_add_test(tc_core, test_parse23);
   tcase_add_test(tc_core, test_parse24);
   suite_add_tcase(s, tc_core);

   return s;
}
