//
//  Copyright (C) 2022-2023  Nick Gasson
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
#include "psl/psl-node.h"
#include "psl/psl-phase.h"
#include "scan.h"

START_TEST(test_parse1)
{
   opt_set_int(OPT_PSL_COMMENTS, 1);

   input_from_file(TESTDIR "/psl/parse1.vhd");

   const error_t expect[] = {
      { 12, "no visible declaration for E" },
      { 19, "no visible declaration for FFF" },
      { -1, NULL }
   };
   expect_errors(expect);

   tree_t e = parse();
   fail_if(e == NULL);
   fail_unless(tree_kind(e) == T_ENTITY);

   lib_put(lib_work(), e);

   tree_t a = parse();
   fail_if(a == NULL);
   fail_unless(tree_kind(a) == T_ARCH);

   fail_unless(parse() == NULL);

   check_expected_errors();
}
END_TEST

START_TEST(test_parse2)
{
   set_standard(STD_08);
   input_from_file(TESTDIR "/psl/parse2.vhd");

   const error_t expect[] = {
      { 12, "no visible declaration for E" },
      { -1, NULL }
   };
   expect_errors(expect);

   tree_t e = parse();
   fail_if(e == NULL);
   fail_unless(tree_kind(e) == T_ENTITY);

   lib_put(lib_work(), e);

   tree_t a = parse();
   fail_if(a == NULL);
   fail_unless(tree_kind(a) == T_ARCH);

   tree_t s0 = tree_stmt(a, 0);
   fail_unless(tree_kind(s0) == T_PSL);

   psl_node_t p0 = tree_psl(s0);
   fail_unless(psl_kind(p0) == P_ASSERT);

   tree_t s3 = tree_stmt(a, 3);
   fail_unless(tree_kind(s3) == T_CONCURRENT);

   tree_t s3a = tree_stmt(s3, 0);
   fail_unless(tree_kind(s3a) == T_ASSERT);

   fail_unless(parse() == NULL);

   check_expected_errors();
}
END_TEST

START_TEST(test_sem1)
{
   opt_set_int(OPT_PSL_COMMENTS, 1);

   input_from_file(TESTDIR "/psl/sem1.vhd");

   const error_t expect[] = {
      { -1, NULL }
   };
   expect_errors(expect);

   parse_and_check(T_ENTITY, T_ARCH);

   check_expected_errors();
}
END_TEST

START_TEST(test_parse3)
{
   opt_set_int(OPT_PSL_COMMENTS, 1);

   input_from_file(TESTDIR "/psl/parse3.vhd");

   tree_t a = parse_and_check(T_ENTITY, T_ARCH);

   psl_node_t p0 = tree_psl(tree_stmt(a, 1));
   fail_unless(psl_kind(p0) == P_ASSERT);
   fail_unless(psl_has_clock(psl_value(p0)));

   fail_if_errors();
}
END_TEST

START_TEST(test_dump)
{
   opt_set_int(OPT_PSL_COMMENTS, 1);

   input_from_file(TESTDIR "/psl/parse3.vhd");

   tree_t a = parse_and_check(T_ENTITY, T_ARCH);

   LOCAL_TEXT_BUF tb = tb_new();
   capture_syntax(tb);

   psl_dump(tree_psl(tree_stmt(a, 0)));
   ck_assert_str_eq(tb_get(tb), "default clock is \"and\"(CLK'EVENT, \"=\"(CLK, '1'))");
   tb_rewind(tb);

   psl_dump(tree_psl(tree_stmt(a, 1)));
   ck_assert_str_eq(tb_get(tb), "assert never B");
   tb_rewind(tb);

   psl_dump(tree_psl(tree_stmt(a, 6)));
   ck_assert_str_eq(tb_get(tb), "assert {A; \"and\"(B, C)}");
   tb_rewind(tb);

   fail_if_errors();
}

Suite *get_psl_tests(void)
{
   Suite *s = suite_create("psl");

   TCase *tc_core = nvc_unit_test();
   tcase_add_test(tc_core, test_parse1);
   tcase_add_test(tc_core, test_parse2);
   tcase_add_test(tc_core, test_sem1);
   tcase_add_test(tc_core, test_parse3);
   tcase_add_test(tc_core, test_dump);
   suite_add_tcase(s, tc_core);

   return s;
}
