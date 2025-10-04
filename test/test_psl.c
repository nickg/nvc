//
//  Copyright (C) 2022-2025  Nick Gasson
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
      { 28, "FOO already declared in this region" },
      { 34, "no visible declaration for XXXX" },
      { 41, "no visible declaration for XXX" },
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
   fail_unless(tree_kind(s0) == T_PSL_DIRECT);

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
      { 12, "expression must be a PSL Boolean but have type INTEGER" },
      { 16, "expression must be a PSL Number but have type BIT" },
      { 17, "expression must be static" },
      { 19, "expression must be a PSL Boolean but have type INTEGER" },
      { 20, "property is not in the simple subset as the left hand side of "
        "this implication is non-Boolean" },
      { 21, "property is not in the simple subset as the right hand side of "
        "this overlapping until operator is non-Boolean" },
      { 22, "property is not in the simple subset as the left hand side of "
        "this until operator is non-Boolean" },
      { 23, "property is not in the simple subset as the operand of this "
        "next_e operator is non-Boolean" },
      { 25, "property is not in the simple subset as the left hand side of "
        "this implication is non-Boolean" },
      { 26, "property is not in the simple subset as the right hand side of "
        "this implication is non-Boolean" },
      { 27, "property is not in the simple subset as the operand of "
        "eventually! is not a Boolean or Sequence" },
      { 28, "property is not in the simple subset as the operand of never "
        "is not a Boolean or Sequence" },
      { 29, "no visible declaration for BAD" },
      { 30, "no visible declaration for BAD" },
      { 32, "no visible declaration for F" },
      { 33, "property is not in the simple subset as the left hand side of "
        "this before operator is non-Boolean" },
      { 34, "property is not in the simple subset as the right hand side of "
        "this before operator is non-Boolean" },
      { 35, "left hand side of suffix implication operator must be a "
        "Sequence" },
      { 36, "property is not in the simple subset as both operands of this "
        "logical OR are non-Boolean" },
      { 37, "operand of non-consecutive repetition operator must be Boolean" },
      { 38, "operand of goto repetition operator must be Boolean" },
      { 39, "expression must be a PSL Number but have type BIT" },
      { 40, "no visible declaration for FOO" },
      { 40, "no visible declaration for Z" },
      { 41, "PSL endpoint declarations are not supported" },
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
   psl_node_t p0_v = psl_value(p0);
   fail_unless(psl_kind(p0_v) == P_CLOCKED);

   psl_node_t p1 = tree_psl(tree_stmt(a, 7));
   fail_unless(psl_kind(p1) == P_COVER);

   psl_node_t p2 = tree_psl(tree_stmt(a, 9));
   fail_unless(psl_kind(p2) == P_ASSUME);

   psl_node_t p3 = tree_psl(tree_stmt(a, 12));
   fail_unless(psl_kind(p3) == P_RESTRICT);

   psl_node_t p4 = tree_psl(tree_stmt(a, 15));
   fail_unless(psl_kind(p4) == P_FAIRNESS);

   fail_if_errors();
}
END_TEST

START_TEST(test_parse4)
{
   opt_set_int(OPT_PSL_COMMENTS, 1);

   input_from_file(TESTDIR "/psl/parse4.vhd");

   const error_t expect[] = {
      { 54, "P1 is not a PSL sequence" },
      { 60, "unexpected ; while parsing architecture statement part" },
      { -1, NULL }
   };
   expect_errors(expect);

   tree_t a = parse_and_check(T_ENTITY, T_ARCH);

   psl_node_t p0 = tree_psl(tree_decl(a, 5));
   fail_unless(psl_kind(p0) == P_SEQUENCE_DECL);

   psl_node_t p1 = tree_psl(tree_decl(a, 6));
   fail_unless(psl_kind(p1) == P_SEQUENCE_DECL);

   psl_node_t p3 = tree_psl(tree_stmt(a, 0));
   psl_node_t p3_v = psl_value(psl_value(p3));
   fail_unless(psl_kind(p3) == P_COVER);
   fail_unless(psl_kind(p3_v) == P_SEQUENCE_INST);

   psl_node_t p6 = tree_psl(tree_stmt(a, 3));
   psl_node_t p6_v = psl_value(psl_value(p6));
   fail_unless(psl_kind(p6_v) == P_REPEAT);
   fail_unless(psl_subkind(p6_v) == PSL_TIMES_REPEAT);

   psl_node_t p7 = tree_psl(tree_stmt(a, 4));
   psl_node_t p7_v_3 = psl_value(psl_value(p7));
   fail_unless(psl_kind(p7_v_3) == P_REPEAT);
   fail_unless(psl_subkind(p7_v_3) == PSL_TIMES_REPEAT);
   fail_unless(tree_ival(psl_tree(psl_delay(p7_v_3))) == 3);

   psl_node_t p7_v_2 = psl_value(p7_v_3);
   fail_unless(psl_kind(p7_v_2) == P_REPEAT);
   fail_unless(psl_subkind(p7_v_2) == PSL_TIMES_REPEAT);
   fail_unless(tree_ival(psl_tree(psl_delay(p7_v_2))) == 2);

   psl_node_t p7_v_1 = psl_value(p7_v_2);
   fail_unless(psl_kind(p7_v_1) == P_REPEAT);
   fail_unless(psl_subkind(p7_v_1) == PSL_TIMES_REPEAT);
   fail_unless(tree_ival(psl_tree(psl_delay(p7_v_1))) == 1);

   psl_node_t p11 = tree_psl(tree_stmt(a, 8));
   psl_node_t p11_v = psl_value(psl_value(p11));

   fail_unless(psl_kind(p11_v) == P_REPEAT);
   fail_unless(tree_ival(psl_tree(psl_left(psl_delay(p11_v)))) == 5);
   fail_unless(tree_ival(psl_tree(psl_right(psl_delay(p11_v)))) == 10);

   psl_node_t p9 = tree_psl(tree_stmt(a, 9));
   psl_node_t p9_v = psl_value(psl_value(p9));

   fail_unless(psl_kind(p9_v) == P_REPEAT);
   fail_unless(psl_subkind(p9_v) == PSL_PLUS_REPEAT);

   psl_node_t p14 = tree_psl(tree_stmt(a, 14));
   fail_unless(psl_kind(p14) == P_COVER);
   psl_node_t p14_v = psl_value(psl_value(p14));
   fail_unless(psl_kind(p14_v) == P_PROC_BLOCK);
   fail_unless(tree_kind(psl_tree(p14_v)) == T_SEQUENCE);
   fail_unless(psl_kind(psl_value(p14_v)) == P_HDL_EXPR);

   check_expected_errors();
}
END_TEST

START_TEST(test_parse5)
{
   opt_set_int(OPT_PSL_COMMENTS, 1);

   input_from_file(TESTDIR "/psl/parse5.vhd");

   tree_t a = parse_and_check(T_ENTITY, T_ARCH);

   psl_node_t p0 = tree_psl(tree_stmt(a, 0));
   psl_node_t p0_v = psl_value(psl_value(p0));
   fail_unless(psl_kind(p0) == P_COVER);
   fail_unless(psl_kind(p0_v) == P_SERE);
   fail_unless(psl_subkind(p0_v) == PSL_SERE_FUSION);
   fail_unless(psl_operands(p0_v) == 4);

   psl_node_t p1 = tree_psl(tree_stmt(a, 1));
   psl_node_t p1_v = psl_value(psl_value(p1));
   psl_node_t p1_v0 = psl_operand(p1_v, 0);
   psl_node_t p1_v1 = psl_operand(p1_v, 1);
   fail_unless(psl_kind(p1_v) == P_SERE);
   fail_unless(psl_subkind(p1_v) == PSL_SERE_CONCAT);
   fail_unless(psl_kind(p1_v0) == P_HDL_EXPR);
   fail_unless(psl_kind(p1_v1) == P_SERE);

   psl_node_t p2 = tree_psl(tree_stmt(a, 2));
   psl_node_t p2_v = psl_value(psl_value(p2));
   fail_unless(psl_kind(p2_v) == P_SERE);
   fail_unless(psl_subkind(p2_v) == PSL_SERE_OR);

   psl_node_t p3 = tree_psl(tree_stmt(a, 3));
   psl_node_t p3_v = psl_value(psl_value(p3));
   psl_node_t p3_v0 = psl_operand(p3_v, 0);
   psl_node_t p3_v1 = psl_operand(p3_v, 1);
   fail_unless(psl_kind(p3_v) == P_SERE);
   fail_unless(psl_subkind(p3_v) == PSL_SERE_OR);
   fail_unless(psl_kind(p3_v0) == P_REPEAT);
   fail_unless(psl_kind(p3_v1) == P_REPEAT);
   fail_unless(psl_subkind(p3_v0) == PSL_TIMES_REPEAT);

   psl_node_t p4 = tree_psl(tree_stmt(a, 4));
   psl_node_t p4_v = psl_value(psl_value(p4));
   psl_node_t p4_v0 = psl_operand(p4_v, 0);
   fail_unless(psl_kind(p4_v) == P_SERE);
   fail_unless(psl_subkind(p4_v) == PSL_SERE_OR);
   fail_unless(psl_subkind(p4_v0) == PSL_SERE_CONCAT);

   psl_node_t p5 = tree_psl(tree_stmt(a, 5));
   psl_node_t p5_v = psl_value(psl_value(p5));
   fail_unless(psl_kind(p5_v) == P_SERE);
   fail_unless(psl_subkind(p5_v) == PSL_SERE_WITHIN);

   psl_node_t p6 = tree_psl(tree_stmt(a, 6));
   psl_node_t p6_v = psl_value(psl_value(p6));
   fail_unless(psl_kind(p6_v) == P_SERE);
   fail_unless(psl_subkind(p6_v) == PSL_SERE_EQU_AND);

   psl_node_t p7 = tree_psl(tree_stmt(a, 7));
   psl_node_t p7_v = psl_value(psl_value(p7));
   fail_unless(psl_kind(p7_v) == P_SERE);
   fail_unless(psl_subkind(p7_v) == PSL_SERE_NEQ_AND);

   fail_if_errors();
}
END_TEST

START_TEST(test_issue910)
{
   opt_set_int(OPT_PSL_COMMENTS, 1);

   input_from_file(TESTDIR "/psl/issue910.vhd");

   tree_t a = parse_and_check(T_ENTITY, T_ARCH);

   psl_node_t p0 = tree_psl(tree_stmt(a, 0));
   fail_unless(psl_kind(p0) == P_COVER);

   psl_node_t p0_clk = psl_value(p0);
   fail_unless(psl_kind(p0_clk) == P_CLOCKED);
   fail_unless(psl_kind(psl_value(p0_clk)) == P_HDL_EXPR);

   psl_node_t p1 = tree_psl(tree_stmt(a, 0));
   fail_unless(psl_kind(p1) == P_COVER);

   psl_node_t p1_clk = psl_value(p1);
   fail_unless(psl_kind(p1_clk) == P_CLOCKED);
   fail_unless(psl_kind(psl_value(p1_clk)) == P_HDL_EXPR);

   fail_if_errors();
}
END_TEST

START_TEST(test_issue1001)
{
   set_standard(STD_08);

   input_from_file(TESTDIR "/psl/issue1001.vhd");

   const error_t expect[] = {
      { 15, "no matching subprogram ONEHOT [BIT return BOOLEAN | BIT]" },
      { 16, "type of actual BIT does not match formal V type BIT_VECTOR" },
      { -1, NULL }
   };
   expect_errors(expect);

   parse_and_check(T_ENTITY, T_ARCH);

   check_expected_errors();
}
END_TEST

START_TEST(test_clock1)
{
   opt_set_int(OPT_PSL_COMMENTS, 1);

   input_from_file(TESTDIR "/psl/clock1.vhd");

   const error_t expect[] = {
      {  9, "sorry, unclocked properties are not supported" },
      { 11, "sorry, clock expressions are only supported at the outermost" },
      { 12, "expression must be a PSL Boolean but have type "
        "universal_integer" },
      { -1, NULL }
   };
   expect_errors(expect);

   parse_and_check(T_ENTITY, T_ARCH);

   check_expected_errors();
}
END_TEST

START_TEST(test_sem2)
{
   opt_set_int(OPT_PSL_COMMENTS, 1);

   input_from_file(TESTDIR "/psl/sem2.vhd");

   const error_t expect[] = {
      { 13, "expression must be a PSL Bit but have type INTEGER" },
      { 16, "expression must be static" },
      { 17, "PSL union operands must be the same type" },
      {  0, "have BIT and INTEGER" },
      { 18, "no visible declaration for XXXX" },
      { 19, "left bound of PSL range (5) must be lower than right bound (3)"},
      { -1, NULL }
   };
   expect_errors(expect);

   parse_and_check(T_ENTITY, T_ARCH);

   check_expected_errors();
}
END_TEST

Suite *get_psl_tests(void)
{
   Suite *s = suite_create("psl");

   TCase *tc_core = nvc_unit_test();
   tcase_add_test(tc_core, test_parse1);
   tcase_add_test(tc_core, test_parse2);
   tcase_add_test(tc_core, test_sem1);
   tcase_add_test(tc_core, test_parse3);
   tcase_add_test(tc_core, test_parse4);
   tcase_add_test(tc_core, test_parse5);
   tcase_add_test(tc_core, test_issue910);
   tcase_add_test(tc_core, test_issue1001);
   tcase_add_test(tc_core, test_clock1);
   tcase_add_test(tc_core, test_sem2);
   suite_add_tcase(s, tc_core);

   return s;
}
