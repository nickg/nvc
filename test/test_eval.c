//
//  Copyright (C) 2021-2022  Nick Gasson
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
#include "eval.h"
#include "phase.h"
#include "scan.h"
#include "vcode.h"

#include <stdlib.h>
#include <inttypes.h>

START_TEST(test_add1)
{
   input_from_file(TESTDIR "/eval/add1.vhd");

   parse_check_simplify_and_lower(T_PACKAGE, T_PACK_BODY);

   eval_t *ex = eval_new(EVAL_FCALL);

   ident_t fn1 = ident_new("WORK.PACK.ADD1(I)I");
   ck_assert_int_eq(eval_call(ex, fn1, NULL, "i", 5).integer, 6);
   ck_assert_int_eq(eval_call(ex, fn1, NULL, "i", INT32_C(-5)).integer, -4);

   ident_t fn2 = ident_new("WORK.PACK.ADD1(R)R");
   ck_assert_double_eq_tol(eval_call(ex, fn2, NULL, "R", 5.0).real, 6.0, 0.001);

   eval_free(ex);
}
END_TEST

START_TEST(test_fact)
{
   input_from_file(TESTDIR "/eval/fact.vhd");

   parse_check_simplify_and_lower(T_PACKAGE, T_PACK_BODY);

   eval_t *ex = eval_new(EVAL_FCALL);

   ident_t fn1 = ident_new("WORK.PACK.FACT(I)I");
   ck_assert_int_eq(eval_call(ex, fn1, NULL, "i", 5).integer, 120);
   ck_assert_int_eq(eval_call(ex, fn1, NULL, "i", 8).integer, 40320);

   ident_t fn2 = ident_new("WORK.PACK.FACT_RECUR(I)I");
   ck_assert_int_eq(eval_call(ex, fn2, NULL, "i", 5).integer, 120);
   ck_assert_int_eq(eval_call(ex, fn2, NULL, "i", 8).integer, 40320);

   eval_free(ex);
}
END_TEST

START_TEST(test_sum)
{
   input_from_file(TESTDIR "/eval/sum.vhd");

   parse_check_simplify_and_lower(T_PACKAGE, T_PACK_BODY);

   eval_t *ex = eval_new(EVAL_FCALL | EVAL_BOUNDS);

   int32_t data[] = { 1, 2, 3, 4, 5 };

   ident_t fn = ident_new("WORK.SUMPKG.SUM(22WORK.SUMPKG.INT_VECTOR)I");
   ck_assert_int_eq(eval_call(ex, fn, NULL, "u", data, 1, 5).integer, 15);
   ck_assert_int_eq(eval_call(ex, fn, NULL, "u", data,
                              5, INT32_C(-5)).integer, 15);
   ck_assert_int_eq(eval_call(ex, fn, NULL, "u", data, 1, 2).integer, 3);
   ck_assert_int_eq(eval_call(ex, fn, NULL, "u", data, 100, 2).integer, 3);
   ck_assert_int_eq(eval_call(ex, fn, NULL, "u", data,
                              INT32_C(-10), 2).integer, 3);
   ck_assert_int_eq(eval_call(ex, fn, NULL, "u", data, 1, 0).integer, 0);

   eval_free(ex);
}
END_TEST

START_TEST(test_context1)
{
   input_from_file(TESTDIR "/eval/context1.vhd");

   parse_check_simplify_and_lower(T_PACKAGE, T_PACK_BODY);

   eval_t *ex = eval_new(EVAL_FCALL | EVAL_WARN);

   void *ctx = eval_link(ex, ident_new("WORK.PACK"));
   fail_if(ctx == NULL);

   ck_assert_int_eq(eval_get_frame_var(ex, ctx, 1).integer, 42);

   ident_t fn1 = ident_new("WORK.PACK.GET_ELT(7NATURAL)I");
   ck_assert_int_eq(eval_call(ex, fn1, ctx, "i", 1).integer, 10);
   ck_assert_int_eq(eval_call(ex, fn1, ctx, "i", 2).integer, 20);

   ident_t fn2 = ident_new("WORK.PACK.NESTED_GET_ELT(7NATURAL)I");
   ck_assert_int_eq(eval_call(ex, fn2, ctx, "i", 1).integer, 10);
   ck_assert_int_eq(eval_call(ex, fn2, ctx, "i", 5).integer, 50);

   eval_free(ex);
}
END_TEST

START_TEST(test_record1)
{
   input_from_file(TESTDIR "/eval/record1.vhd");

   parse_check_simplify_and_lower(T_PACKAGE, T_PACK_BODY,
                                  T_PACKAGE, T_PACK_BODY);

   eval_t *ex = eval_new(EVAL_FCALL);

   eval_frame_t *pack1 = eval_link(ex, ident_new("WORK.PACK1"));
   fail_if(pack1 == NULL);

   eval_frame_t *pack2 = eval_link(ex, ident_new("WORK.PACK2"));
   fail_if(pack2 == NULL);

   ident_t fn = ident_new("WORK.PACK2.SUM_FIELDS()I");
   ck_assert_int_eq(eval_call(ex, fn, pack2, "").integer, 6);

   eval_free(ex);
}
END_TEST

START_TEST(test_record2)
{
   input_from_file(TESTDIR "/eval/record2.vhd");

   parse_check_simplify_and_lower(T_PACKAGE, T_PACK_BODY,
                                  T_PACKAGE, T_PACK_BODY);

   eval_t *ex = eval_new(EVAL_FCALL);

   eval_frame_t *pack3 = eval_link(ex, ident_new("WORK.PACK3"));
   fail_if(pack3 == NULL);

   eval_frame_t *pack4 = eval_link(ex, ident_new("WORK.PACK4"));
   fail_if(pack4 == NULL);

   ident_t fn = ident_new("WORK.PACK4.SUM_FIELDS()I");
   ck_assert_int_eq(eval_call(ex, fn, pack4, "").integer, 21);

   eval_free(ex);
}
END_TEST

START_TEST(test_record3)
{
   input_from_file(TESTDIR "/eval/record3.vhd");

   parse_check_simplify_and_lower(T_PACKAGE, T_PACK_BODY,
                                  T_PACKAGE, T_PACK_BODY);

   eval_t *ex = eval_new(EVAL_FCALL);

   eval_frame_t *pack5 = eval_link(ex, ident_new("WORK.PACK5"));
   fail_if(pack5 == NULL);

   eval_frame_t *pack6 = eval_link(ex, ident_new("WORK.PACK6"));
   fail_if(pack6 == NULL);

   ident_t fn = ident_new("WORK.PACK6.SUM_FIELDS()I");
   ck_assert_int_eq(eval_call(ex, fn, pack6, "").integer, 55);

   eval_free(ex);
}
END_TEST

START_TEST(test_ieee_warnings)
{
   input_from_file(TESTDIR "/eval/ieeewarn.vhd");

   // This should not fold the call to IEEE_WARNINGS
   tree_t top = run_elab();
   tree_t b = tree_stmt(top, 0);

   fail_unless(tree_decls(b) == 2);
   tree_t d1 = tree_decl(b, 1);
   fail_unless(tree_kind(d1) == T_CONST_DECL);
   fail_unless(tree_ident(d1) == ident_new("E"));
   fail_unless(tree_kind(tree_value(d1)) == T_REF);

   eval_t *ex = eval_new(EVAL_FCALL | EVAL_WARN);

   eval_frame_t *pkg = eval_link(ex, ident_new("WORK.IEEEWARN"));
   fail_if(pkg == NULL);

   ck_assert_int_eq(eval_get_frame_var(ex, pkg, 0).integer, 1);

   eval_free(ex);
}
END_TEST

START_TEST(test_overflow)
{
   input_from_file(TESTDIR "/eval/overflow.vhd");

   const error_t expect[] = {
      { 10, "result of 2147483647 + 1 cannot be represented as INTEGER" },
      { 10, "result of 2147483647 + 2147483647 cannot be represented as " },
      { 15, "result of -2147483648 - 53 cannot be represented as INTEGER" },
      { 20, "result of -1942444142 * 128910 cannot be represented as INTEGER" },
      { -1, NULL },
   };
   expect_errors(expect);

   parse_check_simplify_and_lower(T_PACKAGE, T_PACK_BODY);

   eval_t *ex = eval_new(EVAL_FCALL);

   ident_t add = ident_new("WORK.OVERFLOW.ADD(II)I");
   ident_t sub = ident_new("WORK.OVERFLOW.SUB(II)I");
   ident_t mul = ident_new("WORK.OVERFLOW.MUL(II)I");

   eval_scalar_t result;
   fail_if(eval_try_call(ex, add, NULL, &result, "ii", INT32_MAX, 1));
   fail_if(eval_try_call(ex, add, NULL, &result, "ii", INT32_MAX, INT32_MAX));
   fail_if(eval_try_call(ex, sub, NULL, &result, "ii", INT32_MIN, 53));
   fail_if(eval_try_call(ex, mul, NULL, &result, "ii", 2352523154, 128910));

   eval_free(ex);
   check_expected_errors();
}
END_TEST

START_TEST(test_record4)
{
   input_from_file(TESTDIR "/eval/record4.vhd");
   parse_check_simplify_and_lower(T_PACKAGE, T_PACK_BODY);

   eval_t *ex = eval_new(EVAL_FCALL);

   ident_t func1 = ident_new("WORK.PACK5.FUNC1(I)I");
   ident_t func2 = ident_new("WORK.PACK5.FUNC2(I)I");

   ck_assert_int_eq(eval_call(ex, func1, NULL, "i", 5).integer, 40);
   ck_assert_int_eq(eval_call(ex, func1, NULL, "i", 2).integer, 7);
   ck_assert_int_eq(eval_call(ex, func1, NULL, "i", 0).integer, 0);
   ck_assert_int_eq(eval_call(ex, func2, NULL, "i", 5).integer, 40);
   ck_assert_int_eq(eval_call(ex, func2, NULL, "i", 2).integer, 7);
   ck_assert_int_eq(eval_call(ex, func2, NULL, "i", 0).integer, 0);

   eval_free(ex);
   fail_if_errors();
}
END_TEST

Suite *get_eval_tests(void)
{
   Suite *s = suite_create("eval");

   TCase *tc = nvc_unit_test();
   tcase_add_test(tc, test_add1);
   tcase_add_test(tc, test_fact);
   tcase_add_test(tc, test_context1);
   tcase_add_test(tc, test_sum);
   tcase_add_test(tc, test_record1);
   tcase_add_test(tc, test_record2);
   tcase_add_test(tc, test_record3);
   tcase_add_test(tc, test_ieee_warnings);
   tcase_add_test(tc, test_overflow);
   tcase_add_test(tc, test_record4);
   suite_add_tcase(s, tc);

   return s;
}
