//
//  Copyright (C) 2021  Nick Gasson
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
#include "exec.h"
#include "vcode.h"
#include "phase.h"

#include <stdlib.h>
#include <inttypes.h>

START_TEST(test_add1)
{
   input_from_file(TESTDIR "/exec/add1.vhd");

   parse_check_simplify_and_lower(T_PACKAGE, T_PACK_BODY);

   exec_t *ex = exec_new(EVAL_FCALL);

   ident_t fn1 = ident_new("WORK.PACK.ADD1(I)I");
   ck_assert_int_eq(exec_call(ex, fn1, NULL, "i", 5).integer, 6);
   ck_assert_int_eq(exec_call(ex, fn1, NULL, "i", INT32_C(-5)).integer, -4);

   ident_t fn2 = ident_new("WORK.PACK.ADD1(R)R");
   ck_assert_double_eq_tol(exec_call(ex, fn2, NULL, "R", 5.0).real, 6.0, 0.001);

   exec_free(ex);
}
END_TEST

START_TEST(test_fact)
{
   input_from_file(TESTDIR "/exec/fact.vhd");

   parse_check_simplify_and_lower(T_PACKAGE, T_PACK_BODY);

   exec_t *ex = exec_new(EVAL_FCALL);

   ident_t fn1 = ident_new("WORK.PACK.FACT(I)I");
   ck_assert_int_eq(exec_call(ex, fn1, NULL, "i", 5).integer, 120);
   ck_assert_int_eq(exec_call(ex, fn1, NULL, "i", 8).integer, 40320);

   ident_t fn2 = ident_new("WORK.PACK.FACT_RECUR(I)I");
   ck_assert_int_eq(exec_call(ex, fn2, NULL, "i", 5).integer, 120);
   ck_assert_int_eq(exec_call(ex, fn2, NULL, "i", 8).integer, 40320);

   exec_free(ex);
}
END_TEST

START_TEST(test_sum)
{
   input_from_file(TESTDIR "/exec/sum.vhd");

   parse_check_simplify_and_lower(T_PACKAGE, T_PACK_BODY);

   exec_t *ex = exec_new(EVAL_FCALL | EVAL_BOUNDS);

   int32_t data[] = { 1, 2, 3, 4, 5 };

   ident_t fn = ident_new("WORK.SUMPKG.SUM(22WORK.SUMPKG.INT_VECTOR)I");
   ck_assert_int_eq(exec_call(ex, fn, NULL, "u", data, 1, 5).integer, 15);
   ck_assert_int_eq(exec_call(ex, fn, NULL, "u", data,
                              5, INT32_C(-5)).integer, 15);
   ck_assert_int_eq(exec_call(ex, fn, NULL, "u", data, 1, 2).integer, 3);
   ck_assert_int_eq(exec_call(ex, fn, NULL, "u", data, 100, 2).integer, 3);
   ck_assert_int_eq(exec_call(ex, fn, NULL, "u", data,
                              INT32_C(-10), 2).integer, 3);
   ck_assert_int_eq(exec_call(ex, fn, NULL, "u", data, 1, 0).integer, 0);

   exec_free(ex);
}
END_TEST

START_TEST(test_context1)
{
   input_from_file(TESTDIR "/exec/context1.vhd");

   parse_check_simplify_and_lower(T_PACKAGE, T_PACK_BODY);

   exec_t *ex = exec_new(EVAL_FCALL);

   void *ctx = exec_link(ex, ident_new("WORK.PACK"));
   fail_if(ctx == NULL);

   ck_assert_int_eq(exec_get_var(ex, ctx, 1).integer, 42);

   ident_t fn1 = ident_new("WORK.PACK.GET_ELT(7NATURAL)I");
   ck_assert_int_eq(exec_call(ex, fn1, ctx, "I", 1).integer, 10);
   ck_assert_int_eq(exec_call(ex, fn1, ctx, "I", 2).integer, 20);

   ident_t fn2 = ident_new("WORK.PACK.NESTED_GET_ELT(7NATURAL)I");
   ck_assert_int_eq(exec_call(ex, fn2, ctx, "I", 1).integer, 10);
   ck_assert_int_eq(exec_call(ex, fn2, ctx, "I", 5).integer, 50);

   exec_free(ex);
}
END_TEST

START_TEST(test_record1)
{
   input_from_file(TESTDIR "/exec/record1.vhd");

   parse_check_simplify_and_lower(T_PACKAGE, T_PACK_BODY,
                                  T_PACKAGE, T_PACK_BODY);

   exec_t *ex = exec_new(EVAL_FCALL);

   eval_frame_t *pack1 = exec_link(ex, ident_new("WORK.PACK"));
   fail_if(pack1 == NULL);

   eval_frame_t *pack2 = exec_link(ex, ident_new("WORK.PACK2"));
   fail_if(pack2 == NULL);

   ident_t fn = ident_new("WORK.PACK2.SUM_FIELDS()I");
   ck_assert_int_eq(exec_call(ex, fn, pack2, "").integer, 6);

   exec_free(ex);
}
END_TEST

START_TEST(test_record2)
{
   input_from_file(TESTDIR "/exec/record2.vhd");

   parse_check_simplify_and_lower(T_PACKAGE, T_PACK_BODY,
                                  T_PACKAGE, T_PACK_BODY);

   exec_t *ex = exec_new(EVAL_FCALL);

   eval_frame_t *pack1 = exec_link(ex, ident_new("WORK.PACK"));
   fail_if(pack1 == NULL);

   eval_frame_t *pack2 = exec_link(ex, ident_new("WORK.PACK2"));
   fail_if(pack2 == NULL);

   ident_t fn = ident_new("WORK.PACK2.SUM_FIELDS()I");
   ck_assert_int_eq(exec_call(ex, fn, pack2, "").integer, 21);

   exec_free(ex);
}
END_TEST

START_TEST(test_record3)
{
   input_from_file(TESTDIR "/exec/record3.vhd");

   parse_check_simplify_and_lower(T_PACKAGE, T_PACK_BODY,
                                  T_PACKAGE, T_PACK_BODY);

   exec_t *ex = exec_new(EVAL_FCALL);

   eval_frame_t *pack1 = exec_link(ex, ident_new("WORK.PACK"));
   fail_if(pack1 == NULL);

   eval_frame_t *pack2 = exec_link(ex, ident_new("WORK.PACK2"));
   fail_if(pack2 == NULL);

   ident_t fn = ident_new("WORK.PACK2.SUM_FIELDS()I");
   ck_assert_int_eq(exec_call(ex, fn, pack2, "").integer, 55);

   exec_free(ex);
}
END_TEST

Suite *get_exec_tests(void)
{
   Suite *s = suite_create("exec");

   TCase *tc = nvc_unit_test();
   tcase_add_test(tc, test_add1);
   tcase_add_test(tc, test_fact);
   tcase_add_test(tc, test_context1);
   tcase_add_test(tc, test_sum);
   tcase_add_test(tc, test_record1);
   tcase_add_test(tc, test_record2);
   tcase_add_test(tc, test_record3);
   suite_add_tcase(s, tc);

   return s;
}
