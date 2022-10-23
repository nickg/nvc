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
#include "common.h"
#include "diag.h"
#include "ident.h"
#include "jit/jit.h"
#include "jit/jit-ffi.h"
#include "opt.h"
#include "phase.h"
#include "scan.h"
#include "type.h"

#include <math.h>
#include <stdlib.h>

static jit_handle_t compile_for_test(jit_t *j, const char *name)
{
   return jit_lazy_compile(j, ident_new(name));
}

START_TEST(test_add1)
{
   input_from_file(TESTDIR "/jit/add1.vhd");

   parse_check_simplify_and_lower(T_PACKAGE, T_PACK_BODY);

   jit_t *j = jit_new();

   jit_handle_t fn1 = compile_for_test(j, "WORK.PACK.ADD1(I)I");
   ck_assert_int_eq(jit_call(j, fn1, NULL, 5).integer, 6);
   ck_assert_int_eq(jit_call(j, fn1, NULL, INT32_C(-5)).integer, -4);

   jit_handle_t fn2 = compile_for_test(j, "WORK.PACK.ADD1(R)R");
   ck_assert_double_eq_tol(jit_call(j, fn2, NULL, 5.0).real, 6.0, 0.001);

   jit_free(j);

   fail_if_errors();
}
END_TEST

START_TEST(test_fact)
{
   input_from_file(TESTDIR "/jit/fact.vhd");

   parse_check_simplify_and_lower(T_PACKAGE, T_PACK_BODY);

   jit_t *j = jit_new();

   jit_handle_t fn1 = compile_for_test(j, "WORK.PACK.FACT(I)I");
   ck_assert_int_eq(jit_call(j, fn1, NULL, 5).integer, 120);
   ck_assert_int_eq(jit_call(j, fn1, NULL, 8).integer, 40320);

   jit_handle_t fn2 = compile_for_test(j, "WORK.PACK.FACT_RECUR(I)I");
   ck_assert_int_eq(jit_call(j, fn2, NULL, 5).integer, 120);
   ck_assert_int_eq(jit_call(j, fn2, NULL, 8).integer, 40320);

   jit_free(j);
}
END_TEST

START_TEST(test_sum)
{
   input_from_file(TESTDIR "/jit/sum.vhd");

   parse_check_simplify_and_lower(T_PACKAGE, T_PACK_BODY);

   jit_t *j = jit_new();

   int32_t data[] = { 1, 2, 3, 4, 5 };

   jit_handle_t fn1 =
      compile_for_test(j, "WORK.SUMPKG.GET_LEFT(22WORK.SUMPKG.INT_VECTOR)I");
   ck_assert_int_eq(jit_call(j, fn1, NULL, data, 1, 5).integer, 1);
   ck_assert_int_eq(jit_call(j, fn1, NULL, data, -5, 5).integer, -5);

   jit_handle_t fn2 =
      compile_for_test(j, "WORK.SUMPKG.GET_RIGHT(22WORK.SUMPKG.INT_VECTOR)I");
   ck_assert_int_eq(jit_call(j, fn2, NULL, data, 1, 5).integer, 5);
   ck_assert_int_eq(jit_call(j, fn2, NULL, data, -5, 0).integer, -6);
   ck_assert_int_eq(jit_call(j, fn2, NULL, data, -5, 2).integer, -4);

   jit_handle_t fn3 =
      compile_for_test(j, "WORK.SUMPKG.GET_LENGTH(22WORK.SUMPKG.INT_VECTOR)I");
   ck_assert_int_eq(jit_call(j, fn3, NULL, data, 1, 5).integer, 5);
   ck_assert_int_eq(jit_call(j, fn3, NULL, data, -5, 0).integer, 0);
   ck_assert_int_eq(jit_call(j, fn3, NULL, data, -5, 2).integer, 2);

   jit_handle_t fn4 =
      compile_for_test(j, "WORK.SUMPKG.SUM(22WORK.SUMPKG.INT_VECTOR)I");
   ck_assert_int_eq(jit_call(j, fn4, NULL, data, 1, 5).integer, 15);
   ck_assert_int_eq(jit_call(j, fn4, NULL, data, 5, -5).integer, 15);
   ck_assert_int_eq(jit_call(j, fn4, NULL, data, 1, 2).integer, 3);
   ck_assert_int_eq(jit_call(j, fn4, NULL, data, 100, 2).integer, 3);
   ck_assert_int_eq(jit_call(j, fn4, NULL, data, -10, 2).integer, 3);
   ck_assert_int_eq(jit_call(j, fn4, NULL, data, 1, 0).integer, 0);

   jit_free(j);

   fail_if_errors();
}
END_TEST

START_TEST(test_context1)
{
   input_from_file(TESTDIR "/jit/context1.vhd");

   const error_t expect[] = {
      { 16, "index 55 outside of NATURAL range 1 to 5" },
      { 16, "index -1 outside of NATURAL range 1 to 5" },
      { 22, "index -1 outside of NATURAL range 1 to 5" },
      { -1, NULL },
   };
   expect_errors(expect);

   parse_check_simplify_and_lower(T_PACKAGE, T_PACK_BODY);

   jit_t *j = jit_new();

   jit_handle_t handle = jit_lazy_compile(j, ident_new("WORK.PACK"));

   void *ctx = jit_link(j, handle);
   fail_if(ctx == NULL);

   int32_t *c1 = jit_get_frame_var(j, handle, 1);
   ck_assert_int_eq(*c1, 42);

   jit_handle_t fn1 = compile_for_test(j, "WORK.PACK.GET_ELT(7NATURAL)I");
   ck_assert_int_eq(jit_call(j, fn1, ctx, 1).integer, 10);
   ck_assert_int_eq(jit_call(j, fn1, ctx, 2).integer, 20);

   jit_scalar_t result;
   fail_if(jit_try_call(j, fn1, &result, NULL, 55));
   fail_if(jit_try_call(j, fn1, &result, NULL, -1));

   jit_handle_t fn2 =
      compile_for_test(j, "WORK.PACK.NESTED_GET_ELT(7NATURAL)I");
   ck_assert_int_eq(jit_call(j, fn2, ctx, 1).integer, 10);
   ck_assert_int_eq(jit_call(j, fn2, ctx, 5).integer, 50);
   fail_if(jit_try_call(j, fn2, &result, NULL, -1));

   int x;
   jit_handle_t fn3 = compile_for_test(j, "WORK.PACK.READ_ELT(7NATURALI)");
   fail_unless(jit_try_call(j, fn3, &result, NULL, ctx, 1, &x));
   ck_assert_int_eq(x, 10);
   fail_unless(jit_try_call(j, fn3, &result, NULL, ctx, 5, &x));
   ck_assert_int_eq(x, 50);

   jit_free(j);

   check_expected_errors();
}
END_TEST

START_TEST(test_record1)
{
   input_from_file(TESTDIR "/jit/record1.vhd");

   parse_check_simplify_and_lower(T_PACKAGE, T_PACK_BODY,
                                  T_PACKAGE, T_PACK_BODY);

   jit_t *j = jit_new();

   jit_handle_t pack1_handle = jit_lazy_compile(j, ident_new("WORK.PACK1"));
   jit_handle_t pack2_handle = jit_lazy_compile(j, ident_new("WORK.PACK2"));

   eval_frame_t *pack1 = jit_link(j, pack1_handle);
   fail_if(pack1 == NULL);

   eval_frame_t *pack2 = jit_link(j, pack2_handle);
   fail_if(pack2 == NULL);

   jit_handle_t fn = compile_for_test(j, "WORK.PACK2.SUM_FIELDS()I");
   ck_assert_int_eq(jit_call(j, fn, pack2).integer, 6);

   jit_free(j);
}
END_TEST

START_TEST(test_record2)
{
   input_from_file(TESTDIR "/jit/record2.vhd");

   parse_check_simplify_and_lower(T_PACKAGE, T_PACK_BODY,
                                  T_PACKAGE, T_PACK_BODY);

   jit_t *j = jit_new();

   jit_handle_t pack3_handle = jit_lazy_compile(j, ident_new("WORK.PACK3"));
   jit_handle_t pack4_handle = jit_lazy_compile(j, ident_new("WORK.PACK4"));

   eval_frame_t *pack3 = jit_link(j, pack3_handle);
   fail_if(pack3 == NULL);

   eval_frame_t *pack4 = jit_link(j, pack4_handle);
   fail_if(pack4 == NULL);

   jit_handle_t fn = compile_for_test(j, "WORK.PACK4.SUM_FIELDS()I");
   ck_assert_int_eq(jit_call(j, fn, pack4).integer, 21);

   jit_free(j);
}
END_TEST

START_TEST(test_record3)
{
   input_from_file(TESTDIR "/jit/record3.vhd");

   parse_check_simplify_and_lower(T_PACKAGE, T_PACK_BODY,
                                  T_PACKAGE, T_PACK_BODY);

   jit_t *j = jit_new();

   jit_handle_t pack5_handle = jit_lazy_compile(j, ident_new("WORK.PACK5"));
   jit_handle_t pack6_handle = jit_lazy_compile(j, ident_new("WORK.PACK6"));

   eval_frame_t *pack5 = jit_link(j, pack5_handle);
   fail_if(pack5 == NULL);

   eval_frame_t *pack6 = jit_link(j, pack6_handle);
   fail_if(pack6 == NULL);

   jit_handle_t fn = compile_for_test(j, "WORK.PACK6.SUM_FIELDS()I");
   ck_assert_int_eq(jit_call(j, fn, pack6).integer, 55);

   jit_free(j);
}
END_TEST

DLLEXPORT
int _nvc_ieee_warnings(void)
{
   return 1;
}

START_TEST(test_ieee_warnings)
{
   input_from_file(TESTDIR "/jit/ieeewarn.vhd");

   // This should not fold the call to IEEE_WARNINGS
   tree_t top = run_elab();
   tree_t b = tree_stmt(top, 0);

   fail_unless(tree_decls(b) == 2);
   tree_t d1 = tree_decl(b, 1);
   fail_unless(tree_kind(d1) == T_CONST_DECL);
   fail_unless(tree_ident(d1) == ident_new("E"));
   fail_unless(tree_kind(tree_value(d1)) == T_REF);

   jit_t *j = jit_new();

   jit_handle_t handle = jit_compile(j, ident_new("WORK.IEEEWARN"));

   void *pkg = jit_link(j, handle);
   fail_if(pkg == NULL);

   uint8_t *p = jit_get_frame_var(j, handle, 0);
   ck_assert_int_eq(*p, 1);

   jit_free(j);
}
END_TEST

START_TEST(test_overflow)
{
   input_from_file(TESTDIR "/jit/overflow.vhd");

   const error_t expect[] = {
      { 16, "result of 2147483647 + 1 cannot be represented as INTEGER" },
      { 16, "result of 2147483647 + 2147483647 cannot be represented as " },
      { 21, "result of -2147483648 - 53 cannot be represented as INTEGER" },
      { 26, "result of -1942444142 * 128910 cannot be represented as INTEGER" },
      { 31, "result of 255 + 128 cannot be represented as UINT8" },
      { 36, "result of 2 - 3 cannot be represented as UINT8" },
      { 41, "result of 255 * 2 cannot be represented as UINT8" },
      { -1, NULL },
   };
   expect_errors(expect);

   parse_check_simplify_and_lower(T_PACKAGE, T_PACK_BODY);

   jit_t *j = jit_new();

   jit_handle_t add = compile_for_test(j, "WORK.OVERFLOW.ADD(II)I");
   jit_handle_t sub = compile_for_test(j, "WORK.OVERFLOW.SUB(II)I");
   jit_handle_t mul = compile_for_test(j, "WORK.OVERFLOW.MUL(II)I");

   jit_scalar_t result;
   fail_if(jit_try_call(j, add, &result, NULL, INT32_MAX, 1));
   fail_if(jit_try_call(j, add, &result, NULL, INT32_MAX, INT32_MAX));
   fail_if(jit_try_call(j, sub, &result, NULL, INT32_MIN, 53));
   fail_if(jit_try_call(j, mul, &result, NULL, 2352523154, 128910));

#define UINT8 "19WORK.OVERFLOW.UINT8"
   jit_handle_t addu =
      compile_for_test(j, "WORK.OVERFLOW.ADD(" UINT8 UINT8 ")" UINT8);
   jit_handle_t subu =
      compile_for_test(j, "WORK.OVERFLOW.SUB(" UINT8 UINT8 ")" UINT8);
   jit_handle_t mulu =
      compile_for_test(j, "WORK.OVERFLOW.MUL(" UINT8 UINT8 ")" UINT8);
#undef UINT8

   ck_assert_int_eq(jit_call(j, addu, NULL, 5, 6).integer, 11);
   ck_assert_int_eq(jit_call(j, addu, NULL, 127, 1).integer, 128);
   fail_if(jit_try_call(j, addu, &result, NULL, 255, 128));

   ck_assert_int_eq(jit_call(j, subu, NULL, 255, 4).integer, 251);
   ck_assert_int_eq(jit_call(j, subu, NULL, 1, 1).integer, 0);
   fail_if(jit_try_call(j, subu, &result, NULL, 2, 3));

   ck_assert_int_eq(jit_call(j, mulu, NULL, 127, 2).integer, 254);
   fail_if(jit_try_call(j, mulu, &result, NULL, 255, 2));

   jit_free(j);
   check_expected_errors();
}
END_TEST

START_TEST(test_record4)
{
   input_from_file(TESTDIR "/jit/record4.vhd");

   parse_check_simplify_and_lower(T_PACKAGE, T_PACK_BODY);

   jit_t *j = jit_new();

   jit_handle_t func1 = compile_for_test(j, "WORK.PACK5.FUNC1(I)I");
   jit_handle_t func2 = compile_for_test(j, "WORK.PACK5.FUNC2(I)I");
   jit_handle_t func3 = compile_for_test(j, "WORK.PACK5.FUNC3(I)I");
   jit_handle_t func4 = compile_for_test(j, "WORK.PACK5.FUNC4(I)I");

   ck_assert_int_eq(jit_call(j, func1, NULL, 5).integer, 40);
   ck_assert_int_eq(jit_call(j, func1, NULL, 2).integer, 7);
   ck_assert_int_eq(jit_call(j, func1, NULL, 0).integer, 0);
   ck_assert_int_eq(jit_call(j, func2, NULL, 5).integer, 40);
   ck_assert_int_eq(jit_call(j, func2, NULL, 2).integer, 7);
   ck_assert_int_eq(jit_call(j, func2, NULL, 0).integer, 0);
   ck_assert_int_eq(jit_call(j, func3, NULL, 2).integer, 6);
   ck_assert_int_eq(jit_call(j, func4, NULL, 3).integer, 27);

   jit_free(j);
   fail_if_errors();
}
END_TEST

START_TEST(test_access1)
{
   input_from_file(TESTDIR "/jit/access1.vhd");

   const error_t expect[] = {
      { 16, "null access dereference" },
      { 38, "out of memory attempting to allocate 1032 byte " },
      { -1, NULL },
   };
   expect_errors(expect);

   parse_check_simplify_and_lower(T_PACKAGE, T_PACK_BODY);

   jit_t *j = jit_new();

   int32_t *p LOCAL = xmalloc_array(2, sizeof(int32_t));
   p[0] = 42;
   p[1] = 0xdeadbeef;

   jit_handle_t deref =
      compile_for_test(j, "WORK.ACCESS1.DEREF(20WORK.ACCESS1.INT_PTRI)");
   jit_call(j, deref, NULL, NULL, p, p + 1);
   ck_assert_int_eq(p[1], 42);

   jit_scalar_t result;
   fail_if(jit_try_call(j, deref, &result, NULL, NULL, NULL, p + 1));
   ck_assert_int_eq(p[1], 42);

   jit_handle_t test1 =
      compile_for_test(j, "WORK.ACCESS1.TEST1(20WORK.ACCESS1.INT_PTR)");
   jit_call(j, test1, NULL, NULL, p);
   ck_assert_int_eq(p[1], 0);

   jit_handle_t oom = compile_for_test(j, "WORK.ACCESS1.OOM");
   fail_if(jit_try_call(j, oom, &result, NULL, NULL));

   jit_handle_t gc_a_lot = compile_for_test(j, "WORK.ACCESS1.GC_A_LOT");
   fail_unless(jit_try_call(j, gc_a_lot, &result, NULL, NULL));

   jit_free(j);
   check_expected_errors();
}
END_TEST

START_TEST(test_array1)
{
   input_from_file(TESTDIR "/jit/array1.vhd");

   const error_t expect[] = {
      { 14, "value length 2 does not match variable A length 3" },
      { 40, "Report Note:" },
      { -1, NULL },
   };
   expect_errors(expect);

   parse_check_simplify_and_lower(T_PACKAGE, T_PACK_BODY);

   jit_t *j = jit_new();

   int32_t a0[3] = {};
   int32_t a1[3] = { 1, 2, 3 };

   jit_handle_t assign = compile_for_test(j,
      "WORK.ARRAY1.ASSIGN(14WORK.ARRAY1.IV14WORK.ARRAY1.IV)");
   jit_call(j, assign, NULL, NULL, a0, 1, 3, a1, 1, 3);
   ck_assert_mem_eq(a0, a1, sizeof(a0));
   a1[0] = 44;
   jit_call(j, assign, NULL, NULL, a0, 1, 3, a1, -4, 3);
   ck_assert_mem_eq(a0, a1, sizeof(a0));

   a1[0] = 99;
   jit_scalar_t result;
   fail_if(jit_try_call(j, assign, &result, NULL, NULL, a0, 1, 3, a1, 1, 2));
   ck_assert_mem_ne(a0, a1, sizeof(a0));

   jit_handle_t get_ints =
      compile_for_test(j, "WORK.ARRAY1.GET_INTS(II)14WORK.ARRAY1.IV");
   fail_unless(jit_try_call(j, get_ints, &result, NULL, 5, -1));
   ck_assert_ptr_nonnull(result.pointer);

   int32_t *vals = result.pointer;
   ck_assert_ptr_nonnull(vals);
   ck_assert_int_eq(vals[0], -1);
   ck_assert_int_eq(vals[1], 0);
   ck_assert_int_eq(vals[2], 1);
   ck_assert_int_eq(vals[3], 2);
   ck_assert_int_eq(vals[4], 3);

   jit_handle_t issue94 = compile_for_test(j, "WORK.ARRAY1.ISSUE94(II)Q");
   fail_unless(jit_try_call(j, issue94, &result, NULL, 4, 4));
   ck_assert_ptr_nonnull(result.pointer);

   unsigned char *bits = result.pointer;
   ck_assert_ptr_nonnull(bits);
   ck_assert_int_eq(bits[0], 1);
   ck_assert_int_eq(bits[1], 1);
   ck_assert_int_eq(bits[2], 1);
   ck_assert_int_eq(bits[3], 1);

   jit_handle_t test2 = compile_for_test(j, "WORK.ARRAY1.TEST2(S)");
   fail_unless(jit_try_call(j, test2, &result, NULL, NULL, NULL, 1, 0));

   jit_free(j);
   check_expected_errors();
}
END_TEST

START_TEST(test_relop1)
{
   input_from_file(TESTDIR "/jit/relop1.vhd");

   parse_check_simplify_and_lower(T_PACKAGE, T_PACK_BODY);

   jit_t *j = jit_new();

#define UINT8 "17WORK.RELOP1.UINT8"
   jit_handle_t cmpless =
      compile_for_test(j, "WORK.RELOP1.CMPLESS(" UINT8 UINT8 ")B");
#undef UINT8

   ck_assert_int_eq(jit_call(j, cmpless, NULL, 5, 6).integer, 1);
   ck_assert_int_eq(jit_call(j, cmpless, NULL, 127, 128).integer, 1);
   ck_assert_int_eq(jit_call(j, cmpless, NULL, 200, 255).integer, 1);

   jit_handle_t fcmpless = compile_for_test(j, "WORK.RELOP1.CMPLESS(RR)B");
   ck_assert_int_eq(jit_call(j, fcmpless, NULL, 5.0, 6.0).integer, 1);
   ck_assert_int_eq(jit_call(j, fcmpless, NULL, -5.0, -6.0).integer, 0);
   ck_assert_int_eq(jit_call(j, fcmpless, NULL, 0.001, 0.2).integer, 1);

   jit_free(j);
   fail_if_errors();
}
END_TEST

START_TEST(test_proc1)
{
   input_from_file(TESTDIR "/jit/proc1.vhd");

   const error_t expect[] = {
      { 48, "cannot wait inside function call" },
      { -1, NULL },
   };
   expect_errors(expect);

   parse_check_simplify_and_lower(T_PACKAGE, T_PACK_BODY,
                                  T_PACKAGE, T_PACK_BODY,
                                  T_PACKAGE, T_PACK_BODY);

   jit_t *j = jit_new();

   jit_handle_t add2 = compile_for_test(j, "WORK.PROC1_PACK2.ADD2(I)I");

   ck_assert_int_eq(jit_call(j, add2, NULL, 5).integer, 7);

   jit_scalar_t result;
   fail_if(jit_try_call(j, add2, &result, NULL, 10));

   jit_free(j);
   check_expected_errors();
}
END_TEST

START_TEST(test_packsignal)
{
   input_from_file(TESTDIR "/jit/packsignal.vhd");

   parse_check_simplify_and_lower(T_PACKAGE, T_PACK_BODY);

   jit_t *j = jit_new();

   jit_handle_t handle = jit_lazy_compile(j, ident_new("WORK.PACKSIGNAL"));
   void *pkg = jit_link(j, handle);
   fail_if(pkg == NULL);

   jit_free(j);
}
END_TEST

START_TEST(test_unreachable)
{
   input_from_file(TESTDIR "/jit/unreachable.vhd");

   const error_t expect[] = {
      {  6, "function FUNC did not return a value" },
      { -1, NULL },
   };
   expect_errors(expect);

   parse_check_simplify_and_lower(T_PACKAGE, T_PACK_BODY);

   jit_t *j = jit_new();

   jit_handle_t func = compile_for_test(j, "WORK.UNREACHABLE.FUNC(I)I");

   ck_assert_int_eq(jit_call(j, func, NULL, 5).integer, 10);

   jit_scalar_t result;
   fail_if(jit_try_call(j, func, &result, NULL, -1));

   jit_free(j);
   check_expected_errors();
}
END_TEST

START_TEST(test_arith1)
{
   input_from_file(TESTDIR "/jit/arith1.vhd");

   const error_t expect[] = {
      { 25, "division by zero" },
      { 45, "negative exponent -1 only allowed for floating-point types" },
      { -1, NULL },
   };
   expect_errors(expect);

   parse_check_simplify_and_lower(T_PACKAGE, T_PACK_BODY);

   jit_t *j = jit_new();

   jit_scalar_t result;

   jit_handle_t divii = compile_for_test(j, "WORK.ARITH1.DIV(II)I");
   ck_assert_int_eq(jit_call(j, divii, NULL, 4, 2).integer, 2);
   ck_assert_int_eq(jit_call(j, divii, NULL, 5, 2).integer, 2);
   ck_assert_int_eq(jit_call(j, divii, NULL, -5, -2).integer, 2);
   ck_assert_int_eq(jit_call(j, divii, NULL, -5, -1).integer, 5);
   fail_if(jit_try_call(j, divii, &result, NULL, 1, 0));

   jit_handle_t divrr = compile_for_test(j, "WORK.ARITH1.DIV(RR)R");
   ck_assert_double_eq(jit_call(j, divrr, NULL, 4.0, 2.0).real, 2.0);
   ck_assert_double_eq(jit_call(j, divrr, NULL, 4.0, 0.0).real, INFINITY);

   jit_handle_t divir = compile_for_test(j, "WORK.ARITH1.DIV(IR)R");
   ck_assert_double_eq(jit_call(j, divir, NULL, 4, 2.0).real, 2.0);
   ck_assert_double_eq(jit_call(j, divir, NULL, 4, 0.0).real, INFINITY);

   jit_handle_t expr = compile_for_test(j, "WORK.ARITH1.EXP(RI)R");
   ck_assert_double_eq(jit_call(j, expr, NULL, 2.0, 4).real, 16.0);
   ck_assert_double_eq(jit_call(j, expr, NULL, 2.0, -1).real, 0.5);

   jit_handle_t expi = compile_for_test(j, "WORK.ARITH1.EXP(II)I");
   ck_assert_int_eq(jit_call(j, expi, NULL, 2, 4).integer, 16);
   fail_if(jit_try_call(j, expi, &result, NULL, 2, -1));

   jit_handle_t negi = compile_for_test(j, "WORK.ARITH1.NEG(I)I");
   ck_assert_int_eq(jit_call(j, negi, NULL, 2).integer, -2);
   ck_assert_int_eq(jit_call(j, negi, NULL, -124).integer, 124);

   jit_handle_t negr = compile_for_test(j, "WORK.ARITH1.NEG(R)R");
   ck_assert_double_eq(jit_call(j, negr, NULL, 2.0).real, -2.0);
   ck_assert_double_eq(jit_call(j, negr, NULL, -256.0).real, 256.0);

   jit_handle_t castri = compile_for_test(j, "WORK.ARITH1.CAST(R)I");
   ck_assert_int_eq(jit_call(j, castri, NULL, 2.0).integer, 2);
   ck_assert_int_eq(jit_call(j, castri, NULL, 1.5).integer, 2);
   ck_assert_int_eq(jit_call(j, castri, NULL, 1.4999).integer, 1);
   ck_assert_int_eq(jit_call(j, castri, NULL, -1.4999).integer, -1);

   jit_handle_t absi = compile_for_test(j, "WORK.ARITH1.ABZ(I)I");
   ck_assert_int_eq(jit_call(j, absi, NULL, 2).integer, 2);
   ck_assert_int_eq(jit_call(j, absi, NULL, 0).integer, 0);
   ck_assert_int_eq(jit_call(j, absi, NULL, -5).integer, 5);
   // Bug! needs a trap abs
   //ck_assert_int_eq(jit_call(j, absi, NULL, "i", INT32_MIN).integer, INT32_MAX);

   jit_handle_t absr = compile_for_test(j, "WORK.ARITH1.ABZ(R)R");
   ck_assert_double_eq(jit_call(j, absr, NULL, 2.0).real, 2.0);
   ck_assert_double_eq(jit_call(j, absr, NULL, -0.0).real, 0.0);
   ck_assert_double_eq(jit_call(j, absr, NULL, -4.0).real, 4.0);

   jit_handle_t modi = compile_for_test(j, "WORK.ARITH1.MODD(II)I");
   ck_assert_int_eq(jit_call(j, modi, NULL, 4, 2).integer, 0);
   ck_assert_int_eq(jit_call(j, modi, NULL, 5, 3).integer, 2);
   ck_assert_int_eq(jit_call(j, modi, NULL, -5, 3).integer, 1);
   ck_assert_int_eq(jit_call(j, modi, NULL, -512, -8).integer, 0);
   ck_assert_int_eq(jit_call(j, modi, NULL, -510, -8).integer, -6);

   jit_handle_t remi = compile_for_test(j, "WORK.ARITH1.REMM(II)I");
   ck_assert_int_eq(jit_call(j, remi, NULL, 4, 2).integer, 0);
   ck_assert_int_eq(jit_call(j, remi, NULL, 5, 3).integer, 2);
   ck_assert_int_eq(jit_call(j, remi, NULL, -5, 3).integer, -2);

   jit_free(j);
   check_expected_errors();
}
END_TEST

START_TEST(test_assert1)
{
   input_from_file(TESTDIR "/jit/assert1.vhd");

   const error_t expect[] = {
      { 10, "Report Note: hello world" },
      { 15, "Assertion Warning: -10 negative" },
      { 16, "Assertion Failure: too big" },
      { -1, NULL },
   };
   expect_errors(expect);

   parse_check_simplify_and_lower(T_PACKAGE, T_PACK_BODY);

   jit_t *j = jit_new();

   jit_scalar_t result;
   jit_handle_t fn1 = compile_for_test(j, "WORK.ASSERT1.DO_REPORT");
   fail_unless(jit_try_call(j, fn1, &result, NULL, NULL));

   jit_handle_t fn2 = compile_for_test(j, "WORK.ASSERT1.DO_ASSERT(I)");
   fail_unless(jit_try_call(j, fn2, &result, NULL, NULL, 10));
   fail_unless(jit_try_call(j, fn2, &result, NULL, NULL, -10));
   fail_if(jit_try_call(j, fn2, &result, NULL, NULL, 555));

   jit_free(j);
   check_expected_errors();
}
END_TEST

START_TEST(test_case1)
{
   input_from_file(TESTDIR "/jit/case1.vhd");

   parse_check_simplify_and_lower(T_PACKAGE, T_PACK_BODY);

   jit_t *j = jit_new();

   jit_handle_t test1 =
      compile_for_test(j, "WORK.CASE1.TEST1(12WORK.CASE1.T)I");
   ck_assert_int_eq(jit_call(j, test1, NULL, 0).integer, 10);
   ck_assert_int_eq(jit_call(j, test1, NULL, 1).integer, 20);
   ck_assert_int_eq(jit_call(j, test1, NULL, 2).integer, 30);

   const uint8_t one[] = { 0, 0, 0, 1 };
   const uint8_t eff[] = { 1, 1, 1, 1 };
   const uint8_t ten[] = { 1, 0, 1, 0 };

   jit_handle_t test2 = compile_for_test(j, "WORK.CASE1.TEST2(Q)I");
   ck_assert_int_eq(jit_call(j, test2, NULL, one).integer, 1);
   ck_assert_int_eq(jit_call(j, test2, NULL, eff).integer, 15);
   ck_assert_int_eq(jit_call(j, test2, NULL, ten).integer, 10);

   jit_free(j);
   fail_if_errors();
}
END_TEST

START_TEST(test_real1)
{
   input_from_file(TESTDIR "/jit/real1.vhd");

   parse_check_simplify_and_lower(T_PACKAGE, T_PACK_BODY);

   jit_t *j = jit_new();

   jit_handle_t approx = compile_for_test(j, "WORK.REAL1.APPROX(RRR)B");
   ck_assert_int_eq(jit_call(j, approx, NULL, 1.0, 1.0001, 0.001).integer, 1);

   jit_free(j);
   fail_if_errors();
}
END_TEST

START_TEST(test_prot1)
{
   set_standard(STD_02);

   input_from_file(TESTDIR "/jit/prot1.vhd");

   parse_check_simplify_and_lower(T_PACKAGE, T_PACK_BODY);

   jit_t *j = jit_new();

   jit_handle_t handle = compile_for_test(j, "WORK.PROT1");
   void *pkg = jit_link(j, handle);
   fail_if(pkg == NULL);

   jit_handle_t fn = compile_for_test(j, "WORK.PROT1.FETCH_AND_ADD(I)I");
   ck_assert_int_eq(jit_call(j, fn, pkg, 0).integer, 0);
   ck_assert_int_eq(jit_call(j, fn, pkg, 1).integer, 1);
   ck_assert_int_eq(jit_call(j, fn, pkg, 1).integer, 2);
   ck_assert_int_eq(jit_call(j, fn, pkg, 6).integer, 8);

   jit_free(j);
   fail_if_errors();
}
END_TEST

static tree_t make_field(const char *name, type_t type)
{
   tree_t f = tree_new(T_FIELD_DECL);
   tree_set_type(f, type);
   tree_set_ident(f, ident_new(name));
   return f;
}

START_TEST(test_layout)
{
   jit_t *j = jit_new();
   const jit_layout_t *l = NULL;

   l = jit_layout(j, std_type(NULL, STD_INTEGER));
   ck_assert_int_eq(l->nparts, 1);
   ck_assert_int_eq(l->parts[0].offset, 0);
   ck_assert_int_eq(l->parts[0].size, 4);
   ck_assert_int_eq(l->parts[0].repeat, 1);
   ck_assert_int_eq(l->parts[0].align, 4);

   l = jit_layout(j, std_type(NULL, STD_REAL));
   ck_assert_int_eq(l->nparts, 1);
   ck_assert_int_eq(l->parts[0].offset, 0);
   ck_assert_int_eq(l->parts[0].size, 8);
   ck_assert_int_eq(l->parts[0].repeat, 1);
   ck_assert_int_eq(l->parts[0].align, 8);

   l = jit_layout(j, std_type(NULL, STD_TIME));
   ck_assert_int_eq(l->nparts, 1);
   ck_assert_int_eq(l->parts[0].offset, 0);
   ck_assert_int_eq(l->parts[0].size, 8);
   ck_assert_int_eq(l->parts[0].repeat, 1);
   ck_assert_int_eq(l->parts[0].align, 8);

   l = jit_layout(j, std_type(NULL, STD_BOOLEAN));
   ck_assert_int_eq(l->nparts, 1);
   ck_assert_int_eq(l->parts[0].offset, 0);
   ck_assert_int_eq(l->parts[0].size, 1);
   ck_assert_int_eq(l->parts[0].repeat, 1);
   ck_assert_int_eq(l->parts[0].align, 1);

   l = jit_layout(j, std_type(NULL, STD_STRING));
   ck_assert_int_eq(l->nparts, 2);
   ck_assert_int_eq(l->parts[0].offset, 0);
   ck_assert_int_eq(l->parts[0].size, sizeof(void *));
   ck_assert_int_eq(l->parts[0].repeat, 1);
   ck_assert_int_eq(l->parts[0].align, 8);
   ck_assert_int_eq(l->parts[1].offset, sizeof(void *));
   ck_assert_int_eq(l->parts[1].size, 4);
   ck_assert_int_eq(l->parts[1].repeat, 2);

   make_new_arena();

   type_t r1 = type_new(T_RECORD);
   type_set_ident(r1, ident_new("R1"));
   type_add_field(r1, make_field("X", std_type(NULL, STD_INTEGER)));
   type_add_field(r1, make_field("Y", std_type(NULL, STD_BOOLEAN)));
   type_add_field(r1, make_field("Z", std_type(NULL, STD_REAL)));

   l = jit_layout(j, r1);
   ck_assert_int_eq(l->nparts, 3);
   ck_assert_int_eq(l->parts[0].offset, 0);
   ck_assert_int_eq(l->parts[0].size, 4);
   ck_assert_int_eq(l->parts[0].repeat, 1);
   ck_assert_int_eq(l->parts[1].offset, 4);
   ck_assert_int_eq(l->parts[1].size, 1);
   ck_assert_int_eq(l->parts[1].repeat, 1);
   ck_assert_int_eq(l->parts[2].offset, 8);
   ck_assert_int_eq(l->parts[2].size, 8);
   ck_assert_int_eq(l->parts[2].repeat, 1);

   type_t r2 = type_new(T_RECORD);
   type_set_ident(r2, ident_new("R2"));
   type_add_field(r2, make_field("X", r1));
   type_add_field(r2, make_field("Y", std_type(NULL, STD_INTEGER)));

   l = jit_layout(j, r2);
   ck_assert_int_eq(l->nparts, 2);
   ck_assert_int_eq(l->parts[0].offset, 0);
   ck_assert_int_eq(l->parts[0].size, 16);
   ck_assert_int_eq(l->parts[0].repeat, 1);
   ck_assert_int_eq(l->parts[0].align, 8);
   ck_assert_int_eq(l->parts[1].offset, 16);
   ck_assert_int_eq(l->parts[1].size, 4);
   ck_assert_int_eq(l->parts[1].repeat, 1);

   type_t a = type_new(T_SUBTYPE);
   type_set_base(a, std_type(NULL, STD_STRING));

   type_t std_int = std_type(NULL, STD_INTEGER);

   tree_t left = tree_new(T_LITERAL);
   tree_set_subkind(left, L_INT);
   tree_set_ival(left, 1);
   tree_set_type(left, std_int);

   tree_t right = tree_new(T_LITERAL);
   tree_set_subkind(right, L_INT);
   tree_set_ival(right, 5);
   tree_set_type(right, std_int);

   tree_t r = tree_new(T_RANGE);
   tree_set_subkind(r, RANGE_TO);
   tree_set_left(r, left);
   tree_set_right(r, right);

   tree_t cons = tree_new(T_CONSTRAINT);
   tree_set_subkind(cons, C_INDEX);
   tree_add_range(cons, r);

   type_add_constraint(a, cons);

   l = jit_layout(j, a);
   ck_assert_int_eq(l->nparts, 1);
   ck_assert_int_eq(l->parts[0].offset, 0);
   ck_assert_int_eq(l->parts[0].size, 1);
   ck_assert_int_eq(l->parts[0].repeat, 5);
   ck_assert_int_eq(l->parts[0].align, 1);

   jit_free(j);
}
END_TEST

START_TEST(test_range1)
{
   input_from_file(TESTDIR "/jit/range1.vhd");

   const error_t expect[] = {
      { 13, "0 outside of POSITIVE range 1 to 2147483647 for variable P" },
      { 13, "value -5 outside of POSITIVE range 1 to 2147483647 for" },
      { 20, "value 0 outside of PREAL range 1 to 1.7976931348623157e+308 "
        "for variable P" },
      { 20, "value -5 outside of PREAL range" },
      { -1, NULL },
   };
   expect_errors(expect);

   parse_check_simplify_and_lower(T_PACKAGE, T_PACK_BODY);

   jit_t *j = jit_new();

   jit_scalar_t result;

   jit_handle_t fn1 =
      compile_for_test(j, "WORK.RANGE1.AS_POSITIVE(I)8POSITIVE");
   ck_assert_int_eq(jit_call(j, fn1, NULL, 1).integer, 1);
   ck_assert_int_eq(jit_call(j, fn1, NULL, 2).integer, 2);
   fail_if(jit_try_call(j, fn1, &result, NULL, 0));
   fail_if(jit_try_call(j, fn1, &result, NULL, -5));

   jit_handle_t fn2 = compile_for_test(j, "WORK.RANGE1.AS_POSITIVE(R)5PREAL");
   ck_assert_double_eq(jit_call(j, fn2, NULL, 1.0).real, 1.0);
   ck_assert_double_eq(jit_call(j, fn2, NULL, 1.001).real, 1.001);
   fail_if(jit_try_call(j, fn2, &result, NULL, 0.0));
   fail_if(jit_try_call(j, fn2, &result, NULL, -5.0));

   jit_free(j);
   check_expected_errors();
}
END_TEST

static void trace1_diag_fn(diag_t *d)
{
   ck_assert_str_eq(diag_get_text(d), "division by zero");
   ck_assert_int_eq(diag_traces(d), 3);
   ck_assert_str_eq(diag_get_trace(d, 0), "Procedure TEST3 []");
   ck_assert_str_eq(diag_get_trace(d, 1), "Procedure TEST2 []");
   ck_assert_str_eq(diag_get_trace(d, 2), "Procedure TEST1 []");
}

START_TEST(test_trace1)
{
   input_from_file(TESTDIR "/jit/trace1.vhd");

   diag_set_consumer(trace1_diag_fn);

   parse_check_simplify_and_lower(T_PACKAGE, T_PACK_BODY);

   jit_t *j = jit_new();

   jit_scalar_t result;

   jit_handle_t test1 = compile_for_test(j, "WORK.TRACE1.TEST1");
   fail_if(jit_try_call(j, test1, &result, NULL, ""));

   jit_free(j);
   fail_unless(error_count() == 1);
}
END_TEST

START_TEST(test_issue496)
{
   set_standard(STD_08);

   input_from_file(TESTDIR "/jit/issue496.vhd");

   parse_check_simplify_and_lower(T_PACKAGE, T_PACKAGE);

   jit_t *j = jit_new();

   jit_handle_t handle = compile_for_test(j, "WORK.ISSUE496");
   void *pkg = jit_link(j, handle);
   fail_if(pkg == NULL);

   char *c = jit_get_frame_var(j, handle, 2);
   ck_assert_mem_eq(c, "one", 3);

   jit_free(j);
   fail_if_errors();
}
END_TEST

START_TEST(test_process1)
{
   input_from_file(TESTDIR "/jit/process1.vhd");

   const error_t expect[] = {
      { 10, "hello, world" },
      { 13, "after 1 ns" },
      { -1, NULL },
   };
   expect_errors(expect);

   tree_t top = run_elab();
   lower_unit(top, NULL);

   jit_t *j = jit_new();

   jit_handle_t root = compile_for_test(j, "WORK.PROCESS1");
   void *inst = jit_link(j, root);
   fail_if(inst == NULL);

   jit_handle_t p1 = compile_for_test(j, "WORK.PROCESS1.P1");
   void *p1_state = jit_call(j, p1, NULL, inst).pointer;
   fail_if(p1_state == NULL);   // TODO: stateless process

   int32_t *fsm_ptr = p1_state + 2*sizeof(void *);
   ck_assert_int_eq(*fsm_ptr, 1);

   int32_t *x_ptr = p1_state + 2*sizeof(void *) + sizeof(int32_t);
   ck_assert_int_eq(*x_ptr, INT32_MIN);

   fail_unless(jit_call(j, p1, p1_state, inst).pointer == NULL);
   ck_assert_int_eq(*fsm_ptr, 2);
   ck_assert_int_eq(*x_ptr, 42);

   jit_call(j, p1, p1_state, inst);
   ck_assert_int_eq(*fsm_ptr, 3);
   ck_assert_int_eq(*x_ptr, 43);

   jit_free(j);
   check_expected_errors();
}
END_TEST

START_TEST(test_value1)
{
   input_from_file(TESTDIR "/jit/value1.vhd");

   const error_t expect[] = {
      { 73, "found invalid characters \"x\" after value \"42x\"" },
      { 77, "invalid real value \"4..4\"" },
      { 81, "\" FOO\" is not a valid unit name" },
      { 23, "\"FOO\" is not a valid enumeration value" },
      { -1, NULL },
   };
   expect_errors(expect);

   parse_check_simplify_and_lower(T_PACKAGE, T_PACK_BODY);

   jit_t *j = jit_new();

   jit_scalar_t result;

   jit_handle_t fn1 = compile_for_test(j, "WORK.VALUE1.STR_TO_INT(S)I");
   ck_assert_int_eq(jit_call(j, fn1, NULL, "123", 1, 3).integer, 123);
   ck_assert_int_eq(jit_call(j, fn1, NULL, "-5", 1, 2).integer, -5);
   ck_assert_int_eq(jit_call(j, fn1, NULL, " 42 ", 1, 4).integer, 42);
   fail_if(jit_try_call(j, fn1, &result, NULL, "42x", 1, 3));

   jit_handle_t fn2 = compile_for_test(j, "WORK.VALUE1.STR_TO_REAL(S)R");
   ck_assert_double_eq(jit_call(j, fn2, NULL, "123", 1, 3).real, 123.0);
   ck_assert_double_eq(jit_call(j, fn2, NULL, "-4.5", 1, 4).real, -4.5);
   fail_if(jit_try_call(j, fn2, &result, NULL, "4..4", 1, 4));

   jit_handle_t fn3 = compile_for_test(j, "WORK.VALUE1.STR_TO_TIME(S)T");
   ck_assert_int_eq(jit_call(j, fn3, NULL, "123 FS", 1, 6).integer, 123);
   ck_assert_int_eq(jit_call(j, fn3, NULL, " 52  PS ", 1, 8).integer, 52000);
   fail_if(jit_try_call(j, fn3, &result, NULL, "4 FOO", 1, 5));

   jit_handle_t fn4 = compile_for_test(j, "WORK.VALUE1.STR_TO_BOOL(S)B");
   ck_assert_int_eq(jit_call(j, fn4, NULL, "true", 1, 4).integer, 1);
   ck_assert_int_eq(jit_call(j, fn4, NULL, " FALSE ", 1, 7).integer, 0);
   fail_if(jit_try_call(j, fn4, &result, NULL, "FOO", 1, 3));

   jit_free(j);
   check_expected_errors();
}
END_TEST

DLLEXPORT
int test_ffi_add(int x, int y)
{
   return x + y;
}

DLLEXPORT
double test_ffi_fma(double x, double y, double z)
{
   return x * y + z;
}

static int test_ffi_arraylen(EXPLODED_UARRAY(arr))
{
   return abs(arr_length);
}

static int test_ffi_arraysum(EXPLODED_UARRAY(arr))
{
   const int len = abs(arr_length);
   int sum = 0;
   for (int i = 0; i < len; i++)
      sum += *((int *)arr_ptr + i);
   return sum;
}

START_TEST(test_ffi1)
{
   ffi_load_dll(NULL);

   ident_t add_i = ident_new("test_ffi_add");

   fail_unless(jit_ffi_get(add_i) == NULL);

   const ffi_spec_t add_spec = FFI_INT32
      | (FFI_INT32 << 4)
      | (FFI_INT32 << 8);

   jit_foreign_t *add_ff = jit_ffi_bind(add_i, add_spec, NULL);
   fail_if(add_ff == NULL);

   fail_unless(jit_ffi_get(add_i) == add_ff);

   {
      jit_scalar_t args[] = { { .integer = 5 }, { .integer = 3 } };
      jit_ffi_call(add_ff, args);
      ck_assert_int_eq(args[0].integer, 8);
   }

   {
      jit_scalar_t args[] = { { .integer = 5 }, { .integer = -7 } };
      jit_ffi_call(add_ff, args);
      ck_assert_int_eq(args[0].integer, -2);
   }

   ident_t fma_i = ident_new("test_ffi_fma");

   const ffi_spec_t fma_spec = FFI_FLOAT
      | (FFI_FLOAT << 4)
      | (FFI_FLOAT << 8)
      | (FFI_FLOAT << 12);

   jit_foreign_t *fma_ff = jit_ffi_bind(fma_i, fma_spec, NULL);
   fail_if(fma_ff == NULL);

   {
      jit_scalar_t args[] = { { .real = 2.0 },
                              { .real = 3.0 },
                              { .real = 1.0 } };
      jit_ffi_call(fma_ff, args);
      ck_assert_double_eq(args[0].real, 7.0);
   }

   {
      jit_scalar_t args[] = { { .real = -2.0 },
                              { .real = 3.0 },
                              { .real = 1.0 } };
      jit_ffi_call(fma_ff, args);
      ck_assert_double_eq(args[0].real, -5.0);
   }

   ident_t len_i = ident_new("len");

   const ffi_spec_t len_spec = FFI_INT32
      | (FFI_UARRAY << 4);

   jit_foreign_t *len_ff = jit_ffi_bind(len_i, len_spec, test_ffi_arraylen);
   fail_if(len_ff == NULL);

   {
      jit_scalar_t args[] = {
         { .pointer = NULL }, { .integer = 1 }, { .integer = 4 }
      };
      jit_ffi_call(len_ff, args);
      ck_assert_int_eq(args[0].integer, 4);
   }

   ident_t sum_i = ident_new("sum");

   const ffi_spec_t sum_spec = FFI_INT32
      | (FFI_UARRAY << 4);

   jit_foreign_t *sum_ff = jit_ffi_bind(sum_i, sum_spec, test_ffi_arraysum);
   fail_if(sum_ff == NULL);

   {
      int data[4] = { 1, 2, 3, 4 };
      jit_scalar_t args[] = {
         { .pointer = data }, { .integer = 1 }, { .integer = 4 }
      };
      jit_ffi_call(sum_ff, args);
      ck_assert_int_eq(args[0].integer, 10);
   }
}
END_TEST

Suite *get_jit_tests(void)
{
   Suite *s = suite_create("jit");

   TCase *tc = nvc_unit_test();
   tcase_add_test(tc, test_add1);
   tcase_add_test(tc, test_fact);
   tcase_add_test(tc, test_sum);
   tcase_add_test(tc, test_context1);
   tcase_add_test(tc, test_record1);
   tcase_add_test(tc, test_record2);
   tcase_add_test(tc, test_record3);
   tcase_add_test(tc, test_ieee_warnings);
   tcase_add_test(tc, test_overflow);
   tcase_add_test(tc, test_record4);
   tcase_add_test(tc, test_access1);
   tcase_add_test(tc, test_array1);
   tcase_add_test(tc, test_relop1);
   tcase_add_test(tc, test_proc1);
   tcase_add_test(tc, test_packsignal);
   tcase_add_test(tc, test_unreachable);
   tcase_add_test(tc, test_arith1);
   tcase_add_test(tc, test_assert1);
   tcase_add_test(tc, test_case1);
   tcase_add_test(tc, test_real1);
   tcase_add_test(tc, test_prot1);
   tcase_add_test(tc, test_layout);
   tcase_add_test(tc, test_range1);
   tcase_add_test(tc, test_trace1);
   tcase_add_test(tc, test_issue496);
   tcase_add_test(tc, test_process1);
   tcase_add_test(tc, test_value1);
   tcase_add_test(tc, test_ffi1);
   suite_add_tcase(s, tc);

   return s;
}
