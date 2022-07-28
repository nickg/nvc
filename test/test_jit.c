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
#include "opt.h"
#include "phase.h"
#include "scan.h"
#include "type.h"

#include <math.h>

START_TEST(test_add1)
{
   input_from_file(TESTDIR "/jit/add1.vhd");

   parse_check_simplify_and_lower(T_PACKAGE, T_PACK_BODY);

   jit_t *j = jit_new();

   ident_t fn1 = ident_new("WORK.PACK.ADD1(I)I");
   ck_assert_int_eq(jit_call(j, fn1, NULL, "i", 5).integer, 6);
   ck_assert_int_eq(jit_call(j, fn1, NULL, "i", INT32_C(-5)).integer, -4);

   ident_t fn2 = ident_new("WORK.PACK.ADD1(R)R");
   ck_assert_double_eq_tol(jit_call(j, fn2, NULL, "R", 5.0).real, 6.0, 0.001);

   jit_free(j);

   fail_if_errors();
}
END_TEST

START_TEST(test_fact)
{
   input_from_file(TESTDIR "/jit/fact.vhd");

   parse_check_simplify_and_lower(T_PACKAGE, T_PACK_BODY);

   jit_t *j = jit_new();

   ident_t fn1 = ident_new("WORK.PACK.FACT(I)I");
   ck_assert_int_eq(jit_call(j, fn1, NULL, "i", 5).integer, 120);
   ck_assert_int_eq(jit_call(j, fn1, NULL, "i", 8).integer, 40320);

   ident_t fn2 = ident_new("WORK.PACK.FACT_RECUR(I)I");
   ck_assert_int_eq(jit_call(j, fn2, NULL, "i", 5).integer, 120);
   ck_assert_int_eq(jit_call(j, fn2, NULL, "i", 8).integer, 40320);

   jit_free(j);
}
END_TEST

START_TEST(test_sum)
{
   input_from_file(TESTDIR "/jit/sum.vhd");

   parse_check_simplify_and_lower(T_PACKAGE, T_PACK_BODY);

   jit_t *j = jit_new();

   int32_t data[] = { 1, 2, 3, 4, 5 };

   ident_t fn1 = ident_new("WORK.SUMPKG.GET_LEFT(22WORK.SUMPKG.INT_VECTOR)I");
   ck_assert_int_eq(jit_call(j, fn1, NULL, "u", data, 1, 5).integer, 1);
   ck_assert_int_eq(jit_call(j, fn1, NULL, "u", data, -5, 5).integer, -5);

   ident_t fn2 = ident_new("WORK.SUMPKG.GET_RIGHT(22WORK.SUMPKG.INT_VECTOR)I");
   ck_assert_int_eq(jit_call(j, fn2, NULL, "u", data, 1, 5).integer, 5);
   ck_assert_int_eq(jit_call(j, fn2, NULL, "u", data, -5, 0).integer, -6);
   ck_assert_int_eq(jit_call(j, fn2, NULL, "u", data, -5, 2).integer, -4);

   ident_t fn3 = ident_new("WORK.SUMPKG.GET_LENGTH(22WORK.SUMPKG.INT_VECTOR)I");
   ck_assert_int_eq(jit_call(j, fn3, NULL, "u", data, 1, 5).integer, 5);
   ck_assert_int_eq(jit_call(j, fn3, NULL, "u", data, -5, 0).integer, 0);
   ck_assert_int_eq(jit_call(j, fn3, NULL, "u", data, -5, 2).integer, 2);

   ident_t fn4 = ident_new("WORK.SUMPKG.SUM(22WORK.SUMPKG.INT_VECTOR)I");
   ck_assert_int_eq(jit_call(j, fn4, NULL, "u", data, 1, 5).integer, 15);
   ck_assert_int_eq(jit_call(j, fn4, NULL, "u", data,
                             5, INT32_C(-5)).integer, 15);
   ck_assert_int_eq(jit_call(j, fn4, NULL, "u", data, 1, 2).integer, 3);
   ck_assert_int_eq(jit_call(j, fn4, NULL, "u", data, 100, 2).integer, 3);
   ck_assert_int_eq(jit_call(j, fn4, NULL, "u", data,
                             INT32_C(-10), 2).integer, 3);
   ck_assert_int_eq(jit_call(j, fn4, NULL, "u", data, 1, 0).integer, 0);

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

   ident_t fn1 = ident_new("WORK.PACK.GET_ELT(7NATURAL)I");
   ck_assert_int_eq(jit_call(j, fn1, ctx, "i", 1).integer, 10);
   ck_assert_int_eq(jit_call(j, fn1, ctx, "i", 2).integer, 20);

   jit_scalar_t result;
   fail_if(jit_try_call(j, fn1, NULL, &result, "i", 55));
   fail_if(jit_try_call(j, fn1, NULL, &result, "i", -1));

   ident_t fn2 = ident_new("WORK.PACK.NESTED_GET_ELT(7NATURAL)I");
   ck_assert_int_eq(jit_call(j, fn2, ctx, "i", 1).integer, 10);
   ck_assert_int_eq(jit_call(j, fn2, ctx, "i", 5).integer, 50);
   fail_if(jit_try_call(j, fn2, NULL, &result, "i", -1));

   int x;
   ident_t fn3 = ident_new("WORK.PACK.READ_ELT(7NATURALI)");
   fail_unless(jit_try_call(j, fn3, ctx, &result, "ip", 1, &x));
   ck_assert_int_eq(x, 10);
   fail_unless(jit_try_call(j, fn3, ctx, &result, "ip", 5, &x));
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

   ident_t fn = ident_new("WORK.PACK2.SUM_FIELDS()I");
   ck_assert_int_eq(jit_call(j, fn, pack2, "").integer, 6);

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

   ident_t fn = ident_new("WORK.PACK4.SUM_FIELDS()I");
   ck_assert_int_eq(jit_call(j, fn, pack4, "").integer, 21);

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

   ident_t fn = ident_new("WORK.PACK6.SUM_FIELDS()I");
   ck_assert_int_eq(jit_call(j, fn, pack6, "").integer, 55);

   jit_free(j);
}
END_TEST

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

   ident_t add = ident_new("WORK.OVERFLOW.ADD(II)I");
   ident_t sub = ident_new("WORK.OVERFLOW.SUB(II)I");
   ident_t mul = ident_new("WORK.OVERFLOW.MUL(II)I");

   jit_scalar_t result;
   fail_if(jit_try_call(j, add, NULL, &result, "ii", INT32_MAX, 1));
   fail_if(jit_try_call(j, add, NULL, &result, "ii", INT32_MAX, INT32_MAX));
   fail_if(jit_try_call(j, sub, NULL, &result, "ii", INT32_MIN, 53));
   fail_if(jit_try_call(j, mul, NULL, &result, "ii", 2352523154, 128910));

#define UINT8 "19WORK.OVERFLOW.UINT8"
   ident_t addu = ident_new("WORK.OVERFLOW.ADD(" UINT8 UINT8 ")" UINT8);
   ident_t subu = ident_new("WORK.OVERFLOW.SUB(" UINT8 UINT8 ")" UINT8);
   ident_t mulu = ident_new("WORK.OVERFLOW.MUL(" UINT8 UINT8 ")" UINT8);
#undef UINT8

   ck_assert_int_eq(jit_call(j, addu, NULL, "ii", 5, 6).integer, 11);
   ck_assert_int_eq(jit_call(j, addu, NULL, "ii", 127, 1).integer, 128);
   fail_if(jit_try_call(j, addu, NULL, &result, "ii", 255, 128));

   ck_assert_int_eq(jit_call(j, subu, NULL, "ii", 255, 4).integer, 251);
   ck_assert_int_eq(jit_call(j, subu, NULL, "ii", 1, 1).integer, 0);
   fail_if(jit_try_call(j, subu, NULL, &result, "ii", 2, 3));

   ck_assert_int_eq(jit_call(j, mulu, NULL, "ii", 127, 2).integer, 254);
   fail_if(jit_try_call(j, mulu, NULL, &result, "ii", 255, 2));

   jit_free(j);
   check_expected_errors();
}
END_TEST

START_TEST(test_record4)
{
   input_from_file(TESTDIR "/jit/record4.vhd");

   parse_check_simplify_and_lower(T_PACKAGE, T_PACK_BODY);

   jit_t *j = jit_new();

   ident_t func1 = ident_new("WORK.PACK5.FUNC1(I)I");
   ident_t func2 = ident_new("WORK.PACK5.FUNC2(I)I");
   ident_t func3 = ident_new("WORK.PACK5.FUNC3(I)I");
   ident_t func4 = ident_new("WORK.PACK5.FUNC4(I)I");

   ck_assert_int_eq(jit_call(j, func1, NULL, "i", 5).integer, 40);
   ck_assert_int_eq(jit_call(j, func1, NULL, "i", 2).integer, 7);
   ck_assert_int_eq(jit_call(j, func1, NULL, "i", 0).integer, 0);
   ck_assert_int_eq(jit_call(j, func2, NULL, "i", 5).integer, 40);
   ck_assert_int_eq(jit_call(j, func2, NULL, "i", 2).integer, 7);
   ck_assert_int_eq(jit_call(j, func2, NULL, "i", 0).integer, 0);
   ck_assert_int_eq(jit_call(j, func3, NULL, "i", 2).integer, 6);
   ck_assert_int_eq(jit_call(j, func4, NULL, "i", 3).integer, 27);

   jit_free(j);
   fail_if_errors();
}
END_TEST

START_TEST(test_access1)
{
   input_from_file(TESTDIR "/jit/access1.vhd");

   const error_t expect[] = {
      { 16, "null access dereference" },
      { LINE_INVALID, "out of memory attempting to allocate 1032 byte " },
      { -1, NULL },
   };
   expect_errors(expect);

   parse_check_simplify_and_lower(T_PACKAGE, T_PACK_BODY);

   jit_t *j = jit_new();

   int32_t *p LOCAL = xmalloc_array(2, sizeof(int32_t));
   p[0] = 42;
   p[1] = 0xdeadbeef;

   ident_t deref = ident_new("WORK.ACCESS1.DEREF(20WORK.ACCESS1.INT_PTRI)");
   jit_call(j, deref, NULL, "pp", p, p + 1);
   ck_assert_int_eq(p[1], 42);

   jit_scalar_t result;
   fail_if(jit_try_call(j, deref, NULL, &result, "pp", NULL, p + 1));
   ck_assert_int_eq(p[1], 42);

   ident_t test1 = ident_new("WORK.ACCESS1.TEST1(20WORK.ACCESS1.INT_PTR)");
   jit_call(j, test1, NULL, "p", p);
   ck_assert_int_eq(p[1], 0);

   ident_t oom = ident_new("WORK.ACCESS1.OOM");
   fail_if(jit_try_call(j, oom, NULL, &result, ""));

   ident_t gc_a_lot = ident_new("WORK.ACCESS1.GC_A_LOT");
   fail_unless(jit_try_call(j, gc_a_lot, NULL, &result, ""));

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

   ident_t assign = ident_new(
      "WORK.ARRAY1.ASSIGN(14WORK.ARRAY1.IV14WORK.ARRAY1.IV)");
   jit_call(j, assign, NULL, "uu", a0, 1, 3, a1, 1, 3);
   ck_assert_mem_eq(a0, a1, sizeof(a0));
   a1[0] = 44;
   jit_call(j, assign, NULL, "uu", a0, 1, 3, a1, -4, 3);
   ck_assert_mem_eq(a0, a1, sizeof(a0));

   jit_scalar_t result;
   a1[0] = 99;
   fail_if(jit_try_call(j, assign, NULL, &result, "uu", a0, 1, 3, a1, 1, 2));
   ck_assert_mem_ne(a0, a1, sizeof(a0));

   ident_t get_ints = ident_new("WORK.ARRAY1.GET_INTS(II)14WORK.ARRAY1.IV");
   fail_unless(jit_try_call(j, get_ints, NULL, &result, "ii", 5, -1));
   ck_assert_ptr_nonnull(result.pointer);

   int32_t *vals = result.pointer;
   ck_assert_ptr_nonnull(vals);
   ck_assert_int_eq(vals[0], -1);
   ck_assert_int_eq(vals[1], 0);
   ck_assert_int_eq(vals[2], 1);
   ck_assert_int_eq(vals[3], 2);
   ck_assert_int_eq(vals[4], 3);

   ident_t issue94 = ident_new("WORK.ARRAY1.ISSUE94(II)Q");
   fail_unless(jit_try_call(j, issue94, NULL, &result, "ii", 4, 4));
   ck_assert_ptr_nonnull(result.pointer);

   unsigned char *bits = result.pointer;
   ck_assert_ptr_nonnull(bits);
   ck_assert_int_eq(bits[0], 1);
   ck_assert_int_eq(bits[1], 1);
   ck_assert_int_eq(bits[2], 1);
   ck_assert_int_eq(bits[3], 1);

   ident_t test2 = ident_new("WORK.ARRAY1.TEST2(S)");
   fail_unless(jit_try_call(j, test2, NULL, &result, "u", NULL, 1, 0));

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
   ident_t cmpless = ident_new("WORK.RELOP1.CMPLESS(" UINT8 UINT8 ")B");
#undef UINT8

   ck_assert_int_eq(jit_call(j, cmpless, NULL, "ii", 5, 6).integer, 1);
   ck_assert_int_eq(jit_call(j, cmpless, NULL, "ii", 127, 128).integer, 1);
   ck_assert_int_eq(jit_call(j, cmpless, NULL, "ii", 200, 255).integer, 1);

   ident_t fcmpless = ident_new("WORK.RELOP1.CMPLESS(RR)B");
   ck_assert_int_eq(jit_call(j, fcmpless, NULL, "RR", 5.0, 6.0).integer, 1);
   ck_assert_int_eq(jit_call(j, fcmpless, NULL, "RR", -5.0, -6.0).integer, 0);
   ck_assert_int_eq(jit_call(j, fcmpless, NULL, "RR", 0.001, 0.2).integer, 1);

   jit_free(j);
   fail_if_errors();
}
END_TEST

START_TEST(test_proc1)
{
   input_from_file(TESTDIR "/jit/proc1.vhd");

   parse_check_simplify_and_lower(T_PACKAGE, T_PACK_BODY,
                                  T_PACKAGE, T_PACK_BODY,
                                  T_PACKAGE, T_PACK_BODY);

   jit_t *j = jit_new();

   ident_t add2 = ident_new("WORK.PROC1_PACK2.ADD2(I)I");

   ck_assert_int_eq(jit_call(j, add2, NULL, "i", 5).integer, 7);

   jit_scalar_t result;
   fail_if(jit_try_call(j, add2, NULL, &result, "i", 10));

   jit_free(j);
   fail_if_errors();
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

   ident_t func = ident_new("WORK.UNREACHABLE.FUNC(I)I");

   ck_assert_int_eq(jit_call(j, func, NULL, "i", 5).integer, 10);

   jit_scalar_t result;
   fail_if(jit_try_call(j, func, NULL, &result, "i", -1));

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

   ident_t divii = ident_new("WORK.ARITH1.DIV(II)I");
   ck_assert_int_eq(jit_call(j, divii, NULL, "ii", 4, 2).integer, 2);
   ck_assert_int_eq(jit_call(j, divii, NULL, "ii", 5, 2).integer, 2);
   ck_assert_int_eq(jit_call(j, divii, NULL, "ii", -5, -2).integer, 2);
   ck_assert_int_eq(jit_call(j, divii, NULL, "ii", -5, -1).integer, 5);
   fail_if(jit_try_call(j, divii, NULL, &result, "ii", 1, 0));

   ident_t divrr = ident_new("WORK.ARITH1.DIV(RR)R");
   ck_assert_double_eq(jit_call(j, divrr, NULL, "RR", 4.0, 2.0).real, 2.0);
   ck_assert_double_eq(jit_call(j, divrr, NULL, "RR", 4.0, 0.0).real, INFINITY);

   ident_t divir = ident_new("WORK.ARITH1.DIV(IR)R");
   ck_assert_double_eq(jit_call(j, divir, NULL, "iR", 4, 2.0).real, 2.0);
   ck_assert_double_eq(jit_call(j, divir, NULL, "iR", 4, 0.0).real, INFINITY);

   ident_t expr = ident_new("WORK.ARITH1.EXP(RI)R");
   ck_assert_double_eq(jit_call(j, expr, NULL, "Ri", 2.0, 4).real, 16.0);
   ck_assert_double_eq(jit_call(j, expr, NULL, "Ri", 2.0, -1).real, 0.5);

   ident_t expi = ident_new("WORK.ARITH1.EXP(II)I");
   ck_assert_int_eq(jit_call(j, expi, NULL, "ii", 2, 4).integer, 16);
   fail_if(jit_try_call(j, expi, NULL, &result, "ii", 2, -1));

   ident_t negi = ident_new("WORK.ARITH1.NEG(I)I");
   ck_assert_int_eq(jit_call(j, negi, NULL, "i", 2).integer, -2);
   ck_assert_int_eq(jit_call(j, negi, NULL, "i", -124).integer, 124);

   ident_t negr = ident_new("WORK.ARITH1.NEG(R)R");
   ck_assert_double_eq(jit_call(j, negr, NULL, "R", 2.0).real, -2.0);
   ck_assert_double_eq(jit_call(j, negr, NULL, "R", -256.0).real, 256.0);

   ident_t castri = ident_new("WORK.ARITH1.CAST(R)I");
   ck_assert_int_eq(jit_call(j, castri, NULL, "R", 2.0).integer, 2);
   ck_assert_int_eq(jit_call(j, castri, NULL, "R", 1.5).integer, 2);
   ck_assert_int_eq(jit_call(j, castri, NULL, "R", 1.4999).integer, 1);
   ck_assert_int_eq(jit_call(j, castri, NULL, "R", -1.4999).integer, -1);

   ident_t absi = ident_new("WORK.ARITH1.ABZ(I)I");
   ck_assert_int_eq(jit_call(j, absi, NULL, "i", 2).integer, 2);
   ck_assert_int_eq(jit_call(j, absi, NULL, "i", 0).integer, 0);
   ck_assert_int_eq(jit_call(j, absi, NULL, "i", -5).integer, 5);
   // Bug! needs a trap abs
   //ck_assert_int_eq(jit_call(j, absi, NULL, "i", INT32_MIN).integer, INT32_MAX);

   ident_t absr = ident_new("WORK.ARITH1.ABZ(R)R");
   ck_assert_double_eq(jit_call(j, absr, NULL, "R", 2.0).real, 2.0);
   ck_assert_double_eq(jit_call(j, absr, NULL, "R", -0.0).real, 0.0);
   ck_assert_double_eq(jit_call(j, absr, NULL, "R", -4.0).real, 4.0);

   ident_t modi = ident_new("WORK.ARITH1.MODD(II)I");
   ck_assert_int_eq(jit_call(j, modi, NULL, "ii", 4, 2).integer, 0);
   ck_assert_int_eq(jit_call(j, modi, NULL, "ii", 5, 3).integer, 2);
   ck_assert_int_eq(jit_call(j, modi, NULL, "ii", -5, 3).integer, 1);
   ck_assert_int_eq(jit_call(j, modi, NULL, "ii", -512, -8).integer, 0);
   ck_assert_int_eq(jit_call(j, modi, NULL, "ii", -510, -8).integer, -6);

   ident_t remi = ident_new("WORK.ARITH1.REMM(II)I");
   ck_assert_int_eq(jit_call(j, remi, NULL, "ii", 4, 2).integer, 0);
   ck_assert_int_eq(jit_call(j, remi, NULL, "ii", 5, 3).integer, 2);
   ck_assert_int_eq(jit_call(j, remi, NULL, "ii", -5, 3).integer, -2);

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

   ident_t fn1 = ident_new("WORK.ASSERT1.DO_REPORT");
   fail_unless(jit_try_call(j, fn1, NULL, &result, ""));

   ident_t fn2 = ident_new("WORK.ASSERT1.DO_ASSERT(I)");
   fail_unless(jit_try_call(j, fn2, NULL, &result, "i", 10));
   fail_unless(jit_try_call(j, fn2, NULL, &result, "i", -10));
   fail_if(jit_try_call(j, fn2, NULL, &result, "i", 555));

   jit_free(j);
   check_expected_errors();
}
END_TEST

START_TEST(test_case1)
{
   input_from_file(TESTDIR "/jit/case1.vhd");

   parse_check_simplify_and_lower(T_PACKAGE, T_PACK_BODY);

   jit_t *j = jit_new();

   ident_t test1 = ident_new("WORK.CASE1.TEST1(12WORK.CASE1.T)I");
   ck_assert_int_eq(jit_call(j, test1, NULL, "i", 0).integer, 10);
   ck_assert_int_eq(jit_call(j, test1, NULL, "i", 1).integer, 20);
   ck_assert_int_eq(jit_call(j, test1, NULL, "i", 2).integer, 30);

   const uint8_t one[] = { 0, 0, 0, 1 };
   const uint8_t eff[] = { 1, 1, 1, 1 };
   const uint8_t ten[] = { 1, 0, 1, 0 };

   ident_t test2 = ident_new("WORK.CASE1.TEST2(Q)I");
   ck_assert_int_eq(jit_call(j, test2, NULL, "p", one).integer, 1);
   ck_assert_int_eq(jit_call(j, test2, NULL, "p", eff).integer, 15);
   ck_assert_int_eq(jit_call(j, test2, NULL, "p", ten).integer, 10);

   jit_free(j);
   fail_if_errors();
}
END_TEST

START_TEST(test_real1)
{
   input_from_file(TESTDIR "/jit/real1.vhd");

   parse_check_simplify_and_lower(T_PACKAGE, T_PACK_BODY);

   jit_t *j = jit_new();

   ident_t approx = ident_new("WORK.REAL1.APPROX(RRR)B");
   ck_assert_int_eq(jit_call(j, approx, NULL, "RRR",
                             1.0, 1.0001, 0.001).integer, 1);

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

   jit_handle_t handle = jit_lazy_compile(j, ident_new("WORK.PROT1"));
   void *pkg = jit_link(j, handle);
   fail_if(pkg == NULL);

   ident_t fn = ident_new("WORK.PROT1.FETCH_AND_ADD(I)I");
   ck_assert_int_eq(jit_call(j, fn, pkg, "i", 0).integer, 0);
   ck_assert_int_eq(jit_call(j, fn, pkg, "i", 1).integer, 1);
   ck_assert_int_eq(jit_call(j, fn, pkg, "i", 1).integer, 2);
   ck_assert_int_eq(jit_call(j, fn, pkg, "i", 6).integer, 8);

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

   ident_t fn1 = ident_new("WORK.RANGE1.AS_POSITIVE(I)8POSITIVE");
   ck_assert_int_eq(jit_call(j, fn1, NULL, "i", 1).integer, 1);
   ck_assert_int_eq(jit_call(j, fn1, NULL, "i", 2).integer, 2);
   fail_if(jit_try_call(j, fn1, NULL, &result, "i", 0));
   fail_if(jit_try_call(j, fn1, NULL, &result, "i", -5));

   ident_t fn2 = ident_new("WORK.RANGE1.AS_POSITIVE(R)5PREAL");
   ck_assert_double_eq(jit_call(j, fn2, NULL, "R", 1.0).real, 1.0);
   ck_assert_double_eq(jit_call(j, fn2, NULL, "R", 1.001).real, 1.001);
   fail_if(jit_try_call(j, fn2, NULL, &result, "R", 0.0));
   fail_if(jit_try_call(j, fn2, NULL, &result, "R", -5.0));

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

   ident_t test1 = ident_new("WORK.TRACE1.TEST1");
   fail_if(jit_try_call(j, test1, NULL, &result, ""));

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

   jit_handle_t handle = jit_lazy_compile(j, ident_new("WORK.ISSUE496"));
   void *pkg = jit_link(j, handle);
   fail_if(pkg == NULL);

   jit_free(j);
   fail_if_errors();
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
   suite_add_tcase(s, tc);

   return s;
}
