//
//  Copyright (C) 2021-2025  Nick Gasson
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
#include "jit/jit-ffi.h"
#include "jit/jit-layout.h"
#include "jit/jit-priv.h"
#include "jit/jit.h"
#include "mask.h"
#include "object.h"
#include "option.h"
#include "phase.h"
#include "scan.h"
#include "type.h"

#include <math.h>
#include <stdlib.h>
#include <inttypes.h>

#define REG(r) ((jit_value_t){ .kind = JIT_VALUE_REG, .reg = (r) })
#define CONST(i) ((jit_value_t){ .kind = JIT_VALUE_INT64, .int64 = (i) })
#define LABEL(l) ((jit_value_t){ .kind = JIT_VALUE_LABEL, .label = (l) })
#define ADDR(r, d) \
   ((jit_value_t){ .kind = JIT_ADDR_REG, .reg = (r), .disp = (d) })

static jit_handle_t compile_for_test(jit_t *j, const char *name)
{
   return jit_lazy_compile(j, ident_new(name));
}

static inline int64_t extend_value(jit_value_t value)
{
   switch (value.kind) {
   case JIT_VALUE_LABEL: return value.label;
   case JIT_VALUE_HANDLE: return value.handle;
   case JIT_VALUE_REG: return value.reg;
   case JIT_ADDR_REG: return (int64_t)value.reg << 32 | value.disp;
   default: return value.int64;
   }
}

static void check_nullary(jit_func_t *f, int nth, jit_op_t expect)
{
   jit_ir_t *ir = &(f->irbuf[nth]);
   if (ir->op == expect)
      return;

   jit_dump_with_mark(f, nth);

   if (ir->op != expect)
      ck_abort_msg("expected op %s but have %s", jit_op_name(expect),
                   jit_op_name(ir->op));
}

static void check_unary(jit_func_t *f, int nth, jit_op_t expect,
                        jit_value_t arg1)
{
   jit_ir_t *ir = &(f->irbuf[nth]);
   if (ir->op == expect && ir->arg1.kind == arg1.kind
       && extend_value(ir->arg1) == extend_value(arg1))
      return;

   jit_dump_with_mark(f, nth);

   if (ir->op != expect)
      ck_abort_msg("expected op %s but have %s", jit_op_name(expect),
                   jit_op_name(ir->op));
   else if (ir->arg1.kind != arg1.kind)
      ck_abort_msg("expected arg1 kind %d but have %d", arg1.kind,
                   ir->arg1.kind);
   else
      ck_abort_msg("expected arg1 value %"PRIi64" but have %"PRIi64,
                   extend_value(arg1), extend_value(ir->arg1));
}

static void check_binary(jit_func_t *f, int nth, jit_op_t expect,
                         jit_value_t arg1, jit_value_t arg2)
{
   jit_ir_t *ir = &(f->irbuf[nth]);
   if (ir->op == expect && ir->arg1.kind == arg1.kind
       && extend_value(ir->arg1) == extend_value(arg1)
       && ir->arg2.kind == arg2.kind
       && extend_value(ir->arg2) == extend_value(arg2))
      return;

   jit_dump_with_mark(f, nth);

   if (ir->op != expect)
      ck_abort_msg("expected op %s but have %s", jit_op_name(expect),
                   jit_op_name(ir->op));
   else if (ir->arg1.kind != arg1.kind)
      ck_abort_msg("expected arg1 kind %d but have %d", arg1.kind,
                   ir->arg1.kind);
   else if (ir->arg1.int64 != arg1.int64)
      ck_abort_msg("expected arg1 value %"PRIi64" but have %"PRIi64,
                   extend_value(arg1), extend_value(ir->arg1));
   else if (ir->arg2.kind != arg2.kind)
      ck_abort_msg("expected arg2 kind %d but have %d", arg2.kind,
                   ir->arg2.kind);
   else
      ck_abort_msg("expected arg2 value %"PRIi64" but have %"PRIi64,
                   extend_value(arg2), extend_value(ir->arg2));
}

START_TEST(test_add1)
{
   input_from_file(TESTDIR "/jit/add1.vhd");

   parse_check_and_simplify(T_PACKAGE, T_PACK_BODY);

   jit_t *j = jit_new(get_registry(), get_mir(), NULL);

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

   parse_check_and_simplify(T_PACKAGE, T_PACK_BODY);

   jit_t *j = jit_new(get_registry(), get_mir(), NULL);

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

   parse_check_and_simplify(T_PACKAGE, T_PACK_BODY);

   jit_t *j = jit_new(get_registry(), get_mir(), NULL);

   int32_t data[] = { 1, 2, 3, 4, 5 };

   jit_handle_t fn1 =
      compile_for_test(j, "WORK.SUMPKG.GET_LEFT(22WORK.SUMPKG.INT_VECTOR)I");
   ck_assert_int_eq(jit_call(j, fn1, NULL, data, 1, 5).integer, 1);
   ck_assert_int_eq(jit_call(j, fn1, NULL, data, -5, 4).integer, -5);

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
   ck_assert_int_eq(jit_call(j, fn3, NULL, data, -5, -2).integer, 1);

   jit_handle_t fn4 =
      compile_for_test(j, "WORK.SUMPKG.SUM(22WORK.SUMPKG.INT_VECTOR)I");
   ck_assert_int_eq(jit_call(j, fn4, NULL, data, 1, 5).integer, 15);
   ck_assert_int_eq(jit_call(j, fn4, NULL, data, 5, -6).integer, 15);
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

   parse_check_and_simplify(T_PACKAGE, T_PACK_BODY);

   jit_t *j = jit_new(get_registry(), get_mir(), NULL);

   jit_handle_t handle = jit_lazy_compile(j, ident_new("WORK.PACK"));

   void *ctx = jit_link(j, handle);
   fail_if(ctx == NULL);

   int32_t *c1 = jit_get_frame_var(j, handle, ident_new("C1"));
   ck_assert_int_eq(*c1, 42);

   jit_handle_t fn1 = compile_for_test(j, "WORK.PACK.GET_ELT(N)I");
   ck_assert_int_eq(jit_call(j, fn1, ctx, 1).integer, 10);
   ck_assert_int_eq(jit_call(j, fn1, ctx, 2).integer, 20);

   jit_scalar_t result;
   fail_if(jit_try_call(j, fn1, &result, NULL, 55));
   fail_if(jit_try_call(j, fn1, &result, NULL, -1));

   jit_handle_t fn2 =
      compile_for_test(j, "WORK.PACK.NESTED_GET_ELT(N)I");
   ck_assert_int_eq(jit_call(j, fn2, ctx, 1).integer, 10);
   ck_assert_int_eq(jit_call(j, fn2, ctx, 5).integer, 50);
   fail_if(jit_try_call(j, fn2, &result, NULL, -1));

   int x;
   jit_handle_t fn3 = compile_for_test(j, "WORK.PACK.READ_ELT(NI)");
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

   parse_check_and_simplify(T_PACKAGE, T_PACK_BODY, T_PACKAGE, T_PACK_BODY);

   jit_t *j = jit_new(get_registry(), get_mir(), NULL);

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

   parse_check_and_simplify(T_PACKAGE, T_PACK_BODY, T_PACKAGE, T_PACK_BODY);

   jit_t *j = jit_new(get_registry(), get_mir(), NULL);

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

   parse_check_and_simplify(T_PACKAGE, T_PACK_BODY, T_PACKAGE, T_PACK_BODY);

   jit_t *j = jit_new(get_registry(), get_mir(), NULL);

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
void _nvc_ieee_warnings(jit_scalar_t *args)
{
   args[0].integer = IEEE_WARNINGS_ON;
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

   jit_t *j = get_jit();

   jit_handle_t handle = jit_compile(j, ident_new("WORK.IEEEWARN"));

   void *pkg = jit_link(j, handle);
   fail_if(pkg == NULL);

   uint8_t *p = jit_get_frame_var(j, handle, ident_new("ENABLED"));
   ck_assert_int_eq(*p, 1);
}
END_TEST

START_TEST(test_overflow)
{
   input_from_file(TESTDIR "/jit/overflow.vhd");

   const error_t expect[] = {
      { 18, "result of 2147483647 + 1 cannot be represented as INTEGER" },
      { 18, "result of 2147483647 + 2147483647 cannot be represented as " },
      { 23, "result of -2147483648 - 53 cannot be represented as INTEGER" },
      { 28, "result of -1942444142 * 128910 cannot be represented as INTEGER" },
      { 33, "result of 5 ** 60 cannot be represented as INTEGER" },
      { 38, "result of 255 + 128 cannot be represented as UINT8" },
      { 43, "result of 2 - 3 cannot be represented as UINT8" },
      { 48, "result of 255 * 2 cannot be represented as UINT8" },
      { 53, "result of 15 ** 26 cannot be represented as UINT8" },
      { -1, NULL },
   };
   expect_errors(expect);

   parse_check_and_simplify(T_PACKAGE, T_PACK_BODY);

   jit_t *j = jit_new(get_registry(), get_mir(), NULL);

   jit_handle_t add = compile_for_test(j, "WORK.OVERFLOW.ADD(II)I");
   jit_handle_t sub = compile_for_test(j, "WORK.OVERFLOW.SUB(II)I");
   jit_handle_t mul = compile_for_test(j, "WORK.OVERFLOW.MUL(II)I");
   jit_handle_t exp = compile_for_test(j, "WORK.OVERFLOW.EXP(II)I");

   jit_scalar_t result;
   fail_if(jit_try_call(j, add, &result, NULL, INT32_MAX, 1));
   fail_if(jit_try_call(j, add, &result, NULL, INT32_MAX, INT32_MAX));
   fail_if(jit_try_call(j, sub, &result, NULL, INT32_MIN, 53));
   fail_if(jit_try_call(j, mul, &result, NULL, 2352523154, 128910));
   fail_if(jit_try_call(j, exp, &result, NULL, 5, 60));

#define UINT8 "19WORK.OVERFLOW.UINT8"
   jit_handle_t addu =
      compile_for_test(j, "WORK.OVERFLOW.ADD(" UINT8 UINT8 ")" UINT8);
   jit_handle_t subu =
      compile_for_test(j, "WORK.OVERFLOW.SUB(" UINT8 UINT8 ")" UINT8);
   jit_handle_t mulu =
      compile_for_test(j, "WORK.OVERFLOW.MUL(" UINT8 UINT8 ")" UINT8);
   jit_handle_t expu =
      compile_for_test(j, "WORK.OVERFLOW.EXP(" UINT8 "I)" UINT8);
#undef UINT8

   ck_assert_int_eq(jit_call(j, addu, NULL, 5, 6).integer, 11);
   ck_assert_int_eq(jit_call(j, addu, NULL, 127, 1).integer, 128);
   fail_if(jit_try_call(j, addu, &result, NULL, 255, 128));

   ck_assert_int_eq(jit_call(j, subu, NULL, 255, 4).integer, 251);
   ck_assert_int_eq(jit_call(j, subu, NULL, 1, 1).integer, 0);
   fail_if(jit_try_call(j, subu, &result, NULL, 2, 3));

   ck_assert_int_eq(jit_call(j, mulu, NULL, 127, 2).integer, 254);
   fail_if(jit_try_call(j, mulu, &result, NULL, 255, 2));

   ck_assert_int_eq(jit_call(j, expu, NULL, 3, 3).integer, 27);
   fail_if(jit_try_call(j, expu, &result, NULL, 15, 26));

   jit_free(j);
   check_expected_errors();
}
END_TEST

START_TEST(test_record4)
{
   input_from_file(TESTDIR "/jit/record4.vhd");

   parse_check_and_simplify(T_PACKAGE, T_PACK_BODY);

   jit_t *j = jit_new(get_registry(), get_mir(), NULL);

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
      { 44, "out of memory attempting to allocate 1032 byte " },
      { -1, NULL },
   };
   expect_errors(expect);

   parse_check_and_simplify(T_PACKAGE, T_PACK_BODY);

   jit_t *j = jit_new(get_registry(), get_mir(), NULL);

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
      { 40, "" },
      { -1, NULL },
   };
   expect_errors(expect);

   parse_check_and_simplify(T_PACKAGE, T_PACK_BODY);

   jit_t *j = jit_new(get_registry(), get_mir(), NULL);

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

   parse_check_and_simplify(T_PACKAGE, T_PACK_BODY);

   jit_t *j = jit_new(get_registry(), get_mir(), NULL);

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

   parse_check_and_simplify(T_PACKAGE, T_PACK_BODY,
                            T_PACKAGE, T_PACK_BODY,
                            T_PACKAGE, T_PACK_BODY);

   jit_t *j = jit_new(get_registry(), get_mir(), NULL);

   jit_handle_t add2 = compile_for_test(j, "WORK.PROC1_PACK2.ADD2(I)I");

   ck_assert_int_eq(jit_call(j, add2, NULL, 5).integer, 7);

   jit_scalar_t result;
   fail_if(jit_try_call(j, add2, &result, NULL, 10));

   jit_free(j);
   check_expected_errors();
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

   parse_check_and_simplify(T_PACKAGE, T_PACK_BODY);

   jit_t *j = jit_new(get_registry(), get_mir(), NULL);

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
      { 35, "value inf outside of REAL range" },
      { 45, "negative exponent -1 only allowed for floating-point types" },
      { 50, "result of -(-2147483648) cannot be represented as INTEGER" },
      { -1, NULL },
   };
   expect_errors(expect);

   parse_check_and_simplify(T_PACKAGE, T_PACK_BODY);

   jit_t *j = jit_new(get_registry(), get_mir(), NULL);

   jit_scalar_t result;

   jit_handle_t divii = compile_for_test(j, "WORK.ARITH1.DIV(II)I");
   ck_assert_int_eq(jit_call(j, divii, NULL, 4, 2).integer, 2);
   ck_assert_int_eq(jit_call(j, divii, NULL, 5, 2).integer, 2);
   ck_assert_int_eq(jit_call(j, divii, NULL, -5, -2).integer, 2);
   ck_assert_int_eq(jit_call(j, divii, NULL, -5, -1).integer, 5);
   fail_if(jit_try_call(j, divii, &result, NULL, 1, 0));

   jit_handle_t divrr = compile_for_test(j, "WORK.ARITH1.DIV(RR)R");
   ck_assert_double_eq(jit_call(j, divrr, NULL, 4.0, 2.0).real, 2.0);
   ck_assert_double_eq(jit_call(j, divrr, NULL, 4.0, 0.5).real, 8.0);

   jit_handle_t divir = compile_for_test(j, "WORK.ARITH1.DIV(IR)R");
   ck_assert_double_eq(jit_call(j, divir, NULL, 4, 2.0).real, 2.0);
   fail_if(jit_try_call(j, divir, &result, NULL, 4, 0.0));

   jit_handle_t expr = compile_for_test(j, "WORK.ARITH1.EXP(RI)R");
   ck_assert_double_eq(jit_call(j, expr, NULL, 2.0, 4).real, 16.0);
   ck_assert_double_eq(jit_call(j, expr, NULL, 2.0, -1).real, 0.5);

   jit_handle_t expi = compile_for_test(j, "WORK.ARITH1.EXP(II)I");
   ck_assert_int_eq(jit_call(j, expi, NULL, 2, 4).integer, 16);
   fail_if(jit_try_call(j, expi, &result, NULL, 2, -1));

   jit_handle_t negi = compile_for_test(j, "WORK.ARITH1.NEG(I)I");
   ck_assert_int_eq(jit_call(j, negi, NULL, 2).integer, -2);
   ck_assert_int_eq(jit_call(j, negi, NULL, -124).integer, 124);
   fail_if(jit_try_call(j, negi, &result, NULL, INT32_MIN));

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
      { 10, "hello world" },
      { 15, "-10 negative" },
      { 16, "too big" },
      { -1, NULL },
   };
   expect_errors(expect);

   parse_check_and_simplify(T_PACKAGE, T_PACK_BODY);

   jit_t *j = jit_new(get_registry(), get_mir(), NULL);

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

   parse_check_and_simplify(T_PACKAGE, T_PACK_BODY);

   jit_t *j = jit_new(get_registry(), get_mir(), NULL);

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

   parse_check_and_simplify(T_PACKAGE, T_PACK_BODY);

   jit_t *j = jit_new(get_registry(), get_mir(), NULL);

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

   parse_check_and_simplify(T_PACKAGE, T_PACK_BODY);

   jit_t *j = jit_new(get_registry(), get_mir(), NULL);

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

START_TEST(test_layout)
{
   jit_t *j = jit_new(NULL, NULL, NULL);
   const jit_layout_t *l = NULL;

   l = layout_of(std_type(NULL, STD_INTEGER));
   ck_assert_int_eq(l->nparts, 1);
   ck_assert_int_eq(l->parts[0].offset, 0);
   ck_assert_int_eq(l->parts[0].size, 4);
   ck_assert_int_eq(l->parts[0].repeat, 1);
   ck_assert_int_eq(l->parts[0].align, 4);

   l = signal_layout_of(std_type(NULL, STD_INTEGER));
   ck_assert_int_eq(l->nparts, 2);
   ck_assert_int_eq(l->parts[0].offset, 0);
   ck_assert_int_eq(l->parts[0].size, sizeof(void *));
   ck_assert_int_eq(l->parts[0].repeat, 1);
   ck_assert_int_eq(l->parts[0].align, 8);
   ck_assert_int_eq(l->parts[1].offset, 8);
   ck_assert_int_eq(l->parts[1].size, 8);
   ck_assert_int_eq(l->parts[1].repeat, 1);
   ck_assert_int_eq(l->parts[1].align, 8);

   l = layout_of(std_type(NULL, STD_REAL));
   ck_assert_int_eq(l->nparts, 1);
   ck_assert_int_eq(l->parts[0].offset, 0);
   ck_assert_int_eq(l->parts[0].size, 8);
   ck_assert_int_eq(l->parts[0].repeat, 1);
   ck_assert_int_eq(l->parts[0].align, 8);

   l = layout_of(std_type(NULL, STD_TIME));
   ck_assert_int_eq(l->nparts, 1);
   ck_assert_int_eq(l->parts[0].offset, 0);
   ck_assert_int_eq(l->parts[0].size, 8);
   ck_assert_int_eq(l->parts[0].repeat, 1);
   ck_assert_int_eq(l->parts[0].align, 8);

   l = layout_of(std_type(NULL, STD_BOOLEAN));
   ck_assert_int_eq(l->nparts, 1);
   ck_assert_int_eq(l->parts[0].offset, 0);
   ck_assert_int_eq(l->parts[0].size, 1);
   ck_assert_int_eq(l->parts[0].repeat, 1);
   ck_assert_int_eq(l->parts[0].align, 1);

   l = layout_of(std_type(NULL, STD_STRING));
   ck_assert_int_eq(l->nparts, 2);
   ck_assert_int_eq(l->parts[0].offset, 0);
   ck_assert_int_eq(l->parts[0].size, sizeof(void *));
   ck_assert_int_eq(l->parts[0].repeat, 1);
   ck_assert_int_eq(l->parts[0].align, 8);
   ck_assert_int_eq(l->parts[1].offset, sizeof(void *));
   ck_assert_int_eq(l->parts[1].size, sizeof(int64_t));
   ck_assert_int_eq(l->parts[1].repeat, 2);

   l = signal_layout_of(std_type(NULL, STD_STRING));
   ck_assert_int_eq(l->nparts, 3);
   ck_assert_int_eq(l->parts[0].offset, 0);
   ck_assert_int_eq(l->parts[0].size, sizeof(void *));
   ck_assert_int_eq(l->parts[0].repeat, 1);
   ck_assert_int_eq(l->parts[0].align, 8);
   ck_assert_int_eq(l->parts[1].offset, 8);
   ck_assert_int_eq(l->parts[1].size, 8);
   ck_assert_int_eq(l->parts[1].repeat, 1);
   ck_assert_int_eq(l->parts[1].align, 8);
   ck_assert_int_eq(l->parts[2].offset, 16);
   ck_assert_int_eq(l->parts[2].size, sizeof(int64_t));
   ck_assert_int_eq(l->parts[2].repeat, 2);

   input_from_file(TESTDIR "/jit/layout.vhd");

   tree_t p = parse_check_and_simplify(T_PACKAGE);

   freeze_global_arena();
   fail_unless(parse() == NULL);

   type_t r1 = tree_type(get_decl(p, "R1"));

   l = layout_of(r1);
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

   type_t r2 = tree_type(get_decl(p, "R2"));

   l = layout_of(r2);
   ck_assert_int_eq(l->nparts, 2);
   ck_assert_int_eq(l->parts[0].offset, 0);
   ck_assert_int_eq(l->parts[0].size, 16);
   ck_assert_int_eq(l->parts[0].repeat, 1);
   ck_assert_int_eq(l->parts[0].align, 8);
   ck_assert_int_eq(l->parts[1].offset, 16);
   ck_assert_int_eq(l->parts[1].size, 4);
   ck_assert_int_eq(l->parts[1].repeat, 1);

   type_t a = tree_type(get_decl(p, "A"));

   l = layout_of(a);
   ck_assert_int_eq(l->nparts, 1);
   ck_assert_int_eq(l->parts[0].offset, 0);
   ck_assert_int_eq(l->parts[0].size, 1);
   ck_assert_int_eq(l->parts[0].repeat, 5);
   ck_assert_int_eq(l->parts[0].align, 1);

   type_t bv = tree_type(get_decl(p, "T_BYTE_VECTOR"));

   l = layout_of(bv);
   ck_assert_int_eq(l->nparts, 2);
   ck_assert_int_eq(l->size, sizeof(void *) + 2*sizeof(int64_t));
   ck_assert_int_eq(l->parts[0].offset, 0);
   ck_assert_int_eq(l->parts[0].size, sizeof(void *));
   ck_assert_int_eq(l->parts[0].align, sizeof(void *));
   ck_assert_int_eq(l->parts[1].offset, sizeof(void *));
   ck_assert_int_eq(l->parts[1].size, sizeof(int64_t));
   ck_assert_int_eq(l->parts[1].repeat, 2);

   type_t c1 = tree_type(get_decl(p, "C1"));

   l = layout_of(c1);
   ck_assert_int_eq(l->nparts, 1);
   ck_assert_int_eq(l->parts[0].offset, 0);
   ck_assert_int_eq(l->parts[0].size, 1);
   ck_assert_int_eq(l->parts[0].align, 1);
   ck_assert_int_eq(l->parts[0].repeat, 3 * 8);

   type_t t_array = tree_type(get_decl(p, "T_ARRAY"));

   l = signal_layout_of(t_array);
   ck_assert_int_eq(l->nparts, 2);
   ck_assert_int_eq(l->parts[0].offset, 0);
   ck_assert_int_eq(l->parts[0].size, 8);
   ck_assert_int_eq(l->parts[0].align, sizeof(void *));
   ck_assert_int_eq(l->parts[0].repeat, 1);
   ck_assert_int_eq(l->parts[1].offset, 8);
   ck_assert_int_eq(l->parts[1].size, 8);
   ck_assert_int_eq(l->parts[1].align, 8);
   ck_assert_int_eq(l->parts[1].repeat, 2);

   l = signal_layout_of(r1);
   ck_assert_int_eq(l->nparts, 3);
   for (int i = 0; i < 3; i++) {
      ck_assert_int_eq(l->parts[i].offset, 16 * i);
      ck_assert_int_eq(l->parts[i].size, 16);
      ck_assert_int_eq(l->parts[i].align, sizeof(void *));
      ck_assert_int_eq(l->parts[i].repeat, 1);
   }

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

   parse_check_and_simplify(T_PACKAGE, T_PACK_BODY);

   jit_t *j = jit_new(get_registry(), get_mir(), NULL);

   jit_scalar_t result;

   jit_handle_t fn1 =
      compile_for_test(j, "WORK.RANGE1.AS_POSITIVE(I)P");
   ck_assert_int_eq(jit_call(j, fn1, NULL, 1).integer, 1);
   ck_assert_int_eq(jit_call(j, fn1, NULL, 2).integer, 2);
   fail_if(jit_try_call(j, fn1, &result, NULL, 0));
   fail_if(jit_try_call(j, fn1, &result, NULL, -5));

   jit_handle_t fn2 =
      compile_for_test(j, "WORK.RANGE1.AS_POSITIVE(R)17WORK.RANGE1.PREAL");
   ck_assert_double_eq(jit_call(j, fn2, NULL, 1.0).real, 1.0);
   ck_assert_double_eq(jit_call(j, fn2, NULL, 1.001).real, 1.001);
   fail_if(jit_try_call(j, fn2, &result, NULL, 0.0));
   fail_if(jit_try_call(j, fn2, &result, NULL, -5.0));

   jit_free(j);
   check_expected_errors();
}
END_TEST

static void trace1_diag_fn(diag_t *d, void *context)
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

   diag_set_consumer(trace1_diag_fn, NULL);

   parse_check_and_simplify(T_PACKAGE, T_PACK_BODY);

   jit_t *j = jit_new(get_registry(), get_mir(), NULL);

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

   parse_check_and_simplify(T_PACKAGE, T_PACKAGE);

   jit_t *j = jit_new(get_registry(), get_mir(), NULL);

   jit_handle_t handle = compile_for_test(j, "WORK.ISSUE496");
   void *pkg = jit_link(j, handle);
   fail_if(pkg == NULL);

   char *c = jit_get_frame_var(j, handle, ident_new("C"));
   ck_assert_mem_eq(c, "one", 3);

   jit_free(j);
   fail_if_errors();
}
END_TEST

START_TEST(test_value1)
{
   input_from_file(TESTDIR "/jit/value1.vhd");

   const error_t expect[] = {
      { 257, "found invalid characters \"x\" after value \"42x\"" },
      { 405, "found invalid characters \".4\" after value \"4..4\"" },
      { 80, "\" FOO\" is not a valid unit name" },
      { 23, "\"FOO\" is not a valid enumeration value" },
      { -1, NULL },
   };
   expect_errors(expect);

   parse_check_and_simplify(T_PACKAGE, T_PACK_BODY);

   jit_t *j = jit_new(get_registry(), get_mir(), NULL);

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

START_TEST(test_assemble1)
{
   jit_t *j = jit_new(NULL, NULL, NULL);

   const char *text1 =
      "RECV  R0, #0       \n"
      "MOV   R1, #2       \n"
      "ADD   R1, R0, R1   \n"
      "SEND  #0, R1       \n"
      "RET                \n";

   jit_handle_t h1 = jit_assemble(j, ident_new("myfunc"), text1);

   tlab_t tlab = jit_null_tlab(j);
   jit_scalar_t result, p0 = { .integer = 5 };
   fail_unless(jit_fastcall(j, h1, &result, p0, p0, &tlab));

   ck_assert_int_eq(result.integer, 7);

   jit_free(j);
}
END_TEST

START_TEST(test_assemble2)
{
   jit_t *j = jit_new(NULL, NULL, NULL);

   const char *text1 =
      "    RECV    R0, #0       \n"
      "L1: CMP.EQ  R0, #0       \n"
      "    JUMP.T  L2           \n"
      "    SUB     R0, R0, #1   \n"
      "    JUMP    L1           \n"
      "L2: SEND    #0, R0       \n"
      "    RET                  \n";

   jit_handle_t h1 = jit_assemble(j, ident_new("myfunc"), text1);

   tlab_t tlab = jit_null_tlab(j);
   jit_scalar_t result, p0 = { .integer = 5 };
   fail_unless(jit_fastcall(j, h1, &result, p0, p0, &tlab));

   ck_assert_int_eq(result.integer, 0);

   jit_free(j);
}
END_TEST

START_TEST(test_cfg1)
{
   jit_t *j = jit_new(NULL, NULL, NULL);

   const char *text1 =
      "    RECV    R0, #0       \n"
      "    MOV     R1, #1       \n"
      "L1: CMP.EQ  R0, #0       \n"
      "    JUMP.T  L2           \n"
      "    MUL     R1, R1, R0   \n"
      "    SUB     R0, R0, #1   \n"
      "    JUMP    L1           \n"
      "L2: SEND    #0, R1       \n"
      "    RET                  \n";

   jit_handle_t h1 = jit_assemble(j, ident_new("myfunc"), text1);

   tlab_t tlab = jit_null_tlab(j);
   jit_scalar_t result, p0 = { .integer = 5 };
   fail_unless(jit_fastcall(j, h1, &result, p0, p0, &tlab));

   ck_assert_int_eq(result.integer, 120);

   jit_func_t *f = jit_get_func(j, h1);
   jit_cfg_t *cfg = jit_get_cfg(f);

   ck_assert_int_eq(cfg->nblocks, 4);

   ck_assert_int_eq(cfg->blocks[0].returns, 0);
   ck_assert_int_eq(cfg->blocks[0].aborts, 0);
   ck_assert_int_eq(cfg->blocks[0].in.count, 0);
   ck_assert_int_eq(cfg->blocks[0].out.count, 1);
   ck_assert_int_eq(jit_get_edge(&cfg->blocks[0].out, 0), 1);
   ck_assert_int_eq(cfg->blocks[0].livein.size, 3);
   ck_assert_int_eq(cfg->blocks[0].livein.bits, 0);
   ck_assert_int_eq(cfg->blocks[0].liveout.size, 3);
   ck_assert_int_eq(cfg->blocks[0].liveout.bits, 0x3);
   ck_assert_int_eq(cfg->blocks[0].varkill.size, 3);
   ck_assert_int_eq(cfg->blocks[0].varkill.bits, 0x3);

   ck_assert_int_eq(cfg->blocks[1].in.count, 2);
   ck_assert_int_eq(jit_get_edge(&cfg->blocks[1].in, 0), 0);
   ck_assert_int_eq(jit_get_edge(&cfg->blocks[1].in, 1), 2);
   ck_assert_int_eq(cfg->blocks[1].out.count, 2);
   ck_assert_int_eq(jit_get_edge(&cfg->blocks[1].out, 0), 2);
   ck_assert_int_eq(jit_get_edge(&cfg->blocks[1].out, 1), 3);
   ck_assert_int_eq(cfg->blocks[1].livein.bits, 0x3);
   ck_assert_int_eq(cfg->blocks[1].liveout.bits, 0x3);
   ck_assert_int_eq(cfg->blocks[1].varkill.bits, 0b100);

   ck_assert_int_eq(cfg->blocks[2].in.count, 1);
   ck_assert_int_eq(jit_get_edge(&cfg->blocks[2].in, 0), 1);
   ck_assert_int_eq(cfg->blocks[2].out.count, 1);
   ck_assert_int_eq(jit_get_edge(&cfg->blocks[2].out, 0), 1);
   ck_assert_int_eq(cfg->blocks[2].livein.bits, 0x3);
   ck_assert_int_eq(cfg->blocks[2].liveout.bits, 0x3);
   ck_assert_int_eq(cfg->blocks[2].varkill.bits, 0x3);

   ck_assert_int_eq(cfg->blocks[3].returns, 1);
   ck_assert_int_eq(cfg->blocks[3].in.count, 1);
   ck_assert_int_eq(jit_get_edge(&cfg->blocks[3].in, 0), 1);
   ck_assert_int_eq(cfg->blocks[3].out.count, 0);
   ck_assert_int_eq(cfg->blocks[3].livein.bits, 0x2);
   ck_assert_int_eq(cfg->blocks[3].liveout.bits, 0);
   ck_assert_int_eq(cfg->blocks[3].varkill.bits, 0);

   jit_free(j);
}
END_TEST

START_TEST(test_lvn1)
{
   jit_t *j = jit_new(NULL, NULL, NULL);

   const char *text1 =
      "    MUL     R0, #2, #3    \n"
      "    MUL.8   R0, #100, #5  \n"
      "    ADD     R0, #5, #-7   \n"
      "    SUB     R0, #12, #30  \n"
      "    ADD     R1, R2, R3    \n"
      "    ADD     R4, R2, R3    \n"
      "    MUL     R1, R2, #3    \n"
      "    MUL     R4, #3, R2    \n"
      "    MOV     R1, #666      \n"
      "    MUL     R5, #3, R2    \n"
      "    MUL     R1, #1, R2    \n"
      "    MUL     R1, #0, R2    \n"
      "    ADD     R1, R1, #0    \n"
      "    SUB     R1, R1, #0    \n"
      "    SUB     R2, #0, R2    \n"
      "    SUB.O   R2, #0, R2    \n"
      "    $EXP    R3, #3, #4    \n"
      "    $EXP    R3, #2, R2    \n"
      "    RET                   \n";

   jit_handle_t h1 = jit_assemble(j, ident_new("myfunc"), text1);

   jit_func_t *f = jit_get_func(j, h1);
   jit_do_lvn(f);

   ck_assert_int_eq(f->irbuf[0].op, J_MOV);
   ck_assert_int_eq(f->irbuf[0].arg1.int64, 6);

   ck_assert_int_eq(f->irbuf[1].op, J_MUL);
   ck_assert_int_eq(f->irbuf[1].size, JIT_SZ_8);

   ck_assert_int_eq(f->irbuf[2].op, J_MOV);
   ck_assert_int_eq(f->irbuf[2].arg1.int64, -2);

   ck_assert_int_eq(f->irbuf[3].op, J_MOV);
   ck_assert_int_eq(f->irbuf[3].arg1.int64, -18);

   ck_assert_int_eq(f->irbuf[5].op, J_MOV);
   ck_assert_int_eq(f->irbuf[5].arg1.reg, f->irbuf[4].result);

   check_unary(f, 7, J_MOV, REG(f->irbuf[6].result));
   check_binary(f, 9, J_MUL, REG(2), CONST(3));

   ck_assert_int_eq(f->irbuf[10].op, J_MOV);
   ck_assert_int_eq(f->irbuf[10].arg1.reg, 2);

   ck_assert_int_eq(f->irbuf[11].op, J_MOV);
   ck_assert_int_eq(f->irbuf[11].arg1.int64, 0);

   ck_assert_int_eq(f->irbuf[12].op, J_NOP);

   ck_assert_int_eq(f->irbuf[13].op, J_NOP);

   ck_assert_int_eq(f->irbuf[14].op, J_NEG);
   ck_assert_int_eq(f->irbuf[14].arg1.kind, JIT_VALUE_REG);
   ck_assert_int_eq(f->irbuf[14].arg1.reg, 2);

   ck_assert_int_eq(f->irbuf[15].op, J_SUB);

   ck_assert_int_eq(f->irbuf[16].op, J_MOV);
   ck_assert_int_eq(f->irbuf[16].arg1.int64, 81);

   ck_assert_int_eq(f->irbuf[17].op, J_SHL);
   ck_assert_int_eq(f->irbuf[17].arg1.int64, 1);

   jit_free(j);
}
END_TEST

START_TEST(test_lvn2)
{
   jit_t *j = jit_new(NULL, NULL, NULL);

   const char *text1 =
      "    MOV     R0, #2        \n"
      "    ADD     R1, R0, #1    \n"
      "L1: ADD     R1, R0, #1    \n"
      "    ADD     R0, R0, #1    \n"
      "    JUMP    L1            \n";

   jit_handle_t h1 = jit_assemble(j, ident_new("myfunc"), text1);

   jit_func_t *f = jit_get_func(j, h1);
   jit_do_lvn(f);

   ck_assert_int_eq(f->irbuf[2].op, J_ADD);
   ck_assert_int_eq(f->irbuf[2].arg1.reg, 0);
   ck_assert_int_eq(f->irbuf[2].arg2.int64, 1);

   ck_assert_int_eq(f->irbuf[3].op, J_MOV);
   ck_assert_int_eq(f->irbuf[3].arg1.reg, 1);

   jit_free(j);
}
END_TEST

START_TEST(test_lvn3)
{
   jit_t *j = jit_new(NULL, NULL, NULL);

   const char *text1 =
      "    MOV     R0, #2          \n"
      "    $COPY   R0, [R1], [R2]  \n"
      "    MOV     R0, #2          \n"
      "    $COPY   R0, [R3], [R4]  \n"
      "    SUB     R46, #1, R41    \n"
      "    ADD     R58, R41, #1    \n";

   jit_handle_t h1 = jit_assemble(j, ident_new("myfunc"), text1);

   jit_func_t *f = jit_get_func(j, h1);
   jit_do_lvn(f);

   ck_assert_int_eq(f->irbuf[2].op, J_MOV);
   ck_assert_int_eq(f->irbuf[2].arg1.kind, JIT_VALUE_INT64);
   ck_assert_int_eq(f->irbuf[2].arg1.int64, 2);

   ck_assert_int_eq(f->irbuf[5].op, J_ADD);
   ck_assert_int_eq(f->irbuf[5].arg2.kind, JIT_VALUE_INT64);
   ck_assert_int_eq(f->irbuf[5].arg2.int64, 1);

   jit_free(j);
}
END_TEST

START_TEST(test_issue575)
{
   input_from_file(TESTDIR "/jit/issue575.vhd");

   parse_check_and_simplify(T_PACKAGE, T_PACK_BODY);

   jit_t *j = jit_new(get_registry(), get_mir(), NULL);

   struct {
      int32_t x;
      uint8_t y[3];
   } rec = { 42, { 0, 0, 0 } };

   jit_handle_t fn =
      compile_for_test(j, "WORK.ISSUE575.TEST(17WORK.ISSUE575.RECJ)");
   jit_call(j, fn, NULL, NULL, &rec, 1);

   ck_assert_int_eq(rec.x, 42);
   for (int i = 0; i < 3; i++)
      ck_assert_int_eq(rec.y[i], 1);

   jit_free(j);
}
END_TEST

START_TEST(test_cfg2)
{
   jit_t *j = jit_new(NULL, NULL, NULL);

   const char *text1 =
      "    RECV    R0, #0       \n"
      "    $CASE   R0, #1, L1   \n"
      "    $CASE   R0, #2, L2   \n"
      "    $CASE   R0, #3, L3   \n"
      "    JUMP    L4           \n"
      "L1: RET                  \n"
      "L2: RET                  \n"
      "L3: RET                  \n"
      "L4: RET                  \n";

   jit_handle_t h1 = jit_assemble(j, ident_new("myfunc1"), text1);

   tlab_t tlab = jit_null_tlab(j);
   jit_scalar_t result, p0 = { .integer = 5 };
   fail_unless(jit_fastcall(j, h1, &result, p0, p0, &tlab));

   ck_assert_int_eq(result.integer, 5);

   jit_func_t *f1 = jit_get_func(j, h1);
   jit_cfg_t *cfg1 = jit_get_cfg(f1);

   ck_assert_int_eq(cfg1->nblocks, 6);
   ck_assert_int_eq(cfg1->blocks[1].first, 4);

   const char *text2 =
      "    RECV    R0, #0       \n"
      "    $CASE   R0, #2, L1   \n"
      "    $CASE   R0, #3, L2   \n"
      "    SEND    #0, #55      \n"
      "    RET                  \n"
      "L1: RET                  \n"
      "L2: RET                  \n";

   jit_handle_t h2 = jit_assemble(j, ident_new("myfunc2"), text2);

   fail_unless(jit_fastcall(j, h2, &result, p0, p0, &tlab));
   ck_assert_int_eq(result.integer, 55);

   jit_func_t *f2 = jit_get_func(j, h2);
   jit_cfg_t *cfg2 = jit_get_cfg(f2);

   ck_assert_int_eq(cfg2->nblocks, 4);
   ck_assert_int_eq(cfg2->blocks[1].first, 3);

   jit_free(j);
}
END_TEST

START_TEST(test_lvn4)
{
   jit_t *j = jit_new(NULL, NULL, NULL);

   const char *text1 =
      "    MOV     R0, #5          \n"
      "    ADD     R1, R0, #2      \n"
      "    CMP.EQ  #1, #1          \n"
      "    CSEL    R2, #5, #6      \n"
      "    CSET    R3              \n"
      "    DIV     R5, R4, #2      \n"
      "    DIV     R6, #7, #5      \n"
      "    DIV     R7, #4, #0      \n"
      "    DIV     R8, R7, #1      \n"
      "    MUL     R9, R7, #16     \n"
      "    MUL     R10, R7, #16    \n";

   jit_handle_t h1 = jit_assemble(j, ident_new("myfunc"), text1);

   jit_func_t *f = jit_get_func(j, h1);
   jit_do_lvn(f);

   check_unary(f, 1, J_MOV, CONST(7));
   check_unary(f, 3, J_MOV, CONST(5));
   check_unary(f, 4, J_MOV, CONST(1));
   check_binary(f, 5, J_DIV, REG(4), CONST(2));
   check_unary(f, 6, J_MOV, CONST(1));
   check_binary(f, 7, J_DIV, CONST(4), CONST(0));
   check_unary(f, 8, J_MOV, REG(7));
   check_binary(f, 9, J_SHL, REG(7), CONST(4));
   check_unary(f, 10, J_MOV, REG(9));

   jit_free(j);
}
END_TEST

START_TEST(test_lvn5)
{
   jit_t *j = jit_new(NULL, NULL, NULL);

   const char *text1 =
      "    JUMP    L1              \n"
      "L1: NOP                     \n"
      "    JUMP    L2              \n"
      "    MOV     R0, R2          \n"
      "L2: JUMP    L3              \n"
      "    NOP                     \n"
      "L3: NOP                     \n";

   jit_handle_t h1 = jit_assemble(j, ident_new("myfunc1"), text1);

   jit_func_t *f1 = jit_get_func(j, h1);
   jit_do_lvn(f1);

   ck_assert_int_eq(f1->irbuf[0].op, J_NOP);
   check_unary(f1, 2, J_JUMP, LABEL(6));

   const char *text2 =
      "    CMP.EQ  #1, #1       \n"
      "    JUMP.T  L1           \n"
      "    NOP                  \n"
      "L1: NOP                  \n"
      "    CMP.EQ  #1, #0       \n"
      "    JUMP.T  L1           \n"
      "    RET                  \n";

   jit_handle_t h2 = jit_assemble(j, ident_new("myfunc2"), text2);

   jit_func_t *f2 = jit_get_func(j, h2);
   jit_do_lvn(f2);

   check_unary(f2, 1, J_JUMP, LABEL(3));
   ck_assert_int_eq(f2->irbuf[1].cc, JIT_CC_NONE);
   ck_assert_int_eq(f2->irbuf[5].op, J_NOP);

   jit_free(j);
}
END_TEST

START_TEST(test_lvn6)
{
   jit_t *j = jit_new(NULL, NULL, NULL);

   const char *text1 =
      "    MOV         R29, R28       \n"
      "    MOV         R32, #1        \n"
      "    MOV         R33, #-1       \n"
      "    CMP.EQ      #1, #1         \n"
      "    MOV         R34, R33       \n"
      "    ADD         R35, R34, #1   \n"
      "    CLAMP       R36, R35       \n"
      "    NEG         R37, #1        \n"
      "    XOR         R38, R37, R36  \n"
      "    ASR         R39, R38, #63  \n"
      "    XOR         R40, R39, R38  \n"
      "    XOR         R50, #0, R49   \n"
      "    XOR         R52, R51, #0   \n";

   jit_handle_t h1 = jit_assemble(j, ident_new("myfunc"), text1);

   jit_func_t *f = jit_get_func(j, h1);
   jit_do_lvn(f);

   check_unary(f, 4, J_MOV, CONST(-1));
   check_unary(f, 5, J_MOV, CONST(0));;
   check_unary(f, 6, J_MOV, CONST(0));
   check_unary(f, 7, J_MOV, CONST(-1));
   check_unary(f, 8, J_MOV, CONST(-1));
   check_unary(f, 9, J_MOV, CONST(-1));
   check_unary(f, 10, J_MOV, CONST(0));
   ck_assert_int_eq(f->irbuf[11].op, J_MOV);
   ck_assert_int_eq(f->irbuf[12].op, J_MOV);

   jit_free(j);
}
END_TEST

START_TEST(test_cprop1)
{
   jit_t *j = jit_new(NULL, NULL, NULL);

   const char *text1 =
      "    MOV    R1, #0     \n"
      "    MOV    R2, R1     \n"
      "    MOV    R3, R2     \n"
      "    ADD    R4, R3, R1 \n";

   jit_handle_t h1 = jit_assemble(j, ident_new("myfunc1"), text1);

   jit_func_t *f = jit_get_func(j, h1);
   jit_do_cprop(f);

   check_unary(f, 0, J_MOV, CONST(0));
   check_unary(f, 1, J_MOV, CONST(0));
   check_unary(f, 2, J_MOV, CONST(0));
   check_binary(f, 3, J_ADD, CONST(0), CONST(0));

   jit_free(j);
}
END_TEST

START_TEST(test_dce1)
{
   jit_t *j = jit_new(NULL, NULL, NULL);

   const char *text1 =
      "    MOV    R1, #0     \n"
      "    MOV    R3, R2     \n"
      "    ADD    R6, R4, R2 \n";

   jit_handle_t h1 = jit_assemble(j, ident_new("myfunc1"), text1);

   jit_func_t *f = jit_get_func(j, h1);
   jit_do_dce(f);

   ck_assert_int_eq(f->irbuf[0].op, J_NOP);
   ck_assert_int_eq(f->irbuf[1].op, J_NOP);
   ck_assert_int_eq(f->irbuf[2].op, J_NOP);

   jit_free(j);
}
END_TEST

START_TEST(test_nops1)
{
   jit_t *j = jit_new(NULL, NULL, NULL);

   const char *text1 =
      "    ADD    R1, R1, #2   \n"
      "    NOP                 \n"
      "    NOP                 \n"
      "    ADD    R2, R1, R1   \n";

   jit_handle_t h1 = jit_assemble(j, ident_new("myfunc1"), text1);

   jit_func_t *f1 = jit_get_func(j, h1);
   jit_delete_nops(f1);

   ck_assert_int_eq(f1->nirs, 2);

   check_binary(f1, 0, J_ADD, REG(1), CONST(2));
   check_binary(f1, 1, J_ADD, REG(1), REG(1));

   const char *text2 =
      "    NOP                 \n"
      "    ADD    R1, R1, #2   \n"
      "    NOP                 \n"
      "L1: NOP                 \n"
      "    JUMP   L1           \n";

   jit_handle_t h2 = jit_assemble(j, ident_new("myfunc2"), text2);

   jit_func_t *f2 = jit_get_func(j, h2);
   jit_delete_nops(f2);

   ck_assert_int_eq(f2->nirs, 2);

   check_binary(f2, 0, J_ADD, REG(1), CONST(2));
   check_unary(f2, 1, J_JUMP, LABEL(1));
   ck_assert_int_eq(f2->irbuf[1].target, 1);

   jit_free(j);
}
END_TEST

START_TEST(test_issue608)
{
   jit_t *j = jit_new(NULL, NULL, NULL);

   const char *text1 =
      "    RECV    R0, #0          \n"
      "    RECV    R1, #1          \n"
      "    JUMP    L1              \n"
      "L1: MOV     R2, #5          \n"
      "    $COPY   R2, [R1], [R0]  \n"
      "    RET                     \n";

   jit_handle_t h1 = jit_assemble(j, ident_new("myfunc"), text1);

   jit_func_t *f = jit_get_func(j, h1);
   jit_cfg_t *cfg = jit_get_cfg(f);

   ck_assert_int_eq(cfg->nblocks, 2);
   ck_assert_int_eq(cfg->blocks[0].liveout.bits, 0x3);
   ck_assert_int_eq(cfg->blocks[1].livein.bits, 0x3);

   jit_free(j);
}
END_TEST

START_TEST(test_lvn7)
{
   jit_t *j = jit_new(NULL, NULL, NULL);

   const char *text1 =
      "    MOV    R1, #8            \n"
      "    $MOVE  R1, [R2], [CP+0]  \n"
      "    MOV    R3, #0            \n"
      "    $COPY  R3, [R1], [R2]    \n"
      "    $MOVE  R7, [R2], [CP+6]  \n"
      "                             \n"
      "00 01 02 03 04 05 06 07      \n";

   jit_handle_t h1 = jit_assemble(j, ident_new("myfunc1"), text1);

   jit_func_t *f = jit_get_func(j, h1);
   jit_do_lvn(f);

   ck_assert_int_eq(f->irbuf[1].op, J_STORE);
   ck_assert_int_eq(f->irbuf[1].arg1.int64, UINT64_C(0x706050403020100));

   ck_assert_int_eq(f->irbuf[3].op, J_NOP);
   ck_assert_int_eq(f->irbuf[4].op, MACRO_COPY);

   jit_free(j);
}
END_TEST

START_TEST(test_lvn8)
{
   jit_t *j = jit_new(NULL, NULL, NULL);

   const char *text1 =
      "    MOV     R1, #8        \n"
      "    $BZERO  R1, [R2]      \n"
      "    MOV     R3, #0        \n"
      "    $BZERO  R3, [R4]      \n";

   jit_handle_t h1 = jit_assemble(j, ident_new("myfunc1"), text1);

   jit_func_t *f = jit_get_func(j, h1);
   jit_do_lvn(f);

   ck_assert_int_eq(f->irbuf[1].op, J_STORE);
   ck_assert_int_eq(f->irbuf[1].arg1.int64, 0);

   ck_assert_int_eq(f->irbuf[3].op, J_NOP);

   jit_free(j);
}
END_TEST

START_TEST(test_lvn9)
{
   jit_t *j = jit_new(NULL, NULL, NULL);

   const char *text1 =
      "    MOV         R1, #32                        \n"
      "    $MEMSET.16  R1, [R2], #0xffff              \n"
      "    $MEMSET.32  R1, [R2], #0xabababab          \n"
      "    $MEMSET.64  R1, [R2], #0x0101010101010101  \n"
      "    $MEMSET.32  R3, [R4], #0                   \n"
      "    MOV         R3, #0                         \n"
      "    $MEMSET.8   R3, [R2], #0xf0                \n"
      "    MOV         R4, #4                         \n"
      "    $MEMSET.8   R4, [R0+20], #1                \n"
      "    $MEMSET.16  R4, [R0+20], #0x2233           \n"
      "    MOV         R4, #8                         \n"
      "    $MEMSET.16  R4, [R0+20], #0x2233           \n"
      "    $MEMSET.32  R4, [R0+20], #0x11223344       \n"
      "    $MEMSET.64  R4, [R0+20], #0xbeef           \n";

   jit_handle_t h1 = jit_assemble(j, ident_new("myfunc1"), text1);

   jit_func_t *f = jit_get_func(j, h1);
   jit_do_lvn(f);

   ck_assert_int_eq(f->irbuf[1].size, JIT_SZ_8);
   ck_assert_int_eq(f->irbuf[1].arg2.int64, 0xff);

   ck_assert_int_eq(f->irbuf[2].size, JIT_SZ_8);
   ck_assert_int_eq(f->irbuf[2].arg2.int64, 0xab);

   ck_assert_int_eq(f->irbuf[3].size, JIT_SZ_8);
   ck_assert_int_eq(f->irbuf[3].arg2.int64, 1);

   ck_assert_int_eq(f->irbuf[4].op, MACRO_BZERO);
   ck_assert_int_eq(f->irbuf[4].size, JIT_SZ_UNSPEC);
   ck_assert_int_eq(f->irbuf[4].arg2.kind, JIT_VALUE_INVALID);

   ck_assert_int_eq(f->irbuf[6].op, J_NOP);

   ck_assert_int_eq(f->irbuf[8].op, J_STORE);
   ck_assert_int_eq(f->irbuf[8].size, JIT_SZ_32);
   ck_assert_int_eq(f->irbuf[8].arg1.int64, 0x1010101);

   ck_assert_int_eq(f->irbuf[9].op, J_STORE);
   ck_assert_int_eq(f->irbuf[9].size, JIT_SZ_32);
   ck_assert_int_eq(f->irbuf[9].arg1.int64, 0x22332233);

   ck_assert_int_eq(f->irbuf[11].op, J_STORE);
   ck_assert_int_eq(f->irbuf[11].size, JIT_SZ_64);
   ck_assert_int_eq(f->irbuf[11].arg1.int64, 0x2233223322332233);

   ck_assert_int_eq(f->irbuf[12].op, J_STORE);
   ck_assert_int_eq(f->irbuf[12].size, JIT_SZ_64);
   ck_assert_int_eq(f->irbuf[12].arg1.int64, 0x1122334411223344);

   ck_assert_int_eq(f->irbuf[13].op, J_STORE);
   ck_assert_int_eq(f->irbuf[13].size, JIT_SZ_64);
   ck_assert_int_eq(f->irbuf[13].arg1.int64, 0xbeef);

   jit_free(j);
}
END_TEST

START_TEST(test_cfg3)
{
   jit_t *j = jit_new(NULL, NULL, NULL);

   const char *text1 =
      "    RECV    R0, #0       \n"
      "    MOV     R1, #1       \n"
      "    CMP.EQ  R0, #1       \n"
      "    JUMP    L1           \n"
      "L1: JUMP.T  L2           \n"
      "L2: CMP.EQ  R0, #0       \n"
      "    JUMP.T  L1           \n"
      "    RET                  \n";

   jit_handle_t h1 = jit_assemble(j, ident_new("myfunc"), text1);

   jit_func_t *f = jit_get_func(j, h1);
   jit_cfg_t *cfg = jit_get_cfg(f);

   ck_assert_int_eq(cfg->nblocks, 4);

   ck_assert_int_eq(cfg->blocks[0].returns, 0);
   ck_assert_int_eq(cfg->blocks[0].aborts, 0);
   ck_assert_int_eq(cfg->blocks[0].in.count, 0);
   ck_assert_int_eq(cfg->blocks[0].out.count, 1);
   ck_assert_int_eq(jit_get_edge(&cfg->blocks[0].out, 0), 1);
   ck_assert_int_eq(cfg->blocks[0].livein.size, 3);
   ck_assert_int_eq(cfg->blocks[0].livein.bits, 0);
   ck_assert_int_eq(cfg->blocks[0].liveout.size, 3);
   ck_assert_int_eq(cfg->blocks[0].liveout.bits, 0b101);
   ck_assert_int_eq(cfg->blocks[0].varkill.size, 3);
   ck_assert_int_eq(cfg->blocks[0].varkill.bits, 0b111);

   ck_assert_int_eq(cfg->blocks[1].in.count, 2);
   ck_assert_int_eq(jit_get_edge(&cfg->blocks[1].in, 0), 0);
   ck_assert_int_eq(jit_get_edge(&cfg->blocks[1].in, 1), 2);
   ck_assert_int_eq(cfg->blocks[1].out.count, 2);
   ck_assert_int_eq(jit_get_edge(&cfg->blocks[1].out, 0), 2);
   ck_assert_int_eq(jit_get_edge(&cfg->blocks[1].out, 1), 2);
   ck_assert_int_eq(cfg->blocks[1].livein.bits, 0b101);
   ck_assert_int_eq(cfg->blocks[1].liveout.bits, 0b1);
   ck_assert_int_eq(cfg->blocks[1].varkill.bits, 0);

   jit_free(j);
}
END_TEST

START_TEST(test_dce2)
{
   jit_t *j = jit_new(NULL, NULL, NULL);

   const char *text1 =
      "    CMP.EQ    R0, #1           \n"
      "    CMP.LT    #0, #1           \n"
      "    JUMP.T    L1               \n"
      "L1: NOP                        \n";

   jit_handle_t h1 = jit_assemble(j, ident_new("myfunc1"), text1);

   jit_func_t *f = jit_get_func(j, h1);
   jit_do_dce(f);

   ck_assert_int_eq(f->irbuf[0].op, J_NOP);
   ck_assert_int_eq(f->irbuf[1].op, J_CMP);

   jit_free(j);
}
END_TEST

START_TEST(test_code1)
{
   const error_t expect[] = {
      { LINE_INVALID, "JIT code buffer for test too small" },
      { -1, NULL },
   };
   expect_errors(expect);

   code_cache_t *code = code_cache_new();

   code_blob_t *blob = code_blob_new(code, ident_new("test"), 100);
   ck_assert_ptr_nonnull(blob);

   void *mem LOCAL = xcalloc(1024 * 1024);

   code_blob_emit(blob, mem, 128);
   fail_if(blob->overflow);

   code_blob_emit(blob, mem, 1024 * 1024);
   fail_unless(blob->overflow);

   jit_entry_fn_t entry = NULL;
   code_blob_finalise(blob, &entry);
   ck_assert_ptr_null(entry);

   for (int i = 0; i < 50; i++) {
      const size_t size = rand() % (1024 * 1024);

      blob = code_blob_new(code, ident_new("loop"), size);
      ck_assert_ptr_nonnull(blob);

      code_blob_emit(blob, mem, size);
      fail_if(blob->overflow);

      code_blob_finalise(blob, &entry);
   }

   code_cache_free(code);

   check_expected_errors();
}
END_TEST

START_TEST(test_lvn10)
{
   jit_t *j = jit_new(NULL, NULL, NULL);

   const char *text1 =
      "    RECV        R0, #0    \n"
      "    RECV        R1, #1    \n"
      "    RECV        R2, #2    \n"
      "    SEND        #0, R0    \n"
      "    SEND        #1, R1    \n"
      "    SEND        #2, R2    \n"
      "    $EXIT       #1        \n"
      "    RECV        R3, #0    \n"
      "    SEND        #0, R3    \n"
      "    RET                   \n";

   jit_handle_t h1 = jit_assemble(j, ident_new("myfunc1"), text1);

   jit_func_t *f = jit_get_func(j, h1);
   jit_do_lvn(f);

   ck_assert_int_eq(f->irbuf[3].op, J_NOP);
   ck_assert_int_eq(f->irbuf[4].op, J_NOP);
   ck_assert_int_eq(f->irbuf[5].op, J_NOP);
   ck_assert_int_eq(f->irbuf[8].op, J_NOP);

   jit_free(j);
}
END_TEST

START_TEST(test_cprop2)
{
   jit_t *j = jit_new(NULL, NULL, NULL);

   const char *text1 =
      "    MOV    R1, R0      \n"
      "    MOV    R2, R1      \n"
      "    ADD    R0, R0, #1  \n"
      "    ADD    R1, R1, #1  \n";

   jit_handle_t h1 = jit_assemble(j, ident_new("myfunc1"), text1);

   jit_func_t *f1 = jit_get_func(j, h1);
   jit_do_cprop(f1);

   check_unary(f1, 0, J_MOV, REG(0));
   check_unary(f1, 1, J_MOV, REG(0));
   check_binary(f1, 2, J_ADD, REG(0), CONST(1));
   check_binary(f1, 3, J_ADD, REG(1), CONST(1));

   jit_free(j);
}
END_TEST

START_TEST(test_mem2reg1)
{
   jit_t *j = jit_new(NULL, NULL, NULL);

   const char *text1 =
      "    $SALLOC   R0, #0, #4   \n"
      "    $SALLOC   R1, #8, #7   \n"
      "    $SALLOC   R2, #16, #8  \n"
      "    LOAD.32   R3, [R0]     \n"
      "    LOAD.32   R4, [R2]     \n"
      "    STORE.32  #5, [R0]     \n"
      ;

   jit_handle_t h1 = jit_assemble(j, ident_new("myfunc1"), text1);

   jit_func_t *f = jit_get_func(j, h1);
   jit_do_mem2reg(f);

   check_nullary(f, 0, J_NOP);
   check_binary(f, 1, MACRO_SALLOC, CONST(0), CONST(7));
   check_binary(f, 2, MACRO_SALLOC, CONST(8), CONST(8));
   check_unary(f, 3, J_MOV, REG(0));
   check_unary(f, 4, J_LOAD, ADDR(2, 0));
   check_unary(f, 5, J_MOV, CONST(5));

   ck_assert_int_eq(f->framesz, 16);

   jit_free(j);
}
END_TEST

START_TEST(test_lscan1)
{
   jit_t *j = jit_new(NULL, NULL, NULL);

   const char *text1 =
      "    MOV    R1, R0      \n"
      "    ADD    R0, R0, #1  \n"
      "    ADD    R1, R1, #1  \n"
      "    MUL    R2, R0, R1  \n"
      "    SEND   #0, R2      \n"
      "    RET                \n";

   jit_handle_t h1 = jit_assemble(j, ident_new("myfunc1"), text1);

   jit_func_t *f = jit_get_func(j, h1);

   phys_slot_t *slots LOCAL = xmalloc_array(f->nregs, sizeof(phys_slot_t));
   const int spills = jit_do_lscan(f, slots, ~UINT64_C(0x3));

   ck_assert_int_eq(spills, 1);

   ck_assert_int_eq(slots[0], 0);
   ck_assert_int_eq(slots[1], 1);
   ck_assert_int_eq(slots[2], STACK_BASE);

   check_unary(f, 0, J_MOV, REG(0));
   check_binary(f, 1, J_ADD, REG(0), CONST(1));
   check_binary(f, 2, J_ADD, REG(1), CONST(1));

   jit_free(j);
}
END_TEST

START_TEST(test_trim1)
{
   set_standard(STD_08);

   input_from_file(TESTDIR "/jit/trim1.vhd");

   parse_check_and_simplify(T_PACKAGE, T_PACK_BODY);

   jit_t *j = jit_new(get_registry(), get_mir(), NULL);

   static const struct {
      const char *fn;
      int32_t delta;
   } cases[] = {
      { "FUNC1()Q", +8 },
      { "FUNC2()J", 0 },
      { "FUNC3(I)C", 0 },
      { "FUNC4()Q", +16 },  // Context pointer plus array
      { "PROC1", 0 },
      { "FUNC5(I)I", 0 },
      { "FUNC6()J", 0 },
      { "FUNC7(I)I", 0 },
   };

   jit_handle_t pack = jit_compile(j, ident_new("WORK.TRIM1"));
   void *context = jit_link(j, pack);
   ck_assert_ptr_nonnull(context);

   mspace_t *m = jit_get_mspace(j);
   tlab_t *tlab = tlab_acquire(m);

   for (int i = 0; i < ARRAY_LEN(cases); i++) {
      ident_t fn = ident_sprintf("WORK.TRIM1.%s", cases[i].fn);
      jit_handle_t handle = jit_compile(j, fn);

      jit_scalar_t arg0 = { .pointer = context };
      jit_scalar_t arg1 = { .integer = 42 };
      jit_scalar_t result;

      const uint32_t entry = ((rand() % (tlab->limit / 2)) + 7) & ~7;
      tlab->alloc = entry;

      jit_fastcall(j, handle, &result, arg0, arg1, tlab);

      ck_assert_msg(tlab->alloc == entry + cases[i].delta,
                    "%s: expected alloc delta %+d but have %+d",
                    cases[i].fn, cases[i].delta, tlab->alloc - entry);
   }

   tlab_release(tlab);
   jit_free(j);
}
END_TEST

START_TEST(test_lvn11)
{
   jit_t *j = jit_new(NULL, NULL, NULL);

   const char *text1 =
      "    SHL         R0, #0, #1   \n"
      "    OR          R1, #1, R0   \n"
      "    OR          R2, R1, #0   \n"
      "    RECV        R3, #0       \n"
      "    SHL         R4, R3, #0   \n"
      "    ASR         R5, R4, #0   \n"
      "    SHR         R6, #8, #5   \n"
      "    SHR         R7, R5, R6   \n"
      "    SEND        #0, R7       \n"
      "    RET                      \n";

   jit_handle_t h1 = jit_assemble(j, ident_new("myfunc1"), text1);

   jit_func_t *f = jit_get_func(j, h1);
   jit_do_lvn(f);

   check_unary(f, 0, J_MOV, CONST(0));
   check_unary(f, 1, J_MOV, CONST(1));
   check_unary(f, 2, J_MOV, CONST(1));
   check_unary(f, 4, J_MOV, REG(3));
   check_unary(f, 5, J_MOV, REG(4));
   check_unary(f, 6, J_MOV, CONST(0));
   check_unary(f, 7, J_MOV, REG(4));

   jit_free(j);
}
END_TEST

START_TEST(test_lvn12)
{
   jit_t *j = jit_new(NULL, NULL, NULL);

   const char *text1 =
      "    RECV        R2, #0       \n"
      "    MOV         R0, R2       \n"
      "    SEND        #0, R0       \n"
      "    CALL        <FUNC>       \n"
      "    RECV        R1, #0       \n"
      "    REM         R0, R1, #2   \n"
      "    CMP.EQ      R0, #0       \n"
      "    CSET        R1           \n"
      "    SEND        #0, R1       \n"   // Should not remove this
      "    RECV        R1, #0       \n"
      "    CSEL        R1, R2, R3   \n"
      "    SEND        #0, R1       \n"   // Should not remove this
      "    RET                      \n";

   jit_handle_t h1 = jit_assemble(j, ident_new("myfunc1"), text1);

   jit_func_t *f = jit_get_func(j, h1);
   jit_do_lvn(f);

   check_unary(f, 0, J_RECV, CONST(0));
   check_nullary(f, 2, J_NOP);
   check_unary(f, 4, J_RECV, CONST(0));
   check_nullary(f, 7, J_CSET);
   check_binary(f, 8, J_SEND, CONST(0), REG(1));
   check_binary(f, 11, J_SEND, CONST(0), REG(1));

   jit_free(j);
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
   tcase_add_test(tc, test_value1);
   tcase_add_test(tc, test_assemble1);
   tcase_add_test(tc, test_assemble2);
   tcase_add_test(tc, test_cfg1);
   tcase_add_test(tc, test_lvn1);
   tcase_add_test(tc, test_lvn2);
   tcase_add_test(tc, test_lvn3);
   tcase_add_test(tc, test_issue575);
   tcase_add_test(tc, test_cfg2);
   tcase_add_test(tc, test_lvn4);
   tcase_add_test(tc, test_lvn5);
   tcase_add_test(tc, test_lvn6);
   tcase_add_test(tc, test_cprop1);
   tcase_add_test(tc, test_dce1);
   tcase_add_test(tc, test_nops1);
   tcase_add_test(tc, test_issue608);
   tcase_add_test(tc, test_lvn7);
   tcase_add_test(tc, test_lvn8);
   tcase_add_test(tc, test_lvn9);
   tcase_add_test(tc, test_cfg3);
   tcase_add_test(tc, test_dce2);
   tcase_add_test(tc, test_code1);
   tcase_add_test(tc, test_lvn10);
   tcase_add_test(tc, test_cprop2);
   tcase_add_test(tc, test_mem2reg1);
   tcase_add_test(tc, test_lscan1);
   tcase_add_test(tc, test_trim1);
   tcase_add_test(tc, test_lvn11);
   tcase_add_test(tc, test_lvn12);
   suite_add_tcase(s, tc);

   return s;
}
