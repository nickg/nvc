#include "type.h"
#include "util.h"
#include "phase.h"
#include "test_util.h"
#include "common.h"

#include <check.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

static bool folded_i(tree_t t, int64_t i)
{
   if (tree_kind(t) != T_LITERAL)
      return false;

   if (tree_subkind(t) != L_INT)
      return false;

   return tree_ival(t) == i;
}

static bool folded_r(tree_t t, double r)
{
   if (tree_kind(t) != T_LITERAL)
      return false;

   if (tree_subkind(t) != L_REAL)
      return false;

   const double dval = tree_dval(t);
   return (dval > r * 0.9999) && (dval < r * 1.0001);
}

static bool folded_b(tree_t t, bool b)
{
   if (type_ident(tree_type(t)) != ident_new("STD.STANDARD.BOOLEAN"))
      return false;

   bool actual;
   if (folded_bool(t, &actual))
      return actual == b;
   else
      return false;
}

START_TEST(test_cfold)
{
   tree_t e, a, p, s;
   range_t r;

   input_from_file(TESTDIR "/simp/cfold.vhd");

   e = parse();
   fail_if(e == NULL);
   fail_unless(tree_kind(e) == T_ENTITY);
   sem_check(e);

   a = parse();
   fail_if(a == NULL);
   fail_unless(tree_kind(a) == T_ARCH);
   sem_check(a);

   fail_unless(parse() == NULL);
   fail_unless(parse_errors() == 0);
   fail_unless(sem_errors() == 0);

   simplify(a, 0);

   fail_unless(folded_i(tree_value(tree_decl(a, 0)), -10));

   r = type_dim(tree_type(tree_decl(a, 1)), 0);
   fail_unless(folded_i(r.left, -5));
   fail_unless(folded_i(r.right, 8));

   p = tree_stmt(a, 0);

   s = tree_stmt(p, 0);
   fail_unless(folded_i(tree_value(tree_waveform(s, 0)), 2));
   s = tree_stmt(p, 1);
   fail_unless(folded_i(tree_value(tree_waveform(s, 0)), 8));
   s = tree_stmt(p, 2);
   fail_unless(folded_i(tree_value(tree_waveform(s, 0)), -5));
   s = tree_stmt(p, 3);
   fail_unless(folded_b(tree_value(s), true));
   s = tree_stmt(p, 4);
   fail_unless(folded_b(tree_value(s), false));
   s = tree_stmt(p, 5);
   fail_unless(folded_b(tree_value(s), true));
   fail_unless(folded_b(tree_value(tree_stmt(p, 6)), true));
   fail_unless(folded_b(tree_value(tree_stmt(p, 7)), false));
   fail_unless(folded_b(tree_value(tree_stmt(p, 8)), true));
   fail_unless(folded_b(tree_value(tree_stmt(p, 9)), false));
   fail_unless(folded_b(tree_value(tree_stmt(p, 10)), false));
   fail_unless(folded_b(tree_value(tree_stmt(p, 11)), true));
   fail_unless(folded_b(tree_value(tree_stmt(p, 12)), false));
   fail_unless(folded_b(tree_value(tree_stmt(p, 13)), true));
   fail_unless(folded_b(tree_value(tree_stmt(p, 14)), false));
   fail_unless(folded_b(tree_value(tree_stmt(p, 15)), false));
   fail_unless(folded_i(tree_value(tree_waveform(tree_stmt(p, 16), 0)), 2));
   fail_unless(folded_i(tree_value(tree_waveform(tree_stmt(p, 17), 0)), 5));
   fail_unless(folded_i(tree_value(tree_waveform(tree_stmt(p, 18), 0)), 6));
   fail_unless(folded_i(tree_value(tree_waveform(tree_stmt(p, 19), 0)), 24));
   fail_unless(folded_i(tree_value(tree_waveform(tree_stmt(p, 20), 0)), 5));
   fail_unless(folded_i(tree_value(tree_waveform(tree_stmt(p, 21), 0)), 4));
   fail_unless(folded_i(tree_value(tree_waveform(tree_stmt(p, 22), 0)), -1));
   fail_unless(folded_i(tree_value(tree_waveform(tree_stmt(p, 23), 0)), 16));

   p = tree_stmt(a, 1);
   fail_unless(tree_stmts(p) == 3);

   s = tree_stmt(p, 0);
   fail_unless(tree_kind(s) == T_SIGNAL_ASSIGN);
   s = tree_stmt(p, 1);
   fail_unless(tree_kind(s) == T_SIGNAL_ASSIGN);
   s = tree_stmt(p, 2);
   fail_unless(tree_kind(s) == T_BLOCK);
   fail_unless(tree_stmts(s) == 2);

   p = tree_stmt(a, 2);
   fail_unless(folded_r(tree_value(tree_stmt(p, 0)), 1.0));
   fail_unless(folded_r(tree_value(tree_stmt(p, 1)), 6.0));
   fail_unless(folded_r(tree_value(tree_stmt(p, 2)), 1.0));
   fail_unless(folded_b(tree_value(tree_stmt(p, 3)), true));

   p = tree_stmt(a, 4);
   fail_unless(folded_b(tree_value(tree_stmt(p, 0)), true));
   fail_unless(folded_b(tree_value(tree_stmt(p, 1)), true));
   fail_unless(folded_b(tree_value(tree_stmt(p, 2)), true));
   fail_unless(folded_b(tree_value(tree_stmt(p, 3)), true));
   fail_unless(folded_b(tree_value(tree_stmt(p, 4)), true));

   p = tree_stmt(a, 5);
   s = tree_stmt(p, 0);
   fail_unless(tree_kind(s) == T_BLOCK);
   fail_unless(tree_stmts(s) == 0);

   p = tree_stmt(a, 6);
   fail_unless(folded_r(tree_value(tree_stmt(p, 0)), 3.0));
   fail_unless(folded_r(tree_value(tree_stmt(p, 1)), 0.6));
   fail_unless(folded_r(tree_value(tree_stmt(p, 2)), 2.5));
   fail_unless(folded_r(tree_value(tree_stmt(p, 3)), 16.0));

   p = tree_stmt(a, 7);
   fail_unless(folded_b(tree_value(tree_stmt(p, 0)), true));
   fail_unless(folded_b(tree_value(tree_stmt(p, 1)), true));
}
END_TEST

START_TEST(test_proc)
{
   tree_t e, a, p, s, r;

   input_from_file(TESTDIR "/simp/proc.vhd");

   e = parse();
   fail_if(e == NULL);
   fail_unless(tree_kind(e) == T_ENTITY);
   sem_check(e);

   a = parse();
   fail_if(a == NULL);
   fail_unless(tree_kind(a) == T_ARCH);
   sem_check(a);

   fail_unless(parse() == NULL);
   fail_unless(parse_errors() == 0);
   fail_unless(sem_errors() == 0);

   simplify(a, 0);

   ////////

   p = tree_stmt(a, 0);
   fail_unless(tree_kind(p) == T_PROCESS);
   fail_if(tree_triggers(p) > 0);
   fail_unless(tree_stmts(p) == 2);

   s = tree_stmt(p, 0);
   fail_unless(tree_kind(s) == T_ASSERT);

   s = tree_stmt(p, 1);
   fail_unless(tree_kind(s) == T_WAIT);
   fail_unless(tree_triggers(s) == 2);
   r = tree_trigger(s, 0);
   fail_unless(tree_kind(r) == T_REF);
   fail_unless(tree_ident(r) == ident_new("X"));
   r = tree_trigger(s, 1);
   fail_unless(tree_kind(r) == T_REF);
   fail_unless(tree_ident(r) == ident_new("Y"));

   ////////

   p = tree_stmt(a, 1);
   fail_unless(tree_kind(p) == T_PROCESS);
   fail_unless(tree_stmts(p) == 2);

   s = tree_stmt(p, 0);
   fail_unless(tree_kind(s) == T_SIGNAL_ASSIGN);

   s = tree_stmt(p, 1);
   fail_unless(tree_kind(s) == T_WAIT);
   fail_unless(tree_triggers(s) == 1);
   fail_unless(tree_ident(tree_trigger(s, 0)) == ident_new("Y"));

   ////////

   p = tree_stmt(a, 2);
   fail_unless(tree_kind(p) == T_PROCESS);
   fail_unless(tree_stmts(p) == 2);

   s = tree_stmt(p, 0);
   fail_unless(tree_kind(s) == T_IF);
   fail_unless(tree_stmts(s) == 1);
   fail_unless(tree_else_stmts(s) == 1);

   s = tree_stmt(p, 1);
   fail_unless(tree_kind(s) == T_WAIT);
   fail_unless(tree_triggers(s) == 2);
   fail_unless(tree_ident(tree_trigger(s, 0)) == ident_new("Y"));
   fail_unless(tree_ident(tree_trigger(s, 1)) == ident_new("X"));

   ////////

   p = tree_stmt(a, 3);
   fail_unless(tree_kind(p) == T_PROCESS);
   fail_unless(tree_stmts(p) == 2);
}
END_TEST

START_TEST(test_args)
{
   tree_t e, a, p, s, c;

   input_from_file(TESTDIR "/simp/args.vhd");

   e = parse();
   fail_if(e == NULL);
   fail_unless(tree_kind(e) == T_ENTITY);
   sem_check(e);

   a = parse();
   fail_if(a == NULL);
   fail_unless(tree_kind(a) == T_ARCH);
   sem_check(a);

   fail_unless(parse() == NULL);
   fail_unless(parse_errors() == 0);
   fail_unless(sem_errors() == 0);

   simplify(a, 0);

   ////////

   p = tree_stmt(a, 0);
   fail_unless(tree_kind(p) == T_PROCESS);

   for (int i = 0; i < 3; i++) {
      s = tree_stmt(p, i);
      fail_unless(tree_kind(s) == T_VAR_ASSIGN);
      c = tree_value(s);
      fail_unless(tree_kind(c) == T_FCALL);
      fail_unless(tree_params(c) == 2);
      fail_unless(tree_subkind(tree_param(c, 0)) == P_POS);
      fail_unless(icmp(tree_ident(tree_value(tree_param(c, 0))), "A"));
      fail_unless(tree_subkind(tree_param(c, 1)) == P_POS);
      fail_unless(icmp(tree_ident(tree_value(tree_param(c, 1))), "B"));
   }

   s = tree_stmt(p, 3);
   fail_unless(tree_kind(s) == T_PCALL);
   fail_unless(tree_params(s) == 2);
   fail_unless(tree_subkind(tree_param(s, 0)) == P_POS);
   fail_unless(icmp(tree_ident(tree_value(tree_param(s, 0))), "A"));
   fail_unless(tree_subkind(tree_param(s, 1)) == P_POS);
   fail_unless(icmp(tree_ident(tree_value(tree_param(s, 1))), "B"));
}
END_TEST

START_TEST(test_ffold)
{
   input_from_file(TESTDIR "/simp/ffold.vhd");

   tree_t a = parse_check_simplify_and_lower(T_PACKAGE, T_PACK_BODY,
                                             T_ENTITY, T_ARCH);
   fail_unless(sem_errors() == 0);

   tree_t b = tree_stmt(a, 0);
   fail_unless(tree_kind(b) == T_BLOCK);

   fail_unless(folded_i(tree_value(tree_decl(b, 0)), 6));
   fail_unless(folded_i(tree_value(tree_decl(b, 2)), 4));
   fail_unless(folded_i(tree_value(tree_decl(b, 3)), 3));
   fail_unless(folded_i(tree_value(tree_decl(b, 4)), 2));
   fail_unless(folded_i(tree_value(tree_decl(b, 5)), 5));
   fail_unless(folded_i(tree_value(tree_decl(b, 6)), 10));
   fail_unless(folded_b(tree_value(tree_decl(b, 7)), true));
   fail_unless(folded_b(tree_value(tree_decl(b, 8)), true));
   fail_unless(folded_b(tree_value(tree_decl(b, 9)), false));
   fail_unless(folded_r(tree_value(tree_decl(b, 10)), 0.62));
   fail_unless(folded_r(tree_value(tree_decl(b, 11)), 71.7));
   fail_unless(folded_b(tree_value(tree_decl(b, 12)), true));
   fail_unless(folded_b(tree_value(tree_decl(b, 13)), true));
   fail_unless(folded_b(tree_value(tree_decl(b, 14)), false));
   fail_unless(folded_b(tree_value(tree_decl(b, 15)), true));
   fail_unless(folded_b(tree_value(tree_decl(b, 16)), true));
   fail_unless(folded_b(tree_value(tree_decl(b, 17)), true));
   fail_unless(folded_b(tree_value(tree_decl(b, 18)), true));
   fail_unless(folded_i(tree_value(tree_decl(b, 19)), 80));
   fail_unless(folded_i(tree_value(tree_decl(b, 20)), 5));
   fail_unless(folded_i(tree_value(tree_decl(b, 21)), 2));
   fail_unless(folded_b(tree_value(tree_decl(b, 22)), true));
   fail_unless(folded_b(tree_value(tree_decl(b, 23)), true));
   fail_unless(folded_b(tree_value(tree_decl(b, 24)), true));
   fail_unless(folded_b(tree_value(tree_decl(b, 25)), true));
   fail_unless(folded_b(tree_value(tree_decl(b, 26)), false));
}
END_TEST

START_TEST(test_ffold2)
{
   input_from_file(TESTDIR "/simp/ffold2.vhd");

   tree_t a = parse_check_simplify_and_lower(T_PACKAGE, T_PACK_BODY,
                                             T_ENTITY, T_ARCH);

   tree_t b = tree_stmt(a, 0);
   fail_unless(tree_kind(b) == T_BLOCK);

   fail_unless(folded_i(tree_value(tree_decl(b, 0)), 3));
}
END_TEST

START_TEST(test_issue49)
{
   input_from_file(TESTDIR "/simp/issue49.vhd");

   tree_t a = parse_and_check(T_ENTITY, T_ARCH);
   fail_unless(sem_errors() == 0);

   simplify(a, 0);
}
END_TEST

START_TEST(test_issue155)
{
   input_from_file(TESTDIR "/simp/issue155.vhd");

   tree_t p = parse_and_check(T_PACKAGE);
   fail_unless(sem_errors() == 0);

   simplify(p, 0);

   range_t ar = type_dim(tree_type(tree_decl(p, 2)), 0);
   fail_unless(folded_i(ar.left, 7));
   fail_unless(folded_i(ar.right, 0));

   range_t br = type_dim(tree_type(tree_decl(p, 3)), 0);
   fail_unless(folded_i(br.left, 3));
   fail_unless(folded_i(br.right, 0));

   range_t cr = type_dim(tree_type(tree_decl(p, 4)), 0);
   fail_unless(folded_i(cr.left, 1));
   fail_unless(folded_i(cr.right, 0));

   range_t dr = type_dim(tree_type(tree_decl(p, 6)), 0);
   fail_unless(folded_i(dr.left, 2));
   fail_unless(folded_i(dr.right, 1));
}
END_TEST

START_TEST(test_context)
{
   set_standard(STD_08);

   input_from_file(TESTDIR "/simp/context.vhd");

   lib_t foo = lib_tmp("foo");
   lib_t bar = lib_tmp("bar");

   lib_set_work(foo);
   parse_and_check(T_PACKAGE, T_CONTEXT, -1);

   lib_set_work(bar);
   tree_t e = parse_and_check(T_ENTITY);
   fail_unless(sem_errors() == 0);

   fail_unless(tree_contexts(e) == 3);

   simplify(e, 0);

   fail_unless(tree_contexts(e) == 4);
   fail_unless(tree_kind(tree_context(e, 2)) == T_LIBRARY);
   fail_unless(tree_kind(tree_context(e, 3)) == T_USE);
}
END_TEST

START_TEST(test_issue212)
{
   input_from_file(TESTDIR "/simp/issue212.vhd");

   tree_t top = run_elab();
   fail_unless(tree_stmts(top) == 1);
}
END_TEST

START_TEST(test_shift2)
{
   input_from_file(TESTDIR "/simp/shift2.vhd");

   tree_t top = run_elab();
   fail_unless(tree_stmts(top) == 0);
}
END_TEST

START_TEST(test_issue194)
{
   input_from_file(TESTDIR "/simp/issue194.vhd");

   tree_t p = parse_check_and_simplify(T_PACKAGE, T_PACK_BODY, -1);
   fail_if(p == NULL);
   bounds_check(p);

   tree_t a = parse_check_and_simplify(T_ENTITY, T_ARCH);
   fail_if(a == NULL);
   bounds_check(a);
}
END_TEST

START_TEST(test_issue309)
{
   input_from_file(TESTDIR "/simp/issue309.vhd");

   tree_t top = run_elab();
   fail_unless(tree_stmts(top) == 0);
}
END_TEST

START_TEST(test_issue320)
{
   input_from_file(TESTDIR "/simp/issue320.vhd");

   tree_t a = parse_and_check(T_PACKAGE, T_ENTITY, T_ARCH);
   simplify(a, EVAL_LOWER);

   fail_unless(tree_decls(a) == 2);
   tree_t d = tree_decl(a, 1);
   fail_unless(tree_kind(d) == T_CONST_DECL);
   fail_unless(tree_kind(tree_value(d)) == T_LITERAL);
   fail_unless(tree_subkind(tree_value(d)) == L_INT);
   fail_unless(tree_ival(tree_value(d)) == 0);
}
END_TEST

START_TEST(test_issue321)
{
   input_from_file(TESTDIR "/simp/issue321.vhd");

   tree_t top = run_elab();
   fail_if(top == NULL);

   fail_unless(tree_decls(top) == 7);
   tree_t d5 = tree_decl(top, 5);
   fail_unless(tree_kind(tree_value(d5)) == T_LITERAL);
   fail_unless(tree_ival(tree_value(d5)) == 23);
   tree_t d6 = tree_decl(top, 6);
   fail_unless(tree_kind(tree_value(d6)) == T_LITERAL);
   fail_unless(tree_ival(tree_value(d6)) == 5);
}
END_TEST

START_TEST(test_issue331)
{
   input_from_file(TESTDIR "/simp/issue331.vhd");

   tree_t top = run_elab();
   fail_if(top == NULL);

   fail_unless(tree_decls(top) == 8);
   tree_t d2 = tree_decl(top, 2);
   fail_unless(tree_ident(d2) == ident_new(":test_ng:vec_range"));
   tree_t agg = tree_value(d2);
   fail_unless(tree_kind(agg) == T_AGGREGATE);
   tree_t info_0 = tree_value(tree_assoc(agg, 2));
   fail_unless(tree_kind(info_0) == T_AGGREGATE);
   tree_t info_1 = tree_value(tree_assoc(agg, 3));
   fail_unless(tree_kind(info_1) == T_AGGREGATE);
   tree_t info_0_data_lo = tree_value(tree_assoc(info_0, 0));
   fail_unless(tree_kind(info_0_data_lo) == T_LITERAL);
   fail_unless(tree_ival(info_0_data_lo) == 0);
   tree_t info_0_data_hi = tree_value(tree_assoc(info_0, 1));
   fail_unless(tree_kind(info_0_data_hi) == T_LITERAL);
   fail_unless(tree_ival(info_0_data_hi) == 0);
   tree_t info_1_data_lo = tree_value(tree_assoc(info_1, 0));
   fail_unless(tree_kind(info_1_data_lo) == T_LITERAL);
   fail_unless(tree_ival(info_1_data_lo) == 1);
   tree_t info_1_data_hi = tree_value(tree_assoc(info_1, 1));
   fail_unless(tree_kind(info_1_data_hi) == T_LITERAL);
   fail_unless(tree_ival(info_1_data_hi) == 1);
}
END_TEST

START_TEST(test_issue322)
{
   input_from_file(TESTDIR "/simp/issue322.vhd");

   tree_t top = run_elab();
   fail_if(top == NULL);

   tree_t p0 = tree_stmt(top, 0);
   fail_unless(tree_kind(p0) == T_PROCESS);

   tree_t d0 = tree_decl(p0, 0);
   fail_unless(tree_kind(d0) == T_VAR_DECL);
   fail_unless(tree_kind(tree_value(d0)) == T_FCALL);
}
END_TEST

START_TEST(test_issue332)
{
   input_from_file(TESTDIR "/simp/issue332.vhd");

   tree_t a = parse_check_and_simplify(T_ENTITY, T_ARCH);
   fail_if(a == NULL);

   tree_t p0 = tree_stmt(a, 0);
   fail_unless(tree_kind(p0) == T_PROCESS);

   tree_t p0s1 = tree_stmt(p0, 1);
   fail_unless(tree_kind(p0s1) == T_WAIT);
   fail_unless(tree_triggers(p0s1) == 1);
   fail_unless(icmp(tree_ident(tree_trigger(p0s1, 0)), "CURR_VALUE"));

   tree_t p1 = tree_stmt(a, 1);
   fail_unless(tree_kind(p1) == T_PROCESS);

   tree_t p1s1 = tree_stmt(p1, 1);
   fail_unless(tree_kind(p1s1) == T_WAIT);
   fail_unless(tree_triggers(p1s1) == 1);
   fail_unless(icmp(tree_ident(tree_trigger(p1s1, 0)), "REGS_RDATA"));
}
END_TEST

START_TEST(test_issue344)
{
   input_from_file(TESTDIR "/simp/issue344.vhd");

   tree_t top = run_elab();

   tree_t p0 = tree_stmt(top, 0);
   fail_unless(tree_stmts(p0) == 2);
}
END_TEST

int main(void)
{
   Suite *s = suite_create("simplify");

   TCase *tc_core = nvc_unit_test();
   tcase_add_test(tc_core, test_cfold);
   tcase_add_test(tc_core, test_proc);
   tcase_add_test(tc_core, test_args);
   tcase_add_test(tc_core, test_ffold);
   tcase_add_test(tc_core, test_issue49);
   tcase_add_test(tc_core, test_issue155);
   tcase_add_test(tc_core, test_context);
   tcase_add_test(tc_core, test_issue212);
   tcase_add_test(tc_core, test_shift2);
   tcase_add_test(tc_core, test_issue194);
   tcase_add_test(tc_core, test_issue309);
   tcase_add_test(tc_core, test_issue320);
   tcase_add_test(tc_core, test_issue321);
   tcase_add_test(tc_core, test_issue322);
   tcase_add_test(tc_core, test_ffold2);
   tcase_add_test(tc_core, test_issue331);
   tcase_add_test(tc_core, test_issue332);
   tcase_add_test(tc_core, test_issue344);
   suite_add_tcase(s, tc_core);

   return nvc_run_test(s);
}
