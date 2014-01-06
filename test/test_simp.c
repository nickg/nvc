#include "type.h"
#include "util.h"
#include "phase.h"

#include <check.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

typedef struct error {
   int        line;
   const char *snippet;
} error_t;

static const error_t  *error_lines = NULL;
static error_fn_t orig_error_fn = NULL;

static void setup(void)
{
   lib_set_work(lib_tmp());
   opt_set_int("bootstrap", 0);
   opt_set_int("unit-test", 1);
}

static void teardown(void)
{
   lib_free(lib_work());
}

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

   return tree_dval(t) == r;
}

static bool folded_b(tree_t t, bool b)
{
   if (tree_kind(t) != T_REF)
      return false;

   if (type_ident(tree_type(t)) != ident_new("STD.STANDARD.BOOLEAN"))
      return false;

   tree_t lit = tree_ref(t);
   if (tree_kind(lit) != T_ENUM_LIT)
      return false;

   return tree_pos(lit) == (b ? 1 : 0);
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

   simplify(a);

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

   simplify(a);

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

   simplify(a);

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
   tree_t e, a, p, s;
   range_t r;

   input_from_file(TESTDIR "/simp/ffold.vhd");

   p = parse();
   fail_if(p == NULL);
   fail_unless(tree_kind(p) == T_PACKAGE);
   sem_check(p);

   p = parse();
   fail_if(p == NULL);
   fail_unless(tree_kind(p) == T_PACK_BODY);
   sem_check(p);

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

   simplify(a);

   fail_unless(folded_i(tree_value(tree_decl(a, 2)), 6));
   fail_unless(folded_i(tree_value(tree_decl(a, 4)), 4));
   fail_unless(folded_i(tree_value(tree_decl(a, 5)), 3));
}
END_TEST

int main(void)
{
   register_trace_signal_handlers();

   setenv("NVC_LIBPATH", "../lib/std", 1);

   Suite *s = suite_create("simplify");

   TCase *tc_core = tcase_create("Core");
   tcase_add_unchecked_fixture(tc_core, setup, teardown);
   tcase_add_test(tc_core, test_cfold);
   tcase_add_test(tc_core, test_proc);
   tcase_add_test(tc_core, test_args);
   tcase_add_test(tc_core, test_ffold);
   suite_add_tcase(s, tc_core);

   SRunner *sr = srunner_create(s);
   srunner_run_all(sr, CK_NORMAL);

   int nfail = srunner_ntests_failed(sr);

   srunner_free(sr);

   return nfail == 0 ? EXIT_SUCCESS : EXIT_FAILURE;
}
