#include "parse.h"
#include "type.h"
#include "phase.h"
#include "util.h"

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
}

static void teardown(void)
{
   lib_free(lib_work());
}

static void test_error_fn(const char *msg, const loc_t *loc)
{
   fail_if(error_lines == NULL);

   bool unexpected = error_lines->line == -1
      || error_lines->snippet == NULL
      || error_lines->line != loc->first_line
      || strstr(msg, error_lines->snippet) == NULL;

   if (unexpected) {
      orig_error_fn(msg, loc);
      printf("expected line %d '%s'\n",
             error_lines->line, error_lines->snippet);
   }

   fail_if(unexpected);

   error_lines++;
}

static void expect_errors(const error_t *lines)
{
   fail_unless(orig_error_fn == NULL);
   orig_error_fn = set_error_fn(test_error_fn);
   error_lines = lines;
}

START_TEST(test_integer)
{
   tree_t a, d, p, s, e;
   type_t t;
   range_t r;

   fail_unless(input_from_file(TESTDIR "/sem/integer.vhd"));

   e = parse();
   fail_if(e == NULL);
   fail_unless(tree_kind(e) == T_ENTITY);
   sem_check(e);

   a = parse();
   fail_if(a == NULL);
   fail_unless(tree_kind(a) == T_ARCH);

   fail_unless(parse() == NULL);
   fail_unless(parse_errors() == 0);

   const error_t expect[] = {
      { 20, "MY_INT1 does not match type of target MY_INT2" },
      { 30, "MY_INT1 does not match type of target MY_INT2_SUB" },
      { 35, "type NOTHING is not defined" },
      { 48, "no suitable overload for operator \"*\"(MY_INT2, MY_INT1)" },
      { 57, "MY_INT2 has no attribute CAKE" },
      { -1, NULL }
   };
   expect_errors(expect);

   sem_check(a);
   fail_unless(sem_errors() == (sizeof(expect) / sizeof(error_t)) - 1);

   d = tree_decl(a, 2);
   fail_unless(tree_ident(d) == ident_new("X"));
   t = tree_type(d);
   fail_unless(type_kind(t) == T_INTEGER);
   e = tree_value(d);
   fail_unless(tree_kind(e) == T_LITERAL);
   t = tree_type(e);
   fail_unless(type_kind(t) == T_INTEGER);

   fail_unless(tree_stmts(a) == 6);

   // Process 1

   p = tree_stmt(a, 0);
   fail_unless(tree_kind(p) == T_PROCESS);

   fail_unless(tree_decls(p) == 1);

   d = tree_decl(p, 0);
   fail_unless(tree_ident(d) == ident_new("Z"));
   fail_unless(type_kind(tree_type(d)) == T_INTEGER);

   s = tree_stmt(p, 0);
   fail_unless(tree_ref(tree_target(s)) == d);

   // Process 6

   p = tree_stmt(a, 5);
   fail_unless(tree_kind(p) == T_PROCESS);
   fail_unless(tree_decls(p) == 1);

   d = tree_decl(p, 0);
   r = type_dim(tree_type(d), 0);

   s = tree_stmt(p, 0);
   fail_unless(tree_kind(tree_value(s)) == T_ATTR_REF);
   fail_unless(tree_value(tree_value(s)) == r.left);
   s = tree_stmt(p, 1);
   fail_unless(tree_kind(tree_value(s)) == T_ATTR_REF);
   fail_unless(tree_value(tree_value(s)) == r.right);
   s = tree_stmt(p, 2);
   fail_unless(tree_kind(tree_value(s)) == T_ATTR_REF);
   fail_unless(tree_value(tree_value(s)) == r.right);

}
END_TEST

START_TEST(test_ports)
{
   tree_t a, e, p;

   fail_unless(input_from_file(TESTDIR "/sem/ports.vhd"));

   const error_t expect[] = {
      { 31, "cannot read output port O" },
      { 42, "cannot assign to input port I" },
      { 66, "missing actual for formal O" },
      { 70, "formal I already has an actual" },
      { 74, "too many positional actuals" },
      { 77, "WORK.FOO has no formal CAKE" },
      { 79, "cannot find unit WORK.BAD" },
      { -1, NULL }
   };
   expect_errors(expect);

   p = parse();
   fail_if(p == NULL);
   fail_unless(tree_kind(p) == T_PACKAGE);
   sem_check(p);

   // Entity foo

   e = parse();
   fail_if(e == NULL);
   fail_unless(tree_kind(e) == T_ENTITY);
   sem_check(e);

   a = parse();
   fail_if(a == NULL);
   fail_unless(tree_kind(a) == T_ARCH);
   sem_check(a);

   // Entity top

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

   fail_unless(sem_errors() == (sizeof(expect) / sizeof(error_t)) - 1);
}
END_TEST


START_TEST(test_scope)
{
   tree_t a, e, p;

   fail_unless(input_from_file(TESTDIR "/sem/scope.vhd"));

   const error_t expect[] = {
      { 31, "WORK.PACK1.MY_INT1 does not match type"
        " of target WORK.PACK2.MY_INT1" },
      { 44, "WORK.PACK1.MY_INT1 does not match type of target MY_INT1" },
      { 63, "G already declared in this scope" },
      { 71, "P already declared in this scope" },
      { -1, NULL }
   };
   expect_errors(expect);

   p = parse();
   fail_if(p == NULL);
   fail_unless(tree_kind(p) == T_PACKAGE);
   sem_check(p);

   p = parse();
   fail_if(p == NULL);
   fail_unless(tree_kind(p) == T_PACKAGE);
   sem_check(p);

   e = parse();
   fail_if(e == NULL);
   fail_unless(tree_kind(e) == T_ENTITY);
   sem_check(e);

   a = parse();
   fail_if(a == NULL);
   fail_unless(tree_kind(a) == T_ARCH);
   sem_check(a);

   e = parse();
   fail_if(e == NULL);
   fail_unless(tree_kind(e) == T_ENTITY);
   sem_check(e);

   for (int i = 0; i < 3; i++) {
      a = parse();
      fail_if(a == NULL);
      fail_unless(tree_kind(a) == T_ARCH);
      sem_check(a);
   }

   fail_unless(parse() == NULL);
   fail_unless(parse_errors() == 0);

   fail_unless(sem_errors() == (sizeof(expect) / sizeof(error_t)) - 1);
}
END_TEST

START_TEST(test_ambiguous)
{
   tree_t a, e, p, s;
   type_t lhs, rhs;

   fail_unless(input_from_file(TESTDIR "/sem/ambiguous.vhd"));

   const error_t expect[] = {
      { 35, "type of value BAR does not match type of target FOO" },
      { 56, "type of aggregate is ambiguous" },
      { -1, NULL }
   };
   expect_errors(expect);

   e = parse();
   fail_if(e == NULL);
   fail_unless(tree_kind(e) == T_ENTITY);
   sem_check(e);

   a = parse();
   fail_if(a == NULL);
   fail_unless(tree_kind(a) == T_ARCH);
   fail_unless(tree_stmts(a) == 6);
   sem_check(a);

   p = tree_stmt(a, 0);
   fail_unless(tree_stmts(p) == 2);
   s = tree_stmt(p, 0);
   lhs = tree_type(tree_target(s));
   rhs = tree_type(tree_value(s));
   fail_unless(type_ident(lhs) == ident_new("FOO"));
   fail_unless(type_ident(rhs) == ident_new("FOO"));
   s = tree_stmt(p, 1);
   lhs = tree_type(tree_target(s));
   rhs = tree_type(tree_value(s));
   fail_unless(type_ident(lhs) == ident_new("BAR"));
   fail_unless(type_ident(rhs) == ident_new("BAR"));

   p = tree_stmt(a, 1);
   fail_unless(tree_stmts(p) == 2);
   s = tree_stmt(p, 0);
   lhs = tree_type(tree_target(s));
   rhs = tree_type(tree_value(s));
   fail_unless(type_ident(lhs) == ident_new("FOO"));
   fail_unless(type_ident(rhs) == ident_new("FOO"));
   s = tree_stmt(p, 1);
   lhs = tree_type(tree_target(s));
   rhs = tree_type(tree_value(s));
   fail_unless(type_ident(lhs) == ident_new("BAR"));
   fail_unless(type_ident(rhs) == ident_new("BAR"));

   p = tree_stmt(a, 2);
   fail_unless(tree_stmts(p) == 3);
   s = tree_stmt(p, 0);
   lhs = tree_type(tree_target(s));
   rhs = tree_type(tree_value(s));
   fail_unless(type_ident(lhs) == ident_new("BAZ"));
   fail_unless(type_ident(rhs) == ident_new("BAZ"));
   s = tree_stmt(p, 1);
   lhs = tree_type(tree_target(s));
   rhs = tree_type(tree_value(s));
   fail_unless(type_ident(lhs) == ident_new("BAZ"));
   fail_unless(type_ident(rhs) == ident_new("BAZ"));
   s = tree_stmt(p, 2);
   lhs = tree_type(tree_target(s));
   rhs = tree_type(tree_value(s));
   fail_unless(type_ident(lhs) == ident_new("FOO"));
   fail_unless(type_ident(rhs) == ident_new("FOO"));

   fail_unless(parse() == NULL);
   fail_unless(parse_errors() == 0);

   fail_unless(sem_errors() == (sizeof(expect) / sizeof(error_t)) - 1);
}
END_TEST

START_TEST(test_const)
{
   tree_t a, e;

   fail_unless(input_from_file(TESTDIR "/sem/const.vhd"));

   const error_t expect[] = {
      { 12, "invalid target of variable assignment" },
      { -1, NULL }
   };
   expect_errors(expect);

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

   fail_unless(sem_errors() == (sizeof(expect) / sizeof(error_t)) - 1);
}
END_TEST

START_TEST(test_std)
{
   tree_t a, e, d;

   fail_unless(input_from_file(TESTDIR "/sem/std.vhd"));

   e = parse();
   fail_if(e == NULL);
   fail_unless(tree_kind(e) == T_ENTITY);
   sem_check(e);

   a = parse();
   fail_if(a == NULL);
   fail_unless(tree_kind(a) == T_ARCH);
   fail_unless(tree_decls(a) == 3);
   sem_check(a);

   fail_unless(parse() == NULL);
   fail_unless(parse_errors() == 0);
   fail_unless(sem_errors() == 0);
}
END_TEST

START_TEST(test_wait)
{
   tree_t a, e, p, w;

   fail_unless(input_from_file(TESTDIR "/sem/wait.vhd"));

   const error_t expect[] = {
      { 14, "type of delay must be TIME" },
      { 23, "name V in sensitivity list is not a signal" },
      { 32, "undefined identifier A" },
      { 37, "wait statement not allowed in process" },
      { -1, NULL }
   };
   expect_errors(expect);

   e = parse();
   fail_if(e == NULL);
   fail_unless(tree_kind(e) == T_ENTITY);
   sem_check(e);

   a = parse();
   fail_if(a == NULL);
   fail_unless(tree_kind(a) == T_ARCH);
   sem_check(a);

   p = tree_stmt(a, 0);
   fail_unless(tree_kind(p) == T_PROCESS);
   fail_unless(tree_stmts(p) == 4);

   ident_t time = ident_new("STD.STANDARD.TIME");

   w = tree_stmt(p, 0);
   fail_unless(tree_kind(w) == T_WAIT);
   fail_unless(type_ident(tree_type(tree_delay(w))) == time);

   w = tree_stmt(p, 1);
   fail_unless(tree_kind(w) == T_WAIT);
   fail_unless(type_ident(tree_type(tree_delay(w))) == time);

   w = tree_stmt(p, 2);
   fail_unless(tree_kind(w) == T_WAIT);
   fail_unless(type_ident(tree_type(tree_delay(w))) == time);

   fail_unless(parse() == NULL);
   fail_unless(parse_errors() == 0);

   fail_unless(sem_errors() == (sizeof(expect) / sizeof(error_t)) - 1);
}
END_TEST

START_TEST(test_func)
{
   tree_t p;

   fail_unless(input_from_file(TESTDIR "/sem/func.vhd"));

   const error_t expect[] = {
      { 5, "function arguments must have mode IN" },
      { -1, NULL }
   };
   expect_errors(expect);

   p = parse();
   fail_if(p == NULL);
   fail_unless(tree_kind(p) == T_PACKAGE);
   sem_check(p);

   fail_unless(parse() == NULL);
   fail_unless(parse_errors() == 0);

   fail_unless(sem_errors() == (sizeof(expect) / sizeof(error_t)) - 1);
}
END_TEST

START_TEST(test_array)
{
   tree_t p, a, e;

   fail_unless(input_from_file(TESTDIR "/sem/array.vhd"));

   const error_t expect[] = {
      { 26, "positional associations must appear first in aggregate" },
      { 32, "named association must not follow others" },
      { 38, "only a single others association allowed" },
      { 45, "type of initial value universal integer does not match" },
      { 54, "type of value universal integer does not match type of" },
      { 56, "type of value WORK.P.INT_ARRAY does not match type" },
      { 64, "for operator \"=\"(WORK.P.INT_ARRAY, WORK.P.TEN_INTS)" },
      { -1, NULL }
   };
   expect_errors(expect);

   p = parse();
   fail_if(p == NULL);
   fail_unless(tree_kind(p) == T_PACKAGE);
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

   fail_unless(sem_errors() == (sizeof(expect) / sizeof(error_t)) - 1);
}
END_TEST

START_TEST(test_assert)
{
   tree_t a, e;

   fail_unless(input_from_file(TESTDIR "/sem/assert.vhd"));

   const error_t expect[] = {
      { 17, "type of assertion expression must be STD.STANDARD.BOOLEAN" },
      { 22, "type of message be STD.STANDARD.STRING" },
      { 27, "type of severity must be STD.STANDARD.SEVERITY_LEVEL" },
      { -1, NULL }
   };
   expect_errors(expect);

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

   fail_unless(sem_errors() == (sizeof(expect) / sizeof(error_t)) - 1);
}
END_TEST

START_TEST(test_generics)
{
   tree_t a, e, p;

   fail_unless(input_from_file(TESTDIR "/sem/generics.vhd"));

   const error_t expect[] = {
      { 34, "missing actual for formal N" },
      { 38, "too many positional actuals" },
      { -1, NULL }
   };
   expect_errors(expect);

   // Entity bot

   e = parse();
   fail_if(e == NULL);
   fail_unless(tree_kind(e) == T_ENTITY);
   sem_check(e);

   a = parse();
   fail_if(a == NULL);
   fail_unless(tree_kind(a) == T_ARCH);
   sem_check(a);

   // Entity top

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

   fail_unless(sem_errors() == (sizeof(expect) / sizeof(error_t)) - 1);
}
END_TEST

START_TEST(test_seq)
{
   tree_t a, e, p;

   fail_unless(input_from_file(TESTDIR "/sem/seq.vhd"));

   const error_t expect[] = {
      { 15, "type of test must be STD.STANDARD.BOOLEAN" },
      { 19, "undefined identifier X" },
      { 25, "no suitable overload for identifier TRUE" },
      { 32, "undefined identifier X" },
      { -1, NULL }
   };
   expect_errors(expect);

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

   fail_unless(sem_errors() == (sizeof(expect) / sizeof(error_t)) - 1);
}
END_TEST

int main(void)
{
   register_trace_signal_handlers();

   setenv("NVC_LIBPATH", "../lib/std", 1);

   Suite *s = suite_create("sem");

   TCase *tc_core = tcase_create("Core");
   tcase_add_unchecked_fixture(tc_core, setup, teardown);
   tcase_add_test(tc_core, test_integer);
   tcase_add_test(tc_core, test_ports);
   tcase_add_test(tc_core, test_scope);
   tcase_add_test(tc_core, test_ambiguous);
   tcase_add_test(tc_core, test_const);
   tcase_add_test(tc_core, test_std);
   tcase_add_test(tc_core, test_wait);
   tcase_add_test(tc_core, test_func);
   tcase_add_test(tc_core, test_array);
   tcase_add_test(tc_core, test_assert);
   tcase_add_test(tc_core, test_generics);
   tcase_add_test(tc_core, test_seq);
   suite_add_tcase(s, tc_core);

   SRunner *sr = srunner_create(s);
   srunner_run_all(sr, CK_NORMAL);

   int nfail = srunner_ntests_failed(sr);

   srunner_free(sr);

   return nfail == 0 ? EXIT_SUCCESS : EXIT_FAILURE;
}
