#include "parse.h"
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

START_TEST(test_bounds)
{
   tree_t e, a, p, s;
   range_t r;

   const error_t expect[] = {
      { 21, "left index 0 violates constraint STD.STANDARD.POSITIVE" },
      { 22, "right index 60 violates constraint FOO" },
      { 26, "array index -52 out of bounds 1 to 10" },
      { 27, "slice right index 11 out of bounds 1 to 10" },
      { 28, "slice left index 0 out of bounds 1 to 10" },
      { 32, "aggregate index 0 out of bounds 1 to 2147483647" },
      { 41, "actual length 8 does not match formal length 4" },
      { 42, "actual length 8 does not match formal length 4" },
      { 45, "actual length 3 for dimension 2 does not" },
      { 48, "length of value 9 does not match length of target 10" },
      { 49, "length of value 2 does not match length of target 0" },
      { 50, "expected 0 elements in aggregate but have 3" },
      { 55, "length of value 10 does not match length of target 3" },
      { 61, "array index 11 out of bounds 1 to 10" },
      { 62, "array index -1 out of bounds 1 to 10" },
      { 69, "aggregate index 5 out of bounds 1 to 3" },
      { 69, "aggregate index 0 out of bounds 1 to 3" },
      { 78, "value '1' out of target bounds 'a' to 'z'" },
      { 79, "value 0 out of target bounds 1 to 2147483647" },
      { 84, "invalid dimension 5 for type MY_VEC1" },
      { -1, NULL }
   };
   expect_errors(expect);

   input_from_file(TESTDIR "/bounds/bounds.vhd");

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
   bounds_check(a);

   fail_unless(bounds_errors() == (sizeof(expect) / sizeof(error_t)) - 1);
}
END_TEST

START_TEST(test_case)
{
   tree_t e, a, p, s;
   range_t r;

   const error_t expect[] = {
      { 13, "missing choice C in case statement" },
      { 19, "missing choice B in case statement" },
      { 30, "10 to 19" },
      { 36, "4 to 2147483647" },
      { 44, "2147483647" },
      { 51, "value 50 is already covered" },
      { 53, "range 60 to 64 is already covered" },
      { 59, "value -1 outside STD.STANDARD.NATURAL bounds" },
      { 58, "0 to 2147483647" },
      { -1, NULL }
   };
   expect_errors(expect);

   input_from_file(TESTDIR "/bounds/case.vhd");

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
   bounds_check(a);

   fail_unless(bounds_errors() == (sizeof(expect) / sizeof(error_t)) - 1);
}
END_TEST

int main(void)
{
   register_trace_signal_handlers();

   setenv("NVC_LIBPATH", "../lib/std", 1);

   Suite *s = suite_create("bounds");

   TCase *tc_core = tcase_create("Core");
   tcase_add_unchecked_fixture(tc_core, setup, teardown);
   tcase_add_test(tc_core, test_bounds);
   tcase_add_test(tc_core, test_case);
   suite_add_tcase(s, tc_core);

   SRunner *sr = srunner_create(s);
   srunner_run_all(sr, CK_NORMAL);

   int nfail = srunner_ntests_failed(sr);

   srunner_free(sr);

   return nfail == 0 ? EXIT_SUCCESS : EXIT_FAILURE;
}
