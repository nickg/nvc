#include "parse.h"
#include "type.h"
#include "sem.h"
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
static sem_error_fn_t orig_error_fn;

static void test_error_fn(const char *msg, const loc_t *loc)
{
#if 0
   orig_error_fn(msg, loc);
#endif
   
   fail_if(error_lines == NULL);
   fail_if(error_lines->line == -1 || error_lines->snippet == NULL);
   fail_unless(error_lines->line == loc->first_line);
   fail_if(strstr(msg, error_lines->snippet) == NULL);

   error_lines++;
}

static void expect_errors(const error_t *lines)
{
   orig_error_fn = sem_set_error_fn(test_error_fn);
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

   a = parse();
   fail_if(a == NULL);
   fail_unless(tree_kind(a) == T_ARCH);
   
   fail_unless(parse() == NULL);
   fail_unless(parse_errors() == 0);

   const error_t expect[] = {
      { 20, "MY_INT1 does not match type of target MY_INT2" },
      { 30, "MY_INT1 does not match type of target MY_INT2_SUB" },
      { 35, "type NOTHING is not defined" },
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

   fail_unless(tree_stmts(a) == 5);

   // Process 1
   
   p = tree_stmt(a, 0);
   fail_unless(tree_kind(p) == T_PROCESS);

   fail_unless(tree_decls(p) == 1);

   d = tree_decl(p, 0);
   fail_unless(tree_ident(d) == ident_new("Z"));
   fail_unless(type_kind(tree_type(d)) == T_INTEGER);

   s = tree_stmt(p, 0);
   fail_unless(tree_ref(tree_target(s)) == d);   
}
END_TEST

int main(void)
{
   register_trace_signal_handlers();
   
   Suite *s = suite_create("sem");

   TCase *tc_core = tcase_create("Core");
   tcase_add_test(tc_core, test_integer);
   suite_add_tcase(s, tc_core);
   
   SRunner *sr = srunner_create(s);
   srunner_run_all(sr, CK_NORMAL);

   int nfail = srunner_ntests_failed(sr);

   srunner_free(sr);
   
   return nfail == 0 ? EXIT_SUCCESS : EXIT_FAILURE;
}
