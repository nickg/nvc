#include "parse.h"
#include "type.h"
#include "sem.h"

#include <check.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

START_TEST(test_integer)
{
   tree_t a, d, p, s, e;
   type_t t;
   range_t r;

   fail_unless(input_from_file(TESTDIR "/sem/integer.vhd"));

   a = parse();
   fail_if(a == NULL);
   fail_unless(tree_kind(a) == T_ARCH);
   
   fail_unless(parse() == NULL);
   fail_unless(parse_errors() == 0);

   sem_check(a);
   fail_unless(sem_errors() == 0);

   d = tree_decl(a, 2);
   fail_unless(tree_ident(d) == ident_new("X"));
   t = tree_type(d);
   fail_unless(type_kind(t) == T_INTEGER);
   e = tree_value(d);
   fail_unless(tree_kind(e) == T_LITERAL);
   t = tree_type(e);
   fail_unless(type_kind(t) == T_INTEGER);

   fail_unless(tree_stmts(a) == 1);

   p = tree_stmt(a, 0);
   fail_unless(tree_kind(p) == T_PROCESS);

   fail_unless(tree_decls(p) == 1);

   d = tree_decl(p, 0);
   fail_unless(tree_ident(d) == ident_new("Y"));
   fail_unless(type_kind(tree_type(d)) == T_INTEGER);
}
END_TEST

int main(void)
{
   Suite *s = suite_create("parse");

   TCase *tc_core = tcase_create("Core");
   tcase_add_test(tc_core, test_integer);
   suite_add_tcase(s, tc_core);
   
   SRunner *sr = srunner_create(s);
   srunner_run_all(sr, CK_NORMAL);

   int nfail = srunner_ntests_failed(sr);

   srunner_free(sr);
   
   return nfail == 0 ? EXIT_SUCCESS : EXIT_FAILURE;
}
