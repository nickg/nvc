#include "parse.h"
#include "type.h"
#include "sem.h"

#include <check.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

START_TEST(test_integer)
{
   tree_t a, d;
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
   fail_unless(tree_ident(d) == ident_new("x"));
   t = tree_type(d);
   fail_unless(type_kind(t) == T_INTEGER);
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
