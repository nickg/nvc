#include "parse.h"

#include <check.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

START_TEST(test_entity)
{
   tree_t t, p;
   
   fail_unless(input_from_file(TESTDIR "/parse/entity.vhd"));
   
   t = parse();
   fail_if(t == NULL);
   fail_unless(tree_kind(t) == T_ENTITY);
   fail_unless(tree_ident(t) == ident_new("one"));

   t = parse();
   fail_if(t == NULL);
   fail_unless(tree_kind(t) == T_ENTITY);
   fail_unless(tree_ident(t) == ident_new("two"));
   
   t = parse();
   fail_if(t == NULL);
   fail_unless(tree_kind(t) == T_ENTITY);
   fail_unless(tree_ident(t) == ident_new("three"));

   t = parse();
   fail_if(t == NULL);
   fail_unless(tree_kind(t) == T_ENTITY);
   fail_unless(tree_ident(t) == ident_new("four"));

   fail_unless(tree_ports(t) == 4);

   p = tree_port(t, 0);
   fail_unless(tree_kind(p) == T_PORT_DECL);
   fail_unless(tree_ident(p) == ident_new("a"));
   fail_unless(tree_port_mode(p) == PORT_IN);

   p = tree_port(t, 1);
   fail_unless(tree_kind(p) == T_PORT_DECL);
   fail_unless(tree_ident(p) == ident_new("b"));
   fail_unless(tree_port_mode(p) == PORT_OUT);

   p = tree_port(t, 2);
   fail_unless(tree_kind(p) == T_PORT_DECL);
   fail_unless(tree_ident(p) == ident_new("c"));
   fail_unless(tree_port_mode(p) == PORT_INOUT);

   p = tree_port(t, 3);
   fail_unless(tree_kind(p) == T_PORT_DECL);
   fail_unless(tree_ident(p) == ident_new("d"));
   fail_unless(tree_port_mode(p) == PORT_BUFFER);
      
   t = parse();
   fail_unless(t == NULL);
   
   fail_unless(parse_errors() == 0);
}
END_TEST

int main(void)
{
   Suite *s = suite_create("parse");

   TCase *tc_core = tcase_create("Core");
   tcase_add_test(tc_core, test_entity);
   suite_add_tcase(s, tc_core);
   
   SRunner *sr = srunner_create(s);
   srunner_run_all(sr, CK_NORMAL);

   int nfail = srunner_ntests_failed(sr);

   srunner_free(sr);
   
   return nfail == 0 ? EXIT_SUCCESS : EXIT_FAILURE;
}
