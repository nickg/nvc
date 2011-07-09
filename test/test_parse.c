#include "parse.h"
#include "type.h"

#include <check.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

START_TEST(test_entity)
{
   tree_t e, p, g, v, x, y;
   type_t t;
   literal_t l;
   
   fail_unless(input_from_file(TESTDIR "/parse/entity.vhd"));
   
   e = parse();
   fail_if(e == NULL);
   fail_unless(tree_kind(e) == T_ENTITY);
   fail_unless(tree_ident(e) == ident_new("one"));

   e = parse();
   fail_if(e == NULL);
   fail_unless(tree_kind(e) == T_ENTITY);
   fail_unless(tree_ident(e) == ident_new("two"));
   
   e = parse();
   fail_if(e == NULL);
   fail_unless(tree_kind(e) == T_ENTITY);
   fail_unless(tree_ident(e) == ident_new("three"));

   e = parse();
   fail_if(e == NULL);
   fail_unless(tree_kind(e) == T_ENTITY);
   fail_unless(tree_ident(e) == ident_new("four"));

   fail_unless(tree_ports(e) == 5);

   p = tree_port(e, 0);
   fail_unless(tree_kind(p) == T_PORT_DECL);
   fail_unless(tree_ident(p) == ident_new("a"));
   fail_unless(tree_port_mode(p) == PORT_IN);
   t = tree_type(p);
   fail_unless(type_kind(t) == T_UNRESOLVED);
   fail_unless(type_ident(t) == ident_new("integer"));
   fail_unless(tree_has_value(p));
   v = tree_value(p);
   fail_unless(tree_kind(v) == T_LITERAL);
   l = tree_literal(v);
   fail_unless(l.kind == L_INT);
   fail_unless(l.u.i == 4);

   p = tree_port(e, 1);
   fail_unless(tree_kind(p) == T_PORT_DECL);
   fail_unless(tree_ident(p) == ident_new("b"));
   fail_unless(tree_port_mode(p) == PORT_OUT);
   t = tree_type(p);
   fail_unless(type_kind(t) == T_UNRESOLVED);
   fail_unless(type_ident(t) == ident_new("bit"));
   fail_if(tree_has_value(p));

   p = tree_port(e, 2);
   fail_unless(tree_kind(p) == T_PORT_DECL);
   fail_unless(tree_ident(p) == ident_new("bee"));
   fail_unless(tree_port_mode(p) == PORT_OUT);
   t = tree_type(p);
   fail_unless(type_kind(t) == T_UNRESOLVED);
   fail_unless(type_ident(t) == ident_new("bit"));
   fail_if(tree_has_value(p));

   p = tree_port(e, 3);
   fail_unless(tree_kind(p) == T_PORT_DECL);
   fail_unless(tree_ident(p) == ident_new("c"));
   fail_unless(tree_port_mode(p) == PORT_INOUT);
   t = tree_type(p);
   fail_unless(type_kind(t) == T_UNRESOLVED);
   fail_unless(type_ident(t) == ident_new("integer"));
   fail_if(tree_has_value(p));

   p = tree_port(e, 4);
   fail_unless(tree_kind(p) == T_PORT_DECL);
   fail_unless(tree_ident(p) == ident_new("d"));
   fail_unless(tree_port_mode(p) == PORT_BUFFER);
   t = tree_type(p);
   fail_unless(type_kind(t) == T_UNRESOLVED);
   fail_unless(type_ident(t) == ident_new("bit"));
   fail_if(tree_has_value(p));

   e = parse();
   fail_if(e == NULL);
   fail_unless(tree_kind(e) == T_ENTITY);
   fail_unless(tree_ident(e) == ident_new("five"));

   fail_unless(tree_generics(e) == 2);

   g = tree_generic(e, 0);
   fail_unless(tree_kind(g) == T_PORT_DECL);
   fail_unless(tree_ident(g) == ident_new("x"));
   t = tree_type(g);
   fail_unless(type_kind(t) == T_UNRESOLVED);
   fail_unless(type_ident(t) == ident_new("boolean"));
   fail_if(tree_has_value(p));

   g = tree_generic(e, 1);
   fail_unless(tree_kind(g) == T_PORT_DECL);
   fail_unless(tree_ident(g) == ident_new("y"));
   t = tree_type(g);
   fail_unless(type_kind(t) == T_UNRESOLVED);
   fail_unless(type_ident(t) == ident_new("integer"));
   fail_unless(tree_has_value(g));
   v = tree_value(g);
   fail_unless(tree_kind(v) == T_FCALL);
   fail_unless(tree_ident(v) == ident_new("*"));
   fail_unless(tree_params(v) == 2);
   x = tree_param(v, 0);
   fail_unless(tree_kind(x) == T_LITERAL);
   l = tree_literal(x);
   fail_unless(l.kind == L_INT);
   fail_unless(l.u.i == 2);
   y = tree_param(v, 1);
   fail_unless(tree_kind(y) == T_LITERAL);
   l = tree_literal(y);
   fail_unless(l.kind == L_INT);
   fail_unless(l.u.i == 5);
   
   fail_unless(tree_ports(e) == 1);

   p = tree_port(e, 0);
   fail_unless(tree_kind(p) == T_PORT_DECL);
   fail_unless(tree_ident(p) == ident_new("p"));
   fail_unless(tree_port_mode(p) == PORT_OUT);
   fail_if(tree_has_value(p));
   
   e = parse();
   fail_unless(e == NULL);
   
   fail_unless(parse_errors() == 0);
}
END_TEST

START_TEST(test_arch)
{
   tree_t a, d, v;
   literal_t l;
   
   fail_unless(input_from_file(TESTDIR "/parse/arch.vhd"));

   a = parse();
   fail_if(a == NULL);
   fail_unless(tree_kind(a) == T_ARCH);
   fail_unless(tree_ident(a) == ident_new("a"));
   fail_unless(tree_ident2(a) == ident_new("one"));
   fail_unless(tree_decls(a) == 3);
   d = tree_decl(a, 0);
   fail_unless(tree_kind(d) == T_SIGNAL_DECL);
   fail_unless(tree_ident(d) == ident_new("x"));
   fail_unless(type_kind(tree_type(d)) == T_UNRESOLVED);
   fail_unless(type_ident(tree_type(d)) == ident_new("integer"));
   fail_if(tree_has_value(d));
   d = tree_decl(a, 1);
   fail_unless(tree_kind(d) == T_SIGNAL_DECL);
   fail_unless(tree_ident(d) == ident_new("y"));
   fail_unless(type_kind(tree_type(d)) == T_UNRESOLVED);
   fail_unless(type_ident(tree_type(d)) == ident_new("integer"));
   fail_unless(tree_has_value(d));
   v = tree_value(d);
   fail_unless(tree_kind(v) == T_LITERAL);
   l = tree_literal(v);
   fail_unless(l.kind == L_INT);
   fail_unless(l.u.i == 7);
   d = tree_decl(a, 2);
   fail_unless(tree_kind(d) == T_SIGNAL_DECL);
   fail_unless(tree_ident(d) == ident_new("z"));
   fail_unless(type_kind(tree_type(d)) == T_UNRESOLVED);
   fail_unless(type_ident(tree_type(d)) == ident_new("integer"));
   fail_unless(tree_has_value(d));
   v = tree_value(d);
   fail_unless(tree_kind(v) == T_LITERAL);
   l = tree_literal(v);
   fail_unless(l.kind == L_INT);
   fail_unless(l.u.i == 7);
   
   a = parse();
   fail_if(a == NULL);
   fail_unless(tree_kind(a) == T_ARCH);
   fail_unless(tree_ident(a) == ident_new("b"));
   fail_unless(tree_ident2(a) == ident_new("one"));
   
   a = parse();
   fail_if(a == NULL);
   fail_unless(tree_kind(a) == T_ARCH);
   fail_unless(tree_ident(a) == ident_new("c"));
   fail_unless(tree_ident2(a) == ident_new("one"));
   
   a = parse();
   fail_unless(a == NULL);
   
   fail_unless(parse_errors() == 0);
}
END_TEST

START_TEST(test_process)
{
   tree_t a, p, d;

   fail_unless(input_from_file(TESTDIR "/parse/process.vhd"));

   a = parse();
   fail_if(a == NULL);
   fail_unless(tree_kind(a) == T_ARCH);
   fail_unless(tree_stmts(a) == 2);

   p = tree_stmt(a, 0);
   fail_unless(tree_kind(p) == T_PROCESS);
   fail_unless(tree_ident(p) == ident_new("p"));

   p = tree_stmt(a, 1);
   fail_unless(tree_kind(p) == T_PROCESS);
   fail_if(tree_ident(p) == NULL);
   fail_unless(tree_decls(p) == 1);
   d = tree_decl(p, 0);
   fail_unless(tree_kind(d) == T_VAR_DECL);
   
   a = parse();
   fail_unless(a == NULL);
   
   fail_unless(parse_errors() == 0);
}
END_TEST

int main(void)
{
   Suite *s = suite_create("parse");

   TCase *tc_core = tcase_create("Core");
   tcase_add_test(tc_core, test_entity);
   tcase_add_test(tc_core, test_arch);
   tcase_add_test(tc_core, test_process);
   suite_add_tcase(s, tc_core);
   
   SRunner *sr = srunner_create(s);
   srunner_run_all(sr, CK_NORMAL);

   int nfail = srunner_ntests_failed(sr);

   srunner_free(sr);
   
   return nfail == 0 ? EXIT_SUCCESS : EXIT_FAILURE;
}
