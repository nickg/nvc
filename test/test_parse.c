#include "parse.h"
#include "type.h"
#include "util.h"

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
   fail_unless(tree_ident(e) == ident_new("ONE"));

   e = parse();
   fail_if(e == NULL);
   fail_unless(tree_kind(e) == T_ENTITY);
   fail_unless(tree_ident(e) == ident_new("TWO"));
   
   e = parse();
   fail_if(e == NULL);
   fail_unless(tree_kind(e) == T_ENTITY);
   fail_unless(tree_ident(e) == ident_new("THREE"));

   e = parse();
   fail_if(e == NULL);
   fail_unless(tree_kind(e) == T_ENTITY);
   fail_unless(tree_ident(e) == ident_new("FOUR"));

   fail_unless(tree_ports(e) == 5);

   p = tree_port(e, 0);
   fail_unless(tree_kind(p) == T_PORT_DECL);
   fail_unless(tree_ident(p) == ident_new("A"));
   fail_unless(tree_port_mode(p) == PORT_IN);
   t = tree_type(p);
   fail_unless(type_kind(t) == T_UNRESOLVED);
   fail_unless(type_ident(t) == ident_new("INTEGER"));
   fail_unless(tree_has_value(p));
   v = tree_value(p);
   fail_unless(tree_kind(v) == T_LITERAL);
   l = tree_literal(v);
   fail_unless(l.kind == L_INT);
   fail_unless(l.u.i == 4);

   p = tree_port(e, 1);
   fail_unless(tree_kind(p) == T_PORT_DECL);
   fail_unless(tree_ident(p) == ident_new("B"));
   fail_unless(tree_port_mode(p) == PORT_OUT);
   t = tree_type(p);
   fail_unless(type_kind(t) == T_UNRESOLVED);
   fail_unless(type_ident(t) == ident_new("BIT"));
   fail_if(tree_has_value(p));

   p = tree_port(e, 2);
   fail_unless(tree_kind(p) == T_PORT_DECL);
   fail_unless(tree_ident(p) == ident_new("BEE"));
   fail_unless(tree_port_mode(p) == PORT_OUT);
   t = tree_type(p);
   fail_unless(type_kind(t) == T_UNRESOLVED);
   fail_unless(type_ident(t) == ident_new("BIT"));
   fail_if(tree_has_value(p));

   p = tree_port(e, 3);
   fail_unless(tree_kind(p) == T_PORT_DECL);
   fail_unless(tree_ident(p) == ident_new("C"));
   fail_unless(tree_port_mode(p) == PORT_INOUT);
   t = tree_type(p);
   fail_unless(type_kind(t) == T_UNRESOLVED);
   fail_unless(type_ident(t) == ident_new("INTEGER"));
   fail_if(tree_has_value(p));

   p = tree_port(e, 4);
   fail_unless(tree_kind(p) == T_PORT_DECL);
   fail_unless(tree_ident(p) == ident_new("D"));
   fail_unless(tree_port_mode(p) == PORT_BUFFER);
   t = tree_type(p);
   fail_unless(type_kind(t) == T_UNRESOLVED);
   fail_unless(type_ident(t) == ident_new("BIT"));
   fail_if(tree_has_value(p));

   e = parse();
   fail_if(e == NULL);
   fail_unless(tree_kind(e) == T_ENTITY);
   fail_unless(tree_ident(e) == ident_new("FIVE"));

   fail_unless(tree_generics(e) == 2);

   g = tree_generic(e, 0);
   fail_unless(tree_kind(g) == T_PORT_DECL);
   fail_unless(tree_ident(g) == ident_new("X"));
   t = tree_type(g);
   fail_unless(type_kind(t) == T_UNRESOLVED);
   fail_unless(type_ident(t) == ident_new("BOOLEAN"));
   fail_if(tree_has_value(p));

   g = tree_generic(e, 1);
   fail_unless(tree_kind(g) == T_PORT_DECL);
   fail_unless(tree_ident(g) == ident_new("Y"));
   t = tree_type(g);
   fail_unless(type_kind(t) == T_UNRESOLVED);
   fail_unless(type_ident(t) == ident_new("INTEGER"));
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
   fail_unless(tree_ident(p) == ident_new("P"));
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
   fail_unless(tree_ident(a) == ident_new("A"));
   fail_unless(tree_ident2(a) == ident_new("ONE"));
   fail_unless(tree_decls(a) == 3);
   d = tree_decl(a, 0);
   fail_unless(tree_kind(d) == T_SIGNAL_DECL);
   fail_unless(tree_ident(d) == ident_new("X"));
   fail_unless(type_kind(tree_type(d)) == T_UNRESOLVED);
   fail_unless(type_ident(tree_type(d)) == ident_new("INTEGER"));
   fail_if(tree_has_value(d));
   d = tree_decl(a, 1);
   fail_unless(tree_kind(d) == T_SIGNAL_DECL);
   fail_unless(tree_ident(d) == ident_new("Y"));
   fail_unless(type_kind(tree_type(d)) == T_UNRESOLVED);
   fail_unless(type_ident(tree_type(d)) == ident_new("INTEGER"));
   fail_unless(tree_has_value(d));
   v = tree_value(d);
   fail_unless(tree_kind(v) == T_LITERAL);
   l = tree_literal(v);
   fail_unless(l.kind == L_INT);
   fail_unless(l.u.i == 7);
   d = tree_decl(a, 2);
   fail_unless(tree_kind(d) == T_SIGNAL_DECL);
   fail_unless(tree_ident(d) == ident_new("Z"));
   fail_unless(type_kind(tree_type(d)) == T_UNRESOLVED);
   fail_unless(type_ident(tree_type(d)) == ident_new("INTEGER"));
   fail_unless(tree_has_value(d));
   v = tree_value(d);
   fail_unless(tree_kind(v) == T_LITERAL);
   l = tree_literal(v);
   fail_unless(l.kind == L_INT);
   fail_unless(l.u.i == 7);
   
   a = parse();
   fail_if(a == NULL);
   fail_unless(tree_kind(a) == T_ARCH);
   fail_unless(tree_ident(a) == ident_new("B"));
   fail_unless(tree_ident2(a) == ident_new("ONE"));
   
   a = parse();
   fail_if(a == NULL);
   fail_unless(tree_kind(a) == T_ARCH);
   fail_unless(tree_ident(a) == ident_new("C"));
   fail_unless(tree_ident2(a) == ident_new("ONE"));
   
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
   fail_unless(tree_ident(p) == ident_new("P"));

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

START_TEST(test_seq)
{
   tree_t a, p, s, e;

   fail_unless(input_from_file(TESTDIR "/parse/seq.vhd"));

   a = parse();
   fail_if(a == NULL);
   fail_unless(tree_kind(a) == T_ARCH);
   fail_unless(tree_stmts(a) == 2);

   // Wait statements
   
   p = tree_stmt(a, 0);
   fail_unless(tree_kind(p) == T_PROCESS);
   fail_unless(tree_stmts(p) == 1);

   s = tree_stmt(p, 0);
   fail_unless(tree_kind(s) == T_WAIT);
   fail_unless(tree_has_delay(s));
   e = tree_delay(s);
   fail_unless(tree_kind(e) == T_FCALL);
   fail_unless(tree_ident(e) == ident_new("*"));
   fail_unless(tree_params(e) == 2);
   fail_unless(tree_kind(tree_param(e, 0)) == T_LITERAL);
   fail_unless(tree_kind(tree_param(e, 1)) == T_REF);
   fail_unless(tree_ident(tree_param(e, 1)) == ident_new("NS"));

   // Variable assignment
   
   p = tree_stmt(a, 1);
   fail_unless(tree_kind(p) == T_PROCESS);
   fail_unless(tree_stmts(p) == 2);

   s = tree_stmt(p, 0);
   fail_unless(tree_kind(s) == T_VAR_ASSIGN);
   e = tree_target(s);
   fail_unless(tree_kind(e) == T_REF);
   fail_unless(tree_ident(e) == ident_new("A"));
   e = tree_value(s);
   fail_unless(tree_kind(e) == T_LITERAL);

   s = tree_stmt(p, 1);
   fail_unless(tree_kind(s) == T_VAR_ASSIGN);
   e = tree_value(s);
   fail_unless(tree_kind(e) == T_FCALL);
   fail_unless(tree_ident(e) == ident_new("+"));
   fail_unless(tree_params(e) == 2);
   fail_unless(tree_kind(tree_param(e, 0)) == T_REF);
   e = tree_param(e, 1);
   fail_unless(tree_kind(e) == T_FCALL);
   fail_unless(tree_ident(e) == ident_new("*"));
   fail_unless(tree_params(e) == 2);
   fail_unless(tree_kind(tree_param(e, 0)) == T_REF);
   fail_unless(tree_kind(tree_param(e, 1)) == T_LITERAL);

   a = parse();
   fail_unless(a == NULL);
   
   fail_unless(parse_errors() == 0);
}
END_TEST

START_TEST(test_types)
{
   tree_t a, d;
   type_t t;
   range_t r;
   unit_t u;

   fail_unless(input_from_file(TESTDIR "/parse/types.vhd"));

   a = parse();
   fail_if(a == NULL);
   fail_unless(tree_kind(a) == T_ARCH);
   fail_unless(tree_decls(a) == 4);

   d = tree_decl(a, 0);
   fail_unless(tree_kind(d) == T_TYPE_DECL);
   fail_unless(tree_ident(d) == ident_new("MY_INT"));
   t = tree_type(d);
   fail_unless(type_kind(t) == T_INTEGER);
   fail_unless(type_dims(t) == 1);
   r = type_dim(t, 0);
   fail_unless(r.kind == RANGE_TO);
   fail_unless(tree_kind(r.left) == T_LITERAL);
   fail_unless(tree_kind(r.right) == T_LITERAL);

   d = tree_decl(a, 2);
   fail_unless(tree_kind(d) == T_TYPE_DECL);
   fail_unless(tree_ident(d) == ident_new("RESISTANCE"));
   t = tree_type(d);
   fail_unless(type_kind(t) == T_PHYSICAL);
   fail_unless(type_dims(t) == 1);
   r = type_dim(t, 0);
   fail_unless(r.kind == RANGE_TO);
   fail_unless(tree_kind(r.left) == T_LITERAL);
   fail_unless(tree_kind(r.right) == T_LITERAL);
   fail_unless(type_units(t) == 3);
   u = type_unit(t, 0);
   fail_unless(u.name == ident_new("OHM"));
   fail_unless(tree_kind(u.multiplier) == T_LITERAL);
   u = type_unit(t, 1);
   fail_unless(u.name == ident_new("KOHM"));
   fail_unless(tree_kind(u.multiplier) == T_FCALL);
   u = type_unit(t, 2);
   fail_unless(u.name == ident_new("MOHM"));
   fail_unless(tree_kind(u.multiplier) == T_FCALL);
   
   a = parse();
   fail_unless(a == NULL);
   
   fail_unless(parse_errors() == 0);
}
END_TEST

START_TEST(test_literal)
{
   tree_t a, d, v;
   literal_t l;

   fail_unless(input_from_file(TESTDIR "/parse/literal.vhd"));

   a = parse();
   fail_if(a == NULL);
   fail_unless(tree_kind(a) == T_ARCH);
   fail_unless(tree_decls(a) == 2);

   d = tree_decl(a, 0);
   fail_unless(tree_ident(d) == ident_new("POS"));
   v = tree_value(d);
   fail_unless(tree_kind(v) == T_LITERAL);
   l = tree_literal(v);
   fail_unless(l.kind == L_INT);
   fail_unless(l.u.i == 64);

   d = tree_decl(a, 1);
   fail_unless(tree_ident(d) == ident_new("NEG"));
   v = tree_value(d);
   fail_unless(tree_kind(v) == T_FCALL);
   fail_unless(tree_ident(v) == ident_new("-"));
   fail_unless(tree_params(v) == 1);
   l = tree_literal(tree_param(v, 0));
   fail_unless(l.kind == L_INT);
   fail_unless(l.u.i == 265);
   
   a = parse();
   fail_unless(a == NULL);
   
   fail_unless(parse_errors() == 0);
}
END_TEST

START_TEST(test_extended)
{
   tree_t a, d, n;

   fail_unless(input_from_file(TESTDIR "/parse/extended.vhd"));

   a = parse();
   fail_if(a == NULL);
   fail_unless(tree_kind(a) == T_ARCH);
   fail_unless(tree_decls(a) == 5);

   d = tree_decl(a, 0);
   fail_unless(tree_ident(d) == ident_new("\\foo bar\\"));
   d = tree_decl(a, 1);
   fail_unless(tree_ident(d) == ident_new("\\a\\b\\"));
   d = tree_decl(a, 2);
   fail_unless(tree_ident(d) == ident_new("\\Thing!!!  \\"));
   d = tree_decl(a, 3);
   fail_unless(tree_ident(d) == ident_new("\\name\\"));
   n = tree_decl(a, 4);
   // LRM states extended identifiers distinct from regular
   fail_if(d == n);
   
   a = parse();
   fail_unless(a == NULL);
   
   fail_unless(parse_errors() == 0);
}
END_TEST

START_TEST(test_package)
{
   tree_t p, d;

   fail_unless(input_from_file(TESTDIR "/parse/package.vhd"));

   p = parse();
   fail_if(p == NULL);
   fail_unless(tree_kind(p) == T_PACKAGE);
   fail_unless(tree_decls(p) == 1);
   fail_unless(tree_contexts(p) == 0);
   
   p = parse();
   fail_if(p == NULL);
   fail_unless(tree_kind(p) == T_PACKAGE);
   fail_unless(tree_decls(p) == 1);
   fail_unless(tree_contexts(p) == 1);
   fail_unless(tree_context(p, 0) == ident_new("WORK.ONE.all"));

   p = parse();
   fail_unless(p == NULL);

   fail_unless(parse_errors() == 0);
}
END_TEST

int main(void)
{
   register_trace_signal_handlers();
   
   Suite *s = suite_create("parse");  

   TCase *tc_core = tcase_create("Core");
   tcase_add_test(tc_core, test_entity);
   tcase_add_test(tc_core, test_arch);
   tcase_add_test(tc_core, test_process);
   tcase_add_test(tc_core, test_seq);
   tcase_add_test(tc_core, test_types);
   tcase_add_test(tc_core, test_literal);
   tcase_add_test(tc_core, test_extended);
   tcase_add_test(tc_core, test_package);
   suite_add_tcase(s, tc_core);
   
   SRunner *sr = srunner_create(s);
   srunner_run_all(sr, CK_NORMAL);

   int nfail = srunner_ntests_failed(sr);

   srunner_free(sr);
   
   return nfail == 0 ? EXIT_SUCCESS : EXIT_FAILURE;
}
