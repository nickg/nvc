#include "parse.h"
#include "type.h"
#include "util.h"

#include <check.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

START_TEST(test_entity)
{
   tree_t e, p, g, v;
   param_t x, y;
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
   fail_unless(l.i == 4);

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
   fail_unless(x.kind == P_POS);
   fail_unless(x.pos == 0);
   fail_unless(tree_kind(x.value) == T_LITERAL);
   l = tree_literal(x.value);
   fail_unless(l.kind == L_INT);
   fail_unless(l.i == 2);
   y = tree_param(v, 1);
   fail_unless(y.kind == P_POS);
   fail_unless(y.pos == 1);
   fail_unless(tree_kind(y.value) == T_LITERAL);
   l = tree_literal(y.value);
   fail_unless(l.kind == L_INT);
   fail_unless(l.i == 5);

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
   fail_unless(l.i == 7);
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
   fail_unless(l.i == 7);

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
   tree_t a, p, d, s;

   fail_unless(input_from_file(TESTDIR "/parse/process.vhd"));

   a = parse();
   fail_if(a == NULL);
   fail_unless(tree_kind(a) == T_ARCH);
   fail_unless(tree_stmts(a) == 3);

   p = tree_stmt(a, 0);
   fail_unless(tree_kind(p) == T_PROCESS);
   fail_unless(tree_ident(p) == ident_new("P"));

   p = tree_stmt(a, 1);
   fail_unless(tree_kind(p) == T_PROCESS);
   fail_if(tree_ident(p) == NULL);
   fail_unless(tree_decls(p) == 1);
   d = tree_decl(p, 0);
   fail_unless(tree_kind(d) == T_VAR_DECL);
   fail_unless(tree_stmts(p) == 1);
   s = tree_stmt(p, 0);
   fail_unless(tree_kind(s) == T_SIGNAL_ASSIGN);

   p = tree_stmt(a, 2);
   fail_unless(tree_kind(p) == T_PROCESS);
   fail_unless(tree_triggers(p) == 1);
   d = tree_trigger(p, 0);
   fail_unless(tree_kind(d) == T_REF);
   fail_unless(tree_ident(d) == ident_new("X"));

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
   fail_unless(tree_stmts(a) == 4);

   // Wait statements

   p = tree_stmt(a, 0);
   fail_unless(tree_kind(p) == T_PROCESS);
   fail_unless(tree_stmts(p) == 5);

   s = tree_stmt(p, 0);
   fail_unless(tree_kind(s) == T_WAIT);
   fail_unless(tree_has_delay(s));
   e = tree_delay(s);
   fail_unless(tree_kind(e) == T_FCALL);
   fail_unless(tree_ident(e) == ident_new("*"));
   fail_unless(tree_params(e) == 2);
   fail_unless(tree_kind(tree_param(e, 0).value) == T_LITERAL);
   fail_unless(tree_kind(tree_param(e, 1).value) == T_REF);
   fail_unless(tree_ident(tree_param(e, 1).value) == ident_new("NS"));

   s = tree_stmt(p, 1);
   fail_unless(tree_kind(s) == T_WAIT);
   fail_if(tree_has_delay(s));
   fail_unless(tree_ident(s) == ident_new("BLOCK_FOREVER"));

   s = tree_stmt(p, 2);
   fail_unless(tree_kind(s) == T_WAIT);
   fail_if(tree_has_delay(s));
   fail_unless(tree_triggers(s) == 1);
   e = tree_trigger(s, 0);
   fail_unless(tree_kind(e) == T_REF);

   s = tree_stmt(p, 3);
   fail_unless(tree_kind(s) == T_WAIT);
   fail_if(tree_has_delay(s));
   fail_unless(tree_triggers(s) == 3);
   e = tree_trigger(s, 2);
   fail_unless(tree_kind(e) == T_ARRAY_SLICE);

   s = tree_stmt(p, 4);
   fail_unless(tree_kind(s) == T_WAIT);
   fail_unless(tree_has_delay(s));

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
   fail_unless(tree_kind(tree_param(e, 0).value) == T_REF);
   e = tree_param(e, 1).value;
   fail_unless(tree_kind(e) == T_FCALL);
   fail_unless(tree_ident(e) == ident_new("*"));
   fail_unless(tree_params(e) == 2);
   fail_unless(tree_kind(tree_param(e, 0).value) == T_REF);
   fail_unless(tree_kind(tree_param(e, 1).value) == T_LITERAL);

   // Assert and report

   p = tree_stmt(a, 2);
   fail_unless(tree_kind(p) == T_PROCESS);
   fail_unless(tree_stmts(p) == 5);

   s = tree_stmt(p, 0);
   fail_unless(tree_kind(s) == T_ASSERT);
   fail_unless(tree_kind(tree_value(s)) == T_REF);
   fail_unless(tree_ident(tree_value(s)) == ident_new("TRUE"));
   fail_unless(tree_kind(tree_severity(s)) == T_REF);
   fail_unless(tree_ident(tree_severity(s)) == ident_new("ERROR"));
   fail_unless(tree_kind(tree_message(s)) == T_AGGREGATE);

   s = tree_stmt(p, 3);
   fail_unless(tree_kind(s) == T_ASSERT);
   fail_unless(tree_kind(tree_value(s)) == T_REF);
   fail_unless(tree_ident(tree_value(s)) == ident_new("FALSE"));
   fail_unless(tree_kind(tree_severity(s)) == T_REF);
   fail_unless(tree_ident(tree_severity(s)) == ident_new("NOTE"));
   fail_unless(tree_kind(tree_message(s)) == T_AGGREGATE);
   fail_unless(tree_attr_int(s, ident_new("is_report"), 0) == 1);

   // Function calls

   p = tree_stmt(a, 3);
   fail_unless(tree_kind(p) == T_PROCESS);
   fail_unless(tree_stmts(p) == 1);

   s = tree_stmt(p, 0);
   fail_unless(tree_kind(s) == T_VAR_ASSIGN);
   e = tree_value(s);
   fail_unless(tree_kind(e) == T_FCALL);
   fail_unless(tree_params(e) == 3);
   fail_unless(tree_param(e, 0).kind == P_POS);
   fail_unless(tree_param(e, 0).pos == 0);
   fail_unless(tree_literal(tree_param(e, 0).value).i == 1);
   fail_unless(tree_param(e, 1).kind == P_POS);
   fail_unless(tree_param(e, 1).pos == 1);
   fail_unless(tree_literal(tree_param(e, 1).value).i == 2);
   fail_unless(tree_param(e, 2).kind == P_POS);
   fail_unless(tree_param(e, 2).pos == 2);
   fail_unless(tree_literal(tree_param(e, 2).value).i == 3);

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
   fail_unless(tree_decls(a) == 7);

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

   d = tree_decl(a, 4);
   fail_unless(tree_kind(d) == T_TYPE_DECL);
   fail_unless(tree_ident(d) == ident_new("BIG_R"));
   t = tree_type(d);
   fail_unless(type_kind(t) == T_SUBTYPE);
   fail_unless(type_kind(type_base(t)) == T_UNRESOLVED);
   fail_unless(type_ident(type_base(t)) == ident_new("RESISTANCE"));

   d = tree_decl(a, 5);
   fail_unless(tree_kind(d) == T_TYPE_DECL);
   fail_unless(tree_ident(d) == ident_new("MY_SMALL_INT"));
   t = tree_type(d);
   fail_unless(type_kind(t) == T_SUBTYPE);
   fail_unless(type_kind(type_base(t)) == T_UNRESOLVED);
   fail_unless(type_ident(type_base(t)) == ident_new("MY_INT"));

   d = tree_decl(a, 6);
   fail_unless(tree_kind(d) == T_TYPE_DECL);
   fail_unless(tree_ident(d) == ident_new("FOO"));
   t = tree_type(d);
   fail_unless(type_kind(t) == T_SUBTYPE);
   fail_unless(type_kind(type_base(t)) == T_UNRESOLVED);
   fail_unless(type_ident(type_base(t)) == ident_new("MY_INT"));
   r = type_dim(t, 0);
   fail_unless(tree_kind(r.left) == T_LITERAL);
   fail_unless(tree_kind(r.right) == T_ATTR_REF);
   fail_unless(tree_ident(r.right) == ident_new("MY_INT"));
   fail_unless(tree_ident2(r.right) == ident_new("HIGH"));

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
   fail_unless(tree_decls(a) == 3);

   d = tree_decl(a, 0);
   fail_unless(tree_ident(d) == ident_new("POS"));
   v = tree_value(d);
   fail_unless(tree_kind(v) == T_LITERAL);
   l = tree_literal(v);
   fail_unless(l.kind == L_INT);
   fail_unless(l.i == 64);

   d = tree_decl(a, 1);
   fail_unless(tree_ident(d) == ident_new("NEG"));
   v = tree_value(d);
   fail_unless(tree_kind(v) == T_FCALL);
   fail_unless(tree_ident(v) == ident_new("-"));
   fail_unless(tree_params(v) == 1);
   l = tree_literal(tree_param(v, 0).value);
   fail_unless(l.kind == L_INT);
   fail_unless(l.i == 265);

   d = tree_decl(a, 2);
   fail_unless(tree_kind(d) == T_CONST_DECL);
   fail_unless(tree_ident(d) == ident_new("C"));
   v = tree_value(d);
   fail_unless(tree_kind(v) == T_LITERAL);
   l = tree_literal(v);
   fail_unless(l.kind == L_INT);
   fail_unless(l.i == 523);

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
   fail_unless(tree_ident(p) == ident_new("ONE"));

   p = parse();
   fail_if(p == NULL);
   fail_unless(tree_kind(p) == T_PACKAGE);
   fail_unless(tree_decls(p) == 1);
   fail_unless(tree_contexts(p) == 1);
   fail_unless(tree_context(p, 0) == ident_new("WORK.ONE.all"));
   fail_unless(tree_ident(p) == ident_new("TWO"));

   p = parse();
   fail_unless(p == NULL);

   fail_unless(parse_errors() == 0);
}
END_TEST

START_TEST(test_enum)
{
   tree_t p, d, i;
   type_t t;

   fail_unless(input_from_file(TESTDIR "/parse/enum.vhd"));

   p = parse();
   fail_if(p == NULL);
   fail_unless(tree_kind(p) == T_PACKAGE);
   fail_unless(tree_decls(p) == 3);

   d = tree_decl(p, 0);
   fail_unless(tree_kind(d) == T_TYPE_DECL);
   fail_unless(tree_ident(d) == ident_new("A"));
   t = tree_type(d);
   fail_unless(type_kind(t) == T_ENUM);
   fail_unless(type_enum_literals(t) == 3);
   i = type_enum_literal(t, 0);
   fail_unless(tree_kind(i) == T_ENUM_LIT);
   fail_unless(tree_ident(i) == ident_new("X"));
   fail_unless(tree_pos(i) == 0);
   i = type_enum_literal(t, 1);
   fail_unless(tree_kind(i) == T_ENUM_LIT);
   fail_unless(tree_ident(i) == ident_new("Y"));
   fail_unless(tree_pos(i) == 1);
   i = type_enum_literal(t, 2);
   fail_unless(tree_kind(i) == T_ENUM_LIT);
   fail_unless(tree_ident(i) == ident_new("Z"));
   fail_unless(tree_pos(i) == 2);

   d = tree_decl(p, 1);
   fail_unless(tree_kind(d) == T_TYPE_DECL);
   fail_unless(tree_ident(d) == ident_new("B"));
   t = tree_type(d);
   fail_unless(type_kind(t) == T_ENUM);
   fail_unless(type_enum_literals(t) == 3);
   i = type_enum_literal(t, 0);
   fail_unless(tree_kind(i) == T_ENUM_LIT);
   fail_unless(tree_ident(i) == ident_new("'x'"));
   i = type_enum_literal(t, 1);
   fail_unless(tree_kind(i) == T_ENUM_LIT);
   fail_unless(tree_ident(i) == ident_new("'y'"));
   i = type_enum_literal(t, 2);
   fail_unless(tree_kind(i) == T_ENUM_LIT);
   fail_unless(tree_ident(i) == ident_new("Z"));

   d = tree_decl(p, 2);
   fail_unless(tree_kind(d) == T_TYPE_DECL);
   fail_unless(tree_ident(d) == ident_new("C"));
   t = tree_type(d);
   fail_unless(type_kind(t) == T_ENUM);
   fail_unless(type_enum_literals(t) == 1);
   i = type_enum_literal(t, 0);
   fail_unless(tree_kind(i) == T_ENUM_LIT);
   fail_unless(tree_ident(i) == ident_new("FOO"));
   fail_unless(tree_pos(i) == 0);

   p = parse();
   fail_unless(p == NULL);

   fail_unless(parse_errors() == 0);
}
END_TEST

START_TEST(test_qual)
{
   tree_t a, p, s, q, e;

   fail_unless(input_from_file(TESTDIR "/parse/qual.vhd"));

   a = parse();
   fail_if(a == NULL);
   fail_unless(tree_kind(a) == T_ARCH);
   fail_unless(tree_stmts(a) == 1);

   p = tree_stmt(a, 0);
   fail_unless(tree_stmts(p) == 2);

   s = tree_stmt(p, 0);
   fail_unless(tree_kind(s) == T_SIGNAL_ASSIGN);
   q = tree_value(s);
   fail_unless(tree_kind(q) == T_QUALIFIED);
   fail_unless(tree_ident(q) == ident_new("FOO"));
   e = tree_value(q);
   fail_unless(tree_kind(e) == T_REF);
   fail_unless(tree_ident(e) == ident_new("B"));

   s = tree_stmt(p, 1);
   fail_unless(tree_kind(s) == T_SIGNAL_ASSIGN);
   q = tree_value(s);
   fail_unless(tree_kind(q) == T_QUALIFIED);
   fail_unless(tree_ident(q) == ident_new("FOO"));
   e = tree_value(q);
   fail_unless(tree_kind(e) == T_REF);
   fail_unless(tree_ident(e) == ident_new("'c'"));

   a = parse();
   fail_unless(a == NULL);

   fail_unless(parse_errors() == 0);
}
END_TEST

START_TEST(test_func)
{
   tree_t p, f, a;
   type_t t;

   fail_unless(input_from_file(TESTDIR "/parse/func.vhd"));

   p = parse();
   fail_if(p == NULL);
   fail_unless(tree_kind(p) == T_PACKAGE);
   fail_unless(tree_decls(p) == 2);

   f = tree_decl(p, 0);
   fail_unless(tree_kind(f) == T_FUNC_DECL);
   fail_unless(tree_ident(f) == ident_new("ADD"));
   fail_unless(tree_ports(f) == 3);
   a = tree_port(f, 0);
   fail_unless(tree_kind(a) == T_PORT_DECL);
   fail_unless(tree_ident(a) == ident_new("X"));
   fail_unless(tree_port_mode(a) == PORT_IN);
   t = tree_type(f);
   fail_unless(type_kind(t) == T_FUNC);

   f = tree_decl(p, 1);
   fail_unless(tree_kind(f) == T_FUNC_DECL);
   fail_unless(tree_ident(f) == ident_new("NAUGHTY"));
   fail_unless(tree_ports(f) == 0);

   p = parse();
   fail_unless(p == NULL);

   fail_unless(parse_errors() == 0);
}
END_TEST

START_TEST(test_array)
{
   tree_t p, d, a, g, s, e;
   type_t t, i, b;
   range_t r;
   assoc_t x;

   fail_unless(input_from_file(TESTDIR "/parse/array.vhd"));

   p = parse();
   fail_if(p == NULL);
   fail_unless(tree_kind(p) == T_PACKAGE);
   fail_unless(tree_decls(p) == 2);

   d = tree_decl(p, 0);
   fail_unless(tree_kind(d) == T_TYPE_DECL);
   fail_unless(tree_ident(d) == ident_new("INT_ARRAY"));
   t = tree_type(d);
   fail_unless(type_kind(t) == T_UARRAY);
   fail_unless(type_index_constrs(t) == 1);
   i = type_index_constr(t, 0);
   fail_unless(type_kind(i) == T_UNRESOLVED);
   fail_unless(type_ident(i) == ident_new("INTEGER"));
   b = type_base(t);
   fail_unless(type_kind(b) == T_UNRESOLVED);
   fail_unless(type_ident(b) == ident_new("INTEGER"));

   d = tree_decl(p, 1);
   fail_unless(tree_kind(d) == T_TYPE_DECL);
   fail_unless(tree_ident(d) == ident_new("TEN_INTS"));
   t = tree_type(d);
   fail_unless(type_kind(t) == T_CARRAY);
   fail_unless(type_dims(t) == 1);
   r = type_dim(t, 0);

   a = parse();
   fail_if(a == NULL);
   fail_unless(tree_kind(a) == T_ARCH);
   fail_unless(tree_decls(a) == 5);

   d = tree_decl(a, 0);
   fail_unless(tree_ident(d) == ident_new("X"));
   t = tree_type(d);
   fail_unless(type_kind(t) == T_SUBTYPE);
   fail_unless(type_dims(t) == 1);
   fail_unless(tree_literal(type_dim(t, 0).left).i == 1);
   fail_unless(tree_literal(type_dim(t, 0).right).i == 5);

   d = tree_decl(a, 1);
   fail_unless(tree_ident(d) == ident_new("Y"));
   t = tree_type(d);
   fail_unless(type_kind(t) == T_UNRESOLVED);
   fail_unless(type_ident(t) == ident_new("TEN_INTS"));

   d = tree_decl(a, 2);
   fail_unless(tree_ident(d) == ident_new("Z"));
   t = tree_type(d);
   fail_unless(type_kind(t) == T_SUBTYPE);
   fail_unless(type_dims(t) == 1);
   fail_unless(tree_literal(type_dim(t, 0).left).i == 1);
   fail_unless(tree_literal(type_dim(t, 0).right).i == 3);
   fail_unless(tree_has_value(d));
   g = tree_value(d);
   fail_unless(tree_kind(g) == T_AGGREGATE);
   fail_unless(tree_assocs(g) == 3);
   for (int i = 0; i < 3; i++) {
      x = tree_assoc(g, i);
      fail_unless(x.kind == A_POS);
      fail_unless(x.pos == i);
      fail_unless(tree_literal(x.value).i == i);
   }

   d = tree_decl(a, 3);
   fail_unless(tree_ident(d) == ident_new("N"));
   t = tree_type(d);
   fail_unless(type_kind(t) == T_SUBTYPE);
   fail_unless(type_dims(t) == 1);
   fail_unless(tree_literal(type_dim(t, 0).left).i == 1);
   fail_unless(tree_literal(type_dim(t, 0).right).i == 3);
   fail_unless(tree_has_value(d));
   g = tree_value(d);
   fail_unless(tree_kind(g) == T_AGGREGATE);
   fail_unless(tree_assocs(g) == 3);
   x = tree_assoc(g, 0);
   fail_unless(x.kind == A_POS);
   fail_unless(x.pos == 0);
   fail_unless(tree_literal(x.value).i == 0);
   x = tree_assoc(g, 1);
   fail_unless(x.kind == A_NAMED);
   fail_unless(tree_kind(x.name) == T_LITERAL);
   fail_unless(tree_literal(x.name).i == 1);
   fail_unless(tree_literal(x.value).i == 1);
   x = tree_assoc(g, 2);
   fail_unless(x.kind == A_OTHERS);
   fail_unless(tree_literal(x.value).i == 2);

   d = tree_decl(a, 4);
   fail_unless(tree_ident(d) == ident_new("M"));
   t = tree_type(d);
   fail_unless(type_kind(t) == T_SUBTYPE);
   fail_unless(type_dims(t) == 1);
   fail_unless(tree_literal(type_dim(t, 0).left).i == 1);
   fail_unless(tree_literal(type_dim(t, 0).right).i == 3);
   fail_unless(tree_has_value(d));
   g = tree_value(d);
   fail_unless(tree_kind(g) == T_AGGREGATE);
   fail_unless(tree_assocs(g) == 1);
   x = tree_assoc(g, 0);
   fail_unless(x.kind == A_RANGE);
   fail_unless(tree_literal(x.range.left).i == 1);
   fail_unless(tree_literal(x.range.right).i == 3);
   fail_unless(tree_literal(x.value).i == 0);

   p = tree_stmt(a, 0);
   fail_unless(tree_kind(p) == T_PROCESS);
   s = tree_stmt(p, 0);
   e = tree_target(s);
   fail_unless(tree_kind(e) == T_ARRAY_REF);
   fail_unless(tree_params(e) == 1);
   fail_unless(tree_param(e, 0).kind == P_POS);
   fail_unless(tree_param(e, 0).pos == 0);
   fail_unless(tree_literal(tree_param(e, 0).value).i == 0);
   s = tree_stmt(p, 1);
   e = tree_value(s);
   fail_unless(tree_kind(e) == T_FCALL);
   s = tree_stmt(p, 3);
   fail_unless(tree_kind(s) == T_SIGNAL_ASSIGN);
   e = tree_target(s);
   fail_unless(tree_kind(e) == T_ARRAY_SLICE);
   e = tree_value(s);
   fail_unless(tree_kind(e) == T_ARRAY_SLICE);

   p = parse();
   fail_unless(p == NULL);

   fail_unless(parse_errors() == 0);
}
END_TEST

START_TEST(test_instance)
{
   tree_t a, s;

   fail_unless(input_from_file(TESTDIR "/parse/instance.vhd"));

   a = parse();
   fail_if(a == NULL);
   fail_unless(tree_kind(a) == T_ARCH);
   fail_unless(tree_stmts(a) == 8);

   s = tree_stmt(a, 0);
   fail_unless(tree_kind(s) == T_INSTANCE);
   fail_unless(tree_ident(s) == ident_new("A"));
   fail_unless(tree_ident2(s) == ident_new("FOO"));

   s = tree_stmt(a, 2);
   fail_unless(tree_kind(s) == T_INSTANCE);
   fail_unless(tree_ident(s) == ident_new("B1"));
   fail_unless(tree_ident2(s) == ident_new("WORK.FOO-GOO"));

   s = tree_stmt(a, 5);
   fail_unless(tree_kind(s) == T_INSTANCE);
   fail_unless(tree_ident(s) == ident_new("E"));
   fail_unless(tree_ident2(s) == ident_new("WORK.FOO"));
   fail_unless(tree_params(s) == 3);

   s = tree_stmt(a, 7);
   fail_unless(tree_kind(s) == T_INSTANCE);
   fail_unless(tree_ident(s) == ident_new("G"));
   fail_unless(tree_ident2(s) == ident_new("WORK.FOO"));
   fail_unless(tree_params(s) == 2);
   fail_unless(tree_genmaps(s) == 1);

   a = parse();
   fail_unless(a == NULL);

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
   tcase_add_test(tc_core, test_enum);
   tcase_add_test(tc_core, test_qual);
   tcase_add_test(tc_core, test_func);
   tcase_add_test(tc_core, test_array);
   tcase_add_test(tc_core, test_instance);
   suite_add_tcase(s, tc_core);

   SRunner *sr = srunner_create(s);
   srunner_run_all(sr, CK_NORMAL);

   int nfail = srunner_ntests_failed(sr);

   srunner_free(sr);

   return nfail == 0 ? EXIT_SUCCESS : EXIT_FAILURE;
}
