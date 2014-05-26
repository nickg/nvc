#include "type.h"
#include "util.h"
#include "tree.h"
#include "phase.h"

#include <check.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

START_TEST(test_entity)
{
   tree_t e, p, g, v, x, y;
   type_t t;

   input_from_file(TESTDIR "/parse/entity.vhd");

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
   fail_unless(tree_subkind(p) == PORT_IN);
   fail_unless(tree_class(p) == C_SIGNAL);
   t = tree_type(p);
   fail_unless(type_kind(t) == T_UNRESOLVED);
   fail_unless(type_ident(t) == ident_new("INTEGER"));
   fail_unless(tree_has_value(p));
   v = tree_value(p);
   fail_unless(tree_kind(v) == T_LITERAL);
   fail_unless(tree_subkind(v) == L_INT);
   fail_unless(tree_ival(v) == 4);

   p = tree_port(e, 1);
   fail_unless(tree_kind(p) == T_PORT_DECL);
   fail_unless(tree_ident(p) == ident_new("B"));
   fail_unless(tree_subkind(p) == PORT_OUT);
   t = tree_type(p);
   fail_unless(type_kind(t) == T_UNRESOLVED);
   fail_unless(type_ident(t) == ident_new("BIT"));
   fail_if(tree_has_value(p));

   p = tree_port(e, 2);
   fail_unless(tree_kind(p) == T_PORT_DECL);
   fail_unless(tree_ident(p) == ident_new("BEE"));
   fail_unless(tree_subkind(p) == PORT_OUT);
   t = tree_type(p);
   fail_unless(type_kind(t) == T_UNRESOLVED);
   fail_unless(type_ident(t) == ident_new("BIT"));
   fail_if(tree_has_value(p));

   p = tree_port(e, 3);
   fail_unless(tree_kind(p) == T_PORT_DECL);
   fail_unless(tree_ident(p) == ident_new("C"));
   fail_unless(tree_subkind(p) == PORT_INOUT);
   t = tree_type(p);
   fail_unless(type_kind(t) == T_UNRESOLVED);
   fail_unless(type_ident(t) == ident_new("INTEGER"));
   fail_if(tree_has_value(p));

   p = tree_port(e, 4);
   fail_unless(tree_kind(p) == T_PORT_DECL);
   fail_unless(tree_ident(p) == ident_new("D"));
   fail_unless(tree_subkind(p) == PORT_BUFFER);
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
   fail_unless(tree_ident(v) == ident_new("\"*\""));
   fail_unless(tree_params(v) == 2);
   x = tree_param(v, 0);
   fail_unless(tree_subkind(x) == P_POS);
   fail_unless(tree_pos(x) == 0);
   fail_unless(tree_kind(tree_value(x)) == T_LITERAL);
   fail_unless(tree_subkind(tree_value(x)) == L_INT);
   fail_unless(tree_ival(tree_value(x)) == 2);
   y = tree_param(v, 1);
   fail_unless(tree_subkind(y) == P_POS);
   fail_unless(tree_pos(y) == 1);
   fail_unless(tree_kind(tree_value(y)) == T_LITERAL);
   fail_unless(tree_subkind(tree_value(y)) == L_INT);
   fail_unless(tree_ival(tree_value(y)) == 5);

   fail_unless(tree_ports(e) == 1);

   p = tree_port(e, 0);
   fail_unless(tree_kind(p) == T_PORT_DECL);
   fail_unless(tree_ident(p) == ident_new("P"));
   fail_unless(tree_subkind(p) == PORT_OUT);
   fail_unless(tree_class(p) == C_SIGNAL);
   fail_if(tree_has_value(p));

   e = parse();
   fail_if(e == NULL);
   fail_unless(tree_kind(e) == T_ENTITY);
   fail_unless(tree_decls(e) == 2);
   fail_unless(tree_kind(tree_decl(e, 0)) == T_ATTR_DECL);
   fail_unless(tree_kind(tree_decl(e, 1)) == T_ATTR_SPEC);

   e = parse();
   fail_if(e == NULL);
   fail_unless(tree_kind(e) == T_ENTITY);
   fail_unless(tree_stmts(e) == 2);
   fail_unless(tree_kind(tree_stmt(e, 0)) == T_CASSERT);
   fail_unless(tree_kind(tree_stmt(e, 1)) == T_CASSERT);

   e = parse();
   fail_unless(e == NULL);

   fail_unless(parse_errors() == 0);
}
END_TEST

START_TEST(test_arch)
{
   tree_t a, d, v;

   input_from_file(TESTDIR "/parse/arch.vhd");

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
   fail_unless(tree_subkind(v) == L_INT);
   fail_unless(tree_ival(v) == 7);
   d = tree_decl(a, 2);
   fail_unless(tree_kind(d) == T_SIGNAL_DECL);
   fail_unless(tree_ident(d) == ident_new("Z"));
   fail_unless(type_kind(tree_type(d)) == T_UNRESOLVED);
   fail_unless(type_ident(tree_type(d)) == ident_new("INTEGER"));
   fail_unless(tree_has_value(d));
   v = tree_value(d);
   fail_unless(tree_kind(v) == T_LITERAL);
   fail_unless(tree_subkind(v) == L_INT);
   fail_unless(tree_ival(v) == 7);

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

   input_from_file(TESTDIR "/parse/process.vhd");

   a = parse();
   fail_if(a == NULL);
   fail_unless(tree_kind(a) == T_ARCH);
   fail_unless(tree_stmts(a) == 4);

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
   fail_if(tree_attr_int(p, ident_new("postponed"), 0));
   d = tree_trigger(p, 0);
   fail_unless(tree_kind(d) == T_REF);
   fail_unless(tree_ident(d) == ident_new("X"));

   p = tree_stmt(a, 3);
   fail_unless(tree_kind(p) == T_PROCESS);
   fail_unless(tree_attr_int(p, ident_new("postponed"), 0));

   a = parse();
   fail_unless(a == NULL);

   fail_unless(parse_errors() == 0);
}
END_TEST

START_TEST(test_seq)
{
   tree_t a, p, s, e, b;

   input_from_file(TESTDIR "/parse/seq.vhd");

   a = parse();
   fail_if(a == NULL);
   fail_unless(tree_kind(a) == T_ARCH);
   fail_unless(tree_stmts(a) == 15);

   // Wait statements

   p = tree_stmt(a, 0);
   fail_unless(tree_kind(p) == T_PROCESS);
   fail_unless(tree_stmts(p) == 8);

   s = tree_stmt(p, 0);
   fail_unless(tree_kind(s) == T_WAIT);
   fail_unless(tree_has_delay(s));
   e = tree_delay(s);
   fail_unless(tree_kind(e) == T_FCALL);
   fail_unless(tree_ident(e) == ident_new("\"*\""));
   fail_unless(tree_params(e) == 2);
   fail_unless(tree_kind(tree_value(tree_param(e, 0))) == T_LITERAL);
   fail_unless(tree_kind(tree_value(tree_param(e, 1))) == T_REF);
   fail_unless(tree_ident(tree_value(tree_param(e, 1))) == ident_new("NS"));

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
   fail_if(tree_has_value(s));

   s = tree_stmt(p, 5);
   fail_unless(tree_kind(s) == T_WAIT);
   fail_if(tree_has_delay(s));
   fail_unless(tree_has_value(s));

   s = tree_stmt(p, 6);
   fail_unless(tree_kind(s) == T_WAIT);
   fail_unless(tree_has_delay(s));
   fail_unless(tree_has_value(s));

   s = tree_stmt(p, 7);
   fail_unless(tree_kind(s) == T_WAIT);
   fail_unless(tree_has_delay(s));
   fail_unless(tree_has_value(s));
   fail_unless(tree_triggers(s) == 1);

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
   fail_unless(tree_ident(e) == ident_new("\"+\""));
   fail_unless(tree_params(e) == 2);
   fail_unless(tree_kind(tree_value(tree_param(e, 0))) == T_REF);
   e = tree_value(tree_param(e, 1));
   fail_unless(tree_kind(e) == T_FCALL);
   fail_unless(tree_ident(e) == ident_new("\"*\""));
   fail_unless(tree_params(e) == 2);
   fail_unless(tree_kind(tree_value(tree_param(e, 0))) == T_REF);
   fail_unless(tree_kind(tree_value(tree_param(e, 1))) == T_LITERAL);

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
   fail_unless(tree_stmts(p) == 2);

   s = tree_stmt(p, 0);
   fail_unless(tree_kind(s) == T_VAR_ASSIGN);
   e = tree_value(s);
   fail_unless(tree_kind(e) == T_FCALL);
   fail_unless(tree_params(e) == 3);
   fail_unless(tree_subkind(tree_param(e, 0)) == P_POS);
   fail_unless(tree_pos(tree_param(e, 0)) == 0);
   fail_unless(tree_ival(tree_value(tree_param(e, 0))) == 1);
   fail_unless(tree_subkind(tree_param(e, 1)) == P_POS);
   fail_unless(tree_pos(tree_param(e, 1)) == 1);
   fail_unless(tree_ival(tree_value(tree_param(e, 1))) == 2);
   fail_unless(tree_subkind(tree_param(e, 2)) == P_POS);
   fail_unless(tree_pos(tree_param(e, 2)) == 2);
   fail_unless(tree_ival(tree_value(tree_param(e, 2))) == 3);

   s = tree_stmt(p, 1);
   fail_unless(tree_kind(s) == T_VAR_ASSIGN);
   e = tree_value(s);
   fail_unless(tree_kind(e) == T_FCALL);
   fail_unless(tree_ident(e) == ident_new("\"abs\""));

   // If statements

   p = tree_stmt(a, 4);
   fail_unless(tree_kind(p) == T_PROCESS);
   fail_unless(tree_stmts(p) == 4);

   s = tree_stmt(p, 0);
   fail_unless(tree_kind(s) == T_IF);
   e = tree_value(s);
   fail_unless(tree_kind(e) == T_REF);
   fail_unless(tree_ident(e) == ident_new("TRUE"));
   fail_unless(tree_stmts(s) == 1);

   s = tree_stmt(p, 2);
   fail_unless(tree_kind(s) == T_IF);
   fail_unless(tree_stmts(s) == 1);
   fail_unless(tree_else_stmts(s) == 1);

   s = tree_stmt(p, 3);
   fail_unless(tree_kind(s) == T_IF);
   fail_unless(tree_stmts(s) == 1);
   fail_unless(tree_else_stmts(s) == 1);
   s = tree_else_stmt(s, 0);
   fail_unless(tree_kind(s) == T_IF);
   fail_unless(tree_stmts(s) == 1);
   fail_unless(tree_else_stmts(s) == 1);
   s = tree_else_stmt(s, 0);
   fail_unless(tree_kind(s) == T_IF);
   fail_unless(tree_stmts(s) == 1);
   fail_unless(tree_else_stmts(s) == 1);
   s = tree_else_stmt(s, 0);
   fail_unless(tree_kind(s) == T_VAR_ASSIGN);

   // Null statements

   p = tree_stmt(a, 5);
   fail_unless(tree_kind(p) == T_PROCESS);
   fail_unless(tree_stmts(p) == 1);

   s = tree_stmt(p, 0);
   fail_unless(tree_kind(s) == T_NULL);

   // Return statements

   p = tree_stmt(a, 6);
   fail_unless(tree_kind(p) == T_PROCESS);
   fail_unless(tree_stmts(p) == 1);

   s = tree_stmt(p, 0);
   fail_unless(tree_kind(s) == T_RETURN);
   fail_unless(tree_kind(tree_value(s)) == T_FCALL);

   // While loops

   p = tree_stmt(a, 7);
   fail_unless(tree_kind(p) == T_PROCESS);
   fail_unless(tree_stmts(p) == 2);

   s = tree_stmt(p, 0);
   fail_unless(tree_kind(s) == T_WHILE);
   fail_unless(tree_stmts(s) == 1);
   fail_unless(tree_kind(tree_value(s)) == T_FCALL);

   s = tree_stmt(p, 1);
   fail_unless(tree_kind(s) == T_WHILE);
   fail_unless(tree_stmts(s) == 1);
   fail_unless(tree_kind(tree_value(s)) == T_REF);
   fail_unless(tree_ident(tree_value(s)) == ident_new("TRUE"));

   // Multiple waveforms in signal assignment

   p = tree_stmt(a, 8);

   s = tree_stmt(p, 0);
   fail_unless(tree_kind(s) == T_SIGNAL_ASSIGN);
   fail_unless(tree_waveforms(s) == 1);
   fail_unless(tree_has_delay(tree_waveform(s, 0)));

   s = tree_stmt(p, 1);
   fail_unless(tree_kind(s) == T_SIGNAL_ASSIGN);
   fail_unless(tree_waveforms(s) == 2);
   fail_unless(tree_has_delay(tree_waveform(s, 0)));
   fail_unless(tree_has_delay(tree_waveform(s, 1)));

   s = tree_stmt(p, 2);
   fail_unless(tree_kind(s) == T_SIGNAL_ASSIGN);
   fail_unless(tree_waveforms(s) == 2);
   fail_if(tree_has_delay(tree_waveform(s, 0)));
   fail_unless(tree_has_delay(tree_waveform(s, 1)));

   // For

   p = tree_stmt(a, 9);

   s = tree_stmt(p, 0);
   fail_unless(tree_kind(s) == T_FOR);
   fail_unless(tree_stmts(s) == 1);
   fail_unless(tree_range(s).kind == RANGE_TO);
   fail_unless(tree_kind(tree_range(s).left) == T_LITERAL);
   fail_unless(tree_kind(tree_range(s).right) == T_LITERAL);

   s = tree_stmt(p, 1);
   fail_unless(tree_kind(s) == T_FOR);
   fail_unless(tree_stmts(s) == 1);
   fail_unless(tree_range(s).kind == RANGE_EXPR);
   fail_unless(tree_kind(tree_range(s).left) == T_ATTR_REF);
   fail_unless(tree_range(s).right == NULL);

   // Exit

   p = tree_stmt(a, 10);

   s = tree_stmt(p, 0);
   fail_unless(tree_kind(s) == T_EXIT);
   fail_if(tree_has_value(s));

   s = tree_stmt(p, 1);
   fail_unless(tree_kind(s) == T_EXIT);
   fail_unless(tree_has_value(s));

   // Procedure call

   p = tree_stmt(a, 11);

   s = tree_stmt(p, 0);
   fail_unless(tree_kind(s) == T_PCALL);
   fail_unless(tree_ident2(s) == ident_new("FOO"));
   fail_unless(tree_params(s) == 3);

   s = tree_stmt(p, 1);
   fail_unless(tree_kind(s) == T_PCALL);
   fail_unless(tree_ident2(s) == ident_new("BAR"));
   fail_unless(tree_params(s) == 0);

   // Case

   p = tree_stmt(a, 12);

   s = tree_stmt(p, 0);
   fail_unless(tree_kind(s) == T_CASE);
   fail_unless(tree_assocs(s) == 4 + 1);
   fail_unless(tree_subkind(tree_assoc(s, 0)) == A_NAMED);
   fail_unless(tree_subkind(tree_assoc(s, 4)) == A_OTHERS);
   b = tree_value(tree_assoc(s, 0));
   fail_unless(tree_kind(b) == T_BLOCK);
   fail_unless(tree_stmts(b) == 1);
   fail_unless(tree_kind(tree_stmt(b, 0)) == T_NULL);

   // Next

   p = tree_stmt(a, 13);

   s = tree_stmt(p, 0);
   fail_unless(tree_kind(s) == T_NEXT);
   fail_if(tree_has_value(s));

   s = tree_stmt(p, 1);
   fail_unless(tree_kind(s) == T_NEXT);
   fail_unless(tree_has_value(s));

   // Signal assignment to aggregate

   p = tree_stmt(a, 14);

   s = tree_stmt(p, 0);
   fail_unless(tree_kind(s) == T_SIGNAL_ASSIGN);
   fail_unless(tree_kind(tree_target(s)) == T_AGGREGATE);

   a = parse();
   fail_unless(a == NULL);

   fail_unless(parse_errors() == 0);
}
END_TEST

START_TEST(test_types)
{
   tree_t a, d, f, u;
   type_t t;
   range_t r;

   input_from_file(TESTDIR "/parse/types.vhd");

   a = parse();
   fail_if(a == NULL);
   fail_unless(tree_kind(a) == T_ARCH);
   fail_unless(tree_decls(a) == 17);

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
   fail_unless(tree_kind(r.left) == T_FCALL);
   fail_unless(tree_kind(r.right) == T_FCALL);
   fail_unless(type_units(t) == 3);
   u = type_unit(t, 0);
   fail_unless(tree_ident(u) == ident_new("OHM"));
   fail_unless(tree_kind(tree_value(u)) == T_LITERAL);
   u = type_unit(t, 1);
   fail_unless(tree_ident(u) == ident_new("KOHM"));
   fail_unless(tree_kind(tree_value(u)) == T_FCALL);
   u = type_unit(t, 2);
   fail_unless(tree_ident(u) == ident_new("MOHM"));
   fail_unless(tree_kind(tree_value(u)) == T_FCALL);

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
   fail_unless(tree_ident(tree_name(r.right)) == ident_new("MY_INT"));
   fail_unless(tree_ident(r.right) == ident_new("HIGH"));

   d = tree_decl(a, 7);
   fail_unless(tree_kind(d) == T_TYPE_DECL);
   fail_unless(tree_ident(d) == ident_new("RINT"));
   t = tree_type(d);
   fail_unless(type_kind(t) == T_SUBTYPE);
   fail_unless(type_has_resolution(t));
   f = type_resolution(t);
   fail_unless(tree_kind(f) == T_REF);
   fail_unless(tree_ident(f) == ident_new("RESOLVED"));

   d = tree_decl(a, 8);
   fail_unless(tree_kind(d) == T_TYPE_DECL);
   fail_unless(tree_ident(d) == ident_new("P"));
   t = tree_type(d);
   fail_unless(type_kind(t) == T_ACCESS);
   fail_unless(type_kind(type_access(t)) == T_UNRESOLVED);
   fail_unless(type_ident(type_access(t)) == ident_new("MY_INT"));

   d = tree_decl(a, 9);
   fail_unless(tree_kind(d) == T_TYPE_DECL);
   fail_unless(tree_ident(d) == ident_new("F"));
   t = tree_type(d);
   fail_unless(type_kind(t) == T_FILE);
   fail_unless(type_kind(type_file(t)) == T_UNRESOLVED);
   fail_unless(type_ident(type_file(t)) == ident_new("MY_INT"));

   d = tree_decl(a, 10);
   fail_unless(tree_kind(d) == T_FILE_DECL);
   fail_unless(tree_ident(d) == ident_new("F1"));
   fail_unless(tree_has_value(d));
   fail_unless(tree_kind(tree_value(d)) == T_AGGREGATE);
   fail_unless(tree_kind(tree_file_mode(d)) == T_REF);

   d = tree_decl(a, 11);
   fail_unless(tree_kind(d) == T_FILE_DECL);
   fail_unless(tree_ident(d) == ident_new("F2"));
   fail_unless(tree_has_value(d));
   fail_unless(tree_kind(tree_value(d)) == T_AGGREGATE);
   fail_unless(tree_kind(tree_file_mode(d)) == T_REF);
   fail_unless(tree_ident(tree_file_mode(d)) == ident_new("READ_MODE"));

   d = tree_decl(a, 12);
   fail_unless(tree_kind(d) == T_FILE_DECL);
   fail_unless(tree_ident(d) == ident_new("F3"));
   fail_if(tree_has_value(d));

   d = tree_decl(a, 13);
   fail_unless(tree_kind(d) == T_TYPE_DECL);
   fail_unless(tree_ident(d) == ident_new("R1"));
   t = tree_type(d);
   fail_unless(type_kind(t) == T_RECORD);
   fail_unless(type_fields(t) == 3);
   f = type_field(t, 1);
   fail_unless(tree_kind(f) == T_FIELD_DECL);
   fail_unless(tree_ident(f) == ident_new("B"));

   d = tree_decl(a, 14);
   fail_unless(tree_kind(d) == T_FILE_DECL);
   fail_unless(tree_ident(d) == ident_new("F4"));
   fail_unless(tree_has_value(d));
   fail_unless(tree_kind(tree_value(d)) == T_AGGREGATE);
   fail_unless(tree_kind(tree_file_mode(d)) == T_REF);
   fail_unless(tree_ident(tree_file_mode(d)) == ident_new("WRITE_MODE"));

   d = tree_decl(a, 15);
   fail_unless(tree_kind(d) == T_FILE_DECL);
   fail_unless(tree_ident(d) == ident_new("F5"));
   fail_unless(tree_has_value(d));
   fail_unless(tree_kind(tree_file_mode(d)) == T_REF);
   fail_unless(tree_ident(tree_file_mode(d)) == ident_new("READ_MODE"));

   d = tree_decl(a, 16);
   fail_unless(tree_kind(d) == T_TYPE_DECL);
   fail_unless(tree_ident(d) == ident_new("R2"));
   t = tree_type(d);
   fail_unless(type_kind(t) == T_RECORD);
   fail_unless(type_fields(t) == 1);

   a = parse();
   fail_unless(a == NULL);

   fail_unless(parse_errors() == 0);
}
END_TEST

START_TEST(test_literal)
{
   tree_t a, d, v;

   input_from_file(TESTDIR "/parse/literal.vhd");

   a = parse();
   fail_if(a == NULL);
   fail_unless(tree_kind(a) == T_ARCH);
   fail_unless(tree_decls(a) == 13);

   d = tree_decl(a, 0);
   fail_unless(tree_ident(d) == ident_new("POS"));
   v = tree_value(d);
   fail_unless(tree_kind(v) == T_LITERAL);
   fail_unless(tree_subkind(v) == L_INT);
   fail_unless(tree_ival(v) == 64);

   d = tree_decl(a, 1);
   fail_unless(tree_ident(d) == ident_new("NEG"));
   v = tree_value(d);
   fail_unless(tree_kind(v) == T_FCALL);
   fail_unless(tree_ident(v) == ident_new("\"-\""));
   fail_unless(tree_params(v) == 1);
   fail_unless(tree_subkind(tree_value(tree_param(v, 0))) == L_INT);
   fail_unless(tree_ival(tree_value(tree_param(v, 0))) == 265);

   d = tree_decl(a, 2);
   fail_unless(tree_kind(d) == T_CONST_DECL);
   fail_unless(tree_ident(d) == ident_new("C"));
   v = tree_value(d);
   fail_unless(tree_kind(v) == T_LITERAL);
   fail_unless(tree_subkind(v) == L_INT);
   fail_unless(tree_ival(v) == 523);

   d = tree_decl(a, 3);
   fail_unless(tree_kind(d) == T_CONST_DECL);
   fail_unless(tree_ident(d) == ident_new("A"));
   v = tree_value(d);
   fail_unless(tree_kind(v) == T_AGGREGATE);

   d = tree_decl(a, 4);
   fail_unless(tree_kind(d) == T_CONST_DECL);
   fail_unless(tree_ident(d) == ident_new("B"));
   v = tree_value(d);
   fail_unless(tree_kind(v) == T_AGGREGATE);

   d = tree_decl(a, 5);
   fail_unless(tree_ident(d) == ident_new("D"));
   v = tree_value(d);
   fail_unless(tree_kind(v) == T_LITERAL);
   fail_unless(tree_subkind(v) == L_INT);
   fail_unless(tree_ival(v) == 1000);

   d = tree_decl(a, 6);
   fail_unless(tree_ident(d) == ident_new("E"));
   v = tree_value(d);
   fail_unless(tree_kind(v) == T_LITERAL);
   fail_unless(tree_subkind(v) == L_REAL);
   fail_unless(tree_dval(v) == 1.234);

   d = tree_decl(a, 7);
   fail_unless(tree_ident(d) == ident_new("F"));
   v = tree_value(d);
   fail_unless(tree_kind(v) == T_LITERAL);
   fail_unless(tree_subkind(v) == L_REAL);
   fail_unless(tree_dval(v) == 0.21712);

   d = tree_decl(a, 8);
   fail_unless(tree_ident(d) == ident_new("G"));
   v = tree_value(d);
   fail_unless(tree_kind(v) == T_LITERAL);
   fail_unless(tree_subkind(v) == L_REAL);
   fail_unless(tree_dval(v) == 1400000.0);

   d = tree_decl(a, 9);
   fail_unless(tree_ident(d) == ident_new("H"));
   v = tree_value(d);
   fail_unless(tree_kind(v) == T_LITERAL);
   fail_unless(tree_subkind(v) == L_REAL);
   fail_unless(tree_dval(v) == 2.351);

   d = tree_decl(a, 10);
   fail_unless(tree_ident(d) == ident_new("I"));
   v = tree_value(d);
   fail_unless(tree_kind(v) == T_LITERAL);
   fail_unless(tree_subkind(v) == L_INT);
   fail_unless(tree_ival(v) == 1234);

   d = tree_decl(a, 11);
   fail_unless(tree_ident(d) == ident_new("J"));
   v = tree_value(d);
   fail_unless(tree_kind(v) == T_LITERAL);
   fail_unless(tree_subkind(v) == L_REAL);
   fail_unless(tree_dval(v) == 567.123);

   d = tree_decl(a, 12);
   fail_unless(tree_ident(d) == ident_new("K"));
   v = tree_value(d);
   fail_unless(tree_kind(v) == T_LITERAL);
   fail_unless(tree_subkind(v) == L_NULL);

   a = parse();
   fail_unless(a == NULL);

   fail_unless(parse_errors() == 0);
}
END_TEST

START_TEST(test_extended)
{
   tree_t a, d, n;

   input_from_file(TESTDIR "/parse/extended.vhd");

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
   tree_t p, d, c;

   input_from_file(TESTDIR "/parse/package.vhd");

   p = parse();
   fail_if(p == NULL);
   fail_unless(tree_kind(p) == T_PACKAGE);
   fail_unless(tree_decls(p) == 2);
   fail_unless(tree_contexts(p) == 0);
   fail_unless(tree_ident(p) == ident_new("ONE"));

   p = parse();
   fail_if(p == NULL);
   fail_unless(tree_kind(p) == T_PACKAGE);
   fail_unless(tree_decls(p) == 1);
   fail_unless(tree_contexts(p) == 1);
   fail_unless(tree_ident(tree_context(p, 0)) == ident_new("WORK.ONE"));
   fail_unless(icmp(tree_ident2(tree_context(p, 0)), "all"));
   fail_unless(tree_ident(p) == ident_new("TWO"));

   p = parse();
   fail_if(p == NULL);
   fail_unless(tree_kind(p) == T_PACK_BODY);
   fail_unless(tree_ident(p) == ident_new("ONE"));
   fail_unless(tree_decls(p) == 2);
   d = tree_decl(p, 0);
   fail_unless(tree_kind(d) == T_FUNC_BODY);
   d = tree_decl(p, 1);
   fail_unless(tree_kind(d) == T_VAR_DECL);

   p = parse();
   fail_if(p == NULL);
   fail_unless(tree_kind(p) == T_PACKAGE);
   fail_unless(tree_decls(p) == 1);
   fail_unless(tree_ident(p) == ident_new("THREE"));
   d = tree_decl(p, 0);
   fail_unless(tree_kind(d) == T_SIGNAL_DECL);

   fail_unless(tree_contexts(p) == 1);
   c = tree_context(p, 0);
   fail_unless(tree_kind(c) == T_LIBRARY);
   fail_unless(tree_ident(c) == ident_new("FOO"));

   p = parse();
   fail_unless(p == NULL);

   fail_unless(parse_errors() == 0);
}
END_TEST

START_TEST(test_enum)
{
   tree_t p, d, i;
   type_t t;

   input_from_file(TESTDIR "/parse/enum.vhd");

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

   input_from_file(TESTDIR "/parse/qual.vhd");

   a = parse();
   fail_if(a == NULL);
   fail_unless(tree_kind(a) == T_ARCH);
   fail_unless(tree_stmts(a) == 1);

   p = tree_stmt(a, 0);
   fail_unless(tree_stmts(p) == 2);

   s = tree_stmt(p, 0);
   fail_unless(tree_kind(s) == T_SIGNAL_ASSIGN);
   q = tree_value(tree_waveform(s, 0));
   fail_unless(tree_kind(q) == T_QUALIFIED);
   fail_unless(tree_ident(q) == ident_new("FOO"));
   e = tree_value(q);
   fail_unless(tree_kind(e) == T_REF);
   fail_unless(tree_ident(e) == ident_new("B"));

   s = tree_stmt(p, 1);
   fail_unless(tree_kind(s) == T_SIGNAL_ASSIGN);
   q = tree_value(tree_waveform(s, 0));
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

   input_from_file(TESTDIR "/parse/func.vhd");

   p = parse();
   fail_if(p == NULL);
   fail_unless(tree_kind(p) == T_PACKAGE);
   fail_unless(tree_decls(p) == 3);

   f = tree_decl(p, 0);
   fail_unless(tree_kind(f) == T_FUNC_DECL);
   fail_unless(tree_ident(f) == ident_new("ADD"));
   fail_unless(tree_ports(f) == 3);
   a = tree_port(f, 0);
   fail_unless(tree_kind(a) == T_PORT_DECL);
   fail_unless(tree_ident(a) == ident_new("X"));
   fail_unless(tree_subkind(a) == PORT_IN);
   t = tree_type(f);
   fail_unless(type_kind(t) == T_FUNC);

   f = tree_decl(p, 1);
   fail_unless(tree_kind(f) == T_FUNC_DECL);
   fail_unless(tree_ident(f) == ident_new("NAUGHTY"));
   fail_unless(tree_ports(f) == 0);

   f = tree_decl(p, 2);
   fail_unless(tree_kind(f) == T_FUNC_DECL);
   fail_unless(tree_ident(f) == ident_new("\"+\""));
   fail_unless(tree_ports(f) == 2);

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
   tree_t x;

   input_from_file(TESTDIR "/parse/array.vhd");

   p = parse();
   fail_if(p == NULL);
   fail_unless(tree_kind(p) == T_PACKAGE);
   fail_unless(tree_decls(p) == 6);

   d = tree_decl(p, 0);
   fail_unless(tree_kind(d) == T_TYPE_DECL);
   fail_unless(tree_ident(d) == ident_new("INT_ARRAY"));
   t = tree_type(d);
   fail_unless(type_kind(t) == T_UARRAY);
   fail_unless(type_index_constrs(t) == 1);
   i = type_index_constr(t, 0);
   fail_unless(type_kind(i) == T_UNRESOLVED);
   fail_unless(type_ident(i) == ident_new("INTEGER"));
   b = type_elem(t);
   fail_unless(type_kind(b) == T_UNRESOLVED);
   fail_unless(type_ident(b) == ident_new("INTEGER"));

   d = tree_decl(p, 1);
   fail_unless(tree_kind(d) == T_TYPE_DECL);
   fail_unless(tree_ident(d) == ident_new("TEN_INTS"));
   t = tree_type(d);
   fail_unless(type_kind(t) == T_CARRAY);
   fail_unless(type_dims(t) == 1);

   d = tree_decl(p, 3);
   fail_unless(tree_kind(d) == T_TYPE_DECL);
   fail_unless(tree_ident(d) == ident_new("CHAR_COUNTS"));
   t = tree_type(d);
   fail_unless(type_kind(t) == T_CARRAY);
   fail_unless(type_dims(t) == 1);
   r = type_dim(t, 0);
   fail_unless(r.kind == RANGE_EXPR);
   fail_unless(tree_kind(r.left) == T_REF);
   fail_unless(tree_ident(r.left) == ident_new("CHARS"));
   fail_unless(r.right == NULL);

   d = tree_decl(p, 4);
   fail_unless(tree_kind(d) == T_TYPE_DECL);
   fail_unless(tree_ident(d) == ident_new("TWO_D"));
   t = tree_type(d);
   fail_unless(type_kind(t) == T_CARRAY);
   fail_unless(type_dims(t) == 2);
   r = type_dim(t, 0);
   fail_unless(tree_ival(r.left) == 1);
   fail_unless(tree_ival(r.right) == 3);
   r = type_dim(t, 1);
   fail_unless(tree_ival(r.left) == 4);
   fail_unless(tree_ival(r.right) == 6);

   a = parse();
   fail_if(a == NULL);
   fail_unless(tree_kind(a) == T_ARCH);
   fail_unless(tree_decls(a) == 9);

   d = tree_decl(a, 0);
   fail_unless(tree_ident(d) == ident_new("X"));
   t = tree_type(d);
   fail_unless(type_kind(t) == T_SUBTYPE);
   fail_unless(type_dims(t) == 1);
   fail_unless(tree_ival(type_dim(t, 0).left) == 1);
   fail_unless(tree_ival(type_dim(t, 0).right) == 5);

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
   fail_unless(tree_ival(type_dim(t, 0).left) == 1);
   fail_unless(tree_ival(type_dim(t, 0).right) == 3);
   fail_unless(tree_has_value(d));
   g = tree_value(d);
   fail_unless(tree_kind(g) == T_AGGREGATE);
   fail_unless(tree_assocs(g) == 3);
   for (int i = 0; i < 3; i++) {
      x = tree_assoc(g, i);
      fail_unless(tree_subkind(x) == A_POS);
      fail_unless(tree_pos(x) == i);
      fail_unless(tree_ival(tree_value(x)) == i);
   }

   d = tree_decl(a, 3);
   fail_unless(tree_ident(d) == ident_new("N"));
   t = tree_type(d);
   fail_unless(type_kind(t) == T_SUBTYPE);
   fail_unless(type_dims(t) == 1);
   fail_unless(tree_ival(type_dim(t, 0).left) == 1);
   fail_unless(tree_ival(type_dim(t, 0).right) == 3);
   fail_unless(tree_has_value(d));
   g = tree_value(d);
   fail_unless(tree_kind(g) == T_AGGREGATE);
   fail_unless(tree_assocs(g) == 3);
   x = tree_assoc(g, 0);
   fail_unless(tree_subkind(x) == A_POS);
   fail_unless(tree_pos(x) == 0);
   fail_unless(tree_ival(tree_value(x)) == 0);
   x = tree_assoc(g, 1);
   fail_unless(tree_subkind(x) == A_NAMED);
   fail_unless(tree_kind(tree_name(x)) == T_LITERAL);
   fail_unless(tree_ival(tree_name(x)) == 1);
   fail_unless(tree_ival(tree_value(x)) == 1);
   x = tree_assoc(g, 2);
   fail_unless(tree_subkind(x) == A_OTHERS);
   fail_unless(tree_ival(tree_value(x)) == 2);

   d = tree_decl(a, 4);
   fail_unless(tree_ident(d) == ident_new("M"));
   t = tree_type(d);
   fail_unless(type_kind(t) == T_SUBTYPE);
   fail_unless(type_dims(t) == 1);
   fail_unless(tree_ival(type_dim(t, 0).left) == 1);
   fail_unless(tree_ival(type_dim(t, 0).right) == 3);
   fail_unless(tree_has_value(d));
   g = tree_value(d);
   fail_unless(tree_kind(g) == T_AGGREGATE);
   fail_unless(tree_assocs(g) == 1);
   x = tree_assoc(g, 0);
   fail_unless(tree_subkind(x) == A_RANGE);
   r = tree_range(x);
   fail_unless(tree_ival(r.left) == 1);
   fail_unless(tree_ival(r.right) == 3);
   fail_unless(tree_ival(tree_value(x)) == 0);

   d = tree_decl(a, 7);
   fail_unless(tree_ident(d) == ident_new("U"));
   fail_unless(tree_assocs(tree_value(d)) == 4);

   d = tree_decl(a, 8);
   fail_unless(tree_ident(d) == ident_new("V"));
   fail_unless(tree_assocs(tree_value(d)) == 4);

   p = tree_stmt(a, 0);
   fail_unless(tree_kind(p) == T_PROCESS);
   s = tree_stmt(p, 0);
   e = tree_target(s);
   fail_unless(tree_kind(e) == T_FCALL);
   fail_unless(tree_params(e) == 1);
   fail_unless(tree_subkind(tree_param(e, 0)) == P_POS);
   fail_unless(tree_pos(tree_param(e, 0)) == 0);
   fail_unless(tree_ival(tree_value(tree_param(e, 0))) == 0);
   s = tree_stmt(p, 1);
   fail_unless(tree_kind(s) == T_SIGNAL_ASSIGN);
   fail_unless(tree_waveforms(s) == 1);
   e = tree_value(tree_waveform(s, 0));
   fail_unless(tree_kind(e) == T_FCALL);
   s = tree_stmt(p, 3);
   fail_unless(tree_kind(s) == T_SIGNAL_ASSIGN);
   e = tree_target(s);
   fail_unless(tree_kind(e) == T_ARRAY_SLICE);
   e = tree_value(tree_waveform(s, 0));
   fail_unless(tree_kind(e) == T_ARRAY_SLICE);

   p = parse();
   fail_unless(p == NULL);

   fail_unless(parse_errors() == 0);
}
END_TEST

START_TEST(test_instance)
{
   tree_t a, s;

   input_from_file(TESTDIR "/parse/instance.vhd");

   a = parse();
   fail_if(a == NULL);
   fail_unless(tree_kind(a) == T_ARCH);
   fail_unless(tree_stmts(a) == 9);

   s = tree_stmt(a, 0);
   fail_unless(tree_kind(s) == T_INSTANCE);
   fail_unless(tree_class(s) == C_COMPONENT);
   fail_unless(tree_ident(s) == ident_new("A"));
   fail_unless(tree_ident2(s) == ident_new("FOO"));

   s = tree_stmt(a, 2);
   fail_unless(tree_kind(s) == T_INSTANCE);
   fail_unless(tree_class(s) == C_ENTITY);
   fail_unless(tree_ident(s) == ident_new("B1"));
   fail_unless(tree_ident2(s) == ident_new("WORK.FOO-GOO"));

   s = tree_stmt(a, 3);
   fail_unless(tree_kind(s) == T_INSTANCE);
   fail_unless(tree_class(s) == C_CONFIGURATION);
   fail_unless(tree_ident(s) == ident_new("C"));
   fail_unless(tree_ident2(s) == ident_new("WORK.BAR"));

   s = tree_stmt(a, 4);
   fail_unless(tree_kind(s) == T_INSTANCE);
   fail_unless(tree_class(s) == C_COMPONENT);
   fail_unless(tree_ident(s) == ident_new("D"));
   fail_unless(tree_ident2(s) == ident_new("FOO"));

   s = tree_stmt(a, 5);
   fail_unless(tree_kind(s) == T_INSTANCE);
   fail_unless(tree_class(s) == C_ENTITY);
   fail_unless(tree_ident(s) == ident_new("E"));
   fail_unless(tree_ident2(s) == ident_new("WORK.FOO"));
   fail_unless(tree_params(s) == 3);

   s = tree_stmt(a, 7);
   fail_unless(tree_kind(s) == T_INSTANCE);
   fail_unless(tree_class(s) == C_ENTITY);
   fail_unless(tree_ident(s) == ident_new("G"));
   fail_unless(tree_ident2(s) == ident_new("WORK.FOO"));
   fail_unless(tree_params(s) == 2);
   fail_unless(tree_genmaps(s) == 1);

   s = tree_stmt(a, 8);
   fail_unless(tree_kind(s) == T_INSTANCE);
   fail_unless(tree_class(s) == C_ENTITY);
   fail_unless(tree_subkind(tree_param(s, 0)) == P_NAMED);
   fail_unless(tree_kind(tree_value(tree_param(s, 0))) == T_OPEN);

   a = parse();
   fail_unless(a == NULL);

   fail_unless(parse_errors() == 0);
}
END_TEST

START_TEST(test_conc)
{
   tree_t a, s, c;

   input_from_file(TESTDIR "/parse/conc.vhd");

   a = parse();
   fail_if(a == NULL);
   fail_unless(tree_kind(a) == T_ARCH);
   fail_unless(tree_stmts(a) == 5);

   s = tree_stmt(a, 0);
   fail_unless(tree_kind(s) == T_CASSIGN);
   fail_unless(tree_kind(tree_target(s)) == T_REF);
   fail_unless(tree_conds(s) == 1);
   c = tree_cond(s, 0);
   fail_unless(tree_kind(c) == T_COND);
   fail_unless(tree_waveforms(c) == 1);
   fail_if(tree_has_value(c));
   fail_unless(tree_kind(tree_value(tree_waveform(c, 0))) == T_FCALL);

   s = tree_stmt(a, 1);
   fail_unless(tree_kind(s) == T_CASSIGN);
   fail_unless(tree_kind(tree_target(s)) == T_REF);
   fail_unless(tree_conds(s) == 3);
   c = tree_cond(s, 0);
   fail_unless(tree_kind(c) == T_COND);
   fail_unless(tree_waveforms(c) == 1);
   fail_unless(tree_has_value(c));
   fail_unless(tree_kind(tree_value(tree_waveform(c, 0))) == T_LITERAL);

   s = tree_stmt(a, 2);
   fail_unless(tree_kind(s) == T_SELECT);
   fail_unless(tree_assocs(s) == 3);

   s = tree_stmt(a, 3);
   fail_unless(tree_kind(s) == T_CPCALL);
   fail_unless(tree_params(s) == 2);

   s = tree_stmt(a, 4);
   fail_unless(tree_kind(s) == T_CASSERT);

   a = parse();
   fail_unless(a == NULL);

   fail_unless(parse_errors() == 0);
}
END_TEST

START_TEST(test_alias)
{
   tree_t a, d;

   input_from_file(TESTDIR "/parse/alias.vhd");

   a = parse();
   fail_if(a == NULL);
   fail_unless(tree_kind(a) == T_ARCH);
   fail_unless(tree_stmts(a) == 0);
   fail_unless(tree_decls(a) == 2);

   d = tree_decl(a, 0);
   fail_unless(tree_kind(d) == T_ALIAS);
   fail_unless(tree_ident(d) == ident_new("FOO"));
   fail_unless(tree_kind(tree_value(d)) == T_REF);
   fail_if(tree_has_type(d));

   d = tree_decl(a, 1);
   fail_unless(tree_kind(d) == T_ALIAS);
   fail_unless(tree_ident(d) == ident_new("BLAH"));
   fail_unless(tree_kind(tree_value(d)) == T_REF);
   fail_unless(tree_has_type(d));

   a = parse();
   fail_unless(a == NULL);

   fail_unless(parse_errors() == 0);
}
END_TEST

START_TEST(test_attr)
{
   tree_t a, d;

   input_from_file(TESTDIR "/parse/attr.vhd");

   a = parse();
   fail_if(a == NULL);
   fail_unless(tree_kind(a) == T_ARCH);
   fail_unless(tree_stmts(a) == 0);
   fail_unless(tree_decls(a) == 4);

   d = tree_decl(a, 0);
   fail_unless(tree_kind(d) == T_ATTR_DECL);
   fail_unless(tree_ident(d) == ident_new("FOO"));
   fail_unless(type_kind(tree_type(d)) == T_UNRESOLVED);
   fail_unless(type_ident(tree_type(d)) == ident_new("INTEGER"));

   d = tree_decl(a, 1);
   fail_unless(tree_kind(d) == T_ATTR_SPEC);
   fail_unless(tree_ident(d) == ident_new("FOO"));
   fail_unless(tree_ident2(d) == ident_new("X"));
   fail_unless(tree_kind(tree_value(d)) == T_LITERAL);
   fail_unless(tree_class(d) == C_SIGNAL);

   d = tree_decl(a, 2);
   fail_unless(tree_kind(d) == T_ATTR_SPEC);
   fail_unless(tree_ident(d) == ident_new("FOO"));
   fail_unless(tree_ident2(d) == ident_new("X"));
   fail_unless(tree_kind(tree_value(d)) == T_LITERAL);
   fail_unless(tree_class(d) == C_COMPONENT);

   d = tree_decl(a, 3);
   fail_unless(tree_kind(d) == T_ATTR_SPEC);
   fail_unless(tree_ident(d) == ident_new("FOO"));
   fail_unless(tree_ident2(d) == ident_new("X"));
   fail_unless(tree_kind(tree_value(d)) == T_LITERAL);
   fail_unless(tree_class(d) == C_LABEL);

   a = parse();
   fail_unless(a == NULL);

   fail_unless(parse_errors() == 0);
}
END_TEST

START_TEST(test_procedure)
{
   tree_t p, d;

   input_from_file(TESTDIR "/parse/procedure.vhd");

   p = parse();
   fail_if(p == NULL);
   fail_unless(tree_kind(p) == T_PACKAGE);
   fail_unless(tree_decls(p) == 1);

   d = tree_decl(p, 0);
   fail_unless(tree_kind(d) == T_PROC_DECL);
   fail_unless(tree_ports(d) == 2);
   fail_unless(tree_ident(d) == ident_new("FOO"));

   p = parse();
   fail_if(p == NULL);
   fail_unless(tree_kind(p) == T_PACK_BODY);
   fail_unless(tree_decls(p) == 1);

   d = tree_decl(p, 0);
   fail_unless(tree_kind(d) == T_PROC_BODY);
   fail_unless(tree_ports(d) == 2);
   fail_unless(tree_ident(d) == ident_new("FOO"));
   fail_unless(tree_decls(d) == 1);
   fail_unless(tree_stmts(d) == 1);

   p = parse();
   fail_unless(p == NULL);

   fail_unless(parse_errors() == 0);
}
END_TEST

START_TEST(test_ir1045)
{
   tree_t a, s, q, v, c;

   input_from_file(TESTDIR "/parse/ir1045.vhd");

   a = parse();
   fail_unless(tree_kind(a) == T_ARCH);

   s = tree_stmt(a, 0);
   fail_unless(tree_kind(s) == T_CASSIGN);
   c = tree_cond(s, 0);
   q = tree_value(tree_waveform(c, 0));
   fail_unless(tree_kind(q) == T_QUALIFIED);
   v = tree_value(q);
   fail_unless(tree_kind(v) == T_AGGREGATE);

   s = tree_stmt(a, 1);
   fail_unless(tree_kind(s) == T_CASSIGN);
   c = tree_cond(s, 0);
   q = tree_value(tree_waveform(c, 0));
   fail_unless(tree_kind(q) == T_QUALIFIED);
   v = tree_value(q);
   fail_unless(tree_kind(v) == T_REF);
   fail_unless(tree_ident(v) == ident_new("'1'"));

   a = parse();
   fail_unless(a == NULL);

   fail_unless(parse_errors() == 0);
}
END_TEST

START_TEST(test_concat)
{
   tree_t a, s, e;

   input_from_file(TESTDIR "/parse/concat.vhd");

   a = parse();
   fail_unless(tree_kind(a) == T_ARCH);

   s = tree_stmt(a, 0);
   fail_unless(tree_kind(s) == T_CASSIGN);
   e = tree_value(tree_waveform(tree_cond(s, 0), 0));
   fail_unless(tree_kind(e) == T_CONCAT);
   fail_unless(tree_params(e) == 2);

   a = parse();
   fail_unless(a == NULL);

   fail_unless(parse_errors() == 0);
}
END_TEST

START_TEST(test_based)
{
   tree_t p, d;

   input_from_file(TESTDIR "/parse/based.vhd");

   p = parse();
   fail_unless(tree_kind(p) == T_PACKAGE);

   d = tree_decl(p, 0);
   fail_unless(tree_ival(tree_value(d)) == 13);

   d = tree_decl(p, 1);
   fail_unless(tree_ival(tree_value(d)) == 6);

   d = tree_decl(p, 2);
   fail_unless(tree_ival(tree_value(d)) == 7);

   d = tree_decl(p, 3);
   fail_unless(tree_ival(tree_value(d)) == 1234);

   d = tree_decl(p, 4);
   fail_unless(tree_ival(tree_value(d)) == 0xbeef01);

   d = tree_decl(p, 5);
   fail_unless(tree_ival(tree_value(d)) == 2);

   d = tree_decl(p, 6);
   fail_unless(tree_ival(tree_value(d)) == 2);

   p = parse();
   fail_unless(p == NULL);

   fail_unless(parse_errors() == 0);
}
END_TEST

START_TEST(test_bitstring)
{
   tree_t p, a;

   input_from_file(TESTDIR "/parse/bitstring.vhd");

   p = parse();
   fail_unless(tree_kind(p) == T_PACKAGE);

   ident_t one = ident_new("'1'");
   ident_t zero = ident_new("'0'");

   a = tree_value(tree_decl(p, 0));
   fail_unless(tree_assocs(a) == 16);
   fail_unless(tree_ident(tree_value(tree_assoc(a, 0))) == zero);
   fail_unless(tree_ident(tree_value(tree_assoc(a, 1))) == zero);
   fail_unless(tree_ident(tree_value(tree_assoc(a, 2))) == zero);
   fail_unless(tree_ident(tree_value(tree_assoc(a, 3))) == one);
   fail_unless(tree_ident(tree_value(tree_assoc(a, 4))) == zero);
   fail_unless(tree_ident(tree_value(tree_assoc(a, 5))) == zero);
   fail_unless(tree_ident(tree_value(tree_assoc(a, 6))) == one);
   fail_unless(tree_ident(tree_value(tree_assoc(a, 7))) == zero);
   fail_unless(tree_ident(tree_value(tree_assoc(a, 8))) == zero);
   fail_unless(tree_ident(tree_value(tree_assoc(a, 9))) == zero);
   fail_unless(tree_ident(tree_value(tree_assoc(a, 10))) == one);
   fail_unless(tree_ident(tree_value(tree_assoc(a, 11))) == one);
   fail_unless(tree_ident(tree_value(tree_assoc(a, 12))) == zero);
   fail_unless(tree_ident(tree_value(tree_assoc(a, 13))) == one);
   fail_unless(tree_ident(tree_value(tree_assoc(a, 14))) == zero);
   fail_unless(tree_ident(tree_value(tree_assoc(a, 15))) == zero);

   a = tree_value(tree_decl(p, 1));
   fail_unless(tree_assocs(a) == 12);
   fail_unless(tree_ident(tree_value(tree_assoc(a, 0))) == zero);
   fail_unless(tree_ident(tree_value(tree_assoc(a, 1))) == zero);
   fail_unless(tree_ident(tree_value(tree_assoc(a, 2))) == one);

   a = tree_value(tree_decl(p, 2));
   fail_unless(tree_assocs(a) == 8);
   fail_unless(tree_ident(tree_value(tree_assoc(a, 0))) == one);
   fail_unless(tree_ident(tree_value(tree_assoc(a, 1))) == zero);
   fail_unless(tree_ident(tree_value(tree_assoc(a, 2))) == one);
   fail_unless(tree_ident(tree_value(tree_assoc(a, 3))) == zero);

   a = tree_value(tree_decl(p, 3));
   fail_unless(tree_assocs(a) == 3);
   fail_unless(tree_ident(tree_value(tree_assoc(a, 0))) == one);
   fail_unless(tree_ident(tree_value(tree_assoc(a, 1))) == zero);
   fail_unless(tree_ident(tree_value(tree_assoc(a, 2))) == one);

   a = tree_value(tree_decl(p, 4));
   fail_unless(tree_assocs(a) == 4);
   fail_unless(tree_ident(tree_value(tree_assoc(a, 0))) == one);
   fail_unless(tree_ident(tree_value(tree_assoc(a, 1))) == one);
   fail_unless(tree_ident(tree_value(tree_assoc(a, 2))) == one);
   fail_unless(tree_ident(tree_value(tree_assoc(a, 3))) == one);

   a = tree_value(tree_decl(p, 5));
   fail_unless(tree_assocs(a) == 8);
   fail_unless(tree_ident(tree_value(tree_assoc(a, 0))) == one);
   fail_unless(tree_ident(tree_value(tree_assoc(a, 1))) == zero);
   fail_unless(tree_ident(tree_value(tree_assoc(a, 2))) == one);
   fail_unless(tree_ident(tree_value(tree_assoc(a, 3))) == zero);
   fail_unless(tree_ident(tree_value(tree_assoc(a, 4))) == one);
   fail_unless(tree_ident(tree_value(tree_assoc(a, 5))) == zero);
   fail_unless(tree_ident(tree_value(tree_assoc(a, 6))) == one);
   fail_unless(tree_ident(tree_value(tree_assoc(a, 7))) == one);

   p = parse();
   fail_unless(p == NULL);

   fail_unless(parse_errors() == 0);
}
END_TEST

START_TEST(test_block)
{
   tree_t a, b;

   input_from_file(TESTDIR "/parse/block.vhd");

   a = parse();
   fail_unless(tree_kind(a) == T_ARCH);
   fail_unless(tree_stmts(a) == 2);

   b = tree_stmt(a, 0);
   fail_unless(tree_kind(b) == T_BLOCK);
   fail_unless(tree_decls(b) == 0);
   fail_unless(tree_stmts(b) == 0);

   b = tree_stmt(a, 1);
   fail_unless(tree_kind(b) == T_BLOCK);
   fail_unless(tree_decls(b) == 2);
   fail_unless(tree_stmts(b) == 1);

   a = parse();
   fail_unless(a == NULL);

   fail_unless(parse_errors() == 0);
}
END_TEST

START_TEST(test_comp)
{
   tree_t p, c;

   input_from_file(TESTDIR "/parse/comp.vhd");

   p = parse();
   fail_unless(tree_kind(p) == T_PACKAGE);
   fail_unless(tree_decls(p) == 2);

   c = tree_decl(p, 0);
   fail_unless(tree_kind(c) == T_COMPONENT);
   fail_unless(tree_ident(c) == ident_new("C"));
   fail_unless(tree_ports(c) == 1);
   fail_unless(tree_generics(c) == 1);

   c = tree_decl(p, 1);
   fail_unless(tree_kind(c) == T_COMPONENT);
   fail_unless(tree_ident(c) == ident_new("FOO"));
   fail_unless(tree_ports(c) == 1);
   fail_unless(tree_generics(c) == 0);

   p = parse();
   fail_unless(p == NULL);

   fail_unless(parse_errors() == 0);
}
END_TEST

START_TEST(test_generate)
{
   tree_t a, g;

   input_from_file(TESTDIR "/parse/generate.vhd");

   a = parse();
   fail_unless(tree_kind(a) == T_ARCH);
   fail_unless(tree_stmts(a) == 4);

   g = tree_stmt(a, 0);
   fail_unless(tree_kind(g) == T_IF_GENERATE);
   fail_unless(tree_decls(g) == 1);
   fail_unless(tree_stmts(g) == 1);
   fail_unless(icmp(tree_ident(g), "G1"));

   g = tree_stmt(a, 1);
   fail_unless(tree_kind(g) == T_IF_GENERATE);
   fail_unless(tree_decls(g) == 0);
   fail_unless(tree_stmts(g) == 1);
   fail_unless(icmp(tree_ident(g), "G2"));
   g = tree_stmt(g, 0);
   fail_unless(tree_kind(g) == T_IF_GENERATE);
   fail_unless(tree_decls(g) == 0);
   fail_unless(tree_stmts(g) == 1);
   fail_unless(icmp(tree_ident(g), "G2A"));

   g = tree_stmt(a, 2);
   fail_unless(tree_kind(g) == T_FOR_GENERATE);
   fail_unless(tree_decls(g) == 1);
   fail_unless(tree_stmts(g) == 1);
   fail_unless(icmp(tree_ident(g), "G3"));
   fail_unless(icmp(tree_ident2(g), "I"));

   g = tree_stmt(a, 3);
   fail_unless(tree_kind(g) == T_FOR_GENERATE);
   fail_unless(tree_decls(g) == 0);
   fail_unless(tree_stmts(g) == 0);
   fail_unless(icmp(tree_ident(g), "G4"));
   fail_unless(icmp(tree_ident2(g), "I"));

   a = parse();
   fail_unless(a == NULL);

   fail_unless(parse_errors() == 0);
}
END_TEST

START_TEST(test_access)
{
   tree_t a, p, s;

   input_from_file(TESTDIR "/parse/access.vhd");

   a = parse();
   fail_unless(tree_kind(a) == T_ARCH);
   fail_unless(tree_stmts(a) == 1);

   p = tree_stmt(a, 0);
   fail_unless(tree_kind(p) == T_PROCESS);
   fail_unless(tree_stmts(p) == 3);

   s = tree_stmt(p, 0);
   fail_unless(tree_kind(tree_target(s)) == T_ALL);

   s = tree_stmt(p, 2);
   fail_unless(tree_kind(tree_value(s)) == T_NEW);

   a = parse();
   fail_unless(a == NULL);

   fail_unless(parse_errors() == 0);
}
END_TEST

START_TEST(test_spec)
{
   tree_t a, d, b;

   input_from_file(TESTDIR "/parse/spec.vhd");

   a = parse();
   fail_if(a == NULL);
   fail_unless(tree_kind(a) == T_ARCH);
   fail_unless(tree_stmts(a) == 0);
   fail_unless(tree_decls(a) == 7);

   d = tree_decl(a, 0);
   fail_unless(tree_kind(d) == T_SPEC);
   fail_unless(tree_ident(d) == ident_new("X"));
   fail_unless(tree_ident2(d) == ident_new("Y"));
   b = tree_value(d);
   fail_unless(tree_kind(b) == T_BINDING);
   fail_unless(tree_class(b) == C_ENTITY);
   fail_unless(tree_ident(b) == ident_new("WORK.FOO"));

   d = tree_decl(a, 1);
   fail_unless(tree_kind(d) == T_SPEC);
   fail_unless(tree_ident(d) == ident_new("X1"));
   fail_unless(tree_ident2(d) == ident_new("Y"));
   b = tree_value(d);
   fail_unless(tree_kind(b) == T_BINDING);
   fail_unless(tree_class(b) == C_ENTITY);
   fail_unless(tree_ident(b) == ident_new("WORK.FOO"));

   d = tree_decl(a, 2);
   fail_unless(tree_kind(d) == T_SPEC);
   fail_unless(tree_ident(d) == ident_new("X2"));
   fail_unless(tree_ident2(d) == ident_new("Y"));
   b = tree_value(d);
   fail_unless(tree_kind(b) == T_BINDING);
   fail_unless(tree_class(b) == C_ENTITY);
   fail_unless(tree_ident(b) == ident_new("WORK.FOO"));

   d = tree_decl(a, 3);
   fail_unless(tree_kind(d) == T_SPEC);
   fail_unless(tree_ident(d) == ident_new("X"));
   fail_unless(tree_ident2(d) == ident_new("Y"));
   b = tree_value(d);
   fail_unless(tree_kind(b) == T_BINDING);
   fail_unless(tree_class(b) == C_ENTITY);
   fail_unless(tree_ident(b) == ident_new("WORK.FOO"));
   fail_unless(tree_ident2(b) == ident_new("BAR"));

   d = tree_decl(a, 4);
   fail_unless(tree_kind(d) == T_SPEC);
   fail_unless(tree_ident(d) == ident_new("X"));
   fail_unless(tree_ident2(d) == ident_new("Y"));
   b = tree_value(d);
   fail_unless(tree_kind(b) == T_BINDING);
   fail_unless(tree_class(b) == C_ENTITY);
   fail_unless(tree_ident(b) == ident_new("WORK.FOO"));
   fail_unless(tree_ident2(b) == ident_new("BAR"));
   fail_unless(tree_genmaps(b) == 1);
   fail_unless(tree_params(b) == 1);

   d = tree_decl(a, 5);
   fail_unless(tree_kind(d) == T_SPEC);
   fail_unless(tree_ident(d) == ident_new("all"));
   fail_unless(tree_ident2(d) == ident_new("Y"));
   b = tree_value(d);
   fail_unless(tree_kind(b) == T_BINDING);
   fail_unless(tree_class(b) == C_CONFIGURATION);
   fail_unless(tree_ident(b) == ident_new("YAH"));

   d = tree_decl(a, 6);
   fail_unless(tree_kind(d) == T_SPEC);
   fail_if(tree_has_ident(d));
   fail_unless(tree_ident2(d) == ident_new("Y"));
   fail_if(tree_has_value(d));

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
#if 0
   tcase_add_test(tc_core, test_extended);
   tcase_add_test(tc_core, test_package);
   tcase_add_test(tc_core, test_enum);
   tcase_add_test(tc_core, test_qual);
   tcase_add_test(tc_core, test_func);
   tcase_add_test(tc_core, test_array);
   tcase_add_test(tc_core, test_instance);
   tcase_add_test(tc_core, test_conc);
   tcase_add_test(tc_core, test_alias);
   tcase_add_test(tc_core, test_attr);
   tcase_add_test(tc_core, test_procedure);
   tcase_add_test(tc_core, test_ir1045);
   tcase_add_test(tc_core, test_concat);
   tcase_add_test(tc_core, test_based);
   tcase_add_test(tc_core, test_bitstring);
   tcase_add_test(tc_core, test_block);
   tcase_add_test(tc_core, test_comp);
   tcase_add_test(tc_core, test_generate);
   tcase_add_test(tc_core, test_access);
   tcase_add_test(tc_core, test_spec);
#endif
   suite_add_tcase(s, tc_core);

   SRunner *sr = srunner_create(s);
   srunner_run_all(sr, CK_NORMAL);

   int nfail = srunner_ntests_failed(sr);

   srunner_free(sr);

   return nfail == 0 ? EXIT_SUCCESS : EXIT_FAILURE;
}
