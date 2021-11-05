#include "type.h"
#include "util.h"
#include "tree.h"
#include "phase.h"
#include "common.h"
#include "test_util.h"
#include "loc.h"

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
   fail_unless(tree_ident(e) == ident_new("WORK.ONE"));

   e = parse();
   fail_if(e == NULL);
   fail_unless(tree_kind(e) == T_ENTITY);
   fail_unless(tree_ident(e) == ident_new("WORK.TWO"));

   e = parse();
   fail_if(e == NULL);
   fail_unless(tree_kind(e) == T_ENTITY);
   fail_unless(tree_ident(e) == ident_new("WORK.THREE"));

   e = parse();
   fail_if(e == NULL);
   fail_unless(tree_kind(e) == T_ENTITY);
   fail_unless(tree_ident(e) == ident_new("WORK.FOUR"));

   fail_unless(tree_ports(e) == 5);

   p = tree_port(e, 0);
   fail_unless(tree_kind(p) == T_PORT_DECL);
   fail_unless(tree_ident(p) == ident_new("A"));
   fail_unless(tree_subkind(p) == PORT_IN);
   fail_unless(tree_class(p) == C_SIGNAL);
   t = tree_type(p);
   fail_unless(type_kind(t) == T_INTEGER);
   fail_unless(type_ident(t) == ident_new("STD.STANDARD.INTEGER"));
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
   fail_unless(type_kind(t) == T_ENUM);
   fail_unless(type_ident(t) == ident_new("STD.STANDARD.BIT"));
   fail_if(tree_has_value(p));

   p = tree_port(e, 2);
   fail_unless(tree_kind(p) == T_PORT_DECL);
   fail_unless(tree_ident(p) == ident_new("BEE"));
   fail_unless(tree_subkind(p) == PORT_OUT);
   t = tree_type(p);
   fail_unless(type_kind(t) == T_ENUM);
   fail_unless(type_ident(t) == ident_new("STD.STANDARD.BIT"));
   fail_if(tree_has_value(p));

   p = tree_port(e, 3);
   fail_unless(tree_kind(p) == T_PORT_DECL);
   fail_unless(tree_ident(p) == ident_new("C"));
   fail_unless(tree_subkind(p) == PORT_INOUT);
   t = tree_type(p);
   fail_unless(type_kind(t) == T_INTEGER);
   fail_unless(type_ident(t) == ident_new("STD.STANDARD.INTEGER"));
   fail_if(tree_has_value(p));

   p = tree_port(e, 4);
   fail_unless(tree_kind(p) == T_PORT_DECL);
   fail_unless(tree_ident(p) == ident_new("D"));
   fail_unless(tree_subkind(p) == PORT_BUFFER);
   t = tree_type(p);
   fail_unless(type_kind(t) == T_ENUM);
   fail_unless(type_ident(t) == ident_new("STD.STANDARD.BIT"));
   fail_if(tree_has_value(p));

   e = parse();
   fail_if(e == NULL);
   fail_unless(tree_kind(e) == T_ENTITY);
   fail_unless(tree_ident(e) == ident_new("WORK.FIVE"));

   fail_unless(tree_generics(e) == 2);

   g = tree_generic(e, 0);
   fail_unless(tree_kind(g) == T_PORT_DECL);
   fail_unless(tree_ident(g) == ident_new("X"));
   t = tree_type(g);
   fail_unless(type_kind(t) == T_ENUM);
   fail_unless(type_ident(t) == ident_new("STD.STANDARD.BOOLEAN"));
   fail_if(tree_has_value(p));

   g = tree_generic(e, 1);
   fail_unless(tree_kind(g) == T_PORT_DECL);
   fail_unless(tree_ident(g) == ident_new("Y"));
   t = tree_type(g);
   fail_unless(type_kind(t) == T_INTEGER);
   fail_unless(type_ident(t) == ident_new("STD.STANDARD.INTEGER"));
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
   fail_if(e == NULL);
   fail_unless(tree_kind(e) == T_ENTITY);
   fail_unless(tree_ident(e) == ident_new("WORK.EIGHT"));
   fail_unless(tree_generics(e) == 1);

   g = tree_generic(e, 0);
   fail_unless(tree_kind(g) == T_PORT_DECL);
   fail_unless(tree_class(g) == C_SIGNAL);

   e = parse();
   fail_unless(e == NULL);

   fail_if_errors();
}
END_TEST

START_TEST(test_arch)
{
   tree_t e, a, d, v;

   input_from_file(TESTDIR "/parse/arch.vhd");

   e = parse();
   fail_if(e == NULL);
   fail_unless(tree_kind(e) == T_ENTITY);

   a = parse();
   fail_if(a == NULL);
   fail_unless(tree_kind(a) == T_ARCH);
   fail_unless(tree_ident(a) == ident_new("WORK.ONE-A"));
   fail_unless(tree_ident2(a) == ident_new("ONE"));
   fail_unless(tree_primary(a) == e);
   fail_unless(tree_decls(a) == 3);
   d = tree_decl(a, 0);
   fail_unless(tree_kind(d) == T_SIGNAL_DECL);
   fail_unless(tree_ident(d) == ident_new("X"));
   fail_unless(type_kind(tree_type(d)) == T_INTEGER);
   fail_unless(type_ident(tree_type(d)) == ident_new("STD.STANDARD.INTEGER"));
   fail_if(tree_has_value(d));
   d = tree_decl(a, 1);
   fail_unless(tree_kind(d) == T_SIGNAL_DECL);
   fail_unless(tree_ident(d) == ident_new("Y"));
   fail_unless(type_kind(tree_type(d)) == T_INTEGER);
   fail_unless(type_ident(tree_type(d)) == ident_new("STD.STANDARD.INTEGER"));
   fail_unless(tree_has_value(d));
   v = tree_value(d);
   fail_unless(tree_kind(v) == T_LITERAL);
   fail_unless(tree_subkind(v) == L_INT);
   fail_unless(tree_ival(v) == 7);
   d = tree_decl(a, 2);
   fail_unless(tree_kind(d) == T_SIGNAL_DECL);
   fail_unless(tree_ident(d) == ident_new("Z"));
   fail_unless(type_kind(tree_type(d)) == T_INTEGER);
   fail_unless(type_ident(tree_type(d)) == ident_new("STD.STANDARD.INTEGER"));
   fail_unless(tree_has_value(d));
   v = tree_value(d);
   fail_unless(tree_kind(v) == T_LITERAL);
   fail_unless(tree_subkind(v) == L_INT);
   fail_unless(tree_ival(v) == 7);

   a = parse();
   fail_if(a == NULL);
   fail_unless(tree_kind(a) == T_ARCH);
   fail_unless(tree_ident(a) == ident_new("WORK.ONE-B"));
   fail_unless(tree_ident2(a) == ident_new("ONE"));

   a = parse();
   fail_if(a == NULL);
   fail_unless(tree_kind(a) == T_ARCH);
   fail_unless(tree_ident(a) == ident_new("WORK.ONE-C"));
   fail_unless(tree_ident2(a) == ident_new("ONE"));

   a = parse();
   fail_unless(a == NULL);

   fail_if_errors();
}
END_TEST

START_TEST(test_process)
{
   tree_t e, a, p, d, s;

   input_from_file(TESTDIR "/parse/process.vhd");

   e = parse();
   fail_if(e == NULL);
   fail_unless(tree_kind(e) == T_ENTITY);

   a = parse();
   fail_if(a == NULL);
   fail_unless(tree_kind(a) == T_ARCH);
   fail_unless(tree_stmts(a) == 5);

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
   fail_if(tree_flags(p) & TREE_F_POSTPONED);
   d = tree_trigger(p, 0);
   fail_unless(tree_kind(d) == T_REF);
   fail_unless(tree_ident(d) == ident_new("X"));

   p = tree_stmt(a, 3);
   fail_unless(tree_kind(p) == T_PROCESS);
   fail_unless(tree_flags(p) & TREE_F_POSTPONED);

   p = tree_stmt(a, 4);
   fail_unless(tree_kind(p) == T_CASSERT);
   fail_unless(tree_flags(p) & TREE_F_POSTPONED);

   a = parse();
   fail_unless(a == NULL);

   fail_if_errors();
}
END_TEST

START_TEST(test_seq)
{
   tree_t a, p, s, e, b;

   set_standard(STD_08);

   input_from_file(TESTDIR "/parse/seq.vhd");

   e = parse();
   fail_if(e == NULL);
   fail_unless(tree_kind(e) == T_ENTITY);

   a = parse();
   fail_if(a == NULL);
   fail_unless(tree_kind(a) == T_ARCH);
   fail_unless(tree_stmts(a) == 18);

   fail_if_errors();

   // Wait statements

   p = tree_stmt(a, 0);
   fail_unless(tree_kind(p) == T_PROCESS);
   fail_unless(tree_stmts(p) == 8);

   s = tree_stmt(p, 0);
   fail_unless(tree_kind(s) == T_WAIT);
   fail_unless(tree_has_delay(s));
   e = tree_delay(s);
   fail_unless(tree_kind(e) == T_LITERAL);
   fail_unless(tree_subkind(e) == L_PHYSICAL);
   fail_unless(type_kind(tree_type(e)) == T_PHYSICAL);
   fail_unless(tree_ival(e) == 1);
   fail_unless(tree_ident(e) == ident_new("NS"));
   fail_unless(tree_kind(tree_ref(e)) == T_UNIT_DECL);

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
   fail_if(tree_has_message(s));

   s = tree_stmt(p, 3);
   fail_unless(tree_kind(s) == T_ASSERT);
   fail_if(tree_has_value(s));
   fail_unless(tree_kind(tree_severity(s)) == T_REF);
   fail_unless(tree_ident(tree_severity(s)) == ident_new("NOTE"));
   fail_unless(tree_has_message(s));
   fail_unless(tree_kind(tree_message(s)) == T_LITERAL);
   fail_unless(tree_subkind(tree_message(s)) == L_STRING);
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
   fail_unless(tree_subkind(tree_range(s, 0)) == RANGE_TO);
   fail_unless(tree_kind(tree_left(tree_range(s, 0))) == T_LITERAL);
   fail_unless(tree_kind(tree_left(tree_range(s, 0))) == T_LITERAL);

   s = tree_stmt(p, 1);
   fail_unless(tree_kind(s) == T_FOR);
   fail_unless(tree_stmts(s) == 1);
   fail_unless(tree_subkind(tree_range(s, 0)) == RANGE_EXPR);
   fail_unless(tree_kind(tree_value(tree_range(s, 0))) == T_ATTR_REF);

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
   fail_unless(tree_has_ident(s));

   s = tree_stmt(p, 1);
   fail_unless(tree_kind(s) == T_PCALL);
   fail_unless(tree_ident2(s) == ident_new("BAR"));
   fail_unless(tree_params(s) == 0);
   fail_unless(tree_has_ident(s));

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

   // Case statement range bug

   p = tree_stmt(a, 15);

   s = tree_stmt(p, 0);
   fail_unless(tree_kind(s) == T_CASE);
   fail_unless(tree_assocs(s) == 1);
   fail_unless(tree_subkind(tree_assoc(s, 0)) == A_NAMED);

   // Process with all-sensitivity

   p = tree_stmt(a, 16);
   fail_unless(tree_triggers(p) == 1);
   fail_unless(tree_kind(tree_trigger(p, 0)) == T_ALL);

   // Aggregate target of variable assignment

   p = tree_stmt(a, 17);

   s = tree_stmt(p, 0);
   fail_unless(tree_kind(s) == T_VAR_ASSIGN);
   fail_unless(tree_kind(tree_target(s)) == T_AGGREGATE);

   a = parse();
   fail_unless(a == NULL);

   fail_if_errors();
}
END_TEST

START_TEST(test_types)
{
   tree_t a, b, d, f, u, r;
   type_t t;

   input_from_file(TESTDIR "/parse/types.vhd");

   b = parse();
   fail_if(b == NULL);
   fail_unless(tree_kind(b) == T_ENTITY);

   a = parse();
   fail_if(a == NULL);
   fail_unless(tree_kind(a) == T_ARCH);
   fail_unless(tree_decls(a) == 78);  // Includes predefined

   d = search_decls(a, ident_new("MY_INT"), 0);
   fail_if(d == NULL);
   fail_unless(tree_kind(d) == T_TYPE_DECL);
   t = tree_type(d);
   fail_unless(type_kind(t) == T_INTEGER);
   fail_unless(type_dims(t) == 1);
   r = type_dim(t, 0);
   fail_unless(tree_subkind(r) == RANGE_TO);
   fail_unless(tree_kind(tree_left(r)) == T_LITERAL);
   fail_unless(tree_kind(tree_right(r)) == T_LITERAL);

   d = search_decls(a, ident_new("RESISTANCE"), 0);
   fail_if(d == NULL);
   fail_unless(tree_kind(d) == T_TYPE_DECL);
   t = tree_type(d);
   fail_unless(type_kind(t) == T_PHYSICAL);
   fail_unless(type_dims(t) == 1);
   r = type_dim(t, 0);
   fail_unless(tree_subkind(r) == RANGE_TO);
   fail_unless(tree_kind(tree_left(r)) == T_LITERAL);
   fail_unless(tree_kind(tree_right(r)) == T_LITERAL);
   fail_unless(type_units(t) == 3);
   u = type_unit(t, 0);
   fail_unless(tree_ident(u) == ident_new("OHM"));
   fail_unless(tree_kind(tree_value(u)) == T_LITERAL);
   fail_unless(tree_subkind(tree_value(u)) == L_PHYSICAL);
   u = type_unit(t, 1);
   fail_unless(tree_ident(u) == ident_new("KOHM"));
   fail_unless(tree_kind(tree_value(u)) == T_LITERAL);
   fail_unless(tree_subkind(tree_value(u)) == L_PHYSICAL);
   fail_unless(tree_ival(tree_value(u)) == 1000);
   u = type_unit(t, 2);
   fail_unless(tree_ident(u) == ident_new("MOHM"));
   fail_unless(tree_kind(tree_value(u)) == T_LITERAL);
   fail_unless(tree_subkind(tree_value(u)) == L_PHYSICAL);
   fail_unless(tree_ival(tree_value(u)) == 1000);
   fail_unless(tree_ident(tree_value(u)) == ident_new("KOHM"));

   d = search_decls(a, ident_new("BIG_R"), 0);
   fail_if(d == NULL);
   fail_unless(tree_kind(d) == T_TYPE_DECL);
   t = tree_type(d);
   fail_unless(type_kind(t) == T_SUBTYPE);
   fail_unless(type_kind(type_base(t)) == T_PHYSICAL);
   fail_unless(type_ident(type_base(t)) == ident_new("WORK.B(A).RESISTANCE"));

   d = search_decls(a, ident_new("MY_SMALL_INT"), 0);
   fail_if(d == NULL);
   fail_unless(tree_kind(d) == T_TYPE_DECL);
   t = tree_type(d);
   fail_unless(type_kind(t) == T_SUBTYPE);
   fail_unless(type_kind(type_base(t)) == T_INTEGER);
   fail_unless(type_ident(type_base(t)) == ident_new("WORK.B(A).MY_INT"));

   d = search_decls(a, ident_new("FOO"), 0);
   fail_if(d == NULL);
   fail_unless(tree_kind(d) == T_TYPE_DECL);
   t = tree_type(d);
   fail_unless(type_kind(t) == T_SUBTYPE);
   fail_unless(type_kind(type_base(t)) == T_INTEGER);
   fail_unless(type_ident(type_base(t)) == ident_new("WORK.B(A).MY_INT"));
   r = tree_range(type_constraint(t), 0);
   fail_unless(tree_kind(tree_left(r)) == T_LITERAL);
   fail_unless(tree_kind(tree_right(r)) == T_ATTR_REF);
   fail_unless(tree_ident(tree_name(tree_right(r))) == ident_new("MY_INT"));
   fail_unless(tree_ident(tree_right(r)) == ident_new("HIGH"));

   d = search_decls(a, ident_new("RINT"), 0);
   fail_if(d == NULL);
   fail_unless(tree_kind(d) == T_TYPE_DECL);
   t = tree_type(d);
   fail_unless(type_kind(t) == T_SUBTYPE);
   fail_unless(type_has_resolution(t));
   f = type_resolution(t);
   fail_unless(tree_kind(f) == T_REF);
   fail_unless(tree_ident(f) == ident_new("RESOLVED"));

   d = search_decls(a, ident_new("P"), 0);
   fail_if(d == NULL);
   fail_unless(tree_kind(d) == T_TYPE_DECL);
   t = tree_type(d);
   fail_unless(type_kind(t) == T_ACCESS);
   fail_unless(type_kind(type_access(t)) == T_INTEGER);
   fail_unless(type_ident(type_access(t)) == ident_new("WORK.B(A).MY_INT"));

   d = search_decls(a, ident_new("F"), 0);
   fail_if(d == NULL);
   fail_unless(tree_kind(d) == T_TYPE_DECL);
   t = tree_type(d);
   fail_unless(type_kind(t) == T_FILE);
   fail_unless(type_kind(type_file(t)) == T_INTEGER);
   fail_unless(type_ident(type_file(t)) == ident_new("WORK.B(A).MY_INT"));

   d = search_decls(a, ident_new("F1"), 0);
   fail_if(d == NULL);
   fail_unless(tree_kind(d) == T_FILE_DECL);
   fail_unless(tree_has_value(d));
   fail_unless(tree_kind(tree_value(d)) == T_LITERAL);
   fail_unless(tree_kind(tree_file_mode(d)) == T_REF);

   d = search_decls(a, ident_new("F2"), 0);
   fail_if(d == NULL);
   fail_unless(tree_kind(d) == T_FILE_DECL);
   fail_unless(tree_has_value(d));
   fail_unless(tree_kind(tree_value(d)) == T_LITERAL);
   fail_unless(tree_subkind(tree_value(d)) == L_STRING);
   fail_unless(tree_kind(tree_file_mode(d)) == T_REF);
   fail_unless(tree_ident(tree_file_mode(d)) == ident_new("READ_MODE"));

   d = search_decls(a, ident_new("F3"), 0);
   fail_if(d == NULL);
   fail_unless(tree_kind(d) == T_FILE_DECL);
   fail_if(tree_has_value(d));

   d = search_decls(a, ident_new("R1"), 0);
   fail_if(d == NULL);
   fail_unless(tree_kind(d) == T_TYPE_DECL);
   t = tree_type(d);
   fail_unless(type_kind(t) == T_RECORD);
   fail_unless(type_fields(t) == 3);
   f = type_field(t, 1);
   fail_unless(tree_kind(f) == T_FIELD_DECL);
   fail_unless(tree_pos(f) == 1);
   fail_unless(tree_ident(f) == ident_new("B"));

   d = search_decls(a, ident_new("F4"), 0);
   fail_if(d == NULL);
   fail_unless(tree_kind(d) == T_FILE_DECL);
   fail_unless(tree_has_value(d));
   fail_unless(tree_kind(tree_value(d)) == T_LITERAL);
   fail_unless(tree_kind(tree_file_mode(d)) == T_REF);
   fail_unless(tree_ident(tree_file_mode(d)) == ident_new("WRITE_MODE"));

   d = search_decls(a, ident_new("F5"), 0);
   fail_if(d == NULL);
   fail_unless(tree_kind(d) == T_FILE_DECL);
   fail_unless(tree_has_value(d));
   fail_unless(tree_kind(tree_file_mode(d)) == T_REF);
   fail_unless(tree_ident(tree_file_mode(d)) == ident_new("READ_MODE"));

   d = search_decls(a, ident_new("R2"), 0);
   fail_if(d == NULL);
   fail_unless(tree_kind(d) == T_TYPE_DECL);
   t = tree_type(d);
   fail_unless(type_kind(t) == T_RECORD);
   fail_unless(type_fields(t) == 1);

   a = parse();
   fail_unless(a == NULL);

   fail_if_errors();
}
END_TEST

START_TEST(test_literal)
{
   tree_t a, e, d, v;

   input_from_file(TESTDIR "/parse/literal.vhd");

   e = parse();
   fail_if(e == NULL);
   fail_unless(tree_kind(e) == T_ENTITY);

   a = parse();
   fail_if(a == NULL);
   fail_unless(tree_kind(a) == T_ARCH);

   d = search_decls(a, ident_new("POS"), 0);
   fail_if(d == NULL);
   v = tree_value(d);
   fail_unless(tree_kind(v) == T_LITERAL);
   fail_unless(tree_subkind(v) == L_INT);
   fail_unless(tree_ival(v) == 64);
   d = search_decls(a, ident_new("POS"), 0);
   fail_if(d == NULL);

   d = search_decls(a, ident_new("NEG"), 0);
   fail_if(d == NULL);
   v = tree_value(d);
   fail_unless(tree_kind(v) == T_FCALL);
   fail_unless(tree_ident(v) == ident_new("\"-\""));
   fail_unless(tree_params(v) == 1);
   fail_unless(tree_subkind(tree_value(tree_param(v, 0))) == L_INT);
   fail_unless(tree_ival(tree_value(tree_param(v, 0))) == 265);

   d = search_decls(a, ident_new("C"), 0);
   fail_if(d == NULL);
   fail_unless(tree_kind(d) == T_CONST_DECL);
   fail_unless(tree_ident(d) == ident_new("C"));
   v = tree_value(d);
   fail_unless(tree_kind(v) == T_LITERAL);
   fail_unless(tree_subkind(v) == L_INT);
   fail_unless(tree_ival(v) == 523);

   d = search_decls(a, ident_new("A"), 0);
   fail_if(d == NULL);
   fail_unless(tree_kind(d) == T_CONST_DECL);
   fail_unless(tree_ident(d) == ident_new("A"));
   v = tree_value(d);
   fail_unless(tree_kind(v) == T_LITERAL);
   fail_unless(tree_subkind(v) == L_STRING);
   fail_unless(tree_chars(v) == 6);
   fail_unless(tree_ident(tree_char(v, 0)) == ident_new("'h'"));
   fail_unless(tree_ident(tree_char(v, 1)) == ident_new("'e'"));
   fail_unless(tree_ident(tree_char(v, 2)) == ident_new("'l'"));
   fail_unless(tree_ident(tree_char(v, 3)) == ident_new("'\"'"));
   fail_unless(tree_ident(tree_char(v, 4)) == ident_new("'l'"));
   fail_unless(tree_ident(tree_char(v, 5)) == ident_new("'o'"));

   d = search_decls(a, ident_new("B"), 0);
   fail_if(d == NULL);
   fail_unless(tree_kind(d) == T_CONST_DECL);
   fail_unless(tree_ident(d) == ident_new("B"));
   v = tree_value(d);
   fail_unless(tree_kind(v) == T_LITERAL);
   fail_unless(tree_subkind(v) == L_STRING);
   fail_unless(tree_chars(v) == 7);
   fail_unless(tree_ident(tree_char(v, 0)) == ident_new("'\"'"));
   fail_unless(tree_ident(tree_char(v, 1)) == ident_new("'q'"));
   fail_unless(tree_ident(tree_char(v, 2)) == ident_new("'u'"));
   fail_unless(tree_ident(tree_char(v, 3)) == ident_new("'o'"));
   fail_unless(tree_ident(tree_char(v, 4)) == ident_new("'t'"));
   fail_unless(tree_ident(tree_char(v, 5)) == ident_new("'e'"));
   fail_unless(tree_ident(tree_char(v, 6)) == ident_new("'\"'"));

   d = search_decls(a, ident_new("D"), 0);
   fail_if(d == NULL);
   fail_unless(tree_ident(d) == ident_new("D"));
   v = tree_value(d);
   fail_unless(tree_kind(v) == T_LITERAL);
   fail_unless(tree_subkind(v) == L_INT);
   fail_unless(tree_ival(v) == 1000);

   d = search_decls(a, ident_new("E"), 0);
   fail_if(d == NULL);
   fail_unless(tree_ident(d) == ident_new("E"));
   v = tree_value(d);
   fail_unless(tree_kind(v) == T_LITERAL);
   fail_unless(tree_subkind(v) == L_REAL);
   fail_unless(tree_dval(v) == 1.234);

   d = search_decls(a, ident_new("F"), 0);
   fail_if(d == NULL);
   v = tree_value(d);
   fail_unless(tree_kind(v) == T_LITERAL);
   fail_unless(tree_subkind(v) == L_REAL);
   fail_unless(tree_dval(v) == 0.21712);

   d = search_decls(a, ident_new("G"), 0);
   fail_if(d == NULL);
   fail_unless(tree_ident(d) == ident_new("G"));
   v = tree_value(d);
   fail_unless(tree_kind(v) == T_LITERAL);
   fail_unless(tree_subkind(v) == L_REAL);
   fail_unless(tree_dval(v) == 1400000.0);

   d = search_decls(a, ident_new("H"), 0);
   fail_if(d == NULL);
   fail_unless(tree_ident(d) == ident_new("H"));
   v = tree_value(d);
   fail_unless(tree_kind(v) == T_LITERAL);
   fail_unless(tree_subkind(v) == L_REAL);
   fail_unless(tree_dval(v) == 2.351);

   d = search_decls(a, ident_new("I"), 0);
   fail_if(d == NULL);
   fail_unless(tree_ident(d) == ident_new("I"));
   v = tree_value(d);
   fail_unless(tree_kind(v) == T_LITERAL);
   fail_unless(tree_subkind(v) == L_INT);
   fail_unless(tree_ival(v) == 1234);

   d = search_decls(a, ident_new("J"), 0);
   fail_if(d == NULL);
   fail_unless(tree_ident(d) == ident_new("J"));
   v = tree_value(d);
   fail_unless(tree_kind(v) == T_LITERAL);
   fail_unless(tree_subkind(v) == L_REAL);
   fail_unless(tree_dval(v) == 567.123);

   d = search_decls(a, ident_new("K"), 0);
   fail_if(d == NULL);
   fail_unless(tree_ident(d) == ident_new("K"));
   v = tree_value(d);
   fail_unless(tree_kind(v) == T_LITERAL);
   fail_unless(tree_subkind(v) == L_NULL);

   d = search_decls(a, ident_new("L"), 0);
   fail_if(d == NULL);
   fail_unless(tree_kind(d) == T_CONST_DECL);
   fail_unless(tree_ident(d) == ident_new("L"));
   v = tree_value(d);
   fail_unless(tree_kind(v) == T_LITERAL);
   fail_unless(tree_subkind(v) == L_STRING);
   fail_unless(tree_chars(v) == 23);
   fail_unless(tree_ident(tree_char(v, 0)) == ident_new("'S'"));
   fail_unless(tree_ident(tree_char(v, 1)) == ident_new("'e'"));
   fail_unless(tree_ident(tree_char(v, 2)) == ident_new("'t'"));
   fail_unless(tree_ident(tree_char(v, 3)) == ident_new("'u'"));
   fail_unless(tree_ident(tree_char(v, 4)) == ident_new("'p'"));
   fail_unless(tree_ident(tree_char(v, 5)) == ident_new("' '"));
   fail_unless(tree_ident(tree_char(v, 6)) == ident_new("'t'"));
   fail_unless(tree_ident(tree_char(v, 7)) == ident_new("'i'"));
   fail_unless(tree_ident(tree_char(v, 8)) == ident_new("'m'"));
   fail_unless(tree_ident(tree_char(v, 9)) == ident_new("'e'"));
   fail_unless(tree_ident(tree_char(v, 10)) == ident_new("' '"));
   fail_unless(tree_ident(tree_char(v, 11)) == ident_new("'i'"));
   fail_unless(tree_ident(tree_char(v, 12)) == ident_new("'s'"));
   fail_unless(tree_ident(tree_char(v, 13)) == ident_new("' '"));
   fail_unless(tree_ident(tree_char(v, 14)) == ident_new("'t'"));
   fail_unless(tree_ident(tree_char(v, 15)) == ident_new("'o'"));
   fail_unless(tree_ident(tree_char(v, 16)) == ident_new("'o'"));
   fail_unless(tree_ident(tree_char(v, 17)) == ident_new("' '"));
   fail_unless(tree_ident(tree_char(v, 18)) == ident_new("'s'"));
   fail_unless(tree_ident(tree_char(v, 19)) == ident_new("'h'"));
   fail_unless(tree_ident(tree_char(v, 20)) == ident_new("'o'"));
   fail_unless(tree_ident(tree_char(v, 21)) == ident_new("'r'"));
   fail_unless(tree_ident(tree_char(v, 22)) == ident_new("'t'"));

   d = search_decls(a, ident_new("M"), 0);
   fail_if(d == NULL);
   fail_unless(tree_kind(d) == T_CONST_DECL);
   fail_unless(tree_ident(d) == ident_new("M"));
   v = tree_value(d);
   fail_unless(tree_kind(v) == T_LITERAL);
   fail_unless(tree_subkind(v) == L_STRING);
   fail_unless(tree_chars(v) == 0);

   d = search_decls(a, ident_new("N"), 0);
   fail_if(d == NULL);
   fail_unless(tree_kind(d) == T_CONST_DECL);
   fail_unless(tree_ident(d) == ident_new("N"));
   v = tree_value(d);
   fail_unless(tree_kind(v) == T_LITERAL);
   fail_unless(tree_subkind(v) == L_STRING);
   fail_unless(tree_chars(v) == 1);
   fail_unless(tree_ident(tree_char(v, 0)) == ident_new("' '"));

   d = search_decls(a, ident_new("O"), 0);
   fail_if(d == NULL);
   fail_unless(tree_kind(d) == T_CONST_DECL);
   fail_unless(tree_ident(d) == ident_new("O"));
   v = tree_value(d);
   fail_unless(tree_kind(v) == T_LITERAL);
   fail_unless(tree_subkind(v) == L_STRING);
   fail_unless(tree_chars(v) == 1);
   fail_unless(tree_ident(tree_char(v, 0)) == ident_new("'A'"));

   d = search_decls(a, ident_new("P"), 0);
   fail_if(d == NULL);
   fail_unless(tree_kind(d) == T_CONST_DECL);
   fail_unless(tree_ident(d) == ident_new("P"));
   v = tree_value(d);
   fail_unless(tree_kind(v) == T_LITERAL);
   fail_unless(tree_subkind(v) == L_STRING);
   fail_unless(tree_chars(v) == 1);
   fail_unless(tree_ident(tree_char(v, 0)) == ident_new("'\"'"));

   d = search_decls(a, ident_new("Q"), 0);
   fail_if(d == NULL);
   fail_unless(tree_kind(d) == T_CONST_DECL);
   fail_unless(tree_ident(d) == ident_new("Q"));
   v = tree_value(d);
   fail_unless(tree_kind(v) == T_LITERAL);
   fail_unless(tree_subkind(v) == L_STRING);
   fail_unless(tree_chars(v) == 23);
   fail_unless(tree_ident(tree_char(v, 0)) == ident_new("'S'"));
   fail_unless(tree_ident(tree_char(v, 1)) == ident_new("'e'"));
   fail_unless(tree_ident(tree_char(v, 2)) == ident_new("'t'"));
   fail_unless(tree_ident(tree_char(v, 3)) == ident_new("'u'"));
   fail_unless(tree_ident(tree_char(v, 4)) == ident_new("'p'"));
   fail_unless(tree_ident(tree_char(v, 5)) == ident_new("' '"));
   fail_unless(tree_ident(tree_char(v, 6)) == ident_new("'t'"));
   fail_unless(tree_ident(tree_char(v, 7)) == ident_new("'i'"));
   fail_unless(tree_ident(tree_char(v, 8)) == ident_new("'m'"));
   fail_unless(tree_ident(tree_char(v, 9)) == ident_new("'e'"));
   fail_unless(tree_ident(tree_char(v, 10)) == ident_new("' '"));
   fail_unless(tree_ident(tree_char(v, 11)) == ident_new("'i'"));
   fail_unless(tree_ident(tree_char(v, 12)) == ident_new("'s'"));
   fail_unless(tree_ident(tree_char(v, 13)) == ident_new("' '"));
   fail_unless(tree_ident(tree_char(v, 14)) == ident_new("'t'"));
   fail_unless(tree_ident(tree_char(v, 15)) == ident_new("'o'"));
   fail_unless(tree_ident(tree_char(v, 16)) == ident_new("'o'"));
   fail_unless(tree_ident(tree_char(v, 17)) == ident_new("' '"));
   fail_unless(tree_ident(tree_char(v, 18)) == ident_new("'s'"));
   fail_unless(tree_ident(tree_char(v, 19)) == ident_new("'h'"));
   fail_unless(tree_ident(tree_char(v, 20)) == ident_new("'o'"));
   fail_unless(tree_ident(tree_char(v, 21)) == ident_new("'r'"));
   fail_unless(tree_ident(tree_char(v, 22)) == ident_new("'t'"));

   d = search_decls(a, ident_new("R"), 0);
   fail_if(d == NULL);
   fail_unless(tree_kind(d) == T_CONST_DECL);
   fail_unless(tree_ident(d) == ident_new("R"));
   v = tree_value(d);
   fail_unless(tree_kind(v) == T_LITERAL);
   fail_unless(tree_subkind(v) == L_STRING);
   fail_unless(tree_chars(v) == 0);

   d = search_decls(a, ident_new("S"), 0);
   fail_if(d == NULL);
   fail_unless(tree_kind(d) == T_CONST_DECL);
   fail_unless(tree_ident(d) == ident_new("S"));
   v = tree_value(d);
   fail_unless(tree_kind(v) == T_LITERAL);
   fail_unless(tree_subkind(v) == L_STRING);
   fail_unless(tree_chars(v) == 1);
   fail_unless(tree_ident(tree_char(v, 0)) == ident_new("' '"));

   d = search_decls(a, ident_new("T"), 0);
   fail_if(d == NULL);
   fail_unless(tree_kind(d) == T_CONST_DECL);
   fail_unless(tree_ident(d) == ident_new("T"));
   v = tree_value(d);
   fail_unless(tree_kind(v) == T_LITERAL);
   fail_unless(tree_subkind(v) == L_STRING);
   fail_unless(tree_chars(v) == 1);
   fail_unless(tree_ident(tree_char(v, 0)) == ident_new("'A'"));

   d = search_decls(a, ident_new("U"), 0);
   fail_if(d == NULL);
   fail_unless(tree_kind(d) == T_CONST_DECL);
   fail_unless(tree_ident(d) == ident_new("U"));
   v = tree_value(d);
   fail_unless(tree_kind(v) == T_LITERAL);
   fail_unless(tree_subkind(v) == L_STRING);
   fail_unless(tree_chars(v) == 1);
   fail_unless(tree_ident(tree_char(v, 0)) == ident_new("'%'"));

   d = search_decls(a, ident_new("V"), 0);
   fail_if(d == NULL);
   fail_unless(tree_kind(d) == T_CONST_DECL);
   fail_unless(tree_ident(d) == ident_new("V"));
   v = tree_value(d);
   fail_unless(tree_kind(v) == T_LITERAL);
   fail_unless(tree_subkind(v) == L_STRING);
   fail_unless(tree_chars(v) == 1);
   fail_unless(ident_char(tree_ident(tree_char(v, 0)), 1) == (char)0xa9);

   a = parse();
   fail_unless(a == NULL);

   fail_if_errors();
}
END_TEST

START_TEST(test_extended)
{
   tree_t a, e, d, n, s;

   input_from_file(TESTDIR "/parse/extended.vhd");

   e = parse();
   fail_if(e == NULL);
   fail_unless(tree_kind(e) == T_ENTITY);

   a = parse();
   fail_if(a == NULL);
   fail_unless(tree_kind(a) == T_ARCH);
   fail_unless(tree_decls(a) == 7);

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

   fail_unless(tree_stmts(a) == 1);
   s = tree_stmt(a, 0);
   fail_unless(tree_ident(tree_target(s)) == ident_new("\\foo.bar.baz\\"));
   fail_unless(tree_kind(s) == T_CASSIGN);

   a = parse();
   fail_unless(a == NULL);

   fail_if_errors();
}
END_TEST

START_TEST(test_package)
{
   tree_t p, d, c;

   input_from_file(TESTDIR "/parse/package.vhd");

   const error_t expect[] = {
      { 28, "package body may not contain attribute declarations" },
      { LINE_INVALID, "library FOO not found in:" },
      { -1, NULL }
   };
   expect_errors(expect);

   p = parse();
   fail_if(p == NULL);
   fail_unless(tree_kind(p) == T_PACKAGE);
   fail_unless(tree_contexts(p) == 3);
   fail_unless(tree_ident(p) == ident_new("WORK.ONE"));

   p = parse();
   fail_if(p == NULL);
   fail_unless(tree_kind(p) == T_PACKAGE);
   fail_unless(tree_decls(p) == 1);
   fail_unless(tree_contexts(p) == 4);
   fail_unless(tree_ident(tree_context(p, 3)) == ident_new("WORK.ONE"));
   fail_unless(icmp(tree_ident2(tree_context(p, 3)), "all"));
   fail_unless(tree_ident(p) == ident_new("WORK.TWO"));

   p = parse();
   fail_if(p == NULL);
   fail_unless(tree_kind(p) == T_PACK_BODY);
   fail_unless(tree_ident(p) == ident_new("WORK.ONE-body"));
   fail_unless(tree_decls(p) == 4);
   d = tree_decl(p, 0);
   fail_unless(tree_kind(d) == T_FUNC_BODY);
   d = tree_decl(p, 1);
   fail_unless(tree_kind(d) == T_VAR_DECL);
   d = tree_decl(p, 2);
   fail_unless(tree_kind(d) == T_FUNC_DECL);
   d = tree_decl(p, 3);
   fail_unless(tree_kind(d) == T_ATTR_SPEC);

   p = parse();
   fail_if(p == NULL);
   fail_unless(tree_kind(p) == T_PACKAGE);
   fail_unless(tree_decls(p) == 2);
   fail_unless(tree_ident(p) == ident_new("WORK.THREE"));
   d = tree_decl(p, 0);
   fail_unless(tree_kind(d) == T_SIGNAL_DECL);
   d = tree_decl(p, 1);
   fail_unless(tree_kind(d) == T_ALIAS);

   fail_unless(tree_contexts(p) == 4);
   c = tree_context(p, 3);
   fail_unless(tree_kind(c) == T_LIBRARY);
   fail_unless(tree_ident(c) == ident_new("FOO"));

   p = parse();
   fail_unless(p == NULL);

   check_expected_errors();
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

   d = search_decls(p, ident_new("A"), 0);
   fail_if(d == NULL);
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

   d = search_decls(p, ident_new("B"), 0);
   fail_if(d == NULL);
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

   d = search_decls(p, ident_new("C"), 0);
   fail_if(d == NULL);
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

   fail_if_errors();
}
END_TEST

START_TEST(test_qual)
{
   tree_t a, p, s, q, e;

   input_from_file(TESTDIR "/parse/qual.vhd");

   e = parse();
   fail_if(e == NULL);
   fail_unless(tree_kind(e) == T_ENTITY);

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
   fail_unless(tree_ident(q) == ident_new("WORK.BAR(FOO).FOO"));
   e = tree_value(q);
   fail_unless(tree_kind(e) == T_REF);
   fail_unless(tree_ident(e) == ident_new("B"));

   s = tree_stmt(p, 1);
   fail_unless(tree_kind(s) == T_SIGNAL_ASSIGN);
   q = tree_value(tree_waveform(s, 0));
   fail_unless(tree_kind(q) == T_QUALIFIED);
   fail_unless(tree_ident(q) == ident_new("WORK.BAR(FOO).FOO"));
   e = tree_value(q);
   fail_unless(tree_kind(e) == T_REF);
   fail_unless(tree_ident(e) == ident_new("'c'"));

   a = parse();
   fail_unless(a == NULL);

   fail_if_errors();
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
   fail_if(p == NULL);
   fail_unless(tree_kind(p) == T_PACK_BODY);
   fail_unless(tree_decls(p) == 1);

   p = parse();
   fail_unless(p == NULL);

   fail_if_errors();
}
END_TEST

START_TEST(test_array)
{
   tree_t p, d, a, g, s, e, r;
   type_t t, i, b;
   tree_t x;

   input_from_file(TESTDIR "/parse/array.vhd");

   p = parse();
   fail_if(p == NULL);
   fail_unless(tree_kind(p) == T_PACKAGE);

   d = search_decls(p, ident_new("INT_ARRAY"), 0);
   fail_if(d == NULL);
   fail_unless(tree_kind(d) == T_TYPE_DECL);
   fail_unless(tree_ident(d) == ident_new("INT_ARRAY"));
   t = tree_type(d);
   fail_unless(type_kind(t) == T_UARRAY);
   fail_unless(type_index_constrs(t) == 1);
   i = type_index_constr(t, 0);
   fail_unless(type_kind(i) == T_INTEGER);
   fail_unless(type_ident(i) == ident_new("STD.STANDARD.INTEGER"));
   b = type_elem(t);
   fail_unless(type_kind(b) == T_INTEGER);
   fail_unless(type_ident(b) == ident_new("STD.STANDARD.INTEGER"));

   d = search_decls(p, ident_new("TEN_INTS"), 0);
   fail_if(d == NULL);
   fail_unless(tree_kind(d) == T_TYPE_DECL);
   fail_unless(tree_ident(d) == ident_new("TEN_INTS"));
   t = tree_type(d);
   fail_unless(type_kind(t) == T_CARRAY);
   fail_unless(type_dims(t) == 1);

   d = search_decls(p, ident_new("CHAR_COUNTS"), 0);
   fail_if(d == NULL);
   fail_unless(tree_kind(d) == T_TYPE_DECL);
   fail_unless(tree_ident(d) == ident_new("CHAR_COUNTS"));
   t = tree_type(d);
   fail_unless(type_kind(t) == T_CARRAY);
   fail_unless(type_dims(t) == 1);
   r = type_dim(t, 0);
   fail_unless(tree_subkind(r) == RANGE_EXPR);
   fail_unless(tree_kind(tree_value(r)) == T_ATTR_REF);
   fail_unless(tree_ident(tree_value(r)) == ident_new("RANGE"));
   x = tree_name(tree_value(r));
   fail_unless(tree_ident(x) == ident_new("CHARS"));
   fail_unless(tree_kind(x) == T_REF);

   d = search_decls(p, ident_new("TWO_D"), 0);
   fail_if(d == NULL);
   fail_unless(tree_kind(d) == T_TYPE_DECL);
   fail_unless(tree_ident(d) == ident_new("TWO_D"));
   t = tree_type(d);
   fail_unless(type_kind(t) == T_CARRAY);
   fail_unless(type_dims(t) == 2);
   r = type_dim(t, 0);
   fail_unless(tree_ival(tree_left(r)) == 1);
   fail_unless(tree_ival(tree_right(r)) == 3);
   r = type_dim(t, 1);
   fail_unless(tree_ival(tree_left(r)) == 4);
   fail_unless(tree_ival(tree_right(r)) == 6);

   e = parse();
   fail_if(e == NULL);
   fail_unless(tree_kind(e) == T_ENTITY);

   a = parse();
   fail_if(a == NULL);
   fail_unless(tree_kind(a) == T_ARCH);
   fail_unless(tree_decls(a) == 9);

   d = search_decls(a, ident_new("X"), 0);
   fail_if(d == NULL);
   fail_unless(tree_ident(d) == ident_new("X"));
   t = tree_type(d);
   fail_unless(type_kind(t) == T_SUBTYPE);
   fail_unless(type_has_constraint(t));
   r = tree_range(type_constraint(t), 0);
   fail_unless(tree_ival(tree_left(r)) == 1);
   fail_unless(tree_ival(tree_right(r)) == 5);

   d = search_decls(a, ident_new("Y"), 0);
   fail_if(d == NULL);
   fail_unless(tree_ident(d) == ident_new("Y"));
   t = tree_type(d);
   fail_unless(type_kind(t) == T_CARRAY);
   fail_unless(type_ident(t) == ident_new("WORK.P.TEN_INTS"));

   d = search_decls(a, ident_new("Z"), 0);
   fail_if(d == NULL);
   t = tree_type(d);
   fail_unless(type_kind(t) == T_SUBTYPE);
   fail_unless(type_has_constraint(t));
   r = tree_range(type_constraint(t), 0);
   fail_unless(tree_ival(tree_left(r)) == 1);
   fail_unless(tree_ival(tree_right(r)) == 3);
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

   d = search_decls(a, ident_new("N"), 0);
   fail_if(d == NULL);
   t = tree_type(d);
   fail_unless(type_kind(t) == T_SUBTYPE);
   fail_unless(type_has_constraint(t));
   r = tree_range(type_constraint(t), 0);
   fail_unless(tree_ival(tree_left(r)) == 1);
   fail_unless(tree_ival(tree_right(r)) == 3);
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

   d = search_decls(a, ident_new("M"), 0);
   fail_if(d == NULL);
   t = tree_type(d);
   fail_unless(type_kind(t) == T_SUBTYPE);
   fail_unless(type_has_constraint(t));
   r = tree_range(type_constraint(t), 0);
   fail_unless(tree_ival(tree_left(r)) == 1);
   fail_unless(tree_ival(tree_right(r)) == 3);
   fail_unless(tree_has_value(d));
   g = tree_value(d);
   fail_unless(tree_kind(g) == T_AGGREGATE);
   fail_unless(tree_assocs(g) == 1);
   x = tree_assoc(g, 0);
   fail_unless(tree_subkind(x) == A_RANGE);
   r = tree_range(x, 0);
   fail_unless(tree_ival(tree_left(r)) == 1);
   fail_unless(tree_ival(tree_right(r)) == 3);
   fail_unless(tree_ival(tree_value(x)) == 0);

   d = search_decls(a, ident_new("U"), 0);
   fail_if(d == NULL);
   fail_unless(tree_assocs(tree_value(d)) == 4);

   d = search_decls(a, ident_new("V"), 0);
   fail_if(d == NULL);
   fail_unless(tree_assocs(tree_value(d)) == 4);

   p = tree_stmt(a, 0);
   fail_unless(tree_kind(p) == T_PROCESS);
   s = tree_stmt(p, 0);
   e = tree_target(s);
   fail_unless(tree_kind(e) == T_ARRAY_REF);
   fail_unless(tree_params(e) == 1);
   fail_unless(tree_subkind(tree_param(e, 0)) == P_POS);
   fail_unless(tree_pos(tree_param(e, 0)) == 0);
   fail_unless(tree_ival(tree_value(tree_param(e, 0))) == 0);
   s = tree_stmt(p, 1);
   fail_unless(tree_kind(s) == T_SIGNAL_ASSIGN);
   fail_unless(tree_waveforms(s) == 1);
   e = tree_value(tree_waveform(s, 0));
   fail_unless(tree_kind(e) == T_ARRAY_REF);
   s = tree_stmt(p, 3);
   fail_unless(tree_kind(s) == T_SIGNAL_ASSIGN);
   e = tree_target(s);
   fail_unless(tree_kind(e) == T_ARRAY_SLICE);
   e = tree_value(tree_waveform(s, 0));
   fail_unless(tree_kind(e) == T_ARRAY_SLICE);
   s = tree_stmt(p, 4);
   fail_unless(tree_kind(s) == T_VAR_ASSIGN);
   e = tree_value(s);
   fail_unless(tree_kind(e) == T_AGGREGATE);
   fail_unless(tree_assocs(e) == 1);
   fail_unless(tree_subkind(tree_assoc(e, 0)) == A_RANGE);
   s = tree_stmt(p, 5);
   fail_unless(tree_kind(s) == T_VAR_ASSIGN);
   e = tree_value(s);
   fail_unless(tree_kind(e) == T_AGGREGATE);
   fail_unless(tree_assocs(e) == 1);
   fail_unless(tree_subkind(tree_assoc(e, 0)) == A_RANGE);

   p = parse();
   fail_unless(p == NULL);

   fail_if_errors();
}
END_TEST

START_TEST(test_instance)
{
   tree_t e, a, s, c, p;

   input_from_file(TESTDIR "/parse/instance.vhd");

   const error_t expect[] = {
      { 60, "invalid instantiated unit name" },
      { -1, NULL }
   };
   expect_errors(expect);

   e = parse();
   fail_if(e == NULL);
   fail_unless(tree_kind(e) == T_ENTITY);

   e = parse();
   fail_if(e == NULL);
   fail_unless(tree_kind(e) == T_ENTITY);

   a = parse();
   fail_if(a == NULL);
   fail_unless(tree_kind(a) == T_ARCH);

   c = parse();
   fail_if(c == NULL);
   fail_unless(tree_kind(c) == T_CONFIGURATION);

   p = parse();
   fail_if(p == NULL);
   fail_unless(tree_kind(p) == T_PACKAGE);

   a = parse();
   fail_if(a == NULL);
   fail_unless(tree_kind(a) == T_ARCH);
   fail_unless(tree_stmts(a) == 12);

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
   fail_unless(tree_ident(s) == ident_new("C1"));
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

   s = tree_stmt(a, 9);
   fail_unless(tree_kind(s) == T_INSTANCE);
   fail_unless(tree_class(s) == C_COMPONENT);

   s = tree_stmt(a, 10);
   fail_unless(tree_kind(s) == T_INSTANCE);
   fail_unless(tree_class(s) == C_COMPONENT);
   fail_unless(tree_has_ref(s));
   fail_unless(tree_loc(tree_ref(s))->first_line == 18);

   a = parse();
   fail_unless(a == NULL);

   check_expected_errors();
}
END_TEST

START_TEST(test_conc)
{
   tree_t e, a, s, c;

   input_from_file(TESTDIR "/parse/conc.vhd");

   e = parse();
   fail_if(e == NULL);
   fail_unless(tree_kind(e) == T_ENTITY);

   a = parse();
   fail_if(a == NULL);
   fail_unless(tree_kind(a) == T_ARCH);
   fail_unless(tree_stmts(a) == 8);

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

   s = tree_stmt(a, 5);
   fail_unless(tree_kind(s) == T_CASSIGN);
   fail_unless(tree_kind(tree_target(s)) == T_AGGREGATE);
   fail_unless(tree_has_ident(s));

   s = tree_stmt(a, 6);
   fail_unless(tree_kind(s) == T_CPCALL);
   fail_unless(tree_has_ident(s));

   s = tree_stmt(a, 7);
   fail_unless(tree_kind(s) == T_BLOCK);
   fail_unless(tree_generics(s) == 2);
   fail_unless(tree_genmaps(s) == 1);
   fail_unless(tree_ports(s) == 1);
   fail_unless(tree_params(s) == 1);

   a = parse();
   fail_unless(a == NULL);

   fail_if_errors();
}
END_TEST

START_TEST(test_alias)
{
   tree_t e, a, d;

   input_from_file(TESTDIR "/parse/alias.vhd");

   e = parse();
   fail_if(e == NULL);
   fail_unless(tree_kind(e) == T_ENTITY);

   a = parse();
   fail_if(a == NULL);
   fail_unless(tree_kind(a) == T_ARCH);
   fail_unless(tree_stmts(a) == 0);
   fail_unless(tree_decls(a) == 12);

   d = tree_decl(a, 6);
   fail_unless(tree_kind(d) == T_ALIAS);
   fail_unless(tree_ident(d) == ident_new("FOO"));
   fail_unless(tree_kind(tree_value(d)) == T_REF);
   fail_unless(tree_has_type(d));

   d = tree_decl(a, 7);
   fail_unless(tree_kind(d) == T_ALIAS);
   fail_unless(tree_ident(d) == ident_new("BLAH"));
   fail_unless(tree_kind(tree_value(d)) == T_REF);
   fail_unless(tree_has_type(d));

   d = tree_decl(a, 8);
   fail_unless(tree_kind(d) == T_ALIAS);
   fail_unless(tree_ident(d) == ident_new("FUNCI"));
   fail_unless(tree_kind(tree_value(d)) == T_REF);
   fail_unless(tree_has_type(d));
   fail_unless(type_kind(tree_type(d)) == T_FUNC);

   d = tree_decl(a, 9);
   fail_unless(tree_kind(d) == T_ALIAS);
   fail_unless(tree_ident(d) == ident_new("PROCI"));
   fail_unless(tree_kind(tree_value(d)) == T_REF);
   fail_unless(tree_has_type(d));
   fail_unless(type_kind(tree_type(d)) == T_PROC);

   a = parse();
   fail_unless(a == NULL);

   fail_if_errors();
}
END_TEST

START_TEST(test_attr)
{
   tree_t e, a, d, s, r, v;

   input_from_file(TESTDIR "/parse/attr.vhd");

   e = parse();
   fail_if(e == NULL);
   fail_unless(tree_kind(e) == T_ENTITY);

   a = parse();
   fail_if(a == NULL);
   fail_unless(tree_kind(a) == T_ARCH);
   fail_unless(tree_stmts(a) == 1);
   fail_unless(tree_decls(a) == 7);

   d = tree_decl(a, 2);
   fail_unless(tree_kind(d) == T_ATTR_DECL);
   fail_unless(tree_ident(d) == ident_new("FOO"));
   fail_unless(type_kind(tree_type(d)) == T_INTEGER);
   fail_unless(type_ident(tree_type(d)) == ident_new("STD.STANDARD.INTEGER"));

   d = tree_decl(a, 3);
   fail_unless(tree_kind(d) == T_ATTR_SPEC);
   fail_unless(tree_ident(d) == ident_new("FOO"));
   fail_unless(tree_ident2(d) == ident_new("X"));
   fail_unless(tree_kind(tree_value(d)) == T_LITERAL);
   fail_unless(tree_class(d) == C_SIGNAL);

   d = tree_decl(a, 4);
   fail_unless(tree_kind(d) == T_ATTR_SPEC);
   fail_unless(tree_ident(d) == ident_new("FOO"));
   fail_unless(tree_ident2(d) == ident_new("C"));
   fail_unless(tree_kind(tree_value(d)) == T_LITERAL);
   fail_unless(tree_class(d) == C_COMPONENT);

   d = tree_decl(a, 5);
   fail_unless(tree_kind(d) == T_ATTR_SPEC);
   fail_unless(tree_ident(d) == ident_new("FOO"));
   fail_unless(tree_ident2(d) == ident_new("'1'"));
   fail_unless(tree_kind(tree_value(d)) == T_LITERAL);
   fail_unless(tree_class(d) == C_LITERAL);

   d = tree_decl(a, 6);
   fail_unless(tree_kind(d) == T_ATTR_SPEC);
   fail_unless(tree_ident(d) == ident_new("FOO"));
   fail_unless(tree_ident2(d) == ident_new("INTEGER"));
   fail_unless(tree_kind(tree_value(d)) == T_LITERAL);
   fail_unless(tree_class(d) == C_TYPE);

   s = tree_stmt(a, 0);
   fail_unless(tree_kind(s) == T_CASSERT);
   r = tree_value(s);
   fail_unless(tree_kind(r) == T_ARRAY_REF);
   fail_unless(tree_params(r) == 1);
   v = tree_value(r);
   fail_unless(tree_kind(v) == T_ATTR_REF);
   fail_unless(tree_params(v) == 0);

   a = parse();
   fail_unless(a == NULL);

   fail_if_errors();
}
END_TEST

START_TEST(test_procedure)
{
   tree_t p, d;

   input_from_file(TESTDIR "/parse/procedure.vhd");

   p = parse();
   fail_if(p == NULL);
   fail_unless(tree_kind(p) == T_PACKAGE);

   d = search_decls(p, ident_new("FOO"), 0);
   fail_if(d == NULL);
   fail_unless(tree_kind(d) == T_PROC_DECL);
   fail_unless(tree_ports(d) == 2);
   fail_unless(tree_ident(d) == ident_new("FOO"));

   p = parse();
   fail_if(p == NULL);
   fail_unless(tree_kind(p) == T_PACK_BODY);
   fail_unless(tree_decls(p) == 4);

   d = search_decls(p, ident_new("FOO"), 0);
   fail_if(d == NULL);
   fail_unless(tree_kind(d) == T_PROC_BODY);
   fail_unless(tree_ports(d) == 2);
   fail_unless(tree_ident(d) == ident_new("FOO"));
   fail_unless(tree_decls(d) == 1);
   fail_unless(tree_stmts(d) == 1);

   d = search_decls(p, ident_new("BAR"), 0);
   fail_if(d == NULL);
   fail_unless(tree_kind(d) == T_PROC_DECL);
   fail_unless(tree_ports(d) == 1);
   fail_unless(tree_ident(d) == ident_new("BAR"));
   fail_unless(tree_class(tree_port(d, 0)) == C_FILE);

   d = tree_decl(p, 2);
   fail_unless(tree_kind(d) == T_PROC_BODY);
   fail_unless(tree_ports(d) == 0);
   fail_unless(tree_ident(d) == ident_new("BAZ"));

   d = tree_decl(p, 3);
   fail_unless(tree_kind(d) == T_PROC_BODY);
   fail_unless(tree_ports(d) == 0);
   fail_unless(tree_kind(tree_decl(d, 0)) == T_USE);

   p = parse();
   fail_unless(p == NULL);

   fail_if_errors();
}
END_TEST

START_TEST(test_ir1045)
{
   tree_t e, a, s, q, v, c;

   input_from_file(TESTDIR "/parse/ir1045.vhd");

   e = parse();
   fail_if(e == NULL);
   fail_unless(tree_kind(e) == T_ENTITY);

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

   fail_if_errors();
}
END_TEST

START_TEST(test_concat)
{
   tree_t a, s, e;

   input_from_file(TESTDIR "/parse/concat.vhd");

   e = parse();
   fail_if(e == NULL);
   fail_unless(tree_kind(e) == T_ENTITY);

   a = parse();
   fail_unless(tree_kind(a) == T_ARCH);

   s = tree_stmt(a, 0);
   fail_unless(tree_kind(s) == T_CASSIGN);
   e = tree_value(tree_waveform(tree_cond(s, 0), 0));
   fail_unless(tree_kind(e) == T_FCALL);
   fail_unless(tree_params(e) == 2);

   a = parse();
   fail_unless(a == NULL);

   fail_if_errors();
}
END_TEST

START_TEST(test_based)
{
   tree_t p, d;

   input_from_file(TESTDIR "/parse/based.vhd");

   p = parse();
   fail_if(p == NULL);
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

   d = tree_decl(p, 7);
   fail_unless(tree_ival(tree_value(d)) == 0xabababab);

   d = tree_decl(p, 8);
   fail_unless(tree_ival(tree_value(d)) == 0x1a);

   d = tree_decl(p, 9);
   fail_unless(tree_ival(tree_value(d)) == 255);

   d = tree_decl(p, 10);
   fail_unless(tree_ival(tree_value(d)) == 255);

   d = tree_decl(p, 11);
   fail_unless(tree_ival(tree_value(d)) == 255);

   d = tree_decl(p, 12);
   fail_unless(tree_ival(tree_value(d)) == 224);

   d = tree_decl(p, 13);
   fail_unless(tree_ival(tree_value(d)) == 224);

   d = tree_decl(p, 14);
   fail_unless(tree_dval(tree_value(d)) == 4095.0);

   d = tree_decl(p, 15);
   fail_unless(tree_dval(tree_value(d)) == 4095.0);

   d = tree_decl(p, 16);
   fail_unless(tree_ival(tree_value(d)) == 224);

   d = tree_decl(p, 17);
   fail_unless(tree_dval(tree_value(d)) == 4095.0);

   p = parse();
   fail_unless(p == NULL);

   fail_if_errors();
}
END_TEST

START_TEST(test_bitstring)
{
   tree_t p, a;

   input_from_file(TESTDIR "/parse/bitstring.vhd");

   const error_t expect[] = {
      { 22, "invalid digit '9' in bit string" },
      { -1, NULL }
   };
   expect_errors(expect);

   p = parse();
   fail_if(p == NULL);
   fail_unless(tree_kind(p) == T_PACKAGE);

   ident_t one = ident_new("'1'");
   ident_t zero = ident_new("'0'");

   a = tree_value(tree_decl(p, 0));
   fail_unless(tree_kind(a) == T_LITERAL);
   fail_unless(tree_subkind(a) == L_STRING);
   fail_unless(tree_chars(a) == 16);
   fail_unless(tree_ident(tree_char(a, 0)) == zero);
   fail_unless(tree_ident(tree_char(a, 1)) == zero);
   fail_unless(tree_ident(tree_char(a, 2)) == zero);
   fail_unless(tree_ident(tree_char(a, 3)) == one);
   fail_unless(tree_ident(tree_char(a, 4)) == zero);
   fail_unless(tree_ident(tree_char(a, 5)) == zero);
   fail_unless(tree_ident(tree_char(a, 6)) == one);
   fail_unless(tree_ident(tree_char(a, 7)) == zero);
   fail_unless(tree_ident(tree_char(a, 8)) == zero);
   fail_unless(tree_ident(tree_char(a, 9)) == zero);
   fail_unless(tree_ident(tree_char(a, 10)) == one);
   fail_unless(tree_ident(tree_char(a, 11)) == one);
   fail_unless(tree_ident(tree_char(a, 12)) == zero);
   fail_unless(tree_ident(tree_char(a, 13)) == one);
   fail_unless(tree_ident(tree_char(a, 14)) == zero);
   fail_unless(tree_ident(tree_char(a, 15)) == zero);

   a = tree_value(tree_decl(p, 1));
   fail_unless(tree_kind(a) == T_LITERAL);
   fail_unless(tree_subkind(a) == L_STRING);
   fail_unless(tree_chars(a) == 12);
   fail_unless(tree_ident(tree_char(a, 0)) == zero);
   fail_unless(tree_ident(tree_char(a, 1)) == zero);
   fail_unless(tree_ident(tree_char(a, 2)) == one);
   fail_unless(tree_ident(tree_char(a, 3)) == zero);
   fail_unless(tree_ident(tree_char(a, 4)) == one);
   fail_unless(tree_ident(tree_char(a, 5)) == zero);
   fail_unless(tree_ident(tree_char(a, 6)) == zero);
   fail_unless(tree_ident(tree_char(a, 7)) == one);
   fail_unless(tree_ident(tree_char(a, 8)) == one);
   fail_unless(tree_ident(tree_char(a, 9)) == one);
   fail_unless(tree_ident(tree_char(a, 10)) == zero);
   fail_unless(tree_ident(tree_char(a, 11)) == zero);

   a = tree_value(tree_decl(p, 2));
   fail_unless(tree_kind(a) == T_LITERAL);
   fail_unless(tree_subkind(a) == L_STRING);
   fail_unless(tree_chars(a) == 8);
   fail_unless(tree_ident(tree_char(a, 0)) == one);
   fail_unless(tree_ident(tree_char(a, 1)) == zero);
   fail_unless(tree_ident(tree_char(a, 2)) == one);
   fail_unless(tree_ident(tree_char(a, 3)) == zero);
   fail_unless(tree_ident(tree_char(a, 4)) == one);
   fail_unless(tree_ident(tree_char(a, 5)) == zero);
   fail_unless(tree_ident(tree_char(a, 6)) == one);
   fail_unless(tree_ident(tree_char(a, 7)) == one);

   a = tree_value(tree_decl(p, 3));
   fail_unless(tree_kind(a) == T_LITERAL);
   fail_unless(tree_subkind(a) == L_STRING);
   fail_unless(tree_chars(a) == 3);
   fail_unless(tree_ident(tree_char(a, 0)) == one);
   fail_unless(tree_ident(tree_char(a, 1)) == zero);
   fail_unless(tree_ident(tree_char(a, 2)) == one);

   a = tree_value(tree_decl(p, 4));
   fail_unless(tree_kind(a) == T_LITERAL);
   fail_unless(tree_subkind(a) == L_STRING);
   fail_unless(tree_chars(a) == 4);
   fail_unless(tree_ident(tree_char(a, 0)) == one);
   fail_unless(tree_ident(tree_char(a, 1)) == one);
   fail_unless(tree_ident(tree_char(a, 2)) == one);
   fail_unless(tree_ident(tree_char(a, 3)) == one);

   a = tree_value(tree_decl(p, 5));
   fail_unless(tree_kind(a) == T_LITERAL);
   fail_unless(tree_subkind(a) == L_STRING);
   fail_unless(tree_chars(a) == 8);
   fail_unless(tree_ident(tree_char(a, 0)) == one);
   fail_unless(tree_ident(tree_char(a, 1)) == zero);
   fail_unless(tree_ident(tree_char(a, 2)) == one);
   fail_unless(tree_ident(tree_char(a, 3)) == zero);
   fail_unless(tree_ident(tree_char(a, 4)) == one);
   fail_unless(tree_ident(tree_char(a, 5)) == zero);
   fail_unless(tree_ident(tree_char(a, 6)) == one);
   fail_unless(tree_ident(tree_char(a, 7)) == one);

   a = tree_value(tree_decl(p, 6));
   fail_unless(tree_kind(a) == T_LITERAL);
   fail_unless(tree_subkind(a) == L_STRING);
   fail_unless(tree_chars(a) == 12);
   fail_unless(tree_ident(tree_char(a, 0)) == one);
   fail_unless(tree_ident(tree_char(a, 1)) == one);
   fail_unless(tree_ident(tree_char(a, 2)) == one);
   fail_unless(tree_ident(tree_char(a, 3)) == one);
   fail_unless(tree_ident(tree_char(a, 4)) == one);
   fail_unless(tree_ident(tree_char(a, 5)) == one);
   fail_unless(tree_ident(tree_char(a, 6)) == one);
   fail_unless(tree_ident(tree_char(a, 7)) == one);
   fail_unless(tree_ident(tree_char(a, 8)) == one);
   fail_unless(tree_ident(tree_char(a, 9)) == one);
   fail_unless(tree_ident(tree_char(a, 10)) == one);
   fail_unless(tree_ident(tree_char(a, 11)) == one);

   a = tree_value(tree_decl(p, 7));
   fail_unless(tree_kind(a) == T_LITERAL);
   fail_unless(tree_subkind(a) == L_STRING);
   fail_unless(tree_chars(a) == 12);
   fail_unless(tree_ident(tree_char(a, 0)) == one);
   fail_unless(tree_ident(tree_char(a, 1)) == one);
   fail_unless(tree_ident(tree_char(a, 2)) == one);
   fail_unless(tree_ident(tree_char(a, 3)) == one);
   fail_unless(tree_ident(tree_char(a, 4)) == one);
   fail_unless(tree_ident(tree_char(a, 5)) == one);
   fail_unless(tree_ident(tree_char(a, 6)) == one);
   fail_unless(tree_ident(tree_char(a, 7)) == one);
   fail_unless(tree_ident(tree_char(a, 8)) == one);
   fail_unless(tree_ident(tree_char(a, 9)) == one);
   fail_unless(tree_ident(tree_char(a, 10)) == one);
   fail_unless(tree_ident(tree_char(a, 11)) == one);

   a = tree_value(tree_decl(p, 8));
   fail_unless(tree_kind(a) == T_LITERAL);
   fail_unless(tree_subkind(a) == L_STRING);
   fail_unless(tree_chars(a) == 9);
   fail_unless(tree_ident(tree_char(a, 0)) == one);
   fail_unless(tree_ident(tree_char(a, 1)) == one);
   fail_unless(tree_ident(tree_char(a, 2)) == one);
   fail_unless(tree_ident(tree_char(a, 3)) == one);
   fail_unless(tree_ident(tree_char(a, 4)) == one);
   fail_unless(tree_ident(tree_char(a, 5)) == one);
   fail_unless(tree_ident(tree_char(a, 6)) == one);
   fail_unless(tree_ident(tree_char(a, 7)) == one);
   fail_unless(tree_ident(tree_char(a, 8)) == one);

   a = tree_value(tree_decl(p, 9));
   fail_unless(tree_kind(a) == T_LITERAL);
   fail_unless(tree_subkind(a) == L_STRING);
   fail_unless(tree_chars(a) == 12);
   fail_unless(tree_ident(tree_char(a, 0)) == zero);
   fail_unless(tree_ident(tree_char(a, 1)) == one);
   fail_unless(tree_ident(tree_char(a, 2)) == one);
   fail_unless(tree_ident(tree_char(a, 3)) == one);
   fail_unless(tree_ident(tree_char(a, 4)) == zero);
   fail_unless(tree_ident(tree_char(a, 5)) == one);
   fail_unless(tree_ident(tree_char(a, 6)) == one);
   fail_unless(tree_ident(tree_char(a, 7)) == one);
   fail_unless(tree_ident(tree_char(a, 8)) == zero);
   fail_unless(tree_ident(tree_char(a, 9)) == one);
   fail_unless(tree_ident(tree_char(a, 10)) == one);
   fail_unless(tree_ident(tree_char(a, 11)) == one);

   a = tree_value(tree_decl(p, 10));
   fail_unless(tree_kind(a) == T_LITERAL);
   fail_unless(tree_subkind(a) == L_STRING);
   fail_unless(tree_chars(a) == 12);
   fail_unless(tree_ident(tree_char(a, 0)) == one);
   fail_unless(tree_ident(tree_char(a, 1)) == one);
   fail_unless(tree_ident(tree_char(a, 2)) == one);
   fail_unless(tree_ident(tree_char(a, 3)) == one);
   fail_unless(tree_ident(tree_char(a, 4)) == one);
   fail_unless(tree_ident(tree_char(a, 5)) == one);
   fail_unless(tree_ident(tree_char(a, 6)) == one);
   fail_unless(tree_ident(tree_char(a, 7)) == one);
   fail_unless(tree_ident(tree_char(a, 8)) == one);
   fail_unless(tree_ident(tree_char(a, 9)) == one);
   fail_unless(tree_ident(tree_char(a, 10)) == one);
   fail_unless(tree_ident(tree_char(a, 11)) == one);

   a = tree_value(tree_decl(p, 11));
   fail_unless(tree_kind(a) == T_LITERAL);
   fail_unless(tree_subkind(a) == L_STRING);
   fail_unless(tree_chars(a) == 12);
   fail_unless(tree_ident(tree_char(a, 0)) == one);
   fail_unless(tree_ident(tree_char(a, 1)) == one);
   fail_unless(tree_ident(tree_char(a, 2)) == one);
   fail_unless(tree_ident(tree_char(a, 3)) == one);
   fail_unless(tree_ident(tree_char(a, 4)) == one);
   fail_unless(tree_ident(tree_char(a, 5)) == one);
   fail_unless(tree_ident(tree_char(a, 6)) == one);
   fail_unless(tree_ident(tree_char(a, 7)) == one);
   fail_unless(tree_ident(tree_char(a, 8)) == one);
   fail_unless(tree_ident(tree_char(a, 9)) == one);
   fail_unless(tree_ident(tree_char(a, 10)) == one);
   fail_unless(tree_ident(tree_char(a, 11)) == one);

   a = tree_value(tree_decl(p, 12));
   fail_unless(tree_kind(a) == T_LITERAL);
   fail_unless(tree_subkind(a) == L_STRING);
   fail_unless(tree_chars(a) == 9);
   fail_unless(tree_ident(tree_char(a, 0)) == one);
   fail_unless(tree_ident(tree_char(a, 1)) == one);
   fail_unless(tree_ident(tree_char(a, 2)) == one);
   fail_unless(tree_ident(tree_char(a, 3)) == one);
   fail_unless(tree_ident(tree_char(a, 4)) == one);
   fail_unless(tree_ident(tree_char(a, 5)) == one);
   fail_unless(tree_ident(tree_char(a, 6)) == one);
   fail_unless(tree_ident(tree_char(a, 7)) == one);
   fail_unless(tree_ident(tree_char(a, 8)) == one);

   a = tree_value(tree_decl(p, 13));
   fail_unless(tree_kind(a) == T_LITERAL);
   fail_unless(tree_subkind(a) == L_STRING);
   fail_unless(tree_chars(a) == 12);
   fail_unless(tree_ident(tree_char(a, 0)) == zero);
   fail_unless(tree_ident(tree_char(a, 1)) == one);
   fail_unless(tree_ident(tree_char(a, 2)) == one);
   fail_unless(tree_ident(tree_char(a, 3)) == one);
   fail_unless(tree_ident(tree_char(a, 4)) == zero);
   fail_unless(tree_ident(tree_char(a, 5)) == one);
   fail_unless(tree_ident(tree_char(a, 6)) == one);
   fail_unless(tree_ident(tree_char(a, 7)) == one);
   fail_unless(tree_ident(tree_char(a, 8)) == zero);
   fail_unless(tree_ident(tree_char(a, 9)) == one);
   fail_unless(tree_ident(tree_char(a, 10)) == one);
   fail_unless(tree_ident(tree_char(a, 11)) == one);

   p = parse();
   fail_if(p == NULL);

   check_expected_errors();
}
END_TEST

START_TEST(test_block)
{
   tree_t e, a, b;

   input_from_file(TESTDIR "/parse/block.vhd");

   e = parse();
   fail_if(e == NULL);
   fail_unless(tree_kind(e) == T_ENTITY);

   a = parse();
   fail_if(a == NULL);
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

   fail_if_errors();
}
END_TEST

START_TEST(test_comp)
{
   tree_t p, c;

   input_from_file(TESTDIR "/parse/comp.vhd");

   p = parse();
   fail_if(p == NULL);
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

   fail_if_errors();
}
END_TEST

START_TEST(test_generate)
{
   tree_t e, a, g, i;

   input_from_file(TESTDIR "/parse/generate.vhd");

   e = parse();
   fail_if(e == NULL);
   fail_unless(tree_kind(e) == T_ENTITY);

   a = parse();
   fail_if(a == NULL);
   fail_unless(tree_kind(a) == T_ARCH);
   fail_unless(tree_stmts(a) == 7);

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
   fail_unless(tree_decls(g) == 2);
   fail_unless(tree_stmts(g) == 1);
   fail_unless(icmp(tree_ident(g), "G3"));

   i = tree_decl(g, 0);
   fail_unless(tree_kind(i) == T_GENVAR);
   fail_unless(icmp(tree_ident(i), "I"));
   fail_unless(tree_flags(i) & TREE_F_LOOP_VAR);

   g = tree_stmt(a, 3);
   fail_unless(tree_kind(g) == T_FOR_GENERATE);
   fail_unless(tree_decls(g) == 1);
   fail_unless(tree_stmts(g) == 0);
   fail_unless(icmp(tree_ident(g), "G4"));

   i = tree_decl(g, 0);
   fail_unless(tree_kind(i) == T_GENVAR);
   fail_unless(icmp(tree_ident(i), "I"));
   fail_unless(tree_flags(i) & TREE_F_LOOP_VAR);

   g = tree_stmt(a, 4);
   fail_unless(tree_kind(g) == T_FOR_GENERATE);
   fail_unless(tree_decls(g) == 1);
   fail_unless(tree_stmts(g) == 0);
   fail_unless(icmp(tree_ident(g), "G5"));

   a = parse();
   fail_unless(a == NULL);

   fail_if_errors();
}
END_TEST

START_TEST(test_access)
{
   tree_t e, a, p, s;

   input_from_file(TESTDIR "/parse/access.vhd");

   e = parse();
   fail_if(e == NULL);
   fail_unless(tree_kind(e) == T_ENTITY);

   a = parse();
   fail_if(a == NULL);
   fail_unless(tree_kind(a) == T_ARCH);
   fail_unless(tree_stmts(a) == 1);

   p = tree_stmt(a, 0);
   fail_unless(tree_kind(p) == T_PROCESS);
   fail_unless(tree_stmts(p) == 5);

   s = tree_stmt(p, 0);
   fail_unless(tree_kind(tree_target(s)) == T_ALL);

   s = tree_stmt(p, 2);
   fail_unless(tree_kind(tree_value(s)) == T_NEW);

   s = tree_stmt(p, 3);
   fail_unless(tree_kind(tree_value(s)) == T_ARRAY_SLICE);
   fail_unless(tree_kind(tree_value(tree_value(s))) == T_ALL);

   s = tree_stmt(p, 4);
   fail_unless(tree_kind(tree_value(s)) == T_ARRAY_REF);
   fail_unless(tree_kind(tree_value(tree_value(s))) == T_ALL);

   a = parse();
   fail_unless(a == NULL);

   fail_if_errors();
}
END_TEST

START_TEST(test_spec)
{
   tree_t a, d, b, e, c;

   input_from_file(TESTDIR "/parse/spec.vhd");

   e = parse();
   fail_if(e == NULL);
   fail_unless(tree_kind(e) == T_ENTITY);

   e = parse();
   fail_if(e == NULL);
   fail_unless(tree_kind(e) == T_ENTITY);

   a = parse();
   fail_if(a == NULL);
   fail_unless(tree_kind(a) == T_ARCH);

   c = parse();
   fail_if(c == NULL);
   fail_unless(tree_kind(c) == T_CONFIGURATION);

   a = parse();
   fail_if(a == NULL);
   fail_unless(tree_kind(a) == T_ARCH);
   fail_unless(tree_stmts(a) == 5);
   fail_unless(tree_decls(a) == 9);

   d = tree_decl(a, 2);
   fail_unless(tree_kind(d) == T_SPEC);
   fail_unless(tree_ident(d) == ident_new("X"));
   fail_unless(tree_ident2(d) == ident_new("Y"));
   b = tree_value(d);
   fail_unless(tree_kind(b) == T_BINDING);
   fail_unless(tree_class(b) == C_ENTITY);
   fail_unless(tree_ident(b) == ident_new("WORK.FOO"));

   d = tree_decl(a, 3);
   fail_unless(tree_kind(d) == T_SPEC);
   fail_unless(tree_ident(d) == ident_new("X1"));
   fail_unless(tree_ident2(d) == ident_new("Y"));
   b = tree_value(d);
   fail_unless(tree_kind(b) == T_BINDING);
   fail_unless(tree_class(b) == C_ENTITY);
   fail_unless(tree_ident(b) == ident_new("WORK.FOO"));

   d = tree_decl(a, 4);
   fail_unless(tree_kind(d) == T_SPEC);
   fail_unless(tree_ident(d) == ident_new("X2"));
   fail_unless(tree_ident2(d) == ident_new("Y"));
   b = tree_value(d);
   fail_unless(tree_kind(b) == T_BINDING);
   fail_unless(tree_class(b) == C_ENTITY);
   fail_unless(tree_ident(b) == ident_new("WORK.FOO"));

   d = tree_decl(a, 5);
   fail_unless(tree_kind(d) == T_SPEC);
   fail_unless(tree_ident(d) == ident_new("X3"));
   fail_unless(tree_ident2(d) == ident_new("Y"));
   b = tree_value(d);
   fail_unless(tree_kind(b) == T_BINDING);
   fail_unless(tree_class(b) == C_ENTITY);
   fail_unless(tree_ident(b) == ident_new("WORK.FOO"));
   fail_unless(tree_ident2(b) == ident_new("BAR"));

   d = tree_decl(a, 6);
   fail_unless(tree_kind(d) == T_SPEC);
   fail_unless(tree_ident(d) == ident_new("X4"));
   fail_unless(tree_ident2(d) == ident_new("Y"));
   b = tree_value(d);
   fail_unless(tree_kind(b) == T_BINDING);
   fail_unless(tree_class(b) == C_ENTITY);
   fail_unless(tree_ident(b) == ident_new("WORK.FOO"));
   fail_unless(tree_ident2(b) == ident_new("BAR"));
   fail_unless(tree_genmaps(b) == 1);
   fail_unless(tree_params(b) == 1);

   d = tree_decl(a, 7);
   fail_unless(tree_kind(d) == T_SPEC);
   fail_unless(tree_ident(d) == ident_new("all"));
   fail_unless(tree_ident2(d) == ident_new("P"));
   b = tree_value(d);
   fail_unless(tree_kind(b) == T_BINDING);
   fail_unless(tree_class(b) == C_CONFIGURATION);
   fail_unless(tree_ident(b) == ident_new("WORK.YAH"));

   d = tree_decl(a, 8);
   fail_unless(tree_kind(d) == T_SPEC);
   fail_if(tree_has_ident(d));
   fail_unless(tree_ident2(d) == ident_new("Y"));
   fail_if(tree_has_value(d));

   a = parse();
   fail_unless(a == NULL);

   fail_if_errors();
}
END_TEST

START_TEST(test_loc)
{
   tree_t a, s, p, e;
   const loc_t *l;

   input_from_file(TESTDIR "/parse/loc.vhd");

   e = parse();
   fail_if(e == NULL);
   fail_unless(tree_kind(e) == T_ENTITY);

   a = parse();
   fail_if(a == NULL);

   s = tree_stmt(a, 0);
   fail_unless(tree_kind(s) == T_CPCALL);
   l = tree_loc(s);
   fail_unless(l->first_line == 7);
   fail_unless(l->line_delta == 0);
   fail_unless(l->first_column == 4);
   fail_unless(l->column_delta == 14);

   p = tree_param(s, 0);
   l = tree_loc(p);
   fail_unless(l->first_line == 7);
   fail_unless(l->line_delta == 0);
   fail_unless(l->first_column == 6);
   fail_unless(l->column_delta == 0);

   p = tree_param(s, 2);
   l = tree_loc(p);
   fail_unless(l->first_line == 7);
   fail_unless(l->line_delta == 0);
   fail_unless(l->first_column == 12);
   fail_unless(l->column_delta == 4);

   s = tree_stmt(a, 1);
   fail_unless(tree_kind(s) == T_CASSERT);
   l = tree_loc(s);
   fail_unless(l->first_line == 8);
   fail_unless(l->line_delta == 0);
   fail_unless(l->first_column == 4);
   fail_unless(l->column_delta == 15);

   e = tree_value(s);
   fail_unless(tree_kind(e) == T_ATTR_REF);
   l = tree_loc(e);
   fail_unless(l->first_line == 8);
   fail_unless(l->line_delta == 0);
   fail_unless(l->first_column == 11);
   fail_unless(l->column_delta == 7);

   a = parse();
   fail_unless(a == NULL);

   fail_if_errors();
}
END_TEST

START_TEST(test_expr)
{
   tree_t a, p, e;

   input_from_file(TESTDIR "/parse/expr.vhd");

   e = parse();
   fail_if(e == NULL);
   fail_unless(tree_kind(e) == T_ENTITY);

   p = parse();
   fail_if(p == NULL);
   fail_unless(tree_kind(p) == T_PACKAGE);

   a = parse();
   fail_if(a == NULL);
   fail_unless(tree_kind(a) == T_ARCH);
   fail_unless(tree_stmts(a) == 1);

   p = tree_stmt(a, 0);
   fail_unless(tree_kind(p) == T_PROCESS);
   fail_unless(tree_stmts(p) == 13);

   e = tree_value(tree_stmt(p, 0));
   fail_unless(tree_kind(e) == T_FCALL);
   fail_unless(tree_ident(e) == ident_new("\"not\""));
   fail_unless(tree_params(e) == 1);

   e = tree_value(tree_stmt(p, 1));
   fail_unless(tree_kind(e) == T_FCALL);
   fail_unless(tree_ident(e) == ident_new("\"abs\""));
   fail_unless(tree_params(e) == 1);

   e = tree_value(tree_stmt(p, 2));
   fail_unless(tree_kind(e) == T_FCALL);
   fail_unless(tree_ident(e) == ident_new("\"**\""));
   fail_unless(tree_params(e) == 2);

   e = tree_value(tree_stmt(p, 3));
   fail_unless(tree_kind(e) == T_RECORD_REF);
   fail_unless(tree_ident(e) == ident_new("Z"));
   fail_unless(tree_kind(tree_value(e)) == T_FCALL);
   fail_unless(tree_has_ref(e));
   fail_unless(tree_kind(tree_ref(e)) == T_FIELD_DECL);
   fail_unless(tree_ident(tree_ref(e)) == ident_new("Z"));

   const char *shift_ops[] = { "sll", "srl", "sla", "sra", "rol", "ror" };

   for (size_t i = 0; i < ARRAY_LEN(shift_ops); i++) {
      char buf[16];
      snprintf(buf, sizeof(buf), "\"%s\"", shift_ops[i]);

      e = tree_value(tree_stmt(p, 4 + i));
      fail_unless(tree_kind(e) == T_FCALL);
      fail_unless(tree_ident(e) == ident_new(buf));
      fail_unless(tree_params(e) == 2);
   }

   e = tree_value(tree_stmt(p, 10));
   fail_unless(tree_kind(e) == T_FCALL);
   fail_unless(tree_ident(e) == ident_new("WORK.FOO.\"and\""));

   e = tree_target(tree_stmt(p, 11));
   fail_unless(tree_kind(e) == T_ARRAY_SLICE);

   e = tree_value(tree_stmt(p, 12));
   fail_unless(tree_kind(e) == T_AGGREGATE);
   fail_unless(tree_assocs(e) == 2);
   fail_unless(tree_subkind(tree_assoc(e, 0)) == A_NAMED);
   fail_unless(tree_subkind(tree_assoc(e, 1)) == A_RANGE);

   a = parse();
   fail_unless(a == NULL);

   fail_if_errors();
}
END_TEST

START_TEST(test_error)
{
   tree_t e, a;

   input_from_file(TESTDIR "/parse/error.vhd");

   e = parse();
   fail_if(e == NULL);
   fail_unless(tree_kind(e) == T_ENTITY);

   const error_t expect[] = {
      {  7, "unexpected identifier while parsing concurrent procedure call "
         "statement, expecting one of postponed or ;" },
      {  7, "no visible subprogram declaration for BAD" },
      { 11, "unexpected identifier while parsing concurrent procedure call "
         "statement, expecting one of postponed or ;" },
      { 11, "no visible subprogram declaration for SOME" },
      { 11, "no visible subprogram declaration for BAD" },
      { 17, "unexpected ; while parsing process statement, expecting process" },
      { 23, "expected trailing process statement label to match FOO" },
      { 27, "trailing label for process statement without label" },
      { 34, "expected trailing if statement label to match MY_IF" },
      { 36, "expected trailing subprogram body label to match \"+\"" },
      { 41, "P1 already declared in this region" },
      { 40, "previous declaration of P1 was here" },
      { 44, "A1 already declared in this region" },
      { 43, "previous declaration of A1 was here" },
      { 47, "S1 already declared in this region" },
      { 46, "previous declaration of S1 was here" },
      { 50, "B1 already declared in this region" },
      { 49, "previous declaration of B1 was here" },
      { 56, "C1 already declared in this region" },
      { 55, "previous declaration of C1 was here" },
      { 59, "NOT_A_LIBRARY does not name a visible component or design unit" },
      { 64, "missing declaration for entity WORK.NOT_HERE" },
      { -1, NULL }
   };
   expect_errors(expect);

   a = parse();
   fail_if(a == NULL);

   a = parse();
   fail_if(a == NULL);

   check_expected_errors();
}
END_TEST

START_TEST(test_config)
{
   tree_t e, c, s, b, a;

   input_from_file(TESTDIR "/parse/config.vhd");

   const error_t expect[] = {
      { 29, "no visible declaration for X" },
      { 38, "CONF does not name an entity in library WORK" },
      { 45, "cannot find unit WORK.ENT-BAD" },
      { 52, "P is not a block that can be configured" },
      { 55, "object P is not an instance" },
      { -1, NULL }
   };
   expect_errors(expect);

   e = parse();
   fail_if(e == NULL);
   fail_unless(tree_kind(e) == T_ENTITY);

   e = parse();
   fail_if(e == NULL);
   fail_unless(tree_kind(e) == T_ENTITY);

   a = parse();
   fail_if(a == NULL);
   fail_unless(tree_kind(a) == T_ARCH);

   a = parse();
   fail_if(a == NULL);
   fail_unless(tree_kind(a) == T_ARCH);

   c = parse();
   fail_if(c == NULL);
   fail_unless(tree_kind(c) == T_CONFIGURATION);

   c = parse();
   fail_if(c == NULL);
   fail_unless(tree_kind(c) == T_CONFIGURATION);
   fail_unless(tree_ident(c) == ident_new("WORK.CONF"));
   fail_unless(tree_ident2(c) == ident_new("WORK.ENT"));
   fail_unless(tree_decls(c) == 3);
   fail_unless(tree_kind(tree_decl(c, 0)) == T_USE);
   fail_unless(tree_kind(tree_decl(c, 1)) == T_ATTR_SPEC);

   b = tree_decl(c, 2);
   fail_unless(tree_kind(b) == T_BLOCK_CONFIG);
   fail_unless(tree_ident(b) == ident_new("ARCH"));
   fail_unless(tree_decls(b) == 2);

   s = tree_decl(b, 0);
   fail_unless(tree_kind(s) == T_SPEC);
   fail_unless(tree_ident(s) == ident_new("all"));
   fail_unless(tree_ident2(s) == ident_new("COMP"));

   c = parse();
   fail_if(c == NULL);
   fail_unless(tree_kind(c) == T_CONFIGURATION);

   c = parse();
   fail_if(c == NULL);
   fail_unless(tree_kind(c) == T_CONFIGURATION);

   c = parse();
   fail_if(c == NULL);
   fail_unless(tree_kind(c) == T_CONFIGURATION);

   c = parse();
   fail_unless(c == NULL);

   check_expected_errors();
}
END_TEST

START_TEST(test_protected)
{
   tree_t p, d, s;
   type_t t;

   set_standard(STD_00);

   input_from_file(TESTDIR "/parse/protected.vhd");

   p = parse();
   fail_if(p == NULL);
   fail_unless(tree_kind(p) == T_PACKAGE);

   d = search_decls(p, ident_new("SHAREDCOUNTER"), 0);
   fail_if(d == NULL);
   fail_unless(tree_kind(d) == T_TYPE_DECL);
   t = tree_type(d);
   fail_unless(type_kind(t) == T_PROTECTED);
   fail_unless(type_decls(t) == 3);

   s = type_decl(t, 0);
   fail_unless(tree_kind(s) == T_PROC_DECL);
   fail_unless(tree_ident(s) == ident_new("INCREMENT"));
   fail_unless(tree_flags(s) & TREE_F_PROTECTED);

   d = search_decls(p, ident_new("SHAREDCOUNTER"), 1);
   fail_if(d == NULL);
   fail_unless(tree_kind(d) == T_PROT_BODY);
   fail_unless(tree_decls(d) == 5);

   d = search_decls(d, ident_new("ADD10"), 0);
   fail_if(d == NULL);
   fail_unless(tree_kind(d) == T_PROC_BODY);
   fail_unless(tree_flags(d) & TREE_F_PROTECTED);

   s = tree_stmt(d, 0);
   fail_unless(tree_kind(s) == T_PROT_PCALL);

   p = parse();
   fail_unless(p == NULL);

   fail_if_errors();
}
END_TEST

START_TEST(test_empty)
{
   input_from_file(TESTDIR "/parse/empty.vhd");

   fail_unless(parse() == NULL);
}
END_TEST

START_TEST(test_issue205)
{
   input_from_file(TESTDIR "/parse/issue205.vhd");

   tree_t e = parse();
   fail_if(e == NULL);
   fail_unless(tree_kind(e) == T_ENTITY);

   tree_t a = parse();
   fail_if(a == NULL);
   fail_unless(tree_kind(a) == T_ARCH);

   tree_t s = tree_message(tree_stmt(tree_stmt(a, 0), 0));
   fail_unless(tree_kind(s) == T_LITERAL);
   fail_unless(tree_chars(s) == 2);
   fail_unless(tree_ident(tree_char(s, 0)) == ident_new("'\"'"));
   fail_unless(tree_ident(tree_char(s, 1)) == ident_new("'\"'"));

   fail_unless(parse() == NULL);
   fail_if_errors();
}
END_TEST

START_TEST(test_context)
{
   set_standard(STD_08);

   input_from_file(TESTDIR "/parse/context.vhd");

   const error_t expect[] = {
      { 19, "context clause preceeding context declaration must be empty" },
      { -1, NULL }
   };
   expect_errors(expect);

   lib_t widget_lib = lib_tmp("widget_lib");
   lib_set_work(widget_lib);

   tree_t p1 = parse();
   fail_if(p1 == NULL);
   fail_unless(tree_kind(p1) == T_PACKAGE);

   tree_t p2 = parse();
   fail_if(p2 == NULL);
   fail_unless(tree_kind(p2) == T_PACKAGE);

   tree_t c1 = parse();
   fail_if(c1 == NULL);
   fail_unless(tree_kind(c1) == T_CONTEXT);
   fail_unless(tree_ident(c1) == ident_new("WIDGET_LIB.WIDGET_CONTEXT"));
   fail_unless(tree_contexts(c1) == 5);
   fail_unless(tree_kind(tree_context(c1, 3)) == T_USE);

   lib_t project = lib_tmp("project");
   lib_set_work(project);

   tree_t c2 = parse();
   fail_if(c2 == NULL);
   fail_unless(tree_kind(c2) == T_CONTEXT);
   fail_unless(tree_ident(c2) == ident_new("PROJECT.DONGLE_CONTEXT"));
   fail_unless(tree_contexts(c2) == 5);

   tree_t r = tree_context(c2, 4);
   fail_unless(tree_kind(r) == T_CTXREF);
   fail_unless(tree_ident(r) == ident_new("WIDGET_LIB.WIDGET_CONTEXT"));

   fail_if(parse() == NULL);

   check_expected_errors();
}
END_TEST

START_TEST(test_issue222)
{
   input_from_file(TESTDIR "/parse/issue222.vhd");

   const error_t expect[] = {
      { 13, "component instantiation statement must have a label" },
      { 22, "block statement must have a label" },
      { 32, "component instantiation statement must have a label" },
      { 37, "generate statement must have a label" },
      { 58, "generate statement must have a label" },
      { -1, NULL }
   };
   expect_errors(expect);

   for (int i = 0; i < 8; i++)
      (void)parse();

   fail_unless(parse() == NULL);
   check_expected_errors();
}
END_TEST

START_TEST(test_guarded)
{
   input_from_file(TESTDIR "/parse/guarded.vhd");

   tree_t e = parse();
   fail_if(e == NULL);
   fail_unless(tree_kind(e) == T_ENTITY);

   tree_t a = parse();
   fail_if(a == NULL);
   fail_unless(tree_kind(a) == T_ARCH);
   fail_unless(tree_stmts(a) == 2);

   tree_t s0 = tree_stmt(a, 0);
   fail_unless(tree_kind(s0) == T_CASSIGN);
   fail_unless(tree_flags(s0) & TREE_F_GUARDED);

   tree_t s1 = tree_stmt(a, 1);
   fail_unless(tree_kind(s1) == T_SELECT);
   fail_unless(tree_flags(s1) & TREE_F_GUARDED);

   fail_unless(parse() == NULL);
   fail_if_errors();
}
END_TEST

START_TEST(test_cond1)
{
   input_from_file(TESTDIR "/parse/cond1.vhd");

   const error_t expect[] = {
      { 10, "\"this is a warning\"" },
      { 13, "\"Using nvc\"" },
      { 21, "\"correct\"" },
      { 25, "\"VHDL version is correct\"" },
      { 32, "undefined conditional analysis identifier FOO" },
      { 35, "unterminated conditional analysis block" },
      { -1, NULL }
   };
   expect_errors(expect);

   tree_t p = parse();
   fail_if(p == NULL);

   fail_unless(tree_kind(p) == T_PACKAGE);
   fail_unless(tree_decls(p) == 1);
   fail_unless(tree_ident(tree_decl(p, 0)) == ident_new("C"));

   fail_unless(parse() != NULL);
   check_expected_errors();
}
END_TEST

START_TEST(test_issue360)
{
   input_from_file(TESTDIR "/parse/issue360.vhd");

   const error_t expect[] = {
      {  8, "unexpected ; while parsing process statement, expecting process" },
      { -1, NULL }
   };
   expect_errors(expect);

   tree_t e = parse();
   fail_if(e == NULL);
   fail_unless(tree_kind(e) == T_ENTITY);

   fail_unless(parse() != NULL);

   check_expected_errors();
}
END_TEST

START_TEST(test_issue367)
{
   input_from_file(TESTDIR "/parse/issue367.vhd");

   tree_t p = parse();
   fail_if(p == NULL);
   fail_unless(tree_kind(p) == T_PACKAGE);

   tree_t b = parse();
   fail_if(b == NULL);
   fail_unless(tree_kind(b) == T_PACK_BODY);

   tree_t f = tree_decl(b, 0);
   fail_unless(tree_kind(f) == T_FUNC_BODY);

   tree_t d = tree_decl(f, 1);
   fail_unless(tree_kind(d) == T_VAR_DECL);
   fail_unless(tree_ident(d) == ident_new("I"));

   type_t type = tree_type(d);
   fail_unless(type_kind(type) == T_SUBTYPE);

   tree_t c = type_constraint(type);
   fail_unless(tree_ranges(c) == 1);

   tree_t r = tree_range(c, 0);
   fail_unless(tree_subkind(r) == RANGE_EXPR);
   fail_unless(tree_kind(tree_value(r)) == T_ATTR_REF);

   fail_if(parse() != NULL);

   fail_if_errors();
}
END_TEST

START_TEST(test_issue369)
{
   input_from_file(TESTDIR "/parse/issue369.vhd");

   tree_t e = parse();
   fail_if(e == NULL);
   fail_unless(tree_kind(e) == T_ENTITY);

   tree_t a = parse();
   fail_if(a == NULL);
   fail_unless(tree_kind(a) == T_ARCH);

   tree_t p = tree_stmt(a, 0);

   tree_t aspec = search_decls(p, ident_new("A"), 1);
   fail_if(aspec == NULL);
   fail_unless(tree_kind(aspec) == T_ATTR_SPEC);
   fail_unless(tree_class(aspec) == C_LITERAL);

   tree_t cmp = tree_value(tree_stmt(p, 0));
   fail_unless(tree_kind(cmp) == T_FCALL);

   tree_t aref = tree_value(tree_param(cmp, 0));
   fail_unless(tree_kind(aref) == T_ATTR_REF);
   fail_unless(tree_ident(aref) == ident_new("A"));

   tree_t ref = tree_name(aref);
   fail_unless(tree_ident(ref) == ident_new("THIS_PROCESS.'1'"));

   fail_if(parse() != NULL);

   fail_if_errors();
}
END_TEST;

START_TEST(test_vests1)
{
   input_from_file(TESTDIR "/parse/vests1.vhd");

   tree_t e0 = parse();
   fail_if(e0 == NULL);
   fail_unless(tree_kind(e0) == T_ENTITY);

   tree_t a0 = parse();
   fail_if(a0 == NULL);
   fail_unless(tree_kind(a0) == T_ARCH);

   tree_t c0 = parse();
   fail_if(c0 == NULL);
   fail_unless(tree_kind(c0) == T_CONFIGURATION);
   fail_unless(tree_primary(c0) == e0);

   tree_t e1 = parse();
   fail_if(e1 == NULL);
   fail_unless(tree_kind(e1) == T_ENTITY);

   tree_t a1 = parse();
   fail_if(a1 == NULL);
   fail_unless(tree_kind(a1) == T_ARCH);

   tree_t c = parse();
   fail_if(c == NULL);
   fail_unless(tree_kind(c) == T_CONFIGURATION);
   fail_unless(tree_decls(c) == 1);
   fail_unless(tree_primary(c) == e1);

   tree_t b = tree_decl(c, 0);
   fail_unless(tree_kind(b) == T_BLOCK_CONFIG);
   fail_unless(tree_decls(b) == 1);
   fail_unless(tree_ref(b) == a1);

   tree_t b2 = tree_decl(b, 0);
   fail_unless(tree_kind(b2) == T_BLOCK_CONFIG);
   fail_unless(tree_decls(b2) == 3);
   fail_unless(tree_ref(b2) == tree_stmt(a1, 0));

   tree_t b3 = tree_decl(b2, 2);
   fail_unless(tree_kind(b3) == T_BLOCK_CONFIG);
   fail_unless(tree_ident(b3) == ident_new("G"));
   fail_unless(tree_decls(b3) == 1);
   fail_unless(tree_ref(b3) == tree_stmt(tree_stmt(a1, 0), 1));

   fail_if(parse() != NULL);

   fail_if_errors();
}
END_TEST;

START_TEST(test_synth)
{
   opt_set_int("synthesis", 1);

   input_from_file(TESTDIR "/parse/synth.vhd");

   tree_t e = parse();
   fail_if(e == NULL);
   fail_unless(tree_kind(e) == T_ENTITY);

   tree_t a = parse();
   fail_if(a == NULL);
   fail_unless(tree_kind(a) == T_ARCH);
   fail_unless(tree_decls(a) == 1);
   fail_unless(tree_ident(tree_decl(a, 0)) == ident_new("Y"));
   fail_unless(tree_stmts(a) == 1);

   fail_if(parse() != NULL);

   fail_if_errors();
}
END_TEST;

START_TEST(test_issue388)
{
   input_from_file(TESTDIR "/parse/issue388.vhd");

   const error_t expect[] = {
      { 11, "unexpected => while parsing slice name, expecting one of" },
      { 12, "unexpected => while parsing concurrent procedure call" },
      { 12, "no visible subprogram declaration for Q" },
      { -1, NULL }
   };
   expect_errors(expect);

   tree_t e = parse();
   fail_if(e == NULL);
   fail_unless(tree_kind(e) == T_ENTITY);

   tree_t p = parse();
   fail_if(p == NULL);

   check_expected_errors();
}
END_TEST

START_TEST(test_names)
{
   input_from_file(TESTDIR "/parse/names.vhd");

   const error_t expect[] = {
      {  14, "FUNC4 [INTEGER return INTEGER] already declared" },
      {  13, "previous declaration of FUNC4 [INTEGER return INTEGER]" },
      {  42, "ambiguous use of enumeration literal '1'" },
      {  26, "visible declaration of '1' as BIT" },
      {  36, "visible declaration of '1' as CHARACTER" },
      {  46, "no possible overload of FUNC5 has formal Z" },
      {  16, "subprogram FUNC5 [CHARACTER return INTEGER] has argument X" },
      {  17, "subprogram FUNC5 [BIT return INTEGER] has argument Y" },
      {  46, "ambiguous use of enumeration literal '1'" },
      {  26, "visible declaration of '1' as BIT" },
      {  36, "visible declaration of '1' as CHARACTER" },
      {  47, "type of string literal cannot be determined from context" },
      {  50, "type of aggregate is ambiguous (BIT_VECTOR, BOOL_VECTOR)" },
      {  51, "type of aggregate is ambiguous (BIT_VECTOR, BOOL_VECTOR)" },
      {  54, "ambiguous call to function FUNC7" },
      {  26, "candidate FUNC7 [INTEGER return INTEGER]" },
      {  27, "candidate FUNC7 [MY_INT return INTEGER]" },
      {  56, "type of string literal is ambiguous (BIT_VECTOR, STRING)" },
      {  69, "PROC4 [INTEGER] already declared in this region" },
      {  68, "previous declaration of PROC4 [INTEGER] was here" },
      {  84, "PROC8 already declared in this region" },
      {  80, "previous declaration of PROC8 was here" },
      {  92, "ambiguous use of enumeration literal '1'" },
      {  26, "visible declaration of '1' as BIT" },
      {  36, "visible declaration of '1' as CHARACTER" },
      {  96, "no possible overload of PROC5 has formal Z" },
      {  71, "subprogram PROC5 [CHARACTER] has argument X" },
      {  72, "subprogram PROC5 [BIT] has argument Y" },
      {  96, "ambiguous use of enumeration literal '1'" },
      {  26, "visible declaration of '1' as BIT" },
      {  36, "visible declaration of '1' as CHARACTER" },
      {  97, "type of string literal cannot be determined from context" },
      { 100, "type of aggregate is ambiguous (BIT_VECTOR, BOOL_VECTOR)" },
      { 101, "type of aggregate is ambiguous (BIT_VECTOR, BOOL_VECTOR)" },
      { 104, "ambiguous call to procedure PROC7" },
      {  77, "candidate PROC7 [INTEGER]" },
      {  78, "candidate PROC7 [MY_INT]" },
      { 106, "type of string literal is ambiguous (BIT_VECTOR, STRING)" },
      { 107, "invalid procedure call statement" },
      { 108, "no visible subprogram declaration for FOO" },
      { 233, "name X not found in \"+\"" },
      { 256, "no visible subprogram declaration for NOTHERE" },
      { 313, "no visible subprogram declaration for FNORK" },
      { 323, "no matching subprogram P26_1 [universal_integer" },
      { 332, "no matching operator \"and\" [BIT, BOOLEAN return BOOLEAN]" },
      { 360, "object X with type INTEGER cannot be selected" },
      { 362, "no visible declaration for FOO" },
      {  -1, NULL }
   };
   expect_errors(expect);

   tree_t e = parse();
   fail_if(e == NULL);
   fail_unless(tree_kind(e) == T_ENTITY);

   tree_t a = parse();
   fail_if(a == NULL);
   fail_unless(tree_kind(a) == T_ARCH);

   type_t std_int = std_type(NULL, "INTEGER");

   tree_t p = tree_stmt(a, 0);
   tree_t s = tree_stmt(p, 0);
   fail_unless(tree_kind(s) == T_VAR_ASSIGN);
   tree_t t = tree_value(s);
   fail_unless(tree_kind(t) == T_FCALL);
   fail_unless(type_eq(tree_type(t), std_int));
   tree_t d = tree_ref(t);
   fail_unless(tree_loc(d)->first_line == 5);
   t = tree_value(tree_param(t, 0));
   fail_unless(tree_kind(t) == T_LITERAL);
   fail_unless(type_eq(tree_type(t), type_universal_int()));

   p = tree_stmt(a, 4);
   d = search_decls(p, ident_new("TABLE"), 0);
   fail_if(d == NULL);
   fail_unless(tree_kind(d) == T_CONST_DECL);
   tree_t v = tree_value(d);
   fail_unless(tree_kind(v) == T_AGGREGATE);
   fail_unless(tree_assocs(v) == 2);
   tree_t a0 = tree_assoc(v, 0);
   fail_unless(tree_subkind(a0) == A_NAMED);
   tree_t n0 = tree_name(a0);
   fail_unless(tree_kind(n0) == T_REF);
   fail_unless(tree_ident(n0) == ident_new("'0'"));
   fail_unless(type_ident(tree_type(n0)) == ident_new("STD.STANDARD.BIT"));

   tree_t b = tree_stmt(a, 7);
   fail_unless(tree_kind(b) == T_BLOCK);
   d = tree_decl(b, 0);
   fail_unless(tree_kind(d) == T_FUNC_DECL);
   fail_unless(tree_ident2(d) == ident_new("_get_foo"));

   p = tree_stmt(a, 8);
   fail_unless(tree_kind(p) == T_PROCESS);
   s = tree_stmt(p, 0);
   fail_unless(tree_kind(s) == T_ASSERT);
   v = tree_value(s);
   fail_unless(tree_kind(v) == T_FCALL);
   fail_unless(tree_ident(v) == ident_new("\"<\""));
   d = tree_ref(v);
   fail_unless(tree_kind(d) == T_FUNC_BODY);
   fail_if(tree_flags(d) & TREE_F_PREDEFINED);

   check_expected_errors();
}
END_TEST

START_TEST(test_implicit)
{
   opt_set_int("missing-body", 1);
   input_from_file(TESTDIR "/parse/names.vhd");

   fail_if_errors();
}
END_TEST

START_TEST(test_error2)
{
   set_standard(STD_00);

   input_from_file(TESTDIR "/parse/error2.vhd");

   const error_t expect[] = {
      {  1, "missing declaration for package WORK.DUNNO" },
      {  2, "no visible declaration for BAR" },
      {  5, "no visible declaration for SDFF" },
      { 10, "no visible declaration for SGHBBX" },
      { 17, "cannot find unit STD.NOTHERE" },
      { 22, "unexpected identifier while parsing range" },
      { 29, "expected physical type definition trailing" },
      { 33, "expected record type definition trailing identifier" },
      { 38, "unexpected procedure while parsing subprogram body" },
      { 42, "unexpected function while parsing subprogram body" },
      { 44, "protected type declaration trailing label to match OTHER" },
      { 47, "unexpected integer while parsing subtype declaration" },
      { -1, NULL }
   };
   expect_errors(expect);

   parse();

   check_expected_errors();
}
END_TEST

START_TEST(test_vhdl2008)
{
   set_standard(STD_08);
   input_from_file(TESTDIR "/parse/vhdl2008.vhd");

   const error_t expect[] = {
      { 57, "no matching operator \"??\" [TIME return BOOLEAN]" },
      { -1, NULL }
   };
   expect_errors(expect);

   tree_t e = parse();
   fail_if(e == NULL);
   fail_unless(tree_kind(e) == T_ENTITY);

   tree_t a = parse();
   fail_if(a == NULL);
   fail_unless(tree_kind(a) == T_ARCH);

   tree_t d = search_decls(a, ident_new("MY_TYPE_VECTOR"), 0);
   fail_if(d == NULL);
   type_t type = tree_type(d);
   fail_unless(type_kind(type) == T_SUBTYPE);
   fail_unless(type_has_resolution(type));
   tree_t r = type_resolution(type);
   fail_unless(tree_kind(r) == T_AGGREGATE);

   fail_unless(parse() == NULL);

   check_expected_errors();
}
END_TEST

START_TEST(test_issue416)
{
   set_relax_rules(RELAX_PREFER_EXPLICT);
   input_from_file(TESTDIR "/parse/issue416.vhd");

   tree_t e = parse();
   fail_if(e == NULL);
   fail_unless(tree_kind(e) == T_ENTITY);

   fail_unless(parse() == NULL);

   fail_if_errors();
}
END_TEST

START_TEST(test_explicit_93)
{
   set_standard(STD_93);
   input_from_file(TESTDIR "/parse/explicit.vhd");

   const error_t expect[] = {
      { 25, "ambiguous use of operator \"<=\"" },
      {  3, "candidate \"<=\" [MY_VEC, MY_VEC return BOOLEAN]" },
      { 10, "candidate \"<=\" [MY_VEC, MY_VEC return BOOLEAN]" },
      { -1, NULL }
   };
   expect_errors(expect);

   tree_t p1 = parse();
   fail_if(p1 == NULL);
   fail_unless(tree_kind(p1) == T_PACKAGE);

   tree_t p2 = parse();
   fail_if(p2 == NULL);
   fail_unless(tree_kind(p2) == T_PACKAGE);

   tree_t e = parse();
   fail_if(e == NULL);
   fail_unless(tree_kind(e) == T_ENTITY);

   tree_t a = parse();
   fail_if(a == NULL);
   fail_unless(tree_kind(a) == T_ARCH);

   fail_unless(parse() == NULL);

   check_expected_errors();
}
END_TEST

START_TEST(test_explicit_08)
{
   set_standard(STD_08);
   input_from_file(TESTDIR "/parse/explicit.vhd");

   tree_t p1 = parse();
   fail_if(p1 == NULL);
   fail_unless(tree_kind(p1) == T_PACKAGE);

   tree_t p2 = parse();
   fail_if(p2 == NULL);
   fail_unless(tree_kind(p2) == T_PACKAGE);

   tree_t e = parse();
   fail_if(e == NULL);
   fail_unless(tree_kind(e) == T_ENTITY);

   tree_t a = parse();
   fail_if(a == NULL);
   fail_unless(tree_kind(a) == T_ARCH);

   fail_unless(parse() == NULL);

   fail_if_errors();
}
END_TEST

START_TEST(test_homograph)
{
   input_from_file(TESTDIR "/parse/homograph.vhd");

   tree_t p1 = parse();
   fail_if(p1 == NULL);
   fail_unless(tree_kind(p1) == T_PACKAGE);

   tree_t e = parse();
   fail_if(e == NULL);
   fail_unless(tree_kind(e) == T_ENTITY);

   tree_t a = parse();
   fail_if(a == NULL);
   fail_unless(tree_kind(a) == T_ARCH);

   fail_unless(parse() == NULL);

   fail_if_errors();
}
END_TEST

Suite *get_parse_tests(void)
{
   Suite *s = suite_create("parse");

   TCase *tc_core = nvc_unit_test();
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
   tcase_add_test(tc_core, test_loc);
   tcase_add_test(tc_core, test_expr);
   tcase_add_test(tc_core, test_error);
   tcase_add_test(tc_core, test_config);
   tcase_add_test(tc_core, test_protected);
   tcase_add_test(tc_core, test_empty);
   tcase_add_test(tc_core, test_issue205);
   tcase_add_test(tc_core, test_context);
   tcase_add_test(tc_core, test_issue222);
   tcase_add_test(tc_core, test_guarded);
   tcase_add_test(tc_core, test_cond1);
   tcase_add_test(tc_core, test_issue360);
   tcase_add_test(tc_core, test_issue367);
   tcase_add_test(tc_core, test_issue369);
   tcase_add_test(tc_core, test_vests1);
   tcase_add_test(tc_core, test_synth);
   tcase_add_test(tc_core, test_issue388);
   tcase_add_test(tc_core, test_names);
   tcase_add_test(tc_core, test_implicit);
   tcase_add_test(tc_core, test_error2);
   tcase_add_test(tc_core, test_vhdl2008);
   tcase_add_test(tc_core, test_issue416);
   tcase_add_test(tc_core, test_explicit_93);
   tcase_add_test(tc_core, test_explicit_08);
   tcase_add_test(tc_core, test_homograph);
   suite_add_tcase(s, tc_core);

   return s;
}
