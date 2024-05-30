//
//  Copyright (C) 2011-2024  Nick Gasson
//
//  This program is free software: you can redistribute it and/or modify
//  it under the terms of the GNU General Public License as published by
//  the Free Software Foundation, either version 3 of the License, or
//  (at your option) any later version.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.
//
//  You should have received a copy of the GNU General Public License
//  along with this program.  If not, see <http://www.gnu.org/licenses/>.
//

#include "test_util.h"
#include "common.h"
#include "diag.h"
#include "lib.h"
#include "option.h"
#include "phase.h"
#include "scan.h"
#include "tree.h"
#include "type.h"

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <inttypes.h>
#include <float.h>

START_TEST(test_entity)
{
   tree_t e, p, g, v, x, y;
   type_t t;

   input_from_file(TESTDIR "/parse/entity.vhd");

   const error_t expect[] = {
      { 39, "invalid object class signal for generic X" },
      { -1, NULL }
   };
   expect_errors(expect);

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
   fail_unless(tree_kind(g) == T_GENERIC_DECL);
   fail_unless(tree_ident(g) == ident_new("X"));
   t = tree_type(g);
   fail_unless(type_kind(t) == T_ENUM);
   fail_unless(type_ident(t) == ident_new("STD.STANDARD.BOOLEAN"));
   fail_if(tree_has_value(p));

   g = tree_generic(e, 1);
   fail_unless(tree_kind(g) == T_GENERIC_DECL);
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
   fail_unless(tree_kind(tree_stmt(tree_stmt(e, 0), 0)) == T_ASSERT);
   fail_unless(tree_kind(tree_stmt(tree_stmt(e, 1), 0)) == T_ASSERT);

   e = parse();
   fail_if(e == NULL);
   fail_unless(tree_kind(e) == T_ENTITY);
   fail_unless(tree_ident(e) == ident_new("WORK.EIGHT"));
   fail_unless(tree_generics(e) == 1);

   g = tree_generic(e, 0);
   fail_unless(tree_kind(g) == T_GENERIC_DECL);
   fail_unless(tree_class(g) == C_SIGNAL);

   e = parse();
   fail_unless(e == NULL);

   check_expected_errors();
}
END_TEST

START_TEST(test_arch)
{
   tree_t e, a, d, v;

   input_from_file(TESTDIR "/parse/arch.vhd");

   e = parse();
   fail_if(e == NULL);
   fail_unless(tree_kind(e) == T_ENTITY);
   lib_put(lib_work(), e);

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
   lib_put(lib_work(), e);

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
   fail_unless(tree_kind(p) == T_CONCURRENT);
   fail_unless(tree_flags(p) & TREE_F_POSTPONED);

   a = parse();
   fail_unless(a == NULL);

   fail_if_errors();
}
END_TEST

START_TEST(test_seq)
{
   tree_t a, p, s, e, b, c, r;

   set_standard(STD_08);

   input_from_file(TESTDIR "/parse/seq.vhd");

   const error_t expect[] = {
      {  15, "type of slice prefix INTEGER is not an array" },
      {  45, "target of variable assignment must be a variable name or" },
      {  84, "return statement not allowed outside subprogram" },
      { 125, "cannot use exit statement outside loop" },
      { 126, "cannot use exit statement outside loop" },
      { 136, "positional parameters must precede named parameters" },
      { 157, "cannot use next statement outside loop" },
      { 158, "cannot use next statement outside loop" },
      { 197, "a null waveform element is only valid when the target" },
      {  -1, NULL }
   };
   expect_errors(expect);

   e = parse();
   fail_if(e == NULL);
   fail_unless(tree_kind(e) == T_ENTITY);
   lib_put(lib_work(), e);

   a = parse();
   fail_if(a == NULL);
   fail_unless(tree_kind(a) == T_ARCH);
   fail_unless(tree_stmts(a) == 19);

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
   fail_unless(tree_kind(tree_message(s)) == T_STRING);

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
   fail_unless(tree_conds(s) == 1);
   c = tree_cond(s, 0);
   fail_unless(tree_has_value(c));
   e = tree_value(c);
   fail_unless(tree_kind(e) == T_REF);
   fail_unless(tree_ident(e) == ident_new("TRUE"));
   fail_unless(tree_stmts(c) == 1);

   s = tree_stmt(p, 2);
   fail_unless(tree_kind(s) == T_IF);
   fail_unless(tree_conds(s) == 2);
   fail_if(tree_has_value(tree_cond(s, 1)));

   s = tree_stmt(p, 3);
   fail_unless(tree_kind(s) == T_IF);
   fail_unless(tree_conds(s) == 4);
   s = tree_stmt(tree_cond(s, 3), 0);
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
   fail_unless(tree_kind(s) == T_LOOP);
   fail_unless(tree_stmts(s) == 1);

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

   r = tree_range(s, 0);
   fail_unless(tree_subkind(r) == RANGE_TO);
   fail_unless(tree_kind(tree_left(r)) == T_TYPE_CONV);
   fail_unless(tree_kind(tree_left(r)) == T_TYPE_CONV);

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

   s = tree_stmt(p, 1);
   fail_unless(tree_kind(s) == T_PCALL);
   fail_unless(tree_ident2(s) == ident_new("BAR"));
   fail_unless(tree_params(s) == 0);

   // Case

   p = tree_stmt(a, 12);

   s = tree_stmt(p, 0);
   fail_unless(tree_kind(s) == T_CASE);
   fail_unless(tree_stmts(s) == 4);
   fail_unless(tree_subkind(tree_assoc(tree_stmt(s, 0), 0)) == A_NAMED);
   fail_unless(tree_subkind(tree_assoc(tree_stmt(s, 3), 0)) == A_OTHERS);
   b = tree_stmt(s, 0);
   fail_unless(tree_kind(b) == T_ALTERNATIVE);
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
   fail_unless(tree_stmts(s) == 1);
   fail_unless(tree_subkind(tree_assoc(tree_stmt(s, 0), 0)) == A_NAMED);

   // Process with all-sensitivity

   p = tree_stmt(a, 16);
   fail_unless(tree_triggers(p) == 1);
   fail_unless(tree_kind(tree_trigger(p, 0)) == T_ALL);

   // Aggregate target of variable assignment

   p = tree_stmt(a, 17);

   s = tree_stmt(p, 0);
   fail_unless(tree_kind(s) == T_VAR_ASSIGN);
   fail_unless(tree_kind(tree_target(s)) == T_AGGREGATE);

   // Signal assignment with null transaction

   p = tree_stmt(a, 18);

   s = tree_stmt(p, 0);
   fail_unless(tree_kind(s) == T_SIGNAL_ASSIGN);
   fail_unless(tree_waveforms(s) == 2);
   fail_unless(tree_has_delay(tree_waveform(s, 1)));
   fail_if(tree_has_value(tree_waveform(s, 1)));

   a = parse();
   fail_unless(a == NULL);

   check_expected_errors();
}
END_TEST

START_TEST(test_types)
{
   tree_t a, b, d, f, u, r;
   type_t t;

   input_from_file(TESTDIR "/parse/types.vhd");

   const error_t expect[] = {
      { 16, "expected type of range bounds to be RESISTANCE but have " },
      { 41, "index constraint cannot be used with non-array type FOO" },
      { 55, "no visible declaration for SOME_ARRAY" },
      { -1, NULL }
   };
   expect_errors(expect);

   b = parse();
   fail_if(b == NULL);
   fail_unless(tree_kind(b) == T_ENTITY);
   lib_put(lib_work(), b);

   a = parse();
   fail_if(a == NULL);
   fail_unless(tree_kind(a) == T_ARCH);
   fail_unless(tree_decls(a) == 79);  // Includes predefined

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
   fail_unless(tree_kind(d) == T_SUBTYPE_DECL);
   t = tree_type(d);
   fail_unless(type_kind(t) == T_SUBTYPE);
   fail_unless(type_kind(type_base(t)) == T_PHYSICAL);
   fail_unless(type_ident(type_base(t)) == ident_new("WORK.B-A.RESISTANCE"));

   d = search_decls(a, ident_new("MY_SMALL_INT"), 0);
   fail_if(d == NULL);
   fail_unless(tree_kind(d) == T_SUBTYPE_DECL);
   t = tree_type(d);
   fail_unless(type_kind(t) == T_SUBTYPE);
   fail_unless(type_kind(type_base(t)) == T_INTEGER);
   fail_unless(type_ident(type_base(t)) == ident_new("WORK.B-A.MY_INT"));

   d = search_decls(a, ident_new("FOO"), 0);
   fail_if(d == NULL);
   fail_unless(tree_kind(d) == T_SUBTYPE_DECL);
   t = tree_type(d);
   fail_unless(type_kind(t) == T_SUBTYPE);
   fail_unless(type_kind(type_base(t)) == T_INTEGER);
   fail_unless(type_ident(type_base(t)) == ident_new("WORK.B-A.MY_INT"));
   r = tree_range(type_constraint(t, 0), 0);
   fail_unless(tree_kind(tree_left(r)) == T_LITERAL);
   fail_unless(tree_kind(tree_right(r)) == T_ATTR_REF);
   fail_unless(tree_ident(tree_name(tree_right(r))) == ident_new("MY_INT"));
   fail_unless(tree_ident(tree_right(r)) == ident_new("HIGH"));

   d = search_decls(a, ident_new("RINT"), 0);
   fail_if(d == NULL);
   fail_unless(tree_kind(d) == T_SUBTYPE_DECL);
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
   fail_unless(type_kind(type_designated(t)) == T_INTEGER);
   fail_unless(type_ident(type_designated(t)) == ident_new("WORK.B-A.MY_INT"));

   d = search_decls(a, ident_new("F"), 0);
   fail_if(d == NULL);
   fail_unless(tree_kind(d) == T_TYPE_DECL);
   t = tree_type(d);
   fail_unless(type_kind(t) == T_FILE);
   fail_unless(type_kind(type_designated(t)) == T_INTEGER);
   fail_unless(type_ident(type_designated(t)) == ident_new("WORK.B-A.MY_INT"));

   d = search_decls(a, ident_new("F1"), 0);
   fail_if(d == NULL);
   fail_unless(tree_kind(d) == T_FILE_DECL);
   fail_unless(tree_has_value(d));
   fail_unless(tree_kind(tree_value(d)) == T_STRING);
   fail_unless(tree_kind(tree_file_mode(d)) == T_REF);

   d = search_decls(a, ident_new("F2"), 0);
   fail_if(d == NULL);
   fail_unless(tree_kind(d) == T_FILE_DECL);
   fail_unless(tree_has_value(d));
   fail_unless(tree_kind(tree_value(d)) == T_STRING);
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
   fail_unless(tree_kind(tree_value(d)) == T_STRING);
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

   d = search_decls(a, ident_new("MY_REAL"), 0);
   fail_if(d == NULL);
   fail_unless(tree_kind(d) == T_SUBTYPE_DECL);
   t = tree_type(d);
   fail_unless(type_kind(t) == T_SUBTYPE);
   fail_unless(type_has_ident(t));
   t = type_base(t);
   fail_unless(t == std_type(NULL, STD_REAL));
   r = type_dim(t, 0);
   fail_unless(tree_kind(tree_left(r)) == T_LITERAL);
   fail_unless(tree_subkind(tree_left(r)) == L_REAL);
   fail_unless(tree_dval(tree_left(r)) == -DBL_MAX);
   fail_unless(tree_kind(tree_right(r)) == T_LITERAL);
   fail_unless(tree_subkind(tree_right(r)) == L_REAL);
   fail_unless(tree_dval(tree_right(r)) == DBL_MAX);

   d = search_decls(a, ident_new("MY_REAL2"), 0);
   fail_if(d == NULL);
   fail_unless(tree_kind(d) == T_SUBTYPE_DECL);
   t = tree_type(d);
   fail_unless(type_kind(t) == T_SUBTYPE);
   fail_unless(type_has_ident(t));
   r = tree_range(type_constraint(t, 0), 0);
   ck_assert_double_eq_tol(tree_dval(tree_left(r)), 0.0, DBL_EPSILON);
   ck_assert_double_eq_tol(tree_dval(tree_right(r)), 10.0, DBL_EPSILON);

   a = parse();
   fail_unless(a == NULL);

   check_expected_errors();
}
END_TEST

START_TEST(test_literal)
{
   tree_t a, e, d, v;

   input_from_file(TESTDIR "/parse/literal.vhd");

   const error_t expect[] = {
      { 36, "value 9223372036854775808 is outside implementation defined "
        "range of universal_integer" },
      { 37, "value 235423414124e124124 is outside implementation defined "
        "range of universal_integer" },
      { -1, NULL }
   };
   expect_errors(expect);

   e = parse();
   fail_if(e == NULL);
   fail_unless(tree_kind(e) == T_ENTITY);
   lib_put(lib_work(), e);

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
   fail_unless(tree_kind(v) == T_STRING);
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
   fail_unless(tree_kind(v) == T_STRING);
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
   fail_unless(tree_kind(v) == T_STRING);
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
   fail_unless(tree_kind(v) == T_STRING);
   fail_unless(tree_chars(v) == 0);

   d = search_decls(a, ident_new("N"), 0);
   fail_if(d == NULL);
   fail_unless(tree_kind(d) == T_CONST_DECL);
   fail_unless(tree_ident(d) == ident_new("N"));
   v = tree_value(d);
   fail_unless(tree_kind(v) == T_STRING);
   fail_unless(tree_chars(v) == 1);
   fail_unless(tree_ident(tree_char(v, 0)) == ident_new("' '"));

   d = search_decls(a, ident_new("O"), 0);
   fail_if(d == NULL);
   fail_unless(tree_kind(d) == T_CONST_DECL);
   fail_unless(tree_ident(d) == ident_new("O"));
   v = tree_value(d);
   fail_unless(tree_kind(v) == T_STRING);
   fail_unless(tree_chars(v) == 1);
   fail_unless(tree_ident(tree_char(v, 0)) == ident_new("'A'"));

   d = search_decls(a, ident_new("P"), 0);
   fail_if(d == NULL);
   fail_unless(tree_kind(d) == T_CONST_DECL);
   fail_unless(tree_ident(d) == ident_new("P"));
   v = tree_value(d);
   fail_unless(tree_kind(v) == T_STRING);
   fail_unless(tree_chars(v) == 1);
   fail_unless(tree_ident(tree_char(v, 0)) == ident_new("'\"'"));

   d = search_decls(a, ident_new("Q"), 0);
   fail_if(d == NULL);
   fail_unless(tree_kind(d) == T_CONST_DECL);
   fail_unless(tree_ident(d) == ident_new("Q"));
   v = tree_value(d);
   fail_unless(tree_kind(v) == T_STRING);
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
   fail_unless(tree_kind(v) == T_STRING);
   fail_unless(tree_chars(v) == 0);

   d = search_decls(a, ident_new("S"), 0);
   fail_if(d == NULL);
   fail_unless(tree_kind(d) == T_CONST_DECL);
   fail_unless(tree_ident(d) == ident_new("S"));
   v = tree_value(d);
   fail_unless(tree_kind(v) == T_STRING);
   fail_unless(tree_chars(v) == 1);
   fail_unless(tree_ident(tree_char(v, 0)) == ident_new("' '"));

   d = search_decls(a, ident_new("T"), 0);
   fail_if(d == NULL);
   fail_unless(tree_kind(d) == T_CONST_DECL);
   fail_unless(tree_ident(d) == ident_new("T"));
   v = tree_value(d);
   fail_unless(tree_kind(v) == T_STRING);
   fail_unless(tree_chars(v) == 1);
   fail_unless(tree_ident(tree_char(v, 0)) == ident_new("'A'"));

   d = search_decls(a, ident_new("U"), 0);
   fail_if(d == NULL);
   fail_unless(tree_kind(d) == T_CONST_DECL);
   fail_unless(tree_ident(d) == ident_new("U"));
   v = tree_value(d);
   fail_unless(tree_kind(v) == T_STRING);
   fail_unless(tree_chars(v) == 1);
   fail_unless(tree_ident(tree_char(v, 0)) == ident_new("'%'"));

   d = search_decls(a, ident_new("V"), 0);
   fail_if(d == NULL);
   fail_unless(tree_kind(d) == T_CONST_DECL);
   fail_unless(tree_ident(d) == ident_new("V"));
   v = tree_value(d);
   fail_unless(tree_kind(v) == T_STRING);
   fail_unless(tree_chars(v) == 1);
   fail_unless(ident_char(tree_ident(tree_char(v, 0)), 1) == (char)0xa9);

   a = parse();
   fail_unless(a == NULL);

   check_expected_errors();
}
END_TEST

START_TEST(test_extended)
{
   tree_t a, e, d, n, s;

   input_from_file(TESTDIR "/parse/extended.vhd");

   const error_t expect[] = {
      { 12, "unexpected error while parsing signal declaration" },
      { -1, NULL }
   };
   expect_errors(expect);

   e = parse();
   fail_if(e == NULL);
   fail_unless(tree_kind(e) == T_ENTITY);
   lib_put(lib_work(), e);

   a = parse();
   fail_if(a == NULL);
   fail_unless(tree_kind(a) == T_ARCH);
   fail_unless(tree_decls(a) == 8);
   lib_put(lib_work(), a);

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
   fail_unless(tree_kind(s) == T_CONCURRENT);
   s = tree_stmt(s, 0);
   fail_unless(tree_kind(s) == T_COND_ASSIGN);
   fail_unless(tree_ident(tree_target(s)) == ident_new("\\foo.bar.baz\\"));

   a = parse();
   fail_unless(a == NULL);

   check_expected_errors();
}
END_TEST

START_TEST(test_package)
{
   tree_t p, d, c;

   input_from_file(TESTDIR "/parse/package.vhd");

   const error_t expect[] = {
      { 28, "package body may not contain attribute declarations" },
      { 34, "library FOO not found" },
      { -1, NULL }
   };
   expect_errors(expect);

   p = parse();
   fail_if(p == NULL);
   fail_unless(tree_kind(p) == T_PACKAGE);
   fail_unless(tree_contexts(p) == 3);
   fail_unless(tree_ident(p) == ident_new("WORK.ONE"));
   lib_put(lib_work(), p);

   p = parse();
   fail_if(p == NULL);
   fail_unless(tree_kind(p) == T_PACKAGE);
   fail_unless(tree_decls(p) == 1);
   fail_unless(tree_contexts(p) == 4);
   fail_unless(tree_ident(tree_context(p, 3)) == ident_new("WORK.ONE"));
   fail_unless(icmp(tree_ident2(tree_context(p, 3)), "all"));
   fail_unless(tree_ident(p) == ident_new("WORK.TWO"));
   lib_put(lib_work(), p);

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
   lib_put(lib_work(), p);

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
   lib_put(lib_work(), e);

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
   fail_unless(tree_ident(q) == ident_new("WORK.BAR-FOO.FOO"));
   e = tree_value(q);
   fail_unless(tree_kind(e) == T_REF);
   fail_unless(tree_ident(e) == ident_new("B"));

   s = tree_stmt(p, 1);
   fail_unless(tree_kind(s) == T_SIGNAL_ASSIGN);
   q = tree_value(tree_waveform(s, 0));
   fail_unless(tree_kind(q) == T_QUALIFIED);
   fail_unless(tree_ident(q) == ident_new("WORK.BAR-FOO.FOO"));
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

   const error_t expect[] = {
      { 20, "\"blah\" is not an operator symbol" },
      { -1, NULL }
   };
   expect_errors(expect);

   p = parse();
   fail_if(p == NULL);
   fail_unless(tree_kind(p) == T_PACKAGE);
   fail_unless(tree_decls(p) == 4);
   lib_put(lib_work(), p);

   f = tree_decl(p, 0);
   fail_unless(tree_kind(f) == T_FUNC_DECL);
   fail_unless(tree_ident(f) == ident_new("ADD"));
   fail_unless(tree_ports(f) == 3);
   a = tree_port(f, 0);
   fail_unless(tree_kind(a) == T_PARAM_DECL);
   fail_unless(tree_ident(a) == ident_new("X"));
   fail_unless(tree_subkind(a) == PORT_IN);
   t = tree_type(f);
   fail_unless(type_kind(t) == T_FUNC);

   f = tree_decl(p, 1);
   fail_unless(tree_kind(f) == T_FUNC_DECL);
   fail_unless(tree_ident(f) == ident_new("NAUGHTY"));
   fail_unless(tree_ports(f) == 0);
   fail_unless(tree_flags(f) & TREE_F_IMPURE);

   f = tree_decl(p, 2);
   fail_unless(tree_kind(f) == T_FUNC_DECL);
   fail_unless(tree_ident(f) == ident_new("\"+\""));
   fail_unless(tree_ports(f) == 2);

   p = parse();
   fail_if(p == NULL);
   fail_unless(tree_kind(p) == T_PACK_BODY);
   fail_unless(tree_decls(p) == 2);

   p = parse();
   fail_unless(p == NULL);

   check_expected_errors();
}
END_TEST

START_TEST(test_array)
{
   tree_t p, d, a, g, s, e, r;
   type_t t, i, b;
   tree_t x;

   input_from_file(TESTDIR "/parse/array.vhd");

   const error_t expect[] = {
      { 26, "named and positional associations cannot be mixed in array" },
      { 39, "cannot index non-array type INTEGER" },
      { 39, "cannot index non-array type INTEGER" },
      { 47, "expected 2 constraints for type BV2D but found 1" },
      { -1, NULL }
   };
   expect_errors(expect);

   p = parse();
   fail_if(p == NULL);
   fail_unless(tree_kind(p) == T_PACKAGE);
   lib_put(lib_work(), p);

   d = search_decls(p, ident_new("INT_ARRAY"), 0);
   fail_if(d == NULL);
   fail_unless(tree_kind(d) == T_TYPE_DECL);
   fail_unless(tree_ident(d) == ident_new("INT_ARRAY"));
   t = tree_type(d);
   fail_unless(type_kind(t) == T_ARRAY);
   fail_unless(type_indexes(t) == 1);
   i = type_index(t, 0);
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
   fail_unless(type_kind(t) == T_SUBTYPE);
   fail_unless(type_has_ident(t));
   fail_unless(tree_ranges(type_constraint(t, 0)) == 1);
   fail_unless(type_kind(type_base(t)) == T_ARRAY);

   d = search_decls(p, ident_new("CHAR_COUNTS"), 0);
   fail_if(d == NULL);
   fail_unless(tree_kind(d) == T_TYPE_DECL);
   fail_unless(tree_ident(d) == ident_new("CHAR_COUNTS"));
   t = tree_type(d);
   fail_unless(type_kind(t) == T_SUBTYPE);
   fail_unless(tree_ranges(type_constraint(t, 0)) == 1);
   r = tree_range(type_constraint(t, 0), 0);
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
   fail_unless(type_kind(t) == T_SUBTYPE);
   fail_unless(tree_ranges(type_constraint(t, 0)) == 2);
   fail_unless(type_indexes(type_base(t)) == 2);
   r = tree_range(type_constraint(t, 0), 0);
   fail_unless(tree_ival(tree_left(r)) == 1);
   fail_unless(tree_ival(tree_right(r)) == 3);
   r = tree_range(type_constraint(t, 0), 1);
   fail_unless(tree_ival(tree_left(r)) == 4);
   fail_unless(tree_ival(tree_right(r)) == 6);

   e = parse();
   fail_if(e == NULL);
   fail_unless(tree_kind(e) == T_ENTITY);
   lib_put(lib_work(), e);

   a = parse();
   fail_if(a == NULL);
   fail_unless(tree_kind(a) == T_ARCH);
   fail_unless(tree_decls(a) == 9);

   d = search_decls(a, ident_new("X"), 0);
   fail_if(d == NULL);
   fail_unless(tree_ident(d) == ident_new("X"));
   t = tree_type(d);
   fail_unless(type_kind(t) == T_SUBTYPE);
   fail_unless(type_constraints(t) == 1);
   r = tree_range(type_constraint(t, 0), 0);
   fail_unless(tree_ival(tree_left(r)) == 1);
   fail_unless(tree_ival(tree_right(r)) == 5);

   d = search_decls(a, ident_new("Y"), 0);
   fail_if(d == NULL);
   fail_unless(tree_ident(d) == ident_new("Y"));
   t = tree_type(d);
   fail_unless(type_kind(t) == T_SUBTYPE);
   fail_unless(type_ident(t) == ident_new("WORK.P.TEN_INTS"));

   d = search_decls(a, ident_new("Z"), 0);
   fail_if(d == NULL);
   t = tree_type(d);
   fail_unless(type_kind(t) == T_SUBTYPE);
   fail_unless(type_constraints(t) == 1);
   r = tree_range(type_constraint(t, 0), 0);
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
   fail_unless(type_constraints(t) == 1);
   r = tree_range(type_constraint(t, 0), 0);
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
   fail_unless(type_constraints(t) == 1);
   r = tree_range(type_constraint(t, 0), 0);
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

   check_expected_errors();
}
END_TEST

START_TEST(test_instance)
{
   tree_t e, a, s, c, p;

   input_from_file(TESTDIR "/parse/instance.vhd");

   const error_t expect[] = {
      { 55, "X is not a formal generic of WORK.FOO" },
      { 61, "found at least 1 positional actuals but FOO has only 0" },
      { 65, "invalid instantiated unit name" },
      { 69, "design unit SOMETHING is not a component declaration" },
      { 71, "design unit SOMETHING is not a configuration" },
      { -1, NULL }
   };
   expect_errors(expect);

   e = parse();
   fail_if(e == NULL);
   fail_unless(tree_kind(e) == T_ENTITY);
   lib_put(lib_work(), e);

   e = parse();
   fail_if(e == NULL);
   fail_unless(tree_kind(e) == T_ENTITY);
   lib_put(lib_work(), e);

   a = parse();
   fail_if(a == NULL);
   fail_unless(tree_kind(a) == T_ARCH);
   lib_put(lib_work(), a);

   c = parse();
   fail_if(c == NULL);
   fail_unless(tree_kind(c) == T_CONFIGURATION);
   lib_put(lib_work(), c);

   p = parse();
   fail_if(p == NULL);
   fail_unless(tree_kind(p) == T_PACKAGE);
   lib_put(lib_work(), p);

   e = parse();
   fail_if(e == NULL);
   fail_unless(tree_kind(e) == T_ENTITY);
   lib_put(lib_work(), e);

   a = parse();
   fail_if(a == NULL);
   fail_unless(tree_kind(a) == T_ARCH);
   fail_unless(tree_stmts(a) == 15);
   lib_put(lib_work(), a);

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

   s = tree_stmt(a, 12);
   fail_unless(tree_kind(s) == T_INSTANCE);
   fail_unless(tree_class(s) == C_ENTITY);
   fail_unless(tree_has_ref(s));
   fail_unless(tree_kind(tree_ref(s)) == T_ENTITY);

   a = parse();
   fail_unless(a == NULL);

   check_expected_errors();
}
END_TEST

START_TEST(test_conc)
{
   tree_t e, a, s, c, s0;

   input_from_file(TESTDIR "/parse/conc.vhd");

   e = parse();
   fail_if(e == NULL);
   fail_unless(tree_kind(e) == T_ENTITY);
   lib_put(lib_work(), e);

   a = parse();
   fail_if(a == NULL);
   fail_unless(tree_kind(a) == T_ARCH);
   fail_unless(tree_stmts(a) == 8);

   s = tree_stmt(a, 0);
   fail_unless(tree_kind(s) == T_CONCURRENT);
   s = tree_stmt(s, 0);
   fail_unless(tree_kind(s) == T_COND_ASSIGN);
   fail_unless(tree_kind(tree_target(s)) == T_REF);
   fail_unless(tree_conds(s) == 1);
   c = tree_cond(s, 0);
   fail_unless(tree_kind(c) == T_COND_STMT);
   fail_unless(tree_stmts(c) == 1);
   s0 = tree_stmt(c, 0);
   fail_unless(tree_kind(s0) == T_SIGNAL_ASSIGN);
   fail_unless(tree_waveforms(s0) == 1);
   fail_if(tree_has_value(c));
   fail_unless(tree_kind(tree_value(tree_waveform(s0, 0))) == T_FCALL);

   s = tree_stmt(a, 1);
   fail_unless(tree_kind(s) == T_CONCURRENT);
   s = tree_stmt(s, 0);
   fail_unless(tree_kind(s) == T_COND_ASSIGN);
   fail_unless(tree_kind(tree_target(s)) == T_REF);
   fail_unless(tree_conds(s) == 3);
   c = tree_cond(s, 0);
   fail_unless(tree_kind(c) == T_COND_STMT);
   s0 = tree_stmt(c, 0);
   fail_unless(tree_kind(s0) == T_SIGNAL_ASSIGN);
   fail_unless(tree_waveforms(s0) == 1);
   fail_unless(tree_has_value(c));
   fail_unless(tree_kind(tree_value(tree_waveform(s0, 0))) == T_REF);

   s = tree_stmt(a, 2);
   fail_unless(tree_kind(s) == T_CONCURRENT);
   s = tree_stmt(s, 0);
   fail_unless(tree_kind(s) == T_SELECT);
   fail_unless(tree_stmts(s) == 3);
   fail_unless(tree_kind(tree_stmt(s, 0)) == T_ALTERNATIVE);
   fail_unless(tree_assocs(tree_stmt(s, 0)) == 1);

   s = tree_stmt(a, 3);
   fail_unless(tree_kind(s) == T_CONCURRENT);
   s = tree_stmt(s, 0);
   fail_unless(tree_kind(s) == T_PCALL);
   fail_unless(tree_params(s) == 2);

   s = tree_stmt(a, 4);
   fail_unless(tree_kind(s) == T_CONCURRENT);
   s = tree_stmt(s, 0);
   fail_unless(tree_kind(s) == T_ASSERT);

   s = tree_stmt(a, 5);
   fail_unless(tree_kind(s) == T_CONCURRENT);
   fail_unless(tree_has_ident(s));
   s = tree_stmt(s, 0);
   fail_unless(tree_kind(s) == T_COND_ASSIGN);
   fail_unless(tree_kind(tree_target(s)) == T_AGGREGATE);

   s = tree_stmt(a, 6);
   fail_unless(tree_kind(s) == T_CONCURRENT);
   s = tree_stmt(s, 0);
   fail_unless(tree_kind(s) == T_PCALL);
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
   lib_put(lib_work(), e);

   a = parse();
   fail_if(a == NULL);
   fail_unless(tree_kind(a) == T_ARCH);
   fail_unless(tree_stmts(a) == 0);
   fail_unless(tree_decls(a) == 12);

   d = tree_decl(a, 6);
   fail_unless(tree_kind(d) == T_ALIAS);
   fail_unless(tree_ident(d) == ident_new("FOO"));
   fail_unless(tree_kind(tree_value(d)) == T_REF);
   fail_if(tree_has_type(d));

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
   lib_put(lib_work(), e);

   a = parse();
   fail_if(a == NULL);
   fail_unless(tree_kind(a) == T_ARCH);
   fail_unless(tree_stmts(a) == 2);

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

   d = tree_decl(a, 12);
   fail_unless(tree_kind(d) == T_ATTR_SPEC);
   fail_unless(tree_ident(d) == ident_new("FOO"));
   fail_unless(tree_ident2(d) == ident_new("'1'"));
   fail_unless(tree_kind(tree_value(d)) == T_LITERAL);
   fail_unless(tree_class(d) == C_LITERAL);

   d = tree_decl(a, 14);
   fail_unless(tree_kind(d) == T_ATTR_SPEC);
   fail_unless(tree_ident(d) == ident_new("FOO"));
   fail_unless(tree_ident2(d) == ident_new("INTEGER"));
   fail_unless(tree_kind(tree_value(d)) == T_LITERAL);
   fail_unless(tree_class(d) == C_TYPE);

   s = tree_stmt(a, 0);
   fail_unless(tree_kind(s) == T_CONCURRENT);
   s = tree_stmt(s, 0);
   fail_unless(tree_kind(s) == T_ASSERT);
   r = tree_value(s);
   fail_unless(tree_kind(r) == T_FCALL);
   fail_unless(tree_params(r) == 2);
   v = tree_value(tree_param(r, 0));
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

   const error_t expect[] = {
      { 34, "unexpected procedure while parsing subprogram specification" },
      { 41, "`\?\?' is a reserved word in VHDL-2008" },
      { 41, "unexpected error while parsing primary" },
      { -1, NULL }
   };
   expect_errors(expect);

   p = parse();
   fail_if(p == NULL);
   fail_unless(tree_kind(p) == T_PACKAGE);
   lib_put(lib_work(), p);

   d = search_decls(p, ident_new("FOO"), 0);
   fail_if(d == NULL);
   fail_unless(tree_kind(d) == T_PROC_DECL);
   fail_unless(tree_ports(d) == 2);
   fail_unless(tree_ident(d) == ident_new("FOO"));

   p = parse();
   fail_if(p == NULL);
   fail_unless(tree_kind(p) == T_PACK_BODY);
   fail_unless(tree_decls(p) == 5);

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

   check_expected_errors();
}
END_TEST

START_TEST(test_ir1045)
{
   tree_t e, a, s, q, v, c;

   input_from_file(TESTDIR "/parse/ir1045.vhd");

   e = parse();
   fail_if(e == NULL);
   fail_unless(tree_kind(e) == T_ENTITY);
   lib_put(lib_work(), e);

   a = parse();
   fail_unless(tree_kind(a) == T_ARCH);

   s = tree_stmt(a, 0);
   fail_unless(tree_kind(s) == T_CONCURRENT);
   s = tree_stmt(s, 0);
   fail_unless(tree_kind(s) == T_COND_ASSIGN);
   c = tree_cond(s, 0);
   q = tree_value(tree_waveform(tree_stmt(c, 0), 0));
   fail_unless(tree_kind(q) == T_QUALIFIED);
   v = tree_value(q);
   fail_unless(tree_kind(v) == T_AGGREGATE);

   s = tree_stmt(a, 1);
   fail_unless(tree_kind(s) == T_CONCURRENT);
   s = tree_stmt(s, 0);
   fail_unless(tree_kind(s) == T_COND_ASSIGN);
   c = tree_cond(s, 0);
   q = tree_value(tree_waveform(tree_stmt(c, 0), 0));
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
   tree_t a, s, e, s0;

   input_from_file(TESTDIR "/parse/concat.vhd");

   e = parse();
   fail_if(e == NULL);
   fail_unless(tree_kind(e) == T_ENTITY);
   lib_put(lib_work(), e);

   a = parse();
   fail_unless(tree_kind(a) == T_ARCH);

   s = tree_stmt(a, 0);
   fail_unless(tree_kind(s) == T_CONCURRENT);
   s = tree_stmt(s, 0);
   fail_unless(tree_kind(s) == T_COND_ASSIGN);

   s0 = tree_stmt(tree_cond(s, 0), 0);
   fail_unless(tree_kind(s0) == T_SIGNAL_ASSIGN);

   e = tree_value(tree_waveform(s0, 0));
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
   fail_unless(tree_kind(a) == T_STRING);
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
   fail_unless(tree_kind(a) == T_STRING);
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
   fail_unless(tree_kind(a) == T_STRING);
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
   fail_unless(tree_kind(a) == T_STRING);
   fail_unless(tree_chars(a) == 3);
   fail_unless(tree_ident(tree_char(a, 0)) == one);
   fail_unless(tree_ident(tree_char(a, 1)) == zero);
   fail_unless(tree_ident(tree_char(a, 2)) == one);

   a = tree_value(tree_decl(p, 4));
   fail_unless(tree_kind(a) == T_STRING);
   fail_unless(tree_chars(a) == 4);
   fail_unless(tree_ident(tree_char(a, 0)) == one);
   fail_unless(tree_ident(tree_char(a, 1)) == one);
   fail_unless(tree_ident(tree_char(a, 2)) == one);
   fail_unless(tree_ident(tree_char(a, 3)) == one);

   a = tree_value(tree_decl(p, 5));
   fail_unless(tree_kind(a) == T_STRING);
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
   fail_unless(tree_kind(a) == T_STRING);
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
   fail_unless(tree_kind(a) == T_STRING);
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
   fail_unless(tree_kind(a) == T_STRING);
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
   fail_unless(tree_kind(a) == T_STRING);
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
   fail_unless(tree_kind(a) == T_STRING);
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
   fail_unless(tree_kind(a) == T_STRING);
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
   fail_unless(tree_kind(a) == T_STRING);
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
   fail_unless(tree_kind(a) == T_STRING);
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
   lib_put(lib_work(), e);

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
   tree_t e, a, g, i, c;

   input_from_file(TESTDIR "/parse/generate.vhd");

   e = parse();
   fail_if(e == NULL);
   fail_unless(tree_kind(e) == T_ENTITY);
   lib_put(lib_work(), e);

   a = parse();
   fail_if(a == NULL);
   fail_unless(tree_kind(a) == T_ARCH);
   fail_unless(tree_stmts(a) == 7);

   g = tree_stmt(a, 0);
   fail_unless(tree_kind(g) == T_IF_GENERATE);
   fail_unless(tree_conds(g) == 1);
   fail_unless(icmp(tree_ident(g), "G1"));

   c = tree_cond(g, 0);
   fail_unless(tree_decls(c) == 1);
   fail_unless(tree_stmts(c) == 1);

   g = tree_stmt(a, 1);
   fail_unless(tree_kind(g) == T_IF_GENERATE);
   fail_unless(icmp(tree_ident(g), "G2"));

   c = tree_cond(g, 0);
   fail_unless(tree_decls(c) == 0);
   fail_unless(tree_stmts(c) == 1);

   g = tree_stmt(c, 0);
   fail_unless(tree_kind(g) == T_IF_GENERATE);
   fail_unless(icmp(tree_ident(g), "G2A"));

   c = tree_cond(g, 0);
   fail_unless(tree_decls(c) == 0);
   fail_unless(tree_stmts(c) == 1);

   g = tree_stmt(a, 2);
   fail_unless(tree_kind(g) == T_FOR_GENERATE);
   fail_unless(tree_decls(g) == 2);
   fail_unless(tree_stmts(g) == 1);
   fail_unless(icmp(tree_ident(g), "G3"));

   i = tree_decl(g, 0);
   fail_unless(tree_kind(i) == T_GENERIC_DECL);
   fail_unless(icmp(tree_ident(i), "I"));
   fail_unless(tree_class(i) == C_CONSTANT);
   fail_unless(tree_subkind(i) == PORT_IN);

   g = tree_stmt(a, 3);
   fail_unless(tree_kind(g) == T_FOR_GENERATE);
   fail_unless(tree_decls(g) == 1);
   fail_unless(tree_stmts(g) == 0);
   fail_unless(icmp(tree_ident(g), "G4"));

   i = tree_decl(g, 0);
   fail_unless(tree_kind(i) == T_GENERIC_DECL);
   fail_unless(icmp(tree_ident(i), "I"));

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
   lib_put(lib_work(), e);

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
   lib_put(lib_work(), e);

   e = parse();
   fail_if(e == NULL);
   fail_unless(tree_kind(e) == T_ENTITY);
   lib_put(lib_work(), e);

   a = parse();
   fail_if(a == NULL);
   fail_unless(tree_kind(a) == T_ARCH);
   lib_put(lib_work(), a);

   c = parse();
   fail_if(c == NULL);
   fail_unless(tree_kind(c) == T_CONFIGURATION);
   lib_put(lib_work(), c);

   a = parse();
   fail_if(a == NULL);
   fail_unless(tree_kind(a) == T_ARCH);
   fail_unless(tree_stmts(a) == 5);
   fail_unless(tree_decls(a) == 9);
   lib_put(lib_work(), a);

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
   fail_unless(tree_ident2(b) == ident_new("ARCH"));

   d = tree_decl(a, 6);
   fail_unless(tree_kind(d) == T_SPEC);
   fail_unless(tree_ident(d) == ident_new("X4"));
   fail_unless(tree_ident2(d) == ident_new("Y"));
   b = tree_value(d);
   fail_unless(tree_kind(b) == T_BINDING);
   fail_unless(tree_class(b) == C_ENTITY);
   fail_unless(tree_ident(b) == ident_new("WORK.FOO"));
   fail_unless(tree_ident2(b) == ident_new("ARCH"));
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
   lib_put(lib_work(), e);

   a = parse();
   fail_if(a == NULL);

   s = tree_stmt(a, 0);
   fail_unless(tree_kind(s) == T_CONCURRENT);
   s = tree_stmt(s, 0);
   fail_unless(tree_kind(s) == T_PCALL);
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
   fail_unless(tree_kind(s) == T_CONCURRENT);
   s = tree_stmt(s, 0);
   fail_unless(tree_kind(s) == T_ASSERT);
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

   const error_t expect[] = {
      { 37, "unexpected + while parsing primary" },
      { -1, NULL }
   };
   expect_errors(expect);

   e = parse();
   fail_if(e == NULL);
   fail_unless(tree_kind(e) == T_ENTITY);
   lib_put(lib_work(), e);

   p = parse();
   fail_if(p == NULL);
   fail_unless(tree_kind(p) == T_PACKAGE);
   lib_put(lib_work(), p);

   a = parse();
   fail_if(a == NULL);
   fail_unless(tree_kind(a) == T_ARCH);
   fail_unless(tree_stmts(a) == 2);

   p = tree_stmt(a, 0);
   fail_unless(tree_kind(p) == T_PROCESS);
   fail_unless(tree_stmts(p) == 14);

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

   // See note under LRM 93 section 7.2.5
   e = tree_value(tree_stmt(p, 13));
   fail_unless(tree_kind(e) == T_FCALL);
   fail_unless(tree_ident(e) == ident_new("\"+\""));
   fail_unless(tree_params(e) == 2);
   e = tree_value(tree_param(e, 0));
   fail_unless(tree_kind(e) == T_FCALL);
   fail_unless(tree_ident(e) == ident_new("\"-\""));
   fail_unless(tree_params(e) == 1);
   e = tree_value(tree_param(e, 0));
   fail_unless(tree_kind(e) == T_FCALL);
   fail_unless(tree_ident(e) == ident_new("\"*\""));
   fail_unless(tree_params(e) == 2);

   a = parse();
   fail_unless(a == NULL);

   check_expected_errors();
}
END_TEST

START_TEST(test_error)
{
   tree_t e, a;

   input_from_file(TESTDIR "/parse/error.vhd");

   e = parse();
   fail_if(e == NULL);
   fail_unless(tree_kind(e) == T_ENTITY);
   lib_put(lib_work(), e);

   const error_t expect[] = {
      {  7, "unexpected identifier while parsing concurrent procedure call "
         "statement, expecting one of ( or ;" },
      {  7, "no visible subprogram declaration for BAD" },
      { 11, "unexpected identifier while parsing concurrent procedure call "
         "statement, expecting one of ( or ;" },
      { 11, "no visible subprogram declaration for SOME" },
      { 11, "no visible subprogram declaration for BAD" },
      { 17, "unexpected ; while parsing process statement, expecting process" },
      { 23, "expected trailing process statement label to match FOO" },
      { 27, "trailing label for process statement without label" },
      { 34, "expected trailing if statement label to match MY_IF" },
      { 35, "signal X is not a formal parameter and subprogram" },
      { 35, "signal X is not a formal parameter and subprogram" },
      { 36, "expected trailing subprogram body label to match \"+\"" },
      { 41, "P1 already declared in this region" },
      { 44, "A1 already declared in this region" },
      { 47, "S1 already declared in this region" },
      { 50, "B1 already declared in this region" },
      { 56, "C1 already declared in this region" },
      { 64, "design unit NOT_HERE not found in library WORK" },
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
   tree_t e, c, s, b, a, p;

   input_from_file(TESTDIR "/parse/config.vhd");

   const error_t expect[] = {
      { 29, "no visible declaration for X" },
      { 38, "design unit CONF is not an entity" },
      { 39, "no visible declaration for ARCH" },
      { 45, "cannot find architecture BAD of entity WORK.ENT" },
      { 52, "P is not a block that can be configured" },
      { 55, "instance P not found" },
      { -1, NULL }
   };
   expect_errors(expect);

   e = parse();
   fail_if(e == NULL);
   fail_unless(tree_kind(e) == T_ENTITY);
   lib_put(lib_work(), e);

   e = parse();
   fail_if(e == NULL);
   fail_unless(tree_kind(e) == T_ENTITY);
   lib_put(lib_work(), e);

   a = parse();
   fail_if(a == NULL);
   fail_unless(tree_kind(a) == T_ARCH);
   lib_put(lib_work(), a);

   a = parse();
   fail_if(a == NULL);
   fail_unless(tree_kind(a) == T_ARCH);
   lib_put(lib_work(), a);

   c = parse();
   fail_if(c == NULL);
   fail_unless(tree_kind(c) == T_CONFIGURATION);
   lib_put(lib_work(), c);

   c = parse();
   fail_if(c == NULL);
   fail_unless(tree_kind(c) == T_CONFIGURATION);
   fail_unless(tree_ident(c) == ident_new("WORK.CONF"));
   fail_unless(tree_ident2(c) == ident_new("ENT"));
   fail_unless(tree_decls(c) == 3);
   fail_unless(tree_kind(tree_decl(c, 0)) == T_USE);
   fail_unless(tree_kind(tree_decl(c, 1)) == T_ATTR_SPEC);
   lib_put(lib_work(), c);

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
   lib_put(lib_work(), c);

   c = parse();
   fail_if(c == NULL);
   fail_unless(tree_kind(c) == T_CONFIGURATION);
   lib_put(lib_work(), c);

   c = parse();
   fail_if(c == NULL);
   fail_unless(tree_kind(c) == T_CONFIGURATION);
   lib_put(lib_work(), c);

   p = parse();
   fail_if(p == NULL);
   fail_unless(tree_kind(p) == T_PACKAGE);
   lib_put(lib_work(), p);

   a = parse();
   fail_if(a == NULL);
   fail_unless(tree_kind(a) == T_ARCH);
   lib_put(lib_work(), a);

   c = parse();
   fail_if(c == NULL);
   fail_unless(tree_kind(c) == T_CONFIGURATION);
   lib_put(lib_work(), c);

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
   lib_put(lib_work(), p);

   d = search_decls(p, ident_new("SHAREDCOUNTER"), 0);
   fail_if(d == NULL);
   fail_unless(tree_kind(d) == T_PROT_DECL);
   t = tree_type(d);
   fail_unless(type_kind(t) == T_PROTECTED);
   fail_unless(type_fields(t) == 3);

   s = type_field(t, 0);
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
   lib_put(lib_work(), e);

   tree_t a = parse();
   fail_if(a == NULL);
   fail_unless(tree_kind(a) == T_ARCH);

   tree_t s = tree_message(tree_stmt(tree_stmt(a, 0), 0));
   fail_unless(tree_kind(s) == T_STRING);
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
   lib_put(lib_work(), p1);

   tree_t p2 = parse();
   fail_if(p2 == NULL);
   fail_unless(tree_kind(p2) == T_PACKAGE);
   lib_put(lib_work(), p2);

   tree_t c1 = parse();
   fail_if(c1 == NULL);
   fail_unless(tree_kind(c1) == T_CONTEXT);
   fail_unless(tree_ident(c1) == ident_new("WIDGET_LIB.WIDGET_CONTEXT"));
   fail_unless(tree_contexts(c1) == 5);
   tree_t u3 = tree_context(c1, 3);
   fail_unless(tree_kind(u3) == T_USE);
   fail_unless(tree_ident(u3) == ident_new("WIDGET_LIB.WIDGET_DEFS"));
   fail_unless(tree_ident2(u3) == well_known(W_ALL));
   fail_unless(tree_ref(u3) == p1);
   lib_put(lib_work(), c1);

   lib_t project = lib_tmp("project");
   lib_set_work(project);

   tree_t c2 = parse();
   fail_if(c2 == NULL);
   fail_unless(tree_kind(c2) == T_CONTEXT);
   fail_unless(tree_ident(c2) == ident_new("PROJECT.DONGLE_CONTEXT"));
   fail_unless(tree_contexts(c2) == 5);
   lib_put(lib_work(), c2);

   tree_t r = tree_context(c2, 4);
   fail_unless(tree_kind(r) == T_CONTEXT_REF);
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

   for (int i = 0; i < 8; i++) {
      tree_t t = parse();
      fail_if(t == NULL);
      lib_put(lib_work(), t);
   }

   fail_unless(parse() == NULL);
   check_expected_errors();
}
END_TEST

START_TEST(test_guarded)
{
   input_from_file(TESTDIR "/parse/guarded.vhd");

   const error_t expect[] = {
      {  7, "guarded assignment has no visible guard signal" },
      {  9, "guarded assignment has no visible guard signal" },
      { 25, "Q in disconnection specification must denote a guarded" },
      { -1, NULL }
   };
   expect_errors(expect);

   tree_t e = parse();
   fail_if(e == NULL);
   fail_unless(tree_kind(e) == T_ENTITY);
   lib_put(lib_work(), e);

   tree_t a = parse();
   fail_if(a == NULL);
   fail_unless(tree_kind(a) == T_ARCH);
   fail_unless(tree_stmts(a) == 4);

   tree_t b2 = tree_stmt(a, 2);
   fail_unless(tree_kind(b2) == T_BLOCK);
   fail_unless(tree_stmts(b2) == 3);
   fail_unless(tree_decls(b2) == 1);

   tree_t g = tree_decl(b2, 0);
   fail_unless(tree_kind(g) == T_IMPLICIT_SIGNAL);
   fail_unless(tree_ident(g) == ident_new("GUARD"));

   tree_t s0 = tree_stmt(b2, 0);
   fail_unless(tree_kind(s0) == T_CONCURRENT);
   fail_unless(tree_has_guard(s0));

   tree_t gref = tree_guard(s0);
   fail_unless(tree_kind(gref) == T_GUARD);
   fail_unless(tree_ref(gref) == g);

   tree_t s1 = tree_stmt(b2, 1);
   fail_unless(tree_kind(s1) == T_CONCURRENT);
   fail_unless(tree_kind(tree_stmt(s1, 0)) == T_SELECT);
   fail_unless(tree_has_guard(s1));

   tree_t b3 = tree_stmt(a, 3);
   fail_unless(tree_kind(b3) == T_BLOCK);
   fail_unless(tree_stmts(b3) == 2);
   fail_unless(tree_decls(b3) == 3);

   tree_t d2 = tree_decl(b3, 2);
   fail_unless(tree_kind(d2) == T_DISCONNECT);
   fail_unless(tree_ident(d2) == ident_new("Q"));
   fail_unless(type_eq(tree_type(d2), std_type(NULL, STD_BIT)));
   fail_unless(tree_has_ref(d2));
   fail_unless(tree_ref(d2) == tree_decl(b3, 1));

   fail_unless(parse() == NULL);
   check_expected_errors();
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
      { 38, "unexpected end of file while parsing package declaration" },
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
   lib_put(lib_work(), e);

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
   lib_put(lib_work(), p);

   tree_t b = parse();
   fail_if(b == NULL);
   fail_unless(tree_kind(b) == T_PACK_BODY);
   lib_put(lib_work(), b);

   tree_t f = tree_decl(b, 0);
   fail_unless(tree_kind(f) == T_FUNC_BODY);

   tree_t d = tree_decl(f, 1);
   fail_unless(tree_kind(d) == T_VAR_DECL);
   fail_unless(tree_ident(d) == ident_new("I"));

   type_t type = tree_type(d);
   fail_unless(type_kind(type) == T_SUBTYPE);

   tree_t c = type_constraint(type, 0);
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
   lib_put(lib_work(), e);

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
   lib_put(lib_work(), e0);

   tree_t a0 = parse();
   fail_if(a0 == NULL);
   fail_unless(tree_kind(a0) == T_ARCH);
   lib_put(lib_work(), a0);

   tree_t c0 = parse();
   fail_if(c0 == NULL);
   fail_unless(tree_kind(c0) == T_CONFIGURATION);
   fail_unless(tree_primary(c0) == e0);
   lib_put(lib_work(), c0);

   tree_t e1 = parse();
   fail_if(e1 == NULL);
   fail_unless(tree_kind(e1) == T_ENTITY);
   lib_put(lib_work(), e1);

   tree_t a1 = parse();
   fail_if(a1 == NULL);
   fail_unless(tree_kind(a1) == T_ARCH);
   lib_put(lib_work(), a1);

   tree_t c = parse();
   fail_if(c == NULL);
   fail_unless(tree_kind(c) == T_CONFIGURATION);
   fail_unless(tree_decls(c) == 1);
   fail_unless(tree_primary(c) == e1);
   lib_put(lib_work(), c);

   tree_t b = tree_decl(c, 0);
   fail_unless(tree_kind(b) == T_BLOCK_CONFIG);
   fail_unless(tree_decls(b) == 1);
   fail_unless(tree_ref(b) == a1);

   tree_t b2 = tree_decl(b, 0);
   fail_unless(tree_kind(b2) == T_BLOCK_CONFIG);
   fail_unless(tree_decls(b2) == 3);
   fail_unless(tree_has_ref(b2));
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
   input_from_file(TESTDIR "/parse/synth.vhd");

   tree_t e = parse();
   fail_if(e == NULL);
   fail_unless(tree_kind(e) == T_ENTITY);
   lib_put(lib_work(), e);

   tree_t a = parse();
   fail_if(a == NULL);
   fail_unless(tree_kind(a) == T_ARCH);
   fail_unless(tree_decls(a) == 2);
   fail_unless(tree_ident(tree_decl(a, 1)) == ident_new("Y"));
   fail_unless(tree_stmts(a) == 2);
   fail_unless(tree_pragmas(a) == 4);

   tree_t p0 = tree_pragma(a, 0);
   fail_unless(tree_kind(p0) == T_PRAGMA);
   fail_unless(tree_subkind(p0) == PRAGMA_SYNTHESIS_OFF);

   fail_if(parse() != NULL);

   fail_if_errors();
}
END_TEST;

START_TEST(test_issue388)
{
   input_from_file(TESTDIR "/parse/issue388.vhd");

   const error_t expect[] = {
      { 11, "unexpected => while parsing slice name, expecting one of" },
      { 12, "expected concurrent statement" },
      { 14, "expected concurrent statement" },
      { -1, NULL }
   };
   expect_errors(expect);

   tree_t e = parse();
   fail_if(e == NULL);
   fail_unless(tree_kind(e) == T_ENTITY);
   lib_put(lib_work(), e);

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
      {  42, "ambiguous use of enumeration literal '1'" },
      {  46, "no possible overload of FUNC5 has formal Z" },
      {  46, "ambiguous use of enumeration literal '1'" },
      {  47, "type of string literal cannot be determined from the" },
      {  50, "type of aggregate cannot be determined from the surrounding" },
      {   0, "could be BIT_VECTOR or BOOL_VECTOR" },
      {   0, "context contains overload FUNC6 [BIT_VECTOR return INTEGER]" },
      {   0, "context contains overload FUNC6 [BOOL_VECTOR return INTEGER]" },
      {  51, "type of aggregate cannot be determined from the surrounding" },
      {  54, "no matching subprogram FUNC7 [universal_integer return INT" },
      {   0, "no implicit conversion was performed on the first argument" },
      {  56, "type of string literal cannot be determined from the" },
      {  69, "PROC4 [INTEGER] already declared in this region" },
      {  84, "PROC8 already declared in this region" },
      {  92, "ambiguous use of enumeration literal '1'" },
      {  96, "no possible overload of PROC5 has formal Z" },
      {  96, "ambiguous use of enumeration literal '1'" },
      {  97, "type of string literal cannot be determined from the" },
      { 100, "type of aggregate cannot be determined from the surrounding" },
      { 101, "type of aggregate cannot be determined from the surrounding" },
      { 104, "no matching subprogram PROC7 [universal_integer]" },
      {   0, "no implicit conversion was performed on the first argument" },
      { 106, "type of string literal cannot be determined from the" },
      {   0, "could be BIT_VECTOR or STRING" },
      { 107, "expected procedure name" },
      { 108, "no visible subprogram declaration for FOO" },
      { 222, "ambiguous use of name FOO" },
      { 222, "ambiguous use of name FOO" },
      { 233, "name X not found in \"+\"" },
      { 256, "no visible subprogram declaration for NOTHERE" },
      { 313, "no visible subprogram declaration for FNORK" },
      { 323, "no matching subprogram P26_1 [universal_integer" },
      { 332, "no matching operator \"and\" [BIT, BOOLEAN return BOOLEAN]" },
      { 360, "object X with type INTEGER cannot be selected" },
      { 362, "no visible declaration for FOO" },
      { 386, "expecting type mark while parsing qualified expression" },
      {  -1, NULL }
   };
   expect_errors(expect);

   tree_t e = parse();
   fail_if(e == NULL);
   fail_unless(tree_kind(e) == T_ENTITY);
   lib_put(lib_work(), e);

   tree_t a = parse();
   fail_if(a == NULL);
   fail_unless(tree_kind(a) == T_ARCH);

   type_t std_int = std_type(NULL, STD_INTEGER);

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
   fail_unless(type_eq(tree_type(t), std_type(NULL, STD_INTEGER)));

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
   fail_unless(tree_ident2(d) == ident_new("WORK.EE-AA.B8.GET_FOO()I"));

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
   opt_set_int(OPT_MISSING_BODY, 1);
   input_from_file(TESTDIR "/parse/names.vhd");

   fail_if_errors();
}
END_TEST

START_TEST(test_error2)
{
   set_standard(STD_00);

   input_from_file(TESTDIR "/parse/error2.vhd");

   const error_t expect[] = {
      {  1, "design unit DUNNO not found in library WORK" },
      {  2, "no visible declaration for BAR" },
      {  5, "no visible declaration for SDFF" },
      { 10, "no visible declaration for SGHBBX" },
      { 17, "design unit NOTHERE not found in library STD" },
      { 22, "unexpected identifier while parsing range" },
      { 29, "expected physical type definition trailing" },
      { 33, "expected record type definition trailing identifier" },
      { 38, "unexpected procedure while parsing subprogram body" },
      { 42, "unexpected function while parsing subprogram body" },
      { 45, "trailing protected type declaration label to match OTHER" },
      { 47, "unexpected integer while parsing subtype declaration" },
      { 53, "no visible subprogram declaration for F" },
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
      {  84, "no matching operator \"+\" [DELAY_LENGTH, TIME" },
      { 111, "excess non-zero digits in bit string literal" },
      { 112, "excess non-zero digits in bit string literal" },
      { 121, "excess significant digits in bit string literal" },
      { 124, "invalid digit 'C' in decimal bit string" },
      { 127, "excess non-zero digits in decimal bit string literal" },
      { 171, "unexpected ; while parsing case statement, expecting ?" },
      { 181, "prefix of 'SUBTYPE attribute does not have a type" },
      { 183, "prefix of 'ELEMENT attribute must be an array type" },
      { 184, "prefix of 'ELEMENT attribute does not have a type" },
      { 230, "unexpected trailing label for generate statement body without" },
      { 232, "expected trailing generate statement body label to match FOO" },
      { 249, "expected trailing case generate statement label to match G3" },
      { 264, "signed bit string literal cannot be an empty string" },
      { 282, "the reserved word INERTIAL can only be used in port map " },
      { 290, "the reserved word INERTIAL can only be used in port map " },
      { -1, NULL }
   };
   expect_errors(expect);

   tree_t e = parse();
   fail_if(e == NULL);
   fail_unless(tree_kind(e) == T_ENTITY);
   lib_put(lib_work(), e);

   tree_t p = parse();
   fail_if(p == NULL);
   fail_unless(tree_kind(p) == T_PACKAGE);
   fail_unless(tree_generics(p) == 2);
   fail_unless(tree_genmaps(p) == 2);
   lib_put(lib_work(), p);

   tree_t p2 = parse();
   fail_if(p2 == NULL);
   fail_unless(tree_kind(p2) == T_PACKAGE);
   fail_unless(package_needs_body(p2));
   lib_put(lib_work(), p2);

   tree_t b = parse();
   fail_if(b == NULL);
   fail_unless(tree_kind(b) == T_PACK_BODY);
   fail_unless(tree_primary(b) == p2);
   lib_put(lib_work(), b);

   tree_t p3 = parse();
   fail_if(p3 == NULL);
   fail_unless(tree_kind(p3) == T_PACK_INST);
   fail_unless(tree_generics(p3) == 2);
   fail_unless(tree_genmaps(p3) == 2);
   fail_unless(tree_decls(p3) == 2);
   fail_unless(tree_kind(tree_decl(p3, 1)) == T_FUNC_BODY);
   lib_put(lib_work(), p3);

   tree_t a = parse();
   fail_if(a == NULL);
   fail_unless(tree_kind(a) == T_ARCH);

   tree_t d = search_decls(a, ident_new("MY_TYPE_VECTOR"), 0);
   fail_if(d == NULL);
   type_t type = tree_type(d);
   fail_unless(type_kind(type) == T_SUBTYPE);
   fail_unless(type_has_resolution(type));
   tree_t r = type_resolution(type);
   fail_unless(tree_kind(r) == T_ELEM_RESOLUTION);

   tree_t p4 = tree_stmt(a, 4);
   fail_unless(tree_chars(tree_value(tree_stmt(p4, 0))) == 8);
   fail_unless(tree_chars(tree_value(tree_stmt(p4, 1))) == 6);
   fail_unless(tree_chars(tree_value(tree_stmt(p4, 2))) == 4);
   fail_unless(tree_chars(tree_value(tree_stmt(p4, 3))) == 0);
   fail_unless(tree_chars(tree_value(tree_stmt(p4, 4))) == 0);
   fail_unless(tree_chars(tree_value(tree_stmt(p4, 5))) == 18);
   fail_unless(tree_chars(tree_value(tree_stmt(p4, 6))) == 0);
   fail_unless(tree_chars(tree_value(tree_stmt(p4, 7))) == 3);

   tree_t p6 = tree_stmt(a, 6);
   fail_unless(tree_kind(p6) == T_PROCESS);
   type_t p6t0 = tree_type(tree_decl(p6, 0));
   fail_unless(type_is_unconstrained(p6t0));
   type_t p6t1 = tree_type(search_decls(p6, ident_new("A"), 0));
   fail_if(type_is_unconstrained(p6t1));
   fail_unless(type_constraints(p6t1) == 1);
   type_t p6t1e = type_elem(p6t1);
   fail_unless(type_kind(p6t1e) == T_SUBTYPE);
   fail_unless(type_constraints(p6t1e) == 1);

   fail_unless(parse() == NULL);

   check_expected_errors();
}
END_TEST

START_TEST(test_vhdl2019)
{
   set_standard(STD_19);
   input_from_file(TESTDIR "/parse/vhdl2019.vhd");

   tree_t p006f = parse();
   fail_if(p006f == NULL);
   fail_unless(tree_kind(p006f) == T_PACKAGE);

   lib_put(lib_work(), p006f);

   tree_t pb006f = parse();
   fail_if(pb006f == NULL);
   fail_unless(tree_kind(pb006f) == T_PACK_BODY);

   tree_t p055a = parse();
   fail_if(p055a == NULL);
   fail_unless(tree_kind(p055a) == T_PACKAGE);

   tree_t e071a = parse();
   fail_if(e071a == NULL);
   fail_unless(tree_kind(e071a) == T_ENTITY);

   tree_t p072b = parse();
   fail_if(p072b == NULL);
   fail_unless(tree_kind(p072b) == T_PACKAGE);

   lib_put(lib_work(), p072b);

   tree_t pb072b = parse();
   fail_if(pb072b == NULL);
   fail_unless(tree_kind(pb072b) == T_PACK_BODY);

   tree_t p082 = parse();
   fail_if(p082 == NULL);
   fail_unless(tree_kind(p082) == T_PACKAGE);

   tree_t e086 = parse();
   fail_if(e086 == NULL);
   fail_unless(tree_kind(e086) == T_ENTITY);

   tree_t e059 = parse();
   fail_if(e059 == NULL);
   fail_unless(tree_kind(e059) == T_ENTITY);

   tree_t e059g0 = tree_generic(e059, 0);
   fail_unless(tree_class(e059g0) == C_TYPE);
   fail_unless(type_subkind(tree_type(e059g0)) == GTYPE_PRIVATE);

   tree_t e059g3 = tree_generic(e059, 3);
   fail_unless(tree_class(e059g3) == C_TYPE);
   fail_unless(type_subkind(tree_type(e059g3)) == GTYPE_SCALAR);

   tree_t e059g6 = tree_generic(e059, 13);
   fail_unless(tree_class(e059g6) == C_TYPE);
   fail_unless(type_subkind(tree_type(e059g6)) == GTYPE_DISCRETE);

   tree_t e059g9 = tree_generic(e059, 23);
   fail_unless(tree_class(e059g9) == C_TYPE);
   fail_unless(type_subkind(tree_type(e059g9)) == GTYPE_INTEGER);

   tree_t e059g12 = tree_generic(e059, 43);
   fail_unless(tree_class(e059g12) == C_TYPE);
   fail_unless(type_subkind(tree_type(e059g12)) == GTYPE_PHYSICAL);

   tree_t e059g15 = tree_generic(e059, 60);
   fail_unless(tree_class(e059g15) == C_TYPE);
   fail_unless(type_subkind(tree_type(e059g15)) == GTYPE_FLOATING);

   tree_t e059g18 = tree_generic(e059, 77);
   fail_unless(tree_class(e059g18) == C_TYPE);
   fail_unless(type_subkind(tree_type(e059g18)) == GTYPE_ARRAY);

   tree_t e016 = parse();
   fail_if(e016 == NULL);
   fail_unless(tree_kind(e016) == T_ENTITY);
   fail_unless(tree_generics(e016) == 1);

   tree_t e016g0 = tree_generic(e016, 0);
   fail_unless(tree_class(e016g0) == C_TYPE);

   fail_unless(parse() == NULL);

   fail_if_errors();
}
END_TEST

START_TEST(test_issue416)
{
   opt_set_int(OPT_RELAXED, 1);
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
      { -1, NULL }
   };
   expect_errors(expect);

   tree_t p1 = parse();
   fail_if(p1 == NULL);
   fail_unless(tree_kind(p1) == T_PACKAGE);
   lib_put(lib_work(), p1);

   tree_t p2 = parse();
   fail_if(p2 == NULL);
   fail_unless(tree_kind(p2) == T_PACKAGE);
   lib_put(lib_work(), p2);

   tree_t e = parse();
   fail_if(e == NULL);
   fail_unless(tree_kind(e) == T_ENTITY);
   lib_put(lib_work(), e);

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
   lib_put(lib_work(), p1);

   tree_t p2 = parse();
   fail_if(p2 == NULL);
   fail_unless(tree_kind(p2) == T_PACKAGE);
   lib_put(lib_work(), p2);

   tree_t e = parse();
   fail_if(e == NULL);
   fail_unless(tree_kind(e) == T_ENTITY);
   lib_put(lib_work(), e);

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
   lib_put(lib_work(), p1);

   tree_t e = parse();
   fail_if(e == NULL);
   fail_unless(tree_kind(e) == T_ENTITY);
   lib_put(lib_work(), e);

   tree_t a = parse();
   fail_if(a == NULL);
   fail_unless(tree_kind(a) == T_ARCH);

   fail_unless(parse() == NULL);

   fail_if_errors();
}
END_TEST

START_TEST(test_group)
{
   input_from_file(TESTDIR "/parse/group.vhd");

   tree_t e = parse();
   fail_if(e == NULL);
   fail_unless(tree_kind(e) == T_ENTITY);
   lib_put(lib_work(), e);

   tree_t a = parse();
   fail_if(a == NULL);
   fail_unless(tree_kind(a) == T_ARCH);

   fail_unless(tree_decls(a) == 3);

   fail_unless(parse() == NULL);

   fail_if_errors();
}
END_TEST

START_TEST(test_alias2)
{
   input_from_file(TESTDIR "/parse/alias2.vhd");

   tree_t p1 = parse();
   fail_if(p1 == NULL);
   fail_unless(tree_kind(p1) == T_PACKAGE);
   lib_put(lib_work(), p1);

   tree_t p2 = parse();
   fail_if(p2 == NULL);
   fail_unless(tree_kind(p2) == T_PACKAGE);
   lib_put(lib_work(), p2);

   tree_t e = parse();
   fail_if(e == NULL);
   fail_unless(tree_kind(e) == T_ENTITY);
   lib_put(lib_work(), e);

   tree_t a = parse();
   fail_if(a == NULL);
   fail_unless(tree_kind(a) == T_ARCH);

   fail_unless(parse() == NULL);

   fail_if_errors();
}
END_TEST

START_TEST(test_badprimary)
{
   input_from_file(TESTDIR "/parse/badprimary.vhd");

   const error_t expect[] = {
      { 17, "design unit NOT_HERE not found in library WORK" },
      { 26, "depends on WORK.NOT_HERE-BAD which was analysed with errors" },
      { -1, NULL }
   };
   expect_errors(expect);

   tree_t top = parse();
   fail_if(top == NULL);
   fail_unless(tree_kind(top) == T_ENTITY);
   lib_put(lib_work(), top);

   tree_t a = parse();
   fail_if(a == NULL);
   fail_unless(tree_kind(a) == T_ARCH);
   lib_put(lib_work(), a);

   tree_t bad = parse();
   fail_if(bad == NULL);
   fail_unless(tree_kind(bad) == T_ARCH);
   fail_if(tree_has_primary(bad));
   lib_put_error(lib_work(), bad);

   tree_t cfg = parse();
   fail_if(cfg == NULL);
   fail_unless(tree_kind(cfg) == T_CONFIGURATION);

   fail_unless(parse() == NULL);

   check_expected_errors();
}
END_TEST

START_TEST(test_error3)
{
   set_standard(STD_93);

   input_from_file(TESTDIR "/parse/error3.vhd");

   const error_t expect[] = {
      { 15, "`protected' is a reserved word in VHDL-2000" },
      { 15, "unexpected identifier while parsing type definition" },
      { 26, "unexpected ; while parsing library unit, expecting one" },
      { 28, "unexpected end while parsing library unit, expecting" },
      { -1, NULL }
   };
   expect_errors(expect);

   tree_t p = parse();
   fail_if(p == NULL);
   fail_unless(tree_kind(p) == T_PACKAGE);

   fail_unless(parse() == NULL);

   fail_unless(parse() == NULL);

   check_expected_errors();
}
END_TEST

START_TEST(test_protected2)
{
   set_standard(STD_08);
   input_from_file(TESTDIR "/parse/protected2.vhd");

   tree_t p = parse();
   fail_if(p == NULL);
   fail_unless(tree_kind(p) == T_PACKAGE);
   lib_put(lib_work(), p);

   tree_t b = parse();
   fail_if(b == NULL);
   fail_unless(tree_kind(b) == T_PACK_BODY);

   tree_t pb = search_decls(b, ident_new("RANDOMPTYPE"), 0);
   fail_if(pb == NULL);
   fail_unless(tree_kind(pb) == T_PROT_BODY);

   tree_t f = search_decls(pb, ident_new("LOCALUNIFORM"), 0);
   fail_if(f == NULL);
   fail_unless(tree_kind(f) == T_FUNC_BODY);

   tree_t s0 = tree_stmt(f, 0);
   fail_unless(tree_kind(s0) == T_RETURN);

   tree_t call = tree_value(tree_param(tree_value(s0), 0));
   fail_unless(tree_kind(call) == T_PROT_FCALL);
   fail_unless(tree_params(call) == 0);

   fail_unless(parse() == NULL);

   fail_if_errors();
}
END_TEST

START_TEST(test_error4)
{
   set_standard(STD_93);

   input_from_file(TESTDIR "/parse/error4.vhd");

   const error_t expect[] = {
      {  2, "no visible declaration for IEEE" },
      { -1, NULL }
   };
   expect_errors(expect);

   tree_t p = parse();
   fail_if(p == NULL);
   fail_unless(tree_kind(p) == T_PACKAGE);
   lib_put(lib_work(), p);

   tree_t b = parse();
   fail_if(b == NULL);
   fail_unless(tree_kind(b) == T_PACK_BODY);

   fail_unless(parse() == NULL);

   check_expected_errors();
}
END_TEST

START_TEST(test_tc1012)
{
   input_from_file(TESTDIR "/parse/tc1012.vhd");

   tree_t e = parse();
   fail_if(e == NULL);
   fail_unless(tree_kind(e) == T_ENTITY);
   lib_put(lib_work(), e);

   tree_t a = parse();
   fail_if(a == NULL);
   fail_unless(tree_kind(a) == T_ARCH);

   tree_t s0 = tree_stmt(a, 0);
   fail_unless(tree_kind(s0) == T_PROCESS);
   fail_unless(tree_triggers(s0) == 2);
   fail_unless(tree_kind(tree_trigger(s0, 0)) == T_REF);
   fail_unless(tree_ident(tree_trigger(s0, 0)) ==
               ident_new("C06S03B00X00P10N01I01012ENT.P"));

   fail_unless(parse() == NULL);

   fail_if_errors();
}
END_TEST

START_TEST(test_issue443)
{
   input_from_file(TESTDIR "/parse/issue443.vhd");

   tree_t p = parse();
   fail_if(p == NULL);
   fail_unless(tree_kind(p) == T_PACKAGE);
   lib_put(lib_work(), p);

   tree_t e = parse();
   fail_if(e == NULL);
   fail_unless(tree_kind(e) == T_ENTITY);
   lib_put(lib_work(), e);

   tree_t a = parse();
   fail_if(a == NULL);
   fail_unless(tree_kind(a) == T_ARCH);

   tree_t d = tree_decl(a, 1);
   fail_unless(tree_kind(d) == T_CONST_DECL);
   fail_unless(tree_ident(d) == ident_new("PARAM"));

   tree_t f = tree_ref(tree_value(d));
   fail_unless(tree_kind(f) == T_FUNC_DECL);
   fail_unless(tree_ports(f) == 3);
   fail_unless(tree_loc(f)->first_line == 28);

   fail_unless(parse() == NULL);

   fail_if_errors();
}
END_TEST

START_TEST(test_vunit5)
{
   input_from_file(TESTDIR "/parse/vunit5.vhd");

   tree_t e = parse();
   fail_if(e == NULL);
   fail_unless(tree_kind(e) == T_ENTITY);
   lib_put(lib_work(), e);

   tree_t a = parse();
   fail_if(a == NULL);
   fail_unless(tree_kind(a) == T_ARCH);
   lib_put(lib_work(), a);

   tree_t d = tree_decl(a, 0);
   fail_unless(tree_kind(d) == T_SIGNAL_DECL);
   fail_unless(tree_ident(d) == ident_new("X"));

   tree_t r = tree_value(d);
   fail_unless(tree_kind(r) == T_REF);
   fail_unless(tree_kind(tree_ref(r)) == T_GENERIC_DECL);

   fail_unless(parse() == NULL);

   fail_if_errors();
}
END_TEST

START_TEST(test_vunit6)
{
   set_standard(STD_08);
   input_from_file(TESTDIR "/parse/vunit6.vhd");

   lib_t foo_lib = lib_tmp("foo");
   lib_set_work(foo_lib);

   tree_t p = parse();
   fail_if(p == NULL);
   fail_unless(tree_kind(p) == T_PACKAGE);
   lib_put(lib_work(), p);

   tree_t c = parse();
   fail_if(c == NULL);
   fail_unless(tree_kind(c) == T_CONTEXT);
   lib_put(lib_work(), c);

   tree_t e = parse();
   fail_if(e == NULL);
   fail_unless(tree_kind(e) == T_ENTITY);

   fail_unless(parse() == NULL);

   fail_if_errors();
}
END_TEST

START_TEST(test_external)
{
   set_standard(STD_08);
   input_from_file(TESTDIR "/parse/external.vhd");

   tree_t e = parse();
   fail_if(e == NULL);
   fail_unless(tree_kind(e) == T_ENTITY);
   lib_put(lib_work(), e);

   tree_t a = parse();
   fail_if(a == NULL);
   fail_unless(tree_kind(a) == T_ARCH);

   tree_t p0 = tree_stmt(a, 0);

   const struct {
      const class_t class;
      const int     nparts;
      struct {
         const path_elt_t   kind;
         const char        *name;
         const tree_kind_t  treek;
      } parts[5];
   } expect[] = {
      { C_SIGNAL, 3, { { PE_RELATIVE },
                       { PE_SIMPLE, "FOO" },
                       { PE_SIMPLE, "BAR" } } },
      { C_CONSTANT, 4, { { PE_RELATIVE },
                         { PE_SIMPLE, "X" },
                         { PE_SIMPLE, "Y" },
                         { PE_SIMPLE, "Z" } } },
      { C_VARIABLE, 3, { { PE_RELATIVE },
                         { PE_SIMPLE, "AYE" },
                         { PE_SIMPLE, "BEE" } } },
      { C_CONSTANT, 4, { { PE_ABSOLUTE },
                         { PE_SIMPLE, "X" },
                         { PE_SIMPLE, "Y" },
                         { PE_SIMPLE, "Z" } } },
      { C_CONSTANT, 4, { { PE_RELATIVE },
                         { PE_CARET },
                         { PE_CARET },
                         { PE_SIMPLE, "FOO" } } },
      { C_CONSTANT, 3, { { PE_LIBRARY, "WORK" },
                         { PE_SIMPLE, "PACK" },
                         { PE_SIMPLE, "FOO" } } },
      { C_SIGNAL, 4, { { PE_RELATIVE },
                       { PE_GENERATE, "G", T_LITERAL },
                       { PE_GENERATE, "X", T_LITERAL },
                       { PE_SIMPLE, "BAZ" } } },
   };

   for (int i = 0; i < ARRAY_LEN(expect); i++) {
      tree_t v = tree_value(tree_stmt(p0, i));
      fail_unless(tree_kind(v) == T_EXTERNAL_NAME);
      ck_assert_int_eq(tree_class(v), expect[i].class);
      const int nparts = tree_parts(v);
      ck_assert_int_eq(nparts, expect[i].nparts);
      for (int j = 0; j < nparts; j++) {
         tree_t p = tree_part(v, j);
         ck_assert_int_eq(tree_subkind(p), expect[i].parts[j].kind);
         switch (expect[i].parts[j].kind) {
         case PE_SIMPLE:
         case PE_LIBRARY:
            ck_assert_str_eq(istr(tree_ident(p)), expect[i].parts[j].name);
            break;
         case PE_GENERATE:
            ck_assert_str_eq(istr(tree_ident(p)), expect[i].parts[j].name);
            ck_assert_int_eq(tree_kind(tree_value(p)),
                             expect[i].parts[j].treek);
            break;
         default:
            break;
         }
      }
   }

   fail_unless(parse() == NULL);

   fail_if_errors();
}
END_TEST

START_TEST(test_error5)
{
   input_from_file(TESTDIR "/parse/error5.vhd");

   const error_t expect[] = {
      {  2, "design unit NOTHERE not found in library WORK" },
      // It would be better to avoid the following errors
      {  8, "no visible declaration for MYTYPE" },
      {  8, "no visible declaration for MYFUNC" },
      { -1, NULL }
   };
   expect_errors(expect);

   tree_t p = parse();
   fail_if(p == NULL);
   fail_unless(tree_kind(p) == T_PACKAGE);
   lib_put(lib_work(), p);

   tree_t b = parse();
   fail_if(b == NULL);
   fail_unless(tree_kind(b) == T_PACK_BODY);

   fail_unless(parse() == NULL);

   check_expected_errors();
}
END_TEST

START_TEST(test_vunit7)
{
   input_from_file(TESTDIR "/parse/vunit7.vhd");

   const error_t expect[] = {
      { 23, "ambiguous use of enumeration literal ERROR" },
      { 23, "ambiguous use of enumeration literal FAILURE" },
      { -1, NULL }
   };
   expect_errors(expect);

   tree_t e = parse();
   fail_if(e == NULL);
   fail_unless(tree_kind(e) == T_ENTITY);
   lib_put(lib_work(), e);

   tree_t a = parse();
   fail_if(a == NULL);
   fail_unless(tree_kind(a) == T_ARCH);

   check_expected_errors();
}
END_TEST

START_TEST(test_error6)
{
   set_standard(STD_08);
   input_from_file(TESTDIR "/parse/error6.vhd");

   const error_t expect[] = {
      { 15, "unexpected end of file while parsing package declaration" },
      { -1, NULL }
   };
   expect_errors(expect);

   tree_t p = parse();
   fail_if(p == NULL);
   fail_unless(tree_kind(p) == T_PACKAGE);

   fail_unless(parse() == NULL);

   check_expected_errors();
}
END_TEST

START_TEST(test_names2)
{
   set_standard(STD_02);
   input_from_file(TESTDIR "/parse/names2.vhd");

   const error_t expect[] = {
      { -1, NULL }
   };
   expect_errors(expect);

   tree_t e = parse();
   fail_if(e == NULL);
   fail_unless(tree_kind(e) == T_ENTITY);
   lib_put(lib_work(), e);

   tree_t a = parse();
   fail_if(a == NULL);
   fail_unless(tree_kind(a) == T_ARCH);

   fail_unless(parse() == NULL);

   check_expected_errors();
}
END_TEST

START_TEST(test_issue457)
{
   set_standard(STD_08);
   input_from_file(TESTDIR "/parse/issue457.vhd");

   lib_t test_context_lib = lib_tmp("test_context");
   lib_set_work(test_context_lib);

   const error_t expect[] = {
      {  7, "TEST_CONTEXT.TEST_CONTEXT is not a library or instantiated "
         "package" },
      { -1, NULL }
   };
   expect_errors(expect);

   tree_t c = parse();
   fail_if(c == NULL);
   fail_unless(tree_kind(c) == T_CONTEXT);
   lib_put(test_context_lib, c);

   lib_t other_lib = lib_tmp("other");
   lib_set_work(other_lib);

   tree_t e = parse();
   fail_if(e == NULL);
   fail_unless(tree_kind(e) == T_ENTITY);

   fail_unless(parse() == NULL);

   check_expected_errors();
}
END_TEST

START_TEST(test_issue458)
{
   set_standard(STD_08);
   input_from_file(TESTDIR "/parse/issue458.vhd");

   tree_t p = parse();
   fail_if(p == NULL);
   fail_unless(tree_kind(p) == T_PACKAGE);

   ident_t one = ident_new("'1'");
   ident_t zero = ident_new("'0'");

   ident_t expect[5][5] = { { zero, zero, zero, zero, zero },
                            { zero, zero, zero, zero, one },
                            { zero, zero, zero, one, zero },
                            { zero, zero, zero, one, one },
                            { one, zero, one, zero, zero } };

   for (int i = 0; i < 5; i++) {
      tree_t d0 = tree_value(tree_decl(p, i));
      fail_unless(tree_kind(d0) == T_STRING);
      fail_unless(tree_chars(d0) == 5);

      for (int j = 0; j < 5; j++)
         fail_unless(tree_ident(tree_char(d0, j)) == expect[i][j]);
   }

   fail_if_errors();
}
END_TEST

START_TEST(test_issue461a)
{
   input_from_file(TESTDIR "/parse/issue461.vhd");

   const error_t expect[] = {
      { 13, "multiple conflicting visible declarations of T_TEST" },
      { -1, NULL }
   };
   expect_errors(expect);

   tree_t p1 = parse();
   fail_if(p1 == NULL);
   fail_unless(tree_kind(p1) == T_PACKAGE);
   lib_put(lib_work(), p1);

   tree_t p2 = parse();
   fail_if(p2 == NULL);
   fail_unless(tree_kind(p2) == T_PACKAGE);
   lib_put(lib_work(), p2);

   tree_t e = parse();
   fail_if(e == NULL);
   fail_unless(tree_kind(e) == T_ENTITY);

   fail_unless(parse() == NULL);

   check_expected_errors();
}
END_TEST

START_TEST(test_issue461b)
{
   set_standard(STD_08);
   input_from_file(TESTDIR "/parse/issue461.vhd");

   tree_t p1 = parse();
   fail_if(p1 == NULL);
   fail_unless(tree_kind(p1) == T_PACKAGE);
   lib_put(lib_work(), p1);

   tree_t p2 = parse();
   fail_if(p2 == NULL);
   fail_unless(tree_kind(p2) == T_PACKAGE);
   lib_put(lib_work(), p2);

   tree_t e = parse();
   fail_if(e == NULL);
   fail_unless(tree_kind(e) == T_ENTITY);

   fail_unless(parse() == NULL);

   fail_if_errors();
}
END_TEST

START_TEST(test_error7)
{
   input_from_file(TESTDIR "/parse/error7.vhd");

   const error_t expect[] = {
      {  4, "FOO already declared in this region" },
      {  7, "depends on WORK.ERROR7 which was analysed with errors" },
      { 11, "depends on WORK.ERROR7 which was analysed with errors" },
      { 17, "unexpected ; while parsing use clause, expecting ." },
      { -1, NULL }
   };
   expect_errors(expect);

   tree_t p = parse();
   fail_if(p == NULL);
   fail_unless(tree_kind(p) == T_PACKAGE);
   fail_unless(error_count() > 0);
   lib_put_error(lib_work(), p);

   tree_t b = parse();
   fail_if(b == NULL);
   fail_unless(tree_kind(b) == T_PACK_BODY);

   tree_t p2 = parse();
   fail_if(p2 == NULL);
   fail_unless(tree_kind(p2) == T_PACKAGE);

   fail_unless(parse() == NULL);

   check_expected_errors();
}
END_TEST

START_TEST(test_error8)
{
   set_standard(STD_02);
   input_from_file(TESTDIR "/parse/error8.vhd");

   const error_t expect[] = {
      { 33, "record type REC has no field named ID" },
      { -1, NULL }
   };
   expect_errors(expect);

   tree_t p = parse();
   fail_if(p == NULL);
   fail_unless(tree_kind(p) == T_PACKAGE);
   lib_put(lib_work(), p);

   tree_t b = parse();
   fail_if(b == NULL);
   fail_unless(tree_kind(b) == T_PACK_BODY);

   fail_unless(parse() == NULL);

   check_expected_errors();
}
END_TEST

START_TEST(test_vunit8)
{
   input_from_file(TESTDIR "/parse/vunit8.vhd");

   tree_t p1 = parse();
   fail_if(p1 == NULL);
   fail_unless(tree_kind(p1) == T_PACKAGE);
   lib_put(lib_work(), p1);

   tree_t p2 = parse();
   fail_if(p2 == NULL);
   fail_unless(tree_kind(p2) == T_PACKAGE);
   lib_put(lib_work(), p2);

   tree_t e = parse();
   fail_if(e == NULL);
   fail_unless(tree_kind(e) == T_ENTITY);
   lib_put(lib_work(), e);

   tree_t a = parse();
   fail_if(a == NULL);
   fail_unless(tree_kind(a) == T_ARCH);

   fail_unless(parse() == NULL);

   fail_if_errors();
}
END_TEST

START_TEST(test_issue464)
{
   input_from_file(TESTDIR "/parse/issue464.vhd");

   tree_t e = parse();
   fail_if(e == NULL);
   fail_unless(tree_kind(e) == T_ENTITY);
   lib_put(lib_work(), e);

   tree_t a = parse();
   fail_if(a == NULL);
   fail_unless(tree_kind(a) == T_ARCH);

   fail_unless(parse() == NULL);

   fail_if_errors();
}
END_TEST

START_TEST(test_osvvm6)
{
   set_standard(STD_08);
   input_from_file(TESTDIR "/parse/osvvm6.vhd");

   tree_t p1 = parse();
   fail_if(p1 == NULL);
   fail_unless(tree_kind(p1) == T_PACKAGE);
   lib_put(lib_work(), p1);

   tree_t p2 = parse();
   fail_if(p2 == NULL);
   fail_unless(tree_kind(p2) == T_PACKAGE);
   lib_put(lib_work(), p2);

   tree_t b = parse();
   fail_if(b == NULL);
   fail_unless(tree_kind(b) == T_PACK_BODY);

   fail_unless(parse() == NULL);

   fail_if_errors();
}
END_TEST

START_TEST(test_issue468)
{
   input_from_file(TESTDIR "/parse/issue468.vhd");

   const error_t expect[] = {
      {  2, "design unit TEST3 not found in library WORK" },
      { -1, NULL }
   };
   expect_errors(expect);

   tree_t p = parse();
   fail_if(p == NULL);
   fail_unless(tree_kind(p) == T_PACKAGE);

   check_expected_errors();
}
END_TEST

START_TEST(test_names3)
{
   input_from_file(TESTDIR "/parse/names3.vhd");

   const error_t expect[] = {
      { 21, "no visible subprogram declaration for BAR" },
      { -1, NULL }
   };
   expect_errors(expect);

   tree_t p = parse();
   fail_if(p == NULL);
   fail_unless(tree_kind(p) == T_PACKAGE);
   lib_put(lib_work(), p);

   tree_t e = parse();
   fail_if(e == NULL);
   fail_unless(tree_kind(e) == T_ENTITY);
   lib_put(lib_work(), e);

   tree_t a = parse();
   fail_if(a == NULL);
   fail_unless(tree_kind(a) == T_ARCH);

   fail_unless(parse() == NULL);

   check_expected_errors();
}
END_TEST

START_TEST(test_uvvm1)
{
   set_standard(STD_08);
   input_from_file(TESTDIR "/parse/uvvm1.vhd");

   tree_t p = parse();
   fail_if(p == NULL);
   fail_unless(tree_kind(p) == T_PACKAGE);
   lib_put(lib_work(), p);

   fail_unless(parse() == NULL);

   fail_if_errors();
}
END_TEST

START_TEST(test_range1)
{
   input_from_file(TESTDIR "/parse/range1.vhd");

   const error_t expect[] = {
      { 17, "object X does not have a range" },
      { 19, "name X in discrete range does not refer to a type" },
      { 21, "expecting a discrete range" },
      { 25, "expected type mark while parsing discrete range" },
      { -1, NULL }
   };
   expect_errors(expect);

   tree_t e = parse();
   fail_if(e == NULL);
   fail_unless(tree_kind(e) == T_ENTITY);
   lib_put(lib_work(), e);

   tree_t a = parse();
   fail_if(a == NULL);
   fail_unless(tree_kind(a) == T_ARCH);

   fail_unless(parse() == NULL);

   check_expected_errors();
}
END_TEST

START_TEST(test_error9)
{
   input_from_file(TESTDIR "/parse/error9.vhd");

   const error_t expect[] = {
      { 13, "object with type STRING cannot be selected" },
      { -1, NULL }
   };
   expect_errors(expect);

   tree_t e = parse();
   fail_if(e == NULL);
   fail_unless(tree_kind(e) == T_ENTITY);
   lib_put(lib_work(), e);

   tree_t a = parse();
   fail_if(a == NULL);
   fail_unless(tree_kind(a) == T_ARCH);

   fail_unless(parse() == NULL);

   check_expected_errors();
}
END_TEST

START_TEST(test_uvvm2)
{
   set_standard(STD_08);
   input_from_file(TESTDIR "/parse/uvvm2.vhd");

   const error_t expect[] = {
      { 24, "initial value T does not match type of declaration INTEGER" },
      { -1, NULL }
   };
   expect_errors(expect);

   for (int i = 0; i < 4; i++) {
      tree_t p = parse();
      fail_if(p == NULL);
      fail_unless(tree_kind(p) == T_PACKAGE);
      lib_put(lib_work(), p);
   }

   fail_unless(parse() == NULL);

   check_expected_errors();
}
END_TEST

START_TEST(test_visibility1)
{
   input_from_file(TESTDIR "/parse/visibility1.vhd");

   const error_t expect[] = {
      { 17, "multiple conflicting visible declarations of K" },
      { 20, "multiple conflicting visible declarations of T" },
      { 21, "ambiguous use of enumeration literal BAR" },
      { -1, NULL }
   };
   expect_errors(expect);

   for (int i = 0; i < 3; i++) {
      tree_t p = parse();
      fail_if(p == NULL);
      fail_unless(tree_kind(p) == T_PACKAGE);
      lib_put(lib_work(), p);
   }

   fail_unless(parse() == NULL);

   check_expected_errors();
}
END_TEST

START_TEST(test_visibility2)
{
   input_from_file(TESTDIR "/parse/visibility2.vhd");

   const error_t expect[] = {
      { -1, NULL }
   };
   expect_errors(expect);

   for (int i = 0; i < 2; i++) {
      tree_t p = parse();
      fail_if(p == NULL);
      fail_unless(tree_kind(p) == T_PACKAGE);
      lib_put(lib_work(), p);
   }

   tree_t b = parse();
   fail_if(b == NULL);
   fail_unless(tree_kind(b) == T_PACK_BODY);

   fail_unless(parse() == NULL);

   check_expected_errors();
}
END_TEST

START_TEST(test_visibility3)
{
   set_standard(STD_08);

   input_from_file(TESTDIR "/parse/visibility3.vhd");

   for (int i = 0; i < 2; i++) {
      tree_t p = parse();
      fail_if(p == NULL);
      fail_unless(tree_kind(p) == T_PACKAGE);
      lib_put(lib_work(), p);
   }

   tree_t b = parse();
   fail_if(b == NULL);
   fail_unless(tree_kind(b) == T_PACK_BODY);

   fail_unless(parse() == NULL);

   fail_if_errors();
}
END_TEST

START_TEST(test_visibility4)
{
   lib_t foo_lib = lib_tmp("FOO");
   lib_set_work(foo_lib);

   input_from_file(TESTDIR "/parse/visibility4.vhd");

   const error_t expect[] = {
      { 18, "unexpected * while parsing use clause, expecting one of id" },
      { -1, NULL }
   };
   expect_errors(expect);

   tree_t p1 = parse();
   fail_if(p1 == NULL);
   fail_unless(tree_kind(p1) == T_PACKAGE);
   lib_put(lib_work(), p1);

   lib_t bar_lib = lib_tmp("BAR");
   lib_set_work(bar_lib);

   tree_t p2 = parse();
   fail_if(p2 == NULL);
   fail_unless(tree_kind(p2) == T_PACKAGE);
   lib_put(lib_work(), p2);

   lib_t baz_lib = lib_tmp("BAZ");
   lib_set_work(baz_lib);

   tree_t p3 = parse();
   fail_if(p3 == NULL);
   fail_unless(tree_kind(p3) == T_PACKAGE);
   lib_put(lib_work(), p3);

   fail_unless(parse() == NULL);

   check_expected_errors();
}
END_TEST

START_TEST(test_visibility5)
{
   input_from_file(TESTDIR "/parse/visibility5.vhd");

   for (int i = 0; i < 2; i++) {
      tree_t p = parse();
      fail_if(p == NULL);
      fail_unless(tree_kind(p) == T_PACKAGE);
      lib_put(lib_work(), p);
   }

   fail_unless(parse() == NULL);

   fail_if_errors();
}
END_TEST

START_TEST(test_visibility6)
{
   set_standard(STD_08);

   input_from_file(TESTDIR "/parse/visibility6.vhd");

   for (int i = 0; i < 2; i++) {
      tree_t p = parse();
      fail_if(p == NULL);
      fail_unless(tree_kind(p) == T_PACKAGE);
      lib_put(lib_work(), p);
   }

   fail_unless(parse() == NULL);

   fail_if_errors();
}
END_TEST

START_TEST(test_issue479)
{
   set_standard(STD_08);

   input_from_file(TESTDIR "/parse/issue479.vhd");

   tree_t p = parse();
   fail_if(p == NULL);
   fail_unless(tree_kind(p) == T_PACKAGE);
   lib_put(lib_work(), p);

   tree_t b = parse();
   fail_if(b == NULL);
   fail_unless(tree_kind(b) == T_PACK_BODY);
   lib_put(lib_work(), b);

   for (int i = 0; i < 2; i++) {
      tree_t p = parse();
      fail_if(p == NULL);
      fail_unless(tree_kind(p) == T_PACKAGE);
      lib_put(lib_work(), p);
   }

   fail_unless(parse() == NULL);

   fail_if_errors();
}
END_TEST

START_TEST(test_typo)
{
   set_standard(STD_02);

   input_from_file(TESTDIR "/parse/typo.vhd");

   const error_t expect[] = {
      {  6, "no visible declaration for BOLEAN" },
      {  0, "did you mean BOOLEAN?" },
      { 23, "no visible declaration for RSET" },
      {  0, "did you mean RESET?" },
      { 31, "no visible declaration for NOEW" },
      {  0, NULL },
      { 32, "no visible subprogram declaration for MY_FUNC" },
      {  0, "did you mean MYFUNC?" },
      { 38, "record type REC has no field named FRODO" },
      {  0, "did you mean FOO?" },
      { 44, "protected type PT has no method named ONN" },
      {  0, "did you mean ONE?" },
      { -1, NULL }
   };
   expect_errors(expect);

   tree_t e = parse();
   fail_if(e == NULL);
   fail_unless(tree_kind(e) == T_ENTITY);
   lib_put(lib_work(), e);

   tree_t a = parse();
   fail_if(a == NULL);
   fail_unless(tree_kind(a) == T_ARCH);

   fail_unless(parse() == NULL);

   check_expected_errors();
}
END_TEST

START_TEST(test_issue532)
{
   set_standard(STD_08);

   input_from_file(TESTDIR "/parse/issue532.vhd");

   tree_t p = parse();
   fail_if(p == NULL);
   fail_unless(tree_kind(p) == T_PACKAGE);
   lib_put(lib_work(), p);

   tree_t b = parse();
   fail_if(b == NULL);
   fail_unless(tree_kind(b) == T_PACK_BODY);

   fail_unless(parse() == NULL);

   fail_if_errors();
}
END_TEST

START_TEST(test_issue539)
{
   set_standard(STD_08);

   input_from_file(TESTDIR "/parse/issue539.vhd");

   const error_t expect[] = {
      { 10, "declaration of variable FOO cannot have unconstrained type SLV" },
      { -1, NULL }
   };
   expect_errors(expect);

   tree_t p = parse();
   fail_if(p == NULL);
   fail_unless(tree_kind(p) == T_PACKAGE);
   lib_put(lib_work(), p);

   tree_t b = parse();
   fail_if(b == NULL);
   fail_unless(tree_kind(b) == T_PACK_BODY);

   fail_unless(parse() == NULL);

   check_expected_errors();
}
END_TEST

START_TEST(test_issue568)
{
   set_standard(STD_08);

   input_from_file(TESTDIR "/parse/issue568.vhd");

   tree_t p = parse();
   fail_if(p == NULL);
   fail_unless(tree_kind(p) == T_PACKAGE);
   lib_put(lib_work(), p);

   tree_t e = parse();
   fail_if(e == NULL);
   fail_unless(tree_kind(e) == T_ENTITY);
   lib_put(lib_work(), e);

   tree_t a = parse();
   fail_if(a == NULL);
   fail_unless(tree_kind(a) == T_ARCH);

   fail_unless(parse() == NULL);

   fail_if_errors();
}
END_TEST

START_TEST(test_issue569)
{
   input_from_file(TESTDIR "/parse/issue569.vhd");

   lib_t foo = lib_tmp("foo");
   lib_t bar = lib_tmp("bar");

   lib_set_work(foo);

   tree_t p = parse();
   fail_if(p == NULL);
   fail_unless(tree_kind(p) == T_PACKAGE);
   lib_put(foo, p);

   lib_set_work(bar);

   tree_t e = parse();
   fail_if(e == NULL);
   fail_unless(tree_kind(e) == T_ENTITY);
   lib_put(bar, e);

   tree_t a = parse();
   fail_if(a == NULL);
   fail_unless(tree_kind(a) == T_ARCH);

   fail_unless(parse() == NULL);

   fail_if_errors();
}
END_TEST

START_TEST(test_osvvm7)
{
   set_standard(STD_08);
   input_from_file(TESTDIR "/parse/osvvm7.vhd");

   lib_t work = lib_work();

   tree_t p = parse();
   fail_if(p == NULL);
   fail_unless(tree_kind(p) == T_PACKAGE);
   lib_put(work, p);

   tree_t b = parse();
   fail_if(b == NULL);
   fail_unless(tree_kind(b) == T_PACK_BODY);
   lib_put(work, b);

   tree_t i1 = parse();
   fail_if(i1 == NULL);
   fail_unless(tree_kind(i1) == T_PACK_INST);
   lib_put(work, i1);

   tree_t i2 = parse();
   fail_if(i2 == NULL);
   fail_unless(tree_kind(i2) == T_PACK_INST);
   lib_put(work, i2);

   ident_t name = ident_new("SCOREBOARDIDTYPE");
   tree_t d1 = search_decls(i1, name, 0);
   fail_if(d1 == NULL);
   fail_unless(tree_kind(d1) == T_TYPE_DECL);
   tree_t d2 = search_decls(i2, name, 0);
   fail_if(d2 == NULL);
   fail_unless(tree_kind(d2) == T_TYPE_DECL);

   fail_if(type_eq(tree_type(d1), tree_type(d2)));

   tree_t e = parse();
   fail_if(e == NULL);
   fail_unless(tree_kind(e) == T_ENTITY);
   lib_put(work, e);

   tree_t a = parse();
   fail_if(a == NULL);
   fail_unless(tree_kind(a) == T_ARCH);

   fail_unless(parse() == NULL);

   fail_if_errors();
}
END_TEST

START_TEST(test_issue580)
{
   set_standard(STD_08);

   input_from_file(TESTDIR "/parse/issue580.vhd");

   const error_t expect[] = {
      { 14, "declaration of signal S2 cannot have unconstrained type SUB_T" },
      { -1, NULL }
   };
   expect_errors(expect);

   tree_t e = parse();
   fail_if(e == NULL);
   fail_unless(tree_kind(e) == T_ENTITY);
   lib_put(lib_work(), e);

   tree_t a = parse();
   fail_if(a == NULL);
   fail_unless(tree_kind(a) == T_ARCH);

   fail_unless(parse() == NULL);

   check_expected_errors();
}
END_TEST

START_TEST(test_visibility7)
{
   lib_t foo = lib_tmp("foo");
   lib_set_work(foo);

   input_from_file(TESTDIR "/parse/visibility7.vhd");

   tree_t p = parse();
   fail_if(p == NULL);
   fail_unless(tree_kind(p) == T_PACKAGE);
   lib_put(foo, p);

   tree_t e = parse();
   fail_if(e == NULL);
   fail_unless(tree_kind(e) == T_ENTITY);
   lib_put(foo, e);

   tree_t a = parse();
   fail_if(a == NULL);
   fail_unless(tree_kind(a) == T_ARCH);

   fail_unless(parse() == NULL);

   fail_if_errors();
}
END_TEST

START_TEST(test_issue541)
{
   set_standard(STD_08);

   input_from_file(TESTDIR "/parse/issue541.vhd");

   tree_t e = parse();
   fail_if(e == NULL);
   fail_unless(tree_kind(e) == T_ENTITY);
   lib_put(lib_work(), e);

   tree_t a = parse();
   fail_if(a == NULL);
   fail_unless(tree_kind(a) == T_ARCH);

   fail_unless(parse() == NULL);

   fail_if_errors();
}
END_TEST

START_TEST(test_issue604)
{
   set_standard(STD_19);

   const error_t expect[] = {
      { 11, "\"Tool is NVC\"" },
      { 18, "unexpected `elsif outside conditional analysis block" },
      { 19, "unexpected `end outside conditional analysis block" },
      { -1, NULL }
   };
   expect_errors(expect);

   input_from_file(TESTDIR "/parse/issue604.vhd");

   tree_t p = parse();
   fail_if(p == NULL);
   fail_unless(tree_kind(p) == T_PACKAGE);

   fail_unless(parse() == NULL);

   check_expected_errors();
}
END_TEST

START_TEST(test_vunit9)
{
   input_from_file(TESTDIR "/parse/vunit9.vhd");

   tree_t p = parse();
   fail_if(p == NULL);
   fail_unless(tree_kind(p) == T_PACKAGE);
   lib_put(lib_work(), p);

   tree_t e = parse();
   fail_if(e == NULL);
   fail_unless(tree_kind(e) == T_ENTITY);
   lib_put(lib_work(), e);

   tree_t a = parse();
   fail_if(a == NULL);
   fail_unless(tree_kind(a) == T_ARCH);

   fail_unless(parse() == NULL);

   fail_if_errors();
}
END_TEST

START_TEST(test_alias3)
{
   input_from_file(TESTDIR "/parse/alias3.vhd");

   tree_t p1 = parse();
   fail_if(p1 == NULL);
   fail_unless(tree_kind(p1) == T_PACKAGE);
   lib_put(lib_work(), p1);

   tree_t p2 = parse();
   fail_if(p2 == NULL);
   fail_unless(tree_kind(p2) == T_PACKAGE);
   lib_put(lib_work(), p2);

   tree_t e = parse();
   fail_if(e == NULL);
   fail_unless(tree_kind(e) == T_ENTITY);
   lib_put(lib_work(), e);

   tree_t a = parse();
   fail_if(a == NULL);
   fail_unless(tree_kind(a) == T_ARCH);

   fail_unless(parse() == NULL);

   fail_if_errors();
}
END_TEST

START_TEST(test_subtype2008)
{
   set_standard(STD_08);

   input_from_file(TESTDIR "/parse/subtype2008.vhd");

   tree_t p = parse();
   fail_if(p == NULL);
   fail_unless(tree_kind(p) == T_PACKAGE);

   type_t sub1 = tree_type(search_decls(p, ident_new("SUB1"), 0));
   fail_unless(type_constraints(sub1) == 0);
   fail_unless(type_is_unconstrained(sub1));

   type_t sub2 = tree_type(search_decls(p, ident_new("SUB2"), 0));
   ck_assert_int_eq(type_constraints(sub2), 1);
   fail_unless(tree_subkind(type_constraint(sub2, 0)) == C_OPEN);
   fail_unless(type_is_unconstrained(sub2));

   type_t sub3 = tree_type(search_decls(p, ident_new("SUB3"), 0));
   fail_unless(type_constraints(sub3) == 0);
   fail_unless(type_is_unconstrained(sub3));

   type_t sub4 = tree_type(search_decls(p, ident_new("SUB4"), 0));
   fail_unless(type_constraints(sub4) == 0);
   fail_unless(type_is_unconstrained(sub4));

   type_t sub5 = tree_type(search_decls(p, ident_new("SUB5"), 0));
   fail_unless(type_constraints(sub5) == 1);
   fail_unless(tree_subkind(type_constraint(sub5, 0)) == C_INDEX);
   fail_unless(type_is_unconstrained(sub5));
   ck_assert_ptr_eq(type_elem(sub5), type_elem(sub4));

   type_t sub6 = tree_type(search_decls(p, ident_new("SUB6"), 0));
   ck_assert_int_eq(type_constraints(sub6), 1);
   fail_unless(tree_subkind(type_constraint(sub2, 0)) == C_OPEN);
   fail_if(type_is_unconstrained(sub6));

   type_t sub7 = tree_type(search_decls(p, ident_new("SUB7"), 0));
   fail_unless(type_constraints(sub7) == 0);
   fail_if(type_is_unconstrained(sub7));

   type_t sub10 = tree_type(search_decls(p, ident_new("SUB10"), 0));
   fail_unless(type_constraints(sub10) == 1);
   fail_unless(type_is_unconstrained(sub10));

   type_t sub11 = tree_type(search_decls(p, ident_new("SUB11"), 0));
   fail_unless(type_constraints(sub11) == 1);
   fail_unless(tree_subkind(type_constraint(sub11, 0)) == C_INDEX);
   fail_unless(type_is_unconstrained(sub11));

   fail_unless(parse() == NULL);

   fail_if_errors();
}
END_TEST

START_TEST(test_issue644)
{
   set_standard(STD_08);

   input_from_file(TESTDIR "/parse/issue644.vhd");

   tree_t p = parse();
   fail_if(p == NULL);
   fail_unless(tree_kind(p) == T_PACKAGE);
   lib_put(lib_work(), p);

   tree_t e1 = parse();
   fail_if(e1 == NULL);
   fail_unless(tree_kind(e1) == T_ENTITY);
   lib_put(lib_work(), e1);

   tree_t a1 = parse();
   fail_if(a1 == NULL);
   fail_unless(tree_kind(a1) == T_ARCH);

   tree_t e2 = parse();
   fail_if(e2 == NULL);
   fail_unless(tree_kind(e2) == T_ENTITY);
   lib_put(lib_work(), e2);

   tree_t a2 = parse();
   fail_if(a2 == NULL);
   fail_unless(tree_kind(a2) == T_ARCH);

   fail_unless(parse() == NULL);

   fail_if_errors();
}
END_TEST

START_TEST(test_issue653)
{
   set_standard(STD_08);

   input_from_file(TESTDIR "/parse/issue653.vhd");

   tree_t p1 = parse();
   fail_if(p1 == NULL);
   fail_unless(tree_kind(p1) == T_PACKAGE);
   lib_put(lib_work(), p1);

   tree_t p2 = parse();
   fail_if(p2 == NULL);
   fail_unless(tree_kind(p2) == T_PACK_INST);

   fail_unless(parse() == NULL);

   fail_if_errors();
}
END_TEST

START_TEST(test_issue654)
{
   set_standard(STD_08);

   const error_t expect[] = {
      { 22, "no visible uninstantiated subprogram GENERATE_CLOCK matches "
        "signature [return INTEGER]" },
      { 23, "no visible uninstantiated subprogram declaration for \"+\"" },
      { 30, "multiple visible uninstantiated subprograms with name GENE" },
      { 40, "invalid use of type BIT" },
      { 37, "subprogram WORK.FREQUENCY.GENERATE_CLOCK [BIT, FREQUENCY, "
        "NATURAL] cannot be instantiated until its body has been analysed" },
      { -1, NULL }
   };
   expect_errors(expect);

   input_from_file(TESTDIR "/parse/issue654.vhd");

   tree_t p = parse();
   fail_if(p == NULL);
   fail_unless(tree_kind(p) == T_PACKAGE);
   lib_put(lib_work(), p);

   tree_t b = parse();
   fail_if(b == NULL);
   fail_unless(tree_kind(b) == T_PACK_BODY);

   fail_unless(tree_decls(b) == 2);

   tree_t d0 = tree_decl(b, 0);
   fail_unless(tree_kind(d0) == T_PROC_BODY);

   tree_t d1 = tree_decl(b, 1);
   fail_unless(tree_kind(d1) == T_PROC_INST);
   fail_unless(tree_ref(d1) == d0);

   fail_unless(parse() == NULL);

   check_expected_errors();
}
END_TEST

START_TEST(test_issue687)
{
   const error_t expect[] = {
      { 11, "record type MY_RECORD has no field named INTEGER" },
      { -1, NULL }
   };
   expect_errors(expect);

   input_from_file(TESTDIR "/parse/issue687.vhd");

   tree_t e = parse();
   fail_if(e == NULL);
   fail_unless(tree_kind(e) == T_ENTITY);
   lib_put(lib_work(), e);

   tree_t a = parse();
   fail_if(a == NULL);
   fail_unless(tree_kind(a) == T_ARCH);

   fail_unless(parse() == NULL);

   check_expected_errors();
}
END_TEST

START_TEST(test_issue688)
{
   input_from_file(TESTDIR "/parse/issue688.vhd");

   lib_t test = lib_tmp("test");
   lib_set_work(test);

   tree_t p1 = parse();
   fail_if(p1 == NULL);
   fail_unless(tree_kind(p1) == T_PACKAGE);
   lib_put(lib_work(), p1);

   lib_t work = lib_tmp("work");
   lib_set_work(work);

   tree_t e = parse();
   fail_if(e == NULL);
   fail_unless(tree_kind(e) == T_ENTITY);
   lib_put(lib_work(), e);

   tree_t a = parse();
   fail_if(a == NULL);
   fail_unless(tree_kind(a) == T_ARCH);

   fail_unless(parse() == NULL);

   fail_if_errors();
}
END_TEST

START_TEST(test_issue686)
{
   set_standard(STD_08);

   input_from_file(TESTDIR "/parse/issue686.vhd");

   tree_t p1 = parse();
   fail_if(p1 == NULL);
   fail_unless(tree_kind(p1) == T_PACKAGE);
   lib_put(lib_work(), p1);

   tree_t p2 = parse();
   fail_if(p2 == NULL);
   fail_unless(tree_kind(p2) == T_PACKAGE);
   lib_put(lib_work(), p2);

   tree_t p3 = parse();
   fail_if(p3 == NULL);
   fail_unless(tree_kind(p3) == T_PACK_INST);

   fail_unless(parse() == NULL);

   fail_if_errors();
}
END_TEST

START_TEST(test_interface)
{
   set_standard(STD_19);

   input_from_file(TESTDIR "/parse/interface.vhd");

   tree_t p1 = parse();
   fail_if(p1 == NULL);
   fail_unless(tree_kind(p1) == T_PACKAGE);
   lib_put(lib_work(), p1);

   fail_unless(parse() == NULL);

   fail_if_errors();
}
END_TEST

START_TEST(test_issue701)
{
   set_standard(STD_08);

   input_from_file(TESTDIR "/parse/issue701.vhd");

   tree_t e = parse();
   fail_if(e == NULL);
   fail_unless(tree_kind(e) == T_ENTITY);
   lib_put(lib_work(), e);

   tree_t a = parse();
   fail_if(a == NULL);
   fail_unless(tree_kind(a) == T_ARCH);

   fail_unless(parse() == NULL);

   fail_if_errors();
}
END_TEST

START_TEST(test_issue708)
{
   set_standard(STD_19);

   input_from_file(TESTDIR "/parse/issue708.vhd");

   tree_t p = parse();
   fail_if(p == NULL);
   fail_unless(tree_kind(p) == T_PACKAGE);

   fail_unless(parse() == NULL);

   fail_if_errors();
}
END_TEST

START_TEST(test_issue727)
{
   set_standard(STD_08);

   input_from_file(TESTDIR "/parse/issue727.vhd");

   lib_t work = lib_work();

   tree_t p1 = parse();
   fail_if(p1 == NULL);
   fail_unless(tree_kind(p1) == T_PACKAGE);
   lib_put(work, p1);

   tree_t p2 = parse();
   fail_if(p2 == NULL);
   fail_unless(tree_kind(p2) == T_PACKAGE);
   lib_put(work, p2);

   tree_t b1 = parse();
   fail_if(b1 == NULL);
   fail_unless(tree_kind(b1) == T_PACK_BODY);
   lib_put(work, b1);

   tree_t p3 = parse();
   fail_if(p3 == NULL);
   fail_unless(tree_kind(p3) == T_PACKAGE);
   lib_put(work, p3);

   tree_t p4 = parse();
   fail_if(p4 == NULL);
   fail_unless(tree_kind(p4) == T_PACKAGE);
   lib_put(work, p4);

   tree_t b2 = parse();
   fail_if(b2 == NULL);
   fail_unless(tree_kind(b2) == T_PACK_BODY);
   lib_put(work, b2);

   fail_unless(parse() == NULL);

   fail_if_errors();
}
END_TEST

START_TEST(test_visibility8)
{
   input_from_file(TESTDIR "/parse/visibility8.vhd");

   const error_t expect[] = {
      { 16, "declaration of S1 is hidden" },
      { 21, "declaration of C1 is hidden" },
      { 22, "declaration of T1 is hidden" },
      { 23, "declaration of T2 is hidden" },
      { 24, "declaration of V1 is hidden" },
      { 25, "declaration of A1 is hidden" },
      { 26, "declaration of F1 is hidden" },
      { -1, NULL }
   };
   expect_errors(expect);

   tree_t e = parse();
   fail_if(e == NULL);
   fail_unless(tree_kind(e) == T_ENTITY);
   lib_put(lib_work(), e);

   tree_t a = parse();
   fail_if(a == NULL);
   fail_unless(tree_kind(a) == T_ARCH);

   fail_unless(parse() == NULL);

   check_expected_errors();
}
END_TEST

START_TEST(test_issue751)
{
   input_from_file(TESTDIR "/parse/issue751.vhd");

   tree_t e = parse();
   fail_if(e == NULL);
   fail_unless(tree_kind(e) == T_ENTITY);
   lib_put(lib_work(), e);

   tree_t a = parse();
   fail_if(a == NULL);
   fail_unless(tree_kind(a) == T_ARCH);

   tree_t p0 = tree_stmt(a, 0);
   fail_unless(tree_ident(p0) == ident_new("_P0"));
   fail_unless(tree_flags(p0) & TREE_F_SYNTHETIC_NAME);

   tree_t outer = tree_decl(p0, 0);
   fail_unless(tree_kind(outer) == T_FUNC_BODY);
   fail_unless(tree_ident2(outer) == ident_new(
                  "WORK.ISSUE751-TEST._P0.OUTER()I"));

   tree_t inner = tree_decl(outer, 0);
   fail_unless(tree_kind(inner) == T_FUNC_BODY);
   fail_unless(tree_ident2(inner) == ident_new(
                  "WORK.ISSUE751-TEST._P0.OUTER()I.INNER()I"));

   tree_t abc = tree_decl(p0, 1);
   fail_unless(tree_kind(abc) == T_TYPE_DECL);
   fail_unless(type_ident(tree_type(abc)) == ident_new(
                  "WORK.ISSUE751-TEST._P0.ABC"));

   fail_unless(parse() == NULL);

   fail_if_errors();
}
END_TEST

START_TEST(test_visibility9)
{
   set_standard(STD_08);

   input_from_file(TESTDIR "/parse/visibility9.vhd");

   const error_t expect[] = {
      { 21, "no matching operator \"=\" [T_XYZ, T_XYZ return BOOLEAN]" },
      { -1, NULL }
   };
   expect_errors(expect);

   tree_t p = parse();
   fail_if(p == NULL);
   fail_unless(tree_kind(p) == T_PACKAGE);
   simplify_local(p, NULL, NULL);   // XXX: remove me
   lib_put(lib_work(), p);

   tree_t e = parse();
   fail_if(e == NULL);
   fail_unless(tree_kind(e) == T_ENTITY);
   lib_put(lib_work(), e);

   tree_t a = parse();
   fail_if(a == NULL);
   fail_unless(tree_kind(a) == T_ARCH);

   tree_t c2 = tree_decl(a, 1);
   fail_unless(tree_kind(c2) == T_CONST_DECL);

   tree_t eq = tree_ref(tree_value(c2));
   fail_if(tree_flags(eq) & TREE_F_PREDEFINED);

   fail_unless(parse() == NULL);

   check_expected_errors();
}
END_TEST

START_TEST(test_issue753)
{
   input_from_file(TESTDIR "/parse/issue753.vhd");

   const error_t expect[] = {
      {  3, "no matching coverage on directive seen before end of design " },
      { 17, "no matching synthesis translate_on directive seen before end" },
      { 14, "no matching pragma translate_on directive seen before end" },
      { -1, NULL }
   };
   expect_errors(expect);

   tree_t e = parse();
   fail_if(e == NULL);
   fail_unless(tree_kind(e) == T_ENTITY);
   lib_put(lib_work(), e);

   tree_t a = parse();
   fail_if(a == NULL);
   fail_unless(tree_kind(a) == T_ARCH);

   fail_unless(parse() == NULL);

   check_expected_errors();
}
END_TEST

START_TEST(test_issue760)
{
   set_standard(STD_19);

   input_from_file(TESTDIR "/parse/issue760.vhd");

   tree_t p = parse();
   fail_if(p == NULL);
   fail_unless(tree_kind(p) == T_PACKAGE);

   lib_put(lib_work(), p);

   tree_t b = parse();
   fail_if(b == NULL);
   fail_unless(tree_kind(b) == T_PACK_BODY);

   fail_unless(parse() == NULL);

   fail_if_errors();
}
END_TEST

START_TEST(test_issue761)
{
   input_from_file(TESTDIR "/parse/issue761.vhd");

   tree_t e = parse();
   fail_if(e == NULL);
   fail_unless(tree_kind(e) == T_ENTITY);
   lib_put(lib_work(), e);

   tree_t a = parse();
   fail_if(a == NULL);
   fail_unless(tree_kind(a) == T_ARCH);

   fail_unless(parse() == NULL);

   fail_if_errors();
}
END_TEST

START_TEST(test_issue776)
{
   set_standard(STD_08);

   input_from_file(TESTDIR "/parse/issue776.vhd");

   tree_t p = parse();
   fail_if(p == NULL);
   fail_unless(tree_kind(p) == T_PACKAGE);
   lib_put(lib_work(), p);

   tree_t e = parse();
   fail_if(e == NULL);
   fail_unless(tree_kind(e) == T_ENTITY);
   lib_put(lib_work(), e);

   tree_t a = parse();
   fail_if(a == NULL);
   fail_unless(tree_kind(a) == T_ARCH);

   fail_unless(parse() == NULL);

   fail_if_errors();
}
END_TEST

START_TEST(test_visibility10)
{
   input_from_file(TESTDIR "/parse/visibility10.vhd");

   tree_t p1 = parse();
   fail_if(p1 == NULL);
   fail_unless(tree_kind(p1) == T_PACKAGE);
   lib_put(lib_work(), p1);

   tree_t p2 = parse();
   fail_if(p2 == NULL);
   fail_unless(tree_kind(p2) == T_PACKAGE);
   lib_put(lib_work(), p2);

   tree_t p3 = parse();
   fail_if(p3 == NULL);
   fail_unless(tree_kind(p3) == T_PACKAGE);

   fail_unless(parse() == NULL);

   fail_if_errors();
}
END_TEST

START_TEST(test_error10)
{
   input_from_file(TESTDIR "/parse/error10.vhd");

   const error_t expect[] = {
      {  6, "unexpected in while parsing paremeter specification" },
      { -1, NULL }
   };
   expect_errors(expect);

   tree_t e = parse();
   fail_if(e == NULL);
   fail_unless(tree_kind(e) == T_ENTITY);
   lib_put(lib_work(), e);

   tree_t a = parse();
   fail_if(a == NULL);
   fail_unless(tree_kind(a) == T_ARCH);

   fail_unless(parse() == NULL);

   check_expected_errors();
}
END_TEST

START_TEST(test_issue783)
{
   input_from_file(TESTDIR "/parse/issue783.vhd");

   lib_t foo = lib_tmp("foo");
   lib_set_work(foo);

   tree_t e = parse();
   fail_if(e == NULL);
   fail_unless(tree_kind(e) == T_ENTITY);
   lib_put(lib_work(), e);

   tree_t a = parse();
   fail_if(a == NULL);
   fail_unless(tree_kind(a) == T_ARCH);
   lib_put(lib_work(), a);

   tree_t c = parse();
   fail_if(c == NULL);
   fail_unless(tree_kind(c) == T_CONFIGURATION);

   fail_unless(parse() == NULL);

   fail_if_errors();
}
END_TEST

START_TEST(test_issue792)
{
   set_standard(STD_02);

   input_from_file(TESTDIR "/parse/issue792.vhd");

   tree_t p1 = parse();
   fail_if(p1 == NULL);
   fail_unless(tree_kind(p1) == T_PACKAGE);
   lib_put(lib_work(), p1);

   tree_t p2 = parse();
   fail_if(p2 == NULL);
   fail_unless(tree_kind(p2) == T_PACKAGE);
   lib_put(lib_work(), p2);

   tree_t p3 = parse();
   fail_if(p3 == NULL);
   fail_unless(tree_kind(p3) == T_PACKAGE);

   fail_unless(parse() == NULL);

   fail_if_errors();
}
END_TEST

START_TEST(test_issue789)
{
   input_from_file(TESTDIR "/parse/issue789.vhd");

   const error_t expect[] = {
      { 15, "invalid use of architecture RTL" },
      {  0, "declaration of literal RTL is hidden" },
      {  0, "name RTL refers to this architecture" },
      { 31, "design unit WORK.TEST1 is not a component declaration" },
      { 32, "invalid use of architecture RTL" },
      {  0, "declaration of literal RTL is hidden" },
      {  0, "name RTL refers to this architecture" },
      { -1, NULL }
   };
   expect_errors(expect);

   tree_t p = parse();
   fail_if(p == NULL);
   fail_unless(tree_kind(p) == T_PACKAGE);
   lib_put(lib_work(), p);

   tree_t e1 = parse();
   fail_if(e1 == NULL);
   fail_unless(tree_kind(e1) == T_ENTITY);
   lib_put(lib_work(), e1);

   tree_t a1 = parse();
   fail_if(a1 == NULL);
   fail_unless(tree_kind(a1) == T_ARCH);

   tree_t e2 = parse();
   fail_if(e2 == NULL);
   fail_unless(tree_kind(e2) == T_ENTITY);
   lib_put(lib_work(), e2);

   tree_t a2 = parse();
   fail_if(a2 == NULL);
   fail_unless(tree_kind(a2) == T_ARCH);

   fail_unless(parse() == NULL);

   check_expected_errors();
}
END_TEST

START_TEST(test_issue793)
{
   input_from_file(TESTDIR "/parse/issue793.vhd");

   const error_t expect[] = {
      { 41, "no possible overload of CHECK_SETUP has formal POL1" },
      {  0, "did you mean POL?" },
      { -1, NULL }
   };
   expect_errors(expect);

   tree_t e = parse();
   fail_if(e == NULL);
   fail_unless(tree_kind(e) == T_ENTITY);
   lib_put(lib_work(), e);

   tree_t a = parse();
   fail_if(a == NULL);
   fail_unless(tree_kind(a) == T_ARCH);

   fail_unless(parse() == NULL);

   check_expected_errors();
}
END_TEST

START_TEST(test_slow1)
{
   input_from_file(TESTDIR "/parse/slow1.vhd");

   tree_t e = parse();
   fail_if(e == NULL);
   fail_unless(tree_kind(e) == T_ENTITY);
   lib_put(lib_work(), e);

   tree_t a = parse();
   fail_if(a == NULL);
   fail_unless(tree_kind(a) == T_ARCH);

   fail_unless(parse() == NULL);

   fail_if_errors();
}
END_TEST

START_TEST(test_issue802)
{
   set_standard(STD_08);

   input_from_file(TESTDIR "/parse/issue802.vhd");

   tree_t p1 = parse();
   fail_if(p1 == NULL);
   fail_unless(tree_kind(p1) == T_PACKAGE);
   lib_put(lib_work(), p1);

   tree_t p2 = parse();
   fail_if(p2 == NULL);
   fail_unless(tree_kind(p2) == T_PACKAGE);
   lib_put(lib_work(), p2);

   tree_t e = parse();
   fail_if(e == NULL);
   fail_unless(tree_kind(e) == T_ENTITY);
   lib_put(lib_work(), e);

   tree_t a = parse();
   fail_if(a == NULL);
   fail_unless(tree_kind(a) == T_ARCH);

   fail_unless(parse() == NULL);

   fail_if_errors();
}
END_TEST

START_TEST(test_issue805)
{
   input_from_file(TESTDIR "/parse/issue805.vhd");

   tree_t e = parse();
   fail_if(e == NULL);
   fail_unless(tree_kind(e) == T_ENTITY);
   lib_put(lib_work(), e);

   tree_t a = parse();
   fail_if(a == NULL);
   fail_unless(tree_kind(a) == T_ARCH);

   fail_unless(parse() == NULL);

   fail_if_errors();
}
END_TEST

START_TEST(test_visibility11)
{
   set_standard(STD_08);

   input_from_file(TESTDIR "/parse/visibility11.vhd");

   const error_t expect[] = {
      { 16, "cannot reference G in uninstantiated package WORK.PACK outside "
        "of the package itself" },
      { 24, "expanded name cannot reference S in label B outside of the "
        "construct itself" },
      { -1, NULL }
   };
   expect_errors(expect);

   tree_t p = parse();
   fail_if(p == NULL);
   fail_unless(tree_kind(p) == T_PACKAGE);
   lib_put(lib_work(), p);

   tree_t e = parse();
   fail_if(e == NULL);
   fail_unless(tree_kind(e) == T_ENTITY);
   lib_put(lib_work(), e);

   tree_t a = parse();
   fail_if(a == NULL);
   fail_unless(tree_kind(a) == T_ARCH);

   fail_unless(parse() == NULL);

   check_expected_errors();
}
END_TEST

START_TEST(test_issue837)
{
   set_standard(STD_08);

   input_from_file(TESTDIR "/parse/issue837.vhd");

   tree_t e = parse();
   fail_if(e == NULL);
   fail_unless(tree_kind(e) == T_ENTITY);
   lib_put(lib_work(), e);

   tree_t a = parse();
   fail_if(a == NULL);
   fail_unless(tree_kind(a) == T_ARCH);

   tree_t s = tree_decl(a, 0);
   fail_unless(tree_kind(s) == T_SIGNAL_DECL);

   type_t t = tree_type(s);
   fail_unless(type_kind(t) == T_SUBTYPE);

   tree_t r = range_of(t, 0);
   fail_unless(tree_subkind(r) == RANGE_EXPR);
   fail_unless(tree_kind(tree_value(r)) == T_ATTR_REF);

   fail_if_errors();
}
END_TEST

START_TEST(test_issue845)
{
   input_from_file(TESTDIR "/parse/issue845.vhd");

   tree_t e = parse();
   fail_if(e == NULL);
   fail_unless(tree_kind(e) == T_ENTITY);
   lib_put(lib_work(), e);

   tree_t a = parse();
   fail_if(a == NULL);
   fail_unless(tree_kind(a) == T_ARCH);

   fail_if_errors();
}
END_TEST

START_TEST(test_issue848)
{
   input_from_file(TESTDIR "/parse/issue848.vhd");

   const error_t expect[] = {
      { 37, "pure function TEST cannot call procedure PROC which references "
        "a shared variable" },
      { 44, "pure function TEST2 cannot call procedure PROC2 which references "
        "a shared variable" },
      { 51, "pure function TEST3 cannot call procedure PROC3 which references "
        "a shared variable" },
      { -1, NULL }
   };
   expect_errors(expect);

   tree_t p = parse();
   fail_if(p == NULL);
   fail_unless(tree_kind(p) == T_PACKAGE);
   lib_put(lib_work(), p);

   tree_t b = parse();
   fail_if(b == NULL);
   fail_unless(tree_kind(b) == T_PACK_BODY);
   lib_put(lib_work(), b);

   fail_unless(parse() == NULL);

   check_expected_errors();
}
END_TEST

START_TEST(test_issue870)
{
   opt_set_int(OPT_RELAXED, 1);

   input_from_file(TESTDIR "/parse/issue870.vhd");

   tree_t e = parse();
   fail_if(e == NULL);
   fail_unless(tree_kind(e) == T_ENTITY);
   lib_put(lib_work(), e);

   tree_t a = parse();
   fail_if(a == NULL);
   fail_unless(tree_kind(a) == T_ARCH);

   fail_unless(parse() == NULL);

   fail_if_errors();
}
END_TEST

START_TEST(test_aggregate)
{
   set_standard(STD_08);

   input_from_file(TESTDIR "/parse/aggregate.vhd");

   tree_t p = parse();
   fail_if(p == NULL);
   fail_unless(tree_kind(p) == T_PACKAGE);

   static const struct {
      const char   *name;
      int64_t       left;
      range_kind_t  dir;
      int64_t       right;
   } cases[] = {
      { "C1", 1, RANGE_TO, 2 },
      { "C2", 1, RANGE_TO, 2 },
      { "C3", 0, RANGE_TO, 1 },
      { "C4", 100, RANGE_DOWNTO, 98 },
      { "C5", 2, RANGE_DOWNTO, 1 },
      { "C6", 0, RANGE_TO, 3 },
      { "C7", 0, RANGE_TO, 2 },
   };

   for (int i = 0; i < ARRAY_LEN(cases); i++) {
      tree_t d = search_decls(p, ident_new(cases[i].name), 0);
      fail_if(d == NULL);

      type_t type = tree_type(tree_value(d));
      fail_unless(type_is_array(type));

      tree_t r = range_of(type, 0);
      const range_kind_t kind = tree_subkind(r);
      const int64_t left = assume_int(tree_left(r));
      const int64_t right = assume_int(tree_right(r));

      if (kind != cases[i].dir || left != cases[i].left
          || right != cases[i].right) {
         fmt_loc(stdout, tree_loc(d));
         ck_abort_msg("range mismatch: %d %"PRIi64" %"PRIi64,
                      kind, left, right);
      }
   }

   fail_unless(parse() == NULL);

   fail_if_errors();
}
END_TEST

START_TEST(test_issue875)
{
   input_from_file(TESTDIR "/parse/issue875.vhd");

   const error_t expect[] = {
      {  6, "homograph of HAS_RD [INSTRUCTION32_T return BOOLEAN] already "
         "declared in this region" },
      {  0, "duplicate declaration" },
      {  0, "only the base type is considered when determining" },
      {  0, "previous declaration was here" },
      {  9, "design unit depends on WORK.REPRO which was analysed "
         "with errors" },
      { -1, NULL }
   };
   expect_errors(expect);

   tree_t p = parse();
   fail_if(p == NULL);
   fail_unless(tree_kind(p) == T_PACKAGE);
   lib_put_error(lib_work(), p);

   tree_t b = parse();
   fail_if(b == NULL);
   fail_unless(tree_kind(b) == T_PACK_BODY);
   lib_put(lib_work(), b);

   fail_unless(parse() == NULL);

   check_expected_errors();
}
END_TEST

START_TEST(test_error11)
{
   set_standard(STD_08);

   input_from_file(TESTDIR "/parse/error11.vhd");

   const error_t expect[] = {
      { 26, "no visible subprogram STD_MATCH matches signature [T_ELEMENT, "
        "T_ELEMENT return BOOLEAN]" },
      { -1, NULL }
   };
   expect_errors(expect);

   tree_t p = parse();
   fail_if(p == NULL);
   fail_unless(tree_kind(p) == T_PACKAGE);
   lib_put(lib_work(), p);

   tree_t b = parse();
   fail_if(b == NULL);
   fail_unless(tree_kind(b) == T_PACK_BODY);
   lib_put(lib_work(), b);

   tree_t e = parse();
   fail_if(e == NULL);
   fail_unless(tree_kind(e) == T_ENTITY);
   lib_put(lib_work(), e);

   tree_t a = parse();
   fail_if(a == NULL);
   fail_unless(tree_kind(a) == T_ARCH);

   fail_unless(parse() == NULL);

   check_expected_errors();
}
END_TEST

START_TEST(test_error12)
{
   set_standard(STD_08);

   input_from_file(TESTDIR "/parse/error12.vhd");

   const error_t expect[] = {
      { 34, "SB_QUEUE_PKG has no generic named SCOPE" },
      { -1, NULL }
   };
   expect_errors(expect);

   tree_t p1 = parse();
   fail_if(p1 == NULL);
   fail_unless(tree_kind(p1) == T_PACKAGE);
   lib_put(lib_work(), p1);

   tree_t p2 = parse();
   fail_if(p2 == NULL);
   fail_unless(tree_kind(p2) == T_PACKAGE);
   lib_put(lib_work(), p2);

   tree_t b = parse();
   fail_if(b == NULL);
   fail_unless(tree_kind(b) == T_PACK_BODY);
   lib_put(lib_work(), b);

   fail_unless(parse() == NULL);

   check_expected_errors();
}
END_TEST

START_TEST(test_issue892)
{
   input_from_file(TESTDIR "/parse/issue892.vhd");

   const error_t expect[] = {
      { 25, "prefix of attribute name with signature does not denote a "
        "subprogram or enumeration literal" },
      { -1, NULL }
   };
   expect_errors(expect);

   tree_t e = parse();
   fail_if(e == NULL);
   fail_unless(tree_kind(e) == T_ENTITY);
   lib_put(lib_work(), e);

   tree_t a = parse();
   fail_if(a == NULL);
   fail_unless(tree_kind(a) == T_ARCH);

   fail_unless(parse() == NULL);

   check_expected_errors();
}
END_TEST

START_TEST(test_issue889)
{
   input_from_file(TESTDIR "/parse/issue889.vhd");

   tree_t e = parse();
   fail_if(e == NULL);
   fail_unless(tree_kind(e) == T_ENTITY);
   lib_put(lib_work(), e);

   tree_t a = parse();
   fail_if(a == NULL);
   fail_unless(tree_kind(a) == T_ARCH);

   fail_unless(parse() == NULL);

   fail_if_errors();
}
END_TEST

START_TEST(test_vests2)
{
   input_from_file(TESTDIR "/parse/vests2.vhd");

   const error_t expect[] = {
      { 39, "design unit FAIL-C12S03B02X02P05N01I03064ARCH_A not found in " },
      { 41, "no visible declaration for TEST" },
      { 41, "cannot determine type of OPEN expression" },
      { -1, NULL }
   };
   expect_errors(expect);

   tree_t e = parse();
   fail_if(e == NULL);
   fail_unless(tree_kind(e) == T_ENTITY);
   lib_put(lib_work(), e);

   tree_t a = parse();
   fail_if(a == NULL);
   fail_unless(tree_kind(a) == T_ARCH);

   fail_unless(parse() == NULL);

   check_expected_errors();
}
END_TEST

START_TEST(test_vests3)
{
   input_from_file(TESTDIR "/parse/vests3.vhd");

   const error_t expect[] = {
      { 38, "unterminated string literal" },
      {  0, "a string literal must fit on one line" },
      { 39, "unexpected begin while parsing variable declaration" },
      { 38, "invalid character '%' in string literal of type BIT_VECTOR" },
      { -1, NULL }
   };
   expect_errors(expect);

   tree_t e = parse();
   fail_if(e == NULL);
   fail_unless(tree_kind(e) == T_ENTITY);
   lib_put(lib_work(), e);

   tree_t a = parse();
   fail_if(a == NULL);
   fail_unless(tree_kind(a) == T_ARCH);

   fail_unless(parse() == NULL);

   check_expected_errors();
}
END_TEST

START_TEST(test_vests4)
{
   input_from_file(TESTDIR "/parse/vests4.vhd");

   const error_t expect[] = {
      { 44, "invalid use of label SIG" },
      { -1, NULL }
   };
   expect_errors(expect);

   tree_t e = parse();
   fail_if(e == NULL);
   fail_unless(tree_kind(e) == T_ENTITY);
   lib_put(lib_work(), e);

   tree_t a = parse();
   fail_if(a == NULL);
   fail_unless(tree_kind(a) == T_ARCH);

   fail_unless(parse() == NULL);

   check_expected_errors();
}
END_TEST

START_TEST(test_vests5)
{
   input_from_file(TESTDIR "/parse/vests5.vhd");

   const error_t expect[] = {
      { 47, "ambiguous use of name MC" },
      { -1, NULL }
   };
   expect_errors(expect);

   for (int i = 0; i < 2; i++) {
      tree_t p2 = parse();
      fail_if(p2 == NULL);
      fail_unless(tree_kind(p2) == T_PACKAGE);
      lib_put(lib_work(), p2);
   }

   tree_t e = parse();
   fail_if(e == NULL);
   fail_unless(tree_kind(e) == T_ENTITY);
   lib_put(lib_work(), e);

   tree_t a = parse();
   fail_if(a == NULL);
   fail_unless(tree_kind(a) == T_ARCH);

   fail_unless(parse() == NULL);

   check_expected_errors();
}
END_TEST

START_TEST(test_basic_identifier)
{
   input_from_file(TESTDIR "/parse/basic_identifier.vhd");

   const error_t expect[] = {
      { 1, "unexpected error while parsing entity declaration, expecting is" },
      { 2, "unexpected error while parsing entity declaration, expecting is" },
      { 3, "unexpected error while parsing entity declaration, expecting identifier" },
      { 8, "unexpected error while parsing architecture body, expecting of" },
      { 12, "unexpected error while parsing architecture body, expecting of" },
      { -1, NULL }
   };
   expect_errors(expect);

   for (int i = 0; i < 5; i++) {
      tree_t e = parse();
      fail_if(e == NULL);
      fail_unless(tree_kind(e) == T_ENTITY);
      lib_put(lib_work(), e);
   }

   for (int i = 0; i < 3; i++) {
      tree_t a = parse();
      fail_if(a == NULL);
      fail_unless(tree_kind(a) == T_ARCH);
   }

   fail_unless(parse() == NULL);

   check_expected_errors();
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
   tcase_add_test(tc_core, test_vhdl2019);
   tcase_add_test(tc_core, test_issue416);
   tcase_add_test(tc_core, test_explicit_93);
   tcase_add_test(tc_core, test_explicit_08);
   tcase_add_test(tc_core, test_homograph);
   tcase_add_test(tc_core, test_group);
   tcase_add_test(tc_core, test_alias2);
   tcase_add_test(tc_core, test_badprimary);
   tcase_add_test(tc_core, test_error3);
   tcase_add_test(tc_core, test_protected2);
   tcase_add_test(tc_core, test_error4);
   tcase_add_test(tc_core, test_tc1012);
   tcase_add_test(tc_core, test_issue443);
   tcase_add_test(tc_core, test_vunit5);
   tcase_add_test(tc_core, test_vunit6);
   tcase_add_test(tc_core, test_external);
   tcase_add_test(tc_core, test_error5);
   tcase_add_test(tc_core, test_vunit7);
   tcase_add_test(tc_core, test_error6);
   tcase_add_test(tc_core, test_names2);
   tcase_add_test(tc_core, test_issue457);
   tcase_add_test(tc_core, test_issue458);
   tcase_add_test(tc_core, test_issue461a);
   tcase_add_test(tc_core, test_issue461b);
   tcase_add_test(tc_core, test_error7);
   tcase_add_test(tc_core, test_error8);
   tcase_add_test(tc_core, test_vunit8);
   tcase_add_test(tc_core, test_issue464);
   tcase_add_test(tc_core, test_osvvm6);
   tcase_add_test(tc_core, test_issue468);
   tcase_add_test(tc_core, test_names3);
   tcase_add_test(tc_core, test_range1);
   tcase_add_test(tc_core, test_error9);
   tcase_add_test(tc_core, test_uvvm1);
   tcase_add_test(tc_core, test_uvvm2);
   tcase_add_test(tc_core, test_visibility1);
   tcase_add_test(tc_core, test_visibility2);
   tcase_add_test(tc_core, test_visibility3);
   tcase_add_test(tc_core, test_visibility4);
   tcase_add_test(tc_core, test_visibility5);
   tcase_add_test(tc_core, test_visibility6);
   tcase_add_test(tc_core, test_issue479);
   tcase_add_test(tc_core, test_typo);
   tcase_add_test(tc_core, test_issue532);
   tcase_add_test(tc_core, test_issue539);
   tcase_add_test(tc_core, test_issue568);
   tcase_add_test(tc_core, test_issue569);
   tcase_add_test(tc_core, test_osvvm7);
   tcase_add_test(tc_core, test_issue580);
   tcase_add_test(tc_core, test_visibility7);
   tcase_add_test(tc_core, test_issue541);
   tcase_add_test(tc_core, test_issue604);
   tcase_add_test(tc_core, test_vunit9);
   tcase_add_test(tc_core, test_alias3);
   tcase_add_test(tc_core, test_subtype2008);
   tcase_add_test(tc_core, test_issue644);
   tcase_add_test(tc_core, test_issue653);
   tcase_add_test(tc_core, test_issue654);
   tcase_add_test(tc_core, test_issue687);
   tcase_add_test(tc_core, test_issue688);
   tcase_add_test(tc_core, test_issue686);
   tcase_add_test(tc_core, test_interface);
   tcase_add_test(tc_core, test_issue701);
   tcase_add_test(tc_core, test_issue708);
   tcase_add_test(tc_core, test_issue727);
   tcase_add_test(tc_core, test_visibility8);
   tcase_add_test(tc_core, test_issue751);
   tcase_add_test(tc_core, test_visibility9);
   tcase_add_test(tc_core, test_issue753);
   tcase_add_test(tc_core, test_issue760);
   tcase_add_test(tc_core, test_issue761);
   tcase_add_test(tc_core, test_issue776);
   tcase_add_test(tc_core, test_visibility10);
   tcase_add_test(tc_core, test_error10);
   tcase_add_test(tc_core, test_issue783);
   tcase_add_test(tc_core, test_issue792);
   tcase_add_test(tc_core, test_issue789);
   tcase_add_test(tc_core, test_issue793);
   tcase_add_test(tc_core, test_slow1);
   tcase_add_test(tc_core, test_issue802);
   tcase_add_test(tc_core, test_issue805);
   tcase_add_test(tc_core, test_visibility11);
   tcase_add_test(tc_core, test_issue837);
   tcase_add_test(tc_core, test_issue845);
   tcase_add_test(tc_core, test_issue848);
   tcase_add_test(tc_core, test_issue870);
   tcase_add_test(tc_core, test_aggregate);
   tcase_add_test(tc_core, test_issue875);
   tcase_add_test(tc_core, test_error11);
   tcase_add_test(tc_core, test_error12);
   tcase_add_test(tc_core, test_issue892);
   tcase_add_test(tc_core, test_issue889);
   tcase_add_test(tc_core, test_vests2);
   tcase_add_test(tc_core, test_vests3);
   tcase_add_test(tc_core, test_vests4);
   tcase_add_test(tc_core, test_vests5);
   tcase_add_test(tc_core, test_basic_identifier);
   suite_add_tcase(s, tc_core);

   return s;
}
