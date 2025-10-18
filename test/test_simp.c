//
//  Copyright (C) 2011-2025  Nick Gasson
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
#include "jit/jit.h"
#include "lib.h"
#include "option.h"
#include "phase.h"
#include "scan.h"
#include "type.h"

#include <check.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

static bool folded_i(tree_t t, int64_t i)
{
   if (tree_kind(t) != T_LITERAL)
      return false;

   if (tree_subkind(t) != L_INT)
      return false;

   return tree_ival(t) == i;
}

static bool folded_r(tree_t t, double r)
{
   if (tree_kind(t) != T_LITERAL)
      return false;

   if (tree_subkind(t) != L_REAL)
      return false;

   const double dval = tree_dval(t);
   return (dval > r * 0.9999) && (dval < r * 1.0001);
}

static bool folded_b(tree_t t, bool b)
{
   if (type_ident(tree_type(t)) != ident_new("STD.STANDARD.BOOLEAN"))
      return false;

   bool actual;
   if (folded_bool(t, &actual))
      return actual == b;
   else
      return false;
}

START_TEST(test_cfold)
{
   tree_t e, a, p, s, r;

   input_from_file(TESTDIR "/simp/cfold.vhd");

   e = parse();
   fail_if(e == NULL);
   fail_unless(tree_kind(e) == T_ENTITY);
   lib_put(lib_work(), e);

   a = parse();
   fail_if(a == NULL);
   fail_unless(tree_kind(a) == T_ARCH);

   fail_if_errors();

   unit_registry_t *ur = get_registry();
   mir_context_t *mc = get_mir();
   jit_t *jit = jit_new(ur, mc, NULL);
   simplify_local(a, jit, ur, mc);
   jit_free(jit);

   fail_unless(folded_i(tree_value(tree_decl(a, 0)), -10));

   r = type_dim(tree_type(tree_decl(a, 1)), 0);
   fail_unless(folded_i(tree_left(r), -5));
   fail_unless(folded_i(tree_right(r), 8));

   p = tree_stmt(a, 0);

   s = tree_stmt(p, 0);
   fail_unless(folded_i(tree_value(tree_waveform(s, 0)), 2));
   s = tree_stmt(p, 1);
   fail_unless(folded_i(tree_value(tree_waveform(s, 0)), 8));
   s = tree_stmt(p, 2);
   fail_unless(folded_i(tree_value(tree_waveform(s, 0)), -5));
   s = tree_stmt(p, 3);
   fail_unless(folded_b(tree_value(s), true));
   s = tree_stmt(p, 4);
   fail_unless(folded_b(tree_value(s), false));
   s = tree_stmt(p, 5);
   fail_unless(folded_b(tree_value(s), true));
   fail_unless(folded_b(tree_value(tree_stmt(p, 6)), true));
   fail_unless(folded_b(tree_value(tree_stmt(p, 7)), false));
   fail_unless(folded_b(tree_value(tree_stmt(p, 8)), true));
   fail_unless(folded_b(tree_value(tree_stmt(p, 9)), false));
   fail_unless(folded_b(tree_value(tree_stmt(p, 10)), false));
   fail_unless(folded_b(tree_value(tree_stmt(p, 11)), true));
   fail_unless(folded_b(tree_value(tree_stmt(p, 12)), false));
   fail_unless(folded_b(tree_value(tree_stmt(p, 13)), true));
   fail_unless(folded_b(tree_value(tree_stmt(p, 14)), false));
   fail_unless(folded_b(tree_value(tree_stmt(p, 15)), false));
   s = tree_stmt(p, 16);
   fail_unless(tree_kind(tree_value(tree_waveform(s, 0))) == T_ARRAY_REF);
   fail_unless(folded_i(tree_value(tree_waveform(tree_stmt(p, 20), 0)), 5));
   fail_unless(folded_i(tree_value(tree_waveform(tree_stmt(p, 23), 0)), 16));

   p = tree_stmt(a, 1);
   fail_unless(tree_stmts(p) == 4);

   s = tree_stmt(p, 0);
   fail_unless(tree_kind(s) == T_SIGNAL_ASSIGN);
   s = tree_stmt(p, 1);
   fail_unless(tree_kind(s) == T_DUMMY_DRIVER);
   s = tree_stmt(p, 2);
   fail_unless(tree_kind(s) == T_SIGNAL_ASSIGN);
   s = tree_stmt(p, 3);
   fail_unless(tree_kind(s) == T_SEQUENCE);
   fail_unless(tree_stmts(s) == 2);

   p = tree_stmt(a, 2);
   fail_unless(folded_r(tree_value(tree_stmt(p, 0)), 1.0));
   fail_unless(folded_r(tree_value(tree_stmt(p, 1)), 6.0));
   fail_unless(folded_r(tree_value(tree_stmt(p, 2)), 1.0));
   fail_unless(folded_b(tree_value(tree_stmt(p, 3)), true));

   p = tree_stmt(a, 4);
   fail_unless(folded_b(tree_value(tree_stmt(p, 0)), true));
   fail_unless(folded_b(tree_value(tree_stmt(p, 1)), true));
   fail_unless(folded_b(tree_value(tree_stmt(p, 2)), true));
   fail_unless(folded_b(tree_value(tree_stmt(p, 3)), true));
   fail_unless(folded_b(tree_value(tree_stmt(p, 4)), true));

   p = tree_stmt(a, 5);
   fail_unless(tree_stmts(p) == 1);
   fail_unless(tree_kind(tree_stmt(p, 0)) == T_DUMMY_DRIVER);

   p = tree_stmt(a, 6);
   fail_unless(folded_r(tree_value(tree_stmt(p, 0)), 3.0));
   fail_unless(folded_r(tree_value(tree_stmt(p, 1)), 0.6));
   fail_unless(folded_r(tree_value(tree_stmt(p, 2)), 2.5));
   fail_unless(folded_r(tree_value(tree_stmt(p, 3)), 16.0));

   p = tree_stmt(a, 7);
   fail_unless(folded_b(tree_value(tree_stmt(p, 0)), true));
   fail_unless(folded_b(tree_value(tree_stmt(p, 1)), true));

   // Constant real attributes are currently not folded
   p = tree_stmt(a, 8);
   fail_unless(tree_stmts(p) == 1);

   p = tree_stmt(a, 9);
   fail_unless(folded_b(tree_value(tree_stmt(p, 0)), true));
   fail_unless(folded_b(tree_value(tree_stmt(p, 1)), true));
   fail_unless(folded_b(tree_value(tree_stmt(p, 2)), true));
   fail_unless(folded_b(tree_value(tree_stmt(p, 3)), true));
   fail_unless(folded_b(tree_value(tree_stmt(p, 4)), true));
   fail_unless(folded_b(tree_value(tree_stmt(p, 5)), true));
   fail_unless(folded_b(tree_value(tree_stmt(p, 6)), true));
   fail_unless(folded_b(tree_value(tree_stmt(p, 7)), true));
}
END_TEST

START_TEST(test_proc)
{
   tree_t e, a, p, s, r;

   input_from_file(TESTDIR "/simp/proc.vhd");

   e = parse();
   fail_if(e == NULL);
   fail_unless(tree_kind(e) == T_ENTITY);
   lib_put(lib_work(), e);

   a = parse();
   fail_if(a == NULL);
   fail_unless(tree_kind(a) == T_ARCH);

   fail_if_errors();

   mir_context_t *mc = get_mir();
   unit_registry_t *ur = get_registry();
   jit_t *jit = jit_new(ur, mc, NULL);
   simplify_local(a, jit, ur, mc);
   jit_free(jit);

   ////////

   p = tree_stmt(a, 0);
   fail_unless(tree_kind(p) == T_PROCESS);
   fail_if(tree_triggers(p) > 0);
   fail_unless(tree_stmts(p) == 2);

   s = tree_stmt(p, 0);
   fail_unless(tree_kind(s) == T_REPORT);

   s = tree_stmt(p, 1);
   fail_unless(tree_kind(s) == T_WAIT);
   fail_unless(tree_triggers(s) == 2);
   r = tree_trigger(s, 0);
   fail_unless(tree_kind(r) == T_REF);
   fail_unless(tree_ident(r) == ident_new("X"));
   r = tree_trigger(s, 1);
   fail_unless(tree_kind(r) == T_REF);
   fail_unless(tree_ident(r) == ident_new("Y"));

   ////////

   p = tree_stmt(a, 1);
   fail_unless(tree_kind(p) == T_PROCESS);
   fail_unless(tree_stmts(p) == 2);
   fail_if(tree_flags(p) & TREE_F_POSTPONED);

   s = tree_stmt(p, 0);
   fail_unless(tree_kind(s) == T_SIGNAL_ASSIGN);

   s = tree_stmt(p, 1);
   fail_unless(tree_kind(s) == T_WAIT);
   fail_unless(tree_triggers(s) == 1);
   fail_unless(tree_ident(tree_trigger(s, 0)) == ident_new("Y"));

   ////////

   p = tree_stmt(a, 2);
   fail_unless(tree_kind(p) == T_PROCESS);
   fail_unless(tree_stmts(p) == 2);
   fail_unless(tree_flags(p) & TREE_F_POSTPONED);

   s = tree_stmt(p, 0);
   fail_unless(tree_kind(s) == T_IF);
   fail_unless(tree_conds(s) == 3);

   s = tree_stmt(p, 1);
   fail_unless(tree_kind(s) == T_WAIT);
   fail_unless(tree_triggers(s) == 2);
   fail_unless(tree_ident(tree_trigger(s, 0)) == ident_new("Y"));
   fail_unless(tree_ident(tree_trigger(s, 1)) == ident_new("X"));

   ////////

   p = tree_stmt(a, 3);
   fail_unless(tree_kind(p) == T_PROCESS);
   fail_unless(tree_stmts(p) == 2);
}
END_TEST

START_TEST(test_args)
{
   tree_t e, a, p, s, c;

   input_from_file(TESTDIR "/simp/args.vhd");

   e = parse();
   fail_if(e == NULL);
   fail_unless(tree_kind(e) == T_ENTITY);
   lib_put(lib_work(), e);

   a = parse();
   fail_if(a == NULL);
   fail_unless(tree_kind(a) == T_ARCH);

   fail_if_errors();

   mir_context_t *mc = get_mir();
   unit_registry_t *ur = get_registry();
   jit_t *jit = jit_new(ur, mc, NULL);
   simplify_local(a, jit, ur, mc);
   jit_free(jit);

   ////////

   p = tree_stmt(a, 0);
   fail_unless(tree_kind(p) == T_PROCESS);

   for (int i = 0; i < 3; i++) {
      s = tree_stmt(p, i);
      fail_unless(tree_kind(s) == T_VAR_ASSIGN);
      c = tree_value(s);
      fail_unless(tree_kind(c) == T_FCALL);
      fail_unless(tree_params(c) == 2);
      fail_unless(tree_subkind(tree_param(c, 0)) == P_POS);
      fail_unless(icmp(tree_ident(tree_value(tree_param(c, 0))), "A"));
      fail_unless(tree_subkind(tree_param(c, 1)) == P_POS);
      fail_unless(icmp(tree_ident(tree_value(tree_param(c, 1))), "B"));
   }

   s = tree_stmt(p, 3);
   fail_unless(tree_kind(s) == T_PCALL);
   fail_unless(tree_params(s) == 2);
   fail_unless(tree_subkind(tree_param(s, 0)) == P_POS);
   fail_unless(icmp(tree_ident(tree_value(tree_param(s, 0))), "A"));
   fail_unless(tree_subkind(tree_param(s, 1)) == P_POS);
   fail_unless(icmp(tree_ident(tree_value(tree_param(s, 1))), "B"));

   s = tree_stmt(p, 4);
   fail_unless(tree_kind(s) == T_PCALL);
   fail_unless(tree_params(s) == 2);
   fail_unless(tree_subkind(tree_param(s, 0)) == P_POS);
   fail_unless(tree_kind(tree_value(tree_param(s, 0))) == T_OPEN);
   fail_unless(tree_subkind(tree_param(s, 1)) == P_POS);
   fail_unless(icmp(tree_ident(tree_value(tree_param(s, 1))), "A"));
}
END_TEST

START_TEST(test_ffold)
{
   input_from_file(TESTDIR "/simp/ffold.vhd");

   tree_t a = parse_check_and_simplify(T_PACKAGE, T_PACK_BODY,
                                       T_ENTITY, T_ARCH);
   fail_if_errors();

   tree_t b = tree_stmt(a, 0);
   fail_unless(tree_kind(b) == T_BLOCK);

   mir_context_t *mc = get_mir();
   unit_registry_t *ur = get_registry();
   jit_t *jit = jit_new(ur, mc, NULL);
   simplify_global(b, NULL, jit, ur, mc);
   jit_free(jit);
   fail_if_errors();

   fail_unless(folded_i(tree_value(tree_decl(b, 0)), 6));
   fail_unless(folded_i(tree_value(tree_decl(b, 2)), 4));
   fail_unless(folded_i(tree_value(tree_decl(b, 3)), 3));
   fail_unless(folded_i(tree_value(tree_decl(b, 4)), 2));
   fail_unless(folded_i(tree_value(tree_decl(b, 5)), 5));
   fail_unless(folded_i(tree_value(tree_decl(b, 6)), 10));
   fail_unless(folded_b(tree_value(tree_decl(b, 7)), true));
   fail_unless(folded_b(tree_value(tree_decl(b, 8)), true));
   fail_unless(folded_b(tree_value(tree_decl(b, 9)), false));
   fail_unless(folded_r(tree_value(tree_decl(b, 10)), 0.62));
   fail_unless(folded_r(tree_value(tree_decl(b, 11)), 71.7));
   fail_unless(folded_b(tree_value(tree_decl(b, 12)), true));
   fail_unless(folded_b(tree_value(tree_decl(b, 13)), true));
   fail_unless(folded_b(tree_value(tree_decl(b, 14)), false));
   fail_unless(folded_b(tree_value(tree_decl(b, 15)), true));
   fail_if(folded_b(tree_value(tree_decl(b, 16)), true));   // Cannot fold now
   fail_unless(folded_b(tree_value(tree_decl(b, 17)), true));
   fail_unless(folded_b(tree_value(tree_decl(b, 18)), true));
   fail_unless(folded_i(tree_value(tree_decl(b, 19)), 80));
   fail_unless(folded_i(tree_value(tree_decl(b, 20)), 5));
   fail_unless(folded_i(tree_value(tree_decl(b, 21)), 2));
   fail_unless(folded_b(tree_value(tree_decl(b, 22)), true));
   fail_unless(folded_b(tree_value(tree_decl(b, 23)), true));
   fail_unless(folded_b(tree_value(tree_decl(b, 24)), true));
   fail_unless(folded_b(tree_value(tree_decl(b, 25)), true));
   fail_unless(folded_b(tree_value(tree_decl(b, 26)), false));

   int64_t bval;
   fail_unless(folded_int(tree_value(tree_decl(b, 27)), &bval));
   fail_unless(bval == 0);
}
END_TEST

START_TEST(test_ffold2)
{
   input_from_file(TESTDIR "/simp/ffold2.vhd");

   tree_t a = parse_check_and_simplify(T_PACKAGE, T_PACK_BODY,
                                       T_ENTITY, T_ARCH);

   tree_t b = tree_stmt(a, 0);
   fail_unless(tree_kind(b) == T_BLOCK);

   mir_context_t *mc = get_mir();
   unit_registry_t *ur = get_registry();
   jit_t *jit = jit_new(ur, mc, NULL);
   simplify_global(b, NULL, jit, ur, mc);
   jit_free(jit);
   fail_if_errors();

   fail_unless(folded_i(tree_value(tree_decl(b, 0)), 3));
}
END_TEST

START_TEST(test_issue49)
{
   input_from_file(TESTDIR "/simp/issue49.vhd");

   tree_t a = parse_and_check(T_ENTITY, T_ARCH);
   fail_if_errors();

   mir_context_t *mc = get_mir();
   unit_registry_t *ur = get_registry();
   jit_t *jit = jit_new(ur, mc, NULL);
   simplify_local(a, jit, ur, mc);
   jit_free(jit);
}
END_TEST

START_TEST(test_issue155)
{
   input_from_file(TESTDIR "/simp/issue155.vhd");

   tree_t p = parse_and_check(T_PACKAGE);
   fail_if_errors();

   mir_context_t *mc = get_mir();
   unit_registry_t *ur = get_registry();
   jit_t *jit = jit_new(ur, mc, NULL);
   simplify_global(p, NULL, jit, ur, mc);
   jit_free(jit);

   tree_t ar = range_of(tree_type(tree_decl(p, 4)), 0);
   fail_unless(folded_i(tree_left(ar), 7));
   fail_unless(folded_i(tree_right(ar), 0));

   tree_t br = range_of(tree_type(tree_decl(p, 5)), 0);
   fail_unless(folded_i(tree_left(br), 3));
   fail_unless(folded_i(tree_right(br), 0));

   tree_t cr = range_of(tree_type(tree_decl(p, 6)), 0);
   fail_unless(folded_i(tree_left(cr), 1));
   fail_unless(folded_i(tree_right(cr), 0));

   tree_t dr = range_of(tree_type(tree_decl(p, 8)), 0);
   fail_unless(folded_i(tree_left(dr), 2));
   fail_unless(tree_kind(tree_right(dr)) == T_RECORD_REF);
}
END_TEST

START_TEST(test_context)
{
   set_standard(STD_08);

   input_from_file(TESTDIR "/simp/context.vhd");

   lib_t foo = lib_tmp("foo");
   lib_t bar = lib_tmp("bar");

   lib_set_work(foo);
   parse_and_check(T_PACKAGE, T_CONTEXT, -1);

   lib_set_work(bar);
   tree_t e = parse_and_check(T_ENTITY);
   fail_if_errors();

   fail_unless(tree_contexts(e) == 5);

   mir_context_t *mc = get_mir();
   unit_registry_t *ur = get_registry();
   jit_t *jit = jit_new(ur, mc, NULL);
   simplify_local(e, jit, ur, mc);
   jit_free(jit);

   fail_unless(tree_contexts(e) == 7);
   fail_unless(tree_kind(tree_context(e, 5)) == T_LIBRARY);
   fail_unless(tree_kind(tree_context(e, 6)) == T_USE);
}
END_TEST

START_TEST(test_issue212)
{
   input_from_file(TESTDIR "/simp/issue212.vhd");

   tree_t top = run_elab();
   fail_unless(tree_stmts(top) == 1);
}
END_TEST

START_TEST(test_shift2)
{
   input_from_file(TESTDIR "/simp/shift2.vhd");

   tree_t top = run_elab();
   fail_unless(tree_stmts(tree_stmt(top, 0)) == 0);
}
END_TEST

START_TEST(test_issue194)
{
   input_from_file(TESTDIR "/simp/issue194.vhd");

   tree_t p = parse_check_and_simplify(T_PACKAGE, T_PACK_BODY, -1);
   fail_if(p == NULL);
   bounds_check(p);

   tree_t a = parse_check_and_simplify(T_ENTITY, T_ARCH);
   fail_if(a == NULL);
   bounds_check(a);
}
END_TEST

START_TEST(test_issue309)
{
   input_from_file(TESTDIR "/simp/issue309.vhd");

   tree_t top = run_elab();

   fail_unless(tree_stmts(tree_stmt(top, 0)) == 2);
   fail_unless(tree_stmts(tree_stmt(tree_stmt(top, 0), 0)) == 0);
   fail_unless(tree_stmts(tree_stmt(tree_stmt(top, 0), 1)) == 0);
}
END_TEST

START_TEST(test_issue320)
{
   input_from_file(TESTDIR "/simp/issue320.vhd");

   tree_t top = run_elab();

   tree_t b0 = tree_stmt(top, 0);

   tree_t d = get_decl(b0, "INIT_VALUE");
   fail_unless(tree_kind(d) == T_CONST_DECL);
   // This used to be folded but no longer is
   fail_unless(tree_kind(tree_value(d)) == T_FCALL);
}
END_TEST

START_TEST(test_issue321)
{
   input_from_file(TESTDIR "/simp/issue321.vhd");

   tree_t top = run_elab();
   fail_if(top == NULL);

   tree_t test_ng = tree_stmt(top, 0);

   fail_unless(tree_decls(test_ng) == 5);
   tree_t d5 = tree_decl(test_ng, 3);
   fail_unless(tree_kind(tree_value(d5)) == T_LITERAL);
   fail_unless(tree_ival(tree_value(d5)) == 23);
   tree_t d6 = tree_decl(test_ng, 4);
   fail_unless(tree_kind(tree_value(d6)) == T_LITERAL);
   fail_unless(tree_ival(tree_value(d6)) == 5);
}
END_TEST

START_TEST(test_issue331)
{
   input_from_file(TESTDIR "/simp/issue331.vhd");

   tree_t top = run_elab();
   fail_if(top == NULL);

   tree_t test_ng = tree_stmt(top, 0);

   tree_t vec_range = get_decl(test_ng, "VEC_RANGE");
   fail_unless(tree_kind(vec_range) == T_CONST_DECL);

   // Earlier versions of nvc folded this to a T_AGGREGATE
   fail_unless(tree_kind(tree_value(vec_range)) == T_FCALL);
}
END_TEST

START_TEST(test_issue322)
{
   input_from_file(TESTDIR "/simp/issue322.vhd");

   tree_t top = run_elab();
   fail_if(top == NULL);

   tree_t p0 = tree_stmt(tree_stmt(top, 0), 0);
   fail_unless(tree_kind(p0) == T_PROCESS);

   tree_t d0 = tree_decl(p0, 0);
   fail_unless(tree_kind(d0) == T_VAR_DECL);
   fail_unless(tree_kind(tree_value(d0)) == T_FCALL);
}
END_TEST

START_TEST(test_issue332)
{
   set_standard(STD_08);

   input_from_file(TESTDIR "/simp/issue332.vhd");

   tree_t a = parse_check_and_simplify(T_ENTITY, T_ARCH);
   fail_if(a == NULL);

   tree_t p0 = tree_stmt(a, 0);
   fail_unless(tree_kind(p0) == T_PROCESS);

   tree_t p0s1 = tree_stmt(p0, 1);
   fail_unless(tree_kind(p0s1) == T_WAIT);
   fail_unless(tree_triggers(p0s1) == 1);
   fail_unless(icmp(tree_ident(tree_trigger(p0s1, 0)), "CURR_VALUE"));

   tree_t p1 = tree_stmt(a, 1);
   fail_unless(tree_kind(p1) == T_PROCESS);

   tree_t p1s1 = tree_stmt(p1, 1);
   fail_unless(tree_kind(p1s1) == T_WAIT);
   fail_unless(tree_triggers(p1s1) == 1);
   fail_unless(icmp(tree_ident(tree_trigger(p1s1, 0)), "REGS_RDATA"));
}
END_TEST

START_TEST(test_issue344)
{
   input_from_file(TESTDIR "/simp/issue344.vhd");

   tree_t top = run_elab();

   // The assertions could be folded but aren't currently
   tree_t p0 = tree_stmt(tree_stmt(top, 0), 0);
   fail_unless(tree_stmts(p0) == 6);
   fail_unless(tree_kind(tree_value(tree_stmt(p0, 0))) == T_FCALL);

   fail_if_errors();
}
END_TEST

START_TEST(test_issue345)
{
   input_from_file(TESTDIR "/simp/issue345.vhd");

   tree_t top = run_elab();

   fail_unless(tree_stmts(tree_stmt(top, 0)) == 0);
}
END_TEST

START_TEST(test_issue362)
{
   input_from_file(TESTDIR "/simp/issue362.vhd");

   tree_t top = run_elab();

   // Used to be optimised out
   fail_unless(tree_stmts(tree_stmt(top, 0)) == 1);
}
END_TEST

START_TEST(test_constarr)
{
   input_from_file(TESTDIR "/simp/constarr.vhd");

   tree_t top = run_elab();
   tree_t b0 = tree_stmt(top, 0);

   tree_t c1 = get_decl(b0, "C1");
   fail_unless(tree_ident(tree_value(c1)) == ident_new("'1'"));
   tree_t c2 = get_decl(b0, "C2");
   fail_unless(tree_ident(tree_value(c2)) == ident_new("'0'"));
}
END_TEST

START_TEST(test_table)
{
   input_from_file(TESTDIR "/simp/table.vhd");

   tree_t p = parse_check_and_simplify(T_PACKAGE, T_PACK_BODY);
   fail_if(p == NULL);

   tree_t table = get_decl(p, "RESOLUTION_TABLE");

   tree_t r0 = tree_range(type_constraint(tree_type(table)), 0);
   fail_unless(tree_subkind(r0) == RANGE_TO);
   fail_unless(assume_int(tree_left(r0)) == 0);
   fail_unless(assume_int(tree_right(r0)) == 8);
}
END_TEST

START_TEST(test_func9)
{
   input_from_file(TESTDIR "/simp/func9.vhd");

   tree_t e = run_elab();

   // All statements are optimised out
   fail_unless(tree_stmts(tree_stmt(e, 0)) == 0);
}
END_TEST

START_TEST(test_allsens)
{
   set_standard(STD_08);

   input_from_file(TESTDIR "/simp/allsens.vhd");

   tree_t a = parse_check_and_simplify(T_ENTITY, T_ARCH);
   fail_if(a == NULL);

   tree_t w, e;

   // P0: y
   w = tree_stmt(tree_stmt(a, 0), 1);
   fail_unless(tree_kind(w) == T_WAIT);
   fail_unless(tree_triggers(w) == 1);
   e = tree_trigger(w, 0);
   fail_unless(tree_kind(e) == T_REF);
   fail_unless(tree_ident(e) == ident_new("Y"));

   // P1: v(1)
   w = tree_stmt(tree_stmt(a, 1), 1);
   fail_unless(tree_kind(w) == T_WAIT);
   fail_unless(tree_triggers(w) == 1);
   e = tree_trigger(w, 0);
   fail_unless(tree_kind(e) == T_ARRAY_REF);
   fail_unless(tree_kind(tree_value(tree_param(e, 0))) == T_LITERAL);
   fail_unless(tree_kind(tree_value(e)) == T_REF);
   fail_unless(tree_ident(tree_value(e)) == ident_new("V"));

   // P2: v, n
   w = tree_stmt(tree_stmt(a, 2), 1);
   fail_unless(tree_kind(w) == T_WAIT);
   fail_unless(tree_triggers(w) == 2);
   e = tree_trigger(w, 0);
   fail_unless(tree_kind(e) == T_REF);
   fail_unless(tree_ident(e) == ident_new("V"));
   e = tree_trigger(w, 1);
   fail_unless(tree_kind(e) == T_REF);
   fail_unless(tree_ident(e) == ident_new("N"));

   // P3: z(1)(2)
   w = tree_stmt(tree_stmt(a, 3), 1);
   fail_unless(tree_kind(w) == T_WAIT);
   fail_unless(tree_triggers(w) == 1);
   e = tree_trigger(w, 0);
   fail_unless(tree_kind(e) == T_ARRAY_REF);
   fail_unless(tree_kind(tree_value(tree_param(e, 0))) == T_LITERAL);
   e = tree_value(e);
   fail_unless(tree_kind(e) == T_ARRAY_REF);
   fail_unless(tree_kind(tree_value(tree_param(e, 0))) == T_LITERAL);
   fail_unless(tree_kind(tree_value(e)) == T_REF);
   fail_unless(tree_ident(tree_value(e)) == ident_new("Z"));

   // P4: z, n
   w = tree_stmt(tree_stmt(a, 4), 1);
   fail_unless(tree_kind(w) == T_WAIT);
   fail_unless(tree_triggers(w) == 2);
   e = tree_trigger(w, 0);
   fail_unless(tree_kind(e) == T_REF);
   fail_unless(tree_ident(e) == ident_new("Z"));
   e = tree_trigger(w, 1);
   fail_unless(tree_kind(e) == T_REF);
   fail_unless(tree_ident(e) == ident_new("N"));

   // P5: z(2), n
   w = tree_stmt(tree_stmt(a, 5), 1);
   fail_unless(tree_kind(w) == T_WAIT);
   fail_unless(tree_triggers(w) == 2);
   e = tree_trigger(w, 0);
   fail_unless(tree_kind(e) == T_ARRAY_REF);
   fail_unless(tree_kind(tree_value(tree_param(e, 0))) == T_LITERAL);
   fail_unless(tree_ident(tree_value(e)) == ident_new("Z"));
   e = tree_trigger(w, 1);
   fail_unless(tree_kind(e) == T_REF);
   fail_unless(tree_ident(e) == ident_new("N"));

   // P6: v(1 to 2)
   w = tree_stmt(tree_stmt(a, 6), 1);
   fail_unless(tree_kind(w) == T_WAIT);
   fail_unless(tree_triggers(w) == 1);
   e = tree_trigger(w, 0);
   fail_unless(tree_kind(e) == T_ARRAY_SLICE);
   fail_unless(tree_kind(tree_left(tree_range(e, 0))) == T_LITERAL);
   fail_unless(tree_kind(tree_right(tree_range(e, 0))) == T_LITERAL);
   fail_unless(tree_ident(tree_value(e)) == ident_new("V"));

   // P7: v, n
   w = tree_stmt(tree_stmt(a, 7), 1);
   fail_unless(tree_kind(w) == T_WAIT);
   fail_unless(tree_triggers(w) == 2);
   e = tree_trigger(w, 0);
   fail_unless(tree_kind(e) == T_REF);
   fail_unless(tree_ident(e) == ident_new("V"));
   e = tree_trigger(w, 1);
   fail_unless(tree_kind(e) == T_REF);
   fail_unless(tree_ident(e) == ident_new("N"));

   // P8: z, n
   w = tree_stmt(tree_stmt(a, 8), 1);
   fail_unless(tree_kind(w) == T_WAIT);
   fail_unless(tree_triggers(w) == 2);
   e = tree_trigger(w, 0);
   fail_unless(tree_kind(e) == T_REF);
   fail_unless(tree_ident(e) == ident_new("Z"));
   e = tree_trigger(w, 1);
   fail_unless(tree_kind(e) == T_REF);
   fail_unless(tree_ident(e) == ident_new("N"));

   // P9: z(1 to 2), n
   w = tree_stmt(tree_stmt(a, 9), 1);
   fail_unless(tree_kind(w) == T_WAIT);
   fail_unless(tree_triggers(w) == 2);
   e = tree_trigger(w, 0);
   fail_unless(tree_kind(e) == T_ARRAY_SLICE);
   fail_unless(tree_kind(tree_left(tree_range(e, 0))) == T_LITERAL);
   fail_unless(tree_kind(tree_left(tree_range(e, 0))) == T_LITERAL);
   fail_unless(tree_ident(tree_value(e)) == ident_new("Z"));
   e = tree_trigger(w, 1);
   fail_unless(tree_kind(e) == T_REF);
   fail_unless(tree_ident(e) == ident_new("N"));

   // P10: y, n
   w = tree_stmt(tree_stmt(a, 10), 1);
   fail_unless(tree_kind(w) == T_WAIT);
   fail_unless(tree_triggers(w) == 2);
   e = tree_trigger(w, 0);
   fail_unless(tree_kind(e) == T_REF);
   fail_unless(tree_ident(e) == ident_new("N"));
   e = tree_trigger(w, 1);
   fail_unless(tree_kind(e) == T_REF);
   fail_unless(tree_ident(e) == ident_new("Y"));

   // P11: n, y, v(1), z(1)
   w = tree_stmt(tree_stmt(a, 11), 1);
   fail_unless(tree_kind(w) == T_WAIT);
   fail_unless(tree_triggers(w) == 4);
   e = tree_trigger(w, 0);
   fail_unless(tree_kind(e) == T_REF);
   fail_unless(tree_ident(e) == ident_new("N"));
   e = tree_trigger(w, 1);
   fail_unless(tree_kind(e) == T_REF);
   fail_unless(tree_ident(e) == ident_new("Y"));
   e = tree_trigger(w, 2);
   fail_unless(tree_kind(e) == T_ARRAY_REF);
   fail_unless(tree_kind(tree_value(tree_param(e, 0))) == T_LITERAL);
   fail_unless(tree_ident(tree_value(e)) == ident_new("V"));
   e = tree_trigger(w, 3);
   fail_unless(tree_kind(e) == T_ARRAY_REF);
   fail_unless(tree_kind(tree_value(tree_param(e, 0))) == T_LITERAL);
   fail_unless(tree_ident(tree_value(e)) == ident_new("Z"));

   // P12: v(2), x, v(1)
   w = tree_stmt(tree_stmt(a, 12), 1);
   fail_unless(tree_kind(w) == T_WAIT);
   fail_unless(tree_triggers(w) == 3);
   e = tree_trigger(w, 0);
   fail_unless(tree_kind(e) == T_ARRAY_REF);
   fail_unless(tree_kind(tree_value(tree_param(e, 0))) == T_LITERAL);
   fail_unless(tree_ident(tree_value(e)) == ident_new("V"));
   e = tree_trigger(w, 1);
   fail_unless(tree_kind(e) == T_REF);
   fail_unless(tree_ident(e) == ident_new("X"));
   e = tree_trigger(w, 2);
   fail_unless(tree_kind(e) == T_ARRAY_REF);
   fail_unless(tree_kind(tree_value(tree_param(e, 0))) == T_LITERAL);
   fail_unless(tree_ident(tree_value(e)) == ident_new("V"));

   // P13: n, v
   w = tree_stmt(tree_stmt(a, 13), 2);
   fail_unless(tree_kind(w) == T_WAIT);
   fail_unless(tree_triggers(w) == 2);
   e = tree_trigger(w, 0);
   fail_unless(tree_kind(e) == T_REF);
   fail_unless(tree_ident(e) == ident_new("N"));
   e = tree_trigger(w, 1);
   fail_unless(tree_kind(e) == T_REF);
   fail_unless(tree_ident(e) == ident_new("V"));

   // P14: x, y, v(2)
   w = tree_stmt(tree_stmt(a, 14), 1);
   fail_unless(tree_kind(w) == T_WAIT);
   fail_unless(tree_triggers(w) == 3);
   e = tree_trigger(w, 0);
   fail_unless(tree_kind(e) == T_REF);
   fail_unless(tree_ident(e) == ident_new("X"));
   e = tree_trigger(w, 1);
   fail_unless(tree_kind(e) == T_REF);
   fail_unless(tree_ident(e) == ident_new("Y"));
   e = tree_trigger(w, 2);
   fail_unless(tree_kind(e) == T_ARRAY_REF);
   fail_unless(tree_kind(tree_value(tree_param(e, 0))) == T_LITERAL);
   fail_unless(tree_ident(tree_value(e)) == ident_new("V"));

   // P15: n
   w = tree_stmt(tree_stmt(a, 15), 1);
   fail_unless(tree_kind(w) == T_WAIT);
   fail_unless(tree_triggers(w) == 1);
   e = tree_trigger(w, 0);
   fail_unless(tree_kind(e) == T_REF);
   fail_unless(tree_ident(e) == ident_new("N"));

   // P16: v(1)
   w = tree_stmt(tree_stmt(a, 16), 2);
   fail_unless(tree_kind(w) == T_WAIT);
   fail_unless(tree_triggers(w) == 1);
   e = tree_trigger(w, 0);
   fail_unless(tree_kind(e) == T_ARRAY_REF);
   fail_unless(tree_kind(tree_value(tree_param(e, 0))) == T_LITERAL);
   fail_unless(tree_ident(tree_value(e)) == ident_new("V"));
}
END_TEST

START_TEST(test_issue425)
{
   input_from_file(TESTDIR "/simp/issue425.vhd");

   tree_t top = run_elab();
   fail_if(top == NULL);

   tree_t test_ng = tree_stmt(top, 0);

   tree_t m = tree_stmt(test_ng, 0);
   fail_unless(tree_ident(m) == ident_new("M"));

   tree_t c0 = tree_stmt(m, 0);
   fail_unless(tree_ident(c0) == ident_new("C0"));

   tree_t init = tree_decl(c0, 2);
   fail_unless(tree_kind(init) == T_CONST_DECL);
   fail_unless(tree_ident(init) == ident_new("INIT_SIGNALS"));

   // Earlier versions of nvc folded this to a T_AGGREGATE
   fail_unless(tree_kind(tree_value(init)) == T_FCALL);
}
END_TEST

START_TEST(test_static1)
{
   input_from_file(TESTDIR "/simp/static1.vhd");

   tree_t top = run_elab();
   fail_if(top == NULL);

   tree_t bigram = tree_stmt(top, 0);
   fail_unless(tree_kind(bigram) == T_BLOCK);
   fail_unless(tree_ident(bigram) == ident_new("BIGRAM"));

   tree_t addr = tree_decl(bigram, 5);
   fail_unless(tree_kind(addr) == T_SIGNAL_DECL);
   fail_unless(tree_ident(addr) == ident_new("ADDR"));
   fail_unless(folded_i(tree_left(range_of(tree_type(addr), 0)), 9));

   tree_t uut = tree_stmt(tree_stmt(top, 0), 1);
   fail_unless(tree_kind(uut) == T_BLOCK);
   fail_unless(tree_ident(uut) == ident_new("UUT"));

   tree_t addr_p = tree_port(uut, 1);
   fail_unless(tree_kind(addr_p) == T_PORT_DECL);
   fail_unless(tree_ident(addr_p) == ident_new("ADDR"));
   fail_unless(folded_i(tree_left(range_of(tree_type(addr_p), 0)), 9));

   tree_t addr_r = get_decl(uut, "ADDR_R");
   fail_unless(tree_kind(addr_r) == T_SIGNAL_DECL);
   fail_unless(folded_i(tree_left(range_of(tree_type(addr_r), 0)), 9));
}
END_TEST

START_TEST(test_predef)
{
   input_from_file(TESTDIR "/simp/predef.vhd");

   tree_t p = parse_check_and_simplify(T_PACKAGE, T_PACK_BODY);
   fail_if(p == NULL);

   tree_t c1 = tree_decl(p, 1);
   fail_unless(tree_kind(c1) == T_CONST_DECL);
   fail_unless(tree_ident(c1) == ident_new("C1"));
   fail_unless(folded_i(tree_value(c1), 3));

   tree_t c2 = tree_decl(p, 2);
   fail_unless(tree_kind(c2) == T_CONST_DECL);
   fail_unless(tree_ident(c2) == ident_new("C2"));
   fail_unless(folded_b(tree_value(c2), true));

   tree_t c3 = tree_decl(p, 3);
   fail_unless(tree_kind(c3) == T_CONST_DECL);
   fail_unless(tree_ident(c3) == ident_new("C3"));
   fail_unless(tree_kind(tree_value(c3)) == T_FCALL);
}
END_TEST

START_TEST(test_guard)
{
   input_from_file(TESTDIR "/simp/guard.vhd");

   tree_t a = parse_check_and_simplify(T_ENTITY, T_ARCH);
   fail_if(a == NULL);

   tree_t b0 = tree_stmt(a, 0);
   fail_unless(tree_kind(b0) == T_BLOCK);

   tree_t p1 = tree_stmt(b0, 0);
   fail_unless(tree_kind(p1) == T_PROCESS);
   fail_unless(tree_stmts(p1) == 2);

   tree_t g_if1 = tree_stmt(p1, 0);
   fail_unless(tree_kind(g_if1) == T_IF);
   fail_unless(tree_ident(g_if1) == ident_new("guard_if"));
   fail_unless(tree_conds(g_if1) == 1);

   tree_t p2 = tree_stmt(b0, 1);
   fail_unless(tree_kind(p2) == T_PROCESS);
   fail_unless(tree_stmts(p2) == 2);

   tree_t g_if2 = tree_stmt(p2, 0);
   fail_unless(tree_kind(g_if2) == T_IF);
   fail_unless(tree_ident(g_if2) == ident_new("guard_if"));
   fail_unless(tree_conds(g_if2) == 1);
   fail_unless(tree_kind(tree_stmt(tree_cond(g_if2, 0), 0)) == T_CASE);
}
END_TEST

START_TEST(test_copysub)
{
   input_from_file(TESTDIR "/simp/copysub.vhd");

   tree_t top = run_elab();
   fail_if(top == NULL);

   tree_t sub1 = tree_stmt(tree_stmt(top, 0), 0);
   fail_unless(tree_kind(sub1) == T_BLOCK);
   fail_unless(tree_ident(sub1) == ident_new("SUB_1"));

   tree_t sub1_x = get_decl(sub1, "X");
   fail_unless(tree_kind(sub1_x) == T_CONST_DECL);
   // No longer folded
   fail_unless(tree_kind(tree_value(sub1_x)) == T_FCALL);

   tree_t sub2 = tree_stmt(tree_stmt(top, 0), 1);
   fail_unless(tree_kind(sub2) == T_BLOCK);
   fail_unless(tree_ident(sub2) == ident_new("SUB_2"));

   tree_t sub2_x = get_decl(sub2, "X");
   fail_unless(tree_kind(sub2_x) == T_CONST_DECL);
   // No longer folded
   fail_unless(tree_kind(tree_value(sub2_x)) == T_FCALL);
}
END_TEST

START_TEST(test_recrange)
{
   input_from_file(TESTDIR "/simp/recrange.vhd");

   tree_t a = parse_check_and_simplify(T_PACKAGE, T_ENTITY, T_ARCH);

   tree_t p1 = tree_stmt(a, 0);
   fail_unless(tree_kind(p1) == T_PROCESS);

   tree_t s0 = tree_stmt(p1, 0);
   fail_unless(tree_kind(s0) == T_ASSERT);

   tree_t slice = tree_value(tree_param(tree_value(s0), 0));
   fail_unless(tree_kind(slice) == T_ARRAY_SLICE);

   // Simplify pass should rewite the 'RANGE expression as the field
   // bounds are known
   tree_t r = tree_range(slice, 0);
   fail_unless(folded_i(tree_left(r), 1));
   fail_unless(folded_i(tree_right(r), 8));

   tree_t rref = tree_value(slice);
   fail_unless(tree_kind(rref) == T_RECORD_REF);
}
END_TEST

START_TEST(test_order1)
{
   input_from_file(TESTDIR "/simp/order1.vhd");

   const error_t expect[] = {
      {  6, "design unit WORK.ORDER1 replaces a previously analysed unit with "
         "the same name" },
      { -1, NULL }
   };
   expect_errors(expect);

   tree_t p = parse_check_and_simplify(T_PACKAGE, T_PACKAGE);

   tree_t x = get_decl(p, "X");
   fail_unless(tree_kind(tree_value(x)) == T_FCALL);

   check_expected_errors();
}
END_TEST

START_TEST(test_genmap)
{
   input_from_file(TESTDIR "/simp/genmap.vhd");

   tree_t a = parse_check_and_simplify(T_ENTITY, T_PACKAGE, T_ENTITY,
                                       T_ENTITY, T_ARCH);

   tree_t u1 = tree_stmt(a, 0);
   fail_unless(tree_genmaps(u1) == 2);
   fail_unless(tree_subkind(tree_genmap(u1, 0)) == P_POS);
   fail_unless(tree_kind(tree_value((tree_genmap(u1, 0)))) == T_LITERAL);
   fail_unless(tree_subkind(tree_genmap(u1, 1)) == P_POS);
   fail_unless(tree_kind(tree_value((tree_genmap(u1, 1)))) == T_AGGREGATE);

   tree_t u2 = tree_stmt(a, 1);
   fail_unless(tree_genmaps(u2) == 2);

   tree_t u3 = tree_stmt(a, 2);
   fail_unless(tree_genmaps(u3) == 2);
   fail_unless(tree_kind(tree_value(tree_genmap(u3, 1))) == T_AGGREGATE);
   fail_unless(tree_assocs(tree_value(tree_genmap(u3, 1))) == 3);

   tree_t u4 = tree_stmt(a, 3);
   fail_unless(tree_genmaps(u4) == 1);
   fail_unless(tree_kind(tree_value(tree_genmap(u4, 0))) == T_AGGREGATE);
   fail_unless(tree_assocs(tree_value(tree_genmap(u4, 0))) == 2);

   fail_if_errors();
}
END_TEST

START_TEST(test_issue436)
{
   input_from_file(TESTDIR "/simp/issue436.vhd");

   tree_t top = run_elab();
   fail_if(top == NULL);

   tree_t b0 = tree_stmt(top, 0);

   tree_t vec_range = get_decl(b0, "VEC_RANGE");
   fail_unless(tree_kind(vec_range) == T_CONST_DECL);
   fail_unless(tree_kind(tree_value(vec_range)) == T_FCALL);

   tree_t data = get_decl(b0, "DATA");
   fail_unless(tree_kind(data) == T_SIGNAL_DECL);

   // This used to be folded by simp but no longer is
   tree_t r1 = range_of(tree_type(data), 0);
   fail_unless(tree_kind(tree_left(r1)) == T_RECORD_REF);

   tree_t c2 = get_decl(b0, "C2");
   fail_unless(tree_kind(c2) == T_CONST_DECL);
   fail_unless(tree_kind(tree_value(c2)) == T_FCALL);

   tree_t data2 = get_decl(b0, "DATA2");
   fail_unless(tree_kind(data2) == T_SIGNAL_DECL);

   // This used to be folded by simp but no longer is
   tree_t r2 = range_of(tree_type(data2), 0);
   fail_unless(tree_kind(tree_left(r2)) == T_ARRAY_REF);
}
END_TEST

START_TEST(test_issue437)
{
   input_from_file(TESTDIR "/simp/issue437.vhd");

   tree_t top = run_elab();
   fail_if(top == NULL);

   tree_t b0 = tree_stmt(top, 0);

   tree_t u = tree_stmt(b0, 0);
   fail_unless(tree_kind(u) == T_BLOCK);
   fail_unless(tree_ident(u) == ident_new("U"));
   fail_unless(tree_genmaps(u) == 1);

   // This is no longer folded
   fail_unless(tree_kind(tree_value(tree_genmap(u, 0))) == T_ATTR_REF);
}
END_TEST

START_TEST(test_condvar)
{
   set_standard(STD_19);
   input_from_file(TESTDIR "/simp/condvar.vhd");

   tree_t a = parse_check_and_simplify(T_ENTITY, T_ARCH);

   tree_t p = tree_stmt(a, 0);
   fail_unless(tree_kind(p) == T_PROCESS);
   fail_unless(tree_stmts(p) == 2);

   tree_t s0 = tree_stmt(p, 0);
   fail_unless(tree_kind(s0) == T_IF);
   fail_unless(tree_conds(s0) == 2);

   tree_t s0c0 = tree_cond(s0, 0);
   fail_unless(tree_has_value(s0c0));
   fail_unless(tree_stmts(s0c0) == 1);
   fail_unless(tree_kind(tree_stmt(s0c0, 0)) == T_VAR_ASSIGN);

   tree_t s0c1 = tree_cond(s0, 1);
   fail_if(tree_has_value(s0c1));
   fail_unless(tree_stmts(s0c1) == 1);
   fail_unless(tree_kind(tree_stmt(s0c1, 0)) == T_VAR_ASSIGN);

   tree_t s1 = tree_stmt(p, 0);
   fail_unless(tree_kind(s1) == T_IF);
   fail_unless(tree_conds(s1) == 2);

   tree_t s1c0 = tree_cond(s1, 0);
   fail_unless(tree_has_value(s1c0));
   fail_unless(tree_stmts(s1c0) == 1);
   fail_unless(tree_kind(tree_stmt(s1c0, 0)) == T_VAR_ASSIGN);
}
END_TEST

START_TEST(test_issue438)
{
   input_from_file(TESTDIR "/simp/issue438.vhd");

   tree_t top = run_elab();
   fail_if(top == NULL);

   tree_t b0 = tree_stmt(top, 0);

   tree_t data = get_decl(b0, "DATA");
   fail_unless(tree_kind(data) == T_SIGNAL_DECL);

   // This is no longer folded
   tree_t r = range_of(tree_type(data), 0);
   fail_unless(tree_kind(tree_left(r)) == T_RECORD_REF);
}
END_TEST

START_TEST(test_ports2008)
{
   set_standard(STD_08);
   input_from_file(TESTDIR "/simp/ports2008.vhd");

   tree_t a = parse_check_and_simplify(T_ENTITY, T_ENTITY, T_ARCH);

   tree_t b = tree_stmt(a, 0);
   fail_unless(tree_kind(b) == T_BLOCK);

   fail_unless(tree_decls(b) == 1);
   fail_unless(tree_stmts(b) == 1);

   tree_t inst = tree_stmt(b, 0);
   fail_unless(tree_kind(inst) == T_INSTANCE);
   fail_unless(tree_kind(tree_value(tree_param(inst, 0))) == T_INERTIAL);
   fail_unless(tree_kind(tree_value(tree_param(inst, 1))) == T_INERTIAL);
}
END_TEST

START_TEST(test_gentype)
{
   set_standard(STD_08);
   input_from_file(TESTDIR "/simp/gentype.vhd");

   tree_t top = run_elab();

   // This used to get folded
   tree_t b0 = tree_stmt(top, 0);
   fail_unless(tree_stmts(b0) == 1);
}
END_TEST

START_TEST(test_osvvm4)
{
   set_standard(STD_08);
   input_from_file(TESTDIR "/simp/osvvm4.vhd");

   tree_t p = parse_check_and_simplify(T_PACKAGE, T_PACKAGE, T_PACK_INST);
   fail_unless(tree_genmaps(p) == 9);
}
END_TEST

START_TEST(test_grlib1)
{
   input_from_file(TESTDIR "/simp/grlib1.vhd");

   tree_t p = parse_check_and_simplify(T_PACKAGE, T_PACKAGE, T_PACKAGE);

   tree_t width = tree_decl(p, 0);
   fail_unless(tree_ident(width) == ident_new("WIDTH"));
   fail_unless(tree_kind(width) == T_CONST_DECL);

   // This expression is globally static so not folded
   fail_unless(tree_kind(tree_value(width)) == T_FCALL);
}
END_TEST

START_TEST(test_protfold1)
{
   set_standard(STD_02);
   input_from_file(TESTDIR "/simp/protfold1.vhd");

   tree_t top = run_elab();

   // Protected type should not prevent folding
   tree_t b0 = tree_stmt(tree_stmt(top, 0), 0);
   fail_unless(tree_stmts(b0) == 1);
}
END_TEST

START_TEST(test_protfold2)
{
   set_standard(STD_02);
   input_from_file(TESTDIR "/simp/protfold2.vhd");

   tree_t top = run_elab();

   // Protected type should not prevent folding
   tree_t b0 = tree_stmt(tree_stmt(top, 0), 0);
   fail_unless(tree_stmts(b0) == 2);
}
END_TEST

START_TEST(test_foreign1)
{
   input_from_file(TESTDIR "/simp/foreign1.vhd");

   const error_t expect[] = {
      {  8, "foreign function symbol not found" },
      { -1, NULL }
   };
   expect_errors(expect);

   run_elab();

   check_expected_errors();
}
END_TEST

START_TEST(test_simpif1)
{
   input_from_file(TESTDIR "/simp/simpif1.vhd");

   tree_t a = parse_check_and_simplify(T_ENTITY, T_ARCH);

   tree_t p1 = tree_stmt(a, 0);
   fail_unless(tree_kind(p1) == T_PROCESS);
   fail_unless(tree_stmts(p1) == 2);

   tree_t s0 = tree_stmt(p1, 0);
   fail_unless(tree_kind(s0) == T_SEQUENCE);
   fail_unless(tree_stmts(s0) == 2);
}
END_TEST

START_TEST(test_concat)
{
   set_standard(STD_08);

   input_from_file(TESTDIR "/simp/concat.vhd");

   tree_t p = parse_check_and_simplify(T_PACKAGE, T_PACKAGE);

   static const char *strings[] = {
      "100", "xyz", "10", "foo"
   };

   for (int i = 0; i < ARRAY_LEN(strings); i++) {
      tree_t c = tree_decl(p, i);
      fail_unless(tree_kind(c) == T_CONST_DECL);

      tree_t str = tree_value(c);
      fail_unless(tree_kind(str) == T_STRING);

      const int len = strlen(strings[i]);
      ck_assert_int_eq(tree_chars(str), len);
      for (int j = 0; j < len; j++) {
         tree_t lit = tree_char(str, j);
         fail_unless(tree_kind(lit) == T_REF);
         ck_assert_int_eq(ident_char(tree_ident(lit), 1), strings[i][j]);
      }
   }

   static const assoc_kind_t concat[][6] = {
      { A_POS, A_POS, -1 },
      { A_POS, A_POS, A_POS, -1 },
      { A_CONCAT, A_POS, -1 },
      { A_POS, A_POS, A_POS, -1 },
      { A_POS, A_CONCAT, -1 },
      { A_CONCAT, A_CONCAT, -1 },
      { A_POS, A_POS, A_POS, A_POS, -1 },
      { A_POS, A_POS, -1 },
    };

   for (int i = 0; i < ARRAY_LEN(concat); i++) {
      tree_t c = tree_decl(p, i + ARRAY_LEN(strings));
      fail_unless(tree_kind(c) == T_CONST_DECL);

      tree_t str = tree_value(c);
      fail_unless(tree_kind(str) == T_AGGREGATE);

      const int nassocs = tree_assocs(str);
      for (int j = 0; concat[i][j] != -1; j++) {
         if (j >= nassocs || tree_subkind(tree_assoc(str, j)) != concat[i][j]) {
            dump(c);
            ck_abort_msg("wrong association kind at position %d", j);
         }
      }
   }

   fail_if_errors();
}
END_TEST

START_TEST(test_issue496)
{
   input_from_file(TESTDIR "/simp/issue496.vhd");

   tree_t top = run_elab();

   tree_t test = tree_stmt(top, 0);
   fail_unless(tree_kind(test) == T_BLOCK);

   tree_t one = tree_decl(test, 1);
   fail_unless(tree_kind(one) == T_CONST_DECL);
   fail_unless(folded_i(tree_value(one), 1));

   fail_if_errors();
}
END_TEST

START_TEST(test_genpack1)
{
   set_standard(STD_08);

   input_from_file(TESTDIR "/simp/genpack1.vhd");

   tree_t top = run_elab();

   tree_t b0 = tree_stmt(top, 0);
   fail_unless(tree_kind(b0) == T_BLOCK);
   fail_unless(tree_stmts(b0) == 1);

   tree_t p1 = tree_stmt(b0, 0);
   fail_unless(tree_kind(p1) == T_PROCESS);
   fail_unless(tree_stmts(p1) == 2);   // Should have been eliminated

   fail_if_errors();
}
END_TEST

START_TEST(test_casefold1)
{
   input_from_file(TESTDIR "/simp/casefold1.vhd");

   tree_t top = run_elab();

   tree_t b0 = tree_stmt(top, 0);
   fail_unless(tree_kind(b0) == T_BLOCK);
   fail_unless(tree_stmts(b0) == 2);

   tree_t uut = tree_stmt(b0, 1);
   fail_unless(tree_kind(uut) == T_BLOCK);
   fail_unless(tree_ident(uut) == ident_new("UUT"));
   fail_unless(tree_stmts(uut) == 1);

   tree_t mycomp = tree_stmt(uut, 0);
   fail_unless(tree_kind(mycomp) == T_BLOCK);
   fail_unless(tree_ident(mycomp) == ident_new("MYCOMP"));
   fail_unless(tree_stmts(mycomp) == 2);

   tree_t casep = tree_stmt(mycomp, 1);
   fail_unless(tree_kind(casep) == T_PROCESS);
   fail_unless(tree_ident(casep) == ident_new("CASEP"));

   tree_t seq = tree_stmt(casep, 0);
   fail_unless(tree_kind(seq) == T_REPORT);

   fail_if_errors();
}
END_TEST

START_TEST(test_issue574)
{
   set_standard(STD_08);

   input_from_file(TESTDIR "/simp/issue574.vhd");

   tree_t p = parse_check_and_simplify(T_PACKAGE);

   tree_t d = get_decl(p, "C_MEMORY_MAP_DEFAULT");

   tree_t agg = tree_value(d);
   fail_unless(tree_kind(agg) == T_AGGREGATE);

   tree_t n0 = tree_name(tree_assoc(agg, 0));
   fail_unless(tree_kind(n0) == T_LITERAL);
   fail_unless(tree_subkind(n0) == L_INT);
   fail_unless(tree_ival(n0) == 1);

   fail_if_errors();
}
END_TEST

START_TEST(test_casegen)
{
   set_standard(STD_08);

   input_from_file(TESTDIR "/simp/casegen.vhd");

   tree_t a = parse_check_and_simplify(T_ENTITY, T_ARCH);
   ck_assert_int_eq(tree_stmts(a), 1);

   tree_t five = tree_stmt(a, 0);
   fail_unless(tree_kind(five) == T_BLOCK);
   fail_unless(tree_ident(five) == ident_new("FIVE"));
   fail_unless(tree_stmts(five) == 1);

   fail_if_errors();
}
END_TEST

START_TEST(test_condexpr)
{
   set_standard(STD_19);

   input_from_file(TESTDIR "/simp/condexpr.vhd");

   tree_t a = parse_check_and_simplify(T_ENTITY, T_ARCH);

   tree_t c1 = tree_decl(a, 0);
   fail_unless(tree_kind(c1) == T_CONST_DECL);
   fail_unless(tree_kind(tree_value(c1)) == T_LITERAL);

   tree_t foo = tree_decl(a, 1);
   fail_unless(tree_stmts(foo) == 3);

   tree_t s0 = tree_stmt(foo, 0);
   fail_unless(tree_kind(s0) == T_IF);
   fail_unless(tree_conds(s0) == 1);

   tree_t s0c0s0 = tree_stmt(tree_cond(s0, 0), 0);
   fail_unless(tree_kind(s0c0s0) == T_RETURN);

   tree_t s1 = tree_stmt(foo, 1);
   fail_unless(tree_kind(s1) == T_IF);
   fail_unless(tree_conds(s1) == 2);

   tree_t s1c0s0 = tree_stmt(tree_cond(s1, 0), 0);
   fail_unless(tree_kind(s1c0s0) == T_RETURN);

   tree_t s1c1s0 = tree_stmt(tree_cond(s1, 1), 0);
   fail_unless(tree_kind(s1c1s0) == T_RETURN);

   tree_t s2 = tree_stmt(foo, 2);
   fail_unless(tree_kind(s2) == T_IF);
   fail_unless(tree_conds(s2) == 1);

   tree_t bar = tree_decl(a, 2);
   fail_unless(tree_stmts(bar) == 2);

   fail_unless(tree_kind(tree_stmt(bar, 0)) == T_IF);
   fail_unless(tree_kind(tree_stmt(bar, 1)) == T_RETURN);

   fail_if_errors();
}
END_TEST

START_TEST(test_ieee1)
{
   set_standard(STD_08);

   input_from_file(TESTDIR "/simp/ieee1.vhd");

   tree_t p = parse_check_and_simplify(T_PACKAGE);

   tree_t b1 = tree_decl(p, 0);
   fail_unless(tree_ident(tree_value(b1)) == ident_new("'1'"));

   tree_t b2 = tree_decl(p, 1);
   fail_unless(tree_ident(tree_value(b2)) == ident_new("'0'"));

   tree_t b3 = tree_decl(p, 2);
   fail_unless(tree_ident(tree_value(b3)) == ident_new("'1'"));

   fail_if_errors();
}
END_TEST

START_TEST(test_issue742)
{
   input_from_file(TESTDIR "/simp/issue742.vhd");

   tree_t b = parse_check_and_simplify(T_PACKAGE, T_PACK_BODY);

   tree_t test = tree_decl(b, 0);
   fail_unless(tree_kind(test) == T_PROC_BODY);
   fail_unless(tree_stmts(test) == 1);
   fail_unless(tree_kind(tree_stmt(test, 0)) == T_VAR_ASSIGN);

   fail_if_errors();
}
END_TEST

START_TEST(test_cpcall)
{
   input_from_file(TESTDIR "/simp/cpcall.vhd");

   tree_t a = parse_check_and_simplify(T_ENTITY, T_ARCH);

   tree_t w0 = tree_stmt(tree_stmt(a, 0), 1);
   fail_unless(tree_kind(w0) == T_WAIT);
   fail_unless(tree_flags(w0) & TREE_F_STATIC_WAIT);

   tree_t w1 = tree_stmt(tree_stmt(a, 1), 1);
   fail_unless(tree_kind(w1) == T_WAIT);
   fail_if(tree_flags(w1) & TREE_F_STATIC_WAIT);

   fail_if_errors();
}
END_TEST

START_TEST(test_length)
{
   set_standard(STD_19);

   input_from_file(TESTDIR "/simp/length.vhd");

   const error_t expect[] = {
      {  3, "value of LENGTH attribute exceeds universal integer range" },
      { -1, NULL },
   };
   expect_errors(expect);

   parse_check_and_simplify(T_PACKAGE);

   check_expected_errors();
}
END_TEST

START_TEST(test_casearray1)
{
   set_standard(STD_08);

   input_from_file(TESTDIR "/simp/casearray1.vhd");

   tree_t a = parse_check_and_simplify(T_ENTITY, T_ARCH);

   tree_t b = tree_decl(tree_stmt(a, 0), 0);
   fail_unless(tree_kind(b) == T_CONST_DECL);
   fail_unless(tree_kind(tree_value(b)) == T_AGGREGATE);

   tree_t s0 = tree_stmt(tree_stmt(a, 0), 0);
   fail_unless(tree_kind(s0) == T_CASE);

   for (int i = 0; i < 4; i++) {
      tree_t n0 = tree_name(tree_assoc(tree_stmt(s0, i), 0));
      fail_unless(tree_kind(n0) == T_STRING);
      fail_unless(tree_chars(n0) == 8);
   }

   fail_if_errors();
}
END_TEST

START_TEST(test_conv1)
{
   set_standard(STD_08);

   input_from_file(TESTDIR "/simp/conv1.vhd");

   tree_t a = parse_check_and_simplify(T_ENTITY, T_ARCH);

   tree_t p1 = tree_stmt(a, 0);
   fail_unless(tree_kind(p1) == T_PROCESS);

   tree_t c1 = tree_decl(p1, 0);
   fail_unless(tree_kind(tree_value(c1)) == T_AGGREGATE);

   tree_t c2 = tree_decl(p1, 1);
   fail_unless(tree_kind(tree_value(c2)) == T_AGGREGATE);

   tree_t c3 = tree_decl(p1, 2);
   fail_unless(tree_kind(tree_value(c3)) == T_TYPE_CONV);

   tree_t c4 = tree_decl(p1, 3);
   fail_unless(tree_kind(tree_value(c4)) == T_TYPE_CONV);

   fail_if_errors();
}
END_TEST

START_TEST(test_issue782)
{
   set_standard(STD_08);

   input_from_file(TESTDIR "/simp/issue782.vhd");

   tree_t top = run_elab();

   tree_t b_block = tree_stmt(tree_stmt(top, 0), 0);
   fail_unless(tree_kind(b_block) == T_BLOCK);

   tree_t s = tree_decl(b_block, 1);
   fail_unless(tree_kind(s) == T_SIGNAL_DECL);

   // This used to be folded but is not currently
   tree_t left = tree_left(range_of(tree_type(s), 0));
   fail_unless(tree_kind(left) == T_FCALL);

   fail_if_errors();
}
END_TEST

START_TEST(test_order2)
{
   input_from_file(TESTDIR "/simp/order2.vhd");

   tree_t b = parse_check_and_simplify(T_PACKAGE, T_PACK_BODY);
   tree_t p = tree_primary(b);

   fail_unless(tree_kind(tree_value(tree_decl(p, 0))) == T_STRING);
   fail_unless(tree_kind(tree_value(tree_decl(p, 1))) == T_STRING);

   fail_unless(tree_kind(tree_value(tree_decl(b, 0))) == T_STRING);

   fail_if_errors();
}
END_TEST

START_TEST(test_issue812)
{
   input_from_file(TESTDIR "/simp/issue812.vhd");

   run_elab();

   fail_if_errors();
}
END_TEST

START_TEST(test_issue821)
{
   input_from_file(TESTDIR "/simp/issue821.vhd");

   tree_t top = run_elab();

   tree_t g1 = tree_stmt(tree_stmt(top, 0), 0);
   fail_unless(tree_kind(g1) == T_BLOCK);

   tree_t seq = tree_stmt(tree_stmt(g1, 0), 0);
   fail_unless(tree_kind(seq) == T_SEQUENCE);
   fail_unless(tree_stmts(seq) == 2);
   fail_unless(tree_kind(tree_stmt(seq, 0)) == T_IF);
   fail_unless(tree_kind(tree_stmt(seq, 1)) == T_DUMMY_DRIVER);

   fail_if_errors();
}
END_TEST

START_TEST(test_issue867)
{
   input_from_file(TESTDIR "/simp/issue867.vhd");

   tree_t top = run_elab();

   tree_t ent = tree_stmt(top, 0);
   tree_t s1 = get_decl(ent, "S1");
   tree_t r = range_of(tree_type(s1), 0);
   fail_unless(tree_subkind(r) == RANGE_DOWNTO);

   int64_t low, high;
   fail_unless(folded_bounds(r, &low, &high));
   fail_unless(low == 0);
   fail_unless(high == 4);

   fail_if_errors();
}
END_TEST

START_TEST(test_issue882)
{
   set_standard(STD_08);

   input_from_file(TESTDIR "/simp/issue882.vhd");

   tree_t a = parse_check_and_simplify(T_ENTITY, T_ARCH);
   fail_if(a == NULL);

   tree_t p0 = tree_stmt(a, 0);
   fail_unless(tree_kind(p0) == T_PROCESS);

   tree_t p0s1 = tree_stmt(p0, 1);
   fail_unless(tree_kind(p0s1) == T_WAIT);
   fail_unless(tree_triggers(p0s1) == 1);
   fail_unless(icmp(tree_ident(tree_trigger(p0s1, 0)), "MY_A"));

   fail_if_errors();
}
END_TEST

START_TEST(test_packinst1)
{
   set_standard(STD_08);

   input_from_file(TESTDIR "/simp/packinst1.vhd");

   tree_t p = parse_check_and_simplify(T_PACKAGE, T_PACK_INST);
   fail_if(p == NULL);

   tree_t k = get_decl(p, "K");

   double val;
   fail_unless(folded_real(tree_value(k), &val));
   ck_assert_double_eq(val, 2.0);

   fail_if_errors();
}
END_TEST

START_TEST(test_physical)
{
   input_from_file(TESTDIR "/simp/physical.vhd");

   const error_t expect[] = {
      {  2, "physical literal 1000 HR exceeds range of type TIME" },
      {  3, "physical literal 42141.4 HR exceeds range of type TIME" },
      { -1, NULL },
   };
   expect_errors(expect);

   tree_t p = parse_check_and_simplify(T_PACKAGE);

   tree_t c3 = get_decl(p, "C3");

   int64_t val;
   fail_unless(folded_int(tree_value(c3), &val));
   ck_assert_int_eq(val, INT64_C(8200000000000));

   check_expected_errors();
}
END_TEST

START_TEST(test_synth1)
{
   opt_set_int(OPT_CHECK_SYNTHESIS, 1);

   input_from_file(TESTDIR "/simp/synth1.vhd");

   const error_t expect[] = {
      { 22, "signal Z is read in process P1 but is not in the sensitivity" },
      { 22, "signal Y is read in process P1 but is not in the sensitivity" },
      { 39, "signal CLK is read in process P4 but is not in the" },
      { 55, "signal RESET is read in process P6 but is not in the" },
      { 80, "signal R is read in the process but is not in the sensitivity" },
      { -1, NULL },
   };
   expect_errors(expect);

   tree_t a = parse_check_and_simplify(T_ENTITY, T_ARCH);

   tree_t p1 = tree_stmt(a, 0);
   fail_unless(tree_kind(p1) == T_PROCESS);

   tree_t w = tree_stmt(p1, 1);
   fail_unless(tree_kind(w) == T_WAIT);
   fail_unless(tree_triggers(w) == 1);   // Should not be modified

   check_expected_errors();
}
END_TEST

START_TEST(test_genpack2)
{
   set_standard(STD_08);

   input_from_file(TESTDIR "/simp/genpack2.vhd");

   tree_t p3 = parse_check_and_simplify(T_PACKAGE, T_PACKAGE, T_PACKAGE);
   ck_assert_int_eq(tree_generics(p3), 1);

   tree_t p3g0 = tree_generic(p3, 0);
   fail_unless(tree_class(p3g0) == C_PACKAGE);

   tree_t p3g0v = tree_value(p3g0);
   fail_unless(tree_kind(p3g0v) == T_PACKAGE_MAP);
   fail_unless(tree_subkind(p3g0v) == PACKAGE_MAP_BOX);
   fail_unless(tree_genmaps(p3g0v) == 0);

   tree_t pi = tree_decl(p3, 0);
   fail_unless(tree_kind(pi) == T_PACK_INST);

   tree_t c = tree_decl(pi, 0);
   fail_unless(tree_kind(c) == T_CONST_DECL);

   int64_t val;
   fail_unless(folded_int(tree_value(c), &val));
   ck_assert_int_eq(val, 3);

   fail_if_errors();
}
END_TEST

START_TEST(test_issue1182)
{
   set_standard(STD_08);

   input_from_file(TESTDIR "/simp/issue1182.vhd");

   tree_t a = parse_check_and_simplify(T_PACKAGE, T_PACKAGE, T_ENTITY, T_ARCH);

   tree_t p_proc = tree_stmt(a, 0);
   fail_unless(tree_stmts(p_proc) == 4);

   tree_t a0 = tree_stmt(p_proc, 0);
   fail_unless(tree_kind(a0) == T_ASSERT);
   fail_unless(tree_kind(tree_value(a0)) == T_FCALL);  // Could be folded

   tree_t a2 = tree_stmt(p_proc, 2);
   fail_unless(tree_kind(a2) == T_ASSERT);
   fail_unless(tree_kind(tree_value(a2)) == T_FCALL);  // Could be folded

   fail_if_errors();
}
END_TEST

START_TEST(test_issue1239)
{
   set_standard(STD_08);

   input_from_file(TESTDIR "/simp/issue1239.vhd");

   tree_t a = parse_check_and_simplify(T_ENTITY, T_ARCH);

   tree_t r = tree_stmt(tree_stmt(a, 0), 0);
   fail_unless(tree_kind(r) == T_REPORT);

   tree_t s = tree_message(r);
   fail_unless(tree_kind(s) == T_STRING);
   fail_unless(tree_chars(s) == 1);
   fail_unless(tree_ident(tree_char(s, 0)) == ident_new("'2'"));

   fail_if_errors();
}
END_TEST

START_TEST(test_issue1318)
{
   set_standard(STD_08);

   input_from_file(TESTDIR "/parse/issue1318.vhd");

   tree_t a = parse_check_and_simplify(T_ENTITY, T_ARCH);
   tree_t d = get_decl(a, "CONST_USING_BUS_ARRAY");

   tree_t r = range_of(tree_type(d), 0);
   fail_unless(tree_subkind(r) == RANGE_DOWNTO);

   int64_t low, high;
   fail_unless(folded_bounds(r, &low, &high));
   fail_unless(low == 0);
   fail_unless(high == 31);

   fail_unless(parse() == NULL);

   fail_if_errors();
}
END_TEST

Suite *get_simp_tests(void)
{
   Suite *s = suite_create("simplify");

   TCase *tc_core = nvc_unit_test();
   tcase_add_test(tc_core, test_cfold);
   tcase_add_test(tc_core, test_proc);
   tcase_add_test(tc_core, test_args);
   tcase_add_test(tc_core, test_ffold);
   tcase_add_test(tc_core, test_issue49);
   tcase_add_test(tc_core, test_issue155);
   tcase_add_test(tc_core, test_context);
   tcase_add_test(tc_core, test_issue212);
   tcase_add_test(tc_core, test_shift2);
   tcase_add_test(tc_core, test_issue194);
   tcase_add_test(tc_core, test_issue309);
   tcase_add_test(tc_core, test_issue320);
   tcase_add_test(tc_core, test_issue321);
   tcase_add_test(tc_core, test_issue322);
   tcase_add_test(tc_core, test_ffold2);
   tcase_add_test(tc_core, test_issue331);
   tcase_add_test(tc_core, test_issue332);
   tcase_add_test(tc_core, test_issue344);
   tcase_add_test(tc_core, test_issue345);
   tcase_add_test(tc_core, test_issue362);
   tcase_add_test(tc_core, test_constarr);
   tcase_add_test(tc_core, test_table);
   tcase_add_test(tc_core, test_func9);
   tcase_add_test(tc_core, test_allsens);
   tcase_add_test(tc_core, test_issue425);
   tcase_add_test(tc_core, test_static1);
   tcase_add_test(tc_core, test_predef);
   tcase_add_test(tc_core, test_guard);
   tcase_add_test(tc_core, test_copysub);
   tcase_add_test(tc_core, test_recrange);
   tcase_add_test(tc_core, test_order1);
   tcase_add_test(tc_core, test_genmap);
   tcase_add_test(tc_core, test_issue436);
   tcase_add_test(tc_core, test_issue437);
   tcase_add_test(tc_core, test_condvar);
   tcase_add_test(tc_core, test_issue438);
   tcase_add_test(tc_core, test_ports2008);
   tcase_add_test(tc_core, test_gentype);
   tcase_add_test(tc_core, test_osvvm4);
   tcase_add_test(tc_core, test_grlib1);
   tcase_add_test(tc_core, test_protfold1);
   tcase_add_test(tc_core, test_protfold2);
   tcase_add_test(tc_core, test_foreign1);
   tcase_add_test(tc_core, test_simpif1);
   tcase_add_test(tc_core, test_concat);
   tcase_add_test(tc_core, test_issue496);
   tcase_add_test(tc_core, test_genpack1);
   tcase_add_test(tc_core, test_casefold1);
   tcase_add_test(tc_core, test_issue574);
   tcase_add_test(tc_core, test_casegen);
   tcase_add_test(tc_core, test_condexpr);
   tcase_add_test(tc_core, test_ieee1);
   tcase_add_test(tc_core, test_issue742);
   tcase_add_test(tc_core, test_cpcall);
   tcase_add_test(tc_core, test_length);
   tcase_add_test(tc_core, test_casearray1);
   tcase_add_test(tc_core, test_conv1);
   tcase_add_test(tc_core, test_issue782);
   tcase_add_test(tc_core, test_order2);
   tcase_add_test(tc_core, test_issue812);
   tcase_add_test(tc_core, test_issue821);
   tcase_add_test(tc_core, test_issue867);
   tcase_add_test(tc_core, test_issue882);
   tcase_add_test(tc_core, test_packinst1);
   tcase_add_test(tc_core, test_physical);
   tcase_add_test(tc_core, test_synth1);
   tcase_add_test(tc_core, test_genpack2);
   tcase_add_test(tc_core, test_issue1182);
   tcase_add_test(tc_core, test_issue1239);
   tcase_add_test(tc_core, test_issue1318);
   suite_add_tcase(s, tc_core);

   return s;
}
