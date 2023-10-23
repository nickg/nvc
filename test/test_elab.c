//
//  Copyright (C) 2011-2022  Nick Gasson
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
#include "jit/jit.h"
#include "lib.h"
#include "phase.h"
#include "rt/cover.h"
#include "scan.h"
#include "type.h"

#include <stdio.h>
#include <string.h>
#include <stdlib.h>

START_TEST(test_elab1)
{
   input_from_file(TESTDIR "/elab/elab1.vhd");

   (void)run_elab();

   fail_if_errors();
}
END_TEST

START_TEST(test_elab2)
{
   input_from_file(TESTDIR "/elab/elab2.vhd");

   (void)run_elab();

   fail_if_errors();
}
END_TEST

START_TEST(test_elab3)
{
   input_from_file(TESTDIR "/elab/elab3.vhd");

   (void)run_elab();

   fail_if_errors();
}
END_TEST

START_TEST(test_elab4)
{
   input_from_file(TESTDIR "/elab/elab4.vhd");

   const error_t expect[] = {
      { 21, "actual length 9 does not match formal length 8" },
      { -1, NULL }
   };
   expect_errors(expect);

   (void)run_elab();

   check_expected_errors();
}
END_TEST

START_TEST(test_open)
{
   tree_t top;

   input_from_file(TESTDIR "/elab/open.vhd");

   top = run_elab();
   fail_if(top == NULL);

   // We used to delete all statements here but the behaviour
   // has changed
   fail_unless(tree_stmts(top) == 1);

   fail_if_errors();
}
END_TEST

START_TEST(test_genagg)
{
   input_from_file(TESTDIR "/elab/genagg.vhd");

   (void)run_elab();

   fail_if_errors();
}
END_TEST

START_TEST(test_comp)
{
   input_from_file(TESTDIR "/elab/comp.vhd");

   const error_t expect[] = {
      { 55, "port Y not found in entity WORK.E2" },
      { 62, "type of port X in component declaration E3 is BIT" },
      { -1, NULL }
   };
   expect_errors(expect);

   (void)run_elab();

   check_expected_errors();
}
END_TEST

START_TEST(test_issue17)
{
   input_from_file(TESTDIR "/elab/issue17.vhd");

   (void)run_elab();

   fail_if_errors();
}
END_TEST

START_TEST(test_issue19)
{
   input_from_file(TESTDIR "/elab/issue19.vhd");

   tree_t e = run_elab();
   fail_if(e == NULL);
   fail_unless(tree_stmts(e) == 1);

   fail_if_errors();
}
END_TEST

START_TEST(test_bounds10)
{
   input_from_file(TESTDIR "/elab/bounds10.vhd");

   const error_t expect[] = {
      { 10, "length of value 1 does not match length of target 101" },
      { -1, NULL }
   };
   expect_errors(expect);

   (void)run_elab();
}
END_TEST

START_TEST(test_copy1)
{
   input_from_file(TESTDIR "/elab/copy1.vhd");

   tree_t e = run_elab();
   fail_if(e == NULL);

   tree_t copy1 = tree_stmt(e, 0);
   tree_t sub1_i = tree_stmt(copy1, 0);
   tree_t sub2_i = tree_stmt(copy1, 1);

   tree_t func1 = search_decls(sub1_i, ident_new("DOUBLE"), 0);
   tree_t func2 = search_decls(sub2_i, ident_new("DOUBLE"), 0);
   fail_if(func1 == NULL);
   fail_if(func2 == NULL);
   fail_if(func1 == func2);   // Should copy functions

   tree_t var1 = search_decls(sub1_i, ident_new("GLOBAL"), 0);
   tree_t var2 = search_decls(sub2_i, ident_new("GLOBAL"), 0);
   fail_if(var1 == NULL);
   fail_if(var2 == NULL);
   fail_if(var1 == var2);   // Should copy variables

   fail_if_errors();
}
END_TEST

START_TEST(test_record)
{
   input_from_file(TESTDIR "/elab/record.vhd");

   fail_if(run_elab() == NULL);

   fail_if_errors();
}
END_TEST

START_TEST(test_ifgen)
{
   input_from_file(TESTDIR "/elab/ifgen.vhd");

   fail_if(run_elab() == NULL);
   fail_if_errors();
}
END_TEST

START_TEST(test_open2)
{
   input_from_file(TESTDIR "/elab/open2.vhd");

   const error_t expect[] = {
      { -1, NULL }
   };
   expect_errors(expect);

   fail_if(run_elab() == NULL);
   fail_if_errors();
}
END_TEST

START_TEST(test_issue93)
{
   input_from_file(TESTDIR "/elab/issue93.vhd");

   const error_t expect[] = {
      { -1, NULL }
   };
   expect_errors(expect);

   tree_t top = run_elab();
   fail_if(top == NULL);
   fail_unless(tree_stmts(tree_stmt(top, 0)) == 4);

   fail_if_errors();
}
END_TEST

START_TEST(test_const1)
{
   input_from_file(TESTDIR "/elab/const1.vhd");

   const error_t expect[] = {
      { -1, NULL }
   };
   expect_errors(expect);

   tree_t top = run_elab();
   fail_if(top == NULL);

   tree_t pwm_1 = tree_stmt(tree_stmt(top, 0), 0);
   fail_unless(tree_kind(pwm_1) == T_BLOCK);

   tree_t ctr_r = tree_decl(pwm_1, tree_decls(pwm_1) - 1);
   fail_unless(tree_kind(ctr_r) == T_SIGNAL_DECL);
   fail_unless(tree_ident(ctr_r) == ident_new("CTR_R"));

   int64_t len;
   fail_unless(folded_length(range_of(tree_type(ctr_r), 0), &len));
   fail_unless(len == 15);

   fail_if_errors();
}
END_TEST

START_TEST(test_libbind)
{
   input_from_file(TESTDIR "/elab/libbind.vhd");

   lib_t work = lib_work();

   lib_t other = lib_tmp("other");
   lib_set_work(other);
   parse_check_and_simplify(T_ENTITY, T_ARCH, -1);
   fail_if_errors();

   lib_set_work(work);
   fail_if(run_elab() == NULL);

   fail_if_errors();
}
END_TEST

START_TEST(test_issue153)
{
   input_from_file(TESTDIR "/elab/issue153.vhd");

   // This used to produce an error at elaboration time
   fail_if(run_elab() == NULL);

   fail_if_errors();
}
END_TEST

START_TEST(test_issue157)
{
   input_from_file(TESTDIR "/elab/issue157.vhd");

   fail_if(run_elab() == NULL);
   fail_if_errors();
}
END_TEST

START_TEST(test_issue159)
{
   input_from_file(TESTDIR "/elab/issue159.vhd");

   lib_t work = lib_work();

   lib_t other = lib_tmp("dummy");
   lib_set_work(other);
   parse_check_and_simplify(T_PACKAGE, T_ENTITY, T_ARCH, -1);
   fail_if_errors();

   lib_set_work(work);
   fail_if(run_elab() == NULL);
   fail_if_errors();
}
END_TEST

START_TEST(test_issue175)
{
   input_from_file(TESTDIR "/elab/issue175.vhd");

   lib_t lib2 = lib_tmp("lib2");
   lib_set_work(lib2);
   parse_check_and_simplify(T_PACKAGE, T_PACK_BODY, T_PACKAGE, T_PACK_BODY, -1);
   fail_if_errors();

   lib_t lib = lib_tmp("lib");
   lib_set_work(lib);
   fail_if(run_elab() == NULL);
   fail_if_errors();
}
END_TEST

START_TEST(test_issue184)
{
   input_from_file(TESTDIR "/elab/issue184.vhd");

   tree_t top = run_elab();
   fail_if(top == NULL);

   tree_t gen_cfg2 = tree_stmt(tree_stmt(top, 0), 0);
   fail_unless(tree_kind(gen_cfg2) == T_BLOCK);

   fail_unless(tree_stmts(gen_cfg2) == 1);
   fail_unless(icmp(tree_ident(tree_stmt(gen_cfg2, 0)), "GOOD"));

   fail_if_errors();
}
END_TEST

START_TEST(test_libbind2)
{
   input_from_file(TESTDIR "/elab/libbind2.vhd");

   lib_t work = lib_work();

   lib_t other = lib_tmp("other");
   lib_set_work(other);
   parse_check_and_simplify(T_ENTITY, T_ARCH, -1);
   fail_if_errors();

   lib_set_work(work);
   fail_if(run_elab() == NULL);
   fail_if_errors();
}
END_TEST

START_TEST(test_toplevel2)
{
   input_from_file(TESTDIR "/elab/toplevel2.vhd");

   const error_t expect[] = {
      { LINE_INVALID, "generic value for UNUSED not used" },
      { -1, NULL }
   };
   expect_errors(expect);

   elab_set_generic("I", "4");
   elab_set_generic("S", "hello");
   elab_set_generic("B", "'1'");
   elab_set_generic("V", "101");
   elab_set_generic("UNUSED", "6678");

   tree_t top = run_elab();
   fail_if(top == NULL);
   fail_unless(tree_stmts(tree_stmt(top, 0)) == 3);

   check_expected_errors();
}
END_TEST

START_TEST(test_libbind3)
{
   input_from_file(TESTDIR "/elab/libbind3.vhd");

   lib_set_work(lib_tmp("foo"));
   parse_check_and_simplify(T_PACKAGE, T_ENTITY, T_ARCH, T_ENTITY, T_ARCH, -1);
   fail_if_errors();

   lib_set_work(lib_tmp("bar"));
   fail_if(run_elab() == NULL);
   fail_if_errors();
}
END_TEST

START_TEST(test_issue251)
{
   input_from_file(TESTDIR "/elab/issue251.vhd");

   const error_t expect[] = {
      { 24, "array X index -1 outside of NATURAL range 3 downto 0" },
      { 30, "array A index -1 outside of NATURAL range 3 downto 0" },
      { -1, NULL }
   };
   expect_errors(expect);

   tree_t a = parse_check_and_simplify(T_ENTITY, T_ARCH, T_ENTITY, T_ARCH);
   bounds_check(a);

   check_expected_errors();
}
END_TEST

START_TEST(test_jcore1)
{
   input_from_file(TESTDIR "/elab/jcore1.vhd");

   tree_t top = run_elab();
   fail_if(top == NULL);

   tree_t sub_i = tree_stmt(tree_stmt(top, 0), 0);
   fail_unless(tree_kind(sub_i) == T_BLOCK);
   fail_unless(tree_params(sub_i) == 2);

   tree_t p0 = tree_param(sub_i, 0);
   fail_unless(tree_subkind(p0) == P_POS);
   fail_unless(tree_kind(tree_value(p0)) == T_RECORD_REF);

   fail_if_errors();
}
END_TEST

START_TEST(test_eval1)
{
   input_from_file(TESTDIR "/elab/eval1.vhd");

   const error_t expect[] = {
      { 12, "index -1 outside of INTEGER range 7 downto 0" },
      { 16, "generate expression is not static" },
      { -1, NULL }
   };
   expect_errors(expect);

   fail_unless(run_elab() == NULL);

   check_expected_errors();
}
END_TEST

START_TEST(test_issue305)
{
   input_from_file(TESTDIR "/elab/issue305.vhd");

   tree_t top = run_elab();
   fail_if(top == NULL);

   tree_t s = tree_decl(tree_stmt(top, 0), 2);
   fail_unless(tree_kind(s) == T_SIGNAL_DECL);
   fail_unless(icmp(tree_ident(s), "DATA_I"));

   // This used to be folded but currently not
   int64_t len;
   fail_if(folded_length(range_of(tree_type(s), 0), &len));

   fail_if_errors();
}
END_TEST

START_TEST(test_gbounds)
{
   input_from_file(TESTDIR "/elab/gbounds.vhd");

   tree_t top = run_elab();
   fail_if(top == NULL);
   fail_if_errors();
}
END_TEST

START_TEST(test_issue307)
{
   input_from_file(TESTDIR "/elab/issue307.vhd");

   tree_t top = run_elab();
   fail_if(top == NULL);

   tree_t proc = tree_stmt(tree_stmt(top, 0), 0);
   fail_unless(tree_kind(proc) == T_PROCESS);
   tree_t s0 = tree_stmt(proc, 0);
   fail_unless(tree_kind(s0) == T_PCALL);

   fail_if_errors();
}
END_TEST

START_TEST(test_issue315)
{
   input_from_file(TESTDIR "/elab/issue315.vhd");

   tree_t top = run_elab();
   fail_if(top == NULL);

   tree_t d2 = search_decls(tree_stmt(top, 0), ident_new("INFO"), 0);
   fail_if(d2 == NULL);

   // Earlier versions of nvc folded this to a T_AGGREGATE
   fail_unless(tree_kind(tree_value(d2)) == T_FCALL);

   fail_if_errors();
}
END_TEST

START_TEST(test_issue325)
{
   input_from_file(TESTDIR "/elab/issue325.vhd");

   lib_set_work(lib_tmp("foo"));
   tree_t top = run_elab();
   fail_if(top == NULL);

   fail_if_errors();
}
END_TEST

START_TEST(test_issue328)
{
   input_from_file(TESTDIR "/elab/issue328.vhd");

   lib_set_work(lib_tmp("foo"));
   tree_t top = run_elab();
   fail_if(top == NULL);

   tree_t b0 = tree_stmt(top, 0);

   tree_t vec_range = search_decls(b0, ident_new("VEC_RANGE"), 0);
   fail_if(vec_range == NULL);
   fail_unless(tree_kind(vec_range) == T_CONST_DECL);

   // Earlier versions of nvc folded this to a T_AGGREGATE
   fail_unless(tree_kind(tree_value(vec_range)) == T_FCALL);

   fail_if_errors();
}
END_TEST

START_TEST(test_issue330)
{
   input_from_file(TESTDIR "/elab/issue330.vhd");

   tree_t top = run_elab();
   fail_if(top == NULL);

   tree_t b0 = tree_stmt(top, 0);

   tree_t vec_range = search_decls(b0, ident_new("VEC_RANGE"), 0);
   fail_if(vec_range == NULL);
   fail_unless(tree_kind(vec_range) == T_CONST_DECL);

   // Earlier versions of nvc folded this to a T_AGGREGATE
   fail_unless(tree_kind(tree_value(vec_range)) == T_FCALL);

   fail_if_errors();
}
END_TEST

START_TEST(test_issue336)
{
   input_from_file(TESTDIR "/elab/issue336.vhd");

   tree_t e = run_elab();
   fail_if(e == NULL);
   fail_unless(tree_stmts(tree_stmt(e, 0)) == 3);

   fail_if_errors();
}
END_TEST

START_TEST(test_openinout)
{
   input_from_file(TESTDIR "/elab/openinout.vhd");

   tree_t e = run_elab();

   tree_t b_top = tree_stmt(e, 0);

   tree_t b_uut = tree_stmt(b_top, 0);
   fail_unless(tree_ident(b_uut) == ident_new("UUT"));
   fail_unless(tree_params(b_uut) == 4);

   tree_t b_uut_m0 = tree_param(b_uut, 0);
   fail_unless(tree_subkind(b_uut_m0) == P_POS);
   fail_unless(tree_kind(tree_value(b_uut_m0)) == T_OPEN);

   tree_t p0 = tree_stmt(b_uut, 0);
   tree_t p0s0 = tree_stmt(p0, 0);
   fail_unless(tree_kind(p0s0) == T_SIGNAL_ASSIGN);
   tree_t p0s0w0 = tree_value(tree_waveform(p0s0, 0));
   fail_unless(tree_kind(p0s0w0) == T_FCALL);

   tree_t p1 = tree_stmt(b_uut, 1);
   tree_t p1s0 = tree_stmt(p1, 0);
   fail_unless(tree_kind(p1s0) == T_SIGNAL_ASSIGN);
   tree_t p1s0w0 = tree_value(tree_waveform(p1s0, 0));
   fail_unless(tree_kind(p1s0w0) == T_FCALL);

   tree_t b_uut2 = tree_stmt(b_top, 1);
   fail_unless(tree_ident(b_uut2) == ident_new("UUT2"));
   fail_unless(tree_params(b_uut2) == 2);

   tree_t b_sub = tree_stmt(b_uut2, 0);
   fail_unless(tree_ident(b_sub) == ident_new("SUB_I"));
   fail_unless(tree_params(b_sub) == 4);

   tree_t p2 = tree_stmt(b_sub, 0);
   tree_t p2s0 = tree_stmt(p2, 0);
   fail_unless(tree_kind(p2s0) == T_SIGNAL_ASSIGN);
   tree_t p2s0w0 = tree_value(tree_waveform(p2s0, 0));
   fail_unless(tree_kind(p2s0w0) == T_FCALL);

   fail_if_errors();
}
END_TEST

START_TEST(test_opencase)
{
   input_from_file(TESTDIR "/elab/opencase.vhd");

   tree_t e = run_elab();

   tree_t uut = tree_stmt(tree_stmt(e, 0), 0);
   fail_unless(tree_params(uut) == 2);
   fail_unless(tree_stmts(uut) == 1);

   tree_t py = tree_param(uut, 1);
   fail_unless(tree_subkind(py) == P_POS);
   fail_unless(tree_pos(py) == 1);
   fail_unless(tree_kind(tree_value(py)) == T_OPEN);

   fail_if_errors();
}
END_TEST

START_TEST(test_issue232)
{
   input_from_file(TESTDIR "/elab/issue232.vhd");

   tree_t e = run_elab();

   tree_t e1 = tree_stmt(tree_stmt(e, 0), 0);
   fail_unless(tree_kind(e1) == T_BLOCK);
   tree_t p0 = tree_stmt(e1, 0);
   fail_unless(tree_kind(p0) == T_PROCESS);
   tree_t s0 = tree_stmt(p0, 0);
   fail_unless(tree_kind(s0) == T_SIGNAL_ASSIGN);
   fail_unless(tree_params(e1) == 2);
   fail_unless(tree_kind(tree_value(tree_param(e1, 0))) == T_OPEN);
   tree_t v = tree_value(tree_port(e1, 0));
   fail_unless(tree_kind(v) == T_STRING);
   fail_unless(tree_chars(v) == 2);
   fail_unless(tree_ident(tree_char(v, 0)) == ident_new("'A'"));
   fail_unless(tree_ident(tree_char(v, 1)) == ident_new("'B'"));

   fail_if_errors();
}
END_TEST

START_TEST(test_issue373)
{
   input_from_file(TESTDIR "/elab/issue373.vhd");

   tree_t e = run_elab();

   tree_t p0 = tree_stmt(tree_stmt(tree_stmt(e, 0), 0), 0);
   fail_unless(tree_kind(p0) == T_PROCESS);
   tree_t s0 = tree_stmt(p0, 0);
   fail_unless(tree_kind(s0) == T_ASSERT);
   tree_t m = tree_message(s0);
   fail_unless(tree_kind(m) == T_REF);
   // This used to get folded by the old evaluator

   fail_if_errors();
}
END_TEST

START_TEST(test_issue374)
{
   input_from_file(TESTDIR "/elab/issue374.vhd");

   tree_t e = run_elab();
   fail_if(e == NULL);
   fail_if_errors();
}
END_TEST

START_TEST(test_issue404)
{
   input_from_file(TESTDIR "/elab/issue404.vhd");

   tree_t e = run_elab();
   fail_if(e == NULL);

   tree_t p0 = tree_stmt(tree_stmt(e, 0), 0);
   fail_unless(tree_kind(p0) == T_PROCESS);
   tree_t s0 = tree_stmt(p0, 0);
   fail_unless(tree_kind(s0) == T_SIGNAL_ASSIGN);
   tree_t v0 = tree_value(tree_waveform(s0, 0));

   // Earlier versions of nvc folded this to a T_AGGREGATE
   fail_unless(tree_kind(v0) == T_FCALL);

   fail_if_errors();
}
END_TEST

START_TEST(test_block1)
{
   input_from_file(TESTDIR "/elab/block1.vhd");

   tree_t e = run_elab();
   fail_if(e == NULL);

   tree_t b = tree_stmt(tree_stmt(e, 0), 0);
   fail_unless(tree_kind(b) == T_BLOCK);

   fail_unless(tree_params(b) == 2);
   fail_unless(tree_genmaps(b) == 1);

   fail_if_errors();
}
END_TEST

START_TEST(test_open3)
{
   input_from_file(TESTDIR "/elab/open3.vhd");

   tree_t e = run_elab();
   fail_if(e == NULL);

   tree_t b = tree_stmt(tree_stmt(e, 0), 0);
   fail_unless(tree_kind(b) == T_BLOCK);

   fail_unless(tree_params(b) == 3);
   fail_unless(tree_subkind(tree_param(b, 0)) == P_NAMED);
   fail_unless(tree_subkind(tree_param(b, 1)) == P_NAMED);
   fail_unless(tree_subkind(tree_param(b, 2)) == P_NAMED);

   fail_if_errors();
}
END_TEST

START_TEST(test_comp2)
{
   input_from_file(TESTDIR "/elab/comp2.vhd");

   tree_t e = run_elab();
   fail_if(e == NULL);

   tree_t top = tree_stmt(e, 0);
   fail_unless(tree_kind(top) == T_BLOCK);
   fail_unless(tree_stmts(top) == 2);

   tree_t sub1 = tree_stmt(top, 0);
   fail_unless(tree_kind(sub1) == T_BLOCK);
   fail_unless(tree_generics(sub1) == 1);
   fail_unless(tree_genmaps(sub1) == 1);

   tree_t sub1_x = tree_value(tree_genmap(sub1, 0));
   fail_unless(tree_kind(sub1_x) == T_LITERAL);
   fail_unless(tree_ival(sub1_x) == 4);

   tree_t sub1_y = tree_value(tree_decl(sub1, 1));
   fail_unless(tree_kind(sub1_y) == T_LITERAL);
   fail_unless(tree_ival(sub1_y) == 4);

   tree_t sub2 = tree_stmt(top, 1);
   fail_unless(tree_kind(sub2) == T_BLOCK);
   fail_unless(tree_generics(sub2) == 2);
   fail_unless(tree_genmaps(sub2) == 2);

   tree_t sub2_x = tree_value(tree_genmap(sub2, 0));
   fail_unless(tree_kind(sub2_x) == T_LITERAL);
   fail_unless(tree_ival(sub2_x) == 7);

   tree_t sub2_y = tree_value(tree_genmap(sub2, 1));
   fail_unless(tree_kind(sub2_y) == T_REF);
   fail_unless(tree_ident(sub2_y) == ident_new("FALSE"));

   fail_if_errors();
}
END_TEST

START_TEST(test_comp3)
{
   input_from_file(TESTDIR "/elab/comp3.vhd");

   const error_t expect[] = {
      { 36, "missing value for generic X with no default" },
      { 31, "type of generic X in component declaration SUB2 is BOOLEAN which"
        " does not match type INTEGER in entity WORK.SUB2" },
      { -1, NULL }
   };
   expect_errors(expect);

   tree_t e = run_elab();
   fail_unless(e == NULL);

   check_expected_errors();
}
END_TEST

START_TEST(test_tc3138)
{
   input_from_file(TESTDIR "/elab/tc3138.vhd");

   tree_t e = run_elab();
   fail_if(e == NULL);

   tree_t b0 = tree_stmt(e, 0);
   fail_unless(tree_ident(b0) == ident_new("C05S02B02X00P02N01I03138ENT"));
   fail_unless(tree_stmts(b0) == 2);
   fail_unless(tree_kind(tree_stmt(b0, 0)) == T_BLOCK);
   fail_unless(tree_kind(tree_stmt(b0, 1)) == T_PROCESS);

   fail_if_errors();
}
END_TEST

START_TEST(test_tc2881)
{
   input_from_file(TESTDIR "/elab/tc2881.vhd");

   tree_t e = run_elab();
   fail_if(e == NULL);

   tree_t b0 = tree_stmt(e, 0);
   fail_unless(tree_ident(b0) == ident_new("C02S01B00X00P07N01I02881ENT"));
   fail_unless(tree_stmts(b0) == 1);

   tree_t p0 = tree_stmt(b0, 0);
   fail_unless(tree_kind(p0) == T_PROCESS);

   // The assignment "x:= func2(3)" is globally static where func2 is
   // declared in the entity is globaally static and should really be
   // folded but isn't currently

   fail_if_errors();
}
END_TEST

START_TEST(test_tc846)
{
   input_from_file(TESTDIR "/elab/tc846.vhd");

   tree_t e = run_elab();
   fail_if(e == NULL);

   tree_t b0 = tree_stmt(e, 0);
   fail_unless(tree_ident(b0) == ident_new("C01S03B01X00P08N01I00846CFG"));
   fail_unless(tree_stmts(b0) == 2);

   tree_t a1 = tree_stmt(b0, 0);
   fail_unless(tree_ident(a1) == ident_new("A1"));
   fail_unless(tree_stmts(a1) == 1);

   tree_t c1 = tree_stmt(a1, 0);
   fail_unless(tree_ident(c1) == ident_new("C1"));
   fail_unless(tree_stmts(c1) == 0);
   fail_unless(tree_decls(c1) == 2);
   fail_unless(tree_ident(tree_decl(c1, 1)) == ident_new("X"));

   fail_if_errors();
}
END_TEST

START_TEST(test_issue435)
{
   input_from_file(TESTDIR "/elab/issue435.vhd");

   tree_t e = run_elab();
   fail_if(e == NULL);

   tree_t b0 = tree_stmt(e, 0);
   fail_unless(tree_ident(b0) == ident_new("ISSUE435"));
   fail_unless(tree_stmts(b0) == 1);

   tree_t q = tree_stmt(b0, 0);
   fail_unless(tree_ident(q) == ident_new("Q"));
   fail_unless(tree_stmts(q) == 1);

   tree_t ifgen = tree_stmt(q, 0);
   fail_unless(tree_ident(ifgen) == ident_new("QUEUE_SIZE_VALID"));
   fail_unless(tree_stmts(ifgen) == 0);

   fail_if_errors();
}
END_TEST

START_TEST(test_issue442)
{
   input_from_file(TESTDIR "/elab/issue442.vhd");

   tree_t e = run_elab();
   fail_if(e == NULL);

   tree_t b0 = tree_stmt(e, 0);
   fail_unless(tree_ident(b0) == ident_new("ISSUE442_1"));
   fail_unless(tree_stmts(b0) == 1);

   tree_t dut = tree_stmt(b0, 0);
   fail_unless(tree_ident(dut) == ident_new("DUT"));
   fail_unless(tree_kind(dut) == T_BLOCK);
   fail_unless(tree_stmts(dut) == 0);
   fail_unless(tree_genmaps(dut) == 2);

   tree_t m1 = tree_value(tree_genmap(dut, 1));
   fail_unless(tree_kind(m1) == T_LITERAL);
   fail_unless(tree_ival(m1) == 4);

   fail_if_errors();
}
END_TEST

START_TEST(test_issue448)
{
   input_from_file(TESTDIR "/elab/issue448.vhd");

   tree_t e = run_elab();
   fail_if(e == NULL);

   tree_t b0 = tree_stmt(e, 0);
   fail_unless(tree_ident(b0) == ident_new("ISSUE448"));
   fail_unless(tree_stmts(b0) == 1);

   tree_t tb = tree_stmt(b0, 0);
   fail_unless(tree_ident(tb) == ident_new("TB"));
   fail_unless(tree_kind(tb) == T_BLOCK);
   fail_unless(tree_stmts(tb) == 0);
   fail_unless(tree_genmaps(tb) == 6);

   tree_t m1 = tree_value(tree_genmap(tb, 2));
   fail_unless(tree_kind(m1) == T_LITERAL);
   fail_unless(tree_ival(m1) == 8192);

   fail_if_errors();
}
END_TEST

START_TEST(test_fold1)
{
   input_from_file(TESTDIR "/elab/fold1.vhd");

   tree_t e = run_elab();
   fail_if(e == NULL);

   tree_t b0 = tree_stmt(e, 0);
   fail_unless(tree_ident(b0) == ident_new("FOLD1"));
   fail_unless(tree_stmts(b0) == 1);

   tree_t u = tree_stmt(b0, 0);
   fail_unless(tree_ident(u) == ident_new("U"));
   fail_unless(tree_kind(u) == T_BLOCK);
   // This used to get folded by the old evaluator
   fail_unless(tree_stmts(u) == 1);
   fail_unless(tree_genmaps(u) == 1);
   fail_unless(tree_decls(u) == 2);

   tree_t k = tree_decl(u, 1);
   fail_unless(tree_ident(k) == ident_new("K"));
   fail_unless(tree_kind(tree_value(k)) == T_REF);
   // This used to get folded by the old evaluator

   fail_if_errors();
}
END_TEST

START_TEST(test_fold2)
{
   input_from_file(TESTDIR "/elab/fold2.vhd");

   tree_t e = run_elab();
   fail_if(e == NULL);

   tree_t b0 = tree_stmt(e, 0);
   fail_unless(tree_ident(b0) == ident_new("FOLD2"));
   fail_unless(tree_stmts(b0) == 1);

   tree_t u = tree_stmt(b0, 0);
   fail_unless(tree_ident(u) == ident_new("U"));
   fail_unless(tree_kind(u) == T_BLOCK);
   // This used to get folded by the old evaluator
   fail_unless(tree_stmts(u) == 1);
   fail_unless(tree_genmaps(u) == 1);
   fail_unless(tree_decls(u) == 2);

   tree_t k = tree_decl(u, 1);
   fail_unless(tree_ident(k) == ident_new("K"));
   fail_unless(tree_kind(tree_value(k)) == T_REF);
   // This used to get folded by the old evaluator

   fail_if_errors();
}
END_TEST

START_TEST(test_toplevel3)
{
   set_standard(STD_08);
   input_from_file(TESTDIR "/elab/toplevel3.vhd");

   const error_t expect[] = {
      { 15, "top-level port R2 cannot have unconstrained type REC" },
      { -1, NULL }
   };
   expect_errors(expect);

   tree_t e = run_elab();
   fail_unless(e == NULL);

   check_expected_errors();
}
END_TEST

START_TEST(test_ename1)
{
   set_standard(STD_08);
   input_from_file(TESTDIR "/elab/ename1.vhd");

   const error_t expect[] = {
      { 30, "class of object X is not variable" },
      { 31, "external name X not found" },
      { 33, "type of signal X is not BIT" },
      { 34, "external name X not found" },
      { 35, "external name X not found" },
      { 36, "relative pathname has no containing declarative region" },
      { -1, NULL }
   };
   expect_errors(expect);

   tree_t e = run_elab();
   fail_unless(e == NULL);

   check_expected_errors();
}
END_TEST

START_TEST(test_issue459)
{
   set_standard(STD_08);
   input_from_file(TESTDIR "/elab/issue459.vhd");

   tree_t e = run_elab();
   fail_if(e == NULL);

   tree_t b0 = tree_stmt(e, 0);
   fail_unless(tree_ident(b0) == ident_new("ISSUE459"));
   fail_unless(tree_stmts(b0) == 1);

   tree_t u = tree_stmt(b0, 0);
   fail_unless(tree_ident(u) == ident_new("U"));
   fail_unless(tree_kind(u) == T_BLOCK);
   fail_unless(tree_stmts(u) == 2);

   fail_if_errors();
}
END_TEST

START_TEST(test_link1)
{
   input_from_file(TESTDIR "/elab/link1.vhd");

   const error_t expect[] = {
      { 2, "division by zero" },
      { 7, "failed to initialise package WORK.PACK" },
      // TODO: this fatal error should be removed after lazy linking is added
      { 24, "failed to link package WORK.PACK" },
      { -1, NULL }
   };
   expect_errors(expect);

   tree_t e = run_elab();
   fail_unless(e == NULL);

   check_expected_errors();
}
END_TEST

START_TEST(test_generate1)
{
   set_standard(STD_08);
   input_from_file(TESTDIR "/elab/generate1.vhd");

   tree_t e = run_elab();
   fail_if(e == NULL);

   tree_t b0 = tree_stmt(e, 0);
   fail_unless(tree_ident(b0) == ident_new("GENERATE1"));
   fail_unless(tree_stmts(b0) == 3);

   tree_t g1 = tree_stmt(b0, 0);
   fail_unless(tree_ident(g1) == ident_new("G(1)"));
   fail_unless(tree_kind(g1) == T_BLOCK);
   fail_unless(tree_stmts(g1) == 1);

   fail_if_errors();
}
END_TEST

START_TEST(test_neorv1)
{
   input_from_file(TESTDIR "/elab/neorv1.vhd");

   tree_t e = run_elab();
   fail_if(e == NULL);

   tree_t b0 = tree_stmt(e, 0);
   fail_unless(tree_ident(b0) == ident_new("NEORV1"));
   fail_unless(tree_stmts(b0) == 2);

   tree_t g0 = tree_stmt(b0, 0);
   fail_unless(tree_ident(g0) == ident_new("G(0)"));
   fail_unless(tree_kind(g0) == T_BLOCK);
   fail_unless(tree_stmts(g0) == 1);

   tree_t u = tree_stmt(g0, 0);
   fail_unless(tree_kind(u) == T_BLOCK);
   tree_t p0 = tree_port(u, 0);
   tree_t m0 = tree_param(u, 0);
   fail_unless(tree_subkind(m0) == P_NAMED);
   tree_t n = tree_name(m0);
   fail_unless(tree_kind(n) == T_ARRAY_REF);
   fail_unless(tree_ref(tree_value(n)) == p0);

   fail_if_errors();
}
END_TEST

START_TEST(test_toplevel4)
{
   elab_set_generic("CONFIG", "hello");
   elab_set_generic("R", "2.5");
   elab_set_generic("T", "5000 us");

   input_from_file(TESTDIR "/elab/toplevel4.vhd");

   tree_t e = run_elab();
   fail_if(e == NULL);

   tree_t b0 = tree_stmt(e, 0);
   fail_unless(tree_ident(b0) == ident_new("TOPLEVEL4"));
   fail_unless(tree_stmts(b0) == 0);

   tree_t m0 = tree_value(tree_genmap(b0, 0));
   fail_unless(tree_kind(m0) == T_STRING);
   fail_unless(type_kind(tree_type(m0)) == T_SUBTYPE);

   fail_if_errors();
}
END_TEST

START_TEST(test_comp4)
{
   set_standard(STD_08);

   input_from_file(TESTDIR "/elab/comp4.vhd");

   tree_t e = run_elab();
   fail_if(e == NULL);

   tree_t b0 = tree_stmt(e, 0);
   fail_unless(tree_ident(b0) == ident_new("COMP4"));
   fail_unless(tree_stmts(b0) == 1);

   tree_t u = tree_stmt(b0, 0);
   fail_unless(tree_ident(u) == ident_new("U"));
   fail_unless(tree_stmts(u) == 1);

   fail_if_errors();
}
END_TEST

START_TEST(test_issue514)
{
   input_from_file(TESTDIR "/elab/issue514.vhd");

   tree_t e = run_elab();
   fail_if(e == NULL);

   tree_t b0 = tree_stmt(e, 0);
   fail_unless(tree_ident(b0) == ident_new("ISSUE514"));
   fail_unless(tree_stmts(b0) == 1);

   tree_t u = tree_stmt(b0, 0);
   fail_unless(tree_ident(u) == ident_new("U"));
   fail_unless(tree_stmts(u) == 3);

   fail_if_errors();
}
END_TEST

START_TEST(test_issue518)
{
   input_from_file(TESTDIR "/elab/issue518.vhd");

   tree_t e = run_elab();
   fail_if(e == NULL);

   fail_if_errors();
}
END_TEST

START_TEST(test_config1)
{
   input_from_file(TESTDIR "/elab/config1.vhd");

   tree_t e = run_elab();
   fail_if(e == NULL);

   tree_t top = tree_stmt(e, 0);
   fail_unless(tree_stmts(top) == 2);

   tree_t u1 = tree_stmt(top, 0);
   fail_unless(tree_ports(u1) == 0);
   fail_unless(tree_params(u1) == 0);

   tree_t u2 = tree_stmt(top, 1);
   fail_unless(tree_ports(u2) == 1);
   fail_unless(tree_params(u2) == 1);

   fail_if_errors();
}
END_TEST

START_TEST(test_ename2)
{
   set_standard(STD_08);

   input_from_file(TESTDIR "/elab/ename2.vhd");

   const error_t expect[] = {
      { 22, "external name K not found" },
      { -1, NULL }
   };
   expect_errors(expect);

   tree_t e = run_elab();
   fail_unless(e == NULL);

   check_expected_errors();
}
END_TEST

START_TEST(test_assert7)
{
   input_from_file(TESTDIR "/elab/assert7.vhd");

   const error_t expect[] = {
      {  8, "Assertion violation." },
      { -1, NULL }
   };
   expect_errors(expect);

   tree_t e = run_elab();
   fail_unless(e == NULL);

   check_expected_errors();
}
END_TEST

START_TEST(test_issue539)
{
   set_standard(STD_08);

   input_from_file(TESTDIR "/elab/issue539.vhd");

   const error_t expect[] = {
      { 23, "value length 2 does not match signal S length 1" },
      { -1, NULL }
   };
   expect_errors(expect);

   tree_t e = run_elab();
   fail_unless(e == NULL);

   check_expected_errors();
}
END_TEST

START_TEST(test_bounds14)
{
   input_from_file(TESTDIR "/elab/bounds14.vhd");

   const error_t expect[] = {
      { 11, "value length 12 does not match signal X length 8" },
      { -1, NULL }
   };
   expect_errors(expect);

   tree_t e = run_elab();
   fail_unless(e == NULL);

   check_expected_errors();
}
END_TEST

START_TEST(test_bounds21)
{
   input_from_file(TESTDIR "/elab/bounds21.vhd");

   const error_t expect[] = {
      { 18, "actual length 8 does not match generic INFO length 7" },
      { -1, NULL }
   };
   expect_errors(expect);

   tree_t e = run_elab();
   fail_unless(e == NULL);

   check_expected_errors();
}
END_TEST

START_TEST(test_issue228)
{
   elab_set_generic("G", "10");

   input_from_file(TESTDIR "/elab/issue228.vhd");

   const error_t expect[] = {
      {  2, "value 10 outside of INTEGER range 0 to 3 for generic G" },
      { -1, NULL }
   };
   expect_errors(expect);

   tree_t e = run_elab();
   fail_unless(e == NULL);

   check_expected_errors();
}
END_TEST

START_TEST(test_generic1)
{
   input_from_file(TESTDIR "/elab/generic1.vhd");

   tree_t e = run_elab();
   fail_if(e == NULL);

   tree_t u = tree_stmt(tree_stmt(e, 0), 0);
   fail_unless(tree_stmts(u) == 2);

   tree_t p1s0 = tree_stmt(tree_stmt(u, 0), 0);
   fail_unless(tree_kind(p1s0) == T_SIGNAL_ASSIGN);

   tree_t one = tree_value(tree_waveform(p1s0, 0));
   fail_unless(tree_kind(one) == T_LITERAL);
   fail_unless(tree_ival(one) == 1);

   tree_t p2s0 = tree_stmt(tree_stmt(u, 1), 0);
   fail_unless(tree_kind(p2s0) == T_ASSERT);

   tree_t m = tree_message(p2s0);
   fail_unless(tree_kind(m) == T_REF);
   fail_unless(tree_ident(m) == ident_new("M"));
   fail_unless(tree_kind(tree_ref(m)) == T_GENERIC_DECL);

   tree_t sev = tree_severity(p2s0);
   fail_unless(tree_kind(sev) == T_REF);
   fail_unless(tree_ident(sev) == ident_new("ERROR"));
   fail_unless(tree_kind(tree_ref(sev)) == T_ENUM_LIT);

   fail_if_errors();
}
END_TEST

START_TEST(test_generic2)
{
   input_from_file(TESTDIR "/elab/generic2.vhd");

   tree_t e = run_elab();
   fail_if(e == NULL);

   tree_t u = tree_stmt(tree_stmt(e, 0), 0);
   fail_unless(tree_stmts(u) == 2);

   tree_t p1s0 = tree_stmt(tree_stmt(u, 0), 0);
   fail_unless(tree_kind(p1s0) == T_SIGNAL_ASSIGN);

   tree_t two = tree_value(tree_waveform(p1s0, 0));
   fail_unless(tree_kind(two) == T_LITERAL);
   fail_unless(tree_ival(two) == 2);

   tree_t p2s0 = tree_stmt(tree_stmt(u, 1), 0);
   fail_unless(tree_kind(p2s0) == T_ASSERT);

   tree_t value = tree_value(p2s0);
   fail_unless(tree_kind(value) == T_REF);
   fail_unless(tree_ident(value) == ident_new("FALSE"));
   fail_unless(tree_kind(tree_ref(value)) == T_ENUM_LIT);

   fail_if_errors();
}
END_TEST

START_TEST(test_issue669)
{
   input_from_file(TESTDIR "/elab/issue669.vhd");

   const error_t expect[] = {
      { -1, NULL }
   };
   expect_errors(expect);

   tree_t e = run_elab();
   fail_if(e == NULL);

   tree_t f = tree_stmt(tree_stmt(e, 0), 0);
   fail_unless(tree_kind(f) == T_BLOCK);
   fail_unless(tree_ident(f) == ident_new("F"));

   tree_t p = tree_stmt(f, 0);
   fail_unless(tree_kind(p) == T_PROCESS);

   tree_t w = tree_stmt(p, 1);
   fail_unless(tree_kind(w) == T_WAIT);
   ck_assert_int_eq(tree_ival(tree_delay(w)), INT64_C(-1000000000000));

   fail_if_errors();
}
END_TEST

START_TEST(test_mixed1)
{
#ifdef ENABLE_VERILOG
   analyse_file(TESTDIR "/elab/mixed1.v", NULL, NULL);

   input_from_file(TESTDIR "/elab/mixed1.vhd");

   const error_t expect[] = {
      { 22, "missing matching VHDL port declaration for Verilog port two "
        "in component MOD1" },
      { 34, "port FOUR not found in Verilog module mod1" },
      { 44, "Verilog module ports must have type STD_LOGIC or STD_LOGIC_VEC" },
      { -1, NULL }
   };
   expect_errors(expect);

   tree_t e = run_elab();
   fail_unless(e == NULL);

   check_expected_errors();
#endif
}
END_TEST

START_TEST(test_genpack1)
{
   set_standard(STD_08);

   input_from_file(TESTDIR "/elab/genpack1.vhd");

   const error_t expect[] = {
      {  8, "Assertion violation." },
      { -1, NULL }
   };
   expect_errors(expect);

   tree_t e = run_elab();
   fail_unless(e == NULL);

   check_expected_errors();
}
END_TEST

START_TEST(test_genpack2)
{
   set_standard(STD_08);

   input_from_file(TESTDIR "/elab/genpack2.vhd");

   const error_t expect[] = {
      { 14, "Assertion violation." },
      { -1, NULL }
   };
   expect_errors(expect);

   tree_t e = run_elab();
   fail_unless(e == NULL);

   check_expected_errors();
}
END_TEST

START_TEST(test_genpack3)
{
   set_standard(STD_08);

   input_from_file(TESTDIR "/elab/genpack3.vhd");

   const error_t expect[] = {
      {  8, "Assertion violation." },
      { -1, NULL }
   };
   expect_errors(expect);

   tree_t e = run_elab();
   fail_unless(e == NULL);

   check_expected_errors();
}
END_TEST

START_TEST(test_genpack4)
{
   set_standard(STD_08);

   input_from_file(TESTDIR "/elab/genpack4.vhd");

   const error_t expect[] = {
      { 19, "Assertion violation." },
      { -1, NULL }
   };
   expect_errors(expect);

   tree_t e = run_elab();
   fail_unless(e == NULL);

   check_expected_errors();
}
END_TEST

START_TEST(test_view1)
{
   set_standard(STD_19);

   input_from_file(TESTDIR "/elab/view1.vhd");

   const error_t expect[] = {
      { 21, "value length 3 does not match view IN_VIEW length 2" },
      { -1, NULL }
   };
   expect_errors(expect);

   tree_t e = run_elab();
   fail_unless(e == NULL);

   check_expected_errors();
}
END_TEST

START_TEST(test_issue707)
{
   set_standard(STD_08);

   input_from_file(TESTDIR "/elab/issue707.vhd");

   tree_t e = run_elab();
   fail_if(e == NULL);

   fail_if_errors();
}
END_TEST

START_TEST(test_bounds40)
{
   set_standard(STD_08);

   input_from_file(TESTDIR "/elab/bounds40.vhd");

   const error_t expect[] = {
      { 12, "actual length 3 does not match port P length 8" },
      { -1, NULL }
   };
   expect_errors(expect);

   tree_t e = run_elab();
   fail_unless(e == NULL);

   check_expected_errors();
}
END_TEST

START_TEST(test_bounds41)
{
   set_standard(STD_08);

   input_from_file(TESTDIR "/elab/bounds41.vhd");

   const error_t expect[] = {
      { 38, "actual length 1 does not match formal length 2" },
      { -1, NULL }
   };
   expect_errors(expect);

   tree_t e = run_elab();
   fail_unless(e == NULL);

   check_expected_errors();
}
END_TEST

START_TEST(test_body1)
{
   input_from_file(TESTDIR "/elab/body1.vhd");

   const error_t expect[] = {
      {  1, "missing body for package WORK.PACK" },
      { -1, NULL }
   };
   expect_errors(expect);

   tree_t e = run_elab();
   fail_unless(e == NULL);

   check_expected_errors();
}
END_TEST

START_TEST(test_issue759)
{
   set_standard(STD_08);

   input_from_file(TESTDIR "/elab/issue759.vhd");

   tree_t a = parse_check_and_simplify(T_PACKAGE, T_PACK_BODY,
                                       T_ENTITY, T_ARCH);

   unit_registry_t *ur = get_registry();
   jit_t *jit = jit_new(ur);
   cover_data_t *cover = cover_data_init(COVER_MASK_ALL, 0);

   tree_t e = elab(a, jit, ur, cover);
   fail_if(e == NULL);

   jit_free(jit);

   fail_if_errors();
}
END_TEST

START_TEST(test_block2)
{
   set_standard(_i);

   input_from_file(TESTDIR "/elab/block2.vhd");

   run_elab();

   fail_if_errors();
}
END_TEST

START_TEST(test_bounds42)
{
   input_from_file(TESTDIR "/elab/bounds42.vhd");

   const error_t expect[] = {
      { 27, "actual length 5 does not match formal length 2" },
      { -1, NULL }
   };
   expect_errors(expect);

   tree_t e = run_elab();
   fail_unless(e == NULL);

   check_expected_errors();
}
END_TEST

Suite *get_elab_tests(void)
{
   Suite *s = suite_create("elab");

   TCase *tc = nvc_unit_test();
   tcase_add_test(tc, test_elab1);
   tcase_add_test(tc, test_elab2);
   tcase_add_test(tc, test_elab3);
   tcase_add_test(tc, test_elab4);
   tcase_add_test(tc, test_open);
   tcase_add_test(tc, test_genagg);
   tcase_add_test(tc, test_comp);
   tcase_add_test(tc, test_issue17);
   tcase_add_test(tc, test_issue19);
   tcase_add_test(tc, test_bounds10);
   tcase_add_test(tc, test_copy1);
   tcase_add_test(tc, test_record);
   tcase_add_test(tc, test_ifgen);
   tcase_add_test(tc, test_open2);
   tcase_add_test(tc, test_issue93);
   tcase_add_test(tc, test_const1);
   tcase_add_test(tc, test_libbind);
   tcase_add_test(tc, test_issue153);
   tcase_add_test(tc, test_issue157);
   tcase_add_test(tc, test_issue159);
   tcase_add_test(tc, test_issue175);
   tcase_add_test(tc, test_issue184);
   tcase_add_test(tc, test_libbind2);
   tcase_add_test(tc, test_toplevel2);
   tcase_add_test(tc, test_libbind3);
   tcase_add_test(tc, test_issue251);
   tcase_add_test(tc, test_jcore1);
   tcase_add_test(tc, test_eval1);
   tcase_add_test(tc, test_issue305);
   tcase_add_test(tc, test_gbounds);
   tcase_add_test(tc, test_issue307);
   tcase_add_test(tc, test_issue315);
   tcase_add_test(tc, test_issue325);
   tcase_add_test(tc, test_issue328);
   tcase_add_test(tc, test_issue330);
   tcase_add_test(tc, test_issue336);
   tcase_add_test(tc, test_openinout);
   tcase_add_test(tc, test_opencase);
   tcase_add_test(tc, test_issue232);
   tcase_add_test(tc, test_issue373);
   tcase_add_test(tc, test_issue374);
   tcase_add_test(tc, test_issue404);
   tcase_add_test(tc, test_block1);
   tcase_add_test(tc, test_open3);
   tcase_add_test(tc, test_comp2);
   tcase_add_test(tc, test_comp3);
   tcase_add_test(tc, test_tc3138);
   tcase_add_test(tc, test_tc2881);
   tcase_add_test(tc, test_tc846);
   tcase_add_test(tc, test_issue435);
   tcase_add_test(tc, test_issue442);
   tcase_add_test(tc, test_issue448);
   tcase_add_test(tc, test_fold1);
   tcase_add_test(tc, test_fold2);
   tcase_add_test(tc, test_toplevel3);
   tcase_add_test(tc, test_ename1);
   tcase_add_test(tc, test_issue459);
   tcase_add_exit_test(tc, test_link1, 1);
   tcase_add_test(tc, test_generate1);
   tcase_add_test(tc, test_neorv1);
   tcase_add_test(tc, test_toplevel4);
   tcase_add_test(tc, test_comp4);
   tcase_add_test(tc, test_issue514);
   tcase_add_test(tc, test_issue518);
   tcase_add_test(tc, test_config1);
   tcase_add_test(tc, test_ename2);
   tcase_add_test(tc, test_assert7);
   tcase_add_test(tc, test_issue539);
   tcase_add_test(tc, test_bounds14);
   tcase_add_test(tc, test_bounds21);
   tcase_add_test(tc, test_issue228);
   tcase_add_test(tc, test_generic1);
   tcase_add_test(tc, test_generic2);
   tcase_add_test(tc, test_issue669);
   tcase_add_test(tc, test_mixed1);
   tcase_add_test(tc, test_genpack1);
   tcase_add_test(tc, test_genpack2);
   tcase_add_test(tc, test_genpack3);
   tcase_add_test(tc, test_genpack4);
   tcase_add_test(tc, test_view1);
   tcase_add_test(tc, test_issue707);
   tcase_add_test(tc, test_bounds40);
   tcase_add_test(tc, test_bounds41);
   tcase_add_test(tc, test_body1);
   tcase_add_test(tc, test_issue759);
   tcase_add_test(tc, test_bounds42);
   tcase_add_loop_test(tc, test_block2, STD_02, STD_19 + 1);
   suite_add_tcase(s, tc);

   return s;
}
