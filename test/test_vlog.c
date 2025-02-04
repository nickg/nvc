//
//  Copyright (C) 2022-2024 Nick Gasson
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

#include "phase.h"
#include "common.h"
#include "ident.h"
#include "lib.h"
#include "object.h"
#include "prim.h"
#include "scan.h"
#include "test_util.h"
#include "vlog/vlog-node.h"
#include "vlog/vlog-number.h"
#include "vlog/vlog-phase.h"

START_TEST(test_dff)
{
   input_from_file(TESTDIR "/vlog/dff.v");

   vlog_node_t m = vlog_parse();
   fail_if(m == NULL);
   fail_unless(vlog_kind(m) == V_MODULE);
   fail_unless(vlog_stmts(m) == 1);
   fail_unless(vlog_ports(m) == 4);
   fail_unless(vlog_decls(m) == 5);

   vlog_node_t p0 = vlog_port(m, 0);
   fail_unless(vlog_kind(p0) == V_REF);
   fail_unless(vlog_ident(p0) == ident_new("d"));

   vlog_node_t p1 = vlog_port(m, 1);
   fail_unless(vlog_kind(p1) == V_REF);
   fail_unless(vlog_ident(p1) == ident_new("clk"));

   vlog_node_t p3 = vlog_port(m, 3);
   fail_unless(vlog_kind(p3) == V_REF);
   fail_unless(vlog_ident(p3) == ident_new("q"));

   vlog_node_t d0 = vlog_decl(m, 0);
   fail_unless(vlog_kind(d0) == V_PORT_DECL);
   fail_unless(vlog_subkind(d0) == V_PORT_INPUT);
   fail_unless(vlog_ident(d0) == ident_new("d"));
   fail_unless(vlog_ident2(d0) == ident_new("D"));

   vlog_node_t d1 = vlog_decl(m, 1);
   fail_unless(vlog_kind(d1) == V_PORT_DECL);
   fail_unless(vlog_subkind(d1) == V_PORT_INPUT);
   fail_unless(vlog_ident(d1) == ident_new("clk"));
   fail_unless(vlog_ident2(d1) == ident_new("CLK"));

   vlog_node_t d3 = vlog_decl(m, 3);
   fail_unless(vlog_kind(d3) == V_PORT_DECL);
   fail_unless(vlog_subkind(d3) == V_PORT_OUTPUT);
   fail_unless(vlog_ident(d3) == ident_new("q"));
   fail_unless(vlog_ident2(d3) == ident_new("Q"));

   vlog_node_t d4 = vlog_decl(m, 4);
   fail_unless(vlog_kind(d4) == V_VAR_DECL);
   fail_unless(vlog_ident(d4) == ident_new("q"));

   vlog_node_t a = vlog_stmt(m, 0);
   fail_unless(vlog_kind(a) == V_ALWAYS);
   fail_unless(vlog_stmts(a) == 1);

   vlog_node_t e = vlog_stmt(a, 0);
   fail_unless(vlog_kind(e) == V_TIMING);
   fail_unless(vlog_stmts(e) == 1);

   vlog_node_t ctrl = vlog_value(e);
   fail_unless(vlog_kind(ctrl) == V_EVENT_CONTROL);
   fail_unless(vlog_params(ctrl) == 1);

   vlog_node_t v = vlog_param(ctrl, 0);
   fail_unless(vlog_kind(v) == V_EVENT);
   fail_unless(vlog_subkind(v) == V_EVENT_POSEDGE);

   vlog_node_t clk = vlog_value(v);
   fail_unless(vlog_kind(clk) == V_REF);
   fail_unless(vlog_ident(clk) == ident_new("clk"));

   vlog_node_t s = vlog_stmt(e, 0);
   fail_unless(vlog_kind(s) == V_NBASSIGN);

   vlog_node_t t = vlog_target(s);
   fail_unless(vlog_kind(t) == V_REF);
   fail_unless(vlog_ident(t) == ident_new("q"));

   fail_unless(vlog_parse() == NULL);

   fail_if_errors();
}
END_TEST

START_TEST(test_simple_sem)
{
   const error_t expect[] = {
      {  7, "duplicate declaration of x" },
      { 13, "no visible declaration for qq" },
      { 13, "no visible declaration for dd" },
      { 19, "'r' cannot be driven by continuous assignment" },
      { 20, "'q' cannot be assigned in a procedural block" },
      { -1, NULL }
   };
   expect_errors(expect);

   input_from_file(TESTDIR "/vlog/simple_sem.v");

   for (int i = 0; i < 4; i++) {
      vlog_node_t m = vlog_parse();
      fail_if(m == NULL);
      fail_unless(vlog_kind(m) == V_MODULE);

      vlog_check(m);
   }

   fail_unless(vlog_parse() == NULL);

   check_expected_errors();
}
END_TEST

START_TEST(test_ports)
{
   const error_t expect[] = {
      {  4, "duplicate declaration of y" },
      { 19, "duplicate declaration of x" },
      { 22, "duplicate declaration of y" },
      { 31, "'o3' cannot be assigned in a procedural block" },
      { 43, "inconsistent dimensions for 'y'" },
      { 44, "cannot reference net 'x' in constant expression" },
      { -1, NULL }
   };
   expect_errors(expect);

   input_from_file(TESTDIR "/vlog/ports.v");

   for (int i = 0; i < 7; i++) {
      vlog_node_t m = vlog_parse();
      fail_if(m == NULL);
      fail_unless(vlog_kind(m) == V_MODULE);

      vlog_check(m);
   }

   fail_unless(vlog_parse() == NULL);

   check_expected_errors();
}
END_TEST

START_TEST(test_parse1)
{
   input_from_file(TESTDIR "/vlog/parse1.v");

   vlog_node_t m = vlog_parse();
   fail_if(m == NULL);
   fail_unless(vlog_kind(m) == V_MODULE);
   fail_unless(vlog_stmts(m) == 15);
   fail_unless(vlog_ports(m) == 0);
   fail_unless(vlog_decls(m) == 14);

   vlog_node_t x = vlog_decl(m, 0);
   fail_unless(vlog_kind(x) == V_NET_DECL);
   fail_unless(vlog_ranges(x) == 0);
   fail_unless(vlog_subkind(x) == V_NET_WIRE);

   vlog_node_t xdt = vlog_type(x);
   fail_unless(vlog_kind(xdt) == V_DATA_TYPE);
   fail_unless(vlog_subkind(xdt) == DT_LOGIC);
   fail_unless(vlog_ranges(xdt) == 1);

   vlog_node_t xd0 = vlog_range(xdt, 0);
   fail_unless(vlog_kind(xd0) == V_DIMENSION);
   fail_unless(vlog_subkind(xd0) == V_DIM_PACKED);

   vlog_node_t s1 = vlog_stmt(m, 1);
   fail_unless(vlog_kind(s1) == V_ALWAYS);
   fail_unless(vlog_stmts(s1) == 1);

   vlog_node_t s1s0 = vlog_stmt(s1, 0);
   fail_unless(vlog_kind(s1s0) == V_SEQ_BLOCK);
   fail_unless(vlog_ident(s1s0) == ident_new("foo"));

   vlog_node_t s1s0s0 = vlog_stmt(s1s0, 0);
   fail_unless(vlog_kind(s1s0s0) == V_SYS_TCALL);
   fail_unless(vlog_ident(s1s0s0) == ident_new("$display"));
   fail_unless(vlog_params(s1s0s0) == 1);

   vlog_node_t hello = vlog_param(s1s0s0, 0);
   fail_unless(vlog_kind(hello) == V_STRING);
   ck_assert_str_eq(vlog_text(hello), "hello");

   vlog_node_t s1s0s1 = vlog_stmt(s1s0, 1);
   fail_unless(vlog_kind(s1s0s1) == V_SYS_TCALL);
   fail_unless(vlog_ident(s1s0s1) == ident_new("$finish"));

   vlog_node_t s1s0s2 = vlog_stmt(s1s0, 2);
   fail_unless(vlog_kind(s1s0s2) == V_IF);
   fail_unless(vlog_conds(s1s0s2) == 1);

   vlog_node_t s1s0s3 = vlog_stmt(s1s0, 3);
   fail_unless(vlog_kind(s1s0s3) == V_IF);
   fail_unless(vlog_conds(s1s0s3) == 1);

   vlog_node_t s1s0s4 = vlog_stmt(s1s0, 4);
   fail_unless(vlog_kind(s1s0s4) == V_IF);
   fail_unless(vlog_conds(s1s0s4) == 3);

   vlog_node_t s2 = vlog_stmt(m, 2);
   fail_unless(vlog_kind(s2) == V_ASSIGN);

   vlog_node_t s2or = vlog_value(s2);
   fail_unless(vlog_kind(s2or) == V_BINARY);
   fail_unless(vlog_subkind(s2or) == V_BINARY_OR);

   vlog_node_t s1s0s5 = vlog_stmt(s1s0, 5);
   fail_unless(vlog_kind(s1s0s5) == V_BASSIGN);

   vlog_node_t s3 = vlog_stmt(m, 3);
   fail_unless(vlog_kind(s3) == V_GATE_INST);
   ck_assert_int_eq(vlog_subkind(s3), V_GATE_PULLDOWN);
   ck_assert_int_eq(vlog_params(s3), 1);
   fail_unless(vlog_ident(s3) == ident_new("p1"));
   fail_unless(vlog_kind(vlog_target(s3)) == V_REF);

   vlog_node_t s4 = vlog_stmt(m, 4);
   fail_unless(vlog_kind(s4) == V_GATE_INST);
   ck_assert_int_eq(vlog_subkind(s4), V_GATE_PULLDOWN);
   ck_assert_int_eq(vlog_params(s4), 1);
   fail_unless(vlog_ident(s4) == ident_new("p2"));
   fail_unless(vlog_kind(vlog_target(s4)) == V_REF);

   vlog_node_t s5 = vlog_stmt(m, 5);
   fail_unless(vlog_kind(s5) == V_GATE_INST);
   ck_assert_int_eq(vlog_subkind(s5), V_GATE_PULLUP);
   ck_assert_int_eq(vlog_params(s5), 1);
   fail_unless(vlog_ident(s5) == ident_new("p3"));
   fail_unless(vlog_kind(vlog_target(s5)) == V_REF);

   vlog_node_t s6 = vlog_stmt(m, 6);
   fail_unless(vlog_kind(s6) == V_GATE_INST);
   ck_assert_int_eq(vlog_subkind(s6), V_GATE_PULLUP);
   ck_assert_int_eq(vlog_params(s6), 1);
   fail_unless(vlog_ident(s6) == ident_new("p4"));
   fail_unless(vlog_kind(vlog_target(s6)) == V_REF);

   vlog_node_t s7 = vlog_stmt(m, 7);
   fail_unless(vlog_kind(s7) == V_GATE_INST);
   ck_assert_int_eq(vlog_subkind(s7), V_GATE_PULLUP);
   ck_assert_int_eq(vlog_params(s7), 1);
   fail_if(vlog_has_ident(s7));
   fail_unless(vlog_kind(vlog_target(s7)) == V_REF);

   vlog_node_t s8 = vlog_stmt(m, 8);
   fail_unless(vlog_kind(s8) == V_ALWAYS);

   vlog_node_t s8ctrl = vlog_value(vlog_stmt(s8, 0));
   fail_unless(vlog_kind(s8ctrl) == V_EVENT_CONTROL);
   fail_unless(vlog_params(s8ctrl) == 3);

   vlog_node_t s9 = vlog_stmt(m, 9);
   fail_unless(vlog_kind(s9) == V_INITIAL);
   vlog_node_t s9b = vlog_stmt(s9, 0);
   fail_unless(vlog_kind(s9b) == V_SEQ_BLOCK);
   fail_unless(vlog_kind(vlog_stmt(s9b, 0)) == V_WHILE);
   fail_unless(vlog_kind(vlog_stmt(s9b, 1)) == V_REPEAT);
   fail_unless(vlog_kind(vlog_stmt(s9b, 2)) == V_DO_WHILE);

   fail_unless(vlog_parse() == NULL);

   fail_if_errors();
}
END_TEST

START_TEST(test_number1)
{
   number_t x = number_new("1'b1");
   ck_assert_int_eq(x.common.tag, TAG_INTEGER);
   fail_unless(number_is_defined(x));
   ck_assert_int_eq(number_width(x), 1);
   ck_assert_int_eq(number_integer(x), 1);
   number_free(&x);

   number_t y = number_new("5'b101");
   ck_assert_int_eq(y.common.tag, TAG_INTEGER);
   fail_unless(number_is_defined(y));
   ck_assert_int_eq(number_width(y), 5);
   ck_assert_int_eq(number_integer(y), 5);
   number_free(&y);

   number_t z = number_new("5'b1xx");
   ck_assert_int_eq(z.common.tag, TAG_SMALLNUM);
   fail_if(number_is_defined(z));
   ck_assert_int_eq(number_width(z), 5);
   number_free(&z);

   number_t n1 = number_new("1'bx");
   ck_assert_int_eq(n1.common.tag, TAG_SMALLNUM);
   fail_if(number_is_defined(n1));
   ck_assert_int_eq(number_width(n1), 1);
   number_free(&n1);
}
END_TEST

START_TEST(test_number2)
{
   struct {
      const char *input;
      unsigned    tag;
      unsigned    width;
      int64_t     ival;
      const char *string;
   } cases[] = {
      { "1'b1",          TAG_INTEGER, 1,   1,           "1'b1"       },
      { "5'b100",        TAG_INTEGER, 5,   4,           "5'd4"       },
      { "1",             TAG_INTEGER, 32,  1,           "1"          },
      { "42",            TAG_INTEGER, 32,  42,          "42"         },
      { "251251",        TAG_INTEGER, 32,  251251,      "251251"     },
      { "'hffffffff",    TAG_INTEGER, 32,  0xffffffff,  "4294967295" },
      { "33'h100000000", TAG_BIGNUM,  33,  0x100000000,
        "33'b100000000000000000000000000000000" },
      { "64'b0",         TAG_BIGNUM,  64,  0,           "64'b0"      },
      { "64'b101",       TAG_BIGNUM,  64,  5,           "64'b101"    },
      { "8'b00_01",      TAG_INTEGER, 8,   1,           "8'd1"       },
      { "128'bx",        TAG_BIGNUM,  128, 0,           "128'bx"     },
   };

   LOCAL_TEXT_BUF tb = tb_new();

   for (int i = 0; i < ARRAY_LEN(cases); i++) {
      number_t n = number_new(cases[i].input);
      ck_assert_int_eq(n.common.tag, cases[i].tag);
      ck_assert_int_eq(number_width(n), cases[i].width);

      if (cases[i].width <= 64)
         ck_assert_int_eq(number_integer(n), cases[i].ival);

      tb_rewind(tb);
      number_print(n, tb);
      ck_assert_str_eq(tb_get(tb), cases[i].string);

      number_free(&n);
   }
}
END_TEST

START_TEST(test_pp1)
{
   input_from_file(TESTDIR "/vlog/pp1.v");

   LOCAL_TEXT_BUF tb = tb_new();
   vlog_preprocess(tb, false);

   ck_assert_str_eq(
      tb_get(tb),
      "\n"
      "\n"
      " // comment\n"
      "bar = 1\n"
      "   // Another comment\n"
      "pass\n"
      "\n"
      "\n"
      "\n"
      "\n"
      "\n"
      "\n"
      "pass\n"
      "\n"
      "\n");
}
END_TEST

START_TEST(test_empty1)
{
   input_from_buffer("", 0, SOURCE_VERILOG);

   fail_unless(vlog_parse() == NULL);

   freeze_global_arena();   // Should not crash

   fail_if_errors();
}
END_TEST

START_TEST(test_timescale1)
{
   input_from_file(TESTDIR "/vlog/timescale1.v");

   const error_t expect[] = {
      {  2, "invalid time unit name 'hello'" },
      {  3, "invalid order of magnitude in `timescale directive" },
      { -1, NULL }
   };
   expect_errors(expect);

   vlog_node_t m = vlog_parse();
   fail_if(m == NULL);
   fail_unless(vlog_kind(m) == V_MODULE);

   vlog_check(m);

   fail_unless(vlog_parse() == NULL);

   check_expected_errors();
}
END_TEST

START_TEST(test_defaultnettype)
{
   input_from_file(TESTDIR "/vlog/defaultnettype.v");

   const error_t expect[] = {
      { 12, "unexpected identifier while parsing default_nettype directive, expecting one of wire, tri, tri0, tri1, wand, triand, wor, trior," },
      { -1, NULL }
   };
   expect_errors(expect);

   vlog_node_t m = vlog_parse();
   fail_if(m == NULL);
   fail_unless(vlog_kind(m) == V_MODULE);

   vlog_check(m);

   fail_unless(vlog_parse() == NULL);

   check_expected_errors();
}
END_TEST

START_TEST(test_gate1)
{
   input_from_file(TESTDIR "/vlog/gate1.v");

   const error_t expect[] = {
      {  6, "duplicate declaration of p1" },
      { -1, NULL }
   };
   expect_errors(expect);

   vlog_node_t m = vlog_parse();
   fail_if(m == NULL);
   fail_unless(vlog_kind(m) == V_MODULE);

   vlog_check(m);

   fail_unless(vlog_parse() == NULL);

   check_expected_errors();
}
END_TEST

START_TEST(test_pp2)
{
   input_from_file(TESTDIR "/vlog/pp2.v");

   const error_t expect[] = {
      {  1, "`else outside of `ifdef" },
      {  2, "`endif outside of `ifdef" },
      {  3, "expected macro name after `ifdef" },
      {  4, "expected macro name after `ifndef" },
      {  5, "expected macro name after `define" },
      {  6, "no corresponding `endif before end of file" },
      { -1, NULL }
   };
   expect_errors(expect);

   LOCAL_TEXT_BUF tb = tb_new();
   vlog_preprocess(tb, false);

   check_expected_errors();
}
END_TEST

START_TEST(test_specify1)
{
   input_from_file(TESTDIR "/vlog/specify1.v");

   vlog_node_t m = vlog_parse();
   fail_if(m == NULL);
   fail_unless(vlog_kind(m) == V_MODULE);

   vlog_check(m);

   fail_unless(vlog_parse() == NULL);

   fail_if_errors();
}
END_TEST

START_TEST(test_udp1)
{
   input_from_file(TESTDIR "/vlog/udp1.v");

   const error_t expect[] = {
      { 13, "the first port of a primitive must be an output" },
      { 13, "all ports of a primitive except the first must be inputs" },
      { 13, "no visible declaration for z" },
      { 25, "missing symbol for input y" },
      { 26, "too many symbols in UDP table entry" },
      { 58, "a sequential input list may have at most one edge indicator" },
      { -1, NULL }
   };
   expect_errors(expect);

   for (int i = 0; i < 5; i++) {
      vlog_node_t udp = vlog_parse();
      fail_if(udp == NULL);
      fail_unless(vlog_kind(udp) == V_PRIMITIVE);

      vlog_check(udp);
   }

   fail_unless(vlog_parse() == NULL);

   check_expected_errors();
}
END_TEST

START_TEST(test_implicit1)
{
   input_from_file(TESTDIR "/vlog/implicit1.v");

   vlog_node_t m = vlog_parse();
   fail_if(m == NULL);
   fail_unless(vlog_kind(m) == V_MODULE);

   vlog_check(m);

   ck_assert_int_eq(vlog_decls(m), 6);

   fail_unless(vlog_parse() == NULL);

   fail_if_errors();
}
END_TEST

START_TEST(test_struct1)
{
   input_from_file(TESTDIR "/vlog/struct1.v");

   const error_t expect[] = {
      { 13, "duplicate declaration of a" },
      { -1, NULL }
   };
   expect_errors(expect);

   vlog_node_t m = vlog_parse();
   fail_if(m == NULL);
   fail_unless(vlog_kind(m) == V_MODULE);

   vlog_check(m);

   ck_assert_int_eq(vlog_decls(m), 3);

   fail_unless(vlog_parse() == NULL);

   check_expected_errors();
}
END_TEST

START_TEST(test_enum1)
{
   input_from_file(TESTDIR "/vlog/enum1.v");

   vlog_node_t m = vlog_parse();
   fail_if(m == NULL);
   fail_unless(vlog_kind(m) == V_MODULE);

   vlog_check(m);

   ck_assert_int_eq(vlog_decls(m), 2);

   fail_unless(vlog_parse() == NULL);

   fail_if_errors();
}
END_TEST

START_TEST(test_union1)
{
   input_from_file(TESTDIR "/vlog/union1.v");

   vlog_node_t m = vlog_parse();
   fail_if(m == NULL);
   fail_unless(vlog_kind(m) == V_MODULE);

   vlog_check(m);

   ck_assert_int_eq(vlog_decls(m), 2);

   fail_unless(vlog_parse() == NULL);

   fail_if_errors();
}
END_TEST

START_TEST(test_param1)
{
   input_from_file(TESTDIR "/vlog/param1.v");

   const error_t expect[] = {
      {  4, "duplicate declaration of p1" },
      { -1, NULL }
   };
   expect_errors(expect);

   vlog_node_t m = vlog_parse();
   fail_if(m == NULL);
   fail_unless(vlog_kind(m) == V_MODULE);

   vlog_check(m);

   fail_unless(vlog_parse() == NULL);

   check_expected_errors();
}
END_TEST

START_TEST(test_pp3)
{
   input_from_file(TESTDIR "/vlog/pp3.v");

   const error_t expect[] = {
      {  3, "macro FOO undefined" },
      { -1, NULL }
   };
   expect_errors(expect);

   LOCAL_TEXT_BUF tb = tb_new();
   vlog_preprocess(tb, false);

   ck_assert_str_eq(
      tb_get(tb),
      "\n"
      "\n"
      "    // Warning\n");

   check_expected_errors();
}
END_TEST

START_TEST(test_concat1)
{
   input_from_file(TESTDIR "/vlog/concat1.v");

   const error_t expect[] = {
      { 10, "'q' cannot be driven by continuous assignment" },
      { -1, NULL }
   };
   expect_errors(expect);

   vlog_node_t m = vlog_parse();
   fail_if(m == NULL);
   fail_unless(vlog_kind(m) == V_MODULE);

   vlog_check(m);

   fail_unless(vlog_parse() == NULL);

   check_expected_errors();
}
END_TEST

START_TEST(test_pp4)
{
   input_from_file(TESTDIR "/vlog/pp4.v");

   const error_t expect[] = {
      { 11, "unexpected module while parsing expression" },
      {  0, "this token was unexpected" },
      {  0, "while expanding macro MACRO2" },
      {  0, "while expanding macro MACRO1" },
      { 16, "unexpected wire while parsing net declaration assignment" },
      { -1, NULL }
   };
   expect_errors(expect);

   LOCAL_TEXT_BUF tb = tb_new();
   vlog_preprocess(tb, true);

   input_from_buffer(tb_get(tb), tb_len(tb), SOURCE_VERILOG);

   vlog_node_t m = vlog_parse();
   fail_if(m == NULL);
   fail_unless(vlog_kind(m) == V_MODULE);

   vlog_check(m);

   fail_unless(vlog_parse() == NULL);

   check_expected_errors();
}
END_TEST

Suite *get_vlog_tests(void)
{
   Suite *s = suite_create("vlog");

   TCase *tc = nvc_unit_test();
   tcase_add_test(tc, test_dff);
   tcase_add_test(tc, test_simple_sem);
   tcase_add_test(tc, test_ports);
   tcase_add_test(tc, test_parse1);
   tcase_add_test(tc, test_number1);
   tcase_add_test(tc, test_number2);
   tcase_add_test(tc, test_pp1);
   tcase_add_test(tc, test_empty1);
   tcase_add_test(tc, test_timescale1);
   tcase_add_test(tc, test_defaultnettype);
   tcase_add_test(tc, test_gate1);
   tcase_add_test(tc, test_pp2);
   tcase_add_test(tc, test_specify1);
   tcase_add_test(tc, test_udp1);
   tcase_add_test(tc, test_implicit1);
   tcase_add_test(tc, test_struct1);
   tcase_add_test(tc, test_enum1);
   tcase_add_test(tc, test_union1);
   tcase_add_test(tc, test_param1);
   tcase_add_test(tc, test_pp3);
   tcase_add_test(tc, test_concat1);
   tcase_add_test(tc, test_pp4);
   suite_add_tcase(s, tc);

   return s;
}
