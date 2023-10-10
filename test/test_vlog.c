//
//  Copyright (C) 2022 Nick Gasson
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
   fail_unless(vlog_decls(m) == 4);

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
   fail_unless(vlog_subkind(d3) == V_PORT_OUTPUT_REG);
   fail_unless(vlog_ident(d3) == ident_new("q"));
   fail_unless(vlog_ident2(d3) == ident_new("Q"));

   vlog_node_t a = vlog_stmt(m, 0);
   fail_unless(vlog_kind(a) == V_ALWAYS);
   fail_unless(vlog_stmts(a) == 1);

   vlog_node_t e = vlog_stmt(a, 0);
   fail_unless(vlog_kind(e) == V_TIMING);
   fail_unless(vlog_stmts(e) == 1);

   vlog_node_t v = vlog_value(e);
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
      {  1, "no visible declaration for z" },
      { -1, NULL }
   };
   expect_errors(expect);

   input_from_file(TESTDIR "/vlog/ports.v");

   for (int i = 0; i < 2; i++) {
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
   fail_unless(vlog_stmts(m) == 3);
   fail_unless(vlog_ports(m) == 0);
   fail_unless(vlog_decls(m) == 4);

   vlog_node_t x = vlog_decl(m, 0);
   fail_unless(vlog_kind(x) == V_NET_DECL);
   fail_unless(vlog_ranges(x) == 1);
   fail_unless(vlog_subkind(x) == V_NET_WIRE);

   vlog_node_t xd0 = vlog_range(x, 0);
   fail_unless(vlog_kind(xd0) == V_DIMENSION);
   fail_unless(vlog_subkind(xd0) == V_DIM_PACKED);

   vlog_node_t s1 = vlog_stmt(m, 1);
   fail_unless(vlog_kind(s1) == V_ALWAYS);
   fail_unless(vlog_stmts(s1) == 1);

   vlog_node_t s1s0 = vlog_stmt(s1, 0);
   fail_unless(vlog_kind(s1s0) == V_SEQ_BLOCK);
   fail_unless(vlog_ident(s1s0) == ident_new("foo"));

   vlog_node_t s1s0s0 = vlog_stmt(s1s0, 0);
   fail_unless(vlog_kind(s1s0s0) == V_SYSTASK);
   fail_unless(vlog_ident(s1s0s0) == ident_new("$display"));
   fail_unless(vlog_params(s1s0s0) == 1);

   vlog_node_t hello = vlog_param(s1s0s0, 0);
   fail_unless(vlog_kind(hello) == V_STRING);
   ck_assert_str_eq(vlog_text(hello), "hello");

   vlog_node_t s1s0s1 = vlog_stmt(s1s0, 1);
   fail_unless(vlog_kind(s1s0s1) == V_SYSTASK);
   fail_unless(vlog_ident(s1s0s1) == ident_new("$finish"));

   vlog_node_t s1s0s2 = vlog_stmt(s1s0, 2);
   fail_unless(vlog_kind(s1s0s2) == V_IF);
   fail_unless(vlog_conds(s1s0s2) == 1);

   vlog_node_t s1s0s3 = vlog_stmt(s1s0, 3);
   fail_unless(vlog_kind(s1s0s3) == V_IF);
   fail_unless(vlog_conds(s1s0s3) == 1);

   vlog_node_t s1s0s4 = vlog_stmt(s1s0, 4);
   fail_unless(vlog_kind(s1s0s4) == V_IF);
   fail_unless(vlog_conds(s1s0s4) == 2);

   vlog_node_t s2 = vlog_stmt(m, 2);
   fail_unless(vlog_kind(s2) == V_ASSIGN);

   vlog_node_t s2or = vlog_value(s2);
   fail_unless(vlog_kind(s2or) == V_BINARY);
   fail_unless(vlog_subkind(s2or) == V_BINARY_OR);

   vlog_node_t s1s0s5 = vlog_stmt(s1s0, 5);
   fail_unless(vlog_kind(s1s0s5) == V_BASSIGN);

   fail_unless(vlog_parse() == NULL);

   fail_if_errors();
}
END_TEST

START_TEST(test_number1)
{
   number_t x = number_new("1'b1");
   fail_unless(number_is_defined(x));
   ck_assert_int_eq(number_width(x), 1);
   ck_assert_int_eq(number_integer(x), 1);
   number_free(&x);

   number_t y = number_new("5'b101");
   fail_unless(number_is_defined(y));
   ck_assert_int_eq(number_width(y), 5);
   ck_assert_int_eq(number_integer(y), 5);
   number_free(&y);

   number_t z = number_new("5'b1xx");
   fail_if(number_is_defined(z));
   ck_assert_int_eq(number_width(z), 5);
   number_free(&z);
}
END_TEST

START_TEST(test_number2)
{
   struct {
      const char *input;
      unsigned    width;
      int64_t     ival;
      const char *string;
   } cases[] = {
      { "1'b1",   1, 1,  "1'b1"      },
      { "5'b100", 5, 4,  "5'b100"    },
      { "1",      5, 1,  "5'b1"      },
      { "42",     8, 42, "8'b101010" },
   };

   LOCAL_TEXT_BUF tb = tb_new();

   for (int i = 0; i < ARRAY_LEN(cases); i++) {
      number_t n = number_new(cases[i].input);
      ck_assert_int_eq(number_width(n), cases[i].width);
      ck_assert_int_eq(number_integer(n), cases[i].ival);

      tb_rewind(tb);
      number_print(n, tb);
      ck_assert_str_eq(tb_get(tb), cases[i].string);

      number_free(&n);
   }
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
   suite_add_tcase(s, tc);

   return s;
}
