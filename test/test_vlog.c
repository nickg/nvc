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
      { -1, NULL }
   };
   expect_errors(expect);

   input_from_file(TESTDIR "/vlog/simple_sem.v");

   for (int i = 0; i < 3; i++) {
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

START_TEST(test_dump)
{
   input_from_file(TESTDIR "/vlog/dff.v");

   vlog_node_t m = vlog_parse();
   fail_if(m == NULL);

   LOCAL_TEXT_BUF tb = tb_new();
   capture_syntax(tb);
   vlog_dump(m);

   ck_assert_str_eq(tb_get(tb),
                    "module dff (d, clk, rstb, q);\n"
                    "  input d;\n"
                    "  input clk;\n"
                    "  input rstb;\n"
                    "  output reg q;\n"
                    "  always @(posedge clk)\n"
                    "    q <= d;\n"
                    "endmodule // dff\n\n");

   fail_unless(vlog_parse() == NULL);

   fail_if_errors();
}
END_TEST

Suite *get_vlog_tests(void)
{
   Suite *s = suite_create("vlog");

   TCase *tc = nvc_unit_test();
   tcase_add_test(tc, test_dff);
   tcase_add_test(tc, test_simple_sem);
   tcase_add_test(tc, test_ports);
   tcase_add_test(tc, test_dump);
   suite_add_tcase(s, tc);

   return s;
}
