//
//  Copyright (C) 2023-2024  Nick Gasson
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
#include "lib.h"
#include "ident.h"
#include "option.h"
#include "phase.h"
#include "psl/psl-node.h"
#include "psl/psl-phase.h"
#include "scan.h"
#include "tree.h"
#include "vlog/vlog-node.h"
#include "vlog/vlog-phase.h"

#include <string.h>

static void diff_dump(const char *have, const char *expect)
{
   if (strcmp(have, expect) == 0)
      return;

   for (;;) {
      const char *nl1 = strchrnul(have, '\n');
      const char *nl2 = strchrnul(expect, '\n');

      const int len1 = nl1 - have;
      const int len2 = nl2 - expect;

      if (len1 != len2 || strncmp(have, expect, len1) != 0) {
         color_printf("$red$- %.*s$$\n", len2, expect);
         color_printf("$green$+ %.*s$$\n", len1, have);
         ck_abort_msg("found difference in dump");
      }
      else if (*nl1 == '\0' || *nl2 == '\0')
         break;

      have = nl1 + 1;
      expect = nl2 + 1;
   }

   ck_assert_str_eq(have, expect);
}

START_TEST(test_vhdl1)
{
   input_from_file(TESTDIR "/dump/vhdl1.vhd");

   tree_t a = parse_and_check(T_ENTITY, T_ARCH);

   LOCAL_TEXT_BUF tb = tb_new();
   capture_syntax(tb);

   dump(tree_primary(a));
   diff_dump(tb_get(tb),
             "use STD.STANDARD.all;\n"
             "\n"
             "entity WORK.VHDL1 is\n"
             "  port (\n"
             "    signal X : in BIT;\n"
             "    signal Y : out INTEGER );\n"
             "end entity;\n"
             "\n");
   tb_rewind(tb);

   dump(a);
   diff_dump(tb_get(tb),
             "use STD.STANDARD.all;\n"
             "\n"
             "architecture WORK.VHDL1-TEST of VHDL1 is\n"
             "  signal Z : BIT;\n"
             "begin\n"
             "  Z <= X after 1 NS;\n"
             "end architecture;\n"
             "\n");
   tb_rewind(tb);

   fail_if_errors();
}
END_TEST

START_TEST(test_vhdl2)
{
   opt_set_int(OPT_MISSING_BODY, 0);
   set_standard(STD_08);

   input_from_file(TESTDIR "/dump/vhdl2.vhd");

   tree_t a = parse_and_check(T_ENTITY, T_ARCH);

   LOCAL_TEXT_BUF tb = tb_new();
   capture_syntax(tb);

   const char *decl_cases[][2] = {
      { "S1",
        "signal S1 : BIT_VECTOR(1 to 6) := (1 => '1', others => '0');\n" },
      { "T1", "type T1 is array (NATURAL range <>) of BIT;\n" },
      { "T2", "subtype T2 is T1(1 to 99);\n" },
      { "F1",
        "function F1 (\n"
        "    constant X : in INTEGER;\n"
        "    constant Y : in INTEGER ) return BIT;\n"
        "  -- WORK.VHDL2-TEST.F1(II)J\n" },
      { "PROC1",
        "procedure PROC1 is   -- Never waits\n"
        "  -- WORK.VHDL2-TEST.PROC1\n"
        "  variable V1 : INTEGER := 5;\n"
        "begin\n"
        "  V1 := \"+\"(V1, 1);\n"
        "end procedure;\n" },
      { "C1",
        "component C1 is\n"
        "  generic (\n"
        "    constant G1 : in INTEGER := 2;\n"
        "    type T is private;\n"
        "    -- predefined function \"=\" : in \"=\" [T, T return BOOLEAN] := <>;\n"
        "    -- predefined function \"/=\" : in \"/=\" [T, T return BOOLEAN] := <>;\n"
        "  );\n"
        "  port (\n"
        "    signal X : out BIT;\n"
        "    signal Y : out BIT );\n"
        "end component;\n" },
      { "T3",
        "type T3 is record\n"
        "  X : INTEGER;\n"
        "  Y : INTEGER;\n"
        "end record;\n" },
      { "FACT",
        "function FACT ( constant N : in NATURAL ) return NATURAL is\n"
        "  -- WORK.VHDL2-TEST.FACT(N)N\n"
        "begin\n"
        "  if \">\"(N, 1) then\n"
        "    return \"*\"(N, FACT(\"-\"(N, 1)));\n"
        "  else\n"
        "    return 1;\n"
        "  end if;\n"
        "end function;\n" },
   };

   for (int i = 0; i < ARRAY_LEN(decl_cases); i++) {
      dump(get_decl(a, decl_cases[i][0]));
      diff_dump(tb_get(tb), decl_cases[i][1]);
      tb_rewind(tb);
   }

   const char *stmt_cases[][2] = {
      { "B1",
        "B1: block is\n"
        "  port ( signal P : in INTEGER );\n"
        "  port map (P => inertial 1);\n"
        "begin\n"
        "end block;\n" },
      { "U1",
        "U1: component C1\n"
        "  generic map (T => INTEGER, \"=\", \"/=\")\n"
        "  port map (open, Y => S1(0));\n" },
      { "P1",
        "P1: process (S1) is\n"
        "begin\n"
        "  S1 <= reject 1 NS inertial \"101010\" after 1 NS, (others => '1') after 2 NS;\n"
        "  report \"hello\";\n"
        "end process;\n" },
      { "G1",
        "G1: for I in INTEGER(5) downto INTEGER(2) generate\n"
        "  /* loop variable */ I : INTEGER range INTEGER(5) downto INTEGER(2);\n"
        "  attribute A : INTEGER;\n"
        "  signal R : REAL := 1.234500;\n"
        "  attribute A of R : signal is 5;\n"
        "begin\n"
        "  assert \">\"(R, 0.000000);\n"
        "end generate;\n" },
   };

   const int nstmts = tree_stmts(a);
   for (int i = 0; i < ARRAY_LEN(stmt_cases); i++) {
      tree_t s = NULL;
      for (int j = 0; j < nstmts; j++) {
         tree_t sj = tree_stmt(a, j);
         if (icmp(tree_ident(sj), stmt_cases[i][0])) {
            s = sj;
            break;
         }
      }

      ck_assert_msg(s != NULL, "cannot find statemnt %s", stmt_cases[i][0]);

      dump(s);
      diff_dump(tb_get(tb), stmt_cases[i][1]);
      tb_rewind(tb);
   }


   fail_if_errors();
}
END_TEST

START_TEST(test_vhdl3)
{
   set_standard(STD_08);

   input_from_file(TESTDIR "/dump/vhdl3.vhd");

   tree_t b = parse_and_check(T_PACKAGE, T_PACK_BODY, -1);

   LOCAL_TEXT_BUF tb = tb_new();
   capture_syntax(tb);

   dump(tree_primary(b));
   diff_dump(tb_get(tb),
             "use STD.STANDARD.all;\n"
             "\n"
             "package WORK.VHDL3 is\n"
             "  generic ( constant G : in INTEGER );\n"
             "  constant C1 : BIT_VECTOR(1 to G);\n"
             "end package;\n"
             "\n");
   tb_rewind(tb);

   dump(b);
   diff_dump(tb_get(tb),
             "use STD.STANDARD.all;\n"
             "\n"
             "package body WORK.VHDL3-body is\n"
             "  constant C1 : BIT_VECTOR(1 to G) := (others => '1');\n"
             "  constant C2 : INTEGER := << constant .UUT.X : INTEGER >>;\n"
             "end package body;\n"
             "\n");
   tb_rewind(tb);

   fail_if_errors();
}
END_TEST

START_TEST(test_vhdl4)
{
   set_standard(STD_19);

   input_from_file(TESTDIR "/dump/vhdl4.vhd");

   tree_t e = parse_and_check(T_ENTITY);

   LOCAL_TEXT_BUF tb = tb_new();
   capture_syntax(tb);

   dump(e);
   diff_dump(tb_get(tb),
             "use STD.STANDARD.all;\n"
             "\n"
             "entity WORK.E is\n"
             "  generic (\n"
             "    type T1 is private;\n"
             "    -- predefined function \"=\" : in \"=\" [T1, T1 return BOOLEAN] := <>;\n"
             "    -- predefined function \"/=\" : in \"/=\" [T1, T1 return BOOLEAN] := <>;\n"
             "    type T2 is <>;\n"
             "    -- predefined function \"<\" : in \"<\" [T2, T2 return BOOLEAN] := <>;\n"
             "    -- predefined function \">\" : in \">\" [T2, T2 return BOOLEAN] := <>;\n"
             "    -- predefined function \"<=\" : in \"<=\" [T2, T2 return BOOLEAN] := <>;\n"
             "    -- predefined function \">=\" : in \">=\" [T2, T2 return BOOLEAN] := <>;\n"
             "    -- predefined function MINIMUM : in MINIMUM [T2, T2 return T2] := <>;\n"
             "    -- predefined function MAXIMUM : in MAXIMUM [T2, T2 return T2] := <>;\n"
             "    -- predefined function TO_STRING : in TO_STRING [T2 return STRING] := <>;\n"
             "    -- predefined function \"=\" : in \"=\" [T2, T2 return BOOLEAN] := <>;\n"
             "    -- predefined function \"/=\" : in \"/=\" [T2, T2 return BOOLEAN] := <>;\n"
             "    type T3 is (<>);\n"
             "    -- predefined function \"<\" : in \"<\" [T3, T3 return BOOLEAN] := <>;\n"
             "    -- predefined function \">\" : in \">\" [T3, T3 return BOOLEAN] := <>;\n"
             "    -- predefined function \"<=\" : in \"<=\" [T3, T3 return BOOLEAN] := <>;\n"
             "    -- predefined function \">=\" : in \">=\" [T3, T3 return BOOLEAN] := <>;\n"
             "    -- predefined function MINIMUM : in MINIMUM [T3, T3 return T3] := <>;\n"
             "    -- predefined function MAXIMUM : in MAXIMUM [T3, T3 return T3] := <>;\n"
             "    -- predefined function TO_STRING : in TO_STRING [T3 return STRING] := <>;\n"
             "    -- predefined function \"=\" : in \"=\" [T3, T3 return BOOLEAN] := <>;\n"
             "    -- predefined function \"/=\" : in \"/=\" [T3, T3 return BOOLEAN] := <>;\n"
             "    type T4 is range <>;\n"
             "    -- predefined function \"**\" : in \"**\" [T4, T4 return T4] := <>;\n"
             "    -- predefined function \"mod\" : in \"mod\" [T4, T4 return T4] := <>;\n"
             "    -- predefined function \"rem\" : in \"rem\" [T4, T4 return T4] := <>;\n"
             "    -- predefined function \"+\" : in \"+\" [T4, T4 return T4] := <>;\n"
             "    -- predefined function \"-\" : in \"-\" [T4, T4 return T4] := <>;\n"
             "    -- predefined function \"+\" : in \"+\" [T4 return T4] := <>;\n"
             "    -- predefined function \"-\" : in \"-\" [T4 return T4] := <>;\n"
             "    -- predefined function \"*\" : in \"*\" [T4, T4 return T4] := <>;\n"
             "    -- predefined function \"/\" : in \"/\" [T4, T4 return T4] := <>;\n"
             "    -- predefined function \"abs\" : in \"abs\" [T4 return T4] := <>;\n"
             "    -- predefined function \"<\" : in \"<\" [T4, T4 return BOOLEAN] := <>;\n"
             "    -- predefined function \">\" : in \">\" [T4, T4 return BOOLEAN] := <>;\n"
             "    -- predefined function \"<=\" : in \"<=\" [T4, T4 return BOOLEAN] := <>;\n"
             "    -- predefined function \">=\" : in \">=\" [T4, T4 return BOOLEAN] := <>;\n"
             "    -- predefined function MINIMUM : in MINIMUM [T4, T4 return T4] := <>;\n"
             "    -- predefined function MAXIMUM : in MAXIMUM [T4, T4 return T4] := <>;\n"
             "    -- predefined function TO_STRING : in TO_STRING [T4 return STRING] := <>;\n"
             "    -- predefined function \"=\" : in \"=\" [T4, T4 return BOOLEAN] := <>;\n"
             "    -- predefined function \"/=\" : in \"/=\" [T4, T4 return BOOLEAN] := <>;\n"
             "    type T5 is units <>;\n"
             "    -- predefined function \"mod\" : in \"mod\" [T5, T5 return T5] := <>;\n"
             "    -- predefined function \"rem\" : in \"rem\" [T5, T5 return T5] := <>;\n"
             "    -- predefined function \"+\" : in \"+\" [T5, T5 return T5] := <>;\n"
             "    -- predefined function \"-\" : in \"-\" [T5, T5 return T5] := <>;\n"
             "    -- predefined function \"+\" : in \"+\" [T5 return T5] := <>;\n"
             "    -- predefined function \"-\" : in \"-\" [T5 return T5] := <>;\n"
             "    -- predefined function \"abs\" : in \"abs\" [T5 return T5] := <>;\n"
             "    -- predefined function \"<\" : in \"<\" [T5, T5 return BOOLEAN] := <>;\n"
             "    -- predefined function \">\" : in \">\" [T5, T5 return BOOLEAN] := <>;\n"
             "    -- predefined function \"<=\" : in \"<=\" [T5, T5 return BOOLEAN] := <>;\n"
             "    -- predefined function \">=\" : in \">=\" [T5, T5 return BOOLEAN] := <>;\n"
             "    -- predefined function MINIMUM : in MINIMUM [T5, T5 return T5] := <>;\n"
             "    -- predefined function MAXIMUM : in MAXIMUM [T5, T5 return T5] := <>;\n"
             "    -- predefined function TO_STRING : in TO_STRING [T5 return STRING] := <>;\n"
             "    -- predefined function \"=\" : in \"=\" [T5, T5 return BOOLEAN] := <>;\n"
             "    -- predefined function \"/=\" : in \"/=\" [T5, T5 return BOOLEAN] := <>;\n"
             "    type T6 is range <> . <>;\n"
             "    -- predefined function \"+\" : in \"+\" [T6, T6 return T6] := <>;\n"
             "    -- predefined function \"-\" : in \"-\" [T6, T6 return T6] := <>;\n"
             "    -- predefined function \"+\" : in \"+\" [T6 return T6] := <>;\n"
             "    -- predefined function \"-\" : in \"-\" [T6 return T6] := <>;\n"
             "    -- predefined function \"*\" : in \"*\" [T6, T6 return T6] := <>;\n"
             "    -- predefined function \"/\" : in \"/\" [T6, T6 return T6] := <>;\n"
             "    -- predefined function \"abs\" : in \"abs\" [T6 return T6] := <>;\n"
             "    -- predefined function \"<\" : in \"<\" [T6, T6 return BOOLEAN] := <>;\n"
             "    -- predefined function \">\" : in \">\" [T6, T6 return BOOLEAN] := <>;\n"
             "    -- predefined function \"<=\" : in \"<=\" [T6, T6 return BOOLEAN] := <>;\n"
             "    -- predefined function \">=\" : in \">=\" [T6, T6 return BOOLEAN] := <>;\n"
             "    -- predefined function MINIMUM : in MINIMUM [T6, T6 return T6] := <>;\n"
             "    -- predefined function MAXIMUM : in MAXIMUM [T6, T6 return T6] := <>;\n"
             "    -- predefined function TO_STRING : in TO_STRING [T6 return STRING] := <>;\n"
             "    -- predefined function \"=\" : in \"=\" [T6, T6 return BOOLEAN] := <>;\n"
             "    -- predefined function \"/=\" : in \"/=\" [T6, T6 return BOOLEAN] := <>;\n"
             "    type T7 is array (T3 range <>) of T1;\n"
             "    -- predefined function \"=\" : in \"=\" [T7, T7 return BOOLEAN] := <>;\n"
             "    -- predefined function \"/=\" : in \"/=\" [T7, T7 return BOOLEAN] := <>;\n"
             "    type T8 is access ..;\n"
             "    -- predefined function \"=\" : in \"=\" [T8, T8 return BOOLEAN] := <>;\n"
             "    -- predefined function \"/=\" : in \"/=\" [T8, T8 return BOOLEAN] := <>;\n"
             "    -- predefined procedure DEALLOCATE : in DEALLOCATE [T8] := <>;\n"
             "    type T9 is file of ..;\n"
             "    -- predefined function \"=\" : in \"=\" [T9, T9 return BOOLEAN] := <>;\n"
             "    -- predefined function \"/=\" : in \"/=\" [T9, T9 return BOOLEAN] := <>;\n"
             "  );\n"
             "end entity;\n"
             "\n");
   tb_rewind(tb);

   fail_if_errors();
}
END_TEST

START_TEST(test_vhdl5)
{
   set_standard(STD_19);

   input_from_file(TESTDIR "/dump/vhdl5.vhd");

   tree_t b = parse_and_check(T_PACKAGE, T_PACK_BODY);

   LOCAL_TEXT_BUF tb = tb_new();
   capture_syntax(tb);

   dump(b);
   diff_dump(tb_get(tb),
             "use STD.STANDARD.all;\n"
             "\n"
             "package body WORK.VHDL5-body is\n"
             "  type PT is protected\n"
             "    function SOMETHING return INTEGER;\n"
             "      -- WORK.VHDL5.PT.SOMETHING()I\n"
             "  end protected;\n"
             "\n"
             "  type PT is protected body\n"
             "    variable V : INTEGER := 55;\n"
             "\n"
             "    function SOMETHING return INTEGER is\n"
             "      -- WORK.VHDL5.PT.SOMETHING()I\n"
             "    begin\n"
             "      return V;\n"
             "    end function;\n"
             "  end protected body;\n"
             "\n"
             "  variable SV : PT;\n"
             "\n"
             "  procedure GET_IT ( variable X : out INTEGER ) is   -- Never waits\n"
             "    -- WORK.VHDL5.GET_IT(I)\n"
             "  begin\n"
             "    X := SV.SOMETHING;\n"
             "  end procedure;\n"
             "\n"
             "  procedure ATTRS is   -- Never waits\n"
             "    -- WORK.VHDL5.ATTRS\n"
             "    attribute FOO : STRING;\n"
             "    attribute FOO of ATTRS : procedure is \"123\";\n"
             "    attribute FOO of all : literal is \"abcd\";\n"
             "    attribute FOO of others : procedure is \"xxx\";\n"
             "  begin\n"
             "  end procedure;\n"
             "end package body;\n"
             "\n");
   tb_rewind(tb);

   fail_if_errors();
}
END_TEST

START_TEST(test_psl1)
{
   opt_set_int(OPT_PSL_COMMENTS, 1);

   input_from_file(TESTDIR "/dump/psl1.vhd");

   tree_t a = parse_and_check(T_ENTITY, T_ARCH);

   LOCAL_TEXT_BUF tb = tb_new();
   capture_syntax(tb);

   psl_dump(tree_psl(tree_decl(a, 4)));
   diff_dump(tb_get(tb), "default clock is \"and\"(CLK'EVENT, \"=\"(CLK, '1'))");
   tb_rewind(tb);

   psl_dump(tree_psl(tree_stmt(a, 0)));
   diff_dump(tb_get(tb), "assert never B");
   tb_rewind(tb);

   psl_dump(tree_psl(tree_stmt(a, 1)));
   diff_dump(tb_get(tb), "assert always A -> (next_a [3 to 5] B)");
   tb_rewind(tb);

   psl_dump(tree_psl(tree_stmt(a, 2)));
   diff_dump(tb_get(tb), "assert {A; \"and\"(B, C)}");
   tb_rewind(tb);

   psl_dump(tree_psl(tree_stmt(a, 3)));
   diff_dump(tb_get(tb), "assert A -> (next [2] (B until! C))");
   tb_rewind(tb);

   psl_dump(tree_psl(tree_stmt(a, 4)));
   diff_dump(tb_get(tb), "cover {{}[*]; {A}[*4]} report \"msg\"");
   tb_rewind(tb);

   fail_if_errors();
}
END_TEST

START_TEST(test_vlog1)
{
   input_from_file(TESTDIR "/dump/vlog1.v");

   vlog_node_t m1 = vlog_parse();
   fail_if(m1 == NULL);

   LOCAL_TEXT_BUF tb = tb_new();
   capture_syntax(tb);

   vlog_dump(m1, 0);
   diff_dump(tb_get(tb),
             "module dff (d, clk, rstb, q);\n"
             "  input d;\n"
             "  input clk;\n"
             "  input rstb;\n"
             "  output q;\n"
             "  reg q;\n"
             "  always @(posedge clk) q <= d;\n"
             "endmodule // dff\n\n");
   tb_rewind(tb);

   vlog_node_t m2 = vlog_parse();
   fail_if(m2 == NULL);

   vlog_dump(m2, 0);
   diff_dump(tb_get(tb),
             "module mod2;\n"
             "  wire [7:0] bus;\n"
             "  wire w;\n"
             "  reg r;\n"
             "  initial begin\n"
             "    $display(\"hello\", $time);\n"
             "    if (bus)\n"
             "      r <= 1 | r;\n"
             "    $finish;\n"
             "    r = 1;\n"
             "    #1 r <= 0;\n"
             "    r = ~w;\n"
             "    r = #1 5;\n"
             "    r <= #5 1;\n"
             "  end\n"
             "  assign bus = 3;\n"
             "  pullup (supply1,supply0) p1 (w);\n"
             "  mod u1 (w);\n"
             "  assign bus[2] = 4'd1;\n"
             "endmodule // mod2\n\n");
   tb_rewind(tb);

   fail_if_errors();
}
END_TEST

Suite *get_dump_tests(void)
{
   Suite *s = suite_create("dump");

   TCase *tc_core = nvc_unit_test();
   tcase_add_test(tc_core, test_vhdl1);
   tcase_add_test(tc_core, test_vhdl2);
   tcase_add_test(tc_core, test_vhdl3);
   tcase_add_test(tc_core, test_vhdl4);
   tcase_add_test(tc_core, test_vhdl5);
   tcase_add_test(tc_core, test_psl1);
   tcase_add_test(tc_core, test_vlog1);
   suite_add_tcase(s, tc_core);

   return s;
}
