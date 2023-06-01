//
//  Copyright (C) 2023  Nick Gasson
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
#include "scan.h"
#include "tree.h"
#include "vlog/vlog-node.h"

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
      { "P1",
        "procedure P1 is   -- Never waits\n"
        "  -- WORK.VHDL2-TEST.P1\n"
        "  variable V1 : INTEGER := 5;\n"
        "begin\n"
        "  V1 := \"+\"(V1, 1);\n"
        "end procedure;\n" },
      { "C1",
        "component C1 is\n"
        "  generic (\n"
        "    constant G1 : in INTEGER := 2;\n"
        "    type T : in T;\n"
        "    -- predefined function \"=\" : in \"=\" [T, T return BOOLEAN];\n"
        "    -- predefined function \"/=\" : in \"/=\" [T, T return BOOLEAN] );\n"
        "  port (\n"
        "    signal X : out BIT;\n"
        "    signal Y : out BIT );\n"
        "end component;\n" },
      { "T3",
        "type T3 is record\n"
        "  X : INTEGER;\n"
        "  Y : INTEGER;\n"
        "end record;\n" },
   };

   for (int i = 0; i < ARRAY_LEN(decl_cases); i++) {
      tree_t d = search_decls(a, ident_new(decl_cases[i][0]), 0);
      ck_assert_ptr_nonnull(d);

      dump(d);
      diff_dump(tb_get(tb), decl_cases[i][1]);
      tb_rewind(tb);
   }

   fail_if_errors();
}
END_TEST

Suite *get_dump_tests(void)
{
   Suite *s = suite_create("dump");

   TCase *tc_core = nvc_unit_test();
   tcase_add_test(tc_core, test_vhdl1);
   tcase_add_test(tc_core, test_vhdl2);
   suite_add_tcase(s, tc_core);

   return s;
}
