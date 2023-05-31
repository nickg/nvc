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

   ck_abort_msg("did not find the difference");
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

Suite *get_dump_tests(void)
{
   Suite *s = suite_create("dump");

   TCase *tc_core = nvc_unit_test();
   tcase_add_test(tc_core, test_vhdl1);
   suite_add_tcase(s, tc_core);

   return s;
}
