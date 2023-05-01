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
#include "ident.h"
#include "jit/jit.h"
#include "lib.h"
#include "rt/shell.h"

START_TEST(test_sanity)
{
   tcl_shell_t *sh = shell_new(NULL);

   const char *result = NULL;
   fail_unless(shell_eval(sh, "expr 1 + 2", &result));
   ck_assert_str_eq(result, "3");

   shell_free(sh);
}
END_TEST

START_TEST(test_analyse)
{
   const error_t expect[] = {
      { 39, "invalid object class signal for generic X" },
      { -1, NULL }
   };
   expect_errors(expect);

   tcl_shell_t *sh = shell_new(jit_new);

   const char *result = NULL;
   fail_if(shell_eval(sh, "analyse " TESTDIR "/parse/entity.vhd", &result));
   ck_assert_ptr_eq(result, NULL);

   tree_t one = lib_get(lib_work(), ident_new("WORK.ONE"));
   fail_if(one == NULL);
   fail_unless(tree_kind(one) == T_ENTITY);

   shell_free(sh);

   check_expected_errors();
}
END_TEST

START_TEST(test_examine1)
{
   const error_t expect[] = {
      { -1, NULL }
   };
   expect_errors(expect);

   tcl_shell_t *sh = shell_new(jit_new);

   const char *result = NULL;

   shell_eval(sh, "analyse " TESTDIR "/shell/examine1.vhd", &result);
   ck_assert_str_eq(result, "");

   shell_eval(sh, "elaborate examine1", &result);
   ck_assert_str_eq(result, "");

   const char *tests[][2] = {
      { "/x", "5" },
      { "-binary /x", "00000000000000000000000000000101" },
      { "-hex /x", "0x5" },
      { "/x /y", "5 -2147483648" },
      { "-radix hex /y", "0x80000000" },
      { "/a", "'1'" },
      { "/b", "\"01XU\"" },
      { "-hex /b", "\"01XU\"" },   // TODO
   };

   for (int i = 0; i < ARRAY_LEN(tests); i++) {
      char script[128];
      checked_sprintf(script, ARRAY_LEN(script), "examine %s", tests[i][0]);

      shell_eval(sh, script, &result);
      ck_assert_msg(strcmp(result, tests[i][1]) == 0,
                    "'%s' ==> '%s' (expected '%s')", script, result,
                    tests[i][1]);
   }

   shell_free(sh);

   check_expected_errors();
}
END_TEST

Suite *get_shell_tests(void)
{
   Suite *s = suite_create("shell");

   TCase *tc = nvc_unit_test();
   tcase_add_test(tc, test_sanity);
   tcase_add_test(tc, test_analyse);
   tcase_add_test(tc, test_examine1);
   suite_add_tcase(s, tc);

   return s;
}
