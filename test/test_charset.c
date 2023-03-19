//  -*- coding: iso-8859-1 -*-
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
#include "lib.h"
#include "phase.h"
#include "scan.h"
#include "tree.h"

START_TEST(test_iso88591)
{
   input_from_file(TESTDIR "/charset/iso8859-1.vhd");

   tree_t e = parse();
   fail_if(e == NULL);
   lib_put(lib_work(), e);

   ck_assert_int_eq(ident_len(tree_ident(e)), 10);
   fail_unless(tree_ident(e) == ident_new("WORK.ÚÛÜÝÞ"));

   tree_t a = parse();
   fail_if(a == NULL);

   fail_unless(tree_ident(a) == ident_new("WORK.ÚÛÜÝÞ-ÛÜÝÞÿÛÜÝÞÿ"));
   fail_unless(tree_decls(a) == 3);

   tree_t d1 = tree_decl(a, 1);
   fail_unless(tree_kind(d1) == T_CONST_DECL);
   fail_unless(tree_ident(d1) == ident_new("ßÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖØÙÚÛÜÝÞÿ"));

   tree_t d2 = tree_decl(a, 2);
   fail_unless(tree_kind(d2) == T_CONST_DECL);
   fail_unless(tree_ident(d2) == ident_new("ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖØÙÚÛÜÝÞ"));

   fail_unless(parse() == NULL);

   fail_if_errors();
}
END_TEST

START_TEST(test_utf8)
{
   input_from_file(TESTDIR "/charset/utf8.vhd");

   const error_t expect[] = {
      {  3, "possible multi-byte UTF-8 character found in input" },
      {  3, "unexpected error while parsing constant declaration" },
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

Suite *get_charset_tests(void)
{
   Suite *s = suite_create("charset");

   TCase *tc = nvc_unit_test();
   tcase_add_test(tc, test_iso88591);
   tcase_add_test(tc, test_utf8);
   suite_add_tcase(s, tc);

   return s;
}
