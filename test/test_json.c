//
//  Copyright (C) 2019-2022  Nick Gasson
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
#include "json.h"
#include "phase.h"
#include "scan.h"
#include "util.h"

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

JsonNode *trees_to_json(tree_t *elements, unsigned int n_elements);

START_TEST(test_basic)
{
   input_from_file(TESTDIR "/json/basic.vhd");

   tree_t arch = parse_check_and_simplify(T_ENTITY, T_ARCH);
   ck_assert_ptr_nonnull(arch);

   JsonNode *json = trees_to_json(&arch, 1);
   ck_assert_ptr_nonnull(json);

   JsonNode *jarch = json_find_element(json, 0);
   ck_assert_ptr_nonnull(jarch);

   JsonNode *jarch_cls = json_find_member(jarch, "cls");
   ck_assert_ptr_nonnull(jarch_cls);
   ck_assert_int_eq(JSON_STRING, jarch_cls->tag);
   ck_assert_str_eq("architecture", jarch_cls->string_);

   JsonNode *jproc = json_find_element(json_find_member(jarch, "stmts"), 0);
   ck_assert_ptr_nonnull(jproc);

   json_delete(json);
}
END_TEST

START_TEST(test_seq)
{
   input_from_file(TESTDIR "/json/seq.vhd");

   tree_t arch = parse_check_and_simplify(T_ENTITY, T_ARCH);
   ck_assert_ptr_nonnull(arch);

   JsonNode *json = trees_to_json(&arch, 1);
   ck_assert_ptr_nonnull(json);

   JsonNode *jarch = json_find_element(json, 0);
   ck_assert_ptr_nonnull(jarch);

   JsonNode *jproc = json_find_element(json_find_member(jarch, "stmts"), 0);
   ck_assert_ptr_nonnull(jproc);

   json_delete(json);
}
END_TEST

Suite *get_json_tests(void)
{
   Suite *s = suite_create("json");

   TCase *tc_core = nvc_unit_test();
   tcase_add_test(tc_core, test_basic);
   tcase_add_test(tc_core, test_seq);
   suite_add_tcase(s, tc_core);

   return s;
}
