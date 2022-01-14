#include "phase.h"
#include "util.h"
#include "common.h"
#include "test_util.h"
#include "json.h"

#include <check.h>
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
