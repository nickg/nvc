//
//  Copyright (C) 2013-2024  Nick Gasson
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
#include "diag.h"
#include "jit/jit.h"
#include "option.h"
#include "phase.h"
#include "scan.h"
#include "type.h"

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

START_TEST(test_bounds)
{
   opt_set_int(OPT_MISSING_BODY, 0);
   input_from_file(TESTDIR "/bounds/bounds.vhd");

   const error_t expect[] = {
      {  26, "left index 0 violates constraint POSITIVE" },
      {  27, "right index 60 violates constraint FOO" },
      {  31, "array S index -52 outside of POSITIVE range 1 to 10" },
      {  32, "array S slice right index 11 outside of POSITIVE range 1 to 10" },
      {  33, "array S slice left index 0 outside of POSITIVE range 1 to 10" },
      {  37, "aggregate choice index 0 outside of POSITIVE range "
         "1 to 2147483647" },
      {  46, "actual length 4 does not match formal length 8 for parameter X" },
      {  47, "actual length 4 does not match formal length 8 for parameter X" },
      {  50, "actual length 4 for dimension 2 does not" },
      {  53, "value 9 does not match length of target 10 for signal S" },
      {  54, "value 2 does not match length of target 0 for signal N" },
      {  55, "expected at most 0 positional associations in MY_VEC1 " },
      {  60, "length of value 10 does not match length of target 3" },
      {  66, "array S index 11 outside of POSITIVE range 1 to 10" },
      {  67, "array S index -1 outside of POSITIVE range 1 to 10" },
      {  74, "aggregate choice index 0 outside of POSITIVE range 1 to "
         "2147483647" },
      {  83, "value '1' outside of ALPHA range 'a' to 'z' for variable A" },
      {  84, "0 outside of POSITIVE range 1 to 2147483647 for variable P" },
      {  89, "invalid dimension 5 for type MY_VEC1" },
      {  94, "value -1 outside of NATURAL range 0 to 2147483647 for "
         "parameter X" },
      { 107, "aggregate choice index 5 outside of POSITIVE range 1 to 3" },
      { 116, "length of sub-aggregate 2 does not match expected length 4" },
      { 137, "array index 14 outside of NATURAL range 0 to 2" },
      { 155, "value 2 outside of REAL range 0 to 1 for parameter X"},
      { 164, "missing choice for element FOUR of T_ARR with index type SE"},
      { 165, "expected at most 3 positional associations in T_ARR aggregate "
        "with index type SE range TWO to FOUR"},
      { 175, "ONE outside of E range THREE downto TWO for parameter ARG2" },
      { 176, "FOUR outside of E range THREE downto TWO for parameter ARG2"},
      { 177, "ONE outside of E range TWO to FOUR for parameter ARG1"},
      { 178, "FIVE outside of E range TWO to FOUR for parameter ARG1"},
      { 188, "aggregate choice index ONE outside of E range TWO to FOUR"},
      { 190, "aggregate choice index ONE outside of E range TWO to FOUR"},
      { 190, "aggregate choice index FIVE outside of E range TWO to FOUR"},
      { 198, "length of sub-aggregate 3 does not match expected length 4" },
      { 206, "left index ONE violates constraint SE" },
      { 206, "right index FOUR violates constraint SE" },
      { 219, "-5 NS outside of TIME range 0 HR to 10 NS for parameter A"},
      { 221, "5 NS outside of TIME range 10 SEC downto 20 US for parameter A"},
      { 227, "200 NS outside of TIME range -10 NS to 10 NS"},
      { 228, "-200 NS outside of TIME range -10 NS to 10 NS"},
      { 236, "array M index 'A' outside of CHARACTER range 'a' to 'z'" },
      { 237, "array M slice left index '1' outside of CHARACTER range "
        "'a' to 'z'" },
      { 242, "value 3 does not match length of target 4 for alias A" },
      { -1, NULL }
   };
   expect_errors(expect);

   tree_t a = parse_and_check(T_ENTITY, T_ARCH);
   fail_unless(error_count() == 0);

   unit_registry_t *ur = get_registry();
   mir_context_t *mc = get_mir();
   jit_t *jit = jit_new(ur, mc, NULL);
   simplify_local(a, jit, ur, mc);
   jit_free(jit);
   bounds_check(a);

   check_expected_errors();
}
END_TEST

START_TEST(test_bounds2)
{
   const error_t expect[] = {
      {  13, "assignment delay may not be negative"},
      {  20, "assignment delay may not be negative"},
      {  24, "assignment delay may not be negative"},
      {  25, "assignment delay may not be negative"},
      {  33, "rejection limit may not be negative"},
      {  34, "rejection limit may not be greater than first assignment delay"},
      {  39, "wait timeout may not be negative"},
      {  52, "value 20 outside of INTEGER range 0 to 9 for signal S"},
      {  53, "'Z' outside of CHARACTER range 'a' to 'z' for constant C1"},
      {  54, "value 10 outside of REAL range 0 to 5"},
      {  55, "value 0 HR outside of TIME range 10 NS to 10 US for constant T"},
      {  56, "value 10 outside of R range 0 to 1 for constant C2"},
      {  59, "value 30 outside of SUBINT range 1 to 10 for parameter A"},
      {  63, "'c' outside of CHARACTER range 'a' to 'b' for parameter A" },
      {  70, "value 20 outside of INTEGER range 10 downto 0 for generic G2"},
      {  73, "value 2 outside of INTEGER range 0 to 1 for port P2"},
      {  79, "value 5.0999999999999996 outside of REAL range 0 to 5"},
      {  95, "assignment delays must be in ascending time order"},
      {  96, "assignment delays must be in ascending time order"},
      {  97, "assignment delays must be in ascending time order"},
      {  98, "assignment delays must be in ascending time order"},
      {  99, "assignment delays must be in ascending time order"},
      { 127, "length of type conversion argument 9 does not match expected "
        "length 10 for constrained array subtype BV10" },
      { -1, NULL }
   };
   expect_errors(expect);

   input_from_file(TESTDIR "/bounds/bounds2.vhd");

   tree_t a = parse_and_check(T_ENTITY, T_ARCH);
   fail_unless(error_count() == 0);

   mir_context_t *mc = get_mir();
   unit_registry_t *ur = get_registry();
   jit_t *jit = jit_new(ur, mc, NULL);
   simplify_local(a, jit, ur, mc);
   jit_free(jit);
   bounds_check(a);

   check_expected_errors();
}
END_TEST

START_TEST(test_case)
{
   const error_t expect[] = {
      {  13, "missing choice for element C of type LETTER" },
      {  19, "missing choice for element B of type AB" },
      {  30, "10 to 19" },
      {  36, "4 to 2147483647" },
      {  44, "2147483647" },
      {  51, "value 50 is already covered" },
      {  53, "range 60 to 64 is already covered" },
      {  59, "case choice index -1 outside of NATURAL range 0 to 2147483647" },
      {  79, "choices cover only 2 of 8 possible values" },
      {  84, "missing choice for element 3 of BIT_VECTOR with index type "
         "NATURAL range 1 to 3" },
      {  86, "expected at most 3 positional associations in BIT_VECTOR "
         "aggregate with index type NATURAL range 1 to 3" },
      {  88, "expected 3 elements in string literal but have 2" },
      {  90, "expected 3 elements in string literal but have 4" },
      {  95, "choices cover only 2 of 65536 possible values" },
      { 101, "choices cover only 2 of 121 possible values" },
      { 113, "missing choices for elements 3 downto 1 of type MY_INT" },
      { 117, "case choice index 11 outside of MY_INT range 10 downto 1" },
      { -1, NULL }
   };
   expect_errors(expect);

   input_from_file(TESTDIR "/bounds/case.vhd");

   tree_t a = parse_and_check(T_ENTITY, T_ARCH);
   fail_unless(error_count() == 0);

   mir_context_t *mc = get_mir();
   unit_registry_t *ur = get_registry();
   jit_t *jit = jit_new(ur, mc, NULL);
   simplify_local(a, jit, ur, mc);
   bounds_check(a);
   jit_free(jit);

   check_expected_errors();
}
END_TEST

START_TEST(test_issue36)
{
   input_from_file(TESTDIR "/bounds/issue36.vhd");

   tree_t e = parse_and_check(T_ENTITY);

   mir_context_t *mc = get_mir();
   unit_registry_t *ur = get_registry();
   jit_t *jit = jit_new(ur, mc, NULL);
   simplify_local(e, jit, ur, mc);
   bounds_check(e);
   jit_free(jit);

   fail_if_errors();
}
END_TEST

START_TEST(test_issue54)
{
   const error_t expect[] = {
      { 12, "index 3 outside of NATURAL range 7 downto 4" },
      { 12, "index 0 outside of NATURAL range 7 downto 4" },
      { -1, NULL }
   };
   expect_errors(expect);

   input_from_file(TESTDIR "/bounds/issue54.vhd");

   tree_t a = parse_and_check(T_ENTITY, T_ARCH);
   fail_unless(error_count() == 0);

   mir_context_t *mc = get_mir();
   unit_registry_t *ur = get_registry();
   jit_t *jit = jit_new(ur, mc, NULL);
   simplify_local(a, jit, ur, mc);
   bounds_check(a);
   jit_free(jit);

   check_expected_errors();
}
END_TEST

START_TEST(test_issue99)
{
   const error_t expect[] = {
      {  7, "type conversion argument -1.5 out of bounds 0 to 2147483647" },
      {  8, "type conversion argument -1 out of bounds 1 to 5" },
      { -1, NULL }
   };
   expect_errors(expect);

   input_from_file(TESTDIR "/bounds/issue99.vhd");

   tree_t a = parse_and_check(T_ENTITY, T_ARCH);
   fail_unless(error_count() == 0);

   mir_context_t *mc = get_mir();
   unit_registry_t *ur = get_registry();
   jit_t *jit = jit_new(ur, mc, NULL);
   simplify_local(a, jit, ur, mc);
   bounds_check(a);

   check_expected_errors();
}
END_TEST

START_TEST(test_issue150)
{
   const error_t expect[] = {
      {  7, "missing choices for elements 6 to 7 of T_LUT8X8 with index "
         "type INTEGER range 0 to 7" },
      { -1, NULL }
   };
   expect_errors(expect);

   input_from_file(TESTDIR "/bounds/issue150.vhd");

   tree_t a = parse_and_check(T_ENTITY, T_ARCH);
   fail_unless(error_count() == 0);

   mir_context_t *mc = get_mir();
   unit_registry_t *ur = get_registry();
   jit_t *jit = jit_new(ur, mc, NULL);
   simplify_local(a, jit, ur, mc);
   bounds_check(a);
   jit_free(jit);

   check_expected_errors();
}
END_TEST

START_TEST(test_issue200)
{
   input_from_file(TESTDIR "/bounds/issue200.vhd");

   tree_t a = parse_and_check(T_ENTITY, T_ARCH);
   fail_unless(error_count() == 0);

   mir_context_t *mc = get_mir();
   unit_registry_t *ur = get_registry();
   jit_t *jit = jit_new(ur, mc, NULL);
   simplify_local(a, jit, ur, mc);
   bounds_check(a);
   jit_free(jit);

   fail_if_errors();
}
END_TEST

START_TEST(test_issue208)
{
   const error_t expect[] = {
      { 20, "missing choice for element 1 of type INTEGER range 1 downto 0" },
      { -1, NULL }
   };
   expect_errors(expect);

   input_from_file(TESTDIR "/bounds/issue208.vhd");

   tree_t a = parse_and_check(T_ENTITY, T_ARCH);
   fail_unless(error_count() == 0);

   mir_context_t *mc = get_mir();
   unit_registry_t *ur = get_registry();
   jit_t *jit = jit_new(ur, mc, NULL);
   simplify_local(a, jit, ur, mc);
   bounds_check(a);
   jit_free(jit);

   check_expected_errors();
}
END_TEST

START_TEST(test_issue247)
{
   const error_t expect[] = {
      { -1, NULL }
   };
   expect_errors(expect);

   input_from_file(TESTDIR "/bounds/issue247.vhd");

   tree_t a = parse_and_check(T_PACKAGE);
   fail_unless(error_count() == 0);

   mir_context_t *mc = get_mir();
   unit_registry_t *ur = get_registry();
   jit_t *jit = jit_new(ur, mc, NULL);
   simplify_local(a, jit, ur, mc);
   bounds_check(a);
   jit_free(jit);

   check_expected_errors();
}
END_TEST

START_TEST(test_issue251)
{
   input_from_file(TESTDIR "/bounds/issue251.vhd");

   const error_t expect[] = {
      { 24, "array X index -1 outside of NATURAL range 3 downto 0" },
      { 30, "array A index -1 outside of NATURAL range 3 downto 0" },
      { 31, "length of value 4 does not match length of target 3" },
      { -1, NULL }
   };
   expect_errors(expect);

   tree_t a = parse_and_check(T_ENTITY, T_ARCH, T_ENTITY, T_ARCH);
   fail_unless(error_count() == 0);

   mir_context_t *mc = get_mir();
   unit_registry_t *ur = get_registry();
   jit_t *jit = jit_new(ur, mc, NULL);
   simplify_local(a, jit, ur, mc);
   bounds_check(a);
   jit_free(jit);

   check_expected_errors();
}
END_TEST

START_TEST(test_issue269)
{
   const error_t expect[] = {
      { -1, NULL }
   };
   expect_errors(expect);

   input_from_file(TESTDIR "/bounds/issue269.vhd");

   tree_t a = parse_and_check(T_PACKAGE);
   fail_unless(error_count() == 0);

   mir_context_t *mc = get_mir();
   unit_registry_t *ur = get_registry();
   jit_t *jit = jit_new(ur, mc, NULL);
   simplify_local(a, jit, ur, mc);
   bounds_check(a);
   jit_free(jit);

   check_expected_errors();
}
END_TEST

START_TEST(test_issue307b)
{
   input_from_file(TESTDIR "/bounds/issue307b.vhd");

   tree_t a = parse_check_and_simplify(T_ENTITY, T_ARCH);
   fail_unless(error_count() == 0);

   bounds_check(a);
   fail_if_errors();
}
END_TEST

START_TEST(test_issue356)
{
   input_from_file(TESTDIR "/bounds/issue356.vhd");

   tree_t a = parse_check_and_simplify(T_ENTITY, T_ARCH);
   fail_unless(error_count() == 0);

   bounds_check(a);
   fail_if_errors();
}
END_TEST

START_TEST(test_issue98)
{
   input_from_file(TESTDIR "/bounds/issue98.vhd");

   const error_t expect[] = {
      {  5, "value 9223372036854775807 outside of INTEGER range -2147483648 "
         "to 2147483647 for signal I" },
      { -1, NULL }
   };
   expect_errors(expect);

   parse_check_and_simplify(T_ENTITY, T_ARCH);

   check_expected_errors();
}
END_TEST

START_TEST(test_tc1147)
{
   input_from_file(TESTDIR "/bounds/tc1147.vhd");

   tree_t a = parse_check_and_simplify(T_ENTITY, T_ARCH);
   fail_unless(error_count() == 0);

   bounds_check(a);
   fail_if_errors();
}
END_TEST

START_TEST(test_aggregate)
{
   set_standard(STD_08);
   input_from_file(TESTDIR "/bounds/aggregate.vhd");

   const error_t expect[] = {
      {  8, "duplicate choice for A" },
      {  8, "for element B of MY_ENUM_MAP with index type MY_ENUM" },
      {  9, "for element C of MY_ENUM_MAP with index type MY_ENUM" },
      { 10, "duplicate choice for B" },
      { 16, "for element 1 of MY_BIT_VEC with index type NATURAL "
        "range 3 downto 1" },
      { 17, "missing choices for elements 6 downto 5, 10 downto 8 of "
        "MY_INT_MAP with index type MY_INT" },
      { 21, "expected at most 3 positional associations in INTEGER_VECTOR "
        "aggregate with index type NATURAL range 1 to 3" },
      { 22, "missing choice for element 5 of INTEGER_VECTOR with index "
        "type NATURAL range 1 to 5" },
      { 24, "length of value 2 does not match length of target 3" },
      { 25, "discrete range has 4 elements but length of expression is 3" },
      { 29, "discrete range has 4 elements but length of expression is 3" },
      { 32, "value -1 outside of POSITIVE range 1 to 2147483647" },
      { 35, "length of element 3 does not match expected length 4" },
      { -1, NULL }
   };
   expect_errors(expect);

   parse_check_and_simplify(T_ENTITY, T_ARCH);

   check_expected_errors();
}
END_TEST

START_TEST(test_osvvm1)
{
   set_standard(STD_08);
   input_from_file(TESTDIR "/bounds/osvvm1.vhd");

   parse_check_and_simplify(T_PACKAGE, T_PACK_BODY);

   fail_if_errors();
}
END_TEST

START_TEST(test_range1)
{
   input_from_file(TESTDIR "/bounds/range1.vhd");

   const error_t expect[] = {
      { 10, "invalid dimension 2 for type BIT_VECTOR" },
      { 11, "invalid dimension -1 for type BIT_VECTOR" },
      { -1, NULL }
   };
   expect_errors(expect);

   parse_check_and_simplify(T_ENTITY, T_ARCH);

   check_expected_errors();
}
END_TEST

START_TEST(test_case2)
{
   set_standard(STD_08);
   input_from_file(TESTDIR "/bounds/case2.vhd");

   const error_t expect[] = {
      { 29, "expected case choice to have length 5 but is 3" },
      { -1, NULL }
   };
   expect_errors(expect);

   parse_check_and_simplify(T_ENTITY, T_ARCH);

   check_expected_errors();
}
END_TEST

START_TEST(test_issue477a)
{
   set_standard(STD_08);
   input_from_file(TESTDIR "/bounds/issue477a.vhd");

   tree_t p = parse_check_and_simplify(T_PACKAGE);

   tree_t d = get_decl(p, "C_DATA_VERSION");
   fail_unless(tree_kind(d) == T_CONST_DECL);

   type_t type = tree_type(d);
   fail_unless(type_kind(type) == T_SUBTYPE);

   int64_t left;
   fail_unless(folded_int(tree_left(range_of(type, 0)), &left));
   ck_assert_int_eq(left, 7);

   fail_if_errors();
}
END_TEST

START_TEST(test_issue477b)
{
   input_from_file(TESTDIR "/bounds/issue477b.vhd");

   const error_t expect[] = {
      { 23, "missing choice for element TYPE_5 of T_DATA_SEGMENT_TEMPLATE " },
      { -1, NULL }
   };
   expect_errors(expect);

   parse_check_and_simplify(T_PACKAGE);

   check_expected_errors();
}
END_TEST

START_TEST(test_case3)
{
   input_from_file(TESTDIR "/bounds/case3.vhd");

   const error_t expect[] = {
      { 16, "duplicate choice in case statement" },
      { -1, NULL }
   };
   expect_errors(expect);

   parse_check_and_simplify(T_ENTITY, T_ARCH);

   check_expected_errors();
}
END_TEST

START_TEST(test_driver1)
{
   input_from_file(TESTDIR "/bounds/driver1.vhd");

   const error_t expect[] = {
      { 13, "array S index 7 outside of NATURAL range 1 to 3" },
      { 11, "array S index 5 outside of NATURAL range 1 to 3" },
      { 16, "array S index -1 outside of NATURAL range 1 to 3" },
      { -1, NULL }
   };
   expect_errors(expect);

   parse_check_and_simplify(T_ENTITY, T_ARCH);

   check_expected_errors();
}
END_TEST

START_TEST(test_nullrange)
{
   input_from_file(TESTDIR "/bounds/nullrange.vhd");

   const error_t expect[] = {
      { 14, "type T_NULL_INT has null range" },
      { 15, "value 0 outside of T_NULL_INT range 0 to -1 for variable X" },
      { -1, NULL }
   };
   expect_errors(expect);

   parse_check_and_simplify(T_ENTITY, T_ARCH);

   check_expected_errors();
}
END_TEST

START_TEST(test_issue617)
{
   input_from_file(TESTDIR "/bounds/issue617.vhd");

   const error_t expect[] = {
      { 17, "array EXAMPLE right bound -32 violates BIT_VECTOR index "
        "constraint 0 to 2147483647" },
      { -1, NULL }
   };
   expect_errors(expect);

   parse_check_and_simplify(T_ENTITY, T_ARCH);

   check_expected_errors();
}
END_TEST

START_TEST(test_issue734)
{
   input_from_file(TESTDIR "/bounds/issue734.vhd");

   const error_t expect[] = {
      { 10, "array VAL index 0 outside of NATURAL range 0 downto 1" },
      { 11, "VAL slice left index 1 outside of NATURAL range 0 downto 1" },
      { -1, NULL }
   };
   expect_errors(expect);

   parse_check_and_simplify(T_ENTITY, T_ARCH);

   check_expected_errors();
}
END_TEST

START_TEST(test_case4)
{
   set_standard(STD_08);

   input_from_file(TESTDIR "/bounds/case4.vhd");

   const error_t expect[] = {
      {  9, "choices cover only 1 of 256 ** 5 possible values" },
      {  0, "expression has 5 elements of type CHARACTER, each of which "
         "has 256 possible values" },
      {  0, "the --relaxed option downgrades this to a warning" },
      {  0, "the case expression subtype is not locally static" },
      { 18, "missing choices for elements 2 to 2147483647 of type NATURAL" },
      { 29, "choices cover only 1 of 256 ** 12 possible values" },
      { -1, NULL }
   };
   expect_errors(expect);

   parse_check_and_simplify(T_ENTITY, T_ARCH);

   check_expected_errors();
}
END_TEST

START_TEST(test_initial)
{
   set_standard(STD_19);

   input_from_file(TESTDIR "/bounds/initial.vhd");

   const error_t expect[] = {
      {  8, "expected at most 5 positional associations in REAL_VECTOR "
         "aggregate with index type NATURAL range 1 to 5" },
      { 11, "expected at most 5 positional associations in REAL_VECTOR "
        "aggregate with index type NATURAL range 1 to 5" },
      { 17, "expected at most 5 positional associations in REAL_VECTOR "
        "aggregate with index type NATURAL range 1 to 5" },
      { -1, NULL }
   };
   expect_errors(expect);

   parse_check_and_simplify(T_ENTITY, T_ARCH);

   check_expected_errors();
}
END_TEST

START_TEST(test_case5)
{
   input_from_file(TESTDIR "/bounds/case5.vhd");

   const error_t expect[] = {
      { 22, "missing choice for element BAZ of type T_TYPE" },
      { -1, NULL }
   };
   expect_errors(expect);

   parse_check_and_simplify(T_PACKAGE, T_PACKAGE, T_PACKAGE, T_PACK_BODY);

   check_expected_errors();
}
END_TEST

START_TEST(test_issue800)
{
   input_from_file(TESTDIR "/bounds/issue800.vhd");

   const error_t expect[] = {
      { 10, "call to predefined operator \"=\" always returns FALSE" },
      {  0, "left length is 3 but right length is 2" },
      { 11, "call to predefined operator \"/=\" always returns FALSE" },
      {  0, "left length is 3 but right length is 9" },
      { 14, "call to predefined operator \"=\" always returns FALSE" },
      {  0, "left length is 3 but right length is 1" },
      { -1, NULL }
   };
   expect_errors(expect);

   parse_check_and_simplify(T_ENTITY, T_ARCH);

   check_expected_errors();
}
END_TEST

START_TEST(test_issue806)
{
   input_from_file(TESTDIR "/bounds/issue806.vhd");

   parse_check_and_simplify(T_PACKAGE, T_ENTITY, T_ARCH);

   fail_if_errors();
}
END_TEST

START_TEST(test_issue819)
{
   input_from_file(TESTDIR "/bounds/issue819.vhd");

   parse_check_and_simplify(T_PACKAGE, T_PACKAGE, T_ENTITY, T_ARCH);

   fail_if_errors();
}
END_TEST

START_TEST(test_cons1)
{
   set_standard(STD_08);

   input_from_file(TESTDIR "/bounds/cons1.vhd");

   const error_t expect[] = {
      {  4, "left index -1 violates constraint POSITIVE" },
      {  7, "left index 0 violates constraint POSITIVE" },
      {  9, "left index 0 violates constraint POSITIVE" },
      { 12, "left index 0 violates constraint POSITIVE" },
      // TODO: these should be suppressed
      { 12, "left index 0 violates constraint POSITIVE" },
      { 12, "left index 0 violates constraint POSITIVE" },
      { 12, "left index 0 violates constraint POSITIVE" },
      { 12, "left index 0 violates constraint POSITIVE" },
      { 12, "left index 0 violates constraint POSITIVE" },
      { 12, "left index 0 violates constraint POSITIVE" },
      { -1, NULL }
   };
   expect_errors(expect);

   parse_check_and_simplify(T_PACKAGE);

   check_expected_errors();
}
END_TEST

START_TEST(test_issue863)
{
   input_from_file(TESTDIR "/bounds/issue863.vhd");

   const error_t expect[] = {
      { 17, "invalid dimension 0 for type REAL_MATRIX" },
      { -1, NULL }
   };
   expect_errors(expect);

   parse_check_and_simplify(T_ENTITY, T_ARCH);

   check_expected_errors();
}
END_TEST

START_TEST(test_subtype)
{
   input_from_file(TESTDIR "/bounds/subtype.vhd");

   const error_t expect[] = {
      {  3, "left index -1 violates constraint INT8" },
      {  4, "right index 300 violates constraint INT8" },
      {  8, "right index C violates constraint AB" },
      { 11, "left index -1 violates constraint REAL10" },
      { 13, "left index -1 violates constraint POSITIVE" },
      { -1, NULL }
   };
   expect_errors(expect);

   parse_check_and_simplify(T_PACKAGE);

   check_expected_errors();
}
END_TEST

START_TEST(test_issue951)
{
   input_from_file(TESTDIR "/bounds/issue951.vhd");

   parse_check_and_simplify(T_ENTITY, T_ARCH);

   fail_if_errors();
}
END_TEST

START_TEST(test_issue966)
{
   set_standard(STD_08);

   input_from_file(TESTDIR "/bounds/issue966.vhd");

   const error_t expect[] = {
      { 27, "duplicate choice in case statement" },
      {  0, "repeated here" },
      {  0, "previous choice for this value" },
      { 32, "choices cover only 13122 of 9 ** 10 possible values" },
      {  0, "expression has 10 elements of type STD_ULOGIC, each of "
         "which has 9 possible values" },
      { -1, NULL }
   };
   expect_errors(expect);

   parse_check_and_simplify(T_PACKAGE, T_PACK_BODY);

   check_expected_errors();
}
END_TEST

START_TEST(test_issue975)
{
   input_from_file(TESTDIR "/bounds/issue975.vhd");

   const error_t expect[] = {
      {  8, "left index 0 violates constraint POSITIVE" },
      {  9, "right index 0 violates constraint POSITIVE" },
      { -1, NULL }
   };
   expect_errors(expect);

   parse_check_and_simplify(T_ENTITY, T_ARCH);

   fail_unless(parse() == NULL);

   check_expected_errors();
}
END_TEST

START_TEST(test_issue1040)
{
   input_from_file(TESTDIR "/bounds/issue1040.vhd");

   const error_t expect[] = {
      {  7, "length of value 32 does not match length of target 12" },
      { -1, NULL }
   };
   expect_errors(expect);

   parse_check_and_simplify(T_ENTITY, T_ARCH);

   fail_unless(parse() == NULL);

   check_expected_errors();
}
END_TEST

START_TEST(test_issue1021)
{
   input_from_file(TESTDIR "/bounds/issue1021.vhd");

   const error_t expect[] = {
      {  6, "length of value 0 does not match length of target 2 for "
         "constant SMAP" },
      { -1, NULL }
   };
   expect_errors(expect);

   parse_check_and_simplify(T_ENTITY, T_ARCH);

   fail_unless(parse() == NULL);

   check_expected_errors();
}
END_TEST

START_TEST(test_issue1091)
{
   input_from_file(TESTDIR "/bounds/issue1091.vhd");

   const error_t expect[] = {
      {  9, "missing choices for elements 2 to 3 of VEC" },
      {  9, "missing choices for elements 0 to 2 of VEC" },
      { 10, "value 1 is already covered" },
      { 10, "missing choice for element 3 of VEC" },
      { -1, NULL }
   };
   expect_errors(expect);

   parse_check_and_simplify(T_PACKAGE, T_ENTITY, T_ARCH);

   fail_unless(parse() == NULL);

   check_expected_errors();
}
END_TEST

START_TEST(test_map1)
{
   input_from_file(TESTDIR "/bounds/map1.vhd");

   const error_t expect[] = {
      { 19, "length of value 3 does not match length of target 2" },
      { 28, "length of value 4 does not match length of target 3 for "
        "generic G1" },
      { 29, "value 7 outside of INTEGER range 0 to 3 for generic G2" },
      { 33, "length of value 2 does not match length of target 3 for "
        "signal P1" },
      { 44, "length of value 2 does not match length of target 3 for "
        "signal P1" },
      { -1, NULL }
   };
   expect_errors(expect);

   parse_check_and_simplify(T_ENTITY, T_ARCH);

   fail_unless(parse() == NULL);

   check_expected_errors();
}
END_TEST

START_TEST(test_issue817)
{
   set_standard(STD_08);

   input_from_file(TESTDIR "/bounds/issue817.vhd");

   const error_t expect[] = {
      { 37, "length of value 7 does not match length of target 8" },
      { -1, NULL }
   };
   expect_errors(expect);

   parse_check_and_simplify(T_PACKAGE, T_ENTITY, T_ARCH, T_ENTITY, T_ARCH);

   fail_unless(parse() == NULL);

   check_expected_errors();
}
END_TEST

START_TEST(test_map2)
{
   input_from_file(TESTDIR "/bounds/map2.vhd");

   const error_t expect[] = {
      { 19, "length of value 8 does not match length of target 7 for "
        "generic INFO" },
      { -1, NULL }
   };
   expect_errors(expect);

   parse_check_and_simplify(T_ENTITY, T_ARCH);

   check_expected_errors();
}
END_TEST

Suite *get_bounds_tests(void)
{
   Suite *s = suite_create("bounds");

   TCase *tc_core = nvc_unit_test();
   tcase_add_test(tc_core, test_bounds);
   tcase_add_test(tc_core, test_bounds2);
   tcase_add_test(tc_core, test_case);
   tcase_add_test(tc_core, test_issue36);
   tcase_add_test(tc_core, test_issue54);
   tcase_add_test(tc_core, test_issue99);
   tcase_add_test(tc_core, test_issue150);
   tcase_add_test(tc_core, test_issue200);
   tcase_add_test(tc_core, test_issue208);
   tcase_add_test(tc_core, test_issue247);
   tcase_add_test(tc_core, test_issue251);
   tcase_add_test(tc_core, test_issue269);
   tcase_add_test(tc_core, test_issue307b);
   tcase_add_test(tc_core, test_issue356);
   tcase_add_test(tc_core, test_issue98);
   tcase_add_test(tc_core, test_tc1147);
   tcase_add_test(tc_core, test_aggregate);
   tcase_add_test(tc_core, test_osvvm1);
   tcase_add_test(tc_core, test_range1);
   tcase_add_test(tc_core, test_case2);
   tcase_add_test(tc_core, test_issue477a);
   tcase_add_test(tc_core, test_issue477b);
   tcase_add_test(tc_core, test_case3);
   tcase_add_test(tc_core, test_driver1);
   tcase_add_test(tc_core, test_nullrange);
   tcase_add_test(tc_core, test_issue617);
   tcase_add_test(tc_core, test_issue734);
   tcase_add_test(tc_core, test_case4);
   tcase_add_test(tc_core, test_initial);
   tcase_add_test(tc_core, test_case5);
   tcase_add_test(tc_core, test_issue800);
   tcase_add_test(tc_core, test_issue806);
   tcase_add_test(tc_core, test_issue819);
   tcase_add_test(tc_core, test_cons1);
   tcase_add_test(tc_core, test_issue863);
   tcase_add_test(tc_core, test_subtype);
   tcase_add_test(tc_core, test_issue951);
   tcase_add_test(tc_core, test_issue966);
   tcase_add_test(tc_core, test_issue975);
   tcase_add_test(tc_core, test_issue1040);
   tcase_add_test(tc_core, test_issue1021);
   tcase_add_test(tc_core, test_issue1091);
   tcase_add_test(tc_core, test_map1);
   tcase_add_test(tc_core, test_issue817);
   tcase_add_test(tc_core, test_map2);
   suite_add_tcase(s, tc_core);

   return s;
}
