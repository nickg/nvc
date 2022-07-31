//
//  Copyright (C) 2013-2022  Nick Gasson
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
#include "type.h"
#include "common.h"
#include "tree.h"

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

START_TEST(test_integer)
{
   make_new_arena();

   tree_t dummy = tree_new(T_ENTITY);
   (void)dummy;   // Ensure at least one object survives GC

   type_t t = type_new(T_INTEGER);
   scalar_value_t v;

   fail_unless(parse_value(t, "0", &v));
   fail_unless(v.integer == 0);

   fail_unless(parse_value(t, "1", &v));
   fail_unless(v.integer == 1);

   fail_unless(parse_value(t, "-1", &v));
   fail_unless(v.integer == -1);

   fail_unless(parse_value(t, "2147483648", &v));
   fail_unless(v.integer == 2147483648);

   fail_unless(parse_value(t, "-2147483648", &v));
   fail_unless(v.integer == -2147483648);

   fail_unless(parse_value(t, "  14124  ", &v));
   fail_unless(v.integer == 14124);

   fail_unless(parse_value(t, "25252781781981", &v));
   fail_unless(v.integer == INT64_C(25252781781981));

   fail_unless(parse_value(t, "1_2_3", &v));
   fail_unless(v.integer == 123);
}
END_TEST

START_TEST(test_enum)
{
   make_new_arena();

   tree_t dummy = tree_new(T_ENTITY);
   (void)dummy;   // Ensure at least one object survives GC

   scalar_value_t v;
   type_t t = type_new(T_ENUM);
   {
      tree_t lit1 = tree_new(T_ENUM_LIT);
      tree_set_ident(lit1, ident_new("'x'"));

      tree_t lit2 = tree_new(T_ENUM_LIT);
      tree_set_ident(lit2, ident_new("HELLO"));

      tree_t lit3 = tree_new(T_ENUM_LIT);
      tree_set_ident(lit3, ident_new("A_B_C"));

      type_enum_add_literal(t, lit1);
      type_enum_add_literal(t, lit2);
      type_enum_add_literal(t, lit3);
   }

   fail_unless(parse_value(t, "HELLO", &v));
   fail_unless(v.integer == 1);

   fail_unless(parse_value(t, "'x'  ", &v));
   fail_unless(v.integer == 0);

   fail_unless(parse_value(t, " \tA_B_C  ", &v));
   fail_unless(v.integer == 2);
}
END_TEST

START_TEST(test_subtype)
{
   make_new_arena();

   tree_t dummy = tree_new(T_ENTITY);
   (void)dummy;   // Ensure at least one object survives GC

   scalar_value_t v;
   type_t t = type_new(T_ENUM);
   type_t s = type_new(T_SUBTYPE);
   {
      tree_t lit1 = tree_new(T_ENUM_LIT);
      tree_set_ident(lit1, ident_new("A"));
      tree_set_type(lit1, t);

      tree_t lit2 = tree_new(T_ENUM_LIT);
      tree_set_ident(lit2, ident_new("B"));
      tree_set_type(lit2, t);

      tree_t lit3 = tree_new(T_ENUM_LIT);
      tree_set_ident(lit3, ident_new("C"));
      tree_set_type(lit3, t);

      type_enum_add_literal(t, lit1);
      type_enum_add_literal(t, lit2);
      type_enum_add_literal(t, lit3);

      type_set_base(s, t);

      tree_t r = tree_new(T_RANGE);
      tree_set_subkind(r, RANGE_TO);
      tree_set_left(r, make_ref(lit1));
      tree_set_right(r, make_ref(lit2));

      tree_t c = tree_new(T_CONSTRAINT);
      tree_set_subkind(c, C_RANGE);
      tree_add_range(c, r);

      type_add_constraint(s, c);
   }

   fail_unless(parse_value(s, "A", &v));
   fail_unless(v.integer == 0);

   fail_unless(parse_value(s, " C  ", &v));
   fail_unless(v.integer == 2);   // Bounds check would happen later
}
END_TEST

START_TEST(test_real)
{
   type_t t = std_type(NULL, STD_REAL);
   scalar_value_t v;

   fail_unless(parse_value(t, "0", &v));
   ck_assert_double_eq(v.real, 0.0);

   fail_unless(parse_value(t, "1.0", &v));
   ck_assert_double_eq(v.real, 1.0);

   fail_unless(parse_value(t, "2.5", &v));
   ck_assert_double_eq(v.real, 2.5);

   fail_unless(parse_value(t, " -4.25", &v));
   ck_assert_double_eq(v.real, -4.25);

   fail_unless(parse_value(t, "  2e3  ", &v));
   ck_assert_double_eq(v.real, 2000.0);
}
END_TEST

START_TEST(test_physical)
{
   type_t t = std_type(NULL, STD_TIME);
   scalar_value_t v;

   fail_unless(parse_value(t, "0ps", &v));
   ck_assert_int_eq(v.integer, 0);

   fail_unless(parse_value(t, "2.5 ns", &v));
   ck_assert_int_eq(v.integer, 2500000);

   fail_unless(parse_value(t, " 3   ps ", &v));
   ck_assert_int_eq(v.integer, 3000);
}
END_TEST

Suite *get_value_tests(void)
{
   Suite *s = suite_create("value");

   TCase *tc_core = nvc_unit_test();
   tcase_add_test(tc_core, test_integer);
   tcase_add_test(tc_core, test_enum);
   tcase_add_test(tc_core, test_subtype);
   tcase_add_test(tc_core, test_real);
   tcase_add_test(tc_core, test_physical);
   suite_add_tcase(s, tc_core);

   return s;
}
