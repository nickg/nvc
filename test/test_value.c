#include "type.h"
#include "common.h"
#include "tree.h"

#include <check.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

START_TEST(test_integer)
{
   make_new_arena();

   tree_t dummy = tree_new(T_ENTITY);
   (void)dummy;   // Ensure at least one object survives GC

   type_t t = type_new(T_INTEGER);
   int64_t v;

   fail_unless(parse_value(t, "0", &v));
   fail_unless(v == 0);

   fail_unless(parse_value(t, "1", &v));
   fail_unless(v == 1);

   fail_unless(parse_value(t, "-1", &v));
   fail_unless(v == -1);

   fail_unless(parse_value(t, "2147483648", &v));
   fail_unless(v == 2147483648);

   fail_unless(parse_value(t, "-2147483648", &v));
   fail_unless(v == -2147483648);

   fail_unless(parse_value(t, "  14124  ", &v));
   fail_unless(v == 14124);

   fail_unless(parse_value(t, "25252781781981", &v));
   fail_unless(v == INT64_C(25252781781981));

   fail_unless(parse_value(t, "1_2_3", &v));
   fail_unless(v == 123);
}
END_TEST

START_TEST(test_enum)
{
   make_new_arena();

   tree_t dummy = tree_new(T_ENTITY);
   (void)dummy;   // Ensure at least one object survives GC

   int64_t v;
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
   fail_unless(v == 1);

   fail_unless(parse_value(t, "'x'  ", &v));
   fail_unless(v == 0);

   fail_unless(parse_value(t, " \tA_B_C  ", &v));
   fail_unless(v == 2);
}
END_TEST

START_TEST(test_subtype)
{
   make_new_arena();

   tree_t dummy = tree_new(T_ENTITY);
   (void)dummy;   // Ensure at least one object survives GC

   int64_t v;
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

      type_set_constraint(s, c);
   }

   fail_unless(parse_value(s, "A", &v));
   fail_unless(v == 0);

   fail_unless(parse_value(s, " C  ", &v));
   fail_unless(v == 2);   // Bounds check would happen later
}
END_TEST

Suite *get_value_tests(void)
{
   Suite *s = suite_create("value");

   TCase *tc_core = tcase_create("Core");
   tcase_add_test(tc_core, test_integer);
   tcase_add_test(tc_core, test_enum);
   tcase_add_test(tc_core, test_subtype);
   suite_add_tcase(s, tc_core);

   return s;
}
