//
//  Copyright (C) 2011-2022  Nick Gasson
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
#include "tree.h"
#include "type.h"
#include "util.h"

#include <stdlib.h>

static lib_t work;
static const char *tmp;

static void setup(void)
{
   tmp = getenv("ORIGINAL_TEMP");
   if (tmp == NULL) {
      tmp = getenv("TEMP");
      if (tmp == NULL)
         tmp = "/tmp";
   }

   char *path LOCAL = xasprintf("%s" DIR_SEP "test_lib", tmp);
   work = lib_new("test_lib", path);
   fail_if(work == NULL);
}

static void teardown(void)
{
   if (work) {
      lib_destroy(work);
      lib_free(work);

      work = NULL;
   }
}

static type_t my_int_type(void)
{
   static type_t type = NULL;

   if (type == NULL) {
      tree_t min = tree_new(T_LITERAL);
      tree_set_subkind(min, L_INT);
      tree_set_ival(min, INT64_MIN);

      tree_t max = tree_new(T_LITERAL);
      tree_set_subkind(max, L_INT);
      tree_set_ival(max, INT64_MAX);

      type = type_new(T_INTEGER);
      type_set_ident(type, ident_new("my_int_type"));

      tree_t r = tree_new(T_RANGE);
      tree_set_subkind(r, RANGE_TO);
      tree_set_left(r, min);
      tree_set_right(r, max);
      tree_set_type(r, type);

      type_add_dim(type, r);

      tree_set_type(min, type);
      tree_set_type(max, type);
   }

   return type;
}

static tree_t str_to_agg(const char *p, const char *end)
{
   tree_t t = tree_new(T_AGGREGATE);

   unsigned len = 0;
   while (*p != '\0' && p != end) {
      const char ch[] = { '\'', *p++, '\'', '\0' };

      tree_t ref = tree_new(T_REF);
      tree_set_ident(ref, ident_new(ch));

      tree_t a = tree_new(T_ASSOC);
      tree_set_subkind(a, A_POS);
      tree_set_value(a, ref);

      tree_add_assoc(t, a);

      ++len;
   }

   tree_t left = tree_new(T_LITERAL);
   tree_set_subkind(left, L_INT);
   tree_set_ival(left, 0);

   tree_t right = tree_new(T_LITERAL);
   tree_set_subkind(right, L_INT);
   tree_set_ival(right, len - 1);

   type_t type = type_new(T_ARRAY);
   type_set_ident(type, ident_new("string"));
   type_set_elem(type, my_int_type());
   type_add_index_constr(type, my_int_type());

   tree_t r = tree_new(T_RANGE);
   tree_set_subkind(r, RANGE_DOWNTO);
   tree_set_left(r, left);
   tree_set_right(r, right);

   tree_t cons = tree_new(T_CONSTRAINT);
   tree_set_subkind(cons, C_INDEX);
   tree_add_range(cons, r);

   type_t sub = type_new(T_SUBTYPE);
   type_set_base(sub, type);
   type_add_constraint(sub, cons);

   tree_set_type(t, sub);

   return t;
}

START_TEST(test_lib_new)
{
   fail_if(work == NULL);
}
END_TEST

START_TEST(test_lib_fopen)
{
   FILE *f = lib_fopen(work, "_test", "w");
   fprintf(f, "hello world");
   fclose(f);

   lib_free(work);

   lib_add_search_path(tmp);
   work = lib_find(ident_new("test_lib"));
   fail_if(work == NULL);

   f = lib_fopen(work, "_test", "r");
   fail_if(f == NULL);
   char buf[12];
   fail_if(fgets(buf, sizeof(buf), f) == NULL);

   fail_unless(strcmp(buf, "hello world") == 0);

   fclose(f);
}
END_TEST

START_TEST(test_lib_save)
{
   {
      make_new_arena();

      tree_t ent = tree_new(T_ENTITY);
      tree_set_ident(ent, ident_new("TEST_LIB.name"));

      type_t e = type_new(T_ENUM);
      type_set_ident(e, ident_new("myenum"));
      tree_t a = tree_new(T_ENUM_LIT);
      tree_set_ident(a, ident_new("a"));
      tree_set_type(a, e);
      tree_set_pos(a, 55);
      type_enum_add_literal(e, a);
      tree_t b = tree_new(T_ENUM_LIT);
      tree_set_ident(b, ident_new("b"));
      tree_set_type(b, e);
      type_enum_add_literal(e, b);

      tree_t p1 = tree_new(T_PORT_DECL);
      tree_set_ident(p1, ident_new("foo"));
      tree_set_subkind(p1, PORT_OUT);
      tree_set_type(p1, my_int_type());
      tree_add_port(ent, p1);

      tree_t p2 = tree_new(T_PORT_DECL);
      tree_set_ident(p2, ident_new("bar"));
      tree_set_subkind(p2, PORT_IN);
      tree_set_type(p2, e);
      tree_add_port(ent, p2);

      lib_put(work, ent);

      make_new_arena();

      tree_t ar = tree_new(T_ARCH);
      tree_set_ident(ar, ident_new("TEST_LIB.arch"));
      tree_set_ident2(ar, ident_new("foo"));

      tree_t pr = tree_new(T_PROCESS);
      tree_set_ident(pr, ident_new("proc"));
      tree_add_stmt(ar, pr);

      tree_t v1 = tree_new(T_VAR_DECL);
      tree_set_ident(v1, ident_new("v1"));
      tree_set_type(v1, e);

      tree_t r = tree_new(T_REF);
      tree_set_ident(r, ident_new("v1"));
      tree_set_ref(r, v1);

      tree_t s = tree_new(T_VAR_ASSIGN);
      tree_set_ident(s, ident_new("var_assign"));
      tree_set_target(s, r);
      tree_set_value(s, r);
      tree_add_stmt(pr, s);

      tree_t c = tree_new(T_LITERAL);
      tree_set_subkind(c, L_INT);
      tree_set_ival(c, 53);

      tree_t s2 = tree_new(T_VAR_ASSIGN);
      tree_set_ident(s2, ident_new("var_assign"));
      tree_set_target(s2, r);
      tree_set_value(s2, c);
      tree_add_stmt(pr, s2);

      tree_t s3 = tree_new(T_VAR_ASSIGN);
      tree_set_ident(s3, ident_new("var_assign"));
      tree_set_target(s3, r);
      tree_set_value(s3, str_to_agg("foobar", NULL));
      tree_add_stmt(pr, s3);

      tree_t s4 = tree_new(T_ASSERT);
      tree_set_ident(s4, ident_new("assert"));
      tree_set_value(s4, c);
      tree_set_severity(s4, c);
      tree_set_message(s4, str_to_agg("message", NULL));
      tree_add_stmt(pr, s4);

      lib_put(work, ar);
   }

   lib_save(work);
   lib_free(work);

   lib_add_search_path(tmp);
   work = lib_find(ident_new("test_lib"));
   fail_if(work == NULL);

   {
      tree_t ent = lib_get(work, ident_new("TEST_LIB.name"));
      fail_if(ent == NULL);
      fail_unless(tree_kind(ent) == T_ENTITY);
      fail_unless(tree_ident(ent) == ident_new("TEST_LIB.name"));
      fail_unless(tree_ports(ent) == 2);

      tree_t p1 = tree_port(ent, 0);
      fail_unless(tree_kind(p1) == T_PORT_DECL);
      fail_unless(tree_subkind(p1) == PORT_OUT);
      fail_unless(type_kind(tree_type(p1)) == T_INTEGER);

      tree_t p2 = tree_port(ent, 1);
      fail_unless(tree_kind(p2) == T_PORT_DECL);
      fail_unless(tree_subkind(p2) == PORT_IN);

      type_t e = tree_type(p2);
      fail_unless(type_kind(e) == T_ENUM);
      fail_unless(type_enum_literals(e) == 2);
      tree_t a = type_enum_literal(e, 0);
      fail_unless(tree_kind(a) == T_ENUM_LIT);
      fail_unless(tree_ident(a) == ident_new("a"));
      fail_unless(tree_type(a) == e);
      fail_unless(tree_pos(a) == 55);
      tree_t b = type_enum_literal(e, 1);
      fail_unless(tree_kind(b) == T_ENUM_LIT);
      fail_unless(tree_ident(b) == ident_new("b"));
      fail_unless(tree_type(b) == e);

      tree_t ar = lib_get(work, ident_new("TEST_LIB.arch"));
      fail_if(ar == NULL);
      fail_unless(tree_ident(ar) == ident_new("TEST_LIB.arch"));
      fail_unless(tree_ident2(ar) == ident_new("foo"));

      tree_t pr = tree_stmt(ar, 0);
      fail_unless(tree_kind(pr) == T_PROCESS);
      fail_unless(tree_ident(pr) == ident_new("proc"));

      tree_t s = tree_stmt(pr, 0);
      fail_unless(tree_kind(s) == T_VAR_ASSIGN);

      tree_t r = tree_target(s);
      fail_unless(tree_kind(r) == T_REF);
      fail_unless(tree_value(s) == r);

      tree_t s2 = tree_stmt(pr, 1);
      fail_unless(tree_kind(s2) == T_VAR_ASSIGN);
      fail_unless(tree_target(s2) == r);

      tree_t s3 = tree_stmt(pr, 2);
      fail_unless(tree_kind(s3) == T_VAR_ASSIGN);
      fail_unless(tree_target(s3) == r);
      fail_unless(tree_kind(tree_value(s3)) == T_AGGREGATE);

      tree_t s4 = tree_stmt(pr, 3);
      fail_unless(tree_kind(s4) == T_ASSERT);
      fail_unless(tree_ident(s4) == ident_new("assert"));

      tree_t c = tree_value(s2);
      fail_unless(tree_kind(c) == T_LITERAL);
      fail_unless(tree_subkind(c) == L_INT);
      fail_unless(tree_ival(c) == 53);

      // Type declaration and reference written to different units
      // so two copies of the type declaration will be read back
      // hence can't check for pointer equality here
      fail_unless(type_eq(tree_type(tree_ref(r)), e));
   }
}
END_TEST

Suite *get_lib_tests(void)
{
   Suite *s = suite_create("lib");

   TCase *tc_core = nvc_unit_test();
   tcase_add_checked_fixture(tc_core, setup, teardown);
   tcase_add_test(tc_core, test_lib_new);
   tcase_add_test(tc_core, test_lib_fopen);
   tcase_add_test(tc_core, test_lib_save);
   suite_add_tcase(s, tc_core);

   return s;
}
