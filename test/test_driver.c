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
#include "ident.h"
#include "driver.h"
#include "phase.h"
#include "scan.h"

static tree_t get_decl(tree_t container, const char *name)
{
   tree_t d = search_decls(container, ident_new(name), 0);
   ck_assert_msg(d != NULL, "cannot find %s", name);
   return d;
}

START_TEST(test_sanity1)
{
   input_from_file(TESTDIR "/driver/sanity1.vhd");

   tree_t a = parse_check_and_simplify(T_ENTITY, T_ARCH);

   driver_set_t *ds = find_drivers(a);

   tree_t x = get_decl(a, "X");
   tree_t y = get_decl(a, "Y");
   tree_t z = get_decl(a, "Z");
   tree_t r = get_decl(a, "R");

   tree_t p0 = tree_stmt(a, 0);
   tree_t p1 = tree_stmt(a, 1);
   tree_t u2 = tree_stmt(a, 2);

   driver_info_t *p0di = get_drivers(ds, p0);
   ck_assert_ptr_nonnull(p0);
   ck_assert_ptr_null(p0di->chain_decl);
   ck_assert_ptr_null(p0di->chain_proc);
   ck_assert_ptr_eq(p0di->where, p0);
   ck_assert(tree_kind(p0di->prefix) == T_REF);
   ck_assert_ptr_eq(tree_ref(p0di->prefix), x);
   ck_assert_ptr_eq(p0di->decl, x);
   ck_assert(!p0di->tentative);

   driver_info_t *p1di0 = get_drivers(ds, p1);
   ck_assert_ptr_nonnull(p1);
   ck_assert_ptr_nonnull(p1di0->chain_decl);
   ck_assert_ptr_nonnull(p1di0->chain_proc);
   ck_assert_ptr_eq(p1di0->where, p1);
   ck_assert(tree_kind(p1di0->prefix) == T_ARRAY_REF);
   ck_assert_ptr_eq(p1di0->decl, z);

   driver_info_t *p1di1 = p1di0->chain_proc;
   ck_assert_ptr_null(p1di1->chain_decl);
   ck_assert_ptr_nonnull(p1di1->chain_proc);
   ck_assert_ptr_eq(p1di1->where, p1);
   ck_assert(tree_kind(p1di1->prefix) == T_REF);
   ck_assert_ptr_eq(p1di1->decl, y);

   driver_info_t *p1di2 = p1di1->chain_proc;
   ck_assert_ptr_nonnull(p1di2->chain_decl);
   ck_assert_ptr_null(p1di2->chain_proc);
   ck_assert_ptr_eq(p1di2->where, p1);
   ck_assert(tree_kind(p1di2->prefix) == T_RECORD_REF);
   ck_assert_ptr_eq(p1di2->decl, r);

   driver_info_t *u2di0 = get_drivers(ds, u2);
   ck_assert_ptr_nonnull(u2);
   ck_assert_ptr_nonnull(u2di0->chain_decl);
   ck_assert_ptr_null(u2di0->chain_proc);
   ck_assert_ptr_eq(u2di0->where, u2);
   ck_assert(tree_kind(u2di0->prefix) == T_ARRAY_REF);
   ck_assert_ptr_eq(u2di0->decl, z);

   tree_t b4p0 = tree_stmt(tree_stmt(a, 4), 0);

   driver_info_t *b4p0di = get_drivers(ds, b4p0);
   ck_assert_ptr_nonnull(b4p0di);
   ck_assert_ptr_null(b4p0di->chain_proc);

   tree_t g5p0 = tree_stmt(tree_cond(tree_stmt(a, 5), 0), 0);

   driver_info_t *g5p0di = get_drivers(ds, g5p0);
   ck_assert_ptr_nonnull(g5p0di);
   ck_assert_ptr_nonnull(g5p0di->chain_proc);
   ck_assert(g5p0di->tentative);

   ck_assert(has_unique_driver(ds, x));
   ck_assert(!has_unique_driver(ds, z));

   free_drivers(ds);

   fail_if_errors();
}
END_TEST

START_TEST(test_sanity2)
{
   input_from_file(TESTDIR "/driver/sanity2.vhd");

   tree_t a = parse_check_and_simplify(T_ENTITY, T_ARCH);

   driver_set_t *di = find_drivers(a);

   tree_t x = get_decl(a, "X");
   tree_t y = get_decl(a, "Y");

   tree_t p0 = tree_stmt(a, 0);
   tree_t p1 = tree_stmt(a, 1);

   driver_info_t *p0di = get_drivers(di, p0);
   ck_assert_ptr_nonnull(p0);
   ck_assert_ptr_null(p0di->chain_decl);
   ck_assert_ptr_null(p0di->chain_proc);
   ck_assert_ptr_eq(p0di->where, p0);
   ck_assert(tree_kind(p0di->prefix) == T_ARRAY_SLICE);
   ck_assert_ptr_eq(p0di->decl, x);
   ck_assert(!p0di->tentative);

   driver_info_t *p1di = get_drivers(di, p1);
   ck_assert_ptr_nonnull(p1);
   ck_assert_ptr_null(p1di->chain_decl);
   ck_assert_ptr_null(p1di->chain_proc);
   ck_assert_ptr_eq(p1di->where, p1);
   ck_assert(tree_kind(p1di->prefix) == T_REF);
   ck_assert_ptr_eq(p1di->decl, y);
   ck_assert(!p1di->tentative);

   free_drivers(di);

   fail_if_errors();
}
END_TEST

START_TEST(test_unique1)
{
   input_from_file(TESTDIR "/driver/unique1.vhd");

   tree_t a = parse_check_and_simplify(T_ENTITY, T_ARCH);

   driver_set_t *ds = find_drivers(a);

   tree_t s1 = get_decl(a, "S1");
   ck_assert(has_unique_driver(ds, s1));

   tree_t s2 = get_decl(a, "S2");
   ck_assert(!has_unique_driver(ds, s2));

   tree_t s3 = get_decl(a, "S3");
   ck_assert(has_unique_driver(ds, s3));

   tree_t s4 = get_decl(a, "S4");
   ck_assert(has_unique_driver(ds, s4));

   tree_t s5 = get_decl(a, "S5");
   ck_assert(!has_unique_driver(ds, s5));

   free_drivers(ds);

   fail_if_errors();
}
END_TEST

START_TEST(test_unique2)
{
   input_from_file(TESTDIR "/driver/unique2.vhd");

   tree_t a = parse_check_and_simplify(T_ENTITY, T_ARCH);

   driver_set_t *ds = find_drivers(a);

   tree_t s1 = get_decl(a, "S1");
   ck_assert(!has_unique_driver(ds, s1));

   free_drivers(ds);

   fail_if_errors();
}
END_TEST

START_TEST(test_unique3)
{
   set_standard(STD_19);

   input_from_file(TESTDIR "/driver/unique3.vhd");

   tree_t a = parse_check_and_simplify(T_ENTITY, T_ARCH);

   driver_set_t *ds = find_drivers(a);

   tree_t s1 = get_decl(a, "S1");
   ck_assert(!has_unique_driver(ds, s1));

   tree_t s2 = get_decl(a, "S2");
   ck_assert(!has_unique_driver(ds, s2));

   free_drivers(ds);

   fail_if_errors();
}
END_TEST

Suite *get_driver_tests(void)
{
   Suite *s = suite_create("driver");

   TCase *tc = nvc_unit_test();
   tcase_add_test(tc, test_sanity1);
   tcase_add_test(tc, test_sanity2);
   tcase_add_test(tc, test_unique1);
   tcase_add_test(tc, test_unique2);
   tcase_add_test(tc, test_unique3);
   suite_add_tcase(s, tc);

   return s;
}
