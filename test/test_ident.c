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
#include "ident.h"
#include "lib.h"

#include <check.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <time.h>

START_TEST(test_ident_new)
{
   ident_t i1 = ident_new("foo");
   fail_if(i1 == NULL);
}
END_TEST

START_TEST(test_equality)
{
   ident_t i1, i2, i3, i4;

   i1 = ident_new("foo");
   i2 = ident_new("foo");
   i3 = ident_new("foobar");
   i4 = ident_new("poo");

   fail_unless(i1 == i2);
   fail_if(i2 == i3);
   fail_if(i1 == i4);
   fail_if(i3 == i4);
}
END_TEST

START_TEST(test_istr)
{
   ident_t i1, i2;

   i1 = ident_new("frob");
   fail_unless(strcasecmp(istr(i1), "frob") == 0);

   i2 = ident_new("FrOB");
   fail_unless(strcasecmp(istr(i2), "FrOB") == 0);

   i1 = ident_new("pingu");
   fail_unless(strcasecmp(istr(i1), "PINGU") == 0);
}
END_TEST

START_TEST(test_rand)
{
   for (int i = 0; i < 10000; i++) {
      char buf[16];
      size_t len = (rand() % (sizeof(buf) - 3)) + 2;

      for (size_t j = 0; j < len; j++)
         buf[j] = '0' + (rand() % 80);
      buf[len - 1] = '\0';

      ident_t i1 = ident_new(buf);
      fail_if(i1 == NULL);
      fail_unless(strcmp(istr(i1), buf) == 0);
   }
}
END_TEST

START_TEST(test_read_write)
{
   ident_t i1, i2, i3;
   i1 = ident_new("goobar");
   i2 = ident_new("foo");
   i3 = ident_new("foo");

   fbuf_t *f = fbuf_open("test.ident", FBUF_OUT, FBUF_CS_NONE);
   fail_if(f == NULL);

   ident_wr_ctx_t wctx = ident_write_begin(f);

   ident_write(i1, wctx);
   ident_write(i2, wctx);
   ident_write(i3, wctx);

   ident_write_end(wctx);

   fbuf_close(f, NULL);

   f = fbuf_open("test.ident", FBUF_IN, FBUF_CS_NONE);
   fail_if(f == NULL);

   ident_rd_ctx_t rctx = ident_read_begin(f);

   ident_t j1, j2, j3;
   j1 = ident_read(rctx);
   j2 = ident_read(rctx);
   j3 = ident_read(rctx);

   ident_read_end(rctx);

   fail_unless(i1 == j1);
   fail_unless(i2 == j2);
   fail_unless(i3 == j3);

   fail_unless(j2 == j3);

   fbuf_close(f, NULL);

   remove("test.ident");
}
END_TEST

START_TEST(test_prefix)
{
   ident_t a, b, c, d, e, f;

   a = ident_new("foo");
   b = ident_new("bar");
   c = ident_prefix(a, b, '.');

   fail_unless(c == ident_new("foo.bar"));
   fail_if(c == a);
   fail_if(c == b);

   d = ident_prefix(ident_prefix(c, c, '.'),
                    ident_new("carrot"), '/');

   fail_unless(d == ident_new("foo.bar.foo.bar/carrot"));
   fail_if(d == c);

   e = ident_prefix(NULL, a, '?');
   fail_unless(e == a);

   f = ident_prefix(a, b, '\0');
   fail_unless(f == ident_new("foobar"));
}
END_TEST

START_TEST(test_strip)
{
   ident_t a, b, c;

   a = ident_new("something");
   b = ident_new("thing");
   c = ident_strip(a, b);

   fail_if(c == NULL);
   fail_unless(c == ident_new("some"));

   a = ident_new("g");
   b = ident_new("cake");
   c = ident_strip(a, b);

   fail_unless(c == NULL);
}
END_TEST

START_TEST(test_char)
{
   ident_t i;

   i = ident_new("foobar");
   fail_unless(ident_char(i, 0) == 'r');
   fail_unless(ident_char(i, 5) == 'f');
   fail_unless(ident_char(i, 3) == 'o');
}
END_TEST

START_TEST(test_until)
{
   ident_t i;
   i = ident_new("aye.bee.c");
   fail_unless(ident_until(i, '.') == ident_new("aye"));
   i = ident_new("nodot");
   fail_unless(ident_until(i, '.') == i);
}
END_TEST

START_TEST(test_runtil)
{
   ident_t i;
   i = ident_new("aye.bee.c");
   fail_unless(ident_runtil(i, '.') == ident_new("aye.bee"));
   i = ident_new("nodot");
   fail_unless(ident_runtil(i, '.') == i);
}
END_TEST

START_TEST(test_rfrom)
{
   ident_t i;
   i = ident_new("foo.bar.yah");
   fail_unless(ident_rfrom(i, '.') == ident_new("yah"));
}
END_TEST

START_TEST(test_from)
{
   ident_t i;
   i = ident_new("foo.bar.yah");
   fail_unless(ident_from(i, '.') == ident_new("bar.yah"));
}
END_TEST

START_TEST(test_icmp)
{
   ident_t i, j;

   i = ident_new("foobar");
   j = ident_new("cake");

   fail_unless(icmp(i, "foobar"));
   fail_unless(icmp(j, "cake"));
   fail_if(icmp(i, "cake"));
   fail_if(icmp(i, "fooba"));
   fail_if(icmp(j, "cakesniffer"));

   fail_unless(icmp(NULL, NULL));
   fail_if(icmp(NULL, "one"));
   fail_if(icmp(i, NULL));
}
END_TEST;

START_TEST(test_glob)
{
   ident_t i;

   i = ident_new("foobar");

   fail_unless(ident_glob(i, "foobar", -1));
   fail_unless(ident_glob(i, "foobar", 6));
   fail_if(ident_glob(i, "foobaz", -1));
   fail_if(ident_glob(i, "goobar", -1));
   fail_unless(ident_glob(i, "*", -1));
   fail_unless(ident_glob(i, "f*", -1));
   fail_unless(ident_glob(i, "f*r", -1));
   fail_unless(ident_glob(i, "f*b*r", -1));
   fail_if(ident_glob(i, "f*c*r", -1));
   fail_unless(ident_glob(i, "**bar", -1));

   i = ident_new("foo:bar:a");

   fail_unless(ident_glob(i, "*:a", -1));
   fail_unless(ident_glob(i, "foo:*", -1));
}
END_TEST;

START_TEST(test_interned)
{
   ident_new("foo14141");
   fail_unless(ident_interned("foo14141"));
   fail_if(ident_interned("foobar11111"));
}
END_TEST

START_TEST(test_contains)
{
   ident_t i = ident_new("cake");
   fail_unless(ident_contains(i, "k"));
   fail_unless(ident_contains(i, "moa"));
   fail_unless(ident_contains(i, "amo"));
   fail_if(ident_contains(i, "zod"));
   fail_if(ident_contains(i, ""));
}
END_TEST

START_TEST(test_len)
{
   fail_unless(ident_len(ident_new("a")) == 1);
   fail_unless(ident_len(ident_new("abc")) == 3);
}
END_TEST

START_TEST(test_downcase)
{
   fail_unless(ident_downcase(ident_new("ABC")) == ident_new("abc"));
   fail_unless(ident_downcase(ident_new("123xY")) == ident_new("123xy"));
}
END_TEST

START_TEST(test_compare)
{
   ck_assert_int_eq(0, ident_compare(ident_new("a"), ident_new("a")));
   ck_assert_int_eq(0, ident_compare(ident_new("aaa"), ident_new("aaa")));
   ck_assert_int_eq('a' - 'b',
                    ident_compare(ident_new("a"), ident_new("b")));
   ck_assert_int_eq('a' - 'b',
                    ident_compare(ident_new("aaa"), ident_new("aab")));
   ck_assert_int_eq('b' - 'a',
                    ident_compare(ident_new("aab"), ident_new("aaa")));
   ck_assert_int_eq(0 - 'a',
                    ident_compare(ident_new("aa"), ident_new("aaa")));
   ck_assert_int_eq('a' - 0,
                    ident_compare(ident_new("aaa"), ident_new("aa")));
   ck_assert_int_eq('b' - 'a',
                    ident_compare(ident_new("bab"), ident_new("aba")));
   ck_assert_int_eq('b' - 'l',
                    ident_compare(ident_new("abcd"), ident_new("alemnic")));
}
END_TEST

START_TEST(test_walk_selected)
{
   ident_t it = ident_new("foo.bar.baz");
   fail_unless(ident_walk_selected(&it) == ident_new("foo"));
   fail_unless(it == ident_new("bar.baz"));
   fail_unless(ident_walk_selected(&it) == ident_new("bar"));
   fail_unless(it == ident_new("baz"));
   fail_unless(ident_walk_selected(&it) == ident_new("baz"));
   fail_unless(it == NULL);

   it = ident_new("foo");
   fail_unless(ident_walk_selected(&it) == ident_new("foo"));
   fail_unless(it == NULL);

   it = ident_new("foo.'.'.bar");
   fail_unless(ident_walk_selected(&it) == ident_new("foo"));
   fail_unless(it == ident_new("'.'.bar"));
   fail_unless(ident_walk_selected(&it) == ident_new("'.'"));
   fail_unless(it == ident_new("bar"));

   it = ident_new("\\foo.'.'.bar\\");
   fail_unless(ident_walk_selected(&it) == ident_new("\\foo.'.'.bar\\"));
   fail_unless(it == NULL);
}
END_TEST

START_TEST(test_starts_with)
{
   fail_unless(ident_starts_with(ident_new("ABCdef"), ident_new("ABC")));
   fail_if(ident_starts_with(ident_new("abcdef"), ident_new("ABC")));
   fail_unless(ident_starts_with(ident_new("foo(x).bar"), ident_new("foo(x)")));
}
END_TEST

START_TEST(test_istr_r)
{
   char buf[16];

   istr_r(ident_new("foo"), buf, 16);
   ck_assert_str_eq(buf, "foo");

   istr_r(ident_new("X"), buf, 16);
   ck_assert_str_eq(buf, "X");

   istr_r(ident_new("?"), buf, 2);
   ck_assert_str_eq(buf, "?");

   istr_r(ident_new("string"), buf, 7);
   ck_assert_str_eq(buf, "string");
}
END_TEST

START_TEST(test_distance)
{
   const char *words[17] = {
      "previous", "clam", "loud", "sore", "striped", "healthy",
      "automatic", "spy", "surround", "trade", "flowers" "nifty",
      "chickens", "beef", "nutty", "kindly", "kitten", "sitting",
   };

   ident_t ids[17];
   for (int i = 0; i < 17; i++)
      ids[i] = ident_new(words[i]);

   const int expect[17][17] = {
      {  0,  8,  6,  7,  7,  7,  9,  8,  7,  7, 10,  7,  7,  8,  8,  8,  7, },
      {  8,  0,  4,  4,  7,  6,  8,  4,  8,  4, 11,  7,  4,  5,  6,  6,  7, },
      {  6,  4,  0,  3,  6,  6,  8,  4,  5,  4, 10,  8,  4,  5,  5,  6,  7, },
      {  7,  4,  3,  0,  4,  7,  8,  3,  6,  4, 10,  7,  4,  5,  6,  5,  6, },
      {  7,  7,  6,  4,  0,  7,  8,  5,  5,  4, 10,  7,  6,  7,  7,  6,  5, },
      {  7,  6,  6,  7,  7,  0,  8,  6,  8,  6, 10,  7,  6,  5,  6,  6,  7, },
      {  9,  8,  8,  8,  8,  8,  0,  9,  8,  7, 11,  9,  9,  6,  9,  7,  7, },
      {  8,  4,  4,  3,  5,  6,  9,  0,  7,  5, 10,  8,  4,  4,  5,  6,  6, },
      {  7,  8,  5,  6,  5,  8,  8,  7,  0,  7, 11,  7,  8,  7,  8,  7,  6, },
      {  7,  4,  4,  4,  4,  6,  7,  5,  7,  0, 11,  7,  5,  5,  5,  5,  6, },
      { 10, 11, 10, 10, 10, 10, 11, 10, 11, 11,  0, 11, 10,  9, 10, 10, 11, },
      {  7,  7,  8,  7,  7,  7,  9,  8,  7,  7, 11,  0,  7,  8,  7,  5,  6, },
      {  7,  4,  4,  4,  6,  6,  9,  4,  8,  5, 10,  7,  0,  5,  6,  5,  7, },
      {  8,  5,  5,  5,  7,  5,  6,  4,  7,  5,  9,  8,  5,  0,  5,  4,  5, },
      {  8,  6,  5,  6,  7,  6,  9,  5,  8,  5, 10,  7,  6,  5,  0,  4,  6, },
      {  8,  6,  6,  5,  6,  6,  7,  6,  7,  5, 10,  5,  5,  4,  4,  0,  3, },
      {  7,  7,  7,  6,  5,  7,  7,  6,  6,  6, 11,  6,  7,  5,  6,  3,  0, },
   };

   for (int i = 0; i < 17; i++) {
      for (int j = 0; j < 17; j++) {
         const int d = ident_distance(ids[i], ids[j]);
         ck_assert_int_eq(d, expect[i][j]);
      }
   }
}
END_TEST

Suite *get_ident_tests(void)
{
   Suite *s = suite_create("ident");

   TCase *tc_core = nvc_unit_test();
   tcase_add_test(tc_core, test_ident_new);
   tcase_add_test(tc_core, test_equality);
   tcase_add_test(tc_core, test_istr);
   tcase_add_test(tc_core, test_rand);
   tcase_add_test(tc_core, test_read_write);
   tcase_add_test(tc_core, test_prefix);
   tcase_add_test(tc_core, test_strip);
   tcase_add_test(tc_core, test_char);
   tcase_add_test(tc_core, test_until);
   tcase_add_test(tc_core, test_runtil);
   tcase_add_test(tc_core, test_icmp);
   tcase_add_test(tc_core, test_glob);
   tcase_add_test(tc_core, test_rfrom);
   tcase_add_test(tc_core, test_from);
   tcase_add_test(tc_core, test_interned);
   tcase_add_test(tc_core, test_contains);
   tcase_add_test(tc_core, test_len);
   tcase_add_test(tc_core, test_downcase);
   tcase_add_test(tc_core, test_compare);
   tcase_add_test(tc_core, test_walk_selected);
   tcase_add_test(tc_core, test_starts_with);
   tcase_add_test(tc_core, test_istr_r);
   tcase_add_test(tc_core, test_distance);
   suite_add_tcase(s, tc_core);

   return s;
}
