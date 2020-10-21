#include "ident.h"
#include "test_util.h"

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

START_TEST(test_compare)
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

   fbuf_t *f = fbuf_open("test.ident", FBUF_OUT);
   fail_if(f == NULL);

   ident_wr_ctx_t wctx = ident_write_begin(f);

   ident_write(i1, wctx);
   ident_write(i2, wctx);
   ident_write(i3, wctx);

   ident_write_end(wctx);

   fbuf_close(f);

   f = fbuf_open("test.ident", FBUF_IN);
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

   fbuf_close(f);

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

START_TEST(test_suffix_until)
{
   ident_t i1 = ident_new("foobar.bar.baz");
   ident_t i2 = ident_new("foobar.'.'.baz");
   ident_t i3 = ident_new("foobar.'.'");
   ident_t i4 = ident_new("foobar");
   ident_t i5 = ident_new("foobar.bar");

   fail_unless(ident_suffix_until(i1, '.', NULL, '\0') == i4);
   fail_unless(ident_suffix_until(i1, '.', i4, '\0') == i5);
   fail_unless(ident_suffix_until(i2, '.', i4, '\'') == i3);
}
END_TEST

Suite *get_ident_tests(void)
{
   Suite *s = suite_create("ident");

   TCase *tc_core = nvc_unit_test();
   tcase_add_test(tc_core, test_ident_new);
   tcase_add_test(tc_core, test_compare);
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
   tcase_add_test(tc_core, test_suffix_until);
   suite_add_tcase(s, tc_core);

   return s;
}
