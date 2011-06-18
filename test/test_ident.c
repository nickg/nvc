#include "ident.h"

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
   i2 = ident_new("FOO");
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
   fail_unless(strcasecmp(istr(i1), "frob") == 0);
   
   i1 = ident_new("pingu");
   fail_unless(strcasecmp(istr(i1), "PINGU") == 0);
}
END_TEST

START_TEST(test_rand)
{
   for (int i = 0; i < 10000; i++) {
      char buf[16];
      size_t len = (random() % (sizeof(buf) - 3)) + 2;

      for (size_t j = 0; j < len; j++)
         buf[j] = '0' + (random() % 80);
      buf[len - 1] = '\0';
      
      ident_t i1 = ident_new(buf);
      fail_if(i1 == NULL);
      fail_unless(strcasecmp(istr(i1), buf) == 0);
   }
}
END_TEST

START_TEST(test_read_write)
{
   ident_t i1, i2, i3;
   i1 = ident_new("goobar");
   i2 = ident_new("foo");
   i3 = ident_new("FOO");

   FILE *f = tmpfile();
   fail_if(f == NULL);
   
   ident_write(i1, f);
   ident_write(i2, f);
   ident_write(i3, f);

   rewind(f);

   ident_t j1, j2, j3;
   j1 = ident_read(f);
   j2 = ident_read(f);
   j3 = ident_read(f);

   fail_unless(i1 == j1);
   fail_unless(i2 == j2);
   fail_unless(i3 == j3);
   
   fail_unless(j2 == j3);

   fclose(f);
}
END_TEST

int main(void)
{
   srandom((unsigned)time(NULL));
   
   Suite *s = suite_create("ident");

   TCase *tc_core = tcase_create("Core");
   tcase_add_test(tc_core, test_ident_new);
   tcase_add_test(tc_core, test_compare);
   tcase_add_test(tc_core, test_istr);
   tcase_add_test(tc_core, test_rand);
   tcase_add_test(tc_core, test_read_write);
   suite_add_tcase(s, tc_core);
   
   SRunner *sr = srunner_create(s);
   srunner_run_all(sr, CK_NORMAL);

   int nfail = srunner_ntests_failed(sr);

   srunner_free(sr);
   
   return nfail == 0 ? EXIT_SUCCESS : EXIT_FAILURE;
}

