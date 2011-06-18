#include "lib.h"

#include <check.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

static lib_t work;

static void setup(void)
{
   work = lib_new("work");
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

START_TEST(test_lib_new)
{
   fail_if(work == NULL);
}
END_TEST

START_TEST(test_lib_fopen)
{
   FILE *f = lib_fopen(work, "test", "w");
   fprintf(f, "hello world");
   fclose(f);

   lib_free(work);

   work = lib_find("work");
   fail_if(work == NULL);   

   f = lib_fopen(work, "test", "r");
   char buf[12];
   fgets(buf, sizeof(buf), f);

   fail_unless(strcmp(buf, "hello world") == 0);

   fclose(f);
}
END_TEST

int main(void)
{
   Suite *s = suite_create("lib");

   TCase *tc_core = tcase_create("Core");
   tcase_add_unchecked_fixture(tc_core, setup, teardown);
   tcase_add_test(tc_core, test_lib_new);
   tcase_add_test(tc_core, test_lib_fopen);
   suite_add_tcase(s, tc_core);
   
   SRunner *sr = srunner_create(s);
   srunner_run_all(sr, CK_NORMAL);

   int nfail = srunner_ntests_failed(sr);

   srunner_free(sr);
   
   return nfail == 0 ? EXIT_SUCCESS : EXIT_FAILURE;
}

