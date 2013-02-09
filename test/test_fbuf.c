#include "fbuf.h"

#include <check.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <time.h>

static void teardown(void)
{
   remove("test.bin");
}

START_TEST(test_basic)
{
   fbuf_t *f = fbuf_open("test.bin", FBUF_OUT);
   fail_if(f == NULL);

   fail_unless(fbuf_open("not.here", FBUF_IN) == NULL);

   write_u8(5, f);
   write_u16(612, f);
   write_u32(71, f);
   write_raw("abc", 4, f);

   fbuf_close(f);

   f = fbuf_open("test.bin", FBUF_IN);
   fail_if(f == NULL);

   fail_unless(read_u8(f) == 5);
   fail_unless(read_u16(f) == 612);
   fail_unless(read_u32(f) == 71);

   char buf[4];
   read_raw(buf, 4, f);
   fail_unless(strcmp(buf, "abc") == 0);

   fbuf_close(f);
}
END_TEST;

int main(void)
{
   srandom((unsigned)time(NULL));

   Suite *s = suite_create("fbuf");

   TCase *tc_core = tcase_create("Core");
   tcase_add_unchecked_fixture(tc_core, NULL, teardown);
   tcase_add_test(tc_core, test_basic);
   suite_add_tcase(s, tc_core);

   SRunner *sr = srunner_create(s);
   srunner_run_all(sr, CK_NORMAL);

   int nfail = srunner_ntests_failed(sr);

   srunner_free(sr);

   return nfail == 0 ? EXIT_SUCCESS : EXIT_FAILURE;
}
