#include "util.h"

#include <check.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <time.h>

#include "../src/group.c"

static void group_dump(group_nets_ctx_t *ctx)
{
   for (group_t *it = ctx->groups; it != NULL; it = it->next)
      printf("%3d : %d..%d\n", it->gid, it->first, it->first + it->length - 1);
}

static bool group_sanity_check(group_nets_ctx_t *ctx, netid_t max)
{
   bool error = false;

   for (netid_t i = 0; i <= max; i++) {
      bool have = false;
      for (group_t *it = ctx->groups; it != NULL; it = it->next) {
         if ((i >= it->first) && (i < it->first + it->length)) {
            if (have) {
               printf("net %d appears in multiple groups\n", i);
               error = true;
            }
            have = true;
         }
      }

      if (!have) {
         printf("net %d in no group\n", i);
         error = true;
      }
   }

   if (error)
      group_dump(ctx);

   return !error;
}

START_TEST(test_group_one)
{
   group_nets_ctx_t ctx = {
      .groups   = NULL,
      .next_gid = 0
   };

   fail_unless(group_add(&ctx, 0, 1) == 0);
   fail_unless(group_add(&ctx, 2, 1) == 1);
   fail_unless(group_add(&ctx, 3, 1) == 2);

   fail_unless(group_add(&ctx, 0, 1) == 0);

   fail_unless(group_add(&ctx, 0, 5) == GROUPID_INVALID);

   fail_unless(ctx.next_gid == 5);

   fail_unless(group_sanity_check(&ctx, 4));
}
END_TEST

START_TEST(test_group_two)
{
   group_nets_ctx_t ctx = {
      .groups   = NULL,
      .next_gid = 0
   };

   fail_unless(group_add(&ctx, 0, 4) == 0);
   fail_if(group_add(&ctx, 1, 2) == GROUPID_INVALID);

   fail_unless(group_sanity_check(&ctx, 3));
}
END_TEST

START_TEST(test_group_three)
{
   group_nets_ctx_t ctx = {
      .groups   = NULL,
      .next_gid = 0
   };

   group_add(&ctx, 0, 5);
   group_add(&ctx, 1, 4);

   fail_unless(group_sanity_check(&ctx, 4));
}
END_TEST

START_TEST(test_group_four)
{
   group_nets_ctx_t ctx = {
      .groups   = NULL,
      .next_gid = 0
   };

   group_add(&ctx, 0, 5);
   group_add(&ctx, 0, 4);

   fail_unless(group_sanity_check(&ctx, 4));
}
END_TEST

START_TEST(test_group_five)
{
   group_nets_ctx_t ctx = {
      .groups   = NULL,
      .next_gid = 0
   };

   group_add(&ctx, 2, 4);
   group_add(&ctx, 0, 4);

   fail_unless(group_sanity_check(&ctx, 5));
}
END_TEST

int main(void)
{
   srandom((unsigned)time(NULL));

   Suite *s = suite_create("group");

   TCase *tc_core = tcase_create("Core");
   tcase_add_test(tc_core, test_group_one);
   tcase_add_test(tc_core, test_group_two);
   tcase_add_test(tc_core, test_group_three);
   tcase_add_test(tc_core, test_group_four);
   tcase_add_test(tc_core, test_group_five);
   suite_add_tcase(s, tc_core);

   SRunner *sr = srunner_create(s);
   srunner_run_all(sr, CK_NORMAL);

   int nfail = srunner_ntests_failed(sr);

   srunner_free(sr);

   return nfail == 0 ? EXIT_SUCCESS : EXIT_FAILURE;
}
