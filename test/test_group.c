#include "util.h"

#include <check.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <time.h>

#include "../src/group.c"

typedef struct {
   int  first;
   int  last;
} group_expect_t;

void cover_tag(void)
{
   assert(false);
}

static void setup(void)
{
   const char *lib_dir = getenv("LIB_DIR");
   if (lib_dir)
      lib_add_search_path(lib_dir);

   lib_set_work(lib_tmp());
   opt_set_int("bootstrap", 0);
   opt_set_int("cover", 0);
   opt_set_int("unit-test", 1);
   opt_set_int("relax", 0);
}

static void teardown(void)
{
   lib_free(lib_work());
}

static tree_t run_elab(void)
{
   tree_t t, last_ent = NULL;
   while ((t = parse())) {
      sem_check(t);
      fail_if(sem_errors() > 0);

      simplify(t);

      if (tree_kind(t) == T_ENTITY)
         last_ent = t;
   }

   return elab(last_ent);
}

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

static void group_expect(group_nets_ctx_t *ctx, const group_expect_t *expect,
                         int n_expect)
{
   int ngroups = 0;
   for (group_t *it = ctx->groups; it != NULL; it = it->next)
      ngroups++;

   const int n_expect_orig = n_expect;

   for (; n_expect-- > 0; expect++) {
      const int length = expect->last - expect->first + 1;

      bool found = false;
      for (group_t *it = ctx->groups; (it != NULL) && !found; it = it->next) {
         if ((it->first == expect->first) && (it->length == length))
            found = true;
      }

      if (!found) {
         group_dump(ctx);
         fail("missing expected group %d..%d", expect->first, expect->last);
      }
   }

   if (ngroups != n_expect_orig) {
      group_dump(ctx);
      fail("expected %d groups but have %d", n_expect_orig, ngroups);
   }
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

START_TEST(test_group_six)
{
   group_nets_ctx_t ctx = {
      .groups   = NULL,
      .next_gid = 0
   };

   group_add(&ctx, 0, 8);
   group_add(&ctx, 1, 8);

   fail_unless(group_sanity_check(&ctx, 8));
}
END_TEST

START_TEST(test_issue72)
{
   input_from_file(TESTDIR "/group/issue72.vhd");

   tree_t top = run_elab();

   group_nets_ctx_t ctx = {
      .groups   = NULL,
      .next_gid = 0
   };
   tree_visit(top, group_nets_visit_fn, &ctx);

   const int nnets = tree_attr_int(top, ident_new("nnets"), 0);
   fail_unless(group_sanity_check(&ctx, nnets - 1));

   const group_expect_t expect[] = {
      { 0, 1 }, { 2, 3 }, { 4, 5 }, { 6, 7 }
   };
   group_expect(&ctx, expect, ARRAY_LEN(expect));
}
END_TEST

START_TEST(test_issue73)
{
   input_from_file(TESTDIR "/group/issue73.vhd");

   tree_t top = run_elab();

   group_nets_ctx_t ctx = {
      .groups   = NULL,
      .next_gid = 0
   };
   tree_visit(top, group_nets_visit_fn, &ctx);

   const int nnets = tree_attr_int(top, ident_new("nnets"), 0);
   fail_unless(group_sanity_check(&ctx, nnets - 1));

   const group_expect_t expect[] = {
      { 0, 2 }, { 3, 3 }, { 4, 5 }, { 6, 7 },  // X
      { 8, 8 }, { 9, 9 }, { 10, 10 }, { 11, 11 }, { 12, 12 }, { 13, 13 },
      { 14, 14 }, { 15, 15}   // Y (sub-optimal!)
   };
   group_expect(&ctx, expect, ARRAY_LEN(expect));
}
END_TEST

START_TEST(test_slice1)
{
   input_from_file(TESTDIR "/group/slice1.vhd");

   tree_t top = run_elab();

   group_nets_ctx_t ctx = {
      .groups   = NULL,
      .next_gid = 0
   };
   tree_visit(top, group_nets_visit_fn, &ctx);

   const int nnets = tree_attr_int(top, ident_new("nnets"), 0);
   fail_unless(group_sanity_check(&ctx, nnets - 1));

   const group_expect_t expect[] = {
      { 0, 3 }, { 4, 4 }, { 5, 5 }, { 6, 7 },
      { 12, 15 }, { 8, 9 }, { 10, 10 }, { 11, 11 }
   };

   group_expect(&ctx, expect, ARRAY_LEN(expect));
}
END_TEST

START_TEST(test_arrayref1)
{
   input_from_file(TESTDIR "/group/arrayref1.vhd");

   tree_t top = run_elab();

   group_nets_ctx_t ctx = {
      .groups   = NULL,
      .next_gid = 0
   };
   tree_visit(top, group_nets_visit_fn, &ctx);

   const int nnets = tree_attr_int(top, ident_new("nnets"), 0);
   fail_unless(group_sanity_check(&ctx, nnets - 1));

   const group_expect_t expect[] = {
      { 1, 1 }, { 0, 0 }, { 2, 2 },        // X
      { 3, 3 }, { 4, 4 },                  // Y
      { 5, 5 },                            // I
      { 6, 6 }, { 7, 7 }, { 8, 9 },        // P
      { 10, 11 }, { 12, 13 }, { 14, 15 },  // Q
      { 16, 19 }, { 21, 21 }, { 20, 20 }   // R
   };

   group_expect(&ctx, expect, ARRAY_LEN(expect));
}
END_TEST

START_TEST(test_issue95)
{
   input_from_file(TESTDIR "/group/issue95.vhd");

   tree_t top = run_elab();

   group_nets_ctx_t ctx = {
      .groups   = NULL,
      .next_gid = 0
   };
   tree_visit(top, group_nets_visit_fn, &ctx);

   const int nnets = tree_attr_int(top, ident_new("nnets"), 0);
   fail_unless(group_sanity_check(&ctx, nnets - 1));

   const group_expect_t expect[] = {
      { 5, 5 }, { 4, 4 }, { 3, 3 }, { 2, 2 }, { 1, 1 }, { 0, 0 }
   };

   group_expect(&ctx, expect, ARRAY_LEN(expect));
}
END_TEST

int main(void)
{
   srandom((unsigned)time(NULL));

   Suite *s = suite_create("group");

   TCase *tc_core = tcase_create("Core");
   tcase_add_unchecked_fixture(tc_core, setup, teardown);
   tcase_add_test(tc_core, test_group_one);
   tcase_add_test(tc_core, test_group_two);
   tcase_add_test(tc_core, test_group_three);
   tcase_add_test(tc_core, test_group_four);
   tcase_add_test(tc_core, test_group_five);
   tcase_add_test(tc_core, test_group_six);
   tcase_add_test(tc_core, test_issue72);
   tcase_add_test(tc_core, test_issue73);
   tcase_add_test(tc_core, test_slice1);
   tcase_add_test(tc_core, test_arrayref1);
   tcase_add_test(tc_core, test_issue95);
   suite_add_tcase(s, tc_core);

   SRunner *sr = srunner_create(s);
   srunner_run_all(sr, CK_NORMAL);

   int nfail = srunner_ntests_failed(sr);

   srunner_free(sr);

   return nfail == 0 ? EXIT_SUCCESS : EXIT_FAILURE;
}
