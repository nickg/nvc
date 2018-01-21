#include "util.h"
#include "test_util.h"

#include <check.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "../src/group.c"

#define DEFAULT_NNETS 256

typedef struct {
   int  first;
   int  last;
} group_expect_t;

static void group_dump(group_nets_ctx_t *ctx)
{
   printf("-------------\n");
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

static void group_test_init(group_nets_ctx_t *ctx, tree_t top)
{
   const int nnets = top ? tree_attr_int(top, nnets_i, 0) : DEFAULT_NNETS;
   group_init_context(ctx, nnets);
}

START_TEST(test_group_one)
{
   group_nets_ctx_t ctx;
   group_test_init(&ctx, NULL);

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
   group_nets_ctx_t ctx;
   group_test_init(&ctx, NULL);

   fail_unless(group_add(&ctx, 0, 4) == 0);
   fail_if(group_add(&ctx, 1, 2) == GROUPID_INVALID);

   fail_unless(group_sanity_check(&ctx, 3));
}
END_TEST

START_TEST(test_group_three)
{
   group_nets_ctx_t ctx;
   group_test_init(&ctx, NULL);

   group_add(&ctx, 0, 5);
   group_add(&ctx, 1, 4);

   fail_unless(group_sanity_check(&ctx, 4));
}
END_TEST

START_TEST(test_group_four)
{
   group_nets_ctx_t ctx;
   group_test_init(&ctx, NULL);

   group_add(&ctx, 0, 5);
   group_add(&ctx, 0, 4);

   fail_unless(group_sanity_check(&ctx, 4));
}
END_TEST

START_TEST(test_group_five)
{
   group_nets_ctx_t ctx;
   group_test_init(&ctx, NULL);

   group_add(&ctx, 2, 4);
   group_add(&ctx, 0, 4);

   fail_unless(group_sanity_check(&ctx, 5));
}
END_TEST

START_TEST(test_group_six)
{
   group_nets_ctx_t ctx;
   group_test_init(&ctx, NULL);

   group_add(&ctx, 0, 8);
   group_add(&ctx, 1, 8);

   fail_unless(group_sanity_check(&ctx, 8));
}
END_TEST

START_TEST(test_issue72)
{
   input_from_file(TESTDIR "/group/issue72.vhd");

   tree_t top = run_elab();

   group_nets_ctx_t ctx;
   group_test_init(&ctx, NULL);
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

   group_nets_ctx_t ctx;
   group_test_init(&ctx, NULL);
   tree_visit(top, group_nets_visit_fn, &ctx);

   const int nnets = tree_attr_int(top, ident_new("nnets"), 0);
   fail_unless(group_sanity_check(&ctx, nnets - 1));

   const group_expect_t expect[] = {
      { 0, 2 }, { 3, 3 }, { 4, 5 }, { 6, 7 },       // X
      { 8, 9 }, { 10, 11 }, { 12, 13 }, { 14, 15 }  // Y
   };
   group_expect(&ctx, expect, ARRAY_LEN(expect));
}
END_TEST

START_TEST(test_slice1)
{
   input_from_file(TESTDIR "/group/slice1.vhd");

   tree_t top = run_elab();

   group_nets_ctx_t ctx;
   group_test_init(&ctx, NULL);
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

   group_nets_ctx_t ctx;
   group_test_init(&ctx, NULL);
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
   fail_if(top == NULL);

   group_nets_ctx_t ctx;
   group_test_init(&ctx, NULL);
   tree_visit(top, group_nets_visit_fn, &ctx);

   const int nnets = tree_attr_int(top, ident_new("nnets"), 0);
   fail_unless(group_sanity_check(&ctx, nnets - 1));

   const group_expect_t expect[] = {
      { 5, 5 }, { 4, 4 }, { 3, 3 }, { 2, 2 }, { 1, 1 }, { 0, 0 }
   };

   group_expect(&ctx, expect, ARRAY_LEN(expect));
}
END_TEST

START_TEST(test_arrayref2)
{
   input_from_file(TESTDIR "/group/arrayref2.vhd");

   tree_t top = run_elab();
   fail_if(top == NULL);

   group_nets_ctx_t ctx;
   group_test_init(&ctx, NULL);
   tree_visit(top, group_nets_visit_fn, &ctx);

   const int nnets = tree_attr_int(top, ident_new("nnets"), 0);
   fail_unless(group_sanity_check(&ctx, nnets - 1));

   const group_expect_t expect[] = {
      { 0, 3 }, { 4, 4 }, { 5, 6 }, { 8, 8 }, { 7, 7 }, { 9, 16 },
      { 17, 18 }, { 19, 22 }, { 25, 26 }, { 23, 24 }
   };

   group_expect(&ctx, expect, ARRAY_LEN(expect));
}
END_TEST

START_TEST(test_recref1)
{
   input_from_file(TESTDIR "/group/recref1.vhd");

   tree_t top = run_elab();
   fail_if(top == NULL);

   group_nets_ctx_t ctx;
   group_test_init(&ctx, NULL);
   tree_visit(top, group_nets_visit_fn, &ctx);

   const int nnets = tree_attr_int(top, ident_new("nnets"), 0);
   fail_unless(group_sanity_check(&ctx, nnets - 1));

   const group_expect_t expect[] = {
      { 0, 2 }, { 3, 3 }, { 4, 6 }, { 7, 7 }, { 8, 10 }, { 11, 11 }
   };

   group_expect(&ctx, expect, ARRAY_LEN(expect));
}
END_TEST

START_TEST(test_array3)
{
   input_from_file(TESTDIR "/group/array3.vhd");

   tree_t top = run_elab();
   fail_if(top == NULL);

   group_nets_ctx_t ctx;
   group_test_init(&ctx, NULL);
   tree_visit(top, group_nets_visit_fn, &ctx);

   const int nnets = tree_attr_int(top, ident_new("nnets"), 0);
   fail_unless(group_sanity_check(&ctx, nnets - 1));

   const group_expect_t expect[] = {
      { 6, 6 }, { 7, 7 }, { 5, 5 }, { 0, 4 }
   };

   group_expect(&ctx, expect, ARRAY_LEN(expect));
}
END_TEST

START_TEST(test_issue250)
{
   input_from_file(TESTDIR "/group/issue250.vhd");

   tree_t top = run_elab();
   fail_if(top == NULL);

   group_nets_ctx_t ctx;
   group_test_init(&ctx, NULL);
   tree_visit(top, group_nets_visit_fn, &ctx);

   const int nnets = tree_attr_int(top, ident_new("nnets"), 0);
   fail_unless(group_sanity_check(&ctx, nnets - 1));

   const group_expect_t expect[] = {
      { 0, 1 }, { 2, 2 }, { 3, 4 }, { 5, 5 }, { 6, 7 }, { 13, 13 },
      { 12, 12 }, { 11, 11 }, { 8, 8 }, { 9, 10 }
   };

   group_expect(&ctx, expect, ARRAY_LEN(expect));
}
END_TEST

START_TEST(test_recref2)
{
   input_from_file(TESTDIR "/group/recref2.vhd");

   tree_t top = run_elab();
   fail_if(top == NULL);

   group_nets_ctx_t ctx;
   group_test_init(&ctx, NULL);
   tree_visit(top, group_nets_visit_fn, &ctx);

   const int nnets = tree_attr_int(top, ident_new("nnets"), 0);
   fail_unless(group_sanity_check(&ctx, nnets - 1));

   const group_expect_t expect[] = {
      { 9, 9 }, { 8, 8 }, { 6, 7 }, { 4, 5 }, { 2, 3 }, { 0, 1 }
   };

   group_expect(&ctx, expect, ARRAY_LEN(expect));
}
END_TEST

START_TEST(test_arrayref3)
{
   input_from_file(TESTDIR "/group/arrayref3.vhd");

   tree_t top = run_elab();
   fail_if(top == NULL);

   group_nets_ctx_t ctx;
   group_test_init(&ctx, NULL);
   tree_visit(top, group_nets_visit_fn, &ctx);

   const int nnets = tree_attr_int(top, ident_new("nnets"), 0);
   fail_unless(group_sanity_check(&ctx, nnets - 1));

   const group_expect_t expect[] = {
      { 14, 15 }, { 12, 13 }, { 10, 11 }, { 8, 9 }, { 6, 7 },
      { 4, 5 }, { 2, 3 }, { 0, 1 }, { 16, 16 }
   };

   group_expect(&ctx, expect, ARRAY_LEN(expect));
}
END_TEST

START_TEST(test_jcore2)
{
   input_from_file(TESTDIR "/group/jcore2.vhd");

   tree_t top = run_elab();
   fail_if(top == NULL);

   group_nets_ctx_t ctx;
   group_test_init(&ctx, NULL);
   tree_visit(top, group_nets_visit_fn, &ctx);

   const int nnets = tree_attr_int(top, ident_new("nnets"), 0);
   fail_unless(group_sanity_check(&ctx, nnets - 1));

   const group_expect_t expect[] = {
      { 0, 31 }, { 32, 32 }, { 33, 64 }, { 65, 65 },
      { 66, 97 }, { 98, 98 }, { 99, 130 }
   };

   group_expect(&ctx, expect, ARRAY_LEN(expect));
}
END_TEST

START_TEST(test_jcore4)
{
   input_from_file(TESTDIR "/group/jcore4.vhd");

   tree_t top = run_elab();
   fail_if(top == NULL);

   group_nets_ctx_t ctx;
   group_test_init(&ctx, NULL);
   tree_visit(top, group_nets_visit_fn, &ctx);

   const int nnets = tree_attr_int(top, ident_new("nnets"), 0);
   fail_unless(group_sanity_check(&ctx, nnets - 1));

   const group_expect_t expect[] = {
      { 6, 8 }, { 3, 5 }, { 0, 2 }
   };

   group_expect(&ctx, expect, ARRAY_LEN(expect));
}
END_TEST

Suite *get_group_tests(void)
{
   Suite *s = suite_create("group");

   TCase *tc_core = nvc_unit_test();
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
   tcase_add_test(tc_core, test_arrayref2);
   tcase_add_test(tc_core, test_recref1);
   tcase_add_test(tc_core, test_array3);
   tcase_add_test(tc_core, test_issue250);
   tcase_add_test(tc_core, test_recref2);
   tcase_add_test(tc_core, test_arrayref3);
   tcase_add_test(tc_core, test_jcore2);
   tcase_add_test(tc_core, test_jcore4);
   suite_add_tcase(s, tc_core);

   return s;
}
