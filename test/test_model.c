//
//  Copyright (C) 2022-2023  Nick Gasson
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
#include "jit/jit.h"
#include "option.h"
#include "phase.h"
#include "rt/model.h"
#include "rt/structs.h"
#include "scan.h"
#include "type.h"

START_TEST(test_basic1)
{
   input_from_file(TESTDIR "/model/basic1.vhd");

   tree_t top = run_elab();
   fail_if(top == NULL);

   jit_t *j = get_jit();
   jit_reset(j);

   rt_model_t *m = model_new(j, NULL);
   create_scope(m, top, NULL);

   tree_t b0 = tree_stmt(top, 0);
   tree_t x = tree_decl(b0, 1);
   fail_unless(tree_kind(x) == T_SIGNAL_DECL);
   tree_t y = tree_decl(b0, 2);
   fail_unless(tree_kind(y) == T_SIGNAL_DECL);

   rt_scope_t *root = find_scope(m, b0);
   fail_if(root == NULL);

   model_reset(m);

   rt_signal_t *xs = find_signal(root, x);
   fail_if(xs == NULL);

   ck_assert_int_eq(xs->n_nexus, 1);
   ck_assert_int_eq(xs->nexus.width, 1);
   ck_assert_int_eq(xs->shared.size, 4);

   const int32_t *xp = signal_value(xs);
   ck_assert_int_eq(*xp, 42);

   rt_signal_t *ys = find_signal(root, y);
   fail_if(ys == NULL);

   const int8_t *yp = signal_value(ys);
   ck_assert_int_eq(*yp, 1);

   model_free(m);

   fail_if_errors();
}
END_TEST

START_TEST(test_index1)
{
   input_from_file(TESTDIR "/model/index1.vhd");

   tree_t top = run_elab();
   fail_if(top == NULL);

   jit_t *j = get_jit();
   jit_reset(j);

   rt_model_t *m = model_new(j, NULL);
   create_scope(m, top, NULL);

   tree_t b0 = tree_stmt(top, 0);
   tree_t s1 = get_decl(b0, "S1");
   tree_t s2 = get_decl(b0, "S2");

   rt_scope_t *root = find_scope(m, b0);
   fail_if(root == NULL);

   model_reset(m);

   rt_signal_t *ss1 = find_signal(root, s1);
   fail_if(ss1 == NULL);

   rt_signal_t *ss2 = find_signal(root, s2);
   fail_if(ss2 == NULL);

   model_run(m, UINT64_MAX);

   ck_assert_int_eq(ss1->n_nexus, 21);
   ck_assert_int_eq(ss1->nexus.width, 8);
   ck_assert_ptr_nonnull(ss1->index);
   ck_assert_int_eq(ss1->index->how, 3);
   ck_assert_ptr_eq(ss1->index->nexus[0], &(ss1->nexus));
   ck_assert_ptr_nonnull(ss1->index->nexus[20]);
   ck_assert_ptr_null(ss1->index->nexus[21]);

   ck_assert_int_eq(ss2->n_nexus, 41);
   ck_assert_int_eq(ss2->nexus.width, 10);
   ck_assert_ptr_nonnull(ss2->index);
   ck_assert_int_eq(ss2->index->how, -10);
   ck_assert_ptr_eq(ss2->index->nexus[0], &(ss2->nexus));
   ck_assert_ptr_nonnull(ss2->index->nexus[40]);
   ck_assert_ptr_null(ss2->index->nexus[41]);

   model_free(m);

   fail_if_errors();
}
END_TEST

START_TEST(test_alias1)
{
   input_from_file(TESTDIR "/model/alias1.vhd");

   tree_t top = run_elab();
   fail_if(top == NULL);

   jit_t *j = get_jit();
   jit_reset(j);

   rt_model_t *m = model_new(j, NULL);
   create_scope(m, top, NULL);
   model_reset(m);

   tree_t b0 = tree_stmt(top, 0);

   rt_scope_t *root = find_scope(m, b0);
   fail_if(root == NULL);

   model_run(m, UINT64_MAX);

   model_free(m);

   fail_if_errors();
}
END_TEST

START_TEST(test_fast1)
{
   input_from_file(TESTDIR "/model/fast1.vhd");

   tree_t top = run_elab();
   fail_if(top == NULL);

   jit_t *j = get_jit();
   jit_reset(j);

   rt_model_t *m = model_new(j, NULL);
   create_scope(m, top, NULL);
   model_reset(m);

   tree_t b0 = tree_stmt(top, 0);

   rt_scope_t *root = find_scope(m, b0);
   fail_if(root == NULL);

   rt_signal_t *sx = find_signal(root, get_decl(b0, "X"));
   fail_if(sx == NULL);
   fail_unless(sx->n_nexus == 1);
   fail_unless(sx->nexus.flags & NET_F_FAST_DRIVER);

   fail_if(model_step(m));

   fail_unless(sx->nexus.flags & NET_F_FAST_DRIVER);
   fail_unless(sx->nexus.sources.fastqueued);
   ck_assert_int_eq(*(int32_t *)sx->shared.data, 0);

   fail_if(model_step(m));

   unsigned deltas;
   ck_assert_int_eq(model_now(m, &deltas), 0);
   ck_assert_int_eq(deltas, 1);

   fail_if(sx->nexus.flags & NET_F_FAST_DRIVER);
   fail_if(sx->nexus.sources.fastqueued);
   ck_assert_int_eq(*(int32_t *)sx->shared.data, 1);

   fail_unless(model_step(m));

   ck_assert_int_eq(model_now(m, &deltas), 1000000);
   ck_assert_int_eq(deltas, 0);

   model_free(m);

   fail_if_errors();
}
END_TEST

START_TEST(test_stateless1)
{
   input_from_file(TESTDIR "/model/stateless1.vhd");

   tree_t top = run_elab();
   fail_if(top == NULL);

   jit_t *j = get_jit();
   jit_reset(j);

   rt_model_t *m = model_new(j, NULL);
   create_scope(m, top, NULL);
   model_reset(m);

   tree_t b0 = tree_stmt(top, 0);
   ck_assert_int_eq(tree_stmts(b0), 2);

   rt_scope_t *root = find_scope(m, b0);
   fail_if(root == NULL);

   rt_proc_t *p1 = find_proc(root, tree_stmt(b0, 0));
   ck_assert_ptr_nonnull(p1);
   ck_assert_str_eq(istr(p1->name), ":stateless1:p1");
   ck_assert_ptr_null(*mptr_get(p1->privdata));

   rt_proc_t *p2 = find_proc(root, tree_stmt(b0, 1));
   ck_assert_ptr_nonnull(p2);
   ck_assert_str_eq(istr(p2->name), ":stateless1:p2");
   ck_assert_ptr_nonnull(*mptr_get(p2->privdata));

   model_free(m);

   fail_if_errors();
}
END_TEST

START_TEST(test_pending1)
{
   input_from_file(TESTDIR "/model/pending1.vhd");

   tree_t top = run_elab();
   fail_if(top == NULL);

   jit_t *j = get_jit();
   jit_reset(j);

   rt_model_t *m = model_new(j, NULL);
   create_scope(m, top, NULL);
   model_reset(m);

   tree_t b0 = tree_stmt(top, 0);
   ck_assert_int_eq(tree_stmts(b0), 2);

   rt_scope_t *root = find_scope(m, b0);
   fail_if(root == NULL);

   rt_proc_t *pwakeup = find_proc(root, tree_stmt(b0, 0));
   ck_assert_ptr_nonnull(pwakeup);
   ck_assert_str_eq(istr(pwakeup->name), ":pending1:wakeup");

   rt_proc_t *pstim = find_proc(root, tree_stmt(b0, 1));
   ck_assert_ptr_nonnull(pstim);
   ck_assert_str_eq(istr(pstim->name), ":pending1:stim");

   rt_signal_t *sx = find_signal(root, get_decl(b0, "X"));
   fail_if(sx == NULL);
   fail_unless(sx->n_nexus == 1);

   ck_assert_ptr_null(sx->nexus.pending);

   model_step(m);

   ck_assert_int_eq(pointer_tag(sx->nexus.pending), 1);
   ck_assert_ptr_eq(untag_pointer(sx->nexus.pending, rt_wakeable_t),
                    &(pwakeup->wakeable));

   model_step(m);

   ck_assert_int_eq(pointer_tag(sx->nexus.pending), 1);
   ck_assert_ptr_eq(untag_pointer(sx->nexus.pending, rt_wakeable_t),
                    &(pwakeup->wakeable));

   model_free(m);

   fail_if_errors();
}
END_TEST

START_TEST(test_fast2)
{
   input_from_file(TESTDIR "/model/fast2.vhd");

   tree_t top = run_elab();
   fail_if(top == NULL);

   jit_t *j = get_jit();
   jit_reset(j);

   rt_model_t *m = model_new(j, NULL);
   create_scope(m, top, NULL);
   model_reset(m);

   tree_t b0 = tree_stmt(top, 0);

   rt_scope_t *root = find_scope(m, b0);
   fail_if(root == NULL);

   rt_signal_t *sx = find_signal(root, get_decl(b0, "X"));
   fail_if(sx == NULL);
   ck_assert_int_eq(sx->n_nexus, 1);
   fail_unless(sx->nexus.flags & NET_F_FAST_DRIVER);
   fail_if(sx->shared.flags & NET_F_FAST_DRIVER);

   rt_source_t *d0 = &(sx->nexus.sources);
   fail_unless(d0->tag == SOURCE_DRIVER);
   fail_if(d0->fastqueued);
   fail_if(d0->sigqueued);

   fail_if(model_step(m));

   unsigned deltas;
   ck_assert_int_eq(model_now(m, &deltas), 0);
   ck_assert_int_eq(deltas, 0);

   ck_assert_int_eq(sx->n_nexus, 8);
   fail_unless(d0->fastqueued);
   fail_if(d0->sigqueued);
   fail_unless(sx->shared.flags & NET_F_FAST_DRIVER);

   fail_if(model_step(m));

   ck_assert_int_eq(model_now(m, &deltas), 0);
   ck_assert_int_eq(deltas, 1);

   ck_assert_int_eq(sx->n_nexus, 8);
   fail_if(d0->fastqueued);
   fail_if(d0->sigqueued);
   fail_unless(sx->shared.flags & NET_F_FAST_DRIVER);

   fail_if(model_step(m));

   ck_assert_int_eq(model_now(m, &deltas), 1000000);
   ck_assert_int_eq(deltas, 0);

   fail_unless(d0->fastqueued);
   fail_unless(d0->sigqueued);
   fail_unless(sx->shared.flags & NET_F_FAST_DRIVER);

   fail_if(model_step(m));

   fail_if(d0->fastqueued);
   fail_if(d0->sigqueued);
   fail_unless(sx->shared.flags & NET_F_FAST_DRIVER);

   fail_if(model_step(m));

   fail_if(d0->fastqueued);
   fail_unless(d0->sigqueued);
   fail_unless(sx->nexus.chain->sources.fastqueued);
   fail_if(sx->nexus.chain->sources.sigqueued);
   fail_unless(sx->shared.flags & NET_F_FAST_DRIVER);

   fail_unless(model_step(m));

   fail_if(d0->fastqueued);
   fail_if(d0->sigqueued);
   fail_if(sx->shared.flags & NET_F_FAST_DRIVER);   // Not profitable

   model_free(m);

   fail_if_errors();
}
END_TEST

START_TEST(test_event1)
{
   input_from_file(TESTDIR "/model/event1.vhd");

   tree_t top = run_elab();
   fail_if(top == NULL);

   jit_t *j = get_jit();
   jit_reset(j);

   rt_model_t *m = model_new(j, NULL);
   create_scope(m, top, NULL);
   model_reset(m);

   tree_t b0 = tree_stmt(top, 0);

   rt_scope_t *root = find_scope(m, b0);
   fail_if(root == NULL);

   rt_signal_t *ss = find_signal(root, get_decl(b0, "S"));
   fail_if(ss == NULL);
   fail_if(ss->shared.flags & SIG_F_CACHE_EVENT);

   rt_signal_t *st = find_signal(root, get_decl(b0, "T"));
   fail_if(st == NULL);
   fail_if(st->shared.flags & SIG_F_CACHE_EVENT);

   fail_if(model_step(m));

   fail_unless(ss->shared.flags & SIG_F_CACHE_EVENT);
   fail_if(ss->shared.flags & SIG_F_EVENT_FLAG);
   fail_if(st->shared.flags & SIG_F_CACHE_EVENT);
   fail_if(st->shared.flags & SIG_F_EVENT_FLAG);

   fail_if(model_step(m));

   fail_unless(ss->shared.flags & SIG_F_CACHE_EVENT);
   fail_unless(ss->shared.flags & SIG_F_EVENT_FLAG);
   fail_if(st->shared.flags & SIG_F_CACHE_EVENT);
   fail_if(st->shared.flags & SIG_F_EVENT_FLAG);

   fail_unless(model_step(m));

   fail_unless(ss->shared.flags & SIG_F_CACHE_EVENT);
   fail_if(ss->shared.flags & SIG_F_EVENT_FLAG);
   fail_if(st->shared.flags & SIG_F_CACHE_EVENT);
   fail_if(st->shared.flags & SIG_F_EVENT_FLAG);

   model_free(m);

   fail_if_errors();
}
END_TEST

START_TEST(test_process1)
{
   input_from_file(TESTDIR "/model/process1.vhd");

   const error_t expect[] = {
      { 10, "hello, world" },
      { 13, "after 1 ns" },
      { -1, NULL },
   };
   expect_errors(expect);

   tree_t a = parse_check_and_simplify(T_ENTITY, T_ARCH);

   mir_context_t *mc = get_mir();
   unit_registry_t *ur = get_registry();
   jit_t *j = jit_new(ur, mc, NULL);
   rt_model_t *m = model_new(j, NULL);

   tree_t top = elab(tree_to_object(a), j, ur, mc, NULL, NULL, m);
   ck_assert_ptr_nonnull(top);

   model_reset(m);

   tree_t b0 = tree_stmt(top, 0);

   rt_scope_t *root = find_scope(m, b0);
   ck_assert_ptr_nonnull(root);

   rt_proc_t *p1 = find_proc(root, tree_stmt(b0, 0));
   ck_assert_ptr_nonnull(p1);

   void *p1_state = *mptr_get(p1->privdata);
   ck_assert_ptr_nonnull(p1_state);

   int32_t *fsm_ptr = p1_state + 2*sizeof(void *);
   ck_assert_int_eq(*fsm_ptr, 1);

   int32_t *x_ptr = p1_state + 2*sizeof(void *) + sizeof(int32_t);
   ck_assert_int_eq(*x_ptr, INT32_MIN);

   model_step(m);

   ck_assert_int_eq(*fsm_ptr, 2);
   ck_assert_int_eq(*x_ptr, 42);

   model_step(m);

   ck_assert_int_eq(*fsm_ptr, 3);
   ck_assert_int_eq(*x_ptr, 43);

   model_free(m);
   jit_free(j);

   check_expected_errors();
}
END_TEST

START_TEST(test_split1)
{
   set_standard(STD_08);

   input_from_file(TESTDIR "/model/split1.vhd");

   tree_t top = run_elab();
   fail_if(top == NULL);

   jit_t *j = get_jit();
   jit_reset(j);

   rt_model_t *m = model_new(j, NULL);
   create_scope(m, top, NULL);
   model_reset(m);

   tree_t b0 = tree_stmt(top, 0);

   rt_scope_t *root = find_scope(m, b0);
   fail_if(root == NULL);

   rt_signal_t *ss = find_signal(root, get_decl(b0, "S"));
   fail_if(ss == NULL);

   ck_assert_int_eq(ss->n_nexus, 1);

   fail_if(model_step(m));

   ck_assert_int_eq(ss->n_nexus, 3);
   ck_assert_int_eq(ss->nexus.width, 3);
   ck_assert_int_eq(ss->nexus.chain->width, 1);
   ck_assert_int_eq(ss->nexus.chain->chain->width, 4);

   model_free(m);

   fail_if_errors();
}
END_TEST

Suite *get_model_tests(void)
{
   Suite *s = suite_create("model");

   TCase *tc = nvc_unit_test();
   tcase_add_test(tc, test_basic1);
   tcase_add_test(tc, test_index1);
   tcase_add_test(tc, test_alias1);
   tcase_add_test(tc, test_fast1);
   tcase_add_test(tc, test_stateless1);
   tcase_add_test(tc, test_pending1);
   tcase_add_test(tc, test_fast2);
   tcase_add_test(tc, test_event1);
   tcase_add_test(tc, test_process1);
   tcase_add_test(tc, test_split1);
   suite_add_tcase(s, tc);

   return s;
}
