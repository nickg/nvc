//
//  Copyright (C) 2022  Nick Gasson
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
#include "opt.h"
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

   lower_unit(top, NULL);

   jit_t *j = jit_new();
   jit_enable_runtime(j, true);

   rt_model_t *m = model_new(top, j);

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
   jit_free(j);

   fail_if_errors();
}
END_TEST

Suite *get_model_tests(void)
{
   Suite *s = suite_create("model");

   TCase *tc = nvc_unit_test();
   tcase_add_test(tc, test_basic1);
   suite_add_tcase(s, tc);

   return s;
}
