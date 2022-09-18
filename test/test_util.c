//
//  Copyright (C) 2014-2022  Nick Gasson
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
#include "lib.h"
#include "phase.h"
#include "common.h"
#include "diag.h"
#include "eval.h"
#include "vcode.h"
#include "opt.h"
#include "diag.h"

#include <assert.h>
#include <stdlib.h>
#include <time.h>

static const error_t *error_lines = NULL;
static lib_t          test_lib = NULL;
static unsigned       errors_seen = 0;

static void test_error_fn(diag_t *d)
{
   fail_if(error_lines == NULL);

   const char *msg = diag_get_text(d);
   const loc_t *loc = diag_get_loc(d);

   bool unexpected = error_lines->line == -1
      || error_lines->snippet == NULL
      || error_lines->line != (loc ? loc->first_line : LINE_INVALID)
      || strstr(msg, error_lines->snippet) == NULL;

   if (unexpected) {
      diag_set_consumer(NULL);
      diag_emit(d);
      printf("expected line %d '%s'\n",
             error_lines->line, error_lines->snippet);
      ck_abort_msg("expected line %d '%s'",
                   error_lines->line, error_lines->snippet);
   }

   error_lines++;
   errors_seen++;
}

static void setup(void)
{
   const char *lib_dir = getenv("LIB_DIR");
   if (lib_dir)
      lib_add_search_path(lib_dir);

   opt_set_int(OPT_BOOTSTRAP, 0);
   opt_set_int(OPT_COVER, 0);
   opt_set_int(OPT_UNIT_TEST, 1);
   opt_set_str(OPT_DUMP_VCODE, getenv("NVC_LOWER_VERBOSE"));
   opt_set_int(OPT_IGNORE_TIME, 0);
   opt_set_int(OPT_VERBOSE, 0);
   opt_set_int(OPT_SYNTHESIS, 0);
   opt_set_int(OPT_ERROR_LIMIT, -1);
   opt_set_int(OPT_ARENA_SIZE, 1 << 20);
   opt_set_str(OPT_GC_VERBOSE, getenv("NVC_GC_VERBOSE"));
   opt_set_str(OPT_OBJECT_VERBOSE, getenv("NVC_OBJECT_VERBOSE"));
   opt_set_str(OPT_ELAB_VERBOSE, getenv("NVC_ELAB_VERBOSE"));
   opt_set_str(OPT_EVAL_VERBOSE, getenv("NVC_EVAL_VERBOSE"));
   opt_set_str(OPT_JIT_VERBOSE, getenv("NVC_JIT_VERBOSE"));
   opt_set_int(OPT_HEAP_SIZE, 128 * 1024);
   opt_set_int(OPT_GC_STRESS, getenv("NVC_GC_STRESS") != 0);
   opt_set_int(OPT_RELAXED, 0);
   opt_set_int(OPT_JIT_LOG, getenv("NVC_JIT_LOG") != NULL);
   opt_set_int(OPT_WARN_HIDDEN, 0);
   opt_set_int(OPT_RT_TRACE, 0);
   opt_set_int(OPT_STOP_DELTA, 1000);
   opt_set_int(OPT_RT_STATS, 0);

   intern_strings();
}

static void setup_per_test(void)
{
   test_lib = lib_tmp("work");
   lib_set_work(test_lib);

   opt_set_int(OPT_COVER, 0);
   opt_set_int(OPT_COVER_BRANCH, 0);
   opt_set_int(OPT_COVER_TOGGLE, 0);
   opt_set_int(OPT_COVER_STMT, 0);
   opt_set_int(OPT_MISSING_BODY, 0);

   reset_error_count();

   set_standard(STD_93);

   error_lines = NULL;
   errors_seen = 0;
   diag_set_consumer(NULL);
}

static void teardown_per_test(void)
{
   lib_set_work(NULL);
   lib_free(test_lib);
   test_lib = NULL;
}

void expect_errors(const error_t *lines)
{
   diag_set_consumer(test_error_fn);

   error_lines = lines;
   errors_seen = 0;
}

void check_expected_errors(void)
{
   ck_assert_ptr_nonnull(error_lines);

   if (error_lines->snippet != NULL || error_lines->line != -1)
      ck_abort_msg("missing expected error line %d '%s'",
                   error_lines->line, error_lines->snippet);

   ck_assert_int_eq(errors_seen, error_count());
}

TCase *nvc_unit_test(void)
{
   TCase *tc_core = tcase_create("Core");
   tcase_add_unchecked_fixture(tc_core, setup, NULL);
   tcase_add_checked_fixture(tc_core, setup_per_test, teardown_per_test);
   return tc_core;
}

tree_t run_elab(void)
{
   eval_t *eval = eval_new(0);

   tree_t t, last_ent = NULL;
   while ((t = parse())) {
      fail_if(error_count() > 0);

      lib_put(lib_work(), t);
      simplify_local(t, eval);
      bounds_check(t);
      fail_if(error_count() > 0);

      if (unit_needs_cgen(t))
         lower_unit(t, NULL);

      const tree_kind_t kind = tree_kind(t);
      if (kind == T_ENTITY || kind == T_CONFIGURATION)
         last_ent = t;
   }

   eval_free(eval);

   return elab(last_ent);
}

tree_t _parse_and_check(const tree_kind_t *array, int num,
                        bool simp, bool lower)
{
   eval_t *eval = simp ? eval_new(0) : NULL;

   tree_t last = NULL;
   for (int i = 0; i < num; i++) {
      if (array[i] == (tree_kind_t)-1)
         goto out;

      last = parse();
      if (last == NULL) {
        ck_abort_msg("expected %s but have NULL", tree_kind_str(array[i]));
        continue;
      }

      const tree_kind_t kind = tree_kind(last);
      ck_assert_msg(tree_kind(last) == array[i],
                    "expected %s have %s", tree_kind_str(array[i]),
                    tree_kind_str(kind));

      lib_put(lib_work(), last);

      if (simp && error_count() == 0)
         simplify_local(last, eval);

      if (lower && error_count() == 0) {
         bounds_check(last);

         if (unit_needs_cgen(last)) {
            vcode_unit_t vu = lower_unit(last, NULL);
            lib_put_vcode(lib_work(), last, vu);
         }
      }
   }

   fail_unless(parse() == NULL);

 out:
   if (eval != NULL)
      eval_free(eval);

   return last;
}

void fail_if_errors(void)
{
   ck_assert_msg(error_count() == 0, "have errors");
   ck_assert_msg(errors_seen == 0, "have errors or diagnostics");
}
