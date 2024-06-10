//
//  Copyright (C) 2014-2023  Nick Gasson
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
#include "common.h"
#include "diag.h"
#include "diag.h"
#include "eval.h"
#include "jit/jit.h"
#include "lib.h"
#include "lower.h"
#include "option.h"
#include "phase.h"
#include "vcode.h"

#include <assert.h>
#include <stdlib.h>
#include <time.h>

static const error_t   *error_lines = NULL;
static lib_t            test_lib    = NULL;
static unsigned         errors_seen = 0;
static unit_registry_t *registry    = NULL;

static void test_error_fn(diag_t *d, void *context)
{
   fail_if(error_lines == NULL);

   const char *msg = diag_get_text(d);
   const loc_t *loc = diag_get_loc(d);

   bool unexpected = error_lines->line == -1
      || error_lines->snippet == NULL
      || error_lines->line != (loc ? loc->first_line : LINE_INVALID)
      || strstr(msg, error_lines->snippet) == NULL;

   if (unexpected) {
      diag_set_consumer(NULL, NULL);
      diag_emit(d);
      printf("expected line %d '%s'\n",
             error_lines->line, error_lines->snippet);
      ck_abort_msg("expected line %d '%s'",
                   error_lines->line, error_lines->snippet);
   }

   error_lines++;
   errors_seen++;

   for (int hint = 0; error_lines->line == 0; hint++, error_lines++) {
      if (error_lines->snippet == NULL)
         ck_assert_int_eq(hint, diag_hints(d));   // Test for absence of hint
      else {
         ck_assert_int_lt(hint, diag_hints(d));

         if (strstr(diag_get_hint(d, hint), error_lines->snippet) == NULL) {
            diag_set_consumer(NULL, NULL);
            diag_emit(d);
            printf("expected hint '%s'\n", error_lines->snippet);
            ck_abort_msg("expected hint '%s'", error_lines->snippet);
         }
      }
   }
}

static void setup_per_test(void)
{
   test_lib = lib_tmp("work");
   lib_set_work(test_lib);

   reset_error_count();

   set_standard(STD_93);

   error_lines = NULL;
   errors_seen = 0;
   diag_set_consumer(NULL, NULL);
}

static void teardown_per_test(void)
{
   if (registry != NULL) {
      unit_registry_free(registry);
      registry = NULL;
   }

   lib_set_work(NULL);
   lib_free(test_lib);
   test_lib = NULL;
}

void expect_errors(const error_t *lines)
{
   diag_set_consumer(test_error_fn, NULL);

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
   tcase_add_checked_fixture(tc_core, setup_per_test, teardown_per_test);
   return tc_core;
}

unit_registry_t *get_registry(void)
{
   if (registry == NULL)
      registry = unit_registry_new();

   return registry;
}

tree_t run_elab(void)
{
   unit_registry_t *ur = get_registry();
   jit_t *j = jit_new(ur);

   tree_t t, last_ent = NULL;
   while ((t = parse())) {
      fail_if(error_count() > 0);

      lib_put(lib_work(), t);
      simplify_local(t, j, ur);
      bounds_check(t);
      fail_if(error_count() > 0);

      const tree_kind_t kind = tree_kind(t);
      if (kind == T_ENTITY || kind == T_CONFIGURATION)
         last_ent = t;
   }

   tree_t top = elab(tree_to_object(last_ent), j, ur, NULL);

   jit_free(j);

   return top;
}

tree_t _parse_and_check(const tree_kind_t *array, int num, bool simp)
{
   jit_t *jit = NULL;
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

      if (simp && error_count() == 0) {
         unit_registry_t *ur = get_registry();
         if (jit == NULL)
            jit = jit_new(ur);
         else
            unit_registry_purge(ur, tree_ident(last));

         simplify_local(last, jit, ur);
         bounds_check(last);
      }
   }

   fail_unless(parse() == NULL);

 out:
   if (jit != NULL)
      jit_free(jit);

   return last;
}

void fail_if_errors(void)
{
   ck_assert_msg(error_count() == 0, "have errors");
   ck_assert_msg(errors_seen == 0, "have errors or diagnostics");
}
