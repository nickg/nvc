#include "test_util.h"
#include "lib.h"
#include "phase.h"
#include "common.h"
#include "loc.h"
#include "vcode.h"

#include <assert.h>
#include <stdlib.h>
#include <time.h>

static const error_t *error_lines = NULL;
static lib_t          test_lib = NULL;
static unsigned       errors_seen = 0;

static void test_error_fn(const char *msg, const loc_t *loc)
{
   fail_if(error_lines == NULL);

   bool unexpected = error_lines->line == -1
      || error_lines->snippet == NULL
      || error_lines->line != loc->first_line
      || strstr(msg, error_lines->snippet) == NULL;

   if (unexpected) {
      set_error_fn(NULL);
      error_at(loc, "%s", msg);
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

   opt_set_int("bootstrap", 0);
   opt_set_int("cover", 0);
   opt_set_int("unit-test", 1);
   opt_set_str("dump-vcode", NULL);
   opt_set_int("ignore-time", 0);
   opt_set_int("verbose", 0);
   opt_set_int("synthesis", 0);
   opt_set_int("error-limit", -1);
   intern_strings();
}

static void setup_per_test(void)
{
   test_lib = lib_tmp("work");
   lib_set_work(test_lib);

   opt_set_int("cover", 0);
   opt_set_int("missing-body", 0);

   reset_error_count();

   set_standard(STD_93);
   set_relax_rules(0);

   error_lines = NULL;
   set_error_fn(NULL);
}

static void teardown_per_test(void)
{
   lib_set_work(NULL);
   lib_free(test_lib);
   test_lib = NULL;
}

void expect_errors(const error_t *lines)
{
   set_error_fn(test_error_fn);

   error_lines = lines;
   errors_seen = 0;
}

void check_expected_errors(void)
{
   ck_assert_ptr_nonnull(error_lines);

   if (error_lines->snippet != NULL || error_lines->line != -1)
      ck_abort_msg("missing expected error line %d '%s'",
                   error_lines->line, error_lines->snippet);

   ck_assert_int_eq(error_count(), errors_seen);
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
   tree_t t, last_ent = NULL;
   while ((t = parse())) {
      fail_if(error_count() > 0);
      sem_check(t);
      fail_if(error_count() > 0);

      simplify_local(t);
      bounds_check(t);
      fail_if(error_count() > 0);

      const tree_kind_t kind = tree_kind(t);
      if ((kind == T_PACKAGE && !package_needs_body(t)) || kind == T_PACK_BODY)
         lower_unit(t, NULL);

      if (kind == T_ENTITY || kind == T_CONFIGURATION)
         last_ent = t;
   }

   return elab(last_ent);
}

tree_t _parse_and_check(const tree_kind_t *array, int num,
                        bool simp, bool lower)
{
   tree_t last = NULL;
   for (int i = 0; i < num; i++) {
      if (array[i] == (tree_kind_t)-1)
         return last;

      last = parse();
      if (last == NULL) {
        ck_abort_msg("expected %s but have NULL", tree_kind_str(array[i]));
        continue;
      }

      const tree_kind_t kind = tree_kind(last);
      ck_assert_msg(tree_kind(last) == array[i],
                    "expected %s have %s", tree_kind_str(array[i]),
                    tree_kind_str(kind));

      const bool sem_ok = sem_check(last);
      if (simp) {
         ck_assert_msg(sem_ok, "semantic check failed");
         simplify_local(last);
      }

      if (lower && ((kind == T_PACKAGE && !package_needs_body(last))
                    || kind == T_PACK_BODY)) {
         bounds_check(last);
         vcode_unit_t vu = lower_unit(last, NULL);
         lib_put_vcode(lib_work(), last, vu);
      }
   }

   fail_unless(parse() == NULL);

   return last;
}

void fail_if_errors(void)
{
   ck_assert_msg(error_count() == 0, "have errors");
}
