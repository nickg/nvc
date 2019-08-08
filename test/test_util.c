#include "test_util.h"
#include "lib.h"
#include "phase.h"
#include "common.h"

#include <assert.h>
#include <stdlib.h>
#include <time.h>

static const error_t *error_lines = NULL;
static error_fn_t     orig_error_fn = NULL;
static lib_t          test_lib = NULL;

static void test_error_fn(const char *msg, const loc_t *loc)
{
   fail_if(error_lines == NULL);

   bool unexpected = error_lines->line == -1
      || error_lines->snippet == NULL
      || error_lines->line != loc->first_line
      || strstr(msg, error_lines->snippet) == NULL;

   if (unexpected) {
      orig_error_fn(msg, loc);
      printf("expected line %d '%s'\n",
             error_lines->line, error_lines->snippet);
   }

   fail_if(unexpected);

   error_lines++;
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
   opt_set_int("parse-pragmas", 0);
   intern_strings();
}

static void setup_per_test(void)
{
   test_lib = lib_tmp("work");
   lib_set_work(test_lib);

   opt_set_int("cover", 0);

   reset_bounds_errors();
   reset_sem_errors();
   reset_parse_errors();
   reset_eval_errors();

   set_standard(STD_93);
   set_relax_rules(0);

   error_lines = NULL;
   set_error_fn(NULL, 1);
}

static void teardown_per_test(void)
{
   lib_set_work(NULL);
   lib_free(test_lib);
   test_lib = NULL;
}

void expect_errors(const error_t *lines)
{
#ifdef __MINGW32__
   error_fn_t old_fn = set_error_fn(test_error_fn, false);
   if (orig_error_fn == NULL)
      orig_error_fn = old_fn;
#else
   fail_unless(orig_error_fn == NULL);
   orig_error_fn = set_error_fn(test_error_fn, false);
#endif
   error_lines = lines;
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
      sem_check(t);
      fail_if(sem_errors() > 0);

      simplify(t, 0);
      if (tree_kind(t) == T_PACKAGE || tree_kind(t) == T_PACK_BODY)
         lower_unit(t);

      if (tree_kind(t) == T_ENTITY)
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
      fail_if(last == NULL);
      const tree_kind_t kind = tree_kind(last);
      fail_unless(tree_kind(last) == array[i],
                  "expected %s have %s", tree_kind_str(array[i]),
                  tree_kind_str(kind));

      const bool sem_ok = sem_check(last);
      if (simp) {
         fail_unless(sem_ok, "semantic check failed");
         simplify(last, 0);
      }

      if (lower && (kind == T_PACKAGE || kind == T_PACK_BODY))
         lower_unit(last);
   }

   fail_unless(parse() == NULL);
   fail_unless(parse_errors() == 0);

   return last;
}
