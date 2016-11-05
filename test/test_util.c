#include "test_util.h"
#include "lib.h"
#include "phase.h"
#include "common.h"

#include <assert.h>
#include <stdlib.h>
#include <time.h>

static const error_t *error_lines = NULL;
static error_fn_t     orig_error_fn = NULL;

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

   lib_set_work(lib_tmp("work"));
   opt_set_int("bootstrap", 0);
   opt_set_int("cover", 0);
   opt_set_int("unit-test", 1);
   opt_set_int("prefer-explicit", 0);
   opt_set_str("dump-vcode", NULL);
   opt_set_int("relax", 0);
   opt_set_int("ignore-time", 0);
   opt_set_int("verbose", 0);
   intern_strings();
}

static void teardown(void)
{
   lib_free(lib_work());
}

void expect_errors(const error_t *lines)
{
   fail_unless(orig_error_fn == NULL);
   orig_error_fn = set_error_fn(test_error_fn, false);
   error_lines = lines;
}

TCase *nvc_unit_test(void)
{
   term_init();
   register_trace_signal_handlers();

   setenv("NVC_LIBPATH", "../lib/std", 1);

   TCase *tc_core = tcase_create("Core");
   tcase_add_unchecked_fixture(tc_core, setup, teardown);
   return tc_core;
}

int nvc_run_test(Suite *s)
{
   SRunner *sr = srunner_create(s);
   srunner_run_all(sr, CK_NORMAL);

   const int nfail = srunner_ntests_failed(sr);

   srunner_free(sr);

   return nfail == 0 ? EXIT_SUCCESS : EXIT_FAILURE;
}

tree_t run_elab(void)
{
   tree_t t, last_ent = NULL;
   while ((t = parse())) {
      sem_check(t);
      fail_if(sem_errors() > 0);

      simplify(t);
      lower_unit(t);

      if (tree_kind(t) == T_ENTITY)
         last_ent = t;
   }

   return elab(last_ent);
}

tree_t _parse_and_check(const tree_kind_t *array, int num, bool simp)
{
   tree_t last = NULL;
   for (int i = 0; i < num; i++) {
      if (array[i] == (tree_kind_t)-1)
         return last;

      last = parse();
      fail_if(last == NULL);
      fail_unless(tree_kind(last) == array[i],
                  "expected %s have %s", tree_kind_str(array[i]),
                  tree_kind_str(tree_kind(last)));

      if (sem_check(last) && simp)
         simplify(last);
   }

   fail_unless(parse() == NULL);
   fail_unless(parse_errors() == 0);

   return last;
}
