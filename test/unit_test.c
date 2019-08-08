#include "test_util.h"
#include "phase.h"
#include "common.h"

#include <check.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <time.h>

Suite *get_ident_tests(void);
Suite *get_lower_tests(void);

#define RUN_TESTS(name)                                         \
   ({ extern Suite *get_##name##_tests(void);                   \
      run_suite(get_##name##_tests(), #name, argc, argv); })

static int run_suite(Suite *s, const char *name, int argc, char **argv)
{
   bool should_run = argc == 1;

   for (int i = 1; i < argc; i++) {
      if (strcmp(argv[i], name) == 0)
         should_run = true;
   }

   if (!should_run)
      return 0;

   SRunner *sr = srunner_create(s);
   srunner_run_all(sr, CK_NORMAL);

   const int nfail = srunner_ntests_failed(sr);

   srunner_free(sr);

   return nfail;
}

int main(int argc, char **argv)
{
   srand((unsigned)time(NULL));

   term_init();
   register_trace_signal_handlers();

   setenv("NVC_LIBPATH", "../lib/std", 1);

   int nfail = 0;
   nfail += RUN_TESTS(ident);
   nfail += RUN_TESTS(hash);
   nfail += RUN_TESTS(heap);
   nfail += RUN_TESTS(lib);
   nfail += RUN_TESTS(parse);
   nfail += RUN_TESTS(sem);
   nfail += RUN_TESTS(bounds);
   nfail += RUN_TESTS(simp);
   nfail += RUN_TESTS(value);
   nfail += RUN_TESTS(lower);
   nfail += RUN_TESTS(group);
   nfail += RUN_TESTS(elab);
   nfail += RUN_TESTS(json);

   return nfail == 0 ? EXIT_SUCCESS : EXIT_FAILURE;
}
