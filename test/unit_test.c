//
//  Copyright (C) 2018-2022  Nick Gasson
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
#include "phase.h"
#include "common.h"
#include "rt/heap.h"

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

   SRunner *sr = srunner_create(s);

   if (srunner_fork_status(sr) == CK_NOFORK) {
      static bool warned_fork = false;
      if (!warned_fork) {
         printf("WARNING: skipping unit tests due to CK_FORK=no\n");
         warned_fork = true;
      }
      return 0;
   }

   int nfail = 0;
   if (should_run) {
     srunner_run_all(sr, CK_NORMAL);
     nfail = srunner_ntests_failed(sr);
   }

   srunner_free(sr);

   return nfail;
}

int main(int argc, char **argv)
{
   srand((unsigned)time(NULL));

   term_init();
   register_signal_handlers();

   setenv("NVC_LIBPATH", "./lib", 1);

   int nfail = 0;
   nfail += RUN_TESTS(ident);
   nfail += RUN_TESTS(misc);
   nfail += RUN_TESTS(lib);
   nfail += RUN_TESTS(parse);
   nfail += RUN_TESTS(sem);
   nfail += RUN_TESTS(bounds);
   nfail += RUN_TESTS(simp);
   nfail += RUN_TESTS(value);
   nfail += RUN_TESTS(lower);
   nfail += RUN_TESTS(elab);
   nfail += RUN_TESTS(json);
   nfail += RUN_TESTS(debug);
   nfail += RUN_TESTS(exec);
   nfail += RUN_TESTS(mspace);

   return nfail == 0 ? EXIT_SUCCESS : EXIT_FAILURE;
}
