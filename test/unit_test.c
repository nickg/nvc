//
//  Copyright (C) 2018-2023  Nick Gasson
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
#include "option.h"
#include "phase.h"
#include "rt/mspace.h"
#include "thread.h"
#include "scan.h"

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

const char copy_string[] = "";
const char version_string[] = "";

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
   thread_init();
   set_default_options();
   intern_strings();
   register_signal_handlers();
   mspace_stack_limit(MSPACE_CURRENT_FRAME);

   opt_set_int(OPT_UNIT_TEST, 1);
   opt_set_size(OPT_ARENA_SIZE, 1 << 20);
   opt_set_str(OPT_GC_VERBOSE, getenv("NVC_GC_VERBOSE"));
   opt_set_size(OPT_HEAP_SIZE, 128 * 1024);
   opt_set_int(OPT_GC_STRESS, getenv("NVC_GC_STRESS") != 0);

   if (getenv("NVC_LIBPATH") == NULL)
      setenv("NVC_LIBPATH", "./lib", 1);

   int nfail = 0;
   nfail += RUN_TESTS(ident);
   nfail += RUN_TESTS(misc);
   nfail += RUN_TESTS(lib);
   nfail += RUN_TESTS(parse);
   nfail += RUN_TESTS(charset);
   nfail += RUN_TESTS(sem);
   nfail += RUN_TESTS(bounds);
   nfail += RUN_TESTS(simp);
   nfail += RUN_TESTS(value);
   nfail += RUN_TESTS(lower);
   nfail += RUN_TESTS(elab);
   nfail += RUN_TESTS(debug);
   nfail += RUN_TESTS(jit);
   nfail += RUN_TESTS(mspace);
   nfail += RUN_TESTS(model);
#if defined ARCH_X86_64
   nfail += RUN_TESTS(native);
#endif
   nfail += RUN_TESTS(psl);
#ifdef ENABLE_VERILOG
   nfail += RUN_TESTS(vlog);
#endif
#ifdef ENABLE_TCL
   nfail += RUN_TESTS(shell);
#endif
   nfail += RUN_TESTS(dump);
#ifdef ENABLE_SERVER
   nfail += RUN_TESTS(server);
#endif

   return nfail == 0 ? EXIT_SUCCESS : EXIT_FAILURE;
}
