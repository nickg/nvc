//
//  Copyright (C) 2021  Nick Gasson
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

#include "debug.h"
#include "test_util.h"

#if defined __clang__
#pragma clang optimize off
#elif defined __GNUC__
#pragma GCC optimize ("O0")
#endif

static int call_line = 0;

DLLEXPORT
__attribute__((noinline))
void global_func(void)
{
   int capture_line = __LINE__; debug_info_t *di = debug_capture();
   (void)capture_line;

   const int nframes = debug_count_frames(di);
   fail_unless(nframes >= 3);

   const debug_frame_t *f0 = debug_get_frame(di, 0);
   fail_unless(f0->kind == FRAME_PROG);
   fail_if(f0->symbol == NULL);
   ck_assert_str_eq(f0->symbol, "global_func");
   fail_unless(strstr(f0->module, "unit_test"));
#if defined HAVE_LIBDW || defined HAVE_LIBDWARF
   fail_unless(f0->lineno == capture_line);
#endif
   fail_unless(f0->pc >= (uintptr_t)global_func);
   fail_unless(f0->pc < (uintptr_t)global_func + 0x1000);

   const debug_frame_t *f1 = debug_get_frame(di, 1);
   fail_unless(f1->kind == FRAME_PROG);
   fail_unless(strstr(f1->module, "unit_test"));
#if defined HAVE_LIBDW || defined HAVE_LIBDWARF
   fail_unless(f1->lineno == call_line);
#endif

   debug_free(di);
}

__attribute__((noinline))
static void static_func(void)
{
   call_line = __LINE__; global_func();
}

START_TEST(test_capture)
{
   static_func();
}
END_TEST

Suite *get_debug_tests(void)
{
   Suite *s = suite_create("debug");

   TCase *tc_core = nvc_unit_test();
   tcase_add_test(tc_core, test_capture);
   suite_add_tcase(s, tc_core);

   return s;
}
