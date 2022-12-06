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
#include "jit/jit-priv.h"
#include "jit/jit.h"
#include "opt.h"

#include <stdlib.h>
#include <inttypes.h>

static jit_handle_t assemble(jit_t *j, const char *text, const char *name,
                             const char *ss)
{
   jit_handle_t h = jit_assemble(j, ident_new(name), text);

   ffi_spec_t spec = 0;
   for (; *ss; ss++) {
      switch (*ss) {
      case 'i': spec |= FFI_INT32; break;
      case 'I': spec |= FFI_INT64; break;
      default:
         fatal_trace("invalid character '%c' in spec", *ss);
      }
      spec <<= 4;
   }

   jit_get_func(j, h)->spec = spec;
   return h;
}

static jit_t *get_native_jit(void)
{
   opt_set_int(OPT_JIT_THRESHOLD, 1);

   jit_t *j = jit_new();
   jit_register_native_plugin(j);

   return j;
}

START_TEST(test_add)
{
   jit_t *j = get_native_jit();

   const char *text1 =
      "    RECV    R0, #0          \n"
      "    RECV    R1, #1          \n"
      "    ADD     R2, R0, R1      \n"
      "    SEND    #0, R2          \n"
      "    RET                     \n";

   jit_handle_t h1 = assemble(j, text1, "add1", "II");

   ck_assert_int_eq(jit_call(j, h1, 1, 2).integer, 3);
   ck_assert_int_eq(jit_call(j, h1, 2, 3).integer, 5);
   ck_assert_int_eq(jit_call(j, h1, INT64_MAX, 1).integer, INT64_MIN);
   ck_assert_int_eq(jit_call(j, h1, 5, INT64_C(-7)).integer, -2);

   const char *text2 =
      "    RECV      R0, #0          \n"
      "    RECV      R1, #1          \n"
      "    ADD.O.32  R2, R0, R1      \n"
      "    CSET      R3              \n"
      "    SEND      #0, R3          \n"
      "    RET                       \n";

   jit_handle_t h2 = assemble(j, text2, "add2", "ii");

   ck_assert_int_eq(jit_call(j, h2, 1, 2).integer, 0);
   ck_assert_int_eq(jit_call(j, h2, INT32_MAX, 1).integer, 1);
   ck_assert_int_eq(jit_call(j, h2, INT32_MAX, INT32_MAX).integer, 1);
   ck_assert_int_eq(jit_call(j, h2, INT32_MIN, INT32_MIN).integer, 1);
   ck_assert_int_eq(jit_call(j, h2, INT32_MIN, INT32_MAX).integer, 0);
   ck_assert_int_eq(jit_call(j, h2, INT32_MIN, INT32_C(-1)).integer, 1);

   const char *text3 =
      "    RECV      R0, #0          \n"
      "    RECV      R1, #1          \n"
      "    ADD.O.8   R2, R0, R1      \n"
      "    CSET      R3              \n"
      "    SEND      #0, R3          \n"
      "    RET                       \n";

   jit_handle_t h3 = assemble(j, text3, "add3", "ii");

   ck_assert_int_eq(jit_call(j, h3, 1, 2).integer, 0);
   ck_assert_int_eq(jit_call(j, h3, 127, 1).integer, 1);
   ck_assert_int_eq(jit_call(j, h3, -128, 1).integer, 0);
   ck_assert_int_eq(jit_call(j, h3, -128, 1000).integer, 1);

   const char *text4 =
      "    RECV      R0, #0          \n"
      "    RECV      R1, #1          \n"
      "    ADD.O.16  R2, R0, R1      \n"
      "    CSET      R3              \n"
      "    SEND      #0, R3          \n"
      "    RET                       \n";

   jit_handle_t h4 = assemble(j, text4, "add4", "ii");

   ck_assert_int_eq(jit_call(j, h4, 1, 2).integer, 0);
   ck_assert_int_eq(jit_call(j, h4, INT16_MAX, 1).integer, 1);
   ck_assert_int_eq(jit_call(j, h4, INT16_MIN, 1).integer, 0);
   ck_assert_int_eq(jit_call(j, h4, -128, 5000).integer, 0);

   const char *text5 =
      "    RECV      R0, #0          \n"
      "    RECV      R1, #1          \n"
      "    ADD.C.32  R2, R0, R1      \n"
      "    CSET      R3              \n"
      "    SEND      #0, R3          \n"
      "    RET                       \n";

   jit_handle_t h5 = assemble(j, text5, "add5", "ii");

   ck_assert_int_eq(jit_call(j, h5, 1, 2).integer, 0);
   ck_assert_int_eq(jit_call(j, h5, INT32_MAX, 1).integer, 0);
   ck_assert_int_eq(jit_call(j, h5, UINT32_MAX, 1).integer, 1);
   ck_assert_int_eq(jit_call(j, h5, INT32_MAX, INT32_MAX).integer, 0);
   ck_assert_int_eq(jit_call(j, h5, UINT32_MAX, UINT32_MAX).integer, 1);

   jit_free(j);
}
END_TEST

Suite *get_native_tests(void)
{
   Suite *s = suite_create("native");

   TCase *tc = nvc_unit_test();
   tcase_add_test(tc, test_add);
   suite_add_tcase(s, tc);

   return s;
}
