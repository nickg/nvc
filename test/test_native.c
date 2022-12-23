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
      case 'p': spec |= FFI_POINTER; break;
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

   const char *text6 =
      "    RECV    R0, #0          \n"
      "    RECV    R1, #1          \n"
      "    ADD.8   R2, R0, R1      \n"
      "    SEND    #0, R2          \n"
      "    RET                     \n";

   jit_handle_t h6 = assemble(j, text6, "add6", "ii");

   ck_assert_int_eq(jit_call(j, h6, 1, 2).integer, 3);
   ck_assert_int_eq(jit_call(j, h6, 2, 3).integer, 5);
   ck_assert_int_eq(jit_call(j, h6, 2, -3).integer, -1);

   jit_free(j);
}
END_TEST

START_TEST(test_mul)
{
   jit_t *j = get_native_jit();

   const char *text1 =
      "    RECV    R0, #0          \n"
      "    RECV    R1, #1          \n"
      "    MUL     R2, R0, R1      \n"
      "    SEND    #0, R2          \n"
      "    RET                     \n";

   jit_handle_t h1 = assemble(j, text1, "mul1", "II");

   ck_assert_int_eq(jit_call(j, h1, 4, 1).integer, 4);
   ck_assert_int_eq(jit_call(j, h1, 2, 3).integer, 6);
   ck_assert_int_eq(jit_call(j, h1, INT64_C(-5), 2).integer, -10);
   ck_assert_int_eq(jit_call(j, h1, INT64_C(-5), INT64_C(-6)).integer, 30);

   const char *text2 =
      "    RECV      R0, #0          \n"
      "    RECV      R1, #1          \n"
      "    MUL.O.32  R2, R0, R1      \n"
      "    CSET      R3              \n"
      "    SEND      #0, R3          \n"
      "    RET                       \n";

   jit_handle_t h2 = assemble(j, text2, "mul2", "ii");

   ck_assert_int_eq(jit_call(j, h2, 4, 1).integer, 0);
   ck_assert_int_eq(jit_call(j, h2, INT32_MAX/2, 2).integer, 0);
   ck_assert_int_eq(jit_call(j, h2, INT32_MAX, 2).integer, 1);
   ck_assert_int_eq(jit_call(j, h2, INT32_MAX, INT32_C(-1)).integer, 0);
   ck_assert_int_eq(jit_call(j, h2, INT32_MIN, INT32_C(-1)).integer, 1);
   ck_assert_int_eq(jit_call(j, h2, INT64_C(-5), INT64_C(-6)).integer, 0);

   const char *text3 =
      "    RECV      R0, #0          \n"
      "    RECV      R1, #1          \n"
      "    MUL.C.32  R2, R0, R1      \n"
      "    CSET      R3              \n"
      "    SEND      #0, R3          \n"
      "    RET                       \n";

   jit_handle_t h3 = assemble(j, text3, "mul3", "ii");

   ck_assert_int_eq(jit_call(j, h3, 4, 1).integer, 0);
   ck_assert_int_eq(jit_call(j, h3, INT32_MAX/2, 2).integer, 0);
   ck_assert_int_eq(jit_call(j, h3, INT32_MAX, 2).integer, 0);
   ck_assert_int_eq(jit_call(j, h3, UINT32_MAX, 2).integer, 1);
   ck_assert_int_eq(jit_call(j, h3, UINT32_MAX, UINT32_MAX).integer, 1);

   const char *text4 =
      "    RECV    R0, #0          \n"
      "    RECV    R1, #1          \n"
      "    MUL.32  R2, R0, R1      \n"
      "    SEND    #0, R2          \n"
      "    RET                     \n";

   jit_handle_t h4 = assemble(j, text4, "mul4", "ii");

   ck_assert_int_eq(jit_call(j, h4, 4, 1).integer, 4);
   ck_assert_int_eq(jit_call(j, h4, 2, 3).integer, 6);
   ck_assert_int_eq(jit_call(j, h4, INT32_C(-5), 2).integer, -10);
   ck_assert_int_eq(jit_call(j, h4, INT32_C(-5), INT64_C(-6)).integer, 30);

   jit_free(j);
}
END_TEST

START_TEST(test_div)
{
   jit_t *j = get_native_jit();

   const char *text1 =
      "    RECV    R0, #0          \n"
      "    RECV    R1, #1          \n"
      "    DIV     R2, R0, R1      \n"
      "    SEND    #0, R2          \n"
      "    RET                     \n";

   jit_handle_t h1 = assemble(j, text1, "div1", "II");

   ck_assert_int_eq(jit_call(j, h1, 4, 1).integer, 4);
   ck_assert_int_eq(jit_call(j, h1, 2, 3).integer, 0);
   ck_assert_int_eq(jit_call(j, h1, 100, 5).integer, 20);
   ck_assert_int_eq(jit_call(j, h1, INT64_C(-50), 6).integer, -8);

   const char *text2 =
      "    RECV    R0, #0          \n"
      "    RECV    R1, #1          \n"
      "    DIV.16  R2, R0, R1      \n"
      "    SEND    #0, R2          \n"
      "    RET                     \n";

   jit_handle_t h2 = assemble(j, text2, "div2", "ii");

   ck_assert_int_eq(jit_call(j, h2, 4, 1).integer, 4);
   ck_assert_int_eq(jit_call(j, h2, 2, 3).integer, 0);
   ck_assert_int_eq(jit_call(j, h2, 100, 5).integer, 20);
   ck_assert_int_eq(jit_call(j, h2, INT32_C(-50), 6).integer, -8);

   const char *text3 =
      "    RECV    R0, #0          \n"
      "    RECV    R1, #1          \n"
      "    DIV.32  R2, R0, R1      \n"
      "    SEND    #0, R2          \n"
      "    RET                     \n";

   jit_handle_t h3 = assemble(j, text3, "div3", "ii");

   ck_assert_int_eq(jit_call(j, h3, 4, 1).integer, 4);
   ck_assert_int_eq(jit_call(j, h3, 2, 3).integer, 0);
   ck_assert_int_eq(jit_call(j, h3, 100, 5).integer, 20);
   ck_assert_int_eq(jit_call(j, h3, INT32_C(-50), 6).integer, -8);

   const char *text4 =
      "    RECV    R0, #0          \n"
      "    RECV    R1, #1          \n"
      "    DIV.8   R2, R0, R1      \n"
      "    SEND    #0, R2          \n"
      "    RET                     \n";

   jit_handle_t h4 = assemble(j, text4, "div4", "ii");

   ck_assert_int_eq(jit_call(j, h4, 4, 1).integer, 4);
   ck_assert_int_eq(jit_call(j, h4, 2, 3).integer, 0);
   ck_assert_int_eq(jit_call(j, h4, 100, 5).integer, 20);
   ck_assert_int_eq(jit_call(j, h4, INT32_C(-50), 6).integer, -8);
   ck_assert_int_eq(jit_call(j, h4, 50, INT32_C(-6)).integer, -8);

   const char *text5 =
      "    RECV    R0, #0          \n"
      "    RECV    R1, #1          \n"
      "    DIV.16  R2, R0, R1      \n"
      "    SEND    #0, R2          \n"
      "    RET                     \n";

   jit_handle_t h5 = assemble(j, text5, "div5", "ii");

   ck_assert_int_eq(jit_call(j, h5, 4, 1).integer, 4);
   ck_assert_int_eq(jit_call(j, h5, 2, 3).integer, 0);
   ck_assert_int_eq(jit_call(j, h5, 100, 5).integer, 20);
   ck_assert_int_eq(jit_call(j, h5, INT32_C(-50), 6).integer, -8);
   ck_assert_int_eq(jit_call(j, h5, 5000, INT32_C(-6)).integer, -833);

   jit_free(j);
}
END_TEST

START_TEST(test_rem)
{
   jit_t *j = get_native_jit();

   const char *text1 =
      "    RECV    R0, #0          \n"
      "    RECV    R1, #1          \n"
      "    REM     R2, R0, R1      \n"
      "    SEND    #0, R2          \n"
      "    RET                     \n";

   jit_handle_t h1 = assemble(j, text1, "rem1", "II");

   ck_assert_int_eq(jit_call(j, h1, 4, 1).integer, 0);
   ck_assert_int_eq(jit_call(j, h1, 2, 3).integer, 2);
   ck_assert_int_eq(jit_call(j, h1, 100, 6).integer, 4);
   ck_assert_int_eq(jit_call(j, h1, INT64_C(-50), 6).integer, -2);
   ck_assert_int_eq(jit_call(j, h1, 6, INT64_C(-50)).integer, 6);

   const char *text2 =
      "    RECV    R0, #0          \n"
      "    RECV    R1, #1          \n"
      "    REM.16  R2, R0, R1      \n"
      "    SEND    #0, R2          \n"
      "    RET                     \n";

   jit_handle_t h2 = assemble(j, text2, "rem2", "ii");

   ck_assert_int_eq(jit_call(j, h2, 4, 1).integer, 0);
   ck_assert_int_eq(jit_call(j, h2, 2, 3).integer, 2);
   ck_assert_int_eq(jit_call(j, h2, 100, 6).integer, 4);
   ck_assert_int_eq(jit_call(j, h2, INT32_C(-50), 6).integer, -2);
   ck_assert_int_eq(jit_call(j, h2, 6, INT32_C(-50)).integer, 6);

   jit_free(j);
}
END_TEST

static void test_call_diag_fn(diag_t *d)
{
   ck_assert_str_eq(diag_get_text(d), "invalid integer value \"foo\"");
   ck_assert_int_eq(diag_traces(d), 2);
   ck_assert_str_eq(diag_get_trace(d, 0), "do_str2int");
   ck_assert_str_eq(diag_get_trace(d, 1), "do_atest");
}

START_TEST(test_call)
{
   jit_t *j = get_native_jit();

   const char *do_add =
      "    RECV    R0, #0          \n"
      "    RECV    R1, #1          \n"
      "    ADD     R2, R0, R1      \n"
      "    SEND    #0, R2          \n"
      "    RET                     \n";

   const char *do_double =
      "    RECV    R0, #0          \n"
      "    SEND    #1, R0          \n"
      "    CALL    <do_add>        \n"
      "    RET                     \n";

   assemble(j, do_add, "do_add", "II");
   jit_handle_t h_double = assemble(j, do_double, "do_double", "II");

   ck_assert_int_eq(jit_call(j, h_double, 4).integer, 8);
   ck_assert_int_eq(jit_call(j, h_double, 2).integer, 4);
   ck_assert_int_eq(jit_call(j, h_double, 5).integer, 10);

   const char *do_str2int =
      "    SEND    #2, #0          \n"
      "    $EXIT   #25             \n"
      "    RET                     \n";

   jit_handle_t h_str2int = assemble(j, do_str2int, "do_str2int", "pI");
   ck_assert_int_eq(jit_call(j, h_str2int, "4", 1).integer, 4);
   ck_assert_int_eq(jit_call(j, h_str2int, "5", 1).integer, 5);
   ck_assert_int_eq(jit_call(j, h_str2int, "-123", 4).integer, -123);

   const char *do_atest =
      "    CALL    <do_str2int>    \n"
      "    RET                     \n";

   jit_handle_t h_atest = assemble(j, do_atest, "do_atest", "pI");
   ck_assert_int_eq(jit_call(j, h_atest, "4", 1).integer, 4);
   ck_assert_int_eq(jit_call(j, h_atest, "5", 1).integer, 5);

   diag_set_consumer(test_call_diag_fn);

   jit_scalar_t result;
   fail_if(jit_try_call(j, h_atest, &result, "foo", 4));
}
END_TEST

Suite *get_native_tests(void)
{
   Suite *s = suite_create("native");

   TCase *tc = nvc_unit_test();
   tcase_add_test(tc, test_add);
   tcase_add_test(tc, test_mul);
   tcase_add_test(tc, test_div);
   tcase_add_test(tc, test_rem);
   tcase_add_test(tc, test_call);
   suite_add_tcase(s, tc);

   return s;
}
