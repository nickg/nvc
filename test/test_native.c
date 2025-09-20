//
//  Copyright (C) 2022-2024  Nick Gasson
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
#include "option.h"

#include <stdlib.h>
#include <inttypes.h>

static jit_handle_t assemble(jit_t *j, const char *text, const char *name,
                             const char *ss)
{
   jit_handle_t h = jit_assemble(j, ident_new(name), text);

   LOCAL_TEXT_BUF tb = tb_new();
   tb_append(tb, 'v');

   for (; *ss; ss++) {
      switch (*ss) {
      case 'i': tb_append(tb, FFI_INT32); break;
      case 'I': tb_append(tb, FFI_INT64); break;
      case 'p': tb_append(tb, FFI_POINTER); break;
      case 'f': tb_append(tb, FFI_FLOAT); break;
      default:
         fatal_trace("invalid character '%c' in spec", *ss);
      }
   }

   ck_assert_int_le(tb_len(tb), 7);   // Would have to copy otherwise
   jit_get_func(j, h)->spec = ffi_spec_new(tb_get(tb), tb_len(tb));
   return h;
}

static jit_t *get_native_jit(void)
{
   opt_set_int(OPT_JIT_THRESHOLD, 1);
   opt_set_int(OPT_JIT_ASYNC, 0);

   jit_t *j = jit_new(NULL, NULL, NULL);
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

   const char *text7 =
      "    RECV     R0, #0         \n"
      "    ADD.O.8  R1, R0, #1     \n"
      "    CSET     R2             \n"
      "    SEND     #0, R2         \n"
      "    RET                     \n";

   jit_handle_t h7 = assemble(j, text7, "add7", "i");

   ck_assert_int_eq(jit_call(j, h7, 127).integer, 1);
   ck_assert_int_eq(jit_call(j, h7, -127).integer, 0);
   ck_assert_int_eq(jit_call(j, h7, 5).integer, 0);
   ck_assert_int_eq(jit_call(j, h7, 127).integer, 1);

   const char *text8 =
      "    RECV      R0, #0        \n"
      "    ADD.O.16  R1, R0, #1    \n"
      "    CSET      R2            \n"
      "    SEND      #0, R2        \n"
      "    RET                     \n";

   jit_handle_t h8 = assemble(j, text8, "add8", "i");

   ck_assert_int_eq(jit_call(j, h8, INT16_MAX).integer, 1);
   ck_assert_int_eq(jit_call(j, h8, INT16_MIN).integer, 0);
   ck_assert_int_eq(jit_call(j, h8, 5).integer, 0);
   ck_assert_int_eq(jit_call(j, h8, INT16_MAX).integer, 1);

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

   const char *text5 =
      "    RECV      R0, #0        \n"
      "    RECV      R1, #1        \n"
      "    MUL.O.16  R2, R0, R1    \n"
      "    CSET      R3            \n"
      "    SEND      #0, R3        \n"
      "    RET                     \n";

   jit_handle_t h5 = assemble(j, text5, "mul5", "ii");

   ck_assert_int_eq(jit_call(j, h5, 1, 1).integer, 0);
   ck_assert_int_eq(jit_call(j, h5, 64, 2).integer, 0);
   ck_assert_int_eq(jit_call(j, h5, 256, 256).integer, 1);
   ck_assert_int_eq(jit_call(j, h5, 256, 255).integer, 1);

   const char *text6 =
      "    RECV      R0, #0        \n"
      "    RECV      R1, #1        \n"
      "    MUL.C.16  R2, R0, R1    \n"
      "    CSET      R3            \n"
      "    SEND      #0, R3        \n"
      "    RET                     \n";

   jit_handle_t h6 = assemble(j, text6, "mul6", "ii");

   ck_assert_int_eq(jit_call(j, h6, 1, 1).integer, 0);
   ck_assert_int_eq(jit_call(j, h6, 64, 2).integer, 0);
   ck_assert_int_eq(jit_call(j, h6, 256, 256).integer, 1);
   ck_assert_int_eq(jit_call(j, h6, 256, 255).integer, 0);

   const char *text7 =
      "    RECV    R0, #0          \n"
      "    MUL     R1, R0, #2      \n"
      "    SEND    #0, R1          \n"
      "    RET                     \n";

   jit_handle_t h7 = assemble(j, text7, "mul7", "i");

   ck_assert_int_eq(jit_call(j, h7, 1).integer, 2);
   ck_assert_int_eq(jit_call(j, h7, 64).integer, 128);
   ck_assert_int_eq(jit_call(j, h7, 256).integer, 512);
   ck_assert_int_eq(jit_call(j, h7, -8).integer, -16);

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

static void test_call_diag_fn(diag_t *d, void *context)
{
   ck_assert_str_eq(diag_get_text(d), "executed unreachable instruction");

   int *state = context;
   switch ((*state)++) {
   case 0:
      ck_assert_int_eq(diag_traces(d), 1);
      ck_assert_str_eq(diag_get_trace(d, 0), "do_unreachable");
      break;
   case 1:
   case 2:
      ck_assert_int_eq(diag_traces(d), 2);
      ck_assert_str_eq(diag_get_trace(d, 0), "do_unreachable");
      ck_assert_str_eq(diag_get_trace(d, 1), "do_atest");
      break;
   default:
      ck_abort_msg("unexpected call to test_call_diag_fn");
   }
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

   int state = 0;
   diag_set_consumer(test_call_diag_fn, &state);

   const char *do_unreachable =
      "    SEND    #0, #0          \n"
      "    $EXIT   #4              \n"
      "    RET                     \n";

   jit_handle_t h_unreachable =
      assemble(j, do_unreachable, "do_unreachable", "");

   jit_scalar_t result;
   fail_if(jit_try_call(j, h_unreachable, &result));

   const char *do_atest =
      "    CALL    <do_unreachable>   \n"
      "    RET                        \n";

   jit_handle_t h_atest = assemble(j, do_atest, "do_atest", "pI");
   fail_if(jit_try_call(j, h_atest, &result));
   fail_if(jit_try_call(j, h_atest, &result));

   ck_assert_int_eq(state, 3);

   jit_free(j);
}
END_TEST

START_TEST(test_stack)
{
   jit_t *j = get_native_jit();

   const char *text1 =
      "    RECV      R0, #0          \n"
      "    $SALLOC   R1, #0, #4      \n"
      "    STORE.32  R0, [R1]        \n"
      "    LOAD.32   R2, [R1]        \n"
      "    SEND      #0, R2          \n"
      "    RET                       \n";

   jit_handle_t h1 = assemble(j, text1, "stack1", "i");
   ck_assert_int_eq(jit_call(j, h1, 4).integer, 4);
   ck_assert_int_eq(jit_call(j, h1, 5).integer, 5);
   ck_assert_int_eq(jit_call(j, h1, INT32_C(-5)).integer, -5);

   const char *text2 =
      "    RECV      R0, #0          \n"
      "    $SALLOC   R1, #0, #1      \n"
      "    STORE.8   R0, [R1]        \n"
      "    MOV       R2, #0xdeadbeef \n"
      "    ULOAD.8   R2, [R1]        \n"
      "    SEND      #0, R2          \n"
      "    RET                       \n";

   jit_handle_t h2 = assemble(j, text2, "stack2", "i");
   ck_assert_int_eq(jit_call(j, h2, 4).integer, 4);
   ck_assert_int_eq(jit_call(j, h2, 5).integer, 5);
   ck_assert_int_eq(jit_call(j, h2, 255).integer, 255);

   const char *text3 =
      "    RECV      R0, #0          \n"
      "    $SALLOC   R1, #0, #4      \n"
      "    STORE.32  R0, [R1]        \n"
      "    MOV       R2, #-1         \n"
      "    ULOAD.32  R2, [R1]        \n"
      "    SEND      #0, R2          \n"
      "    RET                       \n";

   jit_handle_t h3 = assemble(j, text3, "stack3", "i");
   ck_assert_int_eq(jit_call(j, h3, 4).integer, 4);
   ck_assert_int_eq(jit_call(j, h3, 5).integer, 5);
   ck_assert_int_eq(jit_call(j, h3, 255).integer, 255);

   const char *text4 =
      "    RECV      R0, #0          \n"
      "    $SALLOC   R1, #0, #2      \n"
      "    STORE.16  R0, [R1]        \n"
      "    MOV       R2, #0xdeadbeef \n"
      "    ULOAD.16  R2, [R1]        \n"
      "    SEND      #0, R2          \n"
      "    RET                       \n";

   jit_handle_t h4 = assemble(j, text4, "stack4", "i");
   ck_assert_int_eq(jit_call(j, h4, 4).integer, 4);
   ck_assert_int_eq(jit_call(j, h4, 5).integer, 5);
   ck_assert_int_eq(jit_call(j, h4, 1000).integer, 1000);

   jit_free(j);
}
END_TEST

START_TEST(test_bzero)
{
   jit_t *j = get_native_jit();

   const char *text1 =
      "    RECV      R0, #0          \n"
      "    RECV      R1, #1          \n"
      "    $BZERO    R1, [R0]        \n"
      "    RET                       \n";

   uint8_t mem[16];
   memset(mem, 'x', sizeof(mem));

   jit_handle_t h1 = assemble(j, text1, "bzero1", "pI");
   jit_call(j, h1, mem, sizeof(mem));
   for (int i = 0; i < sizeof(mem); i++)
      ck_assert_int_eq(mem[i], '\0');

   memset(mem, 'x', sizeof(mem));
   jit_call(j, h1, mem, 4);
   for (int i = 0; i < 4; i++)
      ck_assert_int_eq(mem[i], '\0');
   for (int i = 4; i < sizeof(mem); i++)
      ck_assert_int_eq(mem[i], 'x');

   memset(mem, 'x', sizeof(mem));
   jit_call(j, h1, mem, sizeof(mem));
   for (int i = 0; i < sizeof(mem); i++)
      ck_assert_int_eq(mem[i], '\0');

   jit_free(j);
}
END_TEST

START_TEST(test_copy)
{
   jit_t *j = get_native_jit();

   const char *text1 =
      "    RECV      R0, #0          \n"
      "    RECV      R1, #1          \n"
      "    RECV      R2, #2          \n"
      "    $COPY     R2, [R0], [R1]  \n"
      "    RET                       \n";

   char mem[16];
   memset(mem, 'x', sizeof(mem));

   jit_handle_t h1 = assemble(j, text1, "copy1", "ppI");
   jit_call(j, h1, mem, "hello", sizeof("hello"));
   ck_assert_str_eq(mem, "hello");

   jit_call(j, h1, mem, "world", sizeof("world"));
   ck_assert_str_eq(mem, "world");

   jit_call(j, h1, mem, "123456789", sizeof("123456789"));
   ck_assert_str_eq(mem, "123456789");

   jit_free(j);
}
END_TEST

START_TEST(test_shift)
{
   jit_t *j = get_native_jit();

   const char *text1 =
      "    RECV      R0, #0          \n"
      "    RECV      R1, #1          \n"
      "    SHL       R2, R0, R1      \n"
      "    SEND      #0, R2          \n"
      "    RET                       \n";

   jit_handle_t h1 = assemble(j, text1, "shift1", "ii");
   ck_assert_int_eq(jit_call(j, h1, 1, 1).integer, 2);
   ck_assert_int_eq(jit_call(j, h1, 2, 1).integer, 4);
   ck_assert_int_eq(jit_call(j, h1, 4, 5).integer, 128);

   const char *text2 =
      "    RECV      R0, #0          \n"
      "    RECV      R1, #1          \n"
      "    ASR       R2, R0, R1      \n"
      "    SEND      #0, R2          \n"
      "    RET                       \n";

   jit_handle_t h2 = assemble(j, text2, "shift2", "ii");
   ck_assert_int_eq(jit_call(j, h2, 8, 1).integer, 4);
   ck_assert_int_eq(jit_call(j, h2, 128, 5).integer, 4);
   ck_assert_int_eq(jit_call(j, h2, 4, 5).integer, 0);

   jit_free(j);
}
END_TEST

START_TEST(test_imm)
{
   jit_t *j = get_native_jit();

   const char *text1 =
      "    MOV    R0, #-1         \n"
      "    SEND   #0, R0          \n"
      "    RET                    \n";

   jit_handle_t h1 = assemble(j, text1, "imm1", "");
   ck_assert_int_eq(jit_call(j, h1).integer, -1);
   ck_assert_int_eq(jit_call(j, h1).integer, -1);

   const char *text2 =
      "    MOV    R0, #0x11adbeefcafebabe  \n"
      "    SEND   #0, R0                   \n"
      "    RET                             \n";

   jit_handle_t h2 = assemble(j, text2, "imm2", "");
   ck_assert_int_eq(jit_call(j, h2).integer, INT64_C(0x11adbeefcafebabe));
   ck_assert_int_eq(jit_call(j, h2).integer, INT64_C(0x11adbeefcafebabe));

   jit_free(j);
}
END_TEST

START_TEST(test_lea)
{
   jit_t *j = get_native_jit();

   const char *text1 =
      "    RECV   R0, #0          \n"
      "    LEA    R1, [R0+5]      \n"
      "    SEND   #0, R1          \n"
      "    RET                    \n";

   jit_handle_t h1 = assemble(j, text1, "lea1", "I");
   ck_assert_int_eq(jit_call(j, h1, 0).integer, 5);
   ck_assert_int_eq(jit_call(j, h1, 5).integer, 10);
   ck_assert_int_eq(jit_call(j, h1, 100).integer, 105);

   jit_free(j);
}
END_TEST

START_TEST(test_csel)
{
   jit_t *j = get_native_jit();

   const char *text1 =
      "    RECV     R0, #0          \n"
      "    RECV     R1, #1          \n"
      "    CMP.LT   R0, R1          \n"
      "    CSEL     R2, R1, R0      \n"
      "    SEND     #0, R2          \n"
      "    RET                      \n";

   jit_handle_t h1 = assemble(j, text1, "csel1", "ii");
   ck_assert_int_eq(jit_call(j, h1, 2, 3).integer, 3);
   ck_assert_int_eq(jit_call(j, h1, 100, 2000).integer, 2000);
   ck_assert_int_eq(jit_call(j, h1, -2, 5).integer, 5);

   jit_free(j);
}
END_TEST

START_TEST(test_lalloc)
{
   jit_t *j = get_native_jit();

   const char *text1 =
      "    RECV     R0, #0          \n"
      "    SHL      R4, R0, #2      \n"
      "    $LALLOC  R1, R4          \n"
      "    MOV      R2, #0          \n"
      "    CMP.GE   R2, R0          \n"
      "    JUMP.T   L2              \n"
      "L1: SHL      R5, R2, #2      \n"
      "    ADD      R3, R1, R5      \n"
      "    STORE.32 R2, [R3]        \n"
      "    ADD      R2, R2, #1      \n"
      "    CMP.LT   R2, R0          \n"
      "    JUMP.T   L1              \n"
      "L2: SEND     #0, R1          \n"
      "    RET                      \n";

   jit_handle_t h1 = assemble(j, text1, "lalloc1", "i");

   static const int trials[] = { 5, 5, 1, 10, 100 };
   for (int i = 0; i < ARRAY_LEN(trials); i++) {
      int *p = jit_call(j, h1, trials[i]).pointer;
      ck_assert_ptr_nonnull(p);
      for (int j = 0; j < trials[i]; j++)
         ck_assert_int_eq(p[j], j);
   }

   jit_free(j);
}
END_TEST

START_TEST(test_logical)
{
   jit_t *j = get_native_jit();

   const char *text1 =
      "    RECV     R0, #0          \n"
      "    RECV     R1, #1          \n"
      "    AND      R2, R1, R0      \n"
      "    SEND     #0, R2          \n"
      "    RET                      \n";

   jit_handle_t h1 = assemble(j, text1, "logical1", "ii");
   ck_assert_int_eq(jit_call(j, h1, 1, 0).integer, 0);
   ck_assert_int_eq(jit_call(j, h1, 1, 1).integer, 1);
   ck_assert_int_eq(jit_call(j, h1, 0, 0).integer, 0);
   ck_assert_int_eq(jit_call(j, h1, 0xffff, 0xff00).integer, 0xff00);

   const char *text2 =
      "    RECV     R0, #0          \n"
      "    RECV     R1, #1          \n"
      "    OR       R2, R1, R0      \n"
      "    SEND     #0, R2          \n"
      "    RET                      \n";

   jit_handle_t h2 = assemble(j, text2, "logical2", "ii");
   ck_assert_int_eq(jit_call(j, h2, 1, 0).integer, 1);
   ck_assert_int_eq(jit_call(j, h2, 1, 1).integer, 1);
   ck_assert_int_eq(jit_call(j, h2, 0, 0).integer, 0);
   ck_assert_int_eq(jit_call(j, h2, 1, 2).integer, 3);
   ck_assert_int_eq(jit_call(j, h2, 0x11223344, 0).integer, 0x11223344);

   const char *text3 =
      "    RECV     R0, #0          \n"
      "    NOT      R1, R0          \n"
      "    SEND     #0, R1          \n"
      "    RET                      \n";

   jit_handle_t h3 = assemble(j, text3, "logical3", "i");
   ck_assert_int_eq(jit_call(j, h3, 1).integer, 0);
   ck_assert_int_eq(jit_call(j, h3, 0).integer, 1);
   ck_assert_int_eq(jit_call(j, h3, 1).integer, 0);
   ck_assert_int_eq(jit_call(j, h3, 5).integer, 0);

   const char *text4 =
      "    RECV     R0, #0          \n"
      "    RECV     R1, #1          \n"
      "    XOR      R2, R1, R0      \n"
      "    SEND     #0, R2          \n"
      "    RET                      \n";

   jit_handle_t h4 = assemble(j, text4, "logical4", "ii");
   ck_assert_int_eq(jit_call(j, h4, 1, 0).integer, 1);
   ck_assert_int_eq(jit_call(j, h4, 1, 1).integer, 0);
   ck_assert_int_eq(jit_call(j, h4, 0, 0).integer, 0);
   ck_assert_int_eq(jit_call(j, h4, 0xbeef, 0xbeef).integer, 0);
   ck_assert_int_eq(jit_call(j, h4, INT64_C(-1), 0).integer, -1);

   const char *text5 =
      "    RECV     R0, #0          \n"
      "    AND.16   R1, R0, #0xf0f0 \n"
      "    SEND     #0, R1          \n"
      "    RET                      \n";

   jit_handle_t h5 = assemble(j, text5, "logical5", "i");
   ck_assert_int_eq(jit_call(j, h5, 0xff).integer, 0xf0);
   ck_assert_int_eq(jit_call(j, h5, 0xf0f).integer, 0);
   ck_assert_int_eq(jit_call(j, h5, 0x3333).integer, 0x3030);

   jit_free(j);
}
END_TEST

START_TEST(test_neg)
{
   jit_t *j = get_native_jit();

   const char *text1 =
      "    RECV     R0, #0          \n"
      "    NEG      R1, R0          \n"
      "    SEND     #0, R1          \n"
      "    RET                      \n";

   jit_handle_t h1 = assemble(j, text1, "neg1", "i");
   ck_assert_int_eq(jit_call(j, h1, 0).integer, 0);
   ck_assert_int_eq(jit_call(j, h1, 5).integer, -5);
   ck_assert_int_eq(jit_call(j, h1, -5).integer, 5);
   ck_assert_int_eq(jit_call(j, h1, 1000).integer, -1000);

   jit_free(j);
}
END_TEST

START_TEST(test_clamp)
{
   jit_t *j = get_native_jit();

   const char *text1 =
      "    RECV     R0, #0          \n"
      "    CLAMP    R1, R0          \n"
      "    SEND     #0, R1          \n"
      "    RET                      \n";

   jit_handle_t h1 = assemble(j, text1, "clamp1", "i");
   ck_assert_int_eq(jit_call(j, h1, 0).integer, 0);
   ck_assert_int_eq(jit_call(j, h1, 5).integer, 5);
   ck_assert_int_eq(jit_call(j, h1, -5).integer, 0);
   ck_assert_int_eq(jit_call(j, h1, 1000).integer, 1000);
   ck_assert_int_eq(jit_call(j, h1, -1000).integer, 0);

   jit_free(j);
}
END_TEST

START_TEST(test_case)
{
   jit_t *j = get_native_jit();

   const char *text1 =
      "    RECV     R0, #0          \n"
      "    $CASE    R0, #1, L1      \n"
      "    $CASE    R0, #2, L2      \n"
      "    $CASE    R0, #3, L3      \n"
      "    RET                      \n"
      "L1: SEND     #0, #10         \n"
      "    RET                      \n"
      "L2: SEND     #0, #20         \n"
      "    RET                      \n"
      "L3: SEND     #0, #30         \n"
      "    RET                      \n";

   jit_handle_t h1 = assemble(j, text1, "case1", "i");
   ck_assert_int_eq(jit_call(j, h1, 1).integer, 10);
   ck_assert_int_eq(jit_call(j, h1, 2).integer, 20);
   ck_assert_int_eq(jit_call(j, h1, 3).integer, 30);
   ck_assert_int_eq(jit_call(j, h1, 4).integer, 4);

   jit_free(j);
}
END_TEST

START_TEST(test_exp)
{
   jit_t *j = get_native_jit();

   const char *text1 =
      "    RECV     R0, #0          \n"
      "    RECV     R1, #1          \n"
      "    $EXP     R2, R0, R1      \n"
      "    SEND     #0, R2          \n"
      "    RET                      \n";

   jit_handle_t h1 = assemble(j, text1, "exp1", "II");
   ck_assert_int_eq(jit_call(j, h1, 2, 3).integer, 8);
   ck_assert_int_eq(jit_call(j, h1, 2, 4).integer, 16);
   ck_assert_int_eq(jit_call(j, h1, 3, 5).integer, 243);
   ck_assert_int_eq(jit_call(j, h1, 7, 4).integer, 2401);
   ck_assert_int_eq(jit_call(j, h1, 666, 1).integer, 666);
   ck_assert_int_eq(jit_call(j, h1, 99, 0).integer, 1);

   jit_free(j);
}
END_TEST

START_TEST(test_float)
{
   jit_t *j = get_native_jit();

   const char *text1 =
      "    RECV     R0, #0          \n"
      "    RECV     R1, #1          \n"
      "    RECV     R2, #2          \n"
      "    FMUL     R3, R1, R0      \n"
      "    FADD     R4, R3, R2      \n"
      "    SEND     #0, R4          \n"
      "    RET                      \n";

   jit_handle_t h1 = assemble(j, text1, "float1", "fff");
   ck_assert_double_eq(jit_call(j, h1, 2.0, 3.0, 0.0).real, 6.0);
   ck_assert_double_eq(jit_call(j, h1, 5.0, 3.0, 0.0).real, 15.0);
   ck_assert_double_eq(jit_call(j, h1, -2.0, 7.0, 0.0).real, -14.0);
   ck_assert_double_eq(jit_call(j, h1, 2.0, 3.0, 1.0).real, 7.0);
   ck_assert_double_eq(jit_call(j, h1, 2.5, 1.0, 0.5).real, 3.0);

   const char *text2 =
      "    RECV     R0, #0          \n"
      "    FDIV     R1, R0, %2.0    \n"
      "    SEND     #0, R1          \n"
      "    RET                      \n";

   jit_handle_t h2 = assemble(j, text2, "float2", "f");
   ck_assert_double_eq(jit_call(j, h2, 2.0).real, 1.0);
   ck_assert_double_eq(jit_call(j, h2, 1.0).real, 0.5);
   ck_assert_double_eq(jit_call(j, h2, 0.5).real, 0.25);

   const char *text3 =
      "    RECV     R0, #0          \n"
      "    RECV     R1, #1          \n"
      "    FNEG     R1, R1          \n"
      "    FSUB     R2, R0, R1      \n"
      "    SEND     #0, R2          \n"
      "    RET                      \n";

   jit_handle_t h3 = assemble(j, text3, "float3", "ff");
   ck_assert_double_eq(jit_call(j, h3, 2.0, 3.0).real, 5.0);
   ck_assert_double_eq(jit_call(j, h3, 2.0, -3.0).real, -1.0);
   ck_assert_double_eq(jit_call(j, h3, -1.0, 0.5).real, -0.5);

   const char *text4 =
      "    RECV     R0, #0          \n"
      "    RECV     R1, #1          \n"
      "    FCMP.LT  R0, R1          \n"
      "    CSET     R2              \n"
      "    SEND     #0, R2          \n"
      "    RET                      \n";

   jit_handle_t h4 = assemble(j, text4, "float4", "ff");
   ck_assert_double_eq(jit_call(j, h4, 2.0, 3.0).integer, 1);
   ck_assert_double_eq(jit_call(j, h4, 0.0, 0.1).integer, 1);
   ck_assert_double_eq(jit_call(j, h4, -5.0, 0.0).integer, 1);
   ck_assert_double_eq(jit_call(j, h4, 5.0, 0.0).integer, 0);

   const char *text5 =
      "    RECV     R0, #0          \n"
      "    FCVTNS   R1, R0          \n"
      "    SEND     #0, R1          \n"
      "    RET                      \n";

   jit_handle_t h5 = assemble(j, text5, "float5", "f");
   ck_assert_int_eq(jit_call(j, h5, 2.0).integer, 2);
   ck_assert_int_eq(jit_call(j, h5, 0.7).integer, 1.0);
   ck_assert_int_eq(jit_call(j, h5, 0.1).integer, 0.0);
   ck_assert_int_eq(jit_call(j, h5, -0.1).integer, 0.0);
   ck_assert_int_eq(jit_call(j, h5, -0.8).integer, -1);

   const int tie = jit_call(j, h5, 0.5).integer;
   fail_unless(tie == 0 || tie == 1);   // LRM allows either result

   const char *text6 =
      "    RECV     R0, #0          \n"
      "    SCVTF    R1, R0          \n"
      "    SEND     #0, R1          \n"
      "    RET                      \n";

   jit_handle_t h6 = assemble(j, text6, "float6", "i");
   ck_assert_double_eq(jit_call(j, h6, 2).real, 2.0);
   ck_assert_double_eq(jit_call(j, h6, 0).real, 0.0);
   ck_assert_double_eq(jit_call(j, h6, -5).real, -5.0);
   ck_assert_double_eq(jit_call(j, h6, 123).real, 123.0);

   const char *text7 =
      "    RECV     R0, #0          \n"
      "    RECV     R1, #1          \n"
      "    $FEXP    R2, R0, R1      \n"
      "    SEND     #0, R2          \n"
      "    RET                      \n";

   jit_handle_t h7 = assemble(j, text7, "float7", "ff");
   ck_assert_double_eq(jit_call(j, h7, 2.0, 3.0).real, 8.0);
   ck_assert_double_eq(jit_call(j, h7, 1.0, 3.0).real, 1.0);
   ck_assert_double_eq(jit_call(j, h7, 2.0, -1.0).real, 0.5);
   ck_assert_double_eq(jit_call(j, h7, 0.5, 2.0).real, 0.25);

   jit_free(j);
}
END_TEST

START_TEST(test_ccmp)
{
   jit_t *j = get_native_jit();

   const char *text1 =
      "    RECV     R0, #0          \n"
      "    CMP.GE   R0, #0          \n"
      "    CCMP.LE  R0, #10         \n"
      "    CSET     R1              \n"
      "    SEND     #0, R1          \n"
      "    RET                      \n";

   jit_handle_t h1 = assemble(j, text1, "ccmp1", "i");
   ck_assert_int_eq(jit_call(j, h1, 0).integer, 1);
   ck_assert_int_eq(jit_call(j, h1, 5).integer, 1);
   ck_assert_int_eq(jit_call(j, h1, 15).integer, 0);
   ck_assert_int_eq(jit_call(j, h1, -6).integer, 0);

   jit_free(j);
}
END_TEST

START_TEST(test_memset)
{
   jit_t *j = get_native_jit();

   const char *text1 =
      "    RECV        R0, #0        \n"
      "    RECV        R1, #1        \n"
      "    RECV        R2, #2        \n"
      "    $MEMSET.8   R2, [R0], R1  \n"
      "    RET                       \n";

   char mem8[16];
   memset(mem8, 'x', sizeof(mem8));

   jit_handle_t h1 = assemble(j, text1, "memset1", "pII");
   jit_call(j, h1, mem8, 42, sizeof(mem8));
   for (int i = 0; i < sizeof(mem8); i++)
      ck_assert_int_eq(mem8[i], 42);
   jit_call(j, h1, mem8, 55, sizeof(mem8));
   for (int i = 0; i < sizeof(mem8); i++)
      ck_assert_int_eq(mem8[i], 55);

   const char *text2 =
      "    RECV        R0, #0        \n"
      "    RECV        R1, #1        \n"
      "    RECV        R2, #2        \n"
      "    $MEMSET.16  R2, [R0], R1  \n"
      "    RET                       \n";

   uint16_t mem16[16];

   jit_handle_t h2 = assemble(j, text2, "memset2", "pII");
   jit_call(j, h2, mem16, 42, sizeof(mem16));
   for (int i = 0; i < ARRAY_LEN(mem16); i++)
      ck_assert_int_eq(mem16[i], 42);
   jit_call(j, h2, mem16, 0xaabb, sizeof(mem16));
   for (int i = 0; i < ARRAY_LEN(mem16); i++)
      ck_assert_int_eq(mem16[i], 0xaabb);
   jit_call(j, h2, mem16, 0x1234, 4);
   for (int i = 0; i < ARRAY_LEN(mem16); i++)
      ck_assert_int_eq(mem16[i], i < 2 ? 0x1234 : 0xaabb);

   const char *text3 =
      "    RECV        R0, #0        \n"
      "    RECV        R1, #1        \n"
      "    RECV        R2, #2        \n"
      "    $MEMSET.32  R2, [R0], R1  \n"
      "    RET                       \n";

   uint32_t mem32[16];

   jit_handle_t h3 = assemble(j, text3, "memset3", "pII");
   jit_call(j, h3, mem32, 42, sizeof(mem32));
   for (int i = 0; i < ARRAY_LEN(mem32); i++)
      ck_assert_int_eq(mem32[i], 42);
   jit_call(j, h3, mem32, 0xaabbccdd, sizeof(mem32));
   for (int i = 0; i < ARRAY_LEN(mem32); i++)
      ck_assert_int_eq(mem32[i], 0xaabbccdd);
   jit_call(j, h3, mem32, 0x12345678, 4);
   for (int i = 0; i < ARRAY_LEN(mem32); i++)
      ck_assert_int_eq(mem32[i], i == 0 ? 0x12345678 : 0xaabbccdd);

   const char *text4 =
      "    RECV        R0, #0        \n"
      "    RECV        R1, #1        \n"
      "    RECV        R2, #2        \n"
      "    $MEMSET.64  R2, [R0], R1  \n"
      "    RET                       \n";

   uint64_t mem64[16];

   jit_handle_t h4 = assemble(j, text4, "memset4", "pII");
   jit_call(j, h4, mem64, 42, sizeof(mem64));
   for (int i = 0; i < ARRAY_LEN(mem64); i++)
      ck_assert_int_eq(mem64[i], 42);
   jit_call(j, h4, mem64, 0xaabbccdd, sizeof(mem64));
   for (int i = 0; i < ARRAY_LEN(mem64); i++)
      ck_assert_int_eq(mem64[i], 0xaabbccdd);
   jit_call(j, h4, mem64, 0x12345678, 8);
   for (int i = 0; i < ARRAY_LEN(mem64); i++)
      ck_assert_int_eq(mem64[i], i == 0 ? 0x12345678 : 0xaabbccdd);

   jit_free(j);
}
END_TEST

START_TEST(test_move)
{
   jit_t *j = get_native_jit();

   const char *text1 =
      "    RECV      R0, #0          \n"
      "    RECV      R1, #1          \n"
      "    RECV      R2, #2          \n"
      "    $MOVE     R2, [R0], [R1]  \n"
      "    RET                       \n";

   char mem[16];
   strncpy(mem, "hello", sizeof(mem));

   jit_handle_t h1 = assemble(j, text1, "move1", "ppI");
   jit_call(j, h1, mem, mem + 2, sizeof("hello"));
   ck_assert_str_eq(mem, "llo");

   strncpy(mem, "hello", sizeof(mem));

   jit_call(j, h1, mem, mem + 2, sizeof("hello"));
   ck_assert_str_eq(mem, "llo");

   strncpy(mem, "hello", sizeof(mem));

   jit_call(j, h1, mem + 2, mem, sizeof("world"));
   ck_assert_str_eq(mem, "hehello");

   memset(mem, 'x', sizeof(mem));

   jit_call(j, h1, mem, "world", sizeof("world"));
   ck_assert_str_eq(mem, "world");

   jit_call(j, h1, mem, "123456789", sizeof("123456789"));
   ck_assert_str_eq(mem, "123456789");

   jit_free(j);
}
END_TEST

START_TEST(test_sub)
{
   jit_t *j = get_native_jit();

   const char *text1 =
      "    RECV    R0, #0          \n"
      "    RECV    R1, #1          \n"
      "    SUB.8   R2, R0, R1      \n"
      "    SEND    #0, R2          \n"
      "    RET                     \n";

   jit_handle_t h1 = assemble(j, text1, "sub1", "ii");

   ck_assert_int_eq(jit_call(j, h1, 1, 2).integer, -1);
   ck_assert_int_eq(jit_call(j, h1, 127, 1).integer, 126);
   ck_assert_int_eq(jit_call(j, h1, 128, 1).integer, 127);

   const char *text2 =
      "    RECV     R0, #0         \n"
      "    SUB.O.8  R1, R0, #1     \n"
      "    CSET     R2             \n"
      "    SEND     #0, R2         \n"
      "    RET                     \n";

   jit_handle_t h2 = assemble(j, text2, "sub2", "i");

   ck_assert_int_eq(jit_call(j, h2, 127).integer, 0);
   ck_assert_int_eq(jit_call(j, h2, -127).integer, 0);
   ck_assert_int_eq(jit_call(j, h2, 5).integer, 0);
   ck_assert_int_eq(jit_call(j, h2, -128).integer, 1);

   const char *text3 =
      "    RECV      R0, #0        \n"
      "    SUB.O.16  R1, R0, #1    \n"
      "    CSET      R2            \n"
      "    SEND      #0, R2        \n"
      "    RET                     \n";

   jit_handle_t h3 = assemble(j, text3, "sub3", "i");

   ck_assert_int_eq(jit_call(j, h3, INT16_MAX).integer, 0);
   ck_assert_int_eq(jit_call(j, h3, INT16_MIN).integer, 1);
   ck_assert_int_eq(jit_call(j, h3, 5).integer, 0);
   ck_assert_int_eq(jit_call(j, h3, INT16_MAX).integer, 0);

   const char *text4 =
      "    RECV      R0, #0        \n"
      "    RECV      R1, #1        \n"
      "    SUB.O.16  R2, R0, R1    \n"
      "    CSET      R3            \n"
      "    SEND      #0, R3        \n"
      "    RET                     \n";

   jit_handle_t h4 = assemble(j, text4, "sub4", "ii");

   ck_assert_int_eq(jit_call(j, h4, 1, 1).integer, 0);
   ck_assert_int_eq(jit_call(j, h4, -128, 1).integer, 0);
   ck_assert_int_eq(jit_call(j, h4, INT16_MIN, 1).integer, 1);

   const char *text5 =
      "    RECV      R0, #0        \n"
      "    RECV      R1, #1        \n"
      "    SUB.O.8   R2, R0, R1    \n"
      "    CSET      R3            \n"
      "    SEND      #0, R3        \n"
      "    RET                     \n";

   jit_handle_t h5 = assemble(j, text5, "sub5", "ii");

   ck_assert_int_eq(jit_call(j, h5, 1, 1).integer, 0);
   ck_assert_int_eq(jit_call(j, h5, -128, 1).integer, 1);
   ck_assert_int_eq(jit_call(j, h5, 127, 1).integer, 0);

   jit_free(j);
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
   tcase_add_test(tc, test_stack);
   tcase_add_test(tc, test_bzero);
   tcase_add_test(tc, test_copy);
   tcase_add_test(tc, test_shift);
   tcase_add_test(tc, test_imm);
   tcase_add_test(tc, test_lea);
   tcase_add_test(tc, test_csel);
   tcase_add_test(tc, test_lalloc);
   tcase_add_test(tc, test_logical);
   tcase_add_test(tc, test_neg);
   tcase_add_test(tc, test_clamp);
   tcase_add_test(tc, test_case);
   tcase_add_test(tc, test_exp);
   tcase_add_test(tc, test_float);
   tcase_add_test(tc, test_ccmp);
   tcase_add_test(tc, test_memset);
   tcase_add_test(tc, test_move);
   tcase_add_test(tc, test_sub);
   suite_add_tcase(s, tc);

   return s;
}
