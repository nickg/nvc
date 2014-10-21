#include "test_util.h"
#include "phase.h"
#include "vcode.h"

typedef struct {
   vcode_op_t    op;
   const char   *func;
   int64_t       value;
   vcode_cmp_t   cmp;
   vcode_block_t target;
   const char   *name;
} check_bb_t;

#define vcode_fail_unless(vu, expr) do {                \
      if (!(expr)) {                                    \
         vcode_dump(vu);                                \
         fail_unless(expr);                             \
      }                                                 \
   } while (0);

static void check_bb(int bb, const check_bb_t *expect, int len)
{
   fail_unless(bb < vcode_count_blocks());
   vcode_select_block(bb);

   const int nops = vcode_count_ops();

   int eptr = 0, actual = nops;
   for (int i = 0; i < nops && eptr < len; i++) {
      const vcode_op_t vop = vcode_get_op(i);
      if (vop == VCODE_OP_COMMENT) {
         actual--;
         continue;
      }

      const check_bb_t *e = &(expect[eptr++]);

      if (vop != e->op) {
         vcode_dump();
         fail("expected op %d in block %d to be %s but was %s",
              i, bb, vcode_op_string(e->op), vcode_op_string(vop));
      }

      switch (e->op) {
      case VCODE_OP_FCALL:
         if ((e->func != NULL) && !icmp(vcode_get_func(i), e->func)) {
            vcode_dump();
            fail("expected op %d in block %d to call %s but calls %s",
                 i, bb, e->func, istr(vcode_get_func(i)));
         }
         break;

      case VCODE_OP_CONST:
         if (e->value != vcode_get_value(i)) {
            vcode_dump();
            fail("expected op %d in block %d to have constant %d but has %d",
                 i, bb, e->value, vcode_get_value(i));
         }
         break;

      case VCODE_OP_CMP:
         if (e->cmp != vcode_get_cmp(i)) {
            vcode_dump();
            fail("expected op %d in block %d to have comparison %d but has %d",
                 i, bb, e->value, vcode_get_cmp(i));
         }
         break;

      case VCODE_OP_ASSERT:
         break;

      case VCODE_OP_WAIT:
         if (e->target != vcode_get_target(i)) {
            vcode_dump();
            fail("expected op %d in block %d to have wait target %d but has %d",
                 i, bb, e->target, vcode_get_target(i));
         }
         break;

      case VCODE_OP_JUMP:
         if (e->target != vcode_get_target(i)) {
            vcode_dump();
            fail("expected op %d in block %d to have jump target %d but has %d",
                 i, bb, e->target, vcode_get_target(i));
         }
         break;

      case VCODE_OP_STORE:
         {
            ident_t name = vcode_var_name(vcode_get_address(i));
            if (name != ident_new(e->name)) {
               vcode_dump();
               fail("expected op %d in block %d to have address name %s but "
                    "has %s", i, bb, e->name, istr(name));
            }
         }
         break;

      default:
         fail("cannot check op %s", vcode_op_string(e->op));
      }
   }

   if (actual != eptr) {
      vcode_dump();
      fail("expected %d ops in block %d but have %d", len, bb, actual);
   }
}

START_TEST(test_wait1)
{
   input_from_file(TESTDIR "/lower/wait1.vhd");

   const error_t expect[] = {
      { -1, NULL }
   };
   expect_errors(expect);

   tree_t e = run_elab();
   lower_unit(e);

   vcode_unit_t v0 = tree_code(tree_stmt(e, 0));
   vcode_select_unit(v0);

   const check_bb_t bb0[] = {
      { VCODE_OP_JUMP, .target = 1 }
   };

   check_bb(0, bb0, ARRAY_LEN(bb0));

   const check_bb_t bb1[] = {
      { VCODE_OP_FCALL, .func = "_std_standard_now" },
      { VCODE_OP_CONST, .value = 0 },
      { VCODE_OP_CMP,   .cmp = VCODE_CMP_EQ },
      { VCODE_OP_ASSERT },
      { VCODE_OP_CONST, .value = 1000000 },
      { VCODE_OP_WAIT,  .target = 2 }
   };

   check_bb(1, bb1, ARRAY_LEN(bb1));

   const check_bb_t bb2[] = {
      { VCODE_OP_FCALL, .func = "_std_standard_now" },
      { VCODE_OP_CONST, .value = 1000000 },
      { VCODE_OP_CMP,   .cmp = VCODE_CMP_EQ },
      { VCODE_OP_ASSERT },
      { VCODE_OP_CONST, .value = 1 },
      { VCODE_OP_WAIT,  .target = 3 }
   };

   check_bb(2, bb2, ARRAY_LEN(bb2));

   const check_bb_t bb3[] = {
      { VCODE_OP_FCALL, .func = "_std_standard_now" },
      { VCODE_OP_CONST, .value = 1000001 },
      { VCODE_OP_CMP,   .cmp = VCODE_CMP_EQ },
      { VCODE_OP_ASSERT },
      { VCODE_OP_WAIT,  .target = 4 }
   };

   check_bb(3, bb3, ARRAY_LEN(bb3));

   const check_bb_t bb4[] = {
      { VCODE_OP_JUMP,  .target = 1 }
   };

   check_bb(4, bb4, ARRAY_LEN(bb4));
}
END_TEST

START_TEST(test_assign1)
{
   input_from_file(TESTDIR "/lower/assign1.vhd");

   const error_t expect[] = {
      { -1, NULL }
   };
   expect_errors(expect);

   tree_t e = run_elab();
   lower_unit(e);

   vcode_unit_t v0 = tree_code(tree_stmt(e, 0));
   vcode_select_unit(v0);

   fail_unless(vcode_count_vars() == 2);

   const check_bb_t bb0[] = {
      { VCODE_OP_CONST, .value = 64 },
      { VCODE_OP_STORE, .name = "X" },
      { VCODE_OP_CONST, .value = -4 },
      { VCODE_OP_STORE, .name = "Y" },
      { VCODE_OP_JUMP,  .target = 1 }
   };

   check_bb(0, bb0, ARRAY_LEN(bb0));

}
END_TEST

int main(void)
{
   Suite *s = suite_create("lower");

   TCase *tc = nvc_unit_test();
   tcase_add_test(tc, test_wait1);
   tcase_add_test(tc, test_assign1);
   suite_add_tcase(s, tc);

   return nvc_run_test(s);
}
