#include "test_util.h"
#include "phase.h"
#include "vcode.h"
#include "common.h"

#include <inttypes.h>

typedef struct {
   vcode_op_t    op;
   const char   *func;
   int64_t       value;
   vcode_cmp_t   cmp;
   vcode_block_t target;
   vcode_block_t target_else;
   const char   *name;
   bool          delay;
   int64_t       low;
   int64_t       high;
   int           length;
   int           args;
   int           dim;
   int           hops;
   int           field;
   int           subkind;
   uint32_t      tag;
   double        real;
} check_bb_t;

#define CAT(x, y) x##y
#define CHECK_BB(n) check_bb(n, CAT(bb, n), ARRAY_LEN(CAT(bb, n)))
#define EXPECT_BB(n) const check_bb_t CAT(bb, n)[]

static void check_bb(int bb, const check_bb_t *expect, int len)
{
   fail_unless(bb < vcode_count_blocks());
   vcode_select_block(bb);

   const int nops = vcode_count_ops();

   int eptr = 0, actual = nops;
   for (int i = 0; i < nops && eptr < len; i++) {
      const vcode_op_t vop = vcode_get_op(i);
      if (vop == VCODE_OP_COMMENT || vop == VCODE_OP_DEBUG_INFO) {
         actual--;
         continue;
      }

      const check_bb_t *e = &(expect[eptr++]);

      if (vop != e->op) {
         vcode_dump_with_mark(i);
         fail("expected op %d in block %d to be %s but was %s",
              i, bb, vcode_op_string(e->op), vcode_op_string(vop));
      }

      switch (e->op) {
      case VCODE_OP_PCALL:
         if (e->target != vcode_get_target(i, 0)) {
            vcode_dump_with_mark(i);
            fail("expected op %d in block %d to have resume target %d but "
                 "has %d", i, bb, e->target, vcode_get_target(i, 0));
         }
         // Fall-through
      case VCODE_OP_FCALL:
      case VCODE_OP_NESTED_FCALL:
         if (e->func != NULL) {
            bool bad;
            if (e->func[0] == '*')
               bad = strstr(istr(vcode_get_func(i)), e->func + 1) == NULL;
            else
               bad = !icmp(vcode_get_func(i), e->func);

            if (bad) {
               vcode_dump_with_mark(i);
               fail("expected op %d in block %d to call %s but calls %s",
                    i, bb, e->func, istr(vcode_get_func(i)));
            }
         }
         else if (e->args != vcode_count_args(i)) {
            vcode_dump_with_mark(i);
            fail("expected op %d in block %d to have %d arguments but has %d",
                 i, bb, e->args, vcode_count_args(i));
         }
         break;

      case VCODE_OP_CONST:
         if (e->value != vcode_get_value(i)) {
            vcode_dump_with_mark(i);
            vcode_dump_with_mark(i);
            fail("expected op %d in block %d to have constant %d but has %d",
                 i, bb, e->value, vcode_get_value(i));
         }
         break;

      case VCODE_OP_CONST_REAL:
         if (e->real != vcode_get_real(i)) {
            vcode_dump_with_mark(i);
            fail("expected op %d in block %d to have constant %lf but has %lf",
                 i, bb, e->real, vcode_get_real(i));
         }
         break;

      case VCODE_OP_CMP:
         if (e->cmp != vcode_get_cmp(i)) {
            vcode_dump_with_mark(i);
            fail("expected op %d in block %d to have comparison %d but has %d",
                 i, bb, e->value, vcode_get_cmp(i));
         }
         break;

      case VCODE_OP_ADDI:
         if (e->value != vcode_get_value(i)) {
            vcode_dump_with_mark(i);
            fail("expected op %d in block %d to have value %"PRIi64" but has "
                 "%"PRIi64, i, bb, e->value, vcode_get_value(i));
         }
         break;

      case VCODE_OP_ASSERT:
      case VCODE_OP_REPORT:
      case VCODE_OP_RETURN:
      case VCODE_OP_IMAGE:
      case VCODE_OP_UNWRAP:
      case VCODE_OP_ARRAY_SIZE:
      case VCODE_OP_NULL:
      case VCODE_OP_RANGE_NULL:
         break;

      case VCODE_OP_UARRAY_LEFT:
      case VCODE_OP_UARRAY_RIGHT:
      case VCODE_OP_UARRAY_DIR:
      case VCODE_OP_UARRAY_LEN:
         if (e->dim != vcode_get_dim(i)) {
            vcode_dump_with_mark(i);
            fail("expected op %d in block %d to have dimension %d but has %d",
                 i, bb, e->dim, vcode_get_dim(i));
         }
         break;

      case VCODE_OP_WAIT:
         if (e->target != vcode_get_target(i, 0)) {
            vcode_dump_with_mark(i);
            fail("expected op %d in block %d to have wait target %d but has %d",
                 i, bb, e->target, vcode_get_target(i, 0));
         }
         else if (e->delay && vcode_get_arg(i, 0) == VCODE_INVALID_REG) {
            vcode_dump_with_mark(i);
            fail("expected op %d in block %d to have wait delay", i, bb);
         }
         break;

      case VCODE_OP_COND:
         if (e->target_else != vcode_get_target(i, 1)) {
            vcode_dump_with_mark(i);
            fail("expected op %d in block %d to have else target %d but has %d",
                 i, bb, e->target_else, vcode_get_target(i, 1));
         }
         // Fall-through

      case VCODE_OP_JUMP:
         if (e->target != vcode_get_target(i, 0)) {
            vcode_dump_with_mark(i);
            fail("expected op %d in block %d to have jump target %d but has %d",
                 i, bb, e->target, vcode_get_target(i, 0));
         }
         break;

      case VCODE_OP_STORE:
      case VCODE_OP_LOAD:
      case VCODE_OP_INDEX:
         {
            ident_t name = vcode_var_name(vcode_get_address(i));
            if (name != ident_new(e->name)) {
               vcode_dump_with_mark(i);
               fail("expected op %d in block %d to have address name %s but "
                    "has %s", i, bb, e->name, istr(name));
            }
         }
         break;

      case VCODE_OP_NETS:
      case VCODE_OP_RESOLVED_ADDRESS:
      case VCODE_OP_NEEDS_LAST_VALUE:
         {
            ident_t name = vcode_signal_name(vcode_get_signal(i));
            if (name != ident_new(e->name)) {
               vcode_dump_with_mark(i);
               fail("expected op %d in block %d to have signal name %s but "
                    "has %s", i, bb, e->name, istr(name));
            }
         }
         break;

      case VCODE_OP_ADD:
      case VCODE_OP_SUB:
      case VCODE_OP_MUL:
      case VCODE_OP_DIV:
      case VCODE_OP_EXP:
      case VCODE_OP_MOD:
      case VCODE_OP_REM:
      case VCODE_OP_NEG:
      case VCODE_OP_ABS:
      case VCODE_OP_CAST:
      case VCODE_OP_OR:
      case VCODE_OP_NOR:
      case VCODE_OP_AND:
      case VCODE_OP_NOT:
      case VCODE_OP_LOAD_INDIRECT:
      case VCODE_OP_STORE_INDIRECT:
      case VCODE_OP_SCHED_WAVEFORM:
      case VCODE_OP_SELECT:
      case VCODE_OP_SET_INITIAL:
      case VCODE_OP_ALLOC_DRIVER:
      case VCODE_OP_EVENT:
      case VCODE_OP_ACTIVE:
      case VCODE_OP_CONST_RECORD:
      case VCODE_OP_COPY:
      case VCODE_OP_MEMCMP:
      case VCODE_OP_MEMSET:
      case VCODE_OP_WRAP:
      case VCODE_OP_VEC_LOAD:
      case VCODE_OP_DYNAMIC_BOUNDS:
      case VCODE_OP_NEW:
      case VCODE_OP_ALL:
      case VCODE_OP_DEALLOCATE:
      case VCODE_OP_CASE:
      case VCODE_OP_HEAP_SAVE:
      case VCODE_OP_HEAP_RESTORE:
         break;

      case VCODE_OP_CONST_ARRAY:
         if (vcode_count_args(i) != e->length) {
            vcode_dump_with_mark(i);
            fail("expected op %d in block %d to have %d array elements but "
                 "has %d", i, bb, e->length, vcode_count_args(i));
         }
         break;

      case VCODE_OP_BOUNDS:
         {
            vcode_type_t bounds = vcode_get_type(i);
            if (vtype_kind(bounds) == VCODE_TYPE_INT) {
               if (e->low != vtype_low(bounds)) {
                  vcode_dump_with_mark(i);
                  fail("expect op %d in block %d to have low bound %"PRIi64
                       " but has %"PRIi64, i, bb, e->low, vtype_low(bounds));
               }
               else if (e->high != vtype_high(bounds)) {
                  vcode_dump_with_mark(i);
                  fail("expect op %d in block %d to have high bound %"PRIi64
                       " but has %"PRIi64, i, bb, e->high, vtype_high(bounds));
               }
            }
         }
         break;

      case VCODE_OP_VAR_UPREF:
         {
            vcode_state_t state;
            vcode_state_save(&state);

            vcode_var_t address = vcode_get_address(i);
            int hops = vcode_get_hops(i);
            while (hops--)
               vcode_select_unit(vcode_unit_context());

            if (!icmp(vcode_var_name(address), e->name)) {
               vcode_dump_with_mark(i);
               fail("expect op %d in block %d to have address %s"
                    " but has %s", i, bb, e->name, istr(vcode_var_name(address)));
            }

            vcode_state_restore(&state);
         }
         // Fall-through

      case VCODE_OP_PARAM_UPREF:
         if (vcode_get_hops(i) != e->hops) {
            vcode_dump_with_mark(i);
            fail("expect op %d in block %d to have hop count %d"
                 " but has %d", i, bb, e->hops, vcode_get_hops(i));
         }
         break;

      case VCODE_OP_RECORD_REF:
         if (vcode_get_field(i) != e->field) {
            vcode_dump_with_mark(i);
            fail("expect op %d in block %d to have field %d"
                 " but has %d", i, bb, e->field, vcode_get_field(i));
         }
         break;

      case VCODE_OP_SCHED_EVENT:
      case VCODE_OP_ALLOCA:
      case VCODE_OP_INDEX_CHECK:
         if (vcode_get_subkind(i) != e->subkind) {
            vcode_dump_with_mark(i);
            fail("expected op %d in block %d to have subkind %x but "
                 "has %x", i, bb, e->subkind, vcode_get_subkind(i));
         }
         break;

      case VCODE_OP_RESUME:
         if ((e->func != NULL) && !icmp(vcode_get_func(i), e->func)) {
            vcode_dump_with_mark(i);
            fail("expected op %d in block %d to call %s but calls %s",
                 i, bb, e->func, istr(vcode_get_func(i)));
         }
         break;

      case VCODE_OP_COVER_COND:
         if (vcode_get_subkind(i) != e->subkind) {
            vcode_dump_with_mark(i);
            fail("expected op %d in block %d to have sub cond %x but "
                 "has %x", i, bb, e->subkind, vcode_get_subkind(i));
         }
         // Fall-through

      case VCODE_OP_COVER_STMT:
         if (e->tag != vcode_get_tag(i)) {
            vcode_dump_with_mark(i);
            fail("expected op %d in block %d to have cover tag %d but has %d",
                 i, bb, e->tag, vcode_get_tag(i));
         }
         break;

      case VCODE_OP_IMAGE_MAP:
         {
            image_map_t map;
            vcode_get_image_map(i, &map);
            if (!icmp(map.name, e->name)) {
               vcode_dump_with_mark(i);
               fail("expected op %d in block %d to have image map name %s but "
                    "has %s", i, bb, e->name, istr(map.name));
            }
         }
         break;

      default:
         fail("cannot check op %s", vcode_op_string(e->op));
      }
   }

   if (actual != eptr) {
      vcode_dump_with_mark(len + nops - actual);
      fail("expected %d ops in block %d but have %d", len, bb, actual);
   }
}

static vcode_unit_t find_unit(tree_t t)
{
   ident_t name = tree_attr_str(t, mangled_i);
   if (name == NULL)
      name = tree_ident(t);
   vcode_unit_t vu = vcode_find_unit(name);
   if (vu == NULL)
      fail("missing vcode unit for %s", istr(name));
   return vu;
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

   vcode_unit_t v0 = find_unit(tree_stmt(e, 0));
   vcode_select_unit(v0);

   const check_bb_t bb0[] = {
      { VCODE_OP_RETURN }
   };

   CHECK_BB(0);

   const check_bb_t bb1[] = {
      { VCODE_OP_CONST, .value = 2 },
      { VCODE_OP_FCALL, .func = "_std_standard_now" },
      { VCODE_OP_CONST, .value = 0 },
      { VCODE_OP_CMP,   .cmp = VCODE_CMP_EQ },
      { VCODE_OP_ASSERT },
      { VCODE_OP_CONST, .value = 1000000 },
      { VCODE_OP_WAIT,  .target = 2 }
   };

   CHECK_BB(1);

   const check_bb_t bb2[] = {
      { VCODE_OP_CONST, .value = 2 },
      { VCODE_OP_FCALL, .func = "_std_standard_now" },
      { VCODE_OP_CONST, .value = 1000000 },
      { VCODE_OP_CMP,   .cmp = VCODE_CMP_EQ },
      { VCODE_OP_ASSERT },
      { VCODE_OP_CONST, .value = 1 },
      { VCODE_OP_WAIT,  .target = 3 }
   };

   CHECK_BB(2);

   const check_bb_t bb3[] = {
      { VCODE_OP_CONST, .value = 2 },
      { VCODE_OP_FCALL, .func = "_std_standard_now" },
      { VCODE_OP_CONST, .value = 1000001 },
      { VCODE_OP_CMP,   .cmp = VCODE_CMP_EQ },
      { VCODE_OP_ASSERT },
      { VCODE_OP_WAIT,  .target = 4 }
   };

   CHECK_BB(3);

   const check_bb_t bb4[] = {
      { VCODE_OP_JUMP,  .target = 1 }
   };

   CHECK_BB(4);
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

   vcode_unit_t v0 = find_unit(tree_stmt(e, 0));
   vcode_select_unit(v0);

   fail_unless(vcode_count_vars() == 2);

   const check_bb_t bb0[] = {
      { VCODE_OP_CONST, .value = 64 },
      { VCODE_OP_STORE, .name = "X" },
      { VCODE_OP_CONST, .value = -4 },
      { VCODE_OP_STORE, .name = "Y" },
      { VCODE_OP_RETURN }
   };

   CHECK_BB(0);

   const check_bb_t bb1[] = {
      { VCODE_OP_CONST, .value = 4000000 },
      { VCODE_OP_WAIT,  .target = 2, .delay = true }
   };

   CHECK_BB(1);

   const check_bb_t bb2[] = {
      { VCODE_OP_CONST, .value = 2 },
      { VCODE_OP_LOAD,  .name = "X" },
      { VCODE_OP_CONST, .value = 64 },
      { VCODE_OP_CMP },
      { VCODE_OP_ASSERT },
      { VCODE_OP_LOAD,  .name = "Y" },
      { VCODE_OP_CONST, .value = -4 },
      { VCODE_OP_CMP },
      { VCODE_OP_ASSERT },
      { VCODE_OP_CONST, .value = 2 },
      { VCODE_OP_MUL },
      { VCODE_OP_BOUNDS, .low = INT32_MIN, .high = INT32_MAX },
      { VCODE_OP_CONST,  .value = -8 },
      { VCODE_OP_CMP },
      { VCODE_OP_ASSERT },
      { VCODE_OP_CONST, .value = 5 },
      { VCODE_OP_STORE, .name = "X" },
      { VCODE_OP_CONST, .value = 7 },
      { VCODE_OP_STORE, .name = "Y" },
      { VCODE_OP_CONST, .value = 1000000 },
      { VCODE_OP_WAIT,  .target = 3, .delay = true }
   };

   CHECK_BB(2);

   const check_bb_t bb3[] = {
      { VCODE_OP_CONST, .value = 2 },
      { VCODE_OP_LOAD, .name = "X" },
      { VCODE_OP_LOAD, .name = "Y" },
      { VCODE_OP_ADD },
      { VCODE_OP_CONST, .value = 12 },
      { VCODE_OP_CMP },
      { VCODE_OP_ASSERT },
      { VCODE_OP_WAIT, .target = 4 }
   };

   CHECK_BB(3);

   const check_bb_t bb4[] = {
      { VCODE_OP_JUMP, .target = 1 }
   };

   CHECK_BB(4);
}
END_TEST

START_TEST(test_assign2)
{
   input_from_file(TESTDIR "/lower/assign2.vhd");

   const error_t expect[] = {
      { -1, NULL }
   };
   expect_errors(expect);

   tree_t e = run_elab();
   lower_unit(e);

   vcode_unit_t v0 = find_unit(tree_stmt(e, 0));
   vcode_select_unit(v0);

   EXPECT_BB(0) = {
      { VCODE_OP_CONST, .value = 8 },
      { VCODE_OP_INDEX, .name = "X" },
      { VCODE_OP_CONST, .value = 1 },
      { VCODE_OP_CONST, .value = 0 },
      { VCODE_OP_CONST_ARRAY, .length = 8 },
      { VCODE_OP_COPY },
      { VCODE_OP_CONST, .value = 3 },
      { VCODE_OP_INDEX, .name = "Y" },
      { VCODE_OP_CONST, .value = 1 },
      { VCODE_OP_CONST_ARRAY, .length = 3 },
      { VCODE_OP_COPY },
      { VCODE_OP_RETURN }
   };

   CHECK_BB(0);

   fail_unless(vcode_get_op(6) == VCODE_OP_CONST_ARRAY);
   for (int i = 0; i < 8; i++)
      fail_unless(vcode_get_arg(6, i) == vcode_get_result((i == 6) ? 4 : 5));

   EXPECT_BB(1) = {
      { VCODE_OP_CONST, .value = 2 },
      { VCODE_OP_INDEX, .name = "X" },
      { VCODE_OP_ADDI, .value = 7 },
      { VCODE_OP_LOAD_INDIRECT },
      { VCODE_OP_CONST, .value = 0 },
      { VCODE_OP_CMP },
      { VCODE_OP_ASSERT },
      { VCODE_OP_ADDI, .value = 3 },
      { VCODE_OP_LOAD_INDIRECT },
      { VCODE_OP_LOAD_INDIRECT },
      { VCODE_OP_CMP },
      { VCODE_OP_ASSERT },
      { VCODE_OP_CONST, .value = 1 },
      { VCODE_OP_ADDI, .value = 5 },
      { VCODE_OP_STORE_INDIRECT },
      { VCODE_OP_INDEX, .name = "Y" },
      { VCODE_OP_ADDI, .value = 2 },
      { VCODE_OP_LOAD_INDIRECT },
      { VCODE_OP_STORE_INDIRECT },
      { VCODE_OP_WAIT, .target = 2 }
   };

   CHECK_BB(1);
}
END_TEST

START_TEST(test_signal1)
{
   input_from_file(TESTDIR "/lower/signal1.vhd");

   const error_t expect[] = {
      { -1, NULL }
   };
   expect_errors(expect);

   tree_t e = run_elab();
   lower_unit(e);

   vcode_unit_t vc = find_unit(e);
   vcode_select_unit(vc);

   {
      EXPECT_BB(0) = {
         { VCODE_OP_CONST, .value = 5 },
         { VCODE_OP_SET_INITIAL },
         { VCODE_OP_RESOLVED_ADDRESS, .name = ":signal1:x" },
         { VCODE_OP_RETURN }
      };

      CHECK_BB(0);
   }

   fail_unless(vcode_count_signals() == 1);
   fail_unless(icmp(vcode_signal_name(0), ":signal1:x"));
   fail_unless(vcode_signal_count_nets(0) == 1);
   fail_unless(vcode_signal_nets(0)[0] == 0);

   vcode_unit_t v0 = find_unit(tree_stmt(e, 0));
   vcode_select_unit(v0);

   {
      EXPECT_BB(0) = {
         { VCODE_OP_NETS, .name = ":signal1:x" },
         { VCODE_OP_CONST, .value = 1 },
         { VCODE_OP_ALLOC_DRIVER },
         { VCODE_OP_RETURN }
      };

      CHECK_BB(0);

      EXPECT_BB(1) = {
         { VCODE_OP_CONST, .value = 2 },
         { VCODE_OP_VAR_UPREF, .hops = 1, .name = "resolved_:signal1:x" },
         { VCODE_OP_LOAD_INDIRECT },
         { VCODE_OP_LOAD_INDIRECT },
         { VCODE_OP_CONST, .value = 5 },
         { VCODE_OP_CMP, .cmp = VCODE_CMP_EQ },
         { VCODE_OP_ASSERT },
         { VCODE_OP_CONST, .value = 0 },
         { VCODE_OP_NETS, .name = ":signal1:x" },
         { VCODE_OP_CONST, .value = 6 },
         { VCODE_OP_CONST, .value = 1 },
         { VCODE_OP_SCHED_WAVEFORM },
         { VCODE_OP_WAIT, .target = 2 }
      };

      CHECK_BB(1);
   }
}
END_TEST

START_TEST(test_cond1)
{
   input_from_file(TESTDIR "/lower/cond1.vhd");

   const error_t expect[] = {
      { -1, NULL }
   };
   expect_errors(expect);

   tree_t e = run_elab();
   lower_unit(e);

   vcode_unit_t v0 = find_unit(tree_stmt(e, 0));
   vcode_select_unit(v0);

   EXPECT_BB(0) = {
      { VCODE_OP_CONST, .value = -2147483648 },
      { VCODE_OP_STORE, .name = "Y" },
      { VCODE_OP_RETURN }
   };

   CHECK_BB(0);

   EXPECT_BB(1) = {
      { VCODE_OP_VAR_UPREF, .hops = 1, .name = "resolved_:cond1:x" },
      { VCODE_OP_LOAD_INDIRECT },
      { VCODE_OP_LOAD_INDIRECT },
      { VCODE_OP_LOAD, .name = "Y" },
      { VCODE_OP_CMP, .cmp = VCODE_CMP_EQ },
      { VCODE_OP_COND, .target = 2, .target_else = 3 }
   };

   CHECK_BB(1);

   EXPECT_BB(2) = {
      { VCODE_OP_CONST, .value = 2 },
      { VCODE_OP_STORE, .name = "Y" },
      { VCODE_OP_JUMP, .target = 3 }
   };

   CHECK_BB(2);

   EXPECT_BB(3) = {
      { VCODE_OP_VAR_UPREF, .hops = 1, .name = "resolved_:cond1:x" },
      { VCODE_OP_LOAD_INDIRECT },
      { VCODE_OP_LOAD_INDIRECT },
      { VCODE_OP_LOAD, .name = "Y" },
      { VCODE_OP_ADDI, .value = 1 },
      { VCODE_OP_CMP, .cmp = VCODE_CMP_EQ },
      { VCODE_OP_COND, .target = 4, .target_else = 5 }
   };

   CHECK_BB(3);

   EXPECT_BB(4) = {
      { VCODE_OP_CONST, .value = 1 },
      { VCODE_OP_STORE, .name = "Y" },
      { VCODE_OP_JUMP, .target = 6 }
   };

   CHECK_BB(4);

   EXPECT_BB(5) = {
      { VCODE_OP_CONST, .value = 3 },
      { VCODE_OP_STORE, .name = "Y" },
      { VCODE_OP_JUMP, .target = 6 }
   };

   CHECK_BB(5);

   EXPECT_BB(6) = {
      { VCODE_OP_CONST, .value = 0 },
      { VCODE_OP_CONST, .value = 100 },
      { VCODE_OP_CONST, .value = 111 },
      { VCODE_OP_CONST_ARRAY, .length = 3 },
      { VCODE_OP_CONST, .value = 3 },
      { VCODE_OP_REPORT },
      { VCODE_OP_WAIT, .target = 7 }
   };

   CHECK_BB(6);
}
END_TEST

START_TEST(test_arith1)
{
   input_from_file(TESTDIR "/lower/arith1.vhd");

   const error_t expect[] = {
      { -1, NULL }
   };
   expect_errors(expect);

   tree_t e = run_elab();
   lower_unit(e);

   vcode_unit_t v0 = find_unit(tree_stmt(e, 0));
   vcode_select_unit(v0);

   EXPECT_BB(0) = {
      { VCODE_OP_CONST, .value = -2147483648 },
      { VCODE_OP_STORE, .name = "X" },
      { VCODE_OP_STORE, .name = "Y" },
      { VCODE_OP_RETURN }
   };

   CHECK_BB(0);

   EXPECT_BB(1) = {
      { VCODE_OP_CONST, .value = 3 },
      { VCODE_OP_STORE, .name = "X" },
      { VCODE_OP_CONST, .value = 12 },
      { VCODE_OP_STORE, .name = "Y" },
      { VCODE_OP_CONST, .value = 1000000 },
      { VCODE_OP_WAIT, .target = 2 }
   };

   CHECK_BB(1);

   EXPECT_BB(2) = {
      { VCODE_OP_CONST, .value = 2 },
      { VCODE_OP_LOAD, .name = "X" },
      { VCODE_OP_LOAD, .name = "Y" },
      { VCODE_OP_ADD },
      { VCODE_OP_CONST, .value = 15 },
      { VCODE_OP_CMP, .cmp = VCODE_CMP_EQ },
      { VCODE_OP_ASSERT },
      { VCODE_OP_SUB },
      { VCODE_OP_CONST, .value = -9 },
      { VCODE_OP_CMP, .cmp = VCODE_CMP_EQ },
      { VCODE_OP_ASSERT },
      { VCODE_OP_MUL },
      { VCODE_OP_CONST, .value = 36 },
      { VCODE_OP_CMP, .cmp = VCODE_CMP_EQ },
      { VCODE_OP_ASSERT },
      { VCODE_OP_CONST, .value = 12 },
      { VCODE_OP_DIV },
      { VCODE_OP_CONST, .value = 0 },
      { VCODE_OP_CMP, .cmp = VCODE_CMP_EQ },
      { VCODE_OP_ASSERT },
      { VCODE_OP_CONST, .value = 3 },
      { VCODE_OP_CMP, .cmp = VCODE_CMP_EQ },
      { VCODE_OP_ASSERT },
      { VCODE_OP_CMP, .cmp = VCODE_CMP_EQ },
      { VCODE_OP_ASSERT },
      { VCODE_OP_CMP, .cmp = VCODE_CMP_NEQ },
      { VCODE_OP_ASSERT },
      { VCODE_OP_CMP, .cmp = VCODE_CMP_LT },
      { VCODE_OP_ASSERT },
      { VCODE_OP_CMP, .cmp = VCODE_CMP_GT },
      { VCODE_OP_ASSERT },
      { VCODE_OP_CMP, .cmp = VCODE_CMP_LEQ },
      { VCODE_OP_ASSERT },
      { VCODE_OP_CMP, .cmp = VCODE_CMP_GEQ },
      { VCODE_OP_ASSERT },
      { VCODE_OP_NEG },
      { VCODE_OP_CONST, .value = -3 },
      { VCODE_OP_CMP, .cmp = VCODE_CMP_EQ },
      { VCODE_OP_ASSERT },
      { VCODE_OP_EXP },
      { VCODE_OP_CONST, .value = 531441 },
      { VCODE_OP_CMP, .cmp = VCODE_CMP_EQ },
      { VCODE_OP_ASSERT },
      { VCODE_OP_CONST, .value = -34 },
      { VCODE_OP_STORE, .name = "X" },
      { VCODE_OP_ABS },
      { VCODE_OP_CMP, .cmp = VCODE_CMP_EQ },
      { VCODE_OP_ASSERT },
      { VCODE_OP_CONST, .value = 5 },
      { VCODE_OP_MOD },
      { VCODE_OP_CONST, .value = 2 },
      { VCODE_OP_CMP, .cmp = VCODE_CMP_EQ },
      { VCODE_OP_ASSERT },
      { VCODE_OP_REM },
      { VCODE_OP_CMP, .cmp = VCODE_CMP_EQ },
      { VCODE_OP_ASSERT },
      { VCODE_OP_CONST, .value = -5 },
      { VCODE_OP_REM },
      { VCODE_OP_CONST, .value = -2 },
      { VCODE_OP_CMP, .cmp = VCODE_CMP_EQ },
      { VCODE_OP_ASSERT },
      { VCODE_OP_MOD },
      { VCODE_OP_CMP, .cmp = VCODE_CMP_EQ },
      { VCODE_OP_ASSERT },
      { VCODE_OP_WAIT, .target = 3 }
   };

   CHECK_BB(2);
}
END_TEST

START_TEST(test_pack1)
{
   input_from_file(TESTDIR "/lower/pack1.vhd");

   const error_t expect[] = {
      { -1, NULL }
   };
   expect_errors(expect);

   tree_t t, body = NULL;
   while ((t = parse())) {
      sem_check(t);
      fail_if(sem_errors() > 0);

      simplify(t, 0);

      if (tree_kind(t) == T_PACK_BODY)
         body = t;
   }

   fail_if(body == NULL);
   lower_unit(body);

   tree_t add1 = tree_decl(body, 0);
   fail_unless(tree_kind(add1) == T_FUNC_BODY);

   vcode_unit_t v0 = find_unit(add1);
   vcode_select_unit(v0);

   EXPECT_BB(0) = {
      { VCODE_OP_ADDI, .value = 1 },
      { VCODE_OP_BOUNDS, .low = INT32_MIN, .high = INT32_MAX },
      { VCODE_OP_RETURN }
   };

   CHECK_BB(0);
}
END_TEST

START_TEST(test_func1)
{
   input_from_file(TESTDIR "/lower/func1.vhd");

   const error_t expect[] = {
      { -1, NULL }
   };
   expect_errors(expect);

   tree_t e = run_elab();
   lower_unit(e);

   vcode_unit_t v0 = find_unit(tree_stmt(e, 0));
   vcode_select_unit(v0);

   EXPECT_BB(0) = {
      { VCODE_OP_CONST, .value = INT32_MIN },
      { VCODE_OP_STORE, .name = "R" },
      { VCODE_OP_RETURN }
   };

   CHECK_BB(0);

   EXPECT_BB(1) = {
      { VCODE_OP_CONST, .value = 2 },
      { VCODE_OP_FCALL, .func = "*__ADD1(I)I", .args = 1 },
      { VCODE_OP_STORE, .name = "R" },
      { VCODE_OP_WAIT, .target = 2 }
   };

   CHECK_BB(1);
}
END_TEST

START_TEST(test_issue94)
{
   input_from_file(TESTDIR "/lower/issue94.vhd");

   const error_t expect[] = {
      { -1, NULL }
   };
   expect_errors(expect);

   tree_t e = run_elab();
   lower_unit(e);
}
END_TEST

START_TEST(test_arrayop1)
{
   input_from_file(TESTDIR "/lower/arrayop1.vhd");

   const error_t expect[] = {
      { -1, NULL }
   };
   expect_errors(expect);

   tree_t e = run_elab();
   lower_unit(e);

   vcode_unit_t v0 = find_unit(tree_stmt(e, 0));
   vcode_select_unit(v0);

   EXPECT_BB(0) = {
      { VCODE_OP_CONST, .value = 3 },
      { VCODE_OP_INDEX, .name = "X" },
      { VCODE_OP_CONST, .value = 0 },
      { VCODE_OP_CONST_ARRAY, .length = 3 },
      { VCODE_OP_COPY },
      { VCODE_OP_RETURN }
   };

   CHECK_BB(0);

   EXPECT_BB(1) = {
      { VCODE_OP_CONST, .value = 2 },
      { VCODE_OP_INDEX, .name = "X" },
      { VCODE_OP_CONST, .value = 0 },
      { VCODE_OP_CONST_ARRAY, .length = 3 },
      { VCODE_OP_CONST, .value = 3 },
      { VCODE_OP_ALLOCA },
      { VCODE_OP_CONST, .value = 0 },
      { VCODE_OP_STORE_INDIRECT },
      { VCODE_OP_ALLOCA },
      { VCODE_OP_STORE_INDIRECT },
      { VCODE_OP_JUMP, .target = 2 }
   };

   CHECK_BB(1);

   EXPECT_BB(2) = {
      { VCODE_OP_LOAD_INDIRECT },
      { VCODE_OP_CMP, .cmp = VCODE_CMP_GEQ },
      { VCODE_OP_STORE_INDIRECT },
      { VCODE_OP_COND, .target = 4, .target_else = 3 }
   };

   CHECK_BB(2);

   EXPECT_BB(3) = {
      { VCODE_OP_ADD },
      { VCODE_OP_ADD },
      { VCODE_OP_LOAD_INDIRECT },
      { VCODE_OP_LOAD_INDIRECT },
      { VCODE_OP_CMP, .cmp = VCODE_CMP_LT },
      { VCODE_OP_CMP, .cmp = VCODE_CMP_EQ },
      { VCODE_OP_ADDI, .value = 1 },
      { VCODE_OP_STORE_INDIRECT },
      { VCODE_OP_STORE_INDIRECT },
      { VCODE_OP_CMP, .cmp = VCODE_CMP_EQ },
      { VCODE_OP_NOT },
      { VCODE_OP_OR },
      { VCODE_OP_COND, .target = 4, .target_else = 2 }
   };

   CHECK_BB(3);

   EXPECT_BB(4) = {
      { VCODE_OP_LOAD_INDIRECT },
      { VCODE_OP_ASSERT },
      { VCODE_OP_WAIT, .target = 5 }
   };

   CHECK_BB(4);
}
END_TEST

START_TEST(test_array1)
{
   input_from_file(TESTDIR "/lower/array1.vhd");

   const error_t expect[] = {
      { -1, NULL }
   };
   expect_errors(expect);

   tree_t e = run_elab();
   lower_unit(e);

   vcode_unit_t v0 = find_unit(tree_stmt(e, 0));
   vcode_select_unit(v0);

   EXPECT_BB(1) = {
      { VCODE_OP_CONST, .value = 2 },
      { VCODE_OP_FCALL, .name = ":array1:func" },
      { VCODE_OP_CONST, .value = 1 },
      { VCODE_OP_CONST, .value = 0 },
      { VCODE_OP_CONST_ARRAY, .length = 2 },
      { VCODE_OP_UNWRAP },
      { VCODE_OP_UARRAY_LEN },
      { VCODE_OP_CONST, .value = 2 },
      { VCODE_OP_CMP, .cmp = VCODE_CMP_EQ },
      { VCODE_OP_ALLOCA },
      { VCODE_OP_STORE_INDIRECT },
      { VCODE_OP_COND, .target = 2, .target_else = 3 }
   };

   CHECK_BB(1);

   EXPECT_BB(2) = {
      { VCODE_OP_MEMCMP },
      { VCODE_OP_STORE_INDIRECT },
      { VCODE_OP_JUMP, .target = 3 }
   };

   CHECK_BB(2);

   EXPECT_BB(3) = {
      { VCODE_OP_LOAD_INDIRECT },
      { VCODE_OP_ASSERT },
      { VCODE_OP_WAIT, .target = 4 }
   };

   CHECK_BB(3);
}
END_TEST

START_TEST(test_nest1)
{
   input_from_file(TESTDIR "/lower/nest1.vhd");

   const error_t expect[] = {
      { -1, NULL }
   };
   expect_errors(expect);

   tree_t e = run_elab();
   lower_unit(e);

   tree_t p = tree_stmt(e, 0);

   {
      vcode_unit_t v0 = find_unit(p);
      vcode_select_unit(v0);

      EXPECT_BB(1) = {
         { VCODE_OP_CONST, .value = 2 },
         { VCODE_OP_CONST, .value = 5 },
         { VCODE_OP_NESTED_FCALL, .func = ":nest1:line_7_LINE_7.ADD_TO_X(I)I",
           .args = 1 },
         { VCODE_OP_CONST, .value = 7 },
         { VCODE_OP_CMP, .cmp = VCODE_CMP_EQ },
         { VCODE_OP_ASSERT },
         { VCODE_OP_WAIT, .target = 2 }
      };

      CHECK_BB(1);
   }

   tree_t f1 = tree_decl(p, 2);
   fail_unless(tree_kind(f1) == T_FUNC_BODY);

   {
      vcode_unit_t v0 = find_unit(f1);
      vcode_select_unit(v0);

      fail_unless(icmp(vcode_unit_name(), ":nest1:line_7_LINE_7.ADD_TO_X(I)I"));

      EXPECT_BB(0) = {
         { VCODE_OP_NESTED_FCALL,
           .func = ":nest1:line_7_LINE_7.ADD_TO_X(I)I__"
           ":nest1:line_7_LINE_7.ADD_TO_X_DO_IT()I" },
         { VCODE_OP_RETURN }
      };

      CHECK_BB(0);
   }

   tree_t f2 = tree_decl(f1, 0);
   fail_unless(tree_kind(f2) == T_FUNC_BODY);

   {
      vcode_unit_t v0 = find_unit(f2);
      vcode_select_unit(v0);

      fail_unless(icmp(vcode_unit_name(), ":nest1:line_7_LINE_7.ADD_TO_X(I)I__"
                       ":nest1:line_7_LINE_7.ADD_TO_X_DO_IT()I"));

      EXPECT_BB(0) = {
         { VCODE_OP_VAR_UPREF, .hops = 2, .name = "LINE_7.X" },
         { VCODE_OP_LOAD_INDIRECT },
         { VCODE_OP_PARAM_UPREF, .hops = 1 },
         { VCODE_OP_ADD },
         { VCODE_OP_BOUNDS, .low = INT32_MIN, .high = INT32_MAX },
         { VCODE_OP_RETURN }
      };

      CHECK_BB(0);
   }

}
END_TEST

START_TEST(test_signal2)
{
   input_from_file(TESTDIR "/lower/signal2.vhd");

   const error_t expect[] = {
      { -1, NULL }
   };
   expect_errors(expect);

   tree_t e = run_elab();
   lower_unit(e);

   vcode_unit_t v0 = find_unit(tree_stmt(e, 0));
   vcode_select_unit(v0);

   EXPECT_BB(0) = {
      { VCODE_OP_RETURN }
   };

   CHECK_BB(0);

   EXPECT_BB(1) = {
      { VCODE_OP_CONST, .value = 2 },
      { VCODE_OP_NETS, .name = ":signal2:x" },
      { VCODE_OP_CONST, .value = 1 },
      { VCODE_OP_EVENT },
      { VCODE_OP_ASSERT },
      { VCODE_OP_ACTIVE },
      { VCODE_OP_ASSERT },
      { VCODE_OP_WAIT, .target = 2 }
   };

   CHECK_BB(1);
}
END_TEST

START_TEST(test_attr1)
{
   input_from_file(TESTDIR "/lower/attr1.vhd");

   const error_t expect[] = {
      { -1, NULL }
   };
   expect_errors(expect);

   tree_t e = run_elab();
   lower_unit(e);

   vcode_unit_t v0 = find_unit(tree_stmt(e, 0));
   vcode_select_unit(v0);

   EXPECT_BB(1) = {
      { VCODE_OP_CONST, .value = 2 },
      { VCODE_OP_LOAD, .name = "X" },
      { VCODE_OP_ADDI, .value = 1 },
      { VCODE_OP_CONST, .value = 1 },
      { VCODE_OP_CMP, .cmp = VCODE_CMP_EQ },
      { VCODE_OP_ASSERT },
      { VCODE_OP_ADDI, .value = -1 },
      { VCODE_OP_CONST, .value = -1 },
      { VCODE_OP_CMP, .cmp = VCODE_CMP_EQ },
      { VCODE_OP_ASSERT },
      { VCODE_OP_LOAD, .name = "Z" },
      { VCODE_OP_ADDI, .value = -1 },
      { VCODE_OP_CONST, .value = 0 },
      { VCODE_OP_CMP },
      { VCODE_OP_ASSERT },
      { VCODE_OP_ADDI, .value = 1 },
      { VCODE_OP_CONST, .value = 2 },
      { VCODE_OP_CMP, .cmp = VCODE_CMP_EQ },
      { VCODE_OP_ASSERT },
      { VCODE_OP_LOAD, .name = "Y" },
      { VCODE_OP_ADDI, .value = 1 },
      { VCODE_OP_CONST, .value = 2 },
      { VCODE_OP_CMP, .cmp = VCODE_CMP_EQ },
      { VCODE_OP_ASSERT },
      { VCODE_OP_ADDI, .value = -1 },
      { VCODE_OP_CONST, .value = 0 },
      { VCODE_OP_CMP, .cmp = VCODE_CMP_EQ },
      { VCODE_OP_ASSERT },
      { VCODE_OP_WAIT, .target = 2 }
   };

   CHECK_BB(1);
}
END_TEST

START_TEST(test_assign3)
{
   input_from_file(TESTDIR "/lower/assign3.vhd");

   const error_t expect[] = {
      { -1, NULL }
   };
   expect_errors(expect);

   tree_t e = run_elab();
   lower_unit(e);

   vcode_unit_t v0 = find_unit(tree_stmt(e, 0));
   vcode_select_unit(v0);

   EXPECT_BB(1) = {
      { VCODE_OP_INDEX, .name = "X" },
      { VCODE_OP_CONST, .value = 8 },
      { VCODE_OP_INDEX, .name = "Y" },
      { VCODE_OP_COPY },
      { VCODE_OP_CONST, .value = 2 },
      { VCODE_OP_MEMCMP },
      { VCODE_OP_NOT },
      { VCODE_OP_ASSERT },
      { VCODE_OP_WAIT, .target = 2 },
   };

   CHECK_BB(1);
}
END_TEST

START_TEST(test_record1)
{
   input_from_file(TESTDIR "/lower/record1.vhd");

   const error_t expect[] = {
      { -1, NULL }
   };
   expect_errors(expect);

   tree_t e = run_elab();
   lower_unit(e);

   vcode_unit_t v0 = find_unit(tree_stmt(e, 0));
   vcode_select_unit(v0);

   EXPECT_BB(0) = {
      { VCODE_OP_INDEX, .name = "A" },
      { VCODE_OP_CONST, .value = 1 },
      { VCODE_OP_CONST, .value = 2 },
      { VCODE_OP_CONST_RECORD },
      { VCODE_OP_STORE_INDIRECT },
      { VCODE_OP_INDEX, .name = "B" },
      { VCODE_OP_STORE_INDIRECT },
      { VCODE_OP_RETURN }
   };

   CHECK_BB(0);

   EXPECT_BB(1) = {
      { VCODE_OP_CONST, .value = 2 },
      { VCODE_OP_INDEX, .name = "A" },
      { VCODE_OP_RECORD_REF, .field = 0 },
      { VCODE_OP_LOAD_INDIRECT },
      { VCODE_OP_CONST, .value = 1 },
      { VCODE_OP_CMP, .cmp = VCODE_CMP_EQ },
      { VCODE_OP_ASSERT },
      { VCODE_OP_CONST, .value = 5 },
      { VCODE_OP_STORE_INDIRECT },
      { VCODE_OP_INDEX, .name = "B" },
      { VCODE_OP_COPY },
      { VCODE_OP_LOAD_INDIRECT },
      { VCODE_OP_CMP, .cmp = VCODE_CMP_EQ },
      { VCODE_OP_ASSERT },
      { VCODE_OP_RECORD_REF, .field = 0 },
      { VCODE_OP_LOAD_INDIRECT },
      { VCODE_OP_LOAD_INDIRECT },
      { VCODE_OP_CMP, .cmp = VCODE_CMP_EQ },
      { VCODE_OP_RECORD_REF, .field = 1 },
      { VCODE_OP_RECORD_REF, .field = 1 },
      { VCODE_OP_LOAD_INDIRECT },
      { VCODE_OP_LOAD_INDIRECT },
      { VCODE_OP_CMP, .cmp = VCODE_CMP_EQ },
      { VCODE_OP_AND },
      { VCODE_OP_ASSERT },
      { VCODE_OP_WAIT, .target = 2 }
   };

   CHECK_BB(1);
}
END_TEST

START_TEST(test_signal4)
{
   input_from_file(TESTDIR "/lower/signal4.vhd");

   const error_t expect[] = {
      { -1, NULL }
   };
   expect_errors(expect);

   tree_t e = run_elab();
   lower_unit(e);

   vcode_unit_t v0 = find_unit(tree_stmt(e, 0));
   vcode_select_unit(v0);

   EXPECT_BB(1) = {
      { VCODE_OP_VAR_UPREF, .hops = 1, .name = "resolved_:signal4:s" },
      { VCODE_OP_LOAD_INDIRECT },
      { VCODE_OP_LOAD_INDIRECT },
      { VCODE_OP_INDEX, .name = "V" },
      { VCODE_OP_ADDI, .value = 1 },
      { VCODE_OP_STORE_INDIRECT },
      { VCODE_OP_CONST, .value = 0 },
      { VCODE_OP_NETS, .name = ":signal4:s" },
      { VCODE_OP_CONST, .value = 4 },
      { VCODE_OP_SCHED_WAVEFORM },
      { VCODE_OP_LOAD_INDIRECT },
      { VCODE_OP_COPY },
      { VCODE_OP_WAIT, .target = 2 }
   };

   CHECK_BB(1);
}
END_TEST

START_TEST(test_staticwait)
{
   input_from_file(TESTDIR "/lower/staticwait.vhd");

   const error_t expect[] = {
      { -1, NULL }
   };
   expect_errors(expect);

   tree_t e = run_elab();
   lower_unit(e);

   vcode_unit_t v0 = find_unit(tree_stmt(e, 0));
   vcode_select_unit(v0);

   EXPECT_BB(0) = {
      { VCODE_OP_NETS, .name = ":staticwait:x" },
      { VCODE_OP_CONST, .value = 1 },
      { VCODE_OP_ALLOC_DRIVER },
      { VCODE_OP_SCHED_EVENT, .subkind = 2 },
      { VCODE_OP_RETURN }
   };

   CHECK_BB(0);

   EXPECT_BB(1) = {
      { VCODE_OP_CONST, .value = 0 },
      { VCODE_OP_NETS, .name = ":staticwait:x" },
      { VCODE_OP_CONST, .value = 0 },
      { VCODE_OP_CONST, .value = 1 },
      { VCODE_OP_SCHED_WAVEFORM },
      { VCODE_OP_WAIT, .target = 2 }
   };

   CHECK_BB(1);
}
END_TEST

START_TEST(test_proc1)
{
   input_from_file(TESTDIR "/lower/proc1.vhd");

   const error_t expect[] = {
      { -1, NULL }
   };
   expect_errors(expect);

   tree_t e = run_elab();
   lower_unit(e);

   {
      vcode_unit_t v0 = find_unit(tree_stmt(e, 0));
      vcode_select_unit(v0);

      EXPECT_BB(1) = {
         { VCODE_OP_LOAD, .name = "A" },
         { VCODE_OP_INDEX, .name = "B" },
         { VCODE_OP_FCALL, .func = ":proc1:add1(II)", .args = 2 },
         { VCODE_OP_CONST, .value = 2 },
         { VCODE_OP_LOAD, .name = "B" },
         { VCODE_OP_CONST, .value = 3 },
         { VCODE_OP_CMP, .cmp = VCODE_CMP_EQ },
         { VCODE_OP_ASSERT },
         { VCODE_OP_CONST, .value = 5 },
         { VCODE_OP_FCALL, .func = ":proc1:add1(II)", .args = 2 },
         { VCODE_OP_LOAD, .name = "B" },
         { VCODE_OP_CONST, .value = 6 },
         { VCODE_OP_CMP, .cmp = VCODE_CMP_EQ },
         { VCODE_OP_ASSERT },
         { VCODE_OP_WAIT, .target = 2 }
      };

      CHECK_BB(1);
   }

   {
      vcode_unit_t v0 = find_unit(tree_decl(e, 1));
      vcode_select_unit(v0);

      EXPECT_BB(0) = {
         { VCODE_OP_ADDI, .value = 1 },
         { VCODE_OP_BOUNDS, .low = INT32_MIN, .high = INT32_MAX },
         { VCODE_OP_STORE_INDIRECT },
         { VCODE_OP_RETURN }
      };

      CHECK_BB(0);
   }
}
END_TEST

START_TEST(test_while1)
{
   input_from_file(TESTDIR "/lower/while1.vhd");

   const error_t expect[] = {
      { -1, NULL }
   };
   expect_errors(expect);

   tree_t e = run_elab();
   lower_unit(e);

   vcode_unit_t v0 = find_unit(tree_stmt(e, 0));
   vcode_select_unit(v0);

   EXPECT_BB(2) = {
      { VCODE_OP_LOAD, .name = "N" },
      { VCODE_OP_CONST, .value = 0 },
      { VCODE_OP_CMP, .cmp = VCODE_CMP_GT },
      { VCODE_OP_COND, .target = 3, .target_else = 4 }
   };

   CHECK_BB(2);

   EXPECT_BB(3) = {
      { VCODE_OP_LOAD, .name = "N" },
      { VCODE_OP_ADDI, .value = -1 },
      { VCODE_OP_BOUNDS, .low = INT32_MIN, .high = INT32_MAX },
      { VCODE_OP_STORE, .name = "N" },
      { VCODE_OP_JUMP, .target = 2 }
   };

   CHECK_BB(3);
}
END_TEST

START_TEST(test_loop1)
{
   input_from_file(TESTDIR "/lower/loop1.vhd");

   const error_t expect[] = {
      { -1, NULL }
   };
   expect_errors(expect);

   tree_t e = run_elab();
   lower_unit(e);

   vcode_unit_t v0 = find_unit(tree_stmt(e, 0));
   vcode_select_unit(v0);

   EXPECT_BB(3) = {
      { VCODE_OP_LOAD, .name = "A" },
      { VCODE_OP_CONST, .value = 10 },
      { VCODE_OP_CMP, .cmp = VCODE_CMP_EQ },
      { VCODE_OP_COND, .target = 6, .target_else = 5 }
   };

   CHECK_BB(3);

   EXPECT_BB(8) = {
      { VCODE_OP_LOAD, .name = "A" },
      { VCODE_OP_ADDI, .value = 1 },
      { VCODE_OP_BOUNDS, .low = INT32_MIN, .high = INT32_MAX },
      { VCODE_OP_STORE, .name = "A" },
      { VCODE_OP_CONST, .value = 2 },
      { VCODE_OP_MOD },
      { VCODE_OP_CONST, .value = 0 },
      { VCODE_OP_CMP, .cmp = VCODE_CMP_EQ },
      { VCODE_OP_COND, .target = 11, .target_else = 10 }
   };

   CHECK_BB(8);
}
END_TEST

START_TEST(test_proc3)
{
   input_from_file(TESTDIR "/lower/proc3.vhd");

   const error_t expect[] = {
      { -1, NULL }
   };
   expect_errors(expect);

   tree_t e = run_elab();
   lower_unit(e);

   {
      vcode_unit_t v0 = find_unit(tree_decl(e, 1));
      vcode_select_unit(v0);

      EXPECT_BB(0) = {
         { VCODE_OP_CONST, .value = 10000000 },
         { VCODE_OP_WAIT, .delay = true, .target = 1 }
      };

      CHECK_BB(0);

      EXPECT_BB(1) = {
         { VCODE_OP_CONST, .value = 1 },
         { VCODE_OP_STORE_INDIRECT },
         { VCODE_OP_CONST, .value = 5000000 },
         { VCODE_OP_WAIT, .delay = true, .target = 2 }
      };

      CHECK_BB(1);
   }

   {
      vcode_unit_t v0 = find_unit(tree_stmt(e, 0));
      vcode_select_unit(v0);

      EXPECT_BB(1) = {
         { VCODE_OP_INDEX, .name = "X" },
         { VCODE_OP_PCALL, .func = ":proc3:p1(I)", .target = 2, .args = 1 }
      };

      CHECK_BB(1);

      EXPECT_BB(2) = {
         { VCODE_OP_RESUME, .func = ":proc3:p1(I)" },
         { VCODE_OP_WAIT, .target = 3 }
      };

      CHECK_BB(2);
   }
}
END_TEST

START_TEST(test_loop2)
{
   input_from_file(TESTDIR "/lower/loop2.vhd");

   const error_t expect[] = {
      { -1, NULL }
   };
   expect_errors(expect);

   tree_t e = run_elab();
   lower_unit(e);

   vcode_unit_t v0 = find_unit(tree_decl(e, 1));
   vcode_select_unit(v0);

   EXPECT_BB(2) = {
      { VCODE_OP_CONST, .value = 1000 },
      { VCODE_OP_CMP, .cmp = VCODE_CMP_GEQ },
      { VCODE_OP_COND, .target = 4, .target_else = 5 }
   };

   CHECK_BB(2);

   EXPECT_BB(3) = {
      { VCODE_OP_RETURN },
   };

   CHECK_BB(3);
}
END_TEST

START_TEST(test_slice1)
{
   input_from_file(TESTDIR "/lower/slice1.vhd");

   const error_t expect[] = {
      { -1, NULL }
   };
   expect_errors(expect);

   tree_t e = run_elab();
   lower_unit(e);

   vcode_unit_t v0 = find_unit(tree_stmt(e, 0));
   vcode_select_unit(v0);

   EXPECT_BB(1) = {
      { VCODE_OP_INDEX, .name = "V" },
      { VCODE_OP_CONST, .value = 4 },
      { VCODE_OP_CONST, .value = 1 },
      { VCODE_OP_CONST, .value = 2 },
      { VCODE_OP_CONST, .value = 3 },
      { VCODE_OP_CONST, .value = 4 },
      { VCODE_OP_CONST_ARRAY, .length = 4 },
      { VCODE_OP_COPY },
      { VCODE_OP_ADDI, .value = 1 },
      { VCODE_OP_CONST, .value = 2 },
      { VCODE_OP_CONST, .value = 6 },
      { VCODE_OP_CONST, .value = 7 },
      { VCODE_OP_CONST_ARRAY, .length = 2 },
      { VCODE_OP_COPY },
      { VCODE_OP_CONST, .value = 2 },
      { VCODE_OP_ADDI, .value = 2 },
      { VCODE_OP_CONST_ARRAY, .length = 2 },
      { VCODE_OP_MEMCMP },
      { VCODE_OP_ASSERT },
      { VCODE_OP_CONST, .value = 1000000 },
      { VCODE_OP_WAIT, .target = 2 },
   };

   CHECK_BB(1);
}
END_TEST

START_TEST(test_funcif)
{
   input_from_file(TESTDIR "/lower/funcif.vhd");

   const error_t expect[] = {
      { -1, NULL }
   };
   expect_errors(expect);

   tree_t e = run_elab();
   lower_unit(e);

   vcode_unit_t v0 = find_unit(tree_decl(e, 1));
   vcode_select_unit(v0);

   fail_unless(vcode_count_blocks() == 3);
}
END_TEST

START_TEST(test_memset)
{
   input_from_file(TESTDIR "/lower/memset.vhd");

   const error_t expect[] = {
      { -1, NULL }
   };
   expect_errors(expect);

   tree_t e = run_elab();
   lower_unit(e);

   {
      vcode_unit_t v0 = find_unit(tree_decl(e, 1));
      vcode_select_unit(v0);

      EXPECT_BB(0) = {
         { VCODE_OP_CONST, .value = 1 },
         { VCODE_OP_ADDI, .value = -1 },
         { VCODE_OP_ADDI, .value = 1 },
         { VCODE_OP_CAST },
         { VCODE_OP_CONST, .value = 0 },
         { VCODE_OP_CMP, .cmp = VCODE_CMP_LT },
         { VCODE_OP_SELECT },
         { VCODE_OP_ALLOCA, .subkind = VCODE_ALLOCA_HEAP },
         { VCODE_OP_CONST, .value = 0 },
         { VCODE_OP_WRAP },
         { VCODE_OP_STORE, .name = "V" },
         { VCODE_OP_MEMSET },
         { VCODE_OP_RETURN }
      };

      CHECK_BB(0);

      fail_if(vcode_unit_pure());
   }

   {
      vcode_unit_t v0 = find_unit(tree_decl(e, 2));
      vcode_select_unit(v0);

      EXPECT_BB(0) = {
         { VCODE_OP_CONST, .value = 1 },
         { VCODE_OP_ADDI, .value = -1 },
         { VCODE_OP_ADDI, .value = 1 },
         { VCODE_OP_CAST },
         { VCODE_OP_CONST, .value = 0 },
         { VCODE_OP_CMP, .cmp = VCODE_CMP_LT },
         { VCODE_OP_SELECT },
         { VCODE_OP_ALLOCA, .subkind = VCODE_ALLOCA_HEAP },
         { VCODE_OP_CONST, .value = 0 },
         { VCODE_OP_WRAP },
         { VCODE_OP_STORE, .name = "V" },
         { VCODE_OP_CONST, .value = 0xab },
         { VCODE_OP_CONST, .value = 4 },
         { VCODE_OP_MUL },
         { VCODE_OP_MEMSET },
         { VCODE_OP_RETURN }
      };

      CHECK_BB(0);

      fail_if(vcode_unit_pure());
   }
}
END_TEST

START_TEST(test_func5)
{
   input_from_file(TESTDIR "/lower/func5.vhd");

   const error_t expect[] = {
      { -1, NULL }
   };
   expect_errors(expect);

   tree_t e = run_elab();
   lower_unit(e);

   {
      vcode_unit_t v0 = find_unit(tree_decl(e, 1));
      vcode_select_unit(v0);

      EXPECT_BB(0) = {
         { VCODE_OP_VEC_LOAD },
         { VCODE_OP_LOAD_INDIRECT },
         { VCODE_OP_ADDI, .value = 1 },
         { VCODE_OP_BOUNDS, .low = INT32_MIN, .high = INT32_MAX },
         { VCODE_OP_RETURN }
      };

      CHECK_BB(0);
   }

   {
      vcode_unit_t v0 = find_unit(tree_decl(e, 2));
      vcode_select_unit(v0);

      EXPECT_BB(0) = {
         { VCODE_OP_CONST, .value = 1 },
         { VCODE_OP_EVENT },
         { VCODE_OP_RETURN }
      };

      CHECK_BB(0);
   }

   {
      vcode_unit_t v0 = find_unit(tree_stmt(e, 0));
      vcode_select_unit(v0);

      EXPECT_BB(1) = {
         { VCODE_OP_CONST, .value = 2 },
         { VCODE_OP_NETS, .name = ":func5:x" },
         { VCODE_OP_FCALL, .func = "*ADD_ONE_S(sI)I", .args = 1 },
         { VCODE_OP_CONST, .value = 6 },
         { VCODE_OP_CMP, .cmp = VCODE_CMP_EQ },
         { VCODE_OP_ASSERT },
         { VCODE_OP_FCALL, .func = "*EVENT(sI)B", .args = 1 },
         { VCODE_OP_ASSERT },
         { VCODE_OP_WAIT, .target = 2 }
      };

      CHECK_BB(1);
   }
}
END_TEST

START_TEST(test_bounds1)
{
   input_from_file(TESTDIR "/lower/bounds1.vhd");

   const error_t expect[] = {
      { -1, NULL }
   };
   expect_errors(expect);

   tree_t e = run_elab();
   lower_unit(e);

   vcode_unit_t v0 = find_unit(tree_stmt(e, 0));
   vcode_select_unit(v0);

   EXPECT_BB(1) = {
      { VCODE_OP_CONST, .value = 2 },
      { VCODE_OP_INDEX, .name = "V" },
      { VCODE_OP_LOAD,  .name = "K" },
      { VCODE_OP_CAST },
      { VCODE_OP_ADD },
      { VCODE_OP_LOAD_INDIRECT },
      { VCODE_OP_CONST, .value = 1 },
      { VCODE_OP_CMP, .cmp = VCODE_CMP_EQ },
      { VCODE_OP_ASSERT },
      { VCODE_OP_ADDI, .value = 1 },
      { VCODE_OP_BOUNDS, .low = 0, .high = 9 },
      { VCODE_OP_CAST },
      { VCODE_OP_ADD },
      { VCODE_OP_LOAD_INDIRECT },
      { VCODE_OP_CMP, .cmp = VCODE_CMP_EQ },
      { VCODE_OP_ASSERT },
      { VCODE_OP_WAIT, .target = 2 }
   };

   CHECK_BB(1);
}
END_TEST

START_TEST(test_record6)
{
   input_from_file(TESTDIR "/lower/record6.vhd");

   const error_t expect[] = {
      { -1, NULL }
   };
   expect_errors(expect);

   tree_t e = run_elab();
   lower_unit(e);

   vcode_unit_t v0 = find_unit(tree_decl(e, 1));
   vcode_select_unit(v0);

   fail_unless(vcode_var_use_heap(0));

   EXPECT_BB(0) = {
      { VCODE_OP_INDEX, .name = "R" },
      { VCODE_OP_CONST, .value = 0 },
      { VCODE_OP_CONST_ARRAY, .length = 3 },
      { VCODE_OP_CONST, .value = INT32_MIN },
      { VCODE_OP_CONST_RECORD },
      { VCODE_OP_STORE_INDIRECT },
      { VCODE_OP_RECORD_REF, .field = 0 },
      { VCODE_OP_CONST, .value = 3 },
      { VCODE_OP_COPY },
      { VCODE_OP_RECORD_REF, .field = 1 },
      { VCODE_OP_STORE_INDIRECT },
      { VCODE_OP_RETURN }
   };

   CHECK_BB(0);
}
END_TEST

START_TEST(test_proc7)
{
   input_from_file(TESTDIR "/lower/proc7.vhd");

   const error_t expect[] = {
      { -1, NULL }
   };
   expect_errors(expect);

   tree_t e = run_elab();
   lower_unit(e);

   vcode_unit_t v0 = find_unit(tree_decl(e, 1));
   vcode_select_unit(v0);

   EXPECT_BB(0) = {
      { VCODE_OP_UARRAY_LEFT },
      { VCODE_OP_CAST },
      { VCODE_OP_UARRAY_RIGHT },
      { VCODE_OP_CAST },
      { VCODE_OP_UARRAY_DIR },
      { VCODE_OP_SUB },
      { VCODE_OP_SUB },
      { VCODE_OP_SELECT },
      { VCODE_OP_ADDI, .value = 1 },
      { VCODE_OP_CAST },
      { VCODE_OP_CONST, .value = 0 },
      { VCODE_OP_CMP, .cmp = VCODE_CMP_LT },
      { VCODE_OP_SELECT },
      { VCODE_OP_ALLOCA, .subkind = 1 },
      { VCODE_OP_WRAP },
      { VCODE_OP_STORE, .name = "Y" },
      { VCODE_OP_CONST, .value = 0 },
      { VCODE_OP_MEMSET },
      { VCODE_OP_SELECT },
      { VCODE_OP_SELECT },
      { VCODE_OP_INDEX_CHECK, .subkind = BOUNDS_INDEX_TO },
      { VCODE_OP_CONST, .value = 1000000 },
      { VCODE_OP_WAIT, .target = 1 }
   };

   CHECK_BB(0);

    EXPECT_BB(1) = {
       { VCODE_OP_RETURN }
   };

   CHECK_BB(1);
}
END_TEST

START_TEST(test_issue116)
{
   input_from_file(TESTDIR "/lower/issue116.vhd");

   const error_t expect[] = {
      { -1, NULL }
   };
   expect_errors(expect);

   tree_t e = run_elab();
   lower_unit(e);

   vcode_unit_t v0 = find_unit(tree_stmt(e, 0));
   vcode_select_unit(v0);

   EXPECT_BB(0) = {
      { VCODE_OP_NETS, .name = ":issue116:intstat" },
      { VCODE_OP_CONST, .value = 1 },
      { VCODE_OP_CONST, .value = 8 },
      { VCODE_OP_ALLOC_DRIVER },
      { VCODE_OP_SCHED_EVENT, .subkind = 3 },
      { VCODE_OP_RETURN }
   };

   CHECK_BB(0);
}
END_TEST

START_TEST(test_mulphys)
{
   input_from_file(TESTDIR "/lower/mulphys.vhd");

   const error_t expect[] = {
      { -1, NULL }
   };
   expect_errors(expect);

   tree_t e = run_elab();
   lower_unit(e);

   vcode_unit_t v0 = find_unit(tree_decl(e, 1));
   vcode_select_unit(v0);

   EXPECT_BB(0) = {
      { VCODE_OP_CONST, .value = 1000000 },
      { VCODE_OP_CAST },
      { VCODE_OP_MUL },
      { VCODE_OP_RETURN }
   };

   CHECK_BB(0);
}
END_TEST

START_TEST(test_cover)
{
   input_from_file(TESTDIR "/lower/cover.vhd");

   const error_t expect[] = {
      { -1, NULL }
   };
   expect_errors(expect);

   opt_set_int("cover", 1);

   tree_t e = run_elab();
   lower_unit(e);

   vcode_unit_t v0 = find_unit(tree_stmt(e, 0));
   vcode_select_unit(v0);

   EXPECT_BB(1) = {
      { VCODE_OP_COVER_STMT, .tag = 0 },
      { VCODE_OP_CONST, .value = 1 },
      { VCODE_OP_STORE, .name = "V" },
      { VCODE_OP_COVER_STMT, .tag = 2 },
      { VCODE_OP_VAR_UPREF, .hops = 1, .name = "resolved_:cover:s" },
      { VCODE_OP_LOAD_INDIRECT },
      { VCODE_OP_LOAD_INDIRECT },
      { VCODE_OP_CMP, .cmp = VCODE_CMP_EQ },
      { VCODE_OP_COVER_COND, .tag = 0, .subkind = 1 },
      { VCODE_OP_LOAD_INDIRECT },
      { VCODE_OP_LOAD_INDIRECT },
      { VCODE_OP_CONST, .value = 10 },
      { VCODE_OP_CMP, .cmp = VCODE_CMP_GT },
      { VCODE_OP_COVER_COND, .tag = 0, .subkind = 2 },
      { VCODE_OP_OR },
      { VCODE_OP_COVER_COND, .tag = 0, .subkind = 0 },
      { VCODE_OP_COND, .target = 2, .target_else = 3 }
   };

   CHECK_BB(1);

   EXPECT_BB(2) = {
      { VCODE_OP_COVER_STMT, .tag = 1 },
      { VCODE_OP_CONST, .value = 2 },
      { VCODE_OP_STORE, .name = "V" },
      { VCODE_OP_JUMP, .target = 3 }
   };

   CHECK_BB(2);
}
END_TEST

START_TEST(test_issue122)
{
   input_from_file(TESTDIR "/lower/issue122.vhd");

   const error_t expect[] = {
      { -1, NULL }
   };
   expect_errors(expect);

   tree_t e = run_elab();
   lower_unit(e);

   vcode_unit_t v0 = find_unit(tree_decl(e, 1));
   vcode_select_unit(v0);

   EXPECT_BB(0) = {
      { VCODE_OP_NESTED_FCALL, .func = ":issue122:func(I)I__NESTED()I" },
      { VCODE_OP_STORE, .name = "V" },
      { VCODE_OP_RETURN }
   };

   CHECK_BB(0);
}
END_TEST

START_TEST(test_issue124)
{
   input_from_file(TESTDIR "/lower/issue124.vhd");

   const error_t expect[] = {
      { -1, NULL }
   };
   expect_errors(expect);

   tree_t e = run_elab();
   lower_unit(e);

   vcode_unit_t v0 = find_unit(tree_decl(e, 1));
   vcode_select_unit(v0);

   EXPECT_BB(0) = {
      { VCODE_OP_FCALL, .func = "WORK.PACK.TO_INTEGER(18WORK.PACK.UNSIGNED)I",
        .args = 1},
      { VCODE_OP_IMAGE },
      { VCODE_OP_RETURN }
   };

   CHECK_BB(0);
}
END_TEST

START_TEST(test_issue135)
{
   input_from_file(TESTDIR "/lower/issue135.vhd");

   tree_t e = run_elab();
   lower_unit(e);

   vcode_unit_t v0 = find_unit(tree_decl(e, 1));
   vcode_select_unit(v0);

   EXPECT_BB(0) = {
      { VCODE_OP_IMAGE },
      { VCODE_OP_IMAGE_MAP, .name = "STD.STANDARD.TIME" },
      { VCODE_OP_IMAGE },
      { VCODE_OP_UARRAY_LEN },
      { VCODE_OP_UARRAY_LEN },
      { VCODE_OP_ADD },
      { VCODE_OP_CONST, .value = 1 },
      { VCODE_OP_ADDI, .value = 1 },
      { VCODE_OP_UARRAY_LEN },
      { VCODE_OP_ADD },
      { VCODE_OP_ADDI, .value = 1 },
      { VCODE_OP_ALLOCA, .subkind = VCODE_ALLOCA_HEAP },
      { VCODE_OP_CONST, .value = 0 },
      { VCODE_OP_WRAP },
      { VCODE_OP_UNWRAP },
      { VCODE_OP_COPY },
      { VCODE_OP_ADD },
      { VCODE_OP_UNWRAP },
      { VCODE_OP_COPY },
      { VCODE_OP_ADD },
      { VCODE_OP_STORE_INDIRECT },
      { VCODE_OP_ADDI, .value = 1 },
      { VCODE_OP_UNWRAP },
      { VCODE_OP_COPY },
      { VCODE_OP_ADD },
      { VCODE_OP_STORE_INDIRECT },
      { VCODE_OP_RETURN }
   };

   CHECK_BB(0);
}
END_TEST

START_TEST(test_issue134)
{
   input_from_file(TESTDIR "/lower/issue134.vhd");

   const error_t expect[] = {
      {  8, "statement is unreachable" },
      {  8, "statement is unreachable" },
      { -1, NULL }
   };
   expect_errors(expect);

   tree_t e = run_elab();
   lower_unit(e);

   vcode_unit_t v0 = find_unit(tree_decl(e, 1));
   vcode_select_unit(v0);

   EXPECT_BB(0) = {
      { VCODE_OP_CONST_ARRAY, .length = 0 },
      { VCODE_OP_CONST, .value = 1 },
      { VCODE_OP_CONST, .value = 0 },
      { VCODE_OP_CONST, .value = 0 },
      { VCODE_OP_WRAP },
      { VCODE_OP_RETURN }
   };

   CHECK_BB(0);
}
END_TEST

START_TEST(test_issue136)
{
   set_standard(STD_00);

   input_from_file(TESTDIR "/lower/issue136.vhd");

   tree_t e = run_elab();
   lower_unit(e);

   vcode_unit_t v0 = find_unit(tree_decl(tree_decl(e, 1), 1));
   vcode_select_unit(v0);

   EXPECT_BB(0) = {
      { VCODE_OP_RECORD_REF, .field = 0 },
      { VCODE_OP_RETURN }
   };

   CHECK_BB(0);
}
END_TEST

START_TEST(test_issue125)
{
   input_from_file(TESTDIR "/lower/issue125.vhd");

   tree_t e = run_elab();
   lower_unit(e);
}
END_TEST

START_TEST(test_access_bug)
{
   input_from_file(TESTDIR "/lower/access_bug.vhd");

   tree_t e = run_elab();
   lower_unit(e);
}
END_TEST

START_TEST(test_rectype)
{
   input_from_file(TESTDIR "/lower/rectype.vhd");

   tree_t e = run_elab();
   lower_unit(e);

   vcode_unit_t v0 = find_unit(e);
   vcode_select_unit(v0);

   fail_unless(vtype_kind(0) == VCODE_TYPE_RECORD);
   fail_unless(vtype_kind(1) == VCODE_TYPE_RECORD);

   ident_t r2_name = vtype_record_name(0);
   fail_unless(strncmp(istr(r2_name), "R2@", 3) == 0);

   ident_t r1_name = vtype_record_name(1);
   fail_unless(icmp(r1_name, "WORK.RECTYPE.R1"));
}
END_TEST

START_TEST(test_issue149)
{
   input_from_file(TESTDIR "/lower/issue149.vhd");

   tree_t e = run_elab();
   lower_unit(e);

   vcode_unit_t v0 = find_unit(tree_decl(e, 1));
   vcode_select_unit(v0);

   EXPECT_BB(0) = {
      { VCODE_OP_CONST, .value = 0 },
      { VCODE_OP_UARRAY_LEN },
      { VCODE_OP_CAST },
      { VCODE_OP_ADDI, .value = -1 },
      { VCODE_OP_CONST, .value = 3 },
      { VCODE_OP_CONST, .value = -1 },
      { VCODE_OP_DYNAMIC_BOUNDS },
      { VCODE_OP_STORE, .name = "I" },
      { VCODE_OP_RETURN }
   };

   CHECK_BB(0);
}
END_TEST

START_TEST(test_issue158)
{
   input_from_file(TESTDIR "/lower/issue158.vhd");

   tree_t p = parse_and_check(T_PACKAGE, T_PACK_BODY);
   simplify(p, 0);
   lower_unit(p);
}
END_TEST

START_TEST(test_issue167)
{
   set_standard(STD_00);

   input_from_file(TESTDIR "/lower/issue167.vhd");

   tree_t e = run_elab();
   lower_unit(e);

   vcode_unit_t v0 = find_unit(e);
   vcode_select_unit(v0);

   fail_unless(vtype_kind(0) == VCODE_TYPE_RECORD);
   fail_unless(vtype_kind(2) == VCODE_TYPE_RECORD);

   ident_t p1_name = vtype_record_name(0);
   fail_unless(icmp(p1_name, "WORK.PKG.P1"));

   ident_t p2_name = vtype_record_name(2);
   fail_unless(strncmp(istr(p2_name), "P2@", 3) == 0);
}
END_TEST

START_TEST(test_issue164)
{
   input_from_file(TESTDIR "/lower/issue164.vhd");

   tree_t p = parse_check_and_simplify(T_PACKAGE, T_PACK_BODY);
   lower_unit(p);

   vcode_select_unit(find_unit(tree_decl(p, 0)));
   fail_unless(icmp(vcode_unit_name(), "WORK.ISSUE164.SAME_NAME(I)"));

   vcode_select_unit(find_unit(tree_decl(p, 1)));
   fail_unless(icmp(vcode_unit_name(), "WORK.ISSUE164.SAME_NAME()I"));
}
END_TEST

START_TEST(test_sigvar)
{
   input_from_file(TESTDIR "/lower/sigvar.vhd");

   tree_t e = run_elab();
   lower_unit(e);

   {
      vcode_unit_t v0 = find_unit(tree_decl(e, 1));
      vcode_select_unit(v0);

      EXPECT_BB(0) = {
         { VCODE_OP_UNWRAP },
         { VCODE_OP_UARRAY_LEN },
         { VCODE_OP_UARRAY_LEFT },
         { VCODE_OP_CAST },
         { VCODE_OP_UARRAY_RIGHT },
         { VCODE_OP_CAST },
         { VCODE_OP_UARRAY_DIR },
         { VCODE_OP_SUB },
         { VCODE_OP_SUB },
         { VCODE_OP_SELECT },
         { VCODE_OP_ADDI, .value = 1 },
         { VCODE_OP_CAST },
         { VCODE_OP_CONST, .value = 0 },
         { VCODE_OP_CMP, .cmp = VCODE_CMP_LT },
         { VCODE_OP_SELECT },
         { VCODE_OP_ALLOCA },
         { VCODE_OP_WRAP },
         { VCODE_OP_STORE, .name = "Y" },
         { VCODE_OP_CONST, .value = 0 },
         { VCODE_OP_MEMSET },
         { VCODE_OP_SELECT },
         { VCODE_OP_SELECT },
         { VCODE_OP_INDEX_CHECK, .subkind = BOUNDS_INDEX_TO },
         { VCODE_OP_ARRAY_SIZE },
         { VCODE_OP_VEC_LOAD },
         { VCODE_OP_COPY },
         { VCODE_OP_RETURN }
      };

      CHECK_BB(0);
   }

   {
      vcode_unit_t v0 = find_unit(tree_decl(e, 2));
      vcode_select_unit(v0);

      EXPECT_BB(0) = {
         { VCODE_OP_CONST, .value = 0 },
         { VCODE_OP_UNWRAP },
         { VCODE_OP_UARRAY_LEN },
         { VCODE_OP_UARRAY_LEN },
         { VCODE_OP_ARRAY_SIZE },
         { VCODE_OP_UNWRAP },
         { VCODE_OP_VEC_LOAD },
         { VCODE_OP_SCHED_WAVEFORM },
         { VCODE_OP_RETURN }
      };

      CHECK_BB(0);
   }
}
END_TEST

START_TEST(test_issue181)
{
   input_from_file(TESTDIR "/lower/issue181.vhd");

   tree_t e = run_elab();
   lower_unit(e);

   vcode_unit_t v0 = find_unit(tree_decl(e, 1));
   vcode_select_unit(v0);

   fail_unless(vcode_count_vars() == 2);

   EXPECT_BB(0) = {
      { VCODE_OP_RETURN }
   };

   CHECK_BB(0);
}
END_TEST

START_TEST(test_issue203)
{
   input_from_file(TESTDIR "/lower/issue203.vhd");

   tree_t e = run_elab();
   lower_unit(e);

   vcode_unit_t v0 = find_unit(e);
   vcode_select_unit(v0);

   fail_unless(vcode_count_vars() == 1);
   fail_unless(icmp(vcode_var_name(0), "STD.TEXTIO.OUTPUT"));
   fail_unless(vcode_var_extern(0));
}
END_TEST

START_TEST(test_issue215)
{
   input_from_file(TESTDIR "/lower/issue215.vhd");

   tree_t e = run_elab();
   lower_unit(e);

   vcode_unit_t v0 = find_unit(e);
   vcode_select_unit(v0);

}
END_TEST

START_TEST(test_choice1)
{
   input_from_file(TESTDIR "/lower/choice1.vhd");

   tree_t e = run_elab();
   lower_unit(e);

   vcode_unit_t v0 = find_unit(tree_stmt(e, 0));
   vcode_select_unit(v0);

   EXPECT_BB(1) = {
      { VCODE_OP_CONST, .value = 1 },
      { VCODE_OP_CONST, .value = 2 },
      { VCODE_OP_CONST, .value = 3 },
      { VCODE_OP_CONST, .value = 4 },
      { VCODE_OP_CONST, .value = 5 },
      { VCODE_OP_VAR_UPREF, .hops = 1, .name = "resolved_:choice1:s" },
      { VCODE_OP_LOAD_INDIRECT },
      { VCODE_OP_LOAD_INDIRECT },
      { VCODE_OP_CASE },
   };

   CHECK_BB(1);

   EXPECT_BB(3) = {
      { VCODE_OP_CONST, .value = 3 },
      { VCODE_OP_STORE, .name = "X" },
      { VCODE_OP_JUMP, .target = 2 }
   };

   CHECK_BB(3);

   EXPECT_BB(4) = {
      { VCODE_OP_CONST, .value = 4 },
      { VCODE_OP_STORE, .name = "X" },
      { VCODE_OP_JUMP, .target = 2 }
   };

   CHECK_BB(4);

   EXPECT_BB(5) = {
      { VCODE_OP_CONST, .value = 5 },
      { VCODE_OP_STORE, .name = "X" },
      { VCODE_OP_JUMP, .target = 2 }
   };

   CHECK_BB(5);
}
END_TEST

START_TEST(test_tag)
{
   input_from_file(TESTDIR "/lower/tag.vhd");

   tree_t e = run_elab();
   lower_unit(e);

   vcode_unit_t v0 = find_unit(e);
   vcode_select_unit(v0);

   EXPECT_BB(0) = {
      { VCODE_OP_CONST, .value = 0 },
      { VCODE_OP_SET_INITIAL },
      { VCODE_OP_NEEDS_LAST_VALUE, .name = ":work:p:s" },
      { VCODE_OP_RESOLVED_ADDRESS, .name = ":work:p:s" },
      { VCODE_OP_SET_INITIAL },
      { VCODE_OP_RESOLVED_ADDRESS, .name = ":tag:p" },
      { VCODE_OP_SET_INITIAL },
      { VCODE_OP_NEEDS_LAST_VALUE, .name = ":tag:x" },
      { VCODE_OP_RESOLVED_ADDRESS, .name = ":tag:x" },
      { VCODE_OP_SET_INITIAL },
      { VCODE_OP_RESOLVED_ADDRESS, .name = ":tag:y" },
      { VCODE_OP_NEEDS_LAST_VALUE, .name = ":tag:sub_i:i" },
      { VCODE_OP_RESOLVED_ADDRESS, .name = ":tag:sub_i:i" },
      { VCODE_OP_RETURN },
   };

   CHECK_BB(0);
}
END_TEST

START_TEST(test_iffold)
{
   input_from_file(TESTDIR "/lower/iffold.vhd");

   tree_t e = run_elab();
   lower_unit(e);

   vcode_unit_t v0 = find_unit(tree_stmt(e, 0));
   vcode_select_unit(v0);

   EXPECT_BB(1) = {
      { VCODE_OP_CONST, .value = 0 },
      { VCODE_OP_NETS, .name = ":iffold:sub_i:x" },
      { VCODE_OP_CONST, .value = 5 },
      { VCODE_OP_CONST, .value = 1 },
      { VCODE_OP_SCHED_WAVEFORM },
      { VCODE_OP_JUMP, .target = 1 }
   };

   CHECK_BB(1);
}
END_TEST

START_TEST(test_real1)
{
   input_from_file(TESTDIR "/lower/real1.vhd");

   tree_t e = run_elab();
   lower_unit(e);

   {
      vcode_unit_t v0 = find_unit(tree_decl(e, 1));
      vcode_select_unit(v0);

      EXPECT_BB(0) = {
         { VCODE_OP_NEG },
         { VCODE_OP_STORE, .name = "R" },
         { VCODE_OP_RETURN }
      };

      CHECK_BB(0);
   }

   {
      vcode_unit_t v0 = find_unit(tree_decl(e, 2));
      vcode_select_unit(v0);

      EXPECT_BB(0) = {
         { VCODE_OP_STORE, .name = "Z" },
         { VCODE_OP_CAST },
         { VCODE_OP_CONST_REAL, .real = 0.5 },
         { VCODE_OP_MUL },
         { VCODE_OP_RETURN }
      };

      CHECK_BB(0);
   }
}
END_TEST

START_TEST(test_assert1)
{
   input_from_file(TESTDIR "/lower/assert1.vhd");

   tree_t e = run_elab();
   lower_unit(e);

   vcode_unit_t v0 = find_unit(tree_stmt(e, 0));
   vcode_select_unit(v0);

   EXPECT_BB(1) = {
      { VCODE_OP_CONST, .value = 1 },
      { VCODE_OP_STORE, .name = "B" },
      { VCODE_OP_WAIT, .target = 2 }
   };

   CHECK_BB(1);
}
END_TEST

START_TEST(test_thunk)
{
   input_from_file(TESTDIR "/lower/thunk.vhd");

   tree_t arch = parse_check_and_simplify(T_PACKAGE, T_ENTITY, T_ARCH);

   vcode_unit_t t0 = lower_thunk(tree_value(tree_decl(arch, 0)));
   fail_if(t0 == NULL);
   vcode_select_unit(t0);

   EXPECT_BB(0) = {
      { VCODE_OP_CONST, .value = 1 },
      { VCODE_OP_CONST_ARRAY, .length = 4 },
      { VCODE_OP_ADDI, .value = 2 },
      { VCODE_OP_LOAD_INDIRECT },
      { VCODE_OP_NOT },
      { VCODE_OP_RETURN },
   };

   CHECK_BB(0);

   vcode_unit_t t1 = lower_thunk(tree_value(tree_decl(arch, 1)));
   fail_unless(t1 == NULL);
}
END_TEST

START_TEST(test_issue303)
{
   input_from_file(TESTDIR "/lower/issue303.vhd");

   tree_t e = run_elab();
   lower_unit(e);
}
END_TEST

START_TEST(test_dealloc)
{
   input_from_file(TESTDIR "/lower/dealloc.vhd");

   tree_t p = parse_check_and_simplify(T_PACKAGE, T_PACK_BODY);
   lower_unit(p);

   vcode_unit_t v1 = find_unit(tree_decl(p, 1));
   vcode_select_unit(v1);

   EXPECT_BB(0) = {
      { VCODE_OP_FCALL, .func = "WORK.PACK.ANOTHER_PROC(13WORK.PACK.PTR)" },
      { VCODE_OP_RETURN }
   };

   CHECK_BB(0);
}
END_TEST

START_TEST(test_issue324)
{
   input_from_file(TESTDIR "/lower/issue324.vhd");

   tree_t e = run_elab();
   lower_unit(e);
}
END_TEST

START_TEST(test_issue333)
{
   input_from_file(TESTDIR "/lower/issue333.vhd");

   tree_t e = run_elab();
   lower_unit(e);

   vcode_unit_t v0 = find_unit(tree_stmt(e, 0));
   vcode_select_unit(v0);

   EXPECT_BB(1) = {
      { VCODE_OP_CONST, .value = 49 },
      { VCODE_OP_CONST_ARRAY, .length = 1 },
      { VCODE_OP_CONST, .value = 1 },
      { VCODE_OP_CONST, .value = 0 },
      { VCODE_OP_CONST, .value = 1 },
      { VCODE_OP_NEW },
      { VCODE_OP_ALL },
      { VCODE_OP_COPY },
      { VCODE_OP_WRAP },
      { VCODE_OP_NEW },
      { VCODE_OP_ALL },
      { VCODE_OP_STORE_INDIRECT },
      { VCODE_OP_STORE, .name = "MAIN.L" },
      { VCODE_OP_INDEX, .name = "MAIN.L" },
      { VCODE_OP_FCALL, .name = ":issue333:proc(vuLINE;", .args = 1 },
      { VCODE_OP_CONST, .value = 50 },
      { VCODE_OP_CONST_ARRAY, .length = 2 },
      { VCODE_OP_CONST, .value = 2 },
      { VCODE_OP_CONST, .value = 2 },
      { VCODE_OP_NEW },
      { VCODE_OP_ALL },
      { VCODE_OP_COPY },
      { VCODE_OP_WRAP },
      { VCODE_OP_NEW },
      { VCODE_OP_ALL },
      { VCODE_OP_STORE_INDIRECT },
      { VCODE_OP_STORE, .name = "MAIN.L" },
      { VCODE_OP_WAIT, .target = 2 }
   };

   CHECK_BB(1);
}
END_TEST

START_TEST(test_issue338)
{
   input_from_file(TESTDIR "/lower/issue338.vhd");

   tree_t e = run_elab();
   lower_unit(e);

   // Function f1
   {
      vcode_unit_t vu = find_unit(tree_decl(e, 1));
      vcode_select_unit(vu);

      EXPECT_BB(0) = {
         { VCODE_OP_ALLOCA },
         { VCODE_OP_STORE_INDIRECT },
         { VCODE_OP_COND, .target = 1, .target_else = 2 },
      };

      CHECK_BB(0);

      EXPECT_BB(1) = {
         { VCODE_OP_FCALL, .func = "WORK.P.F()B" },
         { VCODE_OP_AND },
         { VCODE_OP_STORE_INDIRECT },
         { VCODE_OP_JUMP, .target = 2 },
      };

      CHECK_BB(1);

      EXPECT_BB(2) = {
         { VCODE_OP_LOAD_INDIRECT },
         { VCODE_OP_RETURN },
      };

      CHECK_BB(2);
   }

   // Function f2
   {
      vcode_unit_t vu = find_unit(tree_decl(e, 2));
      vcode_select_unit(vu);

      EXPECT_BB(0) = {
         { VCODE_OP_AND },
         { VCODE_OP_RETURN }
      };

      CHECK_BB(0);
   }

   // Function f3
   {
      vcode_unit_t vu = find_unit(tree_decl(e, 3));
      vcode_select_unit(vu);

      EXPECT_BB(0) = {
         { VCODE_OP_OR },
         { VCODE_OP_AND },
         { VCODE_OP_RETURN }
      };

      CHECK_BB(0);
   }

   // Function f4
   {
      vcode_unit_t vu = find_unit(tree_decl(e, 4));
      vcode_select_unit(vu);

      EXPECT_BB(0) = {
         { VCODE_OP_CONST, .value = 0 },
         { VCODE_OP_STORE, .name = "Y" },
         { VCODE_OP_RETURN }
      };

      CHECK_BB(0);
   }

   // Function f5
   {
      vcode_unit_t vu = find_unit(tree_decl(e, 5));
      vcode_select_unit(vu);

      EXPECT_BB(0) = {
         { VCODE_OP_CONST, .value = 0 },
         { VCODE_OP_STORE, .name = "Y" },
         { VCODE_OP_FCALL, .func = "WORK.P.F()B" },
         { VCODE_OP_NOT },
         { VCODE_OP_RETURN }
      };

      CHECK_BB(0);
   }

   // Function f6
   {
      vcode_unit_t vu = find_unit(tree_decl(e, 6));
      vcode_select_unit(vu);

      EXPECT_BB(0) = {
         { VCODE_OP_ALLOCA },
         { VCODE_OP_STORE_INDIRECT },
         { VCODE_OP_COND, .target = 2, .target_else = 1 },
      };

      CHECK_BB(0);

      EXPECT_BB(1) = {
         { VCODE_OP_FCALL, .func = "WORK.P.F()B" },
         { VCODE_OP_OR },
         { VCODE_OP_STORE_INDIRECT },
         { VCODE_OP_JUMP, .target = 2 },
      };

      CHECK_BB(1);

      EXPECT_BB(2) = {
         { VCODE_OP_LOAD_INDIRECT },
         { VCODE_OP_RETURN },
      };

      CHECK_BB(2);
   }

   // Function f7
   {
      vcode_unit_t vu = find_unit(tree_decl(e, 7));
      vcode_select_unit(vu);

      EXPECT_BB(0) = {
         { VCODE_OP_ALLOCA },
         { VCODE_OP_NOT },
         { VCODE_OP_STORE_INDIRECT },
         { VCODE_OP_COND, .target = 2, .target_else = 1 },
      };

      CHECK_BB(0);

      EXPECT_BB(1) = {
         { VCODE_OP_FCALL, .func = "WORK.P.F()B" },
         { VCODE_OP_NOR },
         { VCODE_OP_STORE_INDIRECT },
         { VCODE_OP_JUMP, .target = 2 },
      };

      CHECK_BB(1);

      EXPECT_BB(2) = {
         { VCODE_OP_LOAD_INDIRECT },
         { VCODE_OP_RETURN },
      };

      CHECK_BB(2);
   }
}
END_TEST

START_TEST(test_issue338b)
{
   input_from_file(TESTDIR "/lower/issue338b.vhd");

   tree_t e = run_elab();
   lower_unit(e);

   vcode_unit_t v0 = find_unit(tree_decl(e, 1));
   vcode_select_unit(v0);

   EXPECT_BB(0) = {
      { VCODE_OP_CONST_ARRAY, .length = 0 },
      { VCODE_OP_UNWRAP },
      { VCODE_OP_UARRAY_LEN },
      { VCODE_OP_CONST, .value = 0 },
      { VCODE_OP_CMP, .cmp = VCODE_CMP_EQ },
      { VCODE_OP_ALLOCA },
      { VCODE_OP_STORE_INDIRECT },
      { VCODE_OP_COND, .target = 1, .target_else = 2 }
   };

   CHECK_BB(0);

   EXPECT_BB(1) = {
      { VCODE_OP_MEMCMP },
      { VCODE_OP_STORE_INDIRECT },
      { VCODE_OP_JUMP, .target = 2 }
   };

   CHECK_BB(1);

   EXPECT_BB(2) = {
      { VCODE_OP_LOAD_INDIRECT },
      { VCODE_OP_RETURN }
   };

   CHECK_BB(2);
}
END_TEST

START_TEST(test_issue347)
{
   input_from_file(TESTDIR "/lower/issue347.vhd");

   tree_t e = run_elab();
   lower_unit(e);
}
END_TEST

START_TEST(test_hintbug)
{
   input_from_file(TESTDIR "/lower/hintbug.vhd");

   tree_t e = run_elab();
   lower_unit(e);

   vcode_unit_t v0 = find_unit(tree_stmt(e, 0));
   vcode_select_unit(v0);

   EXPECT_BB(1) = {
      { VCODE_OP_HEAP_SAVE },
      { VCODE_OP_INDEX, .name = "V" },
      { VCODE_OP_CONST, .value = 2 },
      { VCODE_OP_LOAD, .name = "X" },
      { VCODE_OP_FCALL, .func = ":hintbug:func(J)Q" },
      { VCODE_OP_UNWRAP },
      { VCODE_OP_UARRAY_LEN },
      { VCODE_OP_ARRAY_SIZE },
      { VCODE_OP_COPY },
      { VCODE_OP_HEAP_RESTORE },
      { VCODE_OP_CONST, .value = 2 },
      { VCODE_OP_CONST, .value = 0 },
      { VCODE_OP_ALLOCA },
      { VCODE_OP_STORE_INDIRECT },
      { VCODE_OP_ADDI, .value = 1 },
      { VCODE_OP_STORE_INDIRECT },
      { VCODE_OP_MEMCMP },
      { VCODE_OP_ASSERT },
      { VCODE_OP_WAIT, .target = 2 }
   };

   CHECK_BB(1);
}
END_TEST

START_TEST(test_issue351)
{
   input_from_file(TESTDIR "/lower/issue351.vhd");

   tree_t e = run_elab();
   lower_unit(e);

   vcode_unit_t v0 = find_unit(tree_stmt(e, 0));
   vcode_select_unit(v0);

   EXPECT_BB(6) = {
      { VCODE_OP_CAST },
      { VCODE_OP_ADD },
      { VCODE_OP_WRAP },
      { VCODE_OP_FCALL, .func = ":issue351:dump_words(11WORD_VECTOR)" },
      { VCODE_OP_LOAD, .name = "I.LOOP1" },
      { VCODE_OP_ADDI, .value = 1 },
      { VCODE_OP_STORE, .name = "I.LOOP1" },
      { VCODE_OP_CONST, .value = 3 },
      { VCODE_OP_CMP, .cmp = VCODE_CMP_EQ },
      { VCODE_OP_COND, .target = 3, .target_else = 4 },
   };

   CHECK_BB(6);
}
END_TEST

START_TEST(test_tounsigned)
{
   input_from_file(TESTDIR "/lower/tounsigned.vhd");

   tree_t p = parse_check_and_simplify(T_PACKAGE, T_PACK_BODY);
   lower_unit(p);

   vcode_unit_t v0 = find_unit(tree_decl(p, 0));
   vcode_select_unit(v0);

   EXPECT_BB(0) = {
      { VCODE_OP_ADDI, .value = -1 },
      { VCODE_OP_CONST, .value = 0 },
      { VCODE_OP_ADDI, .value = 1 },
      { VCODE_OP_CAST },
      { VCODE_OP_CONST, .value = 0 },
      { VCODE_OP_CMP, .cmp = VCODE_CMP_LT },
      { VCODE_OP_SELECT },
      { VCODE_OP_ALLOCA, .subkind = VCODE_ALLOCA_HEAP },
      { VCODE_OP_CONST, .value = 1 },
      { VCODE_OP_WRAP },
      { VCODE_OP_STORE, .name = "RESULT" },
      { VCODE_OP_CONST, .value = 0 },
      { VCODE_OP_MEMSET },
      { VCODE_OP_INDEX_CHECK, .subkind = BOUNDS_INDEX_TO },
      { VCODE_OP_STORE, .name = "I_VAL" },
      { VCODE_OP_CMP, .cmp = VCODE_CMP_GT },
      { VCODE_OP_COND, .target = 2, .target_else = 1 },
   };

   CHECK_BB(0);

   EXPECT_BB(1) = {
      { VCODE_OP_STORE, .name = "I.MAINLOOP" },
      { VCODE_OP_JUMP, .target = 3 }
   };

   CHECK_BB(1);

   EXPECT_BB(2) = {
      { VCODE_OP_LOAD, .name = "RESULT" },
      { VCODE_OP_RETURN }
   };

   CHECK_BB(2);

   EXPECT_BB(3) = {
      { VCODE_OP_LOAD, .name = "I_VAL" },
      { VCODE_OP_CONST, .value = 2 },
      { VCODE_OP_MOD },
      { VCODE_OP_CONST, .value = 0 },
      { VCODE_OP_CMP, .cmp = VCODE_CMP_EQ },
      { VCODE_OP_COND, .target = 4, .target_else = 5 }
   };

   CHECK_BB(3);

   EXPECT_BB(4) = {
      { VCODE_OP_CONST, .value = 0 },
      { VCODE_OP_LOAD, .name = "RESULT" },
      { VCODE_OP_LOAD, .name = "I.MAINLOOP" },
      { VCODE_OP_ADDI, .value = -1 },
      { VCODE_OP_CONST, .value = 0 },
      { VCODE_OP_CONST, .value = 1 },
      { VCODE_OP_DYNAMIC_BOUNDS },
      { VCODE_OP_SUB },
      { VCODE_OP_SUB },
      { VCODE_OP_UARRAY_DIR },
      { VCODE_OP_SELECT },
      { VCODE_OP_CAST },
      { VCODE_OP_UNWRAP },
      { VCODE_OP_ADD },
      { VCODE_OP_STORE_INDIRECT },
      { VCODE_OP_JUMP, .target = 6 },
   };

   CHECK_BB(4);

   EXPECT_BB(5) = {
      { VCODE_OP_CONST, .value = 1 },
      { VCODE_OP_LOAD, .name = "RESULT" },
      { VCODE_OP_LOAD, .name = "I.MAINLOOP" },
      { VCODE_OP_ADDI, .value = -1 },
      { VCODE_OP_CONST, .value = 0 },
      { VCODE_OP_CONST, .value = 1 },
      { VCODE_OP_DYNAMIC_BOUNDS },
      { VCODE_OP_SUB },
      { VCODE_OP_SUB },
      { VCODE_OP_UARRAY_DIR },
      { VCODE_OP_SELECT },
      { VCODE_OP_CAST },
      { VCODE_OP_UNWRAP },
      { VCODE_OP_ADD },
      { VCODE_OP_STORE_INDIRECT },
      { VCODE_OP_JUMP, .target = 6 }
   };

   CHECK_BB(5);

   EXPECT_BB(6) = {
      { VCODE_OP_LOAD, .name = "I_VAL" },
      { VCODE_OP_CONST, .value = 2 },
      { VCODE_OP_DIV },
      { VCODE_OP_STORE, .name = "I_VAL" },
      { VCODE_OP_LOAD, .name = "I.MAINLOOP" },
      { VCODE_OP_ADDI, .value = 1 },
      { VCODE_OP_STORE, .name = "I.MAINLOOP" },
      { VCODE_OP_ADDI, .value = -1 },
      { VCODE_OP_CMP, .cmp = VCODE_CMP_EQ },
      { VCODE_OP_COND, .target = 2, .target_else = 3 }
   };

   CHECK_BB(6);
}
END_TEST

START_TEST(test_issue357)
{
   input_from_file(TESTDIR "/lower/issue357.vhd");

   tree_t e = run_elab();
   lower_unit(e);
}
END_TEST

START_TEST(test_signal11)
{
   input_from_file(TESTDIR "/lower/signal11.vhd");

   tree_t e = run_elab();
   lower_unit(e);

   vcode_unit_t vpack = vcode_find_unit(ident_new("WORK.PACK"));
   fail_if(vpack == NULL);

   vcode_select_unit(vpack);
   fail_unless(vcode_count_signals() == 1);
   fail_unless(vcode_signal_name(0) == ident_new(":work:pack:x"));
   fail_if(vcode_signal_extern(0));
   fail_unless(vcode_signal_count_nets(0) == 1);
   fail_unless(vcode_signal_nets(0)[0] == NETID_INVALID);

   vcode_unit_t vbody = vcode_find_unit(ident_new("WORK.PACK-body"));
   fail_if(vpack == NULL);

   vcode_select_unit(vbody);
   fail_unless(vcode_count_signals() == 1);
   fail_unless(vcode_signal_name(0) == ident_new(":work:pack:x"));
   fail_unless(vcode_signal_extern(0));
}
END_TEST

START_TEST(test_access1)
{
   input_from_file(TESTDIR "/lower/access1.vhd");

   tree_t e = run_elab();
   lower_unit(e);

   vcode_unit_t v0 = find_unit(tree_decl(e, 1));
   vcode_select_unit(v0);

   EXPECT_BB(0) = {
      { VCODE_OP_NULL },
      { VCODE_OP_NEW },
      { VCODE_OP_ALL },
      { VCODE_OP_CONST, .value = INT32_MIN },
      { VCODE_OP_CONST_RECORD },
      { VCODE_OP_STORE_INDIRECT },
      { VCODE_OP_STORE, .name = "N" },
      { VCODE_OP_LOAD_INDIRECT },
      { VCODE_OP_RECORD_REF, .field = 0 },
      { VCODE_OP_STORE_INDIRECT },
      { VCODE_OP_RECORD_REF, .field = 1 },
      { VCODE_OP_STORE_INDIRECT },
      { VCODE_OP_STORE_INDIRECT },
      { VCODE_OP_RETURN }
   };

   CHECK_BB(0);
}
END_TEST

START_TEST(test_sum)
{
   input_from_file(TESTDIR "/lower/sum.vhd");

   tree_t p = parse_check_and_simplify(T_PACKAGE, T_PACK_BODY);
   bounds_check(p);
   fail_if(bounds_errors() > 0);
   lower_unit(p);

   vcode_unit_t v0 = find_unit(tree_decl(p, 1));
   vcode_select_unit(v0);

   EXPECT_BB(0) = {
      { VCODE_OP_CONST, .value = 0 },
      { VCODE_OP_STORE, .name = "RESULT" },
      { VCODE_OP_UARRAY_LEFT },
      { VCODE_OP_CAST },
      { VCODE_OP_UARRAY_RIGHT },
      { VCODE_OP_CAST },
      { VCODE_OP_UARRAY_DIR },
      { VCODE_OP_RANGE_NULL },
      { VCODE_OP_COND, .target = 2, .target_else = 1 }
   };

   CHECK_BB(0);

   EXPECT_BB(3) = {
      { VCODE_OP_LOAD, .name = "RESULT" },
      { VCODE_OP_LOAD, .name = "I.SUMLOOP" },
      { VCODE_OP_UARRAY_LEFT },
      { VCODE_OP_CAST },
      { VCODE_OP_SUB },
      { VCODE_OP_SUB },
      { VCODE_OP_UARRAY_DIR },
      { VCODE_OP_SELECT },
      { VCODE_OP_CAST },
      { VCODE_OP_UNWRAP },
      { VCODE_OP_ADD },
      { VCODE_OP_LOAD_INDIRECT },
      { VCODE_OP_ADD },
      { VCODE_OP_BOUNDS, .low = INT32_MIN, .high = INT32_MAX },
      { VCODE_OP_STORE, .name = "RESULT" },
      { VCODE_OP_ADD },
      { VCODE_OP_STORE, .name = "I.SUMLOOP" },
      { VCODE_OP_UARRAY_RIGHT },
      { VCODE_OP_CAST },
      { VCODE_OP_CMP, .cmp = VCODE_CMP_EQ },
      { VCODE_OP_COND, .target = 2, .target_else = 3 }
   };

   CHECK_BB(3);
}
END_TEST

Suite *get_lower_tests(void)
{
   Suite *s = suite_create("lower");

   TCase *tc = nvc_unit_test();
   tcase_add_test(tc, test_wait1);
   tcase_add_test(tc, test_assign1);
   tcase_add_test(tc, test_assign2);
   tcase_add_test(tc, test_signal1);
   tcase_add_test(tc, test_cond1);
   tcase_add_test(tc, test_arith1);
   tcase_add_test(tc, test_pack1);
   tcase_add_test(tc, test_func1);
   tcase_add_test(tc, test_issue94);
   tcase_add_test(tc, test_arrayop1);
   tcase_add_test(tc, test_array1);
   tcase_add_test(tc, test_nest1);
   tcase_add_test(tc, test_signal2);
   tcase_add_test(tc, test_attr1);
   tcase_add_test(tc, test_record1);
   tcase_add_test(tc, test_assign3);
   tcase_add_test(tc, test_signal4);
   tcase_add_test(tc, test_staticwait);
   tcase_add_test(tc, test_proc1);
   tcase_add_test(tc, test_while1);
   tcase_add_test(tc, test_loop1);
   tcase_add_test(tc, test_proc3);
   tcase_add_test(tc, test_loop2);
   tcase_add_test(tc, test_slice1);
   tcase_add_test(tc, test_funcif);
   tcase_add_test(tc, test_memset);
   tcase_add_test(tc, test_func5);
   tcase_add_test(tc, test_bounds1);
   tcase_add_test(tc, test_record6);
   tcase_add_test(tc, test_proc7);
   tcase_add_test(tc, test_mulphys);
   tcase_add_test(tc, test_issue116);
   tcase_add_test(tc, test_cover);
   tcase_add_test(tc, test_issue122);
   tcase_add_test(tc, test_issue124);
   tcase_add_test(tc, test_issue135);
   tcase_add_test(tc, test_issue134);
   tcase_add_test(tc, test_issue136);
   tcase_add_test(tc, test_issue125);
   tcase_add_test(tc, test_access_bug);
   tcase_add_test(tc, test_rectype);
   tcase_add_test(tc, test_issue149);
   tcase_add_test(tc, test_issue158);
   tcase_add_test(tc, test_issue167);
   tcase_add_test(tc, test_issue164);
   tcase_add_test(tc, test_sigvar);
   tcase_add_test(tc, test_issue181);
   tcase_add_test(tc, test_issue203);
   tcase_add_test(tc, test_issue215);
   tcase_add_test(tc, test_choice1);
   tcase_add_test(tc, test_tag);
   tcase_add_test(tc, test_iffold);
   tcase_add_test(tc, test_real1);
   tcase_add_test(tc, test_assert1);
   tcase_add_test(tc, test_thunk);
   tcase_add_test(tc, test_issue303);
   tcase_add_test(tc, test_dealloc);
   tcase_add_test(tc, test_issue324);
   tcase_add_test(tc, test_issue333);
   tcase_add_test(tc, test_issue338);
   tcase_add_test(tc, test_issue338b);
   tcase_add_test(tc, test_issue347);
   tcase_add_test(tc, test_hintbug);
   tcase_add_test(tc, test_issue351);
   tcase_add_test(tc, test_tounsigned);
   tcase_add_test(tc, test_issue357);
   tcase_add_test(tc, test_signal11);
   tcase_add_test(tc, test_access1);
   tcase_add_test(tc, test_sum);
   suite_add_tcase(s, tc);

   return s;
}
