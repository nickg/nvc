//
//  Copyright (C) 2014-2022  Nick Gasson
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
#include "diag.h"
#include "opt.h"
#include "phase.h"
#include "rt/cover.h"
#include "scan.h"
#include "vcode.h"

#include <inttypes.h>

typedef struct {
   vcode_op_t    op;
   const char   *func;
   const char   *name;
   int64_t       value;
   double        real;
   vcode_cmp_t   cmp;
   vcode_block_t target;
   vcode_block_t target_else;
   uint32_t      tag;
   int           length;
   bool          delay;
   uint8_t       dim;
   uint8_t       hops;
   uint8_t       field;
   uint8_t       subkind;
} check_bb_t;

#define CAT(x, y) x##y
#define CHECK_BB(n) check_bb(n, CAT(bb, n), ARRAY_LEN(CAT(bb, n)))
#define EXPECT_BB(n) const check_bb_t CAT(bb, n)[]

static bool fuzzy_cmp(ident_t id, const char *str)
{
   if (*str == '*')
      return strstr(istr(id), str + 1) != NULL;
   else
      return ident_new(str) == id;
}

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
         vcode_dump_with_mark(i, NULL, NULL);
         fail("expected op %d in block %d to be %s but was %s",
              i, bb, vcode_op_string(e->op), vcode_op_string(vop));
      }

      switch (e->op) {
      case VCODE_OP_PCALL:
         if (e->target != vcode_get_target(i, 0)) {
            vcode_dump_with_mark(i, NULL, NULL);
            fail("expected op %d in block %d to have resume target %d but "
                 "has %d", i, bb, e->target, vcode_get_target(i, 0));
         }
         // Fall-through
      case VCODE_OP_FCALL:
      case VCODE_OP_CLOSURE:
         if (!fuzzy_cmp(vcode_get_func(i), e->func)) {
            vcode_dump_with_mark(i, NULL, NULL);
            fail("expected op %d in block %d to call %s but calls %s",
                 i, bb, e->func, istr(vcode_get_func(i)));
         }
         break;

      case VCODE_OP_CONST:
         if (e->value != vcode_get_value(i)) {
            vcode_dump_with_mark(i, NULL, NULL);
            fail("expected op %d in block %d to have constant %"PRIi64
                 " but has %"PRIi64, i, bb, e->value, vcode_get_value(i));
         }
         break;

      case VCODE_OP_CONST_REP:
         if (e->value != vcode_get_value(i)) {
            vcode_dump_with_mark(i, NULL, NULL);
            fail("expected op %d in block %d to have repeat count %"PRIi64
                 " but has %"PRIi64, i, bb, e->value, vcode_get_value(i));
         }
         break;

      case VCODE_OP_CONST_REAL:
         if (e->real != vcode_get_real(i)) {
            vcode_dump_with_mark(i, NULL, NULL);
            fail("expected op %d in block %d to have constant %lf but has %lf",
                 i, bb, e->real, vcode_get_real(i));
         }
         break;

      case VCODE_OP_CMP:
         if (e->cmp != vcode_get_cmp(i)) {
            vcode_dump_with_mark(i, NULL, NULL);
            fail("expected op %d in block %d to have comparison %"PRIi64
                 " but has %d", i, bb, e->value, vcode_get_cmp(i));
         }
         break;

      case VCODE_OP_ASSERT:
      case VCODE_OP_REPORT:
      case VCODE_OP_RETURN:
      case VCODE_OP_UNWRAP:
      case VCODE_OP_LENGTH_CHECK:
      case VCODE_OP_EXPONENT_CHECK:
      case VCODE_OP_NULL:
      case VCODE_OP_RANGE_NULL:
      case VCODE_OP_RANGE_LENGTH:
      case VCODE_OP_NULL_CHECK:
      case VCODE_OP_FILE_WRITE:
         break;

      case VCODE_OP_UARRAY_LEFT:
      case VCODE_OP_UARRAY_RIGHT:
      case VCODE_OP_UARRAY_DIR:
      case VCODE_OP_UARRAY_LEN:
         if (e->dim != vcode_get_dim(i)) {
            vcode_dump_with_mark(i, NULL, NULL);
            fail("expected op %d in block %d to have dimension %d but has %d",
                 i, bb, e->dim, vcode_get_dim(i));
         }
         break;

      case VCODE_OP_WAIT:
         if (e->target != vcode_get_target(i, 0)) {
            vcode_dump_with_mark(i, NULL, NULL);
            fail("expected op %d in block %d to have wait target %d but has %d",
                 i, bb, e->target, vcode_get_target(i, 0));
         }
         else if (e->delay && vcode_get_arg(i, 0) == VCODE_INVALID_REG) {
            vcode_dump_with_mark(i, NULL, NULL);
            fail("expected op %d in block %d to have wait delay", i, bb);
         }
         break;

      case VCODE_OP_COND:
         if (e->target_else != vcode_get_target(i, 1)) {
            vcode_dump_with_mark(i, NULL, NULL);
            fail("expected op %d in block %d to have else target %d but has %d",
                 i, bb, e->target_else, vcode_get_target(i, 1));
         }
         // Fall-through

      case VCODE_OP_JUMP:
         if (e->target != vcode_get_target(i, 0)) {
            vcode_dump_with_mark(i, NULL, NULL);
            fail("expected op %d in block %d to have jump target %d but has %d",
                 i, bb, e->target, vcode_get_target(i, 0));
         }
         break;

      case VCODE_OP_STORE:
      case VCODE_OP_LOAD:
      case VCODE_OP_INDEX:
         {
            ident_t name = vcode_var_name(vcode_get_address(i));
            if (!fuzzy_cmp(name, e->name)) {
               vcode_dump_with_mark(i, NULL, NULL);
               fail("expected op %d in block %d to have address name %s but "
                    "has %s", i, bb, e->name, istr(name));
            }
         }
         break;

      case VCODE_OP_LINK_VAR:
      case VCODE_OP_LINK_PACKAGE:
         {
            ident_t name = vcode_get_ident(i);
            if (name != ident_new(e->name)) {
               vcode_dump_with_mark(i, NULL, NULL);
               fail("expected op %d in block %d to have name %s but has %s",
                    i, bb, e->name, istr(name));
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
      case VCODE_OP_ARRAY_REF:
      case VCODE_OP_ADDRESS_OF:
      case VCODE_OP_SCHED_WAVEFORM:
      case VCODE_OP_SELECT:
      case VCODE_OP_EVENT:
      case VCODE_OP_ACTIVE:
      case VCODE_OP_CONST_RECORD:
      case VCODE_OP_COPY:
      case VCODE_OP_MEMSET:
      case VCODE_OP_WRAP:
      case VCODE_OP_NEW:
      case VCODE_OP_ALL:
      case VCODE_OP_DEALLOCATE:
      case VCODE_OP_CASE:
      case VCODE_OP_TEMP_STACK_MARK:
      case VCODE_OP_TEMP_STACK_RESTORE:
      case VCODE_OP_INIT_SIGNAL:
      case VCODE_OP_MAP_SIGNAL:
      case VCODE_OP_DRIVE_SIGNAL:
      case VCODE_OP_MAP_CONST:
      case VCODE_OP_RESOLVED:
      case VCODE_OP_RESOLUTION_WRAPPER:
      case VCODE_OP_RESOLVE_SIGNAL:
      case VCODE_OP_ALIAS_SIGNAL:
      case VCODE_OP_TRAP_ADD:
      case VCODE_OP_TRAP_SUB:
      case VCODE_OP_TRAP_MUL:
         break;

      case VCODE_OP_CONST_ARRAY:
         if (vcode_count_args(i) != e->length) {
            vcode_dump_with_mark(i, NULL, NULL);
            fail("expected op %d in block %d to have %d array elements but "
                 "has %d", i, bb, e->length, vcode_count_args(i));
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

            ident_t actual = vcode_var_name(address);
            vcode_state_restore(&state);

            if (!icmp(actual, e->name)) {
               vcode_dump_with_mark(i, NULL, NULL);
               fail("expect op %d in block %d to have address %s"
                    " but has %s", i, bb, e->name, istr(actual));
            }
         }
         // Fall-through

      case VCODE_OP_CONTEXT_UPREF:
         if (vcode_get_hops(i) != e->hops) {
            vcode_dump_with_mark(i, NULL, NULL);
            fail("expect op %d in block %d to have hop count %d"
                 " but has %d", i, bb, e->hops, vcode_get_hops(i));
         }
         break;

      case VCODE_OP_RECORD_REF:
         if (vcode_get_field(i) != e->field) {
            vcode_dump_with_mark(i, NULL, NULL);
            fail("expect op %d in block %d to have field %d"
                 " but has %d", i, bb, e->field, vcode_get_field(i));
         }
         break;

      case VCODE_OP_ALLOCA:
         if (vcode_get_subkind(i) != e->subkind) {
            vcode_dump_with_mark(i, NULL, NULL);
            fail("expected op %d in block %d to have subkind %x but "
                 "has %x", i, bb, e->subkind, vcode_get_subkind(i));
         }
         break;

      case VCODE_OP_SCHED_EVENT:
      case VCODE_OP_SCHED_STATIC:
      case VCODE_OP_FILE_OPEN:
      case VCODE_OP_FILE_CLOSE:
      case VCODE_OP_INDEX_CHECK:
      case VCODE_OP_RANGE_CHECK:
      case VCODE_OP_DEBUG_LOCUS:
         break;

      case VCODE_OP_RESUME:
         if ((e->func != NULL) && !icmp(vcode_get_func(i), e->func)) {
            vcode_dump_with_mark(i, NULL, NULL);
            fail("expected op %d in block %d to call %s but calls %s",
                 i, bb, e->func, istr(vcode_get_func(i)));
         }
         break;

      case VCODE_OP_COVER_COND:
         if (vcode_get_subkind(i) != e->subkind) {
            vcode_dump_with_mark(i, NULL, NULL);
            fail("expected op %d in block %d to have sub cond %x but "
                 "has %x", i, bb, e->subkind, vcode_get_subkind(i));
         }
         // Fall-through

      case VCODE_OP_COVER_STMT:
         if (e->tag != vcode_get_tag(i)) {
            vcode_dump_with_mark(i, NULL, NULL);
            fail("expected op %d in block %d to have cover tag %d but has %d",
                 i, bb, e->tag, vcode_get_tag(i));
         }
         break;

      default:
         fail("cannot check op %s", vcode_op_string(e->op));
      }
   }

   if (actual != eptr) {
      vcode_dump_with_mark(len + nops - actual, NULL, NULL);
      fail("expected %d ops in block %d but have %d", len, bb, actual);
   }
}

static vcode_unit_t find_unit_for(tree_t decl)
{
   fail_unless(is_subprogram(decl));

   vcode_unit_t vu = vcode_find_unit(tree_ident2(decl));
   if (vu == NULL)
      fail("missing vcode unit for %s", istr(tree_ident2(decl)));
   return vu;
}

static vcode_unit_t find_unit(const char *name)
{
   vcode_unit_t vu = vcode_find_unit(ident_new(name));
   if (vu == NULL)
      fail("missing vcode unit for %s", name);
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
   lower_unit(e, NULL);

   vcode_unit_t v0 = find_unit("WORK.WAIT1.P1");
   vcode_select_unit(v0);

   const check_bb_t bb0[] = {
      { VCODE_OP_RETURN }
   };

   CHECK_BB(0);

   const check_bb_t bb1[] = {
      { VCODE_OP_TEMP_STACK_MARK },
      { VCODE_OP_CONST, .value = 2 },
      { VCODE_OP_FCALL, .func = "_std_standard_now" },
      { VCODE_OP_CONST, .value = 0 },
      { VCODE_OP_CMP,   .cmp = VCODE_CMP_EQ },
      { VCODE_OP_DEBUG_LOCUS },
      { VCODE_OP_ASSERT },
      { VCODE_OP_TEMP_STACK_RESTORE },
      { VCODE_OP_CONST, .value = 1000000 },
      { VCODE_OP_WAIT,  .target = 2 }
   };

   CHECK_BB(1);

   const check_bb_t bb2[] = {
      { VCODE_OP_TEMP_STACK_MARK },
      { VCODE_OP_CONST, .value = 2 },
      { VCODE_OP_FCALL, .func = "_std_standard_now" },
      { VCODE_OP_CONST, .value = 1000000 },
      { VCODE_OP_CMP,   .cmp = VCODE_CMP_EQ },
      { VCODE_OP_DEBUG_LOCUS },
      { VCODE_OP_ASSERT },
      { VCODE_OP_TEMP_STACK_RESTORE },
      { VCODE_OP_CONST, .value = 1 },
      { VCODE_OP_WAIT,  .target = 3 }
   };

   CHECK_BB(2);

   const check_bb_t bb3[] = {
      { VCODE_OP_TEMP_STACK_MARK },
      { VCODE_OP_CONST, .value = 2 },
      { VCODE_OP_FCALL, .func = "_std_standard_now" },
      { VCODE_OP_CONST, .value = 1000001 },
      { VCODE_OP_CMP,   .cmp = VCODE_CMP_EQ },
      { VCODE_OP_DEBUG_LOCUS },
      { VCODE_OP_ASSERT },
      { VCODE_OP_TEMP_STACK_RESTORE },
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
   lower_unit(e, NULL);

   vcode_unit_t v0 = find_unit("WORK.ASSIGN1.P1");
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
      { VCODE_OP_DEBUG_LOCUS },
      { VCODE_OP_ASSERT },
      { VCODE_OP_LOAD,  .name = "Y" },
      { VCODE_OP_CONST, .value = -4 },
      { VCODE_OP_CMP },
      { VCODE_OP_DEBUG_LOCUS },
      { VCODE_OP_ASSERT },
      { VCODE_OP_CONST, .value = 2 },
      { VCODE_OP_DEBUG_LOCUS },
      { VCODE_OP_TRAP_MUL },
      { VCODE_OP_CONST,  .value = -8 },
      { VCODE_OP_CMP },
      { VCODE_OP_DEBUG_LOCUS },
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
      { VCODE_OP_DEBUG_LOCUS },
      { VCODE_OP_TRAP_ADD },
      { VCODE_OP_CONST, .value = 12 },
      { VCODE_OP_CMP },
      { VCODE_OP_DEBUG_LOCUS },
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
   lower_unit(e, NULL);

   vcode_unit_t v0 = find_unit("WORK.ASSIGN2.P1");
   vcode_select_unit(v0);

   EXPECT_BB(0) = {
      { VCODE_OP_CONST, .value = 8 },
      { VCODE_OP_INDEX, .name = "X" },
      { VCODE_OP_CONST, .value = 1 },
      { VCODE_OP_CONST, .value = 0 },
      { VCODE_OP_CONST_ARRAY, .length = 8 },
      { VCODE_OP_ADDRESS_OF },
      { VCODE_OP_COPY },
      { VCODE_OP_CONST, .value = 3 },
      { VCODE_OP_INDEX, .name = "Y" },
      { VCODE_OP_CONST, .value = 1 },
      { VCODE_OP_CONST_REP, .value = 3 },
      { VCODE_OP_COPY },
      { VCODE_OP_RETURN }
   };

   CHECK_BB(0);

   fail_unless(vcode_get_op(4) == VCODE_OP_CONST_ARRAY);
   for (int i = 0; i < 8; i++)
      fail_unless(vcode_get_arg(4, i) == vcode_get_result((i == 6) ? 2 : 3));

   EXPECT_BB(1) = {
      { VCODE_OP_CONST, .value = 2 },
      { VCODE_OP_INDEX, .name = "X" },
      { VCODE_OP_CONST, .value = 0 },
      { VCODE_OP_CONST, .value = 7 },
      { VCODE_OP_ARRAY_REF },
      { VCODE_OP_LOAD_INDIRECT },
      { VCODE_OP_CONST, .value = 0 },
      { VCODE_OP_CMP },
      { VCODE_OP_DEBUG_LOCUS },
      { VCODE_OP_ASSERT },
      { VCODE_OP_CONST, .value = 3 },
      { VCODE_OP_ARRAY_REF },
      { VCODE_OP_LOAD_INDIRECT },
      { VCODE_OP_ARRAY_REF },
      { VCODE_OP_LOAD_INDIRECT },
      { VCODE_OP_CMP },
      { VCODE_OP_DEBUG_LOCUS },
      { VCODE_OP_ASSERT },
      { VCODE_OP_CONST, .value = 1 },
      { VCODE_OP_CONST, .value = 5 },
      { VCODE_OP_ARRAY_REF },
      { VCODE_OP_STORE_INDIRECT },
      { VCODE_OP_INDEX, .name = "Y" },
      { VCODE_OP_CONST, .value = 2 },
      { VCODE_OP_ARRAY_REF },
      { VCODE_OP_LOAD_INDIRECT },
      { VCODE_OP_ARRAY_REF },
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
   lower_unit(e, NULL);

   vcode_unit_t vc = find_unit("WORK.SIGNAL1");
   vcode_select_unit(vc);

   {
      EXPECT_BB(0) = {
         { VCODE_OP_CONST, .value = 5 },
         { VCODE_OP_CONST, .value = 0 },
         { VCODE_OP_CONST, .value = 4 },
         { VCODE_OP_CONST, .value = 1 },
         { VCODE_OP_DEBUG_LOCUS },
         { VCODE_OP_INIT_SIGNAL },
         { VCODE_OP_STORE, .name = "X" },
         { VCODE_OP_RETURN }
      };

      CHECK_BB(0);
   }

   fail_unless(vcode_count_vars() == 1);
   fail_unless(icmp(vcode_var_name(0), "X"));

   vcode_unit_t v0 = find_unit("WORK.SIGNAL1.P1");
   vcode_select_unit(v0);

   {
      EXPECT_BB(0) = {
         { VCODE_OP_VAR_UPREF, .hops = 1, .name = "X" },
         { VCODE_OP_LOAD_INDIRECT },
         { VCODE_OP_CONST, .value = 1 },
         { VCODE_OP_DRIVE_SIGNAL },
         { VCODE_OP_RETURN }
      };

      CHECK_BB(0);

      EXPECT_BB(1) = {
         { VCODE_OP_CONST, .value = 2 },
         { VCODE_OP_VAR_UPREF, .hops = 1, .name = "X" },
         { VCODE_OP_LOAD_INDIRECT },
         { VCODE_OP_RESOLVED },
         { VCODE_OP_LOAD_INDIRECT },
         { VCODE_OP_CONST, .value = 5 },
         { VCODE_OP_CMP, .cmp = VCODE_CMP_EQ },
         { VCODE_OP_DEBUG_LOCUS },
         { VCODE_OP_ASSERT },
         { VCODE_OP_CONST, .value = 0 },
         { VCODE_OP_LOAD_INDIRECT },
         { VCODE_OP_CONST, .value = 1 },
         { VCODE_OP_CONST, .value = 6 },
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
   lower_unit(e, NULL);

   vcode_unit_t v0 = find_unit("WORK.COND1.P1");
   vcode_select_unit(v0);

   EXPECT_BB(0) = {
      { VCODE_OP_CONST, .value = -2147483648 },
      { VCODE_OP_STORE, .name = "Y" },
      { VCODE_OP_RETURN }
   };

   CHECK_BB(0);

   EXPECT_BB(1) = {
      { VCODE_OP_VAR_UPREF, .hops = 1, .name = "X" },
      { VCODE_OP_LOAD_INDIRECT },
      { VCODE_OP_RESOLVED },
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
      { VCODE_OP_VAR_UPREF, .hops = 1, .name = "X" },
      { VCODE_OP_LOAD_INDIRECT },
      { VCODE_OP_RESOLVED },
      { VCODE_OP_LOAD_INDIRECT },
      { VCODE_OP_LOAD, .name = "Y" },
      { VCODE_OP_CONST, .value = 1 },
      { VCODE_OP_DEBUG_LOCUS },
      { VCODE_OP_TRAP_ADD },
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
      { VCODE_OP_ADDRESS_OF },
      { VCODE_OP_CONST, .value = 3 },
      { VCODE_OP_DEBUG_LOCUS },
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
   lower_unit(e, NULL);

   vcode_unit_t v0 = find_unit("WORK.ARITH1.P1");
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
      { VCODE_OP_DEBUG_LOCUS },
      { VCODE_OP_TRAP_ADD },
      { VCODE_OP_CONST, .value = 15 },
      { VCODE_OP_CMP, .cmp = VCODE_CMP_EQ },
      { VCODE_OP_DEBUG_LOCUS },
      { VCODE_OP_ASSERT },
      { VCODE_OP_DEBUG_LOCUS },
      { VCODE_OP_TRAP_SUB },
      { VCODE_OP_CONST, .value = -9 },
      { VCODE_OP_CMP, .cmp = VCODE_CMP_EQ },
      { VCODE_OP_DEBUG_LOCUS },
      { VCODE_OP_ASSERT },
      { VCODE_OP_DEBUG_LOCUS },
      { VCODE_OP_TRAP_MUL },
      { VCODE_OP_CONST, .value = 36 },
      { VCODE_OP_CMP, .cmp = VCODE_CMP_EQ },
      { VCODE_OP_DEBUG_LOCUS },
      { VCODE_OP_ASSERT },
      { VCODE_OP_CONST, .value = 12 },
      { VCODE_OP_DIV },
      { VCODE_OP_CONST, .value = 0 },
      { VCODE_OP_CMP, .cmp = VCODE_CMP_EQ },
      { VCODE_OP_DEBUG_LOCUS },
      { VCODE_OP_ASSERT },
      { VCODE_OP_CONST, .value = 3 },
      { VCODE_OP_CMP, .cmp = VCODE_CMP_EQ },
      { VCODE_OP_DEBUG_LOCUS },
      { VCODE_OP_ASSERT },
      { VCODE_OP_CMP, .cmp = VCODE_CMP_EQ },
      { VCODE_OP_DEBUG_LOCUS },
      { VCODE_OP_ASSERT },
      { VCODE_OP_CMP, .cmp = VCODE_CMP_NEQ },
      { VCODE_OP_DEBUG_LOCUS },
      { VCODE_OP_ASSERT },
      { VCODE_OP_CMP, .cmp = VCODE_CMP_LT },
      { VCODE_OP_DEBUG_LOCUS },
      { VCODE_OP_ASSERT },
      { VCODE_OP_CMP, .cmp = VCODE_CMP_GT },
      { VCODE_OP_DEBUG_LOCUS },
      { VCODE_OP_ASSERT },
      { VCODE_OP_CMP, .cmp = VCODE_CMP_LEQ },
      { VCODE_OP_DEBUG_LOCUS },
      { VCODE_OP_ASSERT },
      { VCODE_OP_CMP, .cmp = VCODE_CMP_GEQ },
      { VCODE_OP_DEBUG_LOCUS },
      { VCODE_OP_ASSERT },
      { VCODE_OP_NEG },
      { VCODE_OP_CONST, .value = -3 },
      { VCODE_OP_CMP, .cmp = VCODE_CMP_EQ },
      { VCODE_OP_DEBUG_LOCUS },
      { VCODE_OP_ASSERT },
      { VCODE_OP_DEBUG_LOCUS },
      { VCODE_OP_EXPONENT_CHECK },
      { VCODE_OP_EXP },
      { VCODE_OP_CONST, .value = 531441 },
      { VCODE_OP_CMP, .cmp = VCODE_CMP_EQ },
      { VCODE_OP_DEBUG_LOCUS },
      { VCODE_OP_ASSERT },
      { VCODE_OP_CONST, .value = -34 },
      { VCODE_OP_STORE, .name = "X" },
      { VCODE_OP_ABS },
      { VCODE_OP_CMP, .cmp = VCODE_CMP_EQ },
      { VCODE_OP_DEBUG_LOCUS },
      { VCODE_OP_ASSERT },
      { VCODE_OP_CONST, .value = 5 },
      { VCODE_OP_MOD },
      { VCODE_OP_CONST, .value = 2 },
      { VCODE_OP_CMP, .cmp = VCODE_CMP_EQ },
      { VCODE_OP_DEBUG_LOCUS },
      { VCODE_OP_ASSERT },
      { VCODE_OP_REM },
      { VCODE_OP_CMP, .cmp = VCODE_CMP_EQ },
      { VCODE_OP_DEBUG_LOCUS },
      { VCODE_OP_ASSERT },
      { VCODE_OP_CONST, .value = -5 },
      { VCODE_OP_REM },
      { VCODE_OP_CONST, .value = -2 },
      { VCODE_OP_CMP, .cmp = VCODE_CMP_EQ },
      { VCODE_OP_DEBUG_LOCUS },
      { VCODE_OP_ASSERT },
      { VCODE_OP_MOD },
      { VCODE_OP_CMP, .cmp = VCODE_CMP_EQ },
      { VCODE_OP_DEBUG_LOCUS },
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
      fail_if(error_count() > 0);

      simplify_local(t);

      if (tree_kind(t) == T_PACK_BODY)
         body = t;
   }

   fail_if(body == NULL);
   lower_unit(body, NULL);

   tree_t add1 = tree_decl(body, 0);
   fail_unless(tree_kind(add1) == T_FUNC_BODY);

   vcode_unit_t v0 = find_unit_for(add1);
   vcode_select_unit(v0);

   EXPECT_BB(0) = {
      { VCODE_OP_CONST, .value = 1 },
      { VCODE_OP_DEBUG_LOCUS },
      { VCODE_OP_TRAP_ADD },
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
   lower_unit(e, NULL);

   vcode_unit_t v0 = find_unit("WORK.FUNC1.P1");
   vcode_select_unit(v0);

   EXPECT_BB(0) = {
      { VCODE_OP_CONST, .value = INT32_MIN },
      { VCODE_OP_STORE, .name = "R" },
      { VCODE_OP_RETURN }
   };

   CHECK_BB(0);

   EXPECT_BB(1) = {
      { VCODE_OP_CONST, .value = 2 },
      { VCODE_OP_STORE, .name = "R" },
      { VCODE_OP_TEMP_STACK_MARK },
      { VCODE_OP_CONTEXT_UPREF, .hops = 1 },
      { VCODE_OP_FCALL, .func = "WORK.FUNC1.ADD1(I)I" },
      { VCODE_OP_STORE, .name = "R" },
      { VCODE_OP_TEMP_STACK_RESTORE },
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
   lower_unit(e, NULL);
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
   lower_unit(e, NULL);

   vcode_unit_t v0 = find_unit("WORK.ARRAYOP1.P1");
   vcode_select_unit(v0);

   EXPECT_BB(0) = {
      { VCODE_OP_CONST, .value = 3 },
      { VCODE_OP_INDEX, .name = "X" },
      { VCODE_OP_CONST, .value = 0 },
      { VCODE_OP_CONST_REP, .value = 3 },
      { VCODE_OP_COPY },
      { VCODE_OP_RETURN }
   };

   CHECK_BB(0);

   EXPECT_BB(1) = {
      { VCODE_OP_TEMP_STACK_MARK },
      { VCODE_OP_CONST, .value = 2 },
      { VCODE_OP_LINK_PACKAGE, .name = "STD.STANDARD" },
      { VCODE_OP_INDEX, .name = "X" },
      { VCODE_OP_CONST, .value = 1 },
      { VCODE_OP_CONST, .value = 3 },
      { VCODE_OP_CONST, .value = 0 },
      { VCODE_OP_WRAP },
      { VCODE_OP_CONST_ARRAY, .length = 3 },
      { VCODE_OP_ADDRESS_OF },
      { VCODE_OP_CONST, .value = 0 },
      { VCODE_OP_CONST, .value = 2 },
      { VCODE_OP_WRAP },
      { VCODE_OP_FCALL, .func = "STD.STANDARD.\"<\"(QQ)B" },
      { VCODE_OP_DEBUG_LOCUS },
      { VCODE_OP_ASSERT },
      { VCODE_OP_TEMP_STACK_RESTORE },
      { VCODE_OP_WAIT, .target = 2 }
   };

   CHECK_BB(1);
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
   lower_unit(e, NULL);

   vcode_unit_t v0 = find_unit("WORK.ARRAY1.P1");
   vcode_select_unit(v0);

   EXPECT_BB(1) = {
      { VCODE_OP_TEMP_STACK_MARK },
      { VCODE_OP_CONST, .value = 2 },
      { VCODE_OP_LINK_PACKAGE, .name = "STD.STANDARD" },
      { VCODE_OP_CONTEXT_UPREF, .hops = 1 },
      { VCODE_OP_FCALL, .func = "WORK.ARRAY1.FUNC()Q" },
      { VCODE_OP_CONST, .value = 1 },
      { VCODE_OP_CONST, .value = 0 },
      { VCODE_OP_CONST_ARRAY, .length = 2 },
      { VCODE_OP_ADDRESS_OF },
      { VCODE_OP_CONST, .value = 0 },
      { VCODE_OP_CONST, .value = 1 },
      { VCODE_OP_WRAP },
      { VCODE_OP_FCALL, .func = "STD.STANDARD.\"=\"(QQ)B" },
      { VCODE_OP_DEBUG_LOCUS },
      { VCODE_OP_ASSERT },
      { VCODE_OP_TEMP_STACK_RESTORE },
      { VCODE_OP_WAIT, .target = 2 },
   };

   CHECK_BB(1);
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
   lower_unit(e, NULL);

   tree_t p = tree_stmt(tree_stmt(e, 0), 0);
   fail_unless(tree_kind(p) == T_PROCESS);

   {
      vcode_unit_t v0 = find_unit("WORK.NEST1.LINE_7");
      vcode_select_unit(v0);

      EXPECT_BB(1) = {
         { VCODE_OP_TEMP_STACK_MARK },
         { VCODE_OP_CONST, .value = 2 },
         { VCODE_OP_CONTEXT_UPREF, .hops = 0 },
         { VCODE_OP_CONST, .value = 5 },
         { VCODE_OP_FCALL, .func = "WORK.NEST1.LINE_7.ADD_TO_X(I)I" },
         { VCODE_OP_CONST, .value = 7 },
         { VCODE_OP_CMP, .cmp = VCODE_CMP_EQ },
         { VCODE_OP_DEBUG_LOCUS },
         { VCODE_OP_ASSERT },
         { VCODE_OP_TEMP_STACK_RESTORE },
         { VCODE_OP_WAIT, .target = 2 }
      };

      CHECK_BB(1);
   }

   tree_t f1 = tree_decl(p, 2);
   fail_unless(tree_kind(f1) == T_FUNC_BODY);

   {
      vcode_unit_t v0 = find_unit_for(f1);
      vcode_select_unit(v0);

      fail_unless(icmp(vcode_unit_name(), "WORK.NEST1.LINE_7.ADD_TO_X(I)I"));

      EXPECT_BB(0) = {
         { VCODE_OP_STORE, .name = "Y" },
         { VCODE_OP_CONTEXT_UPREF, .hops = 0 },
         { VCODE_OP_FCALL, .func = "WORK.NEST1.LINE_7.ADD_TO_X(I)I.DO_IT()I" },
         { VCODE_OP_RETURN }
      };

      CHECK_BB(0);
   }

   tree_t f2 = tree_decl(f1, 0);
   fail_unless(tree_kind(f2) == T_FUNC_BODY);

   {
      vcode_unit_t v0 = find_unit_for(f2);
      vcode_select_unit(v0);

      fail_unless(icmp(vcode_unit_name(),
                       "WORK.NEST1.LINE_7.ADD_TO_X(I)I.DO_IT()I"));

      EXPECT_BB(0) = {
         { VCODE_OP_VAR_UPREF, .hops = 2, .name = "X" },
         { VCODE_OP_LOAD_INDIRECT },
         { VCODE_OP_VAR_UPREF, .hops = 1, .name = "Y" },
         { VCODE_OP_LOAD_INDIRECT },
         { VCODE_OP_DEBUG_LOCUS },
         { VCODE_OP_TRAP_ADD },
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
   lower_unit(e, NULL);

   vcode_unit_t v0 = find_unit("WORK.SIGNAL2.P1");
   vcode_select_unit(v0);

   EXPECT_BB(0) = {
      { VCODE_OP_RETURN }
   };

   CHECK_BB(0);

   EXPECT_BB(1) = {
      { VCODE_OP_CONST, .value = 2 },
      { VCODE_OP_VAR_UPREF, .name = "X", .hops = 1 },
      { VCODE_OP_LOAD_INDIRECT },
      { VCODE_OP_CONST, .value = 1 },
      { VCODE_OP_EVENT },
      { VCODE_OP_DEBUG_LOCUS },
      { VCODE_OP_ASSERT },
      { VCODE_OP_LOAD_INDIRECT },
      { VCODE_OP_ACTIVE },
      { VCODE_OP_DEBUG_LOCUS },
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
   lower_unit(e, NULL);

   vcode_unit_t v0 = find_unit("WORK.ATTR1.P1");
   vcode_select_unit(v0);

   EXPECT_BB(1) = {
      { VCODE_OP_CONST, .value = 2 },
      { VCODE_OP_LOAD, .name = "X" },
      { VCODE_OP_CONST, .value = 1 },
      { VCODE_OP_ADD },
      { VCODE_OP_CMP, .cmp = VCODE_CMP_EQ },
      { VCODE_OP_DEBUG_LOCUS },
      { VCODE_OP_ASSERT },
      { VCODE_OP_SUB },
      { VCODE_OP_CONST, .value = -1 },
      { VCODE_OP_CMP, .cmp = VCODE_CMP_EQ },
      { VCODE_OP_DEBUG_LOCUS },
      { VCODE_OP_ASSERT },
      { VCODE_OP_LOAD, .name = "Z" },
      { VCODE_OP_ADD },
      { VCODE_OP_CONST, .value = 0 },
      { VCODE_OP_CMP },
      { VCODE_OP_DEBUG_LOCUS },
      { VCODE_OP_ASSERT },
      { VCODE_OP_ADD },
      { VCODE_OP_CONST, .value = 2 },
      { VCODE_OP_CMP, .cmp = VCODE_CMP_EQ },
      { VCODE_OP_DEBUG_LOCUS },
      { VCODE_OP_ASSERT },
      { VCODE_OP_LOAD, .name = "Y" },
      { VCODE_OP_CONST, .value = 1 },
      { VCODE_OP_ADD },
      { VCODE_OP_CONST, .value = 2 },
      { VCODE_OP_CMP, .cmp = VCODE_CMP_EQ },
      { VCODE_OP_DEBUG_LOCUS },
      { VCODE_OP_ASSERT },
      { VCODE_OP_CONST, .value = -1 },
      { VCODE_OP_ADD },
      { VCODE_OP_CONST, .value = 0 },
      { VCODE_OP_CMP, .cmp = VCODE_CMP_EQ },
      { VCODE_OP_DEBUG_LOCUS },
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
   lower_unit(e, NULL);

   vcode_unit_t v0 = find_unit("WORK.ASSIGN3.P1");
   vcode_select_unit(v0);

   EXPECT_BB(1) = {
      { VCODE_OP_INDEX, .name = "X" },
      { VCODE_OP_CONST, .value = 8 },
      { VCODE_OP_INDEX, .name = "Y" },
      { VCODE_OP_COPY },
      { VCODE_OP_TEMP_STACK_MARK },
      { VCODE_OP_CONST, .value = 2 },
      { VCODE_OP_LINK_PACKAGE, .name = "STD.STANDARD" },
      { VCODE_OP_CONST, .value = 7 },
      { VCODE_OP_CONST, .value = 0 },
      { VCODE_OP_CONST, .value = 1 },
      { VCODE_OP_WRAP },
      { VCODE_OP_WRAP },
      { VCODE_OP_FCALL, .func = "STD.STANDARD.\"/=\"(QQ)B" },
      { VCODE_OP_DEBUG_LOCUS },
      { VCODE_OP_ASSERT },
      { VCODE_OP_TEMP_STACK_RESTORE },
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
   lower_unit(e, NULL);

   vcode_unit_t v0 = find_unit("WORK.RECORD1.P1");
   vcode_select_unit(v0);

   EXPECT_BB(0) = {
      { VCODE_OP_INDEX, .name = "A" },
      { VCODE_OP_CONST, .value = 1 },
      { VCODE_OP_CONST, .value = 2 },
      { VCODE_OP_CONST_RECORD },
      { VCODE_OP_ADDRESS_OF },
      { VCODE_OP_COPY },
      { VCODE_OP_INDEX, .name = "B" },
      { VCODE_OP_COPY },
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
      { VCODE_OP_DEBUG_LOCUS },
      { VCODE_OP_ASSERT },
      { VCODE_OP_CONST, .value = 5 },
      { VCODE_OP_STORE_INDIRECT },
      { VCODE_OP_INDEX, .name = "B" },
      { VCODE_OP_COPY },
      { VCODE_OP_LOAD_INDIRECT },
      { VCODE_OP_CMP, .cmp = VCODE_CMP_EQ },
      { VCODE_OP_DEBUG_LOCUS },
      { VCODE_OP_ASSERT },
      { VCODE_OP_TEMP_STACK_MARK },
      { VCODE_OP_CONTEXT_UPREF, .hops = 1 },
      { VCODE_OP_FCALL, .func = "*WORK.RECORD1-TEST.\"=\"(" },
      { VCODE_OP_DEBUG_LOCUS },
      { VCODE_OP_ASSERT },
      { VCODE_OP_TEMP_STACK_RESTORE },
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
   lower_unit(e, NULL);

   vcode_unit_t v0 = find_unit("WORK.SIGNAL4.P1");
   vcode_select_unit(v0);

   EXPECT_BB(1) = {
      { VCODE_OP_VAR_UPREF, .hops = 1, .name = "S" },
      { VCODE_OP_LOAD_INDIRECT },
      { VCODE_OP_RESOLVED },
      { VCODE_OP_CONST, .value = 0 },
      { VCODE_OP_CONST, .value = 1 },
      { VCODE_OP_ARRAY_REF },
      { VCODE_OP_LOAD_INDIRECT },
      { VCODE_OP_INDEX, .name = "V" },
      { VCODE_OP_ARRAY_REF },
      { VCODE_OP_STORE_INDIRECT },
      { VCODE_OP_CONST, .value = 0 },
      { VCODE_OP_LOAD_INDIRECT },
      { VCODE_OP_CONST, .value = 4 },
      { VCODE_OP_SCHED_WAVEFORM },
      { VCODE_OP_LOAD_INDIRECT },
      { VCODE_OP_RESOLVED },
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
   lower_unit(e, NULL);

   vcode_unit_t v0 = find_unit("WORK.STATICWAIT.P1");
   vcode_select_unit(v0);

   EXPECT_BB(0) = {
      { VCODE_OP_VAR_UPREF, .name = "X", .hops = 1 },
      { VCODE_OP_LOAD_INDIRECT },
      { VCODE_OP_CONST, .value = 1 },
      { VCODE_OP_DRIVE_SIGNAL },
      { VCODE_OP_LOAD_INDIRECT },
      { VCODE_OP_SCHED_STATIC },
      { VCODE_OP_RETURN }
   };

   CHECK_BB(0);

   EXPECT_BB(1) = {
      { VCODE_OP_CONST, .value = 0 },
      { VCODE_OP_VAR_UPREF, .name = "X", .hops = 1 },
      { VCODE_OP_LOAD_INDIRECT },
      { VCODE_OP_CONST, .value = 1 },
      { VCODE_OP_CONST, .value = 0 },
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
   lower_unit(e, NULL);

   {
      vcode_unit_t v0 = find_unit("WORK.PROC1.P1");
      vcode_select_unit(v0);

      EXPECT_BB(1) = {
         { VCODE_OP_TEMP_STACK_MARK },
         { VCODE_OP_CONTEXT_UPREF, .hops = 1 },
         { VCODE_OP_LOAD, .name = "A" },
         { VCODE_OP_INDEX, .name = "B" },
         { VCODE_OP_FCALL, .func = "WORK.PROC1.ADD1(II)" },
         { VCODE_OP_TEMP_STACK_RESTORE },
         { VCODE_OP_CONST, .value = 2 },
         { VCODE_OP_LOAD, .name = "B" },
         { VCODE_OP_CONST, .value = 3 },
         { VCODE_OP_CMP, .cmp = VCODE_CMP_EQ },
         { VCODE_OP_DEBUG_LOCUS },
         { VCODE_OP_ASSERT },
         { VCODE_OP_TEMP_STACK_MARK },
         { VCODE_OP_CONST, .value = 5 },
         { VCODE_OP_FCALL, .func = "WORK.PROC1.ADD1(II)" },
         { VCODE_OP_TEMP_STACK_RESTORE },
         { VCODE_OP_LOAD, .name = "B" },
         { VCODE_OP_CONST, .value = 6 },
         { VCODE_OP_CMP, .cmp = VCODE_CMP_EQ },
         { VCODE_OP_DEBUG_LOCUS },
         { VCODE_OP_ASSERT },
         { VCODE_OP_WAIT, .target = 2 }
      };

      CHECK_BB(1);
   }

   {
      vcode_unit_t v0 = find_unit_for(tree_decl(tree_stmt(e, 0), 1));
      vcode_select_unit(v0);

      EXPECT_BB(0) = {
         { VCODE_OP_CONST, .value = 1 },
         { VCODE_OP_DEBUG_LOCUS },
         { VCODE_OP_TRAP_ADD },
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

   tree_t e = run_elab();
   lower_unit(e, NULL);

   vcode_unit_t v0 = find_unit("WORK.WHILE1.P1");
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
      { VCODE_OP_CONST, .value = 1 },
      { VCODE_OP_DEBUG_LOCUS },
      { VCODE_OP_TRAP_SUB },
      { VCODE_OP_STORE, .name = "N" },
      { VCODE_OP_JUMP, .target = 2 }
   };

   CHECK_BB(3);

   fail_if_errors();
}
END_TEST

START_TEST(test_loop1)
{
   input_from_file(TESTDIR "/lower/loop1.vhd");

   tree_t e = run_elab();
   lower_unit(e, NULL);

   vcode_unit_t v0 = find_unit("WORK.LOOP1.P1");
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
      { VCODE_OP_CONST, .value = 1 },
      { VCODE_OP_DEBUG_LOCUS },
      { VCODE_OP_TRAP_ADD },
      { VCODE_OP_STORE, .name = "A" },
      { VCODE_OP_CONST, .value = 2 },
      { VCODE_OP_MOD },
      { VCODE_OP_CONST, .value = 0 },
      { VCODE_OP_CMP, .cmp = VCODE_CMP_EQ },
      { VCODE_OP_COND, .target = 11, .target_else = 10 }
   };

   CHECK_BB(8);

   fail_if_errors();
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
   lower_unit(e, NULL);

   {
      vcode_unit_t v0 = find_unit_for(tree_decl(tree_stmt(e, 0), 1));
      vcode_select_unit(v0);

      EXPECT_BB(0) = {
         { VCODE_OP_STORE, .name = "X" },
         { VCODE_OP_CONST, .value = 10000000 },
         { VCODE_OP_WAIT, .delay = true, .target = 1 }
      };

      CHECK_BB(0);

      EXPECT_BB(1) = {
         { VCODE_OP_CONST, .value = 1 },
         { VCODE_OP_LOAD, .name = "X" },
         { VCODE_OP_STORE_INDIRECT },
         { VCODE_OP_CONST, .value = 5000000 },
         { VCODE_OP_WAIT, .delay = true, .target = 2 }
      };

      CHECK_BB(1);
   }

   {
      vcode_unit_t v0 = find_unit("WORK.PROC3.P2");
      vcode_select_unit(v0);

      EXPECT_BB(1) = {
         { VCODE_OP_TEMP_STACK_MARK },
         { VCODE_OP_CONTEXT_UPREF, .hops = 1 },
         { VCODE_OP_INDEX, .name = "X" },
         { VCODE_OP_STORE, .name = "tmp_mark" },
         { VCODE_OP_PCALL, .func = "WORK.PROC3.P1(I)", .target = 2 }
      };

      CHECK_BB(1);

      EXPECT_BB(2) = {
         { VCODE_OP_RESUME, .func = "WORK.PROC3.P1(I)" },
         { VCODE_OP_LOAD, .name = "tmp_mark" },
         { VCODE_OP_TEMP_STACK_RESTORE },
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
   lower_unit(e, NULL);

   vcode_unit_t v0 = find_unit_for(tree_decl(tree_stmt(e, 0), 1));
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

   tree_t e = run_elab();
   lower_unit(e, NULL);

   vcode_unit_t v0 = find_unit("WORK.SLICE1.P1");
   vcode_select_unit(v0);

   EXPECT_BB(1) = {
      { VCODE_OP_INDEX, .name = "V" },
      { VCODE_OP_CONST, .value = 4 },
      { VCODE_OP_CONST, .value = 1 },
      { VCODE_OP_CONST, .value = 2 },
      { VCODE_OP_CONST, .value = 3 },
      { VCODE_OP_CONST, .value = 4 },
      { VCODE_OP_CONST_ARRAY, .length = 4 },
      { VCODE_OP_ADDRESS_OF },
      { VCODE_OP_COPY },
      { VCODE_OP_CONST, .value = 0 },
      { VCODE_OP_CONST, .value = 1 },
      { VCODE_OP_ARRAY_REF },
      { VCODE_OP_CONST, .value = 2 },
      { VCODE_OP_CONST, .value = 6 },
      { VCODE_OP_CONST, .value = 7 },
      { VCODE_OP_CONST_ARRAY, .length = 2 },
      { VCODE_OP_ADDRESS_OF },
      { VCODE_OP_COPY },
      { VCODE_OP_TEMP_STACK_MARK },
      { VCODE_OP_CONST, .value = 2 },
      { VCODE_OP_CONTEXT_UPREF, .hops = 1 },
      { VCODE_OP_ARRAY_REF },
      { VCODE_OP_WRAP },
      { VCODE_OP_CONST_ARRAY, .length = 2 },
      { VCODE_OP_ADDRESS_OF },
      { VCODE_OP_CONST, .value = -2147483648 },
      { VCODE_OP_CONST, .value = -2147483647 },
      { VCODE_OP_WRAP },
      { VCODE_OP_FCALL, .func="*WORK.SLICE1-TEST.\"=\"" },
      { VCODE_OP_DEBUG_LOCUS },
      { VCODE_OP_ASSERT },
      { VCODE_OP_TEMP_STACK_RESTORE },
      { VCODE_OP_CONST, .value = 1000000 },
      { VCODE_OP_WAIT, .target = 2 },
   };

   CHECK_BB(1);

   fail_if_errors();
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
   lower_unit(e, NULL);

   vcode_unit_t v0 = find_unit_for(tree_decl(tree_stmt(e, 0), 1));
   vcode_select_unit(v0);

   fail_unless(vcode_count_blocks() == 3);
}
END_TEST

START_TEST(test_memset)
{
   input_from_file(TESTDIR "/lower/memset.vhd");

   tree_t e = run_elab();
   lower_unit(e, NULL);

   tree_t b0 = tree_stmt(e, 0);

   {
      tree_t f = search_decls(b0, ident_new("FOO"), 0);
      fail_if(f == NULL);

      vcode_unit_t v0 = find_unit_for(f);
      vcode_select_unit(v0);

      EXPECT_BB(0) = {
         { VCODE_OP_CONST, .value = 1 },
         { VCODE_OP_CONST, .value = 0 },
         { VCODE_OP_RANGE_LENGTH },
         { VCODE_OP_ALLOCA, .subkind = VCODE_ALLOCA_HEAP },
         { VCODE_OP_WRAP },
         { VCODE_OP_STORE, .name = "V" },
         { VCODE_OP_MEMSET },
         { VCODE_OP_RETURN }
      };

      CHECK_BB(0);
   }

   {
      tree_t f = search_decls(b0, ident_new("BAR"), 0);
      fail_if(f == NULL);

      vcode_unit_t v0 = find_unit_for(f);
      vcode_select_unit(v0);

      EXPECT_BB(0) = {
         { VCODE_OP_CONST, .value = 1 },
         { VCODE_OP_CONST, .value = 0 },
         { VCODE_OP_RANGE_LENGTH },
         { VCODE_OP_ALLOCA, .subkind = VCODE_ALLOCA_HEAP },
         { VCODE_OP_WRAP },
         { VCODE_OP_STORE, .name = "V" },
         { VCODE_OP_CONST, .value = 2880154539 },
         { VCODE_OP_MEMSET },
         { VCODE_OP_DEBUG_LOCUS },
         { VCODE_OP_UARRAY_LEN },
         { VCODE_OP_LENGTH_CHECK },
         { VCODE_OP_RETURN }
      };

      CHECK_BB(0);
   }

   fail_if_errors();
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
   lower_unit(e, NULL);

   {
      vcode_unit_t v0 = find_unit_for(tree_decl(tree_stmt(e, 0), 1));
      vcode_select_unit(v0);

      EXPECT_BB(0) = {
         { VCODE_OP_RESOLVED },
         { VCODE_OP_LOAD_INDIRECT },
         { VCODE_OP_CONST, .value = 1 },
         { VCODE_OP_DEBUG_LOCUS },
         { VCODE_OP_TRAP_ADD },
         { VCODE_OP_RETURN }
      };

      CHECK_BB(0);
   }

   {
      vcode_unit_t v0 = find_unit_for(tree_decl(tree_stmt(e, 0), 2));
      vcode_select_unit(v0);

      EXPECT_BB(0) = {
         { VCODE_OP_CONST, .value = 1 },
         { VCODE_OP_EVENT },
         { VCODE_OP_RETURN }
      };

      CHECK_BB(0);
   }

   {
      vcode_unit_t v0 = find_unit("WORK.FUNC5.P1");
      vcode_select_unit(v0);

      EXPECT_BB(1) = {
         { VCODE_OP_TEMP_STACK_MARK },
         { VCODE_OP_CONST, .value = 2 },
         { VCODE_OP_CONTEXT_UPREF, .hops = 1 },
         { VCODE_OP_VAR_UPREF, .name = "X", .hops = 1 },
         { VCODE_OP_LOAD_INDIRECT },
         { VCODE_OP_FCALL, .func = "WORK.FUNC5.ADD_ONE_S(sI)I" },
         { VCODE_OP_CONST, .value = 6 },
         { VCODE_OP_CMP, .cmp = VCODE_CMP_EQ },
         { VCODE_OP_DEBUG_LOCUS },
         { VCODE_OP_ASSERT },
         { VCODE_OP_TEMP_STACK_RESTORE },
         { VCODE_OP_TEMP_STACK_MARK },
         { VCODE_OP_LOAD_INDIRECT },
         { VCODE_OP_FCALL, .func = "WORK.FUNC5.EVENT(sI)B" },
         { VCODE_OP_DEBUG_LOCUS },
         { VCODE_OP_ASSERT },
         { VCODE_OP_TEMP_STACK_RESTORE },
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
   lower_unit(e, NULL);

   vcode_unit_t v0 = find_unit("WORK.BOUNDS1.P1");
   vcode_select_unit(v0);

   EXPECT_BB(1) = {
      { VCODE_OP_CONST, .value = 2 },
      { VCODE_OP_INDEX, .name = "V" },
      { VCODE_OP_LOAD,  .name = "K" },
      { VCODE_OP_CONST, .value = 0 },
      { VCODE_OP_CONST, .value = 9 },
      { VCODE_OP_CONST, .value = 0 },
      { VCODE_OP_CAST },
      { VCODE_OP_ARRAY_REF },
      { VCODE_OP_LOAD_INDIRECT },
      { VCODE_OP_CONST, .value = 1 },
      { VCODE_OP_CMP, .cmp = VCODE_CMP_EQ },
      { VCODE_OP_DEBUG_LOCUS },
      { VCODE_OP_ASSERT },
      { VCODE_OP_DEBUG_LOCUS },
      { VCODE_OP_ADD },
      { VCODE_OP_INDEX_CHECK },
      { VCODE_OP_CAST },
      { VCODE_OP_ARRAY_REF },
      { VCODE_OP_LOAD_INDIRECT },
      { VCODE_OP_CMP, .cmp = VCODE_CMP_EQ },
      { VCODE_OP_DEBUG_LOCUS },
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
   lower_unit(e, NULL);

   tree_t f = search_decls(tree_stmt(e, 0), ident_new("MAKE_REC"), 0);
   fail_if(f == NULL);
   fail_unless(tree_kind(f) == T_FUNC_BODY);

   vcode_unit_t v0 = find_unit_for(f);
   vcode_select_unit(v0);

   fail_unless(vcode_var_flags(0) & VAR_HEAP);

   EXPECT_BB(0) = {
      { VCODE_OP_INDEX, .name = "R" },
      { VCODE_OP_CONST, .value = 0 },
      { VCODE_OP_CONST_ARRAY, .length = 3 },
      { VCODE_OP_CONST, .value = INT32_MIN },
      { VCODE_OP_CONST_RECORD },
      { VCODE_OP_ADDRESS_OF },
      { VCODE_OP_COPY },
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
   lower_unit(e, NULL);

   vcode_unit_t v0 = find_unit_for(tree_decl(tree_stmt(e, 0), 1));
   vcode_select_unit(v0);

   EXPECT_BB(0) = {
      { VCODE_OP_STORE, .name = "X" },
      { VCODE_OP_UARRAY_LEFT },
      { VCODE_OP_CAST },
      { VCODE_OP_UARRAY_RIGHT },
      { VCODE_OP_CAST },
      { VCODE_OP_UARRAY_DIR },
      { VCODE_OP_RANGE_LENGTH },
      { VCODE_OP_ALLOCA, .subkind = 1 },
      { VCODE_OP_WRAP },
      { VCODE_OP_STORE, .name = "Y" },
      { VCODE_OP_CONST, .value = 0 },
      { VCODE_OP_MEMSET },
      { VCODE_OP_RANGE_NULL },
      { VCODE_OP_COND, .target = 2, .target_else = 1 },
   };

   CHECK_BB(0);

   // This block is pointless as both index checks get optimised out
   EXPECT_BB(1) = {
      { VCODE_OP_JUMP, .target = 2 },
   };

   CHECK_BB(1);

   EXPECT_BB(2) = {
      { VCODE_OP_CONST, .value = 1000000 },
      { VCODE_OP_WAIT, .target = 3 },
   };

   CHECK_BB(2);
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
   lower_unit(e, NULL);

   vcode_unit_t v0 = find_unit("WORK.ISSUE116.P1");
   vcode_select_unit(v0);

   EXPECT_BB(0) = {
      { VCODE_OP_VAR_UPREF, .name = "INTSTAT", .hops = 1 },
      { VCODE_OP_LOAD_INDIRECT },
      { VCODE_OP_CONST, .value = 0 },
      { VCODE_OP_CONST, .value = 1 },
      { VCODE_OP_ARRAY_REF },
      { VCODE_OP_DRIVE_SIGNAL },
      { VCODE_OP_LOAD_INDIRECT },
      { VCODE_OP_ARRAY_REF },
      { VCODE_OP_CONST, .value = 7 },
      { VCODE_OP_SCHED_STATIC },
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
   lower_unit(e, NULL);

   vcode_unit_t v0 = find_unit_for(tree_decl(tree_stmt(e, 0), 1));
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

   tree_t e = run_elab();

   cover_tagging_t *tagging = cover_tag(e);
   lower_unit(e, tagging);

   vcode_unit_t v0 = find_unit("WORK.COVER.P1");
   vcode_select_unit(v0);

   EXPECT_BB(1) = {
      { VCODE_OP_COVER_STMT, .tag = 0 },
      { VCODE_OP_CONST, .value = 1 },
      { VCODE_OP_STORE, .name = "V" },
      { VCODE_OP_COVER_STMT, .tag = 2 },
      { VCODE_OP_VAR_UPREF, .hops = 1, .name = "S" },
      { VCODE_OP_LOAD_INDIRECT },
      { VCODE_OP_RESOLVED },
      { VCODE_OP_LOAD_INDIRECT },
      { VCODE_OP_CMP, .cmp = VCODE_CMP_EQ },
      { VCODE_OP_COVER_COND, .tag = 0, .subkind = 1 },
      { VCODE_OP_LOAD_INDIRECT },
      { VCODE_OP_RESOLVED },
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
   lower_unit(e, NULL);

   vcode_unit_t v0 = find_unit_for(tree_decl(tree_stmt(e, 0), 1));
   vcode_select_unit(v0);

   EXPECT_BB(0) = {
      { VCODE_OP_STORE, .name = "X" },
      { VCODE_OP_CONTEXT_UPREF, .hops = 0 },
      { VCODE_OP_FCALL, .func = "WORK.ISSUE122.FUNC(I)I.NESTED()I" },
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
   lower_unit(e, NULL);

   vcode_unit_t v0 = find_unit_for(tree_decl(tree_stmt(e, 0), 1));
   vcode_select_unit(v0);

   EXPECT_BB(0) = {
      { VCODE_OP_LINK_PACKAGE, .name = "STD.STANDARD" },
      { VCODE_OP_LINK_PACKAGE, .name = "WORK.PACK" },
      { VCODE_OP_FCALL, .func = "WORK.PACK.TO_INTEGER(18WORK.PACK.UNSIGNED)I" },
      { VCODE_OP_FCALL, .func = "STD.STANDARD.INTEGER$image" },
      { VCODE_OP_RETURN }
   };

   CHECK_BB(0);
}
END_TEST

START_TEST(test_issue135)
{
   input_from_file(TESTDIR "/lower/issue135.vhd");

   tree_t e = run_elab();
   lower_unit(e, NULL);

   vcode_unit_t v0 = find_unit_for(tree_decl(tree_stmt(e, 0), 1));
   vcode_select_unit(v0);

   EXPECT_BB(0) = {
      { VCODE_OP_LINK_PACKAGE, .name = "STD.STANDARD" },
      { VCODE_OP_FCALL, .func = "STD.STANDARD.INTEGER$image" },
      { VCODE_OP_FCALL, .func = "STD.STANDARD.TIME$image" },
      { VCODE_OP_CONST, .value = 0 },
      { VCODE_OP_CONST, .value = -1 },
      { VCODE_OP_CONST, .value = 0 },
      { VCODE_OP_CONST, .value = 1 },
      { VCODE_OP_UARRAY_LEN },
      { VCODE_OP_ADD },
      { VCODE_OP_UARRAY_LEN },
      { VCODE_OP_ADD },
      { VCODE_OP_ADD },
      { VCODE_OP_CONST, .value = 1 },
      { VCODE_OP_ADD },
      { VCODE_OP_ADD },
      { VCODE_OP_UARRAY_LEN },
      { VCODE_OP_ADD },
      { VCODE_OP_ADD },
      { VCODE_OP_ADD },
      { VCODE_OP_ADD },
      { VCODE_OP_ALLOCA, .subkind = VCODE_ALLOCA_HEAP },
      { VCODE_OP_CAST },
      { VCODE_OP_ADD },
      { VCODE_OP_WRAP },
      { VCODE_OP_ARRAY_REF },
      { VCODE_OP_UNWRAP },
      { VCODE_OP_COPY },
      { VCODE_OP_ARRAY_REF },
      { VCODE_OP_UNWRAP },
      { VCODE_OP_COPY },
      { VCODE_OP_ARRAY_REF },
      { VCODE_OP_STORE_INDIRECT },
      { VCODE_OP_ARRAY_REF },
      { VCODE_OP_UNWRAP },
      { VCODE_OP_COPY },
      { VCODE_OP_ARRAY_REF },
      { VCODE_OP_STORE_INDIRECT },
      { VCODE_OP_RETURN }
   };

   CHECK_BB(0);
}
END_TEST

START_TEST(test_issue134)
{
   input_from_file(TESTDIR "/lower/issue134.vhd");

   tree_t e = run_elab();
   lower_unit(e, NULL);

   vcode_unit_t v0 = find_unit_for(tree_decl(tree_stmt(e, 0), 1));
   vcode_select_unit(v0);

   EXPECT_BB(0) = {
      { VCODE_OP_CONST_ARRAY, .length = 0 },
      { VCODE_OP_ADDRESS_OF },
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
   lower_unit(e, NULL);

   tree_t body = search_decls(tree_stmt(e, 0),
                              ident_new("RECORD_RETURNER_T"), 1);
   fail_if(body == NULL);
   fail_unless(tree_kind(body) == T_PROT_BODY);

   vcode_unit_t v0 = find_unit_for(tree_decl(body, 1));
   vcode_select_unit(v0);

   EXPECT_BB(0) = {
      { VCODE_OP_VAR_UPREF, .name = "REC", .hops = 1 },
      { VCODE_OP_RETURN }
   };

   CHECK_BB(0);
}
END_TEST

START_TEST(test_issue125)
{
   input_from_file(TESTDIR "/lower/issue125.vhd");

   tree_t e = run_elab();
   lower_unit(e, NULL);
}
END_TEST

START_TEST(test_access_bug)
{
   input_from_file(TESTDIR "/lower/access_bug.vhd");

   tree_t e = run_elab();
   lower_unit(e, NULL);
}
END_TEST

START_TEST(test_rectype)
{
   input_from_file(TESTDIR "/lower/rectype.vhd");

   tree_t e = run_elab();
   lower_unit(e, NULL);

   vcode_unit_t v0 = find_unit("WORK.E.P1");
   vcode_select_unit(v0);

   fail_unless(vtype_kind(0) == VCODE_TYPE_RECORD);
   fail_unless(vtype_kind(1) == VCODE_TYPE_RECORD);

   // We used to mangle this with @<address>
   ident_t r2_name = vtype_name(0);
   fail_unless(strncmp(istr(r2_name), "WORK.E(A).R2", 3) == 0);

   ident_t r1_name = vtype_name(1);
   fail_unless(icmp(r1_name, "WORK.RECTYPE.R1$"));
}
END_TEST

START_TEST(test_issue149)
{
   input_from_file(TESTDIR "/lower/issue149.vhd");

   tree_t e = run_elab();
   lower_unit(e, NULL);

   vcode_unit_t v0 = find_unit_for(tree_decl(tree_stmt(e, 0), 1));
   vcode_select_unit(v0);

   EXPECT_BB(0) = {
      { VCODE_OP_CONST, .value = 0 },
      { VCODE_OP_UARRAY_LEN },
      { VCODE_OP_CAST },
      { VCODE_OP_CONST, .value = 1 },
      { VCODE_OP_DEBUG_LOCUS },
      { VCODE_OP_TRAP_SUB },
      { VCODE_OP_CONST, .value = 0 },
      { VCODE_OP_CONST, .value = -1 },
      { VCODE_OP_DEBUG_LOCUS },
      { VCODE_OP_DEBUG_LOCUS },
      { VCODE_OP_RANGE_CHECK },
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
   simplify_local(p);
   lower_unit(p, NULL);
}
END_TEST

START_TEST(test_issue167)
{
   set_standard(STD_00);

   input_from_file(TESTDIR "/lower/issue167.vhd");

   tree_t e = run_elab();
   lower_unit(e, NULL);

   vcode_unit_t v0 = find_unit("WORK.E");
   vcode_select_unit(v0);

   fail_unless(vtype_kind(0) == VCODE_TYPE_CONTEXT);
   fail_unless(vtype_kind(1) == VCODE_TYPE_CONTEXT);

   ident_t p1_name = vtype_name(0);
   fail_unless(icmp(p1_name, "WORK.PKG.P1"));

   // This used to get mangled with @<address>
   ident_t p2_name = vtype_name(2);
   fail_unless(icmp(p2_name, "WORK.E-A.P2"));
}
END_TEST

START_TEST(test_issue164)
{
   input_from_file(TESTDIR "/lower/issue164.vhd");

   tree_t p = parse_check_and_simplify(T_PACKAGE, T_PACK_BODY);
   lower_unit(p, NULL);

   vcode_select_unit(find_unit_for(tree_decl(p, 0)));
   fail_unless(icmp(vcode_unit_name(), "WORK.ISSUE164.SAME_NAME(I)"));

   vcode_select_unit(find_unit_for(tree_decl(p, 1)));
   fail_unless(icmp(vcode_unit_name(), "WORK.ISSUE164.SAME_NAME()I"));
}
END_TEST

START_TEST(test_sigvar)
{
   input_from_file(TESTDIR "/lower/sigvar.vhd");

   tree_t e = run_elab();
   lower_unit(e, NULL);

   {
      vcode_unit_t v0 = find_unit_for(tree_decl(tree_stmt(e, 0), 1));
      vcode_select_unit(v0);

      EXPECT_BB(0) = {
         { VCODE_OP_UARRAY_LEFT },
         { VCODE_OP_CAST },
         { VCODE_OP_UARRAY_RIGHT },
         { VCODE_OP_CAST },
         { VCODE_OP_UARRAY_DIR },
         { VCODE_OP_RANGE_LENGTH },
         { VCODE_OP_ALLOCA },
         { VCODE_OP_WRAP },
         { VCODE_OP_STORE, .name = "Y" },
         { VCODE_OP_CONST, .value = 0 },
         { VCODE_OP_MEMSET },
         { VCODE_OP_RANGE_NULL },
         { VCODE_OP_COND, .target = 2, .target_else = 1 },
      };

      CHECK_BB(0);

      // Pointless block: index checks were elimintated
      EXPECT_BB(1) = {
         { VCODE_OP_JUMP, .target = 2 }
      };

      CHECK_BB(1);

      EXPECT_BB(2) = {
         { VCODE_OP_LOAD, .name = "Y" },
         { VCODE_OP_UARRAY_LEN },
         { VCODE_OP_UNWRAP },
         { VCODE_OP_UNWRAP },
         { VCODE_OP_RESOLVED },
         { VCODE_OP_DEBUG_LOCUS },
         { VCODE_OP_UARRAY_LEN },
         { VCODE_OP_LENGTH_CHECK },
         { VCODE_OP_COPY },
         { VCODE_OP_RETURN }
      };

      CHECK_BB(2);
   }

   {
      vcode_unit_t v0 = find_unit_for(tree_decl(tree_stmt(e, 0), 2));
      vcode_select_unit(v0);

      EXPECT_BB(0) = {
         { VCODE_OP_CONST, .value = 0 },
         { VCODE_OP_UARRAY_LEN },
         { VCODE_OP_DEBUG_LOCUS },
         { VCODE_OP_UARRAY_LEN },
         { VCODE_OP_LENGTH_CHECK },
         { VCODE_OP_UNWRAP },
         { VCODE_OP_UNWRAP },
         { VCODE_OP_RESOLVED },
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
   lower_unit(e, NULL);

   vcode_unit_t v0 = find_unit_for(tree_decl(tree_stmt(e, 0), 1));
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
   lower_unit(e, NULL);

   vcode_unit_t v0 = find_unit("WORK.ISSUE203.MAIN.PROC");
   vcode_select_unit(v0);

   EXPECT_BB(0) = {
      { VCODE_OP_TEMP_STACK_MARK },
      { VCODE_OP_LINK_PACKAGE, .name = "STD.TEXTIO" },
      { VCODE_OP_LINK_VAR, .name = "OUTPUT" },
      { VCODE_OP_CONST, .value = 104 },
      { VCODE_OP_CONST, .value = 101 },
      { VCODE_OP_CONST, .value = 108 },
      { VCODE_OP_CONST, .value = 111 },
      { VCODE_OP_CONST_ARRAY, .length = 5 },
      { VCODE_OP_ADDRESS_OF },
      { VCODE_OP_CONST, .value = 10 },
      { VCODE_OP_CONST, .value = 0 },
      { VCODE_OP_CONST, .value = 5 },
      { VCODE_OP_CONST, .value = 6 },
      { VCODE_OP_ALLOCA },
      { VCODE_OP_ARRAY_REF },
      { VCODE_OP_COPY },
      { VCODE_OP_ARRAY_REF },
      { VCODE_OP_STORE_INDIRECT },
      { VCODE_OP_FILE_WRITE },
      { VCODE_OP_TEMP_STACK_RESTORE },
      { VCODE_OP_RETURN }
   };

   CHECK_BB(0);
}
END_TEST

START_TEST(test_issue215)
{
   input_from_file(TESTDIR "/lower/issue215.vhd");

   tree_t e = run_elab();
   lower_unit(e, NULL);
}
END_TEST

START_TEST(test_choice1)
{
   input_from_file(TESTDIR "/lower/choice1.vhd");

   tree_t e = run_elab();
   lower_unit(e, NULL);

   vcode_unit_t v0 = find_unit("WORK.CHOICE1.P1");
   vcode_select_unit(v0);

   EXPECT_BB(1) = {
      { VCODE_OP_VAR_UPREF, .hops = 1, .name = "S" },
      { VCODE_OP_LOAD_INDIRECT },
      { VCODE_OP_RESOLVED },
      { VCODE_OP_LOAD_INDIRECT },
      { VCODE_OP_CONST, .value = -2147483648 },
      { VCODE_OP_CONST, .value = 0 },
      { VCODE_OP_CMP, .cmp = VCODE_CMP_GEQ },
      { VCODE_OP_CMP, .cmp = VCODE_CMP_LEQ },
      { VCODE_OP_AND },
      { VCODE_OP_COND, .target = 4, .target_else = 3 },
   };

   CHECK_BB(1);

   EXPECT_BB(3) = {
      { VCODE_OP_CONST, .value = 1 },
      { VCODE_OP_CONST, .value = 2 },
      { VCODE_OP_CONST, .value = 3 },
      { VCODE_OP_CONST, .value = 4 },
      { VCODE_OP_CONST, .value = 5 },
      { VCODE_OP_CASE },
   };

   CHECK_BB(3);

   EXPECT_BB(4) = {
      { VCODE_OP_CONST, .value = -1 },
      { VCODE_OP_STORE, .name = "X" },
      { VCODE_OP_JUMP, .target = 2 }
   };

   CHECK_BB(4);

   EXPECT_BB(5) = {
      { VCODE_OP_CONST, .value = 3 },
      { VCODE_OP_STORE, .name = "X" },
      { VCODE_OP_JUMP, .target = 2 }
   };

   CHECK_BB(5);

   EXPECT_BB(6) = {
      { VCODE_OP_CONST, .value = 4 },
      { VCODE_OP_STORE, .name = "X" },
      { VCODE_OP_JUMP, .target = 2 }
   };

   CHECK_BB(6);

   EXPECT_BB(7) = {
      { VCODE_OP_CONST, .value = 5 },
      { VCODE_OP_STORE, .name = "X" },
      { VCODE_OP_JUMP, .target = 2 }
   };

   CHECK_BB(7);
}
END_TEST

START_TEST(test_tag)
{
   input_from_file(TESTDIR "/lower/tag.vhd");

   tree_t e = run_elab();
   lower_unit(e, NULL);

   {
      vcode_unit_t v0 = find_unit("WORK.TAG");
      vcode_select_unit(v0);

      EXPECT_BB(0) = {
         { VCODE_OP_CONST, .value = 0 },
         { VCODE_OP_CONST, .value = 0 },
         { VCODE_OP_CONST, .value = 1 },
         { VCODE_OP_DEBUG_LOCUS },
         { VCODE_OP_INIT_SIGNAL },
         { VCODE_OP_STORE, .name = "P" },
         { VCODE_OP_DEBUG_LOCUS },
         { VCODE_OP_INIT_SIGNAL },
         { VCODE_OP_STORE, .name = "X" },
         { VCODE_OP_DEBUG_LOCUS },
         { VCODE_OP_INIT_SIGNAL },
         { VCODE_OP_STORE, .name = "Y" },
         { VCODE_OP_RETURN },
      };

      CHECK_BB(0);
   }

   {
      vcode_unit_t v1 = find_unit("WORK.P");
      vcode_select_unit(v1);

      EXPECT_BB(0) = {
         { VCODE_OP_CONST, .value = 0 },
         { VCODE_OP_CONST, .value = 0 },
         { VCODE_OP_CONST, .value = 1 },
         { VCODE_OP_DEBUG_LOCUS },
         { VCODE_OP_INIT_SIGNAL },
         { VCODE_OP_STORE, .name = "S" },
         { VCODE_OP_RETURN },
      };

      CHECK_BB(0);
   }
}
END_TEST

START_TEST(test_iffold)
{
   input_from_file(TESTDIR "/lower/iffold.vhd");

   tree_t e = run_elab();
   lower_unit(e, NULL);

   vcode_unit_t v0 = find_unit("WORK.IFFOLD.SUB_I.P1");
   vcode_select_unit(v0);

   EXPECT_BB(1) = {
      { VCODE_OP_CONST, .value = 0 },
      { VCODE_OP_VAR_UPREF, .name = "X", .hops = 1 },
      { VCODE_OP_LOAD_INDIRECT },
      { VCODE_OP_CONST, .value = 1 },
      { VCODE_OP_CONST, .value = 5 },
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
   lower_unit(e, NULL);

   {
      vcode_unit_t v0 = find_unit_for(tree_decl(tree_stmt(e, 0), 1));
      vcode_select_unit(v0);

      EXPECT_BB(0) = {
         { VCODE_OP_NEG },
         { VCODE_OP_STORE, .name = "R" },
         { VCODE_OP_RETURN }
      };

      CHECK_BB(0);
   }

   {
      vcode_unit_t v0 = find_unit_for(tree_decl(tree_stmt(e, 0), 2));
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
   lower_unit(e, NULL);

   vcode_unit_t v0 = find_unit("WORK.ASSERT1.P1");
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

   {
      vcode_unit_t t0 = lower_thunk(tree_value(tree_decl(arch, 0)));
      fail_if(t0 == NULL);
      vcode_select_unit(t0);

      EXPECT_BB(0) = {
         { VCODE_OP_CONST, .value = 1 },
         { VCODE_OP_CONST_ARRAY, .length = 4 },
         { VCODE_OP_ADDRESS_OF },
         { VCODE_OP_CONST, .value = 2 },
         { VCODE_OP_ARRAY_REF },
         { VCODE_OP_LOAD_INDIRECT },
         { VCODE_OP_NOT },
         { VCODE_OP_RETURN },
      };

      CHECK_BB(0);

      vcode_unit_unref(t0);
   }

   {
      vcode_unit_t t1 = lower_thunk(tree_value(tree_decl(arch, 1)));
      fail_if(t1 == NULL);
      vcode_select_unit(t1);

      EXPECT_BB(0) = {
         { VCODE_OP_LINK_PACKAGE, .name = "WORK.PACK" },
         { VCODE_OP_LINK_VAR, .name = "D" },
         { VCODE_OP_CONST, .value = 0 },
         { VCODE_OP_CONST, .value = 1 },
         { VCODE_OP_ARRAY_REF },
         { VCODE_OP_RECORD_REF, .field = 0 },
         { VCODE_OP_LOAD_INDIRECT },
         { VCODE_OP_DEBUG_LOCUS },
         { VCODE_OP_TRAP_ADD },
         { VCODE_OP_RETURN },
      };

      CHECK_BB(0);

      vcode_unit_unref(t1);
   }
}
END_TEST

START_TEST(test_issue303)
{
   input_from_file(TESTDIR "/lower/issue303.vhd");

   tree_t e = run_elab();
   lower_unit(e, NULL);
}
END_TEST

START_TEST(test_dealloc)
{
   input_from_file(TESTDIR "/lower/dealloc.vhd");

   tree_t p = parse_check_and_simplify(T_PACKAGE, T_PACK_BODY);
   lower_unit(p, NULL);

   vcode_unit_t v1 = find_unit_for(tree_decl(p, 1));
   vcode_select_unit(v1);

   EXPECT_BB(0) = {
      { VCODE_OP_TEMP_STACK_MARK },
      { VCODE_OP_CONTEXT_UPREF, .hops = 1 },
      { VCODE_OP_FCALL, .func = "WORK.PACK.ANOTHER_PROC(13WORK.PACK.PTR)" },
      { VCODE_OP_TEMP_STACK_RESTORE },
      { VCODE_OP_RETURN }
   };

   CHECK_BB(0);
}
END_TEST

START_TEST(test_issue324)
{
   input_from_file(TESTDIR "/lower/issue324.vhd");

   tree_t e = run_elab();
   lower_unit(e, NULL);
}
END_TEST

START_TEST(test_issue333)
{
   input_from_file(TESTDIR "/lower/issue333.vhd");

   tree_t e = run_elab();
   lower_unit(e, NULL);

   vcode_unit_t v0 = find_unit("WORK.ISSUE333.MAIN");
   vcode_select_unit(v0);

   EXPECT_BB(1) = {
      { VCODE_OP_CONST, .value = 49 },
      { VCODE_OP_CONST_ARRAY, .length = 1 },
      { VCODE_OP_ADDRESS_OF },
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
      { VCODE_OP_STORE, .name = "L" },
      { VCODE_OP_TEMP_STACK_MARK },
      { VCODE_OP_CONTEXT_UPREF, .hops = 1 },
      { VCODE_OP_INDEX, .name = "L" },
      { VCODE_OP_FCALL, .func = "*WORK.ISSUE333.PROC(" },
      { VCODE_OP_TEMP_STACK_RESTORE },
      { VCODE_OP_CONST, .value = 50 },
      { VCODE_OP_CONST_ARRAY, .length = 2 },
      { VCODE_OP_ADDRESS_OF },
      { VCODE_OP_CONST, .value = 2 },
      { VCODE_OP_CONST, .value = 2 },
      { VCODE_OP_NEW },
      { VCODE_OP_ALL },
      { VCODE_OP_COPY },
      { VCODE_OP_WRAP },
      { VCODE_OP_NEW },
      { VCODE_OP_ALL },
      { VCODE_OP_STORE_INDIRECT },
      { VCODE_OP_STORE, .name = "L" },
      { VCODE_OP_WAIT, .target = 2 }
   };

   CHECK_BB(1);
}
END_TEST

START_TEST(test_issue338)
{
   input_from_file(TESTDIR "/lower/issue338.vhd");

   tree_t e = run_elab();
   lower_unit(e, NULL);

   e = tree_stmt(e, 0);

   // Function f1
   {
      vcode_unit_t vu = find_unit_for(tree_decl(e, 1));
      vcode_select_unit(vu);

      EXPECT_BB(0) = {
         { VCODE_OP_STORE, .name = "*shortcircuit" },
         { VCODE_OP_COND, .target = 1, .target_else = 2 },
      };

      CHECK_BB(0);

      EXPECT_BB(1) = {
         { VCODE_OP_LINK_PACKAGE, .name = "WORK.P" },
         { VCODE_OP_FCALL, .func = "WORK.P.F(B)B" },
         { VCODE_OP_AND },
         { VCODE_OP_STORE, .name = "*shortcircuit" },
         { VCODE_OP_JUMP, .target = 2 },
      };

      CHECK_BB(1);

      EXPECT_BB(2) = {
         { VCODE_OP_LOAD, .name = "*shortcircuit" },
         { VCODE_OP_RETURN },
      };

      CHECK_BB(2);
   }

   // Function f2
   {
      vcode_unit_t vu = find_unit_for(tree_decl(e, 2));
      vcode_select_unit(vu);

      EXPECT_BB(0) = {
         { VCODE_OP_AND },
         { VCODE_OP_RETURN }
      };

      CHECK_BB(0);
   }

   // Function f3
   {
      vcode_unit_t vu = find_unit_for(tree_decl(e, 3));
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
      vcode_unit_t vu = find_unit_for(tree_decl(e, 4));
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
      vcode_unit_t vu = find_unit_for(tree_decl(e, 5));
      vcode_select_unit(vu);

      EXPECT_BB(0) = {
         { VCODE_OP_CONST, .value = 0 },
         { VCODE_OP_STORE, .name = "Y" },
         { VCODE_OP_LINK_PACKAGE, .name = "WORK.P" },
         { VCODE_OP_FCALL, .func = "WORK.P.F(B)B" },
         { VCODE_OP_NOT },
         { VCODE_OP_RETURN }
      };

      CHECK_BB(0);
   }

   // Function f6
   {
      vcode_unit_t vu = find_unit_for(tree_decl(e, 6));
      vcode_select_unit(vu);

      EXPECT_BB(0) = {
         { VCODE_OP_STORE, .name = "*shortcircuit" },
         { VCODE_OP_COND, .target = 2, .target_else = 1 },
      };

      CHECK_BB(0);

      EXPECT_BB(1) = {
         { VCODE_OP_LINK_PACKAGE, .name = "WORK.P" },
         { VCODE_OP_FCALL, .func = "WORK.P.F(B)B" },
         { VCODE_OP_OR },
         { VCODE_OP_STORE, .name = "*shortcircuit" },
         { VCODE_OP_JUMP, .target = 2 },
      };

      CHECK_BB(1);

      EXPECT_BB(2) = {
         { VCODE_OP_LOAD, .name = "*shortcircuit" },
         { VCODE_OP_RETURN },
      };

      CHECK_BB(2);
   }

   // Function f7
   {
      vcode_unit_t vu = find_unit_for(tree_decl(e, 7));
      vcode_select_unit(vu);

      EXPECT_BB(0) = {
         { VCODE_OP_NOT },
         { VCODE_OP_STORE, .name = "*shortcircuit" },
         { VCODE_OP_COND, .target = 2, .target_else = 1 },
      };

      CHECK_BB(0);

      EXPECT_BB(1) = {
         { VCODE_OP_LINK_PACKAGE, .name = "WORK.P" },
         { VCODE_OP_FCALL, .func = "WORK.P.F(B)B" },
         { VCODE_OP_NOR },
         { VCODE_OP_STORE, .name = "*shortcircuit" },
         { VCODE_OP_JUMP, .target = 2 },
      };

      CHECK_BB(1);

      EXPECT_BB(2) = {
         { VCODE_OP_LOAD, .name = "*shortcircuit" },
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
   lower_unit(e, NULL);

   vcode_unit_t v0 = find_unit_for(tree_decl(tree_stmt(e, 0), 1));
   vcode_select_unit(v0);

   EXPECT_BB(0) = {
      { VCODE_OP_LINK_PACKAGE, .name = "STD.STANDARD" },
      { VCODE_OP_CONST_ARRAY, .length = 0 },
      { VCODE_OP_ADDRESS_OF },
      { VCODE_OP_CONST, .value = 1 },
      { VCODE_OP_CONST, .value = 0 },
      { VCODE_OP_CONST, .value = 0 },
      { VCODE_OP_WRAP },
      { VCODE_OP_FCALL, .func = "STD.STANDARD.\"=\"(SS)B" },
      { VCODE_OP_RETURN }
   };

   CHECK_BB(0);
}
END_TEST

START_TEST(test_issue347)
{
   input_from_file(TESTDIR "/lower/issue347.vhd");

   tree_t e = run_elab();
   lower_unit(e, NULL);
}
END_TEST

START_TEST(test_hintbug)
{
   input_from_file(TESTDIR "/lower/hintbug.vhd");

   tree_t e = run_elab();
   lower_unit(e, NULL);

   vcode_unit_t v0 = find_unit("WORK.HINTBUG.P1");
   vcode_select_unit(v0);

   EXPECT_BB(1) = {
      { VCODE_OP_TEMP_STACK_MARK },
      { VCODE_OP_INDEX, .name = "V" },
      { VCODE_OP_CONST, .value = 2 },
      { VCODE_OP_CONTEXT_UPREF, .hops = 1 },
      { VCODE_OP_LOAD, .name = "X" },
      { VCODE_OP_CONST, .value = 0 },
      { VCODE_OP_CONST, .value = 1 },
      { VCODE_OP_FCALL, .func = "WORK.HINTBUG.FUNC(J)Q" },
      { VCODE_OP_UNWRAP },
      { VCODE_OP_DEBUG_LOCUS },
      { VCODE_OP_UARRAY_LEN },
      { VCODE_OP_LENGTH_CHECK },
      { VCODE_OP_COPY },
      { VCODE_OP_TEMP_STACK_RESTORE },
      { VCODE_OP_TEMP_STACK_MARK },
      { VCODE_OP_CONST, .value = 2 },
      { VCODE_OP_LINK_PACKAGE, .name = "STD.STANDARD" },
      { VCODE_OP_CONST, .value = 1 },
      { VCODE_OP_CONST, .value = 0 },
      { VCODE_OP_WRAP },
      { VCODE_OP_LOAD, .name = "X" },
      { VCODE_OP_CONST, .value = 0 },
      { VCODE_OP_CONST, .value = 1 },
      { VCODE_OP_ALLOCA },
      { VCODE_OP_WRAP },
      { VCODE_OP_ARRAY_REF },
      { VCODE_OP_STORE_INDIRECT },
      { VCODE_OP_ARRAY_REF },
      { VCODE_OP_STORE_INDIRECT },
      { VCODE_OP_FCALL , .func = "STD.STANDARD.\"=\"(QQ)B" },
      { VCODE_OP_DEBUG_LOCUS },
      { VCODE_OP_ASSERT },
      { VCODE_OP_TEMP_STACK_RESTORE },
      { VCODE_OP_WAIT, .target = 2 }
   };

   CHECK_BB(1);
}
END_TEST

START_TEST(test_issue351)
{
   input_from_file(TESTDIR "/lower/issue351.vhd");

   tree_t e = run_elab();
   lower_unit(e, NULL);

   vcode_unit_t v0 = find_unit("WORK.ISSUE351.P1");
   vcode_select_unit(v0);

   EXPECT_BB(4) = {
      { VCODE_OP_TEMP_STACK_MARK },
      { VCODE_OP_CONTEXT_UPREF, .hops = 1 },
      { VCODE_OP_LOAD, .name = "I.LOOP1" },
      { VCODE_OP_CONST, .value = 3 },
      { VCODE_OP_CONST, .value = 0 },
      { VCODE_OP_INDEX, .name = "CURR_QUEUE" },
      { VCODE_OP_CAST },
      { VCODE_OP_ARRAY_REF },
      { VCODE_OP_WRAP },
      { VCODE_OP_FCALL, .func = "*WORK.ISSUE351.DUMP_WORDS" },
      { VCODE_OP_TEMP_STACK_RESTORE },
      { VCODE_OP_LOAD, .name = "*right" },
      { VCODE_OP_LOAD, .name = "*step" },
      { VCODE_OP_LOAD, .name = "I.LOOP1" },
      { VCODE_OP_ADD },
      { VCODE_OP_STORE, .name = "I.LOOP1" },
      { VCODE_OP_CMP, .cmp = VCODE_CMP_EQ },
      { VCODE_OP_COND, .target = 3, .target_else = 4 },
   };

   CHECK_BB(4);
}
END_TEST

START_TEST(test_tounsigned)
{
   input_from_file(TESTDIR "/lower/tounsigned.vhd");

   tree_t p = parse_check_and_simplify(T_PACKAGE, T_PACK_BODY);
   lower_unit(p, NULL);

   vcode_unit_t v0 = find_unit_for(tree_decl(p, 0));
   vcode_select_unit(v0);

   EXPECT_BB(0) = {
      { VCODE_OP_CONST, .value = 1 },
      { VCODE_OP_SUB },
      { VCODE_OP_CONST, .value = 0 },
      { VCODE_OP_CONST, .value = 1 },
      { VCODE_OP_RANGE_LENGTH },
      { VCODE_OP_ALLOCA, .subkind = VCODE_ALLOCA_HEAP },
      { VCODE_OP_WRAP },
      { VCODE_OP_STORE, .name = "RESULT" },
      { VCODE_OP_CONST, .value = 0 },
      { VCODE_OP_MEMSET },
      { VCODE_OP_CONST, .value = INT32_MAX },
      { VCODE_OP_CMP, .cmp = VCODE_CMP_GT },
      { VCODE_OP_COND, .target = 2, .target_else = 1 },
   };

   CHECK_BB(0);

   EXPECT_BB(1) = {
      { VCODE_OP_DEBUG_LOCUS },
      { VCODE_OP_INDEX_CHECK },
      { VCODE_OP_JUMP, .target = 2 }
   };

   CHECK_BB(1);

   EXPECT_BB(2) = {
      { VCODE_OP_CONST, .value = 1 },
      { VCODE_OP_SUB },
      { VCODE_OP_CONST, .value = 0 },
      { VCODE_OP_STORE, .name = "I_VAL" },
      { VCODE_OP_CMP, .cmp = VCODE_CMP_GT },
      { VCODE_OP_COND, .target = 4, .target_else = 3 },
   };

   CHECK_BB(2);

   EXPECT_BB(3) = {
      { VCODE_OP_CONST, .value = 1 },
      { VCODE_OP_STORE, .name = "I.MAINLOOP" },
      { VCODE_OP_JUMP, .target = 5 }
   };

   CHECK_BB(3);

   EXPECT_BB(4) = {
      { VCODE_OP_LOAD, .name = "RESULT" },
      { VCODE_OP_RETURN }
   };

   CHECK_BB(4);

   EXPECT_BB(5) = {
      { VCODE_OP_LOAD, .name = "I.MAINLOOP" },
      { VCODE_OP_LOAD, .name = "I_VAL" },
      { VCODE_OP_CONST, .value = 2 },
      { VCODE_OP_REM },    // Optimised from mod
      { VCODE_OP_CONST, .value = 0 },
      { VCODE_OP_CMP, .cmp = VCODE_CMP_EQ },
      { VCODE_OP_COND, .target = 6, .target_else = 7 }
   };

   CHECK_BB(5);

   EXPECT_BB(6) = {
      { VCODE_OP_CONST, .value = 0 },
      { VCODE_OP_CONST, .value = 1 },
      { VCODE_OP_LOAD, .name = "RESULT" },
      { VCODE_OP_CONST, .value = 1 },
      { VCODE_OP_SUB },
      { VCODE_OP_CONST, .value = 0 },
      { VCODE_OP_DEBUG_LOCUS },
      { VCODE_OP_INDEX_CHECK },
      { VCODE_OP_SUB },
      { VCODE_OP_SUB },
      { VCODE_OP_UARRAY_DIR },
      { VCODE_OP_SELECT },
      { VCODE_OP_CAST },
      { VCODE_OP_UNWRAP },
      { VCODE_OP_ARRAY_REF },
      { VCODE_OP_STORE_INDIRECT },
      { VCODE_OP_JUMP, .target = 8 },
   };

   CHECK_BB(6);

   EXPECT_BB(7) = {
      { VCODE_OP_CONST, .value = 1 },
      { VCODE_OP_LOAD, .name = "RESULT" },
      { VCODE_OP_CONST, .value = 1 },
      { VCODE_OP_SUB },
      { VCODE_OP_CONST, .value = 0 },
      { VCODE_OP_DEBUG_LOCUS },
      { VCODE_OP_INDEX_CHECK },
      { VCODE_OP_SUB },
      { VCODE_OP_SUB },
      { VCODE_OP_UARRAY_DIR },
      { VCODE_OP_SELECT },
      { VCODE_OP_CAST },
      { VCODE_OP_UNWRAP },
      { VCODE_OP_ARRAY_REF },
      { VCODE_OP_STORE_INDIRECT },
      { VCODE_OP_JUMP, .target = 8 }
   };

   CHECK_BB(7);

   EXPECT_BB(8) = {
      { VCODE_OP_LOAD, .name = "I_VAL" },
      { VCODE_OP_CONST, .value = 2 },
      { VCODE_OP_DIV },
      { VCODE_OP_STORE, .name = "I_VAL" },
      { VCODE_OP_ADD },
      { VCODE_OP_STORE, .name = "I.MAINLOOP" },
      { VCODE_OP_CMP, .cmp = VCODE_CMP_EQ },
      { VCODE_OP_COND, .target = 4, .target_else = 5 }
   };

   CHECK_BB(8);
}
END_TEST

START_TEST(test_issue357)
{
   input_from_file(TESTDIR "/lower/issue357.vhd");

   tree_t e = run_elab();
   lower_unit(e, NULL);
}
END_TEST

START_TEST(test_signal11)
{
   input_from_file(TESTDIR "/lower/signal11.vhd");

   tree_t e = run_elab();
   lower_unit(e, NULL);

   vcode_unit_t vpack = vcode_find_unit(ident_new("WORK.PACK"));
   fail_if(vpack == NULL);

   vcode_select_unit(vpack);
   fail_unless(vcode_count_vars() == 1);
   fail_unless(vcode_var_name(0) == ident_new("X"));
}
END_TEST

START_TEST(test_access1)
{
   input_from_file(TESTDIR "/lower/access1.vhd");

   tree_t e = run_elab();
   lower_unit(e, NULL);

   tree_t d = search_decls(tree_stmt(e, 0), ident_new("LIST_ADD"), 0);
   fail_if(d == NULL);

   vcode_unit_t v0 = find_unit_for(d);
   vcode_select_unit(v0);

   EXPECT_BB(0) = {
      { VCODE_OP_NULL },
      { VCODE_OP_NEW },
      { VCODE_OP_ALL },
      { VCODE_OP_CONST, .value = INT32_MIN },
      { VCODE_OP_CONST_RECORD },
      { VCODE_OP_ADDRESS_OF },
      { VCODE_OP_COPY },
      { VCODE_OP_CAST },
      { VCODE_OP_STORE, .name = "N" },
      { VCODE_OP_LOAD_INDIRECT },
      { VCODE_OP_DEBUG_LOCUS },
      { VCODE_OP_NULL_CHECK },   // Redundant, optimisation prevented by cast
      { VCODE_OP_CAST },
      { VCODE_OP_ALL },   // Redundant, optimisation prevented by cast
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
   fail_if(error_count() > 0);
   lower_unit(p, NULL);

   tree_t f = tree_decl(p, 11);
   fail_unless(tree_kind(f) == T_FUNC_BODY);

   vcode_unit_t v0 = find_unit_for(f);
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
      { VCODE_OP_LOAD, .name = "I.SUMLOOP" },
      { VCODE_OP_LOAD, .name = "RESULT" },
      { VCODE_OP_UARRAY_LEFT },
      { VCODE_OP_CAST },
      { VCODE_OP_SUB },
      { VCODE_OP_SUB },
      { VCODE_OP_UARRAY_DIR },
      { VCODE_OP_SELECT },
      { VCODE_OP_CAST },
      { VCODE_OP_UNWRAP },
      { VCODE_OP_ARRAY_REF },
      { VCODE_OP_LOAD_INDIRECT },
      { VCODE_OP_DEBUG_LOCUS },
      { VCODE_OP_TRAP_ADD },
      { VCODE_OP_STORE, .name = "RESULT" },
      { VCODE_OP_ADD },
      { VCODE_OP_STORE, .name = "I.SUMLOOP" },
      { VCODE_OP_CMP, .cmp = VCODE_CMP_EQ },
      { VCODE_OP_COND, .target = 2, .target_else = 3 }
   };

   CHECK_BB(3);
}
END_TEST

START_TEST(test_extern1)
{
   input_from_file(TESTDIR "/lower/extern1.vhd");

   tree_t p = parse_check_and_simplify(T_PACKAGE, T_PACK_BODY);
   bounds_check(p);
   fail_if(error_count() > 0);
   lower_unit(p, NULL);

   tree_t f = tree_decl(p, 0);
   fail_unless(tree_kind(f) == T_FUNC_BODY);

   vcode_unit_t v0 = find_unit_for(f);
   vcode_select_unit(v0);

   EXPECT_BB(0) = {
      { VCODE_OP_VAR_UPREF, .hops = 1, .name = "MATH_CZERO" },
      { VCODE_OP_RETURN },
   };

   CHECK_BB(0);
}
END_TEST

START_TEST(test_synopsys1)
{
   input_from_file(TESTDIR "/lower/synopsys1.vhd");

   tree_t p = parse_check_and_simplify(T_PACKAGE, T_PACK_BODY);
   bounds_check(p);
   fail_if(error_count() > 0);
   lower_unit(p, NULL);

   tree_t f = search_decls(p, ident_new("WRITE"), 0);
   fail_if(f == NULL);

   vcode_unit_t v0 = find_unit_for(f);
   vcode_select_unit(v0);

   EXPECT_BB(0) = {
      { VCODE_OP_CONST, .value = 1 },
      { VCODE_OP_UARRAY_LEN },
      { VCODE_OP_CAST },
      { VCODE_OP_CONST, .value = 0 },
      { VCODE_OP_RANGE_LENGTH },
      { VCODE_OP_ALLOCA },
      { VCODE_OP_WRAP },
      { VCODE_OP_STORE, .name = "S" },
      { VCODE_OP_CONST, .value = 0 },
      { VCODE_OP_MEMSET },
      { VCODE_OP_CONST, .value = INT32_MAX },
      { VCODE_OP_CMP, .cmp = VCODE_CMP_GT },
      { VCODE_OP_COND, .target = 2, .target_else = 1 },
   };

   CHECK_BB(0);

   EXPECT_BB(1) = {
      { VCODE_OP_DEBUG_LOCUS },
      { VCODE_OP_INDEX_CHECK },
      { VCODE_OP_JUMP, .target = 2 },
   };

   CHECK_BB(1);

   EXPECT_BB(2) = {
      { VCODE_OP_CONST, .value = 1 },
      { VCODE_OP_UARRAY_LEN },
      { VCODE_OP_CAST },
      { VCODE_OP_CONST, .value = 0 },
      { VCODE_OP_RANGE_LENGTH },
      { VCODE_OP_ALLOCA },
      { VCODE_OP_WRAP },
      { VCODE_OP_STORE, .name = "M" },
      { VCODE_OP_UNWRAP },
      { VCODE_OP_CONST, .value = 0 },
      { VCODE_OP_CONST, .value = INT32_MAX },
      { VCODE_OP_CMP, .cmp = VCODE_CMP_GT },
      { VCODE_OP_COND, .target = 4, .target_else = 3 }
   };

   CHECK_BB(2);
}
END_TEST

START_TEST(test_access2)
{
   input_from_file(TESTDIR "/lower/access2.vhd");

   tree_t p = parse_check_and_simplify(T_PACKAGE, T_PACK_BODY);
   bounds_check(p);
   fail_if(error_count() > 0);
   lower_unit(p, NULL);

   tree_t f = search_decls(p, ident_new("GET_FRESH"), 0);
   fail_if(f == NULL);

   vcode_unit_t v0 = find_unit_for(f);
   vcode_select_unit(v0);

   EXPECT_BB(0) = {
      { VCODE_OP_UARRAY_LEFT },
      { VCODE_OP_CAST },
      { VCODE_OP_UARRAY_RIGHT },
      { VCODE_OP_CAST },
      { VCODE_OP_UARRAY_DIR },
      { VCODE_OP_RANGE_LENGTH },
      { VCODE_OP_NEW },
      { VCODE_OP_ALL },
      { VCODE_OP_CONST, .value = 0 },
      { VCODE_OP_MEMSET },
      { VCODE_OP_WRAP },
      { VCODE_OP_NEW },
      { VCODE_OP_ALL },
      { VCODE_OP_STORE_INDIRECT },
      { VCODE_OP_RETURN }
   };

   CHECK_BB(0);
}
END_TEST

START_TEST(test_vital1)
{
   input_from_file(TESTDIR "/lower/vital1.vhd");

   tree_t p = parse_check_and_simplify(T_PACKAGE, T_PACK_BODY);
   bounds_check(p);
   fail_if(error_count() > 0);
   lower_unit(p, NULL);

   tree_t f = search_decls(p, ident_new("VITALSETUPHOLDCHECK"), 0);
   fail_if(f == NULL);

   vcode_unit_t v0 = find_unit_for(f);
   vcode_select_unit(v0);

   EXPECT_BB(1) = {
      { VCODE_OP_CONST, .value = -1 },
      { VCODE_OP_CONST, .value = 1 },
      { VCODE_OP_SELECT },
      { VCODE_OP_STORE, .name = "*right" },
      { VCODE_OP_STORE, .name = "*step" },
      { VCODE_OP_STORE, .name = "I.L1" },
      { VCODE_OP_JUMP, .target = 3 }
   };

   CHECK_BB(1);

   EXPECT_BB(4) = {
      { VCODE_OP_RESUME,
        .func = "WORK.VITAL_TIMING.PROC(22WORK.VITAL_TIMING.LINEI)" },
      { VCODE_OP_LOAD, .name = "tmp_mark" },
      { VCODE_OP_TEMP_STACK_RESTORE },
      { VCODE_OP_LOAD, .name = "*right" },
      { VCODE_OP_LOAD, .name = "*step" },
      { VCODE_OP_LOAD, .name = "I.L1" },
      { VCODE_OP_ADD },
      { VCODE_OP_STORE, .name = "I.L1" },
      { VCODE_OP_CMP, .cmp = VCODE_CMP_EQ },
      { VCODE_OP_COND, .target = 2, .target_else = 3 }
   };

   CHECK_BB(4);
}
END_TEST

START_TEST(test_case1)
{
   input_from_file(TESTDIR "/lower/case1.vhd");

   tree_t e = run_elab();
   lower_unit(e, NULL);

   tree_t s = tree_stmt(tree_stmt(tree_stmt(e, 0), 0), 0);
   fail_unless(tree_kind(s) == T_CASE);


   vcode_unit_t v0 = find_unit("WORK.CASE7.TESTP");
   vcode_select_unit(v0);

   fail_unless(vcode_count_vars() == 2);

   EXPECT_BB(1) = {
      { VCODE_OP_TEMP_STACK_MARK },
      { VCODE_OP_VAR_UPREF, .name = "X", .hops = 1 },
      { VCODE_OP_LOAD_INDIRECT },
      { VCODE_OP_RESOLVED },
      { VCODE_OP_CONST, .value = 0 },
      { VCODE_OP_STORE, .name = "*enc" },
      { VCODE_OP_CONST, .value = 0 },
      { VCODE_OP_STORE, .name = "*i" },
      { VCODE_OP_JUMP, .target = 3 },
   };

   CHECK_BB(1);

   EXPECT_BB(4) = {
      { VCODE_OP_LOAD, .name = "*enc" },
      { VCODE_OP_CONST, .value = 0x10 },
      { VCODE_OP_CONST, .value = 0x18 },
      { VCODE_OP_CONST, .value = 0x22 },
      { VCODE_OP_CASE },
   };

   CHECK_BB(4);
}
END_TEST

START_TEST(test_incomplete)
{
   input_from_file(TESTDIR "/lower/incomplete.vhd");

   tree_t p = parse_check_and_simplify(T_PACKAGE, T_PACK_BODY);
   bounds_check(p);
   fail_if(error_count() > 0);
   lower_unit(p, NULL);

   vcode_unit_t v0 = find_unit("WORK.P");
   vcode_select_unit(v0);

   fail_unless(vcode_count_vars() == 2);

   EXPECT_BB(0) = {
      { VCODE_OP_CONST, .value = 1 },
      { VCODE_OP_STORE, .name = "C" },
      { VCODE_OP_STORE, .name = "CP" },
      { VCODE_OP_RETURN },
   };

   CHECK_BB(0);
}
END_TEST

START_TEST(test_issue389)
{
   input_from_file(TESTDIR "/lower/issue389.vhd");

   tree_t p = parse_check_and_simplify(T_PACKAGE);
   bounds_check(p);
   fail_if(error_count() > 0);
   lower_unit(p, NULL);

   vcode_unit_t v0 = find_unit("WORK.COMMON");
   vcode_select_unit(v0);

   EXPECT_BB(0) = {
      { VCODE_OP_INDEX, .name = "EXAMPLE_INIT" },
      { VCODE_OP_CONST, .value = 0 },
      { VCODE_OP_CONST_ARRAY, .length = 64 },
      { VCODE_OP_CONST_ARRAY, .length = 32 },
      { VCODE_OP_CONST_RECORD },
      { VCODE_OP_ADDRESS_OF },
      { VCODE_OP_COPY },
      { VCODE_OP_RETURN },
   };

   CHECK_BB(0);
}
END_TEST

START_TEST(test_const1)
{
   input_from_file(TESTDIR "/lower/const1.vhd");

   tree_t b = parse_check_and_simplify(T_PACKAGE, T_PACK_BODY);
   bounds_check(b);
   fail_if(error_count() > 0);
   lower_unit(b, NULL);

   {
      vcode_unit_t v1 = find_unit("WORK.ISSUEH");
      vcode_select_unit(v1);

      fail_unless(vcode_count_vars() == 1);

      EXPECT_BB(0) = {
         { VCODE_OP_CONST, .value = 1 },
         { VCODE_OP_CONST, .value = 0 },
         { VCODE_OP_CONST_ARRAY, .length = 3 },
         { VCODE_OP_ADDRESS_OF },
         { VCODE_OP_CONST, .value = 0 },
         { VCODE_OP_CONST, .value = 2 },
         { VCODE_OP_WRAP },
         { VCODE_OP_STORE, .name = "C" },
         { VCODE_OP_RETURN },
      };

      CHECK_BB(0);
   }
}
END_TEST

START_TEST(test_const2)
{
   input_from_file(TESTDIR "/lower/const2.vhd");

   tree_t e = run_elab();
   lower_unit(e, NULL);

   vcode_unit_t vu = find_unit("WORK.CONST2");
   vcode_select_unit(vu);

   fail_unless(vcode_count_vars() == 1);

   vcode_type_t v0_type = vcode_var_type(0);
   fail_unless(vtype_kind(v0_type) == VCODE_TYPE_UARRAY);
   fail_unless(vtype_kind(vtype_elem(v0_type)) == VCODE_TYPE_SIGNAL);
   fail_unless(vcode_var_flags(0) & VAR_SIGNAL);
}
END_TEST

START_TEST(test_vital2)
{
   input_from_file(TESTDIR "/lower/vital2.vhd");

   tree_t p = parse_check_and_simplify(T_PACKAGE, T_PACK_BODY);
   bounds_check(p);
   fail_if(error_count() > 0);
   vcode_select_unit(lower_unit(p, NULL));

   tree_t f = search_decls(p, ident_new("VITALSETUPHOLDCHECK"), 0);
   fail_if(f == NULL);

   vcode_unit_t v0 = find_unit(istr(tree_ident2(f)));
   vcode_select_unit(v0);

   EXPECT_BB(0) = {
      { VCODE_OP_TEMP_STACK_MARK },
      { VCODE_OP_RECORD_REF, .field = 8 },
      { VCODE_OP_LOAD_INDIRECT },
      { VCODE_OP_DEBUG_LOCUS },
      { VCODE_OP_NULL_CHECK },
      { VCODE_OP_ALL },
      { VCODE_OP_LOAD_INDIRECT },
      { VCODE_OP_UARRAY_LEN },
      { VCODE_OP_UNWRAP },
      { VCODE_OP_UARRAY_DIR },
      { VCODE_OP_UARRAY_LEFT },
      { VCODE_OP_CAST },
      { VCODE_OP_UARRAY_RIGHT },
      { VCODE_OP_CAST },
      { VCODE_OP_RANGE_NULL },
      { VCODE_OP_RANGE_LENGTH },
      { VCODE_OP_ALLOCA },
      { VCODE_OP_STORE, .name = "*i" },
      { VCODE_OP_COND, .target = 2, .target_else = 1 },
   };

   CHECK_BB(0);
}
END_TEST

START_TEST(test_conv1)
{
   input_from_file(TESTDIR "/lower/conv1.vhd");

   tree_t p = parse_check_and_simplify(T_PACKAGE, T_PACK_BODY);
   bounds_check(p);
   fail_if(error_count() > 0);
   lower_unit(p, NULL);

   tree_t f = search_decls(p, ident_new("GET"), 0);
   fail_if(f == NULL);

   vcode_unit_t v0 = find_unit_for(f);
   vcode_select_unit(v0);

   EXPECT_BB(0) = {
      { VCODE_OP_CONST, .value = 0 },
      { VCODE_OP_UARRAY_LEFT },
      { VCODE_OP_CAST },
      { VCODE_OP_UARRAY_RIGHT },
      { VCODE_OP_CAST },
      { VCODE_OP_UARRAY_DIR },
      { VCODE_OP_DEBUG_LOCUS },
      { VCODE_OP_INDEX_CHECK },
      { VCODE_OP_NEG },
      { VCODE_OP_SELECT },
      { VCODE_OP_CAST },
      { VCODE_OP_UNWRAP },
      { VCODE_OP_ARRAY_REF },
      { VCODE_OP_RECORD_REF, .field = 0 },
      { VCODE_OP_LOAD_INDIRECT },
      { VCODE_OP_CAST },
      { VCODE_OP_RETURN }
   };

   CHECK_BB(0);
}
END_TEST

START_TEST(test_resfn1)
{
   input_from_file(TESTDIR "/lower/resfn1.vhd");

   tree_t e = run_elab();
   lower_unit(e, NULL);

   vcode_unit_t vu = find_unit("WORK.RESFN1");
   vcode_select_unit(vu);

   // Should only be one call to resolution wrapper
   EXPECT_BB(0) = {
      { VCODE_OP_CONST, .value = 0 },
      { VCODE_OP_CONST, .value = 0 },
      { VCODE_OP_CONST, .value = 2 },
      { VCODE_OP_CONTEXT_UPREF, .hops = 0 },
      { VCODE_OP_CLOSURE, .func = "WORK.RESFN1.RESOLVED(Q)J" },
      { VCODE_OP_RESOLUTION_WRAPPER },
      { VCODE_OP_CONST, .value = 1 },
      { VCODE_OP_DEBUG_LOCUS },
      { VCODE_OP_INIT_SIGNAL },
      { VCODE_OP_RESOLVE_SIGNAL },
      { VCODE_OP_STORE, .name = "X" },
      { VCODE_OP_DEBUG_LOCUS },
      { VCODE_OP_INIT_SIGNAL },
      { VCODE_OP_RESOLVE_SIGNAL },
      { VCODE_OP_STORE, .name = "Y" },
      { VCODE_OP_RETURN }
   };

   CHECK_BB(0);
}
END_TEST

START_TEST(test_issue426)
{
   input_from_file(TESTDIR "/lower/issue426.vhd");

   tree_t e = run_elab();
   lower_unit(e, NULL);

   vcode_unit_t vu = find_unit("WORK.TEST_1_1.U");
   vcode_select_unit(vu);

   EXPECT_BB(0) = {
      { VCODE_OP_INDEX, .name = "EXP_STATUS" },
      { VCODE_OP_VAR_UPREF, .hops = 1, .name = "EXP_STATUS" },
      { VCODE_OP_COPY },
      { VCODE_OP_RETURN }
   };

   CHECK_BB(0);
}
END_TEST

START_TEST(test_instance1)
{
   input_from_file(TESTDIR "/lower/instance1.vhd");

   tree_t e = run_elab();
   lower_unit(e, NULL);

   vcode_unit_t vu = find_unit("WORK.INSTANCE1.SUB_I");
   vcode_select_unit(vu);

   fail_unless(vcode_unit_kind() == VCODE_UNIT_INSTANCE);

   EXPECT_BB(0) = {
      { VCODE_OP_CONST, .value = 5 },
      { VCODE_OP_CONST, .value = 0 },
      { VCODE_OP_STORE, .name = "WIDTH" },
      { VCODE_OP_CONST_REP, .value = 5 },
      { VCODE_OP_CONST, .value = 0 },
      { VCODE_OP_CONST, .value = 1 },
      { VCODE_OP_DEBUG_LOCUS },
      { VCODE_OP_CONST, .value = 5 },
      { VCODE_OP_INIT_SIGNAL },
      { VCODE_OP_STORE, .name = "X" },
      { VCODE_OP_CONST_ARRAY, .length = 5 },
      { VCODE_OP_ADDRESS_OF },
      { VCODE_OP_MAP_CONST },
      { VCODE_OP_RETURN }
   };

   CHECK_BB(0);
}
END_TEST

START_TEST(test_sig2var)
{
   input_from_file(TESTDIR "/lower/sig2var.vhd");

   tree_t e = run_elab();
   lower_unit(e, NULL);

   {
      vcode_unit_t vfoo = find_unit("WORK.SIG2VAR.FOO(sQ)Q");
      vcode_select_unit(vfoo);

      fail_unless(vcode_count_vars() == 1);

      EXPECT_BB(0) = {
         { VCODE_OP_CONST, .value = 8 },
         { VCODE_OP_INDEX, .name = "V" },
         { VCODE_OP_UNWRAP },
         { VCODE_OP_RESOLVED },
         { VCODE_OP_CONST, .value = 0 },
         { VCODE_OP_CONST, .value = 1 },
         { VCODE_OP_CONST, .value = 8 },
         { VCODE_OP_DEBUG_LOCUS },
         { VCODE_OP_UARRAY_LEN },
         { VCODE_OP_LENGTH_CHECK },
         { VCODE_OP_COPY },
         { VCODE_OP_WRAP },
         { VCODE_OP_RETURN },
      };

      CHECK_BB(0);
   }

   {
      vcode_unit_t vbar = find_unit("WORK.SIG2VAR.BAR(sJ)J");
      vcode_select_unit(vbar);

      fail_unless(vcode_count_vars() == 1);

      EXPECT_BB(0) = {
         { VCODE_OP_RESOLVED },
         { VCODE_OP_LOAD_INDIRECT },
         { VCODE_OP_STORE, .name = "V" },
         { VCODE_OP_RETURN },
      };

      CHECK_BB(0);
   }
}
END_TEST

START_TEST(test_protupref)
{
   set_standard(STD_08);

   input_from_file(TESTDIR "/lower/protupref.vhd");

   parse_check_simplify_and_lower(T_PACKAGE, T_PACK_BODY);

   vcode_unit_t vu = find_unit(
      "WORK.ALERTLOGPKG.ALERTLOGSTRUCTPTYPE.ALERT"
      "(14ALERTLOGIDTYPES26WORK.ALERTLOGPKG.ALERTTYPE)");
   vcode_select_unit(vu);

   EXPECT_BB(0) = {
      { VCODE_OP_CONST, .value = 0 },
      { VCODE_OP_VAR_UPREF, .name = "ALERT_NAME", .hops = 2 },
      { VCODE_OP_CAST },
      { VCODE_OP_CONST, .value = 7 },
      { VCODE_OP_MUL },
      { VCODE_OP_ARRAY_REF },
      { VCODE_OP_DEBUG_LOCUS },
      { VCODE_OP_REPORT },
      { VCODE_OP_RETURN },
   };

   CHECK_BB(0);
}
END_TEST

START_TEST(test_closefile)
{
   input_from_file(TESTDIR "/lower/closefile.vhd");

   parse_check_simplify_and_lower(T_PACKAGE, T_PACK_BODY);

   vcode_unit_t vu = find_unit("WORK.FILEPACK.TEST");
   vcode_select_unit(vu);

   EXPECT_BB(0) = {
      { VCODE_OP_NULL },
      { VCODE_OP_STORE, .name = "F" },
      { VCODE_OP_CONST, .value = 'f' },
      { VCODE_OP_CONST_ARRAY, .length = 1 },
      { VCODE_OP_ADDRESS_OF },
      { VCODE_OP_CONST, .value = 1 },
      { VCODE_OP_INDEX, .name = "F" },
      { VCODE_OP_CONST, .value = 1 },
      { VCODE_OP_FILE_OPEN },
      { VCODE_OP_LOAD_INDIRECT },
      { VCODE_OP_CMP, .cmp = VCODE_CMP_EQ },
      { VCODE_OP_COND, .target = 2, .target_else = 1 },
   };

   CHECK_BB(0);

   EXPECT_BB(1) = {
      { VCODE_OP_FILE_CLOSE },
      { VCODE_OP_JUMP, .target = 2 },
   };

   CHECK_BB(1);
}
END_TEST

START_TEST(test_record2)
{
   input_from_file(TESTDIR "/lower/record2.vhd");

   tree_t e = run_elab();
   lower_unit(e, NULL);

   {
      vcode_unit_t vu = find_unit("WORK.RECORD2.P1");
      vcode_select_unit(vu);

      EXPECT_BB(1) = {
         { VCODE_OP_INDEX, .name = "R" },
         { VCODE_OP_LOAD, .name = "X" },
         { VCODE_OP_RECORD_REF, .field = 0 },
         { VCODE_OP_STORE_INDIRECT },
         { VCODE_OP_RECORD_REF, .field = 1 },
         { VCODE_OP_STORE_INDIRECT },
         { VCODE_OP_WAIT, .target = 2 },
      };

      CHECK_BB(1);
   }

   {
      vcode_unit_t vu = find_unit("WORK.RECORD2.P2");
      vcode_select_unit(vu);

      EXPECT_BB(1) = {
         { VCODE_OP_NEW },
         { VCODE_OP_ALL },
         { VCODE_OP_CONST, .value = INT32_MIN },
         { VCODE_OP_CONST_RECORD },
         { VCODE_OP_ADDRESS_OF },
         { VCODE_OP_COPY },
         { VCODE_OP_STORE, .name = "R" },
         { VCODE_OP_WAIT, .target = 2 },
      };

      CHECK_BB(1);
   }

   {
      vcode_unit_t vu = find_unit("WORK.RECORD2.P3");
      vcode_select_unit(vu);

      EXPECT_BB(1) = {
         { VCODE_OP_NEW },
         { VCODE_OP_ALL },
         { VCODE_OP_LOAD, .name = "X" },
         { VCODE_OP_RECORD_REF, .field = 0 },
         { VCODE_OP_STORE_INDIRECT },
         { VCODE_OP_RECORD_REF, .field = 1 },
         { VCODE_OP_STORE_INDIRECT },
         { VCODE_OP_STORE, .name = "R" },
         { VCODE_OP_WAIT, .target = 2 },
      };

      CHECK_BB(1);
   }
}
END_TEST

START_TEST(test_array2)
{
   input_from_file(TESTDIR "/lower/array2.vhd");

   tree_t e = run_elab();
   lower_unit(e, NULL);

   {
      vcode_unit_t vu = find_unit("WORK.ARRAY2.P1");
      vcode_select_unit(vu);

      EXPECT_BB(1) = {
         { VCODE_OP_INDEX, .name = "V" },
         { VCODE_OP_LOAD, .name = "X" },
         { VCODE_OP_CONST, .value = 0 },
         { VCODE_OP_ARRAY_REF },
         { VCODE_OP_STORE_INDIRECT },
         { VCODE_OP_CONST, .value = 1 },
         { VCODE_OP_ARRAY_REF },
         { VCODE_OP_STORE_INDIRECT },
         { VCODE_OP_WAIT, .target = 2 },
      };

      CHECK_BB(1);
   }

   {
      vcode_unit_t vu = find_unit("WORK.ARRAY2.P2");
      vcode_select_unit(vu);

      EXPECT_BB(1) = {
         { VCODE_OP_INDEX, .name = "V" },
         { VCODE_OP_CONST, .value = 2 },
         { VCODE_OP_CONST, .value = 0 },
         { VCODE_OP_MEMSET },
         { VCODE_OP_LOAD, .name = "X" },
         { VCODE_OP_CONST, .value = 0 },
         { VCODE_OP_ARRAY_REF },
         { VCODE_OP_STORE_INDIRECT },
         { VCODE_OP_WAIT, .target = 2 },
      };

      CHECK_BB(1);
   }

   {
      vcode_unit_t vu = find_unit("WORK.ARRAY2.P3");
      vcode_select_unit(vu);

      EXPECT_BB(1) = {
         { VCODE_OP_INDEX, .name = "V" },
         { VCODE_OP_CONST, .value = 2 },
         { VCODE_OP_CONST, .value = 1 },
         { VCODE_OP_CONST, .value = 0 },
         { VCODE_OP_ARRAY_REF },
         { VCODE_OP_LOAD, .name = "X" },
         { VCODE_OP_ARRAY_REF },
         { VCODE_OP_STORE_INDIRECT },
         { VCODE_OP_ARRAY_REF },
         { VCODE_OP_STORE_INDIRECT },
         { VCODE_OP_ARRAY_REF },
         { VCODE_OP_LOAD, .name = "Y" },
         { VCODE_OP_ARRAY_REF },
         { VCODE_OP_STORE_INDIRECT },
         { VCODE_OP_ARRAY_REF },
         { VCODE_OP_STORE_INDIRECT },
         { VCODE_OP_WAIT, .target = 2 },
      };

      CHECK_BB(1);
   }

   {
      vcode_unit_t vu = find_unit("WORK.ARRAY2.P4");
      vcode_select_unit(vu);

      EXPECT_BB(1) = {
         { VCODE_OP_TEMP_STACK_MARK },
         { VCODE_OP_INDEX, .name = "V" },
         { VCODE_OP_CONST, .value = 2 },
         { VCODE_OP_CONST, .value = 4 },
         { VCODE_OP_CONST, .value = 0 },
         { VCODE_OP_STORE, .name = "*i" },
         { VCODE_OP_JUMP, .target = 2 },
      };

      CHECK_BB(1);

      EXPECT_BB(2) = {
         { VCODE_OP_LOAD, .name = "*i" },
         { VCODE_OP_ADD },
         { VCODE_OP_STORE, .name = "*i" },
         { VCODE_OP_ARRAY_REF },
         { VCODE_OP_LOAD, .name = "Y" },
         { VCODE_OP_CONST, .value = 0 },
         { VCODE_OP_ARRAY_REF },
         { VCODE_OP_STORE_INDIRECT },
         { VCODE_OP_CONST, .value = 1 },
         { VCODE_OP_ARRAY_REF },
         { VCODE_OP_STORE_INDIRECT },
         { VCODE_OP_CMP, .cmp = VCODE_CMP_EQ },
         { VCODE_OP_COND, .target = 3, .target_else = 2 },
      };

      CHECK_BB(2);

      EXPECT_BB(3) = {
         { VCODE_OP_CONST, .value = 0 },
         { VCODE_OP_ARRAY_REF, .value = 0 },
         { VCODE_OP_LOAD, .name = "X" },
         { VCODE_OP_ARRAY_REF, .value = 0 },
         { VCODE_OP_STORE_INDIRECT },
         { VCODE_OP_CONST, .value = 1 },
         { VCODE_OP_ARRAY_REF },
         { VCODE_OP_STORE_INDIRECT },
         { VCODE_OP_TEMP_STACK_RESTORE },
         { VCODE_OP_WAIT, .target = 4 },
      };

      CHECK_BB(3);
   }

   {
      vcode_unit_t vu = find_unit("WORK.ARRAY2.P5");
      vcode_select_unit(vu);

      EXPECT_BB(1) = {
         { VCODE_OP_INDEX, .name = "V" },
         { VCODE_OP_CONST, .value = 1 },
         { VCODE_OP_CONST, .value = 3 },
         { VCODE_OP_CONST, .value = 0 },
         { VCODE_OP_ARRAY_REF },
         { VCODE_OP_STORE_INDIRECT },
         { VCODE_OP_LOAD, .name = "X" },
         { VCODE_OP_CONST, .value = 1 },
         { VCODE_OP_ARRAY_REF },
         { VCODE_OP_STORE_INDIRECT },
         { VCODE_OP_CONST, .value = 2 },
         { VCODE_OP_ARRAY_REF },
         { VCODE_OP_STORE_INDIRECT },
         { VCODE_OP_WAIT, .target = 2 },
      };

      CHECK_BB(1);
   }

   {
      vcode_unit_t vu = find_unit("WORK.ARRAY2.P6");
      vcode_select_unit(vu);

      EXPECT_BB(1) = {
         { VCODE_OP_CONST, .value = 0 },
         { VCODE_OP_VAR_UPREF, .name = "S", .hops = 1 },
         { VCODE_OP_LOAD_INDIRECT },
         { VCODE_OP_CONST, .value = 1 },
         { VCODE_OP_CONST, .value = 2 },
         { VCODE_OP_INDEX, .name = "*tmp" },
         { VCODE_OP_LOAD, .name = "X" },
         { VCODE_OP_CONST, .value = 0 },
         { VCODE_OP_ARRAY_REF },
         { VCODE_OP_STORE_INDIRECT },
         { VCODE_OP_ARRAY_REF },
         { VCODE_OP_STORE_INDIRECT },
         { VCODE_OP_SCHED_WAVEFORM },
         { VCODE_OP_WAIT, .target = 2 },
      };

      CHECK_BB(1);
   }

   {
      vcode_unit_t vu = find_unit("WORK.ARRAY2.P7");
      vcode_select_unit(vu);

      EXPECT_BB(1) = {
         { VCODE_OP_CONST, .value = 3 },
         { VCODE_OP_NEW },
         { VCODE_OP_ALL },
         { VCODE_OP_CONST, .value = INT32_MIN },
         { VCODE_OP_CONST_REP, .value = 3 },
         { VCODE_OP_COPY },
         { VCODE_OP_CONST, .value = 1 },
         { VCODE_OP_CONST, .value = 3 },
         { VCODE_OP_CONST, .value = 0 },
         { VCODE_OP_WRAP },
         { VCODE_OP_NEW },
         { VCODE_OP_ALL },
         { VCODE_OP_STORE_INDIRECT },
         { VCODE_OP_STORE, .name = "P" },
         { VCODE_OP_WAIT, .target = 2 },
      };

      CHECK_BB(1);
   }

   {
      vcode_unit_t vu = find_unit("WORK.ARRAY2.P8");
      vcode_select_unit(vu);

      EXPECT_BB(1) = {
         { VCODE_OP_CONST, .value = 2 },
         { VCODE_OP_NEW },
         { VCODE_OP_ALL },
         { VCODE_OP_CONST, .value = 0 },
         { VCODE_OP_CONST, .value = 0 },
         { VCODE_OP_CONST, .value = 1 },
         { VCODE_OP_LOAD, .name = "X" },
         { VCODE_OP_CONST, .value = 0 },
         { VCODE_OP_ARRAY_REF },
         { VCODE_OP_STORE_INDIRECT },
         { VCODE_OP_CONST, .value = 1 },
         { VCODE_OP_ARRAY_REF },
         { VCODE_OP_STORE_INDIRECT },
         { VCODE_OP_WRAP },
         { VCODE_OP_NEW },
         { VCODE_OP_ALL },
         { VCODE_OP_STORE_INDIRECT },
         { VCODE_OP_STORE, .name = "P" },
         { VCODE_OP_WAIT, .target = 2 },
      };

      CHECK_BB(1);
   }
}
END_TEST

START_TEST(test_concat)
{
   input_from_file(TESTDIR "/lower/concat.vhd");

   tree_t e = run_elab();
   lower_unit(e, NULL);

   {
      vcode_unit_t vu = find_unit("WORK.CONCAT.P1");
      vcode_select_unit(vu);

      EXPECT_BB(1) = {
         { VCODE_OP_INDEX, .name = "V" },
         { VCODE_OP_CONST, .value = 1 },
         { VCODE_OP_CONST, .value = 2 },
         { VCODE_OP_CONST_ARRAY, .length = 2 },
         { VCODE_OP_ADDRESS_OF },
         { VCODE_OP_CONST, .value = 3 },
         { VCODE_OP_CONST, .value = 0 },
         { VCODE_OP_CONST, .value = 2 },
         { VCODE_OP_ARRAY_REF },
         { VCODE_OP_COPY },
         { VCODE_OP_ARRAY_REF },
         { VCODE_OP_STORE_INDIRECT },
         { VCODE_OP_WAIT, .target = 2 },
      };

      CHECK_BB(1);
   }

   {
      vcode_unit_t vu = find_unit("WORK.CONCAT.P2");
      vcode_select_unit(vu);

      EXPECT_BB(1) = {
         { VCODE_OP_CONST, .value = 0 },
         { VCODE_OP_VAR_UPREF, .name = "S", .hops = 1 },
         { VCODE_OP_LOAD_INDIRECT },
         { VCODE_OP_CONST, .value = 1 },
         { VCODE_OP_CONST, .value = 3 },
         { VCODE_OP_INDEX, .name = "*tmp" },
         { VCODE_OP_CONST, .value = 1 },
         { VCODE_OP_CONST, .value = 2 },
         { VCODE_OP_CONST_ARRAY, .length = 2 },
         { VCODE_OP_ADDRESS_OF },
         { VCODE_OP_CONST, .value = 3 },
         { VCODE_OP_CONST, .value = 0 },
         { VCODE_OP_CONST, .value = 2 },
         { VCODE_OP_ARRAY_REF },
         { VCODE_OP_COPY },
         { VCODE_OP_ARRAY_REF },
         { VCODE_OP_STORE_INDIRECT },
         { VCODE_OP_SCHED_WAVEFORM },
         { VCODE_OP_LOAD_INDIRECT },
         { VCODE_OP_CONST, .value = 4 },
         { VCODE_OP_CONST, .value = 5 },
         { VCODE_OP_CONST, .value = 6 },
         { VCODE_OP_STORE_INDIRECT },
         { VCODE_OP_ARRAY_REF },
         { VCODE_OP_STORE_INDIRECT },
         { VCODE_OP_STORE_INDIRECT },
         { VCODE_OP_SCHED_WAVEFORM },
         { VCODE_OP_WAIT, .target = 2 },
      };

      CHECK_BB(1);
   }
}
END_TEST

START_TEST(test_nullarray)
{
   input_from_file(TESTDIR "/lower/nullarray.vhd");

   tree_t e = run_elab();
   lower_unit(e, NULL);

   vcode_unit_t vu = find_unit("WORK.NULLARRAY");
   vcode_select_unit(vu);

     EXPECT_BB(0) = {
         { VCODE_OP_CONST, .value = 0 },
         { VCODE_OP_CONST, .value = 1 },
         { VCODE_OP_CONST_ARRAY, .length = 3 },
         { VCODE_OP_ADDRESS_OF },
         { VCODE_OP_CONST, .value = 0 },
         { VCODE_OP_CONST, .value = 2 },
         { VCODE_OP_WRAP },
         { VCODE_OP_STORE, .name = "A" },
         { VCODE_OP_RETURN },
     };

     CHECK_BB(0);
}
END_TEST

START_TEST(test_osvvm2)
{
   input_from_file(TESTDIR "/lower/osvvm2.vhd");

   tree_t e = run_elab();
   lower_unit(e, NULL);

   vcode_unit_t vu = find_unit("WORK.OSVVM2.TEST(7NATURAL)");
   vcode_select_unit(vu);

   EXPECT_BB(2) = {
      { VCODE_OP_CONST, .value = 1 },
      { VCODE_OP_CONST, .value = 0 },
      { VCODE_OP_ARRAY_REF },
      { VCODE_OP_CONST, .value = 0 },
      { VCODE_OP_RANGE_LENGTH },
      { VCODE_OP_NEW },
      { VCODE_OP_ALL },
      { VCODE_OP_COPY },
      { VCODE_OP_WRAP },
      { VCODE_OP_NEW },
      { VCODE_OP_ALL },
      { VCODE_OP_STORE_INDIRECT },
      { VCODE_OP_STORE, .name = "FIELDNAME" },
      { VCODE_OP_TEMP_STACK_RESTORE },
      { VCODE_OP_RETURN },
   };

   CHECK_BB(2);
}
END_TEST

START_TEST(test_issue444)
{
   input_from_file(TESTDIR "/lower/issue444.vhd");

   parse_check_simplify_and_lower(T_PACKAGE, T_PACK_BODY);

   fail_if_errors();
}
END_TEST

START_TEST(test_vunit1)
{
   set_standard(STD_02);
   input_from_file(TESTDIR "/lower/vunit1.vhd");

   parse_check_simplify_and_lower(T_PACKAGE, T_PACK_BODY);

   fail_if_errors();
}
END_TEST

START_TEST(test_vunit2)
{
   input_from_file(TESTDIR "/lower/vunit2.vhd");

   parse_check_simplify_and_lower(T_PACKAGE, T_PACK_BODY);

   vcode_unit_t vu = find_unit(
      "WORK.VUNIT2.GET_ONE(7NATURAL)19WORK.VUNIT2.INT_PTR");
   vcode_select_unit(vu);

   EXPECT_BB(0) = {
      { VCODE_OP_VAR_UPREF, .hops = 1, .name = "A" },
      { VCODE_OP_CONST, .value = 1 },
      { VCODE_OP_CONST, .value = 5 },
      { VCODE_OP_CONST, .value = 0 },
      { VCODE_OP_DEBUG_LOCUS },
      { VCODE_OP_INDEX_CHECK },
      { VCODE_OP_SUB },
      { VCODE_OP_CAST },
      { VCODE_OP_ARRAY_REF },
      { VCODE_OP_LOAD_INDIRECT },
      { VCODE_OP_RETURN },
   };

   CHECK_BB(0);

   fail_if_errors();
}
END_TEST

START_TEST(test_vunit3)
{
   input_from_file(TESTDIR "/lower/vunit3.vhd");

   parse_check_simplify_and_lower(T_PACKAGE, T_PACK_BODY);

   vcode_unit_t vu = find_unit("WORK.VUNIT3.ALLOC_REG()19WORK.VUNIT3.REC_PTR");
   vcode_select_unit(vu);

   EXPECT_BB(0) = {
      { VCODE_OP_NEW },
      { VCODE_OP_ALL },
      { VCODE_OP_CONST, .value = INT32_MIN },
      { VCODE_OP_CONST_RECORD },
      { VCODE_OP_ADDRESS_OF },
      { VCODE_OP_COPY },
      { VCODE_OP_RETURN },
   };

   CHECK_BB(0);

   fail_if_errors();
}
END_TEST

START_TEST(test_vunit4)
{
   input_from_file(TESTDIR "/lower/vunit4.vhd");

   parse_check_simplify_and_lower(T_PACKAGE, T_PACK_BODY);

   vcode_unit_t vu = find_unit(
      "WORK.VUNIT4.GET_REC(7NATURAL)15WORK.VUNIT4.REC");
   vcode_select_unit(vu);

   EXPECT_BB(0) = {
      { VCODE_OP_VAR_UPREF, .hops = 1, .name = "V" },
      { VCODE_OP_CONST, .value = 1 },
      { VCODE_OP_CONST, .value = 5 },
      { VCODE_OP_CONST, .value = 0 },
      { VCODE_OP_DEBUG_LOCUS },
      { VCODE_OP_INDEX_CHECK },
      { VCODE_OP_SUB },
      { VCODE_OP_CAST },
      { VCODE_OP_ARRAY_REF },
      { VCODE_OP_LOAD_INDIRECT },
      { VCODE_OP_INDEX, .name = "record" },
      { VCODE_OP_RECORD_REF, .field = 0 },
      { VCODE_OP_STORE_INDIRECT },
      { VCODE_OP_RETURN },
   };

   CHECK_BB(0);

   fail_if_errors();
}
END_TEST

START_TEST(test_directmap)
{
   input_from_file(TESTDIR "/lower/directmap.vhd");

   tree_t e = run_elab();
   lower_unit(e, NULL);

   vcode_unit_t vu = find_unit("WORK.DIRECTMAP.UUT");
   vcode_select_unit(vu);

   EXPECT_BB(0) = {
      { VCODE_OP_VAR_UPREF, .hops = 1, .name = "X" },
      { VCODE_OP_LOAD_INDIRECT },
      { VCODE_OP_DEBUG_LOCUS },
      { VCODE_OP_ALIAS_SIGNAL },
      { VCODE_OP_STORE, .name = "I" },
      { VCODE_OP_CONST, .value = INT32_MIN },
      { VCODE_OP_CONST, .value = 0 },
      { VCODE_OP_CONST, .value = 4 },
      { VCODE_OP_CONST, .value = 1 },
      { VCODE_OP_DEBUG_LOCUS },
      { VCODE_OP_INIT_SIGNAL },
      { VCODE_OP_STORE, .name = "O" },
      { VCODE_OP_VAR_UPREF, .hops = 1, .name = "Y" },
      { VCODE_OP_LOAD_INDIRECT },
      { VCODE_OP_MAP_SIGNAL },
      { VCODE_OP_RETURN },
   };

   CHECK_BB(0);

   fail_if_errors();
}
END_TEST

START_TEST(test_directmap2)
{
   input_from_file(TESTDIR "/lower/directmap2.vhd");

   tree_t e = run_elab();
   lower_unit(e, NULL);

   vcode_unit_t vu = find_unit("WORK.DIRECTMAP2.UUT");
   vcode_select_unit(vu);

   EXPECT_BB(0) = {
      { VCODE_OP_VAR_UPREF, .hops = 1, .name = "P" },
      { VCODE_OP_LOAD_INDIRECT },
      { VCODE_OP_INDEX, .name = "R" },
      { VCODE_OP_RECORD_REF, .field = 1 },
      { VCODE_OP_STORE_INDIRECT },
      { VCODE_OP_VAR_UPREF, .hops = 1, .name = "Q" },
      { VCODE_OP_LOAD_INDIRECT },
      { VCODE_OP_RECORD_REF, .field = 2 },
      { VCODE_OP_STORE_INDIRECT },
      { VCODE_OP_VAR_UPREF, .hops = 1, .name = "I" },
      { VCODE_OP_LOAD_INDIRECT },
      { VCODE_OP_RECORD_REF, .field = 0 },
      { VCODE_OP_STORE_INDIRECT },
      { VCODE_OP_RETURN },
   };

   CHECK_BB(0);

   fail_if_errors();
}
END_TEST

START_TEST(test_recsignal1)
{
   input_from_file(TESTDIR "/lower/recsignal1.vhd");

   tree_t e = run_elab();
   lower_unit(e, NULL);

   vcode_unit_t vu = find_unit("WORK.RECSIGNAL1.P1");
   vcode_select_unit(vu);

   EXPECT_BB(1) = {
      { VCODE_OP_CONST, .value = 0 },
      { VCODE_OP_VAR_UPREF, .hops = 1, .name = "P" },
      { VCODE_OP_CONST, .value = 1 },
      { VCODE_OP_VAR_UPREF, .hops = 1, .name = "Q" },
      { VCODE_OP_RECORD_REF, .field = 0 },
      { VCODE_OP_RECORD_REF, .field = 0 },
      { VCODE_OP_LOAD_INDIRECT },
      { VCODE_OP_LOAD_INDIRECT },
      { VCODE_OP_RESOLVED },
      { VCODE_OP_SCHED_WAVEFORM },
      { VCODE_OP_RECORD_REF, .field = 1 },
      { VCODE_OP_RECORD_REF, .field = 1 },
      { VCODE_OP_LOAD_INDIRECT },
      { VCODE_OP_LOAD_INDIRECT },
      { VCODE_OP_RESOLVED },
      { VCODE_OP_SCHED_WAVEFORM },
      { VCODE_OP_WAIT, .target = 2 },
   };

   CHECK_BB(1);

   fail_if_errors();
}
END_TEST

START_TEST(test_vunit5)
{
   set_standard(STD_08);
   input_from_file(TESTDIR "/lower/vunit5.vhd");

   tree_t e = run_elab();
   lower_unit(e, NULL);

   vcode_unit_t vu = find_unit("WORK.VUNIT5.PROC(Q)");
   vcode_select_unit(vu);

   EXPECT_BB(0) = {
      { VCODE_OP_INDEX, .name = "R" },
      { VCODE_OP_INDEX, .name = "*def" },
      { VCODE_OP_RECORD_REF, .field = 0 },
      { VCODE_OP_UARRAY_LEN },
      { VCODE_OP_CAST },
      { VCODE_OP_CONST, .value = 1 },
      { VCODE_OP_DEBUG_LOCUS },
      { VCODE_OP_TRAP_SUB },
      { VCODE_OP_CONST, .value = 0 },
      { VCODE_OP_CONST, .value = 1 },
      { VCODE_OP_NULL },
      { VCODE_OP_WRAP },
      { VCODE_OP_UARRAY_LEN },
      { VCODE_OP_ALLOCA, .subkind = VCODE_ALLOCA_HEAP },
      { VCODE_OP_CONST, .value = 0 },
      { VCODE_OP_MEMSET },
      { VCODE_OP_WRAP },
      { VCODE_OP_STORE_INDIRECT },
      { VCODE_OP_COPY },
      { VCODE_OP_RECORD_REF, .field = 0 },
      { VCODE_OP_LOAD_INDIRECT },
      { VCODE_OP_UARRAY_LEN },
      { VCODE_OP_UNWRAP },
      { VCODE_OP_UNWRAP },
      { VCODE_OP_DEBUG_LOCUS },
      { VCODE_OP_LENGTH_CHECK },
      { VCODE_OP_COPY },
      { VCODE_OP_RETURN },
   };

   CHECK_BB(0);

   fail_if_errors();
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
   tcase_add_test(tc, test_extern1);
   tcase_add_test(tc, test_synopsys1);
   tcase_add_test(tc, test_access2);
   tcase_add_test(tc, test_vital1);
   tcase_add_test(tc, test_case1);
   tcase_add_test(tc, test_incomplete);
   tcase_add_test(tc, test_issue389);
   tcase_add_test(tc, test_const1);
   tcase_add_test(tc, test_const2);
   tcase_add_test(tc, test_vital2);
   tcase_add_test(tc, test_conv1);
   tcase_add_test(tc, test_resfn1);
   tcase_add_test(tc, test_issue426);
   tcase_add_test(tc, test_instance1);
   tcase_add_test(tc, test_sig2var);
   tcase_add_test(tc, test_protupref);
   tcase_add_test(tc, test_closefile);
   tcase_add_test(tc, test_record2);
   tcase_add_test(tc, test_array2);
   tcase_add_test(tc, test_concat);
   tcase_add_test(tc, test_nullarray);
   tcase_add_test(tc, test_osvvm2);
   tcase_add_test(tc, test_issue444);
   tcase_add_test(tc, test_vunit1);
   tcase_add_test(tc, test_vunit2);
   tcase_add_test(tc, test_vunit3);
   tcase_add_test(tc, test_vunit4);
   tcase_add_test(tc, test_directmap);
   tcase_add_test(tc, test_directmap2);
   tcase_add_test(tc, test_recsignal1);
   tcase_add_test(tc, test_vunit5);
   suite_add_tcase(s, tc);

   return s;
}
