//
//  Copyright (C) 2024-2025  Nick Gasson
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
#include "mir/mir-node.h"
#include "mir/mir-unit.h"
#include "object.h"
#include "option.h"

#include <inttypes.h>

typedef union {
   struct {
      uint64_t data : 60;
      uint64_t tag : 4;
   };
   struct {
      float    real;
      uint32_t ones : 28;
      uint32_t zero : 4;
   };
   const void *ptr;
   uint64_t    bits;
} mir_pattern_t;

STATIC_ASSERT(sizeof(mir_pattern_t) == 8);

#define _ (UINT32_MAX + 1)
#define VAR(name) ((mir_pattern_t){ .ptr = ("\x8" name) })
#define PARAM(name) ((mir_pattern_t){ .ptr = ("\x2" name) })
#define LINK(name) ((mir_pattern_t){ .ptr = ("\xa" name) })
#define NODE(n) ((mir_pattern_t){ .tag = MIR_TAG_NODE, .data = n })
#define BLOCK(n) ((mir_pattern_t){ .tag = MIR_TAG_BLOCK, .data = n })
#define CONST(n) ((mir_pattern_t){ .tag = MIR_TAG_CONST, .data = n })
#define ENUM(n) ((mir_pattern_t){ .tag = MIR_TAG_ENUM, .data = n })
#define REAL(f) ((mir_pattern_t){ .ones = ~0, .real = (float)(f) })

#define mir_match(mu, b, pat) _mir_match((mu), (b), (pat), ARRAY_LEN((pat)))

#define mir_assert_const_eq(mu, value, exp) do {          \
      int64_t __cval;                                     \
      ck_assert(mir_get_const((mu), (value), &__cval));   \
      ck_assert_int_eq(__cval, (exp));                    \
   } while (0)

typedef struct {
   mir_op_t      op;
   mir_pattern_t arg0;
   mir_pattern_t arg1;
   mir_pattern_t arg2;
} mir_match_t;

STATIC_ASSERT(sizeof(mir_match_t) <= 32);

static const char *mir_tag_name(int tag)
{
   static const char *map[] = {
      [MIR_TAG_PARAM] = "parameter",
      [MIR_TAG_VAR] = "variable",
      [MIR_TAG_LINKAGE] = "linkage",
      [MIR_TAG_BLOCK] = "block",
      [MIR_TAG_NODE] = "node",
   };

   return map[tag];
}

static void mir_match_arg(mir_unit_t *mu, mir_value_t node,
                          const mir_pattern_t *arg, int nth)
{
   if (arg->tag == MIR_TAG_NULL && arg->data == 0)
      return;

   const char *str = NULL;
   const int64_t data = arg->data;
   unsigned tag = arg->tag;

   if (arg->tag == MIR_TAG_NULL && arg->ones != 0xfffffff) {
      // Assume this is a pointer to string data
      tag = *(uint8_t *)arg->ptr;
      str = arg->ptr + 1;
   }

   mir_value_t actual = mir_get_arg(mu, node, nth);
   if (actual.tag == MIR_TAG_NULL) {
      mir_dump(mu);
      ck_abort_msg("expected node to have %s argument", ordinal_str(nth + 1));
   }
   else if (tag == MIR_TAG_CONST) {
      int64_t value;
      if (!mir_get_const(mu, actual, &value)) {
         mir_dump(mu);
         ck_abort_msg("%s argument is not constant", ordinal_str(nth + 1));
      }
      else if (value != data) {
         mir_dump(mu);
         ck_abort_msg("expected %s argument to have constant value %"PRIi64
                      " but is %"PRIi64, ordinal_str(nth + 1), data, value);
      }
   }
   else if (tag == MIR_TAG_VAR || tag == MIR_TAG_PARAM
            || tag == MIR_TAG_LINKAGE) {
      ck_assert_ptr_nonnull(str);

      if (actual.tag != tag) {
         mir_dump(mu);
         ck_abort_msg("expected %s for %s argument", mir_tag_name(tag),
                      ordinal_str(nth + 1));
      }

      ident_t name = mir_get_name(mu, actual);
      if (!icmp(name, str)) {
         mir_dump(mu);
         ck_abort_msg("expected %s for %s argument to have name %s but is %s",
                      mir_tag_name(tag), ordinal_str(nth + 1), str, istr(name));
      }
   }
   else if (tag == MIR_TAG_NODE || tag == MIR_TAG_BLOCK) {
      if (actual.tag != tag) {
         mir_dump(mu);
         ck_abort_msg("expected %s for %s argument", mir_tag_name(tag),
                      ordinal_str(nth + 1));
      }
      else if (data != _ && actual.id != data) {
         mir_dump(mu);
         ck_abort_msg("expected %s %%%u for %s argument but have %%%u",
                      mir_tag_name(tag), (unsigned)data, ordinal_str(nth + 1),
                      actual.id);
      }
   }
   else if (tag == MIR_TAG_ENUM) {
      if (actual.tag != tag) {
         mir_dump(mu);
         ck_abort_msg("expected enum for %s argument", ordinal_str(nth + 1));
      }
      else if (data != _ && actual.id != data) {
         mir_dump(mu);
         ck_abort_msg("expected enum %u for %s argument but have %u",
                      (unsigned)data, ordinal_str(nth + 1), actual.id);
      }
   }
   else {
      mir_dump(mu);
      ck_abort_msg("cannot check tag %d", tag);
   }
}

static void _mir_match(mir_unit_t *mu, int nth, const mir_match_t *mm,
                       size_t length)
{
   ck_assert_int_lt(nth, mir_count_blocks(mu));
   mir_block_t block = mir_get_block(mu, nth);

   const int nops = mir_count_nodes(mu, block);
   int eptr = 0, actual = nops;
   for (int i = 0; i < nops; i++) {
      mir_value_t node = mir_get_node(mu, block, i);
      ck_assert_int_eq(node.tag, MIR_TAG_NODE);

      const mir_op_t op = mir_get_op(mu, node);
      if (op == MIR_OP_COMMENT) {
         actual--;
         continue;
      }

      mir_set_cursor(mu, block, i);

      if (eptr >= length)
         break;

      const mir_match_t *e = &(mm[eptr++]);

      if (op != e->op) {
         mir_dump(mu);
         ck_abort_msg("expected op %d in block %d to be %s but was %s",
                      i, nth, mir_op_string(e->op), mir_op_string(op));
      }

      if (op == MIR_OP_CONST_REAL && e->arg0.bits != 0) {
         double dval;
         ck_assert(mir_get_const_real(mu, node, &dval));

         if (fabsl(dval - e->arg0.real) > 0.0001) {
            mir_dump(mu);
            ck_abort_msg("expected constant real value %f but have %f",
                         e->arg0.real, dval);
         }
      }
      else {
         mir_match_arg(mu, node, &(e->arg0), 0);
         mir_match_arg(mu, node, &(e->arg1), 1);
         mir_match_arg(mu, node, &(e->arg2), 2);
      }
   }

   if (eptr != length || length != actual) {
      mir_dump(mu);
      ck_abort_msg("expected %zd ops in block %d but have %d",
                   length, nth, actual);
   }
}

START_TEST(test_sanity1)
{
   mir_context_t *mc = mir_context_new();

   mir_unit_t *mu = mir_unit_new(mc, ident_new("test"), NULL,
                                 MIR_UNIT_FUNCTION, NULL);

   mir_type_t i32 = mir_int_type(mu, INT32_MIN, INT32_MAX);
   fail_unless(mir_equals(i32, mir_int_type(mu, INT32_MIN, INT32_MAX)));
   fail_if(mir_equals(i32, mir_int_type(mu, 0, INT32_MAX)));
   fail_if(mir_equals(i32, mir_int_type(mu, INT32_MIN, 0)));

   mir_set_result(mu, i32);

   mir_value_t p1 = mir_add_param(mu, i32, MIR_NULL_STAMP, ident_new("p1"));
   mir_value_t p2 = mir_add_param(mu, i32, MIR_NULL_STAMP, ident_new("p2"));
   fail_if(mir_equals(p1, p2));

   mir_comment(mu, "hello, world!");

   mir_value_t one = mir_const(mu, i32, 1);
   ck_assert_int_eq(one.tag, MIR_TAG_CONST);

   mir_value_t two = mir_const(mu, i32, 2);
   ck_assert_int_eq(two.tag, MIR_TAG_CONST);

   fail_if(mir_equals(one, two));

   mir_value_t three = mir_build_add(mu, i32, one, two);

   int64_t cval;
   ck_assert(mir_get_const(mu, three, &cval));
   ck_assert_int_eq(cval, 3);

   mir_build_consume(mu, three);

   mir_value_t zero = mir_build_sub(mu, i32, one, one);
   ck_assert(mir_get_const(mu, zero, &cval));
   ck_assert_int_eq(cval, 0);

   mir_value_t cancel = mir_build_sub(mu, i32, p1, p1);
   ck_assert(mir_get_const(mu, cancel, &cval));
   ck_assert_int_eq(cval, 0);

   mir_value_t var = mir_add_var(mu, i32, MIR_NULL_STAMP, ident_new("var"), 0);
   ck_assert_int_eq(var.tag, MIR_TAG_VAR);

   mir_build_store(mu, var, one);

   mir_value_t load = mir_build_load(mu, var);
   ck_assert_int_eq(load.tag, MIR_TAG_NODE);

   mir_type_t t_double = mir_double_type(mu);

   mir_value_t pi = mir_const_real(mu, t_double, 3.142);
   ck_assert_int_eq(pi.tag, MIR_TAG_NODE);

   mir_value_t args[] = { pi, load };
   mir_value_t fcall = mir_build_fcall(mu, ident_new("func"), i32,
                                       MIR_NULL_STAMP, args, ARRAY_LEN(args));
   ck_assert_int_eq(fcall.tag, MIR_TAG_NODE);

   mir_value_t add = mir_build_add(mu, i32, p1, p2);
   mir_value_t result = mir_build_add(mu, i32, add, three);
   mir_build_return(mu, result);

   static const mir_match_t bb0[] = {
      { MIR_OP_STORE, VAR("var") },
      { MIR_OP_LOAD, VAR("var") },
      { MIR_OP_CONST_REAL, REAL(3.142) },
      { MIR_OP_FCALL, LINK("func") },
      { MIR_OP_ADD },
      { MIR_OP_ADD, NODE(_), CONST(3) },
      { MIR_OP_RETURN },
   };
   mir_match(mu, 0, bb0);

   mir_unit_free(mu);
   mir_context_free(mc);
}
END_TEST

START_TEST(test_arith1)
{
   mir_context_t *mc = mir_context_new();

   mir_unit_t *mu = mir_unit_new(mc, ident_new("arith1"), NULL,
                                 MIR_UNIT_FUNCTION, NULL);

   mir_type_t t_int32 = mir_int_type(mu, INT32_MIN, INT32_MAX);

   mir_value_t one = mir_const(mu, t_int32, 1);
   mir_value_t zero = mir_const(mu, t_int32, 0);
   mir_value_t min32 = mir_const(mu, t_int32, INT32_MIN);
   mir_value_t max32 = mir_const(mu, t_int32, INT32_MAX);
   mir_value_t max64 = mir_const(mu, t_int32, INT64_MAX);

   mir_value_t add1 = mir_build_add(mu, t_int32, max32, one);
   ck_assert_int_eq(add1.tag, MIR_TAG_NODE);

   int64_t cval;
   ck_assert(mir_get_const(mu, add1, &cval));
   ck_assert_int_eq(cval, INT32_MAX + UINT64_C(1));

   mir_value_t add2 = mir_build_add(mu, t_int32, max32, zero);
   ck_assert(mir_equals(add2, max32));

   mir_value_t ovf1 = mir_build_add(mu, t_int32, max64, one);
   ck_assert_int_eq(ovf1.tag, MIR_TAG_NODE);
   ck_assert(!mir_get_const(mu, ovf1, &cval));

   mir_value_t bigconst = mir_const(mu, t_int32, (1 << (_MIR_ID_BITS - 1)) - 1);
   ck_assert_int_eq(bigconst.tag, MIR_TAG_CONST);

   mir_value_t add3 = mir_build_add(mu, t_int32, bigconst, one);
   ck_assert_int_eq(add3.tag, MIR_TAG_NODE);

   mir_value_t neg1 = mir_build_neg(mu, t_int32, one);
   mir_assert_const_eq(mu, neg1, -1);

   mir_value_t locus1 = mir_build_locus(mu, NULL);
   mir_value_t neg2 = mir_build_trap_neg(mu, t_int32, min32, locus1);
   ck_assert_int_eq(neg2.tag, MIR_TAG_NODE);

   mir_value_t p1 = mir_add_param(mu, t_int32, MIR_NULL_STAMP, ident_new("p1"));

   mir_value_t mul1 = mir_build_mul(mu, t_int32, p1, one);
   ck_assert(mir_equals(mul1, p1));

   mir_value_t mul2 = mir_build_mul(mu, t_int32, one, p1);
   ck_assert(mir_equals(mul2, p1));

   mir_value_t mul3 = mir_build_trap_mul(mu, t_int32, p1, p1, locus1);
   ck_assert_int_eq(mul3.tag, MIR_TAG_NODE);
   ck_assert(!mir_get_const(mu, mul3, &cval));

   mir_unit_free(mu);
   mir_context_free(mc);
}
END_TEST

START_TEST(test_real1)
{
   mir_context_t *mc = mir_context_new();

   mir_unit_t *mu = mir_unit_new(mc, ident_new("real1"), NULL,
                                 MIR_UNIT_FUNCTION, NULL);

   mir_type_t t_double = mir_double_type(mu);

   mir_value_t one = mir_const_real(mu, t_double, 1.0);
   mir_value_t zero = mir_const_real(mu, t_double, 0.0);

   double cval;

   mir_value_t add1 = mir_build_add(mu, t_double, one, zero);
   ck_assert_int_eq(add1.tag, MIR_TAG_NODE);
   ck_assert(mir_get_const_real(mu, add1, &cval));
   ck_assert_double_eq(cval, 1.0);

   mir_value_t add2 = mir_build_add(mu, t_double, zero, one);
   ck_assert_int_eq(add2.tag, MIR_TAG_NODE);
   ck_assert(mir_get_const_real(mu, add2, &cval));
   ck_assert_double_eq(cval, 1.0);

   mir_value_t add3 = mir_build_add(mu, t_double, one, one);
   ck_assert_int_eq(add3.tag, MIR_TAG_NODE);
   ck_assert(mir_get_const_real(mu, add3, &cval));
   ck_assert_double_eq(cval, 2.0);

   mir_unit_free(mu);
   mir_context_free(mc);
}
END_TEST

START_TEST(test_control1)
{
   mir_context_t *mc = mir_context_new();

   mir_unit_t *mu = mir_unit_new(mc, ident_new("control1"), NULL,
                                 MIR_UNIT_FUNCTION, NULL);

   mir_type_t t_bool = mir_bool_type(mu);
   mir_value_t p1 = mir_add_param(mu, t_bool, MIR_NULL_STAMP, ident_new("p1"));

   mir_block_t b1 = mir_add_block(mu);
   mir_block_t b2 = mir_add_block(mu);
   mir_block_t b3 = mir_add_block(mu);

   mir_build_cond(mu, p1, b1, b2);

   mir_set_cursor(mu, b1, MIR_APPEND);
   mir_build_jump(mu, b3);

   mir_set_cursor(mu, b2, MIR_APPEND);
   mir_build_cond(mu, mir_const(mu, t_bool, 1), b3, b2);

   mir_value_t last = mir_get_node(mu, b2, 0);
   ck_assert_int_eq(mir_get_op(mu, last), MIR_OP_JUMP);

   mir_set_cursor(mu, b3, MIR_APPEND);
   mir_build_return(mu, MIR_NULL_VALUE);

   mir_optimise(mu, MIR_PASS_GVN);

   mir_unit_free(mu);
   mir_context_free(mc);
}
END_TEST

START_TEST(test_gvn1)
{
   mir_context_t *mc = mir_context_new();

   mir_unit_t *mu = mir_unit_new(mc, ident_new("gvn1"), NULL,
                                 MIR_UNIT_FUNCTION, NULL);

   mir_type_t t_int32 = mir_int_type(mu, INT32_MIN, INT32_MAX);
   mir_value_t p1 = mir_add_param(mu, t_int32, MIR_NULL_STAMP, ident_new("p1"));
   mir_value_t p2 = mir_add_param(mu, t_int32, MIR_NULL_STAMP, ident_new("p2"));

   mir_set_result(mu, t_int32);

   mir_block_t b1 = mir_add_block(mu);
   mir_block_t b2 = mir_add_block(mu);
   mir_block_t b3 = mir_add_block(mu);

   mir_value_t ten = mir_const(mu, t_int32, 10);

   mir_value_t add1 = mir_build_add(mu, t_int32, p1, p2);
   mir_value_t sub1 = mir_build_sub(mu, t_int32, add1, p2);
   mir_value_t mul1 = mir_build_mul(mu, t_int32, sub1, p1);
   mir_value_t cmp1 = mir_build_cmp(mu, MIR_CMP_EQ, mul1, ten);
   mir_value_t cmp2 = mir_build_cmp(mu, MIR_CMP_EQ, ten, sub1);
   mir_value_t and = mir_build_and(mu, cmp1, cmp2);
   mir_value_t or = mir_build_or(mu, and, cmp2);
   mir_value_t not = mir_build_not(mu, or);

   mir_build_cond(mu, not, b1, b2);

   mir_set_cursor(mu, b1, MIR_APPEND);
   mir_value_t add2 = mir_build_add(mu, t_int32, p2, p1);
   mir_build_jump(mu, b3);

   mir_set_cursor(mu, b2, MIR_APPEND);
   mir_value_t add3 = mir_build_add(mu, t_int32, p1, p2);
   mir_build_jump(mu, b3);

   mir_set_cursor(mu, b3, MIR_APPEND);
   mir_value_t phi = mir_build_phi(mu, t_int32, 2);
   mir_set_input(mu, phi, 0, b1, add2);
   mir_set_input(mu, phi, 1, b2, add3);
   mir_build_return(mu, phi);

   mir_optimise(mu, MIR_PASS_GVN);

   static const mir_match_t bb0[] = {
      { MIR_OP_ADD, PARAM("p1"), PARAM("p2") },
      { MIR_OP_SUB, NODE(0), PARAM("p2") },
      { MIR_OP_MUL, PARAM("p1"), PARAM("p1") },
      { MIR_OP_CMP, ENUM(MIR_CMP_EQ), NODE(2), CONST(10) },
      { MIR_OP_CMP, ENUM(MIR_CMP_EQ), PARAM("p1"), CONST(10) },
      { MIR_OP_AND, NODE(3), NODE(4) },
      { MIR_OP_OR, NODE(4), NODE(5) },
      { MIR_OP_NOT, NODE(6) },
      { MIR_OP_COND, NODE(7), BLOCK(1), BLOCK(2) },
   };
   mir_match(mu, 0, bb0);

   static const mir_match_t bb3[] = {
      { MIR_OP_PHI, BLOCK(1), NODE(0) },
      { MIR_OP_RETURN, NODE(0) },
   };
   mir_match(mu, 3, bb3);

   mir_unit_free(mu);
   mir_context_free(mc);
}
END_TEST

START_TEST(test_dce1)
{
   mir_context_t *mc = mir_context_new();

   mir_unit_t *mu = mir_unit_new(mc, ident_new("dce1"), NULL,
                                 MIR_UNIT_FUNCTION, NULL);

   mir_type_t t_int32 = mir_int_type(mu, INT32_MIN, INT32_MAX);
   mir_type_t t_ptr = mir_pointer_type(mu, t_int32);

   mir_value_t p1 = mir_add_param(mu, t_int32, MIR_NULL_STAMP, ident_new("p1"));
   mir_value_t p2 = mir_add_param(mu, t_int32, MIR_NULL_STAMP, ident_new("p2"));
   mir_value_t p3 = mir_add_param(mu, t_ptr, MIR_NULL_STAMP, ident_new("p3"));

   mir_set_result(mu, t_int32);

   mir_build_store(mu, p3, p2);

   mir_value_t add1 = mir_build_add(mu, t_int32, p1, p2);
   mir_build_add(mu, t_int32, add1, p2);
   mir_build_return(mu, p1);

   mir_optimise(mu, MIR_PASS_DCE);

   static const mir_match_t bb0[] = {
      { MIR_OP_STORE, PARAM("p3"), PARAM("p2") },
      { MIR_OP_RETURN, PARAM("p1") },
   };
   mir_match(mu, 0, bb0);

   mir_unit_free(mu);
   mir_context_free(mc);
}
END_TEST

START_TEST(test_uarray1)
{
   mir_context_t *mc = mir_context_new();

   mir_unit_t *mu = mir_unit_new(mc, ident_new("uarray1"), NULL,
                                 MIR_UNIT_FUNCTION, NULL);

   mir_type_t t_int32 = mir_int_type(mu, INT32_MIN, INT32_MAX);
   mir_type_t t_intptr = mir_pointer_type(mu, t_int32);

   mir_stamp_t stamp1 = mir_int_stamp(mu, 1, 10);

   mir_value_t p1 = mir_add_param(mu, t_intptr, stamp1, ident_new("p1"));
   mir_value_t p2 = mir_add_param(mu, t_int32, MIR_NULL_STAMP, ident_new("p2"));

   const mir_dim_t dims[] = {
      { mir_const(mu, t_int32, 1), p2, mir_const(mu, t_int32, RANGE_TO) },
   };
   mir_value_t wrap1 = mir_build_wrap(mu, p1, dims, ARRAY_LEN(dims));
   mir_value_t unwrap1 = mir_build_unwrap(mu, wrap1);
   ck_assert_int_eq(p1.bits, unwrap1.bits);

   static const mir_match_t bb0[] = {
      { MIR_OP_WRAP, PARAM("p1") },
   };
   mir_match(mu, 0, bb0);

   mir_unit_free(mu);
   mir_context_free(mc);
}
END_TEST

START_TEST(test_logical1)
{
   mir_context_t *mc = mir_context_new();

   mir_unit_t *mu = mir_unit_new(mc, ident_new("logical1"), NULL,
                                 MIR_UNIT_FUNCTION, NULL);

   mir_type_t t_bool = mir_bool_type(mu);
   mir_value_t p1 = mir_add_param(mu, t_bool, MIR_NULL_STAMP, ident_new("p1"));

   mir_value_t t = mir_const(mu, t_bool, 1), f = mir_const(mu, t_bool, 0);

   mir_value_t not1 = mir_build_not(mu, t);
   mir_assert_const_eq(mu, not1, 0);

   mir_value_t not2 = mir_build_not(mu, f);
   mir_assert_const_eq(mu, not2, 1);

   mir_value_t not3 = mir_build_not(mu, p1);
   mir_value_t not4 = mir_build_not(mu, not3);
   ck_assert_int_eq(not4.bits, p1.bits);

   mir_value_t or1 = mir_build_or(mu, t, f);
   mir_assert_const_eq(mu, or1, 1);

   mir_value_t or2 = mir_build_or(mu, f, f);
   mir_assert_const_eq(mu, or2, 0);

   mir_value_t or3 = mir_build_or(mu, p1, t);
   mir_assert_const_eq(mu, or3, 1);

   mir_value_t or4 = mir_build_or(mu, f, p1);
   ck_assert_int_eq(or4.bits, p1.bits);

   mir_value_t xor1 = mir_build_xor(mu, t, f);
   mir_assert_const_eq(mu, xor1, 1);

   mir_value_t xor2 = mir_build_xor(mu, p1, p1);
   mir_assert_const_eq(mu, xor2, 0);

   mir_value_t and1 = mir_build_and(mu, t, f);
   mir_assert_const_eq(mu, and1, 0);

   mir_value_t and2 = mir_build_and(mu, t, t);
   mir_assert_const_eq(mu, and2, 1);

   mir_value_t and3 = mir_build_and(mu, p1, f);
   mir_assert_const_eq(mu, and3, 0);

   mir_value_t and4 = mir_build_and(mu, p1, t);
   ck_assert_int_eq(and4.bits, p1.bits);

   mir_unit_free(mu);
   mir_context_free(mc);
}
END_TEST

START_TEST(test_signal1)
{
   mir_context_t *mc = mir_context_new();

   mir_unit_t *mu = mir_unit_new(mc, ident_new("uarray1"), NULL,
                                 MIR_UNIT_INSTANCE, NULL);

   mir_type_t t_int32 = mir_int_type(mu, INT32_MIN, INT32_MAX);
   mir_type_t t_intsig = mir_signal_type(mu, t_int32);
   mir_type_t t_offset = mir_offset_type(mu);

   mir_stamp_t pos = mir_int_stamp(mu, 1, INT32_MAX);

   mir_value_t p1 = mir_add_param(mu, t_intsig, pos, ident_new("p1"));

   mir_value_t r1 = mir_build_resolved(mu, p1);

   mir_value_t locus1 = mir_build_locus(mu, NULL);
   mir_value_t s1 = mir_build_init_signal(mu, t_int32,
                                          mir_const(mu, t_offset, 1),
                                          mir_const(mu, t_offset, 4),
                                          mir_const(mu, t_offset, 42),
                                          mir_const(mu, t_int32, 0), locus1,
                                          MIR_NULL_VALUE);
   ck_assert(mir_is_signal(mu, s1));

   mir_block_t block1 = mir_add_block(mu);
   mir_build_jump(mu, block1);

   mir_set_cursor(mu, block1, MIR_APPEND);

   mir_value_t r2 = mir_build_resolved(mu, p1);
   mir_value_t load1 = mir_build_load(mu, r2);
   ck_assert(mir_is_integral(mu, load1));

   mir_build_load(mu, r1);

   mir_build_return(mu, MIR_NULL_VALUE);

   mir_optimise(mu, MIR_PASS_GVN);

   static const mir_match_t bb0[] = {
      { MIR_OP_RESOLVED, PARAM("p1") },
      { MIR_OP_LOCUS },
      { MIR_OP_INIT_SIGNAL },
      { MIR_OP_JUMP, BLOCK(1) },
   };
   mir_match(mu, 0, bb0);

   static const mir_match_t bb1[] = {
      { MIR_OP_RESOLVED, PARAM("p1") },
      { MIR_OP_LOAD, NODE(0) },
      { MIR_OP_LOAD, NODE(0) },
      { MIR_OP_RETURN },
   };
   mir_match(mu, 1, bb1);

   mir_unit_free(mu);
   mir_context_free(mc);
}
END_TEST

START_TEST(test_select1)
{
   mir_context_t *mc = mir_context_new();

   mir_unit_t *mu = mir_unit_new(mc, ident_new("select1"), NULL,
                                 MIR_UNIT_FUNCTION, NULL);

   mir_type_t t_int32 = mir_int_type(mu, INT32_MIN, INT32_MAX);
   mir_type_t t_bool = mir_bool_type(mu);

   mir_set_result(mu, t_int32);

   mir_value_t p1 = mir_add_param(mu, t_bool, MIR_NULL_STAMP, ident_new("p1"));
   mir_value_t p2 = mir_add_param(mu, t_int32, MIR_NULL_STAMP, ident_new("p2"));

   mir_value_t zero = mir_const(mu, t_bool, 0);
   mir_value_t one = mir_const(mu, t_bool, 1);
   mir_value_t two = mir_const(mu, t_int32, 2);

   mir_value_t s1 = mir_build_select(mu, t_int32, one, one, two);
   mir_assert_const_eq(mu, s1, 1);

   mir_value_t s2 = mir_build_select(mu, t_int32, zero, one, two);
   mir_assert_const_eq(mu, s2, 2);

   mir_value_t s3 = mir_build_select(mu, t_int32, p1, two, two);
   mir_assert_const_eq(mu, s3, 2);

   mir_value_t s4 = mir_build_select(mu, t_int32, p1, p2, one);
   mir_value_t s5 = mir_build_select(mu, t_int32, p1, p2, one);

   mir_build_return(mu, mir_build_add(mu, t_int32, s4, s5));

   mir_optimise(mu, MIR_PASS_GVN | MIR_PASS_DCE);

   static const mir_match_t bb0[] = {
      { MIR_OP_SELECT, PARAM("p1"), PARAM("p2"), CONST(1) },
      { MIR_OP_ADD, NODE(0), NODE(0) },
      { MIR_OP_RETURN },
   };
   mir_match(mu, 0, bb0);

   mir_unit_free(mu);
   mir_context_free(mc);
}
END_TEST

START_TEST(test_modrem1)
{
   mir_context_t *mc = mir_context_new();

   mir_unit_t *mu = mir_unit_new(mc, ident_new("modrem1"), NULL,
                                 MIR_UNIT_FUNCTION, NULL);

   mir_type_t t_int32 = mir_int_type(mu, INT32_MIN, INT32_MAX);
   mir_stamp_t pos = mir_int_stamp(mu, 0, INT32_MAX);

   mir_set_result(mu, t_int32);

   mir_value_t p1 = mir_add_param(mu, t_int32, MIR_NULL_STAMP, ident_new("p1"));
   mir_value_t p2 = mir_add_param(mu, t_int32, pos, ident_new("p2"));

   mir_value_t zero = mir_const(mu, t_int32, 0);
   mir_value_t one = mir_const(mu, t_int32, 1);
   mir_value_t two = mir_const(mu, t_int32, 2);

   mir_value_t v1 = mir_build_rem(mu, t_int32, p1, p2);
   mir_value_t v2 = mir_build_rem(mu, t_int32, p1, p2);

   mir_value_t v3 = mir_build_rem(mu, t_int32, one, two);
   mir_assert_const_eq(mu, v3, 1);

   mir_value_t v4 = mir_build_mod(mu, t_int32, p2, two);
   ck_assert_int_eq(mir_get_op(mu, v4), MIR_OP_REM);

   mir_value_t v5 = mir_build_mod(mu, t_int32,
                                  mir_const(mu, t_int32, -5),
                                  mir_const(mu, t_int32, 3));
   ck_assert_int_eq(mir_get_op(mu, v5), MIR_OP_MOD);

   mir_value_t cmp = mir_build_cmp(mu, MIR_CMP_GEQ, v4, zero);

   mir_value_t sel1 = mir_build_select(mu, t_int32, cmp, v1, v2);
   mir_value_t sel2 = mir_build_select(mu, t_int32, cmp, v4, v1);

   mir_build_return(mu, mir_build_select(mu, t_int32, cmp, sel1, sel2));

   mir_optimise(mu, MIR_PASS_GVN | MIR_PASS_DCE);

   static const mir_match_t bb0[] = {
      { MIR_OP_REM, PARAM("p1"), PARAM("p2") },
      { MIR_OP_RETURN },
   };
   mir_match(mu, 0, bb0);

   mir_unit_free(mu);
   mir_context_free(mc);
}
END_TEST

START_TEST(test_cmp1)
{
   mir_context_t *mc = mir_context_new();

   mir_unit_t *mu = mir_unit_new(mc, ident_new("cmp1"), NULL,
                                 MIR_UNIT_FUNCTION, NULL);

   static const struct {
      int64_t left, right;
      mir_cmp_t cmp;
      int result;
   } cases[] = {
      { 0, 0, MIR_CMP_EQ, 1},
      { INT64_MAX, INT64_MAX, MIR_CMP_EQ, 1},
      { INT64_MAX, INT64_MIN, MIR_CMP_EQ, 0},
      { 1, 0, MIR_CMP_NEQ, 1 },
      { 1, 1, MIR_CMP_NEQ, 0 },
      { -1, 0, MIR_CMP_LT, 1 },
      { 5, 0, MIR_CMP_LT, 0 },
      { 1, 0, MIR_CMP_GT, 1 },
      { -5, -2, MIR_CMP_GT, 0 },
      { 6, 6, MIR_CMP_GEQ, 1 },
      { 6, 7, MIR_CMP_GEQ, 0 },
      { 6, 6, MIR_CMP_LEQ, 1 },
      { 6, 7, MIR_CMP_LEQ, 1 },
   };

   mir_type_t t_int32 = mir_int_type(mu, INT32_MIN, INT32_MAX);

   for (int i = 0; i < ARRAY_LEN(cases); i++) {
      mir_value_t left = mir_const(mu, t_int32, cases[i].left);
      mir_value_t right = mir_const(mu, t_int32, cases[i].right);
      mir_value_t value = mir_build_cmp(mu, cases[i].cmp, left, right);

      int64_t result;
      ck_assert(mir_get_const(mu, value, &result));
      ck_assert_msg(result == cases[i].result,
                    "%"PRIi64" <%d> %"PRIi64" ==> %"PRIi64" != %d",
                    cases[i].left, cases[i].cmp, cases[i].right,
                    result, cases[i].result);
   }

   mir_unit_free(mu);
   mir_context_free(mc);
}
END_TEST

START_TEST(test_wait1)
{
   mir_context_t *mc = mir_context_new();

   mir_unit_t *mu = mir_unit_new(mc, ident_new("wait1"), NULL,
                                 MIR_UNIT_PROCESS, NULL);

   ident_t pack = ident_new("foo");
   ident_t func = ident_new("func");

   mir_value_t p1 = mir_build_link_package(mu, pack);
   mir_value_t p2 = mir_build_link_package(mu, pack);
   mir_build_fcall(mu, func, MIR_NULL_TYPE, MIR_NULL_STAMP, &p1, 1);
   mir_build_fcall(mu, func, MIR_NULL_TYPE, MIR_NULL_STAMP, &p2, 1);

   mir_block_t b1 = mir_add_block(mu);
   mir_build_wait(mu, b1);

   mir_set_cursor(mu, b1, MIR_APPEND);

   mir_value_t p3 = mir_build_link_package(mu, pack);
   mir_build_fcall(mu, func, MIR_NULL_TYPE, MIR_NULL_STAMP, &p3, 1);

   mir_build_return(mu, MIR_NULL_VALUE);

   mir_optimise(mu, MIR_PASS_GVN | MIR_PASS_DCE);

   static const mir_match_t bb0[] = {
      { MIR_OP_LINK_PACKAGE, LINK("foo") },
      { MIR_OP_FCALL, LINK("func") },
      { MIR_OP_FCALL, LINK("func") },
      { MIR_OP_WAIT, BLOCK(1) },
   };
   mir_match(mu, 0, bb0);

   static const mir_match_t bb1[] = {
      { MIR_OP_LINK_PACKAGE, LINK("foo") },
      { MIR_OP_FCALL, LINK("func") },
      { MIR_OP_RETURN },
   };
   mir_match(mu, 1, bb1);

   mir_unit_free(mu);
   mir_context_free(mc);
}
END_TEST

static void defer1_cb(mir_unit_t *mu, object_t *obj)
{
   ck_assert_int_eq(mir_get_kind(mu), MIR_UNIT_PROCEDURE);
}

START_TEST(test_defer1)
{
   mir_context_t *mc = mir_context_new();

   make_new_arena();

   ident_t name = ident_new("testfn");
   object_t *obj = tree_to_object(tree_new(T_FUNC_BODY));

   mir_defer(mc, name, NULL, MIR_UNIT_PROCEDURE, defer1_cb, obj);

   mir_unit_t *mu = mir_get_unit(mc, name);
   ck_assert_int_eq(mir_get_kind(mu), MIR_UNIT_PROCEDURE);

   mir_shape_t *sh = mir_get_shape(mc, name);
   ck_assert_ptr_nonnull(sh);

   mir_unit_free(mu);
   ck_assert_ptr_eq(sh, mir_get_shape(mc, name));

   mir_context_free(mc);
}
END_TEST

START_TEST(test_array1)
{
   mir_context_t *mc = mir_context_new();

   mir_unit_t *mu = mir_unit_new(mc, ident_new("array1"), NULL,
                                 MIR_UNIT_FUNCTION, NULL);

   mir_type_t t_int32 = mir_int_type(mu, INT32_MIN, INT32_MAX);
   mir_type_t t_array = mir_carray_type(mu, 3, t_int32);
   mir_type_t t_offset = mir_offset_type(mu);

   ck_assert_int_eq(mir_get_class(mu, t_array), MIR_TYPE_CARRAY);
   ck_assert_int_eq(mir_get_size(mu, t_array), 3);

   const mir_value_t elts[] = {
      mir_const(mu, t_int32, 1),
      mir_const(mu, t_int32, 2),
      mir_const(mu, t_int32, 541241251),
   };
   mir_value_t a1 = mir_const_array(mu, t_array, elts, ARRAY_LEN(elts));

   mir_value_t p1 = mir_build_address_of(mu, a1);
   ck_assert(mir_is(mu, p1, MIR_TYPE_POINTER));

   mir_value_t e1 = mir_build_array_ref(mu, p1, mir_const(mu, t_offset, 1));
   mir_value_t v1 = mir_build_load(mu, e1);
   mir_value_t zero = mir_const(mu, t_int32, 0);
   mir_value_t c1 = mir_build_cmp(mu, MIR_CMP_LT, v1, zero);
   mir_assert_const_eq(mu, c1, 0);

   mir_unit_free(mu);
   mir_context_free(mc);
}
END_TEST

START_TEST(test_case1)
{
   mir_context_t *mc = mir_context_new();

   mir_unit_t *mu = mir_unit_new(mc, ident_new("case1"), NULL,
                                 MIR_UNIT_FUNCTION, NULL);

   mir_type_t t_int32 = mir_int_type(mu, INT32_MIN, INT32_MAX);

   mir_block_t b1 = mir_add_block(mu);
   mir_block_t b2 = mir_add_block(mu);

   mir_value_t cases1[] = { mir_const(mu, t_int32, 42) };
   mir_block_t blocks1[] = { b2 };
   mir_build_case(mu, mir_const(mu, t_int32, 66), b1, cases1, blocks1, 1);

   mir_set_cursor(mu, b1, MIR_APPEND);

   mir_value_t cases2[] = { mir_const(mu, t_int32, 5) };
   mir_block_t blocks2[] = { b2 };
   mir_build_case(mu, mir_const(mu, t_int32, 5), b1, cases2, blocks2, 1);

   static const mir_match_t bb0[] = {
      { MIR_OP_JUMP, BLOCK(1) },
   };
   mir_match(mu, 0, bb0);

   static const mir_match_t bb1[] = {
      { MIR_OP_JUMP, BLOCK(2) },
   };
   mir_match(mu, 1, bb1);

   mir_unit_free(mu);
   mir_context_free(mc);
}
END_TEST

START_TEST(test_record1)
{
   mir_context_t *mc = mir_context_new();

   mir_unit_t *mu = mir_unit_new(mc, ident_new("record1"), NULL,
                                 MIR_UNIT_FUNCTION, NULL);

   mir_type_t t_int32 = mir_int_type(mu, INT32_MIN, INT32_MAX);
   mir_set_result(mu, t_int32);

   const mir_type_t fields[] = { t_int32, t_int32 };
   mir_type_t t_rec = mir_record_type(mu, ident_new("t_rec"), fields,
                                      ARRAY_LEN(fields));

   mir_type_t dup = mir_record_type(mu, ident_new("t_rec"), fields,
                                    ARRAY_LEN(fields));
   ck_assert(mir_equals(t_rec, dup));

   mir_value_t p1 = mir_add_param(mu, mir_pointer_type(mu, t_rec),
                                  MIR_NULL_STAMP, ident_new("p1"));

   mir_value_t v1 = mir_add_var(mu, t_rec, MIR_NULL_STAMP, ident_new("v1"), 0);

   mir_value_t vals[] = {
      mir_const(mu, t_int32, 1),
      mir_const(mu, t_int32, 2),
   };

   mir_value_t c1 = mir_const_record(mu, t_rec, vals, ARRAY_LEN(vals));
   mir_build_copy(mu, v1, mir_build_address_of(mu, c1), MIR_NULL_VALUE);

   mir_value_t rr1 = mir_build_record_ref(mu, p1, 0);
   mir_build_return(mu, mir_build_load(mu, rr1));

   static const mir_match_t bb0[] = {
      { MIR_OP_CONST_RECORD, CONST(1), CONST(2) },
      { MIR_OP_ADDRESS_OF },
      { MIR_OP_COPY, VAR("v1") },
      { MIR_OP_RECORD_REF, PARAM("p1") },
      { MIR_OP_LOAD },
      { MIR_OP_RETURN },
   };
   mir_match(mu, 0, bb0);

   mir_unit_free(mu);
   mir_context_free(mc);
}
END_TEST

START_TEST(test_alias1)
{
   mir_context_t *mc = mir_context_new();

   mir_unit_t *mu = mir_unit_new(mc, ident_new("alias1"), NULL,
                                 MIR_UNIT_FUNCTION, NULL);

   mir_type_t t_char = mir_int_type(mu, 0, UINT8_MAX);
   mir_type_t t_string3 = mir_carray_type(mu, 3, t_char);
   mir_type_t t_offset = mir_offset_type(mu);

   mir_value_t vals[] = {
      mir_const(mu, t_char, 'a'),
      mir_const(mu, t_char, 'b'),
      mir_const(mu, t_char, 'c'),
   };

   mir_value_t array1 = mir_const_array(mu, t_string3, vals, ARRAY_LEN(vals));
   ck_assert_int_eq(mir_get_mem(mu, array1), MIR_MEM_CONST);

   mir_value_t v1 = mir_add_var(mu, t_string3, MIR_NULL_STAMP,
                                ident_new("v1"), 0);
   ck_assert_int_eq(mir_get_mem(mu, v1), MIR_MEM_STACK);

   mir_value_t v2 = mir_add_var(mu, t_string3, MIR_NULL_STAMP,
                                ident_new("v2"), MIR_VAR_HEAP);
   ck_assert_int_eq(mir_get_mem(mu, v2), MIR_MEM_LOCAL);

   mir_value_t ptr1 = mir_build_address_of(mu, array1);
   ck_assert_int_eq(mir_get_mem(mu, ptr1), MIR_MEM_CONST);

   mir_value_t ref1 = mir_build_array_ref(mu, v1, mir_const(mu, t_offset, 1));
   ck_assert_int_eq(mir_get_mem(mu, ref1), MIR_MEM_STACK);

   ck_assert(!mir_may_alias(mu, array1, v1));
   ck_assert(!mir_may_alias(mu, array1, ptr1));
   ck_assert(!mir_may_alias(mu, v1, ptr1));
   ck_assert(mir_may_alias(mu, v1, ref1));
   ck_assert(!mir_may_alias(mu, v1, v2));

   mir_build_return(mu, MIR_NULL_VALUE);

   mir_unit_free(mu);
   mir_context_free(mc);
}
END_TEST

START_TEST(test_cast1)
{
   mir_unit_t *mu = mir_unit_new(get_mir(), ident_new("alias1"), NULL,
                                 MIR_UNIT_FUNCTION, NULL);

   mir_type_t t_offset = mir_offset_type(mu);
   mir_type_t t_int32 = mir_int_type(mu, INT32_MIN, INT32_MAX);

   mir_value_t p1 = mir_add_param(mu, t_offset, mir_int_stamp(mu, 0, INT64_MAX),
                                  ident_new("p1"));
   mir_value_t zero = mir_const(mu, t_int32, 0);

   mir_value_t cast1 = mir_build_cast(mu, t_int32, p1);
   mir_value_t cmp1 = mir_build_cmp(mu, MIR_CMP_LT, cast1, zero);
   mir_assert_const_eq(mu, cmp1, 0);

   mir_build_return(mu, MIR_NULL_VALUE);

   mir_unit_free(mu);
}
END_TEST

START_TEST(test_vec1)
{
   mir_unit_t *mu = mir_unit_new(get_mir(), ident_new("vec1"), NULL,
                                 MIR_UNIT_FUNCTION, NULL);

   mir_type_t t_vec2_8 = mir_vec2_type(mu, 8, false);
   mir_type_t t_vec2_9 = mir_vec2_type(mu, 9, false);

   mir_set_result(mu, t_vec2_9);

   mir_value_t p1 = mir_add_param(mu, t_vec2_8, MIR_NULL_STAMP,
                                  ident_new("p1"));

   mir_value_t c1 = mir_const_vec(mu, t_vec2_8, 5, 0);
   mir_value_t add1 = mir_build_add(mu, t_vec2_9, p1, c1);
   mir_value_t add2 = mir_build_add(mu, t_vec2_9, add1, c1);

   mir_type_t t_carray9 = mir_carray_type(mu, 9, mir_int_type(mu, 0, 3));
   mir_value_t tmp1 = mir_add_var(mu, t_carray9, MIR_NULL_STAMP,
                                  ident_new("tmp1"), MIR_VAR_TEMP);

   mir_value_t unpack1 = mir_build_unpack(mu, add2, 0, tmp1);
   mir_value_t pack1 = mir_build_pack(mu, t_vec2_9, unpack1);

   mir_build_return(mu, pack1);

   mir_unit_free(mu);
}
END_TEST

START_TEST(test_vec2)
{
   mir_unit_t *mu = mir_unit_new(get_mir(), ident_new("vec2"), NULL,
                                 MIR_UNIT_FUNCTION, NULL);

   mir_type_t t_vec2_8 = mir_vec2_type(mu, 8, false);
   mir_type_t t_vec2_1 = mir_vec2_type(mu, 1, false);
   mir_type_t t_offset = mir_offset_type(mu);

   mir_value_t const1 = mir_const_vec(mu, t_vec2_8, 1, 0);
   mir_value_t cast1 = mir_build_cast(mu, t_vec2_1, const1);
   ck_assert_int_eq(mir_get_op(mu, cast1), MIR_OP_CONST_VEC);

   mir_value_t cast2 = mir_build_cast(mu, t_offset, const1);
   mir_assert_const_eq(mu, cast2, 1);

   mir_unit_free(mu);
}
END_TEST

START_TEST(test_check1)
{
   mir_unit_t *mu = mir_unit_new(get_mir(), ident_new("check1"), NULL,
                                 MIR_UNIT_FUNCTION, NULL);

   mir_type_t t_bool = mir_bool_type(mu);
   mir_value_t p1 = mir_add_param(mu, t_bool, MIR_NULL_STAMP, ident_new("p1"));
   mir_value_t locus = mir_build_locus(mu, NULL);

   mir_build_dir_check(mu, p1, mir_const(mu, t_bool, RANGE_TO), locus);
   mir_build_dir_check(mu, p1, p1, locus);

   static const mir_match_t bb0[] = {
      { MIR_OP_LOCUS },
      { MIR_OP_DIR_CHECK, PARAM("p1"), CONST(0) },
   };
   mir_match(mu, 0, bb0);
}
END_TEST

START_TEST(test_cfg1)
{
   mir_unit_t *mu = mir_unit_new(get_mir(), ident_new("cfg1"), NULL,
                                 MIR_UNIT_FUNCTION, NULL);

   mir_build_return(mu, MIR_NULL_VALUE);

   mir_block_t b2 = mir_add_block(mu);
   mir_block_t b3 = mir_add_block(mu);

   mir_set_cursor(mu, b2, MIR_APPEND);

   mir_type_t t_offset = mir_offset_type(mu);

   mir_build_debug_out(mu, mir_const(mu, t_offset, 42));
   mir_build_jump(mu, b3);

   mir_set_cursor(mu, b3, MIR_APPEND);

   mir_build_debug_out(mu, mir_const(mu, t_offset, 111));
   mir_build_jump(mu, b2);

   mir_optimise(mu, MIR_PASS_CFG);

   for (int i = 1; i <= 2; i++) {
      static const mir_match_t bb[] = {
         { MIR_OP_UNREACHABLE },
      };
      mir_match(mu, i, bb);
   }
}
END_TEST

Suite *get_mir_tests(void)
{
   Suite *s = suite_create("mir");

   TCase *tc = nvc_unit_test();
   tcase_add_test(tc, test_sanity1);
   tcase_add_test(tc, test_arith1);
   tcase_add_test(tc, test_real1);
   tcase_add_test(tc, test_control1);
   tcase_add_test(tc, test_gvn1);
   tcase_add_test(tc, test_dce1);
   tcase_add_test(tc, test_uarray1);
   tcase_add_test(tc, test_logical1);
   tcase_add_test(tc, test_signal1);
   tcase_add_test(tc, test_select1);
   tcase_add_test(tc, test_modrem1);
   tcase_add_test(tc, test_cmp1);
   tcase_add_test(tc, test_wait1);
   tcase_add_test(tc, test_defer1);
   tcase_add_test(tc, test_array1);
   tcase_add_test(tc, test_case1);
   tcase_add_test(tc, test_record1);
   tcase_add_test(tc, test_alias1);
   tcase_add_test(tc, test_cast1);
   tcase_add_test(tc, test_vec1);
   tcase_add_test(tc, test_vec2);
   tcase_add_test(tc, test_check1);
   tcase_add_test(tc, test_cfg1);
   suite_add_tcase(s, tc);

   return s;
}
