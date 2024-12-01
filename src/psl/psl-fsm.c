//
//  Copyright (C) 2023-2024 Nick Gasson
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

#include "util.h"
#include "array.h"
#include "common.h"
#include "diag.h"
#include "hash.h"
#include "ident.h"
#include "mask.h"
#include "option.h"
#include "phase.h"
#include "psl/psl-fsm.h"
#include "psl/psl-node.h"
#include "psl/psl-phase.h"

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <inttypes.h>

#define CANNOT_HANDLE(p) do {                                   \
      fatal_at(psl_loc(p), "cannot handle PSL kind %s in %s",   \
               psl_kind_str(psl_kind(p)),  __FUNCTION__);       \
   } while (0)

static fsm_state_t *build_node(psl_fsm_t *fsm, fsm_state_t *state,
                               psl_node_t p);
static void psl_simplify(psl_fsm_t *fsm);

static fsm_state_t *add_state(psl_fsm_t *fsm, psl_node_t where)
{
   fsm_state_t *s = pool_calloc(fsm->pool, sizeof(fsm_state_t));
   s->id    = fsm->next_id++;
   s->where = where;

   *(fsm->tail) = s;
   fsm->tail = &(s->next);

   return s;
}

static bool same_guard(psl_guard_t g1, psl_guard_t g2)
{
   if (g1 == g2)
      return true;

   const guard_kind_t g1_kind = psl_guard_kind(g1);
   const guard_kind_t g2_kind = psl_guard_kind(g2);

   if (g1_kind != g2_kind)
      return false;
   else if (g1_kind != GUARD_BINOP)
      return false;

   const guard_binop_t *bop1 = psl_guard_binop(g1);
   const guard_binop_t *bop2 = psl_guard_binop(g1);

   if (bop1->kind != bop2->kind)
      return false;

   return same_guard(bop1->left, bop2->left)
      && same_guard(bop1->right, bop2->right);
}

static void add_edge(psl_fsm_t *fsm, fsm_state_t *from, fsm_state_t *to,
                     edge_kind_t kind, psl_guard_t guard)
{
   fsm_edge_t **p = &(from->edges);
   for (; *p; p = &((*p)->next)) {
      if ((*p)->kind != kind || (*p)->dest != to)
         continue;
      else if (same_guard((*p)->guard, guard))
         return;   // Duplicate edge
   }

   fsm_edge_t *e = pool_malloc(fsm->pool, sizeof(fsm_edge_t));
   e->next  = *p;
   e->kind  = kind;
   e->dest  = to;
   e->guard = guard;

   *p = e;
}

static int64_t get_number(tree_t t)
{
   int64_t result = 0;
   if (!folded_int(t, &result))
      error_at(tree_loc(t), "static value of PSL Number is not known");
   else if (result < 0)
      warn_at(tree_loc(t), "PSL Number %"PRIi64" is negative", result);

   return result;
}

static psl_guard_t make_binop_guard(psl_fsm_t *fsm, binop_kind_t kind,
                                    psl_guard_t left, psl_guard_t right)
{
   if (left == NULL)
      return right;
   else if (right == NULL || left == right)
      return left;

   guard_binop_t *bop = pool_malloc(fsm->pool, sizeof(guard_binop_t));
   bop->kind = kind;
   bop->left = left;
   bop->right = right;

   return tag_pointer(bop, GUARD_BINOP);
}

static inline psl_guard_t or_guard(psl_fsm_t *fsm, psl_guard_t left,
                                   psl_guard_t right)
{
   return make_binop_guard(fsm, BINOP_OR, left, right);
}

static inline psl_guard_t and_guard(psl_fsm_t *fsm, psl_guard_t left,
                                    psl_guard_t right)
{
   return make_binop_guard(fsm, BINOP_AND, left, right);
}

static psl_guard_t not_guard(psl_guard_t g)
{
   assert(psl_guard_kind(g) == GUARD_EXPR);

   psl_node_t p = psl_guard_expr(g);
   return tag_pointer(p, p == NULL ? GUARD_FALSE : GUARD_NOT);
}

static psl_guard_t build_else_guard(psl_fsm_t *fsm, fsm_state_t *state)
{
   psl_guard_t g = NULL;

   for (fsm_edge_t *e = state->edges; e; e = e->next)
      g = and_guard(fsm, g, not_guard(e->guard));

   return g;
}

static void connect_abort(psl_fsm_t *fsm, fsm_state_t *from, fsm_state_t *to,
                          psl_node_t guard, bit_mask_t *visited)
{
   if (from->edges == NULL)
      return;   // Final state
   else if (mask_test_and_set(visited, from->id))
      return;   // Cycle

   for (fsm_edge_t *e = from->edges; e; e = e->next)
      connect_abort(fsm, e->dest, to, guard, visited);

   add_edge(fsm, from, to, EDGE_EPSILON, guard);
}

static void connect_default(psl_fsm_t *fsm, fsm_state_t *from, fsm_state_t *to,
                            bit_mask_t *visited)
{
   if (from->edges == NULL)
      return;   // Final state
   else if (mask_test_and_set(visited, from->id))
      return;   // Cycle

   bool have_def = false;
   for (fsm_edge_t *e = from->edges; e; e = e->next) {
      connect_default(fsm, e->dest, to, visited);
      have_def |= e->guard == NULL;
   }

   if (!have_def)
      add_edge(fsm, from, to, EDGE_NEXT, build_else_guard(fsm, from));
}

static fsm_state_t *intersect_fsm(psl_fsm_t *fsm, fsm_state_t *initial,
                                  fsm_state_t *l_initial, fsm_state_t *l_final,
                                  fsm_state_t *r_initial, fsm_state_t *r_final)
{
   // See chapter 3.5 of "Martin, J.C. (2003) Introduction to languages
   // and the theory of computation" for basic idea.

   ihash_t *map = ihash_new((fsm->next_id - initial->id) * 2);

   // Remove all epsilon transitions before continuing
   l_final->accept = true;
   r_final->accept = true;
   psl_simplify(fsm);

   fsm_state_t *final = add_state(fsm, l_final->where);

   typedef struct {
      fsm_state_t *left;
      fsm_state_t *right;
      fsm_state_t *into;
   } work_item_t;

   SCOPED_A(work_item_t) worklist = AINIT;

   const work_item_t seed = { l_initial, r_initial, initial };
   APUSH(worklist, seed);

   while (worklist.count > 0) {
      const work_item_t wi = APOP(worklist);

      for (fsm_edge_t *e1 = wi.left->edges; e1; e1 = e1->next) {
         for (fsm_edge_t *e2 = wi.right->edges; e2; e2 = e2->next) {
            fsm_state_t *dest;
            if (e1->dest == e2->dest)
               dest = e1->dest;
            else {
               const uint64_t key = (uint64_t)e1->dest->id << 32 | e2->dest->id;

               if ((dest = ihash_get(map, key)) == NULL) {
                  dest = add_state(fsm, wi.left->where);
                  DEBUG_ONLY(dest->mergekey = key);
                  ihash_put(map, key, dest);

                  const work_item_t new = { e1->dest, e2->dest, dest };
                  APUSH(worklist, new);
               }
            }

            assert(e1->kind == e2->kind);

            psl_guard_t guard = and_guard(fsm, e1->guard, e2->guard);
            add_edge(fsm, wi.into, dest, e1->kind, guard);
         }
      }

      if (wi.left->accept && wi.right->accept) {
         psl_guard_t guard = and_guard(fsm, wi.left->guard, wi.right->guard);
         add_edge(fsm, wi.into, final, EDGE_EPSILON, guard);
      }
   }

   ihash_free(map);
   return final;
}

static fsm_state_t *build_logical(psl_fsm_t *fsm, fsm_state_t *state,
                                  psl_node_t p)
{
   psl_node_t lhs = psl_operand(p, 0);
   psl_node_t rhs = psl_operand(p, 1);

   switch (psl_subkind(p)) {
   case PSL_LOGIC_IFF:
      {
         // Only legal with Boolean HDL expression
         fsm_state_t *left = add_state(fsm, p);
         fsm_state_t *right = add_state(fsm, p);
         fsm_state_t *accept = add_state(fsm, p);
         add_edge(fsm, state, left, EDGE_EPSILON, lhs);
         add_edge(fsm, state, right, EDGE_EPSILON, rhs);
         add_edge(fsm, left, accept, EDGE_EPSILON, rhs);
         add_edge(fsm, right, accept, EDGE_EPSILON, lhs);

         psl_guard_t def = build_else_guard(fsm, state);
         add_edge(fsm, state, accept, EDGE_EPSILON, def);

         return accept;
      }

   case PSL_LOGIC_IF:
      {
         fsm_state_t *left = add_state(fsm, p);
         fsm_state_t *right = build_node(fsm, left, rhs);
         add_edge(fsm, state, left, EDGE_EPSILON, lhs);

         psl_guard_t def = build_else_guard(fsm, state);
         add_edge(fsm, state, right, EDGE_EPSILON, def);

         return right;
      }

   case PSL_LOGIC_OR:
      {
         fsm_state_t *accept = add_state(fsm, p), *final;

         // At least one operand must be Boolean
         if (psl_kind(lhs) == P_HDL_EXPR) {
            add_edge(fsm, state, accept, EDGE_EPSILON, lhs);
            final = build_node(fsm, state, rhs);
         }
         else {
            add_edge(fsm, state, accept, EDGE_EPSILON, rhs);
            final = build_node(fsm, state, lhs);
         }

         add_edge(fsm, final, accept, EDGE_EPSILON, NULL);
         return accept;
      }

   default:
      CANNOT_HANDLE(p);
   }
}

static fsm_state_t *build_until(psl_fsm_t *fsm, fsm_state_t *state,
                                psl_node_t p)
{
   psl_node_t lhs = psl_operand(p, 0);
   psl_node_t rhs = psl_operand(p, 1);

   if (psl_flags(p) & PSL_F_INCLUSIVE) {
      fsm_state_t *test = add_state(fsm, p);
      add_edge(fsm, state, test, EDGE_EPSILON, lhs);

      fsm_state_t *new = add_state(fsm, p);
      add_edge(fsm, test, new, EDGE_NEXT, rhs);
      add_edge(fsm, test, state, EDGE_NEXT, NULL);

      return new;
   }
   else {
      fsm_state_t *new = add_state(fsm, p);
      add_edge(fsm, state, new, EDGE_NEXT, rhs);
      add_edge(fsm, state, state, EDGE_NEXT, lhs);

      return new;
   }
}

static fsm_state_t *build_abort(psl_fsm_t *fsm, fsm_state_t *state,
                                psl_node_t p)
{
   psl_node_t lhs = psl_operand(p, 0);
   psl_node_t rhs = psl_operand(p, 1);

   fsm_state_t *final = build_node(fsm, state, lhs);

   fsm_state_t *sink = add_state(fsm, p);
   sink->accept = true;

   LOCAL_BIT_MASK visited;
   mask_init(&visited, fsm->next_id);

   connect_abort(fsm, state, sink, rhs, &visited);

   return final;
}

static fsm_state_t *build_sere(psl_fsm_t *fsm, fsm_state_t *state, psl_node_t p)
{
   const psl_sere_kind_t kind = psl_subkind(p);
   const int nops = psl_operands(p);

   switch (kind) {
   case PSL_SERE_FUSION:
   case PSL_SERE_CONCAT:
      {
         for (int i = 0; i < nops; i++) {
            fsm_state_t *new = build_node(fsm, state, psl_operand(p, i));
            if (i + 1 < nops && new != state) {
               const edge_kind_t ekind =
                  kind == PSL_SERE_FUSION ? EDGE_EPSILON : EDGE_NEXT;
               state = add_state(fsm, p);
               add_edge(fsm, new, state, ekind, NULL);
            }
            else
               state = new;
         }

         return state;
      }

   case PSL_SERE_EQU_AND:
      {
         assert(nops == 2);

         fsm_state_t *l_initial = add_state(fsm, p);
         fsm_state_t *l_final = build_node(fsm, l_initial, psl_operand(p, 0));

         fsm_state_t *r_initial = add_state(fsm, p);
         fsm_state_t *r_final = build_node(fsm, r_initial, psl_operand(p, 1));

         return intersect_fsm(fsm, state, l_initial, l_final,
                              r_initial, r_final);
      }

   default:
      CANNOT_HANDLE(p);
   }
}

static void get_repeat_bounds(psl_node_t p, int *low, int *high, bool *infinite,
                              bool *noncon, bool *goto_rep)
{
   psl_repeat_t kind = psl_subkind(p);

   *low = 0;
   *high = 0;
   *noncon = false;
   *goto_rep = false;
   *infinite = false;

   switch (kind) {
   case PSL_PLUS_REPEAT:
      *low = 1;
      *high = INT32_MAX;
      break;

   case PSL_GOTO_REPEAT:
      *goto_rep = true;
      // Fall-through
   case PSL_EQUAL_REPEAT:
      *noncon = true;
      // Fall-through
   case PSL_TIMES_REPEAT:
      if (psl_has_delay(p)) {
         psl_node_t count = psl_delay(p);
         switch (psl_kind(count)) {
         case P_RANGE:
            *low = get_number(psl_tree(psl_left(count)));
            *high = get_number(psl_tree(psl_right(count)));
            break;
         case P_HDL_EXPR:
            *low = *high = get_number(psl_tree(count));
            break;
         default:
            should_not_reach_here();
         }
      }
      else {
         *low = 0;
         *high = INT32_MAX;
      }
      break;

   default:
      CANNOT_HANDLE(p);
   }

   if (*high == INT32_MAX) {
      *infinite = true;
      *high = *low;
   }
}

static fsm_state_t *build_repeat(psl_fsm_t *fsm, fsm_state_t *state,
                                 psl_node_t p)
{
   int low, high;
   bool infinite, noncon, goto_rep;

   get_repeat_bounds(p, &low, &high, &infinite, &noncon, &goto_rep);

   fsm_state_t *skip = (high > low) ? add_state(fsm, p) : NULL;
   fsm_state_t *initial = state;
   fsm_state_t *last_but_one = NULL;

   psl_node_t seq = psl_value(p);

   for (int i = 0; i < high; i++) {
      bool is_last = (i == high - 1) ? true : false;

      last_but_one = state;
      state = build_node(fsm, state, seq);

      if (noncon) {
         fsm_state_t *wait = add_state(fsm, p);
         psl_guard_t guard = build_else_guard(fsm, last_but_one);
         add_edge(fsm, last_but_one, wait, EDGE_EPSILON, guard);
         add_edge(fsm, wait, last_but_one, EDGE_NEXT, NULL);
      }

      if (!is_last) {
         if (i >= low - 1)
            add_edge(fsm, state, skip, EDGE_EPSILON, NULL);

         fsm_state_t *curr = state;
         state = add_state(fsm, p);
         add_edge(fsm, curr, state, EDGE_NEXT, NULL);
      }
      else if (noncon && !goto_rep) {
         assert(psl_kind(seq) == P_HDL_EXPR);
         fsm_state_t *aux = add_state(fsm, p);
         fsm_state_t *dead = add_state(fsm, p);
         fsm_state_t *wait = add_state(fsm, p);
         add_edge(fsm, state, aux, EDGE_NEXT, NULL);
         add_edge(fsm, aux, dead, EDGE_EPSILON, seq);
         add_edge(fsm, aux, wait, EDGE_EPSILON, build_else_guard(fsm, aux));
         add_edge(fsm, wait, aux, EDGE_NEXT, NULL);
         add_edge(fsm, wait, state, EDGE_EPSILON, NULL);
      }
   }

   if (skip)
      add_edge(fsm, skip, state, EDGE_EPSILON, NULL);

   if (initial != state) {
      if (low == 0)
         add_edge(fsm, initial, state, EDGE_EPSILON, NULL);
      if (infinite)
         add_edge(fsm, state, last_but_one, EDGE_NEXT, NULL);
   }

   return state;
}

static fsm_state_t *build_next(psl_fsm_t *fsm, fsm_state_t *state, psl_node_t p)
{
   if (psl_has_delay(p)) {
      const int cycles = get_number(psl_tree(psl_delay(p)));
      for (int i = 0; i < cycles; i++) {
         fsm_state_t *new = add_state(fsm, p);
         add_edge(fsm, state, new, EDGE_NEXT, NULL);
         state = new;
      }
   }
   else {
      fsm_state_t *new = add_state(fsm, p);
      add_edge(fsm, state, new, EDGE_NEXT, NULL);
      state = new;
   }

   return build_node(fsm, state, psl_value(p));
}

static fsm_state_t *build_eventually(psl_fsm_t *fsm, fsm_state_t *state,
                                     psl_node_t p)
{
   fsm_state_t *wait = add_state(fsm, p);
   fsm_state_t *accept = build_node(fsm, wait, psl_value(p));

   LOCAL_BIT_MASK visited;
   mask_init(&visited, fsm->next_id);

   connect_default(fsm, wait, wait, &visited);

   add_edge(fsm, state, wait, EDGE_NEXT, NULL);
   wait->strong = true;

   return accept;
}

static fsm_state_t *build_before(psl_fsm_t *fsm, fsm_state_t *state,
                                 psl_node_t p)
{
   fsm_state_t *accept = add_state(fsm, p);
   fsm_state_t *fail = add_state(fsm, p);

   state->strong = !!(psl_flags(p) & PSL_F_STRONG);

   if (psl_flags(p) & PSL_F_INCLUSIVE) {
      add_edge(fsm, state, accept, EDGE_EPSILON, psl_operand(p, 0));
      add_edge(fsm, state, fail, EDGE_EPSILON, psl_operand(p, 1));
   }
   else {
      add_edge(fsm, state, fail, EDGE_EPSILON, psl_operand(p, 1));
      add_edge(fsm, state, accept, EDGE_EPSILON, psl_operand(p, 0));
   }

   add_edge(fsm, state, state, EDGE_NEXT, NULL);

   return accept;
}

static fsm_state_t *build_suffix_impl(psl_fsm_t *fsm, fsm_state_t *state,
                                      psl_node_t p)
{
   fsm_state_t *left = build_node(fsm, state, psl_operand(p, 0));
   fsm_state_t *right = add_state(fsm, p);
   fsm_state_t *vacuous = add_state(fsm, p);

   if (psl_subkind(p) == PSL_SUFFIX_OVERLAP)
      add_edge(fsm, left, right, EDGE_EPSILON, NULL);
   else
      add_edge(fsm, left, right, EDGE_NEXT, NULL);

   LOCAL_BIT_MASK visited;
   mask_init(&visited, fsm->next_id);

   connect_default(fsm, state, vacuous, &visited);

   fsm_state_t *final = build_node(fsm, right, psl_operand(p, 1));
   add_edge(fsm, vacuous, final, EDGE_EPSILON, NULL);

   return final;
}

static fsm_state_t *build_node(psl_fsm_t *fsm, fsm_state_t *state, psl_node_t p)
{
   switch (psl_kind(p)) {
   case P_NEVER:
      fsm->kind = FSM_NEVER;
      return build_node(fsm, state, psl_value(p));
   case P_ALWAYS:
      fsm->kind = FSM_ALWAYS;
      return build_node(fsm, state, psl_value(p));
   case P_HDL_EXPR:
      {
         fsm_state_t *new = add_state(fsm, p);
         add_edge(fsm, state, new, EDGE_EPSILON, p);
         return new;
      }
   case P_NEXT:
      return build_next(fsm, state, p);
   case P_SERE:
      return build_sere(fsm, state, p);
   case P_REPEAT:
      return build_repeat(fsm, state, p);
   case P_LOGICAL:
      return build_logical(fsm, state, p);
   case P_UNTIL:
      return build_until(fsm, state, p);
   case P_EVENTUALLY:
      return build_eventually(fsm, state, p);
   case P_ABORT:
      return build_abort(fsm, state, p);
   case P_BEFORE:
      return build_before(fsm, state, p);
   case P_SUFFIX_IMPL:
      return build_suffix_impl(fsm, state, p);
   case P_CLOCKED:
      return build_node(fsm, state, psl_value(p));
   default:
      CANNOT_HANDLE(p);
   }
}

#ifdef DEBUG
static void psl_loops_dfs(psl_fsm_t *fsm, fsm_state_t *state,
                          bit_mask_t *discovered, bit_mask_t *finished)
{
   mask_set(discovered, state->id);

   bool have_epsilon = false, have_unguarded = false;
   for (fsm_edge_t *e = state->edges; e; e = e->next) {
      if (e->kind != EDGE_EPSILON || mask_test(finished, e->dest->id))
         continue;
      else if (mask_test(discovered, e->dest->id)) {
         psl_fsm_dump(fsm, "loop");
         fatal_trace("detected loop in PSL state machine %d -> %d",
                     state->id, e->dest->id);
      }
      else
         psl_loops_dfs(fsm, e->dest, discovered, finished);

      if (have_unguarded || (have_epsilon && e->guard == NULL)) {
         psl_fsm_dump(fsm, "unguarded");
         fatal_trace("unguarded epsilon edge must be only epsilon edge "
                     "(%d -> %d)", state->id, e->dest->id);
      }

      have_epsilon = true;
      have_unguarded |= (e->guard == NULL);
   }

   mask_clear(discovered, state->id);
   mask_set(finished, state->id);
}

static void psl_detect_loops(psl_fsm_t *fsm)
{
   LOCAL_BIT_MASK discovered, finished;
   mask_init(&discovered, fsm->next_id);
   mask_init(&finished, fsm->next_id);

   for (fsm_state_t *it = fsm->states; it; it = it->next) {
      if (!mask_test(&finished, it->id))
         psl_loops_dfs(fsm, it, &discovered, &finished);
   }

   assert(mask_popcount(&discovered) == 0);
   assert(mask_popcount(&finished) == fsm->next_id);
}
#endif

static void psl_simplify_dfs(psl_fsm_t *fsm, fsm_state_t *state,
                             bit_mask_t *visited)
{
   mask_set(visited, state->id);

   for (fsm_edge_t **p = &(state->edges), *e = *p; e; e = *p) {
      if (e->kind == EDGE_EPSILON) {
         if (!mask_test(visited, e->dest->id))
            psl_simplify_dfs(fsm, e->dest, visited);

         if (e->dest->accept) {
            psl_guard_t guard = and_guard(fsm, e->guard, e->dest->guard);
            state->guard = or_guard(fsm, state->guard, guard);
            state->accept = true;
         }

         fsm_edge_t *first = e->dest->edges;
         if (first == NULL)
            *p = e->next;
         else {
            for (fsm_edge_t *e2 = first->next; e2; e2 = e2->next) {
               assert(e2->kind != EDGE_EPSILON);
               psl_guard_t guard = and_guard(fsm, e->guard, e2->guard);
               add_edge(fsm, state, e2->dest, e2->kind, guard);
            }

            assert(first->kind != EDGE_EPSILON);
            e->kind  = first->kind;
            e->dest  = first->dest;
            e->guard = and_guard(fsm, e->guard, first->guard);

            p = &(e->next);
         }
      }
      else
         p = &(e->next);
   }
}

static void psl_simplify(psl_fsm_t *fsm)
{
   LOCAL_BIT_MASK visited;
   mask_init(&visited, fsm->next_id);

   for (fsm_state_t *it = fsm->states; it; it = it->next) {
      if (!mask_test(&visited, it->id))
         psl_simplify_dfs(fsm, it, &visited);
   }

   assert(mask_popcount(&visited) == fsm->next_id);
}

static void psl_prune_dfs(psl_fsm_t *fsm, fsm_state_t *state,
                          bit_mask_t *visited)
{
   if (mask_test_and_set(visited, state->id))
      return;

   state->next = NULL;

   *fsm->tail = state;
   fsm->tail = &(state->next);

   if (state->accept && state->guard == NULL)
      state->edges = NULL;   // Unreachable

   for (fsm_edge_t *e = state->edges; e; e = e->next) {
      assert(e->kind != EDGE_EPSILON);
      psl_prune_dfs(fsm, e->dest, visited);
   }
}

static void psl_prune(psl_fsm_t *fsm)
{
   LOCAL_BIT_MASK visited;
   mask_init(&visited, fsm->next_id);

   fsm_state_t *s0 = fsm->states;
   assert(s0->initial);

   mask_clearall(&visited);
   fsm->next_id = 0;
   fsm->tail    = &(fsm->states);
   fsm->states  = NULL;

   psl_prune_dfs(fsm, s0, &visited);

   for (fsm_state_t *it = fsm->states; it; it = it->next)
      it->id = fsm->next_id++;
}

psl_fsm_t *psl_fsm_new(psl_node_t p, ident_t label)
{
   psl_fsm_t *fsm = xcalloc(sizeof(psl_fsm_t));
   fsm->pool  = pool_new();
   fsm->label = label;
   fsm->tail  = &(fsm->states);
   fsm->src   = p;
   fsm->kind  = psl_kind(p) == P_COVER ? FSM_COVER : FSM_BARE;

   fsm_state_t *initial = add_state(fsm, p), *final = initial;
   initial->initial = true;

   final = build_node(fsm, initial, psl_value(p));
   final->accept = true;

   DEBUG_ONLY(psl_detect_loops(fsm));

   const bool verbose = opt_get_verbose(OPT_PSL_VERBOSE, istr(label));
   if (verbose)
      psl_fsm_dump(fsm, "initial");

   psl_simplify(fsm);
   psl_prune(fsm);

   if (verbose)
      psl_fsm_dump(fsm, "final");

   return fsm;
}

void psl_fsm_free(psl_fsm_t *fsm)
{
   pool_free(fsm->pool);
   free(fsm);
}

bool psl_fsm_repeating(psl_fsm_t *fsm)
{
   return fsm->kind == FSM_COVER || fsm->kind == FSM_ALWAYS
      || fsm->kind == FSM_NEVER;
}

guard_kind_t psl_guard_kind(psl_guard_t g)
{
   const unsigned tag = pointer_tag(g);
   switch (tag) {
   case GUARD_EXPR:
   case GUARD_BINOP:
   case GUARD_NOT:
   case GUARD_FALSE:
      return tag;
   default:
      should_not_reach_here();
   }
}

const guard_binop_t *psl_guard_binop(psl_guard_t g)
{
   assert(psl_guard_kind(g) == GUARD_BINOP);
   return untag_pointer(g, guard_binop_t);
}

psl_node_t psl_guard_expr(psl_guard_t g)
{
   switch (pointer_tag(g)) {
   case GUARD_EXPR:
   case GUARD_NOT:
      return untag_pointer(g, struct _psl_node);
   default:
      should_not_reach_here();
   }
}

// LCOV_EXCL_START /////////////////////////////////////////////////////////////

static void psl_dump_guard(psl_guard_t g, text_buf_t *tb, bool nested)
{
   switch (psl_guard_kind(g)) {
   case GUARD_NOT:
      tb_cat(tb, "!");
      // Fall-through
   case GUARD_EXPR:
      {
         psl_node_t p = psl_guard_expr(g);
         assert(psl_kind(p) == P_HDL_EXPR);

         tree_t t = psl_tree(p);

         const bool is_cconv = tree_kind(t) == T_FCALL
            && is_well_known(tree_ident(t)) == W_OP_CCONV;

         vhdl_dump(is_cconv ? tree_param(t, 0) : t, 0);
      }
      break;
   case GUARD_BINOP:
      {
         if (nested) tb_append(tb, '(');

         const guard_binop_t *bop = psl_guard_binop(g);
         psl_dump_guard(bop->left, tb, true);

         switch (bop->kind) {
         case BINOP_AND: tb_cat(tb, " && "); break;
         case BINOP_OR: tb_cat(tb, " || "); break;
         default: should_not_reach_here();
         }

         psl_dump_guard(bop->right, tb, true);

         if (nested) tb_append(tb, ')');
      }
      break;
   case GUARD_FALSE:
      tb_cat(tb, "false");
      break;
   default:
      should_not_reach_here();
   }
}

static void psl_dump_label(FILE *f, psl_guard_t g)
{
   LOCAL_TEXT_BUF tb = tb_new();
   capture_syntax(tb);
   psl_dump_guard(g, tb, false);

   for (const char *p = tb_get(tb); *p; p++) {
      if (*p == '"')
         fputs("\\\"", f);
      else
         fputc(*p, f);
   }

   capture_syntax(NULL);
}

void psl_fsm_dump(psl_fsm_t *fsm, const char *tag)
{
#ifdef HAVE_POPEN
   char *cmd LOCAL = xasprintf("dot -Tsvg -o %s.%s.svg", istr(fsm->label), tag);
   FILE *f = popen(cmd, "w");
   if (f == NULL)
      fatal_errno("failed to start dot process");

   fprintf(f, "digraph psl {\n");

   for (fsm_state_t *s = fsm->states; s; s = s->next) {
      if (s->accept || s->initial) {
         fprintf(f, "%d [", s->id);

         if (s->accept) fprintf(f, "peripheries=2,");
         if (s->initial) fprintf(f, "style=bold,");

         if (s->guard != NULL) {
            fprintf(f, "label=\"%d: ", s->id);
            psl_dump_label(f, s->guard);
            fputs("\",", f);
         }

         fputs("];\n", f);
      }
#ifdef DEBUG
      else if (s->mergekey != 0)
         fprintf(f, "%d [label=\"%d (%d ∩ %d)\"]", s->id, s->id,
                 (int)(s->mergekey >> 32), (int)s->mergekey);
#endif

      for (fsm_edge_t *e = s->edges; e; e = e->next) {
         fprintf(f, "%d -> %d [", s->id, e->dest->id);
         if (e->guard != NULL) {
            fprintf(f, "label=\"");
            psl_dump_label(f, e->guard);
            fputs("\",", f);
         }
         if (e->kind == EDGE_EPSILON)
            fputs("style=dashed,", f);
         fprintf(f, "];\n");
      }
   }

   fprintf(f, "}\n");

   if (pclose(f) != 0)
      fatal_errno("dot process failed");

   debugf("wrote PSL state machine graph to %s.%s.svg", istr(fsm->label), tag);
#else
   warnf("PSL state machine graph is not supported on this platform");
#endif
}

// LCOV_EXCL_STOP //////////////////////////////////////////////////////////////
