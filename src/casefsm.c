//
//  Copyright (C) 2015-2021  Nick Gasson
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
#include "casefsm.h"
#include "tree.h"
#include "common.h"

#include <inttypes.h>
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>

#define WRITE_DOT 0

struct __case_fsm {
   case_state_t  *root;
   case_state_t **tailp;
   unsigned       nextid;
   unsigned       maxarcs;
   unsigned       maxdepth;
};

static int64_t lower_case_find_choice_element(tree_t value, int depth)
{
   switch (tree_kind(value)) {
   case T_LITERAL:
      {
         assert(tree_subkind(value) == L_STRING);
         tree_t ch = tree_char(value, depth);
         return tree_pos(tree_ref(ch));
      }
      break;

   case T_AGGREGATE:
      {
         const int nassocs = tree_assocs(value);
         type_t type = tree_type(value);

         for (int i = 0; i < nassocs; i++) {
            tree_t a = tree_assoc(value, i);
            switch (tree_subkind(a)) {
            case A_NAMED:
               if (rebase_index(type, 0, assume_int(tree_name(a))) == depth)
                  return assume_int(tree_value(a));
               break;

            case A_POS:
               if (tree_pos(a) == (unsigned)depth)
                  return assume_int(tree_value(a));
               break;

            case A_OTHERS:
               return assume_int(tree_value(a));
            }
         }
      }
      break;

   case T_REF:
      {
         tree_t decl = tree_ref(value);
         assert(tree_kind(decl) == T_CONST_DECL);
         return lower_case_find_choice_element(tree_value(decl), depth);
      }
      break;

   case T_ARRAY_SLICE:
      {
         tree_t base = tree_value(value);
         tree_t r = tree_range(value, 0);
         const int64_t rleft = assume_int(tree_left(r));
         const int64_t offset = rebase_index(tree_type(base), 0, rleft);
         return lower_case_find_choice_element(base, depth + offset);
      }

   case T_FCALL:
      if (tree_subkind(tree_ref(value)) == S_CONCAT) {
         const int nparams = tree_params(value);
         for (int i = 0; i < nparams; i++) {
            tree_t left = tree_value(tree_param(value, i));

            tree_t lr = range_of(tree_type(left), 0);
            int64_t left_len;
            if (!folded_length(lr, &left_len))
               fatal_at(tree_loc(left), "cannot determine length of left hand "
                        "side of concatenation");

            if (depth < left_len || i + 1 == nparams)
               return lower_case_find_choice_element(left, depth);

            depth -= left_len;
         }
      }
      // Fall-through

   default:
      fatal_at(tree_loc(value), "unsupported tree type %s in case choice",
               tree_kind_str(tree_kind(value)));
   }

   fatal_at(tree_loc(value), "cannot find element %d in choice", depth);
}

static case_state_t *case_fsm_alloc_state(case_fsm_t *fsm)
{
   case_state_t *new =
      xcalloc(sizeof(case_state_t) + fsm->maxarcs * sizeof(case_arc_t));
   new->id = fsm->nextid++;

   *(fsm->tailp) = new;
   fsm->tailp = &(new->next);

   return new;
}

static void lower_case_add_branch(case_fsm_t *fsm, case_state_t *where,
                                  int left, int right, int depth, int dirmul,
                                  tree_t value, tree_t stmts)
{
   const int n = left + (depth * dirmul);

   if ((dirmul == -1 && n < right) || (dirmul == 1 && n > right)) {
      if (where->stmts != NULL)
         fatal_at(tree_loc(value), "duplicate choice in case statement");

      assert(where->narcs == 0);
      where->stmts = stmts;
   }
   else {
      const int64_t this = lower_case_find_choice_element(value, depth);

      for (int i = 0; i < where->narcs; i++) {
         if (where->arcs[i].value == this) {
            lower_case_add_branch(fsm, where->arcs[i].next,
                                  left, right, depth + 1, dirmul, value, stmts);
            return;
         }
      }

      case_state_t *next = case_fsm_alloc_state(fsm);
      next->depth = depth + 1;

      fsm->maxdepth = MAX(next->depth, fsm->maxdepth);

      assert(where->narcs < fsm->maxarcs);
      case_arc_t *arc = &(where->arcs[(where->narcs)++]);
      arc->value = this;
      arc->next  = next;

      lower_case_add_branch(fsm, next, left, right, depth + 1,
                            dirmul, value, stmts);
   }
}

#if WRITE_DOT
static void case_fsm_write_dot_for_state(FILE *f, case_state_t *where)
{
   if (where->stmts != NULL)
      fprintf(f, "%d [peripheries=2];\n", where->id);
   else {
      for (int i = 0; i < where->narcs; i++) {
         case_arc_t *arc = &(where->arcs[i]);
         fprintf(f, "%d -> %d [label=\"%"PRIi64"\"];\n", where->id,
                 arc->next->id, arc->value);
         case_fsm_write_dot_for_state(f, arc->next);
      }
   }
}

static void case_fsm_write_dot(case_fsm_t *fsm)
{
   FILE *f = fopen("/tmp/case.dot", "w");
   fprintf(f, "digraph case {\n");

   case_fsm_write_dot_for_state(f, &(fsm->root));

   fprintf(f, "}\n");
   fclose(f);
}
#endif  // WRITE_DOT

case_fsm_t *case_fsm_new(tree_t stmt)
{
   case_fsm_t *fsm = xcalloc(sizeof(case_fsm_t));

   type_t type = tree_type(tree_value(stmt));
   assert(type_is_array(type));

   type_t elem = type_elem(type);
   assert(type_is_enum(elem));

   const int nlits   = type_enum_literals(type_base_recur(elem));
   const int nassocs = tree_assocs(stmt);

   fsm->maxarcs = MIN(nlits, nassocs);
   fsm->tailp   = &(fsm->root);
   fsm->root    = case_fsm_alloc_state(fsm);

   tree_t r = range_of(type, 0);
   const int64_t left  = assume_int(tree_left(r));
   const int64_t right = assume_int(tree_right(r));
   const int dirmul = (tree_subkind(r) == RANGE_DOWNTO) ? -1 : 1;

   for (int i = 0; i < nassocs; i++) {
      tree_t a = tree_assoc(stmt, i);
      switch (tree_subkind(a)) {
      case A_NAMED:
         lower_case_add_branch(fsm, fsm->root, left, right, 0, dirmul,
                               tree_name(a), tree_value(a));
         break;

      case A_OTHERS:
         break;

      default:
         assert(false);
      }
   }

#if WRITE_DOT
   case_fsm_write_dot(fsm);
#endif

   return fsm;
}

void case_fsm_free(case_fsm_t *fsm)
{
   for (case_state_t *it = fsm->root; it != NULL; ) {
      case_state_t *tmp = it->next;
      free(it);
      it = tmp;
   }

   free(fsm);
}

case_state_t *case_fsm_root(case_fsm_t *fsm)
{
   return fsm->root;
}

unsigned case_fsm_count_states(case_fsm_t *fsm)
{
   return fsm->nextid;
}

unsigned case_fsm_max_depth(case_fsm_t *fsm)
{
   return fsm->maxdepth;
}

unsigned case_fsm_max_arcs(case_fsm_t *fsm)
{
   return fsm->maxarcs;
}
