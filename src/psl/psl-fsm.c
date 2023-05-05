//
//  Copyright (C) 2023 Nick Gasson
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
#include "common.h"
#include "ident.h"
#include "psl/psl-fsm.h"
#include "psl/psl-node.h"
#include "psl/psl-phase.h"

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>

#define CANNOT_HANDLE(p) do {                                   \
      psl_dump(p);                                              \
      printf("\n");                                             \
      fatal_trace("cannot handle PSL kind %s in %s",            \
                  psl_kind_str(psl_kind(p)),  __FUNCTION__);    \
   } while (0)

static void build_node(psl_fsm_t *fsm, fsm_state_t *state, psl_node_t p);

static fsm_state_t *add_state(psl_fsm_t *fsm)
{
   fsm_state_t *s = xcalloc(sizeof(fsm_state_t));
   s->id = fsm->next_id++;

   *(fsm->tail) = s;
   fsm->tail = &(s->next);

   return s;
}

static void add_edge(fsm_state_t *from, fsm_state_t *to, edge_kind_t kind,
                     psl_node_t guard)
{
   fsm_edge_t *e = xcalloc(sizeof(fsm_edge_t));
   e->next  = from->edges;
   e->kind  = kind;
   e->dest  = to;
   e->guard = guard;

   from->edges = e;
}

static void build_implication(psl_fsm_t *fsm, fsm_state_t *state, psl_node_t p)
{
   psl_node_t rhs = psl_operand(p, 1);

   switch (psl_kind(rhs)) {
   case P_NEXT:
      {
         fsm_state_t *new = add_state(fsm);
         add_edge(state, new, EDGE_NEXT, psl_operand(p, 0));
         build_node(fsm, new, psl_value(rhs));
      }
      break;

   default:
      CANNOT_HANDLE(rhs);
   }
}

static void build_sere(psl_fsm_t *fsm, fsm_state_t *state, psl_node_t p)
{
   int ops = psl_operands(p);
   assert(ops > 0);

   build_node(fsm, state, psl_operand(p, 0));
   if (ops == 1)
      return;

   for (int i = 1; i < ops; i++) {
      psl_node_t rhs = psl_operand(p, i);
      switch (psl_subkind(p)) {
      case PSL_SERE_CONCAT:
         {
            fsm_state_t *new = add_state(fsm);
            add_edge(state, new, EDGE_NEXT, psl_value(rhs));
            build_node(fsm, new, rhs);
         }
         break;
      default:
         CANNOT_HANDLE(p);
      }
   }
}

static void build_node(psl_fsm_t *fsm, fsm_state_t *state, psl_node_t p)
{
   switch (psl_kind(p)) {
   case P_HDL_EXPR:
      assert(state->test == NULL);
      state->test   = p;
      state->accept = true;
      break;
   case P_SERE:
      build_sere(fsm, state, p);
      break;
   case P_IMPLICATION:
      build_implication(fsm, state, p);
      break;
   default:
      CANNOT_HANDLE(p);
   }
}

psl_fsm_t *psl_fsm_new(psl_node_t p)
{
   psl_fsm_t *fsm = xcalloc(sizeof(psl_fsm_t));
   fsm->tail = &(fsm->states);

   fsm_state_t *initial = add_state(fsm);
   initial->initial = true;

   switch (psl_kind(p)) {
   case P_ALWAYS:
      build_node(fsm, initial, psl_value(p));
      break;

   default:
      CANNOT_HANDLE(p);
   }

   return fsm;
}

void psl_fsm_free(psl_fsm_t *fsm)
{
   free(fsm);
}

static void psl_dump_label(FILE *f, psl_node_t p)
{
   LOCAL_TEXT_BUF tb = tb_new();
   capture_syntax(tb);

   psl_dump(p);

   for (const char *p = tb_get(tb); *p; p++) {
      if (*p == '"')
         fputs("\\\"", f);
      else
         fputc(*p, f);
   }

   capture_syntax(NULL);
}

void psl_fsm_dump(psl_fsm_t *fsm, const char *fname)
{
   FILE *f = fopen(fname, "w");
   if (f == NULL)
      fatal_errno("%s", fname);

   fprintf(f, "digraph psl {\n");

   for (fsm_state_t *s = fsm->states; s; s = s->next) {
      if (s->test != NULL || s->accept) {
         fprintf(f, "%d [", s->id);
         if (s->test != NULL) {
            fprintf(f, "label=\"%d: ", s->id);
            psl_dump_label(f, s->test);
            fputs(s->accept ? "\", " : "\"", f);
         }
         if (s->accept)
            fputs("peripheries=2", f);
         fputs("];\n", f);
      }

      for (fsm_edge_t *e = s->edges; e; e = e->next) {
         fprintf(f, "%d -> %d", s->id, e->dest->id);
         if (e->guard != NULL) {
            fprintf(f, " [label=\"");
            psl_dump_label(f, e->guard);
            fputs("\"]", f);
         }
         fprintf(f, ";\n");
      }
   }

   fprintf(f, "}\n");
   fclose(f);

   const char *args[] = {
      "/usr/bin/dot",
      "-Tsvg",
      "-O",
      fname,
      NULL
   };
   run_program(args);

   debugf("wrote PSL state machine graph to %s.svg", fname);
}
