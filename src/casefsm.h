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

#ifndef _CASEFSM_H
#define _CASEFSM_H

#include "prim.h"

typedef struct __case_fsm   case_fsm_t;
typedef struct __case_arc   case_arc_t;
typedef struct __case_state case_state_t;

// Connects case_state_t elements
struct __case_arc {
   unsigned      nvalues;
   case_state_t *next;
   union {
      int64_t  value;
      int64_t *values;
   } u;
};

// Decision tree used for array case statements
struct __case_state {
   tree_t        stmts;
   case_state_t *next;
   unsigned      id;
   unsigned      depth;
   unsigned      narcs;
   case_arc_t    arcs[0];
};

case_fsm_t *case_fsm_new(tree_t stmt);
case_state_t *case_fsm_root(case_fsm_t *fsm);
void case_fsm_free(case_fsm_t *fsm);
unsigned case_fsm_count_states(case_fsm_t *fsm);
unsigned case_fsm_max_depth(case_fsm_t *fsm);
unsigned case_fsm_max_arcs(case_fsm_t *fsm);

#endif   // _CASEFSM_H
