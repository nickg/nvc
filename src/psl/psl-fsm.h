//
//  Copyright (C) 2023  Nick Gasson
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

#ifndef _PSL_FSM_H
#define _PSL_FSM_H

#include "prim.h"

typedef struct _psl_fsm psl_fsm_t;

psl_fsm_t *psl_fsm_new(psl_node_t p);
void psl_fsm_free(psl_fsm_t *fsm);
void psl_fsm_dump(psl_fsm_t *fsm, const char *fname);

#endif  // _PSL_FSM_H
