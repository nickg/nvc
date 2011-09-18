//
//  Copyright (C) 2011  Nick Gasson
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

#ifndef _SEM_H
#define _SEM_H

#include "tree.h"

// Annotate types and perform other semantics checks on a tree.
// Returns false on error.
bool sem_check(tree_t t);

// The number of errors found during the semantic check phase.
int sem_errors(void);

// Error callback for use in unit tests.
typedef void (*sem_error_fn_t)(const char *msg, const loc_t *loc);
sem_error_fn_t sem_set_error_fn(sem_error_fn_t fn);

// Enable special mode for analysing STANDARD package
void sem_bootstrap_en(bool en);

#endif  // _SEM_H
