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

#ifndef _PHASE_H
#define _PHASE_H

#include "tree.h"

// Annotate types and perform other semantics checks on a tree.
// Returns false on error.
bool sem_check(tree_t t);

// The number of errors found during the semantic check phase.
int sem_errors(void);

// Enable special mode for analysing STANDARD package
void sem_bootstrap_en(bool en);

// Fold all constant expressions
void simplify(tree_t top);

// Number of errors found during simplification
int simplify_errors(void);

// Find all drivers associated with signals
void driver_extract(tree_t top);

// Number of errors found during driver extraction
int driver_errors(void);

// Elaborate a top level entity
tree_t elab(tree_t top);

// Generate LLVM bitcode for an elaborated design
void cgen(tree_t top);

// Toggle LLVM optimisations on and off
void cgen_optimise_en(bool en);

// Dump out a VHDL representation of the given unit
void dump(tree_t top);

#endif  // _PHASE_H
