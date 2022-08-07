//
//  Copyright (C) 2011-2022  Nick Gasson
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

#include "prim.h"

#include <stdbool.h>
#include <stdio.h>

// Annotate types and perform other semantics checks on a tree.
// Returns false on error.
bool sem_check(tree_t t, nametab_t *tab);

// Rewrite to simpler forms folding locally static expressions
void simplify_local(tree_t top, eval_t *ex);

// Rewrite to simpler forms folding globally static expressions
void simplify_global(tree_t top, hash_t *generics, eval_t *ex);

// Perform static bounds checking
void bounds_check(tree_t top);

// Elaborate a top level entity
tree_t elab(tree_t top);

// Set the value of a top-level generic
void elab_set_generic(const char *name, const char *value);

// Generate LLVM bitcode for a design unit
void cgen(tree_t top, vcode_unit_t vu, cover_tagging_t *cover);

// Dump out a VHDL representation of the given unit
void dump(tree_t top);

// Generate a makefile for the givein unit
void make(tree_t *targets, int count, FILE *out);

// Read the next unit from the input file
tree_t parse(void);

// Generate vcode for a design unit
vcode_unit_t lower_unit(tree_t unit, cover_tagging_t *cover);

// Generate vcode for an isolated function call
vcode_unit_t lower_thunk(tree_t fcall);

#endif  // _PHASE_H
