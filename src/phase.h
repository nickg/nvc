//
//  Copyright (C) 2011-2023  Nick Gasson
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
void simplify_local(tree_t top, jit_t *jit, unit_registry_t *ur);

// Rewrite to simpler forms folding globally static expressions
void simplify_global(tree_t top, hash_t *generics, jit_t *jit,
                     unit_registry_t *ur);

// Perform static bounds checking
void bounds_check(tree_t top);

// Elaborate a top level entity
tree_t elab(tree_t top, jit_t *jit, unit_registry_t *ur, cover_data_t *cover);

// Elaborate a Verilog top level
tree_t elab_verilog(vlog_node_t top, jit_t *jit, unit_registry_t *ur,
                    cover_data_t *cover);

// Set the value of a top-level generic
void elab_set_generic(const char *name, const char *value);

// Generate LLVM bitcode for a design unit
void cgen(tree_t top, unit_registry_t *ur, jit_t *jit);

// Generate ahead-of-time preload library
void aotgen(const char *outfile, char **argv, int argc);

// Dump out a VHDL representation of the given unit
void dump(tree_t top);

// Dump VHDL node with indentation no trailing newline
void vhdl_dump(tree_t t, int indent);

// Generate a makefile for the givein unit
void make(tree_t *targets, int count, FILE *out);

// Read the next unit from the input file
tree_t parse(void);

#endif  // _PHASE_H
