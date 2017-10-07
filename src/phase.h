//
//  Copyright (C) 2011-2017  Nick Gasson
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

typedef enum {
   EVAL_BOUNDS  = (1 << 0),
   EVAL_FCALL   = (1 << 1),
   EVAL_WARN    = (1 << 2),
   EVAL_VERBOSE = (1 << 4),
   EVAL_REPORT  = (1 << 5),
   EVAL_FOLDING = (1 << 6),
   EVAL_LOWER   = (1 << 7)
} eval_flags_t;

// Annotate types and perform other semantics checks on a tree.
// Returns false on error.
bool sem_check(tree_t t);

// The number of errors found during the semantic check phase.
int sem_errors(void);
void reset_sem_errors(void);

// The number of errors found while constant folding
int eval_errors(void);

// Rewrite to simpler forms
void simplify(tree_t top, eval_flags_t flags);

// Perform static bounds checking
void bounds_check(tree_t top);

// Number of errors found during bounds checking
int bounds_errors(void);
void reset_bounds_errors(void);

// Evaluate a function call at compile time
tree_t eval(tree_t fcall, eval_flags_t flags);

// Elaborate a top level entity
tree_t elab(tree_t top);

// Set the value of a top-level generic
void elab_set_generic(const char *name, const char *value);

// Generate LLVM bitcode for a design unit
void cgen(tree_t top, vcode_unit_t vu);

// Dump out a VHDL representation of the given unit
void dump(tree_t top);

// Print out the interconnect nets in an elaborated design
void dump_nets(tree_t top);

// Groups nets which never have sub-elements assigned.
void group_nets(tree_t top);

// Generate a makefile for the givein unit
void make(tree_t *targets, int count, FILE *out);

// Set parser input file
void input_from_file(const char *file);

// Read the next unit from the input file
tree_t parse(void);

// Number of errors found while parsing last unit
int parse_errors(void);
void reset_parse_errors(void);

// Generate vcode for a design unit
vcode_unit_t lower_unit(tree_t unit);

// Generate vcode for an isolated function call
vcode_unit_t lower_thunk(tree_t fcall);

// Lower an isolated function body
vcode_unit_t lower_func(tree_t body);

// Compute the mangled name of a subprogram
ident_t lower_mangle_package_name(tree_t decl);

#endif  // _PHASE_H
