//
//  Copyright (C) 2013  Nick Gasson
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

#ifndef _COMMON_H
#define _COMMON_H

#include "tree.h"

//
// Various utility functions
//

int64_t assume_int(tree_t t);
void range_bounds(range_t r, int64_t *low, int64_t *high);
tree_t call_builtin(const char *builtin, type_t type, ...);
bool folded_int(tree_t t, int64_t *i);
bool folded_real(tree_t t, double *d);
bool folded_bool(tree_t t, bool *b);
bool folded_length(range_t r, int64_t *l);
bool folded_enum(tree_t t, unsigned *pos);
tree_t get_int_lit(tree_t t, int64_t i);
tree_t get_real_lit(tree_t t, double r);
const char *package_signal_path_name(ident_t i);

//
// Utility typedefs
//

typedef unsigned (*tree_formals_t)(tree_t t);
typedef tree_t (*tree_formal_t)(tree_t t, unsigned n);
typedef unsigned (*tree_actuals_t)(tree_t t);
typedef tree_t (*tree_actual_t)(tree_t t, unsigned n);

#endif  // _COMMON_H
