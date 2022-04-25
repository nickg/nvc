//
//  Copyright (C) 2022-2023  Nick Gasson
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

#ifndef _PSL_PHASE_H
#define _PSL_PHASE_H

#include "prim.h"

// Check PSL statements for errors.
void psl_check(psl_node_t p);

// Print PSL expression for debugging.
void psl_dump(psl_node_t p);

// Convert PSL to vcode unit.
void psl_lower(lower_unit_t *parent, psl_node_t p, ident_t name);

#endif  // _PSL_PHASE_H
