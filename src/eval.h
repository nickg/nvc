//
//  Copyright (C) 2021-2023  Nick Gasson
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

#ifndef _EVAL_H
#define _EVAL_H

#include "prim.h"
#include "phase.h"

tree_t eval_try_fold(jit_t *jit, tree_t expr, unit_registry_t *registry,
                     lower_unit_t *parent, void *context);
tree_t eval_must_fold(jit_t *jit, tree_t expr, unit_registry_t *registry,
                      lower_unit_t *parent, void *context);
bool eval_possible(tree_t t, unit_registry_t *ur);
tree_t eval_case(jit_t *jit, tree_t stmt, lower_unit_t *parent, void *context);
void *eval_instance(jit_t *jit, ident_t name, void *context);

#endif  // _EVAL_H
