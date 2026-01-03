//
//  Copyright (C) 2025-2026 Nick Gasson
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

#ifndef _VHDL_PHASE_H
#define _VHDL_PHASE_H

#include "prim.h"

tree_t vhdl_component_instance(tree_t comp, tree_t inst, ident_t dotted);
tree_t vhdl_architecture_instance(tree_t arch, tree_t inst, ident_t dotted);
tree_t vhdl_config_instance(tree_t conf, tree_t bind, ident_t dotted);

cover_scope_t *vhdl_cover_block(tree_t block, cover_data_t *db,
                                cover_scope_t *parent);

#endif  // _VHDL_PHASE_H
