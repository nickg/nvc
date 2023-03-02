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

#ifndef _LOWER_H
#define _LOWER_H

#include <prim.h>

lower_unit_t *lower_unit_new(lower_unit_t *parent, vcode_unit_t vunit,
                             cover_tagging_t *cover, tree_t container);
void lower_unit_free(lower_unit_t *lu);

void lower_standalone_unit(tree_t unit);
lower_unit_t *lower_instance(lower_unit_t *parent, cover_tagging_t *cover,
                             tree_t block);
void lower_process(lower_unit_t *parent, tree_t proc);
vcode_unit_t lower_thunk(lower_unit_t *parent, tree_t fcall);

#endif  // _LOWER_H
