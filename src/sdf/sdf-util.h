//
//  Copyright (C) 2022 Nick Gasson
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

#ifndef _SDF_UTIL_H
#define _SDF_UTIL_H

#include "prim.h"
#include "object.h"
#include "sdf-node.h"
#include "common.h"
#include "hash.h"

sdf_node_t sdf_get_cell_from_hash(sdf_file_t *sdf_file, ident_t hier, ident_t cell_type);
void sdf_put_cell_to_hash(sdf_file_t *sdf_file, sdf_node_t cell, int unsigned index);

void sdf_save_file(sdf_file_t *sdf_file, const char *file);
sdf_file_t* sdf_load_file(const char *file);

sdf_file_t *sdf_file_new(int exp_hier_cells, int exp_wild_cells);
void sdf_file_free(sdf_file_t *sdf);

#endif  // _SDF_UTIL_H
