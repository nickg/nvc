//
//  Copyright (C) 2024  Nick Gasson
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

#include "util.h"
#include "hash.h"
#include "sdf/sdf-util.h"

#include <stdlib.h>

sdf_file_t *sdf_file_new(int exp_hier_cells, int exp_wild_cells)
{
   sdf_file_t *sdf_file = xcalloc(sizeof(struct _sdf_file));

   sdf_file->hier_map = hash_new(exp_hier_cells);
   sdf_file->name_map = hash_new(exp_wild_cells);

   return sdf_file;
}

void sdf_file_free(sdf_file_t *sdf_file)
{
   hash_free(sdf_file->name_map);
   hash_free(sdf_file->hier_map);

   free(sdf_file);
}
