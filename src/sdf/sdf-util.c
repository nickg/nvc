// -*- mode: bison; c-basic-offset: 3 -*-
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

#include <stdlib.h>

#include "hash.h"
#include "fbuf.h"
#include "ident.h"
#include "sdf/sdf-node.h"
#include "sdf/sdf-util.h"

#define COMPILED_SDF_FILE_MAGIC 0x6E637364   // ASCII "ncsd"
#define COMPILED_SDF_FILE_VERSION 1


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

sdf_node_t sdf_get_cell_from_hash(sdf_file_t *sdf_file, ident_t hier,
                                  ident_t cell_type)
{
   // Hierarhcy a wildcard -> Search by cell name
   if (icmp(hier, "*")) {
      unsigned long index = (unsigned long) hash_get(sdf_file->name_map,
                                                     (void *) cell_type);
      if (index)
         return sdf_cell(sdf_file->root, index - 1);
   }
   // Hierarchy not a wildcard -> Search by hierarchy
   else {
      // TODO: Handle type size difference between hash value and
      //       sdf_cell array index
      unsigned long index = (unsigned long) hash_get(sdf_file->hier_map,
                                                     (void *) hier);
      if (index)
         return sdf_cell(sdf_file->root, index - 1);
   }

   return NULL;
}

void sdf_put_cell_to_hash(sdf_file_t *sdf_file, sdf_node_t cell, int unsigned index)
{
   assert(sdf_kind(cell) == S_CELL);
   assert(sdf_has_ident(cell));

   ident_t hier = sdf_has_ident2(cell) ? sdf_ident2(cell) : NULL;
   ident_t cell_type = sdf_ident(cell);

   // TODO: handle type difference
   unsigned long l_index = (unsigned long) index;

   // Wildcard -> Hash by cell name (CELLTYPE)
   if (icmp(hier, "*"))
      hash_put(sdf_file->name_map, (void *) cell_type, (void *) l_index);
   // No wildcard -> Hash by hierarchy (INSTANCE [hierarchy])
   else
      hash_put(sdf_file->hier_map, (void *) hier, (void *) l_index);
}

void sdf_save_file(sdf_file_t *sdf_file, const char *file)
{
   // TODO: Add checksum ?
   fbuf_t *f = fbuf_open(file, FBUF_OUT, FBUF_CS_NONE);

   loc_wr_ctx_t *loc_wr_ctx = loc_write_begin(f);
   ident_wr_ctx_t ident_wr_ctx = ident_write_begin(f);

   write_u32(COMPILED_SDF_FILE_MAGIC, f);
   write_u32(COMPILED_SDF_FILE_VERSION, f);

   // Serialize SDF tree
   // TODO: Should we freeze right after parse?
   freeze_global_arena();
   object_write(sdf_to_object(sdf_file->root), f, ident_wr_ctx, loc_wr_ctx);

   write_u32(sdf_file->std, f);
   write_double(sdf_file->unit_mult, f);
   write_u8(sdf_file->hchar, f);
   write_u8(sdf_file->hchar_other, f);
   write_u32(sdf_file->min_max_spec, f);

   fbuf_close(f, NULL);
}

sdf_file_t *sdf_load_file(const char *file)
{
   // TODO: Add checksum ?
   fbuf_t *f = fbuf_open(file, FBUF_IN, FBUF_CS_NONE);

   loc_rd_ctx_t *loc_rd_ctx = loc_read_begin(f);
   ident_rd_ctx_t ident_rd_ctx = ident_read_begin(f);

   int unsigned magic = read_u32(f);
   if (magic != COMPILED_SDF_FILE_MAGIC) {
      fatal("%s is not a compiled SDF file", file);
      return NULL;
   }

   int unsigned version = read_u32(f);
   if (version != COMPILED_SDF_FILE_VERSION) {
      fatal("%s compiled SDF file was written with version %d, "
            "current SDF file version is %d", file, version,
            COMPILED_SDF_FILE_VERSION);
      return NULL;
   }

   // TODO: Define "object_load_fn_t loader"
   object_t *obj = object_read(f, NULL, ident_rd_ctx, loc_rd_ctx);
   sdf_node_t root = sdf_from_object(obj);

   // Assume all cells are non-wildcards -> True in most of SDF files
   // Wildcard instance used rarely!
   sdf_file_t *sdf_file = sdf_file_new(sdf_cells(root), 256);
   sdf_file->root = root;

   sdf_file->std = read_u32(f);
   sdf_file->unit_mult = read_double(f);
   sdf_file->hchar = read_u8(f);
   sdf_file->hchar_other = read_u8(f);
   sdf_file->min_max_spec = read_u32(f);

   // Re-create the hash maps from cells.
   int unsigned n_cells = sdf_cells(root);
   for (int i = 0; i < n_cells; i++) {
      sdf_node_t cell = sdf_cell(root, i);

      // Check there are no duplicates -> Should be ensured by parser
      assert(sdf_get_cell_from_hash(sdf_file,
                                    sdf_has_ident2() ? sdf_ident2(cell) : NULL,
                                    sdf_ident(cell)));

      sdf_put_cell_to_hash(sdf_file, cell, i + 1);
   }

   fbuf_close(f, NULL);

   return sdf_file;
}