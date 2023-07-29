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

#include "util.h"
#include "common.h"
#include "hash.h"
#include "jit/jit-layout.h"
#include "type.h"

#include <assert.h>

const jit_layout_t *layout_of(type_t type)
{
   assert(type_frozen(type));   // Not safe to cache otherwise
   static hash_t *cache = NULL;

   if (cache == NULL)
      cache = hash_new(256);

   jit_layout_t *l = hash_get(cache, type);
   if (l != NULL)
      return l;

   if (type_is_integer(type) || type_is_physical(type) || type_is_enum(type)) {
      l = xcalloc_flex(sizeof(jit_layout_t), 1, sizeof(layout_part_t));
      l->nparts = 1;

      tree_t r = type_dim(type, 0);
      int64_t low, high;
      if (!folded_bounds(r, &low, &high))
         fatal_trace("type %s has unknown bounds", type_pp(type));

      const int bits = bits_for_range(low, high);
      l->parts[0].offset = 0;
      l->parts[0].size   = l->size = ALIGN_UP(bits, 8) / 8;
      l->parts[0].repeat = 1;
      l->parts[0].align  = l->align = l->parts[0].size;
   }
   else if (type_is_real(type)) {
      l = xcalloc_flex(sizeof(jit_layout_t), 1, sizeof(layout_part_t));
      l->nparts = 1;

      l->parts[0].offset = 0;
      l->parts[0].size   = l->size = sizeof(double);
      l->parts[0].repeat = 1;
      l->parts[0].align  = l->align = l->parts[0].size;
   }
   else if (type_is_array(type)) {
      const int ndims = dimension_of(type);

      if (type_is_unconstrained(type)) {
         l = xcalloc_flex(sizeof(jit_layout_t), 2, sizeof(layout_part_t));
         l->nparts = 2;
         l->size   = sizeof(void *) + ndims * 2 * sizeof(int32_t);
         l->align  = sizeof(void *);

         l->parts[0].offset = 0;
         l->parts[0].size   = sizeof(void *);
         l->parts[0].repeat = 1;
         l->parts[0].align  = l->parts[0].size;

         l->parts[1].offset = sizeof(void *);
         l->parts[1].size   = sizeof(int64_t);
         l->parts[1].repeat = ndims * 2;
         l->parts[1].align  = l->parts[1].size;
      }
      else {
         int length = 1;
         for (int i = 0; i < ndims; i++) {
            tree_t r = range_of(type, i);

            int64_t dlen;
            if (!folded_length(r, &dlen))
               fatal_at(tree_loc(r), "dimension %d of type %s is not static",
                        i, type_pp(type));

            length *= dlen;
         }

         const jit_layout_t *el = layout_of(type_elem(type));
         assert(!type_is_array(type_elem(type)));

         l = xcalloc_flex(sizeof(jit_layout_t), 1, sizeof(layout_part_t));
         l->nparts = 1;
         l->size   = length * el->size;
         l->align  = el->align;

         l->parts[0].offset = 0;
         l->parts[0].size   = el->size;
         l->parts[0].repeat = length;
         l->parts[0].align  = el->align;
      }
   }
   else if (type_is_record(type)) {
      const int nfields = type_fields(type);

      l = xcalloc_flex(sizeof(jit_layout_t), nfields, sizeof(layout_part_t));
      l->nparts = nfields;

      int offset = 0;
      for (int i = 0; i < nfields; i++) {
         type_t ftype = tree_type(type_field(type, i));
         const jit_layout_t *fl = layout_of(ftype);

         offset = ALIGN_UP(offset, fl->align);

         l->parts[i].offset = offset;
         l->parts[i].size   = fl->size;
         l->parts[i].repeat = 1;
         l->parts[i].align  = fl->align;

         offset += fl->size;
      }

      l->size  = offset;
      l->align = sizeof(void *);  // Matches irgen_align_of
   }
   else
      fatal_trace("cannot get layout for %s", type_pp(type));

   hash_put(cache, type, l);
   return l;
}
