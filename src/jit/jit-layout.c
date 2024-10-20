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
#include "thread.h"
#include "type.h"

#include <assert.h>

static int count_sub_elements(type_t type)
{
   if (type_is_array(type)) {
      if (type_is_unconstrained(type))
         return -1;
      else {
         const int ndims = dimension_of(type);

         int length = count_sub_elements(type_elem(type));
         for (int i = 0; i < ndims; i++) {
            tree_t r = range_of(type, i);

            int64_t dlen;
            if (!folded_length(r, &dlen))
               return -1;

            length *= dlen;
         }

         return length;
      }
   }
   else
      return 1;
}

const jit_layout_t *layout_of(type_t type)
{
   assert(type_frozen(type));   // Not safe to cache otherwise
   static hash_t *cache = NULL;

   INIT_ONCE(cache = hash_new(256));

   jit_layout_t *l = hash_get(cache, type);
   if (l != NULL)
      return l;

   if (type_is_integer(type) || type_is_physical(type) || type_is_enum(type)) {
      l = xcalloc_flex(sizeof(jit_layout_t), 1, sizeof(layout_part_t));
      l->nparts = 1;

      type_t base = type_base_recur(type);

      tree_t r = type_dim(base, 0);
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
      const int nelems = count_sub_elements(type);

      if (nelems < 0) {
         l = xcalloc_flex(sizeof(jit_layout_t), 2, sizeof(layout_part_t));
         l->nparts = 2;
         l->size   = sizeof(void *) + ndims * 2 * sizeof(int64_t);
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
         type_t elem = type_elem_recur(type);
         const jit_layout_t *el = layout_of(elem);

         l = xcalloc_flex(sizeof(jit_layout_t), 1, sizeof(layout_part_t));
         l->nparts = 1;
         l->size   = nelems * el->size;
         l->align  = el->align;

         l->parts[0].offset = 0;
         l->parts[0].size   = el->size;
         l->parts[0].repeat = nelems;
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

const jit_layout_t *signal_layout_of(type_t type)
 {
   assert(type_frozen(type));   // Not safe to cache otherwise
   static hash_t *cache = NULL;

   INIT_ONCE(cache = hash_new(256));

   jit_layout_t *l = hash_get(cache, type);
   if (l != NULL)
      return l;

   if (type_is_scalar(type)) {
      l = xcalloc_flex(sizeof(jit_layout_t), 2, sizeof(layout_part_t));
      l->nparts = 2;
      l->size   = 16;
      l->align  = 8;

      // Shared signal data pointer
      l->parts[0].offset = 0;
      l->parts[0].size   = sizeof(void *);
      l->parts[0].repeat = 1;
      l->parts[0].align  = sizeof(void *);

      // Offset
      l->parts[1].offset = 8;
      l->parts[1].size   = 8;
      l->parts[1].repeat = 1;
      l->parts[1].align  = 8;
   }
   else if (type_is_array(type)) {
      const bool has_offset = type_is_homogeneous(type);
      const bool has_bounds = !type_const_bounds(type);
      const int ndims = dimension_of(type);
      const int nparts = 1 + has_offset + has_bounds;

      l = xcalloc_flex(sizeof(jit_layout_t), nparts, sizeof(layout_part_t));
      l->nparts = nparts;
      l->align  = sizeof(void *);

      layout_part_t *p = l->parts;

      // Pointer to signal or record data
      p->offset = 0;
      p->size   = sizeof(void *);
      p->repeat = 1;
      p->align  = sizeof(void *);

      l->size += p->size * p->repeat;
      p++;

      if (has_offset) {
         p->offset = ALIGN_UP(l->size, 8);
         p->size   = 8;
         p->repeat = 1;
         p->align  = 8;

         l->size += p->size * p->repeat;
         p++;
      }

      if (has_bounds) {
         p->offset = ALIGN_UP(l->size, 8);
         p->size   = 8;
         p->repeat = ndims * 2;
         p->align  = 8;

         l->size += p->size * p->repeat;
         p++;
      }
   }
   else if (type_is_record(type)) {
      const int nfields = type_fields(type);

      l = xcalloc_flex(sizeof(jit_layout_t), nfields, sizeof(layout_part_t));
      l->nparts = nfields;

      int offset = 0;
      for (int i = 0; i < nfields; i++) {
         type_t ftype = tree_type(type_field(type, i));
         const jit_layout_t *fl = signal_layout_of(ftype);

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
