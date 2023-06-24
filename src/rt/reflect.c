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

#include "util.h"
#include "jit/jit-exits.h"
#include "jit/jit.h"
#include "tree.h"
#include "type.h"

#include <assert.h>

typedef enum {
   CLASS_ENUMERATION,
   CLASS_INTEGER,
   CLASS_FLOATING,
   CLASS_PHYSICAL,
   CLASS_RECORD,
   CLASS_ARRAY,
   CLASS_ACCESS,
   CLASS_FILE,
   CLASS_PROTECTED
} reflect_class_t;

typedef struct _value_mirror value_mirror;

typedef struct {
   void         *context;
   int64_t       f_value;
   value_mirror *f_owner;
} integer_value_mirror_pt;

typedef struct {
   void                    *access;
   integer_value_mirror_pt  pt;
} integer_value_mirror;

typedef struct {
   void                 *context;
   uint8_t               f_class;
   integer_value_mirror *f_integer;
} value_mirror_pt;

typedef struct _value_mirror {
   void            *access;
   value_mirror_pt  pt;
} value_mirror;

void *x_reflect_value(void *context, jit_scalar_t value, tree_t where)
{
   type_t type = tree_type(where);

   value_mirror *vm = jit_mspace_alloc(sizeof(value_mirror));

   vm->access = &(vm->pt);
   vm->pt.context = context;

   if (type_is_integer(type)) {
      integer_value_mirror *ivm =
         jit_mspace_alloc(sizeof(integer_value_mirror));
      ivm->access = &(ivm->pt);

      ivm->pt.context = context;
      ivm->pt.f_value = value.integer;
      ivm->pt.f_owner = vm;

      vm->pt.f_class = CLASS_INTEGER;
      vm->pt.f_integer = ivm;
   }
   else
      fatal_at(tree_loc(where), "unsupported type %s in prefix of REFLECT "
               "attribute", type_pp(type));

   return vm;
}

void _std_reflection_init(void)
{
   // Dummy function to force linking
}
