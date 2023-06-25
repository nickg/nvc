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
#include <string.h>

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
typedef struct _subtype_mirror subtype_mirror;

typedef struct {
   void           *context;
   subtype_mirror *f_owner;
} integer_subtype_mirror_pt;

typedef struct {
   void                      *access;
   integer_subtype_mirror_pt  pt;
} integer_subtype_mirror;

typedef struct {
   void                   *context;
   value_mirror           *f_owner;
   integer_subtype_mirror *f_subtype;
   int64_t                 f_value;
} integer_value_mirror_pt;

typedef struct {
   void                    *access;
   integer_value_mirror_pt  pt;
} integer_value_mirror;

typedef struct {
   void                 *context;
   uint8_t               f_class;
   subtype_mirror       *f_subtype;
   integer_value_mirror *f_integer;
} value_mirror_pt;

typedef struct _value_mirror {
   void            *access;
   value_mirror_pt  pt;
} value_mirror;

typedef struct {
   void                   *context;
   uint8_t                 f_class;
   ffi_uarray_t           *f_name;
   integer_subtype_mirror *f_integer;
} subtype_mirror_pt;

typedef struct _subtype_mirror {
   void              *access;
   subtype_mirror_pt  pt;
} subtype_mirror;

static subtype_mirror *get_subtype_mirror(void *context, type_t type);

static void *zero_alloc(size_t size)
{
   void *ptr = jit_mspace_alloc(size);
   memset(ptr, '\0', size);
   return ptr;
}

static ffi_uarray_t *get_string(const char *str)
{
   const size_t nchars = strlen(str);
   void *mem = jit_mspace_alloc(sizeof(ffi_uarray_t) + nchars);
   memcpy(mem + sizeof(ffi_uarray_t), str, nchars);

   ffi_uarray_t *u = mem;
   u->ptr = mem + sizeof(ffi_uarray_t);
   u->dims[0].left = 1;
   u->dims[0].length = nchars;

   return u;
}

static value_mirror *get_value_mirror(void *context, jit_scalar_t value,
                                      type_t type)
{
   value_mirror *vm = zero_alloc(sizeof(value_mirror));

   vm->access = &(vm->pt);
   vm->pt.context = context;
   vm->pt.f_subtype = get_subtype_mirror(context, type);

   if (type_is_integer(type)) {
      integer_value_mirror *ivm = zero_alloc(sizeof(integer_value_mirror));
      ivm->access = &(ivm->pt);

      ivm->pt.context = context;
      ivm->pt.f_value = value.integer;
      ivm->pt.f_owner = vm;

      vm->pt.f_class = CLASS_INTEGER;
      vm->pt.f_integer = ivm;
   }
   else
      jit_msg(NULL, DIAG_FATAL, "unsupported type %s in prefix of REFLECT "
              "attribute", type_pp(type));

   return vm;
}

static subtype_mirror *get_subtype_mirror(void *context, type_t type)
{
   // TODO: cache this (safely)

   subtype_mirror *sm = zero_alloc(sizeof(subtype_mirror));
   sm->access = &(sm->pt);
   sm->pt.context = context;

   const char *simple = strrchr(istr(type_ident(type)), '.') + 1;
   sm->pt.f_name = get_string(simple);

   if (type_is_integer(type)) {
      integer_subtype_mirror *ism = zero_alloc(sizeof(integer_subtype_mirror));
      ism->access = &(ism->pt);

      ism->pt.context = context;
      ism->pt.f_owner = sm;

      sm->pt.f_class = CLASS_INTEGER;
      sm->pt.f_integer = ism;
   }
   else
      jit_msg(NULL, DIAG_FATAL, "unsupported type %s in prefix of REFLECT "
              "attribute", type_pp(type));

   return sm;
}

void *x_reflect_value(void *context, jit_scalar_t value, tree_t where)
{
   return get_value_mirror(context, value, tree_type(where));
}

void _std_reflection_init(void)
{
   // Dummy function to force linking
}
