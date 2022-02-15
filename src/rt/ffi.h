//
//  Copyright (C) 2021-2022  Nick Gasson
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

#ifndef _RT_FFI_H
#define _RT_FFI_H

#include "prim.h"
#include "util.h"

#include <stdint.h>

typedef enum {
   FFI_INT8,
   FFI_INT16,
   FFI_INT32,
   FFI_INT64,
   FFI_FLOAT,
   FFI_POINTER,
   FFI_UARRAY,
   FFI_VOID,
} ffi_type_t;

typedef union {
   struct {
      ffi_type_t atype : 4;
      ffi_type_t rtype : 4;
   };
   uint32_t bits;
} ffi_spec_t;

STATIC_ASSERT(sizeof(ffi_spec_t) == 4);

// The code generator knows the layout of this struct
typedef struct {
   void       *fn;
   void       *context;
   ffi_spec_t  spec;
   uint32_t    refcnt;
} ffi_closure_t;

STATIC_ASSERT(sizeof(ffi_closure_t) == 24);

// The code generator knows the layout of this struct
typedef struct {
   void *ptr;
   struct {
      int32_t left;
      int32_t length;
   } dims[1];
} ffi_uarray_t;

STATIC_ASSERT(sizeof(ffi_uarray_t) == 16);

// Macro to generate the correct calling convention for LLVM by-value
// uarray aggregates
#define EXPLODED_UARRAY(name) \
   void *name##_ptr, int32_t name##_left, int32_t name##_length

void ffi_call(ffi_closure_t *c, const void *input, size_t insz,
              void *output, size_t outsz);
void ffi_unref_closure(ffi_closure_t *c);
ffi_closure_t *ffi_ref_closure(ffi_closure_t *c);

#endif  // _RT_FFI_H
