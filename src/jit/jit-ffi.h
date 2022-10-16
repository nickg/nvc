//
//  Copyright (C) 2022  Nick Gasson
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

#ifndef _JIT_FFI_H
#define _JIT_FFI_H

#include "prim.h"
#include "jit/jit.h"

typedef enum {
   FFI_VOID,
   FFI_INT8,
   FFI_INT16,
   FFI_INT32,
   FFI_INT64,
   FFI_FLOAT,
   FFI_POINTER,
   FFI_UARRAY,
   FFI_SIGNAL,
} ffi_type_t;

typedef uint64_t ffi_spec_t;

typedef struct _jit_foreign jit_foreign_t;

// Macro to generate the correct calling convention for by-value uarray
// aggregates
#define EXPLODED_UARRAY(name) \
   void *name##_ptr, int32_t name##_left, int32_t name##_length

// The code generator knows the layout of this struct
typedef struct _ffi_uarray {
   void *ptr;
   struct {
      int32_t left;
      int32_t length;
   } dims[1];
} ffi_uarray_t;

// The code generator knows the layout of this struct
typedef struct _ffi_closure {
   jit_handle_t  handle;
   void         *context;
} ffi_closure_t;

jit_foreign_t *jit_ffi_bind(ident_t sym, ffi_spec_t spec, void *ptr);
jit_foreign_t *jit_ffi_get(ident_t sym);
jit_scalar_t jit_ffi_call(jit_foreign_t *ff, jit_scalar_t *args);

int ffi_count_args(ffi_spec_t spec);
ffi_uarray_t ffi_wrap_str(char *buf, size_t len);
size_t ffi_uarray_len(const ffi_uarray_t *u);
bool ffi_is_integral(ffi_type_t type);
int64_t ffi_widen_int(ffi_type_t type, const void *input, size_t insz);
void ffi_store_int(ffi_type_t type, uint64_t value, void *output, size_t outsz);

typedef struct _jit_dll jit_dll_t;

jit_dll_t *ffi_load_dll(const char *path);
void ffi_unload_dll(jit_dll_t *dll);
void *ffi_find_symbol(jit_dll_t *dll, const char *name);

#endif   // _JIT_FFI_H
