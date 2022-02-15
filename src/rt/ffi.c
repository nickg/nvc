//
//  Copyright (C) 2021  Nick Gasson
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

#include "ffi.h"

#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include <inttypes.h>

static bool ffi_is_integral(ffi_type_t type)
{
   return type == FFI_INT8 || type == FFI_INT16 || type == FFI_INT32
      || type == FFI_INT64;
}

static int64_t ffi_widen_int(ffi_type_t type, const void *input, size_t insz)
{
   switch (type) {
   case FFI_INT8: return *((int8_t *)input);
   case FFI_INT16: return *((int16_t *)input);
   case FFI_INT32: return *((int32_t *)input);
   case FFI_INT64: return *((int32_t *)input);
   default:
      fatal_trace("invalid integer type in ffi_widen_int");
   }
}

static void ffi_store_int(ffi_type_t type, uint64_t value,
                          void *output, size_t outsz)
{
   assert(outsz <= sizeof(int64_t));
   switch (type) {
   case FFI_INT8: *(uint8_t *)output = (uint8_t)value; break;
   case FFI_INT16: *(uint16_t *)output = (uint16_t)value; break;
   case FFI_INT32: *(uint32_t *)output = (uint32_t)value; break;
   case FFI_INT64: *(uint64_t *)output = value; break;
   default:
      fatal_trace("invalid integer type in ffi_store_int");
   }
}

void ffi_call(ffi_closure_t *c, const void *input, size_t insz,
              void *output, size_t outsz)
{
   if (ffi_is_integral(c->spec.atype)) {
      const int64_t arg = ffi_widen_int(c->spec.atype, input, insz);

      if (ffi_is_integral(c->spec.rtype)) {
         uint64_t (*fn)(void *, int64_t) = c->fn;
         const uint64_t r = (*fn)(c->context, arg);
         ffi_store_int(c->spec.rtype, r, output, outsz);
      }
      else if (c->spec.rtype == FFI_FLOAT) {
         double (*fn)(void *, int64_t) = c->fn;
         assert(outsz == sizeof(double));
         *(double *)output = (*fn)(c->context, arg);
      }
      else if (c->spec.rtype == FFI_POINTER) {
         void *(*fn)(void *, int64_t) = c->fn;
         void *r = (*fn)(c->context, arg);
         memcpy(output, r, outsz);
      }
      else
         fatal_trace("unhandled FFI function argument combination");
   }
   else if (c->spec.atype == FFI_FLOAT && ffi_is_integral(c->spec.rtype)) {
      uint64_t (*fn)(void *, double) = c->fn;
      const uint64_t r = (*fn)(c->context, *(double *)input);
      ffi_store_int(c->spec.rtype, r, output, outsz);
   }
   else if (c->spec.atype == FFI_FLOAT && c->spec.rtype == FFI_FLOAT) {
      double (*fn)(void *, double) = c->fn;
      assert(insz == sizeof(double));
      assert(outsz == sizeof(double));
      *(double *)output = (*fn)(c->context, *(double *)input);
   }
   else if (c->spec.atype == FFI_POINTER && ffi_is_integral(c->spec.rtype)) {
      int64_t (*fn)(void *, const void *) = c->fn;
      const int64_t r = (*fn)(c->context, input);
      ffi_store_int(c->spec.rtype, r, output, outsz);
   }
   else if (c->spec.atype == FFI_VOID && ffi_is_integral(c->spec.rtype)) {
      int64_t (*fn)(void *) = c->fn;
      const int64_t r = (*fn)(c->context);
      ffi_store_int(c->spec.rtype, r, output, outsz);
   }
   else if (c->spec.atype == FFI_UARRAY && ffi_is_integral(c->spec.rtype)) {
      int64_t (*fn)(void *, EXPLODED_UARRAY(arg)) = c->fn;
      assert(insz == sizeof(ffi_uarray_t));
      const ffi_uarray_t *u = input;
      const int64_t r = (*fn)(c->context, u->ptr, u->dims[0].left,
                              u->dims[0].length);
      ffi_store_int(c->spec.rtype, r, output, outsz);
   }
   else if (c->spec.atype == FFI_UARRAY && c->spec.rtype == FFI_FLOAT) {
      double (*fn)(void *, EXPLODED_UARRAY(arg)) = c->fn;
      assert(insz == sizeof(ffi_uarray_t));
      assert(outsz == sizeof(double));
      const ffi_uarray_t *u = input;
      *(double *)output = (*fn)(c->context, u->ptr, u->dims[0].left,
                                u->dims[0].length);
   }
   else if (c->spec.atype == FFI_UARRAY && c->spec.rtype == FFI_POINTER) {
      void *(*fn)(void *, EXPLODED_UARRAY(arg)) = c->fn;
      assert(insz == sizeof(ffi_uarray_t));
      const ffi_uarray_t *u = input;
      void *r = (*fn)(c->context, u->ptr, u->dims[0].left, u->dims[0].length);
      memcpy(output, r, outsz);
   }
   else
      fatal_trace("unhandled FFI function argument combination");
}

ffi_closure_t *ffi_ref_closure(ffi_closure_t *c)
{
   assert(c->refcnt > 0);
   c->refcnt++;
   return c;
}

void ffi_unref_closure(ffi_closure_t *c)
{
   assert(c->refcnt > 0);
   if (--(c->refcnt) == 0)
      free(c);
}
