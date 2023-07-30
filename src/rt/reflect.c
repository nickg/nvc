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
#include "jit/jit-layout.h"
#include "jit/jit.h"
#include "tree.h"
#include "type.h"

#include <assert.h>
#include <string.h>
#include <stdlib.h>
#include <float.h>

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
typedef struct _integer_value_mirror integer_value_mirror;
typedef struct _floating_value_mirror floating_value_mirror;
typedef struct _enumeration_value_mirror enumeration_value_mirror;

typedef struct {
   type_t          f_type;
   subtype_mirror *f_mirror;
} cache_elem_t;

typedef struct {
   void         *context;
   int64_t       f_canary1;
   cache_elem_t *f_subtype_cache;
   int64_t       f_num_subtypes;
   int64_t       f_max_subtypes;
   int64_t       f_canary2;
} internal_cache_pt;

typedef struct {
   void                 *context;
   subtype_mirror       *f_owner;
   integer_value_mirror *f_left;
   integer_value_mirror *f_right;
   integer_value_mirror *f_low;
   integer_value_mirror *f_high;
   uint8_t               f_ascending;
} integer_subtype_mirror_pt;

typedef struct {
   void                      *access;
   integer_subtype_mirror_pt  pt;
} integer_subtype_mirror;

typedef struct {
   void           *context;
   subtype_mirror *f_owner;
   ffi_uarray_t   *f_literals;
} enumeration_subtype_mirror_pt;

typedef struct {
   void                          *access;
   enumeration_subtype_mirror_pt  pt;
} enumeration_subtype_mirror;

typedef struct {
   void                  *context;
   subtype_mirror        *f_owner;
   floating_value_mirror *f_left;
   floating_value_mirror *f_right;
   floating_value_mirror *f_low;
   floating_value_mirror *f_high;
   uint8_t                f_ascending;
} floating_subtype_mirror_pt;

typedef struct {
   void                       *access;
   floating_subtype_mirror_pt  pt;
} floating_subtype_mirror;

typedef struct {
   subtype_mirror *f_index_subtype;
   int64_t         f_left;
   int64_t         f_right;
   int64_t         f_length;
   uint8_t         f_ascending;
} dimension_rec;

typedef struct {
   void           *context;
   subtype_mirror *f_owner;
   uint64_t        f_dimensions;
   subtype_mirror *f_element_subtype;
   ffi_uarray_t   *f_dimension_data;
} array_subtype_mirror_pt;

typedef struct {
   void                    *access;
   array_subtype_mirror_pt  pt;
} array_subtype_mirror;

typedef struct {
   ffi_uarray_t   *f_name;
   subtype_mirror *f_subtype;
} field_rec;

typedef struct {
   void           *context;
   subtype_mirror *f_owner;
   ffi_uarray_t   *f_fields;
} record_subtype_mirror_pt;

typedef struct {
   void                     *access;
   record_subtype_mirror_pt  pt;
} record_subtype_mirror;

typedef struct {
   void           *context;
   subtype_mirror *f_owner;
   subtype_mirror *f_designated;
} file_subtype_mirror_pt;

typedef struct {
   void                   *access;
   file_subtype_mirror_pt  pt;
} file_subtype_mirror;

typedef struct {
   void           *context;
   subtype_mirror *f_owner;
   subtype_mirror *f_designated;
} access_subtype_mirror_pt;

typedef struct {
   void                     *access;
   access_subtype_mirror_pt  pt;
} access_subtype_mirror;

typedef struct {
   void                   *context;
   value_mirror           *f_owner;
   integer_subtype_mirror *f_subtype;
   int64_t                 f_value;
} integer_value_mirror_pt;

typedef struct _integer_value_mirror {
   void                    *access;
   integer_value_mirror_pt  pt;
} integer_value_mirror;

typedef struct {
   void                       *context;
   value_mirror               *f_owner;
   enumeration_subtype_mirror *f_subtype;
   int64_t                     f_pos;
   ffi_uarray_t               *f_image;
} enumeration_value_mirror_pt;

typedef struct _enumeration_value_mirror {
   void                        *access;
   enumeration_value_mirror_pt  pt;
} enumeration_value_mirror;

typedef struct {
   void                    *context;
   value_mirror            *f_owner;
   floating_subtype_mirror *f_subtype;
   double                   f_value;
} floating_value_mirror_pt;

typedef struct _floating_value_mirror {
   void                     *access;
   floating_value_mirror_pt  pt;
} floating_value_mirror;

typedef struct {
   void                 *context;
   value_mirror         *f_owner;
   array_subtype_mirror *f_subtype;
   ffi_uarray_t         *f_elements;
} array_value_mirror_pt;

typedef struct {
   void                  *access;
   array_value_mirror_pt  pt;
} array_value_mirror;

typedef struct {
   void                  *context;
   value_mirror          *f_owner;
   record_subtype_mirror *f_subtype;
   ffi_uarray_t          *f_elements;
} record_value_mirror_pt;

typedef struct {
   void                   *access;
   record_value_mirror_pt  pt;
} record_value_mirror;

typedef struct {
   void                *context;
   value_mirror        *f_owner;
   file_subtype_mirror *f_subtype;
   ffi_uarray_t        *f_logical_name;
   uint8_t              f_open_kind;
} file_value_mirror_pt;

typedef struct {
   void                 *access;
   file_value_mirror_pt  pt;
} file_value_mirror;

typedef struct {
   void                  *context;
   value_mirror          *f_owner;
   access_subtype_mirror *f_subtype;
   value_mirror          *f_value;
} access_value_mirror_pt;

typedef struct {
   void                   *access;
   access_value_mirror_pt  pt;
} access_value_mirror;

typedef struct {
   void                     *context;
   uint8_t                   f_class;
   subtype_mirror           *f_subtype;
   integer_value_mirror     *f_integer;
   enumeration_value_mirror *f_enumeration;
   floating_value_mirror    *f_floating;
   array_value_mirror       *f_array;
   record_value_mirror      *f_record;
   file_value_mirror        *f_file;
   access_value_mirror      *f_access;
} value_mirror_pt;

typedef struct _value_mirror {
   void            *access;
   value_mirror_pt  pt;
} value_mirror;

typedef struct {
   void                       *context;
   uint8_t                     f_class;
   ffi_uarray_t               *f_name;
   integer_subtype_mirror     *f_integer;
   enumeration_subtype_mirror *f_enumeration;
   floating_subtype_mirror    *f_floating;
   array_subtype_mirror       *f_array;
   record_subtype_mirror      *f_record;
   file_subtype_mirror        *f_file;
   access_subtype_mirror      *f_access;
} subtype_mirror_pt;

typedef struct _subtype_mirror {
   void              *access;
   subtype_mirror_pt  pt;
} subtype_mirror;

static subtype_mirror *get_subtype_mirror(void *context, type_t type,
                                          const jit_scalar_t *bounds);

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

static internal_cache_pt *get_cache(void *context)
{
   // This will break if the package layout ever changes
   internal_cache_pt *pt = *(internal_cache_pt **)(context + sizeof(void *));
   assert(pt->f_canary1 == 0xdeadbeef);
   assert(pt->f_canary2 == 0xcafebabe);
   assert(pt->context == context);
   return pt;
}

static jit_scalar_t load_scalar(const void *base, size_t off, size_t size)
{
   jit_scalar_t elt = { .integer = 0 };

#define UNPACK_ELEMENT(type) do {               \
      const type *p = base;                     \
      elt.integer = p[off];                     \
   } while (0);

   FOR_ALL_SIZES(size, UNPACK_ELEMENT);

   return elt;
}

static jit_scalar_t *get_array_bounds(type_t type, void *ptr)
{
   const int ndims = dimension_of(type);
   jit_scalar_t *fbounds = xmalloc_array(ndims*2 + 1, sizeof(jit_scalar_t));

   if (type_is_unconstrained(type)) {
      ffi_uarray_t *u = ptr;
      fbounds[0].pointer = u->ptr;

      for (int i = 0; i < ndims; i++) {
         fbounds[i*2+1].integer = u->dims[i].left;
         fbounds[i*2+2].integer = u->dims[i].length;
      }
   }
   else {
      fbounds[0].pointer = ptr;

      for (int i = 0; i < ndims; i++) {
         tree_t r = range_of(type, i);

         int64_t left, length;
         if (!folded_int(tree_left(r), &left))
            goto not_const;
         else if (!folded_length(r, &length))
            goto not_const;

         const range_kind_t dir = tree_subkind(r);
         fbounds[i*2+1].integer = left;
         fbounds[i*2+2].integer = (dir == RANGE_DOWNTO ? ~length : length);
      }
   }

   return fbounds;

 not_const:
   jit_msg(NULL, DIAG_FATAL, "array %s bounds are not constant", type_pp(type));
   return NULL;
}

static jit_scalar_t *get_null_array(type_t type)
{
   const int ndims = dimension_of(type);
   jit_scalar_t *bounds = xmalloc_array(ndims*2 + 1, sizeof(jit_scalar_t));
   bounds[0].pointer = NULL;

   for (int i = 0; i < ndims; i++) {
      bounds[i*2+1].integer = 0;
      bounds[i*2+2].integer = 0;
   }

   return bounds;
}

static ffi_uarray_t *get_file_logical_name(FILE *fp)
{
   LOCAL_TEXT_BUF tb = tb_new();
   if (get_handle_path(fileno(fp), tb))
      return get_string(tb_get(tb));
   else {
      jit_msg(NULL, DIAG_WARN, "unable to get file logical name");
      return NULL;
   }
}

static value_mirror *get_value_mirror(void *context, jit_scalar_t value,
                                      type_t type, const jit_scalar_t *bounds)
{
   value_mirror *vm = zero_alloc(sizeof(value_mirror));

   vm->access = &(vm->pt);
   vm->pt.context = context;
   vm->pt.f_subtype = get_subtype_mirror(context, type, bounds);

   if (type_is_integer(type)) {
      integer_value_mirror *ivm = zero_alloc(sizeof(integer_value_mirror));
      ivm->access = &(ivm->pt);

      ivm->pt.context = context;
      ivm->pt.f_value = value.integer;
      ivm->pt.f_owner = vm;
      ivm->pt.f_subtype = vm->pt.f_subtype->pt.f_integer;

      vm->pt.f_class = CLASS_INTEGER;
      vm->pt.f_integer = ivm;
   }
   else if (type_is_enum(type)) {
      enumeration_value_mirror *evm =
         zero_alloc(sizeof(enumeration_value_mirror));
      evm->access = &(evm->pt);

      evm->pt.context = context;
      evm->pt.f_pos = value.integer;
      evm->pt.f_owner = vm;
      evm->pt.f_subtype = vm->pt.f_subtype->pt.f_enumeration;

      LOCAL_TEXT_BUF tb = tb_new();
      type_t base = type_base_recur(type);
      tb_istr(tb, tree_ident(type_enum_literal(base, value.integer)));
      if (tb_get(tb)[0] != '\'')
         tb_downcase(tb);

      evm->pt.f_image = get_string(tb_get(tb));

      vm->pt.f_class = CLASS_ENUMERATION;
      vm->pt.f_enumeration = evm;
   }
   else if (type_is_real(type)) {
      floating_value_mirror *fvm = zero_alloc(sizeof(floating_value_mirror));
      fvm->access = &(fvm->pt);

      fvm->pt.context = context;
      fvm->pt.f_value = value.real;
      fvm->pt.f_owner = vm;
      fvm->pt.f_subtype = vm->pt.f_subtype->pt.f_floating;

      vm->pt.f_class = CLASS_FLOATING;
      vm->pt.f_floating = fvm;
   }
   else if (type_is_array(type)) {
      array_value_mirror *avm = zero_alloc(sizeof(array_value_mirror));
      avm->access = &(avm->pt);

      avm->pt.context = context;
      avm->pt.f_owner = vm;
      avm->pt.f_subtype = vm->pt.f_subtype->pt.f_array;

      dimension_rec *dims = avm->pt.f_subtype->pt.f_dimension_data->ptr;
      size_t total = 1;
      for (int i = 0; i < avm->pt.f_subtype->pt.f_dimensions; i++)
         total *= dims[i].f_length;

      avm->pt.f_elements =
         zero_alloc(sizeof(ffi_uarray_t) + total * sizeof(value_mirror *));

      avm->pt.f_elements->dims[0].left = 0;
      avm->pt.f_elements->dims[0].length = total;
      avm->pt.f_elements->ptr = avm->pt.f_elements + 1;

      type_t elem = type_elem(type);
      const int ebytes = type_byte_width(elem);

      value_mirror **elems = avm->pt.f_elements->ptr;
      for (int i = 0; i < total; i++) {
         jit_scalar_t elt = load_scalar(value.pointer, i, ebytes);
         elems[i] = get_value_mirror(context, elt, elem, NULL);
      }

      vm->pt.f_class = CLASS_ARRAY;
      vm->pt.f_array = avm;
   }
   else if (type_is_record(type)) {
      record_value_mirror *rvm = zero_alloc(sizeof(record_value_mirror));
      rvm->access = &(rvm->pt);

      rvm->pt.context = context;
      rvm->pt.f_owner = vm;
      rvm->pt.f_subtype = vm->pt.f_subtype->pt.f_record;

      type_t base = type_base_recur(type);
      const int nfields = type_fields(base);

      rvm->pt.f_elements =
         zero_alloc(sizeof(ffi_uarray_t) + nfields * sizeof(value_mirror *));
      rvm->pt.f_elements->dims[0].left = 0;
      rvm->pt.f_elements->dims[0].length = nfields;
      rvm->pt.f_elements->ptr = rvm->pt.f_elements + 1;

      const jit_layout_t *l = layout_of(type);
      assert(l->nparts == nfields);

      value_mirror **elements = rvm->pt.f_elements->ptr;
      for (int i = 0; i < l->nparts; i++) {
         type_t ft = tree_type(type_field(base, i));
         void *ptr = value.pointer + l->parts[i].offset;
         if (type_is_scalar(ft)) {
            jit_scalar_t elt = load_scalar(ptr, 0, l->parts[i].size);
            elements[i] = get_value_mirror(context, elt, ft, NULL);
         }
         else if (type_is_array(ft)) {
            jit_scalar_t *fbounds LOCAL = get_array_bounds(ft, ptr);
            elements[i] = get_value_mirror(context, fbounds[0], ft, fbounds);
         }
         else {
            jit_scalar_t elt = { .pointer = ptr };
            elements[i] = get_value_mirror(context, elt, ft, NULL);
         }
      }

      vm->pt.f_class = CLASS_RECORD;
      vm->pt.f_record = rvm;
   }
   else if (type_is_file(type)) {
      file_value_mirror *fvm = zero_alloc(sizeof(file_value_mirror));
      fvm->access = &(fvm->pt);

      fvm->pt.context = context;
      fvm->pt.f_owner = vm;
      fvm->pt.f_subtype = vm->pt.f_subtype->pt.f_file;

      FILE *fp = value.pointer;

      extern int8_t __nvc_file_mode(FILE **fp);
      fvm->pt.f_open_kind = __nvc_file_mode(&fp);
      fvm->pt.f_logical_name = get_file_logical_name(fp);

      vm->pt.f_class = CLASS_FILE;
      vm->pt.f_file = fvm;
   }
   else if (type_is_access(type)) {
      access_value_mirror *avm = zero_alloc(sizeof(access_value_mirror));
      avm->access = &(avm->pt);

      avm->pt.context = context;
      avm->pt.f_owner = vm;
      avm->pt.f_subtype = vm->pt.f_subtype->pt.f_access;

      if (value.pointer != NULL) {
         type_t designated = type_designated(type);
         if (type_is_array(designated)) {
            jit_scalar_t *bounds LOCAL =
               get_array_bounds(designated, value.pointer);
            avm->pt.f_value =
               get_value_mirror(context, bounds[0], designated, bounds);
         }
         else {
            jit_scalar_t access = { .pointer = value.pointer };
            avm->pt.f_value =
               get_value_mirror(context, access, designated, NULL);
         }
      }

      vm->pt.f_class = CLASS_ACCESS;
      vm->pt.f_access = avm;
   }
   else
      jit_msg(NULL, DIAG_FATAL, "unsupported type %s in prefix of REFLECT "
              "attribute", type_pp(type));

   return vm;
}

static integer_value_mirror *get_integer_mirror(void *context, type_t type,
                                                int64_t value)
{
   jit_scalar_t scalar = { .integer = value };
   return get_value_mirror(context, scalar, type, NULL)->pt.f_integer;
}

static enumeration_value_mirror *get_enumeration_mirror(void *context,
                                                        type_t type,
                                                        int64_t value)
{
   jit_scalar_t scalar = { .integer = value };
   return get_value_mirror(context, scalar, type, NULL)->pt.f_enumeration;
}

static floating_value_mirror *get_floating_mirror(void *context, type_t type,
                                                  double value)
{
   jit_scalar_t scalar = { .real = value };
   return get_value_mirror(context, scalar, type, NULL)->pt.f_floating;
}

static bool safe_to_cache(type_t type)
{
   if (type_is_unconstrained(type))
      return false;
   else if (type_is_array(type)) {
      const int ndims = dimension_of(type);
      for (int i = 0; i < ndims; i++) {
         int64_t length;
         if (!folded_length(range_of(type, i), &length))
            return false;
      }

      return true;
   }
   else if (type_is_record(type)) {
      const int nfields = type_fields(type);
      for (int i = 0; i < nfields; i++) {
         if (!safe_to_cache(tree_type(type_field(type, i))))
            return false;
      }

      return true;
   }
   else
      return true;
}

static subtype_mirror *get_subtype_mirror(void *context, type_t type,
                                          const jit_scalar_t *bounds)
{
   internal_cache_pt *cache = get_cache(context);

   for (int i = 0; i < cache->f_num_subtypes; i++) {
      cache_elem_t *e = &(cache->f_subtype_cache[i]);
      if (e->f_type == type)
         return e->f_mirror;
   }

   subtype_mirror *sm = zero_alloc(sizeof(subtype_mirror));
   sm->access = &(sm->pt);
   sm->pt.context = context;

   if (safe_to_cache(type)) {
      if (cache->f_num_subtypes == cache->f_max_subtypes) {
         const size_t new_max = MAX(cache->f_max_subtypes * 2, 128);
         cache_elem_t *tmp = zero_alloc(new_max * sizeof(cache_elem_t));
         memcpy(tmp, cache->f_subtype_cache,
                cache->f_max_subtypes * sizeof(cache_elem_t));

         cache->f_subtype_cache = tmp;
         cache->f_max_subtypes = new_max;
      }

      cache_elem_t *e = &(cache->f_subtype_cache[cache->f_num_subtypes++]);
      e->f_type = type;
      e->f_mirror = sm;
   }

   const char *simple = strrchr(istr(type_ident(type)), '.') + 1;
   sm->pt.f_name = get_string(simple);

   if (type_is_integer(type)) {
      integer_subtype_mirror *ism = zero_alloc(sizeof(integer_subtype_mirror));
      ism->access = &(ism->pt);

      range_kind_t rkind;
      int64_t low = INT64_MIN, high = INT64_MAX;
      for (;; type = type_base(type)) {
         tree_t r = range_of(type, 0);
         rkind = tree_subkind(r);
         if (rkind != RANGE_EXPR && folded_bounds(r, &low, &high))
            break;
         else if (type_kind(type) != T_SUBTYPE)
            break;
      }

      ism->pt.context = context;
      ism->pt.f_owner = sm;
      ism->pt.f_ascending = (rkind == RANGE_TO);

      type_t index = index_type_of(type, 0);
      ism->pt.f_low = get_integer_mirror(context, index, low);
      ism->pt.f_high = get_integer_mirror(context, index, high);

      ism->pt.f_left = (rkind == RANGE_TO) ? ism->pt.f_low : ism->pt.f_high;
      ism->pt.f_right = (rkind == RANGE_TO) ? ism->pt.f_high : ism->pt.f_low;

      sm->pt.f_class = CLASS_INTEGER;
      sm->pt.f_integer = ism;
   }
   else if (type_is_enum(type)) {
      enumeration_subtype_mirror *esm =
         zero_alloc(sizeof(enumeration_subtype_mirror));
      esm->access = &(esm->pt);

      range_kind_t rkind;
      int64_t low = INT64_MIN, high = INT64_MAX;
      for (;; type = type_base(type)) {
         tree_t r = range_of(type, 0);
         rkind = tree_subkind(r);
         if (rkind != RANGE_EXPR && folded_bounds(r, &low, &high))
            break;
         else if (type_kind(type) != T_SUBTYPE)
            break;
      }

      esm->pt.context = context;
      esm->pt.f_owner = sm;

      type_t base = type_base_recur(type);
      const int nlits = type_enum_literals(base);

      const size_t size =
         sizeof(ffi_uarray_t) + nlits * sizeof(enumeration_value_mirror *);
      esm->pt.f_literals = zero_alloc(size);

      esm->pt.f_literals->dims[0].left = 0;
      esm->pt.f_literals->dims[0].length = nlits;
      esm->pt.f_literals->ptr = esm->pt.f_literals + 1;

      enumeration_value_mirror **lits = esm->pt.f_literals->ptr;
      for (int i = 0; i < nlits; i++)
         lits[i] = get_enumeration_mirror(context, base, i);

      sm->pt.f_class = CLASS_ENUMERATION;
      sm->pt.f_enumeration = esm;
   }
   else if (type_is_real(type)) {
      floating_subtype_mirror *fsm =
         zero_alloc(sizeof(floating_subtype_mirror));
      fsm->access = &(fsm->pt);

      range_kind_t rkind;
      double low = DBL_MIN, high = DBL_MAX;
      tree_t r;
      for (;; type = type_base(type)) {
         r = range_of(type, 0);
         rkind = tree_subkind(r);
         if (rkind != RANGE_EXPR && folded_bounds_real(r, &low, &high))
            break;
         else if (type_kind(type) != T_SUBTYPE)
            break;
      }

      fsm->pt.context = context;
      fsm->pt.f_owner = sm;
      fsm->pt.f_ascending = (rkind == RANGE_TO);

      type_t index = index_type_of(type, 0);
      fsm->pt.f_low = get_floating_mirror(context, index, low);
      fsm->pt.f_high = get_floating_mirror(context, index, high);

      fsm->pt.f_left = (rkind == RANGE_TO) ? fsm->pt.f_low : fsm->pt.f_high;
      fsm->pt.f_right = (rkind == RANGE_TO) ? fsm->pt.f_high : fsm->pt.f_low;

      sm->pt.f_class = CLASS_FLOATING;
      sm->pt.f_floating = fsm;
   }
   else if (type_is_array(type)) {
      array_subtype_mirror *astm = zero_alloc(sizeof(array_subtype_mirror));
      astm->access = &(astm->pt);

      const int ndims = dimension_of(type);
      type_t elem = type_elem(type);

      astm->pt.context = context;
      astm->pt.f_owner = sm;
      astm->pt.f_dimensions = ndims;
      astm->pt.f_element_subtype = get_subtype_mirror(context, elem, NULL);

      astm->pt.f_dimension_data =
         zero_alloc(sizeof(ffi_uarray_t) + ndims * sizeof(dimension_rec));
      astm->pt.f_dimension_data->dims[0].left = 1;
      astm->pt.f_dimension_data->dims[0].length = ndims;
      astm->pt.f_dimension_data->ptr = astm->pt.f_dimension_data + 1;

      dimension_rec *dims = astm->pt.f_dimension_data->ptr;
      for (int i = 0; i < ndims; i++) {
         type_t itype = index_type_of(type, i);
         dims[i].f_index_subtype = get_subtype_mirror(context, itype, NULL);

         const int64_t left = bounds[i*2 + 1].integer;
         const int64_t length = bounds[i*2 + 2].integer;

         dims[i].f_ascending = length >= 0;
         dims[i].f_left = left;
         dims[i].f_length = length ^ (length >> 63);

         if (length < 0)
            dims[i].f_right = left + length + 2;
         else
            dims[i].f_right = left + length - 1;
      }

      sm->pt.f_class = CLASS_ARRAY;
      sm->pt.f_array = astm;
   }
   else if (type_is_record(type)) {
      record_subtype_mirror *rsm = zero_alloc(sizeof(record_subtype_mirror));
      rsm->access = &(rsm->pt);

      rsm->pt.context = context;
      rsm->pt.f_owner = sm;

      type_t base = type_base_recur(type);
      const int nfields = type_fields(base);

      rsm->pt.f_fields =
         zero_alloc(sizeof(ffi_uarray_t) + nfields * sizeof(field_rec));
      rsm->pt.f_fields->dims[0].left = 0;
      rsm->pt.f_fields->dims[0].length = nfields;
      rsm->pt.f_fields->ptr = rsm->pt.f_fields + 1;

      const jit_layout_t *l = layout_of(type);
      assert(l->nparts == nfields);

      field_rec *fields = rsm->pt.f_fields->ptr;
      for (int i = 0; i < nfields; i++) {
         tree_t f = type_field(base, i);
         fields[i].f_name = get_string(istr(tree_ident(f)));

         type_t ft = tree_type(f);
         if (type_is_array(ft)) {
            void *ptr = bounds[0].pointer + l->parts[i].offset;
            jit_scalar_t *fbounds LOCAL = get_array_bounds(ft, ptr);
            fields[i].f_subtype = get_subtype_mirror(context, ft, fbounds);
         }
         else
            fields[i].f_subtype = get_subtype_mirror(context, ft, NULL);
      }

      sm->pt.f_class = CLASS_RECORD;
      sm->pt.f_record = rsm;
   }
   else if (type_is_file(type)) {
      file_subtype_mirror *fsm = zero_alloc(sizeof(file_subtype_mirror));
      fsm->access = &(fsm->pt);

      fsm->pt.context = context;
      fsm->pt.f_owner = sm;

      type_t designated = type_designated(type);

      jit_scalar_t *dbounds LOCAL = NULL;
      if (type_is_array(designated))
         dbounds = get_null_array(designated);

      fsm->pt.f_designated = get_subtype_mirror(context, designated, dbounds);

      sm->pt.f_class = CLASS_FILE;
      sm->pt.f_file = fsm;
   }
   else if (type_is_access(type)) {
      access_subtype_mirror *astm = zero_alloc(sizeof(access_subtype_mirror));
      astm->access = &(astm->pt);

      astm->pt.context = context;
      astm->pt.f_owner = sm;

      type_t designated = type_designated(type);

      jit_scalar_t *dbounds LOCAL = NULL;
      if (type_is_array(designated))
         dbounds = get_null_array(designated);

      astm->pt.f_designated = get_subtype_mirror(context, designated, dbounds);

      sm->pt.f_class = CLASS_ACCESS;
      sm->pt.f_access = astm;
   }
   else
      jit_msg(NULL, DIAG_FATAL, "unsupported type %s in prefix of REFLECT "
              "attribute", type_pp(type));

   return sm;
}

void *x_reflect_value(void *context, jit_scalar_t value, tree_t where,
                      const jit_scalar_t *bounds)
{
   return get_value_mirror(context, value, tree_type(where), bounds);
}

void *x_reflect_subtype(void *context, tree_t where, const jit_scalar_t *bounds)
{
   type_t type = tree_type(where);
   assert(!type_is_unconstrained(type));   // Should be caught earlier
   return get_subtype_mirror(context, type, bounds);
}

void _std_reflection_init(void)
{
   // Dummy function to force linking
}
