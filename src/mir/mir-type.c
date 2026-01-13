//
//  Copyright (C) 2024-2025  Nick Gasson
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
#include "array.h"
#include "ident.h"
#include "mir/mir-node.h"
#include "mir/mir-priv.h"
#include "mir/mir-structs.h"
#include "thread.h"

#include <assert.h>
#include <float.h>
#include <inttypes.h>
#include <stdlib.h>

#define REPROBE_LIMIT 10

static inline int64_t sadd64(int64_t a, int64_t b)
{
   int64_t result;
   if (__builtin_add_overflow(a, b, &result))
      return b < 0 ? INT64_MIN : INT64_MAX;

   return result;
}

static inline int64_t ssub64(int64_t a, int64_t b)
{
   int64_t result;
   if (__builtin_sub_overflow(a, b, &result))
      return b > 0 ? INT64_MIN : INT64_MAX;

   return result;
}

static inline int64_t smul64(int64_t a, int64_t b)
{
   int64_t result;
   if (__builtin_mul_overflow(a, b, &result))
      return (a > 0 && b > 0) || (a < 0 && b < 0) ? INT64_MAX : INT64_MIN;

   return result;
}

static uint32_t mir_hash_type(mir_unit_t *mu, const type_data_t *td)
{
   uint32_t h = knuth_hash(td->class);

   switch (td->class) {
   case _MIR_INVALID_TYPE:
   case MIR_TYPE_OFFSET:
   case MIR_TYPE_LOCUS:
   case MIR_TYPE_CONVERSION:
   case MIR_TYPE_OPAQUE:
   case MIR_TYPE_TRIGGER:
      break;

   case MIR_TYPE_POINTER:
      h ^= mir_type_data(mu, td->u.pointer)->hash;
      break;

   case MIR_TYPE_SIGNAL:
      h ^= mir_type_data(mu, td->u.signal.base)->hash;
      break;

   case MIR_TYPE_INT:
      h ^= mix_bits_64(td->u.intg.low) >> 32;
      h ^= mix_bits_64(td->u.intg.high);
      break;

   case MIR_TYPE_REAL:
      h ^= mix_bits_64(FLOAT_BITS(td->u.real.low)) >> 32;
      h ^= mix_bits_64(FLOAT_BITS(td->u.real.high));
      break;

   case MIR_TYPE_CARRAY:
      h += knuth_hash(td->u.carray.size);
      h ^= mir_type_data(mu, td->u.carray.elem)->hash;
      break;

   case MIR_TYPE_UARRAY:
      h += knuth_hash(td->u.uarray.dims);
      h ^= mir_type_data(mu, td->u.uarray.elem)->hash;
      break;

   case MIR_TYPE_CONTEXT:
      h ^= ident_hash(td->u.context);
      break;

   case MIR_TYPE_ACCESS:
      h ^= mir_type_data(mu, td->u.access.to)->hash;
      break;

   case MIR_TYPE_RECORD:
      h ^= ident_hash(td->u.record.name);
      for (int i = 0; i < td->u.record.count; i++)
         h ^= mir_type_data(mu, td->u.record.fields[i])->hash;
      break;

   case MIR_TYPE_CLOSURE:
      h ^= mir_type_data(mu, td->u.closure.rtype)->hash;
      break;

   case MIR_TYPE_RESOLUTION:
   case MIR_TYPE_FILE:
      h ^= mir_type_data(mu, td->u.base)->hash;
      break;

   case MIR_TYPE_VEC2:
   case MIR_TYPE_VEC4:
      h ^= knuth_hash(td->u.vec.size) ^ td->u.vec.issigned;
      break;
   }

   return h;
}

static bool mir_compare_types(const type_data_t *a, const type_data_t *b)
{
   if (a->class != b->class)
      return false;

   switch (a->class) {
   case _MIR_INVALID_TYPE:
      return false;

   case MIR_TYPE_OFFSET:
   case MIR_TYPE_LOCUS:
   case MIR_TYPE_OPAQUE:
   case MIR_TYPE_CONVERSION:
   case MIR_TYPE_TRIGGER:
      return true;

   case MIR_TYPE_POINTER:
      return a->u.pointer.bits == b->u.pointer.bits;

   case MIR_TYPE_SIGNAL:
      return a->u.signal.base.bits == b->u.signal.base.bits;

   case MIR_TYPE_INT:
      return a->u.intg.low == b->u.intg.low
         && a->u.intg.high == b->u.intg.high;

   case MIR_TYPE_REAL:
      return a->u.real.low == b->u.real.low
         && a->u.real.high == b->u.real.high;

   case MIR_TYPE_CARRAY:
      return a->u.carray.size == b->u.carray.size
         && a->u.carray.elem.bits == b->u.carray.elem.bits;

   case MIR_TYPE_UARRAY:
      return a->u.uarray.dims == b->u.uarray.dims
         && a->u.uarray.elem.bits == b->u.uarray.elem.bits;

   case MIR_TYPE_CONTEXT:
      return a->u.context == b->u.context;

   case MIR_TYPE_ACCESS:
      return a->u.access.to.bits == b->u.access.to.bits;

   case MIR_TYPE_RECORD:
      if (a->u.record.name != b->u.record.name)
         return false;
      else if (a->u.record.count != b->u.record.count)
         return false;
      else {
         for (int i = 0; i < a->u.record.count; i++) {
            if (a->u.record.fields[i].bits != b->u.record.fields[i].bits)
               return false;
         }

         return true;
      }

   case MIR_TYPE_CLOSURE:
      return a->u.closure.rtype.bits == b->u.closure.rtype.bits;

   case MIR_TYPE_RESOLUTION:
   case MIR_TYPE_FILE:
      return a->u.base.bits == b->u.base.bits;

   case MIR_TYPE_VEC2:
   case MIR_TYPE_VEC4:
      return a->u.vec.size == b->u.vec.size
         && a->u.vec.size == b->u.vec.size
         && a->u.vec.issigned == b->u.vec.issigned;
   }

   should_not_reach_here();
}

const type_data_t *mir_type_data(mir_unit_t *mu, mir_type_t type)
{
   mir_context_t *mc = mu->context;
   type_tab_t *tab = load_acquire(&mc->typetab);

   assert(type.tag == MIR_TAG_TYPE);
   assert(type.id < tab->max_types);

   const type_data_t *td = &(tab->types[type.id]);
   assert(td->class != _MIR_INVALID_TYPE);

   return td;
}

static void mir_install_type(mir_unit_t *mu, type_data_t *dst,
                             const type_data_t *src, uint32_t hash)
{
   dst->class = src->class;
   dst->repr  = src->repr;
   dst->hash  = hash;

   switch (src->class) {
   case MIR_TYPE_CARRAY:
      {
         const mir_carray_type_t *src_a = &src->u.carray;
         mir_carray_type_t *dst_a = &dst->u.carray;

         dst_a->elem    = src_a->elem;
         dst_a->size    = src_a->size;
         dst_a->pointer = mir_pointer_type(mu, src_a->elem);
      }
      break;

   case MIR_TYPE_UARRAY:
      {
         const mir_uarray_type_t *src_a = &src->u.uarray;
         mir_uarray_type_t *dst_a = &dst->u.uarray;

         dst_a->dims    = src_a->dims;
         dst_a->elem    = src_a->elem;

         if (mir_get_class(mu, src_a->elem) == MIR_TYPE_SIGNAL)
            dst_a->pointer = src_a->elem;
         else
            dst_a->pointer = mir_pointer_type(mu, src_a->elem);
      }
      break;

   case MIR_TYPE_RECORD:
      {
         const mir_record_type_t *src_r = &src->u.record;
         mir_record_type_t *dst_r = &dst->u.record;

         dst_r->name   = src_r->name;
         dst_r->count  = src_r->count;
         dst_r->fields = mir_global_malloc(mu->context, 0, src_r->count * 2,
                                           sizeof(mir_type_t));

         for (int i = 0; i < src_r->count; i++) {
            mir_type_t pointer = mir_get_var_pointer(mu, src_r->fields[i]);
            dst_r->fields[i] = src->u.record.fields[i];
            dst_r->fields[i + src_r->count] = pointer;
         }
      }
      break;

   case MIR_TYPE_ACCESS:
      {
         const mir_access_type_t *src_a = &src->u.access;
         mir_access_type_t *dst_a = &dst->u.access;

         dst_a->to      = src_a->to;
         dst_a->pointer = mir_pointer_type(mu, src_a->to);
      }
      break;

   case MIR_TYPE_SIGNAL:
      {
         const mir_signal_type_t *src_s = &src->u.signal;
         mir_signal_type_t *dst_s = &dst->u.signal;

         dst_s->base    = src_s->base;
         dst_s->pointer = mir_pointer_type(mu, src_s->base);
      }
      break;

   default:
      dst->u = src->u;
   }
}

__attribute__((always_inline))
static inline type_slot_t mir_type_slot(const type_tab_t *tab, int pos)
{
   return (type_slot_t) {
      .bits = load_acquire(&tab->hashtab[pos].bits)
   };
}

static void mir_copy_table(type_tab_t *from, type_tab_t *to)
{
   uint32_t max_id = 0;
   for (int i = 0; i < from->max_types; i++) {
      for (;;) {
         const type_slot_t slot = mir_type_slot(from, i);
         switch (slot.marker) {
         case BUSY_MARKER:
            spin_wait();
            continue;
         case INUSE_MARKER:
            {
               const type_data_t *from_td = &(from->types[slot.id]);
               type_data_t *to_td = &(to->types[slot.id]);

               max_id = MAX((uint32_t)slot.id, max_id);

               for (int pos = from_td->hash & (to->max_types - 1);;
                    pos = (pos + 1) & (to->max_types - 1)) {
                  const type_slot_t exist = mir_type_slot(to, pos);
                  if (exist.marker == FREE_MARKER) {
                     *to_td = *from_td;
                     store_release(&(to->hashtab[pos].bits), slot.bits);
                     break;
                  }
                  else
                     assert(exist.marker == INUSE_MARKER);
               }
            }
            break;
         case FREE_MARKER:
            break;
         default:
            should_not_reach_here();
         }

         const type_slot_t moved = { .marker = MOVED_MARKER, .id = slot.id };
         if (atomic_cas(&(from->hashtab[i].bits), slot.bits, moved.bits))
            break;
         else
            assert(slot.marker == FREE_MARKER);   // Raced to move free slot
      }
   }

   atomic_store(&to->next_id, max_id + 1);
}

static void mir_wait_for_resize(mir_context_t *mc)
{
   for (;;) {
      type_tab_t *from = atomic_load(&mc->typetab);
      type_tab_t *to = atomic_load(&mc->resizing);

      if (from == to)
         break;

      thread_sleep(10);
   }
}

static void mir_grow_table(mir_context_t *mc, type_tab_t *cur)
{
   type_tab_t *newtab =
      xcalloc_flex(sizeof(type_tab_t), cur->max_types * 2, sizeof(type_data_t));
   newtab->max_types = cur->max_types * 2;
   newtab->hashtab = xcalloc_array(newtab->max_types, sizeof(uint32_t));

   if (atomic_cas(&mc->resizing, cur, newtab)) {
      mir_copy_table(cur, newtab);

      assert(atomic_load(&mc->resizing) == newtab);

      if (!atomic_cas(&mc->typetab, cur, newtab))
         should_not_reach_here();

      async_free(cur->hashtab);
      async_free(cur);
   }
   else {
      free(newtab);
      mir_wait_for_resize(mc);
   }
}

static mir_type_t mir_try_build_type(mir_unit_t *mu, const type_data_t *td,
                                     uint32_t hash)
{
   mir_context_t *mc = mu->context;
   type_tab_t *tab = load_acquire(&mc->typetab);

   assert(is_power_of_2(tab->max_types));

   if (tab->next_id > tab->max_types / 2) {
      mir_grow_table(mc, tab);
      return MIR_NULL_TYPE;
   }

   for (int pos = hash & (tab->max_types - 1);;
        pos = (pos + 1) & (tab->max_types - 1)) {
      const type_slot_t slot = mir_type_slot(tab, pos);
      switch (slot.marker) {
      case BUSY_MARKER:
         spin_wait();
         return MIR_NULL_TYPE;
      case FREE_MARKER:
         {
            // Copy the type data before CAS-ing the slot to busy since
            // mir_install_type may allocate new types and deadlock
            type_data_t copy;
            mir_install_type(mu, &copy, td, hash);

            const type_slot_t busy = { .marker = BUSY_MARKER };
            if (atomic_cas(&tab->hashtab[pos].bits, slot.bits, busy.bits)) {
               const uint32_t id = atomic_fetch_add(&tab->next_id, 1);
               const type_slot_t new = { .marker = INUSE_MARKER, .id = id };
               if (unlikely(new.id != id))
                  fatal_trace("too many types");

               tab->types[id] = copy;
               store_release(&tab->hashtab[pos].bits, new.bits);
               return (mir_type_t){ .tag = MIR_TAG_TYPE, .id = id };
            }
            else {   // Raced for this slot, leak copy for now
               spin_wait();
               return MIR_NULL_TYPE;
            }
         }
      case INUSE_MARKER:
         if (hash != tab->types[slot.id].hash)
            break;
         else if (mir_compare_types(td, &(tab->types[slot.id])))
            return (mir_type_t){ .tag = MIR_TAG_TYPE, .id = slot.id };
         break;
      case MOVED_MARKER:
         mir_wait_for_resize(mc);
         return MIR_NULL_TYPE;
      }
   }
}

static mir_type_t mir_build_type(mir_unit_t *mu, const type_data_t *td)
{
   const uint32_t hash = mir_hash_type(mu, td);

   mir_type_t type;
   do {
      type = mir_try_build_type(mu, td, hash);
   } while (mir_is_null(type));

   return type;
}

mir_type_t mir_int_type(mir_unit_t *mu, int64_t low, int64_t high)
{
   mir_repr_t repr;
   switch (bits_for_range(low, high)) {
   case 64: repr = low < 0 ? MIR_REPR_I64 : MIR_REPR_U64; break;
   case 32: repr = low < 0 ? MIR_REPR_I32 : MIR_REPR_U32; break;
   case 16: repr = low < 0 ? MIR_REPR_I16 : MIR_REPR_U16; break;
   case 8:  repr = low < 0 ? MIR_REPR_I8 : MIR_REPR_U8; break;
   case 1:  repr = MIR_REPR_U1; break;
   case 0:  repr = MIR_REPR_I64; break;    // Null range
   default:
      fatal_trace("cannot represent %"PRIi64"..%"PRIi64, low, high);
   }

   const type_data_t td = {
      .class = MIR_TYPE_INT,
      .repr = repr,
      .u = { .intg = { .low = low, .high = high } }
   };

   return mir_build_type(mu, &td);
}

mir_type_t mir_real_type(mir_unit_t *mu, double low, double high)
{
   const type_data_t td = {
      .class = MIR_TYPE_REAL,
      .u = { .real = { .low = low, .high = high } }
   };

   return mir_build_type(mu, &td);
}

mir_type_t mir_offset_type(mir_unit_t *mu)
{
   if (mir_is_null(mu->types.offset_type)) {
      const type_data_t td = {
         .class = MIR_TYPE_OFFSET,
         .repr = MIR_REPR_I64,
         .u = { .intg = { .low = INT64_MIN, .high = INT64_MAX } }
      };

      mu->types.offset_type = mir_build_type(mu, &td);
   }

   return mu->types.offset_type;
}

mir_type_t mir_locus_type(mir_unit_t *mu)
{
   if (mir_is_null(mu->types.locus_type)) {
      const type_data_t td = { .class = MIR_TYPE_LOCUS };
      mu->types.locus_type = mir_build_type(mu, &td);
   }

   return mu->types.locus_type;
}

mir_type_t mir_conversion_type(mir_unit_t *mu)
{
   if (mir_is_null(mu->types.conversion_type)) {
      const type_data_t td = { .class = MIR_TYPE_CONVERSION };
      mu->types.conversion_type = mir_build_type(mu, &td);
   }

   return mu->types.conversion_type;
}

mir_type_t mir_trigger_type(mir_unit_t *mu)
{
   if (mir_is_null(mu->types.trigger_type)) {
      const type_data_t td = { .class = MIR_TYPE_TRIGGER };
      mu->types.trigger_type = mir_build_type(mu, &td);
   }

   return mu->types.trigger_type;
}

mir_type_t mir_self_type(mir_unit_t *mu)
{
   if (mir_is_null(mu->types.self_type)) {
      const type_data_t td = {
         .class = MIR_TYPE_CONTEXT,
         .u.context = mu->name,
      };

      mu->types.self_type = mir_build_type(mu, &td);
   }

   return mu->types.self_type;
}

mir_type_t mir_bool_type(mir_unit_t *mu)
{
   if (mir_is_null(mu->types.bool_type))
      mu->types.bool_type = mir_int_type(mu, 0, 1);

   return mu->types.bool_type;
}

mir_type_t mir_time_type(mir_unit_t *mu)
{
   if (mir_is_null(mu->types.time_type))
      mu->types.time_type = mir_int_type(mu, INT64_MIN, INT64_MAX);

   return mu->types.time_type;
}

mir_type_t mir_logic_type(mir_unit_t *mu)
{
   if (mir_is_null(mu->types.logic_type))
      mu->types.logic_type = mir_int_type(mu, 0, UINT8_MAX);

   return mu->types.logic_type;
}

mir_type_t mir_double_type(mir_unit_t *mu)
{
   if (mir_is_null(mu->types.double_type))
      mu->types.double_type = mir_real_type(mu, -DBL_MAX, DBL_MAX);

   return mu->types.double_type;
}

mir_type_t mir_char_type(mir_unit_t *mu)
{
   if (mir_is_null(mu->types.char_type))
      mu->types.char_type = mir_int_type(mu, 0, UINT8_MAX);

   return mu->types.char_type;
}

mir_type_t mir_string_type(mir_unit_t *mu)
{
   if (mir_is_null(mu->types.string_type))
      mu->types.string_type = mir_uarray_type(mu, 1, mir_char_type(mu));

   return mu->types.string_type;
}

mir_type_t mir_pointer_type(mir_unit_t *mu, mir_type_t to)
{
   MIR_ASSERT(mir_get_class(mu, to) != MIR_TYPE_CARRAY,
              "cannot get pointer to carray type");

   const type_data_t td = {
      .class = MIR_TYPE_POINTER,
      .u = { .pointer = to }
   };

   return mir_build_type(mu, &td);
}

mir_type_t mir_access_type(mir_unit_t *mu, mir_type_t to)
{
   const type_data_t td = {
      .class = MIR_TYPE_ACCESS,
      .u = { .access = { .to = to } },
   };

   return mir_build_type(mu, &td);
}

mir_type_t mir_opaque_type(mir_unit_t *mu)
{
   const type_data_t td = {
      .class = MIR_TYPE_OPAQUE,
   };

   return mir_build_type(mu, &td);
}

mir_type_t mir_record_type(mir_unit_t *mu, ident_t name,
                           const mir_type_t *fields, unsigned count)
{
   const type_data_t td = {
      .class = MIR_TYPE_RECORD,
      .u = {
         .record = {
            .name   = name,
            .fields = (mir_type_t *)fields,   // Copied in mir_install_type
            .count  = count
         }
      }
   };

   return mir_build_type(mu, &td);
}

mir_type_t mir_closure_type(mir_unit_t *mu, mir_type_t atype, mir_type_t rtype)
{
   const type_data_t td = {
      .class = MIR_TYPE_CLOSURE,
      .u = {
         .closure = {
            .rtype = rtype
         }
      },
   };

   return mir_build_type(mu, &td);
}

mir_type_t mir_resolution_type(mir_unit_t *mu, mir_type_t base)
{
   const type_data_t td = {
      .class = MIR_TYPE_RESOLUTION,
      .u = { .base = base },
   };

   return mir_build_type(mu, &td);
}

mir_type_t mir_file_type(mir_unit_t *mu, mir_type_t base)
{
   const type_data_t td = {
      .class = MIR_TYPE_FILE,
      .u = { .base = base },
   };

   return mir_build_type(mu, &td);
}

mir_type_t mir_carray_type(mir_unit_t *mu, int size, mir_type_t elem)
{
   MIR_ASSERT(mir_get_class(mu, elem) != MIR_TYPE_CARRAY,
              "array types may not be nested");

   const type_data_t td = {
      .class = MIR_TYPE_CARRAY,
      .u = {
         .carray = {
            .size = size,
            .elem = elem,
         }
      }
   };

   return mir_build_type(mu, &td);
}

mir_type_t mir_uarray_type(mir_unit_t *mu, int ndim, mir_type_t elem)
{
   MIR_ASSERT(mir_get_class(mu, elem) != MIR_TYPE_CARRAY,
              "array types may not be nested");

   const type_data_t td = {
      .class = MIR_TYPE_UARRAY,
      .u = {
         .uarray = {
            .dims = ndim,
            .elem = elem,
         }
      }
   };

   return mir_build_type(mu, &td);
}

mir_type_t mir_signal_type(mir_unit_t *mu, mir_type_t base)
{
   const type_data_t td = {
      .class = MIR_TYPE_SIGNAL,
      .u = { .signal = { .base = base } }
   };

   return mir_build_type(mu, &td);
}

mir_type_t mir_context_type(mir_unit_t *mu, ident_t name)
{
   const type_data_t td = {
      .class = MIR_TYPE_CONTEXT,
      .u = { .context = name }
   };

   return mir_build_type(mu, &td);
}

mir_type_t mir_vec2_type(mir_unit_t *mu, int size, bool issigned)
{
   assert(size >= 0);

   const type_data_t td = {
      .class = MIR_TYPE_VEC2,
      .u = { .vec = { .size = size, .issigned = issigned } }
   };

   return mir_build_type(mu, &td);
}

mir_type_t mir_vec4_type(mir_unit_t *mu, int size, bool issigned)
{
   assert(size >= 0);

   const type_data_t td = {
      .class = MIR_TYPE_VEC4,
      .u = { .vec = { .size = size, .issigned = issigned } }
   };

   return mir_build_type(mu, &td);
}

mir_type_t mir_vector_slice(mir_unit_t *mu, mir_type_t base, int size)
{
   assert(size >= 0);

   const type_data_t *td = mir_type_data(mu, base);
   assert(td->class == MIR_TYPE_VEC2 || td->class == MIR_TYPE_VEC4);
   assert(size <= td->u.vec.size);

   const type_data_t new = {
      .class = td->class,
      .u = { .vec = { .size = size, .issigned = td->u.vec.issigned } }
   };

   return mir_build_type(mu, &new);
}

mir_type_t mir_get_base(mir_unit_t *mu, mir_type_t type)
{
   const type_data_t *td = mir_type_data(mu, type);
   switch (td->class) {
   case MIR_TYPE_SIGNAL:
      return td->u.signal.base;
   case MIR_TYPE_RESOLUTION:
   case MIR_TYPE_FILE:
      return td->u.base;
   default:
      return MIR_NULL_TYPE;
   }
}

mir_type_t mir_get_elem(mir_unit_t *mu, mir_type_t type)
{
   const type_data_t *td = mir_type_data(mu, type);
   switch (td->class) {
   case MIR_TYPE_UARRAY:
      return td->u.uarray.elem;
   case MIR_TYPE_CARRAY:
      return td->u.carray.elem;
   case MIR_TYPE_ACCESS:
      return td->u.access.to;
   case MIR_TYPE_POINTER:
      return td->u.pointer;
   default:
      return MIR_NULL_TYPE;
   }
}

mir_type_t mir_get_pointer(mir_unit_t *mu, mir_type_t type)
{
   const type_data_t *td = mir_type_data(mu, type);
   switch (td->class) {
   case MIR_TYPE_SIGNAL:
      return td->u.signal.pointer;
   case MIR_TYPE_POINTER:
      return td->u.pointer;
   case MIR_TYPE_UARRAY:
      return td->u.uarray.pointer;
   case MIR_TYPE_CARRAY:
      return td->u.carray.pointer;
   case MIR_TYPE_ACCESS:
      return td->u.access.pointer;
   default:
      return MIR_NULL_TYPE;
   }
}

mir_type_t mir_get_var_pointer(mir_unit_t *mu, mir_type_t type)
{
   const type_data_t *td = mir_type_data(mu, type);
   switch (td->class) {
   case MIR_TYPE_CARRAY:
      return td->u.carray.pointer;
   default:
      return mir_pointer_type(mu, type);
   }
}

unsigned mir_get_dims(mir_unit_t *mu, mir_type_t type)
{
   const type_data_t *td = mir_type_data(mu, type);
   switch (td->class) {
   case MIR_TYPE_UARRAY:
      return td->u.uarray.dims;
   default:
      should_not_reach_here();
   }
}

unsigned mir_get_size(mir_unit_t *mu, mir_type_t type)
{
   const type_data_t *td = mir_type_data(mu, type);
   switch (td->class) {
   case MIR_TYPE_CARRAY:
      return td->u.carray.size;
   case MIR_TYPE_VEC2:
   case MIR_TYPE_VEC4:
      return td->u.vec.size;
   default:
      should_not_reach_here();
   }
}

unsigned mir_get_slots(mir_unit_t *mu, mir_type_t type)
{
   if (mir_is_null(type))
      return 1;   // Untyped constants

   const type_data_t *td = mir_type_data(mu, type);
   switch (td->class) {
   case MIR_TYPE_UARRAY:
      // Always passed around scalarised
      if (mir_get_class(mu, td->u.uarray.elem) == MIR_TYPE_SIGNAL)
         return 2 + td->u.uarray.dims * 2;
      else
         return 1 + td->u.uarray.dims * 2;
   case MIR_TYPE_SIGNAL:
      // Signal pointer plus offset
      return 2;
   case MIR_TYPE_CLOSURE:
      // Function pointer, context
      return 2;
   case MIR_TYPE_RESOLUTION:
      // Closure slots plus nlits, and flags (this is silly)
      return 4;
   case MIR_TYPE_VEC4:
      return 2;
   default:
      // Passed by pointer or fits in 64-bit register
      return 1;
   }
}

bool mir_get_signed(mir_unit_t *mu, mir_type_t type)
{
   const type_data_t *td = mir_type_data(mu, type);
   switch (td->class) {
   case MIR_TYPE_VEC2:
   case MIR_TYPE_VEC4:
      return td->u.vec.issigned;
   default:
      should_not_reach_here();
   }
}

mir_class_t mir_get_class(mir_unit_t *mu, mir_type_t type)
{
   return mir_type_data(mu, type)->class;
}

mir_repr_t mir_get_repr(mir_unit_t *mu, mir_type_t type)
{
   const type_data_t *td = mir_type_data(mu, type);
   switch (td->class) {
   case MIR_TYPE_INT:
   case MIR_TYPE_OFFSET:
      return td->repr;
   default:
      should_not_reach_here();
   }
}

const mir_type_t *mir_get_fields(mir_unit_t *mu, mir_type_t type, size_t *count)
{
   const type_data_t *td = mir_type_data(mu, type);
   assert(td->class == MIR_TYPE_RECORD);

   if (count != NULL) *count = td->u.record.count;
   return td->u.record.fields;
}

static uint32_t mir_hash_stamp(mir_unit_t *mu, const stamp_data_t *sd)
{
   uint32_t h = sd->kind;

   switch (sd->kind) {
   case _MIR_INVALID_STAMP:
      break;

   case MIR_STAMP_INT:
      h ^= mix_bits_64(sd->u.intg.low) >> 32;
      h ^= mix_bits_64(sd->u.intg.high);
      break;

   case MIR_STAMP_REAL:
      h ^= mix_bits_64(FLOAT_BITS(sd->u.real.low)) >> 32;
      h ^= mix_bits_64(FLOAT_BITS(sd->u.real.high));
      break;

   case MIR_STAMP_POINTER:
      h += knuth_hash(sd->u.pointer.memory);
      if (!mir_is_null(sd->u.pointer.elem))
         h ^= mir_stamp_data(mu, sd->u.pointer.elem)->hash;
      break;
   }

   return h;
}

static bool mir_compare_stamps(const stamp_data_t *a, const stamp_data_t *b)
{
   if (a->kind != b->kind)
      return false;

   switch (a->kind) {
   case _MIR_INVALID_STAMP:
      return false;

   case MIR_STAMP_INT:
      return a->u.intg.low == b->u.intg.low
         && a->u.intg.high == b->u.intg.high;

   case MIR_STAMP_REAL:
      return a->u.real.low == b->u.real.low
         && a->u.real.high == b->u.real.high;

   case MIR_STAMP_POINTER:
      return a->u.pointer.memory == b->u.pointer.memory
         && a->u.pointer.elem.bits == b->u.pointer.elem.bits;
   }

   should_not_reach_here();
}

static mir_stamp_t mir_build_stamp(mir_unit_t *mu, const stamp_data_t *sd)
{
   const uint32_t hash = mir_hash_stamp(mu, sd);

   for (int i = 0; i < mu->stamps.count; i++) {
      const stamp_data_t *other = &(mu->stamps.items[i]);
      if (other->hash == hash && mir_compare_stamps(other, sd))
         return (mir_stamp_t){ .tag = MIR_TAG_STAMP, .id = i };
   }

   mir_stamp_t s = { .tag = MIR_TAG_STAMP, .id  = mu->stamps.count };

   stamp_data_t new = {
      .kind = sd->kind,
      .hash = hash,
      .u    = sd->u,
   };
   APUSH(mu->stamps, new);

   return s;
}

const stamp_data_t *mir_stamp_data(mir_unit_t *mu, mir_stamp_t stamp)
{
   assert(stamp.tag == MIR_TAG_STAMP);
   return AREF(mu->stamps, stamp.id);
}

mir_stamp_t mir_int_stamp(mir_unit_t *mu, int64_t low, int64_t high)
{
   assert(low <= high);

   const stamp_data_t sd = {
      .kind = MIR_STAMP_INT,
      .u = { .intg = { .low = low, .high = high } }
   };

   return mir_build_stamp(mu, &sd);
}

mir_stamp_t mir_real_stamp(mir_unit_t *mu, double low, double high)
{
   assert(low <= high);

   const stamp_data_t sd = {
      .kind = MIR_STAMP_REAL,
      .u = { .real = { .low = low, .high = high } }
   };

   return mir_build_stamp(mu, &sd);
}

mir_stamp_t mir_pointer_stamp(mir_unit_t *mu, mir_mem_t mem, mir_stamp_t elem)
{
   const stamp_data_t sd = {
      .kind = MIR_STAMP_POINTER,
      .u = { .pointer = { .memory = mem, .elem = elem } },
   };

   return mir_build_stamp(mu, &sd);
}

bool mir_is_top(mir_unit_t *mu, mir_type_t type, mir_stamp_t stamp)
{
   const type_data_t *td = mir_type_data(mu, type);
   const stamp_data_t *sd = mir_stamp_data(mu, stamp);

   switch (td->class) {
   case MIR_TYPE_INT:
   case MIR_TYPE_OFFSET:
      return sd->u.intg.low <= td->u.intg.low
         && sd->u.intg.high >= td->u.intg.high;

   default:
      return false;
   }
}

mir_stamp_t mir_stamp_elem(mir_unit_t *mu, mir_stamp_t stamp)
{
   if (mir_is_null(stamp))
      return MIR_NULL_STAMP;

   const stamp_data_t *sd = mir_stamp_data(mu, stamp);
   switch (sd->kind) {
   case MIR_STAMP_POINTER:
      return sd->u.pointer.elem;
   default:
      return MIR_NULL_STAMP;
   }
}

mir_stamp_t mir_stamp_add(mir_unit_t *mu, mir_stamp_t left, mir_stamp_t right)
{
   if (mir_is_null(left) || mir_is_null(right))
      return MIR_NULL_STAMP;

   const stamp_data_t *lsd = mir_stamp_data(mu, left);
   const stamp_data_t *rsd = mir_stamp_data(mu, right);

   assert(lsd->kind == rsd->kind);

   switch (lsd->kind) {
   case MIR_STAMP_INT:
      {
         const int64_t low  = sadd64(lsd->u.intg.low, rsd->u.intg.low);
         const int64_t high = sadd64(lsd->u.intg.high, rsd->u.intg.high);

         return mir_int_stamp(mu, low, high);
      }

   case MIR_STAMP_REAL:
      {
         const double low  = lsd->u.real.low + rsd->u.real.low;
         const double high = lsd->u.real.high + rsd->u.real.high;

         return mir_real_stamp(mu, low, high);
      }

   default:
      should_not_reach_here();
   }
}

mir_stamp_t mir_stamp_sub(mir_unit_t *mu, mir_stamp_t left, mir_stamp_t right)
{
   if (mir_is_null(left) || mir_is_null(right))
      return MIR_NULL_STAMP;

   const stamp_data_t *lsd = mir_stamp_data(mu, left);
   const stamp_data_t *rsd = mir_stamp_data(mu, right);

   assert(lsd->kind == rsd->kind);

   switch (lsd->kind) {
   case MIR_STAMP_INT:
      {
         const int64_t low  = ssub64(lsd->u.intg.low, rsd->u.intg.high);
         const int64_t high = ssub64(lsd->u.intg.high, rsd->u.intg.low);

         return mir_int_stamp(mu, low, high);
      }

   case MIR_STAMP_REAL:
      {
         const double low  = lsd->u.real.low - rsd->u.real.high;
         const double high = lsd->u.real.high - rsd->u.real.low;

         return mir_real_stamp(mu, low, high);
      }

   default:
      should_not_reach_here();
   }
}

mir_stamp_t mir_stamp_mul(mir_unit_t *mu, mir_stamp_t left, mir_stamp_t right)
{
   if (mir_is_null(left) || mir_is_null(right))
      return MIR_NULL_STAMP;

   const stamp_data_t *lsd = mir_stamp_data(mu, left);
   const stamp_data_t *rsd = mir_stamp_data(mu, right);

   assert(lsd->kind == rsd->kind);

   switch (lsd->kind) {
   case MIR_STAMP_INT:
      {
         const int64_t ll = smul64(lsd->u.intg.low, rsd->u.intg.low);
         const int64_t lh = smul64(lsd->u.intg.low, rsd->u.intg.high);
         const int64_t hl = smul64(lsd->u.intg.high, rsd->u.intg.low);
         const int64_t hh = smul64(lsd->u.intg.high, rsd->u.intg.high);

         int64_t min = MIN(MIN(ll, lh), MIN(hl, hh));
         int64_t max = MAX(MAX(ll, lh), MAX(hl, hh));

         return mir_int_stamp(mu, min, max);
      }

   case MIR_STAMP_REAL:
      {
         const double ll = lsd->u.real.low * rsd->u.real.low;
         const double lh = lsd->u.real.low * rsd->u.real.high;
         const double hl = lsd->u.real.high * rsd->u.real.low;
         const double hh = lsd->u.real.high * rsd->u.real.high;

         double min = MIN(MIN(ll, lh), MIN(hl, hh));
         double max = MAX(MAX(ll, lh), MAX(hl, hh));

         return mir_real_stamp(mu, min, max);
      }

   default:
      should_not_reach_here();
   }
}

mir_stamp_t mir_stamp_div(mir_unit_t *mu, mir_stamp_t left, mir_stamp_t right)
{
   if (mir_is_null(left) || mir_is_null(right))
      return MIR_NULL_STAMP;

   return MIR_NULL_STAMP;  // TODO
}

mir_stamp_t mir_stamp_rem(mir_unit_t *mu, mir_stamp_t left, mir_stamp_t right)
{
   if (mir_is_null(left) || mir_is_null(right))
      return MIR_NULL_STAMP;

   const stamp_data_t *lsd = mir_stamp_data(mu, left);
   const stamp_data_t *rsd = mir_stamp_data(mu, right);

   assert(lsd->kind == rsd->kind);

   switch (lsd->kind) {
   case MIR_STAMP_INT:
      if (lsd->u.intg.low >= 0 && rsd->u.intg.high > 0)
         return mir_int_stamp(mu, 0, rsd->u.intg.high - 1);
      else
         return MIR_NULL_STAMP;

   default:
      should_not_reach_here();
   }
}

mir_stamp_t mir_stamp_cmp(mir_unit_t *mu, mir_cmp_t cmp, mir_stamp_t left,
                          mir_stamp_t right)
{
   if (mir_is_null(left) || mir_is_null(right))
      return MIR_NULL_STAMP;

   const stamp_data_t *lsd = mir_stamp_data(mu, left);
   const stamp_data_t *rsd = mir_stamp_data(mu, right);

   assert(lsd->kind == rsd->kind);

   switch (lsd->kind) {
   case MIR_STAMP_INT:
      {
         const int64_t a = lsd->u.intg.low;
         const int64_t b = lsd->u.intg.high;
         const int64_t c = rsd->u.intg.low;
         const int64_t d = rsd->u.intg.high;

         int trueval = 1;
         switch (cmp) {
         case MIR_CMP_EQ:
            trueval = 0;
            // Fall-through
         case MIR_CMP_NEQ:
            if (MAX(a, c) <= MIN(b, d))
               return MIR_NULL_STAMP;    // Ranges overlap
            else
               return mir_int_stamp(mu, trueval, trueval);

         case MIR_CMP_GEQ:
            trueval = 0;
            // Fall-through
         case MIR_CMP_LT:
            if (b < c)
               return mir_int_stamp(mu, trueval, trueval);
            else if (a >= d)
               return mir_int_stamp(mu, !trueval, !trueval);
            else
               return MIR_NULL_STAMP;

         case MIR_CMP_LEQ:
            trueval = 0;
            // Fall-through
         case MIR_CMP_GT:
            if (a > d)
               return mir_int_stamp(mu, trueval, trueval);
            else if (b <= c)
               return mir_int_stamp(mu, !trueval, !trueval);
            else
               return MIR_NULL_STAMP;

         default:
            should_not_reach_here();
         }
      }

   case MIR_STAMP_REAL:
      return MIR_NULL_STAMP;

   default:
      should_not_reach_here();
   }
}

mir_stamp_t mir_stamp_union(mir_unit_t *mu, mir_stamp_t left, mir_stamp_t right)
{
   if (mir_is_null(left))
      return right;
   else if (mir_is_null(right))
      return left;

   const stamp_data_t *lsd = mir_stamp_data(mu, left);
   const stamp_data_t *rsd = mir_stamp_data(mu, right);

   assert(lsd->kind == rsd->kind);

   switch (lsd->kind) {
   case MIR_STAMP_INT:
      {
         const int64_t low  = MIN(lsd->u.intg.low, rsd->u.intg.low);
         const int64_t high = MAX(lsd->u.intg.high, rsd->u.intg.high);

         return mir_int_stamp(mu, low, high);
      }

   case MIR_STAMP_REAL:
      {
         const double low  = MIN(lsd->u.real.low, rsd->u.real.low);
         const double high = MAX(lsd->u.real.high, rsd->u.real.high);

         return mir_real_stamp(mu, low, high);
      }

   default:
      should_not_reach_here();
   }
}

mir_stamp_t mir_stamp_cast(mir_unit_t *mu, mir_type_t type, mir_stamp_t stamp)
{
   if (mir_is_null(stamp))
      return stamp;

   const type_data_t *td = mir_type_data(mu, type);
   const stamp_data_t *sd = mir_stamp_data(mu, stamp);

   switch (td->class) {
   case MIR_TYPE_OFFSET:
   case MIR_TYPE_INT:
      if (sd->kind == MIR_STAMP_INT)
         return mir_int_stamp(mu, MAX(td->u.intg.low, sd->u.intg.low),
                              MIN(td->u.intg.high, sd->u.intg.high));
      else
         return MIR_NULL_STAMP;

   default:
      return MIR_NULL_STAMP;
   }
}

bool mir_stamp_const(mir_unit_t *mu, mir_stamp_t stamp, int64_t *cval)
{
   if (mir_is_null(stamp))
      return false;

   const stamp_data_t *sd = mir_stamp_data(mu, stamp);

   if (sd->kind != MIR_STAMP_INT || sd->u.intg.low != sd->u.intg.high)
      return false;

   *cval = sd->u.intg.low;
   return true;
}

bool mir_same_type(mir_unit_t *mu, mir_type_t a, mir_type_t b)
{
   if (mir_is_null(a) && mir_is_null(b))
      return true;
   else if (mir_is_null(a)) {
      const type_data_t *btd = mir_type_data(mu, b);
      return btd->class == MIR_TYPE_OFFSET || btd->class == MIR_TYPE_INT;
   }
   else if (mir_is_null(b)) {
      const type_data_t *atd = mir_type_data(mu, a);
      return atd->class == MIR_TYPE_OFFSET || atd->class == MIR_TYPE_INT;
   }
   else
      return mir_equals(a, b);
}

void mir_free_types(type_tab_t *tab)
{
#ifdef DEBUG
   for (int i = 0; i < tab->max_types; i++) {
      const type_slot_t islot = mir_type_slot(tab, i);
      assert(islot.marker == FREE_MARKER || islot.marker == INUSE_MARKER);

      if (islot.marker == INUSE_MARKER) {
         const type_data_t *itd = &(tab->types[islot.id]);
         for (int j = 0; j < i; j++) {
            const type_slot_t jslot = mir_type_slot(tab, j);
            if (jslot.marker == INUSE_MARKER) {
               const type_data_t *jtd = &(tab->types[jslot.id]);
               if (mir_compare_types(itd, jtd))
                  fatal_trace("duplicate types in table at %d (%d => %08x) "
                              "and %d (%d => %08x)", i, islot.id, itd->hash,
                              j, jslot.id, jtd->hash);
            }
         }
      }
   }
#endif

   free(tab->hashtab);
   free(tab);
}
