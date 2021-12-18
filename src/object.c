//
//  Copyright (C) 2014-2021  Nick Gasson
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

#include "object.h"
#include "common.h"
#include "hash.h"

#include <string.h>
#include <stdlib.h>
#include <inttypes.h>

typedef uint64_t mark_mask_t;

typedef A(object_arena_t *) arena_array_t;
typedef A(object_t **) object_ptr_array_t;

typedef enum { OBJ_DISK, OBJ_FRESH } obj_src_t;

typedef struct _object_arena {
   void           *base;
   void           *alloc;
   void           *limit;
   bool            frozen;
   mark_mask_t    *mark_bits;
   size_t          mark_sz;
   generation_t    generation;
   arena_key_t     key;
   arena_array_t   deps;
   obj_src_t       source;
   vhdl_standard_t std;
} object_arena_t;

static const char *item_text_map[] = {
   "I_IDENT",    "I_VALUE",     "I_SEVERITY", "I_MESSAGE",    "I_TARGET",
   "I_LITERAL",  "I_IDENT2",    "I_DECLS",    "I_STMTS",      "I_PORTS",
   "I_GENERICS", "I_PARAMS",    "I_GENMAPS",  "I_WAVES",      "I_CONDS",
   "I_TYPE",     "I_SUBKIND",   "I_DELAY",    "I_REJECT",     "I_POS",
   "I_REF",      "I_FILE_MODE", "I_ASSOCS",   "I_CONTEXT",    "I_TRIGGERS",
   "I_ELSES",    "I_CLASS",     "I_RANGES",   "I_NAME",       "I_EOPT",
   "I_DVAL",     "I_SPEC",      "I_SCOPES",   "I_INDEXCON",   "I_BASE",
   "I_ELEM",     "I_FILE",      "I_ACCESS",   "I_RESOLUTION", "I_RESULT",
   "I_UNITS",    "I_LITERALS",  "I_DIMS",     "I_FIELDS",     "I_PARENT",
   "I_GUARD",    "I_PTYPES",    "I_CHARS",    "I_CONSTR",     "I_FLAGS",
   "I_SIGNALS",  "I_LEFT",      "I_RIGHT",    "I_PROCS",      "I_NEXUS",
   "I_PATH",     "I_DEPS",      "I_SIZE",     "I_VCODE",      "I_PRIMARY",
   "I_SOURCES",  "I_OUTPUTS",
};


static object_class_t     *classes[4];
static uint32_t            format_digest;
static generation_t        next_generation = 1;
static arena_array_t       all_arenas;
static object_ptr_array_t  global_roots;

#if __SANITIZE_ADDRESS__
static void object_purge_at_exit(void);
#endif

static inline bool object_in_arena_p(object_arena_t *arena, object_t *object)
{
   return (void *)object >= arena->base && (void *)object < arena->limit;
}

static inline object_arena_t *__object_arena(object_t *object)
{
   assert(object->arena < all_arenas.count);
   assert(object->arena != 0);
   return all_arenas.items[object->arena];
}

static ident_t object_arena_name(object_arena_t *arena)
{
   if (arena->alloc > arena->base) {
      object_t *root = (object_t *)arena->base;

      const object_class_t *class = classes[root->tag];
      const imask_t has = class->has_map[root->kind];

      if (has & I_IDENT) {
         const int tzc = __builtin_ctzll(I_IDENT);
         const int off = (root->kind * 64) + tzc;
         const int n   = class->item_lookup[off];

         return root->items[n].ident;
      }
   }

   return ident_new("???");
}

static bool object_marked_p(object_t *object, generation_t generation)
{
   object_arena_t *arena = __object_arena(object);

   if (arena->mark_bits == NULL) {
      const size_t nbits = (arena->limit - arena->base) / OBJECT_ALIGN;
      arena->mark_sz = ALIGN_UP(nbits, 64) / 8;
      arena->mark_bits = xcalloc(arena->mark_sz);
      arena->generation = generation;
   }
   else if (arena->generation != generation) {
      memset(arena->mark_bits, '\0', arena->mark_sz);
      arena->generation = generation;
   }

   uintptr_t bit = ((void *)object - arena->base) >> OBJECT_ALIGN_BITS;
   uintptr_t word = bit / 64;
   uint64_t mask = 1ull << bit;

   const bool marked = !!(arena->mark_bits[word] & mask);
   arena->mark_bits[word] |= mask;

   return marked;
}

object_arena_t *object_arena(object_t *object)
{
   return __object_arena(object);
}

void __object_write_barrier(object_t *lhs, object_t *rhs)
{
   const uintptr_t lhs_mask = (uintptr_t)lhs & ~OBJECT_PAGE_MASK;
   const uintptr_t rhs_mask = (uintptr_t)rhs & ~OBJECT_PAGE_MASK;

   if (lhs_mask == rhs_mask || rhs == NULL)
      return;
   else if (lhs->arena == rhs->arena)
      return;

   object_arena_t *larena = __object_arena(lhs);
   object_arena_t *rarena = __object_arena(rhs);

   assert(!larena->frozen);
   assert(rarena->frozen);

   for (unsigned i = 0; i < larena->deps.count; i++) {
      if (larena->deps.items[i] == rarena)
         return;
   }

   APUSH(larena->deps, rarena);
}

void object_lookup_failed(const char *name, const char **kind_text_map,
                          int kind, imask_t mask)
{
   unsigned int item;
   for (item = 0; (mask & (1ull << item)) == 0; item++)
      ;

   assert(item < ARRAY_LEN(item_text_map));
   fatal_trace("%s kind %s does not have item %s", name,
               kind_text_map[kind], item_text_map[item]);
}

void item_without_type(imask_t mask)
{
   int item;
   for (item = 0; (mask & (1ull << item)) == 0; item++)
      ;

   assert(item < ARRAY_LEN(item_text_map));
   fatal_trace("item %s does not have a type", item_text_map[item]);
}

void object_change_kind(const object_class_t *class, object_t *object, int kind)
{
   if (kind == object->kind)
      return;

   bool allow = false;
   for (size_t i = 0; (class->change_allowed[i][0] != -1) && !allow; i++) {
      allow = (class->change_allowed[i][0] == object->kind)
         && (class->change_allowed[i][1] == kind);
   }

   if (!allow)
      fatal_trace("cannot change %s kind %s to %s", class->name,
                  class->kind_text_map[object->kind],
                  class->kind_text_map[kind]);

   const imask_t old_has = class->has_map[object->kind];
   const imask_t new_has = class->has_map[kind];

   const int old_nitems = class->object_nitems[object->kind];
   const int new_nitems = class->object_nitems[kind];

   const int max_items = MAX(old_nitems, new_nitems);

   item_t tmp[max_items];
   memcpy(tmp, object->items, sizeof(item_t) * max_items);

   int op = 0, np = 0;
   for (imask_t mask = 1; np < new_nitems; mask <<= 1) {
      if ((old_has & mask) && (new_has & mask))
         object->items[np++] = tmp[op++];
      else if (old_has & mask)
         ++op;
      else if (new_has & mask)
         memset(&(object->items[np++]), '\0', sizeof(item_t));
   }

   object->kind = kind;
}

static void object_init(object_class_t *class)
{
   class->object_size   = xmalloc_array(class->last_kind, sizeof(size_t));
   class->object_nitems = xmalloc_array(class->last_kind, sizeof(int));
   class->item_lookup   = xmalloc_array(class->last_kind, sizeof(int) * 64);

   assert(class->last_kind < (1 << (sizeof(uint8_t) * 8)));

   assert(class->tag < ARRAY_LEN(classes));
   classes[class->tag] = class;

   for (int i = 0; i < class->last_kind; i++) {
      const int nitems = __builtin_popcountll(class->has_map[i]);
      class->object_size[i]   = sizeof(object_t) + (nitems * sizeof(item_t));
      class->object_nitems[i] = nitems;

      // Knuth's multiplicative hash
      format_digest +=
         (uint32_t)(class->has_map[i] >> 32) * UINT32_C(2654435761);
      format_digest +=
         (uint32_t)(class->has_map[i]) * UINT32_C(2654435761);

      int n = 0;
      for (int j = 0; j < 64; j++) {
         if (class->has_map[i] & ONE_HOT(j))
            class->item_lookup[(i * 64) + j] = n++;
         else
            class->item_lookup[(i * 64) + j] = -1;
      }
   }

   bool changed = false;
   do {
      changed = false;
      for (int i = 0; i < class->last_kind; i++) {
         size_t max_size = class->object_size[i];
         for (size_t j = 0; class->change_allowed[j][0] != -1; j++) {
            if (class->change_allowed[j][0] == i)
               max_size = MAX(max_size,
                              class->object_size[class->change_allowed[j][1]]);
            else if (class->change_allowed[j][1] == i)
               max_size = MAX(max_size,
                              class->object_size[class->change_allowed[j][0]]);
         }

         if (max_size != class->object_size[i]) {
            class->object_size[i] = max_size;
            changed = true;
         }
      }
   } while (changed);

   if (getenv("NVC_TREE_SIZES") != NULL) {
      for (int i = 0; i < class->last_kind; i++)
         printf("%-15s %d\n", class->kind_text_map[i],
                (int)class->object_size[i]);
   }
}

void object_one_time_init(void)
{
   extern object_class_t tree_object;
   extern object_class_t type_object;
   extern object_class_t e_node_object;

   static bool done = false;

   if (unlikely(!done)) {
      object_init(&tree_object);
      object_init(&type_object);
      object_init(&e_node_object);

      // Increment this each time a incompatible change is made to the
      // on-disk format not expressed in the object items table
      const uint32_t format_fudge = 22;

      format_digest += format_fudge * UINT32_C(2654435761);

#if __SANITIZE_ADDRESS__
      atexit(object_purge_at_exit);
#endif

      done = true;
   }
}

object_t *object_new(object_arena_t *arena,
                     const object_class_t *class, int kind)
{
   if (unlikely(kind >= class->last_kind))
      fatal_trace("invalid kind %d for %s object", kind, class->name);

   object_one_time_init();

   if (unlikely(arena == NULL))
      fatal_trace("allocating object without active arena");

   const size_t size = ALIGN_UP(class->object_size[kind], OBJECT_ALIGN);

   const uintptr_t alloc_addr = (uintptr_t)arena->alloc;
   assert((alloc_addr & (OBJECT_ALIGN - 1)) == 0);

   if (unlikely(arena->limit - arena->alloc < size))
      fatal_trace("out of space in arena %s", istr(object_arena_name(arena)));

   object_t *object = arena->alloc;
   arena->alloc = (char *)arena->alloc + size;

   memset(object, '\0', size);

   object->kind  = kind;
   object->tag   = class->tag;
   object->arena = arena->key;
   object->loc   = LOC_INVALID;

   return object;
}

static void gc_forward_one_pointer(object_t **pobject, object_arena_t *arena,
                                   uint32_t *forward)
{
   if (*pobject != NULL && object_in_arena_p(arena, *pobject)) {
      ptrdiff_t index = ((void *)*pobject - arena->base) >> OBJECT_ALIGN_BITS;
      *pobject = (object_t *)((char *)arena->base + forward[index]);
   }
}

static void gc_forward_pointers(object_t *object, object_arena_t *arena,
                                const object_class_t *class, uint32_t *forward)
{
   const imask_t has = class->has_map[object->kind];
   const int nitems = class->object_nitems[object->kind];
   imask_t mask = 1;
   for (int i = 0; i < nitems; mask <<= 1) {
      if (has & mask) {
         if (ITEM_OBJECT & mask)
            gc_forward_one_pointer(&(object->items[i].object), arena, forward);
         else if (ITEM_OBJ_ARRAY & mask) {
            for (unsigned j = 0; j < object->items[i].obj_array.count; j++)
               gc_forward_one_pointer(&(object->items[i].obj_array.items[j]),
                                      arena, forward);
         }

         i++;
      }
   }
}

static void gc_mark_from_root(object_t *root, generation_t generation)
{
   object_visit_ctx_t ctx = {
      .count      = 0,
      .postorder  = NULL,
      .preorder   = NULL,
      .context    = NULL,
      .kind       = T_LAST_TREE_KIND,
      .generation = generation,
      .deep       = true
   };

   object_visit(root, &ctx);
}

static void gc_free_external(object_t *object)
{
   const object_class_t *class = classes[object->tag];
   const imask_t has = class->has_map[object->kind];
   const int nitems = class->object_nitems[object->kind];
   imask_t mask = 1;
   for (int i = 0; i < nitems; mask <<= 1) {
      if (has & mask) {
         if (ITEM_OBJ_ARRAY & mask)
            ACLEAR(object->items[i].obj_array);
         else if (ITEM_IDENT_ARRAY & mask)
            ACLEAR(object->items[i].ident_array);
         i++;
      }
   }
}

void object_arena_gc(object_arena_t *arena)
{
   const generation_t generation = object_next_generation();
   const uint64_t start_ticks = get_timestamp_us();

   // Mark
   for (void *p = arena->base; p != arena->alloc; ) {
      assert(p < arena->alloc);
      object_t *object = p;

      const object_class_t *class = classes[object->tag];

      bool top_level = false;
      for (int j = 0; (j < class->gc_num_roots) && !top_level; j++) {
         if (class->gc_roots[j] == object->kind)
            top_level = true;
      }

      if (top_level)
         gc_mark_from_root(object, generation);

      const size_t size =
         ALIGN_UP(class->object_size[object->kind], OBJECT_ALIGN);
      p = (char *)p + size;
   }

   for (unsigned i = 0; i < global_roots.count; i++)
      gc_mark_from_root(*(global_roots.items[i]), generation);

   // Calculate forwarding addresses
   const size_t fwdsz = (arena->alloc - arena->base) / OBJECT_ALIGN;
   uint32_t *forward LOCAL = xmalloc_array(fwdsz, sizeof(uint32_t));
   unsigned woffset = 0, live = 0, dead = 0;
   for (void *rptr = arena->base; rptr != arena->alloc; ) {
      assert(rptr < arena->alloc);
      object_t *object = rptr;

      const object_class_t *class = classes[object->tag];

      const size_t size =
         ALIGN_UP(class->object_size[object->kind], OBJECT_ALIGN);

      ptrdiff_t index = (rptr - arena->base) >> OBJECT_ALIGN_BITS;
      if (!object_marked_p(object, generation)) {
         forward[index] = UINT32_MAX;
         gc_free_external(object);
         dead++;
      }
      else {
         forward[index] = woffset;
         woffset += size;
         live++;
      }

      rptr = (char *)rptr + size;
   }

   if (dead == 0)
      goto skip_gc;
   else if (woffset == 0)
      fatal_trace("GC removed all objects from arena %s",
                  istr(object_arena_name(arena)));

   // Compact
   for (void *rptr = arena->base; rptr != arena->alloc; ) {
      assert(rptr < arena->alloc);
      object_t *object = rptr;

      const object_class_t *class = classes[object->tag];

      const size_t size =
         ALIGN_UP(class->object_size[object->kind], OBJECT_ALIGN);

      ptrdiff_t index = (rptr - arena->base) >> OBJECT_ALIGN_BITS;
      if (forward[index] != UINT32_MAX) {
         void *wptr = (char *)arena->base + forward[index];
         assert(wptr <= rptr);

         gc_forward_pointers(object, arena, class, forward);

         if (wptr != rptr) memmove(wptr, rptr, size);
      }

      rptr = (char *)rptr + size;
   }

   for (unsigned i = 0; i < global_roots.count; i++)
      gc_forward_one_pointer(global_roots.items[i], arena, forward);

   arena->alloc = (char *)arena->base + woffset;

 skip_gc:
   if ((getenv("NVC_GC_VERBOSE") != NULL) || is_debugger_running()) {
      const int ticks = get_timestamp_us() - start_ticks;
      notef("GC: %s: freed %d objects; %d allocated [%d us]",
            istr(object_arena_name(arena)), dead, live, ticks);
   }
}

void object_visit(object_t *object, object_visit_ctx_t *ctx)
{
   // If `deep' then will follow links above the tree originally passed
   // to tree_visit - e.g. following references back to their declarations

   if (object == NULL)
      return;
   else if (object_marked_p(object, ctx->generation))
      return;

   const bool visit =
      (object->tag == OBJECT_TAG_TREE && object->kind == ctx->kind)
      || ctx->kind == T_LAST_TREE_KIND;

   if (visit && ctx->preorder != NULL)
      (*ctx->preorder)((tree_t)object, ctx->context);

   const imask_t deep_mask = I_TYPE | I_REF;

   const object_class_t *class = classes[object->tag];

   const imask_t has = class->has_map[object->kind];
   const int nitems = class->object_nitems[object->kind];
   imask_t mask = 1;
   for (int i = 0; i < nitems; mask <<= 1) {
      if (has & mask & ~(ctx->deep ? 0 : deep_mask)) {
         if (ITEM_IDENT & mask)
            ;
         else if (ITEM_OBJECT & mask)
            object_visit(object->items[i].object, ctx);
         else if (ITEM_OBJ_ARRAY & mask) {
            for (unsigned j = 0; j < object->items[i].obj_array.count; j++)
               object_visit(object->items[i].obj_array.items[j], ctx);
         }
         else if (ITEM_INT64 & mask)
            ;
         else if (ITEM_INT32 & mask)
            ;
         else if (ITEM_DOUBLE & mask)
            ;
         else if (ITEM_IDENT_ARRAY & mask)
            ;
         else
            item_without_type(mask);
      }

      if (has & mask)
         i++;
   }

   if (visit) {
      if (ctx->postorder != NULL)
         (*ctx->postorder)((tree_t)object, ctx->context);
      ctx->count++;
   }
}

object_t *object_rewrite(object_t *object, object_rewrite_ctx_t *ctx)
{
   if (object == NULL)
      return NULL;

   if (!object_in_arena_p(ctx->arena, object))
      return object;

   if (unlikely(ctx->cache == NULL)) {
      const size_t n = (ctx->arena->alloc - ctx->arena->base) / OBJECT_ALIGN;
      ctx->cache = xmalloc_array(sizeof(object_t *), n);
   }

   const ptrdiff_t index =
      ((void *)object - ctx->arena->base) >> OBJECT_ALIGN_BITS;

   if (object_marked_p(object, ctx->generation)) {
      if (ctx->cache[index] == (object_t *)-1) {
         // Found a circular reference: eagerly rewrite the object now
         // and break the cycle
         if (object->tag == ctx->tag) {
            if (ctx->pre_fn != NULL)
               (*ctx->pre_fn)(object, ctx->context);
            object_t *new = (object_t *)(*ctx->post_fn)(object, ctx->context);
            object_write_barrier(object, new);
            return (ctx->cache[index] = new);
         }
         else
            return (ctx->cache[index] = object);
      }
      else {
         // Already rewritten this tree so return the cached version
         return ctx->cache[index];
      }
   }

   ctx->cache[index] = (object_t *)-1;  // Rewrite in progress marker

   if (ctx->pre_fn != NULL)
      (*ctx->pre_fn)(object, ctx->context);

   const imask_t skip_mask = I_REF;

   const object_class_t *class = classes[object->tag];

   const imask_t has = class->has_map[object->kind];
   const int nitems = class->object_nitems[object->kind];
   imask_t mask = 1;
   for (int n = 0; n < nitems; mask <<= 1) {
      if (has & mask & ~skip_mask) {
         if (ITEM_IDENT & mask)
            ;
         else if (ITEM_OBJECT & mask) {
            object_t *o = object->items[n].object;
            object->items[n].object = object_rewrite(o, ctx);
         }
         else if (ITEM_OBJ_ARRAY & mask) {
            unsigned wptr = 0;
            for (size_t i = 0; i < object->items[n].obj_array.count; i++) {
               object_t *o = object->items[n].obj_array.items[i];
               if ((o = object_rewrite(o, ctx)))
                  object->items[n].obj_array.items[wptr++] = o;
            }
            ATRIM(object->items[n].obj_array, wptr);
         }
         else if (ITEM_INT64 & mask)
            ;
         else if (ITEM_INT32 & mask)
            ;
         else if (ITEM_DOUBLE & mask)
            ;
         else if (ITEM_IDENT_ARRAY & mask)
            ;
         else
            item_without_type(mask);
      }

      if (has & mask)
         n++;
   }

   if (ctx->cache[index] != (object_t *)-1) {
      // The cache was already updated due to a circular reference
   }
   else if (object->tag == ctx->tag) {
      object_t *new = (object_t *)(*ctx->post_fn)(object, ctx->context);
      object_write_barrier(object, new);
      ctx->cache[index] = new;
   }
   else
      ctx->cache[index] = object;

   return ctx->cache[index];
}

static void object_write_ref(object_t *object, fbuf_t *f)
{
   if (object == NULL)
      write_u16(0, f);
   else {
      object_arena_t *arena = __object_arena(object);
      assert(arena->key != 0);

      write_u16(arena->key, f);
      write_u32((void *)object - arena->base, f);
   }
}

void object_write(object_t *root, fbuf_t *f)
{
   object_arena_t *arena = __object_arena(root);

   write_u32(format_digest, f);
   write_u8(standard(), f);
   write_u32(arena->limit - arena->base, f);

   ident_wr_ctx_t ident_ctx = ident_write_begin(f);
   loc_wr_ctx_t *loc_ctx = loc_write_begin(f);

   if (root != arena->base)
      fatal_trace("must write root object first");
   else if (arena->source == OBJ_DISK)
      fatal_trace("writing arena %s originally read from disk",
                  istr(object_arena_name(arena)));
   else if (!arena->frozen)
      fatal_trace("arena %s must be frozen before writing to disk",
                  istr(object_arena_name(arena)));

   write_u16(arena->key, f);
   ident_write(object_arena_name(arena), ident_ctx);

   arena_key_t max_key = arena->key;
   for (unsigned i = 0; i < arena->deps.count; i++)
      max_key = MAX(max_key, arena->deps.items[i]->key);
   write_u16(max_key, f);

   write_u16(arena->deps.count, f);
   for (unsigned i = 0; i < arena->deps.count; i++) {
      write_u16(arena->deps.items[i]->key, f);
      write_u8(arena->deps.items[i]->std, f);
      ident_write(object_arena_name(arena->deps.items[i]), ident_ctx);
   }

   for (void *p = arena->base; p != arena->alloc; ) {
      assert(p < arena->alloc);

      object_t *object = p;
      object_class_t *class = classes[object->tag];

      write_u8(object->tag, f);
      write_u8(object->kind, f);

      if (object->tag == OBJECT_TAG_TREE || object->tag == OBJECT_TAG_E_NODE)
         loc_write(&object->loc, loc_ctx);

      const imask_t has = class->has_map[object->kind];
      const int nitems = class->object_nitems[object->kind];
      imask_t mask = 1;
      for (int n = 0; n < nitems; mask <<= 1) {
         if (has & mask) {
            if (ITEM_IDENT & mask)
               ident_write(object->items[n].ident, ident_ctx);
            else if (ITEM_OBJECT & mask)
               object_write_ref(object->items[n].object, f);
            else if (ITEM_OBJ_ARRAY & mask) {
               const unsigned count = object->items[n].obj_array.count;
               write_u32(count, f);
               for (unsigned i = 0; i < count; i++)
                  object_write_ref(object->items[n].obj_array.items[i], f);
            }
            else if (ITEM_INT64 & mask)
               write_u64(object->items[n].ival, f);
            else if (ITEM_INT32 & mask)
               write_u32(object->items[n].ival, f);
            else if (ITEM_DOUBLE & mask)
               write_double(object->items[n].dval, f);
            else if (ITEM_IDENT_ARRAY & mask) {
               item_t *item = &(object->items[n]);
               write_u32(item->ident_array.count, f);
               for (unsigned i = 0; i < item->ident_array.count; i++)
                  ident_write(item->ident_array.items[i], ident_ctx);
            }
            else
               item_without_type(mask);
            n++;
         }
      }

      p = (char *)p + ALIGN_UP(class->object_size[object->kind], OBJECT_ALIGN);
   }

   write_u8(0xff, f);   // End of objects marker

   ident_write_end(ident_ctx);
   loc_write_end(loc_ctx);
}

static object_t *object_read_ref(fbuf_t *f, const arena_key_t *key_map)
{
   arena_key_t key = read_u16(f);
   if (key == 0)
      return NULL;

   arena_key_t mapped = key_map[key];
   uint32_t offset = read_u32(f);

   if (unlikely(mapped == 0))
      fatal_trace("%s missing dependency with key %d", fbuf_file_name(f), key);

   assert(mapped < all_arenas.count);
   assert(mapped > 0);
   assert((offset & (OBJECT_ALIGN - 1)) == 0);

   object_arena_t *arena = all_arenas.items[mapped];
   assert(!arena->frozen || offset < arena->alloc - arena->base);

   return (object_t *)((char *)arena->base + offset);
}

object_t *object_read(fbuf_t *f, object_load_fn_t loader_fn)
{
   object_one_time_init();

   const uint32_t ver = read_u32(f);
   if (ver != format_digest)
      fatal("%s: serialised format digest is %x expected %x. This design "
            "unit uses a library format from an earlier version of "
            PACKAGE_NAME " and should be reanalysed.",
            fbuf_file_name(f), ver, format_digest);

   const vhdl_standard_t std = read_u8(f);
   if (std > standard())
      fatal("%s: design unit was analysed using standard revision %s which "
            "is more recent that the currently selected standard %s",
            fbuf_file_name(f), standard_text(std), standard_text(standard()));

   const unsigned size = read_u32(f);
   if (size > OBJECT_ARENA_SZ)
      fatal("%s: arena size %u is greater than current maximum %u",
            fbuf_file_name(f), size, OBJECT_ARENA_SZ);
   else if (size & OBJECT_PAGE_MASK)
      fatal("%s: arena size %x bad alignment", fbuf_file_name(f), size);

   ident_rd_ctx_t ident_ctx = ident_read_begin(f);
   loc_rd_ctx_t *loc_ctx = loc_read_begin(f);

   object_arena_t *arena = object_arena_new(size, std);
   arena->source = OBJ_DISK;

   arena_key_t key = read_u16(f);
   ident_t name = ident_read(ident_ctx);
   (void)name;  // Not currently used

   arena_key_t max_key = read_u16(f);

   arena_key_t *key_map LOCAL = xcalloc_array(max_key + 1, sizeof(arena_key_t));
   key_map[key] = arena->key;

   const int ndeps = read_u16(f);
   for (int i = 0; i < ndeps; i++) {
      arena_key_t dkey = read_u16(f);
      vhdl_standard_t dstd = read_u8(f);
      ident_t dep = ident_read(ident_ctx);

      object_arena_t *a = NULL;
      for (unsigned j = 1; a == NULL && j < all_arenas.count; j++) {
         if (dep == object_arena_name(all_arenas.items[j]))
            a = all_arenas.items[j];
      }

      if (a == NULL) {
         object_t *droot = NULL;
         if (loader_fn) droot = (*loader_fn)(dep);

         if (droot == NULL)
            fatal("%s depends on %s which cannot be found",
                  fbuf_file_name(f), istr(dep));

         a = __object_arena(droot);
      }

      if (a->std != dstd)
	 fatal("%s: design unit depends on %s version of %s but conflicting "
	       "%s version has been loaded", fbuf_file_name(f),
	       standard_text(dstd), istr(dep), standard_text(a->std));

      APUSH(arena->deps, a);

      assert(dkey <= max_key);
      key_map[dkey] = a->key;
   }

   for (;;) {
      const uint8_t tag = read_u8(f);
      if (tag == 0xff) break;

      assert(tag < OBJECT_TAG_COUNT);

      const object_class_t *class = classes[tag];

      const uint8_t kind = read_u8(f);

      object_t *object = object_new(arena, class, kind);

      if (tag == OBJECT_TAG_TREE || tag == OBJECT_TAG_E_NODE)
         loc_read(&(object->loc), loc_ctx);

      const imask_t has = class->has_map[object->kind];
      const int nitems = class->object_nitems[object->kind];
      imask_t mask = 1;
      for (int n = 0; n < nitems; mask <<= 1) {
         if (has & mask) {
            if (ITEM_IDENT & mask)
               object->items[n].ident = ident_read(ident_ctx);
            else if (ITEM_OBJECT & mask)
               object->items[n].object = object_read_ref(f, key_map);
            else if (ITEM_OBJ_ARRAY & mask) {
               const unsigned count = read_u32(f);
               ARESIZE(object->items[n].obj_array, count);
               for (unsigned i = 0; i < count; i++) {
                  object_t *o = object_read_ref(f, key_map);
                  object->items[n].obj_array.items[i] = o;
               }
            }
            else if (ITEM_INT64 & mask)
               object->items[n].ival = read_u64(f);
            else if (ITEM_INT32 & mask)
               object->items[n].ival = read_u32(f);
            else if (ITEM_DOUBLE & mask)
               object->items[n].dval = read_double(f);
            else if (ITEM_IDENT_ARRAY & mask) {
               const unsigned count = read_u32(f);
               ARESIZE(object->items[n].ident_array, count);;
               for (unsigned i = 0; i < count; i++) {
                  ident_t id = ident_read(ident_ctx);
                  object->items[n].ident_array.items[i] = id;
               }
            }
            else
               item_without_type(mask);
            n++;
         }
      }
   }

   object_arena_freeze(arena);
   ident_read_end(ident_ctx);
   loc_read_end(loc_ctx);

   return (object_t *)arena->base;
}

unsigned object_next_generation(void)
{
   return next_generation++;
}

static bool object_copy_mark(object_t *object, object_copy_ctx_t *ctx)
{
   if (object == NULL)
      return false;

   if (ctx->copy_map == NULL)
      ctx->copy_map = hash_new(1024, true);

   if (object_marked_p(object, ctx->generation))
      return hash_get(ctx->copy_map, object) != NULL;

   const object_class_t *class = classes[object->tag];

   bool marked = false;
   if (object->tag == ctx->tag)
      marked = (*ctx->should_copy)(object, ctx->context);

   object_t *copy = NULL;
   if (marked) {
      copy = object_new(ctx->arena, class, object->kind);
      hash_put(ctx->copy_map, object, copy);
   }

   const imask_t has = class->has_map[object->kind];
   const int nitems = class->object_nitems[object->kind];
   imask_t mask = 1;
   for (int n = 0; n < nitems; mask <<= 1) {
      if (has & mask) {
         if (ITEM_IDENT & mask)
            ;
         else if (ITEM_OBJECT & mask) {
            object_t *o = object->items[n].object;
            marked |= object_copy_mark(o, ctx);
         }
         else if (ITEM_DOUBLE & mask)
            ;
         else if (ITEM_OBJ_ARRAY & mask) {
            for (unsigned i = 0; i < object->items[n].obj_array.count; i++) {
               object_t *o = object->items[n].obj_array.items[i];
               marked |= object_copy_mark(o, ctx);
            }
         }
         else if (ITEM_INT64 & mask)
            ;
         else if (ITEM_INT32 & mask)
            ;
         else
            item_without_type(mask);
         n++;
      }
   }

   if (marked && copy == NULL) {
      copy = object_new(ctx->arena, class, object->kind);
      hash_put(ctx->copy_map, object, copy);
   }

   return marked;
}

static object_t *object_copy_map(object_t *object, object_copy_ctx_t *ctx)
{
   if (object == NULL)
      return NULL;

   object_t *map = hash_get(ctx->copy_map, object);
   return map ?: object;
}

object_t *object_copy(object_t *root, object_copy_ctx_t *ctx)
{
   (void)object_copy_mark(root, ctx);

   object_t *result = hash_get(ctx->copy_map, root);
   if (result == NULL) {
      hash_free(ctx->copy_map);
      return root;
   }

   unsigned ncopied = 0;
   const void *key;
   void *value;
   for (hash_iter_t it = HASH_BEGIN;
        hash_iter(ctx->copy_map, &it, &key, &value); ) {
      const object_t *object = key;
      object_t *copy = value;
      ncopied++;

      copy->loc = object->loc;

      const object_class_t *class = classes[object->tag];

      const imask_t has = class->has_map[object->kind];
      const int nitems = class->object_nitems[object->kind];
      imask_t mask = 1;
      for (int n = 0; n < nitems; mask <<= 1) {
         if (has & mask) {
            if (ITEM_IDENT & mask)
               copy->items[n].ident = object->items[n].ident;
            else if (ITEM_OBJECT & mask) {
               object_t *o = object_copy_map(object->items[n].object, ctx);
               copy->items[n].object = o;
               object_write_barrier(copy, o);
            }
            else if (ITEM_DOUBLE & mask)
               copy->items[n].dval = object->items[n].dval;
            else if (ITEM_OBJ_ARRAY & mask) {
               const item_t *from = &(object->items[n]);
               item_t *to = &(copy->items[n]);
               ARESIZE(to->obj_array, from->obj_array.count);
               for (size_t i = 0; i < from->obj_array.count; i++) {
                  object_t *o = object_copy_map(from->obj_array.items[i], ctx);
                  to->obj_array.items[i] = o;
                  object_write_barrier(copy, o);
               }
            }
            else if ((ITEM_INT64 & mask) || (ITEM_INT32 & mask))
               copy->items[n].ival = object->items[n].ival;
            else
               item_without_type(mask);
            n++;
         }
      }
   }

   if (ctx->callback != NULL) {
      for (hash_iter_t it = HASH_BEGIN;
           hash_iter(ctx->copy_map, &it, &key, &value); ) {
         object_t *copy = value;
         (*ctx->callback)(copy, ctx->context);
      }
   }

   if (getenv("NVC_GC_VERBOSE") != NULL)
      notef("copied %d objects into arena %s", ncopied,
            istr(object_arena_name(ctx->arena)));

   hash_free(ctx->copy_map);
   return result;
}

object_arena_t *object_arena_new(size_t size, unsigned std)
{
   if (all_arenas.count == 0)
      APUSH(all_arenas, NULL);   // Dummy null arena

   object_arena_t *arena = xcalloc(sizeof(object_arena_t));
   arena->base   = nvc_memalign(OBJECT_PAGE_SZ, size);
   arena->alloc  = arena->base;
   arena->limit  = (char *)arena->base + size;
   arena->key    = all_arenas.count;
   arena->source = OBJ_FRESH;
   arena->std    = std;

   APUSH(all_arenas, arena);

   if (all_arenas.count == UINT16_MAX - 1)
      fatal_trace("too many object arenas");

   return arena;
}

void object_arena_freeze(object_arena_t *arena)
{
   if (arena->frozen)
      fatal_trace("arena %s already frozen", istr(object_arena_name(arena)));

   if (arena->source == OBJ_FRESH)
      object_arena_gc(arena);

   if (getenv("NVC_GC_VERBOSE") != NULL)
      notef("arena %s frozen (%d bytes)", istr(object_arena_name(arena)),
            (int)(arena->alloc - arena->base));

   void *next_page = ALIGN_UP(arena->alloc, OBJECT_PAGE_SZ);
   nvc_memprotect(arena->base, next_page - arena->base, MEM_RO);

   if (next_page < arena->limit) {
#if OBJECT_UNMAP_UNUSED
      nvc_munmap(next_page, arena->limit - next_page);
      arena->limit = next_page;
#else
      // This can be useful for debugging use-after-free
      nvc_memprotect(next_page, arena->limit - next_page, MEM_NONE);
#endif
   }

   arena->frozen = true;
}

void check_frozen_object_fault(void *addr)
{
   for (unsigned i = 1; i < all_arenas.count; i++) {
      object_arena_t *arena = AGET(all_arenas, i);
      if (!arena->frozen)
         continue;
      else if (addr < arena->base)
         continue;
      else if (addr >= arena->limit)
         continue;

      fatal_trace("Write to object in frozen arena %s [address=%p]",
                  istr(object_arena_name(arena)), addr);
   }
}

void object_add_global_root(object_t **object)
{
   APUSH(global_roots, object);
}

void object_arena_walk_deps(object_arena_t *arena, object_arena_deps_fn_t fn,
                            void *context)
{
   for (unsigned i = 0; i < arena->deps.count; i++)
      (*fn)(object_arena_name(arena->deps.items[i]), context);
}

#if __SANITIZE_ADDRESS__
static void object_arena_purge(object_arena_t *arena)
{
   nvc_memprotect(arena->base, arena->limit - arena->base, MEM_RW);

   for (void *p = arena->base; p != arena->alloc; ) {
      object_t *object = p;
      object_class_t *class = classes[object->tag];
      gc_free_external(object);
      p = (char *)p + ALIGN_UP(class->object_size[object->kind], OBJECT_ALIGN);
   }

   nvc_munmap(arena->base, arena->limit - arena->base);
}

static void object_purge_at_exit(void)
{
   // Address sanitizer cannot track addresses from user-mmaped regions
   // so manually free up all malloc-ed memory accessible from objects

   for (unsigned i = 1; i < all_arenas.count; i++)
      object_arena_purge(AGET(all_arenas, i));
}
#endif  // __SANITIZE_ADDRESS__
