//
//  Copyright (C) 2014-2023  Nick Gasson
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
#include "diag.h"
#include "hash.h"
#include "ident.h"
#include "lib.h"
#include "object.h"
#include "option.h"

#include <string.h>
#include <stdlib.h>
#include <inttypes.h>
#include <signal.h>

typedef uint64_t mark_mask_t;

typedef A(object_arena_t *) arena_array_t;
typedef A(object_t **) object_ptr_array_t;

typedef enum { OBJ_DISK, OBJ_FRESH } obj_src_t;

typedef struct _object_arena {
   void           *base;
   void           *alloc;
   void           *limit;
   bool            frozen;
   bool            has_locus;
   uint32_t       *forward;
   mark_mask_t    *mark_bits;
   size_t          mark_sz;
   generation_t    generation;
   arena_key_t     key;
   arena_array_t   deps;
   object_t       *root;
   obj_src_t       source;
   vhdl_standard_t std;
   uint32_t        checksum;
} object_arena_t;

#ifndef __SANITIZE_ADDRESS__
#define OBJECT_UNMAP_UNUSED 1
#endif

#define ITEM_IDENT       (I_IDENT | I_IDENT2)
#define ITEM_OBJECT      (I_VALUE | I_SEVERITY | I_MESSAGE | I_TARGET   \
                          | I_DELAY | I_REJECT | I_REF | I_FILE_MODE    \
                          | I_NAME | I_SPEC | I_RESOLUTION              \
                          | I_LEFT | I_RIGHT | I_TYPE | I_BASE | I_ELEM \
                          | I_ACCESS | I_RESULT | I_FILE | I_PRIMARY    \
                          | I_GUARD | I_FOREIGN | I_CLOCK | I_REPEAT)
#define ITEM_OBJ_ARRAY   (I_DECLS | I_STMTS | I_PORTS | I_GENERICS      \
                          | I_WAVES | I_CONDS | I_TRIGGERS | I_CONSTR   \
                          | I_PARAMS | I_GENMAPS | I_ASSOCS | I_CONTEXT \
                          | I_LITERALS | I_FIELDS | I_UNITS | I_CHARS   \
                          | I_DIMS | I_RANGES | I_INDEXCON | I_PARTS \
                          | I_PRAGMAS)
#define ITEM_INT64       (I_POS | I_IVAL)
#define ITEM_INT32       (I_SUBKIND | I_CLASS | I_FLAGS)
#define ITEM_DOUBLE      (I_DVAL)

static const char *item_text_map[] = {
   "I_IDENT",    "I_VALUE",     "I_SEVERITY", "I_MESSAGE",    "I_TARGET",
   "I_LITERAL",  "I_IDENT2",    "I_DECLS",    "I_STMTS",      "I_PORTS",
   "I_GENERICS", "I_PARAMS",    "I_GENMAPS",  "I_WAVES",      "I_CONDS",
   "I_TYPE",     "I_SUBKIND",   "I_DELAY",    "I_REJECT",     "I_POS",
   "I_REF",      "I_FILE_MODE", "I_ASSOCS",   "I_CONTEXT",    "I_TRIGGERS",
   "I_PARTS"  ,  "I_CLASS",     "I_RANGES",   "I_NAME",       "I_PRAGMAS",
   "I_DVAL",     "I_SPEC",      "I_FOREIGN",  "I_INDEXCON",   "I_BASE",
   "I_ELEM",     "I_FILE",      "I_ACCESS",   "I_RESOLUTION", "I_RESULT",
   "I_UNITS",    "I_LITERALS",  "I_DIMS",     "I_FIELDS",     "I_CLOCK",
   "I_GUARD",    "????",        "I_CHARS",    "I_CONSTR",     "I_FLAGS",
   "????",       "I_LEFT",      "I_RIGHT",    "????",         "????",
   "????",       "????",        "????",       "????",         "I_PRIMARY",
};

static object_class_t *classes[4];
static uint32_t        format_digest;
static generation_t    next_generation = 1;
static arena_array_t   all_arenas;
static object_arena_t *global_arena = NULL;

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
      object_t *root = arena_root(arena);

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
   uint64_t mask = UINT64_C(1) << (bit & 63);

   const bool marked = !!(arena->mark_bits[word] & mask);
   arena->mark_bits[word] |= mask;

   return marked;
}

void arena_set_checksum(object_arena_t *arena, uint32_t checksum)
{
   arena->checksum = checksum;
}

object_t *arena_root(object_arena_t *arena)
{
   return arena->root ?: (object_t *)arena->base;
}

bool arena_frozen(object_arena_t *arena)
{
   return arena->frozen;
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

void object_lookup_failed(object_class_t *class, object_t *object, imask_t mask)
{
   unsigned int item;
   for (item = 0; (mask & (UINT64_C(1) << item)) == 0; item++)
      ;

   assert(item < ARRAY_LEN(item_text_map));

   diag_t *d = diag_new(DIAG_FATAL, &(object->loc));
   diag_printf(d, "%s kind %s does not have item %s", class->name,
               class->kind_text_map[object->kind], item_text_map[item]);
   diag_set_consumer(NULL);
   diag_suppress(d, false);
   diag_emit(d);
   show_stacktrace();
   fatal_exit(EXIT_FAILURE);
}

void item_without_type(imask_t mask)
{
   int item;
   for (item = 0; (mask & (UINT64_C(1) << item)) == 0; item++)
      ;

   assert(item < ARRAY_LEN(item_text_map));
   fatal_trace("item %s does not have a type", item_text_map[item]);
}

void obj_array_add(obj_array_t **a, object_t *o)
{
   if (*a == NULL) {
      const int defsz = 8;
      *a = xmalloc_flex(sizeof(obj_array_t), defsz, sizeof(object_t *));
      (*a)->count = 0;
      (*a)->limit = defsz;
   }
   else if ((*a)->count == (*a)->limit) {
      (*a)->limit *= 2;
      *a = xrealloc_flex(*a, sizeof(obj_array_t),
                         (*a)->limit, sizeof(object_t *));
   }

   (*a)->items[(*a)->count++] = o;
}

void obj_array_free(obj_array_t **a)
{
   free(*a);
   *a = NULL;
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

static void check_frozen_object_fault(int sig, void *addr,
                                      struct cpu_state *cpu, void *context)
{
   if (sig != SIGSEGV)
      return;

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

void object_one_time_init(void)
{
   extern object_class_t tree_object;
   extern object_class_t type_object;
   extern object_class_t vlog_object;
   extern object_class_t psl_object;

   static bool done = false;

   if (unlikely(!done)) {
      object_init(&tree_object);
      object_init(&type_object);
      object_init(&vlog_object);
      object_init(&psl_object);

      // Increment this each time a incompatible change is made to the
      // on-disk format not expressed in the object items table
      const uint32_t format_fudge = 29;

      format_digest += format_fudge * UINT32_C(2654435761);

      add_fault_handler(check_frozen_object_fault, NULL);

      done = true;
   }
}

object_t *object_new(object_arena_t *arena,
                     const object_class_t *class, int kind)
{
   if (unlikely(kind >= class->last_kind))
      fatal_trace("invalid kind %d for %s object", kind, class->name);

   object_one_time_init();

   if (arena == NULL)
      arena = global_arena;

   if (unlikely(arena == NULL))
      fatal_trace("allocating object without active arena");

   const size_t size = ALIGN_UP(class->object_size[kind], OBJECT_ALIGN);

   assert(((uintptr_t)arena->alloc & (OBJECT_ALIGN - 1)) == 0);

   if (unlikely(arena->limit - arena->alloc < size)) {
      diag_t *d = diag_new(DIAG_FATAL, NULL);
      diag_suppress(d, false);
      diag_printf(d, "memory exhausted while creating unit %s",
                  istr(object_arena_name(arena)));
      diag_hint(d, NULL, "The current limit is %zu bytes which you can "
                "increase with the $bold$-M$$ option, for example "
                "$bold$-M 32m$$", object_arena_default_size());
      diag_emit(d);
      fatal_exit(EXIT_FAILURE);
   }

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
         item_t *item = &(object->items[i]);
         if (ITEM_OBJECT & mask)
            gc_forward_one_pointer(&(item->object), arena, forward);
         else if ((ITEM_OBJ_ARRAY & mask) && item->obj_array != NULL) {
            for (unsigned j = 0; j < item->obj_array->count; j++)
               gc_forward_one_pointer(&(item->obj_array->items[j]),
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
            obj_array_free(&(object->items[i].obj_array));
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

   // Calculate forwarding addresses
   const size_t fwdsz = (arena->alloc - arena->base) / OBJECT_ALIGN;
   uint32_t *forward = xmalloc_array(fwdsz, sizeof(uint32_t));
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

   arena->alloc = (char *)arena->base + woffset;

 skip_gc:
   if (arena->has_locus) {
      // Need to keep a copy of the forwarding table for loci created
      // before freezing
      arena->forward = forward;
   }
   else
      free(forward);

   if (opt_get_verbose(OPT_OBJECT_VERBOSE, NULL)) {
      const int ticks = get_timestamp_us() - start_ticks;
      debugf("GC: %s: freed %d objects; %d allocated [%d us]",
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

   const object_class_t *class = classes[object->tag];

   const bool visit =
      (object->tag == ctx->tag && object->kind == ctx->kind)
      || ctx->kind == class->last_kind;

   if (visit && ctx->preorder != NULL)
      (*ctx->preorder)(object, ctx->context);

   const imask_t deep_mask = I_TYPE | I_REF;

   const imask_t has = class->has_map[object->kind];
   const int nitems = class->object_nitems[object->kind];
   imask_t mask = 1;
   for (int i = 0; i < nitems; mask <<= 1) {
      if (has & mask & ~(ctx->deep ? 0 : deep_mask)) {
         item_t *item = &(object->items[i]);
         if (ITEM_IDENT & mask)
            ;
         else if (ITEM_OBJECT & mask)
            object_visit(item->object, ctx);
         else if (ITEM_OBJ_ARRAY & mask) {
            if (item->obj_array != NULL) {
               for (unsigned j = 0; j < item->obj_array->count; j++)
                  object_visit(item->obj_array->items[j], ctx);
            }
         }
         else if (ITEM_INT64 & mask)
            ;
         else if (ITEM_INT32 & mask)
            ;
         else if (ITEM_DOUBLE & mask)
            ;
         else
            item_without_type(mask);
      }

      if (has & mask)
         i++;
   }

   if (visit) {
      if (ctx->postorder != NULL)
         (*ctx->postorder)(object, ctx->context);
      ctx->count++;
   }
}

static object_t *object_rewrite_iter(object_t *object,
                                     object_rewrite_ctx_t *ctx)
{
   // The callback may return a new object or a pointer to an existing
   // object in the same arena that that needs to be rewritten so
   // iterate rewriting until we reach a fixed point
   for (;;) {
      object_t *new = (*ctx->post_fn[object->tag])(object, ctx->context);
      if (new == object || new == NULL)
         return new;
      object = object_rewrite(new, ctx);
   }
}

object_t *object_rewrite(object_t *object, object_rewrite_ctx_t *ctx)
{
   if (object == NULL)
      return NULL;

   if (!object_in_arena_p(ctx->arena, object))
      return object;

   const ptrdiff_t index =
      ((void *)object - ctx->arena->base) >> OBJECT_ALIGN_BITS;

   // New objects can be allocated while rewrite is in progress so we
   // need to check if the inex is greater than the current cache size
   if (unlikely(ctx->cache == NULL || index >= ctx->cache_sz)) {
      ctx->cache_sz = (ctx->arena->alloc - ctx->arena->base) / OBJECT_ALIGN;
      ctx->cache = xrealloc_array(ctx->cache, sizeof(object_t *),
                                  ctx->cache_sz);
   }

   if (object_marked_p(object, ctx->generation)) {
      if (ctx->cache[index] == (object_t *)-1) {
         // Found a circular reference: eagerly rewrite the object now
         // and break the cycle
         if (ctx->post_fn[object->tag] != NULL) {
            if (ctx->pre_fn[object->tag] != NULL)
               (*ctx->pre_fn[object->tag])(object, ctx->context);
            object_t *new = object_rewrite_iter(object, ctx);
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

   if (ctx->pre_fn[object->tag] != NULL)
      (*ctx->pre_fn[object->tag])(object, ctx->context);

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
            object_write_barrier(object, o);
         }
         else if (ITEM_OBJ_ARRAY & mask) {
            obj_array_t **a = &(object->items[n].obj_array);
            if (object->items[n].obj_array != NULL) {
               // The callback may add new items to the array so the
               // array pointer cannot be cached between iterations
               unsigned wptr = 0;
               for (size_t i = 0; i < object->items[n].obj_array->count; i++) {
                  object_t *o = object->items[n].obj_array->items[i];
                  if ((o = object_rewrite(o, ctx))) {
                     object_write_barrier(object, o);
                     object->items[n].obj_array->items[wptr++] = o;
                  }
               }

               if (wptr == 0)
                  obj_array_free(a);
               else
                  (*a)->count = wptr;
            }
         }
         else if (ITEM_INT64 & mask)
            ;
         else if (ITEM_INT32 & mask)
            ;
         else if (ITEM_DOUBLE & mask)
            ;
         else
            item_without_type(mask);
      }

      if (has & mask)
         n++;
   }

   if (ctx->cache[index] != (object_t *)-1) {
      // The cache was already updated due to a circular reference
      return ctx->cache[index];
   }
   else if (ctx->post_fn[object->tag] != NULL) {
      object_t *new = object_rewrite_iter(object, ctx);
      object_write_barrier(object, new);
      return (ctx->cache[index] = new);
   }
   else
      return (ctx->cache[index] = object);
}

static void object_write_ref(object_t *object, fbuf_t *f)
{
   if (object == NULL)
      fbuf_put_uint(f, 0);
   else {
      object_arena_t *arena = __object_arena(object);
      assert(arena->key != 0);
      fbuf_put_uint(f, arena->key);

      ptrdiff_t offset = ((void *)object - arena->base) >> OBJECT_ALIGN_BITS;
      fbuf_put_uint(f, offset);
   }
}

void object_write(object_t *root, fbuf_t *f, ident_wr_ctx_t ident_ctx,
                  loc_wr_ctx_t *loc_ctx)
{
   object_arena_t *arena = __object_arena(root);

   write_u32(format_digest, f);
   fbuf_put_uint(f, standard());
   fbuf_put_uint(f, arena->limit - arena->base);

   if (root != arena_root(arena))
      fatal_trace("must write root object first");
   else if (arena->source == OBJ_DISK)
      fatal_trace("writing arena %s originally read from disk",
                  istr(object_arena_name(arena)));
   else if (!arena->frozen)
      fatal_trace("arena %s must be frozen before writing to disk",
                  istr(object_arena_name(arena)));

   fbuf_put_uint(f, arena->key);
   ident_write(object_arena_name(arena), ident_ctx);

   arena_key_t max_key = arena->key;
   for (unsigned i = 0; i < arena->deps.count; i++)
      max_key = MAX(max_key, arena->deps.items[i]->key);
   fbuf_put_uint(f, max_key);

   fbuf_put_uint(f, arena->deps.count);
   for (unsigned i = 0; i < arena->deps.count; i++) {
      fbuf_put_uint(f, arena->deps.items[i]->key);
      fbuf_put_uint(f, arena->deps.items[i]->std);
      fbuf_put_uint(f, arena->deps.items[i]->checksum);
      ident_write(object_arena_name(arena->deps.items[i]), ident_ctx);
   }

   for (void *p = arena->base; p != arena->alloc; ) {
      assert(p < arena->alloc);

      object_t *object = p;
      object_class_t *class = classes[object->tag];

      STATIC_ASSERT(OBJECT_TAG_COUNT <= 4);
      fbuf_put_uint(f, object->tag | (object->kind << 2));

      if (object->tag == OBJECT_TAG_TREE)
         loc_write(&object->loc, loc_ctx);

      const imask_t has = class->has_map[object->kind];
      const int nitems = class->object_nitems[object->kind];
      imask_t mask = 1;
      for (int n = 0; n < nitems; mask <<= 1) {
         if (has & mask) {
            item_t *item = &(object->items[n]);
            if (ITEM_IDENT & mask)
               ident_write(item->ident, ident_ctx);
            else if (ITEM_OBJECT & mask)
               object_write_ref(item->object, f);
            else if (ITEM_OBJ_ARRAY & mask) {
               if (item->obj_array != NULL) {
                  const unsigned count = item->obj_array->count;
                  fbuf_put_uint(f, count);
                  for (unsigned i = 0; i < count; i++)
                     object_write_ref(item->obj_array->items[i], f);
               }
               else
                  fbuf_put_uint(f, 0);
            }
            else if (ITEM_INT64 & mask)
               fbuf_put_int(f, item->ival);
            else if (ITEM_INT32 & mask)
               fbuf_put_int(f, item->ival);
            else if (ITEM_DOUBLE & mask)
               write_double(item->dval, f);
            else
               item_without_type(mask);
            n++;
         }
      }

      p = (char *)p + ALIGN_UP(class->object_size[object->kind], OBJECT_ALIGN);
   }

   fbuf_put_uint(f, UINT16_MAX);   // End of objects marker
}

static object_t *object_read_ref(fbuf_t *f, const arena_key_t *key_map)
{
   arena_key_t key = fbuf_get_uint(f);
   if (key == 0)
      return NULL;

   arena_key_t mapped = key_map[key];
   ptrdiff_t offset = fbuf_get_uint(f) << OBJECT_ALIGN_BITS;

   if (unlikely(mapped == 0))
      fatal_trace("%s missing dependency with key %d", fbuf_file_name(f), key);

   assert(mapped < all_arenas.count);
   assert(mapped > 0);

   object_arena_t *arena = all_arenas.items[mapped];
   assert(!arena->frozen || offset < arena->alloc - arena->base);

   return (object_t *)((char *)arena->base + offset);
}

object_t *object_read(fbuf_t *f, object_load_fn_t loader_fn,
                      ident_rd_ctx_t ident_ctx, loc_rd_ctx_t *loc_ctx)
{
   object_one_time_init();

   const uint32_t ver = read_u32(f);
   if (ver != format_digest)
      fatal("%s: serialised format digest is %x expected %x. This design "
            "unit uses a library format from an earlier version of "
            PACKAGE_NAME " and should be reanalysed.",
            fbuf_file_name(f), ver, format_digest);

   const vhdl_standard_t std = fbuf_get_uint(f);

   // If this is the first design unit we've loaded then allow it to set
   // the default standard
   if (all_arenas.count == 0)
      set_default_standard(std);

   if (std > standard())
      fatal("%s: design unit was analysed using standard revision %s which "
            "is more recent that the currently selected standard %s",
            fbuf_file_name(f), standard_text(std), standard_text(standard()));

   const unsigned size = fbuf_get_uint(f);
   if (size & OBJECT_PAGE_MASK)
      fatal("%s: arena size %x bad alignment", fbuf_file_name(f), size);

   object_arena_t *arena = object_arena_new(size, std);
   arena->source = OBJ_DISK;

   arena_key_t key = fbuf_get_uint(f);
   ident_t name = ident_read(ident_ctx);

   arena_key_t max_key = fbuf_get_uint(f);

   arena_key_t *key_map LOCAL = xcalloc_array(max_key + 1, sizeof(arena_key_t));
   key_map[key] = arena->key;

   const int ndeps = fbuf_get_uint(f);
   for (int i = 0; i < ndeps; i++) {
      arena_key_t dkey = fbuf_get_uint(f);
      vhdl_standard_t dstd = fbuf_get_uint(f);
      uint32_t checksum = fbuf_get_uint(f);
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
      else if (a->checksum != checksum) {
         diag_t *d = diag_new(DIAG_FATAL, NULL);
         diag_suppress(d, false);
         diag_printf(d, "%s: design unit depends on %s with checksum %08x "
                     "but the current version in the library has checksum %08x",
                     fbuf_file_name(f), istr(dep), checksum, a->checksum);
         diag_hint(d, NULL, "this usually means %s is outdated and needs to "
                   "be reanalysed", istr(name));
         diag_emit(d);
         fatal_exit(EXIT_FAILURE);
      }

      APUSH(arena->deps, a);

      assert(dkey <= max_key);
      key_map[dkey] = a->key;
   }

   for (;;) {
      const uint64_t hdr = fbuf_get_uint(f);
      if (hdr == UINT16_MAX) break;

      const unsigned tag = hdr & 3;
      const unsigned kind = hdr >> 2;

      assert(tag < OBJECT_TAG_COUNT);

      const object_class_t *class = classes[tag];

      object_t *object = object_new(arena, class, kind);

      if (tag == OBJECT_TAG_TREE)
         loc_read(&(object->loc), loc_ctx);

      const imask_t has = class->has_map[object->kind];
      const int nitems = class->object_nitems[object->kind];
      imask_t mask = 1;
      for (int n = 0; n < nitems; mask <<= 1) {
         if (has & mask) {
            item_t *item = &(object->items[n]);
            if (ITEM_IDENT & mask)
               item->ident = ident_read(ident_ctx);
            else if (ITEM_OBJECT & mask)
               item->object = object_read_ref(f, key_map);
            else if (ITEM_OBJ_ARRAY & mask) {
               const unsigned count = fbuf_get_uint(f);
               if (count > 0) {
                  item->obj_array = xmalloc_flex(sizeof(obj_array_t),
                                                 count, sizeof(object_t *));
                  item->obj_array->count =
                     item->obj_array->limit = count;
                  for (unsigned i = 0; i < count; i++) {
                     object_t *o = object_read_ref(f, key_map);
                     item->obj_array->items[i] = o;
                  }
               }
            }
            else if (ITEM_INT64 & mask)
               item->ival = fbuf_get_int(f);
            else if (ITEM_INT32 & mask)
               item->ival = fbuf_get_int(f);
            else if (ITEM_DOUBLE & mask)
               item->dval = read_double(f);
            else
               item_without_type(mask);
            n++;
         }
      }
   }

   object_arena_freeze(arena);
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

   unsigned pos = 0;
   for (; pos < ctx->nroots; pos++) {
      if (object->arena == ctx->roots[pos]->arena)
         break;
   }

   if (pos == ctx->nroots)
      return false;

   if (ctx->copy_map == NULL)
      ctx->copy_map = hash_new(1024);

   if (object_marked_p(object, ctx->generation))
      return hash_get(ctx->copy_map, object) != NULL;

   const object_class_t *class = classes[object->tag];

   bool marked = false;
   if (ctx->should_copy[object->tag] != NULL)
      marked = (*ctx->should_copy[object->tag])(object, ctx->pred_context);

   object_t *copy = NULL;
   if (marked) {
      copy = object_new(global_arena, class, object->kind);
      hash_put(ctx->copy_map, object, copy);
   }

   const imask_t has = class->has_map[object->kind];
   const int nitems = class->object_nitems[object->kind];
   imask_t mask = 1;
   for (int n = 0; n < nitems; mask <<= 1) {
      if (has & mask) {
         item_t *item = &(object->items[n]);
         if (ITEM_IDENT & mask)
            ;
         else if (ITEM_OBJECT & mask)
            marked |= object_copy_mark(item->object, ctx);
         else if (ITEM_DOUBLE & mask)
            ;
         else if (ITEM_OBJ_ARRAY & mask) {
            if (item->obj_array != NULL) {
               for (unsigned i = 0; i < item->obj_array->count; i++) {
                  object_t *o = item->obj_array->items[i];
                  marked |= object_copy_mark(o, ctx);
               }
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
      copy = object_new(global_arena, class, object->kind);
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

void object_copy(object_copy_ctx_t *ctx)
{
   for (unsigned i = 0; i < ctx->nroots; i++)
      (void)object_copy_mark(ctx->roots[i], ctx);

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
            const item_t *from = &(object->items[n]);
            item_t *to = &(copy->items[n]);

            if (ITEM_IDENT & mask)
               to->ident = from->ident;
            else if (ITEM_OBJECT & mask) {
               to->object = object_copy_map(from->object, ctx);
               object_write_barrier(copy, to->object);
            }
            else if (ITEM_DOUBLE & mask)
               to->dval = from->dval;
            else if (ITEM_OBJ_ARRAY & mask) {
               if (from->obj_array != NULL) {
                  // TODO: make a resize macro
                  to->obj_array = xmalloc_flex(sizeof(obj_array_t),
                                               from->obj_array->count,
                                               sizeof(object_t *));
                  to->obj_array->count =
                     to->obj_array->limit = from->obj_array->count;
                  for (size_t i = 0; i < from->obj_array->count; i++) {
                     object_t *o =
                        object_copy_map(from->obj_array->items[i], ctx);
                     to->obj_array->items[i] = o;
                     object_write_barrier(copy, o);
                  }
               }
            }
            else if ((ITEM_INT64 & mask) || (ITEM_INT32 & mask))
               to->ival = from->ival;
            else
               item_without_type(mask);
            n++;
         }
      }
   }

   for (hash_iter_t it = HASH_BEGIN;
        hash_iter(ctx->copy_map, &it, &key, &value); ) {
      object_t *copy = value;
      if (ctx->callback[copy->tag] != NULL)
         (*ctx->callback[copy->tag])(copy, ctx->callback_context);
   }

   if (opt_get_verbose(OPT_OBJECT_VERBOSE, NULL))
      debugf("copied %d objects into arena %s", ncopied,
             istr(object_arena_name(global_arena)));

   for (unsigned i = 0; i < ctx->nroots; i++) {
      object_t *copy = hash_get(ctx->copy_map, ctx->roots[i]);
      if (copy != NULL)
         ctx->roots[i] = copy;
   }

   hash_free(ctx->copy_map);
}

size_t object_arena_default_size(void)
{
   return ALIGN_UP(opt_get_size(OPT_ARENA_SIZE), OBJECT_PAGE_SZ);
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

   if (opt_get_verbose(OPT_OBJECT_VERBOSE, NULL))
      debugf("arena %s frozen (%d bytes)", istr(object_arena_name(arena)),
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

void object_arena_walk_deps(object_arena_t *arena, object_arena_deps_fn_t fn,
                            void *context)
{
   for (unsigned i = 0; i < arena->deps.count; i++)
      (*fn)(object_arena_name(arena->deps.items[i]), context);
}

void object_locus(object_t *object, ident_t *module, ptrdiff_t *offset)
{
   object_arena_t *arena = __object_arena(object);
   *module = object_arena_name(arena);

   *offset = ((void *)object - arena->base) >> OBJECT_ALIGN_BITS;

   if (!arena->frozen) {
      *offset = -*offset;
      arena->has_locus = true;
   }
}

static object_arena_t *arena_by_name(ident_t module)
{
   // Search backwards to ensure we find the most recent arena with the
   // given name
   for (int j = all_arenas.count - 1; j > 0; j--) {
      if (module == object_arena_name(all_arenas.items[j]))
         return all_arenas.items[j];
   }

   return NULL;
}

object_t *object_from_locus(ident_t module, ptrdiff_t offset,
                            object_load_fn_t loader)
{
   object_arena_t *arena = arena_by_name(module);

   if (arena == NULL) {
      object_t *droot = NULL;
      if (loader) droot = (*loader)(module);

      if (droot == NULL)
         fatal("cannot find object locus %s%+"PRIiPTR, istr(module), offset);

      arena = __object_arena(droot);
   }

   if (offset < 0 && arena->frozen && arena->forward == NULL)
      fatal_trace("locus %s%+"PRIiPTR" was created before arena was frozen",
                  istr(module), offset);
   else if (offset < 0 && arena->frozen)
      offset = arena->forward[-offset] >> OBJECT_ALIGN_BITS;
   else if (offset < 0)
      offset = -offset;

   void *ptr = (char *)arena->base + (offset << OBJECT_ALIGN_BITS);

   if (ptr > arena->limit)
      fatal_trace("invalid object locus %s%+"PRIiPTR, istr(module), offset);

   object_t *obj = ptr;
   if (obj->tag >= OBJECT_TAG_COUNT)
      fatal_trace("invalid tag %d for object locus %s%+"PRIiPTR, obj->tag,
                  istr(module), offset);
   else if (obj->arena != arena->key)
      fatal_trace("invalid arena key %d != %d for object locus %s%+"PRIiPTR,
                  obj->arena, arena->key, istr(module), offset);

   return obj;
}

void object_fixup_locus(ident_t module, ptrdiff_t *offset)
{
   if (*offset < 0) {
      object_arena_t *arena = arena_by_name(module);
      if (!arena->frozen)
         fatal_trace("cannot fixup locus %s%+"PRIiPTR" as arena not yet frozen",
                     istr(module), *offset);

      *offset = arena->forward[-*offset] >> OBJECT_ALIGN_BITS;
   }
}

void freeze_global_arena(void)
{
   if (global_arena != NULL) {
      object_arena_freeze(global_arena);
      global_arena = NULL;
   }
}

void make_new_arena(void)
{
   freeze_global_arena();
   global_arena = object_arena_new(object_arena_default_size(), standard());
}
