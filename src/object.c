//
//  Copyright (C) 2014  Nick Gasson
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

#include <string.h>
#include <stdlib.h>

DEFINE_ARRAY(tree);
DEFINE_ARRAY(netid);
DEFINE_ARRAY(type);
DEFINE_ARRAY(range);

static const char *item_text_map[] = {
   "I_IDENT",    "I_VALUE",     "I_SEVERITY", "I_MESSAGE",    "I_TARGET",
   "I_LITERAL",  "I_IDENT2",    "I_DECLS",    "I_STMTS",      "I_PORTS",
   "I_GENERICS", "I_PARAMS",    "I_GENMAPS",  "I_WAVES",      "I_CONDS",
   "I_TYPE",     "I_SUBKIND",   "I_DELAY",    "I_REJECT",     "I_POS",
   "I_REF",      "I_FILE_MODE", "I_ASSOCS",   "I_CONTEXT",    "I_TRIGGERS",
   "I_ELSES",    "I_CLASS",     "I_RANGE",    "I_NAME",       "I_NETS",
   "I_DVAL",     "I_SPEC",      "I_OPS",      "I_CONSTR",     "I_BASE",
   "I_ELEM",     "I_FILE",      "I_ACCESS",   "I_RESOLUTION", "I_RESULT",
   "I_UNITS",    "I_LITERALS",  "I_DIMS",     "I_FIELDS",     "I_TEXT_BUF",
   "I_ATTRS"
};

static object_class_t *classes[4];

// Garbage collection
static object_t **all_objects = NULL;
static size_t     max_objects = 256;   // Grows at runtime
static size_t     n_objects_alloc = 0;

uint32_t format_digest;    // XXX: make static
extern unsigned next_generation;  // XXX

void object_lookup_failed(const char *name, const char **kind_text_map,
                          int kind, imask_t mask)
{
   int item;
   for (item = 0; (mask & (1ull << item)) == 0; item++)
      ;

   printf("item=%d len=%d\n", item, (int)ARRAY_LEN(item_text_map));

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

uint32_t object_index(const object_t *object)
{
   assert(object != NULL);
   assert(object->index != UINT32_MAX);

   return object->index;
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
   class->object_size   = xmalloc(class->last_kind * sizeof(size_t));
   class->object_nitems = xmalloc(class->last_kind * sizeof(int));
   class->item_lookup   = xmalloc(class->last_kind * sizeof(int) * 64);

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
         }

         if (max_size != class->object_size[i]) {
            class->object_size[i] = max_size;
            changed = true;
         }
      }
   } while (changed);

   if (getenv("NVC_TREE_SIZES") != NULL) {
      for (int i = 0; i < T_LAST_TREE_KIND; i++)
         printf("%-15s %d\n", class->kind_text_map[i],
                (int)class->object_size[i]);
   }
}

void object_one_time_init(void)
{
   extern object_class_t tree_object;
   extern object_class_t type_object;

   static bool done = false;

   if (unlikely(!done)) {
      object_init(&tree_object);
      object_init(&type_object);

      // Increment this each time a incompatible change is made to the
      // on-disk format not expressed in the tree and type items table
      const uint32_t format_fudge = 2;

      format_digest += format_fudge;

      done = true;
   }
}

object_t *object_new(object_class_t *class, int kind)
{
   if (unlikely(kind >= class->last_kind))
      fatal_trace("invalid kind %d for %s object", kind, class->name);

   object_one_time_init();

   object_t *object = xcalloc(class->object_size[kind]);

   object->kind  = kind;
   object->tag   = class->tag;
   object->index = UINT32_MAX;

   if (unlikely(all_objects == NULL))
      all_objects = xmalloc(sizeof(object_t *) * max_objects);

   ARRAY_APPEND(all_objects, object, n_objects_alloc, max_objects);

   return object;
}

static void object_sweep(object_t *object)
{
   const object_class_t *class = classes[object->tag];

   const imask_t has = class->has_map[object->kind];
   const int nitems = class->object_nitems[object->kind];
   imask_t mask = 1;
   for (int n = 0; n < nitems; mask <<= 1) {
      if (has & mask) {
         if (ITEM_TREE_ARRAY & mask)
            free(object->items[n].tree_array.items);
         else if (ITEM_NETID_ARRAY & mask)
            free(object->items[n].netid_array.items);
         else if (ITEM_RANGE & mask)
            free(object->items[n].range);
         else if (ITEM_ATTRS & mask)
            free(object->items[n].attrs.table);
         else if (ITEM_RANGE_ARRAY & mask)
            free(object->items[n].range_array.items);
         else if (ITEM_TEXT_BUF & mask) {
            if (object->items[n].text_buf != NULL)
               tb_free(object->items[n].text_buf);
         }
         n++;
      }
   }

   free(object);
}

void object_gc(void)
{
   // Generation will be updated by tree_visit
   const unsigned base_gen = next_generation;

   // Mark
   for (unsigned i = 0; i < n_objects_alloc; i++) {
      assert(all_objects[i] != NULL);

      const object_class_t *class = classes[all_objects[i]->tag];

      bool top_level = false;
      for (int j = 0; (j < class->gc_num_roots) && !top_level; j++) {
         if (class->gc_roots[j] == all_objects[i]->kind)
            top_level = true;
      }

      if (top_level) {
         object_visit_ctx_t ctx = {
            .count      = 0,
            .fn         = NULL,
            .context    = NULL,
            .kind       = T_LAST_TREE_KIND,
            .generation = next_generation++,
            .deep       = true
         };

         object_visit(all_objects[i], &ctx);
      }
   }

   // Sweep
   for (unsigned i = 0; i < n_objects_alloc; i++) {
      object_t *object = all_objects[i];
      if (object->generation < base_gen) {
         object_sweep(object);
         all_objects[i] = NULL;
      }
   }

   // Compact
   size_t p = 0;
   for (unsigned i = 0; i < n_objects_alloc; i++) {
      if (all_objects[i] != NULL)
         all_objects[p++] = all_objects[i];
   }

   if ((getenv("NVC_GC_VERBOSE") != NULL) || is_debugger_running())
      notef("GC: freed %zu objects; %zu allocated", n_objects_alloc - p, p);

   n_objects_alloc = p;
}

void object_visit(object_t *object, object_visit_ctx_t *ctx)
{
   // If `deep' then will follow links above the tree originally passed
   // to tree_visit - e.g. following references back to their declarations
   // Outside the garbage collector this is usually not what is required

   if ((object == NULL) || (object->generation == ctx->generation))
      return;

   object->generation = ctx->generation;

   const imask_t deep_mask = I_TYPE | I_REF | I_ATTRS;

   const object_class_t *class = classes[object->tag];

   const imask_t has = class->has_map[object->kind];
   const int nitems = class->object_nitems[object->kind];
   imask_t mask = 1;
   for (int i = 0; i < nitems; mask <<= 1) {
      if (has & mask & ~(ctx->deep ? 0 : deep_mask)) {
         if (ITEM_IDENT & mask)
            ;
         else if (ITEM_TREE & mask)
            object_visit((object_t *)object->items[i].tree, ctx);
         else if (ITEM_TREE_ARRAY & mask) {
            for (unsigned j = 0; j < object->items[i].tree_array.count; j++)
               object_visit((object_t *)object->items[i].tree_array.items[j], ctx);
         }
         else if (ITEM_TYPE_ARRAY & mask) {
            type_array_t *a = &(object->items[i].type_array);
            for (unsigned j = 0; j < a->count; j++)
               object_visit((object_t *)a->items[j], ctx);
         }
         else if (ITEM_TYPE & mask)
            object_visit((object_t *)object->items[i].type, ctx);
         else if (ITEM_INT64 & mask)
            ;
         else if (ITEM_DOUBLE & mask)
            ;
         else if (ITEM_RANGE & mask) {
            if (object->items[i].range != NULL) {
               object_visit((object_t *)object->items[i].range->left, ctx);
               object_visit((object_t *)object->items[i].range->right, ctx);
            }
         }
         else if (ITEM_RANGE_ARRAY & mask) {
            range_array_t *a = &(object->items[i].range_array);
            for (unsigned j = 0; j < a->count; j++) {
               object_visit((object_t *)a->items[j].left, ctx);
               object_visit((object_t *)a->items[j].right, ctx);
            }
         }
         else if (ITEM_NETID_ARRAY & mask)
            ;
         else if (ITEM_TEXT_BUF & mask)
            ;
         else if (ITEM_ATTRS & mask) {
            for (unsigned j = 0; j < object->items[i].attrs.num; j++) {
               switch (object->items[i].attrs.table[j].kind) {
               case A_TREE:
                  object_visit((object_t *)object->items[i].attrs.table[j].tval, ctx);
                  break;

               default:
                  break;
               }
            }
         }
         else
            item_without_type(mask);
      }

      if (has & mask)
         i++;
   }

   if (object->tag == ctx->tag) {
      if ((object->kind == ctx->kind) || (ctx->kind == T_LAST_TREE_KIND)) {
         if (ctx->fn)
            (*ctx->fn)((tree_t)object, ctx->context);
         ctx->count++;
      }
   }
}
