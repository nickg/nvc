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

#include <string.h>
#include <stdlib.h>

static const char *item_text_map[] = {
   "I_IDENT",    "I_VALUE",     "I_SEVERITY", "I_MESSAGE",    "I_TARGET",
   "I_LITERAL",  "I_IDENT2",    "I_DECLS",    "I_STMTS",      "I_PORTS",
   "I_GENERICS", "I_PARAMS",    "I_GENMAPS",  "I_WAVES",      "I_CONDS",
   "I_TYPE",     "I_SUBKIND",   "I_DELAY",    "I_REJECT",     "I_POS",
   "I_REF",      "I_FILE_MODE", "I_ASSOCS",   "I_CONTEXT",    "I_TRIGGERS",
   "I_ELSES",    "I_CLASS",     "I_RANGES",   "I_NAME",       "????",
   "I_DVAL",     "I_SPEC",      "I_SCOPES",   "I_INDEXCON",   "I_BASE",
   "I_ELEM",     "I_FILE",      "I_ACCESS",   "I_RESOLUTION", "I_RESULT",
   "I_UNITS",    "I_LITERALS",  "I_DIMS",     "I_FIELDS",     "I_PARENT",
   "I_ATTRS",    "I_PTYPES",    "I_CHARS",    "I_CONSTR",     "I_FLAGS",
   "I_SIGNALS",  "I_LEFT",      "I_RIGHT",    "I_PROCS",      "I_NEXUS",
   "I_PATH",     "I_DEPS",      "I_SIZE",     "I_VCODE",      "I_SLICE",
};

static object_class_t *classes[4];
static uint32_t        format_digest;
static generation_t    next_generation = 1;
static A(object_t*)    all_objects;

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
      const uint32_t format_fudge = 16;

      format_digest += format_fudge * UINT32_C(2654435761);

      done = true;
   }
}

object_t *object_new(const object_class_t *class, int kind)
{
   if (unlikely(kind >= class->last_kind))
      fatal_trace("invalid kind %d for %s object", kind, class->name);

   object_one_time_init();

   object_t *object = xcalloc(class->object_size[kind]);

   object->kind  = kind;
   object->tag   = class->tag;
   object->index = UINT32_MAX;
   object->loc   = LOC_INVALID;

   APUSH(all_objects, object);

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
         if (ITEM_OBJ_ARRAY & mask)
            free(object->items[n].obj_array.items);
         else if (ITEM_ATTRS & mask)
            free(object->items[n].attrs.table);
         n++;
      }
   }

   free(object);
}

void object_gc(void)
{
   // Generation will be updated by tree_visit
   const generation_t base_gen = next_generation;

   // Mark
   for (unsigned i = 0; i < all_objects.count; i++) {
      assert(all_objects.items[i] != NULL);

      const object_class_t *class = classes[all_objects.items[i]->tag];

      bool top_level = false;
      for (int j = 0; (j < class->gc_num_roots) && !top_level; j++) {
         if (class->gc_roots[j] == all_objects.items[i]->kind)
            top_level = true;
      }

      if (top_level) {
         object_visit_ctx_t ctx = {
            .count      = 0,
            .postorder  = NULL,
            .preorder   = NULL,
            .context    = NULL,
            .kind       = T_LAST_TREE_KIND,
            .generation = next_generation++,
            .deep       = true
         };

         object_visit(all_objects.items[i], &ctx);
      }
   }

   // Sweep
   for (unsigned i = 0; i < all_objects.count; i++) {
      object_t *object = all_objects.items[i];
      if (object->generation < base_gen) {
         object_sweep(object);
         all_objects.items[i] = NULL;
      }
   }

   // Compact
   size_t p = 0;
   for (unsigned i = 0; i < all_objects.count; i++) {
      if (all_objects.items[i] != NULL)
         all_objects.items[p++] = all_objects.items[i];
   }

   if ((getenv("NVC_GC_VERBOSE") != NULL) || is_debugger_running())
      notef("GC: freed %zu objects; %zu allocated", all_objects.count - p, p);

   ATRIM(all_objects, p);
}

void object_visit(object_t *object, object_visit_ctx_t *ctx)
{
   // If `deep' then will follow links above the tree originally passed
   // to tree_visit - e.g. following references back to their declarations
   // Outside the garbage collector this is usually not what is required

   if ((object == NULL) || (object->generation == ctx->generation))
      return;

   object->generation = ctx->generation;

   const bool visit =
      (object->tag == OBJECT_TAG_TREE && object->kind == ctx->kind)
      || ctx->kind == T_LAST_TREE_KIND;

   if (visit && ctx->preorder != NULL)
      (*ctx->preorder)((tree_t)object, ctx->context);

   const imask_t deep_mask = I_TYPE | I_REF | I_ATTRS;

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
         else if (ITEM_ATTRS & mask) {
            attr_tab_t *attrs = &(object->items[i].attrs);
            for (unsigned j = 0; j < attrs->num; j++) {
               switch (attrs->table[j].kind) {
               case A_TREE:
                  object_visit((object_t *)attrs->table[j].tval, ctx);
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

   if (object->generation == ctx->generation) {
      // Already rewritten this tree so return the cached version
      return ctx->cache[object->index];
   }

   const imask_t skip_mask = (I_REF | I_ATTRS);

   const object_class_t *class = classes[object->tag];

   const imask_t has = class->has_map[object->kind];
   const int nitems = class->object_nitems[object->kind];
   int type_item = -1;
   imask_t mask = 1;
   for (int n = 0; n < nitems; mask <<= 1) {
      if (has & mask & ~skip_mask) {
         if (ITEM_IDENT & mask)
            ;
         else if (ITEM_OBJECT & mask) {
            object_t *o = object->items[n].object;
            if (o != NULL && o->tag == OBJECT_TAG_TYPE)
               type_item = n;
            else {
               object->items[n].object = object_rewrite(o, ctx);
            }
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
         else
            item_without_type(mask);
      }

      if (has & mask)
         n++;
   }

   if (unlikely(ctx->cache == NULL)) {
      ctx->cache_size = MIN(4096, all_objects.count);
      ctx->cache = xcalloc(sizeof(object_t *) * ctx->cache_size);
   }
   else if (unlikely(ctx->index >= ctx->cache_size)) {
      assert(ctx->index < all_objects.count);
      while (ctx->index >= ctx->cache_size) {
         const size_t newsz = (ctx->cache_size * 3) / 2;
         ctx->cache = xrealloc(ctx->cache, newsz * sizeof(object_t *));
         memset(ctx->cache + ctx->cache_size, '\0',
                (newsz - ctx->cache_size) * sizeof(object_t *));
         ctx->cache_size = newsz;
      }
   }

   if (object->generation != ctx->generation) {
      object->generation = ctx->generation;
      object->index      = ctx->index++;
   }

   if (object->tag == OBJECT_TAG_TREE) {
      // Rewrite this tree before we rewrite the type as there may
      // be a circular reference
      object_t *new = (object_t *)(*ctx->fn)((tree_t)object, ctx->context);
      if (new != NULL && object != new) {
         new->generation = ctx->generation;
         new->index      = object->index;
      }
      ctx->cache[object->index] = new;
   }
   else
      ctx->cache[object->index] = object;

   if (type_item != -1)
      (void)object_rewrite((object_t *)object->items[type_item].type, ctx);

   return ctx->cache[object->index];
}

void object_write(object_t *object, object_wr_ctx_t *ctx)
{
   if (object == NULL) {
      write_u16(UINT16_C(0xffff), ctx->file);  // Null marker
      return;
   }

   if (object->generation == ctx->generation) {
      // Already visited this tree
      write_u16(UINT16_C(0xfffe), ctx->file);   // Back reference marker
      write_u32(object->index, ctx->file);
      return;
   }

   object->generation = ctx->generation;
   object->index      = (ctx->n_objects)++;

   const uint16_t marker = (object->tag & 0xf) << 12 | (object->kind & 0xfff);
   write_u16(marker, ctx->file);

   if (object->tag == OBJECT_TAG_TREE)
      loc_write(&object->loc, ctx->loc_ctx);

   const object_class_t *class = classes[object->tag];

   const imask_t has = class->has_map[object->kind];
   const int nitems = class->object_nitems[object->kind];
   imask_t mask = 1;
   for (int n = 0; n < nitems; mask <<= 1) {
      if (has & mask) {
         if (ITEM_IDENT & mask)
            ident_write(object->items[n].ident, ctx->ident_ctx);
         else if (ITEM_OBJECT & mask)
            object_write(object->items[n].object, ctx);
         else if (ITEM_OBJ_ARRAY & mask) {
            const unsigned count = object->items[n].obj_array.count;
            write_u32(count, ctx->file);
            for (unsigned i = 0; i < count; i++)
               object_write(object->items[n].obj_array.items[i], ctx);
         }
         else if (ITEM_INT64 & mask)
            write_u64(object->items[n].ival, ctx->file);
         else if (ITEM_INT32 & mask)
            write_u32(object->items[n].ival, ctx->file);
         else if (ITEM_DOUBLE & mask)
            write_double(object->items[n].dval, ctx->file);
         else if (ITEM_ATTRS & mask) {
            const attr_tab_t *attrs = &(object->items[n].attrs);
            write_u16(attrs->num, ctx->file);
            for (unsigned i = 0; i < attrs->num; i++) {
               write_u16(attrs->table[i].kind, ctx->file);
               ident_write(attrs->table[i].name, ctx->ident_ctx);

               switch (attrs->table[i].kind) {
               case A_STRING:
                  ident_write(attrs->table[i].sval, ctx->ident_ctx);
                  break;

               case A_INT:
                  write_u32(attrs->table[i].ival, ctx->file);
                  break;

               case A_TREE:
                  object_write((object_t *)attrs->table[i].tval, ctx);
                  break;

               case A_PTR:
                  fatal("pointer attributes cannot be saved");
               }
            }
         }
         else if (ITEM_IDENT_ARRAY & mask) {
            item_t *item = &(object->items[n]);
            write_u32(item->ident_array.count, ctx->file);
            for (unsigned i = 0; i < item->ident_array.count; i++)
               ident_write(item->ident_array.items[i], ctx->ident_ctx);
         }
         else
            item_without_type(mask);
         n++;
      }
   }
}

object_wr_ctx_t *object_write_begin(fbuf_t *f)
{
   write_u32(format_digest, f);
   write_u8(standard(), f);

   object_wr_ctx_t *ctx = xmalloc(sizeof(object_wr_ctx_t));
   ctx->file       = f;
   ctx->generation = next_generation++;
   ctx->n_objects  = 0;
   ctx->ident_ctx  = ident_write_begin(f);
   ctx->loc_ctx    = loc_write_begin(f);

   return ctx;
}

void object_write_end(object_wr_ctx_t *ctx)
{
   ident_write_end(ctx->ident_ctx);
   loc_write_end(ctx->loc_ctx);
   free(ctx);
}

object_t *object_read(object_rd_ctx_t *ctx)
{
   uint16_t marker = read_u16(ctx->file);
   if (marker == UINT16_C(0xffff))
      return NULL;    // Null marker
   else if (marker == UINT16_C(0xfffe)) {
      // Back reference marker
      index_t index = read_u32(ctx->file);
      assert(index < ctx->n_objects);
      return ctx->store[index];
   }

   const unsigned tag = (marker >> 12) & 0xf;
   assert(tag < OBJECT_TAG_COUNT);

   const object_class_t *class = classes[tag];

   const unsigned kind = marker & 0xfff;
   assert(kind < class->last_kind);

   object_t *object = object_new(class, kind);

   if (tag == OBJECT_TAG_TREE)
      loc_read(&(object->loc), ctx->loc_ctx);

   // Stash pointer for later back references
   // This must be done early as a child node of this type may
   // reference upwards
   object->index = ctx->n_objects++;
   if (ctx->n_objects == ctx->store_sz) {
      ctx->store_sz *= 2;
      ctx->store = xrealloc(ctx->store, ctx->store_sz * sizeof(tree_t));
   }
   ctx->store[object->index] = object;

   const imask_t has = class->has_map[object->kind];
   const int nitems = class->object_nitems[object->kind];
   imask_t mask = 1;
   for (int n = 0; n < nitems; mask <<= 1) {
      if (has & mask) {
         if (ITEM_IDENT & mask)
            object->items[n].ident = ident_read(ctx->ident_ctx);
         else if (ITEM_OBJECT & mask)
            object->items[n].object = object_read(ctx);
         else if (ITEM_OBJ_ARRAY & mask) {
            const unsigned count = read_u32(ctx->file);
            ARESIZE(object->items[n].obj_array, count);
            for (unsigned i = 0; i < count; i++) {
               object_t *o = object_read(ctx);
               object->items[n].obj_array.items[i] = o;
            }
         }
         else if (ITEM_INT64 & mask)
            object->items[n].ival = read_u64(ctx->file);
         else if (ITEM_INT32 & mask)
            object->items[n].ival = read_u32(ctx->file);
         else if (ITEM_DOUBLE & mask)
            object->items[n].dval = read_double(ctx->file);
         else if (ITEM_ATTRS & mask) {
            attr_tab_t *attrs = &(object->items[n].attrs);

            attrs->num = read_u16(ctx->file);
            if (attrs->num > 0) {
               attrs->alloc = next_power_of_2(attrs->num);
               attrs->table = xmalloc_array(sizeof(attr_t), attrs->alloc);
            }

            for (unsigned i = 0; i < attrs->num; i++) {
               attrs->table[i].kind = read_u16(ctx->file);
               attrs->table[i].name = ident_read(ctx->ident_ctx);

               switch (attrs->table[i].kind) {
               case A_STRING:
                  attrs->table[i].sval = ident_read(ctx->ident_ctx);
                  break;

               case A_INT:
                  attrs->table[i].ival = read_u32(ctx->file);
                  break;

               case A_TREE:
                  attrs->table[i].tval = (tree_t)object_read(ctx);
                  break;

               default:
                  abort();
               }
            }
         }
         else if (ITEM_IDENT_ARRAY & mask) {
            const unsigned count = read_u32(ctx->file);
            ARESIZE(object->items[n].ident_array, count);;
            for (unsigned i = 0; i < count; i++) {
               ident_t id = ident_read(ctx->ident_ctx);
               object->items[n].ident_array.items[i] = id;
            }
         }
         else
            item_without_type(mask);
         n++;
      }
   }

   return object;
}

object_rd_ctx_t *object_read_begin(fbuf_t *f, const char *fname)
{
   object_one_time_init();

   const uint32_t ver = read_u32(f);
   if (ver != format_digest)
      fatal("%s: serialised format digest is %x expected %x. This design "
            "unit uses a library format from an earlier version of "
            PACKAGE_NAME " and should be reanalysed.",
            fname, ver, format_digest);

   const vhdl_standard_t std = read_u8(f);
   if (std > standard())
      fatal("%s: design unit was analysed using standard revision %s which "
            "is more recent that the currently selected standard %s",
            fname, standard_text(std), standard_text(standard()));

   object_rd_ctx_t *ctx = xcalloc(sizeof(object_rd_ctx_t));
   ctx->file      = f;
   ctx->ident_ctx = ident_read_begin(f);
   ctx->loc_ctx   = loc_read_begin(f);
   ctx->store_sz  = 256;
   ctx->store     = xmalloc_array(ctx->store_sz, sizeof(object_t *));
   ctx->n_objects = 0;
   ctx->db_fname  = xstrdup(fname);

   return ctx;
}

void object_read_end(object_rd_ctx_t *ctx)
{
   ident_read_end(ctx->ident_ctx);
   loc_read_end(ctx->loc_ctx);
   free(ctx->store);
   free(ctx->db_fname);
   free(ctx);
}

unsigned object_next_generation(void)
{
   return next_generation++;
}

bool object_copy_mark(object_t *object, object_copy_ctx_t *ctx)
{
   if (object == NULL)
      return false;

   if (object->generation == ctx->generation)
      return (object->index != UINT32_MAX);

   object->generation = ctx->generation;
   object->index      = UINT32_MAX;

   const object_class_t *class = classes[object->tag];

   bool marked = false;
   if (object->tag == OBJECT_TAG_TREE) {
      if ((marked = (*ctx->callback)((tree_t)object, ctx->context)))
         object->index = (ctx->index)++;
   }

   int type_item = -1;
   const imask_t has = class->has_map[object->kind];
   const int nitems = class->object_nitems[object->kind];
   imask_t mask = 1;
   for (int n = 0; n < nitems; mask <<= 1) {
      if (has & mask) {
         if (ITEM_IDENT & mask)
            ;
         else if (ITEM_OBJECT & mask) {
            object_t *o = object->items[n].object;
            if (o != NULL && o->tag == OBJECT_TAG_TYPE)
               type_item = n;
            else
               marked = object_copy_mark(o, ctx) || marked;
         }
         else if (ITEM_DOUBLE & mask)
            ;
         else if (ITEM_OBJ_ARRAY & mask) {
            for (unsigned i = 0; i < object->items[n].obj_array.count; i++) {
               object_t *o = object->items[n].obj_array.items[i];
               marked = object_copy_mark(o, ctx) || marked;
            }
         }
         else if (ITEM_INT64 & mask)
            ;
         else if (ITEM_INT32 & mask)
            ;
         else if (ITEM_ATTRS & mask)
            ;
         else
            item_without_type(mask);
         n++;
      }
   }

   // Check type last as it may contain a circular reference
   if (type_item != -1)
      marked = object_copy_mark(object->items[type_item].object, ctx)
         || marked;

   if (marked && (object->index == UINT32_MAX))
      object->index = (ctx->index)++;

   return marked;
}

object_t *object_copy_sweep(object_t *object, object_copy_ctx_t *ctx)
{
   if (object == NULL)
      return NULL;

   assert(object->generation == ctx->generation);

   if (object->index == UINT32_MAX)
      return object;

   assert(object->index < ctx->index);

   if (ctx->copied[object->index] != NULL) {
      // Already copied this object
      return ctx->copied[object->index];
   }

   const object_class_t *class = classes[object->tag];

   object_t *copy = object_new(class, object->kind);
   ctx->copied[object->index] = copy;

   copy->loc = object->loc;

   const imask_t has = class->has_map[object->kind];
   const int nitems = class->object_nitems[object->kind];
   imask_t mask = 1;
   for (int n = 0; n < nitems; mask <<= 1) {
      if (has & mask) {
         if (ITEM_IDENT & mask)
            copy->items[n].ident = object->items[n].ident;
         else if (ITEM_OBJECT & mask)
            copy->items[n].object =
               object_copy_sweep(object->items[n].object, ctx);
         else if (ITEM_DOUBLE & mask)
            copy->items[n].dval = object->items[n].dval;
         else if (ITEM_OBJ_ARRAY & mask) {
            const item_t *from = &(object->items[n]);
            item_t *to = &(copy->items[n]);
            ARESIZE(to->obj_array, from->obj_array.count);
            for (size_t i = 0; i < from->obj_array.count; i++) {
               object_t *o = object_copy_sweep(from->obj_array.items[i], ctx);
               to->obj_array.items[i] = o;
            }
         }
         else if ((ITEM_INT64 & mask) || (ITEM_INT32 & mask))
            copy->items[n].ival = object->items[n].ival;
         else if (ITEM_ATTRS & mask) {
            if ((copy->items[n].attrs.num = object->items[n].attrs.num) > 0) {
               copy->items[n].attrs.alloc = object->items[n].attrs.alloc;
               copy->items[n].attrs.table =
                  xmalloc_array(copy->items[n].attrs.alloc, sizeof(attr_t));
               for (unsigned i = 0; i < object->items[n].attrs.num; i++)
                  copy->items[n].attrs.table[i] =
                     object->items[n].attrs.table[i];
            }
         }
         else
            item_without_type(mask);
         n++;
      }
   }

   return copy;
}
