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
#include "common.h"

#include <string.h>
#include <stdlib.h>

DEFINE_ARRAY(tree);
DEFINE_ARRAY(netid);
DEFINE_ARRAY(type);
DEFINE_ARRAY(range);
DEFINE_ARRAY(ident);

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
   "I_ATTRS",    "I_PTYPES",    "I_CHARS"
};

static object_class_t *classes[4];
static uint32_t        format_digest;
static unsigned        next_generation = 1;
static object_t      **all_objects = NULL;
static size_t          max_objects = 256;   // Grows at runtime
static size_t          n_objects_alloc = 0;

void object_lookup_failed(const char *name, const char **kind_text_map,
                          int kind, imask_t mask)
{
   int item;
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
      const uint32_t format_fudge = 4;

      format_digest += format_fudge;

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
            tree_array_t *a = &(object->items[i].tree_array);
            for (unsigned j = 0; j < a->count; j++)
               object_visit((object_t *)a->items[j], ctx);
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
         else if (ITEM_IDENT_ARRAY & mask)
            ;
         else
            item_without_type(mask);
      }

      if (has & mask)
         i++;
   }

   const bool visit =
      ((object->tag == OBJECT_TAG_TREE) && (object->kind == ctx->kind))
      || (ctx->kind == T_LAST_TREE_KIND);

   if (visit) {
      if ((object->tag == OBJECT_TAG_TREE) && (ctx->fn != NULL))
         (*ctx->fn)((tree_t)object, ctx->context);
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

   if (unlikely(ctx->cache == NULL))
      ctx->cache = xcalloc(sizeof(object_t *) * n_objects_alloc);

   const imask_t skip_mask = (I_REF | I_ATTRS | I_NETS);

   const object_class_t *class = classes[object->tag];

   const imask_t has = class->has_map[object->kind];
   const int nitems = class->object_nitems[object->kind];
   int type_item = -1;
   imask_t mask = 1;
   for (int n = 0; n < nitems; mask <<= 1) {
      if (has & mask & ~skip_mask) {
         if (ITEM_IDENT & mask)
            ;
         else if (ITEM_TREE & mask)
            object->items[n].tree =
               (tree_t)object_rewrite((object_t *)object->items[n].tree, ctx);
         else if (ITEM_TREE_ARRAY & mask) {
            tree_array_t *a = &(object->items[n].tree_array);

            for (size_t i = 0; i < a->count; i++)
               a->items[i] =
                  (tree_t)object_rewrite((object_t *)a->items[i], ctx);

            // If an item was rewritten to NULL then delete it
            size_t n = 0;
            for (size_t i = 0; i < a->count; i++) {
               if (a->items[i] != NULL)
                  a->items[n++] = a->items[i];
            }
            a->count = n;
         }
         else if (ITEM_TYPE & mask)
            type_item = n;
         else if (ITEM_INT64 & mask)
            ;
         else if (ITEM_DOUBLE & mask)
            ;
         else if (ITEM_RANGE & mask) {
            range_t *r = object->items[n].range;
            if (r != NULL) {
               r->left  = (tree_t)object_rewrite((object_t *)r->left, ctx);
               r->right = (tree_t)object_rewrite((object_t *)r->right, ctx);
            }
         }
         else if (ITEM_TYPE_ARRAY & mask) {
            type_array_t *a = &(object->items[n].type_array);
            for (unsigned i = 0; i < a->count; i++)
               (void)object_rewrite((object_t *)a->items[i], ctx);
         }
         else if (ITEM_RANGE_ARRAY & mask) {
            range_array_t *a = &(object->items[n].range_array);
            for (unsigned i = 0; i < a->count; i++) {
               a->items[i].left =
                  (tree_t)object_rewrite((object_t *)a->items[i].left, ctx);
               a->items[i].right =
                  (tree_t)object_rewrite((object_t *)a->items[i].right, ctx);
               assert(a->items[i].left);
               assert(a->items[i].right);
            }
         }
         else if (ITEM_TEXT_BUF & mask)
            ;
         else if (ITEM_IDENT_ARRAY & mask)
            ;
         else
            item_without_type(mask);
      }

      if (has & mask)
         n++;
   }

   object->generation = ctx->generation;
   object->index      = ctx->index++;

   if (object->tag == OBJECT_TAG_TREE) {
      // Rewrite this tree before we rewrite the type as there may
      // be a circular reference
      ctx->cache[object->index] =
         (object_t *)(*ctx->fn)((tree_t)object, ctx->context);
   }
   else
      ctx->cache[object->index] = object;

   if (type_item != -1)
      (void)object_rewrite((object_t *)object->items[type_item].type, ctx);

   return ctx->cache[object->index];
}

static void write_loc(loc_t *l, object_wr_ctx_t *ctx)
{
   if (l->file == NULL) {
      write_u16(UINT16_C(0xfffe), ctx->file);  // Invalid location marker
      return;
   }

   uint16_t findex;
   for (findex = 0;
        (findex < MAX_FILES)
           && (ctx->file_names[findex] != NULL)
           && (strcmp(ctx->file_names[findex], l->file) != 0);
        findex++)
      ;
   assert(findex != MAX_FILES);

   if (ctx->file_names[findex] == NULL) {
      const size_t len = strlen(l->file) + 1;

      ctx->file_names[findex] = l->file;

      write_u16(findex | UINT16_C(0x8000), ctx->file);
      write_u16(len, ctx->file);
      write_raw(l->file, len, ctx->file);
   }
   else
      write_u16(findex, ctx->file);

   const uint64_t merged =
      ((uint64_t)l->first_line << 44)
      | ((uint64_t)l->first_column << 32)
      | ((uint64_t)l->last_line << 12)
      | (uint64_t)l->last_column;

   write_u64(merged, ctx->file);
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

   write_u16(object->kind, ctx->file);

   if (object->tag == OBJECT_TAG_TREE)
      write_loc(&object->loc, ctx);

   const object_class_t *class = classes[object->tag];

   const imask_t has = class->has_map[object->kind];
   const int nitems = class->object_nitems[object->kind];
   imask_t mask = 1;
   for (int n = 0; n < nitems; mask <<= 1) {
      if (has & mask) {
         if (ITEM_IDENT & mask)
            ident_write(object->items[n].ident, ctx->ident_ctx);
         else if (ITEM_TREE & mask)
            object_write((object_t *)object->items[n].tree, ctx);
         else if (ITEM_TYPE & mask)
            object_write((object_t *)object->items[n].type, ctx);
         else if (ITEM_TREE_ARRAY & mask) {
            const tree_array_t *a = &(object->items[n].tree_array);
            write_u32(a->count, ctx->file);
            for (unsigned i = 0; i < a->count; i++)
               object_write((object_t *)a->items[i], ctx);
         }
         else if (ITEM_TYPE_ARRAY & mask) {
            const type_array_t *a = &(object->items[n].type_array);
            write_u16(a->count, ctx->file);
            for (unsigned i = 0; i < a->count; i++)
               object_write((object_t *)a->items[i], ctx);
         }
         else if (ITEM_INT64 & mask)
            write_u64(object->items[n].ival, ctx->file);
         else if (ITEM_RANGE & mask) {
            if (object->items[n].range != NULL) {
               write_u8(object->items[n].range->kind, ctx->file);
               object_write((object_t *)object->items[n].range->left, ctx);
               object_write((object_t *)object->items[n].range->right, ctx);
            }
            else
               write_u8(UINT8_C(0xff), ctx->file);
         }
         else if (ITEM_NETID_ARRAY & mask) {
            const netid_array_t *a = &(object->items[n].netid_array);
            write_u32(a->count, ctx->file);
            for (unsigned i = 0; i < a->count; i++)
               write_u32(a->items[i], ctx->file);
         }
         else if (ITEM_DOUBLE & mask) {
            union { double d; uint64_t i; } u;
            u.d = object->items[n].dval;
            write_u64(u.i, ctx->file);
         }
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
         else if (ITEM_RANGE_ARRAY & mask) {
            range_array_t *a = &(object->items[n].range_array);
            write_u16(a->count, ctx->file);
            for (unsigned i = 0; i < a->count; i++) {
               write_u8(a->items[i].kind, ctx->file);
               object_write((object_t *)a->items[i].left, ctx);
               object_write((object_t *)a->items[i].right, ctx);
            }
         }
         else if (ITEM_IDENT_ARRAY & mask) {
            ident_array_t *a = &(object->items[n].ident_array);
            write_u32(a->count, ctx->file);
            for (unsigned i = 0; i < a->count; i++)
               ident_write(a->items[i], ctx->ident_ctx);
         }
         else if (ITEM_TEXT_BUF & mask)
            ;
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
   memset(ctx->file_names, '\0', sizeof(ctx->file_names));

   return ctx;
}

void object_write_end(object_wr_ctx_t *ctx)
{
   ident_write_end(ctx->ident_ctx);
   free(ctx);
}

fbuf_t *object_write_file(object_wr_ctx_t *ctx)
{
   return ctx->file;
}

static loc_t read_loc(object_rd_ctx_t *ctx)
{
   const char *fname;
   uint16_t fmarker = read_u16(ctx->file);
   if (fmarker == UINT16_C(0xfffe))
      return LOC_INVALID;
   else if (fmarker & UINT16_C(0x8000)) {
      uint16_t index = fmarker & UINT16_C(0x7fff);
      assert(index < MAX_FILES);
      uint16_t len = read_u16(ctx->file);
      char *buf = xmalloc(len);
      read_raw(buf, len, ctx->file);

      ctx->file_names[index] = buf;
      fname = buf;
   }
   else {
      assert(fmarker < MAX_FILES);
      fname = ctx->file_names[fmarker];
      assert(fname != NULL);
   }

   loc_t l = { .file = fname, .linebuf = NULL };

   const uint64_t merged = read_u64(ctx->file);

   l.first_line   = (merged >> 44) & 0xfffff;
   l.first_column = (merged >> 32) & 0xfff;
   l.last_line    = (merged >> 12) & 0xfffff;
   l.last_column  = merged & 0xfff;

   return l;
}

object_t *object_read(object_rd_ctx_t *ctx, int tag)
{
   uint16_t marker = read_u16(ctx->file);
   if (marker == UINT16_C(0xffff))
      return NULL;    // Null marker
   else if (marker == UINT16_C(0xfffe)) {
      // Back reference marker
      unsigned index = read_u32(ctx->file);
      assert(index < ctx->n_objects);
      return ctx->store[index];
   }

   const object_class_t *class = classes[tag];

   assert(marker < class->last_kind);

   object_t *object = object_new(class, marker);

   if (tag == OBJECT_TAG_TREE)
      object->loc = read_loc(ctx);

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
         else if (ITEM_TREE & mask)
            object->items[n].tree = (tree_t)object_read(ctx, OBJECT_TAG_TREE);
         else if (ITEM_TYPE & mask)
            object->items[n].tree = (tree_t)object_read(ctx, OBJECT_TAG_TYPE);
         else if (ITEM_TREE_ARRAY & mask) {
            tree_array_t *a = &(object->items[n].tree_array);
            tree_array_resize(a, read_u32(ctx->file), NULL);
            for (unsigned i = 0; i < a->count; i++)
               a->items[i] = (tree_t)object_read(ctx, OBJECT_TAG_TREE);
         }
         else if (ITEM_TYPE_ARRAY & mask) {
            type_array_t *a = &(object->items[n].type_array);
            type_array_resize(a, read_u16(ctx->file), NULL);
            for (unsigned i = 0; i < a->count; i++)
               a->items[i] = (type_t)object_read(ctx, OBJECT_TAG_TYPE);
         }
         else if (ITEM_INT64 & mask)
            object->items[n].ival = read_u64(ctx->file);
         else if (ITEM_RANGE & mask) {
            const uint8_t rmarker = read_u8(ctx->file);
            if (rmarker != UINT8_C(0xff)) {
               object->items[n].range = xmalloc(sizeof(range_t));
               object->items[n].range->kind = rmarker;
               object->items[n].range->left =
                  (tree_t)object_read(ctx, OBJECT_TAG_TREE);
               object->items[n].range->right =
                  (tree_t)object_read(ctx, OBJECT_TAG_TREE);
            }
         }
         else if (ITEM_RANGE_ARRAY & mask) {
            range_array_t *a = &(object->items[n].range_array);
            range_t dummy = { NULL, NULL, 0 };
            range_array_resize(a, read_u16(ctx->file), dummy);

            for (unsigned i = 0; i < a->count; i++) {
               a->items[i].kind  = read_u8(ctx->file);
               a->items[i].left  =
                  (tree_t)object_read(ctx, OBJECT_TAG_TREE);
               a->items[i].right =
                  (tree_t)object_read(ctx, OBJECT_TAG_TREE);
            }
         }
         else if (ITEM_TEXT_BUF & mask)
            ;
         else if (ITEM_NETID_ARRAY & mask) {
            netid_array_t *a = &(object->items[n].netid_array);
            netid_array_resize(a, read_u32(ctx->file), NETID_INVALID);
            for (unsigned i = 0; i < a->count; i++)
               a->items[i] = read_u32(ctx->file);
         }
         else if (ITEM_IDENT_ARRAY & mask) {
            ident_array_t *a = &(object->items[n].ident_array);
            ident_array_resize(a, read_u32(ctx->file), NULL);
            for (unsigned i = 0; i < a->count; i++)
               a->items[i] = ident_read(ctx->ident_ctx);
         }
         else if (ITEM_DOUBLE & mask) {
            union { uint64_t i; double d; } u;
            u.i = read_u64(ctx->file);
            object->items[n].dval = u.d;
         }
         else if (ITEM_ATTRS & mask) {
            attr_tab_t *attrs = &(object->items[n].attrs);

            attrs->num = read_u16(ctx->file);
            if (attrs->num > 0) {
               attrs->alloc = next_power_of_2(attrs->num);
               attrs->table = xmalloc(sizeof(attr_t) * attrs->alloc);
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
                  attrs->table[i].tval =
                     (tree_t)object_read(ctx, OBJECT_TAG_TREE);
                  break;

               default:
                  abort();
               }
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

   object_rd_ctx_t *ctx = xmalloc(sizeof(object_rd_ctx_t));
   ctx->file      = f;
   ctx->ident_ctx = ident_read_begin(f);
   ctx->store_sz  = 128;
   ctx->store     = xmalloc(ctx->store_sz * sizeof(tree_t));
   ctx->n_objects = 0;
   ctx->db_fname  = strdup(fname);
   memset(ctx->file_names, '\0', sizeof(ctx->file_names));

   return ctx;
}

void object_read_end(object_rd_ctx_t *ctx)
{
   ident_read_end(ctx->ident_ctx);
   free(ctx->store);
   free(ctx->db_fname);
   free(ctx);
}

fbuf_t *object_read_file(object_rd_ctx_t *ctx)
{
   return ctx->file;
}

object_t *object_read_recall(object_rd_ctx_t *ctx, uint32_t index)
{
   assert(index < ctx->n_objects);
   return ctx->store[index];
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

   int type_item = -1;
   bool marked = false;
   const imask_t has = class->has_map[object->kind];
   const int nitems = class->object_nitems[object->kind];
   imask_t mask = 1;
   for (int n = 0; n < nitems; mask <<= 1) {
      if (has & mask) {
         if (ITEM_IDENT & mask)
            ;
         else if (ITEM_TREE & mask)
            marked = object_copy_mark((object_t *)object->items[n].tree, ctx)
               || marked;
         else if (ITEM_DOUBLE & mask)
            ;
         else if (ITEM_TREE_ARRAY & mask) {
            tree_array_t *a = &(object->items[n].tree_array);
            for (unsigned i = 0; i < a->count; i++)
               marked = object_copy_mark((object_t *)a->items[i], ctx)
                  || marked;
         }
         else if (ITEM_TYPE_ARRAY & mask) {
            type_array_t *a = &(object->items[n].type_array);
            for (unsigned i = 0; i < a->count; i++)
               marked = object_copy_mark((object_t *)a->items[i], ctx)
                  || marked;
         }
         else if (ITEM_TYPE & mask)
            type_item = n;
         else if (ITEM_INT64 & mask)
            ;
         else if (ITEM_RANGE & mask) {
            range_t *r = object->items[n].range;
            if (r != NULL) {
               marked = object_copy_mark((object_t *)r->left, ctx) || marked;
               marked = object_copy_mark((object_t *)r->right, ctx) || marked;
            }
         }
         else if (ITEM_RANGE_ARRAY & mask) {
            range_array_t *a = &(object->items[n].range_array);
            for (unsigned i = 0; i < a->count; i++) {
               marked = object_copy_mark((object_t *)a->items[i].left, ctx)
                  || marked;
               marked = object_copy_mark((object_t *)a->items[i].right, ctx)
                  || marked;
            }
         }
         else if (ITEM_NETID_ARRAY & mask)
            ;
         else if (ITEM_ATTRS & mask)
            ;
         else if (ITEM_TEXT_BUF & mask)
            ;
         else if (ITEM_IDENT_ARRAY & mask)
            ;
         else
            item_without_type(mask);
         n++;
      }
   }

   if (!marked && (object->tag == OBJECT_TAG_TREE))
      marked = (*ctx->callback)((tree_t)object, ctx->context);

   if (marked)
      object->index = (ctx->index)++;

   // Check type last as it may contain a circular reference
   if (type_item != -1)
      marked = object_copy_mark((object_t *)object->items[type_item].type, ctx)
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
         else if (ITEM_TREE & mask)
            copy->items[n].tree = (tree_t)
               object_copy_sweep((object_t *)object->items[n].tree, ctx);
         else if (ITEM_DOUBLE & mask)
            copy->items[n].dval = object->items[n].dval;
         else if (ITEM_TREE_ARRAY & mask) {
            const tree_array_t *from = &(object->items[n].tree_array);
            tree_array_t *to = &(copy->items[n].tree_array);

            tree_array_resize(to, from->count, NULL);

            for (size_t i = 0; i < from->count; i++)
               to->items[i] = (tree_t)
                  object_copy_sweep((object_t *)from->items[i], ctx);
         }
         else if (ITEM_TYPE & mask)
            copy->items[n].type = (type_t)
               object_copy_sweep((object_t *)object->items[n].type, ctx);
         else if (ITEM_INT64 & mask)
            copy->items[n].ival = object->items[n].ival;
         else if (ITEM_RANGE & mask) {
            const range_t *from = object->items[n].range;
            if (from != NULL) {
               range_t *to = copy->items[n].range = xmalloc(sizeof(range_t));
               to->kind  = from->kind;
               to->left  =
                  (tree_t)object_copy_sweep((object_t *)from->left, ctx);
               to->right =
                  (tree_t)object_copy_sweep((object_t *)from->right, ctx);
            }
         }
         else if (ITEM_NETID_ARRAY & mask) {
            const netid_array_t *from = &(object->items[n].netid_array);
            netid_array_t *to = &(copy->items[n].netid_array);

            netid_array_resize(to, from->count, NETID_INVALID);

            for (unsigned i = 0; i < from->count; i++)
               to->items[i] = from->items[i];
         }
         else if (ITEM_IDENT_ARRAY & mask) {
            const ident_array_t *from = &(object->items[n].ident_array);
            ident_array_t *to = &(copy->items[n].ident_array);

            ident_array_resize(to, from->count, NULL);

            for (unsigned i = 0; i < from->count; i++)
               to->items[i] = from->items[i];
         }
         else if (ITEM_ATTRS & mask) {
            if ((copy->items[n].attrs.num = object->items[n].attrs.num) > 0) {
               copy->items[n].attrs.alloc = object->items[n].attrs.alloc;
               copy->items[n].attrs.table =
                  xmalloc(sizeof(attr_t) * copy->items[n].attrs.alloc);
               for (unsigned i = 0; i < object->items[n].attrs.num; i++)
                  copy->items[n].attrs.table[i] =
                     object->items[n].attrs.table[i];
            }
         }
         else if (ITEM_RANGE_ARRAY & mask) {
            const range_array_t *from = &(object->items[n].range_array);
            range_array_t *to = &(copy->items[n].range_array);

            range_t dummy;
            range_array_resize(to, from->count, dummy);

            for (unsigned i = 0; i < from->count; i++) {
               to->items[i].kind = from->items[i].kind;
               to->items[i].left = (tree_t)
                  object_copy_sweep((object_t *)from->items[i].left, ctx);
               to->items[i].right = (tree_t)
                  object_copy_sweep((object_t *)from->items[i].right, ctx);
            }
         }
         else if (ITEM_TYPE_ARRAY & mask) {
            const type_array_t *from = &(object->items[n].type_array);
            type_array_t *to = &(object->items[n].type_array);

            type_array_resize(to, from->count, NULL);

            for (unsigned i = 0; i < from->count; i++)
               to->items[i] = (type_t)
                  object_copy_sweep((object_t *)from->items[i], ctx);
         }
         else if (ITEM_TEXT_BUF & mask)
            ;
         else
            item_without_type(mask);
         n++;
      }
   }

   return copy;
}

void object_replace(object_t *t, object_t *a)
{
   const object_class_t *class = classes[t->tag];

   object_change_kind(class, t, a->kind);

   const imask_t has = class->has_map[t->kind];
   const int nitems = class->object_nitems[t->kind];
   imask_t mask = 1;
   for (int n = 0; n < nitems; mask <<= 1) {
      if (has & mask) {
         if (ITEM_TYPE_ARRAY & mask) {
            const type_array_t *from = &(a->items[n].type_array);
            type_array_t *to = &(t->items[n].type_array);

            type_array_resize(to, from->count, NULL);

            for (unsigned i = 0; i < from->count; i++)
               to->items[i] = from->items[i];
         }
         else if (ITEM_TYPE & mask)
            t->items[n].type = a->items[n].type;
         else if (ITEM_TREE & mask)
            t->items[n].tree = a->items[n].tree;
         else if (ITEM_TREE_ARRAY & mask) {
            const tree_array_t *from = &(a->items[n].tree_array);
            tree_array_t *to = &(t->items[n].tree_array);

            tree_array_resize(to, from->count, NULL);

            for (size_t i = 0; i < from->count; i++)
               to->items[i] = from->items[i];
         }
         else if (ITEM_RANGE_ARRAY & mask) {
            const range_array_t *from = &(a->items[n].range_array);
            range_array_t *to = &(t->items[n].range_array);

            range_t dummy;
            range_array_resize(to, from->count, dummy);

            for (unsigned i = 0; i < from->count; i++)
               to->items[i] = from->items[i];
         }
         else if (ITEM_TEXT_BUF & mask)
            ;
         else if (ITEM_IDENT & mask)
            t->items[n].ident = a->items[n].ident;
         else
            item_without_type(mask);
         n++;
      }
   }
}
