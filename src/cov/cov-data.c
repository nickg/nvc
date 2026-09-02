//
//  Copyright (C) 2013-2026  Nick Gasson
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
#include "cov/cov-api.h"
#include "cov/cov-data.h"
#include "cov/cov-priv.h"
#include "cov/cov-structs.h"
#include "hash.h"
#include "ident.h"
#include "lib.h"
#include "mask.h"
#include "option.h"
#include "printf.h"

#include <assert.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <limits.h>
#include <libgen.h>
#include <inttypes.h>

static const struct {
   const char *name;
   uint32_t   flag;
} bin_map[] = {
   { "BIN_TRUE",       COV_FLAG_TRUE},
   { "BIN_FALSE",      COV_FLAG_FALSE},
   { "BIN_CHOICE",     COV_FLAG_CHOICE},
   { "BIN_0_0",        COV_FLAG_00},
   { "BIN_0_1",        COV_FLAG_01},
   { "BIN_1_0",        COV_FLAG_10},
   { "BIN_1_1",        COV_FLAG_11},
   { "BIN_0_TO_1",     COV_FLAG_TOGGLE_TO_1},
   { "BIN_1_TO_0",     COV_FLAG_TOGGLE_TO_0},
};

#define COVER_FILE_MAGIC   0x6e636462   // ASCII "ncdb"
#define COVER_FILE_VERSION 9

static cover_bin_t *cover_get_bins(const cover_data_t *db,
                                   const cover_item_t *item)
{
   if (cover_is_null(item->first_bin))
      return NULL;

   assert(item->first_bin.tag == COVER_TAG_BIN);
   assert(item->first_bin.id < db->bins.count);

   return db->bins.items + item->first_bin.id;
}

static const cover_bin_t *cover_bin_data_const(const cover_data_t *db,
                                               cover_obj_t obj)
{
   assert(obj.tag == COVER_TAG_BIN);
   assert(obj.id < db->bins.count);
   return db->bins.items + obj.id;
}

static cover_bin_t *cover_bin_data(cover_data_t *db, cover_obj_t obj)
{
   assert(obj.tag == COVER_TAG_BIN);
   assert(obj.id < db->bins.count);
   return db->bins.items + obj.id;
}

static const cover_item_t *cover_item_data_const(const cover_data_t *db,
                                                 cover_obj_t obj)
{
   assert(obj.tag == COVER_TAG_ITEM);
   assert(obj.id < db->items.count);
   return db->items.items + obj.id;
}

static cover_item_t *cover_item_data(const cover_data_t *db, cover_obj_t obj)
{
   assert(obj.tag == COVER_TAG_ITEM);
   assert(obj.id < db->items.count);
   return db->items.items + obj.id;
}

static const cover_range_t *cover_range_data_const(const cover_data_t *db,
                                                   cover_obj_t obj)
{
   assert(obj.tag == COVER_TAG_RANGE);
   assert(obj.id < db->ranges.count);
   return db->ranges.items + obj.id;
}

static cover_range_t *cover_range_data(cover_data_t *db, cover_obj_t obj)
{
   assert(obj.tag == COVER_TAG_RANGE);
   assert(obj.id < db->ranges.count);
   return db->ranges.items + obj.id;
}

static const cover_scope_t *cover_scope_data_const(const cover_data_t *db,
                                                   cover_obj_t obj)
{
   assert(obj.tag == COVER_TAG_SCOPE);
   assert(obj.id < db->scopes.count);
   return db->scopes.items + obj.id;
}

static cover_scope_t *cover_scope_data(cover_data_t *db, cover_obj_t obj)
{
   assert(obj.tag == COVER_TAG_SCOPE);
   assert(obj.id < db->scopes.count);
   return db->scopes.items + obj.id;
}

static const cover_inst_t *cover_inst_data_const(const cover_data_t *db,
                                                 cover_obj_t obj)
{
   assert(obj.tag == COVER_TAG_INST);
   assert(obj.id < db->insts.count);
   return db->insts.items + obj.id;
}

static cover_inst_t *cover_inst_data(cover_data_t *db, cover_obj_t obj)
{
   assert(obj.tag == COVER_TAG_INST);
   assert(obj.id < db->insts.count);
   return db->insts.items + obj.id;
}

static cover_obj_t cover_alloc_bins(cover_data_t *db, unsigned count)
{
   if (db->bins.count + count > db->bins.limit) {
      const unsigned new_limit =
         MAX(16, MAX(db->bins.limit * 2, db->bins.count + count));
      db->bins.items = xrealloc_array(db->bins.items, new_limit,
                                      sizeof(cover_bin_t));
      db->bins.limit = new_limit;
   }

   cover_obj_t obj = {
      .tag = COVER_TAG_BIN,
      .id = db->bins.count,
   };

   db->bins.count += count;
   return obj;
}

void cover_add_ranges(cover_data_t *db, cover_obj_t obj, unsigned count)
{
   cover_bin_t *bin = cover_bin_data(db, obj);
   assert(cover_is_null(bin->first_range));

   if (count == 0)
      return;

   if (db->ranges.count + count > db->ranges.limit) {
      const unsigned new_limit =
         MAX(8, MAX(db->ranges.limit * 2, db->ranges.count + count));
      db->ranges.items = xrealloc_array(db->ranges.items, new_limit,
                                        sizeof(cover_range_t));
      db->ranges.limit = new_limit;
   }

   bin->first_range.tag = COVER_TAG_RANGE;
   bin->first_range.id = db->ranges.count;

   bin->n_ranges = count;

   db->ranges.count += count;
}

cover_obj_t cover_item_new(cover_data_t *db, cover_obj_t scope,
                           cover_item_kind_t kind, int nbins)
{
   cover_scope_t *sd = cover_scope_data(db, scope);

   if (!sd->emit)
      return COVER_NULL_OBJ;

   // Everything creates scope, so name of current item is already given
   // by scope in hierarchy.
   ident_t hier = sd->hier;

   // Expression items do not nest scope, expression name must be created
   if (kind == COV_ITEM_EXPRESSION) {
      char buf[16];
      checked_sprintf(buf, sizeof(buf), "_E%d", sd->expression_label);
      hier = ident_prefix(hier, ident_new(buf), '.');
      sd->expression_label++;
   }

   int64_t metadata = 0;
   if (kind == COV_ITEM_TOGGLE)
      metadata = sd->sig_pos;

   cover_item_t item = {
      .nbins     = nbins,
      .kind      = kind,
      .loc       = LOC_INVALID,
      .loc_lhs   = LOC_INVALID,
      .loc_rhs   = LOC_INVALID,
      .atleast   = db->threshold,
      .metadata  = metadata,
      .source    = COV_SRC_UNKNOWN,
      .first_bin = cover_alloc_bins(db, nbins),
   };

   cover_inst_t *id = NULL;
   if (!cover_is_null(sd->inst)) {
      id = cover_inst_data(db, sd->inst);
      assert(id->data == NULL);
   }

   cover_bin_t *bins = db->bins.items + item.first_bin.id;
   for (int i = 0; i < nbins; i++) {
      bins[i] = (cover_bin_t){
         .tag  = id != NULL ? id->next_tag++ : INT32_MAX,
         .hier = hier,
      };
   }

   cover_obj_t obj = {
      .tag = COVER_TAG_ITEM,
      .id = db->items.count,
   };

   APUSH(db->items, item);

   APUSH(sd->items, obj);
   return obj;
}

cover_obj_t cover_inst_new(cover_data_t *db, ident_t name)
{
   cover_inst_t new = {
      .name = name,
   };

   cover_obj_t obj = {
      .tag = COVER_TAG_INST,
      .id = db->insts.count,
   };

   assert(hash_get(db->inst_map, name) == NULL);
   hash_put(db->inst_map, name, (void *)(uintptr_t)obj.bits);

   APUSH(db->insts, new);
   return obj;
}

///////////////////////////////////////////////////////////////////////////////
// Coverage data write/read to covdb, covdb merging and coverage scope handling
///////////////////////////////////////////////////////////////////////////////

void cover_merge_bin(cover_data_t *db, cover_obj_t obj, int32_t data)
{
   cover_bin_t *bin = cover_bin_data(db, obj);

   const bool is_toggle_bin =
      !!(bin->flags & (COV_FLAG_TOGGLE_TO_0 | COV_FLAG_TOGGLE_TO_1));

   if (is_toggle_bin) {
      // Highest bit tracks unreachability#
      if ((bin->data & COV_FLAG_UNREACHABLE) || (data & COV_FLAG_UNREACHABLE))
         bin->data = COV_FLAG_UNREACHABLE;
      else
         bin->data = saturate_add(bin->data, data);
   }
   else
      bin->data = saturate_add(bin->data, data);
}

static void cover_update_counts(cover_data_t *db, cover_scope_t *s)
{
   if (!cover_is_null(s->inst)) {
      cover_inst_t *id = cover_inst_data(db, s->inst);
      if (id->data != NULL) {
         for (int i = 0; i < s->items.count; i++) {
            cover_obj_t item = s->items.items[i];

            cover_obj_t chunk[10];
            int pos = 0, nbins;
            do {
               nbins = cover_rel(db, item, COV_REL_BINS, pos, chunk,
                                 ARRAY_LEN(chunk));
               pos += ARRAY_LEN(chunk);

               for (int j = 0; j < MIN(nbins, ARRAY_LEN(chunk)); j++) {
                  uint32_t tag = cover_get_tag(db, chunk[j]);
                  cover_merge_bin(db, chunk[j], id->data[tag]);
               }
            } while (nbins > ARRAY_LEN(chunk));
         }
      }
   }

   for (int i = 0; i < s->children.count; i++)
      cover_update_counts(db, cover_scope_data(db, s->children.items[i]));
}

LCOV_EXCL_START
static void cover_debug_dump(const cover_data_t *db, cover_obj_t scope,
                             int indent)
{
   ident_t name = cover_get_ident(db, scope, COV_ATTR_NAME);
   ident_t block_name = cover_get_ident(db, scope, COV_ATTR_BLOCK_NAME);

   nvc_printf("%*s$!blue$%pi$$", indent, "", name);
   if (block_name != NULL)
      nvc_printf(" : %pi", block_name);
   nvc_printf("\n");

   const int nitems = cover_count(db, scope, COV_REL_ITEMS);

   for (int i = 0; i < nitems; i++) {
      cover_obj_t item = cover_at(db, scope, COV_REL_ITEMS, i);

      int nbins = cover_count(db, item, COV_REL_BINS);
      cover_item_kind_t kind = cover_get_kind(db, item);
      loc_t loc = cover_get_loc(db, item, COV_ATTR_LOC);

      for (int j = 0; j < nbins; j++) {
         cover_obj_t bin = cover_at(db, item, COV_REL_BINS, j);

         uint32_t tag = cover_get_u32(db, bin, COV_ATTR_TAG, -1);
         uint32_t data = cover_get_u32(db, bin, COV_ATTR_DATA, -1);
         ident_t hier = cover_get_ident(db, bin, COV_ATTR_HIER);

         if (loc_invalid_p(&loc))
            printf("%*s%d: %s %s <invalid> => %x\n", indent + 2, "",
                   tag, cover_item_kind_str(kind), istr(hier), data);
         else {
            const char *path = loc_file_str(&loc), *basename;
            if ((basename = strrchr(path, '/')))
               path = basename + 1;

            printf("%*s%d: %s %s %s:%d => %x\n", indent + 2, "", tag,
                   cover_item_kind_str(kind), istr(hier),
                   path, loc.first_line, data);
         }
      }
   }

   const int nchildren = cover_count(db, scope, COV_REL_CHILDREN);

   for (int i = 0; i < nchildren; i++) {
      cover_obj_t child = cover_at(db, scope, COV_REL_CHILDREN, i);
      cover_debug_dump(db, child, indent + 2);
   }
}
LCOV_EXCL_STOP

void cover_write(cover_data_t *db, fbuf_t *f, cover_dump_t dt)
{
   if (dt == COV_DUMP_RUNTIME)
      cover_update_counts(db, cover_scope_data(db, db->root_scope));

   if (opt_get_int(OPT_COVER_VERBOSE))
      cover_debug_dump(db, db->root_scope, 0);

   write_u32(COVER_FILE_MAGIC, f);
   fbuf_put_uint(f, COVER_FILE_VERSION);

   fbuf_put_uint(f, db->mask);
   fbuf_put_uint(f, db->array_limit);

   loc_wr_ctx_t *loc_wr = loc_write_begin(f);
   ident_wr_ctx_t ident_ctx = ident_write_begin(f);

   fbuf_put_uint(f, db->items.count);
   for (int i = 0; i < db->items.count; i++) {
      const cover_item_t *item = &(db->items.items[i]);

      fbuf_put_uint(f, item->nbins);
      fbuf_put_uint(f, item->kind);
      fbuf_put_uint(f, item->source);
      fbuf_put_uint(f, item->atleast);
      fbuf_put_uint(f, item->metadata);

      loc_write(&(item->loc), loc_wr);

      if (item->kind == COV_ITEM_EXPRESSION
          || item->kind == COV_ITEM_STATE
          || item->kind == COV_ITEM_FUNCTIONAL)
         ident_write(item->func_name, ident_ctx);

      if (item->kind == COV_ITEM_EXPRESSION) {
         loc_write(&(item->loc_lhs), loc_wr);
         loc_write(&(item->loc_rhs), loc_wr);
      }

      if (item->nbins > 0)
         fbuf_put_uint(f, item->first_bin.id);
   }

   fbuf_put_uint(f, db->ranges.count);
   for (int i = 0; i < db->ranges.count; i++) {
      fbuf_put_uint(f, db->ranges.items[i].min);
      fbuf_put_uint(f, db->ranges.items[i].max);
   }

   fbuf_put_uint(f, db->bins.count);
   for (int i = 0; i < db->bins.count; i++) {
      cover_bin_t *bin = &(db->bins.items[i]);

      fbuf_put_uint(f, bin->tag);
      fbuf_put_uint(f, bin->data);
      fbuf_put_uint(f, bin->flags);
      fbuf_put_uint(f, bin->field_idx);
      fbuf_put_uint(f, bin->n_ranges);

      if (bin->n_ranges > 0)
         fbuf_put_uint(f, bin->first_range.id);

      ident_write(bin->hier, ident_ctx);
   }

   fbuf_put_uint(f, db->insts.count);
   for (int i = 0; i < db->insts.count; i++) {
      const cover_inst_t *inst = &(db->insts.items[i]);

      ident_write(inst->name, ident_ctx);
      fbuf_put_uint(f, inst->next_tag);
      fbuf_put_uint(f, inst->root.bits);
   }

   fbuf_put_uint(f, db->scopes.count);
   for (int i = 0; i < db->scopes.count; i++) {
      const cover_scope_t *s = &(db->scopes.items[i]);

      ident_write(s->name, ident_ctx);
      ident_write(s->hier, ident_ctx);
      ident_write(s->block_name, ident_ctx);
      fbuf_put_uint(f, s->kind);
      loc_write(&s->loc, loc_wr);

      fbuf_put_uint(f, s->parent.bits);
      fbuf_put_uint(f, s->inst.bits);

      fbuf_put_uint(f, s->items.count);
      for (int i = 0; i < s->items.count; i++) {
         assert(s->items.items[i].tag == COVER_TAG_ITEM);
         fbuf_put_uint(f, s->items.items[i].id);
      }

      fbuf_put_uint(f, s->children.count);
      for (int i = 0; i < s->children.count; i++) {
         assert(s->children.items[i].tag == COVER_TAG_SCOPE);
         fbuf_put_uint(f, s->children.items[i].id);
      }
   }

   fbuf_put_uint(f, db->root_scope.bits);

   loc_write_end(loc_wr);
   ident_write_end(ident_ctx);
}

cover_data_t *cover_data_init(cover_mask_t mask, int array_limit, int threshold)
{
   cover_data_t *data = xcalloc(sizeof(cover_data_t));
   data->mask        = mask;
   data->array_limit = array_limit;
   data->threshold   = threshold;
   data->inst_map    = hash_new(16);
   data->pool        = pool_new();

   return data;
}

void cover_data_free(cover_data_t *db)
{
#ifdef DEBUG
   size_t alloc, npages;
   pool_stats(db->pool, &alloc, &npages);
   if (npages > 0)
      debugf("coverage database allocated %zu bytes in %zu pages",
             alloc, npages);
#endif

   free(db->items.items);
   free(db->bins.items);
   free(db->ranges.items);
   free(db->scopes.items);
   free(db->insts.items);
   hash_free(db->inst_map);
   pool_free(db->pool);
   free(db);
}

bool cover_enabled(cover_data_t *data, cover_mask_t mask)
{
   return data != NULL && (data->mask & mask);
}

cover_obj_t cover_create_block(cover_data_t *db, ident_t qual,
                               cover_obj_t parent, tree_t inst, tree_t unit)
{
   if (db == NULL)
      return COVER_NULL_OBJ;

   if (cover_is_null(parent)) {
      assert(cover_is_null(db->root_scope));

      cover_scope_t root = {
         .loc = LOC_INVALID,
         .name = lib_name(lib_work()),
         .hier = lib_name(lib_work()),
      };

      db->root_scope.tag = COVER_TAG_SCOPE;
      db->root_scope.id = 0;

      APUSH(db->scopes, root);

      parent = db->root_scope;
   }

   cover_obj_t root = cover_create_scope(db, parent, inst, tree_ident(inst));

   cover_obj_t obj = cover_inst_new(db, qual);
   cover_put_obj(db, obj, COV_ATTR_ROOT, root);

   cover_scope_t *sd = cover_scope_data(db, root);
   sd->inst = obj;
   sd->block_name = ident_rfrom(tree_ident(unit), '.');
   sd->emit = cover_should_emit_scope(db, root);

   return root;
}

cover_obj_t cover_create_user_scope(cover_data_t *db, cover_obj_t parent,
                                    loc_t loc, ident_t name)
{
   if (db == NULL)
      return COVER_NULL_OBJ;

   assert(!cover_is_null(parent));

   cover_scope_t *pd = cover_scope_data(db, parent);

   cover_scope_t new = {};
   new.name   = name;
   new.parent = parent;
   new.loc    = loc;
   new.hier   = ident_prefix(pd->hier, name, '.');
   new.kind   = CSCOPE_USER;

   cover_obj_t obj = {
      .tag = COVER_TAG_SCOPE,
      .id = db->scopes.count,
   };

   APUSH(pd->children, obj);
   APUSH(db->scopes, new);

   cover_scope_t *sd = cover_scope_data(db, obj);
   sd->emit = cover_should_emit_scope(db, obj);

   return obj;
}

cover_obj_t cover_create_scope(cover_data_t *db, cover_obj_t parent,
                               tree_t t, ident_t name)
{
   if (db == NULL)
      return COVER_NULL_OBJ;

   assert(!cover_is_null(parent));
   assert(!cover_is_null(db->root_scope));
   assert(!is_design_unit(t));

   cover_scope_t *pd = cover_scope_data(db, parent);

   cover_scope_t new = {};

   switch (tree_kind(t)) {
   case T_BLOCK:
      new.kind = CSCOPE_INSTANCE;
      break;
   case T_PROCESS:
   case T_INERTIAL:
      new.kind = CSCOPE_PROCESS;
      break;
   case T_PROC_BODY:
   case T_FUNC_BODY:
      new.kind = CSCOPE_SUBPROG;
      break;
   case T_PSL_DIRECT:
      new.kind = CSCOPE_PROPERTY;
      break;
   case T_PACK_INST:
   case T_PACKAGE:
   case T_PACK_BODY:
      new.kind = CSCOPE_PACKAGE;
      break;
   case T_SIGNAL_DECL:
   case T_PORT_DECL:
      // For toggle coverage, remember the position where its name in
      // the hierarchy starts.
      new.sig_pos = ident_len(pd->hier) + 1;
      break;
   default:
      break;
   }

   new.parent = parent;
   new.inst   = pd->inst;
   new.name   = name;
   new.loc    = *tree_loc(t);
   new.hier   = ident_prefix(pd->hier, new.name, '.');

   if (new.sig_pos == 0)
      new.sig_pos = pd->sig_pos;

   cover_obj_t obj = {
      .tag = COVER_TAG_SCOPE,
      .id = db->scopes.count,
   };

   APUSH(pd->children, obj);
   APUSH(db->scopes, new);

   cover_scope_t *sd = cover_scope_data(db, obj);
   sd->emit = cover_should_emit_scope(db, obj);

   return obj;
}

static void cover_read_header(fbuf_t *f, cover_data_t *data)
{
   assert(data != NULL);

   if (read_u32(f) != COVER_FILE_MAGIC)
      fatal("%s is not a valid coverage database", fbuf_file_name(f));

   const unsigned version = fbuf_get_uint(f);
   if (version != COVER_FILE_VERSION)
      fatal("coverage database %s format version %d is not the expected %d",
            fbuf_file_name(f), version, COVER_FILE_VERSION);

   data->mask        = fbuf_get_uint(f);
   data->array_limit = fbuf_get_uint(f);
}

cover_data_t *cover_read(fbuf_t *f, uint32_t pre_mask)
{
   cover_data_t *db = xcalloc(sizeof(cover_data_t));
   cover_read_header(f, db);
   db->mask |= pre_mask;
   db->inst_map = hash_new(16);
   db->pool = pool_new();

   loc_rd_ctx_t *loc_rd = loc_read_begin(f);
   ident_rd_ctx_t ident_ctx = ident_read_begin(f);

   db->items.count = db->items.limit = fbuf_get_uint(f);
   db->items.items = xmalloc_array(db->items.count, sizeof(cover_item_t));

   for (int i = 0; i < db->items.count; i++) {
      cover_item_t *item = &(db->items.items[i]);

      item->nbins = fbuf_get_uint(f);
      item->kind = fbuf_get_uint(f);
      item->source = fbuf_get_uint(f);
      item->atleast = fbuf_get_uint(f);
      item->metadata = fbuf_get_uint(f);

      loc_read(&item->loc, loc_rd);

      if (item->kind == COV_ITEM_EXPRESSION || item->kind == COV_ITEM_STATE
          || item->kind == COV_ITEM_FUNCTIONAL)
         item->func_name = ident_read(ident_ctx);

      if (item->kind == COV_ITEM_EXPRESSION) {
         loc_read(&item->loc_lhs, loc_rd);
         loc_read(&item->loc_rhs, loc_rd);
      }
      else {
         item->loc_lhs = LOC_INVALID;
         item->loc_rhs = LOC_INVALID;
      }

      if (item->nbins > 0) {
         item->first_bin.tag = COVER_TAG_BIN;
         item->first_bin.id = fbuf_get_uint(f);
      }
   }

   db->ranges.count = db->ranges.limit = fbuf_get_uint(f);
   db->ranges.items = xmalloc_array(db->ranges.count, sizeof(cover_range_t));

   for (int i = 0; i < db->ranges.count; i++) {
      db->ranges.items[i].min = fbuf_get_uint(f);
      db->ranges.items[i].max = fbuf_get_uint(f);
   }

   db->bins.count = db->bins.limit = fbuf_get_uint(f);
   db->bins.items = xmalloc_array(db->bins.count, sizeof(cover_bin_t));

   for (int i = 0; i < db->bins.count; i++) {
      cover_bin_t *bin = &(db->bins.items[i]);

      bin->tag = fbuf_get_uint(f);
      bin->data = fbuf_get_uint(f);
      bin->flags = fbuf_get_uint(f);
      bin->field_idx = fbuf_get_uint(f);
      bin->n_ranges = fbuf_get_uint(f);

      if (bin->n_ranges > 0) {
         bin->first_range.tag = COVER_TAG_RANGE;
         bin->first_range.id = fbuf_get_uint(f);
      }

      bin->hier = ident_read(ident_ctx);
   }

   db->insts.count = db->insts.limit = fbuf_get_uint(f);
   db->insts.items = xcalloc_array(db->insts.count, sizeof(cover_inst_t));

   for (int i = 0; i < db->insts.count; i++) {
      cover_inst_t *inst = &(db->insts.items[i]);

      inst->name = ident_read(ident_ctx);
      inst->next_tag = fbuf_get_uint(f);
      inst->root.bits = fbuf_get_uint(f);

      cover_obj_t obj = { .tag = COVER_TAG_INST, .id = i };
      assert(hash_get(db->inst_map, inst->name) == NULL);
      hash_put(db->inst_map, inst->name, (void *)(uintptr_t)obj.bits);
   }

   db->scopes.count = db->scopes.limit = fbuf_get_uint(f);
   db->scopes.items = xcalloc_array(db->scopes.count, sizeof(cover_scope_t));

   for (int i = 0; i < db->scopes.count; i++) {
      cover_scope_t *s = &(db->scopes.items[i]);

      s->name = ident_read(ident_ctx);
      s->hier = ident_read(ident_ctx);
      s->block_name = ident_read(ident_ctx);
      s->kind = fbuf_get_uint(f);
      loc_read(&s->loc, loc_rd);

      s->parent.bits = fbuf_get_uint(f);
      s->inst.bits = fbuf_get_uint(f);

      s->items.count = s->items.limit = fbuf_get_uint(f);
      s->items.items = xmalloc_array(s->items.count, sizeof(cover_obj_t));

      for (int j = 0; j < s->items.count; j++) {
         cover_obj_t obj = { .tag = COVER_TAG_ITEM };
         obj.id = fbuf_get_uint(f);
         assert(obj.id < db->items.count);
         s->items.items[j] = obj;
      }

      s->children.count = s->children.limit = fbuf_get_uint(f);
      s->children.items = xmalloc_array(s->children.count, sizeof(cover_obj_t));

      for (int j = 0; j < s->children.count; j++) {
         cover_obj_t obj = { .tag = COVER_TAG_SCOPE };
         obj.id = fbuf_get_uint(f);
         assert(obj.id < db->scopes.count);
         s->children.items[j] = obj;
      }
   }

   db->root_scope.bits = fbuf_get_uint(f);

   ident_read_end(ident_ctx);
   loc_read_end(loc_rd);

   return db;
}

static bool cover_merge_items(cover_data_t *dst_db, const cover_data_t *src_db,
                              cover_obj_t dst, cover_obj_t src)
{
   if (cover_get_kind(dst_db, dst) != cover_get_kind(src_db, src))
      return false;

   int src_nbins = cover_count(src_db, src, COV_REL_BINS);
   int dst_nbins = cover_count(dst_db, dst, COV_REL_BINS);

   LOCAL_BIT_MASK missed;
   mask_init(&missed, src_nbins);
   mask_setall(&missed);

   for (int i = 0; i < src_nbins; i++) {
      cover_obj_t sbin = cover_at(src_db, src, COV_REL_BINS, i);
      cover_flags_t src_flags = cover_get_flags(src_db, sbin);
      ident_t src_hier = cover_get_ident(src_db, sbin, COV_ATTR_HIER);

      // Try the same index first assuming the scopes are identical
      cover_obj_t dbin_i = cover_at(dst_db, dst, COV_REL_BINS, i);
      if (!cover_is_null(dbin_i)) {
         cover_flags_t dst_flags = cover_get_flags(dst_db, dbin_i);
         ident_t dst_hier = cover_get_ident(dst_db, dbin_i, COV_ATTR_HIER);

         if (dst_flags == src_flags && dst_hier == src_hier) {
            uint32_t src_data = cover_get_u32(src_db, sbin, COV_ATTR_DATA, 0);
            cover_merge_bin(dst_db, dbin_i, src_data);
            mask_clear(&missed, i);
            continue;
         }
      }

      for (int j = 0; j < dst_nbins; j++) {
         if (i == j)
            continue;   // Checked above

         cover_obj_t dbin = cover_at(dst_db, dst, COV_REL_BINS, j);
         cover_flags_t dst_flags = cover_get_flags(dst_db, dbin);
         ident_t dst_hier = cover_get_ident(dst_db, dbin, COV_ATTR_HIER);

         if (dst_flags == src_flags && dst_hier == src_hier) {
            uint32_t src_data = cover_get_u32(src_db, sbin, COV_ATTR_DATA, 0);
            cover_merge_bin(dst_db, dbin, src_data);
            mask_clear(&missed, i);
            break;
         }
      }
   }

   const int nmissed = mask_popcount(&missed);

   if (nmissed == 0)
      return true;    // Merged all items
   else if (nmissed == src_nbins)
      return false;   // Unrelated

   // Append the unmerged items to the destination array

   const int new_count = dst_nbins + nmissed;
   cover_obj_t new_first = cover_alloc_bins(dst_db, new_count);

   cover_item_t *dst_data = cover_item_data(dst_db, dst);

   // Allocation may have moved the table
   cover_bin_t *sbins = cover_get_bins(src_db, cover_item_data(src_db, src));
   cover_bin_t *dbins = cover_get_bins(dst_db, dst_data);

   cover_bin_t *nbins = dst_db->bins.items + new_first.id;
   memcpy(nbins, dbins, dst_nbins * sizeof(cover_bin_t));

   cover_bin_t *ptr = nbins + dst_nbins;
   for (size_t i = -1; mask_iter(&missed, &i);)
      *ptr++ = sbins[i];
   assert(ptr == nbins + new_count);

   dst_data->first_bin = new_first;
   dst_data->nbins = new_count;
   return true;
}

static cover_obj_t cover_clone_item(cover_data_t *dst_db,
                                    const cover_data_t *src_db,
                                    cover_obj_t src)
{
   const cover_item_t *src_data = cover_item_data_const(src_db, src);
   cover_item_t copy = *src_data;

   copy.first_bin = cover_alloc_bins(dst_db, src_data->nbins);

   cover_bin_t *dst_bins = cover_get_bins(dst_db, &copy);
   cover_bin_t *src_bins = cover_get_bins(src_db, src_data);
   memcpy(dst_bins, src_bins, src_data->nbins * sizeof(cover_bin_t));

   cover_obj_t item_obj = {
      .tag = COVER_TAG_ITEM,
      .id = dst_db->items.count,
   };

   APUSH(dst_db->items, copy);

   return item_obj;
}

static cover_obj_t cover_clone_scope(cover_data_t *dst_db,
                                     const cover_data_t *src_db,
                                     cover_obj_t src_scope,
                                     cover_obj_t parent_scope,
                                     cover_obj_t parent_inst)
{
   cover_obj_t obj = {
      .tag = COVER_TAG_SCOPE,
      .id = dst_db->scopes.count,
   };

   APUSH(dst_db->scopes, *cover_scope_data_const(src_db, src_scope));

   cover_scope_t *copy = cover_scope_data(dst_db, obj);
   const cover_scope_t *src = cover_scope_data_const(src_db, src_scope);

   copy->parent = parent_scope;
   copy->items = (cover_array_t)AINIT;
   copy->children = (cover_array_t)AINIT;

   cover_obj_t dst_inst = parent_inst;
   cover_obj_t src_inst = cover_get_obj(src_db, src_scope, COV_ATTR_INST);
   cover_obj_t src_root = cover_get_obj(src_db, src_inst, COV_ATTR_ROOT);
   if (cover_equals(src_scope, src_root)) {
      ident_t name = cover_get_ident(src_db, src_inst, COV_ATTR_NAME);
      dst_inst = cover_inst_new(dst_db, name);
      cover_put_obj(dst_db, dst_inst, COV_ATTR_ROOT, obj);
   }
   copy->inst = dst_inst;

   for (int i = 0; i < src->items.count; i++)
      APUSH(copy->items, cover_clone_item(dst_db, src_db, src->items.items[i]));

   for (int i = 0; i < src->children.count; i++) {
      cover_obj_t child = cover_clone_scope(dst_db, src_db,
                                            src->children.items[i], obj,
                                            dst_inst);
      copy = cover_scope_data(dst_db, obj);  // May be invalidated
      APUSH(copy->children, child);
   }

   return obj;
}

static void cover_merge_scope(cover_data_t *dst_db,
                              const cover_data_t *src_db,
                              cover_obj_t dst_scope,
                              cover_obj_t src_scope,
                              merge_mode_t mode)
{
   const cover_scope_t *src_s = cover_scope_data_const(src_db, src_scope);
   cover_scope_t *dst_s = cover_scope_data(dst_db, dst_scope);

   cover_obj_t parent_inst = cover_get_obj(dst_db, dst_scope, COV_ATTR_INST);

   for (int i = 0; i < src_s->items.count; i++) {
      cover_obj_t src = AGET(src_s->items, i);

      // Try the same index first assuming the scopes are identical
      if (i < dst_s->items.count) {
         cover_obj_t dst = AGET(dst_s->items, i);
         if (cover_merge_items(dst_db, src_db, dst, src))
            continue;
      }

      bool merged = false;
      for (int j = 0; j < dst_s->items.count; j++) {
         if (i == j)
            continue;
         else {
            cover_obj_t dst = AGET(dst_s->items, j);
            if ((merged = cover_merge_items(dst_db, src_db, dst, src)))
               break;
         }
      }

      if (!merged) {
         // TOOD: if mode == MERGE_UNION add to dst_s->items?
      }
   }

   const int src_nchildren = cover_count(src_db, src_scope, COV_REL_CHILDREN);
   const int dst_nchildren = cover_count(src_db, dst_scope, COV_REL_CHILDREN);

   for (int i = 0; i < src_nchildren; i++) {
      cover_obj_t src_c = cover_at(src_db, src_scope, COV_REL_CHILDREN, i);
      ident_t src_name = cover_get_ident(src_db, src_c, COV_ATTR_NAME);

      bool found = false;
      for (int j = 0; j < dst_nchildren; j++) {
         cover_obj_t dst_c = cover_at(dst_db, dst_scope, COV_REL_CHILDREN, j);
         ident_t dst_name = cover_get_ident(dst_db, dst_c, COV_ATTR_NAME);

         if (dst_name == src_name) {
            cover_merge_scope(dst_db, src_db, dst_c, src_c, mode);
            found = true;
            break;
         }
      }

      if (!found && mode == MERGE_UNION) {
         cover_obj_t copy = cover_clone_scope(dst_db, src_db, src_c, dst_scope,
                                              parent_inst);
         cover_append(dst_db, dst_scope, COV_REL_CHILDREN, copy);
      }
   }
}

void cover_merge(cover_data_t *dst, const cover_data_t *src, merge_mode_t mode)
{
   cover_merge_scope(dst, src, dst->root_scope, src->root_scope, mode);

   if (opt_get_int(OPT_COVER_VERBOSE))
      cover_debug_dump(dst, dst->root_scope, 0);
}

int32_t *cover_get_counters(cover_data_t *db, ident_t name)
{
   if (db == NULL)
      return NULL;

   cover_obj_t inst = { .bits = (uintptr_t)hash_get(db->inst_map, name) };
   if (cover_is_null(inst))
      return NULL;

   cover_inst_t *id = cover_inst_data(db, inst);
   if (id->next_tag == 0)
      return NULL;

   if (id->data == NULL)
      id->data = pool_calloc(db->pool, id->next_tag * sizeof(int32_t));

   return id->data;
}

cover_obj_t cover_get_scope(cover_data_t *db, ident_t name)
{
   if (db == NULL)
      return COVER_NULL_OBJ;

   cover_obj_t inst = { .bits = (uintptr_t)hash_get(db->inst_map, name) };
   if (cover_is_null(inst))
      return COVER_NULL_OBJ;

   return cover_get_obj(db, inst, COV_ATTR_ROOT);
}

cover_obj_t cover_get_child(const cover_data_t *db, cover_obj_t scope,
                            ident_t name)
{
   if (cover_is_null(scope))
      return COVER_NULL_OBJ;

   const cover_scope_t *sd = cover_scope_data_const(db, scope);

   for (int i = 0; i < sd->children.count; i++) {
      if (cover_get_ident(db, sd->children.items[i], COV_ATTR_NAME) == name)
         return sd->children.items[i];
   }

   return COVER_NULL_OBJ;
}

cover_obj_t cover_get_item(const cover_data_t *db, cover_obj_t scope,
                           cover_item_kind_t kind, int nth)
{
   if (cover_is_null(scope))
      return COVER_NULL_OBJ;

   const cover_scope_t *sd = cover_scope_data_const(db, scope);

   for (int i = 0; i < sd->items.count; i++) {
      cover_item_t *item = &(db->items.items[sd->items.items[i].id]);
      if (item->kind == kind && nth-- == 0)
         return sd->items.items[i];
   }

   return COVER_NULL_OBJ;
}

const char *cover_bmask_to_bin_str(uint32_t bmask)
{
   // TODO: Smarter way instead of iterating -> Probably OK for such small array
   //       even if called many times!
   for (int i = 0; i < ARRAY_LEN(bin_map); i++)
      if (bmask & bin_map[i].flag)
         return bin_map[i].name;

   should_not_reach_here();
}

const char *cover_item_kind_str(cover_item_kind_t kind)
{
   static const char* item_kind_str[] = {
      "statement",
      "branch",
      "toggle",
      "expression",
      "FSM state",
      "cover point",
   };
   assert(kind < ARRAY_LEN(item_kind_str));
   return item_kind_str[kind];
}

bool cover_is_hier(const cover_data_t *db, cover_obj_t scope)
{
   switch (cover_scope_data_const(db, scope)->kind) {
   case CSCOPE_INSTANCE:
   case CSCOPE_PACKAGE:
      return true;
   default:
      return false;
   }
}

bool cover_is_leaf(const cover_data_t *db, cover_obj_t scope)
{
   const cover_scope_t *s = cover_scope_data_const(db, scope);

   for (int i = 0; i < s->children.count; i++) {
      if (cover_is_hier(db, s->children.items[i]))
         return false;
   }

   return true;
}

size_t cover_count(const cover_data_t *db, cover_obj_t obj, cover_rel_t rel)
{
   return cover_rel(db, obj, rel, 0, NULL, 0);
}

static size_t cover_rel_contig(unsigned rel_count, cover_obj_t rel_first,
                               unsigned first, cover_obj_t *out, size_t max)
{
   if (first >= rel_count)
      return 0;

   const size_t count = rel_count - first;

   for (int i = 0; i < MIN(count, max); i++) {
      out[i].tag = rel_first.tag;
      out[i].id  = rel_first.id + first + i;
   }

   return count;
}

static size_t cover_rel_array(const cover_array_t *arr, unsigned first,
                              cover_obj_t *out, size_t max)
{
   if (first >= arr->count)
      return 0;

   const size_t count = arr->count - first;

   memcpy(out, arr->items + first, MIN(count, max) * sizeof(cover_obj_t));
   return count;
}

size_t cover_rel(const cover_data_t *db, cover_obj_t obj, cover_rel_t rel,
                 unsigned first, cover_obj_t *out, size_t max)
{
   switch (obj.tag) {
   case COVER_TAG_ITEM:
      {
         const cover_item_t *item = cover_item_data_const(db, obj);
         switch (rel) {
         case COV_REL_BINS:
            return cover_rel_contig(item->nbins, item->first_bin,
                                    first, out, max);
         default:
            return 0;
         }
      }
   case COVER_TAG_BIN:
      {
         const cover_bin_t *bin = cover_bin_data_const(db, obj);
         switch (rel) {
         case COV_REL_RANGES:
            return cover_rel_contig(bin->n_ranges, bin->first_range,
                                    first, out, max);
         default:
            return 0;
         }
      }
   case COVER_TAG_SCOPE:
      {
         const cover_scope_t *scope = cover_scope_data_const(db, obj);
         switch (rel) {
         case COV_REL_ITEMS:
            return cover_rel_array(&scope->items, first, out, max);
         case COV_REL_CHILDREN:
            return cover_rel_array(&scope->children, first, out, max);
         default:
            return 0;
         }
      }
   default:
      return 0;
   }
}

cover_obj_t cover_at(const cover_data_t *db, cover_obj_t obj, cover_rel_t rel,
                     unsigned index)
{
   cover_obj_t result[1];
   if (cover_rel(db, obj, rel, index, result, 1) == 0)
      return COVER_NULL_OBJ;
   else
      return result[0];
}

void cover_map(cover_data_t *db, cover_obj_t obj, cover_rel_t rel,
               cover_map_fn_t fn, void *ctx)
{
   cover_obj_t batch[32];
   int pos = 0, total;
   do {
      total = cover_rel(db, obj, rel, pos, batch, ARRAY_LEN(batch));
      pos += ARRAY_LEN(batch);

      for (int j = 0; j < MIN(total, ARRAY_LEN(batch)); j++)
         (*fn)(db, batch[j], ctx);
   } while (total > ARRAY_LEN(batch));
}

void cover_append(cover_data_t *db, cover_obj_t parent, cover_rel_t rel,
                  cover_obj_t obj)
{
   switch (parent.tag) {
   case COVER_TAG_SCOPE:
      {
         cover_scope_t *s = cover_scope_data(db, parent);
         switch (rel) {
         case COV_REL_CHILDREN:
            assert(obj.tag == COVER_TAG_SCOPE);
            APUSH(s->children, obj);
            return;
         default:
            should_not_reach_here();
         }
      }
   default:
      should_not_reach_here();
   }
}

uint32_t cover_get_u32(const cover_data_t *db, cover_obj_t obj,
                       cover_attr_t attr, uint32_t def)
{
   switch (obj.tag) {
   case COVER_TAG_ITEM:
      {
         const cover_item_t *item = cover_item_data_const(db, obj);
         switch (attr) {
         case COV_ATTR_KIND:    return item->kind;
         case COV_ATTR_SOURCE:  return item->source;
         case COV_ATTR_ATLEAST: return item->atleast;
         default:               return def;
         }
      }
   case COVER_TAG_BIN:
      {
         const cover_bin_t *bin = cover_bin_data_const(db, obj);
         switch (attr) {
         case COV_ATTR_TAG:       return bin->tag;
         case COV_ATTR_FLAGS:     return bin->flags;
         case COV_ATTR_FIELD_IDX: return bin->field_idx;
         case COV_ATTR_DATA:      return bin->data;
         default:                 return def;
         }
      }
   default:
      return def;
   }
}

int64_t cover_get_i64(const cover_data_t *db, cover_obj_t obj,
                      cover_attr_t attr, int64_t def)
{
   switch (obj.tag) {
   case COVER_TAG_RANGE:
      {
         const cover_range_t *r = cover_range_data_const(db, obj);
         switch (attr) {
         case COV_ATTR_MAX: return r->max;
         case COV_ATTR_MIN: return r->min;
         default:           return def;
         }
      }
   case COVER_TAG_ITEM:
      {
         const cover_item_t *item = cover_item_data_const(db, obj);
         switch (attr) {
         case COV_ATTR_METADATA: return item->metadata;
         default:                return def;
         }
      }
   default:
      return def;
   }
}

ident_t cover_get_ident(const cover_data_t *db, cover_obj_t obj,
                        cover_attr_t attr)
{
   switch (obj.tag) {
   case COVER_TAG_BIN:
      {
         const cover_bin_t *bin = cover_bin_data_const(db, obj);
         switch (attr) {
         case COV_ATTR_HIER: return bin->hier;
         default:            return NULL;
         }
      }
   case COVER_TAG_ITEM:
      {
         const cover_item_t *item = cover_item_data_const(db, obj);
         switch (attr) {
         case COV_ATTR_FUNC_NAME: return item->func_name;
         default:                 return NULL;
         }
      }
   case COVER_TAG_SCOPE:
      {
         const cover_scope_t *scope = cover_scope_data_const(db, obj);
         switch (attr) {
         case COV_ATTR_NAME:       return scope->name;
         case COV_ATTR_HIER:       return scope->hier;
         case COV_ATTR_BLOCK_NAME: return scope->block_name;
         default:                  return NULL;
         }
      }
   case COVER_TAG_INST:
      {
         const cover_inst_t *inst = cover_inst_data_const(db, obj);
         switch (attr) {
         case COV_ATTR_NAME: return inst->name;
         default:            return NULL;
         }
      }
   default:
      return NULL;
   }
}

loc_t cover_get_loc(const cover_data_t *db, cover_obj_t obj, cover_attr_t attr)
{
   switch (obj.tag) {
   case COVER_TAG_ITEM:
      {
         const cover_item_t *item = cover_item_data_const(db, obj);
         switch (attr) {
         case COV_ATTR_LOC:     return item->loc;
         case COV_ATTR_LHS_LOC: return item->loc_lhs;
         case COV_ATTR_RHS_LOC: return item->loc_rhs;
         default:               return LOC_INVALID;
         }
      }
   case COVER_TAG_SCOPE:
      {
         const cover_scope_t *scope = cover_scope_data_const(db, obj);
         switch (attr) {
         case COV_ATTR_LOC: return scope->loc;
         default:           return LOC_INVALID;
         }
      }
   default:
      return LOC_INVALID;
   }
}

cover_obj_t cover_get_obj(const cover_data_t *db, cover_obj_t obj,
                          cover_attr_t attr)
{
   switch (obj.tag) {
   case COVER_TAG_SCOPE:
      {
         const cover_scope_t *scope = cover_scope_data_const(db, obj);
         switch (attr) {
         case COV_ATTR_PARENT: return scope->parent;
         case COV_ATTR_INST:   return scope->inst;
         default:              return COVER_NULL_OBJ;
         }
      }
   case COVER_TAG_INST:
      {
         const cover_inst_t *inst = cover_inst_data_const(db, obj);
         switch (attr) {
         case COV_ATTR_ROOT: return inst->root;
         default:            return COVER_NULL_OBJ;
         }
      }
   default:
      return COVER_NULL_OBJ;
   }
}

cover_flags_t cover_get_flags(const cover_data_t *db, cover_obj_t obj)
{
   return cover_get_u32(db, obj, COV_ATTR_FLAGS, 0);
}

uint32_t cover_get_tag(const cover_data_t *db, cover_obj_t obj)
{
   return cover_get_u32(db, obj, COV_ATTR_TAG, -1);
}

unsigned cover_get_kind(const cover_data_t *db, cover_obj_t obj)
{
   return cover_get_u32(db, obj, COV_ATTR_KIND, -1);
}

unsigned cover_get_threshold(const cover_data_t *db, cover_obj_t obj)
{
   return cover_get_u32(db, obj, COV_ATTR_ATLEAST, db->threshold);
}

void cover_put_u32(cover_data_t *db, cover_obj_t obj, cover_attr_t attr,
                   uint32_t value)
{
   switch (obj.tag) {
   case COVER_TAG_BIN:
      {
         cover_bin_t *bin = cover_bin_data(db, obj);
         switch (attr) {
         case COV_ATTR_FLAGS:
            bin->flags = value;
            return;
         case COV_ATTR_FIELD_IDX:
            bin->field_idx = value;
            return;
         default:
            should_not_reach_here();
         }
      }
   case COVER_TAG_ITEM:
      {
         cover_item_t *item = cover_item_data(db, obj);
         switch (attr) {
         case COV_ATTR_SOURCE:
            item->source = value;
            return;
         case COV_ATTR_ATLEAST:
            item->atleast = value;
            return;
         default:
            should_not_reach_here();
         }
      }
   default:
      should_not_reach_here();
   }
}

void cover_put_i64(cover_data_t *db, cover_obj_t obj, cover_attr_t attr,
                   int64_t value)
{
   switch (obj.tag) {
   case COVER_TAG_RANGE:
      {
         cover_range_t *r = cover_range_data(db, obj);
         switch (attr) {
         case COV_ATTR_MAX:
            r->max = value;
            return;
         case COV_ATTR_MIN:
            r->min = value;
            return;
         default:
            should_not_reach_here();
         }
      }
   case COVER_TAG_ITEM:
      {
         cover_item_t *item = cover_item_data(db, obj);
         switch (attr) {
         case COV_ATTR_METADATA:
            item->metadata = value;
            return;
         default:
            should_not_reach_here();
         }
      }
   default:
      should_not_reach_here();
   }
}

void cover_put_ident(cover_data_t *db, cover_obj_t obj, cover_attr_t attr,
                     ident_t value)
{
   switch (obj.tag) {
   case COVER_TAG_BIN:
      {
         cover_bin_t *bin = cover_bin_data(db, obj);
         switch (attr) {
         case COV_ATTR_HIER:
            bin->hier = value;
            return;
         default:
            should_not_reach_here();
         }
      }
   case COVER_TAG_ITEM:
      {
         cover_item_t *item = cover_item_data(db, obj);
         switch (attr) {
         case COV_ATTR_FUNC_NAME:
            item->func_name = value;
            return;
         default:
            should_not_reach_here();
         }
      }
   case COVER_TAG_SCOPE:
      {
         cover_scope_t *scope = cover_scope_data(db, obj);
         switch (attr) {
         case COV_ATTR_NAME:
            scope->name = value;
            return;
         case COV_ATTR_HIER:
            scope->hier = value;
            return;
         case COV_ATTR_BLOCK_NAME:
            scope->block_name = value;
            return;
         default:
            should_not_reach_here();
         }
      }
   default:
      should_not_reach_here();
   }
}

void cover_put_loc(cover_data_t *db, cover_obj_t obj, cover_attr_t attr,
                   loc_t value)
{
   switch (obj.tag) {
   case COVER_TAG_ITEM:
      {
         cover_item_t *item = cover_item_data(db, obj);
         switch (attr) {
         case COV_ATTR_LOC:
            item->loc = value;
            return;
         case COV_ATTR_LHS_LOC:
            item->loc_lhs = value;
            return;
         case COV_ATTR_RHS_LOC:
            item->loc_rhs = value;
            return;
         default:
            should_not_reach_here();
         }
      }
   default:
      should_not_reach_here();
   }
}

void cover_put_obj(cover_data_t *db, cover_obj_t obj, cover_attr_t attr,
                   cover_obj_t value)
{
   switch (obj.tag) {
   case COVER_TAG_SCOPE:
      {
         cover_scope_t *scope = cover_scope_data(db, obj);
         switch (attr) {
         case COV_ATTR_INST:
            scope->inst = value;
            return;
         default:
            should_not_reach_here();
         }
      }
   case COVER_TAG_INST:
      {
         cover_inst_t *inst = cover_inst_data(db, obj);
         switch (attr) {
         case COV_ATTR_ROOT:
            inst->root = value;
            return;
         default:
            should_not_reach_here();
         }
      }
   default:
      should_not_reach_here();
   }
}

void cover_put_flags(cover_data_t *db, cover_obj_t obj, cover_flags_t flags)
{
   switch (obj.tag) {
   case COVER_TAG_BIN:
      cover_bin_data(db, obj)->flags |= flags;
      break;
   default:
      should_not_reach_here();
   }
}
