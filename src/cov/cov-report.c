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
#include "cov/cov-api.h"
#include "cov/cov-priv.h"
#include "cov/cov-structs.h"
#include "hash.h"
#include "ident.h"
#include "lib.h"

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <time.h>
#include <libgen.h>
#include <inttypes.h>

typedef struct _cover_rpt {
   cover_data_t *data;
   mem_pool_t   *pool;
   shash_t      *files;
   ihash_t      *hier;
   unsigned      skipped;
   unsigned      item_limit;
} cover_rpt_t;

static void rpt_visit_children(cover_rpt_t *rpt, rpt_hier_t *h,
                               cover_obj_t scope);

static rpt_line_t *rpt_get_line(rpt_file_t *f, const loc_t *loc)
{
   if (loc->first_line - 1 < f->n_lines)
      return &(f->lines[loc->first_line - 1]);

   return NULL;
}

static void rpt_merge_stats(rpt_stats_t *dst, const rpt_stats_t *src)
{
   for (int i = 0; i < ARRAY_LEN(dst->total); i++) {
      dst->total[i] += src->total[i];
      dst->hit[i] += src->hit[i];
   }
}

static bool rpt_is_hit(cover_rpt_t *rpt, const rpt_item_t *ri)
{
   int32_t threshold = cover_get_threshold(rpt->data, ri->item);
   return ri->data >= threshold && threshold > 0;
}

static bool rpt_is_excluded(cover_rpt_t *rpt, const rpt_item_t *ri)
{
   cover_flags_t flags = cover_get_flags(rpt->data, ri->bin);

   if (flags & COV_FLAG_EXCLUDED)
      return true;

   if (rpt->data->mask & COVER_MASK_EXCLUDE_UNREACHABLE) {
      cover_item_kind_t kind = cover_get_kind(rpt->data, ri->item);

      if (kind == COV_ITEM_TOGGLE && (ri->data & COV_FLAG_UNREACHABLE))
         return true;

      if (kind == COV_ITEM_EXPRESSION && (flags & COV_FLAG_UNREACHABLE))
         return true;
   }

   return false;
}

static rpt_table_t *rpt_table_new(cover_rpt_t *rpt, const rpt_line_t *line,
                                  int count)
{
   if (count == 0)
      return NULL;

   rpt_table_t *table = pool_malloc_flex(rpt->pool, sizeof(rpt_table_t), count,
                                         sizeof(rpt_item_t));
   table->line  = line;
   table->count = count;

   return table;
}

static void rpt_get_detail(cover_rpt_t *rpt, rpt_detail_t *detail,
                           rpt_stats_t *stats, cover_obj_t item,
                           const cover_obj_t *bins, const int32_t *data,
                           int count, const rpt_line_t *line)
{
   int nhit = 0, nmiss = 0, nexcl = 0;

   cover_item_kind_t kind = cover_get_kind(rpt->data, item);

   for (int i = 0; i < count; i++) {
      rpt_item_t ri = {
         .item = item,
         .bin  = bins[i],
      };

      if (data == NULL)
         ri.data = cover_get_u32(rpt->data, bins[i], COV_ATTR_DATA, 0);
      else
         ri.data = data[i];

      stats->total[kind]++;

      if (rpt_is_hit(rpt, &ri)) {
         stats->hit[kind]++;
         nhit++;
      }
      else if (rpt_is_excluded(rpt, &ri)) {
         stats->hit[kind]++;
         nexcl++;
      }
      else
         nmiss++;
   }

   rpt_table_t *hit = rpt_table_new(rpt, line, nhit);
   rpt_table_t *miss = rpt_table_new(rpt, line, nmiss);
   rpt_table_t *excl = rpt_table_new(rpt, line, nexcl);

   for (int i = 0, hpos = 0, mpos = 0, epos = 0; i < count; i++) {
      rpt_item_t ref = {
         .item = item,
         .bin  = bins[i],
      };

      if (data == NULL)
         ref.data = cover_get_u32(rpt->data, bins[i], COV_ATTR_DATA, 0);
      else
         ref.data = data[i];

      if (rpt_is_hit(rpt, &ref))
         hit->items[hpos++] = ref;
      else if (rpt_is_excluded(rpt, &ref))
         excl->items[epos++] = ref;
      else
         miss->items[mpos++] = ref;
   }

   if (hit != NULL) {
      APUSH(detail->hits[kind], hit);

      if (hit->count > rpt->item_limit) {
         rpt->skipped += hit->count - rpt->item_limit;
         hit->count = rpt->item_limit;
      }
   }

   if (miss != NULL) {
      APUSH(detail->miss[kind], miss);

      if (miss->count > rpt->item_limit) {
         rpt->skipped += miss->count - rpt->item_limit;
         miss->count = rpt->item_limit;
      }
   }

   if (excl != NULL) {
      APUSH(detail->excl[kind], excl);

      if (excl->count > rpt->item_limit) {
         rpt->skipped += excl->count - rpt->item_limit;
         excl->count = rpt->item_limit;
      }
   }

   detail->total += nhit + nmiss + nexcl;
}

static void rpt_merge_data(cover_rpt_t *rpt, rpt_item_t *ri, int32_t data)
{
   switch (cover_get_kind(rpt->data, ri->item)) {
   case COV_ITEM_TOGGLE:
      if ((ri->data & COV_FLAG_UNREACHABLE) || (data & COV_FLAG_UNREACHABLE))
         ri->data = COV_FLAG_UNREACHABLE;
      else
         ri->data = saturate_add(ri->data, data);
      break;
   default:
      ri->data = saturate_add(ri->data, data);
      break;
   }
}

static void rpt_merge_file_items(cover_rpt_t *rpt, rpt_file_t *f,
                                 cover_obj_t scope)
{
   const int nitems = cover_count(rpt->data, scope, COV_REL_ITEMS);

   // TODO: This has O(n^2). May be issue for large designs ?
   for (int i = 0; i < nitems; i++) {
      cover_obj_t scope_item = cover_at(rpt->data, scope, COV_REL_ITEMS, i);

      cover_obj_t scope_bin0 = cover_at(rpt->data, scope_item, COV_REL_BINS, 0);
      cover_flags_t scope_flags = cover_get_flags(rpt->data, scope_bin0);
      uint32_t data = cover_get_u32(rpt->data, scope_bin0, COV_ATTR_DATA, 0);

      loc_t scope_loc = cover_get_loc(rpt->data, scope_item, COV_ATTR_LOC);
      cover_item_kind_t scope_kind = cover_get_kind(rpt->data, scope_item);

      bool found = false;
      for (int j = 0; j < f->items.count; j++) {
         rpt_item_t *file_item = AREF(f->items, j);

         cover_obj_t file_bin0 = cover_at(rpt->data, file_item->item,
                                          COV_REL_BINS, 0);
         cover_flags_t file_flags = cover_get_flags(rpt->data, file_bin0);

         loc_t file_loc =
            cover_get_loc(rpt->data, file_item->item, COV_ATTR_LOC);
         cover_item_kind_t file_kind =
            cover_get_kind(rpt->data, file_item->item);

         // We must take into account:
         //    - kind   - different kind items can be at the same loc
         //    - loc    - to get aggregated per-file data
         //    - flags  - to not merge different bins
         found =
            (file_kind == scope_kind)
            && file_loc.first_line == scope_loc.first_line
            && file_loc.first_column == scope_loc.first_column
            && (file_flags == scope_flags);

         if (found) {
            rpt_merge_data(rpt, file_item, data);
            break;
         }
      }

      if (!found) {
         const rpt_item_t new = {
            .item = scope_item,
            .bin  = scope_bin0,
            .data = data,
         };
         APUSH(f->items, new);
      }
   }
}

static rpt_file_t *rpt_visit_file(cover_rpt_t *rpt, cover_obj_t scope)
{
   loc_t loc = cover_get_loc(rpt->data, scope, COV_ATTR_LOC);

   if (loc_invalid_p(&loc))
      return NULL;

   const char *path = loc_file_str(&loc);
   rpt_file_t *f = shash_get(rpt->files, path);
   if (f != NULL) {
      rpt_merge_file_items(rpt, f, scope);
      return f->valid ? f : NULL;
   }

   f = pool_calloc(rpt->pool, sizeof(rpt_file_t));
   f->path = path;

   shash_put(rpt->files, path, f);

   rpt_merge_file_items(rpt, f, scope);

   FILE *fp = fopen(path, "r");
   if (fp == NULL) {
      // Guess the path is relative to the work library
      char *relpath LOCAL = xasprintf("%s/../%s", lib_path(lib_work()), path);
      fp = fopen(relpath, "r");
   }

   if (fp == NULL) {
      warn_at(&loc, "omitting hierarchy %pi from the coverage report as "
              "the correpsonding source file could not be found",
              cover_get_ident(rpt->data, scope, COV_ATTR_HIER));
      return NULL;
   }

   size_t alloc_lines = 128;
   rpt_line_t *tmp_lines LOCAL =
      xmalloc_array(alloc_lines, sizeof(rpt_line_t));

   ssize_t nread;
   size_t len = 0;
   char *buf LOCAL = NULL;
   while ((nread = getline(&buf, &len, fp)) != -1) {
      if (f->n_lines == alloc_lines) {
         alloc_lines *= 2;
         tmp_lines = xrealloc_array(tmp_lines, alloc_lines, sizeof(rpt_line_t));
      }

      rpt_line_t *l = &(tmp_lines[f->n_lines++]);
      l->text = pool_malloc(rpt->pool, nread + 1);
      l->len  = nread;

      memcpy(l->text, buf, nread + 1);
   }

   fclose(fp);

   f->lines = pool_malloc_array(rpt->pool, f->n_lines, sizeof(rpt_line_t));
   memcpy(f->lines, tmp_lines, f->n_lines * sizeof(rpt_line_t));

   get_hex_hash(f->path, f->path_hash);

   f->valid = true;
   return f;
}

static rpt_hier_t *rpt_visit_hier(cover_rpt_t *rpt, cover_obj_t scope)
{
   ident_t hier = cover_get_ident(rpt->data, scope, COV_ATTR_HIER);

   rpt_hier_t *h = pool_calloc(rpt->pool, sizeof(rpt_hier_t));
   get_hex_hash(istr(hier), h->name_hash);

   rpt_visit_file(rpt, scope);

   cover_obj_t inst = cover_get_obj(rpt->data, scope, COV_ATTR_INST);

   assert(ihash_get(rpt->hier, inst.bits) == NULL);
   ihash_put(rpt->hier, inst.bits, h);

   rpt_visit_children(rpt, h, scope);

   rpt_merge_stats(&h->nested_stats, &h->flat_stats);

   return h;
}

static void rpt_visit_sub_scope(cover_rpt_t *rpt, rpt_hier_t *h,
                                cover_obj_t scope)
{
   rpt_file_t *f_src = rpt_visit_file(rpt, scope);
   if (f_src != NULL) {
      const int nitems = cover_count(rpt->data, scope, COV_REL_ITEMS);

      for (int i = 0; i < nitems; i++) {
         cover_obj_t item = cover_at(rpt->data, scope, COV_REL_ITEMS, i);

         loc_t loc = cover_get_loc(rpt->data, item, COV_ATTR_LOC);

         const rpt_line_t *line = rpt_get_line(f_src, &loc);
         if (line == NULL)
            continue;

         cover_obj_t small[10];
         const int nbins = cover_rel(rpt->data, item, COV_REL_BINS, 0, small,
                                     ARRAY_LEN(small));

         if (nbins <= ARRAY_LEN(small))
            rpt_get_detail(rpt, &h->detail, &h->flat_stats, item, small, NULL,
                           nbins, line);
         else {
            cover_obj_t *ext LOCAL = xmalloc_array(nbins, sizeof(cover_obj_t));
            cover_rel(rpt->data, item, COV_REL_BINS, 0, ext, nbins);

            rpt_get_detail(rpt, &h->detail, &h->flat_stats, item, ext, NULL,
                           nbins, line);
         }
      }
   }

   rpt_visit_children(rpt, h, scope);
}

static void rpt_visit_children(cover_rpt_t *rpt, rpt_hier_t *h,
                               cover_obj_t scope)
{
   cover_iter_t it = cover_begin(rpt->data, scope, COV_REL_CHILDREN);
   cover_obj_t child;
   while (cover_next(&it, &child)) {
      if (cover_is_hier(rpt->data, child)) {
         rpt_hier_t *sub = rpt_visit_hier(rpt, child);
         rpt_merge_stats(&h->nested_stats, &sub->nested_stats);
      }
      else
         rpt_visit_sub_scope(rpt, h, child);
   }
}

static void rpt_gen_file_details(cover_rpt_t *rpt, rpt_file_t *f)
{
   for (int i = 0; i < f->items.count; i++) {
      const rpt_item_t *ri = &(f->items.items[i]);

      loc_t loc = cover_get_loc(rpt->data, ri->item, COV_ATTR_LOC);

      const rpt_line_t *line = rpt_get_line(f, &loc);
      if (line == NULL)
         continue;

      cover_obj_t bins[1] = { ri->bin };
      rpt_get_detail(rpt, &f->detail, &f->stats, ri->item,
                     bins, &ri->data, 1, line);
   }
}

const rpt_file_t *rpt_get_file(cover_rpt_t *rpt, cover_obj_t scope)
{
   loc_t loc = cover_get_loc(rpt->data, scope, COV_ATTR_LOC);

   if (loc_invalid_p(&loc))
      return NULL;

   const char *path = loc_file_str(&loc);
   rpt_file_t *f = shash_get(rpt->files, path);
   if (f != NULL)
      return f->valid ? f : NULL;

   return NULL;
}

const rpt_hier_t *rpt_get_hier(cover_rpt_t *rpt, cover_obj_t scope)
{
   assert(cover_is_hier(rpt->data, scope));

   cover_obj_t inst = cover_get_obj(rpt->data, scope, COV_ATTR_INST);

   rpt_hier_t *h = ihash_get(rpt->hier, inst.bits);
   if (h == NULL)
      fatal_trace("no hierarchy report for %pI",
                  cover_get_ident(rpt->data, inst, COV_ATTR_NAME));

   return h;
}

unsigned rpt_get_skipped(cover_rpt_t *rpt)
{
   return rpt->skipped;
}

int rpt_iter_files(cover_rpt_t *rpt, rpt_file_fn_t fn, void *ctx)
{
   const char *key;
   void *value;
   int count = 0;
   for (hash_iter_t it = HASH_BEGIN;
        shash_iter(rpt->files, &it, &key, &value); ) {
      rpt_file_t *f = value;
      if (f->valid) {
         if (fn != NULL) (*fn)(f, ctx);
         count++;
      }
   }
   return count;
}

cover_rpt_t *cover_report_new(cover_data_t *db, int item_limit)
{
   cover_rpt_t *rpt = xcalloc(sizeof(cover_rpt_t));
   rpt->data       = db;
   rpt->pool       = pool_new();
   rpt->files      = shash_new(32);
   rpt->hier       = ihash_new(32);
   rpt->item_limit = item_limit;

   const int nchildren = cover_count(db, db->root_scope, COV_REL_CHILDREN);
   for (int i = 0; i < nchildren; i++) {
      cover_obj_t child = cover_at(db, db->root_scope, COV_REL_CHILDREN, i);
      rpt_visit_hier(rpt, child);
   }

   const char *key;
   void *value;
   for (hash_iter_t it = HASH_BEGIN;
        shash_iter(rpt->files, &it, &key, &value); ) {
      rpt_file_t *f = value;
      if (f->valid)
         rpt_gen_file_details(rpt, value);
   }

   return rpt;
}

void cover_report_free(cover_rpt_t *rpt)
{
#ifdef DEBUG
   size_t alloc, npages;
   pool_stats(rpt->pool, &alloc, &npages);
   if (npages > 0)
      debugf("coverage report allocated %zu bytes in %zu pages",
             alloc, npages);
#endif

   pool_free(rpt->pool);
   shash_free(rpt->files);
   ihash_free(rpt->hier);
   free(rpt);
}
