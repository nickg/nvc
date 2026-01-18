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
#include "cov/cov-data.h"
#include "cov/cov-priv.h"
#include "cov/cov-structs.h"
#include "hash.h"
#include "ident.h"
#include "lib.h"
#include "thirdparty/sha1.h"

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
   hash_t       *hier;
   unsigned      skipped;
   unsigned      item_limit;
} cover_rpt_t;

static void rpt_visit_children(cover_rpt_t *rpt, rpt_hier_t *h,
                               const cover_scope_t *s);

static void get_hex_hash(const char *str, char out[SHA_HEX_LEN])
{
   SHA1_CTX ctx;
   unsigned char hash[SHA1_LEN];

   SHA1Init(&ctx);
   SHA1Update(&ctx, (const unsigned char *)str, strlen(str));
   SHA1Final(hash, &ctx);

   for (int i = 0; i < SHA1_LEN; i++)
      snprintf(out + i * 2, 3, "%02x", hash[i]);
}

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

static bool rpt_is_hit(const cover_item_t *item)
{
   return item->data >= item->atleast && item->atleast > 0;
}

static bool rpt_is_excluded(cover_rpt_t *rpt, const cover_item_t *item)
{
   if (item->flags & COV_FLAG_EXCLUDED)
      return true;

   if (cover_bin_unreachable(rpt->data, item))
      return true;

   return false;
}

static rpt_table_t *rpt_table_new(cover_rpt_t *rpt, const rpt_line_t *line,
                                  int count)
{
   if (count == 0)
      return NULL;

   rpt_table_t *table = pool_malloc_flex(rpt->pool, sizeof(rpt_table_t), count,
                                         sizeof(cover_item_t *));
   table->line  = line;
   table->count = count;

   return table;
}

static void rpt_get_detail(cover_rpt_t *rpt, rpt_detail_t *detail,
                           rpt_stats_t *stats, const cover_item_t *item,
                           const rpt_line_t *line)
{
   int nhit = 0, nmiss = 0, nexcl = 0;

   for (int i = 0; i < item->consecutive; i++) {
      stats->total[item->kind]++;

      if (rpt_is_hit(item + i)) {
         stats->hit[item->kind]++;
         nhit++;
      }
      else if (rpt_is_excluded(rpt, item + i)) {
         stats->hit[item->kind]++;
         nexcl++;
      }
      else
         nmiss++;

      assert(item[i].kind == item->kind);
   }

   if (detail->total > rpt->item_limit) {
      rpt->skipped += item->consecutive;
      return;
   }

   rpt_table_t *hit = rpt_table_new(rpt, line, nhit);
   rpt_table_t *miss = rpt_table_new(rpt, line, nmiss);
   rpt_table_t *excl = rpt_table_new(rpt, line, nexcl);

   for (int i = 0, hpos = 0, mpos = 0, epos = 0; i < item->consecutive; i++) {
      if (rpt_is_hit(item + i))
         hit->items[hpos++] = item + i;
      else if (rpt_is_excluded(rpt, item + i))
         excl->items[epos++] = item + i;
      else
         miss->items[mpos++] = item + i;
   }

   if (hit != NULL)
      APUSH(detail->hits[item->kind], hit);

   if (miss != NULL)
      APUSH(detail->miss[item->kind], miss);

   if (excl != NULL)
      APUSH(detail->excl[item->kind], excl);

   detail->total += nhit + nmiss + nexcl;
}

static void rpt_merge_file_items(cover_rpt_t *rpt, rpt_file_t *f,
                                 const cover_scope_t *s)
{
   // TODO: This has O(n^2). May be issue for large designs ?
   for (int i = 0; i < s->items.count; i++) {
      cover_item_t *scope_item = AGET(s->items, i);
      bool found = false;

      for (int j = 0; j < f->items.count; j++) {
         cover_item_t *file_item = AGET(f->items, j);

         // We must take into account:
         //    - kind   - different kind items can be at the same loc
         //    - loc    - to get aggregated per-file data
         //    - flags  - to not merge different bins
         found =
            (file_item->kind == scope_item->kind)
            && loc_eq(&(file_item->loc), &(scope_item->loc))
            && (file_item->flags == scope_item->flags);

         if (found) {
            cover_merge_one_item(file_item, scope_item->data);
            break;
         }
      }

      if (!found) {
         cover_item_t *copy = pool_malloc(rpt->pool, sizeof(cover_item_t));
         *copy = *scope_item;
         copy->consecutive = 1;
         APUSH(f->items, copy);
      }
   }
}

static rpt_file_t *rpt_visit_file(cover_rpt_t *rpt, const cover_scope_t *s)
{
   if (loc_invalid_p(&(s->loc)))
      return NULL;

   const char *path = loc_file_str(&(s->loc));
   rpt_file_t *f = shash_get(rpt->files, path);
   if (f != NULL) {
      rpt_merge_file_items(rpt, f, s);
      return f->valid ? f : NULL;
   }

   f = pool_calloc(rpt->pool, sizeof(rpt_file_t));
   f->path = path;

   shash_put(rpt->files, path, f);

   for (int i = 0; i < s->items.count; i++)
      APUSH(f->items, s->items.items[i]);

   FILE *fp = fopen(path, "r");
   if (fp == NULL) {
      // Guess the path is relative to the work library
      char *relpath LOCAL = xasprintf("%s/../%s", lib_path(lib_work()), path);
      fp = fopen(relpath, "r");
   }

   if (fp == NULL) {
      warn_at(&(s->loc), "omitting hierarchy %s from the coverage report as "
              "the correpsonding source file could not be found",
              istr(s->hier));
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

static rpt_hier_t *rpt_visit_hier(cover_rpt_t *rpt, const cover_scope_t *s)
{
   rpt_hier_t *h = pool_calloc(rpt->pool, sizeof(rpt_hier_t));
   get_hex_hash(istr(s->hier), h->name_hash);

   rpt_visit_file(rpt, s);

   assert(hash_get(rpt->hier, s->block) == NULL);
   hash_put(rpt->hier, s->block, h);

   rpt_visit_children(rpt, h, s);

   rpt_merge_stats(&h->nested_stats, &h->flat_stats);

   return h;
}

static void rpt_visit_sub_scope(cover_rpt_t *rpt, rpt_hier_t *h,
                                cover_scope_t *s)
{
   rpt_file_t *f_src = rpt_visit_file(rpt, s);
   if (f_src != NULL) {
      for (int i = 0; i < s->items.count;) {
         cover_item_t *item = s->items.items[i];
         assert(item->loc.file_ref == s->loc.file_ref);

         const rpt_line_t *line = rpt_get_line(f_src, &item->loc);
         if (line != NULL)
            rpt_get_detail(rpt, &h->detail, &h->flat_stats, item, line);

         i += item->consecutive;
      }
   }

   rpt_visit_children(rpt, h, s);
}

static void rpt_visit_children(cover_rpt_t *rpt, rpt_hier_t *h,
                               const cover_scope_t *s)
{
   for (int i = 0; i < s->children.count; i++) {
      cover_scope_t *it = s->children.items[i];
      if (cover_is_hier(it)) {
         rpt_hier_t *sub = rpt_visit_hier(rpt, it);
         rpt_merge_stats(&h->nested_stats, &sub->nested_stats);
      }
      else
         rpt_visit_sub_scope(rpt, h, it);
   }
}

static void rpt_gen_file_details(cover_rpt_t *rpt, rpt_file_t *f)
{
   for (int i = 0; i < f->items.count;) {
      cover_item_t *item = f->items.items[i];

      const rpt_line_t *line = rpt_get_line(f, &item->loc);
      if (line != NULL)
         rpt_get_detail(rpt, &f->detail, &f->stats, item, line);

      i += item->consecutive;
   }
}

const rpt_file_t *rpt_get_file(cover_rpt_t *rpt, cover_scope_t *s)
{
   if (loc_invalid_p(&(s->loc)))
      return NULL;

   const char *path = loc_file_str(&(s->loc));
   rpt_file_t *f = shash_get(rpt->files, path);
   if (f != NULL)
      return f->valid ? f : NULL;

   return NULL;
}

const rpt_hier_t *rpt_get_hier(cover_rpt_t *rpt, cover_scope_t *s)
{
   assert(cover_is_hier(s));

   rpt_hier_t *h = hash_get(rpt->hier, s->block);
   if (h == NULL)
      fatal_trace("no hierarchy report for %s", istr(s->block->name));

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
   rpt->hier       = hash_new(32);
   rpt->item_limit = item_limit;

   for (int i = 0; i < db->root_scope->children.count; i++) {
      cover_scope_t *child = AGET(db->root_scope->children, i);
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
   hash_free(rpt->hier);
   free(rpt);
}
