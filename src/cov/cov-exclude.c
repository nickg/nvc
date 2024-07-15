//
//  Copyright (C) 2013-2023  Nick Gasson
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
#include "ident.h"

#include <limits.h>
#include <string.h>

struct _cover_exclude_ctx {
   FILE    *ef;
   char    *line;
   loc_t    loc;
};

//#define COVER_DEBUG_EXCLUDE

///////////////////////////////////////////////////////////////////////////////
// Exclude file
///////////////////////////////////////////////////////////////////////////////

static bool cover_exclude_hier(cover_scope_t *s, cover_exclude_ctx_t *ctx,
                               const char *excl_hier)
{
   bool match = false;
   int len = strlen(excl_hier);

   for (int i = 0; i < s->items.count; i += 1) {
      cover_item_t *item = AREF(s->items, i);
      ident_t hier = item->hier;

      if (ident_glob(hier, excl_hier, len)) {
#ifdef COVER_DEBUG_EXCLUDE
         printf("Applying matching exclude:\n");
         printf("    Item:        %s\n", istr(hier));
         printf("    Item flags:  %x\n", item->flags);
         printf("    Item data:   %x\n", item->data);
#endif
         match = true;

         const char *kind_str = cover_item_kind_str(item->kind);

         if (item->data > 0) {
            warn_at(&ctx->loc, "%s: '%s' is already covered, "
                               "it will be reported as covered.",
                               kind_str, istr(hier));
         }
         else {
            note_at(&ctx->loc, "excluding %s: '%s'", kind_str, istr(hier));
            item->flags |= COV_FLAG_EXCLUDED;
         }
      }
   }

   for (int i = 0; i < s->children.count; i++)
      match |= cover_exclude_hier(s->children.items[i], ctx, excl_hier);

   return match;
}

void cover_load_exclude_file(const char *path, cover_data_t *data)
{
   cover_exclude_ctx_t ctx = {};

   ctx.ef = fopen(path, "r");
   if (ctx.ef == NULL)
      fatal_errno("failed to open exclude file: %s\n", path);

   char *delim = " ";
   ssize_t read;
   file_ref_t file_ref = loc_file_ref(path, NULL);
   int line_num = 1;
   size_t line_len;

   while ((read = getline(&(ctx.line), &line_len, ctx.ef)) != -1) {

      // Strip comments and newline
      char *p = ctx.line;
      for (; *p != '\0' && *p != '#' && *p != '\n'; p++);
      *p = '\0';

      ctx.loc = get_loc(line_num, 0, line_num, p - ctx.line - 1, file_ref);

      // Parse line as space separated tokens
      for (char *tok = strtok(ctx.line, delim); tok; tok = strtok(NULL, delim)) {
         if (!strcmp(tok, "exclude")) {
            char *excl_hier = strtok(NULL, delim);

            if (!excl_hier)
               fatal_at(&ctx.loc, "exclude hierarchy missing!");

            int i = 0;
            while (excl_hier[i]) {
               excl_hier[i] = toupper_iso88591(excl_hier[i]);
               i++;
            }

            if (!cover_exclude_hier(data->root_scope, &ctx, excl_hier))
               warn_at(&ctx.loc, "exluded hierarchy does not match any "
                       "coverage item: '%s'", excl_hier);
         }
         else
            fatal_at(&ctx.loc, "invalid command: $bold$%s$$", tok);

         tok = strtok(NULL, delim);
      }
      line_num++;
   }

   fclose(ctx.ef);
}

///////////////////////////////////////////////////////////////////////////////
// Spec file
///////////////////////////////////////////////////////////////////////////////

void cover_load_spec_file(cover_data_t *data, const char *path)
{
   cover_exclude_ctx_t ctx = {};

   ctx.ef = fopen(path, "r");
   if (ctx.ef == NULL)
      fatal_errno("failed to open coverage specification file: %s\n", path);

   data->spec = xcalloc(sizeof(cover_spec_t));

   const char *delim = " ";
   ssize_t read;
   file_ref_t file_ref = loc_file_ref(path, NULL);
   int line_num = 1;
   size_t line_len;

   while ((read = getline(&(ctx.line), &line_len, ctx.ef)) != -1) {

      // Strip comments and newline
      char *p = ctx.line;
      for (; *p != '\0' && *p != '#' && *p != '\n'; p++);
      *p = '\0';

      ctx.loc = get_loc(line_num, 0, line_num, p - ctx.line - 1, file_ref);

      // Parse line as space separated tokens
      for (char *tok = strtok(ctx.line, delim); tok; tok = strtok(NULL, delim)) {
         bool include;
         if (tok[0] == '+')
            include = true;
         else if (tok[0] == '-')
            include = false;
         else
            fatal_at(&ctx.loc, "coverage specification command must "
                               "start with '+' or '-");
         tok++;

         char_array_t *arr;
         if (!strcmp(tok, "hierarchy")) {
            if (include)
               arr = &(data->spec->hier_include);
            else
               arr = &(data->spec->hier_exclude);
         }
         else if (!strcmp(tok, "block")) {
            if (include)
               arr = &(data->spec->block_include);
            else
               arr = &(data->spec->block_exclude);
         }
         else if (!strcmp(tok, "fsm_type")) {
            if (include)
               arr = &(data->spec->fsm_type_include);
            else
               arr = &(data->spec->fsm_type_exclude);
         }
         else
            fatal_at(&ctx.loc, "invalid command: $bold$%s$$", tok);

         char *hier = strtok(NULL, delim);
         if (!hier)
            fatal_at(&ctx.loc, "%s name missing", tok);

         APUSH(*arr, xstrdup(hier));

         char *str = AGET(*arr, arr->count - 1);
         int i = 0;
         while (str[i]) {
            str[i] = toupper_iso88591(str[i]);
            i++;
         }

         tok = strtok(NULL, delim);
      }
      line_num++;
   }

   fclose(ctx.ef);
}

///////////////////////////////////////////////////////////////////////////////
// Pragma handling
///////////////////////////////////////////////////////////////////////////////

void cover_ignore_from_pragmas(cover_data_t *data, tree_t unit)
{
   assert(data->top_scope != NULL);

   if (!is_design_unit(unit))
      return;   // Generate block, etc.

   range_array_t *excl = &(data->top_scope->ignore_lines);
   bool state = true;
   const int npragmas = tree_pragmas(unit);
   for (int i = 0; i < npragmas; i++) {
      tree_t p = tree_pragma(unit, i);
      const pragma_kind_t kind = tree_subkind(p);
      if (kind != PRAGMA_COVERAGE_OFF && kind != PRAGMA_COVERAGE_ON)
         continue;

      if (kind == PRAGMA_COVERAGE_OFF && state) {
         line_range_t lr = { tree_loc(p)->first_line, INT_MAX };
         APUSH(*excl, lr);
         state = false;
      }
      else if (kind == PRAGMA_COVERAGE_ON && !state) {
         assert(excl->count > 0);
         assert(excl->items[excl->count - 1].end == INT_MAX);
         excl->items[excl->count - 1].end = tree_loc(p)->first_line;
         state = true;
      }
   }
}
