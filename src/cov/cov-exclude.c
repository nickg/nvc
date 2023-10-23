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

static int cover_exclude_item(cover_exclude_ctx_t *ctx, cover_item_t *item,
                              const char *bin, ident_t hier)
{
   int kind = item->kind;

   switch (kind) {
   case COV_ITEM_STMT:
      if (bin)
         fatal_at(&ctx->loc, "statements do not contain bins, but bin '%s' "
                             "was given for statement: '%s'", bin, istr(hier));

      note_at(&ctx->loc, "excluding statement: '%s'", istr(hier));
      if (item->data)
         warn_at(&ctx->loc, "statement: '%s' already covered!", istr(hier));

      item->excl_msk |= 0xFFFFFFFF;
      return 1;

   case COV_ITEM_BRANCH:
   case COV_ITEM_TOGGLE:
   case COV_ITEM_EXPRESSION:
      {
         uint32_t allowed = item->flags & COVER_FLAGS_ALL_BINS, bmask = allowed;

         // If bin is not given, exclude all bins of a item
         if (bin != NULL)
            bmask = cover_bin_str_to_bmask(bin);

         if (!(bmask & allowed)) {
            LOCAL_TEXT_BUF tb = tb_new();
            cover_bmask_to_bin_list(allowed, tb);

            diag_t *d = diag_new(DIAG_FATAL, &ctx->loc);
            diag_printf(d, "invalid bin: $bold$'%s'$$ for %s: '%s'", bin,
                        cover_item_kind_str(item->kind), istr(item->hier));
            diag_hint(d, NULL, "valid bins are: %s", tb_get(tb));
            diag_emit(d);

            fatal_exit(1);
         }

         LOCAL_TEXT_BUF tb = tb_new();
         cover_bmask_to_bin_list(bmask, tb);
         note_at(&ctx->loc, "excluding %s: '%s' bins: %s",
                 cover_item_kind_str(item->kind), istr(hier), tb_get(tb));

         uint32_t excl_cov = bmask & item->data;
         if (excl_cov) {
            tb_rewind(tb);
            cover_bmask_to_bin_list(excl_cov, tb);
            warn_at(&ctx->loc, "%s: '%s' bins: %s already covered!",
                    cover_item_kind_str(item->kind), istr(hier), tb_get(tb));
         }

         item->excl_msk |= bmask;
         return 1;
      }

   case COV_ITEM_STATE:
      {
         int n_states = item->num;
         cover_item_t* curr_item = item;
         for (int i = 0; i < n_states; i++) {
            ident_t state_name = ident_rfrom(curr_item->hier, '.');
            if (!bin || !strcmp(istr(state_name), bin)) {
               if (curr_item->data & COV_FLAG_STATE)
                  warn_at(&ctx->loc, "%s: '%s' of '%s' already covered!",
                          cover_item_kind_str(item->kind), istr(state_name),
                          istr(hier));
               curr_item->excl_msk |= COV_FLAG_STATE;
            }
            curr_item++;
         }

         return n_states;
      }

   default:
      fatal("Unsupported cover item kind: %d", kind);
   }

   return 1;
}

static bool cover_exclude_hier(cover_scope_t *s, cover_exclude_ctx_t *ctx,
                               const char *excl_hier, const char *bin)
{
   bool match = false;
   int len = strlen(excl_hier);
   int step;

   for (int i = 0; i < s->items.count; i += step) {
      cover_item_t *item = AREF(s->items, i);
      ident_t hier = item->hier;

      // FSM state items contain bin name as part of hierarchy -> Strip it
      if (item->kind == COV_ITEM_STATE)
         hier = ident_runtil(hier, '.');

      if (ident_glob(hier, excl_hier, len)) {
#ifdef COVER_DEBUG_EXCLUDE
         printf("Applying matching exclude:\n");
         printf("    Item:        %s\n", istr(hier));
         printf("    Item flags:  %x\n", item->flags);
         printf("    Item data:   %x\n", item->data);
#endif
         match = true;
         step = cover_exclude_item(ctx, item, bin, hier);
      }
      else
         step = 1;
   }

   for (list_iter(cover_scope_t *, it, s->children))
      match |= cover_exclude_hier(it, ctx, excl_hier, bin);

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
   loc_file_ref_t file_ref = loc_file_ref(path, NULL);
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
            char *bin = strtok(NULL, delim);

            if (!excl_hier)
               fatal_at(&ctx.loc, "exclude hierarchy missing!");

            int i = 0;
            while (excl_hier[i]) {
               excl_hier[i] = toupper_iso88591(excl_hier[i]);
               i++;
            }

            if (!cover_exclude_hier(data->root_scope, &ctx, excl_hier, bin))
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
   loc_file_ref_t file_ref = loc_file_ref(path, NULL);
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
