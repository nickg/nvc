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

static void to_upper_str(char *str) {
   int i = 0;
   while (str[i]) {
      str[i] = toupper_iso88591(str[i]);
      i++;
   }
}

///////////////////////////////////////////////////////////////////////////////
// Exclude file
///////////////////////////////////////////////////////////////////////////////

static void cover_exclude_scope(cover_data_t *data, cover_scope_t *s)
{
   for (int i = 0; i < s->items.count; i++) {
      cover_item_t *item = AREF(s->items, i);
      ident_t hier = item->hier;
      const char *kind_str = cover_item_kind_str(item->kind);

      for (int j = 0; j < data->ef->n_excl_cmds; j++) {
         cover_excl_cmd_t *excl = &(data->ef->excl[j]);
         const char *excl_hier = istr(excl->hier);

         if (ident_glob(hier, excl_hier, strlen(excl_hier))) {
   #ifdef COVER_DEBUG_EXCLUDE
            printf("Applying matching exclude:\n");
            printf("    Exclude hierarchy:   %s\n", istr(excl_hier));
            printf("    Item:                %s\n", istr(hier));
            printf("    Item flags:          %x\n", item->flags);
            printf("    Item data:           %x\n", item->data);
            printf("    Item kind:           %s\n", kind_str);
   #endif

            excl->found = true;

            if (item->data > 0) {
               warn_at(&excl->loc, "%s: '%s' is already covered, "
                                   "it will be reported as covered.",
                                   kind_str, istr(hier));
            }
            else {
               note_at(&excl->loc, "excluding %s: '%s'", kind_str, istr(hier));
               item->flags |= COV_FLAG_EXCLUDED;
            }
         }

      }
   }

   for (int i = 0; i < s->children.count; i++)
      cover_exclude_scope(data, s->children.items[i]);
}

void cover_parse_exclude_file(const char *path, cover_data_t *data)
{
   FILE *ef = fopen(path, "r");
   if (ef == NULL)
      fatal_errno("failed to open exclude file: %s\n", path);

   char *delim = " ";
   ssize_t read;
   file_ref_t file_ref = loc_file_ref(path, NULL);
   int line_num = 1;
   char *line = NULL;
   size_t line_len;

   if (data->ef == NULL) {
      data->ef = xmalloc(sizeof(cover_ef_t));
      data->ef->alloc_excl_cmds = 16;
      data->ef->n_excl_cmds = 0;
      data->ef->excl = xmalloc_array(data->ef->alloc_excl_cmds,
                                     sizeof(cover_excl_cmd_t));
   }

   while ((read = getline(&line, &line_len, ef)) != -1) {

      // Strip comments and newline
      char *p = line;
      for (; *p != '\0' && *p != '#' && *p != '\n'; p++);
      *p = '\0';

      loc_t loc = get_loc(line_num, 0, line_num, p - line - 1, file_ref);

      // Parse line as space separated tokens
      for (char *tok = strtok(line, delim); tok; tok = strtok(NULL, delim)) {
         if (!strcmp(tok, "exclude")) {
            char *hier = strtok(NULL, delim);

            if (!hier) {
               error_at(&loc, "exclude hierarchy missing!");
               continue;
            }

            to_upper_str(hier);

            int n = data->ef->n_excl_cmds;
            if (n == data->ef->alloc_excl_cmds) {
               data->ef->alloc_excl_cmds *= 2;
               data->ef->excl = xrealloc_array(data->ef->excl,
                                               data->ef->alloc_excl_cmds,
                                               sizeof(cover_excl_cmd_t));
            }
            data->ef->excl[n].found = false;
            data->ef->excl[n].loc = loc;
            data->ef->excl[n].hier = ident_new(hier);
            data->ef->n_excl_cmds += 1;
         }
         else if (!strcmp(tok, "collapse")) {
            char *hier = strtok(NULL, delim);
            char *hier2 = strtok(NULL, delim);

            if (!hier) {
               error_at(&loc, "collapse target hierarchy missing!");
               continue;
            }

            if (!hier2) {
               error_at(&loc, "collapse source hierarchy missing!");
               continue;
            }

            to_upper_str(hier);
            to_upper_str(hier2);

            int n = data->ef->n_coll_cmds++;
            data->ef->coll = xrealloc_array(data->ef->coll, n,
                                             sizeof(cover_coll_cmd_t));
            data->ef->coll[n].found = false;
            data->ef->coll[n].hier = ident_new(hier);
            data->ef->coll[n].hier2 = ident_new(hier2);
         }
         else
            error_at(&loc, "invalid command: $bold$%s$$", tok);

         tok = strtok(NULL, delim);
      }
      line_num++;
   }

   fclose(ef);
}

void cover_apply_exclude_cmds(cover_data_t *data)
{
   for (int i = 0; i < data->ef->n_excl_cmds; i++)
      data->ef->excl[i].found = false;

   cover_exclude_scope(data, data->root_scope);

   for (int i = 0; i < data->ef->n_excl_cmds; i++)
      if (!data->ef->excl[i].found)
         warn_at(&data->ef->excl[i].loc, "exluded hierarchy does not match any "
                 "coverage item: '%s'", istr(data->ef->excl[i].hier));
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
