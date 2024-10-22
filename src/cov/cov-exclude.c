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

static void to_upper_str(char *str)
{
   while (*str) {
      *str = toupper_iso88591(*str);
      str++;
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
            excl->found = true;

            if (item->data >= item->atleast) {
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

static void cover_parse_exclude_file(const char *path, cover_data_t *data)
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
      data->ef = xcalloc(sizeof(cover_ef_t));
      data->ef->alloc_excl_cmds = 16;
      data->ef->alloc_fold_cmds = 16;
      data->ef->excl = xcalloc_array(data->ef->alloc_excl_cmds,
                                     sizeof(cover_excl_cmd_t));
      data->ef->fold = xcalloc_array(data->ef->alloc_fold_cmds,
                                     sizeof(cover_fold_cmd_t));
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
               error_at(&loc, "exclude hierarchy missing");
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
            data->ef->n_excl_cmds++;
         }
         else if (!strcmp(tok, "fold")) {
            char *target = strtok(NULL, delim);
            char *source = strtok(NULL, delim);

            if (!target) {
               error_at(&loc, "fold target hierarchy missing");
               continue;
            }

            if (!source) {
               error_at(&loc, "fold source hierarchy missing");
               continue;
            }

            to_upper_str(target);
            to_upper_str(source);

            int n = data->ef->n_fold_cmds;
            if (n == data->ef->alloc_fold_cmds) {
               data->ef->alloc_fold_cmds *= 2;
               data->ef->fold = xrealloc_array(data->ef->fold,
                                               data->ef->alloc_fold_cmds,
                                               sizeof(cover_fold_cmd_t));
            }
            data->ef->fold[n].loc = loc;
            data->ef->fold[n].found_target = false;
            data->ef->fold[n].found_source = false;
            data->ef->fold[n].target = ident_new(target);
            data->ef->fold[n].source = ident_new(source);
            data->ef->n_fold_cmds++;
         }
         else
            error_at(&loc, "invalid command: $bold$%s$$", tok);

         tok = strtok(NULL, delim);
      }
      line_num++;
   }

   fclose(ef);
}

static void cover_apply_exclude_cmds(cover_data_t *data)
{
   assert (data->ef != NULL);

   for (int i = 0; i < data->ef->n_excl_cmds; i++)
      data->ef->excl[i].found = false;

   cover_exclude_scope(data, data->root_scope);

   for (int i = 0; i < data->ef->n_excl_cmds; i++)
      if (!data->ef->excl[i].found)
         warn_at(&data->ef->excl[i].loc, "excluded hierarchy does not match any "
                 "coverage item: '%s'", istr(data->ef->excl[i].hier));
}

static void cover_fold_scopes(cover_scope_t *tgt_scope, cover_scope_t *src_scope)
{
   int tgt_prefix_len = ident_len(tgt_scope->hier);
   int src_prefix_len = ident_len(src_scope->hier);

   // Process items
   for (int i = 0; i < src_scope->items.count; i++) {
      cover_item_t *src = AREF(src_scope->items, i);

      for (int j = 0; j < tgt_scope->items.count; j++) {
         cover_item_t *tgt = AREF(tgt_scope->items, j);
         ident_t tgt_suffix_hier = NULL;
         ident_t src_suffix_hier = NULL;

         // Compare hierarchical paths, but strip "tgt_scope" prefix from "tgt",
         // and "src_scope" from "src". Only hierarchy suffix needs to be the same.
         if (ident_len(tgt->hier) > tgt_prefix_len)
            tgt_suffix_hier = ident_new(istr(tgt->hier) + tgt_prefix_len);
         if (ident_len(src->hier) > src_prefix_len)
            src_suffix_hier = ident_new(istr(src->hier) + src_prefix_len);

         if ((tgt_suffix_hier == src_suffix_hier) && (tgt->flags == src->flags)) {
            assert(tgt->kind == src->kind);
            cover_merge_one_item(tgt, src->data);
            break;
         }
      }
   }

   // Process sub-scopes
   for (int i = 0; i < src_scope->children.count; i++) {
      cover_scope_t *src = src_scope->children.items[i];

      for (int j = 0; j < tgt_scope->children.count; j++) {
         cover_scope_t *tgt = tgt_scope->children.items[j];

         // Compare hierarchical paths, but strip "tgt_scope" prefix from "tgt",
         // and "src_scope" from "src". Only suffix of hierarchy needs to be the same!
         ident_t tgt_suffix_hier = NULL;
         ident_t src_suffix_hier = NULL;

         if (ident_len(tgt->hier) > tgt_prefix_len)
            tgt_suffix_hier = ident_new(istr(tgt->hier) + tgt_prefix_len);
         if (ident_len(src->hier) > src_prefix_len)
            src_suffix_hier = ident_new(istr(src->hier) + src_prefix_len);

         if (tgt_suffix_hier == src_suffix_hier) {
            cover_fold_scopes(tgt, src);
            break;
         }
      }
   }
}

static void cover_iterate_fold_source(cover_data_t *data, cover_scope_t *tgt_scope,
                                      cover_scope_t *src_scope, cover_fold_cmd_t *cmd)
{
   for (int i = 0; i < src_scope->children.count; i++) {
      cover_scope_t *curr_scp = src_scope->children.items[i];

      if (curr_scp->hier == cmd->source) {
         cmd->found_source = true;
         diag_t *d = diag_new(DIAG_DEBUG, NULL);
         diag_printf(d, "folding coverage scopes:");
         diag_hint(d, NULL, "        Target - %s", istr(tgt_scope->hier));
         diag_hint(d, NULL, "        Source - %s", istr(curr_scp->hier));
         diag_emit(d);
         cover_fold_scopes(tgt_scope, curr_scp);
      }

      cover_iterate_fold_source(data, tgt_scope, curr_scp, cmd);
   }
}

static void cover_iterate_fold_target(cover_data_t *data, cover_scope_t *tgt_scope,
                                      cover_fold_cmd_t *cmd)
{
   for (int i = 0; i < tgt_scope->children.count; i++) {
      cover_scope_t *curr_scp = tgt_scope->children.items[i];

      // On target scope name match, go and search for all source scopes and collapse them
      // into the target scope!
      if (curr_scp->hier == cmd->target) {
         cmd->found_target = true;
         cover_iterate_fold_source(data, curr_scp, data->root_scope, cmd);
      }

      cover_iterate_fold_target(data, curr_scp, cmd);
   }
}

static void cover_apply_fold_cmds(cover_data_t *data)
{
   assert (data->ef != NULL);

   if (data->ef->n_fold_cmds == 0)
      return;

   for (int i = 0; i < data->ef->n_fold_cmds; i++) {
      cover_fold_cmd_t *cmd = &(data->ef->fold[i]);
      cmd->found_source = false;
      cmd->found_target = false;

      cover_iterate_fold_target(data, data->root_scope, cmd);

      if (cmd->found_target == false)
         warn_at(&(cmd->loc), "fold target does not match any "
                 "coverage scope hierarchy: '%s'", istr(cmd->target));

      if (cmd->found_source == false)
         warn_at(&(cmd->loc), "fold source does not match any "
                 "coverage scope hierarchy: '%s'", istr(cmd->source));
   }
}

void cover_load_exclude_file(const char *path, cover_data_t *data)
{
   cover_parse_exclude_file(path, data);
   cover_apply_exclude_cmds(data);
   cover_apply_fold_cmds(data);
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

void cover_ignore_from_pragmas(cover_data_t *data, cover_scope_t *cs,
                               tree_t unit)
{
   if (!is_design_unit(unit))
      return;   // Generate block, etc.

   range_array_t *excl = &(cs->ignore_lines);
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
