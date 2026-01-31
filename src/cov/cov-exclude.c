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
#include "ident.h"
#include "option.h"
#include "printf.h"

#include <limits.h>
#include <string.h>
#include <stdlib.h>

typedef struct _excl_children excl_children_t;
typedef struct _excl_trie excl_trie_t;

typedef struct _cover_spec {
   excl_trie_t *hier_trie;
   excl_trie_t *block_trie;
   excl_trie_t *fsm_trie;
} cover_spec_t;

typedef enum {
   ACTION_NONE,
   ACTION_INCLUDE,
   ACTION_EXCLUDE,
} excl_action_t;

typedef enum {
   EXCL_EXACT,
   EXCL_SUBTREE,
} excl_scope_t;

typedef struct {
   ident_t      name;
   excl_trie_t *trie;
} excl_child_t;

typedef struct _excl_children {
   excl_child_t     items[7];
   excl_children_t *next;
} excl_children_t;

typedef struct _excl_trie {
   excl_action_t   action;
   excl_scope_t    scope;
   excl_children_t children;
} excl_trie_t;

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

static excl_trie_t *excl_trie_get(cover_data_t *db,excl_trie_t *where,
                                  const char *hier)
{
   assert(where != NULL);

   if (hier[0] == '\0')
      return where;
   else if (hier[0] == '*') {
      where->scope = EXCL_SUBTREE;
      return where;
   }

   const char *dot = strchrnul(hier, '.');
   ident_t id = ident_new_n(hier, dot - hier);

   for (excl_children_t *it = &(where->children); it; it = it->next) {
      int pos = 0;
      for (; pos < ARRAY_LEN(it->items); pos++) {
         if (it->items[pos].name == NULL)
            break;
         else if (ident_casecmp(it->items[pos].name, id))
            return excl_trie_get(db, it->items[pos].trie, dot + 1);
      }

      if (pos < ARRAY_LEN(it->items)) {
         excl_trie_t *et = pool_calloc(db->pool, sizeof(excl_trie_t));
         et->action = ACTION_NONE;
         et->scope = EXCL_EXACT;

         it->items[pos].name = id;
         it->items[pos].trie = et;

         if (*dot == '\0')
            return et;
         else
            return excl_trie_get(db, it->items[pos].trie, dot + 1);
      }
      else if (it->next == NULL)
         it->next = pool_calloc(db->pool, sizeof(excl_children_t));
   }

   should_not_reach_here();
}

static excl_trie_t *excl_trie_search(excl_trie_t *et, ident_t id)
{
   for (excl_children_t *it = &(et->children); it; it = it->next) {
      int pos = 0;
      for (; pos < ARRAY_LEN(it->items); pos++) {
         if (it->items[pos].name == NULL)
            break;
         else if (ident_casecmp(it->items[pos].name, id))
            return it->items[pos].trie;
      }
   }

   return NULL;
}

static excl_trie_t *excl_trie_for_scope(excl_trie_t *root,
                                        const cover_scope_t *cs)
{
   if (cs == NULL)
      return root;

   excl_trie_t *parent = excl_trie_for_scope(root, cs->parent);
   if (parent == NULL)
      return NULL;

   excl_trie_t *et = excl_trie_search(parent, ident_rfrom(cs->hier, '.'));
   if (et != NULL)
      return et;

   if (parent->scope == EXCL_SUBTREE)
      return parent;

   return NULL;
}

static void excl_trie_print(excl_trie_t *et, int indent)
{
   if (et->action != ACTION_NONE) {
      switch (et->action) {
      case ACTION_NONE: should_not_reach_here();
      case ACTION_INCLUDE: printf(" include"); break;
      case ACTION_EXCLUDE: printf(" exclude"); break;
      }

      switch (et->scope) {
      case EXCL_EXACT: printf(" exact"); break;
      case EXCL_SUBTREE: printf(" subtree"); break;
      }
   }

   printf("\n");

   for (excl_children_t *it = &(et->children); it; it = it->next) {
      for (int i = 0; i < ARRAY_LEN(it->items); i++) {
         if (it->items[i].name == NULL)
            break;

         nvc_printf("%*.s$!blue$%pi$$", indent, "", it->items[i].name);
         excl_trie_print(it->items[i].trie, indent + 2);
      }
   }
}

////////////////////////////////////////////////////////////////////////////////
// Exclude file

static void cover_exclude_items(cover_ef_t *ef, cover_item_t *item)
{
   for (int i = 0; i < item->consecutive; i++) {
      ident_t hier = item[i].hier;

      for (int j = 0; j < ef->n_excl_cmds; j++) {
         cover_excl_cmd_t *excl = &(ef->excl[j]);
         const char *excl_hier = istr(excl->hier);

         if (ident_glob(hier, excl_hier, strlen(excl_hier))) {
            excl->found = true;

            const char *kind_str = cover_item_kind_str(item[i].kind);
            if (item[i].data >= item[i].atleast) {
               warn_at(&excl->loc, "%s: '%s' is already covered, "
                                   "it will be reported as covered.",
                                   kind_str, istr(hier));
            }
            else {
               note_at(&excl->loc, "excluding %s: '%s'", kind_str, istr(hier));
               item[i].flags |= COV_FLAG_EXCLUDED;
            }
         }
      }
   }
}

static void cover_exclude_scope(cover_data_t *data, cover_scope_t *s)
{
   for (int i = 0; i < s->items.count; i++)
      cover_exclude_items(data->ef, AGET(s->items, i));

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
      cover_item_t *src = AGET(src_scope->items, i);

      for (int j = 0; j < tgt_scope->items.count; j++) {
         cover_item_t *tgt = AGET(tgt_scope->items, j);
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

void cover_load_spec_file(cover_data_t *db, const char *path)
{
   FILE *f = fopen(path, "r");
   if (f == NULL)
      fatal_errno("failed to open %s", path);

   file_ref_t file_ref = loc_file_ref(path, NULL);

   cover_spec_t *spec = pool_calloc(db->pool, sizeof(cover_spec_t));
   spec->hier_trie  = pool_calloc(db->pool, sizeof(excl_trie_t));
   spec->block_trie = pool_calloc(db->pool, sizeof(excl_trie_t));
   spec->fsm_trie   = pool_calloc(db->pool, sizeof(excl_trie_t));

   const char *delim = " ";
   ssize_t read;
   int line_num = 1;
   size_t line_len;
   char *line = NULL;

   for (; (read = getline(&line, &line_len, f)) != -1; line_num++) {

      // Strip comments and newline
      char *p = line;
      for (; *p != '\0' && *p != '#' && *p != '\n'; p++);
      *p = '\0';

      loc_t loc = get_loc(line_num, 0, line_num, p - line - 1, file_ref);

      // Parse line as space separated tokens
      char *tok = strtok(line, delim);
      if (tok == NULL)
         continue;

      bool include;
      if (tok[0] == '+')
         include = true;
      else if (tok[0] == '-')
         include = false;
      else {
         error_at(&loc, "coverage specification command must start "
                  "with '+' or '-");
         continue;
      }

      tok++;

      excl_trie_t *root;
      if (strcmp(tok, "hierarchy") == 0)
         root = spec->hier_trie;
      else if (strcmp(tok, "block") == 0)
         root = spec->block_trie;
      else if (!strcmp(tok, "fsm_type"))
         root = spec->fsm_trie;
      else {
         error_at(&loc, "invalid command $bold$%s$$", tok);
         continue;
      }

      char *hier = strtok(NULL, delim);
      if (!hier) {
         error_at(&loc, "%s name missing", tok);
         continue;
      }

      excl_trie_t *et = excl_trie_get(db, root, hier);
      et->action = include ? ACTION_INCLUDE : ACTION_EXCLUDE;

      if ((tok = strtok(NULL, delim)) != NULL)
         error_at(&loc, "unexpected '%s' after coverage specification "
                  "command", tok);
   }

   fclose(f);
   free(line);

   db->spec = spec;

   if (opt_get_int(OPT_EXCL_VERBOSE)) {
      nvc_printf("$!magenta$Blocks$$");
      excl_trie_print(spec->block_trie, 2);

      nvc_printf("\n$!magenta$Hierarchy$$");
      excl_trie_print(spec->hier_trie, 2);

      nvc_printf("\n$!magenta$FSM types$$");
      excl_trie_print(spec->fsm_trie, 2);

      printf("\n");
   }
}

bool cover_should_emit_scope(cover_data_t *db, cover_scope_t *cs)
{
   if (db->spec == NULL)
      return true;

   cover_scope_t *blk = cs;
   for (; blk != NULL && blk->block_name == NULL; blk = blk->parent);

   if (blk == NULL)
      return true;

   ident_t ename = ident_until(blk->block_name, '-');

   const excl_trie_t *et_block = excl_trie_search(db->spec->block_trie, ename);
   if (et_block != NULL)
      return et_block->action == ACTION_INCLUDE;

   const excl_trie_t *et_hier = excl_trie_for_scope(db->spec->hier_trie, cs);
   if (et_hier != NULL)
      return et_hier->action == ACTION_INCLUDE;

   return false;
}

bool cover_should_emit_fsm_type(cover_data_t *db, ident_t name)
{
   excl_trie_t *et = NULL;
   if (db->spec != NULL)
      et = excl_trie_search(db->spec->fsm_trie, name);

   // Type should be recognized as FSM
   if (et != NULL && et->action == ACTION_INCLUDE)
      return true;

   // By default enums should not included
   if (db->mask & COVER_MASK_FSM_NO_DEFAULT_ENUMS)
      return false;

   // Type should not be included
   if (et != NULL && et->action == ACTION_EXCLUDE)
      return false;

   return true;
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
