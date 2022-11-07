//
//  Copyright (C) 2013-2022  Nick Gasson
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
#include "common.h"
#include "cover.h"
#include "lib.h"
#include "opt.h"
#include "type.h"
#include "rt.h"
#include "rt/structs.h"
#include "model.h"

#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <time.h>

//#define COVER_DEBUG

#define MARGIN_LEFT "20%%"
#define SIDEBAR_WIDTH "15%%"

typedef A(cover_tag_t) tag_array_t;

typedef struct _cover_report_ctx cover_report_ctx_t;
typedef struct _cover_file       cover_file_t;
typedef struct _cover_scope      cover_scope_t;

typedef struct _cover_scope {
   ident_t        name;
   int            branch_label;
   int            stmt_label;
   cover_scope_t *parent;
} cover_scope_t;

struct _cover_tagging {
   int            next_stmt_tag;
   int            next_branch_tag;
   int            next_toggle_tag;
   int            next_hier_tag;
   ident_t        hier;
   tag_array_t    tags;
   cover_mask_t   mask;
   cover_scope_t *top_scope;
};

typedef struct {
   unsigned    total_stmts;
   unsigned    hit_stmts;
   unsigned    total_branches;
   unsigned    hit_branches;
   unsigned    total_toggles;
   unsigned    hit_toggles;
} cover_stats_t;

typedef struct {
   char       *text;
   size_t      len;
} cover_line_t;

typedef struct {
   cover_line_t *line;
   cover_tag_t *tag;
   int flags;
} cover_pair_t;

typedef struct {
   cover_pair_t *hits;
   cover_pair_t *miss;
   int          n_hits;
   int          n_miss;
   int          alloc_hits;
   int          alloc_miss;
} cover_chain_t;

struct _cover_file {
   const char   *name;
   cover_line_t *lines;
   unsigned      n_lines;
   unsigned      alloc_lines;
   bool          valid;
   cover_file_t *next;
};

struct _cover_report_ctx {
   cover_stats_t        flat_stats;
   cover_stats_t        nested_stats;
   cover_report_ctx_t   *parent;
   cover_tag_t          *start_tag;
   cover_chain_t        ch_stmt;
   cover_chain_t        ch_branch;
   cover_chain_t        ch_toggle;
};

static cover_file_t  *files;

enum std_ulogic {
   _U  = 0x0,
   _X  = 0x1,
   _0  = 0x2,
   _1  = 0x3,
   _Z  = 0x4,
   _W  = 0x5,
   _L  = 0x6,
   _H  = 0x7,
   _DC = 0x8
};

bool cover_is_stmt(tree_t t)
{
   switch (tree_kind(t)) {
   case T_IF:
   case T_WHILE:
   case T_NEXT:
   case T_EXIT:
   case T_SIGNAL_ASSIGN:
   case T_ASSERT:
   case T_VAR_ASSIGN:
   case T_RETURN:
   case T_FOR:
   case T_PCALL:
   case T_FCALL:
   case T_CASE:
      return true;

   // Static waits are introduced during simp pass. These are hidden
   // for user, no need to cover them.
   case T_WAIT:
      if (tree_flags(t) & TREE_F_STATIC_WAIT)
         return false;
      return true;

   default:
      return false;
   }
}

fbuf_t *cover_open_lib_file(tree_t top, fbuf_mode_t mode, bool check_null)
{
   char *dbname LOCAL = xasprintf("_%s.covdb", istr(tree_ident(top)));
   fbuf_t *f = lib_fbuf_open(lib_work(), dbname, mode, FBUF_CS_NONE);

   if (check_null && (f == NULL))
      fatal_errno("failed to open coverage db file: %s", dbname);

   return f;
}

cover_tag_t *cover_add_tag(tree_t t, ident_t suffix, cover_tagging_t *ctx,
                           tag_kind_t kind, uint32_t flags)
{
   int *cnt;
   assert (ctx != NULL);

   switch (kind) {
      case TAG_STMT:   cnt = &(ctx->next_stmt_tag);   break;
      case TAG_BRANCH: cnt = &(ctx->next_branch_tag); break;
      case TAG_TOGGLE: cnt = &(ctx->next_toggle_tag); break;
      case TAG_HIER:   cnt = &(ctx->next_hier_tag);   break;
      default:
         fatal("Unknown coverage type: %d", kind);
   }
   assert (cnt != NULL);

   // Everything creates scope, so name of current tag is already given
   // by scope in hierarchy.
   ident_t hier = ctx->hier;
   if (suffix)
      hier = ident_prefix(hier, suffix, '\0');

#ifdef COVER_DEBUG
   printf("Tag: %s\n", istr(hier));
   printf("    First line: %d\n", tree_loc(t)->first_line);
   printf("    First column: %d\n", tree_loc(t)->first_column);
   printf("    Line delta: %d\n", tree_loc(t)->line_delta);
   printf("    Column delta: %d\n", tree_loc(t)->column_delta);
   printf("\n\n");
#endif

   cover_tag_t new = {
      .kind       = kind,
      .tag        = *cnt,
      .data       = 0,
      .flags      = flags,
      .loc        = *tree_loc(t),
      .hier       = hier
   };

   APUSH(ctx->tags, new);
   (*cnt)++;

   return AREF(ctx->tags, ctx->tags.count - 1);
}

void cover_dump_tags(cover_tagging_t *ctx, fbuf_t *f, cover_dump_t dt,
                     const int32_t *stmts, const int32_t *branches,
                     const int32_t *toggles)
{

#ifdef COVER_DEBUG
   printf("Dumping coverage entries:\n");
   printf("Number of statement tags: %d\n", ctx->next_stmt_tag);
   printf("Number of branch tags: %d\n", ctx->next_branch_tag);
   printf("Number of toggle tags: %d\n", ctx->next_toggle_tag);
   printf("Number of hierarchy tags: %d\n", ctx->next_hier_tag);
   printf("Total tag count: %d\n", ctx->tags.count);
#endif

   write_u32(ctx->next_stmt_tag, f);
   write_u32(ctx->next_branch_tag, f);
   write_u32(ctx->next_toggle_tag, f);
   write_u32(ctx->next_hier_tag, f);

   loc_wr_ctx_t *loc_wr = loc_write_begin(f);
   ident_wr_ctx_t ident_ctx = ident_write_begin(f);

   for (int i = 0; i < ctx->tags.count; i++) {
      cover_tag_t *tag = &(ctx->tags.items[i]);

      write_u8(tag->kind, f);
      write_u32(tag->tag, f);

      if (dt == COV_DUMP_RUNTIME) {
         const int32_t *cnts = NULL;
         if (tag->kind == TAG_STMT)
            cnts = stmts;
         else if (tag->kind == TAG_BRANCH)
            cnts = branches;
         else if (tag->kind == TAG_TOGGLE)
            cnts = toggles;

         int32_t data = (cnts) ? cnts[tag->tag] : 0;
         write_u32(data, f);

#ifdef COVER_DEBUG
         printf("Index: %4d Tag: %s Kind: %d  Data: %d\n", tag->tag,
                istr(tag->hier), tag->kind, data);
#endif

      }
      else {
         write_u32(tag->data, f);
#ifdef COVER_DEBUG
         printf("Index: %4d Tag: %s Kind: %d  Data: %d\n", tag->tag,
                istr(tag->hier), tag->kind, tag->data);
#endif
      }
      write_u32(tag->flags, f);
      loc_write(&(tag->loc), loc_wr);
      ident_write(tag->hier, ident_ctx);
   }

   write_u8(TAG_LAST, f);

   loc_write_end(loc_wr);
   ident_write_end(ident_ctx);
}

cover_tagging_t *cover_tags_init(cover_mask_t mask)
{
   cover_tagging_t *ctx = xcalloc(sizeof(cover_tagging_t));
   ctx->mask = mask;

   return ctx;
}

bool cover_enabled(cover_tagging_t *tagging, cover_mask_t mask)
{
   return tagging != NULL && (tagging->mask & mask);
}

void cover_reset_scope(cover_tagging_t *tagging, ident_t hier)
{
   if (tagging == NULL)
    return;

   assert(tagging->top_scope == NULL);

   cover_scope_t *s = xcalloc(sizeof(cover_scope_t));
   s->name = hier;

   tagging->top_scope = s;
   tagging->hier = hier;
}

void cover_push_scope(cover_tagging_t *tagging, tree_t t)
{
   if (tagging == NULL)
      return;

   cover_scope_t *s = xcalloc(sizeof(cover_scope_t));
   ident_t name = NULL;
   tree_kind_t kind = tree_kind(t);

   // Named tree elements give hierarchy
   if (kind != T_ASSOC && kind != T_COND && tree_has_ident(t))
      name = tree_ident(t);

   // Branches / Statements are implicitly labelled like so:
   //   - By counter relevant for each scope
   //   - others choice separately
   else {
      assert(tagging->top_scope);

      char prefix[16] = {0};
      int *cnt;
      char c;

      // when others choice labelled explicitly
      if (kind == T_ASSOC && tree_subkind(t) == A_OTHERS)
         checked_sprintf(prefix, sizeof(prefix), "_B_OTHERS");
      else {
         if (kind == T_ASSOC || kind == T_COND) {
            cnt = &tagging->top_scope->branch_label;
            c = 'B';
         }
         else {
            cnt = &tagging->top_scope->stmt_label;
            c = 'S';
         }
         checked_sprintf(prefix, sizeof(prefix), "_%c%u", c, *cnt);
         (*cnt)++;
      }
      name = ident_new(prefix);
   }

   s->name   = name;
   s->parent = tagging->top_scope;

   tagging->top_scope = s;
   tagging->hier = ident_prefix(tagging->hier, name, '.');

#ifdef COVER_DEBUG
   printf("Pushing cover scope: %s\n", istr(tagging->hier));
#endif
}

void cover_pop_scope(cover_tagging_t *tagging)
{
   if (tagging == NULL)
      return;

   assert(tagging->top_scope != NULL);

   cover_scope_t *tmp = tagging->top_scope->parent;
   free(tagging->top_scope);
   tagging->top_scope = tmp;

#ifdef COVER_DEBUG
   printf("Popping cover scope: %s\n", istr(tagging->hier));
#endif

   tagging->hier = ident_runtil(tagging->hier, '.');
   assert(tagging->hier != NULL);
}

void cover_read_header(fbuf_t *f, cover_tagging_t *tagging)
{
   assert(tagging != NULL);

   tagging->next_stmt_tag = read_u32(f);
   tagging->next_branch_tag = read_u32(f);
   tagging->next_toggle_tag = read_u32(f);
   tagging->next_hier_tag = read_u32(f);
}

void cover_read_one_tag(fbuf_t *f, loc_rd_ctx_t *loc_rd,
                        ident_rd_ctx_t ident_ctx, cover_tag_t *tag)
{
   tag->kind = read_u8(f);
   if (tag->kind == TAG_LAST)
      return;

   tag->tag = read_u32(f);
   tag->data = read_u32(f);
   tag->flags = read_u32(f);

   loc_read(&(tag->loc), loc_rd);
   tag->hier = ident_read(ident_ctx);
}

cover_tagging_t *cover_read_tags(fbuf_t *f)
{
#ifdef COVER_DEBUG
   printf("Reading coverage database.\n");
#endif

   cover_tagging_t *tagging = xcalloc(sizeof(cover_tagging_t));
   cover_read_header(f, tagging);

   loc_rd_ctx_t *loc_rd = loc_read_begin(f);
   ident_rd_ctx_t ident_ctx = ident_read_begin(f);

   for (;;) {
      cover_tag_t new;
      cover_read_one_tag(f, loc_rd, ident_ctx, &new);

      if (new.kind == TAG_LAST)
         break;

      APUSH(tagging->tags, new);
   }

   loc_read_end(loc_rd);
   return tagging;
}

void cover_merge_tags(fbuf_t *f, cover_tagging_t *tagging)
{
   assert (tagging != NULL);

   cover_read_header(f, tagging);

   loc_rd_ctx_t *loc_rd = loc_read_begin(f);
   ident_rd_ctx_t ident_ctx = ident_read_begin(f);

   for (;;) {
      cover_tag_t new;
      cover_read_one_tag(f, loc_rd, ident_ctx, &new);

      if (new.kind == TAG_LAST)
         break;

      // TODO: Could merging be done more efficiently?
      bool found = false;
      for (int i = 0; i < tagging->tags.count; i++) {
         cover_tag_t *old = AREF(tagging->tags, i);

         // Compare based on hierarchical path, each
         // statement / branch / signal has unique hierarchical name
         if (new.hier == old->hier) {
            assert(new.kind == old->kind);
#ifdef COVER_DEBUG
            printf("Merging coverage tag: %s\n", istr(old->hier));
#endif
            switch (new.kind) {
            case TAG_STMT:
               old->data += new.data;
               break;
            case TAG_TOGGLE:
            case TAG_BRANCH:
               old->data |= new.data;
               break;
            default:
               break;
            }

            found = true;
            break;
         }
      }

      // TODO: Append the new tag just before popping hierarchy tag
      //       with longest common prefix of new tag. That will allow to
      //       merge coverage of IPs from different configurations of
      //       generics which form hierarchy differently!
      if (!found)
         warnf("Dropping coverage tag: %s\n", istr(new.hier));
   }
}

void cover_count_tags(cover_tagging_t *tagging, int32_t *n_stmts,
                      int32_t *n_branches, int32_t *n_toggles)
{
   if (tagging == NULL) {
      *n_stmts = 0;
      *n_branches = 0;
      *n_toggles = 0;
   }
   else {
      *n_stmts = tagging->next_stmt_tag;
      *n_branches = tagging->next_branch_tag;
      *n_toggles = tagging->next_toggle_tag;
   }
}

void cover_toggle_event_cb(uint64_t now, rt_signal_t *s, rt_watch_t *w,
                           void *user)
{

#ifdef COVER_DEBUG
   printf("Time: %lu Callback on signal: %s\n",
           now, istr(tree_ident(s->where)));
#endif

   uint32_t sig_size = s->shared.size;
   int32_t *toggle_mask = ((int32_t *)user) + sig_size - 1;

   for (int i = 0; i < sig_size; i++) {
      uint8_t new = ((uint8_t*)signal_value(s))[i];
      uint8_t old = ((uint8_t*)signal_last_value(s))[i];

      // 0->1
      if (old == _0 && new == _1)
         *toggle_mask |= 0x1;

      // 1->0
      if (old == _1 && new == _0)
         *toggle_mask |= 0x2;

      toggle_mask--;
   }

#ifdef COVER_DEBUG
   printf("New signal value:\n");
   for (int i = 0; i < sig_size; i++)
      printf("0x%x ", ((uint8_t*)signal_value(s))[i]);
   printf("\n");

   printf("Old signal value:\n");
   for (int i = 0; i < sig_size; i++) {
      printf("0x%x ", ((const uint8_t *)signal_last_value(s))[i]);
   }
   printf("\n\n");
#endif

}

void x_cover_setup_toggle_cb(sig_shared_t *ss, int32_t *toggle_mask)
{
   rt_signal_t *s = container_of(ss, rt_signal_t, shared);
   rt_model_t *m = get_model();
   model_set_event_cb(m, s, cover_toggle_event_cb, toggle_mask, false);
}


///////////////////////////////////////////////////////////////////////////////
// Report generation
///////////////////////////////////////////////////////////////////////////////

static void cover_append_line(cover_file_t *f, const char *buf)
{
   if (f->n_lines == f->alloc_lines) {
      f->alloc_lines *= 2;
      f->lines = xrealloc_array(f->lines, f->alloc_lines, sizeof(cover_line_t));
   }

   cover_line_t *l = &(f->lines[(f->n_lines)++]);
   l->text = xstrdup(buf);
   l->len  = strlen(buf);
}

static cover_file_t *cover_file(const loc_t *loc)
{
   if (loc_invalid_p(loc))
      return NULL;

   cover_file_t *f;
   for (f = files; f != NULL; f = f->next) {
      // Comparing pointers directly here is OK since only one copy
      // of the file name string will be created by tree_read
      if (f->name == loc_file_str(loc))
         return f->valid ? f : NULL;
   }

   f = xmalloc(sizeof(cover_file_t));
   f->name        = loc_file_str(loc);
   f->n_lines     = 0;
   f->alloc_lines = 1024;
   f->lines       = xmalloc_array(f->alloc_lines, sizeof(cover_line_t));
   f->next        = files;

   FILE *fp = fopen(loc_file_str(loc), "r");

   if (fp == NULL) {
      // Guess the path is relative to the work library
      char *path LOCAL =
         xasprintf("%s/../%s", lib_path(lib_work()), loc_file_str(loc));
      fp = fopen(path, "r");
   }

   if (fp == NULL) {
      warnf("failed to open %s for coverage report", loc_file_str(loc));
      f->valid = false;
   }
   else {
      f->valid = true;

      while (!feof(fp)) {
         char buf[1024];
         if (fgets(buf, sizeof(buf), fp) != NULL)
            cover_append_line(f, buf);
         else if (ferror(fp))
            fatal("error reading %s", loc_file_str(loc));
      }

      fclose(fp);
   }

   return (files = f);
}


static void cover_print_html_header(FILE *f, cover_report_ctx_t *ctx, bool top,
                                    const char *title, ...)
{
   fprintf(f, "<!DOCTYPE html>\n"
              "<html>\n"
              "  <head>\n"
              "  <title>\n");

   va_list ap;
   va_start(ap, title);
   vfprintf(f, title, ap);
   va_end(ap);

   fprintf(f, "  </title>\n"
              "  <style>\n"
              "\n"
              "   header {\n"
              "      padding: 30px;\n"
              "      text-align: center;\n"
              "      font-size: 35px;\n"
              "   }\n"
              "\n"
              "   h2, h3 {\n"
              "      word-wrap: break-word;\n"
              "      width:70%%\n"
              "   }\n"
              "\n"
              "   nav {\n"
              "      float: left;\n"
              "      background-color: #ccc;\n"
              "      width: " SIDEBAR_WIDTH ";\n"
              "      height: 100%%;\n"
              "      padding: 10px;\n"
              "      margin-top: 100px;\n"
              "      word-wrap: break-word;\n"
              "     }\n"
              "\n"
              "     table {\n"
              "        table-layout: fixed;"
              "     }\n"
              "\n"
              "     table, th, td {\n"
              "        border: 2px solid black;\n"
              "        border-collapse: collapse;\n"
              "        word-wrap: break-word;\n"
              "     }\n"
              "\n"
              "     .tabcontent {\n"
              "         display: none;\n"
              "         padding: 6px 12px;\n"
              "         border: 2px solid #ccc;\n"
              "         border-top: none;\n"
              "         word-wrap: break-word;\n"
              "      }\n"
              "\n"
              "      .tab {\n"
              "         overflow: hidden;\n"
              "         border: none;\n"
              "         background-color: none;\n"
              "         margin-left: " MARGIN_LEFT ";\n"
              "         margin-top: 10px;\n"
              "      }\n"
              "\n"
              "      .tab button.active {\n"
              "         background-color: #ccc;\n"
              "      }\n"
              "\n"
              "      .tab button:hover {\n"
              "         background-color: #ddd;\n"
              "      }\n"
              "\n"
              "      .tab button {\n"
              "         background-color: inherit;\n"
              "         float: left;\n"
              "         margin-left: 20px\n"
              "         border: 2px solid black;\n"
              "         cursor: pointer;\n"
              "         padding: 14px 16px;\n"
              "         font-size: 17px;\n"
              "      }\n"
              "\n"
              "  </style>\n"
              "  </head>\n"
              "  <section>\n");

   if (!top) {
      fprintf(f, "<nav>");
      fprintf(f, "   <b>Hierarchy:</b><br>\n");
      int offset = 0;

      ident_t full_hier = ctx->start_tag->hier;
      ident_t curr_id;
      ident_t curr_hier = NULL;
      const char *link = "../coverage_report";
      do {
         curr_id = ident_walk_selected(&full_hier);
         curr_hier = ident_prefix(curr_hier, curr_id, '.');
         if (offset > 0)
            link = istr(curr_hier);
         if (curr_id)
            fprintf(f, "<p style=\"margin-left: %dpx\"><a href=%s.html>%s</a></p>\n",
                        offset * 10, link, istr(curr_id));
         offset++;
      } while (curr_id != NULL);
      fprintf(f, "</nav>");
   }

   fprintf(f, "  <header>\n");
   va_start(ap, title);
   vfprintf(f, title, ap);
   va_end(ap);
   fprintf(f, "  </header>\n");

   fprintf(f, "  <h2 style=\"margin-left: " MARGIN_LEFT ";\">\n");
   if (!top)
      fprintf(f, "     Instance:&nbsp;%s\n", istr(ctx->start_tag->hier));
   else
      fprintf(f, "     Instance:");
   fprintf(f, "  </h2>\n");

   // start_tag has still loc corresponding to a file where hierarchy
   // is instantiated.
   cover_file_t *src = cover_file(&((ctx->start_tag + 1)->loc));
   fprintf(f, "  <h2 style=\"margin-left: " MARGIN_LEFT ";\">\n");
   if (!top)
      fprintf(f, "     File:&nbsp; <a href=\"../../%s\">../../%s</a>\n",
               src->name, src->name);
   else
      fprintf(f, "     File:");
   fprintf(f, "  </h2>\n");

}

static void cover_print_percents_cell(FILE *f, unsigned hit, unsigned total)
{
   if (total > 0) {
      float perc = ((float) hit / (float) total) * 100;
      char color[8];
      if (hit == total)
         checked_sprintf(color, sizeof(color), "#00cc00");
      else if (perc > 90)
         checked_sprintf(color, sizeof(color), "#e6e600");
      else if (perc > 80)
         checked_sprintf(color, sizeof(color), "#ff9900");
      else
         checked_sprintf(color, sizeof(color), "#ff0000");

      fprintf(f, "      <td bgcolor=%s>%.1f %% (%d/%d)</td>\n",
              color, perc, hit, total);
      return;
   }

   fprintf(f, "      <td bgcolor=#aaaaaa>N.A.</td>\n");
}

static void cover_print_hierarchy_header(FILE *f)
{
   fprintf(f, "<table style=\"width:70%%;margin-left:" MARGIN_LEFT ";margin-right:auto;\"> \n"
              "  <tr>\n"
              "     <th bgcolor=#777777 style=\"width:40%%\">Instance</th>\n"
              "     <th bgcolor=#777777 style=\"width:10%%\">Statement</th>\n"
              "     <th bgcolor=#777777 style=\"width:10%%\">Branch</th>\n"
              "     <th bgcolor=#777777 style=\"width:10%%\">Toggle</th>\n"
              "     <th bgcolor=#777777 style=\"width:10%%\">Average</th>\n"
              "  </tr>\n");
}

static void cover_print_hierarchy_footer(FILE *f)
{
   fprintf(f, "</table>\n");
}

static void cover_print_timestamp(FILE *f)
{
   time_t t;
   time(&t);

   fprintf(f, "  </section>\n");

   fprintf(f, "<footer>");
   fprintf(f, "   <p> NVC version: %s </p>\n", PACKAGE_VERSION);
   fprintf(f, "   <p> Generated on: %s </p>\n", ctime(&t));
   fprintf(f,  "</footer>");
}

static void cover_print_hierarchy_summary(FILE *f, cover_stats_t *stats, ident_t hier,
                                          bool top)
{
   fprintf(f, "   <tr>\n"
              "      <td><a href=\"%s%s.html\">%s</a></td>\n",
              top ? "hier/" : "", istr(hier), istr(hier));

   cover_print_percents_cell(f, stats->hit_stmts, stats->total_stmts);
   cover_print_percents_cell(f, stats->hit_branches, stats->total_branches);
   cover_print_percents_cell(f, stats->hit_toggles, stats->total_toggles);

   int avg_total = stats->total_stmts + stats->total_branches + stats->total_toggles;
   int avg_hit = stats->hit_stmts + stats->hit_branches + stats->hit_toggles;
   cover_print_percents_cell(f, avg_hit, avg_total);

   fprintf(f, "   </tr>\n");

   if (top) {
      notef("code coverage results for: %s", istr(hier));

      if (stats->total_stmts > 0)
         notef("     statement:  %.1f %%",
               100.0 * ((double)stats->hit_stmts) / stats->total_stmts);
      else
         notef("     statement:  N.A.");

      if (stats->total_branches > 0)
         notef("     branch:     %.1f %%",
               100.0 * ((double)stats->hit_branches) / stats->total_branches);
      else
         notef("     branch:     N.A.");

      if (stats->total_toggles > 0)
         notef("     toggle:     %.1f %%",
               100.0 * ((double)stats->hit_toggles) / stats->total_toggles);
      else
         notef("     toggle:     N.A.");
   }
}


static void cover_print_chain(FILE *f, cover_chain_t *chn, tag_kind_t kind)
{
   // HTML TAB
   fprintf(f, "<div id=\"");
   if (kind == TAG_STMT)
      fprintf(f, "Statement");
   else if (kind == TAG_BRANCH)
      fprintf(f, "Branch");
   else if (kind == TAG_TOGGLE)
      fprintf(f, "Toggle");
   fprintf(f, "\" class=\"tabcontent\" style=\"width:68.5%%;margin-left:" MARGIN_LEFT "; "
                          "margin-right:auto; margin-top:10px; border: 2px solid black;\">\n");

   for (int i = 0; i < 2; i++) {
      int n;
      cover_pair_t *pair;

      if (i == 0) {
         pair = chn->miss;
         n = chn->n_miss;
      }
      else {
         pair = chn->hits;
         n = chn->n_hits;
      }

      fprintf(f, "   <h3>");
      if (i == 0)
         fprintf(f, "Uncovered ");
      else
         fprintf(f, "Covered ");

      if (kind == TAG_STMT)
         fprintf(f, "statements:");
      else if (kind == TAG_BRANCH)
         fprintf(f, "branches:");
      else if (kind == TAG_TOGGLE)
         fprintf(f, "toggles:");
      fprintf(f, "   </h3>");

      for (int j = 0; j < n; j++) {
         loc_t loc = pair->tag->loc;

         if (kind == TAG_BRANCH || kind == TAG_STMT)
            fprintf(f, "<p><b>Line %d", loc.first_line);

         if (kind == TAG_BRANCH) {
            // True / False flags set for T_IF on tag
            if ((pair->tag->flags & COV_FLAG_HAS_TRUE) &&
                (pair->tag->flags & COV_FLAG_HAS_FALSE))
            {
               fprintf(f, " (Evaluated to ");
               if (pair->flags & COV_FLAG_HAS_TRUE)
                  fprintf(f, "True)");
               else
                  fprintf(f, "False)");
            }
            else
               fprintf(f, " (Choice)");
         }
         if (kind == TAG_BRANCH || kind == TAG_STMT)
            fprintf(f, ":</b><br>");

         // Print line on with the tag, and highlight its location
         if (kind == TAG_BRANCH || kind == TAG_STMT) {
            fprintf(f, "<code>");
            int last = strlen(pair->line->text);
            int curr = 0;
            while (curr <= last) {
               if (curr == loc.first_column)
                  fprintf(f, "<code style=\"background-color:#bbbbbb;\">");
               fprintf(f, "%c", pair->line->text[curr]);
               if (curr == (loc.first_column + loc.column_delta) &&
                   loc.line_delta == 0)
                  fprintf(f, "</code>");
               curr++;
            }
            if (loc.line_delta > 0)
               fprintf(f, "</code>");
         }
         fprintf(f, "</code>");

         // Hier contains also indices of sub-signals
         if (kind == TAG_TOGGLE) {
            fprintf(f, "<b>");
            if (pair->flags & COV_FLAG_TOGGLE_TO_1)
               fprintf(f, "Toggle to 1 &emsp;");
            else if (pair->flags & COV_FLAG_TOGGLE_TO_0)
               fprintf(f, "Toggle to 0 &emsp;");

            fprintf(f, "on ");
            if (pair->tag->flags & COV_FLAG_TOGGLE_SIGNAL)
               fprintf(f, "signal:");
            else if (pair->tag->flags & COV_FLAG_TOGGLE_PORT)
               fprintf(f, "port:&nbsp&nbsp&nbsp");
            fprintf(f, "</b><br>");
            fprintf(f, "<code>%s</code>", istr(ident_rfrom(pair->tag->hier, '.')));
         }

         fprintf(f, "</p>\n");
         pair++;
      }
   }

   fprintf(f, "</div>\n");
}

static void cover_print_hierarchy_guts(FILE *f, cover_report_ctx_t *ctx)
{
   fprintf(f, "<div class=\"tab\">"
              "   <button class=\"tablinks\" onclick=\"selectCoverage(event, 'Statement')\" id=\"defaultOpen\">Statement</button>\n"
              "   <button class=\"tablinks\" style=\"margin-left:10px;\" onclick=\"selectCoverage(event, 'Branch')\">Branch</button>\n"
              "   <button class=\"tablinks\" style=\"margin-left:10px;\" onclick=\"selectCoverage(event, 'Toggle')\">Toggle</button>\n"
              "</div>\n");

   cover_print_chain(f, &(ctx->ch_stmt), TAG_STMT);
   cover_print_chain(f, &(ctx->ch_branch), TAG_BRANCH);
   cover_print_chain(f, &(ctx->ch_toggle), TAG_TOGGLE);

   fprintf(f, "<script>\n"
              "   document.getElementById(\"defaultOpen\").click();"
              "   function selectCoverage(evt, coverageType) {\n"
              "      var i, tabcontent, tablinks;\n"
              "      tabcontent = document.getElementsByClassName(\"tabcontent\");\n"
              "      for (i = 0; i < tabcontent.length; i++) {\n"
              "         tabcontent[i].style.display = \"none\";\n"
              "      }\n"
              "      tablinks = document.getElementsByClassName(\"tablinks\");\n"
              "      for (i = 0; i < tablinks.length; i++) {\n"
              "         tablinks[i].className = tablinks[i].className.replace(\" active\", \"\");\n"
              "      }\n"
              "      document.getElementById(coverageType).style.display = \"block\";\n"
              "      evt.currentTarget.className += \" active\";\n"
              "   }\n"
              "</script>\n");
}

static void cover_append_to_chain(cover_chain_t *chain, bool hits,
                                  cover_tag_t *tag, cover_line_t *line,
                                  unsigned flags)
{
   cover_pair_t *pair;
   int *n;
   int *alloc;

   if (hits) {
      pair = chain->hits;
      n = &(chain->n_hits);
      alloc = &(chain->alloc_hits);
   }
   else {
      pair = chain->miss;
      n = &(chain->n_miss);
      alloc = &(chain->alloc_miss);
   }

   pair[*n].tag = tag;
   pair[*n].line = line;
   pair[*n].flags = flags;
   (*n)++;

   if (*n == *alloc) {
      *alloc = (*alloc) * 2;
      pair = xrealloc_array(pair, *alloc, sizeof(cover_pair_t));
   }
}

static cover_tag_t* cover_report_hierarchy(cover_report_ctx_t *ctx,
                                           const char *dir)
{
   char *hier LOCAL = xasprintf("%s/%s.html", dir, istr(ctx->start_tag->hier));
   cover_tag_t *tag = ctx->start_tag;

   // TODO: Handle escaped identifiers in hierarchy path!
   FILE *f = fopen(hier, "w");
   if (f == NULL)
      fatal("Failed to open file: %s\n", hier);

   ctx->ch_stmt.hits = xcalloc_array(1024, sizeof(cover_pair_t));
   ctx->ch_stmt.miss = xcalloc_array(1024, sizeof(cover_pair_t));
   ctx->ch_stmt.alloc_hits = 1024;
   ctx->ch_stmt.alloc_miss = 1024;

   ctx->ch_branch.hits = xcalloc_array(1024, sizeof(cover_pair_t));
   ctx->ch_branch.miss = xcalloc_array(1024, sizeof(cover_pair_t));
   ctx->ch_branch.alloc_hits = 1024;
   ctx->ch_branch.alloc_miss = 1024;

   ctx->ch_toggle.hits = xcalloc_array(1024, sizeof(cover_pair_t));
   ctx->ch_toggle.miss = xcalloc_array(1024, sizeof(cover_pair_t));
   ctx->ch_toggle.alloc_hits = 1024;
   ctx->ch_toggle.alloc_miss = 1024;

   cover_print_html_header(f, ctx, false, "NVC code coverage report");

   fprintf(f, "  <h3 style=\"margin-left: " MARGIN_LEFT ";\"> Sub-instances: </h3>\n");
   cover_print_hierarchy_header(f);

   for(;;) {
      tag++;

      if (tag->kind == TAG_HIER) {
         if (tag->flags & COV_FLAG_HIER_DOWN) {

            // Collect coverage of sub-block
            cover_report_ctx_t sub_ctx = {0};
            sub_ctx.start_tag = tag;
            sub_ctx.parent = ctx;
            tag = cover_report_hierarchy(&sub_ctx, dir);
            cover_print_hierarchy_summary(f, &(sub_ctx.nested_stats),
                                          tag->hier, false);

            // Add coverage from sub-hierarchies
            ctx->nested_stats.hit_stmts += sub_ctx.nested_stats.hit_stmts;
            ctx->nested_stats.total_stmts += sub_ctx.nested_stats.total_stmts;
            ctx->nested_stats.hit_branches += sub_ctx.nested_stats.hit_branches;
            ctx->nested_stats.total_branches += sub_ctx.nested_stats.total_branches;
            ctx->nested_stats.hit_toggles += sub_ctx.nested_stats.hit_toggles;
            ctx->nested_stats.total_toggles += sub_ctx.nested_stats.total_toggles;

         }
         else if (tag->flags & COV_FLAG_HIER_UP)
            break;

      }
      else {

         cover_file_t *f_src = cover_file(&(tag->loc));
         if (f_src == NULL)
            continue;
         cover_line_t *line = &(f_src->lines[tag->loc.first_line-1]);

         switch (tag->kind){
         case TAG_STMT:
            (ctx->flat_stats.total_stmts)++;
            (ctx->nested_stats.total_stmts)++;

            if (tag->data > 0) {
               (ctx->flat_stats.hit_stmts)++;
               (ctx->nested_stats.hit_stmts)++;
               cover_append_to_chain(&(ctx->ch_stmt), true, tag, line, 0);
            }
            else
               cover_append_to_chain(&(ctx->ch_stmt), false, tag, line, 0);

            break;

         case TAG_BRANCH:
            if (tag->flags & COV_FLAG_HAS_TRUE) {
               (ctx->flat_stats.total_branches)++;
               (ctx->nested_stats.total_branches)++;

               if (tag->data & 0x1) {
                  (ctx->flat_stats.hit_branches)++;
                  (ctx->nested_stats.hit_branches)++;
                  cover_append_to_chain(&(ctx->ch_branch), true, tag,
                                        line, COV_FLAG_HAS_TRUE);
               }
               else
                  cover_append_to_chain(&(ctx->ch_branch), false, tag,
                                        line, COV_FLAG_HAS_TRUE);
            }
            if (tag->flags & COV_FLAG_HAS_FALSE) {
               (ctx->flat_stats.total_branches)++;
               (ctx->nested_stats.total_branches)++;

               if (tag->data & 0x2) {
                  (ctx->flat_stats.hit_branches)++;
                  (ctx->nested_stats.hit_branches)++;

                  cover_append_to_chain(&(ctx->ch_branch), true, tag,
                                        line, COV_FLAG_HAS_FALSE);
               }
               else
                  cover_append_to_chain(&(ctx->ch_branch), false, tag,
                                        line, COV_FLAG_HAS_FALSE);
            }
            break;

         case TAG_TOGGLE:
            (ctx->flat_stats.total_toggles) += 2;
            (ctx->nested_stats.total_toggles) += 2;

            if (tag->data & 0x1) {
               (ctx->flat_stats.hit_toggles)++;
               (ctx->nested_stats.hit_toggles)++;
               cover_append_to_chain(&(ctx->ch_toggle), true, tag,
                                     line, COV_FLAG_TOGGLE_TO_1);
            }
            else
               cover_append_to_chain(&(ctx->ch_toggle), false, tag,
                                     line, COV_FLAG_TOGGLE_TO_1);

            if (tag->data & 0x2) {
               (ctx->flat_stats.hit_toggles)++;
               (ctx->nested_stats.hit_toggles)++;
               cover_append_to_chain(&(ctx->ch_toggle), true, tag,
                                     line, COV_FLAG_TOGGLE_TO_0);
            }
            else
               cover_append_to_chain(&(ctx->ch_toggle), false, tag,
                                     line, COV_FLAG_TOGGLE_TO_0);

            break;

         default:
            fatal("Unsupported type of code coverage:%d !", tag->kind);
         }
      }
   }

   cover_print_hierarchy_footer(f);

   fprintf(f, "  <h3 style=\"margin-left: " MARGIN_LEFT ";\"> Current Instance: </h3>\n");
   cover_print_hierarchy_header(f);
   cover_print_hierarchy_summary(f, &(ctx->flat_stats), tag->hier, false);
   cover_print_hierarchy_footer(f);

   fprintf(f, "  <h3 style=\"margin-left: " MARGIN_LEFT ";\"> Details: </h3>\n");
   cover_print_hierarchy_guts(f, ctx);
   cover_print_timestamp(f);

   fclose(f);
   return tag;
}


void cover_report(const char *path, cover_tagging_t *tagging)
{
   char *subdir LOCAL = xasprintf("%s/hier", path);
   make_dir(path);
   make_dir(subdir);

   assert(tagging->tags.items[0].kind == TAG_HIER);

   cover_report_ctx_t top_ctx = {0};
   top_ctx.start_tag = AREF(tagging->tags, 0);
   cover_report_hierarchy(&top_ctx, subdir);

   char *top LOCAL = xasprintf("%s/coverage_report.html", path);
   FILE *f = fopen(top, "w");

   cover_print_html_header(f, &top_ctx, true, "NVC code coverage report");
   cover_print_hierarchy_header(f);
   cover_print_hierarchy_summary(f, &(top_ctx.nested_stats),
                                 top_ctx.start_tag->hier, true);
   cover_print_hierarchy_footer(f);
   cover_print_timestamp(f);

   fclose(f);
}
