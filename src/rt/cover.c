//
//  Copyright (C) 2013-2018  Nick Gasson
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
#include "cover.h"

#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>

#if 0
#define CSS_DIR "/home/nick/nvc/data/"
#else
#define CSS_DIR DATADIR
#endif

#define PERCENT_RED    50.0f
#define PERCENT_ORANGE 90.0f

typedef struct cover_hl cover_hl_t;
typedef struct cover_file cover_file_t;

typedef enum {
   HL_HIT,
   HL_MISS
} hl_kind_t;

struct cover_hl {
   cover_hl_t *next;
   int         start;
   int         end;
   hl_kind_t   kind;
   const char *help;
};

typedef struct {
   char       *text;
   size_t      len;
   int         hits;
   cover_hl_t *hl;
} cover_line_t;

struct cover_file {
   ident_t       name;
   cover_line_t *lines;
   unsigned      n_lines;
   unsigned      alloc_lines;
   bool          valid;
   cover_file_t *next;
};

typedef struct {
   int next_stmt_tag;
   int next_cond_tag;
   int next_sub_cond;
} cover_tag_ctx_t;

typedef struct {
   unsigned total_branches;
   unsigned hit_branches;
   unsigned total_conds;
   unsigned hit_conds;
   unsigned total_stmts;
   unsigned hit_stmts;
} cover_stats_t;

typedef struct {
   const int32_t *stmts;
   const int32_t *conds;
} report_ctx_t;

static ident_t       stmt_tag_i;
static ident_t       cond_tag_i;
static ident_t       sub_cond_i;
static ident_t       builtin_i;
static ident_t       std_bool_i;
static cover_file_t *files;
static cover_stats_t stats;

static void cover_tag_conditions(tree_t t, cover_tag_ctx_t *ctx, int branch)
{
   const int tag = (branch == -1) ? (ctx->next_cond_tag)++ : branch;

   if (ctx->next_sub_cond == 16)
      return;

   tree_add_attr_int(t, cond_tag_i, tag);
   tree_add_attr_int(t, sub_cond_i, (ctx->next_sub_cond)++);

   if (tree_kind(t) != T_FCALL)
      return;

   // Tag Boolean sub-expressions

   if (tree_attr_str(tree_ref(t), builtin_i) == NULL)
      return;

   const int nparams = tree_params(t);
   for (int i = 0; i < nparams; i++) {
      tree_t value = tree_value(tree_param(t, i));
      if (type_ident(tree_type(value)) == std_bool_i)
         cover_tag_conditions(value, ctx, tag);
   }
}

static bool cover_has_conditions(tree_t t)
{
   switch (tree_kind(t)) {
   case T_IF:
   case T_WHILE:
   case T_NEXT:
   case T_EXIT:
      return tree_has_value(t);
   default:
      return false;
   }
}

static bool cover_is_stmt(tree_t t)
{
   switch (tree_kind(t)) {
   case T_IF:
   case T_WHILE:
   case T_NEXT:
   case T_EXIT:
   case T_SIGNAL_ASSIGN:
   case T_ASSERT:
   case T_VAR_ASSIGN:
   case T_WAIT:
   case T_RETURN:
   case T_CASE:
      return true;

   default:
      return false;
   }
}

static void cover_tag_visit_fn(tree_t t, void *context)
{
   cover_tag_ctx_t *ctx = context;

   if (cover_is_stmt(t)) {
      tree_add_attr_int(t, stmt_tag_i, (ctx->next_stmt_tag)++);

      if (cover_has_conditions(t)) {
         ctx->next_sub_cond = 0;
         cover_tag_conditions(tree_value(t), ctx, -1);
      }
   }
}

void cover_tag(tree_t top)
{
   stmt_tag_i = ident_new("stmt_tag");
   cond_tag_i = ident_new("cond_tag");
   sub_cond_i = ident_new("sub_cond");
   std_bool_i = ident_new("STD.STANDARD.BOOLEAN");
   builtin_i  = ident_new("builtin");

   cover_tag_ctx_t ctx = {
      .next_stmt_tag = 0,
      .next_cond_tag = 0
   };

   tree_visit(top, cover_tag_visit_fn, &ctx);

   tree_add_attr_int(top, ident_new("stmt_tags"), ctx.next_stmt_tag);
   tree_add_attr_int(top, ident_new("cond_tags"), ctx.next_cond_tag);
}

static void cover_append_line(cover_file_t *f, const char *buf)
{
   if (f->n_lines == f->alloc_lines) {
      f->alloc_lines *= 2;
      f->lines = xrealloc(f->lines, f->alloc_lines * sizeof(cover_line_t));
   }

   cover_line_t *l = &(f->lines[(f->n_lines)++]);
   l->text = xstrdup(buf);
   l->len  = strlen(buf);
   l->hits = -1;
   l->hl   = NULL;
}

static cover_file_t *cover_file(const loc_t *loc)
{
   if (loc->file == NULL)
      return NULL;

   cover_file_t *f;
   for (f = files; f != NULL; f = f->next) {
      // Comparing pointers directly here is OK since only one copy
      // of the file name string will be created by tree_read
      if (f->name == loc->file)
         return f->valid ? f : NULL;
   }

   f = xmalloc(sizeof(cover_file_t));
   f->name        = loc->file;
   f->n_lines     = 0;
   f->alloc_lines = 1024;
   f->lines       = xmalloc(sizeof(cover_line_t) * f->alloc_lines);
   f->next        = files;

   FILE *fp = fopen(istr(loc->file), "r");

   if (fp == NULL) {
      // Guess the path is relative to the work library
      char *path LOCAL =
         xasprintf("%s/../%s", lib_path(lib_work()), istr(loc->file));
      fp = fopen(path, "r");
   }

   if (fp == NULL) {
      warnf("failed to open %s for coverage report", istr(loc->file));
      f->valid = false;
   }
   else {
      f->valid = true;

      while (!feof(fp)) {
         char buf[1024];
         if (fgets(buf, sizeof(buf), fp) != NULL)
            cover_append_line(f, buf);
         else if (ferror(fp))
            fatal("error reading %s", istr(loc->file));
      }

      fclose(fp);
   }

   return (files = f);
}

static void cover_report_conds(tree_t t, report_ctx_t *ctx)
{
   const int32_t *masks = ctx->conds;

   const int tag = tree_attr_int(t, cond_tag_i, -1);
   if ((tag == -1) || (masks[tag] == 0))
      return;

   const loc_t *loc = tree_loc(t);
   cover_file_t *file = cover_file(loc);
   if ((file == NULL) || !file->valid)
      return;

   assert(loc->first_line < file->n_lines);

   cover_line_t *l = &(file->lines[loc->first_line - 1]);

   const int start = loc->first_column;
   const int end = (loc->last_line == loc->first_line)
      ? loc->last_column : l->len;

   const int sub_cond = tree_attr_int(t, sub_cond_i, 0);
   const int mask = (masks[tag] >> (sub_cond * 2)) & 3;

   if (sub_cond == 0) {
      stats.total_branches++;
      if (mask == 3)
         stats.hit_branches++;
   }

   stats.total_conds++;
   if (mask == 3)
      stats.hit_conds++;

   cover_hl_t *hl;
   for (hl = l->hl; hl != NULL; hl = hl->next) {
      if ((hl->start == start) && (hl->end == end))
         break;
   }

   if (hl == NULL) {
      hl = xmalloc(sizeof(cover_hl_t));
      hl->start  = start;
      hl->end    = end;
      hl->next   = l->hl;

      l->hl = hl;
   }
   else if (hl->kind == HL_HIT)
      return;

   hl->kind = (mask == 3) ? HL_HIT : HL_MISS;
   hl->help = NULL;

   if (mask == 1)
      hl->help = "Condition never evaluated to TRUE";
   else if (mask == 2)
      hl->help = "Condition never evaluated to FALSE";
}

static void cover_report_fn(tree_t t, void *context)
{
   report_ctx_t *ctx = context;
   const int32_t *counts = ctx->stmts;

   if (!cover_is_stmt(t))
      return;

   if (cover_has_conditions(t))
      cover_report_conds(tree_value(t), ctx);

   const int tag = tree_attr_int(t, stmt_tag_i, -1);
   if (tag == -1)
      return;

   const loc_t *loc = tree_loc(t);
   cover_file_t *file = cover_file(loc);
   if ((file == NULL) || !file->valid)
      return;

   assert(loc->first_line < file->n_lines);

   cover_line_t *l = &(file->lines[loc->first_line - 1]);
   l->hits = MAX(counts[tag], l->hits);

   if (counts[tag] > 0)
      stats.hit_stmts++;

   stats.total_stmts++;
}

static void cover_report_line(FILE *fp, cover_line_t *l)
{
   fprintf(fp, "<tr>");

   if (l->hits != -1) {
      fprintf(fp, "<td>%d</td>", l->hits);
      fprintf(fp, "<td class=\"%s\">", (l->hits > 0) ? "hit" : "miss");
   }
   else {
      fprintf(fp, "<td></td>");
      fprintf(fp, "<td>");
   }

   int col = 0;
   for (const char *p = l->text; *p != '\0'; p++, col++) {
      for (cover_hl_t *it = l->hl; it != NULL; it = it->next) {
         if (it->start == col) {
            fprintf(fp, "<span class=\"hl_%s\"",
                    (it->kind == HL_HIT) ? "hit" : "miss");
            if (it->help != NULL)
               fprintf(fp, " title=\"%s\"", it->help);
            fprintf(fp, ">");
         }
      }

      switch (*p) {
      case ' ':
         fprintf(fp, "&nbsp;");
         break;
      case '\t':
         {
            int col = (p - l->text);
            while (++col % 8)
               fprintf(fp, "&nbsp;");
         }
         break;
      case '<':
         fprintf(fp, "&lt;");
         break;
      case '>':
         fprintf(fp, "&gt;");
         break;
      default:
         fputc(*p, fp);
         break;
      }

      for (cover_hl_t *it = l->hl; it != NULL; it = it->next) {
         if (it->end == col)
            fprintf(fp, "</span>");
      }
   }

   switch (*(l->text)) {
   case '\n':
   case '\r':
   case '\0':
      fprintf(fp, "&nbsp;");   // Equal height for empty lines
      break;
   }

   fprintf(fp, "</td></tr>\n");
}

static const char *cover_file_url(cover_file_t *f)
{
   static char buf[256];
   checked_sprintf(buf, sizeof(buf) - 6, "report_%s.html", istr(f->name));
   for (char *p = buf; *(p + 5) != '\0'; p++) {
      if (*p == PATH_SEP[0] || *p == '.')
         *p = '_';
   }
   return buf;
}

static void cover_html_header(FILE *fp, const char *title, ...)
{
    fprintf(fp,
            "<!DOCTYPE html PUBLIC \"-//W3C//DTD HTML 4.01//EN\">\n"
            "<html>\n"
            "<head>\n"
            "  <meta http-equiv=\"Content-type\" \n"
            "        content=\"text/html;charset=UTF-8\">\n"
            "  <title>");

    va_list ap;
    va_start(ap, title);
    vfprintf(fp, title, ap);
    va_end(ap);

    fprintf(fp,
            "</title>\n"
            "  <link rel=\"stylesheet\" href=\""CSS_DIR"/coverage.css\">\n"
            "</head>\n"
            "<body>\n");

    fprintf(fp, "<div class=\"nav\">\n");
    fprintf(fp, "<h3>Reports</h3>\n");
    fprintf(fp, "<ul>\n");
    fprintf(fp, "  <li><a href=\"index.html\">Index</a></li>\n");
    fprintf(fp, "</ul>\n");
    fprintf(fp, "<h3>Files</h3>\n");
    fprintf(fp, "<ul class=\"nav\">\n");
    for (cover_file_t *f = files; f != NULL; f = f->next)
      fprintf(fp, "  <li><a href=\"%s\">%s</a></li>\n",
              cover_file_url(f), istr(f->name));
    fprintf(fp, "</ul>\n");
    fprintf(fp, "</div>\n");


    fprintf(fp, "<div id=\"body\">\n");
}

static void cover_html_footer(FILE *fp)
{
   fprintf(fp, "</div></body>\n</html>\n");
}

static void cover_report_file(cover_file_t *f, const char *dir)
{
   char *buf LOCAL = xasprintf("%s" PATH_SEP "%s", dir, cover_file_url(f));
   FILE *fp = lib_fopen(lib_work(), buf, "w");
   if (fp == NULL)
      fatal("failed to create %s", buf);

   cover_html_header(fp, "Coverage report for %s", f->name);

   fprintf(fp, "<table class=\"code\">\n");
   for (int i = 0; i < f->n_lines; i++) {
      cover_line_t *l = &(f->lines[i]);
      cover_report_line(fp, l);
   }
   fprintf(fp, "</table>\n");

   cover_html_footer(fp);

   fclose(fp);
}

static const char *cover_percent(unsigned x, unsigned y)
{
   const float pct = ((float)x / (float)y) * 100.0f;

   static char buf[256];
   snprintf(buf, sizeof(buf), "<span class=\"pct_%s\">%.0f%%</span>",
            (pct < PERCENT_RED) ? "red"
            : ((pct < PERCENT_ORANGE) ? "orange" : "green"), pct);

   return buf;
}

static void cover_stat_line(FILE *fp, const char *text,
                            unsigned hit, unsigned total)
{
   if (total > 0) {
      fprintf(fp, "<tr><td>%s</td><td class=\"num\">%u</td>"
              "<td class=\"num\">%u</td><td class=\"num\">%s</td></tr>\n",
              text, hit, total, cover_percent(hit, total));
   }
}

static void cover_index(ident_t name, const char *dir)
{
   char *buf = xasprintf("%s/index.html", dir);
   FILE *fp = lib_fopen(lib_work(), buf, "w");
   if (fp == NULL)
      fatal("failed to create %s", buf);
   free(buf);

   cover_html_header(fp, "Coverage report for %s", istr(name));

   fprintf(fp, "<h1>Coverage report for %s</h1>\n", istr(name));
   fprintf(fp, "<div class=\"help\"><p>Select a file from the sidebar to see "
           "annotated statement and condition coverage.</p></div>\n");

   fprintf(fp, "<h2>Overall Statistics</h2>\n");
   fprintf(fp, "<table class=\"stats\">\n");
   fprintf(fp, "<tr><th>Metric</th><th>Covered</th><th>Total</th>"
           "<th>Percentage</th></tr>\n");
   cover_stat_line(fp, "Statements executed",
                   stats.hit_stmts, stats.total_stmts);
   cover_stat_line(fp, "Branches observed taken and not taken",
                   stats.hit_branches, stats.total_branches);
   cover_stat_line(fp, "Conditions evaluated to both TRUE and FALSE",
                   stats.hit_conds, stats.total_conds);
   fprintf(fp, "</table>\n");

   fprintf(fp, "<div class=\"advert\"><p>Generated by %s\n"
           "<br/><a href=\"https://github.com/nickg/nvc\">"
           "github.com/nickg/nvc</a></p></div>\n",
           PACKAGE_STRING);

   cover_html_footer(fp);

   fclose(fp);
}

void cover_report(tree_t top, const int32_t *stmts, const int32_t *conds)
{
   stmt_tag_i = ident_new("stmt_tag");
   cond_tag_i = ident_new("cond_tag");
   sub_cond_i = ident_new("sub_cond");

   report_ctx_t report_ctx = {
      .stmts = stmts,
      .conds = conds
   };

   tree_visit(top, cover_report_fn, &report_ctx);

   ident_t name = ident_strip(tree_ident(top), ident_new(".elab"));

   char *dir LOCAL = xasprintf("%s.cover", istr(name));

   lib_t work = lib_work();
   lib_mkdir(work, dir);

   for (cover_file_t *f = files; f != NULL; f = f->next)
      cover_report_file(f, dir);

   cover_index(name, dir);

   char output[PATH_MAX];
   lib_realpath(work, dir, output, sizeof(output));

   char *buf LOCAL = xasprintf(
            "coverage report generated in %s/\n"
            "  %u/%u statements covered\n"
            "  %u/%u branches covered\n"
            "  %u/%u conditions covered",
            output,
            stats.hit_stmts, stats.total_stmts,
            stats.hit_branches, stats.total_branches,
            stats.hit_conds, stats.total_conds);
   notef("%s", buf);
}
