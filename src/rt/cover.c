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
#include "diag.h"
#include "fbuf.h"
#include "hash.h"
#include "lib.h"
#include "opt.h"
#include "type.h"

#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>

#define PERCENT_RED    50.0f
#define PERCENT_ORANGE 90.0f

#define COVER_DEBUG


typedef struct _cover_hl cover_hl_t;
typedef struct _cover_file cover_file_t;

typedef enum {
   HL_HIT,
   HL_MISS
} hl_kind_t;

struct _cover_hl {
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

struct _cover_file {
   const char   *name;
   cover_line_t *lines;
   unsigned      n_lines;
   unsigned      alloc_lines;
   bool          valid;
   cover_file_t *next;
};

typedef enum { TAG_STMT, TAG_COND, TAG_LAST } tag_kind_t;

typedef struct {
   tag_kind_t kind;
   int32_t    tag;
   int32_t    sub_cond;
   int32_t    hit_cnt;
   loc_t      loc;
   tree_t     tree;
} cover_tag_t;

typedef A(cover_tag_t) tag_array_t;

struct _cover_tagging {
   int          next_stmt_tag;
   int          next_cond_tag;
   int          next_sub_cond;
   hash_t      *tree_hash;
   tag_array_t  tags;
};

typedef struct {
   unsigned total_branches;
   unsigned hit_branches;
   unsigned total_conds;
   unsigned hit_conds;
   unsigned total_stmts;
   unsigned hit_stmts;
} cover_stats_t;

typedef struct {
   const int32_t   *stmts;
   const int32_t   *conds;
   cover_tagging_t *tagging;
} report_ctx_t;

static cover_file_t  *files;
static cover_stats_t  stats;

static void cover_tag_conditions(tree_t t, cover_tagging_t *ctx, int branch)
{
   const int32_t tag = (branch == -1) ? (ctx->next_cond_tag)++ : branch;

   cover_tag_t new = {
      .kind     = TAG_COND,
      .loc      = 0,
      .tag      = tag,
      .sub_cond = (ctx->next_sub_cond++),
      .hit_cnt  = 0,
      .tree     = t
   };

   APUSH(ctx->tags, new);
   uintptr_t index = ctx->tags.count;
   hash_put(ctx->tree_hash, t, (void *)index);

   if (tree_kind(t) != T_FCALL)
      return;

   // Tag Boolean sub-expressions

   if (!is_builtin(tree_subkind(tree_ref(t))))
      return;

   const int nparams = tree_params(t);
   for (int i = 0; i < nparams; i++) {
      tree_t value = tree_value(tree_param(t, i));
      if (type_ident(tree_type(value)) == well_known(W_STD_BOOL))
         cover_tag_conditions(value, ctx, tag);
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
   cover_tagging_t *ctx = context;

   if (cover_is_stmt(t)) {

      cover_tag_t new = {
         .kind     = TAG_STMT,
         .loc      = 0,
         .tag      = (ctx->next_stmt_tag)++,
         .sub_cond = 0,
         .hit_cnt  = 0,
         .tree     = t
      };
      APUSH(ctx->tags, new);
      uintptr_t index = ctx->tags.count;
      hash_put(ctx->tree_hash, t, (void *)index);

      ctx->next_sub_cond = 0;
      switch (tree_kind(t)) {
      case T_IF:
         {
            const int nconds = tree_conds(t);
            for (int i = 0; i < nconds; i++) {
               tree_t c = tree_cond(t, i);
               if (tree_has_value(c))
                  cover_tag_conditions(tree_value(c), ctx, -1);
            }
         }
         break;

      case T_WHILE:
      case T_NEXT:
      case T_EXIT:
         cover_tag_conditions(tree_value(t), ctx, -1);
         break;

      default:
         break;
      }
   }
}

void cover_print_tags(cover_tagging_t *ctx, bool dump_rt_cnts,
                      int32_t *stmts, int32_t *conds)
{
   printf("Printing cover tags...\n");
   printf("Tag count: %d\n", ctx->tags.count);

   for (int i = 0; i < ctx->tags.count; i++) {
      cover_tag_t *tag = &(ctx->tags.items[i]);
      int32_t hit_cnt;
      if (dump_rt_cnts) {
         hit_cnt = (tag->kind == TAG_STMT) ? stmts[tag->tag] : conds[tag->tag];
      } else {
         hit_cnt = tag->hit_cnt;
      }
      
      printf("Index: %4d  Tag: %4d  Kind: %d  Subcond: %4d  Hit Count: %4d Tree: %p\n", i,
               tag->tag, tag->kind, tag->sub_cond, hit_cnt, (void *) tag->tree);
   }
}

void cover_dump_tags(cover_tagging_t *ctx, tree_t top, bool dump_rt_cnts,
                     int32_t *stmts, int32_t *conds)
{
   char *dbname LOCAL = xasprintf("_%s.covdb", istr(tree_ident(top)));
   fbuf_t *f = lib_fbuf_open(lib_work(), dbname, FBUF_OUT, FBUF_CS_NONE);
   if (f == NULL)
      fatal_errno("failed to create coverage db file: %s", dbname);

   write_u32(ctx->next_stmt_tag, f);
   write_u32(ctx->next_cond_tag, f);

   loc_wr_ctx_t *loc_wr = loc_write_begin(f);

   for (int i = 0; i < ctx->tags.count; i++) {
      cover_tag_t *tag = &(ctx->tags.items[i]);

      write_u8(tag->kind, f);
      write_u32(tag->tag, f);

      if (tag->kind == TAG_COND)
         write_u32(tag->sub_cond, f);

      if (dump_rt_cnts) {
         const int32_t *cnts = (tag->kind == TAG_STMT) ? stmts : conds;
         write_u32(cnts[i], f);
      } else {
         write_u32(0, f);
      }

      loc_write(tree_loc(tag->tree), loc_wr);
   }

   write_u8(TAG_LAST, f);

   loc_write_end(loc_wr);
   fbuf_close(f, NULL);
}

cover_tagging_t *cover_tag(tree_t top)
{
   cover_tagging_t *ctx = xcalloc(sizeof(cover_tagging_t));
   ctx->tree_hash = hash_new(1024);

   tree_visit(top, cover_tag_visit_fn, ctx);

   if (opt_get_int(OPT_UNIT_TEST))
      return ctx;

   cover_dump_tags(ctx, top, false, NULL, NULL);

#ifdef COVER_DEBUG
   cover_print_tags(ctx, false, NULL, NULL);
#endif

   return ctx;
}

cover_tagging_t *cover_read_tags(tree_t top)
{
   char *dbname LOCAL = xasprintf("_%s.covdb", istr(tree_ident(top)));
   fbuf_t *f = lib_fbuf_open(lib_work(), dbname, FBUF_IN, FBUF_CS_NONE);
   if (f == NULL)
      return NULL;

   cover_tagging_t *tagging = xcalloc(sizeof(cover_tagging_t));

   tagging->next_stmt_tag = read_u32(f);
   tagging->next_cond_tag = read_u32(f);

   loc_rd_ctx_t *loc_rd = loc_read_begin(f);

   for (;;) {
      const tag_kind_t kind = read_u8(f);
      if (kind == TAG_LAST)
         break;

      const int32_t tag = read_u32(f);
      const int32_t sub_cond = kind == TAG_COND ? read_u32(f) : 0;
      const int32_t hit_cnt = read_u32(f);

      loc_t loc;
      loc_read(&loc, loc_rd);

      cover_tag_t new = {
         .kind     = kind,
         .loc      = loc,
         .tag      = tag,
         .sub_cond = sub_cond,
         .hit_cnt  = hit_cnt,
         .tree     = NULL
      };
      APUSH(tagging->tags, new);
   }

   loc_read_end(loc_rd);
   fbuf_close(f, NULL);
   return tagging;
}

void cover_count_tags(cover_tagging_t *tagging, int32_t *n_stmts,
                      int32_t *n_conds)
{
   if (tagging == NULL) {
      *n_stmts = 0;
      *n_conds = 0;
   }
   else {
      *n_stmts = tagging->next_stmt_tag;
      *n_conds = tagging->next_cond_tag;
   }
}

bool cover_is_tagged(cover_tagging_t *tagging, tree_t t,
                     int32_t *tag, int32_t *sub_cond)
{
   if (tagging == NULL)
      return false;

#ifdef COVER_DEBUG
   printf("Checking tree %p :", (void *)t);
#endif

   // Push to array is post-insert so has index is offset by 0,
   // thus:
   //    0 -> no tag
   //    1 -> Index 0
   //    n -> Index n - 
   intptr_t index = (intptr_t)hash_get(tagging->tree_hash, t);
   if (index == 0) {
#ifdef COVER_DEBUG
      printf("No cover tag\n");
#endif
      return false;
   }

#ifdef COVER_DEBUG
   printf("Index: %3ld  Tag: %3d  Kind: %d  Subcond: %3d,\n", index - 1,
            tagging->tags.items[index - 1].tag,
            tagging->tags.items[index - 1].kind,
            tagging->tags.items[index - 1].sub_cond);
#endif

   if (sub_cond)
      *sub_cond = tagging->tags.items[index - 1].sub_cond;
   if (tag)
      *tag = tagging->tags.items[index - 1].tag;

   return true;
}

static void cover_append_line(cover_file_t *f, const char *buf)
{
   if (f->n_lines == f->alloc_lines) {
      f->alloc_lines *= 2;
      f->lines = xrealloc_array(f->lines, f->alloc_lines, sizeof(cover_line_t));
   }

   cover_line_t *l = &(f->lines[(f->n_lines)++]);
   l->text = xstrdup(buf);
   l->len  = strlen(buf);
   l->hits = -1;
   l->hl   = NULL;
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

static void cover_process_tag(cover_tag_t *tag, const int32_t *stmts,
                              const int32_t *conds)
{
   cover_file_t *file = cover_file(&(tag->loc));
   if (file == NULL || !file->valid || tag->loc.first_line == 0)
      return;

   assert(tag->loc.first_line < file->n_lines);

   cover_line_t *l = &(file->lines[tag->loc.first_line - 1]);

   if (tag->kind == TAG_STMT) {
      l->hits = MAX(stmts[tag->tag], l->hits);

      if (stmts[tag->tag] > 0)
         stats.hit_stmts++;

      stats.total_stmts++;
   }
   else {
      const int start = tag->loc.first_column;
      const int end = (tag->loc.line_delta == 0)
         ? tag->loc.first_column + tag->loc.column_delta : l->len;

      const int mask = (conds[tag->tag] >> (tag->sub_cond * 2)) & 3;

      if (tag->sub_cond == 0) {
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
   checked_sprintf(buf, sizeof(buf) - 6, "report_%s.html", f->name);
   for (char *p = buf; *(p + 5) != '\0'; p++) {
      if (*p == DIR_SEP[0] || *p == '.')
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

    fprintf(fp, "  </title>\n  <style type=\"text/css\">\n");
    fprintf(fp,
            "table.code { font-family: monospace; border-spacing: 0; }\n"
            "td.hit { background: #c0ffc0; }\n"
            "td.miss { background: #ffc0c0; }\n"
            "div.nav { width: 250px; height: 100%%;\n"
            "          box-shadow: 0px 1px 50px #5E5E5E; position: fixed;\n"
            "          top: 0px; left: 0px; background: #f0f040;\n"
            "          font-family: sans-serif; padding: 0px;\n"
            "          overflow: auto; }\n"
            "div.nav ul { list-style-type: none; margin: 0;\n"
            "             margin-left: 20px; padding: 0; }\n"
            "div.nav h3 { font-size: 14pt; padding-left: 10px; }\n"
            "div.nav a:link { text-decoration: none; }\n"
            "div.advert { bottom: 0; margin-left: auto; margin-right: auto; }\n"
            "div.advert p { text-align: center; }\n"
            "div.advert a:link { text-decoration: none; }\n"
            "#body { margin: 0px auto; margin-left: 260px;\n"
            "        font-family: sans-serif; }\n"
            "#body h1 { font-size: 16pt; text-align: center; }\n"
            "#body h2 { font-size: 14pt; text-align: center; }\n"
            "span.hl_miss { background: #ff6060; }\n"
            "span.hl_hit { background: #60ff60; }\n"
            "table.stats { border-collapse: collapse; margin-left: auto;\n"
            "              margin-right: auto; }\n"
            "table.stats td { padding-right: 10px; }\n"
            "table.stats th { border-bottom: 1px solid black;\n"
            "                 padding-right: 10px; font-size: 12pt; }\n"
            "table.stats td.num { text-align: center; }\n"
            "span.pct_green { color: #339933; }\n"
            "span.pct_orange { color: #ff8800; }\n"
            "span.pct_red { color: #cc0000; }\n");
    fprintf(fp, "  </style>\n</head>\n<body>\n");

    fprintf(fp, "<div class=\"nav\">\n");
    fprintf(fp, "<h3>Reports</h3>\n");
    fprintf(fp, "<ul>\n");
    fprintf(fp, "  <li><a href=\"index.html\">Index</a></li>\n");
    fprintf(fp, "</ul>\n");
    fprintf(fp, "<h3>Files</h3>\n");
    fprintf(fp, "<ul class=\"nav\">\n");
    for (cover_file_t *f = files; f != NULL; f = f->next)
      fprintf(fp, "  <li><a href=\"%s\">%s</a></li>\n",
              cover_file_url(f), f->name);
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
   char *buf LOCAL = xasprintf("%s" DIR_SEP "%s", dir, cover_file_url(f));
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
           "<br/><a href=\""PACKAGE_URL"\">"
           PACKAGE_URL"</a></p></div>\n",
           PACKAGE_STRING);

   cover_html_footer(fp);

   fclose(fp);
}

void cover_report(tree_t top, cover_tagging_t *tagging,
                  const int32_t *stmts, const int32_t *conds)
{
   for (unsigned i = 0; i < tagging->tags.count; i++)
      cover_process_tag(&(tagging->tags.items[i]), stmts, conds);

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
