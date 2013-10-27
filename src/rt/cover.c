//
//  Copyright (C) 2013  Nick Gasson
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
#include <string.h>
#include <limits.h>

#if 0
#define CSS_DIR "/home/nick/nvc/data/"
#else
#define CSS_DIR DATADIR
#endif

typedef struct cover_hl cover_hl_t;

struct cover_hl {
   cover_hl_t *next;
   int         start;
   int         end;
   const char *colour;
};

typedef struct {
   char *text;
   int   hits;
} cover_line_t;

typedef struct cover_file {
   const char        *name;
   cover_line_t      *lines;
   unsigned           n_lines;
   unsigned           alloc_lines;
   bool               valid;
   struct cover_file *next;
} cover_file_t;

typedef struct {
   int next_stmt_tag;
   int next_cond_tag;
} cover_tag_ctx_t;

static ident_t       stmt_tag_i;
static ident_t       cond_tag_i;
static cover_file_t *files;

static void cover_tag_visit_fn(tree_t t, void *context)
{
   cover_tag_ctx_t *ctx = context;

   switch (tree_kind(t)) {
   case T_IF:
   case T_WHILE:
   case T_NEXT:
   case T_EXIT:
      if (tree_has_value(t))
         tree_add_attr_int(tree_value(t), cond_tag_i, (ctx->next_cond_tag)++);
      // Fall-through
   case T_SIGNAL_ASSIGN:
   case T_ASSERT:
   case T_VAR_ASSIGN:
   case T_WAIT:
   case T_RETURN:
   case T_CASE:
      tree_add_attr_int(t, stmt_tag_i, (ctx->next_stmt_tag)++);
      break;

   default:
      break;
   }
}

void cover_tag(tree_t top)
{
   stmt_tag_i = ident_new("stmt_tag");
   cond_tag_i = ident_new("cond_tag");

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
   l->text = strdup(buf);
   l->hits = -1;
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

   FILE *fp = fopen(loc->file, "r");

   if (fp == NULL) {
      // Guess the path is relative to the work library
      char path[PATH_MAX];
      snprintf(path, PATH_MAX, "%s/../%s", lib_path(lib_work()), loc->file);
      fp = fopen(path, "r");
   }

   if (fp == NULL) {
      warnf("failed to open %s for coverage report", loc->file);
      f->valid = false;
   }
   else {
      f->valid = true;

      while (!feof(fp)) {
         char buf[1024];
         fgets(buf, sizeof(buf), fp);
         cover_append_line(f, buf);
      }

      fclose(fp);
   }

   return (files = f);
}

static void cover_report_stmts_fn(tree_t t, void *context)
{
   const int32_t *counts = context;

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
}

static void cover_report_conds_fn(tree_t t, void *context)
{
   const int32_t *masks = context;

   const int tag = tree_attr_int(t, cond_tag_i, -1);
   if (tag == -1)
      return;

   const loc_t *loc = tree_loc(t);
   cover_file_t *file = cover_file(loc);
   if ((file == NULL) || !file->valid)
      return;

   assert(loc->first_line < file->n_lines);

   //cover_line_t *l = &(file->lines[loc->first_line - 1]);

   printf("%s %d mask %x\n", loc->file, loc->first_line, masks[tag]);
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

   for (const char *p = l->text; *p != '\0'; p++) {
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
   snprintf(buf, sizeof(buf) - 6, "report_%s", f->name);
   for (char *p = buf; *p != '\0'; p++) {
      if (*p == '/' || *p == '.')
         *p = '_';
   }
   strcat(buf, ".html");
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
   char buf[256];
   snprintf(buf, sizeof(buf), "%s/%s", dir, cover_file_url(f));

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

static void cover_index(ident_t name, const char *dir)
{
   char buf[256];
   snprintf(buf, sizeof(buf), "%s/index.html", dir);

   FILE *fp = lib_fopen(lib_work(), buf, "w");
   if (fp == NULL)
      fatal("failed to create %s", buf);

   cover_html_header(fp, "Coverage report for %s", istr(name));

   fprintf(fp, "<h1>Coverage report for %s</h1>\n", istr(name));

   fprintf(fp, "<div class=\"help\"><p>Select a file from the sidebar to see "
           "annotated statement and branch coverage.</p></div>\n");
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

   tree_visit(top, cover_report_stmts_fn, (void *)stmts);

   if (conds != NULL)
      tree_visit(top, cover_report_conds_fn, (void *)conds);

   ident_t name = ident_strip(tree_ident(top), ident_new(".elab"));

   char dir[256];
   snprintf(dir, sizeof(dir), "%s.cover", istr(name));

   lib_t work = lib_work();
   lib_mkdir(work, dir);

   for (cover_file_t *f = files; f != NULL; f = f->next)
      cover_report_file(f, dir);

   cover_index(name, dir);

   char output[PATH_MAX];
   lib_realpath(work, dir, output, sizeof(output));
   notef("coverage report generated in %s/", output);
}
