//
//  Copyright (C) 2023  Nick Gasson
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
#include "hash.h"
#include "ident.h"
#include "option.h"

#include <stdlib.h>
#include <string.h>
#include <time.h>

////////////////////////////////////////////////////////////////////////////////
// Cobertura XML export

typedef struct _cobertura_class cobertura_class_t;

typedef struct {
   unsigned lineno;
   unsigned hits;
   bool     branch;
   unsigned bflags;
} cobertura_line_t;

typedef struct _cobertura_class {
   const char        *file;
   ident_t            name;
   cobertura_class_t *next;
   unsigned           nlines;
   unsigned           maxlines;
   cobertura_line_t  *lines;
} cobertura_class_t;

typedef struct {
   hash_t            *class_map;
   cobertura_class_t *classes;
   char              *relative;
} cobertura_report_t;

static cobertura_class_t *cobertura_get_class(cobertura_report_t *report,
                                              ident_t name, const loc_t *loc)
{
   cobertura_class_t *c = hash_get(report->class_map, name);
   if (c == NULL) {
      c = xcalloc(sizeof(cobertura_class_t));
      c->name = name;
      c->file = loc_file_str(loc);
      c->next = report->classes;

      if (is_absolute_path(c->file) && report->relative != NULL) {
         const size_t rlen = strlen(report->relative);
         if (strncmp(report->relative, c->file, rlen) == 0) {
            c->file += rlen;
            while (c->file[0] == DIR_SEP[0] || c->file[0] == '/')
               c->file++;
         }
      }

      report->classes = c;
      hash_put(report->class_map, name, c);
   }

   return c;
}

static cobertura_line_t *cobertura_get_line(cobertura_class_t *class,
                                            const loc_t *loc)
{
   if (class->nlines > 0) {
      cobertura_line_t *last = &(class->lines[class->nlines - 1]);
      if (last->lineno == loc->first_line)  // Most likely
         return last;

      for (int i = 0; i < class->nlines - 1; i++) {
         cobertura_line_t *line = &(class->lines[i]);
         if (line->lineno == loc->first_line)
            return line;
      }
   }

   if (class->nlines == class->maxlines) {
      class->maxlines = MAX(class->maxlines * 2, 100);
      class->lines = xrealloc_array(class->lines, class->maxlines,
                                    sizeof(cobertura_line_t));
   }

   cobertura_line_t *line = &(class->lines[class->nlines++]);
   memset(line, '\0', sizeof(cobertura_line_t));
   line->lineno = loc->first_line;
   return line;
}

static void cobertura_export_scope(cobertura_report_t *report,
                                   cobertura_class_t *class,
                                   cover_scope_t *s)
{
   if (s->block_name != NULL)
      class = cobertura_get_class(report, s->block_name, &s->loc);

   for (int i = 0; i < s->items.count; i++) {
      cover_item_t *t = &(s->items.items[i]);
      switch (t->kind) {
      case COV_ITEM_STMT:
         {
            cobertura_line_t *l = cobertura_get_line(class, &(t->loc));
            l->hits += t->data;
         }
         break;
      case COV_ITEM_BRANCH:
         {
            cobertura_line_t *l = cobertura_get_line(class, &(t->loc));
            l->branch = true;
            l->bflags |= t->data;
         }
         break;
      default:
         break;
      }
   }

   for (list_iter(cover_scope_t *, it, s->children))
      cobertura_export_scope(report, class, it);
}

static void cobertura_class_stats(const cobertura_class_t *class,
                                  int *nlines, int *hitlines,
                                  int *nbranches, int *hitbranches)
{
   *nlines += class->nlines;
   for (int i = 0; i < class->nlines; i++) {
      const cobertura_line_t *line = &(class->lines[i]);
      if (line->hits > 0)
         (*hitlines)++;
      if (line->branch) {
         (*nbranches)++;
         if ((line->bflags & COV_FLAG_TRUE) && (line->bflags & COV_FLAG_FALSE))
             (*hitbranches)++;
      }
   }
}

static void cobertura_print_class(cobertura_class_t *class, FILE *f)
{
   ident_t ename = ident_until(class->name, '-');
   ident_t aname = ident_from(class->name, '-');

   int nlines = 0, hitlines = 0, nbranches = 0, hitbranches = 0;
   cobertura_class_stats(class, &nlines, &hitlines, &nbranches, &hitbranches);

   const double line_rate = (double)hitlines / (double)nlines;
   const double branch_rate = (double)hitbranches / (double)nbranches;

   fprintf(f, "<class name=\"%s(%s)\" filename=\"%s\" "
           "line-rate=\"%f\" branch-rate=\"%f\" complexity=\"0.0\" >\n",
           istr(ename), istr(aname), class->file, line_rate, branch_rate);
   fprintf(f, "<methods/>\n");

   fprintf(f, "<lines>\n");
   for (int i = 0; i < class->nlines; i++) {
      const cobertura_line_t *line = &(class->lines[i]);
      if (line->branch) {
         int pct = 0;
         if (line->bflags & COV_FLAG_TRUE) pct += 50;
         if (line->bflags & COV_FLAG_FALSE) pct += 50;

         fprintf(f, "<line number=\"%d\" hits=\"%d\" branch=\"true\" "
                 "condition-coverage=\"%d %%\">\n",
                 line->lineno, line->hits, pct);
         fprintf(f, "<conditions>\n");
         fprintf(f, "<condition number=\"0\" type=\"jump\" "
                 "coverage=\"%d %%\"/>\n", pct);
         fprintf(f, "</conditions>\n");
         fprintf(f, "</line>\n");
      }
      else
         fprintf(f, "<line number=\"%d\" hits=\"%d\" branch=\"false\"/>\n",
                 line->lineno, line->hits);
   }
   fprintf(f, "</lines>\n");

   fprintf(f, "</class>\n");
}

void cover_export_cobertura(cover_data_t *data, FILE *f, const char *relative)
{
   cobertura_report_t report = {
      .class_map = hash_new(64),
      .relative = relative ? realpath(relative, NULL) : NULL,
   };

   cobertura_export_scope(&report, NULL, data->root_scope);

   fprintf(f, "<?xml version='1.0' encoding='UTF-8'?>\n");
   fprintf(f, "<!DOCTYPE coverage SYSTEM "
           "'http://cobertura.sourceforge.net/xml/coverage-04.dtd'>\n");

   int nlines = 0, hitlines = 0, nbranches = 0, hitbranches = 0;
   for (cobertura_class_t *it = report.classes; it; it = it->next)
      cobertura_class_stats(it, &nlines, &hitlines, &nbranches, &hitbranches);

   const double line_rate = (double)hitlines / (double)nlines;
   const double branch_rate = (double)hitbranches / (double)nbranches;

   unsigned long timestamp;
   const long override_time = opt_get_int(OPT_COVER_TIMESTAMP);
   if (override_time >= 0)
      timestamp = override_time;
   else
      timestamp = time(NULL);

   fprintf(f, "<coverage version=\"" PACKAGE_STRING "\" "
           "line-rate=\"%f\" branch-rate=\"%f\" complexity=\"0.0\" "
           "lines-valid=\"%d\" lines-covered=\"%d\" "
           "branches-valid=\"%d\" branches-covered=\"%d\" "
           "timestamp=\"%lu\">\n",
           line_rate, branch_rate, nlines, hitlines, nbranches, hitbranches,
           timestamp);
   fprintf(f, "<sources>\n");
   fprintf(f, "<source>.</source>\n");
   fprintf(f, "</sources>\n");
   fprintf(f, "<packages>\n");
   fprintf(f, "<package name=\"%s\" "
           "line-rate=\"%f\" branch-rate=\"%f\" complexity=\"0.0\">\n",
           istr(data->root_scope->name), line_rate, branch_rate);

   fprintf(f, "<classes>\n");
   for (cobertura_class_t *it = report.classes; it; it = it->next)
      cobertura_print_class(it, f);
   fprintf(f, "</classes>\n");

   fprintf(f, "</package>\n");
   fprintf(f, "</packages>\n");
   fprintf(f, "</coverage>\n");

   for (cobertura_class_t *it = report.classes, *tmp; it; it = tmp) {
      tmp = it->next;
      free(it->lines);
      free(it);
   }

   free(report.relative);
   hash_free(report.class_map);
}
