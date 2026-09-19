//
//  Copyright (C) 2023-2026  Nick Gasson
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
#include "hash.h"
#include "ident.h"
#include "option.h"

#include <assert.h>
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
   char              *file;
   ident_t            name;
   cobertura_class_t *next;
   unsigned           nlines;
   unsigned           maxlines;
   cobertura_line_t  *lines;
} cobertura_class_t;

typedef struct {
   hash_t             *class_map;
   cobertura_class_t  *classes;
   const cover_data_t *data;
   const char         *relative;
} cobertura_report_t;

static cobertura_class_t *cobertura_get_class(cobertura_report_t *report,
                                              cover_obj_t scope)
{
   cover_obj_t inst = cover_get_obj(report->data, scope, COV_ATTR_INST);
   ident_t block_name = cover_get_ident(report->data, inst,
                                        COV_ATTR_BLOCK_NAME);

   cobertura_class_t *c = hash_get(report->class_map, block_name);
   if (c != NULL)
      return c;

   cover_obj_t item0 = cover_at(report->data, scope, COV_REL_ITEMS, 0);
   cover_obj_t child0 = cover_at(report->data, scope, COV_REL_CHILDREN, 0);

   // For instance scopes location will be in the instantiating file
   loc_t loc;
   if (!cover_is_null(item0))
      loc = cover_get_loc(report->data, item0, COV_ATTR_LOC);
   else if (!cover_is_null(child0))
      loc = cover_get_loc(report->data, child0, COV_ATTR_LOC);
   else
      loc = cover_get_loc(report->data, scope, COV_ATTR_LOC);

   LOCAL_TEXT_BUF tb = tb_new();
   get_relative_path(tb, report->relative, loc_file_str(&loc));

   c = xcalloc(sizeof(cobertura_class_t));
   c->name = block_name;
   c->file = tb_claim(tb);
   c->next = report->classes;

   report->classes = c;
   hash_put(report->class_map, block_name, c);
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
                                   cobertura_class_t *class, cover_obj_t scope)
{
   cover_iter_t item_it = cover_begin(report->data, scope, COV_REL_ITEMS);
   cover_obj_t item;
   while (cover_next(&item_it, &item)) {
      loc_t loc = cover_get_loc(report->data, item, COV_ATTR_LOC);
      cobertura_line_t *l = cobertura_get_line(class, &loc);

      switch (cover_get_kind(report->data, item)) {
      case COV_ITEM_STMT:
         {
            cover_obj_t bin = cover_at(report->data, item, COV_REL_BINS, 0);
            l->hits += cover_get_u32(report->data, bin, COV_ATTR_DATA, 0);
         }
         break;
      case COV_ITEM_BRANCH:
         {
            l->branch = true;

            cover_obj_t bins[2];
            int nbins = cover_rel(report->data, item, COV_REL_BINS, 0, bins, 2);
            assert(nbins <= ARRAY_LEN(bins));

            for (int j = 0; j < nbins; j++) {
               uint32_t data = cover_get_u32(report->data, bins[j],
                                             COV_ATTR_DATA, 0);
               if (data > 0)
                  l->bflags |= cover_get_u32(report->data, bins[j],
                                             COV_ATTR_FLAGS, 0);
            }
         }
         break;
      default:
         break;
      }
   }

   cover_obj_t inst = cover_get_obj(report->data, scope, COV_ATTR_INST);

   cover_iter_t child_it = cover_begin(report->data, scope, COV_REL_CHILDREN);
   cover_obj_t child;
   while (cover_next(&child_it, &child)) {
      // TODO: remove this check
      if (cover_equals(cover_get_obj(report->data, child, COV_ATTR_INST), inst))
         cobertura_export_scope(report, class, child);
   }
}

static void cobertura_export_inst(cobertura_report_t *report, cover_obj_t inst)
{
   cover_obj_t root = cover_get_obj(report->data, inst, COV_ATTR_ROOT);
   cobertura_class_t *class = cobertura_get_class(report, root);

   cobertura_export_scope(report, class, root);

   cover_iter_t it = cover_begin(report->data, inst, COV_REL_CHILDREN);
   cover_obj_t child;
   while (cover_next(&it, &child))
      cobertura_export_inst(report, child);
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

   const double line_rate =
      nlines > 0 ? (double)hitlines / (double)nlines : 1.0;
   const double branch_rate =
      nbranches > 0 ? (double)hitbranches / (double)nbranches : 1.0;

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

void cover_export_cobertura(const cover_data_t *db, FILE *f,
                            const char *relative)
{
   cobertura_report_t report = {
      .class_map = hash_new(64),
      .relative = relative,
      .data = db,
   };

   cover_iter_t it = cover_begin(db, COVER_NULL_OBJ, COV_REL_CHILDREN);
   cover_obj_t root;
   while (cover_next(&it, &root))
      cobertura_export_inst(&report, root);

   fprintf(f, "<?xml version='1.0' encoding='UTF-8'?>\n");
   fprintf(f, "<!DOCTYPE coverage SYSTEM "
           "'http://cobertura.sourceforge.net/xml/coverage-04.dtd'>\n");

   int nlines = 0, hitlines = 0, nbranches = 0, hitbranches = 0;
   for (cobertura_class_t *it = report.classes; it; it = it->next)
      cobertura_class_stats(it, &nlines, &hitlines, &nbranches, &hitbranches);

   const double line_rate =
      nlines > 0 ? (double)hitlines / (double)nlines : 1.0;
   const double branch_rate =
      nbranches > 0 ? (double)hitbranches / (double)nbranches : 1.0;

   time_t timestamp;
   const long override_time = opt_get_int(OPT_COVER_TIMESTAMP);
   if (override_time >= 0)
      timestamp = override_time;
   else
      timestamp = time(NULL);

   const char *version = opt_get_str(OPT_COVER_VERSION) ?: PACKAGE_STRING;

   cover_obj_t root0 = cover_at(db, COVER_NULL_OBJ, COV_REL_CHILDREN, 0);
   ident_t work_name = cover_get_ident(db, root0, COV_ATTR_LIB_NAME);

   fprintf(f, "<coverage version=\"%s\" "
           "line-rate=\"%f\" branch-rate=\"%f\" complexity=\"0.0\" "
           "lines-valid=\"%d\" lines-covered=\"%d\" "
           "branches-valid=\"%d\" branches-covered=\"%d\" "
           "timestamp=\"%llu\">\n",
           version, line_rate, branch_rate, nlines, hitlines, nbranches,
           hitbranches, (long long)timestamp);
   fprintf(f, "<sources>\n");
   fprintf(f, "<source>.</source>\n");
   fprintf(f, "</sources>\n");
   fprintf(f, "<packages>\n");
   fprintf(f, "<package name=\"%s\" "
           "line-rate=\"%f\" branch-rate=\"%f\" complexity=\"0.0\">\n",
           istr(work_name), line_rate, branch_rate);

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
      free(it->file);
      free(it);
   }

   hash_free(report.class_map);
}

////////////////////////////////////////////////////////////////////////////////
// XML dump format for debugging and testing

static void dump_item_xml(const cover_data_t *db, cover_obj_t item, int indent,
                          FILE *f)
{
   const cover_item_kind_t kind = cover_get_kind(db, item);

   cover_iter_t it = cover_begin(db, item, COV_REL_BINS);
   cover_obj_t bin;
   while (cover_next(&it, &bin)) {
      ident_t hier = cover_get_ident(db, bin, COV_ATTR_HIER);
      uint32_t data = cover_get_u32(db, bin, COV_ATTR_DATA, 0);

      switch (kind) {
      case COV_ITEM_STMT:
         fprintf(f, "%*s<statement hier=\"%s\" data=\"%d\"/>\n", indent + 2, "",
                 istr(hier), data);
         break;
      case COV_ITEM_BRANCH:
         fprintf(f, "%*s<branch hier=\"%s\" data=\"%d\"/>\n", indent + 2, "",
                 istr(hier), data);
         break;
      case COV_ITEM_EXPRESSION:
         fprintf(f, "%*s<expression hier=\"%s\" data=\"%d\"/>\n", indent + 2,
                 "", istr(hier), data);
         break;
      case COV_ITEM_TOGGLE:
         fprintf(f, "%*s<toggle hier=\"%s\" data=\"%d\"/>\n", indent + 2, "",
                 istr(hier), data);
         break;
      case COV_ITEM_FUNCTIONAL:
         fprintf(f, "%*s<functional hier=\"%s\" data=\"%d\"/>\n", indent + 2,
                 "", istr(hier), data);
         break;
      case COV_ITEM_STATE:
         fprintf(f, "%*s<state hier=\"%s\" data=\"%d\"/>\n", indent + 2,
                 "", istr(hier), data);
         break;
      }
   }
}

static void dump_scope_xml(const cover_data_t *db, cover_obj_t scope,
                           int indent, const loc_t *loc, const char *relative,
                           FILE *f)
{
   ident_t name = cover_get_ident(db, scope, COV_ATTR_NAME);
   cover_obj_t inst = cover_get_obj(db, scope, COV_ATTR_INST);
   loc_t scope_loc = cover_get_loc(db, scope, COV_ATTR_LOC);

   cover_obj_t root = cover_get_obj(db, inst, COV_ATTR_ROOT);
   if (cover_equals(root, scope))
      return;  // TODO: remove

   fprintf(f, "%*s<scope name=\"%s\"", indent, "", istr(name));

   if (scope_loc.file_ref != FILE_INVALID
       && scope_loc.file_ref != loc->file_ref) {
      LOCAL_TEXT_BUF tb = tb_new();
      get_relative_path(tb, relative, loc_file_str(&scope_loc));
      fprintf(f, " file=\"%s\"", tb_get(tb));
   }

   if (scope_loc.first_line != LINE_INVALID && scope_loc.first_line > 0
       && scope_loc.first_line != loc->first_line)
      fprintf(f, " line=\"%d\"", scope_loc.first_line);

   fprintf(f, ">\n");

   cover_iter_t item_it = cover_begin(db, scope, COV_REL_ITEMS);
   cover_obj_t item;
   while (cover_next(&item_it, &item))
      dump_item_xml(db, item, indent, f);

   cover_iter_t child_it = cover_begin(db, scope, COV_REL_CHILDREN);
   cover_obj_t child;
   while (cover_next(&child_it, &child)) {
      // TODO: remove this check
      if (cover_equals(cover_get_obj(db, child, COV_ATTR_INST), inst))
         dump_scope_xml(db, child, indent + 2, &scope_loc, relative, f);
   }

   fprintf(f, "%*s</scope>\n", indent, "");
}

static void dump_inst_xml(const cover_data_t *db, cover_obj_t inst, int indent,
                          const loc_t *loc, const char *relative, FILE *f)
{
   ident_t name = cover_get_ident(db, inst, COV_ATTR_NAME);
   cover_obj_t root = cover_get_obj(db, inst, COV_ATTR_ROOT);
   loc_t scope_loc = cover_get_loc(db, root, COV_ATTR_LOC);

   fprintf(f, "%*s<scope name=\"%s\"", indent, "", istr(name));

   ident_t block_name = cover_get_ident(db, inst, COV_ATTR_BLOCK_NAME);
   if (block_name != NULL)
      fprintf(f, " block_name=\"%s\"", istr(block_name));

   if (scope_loc.file_ref != FILE_INVALID
       && scope_loc.file_ref != loc->file_ref) {
      LOCAL_TEXT_BUF tb = tb_new();
      get_relative_path(tb, relative, loc_file_str(&scope_loc));
      fprintf(f, " file=\"%s\"", tb_get(tb));
   }

   if (scope_loc.first_line != LINE_INVALID && scope_loc.first_line > 0
       && scope_loc.first_line != loc->first_line)
      fprintf(f, " line=\"%d\"", scope_loc.first_line);

   fprintf(f, ">\n");

   cover_iter_t scope_it = cover_begin(db, root, COV_REL_CHILDREN);
   cover_obj_t scope;
   while (cover_next(&scope_it, &scope))
      dump_scope_xml(db, scope, indent + 2, &scope_loc, relative, f);

   cover_iter_t child_it = cover_begin(db, inst, COV_REL_CHILDREN);
   cover_obj_t child;
   while (cover_next(&child_it, &child))
      dump_inst_xml(db, child, indent + 2, &scope_loc, relative, f);

   fprintf(f, "%*s</scope>\n", indent, "");
}

void cover_export_xml(const cover_data_t *db, FILE *f, const char *relative)
{
   fprintf(f, "<?xml version=\"1.0\"?>\n");

   cover_obj_t root0 = cover_at(db, COVER_NULL_OBJ, COV_REL_CHILDREN, 0);
   ident_t work_name = cover_get_ident(db, root0, COV_ATTR_LIB_NAME);
   fprintf(f, "<scope name=\"%s\">\n", istr(work_name));

   cover_iter_t it = cover_begin(db, COVER_NULL_OBJ, COV_REL_CHILDREN);
   cover_obj_t root;
   while (cover_next(&it, &root))
      dump_inst_xml(db, root, 2, &LOC_INVALID, relative, f);

   fprintf(f, "</scope>\n");
}
