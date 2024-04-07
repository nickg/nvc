//
//  Copyright (C) 2023-2024  Nick Gasson
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
#include "ucis/ucis-api.h"

#include <stdlib.h>
#include <string.h>
#include <time.h>

static const char *get_relative_path(const char *path, const char *relative)
{
   if (relative != NULL && is_absolute_path(path)) {
      const size_t rlen = strlen(relative);
      if (strncmp(relative, path, rlen) == 0) {
         path += rlen;
         while (path[0] == DIR_SEP[0] || path[0] == '/')
            path++;
      }
   }

   return path;
}

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
      c->file = get_relative_path(loc_file_str(loc), report->relative);
      c->next = report->classes;

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
            if (t->data > 0)
               l->bflags |= t->flags;
         }
         break;
      default:
         break;
      }
   }

   for (int i = 0; i < s->children.count; i++)
      cobertura_export_scope(report, class, s->children.items[i]);
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

   const char *version = opt_get_str(OPT_COVER_VERSION) ?: PACKAGE_STRING;

   fprintf(f, "<coverage version=\"%s\" "
           "line-rate=\"%f\" branch-rate=\"%f\" complexity=\"0.0\" "
           "lines-valid=\"%d\" lines-covered=\"%d\" "
           "branches-valid=\"%d\" branches-covered=\"%d\" "
           "timestamp=\"%lu\">\n",
           version, line_rate, branch_rate, nlines, hitlines, nbranches,
           hitbranches, timestamp);
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

////////////////////////////////////////////////////////////////////////////////
// XML dump format for debugging and testing

static void dump_scope_xml(cover_scope_t *s, int indent, const loc_t *loc,
                           const char *relative, FILE *f)
{
   fprintf(f, "%*s<scope name=\"%s\"", indent, "", istr(s->name));

   if (s->block_name != NULL)
      fprintf(f, " block_name=\"%s\"", istr(s->block_name));

   if (s->loc.file_ref != FILE_INVALID && s->loc.file_ref != loc->file_ref) {
      const char *path = get_relative_path(loc_file_str(&s->loc), relative);
      fprintf(f, " file=\"%s\"", path);
   }

   if (s->loc.first_line != LINE_INVALID && s->loc.first_line > 0
       && s->loc.first_line != loc->first_line)
      fprintf(f, " line=\"%d\"", s->loc.first_line);

   fprintf(f, ">\n");

   for (int i = 0; i < s->items.count; i++) {
      cover_item_t *item = &(s->items.items[i]);
      switch (item->kind) {
      case COV_ITEM_STMT:
         fprintf(f, "%*s<statement hier=\"%s\" data=\"%d\"/>\n", indent + 2, "",
                 istr(item->hier), item->data);
         break;
      case COV_ITEM_BRANCH:
         fprintf(f, "%*s<branch hier=\"%s\" data=\"%d\"/>\n", indent + 2, "",
                 istr(item->hier), item->data);
         break;
      case COV_ITEM_EXPRESSION:
         fprintf(f, "%*s<expression hier=\"%s\" data=\"%d\"/>\n", indent + 2,
                 "", istr(item->hier), item->data);
         break;
      case COV_ITEM_TOGGLE:
         fprintf(f, "%*s<toggle hier=\"%s\" data=\"%d\"/>\n", indent + 2, "",
                 istr(item->hier), item->data);
         break;
      case COV_ITEM_FUNCTIONAL:
         fprintf(f, "%*s<functional hier=\"%s\" data=\"%d\"/>\n", indent + 2,
                 "", istr(item->hier), item->data);
         break;
      case COV_ITEM_STATE:
         fprintf(f, "%*s<state hier=\"%s\" data=\"%d\"/>\n", indent + 2,
                 "", istr(item->hier), item->data);
         break;
      }
   }

   for (int i = 0; i < s->children.count; i++)
      dump_scope_xml(s->children.items[i], indent + 2, &s->loc, relative, f);

   fprintf(f, "%*s</scope>\n", indent, "");
}

void cover_export_xml(cover_data_t *data, FILE *f, const char *relative)
{
   fprintf(f, "<?xml version=\"1.0\"?>\n");
   dump_scope_xml(data->root_scope, 0, &LOC_INVALID, relative, f);
}

////////////////////////////////////////////////////////////////////////////////
// Unified Coverage Interoperability Standard (UCIS)

typedef struct {
   ucisT    db;
   shash_t *files;
} ucis_export_t;

static ucisFileHandleT ucis_get_file(ucis_export_t *u, const loc_t *loc)
{
   const char *f = loc_file_str(loc);

   ucisFileHandleT h = shash_get(u->files, f);
   if (h != NULL)
      return h;

   h = ucis_CreateFileHandle(u->db, f, "./");
   shash_put(u->files, f, h);
   return h;
}

static void ucis_get_source_info(ucis_export_t *u, const loc_t *loc,
                                 ucisSourceInfoT *info)
{
   info->filehandle = ucis_get_file(u, loc);
   info->line = loc->first_line;
   info->token = 0;
}

static void ucis_export_scope(ucis_export_t *u, ucisScopeT parent,
                              cover_scope_t *s)
{
   printf("%s %d\n", istr(s->name), s->type);

   ucisScopeT scope = parent;

   if (s->type == CSCOPE_INSTANCE) {
      ucisSourceInfoT srcinfo;
      ucis_get_source_info(u, &(s->loc), &srcinfo);

      ucisScopeT du = ucis_CreateScope(u->db, parent, istr(s->block_name),
                                       &srcinfo, 1, UCIS_VHDL, UCIS_DU_MODULE,
                                       UCIS_ENABLED_STMT | UCIS_ENABLED_BRANCH |
                                       UCIS_ENABLED_COND | UCIS_ENABLED_EXPR |
                                       UCIS_ENABLED_FSM | UCIS_ENABLED_TOGGLE |
                                       UCIS_SCOPE_UNDER_DU);

      scope = ucis_CreateInstance(u->db, NULL, istr(s->hier), &srcinfo, 1,
                                  UCIS_VHDL, UCIS_INSTANCE, du, 0);
   }

   ucisScopeT branch = NULL;

   for (int i = 0; i < s->items.count; i++) {
      cover_item_t *t = &(s->items.items[i]);
      switch (t->kind) {
      case COV_ITEM_STMT:
         {
            ucisSourceInfoT srcinfo;
            ucis_get_source_info(u, &(t->loc), &srcinfo);

            ucisCoverDataT coverdata = {
               .type = UCIS_STMTBIN,
               .flags = UCIS_IS_32BIT,
               .data = { .int32 = t->data }
            };

            const int index = ucis_CreateNextCover(u->db,parent, NULL,
                                                   &coverdata, &srcinfo);
            ucis_SetIntProperty(u->db, parent, index, UCIS_INT_STMT_INDEX, 1);
         }
         break;
      case COV_ITEM_BRANCH:
         {
            ucisSourceInfoT srcinfo;
            ucis_get_source_info(u, &(t->loc), &srcinfo);

            if (branch == NULL) {
               char name[64];
               checked_sprintf(name, sizeof(name), "branch#%d#%d#",
                               t->loc.first_line, 1);

               branch = ucis_CreateScope(u->db, scope, name, &srcinfo, 1,
                                         UCIS_VHDL, UCIS_BRANCH, 0);
            }

            ucisCoverDataT coverdata = {
               .type = UCIS_BRANCHBIN,
               .flags = UCIS_IS_32BIT,
               .data = { .int32 = t->data }
            };
            ucis_CreateNextCover(u->db, branch, NULL, &coverdata, &srcinfo);
         }
         break;
      default:
         break;
      }
   }

   for (int i = 0; i < s->children.count; i++)
      ucis_export_scope(u, scope, s->children.items[i]);
}

static void ucis_error_handler(void *data, ucisErrorT *errorInfo)
{
   static const diag_level_t map[] = { DIAG_NOTE, DIAG_WARN, DIAG_ERROR };

   diag_t *d = diag_new(map[errorInfo->severity], NULL);
   diag_printf(d, "UCIS error: %s", errorInfo->msgstr);
   diag_emit(d);

   if (errorInfo->severity == UCIS_MSG_ERROR)
      fatal_exit(1);
}

static ucisT ucis_convert_from_internal(cover_data_t *data)
{
   ucis_RegisterErrorHandler(ucis_error_handler, NULL);

   ucis_export_t u = {
      .db = ucis_Open(NULL),
      .files = shash_new(64),
   };

   LOCAL_TEXT_BUF date = tb_new();
   tb_strftime(date, "L%Y%m%d%H%M%S", time(NULL));

   ucisTestDataT testdata = {
      .teststatus   = UCIS_TESTSTATUS_OK,
      .simtime      = 0.0,
      .timeunit     = "fs",
      .runcwd       = "./",
      .cputime      = 0.0,
      .seed         = "0",
      .cmd          = PACKAGE,
      .args         = "",
      .compulsory   = 0,
      .date         = tb_get(date),
      .username     = "",
      .cost         = 0.0,
      .toolcategory = UCIS_SIM_TOOL
   };

   ucisHistoryNodeT testnode =
      ucis_CreateHistoryNode(u.db, NULL, "TestLogicalName", NULL,
                             UCIS_HISTORYNODE_TEST);

   ucis_SetTestData(u.db, testnode, &testdata);

   ucis_export_scope(&u, NULL, data->root_scope);

   shash_free(u.files);
   return u.db;
}

void cover_export_ucdb(cover_data_t *data, const char *path)
{
   ucisT db = ucis_convert_from_internal(data);
   ucis_Write(db, path, NULL, 0, -1);
   ucis_Close(db);
}

void cover_export_ucis(cover_data_t *data, const char *path)
{
   ucisT db = ucis_convert_from_internal(data);
   ucis_WriteToInterchangeFormat(db, path, NULL, 0, -1);
   ucis_Close(db);
}
