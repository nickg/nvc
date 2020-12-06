//
//  Copyright (C) 2011-2021  Nick Gasson
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

#include "loc.h"
#include "ident.h"
#include "array.h"

#include <assert.h>
#include <string.h>
#include <stdlib.h>

#define LOC_MAGIC 0xf00f

typedef struct {
   loc_file_ref_t  ref;
   char           *name_str;
   const char     *linebuf;
} loc_file_t;

struct loc_wr_ctx {
   fbuf_t *fbuf;
};

struct loc_rd_ctx {
   fbuf_t          *fbuf;
   char           **file_map;
   loc_file_ref_t  *ref_map;
   size_t           n_files;
};

static A(loc_file_t) loc_files;

static loc_file_t *loc_file_data(const loc_t *loc)
{
   return AREF(loc_files, loc->file_ref);
}

void fmt_loc(FILE *f, const loc_t *loc)
{
   if (loc == NULL || loc->first_line == LINE_INVALID
       || loc->file_ref == FILE_INVALID)
      return;

   loc_file_t *file_data = loc_file_data(loc);

   if (file_data->name_str == NULL)
      return;

   if (get_message_style() == MESSAGE_COMPACT) {
      fprintf(f, "%s:%d:%d: ", file_data->name_str, loc->first_line,
              loc->first_column + 1);
      return;
   }

   fprintf(f, "\tFile %s, Line %u\n", file_data->name_str, loc->first_line);

   const char *lb = loc_linebuf(loc);
   if (lb == NULL)
      return;

   char buf[80];
   size_t i = 0;
   while (i < sizeof(buf) - 1 && *lb != '\0' && *lb != '\n') {
      if (*lb == '\t')
         buf[i++] = ' ';
      else
         buf[i++] = *lb;
      ++lb;
   }
   buf[i] = '\0';

   // Print ... if error location spans multiple lines
   unsigned last_column = loc->first_column + loc->column_delta;
   bool many_lines = (loc->line_delta > 0)
      || (i == sizeof(buf) - 1 && i <= last_column);
   int last_col = many_lines ? strlen(buf) + 3 : last_column;

   color_fprintf(f, "    $cyan$%s%s\n", buf, many_lines ? " ..." : "");
   if (last_col >= loc->first_column) {
      for (unsigned j = 0; j < loc->first_column + 4; j++)
         fprintf(f, " ");
      color_fprintf(f, "$green$");
      for (unsigned j = 0; j < last_col - loc->first_column + 1; j++)
         fprintf(f, "^");
      color_fprintf(f, "$$\n");
   }

   fflush(f);
}

loc_file_ref_t loc_file_ref(const char *name, const char *linebuf)
{
   if (name == NULL)
      return FILE_INVALID;

   for (unsigned i = 0; i < loc_files.count; i++) {
      if (strcmp(loc_files.items[i].name_str, name) == 0)
         return loc_files.items[i].ref;
   }

   // Strip any consecutive '/' characters
   char *name_buf = xstrdup(name), *p = name_buf;
   for (char *s = name_buf; *s != '\0'; s++) {
      if (*s != '/' || *(s + 1) != '/')
         *p++ = *s;
   }
   *p = '\0';

   loc_file_t new = {
      .linebuf  = linebuf,
      .name_str = name_buf,
      .ref      = loc_files.count
   };

   APUSH(loc_files, new);

   return new.ref;
}

const char *loc_file_str(const loc_t *loc)
{
   if (loc->file_ref != FILE_INVALID)
      return loc_file_data(loc)->name_str;
   else
      return NULL;
}

const char *loc_linebuf(const loc_t *loc)
{
   if (loc->file_ref != FILE_INVALID
       && loc->first_line != LINE_INVALID
       && loc->first_column != COLUMN_INVALID) {

      const char *start = loc_file_data(loc)->linebuf;
      for (unsigned i = 1; start && i < loc->first_line; i++) {
         if ((start = strchr(start, '\n')))
            start++;
      }

      return start;
   }
   else
      return NULL;
}

bool loc_invalid_p(const loc_t *loc)
{
   return loc->first_line == LINE_INVALID
      || loc->file_ref == FILE_INVALID;
}

loc_t get_loc(unsigned first_line, unsigned first_column, unsigned last_line,
              unsigned last_column, loc_file_ref_t file_ref)
{
   if (first_line == LINE_INVALID || last_line == LINE_INVALID
       || first_column == COLUMN_INVALID || last_column == COLUMN_INVALID)
      return LOC_INVALID;

   assert(first_line <= last_line);
   assert(first_line != last_line || first_column <= last_column);

   loc_t result = {
      .first_line   = MIN(first_line, LINE_INVALID),
      .first_column = MIN(first_column, COLUMN_INVALID),
      .line_delta   = MIN(last_line - first_line, DELTA_INVALID),
      .column_delta = MIN(last_column - first_column, DELTA_INVALID),
      .file_ref     = file_ref
   };
   return result;
}

bool loc_eq(const loc_t *a, const loc_t *b)
{
   return a->first_line == b->first_line
      && a->first_column == b->first_column
      && a->line_delta == b->line_delta
      && a->column_delta == b->column_delta
      && a->file_ref == b->file_ref;
}

loc_wr_ctx_t *loc_write_begin(fbuf_t *f)
{
   loc_wr_ctx_t *ctx = xmalloc(sizeof(loc_wr_ctx_t));
   ctx->fbuf = f;

   write_u16(LOC_MAGIC, f);
   write_u16(loc_files.count, f);

   for (unsigned i = 0; i < loc_files.count; i++) {
      size_t len = strlen(loc_files.items[i].name_str) + 1;
      write_u16(len, f);
      write_raw(loc_files.items[i].name_str, len, f);
   }

   return ctx;
}

void loc_write_end(loc_wr_ctx_t *ctx)
{
   free(ctx);
}

void loc_write(const loc_t *loc, loc_wr_ctx_t *ctx)
{
   const uint64_t merged =
      ((uint64_t)loc->first_line << 44)
      | ((uint64_t)loc->first_column << 32)
      | ((uint64_t)loc->line_delta << 24)
      | (uint64_t)(loc->column_delta << 16)
      | loc->file_ref;

   write_u64(merged, ctx->fbuf);
}

loc_rd_ctx_t *loc_read_begin(fbuf_t *f)
{
   uint16_t magic = read_u16(f);
   if (magic != LOC_MAGIC)
      fatal("corrupt location header in %s", fbuf_file_name(f));

   loc_rd_ctx_t *ctx = xmalloc(sizeof(loc_rd_ctx_t));
   ctx->fbuf    = f;
   ctx->n_files = read_u16(f);

   ctx->file_map = xcalloc(sizeof(ident_t) * ctx->n_files);
   ctx->ref_map  = xcalloc(sizeof(loc_file_ref_t) * ctx->n_files);

   for (size_t i = 0; i < ctx->n_files; i++) {
      size_t len = read_u16(f);
      char *buf = xmalloc(len + 1);
      read_raw(buf, len, f);
      buf[len] = '\0';
      ctx->file_map[i] = buf;
      ctx->ref_map[i]  = FILE_INVALID;
   }

   return ctx;
}

void loc_read_end(loc_rd_ctx_t *ctx)
{
   for (size_t i = 0; i < ctx->n_files; i++)
      free(ctx->file_map[i]);

   free(ctx->file_map);
   free(ctx->ref_map);
   free(ctx);
}

void loc_read(loc_t *loc, loc_rd_ctx_t *ctx)
{
   const uint64_t merged = read_u64(ctx->fbuf);

   uint16_t old_ref = merged & 0xffff;
   loc_file_ref_t new_ref = FILE_INVALID;
   if (old_ref != FILE_INVALID) {
      if (unlikely(old_ref >= ctx->n_files))
         fatal("corrupt location file reference %x", old_ref);

      if (ctx->ref_map[old_ref] == FILE_INVALID) {
         for (unsigned i = 0; i < loc_files.count; i++) {
            if (strcmp(loc_files.items[i].name_str,
                       ctx->file_map[old_ref]) == 0)
               ctx->ref_map[old_ref] = loc_files.items[i].ref;
         }
      }

      if (ctx->ref_map[old_ref] == FILE_INVALID) {
         loc_file_t new = {
            .linebuf  = NULL,
            .name_str = ctx->file_map[old_ref],
            .ref      = loc_files.count
         };

         APUSH(loc_files, new);

         ctx->ref_map[old_ref]  = new.ref;
         ctx->file_map[old_ref] = NULL;   // Owned by loc_file_t now
      }

      new_ref = ctx->ref_map[old_ref];
   }

   loc->first_line   = (merged >> 44) & 0xfffff;
   loc->first_column = (merged >> 32) & 0xfff;
   loc->line_delta   = (merged >> 24) & 0xff;
   loc->column_delta = (merged >> 16) & 0xff;
   loc->file_ref     = new_ref;
}
