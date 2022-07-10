//
//  Copyright (C) 2011-2022  Nick Gasson
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
#include "diag.h"
#include "fbuf.h"
#include "opt.h"

#include <ctype.h>
#include <string.h>
#include <stdlib.h>
#include <stdarg.h>
#include <limits.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>

#define LOC_MAGIC 0xf00f

typedef struct {
   loc_file_ref_t  ref;
   char           *name_str;
   const char     *linebuf;
   bool            tried_open;
} loc_file_t;

typedef A(loc_file_t) file_list_t;

struct loc_wr_ctx {
   fbuf_t *fbuf;
   bool    have_index;
};

struct loc_rd_ctx {
   fbuf_t          *fbuf;
   char           **file_map;
   loc_file_ref_t  *ref_map;
   size_t           n_files;
   bool             have_index;
};

typedef enum {
   HINT_NOTE,
   HINT_HELP
} hint_kind_t;

typedef struct {
   loc_t        loc;
   char        *text;
   int          priority;
   hint_kind_t  kind;
} diag_hint_t;

typedef A(diag_hint_t) hint_list_t;

struct _diag {
   text_buf_t   *msg;
   diag_level_t  level;
   hint_list_t   hints;
   hint_list_t   trace;
   bool          color;
   bool          source;
};

static diag_consumer_t  consumer = NULL;
static diag_hint_fn_t   hint_fn = NULL;
static void            *hint_ctx = NULL;
static unsigned         n_errors = 0;
static file_list_t      loc_files;
static vhdl_severity_t  exit_severity = SEVERITY_ERROR;

#define DIAG_THEME_CLASSIC 1
#define DIAG_THEME_RUST    2

#define DIAG_THEME DIAG_THEME_CLASSIC

////////////////////////////////////////////////////////////////////////////////
// Source location tracking

static loc_file_t *loc_file_data(const loc_t *loc)
{
   return AREF(loc_files, loc->file_ref);
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

bool loc_invalid_p(const loc_t *loc)
{
   return loc == NULL
      || loc->first_line == LINE_INVALID
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
   ctx->have_index = false;

   return ctx;
}

void loc_write_end(loc_wr_ctx_t *ctx)
{
   free(ctx);
}

void loc_write(const loc_t *loc, loc_wr_ctx_t *ctx)
{
   if (!ctx->have_index) {
      write_u16(LOC_MAGIC, ctx->fbuf);
      fbuf_put_uint(ctx->fbuf, loc_files.count);

      for (unsigned i = 0; i < loc_files.count; i++) {
         size_t len = strlen(loc_files.items[i].name_str) + 1;
         fbuf_put_uint(ctx->fbuf, len);
         write_raw(loc_files.items[i].name_str, len, ctx->fbuf);
      }

      ctx->have_index = true;
   }

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
   loc_rd_ctx_t *ctx = xcalloc(sizeof(loc_rd_ctx_t));
   ctx->fbuf = f;

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
   if (!ctx->have_index) {
      uint16_t magic = read_u16(ctx->fbuf);
      if (magic != LOC_MAGIC)
         fatal("corrupt location header in %s", fbuf_file_name(ctx->fbuf));

      ctx->n_files = fbuf_get_uint(ctx->fbuf);

      ctx->file_map = xcalloc_array(ctx->n_files, sizeof(ident_t));
      ctx->ref_map  = xcalloc_array(ctx->n_files, sizeof(loc_file_ref_t));

      for (size_t i = 0; i < ctx->n_files; i++) {
         size_t len = fbuf_get_uint(ctx->fbuf);
         char *buf = xmalloc(len + 1);
         read_raw(buf, len, ctx->fbuf);
         buf[len] = '\0';
         ctx->file_map[i] = buf;
         ctx->ref_map[i]  = FILE_INVALID;
      }

      ctx->have_index = true;
   }

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

////////////////////////////////////////////////////////////////////////////////
// Fancy diagnostics

#if DIAG_THEME == DIAG_THEME_CLASSIC
#define DEBUG_PREFIX   "** Debug: "
#define NOTE_PREFIX    "** Note: "
#define WARNING_PREFIX "$yellow$** Warning:$$ "
#define ERROR_PREFIX   "$red$** Error:$$ "
#define FATAL_PREFIX   "$red$** Fatal:$$ "
#define GUTTER_STYLE   "$blue$"
#define HINT_STYLE     ""
#define CARET_STYLE    ""
#define TRACE_STYLE    ""
#define TRAILING_BLANK 0
#define TRACE_LOC      0
#elif DIAG_THEME == DIAG_THEME_RUST
#define DEBUG_PREFIX   "$!green$Debug:$$ "
#define NOTE_PREFIX    "$bold$Note:$$ "
#define WARNING_PREFIX "$!yellow$Warning:$$ "
#define ERROR_PREFIX   "$!red$Error:$$ "
#define FATAL_PREFIX   "$!red$Fatal:$$ "
#define GUTTER_STYLE   "$!blue$"
#define HINT_STYLE     "$bold$"
#define CARET_STYLE    "$bold$"
#define TRACE_STYLE    "$bold$"
#define TRAILING_BLANK 1
#define TRACE_LOC      1
#else
#error "invalid DIAG_THEME"
#endif

static const struct {
   const char *title;
   const char *section[STD_08 + 1];
} lrm_sections[] = {
   { "Names", { [STD_08] = "8.1", [STD_93] = "6.1" } },
   { "Signal parameters", { [STD_08] = "4.2.2.3", [STD_93] = "2.1.1.2" } },
   { "Constant and variable parameters", { [STD_93] = "2.1.1.1",
        [STD_08] = "4.2.2.2" } },
   { "Object aliases", { [STD_93] = "4.3.3.1", [STD_08] = "6.6.2" } },
   { "Case statement", { [STD_93] = "8.8", [STD_08] = "10.9" } },
   { "Elaboration of a declarative part", { [STD_93] = "12.3",
        [STD_08] = "14.4" } },
   { "Process statement", { [STD_93] = "9.2", [STD_08] = "11.3" } },
   { "Entity statement part", { [STD_93] = "1.1.3", [STD_08] = "3.2.4" } },
};

diag_t *diag_new(diag_level_t level, const loc_t *loc)
{
   diag_t *d = xcalloc(sizeof(diag_t));
   d->msg    = tb_new();
   d->level  = level;
   d->color  = color_terminal() && consumer == NULL;
   d->source = true;

   if (!loc_invalid_p(loc)) {
      diag_hint_t hint = {
         .loc   = *loc
      };
      APUSH(d->hints, hint);
   }

   if (hint_fn != NULL)
      (*hint_fn)(d, hint_ctx);

   return d;
}

void diag_vprintf(diag_t *d, const char *fmt, va_list ap)
{
   if (strchr(fmt, '$') != 0) {
      char *buf LOCAL;
      if (d->color)
         buf = color_vasprintf(fmt, ap);
      else
         buf = strip_color(fmt, ap);

      tb_cat(d->msg, buf);
   }
   else
      tb_vprintf(d->msg, fmt, ap);
}

void diag_printf(diag_t *d, const char *fmt, ...)
{
   va_list ap;
   va_start(ap, fmt);
   diag_vprintf(d, fmt, ap);
   va_end(ap);
}

void diag_hint(diag_t *d, const loc_t *loc, const char *fmt, ...)
{
   va_list ap;
   va_start(ap, fmt);

   char *text;
   if (strchr(fmt, '$') != 0) {
      if (d->color)
         text = color_vasprintf(fmt, ap);
      else
         text = strip_color(fmt, ap);
   }
   else
      text = xvasprintf(fmt, ap);

   va_end(ap);

   if (!loc_invalid_p(loc)) {
      for (int i = 0; i < d->hints.count; i++) {
         diag_hint_t *hint = &(d->hints.items[i]);
         if (loc_eq(loc, &(hint->loc))) {
            free(hint->text);
            hint->text = text;
            return;
         }
      }
   }

   diag_hint_t h = {
      .loc      = loc ? *loc : LOC_INVALID,
      .text     = text,
      .priority = -(d->hints.count),
      .kind     = HINT_NOTE,
   };
   APUSH(d->hints, h);
}

void diag_trace(diag_t *d, const loc_t *loc, const char *fmt, ...)
{
   va_list ap;
   va_start(ap, fmt);

   char *text;
   if (strchr(fmt, '$') != 0) {
      if (d->color)
         text = color_vasprintf(fmt, ap);
      else
         text = strip_color(fmt, ap);
   }
   else
      text = xvasprintf(fmt, ap);

   va_end(ap);

   diag_hint_t h = {
      .loc      = loc ? *loc : LOC_INVALID,
      .text     = text,
      .priority = d->hints.count,
   };
   APUSH(d->trace, h);

   if (loc != NULL && d->hints.count == 0) {
      diag_hint_t hint = {
         .loc   = *loc
      };
      APUSH(d->hints, hint);
   }
}

void diag_lrm(diag_t *d, vhdl_standard_t std, const char *section)
{
   const char *title = NULL;
   for (int i = 0; i < ARRAY_LEN(lrm_sections); i++) {
      const char *s = lrm_sections[i].section[std];
      if (s != NULL && strcmp(s, section) == 0) {
         // Prefer the section number from the current standard
         const char *ss = lrm_sections[i].section[standard()];
         if (ss != NULL) {
            std = standard();
            section = ss;
         }

         title = lrm_sections[i].title;
         break;
      }
   }

   LOCAL_TEXT_BUF tb = tb_new();
   tb_printf(tb, "IEEE Std 1076-%s section %s",
             standard_text(std), section);

   if (title != NULL)
      tb_printf(tb, " \"%s\"", title);

   diag_hint_t h = {
      .loc      = LOC_INVALID,
      .text     = tb_claim(tb),
      .priority = d->hints.count,
      .kind     = HINT_HELP
   };
   APUSH(d->hints, h);
}

static void diag_paginate(const char *str, int left, FILE *f)
{
   const int right = terminal_width();

   if (right == 0 || left + strlen(str) < right) {
      fputs(str, f);
      return;
   }

   const char *p = str, *begin = str;
   for (int col = left, escape = 0; *p != '\0'; p++) {
      if (*p == '\033')
         escape = 1;
      else if (escape) {
         if (*p == 'm')
            escape = 0;
      }
      else if (col + 1 >= right && p - begin + 1 < right - left) {
         fprintf(f, "\n%*s", left, "");
         col = left;
      }
      else if (isspace((int)*p)) {
         fwrite(begin, 1, p - begin + 1, f);
         if (*p == '\n') {
            fprintf(f, "%*s", left, "");
            col = left;
         }
         else
            col++;
         begin = p + 1;
      }
      else
         ++col;
   }

   if (begin < p)
      fwrite(begin, 1, p - begin, f);
}

static const char *diag_get_source(const loc_t *loc)
{
   if (loc->file_ref == FILE_INVALID
       || loc->first_line == LINE_INVALID
       || loc->first_column == COLUMN_INVALID)
      return NULL;

   loc_file_t *file = loc_file_data(loc);

   if (file->linebuf == NULL && !file->tried_open) {
      file->tried_open = true;

      int fd = open(file->name_str, O_RDONLY);
      if (fd < 0)
         return NULL;

      struct stat buf;
      if (fstat(fd, &buf) != 0)
         goto close_file;

      if (!S_ISREG(buf.st_mode))
         goto close_file;

      if (buf.st_size > 0)
         file->linebuf = map_file(fd, buf.st_size);

   close_file:
      close(fd);
   }

   if (file->linebuf == NULL)
      return NULL;

   const char *start = file->linebuf;
   for (unsigned i = 1; i < loc->first_line; i++) {
      if ((start = strchr(start, '\n')))
         start++;
   }

   return start;
}

static int diag_compar(const void *_a, const void *_b)
{
   const diag_hint_t *a = _a, *b = _b;

   assert(a->loc.file_ref == b->loc.file_ref);
   return a->loc.first_line - b->loc.first_line;
}

static void diag_emit_loc(const loc_t *loc, FILE *f)
{
   const char *file = loc_file_str(loc);
   char *abspath LOCAL = realpath(file, NULL);

   color_fprintf(f, "$$$link:file://%s#%u\07%s:%u$\n",
                 abspath, loc->first_line, file, loc->first_line);
}

static void diag_emit_hints(diag_t *d, FILE *f)
{
   int fwidth = 0;
   const char *linebuf = NULL;
   bool need_gap = false;

   loc_t loc0 = d->hints.items[0].loc;
   if (loc0.file_ref == FILE_INVALID)
      goto other_files;

   int same_file = 0, line_max = 0, line_min = INT_MAX, swap = -1;
   for (int i = 0; i < d->hints.count; i++) {
      if (d->hints.items[i].loc.file_ref == loc0.file_ref) {
         same_file++;
         line_max = MAX(d->hints.items[i].loc.first_line, line_max);
         line_min = MIN(d->hints.items[i].loc.first_line, line_min);

         if (swap != -1) {
            diag_hint_t tmp = d->hints.items[swap];
            d->hints.items[swap] = d->hints.items[i];
            d->hints.items[i] = tmp;
            swap = -1;
         }
      }
      else if (swap == -1)
         swap = i;
   }

   qsort(d->hints.items, same_file, sizeof(diag_hint_t), diag_compar);

   if (d->source)
      linebuf = diag_get_source(&(d->hints.items[0].loc));

   if (linebuf == NULL)
      fwidth = 1;
   else {
      for (int n = line_max; n > 0; n /= 10, fwidth++)
      ;
   }

#if !TRACE_LOC
   if (linebuf == NULL && d->trace.count > 1)
      goto other_files;
#endif

   if (linebuf == NULL) {
      fprintf(f, "\t ");
      diag_emit_loc(&loc0, f);
      goto other_files;
   }


   color_fprintf(f, "%*s$blue$  > ", fwidth, "");
   diag_emit_loc(&loc0, f);

   color_fprintf(f, "%*s " GUTTER_STYLE " |$$\n", fwidth, "");
   need_gap = true;

   const char *p = linebuf;
   for (int i = line_min, h = 0; h < d->hints.count; i++) {
      diag_hint_t *hint = &(d->hints.items[h]);
      while (hint->loc.file_ref != loc0.file_ref) {
         if (h + 1 == d->hints.count)
            goto other_files;
         else
            hint = &(d->hints.items[++h]);
      }

      if (hint->loc.first_line > i + 2) {
         // Skip some lines
         color_fprintf(f, " " GUTTER_STYLE "...$$\n");
         for (; i < hint->loc.first_line; i++) {
            if ((p = strchr(p, '\n')) == NULL)
               return;
            p++;   // Skip newline
         }
      }

      color_fprintf(f, " " GUTTER_STYLE "%*.u |$$ $cyan$", fwidth, i);

      int first_col = hint->loc.first_column;
      for (int col = 0, n = 0; *p != '\0' && *p != '\n'; p++, n++) {
         if (n == hint->loc.first_column)
            first_col = col;
         if (*p == '\r')
            continue;
         else if (*p == '\t') {
            do {
               fputc(' ', f);
               col++;
            } while (col % 8 != 0);
         }
         else if (isprint((int)*p)) {
            fputc(*p, f);
            col++;
         }
      }

      color_fprintf(f, "$$\n");
      p++;   // Skip newline

      if (hint->loc.first_line == i) {
         color_fprintf(f, "%*s " GUTTER_STYLE " |$$ ", fwidth, "");

         const bool red =
            same_file > 1 && hint->priority == 0 && d->level >= DIAG_ERROR;

         if (red)
            color_fprintf(f, CARET_STYLE "$red$");
         else
            color_fprintf(f, CARET_STYLE "$green$");
         color_fprintf(f, "%*s", first_col, "");

         int ncarets = 1;
         if (hint->loc.line_delta == 0)
            ncarets = hint->loc.column_delta + 1;

         const int hintcol = fwidth + hint->loc.first_column + ncarets + 4;

         while (ncarets--) fputc('^', f);

         if (hint->text != NULL) {
            if (hintcol + strlen(hint->text) >= MAX(terminal_width(), 80))
               color_fprintf(f, "$$\n%*s " GUTTER_STYLE " |$$%*s", fwidth, "",
                             hint->loc.first_column, "");

            if (red)
               color_fprintf(f, "$$$red$ %s$$\n", hint->text);
            else
               color_fprintf(f, "$$$green$ %s$$\n", hint->text);
         }
         else
            color_fprintf(f, "$$\n");

         // Only support one hint per line
         for (; h < d->hints.count
                 && d->hints.items[h].loc.first_line == i; h++)
            ;
      }
   }

 other_files:
   for (int i = 0; i < d->hints.count; i++) {
      diag_hint_t *hint = &(d->hints.items[i]);
      if (hint->loc.file_ref == loc0.file_ref && fwidth > 0)
         continue;   // Printed above
      else if (hint->text == NULL)
         continue;

      if (need_gap) {
         color_fprintf(f, "%*s " GUTTER_STYLE " |$$\n", fwidth, "");
         need_gap = false;
      }

      int col = color_fprintf(f, "%*s", fwidth, "");

      if (linebuf != NULL)
         col += color_fprintf(f, " " GUTTER_STYLE " = $$");

      if (col == 0)
         col += color_fprintf(f, NOTE_PREFIX);
      else
         col += color_fprintf(f, HINT_STYLE "%s:$$ ",
                              hint->kind == HINT_HELP ? "Help" : "Note");

      diag_paginate(hint->text, col, f);
      fputc('\n', f);

      if (!loc_invalid_p(&(hint->loc))) {
         fprintf(f, "\t ");
         diag_emit_loc(&(hint->loc), f);
      }
   }
}

static void diag_emit_trace(diag_t *d, FILE *f)
{
   // Do not show a stack trace if it just repeats the initial location
   if (d->trace.count == 1 && d->hints.count > 0) {
      const loc_t hloc0 = d->hints.items[0].loc;
      const loc_t tloc0 = d->trace.items[0].loc;

      if (tloc0.file_ref == hloc0.file_ref
          || tloc0.first_line == hloc0.first_line)
         return;
   }

#if TRAILING_BLANK
   fputc('\n', f);
#endif

   const int fwidth UNUSED = 6;

   for (int i = 0; i < d->trace.count; i++) {
      diag_hint_t *hint = &(d->trace.items[i]);
      fprintf(f, "   " TRACE_STYLE "%s\n", hint->text);

      if (!loc_invalid_p(&(hint->loc))) {
         fprintf(f, "\t ");
         diag_emit_loc(&(hint->loc), f);
      }
   }
}

void diag_femit(diag_t *d, FILE *f)
{
   if (consumer != NULL && d->level > DIAG_DEBUG)
      (*consumer)(d);
   else if (get_message_style() == MESSAGE_COMPACT) {
      if (d->hints.count > 0) {
         loc_t *loc = &(d->hints.items[0].loc);
         if (!loc_invalid_p(loc)) {
            loc_file_t *file_data = loc_file_data(loc);
            fprintf(f, "%s:%d:%d: ", file_data->name_str, loc->first_line,
                    loc->first_column + 1);
         }

         switch (d->level) {
         case DIAG_DEBUG: fprintf(f, "note: "); break;
         case DIAG_NOTE:  fprintf(f, "note: "); break;
         case DIAG_WARN:  fprintf(f, "warning: "); break;
         case DIAG_ERROR: fprintf(f, "error: "); break;
         case DIAG_FATAL: fprintf(f, "fatal: "); break;
         }

         fprintf(f, "%s\n", tb_get(d->msg));
      }
   }
   else {
      if (tb_len(d->msg) > 0) {
         int col = 0;
         switch (d->level) {
         case DIAG_DEBUG: col = color_fprintf(f, DEBUG_PREFIX); break;
         case DIAG_NOTE:  col = color_fprintf(f, NOTE_PREFIX); break;
         case DIAG_WARN:  col = color_fprintf(f, WARNING_PREFIX); break;
         case DIAG_ERROR: col = color_fprintf(f, ERROR_PREFIX); break;
         case DIAG_FATAL: col = color_fprintf(f, FATAL_PREFIX); break;
         }

         diag_paginate(tb_get(d->msg), col, f);
         fputc('\n', f);
      }

      if (d->hints.count > 0)
         diag_emit_hints(d, f);

      if (d->trace.count > 0)
         diag_emit_trace(d, f);

#if TRAILING_BLANK
      if (d->trace.count > 0 || d->hints.count > 0)
         fputc('\n', f);
#endif

      fflush(f);
   }

   const bool is_error = d->level >= DIAG_ERROR
      || (opt_get_int(OPT_UNIT_TEST) && d->level > DIAG_DEBUG);

   if (is_error && ++n_errors == opt_get_int(OPT_ERROR_LIMIT))
      fatal("too many errors, giving up");

   for (int i = 0; i < d->hints.count; i++)
      free(d->hints.items[i].text);
   ACLEAR(d->hints);

   for (int i = 0; i < d->trace.count; i++)
      free(d->trace.items[i].text);
   ACLEAR(d->trace);

   tb_free(d->msg);
   free(d);
}

void diag_emit(diag_t *d)
{
   diag_femit(d, stderr);
}

void diag_show_source(diag_t *d, bool show)
{
   d->source = show;
}

void diag_set_consumer(diag_consumer_t fn)
{
   consumer = fn;
}

const char *diag_get_text(diag_t *d)
{
   return tb_get(d->msg);
}

const char *diag_get_hint(diag_t *d, int nth)
{
   assert(nth + 1 < d->hints.count);
   return d->hints.items[nth + 1].text;
}

const char *diag_get_trace(diag_t *d, int nth)
{
   assert(nth < d->trace.count);
   return d->trace.items[nth].text;
}

const loc_t *diag_get_loc(diag_t *d)
{
   if (d->hints.count > 0)
      return &(d->hints.items[0].loc);
   else
      return NULL;
}

int diag_hints(diag_t *d)
{
   return d->hints.count - 1;
}

int diag_traces(diag_t *d)
{
   return d->trace.count;
}

void diag_set_hint_fn(diag_hint_fn_t fn, void *context)
{
   hint_fn = fn;
   hint_ctx = context;
}

unsigned error_count(void)
{
   return n_errors;
}

void reset_error_count(void)
{
   n_errors = 0;
}

void fmt_loc(FILE *f, const loc_t *loc)
{
   // Legacy interface for debugging only
   diag_t *d = diag_new(DIAG_DEBUG, loc);
   diag_consumer_t old = consumer;
   consumer = NULL;
   diag_femit(d, f);
   consumer = old;
}

diag_level_t diag_severity(vhdl_severity_t severity)
{
   if (severity >= exit_severity)
      return DIAG_FATAL;

   switch (severity) {
   case SEVERITY_NOTE:    return DIAG_NOTE;
   case SEVERITY_WARNING: return DIAG_WARN;
   case SEVERITY_ERROR:
   case SEVERITY_FAILURE: return DIAG_ERROR;
   }

   return DIAG_ERROR;
}

void set_exit_severity(vhdl_severity_t severity)
{
   exit_severity = severity;

}
