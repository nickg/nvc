//
//  Copyright (C) 2011-2014  Nick Gasson
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
#include "phase.h"
#include "token.h"

#include <sys/types.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <string.h>
#include <stdarg.h>
#include <ctype.h>
#include <assert.h>
#include <stdbool.h>
#include <stdlib.h>

static const char *perm_linebuf = NULL;
static const char *perm_file_name = NULL;
static int         n_token_next_start = 0;
static int         n_row = 0;
static bool        last_was_newline = true;
static loc_t       yylloc;
static loc_t       start_loc;
static const char *read_ptr;
static const char *file_start;
static size_t      file_sz;
static int         n_errors = 0;
static bool        peek_valid = false;
static const char *hint_str = NULL;

yylval_t yylval;

int yylex(void);

#define F(list) list, ARRAY_LEN(list)
#define scan(...) _scan(1, __VA_ARGS__)
#define expect(...) _expect(1, __VA_ARGS__)

typedef struct {
   const char *old_hint;
   loc_t       old_start_loc;
} state_t;

#define BEGIN(s)                                                       \
   __attribute__((cleanup(_unbegin))) const state_t _state =           \
      { _hint(s), start_loc };                                         \
   start_loc = LOC_INVALID

#define CURRENT_LOC _diff_loc(&start_loc, &yylloc)

static const token_t f_library_clause[] = { tLIBRARY };
static const token_t f_use_clause[] = { tUSE };
static const token_t f_entity_declaration[] = { tENTITY };
static const token_t f_identifier[] = { tID };
static const token_t f_port_clause[] = { tPORT };
static const token_t f_generic_clause[] = { tGENERIC };

#define F_library_clause \
   F(f_library_clause), NULL
#define F_use_clause \
   F(f_use_clause), NULL
#define F_context_item \
   F_library_clause, F_use_clause, NULL
#define F_primary_unit \
   F_entity_declaration, F_configuration_declaration, \
      F_package_declaration, NULL
#define F_configuration_declaration \
   NULL   // TODO
#define F_package_declaration \
   NULL   // TODO
#define F_entity_declaration \
   F(f_entity_declaration), NULL
#define F_library_unit \
   F_primary_unit, NULL
#define F_identifier \
   F(f_identifier), NULL
#define F_port_clause \
   F(f_port_clause), NULL
#define F_generic_clause \
   F(f_generic_clause), NULL

static const char *_hint(const char *s)
{
   const char *old = hint_str;
   hint_str = s;
   return old;
}

static void _unbegin(const state_t *s)
{
   hint_str  = s->old_hint;
   start_loc = s->old_start_loc;
}

static const char *token_str(token_t tok)
{
   static const char *token_strs[] = {
      "end of file", "identifier", "entity", "is", "end", "generic", "port",
      "constant", "component", "configuration", "architecture", "of", "begin",
      "for", "type", "to", "all", "in", "out", "buffer", "bus", "unaffected",
      "signal", "downto", "process", "postponed", "wait", "report", "(", ")",
      ";", ":=", ":", ",", "integer"
   };

   if ((size_t)tok >= ARRAY_LEN(token_strs))
      return "???";
   else
      return token_strs[tok];
}

static token_t peek(void)
{
   static token_t p;

   if (peek_valid)
      return p;
   else {
      peek_valid = true;
      return (p = yylex());
   }
}

static void consume(token_t tok)
{
   const token_t got = peek();
   if (tok != got) {
      error_at(&yylloc, "expected $yellow$%s$$ but found $yellow$%s$$ while "
               "parsing $yellow$%s$$",
               token_str(tok), token_str(got), hint_str);
      n_errors++;
   }

   if (start_loc.linebuf == NULL)
      start_loc = yylloc;

   peek_valid = false;
}

static bool optional(token_t tok)
{
   if (peek() == tok) {
      consume(tok);
      return true;
   }
   else
      return false;
}

static void _expect(int dummy, ...)
{
   va_list ap;
   va_start(ap, dummy);

   LOCAL_TEXT_BUF tb = tb_new();

   tb_printf(tb, "unexpected $yellow$%s$$ while parsing $yellow$%s$$, "
             "expecting one of ", token_str(peek()), hint_str);

   bool first = true;
   for (;;) {
      const token_t *list = va_arg(ap, const token_t *);
      if (list == NULL)
         break;

      const size_t len = va_arg(ap, size_t);

      if (!first)
         tb_printf(tb, ", ");

      for (size_t i = 0; i < len; i++) {
         if (i != 0)
            tb_printf(tb, ", ");
         tb_printf(tb, "$yellow$%s$$", token_str(list[i]));
      }

      first = false;
   }

   error_at(&yylloc, tb_get(tb));
   n_errors++;

   va_end(ap);
}

static bool _scan(int dummy, ...)
{
   va_list ap;
   va_start(ap, dummy);

   token_t p = peek();
   bool found = false;

   while (!found) {
      const token_t *list = va_arg(ap, const token_t *);
      if (list == NULL)
         break;

      const size_t len = va_arg(ap, size_t);

      for (size_t i = 0; i < len; i++) {
         if (list[i] == p)
            found = true;
      }
   }

   va_end(ap);
   return found;
}

static const loc_t *_diff_loc(const loc_t *start, const loc_t *end)
{
   static loc_t result;

   result.first_line   = start->first_line;
   result.first_column = start->first_column;
   result.last_line    = end->last_line;
   result.last_column  = end->last_column;
   result.file         = start->file;
   result.linebuf      = start->linebuf;

   return &result;
}

static ident_t p_identifier(void)
{
   consume(tID);
   ident_t i = ident_new(yylval.s);
   free(yylval.s);
   return i;
}

static void p_library_clause(void)
{

}

static void p_use_clause(void)
{

}

static void p_context_item(void)
{
   // library_clause | use_clause

   BEGIN("context item");

   if (scan(F_library_clause))
      p_library_clause();
   else if (scan(F_use_clause))
      p_use_clause();
   else
      expect(F_library_clause, F_use_clause);
}

static void p_context_clause(void)
{
   // { context_item }

   BEGIN("context clause");

   while (scan(F_context_item)) {
      p_context_item();
   }
}

static void p_generic_clause(void)
{
   // generic ( generic_list ) ;

   BEGIN("generic clause");

   consume(tGENERIC);
}

static void p_port_clause(void)
{
   // port ( port_list ) ;

   BEGIN("port clause");

   consume(tPORT);
   consume(tLPAREN);

   // p_port_list()

   consume(tRPAREN);
}

static void p_entity_header(void)
{
   // [ generic_clause ] [ port_clause ]

   BEGIN("entity header");

   if (scan(F_generic_clause))
      p_generic_clause();

   if (scan(F_port_clause))
      p_port_clause();
}

static tree_t p_entity_declaration(void)
{
   // entity identifier is entity_header entity_declarative_part
   //   [ begin entity_statement_part ] end [ entity ] [ entity_simple_name ] ;

   BEGIN("entity declaration");

   tree_t t = tree_new(T_ENTITY);

   consume(tENTITY);

   ident_t id = p_identifier();
   tree_set_ident(t, id);

   consume(tIS);

   p_entity_header();

   // p_entity_header()
   // p_entity_declarative_part()
   //   [ begin entity_statement_part ]

   consume(tEND);
   optional(tENTITY);

   if (scan(F_identifier)) {
      ident_t tail_id = p_identifier();
      (void)tail_id;
      // XXX: test me
   }

   consume(tSEMI);

   tree_set_loc(t, CURRENT_LOC);
   return t;
}

static tree_t p_primary_unit(void)
{
   // entity_declaration | configuration_declaration | package_declaration

   BEGIN("primary unit");

   if (scan(F_entity_declaration))
      return p_entity_declaration();
   else {
      expect(F_primary_unit);
      return NULL;
   }
}

static tree_t p_library_unit(void)
{
   // primary_unit | secondary_unit

   BEGIN("library unit");

   if (scan(F_primary_unit))
      return p_primary_unit();
   else {
      expect(F_library_unit);
      return NULL;
   }
}

static tree_t p_design_unit(void)
{
   BEGIN("design unit");

   p_context_clause();
   tree_t unit = p_library_unit();

   return unit;
}

void begin_token(char *tok)
{
   const char *newline = strrchr(tok, '\n');
   int n_token_start, n_token_length;
   if (newline != NULL) {
      n_token_start = 0;
      n_token_length = strlen(tok) - (newline - tok);
      n_token_next_start = n_token_length - 1;
   }
   else {
      n_token_start = n_token_next_start;
      n_token_length = strlen(tok);
      n_token_next_start += n_token_length;
   }

   const int last_col = n_token_start + n_token_length - 1;

   yylloc.first_line   = MIN(n_row, LINE_INVALID);
   yylloc.first_column = MIN(n_token_start, COLUMN_INVALID);
   yylloc.last_line    = MIN(n_row, LINE_INVALID);
   yylloc.last_column  = MIN(last_col, COLUMN_INVALID);
   yylloc.file         = perm_file_name;
   yylloc.linebuf      = perm_linebuf;
}

int get_next_char(char *b, int max_buffer)
{
   if (last_was_newline) {
      n_row += 1;
      perm_linebuf = read_ptr;
      last_was_newline = false;
   }

   const bool eof = read_ptr >= file_start + file_sz;
   if (eof)
      return 0;
   else
      *b = *read_ptr++;

   if (perm_linebuf == NULL)
      perm_linebuf = read_ptr;

   if (*b == '\n')
      last_was_newline = true;

   return *b == 0 ? 0 : 1;
}

void input_from_file(const char *file)
{
   int fd = open(file, O_RDONLY);
   if (fd < 0)
      fatal_errno("opening %s", file);

   struct stat buf;
   if (fstat(fd, &buf) != 0)
      fatal_errno("fstat");

   if (!S_ISREG(buf.st_mode))
      fatal("opening %s: not a regular file", file);

   file_sz = buf.st_size;

   file_start = mmap(NULL, file_sz, PROT_READ, MAP_PRIVATE, fd, 0);
   if (file_start == MAP_FAILED)
      fatal_errno("mmap");

   read_ptr           = file_start;
   last_was_newline   = true;
   perm_file_name     = strdup(file);
   n_row              = 0;
   n_token_next_start = 0;
}

tree_t parse(void)
{
   n_errors = 0;

   tree_t unit = p_design_unit();
   if (n_errors > 0)
      return NULL;
   else
      return unit;
}

int parse_errors(void)
{
   return n_errors;
}
