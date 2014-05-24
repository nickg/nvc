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
static int         n_correct = 0;

yylval_t yylval;

int yylex(void);

#define F(list) list, ARRAY_LEN(list)
#define scan(...) _scan(1, __VA_ARGS__, -1)
#define expect(...) _expect(1, __VA_ARGS__, -1)
#define one_of(...) _one_of(1, __VA_ARGS__, -1)

#define RECOVER_THRESH 5
#define TRACE_PARSE    1

#if TRACE_PARSE
static int depth = 0;
#endif

typedef struct {
   const char *old_hint;
   loc_t       old_start_loc;
} state_t;

#define BEGIN(s)                                                       \
   __attribute__((cleanup(_pop_state))) const state_t _state =         \
      { hint_str, start_loc };                                         \
   start_loc = LOC_INVALID;                                            \
   hint_str  = s;                                                      \
   _push_state(&_state);

#define CURRENT_LOC _diff_loc(&start_loc, &yylloc)

static const token_t f_library_clause[] = { tLIBRARY };
static const token_t f_use_clause[] = { tUSE };
static const token_t f_entity_declaration[] = { tENTITY };
static const token_t f_identifier[] = { tID };
static const token_t f_port_clause[] = { tPORT };
static const token_t f_generic_clause[] = { tGENERIC };
static const token_t f_interface_constant_declaration[] = { tCONSTANT };
static const token_t f_identifier_list[] = { tID };
static const token_t f_interface_signal_declaration[] = { tSIGNAL };
static const token_t f_interface_variable_declaration[] = { tSIGNAL };
static const token_t f_interface_file_declaration[] = { tFILE };

static tree_t p_expression(void);

static void _pop_state(const state_t *s)
{
#if TRACE_PARSE
   depth--;
   for (int i = 0; i < depth; i++)
      printf(" ");
   printf("<-- %s\n", hint_str);
#endif
   hint_str  = s->old_hint;
   start_loc = s->old_start_loc;
}

static void _push_state(const state_t *s)
{
#if TRACE_PARSE
   for (int i = 0; i < depth; i++)
      printf(" ");
   printf("--> %s\n", hint_str);
   depth++;
#endif
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

static bool consume(token_t tok)
{
   const token_t got = peek();
   if (tok != got) {
      if (n_correct >= RECOVER_THRESH) {
         error_at(&yylloc, "expected $yellow$%s$$ but found $yellow$%s$$ while "
                  "parsing $yellow$%s$$",
                  token_str(tok), token_str(got), hint_str);
         n_errors++;
      }
      n_correct = 0;
   }
   else
      n_correct++;

   if (start_loc.linebuf == NULL)
      start_loc = yylloc;

   peek_valid = false;

   return (tok == got);
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

static void _vexpect(va_list ap)
{
   LOCAL_TEXT_BUF tb = tb_new();

   tb_printf(tb, "unexpected $yellow$%s$$ while parsing $yellow$%s$$, "
             "expecting one of ", token_str(peek()), hint_str);

   bool first = true;
   for (;;) {
      const int tok = va_arg(ap, int);
      if (tok == -1)
         break;

      if (!first)
         tb_printf(tb, ", ");

      tb_printf(tb, "$yellow$%s$$", token_str(tok));

      first = false;
   }

   if (n_correct >= RECOVER_THRESH) {
      error_at(&yylloc, tb_get(tb));
      n_errors++;
   }

   n_correct = 0;
}

static void _expect(int dummy, ...)
{
   va_list ap;
   va_start(ap, dummy);
   _vexpect(ap);
   va_end(ap);
}

static bool _scan(int dummy, ...)
{
   va_list ap;
   va_start(ap, dummy);

   token_t p = peek();
   bool found = false;

   while (!found) {
      const token_t tok = va_arg(ap, token_t);
      if (tok == -1)
         break;
      else if (p == tok)
         found = true;
   }

   va_end(ap);
   return found;
}

static int _one_of(int dummy, ...)
{
   va_list ap;
   va_start(ap, dummy);

   token_t p = peek();
   bool found = false;

   while (!found) {
      const token_t tok = va_arg(ap, token_t);
      if (p == tok)
         found = true;
   }

   va_end(ap);

   if (found) {
      consume(p);
      return p;
   }
   else {
      va_start(ap, dummy);
      _vexpect(ap);
      va_end(ap);

      return -1;
   }
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
   // basic_identifier | extended_identifier

   if (consume(tID)) {
      ident_t i = ident_new(yylval.s);
      free(yylval.s);
      return i;
   }
   else
      return ident_new("error");
}

static void p_identifier_list(void)
{
   // identifier { , identifier }

   p_identifier();

   while (optional(tCOMMA))
      p_identifier();
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

   switch (peek()) {
   case tLIBRARY:
      p_library_clause();
      break;

   case tUSE:
      p_use_clause();
      break;

   default:
      expect(tLIBRARY, tUSE);
   }
}

static void p_context_clause(void)
{
   // { context_item }

   BEGIN("context clause");

   while (scan(tLIBRARY, tUSE)) {
      p_context_item();
   }
}

static port_mode_t p_mode(void)
{
   // in | out | inout | buffer | linkage

   switch (one_of(tIN, tOUT, tINOUT, tBUFFER, tLINKAGE)) {
   case tIN:
      return PORT_IN;
   case tOUT:
      return PORT_OUT;
   case tINOUT:
      return PORT_INOUT;
   case tBUFFER:
      return PORT_BUFFER;
   case tLINKAGE:
      return PORT_LINKAGE;
   default:
      return PORT_INVALID;
   }
}

static tree_t p_simple_name(ident_t head)
{
   // identifier

   tree_t t = tree_new(T_REF);
   tree_set_ident(t, head);
   tree_set_loc(t, CURRENT_LOC);

   return t;
}

static ident_t p_operator_symbol(void)
{
   // string_literal

   consume(tSTRING);

   for (char *p = yylval.s; *p != '\0'; p++)
      *p = tolower((int)*p);
   ident_t id = ident_new(yylval.s);
   free(yylval.s);

   return id;
}

static tree_t p_name(ident_t head)
{
   // simple_name | operator_symbol | selected_name | indexed_name
   //   | slice_name | attribute_name

   BEGIN("name");

   if (peek() == tSTRING) {
      assert(head == NULL);

      ident_t op = p_operator_symbol();

      tree_t t = tree_new(T_REF);
      tree_set_ident(t, op);
      tree_set_loc(t, CURRENT_LOC);
      return t;
   }
   else {
      if (head == NULL)
         head = p_identifier();

      switch (peek()) {
      default:
         return p_simple_name(head);
      }
   }
}

static type_t p_type_mark(ident_t head)
{
   // name

   if (head == NULL)
      head = p_identifier();

   type_t t = type_new(T_UNRESOLVED);
   type_set_ident(t, head);
   return t;
}

static type_t p_subtype_indication(void)
{
   // [ name ] type_mark [ constraint ]

   BEGIN("subtype indication");

   type_t t = type_new(T_SUBTYPE);

   ident_t head = p_identifier();

   if (scan(tID)) {
      tree_t rname = p_name(head);
      // XXX: check name is resolution_function_name
      type_set_resolution(t, rname);
      head = NULL;
   }

   type_t base = p_type_mark(head);
   type_set_base(t, base);

   // p_constraint()

   return t;
}

static tree_t p_abstract_literal(void)
{
   // decimal_literal | based_literal

   BEGIN("abstract literal");

   tree_t t = tree_new(T_LITERAL);

   switch (one_of(tINT, tREAL)) {
   case tINT:
      tree_set_subkind(t, L_INT);
      tree_set_ival(t, yylval.n);
      break;

   case tREAL:
      tree_set_subkind(t, L_REAL);
      tree_set_dval(t, yylval.d);
      break;
   }

   tree_set_loc(t, CURRENT_LOC);
   return t;
}

static tree_t p_numeric_literal(void)
{
   // abstract_literal | physical_literal

   BEGIN("numeric literal");

   return p_abstract_literal();
}

static tree_t p_literal(void)
{
   // numeric_literal | enumeration_literal | string_literal
   // | bit_string_literal | null

   BEGIN("literal");

   switch (peek()) {
   case tNULL:
      {
         consume(tNULL);

         tree_t t = tree_new(T_LITERAL);
         tree_set_loc(t, CURRENT_LOC);
         tree_set_subkind(t, L_NULL);
         return t;
      }

   case tINT:
   case tREAL:
      return p_numeric_literal();

   default:
      expect(tNULL, tINT, tREAL);
      return tree_new(T_OPEN);
   }
}

static tree_t p_primary(void)
{
   // name | literal | aggregate | function_call | qualified_expression
   //   | type_conversion | allocator | ( expression )

   BEGIN("primary");

   switch (peek()) {
   case tLPAREN:
      {
         tree_t sub = p_expression();
         consume(tRPAREN);
         return sub;
      }

   case tINT:
   case tREAL:
   case tNULL:
      return p_literal();

   default:
      expect(tLPAREN, tLITERAL, tINT, tREAL, tNULL);
      return tree_new(T_OPEN);
   }
}

static tree_t p_factor(void)
{
   // primary [ ** primary ] | abs primary | not primary

   BEGIN("factor");

   tree_t operand = p_primary();

   // XXX
   return operand;
}

static tree_t p_term(void)
{
   // factor { multiplying_operator factor }

   BEGIN("term");

   tree_t left = p_factor();

   // XXX
   return left;
}

static tree_t p_simple_expression(void)
{
   // [ sign ] term { adding_operator term }

   BEGIN("simple expression");

   // p_sign()

   tree_t left = p_term();

   // XXX
   return left;
}

static tree_t p_shift_expression(void)
{
   // simple_expression [ shift_operator simple_expression ]

   BEGIN("shift expression");

   tree_t left = p_simple_expression();

   // XXX
   return left;
}

static tree_t p_relation(void)
{
   // shift_expression [ relational_operator shift_expression ]

   BEGIN("relation");

   tree_t left = p_shift_expression();

   // XXX
   return left;
}

static tree_t p_expression(void)
{
   // relation { and relation } | relation { or relation }
   //   | relation { xor relation } | relation [ nand relation ]
   //   | relation [ nor relation ] | relation { xnor relation }

   BEGIN("expression");

   tree_t left = p_relation();

   // XXX
   return left;
}

static void p_interface_constant_declaration(void)
{
   // [ constant ] identifier_list : [ in ] subtype_indication [ := expression ]

   BEGIN("interface constant declaration");

   optional(tCONSTANT);

   p_identifier_list();

   consume(tCOLON);
   optional(tIN);

   p_subtype_indication();

   if (optional(tASSIGN))
      p_expression();
}

static void p_interface_signal_declaration(void)
{
   // [signal] identifier_list : [ mode ] subtype_indication [ bus ]
   //    [ := expression ]

   BEGIN("interface signal declaration");

   optional(tSIGNAL);

   p_identifier_list();

   consume(tCOLON);

   if (scan(tIN, tOUT, tINOUT, tBUFFER, tLINKAGE))
      p_mode();

   p_subtype_indication();

   optional(tBUS);

   if (optional(tASSIGN))
      p_expression();
}

static void p_interface_variable_declaration(void)
{
   // [variable] identifier_list : [ mode ] subtype_indication [ := expression ]

   BEGIN("interface variable declaration");
}

static void p_interface_file_declaration(void)
{
   // file identifier_list : subtype_indication

   BEGIN("interface file declaration");
}

static void p_interface_declaration(class_t def_class)
{
   // interface_constant_declaration | interface_signal_declaration
   //   | interface_variable_declaration | interface_file_declaration

   BEGIN("interface declaration");

   const token_t p = peek();
   switch (p) {
   case tCONSTANT:
      p_interface_constant_declaration();
      break;

   case tSIGNAL:
      p_interface_signal_declaration();
      break;

   case tVARIABLE:
      p_interface_variable_declaration();
      break;

   case tFILE:
      p_interface_file_declaration();
      break;

   case tID:
      {
         switch (def_class) {
         case C_CONSTANT:
            p_interface_constant_declaration();
            break;

         case C_SIGNAL:
            p_interface_signal_declaration();
            break;

         case C_VARIABLE:
            p_interface_variable_declaration();
            break;

         default:
            assert(false);
         }
      }
      break;

   default:
      expect(tCONSTANT, tSIGNAL, tVARIABLE, tFILE, tID);
   }
}

static void p_interface_element(class_t def_class)
{
   // interface_declaration

   BEGIN("interface element");

   p_interface_declaration(def_class);
}

static void p_interface_list(class_t def_class)
{
   // interface_element { ; interface_element }

   BEGIN("interface list");

   p_interface_element(def_class);

   while (optional(tSEMI))
      p_interface_element(def_class);
}

static void p_port_list(void)
{
   // port_list ::= interface_list

   BEGIN("port list");

   p_interface_list(C_SIGNAL);
}

static void p_port_clause(void)
{
   // port ( port_list ) ;

   BEGIN("port clause");

   consume(tPORT);
   consume(tLPAREN);

   p_port_list();

   consume(tRPAREN);
   consume(tSEMI);
}

static void p_generic_list(void)
{
   // generic_list ::= interface_list

   BEGIN("generic list");

   p_interface_list(C_CONSTANT);
}

static void p_generic_clause(void)
{
   // generic ( generic_list ) ;

   BEGIN("generic clause");

   consume(tGENERIC);
   consume(tLPAREN);

   p_generic_list();

   consume(tRPAREN);
   consume(tSEMI);
}

static void p_entity_header(void)
{
   // [ generic_clause ] [ port_clause ]

   BEGIN("entity header");

   if (scan(tGENERIC))
      p_generic_clause();

   if (scan(tPORT))
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

   if (peek() == tID) {
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

   switch (peek()) {
   case tENTITY:
      return p_entity_declaration();

   default:
      expect(tENTITY);
      return NULL;
   }
}

static tree_t p_library_unit(void)
{
   // primary_unit | secondary_unit

   BEGIN("library unit");

   switch (peek()) {
   case tENTITY:
      return p_primary_unit();

   default:
      expect(tENTITY);
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
   n_errors  = 0;
   n_correct = RECOVER_THRESH;

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
