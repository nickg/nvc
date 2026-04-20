//
//  Copyright (C) 2026  Nick Gasson
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
#include "hash.h"
#include "ident.h"
#include "option.h"
#include "parse.h"
#include "vlog/vlog-phase.h"

#include <assert.h>
#include <string.h>
#include <stdlib.h>

typedef struct _ifdef_stack ifdef_stack_t;

#define BEGIN(s)                                         \
   EXTEND(s);                                            \
   state.start_loc = LOC_INVALID;                        \

struct _ifdef_stack {
   ifdef_stack_t *next;
   loc_t          loc;
   int            state;
   bool           cond;
};

typedef A(ident_t) arg_list_t;
typedef A(text_buf_t *) def_list_t;

typedef struct {
   ident_t     name;
   loc_t       loc;
   text_buf_t *text;
   arg_list_t  args;
   def_list_t  defs;
   bool        has_args;
   bool        error;
} macro_t;

typedef enum {
   PP_INITIAL, PP_C_COMMENT,
} pp_mode_t;

static hash_t        *macros;
static text_buf_t    *output;
static ifdef_stack_t *ifdefs = NULL;
static bool           emit_locs;
static pp_mode_t      mode;
static parse_state_t  state;
static hash_t        *macro_args = NULL;

extern loc_t yylloc;
extern yylval_t yylval;
extern scan_buf_t yyspan;

static void p_block_of_text(void);
static void p_text_macro_usage(void);

static void free_macros(void)
{
   const void *key;
   void *value;
   for (hash_iter_t it = HASH_BEGIN; hash_iter(macros, &it, &key, &value); ) {
      macro_t *m = value;
      tb_free(m->text);
      for (int i = 0; i < m->defs.count; i++)
         tb_free(AGET(m->defs, i));
      ACLEAR(m->args);
      ACLEAR(m->defs);
      free(m);
   }

   hash_free(macros);
   macros = NULL;
}

static void undefall_macro(void)
{
   free_macros();
   macros = hash_new(64);
}

static void vlog_define_cb(const char *key, const char *value, void *ctx)
{
   macro_t *m = xcalloc(sizeof(macro_t));
   m->name = ident_new(key);
   m->loc  = LOC_INVALID;
   m->text = tb_new();

   tb_cat(m->text, value);

   hash_put(macros, m->name, m);
}

static token_t lex_identifier(scan_buf_t buf)
{
   char ch;
   while (scan_peek(buf, &ch)) {
      switch (ch) {
      case 'a'...'z':
      case 'A'...'Z':
      case '0'...'9':
      case '_':
         scan_advance(&buf);
         continue;
      default:
         goto out;
      }
   }

 out:
   return make_token(tID, buf, (yylval_t){ .span = buf });
}

static token_t lex_directive(scan_buf_t buf)
{
   char ch;
   while (scan_peek(buf, &ch)) {
      switch (ch) {
      case 'a'...'z':
      case 'A'...'Z':
      case '0'...'9':
      case '_':
         scan_advance(&buf);
         continue;
      default:
         goto out;
      }
   }

 out:
   // TODO: use a DFA here

   if (buf.len == 7 && strncmp(buf.ptr, "`define", buf.len) == 0)
      return make_token(tDEFINE, buf, (yylval_t){});
   if (buf.len == 6 && strncmp(buf.ptr, "`ifdef", buf.len) == 0)
      return make_token(tIFDEF, buf, (yylval_t){});
   if (buf.len == 7 && strncmp(buf.ptr, "`ifndef", buf.len) == 0)
      return make_token(tIFNDEF, buf, (yylval_t){});
   if (buf.len == 6 && strncmp(buf.ptr, "`endif", buf.len) == 0)
      return make_token(tENDIF, buf, (yylval_t){});
   if (buf.len == 5 && strncmp(buf.ptr, "`else", buf.len) == 0)
      return make_token(tCONDELSE, buf, (yylval_t){});
   if (buf.len == 6 && strncmp(buf.ptr, "`elsif", buf.len) == 0)
      return make_token(tCONDELSIF, buf, (yylval_t){});
   if (buf.len == 12 && strncmp(buf.ptr, "`undefineall", buf.len) == 0)
      return make_token(tUNDEFALL, buf, (yylval_t){});
   if (buf.len == 6 && strncmp(buf.ptr, "`undef", buf.len) == 0)
      return make_token(tUNDEF, buf, (yylval_t){});
   if (buf.len == 8 && strncmp(buf.ptr, "`include", buf.len) == 0)
      return make_token(tINCLUDE, buf, (yylval_t){});

   if (buf.len == 10 && strncmp(buf.ptr, "`timescale", buf.len) == 0)
      return make_token(tTEXT, buf, (yylval_t){ .span = buf });
   if (buf.len == 16 && strncmp(buf.ptr, "`default_nettype", buf.len) == 0)
      return make_token(tTEXT, buf, (yylval_t){ .span = buf });
   if (buf.len == 9 && strncmp(buf.ptr, "`resetall", buf.len) == 0)
      return make_token(tTEXT, buf, (yylval_t){ .span = buf });
   if (buf.len == 7 && strncmp(buf.ptr, "`pragma", buf.len) == 0)
      return make_token(tTEXT, buf, (yylval_t){ .span = buf });
   if (buf.len == 18 && strncmp(buf.ptr, "`unconnected_drive", buf.len) == 0)
      return make_token(tTEXT, buf, (yylval_t){ .span = buf });
   if (buf.len == 20 && strncmp(buf.ptr, "`nounconnected_drive", buf.len) == 0)
      return make_token(tTEXT, buf, (yylval_t){ .span = buf });
   if (buf.len == 15 && strncmp(buf.ptr, "`begin_keywords", buf.len) == 0)
      return make_token(tTEXT, buf, (yylval_t){ .span = buf });
   if (buf.len == 13 && strncmp(buf.ptr, "`end_keywords", buf.len) == 0)
      return make_token(tTEXT, buf, (yylval_t){ .span = buf });
   if (buf.len == 11 && strncmp(buf.ptr, "`celldefine", buf.len) == 0)
      return make_token(tTEXT, buf, (yylval_t){ .span = buf });
   if (buf.len == 14 && strncmp(buf.ptr, "`endcelldefine", buf.len) == 0)
      return make_token(tTEXT, buf, (yylval_t){ .span = buf });

   return make_token(tMACROUSAGE, buf, (yylval_t){ .span = buf });
}

static token_t lex_whitespace(scan_buf_t buf)
{
   char ch;
   while (scan_peek(buf, &ch)) {
      switch (ch) {
      case ' ':
      case '\t':
         scan_advance(&buf);
         continue;
      default:
         goto out;
      }
   }

 out:
   return make_token(tWHITESPACE, buf, (yylval_t){ .span = buf });
}

static token_t lex_newline(scan_buf_t buf)
{
   make_token(tNEWLINE, buf, (yylval_t){ .ch = '\n' });
   scan_next_line();
   return tNEWLINE;
}

static token_t lex_dos_newline(scan_buf_t buf)
{
   char ch;
   if (scan_peek(buf, &ch) && ch == '\n') {
      scan_advance(&buf);
      make_token(tNEWLINE, buf, (yylval_t){ .ch = '\n' });
      scan_next_line();
      return tNEWLINE;
   }

   return make_token(tTEXT, buf, (yylval_t){ .span = buf });
}

static token_t lex_continuation(scan_buf_t buf)
{
   char ch;
   if (scan_peek(buf, &ch)) {
      switch (ch) {
      case '\n':
         return make_token(tCONTINUATION, buf, (yylval_t){});
      case '\r':
         scan_advance(&buf);
         if (scan_peek(buf, &ch) && ch == '\n')
            return make_token(tCONTINUATION, buf, (yylval_t){});
         break;
      default:
         break;
      }
   }

   return make_token(tTEXT, buf, (yylval_t){ .span = buf });
}

static token_t lex_string(scan_buf_t buf)
{
   char ch;
   while (scan_peek(buf, &ch)) {
      switch (ch) {
      case '"':
         scan_advance(&buf);
         goto out;
      case '\n':
         return make_token(tTEXT, buf, (yylval_t){ .span = buf });
      default:
         scan_advance(&buf);
         continue;
      }
   }

 out:
   return make_token(tSTRING, buf, (yylval_t){ .span = buf });
}

static token_t lex_text(scan_buf_t buf)
{
   char ch;
   while (scan_peek(buf, &ch)) {
      switch (ch) {
      case '0'...'9':
      case 'A'...'F':
      case 'a'...'f':
      case 'h': case 'H':
      case 'o': case 'O':
      case 's': case 'S':
      case 'x': case 'X':
      case 'z': case 'Z': case '?':
      case '\'': case '_':
      case '+': case '-': case '*':
         scan_advance(&buf);
         continue;
      default:
         goto out;
      }
   }

 out:
   return make_token(tTEXT, buf, (yylval_t){ .span = buf });
}

static token_t lex_line_comment(scan_buf_t buf)
{
   char ch;
   while (scan_peek(buf, &ch)) {
      switch (ch) {
      case '\r':
      case '\n':
         goto out;
      default:
         scan_advance(&buf);
         continue;
      }
   }

 out:
   return make_token(tCOMMENT, buf, (yylval_t){ .span = buf });
}

static token_t lex_c_comment(scan_buf_t buf)
{
   assert(mode == PP_C_COMMENT);

   char last;
   if (!scan_next(&buf, &last)) {
      error_at(&yylloc, "unterminated block comment");
      return tEOF;
   }

   if (last == '\r')
      return lex_dos_newline(buf);
   else if (last == '\n')
      return lex_newline(buf);

   for (char peek; scan_peek(buf, &peek); last = peek) {
      if (last == '*' && peek == '/') {
         scan_advance(&buf);
         mode = PP_INITIAL;
         goto out;
      }
      else if (peek == '\n' || peek == '\r')
         goto out;
      else
         scan_advance(&buf);
   }

 out:
   return make_token(tCOMMENT, buf, (yylval_t){ .span = buf });
}

static token_t vlogpp_lex(void)
{
   scan_buf_t buf = get_input_buffer();

   if (mode == PP_C_COMMENT)
      return lex_c_comment(buf);

   char ch;
   if (!scan_next(&buf, &ch))
      return tEOF;

   switch (ch) {
   case 'a'...'z':
   case 'A'...'Z':
   case '_':
      return lex_identifier(buf);
   case '`':
      return lex_directive(buf);
   case ' ':
   case '\t':
      return lex_whitespace(buf);
   case '=':
      return make_token(tEQ, buf, (yylval_t){ .ch = '=' });
   case '(':
      return make_token(tLPAREN, buf, (yylval_t){ .ch = '(' });
   case ')':
      return make_token(tRPAREN, buf, (yylval_t){ .ch = ')' });
   case '{':
      return make_token(tLBRACE, buf, (yylval_t){ .ch = '{' });
   case '}':
      return make_token(tRBRACE, buf, (yylval_t){ .ch = '}' });
   case '[':
      return make_token(tLSQUARE, buf, (yylval_t){ .ch = '[' });
   case ']':
      return make_token(tRSQUARE, buf, (yylval_t){ .ch = ']' });
   case ',':
      return make_token(tCOMMA, buf, (yylval_t){ .ch = ',' });
   case '/':
      if (scan_peek(buf, &ch)) {
         if (ch == '/')
            return lex_line_comment(buf);
         else if (ch == '*') {
            scan_advance(&buf);
            mode = PP_C_COMMENT;
            return make_token(tCOMMENT, buf, (yylval_t){ .span = buf });
         }
      }
      return lex_text(buf);
   case '\\':
      return lex_continuation(buf);
   case '\r':
      return lex_dos_newline(buf);
   case '\n':
      return lex_newline(buf);
   case '"':
      return lex_string(buf);
   default:
      return lex_text(buf);
   }
}

static void p_expression(text_buf_t *tb, token_t end, bool stop_at_comma)
{
   BEGIN("expression");

   // 1800-2023 section 22.5.1: Actual arguments and defaults shall not
   // contain comma or right parenthesis characters outside matched
   // pairs of left and right parentheses (), square brackets [], braces
   // {}, double quotes " ", triple quotes """ """, or an escaped
   // identifier.

   while (not_at_token(end)) {
      const token_t tok = peek();
      switch (tok) {
      case tTEXT:
      case tWHITESPACE:
      case tID:
      case tSTRING:
         consume(tok);
         tb_catn(tb, state.last_lval.span.ptr, state.last_lval.span.len);
         break;
      case tLPAREN:
         consume(tLPAREN);
         tb_append(tb, '(');
         p_expression(tb, tRPAREN, false);
         consume(tRPAREN);
         tb_append(tb, ')');
         break;
      case tLSQUARE:
         consume(tLSQUARE);
         tb_append(tb, '[');
         p_expression(tb, tRSQUARE, false);
         consume(tRSQUARE);
         tb_append(tb, ']');
         break;
      case tLBRACE:
         consume(tLBRACE);
         tb_append(tb, '{');
         p_expression(tb, tRBRACE, false);
         consume(tRBRACE);
         tb_append(tb, '}');
         break;
      case tCOMMA:
         if (stop_at_comma)
            return;
         consume(tCOMMA);
         tb_append(tb, ',');
         break;
      default:
         one_of(tTEXT, tWHITESPACE, tID, tSTRING,
               tLPAREN, tLBRACE, tLSQUARE);
         return;
      }
   }
}

static ident_t p_identifier(void)
{
   if (consume(tID))
      return ident_new_n(state.last_lval.span.ptr, state.last_lval.span.len);
   else
      return error_marker();
}

static void p_formal_argument(macro_t *m)
{
   // simple_identifier [ = default_text ]

   BEGIN("formal argument");

   optional(tWHITESPACE);

   APUSH(m->args, p_identifier());

   optional(tWHITESPACE);

   text_buf_t *tb = tb_new();
   if (optional(tEQ))
      p_expression(tb, tRPAREN, true);

   APUSH(m->defs, tb);

}

static void p_list_of_formal_arguments(macro_t *m)
{
   // formal_argument { , formal_argument }

   BEGIN("list of formal arguments");

   do {
      p_formal_argument(m);
   } while (optional(tCOMMA));
}

static macro_t *p_text_macro_name(void)
{
   // text_macro_identifier [ ( list_of_formal_arguments ) ]

   BEGIN("text macro name");

   optional(tWHITESPACE);

   macro_t *m = xcalloc(sizeof(macro_t));
   m->name = p_identifier();
   m->loc  = state.last_loc;
   m->text = tb_new();

   if (optional(tLPAREN)) {
      m->has_args = true;
      p_list_of_formal_arguments(m);
      consume(tRPAREN);
   }

   hash_put(macros, m->name, m);
   return m;
}

static void p_text_macro_definition(void)
{
   // `define text_macro_name macro_text

   BEGIN("text macro definition");

   consume(tDEFINE);

   macro_t *m = p_text_macro_name();

   optional(tWHITESPACE);

   while (not_at_token(tNEWLINE)) {
      const token_t tok = peek();
      switch (tok) {
      case tTEXT:
      case tWHITESPACE:
      case tID:
      case tMACROUSAGE:
      case tSTRING:
         consume(tok);
         tb_catn(m->text, state.last_lval.span.ptr, state.last_lval.span.len);
         break;
      case tNEWLINE:
      case tLPAREN:
      case tRPAREN:
      case tLBRACE:
      case tRBRACE:
      case tLSQUARE:
      case tRSQUARE:
      case tEQ:
      case tCOMMA:
         consume(tok);
         tb_append(m->text, state.last_lval.ch);
         break;
      case tCONTINUATION:
         consume(tok);
         consume(tNEWLINE);
         tb_append(m->text, '\n');
         tb_append(output, '\n');
         break;
      case tCOMMENT:
         consume(tok);
         goto done;
      default:
         one_of(tTEXT, tWHITESPACE, tMACROUSAGE);
         break;
      }
   }
 done:

   consume(tNEWLINE);
   tb_append(output, '\n');
}

static void p_actual_argument(macro_t *m, int pos)
{
   // expression

   BEGIN("actual argument");

   optional(tWHITESPACE);

   LOCAL_TEXT_BUF tb = tb_new();

   p_expression(tb, tRPAREN, true);

   if (pos < m->args.count) {
      if (tb_is_empty(tb) && ! tb_is_empty(AGET(m->defs, pos)))
         hash_put(macro_args, AGET(m->args, pos), xstrdup(tb_get(AGET(m->defs, pos))));
      else
         hash_put(macro_args, AGET(m->args, pos), tb_claim(tb));
   }
}

static void p_list_of_actual_arguments(macro_t *m)
{
   // actual_argument { , actual_argument }

   BEGIN("list of actual arguments");

   int pos = 0;
   do {
      p_actual_argument(m, pos++);
   } while (optional(tCOMMA));

   if (pos > m->args.count && !m->error) {
      error_at(&state.last_loc, "macro '%pi' requires exactly %d arguments, %d were given",
               m->name, m->args.count, pos);
      m->error = true;
   }
   else if (pos != m->args.count) {
      for (; pos < m->args.count; pos++) {
         if (! tb_is_empty(AGET(m->defs, pos)))
            hash_put(macro_args, AGET(m->args, pos), xstrdup(tb_get(AGET(m->defs, pos))));
         else if (!m->error) {
            error_at(&state.last_loc,
                     "macro '%pi' is missing argument '%pi' with no default value",
                     m->name, AGET(m->args, pos));
            m->error = true;
            break;
         }
      }
   }
}

static void p_text_macro_usage(void)
{
   // `text_macro_identifier [ ( list_of_actual_arguments ) ]

   BEGIN("text macro usage");

   consume(tMACROUSAGE);

   scan_buf_t span = state.last_lval.span;

   if (span.len == 9 && strncmp(span.ptr, "`__LINE__", span.len) == 0) {
      tb_printf(output, "\"%d\"", yylloc.first_line);
      return;
   }

   if (span.len == 9 && strncmp(span.ptr, "`__FILE__", span.len) == 0) {
      tb_printf(output, "\"%s\"", loc_file_str(&yylloc));
      return;
   }

   ident_t name = ident_new_n(span.ptr + 1, span.len - 1);

   macro_t *m = hash_get(macros, name);
   if (m == NULL) {
      warn_at(&yylloc, "macro '%pi' undefined", name);

      // Prevent further warnings for this macro name
      macro_t *dummy = xcalloc(sizeof(macro_t));
      dummy->name = name;
      dummy->loc  = LOC_INVALID;
      dummy->text = tb_new();
      dummy->error = true;

      hash_put(macros, name, (m = dummy));
   }

   hash_t *old_args = macro_args;

   if (!m->has_args)
      macro_args = NULL;
   else {
      // Do not use optional() here as we must not consume any more of
      // the input buffer until after the macro is expanded if there are
      // no arguments
      scan_buf_t buf = get_input_buffer();
      char peek;
      while (scan_peek(buf, &peek) && (peek == ' ' || peek == '\t'))
         scan_advance(&buf);
      if (scan_peek(buf, &peek) && peek == '(') {
         optional(tWHITESPACE);
         consume(tLPAREN);
         macro_args = hash_new(16);
         p_list_of_actual_arguments(m);
         consume(tRPAREN);
      }
      else {
         error_at(&state.last_loc, "macro '%pi' requires arguments", m->name);
         m->error = true;
         macro_args = NULL;
      }
   }

   // Only emit location tracking when the macro starts at a token
   // boundary.  If the last character output was not whitespace or a
   // newline, the macro is expanding in the middle of a token (e.g.
   // 32'd`MACRO) and inserting directives would break the token.
   const size_t outlen = tb_len(output);
   const char last = outlen > 0 ? tb_get(output)[outlen - 1] : '\0';
   const bool at_boundary = last == '\0' || last == ' '
      || last == '\t' || last == '\n';

   if (emit_locs && at_boundary)
      tb_printf(output, "\n`__nvc_push %pi,%d:%d,%d\n", name,
                yylloc.first_line, yylloc.first_column, yylloc.column_delta);

   push_buffer(tb_get(m->text), tb_len(m->text), FILE_INVALID);

   while (not_at_token(tEOF))
      p_block_of_text();

   consume(tEOF);

   if (macro_args != NULL) {
      const void *key;
      void *value;
      for (hash_iter_t it = HASH_BEGIN;
           hash_iter(macro_args, &it, &key, &value); )
         free(value);
      hash_free(macro_args);
   }

   macro_args = old_args;

   pop_buffer();

   if (emit_locs && at_boundary)
      tb_printf(output, "\n`__nvc_pop\n");
}

static bool p_ifdef_condition(void)
{
   // text_macro_identifier | ( ifdef_macro_expression )

   BEGIN("ifdef condition");

   optional(tWHITESPACE);

   bool cond = true;
   switch (peek()) {
   case tID:
      {
         ident_t id = p_identifier();
         cond = hash_get(macros, id) != NULL;
      }
      break;
   case tLPAREN:
      consume(tLPAREN);
      fatal_at(&state.last_loc, "sorry, macro expressions are not yet "
               "supported");
   default:
      one_of(tID, tLPAREN);
   }

   return cond;
}

static void p_conditional_compilation_directive(void)
{
   // ifdef_or_ifndef ifdef_condition block_of_text
   //   { `elsif ifdef_condition block_of_text }
   //   [ `else block_of_text ]
   //   `endif

   BEGIN("conditional compilation directive");

   const token_t tok = one_of(tIFDEF, tIFNDEF);

   bool matched = false;

   {
      const bool cond = p_ifdef_condition() ^ (tok == tIFNDEF);

      ifdef_stack_t ifdef = {
         .next = ifdefs,
         .loc  = state.last_loc,
         .cond = cond && (ifdefs ? ifdefs->cond : true),
      };
      ifdefs = &ifdef;

      while (not_at_token(tCONDELSIF, tCONDELSE, tENDIF))
         p_block_of_text();

      matched |= cond;

      ifdefs = ifdef.next;
   }

   while (optional(tCONDELSIF)) {
      const bool cond = p_ifdef_condition() && !matched;

      ifdef_stack_t ifdef = {
         .next = ifdefs,
         .loc  = state.last_loc,
         .cond = cond && (ifdefs ? ifdefs->cond : true),
      };
      ifdefs = &ifdef;

      while (not_at_token(tCONDELSIF, tCONDELSE, tENDIF))
         p_block_of_text();

      matched |= ifdef.cond;

      ifdefs = ifdef.next;
   }

   if (optional(tCONDELSE)) {
      ifdef_stack_t ifdef = {
         .next = ifdefs,
         .loc  = state.last_loc,
         .cond = !matched && (ifdefs ? ifdefs->cond : true),
      };
      ifdefs = &ifdef;

      while (not_at_token(tENDIF))
         p_block_of_text();

      ifdefs = ifdef.next;
   }

   consume(tENDIF);
}

static void p_include_compiler_directive(void)
{
   // `include " filename " | `include < filename >

   BEGIN("include compiler directive");

   consume(tINCLUDE);

   optional(tWHITESPACE);

   switch (peek()) {
   case tSTRING:
      consume(tSTRING);
      break;
   case tERROR:
      // Already printed an error
      drop_token(&state);
      return;
   default:
      expect(tSTRING);
      return;
   }

   const loc_t loc = state.last_loc;
   char *file_name = xstrndup(state.last_lval.span.ptr + 1,
                              state.last_lval.span.len - 2);

   bool saw_newline = false;
   while (!saw_newline && not_at_token(tEOF)) {
      const token_t tok = peek();
      consume(tok);

      switch (tok) {
      case tNEWLINE:
         saw_newline = true;
         break;
      case tCOMMENT:
      case tWHITESPACE:
         break;
      default:
         parse_error(&state.last_loc, "only white space or a comment "
                     "may appear on the same line as an `include directive");
         drop_tokens_until(&state, tNEWLINE);
         saw_newline = true;
         break;
      }
   }

   if (emit_locs)
      tb_printf(output, "\n`__nvc_push \"%s\",%d:%d,%d\n", file_name,
                loc.first_line, loc.first_column, loc.column_delta);

   push_file(file_name, &loc);

   free(file_name);

   while (not_at_token(tEOF))
      p_block_of_text();

   consume(tEOF);

   pop_buffer();
}

static void p_undef_compiler_directive(void)
{
   // `undef text_macro_identifier

   BEGIN("undef compiler directive");

   consume(tUNDEF);

   optional(tWHITESPACE);

   ident_t id = p_identifier();

   hash_delete(macros, id);
}

static void p_block_of_text(void)
{
   BEGIN("block of text");

   const token_t tok = peek();
   switch (tok) {
   case tDEFINE:
      p_text_macro_definition();
      break;
   case tMACROUSAGE:
      p_text_macro_usage();
      break;
   case tIFDEF:
   case tIFNDEF:
      p_conditional_compilation_directive();
      break;
   case tINCLUDE:
      p_include_compiler_directive();
      break;
   case tUNDEF:
      p_undef_compiler_directive();
      break;
   case tUNDEFALL:
      consume(tUNDEFALL);
      undefall_macro();
      break;
   case tID:
      if (macro_args != NULL && (ifdefs == NULL || ifdefs->cond)) {
         ident_t name = p_identifier();
         const char *rep = hash_get(macro_args, name);
         if (rep != NULL)
            tb_cat(output, rep);
         else
            tb_catn(output, state.last_lval.span.ptr, state.last_lval.span.len);
         break;
      }
      // Fall-through
   case tTEXT:
   case tWHITESPACE:
   case tCOMMENT:
   case tSTRING:
      consume(tok);
      if (ifdefs == NULL || ifdefs->cond)
         tb_catn(output, state.last_lval.span.ptr, state.last_lval.span.len);
      break;
   case tNEWLINE:
      consume(tNEWLINE);
      tb_append(output, '\n');
      break;
   case tCONTINUATION:
      consume(tCONTINUATION);
      consume(tNEWLINE);
      tb_cat(output, "\\\n");
      break;
   case tLPAREN:
   case tRPAREN:
   case tLBRACE:
   case tRBRACE:
   case tLSQUARE:
   case tRSQUARE:
   case tEQ:
   case tCOMMA:
      consume(tok);
      if (ifdefs == NULL || ifdefs->cond)
         tb_append(output, state.last_lval.ch);
      break;
   default:
      one_of(tDEFINE, tMACROUSAGE, tIFDEF, tIFNDEF, tINCLUDE, tTEXT,
             tWHITESPACE);
      break;
   }
}

static void p_source_text(void)
{
   BEGIN("source text");

   while (not_at_token(tEOF))
      p_block_of_text();
}

void vlog_preprocess(text_buf_t *tb, bool precise)
{
   if (macros == NULL) {
      macros = hash_new(64);
      pp_defines_iter(vlog_define_cb, NULL);
   }
   else
      assert(opt_get_int(OPT_SINGLE_UNIT));

   assert(output == NULL);
   output = tb;

   assert(ifdefs == NULL);
   assert(macro_args == NULL);

   emit_locs = precise;
   mode = PP_INITIAL;

   state.n_correct = RECOVER_THRESH;
   state.tokenq_head = state.tokenq_tail = 0;
   state.lex_fn = vlogpp_lex;

   p_source_text();

   assert(ifdefs == NULL);
   assert(macro_args == NULL);
   output = NULL;

   if (!opt_get_int(OPT_SINGLE_UNIT))
      free_macros();
}
