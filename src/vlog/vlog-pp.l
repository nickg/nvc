/* -*- mode: c; c-basic-offset: 3 -*- */

/*
 *  Copyright (C) 2023-2026  Nick Gasson
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

%option prefix="vlogpp_"
%option noyywrap
%option nounput
%option noinput

%{
#include "util.h"
#include "array.h"
#include "diag.h"
#include "hash.h"
#include "ident.h"
#include "option.h"
#include "scan.h"
#include "vlog/vlog-phase.h"

#include <assert.h>

#define YY_INPUT(buf, result, max_size) {    \
      result = get_next_char(buf, max_size); \
      if (result <= 0)                       \
         result = YY_NULL;                   \
   }

#define YY_USER_ACTION convert_line_ending();

typedef struct _ifdef_stack ifdef_stack_t;

struct _ifdef_stack {
   ifdef_stack_t *next;
   loc_t          loc;
   int            state;
   bool           cond;
};

static int            caller = 0;
static shash_t       *macros;
static text_buf_t    *macro_text;
static text_buf_t    *output;
static ifdef_stack_t *ifdefs = NULL;
static bool           emit_locs;
static bool           continuation = false;
static parse_state_t  state;

static void undefall_macro(void);
static void convert_line_ending(void);
static void p_block_of_text(void);

extern loc_t yylloc;
extern yylval_t yylval;
%}

ID              [a-zA-Z_]([a-zA-Z0-9_$])*
VLOG_ID         [a-zA-Z_]([a-zA-Z0-9_$])*
SPACE           [ \t\r]

%x COMMENT C_COMMENT INCLUDE INCLUDE2 MACRO_DEF MACRO_TEXT MACRO_PARAM

%%

"//"                    { caller = YY_START;
                          BEGIN(COMMENT);
                          REJECT;
                        }

<COMMENT>[^\r\n]        { yylval.ch = yytext[0]; return tTEXT; }
<COMMENT>\n\r           |
<COMMENT>\r\n           |
<COMMENT>\n             |
<COMMENT>\r             { yylval.ch = '\n';
                          BEGIN(caller);
                          return tTEXT;
                        }

"/*"                    { caller = YY_START;
                          BEGIN(C_COMMENT);
                          REJECT;
                        }
<C_COMMENT>[^\r\n]      { yylval.ch = yytext[0]; return tTEXT; }
<C_COMMENT>\n\r         |
<C_COMMENT>\r\n         |
<C_COMMENT>\n           |
<C_COMMENT>\r           { yylval.ch = '\n'; return tTEXT; }
<C_COMMENT>"*/"         { BEGIN(caller); REJECT; }

`define                 { return tDEFINE; }
`ifdef                  { return tIFDEF; }
`ifndef                 { return tIFNDEF; }
`endif                  { return tENDIF; }
`elsif                  { return tCONDELSIF; }
`else                   { return tCONDELSE; }
`undef                  { return tUNDEF; }
`undefineall            { return tUNDEFALL; }
`include                { return tINCLUDE; }

`__FILE__               { tb_printf(output, "\"%s\"", loc_file_str(&yylloc)); }
`__LINE__               { tb_printf(output, "\"%d\"", yylloc.first_line); }

`timescale              |
`default_nettype        |
`resetall               |
`pragma                 |
`unconnected_drive      |
`nounconnected_drive    |
`begin_keywords         |
`end_keywords           |
`celldefine             |
`endcelldefine          { tb_cat(output, vlogpp_text); }

`{VLOG_ID}              { yylval.ident = ident_new(yytext + 1);
                          return tMACROUSAGE;
                        }

<MACRO_DEF>{SPACE}      { }
<MACRO_DEF>{VLOG_ID}    { yylval.ident = ident_new(yytext); return tID; }
<MACRO_DEF>.            { return tERROR; }

<MACRO_TEXT>\\          { continuation = true; }
<MACRO_TEXT>[^\r\n]     { if (continuation) {
                             tb_append(macro_text, '\\');
                             continuation = false;
                          }
                          tb_cat(macro_text, vlogpp_text);
                        }
<MACRO_TEXT>\n\r        |
<MACRO_TEXT>\r\n        |
<MACRO_TEXT>\n          |
<MACRO_TEXT>\r          { if (continuation) {
                             tb_append(macro_text, '\n');
                             tb_append(output, '\n');
                             continuation = false;
                          }
                          else {
                             tb_cat(output, vlogpp_text);
                             return tMACROTEXT;
                          }
                        }

<MACRO_PARAM>{SPACE}    { }
<MACRO_PARAM>{VLOG_ID}  { yylval.ident = ident_new(yytext); return tID; }
<MACRO_PARAM>"("        { return tLPAREN; }
<MACRO_PARAM>")"        { return tRPAREN; }
<MACRO_PARAM>.          { yylval.ch = yytext[0]; return tTEXT; }

<INCLUDE>"\""[^\"]*"\"" { yytext[strlen(yytext) - 1] = '\0';
                          yylval.str = xstrdup(yytext + 1);
                          return tSTRING;
                        }
<INCLUDE>{SPACE}        { }
<INCLUDE>.              { error_at(&yylloc, "expected file name");
                          return tERROR;
                        }

<INCLUDE2>{SPACE}       { }
<INCLUDE2>"//"[^\r\n]*  { }
<INCLUDE2>"/*"          { caller = YY_START;
                          BEGIN(C_COMMENT);
                        }
<INCLUDE2>\n\r          |
<INCLUDE2>\r\n          |
<INCLUDE2>\n            |
<INCLUDE2>\r            { yylval.ch = '\n'; return tTEXT; }
<INCLUDE2>.             { error_at(&yylloc, "only white space or a comment "
                                   "may appear on the same line as an "
                                   "`include directive");
                          return tERROR;
                        }

[^\r\n]                 { yylval.ch = yytext[0]; return tTEXT; }
\n\r                    |
\r\n                    |
\n                      |
\r                      { yylval.ch = '\n'; return tTEXT; }

<*><<EOF>>              { return tEOF; }

%%

#define EXTEND(s)                                                      \
   __attribute__((cleanup(_pop_state), unused))                        \
   const rule_state_t _state = { state.hint_str, state.start_loc };    \
   state.hint_str = s;                                                 \
   _push_state(&_state);

#define PBEGIN(s)                                        \
   EXTEND(s);                                            \
   state.start_loc = LOC_INVALID;                        \

#if TRACE_PARSE
static void _push_state(const rule_state_t *s);
#else
#define _push_state(s)
#endif

static inline void _pop_state(const rule_state_t *r)
{
#if TRACE_PARSE
   printf("%*s<-- %s\n", state.depth--, "", state.hint_str);
#endif

   state.hint_str = r->old_hint;

   if (r->old_start_loc.first_line != LINE_INVALID)
      state.start_loc = r->old_start_loc;
}

#if TRACE_PARSE
static inline void _push_state(const rule_state_t *r)
{
   printf("%*s--> %s\n", state.depth++, "", state.hint_str);
}
#endif

static token_t peek_nth(int n)
{
   while (((state.tokenq_head - state.tokenq_tail) & (TOKENQ_SIZE - 1)) < n) {
      const token_t token = vlogpp_lex();

      int next = (state.tokenq_head + 1) & (TOKENQ_SIZE - 1);
      assert(next != state.tokenq_tail);

      extern yylval_t yylval;

      state.tokenq[state.tokenq_head].token = token;
      state.tokenq[state.tokenq_head].lval  = yylval;
      state.tokenq[state.tokenq_head].loc   = yylloc;

      state.tokenq_head = next;
   }

   const int pos = (state.tokenq_tail + n - 1) & (TOKENQ_SIZE - 1);
   return state.tokenq[pos].token;
}

static void drop_token(void)
{
   assert(state.tokenq_head != state.tokenq_tail);

   if (state.start_loc.first_line == LINE_INVALID)
      state.start_loc = state.tokenq[state.tokenq_tail].loc;

   state.last_lval = state.tokenq[state.tokenq_tail].lval;
   state.last_loc  = state.tokenq[state.tokenq_tail].loc;

   state.tokenq_tail = (state.tokenq_tail + 1) & (TOKENQ_SIZE - 1);

   state.nopt_hist = 0;
}

static void _vexpect(va_list ap)
{
   if (state.n_correct >= RECOVER_THRESH) {
      diag_t *d = diag_new(DIAG_ERROR, &(state.tokenq[state.tokenq_tail].loc));
      diag_printf(d, "unexpected $yellow$%s$$ while parsing %s, expecting ",
                  token_str(peek()), state.hint_str);

      bool first = true;
      for (int i = 0; i < state.nopt_hist; i++) {
         diag_printf(d, "%s$yellow$%s$$", i == 0 ? "one of " : ", ",
                     token_str(state.opt_hist[i]));
         first = false;
      }

      int tok = va_arg(ap, int);
      while (tok != -1) {
         const int tmp = tok;
         tok = va_arg(ap, int);

         if (first && (tok != -1))
            diag_printf(d, "one of ");
         else if (!first)
            diag_printf(d, (tok == -1) ? " or " : ", ");

         diag_printf(d, "$yellow$%s$$", token_str(tmp));

         first = false;
      }

      diag_hint(d, &(state.tokenq[state.tokenq_tail].loc),
                "this token was unexpected");
      diag_emit(d);
   }

   state.n_correct = 0;

   drop_token();
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
      const int tok = va_arg(ap, token_t);
      if (tok == -1)
         break;
      else if (p == tok)
         found = true;
   }

   va_end(ap);
   return found;
}

static bool consume(token_t tok)
{
   const token_t got = peek();
   if (tok != got) {
      expect(tok);
      return false;
   }
   else {
      state.n_correct++;
      drop_token();
      return true;
   }
}

static bool optional(token_t tok)
{
   if (peek() == tok) {
      consume(tok);
      return true;
   }
   else {
      if (state.nopt_hist < ARRAY_LEN(state.opt_hist))
         state.opt_hist[state.nopt_hist++] = tok;
      return false;
   }
}

static int _one_of(int dummy, ...)
{
   va_list ap;
   va_start(ap, dummy);

   token_t p = peek();
   bool found = false;

   while (!found) {
      const int tok = va_arg(ap, token_t);
      if (tok == -1)
         break;
      else if (p == tok)
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

static void convert_line_ending(void)
{
   if (vlogpp_leng >= 2) {
      char *c1 = vlogpp_text + vlogpp_leng - 1;
      char *c2 = vlogpp_text + vlogpp_leng - 2;

      if (*c1 == '\n' && *c2 == '\r') {
         *c1 = '\0';
         *c2 = '\n';
         vlogpp_leng--;
      }
      else if (*c1 == '\r' && *c2 == '\n') {
         *c1 = '\0';
         vlogpp_leng--;
      }
   }

   begin_token(vlogpp_text, vlogpp_leng);
}

static void undefall_macro(void)
{
   shash_free(macros);
   macros = shash_new(64);
}

static void vlog_define_cb(const char *key, const char *value, void *ctx)
{
   shash_put(macros, key, (void *)value);
}

static ident_t p_identifier(void)
{
   if (consume(tID))
      return state.last_lval.ident;
   else
      return error_marker();
}

static ident_t p_text_macro_name(void)
{
   // text_macro_identifier [ ( list_of_formal_arguments ) ]

   PBEGIN("text macro name");

   return p_identifier();
}

static void p_text_macro_definition(void)
{
   // `define text_macro_name macro_text

   PBEGIN("text macro definition");

   consume(tDEFINE);

   BEGIN(MACRO_DEF);

   ident_t id = p_text_macro_name();

   BEGIN(MACRO_TEXT);

   consume(tMACROTEXT);

   char *copy = xstrdup(tb_get(macro_text)), *p = copy;

   for (; isspace_iso88591(*p); p++);

   shash_put(macros, istr(id), p);

   tb_rewind(macro_text);

   BEGIN(INITIAL);
}

static void p_text_macro_usage(void)
{
   // `text_macro_identifier [ ( list_of_actual_arguments ) ]

   PBEGIN("text macro usage");

   consume(tMACROUSAGE);

   ident_t name = state.last_lval.ident;

   const char *text = shash_get(macros, istr(name));
   if (text == NULL) {
      warn_at(&yylloc, "macro '%pi' undefined", name);
      shash_put(macros, istr(name), "");
      return;
   }

   if (emit_locs)
      tb_printf(output, "\n`__nvc_push %pi,%d:%d,%d\n", name,
                yylloc.first_line, yylloc.first_column, yylloc.column_delta);

   push_buffer(text, strlen(text), FILE_INVALID);
   yypush_buffer_state(yy_create_buffer(NULL, YY_BUF_SIZE));

   while (not_at_token(tEOF))
      p_block_of_text();

   consume(tEOF);

   yypop_buffer_state();
   pop_buffer();

   if (emit_locs)
      tb_printf(output, "\n`__nvc_pop\n");
}

static bool p_ifdef_condition(void)
{
   // text_macro_identifier | ( ifdef_macro_expression )

   PBEGIN("ifdef condition");

   BEGIN(MACRO_PARAM);

   bool cond = true;
   switch (peek()) {
   case tID:
      {
         ident_t id = p_identifier();
         cond = shash_get(macros, istr(id)) != NULL;
      }
      break;
   case tLPAREN:
      consume(tLPAREN);
      fatal_at(&state.last_loc, "sorry, macro expressions are not yet "
               "supported");
   default:
      one_of(tID, tLPAREN);
   }

   BEGIN(INITIAL);

   return cond;
}

static void p_conditional_compilation_directive(void)
{
   // ifdef_or_ifndef ifdef_condition block_of_text
   //   { `elsif ifdef_condition block_of_text }
   //   [ `else block_of_text ]
   //   `endif

   PBEGIN("conditional compilation directive");

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

   PBEGIN("include compiler directive");

   consume(tINCLUDE);

   BEGIN(INCLUDE);

   switch (peek()) {
   case tSTRING:
      consume(tSTRING);
      break;
   case tERROR:
      // Already printed an error
      drop_token();
      BEGIN(INITIAL);
      return;
   default:
      expect(tSTRING);
      BEGIN(INITIAL);
      return;
   }

   const loc_t loc = state.last_loc;
   char *file_name = state.last_lval.str;
   state.last_lval.str = NULL;

   BEGIN(INCLUDE2);

   while (not_at_token(tEOF)) {
      if (peek() == tERROR) {
         // Already printed an error
         drop_token();
         break;
      }
      else if (consume(tTEXT)) {
         if (state.last_lval.ch == '\n')
            break;
      }
   }

   BEGIN(INITIAL);

   if (emit_locs)
      tb_printf(output, "\n`__nvc_push \"%s\",%d:%d,%d\n", file_name,
                loc.first_line, loc.first_column, loc.column_delta);

   push_file(file_name, &loc);
   yypush_buffer_state(yy_create_buffer(NULL, YY_BUF_SIZE));

   free(file_name);

   while (not_at_token(tEOF))
      p_block_of_text();

   consume(tEOF);

   yypop_buffer_state();
   pop_buffer();
}

static void p_undef_compiler_directive(void)
{
   // `undef text_macro_identifier

   PBEGIN("undef compiler directive");

   consume(tUNDEF);

   BEGIN(MACRO_PARAM);

   ident_t id = p_identifier();

   BEGIN(INITIAL);

   shash_delete(macros, istr(id));
}

static void p_block_of_text(void)
{
   PBEGIN("block of text");

   switch (peek()) {
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
   case tTEXT:
      consume(tTEXT);
      if (ifdefs == NULL || ifdefs->cond || state.last_lval.ch == '\n')
         tb_append(output, state.last_lval.ch);
      break;
   default:
      one_of(tDEFINE, tMACROUSAGE, tIFDEF, tIFNDEF, tINCLUDE, tTEXT);
      break;
   }
}

static void p_source_text(void)
{
   PBEGIN("source text");

   while (not_at_token(tEOF))
      p_block_of_text();
}

void vlog_preprocess(text_buf_t *tb, bool precise)
{
   if (macros == NULL) {
      macros = shash_new(64);
      pp_defines_iter(vlog_define_cb, NULL);
   }
   else
      assert(opt_get_int(OPT_SINGLE_UNIT));

   assert(macro_text == NULL);
   macro_text = tb_new();

   assert(output == NULL);
   output = tb;

   assert(ifdefs == NULL);

   emit_locs = precise;
   continuation = false;

   state.n_correct = RECOVER_THRESH;
   state.tokenq_head = state.tokenq_tail = 0;

   if (!YY_CURRENT_BUFFER)
      yypush_buffer_state(yy_create_buffer(NULL, YY_BUF_SIZE));

   YY_FLUSH_BUFFER;
   BEGIN(INITIAL);

   p_source_text();

   assert(ifdefs == NULL);
   output = NULL;

   if (!opt_get_int(OPT_SINGLE_UNIT)) {
      shash_free(macros);
      macros = NULL;
   }

   tb_free(macro_text);
   macro_text = NULL;
}
