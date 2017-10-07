//
//  Copyright (C) 2014-2017  Nick Gasson
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
#include "common.h"

#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <string.h>
#include <stdarg.h>
#include <ctype.h>
#include <assert.h>
#include <stdbool.h>
#include <stdlib.h>
#include <limits.h>

typedef struct {
   token_t  token;
   yylval_t lval;
   loc_t    loc;
} tokenq_t;

typedef struct {
   token_t look[4];
   token_t stop[4];
   token_t abort;
   token_t nest_in;
   token_t nest_out;
   int     depth;
} look_params_t;

typedef struct cond_state cond_state_t;

struct cond_state {
   cond_state_t *next;
   bool          result;
   loc_t         loc;
};

static const char   *perm_linebuf = NULL;
static ident_t       perm_file_name = NULL;
static int           n_token_next_start = 0;
static int           n_row = 0;
static bool          last_was_newline = true;
static loc_t         start_loc;
static loc_t         last_loc;
static const char   *read_ptr;
static const char   *file_start;
static size_t        file_sz;
static int           n_errors = 0;
static const char   *hint_str = NULL;
static int           n_correct = 0;
static tokenq_t     *tokenq;
static int           tokenq_sz;
static int           tokenq_head;
static int           tokenq_tail;
static yylval_t      last_lval;
static token_t       opt_hist[8];
static int           nopt_hist = 0;
static cond_state_t *cond_state = NULL;

loc_t yylloc;
int yylex(void);

#define scan(...) _scan(1, __VA_ARGS__, -1)
#define expect(...) _expect(1, __VA_ARGS__, -1)
#define one_of(...) _one_of(1, __VA_ARGS__, -1)
#define not_at_token(...) ((peek() != tEOF) && !_scan(1, __VA_ARGS__, -1))
#define peek() peek_nth(1)

#define parse_error(loc, ...) do {            \
      if (n_correct >= RECOVER_THRESH) {      \
         error_at(loc, __VA_ARGS__);          \
         n_errors++;                          \
      }                                       \
   } while (0)

#define RECOVER_THRESH 5
#define TRACE_PARSE    0
#define WARN_LOOKAHEAD 0
#define TRACE_RECOVERY 0

#define STD(x, y) (standard() >= (STD_##x) ? y : -1)

#if TRACE_PARSE
static int depth = 0;
#endif

typedef void (*add_func_t)(tree_t, tree_t);

typedef struct {
   const char *old_hint;
   loc_t       old_start_loc;
} state_t;

#define EXTEND(s)                                                      \
   __attribute__((cleanup(_pop_state))) const state_t _state =         \
      { hint_str, start_loc };                                         \
   hint_str = s;                                                       \
   _push_state(&_state);

#define BEGIN(s)                                                       \
   EXTEND(s);                                                          \
   start_loc = LOC_INVALID;

#define CURRENT_LOC _diff_loc(&start_loc, &last_loc)

static tree_t p_expression(void);
static tree_t p_sequential_statement(void);
static tree_t p_concurrent_statement(void);
static tree_t p_subprogram_declaration(tree_t spec);
static tree_t p_subprogram_body(tree_t spec);
static tree_t p_subprogram_specification(void);
static tree_t p_name(void);
static void p_block_configuration(tree_t unit);
static tree_t p_protected_type_body(void);
static bool p_cond_analysis_expr(void);

static const char *token_str(token_t tok);
static bool consume(token_t tok);
static bool optional(token_t tok);

static void _pop_state(const state_t *s)
{
#if TRACE_PARSE
   depth--;
   for (int i = 0; i < depth; i++)
      printf(" ");
   printf("<-- %s\n", hint_str);
#endif
   hint_str  = s->old_hint;
   if (s->old_start_loc.first_line != LINE_INVALID)
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
      ";", ":=", ":", ",", "integer", "string", "error", "inout", "linkage",
      "variable", "if", "range", "subtype", "units", "package", "library",
      "use", ".", "null", "'", "function", "impure", "return", "pure", "array",
      "<>", "=>", "others", "assert", "severity", "on", "map", "then", "else",
      "elsif", "body", "while", "loop", "after", "alias", "attribute",
      "procedure", "exit", "next", "when", "case", "label", "group", "literal",
      "|", "[", "]", "inertial", "transport", "reject", "bit string", "block",
      "with", "select", "generate", "access", "file", "open", "real", "until",
      "record", "new", "shared", "and", "or", "nand", "nor", "xor", "xnor",
      "=", "/=", "<", "<=", ">", ">=", "+", "-", "&", "**", "/", "sll", "srl",
      "sla", "sra", "rol", "ror", "mod", "rem", "abs", "not", "*", "guarded",
      "reverse_range", "protected", "context", "`if", "`else", "`elsif", "`end",
      "`error", "`warning"
   };

   if ((size_t)tok >= ARRAY_LEN(token_strs))
      return "???";
   else
      return token_strs[tok];
}

static token_t conditional_yylex(void)
{
   const token_t token = yylex();

#if 0
   printf("%s %s\n", (cond_state ? (cond_state->result ? "1" : "0") : "-"),
          token_str(token));
#endif

   switch (token) {
   case tCONDIF:
      {
         BEGIN("conditional analysis directive");

         cond_state_t *new = xmalloc(sizeof(cond_state_t));
         new->loc    = yylloc;
         new->result = p_cond_analysis_expr();
         new->next   = cond_state;

         consume(tTHEN);

         new->loc.last_column = yylloc.last_column;
         new->loc.last_line   = yylloc.last_line;

         cond_state = new;
         return conditional_yylex();
      }

   case tCONDELSE:
      {
         BEGIN("conditional analysis directive");

         if (cond_state == NULL)
            parse_error(&yylloc, "unexpected $yellow$%s$$ outside conditional "
                        "analysis block", token_str(token));
         else
            cond_state->result = !(cond_state->result);

         return conditional_yylex();
      }

   case tCONDEND:
      {
         BEGIN("conditional analysis directive");

         if (cond_state == NULL)
            parse_error(&yylloc, "unexpected $yellow$%s$$ outside conditional "
                        "analysis block", token_str(token));
         else {
            cond_state_t *old = cond_state;
            cond_state = cond_state->next;
            free(old);
         }

         optional(tIF);

         return conditional_yylex();
      }

   case tCONDERROR:
   case tCONDWARN:
      {
         if (cond_state == NULL || cond_state->result) {
            BEGIN("conditional analysis directive");

            loc_t loc = yylloc;
            if (consume(tSTRING)) {
               loc.last_column = yylloc.last_column;
               loc.last_line   = yylloc.last_line;

               if (token == tCONDWARN)
                  warn_at(&loc, "%s", last_lval.s);
               else
                  parse_error(&loc, "%s", last_lval.s);

               free(last_lval.s);
            }
         }

         return conditional_yylex();
      }

   case tEOF:
      if (cond_state != NULL) {
         parse_error(&(cond_state->loc), "unterminated conditional "
                     "analysis block");
         n_correct = 0;
      }
      return tEOF;

   default:
      if (cond_state == NULL || cond_state->result)
         return token;
      else
         return conditional_yylex();
   }
}

static token_t peek_nth(int n)
{
   while (((tokenq_head - tokenq_tail) & (tokenq_sz - 1)) < n) {
      // Calling conditional_yylex may recursively call this function
      const token_t token = conditional_yylex();

      int next = (tokenq_head + 1) & (tokenq_sz - 1);
      if (unlikely(next == tokenq_tail)) {
         const int newsz = tokenq_sz * 2;
         tokenq_t *new = xmalloc(newsz * sizeof(tokenq_t));

         tokenq_t *p = new;
         for (int i = tokenq_tail; i != tokenq_head;
              i = (i + 1) & (tokenq_sz - 1))
            *p++ = tokenq[i];

         free(tokenq);

         tokenq      = new;
         tokenq_sz   = newsz;
         tokenq_head = p - new;
         tokenq_tail = 0;

         next = (tokenq_head + 1) & (tokenq_sz - 1);
      }

      extern yylval_t yylval;

      tokenq[tokenq_head].token = token;
      tokenq[tokenq_head].lval  = yylval;
      tokenq[tokenq_head].loc   = yylloc;

      tokenq_head = next;
   }

   const int pos = (tokenq_tail + n - 1) & (tokenq_sz - 1);
   return tokenq[pos].token;
}

static bool look_for(const look_params_t *params)
{
   bool found = false;
   token_t tok = -1;
   int n, nest = 0;
   for (n = 1; ;) {
      tok = peek_nth(n++);
      if ((tok == tEOF) || (tok == params->abort))
         goto stop_looking;
      else if (tok == params->nest_in)
         nest++;

      if (nest == params->depth) {
         for (int i = 0; i < ARRAY_LEN(params->look); i++) {
            if (tok == params->look[i]) {
               found = true;
               goto stop_looking;
            }
         }

         for (int i = 0; i < ARRAY_LEN(params->stop); i++) {
            if (tok == params->stop[i])
               goto stop_looking;
         }
      }

      if (tok == params->nest_out)
         nest--;
   }
 stop_looking:

#if WARN_LOOKAHEAD > 0
   if (n >= WARN_LOOKAHEAD)
      warn_at(&(tokenq[tokenq_tail].loc), "look ahead depth %d", n);
#endif

   return found;
}

static void drop_token(void)
{
   assert(tokenq_head != tokenq_tail);

   if (start_loc.last_line == LINE_INVALID)
      start_loc = tokenq[tokenq_tail].loc;

   last_lval = tokenq[tokenq_tail].lval;
   last_loc  = tokenq[tokenq_tail].loc;

   tokenq_tail = (tokenq_tail + 1) & (tokenq_sz - 1);

   nopt_hist = 0;
}

static void drop_tokens_until(token_t tok)
{
   token_t next;
   do {
      next = peek();
      drop_token();
   } while ((tok != next) && (tok != tEOF));

#if TRACE_RECOVERY
   if (peek() != tEOF)
      fmt_loc(stdout, &(tokenq[tokenq_tail].loc));
#endif
}

static void _vexpect(va_list ap)
{
   LOCAL_TEXT_BUF tb = tb_new();

   tb_printf(tb, "unexpected $yellow$%s$$ while parsing %s, "
             "expecting ", token_str(peek()), hint_str);

   bool first = true;

   for (int i = 0; i < nopt_hist; i++) {
      if (first)
         tb_printf(tb, "one of ");
      else
         tb_printf(tb, ", ");

      tb_printf(tb, "$yellow$%s$$", token_str(opt_hist[i]));

      first = false;
   }

   int tok = va_arg(ap, int);
   while (tok != -1) {
      const int tmp = tok;
      tok = va_arg(ap, int);

      if (first && (tok != -1))
         tb_printf(tb, "one of ");
      else if (!first)
         tb_printf(tb, (tok == -1) ? " or " : ", ");

      tb_printf(tb, "$yellow$%s$$", token_str(tmp));

      first = false;
   }

   if (n_correct >= RECOVER_THRESH) {
      error_at(&(tokenq[tokenq_tail].loc), "%s", tb_get(tb));
      n_errors++;
   }

   n_correct = 0;

   drop_token();
}

static void _expect(int dummy, ...)
{
   va_list ap;
   va_start(ap, dummy);
   _vexpect(ap);
   va_end(ap);
}

static bool consume(token_t tok)
{
   const token_t got = peek();
   if (tok != got) {
      expect(tok);
      return false;
   }
   else {
      n_correct++;
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
      if (nopt_hist < ARRAY_LEN(opt_hist))
         opt_hist[nopt_hist++] = tok;
      return false;
   }
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

static ident_t loc_to_ident(const loc_t *loc)
{
   int bufsz = 128;
   char *buf = xmalloc(bufsz);
   checked_sprintf(buf, bufsz, "line_%d", loc->first_line);
   const int nprint = strlen(buf);

   for (int i = 0; ident_interned(buf); i++) {
      if (nprint + 1 > bufsz) {
         bufsz *= 2;
         buf = xrealloc(buf, bufsz);
      }
      buf[nprint] = 'a' + i;
      buf[nprint + 1] = '\0';
   }

   ident_t ident = ident_new(buf);
   free(buf);
   return ident;
}

static void set_label_and_loc(tree_t t, ident_t label, const loc_t *loc)
{
   tree_set_loc(t, loc);

   if (label == NULL)
      label = loc_to_ident(loc);
   tree_set_ident(t, label);
}

static tree_t bit_str_to_literal(const char *str, const loc_t *loc)
{
   tree_t t = tree_new(T_LITERAL);
   tree_set_loc(t, loc);
   tree_set_subkind(t, L_STRING);

   char base_ch = str[0];
   int base;
   switch (base_ch) {
   case 'X': case 'x': base = 16; break;
   case 'O': case 'o': base = 8;  break;
   case 'B': case 'b': base = 2;  break;
   default:
      parse_error(loc, "invalid base '%c' for bit string", base_ch);
      return t;
   }

   tree_t one = tree_new(T_REF);
   tree_set_ident(one, ident_new("'1'"));
   tree_set_loc(one, loc);

   tree_t zero = tree_new(T_REF);
   tree_set_ident(zero, ident_new("'0'"));
   tree_set_loc(zero, loc);

   for (const char *p = str + 2; *p != '\"'; p++) {
      if (*p == '_')
         continue;

      int n = (isdigit((int)*p) ? (*p - '0')
               : 10 + (isupper((int)*p) ? (*p - 'A') : (*p - 'a')));

      if (n >= base) {
         parse_error(loc, "invalid digit '%c' in bit string", *p);
         return t;
      }

      for (int d = (base >> 1); d > 0; n = n % d, d >>= 1)
         tree_add_char(t, (n / d) ? one : zero);
   }

   return t;
}

static tree_t get_time(int64_t fs)
{
   tree_t lit = tree_new(T_LITERAL);
   tree_set_subkind(lit, L_INT);
   tree_set_ival(lit, fs);

   tree_t unit = tree_new(T_REF);
   tree_set_ident(unit, ident_new("FS"));

   tree_t f = tree_new(T_FCALL);
   tree_set_ident(f, ident_new("\"*\""));

   tree_t left = tree_new(T_PARAM);
   tree_set_subkind(left, P_POS);
   tree_set_value(left, lit);

   tree_t right = tree_new(T_PARAM);
   tree_set_subkind(right, P_POS);
   tree_set_value(right, unit);

   tree_add_param(f, left);
   tree_add_param(f, right);

   return f;
}

static tree_t int_to_physical(tree_t t, tree_t unit)
{
   tree_t ref = tree_new(T_REF);
   tree_set_ident(ref, tree_ident(unit));

   tree_t fcall = tree_new(T_FCALL);
   tree_set_loc(fcall, tree_loc(t));
   tree_set_ident(fcall, ident_new("\"*\""));

   tree_t a = tree_new(T_PARAM);
   tree_set_subkind(a, P_POS);
   tree_set_value(a, t);

   tree_t b = tree_new(T_PARAM);
   tree_set_subkind(b, P_POS);
   tree_set_value(b, ref);

   tree_add_param(fcall, a);
   tree_add_param(fcall, b);

   return fcall;
}

static void set_delay_mechanism(tree_t t, tree_t reject)
{
   if (reject == NULL) {
      // Inertial delay with same value as waveform
      // LRM 93 section 8.4 the rejection limit in this case is
      // specified by the time expression of the first waveform
      tree_t w = (tree_kind(t) == T_CASSIGN
                  ? tree_waveform(tree_cond(t, 0), 0)
                  : tree_waveform(t, 0));
      if (tree_has_delay(w))
         tree_set_reject(t, tree_delay(w));
   }
   else
      tree_set_reject(t, reject);
}

static const char *get_cond_analysis_identifier(const char *name)
{
   if (strcmp(name, "VHDL_VERSION") == 0)
      return standard_text(standard());
   else if (strcmp(name, "TOOL_TYPE") == 0)
      return "SIMULATION";
   else if (strcmp(name, "TOOL_VENDOR") == 0)
      return PACKAGE_URL;
   else if (strcmp(name, "TOOL_NAME") == 0)
      return PACKAGE_NAME;
   else if (strcmp(name, "TOOL_EDITION") == 0)
      return "";
   else if (strcmp(name, "TOOL_VERSION") == 0)
      return PACKAGE_VERSION;
   else
      return NULL;
}

static bool p_cond_analysis_relation(void)
{
   // (  conditional_analysis_expression )
   //   | not ( conditional_analysis_expression )
   //   | conditional_analysis_identifier = string_literal
   //   | conditional_analysis_identifier /= string_literal
   //   | conditional_analysis_identifier < string_literal
   //   | conditional_analysis_identifier <= string_literal
   //   | conditional_analysis_identifier > string_literal
   //   | conditional_analysis_identifier >= string_literal

   BEGIN("conditional analysis relation");

   bool result = false;
   switch (one_of(tLPAREN, tNOT, tID)) {
   case tLPAREN:
      result = p_cond_analysis_expr();
      consume(tRPAREN);
      break;

   case tNOT:
      result = !p_cond_analysis_expr();
      break;

   case tID:
      {
         char *name = last_lval.s;
         token_t rel = one_of(tEQ, tNEQ, tLT, tLE, tGT, tGE);

         if (consume(tSTRING)) {
            const char *value = get_cond_analysis_identifier(name);
            if (value == NULL)
               parse_error(CURRENT_LOC, "undefined conditional analysis "
                           "identifier %s", name);
            else {
               char *cmp = last_lval.s + 1;
               cmp[strlen(cmp) - 1] = '\0';

               switch (rel) {
               case tEQ:
                  result = strcmp(value, cmp) == 0;
                  break;
               case tNEQ:
                  result = strcmp(value, cmp) != 0;
                  break;
               case tLT:
                  result = strcmp(value, cmp) < 0;
                  break;
               case tLE:
                  result = strcmp(value, cmp) <= 0;
                  break;
               case tGT:
                  result = strcmp(value, cmp) > 0;
                  break;
               case tGE:
                  result = strcmp(value, cmp) >= 0;
                  break;
               default:
                  break;
               }
            }

            free(last_lval.s);
         }

         free(name);
      }
      break;
   }

   return result;
}

static bool p_cond_analysis_expr(void)
{
   // conditional_analysis_relation
   //   | conditional_analysis_relation { and conditional_analysis_relation }
   //   | conditional_analysis_relation { or conditional_analysis_relation }
   //   | conditional_analysis_relation { xor conditional_analysis_relation }
   //   | conditioanl_analysis_relation { xnor conditional_analysis_relation }

   BEGIN("conditional analysis expression");

   const bool lhs = p_cond_analysis_relation();

   switch (peek()) {
   case tAND:
      consume(tAND);
      return p_cond_analysis_relation() && lhs;
   case tOR:
      consume(tOR);
      return p_cond_analysis_relation() || lhs;
   case tXOR:
      consume(tXOR);
      return p_cond_analysis_relation() ^ lhs;
   case tXNOR:
      consume(tXNOR);
      return !(p_cond_analysis_relation() ^ lhs);
   default:
      return lhs;
   }
}

static ident_t p_identifier(void)
{
   // basic_identifier | extended_identifier

   if (consume(tID)) {
      char *s = last_lval.s;
      ident_t i = ident_new(s);
      free(s);
      return i;
   }
   else
      return ident_new("error");
}

static ident_t p_selected_identifier(void)
{
   // identifier { . identifier }

   ident_t id = p_identifier();
   while (optional(tDOT))
      id = ident_prefix(id, p_identifier(), '.');

   return id;
}

static ident_list_t *p_identifier_list(void)
{
   // identifier { , identifier }

   ident_list_t *result = NULL;

   ident_list_add(&result, p_identifier());

   while (optional(tCOMMA))
      ident_list_push(&result, p_identifier());

   return result;
}

static ident_t p_operator_symbol(void)
{
   // string_literal

   consume(tSTRING);

   char *s = last_lval.s;
   for (char *p = s; *p != '\0'; p++)
      *p = tolower((int)*p);
   ident_t id = ident_new(s);
   free(s);

   return id;
}

static void p_library_clause(tree_t unit)
{
   // library logical_name_list ;

   BEGIN("library clause");

   consume(tLIBRARY);

   LOCAL_IDENT_LIST ids = p_identifier_list();

   consume(tSEMI);

   for (ident_list_t *it = ids; it != NULL; it = it->next) {
      tree_t l = tree_new(T_LIBRARY);
      tree_set_ident(l, it->ident);
      tree_set_loc(l, CURRENT_LOC);

      tree_add_context(unit, l);
   }
}

static void p_use_clause(tree_t unit, add_func_t addf)
{
   // use selected_name { , selected_name } ;

   // TODO: the structure of this is a hangover from the old Bison parser

   BEGIN("use clause");

   consume(tUSE);

   do {
      tree_t u = tree_new(T_USE);

      ident_t i1 = p_identifier();
      consume(tDOT);

      switch (peek()) {
      case tID:
         tree_set_ident(u, ident_prefix(i1, p_identifier(), '.'));

         if (optional(tDOT)) {
            switch (peek()) {
            case tID:
               tree_set_ident2(u, p_identifier());
               break;

            case tSTRING:
               tree_set_ident2(u, p_operator_symbol());
               break;

            case tALL:
               consume(tALL);
               tree_set_ident2(u, all_i);
               break;

            default:
               expect(tID, tSTRING, tALL);
            }
         }
         break;

      case tALL:
         consume(tALL);
         tree_set_ident(u, i1);
         tree_set_ident2(u, all_i);
         break;

      default:
         expect(tID, tALL);
      }

      tree_set_loc(u, CURRENT_LOC);
      (*addf)(unit, u);
   } while (optional(tCOMMA));

   consume(tSEMI);
}

static void p_context_reference(tree_t unit)
{
   // context selected_name { , selected_name } ;

   BEGIN("context reference");

   consume(tCONTEXT);

   do {
      tree_t c = tree_new(T_CTXREF);
      tree_set_ident(c, p_selected_identifier());
      tree_set_loc(c, CURRENT_LOC);

      tree_add_context(unit, c);
   } while (optional(tCOMMA));

   consume(tSEMI);
}

static void p_context_item(tree_t unit)
{
   // library_clause | use_clause | 2008: context_reference

   BEGIN("context item");

   switch (peek()) {
   case tLIBRARY:
      p_library_clause(unit);
      break;

   case tUSE:
      p_use_clause(unit, tree_add_context);
      break;

   case tCONTEXT:
      p_context_reference(unit);
      break;

   default:
      expect(tLIBRARY, tUSE, tCONTEXT);
   }
}

static void p_context_clause(tree_t unit)
{
   // { context_item }

   BEGIN("context clause");

   while (scan(tLIBRARY, tUSE, tCONTEXT)) {
      if (peek() == tCONTEXT && peek_nth(3) == tIS)
         break;
      else
         p_context_item(unit);
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

static range_t p_range(tree_t left)
{
   // attribute_name | simple_expression direction simple_expression

   EXTEND("range");

   range_t r = {};

   switch (one_of(tTO, tDOWNTO)) {
   case tTO:
      r.kind  = RANGE_TO;
      r.left  = left;
      r.right = p_expression();
      break;

   case tDOWNTO:
      r.kind  = RANGE_DOWNTO;
      r.left  = left;
      r.right = p_expression();
      break;
   }

   return r;
}

static range_t p_range_constraint(void)
{
   // range range

   BEGIN("range constraint");

   consume(tRANGE);

   return p_range(p_expression());
}

static range_t p_discrete_range(void)
{
   // subtype_indication | range

   BEGIN("discrete range");

   tree_t expr1 = p_expression();

   switch (peek()) {
   case tTO:
   case tDOWNTO:
   case tTICK:
      return p_range(expr1);

   case tRANGE:
      {
         if (tree_kind(expr1) != T_REF)
            assert(false);   // XXX: FIXME

         type_t type = type_new(T_UNRESOLVED);
         type_set_ident(type, tree_ident(expr1));

         range_t r = p_range_constraint();
         if (r.left != NULL)
             tree_set_type(r.left, type);
         if (r.right != NULL)
            tree_set_type(r.right, type);

         return r;
      }

   default:
      {
         range_t r = {
            .kind  = RANGE_EXPR,
            .left  = expr1,
            .right = NULL
         };
         return r;
      }
   }
}

static tree_t p_slice_name(tree_t prefix)
{
   // prefix ( discrete_range )

   EXTEND("slice name");

   tree_t t = tree_new(T_ARRAY_SLICE);
   tree_set_value(t, prefix);

   consume(tLPAREN);
   tree_set_range(t, p_discrete_range());
   consume(tRPAREN);

   tree_set_loc(t, CURRENT_LOC);
   return t;
}

static tree_t p_formal_part(void)
{
   // formal_designator
   //   | name ( formal_designator )
   //   | type_mark ( formal_designator )

   BEGIN("formal part");

   return p_name();
}

static tree_t p_actual_part(void)
{
   // actual_designator
   //   | name ( actual_designator )
   //   | type_mark ( actual_designator )

   BEGIN("actual part");

   if (optional(tOPEN)) {
      tree_t t = tree_new(T_OPEN);
      tree_set_loc(t, CURRENT_LOC);
      return t;
   }

   // If the actual part takes either the second or third form above then the
   // argument to the function call is the actual designator but only if the
   // call is to a named function rather than an operator
   // This is import for identifying conversion functions later
   const token_t next = peek();
   const bool had_name = (next == tID || next == tSTRING);

   tree_t designator = p_expression();

   const bool could_be_conversion =
      had_name
      && tree_kind(designator) == T_FCALL
      && tree_params(designator) == 1;

   if (could_be_conversion)
      tree_set_flag(designator, TREE_F_CONVERSION);

   return designator;
}

static void p_association_element(tree_t map, add_func_t addf)
{
   // [ formal_part => ] actual_part

   BEGIN("association element");

   tree_t p = tree_new(T_PARAM);

   const look_params_t lookp = {
      .look     = { tASSOC },
      .stop     = { tCOMMA, tRPAREN },
      .abort    = tSEMI,
      .nest_in  = tLPAREN,
      .nest_out = tRPAREN,
      .depth    = 0
   };

   if (look_for(&lookp)) {
      tree_set_subkind(p, P_NAMED);
      tree_set_name(p, p_formal_part());

      consume(tASSOC);
   }
   else
      tree_set_subkind(p, P_POS);

   tree_set_value(p, p_actual_part());
   tree_set_loc(p, CURRENT_LOC);

   (*addf)(map, p);
}

static void p_association_list(tree_t map, add_func_t addf)
{
   // association_element { , association_element }

   p_association_element(map, addf);

   while (optional(tCOMMA))
      p_association_element(map, addf);
}

static void p_actual_parameter_part(tree_t call)
{
   // association_list

   BEGIN("actual parameter part");

   p_association_list(call, tree_add_param);
}

static tree_t p_function_call(tree_t name)
{
   // name [ ( actual_parameter_part ) ]

   EXTEND("function call");

   tree_change_kind(name, T_FCALL);

   consume(tLPAREN);
   p_actual_parameter_part(name);
   consume(tRPAREN);

   tree_set_loc(name, CURRENT_LOC);
   return name;
}

static tree_t p_attribute_name(tree_t prefix)
{
   // prefix [ signature ] ' attribute_designator [ ( expression ) ]

   EXTEND("attribute name");

   consume(tTICK);

   tree_t t = tree_new(T_ATTR_REF);
   tree_set_name(t, prefix);

   if (optional(tRANGE))
      tree_set_ident(t, ident_new("RANGE"));
   else if (optional(tREVRANGE))
      tree_set_ident(t, ident_new("REVERSE_RANGE"));
   else
      tree_set_ident(t, p_identifier());

   if (optional(tLPAREN)) {
      add_param(t, p_expression(), P_POS, NULL);
      consume(tRPAREN);
   }

   tree_set_loc(t, CURRENT_LOC);
   return t;
}

static tree_t p_selected_name(tree_t prefix)
{
   // prefix . suffix

   EXTEND("selected name");

   consume(tDOT);

   ident_t suffix = NULL;
   switch (peek()) {
   case tID:
      suffix = p_identifier();
      break;

   case tSTRING:
      suffix = p_operator_symbol();
      break;

   case tALL:
      {
         consume(tALL);

         tree_t all = tree_new(T_ALL);
         tree_set_loc(all, CURRENT_LOC);
         tree_set_value(all, prefix);

         return all;
      }

   default:
      expect(tID, tALL);
      return prefix;
   }

   if (tree_kind(prefix) == T_REF) {
      ident_t joined = ident_prefix(tree_ident(prefix), suffix, '.');
      tree_set_ident(prefix, joined);
      return prefix;
   }
   else {
      tree_t rref = tree_new(T_RECORD_REF);
      tree_set_value(rref, prefix);
      tree_set_ident(rref, suffix);
      tree_set_loc(rref, CURRENT_LOC);
      return rref;
   }
}

static tree_t p_indexed_name(tree_t prefix)
{
   // prefix ( expression { , expression } )

   EXTEND("indexed name");

   tree_t t = tree_new(T_ARRAY_REF);
   tree_set_value(t, prefix);

   consume(tLPAREN);

   do {
      add_param(t, p_expression(), P_POS, NULL);
   } while (optional(tCOMMA));

   consume(tRPAREN);

   tree_set_loc(t, CURRENT_LOC);
   return t;
}

static tree_t p_name(void)
{
   // simple_name | operator_symbol | selected_name | indexed_name
   //   | slice_name | attribute_name

   BEGIN("name");

   ident_t id = NULL;

   switch (peek()) {
   case tSTRING:
      id = p_operator_symbol();
      break;

   case tID:
      id = p_identifier();
      break;

   default:
      expect(tSTRING, tID);
      return tree_new(T_OPEN);
   }

   tree_t name = tree_new(T_REF);
   tree_set_ident(name, id);
   tree_set_loc(name, CURRENT_LOC);

   for (;;) {
      switch (peek()) {
      case tLPAREN:
         break;

      case tDOT:
         name = p_selected_name(name);
         tree_set_loc(name, CURRENT_LOC);
         continue;

      case tTICK:
         if (peek_nth(2) == tLPAREN)
            return name;   // Qualified expression
         name = p_attribute_name(name);
         continue;

      default:
         return name;
      }

      // Either a function call, indexed name, or selected name

      const look_params_t lookp = {
         .look     = { tDOWNTO, tTO, tRANGE, tREVRANGE },
         .stop     = { tRPAREN, tCOMMA },
         .abort    = tSEMI,
         .nest_in  = tLPAREN,
         .nest_out = tRPAREN,
         .depth    = 1
      };

      if (look_for(&lookp))
         name = p_slice_name(name);
      else if (tree_kind(name) == T_REF)
         name = p_function_call(name);
      else
         name = p_indexed_name(name);
   }
}

static type_t p_type_mark(void)
{
   // name

   BEGIN("type mark");

   ident_t name = p_identifier();
   while (optional(tDOT))
      name = ident_prefix(name, p_identifier(), '.');

   type_t t = type_new(T_UNRESOLVED);
   type_set_ident(t, name);
   return t;
}

static void p_index_constraint(type_t type)
{
   // ( discrete_range { , discrete_range } )

   consume(tLPAREN);

   do {
      type_add_dim(type, p_discrete_range());
   } while (optional(tCOMMA));

   consume(tRPAREN);
}

static void p_constraint(type_t type)
{
   // range_constraint | index_constraint

   switch (peek()) {
   case tRANGE:
      type_add_dim(type, p_range_constraint());
      break;

   case tLPAREN:
      p_index_constraint(type);
      break;

   default:
      expect(tRANGE);
   }
}

static type_t p_subtype_indication(void)
{
   // [ name ] type_mark [ constraint ]

   BEGIN("subtype indication");

   bool made_subtype = false;
   type_t type = NULL;
   if ((peek() == tID) && (peek_nth(2) == tID)) {
      type = type_new(T_SUBTYPE);
      made_subtype = true;

      tree_t rname = p_name();
      // XXX: check name is resolution_function_name
      type_set_resolution(type, rname);

      type_t base = p_type_mark();
      type_set_base(type, base);
   }
   else
      type = p_type_mark();

   if (scan(tRANGE, tLPAREN)) {
      if (!made_subtype) {
         type_t sub = type_new(T_SUBTYPE);
         type_set_base(sub, type);

         type = sub;
      }

      p_constraint(type);
   }

   return type;
}

static tree_t p_abstract_literal(void)
{
   // decimal_literal | based_literal

   BEGIN("abstract literal");

   tree_t t = tree_new(T_LITERAL);

   switch (one_of(tINT, tREAL)) {
   case tINT:
      tree_set_subkind(t, L_INT);
      tree_set_ival(t, last_lval.n);
      break;

   case tREAL:
      tree_set_subkind(t, L_REAL);
      tree_set_dval(t, last_lval.d);
      break;
   }

   tree_set_loc(t, CURRENT_LOC);
   return t;
}

static tree_t p_physical_literal(tree_t mult)
{
   // [ abstract_literal ] name

   EXTEND("physical literal");

   tree_t unit = tree_new(T_REF);
   tree_set_ident(unit, p_identifier());
   tree_set_loc(unit, CURRENT_LOC);

   if (mult != NULL) {
      tree_t t = tree_new(T_FCALL);
      tree_set_loc(t, CURRENT_LOC);
      tree_set_ident(t, ident_new("\"*\""));

      add_param(t, mult, P_POS, NULL);
      add_param(t, unit, P_POS, NULL);

      return t;
   }
   else
      return unit;
}

static tree_t p_numeric_literal(void)
{
   // abstract_literal | physical_literal

   BEGIN("numeric literal");

   tree_t abs = NULL;
   if (scan(tINT, tREAL))
      abs = p_abstract_literal();

   if (peek() == tID)
      return p_physical_literal(abs);
   else
      return abs;
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

   case tSTRING:
      {
         consume(tSTRING);

         char *p = last_lval.s;
         size_t len = strlen(p);
         tree_t t = str_to_literal(p + 1, p + len - 1, NULL);
         tree_set_loc(t, CURRENT_LOC);
         free(p);

         return t;
      }

   case tBITSTRING:
      {
         consume(tBITSTRING);

         tree_t t = bit_str_to_literal(last_lval.s, CURRENT_LOC);
         free(last_lval.s);
         return t;
      }

   default:
      expect(tNULL, tINT, tREAL);
      return tree_new(T_OPEN);
   }
}

static void p_choice(tree_t parent)
{
   // simple_expression | discrete_range | simple_name | others

   BEGIN("choice");

   tree_t t = tree_new(T_ASSOC);

   if (optional(tOTHERS))
      tree_set_subkind(t, A_OTHERS);
   else {
      const look_params_t lookp = {
         .look     = { tDOWNTO, tTO, tRANGE, tREVRANGE },
         .stop     = { tRPAREN, tCOMMA, tASSOC, tBAR },
         .abort    = tSEMI,
         .nest_in  = tLPAREN,
         .nest_out = tRPAREN,
         .depth    = 0
      };

      if (look_for(&lookp)) {
         tree_set_subkind(t, A_RANGE);
         tree_set_range(t, p_discrete_range());
      }
      else {
         tree_set_subkind(t, A_NAMED);
         tree_set_name(t, p_expression());
      }
   }

   tree_set_loc(t, CURRENT_LOC);
   tree_add_assoc(parent, t);
}

static void p_choices(tree_t parent)
{
   // choices ::= choice { | choice }

   BEGIN("choices");

   p_choice(parent);

   while (optional(tBAR))
      p_choice(parent);
}

static void p_element_association(tree_t agg)
{
   // [ choices => ] expression

   BEGIN("element association");

   const look_params_t lookp = {
      .look     = { tASSOC },
      .stop     = { tCOMMA, tRPAREN },
      .abort    = tSEMI,
      .nest_in  = tLPAREN,
      .nest_out = tRPAREN,
      .depth    = 0
   };

   if (look_for(&lookp)) {
      const int nstart = tree_assocs(agg);
      p_choices(agg);

      consume(tASSOC);

      tree_t value = p_expression();
      const int nassocs = tree_assocs(agg);
      for (int i = nstart; i < nassocs; i++)
         tree_set_value(tree_assoc(agg, i), value);
   }
   else {
      tree_t t = tree_new(T_ASSOC);
      tree_set_subkind(t, A_POS);
      tree_set_value(t, p_expression());
      tree_set_loc(t, CURRENT_LOC);

      tree_add_assoc(agg, t);
   }
}

static tree_t p_aggregate(void)
{
   // ( element_association { , element_association } )

   BEGIN("aggregate");

   tree_t t = tree_new(T_AGGREGATE);

   consume(tLPAREN);

   do {
      p_element_association(t);
   } while (optional(tCOMMA));

   consume(tRPAREN);

   tree_set_loc(t, CURRENT_LOC);
   return t;
}

static tree_t p_qualified_expression(tree_t name)
{
   // type_mark ' ( expression ) | type_mark ' aggregate

   EXTEND("qualified expression");

   tree_t t = tree_new(T_QUALIFIED);

   if (tree_kind(name) != T_REF)
      assert(false);   // XXX: FIXME
   else
      tree_set_ident(t, tree_ident(name));

   consume(tTICK);

   const look_params_t lookp = {
      .look     = { tCOMMA, tASSOC },
      .stop     = { tRPAREN },
      .abort    = tSEMI,
      .nest_in  = tLPAREN,
      .nest_out = tRPAREN,
      .depth    = 1
   };

   if (look_for(&lookp))
      tree_set_value(t, p_aggregate());
   else {
      consume(tLPAREN);
      tree_set_value(t, p_expression());
      consume(tRPAREN);
   }
   tree_set_loc(t, CURRENT_LOC);

   return t;
}

static tree_t p_allocator(void)
{
   // new subtype_indication | new qualified_expression

   BEGIN("allocator");

   consume(tNEW);

   tree_t new = tree_new(T_NEW);
   tree_set_value(new, p_expression());
   tree_set_loc(new, CURRENT_LOC);

   return new;
}

static tree_t p_primary(void)
{
   // name | literal | aggregate | function_call | qualified_expression
   //   | type_conversion | allocator | ( expression )

   BEGIN("primary");

   switch (peek()) {
   case tLPAREN:
      {
         const look_params_t lookp = {
            .look     = { tCOMMA, tASSOC },
            .stop     = { tRPAREN },
            .abort    = tSEMI,
            .nest_in  = tLPAREN,
            .nest_out = tRPAREN,
            .depth    = 1
         };

         if (look_for(&lookp))
            return p_aggregate();
         else {
            consume(tLPAREN);
            tree_t sub = p_expression();
            consume(tRPAREN);
            return sub;
         }
      }

   case tINT:
   case tREAL:
   case tNULL:
   case tBITSTRING:
      return p_literal();

   case tSTRING:
      return (peek_nth(2) == tLPAREN) ? p_name() : p_literal();

   case tID:
      {
         tree_t name = p_name();
         switch (peek()) {
         case tTICK:
            return p_qualified_expression(name);
         default:
            return name;
         }
      }

   case tNEW:
      return p_allocator();

   default:
      expect(tLPAREN, tINT, tREAL, tNULL, tID, tSTRING, tBITSTRING, tNEW);
      return tree_new(T_OPEN);
   }
}

static tree_t p_factor(void)
{
   // primary [ ** primary ] | abs primary | not primary

   BEGIN("factor");

   ident_t op = NULL;
   switch (peek()) {
   case tNOT:
      consume(tNOT);
      op = ident_new("\"not\"");
      break;

   case tABS:
      consume(tABS);
      op = ident_new("\"abs\"");
      break;

   default:
      break;
   }

   tree_t operand = p_primary();

   if (op != NULL) {
      tree_t t = tree_new(T_FCALL);
      tree_set_loc(t, CURRENT_LOC);
      tree_set_ident(t, op);
      add_param(t, operand, P_POS, NULL);

      return t;
   }
   else if (optional(tPOWER)) {
      tree_t second = p_primary();

      tree_t t = tree_new(T_FCALL);
      tree_set_loc(t, CURRENT_LOC);
      tree_set_ident(t, ident_new("\"**\""));
      add_param(t, operand, P_POS, NULL);
      add_param(t, second, P_POS, NULL);

      return t;
   }
   else
      return operand;
}

static ident_t p_multiplying_operator(void)
{
   switch (one_of(tTIMES, tOVER, tMOD, tREM)) {
   case tTIMES:
      return ident_new("\"*\"");
   case tOVER:
      return ident_new("\"/\"");
   case tMOD:
      return ident_new("\"mod\"");
   case tREM:
      return ident_new("\"rem\"");
   default:
      return ident_new("error");
   }
}

static tree_t p_term(void)
{
   // factor { multiplying_operator factor }

   BEGIN("term");

   tree_t term = p_factor();

   while (scan(tTIMES, tOVER, tMOD, tREM)) {
      ident_t op   = p_multiplying_operator();
      tree_t left  = term;
      tree_t right = p_factor();

      term = tree_new(T_FCALL);
      tree_set_ident(term, op);
      tree_set_loc(term, CURRENT_LOC);

      add_param(term, left, P_POS, NULL);
      add_param(term, right, P_POS, NULL);
   }

   return term;
}

static ident_t p_adding_operator(void)
{
   switch (one_of(tPLUS, tMINUS, tAMP)) {
   case tPLUS:
      return ident_new("\"+\"");
   case tMINUS:
      return ident_new("\"-\"");
   case tAMP:
      return ident_new("\"&\"");
   default:
      return ident_new("error");
   }
}

static ident_t p_sign(void)
{
   switch (one_of(tPLUS, tMINUS)) {
   case tPLUS:
      return ident_new("\"+\"");
   case tMINUS:
      return ident_new("\"-\"");
   default:
      return ident_new("error");
   }
}

static tree_t p_simple_expression(void)
{
   // [ sign ] term { adding_operator term }

   BEGIN("simple expression");

   ident_t sign = NULL;
   if (scan(tPLUS, tMINUS))
      sign = p_sign();

   tree_t expr = p_term();

   if (sign != NULL) {
      tree_t tmp = tree_new(T_FCALL);
      tree_set_ident(tmp, sign);
      tree_set_loc(tmp, CURRENT_LOC);

      add_param(tmp, expr, P_POS, NULL);

      expr = tmp;
   }

   while (scan(tPLUS, tMINUS, tAMP)) {
      tree_t left = expr;

      if (optional(tAMP))
         expr = tree_new(T_CONCAT);
      else {
         expr = tree_new(T_FCALL);
         tree_set_ident(expr, p_adding_operator());
      }

      tree_t right = p_term();
      tree_set_loc(expr, CURRENT_LOC);

      add_param(expr, left, P_POS, NULL);
      add_param(expr, right, P_POS, NULL);
   }

   return expr;
}

static ident_t p_shift_operator(void)
{
   switch (one_of(tSLL, tSRL, tSLA, tSRA, tROL, tROR)) {
   case tSLL:
      return ident_new("\"sll\"");
   case tSRL:
      return ident_new("\"srl\"");
   case tSLA:
      return ident_new("\"sla\"");
   case tSRA:
      return ident_new("\"sra\"");
   case tROL:
      return ident_new("\"rol\"");
   case tROR:
      return ident_new("\"ror\"");
   default:
      return ident_new("error");
   }
}

static tree_t p_shift_expression(void)
{
   // simple_expression [ shift_operator simple_expression ]

   BEGIN("shift expression");

   tree_t shift = p_simple_expression();

   while (scan(tSLL, tSRL, tSLA, tSRA, tROL, tROR)) {
      ident_t op   = p_shift_operator();
      tree_t left  = shift;
      tree_t right = p_simple_expression();

      shift = tree_new(T_FCALL);
      tree_set_ident(shift, op);
      tree_set_loc(shift, CURRENT_LOC);

      add_param(shift, left, P_POS, NULL);
      add_param(shift, right, P_POS, NULL);
   }

   return shift;
}

static ident_t p_relational_operator(void)
{
   switch (one_of(tEQ, tNEQ, tLT, tLE, tGT, tGE)) {
   case tEQ:
      return ident_new("\"=\"");
   case tNEQ:
      return ident_new("\"/=\"");
   case tLT:
      return ident_new("\"<\"");
   case tLE:
      return ident_new("\"<=\"");
   case tGT:
      return ident_new("\">\"");
   case tGE:
      return ident_new("\">=\"");
   default:
      return ident_new("error");
   }
}

static tree_t p_relation(void)
{
   // shift_expression [ relational_operator shift_expression ]

   BEGIN("relation");

   tree_t rel = p_shift_expression();

   while (scan(tEQ, tNEQ, tLT, tLE, tGT, tGE)) {
      ident_t op   = p_relational_operator();
      tree_t left  = rel;
      tree_t right = p_shift_expression();

      rel = tree_new(T_FCALL);
      tree_set_ident(rel, op);
      tree_set_loc(rel, CURRENT_LOC);

      add_param(rel, left, P_POS, NULL);
      add_param(rel, right, P_POS, NULL);
   }

   return rel;
}

static tree_t p_expression(void)
{
   // relation { and relation } | relation { or relation }
   //   | relation { xor relation } | relation [ nand relation ]
   //   | relation [ nor relation ] | relation { xnor relation }

   BEGIN("expression");

   tree_t expr = p_relation();

   int loop_limit = (scan(tNOR, tNAND) ? 1 : INT_MAX);

   while (loop_limit-- && scan(tAND, tOR, tXOR, tNAND, tNOR, tXNOR)) {
      ident_t op;
      switch (one_of(tAND, tOR, tXOR, tNAND, tNOR, tXNOR)) {
      case tAND:  op = ident_new("\"and\""); break;
      case tOR:   op = ident_new("\"or\""); break;
      case tXOR:  op = ident_new("\"xor\""); break;
      case tNAND: op = ident_new("\"nand\""); break;
      case tNOR:  op = ident_new("\"nor\""); break;
      case tXNOR: op = ident_new("\"xnor\""); break;
      default:
         op = ident_new("error");
      }

      tree_t left  = expr;
      tree_t right = p_relation();

      expr = tree_new(T_FCALL);
      tree_set_ident(expr, op);
      tree_set_loc(expr, CURRENT_LOC);

      add_param(expr, left, P_POS, NULL);
      add_param(expr, right, P_POS, NULL);
   }

   return expr;
}

static void p_interface_constant_declaration(tree_t parent, add_func_t addf)
{
   // [ constant ] identifier_list : [ in ] subtype_indication [ := expression ]

   BEGIN("interface constant declaration");

   optional(tCONSTANT);

   LOCAL_IDENT_LIST ids = p_identifier_list();

   consume(tCOLON);

   // The grammar only allows IN here but we are more leniant to allow the
   // semantic checker to generate a more helpful error message
   port_mode_t mode = PORT_IN;
   if (scan(tIN, tOUT, tINOUT, tBUFFER, tLINKAGE))
      mode = p_mode();

   type_t type = p_subtype_indication();

   tree_t init = NULL;
   if (optional(tASSIGN))
      init = p_expression();

   const loc_t *loc = CURRENT_LOC;

   for (ident_list_t *it = ids; it != NULL; it = it->next) {
      tree_t d = tree_new(T_PORT_DECL);
      tree_set_ident(d, it->ident);
      tree_set_loc(d, loc);
      tree_set_subkind(d, mode);
      tree_set_type(d, type);
      tree_set_class(d, C_CONSTANT);

      if (init != NULL)
         tree_set_value(d, init);

      (*addf)(parent, d);
   }
}

static void p_interface_signal_declaration(tree_t parent, add_func_t addf)
{
   // [signal] identifier_list : [ mode ] subtype_indication [ bus ]
   //    [ := expression ]

   BEGIN("interface signal declaration");

   optional(tSIGNAL);
   LOCAL_IDENT_LIST ids = p_identifier_list();
   consume(tCOLON);

   port_mode_t mode = PORT_IN;
   if (scan(tIN, tOUT, tINOUT, tBUFFER, tLINKAGE))
      mode = p_mode();

   type_t type = p_subtype_indication();

   optional(tBUS);

   tree_t init = NULL;
   if (optional(tASSIGN))
      init = p_expression();

   const loc_t *loc = CURRENT_LOC;

   for (ident_list_t *it = ids; it != NULL; it = it->next) {
      tree_t d = tree_new(T_PORT_DECL);
      tree_set_ident(d, it->ident);
      tree_set_loc(d, loc);
      tree_set_subkind(d, mode);
      tree_set_type(d, type);
      tree_set_class(d, C_SIGNAL);

      if (init != NULL)
         tree_set_value(d, init);

      (*addf)(parent, d);
   }
}

static void p_interface_variable_declaration(tree_t parent, class_t def_class, add_func_t addf)
{
   // [variable] identifier_list : [ mode ] subtype_indication [ := expression ]

   BEGIN("interface variable declaration");

   if (optional(tVARIABLE))
      def_class = C_VARIABLE;

   LOCAL_IDENT_LIST ids = p_identifier_list();
   consume(tCOLON);

   port_mode_t mode = PORT_IN;
   if (scan(tIN, tOUT, tINOUT, tBUFFER, tLINKAGE))
      mode = p_mode();

   type_t type = p_subtype_indication();

   tree_t init = NULL;
   if (optional(tASSIGN))
      init = p_expression();

   const loc_t *loc = CURRENT_LOC;

   for (ident_list_t *it = ids; it != NULL; it = it->next) {
      tree_t d = tree_new(T_PORT_DECL);
      tree_set_ident(d, it->ident);
      tree_set_loc(d, loc);
      tree_set_subkind(d, mode);
      tree_set_type(d, type);
      tree_set_class(d, def_class);

      if (init != NULL)
         tree_set_value(d, init);

      (*addf)(parent, d);
   }
}

static void p_interface_file_declaration(tree_t parent, add_func_t addf)
{
   // file identifier_list : subtype_indication

   BEGIN("interface file declaration");

   consume(tFILE);

   LOCAL_IDENT_LIST ids = p_identifier_list();

   consume(tCOLON);

   type_t type = p_subtype_indication();

   const loc_t *loc = CURRENT_LOC;
   for (ident_list_t *it = ids; it != NULL; it = it->next) {
      tree_t d = tree_new(T_PORT_DECL);
      tree_set_ident(d, it->ident);
      tree_set_loc(d, loc);
      tree_set_subkind(d, PORT_IN);
      tree_set_type(d, type);
      tree_set_class(d, C_FILE);

      (*addf)(parent, d);
   }
}

static void p_interface_declaration(class_t def_class, tree_t parent,
                                    add_func_t addf)
{
   // interface_constant_declaration | interface_signal_declaration
   //   | interface_variable_declaration | interface_file_declaration

   BEGIN("interface declaration");

   const token_t p = peek();
   switch (p) {
   case tCONSTANT:
      p_interface_constant_declaration(parent, addf);
      break;

   case tSIGNAL:
      p_interface_signal_declaration(parent, addf);
      break;

   case tVARIABLE:
      p_interface_variable_declaration(parent, C_VARIABLE, addf);
      break;

   case tFILE:
      p_interface_file_declaration(parent, addf);
      break;

   case tID:
      {
         switch (def_class) {
         case C_CONSTANT:
            p_interface_constant_declaration(parent, addf);
            break;

         case C_SIGNAL:
            p_interface_signal_declaration(parent, addf);
            break;

         case C_VARIABLE:
         case C_DEFAULT:
            p_interface_variable_declaration(parent, def_class, addf);
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

static void p_interface_element(class_t def_class, tree_t parent,
                                add_func_t addf)
{
   // interface_declaration

   BEGIN("interface element");

   p_interface_declaration(def_class, parent, addf);
}

static void p_interface_list(class_t def_class, tree_t parent, add_func_t addf)
{
   // interface_element { ; interface_element }

   BEGIN("interface list");

   p_interface_element(def_class, parent, addf);

   while (optional(tSEMI))
      p_interface_element(def_class, parent, addf);
}

static void p_port_list(tree_t parent)
{
   // port_list ::= interface_list

   BEGIN("port list");

   p_interface_list(C_SIGNAL, parent, tree_add_port);
}

static void p_port_clause(tree_t parent)
{
   // port ( port_list ) ;

   BEGIN("port clause");

   consume(tPORT);
   consume(tLPAREN);

   p_port_list(parent);

   consume(tRPAREN);
   consume(tSEMI);
}

static void p_generic_list(tree_t parent)
{
   // generic_list ::= interface_list

   BEGIN("generic list");

   p_interface_list(C_CONSTANT, parent, tree_add_generic);
}

static void p_generic_clause(tree_t parent)
{
   // generic ( generic_list ) ;

   BEGIN("generic clause");

   consume(tGENERIC);
   consume(tLPAREN);

   p_generic_list(parent);

   consume(tRPAREN);
   consume(tSEMI);
}

static void p_entity_header(tree_t entity)
{
   // [ generic_clause ] [ port_clause ]

   BEGIN("entity header");

   if (scan(tGENERIC))
      p_generic_clause(entity);

   if (scan(tPORT))
      p_port_clause(entity);
}

static tree_t p_attribute_declaration(void)
{
   // attribute identifier : type_mark ;

   BEGIN("attribute declaration");

   tree_t t = tree_new(T_ATTR_DECL);

   consume(tATTRIBUTE);
   tree_set_ident(t, p_identifier());
   consume(tCOLON);
   tree_set_type(t, p_type_mark());
   consume(tSEMI);

   tree_set_loc(t, CURRENT_LOC);
   return t;
}

static class_t p_entity_class(void)
{
   // entity | procedure | type | signal | label | group | architecture
   //   | function | subtype | variable | literal | file | configuration
   //   | package | constant | component | units

   BEGIN("entity class");

   switch (one_of(tENTITY, tPROCEDURE, tSIGNAL, tLABEL, tFUNCTION,
                  tCOMPONENT, tVARIABLE, tARCHITECTURE, tTYPE, tPACKAGE,
                  tCONSTANT)) {
   case tENTITY:
      return C_ENTITY;
   case tPROCEDURE:
      return C_PROCEDURE;
   case tSIGNAL:
      return C_SIGNAL;
   case tLABEL:
      return C_LABEL;
   case tFUNCTION:
      return C_FUNCTION;
   case tCOMPONENT:
      return C_COMPONENT;
   case tVARIABLE:
      return C_VARIABLE;
   case tARCHITECTURE:
      return C_ARCHITECTURE;
   case tTYPE:
      return C_TYPE;
   case tPACKAGE:
      return C_PACKAGE;
   case tCONSTANT:
      return C_CONSTANT;
   default:
      return C_DEFAULT;
   }
}

static ident_list_t *p_entity_specification(class_t *class)
{
   // entity_name_list : entity_class

   BEGIN("entity specification");

   ident_list_t *ids = p_identifier_list();

   consume(tCOLON);

   *class = p_entity_class();
   return ids;
}

static void p_attribute_specification(tree_t parent, add_func_t addf)
{
   // attribute attribute_designator of entity_specification is expression ;

   BEGIN("attribute specification");

   consume(tATTRIBUTE);
   ident_t head = p_identifier();
   consume(tOF);

   class_t class;
   LOCAL_IDENT_LIST ids = p_entity_specification(&class);

   consume(tIS);

   tree_t value = p_expression();

   consume(tSEMI);

   const loc_t *loc = CURRENT_LOC;

   for (ident_list_t *it = ids; it != NULL; it = it->next) {
      tree_t t = tree_new(T_ATTR_SPEC);
      tree_set_loc(t, loc);
      tree_set_class(t, class);
      tree_set_ident(t, head);
      tree_set_ident2(t, it->ident);
      tree_set_value(t, value);

      (*addf)(parent, t);
   }
}

static type_t p_integer_type_definition(range_t r)
{
   // range_constraint

   EXTEND("integer type definition");

   type_t t = type_new(T_INTEGER);
   type_add_dim(t, r);
   return t;
}

static type_t p_real_type_definition(range_t r)
{
   // range_constraint

   EXTEND("real type definition");

   type_t t = type_new(T_REAL);
   type_add_dim(t, r);
   return t;
}

static tree_t p_base_unit_declaration(void)
{
   // identifier ;

   BEGIN("base unit declaration");

   ident_t id = p_identifier();
   consume(tSEMI);

   tree_t mult = tree_new(T_LITERAL);
   tree_set_loc(mult, CURRENT_LOC);
   tree_set_subkind(mult, L_INT);
   tree_set_ival(mult, 1);

   tree_t t = tree_new(T_UNIT_DECL);
   tree_set_loc(t, CURRENT_LOC);
   tree_set_value(t, mult);
   tree_set_ident(t, id);

   return t;
}

static tree_t p_secondary_unit_declaration(void)
{
   // identifier = physical_literal ;

   BEGIN("secondary unit declaration");

   ident_t id = p_identifier();
   consume(tEQ);
   tree_t value = p_physical_literal(p_abstract_literal());
   consume(tSEMI);

   tree_t u = tree_new(T_UNIT_DECL);
   tree_set_ident(u, id);
   tree_set_value(u, value);
   tree_set_loc(u, CURRENT_LOC);

   return u;
}

static type_t p_physical_type_definition(range_t r)
{
   // range_constraint units base_unit_declaration
   //   { secondary_unit_declaration } end units [ name ]

   EXTEND("physical type definition");

   type_t t = type_new(T_PHYSICAL);

   consume(tUNITS);

   tree_t base = p_base_unit_declaration();
   type_add_unit(t, base);

   r.left  = int_to_physical(r.left, base);
   r.right = int_to_physical(r.right, base);
   type_add_dim(t, r);

   while (scan(tINT, tREAL, tID)) {
      tree_t unit = p_secondary_unit_declaration();
      type_add_unit(t, unit);
   }

   consume(tEND);
   consume(tUNITS);

   if (peek() == tID) {
      ident_t id = p_identifier();
      (void)id;  // XXX: test this
   }

   return t;
}

static tree_t p_enumeration_literal(void)
{
   // identifier | character_literal

   BEGIN("enumeration literal");

   tree_t t = tree_new(T_ENUM_LIT);
   tree_set_ident(t, p_identifier());
   tree_set_loc(t, CURRENT_LOC);

   return t;
}

static type_t p_enumeration_type_definition(void)
{
   // ( enumeration_literal { , enumeration_literal } )

   BEGIN("enumeration type definition");

   type_t t = type_new(T_ENUM);

   consume(tLPAREN);

   unsigned pos = 0;
   do {
      tree_t lit = p_enumeration_literal();
      tree_set_pos(lit, pos++);
      tree_set_type(lit, t);
      type_enum_add_literal(t, lit);
   } while (optional(tCOMMA));

   consume(tRPAREN);

   return t;
}

static type_t p_scalar_type_definition(void)
{
   // enumeration_type_definition | integer_type_definition
   //   | floating_type_definition | physical_type_definition

   BEGIN("scalar type definition");

   switch (peek()) {
   case tRANGE:
      {
         range_t r = p_range_constraint();

         if (peek() == tUNITS)
            return p_physical_type_definition(r);
         else {
            const bool real = ((r.left != NULL)
                               && (tree_kind(r.left) == T_LITERAL)
                               && (tree_subkind(r.left) == L_REAL))
               || ((r.right != NULL)
                   && (tree_kind(r.right) == T_LITERAL)
                   && (tree_subkind(r.right) == L_REAL));

            if (real)
               return p_real_type_definition(r);
            else
               return p_integer_type_definition(r);
         }
      }

   case tLPAREN:
      return p_enumeration_type_definition();

   default:
      expect(tRANGE);
      return type_new(T_NONE);
   }
}

static type_t p_access_type_definition(void)
{
   // access subtype_indication

   BEGIN("access type definition");

   consume(tACCESS);

   type_t t = type_new(T_ACCESS);
   type_set_access(t, p_subtype_indication());

   return t;
}

static type_t p_file_type_definition(void)
{
   // file of type_mark

   BEGIN("file type definition");

   consume(tFILE);
   consume(tOF);

   type_t t = type_new(T_FILE);
   type_set_file(t, p_type_mark());

   return t;
}

static void p_element_declaration(type_t rec)
{
   // identifier_list : element_subtype_definition ;

   BEGIN("element declaration");

   LOCAL_IDENT_LIST ids = p_identifier_list();

   consume(tCOLON);

   type_t type = p_subtype_indication();

   consume(tSEMI);

   for (ident_list_t *it = ids; it != NULL; it = it->next) {
      tree_t f = tree_new(T_FIELD_DECL);
      tree_set_ident(f, it->ident);
      tree_set_type(f, type);
      tree_set_loc(f, CURRENT_LOC);

      type_add_field(rec, f);
   }
}

static type_t p_record_type_definition(void)
{
   // record element_declaration { element_declaration } end record
   //   [ simple_name ]

   BEGIN("record type definition");

   consume(tRECORD);

   type_t r = type_new(T_RECORD);

   do {
      p_element_declaration(r);
   } while (peek() == tID);

   consume(tEND);
   consume(tRECORD);

   if (peek() == tID) {
      ident_t id = p_identifier();
      (void)id;  // XXX: test this
   }

   return r;
}

static type_t p_index_subtype_definition(void)
{
   // type_mark range <>

   BEGIN("index subtype definition");

   type_t t = p_type_mark();

   consume(tRANGE);
   consume(tBOX);

   return t;
}

static type_t p_unconstrained_array_definition(void)
{
   // array ( index_subtype_definition { , index_subtype_definition } )
   //   of subtype_indication

   BEGIN("unconstrained array definition");

   consume(tARRAY);
   consume(tLPAREN);

   type_t t = type_new(T_UARRAY);
   do {
      type_add_index_constr(t, p_index_subtype_definition());
   } while (optional(tCOMMA));

   consume(tRPAREN);
   consume(tOF);

   type_set_elem(t, p_subtype_indication());
   return t;
}

static type_t p_constrained_array_definition(void)
{
   // array index_constraint of element_subtype_indication

   BEGIN("constrained array definition");

   consume(tARRAY);

   type_t t = type_new(T_CARRAY);
   p_index_constraint(t);

   consume(tOF);

   type_set_elem(t, p_subtype_indication());
   return t;
}

static type_t p_array_type_definition(void)
{
   // unconstrained_array_definition | constrained_array_definition

   BEGIN("array type definition");

   const look_params_t lookp = {
      .look     = { tBOX },
      .stop     = { tRPAREN },
      .abort    = tSEMI,
      .nest_in  = tLPAREN,
      .nest_out = tRPAREN,
      .depth    = 1
   };

   if (look_for(&lookp))
      return p_unconstrained_array_definition();
   else
      return p_constrained_array_definition();
}

static type_t p_composite_type_definition(void)
{
   // array_type_definition | record_type_definition

   BEGIN("composite type definition");

   switch (peek()) {
   case tRECORD:
      return p_record_type_definition();

   case tARRAY:
      return p_array_type_definition();

   default:
      expect(tRECORD, tARRAY);
      return type_new(T_NONE);
   }
}

static void p_protected_type_declarative_item(type_t type)
{
   // subprogram_declaration | 2008: subprogram_instantiation_declaration
   //   | attribute_specification | use_clause

   BEGIN("protected type declarative item");

   switch (peek()) {
   case tATTRIBUTE:
      p_attribute_specification((tree_t)type, (add_func_t)type_add_unit);
      break;

   case tUSE:
      p_use_clause((tree_t)type, (add_func_t)type_add_unit);
      break;

   case tFUNCTION:
   case tPROCEDURE:
   case tIMPURE:
   case tPURE:
      {
         tree_t spec = p_subprogram_specification();
         type_add_decl(type, p_subprogram_declaration(spec));
      }
      break;

   default:
      expect(tATTRIBUTE, tUSE, tFUNCTION, tPROCEDURE, tIMPURE, tPURE);
   }
}

static void p_protected_type_declarative_part(type_t type)
{
   // { protected_type_declarative_item }

   BEGIN("protected type declarative part");

   while (not_at_token(tEND))
      p_protected_type_declarative_item(type);
}

static type_t p_protected_type_declaration(void)
{
   // protected protected_type_declarative_part end protected [ simple_name ]

   BEGIN("protected type declaration");

   consume(tPROTECTED);

   type_t type = type_new(T_PROTECTED);

   p_protected_type_declarative_part(type);

   consume(tEND);
   consume(tPROTECTED);

   if (peek() == tID)
      type_set_ident(type, p_identifier());

   return type;
}

static type_t p_protected_type_definition(void)
{
   // protected_type_declaration | protected_type_body

   BEGIN("protected type definition");

   // Protected type bodies are trees rather than types and so handled
   // elsewhere to simplify the parser

   return p_protected_type_declaration();
}

static type_t p_type_definition(void)
{
   // scalar_type_definition | composite_type_definition
   //   | access_type_definition | file_type_definition
   //   | 2000: protected_type_definition

   BEGIN("type definition");

   switch (peek()) {
   case tRANGE:
   case tLPAREN:
      return p_scalar_type_definition();

   case tACCESS:
      return p_access_type_definition();

   case tFILE:
      return p_file_type_definition();

   case tRECORD:
   case tARRAY:
      return p_composite_type_definition();

   case tPROTECTED:
      return p_protected_type_definition();

   default:
      expect(tRANGE, tACCESS, tFILE, tRECORD, STD(00, tPROTECTED));
      return type_new(T_NONE);
   }
}

static type_t p_full_type_declaration(ident_t id)
{
   // type identifier is type_definition ;

   EXTEND("full type declaration");

   consume(tIS);

   type_t t = p_type_definition();
   type_set_ident(t, id);

   consume(tSEMI);

   return t;
}

static type_t p_incomplete_type_declaration(ident_t id)
{
   // type identifier ;

   EXTEND("incomplete type declaration");

   consume(tSEMI);

   type_t t = type_new(T_INCOMPLETE);
   type_set_ident(t, id);

   return t;
}

static tree_t p_type_declaration(void)
{
   // full_type_declaration | incomplete_type_declaration

   BEGIN("type declaration");

   consume(tTYPE);

   ident_t id = p_identifier();

   // Protected type bodies are broken out here to avoid having to
   // return a dummy type for them in p_full_type_declaration

   const bool is_prot_body =
      (peek_nth(1) == tIS)
      && (peek_nth(2) == tPROTECTED)
      && (peek_nth(3) == tBODY);

   if (is_prot_body) {
      consume(tIS);
      tree_t body = p_protected_type_body();
      consume(tSEMI);

      if (tree_has_ident(body)) {
         if (tree_ident(body) != id)
            parse_error(CURRENT_LOC, "expected protected body trailing label "
                        "to match %s", istr(id));
      }
      else
         tree_set_ident(body, id);

      return body;
   }
   else {
      type_t type;
      if (peek() == tSEMI)
         type = p_incomplete_type_declaration(id);
      else
         type = p_full_type_declaration(id);

      type_set_ident(type, id);

      tree_t t = tree_new(T_TYPE_DECL);
      tree_set_ident(t, id);
      tree_set_type(t, type);
      tree_set_loc(t, CURRENT_LOC);

      return t;
   }
}

static tree_t p_subtype_declaration(void)
{
   // subtype identifier is subtype_indication ;

   BEGIN("subtype declaration");

   consume(tSUBTYPE);
   ident_t id = p_identifier();
   consume(tIS);
   type_t sub = p_subtype_indication();
   consume(tSEMI);

   if (type_kind(sub) != T_SUBTYPE) {
      // Case where subtype_indication did not impose any
      // constraint so we must create the subtype object here
      type_t new = type_new(T_SUBTYPE);
      type_set_base(new, sub);

      sub = new;
   }
   type_set_ident(sub, id);

   tree_t t = tree_new(T_TYPE_DECL);
   tree_set_ident(t, id);
   tree_set_type(t, sub);
   tree_set_loc(t, CURRENT_LOC);

   return t;
}

static void p_constant_declaration(tree_t parent)
{
   // constant identifier_list : subtype_indication [ := expression ] ;

   BEGIN("constant declaration");

   consume(tCONSTANT);

   LOCAL_IDENT_LIST ids = p_identifier_list();

   consume(tCOLON);

   type_t type = p_subtype_indication();

   tree_t init = NULL;
   if (optional(tASSIGN))
      init = p_expression();

   consume(tSEMI);

   for (ident_list_t *it = ids; it != NULL; it = it->next) {
      tree_t t = tree_new(T_CONST_DECL);
      tree_set_ident(t, it->ident);
      tree_set_type(t, type);
      tree_set_loc(t, CURRENT_LOC);
      if (init != NULL)
         tree_set_value(t, init);

      tree_add_decl(parent, t);
   }
}

static tree_t p_assertion(void)
{
   // assert condition [ report expression ] [ severity expression ]

   BEGIN("assertion");

   tree_t s = tree_new(T_ASSERT);

   consume(tASSERT);

   tree_set_value(s, p_expression());

   if (optional(tREPORT))
      tree_set_message(s, p_expression());

   if (optional(tSEVERITY))
      tree_set_severity(s, p_expression());
   else {
      tree_t sev = tree_new(T_REF);
      tree_set_ident(sev, ident_new("ERROR"));

      tree_set_severity(s, sev);
   }

   tree_set_loc(s, CURRENT_LOC);
   return s;
}

static tree_t p_concurrent_assertion_statement(ident_t label)
{
   // [ label : ] [ postponed ] assertion ;

   BEGIN("concurrent assertion statement");

   const bool postponed = optional(tPOSTPONED);

   tree_t s = p_assertion();
   tree_change_kind(s, T_CASSERT);

   consume(tSEMI);

   const loc_t *loc = CURRENT_LOC;
   tree_set_loc(s, loc);

   if (label == NULL)
      label = loc_to_ident(loc);
   tree_set_ident(s, label);

   if (postponed)
      tree_set_flag(s, TREE_F_POSTPONED);

   return s;
}

static ident_t p_designator(void)
{
   // identifier | operator_symbol

   BEGIN("designator");

   switch (peek()) {
   case tID:
      return p_identifier();
   case tSTRING:
      return p_operator_symbol();
   default:
      expect(tID, tSTRING);
      return ident_new("error");
   }
}

static tree_t p_subprogram_specification(void)
{
   // procedure designator [ ( formal_parameter_list ) ]
   //   | [ pure | impure ] function designator [ ( formal_parameter_list ) ]
   //     return type_mark

   BEGIN("subprogram specification");

   tree_t t = NULL;
   type_t type = NULL;

   bool impure = false;
   if (optional(tIMPURE))
      impure = true;
   else if (optional(tPURE))
      ;

   switch (one_of(tFUNCTION, tPROCEDURE)) {
   case tFUNCTION:
      t = tree_new(T_FUNC_DECL);
      type = type_new(T_FUNC);
      break;

   case tPROCEDURE:
      t = tree_new(T_PROC_DECL);
      type = type_new(T_PROC);
      break;

   default:
      return tree_new(T_FUNC_DECL);
   }

   tree_set_type(t, type);
   tree_set_ident(t, p_designator());

   type_set_ident(type, tree_ident(t));

   if (impure)
      tree_set_flag(t, TREE_F_IMPURE);

   if (optional(tLPAREN)) {
      //const class_t class =
      //   (tree_kind(t) == T_FUNC_DECL) ? C_CONSTANT : C_VARIABLE;
      p_interface_list(C_DEFAULT, t, tree_add_port);
      consume(tRPAREN);
   }

   if (tree_kind(t) == T_FUNC_DECL) {
      consume(tRETURN);
      type_set_result(type, p_type_mark());
   }

   tree_set_loc(t, CURRENT_LOC);
   return t;
}

static void p_variable_declaration(tree_t parent)
{
   // [ shared ] variable identifier_list : subtype_indication
   //   [ := expression ] ;

   BEGIN("variable declaration");

   const bool shared = optional(tSHARED);

   consume(tVARIABLE);

   LOCAL_IDENT_LIST ids = p_identifier_list();

   consume(tCOLON);

   type_t type = p_subtype_indication();

   tree_t init = NULL;
   if (optional(tASSIGN))
      init = p_expression();

   consume(tSEMI);

   const loc_t *loc = CURRENT_LOC;

   for (ident_list_t *it = ids; it != NULL; it = it->next) {
      tree_t t = tree_new(T_VAR_DECL);
      tree_set_loc(t, loc);
      tree_set_ident(t, it->ident);
      tree_set_type(t, type);

      if (init != NULL)
         tree_set_value(t, init);

      if (shared)
         tree_set_flag(t, TREE_F_SHARED);

      tree_add_decl(parent, t);
   }
}

static void p_signal_declaration(tree_t parent)
{
   // signal identifier_list : subtype_indication [ signal_kind ]
   //   [ := expression ] ;

   BEGIN("signal declaration");

   consume(tSIGNAL);

   LOCAL_IDENT_LIST ids = p_identifier_list();

   consume(tCOLON);

   type_t type = p_subtype_indication();

   // [ signal_kind ]

   tree_t init = NULL;
   if (optional(tASSIGN))
      init = p_expression();

   consume(tSEMI);

   const loc_t *loc = CURRENT_LOC;

   for (ident_list_t *it = ids; it != NULL; it = it->next) {
      tree_t t = tree_new(T_SIGNAL_DECL);
      tree_set_loc(t, loc);
      tree_set_ident(t, it->ident);
      tree_set_type(t, type);

      if (init != NULL)
         tree_set_value(t, init);

      tree_add_decl(parent, t);
   }
}

static type_t p_signature(void)
{
   // [ [ type_mark { , type_mark } ] [ return type_mark ] ]

   BEGIN("signature");

   const look_params_t lookp = {
      .look   = { tRETURN },
      .stop   = { tRSQUARE },
      .abort  = tSEMI
   };

   type_t type = type_new(look_for(&lookp) ? T_FUNC : T_PROC);

   consume(tLSQUARE);

   if (not_at_token(tRETURN, tRSQUARE)) {
      type_add_param(type, p_type_mark());
      while (optional(tCOMMA))
         type_add_param(type, p_type_mark());
   }

   if (optional(tRETURN))
      type_set_result(type, p_type_mark());

   consume(tRSQUARE);

   return type;
}

static tree_t p_alias_declaration(void)
{
   // alias alias_designator [ : subtype_indication ] is name [ signature ] ;

   BEGIN("alias declaration");

   tree_t t = tree_new(T_ALIAS);

   bool has_subtype_indication = false;
   consume(tALIAS);
   tree_set_ident(t, p_identifier());
   if (optional(tCOLON)) {
      tree_set_type(t, p_subtype_indication());
      has_subtype_indication = true;
   }
   consume(tIS);
   tree_set_value(t, p_name());

   if (peek() == tLSQUARE) {
      tree_set_type(t, p_signature());
      if (has_subtype_indication)
         parse_error(CURRENT_LOC, "alias declaration may not contain both a "
                     "signature and a subtype indication");
   }

   consume(tSEMI);

   tree_set_loc(t, CURRENT_LOC);
   return t;
}

static void p_file_open_information(tree_t *mode, tree_t *name)
{
   // [ open expression ] is file_logical_name

   BEGIN("file open information");

   if (optional(tOPEN))
      *mode = p_expression();
   else
      *mode = NULL;

   if (optional(tIS)) {
      if ((*mode == NULL) && scan(tIN, tOUT)) {
         // VHDL-87 compatibility
         switch (one_of(tIN, tOUT)) {
         case tIN:
            *mode = tree_new(T_REF);
            tree_set_ident(*mode, ident_new("READ_MODE"));
            break;

         case tOUT:
            *mode = tree_new(T_REF);
            tree_set_ident(*mode, ident_new("WRITE_MODE"));
            break;
         }
      }

      *name = p_expression();

      if (*mode == NULL) {
         *mode = tree_new(T_REF);
         tree_set_ident(*mode, ident_new("READ_MODE"));
      }
   }
   else
      *name = NULL;
}

static void p_file_declaration(tree_t parent)
{
   // file identifier_list : subtype_indication [ file_open_information ] ;

   BEGIN("file declaration");

   consume(tFILE);

   LOCAL_IDENT_LIST ids = p_identifier_list();

   consume(tCOLON);

   type_t type = p_subtype_indication();

   tree_t mode, name;
   p_file_open_information(&mode, &name);

   consume(tSEMI);

   for (ident_list_t *it = ids; it != NULL; it = it->next) {
      tree_t t = tree_new(T_FILE_DECL);
      tree_set_ident(t, it->ident);
      tree_set_type(t, type);
      if (name != NULL) {
         tree_set_file_mode(t, mode);
         tree_set_value(t, name);
      }
      tree_set_loc(t, CURRENT_LOC);

      tree_add_decl(parent, t);
   }
}

static void p_protected_type_body_declarative_item(tree_t body)
{
   // subprogram_declaration | subprogram_body | type_declaration
   //   | subtype_declaration | constant_declaration | variable_declaration
   //   | file_declaration | alias_declaration | attribute_declaration
   //   | attribute_specification | use_clause | group_template_declaration
   //   | group_declaration

   BEGIN("protected type body declarative item");

   switch (peek()) {
   case tATTRIBUTE:
      if (peek_nth(3) == tOF)
         p_attribute_specification(body, tree_add_decl);
      else
         tree_add_decl(body, p_attribute_declaration());
      break;

   case tTYPE:
      tree_add_decl(body, p_type_declaration());
      break;

   case tSUBTYPE:
      tree_add_decl(body, p_subtype_declaration());
      break;

   case tCONSTANT:
      p_constant_declaration(body);
      break;

   case tALIAS:
      tree_add_decl(body, p_alias_declaration());
      break;

   case tFUNCTION:
   case tPROCEDURE:
   case tIMPURE:
   case tPURE:
      {
         tree_t spec = p_subprogram_specification();
         if (peek() == tSEMI)
            tree_add_decl(body, p_subprogram_declaration(spec));
         else
            tree_add_decl(body, p_subprogram_body(spec));
      }
      break;

   case tVARIABLE:
      p_variable_declaration(body);
      break;

   case tUSE:
      p_use_clause(body, tree_add_decl);
      break;

   case tFILE:
      p_file_declaration(body);
      break;

   default:
      expect(tATTRIBUTE, tTYPE, tSUBTYPE, tCONSTANT, tFUNCTION, tPROCEDURE,
             tIMPURE, tPURE, tALIAS, tVARIABLE, tUSE, tFILE);
   }
}

static void p_protected_type_body_declarative_part(tree_t body)
{
   // { protected_type_body_declarative_item }

   BEGIN("protected type body declarative part");

   while (not_at_token(tEND))
      p_protected_type_body_declarative_item(body);
}

static tree_t p_protected_type_body(void)
{
   // protected body protected_type_body_declarative_part end protected body
   //   [ simple name ]

   BEGIN("protected type body");

   consume(tPROTECTED);
   consume(tBODY);

   tree_t body = tree_new(T_PROT_BODY);
   p_protected_type_body_declarative_part(body);

   consume(tEND);
   consume(tPROTECTED);
   consume(tBODY);

   if (peek() == tID)
      tree_set_ident(body, p_identifier());

   tree_set_loc(body, CURRENT_LOC);
   return body;
}

static void p_entity_declarative_item(tree_t entity)
{
   // subprogram_declaration | subprogram_body | type_declaration
   //   | subtype_declaration | constant_declaration | signal_declaration
   //   | shared_variable_declaration | file_declaration | alias_declaration
   //   | attribute_declaration | attribute_specification
   //   | disconnection_specification | use_clause | group_template_declaration
   //   | group_declaration

   BEGIN("entity declarative item");

   switch (peek()) {
   case tATTRIBUTE:
      if (peek_nth(3) == tOF)
         p_attribute_specification(entity, tree_add_decl);
      else
         tree_add_decl(entity, p_attribute_declaration());
      break;

   case tTYPE:
      tree_add_decl(entity, p_type_declaration());
      break;

   case tSUBTYPE:
      tree_add_decl(entity, p_subtype_declaration());
      break;

   case tCONSTANT:
      p_constant_declaration(entity);
      break;

   case tALIAS:
      tree_add_decl(entity, p_alias_declaration());
      break;

   case tFUNCTION:
   case tPROCEDURE:
   case tIMPURE:
   case tPURE:
      {
         tree_t spec = p_subprogram_specification();
         if (peek() == tSEMI)
            tree_add_decl(entity, p_subprogram_declaration(spec));
         else
            tree_add_decl(entity, p_subprogram_body(spec));
      }
      break;

   case tUSE:
      p_use_clause(entity, tree_add_decl);
      break;

   default:
      expect(tATTRIBUTE, tTYPE, tSUBTYPE, tCONSTANT, tFUNCTION, tPROCEDURE,
             tIMPURE, tPURE, tALIAS, tUSE);
   }
}

static void p_entity_declarative_part(tree_t entity)
{
   // { entity_declarative_item }

   BEGIN("entity declarative part");

   while (not_at_token(tEND, tBEGIN))
      p_entity_declarative_item(entity);
}

static void p_subprogram_declarative_item(tree_t sub)
{
   // subprogram_declaration | subprogram_body | type_declaration
   //   | subtype_declaration | constant_declaration | variable_declaration
   //   | file_declaration | alias_declaration | attribute_declaration
   //   | attribute_specification | use_clause | group_template_declaration
   //   | group_declaration

   BEGIN("subprogram delcarative item");

   switch (peek()) {
   case tVARIABLE:
      p_variable_declaration(sub);
      break;

   case tTYPE:
      tree_add_decl(sub, p_type_declaration());
      break;

   case tALIAS:
      tree_add_decl(sub, p_alias_declaration());
      break;

   case tCONSTANT:
      p_constant_declaration(sub);
      break;

   case tFUNCTION:
   case tPROCEDURE:
   case tIMPURE:
   case tPURE:
      {
         tree_t spec = p_subprogram_specification();
         if (peek() == tSEMI)
            tree_add_decl(sub, p_subprogram_declaration(spec));
         else
            tree_add_decl(sub, p_subprogram_body(spec));
      }
      break;

   case tATTRIBUTE:
      if (peek_nth(3) == tOF)
         p_attribute_specification(sub, tree_add_decl);
      else
         tree_add_decl(sub, p_attribute_declaration());
      break;

   case tSUBTYPE:
      tree_add_decl(sub, p_subtype_declaration());
      break;

   case tUSE:
      p_use_clause(sub, tree_add_decl);
      break;

   case tFILE:
      p_file_declaration(sub);
      break;

   default:
      expect(tVARIABLE, tTYPE, tALIAS, tCONSTANT, tFUNCTION, tPROCEDURE,
             tIMPURE, tPURE, tATTRIBUTE, tSUBTYPE, tUSE, tFILE);
   }
}

static void p_subprogram_declarative_part(tree_t sub)
{
   // { subprogram_declarative_item }

   BEGIN("subprogram declarative part");

   while (not_at_token(tBEGIN))
      p_subprogram_declarative_item(sub);
}

static void p_sequence_of_statements(tree_t parent, add_func_t addf)
{
   // { sequential_statement }

   BEGIN("sequence of statements");

   while (not_at_token(tEND, tELSE, tELSIF, tWHEN))
      (*addf)(parent, p_sequential_statement());
}

static void p_trailing_label(ident_t label)
{
   // [ label ]

   if ((peek() == tID) || (peek() == tSTRING)) {
      ident_t trailing = p_designator();
      if (label == NULL)
         parse_error(&last_loc, "unexpected trailing label for %s without "
                     "label", hint_str);
      else if (trailing != label)
         parse_error(&last_loc, "expected trailing %s label to match %s",
                     hint_str, istr(label));
   }
}

static tree_t p_subprogram_body(tree_t spec)
{
   // subprogram_specification is subprogram_declarative_part begin
   //   subprogram_statement_part end [ subprogram_kind ] [ designator ] ;

   EXTEND("subprogram body");

   consume(tIS);

   const tree_kind_t kind =
      (tree_kind(spec) == T_FUNC_DECL) ? T_FUNC_BODY : T_PROC_BODY;
   tree_change_kind(spec, kind);

   p_subprogram_declarative_part(spec);

   consume(tBEGIN);

   p_sequence_of_statements(spec, tree_add_stmt);

   consume(tEND);

   switch (peek()) {
   case tFUNCTION:
      consume(tFUNCTION);
      // XXX: check is function;
      break;

   case tPROCEDURE:
      consume(tPROCEDURE);
      // XXX: check is procedure
      break;

   default:
      break;
   }

   p_trailing_label(tree_ident(spec));
   consume(tSEMI);

   tree_set_loc(spec, CURRENT_LOC);
   return spec;
}

static tree_t p_subprogram_declaration(tree_t spec)
{
   // subprogram_specification ;

   EXTEND("subprogram declaration");

   consume(tSEMI);
   return spec;
}

static void p_sensitivity_list(tree_t proc)
{
   // name { , name }

   BEGIN("sensitivity list");

   tree_add_trigger(proc, p_name());

   while (optional(tCOMMA))
      tree_add_trigger(proc, p_name());
}

static void p_process_declarative_item(tree_t proc)
{
   // subprogram_declaration | subprogram_body | type_declaration
   //   | subtype_declaration | constant_declaration | variable_declaration
   //   | file_declaration | alias_declaration | attribute_declaration
   //   | attribute_specification | use_clause | group_template_declaration
   //   | group_declaration

   BEGIN("process declarative item");

   switch (peek()) {
   case tVARIABLE:
      p_variable_declaration(proc);
      break;

   case tTYPE:
      tree_add_decl(proc, p_type_declaration());
      break;

   case tSUBTYPE:
      tree_add_decl(proc, p_subtype_declaration());
      break;

   case tCONSTANT:
      p_constant_declaration(proc);
      break;

   case tFUNCTION:
   case tPROCEDURE:
   case tIMPURE:
   case tPURE:
      {
         tree_t spec = p_subprogram_specification();
         if (peek() == tSEMI)
            tree_add_decl(proc, p_subprogram_declaration(spec));
         else
            tree_add_decl(proc, p_subprogram_body(spec));
      }
      break;

   case tATTRIBUTE:
      if (peek_nth(3) == tOF)
         p_attribute_specification(proc, tree_add_decl);
      else
         tree_add_decl(proc, p_attribute_declaration());
      break;

   case tUSE:
      p_use_clause(proc, tree_add_decl);
      break;

   case tALIAS:
      tree_add_decl(proc, p_alias_declaration());
      break;

   case tFILE:
      p_file_declaration(proc);
      break;

   default:
      expect(tVARIABLE, tTYPE, tSUBTYPE, tCONSTANT, tFUNCTION, tPROCEDURE,
             tIMPURE, tPURE, tATTRIBUTE, tUSE, tALIAS, tFILE);
   }
}

static void p_process_declarative_part(tree_t proc)
{
   // { process_declarative_item }

   BEGIN("process declarative part");

   while (not_at_token(tBEGIN))
      p_process_declarative_item(proc);
}

static void p_process_statement_part(tree_t proc)
{
   // { sequential_statement }

   BEGIN("process statement part");

   p_sequence_of_statements(proc, tree_add_stmt);
}

static tree_t p_process_statement(ident_t label)
{
   // [ process_label : ] [ postponed ] process [ ( sensitivity_list ) ] [ is ]
   //   process_declarative_part begin process_statement_part end [ postponed ]
   //   process [ label ] ;

   EXTEND("process statement");

   tree_t t = tree_new(T_PROCESS);

   const bool postponed = optional(tPOSTPONED);

   consume(tPROCESS);

   if (optional(tLPAREN)) {
      p_sensitivity_list(t);
      consume(tRPAREN);
   }

   optional(tIS);

   p_process_declarative_part(t);

   consume(tBEGIN);

   p_process_statement_part(t);

   consume(tEND);
   if (postponed)
      optional(tPOSTPONED);
   consume(tPROCESS);
   p_trailing_label(label);
   consume(tSEMI);

   const loc_t *loc = CURRENT_LOC;
   tree_set_loc(t, loc);

   if (label == NULL)
      label = loc_to_ident(loc);
   tree_set_ident(t, label);

   if (postponed)
      tree_set_flag(t, TREE_F_POSTPONED);

   return t;
}

static tree_t p_entity_statement(void)
{
   // concurrent_assertion_statement | concurrent_procedure_call_statement
   //   | process_statement

   BEGIN("entity statement");

   ident_t label = NULL;
   if ((peek() == tID) && (peek_nth(2) == tCOLON)) {
      label = p_identifier();
      consume(tCOLON);
   }

   switch (peek()) {
   case tASSERT:
      return p_concurrent_assertion_statement(label);

   case tPROCESS:
      return p_process_statement(label);

   case tPOSTPONED:
      if (peek_nth(2) == tASSERT)
         return p_concurrent_assertion_statement(label);
      else
         return p_process_statement(label);

   default:
      expect(tASSERT, tPOSTPONED);
      return tree_new(T_NULL);
   }
}

static void p_entity_statement_part(tree_t entity)
{
   // { entity_statement }

   BEGIN("entity statement part");

   while (not_at_token(tEND))
      tree_add_stmt(entity, p_entity_statement());
}

static void p_entity_declaration(tree_t unit)
{
   // entity identifier is entity_header entity_declarative_part
   //   [ begin entity_statement_part ] end [ entity ] [ entity_simple_name ] ;

   BEGIN("entity declaration");

   tree_change_kind(unit, T_ENTITY);

   consume(tENTITY);

   ident_t id = p_identifier();
   tree_set_ident(unit, id);

   consume(tIS);

   p_entity_header(unit);
   p_entity_declarative_part(unit);

   if (optional(tBEGIN))
      p_entity_statement_part(unit);

   consume(tEND);
   optional(tENTITY);
   p_trailing_label(id);
   consume(tSEMI);

   tree_set_loc(unit, CURRENT_LOC);
}

static tree_t p_component_declaration(void)
{
   // component identifier [ is ] [ generic_clause ] [ port_clause ]
   //   end component [ simple_name ] ;

   BEGIN("component declaration");

   tree_t c = tree_new(T_COMPONENT);

   consume(tCOMPONENT);
   tree_set_ident(c, p_identifier());
   optional(tIS);

   if (peek() == tGENERIC)
      p_generic_clause(c);

   if (peek() == tPORT)
      p_port_clause(c);

   consume(tEND);
   consume(tCOMPONENT);
   p_trailing_label(tree_ident(c));
   consume(tSEMI);

   tree_set_loc(c, CURRENT_LOC);
   return c;
}

static void p_package_declarative_item(tree_t pack)
{
   // subprogram_declaration | type_declaration | subtype_declaration
   //   | constant_declaration | signal_declaration
   //   | shared_variable_declaration | file_declaration | alias_declaration
   //   | component_declaration | attribute_declaration
   //   | attribute_specification | disconnection_specification | use_clause
   //   | group_template_declaration | group_declaration
   //

   BEGIN("package declarative item");

   switch (peek()) {
   case tTYPE:
      tree_add_decl(pack, p_type_declaration());
      break;

   case tFUNCTION:
   case tPROCEDURE:
   case tIMPURE:
   case tPURE:
      {
         tree_t spec = p_subprogram_specification();
         if (peek() == tSEMI)
            tree_add_decl(pack, p_subprogram_declaration(spec));
         else
            tree_add_decl(pack, p_subprogram_body(spec));
      }
      break;

   case tSUBTYPE:
      tree_add_decl(pack, p_subtype_declaration());
      break;

   case tSIGNAL:
      p_signal_declaration(pack);
      break;

   case tATTRIBUTE:
      if (peek_nth(3) == tOF)
         p_attribute_specification(pack, tree_add_decl);
      else
         tree_add_decl(pack, p_attribute_declaration());
      break;

   case tCONSTANT:
      p_constant_declaration(pack);
      break;

   case tCOMPONENT:
      tree_add_decl(pack, p_component_declaration());
      break;

   case tFILE:
      p_file_declaration(pack);
      break;

   case tSHARED:
      p_variable_declaration(pack);
      break;

   case tALIAS:
      tree_add_decl(pack, p_alias_declaration());
      break;

   case tUSE:
      p_use_clause(pack, tree_add_decl);
      break;

   default:
      expect(tTYPE, tFUNCTION, tPROCEDURE, tIMPURE, tPURE, tSUBTYPE, tSIGNAL,
             tATTRIBUTE, tCONSTANT, tCOMPONENT, tFILE, tSHARED, tALIAS, tUSE);
   }
}

static void p_package_declarative_part(tree_t pack)
{
   // { package_declarative_item }

   BEGIN("package declarative part");

   while (not_at_token(tEND))
      p_package_declarative_item(pack);
}

static void p_package_declaration(tree_t unit)
{
   // package identifier is package_declarative_part end [ package ]
   //   [ simple_name ] ;

   BEGIN("package declaration");

   consume(tPACKAGE);

   tree_change_kind(unit, T_PACKAGE);
   tree_set_ident(unit, p_identifier());

   consume(tIS);

   p_package_declarative_part(unit);

   consume(tEND);
   optional(tPACKAGE);
   p_trailing_label(tree_ident(unit));
   consume(tSEMI);

   tree_set_loc(unit, CURRENT_LOC);
}

static ident_list_t *p_instantiation_list(void)
{
   // label { , label } | others | all

   switch (peek()) {
   case tID:
      return p_identifier_list();

   case tOTHERS:
      consume(tOTHERS);
      return NULL;

   case tALL:
      {
         consume(tALL);

         ident_list_t *result = NULL;
         ident_list_add(&result, all_i);
         return result;
      }

   default:
      expect(tID, tOTHERS, tALL);
      return NULL;
   }
}

static ident_list_t *p_component_specification(ident_t *comp_name)
{
   // instantiation_list : name

   BEGIN("component specification");

   ident_list_t *ids = p_instantiation_list();
   consume(tCOLON);
   *comp_name = p_identifier();

   return ids;
}

static void p_port_map_aspect(tree_t inst)
{
   // port map ( association_list )

   BEGIN("port map aspect");

   consume(tPORT);
   consume(tMAP);
   consume(tLPAREN);

   p_association_list(inst, tree_add_param);

   consume(tRPAREN);
}

static void p_generic_map_aspect(tree_t inst)
{
   // generic map ( association_list )

   BEGIN("generic map aspect");

   consume(tGENERIC);
   consume(tMAP);
   consume(tLPAREN);

   p_association_list(inst, tree_add_genmap);

   consume(tRPAREN);
}

static tree_t p_entity_aspect(void)
{
   // entity name [ ( identifier) ] | configuration name | open

   switch (one_of(tENTITY, tCONFIGURATION, tOPEN)) {
   case tENTITY:
      {
         tree_t bind = tree_new(T_BINDING);
         tree_set_class(bind, C_ENTITY);
         tree_set_ident(bind, p_selected_identifier());
         if (optional(tLPAREN)) {
            tree_set_ident2(bind, p_identifier());
            consume(tRPAREN);
         }

         return bind;
      }

   case tCONFIGURATION:
      {
         tree_t bind = tree_new(T_BINDING);
         tree_set_class(bind, C_CONFIGURATION);
         tree_set_ident(bind, p_identifier());

         return bind;
      }

   case tOPEN:
   default:
      return NULL;
   }
}

static tree_t p_binding_indication(void)
{
   // [ use entity_aspect ] [ generic_map_aspect ] [ port_map_aspect ]

   BEGIN("binding indication");

   tree_t bind = NULL;
   if (optional(tUSE))
      bind = p_entity_aspect();
   else
      bind = tree_new(T_BINDING);

   if (peek() == tGENERIC) {
      assert(bind != NULL);   // XXX: check for open here
      p_generic_map_aspect(bind);
   }

   if (peek() == tPORT) {
      assert(bind != NULL);   // XXX: check for open here
      p_port_map_aspect(bind);
   }

   if (bind != NULL)
      tree_set_loc(bind, CURRENT_LOC);

   return bind;
}

static void p_configuration_specification(tree_t parent)
{
   // for component_specification binding_indication ;

   BEGIN("configuration specification");

   consume(tFOR);

   ident_t comp_name;
   LOCAL_IDENT_LIST ids = p_component_specification(&comp_name);

   tree_t bind = p_binding_indication();
   consume(tSEMI);

   const loc_t *loc = CURRENT_LOC;

   if (ids != NULL) {
      for (ident_list_t *it = ids; it != NULL; it = it->next) {
         tree_t t = tree_new(T_SPEC);
         tree_set_loc(t, loc);
         tree_set_ident(t, it->ident);
         tree_set_ident2(t, comp_name);
         tree_set_value(t, bind);

         tree_add_decl(parent, t);
      }
   }
   else {
      // Instantiation list was "others"
      tree_t t = tree_new(T_SPEC);
      tree_set_loc(t, loc);
      tree_set_ident2(t, comp_name);
      tree_set_value(t, bind);

      tree_add_decl(parent, t);
   }
}

static void p_configuration_declarative_part(tree_t unit)
{
   // use_clause | attribute_specification | group_declaration

   BEGIN("configuration declarative part");

   switch (peek()) {
   case tUSE:
      p_use_clause(unit, tree_add_decl);
      break;

   case tATTRIBUTE:
      p_attribute_specification(unit, tree_add_decl);
      break;

   default:
      expect(tUSE, tATTRIBUTE);
   }
}

static void p_component_configuration(tree_t unit)
{
   // for component_specification [ binding_indication ; ]
   //   [ block_configuration ] end for ;

   BEGIN("component configuration");

   consume(tFOR);

   ident_t comp_name;
   LOCAL_IDENT_LIST ids = p_component_specification(&comp_name);

   // TODO: should be optional
   tree_t bind = p_binding_indication();
   consume(tSEMI);

   const loc_t *loc = CURRENT_LOC;

   if (ids != NULL) {
      for (ident_list_t *it = ids; it != NULL; it = it->next) {
         tree_t t = tree_new(T_SPEC);
         tree_set_loc(t, loc);
         tree_set_ident(t, it->ident);
         tree_set_ident2(t, comp_name);
         tree_set_value(t, bind);

         tree_add_decl(unit, t);
      }
   }
   else {
      // Instantiation list was "others"
      tree_t t = tree_new(T_SPEC);
      tree_set_loc(t, loc);
      tree_set_ident2(t, comp_name);
      tree_set_value(t, bind);

      tree_add_decl(unit, t);
   }

   // TODO: optional block_configuration

   consume(tEND);
   consume(tFOR);
   consume(tSEMI);
}

static void p_configuration_item(tree_t unit)
{
   // block_configuration | component_configuration

   BEGIN("configuration item");

   const token_t third = peek_nth(3);
   if ((third == tCOLON) || (third == tCOMMA))
      p_component_configuration(unit);
   else {
      consume(tFOR);
      parse_error(&last_loc, "nested block configuration not supported");
      //p_block_configuration(unit);
   }
}

static void p_block_specification(tree_t unit)
{
   // label | label [ ( index_specification ) ]

   BEGIN("block specification");

   tree_set_ident2(unit, ident_prefix(tree_ident2(unit), p_identifier(), '-'));

   // TODO: [ ( index_specification ) ]
}

static void p_block_configuration(tree_t unit)
{
   // for block_specification { use_clause } { configuration_item } end for ;

   BEGIN("block configuration");

   consume(tFOR);

   p_block_specification(unit);

   while (not_at_token(tEND))
      p_configuration_item(unit);

   consume(tEND);
   consume(tFOR);
   consume(tSEMI);
}

static void p_configuration_declaration(tree_t unit)
{
   // configuration identifier of name is configuration_declarative_part
   //   block_configuration end [ configuration ] [ simple_name ] ;

   BEGIN("configuration declaration");

   consume(tCONFIGURATION);

   tree_change_kind(unit, T_CONFIG);
   tree_set_ident(unit, p_identifier());

   consume(tOF);

   tree_set_ident2(unit, p_identifier());

   consume(tIS);

   while (not_at_token(tFOR))
      p_configuration_declarative_part(unit);

   p_block_configuration(unit);

   consume(tEND);
   optional(tCONFIGURATION);
   p_trailing_label(tree_ident(unit));
   consume(tSEMI);

   tree_set_loc(unit, CURRENT_LOC);
}

static void p_context_declaration(tree_t unit)
{
   // 2008: context identifier is context_clause end [ context ]
   //       [ context_simple_name ] ;

   BEGIN("context declaration");

   consume(tCONTEXT);

   tree_change_kind(unit, T_CONTEXT);
   tree_set_ident(unit, p_identifier());

   consume(tIS);

   // LRM 08 section 13.1 forbids preceeding context clause
   if (tree_contexts(unit) != 2)     // Implicit WORK and STD
      parse_error(tree_loc(tree_context(unit, 2)), "context clause preceeding "
                  "context declaration must be empty");

   p_context_clause(unit);

   consume(tEND);
   optional(tCONTEXT);
   p_trailing_label(tree_ident(unit));
   consume(tSEMI);

   tree_set_loc(unit, CURRENT_LOC);
}

static void p_primary_unit(tree_t unit)
{
   // entity_declaration | configuration_declaration | package_declaration

   BEGIN("primary unit");

   switch (peek()) {
   case tENTITY:
      p_entity_declaration(unit);
      break;

   case tPACKAGE:
      p_package_declaration(unit);
      break;

   case tCONFIGURATION:
      p_configuration_declaration(unit);
      break;

   case tCONTEXT:
      p_context_declaration(unit);
      break;

   default:
      expect(tENTITY, tPACKAGE, tCONFIGURATION);
   }
}

static void p_block_declarative_item(tree_t parent)
{
   // subprogram_declaration | subprogram_body | type_declaration
   //   | subtype_declaration | constant_declaration | signal_declaration
   //   | shared_variable_declaration | file_declaration | alias_declaration
   //   | component_declaration | attribute_declaration
   //   | attribute_specification | configuration_specification
   //   | disconnection_specification | use_clause | group_template_declaration
   //   | group_declaration

   BEGIN("block declarative item");

   switch (peek()) {
   case tSIGNAL:
      p_signal_declaration(parent);
      break;

   case tTYPE:
      tree_add_decl(parent, p_type_declaration());
      break;

   case tSUBTYPE:
      tree_add_decl(parent, p_subtype_declaration());
      break;

   case tFILE:
      p_file_declaration(parent);
      break;

   case tCONSTANT:
      p_constant_declaration(parent);
      break;

   case tFUNCTION:
   case tPROCEDURE:
   case tIMPURE:
   case tPURE:
      {
         tree_t spec = p_subprogram_specification();
         if (peek() == tSEMI)
            tree_add_decl(parent, p_subprogram_declaration(spec));
         else
            tree_add_decl(parent, p_subprogram_body(spec));
      }
      break;

   case tALIAS:
      tree_add_decl(parent, p_alias_declaration());
      break;

   case tATTRIBUTE:
      if (peek_nth(3) == tOF)
         p_attribute_specification(parent, tree_add_decl);
      else
         tree_add_decl(parent, p_attribute_declaration());
      break;

   case tFOR:
      p_configuration_specification(parent);
      break;

   case tCOMPONENT:
      tree_add_decl(parent, p_component_declaration());
      break;

   case tUSE:
      p_use_clause(parent, tree_add_decl);
      break;

   case tSHARED:
      p_variable_declaration(parent);
      break;

   default:
      expect(tSIGNAL, tTYPE, tSUBTYPE, tFILE, tCONSTANT, tFUNCTION, tIMPURE,
             tPURE, tPROCEDURE, tALIAS, tATTRIBUTE, tFOR, tCOMPONENT, tUSE,
             tSHARED);
   }
}

static tree_t p_target(tree_t name)
{
   // name | aggregate

   BEGIN("target");

   if (name == NULL) {
      if (peek() == tLPAREN)
         return p_aggregate();
      else
         return p_name();
   }
   else
      return name;
}

static tree_t p_variable_assignment_statement(ident_t label, tree_t name)
{
   // [ label : ] target := expression ;

   EXTEND("variable assignment statement");

   tree_t t = tree_new(T_VAR_ASSIGN);

   tree_set_target(t, p_target(name));

   consume(tASSIGN);

   tree_set_value(t, p_expression());

   consume(tSEMI);

   const loc_t *loc = CURRENT_LOC;
   tree_set_loc(t, loc);

   if (label == NULL)
      label = loc_to_ident(loc);
   tree_set_ident(t, label);

   return t;
}

static tree_t p_waveform_element(void)
{
   // expression [ after expression ] | null [ after expression ]

   BEGIN("waveform element");

   tree_t w = tree_new(T_WAVEFORM);
   tree_set_value(w, p_expression());

   if (optional(tAFTER))
      tree_set_delay(w, p_expression());

   tree_set_loc(w, CURRENT_LOC);

   return w;
}

static void p_waveform(tree_t stmt)
{
   // waveform_element { , waveform_element } | unaffected

   BEGIN("waveform");

   if (optional(tUNAFFECTED))
      return;

   tree_add_waveform(stmt, p_waveform_element());

   while (optional(tCOMMA))
      tree_add_waveform(stmt, p_waveform_element());
}

static tree_t p_delay_mechanism(void)
{
   // transport | [ reject expression ] inertial

   BEGIN("delay mechanism");

   switch (peek()) {
   case tTRANSPORT:
      consume(tTRANSPORT);
      return get_time(0);

   case tREJECT:
      {
         consume(tREJECT);
         tree_t t = p_expression();
         consume(tINERTIAL);
         return t;
      }

   case tINERTIAL:
      consume(tINERTIAL);
      return NULL;

   default:
      return NULL;
   }
}

static tree_t p_signal_assignment_statement(ident_t label, tree_t name)
{
   // [ label : ] target <= [ delay_mechanism ] waveform ;

   EXTEND("signal assignment statement");

   tree_t t = tree_new(T_SIGNAL_ASSIGN);

   tree_set_target(t, p_target(name));

   consume(tLE);

   tree_t reject = p_delay_mechanism();

   p_waveform(t);

   consume(tSEMI);

   set_delay_mechanism(t, reject);
   set_label_and_loc(t, label, CURRENT_LOC);
   return t;
}

static void p_sensitivity_clause(tree_t wait)
{
   // on sensitivity_list

   BEGIN("sensitivity clause");

   consume(tON);
   p_sensitivity_list(wait);
}

static void p_condition_clause(tree_t wait)
{
   // until condition

   BEGIN("condition clause");

   consume(tUNTIL);
   tree_set_value(wait, p_expression());
}

static void p_timeout_clause(tree_t wait)
{
   // for expression

   BEGIN("timeout clause");

   consume(tFOR);
   tree_set_delay(wait, p_expression());
}

static tree_t p_wait_statement(ident_t label)
{
   // [ label : ] wait [ sensitivity_clause ] [ condition_clause ]
   //   [ timeout_clause ] ;

   EXTEND("wait statement");

   tree_t t = tree_new(T_WAIT);

   consume(tWAIT);

   if (peek() == tON)
      p_sensitivity_clause(t);

   if (peek() == tUNTIL)
      p_condition_clause(t);

   if (peek() == tFOR)
      p_timeout_clause(t);

   consume(tSEMI);

   set_label_and_loc(t, label, CURRENT_LOC);
   return t;
}

static tree_t p_assertion_statement(ident_t label)
{
   // [ label : ] assertion ;

   EXTEND("assertion statement");

   tree_t t = p_assertion();
   consume(tSEMI);

   set_label_and_loc(t, label, CURRENT_LOC);
   return t;
}

static tree_t p_report_statement(ident_t label)
{
   // [ label : ] report expression [ severity expression ] ;

   EXTEND("report statement");

   tree_t t = tree_new(T_ASSERT);

   consume(tREPORT);

   tree_set_message(t, p_expression());

   if (optional(tSEVERITY))
      tree_set_severity(t, p_expression());
   else {
      tree_t sev = tree_new(T_REF);
      tree_set_ident(sev, ident_new("NOTE"));

      tree_set_severity(t, sev);
   }

   consume(tSEMI);

   tree_add_attr_int(t, ident_new("is_report"), 1);

   set_label_and_loc(t, label, CURRENT_LOC);
   return t;
}

static tree_t p_if_statement(ident_t label)
{
   // [ label : ] if condition then sequence_of_statements
   //   { elsif condition then sequence_of_statements }
   //   [ else sequence_of_statements ] end if [ label ] ;

   EXTEND("if statement");

   tree_t t = tree_new(T_IF);

   consume(tIF);

   tree_set_value(t, p_expression());

   consume(tTHEN);

   p_sequence_of_statements(t, tree_add_stmt);

   tree_t tail = t;

   while (optional(tELSIF)) {
      tree_t elsif = tree_new(T_IF);
      tree_set_ident(elsif, ident_uniq("elsif"));
      tree_set_value(elsif, p_expression());

      consume(tTHEN);

      p_sequence_of_statements(elsif, tree_add_stmt);

      tree_set_loc(elsif, CURRENT_LOC);

      tree_add_else_stmt(tail, elsif);
      tail = elsif;
   }

   if (optional(tELSE))
      p_sequence_of_statements(tail, tree_add_else_stmt);

   consume(tEND);
   consume(tIF);
   p_trailing_label(label);
   consume(tSEMI);

   set_label_and_loc(t, label, CURRENT_LOC);
   return t;
}

static tree_t p_null_statement(ident_t label)
{
   // [ label : ] null ;

   EXTEND("null statement");

   consume(tNULL);
   consume(tSEMI);

   tree_t t = tree_new(T_NULL);
   set_label_and_loc(t, label, CURRENT_LOC);
   return t;
}

static void p_parameter_specification(tree_t loop)
{
   // identifier in discrete_range

   BEGIN("paremeter specification");

   tree_set_ident2(loop, p_identifier());

   consume(tIN);

   tree_set_range(loop, p_discrete_range());
}

static tree_t p_iteration_scheme(void)
{
   // while condition | for parameter_specification

   BEGIN("iteration scheme");

   if (optional(tWHILE)) {
      tree_t t = tree_new(T_WHILE);
      tree_set_value(t, p_expression());
      return t;
   }
   else if (optional(tFOR)) {
      tree_t t = tree_new(T_FOR);
      p_parameter_specification(t);
      return t;
   }
   else {
      tree_t true_ref = tree_new(T_REF);
      tree_set_ident(true_ref, ident_new("TRUE"));

      tree_t t = tree_new(T_WHILE);
      tree_set_value(t, true_ref);
      return t;
   }
}

static tree_t p_loop_statement(ident_t label)
{
   // [ loop_label : ] [ iteration_scheme ] loop sequence_of_statements
   //   end loop [ loop_label ] ;

   BEGIN("loop statement");

   tree_t t = p_iteration_scheme();

   consume(tLOOP);

   p_sequence_of_statements(t, tree_add_stmt);

   consume(tEND);
   consume(tLOOP);
   p_trailing_label(label);
   consume(tSEMI);

   set_label_and_loc(t, label, CURRENT_LOC);
   return t;
}

static tree_t p_return_statement(ident_t label)
{
   // [ label : ] return [ expression ] ;

   EXTEND("return statement");

   consume(tRETURN);

   tree_t t = tree_new(T_RETURN);

   if (peek() != tSEMI)
      tree_set_value(t, p_expression());

   consume(tSEMI);

   set_label_and_loc(t, label, CURRENT_LOC);
   return t;
}

static tree_t p_exit_statement(ident_t label)
{
   // [ label : ] exit [ label ] [ when condition ] ;

   EXTEND("exit statement");

   consume(tEXIT);

   tree_t t = tree_new(T_EXIT);

   if (peek() == tID)
      tree_set_ident2(t, p_identifier());

   if (optional(tWHEN))
      tree_set_value(t, p_expression());

   consume(tSEMI);

   set_label_and_loc(t, label, CURRENT_LOC);
   return t;
}

static tree_t p_next_statement(ident_t label)
{
   // [ label : ] next [ label ] [ when condition ] ;

   EXTEND("next statement");

   consume(tNEXT);

   tree_t t = tree_new(T_NEXT);

   if (peek() == tID)
      tree_set_ident2(t, p_identifier());

   if (optional(tWHEN))
      tree_set_value(t, p_expression());

   consume(tSEMI);

   set_label_and_loc(t, label, CURRENT_LOC);
   return t;
}

static tree_t p_procedure_call_statement(ident_t label, tree_t name)
{
   // [ label : ] procedure_call ;

   EXTEND("procedure call statement");

   consume(tSEMI);

   tree_change_kind(name, T_PCALL);
   tree_set_ident2(name, tree_ident(name));
   set_label_and_loc(name, label, CURRENT_LOC);
   return name;
}

static void p_case_statement_alternative(tree_t stmt)
{
   // when choices => sequence_of_statements

   BEGIN("case statement alternative");

   consume(tWHEN);

   const int nstart = tree_assocs(stmt);
   p_choices(stmt);

   consume(tASSOC);

   tree_t b = tree_new(T_BLOCK);
   tree_set_ident(b, loc_to_ident(CURRENT_LOC));
   p_sequence_of_statements(b, tree_add_stmt);

   const int nassocs = tree_assocs(stmt);
   for (int i = nstart; i < nassocs; i++)
      tree_set_value(tree_assoc(stmt, i), b);
}

static tree_t p_case_statement(ident_t label)
{
   // [ label : ] case expression is case_statement_alternative
   //   { case_statement_alternative } end case [ label ] ;

   EXTEND("case statement");

   consume(tCASE);

   tree_t t = tree_new(T_CASE);
   tree_set_value(t, p_expression());

   consume(tIS);

   do {
      p_case_statement_alternative(t);
   } while (peek() == tWHEN);

   consume(tEND);
   consume(tCASE);
   p_trailing_label(label);
   consume(tSEMI);

   set_label_and_loc(t, label, CURRENT_LOC);
   return t;
}

static tree_t p_sequential_statement(void)
{
   // wait_statement | assertion_statement | report_statement
   //   | signal_assignment_statement | variable_assignment_statement
   //   | procedure_call_statement | if_statement | case_statement
   //   | loop_statement | next_statement | exit_statement | return_statement
   //   | null_statement

   BEGIN("sequential statement");

   ident_t label = NULL;
   if ((peek() == tID) && (peek_nth(2) == tCOLON)) {
      label = p_identifier();
      consume(tCOLON);
   }

   switch (peek()) {
   case tWAIT:
      return p_wait_statement(label);

   case tASSERT:
      return p_assertion_statement(label);

   case tREPORT:
      return p_report_statement(label);

   case tIF:
      return p_if_statement(label);

   case tNULL:
      return p_null_statement(label);

   case tRETURN:
      return p_return_statement(label);

   case tCASE:
      return p_case_statement(label);

   case tWHILE:
   case tLOOP:
   case tFOR:
      return p_loop_statement(label);

   case tEXIT:
      return p_exit_statement(label);

   case tNEXT:
      return p_next_statement(label);

   case tID:
      break;

   case tLPAREN:
      {
         tree_t agg = p_aggregate();

         switch (peek()) {
         case tASSIGN:
            return p_variable_assignment_statement(label, agg);

         case tLE:
            return p_signal_assignment_statement(label, agg);

         default:
            expect(tASSIGN, tLE);
            return tree_new(T_NULL);
         }
      }

   default:
      expect(tWAIT, tID);
      drop_tokens_until(tSEMI);
      return tree_new(T_NULL);
   }

   tree_t name = p_name();

   switch (peek()) {
   case tASSIGN:
      return p_variable_assignment_statement(label, name);

   case tLE:
      return p_signal_assignment_statement(label, name);

   case tSEMI:
      return p_procedure_call_statement(label, name);

   default:
      expect(tASSIGN, tLE, tSEMI);
      drop_tokens_until(tSEMI);
      return tree_new(T_NULL);
   }
}

static tree_t p_instantiated_unit(void)
{
   // [ component ] name
   //   | entity name [ ( identifier ) ]
   //   | configuration name

   BEGIN("instantiated unit");

   tree_t t = tree_new(T_INSTANCE);

   switch (peek()) {
   case tENTITY:
      consume(tENTITY);
      tree_set_class(t, C_ENTITY);
      break;

   case tCONFIGURATION:
      consume(tCONFIGURATION);
      tree_set_class(t, C_CONFIGURATION);
      break;

   case tCOMPONENT:
      consume(tCOMPONENT);
      // Fall-through

   default:
      tree_set_class(t, C_COMPONENT);
   }

   tree_set_ident2(t, p_selected_identifier());

   if ((tree_class(t) == C_ENTITY) && optional(tLPAREN)) {
      tree_set_ident2(t, ident_prefix(tree_ident2(t), p_identifier(), '-'));
      consume(tRPAREN);
   }

   return t;
}

static tree_t p_component_instantiation_statement(ident_t label)
{
   // label : instantiated_unit [ generic_map_aspect ] [ port_map_aspect ] ;

   EXTEND("component instantiation statement");

   tree_t t = p_instantiated_unit();
   tree_set_ident(t, label);

   if (peek() == tGENERIC)
      p_generic_map_aspect(t);

   if (peek() == tPORT)
      p_port_map_aspect(t);

   consume(tSEMI);

   if (label == NULL)
      parse_error(CURRENT_LOC, "component instantiation statement must "
                  "have a label");

   tree_set_loc(t, CURRENT_LOC);
   return t;
}

static tree_t p_options(tree_t stmt)
{
   // [ guarded ] [ delay_mechanism ]

   BEGIN("options");

   if (optional(tGUARDED))
      tree_set_flag(stmt, TREE_F_GUARDED);

   return p_delay_mechanism();
}

static void p_conditional_waveforms(tree_t stmt)
{
   // { waveform when condition else } waveform [ when condition ]

   BEGIN("conditional waveforms");

   for (;;) {
      tree_t c = tree_new(T_COND);
      p_waveform(c);
      tree_set_loc(c, CURRENT_LOC);

      tree_add_cond(stmt, c);

      if (optional(tWHEN)) {
         tree_set_value(c, p_expression());

         if (!optional(tELSE))
            break;
      }
      else
         break;
   }
}

static tree_t p_conditional_signal_assignment(void)
{
   // target <= options conditional_waveforms ;

   BEGIN("conditional signal assignment");

   tree_t t = tree_new(T_CASSIGN);
   tree_set_target(t, p_target(NULL));

   consume(tLE);

   tree_t reject = p_options(t);
   p_conditional_waveforms(t);

   const int nconds = tree_conds(t);
   for (int i = 0; i < nconds; i++)
      set_delay_mechanism(tree_cond(t, i), reject);

   consume(tSEMI);

   tree_set_loc(t, CURRENT_LOC);
   return t;
}

static void p_selected_waveforms(tree_t stmt)
{
   // { waveform when choices , } waveform when choices

   BEGIN("selected waveforms");

   do {
      tree_t a = tree_new(T_SIGNAL_ASSIGN);
      p_waveform(a);

      consume(tWHEN);

      const int nstart = tree_assocs(stmt);
      p_choices(stmt);

      const int nassocs = tree_assocs(stmt);
      for (int i = nstart; i < nassocs; i++)
         tree_set_value(tree_assoc(stmt, i), a);

      tree_set_loc(a, CURRENT_LOC);
   } while (optional(tCOMMA));
}

static tree_t p_selected_signal_assignment(void)
{
   // with expression select target <= options selected_waveforms ;

   BEGIN("selected signal assignment");

   consume(tWITH);

   tree_t t = tree_new(T_SELECT);
   tree_set_value(t, p_expression());

   consume(tSELECT);
   tree_t target = p_target(NULL);
   consume(tLE);
   tree_t reject = p_options(t);
   p_selected_waveforms(t);
   consume(tSEMI);

   const int nassocs = tree_assocs(t);
   for (int i = 0; i < nassocs; i++) {
      tree_t s = tree_value(tree_assoc(t, i));
      tree_set_target(s, target);
      set_delay_mechanism(s, reject);
   }

   tree_set_loc(t, CURRENT_LOC);
   return t;
}

static tree_t p_concurrent_signal_assignment_statement(ident_t label)
{
   // [ label : ] [ postponed ] conditional_signal_assignment
   //   | [ label : ] [ postponed ] selected_signal_assignment

   EXTEND("concurrent signal assignment statement");

   const bool postponed = optional(tPOSTPONED);

   tree_t t = (peek() == tWITH)
      ? p_selected_signal_assignment()
      : p_conditional_signal_assignment();

   const loc_t *loc = CURRENT_LOC;
   tree_set_loc(t, loc);

   if (label == NULL)
      label = loc_to_ident(loc);
   tree_set_ident(t, label);

   if (postponed)
      tree_set_flag(t, TREE_F_POSTPONED);

   tree_set_loc(t, CURRENT_LOC);
   return t;
}

static tree_t p_concurrent_procedure_call_statement(ident_t label)
{
   // [ label : ] [ postponed ] procedure_call ;

   EXTEND("concurrent procedure call statement");

   const bool postponed = optional(tPOSTPONED);

   tree_t t = p_name();

   const tree_kind_t kind = tree_kind(t);
   if ((kind != T_REF) && (kind != T_FCALL))
      assert(false);  // XXX: FIXME

   tree_change_kind(t, T_CPCALL);
   tree_set_ident2(t, tree_ident(t));

   consume(tSEMI);

   if (postponed)
      tree_set_flag(t, TREE_F_POSTPONED);

   const loc_t *loc = CURRENT_LOC;
   tree_set_loc(t, loc);

   if (label == NULL)
      label = loc_to_ident(loc);
   tree_set_ident(t, label);

   return t;
}

static void p_block_statement_part(tree_t arch)
{
   // { concurrent_statement }

   BEGIN("block statement part");

   while (not_at_token(tEND))
      tree_add_stmt(arch, p_concurrent_statement());
}

static void p_block_declarative_part(tree_t arch)
{
   // { block_declarative_item }

   BEGIN("block declarative part");

   while (not_at_token(tBEGIN))
      p_block_declarative_item(arch);
}

static tree_t p_block_statement(ident_t label)
{
   // label : block [ ( expression ) ] [ is ] block_header
   //   block_declarative_part begin block_statement_part end block [ label ] ;

   EXTEND("block statement");

   tree_t b = tree_new(T_BLOCK);
   tree_set_ident(b, label);

   consume(tBLOCK);
   optional(tIS);
   p_block_declarative_part(b);
   consume(tBEGIN);
   p_block_statement_part(b);
   consume(tEND);
   consume(tBLOCK);
   p_trailing_label(label);
   consume(tSEMI);

   if (label == NULL)
      parse_error(CURRENT_LOC, "block statement must have a label");

   tree_set_loc(b, CURRENT_LOC);
   return b;
}

static tree_t p_generation_scheme(void)
{
   // for parameter_specification | if condition

   BEGIN("generation scheme");

   switch (one_of(tIF, tFOR)) {
   case tIF:
      {
         tree_t g = tree_new(T_IF_GENERATE);
         tree_set_value(g, p_expression());
         return g;
      }

   case tFOR:
      {
         tree_t g = tree_new(T_FOR_GENERATE);
         p_parameter_specification(g);
         return g;
      }

   default:
      return tree_new(T_IF_GENERATE);
   }
}

static tree_t p_generate_statement(ident_t label)
{
   // label : generation_scheme generate [ { block_declarative_item }
   //   begin ] { concurrent_statement } end generate [ label ] ;

   EXTEND("generate statement");

   tree_t g = p_generation_scheme();
   tree_set_ident(g, label);

   consume(tGENERATE);

   if (scan(tSIGNAL, tTYPE, tSUBTYPE, tFILE, tCONSTANT, tFUNCTION, tIMPURE,
            tPURE, tALIAS, tATTRIBUTE, tBEGIN, tPROCEDURE, tFOR, tCOMPONENT,
            tUSE, tSHARED)) {
      while (not_at_token(tBEGIN))
         p_block_declarative_item(g);
      consume(tBEGIN);
   }

   while (not_at_token(tEND))
      tree_add_stmt(g, p_concurrent_statement());

   consume(tEND);
   consume(tGENERATE);
   p_trailing_label(label);
   consume(tSEMI);

   if (label == NULL)
      parse_error(CURRENT_LOC, "generate statement must have a label");

   tree_set_loc(g, CURRENT_LOC);
   return g;
}

static tree_t p_concurrent_statement(void)
{
   // block_statement | process_statement | concurrent_procedure_call_statement
   //   | concurrent_assertion_statement
   //   | concurrent_signal_assignment_statement
   //   | component_instantiation_statement | generate_statement

   BEGIN("concurrent statement");

   ident_t label = NULL;
   if ((peek() == tID) && (peek_nth(2) == tCOLON)) {
      label = p_identifier();
      consume(tCOLON);
   }

   if (peek() == tID) {
      const token_t p2 = peek_nth(2);
      if (((label != NULL) && (p2 == tSEMI))
          || (p2 == tGENERIC) || (p2 == tPORT))
         return p_component_instantiation_statement(label);
      else {
         const look_params_t lookp = {
            .look     = { tLE },
            .stop     = { tSEMI },
            .nest_in  = tLPAREN,
            .nest_out = tRPAREN,
            .depth    = 0
         };

         if (look_for(&lookp))
            return p_concurrent_signal_assignment_statement(label);
         else
            return p_concurrent_procedure_call_statement(label);
      }
   }
   else {
      switch (peek()) {
      case tPROCESS:
         return p_process_statement(label);

      case tCOMPONENT:
      case tENTITY:
      case tCONFIGURATION:
         return p_component_instantiation_statement(label);

      case tWITH:
         return p_concurrent_signal_assignment_statement(label);

      case tASSERT:
         return p_concurrent_assertion_statement(label);

      case tPOSTPONED:
         if (peek_nth(2) == tASSERT)
            return p_concurrent_assertion_statement(label);
         else
            return p_process_statement(label);

      case tBLOCK:
         return p_block_statement(label);

      case tIF:
      case tFOR:
         return p_generate_statement(label);

      case tLPAREN:
         return p_concurrent_signal_assignment_statement(label);

      default:
         expect(tPROCESS, tPOSTPONED, tCOMPONENT, tENTITY, tCONFIGURATION,
                tWITH, tASSERT, tBLOCK, tIF, tFOR);
         drop_tokens_until(tSEMI);
         return tree_new(T_BLOCK);
      }
   }
}

static void p_architecture_declarative_part(tree_t arch)
{
   // { block_declarative_item }

   BEGIN("architecture declarative part");

   while (not_at_token(tBEGIN))
      p_block_declarative_item(arch);
}

static void p_architecture_statement_part(tree_t arch)
{
   // { concurrent_statement }

   BEGIN("architecture statement part");

   while (not_at_token(tEND))
      tree_add_stmt(arch, p_concurrent_statement());
}

static void p_architecture_body(tree_t unit)
{
   // architecture identifier of entity_name is architecture_declarative_part
   //   begin architecture_statement_part end [ architecture ]
   //   [ architecture_simple_name ] ;

   BEGIN("architecture body");

   tree_change_kind(unit, T_ARCH);

   consume(tARCHITECTURE);
   tree_set_ident(unit, p_identifier());
   consume(tOF);
   tree_set_ident2(unit, p_identifier());
   consume(tIS);

   p_architecture_declarative_part(unit);

   consume(tBEGIN);

   p_architecture_statement_part(unit);

   consume(tEND);
   optional(tARCHITECTURE);
   p_trailing_label(tree_ident(unit));
   consume(tSEMI);

   tree_set_loc(unit, CURRENT_LOC);
}

static void p_package_body_declarative_item(tree_t parent)
{
   // subprogram_declaration | subprogram_body | type_declaration
   //   | subtype_declaration | constant_declaration
   //   | shared_variable_declaration | file_declaration | alias_declaration
   //   | use_clause | group_template_declaration | group_declaration
   //
   // 2008: subprogram_instantiation_declaration | attribute_declaration
   //         | attribute_specification | package_instantiation_declaration

   BEGIN("package body declarative item");

   switch (peek()) {
   case tFUNCTION:
   case tPROCEDURE:
   case tIMPURE:
   case tPURE:
      {
         tree_t spec = p_subprogram_specification();
         if (peek() == tSEMI)
            tree_add_decl(parent, p_subprogram_declaration(spec));
         else
            tree_add_decl(parent, p_subprogram_body(spec));
      }
      break;

   case tSHARED:
      p_variable_declaration(parent);
      break;

   case tFILE:
      p_file_declaration(parent);
      break;

   case tATTRIBUTE:
      if (standard() < STD_08)
         parse_error(CURRENT_LOC, "package body may not contain attribute "
                     "declarations or specifications in VHDL-%s",
                     standard_text(standard()));
      else {
         if (peek_nth(3) == tOF)
            p_attribute_specification(parent, tree_add_decl);
         else
            tree_add_decl(parent, p_attribute_declaration());
      }
      break;

   case tTYPE:
      tree_add_decl(parent, p_type_declaration());
      break;

   case tCONSTANT:
      p_constant_declaration(parent);
      break;

   case tSUBTYPE:
      tree_add_decl(parent, p_subtype_declaration());
      break;

   case tALIAS:
      tree_add_decl(parent, p_alias_declaration());
      break;

   case tUSE:
      p_use_clause(parent, tree_add_decl);
      break;

   default:
      expect(tFUNCTION, tPROCEDURE, tSHARED, tIMPURE, tPURE, tATTRIBUTE, tTYPE,
             tCONSTANT, tSUBTYPE, tFILE, tALIAS, tUSE);
   }
}

static void p_package_body_declarative_part(tree_t unit)
{
   // { package_body_declarative_item }

   BEGIN("package body declarative part");

   while (not_at_token(tEND))
      p_package_body_declarative_item(unit);
}

static void p_package_body(tree_t unit)
{
   // package body simple_name is package_body_declarative_part
   //   end [ package body ] [ simple_name ] ;

   BEGIN("package body");

   consume(tPACKAGE);
   consume(tBODY);

   tree_change_kind(unit, T_PACK_BODY);
   tree_set_ident(unit, p_identifier());

   consume(tIS);

   p_package_body_declarative_part(unit);

   consume(tEND);

   if (optional(tPACKAGE))
      consume(tBODY);

   p_trailing_label(tree_ident(unit));
   consume(tSEMI);

   tree_set_loc(unit, CURRENT_LOC);
}

static void p_secondary_unit(tree_t unit)
{
   // architecture_body | package_body

   BEGIN("secondary unit");

   switch (peek()) {
   case tARCHITECTURE:
      p_architecture_body(unit);
      break;

   case tPACKAGE:
      p_package_body(unit);
      break;

   default:
      expect(tARCHITECTURE, tPACKAGE);
   }
}

static void p_library_unit(tree_t unit)
{
   // primary_unit | secondary_unit

   BEGIN("library unit");

   switch (peek()) {
   case tENTITY:
   case tCONFIGURATION:
   case tCONTEXT:
      p_primary_unit(unit);
      break;

   case tARCHITECTURE:
      p_secondary_unit(unit);
      break;

   case tPACKAGE:
      if (peek_nth(2) == tBODY)
         p_secondary_unit(unit);
      else
         p_primary_unit(unit);
      break;

   default:
      expect(tENTITY, tCONFIGURATION, tARCHITECTURE, tPACKAGE, tCONTEXT);
   }
}

static tree_t p_design_unit(void)
{
   BEGIN("design unit");

   tree_t unit = tree_new(T_DESIGN_UNIT);

   tree_t std = tree_new(T_LIBRARY);
   tree_set_ident(std, std_i);
   tree_add_context(unit, std);

   tree_t work = tree_new(T_LIBRARY);
   tree_set_ident(work, lib_name(lib_work()));
   tree_add_context(unit, work);

   p_context_clause(unit);
   p_library_unit(unit);

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

   if (file_sz > 0)
      file_start = map_file(fd, file_sz);
   else
      file_start = NULL;

   read_ptr           = file_start;
   last_was_newline   = true;
   perm_file_name     = ident_new(file);
   n_row              = 0;
   n_token_next_start = 0;

   if (tokenq == NULL) {
      tokenq_sz = 128;
      tokenq = xmalloc(tokenq_sz * sizeof(tokenq_t));
   }

   tokenq_head = tokenq_tail = 0;
}

tree_t parse(void)
{
   int old_errors = n_errors;
   n_correct = RECOVER_THRESH;

   if (peek() == tEOF)
      return NULL;

   tree_t unit = p_design_unit();
   if (n_errors > old_errors)
      return NULL;
   else
      return unit;
}

int parse_errors(void)
{
   return n_errors;
}

void reset_parse_errors(void)
{
   n_errors = 0;
}
