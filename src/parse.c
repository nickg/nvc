//
//  Copyright (C) 2014-2021  Nick Gasson
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
#include "loc.h"
#include "hash.h"
#include "names.h"

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

static loc_file_ref_t file_ref = FILE_INVALID;
static int            n_token_next_start = 0;
static int            n_row = 0;
static bool           last_was_newline = true;
static loc_t          start_loc;
static loc_t          last_loc;
static const char    *read_ptr;
static const char    *file_start;
static size_t         file_sz;
static const char    *hint_str = NULL;
static int            n_correct = 0;
static tokenq_t      *tokenq;
static int            tokenq_sz;
static int            tokenq_head;
static int            tokenq_tail;
static yylval_t       last_lval;
static token_t        opt_hist[8];
static int            nopt_hist = 0;
static cond_state_t  *cond_state = NULL;
static bool           translate_on = true;
static bool           parse_pragmas = false;
static nametab_t     *nametab = NULL;
static bool           bootstrapping = false;

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
static tree_t p_block_configuration(void);
static tree_t p_protected_type_body(ident_t id);
static bool p_cond_analysis_expr(void);
static type_t p_signature(void);
static type_t p_type_mark(ident_t name);

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
      "`error", "`warning", "translate_off", "translate_on", "pragma"
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

         new->loc.column_delta =
            yylloc.first_column + yylloc.column_delta - new->loc.first_column;
         new->loc.line_delta =
            yylloc.first_line + yylloc.line_delta - new->loc.first_line;

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
               loc.column_delta =
                  yylloc.first_column + yylloc.column_delta - loc.first_column;
               loc.line_delta =
                  yylloc.first_line + yylloc.line_delta - loc.first_line;

               if (token == tCONDWARN)
                  warn_at(&loc, "%s", last_lval.s);
               else
                  parse_error(&loc, "%s", last_lval.s);

               free(last_lval.s);
            }
         }

         return conditional_yylex();
      }

   case tSYNTHOFF:
      {
         BEGIN("synthesis translate_off");

         if (opt_get_int("synthesis"))
            translate_on = false;

         return conditional_yylex();
      }

   case tSYNTHON:
      {
         BEGIN("synthesis translate_off");

         translate_on = true;
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
      if (translate_on && (cond_state == NULL || cond_state->result)) {
         if (token == tPRAGMA && !parse_pragmas)
            return conditional_yylex();
         else
            return token;
      }
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

   if (start_loc.first_line == LINE_INVALID)
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
   } while ((tok != next) && (next != tEOF));

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

   result = get_loc(start->first_line,
                    start->first_column,
                    end->first_line + end->line_delta,
                    end->first_column + end->column_delta,
                    start->file_ref);
   return &result;
}

static tree_t find_unit(const loc_t *where, ident_t name)
{
   ident_t lname = ident_until(name, '.');
   lib_t lib = lib_loaded(lname);
   if (lib != NULL) {
      tree_t unit = lib_get_check_stale(lib, name);
      if (unit == NULL)
         parse_error(where, "cannot find unit %s", istr(name));

      return unit;
   }
   else {
      parse_error(where, "missing library clause for %s", istr(lname));
      return NULL;
   }
}

tree_t find_binding(tree_t inst)
{
   if (inst == NULL)
      return NULL;

   ident_t name =
      tree_kind(inst) == T_BINDING ? tree_ident(inst) : tree_ident2(inst);

   tree_t ref = query_name(nametab, name);
   if (ref != NULL) {
      if (tree_kind(ref) != T_COMPONENT) {
         parse_error(tree_loc(inst), "object %s is not a component declaration",
                     istr(name));
         return NULL;
      }
   }
   else {
      ident_t ename = ident_until(name, '-');

      ref = find_unit(tree_loc(inst), ename);
      if (ref != NULL) {
         tree_kind_t kind = tree_kind(ref);
         if (kind != T_ENTITY && kind != T_CONFIGURATION) {
            parse_error(tree_loc(inst), "unit %s cannot be instantiated",
                        istr(name));
            ref = NULL;
         }
         else if (kind == T_CONFIGURATION) {
            ref = NULL;  // TODO
         }
      }
   }

   return ref;
}

static ident_t loc_to_ident(const loc_t *loc)
{
   char sbuf[64];
   checked_sprintf(sbuf, sizeof(sbuf), "line_%d", loc->first_line);

   if (!ident_interned(sbuf))
      return ident_new(sbuf);

   LOCAL_TEXT_BUF tb = tb_new();
   tb_printf(tb, "%sa", sbuf);

   for (int i = 1; ident_interned(tb_get(tb)); i++) {
      if (i % 26 != 0)
         tb_backup(tb, 1);
      tb_append(tb, 'a' + i % 26);
   }

   return ident_new(tb_get(tb));
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

static tree_t get_time(int64_t fs, const loc_t *loc)
{
   tree_t lit = tree_new(T_LITERAL);
   tree_set_subkind(lit, L_INT);
   tree_set_ival(lit, fs);
   tree_set_loc(lit, loc);
   tree_set_type(lit, std_type(find_std(nametab), "TIME"));

   return lit;
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
   else {
      tree_set_reject(t, reject);
      solve_types(nametab, reject, std_type(find_std(nametab), "TIME"));
   }
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

static tree_t add_port(tree_t d, type_t type, port_mode_t mode, tree_t def)
{
   type_t ftype = tree_type(d);

   char *argname LOCAL = xasprintf("_arg%d", type_params(ftype));
   tree_t port = tree_new(T_PORT_DECL);
   tree_set_ident(port, ident_new(argname));
   tree_set_loc(port, tree_loc(d));
   tree_set_type(port, type);
   tree_set_subkind(port, mode);
   if (def != NULL)
      tree_set_value(port, def);
   if (type_is_file(type))
      tree_set_class(port, C_FILE);

   tree_add_port(d, port);
   type_add_param(ftype, type);

   return port;
}

static tree_t builtin_proc(ident_t name, const char *builtin, ...)
{
   type_t f = type_new(T_PROC);
   type_set_ident(f, name);

   tree_t d = tree_new(T_PROC_DECL);
   tree_set_ident(d, name);
   tree_set_type(d, f);
   tree_add_attr_str(d, builtin_i, ident_new(builtin));
   tree_add_attr_int(d, wait_level_i, WAITS_NO);

   tree_set_flag(d, TREE_F_PREDEFINED);
   tree_set_loc(d, CURRENT_LOC);
   return d;
}

static tree_t builtin_fn(ident_t name, type_t result, const char *builtin, ...)
{
   type_t f = type_new(T_FUNC);
   type_set_ident(f, name);
   type_set_result(f, result);

   tree_t d = tree_new(T_FUNC_DECL);
   tree_set_ident(d, name);
   tree_set_type(d, f);
   tree_add_attr_str(d, builtin_i, ident_new(builtin));

   va_list ap;
   va_start(ap, builtin);
   type_t arg;
   while ((arg = va_arg(ap, type_t)))
      add_port(d, arg, PORT_IN, NULL);
   va_end(ap);

   tree_set_flag(d, TREE_F_PREDEFINED);
   tree_set_loc(d, CURRENT_LOC);
   return d;
}

static void declare_binary(tree_t container, ident_t name, type_t lhs,
                           type_t rhs, type_t result, const char *builtin)
{
   tree_t d = builtin_fn(name, result, builtin, lhs, rhs, NULL);
   insert_name(nametab, d, NULL, 0);
   tree_add_decl(container, d);
}

static void declare_unary(tree_t container, ident_t name, type_t operand,
                          type_t result, const char *builtin)
{
   tree_t d = builtin_fn(name, result, builtin, operand, NULL);
   insert_name(nametab, d, NULL, 0);
   tree_add_decl(container, d);
}

static void declare_predefined_ops(tree_t container, type_t t)
{
   // Prefined operators are defined in LRM 93 section 7.2

   if (type_kind(t) == T_CARRAY) {
      // Construct an unconstrained array type for parameters
      type_t u = type_new(T_UARRAY);
      type_set_ident(u, type_ident(t));
      type_set_elem(u, type_elem(t));

      const int ndims = type_dims(t);
      for (int i = 0; i < ndims; i++)
         type_add_index_constr(u, tree_type(type_dim(t, i).left));

      t = u;
   }

   ident_t mult  = ident_new("\"*\"");
   ident_t div   = ident_new("\"/\"");
   ident_t plus  = ident_new("\"+\"");
   ident_t minus = ident_new("\"-\"");

   // Predefined operators

   tree_t std = find_std(nametab);

   type_t std_bool = std_type(std, "BOOLEAN");
   type_t std_int  = NULL;
   type_t std_real = NULL;

   const bool universal = (bootstrapping && type_is_universal(t));

   type_kind_t kind = type_kind(t);

   switch (kind) {
   case T_SUBTYPE:
      // Use operators of base type
      break;

   case T_CARRAY:
   case T_UARRAY:
      // Operators on arrays
      declare_binary(container, ident_new("\"=\""), t, t, std_bool, "aeq");
      declare_binary(container, ident_new("\"/=\""), t, t, std_bool, "aneq");
      if (dimension_of(t) == 1) {
         declare_binary(container, ident_new("\"<\""), t, t, std_bool, "alt");
         declare_binary(container, ident_new("\"<=\""), t, t, std_bool, "aleq");
         declare_binary(container, ident_new("\">\""), t, t, std_bool, "agt");
         declare_binary(container, ident_new("\">=\""), t, t, std_bool, "ageq");

         type_t elem = type_elem(t);
         ident_t concat = ident_new("\"&\"");
         declare_binary(container, concat, t, t, t, "concat");
         declare_binary(container, concat, t, elem, t, "concat");
         declare_binary(container, concat, elem, t, t, "concat");
         declare_binary(container, concat, elem, elem, t, "concat");
      }
      break;

   case T_RECORD:
      // Operators on records
      declare_binary(container, ident_new("\"=\""), t, t, std_bool, "req");
      declare_binary(container, ident_new("\"/=\""), t, t, std_bool, "rneq");
      break;

   case T_PHYSICAL:
      std_int  = std_type(std, "INTEGER");
      std_real = std_type(std, "REAL");

      // Multiplication
      declare_binary(container, mult, t, std_int, t, "mul");
      declare_binary(container, mult, t, std_real, t, "mulpr");
      declare_binary(container, mult, std_int, t, t, "mul");
      declare_binary(container, mult, std_real, t, t, "mulrp");

      // Division
      declare_binary(container, div, t, std_int, t, "div");
      declare_binary(container, div, t, std_real, t, "divpr");
      declare_binary(container, div, t, t, std_int, "div");

      // Addition
      declare_binary(container, plus, t, t, t, "add");

      // Subtraction
      declare_binary(container, minus, t, t, t, "sub");

      // Sign operators
      declare_unary(container, plus, t, t, "identity");
      declare_unary(container, minus, t, t, "neg");

      // Comparison
      declare_binary(container, ident_new("\"<\""), t, t, std_bool, "lt");
      declare_binary(container, ident_new("\"<=\""), t, t, std_bool, "leq");
      declare_binary(container, ident_new("\">\""), t, t, std_bool, "gt");
      declare_binary(container, ident_new("\">=\""), t, t, std_bool, "geq");

      // Equality
      declare_binary(container, ident_new("\"=\""), t, t, std_bool, "eq");
      declare_binary(container, ident_new("\"/=\""), t, t, std_bool, "neq");

      // Absolute value
      declare_unary(container, ident_new("\"abs\""), t, t, "abs");

      break;

   case T_INTEGER:
      // Modulus
      declare_binary(container, ident_new("\"mod\""), t, t, t, "mod");

      // Remainder
      declare_binary(container, ident_new("\"rem\""), t, t, t, "rem");

      // Fall-through
   case T_REAL:
      // Addition
      declare_binary(container, plus, t, t, t, "add");

      // Subtraction
      declare_binary(container, minus, t, t, t, "sub");

      // Multiplication
      declare_binary(container, mult, t, t, t, "mul");

      // Division
      declare_binary(container, div, t, t, t, "div");

      // Sign operators
      declare_unary(container, plus, t, t, "identity");
      declare_unary(container, minus, t, t, "neg");

      // Exponentiation
      if (!universal) {
         std_int = std_type(std, "INTEGER");
         declare_binary(container, ident_new("\"**\""), t, std_int, t, "exp");
      }

      // Absolute value
      declare_unary(container, ident_new("\"abs\""), t, t, "abs");

      // Fall-through
   case T_ENUM:
      declare_binary(container, ident_new("\"<\""), t, t, std_bool, "lt");
      declare_binary(container, ident_new("\"<=\""), t, t, std_bool, "leq");
      declare_binary(container, ident_new("\">\""), t, t, std_bool, "gt");
      declare_binary(container, ident_new("\">=\""), t, t, std_bool, "geq");

      // Fall-through
   default:
      declare_binary(container, ident_new("\"=\""), t, t, std_bool, "eq");
      declare_binary(container, ident_new("\"/=\""), t, t, std_bool, "neq");

      break;
   }

   // Universal integers and reals have some additional overloaded operators
   // that are not valid for regular integer and real types
   // See LRM 93 section 7.5

   if (universal && t == type_universal_real()) {
      type_t uint  = type_universal_int();
      type_t ureal = type_universal_real();

      ident_t mult = ident_new("\"*\"");
      ident_t div  = ident_new("\"/\"");

      declare_binary(container, mult, ureal, uint, ureal, "mulri");
      declare_binary(container, mult, uint, ureal, ureal, "mulir");
      declare_binary(container, div, ureal, uint, ureal, "divri");
   }

   // Logical operators

   if (bootstrapping && (t == std_bool || t == std_type(std, "BIT"))) {
      declare_binary(container, ident_new("\"and\""), t, t, t, "and");
      declare_binary(container, ident_new("\"or\""), t, t, t, "or");
      declare_binary(container, ident_new("\"xor\""), t, t, t, "xor");
      declare_binary(container, ident_new("\"nand\""), t, t, t, "nand");
      declare_binary(container, ident_new("\"nor\""), t, t, t, "nor");
      declare_binary(container, ident_new("\"xnor\""), t, t, t, "xnor");
      declare_unary(container, ident_new("\"not\""), t, t, "not");
   }

   bool vec_logical = false;
   if (kind == T_CARRAY || kind == T_UARRAY) {
      type_t base = type_elem(t);
      vec_logical = (base == std_bool || base == std_type(std, "BIT"));
   }

   if (vec_logical) {
      std_int = std_type(std, "INTEGER");

      declare_binary(container, ident_new("\"and\""), t, t, t, "v_and");
      declare_binary(container, ident_new("\"or\""), t, t, t, "v_or");
      declare_binary(container, ident_new("\"xor\""), t, t, t, "v_xor");
      declare_binary(container, ident_new("\"nand\""), t, t, t, "v_nand");
      declare_binary(container, ident_new("\"nor\""), t, t, t, "v_nor");
      declare_binary(container, ident_new("\"xnor\""), t, t, t, "v_xnor");
      declare_unary(container, ident_new("\"not\""), t, t, "v_not");

      declare_binary(container, ident_new("\"sll\""), t, std_int, t, "sll");
      declare_binary(container, ident_new("\"srl\""), t, std_int, t, "srl");
      declare_binary(container, ident_new("\"sla\""), t, std_int, t, "sla");
      declare_binary(container, ident_new("\"sra\""), t, std_int, t, "sra");
      declare_binary(container, ident_new("\"rol\""), t, std_int, t, "rol");
      declare_binary(container, ident_new("\"ror\""), t, std_int, t, "ror");
   }

   // Predefined procedures

   switch (kind) {
   case T_FILE:
      {
         tree_t read_mode = search_decls(std, ident_new("READ_MODE"), 0);
         assert(read_mode != NULL);

         ident_t file_open_i  = ident_new("FILE_OPEN");
         ident_t file_close_i = ident_new("FILE_CLOSE");
         ident_t read_i       = ident_new("READ");
         ident_t write_i      = ident_new("WRITE");
         ident_t endfile_i    = ident_new("ENDFILE");

         type_t open_kind   = std_type(std, "FILE_OPEN_KIND");
         type_t open_status = std_type(std, "FILE_OPEN_STATUS");
         type_t std_string  = std_type(std, "STRING");

         tree_t file_open1 = builtin_proc(file_open_i, "file_open1");
         add_port(file_open1, t, PORT_INOUT, NULL);
         add_port(file_open1, std_string, PORT_IN, NULL);
         add_port(file_open1, open_kind, PORT_IN, make_ref(read_mode));
         insert_name(nametab, file_open1, file_open_i, 0);
         tree_add_decl(container, file_open1);

         tree_t file_open2 = builtin_proc(file_open_i, "file_open2");
         add_port(file_open2, open_status, PORT_OUT, NULL);
         add_port(file_open2, t, PORT_INOUT, NULL);
         add_port(file_open2, std_string, PORT_IN, NULL);
         add_port(file_open2, open_kind, PORT_IN, make_ref(read_mode));
         insert_name(nametab, file_open2, file_open_i, 0);
         tree_add_decl(container, file_open2);

         tree_t file_close = builtin_proc(file_close_i, "file_close");
         add_port(file_close, t, PORT_INOUT, NULL);
         insert_name(nametab, file_close, file_close_i, 0);
         tree_add_decl(container, file_close);

         type_t of = type_file(t);

         tree_t read = builtin_proc(read_i, "file_read");
         add_port(read, t, PORT_INOUT, NULL);
         add_port(read, of, PORT_OUT, NULL);
         if (type_is_array(of) && type_is_unconstrained(of))
            add_port(read, std_type(std, "INTEGER"), PORT_OUT, NULL);
         insert_name(nametab, read, read_i, 0);
         tree_add_decl(container, read);

         tree_t write = builtin_proc(write_i, "file_write");
         add_port(write, t, PORT_INOUT, NULL);
         add_port(write, of, PORT_IN, NULL);
         insert_name(nametab, write, write_i, 0);
         tree_add_decl(container, write);

         declare_unary(container, endfile_i, t, std_bool, "endfile");
      }
      break;

   case T_ACCESS:
      {
         ident_t deallocate_i = ident_new("DEALLOCATE");

         tree_t deallocate = builtin_proc(deallocate_i, "deallocate");
         add_port(deallocate, t, PORT_INOUT, NULL);
         insert_name(nametab, deallocate, deallocate_i, 0);
         tree_add_decl(container, deallocate);
      }
      break;

   default:
      break;
   }
}

static void skip_selected_name(void)
{
   // Skip to the end of a selected name to avoid cascading errors
   while (peek() == tDOT) {
      consume(tDOT);
      consume(tID);
      free(last_lval.s);
   }
}

static void unary_op(tree_t expr, tree_t (*arg_fn)(void))
{
   tree_t right = (*arg_fn)();
   tree_set_loc(expr, CURRENT_LOC);
   add_param(expr, right, P_POS, NULL);
}

static void binary_op(tree_t expr, tree_t left, tree_t (*right_fn)(void))
{
   add_param(expr, left, P_POS, NULL);

   tree_t right = (*right_fn)();
   tree_set_loc(expr, CURRENT_LOC);
   add_param(expr, right, P_POS, NULL);
}

static bool bare_subprogram_name(void)
{
   // Context in which a function name appears as a reference rather
   // than a call (i.e. before a signature or as a resolution function
   // name in a subtype declaration)
   return peek() == tLSQUARE || peek() == tID || peek() == tTICK;
}

static tree_t implicit_dereference(tree_t t)
{
   type_t access = type_access(tree_type(t));

   tree_t all = tree_new(T_ALL);
   tree_set_loc(all, tree_loc(t));
   tree_set_value(all, t);
   tree_set_type(all, access);

   return all;
}

static type_t prefix_type(tree_t prefix)
{
   // Check we can acutally resolve the base reference at this point
   tree_t ref = prefix;
   tree_kind_t kind;
   while ((kind = tree_kind(ref)) != T_REF) {
      switch (kind) {
      case T_ARRAY_SLICE:
      case T_ARRAY_REF:
      case T_RECORD_REF:
      case T_ALL:
         ref = tree_value(ref);
         break;
      default:
         return NULL;
      }
   }

   if (tree_has_ref(ref) && !class_has_type(class_of(tree_ref(ref))))
      return NULL;

   return solve_types(nametab, prefix, NULL);
}

static bool is_range_expr(tree_t t)
{
   switch (tree_kind(t)) {
   case T_REF:
      if (tree_has_ref(t))
         return tree_kind(tree_ref(t)) == T_TYPE_DECL;
      else {
         tree_t decl = query_name(nametab, tree_ident(t));
         return decl != NULL && tree_kind(decl) == T_TYPE_DECL;
      }

   case T_ATTR_REF:
      {
         predef_attr_t predef = tree_attr_int(t, builtin_i, -1);
         return predef == ATTR_RANGE || predef == ATTR_REVERSE_RANGE;
      }

   default:
      return false;
   }
}

static tree_t aliased_type_decl(tree_t decl) {
   switch (tree_kind(decl)) {
   case T_ALIAS:
      {
         tree_t value = tree_value(decl);
         if (tree_kind(value) == T_REF && tree_has_ref(value))
            return aliased_type_decl(tree_ref(value));
         else
            return NULL;
      }
   case T_TYPE_DECL:
      return decl;
   default:
      return NULL;
   }
}

static type_t positional_actual_type(tree_t unit, unsigned pos,
                                     formal_kind_t kind)
{
   switch (kind) {
   case F_GENERIC_MAP:
      if (pos >= tree_generics(unit))
         return NULL;
      else
         return tree_type(tree_generic(unit, pos));
   case F_PORT_MAP:
      if (pos >= tree_ports(unit))
         return NULL;
      else
         return tree_type(tree_port(unit, pos));
   default:
      return NULL;
   }
}

static tree_t ensure_labelled(tree_t t, ident_t label)
{
   tree_set_ident(t, label ?: loc_to_ident(CURRENT_LOC));
   return t;
}

static tree_t external_reference(tree_t t)
{
   switch (tree_kind(t)) {
   case T_ENTITY:
   case T_LIBRARY:
   case T_ARCH:
   case T_PACKAGE:
   case T_CONFIGURATION:
      {
         tree_t ref = tree_new(T_REF);
         tree_set_loc(ref, CURRENT_LOC);
         tree_set_ident(ref, tree_ident(t));
         tree_set_ref(ref, t);
         return ref;
      }
   default:
      return t;
   }
}

static void apply_foreign_attribute(tree_t decl, tree_t value)
{
   if (tree_kind(value) != T_LITERAL)
      fatal_at(tree_loc(decl), "foreign attribute must have string "
               "literal value");

   const int nchars = tree_chars(value);
   char buf[nchars + 1];
   for (int i = 0; i < nchars; i++)
      buf[i] = tree_pos(tree_ref(tree_char(value, i)));
   buf[nchars] = '\0';

   ident_t name = ident_new(buf);
   tree_set_ident2(decl, name);

   tree_set_flag(decl, TREE_F_FOREIGN);
}

////////////////////////////////////////////////////////////////////////////////
// Parser rules

static bool p_cond_analysis_relation(void)
{
   // ( conditional_analysis_expression )
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

      (void)lib_find(it->ident, false);

      insert_name(nametab, l, NULL, 0);
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
      insert_names_from_use(nametab, u);
   } while (optional(tCOMMA));

   consume(tSEMI);
}

static void p_context_reference(tree_t unit)
{
   // context selected_name { , selected_name } ;

   BEGIN("context reference");

   consume(tCONTEXT);

   do {
      ident_t name = p_selected_identifier();

      tree_t c = tree_new(T_CTXREF);
      tree_set_ident(c, name);
      tree_set_loc(c, CURRENT_LOC);

      tree_t ctx = find_unit(CURRENT_LOC, name);
      if (ctx != NULL && tree_kind(ctx) == T_CONTEXT) {
         insert_names_from_context(nametab, ctx);
         tree_set_ref(c, ctx);
      }
      else if (ctx != NULL)
         parse_error(CURRENT_LOC, "unit %s is not a context declaration",
                     istr(name));

      tree_add_context(unit, c);
   } while (optional(tCOMMA));

   consume(tSEMI);
}

static tree_t p_pragma(void)
{
   // A pragma is a special comment such as
   //     -- lint_off ....
   // The contents of the comment are stored in a special T_PRAGMA node for
   // processing by external tools.

   consume(tPRAGMA);

   extern char *yytext;

   tree_t pragma = tree_new(T_PRAGMA);
   tree_set_text(pragma, yytext);
   return pragma;
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

   case tPRAGMA:
      tree_add_context(unit, p_pragma());
      break;

   default:
      expect(tLIBRARY, tUSE, tCONTEXT);
   }
}

static void p_context_clause(tree_t unit)
{
   // { context_item }

   BEGIN("context clause");

   while (scan(tLIBRARY, tUSE, tCONTEXT, tPRAGMA)) {
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

static range_t p_range(tree_t left, type_t constraint)
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

   solve_range(nametab, r, constraint);

   return r;
}

static tree_t p_range_constraint(type_t constraint)
{
   // range range

   BEGIN("range constraint");

   consume(tRANGE);

   tree_t t = tree_new(T_CONSTRAINT);
   tree_set_subkind(t, C_RANGE);

   tree_t expr1 = p_expression();

   switch (peek()) {
   case tTO:
   case tDOWNTO:
      tree_add_range(t, p_range(expr1, constraint));
      break;
   default:
      {
         solve_types(nametab, expr1, constraint);

         range_t r = {
            .kind  = RANGE_EXPR,
            .left  = expr1,
            .right = NULL
         };
         tree_add_range(t, r);
      }
   }

   tree_set_loc(t, CURRENT_LOC);
   return t;
}

static range_t p_discrete_range(type_t constraint, tree_t head)
{
   // subtype_indication | range

   BEGIN("discrete range");

   // TODO: check for peek() == tID and if type parse p_type_mark

   tree_t expr1 = head ?: p_expression();

   switch (peek()) {
   case tTO:
   case tDOWNTO:
   case tTICK:
      return p_range(expr1, constraint);

   case tRANGE:
      {
         if (tree_kind(expr1) != T_REF)
            assert(false);   // XXX: FIXME

         constraint = solve_types(nametab, expr1, NULL);

         if (tree_has_ref(expr1))
            assert(tree_kind(tree_ref(expr1)) == T_TYPE_DECL);  // XXX

         consume(tRANGE);

         tree_t left = p_expression();
         range_t r = p_range(left, constraint);

         tree_set_type(left, constraint);  // XXX: need type check
         if (r.right) tree_set_type(r.right, constraint);

         return r;
      }

   default:
      {
         solve_types(nametab, expr1, constraint);

         if (tree_kind(expr1) != T_ATTR_REF) {
            tree_t tmp = tree_new(T_ATTR_REF);
            tree_set_name(tmp, expr1);
            tree_set_ident(tmp, ident_new("RANGE"));
            tree_set_loc(tmp, tree_loc(expr1));
            tree_set_type(tmp, tree_type(expr1));
            tree_add_attr_int(tmp, builtin_i, ATTR_RANGE);

            expr1 = tmp;
         }

         range_t r = {
            .kind  = RANGE_EXPR,
            .left  = expr1,
            .right = NULL
         };
         return r;
      }
   }
}

static tree_t p_slice_name(tree_t prefix, tree_t head)
{
   // prefix ( discrete_range )

   EXTEND("slice name");

   type_t type = prefix_type(prefix);

   if (type != NULL && type_is_access(type)) {
      prefix = implicit_dereference(prefix);
      type   = tree_type(prefix);
   }

   tree_t t = tree_new(T_ARRAY_SLICE);
   tree_set_value(t, prefix);

   type_t index_type = NULL;
   if (type != NULL && type_is_array(type))
      index_type = index_type_of(type, 0);

   tree_add_range(t, p_discrete_range(index_type, head));
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

static tree_t p_association_element(int pos, tree_t unit, formal_kind_t kind)
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

   type_t type = NULL;
   if (look_for(&lookp)) {
      tree_set_subkind(p, P_NAMED);

      push_scope(nametab);
      scope_set_formal_kind(nametab, unit, kind);

      tree_t name = p_formal_part();
      tree_set_name(p, name);

      if (kind == F_GENERIC_MAP || kind == F_PORT_MAP)
         type = solve_types(nametab, name, NULL);

      pop_scope(nametab);

      consume(tASSOC);
   }
   else {
      tree_set_subkind(p, P_POS);
      tree_set_pos(p, pos);
      type = positional_actual_type(unit, pos, kind);
   }

   tree_t value = p_actual_part();
   tree_set_value(p, value);
   tree_set_loc(p, CURRENT_LOC);

   if (kind == F_GENERIC_MAP || kind == F_PORT_MAP)
      solve_types(nametab, value, type);

   return p;
}

static void p_association_list(tree_t map, tree_t unit, formal_kind_t kind)
{
   // association_element { , association_element }

   BEGIN("association list");

   int pos = 0;
   do {
      tree_t p = p_association_element(pos, unit, kind);
      switch (kind) {
      case F_GENERIC_MAP:
         tree_add_genmap(map, p);
         break;
      case F_PORT_MAP:
      case F_SUBPROGRAM:
         tree_add_param(map, p);
         break;
      default:
         fatal_trace("unexpected formal kind in p_association_list");
      }
      if (tree_subkind(p) == P_POS)
         pos++;
   } while (optional(tCOMMA));
}

static void p_actual_parameter_part(tree_t call)
{
   // association_list

   BEGIN("actual parameter part");

   p_association_list(call, NULL, F_SUBPROGRAM);
}

static tree_t p_function_call(ident_t id, tree_t prefix, bool have_args)
{
   // name [ ( actual_parameter_part ) ]

   EXTEND("function call");

   bool protected = prefix != NULL
      && tree_kind(prefix) == T_REF
      && type_is_protected(tree_type(prefix));

   tree_t call = tree_new(protected ? T_PROT_FCALL : T_FCALL);
   tree_set_ident(call, id);
   if (protected)
      tree_set_name(call, prefix);

   if (have_args && optional(tLPAREN)) {
      p_actual_parameter_part(call);
      consume(tRPAREN);
   }

   tree_set_loc(call, CURRENT_LOC);
   return call;
}

static predef_attr_t parse_predefined_attr(ident_t ident)
{
   if (icmp(ident, "RANGE"))
      return ATTR_RANGE;
   else if (icmp(ident, "REVERSE_RANGE"))
      return ATTR_REVERSE_RANGE;
   else if (icmp(ident, "LENGTH"))
      return ATTR_LENGTH;
   else if (icmp(ident, "LEFT"))
      return ATTR_LEFT;
   else if (icmp(ident, "RIGHT"))
      return ATTR_RIGHT;
   else if (icmp(ident, "LOW"))
      return ATTR_LOW;
   else if (icmp(ident, "HIGH"))
      return ATTR_HIGH;
   else if (icmp(ident, "EVENT"))
      return ATTR_EVENT;
   else if (icmp(ident, "ACTIVE"))
      return ATTR_ACTIVE;
   else if (icmp(ident, "IMAGE"))
      return ATTR_IMAGE;
   else if (icmp(ident, "ASCENDING"))
      return ATTR_ASCENDING;
   else if (icmp(ident, "LAST_VALUE"))
      return ATTR_LAST_VALUE;
   else if (icmp(ident, "LAST_EVENT"))
      return ATTR_LAST_EVENT;
   else if (icmp(ident, "PATH_NAME"))
      return ATTR_PATH_NAME;
   else if (icmp(ident, "INSTANCE_NAME"))
      return ATTR_INSTANCE_NAME;
   else if (icmp(ident, "DELAYED"))
      return ATTR_DELAYED;
   else if (icmp(ident, "STABLE"))
      return ATTR_STABLE;
   else if (icmp(ident, "QUIET"))
      return ATTR_QUIET;
   else if (icmp(ident, "TRANSACTION"))
      return ATTR_TRANSACTION;
   else if (icmp(ident, "DRIVING_VALUE"))
      return ATTR_DRIVING_VALUE;
   else if (icmp(ident, "LAST_ACTIVE"))
      return ATTR_LAST_ACTIVE;
   else if (icmp(ident, "DRIVING"))
      return ATTR_DRIVING;
   else if (icmp(ident, "VALUE"))
      return ATTR_VALUE;
   else if (icmp(ident, "SUCC"))
      return ATTR_SUCC;
   else if (icmp(ident, "PRED"))
      return ATTR_PRED;
   else if (icmp(ident, "LEFTOF"))
      return ATTR_LEFTOF;
   else if (icmp(ident, "RIGHTOF"))
      return ATTR_RIGHTOF;
   else if (icmp(ident, "POS"))
      return ATTR_POS;
   else if (icmp(ident, "VAL"))
      return ATTR_VAL;
   else
      return (predef_attr_t)-1;
}

static tree_t p_attribute_name(tree_t prefix)
{
   // prefix [ signature ] ' attribute_designator [ ( expression ) ]

   EXTEND("attribute name");

   type_t type = prefix_type(prefix);

   if (type != NULL && type_is_access(type)) {
      prefix = implicit_dereference(prefix);
      type   = tree_type(prefix);
   }

   prefix = external_reference(prefix);

   consume(tTICK);

   tree_t t = tree_new(T_ATTR_REF);
   tree_set_name(t, prefix);

   ident_t id;
   switch (peek()) {
   case tRANGE:
      consume(tRANGE);
      id = ident_new("RANGE");
      break;
   case tREVRANGE:
      consume(tREVRANGE);
      id = ident_new("REVERSE_RANGE");
      break;
   case tID:
      id = p_identifier();
      break;
   default:
      one_of(tRANGE, tREVRANGE, tID);
      id = ident_new("error");
   }
   tree_set_ident(t, id);

   predef_attr_t predef = -1;
   if ((predef = parse_predefined_attr(id)) != -1) {
      tree_add_attr_int(t, builtin_i, predef);

      if (optional(tLPAREN)) {
         add_param(t, p_expression(), P_POS, NULL);
         consume(tRPAREN);
      }
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

   switch (tree_kind(prefix)) {
   case T_LIBRARY:
      {
         ident_t unit_name = ident_prefix(tree_ident(prefix), suffix, '.');
         tree_t unit = find_unit(CURRENT_LOC, unit_name);
         if (unit == NULL) {
            tree_t dummy = tree_new(T_REF);
            tree_set_ident(dummy, unit_name);
            tree_set_type(dummy, type_new(T_NONE));
            return dummy;
         }

         // Ensure there is a use clause for this unit
         // XXX: this is a kludge that should be rethought
         tree_t use = tree_new(T_USE);
         tree_set_ident(use, unit_name);
         tree_set_loc(use, CURRENT_LOC);

         tree_add_context(scope_unit(nametab), use);

         return unit;

      }

   case T_PACKAGE:
   case T_PROCESS:
   case T_FOR:
   case T_BLOCK:
   case T_ENTITY:
   case T_ARCH:
      {
         ident_t qual = ident_prefix(tree_ident(prefix), suffix, '.');

         tree_t d = search_decls(prefix, suffix, 0);
         if (d == NULL) {
            parse_error(CURRENT_LOC, "name %s not found in %s", istr(suffix),
                        istr(tree_ident(prefix)));
         }
         else if (is_subprogram(d) && !bare_subprogram_name()) {
            tree_t f = p_function_call(suffix, prefix, true);
            tree_set_ident(f, qual); // TODO: seems a bit hacky?
            return f;
         }

         tree_t ref = tree_new(T_REF);
         tree_set_ident(ref, qual);
         tree_set_ref(ref, d);
         tree_set_type(ref, d ? tree_type(d) : type_new(T_NONE));
         tree_set_loc(ref, CURRENT_LOC);

         return ref;
      }

   default:
      break;
   }

   type_t type = solve_types(nametab, prefix, NULL);

   if (type_kind(type) == T_NONE) {
      tree_t r = tree_new(T_REF);
      tree_set_ident(r, suffix);
      tree_set_type(r, type);
      tree_set_loc(r, CURRENT_LOC);
      return r;
   }

   if (type_is_access(type)) {
      prefix = implicit_dereference(prefix);
      type   = tree_type(prefix);
   }

   if (type_kind(type) == T_INCOMPLETE) {
      type = resolve_type(nametab, type);
      tree_set_type(prefix, type);
   }

   if (type_is_record(type)) {
      tree_t rref = tree_new(T_RECORD_REF);
      tree_set_value(rref, prefix);
      tree_set_ident(rref, suffix);
      tree_set_loc(rref, CURRENT_LOC);
      return rref;
   }
   else if (type_is_protected(type)) {
      return p_function_call(suffix, prefix, true);
   }
   else if (type_kind(type) == T_INCOMPLETE) {
      parse_error(tree_loc(prefix), "object with incomplete type %s cannot be "
                  "selected", type_pp(type));
      return prefix;
   }
   else if (tree_kind(prefix) == T_REF) {
      parse_error(tree_loc(prefix), "object %s with type %s cannot be selected",
                  istr(tree_ident(prefix)), type_pp(type));
      return prefix;
   }
   else {
      parse_error(tree_loc(prefix), "object with type %s cannot be selected",
                  type_pp(type));
      return prefix;
   }
}

static tree_t p_indexed_name(tree_t prefix, tree_t head)
{
   // prefix ( expression { , expression } )

   EXTEND("indexed name");

   type_t type = prefix_type(prefix);

   if (type != NULL && type_is_access(type)) {
      prefix = implicit_dereference(prefix);
      type   = tree_type(prefix);
   }

   tree_t t = tree_new(T_ARRAY_REF);
   tree_set_value(t, prefix);

   int n = 0;
   do {
      tree_t index = head ?: p_expression();
      head = NULL;
      add_param(t, index, P_POS, NULL);

      type_t index_type = type ? index_type_of(type, n++) : NULL;
      solve_types(nametab, index, index_type);
   } while (optional(tCOMMA));

   consume(tRPAREN);

   tree_set_loc(t, CURRENT_LOC);
   return t;
}

static tree_t p_type_conversion(ident_t id)
{
   // type_conversion ::= type_mark ( expression )

   EXTEND("type conversion");

   consume(tLPAREN);

   type_t type = p_type_mark(id);

   tree_t conv = tree_new(T_TYPE_CONV);
   tree_set_type(conv, type);
   tree_set_value(conv, p_expression());

   consume(tRPAREN);

   tree_set_loc(conv, CURRENT_LOC);
   return conv;
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

   tree_t prefix = NULL;
   tree_t decl = query_name(nametab, id);

   if (decl != NULL) {
      type_t type = NULL;
      if (class_has_type(class_of(decl)))
         type = tree_type(decl);

      if (peek() == tLPAREN && aliased_type_decl(decl) != NULL)
         prefix = p_type_conversion(id);
      else if (type == NULL && peek() == tDOT)
         prefix = decl;
      else if (type != NULL && type_is_subprogram(type))
         prefix = p_function_call(id, NULL, true);
      else {
         prefix = tree_new(T_REF);
         tree_set_ident(prefix, id);
         tree_set_loc(prefix, CURRENT_LOC);
         tree_set_ref(prefix, decl);
      }
   }
   else if (peek() == tLPAREN && scope_formal_kind(nametab) != F_SUBPROGRAM)
      prefix = p_function_call(id, NULL, true);
   else {
      prefix = tree_new(T_REF);
      tree_set_ident(prefix, id);
      tree_set_loc(prefix, CURRENT_LOC);
   }

   for (;;) {
      switch (peek()) {
      case tLPAREN:
         break;

      case tDOT:
         prefix = p_selected_name(prefix);
         continue;

      case tTICK:
         prefix = p_attribute_name(prefix);
         continue;

      default:
         return prefix;
      }

      // Prefix should be an array that is being indexed or sliced. We
      // have to parse up to the first expression to know which.

      consume(tLPAREN);

      tree_t head = p_expression();

      if (peek() == tDOWNTO || peek() == tTO || is_range_expr(head))
         prefix = p_slice_name(prefix, head);
      else
         prefix = p_indexed_name(prefix, head);
   }
}

static type_t p_type_mark(ident_t name)
{
   // name

   BEGIN("type mark");

   name = name ?: p_identifier();
   tree_t decl = resolve_name(nametab, CURRENT_LOC, name);

   while (decl != NULL && peek() == tDOT) {
      consume(tDOT);
      name = p_identifier();
      decl = search_decls(decl, name, 0);
   }

   if (decl == NULL) {
      skip_selected_name();
      return type_new(T_NONE);
   }

   decl = aliased_type_decl(decl);

   if (decl == NULL) {
      parse_error(CURRENT_LOC, "type mark %s does not refer to a type",
                  istr(name));
      skip_selected_name();
      return type_new(T_NONE);
   }

   return tree_type(decl);
}

static tree_t p_index_constraint(type_t base)
{
   // ( discrete_range { , discrete_range } )

   BEGIN("index constraint");

   consume(tLPAREN);

   int n = 0;
   tree_t t = tree_new(T_CONSTRAINT);
   tree_set_subkind(t, C_INDEX);
   do {
      type_t index_type = base ? index_type_of(base, n++) : NULL;
      tree_add_range(t, p_discrete_range(index_type, NULL));
   } while (optional(tCOMMA));

   consume(tRPAREN);

   if (t != NULL)
      tree_set_loc(t, CURRENT_LOC);

   return t;
}

static void p_constraint(type_t type)
{
   // range_constraint | index_constraint

   assert(type_kind(type) == T_SUBTYPE);
   type_t base = type_base(type);

   switch (peek()) {
   case tRANGE:
      type_set_constraint(type, p_range_constraint(base));
      break;

   case tLPAREN:
      type_set_constraint(type, p_index_constraint(base));
      break;

   default:
      one_of(tRANGE, tLPAREN);
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
      solve_types(nametab, rname, NULL);

      type_t base = p_type_mark(NULL);
      type_set_base(type, base);
   }
   else
      type = p_type_mark(NULL);

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
      tree_set_type(t, type_universal_int());
      break;

   case tREAL:
      tree_set_subkind(t, L_REAL);
      tree_set_dval(t, last_lval.d);
      tree_set_type(t, type_universal_real());
      break;
   }

   tree_set_loc(t, CURRENT_LOC);
   return t;
}

static tree_t p_physical_literal(void)
{
   // [ abstract_literal ] name

   BEGIN("physical literal");

   tree_t  mult  = p_abstract_literal();
   ident_t ident = p_identifier();

   tree_t decl = resolve_name(nametab, CURRENT_LOC, ident);
   if (decl != NULL && tree_kind(decl) == T_UNIT_DECL) {
      tree_set_type(mult, tree_type(decl));
      // TODO: check for overflow here
      int64_t unit = tree_ival(tree_value(decl)), ival;
      if (tree_subkind(mult) == L_REAL)
         ival = unit * tree_dval(mult);
      else
         ival = unit * tree_ival(mult);

      tree_set_subkind(mult, L_PHYSICAL);
      tree_set_ival(mult, ival);
   }
   else {
      if (decl != NULL)
         parse_error(CURRENT_LOC, "%s is not a physical unit", istr(ident));
      tree_set_type(mult, type_new(T_NONE));
   }

   tree_set_loc(mult, CURRENT_LOC);
   return mult;
}

static tree_t p_numeric_literal(void)
{
   // abstract_literal | physical_literal

   BEGIN("numeric literal");

   if (peek_nth(2) == tID)
      return p_physical_literal();
   else
      return p_abstract_literal();
}

static tree_t p_string_literal(void)
{
   // string_literal

   BEGIN("string literal");

   consume(tSTRING);

   char *p = last_lval.s;
   size_t len = strlen(p);
   tree_t t = str_to_literal(p + 1, p + len - 1, NULL);
   free(p);

   tree_set_loc(t, CURRENT_LOC);
   return t;
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
      return p_string_literal();

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

static void p_choice(tree_t parent, type_t constraint)
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
         tree_add_range(t, p_discrete_range(constraint, NULL));
      }
      else {
         tree_t name = p_expression();
         tree_set_subkind(t, A_NAMED);
         tree_set_name(t, name);
         solve_types(nametab, name, constraint);
      }
   }

   tree_set_loc(t, CURRENT_LOC);
   tree_add_assoc(parent, t);
}

static void p_choices(tree_t parent, type_t constraint)
{
   // choices ::= choice { | choice }

   BEGIN("choices");

   p_choice(parent, constraint);

   while (optional(tBAR))
      p_choice(parent, constraint);
}


static void p_element_association_choice(tree_t parent)
{
   // simple_expression | discrete_range | simple_name | others

   // This is duplicated from p_choice as we cannot solve the types
   // eagerly in an element association

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
         // XXX: p_discrete range solves types!
         tree_add_range(t, p_discrete_range(NULL, NULL));
      }
      else {
         tree_t name = p_expression();
         tree_set_subkind(t, A_NAMED);
         tree_set_name(t, name);
      }
   }

   tree_set_loc(t, CURRENT_LOC);
   tree_add_assoc(parent, t);
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

      do {
         p_element_association_choice(agg);
      } while (optional(tBAR));

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
      tree_set_pos(t, tree_assocs(agg));

      tree_add_assoc(agg, t);
   }
}

static tree_t p_aggregate(bool is_target)
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

static tree_t p_qualified_expression(void)
{
   // type_mark ' ( expression ) | type_mark ' aggregate

   EXTEND("qualified expression");

   type_t type = p_type_mark(NULL);

   tree_t qual = tree_new(T_QUALIFIED);
   tree_set_type(qual, type);
   tree_set_ident(qual, type_ident(type));

   consume(tTICK);

   const look_params_t lookp = {
      .look     = { tCOMMA, tASSOC },
      .stop     = { tRPAREN },
      .abort    = tSEMI,
      .nest_in  = tLPAREN,
      .nest_out = tRPAREN,
      .depth    = 1
   };

   tree_t value;
   if (look_for(&lookp))
      value = p_aggregate(false);
   else {
      consume(tLPAREN);
      value = p_expression();
      consume(tRPAREN);
   }

   tree_set_value(qual, value);
   solve_types(nametab, value, type);

   tree_set_loc(qual, CURRENT_LOC);
   return qual;
}

static tree_t p_allocator(void)
{
   // new subtype_indication | new qualified_expression

   BEGIN("allocator");

   consume(tNEW);

   tree_t new = tree_new(T_NEW);

   tree_t value;
   if (peek_nth(2) == tTICK)
      value = p_qualified_expression();
   else {
      type_t type = p_subtype_indication();

      value = tree_new(T_QUALIFIED);
      tree_set_type(value, type);
      tree_set_loc(value, CURRENT_LOC);
      tree_set_value(value, make_default_value(type, CURRENT_LOC));
   }

   tree_set_value(new, value);
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
            return p_aggregate(false);
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
      if (peek_nth(2) == tTICK && peek_nth(3) == tLPAREN)
         return p_qualified_expression();
      else
         return p_name();

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

   if (op != NULL) {
      tree_t t = tree_new(T_FCALL);
      tree_set_ident(t, op);
      unary_op(t, p_primary);
      tree_set_loc(t, CURRENT_LOC);
      return t;
   }
   else {
      tree_t operand = p_primary();

      if (optional(tPOWER)) {
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
      ident_t op  = p_multiplying_operator();
      tree_t left = term;

      term = tree_new(T_FCALL);
      tree_set_ident(term, op);
      binary_op(term, left, p_factor);
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

   tree_t expr = NULL;
   if (scan(tPLUS, tMINUS)) {
      ident_t sign = p_sign();
      expr = tree_new(T_FCALL);
      tree_set_ident(expr, sign);
      unary_op(expr, p_term);
      tree_set_loc(expr, CURRENT_LOC);
   }
   else
      expr = p_term();

   while (scan(tPLUS, tMINUS, tAMP)) {
      tree_t left = expr;
      expr = tree_new(T_FCALL);
      tree_set_ident(expr, p_adding_operator());
      binary_op(expr, left, p_term);
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
      ident_t op  = p_relational_operator();
      tree_t left = rel;

      rel = tree_new(T_FCALL);
      tree_set_ident(rel, op);
      binary_op(rel, left, p_shift_expression);
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

      tree_t left = expr;

      expr = tree_new(T_FCALL);
      tree_set_ident(expr, op);
      binary_op(expr, left, p_relation);
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
   if (optional(tASSIGN)) {
      init = p_expression();
      solve_types(nametab, init, type);
   }

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
   if (optional(tASSIGN)) {
      init = p_expression();
      solve_types(nametab, init, type);
   }

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

static void p_interface_variable_declaration(tree_t parent, class_t def_class,
                                             add_func_t addf)
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

   // See LRM 93 section 2.1.1 for default class
   if ((mode == PORT_OUT || mode == PORT_INOUT) && def_class == C_DEFAULT)
      def_class = C_VARIABLE;

   type_t type = p_subtype_indication();

   tree_t init = NULL;
   if (optional(tASSIGN)) {
      init = p_expression();
      solve_types(nametab, init, type);
   }

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

   insert_ports(nametab, parent);

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

   insert_generics(nametab, parent);

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
   tree_set_type(t, p_type_mark(NULL));
   consume(tSEMI);

   tree_set_loc(t, CURRENT_LOC);
   insert_name(nametab, t, NULL, 0);
   return t;
}

static class_t p_entity_class(void)
{
   // entity | procedure | type | signal | label | group | architecture
   //   | function | subtype | variable | literal | file | configuration
   //   | package | constant | component | units

   BEGIN("entity class");

   switch (one_of(tENTITY, tPROCEDURE, tTYPE, tSIGNAL, tLABEL, tGROUP,
                  tARCHITECTURE, tFUNCTION, tSUBTYPE, tVARIABLE, tLITERAL,
                  tFILE, tCONFIGURATION, tPACKAGE, tCONSTANT, tCOMPONENT,
                  tUNITS)) {
   case tENTITY:
      return C_ENTITY;
   case tPROCEDURE:
      return C_PROCEDURE;
   case tTYPE:
      return C_TYPE;
   case tSIGNAL:
      return C_SIGNAL;
   case tLABEL:
      return C_LABEL;
   case tGROUP:
      return C_DEFAULT;
   case tARCHITECTURE:
      return C_ARCHITECTURE;
   case tFUNCTION:
      return C_FUNCTION;
   case tSUBTYPE:
      return C_SUBTYPE;
   case tVARIABLE:
      return C_VARIABLE;
   case tLITERAL:
      return C_LITERAL;
   case tFILE:
      return C_FILE;
   case tCONFIGURATION:
      return C_CONFIGURATION;
   case tPACKAGE:
      return C_PACKAGE;
   case tCONSTANT:
      return C_CONSTANT;
   case tCOMPONENT:
      return C_COMPONENT;
   case tUNITS:
      return C_UNITS;
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

   type_t type;
   tree_t attr_decl = resolve_name(nametab, CURRENT_LOC, head);
   if (attr_decl == NULL)
      type = type_new(T_NONE);
   else if (tree_kind(attr_decl) != T_ATTR_DECL) {
      parse_error(CURRENT_LOC, "name %s is not an attribute declaration",
                  istr(head));
      type = type_new(T_NONE);
   }
   else
      type = tree_type(attr_decl);

   consume(tOF);

   class_t class;
   LOCAL_IDENT_LIST ids = p_entity_specification(&class);

   consume(tIS);

   tree_t value = p_expression();
   solve_types(nametab, value, type);

   consume(tSEMI);

   const loc_t *loc = CURRENT_LOC;

   for (ident_list_t *it = ids; it != NULL; it = it->next) {
      tree_t t = tree_new(T_ATTR_SPEC);
      tree_set_loc(t, loc);
      tree_set_class(t, class);
      tree_set_ident(t, head);
      tree_set_ident2(t, it->ident);
      tree_set_value(t, value);
      tree_set_ref(t, attr_decl);

      if (class != C_LITERAL) {
         // TODO: this check shouldn't really be here
         tree_t d = resolve_name(nametab, loc, it->ident);
         if (d != NULL && class_of(d) != class)
            parse_error(loc, "class of object %s is %s not %s",
                        istr(it->ident), class_str(class_of(d)),
                        class_str(class));

         if (d != NULL && head == foreign_i)
            apply_foreign_attribute(d, value);
      }

      insert_name(nametab, t, NULL, 0);
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

   tree_t value = tree_new(T_LITERAL);
   tree_set_loc(value, CURRENT_LOC);
   tree_set_subkind(value, L_PHYSICAL);
   tree_set_ival(value, 1);

   tree_t t = tree_new(T_UNIT_DECL);
   tree_set_loc(t, CURRENT_LOC);
   tree_set_value(t, value);
   tree_set_ident(t, id);

   return t;
}

static tree_t p_secondary_unit_declaration(void)
{
   // identifier = physical_literal ;

   BEGIN("secondary unit declaration");

   ident_t id = p_identifier();
   consume(tEQ);
   tree_t value = p_physical_literal();
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
   tree_set_type(base, t);
   tree_set_type(tree_value(base), t);
   type_add_unit(t, base);
   type_add_dim(t, r);

   push_scope(nametab);

   insert_name(nametab, base, NULL, 0);

   while (scan(tINT, tREAL, tID)) {
      tree_t unit = p_secondary_unit_declaration();
      tree_set_type(unit, t);
      type_add_unit(t, unit);
      insert_name(nametab, unit, NULL, 0);
   }

   pop_scope(nametab);

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

   range_t r = {
      .kind  = RANGE_TO,
      .left  = get_int_lit(type_enum_literal(t, 0), 0),
      .right = get_int_lit(type_enum_literal(t, pos - 1), pos -1)
   };
   type_add_dim(t, r);

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
         range_t r = tree_range(p_range_constraint(NULL), 0);

         if (peek() == tUNITS)
            return p_physical_type_definition(r);
         else if (type_is_real(tree_type(r.left)))
           return p_real_type_definition(r);
         else
           return p_integer_type_definition(r);
      }

   case tLPAREN:
      return p_enumeration_type_definition();

   default:
      one_of(tRANGE, tLPAREN);
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
   type_set_file(t, p_type_mark(NULL));

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

   type_t t = p_type_mark(NULL);

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

   consume(tLPAREN);
   do {
      type_t index_type = std_type(find_std(nametab), "INTEGER");
      type_add_dim(t, p_discrete_range(index_type, NULL));
   } while (optional(tCOMMA));
   consume(tRPAREN);

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
         push_scope(nametab);
         tree_t spec = p_subprogram_specification();
         tree_set_flag(spec, TREE_F_PROTECTED);
         type_add_decl(type, p_subprogram_declaration(spec));
         pop_scope(nametab);
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

static type_t p_protected_type_declaration(tree_t tdecl)
{
   // protected protected_type_declarative_part end protected [ simple_name ]

   BEGIN("protected type declaration");

   consume(tPROTECTED);

   type_t type = type_new(T_PROTECTED);
   ident_t id = tree_ident(tdecl);
   type_set_ident(type, id);

   tree_set_type(tdecl, type);

   push_scope(nametab);
   scope_set_prefix(nametab, id);
   insert_name(nametab, tdecl, NULL, 0);

   p_protected_type_declarative_part(type);

   pop_scope(nametab);

   consume(tEND);
   consume(tPROTECTED);

   if (peek() == tID)
      type_set_ident(type, p_identifier());  // XXX: check same as tdecl id

   return type;
}

static type_t p_protected_type_definition(tree_t tdecl)
{
   // protected_type_declaration | protected_type_body

   BEGIN("protected type definition");

   // Protected type bodies are trees rather than types and so handled
   // elsewhere to simplify the parser

   return p_protected_type_declaration(tdecl);
}

static type_t p_type_definition(tree_t tdecl)
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
      return p_protected_type_definition(tdecl);

   default:
      expect(tRANGE, tACCESS, tFILE, tRECORD, STD(00, tPROTECTED));
      return type_new(T_NONE);
   }
}

static type_t p_full_type_declaration(tree_t tdecl)
{
   // type identifier is type_definition ;

   EXTEND("full type declaration");

   consume(tIS);

   type_t t = p_type_definition(tdecl);
   type_set_ident(t, tree_ident(tdecl));
   mangle_type(nametab, t);

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
   mangle_type(nametab, t);

   return t;
}

static void p_type_declaration(tree_t container)
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
      tree_t body = p_protected_type_body(id);
      consume(tSEMI);

      tree_add_decl(container, body);
   }
   else {
      // Insert univeral_integer and universal_real predefined functions
      // before STD.INTEGER and STD.REAL respectively
      if (bootstrapping) {
         int dpos = tree_decls(container);

         if (id == ident_new("INTEGER"))
            declare_predefined_ops(container, type_universal_int());
         else if (id == ident_new("REAL"))
            declare_predefined_ops(container, type_universal_real());

         for (; dpos < tree_decls(container); dpos++)
            tree_set_flag(tree_decl(container, dpos), TREE_F_UNIVERSAL);
      }

      tree_t t = tree_new(T_TYPE_DECL);
      tree_set_ident(t, id);
      tree_set_loc(t, CURRENT_LOC);

      type_t type;
      if (peek() == tSEMI)
         type = p_incomplete_type_declaration(id);
      else
         type = p_full_type_declaration(t);

      tree_set_type(t, type);
      tree_set_loc(t, CURRENT_LOC);

      insert_name(nametab, t, id, 0);

      tree_add_decl(container, t);

      if (type_kind(type) != T_INCOMPLETE)
         declare_predefined_ops(container, type);
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

   if (type_kind(sub) != T_SUBTYPE || type_has_ident(sub)) {
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

   insert_name(nametab, t, id, 0);
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
   if (optional(tASSIGN)) {
      init = p_expression();
      solve_types(nametab, init, type);
   }

   consume(tSEMI);

   for (ident_list_t *it = ids; it != NULL; it = it->next) {
      tree_t t = tree_new(T_CONST_DECL);
      tree_set_ident(t, it->ident);
      tree_set_type(t, type);
      tree_set_loc(t, CURRENT_LOC);
      if (init != NULL)
         tree_set_value(t, init);

      tree_add_decl(parent, t);

      mangle_decl(nametab, t);
      insert_name(nametab, t, it->ident, 0);
   }
}

static tree_t p_assertion(void)
{
   // assert condition [ report expression ] [ severity expression ]

   BEGIN("assertion");

   tree_t s = tree_new(T_ASSERT);

   consume(tASSERT);

   tree_t std = find_std(nametab);

   tree_t value = p_expression();
   solve_types(nametab, value, std_type(std, "BOOLEAN"));
   tree_set_value(s, value);

   if (optional(tREPORT)) {
      tree_t message = p_expression();
      solve_types(nametab, message, std_type(std, "STRING"));
      tree_set_message(s, message);
   }

   tree_t severity;
   if (optional(tSEVERITY))
      severity = p_expression();
   else
      severity = make_ref(search_decls(std, ident_new("ERROR"), 0));

   solve_types(nametab, severity, std_type(std, "SEVERITY_LEVEL"));
   tree_set_severity(s, severity);

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
      p_interface_list(C_DEFAULT, t, tree_add_port);
      consume(tRPAREN);

      const int nports = tree_ports(t);
      for (int i = 0; i < nports; i++)
         type_add_param(type, tree_type(tree_port(t, i)));
   }

   if (tree_kind(t) == T_FUNC_DECL) {
      consume(tRETURN);
      type_set_result(type, p_type_mark(NULL));
   }

   mangle_func(nametab, t);

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
   if (optional(tASSIGN)) {
      init = p_expression();
      solve_types(nametab, init, type);
   }

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

      mangle_decl(nametab, t);
      insert_name(nametab, t, it->ident, 0);
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
   if (optional(tASSIGN)) {
      init = p_expression();
      solve_types(nametab, init, type);
   }

   consume(tSEMI);

   const loc_t *loc = CURRENT_LOC;

   for (ident_list_t *it = ids; it != NULL; it = it->next) {
      tree_t t = tree_new(T_SIGNAL_DECL);
      tree_set_loc(t, loc);
      tree_set_ident(t, it->ident);
      tree_set_type(t, type);
      tree_set_value(t, init);

      tree_add_decl(parent, t);

      mangle_decl(nametab, t);
      insert_name(nametab, t, it->ident, 0);
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

   bool error = false;
   if (not_at_token(tRETURN, tRSQUARE)) {
      do {
         type_t param = p_type_mark(NULL);
         type_add_param(type, param);
         error = error || type_is_none(param);
      } while (optional(tCOMMA));
   }

   if (optional(tRETURN)) {
      type_t ret = p_type_mark(NULL);
      type_set_result(type, ret);
      error = error || type_is_none(ret);
   }

   consume(tRSQUARE);

   return error ? type_new(T_NONE) : type;
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

   tree_t value = p_name();
   tree_set_value(t, value);

   if (peek() == tLSQUARE) {
      type_t type = p_signature();
      if (has_subtype_indication) {
         parse_error(CURRENT_LOC, "alias declaration may not contain both a "
                     "signature and a subtype indication");
         type = type_new(T_NONE);
      }
      else if (tree_kind(value) != T_REF) {
         parse_error(tree_loc(value), "invalid name in subprogram alias");
         type = type_new(T_NONE);
      }
      else
         solve_types(nametab, value, type);
      tree_set_type(t, type);
   }
   else {
      type_t value_type = solve_types(nametab, value, NULL);
      if (!has_subtype_indication)
         tree_set_type(t, value_type);
   }

   const bool type_alias =
      tree_kind(value) == T_REF
      && tree_has_ref(value)
      && tree_kind(tree_ref(value)) == T_TYPE_DECL;

   if (type_alias && has_subtype_indication)
      parse_error(CURRENT_LOC, "non-object alias may not have "
                  "subtype indication");

   consume(tSEMI);

   insert_name(nametab, t, NULL, 0);
   tree_set_loc(t, CURRENT_LOC);
   return t;
}

static void p_file_open_information(tree_t *mode, tree_t *name)
{
   // [ open expression ] is file_logical_name

   BEGIN("file open information");

   tree_t std = find_std(nametab);

   if (optional(tOPEN)) {
      *mode = p_expression();
      solve_types(nametab, *mode, std_type(std, "FILE_OPEN_KIND"));
   }
   else
      *mode = NULL;

   if (optional(tIS)) {
      ident_t mode_name = ident_new("READ_MODE");
      if ((*mode == NULL) && scan(tIN, tOUT)) {
         // VHDL-87 compatibility
         switch (one_of(tIN, tOUT)) {
         case tIN: break;
         case tOUT: mode_name = ident_new("WRITE_MODE"); break;
         }
      }

      *name = p_expression();
      solve_types(nametab, *name, std_type(std, "STRING"));

      if (*mode == NULL) {
         tree_t decl = search_decls(std, mode_name, 0);
         *mode = make_ref(decl);
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

      mangle_decl(nametab, t);
      insert_name(nametab, t, it->ident, 0);
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
      p_type_declaration(body);
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
         push_scope(nametab);
         tree_t spec = p_subprogram_specification();
         tree_set_flag(spec, TREE_F_PROTECTED);
         if (peek() == tSEMI)
            tree_add_decl(body, p_subprogram_declaration(spec));
         else
            tree_add_decl(body, p_subprogram_body(spec));
         pop_scope(nametab);
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

static tree_t p_protected_type_body(ident_t id)
{
   // protected body protected_type_body_declarative_part end protected body
   //   [ simple name ]

   BEGIN("protected type body");

   consume(tPROTECTED);
   consume(tBODY);

   push_scope(nametab);
   scope_set_prefix(nametab, id);

   tree_t tdecl = resolve_name(nametab, CURRENT_LOC, id);

   type_t type = NULL;
   if (tdecl != NULL) {
      const bool protected = tree_kind(tdecl) == T_TYPE_DECL
         && type_is_protected((type = tree_type(tdecl)));

      if (!protected) {
         parse_error(CURRENT_LOC, "object %s is not a protected type "
                     "declaration", istr(id));
         type = type_new(T_NONE);
      }

      insert_name(nametab, tdecl, NULL, 0);
   }

   tree_t body = tree_new(T_PROT_BODY);
   tree_set_ident(body, id);
   tree_set_type(body, type);

   p_protected_type_body_declarative_part(body);

   pop_scope(nametab);

   consume(tEND);
   consume(tPROTECTED);
   consume(tBODY);

   if (peek() == tID && p_identifier() != id)
      parse_error(CURRENT_LOC, "expected protected body trailing label "
                  "to match %s", istr(id));

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
      p_type_declaration(entity);
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
         push_scope(nametab);
         tree_t spec = p_subprogram_specification();
         if (peek() == tSEMI)
            tree_add_decl(entity, p_subprogram_declaration(spec));
         else
            tree_add_decl(entity, p_subprogram_body(spec));
         pop_scope(nametab);
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
      p_type_declaration(sub);
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
         push_scope(nametab);
         tree_t spec = p_subprogram_specification();
         tree_set_flag(spec, tree_flags(sub) & TREE_F_PROTECTED);
         if (peek() == tSEMI)
            tree_add_decl(sub, p_subprogram_declaration(spec));
         else
            tree_add_decl(sub, p_subprogram_body(spec));
         pop_scope(nametab);
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

   insert_ports(nametab, spec);

   consume(tIS);

   const tree_kind_t kind =
      (tree_kind(spec) == T_FUNC_DECL) ? T_FUNC_BODY : T_PROC_BODY;
   tree_change_kind(spec, kind);

   scope_set_subprogram(nametab, spec);

   insert_name(nametab, spec, NULL, 1);
   push_scope(nametab);

   p_subprogram_declarative_part(spec);

   consume(tBEGIN);

   p_sequence_of_statements(spec, tree_add_stmt);

   consume(tEND);

   pop_scope(nametab);

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

   insert_name(nametab, spec, NULL, 1);

   consume(tSEMI);
   return spec;
}

static void p_sensitivity_list(tree_t proc)
{
   // name { , name }

   BEGIN("sensitivity list");

   do {
      tree_t name = p_name();
      tree_add_trigger(proc, name);
      solve_types(nametab, name, NULL);
   } while (optional(tCOMMA));
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
      p_type_declaration(proc);
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
         push_scope(nametab);
         tree_t spec = p_subprogram_specification();
         if (peek() == tSEMI)
            tree_add_decl(proc, p_subprogram_declaration(spec));
         else
            tree_add_decl(proc, p_subprogram_body(spec));
         pop_scope(nametab);
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

   push_scope(nametab);

   if (label != NULL) {
      tree_set_ident(t, label);
      tree_set_loc(t, CURRENT_LOC);
      insert_name(nametab, t, label, 1);
      scope_set_prefix(nametab, label);
   }
   else {
      ident_t tmp = loc_to_ident(CURRENT_LOC);
      tree_set_ident(t, tmp);
      tree_set_flag(t, TREE_F_SYNTHETIC_NAME);
      scope_set_prefix(nametab, tmp);
   }

   p_process_declarative_part(t);

   consume(tBEGIN);

   p_process_statement_part(t);

   consume(tEND);
   if (postponed)
      optional(tPOSTPONED);
   consume(tPROCESS);
   p_trailing_label(label);
   consume(tSEMI);

   pop_scope(nametab);

   tree_set_loc(t, CURRENT_LOC);

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

   tree_set_loc(unit, CURRENT_LOC);
   insert_name(nametab, unit, id, 0);

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

   push_scope(nametab);

   if (peek() == tGENERIC)
      p_generic_clause(c);

   if (peek() == tPORT)
      p_port_clause(c);

   pop_scope(nametab);

   consume(tEND);
   consume(tCOMPONENT);
   p_trailing_label(tree_ident(c));
   consume(tSEMI);

   tree_set_loc(c, CURRENT_LOC);
   insert_name(nametab, c, NULL, 0);
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
      p_type_declaration(pack);
      break;

   case tFUNCTION:
   case tPROCEDURE:
   case tIMPURE:
   case tPURE:
      {
         push_scope(nametab);
         tree_t spec = p_subprogram_specification();
         if (peek() == tSEMI)
            tree_add_decl(pack, p_subprogram_declaration(spec));
         else
            tree_add_decl(pack, p_subprogram_body(spec));
         pop_scope(nametab);
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

   ident_t name = p_identifier();

   tree_change_kind(unit, T_PACKAGE);
   tree_set_ident(unit, name);

   ident_t qual = ident_prefix(lib_name(lib_work()), name, '.');
   scope_set_prefix(nametab, qual);

   insert_name(nametab, unit, NULL, 0);

   consume(tIS);

   push_scope(nametab);
   p_package_declarative_part(unit);
   pop_scope(nametab);

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

static void p_port_map_aspect(tree_t inst, tree_t unit)
{
   // port map ( association_list )

   BEGIN("port map aspect");

   consume(tPORT);
   consume(tMAP);
   consume(tLPAREN);

   p_association_list(inst, unit, F_PORT_MAP);

   consume(tRPAREN);
}

static void p_generic_map_aspect(tree_t inst, tree_t unit)
{
   // generic map ( association_list )

   BEGIN("generic map aspect");

   consume(tGENERIC);
   consume(tMAP);
   consume(tLPAREN);

   p_association_list(inst, unit, F_GENERIC_MAP);

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
         tree_set_loc(bind, CURRENT_LOC);

         return bind;
      }

   case tCONFIGURATION:
      {
         tree_t bind = tree_new(T_BINDING);
         tree_set_class(bind, C_CONFIGURATION);
         tree_set_ident(bind, p_selected_identifier());
         tree_set_loc(bind, CURRENT_LOC);

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

   tree_t ref = find_binding(bind);

   if (peek() == tGENERIC) {
      assert(bind != NULL);   // XXX: check for open here
      p_generic_map_aspect(bind, ref);
   }

   if (peek() == tPORT) {
      assert(bind != NULL);   // XXX: check for open here
      p_port_map_aspect(bind, ref);
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
   else
      tree_add_decl(unit, p_block_configuration());
}

static void p_index_specification(void)
{
   // discrete_range | expression

   const look_params_t lookp = {
      .look     = { tDOWNTO, tTO, tRANGE, tREVRANGE },
      .stop     = { tRPAREN, tCOMMA, tASSOC, tBAR },
      .abort    = tSEMI,
      .nest_in  = tLPAREN,
      .nest_out = tRPAREN,
      .depth    = 0
   };

   if (look_for(&lookp)) {
      p_discrete_range(NULL, NULL);
   }
   else {
      p_expression();
   }
}

static void p_block_specification(tree_t unit)
{
   // label | label [ ( index_specification ) ]

   BEGIN("block specification");

   tree_set_ident(unit, p_identifier());

   if (optional(tLPAREN)) {
      p_index_specification();
      consume(tRPAREN);
   }
}

static tree_t p_block_configuration(void)
{
   // for block_specification { use_clause } { configuration_item } end for ;

   BEGIN("block configuration");

   consume(tFOR);

   tree_t b = tree_new(T_BLOCK_CONFIG);

   p_block_specification(b);

   while (not_at_token(tEND))
      p_configuration_item(b);

   consume(tEND);
   consume(tFOR);
   consume(tSEMI);

   return b;
}

static void p_configuration_declaration(tree_t unit)
{
   // configuration identifier of name is configuration_declarative_part
   //   block_configuration end [ configuration ] [ simple_name ] ;

   BEGIN("configuration declaration");

   consume(tCONFIGURATION);

   tree_change_kind(unit, T_CONFIGURATION);
   tree_set_ident(unit, p_identifier());

   consume(tOF);

   tree_set_ident2(unit, p_identifier());

   consume(tIS);

   while (not_at_token(tFOR))
      p_configuration_declarative_part(unit);

   tree_add_decl(unit, p_block_configuration());

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
   if (tree_contexts(unit) != 3)     // Implicit WORK and STD
      parse_error(tree_loc(tree_context(unit, 3)), "context clause preceeding "
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
      p_type_declaration(parent);
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
         push_scope(nametab);
         tree_t spec = p_subprogram_specification();
         if (peek() == tSEMI)
            tree_add_decl(parent, p_subprogram_declaration(spec));
         else
            tree_add_decl(parent, p_subprogram_body(spec));
         pop_scope(nametab);
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
         return p_aggregate(true);
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

   tree_t target = p_target(name);
   tree_set_target(t, target);

   type_t target_type = solve_types(nametab, target, NULL);

   consume(tASSIGN);

   tree_t value = p_expression();
   tree_set_value(t, value);
   solve_types(nametab, value, target_type);

   consume(tSEMI);

   const loc_t *loc = CURRENT_LOC;
   tree_set_loc(t, loc);

   if (label == NULL)
      label = loc_to_ident(loc);
   tree_set_ident(t, label);

   return t;
}

static tree_t p_waveform_element(type_t constraint)
{
   // expression [ after expression ] | null [ after expression ]

   BEGIN("waveform element");

   tree_t w = tree_new(T_WAVEFORM);
   tree_t value = p_expression();
   tree_set_value(w, value);

   solve_types(nametab, value, constraint);

   if (optional(tAFTER)) {
      tree_t delay = p_expression();
      tree_set_delay(w, delay);
      solve_types(nametab, delay, std_type(find_std(nametab), "TIME"));
   }

   tree_set_loc(w, CURRENT_LOC);

   return w;
}

static void p_waveform(tree_t stmt, type_t constraint)
{
   // waveform_element { , waveform_element } | unaffected

   BEGIN("waveform");

   if (optional(tUNAFFECTED))
      return;

   tree_add_waveform(stmt, p_waveform_element(constraint));

   while (optional(tCOMMA))
      tree_add_waveform(stmt, p_waveform_element(constraint));
}

static tree_t p_delay_mechanism(void)
{
   // transport | [ reject expression ] inertial

   BEGIN("delay mechanism");

   switch (peek()) {
   case tTRANSPORT:
      consume(tTRANSPORT);
      return get_time(0, CURRENT_LOC);

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

   tree_t target = p_target(name);
   tree_set_target(t, target);

   consume(tLE);

   type_t target_type = NULL;
   const bool aggregate = tree_kind(target) == T_AGGREGATE;
   if (!aggregate)
      target_type = solve_types(nametab, target, NULL);

   tree_t reject = p_delay_mechanism();

   p_waveform(t, target_type);

   consume(tSEMI);

   if (aggregate)
      solve_types(nametab, target, tree_type(tree_value(tree_waveform(t, 0))));

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

   tree_t value = p_expression();
   tree_set_value(wait, value);
   solve_types(nametab, value, std_type(find_std(nametab), "BOOLEAN"));
}

static void p_timeout_clause(tree_t wait)
{
   // for expression

   BEGIN("timeout clause");

   consume(tFOR);

   tree_t delay = p_expression();
   tree_set_delay(wait, delay);
   solve_types(nametab, delay, std_type(find_std(nametab), "TIME"));
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

   tree_t std = find_std(nametab);

   tree_t m = p_expression();
   tree_set_message(t, m);
   solve_types(nametab, m, std_type(std, "STRING"));

   tree_t s;
   if (optional(tSEVERITY))
      s = p_expression();
   else
      s = make_ref(search_decls(std, ident_new("NOTE"), 0));

   tree_set_severity(t, s);
   solve_types(nametab, s, std_type(std, "SEVERITY_LEVEL"));

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

   type_t boolean = std_type(find_std(nametab), "BOOLEAN");

   tree_t value = p_expression();
   tree_set_value(t, value);
   solve_types(nametab, value, boolean);

   consume(tTHEN);

   p_sequence_of_statements(t, tree_add_stmt);

   tree_t tail = t;

   while (optional(tELSIF)) {
      tree_t elsif = tree_new(T_IF);
      tree_set_ident(elsif, ident_uniq("elsif"));
      tree_t elsif_value = p_expression();
      tree_set_value(elsif, elsif_value);
      solve_types(nametab, elsif_value, boolean);

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

   ident_t id = p_identifier();

   consume(tIN);

   range_t r = p_discrete_range(NULL, NULL);
   tree_add_range(loop, r);

   type_t base = tree_type(r.left);
   if (type_is_universal(base))
      // XXX: seems like this will hide some errors
      base = std_type(find_std(nametab), "INTEGER");

   tree_t constraint = tree_new(T_CONSTRAINT);
   tree_set_subkind(constraint, C_RANGE);
   tree_add_range(constraint, r);

   type_t sub = type_new(T_SUBTYPE);
   type_set_ident(sub, type_ident(base));
   type_set_base(sub, base);
   type_set_constraint(sub, constraint);

   tree_kind_t kind = tree_kind(loop) == T_FOR_GENERATE ? T_GENVAR : T_VAR_DECL;
   tree_t var = tree_new(kind);
   tree_set_ident(var, id);
   tree_set_type(var, sub);
   tree_set_loc(var, CURRENT_LOC);
   tree_set_flag(var, TREE_F_LOOP_VAR);
   tree_add_decl(loop, var);

   insert_name(nametab, var, NULL, 0);
}

static tree_t p_iteration_scheme(void)
{
   // while condition | for parameter_specification

   BEGIN("iteration scheme");

   if (optional(tWHILE)) {
      tree_t t = tree_new(T_WHILE);
      tree_t value = p_expression();
      tree_set_value(t, value);
      solve_types(nametab, value, std_type(find_std(nametab), "BOOLEAN"));
      return t;
   }
   else if (optional(tFOR)) {
      tree_t t = tree_new(T_FOR);
      p_parameter_specification(t);
      return t;
   }
   else {
      tree_t btrue = search_decls(find_std(nametab), ident_new("TRUE"), 0);
      tree_t t = tree_new(T_WHILE);
      tree_set_value(t, make_ref(btrue));
      return t;
   }
}

static tree_t p_loop_statement(ident_t label)
{
   // [ loop_label : ] [ iteration_scheme ] loop sequence_of_statements
   //   end loop [ loop_label ] ;

   BEGIN("loop statement");

   push_scope(nametab);

   tree_t t = p_iteration_scheme();

   consume(tLOOP);

   if (label != NULL) {
      set_label_and_loc(t, label, CURRENT_LOC);
      insert_name(nametab, t, NULL, 0);
   }

   p_sequence_of_statements(t, tree_add_stmt);

   consume(tEND);
   consume(tLOOP);
   p_trailing_label(label);
   consume(tSEMI);

   set_label_and_loc(t, label, CURRENT_LOC);
   pop_scope(nametab);
   return t;
}

static tree_t p_return_statement(ident_t label)
{
   // [ label : ] return [ expression ] ;

   EXTEND("return statement");

   consume(tRETURN);

   tree_t t = tree_new(T_RETURN);

   if (peek() != tSEMI) {
      type_t return_type = NULL;
      tree_t subprog = scope_subprogram(nametab);
      if (subprog != NULL && tree_kind(subprog) == T_FUNC_BODY)
         return_type = type_result(tree_type(subprog));

      tree_t value = p_expression();
      solve_types(nametab, value, return_type);
      tree_set_value(t, value);
   }

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

   if (optional(tWHEN)) {
      tree_t when = p_expression();
      tree_set_value(t, when);
      solve_types(nametab, when, std_type(find_std(nametab), "BOOLEAN"));
   }

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

   if (optional(tWHEN)) {
      tree_t when = p_expression();
      tree_set_value(t, when);
      solve_types(nametab, when, std_type(find_std(nametab), "BOOLEAN"));
   }

   consume(tSEMI);

   set_label_and_loc(t, label, CURRENT_LOC);
   return t;
}

static tree_t p_procedure_call_statement(ident_t label, tree_t name)
{
   // [ label : ] procedure_call ;

   EXTEND("procedure call statement");

   consume(tSEMI);

   tree_kind_t namek = tree_kind(name);
   switch (namek) {
   case T_FCALL:
   case T_PROT_FCALL:
   case T_REF:
      tree_change_kind(name, namek == T_PROT_FCALL ? T_PROT_PCALL : T_PCALL);
      // Fall-through
   case T_PCALL:
      tree_set_ident2(name, tree_ident(name));
      solve_types(nametab, name, NULL);
      break;
   default:
      if (!type_is_none(solve_types(nametab, name, NULL)))
         parse_error(CURRENT_LOC, "invalid procedure call statement");
      name = tree_new(T_PCALL);
   }

   set_label_and_loc(name, label, CURRENT_LOC);
   return name;
}

static void p_case_statement_alternative(tree_t stmt)
{
   // when choices => sequence_of_statements

   BEGIN("case statement alternative");

   consume(tWHEN);

   const int nstart = tree_assocs(stmt);
   p_choices(stmt, tree_type(tree_value(stmt)));

   consume(tASSOC);

   tree_t b = tree_new(T_BLOCK);
   tree_set_ident(b, loc_to_ident(CURRENT_LOC));
   p_sequence_of_statements(b, tree_add_stmt);

   const int nassocs = tree_assocs(stmt);
   for (int i = nstart; i < nassocs; i++) {
      tree_t a = tree_assoc(stmt, i);
      tree_set_value(a, b);

      if (tree_subkind(a) == A_NAMED)
         solve_types(nametab, tree_name(a), tree_type(tree_value(stmt)));
   }
}

static tree_t p_case_statement(ident_t label)
{
   // [ label : ] case expression is case_statement_alternative
   //   { case_statement_alternative } end case [ label ] ;

   EXTEND("case statement");

   consume(tCASE);

   tree_t t = tree_new(T_CASE);

   tree_t value = p_expression();
   tree_set_value(t, value);
   solve_types(nametab, value, NULL);

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

   if (peek() == tPRAGMA)
      return p_pragma();

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
         tree_t agg = p_aggregate(true);

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

   tree_set_loc(t, CURRENT_LOC);
   return t;
}

static tree_t p_component_instantiation_statement(ident_t label)
{
   // label : instantiated_unit [ generic_map_aspect ] [ port_map_aspect ] ;

   EXTEND("component instantiation statement");

   tree_t t = p_instantiated_unit();
   tree_set_ident(t, label);

   tree_t ref = find_binding(t);
   tree_set_ref(t, ref);

   if (peek() == tGENERIC)
      p_generic_map_aspect(t, ref);

   if (peek() == tPORT)
      p_port_map_aspect(t, ref);

   consume(tSEMI);

   tree_set_loc(t, CURRENT_LOC);

   if (label == NULL)
      parse_error(CURRENT_LOC, "component instantiation statement must "
                  "have a label");
   else
      insert_name(nametab, t, NULL, 0);

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

static void p_conditional_waveforms(tree_t stmt, type_t constraint)
{
   // { waveform when condition else } waveform [ when condition ]

   BEGIN("conditional waveforms");

   for (;;) {
      tree_t c = tree_new(T_COND);
      p_waveform(c, constraint);
      tree_set_loc(c, CURRENT_LOC);

      tree_add_cond(stmt, c);

      if (optional(tWHEN)) {
         tree_t when = p_expression();
         tree_set_value(c, when);
         solve_types(nametab, when, std_type(find_std(nametab), "BOOLEAN"));

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
   tree_t target = p_target(NULL);
   tree_set_target(t, target);

   consume(tLE);

   tree_t reject = p_options(t);

   type_t target_type = NULL;
   const bool aggregate = tree_kind(target) == T_AGGREGATE;
   if (!aggregate)
      target_type = solve_types(nametab, target, NULL);

   p_conditional_waveforms(t, target_type);

   const int nconds = tree_conds(t);
   for (int i = 0; i < nconds; i++)
      set_delay_mechanism(tree_cond(t, i), reject);

   consume(tSEMI);

   if (aggregate) {
      type_t type = tree_type(tree_value(tree_waveform(tree_cond(t, 0), 0)));
      solve_types(nametab, target, type);
   }

   tree_set_loc(t, CURRENT_LOC);
   return t;
}

static void p_selected_waveforms(tree_t stmt, type_t constraint)
{
   // { waveform when choices , } waveform when choices

   BEGIN("selected waveforms");

   type_t with_type = tree_type(tree_value(stmt));

   do {
      tree_t a = tree_new(T_SIGNAL_ASSIGN);
      p_waveform(a, constraint);

      consume(tWHEN);

      const int nstart = tree_assocs(stmt);
      p_choices(stmt, with_type);

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

   tree_t value = p_expression();
   tree_set_value(t, value);
   solve_types(nametab, value, NULL);

   consume(tSELECT);
   tree_t target = p_target(NULL);

   type_t target_type = NULL;
   const bool aggregate = tree_kind(target) == T_AGGREGATE;
   if (!aggregate)
      target_type = solve_types(nametab, target, NULL);

   consume(tLE);
   tree_t reject = p_options(t);
   p_selected_waveforms(t, target_type);
   consume(tSEMI);

   const int nassocs = tree_assocs(t);
   for (int i = 0; i < nassocs; i++) {
      tree_t s = tree_value(tree_assoc(t, i));
      tree_set_target(s, target);
      set_delay_mechanism(s, reject);
   }

   if (aggregate) {
      type_t type = tree_type(tree_value(tree_waveform(tree_assoc(t, 0), 0)));
      solve_types(nametab, target, type);
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
   if (kind != T_REF && kind != T_FCALL && kind != T_PCALL) {
      // This can only happen due to some earlier parsing error
      assert(error_count() > 0);
      consume(tSEMI);
      return ensure_labelled(tree_new(T_CPCALL), label);
   }

   tree_change_kind(t, T_CPCALL);
   tree_set_ident2(t, tree_ident(t));

   consume(tSEMI);

   if (postponed)
      tree_set_flag(t, TREE_F_POSTPONED);

   tree_set_loc(t, CURRENT_LOC);
   ensure_labelled(t, label);

   solve_types(nametab, t, NULL);
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

   push_scope(nametab);

   consume(tBLOCK);

   if (label == NULL)
      parse_error(CURRENT_LOC, "block statement must have a label");
   else {
      insert_name(nametab, b, NULL, 0);
      scope_set_prefix(nametab, label);
   }

   optional(tIS);
   p_block_declarative_part(b);
   consume(tBEGIN);
   p_block_statement_part(b);
   consume(tEND);
   consume(tBLOCK);
   p_trailing_label(label);
   consume(tSEMI);

   resolve_specs(nametab, b);

   pop_scope(nametab);

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
         tree_t expr = p_expression();
         tree_set_value(g, expr);
         solve_types(nametab, expr, std_type(find_std(nametab), "BOOLEAN"));
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

   push_scope(nametab);

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

   pop_scope(nametab);

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

   if (peek() == tPRAGMA)
      return p_pragma();

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
         return ensure_labelled(tree_new(T_BLOCK), label);
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
   ident_t arch_name = p_identifier();
   tree_set_ident(unit, arch_name);
   consume(tOF);
   ident_t entity_name = p_identifier();
   tree_set_ident2(unit, entity_name);
   consume(tIS);

   push_scope(nametab);

   ident_t qual = ident_prefix(lib_name(lib_work()), entity_name, '.');
   tree_t e = lib_get_check_stale(lib_work(), qual);
   if (e == NULL)
      parse_error(CURRENT_LOC, "missing declaration for entity %s",
                  istr(entity_name));
   else if (tree_kind(e) == T_ENTITY) {
      tree_set_ref(unit, e);

      insert_names_from_context(nametab, e);

      if (entity_name != arch_name)
         insert_name(nametab, e, entity_name, 0);
   }
   else
      e = NULL;   // TODO: raise error here?

   char *LOCAL prefix = xasprintf("%s(%s)", istr(qual), istr(arch_name));
   scope_set_prefix(nametab, ident_new(prefix));

   insert_name(nametab, unit, NULL, 0);

   push_scope(nametab);

   if (e != NULL) {
      insert_generics(nametab, e);
      insert_ports(nametab, e);
      insert_decls(nametab, e);
   }

   p_architecture_declarative_part(unit);

   consume(tBEGIN);

   p_architecture_statement_part(unit);

   consume(tEND);
   optional(tARCHITECTURE);
   p_trailing_label(tree_ident(unit));
   consume(tSEMI);

   resolve_specs(nametab, unit);

   pop_scope(nametab);
   pop_scope(nametab);

   tree_set_loc(unit, CURRENT_LOC);

   // Prefix the architecture with the entity name
   tree_set_ident(unit, ident_prefix(tree_ident2(unit),
                                     tree_ident(unit), '-'));
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
         push_scope(nametab);
         tree_t spec = p_subprogram_specification();
         if (peek() == tSEMI)
            tree_add_decl(parent, p_subprogram_declaration(spec));
         else
            tree_add_decl(parent, p_subprogram_body(spec));
         pop_scope(nametab);
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
      p_type_declaration(parent);
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

   ident_t name = p_identifier();

   tree_change_kind(unit, T_PACK_BODY);
   tree_set_ident(unit, ident_prefix(name, ident_new("body"), '-'));

   push_scope(nametab);

   ident_t qual = ident_prefix(lib_name(lib_work()), name, '.');
   tree_t pack = find_unit(CURRENT_LOC, qual);
   if (pack != NULL && tree_kind(pack) != T_PACKAGE) {
      parse_error(CURRENT_LOC, "unit %s is not a package", istr(qual));
      pack = NULL;
   }
   else if (pack != NULL)
      insert_names_from_context(nametab, pack);

   scope_set_prefix(nametab, qual);
   insert_name(nametab, unit, name, 0);

   consume(tIS);

   push_scope(nametab);

   if (pack != NULL)
      insert_decls(nametab, pack);

   p_package_body_declarative_part(unit);

   pop_scope(nametab);
   pop_scope(nametab);

   consume(tEND);

   if (optional(tPACKAGE))
      consume(tBODY);

   p_trailing_label(name);
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

   push_scope(nametab);

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

   if (bootstrapping && unit != find_std(nametab))
      parse_error(tree_loc(unit), "--bootstrap must only be used with "
                  "STANDARD package");


   pop_scope(nametab);
}

static tree_t p_design_unit(void)
{
   BEGIN("design unit");

   push_scope(nametab);

   tree_t unit = tree_new(T_DESIGN_UNIT);
   scope_set_unit(nametab, unit);

   tree_t std = tree_new(T_LIBRARY);
   tree_set_ident(std, std_i);
   tree_add_context(unit, std);
   insert_name(nametab, std, std_i, 0);

   tree_t work = tree_new(T_LIBRARY);
   tree_set_ident(work, lib_name(lib_work()));
   tree_add_context(unit, work);
   insert_name(nametab, work, work_i, 0);
   insert_name(nametab, work, NULL, 0);

   // The std.standard package is implicit unless we are bootstrapping
   if (!bootstrapping) {
      tree_t u = tree_new(T_USE);
      tree_set_ident(u, std_standard_i);
      tree_set_ident2(u, all_i);

      tree_add_context(unit, u);
      insert_names_from_use(nametab, u);
   }

   p_context_clause(unit);
   p_library_unit(unit);

   pop_scope(nametab);

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

   yylloc = get_loc(MIN(n_row, LINE_INVALID),
                    MIN(n_token_start, COLUMN_INVALID),
                    MIN(n_row, LINE_INVALID),
                    MIN(last_col, COLUMN_INVALID),
                    file_ref);
}

int get_next_char(char *b, int max_buffer)
{
   if (last_was_newline) {
      n_row += 1;
      last_was_newline = false;
   }

   const bool eof = read_ptr >= file_start + file_sz;
   if (eof)
      return 0;
   else
      *b = *read_ptr++;

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
   file_ref           = loc_file_ref(file, file_start);
   n_row              = 0;
   n_token_next_start = 0;
   translate_on       = true;
   parse_pragmas      = opt_get_int("parse-pragmas");
   bootstrapping      = opt_get_int("bootstrap");

   if (tokenq == NULL) {
      tokenq_sz = 128;
      tokenq = xmalloc(tokenq_sz * sizeof(tokenq_t));
   }

   tokenq_head = tokenq_tail = 0;
}

tree_t parse(void)
{
   n_correct = RECOVER_THRESH;

   if (peek() == tEOF)
      return NULL;

   nametab = nametab_new();

   tree_t unit = p_design_unit();

   while (cond_state != NULL) {
      cond_state_t *tmp = cond_state->next;
      free(cond_state);
      cond_state = tmp;
   }

   if (tree_kind(unit) == T_DESIGN_UNIT)
      return NULL;

   ident_t qual = ident_prefix(lib_name(lib_work()), tree_ident(unit), '.');
   tree_set_ident(unit, qual);
   lib_put(lib_work(), unit);

   return unit;
}
