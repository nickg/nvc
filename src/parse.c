//
//  Copyright (C) 2014-2023  Nick Gasson
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
#include "common.h"
#include "diag.h"
#include "hash.h"
#include "lib.h"
#include "names.h"
#include "object.h"
#include "option.h"
#include "phase.h"
#include "psl/psl-node.h"
#include "psl/psl-phase.h"
#include "scan.h"
#include "thread.h"
#include "tree.h"
#include "type.h"

#include <string.h>
#include <stdarg.h>
#include <ctype.h>
#include <assert.h>
#include <stdbool.h>
#include <stdlib.h>
#include <limits.h>
#include <float.h>
#include <inttypes.h>

typedef struct {
   token_t  token;
   yylval_t lval;
   loc_t    loc;
} tokenq_t;

typedef bool (*look_fn_t)(token_t);

typedef struct {
   token_t   look[4];
   token_t   stop[4];
   token_t   abort;
   token_t   nest_in;
   token_t   nest_out;
   look_fn_t lookfn;
   int       depth;
} look_params_t;

typedef A(tree_t) tree_list_t;
typedef A(type_t) type_list_t;

typedef struct _ident_list ident_list_t;

struct _ident_list {
   ident_list_t *next;
   ident_t       ident;
   loc_t         loc;
};

#define LOCAL_IDENT_LIST \
   __attribute__((cleanup(_ident_list_cleanup))) ident_list_t *

static loc_t          start_loc;
static loc_t          last_loc;
static const char    *hint_str = NULL;
static int            n_correct = 0;
static tokenq_t      *tokenq;
static int            tokenq_sz;
static int            tokenq_head;
static int            tokenq_tail;
static yylval_t       last_lval;
static token_t        opt_hist[8];
static int            nopt_hist = 0;
static nametab_t     *nametab = NULL;
static bool           bootstrapping = false;
static tree_list_t    pragmas = AINIT;

extern loc_t yylloc;

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

#define BEGIN_WITH_HEAD(s, t)                           \
   EXTEND(s);                                           \
   start_loc = (t) ? *tree_loc(t) : LOC_INVALID;        \

#define BEGIN(s)  BEGIN_WITH_HEAD(s, NULL)

#define CURRENT_LOC _diff_loc(&start_loc, &last_loc)

static tree_t p_expression(void);
static tree_t p_sequential_statement(void);
static tree_t p_concurrent_statement(void);
static tree_t p_package_declaration(tree_t unit);
static tree_t p_package_body(tree_t unit);
static tree_t p_subprogram_declaration(tree_t spec);
static tree_t p_subprogram_body(tree_t spec);
static tree_t p_subprogram_specification(void);
static tree_t p_name(name_mask_t stop_mask);
static tree_t p_block_configuration(tree_t of);
static tree_t p_protected_type_body(ident_t id);
static type_t p_signature(void);
static type_t p_type_mark(void);
static tree_t p_function_call(ident_t id, tree_t prefix);
static tree_t p_resolution_indication(void);
static void p_conditional_waveforms(tree_t stmt, tree_t target, tree_t s0);
static void p_generic_map_aspect(tree_t inst, tree_t unit);
static ident_t p_designator(void);
static void p_interface_list(tree_t parent, tree_kind_t kind, bool ordered);
static void p_trailing_label(ident_t label);
static tree_t p_condition(void);
static type_t p_subtype_indication(void);
static tree_t p_record_constraint(type_t base);
static tree_t p_qualified_expression(tree_t prefix);
static tree_t p_concurrent_procedure_call_statement(ident_t label, tree_t name);
static tree_t p_subprogram_instantiation_declaration(void);
static tree_t p_record_element_constraint(type_t base);
static void p_selected_waveforms(tree_t stmt, tree_t target, tree_t reject);
static type_t p_index_subtype_definition(void);
static type_t p_anonymous_type_indication(void);
static void p_alias_declaration(tree_t parent);
static void p_variable_declaration(tree_t parent);
static tree_t p_psl_declaration(void);
static psl_node_t p_psl_sequence(void);
static psl_node_t p_psl_property(void);
static tree_t p_psl_builtin_function_call(void);
static psl_node_t p_psl_sere(void);

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

static void skip_pragma(pragma_kind_t kind)
{
   if (nametab == NULL)
      warn_at(&yylloc, "ignoring pragma outside of design unit");
   else  {
      tree_t p = tree_new(T_PRAGMA);
      tree_set_loc(p, &yylloc);
      tree_set_subkind(p, kind);

      APUSH(pragmas, p);
   }
}

static token_t wrapped_yylex(void)
{
   for (;;) {
      const token_t token = processed_yylex();
      switch (token) {
      case tSYNTHON:
         skip_pragma(PRAGMA_SYNTHESIS_ON);
         break;
      case tSYNTHOFF:
         skip_pragma(PRAGMA_SYNTHESIS_OFF);
         break;
      case tCOVERAGEON:
         skip_pragma(PRAGMA_COVERAGE_ON);
         break;
      case tCOVERAGEOFF:
         skip_pragma(PRAGMA_COVERAGE_OFF);
         break;
      case tTRANSLATEON:
         skip_pragma(PRAGMA_TRANSLATE_ON);
         break;
      case tTRANSLATEOFF:
         skip_pragma(PRAGMA_TRANSLATE_OFF);
         break;
      default:
         return token;
      }
   }
}

static token_t peek_nth(int n)
{
   while (((tokenq_head - tokenq_tail) & (tokenq_sz - 1)) < n) {
      const token_t token = wrapped_yylex();

      int next = (tokenq_head + 1) & (tokenq_sz - 1);
      if (unlikely(next == tokenq_tail)) {
         const int newsz = tokenq_sz * 2;
         tokenq_t *new = xmalloc_array(newsz, sizeof(tokenq_t));

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

static ident_t peek_ident(void)
{
   if (peek() != tID)
      return NULL;

   return tokenq[tokenq_tail].lval.ident;
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

         if (params->lookfn != NULL && (*params->lookfn)(tok)) {
            found = true;
            goto stop_looking;
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
   token_t next = tEOF;
   do {
      free_token(tok, &last_lval);
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
   if (n_correct >= RECOVER_THRESH) {
      diag_t *d = diag_new(DIAG_ERROR, &(tokenq[tokenq_tail].loc));
      diag_printf(d, "unexpected $yellow$%s$$ while parsing %s, expecting ",
                  token_str(peek()), hint_str);

      bool first = true;
      for (int i = 0; i < nopt_hist; i++) {
         diag_printf(d, "%s$yellow$%s$$", i == 0 ? "one of " : ", ",
                     token_str(opt_hist[i]));
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

      diag_hint(d, &(tokenq[tokenq_tail].loc), "this token was unexpected");
      diag_emit(d);
   }

   n_correct = 0;

   drop_token();
   suppress_errors(nametab);
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

static ident_t error_marker(void)
{
   return well_known(W_ERROR);
}

static tree_t error_expr(void)
{
   tree_t t = tree_new(T_REF);
   tree_set_ident(t, error_marker());
   tree_set_type(t, type_new(T_NONE));
   return t;
}

static tree_t find_binding(tree_t inst)
{
   ident_t name;
   tree_t unit = NULL;
   if (tree_kind(inst) == T_BINDING) {
      name = tree_ident(inst);
      if (tree_has_ident2(inst))
         name = ident_prefix(name, tree_ident2(inst), '-');
   }
   else {
      name = tree_ident2(inst);
      if (tree_has_ref(inst))
         unit = tree_ref(inst);
   }

   if (unit == NULL)
      unit = resolve_name(nametab, tree_loc(inst), name);

   if (unit == NULL)
      return NULL;

   const char *what = is_design_unit(unit) ? "design unit" : "object";
   const tree_kind_t kind = tree_kind(unit);
   switch (tree_class(inst)) {
   case C_COMPONENT:
      if (kind != T_COMPONENT) {
         parse_error(tree_loc(inst), "%s %s is not a component declaration",
                     what, istr(name));
         return NULL;
      }
      break;
   case C_ENTITY:
      if (kind != T_ENTITY && kind != T_ARCH) {
         parse_error(tree_loc(inst), "%s %s is not an entity",
                     what, istr(name));
         return NULL;
      }
      break;
   case C_CONFIGURATION:
      if (kind != T_CONFIGURATION) {
         parse_error(tree_loc(inst), "%s %s is not a configuration",
                     what, istr(name));
         return NULL;
      }
      break;
   default:
      break;
   }

   return unit;
}

static void set_label_and_loc(tree_t t, ident_t label, const loc_t *loc)
{
   tree_set_loc(t, loc);

   if (label == NULL)
      label = get_implicit_label(t, nametab);

   tree_set_ident(t, label);
}

static void require_std(vhdl_standard_t which, const char *feature)
{
   static bool warned = false;

   if (standard() < which && !warned) {
      warned = true;

      if (n_correct >= RECOVER_THRESH) {
         diag_t *d = diag_new(DIAG_ERROR, CURRENT_LOC);
         diag_printf(d, "%s %s not supported in VHDL-%s",
                     feature, feature[strlen(feature)-1] == 's' ? "are" : "is",
                     standard_text(standard()));
         diag_hint(d, NULL, "pass $bold$--std=%s$$ to enable this feature",
                   standard_text(which));
         diag_emit(d);
      }
   }
}

static tree_t bit_string_to_literal(const char *str, const loc_t *loc)
{
   tree_t t = tree_new(T_STRING);
   tree_set_loc(t, loc);

   const char *p = str;
   int length = -1;

   if (isdigit_iso88591(*p)) {
      require_std(STD_08, "bit string literals with length specifier");
      length = strtoul(p, (char **)&p, 10);
   }

   enum { UNSIGNED, SIGNED } mode = UNSIGNED;

   switch (*p) {
   case 'U': case 'u': mode = UNSIGNED; ++p; break;
   case 'S': case 's': mode = SIGNED; ++p; break;
   }

   char base_ch = *p++;
   int base;
   switch (base_ch) {
   case 'X': case 'x': base = 16; break;
   case 'O': case 'o': base = 8;  break;
   case 'B': case 'b': base = 2;  break;
   case 'D': case 'd': base = 10; break;
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

   if (base == 10) {
      require_std(STD_08, "decimal bit string literals");

      int ddigits = 0;
      uint8_t *decimal LOCAL = xmalloc(strlen(str));

      for (++p; *p != '\"'; p++) {
         if (*p == '_')
            continue;
         else if (!isdigit_iso88591(*p)) {
            parse_error(loc, "invalid digit '%c' in decimal bit string", *p);
            return t;
         }
         else
            decimal[ddigits++] = *p - '0';
      }

      const int maxbits = (length == -1 ? ddigits * 4 : length);
      tree_t *bits LOCAL = xmalloc_array(maxbits, sizeof(tree_t));
      int pos = maxbits - 1;

      for (;;) {
         bool all_zero = true;
         int cout = 0;
         for (int i = 0, cin = 0; i < ddigits; i++, cin = cout) {
            all_zero &= (decimal[i] == 0);
            cout = decimal[i] & 1;
            decimal[i] >>= 1;
            if (cin) decimal[i] += 5;
         }

         if (all_zero)
            break;
         else if (pos < 0 && cout) {
            parse_error(CURRENT_LOC, "excess non-zero digits in "
                        "decimal bit string literal");
            return t;
         }
         else if (pos >= 0)
            bits[pos--] = cout ? one : zero;
      }

      if (length == -1)
         length = maxbits - pos - 1;

      for (int i = maxbits - length; i < maxbits; i++)
         tree_add_char(t, i <= pos ? zero : bits[i]);

      return t;
   }

   tree_t *bits LOCAL = NULL;
   if (length >= 0)
      bits = xmalloc_array(length, sizeof(tree_t));

   tree_t pad = mode == UNSIGNED ? zero : NULL;
   int nbits = 0;
   for (++p; *p != '\"'; p++) {
      if (*p == '_')
         continue;

      const bool extended = (isdigit_iso88591(*p) && *p < '0' + base)
         || (base > 10 && *p >= 'A' && *p < 'A' + base - 10)
         || (base > 10 && *p >= 'a' && *p < 'a' + base - 10);

      int n = (isdigit_iso88591(*p) ? (*p - '0')
               : 10 + (isupper_iso88591(*p) ? (*p - 'A') : (*p - 'a')));
      tree_t digit = NULL;

      if (!extended) {
         if (standard() < STD_08 || !isprint_iso88591(*p)) {
            parse_error(loc, "invalid digit '%c' in bit string", *p);
            return t;
         }
         else {
            const char rune[] = { '\'', *p, '\'', '\0' };
            digit = tree_new(T_REF);
            tree_set_ident(digit, ident_new(rune));
            tree_set_loc(digit, loc);
         }
      }

      for (int d = (base >> 1); d > 0; n = n % d, d >>= 1) {
         tree_t bit = extended ? ((n / d) ? one : zero) : digit;
         if (pad == NULL) pad = bit;
         if (length >= 0) {
            tree_t left = nbits == 0 ? bit : bits[0];
            if (nbits < length)
               bits[nbits++] = bit;
            else if (left != pad && tree_ident(left) != tree_ident(pad)) {
               parse_error(CURRENT_LOC, "excess %s digits in bit "
                           "string literal",
                           mode == SIGNED ? "significant" : "non-zero");
               return t;
            }
            else if (length > 0) {
               for (int i = 0; i < length - 1; i++)
                  bits[i] = bits[i + 1];
               bits[length - 1] = bit;
            }
         }
         else
            tree_add_char(t, bit);
      }
   }

   if (pad == NULL && nbits < length)
      parse_error(CURRENT_LOC, "signed bit string literal cannot be an "
                  "empty string");
   else if (length >= 0) {
      // Left-pad with sign bit or zero
      int pos = 0;
      for (; pos < length - nbits; pos++)
         tree_add_char(t, pad);
      for (int i = 0; pos < length; pos++, i++)
         tree_add_char(t, bits[i]);
   }

   return t;
}

static tree_t get_time(int64_t fs, const loc_t *loc)
{
   tree_t lit = tree_new(T_LITERAL);
   tree_set_subkind(lit, L_INT);
   tree_set_ival(lit, fs);
   tree_set_loc(lit, loc);
   tree_set_type(lit, std_type(NULL, STD_TIME));

   return lit;
}

static void set_delay_mechanism(tree_t t, tree_t reject)
{
   if (reject == NULL) {
      // Inertial delay with same value as waveform
      // LRM 93 section 8.4 the rejection limit in this case is
      // specified by the time expression of the first waveform
      tree_t assign = (tree_kind(t) == T_COND_ASSIGN ? tree_cond(t, 0) : t);
      if (tree_waveforms(assign) == 0)
         return;

      tree_t w = tree_waveform(assign, 0);
      if (tree_has_delay(w))
         tree_set_reject(t, tree_delay(w));
   }
   else {
      tree_set_reject(t, reject);
      solve_types(nametab, reject, std_type(NULL, STD_TIME));
   }
}

static tree_t add_port(tree_t d, const char *name, type_t type,
                       port_mode_t mode, tree_t def)
{
   type_t ftype = tree_type(d);

   tree_t port = tree_new(T_PORT_DECL);
   tree_set_ident(port, ident_new(name));
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

static tree_t builtin_proc(ident_t name, subprogram_kind_t kind, ...)
{
   type_t f = type_new(T_PROC);
   type_set_ident(f, name);

   tree_t d = tree_new(T_PROC_DECL);
   tree_set_ident(d, name);
   tree_set_type(d, f);
   tree_set_subkind(d, kind);
   tree_set_flag(d, TREE_F_NEVER_WAITS);

   tree_set_flag(d, TREE_F_PREDEFINED);
   tree_set_loc(d, CURRENT_LOC);
   return d;
}

static tree_t builtin_fn(ident_t name, type_t result,
                         subprogram_kind_t kind, ...)
{
   type_t f = type_new(T_FUNC);
   type_set_ident(f, name);
   type_set_result(f, result);

   tree_t d = tree_new(T_FUNC_DECL);
   tree_set_ident(d, name);
   tree_set_type(d, f);
   tree_set_subkind(d, kind);

   va_list ap;
   va_start(ap, kind);
   char *argname;
   while ((argname = va_arg(ap, char*))) {
      type_t type = va_arg(ap, type_t);
      assert(type != NULL);
      add_port(d, argname, type, PORT_IN, NULL);
   }
   va_end(ap);

   tree_set_flag(d, TREE_F_PREDEFINED);
   tree_set_loc(d, CURRENT_LOC);
   return d;
}

static void declare_binary(tree_t container, ident_t name, type_t lhs,
                           type_t rhs, type_t result, subprogram_kind_t kind)
{
   tree_t d = builtin_fn(name, result, kind, "L", lhs, "R", rhs, NULL);
   mangle_func(nametab, d);
   insert_name(nametab, d, NULL);
   tree_add_decl(container, d);

   if (bootstrapping && type_is_universal(lhs))
      tree_set_flag(d, TREE_F_UNIVERSAL);
}

static void declare_unary(tree_t container, ident_t name, type_t operand,
                          type_t result, subprogram_kind_t kind)
{
   tree_t d = builtin_fn(name, result, kind, "VALUE", operand, NULL);
   mangle_func(nametab, d);
   insert_name(nametab, d, NULL);
   tree_add_decl(container, d);

   if (bootstrapping && type_is_universal(operand))
      tree_set_flag(d, TREE_F_UNIVERSAL);
}

static bool is_bit_or_std_ulogic(type_t type)
{
   if (!type_is_enum(type))
      return false;

   ident_t name = type_ident(type);

   return name == well_known(W_STD_BIT) || name == well_known(W_IEEE_ULOGIC);
}

static void declare_predefined_ops(tree_t container, type_t t)
{
   // Prefined operators are defined in LRM 93 section 7.2

   ident_t mult   = ident_new("\"*\"");
   ident_t div    = ident_new("\"/\"");
   ident_t plus   = ident_new("\"+\"");
   ident_t minus  = ident_new("\"-\"");
   ident_t cmp_lt = ident_new("\"<\"");
   ident_t cmp_le = ident_new("\"<=\"");
   ident_t cmp_gt = ident_new("\">\"");
   ident_t cmp_ge = ident_new("\">=\"");
   ident_t eq     = ident_new("\"=\"");
   ident_t neq    = ident_new("\"/=\"");

   ident_t min_i = NULL, max_i = NULL;
   if (standard() >= STD_08) {
      min_i = ident_new("MINIMUM");
      max_i = ident_new("MAXIMUM");
   }

   // Predefined operators

   tree_t std = find_std(nametab);

   type_t std_bool = std_type(std, STD_BOOLEAN);
   type_t std_int  = NULL;
   type_t std_real = NULL;
   type_t std_uint = NULL;

   type_kind_t kind = type_kind(t);

   switch (kind) {
   case T_SUBTYPE:
      // Use operators of base type
      break;

   case T_ARRAY:
      // Operators on arrays
      declare_binary(container, eq, t, t, std_bool, S_ARRAY_EQ);
      declare_binary(container, neq, t, t, std_bool, S_ARRAY_NEQ);
      if (dimension_of(t) == 1) {
         type_t elem = type_elem(t);
         const bool ordered = (standard() >= STD_19)
            ? type_is_scalar(elem) : type_is_discrete(elem);

         if (ordered) {
            declare_binary(container, cmp_lt, t, t, std_bool, S_ARRAY_LT);
            declare_binary(container, cmp_le, t, t, std_bool, S_ARRAY_LE);
            declare_binary(container, cmp_gt, t, t, std_bool, S_ARRAY_GT);
            declare_binary(container, cmp_ge, t, t, std_bool, S_ARRAY_GE);
         }

         ident_t concat = ident_new("\"&\"");
         declare_binary(container, concat, t, t, t, S_CONCAT);
         declare_binary(container, concat, t, elem, t, S_CONCAT);
         declare_binary(container, concat, elem, t, t, S_CONCAT);
         declare_binary(container, concat, elem, elem, t, S_CONCAT);

         if (standard() >= STD_08) {
            if (ordered) {
               declare_binary(container, min_i, t, t, t, S_MINIMUM);
               declare_binary(container, max_i, t, t, t, S_MAXIMUM);
            }

            if (type_is_scalar(elem)) {
               declare_unary(container, min_i, t, elem, S_MINIMUM);
               declare_unary(container, max_i, t, elem, S_MAXIMUM);
            }
         }
      }
      break;

   case T_RECORD:
      // Operators on records
      declare_binary(container, eq, t, t, std_bool, S_RECORD_EQ);
      declare_binary(container, neq, t, t, std_bool, S_RECORD_NEQ);
      break;

   case T_PHYSICAL:
      std_int  = std_type(std, STD_INTEGER);
      std_real = std_type(std, STD_REAL);
      std_uint = std_type(std, STD_UNIVERSAL_INTEGER);

      // Multiplication
      declare_binary(container, mult, t, std_int, t, S_MUL);
      declare_binary(container, mult, t, std_real, t, S_MUL_PR);
      declare_binary(container, mult, std_int, t, t, S_MUL);
      declare_binary(container, mult, std_real, t, t, S_MUL_RP);

      // Division
      declare_binary(container, div, t, std_int, t, S_DIV);
      declare_binary(container, div, t, std_real, t, S_DIV_PR);
      declare_binary(container, div, t, t, std_uint, S_DIV);

      // Addition
      declare_binary(container, plus, t, t, t, S_ADD);

      // Subtraction
      declare_binary(container, minus, t, t, t, S_SUB);

      // Sign operators
      declare_unary(container, plus, t, t, S_IDENTITY);
      declare_unary(container, minus, t, t, S_NEGATE);

      // Comparison
      declare_binary(container, cmp_lt, t, t, std_bool, S_SCALAR_LT);
      declare_binary(container, cmp_le, t, t, std_bool, S_SCALAR_LE);
      declare_binary(container, cmp_gt, t, t, std_bool, S_SCALAR_GT);
      declare_binary(container, cmp_ge, t, t, std_bool, S_SCALAR_GE);

      // Equality
      declare_binary(container, eq, t, t, std_bool, S_SCALAR_EQ);
      declare_binary(container, neq, t, t, std_bool, S_SCALAR_NEQ);

      // Absolute value
      declare_unary(container, ident_new("\"abs\""), t, t, S_ABS);

      if (standard() >= STD_08) {
         declare_binary(container, min_i, t, t, t, S_MINIMUM);
         declare_binary(container, max_i, t, t, t, S_MAXIMUM);

         // Modulus and remainder in 2008 only
         declare_binary(container, ident_new("\"mod\""), t, t, t, S_MOD);
         declare_binary(container, ident_new("\"rem\""), t, t, t, S_REM);
      }

      break;

   case T_INTEGER:
      // Modulus
      declare_binary(container, ident_new("\"mod\""), t, t, t, S_MOD);

      // Remainder
      declare_binary(container, ident_new("\"rem\""), t, t, t, S_REM);

      // Fall-through
   case T_REAL:
      // Addition
      declare_binary(container, plus, t, t, t, S_ADD);

      // Subtraction
      declare_binary(container, minus, t, t, t, S_SUB);

      // Multiplication
      declare_binary(container, mult, t, t, t, S_MUL);

      // Division
      declare_binary(container, div, t, t, t, S_DIV);

      // Sign operators
      declare_unary(container, plus, t, t, S_IDENTITY);
      declare_unary(container, minus, t, t, S_NEGATE);

      // Exponentiation
      if (!bootstrapping) {
         std_int = std_type(std, STD_INTEGER);
         declare_binary(container, ident_new("\"**\""), t, std_int, t, S_EXP);
      }

      // Absolute value
      declare_unary(container, ident_new("\"abs\""), t, t, S_ABS);

      // Fall-through
   case T_ENUM:
      declare_binary(container, cmp_lt, t, t, std_bool, S_SCALAR_LT);
      declare_binary(container, cmp_le, t, t, std_bool, S_SCALAR_LE);
      declare_binary(container, cmp_gt, t, t, std_bool, S_SCALAR_GT);
      declare_binary(container, cmp_ge, t, t, std_bool, S_SCALAR_GE);

      if (standard() >= STD_08) {
         declare_binary(container, min_i, t, t, t, S_MINIMUM);
         declare_binary(container, max_i, t, t, t, S_MAXIMUM);
      }

      // Fall-through
   default:
      declare_binary(container, eq, t, t, std_bool, S_SCALAR_EQ);
      declare_binary(container, neq, t, t, std_bool, S_SCALAR_NEQ);

      break;
   }

   if (standard() >= STD_08 && !bootstrapping && type_is_representable(t)) {
      // The TO_STRING operators in STD.STANDARD are declared at
      // the end of the package according to LRM 08 section 5.2.6
      declare_unary(container, ident_new("TO_STRING"), t,
                    std_type(NULL, STD_STRING), S_TO_STRING);
   }

   // Universal integers and reals have some additional overloaded operators
   // that are not valid for regular integer and real types
   // See LRM 93 section 7.5

   if (bootstrapping && type_kind(t) == T_REAL
       && t == std_type(std, STD_UNIVERSAL_REAL)) {
      type_t uint = std_type(std, STD_UNIVERSAL_INTEGER);

      ident_t mult = ident_new("\"*\"");
      ident_t div  = ident_new("\"/\"");

      declare_binary(container, mult, t, uint, t, S_MUL_RI);
      declare_binary(container, mult, uint, t, t, S_MUL_IR);
      declare_binary(container, div, t, uint, t, S_DIV_RI);
   }

   // Matching comparison for BIT and STD_ULOGIC

   if (standard() >= STD_08) {
      if (kind == T_ARRAY) {
         type_t elem = type_elem(t);
         if (is_bit_or_std_ulogic(elem)) {
            declare_binary(container, ident_new("\"?=\""),
                           t, t, elem, S_MATCH_EQ);
            declare_binary(container, ident_new("\"?/=\""),
                           t, t, elem, S_MATCH_NEQ);
         }
      }
      else if (is_bit_or_std_ulogic(t)) {
         declare_binary(container, ident_new("\"?=\""), t, t, t, S_MATCH_EQ);
         declare_binary(container, ident_new("\"?/=\""), t, t, t, S_MATCH_NEQ);
         declare_binary(container, ident_new("\"?<\""), t, t, t, S_MATCH_LT);
         declare_binary(container, ident_new("\"?<=\""), t, t, t, S_MATCH_LE);
         declare_binary(container, ident_new("\"?>\""), t, t, t, S_MATCH_GT);
         declare_binary(container, ident_new("\"?>=\""), t, t, t, S_MATCH_GE);
      }
   }

   // Logical operators

   if (bootstrapping && (t == std_bool || t == std_type(std, STD_BIT))) {
      declare_binary(container, ident_new("\"and\""), t, t, t, S_SCALAR_AND);
      declare_binary(container, ident_new("\"or\""), t, t, t, S_SCALAR_OR);
      declare_binary(container, ident_new("\"xor\""), t, t, t, S_SCALAR_XOR);
      declare_binary(container, ident_new("\"nand\""), t, t, t, S_SCALAR_NAND);
      declare_binary(container, ident_new("\"nor\""), t, t, t, S_SCALAR_NOR);
      declare_binary(container, ident_new("\"xnor\""), t, t, t, S_SCALAR_XNOR);
      declare_unary(container, ident_new("\"not\""), t, t, S_SCALAR_NOT);
   }

   bool vec_logical = false;
   if (kind == T_ARRAY && dimension_of(t) == 1) {
      type_t base = type_elem(t);
      vec_logical = (base == std_bool || base == std_type(NULL, STD_BIT));
   }

   if (vec_logical) {
      std_int = std_type(NULL, STD_INTEGER);

      ident_t and  = ident_new("\"and\"");
      ident_t or   = ident_new("\"or\"");
      ident_t xor  = ident_new("\"xor\"");
      ident_t nand = ident_new("\"nand\"");
      ident_t nor  = ident_new("\"nor\"");
      ident_t xnor = ident_new("\"xnor\"");

      declare_binary(container, and, t, t, t, S_ARRAY_AND);
      declare_binary(container, or, t, t, t, S_ARRAY_OR);
      declare_binary(container, xor, t, t, t, S_ARRAY_XOR);
      declare_binary(container, nand, t, t, t, S_ARRAY_NAND);
      declare_binary(container, nor, t, t, t, S_ARRAY_NOR);
      declare_binary(container, xnor, t, t, t, S_ARRAY_XNOR);

      declare_unary(container, ident_new("\"not\""), t, t, S_ARRAY_NOT);

      declare_binary(container, ident_new("\"sll\""), t, std_int, t, S_SLL);
      declare_binary(container, ident_new("\"srl\""), t, std_int, t, S_SRL);
      declare_binary(container, ident_new("\"sla\""), t, std_int, t, S_SLA);
      declare_binary(container, ident_new("\"sra\""), t, std_int, t, S_SRA);
      declare_binary(container, ident_new("\"rol\""), t, std_int, t, S_ROL);
      declare_binary(container, ident_new("\"ror\""), t, std_int, t, S_ROR);

      if (standard() >= STD_08) {
         type_t e = type_elem(t);

         declare_unary(container, and, t, e, S_REDUCE_AND);
         declare_unary(container, or, t, e, S_REDUCE_OR);
         declare_unary(container, xor, t, e, S_REDUCE_XOR);
         declare_unary(container, nand, t, e, S_REDUCE_NAND);
         declare_unary(container, nor, t, e, S_REDUCE_NOR);
         declare_unary(container, xnor, t, e, S_REDUCE_XNOR);

         declare_binary(container, and, t, e, t, S_MIXED_AND);
         declare_binary(container, or, t, e, t, S_MIXED_OR);
         declare_binary(container, xor, t, e, t, S_MIXED_XOR);
         declare_binary(container, nand, t, e, t, S_MIXED_NAND);
         declare_binary(container, nor, t, e, t, S_MIXED_NOR);
         declare_binary(container, xnor, t, e, t, S_MIXED_XNOR);

         declare_binary(container, and, e, t, t, S_MIXED_AND);
         declare_binary(container, or, e, t, t, S_MIXED_OR);
         declare_binary(container, xor, e, t, t, S_MIXED_XOR);
         declare_binary(container, nand, e, t, t, S_MIXED_NAND);
         declare_binary(container, nor, e, t, t, S_MIXED_NOR);
         declare_binary(container, xnor, e, t, t, S_MIXED_XNOR);
      }
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

         type_t open_kind   = std_type(NULL, STD_FILE_OPEN_KIND);
         type_t open_status = std_type(NULL, STD_FILE_OPEN_STATUS);
         type_t std_string  = std_type(NULL, STD_STRING);

         tree_t file_open1 = builtin_proc(file_open_i, S_FILE_OPEN1);
         add_port(file_open1, "F", t, PORT_INOUT, NULL);
         add_port(file_open1, "EXTERNAL_NAME", std_string, PORT_IN, NULL);
         add_port(file_open1, "OPEN_KIND", open_kind, PORT_IN,
                  make_ref(read_mode));
         insert_name(nametab, file_open1, file_open_i);
         tree_add_decl(container, file_open1);

         tree_t file_open2 = builtin_proc(file_open_i, S_FILE_OPEN2);
         add_port(file_open2, "STATUS", open_status, PORT_OUT, NULL);
         add_port(file_open2, "F", t, PORT_INOUT, NULL);
         add_port(file_open2, "EXTERNAL_NAME", std_string, PORT_IN, NULL);
         add_port(file_open2, "OPEN_KIND", open_kind, PORT_IN,
                  make_ref(read_mode));
         insert_name(nametab, file_open2, file_open_i);
         tree_add_decl(container, file_open2);

         tree_t file_close = builtin_proc(file_close_i, S_FILE_CLOSE);
         add_port(file_close, "F", t, PORT_INOUT, NULL);
         insert_name(nametab, file_close, file_close_i);
         tree_add_decl(container, file_close);

         if (standard() >= STD_08) {
            ident_t flush_i = ident_new("FLUSH");

            tree_t flush = builtin_proc(flush_i, S_FOREIGN);
            tree_set_ident2(flush, ident_new("__nvc_flush"));
            add_port(flush, "F", t, PORT_IN, NULL);
            insert_name(nametab, flush, flush_i);
            tree_add_decl(container, flush);
         }

         if (standard() >= STD_19) {
            ident_t rewind_i   = ident_new("FILE_REWIND");
            ident_t seek_i     = ident_new("FILE_SEEK");
            ident_t begin_i    = ident_new("FILE_ORIGIN_BEGIN");
            ident_t truncate_i = ident_new("FILE_TRUNCATE");
            ident_t state_i    = ident_new("FILE_STATE");
            ident_t mode_i     = ident_new("FILE_MODE");
            ident_t position_i = ident_new("FILE_POSITION");
            ident_t size_i     = ident_new("FILE_SIZE");
            ident_t canseek_i  = ident_new("FILE_CANSEEK");

            type_t origin_kind = std_type(NULL, STD_FILE_ORIGIN_KIND);
            type_t open_state = std_type(NULL, STD_FILE_OPEN_STATE);

            tree_t origin_begin = search_decls(std, begin_i, 0);
            assert(origin_begin != NULL);

            tree_t file_open3 = builtin_fn(file_open_i, open_status, S_FOREIGN,
                                           "F", t,
                                           "EXTERNAL_NAME", std_string,
                                           "OPEN_KIND", open_kind,
                                           NULL);
            tree_set_flag(file_open3, TREE_F_IMPURE);
            tree_set_ident2(file_open3, ident_new("__nvc_open3"));
            tree_set_class(tree_port(file_open3, 0), C_FILE);
            tree_set_value(tree_port(file_open3, 2), make_ref(read_mode));
            insert_name(nametab, file_open3, file_open_i);
            tree_add_decl(container, file_open3);

            tree_t rewind = builtin_proc(rewind_i, S_FOREIGN);
            tree_set_ident2(rewind, ident_new("__nvc_rewind"));
            add_port(rewind, "F", t, PORT_IN, NULL);
            insert_name(nametab, rewind, rewind_i);
            tree_add_decl(container, rewind);

            std_int = std_type(std, STD_INTEGER);

            tree_t seek = builtin_proc(seek_i, S_FOREIGN);
            tree_set_ident2(seek, ident_new("__nvc_seek"));
            add_port(seek, "F", t, PORT_IN, NULL);
            add_port(seek, "OFFSET", std_int, PORT_IN, NULL);
            add_port(seek, "ORIGIN", origin_kind, PORT_IN,
                     make_ref(origin_begin));
            insert_name(nametab, seek, seek_i);
            tree_add_decl(container, seek);

            tree_t truncate = builtin_proc(truncate_i, S_FOREIGN);
            tree_set_ident2(truncate, ident_new("__nvc_truncate"));
            add_port(truncate, "F", t, PORT_IN, NULL);
            add_port(truncate, "SIZE", std_int, PORT_IN, NULL);
            add_port(truncate, "ORIGIN", origin_kind, PORT_IN,
                     make_ref(origin_begin));
            insert_name(nametab, truncate, truncate_i);
            tree_add_decl(container, truncate);

            tree_t state = builtin_fn(state_i, open_state, S_FOREIGN,
                                      "F", t, NULL);
            tree_set_ident2(state, ident_new("__nvc_file_state"));
            tree_set_class(tree_port(state, 0), C_FILE);
            insert_name(nametab, state, state_i);
            tree_add_decl(container, state);

            tree_t mode = builtin_fn(mode_i, open_kind, S_FOREIGN,
                                     "F", t, NULL);
            tree_set_ident2(mode, ident_new("__nvc_file_mode"));
            tree_set_class(tree_port(mode, 0), C_FILE);
            insert_name(nametab, mode, mode_i);
            tree_add_decl(container, mode);

            tree_t position = builtin_fn(position_i, std_int, S_FOREIGN,
                                         "F", t, "ORIGIN", origin_kind, NULL);
            tree_set_ident2(position, ident_new("__nvc_file_position"));
            tree_set_class(tree_port(position, 0), C_FILE);
            tree_set_value(tree_port(position, 1), make_ref(origin_begin));
            insert_name(nametab, position, position_i);
            tree_add_decl(container, position);

            tree_t size = builtin_fn(size_i, std_int, S_FOREIGN, "F", t, NULL);
            tree_set_ident2(size, ident_new("__nvc_file_size"));
            tree_set_class(tree_port(size, 0), C_FILE);
            insert_name(nametab, size, size_i);
            tree_add_decl(container, size);

            tree_t canseek = builtin_fn(canseek_i, std_bool, S_FOREIGN,
                                        "F", t, NULL);
            tree_set_ident2(canseek, ident_new("__nvc_file_canseek"));
            tree_set_class(tree_port(canseek, 0), C_FILE);
            insert_name(nametab, canseek, canseek_i);
            tree_add_decl(container, canseek);
         }

         type_t of = type_designated(t);

         tree_t read = builtin_proc(read_i, S_FILE_READ);
         add_port(read, "F", t, PORT_INOUT, NULL);
         add_port(read, "VALUE", of, PORT_OUT, NULL);
         if (type_is_array(of) && type_is_unconstrained(of)) {
            type_t std_nat = std_type(NULL, STD_NATURAL);
            add_port(read, "LENGTH", std_nat, PORT_OUT, NULL);
         }
         insert_name(nametab, read, read_i);
         tree_add_decl(container, read);

         tree_t write = builtin_proc(write_i, S_FILE_WRITE);
         add_port(write, "F", t, PORT_INOUT, NULL);
         add_port(write, "VALUE", of, PORT_IN, NULL);
         insert_name(nametab, write, write_i);
         tree_add_decl(container, write);

         declare_unary(container, endfile_i, t, std_bool, S_ENDFILE);
      }
      break;

   case T_ACCESS:
      {
         ident_t deallocate_i = ident_new("DEALLOCATE");

         tree_t deallocate = builtin_proc(deallocate_i, S_DEALLOCATE);
         add_port(deallocate, "P", t, PORT_INOUT, NULL);

         mangle_func(nametab, deallocate);
         insert_name(nametab, deallocate, deallocate_i);
         tree_add_decl(container, deallocate);
      }
      break;

   default:
      break;
   }

   if (bootstrapping && standard() >= STD_08) {
      // Special predefined operators only declared in STANDARD
      type_t std_bit = NULL;
      if (t == std_bool || t == (std_bit = std_type(NULL, STD_BIT))) {
         tree_t d1 = builtin_fn(ident_new("RISING_EDGE"), std_bool,
                                S_RISING_EDGE, "S", t, NULL);
         tree_set_class(tree_port(d1, 0), C_SIGNAL);
         tree_add_decl(container, d1);

         tree_t d2 = builtin_fn(ident_new("FALLING_EDGE"), std_bool,
                                S_FALLING_EDGE, "S", t, NULL);
         tree_set_class(tree_port(d2, 0), C_SIGNAL);
         tree_add_decl(container, d2);

         if (t == std_bit)
            declare_unary(container, ident_new("\"??\""), t,
                          std_bool, S_IDENTITY);
      }
   }
}

static void declare_alias(tree_t container, tree_t to, ident_t name)
{
   tree_t alias = tree_new(T_ALIAS);
   tree_set_ident(alias, name);
   tree_set_value(alias, make_ref(to));
   tree_set_type(alias, tree_type(to));

   tree_add_decl(container, alias);
}

static void declare_additional_standard_operators(tree_t unit)
{
   assert(bootstrapping);

   // The exponentiation operator must be declared here after INTEGER is
   // declared

   type_t std_uint  = std_type(unit, STD_UNIVERSAL_INTEGER);
   type_t std_ureal = std_type(unit, STD_UNIVERSAL_REAL);
   type_t std_int   = std_type(unit, STD_INTEGER);
   type_t std_real  = std_type(unit, STD_REAL);

   ident_t exp_i = ident_new("\"**\"");

   declare_binary(unit, exp_i, std_uint, std_int, std_uint, S_EXP);
   declare_binary(unit, exp_i, std_ureal, std_int, std_ureal, S_EXP);
   declare_binary(unit, exp_i, std_int, std_int, std_int, S_EXP);
   declare_binary(unit, exp_i, std_real, std_int, std_real, S_EXP);

   if (standard() < STD_08)
      return;

   // LRM 08 5.2.6 says TO_STRING is declared at the end of the STANDARD
   // package

   ident_t to_string   = ident_new("TO_STRING");
   type_t  std_string  = std_type(unit, STD_STRING);
   type_t  std_time    = std_type(unit, STD_TIME);
   type_t  std_natural = std_type(unit, STD_NATURAL);
   type_t  std_bit_vec = std_type(unit, STD_BIT_VECTOR);

   const int ndecls = tree_decls(unit);
   for (int i = 0; i < ndecls; i++) {
      tree_t d = tree_decl(unit, i);
      if (tree_kind(d) == T_TYPE_DECL) {
         type_t type = tree_type(d);
         if (type_is_representable(type))
            declare_unary(unit, to_string, type, std_string, S_TO_STRING);
      }
   }

   // The following special cases are implicitly defined

   tree_t d1 = builtin_fn(to_string, std_string, S_FOREIGN,
                          "VALUE", std_time, "UNIT", std_time, NULL);
   tree_set_ident2(d1, ident_new("_std_to_string_time"));
   tree_add_decl(unit, d1);

   tree_t d2 = builtin_fn(to_string, std_string, S_FOREIGN,
                          "VALUE", std_real, "DIGITS", std_natural, NULL);
   tree_set_ident2(d2, ident_new("_std_to_string_real_digits"));
   tree_add_decl(unit, d2);

   tree_t d3 = builtin_fn(to_string, std_string, S_FOREIGN,
                          "VALUE", std_real, "FORMAT", std_string, NULL);
   tree_set_ident2(d3, ident_new("_std_to_string_real_format"));
   tree_add_decl(unit, d3);

   tree_t d4 = builtin_fn(ident_new("TO_HSTRING"), std_string, S_FOREIGN,
                          "VALUE", std_bit_vec, NULL);
   tree_set_ident2(d4, ident_new("_std_to_hstring_bit_vec"));
   tree_add_decl(unit, d4);

   declare_alias(unit, d4, ident_new("TO_HEX_STRING"));

   tree_t d5 = builtin_fn(ident_new("TO_OSTRING"), std_string, S_FOREIGN,
                          "VALUE", std_bit_vec, NULL);
   tree_set_ident2(d5, ident_new("_std_to_ostring_bit_vec"));
   tree_add_decl(unit, d5);

   declare_alias(unit, d5, ident_new("TO_OCTAL_STRING"));

   tree_t d6;
   for (int n = 0; (d6 = search_decls(unit, to_string, n)); n++) {
      if (type_eq(tree_type(tree_port(d6, 0)), std_bit_vec))
         break;
   }

   assert(d6 != NULL);
   declare_alias(unit, d6, ident_new("TO_BSTRING"));
   declare_alias(unit, d6, ident_new("TO_BINARY_STRING"));
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

static tree_t implicit_dereference(tree_t t)
{
   type_t access = type_designated(tree_type(t));

   tree_t all = tree_new(T_ALL);
   tree_set_loc(all, tree_loc(t));
   tree_set_value(all, t);
   tree_set_type(all, access);

   return all;
}

static type_t prefix_type(tree_t prefix)
{
   if (scope_formal_kind(nametab) == F_SUBPROGRAM)
      return NULL;

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
         return is_type_decl(tree_ref(t));
      else
         return !!(query_name(nametab, tree_ident(t), NULL) & N_TYPE);

   case T_ATTR_REF:
      {
         const attr_kind_t predef = tree_subkind(t);
         return predef == ATTR_RANGE || predef == ATTR_REVERSE_RANGE;
      }

   default:
      return false;
   }
}

static tree_t ensure_labelled(tree_t t, ident_t label)
{
   tree_set_ident(t, label ?: get_implicit_label(t, nametab));
   return t;
}

static tree_t external_reference(tree_t t)
{
   switch (tree_kind(t)) {
   case T_ENTITY:
   case T_LIBRARY:
   case T_ARCH:
   case T_PACKAGE:
   case T_PACK_INST:
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
   // See LRM 08 section 20.2.4.3

   if (tree_kind(value) != T_STRING) {
      error_at(tree_loc(value), "foreign attribute must have string "
               "literal value");
      return;
   }

   const int nchars = tree_chars(value);
   char *buf LOCAL = xmalloc(nchars + 1);
   for (int i = 0; i < nchars; i++)
      buf[i] = tree_pos(tree_ref(tree_char(value, i)));
   buf[nchars] = '\0';

   subprogram_kind_t kind = S_FOREIGN;
   char *p = strtok(buf, " ");
   if (strcmp(p, "VHPIDIRECT") == 0) {
      p = strtok(NULL, " ");
      if (p != NULL) {
         // The object library specifier is silently ignored
         char *p2 = strtok(NULL, " ");
         if (p2 != NULL) p = p2;
      }
   }
   else if (strcmp(p, "INTERNAL") == 0) {
      p = strtok(NULL, " ");
      kind = S_INTERNAL;
   }
   else if (strtok(NULL, " ") != NULL)
      error_at(tree_loc(value), "failed to parse foreign attribute");

   ident_t name = ident_new(p);
   tree_set_ident2(decl, name);

   tree_set_subkind(decl, kind);
   tree_set_flag(decl, TREE_F_NEVER_WAITS);
}

static tree_t select_decl(tree_t prefix, ident_t suffix, name_mask_t *mask)
{
   ident_t qual = ident_prefix(tree_ident(prefix), suffix, '.');

   tree_t decl = NULL;
   *mask = query_name(nametab, qual, &decl);

   tree_t ref = tree_new(T_REF);
   tree_set_ident(ref, qual);
   tree_set_loc(ref, CURRENT_LOC);
   tree_set_ref(ref, decl);

   if (*mask == 0) {
      parse_error(CURRENT_LOC, "name %s not found in %s", istr(suffix),
                  istr(tree_ident(prefix)));
      tree_set_type(ref, type_new(T_NONE));
   }

   return ref;
}

static tree_t could_be_slice_name(tree_t fcall)
{
   // The expression F(X) where X is a type name and F is a function
   // should be parsed as an array slice F(X'RANGE) where F is called
   // with no arguments

   if (tree_params(fcall) != 1)
      return fcall;

   tree_t p0 = tree_param(fcall, 0);
   if (tree_subkind(p0) != P_POS)
      return fcall;

   tree_t value = tree_value(p0);
   if (tree_kind(value) != T_REF)
      return fcall;

   if (!tree_has_ref(value))
      return fcall;

   tree_t decl = tree_ref(value);
   if (!is_type_decl(decl))
      return fcall;

   tree_t new = tree_new(T_FCALL);
   tree_set_ident(new, tree_ident(fcall));
   tree_set_loc(new, tree_loc(fcall));

   tree_t aref = tree_new(T_ATTR_REF);
   tree_set_name(aref, value);
   tree_set_ident(aref, ident_new("RANGE"));
   tree_set_loc(aref, tree_loc(value));
   tree_set_subkind(aref, ATTR_RANGE);

   tree_t r = tree_new(T_RANGE);
   tree_set_subkind(r, RANGE_EXPR);
   tree_set_value(r, aref);
   tree_set_loc(r, tree_loc(p0));

   solve_types(nametab, r, NULL);

   tree_t slice = tree_new(T_ARRAY_SLICE);
   tree_set_value(slice, new);
   tree_add_range(slice, r);
   tree_set_loc(slice, tree_loc(fcall));

   return slice;
}

static bool is_implicit_block(tree_t t)
{
   const tree_kind_t kind = tree_kind(t);
   return kind == T_ARCH || kind == T_BLOCK || kind == T_IF_GENERATE
      || kind == T_FOR_GENERATE;
}

static void make_universal_type(tree_t container, type_kind_t kind,
                                const char *name, tree_t min, tree_t max)
{
   assert(bootstrapping);
   ident_t name_i = ident_new(name);

   type_t type = type_new(kind);
   type_set_ident(type, name_i);

   tree_t r = tree_new(T_RANGE);
   tree_set_subkind(r, RANGE_TO);
   tree_set_left(r, min);
   tree_set_right(r, max);
   tree_set_type(r, type);

   type_add_dim(type, r);

   tree_set_type(min, type);
   tree_set_type(max, type);

   tree_t decl = tree_new(T_TYPE_DECL);
   tree_set_loc(decl, CURRENT_LOC);
   tree_set_ident(decl, name_i);
   tree_set_type(decl, type);

   tree_add_decl(container, decl);

   declare_predefined_ops(container, type);
}

static void make_universal_int(tree_t container)
{
   tree_t min = tree_new(T_LITERAL);
   tree_set_subkind(min, L_INT);
   tree_set_ival(min, INT64_MIN);

   tree_t max = tree_new(T_LITERAL);
   tree_set_subkind(max, L_INT);
   tree_set_ival(max, INT64_MAX);

   make_universal_type(container, T_INTEGER, "universal_integer", min, max);
}

static void make_universal_real(tree_t container)
{
   tree_t min = tree_new(T_LITERAL);
   tree_set_subkind(min, L_REAL);
   tree_set_dval(min, -DBL_MAX);

   tree_t max = tree_new(T_LITERAL);
   tree_set_subkind(max, L_REAL);
   tree_set_dval(max, DBL_MAX);

   make_universal_type(container, T_REAL, "universal_real", min, max);
}

static void make_implicit_guard_signal(tree_t block, tree_t expr)
{
   tree_t guard = tree_new(T_IMPLICIT_SIGNAL);
   tree_set_subkind(guard, IMPLICIT_GUARD);
   tree_set_ident(guard, ident_new("GUARD"));
   tree_set_loc(guard, tree_loc(expr));
   tree_set_type(guard, std_type(NULL, STD_BOOLEAN));
   tree_set_value(guard, expr);

   tree_add_decl(block, guard);
   insert_name(nametab, guard, NULL);
   sem_check(guard, nametab);
}

static tree_t fcall_to_conv_func(tree_t value)
{
   assert(tree_kind(value) == T_FCALL);

   if (!(tree_flags(value) & TREE_F_CONVERSION))
      return value;

   if (!tree_has_ref(value))
      return value;

   tree_t decl = tree_ref(value);
   if (tree_ports(decl) != 1)
      return value;

   tree_t p0 = tree_value(tree_param(value, 0));

   tree_t ref = name_to_ref(p0);
   if (ref == NULL || class_of(ref) != C_SIGNAL)
      return value;

   tree_t conv = tree_new(T_CONV_FUNC);
   tree_set_loc(conv, tree_loc(value));
   tree_set_value(conv, p0);
   tree_set_ident(conv, tree_ident(value));
   tree_set_type(conv, tree_type(value));
   tree_set_ref(conv, decl);

   return conv;
}

static bool instantiate_should_copy_type(type_t type, void *__ctx)
{
   hset_t *decls = __ctx;
   return (type_kind(type) != T_SUBTYPE) && hset_contains(decls, type);
}

static bool instantiate_should_copy_tree(tree_t t, void *__ctx)
{
   hset_t *decls = __ctx;

   switch (tree_kind(t)) {
   case T_FCALL:
      // Globally static expressions should be copied and folded
      return !!(tree_flags(t) & TREE_F_GLOBALLY_STATIC);
   case T_PACKAGE:
   case T_PACK_BODY:
      return true;
   case T_FUNC_DECL:
   case T_PROC_DECL:
      if (tree_flags(t) & TREE_F_PREDEFINED)
         return false;
      // Fall-through
   case T_FUNC_BODY:
   case T_PROC_BODY:
   case T_CONST_DECL:
   case T_VAR_DECL:
   case T_GENERIC_DECL:
   case T_SIGNAL_DECL:
      // Make a unique copy of all public declarations in the package
      return hset_contains(decls, t);
   default:
      return false;
   }
}

static void collect_decls(tree_t t, hset_t *decls, tree_list_t *roots)
{
   const tree_kind_t kind = tree_kind(t);

   if (kind != T_PACK_BODY && kind != T_PROT_BODY && kind != T_PROT_DECL) {
      const int ngenerics = tree_generics(t);
      for (int i = 0; i < ngenerics; i++) {
         tree_t g = tree_generic(t, i);
         hset_insert(decls, g);

         // If the uninstantiated unit has any package generics then we
         // need to copy those too in order to fix up the types
         switch (tree_class(g)) {
         case C_PACKAGE:
            {
               tree_t ref = tree_value(g);
               if (tree_has_ref(ref)) {
                  tree_t pack = tree_ref(ref);
                  assert(is_uninstantiated_package(pack));
                  collect_decls(pack, decls, roots);
                  APUSH(*roots, pack);
               }
            }
            break;
         case C_TYPE:
            hset_insert(decls, tree_type(g));
            break;
         default:
            break;
         }
      }
   }

   if (kind != T_PROC_DECL && kind != T_FUNC_DECL) {
      const int ndecls = tree_decls(t);
      for (int i = 0 ; i < ndecls; i++) {
         tree_t d = tree_decl(t, i);
         hset_insert(decls, d);

         switch (tree_kind(d)) {
         case T_PROT_DECL:
         case T_PROT_BODY:
         case T_PACKAGE:
         case T_PACK_BODY:
         case T_PACK_INST:
            collect_decls(d, decls, roots);
            break;
         case T_TYPE_DECL:
            hset_insert(decls, type_base_recur(tree_type(d)));
            break;
         default:
            break;
         }
      }
   }
}

static void instantiate_helper(tree_t new, tree_t *pdecl, tree_t *pbody)
{
   tree_list_t roots = AINIT;
   APUSH(roots, *pdecl);
   if (*pbody != NULL)
      APUSH(roots, *pbody);

   hset_t *decls = hset_new(256);

   collect_decls(*pdecl, decls, &roots);
   if (*pbody != NULL)
      collect_decls(*pbody, decls, &roots);

   ident_t prefixes[] = { tree_ident(*pdecl) };
   ident_t dotted = ident_prefix(scope_prefix(nametab), tree_ident(new), '.');

   copy_with_renaming(roots.items, roots.count,
                      instantiate_should_copy_tree,
                      instantiate_should_copy_type,
                      decls, dotted, prefixes, 1);

   *pdecl = roots.items[0];
   *pbody = *pbody != NULL ? roots.items[1] : NULL;

   hset_free(decls);
   ACLEAR(roots);
}

static void instantiate_subprogram(tree_t new, tree_t decl, tree_t body)
{
   assert(body != NULL);
   assert(type_eq(tree_type(body), tree_type(decl)));

   tree_t decl_copy = decl, body_copy = body;
   instantiate_helper(new, &decl_copy, &body_copy);

   tree_set_type(new, tree_type(body_copy));

   const int ngenerics = tree_generics(body_copy);
   for (int i = 0; i < ngenerics; i++)
      tree_add_generic(new, tree_generic(body_copy, i));

   const int ndecls = tree_decls(body_copy);
   for (int i = 0; i < ndecls; i++)
      tree_add_decl(new, tree_decl(body_copy, i));

   const int nstmts = tree_stmts(body_copy);
   for (int i = 0; i < nstmts; i++)
      tree_add_stmt(new, tree_stmt(body_copy, i));

   const int nports = tree_ports(body_copy);
   for (int i = 0; i < nports; i++)
      tree_add_port(new, tree_port(body_copy, i));

   // Allow recursive calls to the uninstantiated subprogram
   map_generic_subprogram(nametab, decl_copy, new);
   map_generic_subprogram(nametab, body_copy, new);
}

static void instantiate_package(tree_t new, tree_t pack, tree_t body)
{
   assert(body == NULL || tree_primary(body) == pack);

   tree_t pack_copy = pack, body_copy = body;
   instantiate_helper(new, &pack_copy, &body_copy);

   const int ngenerics = tree_generics(pack_copy);
   for (int i = 0; i < ngenerics; i++)
      tree_add_generic(new, tree_generic(pack_copy, i));

   const int ndecls = tree_decls(pack_copy);
   for (int i = 0; i < ndecls; i++)
      tree_add_decl(new, tree_decl(pack_copy, i));

   if (body != NULL) {
      // Copy all the declarations from the body into the package to
      // save keeping track of two separate units. The LRM says the
      // implicit instantiated package body is in the body of an
      // enclosing package if this is in a package declaration. Just
      // ignore that, it doesn't matter.
      const int ndecls = tree_decls(body_copy);
      for (int i = 0; i < ndecls; i++)
         tree_add_decl(new, tree_decl(body_copy, i));
   }
}

static type_t rewrite_generic_types_cb(type_t type, void *__ctx)
{
   hash_t *map = __ctx;
   return hash_get(map, type) ?: type;
}

static tree_t rewrite_generic_refs_cb(tree_t t, void *__ctx)
{
   hash_t *map = __ctx;

   switch (tree_kind(t)) {
   case T_REF:
      if (tree_flags(t) & TREE_F_FORMAL_NAME)
         return t;   // Do not rewrite names in generic maps
      // Fall-through
   case T_FCALL:
   case T_PCALL:
   case T_PROT_FCALL:
   case T_PROT_PCALL:
      if (tree_has_ref(t)) {
         tree_t new = hash_get(map, tree_ref(t));
         if (new != NULL)
            tree_set_ref(t, new);
      }
      break;

   default:
      break;
   }

   return t;
}

static void add_interface(tree_t container, tree_t decl, tree_kind_t kind)
{
   if (kind == T_GENERIC_DECL)
      tree_add_generic(container, decl);
   else
      tree_add_port(container, decl);
}

static void ident_list_push(ident_list_t **list, ident_t i, loc_t loc)
{
   ident_list_t *c = xmalloc(sizeof(ident_list_t));
   c->ident = i;
   c->loc   = loc;
   c->next  = NULL;

   if (*list == NULL)
      *list = c;
   else {
      ident_list_t *it;
      for (it = *list; it->next != NULL; it = it->next)
         ;
      it->next = c;
   }
}

static void ident_list_free(ident_list_t *list)
{
   ident_list_t *it = list;
   while (it != NULL) {
      ident_list_t *next = it->next;
      free(it);
      it = next;
   }
}

static void _ident_list_cleanup(ident_list_t **list)
{
   ident_list_free(*list);
   *list = NULL;
}

static type_t get_subtype_for(tree_t expr)
{
   type_t type = tree_type(expr);
   assert(type_is_unconstrained(type));

   type_t sub = type_new(T_SUBTYPE);
   type_set_base(sub, type);

   const loc_t *loc = tree_loc(expr);

   tree_t c = tree_new(T_CONSTRAINT);
   tree_set_loc(c, loc);

   type_add_constraint(sub, c);

   if (type_is_record(type)) {
      tree_set_subkind(c, C_RECORD);

      const int nfields = type_fields(type);
      for (int i = 0; i < nfields; i++) {
         tree_t f = type_field(type, i);
         type_t ft = tree_type(f);
         if (type_is_unconstrained(ft)) {
            tree_t rref = tree_new(T_RECORD_REF);
            tree_set_ident(rref, tree_ident(f));
            tree_set_loc(rref, loc);
            tree_set_ref(rref, f);
            tree_set_value(rref, expr);
            tree_set_type(rref, ft);

            tree_t ec = tree_new(T_ELEM_CONSTRAINT);
            tree_set_loc(ec, loc);
            tree_set_ident(ec, tree_ident(f));
            tree_set_ref(ec, f);
            tree_set_type(ec, get_subtype_for(rref));

            tree_add_range(c, ec);
         }
      }
   }
   else {
      tree_set_subkind(c, C_INDEX);

      const int ndims = dimension_of(type);
      for (int i = 0; i < ndims; i++) {
         tree_t rref = tree_new(T_ATTR_REF);
         tree_set_name(rref, expr);
         tree_set_ident(rref, ident_new("RANGE"));
         tree_set_loc(rref, loc);
         tree_set_subkind(rref, ATTR_RANGE);

         if (i > 0) {
            tree_t p = tree_new(T_LITERAL);
            tree_set_subkind(p, L_INT);
            tree_set_ival(p, i + 1);
            tree_set_loc(p, loc);
            tree_set_type(p, std_type(NULL, STD_INTEGER));

            add_param(rref, p, P_POS, NULL);
         }

         tree_t r = tree_new(T_RANGE);
         tree_set_subkind(r, RANGE_EXPR);
         tree_set_value(r, rref);

         solve_types(nametab, r, NULL);

         tree_add_range(c, r);
      }

      type_t elem = type_elem(type);
      if (type_is_unconstrained(elem)) {
         tree_t aref = tree_new(T_ATTR_REF);
         tree_set_name(aref, expr);
         tree_set_ident(aref, ident_new("ELEMENT"));
         tree_set_loc(aref, tree_loc(expr));
         tree_set_subkind(aref, ATTR_ELEMENT);
         tree_set_type(aref, elem);

         type_set_elem(sub, get_subtype_for(aref));
      }
   }

   return sub;
}

static type_t apply_subtype_attribute(tree_t aref)
{
   assert(tree_subkind(aref) == ATTR_SUBTYPE);

   tree_t name = tree_name(aref);
   type_t type = get_type_or_null(name);

   if (type == NULL) {
      parse_error(tree_loc(aref), "prefix of 'SUBTYPE attribute does not "
                  "have a type");
      return type_new(T_NONE);
   }
   else if (type_is_unconstrained(type)) {
      // Construct a new subtype using the constraints from the prefix
      return get_subtype_for(name);
   }
   else
      return type;
}

static type_t apply_element_attribute(tree_t aref)
{
   assert(tree_subkind(aref) == ATTR_ELEMENT);

   type_t type = get_type_or_null(tree_name(aref));

   if (type == NULL) {
      parse_error(tree_loc(aref), "prefix of 'ELEMENT attribute does not "
                  "have a type");
      return type_new(T_NONE);
   }
   else if (!type_is_array(type)) {
      parse_error(tree_loc(aref), "prefix of 'ELEMENT attribute must be an "
                  "array type");
      return type_new(T_NONE);
   }

   return type_elem(type);
}

static type_t apply_designated_subtype_attribute(tree_t aref)
{
   assert(tree_subkind(aref) == ATTR_DESIGNATED_SUBTYPE);

   type_t type = get_type_or_null(tree_name(aref));

   if (type == NULL) {
      parse_error(tree_loc(aref), "prefix of 'DESIGNATED_SUBTYPE attribute "
                  "does not have a type");
      return type_new(T_NONE);
   }
   else if (!type_is_file(type) && !type_is_access(type)) {
      parse_error(tree_loc(aref), "prefix of 'DESIGNATED_SUBTYPE attribute "
                  "must be an access or file type");
      return type_new(T_NONE);
   }

   return type_designated(type);
}

static type_t apply_base_attribute(tree_t aref)
{
   assert(tree_subkind(aref) == ATTR_BASE);

   tree_t name = tree_name(aref);
   type_t type = NULL;

   if (tree_kind(name) == T_REF && tree_has_ref(name)) {
      tree_t decl = aliased_type_decl(tree_ref(name));
      if (decl != NULL)
         type = tree_type(decl);
   }

   if (type == NULL) {
      parse_error(tree_loc(aref), "prefix of 'BASE attribute must be a type "
                  "or subtype declaration");
      return type_new(T_NONE);
   }
   else if (type_kind(type) == T_SUBTYPE)
      return type_base(type);
   else
      return type;
}

static type_t apply_index_attribute(tree_t aref)
{
   assert(tree_subkind(aref) == ATTR_INDEX);

   type_t type = get_type_or_null(tree_name(aref));

   if (!type_is_array(type)) {
      parse_error(tree_loc(aref), "prefix of 'INDEX attribute must be an "
                  "array type");
      return type_new(T_NONE);
   }

   const int ndims = dimension_of(type);
   const int nparams = tree_params(aref);

   int index = 0;
   if (nparams == 1) {
      // The LRM allows any locally static expression here but that is
      // difficult to implement and doesn't seem useful
      tree_t p = tree_value(tree_param(aref, 0));
      if (tree_kind(p) != T_LITERAL) {
         parse_error(tree_loc(p), "only integer literals are supported "
                     "for 'INDEX parameter");
         return type_new(T_NONE);
      }

      const int64_t ival = tree_ival(p);
      if (ival < 1 || ival > ndims) {
         parse_error(tree_loc(p), "'INDEX parameter for type %s must be "
                     "between 1 and %d", type_pp(type), ndims);
         return type_new(T_NONE);
      }

      index = ival - 1;
   }

   return index_type_of(type, index);
}

static type_t apply_type_attribute(tree_t aref)
{
   switch (tree_subkind(aref)) {
   case ATTR_SUBTYPE:
      return apply_subtype_attribute(aref);
   case ATTR_ELEMENT:
      return apply_element_attribute(aref);
   case ATTR_BASE:
      return apply_base_attribute(aref);
   case ATTR_DESIGNATED_SUBTYPE:
      return apply_designated_subtype_attribute(aref);
   case ATTR_INDEX:
      return apply_index_attribute(aref);
   default:
      parse_error(tree_loc(aref), "attribute name is not a valid type mark");
      return type_new(T_NONE);
   }
}

static void implicit_signal_attribute(tree_t aref)
{
   if (find_enclosing(nametab, S_SUBPROGRAM) != NULL) {
      parse_error(tree_loc(aref), "implicit signal %s cannot be used in a "
                  "subprogram body", istr(tree_ident(aref)));
      return;
   }

   tree_t b = find_enclosing(nametab, S_CONCURRENT_BLOCK);
   if (b == NULL) {
      parse_error(tree_loc(aref), "implicit signal %s cannot be used in "
                  "this context", istr(tree_ident(aref)));
      return;
   }

   tree_t prefix = tree_name(aref);
   const attr_kind_t attr = tree_subkind(aref);

   tree_t delay = NULL;
   bool const_delay = false;
   if (attr == ATTR_DELAYED && tree_params(aref) > 0) {
      delay = tree_value(tree_param(aref, 0));
      const_delay = (tree_kind(delay) == T_LITERAL);
   }

   LOCAL_TEXT_BUF tb = tb_new();
   tree_t ref = name_to_ref(prefix);
   if (ref != NULL)
      tb_istr(tb, tree_ident(ref));
   tb_printf(tb, "$%s", attr == ATTR_DELAYED ? "delayed" : "transaction");
   if (const_delay) {
      tb_printf(tb, "_%"PRIi64, tree_ival(delay));
      if (tree_has_ident(delay))
         tb_printf(tb, "_%s", istr(tree_ident(delay)));
   }

   ident_t id;
   if (ref == prefix && (delay == NULL || const_delay)) {
      id = ident_new(tb_get(tb));

      tree_t exist = search_decls(b, id, 0);
      if (exist != NULL) {
         tree_set_value(aref, make_ref(exist));
         return;
      }
   }
   else
      id = ident_uniq(tb_get(tb));

   tree_t imp = tree_new(T_IMPLICIT_SIGNAL);
   tree_set_ident(imp, id);
   tree_set_loc(imp, CURRENT_LOC);

   switch (attr) {
   case ATTR_DELAYED:
      {
         tree_t w = tree_new(T_WAVEFORM);
         tree_set_loc(w, CURRENT_LOC);
         tree_set_value(w, prefix);
         tree_set_delay(w, delay ?: get_time(0, CURRENT_LOC));

         tree_set_subkind(imp, IMPLICIT_DELAYED);
         tree_set_type(imp, tree_type(aref));
         tree_set_value(imp, w);
      }
      break;

   case ATTR_TRANSACTION:
      {
         type_t std_bit = std_type(NULL, STD_BIT);
         tree_set_type(imp, std_bit);

         tree_t not = tree_new(T_FCALL);
         tree_set_ident(not, ident_new("\"not\""));
         tree_set_loc(not, CURRENT_LOC);
         add_param(not, make_ref(imp), P_POS, NULL);

         solve_types(nametab, not, std_bit);

         tree_set_subkind(imp, IMPLICIT_TRANSACTION);
         tree_add_trigger(imp, prefix);
         tree_set_value(imp, not);
      }
      break;

   default:
      fatal_trace("invalid implicit signal attribute");
   }

   tree_add_decl(b, imp);

   tree_set_value(aref, make_ref(imp));
}

static attr_kind_t parse_predefined_attr(ident_t ident)
{
   static struct {
      const char      *str;
      attr_kind_t      attr;
      vhdl_standard_t  std;
      ident_t          ident;
   } predef[] = {
      { "RANGE", ATTR_RANGE },
      { "REVERSE_RANGE", ATTR_REVERSE_RANGE },
      { "LENGTH", ATTR_LENGTH },
      { "LEFT", ATTR_LEFT },
      { "RIGHT", ATTR_RIGHT },
      { "LOW", ATTR_LOW },
      { "HIGH", ATTR_HIGH },
      { "EVENT", ATTR_EVENT },
      { "ACTIVE", ATTR_ACTIVE },
      { "IMAGE", ATTR_IMAGE },
      { "ASCENDING", ATTR_ASCENDING },
      { "LAST_VALUE", ATTR_LAST_VALUE },
      { "LAST_EVENT", ATTR_LAST_EVENT },
      { "LAST_ACTIVE", ATTR_LAST_ACTIVE },
      { "PATH_NAME", ATTR_PATH_NAME },
      { "INSTANCE_NAME", ATTR_INSTANCE_NAME },
      { "SIMPLE_NAME", ATTR_SIMPLE_NAME },
      { "DELAYED", ATTR_DELAYED },
      { "STABLE", ATTR_STABLE },
      { "QUIET", ATTR_QUIET },
      { "TRANSACTION", ATTR_TRANSACTION },
      { "DRIVING_VALUE", ATTR_DRIVING_VALUE },
      { "DRIVING", ATTR_DRIVING },
      { "VALUE", ATTR_VALUE },
      { "SUCC", ATTR_SUCC },
      { "PRED", ATTR_PRED },
      { "LEFTOF", ATTR_LEFTOF },
      { "RIGHTOF", ATTR_RIGHTOF },
      { "POS", ATTR_POS },
      { "VAL", ATTR_VAL },
      { "BASE", ATTR_BASE },
      { "ELEMENT", ATTR_ELEMENT, STD_08 },
      { "CONVERSE", ATTR_CONVERSE, STD_19 },
      { "DESIGNATED_SUBTYPE", ATTR_DESIGNATED_SUBTYPE, STD_19 },
      { "INDEX", ATTR_INDEX, STD_19 },
      { "REFLECT", ATTR_REFLECT, STD_19 },
   };

   INIT_ONCE({
         for (int i = 0; i < ARRAY_LEN(predef); i++)
            predef[i].ident = ident_new(predef[i].str);
      });

   for (int i = 0; i < ARRAY_LEN(predef); i++) {
      if (predef[i].ident == ident
          && (predef[i].std <= STD_93 || standard() >= predef[i].std))
         return predef[i].attr;
   }

   return ATTR_USER;
}

static void add_generic_type_op(tree_t parent, int nargs, type_t type,
                                type_t result, const char *name)
{
   ident_t id = ident_new(name);

   type_t ftype = type_new(T_FUNC);
   type_set_ident(ftype, id);
   type_set_result(ftype, result);

   for (int i = 0; i < nargs; i++)
      type_add_param(ftype, type);

   tree_t p = tree_new(T_GENERIC_DECL);
   tree_set_class(p, C_FUNCTION);
   tree_set_ident(p, id);
   tree_set_type(p, ftype);
   tree_set_subkind(p, PORT_IN);
   tree_set_loc(p, CURRENT_LOC);
   tree_set_flag(p, TREE_F_PREDEFINED);

   if (standard() >= STD_19) {
      // LRM 08 section 6.5.3.1: the *predefined* [..] operators,
      // implicitly declared as formal generic subprograms
      //
      // LCS2016-59 changed the wording here: additional operators are
      // implicitly declared as formal generic subprograms with an
      // interface subprogram default in form of a box (<>)

      tree_t box = tree_new(T_BOX);
      tree_set_loc(box, CURRENT_LOC);

      tree_set_value(p, box);
   }

   for (int j = 0; j < nargs; j++) {
      tree_t arg = tree_new(T_PORT_DECL);
      tree_set_ident(arg, ident_new(j == 0 ? "L" : "R"));
      tree_set_type(arg, type);
      tree_set_subkind(arg, PORT_IN);
      tree_set_class(arg, C_CONSTANT);
      tree_set_loc(arg, CURRENT_LOC);

      tree_add_port(p, arg);
   }

   add_interface(parent, p, T_GENERIC_DECL);
   insert_name(nametab, p, NULL);
}

static bool is_psl_infix_op(token_t tok)
{
   return tok == tIFIMPL || tok == tUNTIL || tok == tUNTIL_ || tok == tUNTIL1
      || tok == tUNTIL_1;
}

static void add_predef_alias(tree_t t, void *context)
{
   tree_t parent = context;
   assert(is_subprogram(t));

   tree_t a = tree_new(T_ALIAS);
   tree_set_loc(a, CURRENT_LOC);
   tree_set_ident(a, tree_ident(t));
   tree_set_value(a, make_ref(t));
   tree_set_type(a, tree_type(t));
   tree_set_flag(a, TREE_F_PREDEFINED);

   insert_name(nametab, a, NULL);
   tree_add_decl(parent, a);
}

////////////////////////////////////////////////////////////////////////////////
// Parser rules

static ident_t p_identifier(void)
{
   // basic_identifier | extended_identifier

   if (consume(tID))
      return last_lval.ident;
   else
      return error_marker();
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

   ident_list_push(&result, p_identifier(), last_loc);

   while (optional(tCOMMA))
      ident_list_push(&result, p_identifier(), last_loc);

   return result;
}

static ident_t p_operator_symbol(void)
{
   // string_literal

   consume(tSTRING);

   char *s = last_lval.str;
   for (char *p = s; *p != '\0'; p++)
      *p = tolower_iso88591(*p);

   ident_t id = ident_new(s);

   if (!is_operator_symbol(id))
      parse_error(CURRENT_LOC, "%s is not an operator symbol", s);

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
      tree_set_loc(l, &(it->loc));

      tree_add_context(unit, l);

      lib_t lib = lib_find(it->ident);
      if (lib == NULL) {
         LOCAL_TEXT_BUF tb = tb_new();
         lib_print_search_paths(tb);

         diag_t *d = diag_new(DIAG_ERROR, CURRENT_LOC);
         diag_printf(d, "library %s not found", istr(it->ident));
         lib_search_paths_to_diag(d);
         diag_emit(d);
      }
      else
         tree_set_ident2(l, lib_name(lib));

      insert_name(nametab, l, NULL);
   }
}

static void p_use_clause(tree_t unit, add_func_t addf)
{
   // use selected_name { , selected_name } ;

   BEGIN("use clause");

   consume(tUSE);

   do {
      tree_t u = tree_new(T_USE);

      ident_t i1 = p_identifier(), i2 = NULL;
      consume(tDOT);

      do {
         i1 = ident_prefix(i1, i2, '.');

         switch (peek()) {
         case tID:
            i2 = p_identifier();
            break;
         case tSTRING:
            i2 = p_operator_symbol();
            break;
         case tALL:
            consume(tALL);
            i2 = well_known(W_ALL);
            break;
         default:
            expect(tID, tSTRING, tALL);
            i2 = NULL;
            break;
         }
      } while (optional(tDOT));

      tree_set_ident(u, i1);
      tree_set_ident2(u, i2);

      tree_set_loc(u, CURRENT_LOC);
      (*addf)(unit, u);

      tree_t head = resolve_name(nametab, CURRENT_LOC, i1);
      if (head == NULL)
         continue;

      const tree_kind_t kind = tree_kind(head);
      if (kind == T_LIBRARY && !tree_has_ident2(head)) {
         // Library declaration had an error
      }
      else if (is_uninstantiated_package(head))
         parse_error(CURRENT_LOC, "cannot use an uninstantiated package");
      else if (kind == T_LIBRARY || kind == T_PACKAGE
               || kind == T_PACK_INST
               || (kind == T_GENERIC_DECL
                   && tree_class(head) == C_PACKAGE)) {
         tree_set_ref(u, head);
         insert_names_from_use(nametab, u);
      }
      else
         parse_error(CURRENT_LOC, "%s is not a library or %spackage",
                     istr(i1), standard() >= STD_08 ? "instantiated " : "");
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

      tree_t c = tree_new(T_CONTEXT_REF);
      tree_set_ident(c, name);
      tree_set_loc(c, CURRENT_LOC);

      tree_t ctx = resolve_name(nametab, CURRENT_LOC, name);
      if (ctx != NULL && tree_kind(ctx) == T_CONTEXT) {
         insert_names_from_context(nametab, ctx);
         tree_set_ref(c, ctx);
      }
      else if (ctx != NULL)
         parse_error(CURRENT_LOC, "%s%s is not a context declaration",
                     is_design_unit(ctx) ? "design unit " : "", istr(name));

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

   const int start_errors = error_count();

   while (scan(tLIBRARY, tUSE, tCONTEXT)) {
      if (peek() == tCONTEXT && peek_nth(3) == tIS)
         break;
      else
         p_context_item(unit);
   }

   // Suppress further errors if there are errors in the context
   if (error_count() > start_errors)
      suppress_errors(nametab);
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

static tree_t p_range(tree_t left)
{
   // attribute_name | simple_expression direction simple_expression

   EXTEND("range");

   tree_t r = tree_new(T_RANGE);
   tree_set_subkind(r, RANGE_ERROR);

   if (is_range_expr(left)) {
      tree_set_subkind(r, RANGE_EXPR);
      tree_set_value(r, left);
      tree_set_loc(r, tree_loc(left));
   }
   else {
      tree_set_left(r, left);

      switch (one_of(tTO, tDOWNTO)) {
      case tTO:
         tree_set_subkind(r, RANGE_TO);
         tree_set_right(r, p_expression());
         break;

      case tDOWNTO:
         tree_set_subkind(r, RANGE_DOWNTO);
         tree_set_right(r, p_expression());
         break;
      }

      tree_set_loc(r, CURRENT_LOC);
   }

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
      {
         tree_t r = p_range(expr1);
         solve_types(nametab, r, constraint);
         tree_add_range(t, r);
      }
      break;
   default:
      {
         tree_t r = tree_new(T_RANGE);
         tree_set_loc(r, tree_loc(expr1));
         tree_set_subkind(r, RANGE_EXPR);
         tree_set_value(r, expr1);

         solve_types(nametab, r, constraint);

         tree_add_range(t, r);
      }
   }

   tree_set_loc(t, CURRENT_LOC);
   return t;
}

static tree_t p_discrete_range(tree_t head)
{
   // subtype_indication | range

   BEGIN_WITH_HEAD("discrete range", head);

   tree_t expr1 = head ?: p_expression();

   switch (peek()) {
   case tTO:
   case tDOWNTO:
   case tTICK:
      return p_range(expr1);

   case tRANGE:
      {
         type_t constraint = solve_types(nametab, expr1, NULL);

         const bool is_type =
            tree_kind(expr1) == T_REF
            && tree_has_ref(expr1)
            && is_type_decl(tree_ref(expr1));

         if (!is_type && !type_is_none(constraint)) {
            parse_error(tree_loc(expr1), "expected type mark while parsing "
                        "discrete range");
            constraint = type_new(T_NONE);
         }

         consume(tRANGE);

         tree_t left = p_expression();
         tree_t r = p_range(left);
         solve_types(nametab, r, constraint);
         tree_set_type(r, constraint);
         return r;
      }

   default:
      {
         type_t type = solve_types(nametab, expr1, NULL);

         if (tree_kind(expr1) == T_ATTR_REF)
            return p_range(expr1);   // Special attributes such as 'RANGE
         else if (tree_kind(expr1) == T_REF && tree_has_ref(expr1)) {
            // A type name T may stand in for a discrete range
            // equivalent to T'RANGE
            if (aliased_type_decl(tree_ref(expr1)) != NULL) {
               tree_t tmp = tree_new(T_ATTR_REF);
               tree_set_name(tmp, expr1);
               tree_set_ident(tmp, ident_new("RANGE"));
               tree_set_loc(tmp, tree_loc(expr1));
               tree_set_subkind(tmp, ATTR_RANGE);

               return p_range(tmp);
            }
            else
               parse_error(CURRENT_LOC, "name %s in discrete range does not "
                           "refer to a type", istr(tree_ident(expr1)));
         }
         else if (!type_is_none(type))
            parse_error(CURRENT_LOC, "expecting a discrete range");

         // Not a valid discrete range
         tree_t r = tree_new(T_RANGE);
         tree_set_loc(r, CURRENT_LOC);
         tree_set_subkind(r, RANGE_ERROR);
         return r;
      }
   }
}

static tree_t p_constrained_discrete_range(type_t index_type)
{
   tree_t r = p_discrete_range(NULL);

   if (tree_has_type(r))
      return r;   // Already constrained e.g. from INTEGER range X to Y

   type_t type = solve_types(nametab, r, index_type);

   // LRM 08 section 5.3.2.2: an implicit conversion to the predefined
   // type INTEGER is assumed if the type of both bounds is the type
   // universal_integer
   if (type_eq(type, std_type(NULL, STD_UNIVERSAL_INTEGER)))
      tree_set_type(r, index_type ?: std_type(NULL, STD_INTEGER));

   return r;
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

   tree_t r = p_discrete_range(head);
   solve_types(nametab, r, index_type);

   tree_add_range(t, r);
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

   tree_t name = p_name(0);

   if (tree_kind(name) == T_FCALL && tree_params(name) == 1)
      tree_set_flag(name, TREE_F_CONVERSION);

   return name;
}

static tree_t p_actual_part(class_t class)
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

   if (class == C_FUNCTION || class == C_PROCEDURE || class == C_PACKAGE)
      return p_name(N_SUBPROGRAM);
   else if (class == C_TYPE) {
      type_t type = p_subtype_indication();

      tree_t ref = tree_new(T_TYPE_REF);
      tree_set_ident(ref, type_ident(type));
      tree_set_type(ref, type);
      tree_set_loc(ref, CURRENT_LOC);
      return ref;
   }

   // If the actual part takes either the second or third form above then the
   // argument to the function call is the actual designator but only if the
   // call is to a named function rather than an operator.
   // This is important for identifying conversion functions later.
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

static void p_association_element(tree_t map, int pos, tree_t unit,
                                  formal_kind_t kind)
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

   class_t class = C_DEFAULT;
   type_t type = NULL;
   if (look_for(&lookp)) {
      tree_set_subkind(p, P_NAMED);

      push_scope_for_formals(nametab, kind, unit);

      tree_t name = p_formal_part();

      tree_t ref = name_to_ref(name);
      if (ref != NULL && tree_has_ref(ref)) {
         tree_t decl = tree_ref(ref);
         const tree_kind_t kind = tree_kind(decl);
         if (kind == T_PORT_DECL || kind == T_PARAM_DECL
             || kind == T_GENERIC_DECL)
            class = tree_class(decl);
      }

      if (class != C_PACKAGE && (kind == F_GENERIC_MAP || kind == F_PORT_MAP))
         type = solve_types(nametab, name, NULL);

      if (kind == F_PORT_MAP && tree_kind(name) == T_FCALL)
         name = fcall_to_conv_func(name);

      tree_set_name(p, name);

      pop_scope(nametab);

      consume(tASSOC);
   }
   else {
      tree_set_subkind(p, P_POS);
      tree_set_pos(p, pos);

      tree_t formal = NULL;
      switch (kind) {
      case F_GENERIC_MAP:
         if (unit != NULL && pos < tree_generics(unit))
            formal = tree_generic(unit, pos);
         break;
      case F_PORT_MAP:
         if (unit != NULL && pos < tree_ports(unit))
            formal = tree_port(unit, pos);
         break;
      default:
         break;
      }

      if (formal != NULL && (class = tree_class(formal)) != C_PACKAGE)
         type = tree_type(formal);
   }

   tree_t value = p_actual_part(class);

   if (kind == F_PORT_MAP)
      solve_types(nametab, value, type);
   else if (kind == F_GENERIC_MAP && class != C_PACKAGE) {
      type_t value_type = solve_types(nametab, value, type);

      // Make the mapped type available immediately as it may be used in
      // later actuals
      if (class == C_TYPE && type != NULL)
         map_generic_type(nametab, type, value_type);
   }

   if (kind == F_PORT_MAP && tree_kind(value) == T_FCALL)
      value = fcall_to_conv_func(value);

   tree_set_value(p, value);
   tree_set_loc(p, CURRENT_LOC);

   switch (kind) {
   case F_GENERIC_MAP:
      tree_add_genmap(map, p);
      break;
   case F_PORT_MAP:
   case F_SUBPROGRAM:
      tree_add_param(map, p);
      break;
   default:
      fatal_trace("unexpected formal kind in p_association_element");
   }
}

static void p_association_list(tree_t map, tree_t unit, formal_kind_t kind)
{
   // association_element { , association_element }

   BEGIN("association list");

   int pos = 0;
   do {
      p_association_element(map, pos++, unit, kind);
   } while (optional(tCOMMA));
}

static void p_actual_parameter_part(tree_t call)
{
   // association_list

   BEGIN("actual parameter part");

   p_association_list(call, call, F_SUBPROGRAM);
}

static tree_t p_function_call(ident_t id, tree_t prefix)
{
   // name [ ( actual_parameter_part ) ]

   EXTEND("function call");

   tree_t call;
   if (prefix != NULL) {
      call = tree_new(T_PROT_FCALL);
      tree_set_ident(call, id);
      tree_set_name(call, prefix);
   }
   else {
      call = tree_new(T_FCALL);
      tree_set_ident(call, id);
   }

   if (optional(tLPAREN)) {
      p_actual_parameter_part(call);
      consume(tRPAREN);
   }

   tree_set_loc(call, CURRENT_LOC);
   return could_be_slice_name(call);
}

static tree_t p_attribute_name(tree_t prefix)
{
   // prefix [ signature ] ' attribute_designator [ ( expression ) ]

   EXTEND("attribute name");

   consume(tTICK);

   attr_kind_t kind;
   ident_t id;
   switch (peek()) {
   case tRANGE:
      consume(tRANGE);
      id = ident_new("RANGE");
      kind = ATTR_RANGE;
      break;
   case tREVRANGE:
      consume(tREVRANGE);
      id = ident_new("REVERSE_RANGE");
      kind = ATTR_REVERSE_RANGE;
      break;
   case tSUBTYPE:
      consume(tSUBTYPE);
      require_std(STD_08, "subtype attribute");
      id = ident_new("SUBTYPE");
      kind = ATTR_SUBTYPE;
      break;
   case tID:
      id = p_identifier();
      kind = parse_predefined_attr(id);
      break;
   default:
      one_of(tRANGE, tREVRANGE, tID, tSUBTYPE);
      kind = ATTR_USER;
      id = error_marker();
   }

   type_t type = prefix_type(prefix);

   const bool deref_prefix =
      !is_type_attribute(kind) && kind != ATTR_REFLECT
      && type != NULL && type_is_access(type);

   if (deref_prefix) {
      prefix = implicit_dereference(prefix);
      type   = tree_type(prefix);
   }

   prefix = external_reference(prefix);

   tree_t t = tree_new(T_ATTR_REF);
   tree_set_name(t, prefix);

   tree_set_ident(t, id);
   tree_set_subkind(t, kind);
   tree_set_loc(t, CURRENT_LOC);

   if (attribute_has_param(kind) && optional(tLPAREN)) {
      add_param(t, p_expression(), P_POS, NULL);
      consume(tRPAREN);
      tree_set_loc(t, CURRENT_LOC);
   }

   if (is_type_attribute(kind))
      tree_set_type(t, apply_type_attribute(t));
   else
      solve_types(nametab, t, NULL);

   if (kind == ATTR_DELAYED || kind == ATTR_TRANSACTION)
      implicit_signal_attribute(t);

   return t;
}

static tree_t p_selected_name(tree_t prefix, name_mask_t *mask)
{
   // prefix . suffix

   EXTEND("selected name");

   // If the prefix is a reference to a function then convert it to a
   // call unless it matches the name of the enclosing subprogram
   tree_kind_t prefix_kind = tree_kind(prefix);
   if ((*mask & N_FUNC) && prefix_kind == T_REF) {
      ident_t id = tree_ident(prefix);
      tree_t sub = find_enclosing(nametab, S_SUBPROGRAM);
      if (sub != NULL && tree_ident(sub) == id)
         tree_set_ref(prefix, sub);
      else {
         prefix = p_function_call(id, NULL);
         prefix_kind = T_FCALL;
      }
   }
   else if (prefix_kind == T_PROT_REF) {
      prefix = p_function_call(tree_ident(prefix), tree_value(prefix));
      prefix_kind = T_FCALL;
   }

   consume(tDOT);
   *mask = 0;

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
         *mask |= N_OBJECT;
         return all;
      }

   default:
      expect(tID, tSTRING, tALL);
      *mask |= N_ERROR;
      return prefix;
   }

   if (prefix_kind == T_REF && tree_has_ref(prefix)) {
      tree_t decl = tree_ref(prefix);
      const tree_kind_t kind = tree_kind(decl);
      if (kind == T_LIBRARY) {
         ident_t unit_name = ident_prefix(tree_ident(decl), suffix, '.');
         tree_t unit = resolve_name(nametab, CURRENT_LOC, unit_name);
         if (unit == NULL) {
            tree_t dummy = tree_new(T_REF);
            tree_set_ident(dummy, unit_name);
            tree_set_type(dummy, type_new(T_NONE));
            *mask |= N_ERROR;
            return dummy;
         }
         else
            return external_reference(unit);
      }
      else if (kind == T_GENERIC_DECL && tree_class(decl) == C_PACKAGE)
         return select_decl(tree_value(decl), suffix, mask);
      else if (is_container(decl))
         return select_decl(prefix, suffix, mask);
   }

   if (scope_formal_kind(nametab) == F_SUBPROGRAM) {
      tree_t rref = tree_new(T_RECORD_REF);
      tree_set_value(rref, prefix);
      tree_set_ident(rref, suffix);
      tree_set_loc(rref, CURRENT_LOC);
      *mask |= N_OBJECT;
      return rref;
   }

   type_t type = solve_types(nametab, prefix, NULL);

   if (type_is_access(type)) {
      prefix = implicit_dereference(prefix);
      type   = tree_type(prefix);
      prefix_kind = T_ALL;
   }

   if (type_kind(type) == T_INCOMPLETE) {
      type = resolve_type(nametab, type);
      tree_set_type(prefix, type);
   }

   if (type_is_record(type) || type_is_none(type)) {
      tree_t rref = tree_new(T_RECORD_REF);
      tree_set_value(rref, prefix);
      tree_set_ident(rref, suffix);
      tree_set_loc(rref, CURRENT_LOC);
      *mask |= N_OBJECT;
      return rref;
   }
   else if (type_is_protected(type)) {
      tree_t pref = tree_new(T_PROT_REF);
      tree_set_value(pref, prefix);
      tree_set_ident(pref, suffix);
      tree_set_loc(pref, CURRENT_LOC);
      *mask |= N_SUBPROGRAM;
      return pref;
   }
   else if (type_kind(type) == T_INCOMPLETE) {
      parse_error(tree_loc(prefix), "object with incomplete type %s cannot be "
                  "selected", type_pp(type));
      *mask |= N_ERROR;
      return prefix;
   }
   else if (prefix_kind == T_REF) {
      parse_error(tree_loc(prefix), "object %s with type %s cannot be selected",
                  istr(tree_ident(prefix)), type_pp(type));
      tree_set_type(prefix, type_new(T_NONE));
      *mask |= N_ERROR;
      return prefix;
   }
   else {
      parse_error(tree_loc(prefix), "object with type %s cannot be selected",
                  type_pp(type));
      tree_set_type(prefix, type_new(T_NONE));
      *mask |= N_ERROR;
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

      type_t index_type = NULL;
      if (type != NULL && type_is_array(type))
         index_type = index_type_of(type, n++);
      solve_types(nametab, index, index_type);
   } while (optional(tCOMMA));

   consume(tRPAREN);

   tree_set_loc(t, CURRENT_LOC);
   return t;
}

static tree_t p_type_conversion(tree_t prefix)
{
   // type_conversion ::= type_mark ( expression )

   EXTEND("type conversion");

   consume(tLPAREN);

   type_t type = NULL;
   if (tree_kind(prefix) == T_ATTR_REF)
      type = tree_type(prefix);
   else {
      assert(tree_kind(prefix) == T_REF);
      tree_t tdecl = resolve_name(nametab, CURRENT_LOC, tree_ident(prefix));
      if (tdecl == NULL)
         type = type_new(T_NONE);
      else {
         tdecl = aliased_type_decl(tdecl);
         assert(tdecl);   // Call to this is guarded by N_TYPE mask
         type = tree_type(tdecl);
      }
   }

   tree_t conv = tree_new(T_TYPE_CONV);
   tree_set_type(conv, type);
   tree_set_value(conv, p_expression());

   consume(tRPAREN);

   tree_set_loc(conv, CURRENT_LOC);
   return conv;
}

static void p_partial_pathname(tree_t name)
{
   // { pathname_element . } object_simple_name

   BEGIN("partial pathname");

   do {
      tree_t pe = tree_new(T_PATH_ELT);
      tree_set_ident(pe, p_identifier());

      if (optional(tLPAREN)) {
         tree_t expr = p_expression();
         solve_types(nametab, expr, NULL);

         tree_set_subkind(pe, PE_GENERATE);
         tree_set_value(pe, expr);
         consume(tRPAREN);
      }
      else
         tree_set_subkind(pe, PE_SIMPLE);

      tree_set_loc(pe, CURRENT_LOC);

      tree_add_part(name, pe);
   } while (optional(tDOT));
}

static void p_package_pathname(tree_t name)
{
   // @ library_logical_name . package_simple_name . { package_simple_name . }
   //       object_simple_name

   BEGIN("package pathname");

   consume(tAT);

   tree_t pe = tree_new(T_PATH_ELT);
   tree_set_subkind(pe, PE_LIBRARY);
   tree_set_ident(pe, p_identifier());
   tree_set_loc(pe, CURRENT_LOC);

   tree_add_part(name, pe);

   consume(tDOT);

   p_partial_pathname(name);
}

static void p_absolute_pathname(tree_t name)
{
   // . partial_pathname

   BEGIN("absolute pathname");

   consume(tDOT);

   tree_t pe = tree_new(T_PATH_ELT);
   tree_set_subkind(pe, PE_ABSOLUTE);
   tree_set_loc(pe, CURRENT_LOC);

   tree_add_part(name, pe);

   p_partial_pathname(name);
}

static void p_relative_pathname(tree_t name)
{
   // { ^ . } partial_pathname

   BEGIN("relative pathname");

   tree_t pe = tree_new(T_PATH_ELT);
   tree_set_subkind(pe, PE_RELATIVE);

   tree_add_part(name, pe);

   while (peek() == tCARET) {
      consume(tCARET);
      consume(tDOT);

      tree_t pe = tree_new(T_PATH_ELT);
      tree_set_loc(pe, CURRENT_LOC);
      tree_set_subkind(pe, PE_CARET);

      tree_add_part(name, pe);
   }

   p_partial_pathname(name);
}

static void p_external_pathname(tree_t name)
{
   // package_pathname | absolute_pathname | relative_pathname

   BEGIN("external pathname");

   switch (peek()) {
   case tDOT:
      p_absolute_pathname(name);
      break;
   case tCARET:
   case tID:
      p_relative_pathname(name);
      break;
   case tAT:
      p_package_pathname(name);
      break;
   default:
      one_of(tDOT, tCARET, tID, tAT);
   }
}

static tree_t p_external_name(void)
{
   // << constant external_pathname : subtype_indication >>
   //   | << signal external_pathname : subtype_indication >>
   //   | << variable external_pathname : subtype_indication >>

   BEGIN("external name");

   consume(tLTLT);

   require_std(STD_08, "external names");

   tree_t t = tree_new(T_EXTERNAL_NAME);

   switch (one_of(tCONSTANT, tSIGNAL, tVARIABLE)) {
   case tSIGNAL:   tree_set_class(t, C_SIGNAL); break;
   case tCONSTANT: tree_set_class(t, C_CONSTANT); break;
   case tVARIABLE: tree_set_class(t, C_VARIABLE); break;
   }

   p_external_pathname(t);

   consume(tCOLON);

   tree_set_type(t, p_subtype_indication());

   consume(tGTGT);

   tree_set_loc(t, CURRENT_LOC);
   return t;
}

static tree_t p_name(name_mask_t stop_mask)
{
   // simple_name | operator_symbol | selected_name | indexed_name
   //   | slice_name | attribute_name | 2008: external_name

   BEGIN("name");

   ident_t id = NULL;
   switch (peek()) {
   case tSTRING:
      id = p_operator_symbol();
      break;

   case tID:
      id = p_identifier();
      break;

   case tLTLT:
      return p_external_name();

   default:
      {
         expect(tSTRING, tID);

         tree_t dummy = tree_new(T_REF);
         tree_set_loc(dummy, CURRENT_LOC);
         tree_set_ident(dummy, error_marker());
         tree_set_type(dummy, type_new(T_NONE));
         return dummy;
      }
   }

   tree_t decl = NULL;
   name_mask_t mask = query_name(nametab, id, &decl);

   tree_t prefix = tree_new(T_REF);
   tree_set_ident(prefix, id);
   tree_set_loc(prefix, CURRENT_LOC);
   tree_set_ref(prefix, decl);

   for (;;) {
      switch (peek()) {
      case tLPAREN:
         break;

      case tDOT:
         prefix = p_selected_name(prefix, &mask);
         continue;

      case tTICK:
         if (peek_nth(2) == tLPAREN) {
            if (mask & stop_mask)
               return prefix;
            else {
               prefix = p_qualified_expression(prefix);
               mask = N_OBJECT;
            }
         }
         else {
            prefix = p_attribute_name(prefix);
            mask = is_type_attribute(tree_subkind(prefix)) ? N_TYPE : N_OBJECT;
         }
         continue;

      default:
         return prefix;
      }

      // Prefix could either be an array to be indexed or sliced, a
      // subprogram to be called, or a type conversion.

      if (mask & stop_mask)
         return prefix;
      else if (!(mask & N_FUNC) && (stop_mask == N_TYPE))
         return prefix;   // Better error messages for bad type declaration

      if (!(mask & N_FUNC) && scope_formal_kind(nametab) == F_SUBPROGRAM) {
         // Assume that A in F(A(N) => ...) is a parameter name
         mask |= N_OBJECT;
      }

      const tree_kind_t prefix_kind = tree_kind(prefix);

      if (mask & N_TYPE) {
         // Type conversion
         prefix = p_type_conversion(prefix);
         mask = N_OBJECT;
         continue;
      }
      else if (!(mask & N_OBJECT) && prefix_kind == T_REF) {
         // Function call
         prefix = p_function_call(tree_ident(prefix), NULL);
         mask = N_OBJECT;
         continue;
      }
      else if (!(mask & N_OBJECT) && prefix_kind == T_PROT_REF) {
         // Protected function call
         prefix = p_function_call(tree_ident(prefix), tree_value(prefix));
         mask = N_OBJECT;
         continue;
      }

      // Must be a slice or index name: we have to parse up to the first
      // expression to know which

      consume(tLPAREN);

      tree_t head = p_expression();

      if (scan(tDOWNTO, tTO) || is_range_expr(head))
         prefix = p_slice_name(prefix, head);
      else
         prefix = p_indexed_name(prefix, head);
   }
}

static type_t p_type_mark(void)
{
   // name

   BEGIN("type mark");

   tree_t name = p_name(N_TYPE);
   type_t type = solve_types(nametab, name, NULL);

   if (type_is_none(type))
      return type;

   const tree_kind_t namek = tree_kind(name);

   if (namek == T_ATTR_REF)
      return apply_type_attribute(name);

   tree_t decl = NULL;
   if (namek == T_REF && tree_has_ref(name))
      decl = aliased_type_decl(tree_ref(name));

   if (decl == NULL) {
      diag_t *d = diag_new(DIAG_ERROR, CURRENT_LOC);
      const char *id = namek == T_REF ? istr(tree_ident(name)) : NULL;
      diag_printf(d, "type mark%s%s does not denote a type or a subtype",
                  id ? " " : "", id ?: "");
      diag_hint(d, CURRENT_LOC, "%s is a %s name", id ?: "this",
                class_str(class_of(name)));
      diag_emit(d);
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
      tree_add_range(t, p_constrained_discrete_range(index_type));
   } while (optional(tCOMMA));

   consume(tRPAREN);

   tree_set_loc(t, CURRENT_LOC);
   return t;
}

static void p_array_constraint(type_t type, type_t base)
{
   // index_constraint [ array_element_constraint ]
   //   | ( open ) [ array_element_constraint ]

   BEGIN("array constraint");

   for (;;) {
      if (peek_nth(2) == tOPEN) {
         consume(tLPAREN);
         consume(tOPEN);
         consume(tRPAREN);

         tree_t c = tree_new(T_CONSTRAINT);
         tree_set_subkind(c, C_OPEN);
         tree_set_loc(c, CURRENT_LOC);

         type_add_constraint(type, c);
      }
      else if (type_is_record(base)) {
         type_add_constraint(type, p_record_constraint(base));
         break;
      }
      else
         type_add_constraint(type, p_index_constraint(base));

      if (peek() != tLPAREN)
         break;

      // Base type may not actually be an array due to earlier errors
      if (type_is_array(base))
         base = type_elem(base);

      type_t sub = type_new(T_SUBTYPE);
      type_set_base(sub, base);

      type_set_elem(type, sub);

      type = sub;
   }
}

static tree_t p_record_element_constraint(type_t base)
{
   // simple_name element_constraint

   BEGIN("record element constraint");

   push_scope_for_fields(nametab, base);

   ident_t id = p_identifier();
   tree_t decl = resolve_name(nametab, CURRENT_LOC, id);

   type_t ftype;
   if (decl != NULL) {
      assert(tree_kind(decl) == T_FIELD_DECL);

      tree_t cons = type_constraint_for_field(base, decl);
      ftype = cons ? tree_type(cons) : tree_type(decl);
   }
   else
      ftype = type_new(T_NONE);

   pop_scope(nametab);

   tree_t elem = tree_new(T_ELEM_CONSTRAINT);
   tree_set_ident(elem, id);
   tree_set_ref(elem, decl);

   type_t sub = type_new(T_SUBTYPE);
   type_set_base(sub, ftype);

   if (type_is_record(ftype))
      type_add_constraint(sub, p_record_constraint(ftype));
   else
      p_array_constraint(sub, ftype);

   tree_set_type(elem, sub);
   tree_set_loc(elem, CURRENT_LOC);
   return elem;
}

static tree_t p_record_constraint(type_t base)
{
   // ( record_element_constraint { , record_element_constraint } )

   BEGIN("record constraint");

   consume(tLPAREN);

   tree_t c = tree_new(T_CONSTRAINT);
   tree_set_subkind(c, C_RECORD);

   do {
      tree_add_range(c, p_record_element_constraint(base));
   } while (optional(tCOMMA));

   consume(tRPAREN);

   tree_set_loc(c, CURRENT_LOC);
   return c;
}

static void p_constraint(type_t type)
{
   // range_constraint | index_constraint
   // 2008: range_constraint | array_constraint | record_constraint

   BEGIN("constraint");

   assert(type_kind(type) == T_SUBTYPE);
   type_t base = type_base(type);

   switch (peek()) {
   case tRANGE:
      type_add_constraint(type, p_range_constraint(base));
      break;

   case tLPAREN:
      if (standard() < STD_08)
         type_add_constraint(type, p_index_constraint(base));
      else if (type_is_record(base))
         type_add_constraint(type, p_record_constraint(base));
      else
         p_array_constraint(type, base);
      break;

   default:
      one_of(tRANGE, tLPAREN);
   }
}

static tree_t p_element_resolution(void)
{
   // array_element_resolution | record_resolution

   BEGIN("element resolution");

   tree_t t = tree_new(T_AGGREGATE);

   do {
      tree_t a = tree_new(T_ASSOC);
      tree_set_subkind(a, A_POS);
      tree_set_value(a, p_resolution_indication());
      tree_set_loc(a, CURRENT_LOC);

      tree_add_assoc(t, a);
   } while (optional(tCOMMA));

   tree_set_loc(t, CURRENT_LOC);
   return t;
}

static tree_t p_resolution_indication(void)
{
   // resolution_function_name | 2008: ( element_resolution )

   BEGIN("resolution indication");

   if (peek() == tID || standard() < STD_08)
      return p_name(N_SUBPROGRAM);
   else {
      one_of(tLPAREN, tID);
      tree_t rname = p_element_resolution();
      consume(tRPAREN);
      return rname;
   }
}

static type_t p_subtype_indication(void)
{
   // [ name ] type_mark [ constraint ]

   BEGIN("subtype indication");

   bool made_subtype = false;
   type_t type = NULL, base = NULL;
   if ((peek() == tID && peek_nth(2) == tID) || peek() == tLPAREN) {
      type = type_new(T_SUBTYPE);
      made_subtype = true;

      tree_t rname = p_resolution_indication();
      type_set_resolution(type, rname);

      type_set_base(type, (base = p_type_mark()));

      resolve_resolution(nametab, rname, type);
   }
   else
      type = p_type_mark();

   if (scan(tRANGE, tLPAREN)) {
      if (!made_subtype) {
         type_t sub = type_new(T_SUBTYPE);
         type_set_base(sub, (base = type));

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
      tree_set_ival(t, last_lval.i64);
      tree_set_type(t, std_type(NULL, STD_UNIVERSAL_INTEGER));
      break;

   case tREAL:
      tree_set_subkind(t, L_REAL);
      tree_set_dval(t, last_lval.real);
      tree_set_type(t, std_type(NULL, STD_UNIVERSAL_REAL));
      break;
   }

   tree_set_loc(t, CURRENT_LOC);
   return t;
}

static tree_t p_physical_literal(void)
{
   // [ abstract_literal ] name

   BEGIN("physical literal");

   tree_t mult;
   if (scan(tINT, tREAL))
      mult = p_abstract_literal();
   else {
      mult = tree_new(T_LITERAL);
      tree_set_ival(mult, 1);
   }

   ident_t ident = p_identifier();

   tree_set_subkind(mult, L_PHYSICAL);
   tree_set_loc(mult, CURRENT_LOC);
   tree_set_ident(mult, ident);
   tree_set_type(mult, NULL);

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

   tree_t t = tree_new(T_STRING);
   tree_set_loc(t, CURRENT_LOC);

   for (const char *p = last_lval.str + 1; *(p + 1) != '\0'; p++) {
      const char ch[] = { '\'', *p, '\'', '\0' };
      tree_t ref = tree_new(T_REF);
      tree_set_loc(ref, CURRENT_LOC);
      tree_set_ident(ref, ident_new(ch));
      tree_add_char(t, ref);
   }

   free(last_lval.str);
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

         tree_t t = bit_string_to_literal(last_lval.str, CURRENT_LOC);
         free(last_lval.str);
         return t;
      }

   default:
      expect(tNULL, tINT, tREAL);
      return error_expr();
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
      tree_t name = p_expression();
      const tree_kind_t name_kind = tree_kind(name);

      bool is_range = false;

      if (scan(tDOWNTO, tTO, tRANGE, tREVRANGE))
         is_range = true;
      else if (constraint != NULL && name_kind == T_REF && tree_has_ref(name))
         is_range = is_type_decl(tree_ref(name));
      else if (name_kind == T_ATTR_REF) {
         const attr_kind_t attr = tree_subkind(name);
         is_range = attr == ATTR_RANGE || attr == ATTR_REVERSE_RANGE;
      }

      tree_t choice = NULL;
      if (is_range) {
         tree_set_subkind(t, A_RANGE);
         tree_add_range(t, (choice = p_discrete_range(name)));
      }
      else {
         tree_set_subkind(t, A_NAMED);
         tree_set_name(t, (choice = name));
      }

      if (constraint != NULL)
         solve_types(nametab, choice, constraint);
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
         p_choice(agg, NULL);
      } while (optional(tBAR));

      consume(tASSOC);

      tree_t value = p_expression();
      const int nassocs = tree_assocs(agg);
      for (int i = nstart; i < nassocs; i++) {
         tree_t a = tree_assoc(agg, i);
         tree_set_value(a, value);
         tree_set_loc(a, CURRENT_LOC);
      }
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

static tree_t p_qualified_expression(tree_t prefix)
{
   // type_mark ' ( expression ) | type_mark ' aggregate

   EXTEND("qualified expression");

   type_t type;
   if (prefix == NULL)
      type = p_type_mark();
   else {
      tree_t decl = NULL;
      if (tree_kind(prefix) == T_REF && tree_has_ref(prefix))
         decl = aliased_type_decl(tree_ref(prefix));

      if (decl != NULL)
         type = tree_type(decl);
      else {
         parse_error(tree_loc(prefix), "expecting type mark while parsing "
                     "qualified expression");
         type = type_new(T_NONE);
      }
   }

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
      value = p_aggregate();
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
   if (peek_nth(2) == tTICK && peek_nth(3) == tLPAREN)
      value = p_qualified_expression(NULL);
   else {
      type_t type = p_subtype_indication();

      value = tree_new(T_QUALIFIED);
      tree_set_type(value, type);
      tree_set_loc(value, CURRENT_LOC);
   }

   tree_set_value(new, value);
   tree_set_loc(new, CURRENT_LOC);

   return new;
}

static tree_t p_primary(void)
{
   // name | literal | aggregate | function_call | qualified_expression
   //   | type_conversion | allocator | ( expression ) |
   //  PSL: Built_In_Function_Call

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
      if (peek_nth(2) != tLPAREN && peek_nth(2) != tDOT)
         return p_literal();
      // Fall-through
   case tID:
      {
         tree_t expr = p_name(0);
         if (tree_kind(expr) == T_PROT_REF)
            return p_function_call(tree_ident(expr), tree_value(expr));
         else
            return expr;
      }

   case tLTLT:
      return p_name(N_SUBPROGRAM);

   case tNEW:
      return p_allocator();

   case tNEXT:
   case tPREV:
   case tSTABLE:
   case tROSE:
   case tFELL:
   case tENDED:
   case tNONDET:
   case tNONDETV:
      assert(is_scanned_as_psl());
      return p_psl_builtin_function_call();

   default:
      expect(tLPAREN, tINT, tREAL, tNULL, tID, tSTRING, tBITSTRING, tNEW);
      return error_expr();
   }
}

static ident_t p_logical_operator(void)
{
   switch (one_of(tAND, tOR, tNAND, tNOR, tXOR, tXNOR)) {
   case tAND:
      return ident_new("\"and\"");
   case tOR:
      return ident_new("\"or\"");
   case tNAND:
      return ident_new("\"nand\"");
   case tNOR:
      return ident_new("\"nor\"");
   case tXOR:
      return ident_new("\"xor\"");
   case tXNOR:
      return ident_new("\"xnor\"");
   default:
      return error_marker();
   }
}

static tree_t p_factor(void)
{
   // primary [ ** primary ] | abs primary | not primary
   //   2008: logical_operator primary

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

   case tAND:
   case tOR:
   case tNAND:
   case tNOR:
   case tXOR:
   case tXNOR:
      if (standard() >= STD_08)
         op = p_logical_operator();
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
      return error_marker();
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
      return error_marker();
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
      return error_marker();
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
      return error_marker();
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
   switch (one_of(tEQ, tNEQ, tLT, tLE, tGT, tGE,
                  tMEQ, tMNEQ, tMLT, tMLE, tMGT, tMGE)) {
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
   case tMEQ:
      return ident_new("\"?=\"");
   case tMNEQ:
      return ident_new("\"?/=\"");
   case tMLT:
      return ident_new("\"?<\"");
   case tMLE:
      return ident_new("\"?<=\"");
   case tMGT:
      return ident_new("\"?>\"");
   case tMGE:
      return ident_new("\"?>=\"");
   default:
      return error_marker();
   }
}

static tree_t p_relation(void)
{
   // shift_expression [ relational_operator shift_expression ]

   BEGIN("relation");

   tree_t rel = p_shift_expression();

   while (scan(tEQ, tNEQ, tLT, tLE, tGT, tGE,
               tMEQ, tMNEQ, tMLT, tMLE, tMGT, tMGE)) {
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
   //   | 2008: condition_operator primary

   BEGIN("expression");

   if (optional(tCCONV)) {
      // VHDL-2008 condition conversion
      tree_t expr = tree_new(T_FCALL);
      tree_set_ident(expr, ident_new("\"??\""));
      unary_op(expr, p_primary);
      return expr;
   }

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
      default:    op = error_marker();
      }

      tree_t left = expr;

      expr = tree_new(T_FCALL);
      tree_set_ident(expr, op);
      binary_op(expr, left, p_relation);
   }

   return expr;
}

static void p_interface_constant_declaration(tree_t parent, tree_kind_t kind,
                                             bool ordered)
{
   // [ constant ] identifier_list : [ in ] subtype_indication [ := expression ]

   BEGIN("interface constant declaration");

   const bool explicit_constant = optional(tCONSTANT);
   tree_flags_t flags = (explicit_constant) ? TREE_F_EXPLICIT_CLASS : 0;

   LOCAL_IDENT_LIST ids = p_identifier_list();

   consume(tCOLON);

   // The grammar only allows IN here but we are more leniant to avoid
   // having disambiguate constant and variable interface declarations
   // See LRM 93 section 2.1.1 for default class
   port_mode_t mode = PORT_IN;
   if (scan(tIN, tOUT, tINOUT, tBUFFER, tLINKAGE)) {
      flags |= TREE_F_EXPLICIT_MODE;
      mode = p_mode();
   }

   class_t class = C_CONSTANT;
   if ((mode == PORT_OUT || mode == PORT_INOUT) && !explicit_constant)
      class = C_VARIABLE;

   type_t type = p_subtype_indication();

   tree_t init = NULL;
   if (optional(tASSIGN)) {
      init = p_expression();
      solve_types(nametab, init, type);
   }

   for (ident_list_t *it = ids; it != NULL; it = it->next) {
      tree_t d = tree_new(kind);
      tree_set_ident(d, it->ident);
      tree_set_loc(d, &(it->loc));
      tree_set_subkind(d, mode);
      tree_set_type(d, type);
      tree_set_class(d, class);
      tree_set_flag(d, flags);

      if (init != NULL)
         tree_set_value(d, init);

      add_interface(parent, d, kind);
      sem_check(d, nametab);

      if (ordered)
         insert_name(nametab, d, NULL);
   }
}

static void p_array_mode_view_indication(type_t *type, tree_t *name)
{
   // view ( name ) of subtype_indication

   BEGIN("array mode view indication");

   consume(tVIEW);
   consume(tLPAREN);

   *name = p_name(0);
   solve_types(nametab, *name, NULL);

   consume(tRPAREN);
   consume(tOF);

   *type = p_subtype_indication();
}

static void p_record_mode_view_indication(type_t *type, tree_t *name)
{
   // view name [ of subtype_indication ]

   BEGIN("record mode view indication");

   consume(tVIEW);

   *name = p_name(0);
   type_t name_type = solve_types(nametab, *name, NULL);

   if (optional(tOF))
      *type = p_subtype_indication();
   else if (type_kind(name_type) != T_VIEW) {
      parse_error(tree_loc(*name), "name in mode view indication does not "
                  "denote a mode view");
      *type = type_new(T_NONE);
   }
   else
      *type = type_designated(name_type);
}

static port_mode_t p_mode_view_indication(type_t *type, tree_t *name)
{
   // record_mode_view_indication | array_mode_view_indication

   BEGIN("mode view indication");

   if (peek_nth(2) == tLPAREN) {
      p_array_mode_view_indication(type, name);
      return PORT_ARRAY_VIEW;
   }
   else {
      p_record_mode_view_indication(type, name);
      return PORT_RECORD_VIEW;
   }
}

static void p_interface_signal_declaration(tree_t parent, tree_kind_t kind,
                                           bool ordered)
{
   // [signal] identifier_list : [ mode ] subtype_indication [ bus ]
   //    [ := expression ]
   // 2019: [ signal ] identifier_list : mode_indication

   BEGIN("interface signal declaration");

   tree_flags_t flags = optional(tSIGNAL) ? TREE_F_EXPLICIT_CLASS : 0;

   LOCAL_IDENT_LIST ids = p_identifier_list();
   consume(tCOLON);

   type_t type = NULL;
   tree_t init = NULL;
   port_mode_t mode = PORT_IN;

   if (peek() == tVIEW) {
      require_std(STD_19, "mode view indication");
      mode = p_mode_view_indication(&type, &init);
   }
   else {
      if (scan(tIN, tOUT, tINOUT, tBUFFER, tLINKAGE)) {
         mode = p_mode();
         flags |= TREE_F_EXPLICIT_MODE;
      }

      type = p_subtype_indication();

      if (optional(tBUS))
         flags |= TREE_F_BUS;

      if (optional(tASSIGN)) {
         init = p_expression();
         solve_types(nametab, init, type);
      }
   }

   for (ident_list_t *it = ids; it != NULL; it = it->next) {
      tree_t d = tree_new(kind);
      tree_set_ident(d, it->ident);
      tree_set_loc(d, &(it->loc));
      tree_set_subkind(d, mode);
      tree_set_type(d, type);
      tree_set_class(d, C_SIGNAL);
      tree_set_flag(d, flags);

      if (init != NULL)
         tree_set_value(d, init);

      add_interface(parent, d, kind);
      sem_check(d, nametab);

      if (ordered)
         insert_name(nametab, d, NULL);
   }
}

static void p_interface_variable_declaration(tree_t parent, tree_kind_t kind)
{
   // [variable] identifier_list : [ mode ] subtype_indication [ := expression ]

   BEGIN("interface variable declaration");

   tree_flags_t flags = optional(tVARIABLE) ? TREE_F_EXPLICIT_CLASS : 0;

   LOCAL_IDENT_LIST ids = p_identifier_list();
   consume(tCOLON);

   port_mode_t mode = PORT_IN;
   if (scan(tIN, tOUT, tINOUT, tBUFFER, tLINKAGE)) {
      mode = p_mode();
      flags |= TREE_F_EXPLICIT_MODE;
   }

   type_t type = p_subtype_indication();

   tree_t init = NULL;
   if (optional(tASSIGN)) {
      init = p_expression();
      solve_types(nametab, init, type);
   }

   for (ident_list_t *it = ids; it != NULL; it = it->next) {
      tree_t d = tree_new(kind);
      tree_set_ident(d, it->ident);
      tree_set_loc(d, &(it->loc));
      tree_set_type(d, type);
      tree_set_class(d, C_VARIABLE);
      tree_set_subkind(d, mode);
      tree_set_flag(d, flags);

      if (init != NULL)
         tree_set_value(d, init);

      add_interface(parent, d, kind);
      sem_check(d, nametab);

      if (standard() >= STD_19)
         insert_name(nametab, d, NULL);
   }
}

static void p_interface_file_declaration(tree_t parent, tree_kind_t kind)
{
   // file identifier_list : subtype_indication

   BEGIN("interface file declaration");

   consume(tFILE);

   LOCAL_IDENT_LIST ids = p_identifier_list();

   consume(tCOLON);

   type_t type = p_subtype_indication();

   for (ident_list_t *it = ids; it != NULL; it = it->next) {
      tree_t d = tree_new(kind);
      tree_set_ident(d, it->ident);
      tree_set_loc(d, &(it->loc));
      tree_set_subkind(d, PORT_IN);
      tree_set_type(d, type);
      tree_set_class(d, C_FILE);

      add_interface(parent, d, kind);
      sem_check(d, nametab);

      if (standard() >= STD_19)
         insert_name(nametab, d, NULL);
   }
}

static void p_private_incomplete_type_definition(type_t type)
{
   // 2019: private

   BEGIN("private incomplete type definition");

   consume(tPRIVATE);

   type_set_subkind(type, GTYPE_PRIVATE);
}

static void p_scalar_incomplete_type_definition(type_t type)
{
   // 2019: <>

   BEGIN("scalar incomplete type definition");

   consume(tBOX);

   type_set_subkind(type, GTYPE_SCALAR);
}

static void p_discrete_incomplete_type_definition(type_t type)
{
   // 2019: ( <> )

   BEGIN("discrete incomplete type definition");

   consume(tLPAREN);
   consume(tBOX);
   consume(tRPAREN);

   type_set_subkind(type, GTYPE_DISCRETE);
}

static void p_integer_incomplete_type_definition(type_t type)
{
   // 2019: range <>

   BEGIN("integer incomplete type definition");

   consume(tRANGE);
   consume(tBOX);

   type_set_subkind(type, GTYPE_INTEGER);
}

static void p_physical_incomplete_type_definition(type_t type)
{
   // 2019: units <>

   BEGIN("physical incomplete type definition");

   consume(tUNITS);
   consume(tBOX);

   type_set_subkind(type, GTYPE_PHYSICAL);
}

static void p_floating_incomplete_type_definition(type_t type)
{
   // 2019: range <> . <>

   BEGIN("floating incomplete type definition");

   consume(tRANGE);
   consume(tBOX);
   consume(tDOT);
   consume(tBOX);

   type_set_subkind(type, GTYPE_FLOATING);
}

static type_t p_array_index_incomplete_type(void)
{
   // index_subtype_definition | index_constraint | anonymous_type_indication

   BEGIN("array index incomplete type");

   if (peek() == tTYPE)
      return p_anonymous_type_indication();
   else
      return p_index_subtype_definition();
}

static void p_array_index_incomplete_type_list(type_t type)
{
   // array_index_incomplete_type { , array_index_incomplete_type }

   BEGIN("array index incomplete type list");

   do {
      type_add_index(type, p_array_index_incomplete_type());
   } while (optional(tCOMMA));
}

static type_t p_incomplete_subtype_indication(void)
{
   // subtype_indication | anonymous_type_indication

   BEGIN("incomplete subtype indication");

   if (peek() == tTYPE)
      return p_anonymous_type_indication();
   else
      return p_subtype_indication();
}

static void p_array_incomplete_type_definition(type_t type)
{
   // 2019: array ( array_index_incomplete_type_list )
   //   of element_incomplete_subtype_indication

   BEGIN("array incomplete type definition");

   consume(tARRAY);
   consume(tLPAREN);

   p_array_index_incomplete_type_list(type);

   consume(tRPAREN);
   consume(tOF);

   type_set_elem(type, p_incomplete_subtype_indication());

   type_set_subkind(type, GTYPE_ARRAY);
}

static void p_access_incomplete_type_definition(type_t type)
{
   // 2019: access incomplete_subtype_indication

   BEGIN("access incomplete type definition");

   consume(tACCESS);

   type_set_designated(type, p_incomplete_subtype_indication());

   type_set_subkind(type, GTYPE_ACCESS);
}

static void p_file_incomplete_type_definition(type_t type)
{
   // 2019: file of incomplete_subtype_indication

   BEGIN("file incomplete type definition");

   consume(tFILE);
   consume(tOF);

   type_set_designated(type, p_incomplete_subtype_indication());

   type_set_subkind(type, GTYPE_FILE);
}

static void p_incomplete_type_definition(type_t type)
{
   // private_incomplete_type_definition
   //   | scalar_incomplete_type_definition
   //   | discrete_incomplete_type_definition
   //   | integer_incomplete_type_definition
   //   | physical_incomplete_type_definition
   //   | floating_incomplete_type_definition
   //   | array_incomplete_type_definition
   //   | access_incomplete_type_definition
   //   | file_incomplete_type_definition

   BEGIN("incomplete type definition");

   require_std(STD_19, "incomplete type definition");

   switch (peek()) {
   case tPRIVATE:
      p_private_incomplete_type_definition(type);
      break;
   case tBOX:
      p_scalar_incomplete_type_definition(type);
      break;
   case tLPAREN:
      p_discrete_incomplete_type_definition(type);
      break;
   case tRANGE:
      if (peek_nth(3) == tDOT)
         p_floating_incomplete_type_definition(type);
      else
         p_integer_incomplete_type_definition(type);
      break;
   case tUNITS:
      p_physical_incomplete_type_definition(type);
      break;
   case tARRAY:
      p_array_incomplete_type_definition(type);
      break;
   case tACCESS:
      p_access_incomplete_type_definition(type);
      break;
   case tFILE:
      p_file_incomplete_type_definition(type);
      break;
   default:
      one_of(tPRIVATE, tBOX, tLPAREN, tRANGE, tUNITS, tARRAY, tACCESS, tFILE);
   }
}

static type_t p_anonymous_type_indication(void)
{
   // type is incomplete_type_definition

   BEGIN("anonymous type indication");

   consume(tTYPE);
   consume(tIS);

   type_t type = type_new(T_GENERIC);
   p_incomplete_type_definition(type);

   return type;
}

static void p_interface_type_declaration(tree_t parent, tree_kind_t kind)
{
   // 2008: type identifier
   // 2019: type identifier [ is incomplete_type_definition ]

   BEGIN("interface type declaration");

   consume(tTYPE);

   ident_t id = p_identifier();

   require_std(STD_08, "interface type declarations");

   type_t type = type_new(T_GENERIC);
   type_set_ident(type, id);

   if (optional(tIS))
      p_incomplete_type_definition(type);
   else
      type_set_subkind(type, GTYPE_PRIVATE);

   tree_t d = tree_new(kind);
   tree_set_ident(d, id);
   tree_set_loc(d, CURRENT_LOC);
   tree_set_type(d, type);
   tree_set_class(d, C_TYPE);
   tree_set_subkind(d, PORT_IN);

   add_interface(parent, d, kind);
   sem_check(d, nametab);

   // Type generics are immediately visible
   insert_name(nametab, d, NULL);

   // LRM 08 section 6.5.3: the predefined equality and inequality
   // operators are implicitly declared as formal generic subprograms
   // immediately following the interface type declaration in the
   // enclosing interface list

   type_t std_bool = std_type(NULL, STD_BOOLEAN);
   type_t std_string = std_type(NULL, STD_STRING);

   const gtype_class_t class = type_subkind(type);

   switch (class) {
   case GTYPE_INTEGER:
      add_generic_type_op(parent, 2, type, type, "\"**\"");
      // Fall-through
   case GTYPE_PHYSICAL:
      add_generic_type_op(parent, 2, type, type, "\"mod\"");
      add_generic_type_op(parent, 2, type, type, "\"rem\"");
      // Fall-through
   case GTYPE_FLOATING:
      add_generic_type_op(parent, 2, type, type, "\"+\"");
      add_generic_type_op(parent, 2, type, type, "\"-\"");
      add_generic_type_op(parent, 1, type, type, "\"+\"");
      add_generic_type_op(parent, 1, type, type, "\"-\"");
      if (class != GTYPE_PHYSICAL) {
         add_generic_type_op(parent, 2, type, type, "\"*\"");
         add_generic_type_op(parent, 2, type, type, "\"/\"");
      }
      add_generic_type_op(parent, 1, type, type, "\"abs\"");
      // Fall-through
   case GTYPE_DISCRETE:
   case GTYPE_SCALAR:
      add_generic_type_op(parent, 2, type, std_bool, "\"<\"");
      add_generic_type_op(parent, 2, type, std_bool, "\">\"");
      add_generic_type_op(parent, 2, type, std_bool, "\"<=\"");
      add_generic_type_op(parent, 2, type, std_bool, "\">=\"");
      add_generic_type_op(parent, 2, type, type, "MINIMUM");
      add_generic_type_op(parent, 2, type, type, "MAXIMUM");
      add_generic_type_op(parent, 1, type, std_string, "TO_STRING");
      // Fall-through
   case GTYPE_ARRAY:
   case GTYPE_FILE:
   case GTYPE_ACCESS:
   case GTYPE_PRIVATE:
      add_generic_type_op(parent, 2, type, std_bool, "\"=\"");
      add_generic_type_op(parent, 2, type, std_bool, "\"/=\"");
      break;
   }

   switch (class) {
   case GTYPE_ACCESS:
      {
         ident_t id = ident_new("DEALLOCATE");

         type_t ftype = type_new(T_PROC);
         type_set_ident(ftype, id);
         type_add_param(ftype, type);

         tree_t p = tree_new(T_GENERIC_DECL);
         tree_set_class(p, C_PROCEDURE);
         tree_set_ident(p, id);
         tree_set_type(p, ftype);
         tree_set_subkind(p, PORT_IN);
         tree_set_loc(p, CURRENT_LOC);
         tree_set_flag(p, TREE_F_PREDEFINED);

         tree_t box = tree_new(T_BOX);
         tree_set_loc(box, CURRENT_LOC);

         tree_set_value(p, box);

         tree_t arg = tree_new(T_PORT_DECL);
         tree_set_ident(arg, ident_new("PTR"));
         tree_set_type(arg, type);
         tree_set_subkind(arg, PORT_INOUT);
         tree_set_class(arg, C_VARIABLE);
         tree_set_loc(arg, CURRENT_LOC);

         tree_add_port(p, arg);

         add_interface(parent, p, T_GENERIC_DECL);
         insert_name(nametab, p, NULL);
      }
      break;

   default:
      break;
   }
}

static tree_t p_interface_function_specification(void)
{
   // [ pure | impure ] function designator
   //    [ [ parameter ] ( formal_parameter_list ) ] return type_mark

   // 2019:
   // [ pure | impure ] function designator
   //    [ [ parameter ] ( formal_parameter_list ) ]
   //    return [ return_identifier of ] type_mark

   BEGIN("interface function specification");

   consume(tFUNCTION);

   ident_t id = p_designator();

   type_t type = type_new(T_FUNC);
   type_set_ident(type, id);

   tree_t d = tree_new(T_GENERIC_DECL);
   tree_set_class(d, C_FUNCTION);
   tree_set_ident(d, id);
   tree_set_type(d, type);
   tree_set_subkind(d, PORT_IN);

   if (optional(tLPAREN)) {
      push_scope(nametab);

      p_interface_list(d, T_PARAM_DECL, false);
      consume(tRPAREN);

      const int nports = tree_ports(d);
      for (int i = 0; i < nports; i++)
         type_add_param(type, tree_type(tree_port(d, i)));

      pop_scope(nametab);
   }

   consume(tRETURN);

   if (peek_nth(2) != tOF)
      type_set_result(type, p_type_mark());
   else {
      require_std(STD_19, "function knows return type");
      ident_t id = p_identifier();

      consume(tOF);

      type_t sub = type_new(T_SUBTYPE);
      type_set_ident(sub, id);
      type_set_base(sub, p_type_mark());

      type_set_result(type, sub);
   }

   tree_set_loc(d, CURRENT_LOC);
   return d;
}

static tree_t p_interface_procedure_specification(void)
{
   // procedure designator [ [ parameter ] ( formal_parameter_list ) ]

   BEGIN("interface procedure specification");

   consume(tPROCEDURE);

   ident_t id = p_designator();

   type_t type = type_new(T_PROC);
   type_set_ident(type, id);

   tree_t d = tree_new(T_GENERIC_DECL);
   tree_set_class(d, C_PROCEDURE);
   tree_set_ident(d, id);
   tree_set_type(d, type);
   tree_set_subkind(d, PORT_IN);

   if (optional(tLPAREN)) {
      push_scope(nametab);

      p_interface_list(d, T_PARAM_DECL, false);
      consume(tRPAREN);

      const int nports = tree_ports(d);
      for (int i = 0; i < nports; i++)
         type_add_param(type, tree_type(tree_port(d, i)));

      pop_scope(nametab);
   }

   tree_set_loc(d, CURRENT_LOC);
   return d;
}

static void p_interface_subprogram_declaration(tree_t parent, tree_kind_t kind)
{
   // interface_subprogram_specification [ is interface_subprogram_default ]

   BEGIN("interface subprogram declaration");

   tree_t d = NULL;

   require_std(STD_08, "interface subprogram declarations");

   switch (peek()) {
   case tFUNCTION:
   case tPURE:
   case tIMPURE:
      d = p_interface_function_specification();
      break;

   case tPROCEDURE:
      d = p_interface_procedure_specification();
      break;

   default:
      one_of(tFUNCTION, tPROCEDURE, tPURE, tIMPURE);
      return;
   }

   if (optional(tIS)) {
      switch (peek()) {
      case tID:
         p_identifier();
         break;
      case tBOX:
         {
            consume(tBOX);

            tree_t box = tree_new(T_BOX);
            tree_set_loc(box, CURRENT_LOC);
            tree_set_type(box, tree_type(d));

            tree_set_value(d, box);
         }
         break;
      default:
         expect(tID, tBOX);
      }
   }

   add_interface(parent, d, kind);
   sem_check(d, nametab);

   insert_name(nametab, d, NULL);
}

static void p_interface_package_generic_map_aspect(tree_t map, tree_t pack)
{
   // generic_map_aspect | generic map ( <> ) | generic map ( default )

   BEGIN("interface package generic map aspect");

   consume(tGENERIC);
   consume(tMAP);
   consume(tLPAREN);

   switch (peek()) {
   case tBOX:
      consume(tBOX);
      tree_set_subkind(map, PACKAGE_MAP_BOX);
      break;

   case tDEFAULT:
      consume(tDEFAULT);
      tree_set_subkind(map, PACKAGE_MAP_DEFAULT);
      break;

   default:
      tree_set_subkind(map, PACKAGE_MAP_MATCHING);
      p_association_list(map, pack, F_GENERIC_MAP);
      break;
   }

   consume(tRPAREN);

   tree_set_loc(map, CURRENT_LOC);
}

static void p_interface_package_declaration(tree_t parent, tree_kind_t kind)
{
   // package identifier is new uninstantiated_package_name
   //    interface_package_generic_map_aspect

   BEGIN("interface package declaration");

   consume(tPACKAGE);

   require_std(STD_08, "interface package declarations");

   tree_t d = tree_new(T_GENERIC_DECL);
   tree_set_class(d, C_PACKAGE);
   tree_set_ident(d, p_identifier());
   tree_set_subkind(d, PORT_IN);

   consume(tIS);
   consume(tNEW);

   ident_t unit_name = p_selected_identifier();

   tree_t pack = resolve_name(nametab, CURRENT_LOC, unit_name);
   if (pack != NULL && !is_uninstantiated_package(pack)) {
      parse_error(CURRENT_LOC, "unit %s is not an uninstantiated package",
                  istr(unit_name));
      pack = NULL;
   }

   tree_t map = tree_new(T_PACKAGE_MAP);
   tree_set_ident(map, unit_name);
   tree_set_loc(map, CURRENT_LOC);
   tree_set_ref(map, pack);

   tree_set_value(d, map);

   p_interface_package_generic_map_aspect(map, pack);

   tree_set_loc(d, CURRENT_LOC);

   add_interface(parent, d, kind);
   sem_check(d, nametab);

   insert_name(nametab, d, NULL);
}

static void p_interface_declaration(tree_t parent, tree_kind_t kind,
                                    bool ordered)
{
   // interface_constant_declaration | interface_signal_declaration
   //   | interface_variable_declaration | interface_file_declaration
   //   | 2008: interface_type_declaration
   //   | 2008: interface_subprogram_declaration
   //   | 2008: interface_package_declaration

   BEGIN("interface declaration");

   const token_t p = peek();
   switch (p) {
   case tCONSTANT:
      p_interface_constant_declaration(parent, kind, ordered);
      break;

   case tSIGNAL:
      p_interface_signal_declaration(parent, kind, ordered);
      break;

   case tVARIABLE:
      p_interface_variable_declaration(parent, kind);
      break;

   case tFILE:
      p_interface_file_declaration(parent, kind);
      break;

   case tTYPE:
      p_interface_type_declaration(parent, kind);
      break;

   case tFUNCTION:
   case tPROCEDURE:
      p_interface_subprogram_declaration(parent, kind);
      break;

   case tPACKAGE:
      p_interface_package_declaration(parent, kind);
      break;

   case tID:
      if (kind == T_PORT_DECL)
         p_interface_signal_declaration(parent, kind, ordered);
      else
         p_interface_constant_declaration(parent, kind, ordered);
      break;

   default:
      expect(tCONSTANT, tSIGNAL, tVARIABLE, tFILE, tID, tTYPE);
   }
}

static void p_interface_element(tree_t parent, tree_kind_t kind, bool ordered)
{
   // interface_declaration

   BEGIN("interface element");

   p_interface_declaration(parent, kind, ordered);
}

static void p_interface_list(tree_t parent, tree_kind_t kind, bool ordered)
{
   // interface_element { ; interface_element }

   BEGIN("interface list");

   if (ordered)
      push_scope(nametab);

   p_interface_element(parent, kind, ordered);

   while (optional(tSEMI)) {
      if (peek() == tRPAREN) {
         require_std(STD_19, "optional trailing semicolons on interface lists");
         break;
      }
      p_interface_element(parent, kind, ordered);
   }

   if (ordered)
      pop_scope(nametab);
}

static void p_port_list(tree_t parent)
{
   // port_list ::= interface_list

   BEGIN("port list");

   p_interface_list(parent, T_PORT_DECL, standard() >= STD_19);
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

   p_interface_list(parent, T_GENERIC_DECL, standard() >= STD_08);
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

   if (scan(tGENERIC)) {
      p_generic_clause(entity);
      insert_generics(nametab, entity);
   }

   if (scan(tPORT)) {
      p_port_clause(entity);
      insert_ports(nametab, entity);
   }
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
   insert_name(nametab, t, NULL);
   sem_check(t, nametab);
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

static ident_t p_entity_designator()
{
   // entity_tag [ signature ]
   ident_t id = p_identifier();

   if (peek() == tLSQUARE) {
      // XXX: Review what to do here
      (void)p_signature();
      warn_at(CURRENT_LOC, "sorry, signature in attribute entity name list is not yet supported");
   }
   return id;
}

static ident_list_t *p_entity_name_list(void)
{
   // entity_designator { , entity_designator } | others | all
   ident_list_t *result = NULL;

   switch (peek()) {
   case tOTHERS:
      consume(tOTHERS);
      break;
   case tALL:
      consume(tALL);
      ident_list_push(&result, well_known(W_ALL), last_loc);
      break;
   default:
      ident_list_push(&result, p_entity_designator(), last_loc);

      while (optional(tCOMMA))
         ident_list_push(&result, p_entity_designator(), last_loc);
   }
   return result;
}

static ident_list_t *p_entity_specification(class_t *class)
{
   // entity_name_list : entity_class

   BEGIN("entity specification");

   ident_list_t *ids = p_entity_name_list();

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

      if (class != C_LITERAL && class != C_LABEL) {
         tree_t d = resolve_name(nametab, loc, it->ident);
         if (d != NULL && class_of(d) != class)
            parse_error(loc, "class of object %s is %s not %s",
                        istr(it->ident), class_str(class_of(d)),
                        class_str(class));

         if (d != NULL && head == well_known(W_FOREIGN))
            apply_foreign_attribute(d, value);
      }

      insert_name(nametab, t, NULL);
      (*addf)(parent, t);
      sem_check(t, nametab);
   }
}

static type_t p_integer_type_definition(tree_t r, ident_t id)
{
   // range_constraint

   EXTEND("integer type definition");

   type_t t = type_new(T_INTEGER);
   type_add_dim(t, r);
   type_set_ident(t, id);
   mangle_type(nametab, t);

   return t;
}

static type_t p_real_type_definition(tree_t r, ident_t id)
{
   // range_constraint

   EXTEND("real type definition");

   type_t t = type_new(T_REAL);
   type_add_dim(t, r);
   type_set_ident(t, id);
   mangle_type(nametab, t);

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
   tree_set_ident(value, id);
   tree_set_ival(value, 1);

   tree_t t = tree_new(T_UNIT_DECL);
   tree_set_loc(t, CURRENT_LOC);
   tree_set_value(t, value);
   tree_set_ident(t, id);

   return t;
}

static tree_t p_secondary_unit_declaration(type_t type)
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
   tree_set_type(u, type);

   return u;
}

static type_t p_physical_type_definition(tree_t range, ident_t id)
{
   // range_constraint units base_unit_declaration
   //   { secondary_unit_declaration } end units [ name ]

   EXTEND("physical type definition");

   type_t t = type_new(T_PHYSICAL);
   type_set_ident(t, id);
   mangle_type(nametab, t);

   consume(tUNITS);

   tree_t base = p_base_unit_declaration();
   tree_set_type(base, t);
   tree_set_type(tree_value(base), t);
   type_add_unit(t, base);
   type_add_dim(t, range);

   push_scope(nametab);

   insert_name(nametab, base, NULL);

   while (scan(tINT, tREAL, tID)) {
      tree_t unit = p_secondary_unit_declaration(t);
      type_add_unit(t, unit);
      insert_name(nametab, unit, NULL);
   }

   pop_scope(nametab);

   consume(tEND);
   consume(tUNITS);

   if (peek() == tID) {
      ident_t trailing = p_identifier();
      if (trailing != id)
         parse_error(&yylloc, "expected physical type definition trailing "
                     "identifier to match %s", istr(id));
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

static type_t p_enumeration_type_definition(ident_t id)
{
   // ( enumeration_literal { , enumeration_literal } )

   BEGIN("enumeration type definition");

   type_t t = type_new(T_ENUM);
   type_set_ident(t, id);
   mangle_type(nametab, t);

   consume(tLPAREN);

   unsigned pos = 0;
   do {
      tree_t lit = p_enumeration_literal();
      tree_set_pos(lit, pos++);
      tree_set_type(lit, t);
      type_enum_add_literal(t, lit);
   } while (optional(tCOMMA));

   tree_t r = tree_new(T_RANGE);
   tree_set_subkind(r, RANGE_TO);
   tree_set_left(r, make_ref(type_enum_literal(t, 0)));
   tree_set_right(r, make_ref(type_enum_literal(t, pos - 1)));
   tree_set_loc(r, CURRENT_LOC);
   tree_set_type(r, t);

   type_add_dim(t, r);

   consume(tRPAREN);

   return t;
}

static type_t p_scalar_type_definition(ident_t id)
{
   // enumeration_type_definition | integer_type_definition
   //   | floating_type_definition | physical_type_definition

   BEGIN("scalar type definition");

   switch (peek()) {
   case tRANGE:
      {
         tree_t r = tree_range(p_range_constraint(NULL), 0);

         if (peek() == tUNITS)
            return p_physical_type_definition(r, id);
         else if (type_is_real(tree_type(r)))
            return p_real_type_definition(r, id);
         else
            return p_integer_type_definition(r, id);
      }

   case tLPAREN:
      return p_enumeration_type_definition(id);

   default:
      one_of(tRANGE, tLPAREN);
      return type_new(T_NONE);
   }
}

static type_t p_access_type_definition(ident_t id)
{
   // access subtype_indication

   BEGIN("access type definition");

   consume(tACCESS);

   type_t t = type_new(T_ACCESS);
   type_set_ident(t, id);
   type_set_designated(t, p_subtype_indication());
   mangle_type(nametab, t);

   return t;
}

static type_t p_file_type_definition(ident_t id)
{
   // file of type_mark

   BEGIN("file type definition");

   consume(tFILE);
   consume(tOF);

   type_t t = type_new(T_FILE);
   type_set_ident(t, id);
   type_set_designated(t, p_type_mark());
   mangle_type(nametab, t);

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

   int pos = type_fields(rec);
   for (ident_list_t *it = ids; it != NULL; it = it->next) {
      tree_t f = tree_new(T_FIELD_DECL);
      tree_set_ident(f, it->ident);
      tree_set_type(f, type);
      tree_set_pos(f, pos++);
      tree_set_loc(f, &(it->loc));

      type_add_field(rec, f);
   }
}

static type_t p_record_type_definition(ident_t id)
{
   // record element_declaration { element_declaration } end record
   //   [ simple_name ]

   // 2019: record { element_declaration } end record [ simple_name ]
   BEGIN("record type definition");

   consume(tRECORD);

   type_t r = type_new(T_RECORD);
   type_set_ident(r, id);
   mangle_type(nametab, r);

   if (peek() == tEND)
      require_std(STD_19, "empty record");
   else {
      do {
         p_element_declaration(r);
      } while (peek() == tID);
   }

   consume(tEND);
   consume(tRECORD);

   if (peek() == tID) {
      ident_t trailing = p_identifier();
      if (trailing != id)
         parse_error(&yylloc, "expected record type definition trailing "
                     "identifier to match %s", istr(id));
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

static type_t p_unconstrained_array_definition(ident_t id)
{
   // array ( index_subtype_definition { , index_subtype_definition } )
   //   of subtype_indication

   BEGIN("unconstrained array definition");

   consume(tARRAY);
   consume(tLPAREN);

   type_t t = type_new(T_ARRAY);
   type_set_ident(t, id);
   do {
      type_add_index(t, p_index_subtype_definition());
   } while (optional(tCOMMA));

   mangle_type(nametab, t);

   consume(tRPAREN);
   consume(tOF);

   type_set_elem(t, p_subtype_indication());
   return t;
}

static type_t p_constrained_array_definition(ident_t id)
{
   // array index_constraint of element_subtype_indication

   BEGIN("constrained array definition");

   consume(tARRAY);

   type_t base = type_new(T_ARRAY);
   type_set_ident(base, id);

   tree_t constraint = tree_new(T_CONSTRAINT);
   tree_set_loc(constraint, CURRENT_LOC);
   tree_set_subkind(constraint, C_INDEX);

   type_t sub = type_new(T_SUBTYPE);
   type_set_base(sub, base);
   type_add_constraint(sub, constraint);
   type_set_ident(sub, id);

   mangle_type(nametab, sub);

   consume(tLPAREN);
   do {
      type_t index_type = std_type(NULL, STD_INTEGER);
      tree_t r = p_constrained_discrete_range(index_type);
      tree_add_range(constraint, r);
      type_add_index(base, tree_type(r));
   } while (optional(tCOMMA));
   consume(tRPAREN);

   consume(tOF);

   type_set_elem(base, p_subtype_indication());
   return sub;
}

static type_t p_array_type_definition(ident_t id)
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
      return p_unconstrained_array_definition(id);
   else
      return p_constrained_array_definition(id);
}

static type_t p_composite_type_definition(ident_t id)
{
   // array_type_definition | record_type_definition

   BEGIN("composite type definition");

   switch (peek()) {
   case tRECORD:
      return p_record_type_definition(id);

   case tARRAY:
      return p_array_type_definition(id);

   default:
      expect(tRECORD, tARRAY);
      return type_new(T_NONE);
   }
}

static void p_private_variable_declaration(tree_t decl)
{
   // 2019: private variable_declaration

   BEGIN("private variable declaration");

   consume(tPRIVATE);

   require_std(STD_19, "private variable declarations");

   p_variable_declaration(decl);
}

static void p_protected_type_declarative_item(tree_t decl)
{
   // subprogram_declaration | 2008: subprogram_instantiation_declaration
   //   | attribute_specification | use_clause
   //   | 2019: private_variable_declaration | alias_declaration

   BEGIN("protected type declarative item");

   switch (peek()) {
   case tATTRIBUTE:
      p_attribute_specification(decl, tree_add_decl);
      break;

   case tUSE:
      p_use_clause(decl, tree_add_decl);
      break;

   case tFUNCTION:
   case tPROCEDURE:
   case tIMPURE:
   case tPURE:
      if (peek_nth(3) == tIS && peek_nth(4) == tNEW)
         tree_add_decl(decl, p_subprogram_instantiation_declaration());
      else {
         tree_t spec = p_subprogram_specification();
         tree_set_flag(spec, TREE_F_PROTECTED);
         tree_add_decl(decl, p_subprogram_declaration(spec));
      }
      break;

   case tPRIVATE:
      p_private_variable_declaration(decl);
      break;

   case tALIAS:
      p_alias_declaration(decl);
      break;

   default:
      expect(tATTRIBUTE, tUSE, STD(08, tFUNCTION), tPROCEDURE, tIMPURE, tPURE,
             STD(19, tPRIVATE), tALIAS);
   }
}

static void p_protected_type_declarative_part(tree_t decl)
{
   // { protected_type_declarative_item }

   BEGIN("protected type declarative part");

   while (not_at_token(tEND))
      p_protected_type_declarative_item(decl);
}

static tree_t p_protected_type_declaration(ident_t id)
{
   // protected protected_type_declarative_part end protected [ simple_name ]

   BEGIN("protected type declaration");

   consume(tPROTECTED);

   type_t type = type_new(T_PROTECTED);
   type_set_ident(type, id);

   mangle_type(nametab, type);

   tree_t t = tree_new(T_PROT_DECL);
   tree_set_ident(t, id);
   tree_set_type(t, type);

   insert_name(nametab, t, NULL);

   push_scope(nametab);
   scope_set_prefix(nametab, id);

   p_protected_type_declarative_part(t);

   const int ndecls = tree_decls(t);
   for (int i = 0; i < ndecls; i++) {
      tree_t d = tree_decl(t, i);
      if (is_subprogram(d) || tree_kind(d) == T_ALIAS)
         type_add_field(type, d);
   }

   pop_scope(nametab);

   consume(tEND);
   consume(tPROTECTED);

   p_trailing_label(id);

   tree_set_loc(t, CURRENT_LOC);
   sem_check(t, nametab);
   return t;
}

static tree_t p_protected_type_definition(ident_t id)
{
   // protected_type_declaration | protected_type_body

   BEGIN("protected type definition");

   if (peek_nth(2) == tBODY)
      return p_protected_type_body(id);
   else
      return p_protected_type_declaration(id);
}

static type_t p_type_definition(tree_t tdecl)
{
   // scalar_type_definition | composite_type_definition
   //   | access_type_definition | file_type_definition
   //   | 2000: protected_type_definition

   BEGIN("type definition");

   ident_t id = tree_ident(tdecl);

   switch (peek()) {
   case tRANGE:
   case tLPAREN:
      return p_scalar_type_definition(id);

   case tACCESS:
      return p_access_type_definition(id);

   case tFILE:
      return p_file_type_definition(id);

   case tRECORD:
   case tARRAY:
      return p_composite_type_definition(id);

   default:
      expect(tRANGE, tACCESS, tFILE, tRECORD);
      return type_new(T_NONE);
   }
}

static type_t p_full_type_declaration(tree_t tdecl)
{
   // type identifier is type_definition ;

   EXTEND("full type declaration");

   consume(tIS);

   type_t t = p_type_definition(tdecl);

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
   hide_name(nametab, id);

   // Protected type definitions are trees rather than types
   if (peek_nth(1) == tIS && peek_nth(2) == tPROTECTED) {
      consume(tIS);
      tree_add_decl(container, p_protected_type_definition(id));
      consume(tSEMI);
   }
   else {
      // Insert univeral_integer and universal_real predefined functions
      // before STD.INTEGER and STD.REAL respectively
      if (bootstrapping) {
         if (id == ident_new("INTEGER"))
            make_universal_int(container);
         else if (id == ident_new("REAL"))
            make_universal_real(container);
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

      insert_name(nametab, t, id);
      tree_add_decl(container, t);

      const type_kind_t kind = type_kind(type);

      type_t base = type;
      if (kind == T_SUBTYPE) {
         base = type_base(type);
         assert(type_kind(base) == T_ARRAY);
         mangle_type(nametab, base);
      }

      if (kind != T_INCOMPLETE)
         declare_predefined_ops(container, base);

      if (kind == T_PHYSICAL) {
         const int nunits = type_units(type);
         for (int i = 0; i < nunits; i++)
            solve_types(nametab, tree_value(type_unit(type, i)), type);
      }

      sem_check(t, nametab);
   }
}

static tree_t p_subtype_declaration(void)
{
   // subtype identifier is subtype_indication ;

   BEGIN("subtype declaration");

   consume(tSUBTYPE);

   ident_t id = p_identifier();
   hide_name(nametab, id);

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

   mangle_type(nametab, sub);

   tree_t t = tree_new(T_SUBTYPE_DECL);
   tree_set_ident(t, id);
   tree_set_type(t, sub);
   tree_set_loc(t, CURRENT_LOC);

   insert_name(nametab, t, id);
   sem_check(t, nametab);
   return t;
}

static tree_t p_conditional_expression(void)
{
   // expression { when condition else expression }

   BEGIN("conditional expression");

   tree_t expr0 = p_expression();

   if (optional(tWHEN)) {
      require_std(STD_19, "conditional expressions");

      tree_t value = tree_new(T_COND_VALUE);

      do {
         tree_t cond = tree_new(T_COND_EXPR);
         tree_set_result(cond, expr0);
         tree_set_value(cond, p_condition());

         tree_set_loc(cond, CURRENT_LOC);

         tree_add_cond(value, cond);

         consume(tELSE);

         expr0 = p_expression();
      } while (optional(tWHEN));

      tree_t last = tree_new(T_COND_EXPR);
      tree_set_result(last, expr0);
      tree_set_loc(last, CURRENT_LOC);

      tree_add_cond(value, last);

      tree_set_loc(value, CURRENT_LOC);
      return value;
   }
   else
      return expr0;
}

static tree_t p_expression_or_unaffected(void)
{
   // expression | unaffected

   BEGIN("expression or unaffected");

   if (optional(tUNAFFECTED)) {
      require_std(STD_19, "unaffected in conditional expression");
      return NULL;
   }
   else
      return p_expression();
}

static tree_t p_conditional_or_unaffected_expression(vhdl_standard_t minstd)
{
   // expression_or_unaffected { when condition else expression_or_unaffected }
   //   [ when condition ]

   BEGIN("conditional expression");

   tree_t expr0 = p_expression_or_unaffected();

   if (optional(tWHEN)) {
      require_std(minstd, "conditional expressions");

      tree_t value = tree_new(T_COND_VALUE);

      tree_t c0 = tree_new(T_COND_EXPR);
      tree_set_result(c0, expr0);
      tree_set_value(c0, p_condition());
      tree_set_loc(c0, CURRENT_LOC);

      tree_add_cond(value, c0);

      while (optional(tELSE)) {
         tree_t cond = tree_new(T_COND_EXPR);
         tree_set_result(cond, p_expression_or_unaffected());
         tree_set_loc(cond, CURRENT_LOC);

         tree_add_cond(value, cond);

         if (!optional(tWHEN))
            break;

         tree_set_value(cond, p_condition());
         tree_set_loc(cond, CURRENT_LOC);
      }

      tree_set_loc(value, CURRENT_LOC);
      return value;
   }
   else
      return expr0;
}

static void p_constant_declaration(tree_t parent)
{
   // constant identifier_list : subtype_indication [ := expression ] ;

   BEGIN("constant declaration");

   consume(tCONSTANT);

   LOCAL_IDENT_LIST ids = p_identifier_list();

   for (ident_list_t *it = ids; it != NULL; it = it->next)
      hide_name(nametab, it->ident);

   consume(tCOLON);

   type_t type = p_subtype_indication();

   tree_t init = NULL;
   if (optional(tASSIGN)) {
      init = p_conditional_expression();

      if (standard() < STD_19 || type_is_unconstrained(type))
         solve_types(nametab, init, type);
      else
         solve_known_subtype(nametab, init, type);
   }

   consume(tSEMI);

   for (ident_list_t *it = ids; it != NULL; it = it->next) {
      tree_t t = tree_new(T_CONST_DECL);
      tree_set_ident(t, it->ident);
      tree_set_type(t, type);
      tree_set_loc(t, &(it->loc));
      if (init != NULL)
         tree_set_value(t, init);

      tree_add_decl(parent, t);

      insert_name(nametab, t, it->ident);
      sem_check(t, nametab);
   }
}

static tree_t p_psl_condition(void)
{
   BEGIN("condition");

   scan_as_vhdl();

   tree_t value = p_expression();
   solve_psl_condition(nametab, value);

   scan_as_psl();

   return value;
}

static tree_t p_condition(void)
{
   BEGIN("condition");

   type_t boolean = std_type(NULL, STD_BOOLEAN);

   tree_t value = p_expression();
   solve_condition(nametab, &value, boolean);

   return value;
}

static tree_t p_assertion(void)
{
   // assert condition [ report expression ] [ severity expression ]

   BEGIN("assertion");

   tree_t s = tree_new(T_ASSERT);

   consume(tASSERT);

   tree_set_value(s, p_condition());

   if (optional(tREPORT)) {
      tree_t message = p_expression();
      solve_types(nametab, message, std_type(NULL, STD_STRING));
      tree_set_message(s, message);
   }

   tree_t severity;
   if (optional(tSEVERITY))
      severity = p_expression();
   else {
      tree_t std = find_std(nametab);
      severity = make_ref(search_decls(std, ident_new("ERROR"), 0));
   }

   solve_types(nametab, severity, std_type(NULL, STD_SEVERITY_LEVEL));
   tree_set_severity(s, severity);

   tree_set_loc(s, CURRENT_LOC);
   return s;
}

static tree_t p_concurrent_assertion_statement(ident_t label)
{
   // [ label : ] [ postponed ] assertion ;

   BEGIN("concurrent assertion statement");

   tree_t conc = tree_new(T_CONCURRENT);

   if (optional(tPOSTPONED))
      tree_set_flag(conc, TREE_F_POSTPONED);

   tree_t s = p_assertion();
   tree_set_ident(s, get_implicit_label(s, nametab));
   tree_add_stmt(conc, s);

   consume(tSEMI);

   tree_set_loc(s, CURRENT_LOC);
   tree_set_loc(conc, CURRENT_LOC);

   ensure_labelled(conc, label);

   if (label) insert_name(nametab, conc, NULL);
   sem_check(conc, nametab);
   return conc;
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
      return error_marker();
   }
}

static void p_subprogram_header(tree_t spec)
{
   // 2008: [ generic ( generic_list ) [ generic_map_aspect ] ]

   if (optional(tGENERIC)) {
      require_std(STD_08, "generic subprograms");

      consume(tLPAREN);
      p_generic_list(spec);
      consume(tRPAREN);

      insert_generics(nametab, spec);
   }
}

static tree_t p_subprogram_specification(void)
{
   // procedure designator subprogram_header
   //       [ [parameter] ( formal_parameter_list ) ]
   //   | [ pure | impure ] function designator subprogram_header
   //       [ [parameter] ( formal_parameter_list ) ] return type_mark

   // 2019:
   //  [ pure | impure ] function designator subprogram_header
   //       [ [parameter] ( formal_parameter_list ) ]
   //       return [ return_identifier of ] type_mark

   BEGIN("subprogram specification");

   tree_t t = NULL;
   type_t type = NULL;

   bool impure = false;
   switch (one_of(tFUNCTION, tPROCEDURE, tPURE, tIMPURE)) {
   case tIMPURE:
      impure = true;
      // Fall-through
   case tPURE:
      consume(tFUNCTION);
      // Fall-through
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
   tree_set_subkind(t, S_USER);

   type_set_ident(type, tree_ident(t));

   if (impure)
      tree_set_flag(t, TREE_F_IMPURE);

   push_scope(nametab);

   p_subprogram_header(t);

   bool has_param_list = false;
   if (optional(tLPAREN))
      has_param_list = true;
   else if (standard() >= STD_08 && optional(tPARAMETER)) {
      consume(tLPAREN);
      has_param_list = true;
   }

   if (has_param_list) {
      p_interface_list(t, T_PARAM_DECL, standard() >= STD_19);
      consume(tRPAREN);

      const int nports = tree_ports(t);
      for (int i = 0; i < nports; i++)
         type_add_param(type, tree_type(tree_port(t, i)));
   }

   if (tree_kind(t) == T_FUNC_DECL) {
      consume(tRETURN);

      if (peek_nth(2) != tOF)
         type_set_result(type, p_type_mark());
      else {
         require_std(STD_19, "function knows return type");

         ident_t id = p_identifier();

         consume(tOF);

         type_t of = p_type_mark(), sub;

         if (type_is_unconstrained(of)) {
            tree_t rref = tree_new(T_REF);
            tree_set_loc(rref, CURRENT_LOC);
            tree_set_ident(rref, ident_new("result"));
            tree_set_ref(rref, t);
            tree_set_type(rref, of);

            sub = get_subtype_for(rref);
         }
         else {
            parse_error(CURRENT_LOC, "type mark of return identifier must "
                        "denote an unconstrained type");

            sub = type_new(T_SUBTYPE);
            type_set_base(sub, of);
         }

         type_set_ident(sub, id);
         type_set_result(type, sub);

         tree_set_flag(t, TREE_F_KNOWS_SUBTYPE);
      }
   }

   pop_scope(nametab);

   mangle_func(nametab, t);

   tree_set_loc(t, CURRENT_LOC);
   return t;
}

static tree_t p_subprogram_instantiation_declaration(void)
{
   // subprogram_kind designator is new uninstantiated_subprogram_name
   //   [ signature ] [ generic_map_aspect ] ;

   tree_kind_t kind;
   switch (one_of(tFUNCTION, tPROCEDURE)) {
   case tFUNCTION: kind = T_FUNC_INST; break;
   case tPROCEDURE: kind = T_PROC_INST; break;
   default: return NULL;
   }

   tree_t inst = tree_new(kind);
   tree_set_ident(inst, p_designator());

   consume(tIS);
   consume(tNEW);

   require_std(STD_08, "subprogram instantiation declarations");

   tree_t name = p_name(N_SUBPROGRAM | N_TYPE);

   type_t constraint = NULL;
   if (peek() == tLSQUARE)
      constraint = p_signature();

   tree_t decl = NULL;
   if (tree_kind(name) != T_REF)
      parse_error(CURRENT_LOC, "expecting uninstantiated subprogram name");
   else {
      decl = resolve_subprogram_name(nametab, name, constraint);

      if (decl != NULL && !is_uninstantiated_subprogram(decl)) {
         parse_error(CURRENT_LOC, "%s %s is not an uninstantiated subprogram",
                     class_str(class_of(decl)), istr(tree_ident(name)));
         decl = NULL;
      }
   }

   tree_t body = NULL;
   if (decl != NULL) {
      const tree_kind_t decl_kind = tree_kind(decl);
      if (decl_kind == T_FUNC_BODY || decl_kind == T_PROC_BODY)
         body = decl;
      else {
         // Attempt to load the package body if available
         type_t type = tree_type(decl);
         tree_t pack = tree_container(decl), pack_body, d;
         if (tree_kind(pack) == T_PACKAGE && (pack_body = body_of(pack))) {
            ident_t id = tree_ident(decl);
            for (int nth = 0; (d = search_decls(pack_body, id, nth)); nth++) {
               if (is_subprogram(d) && type_eq(tree_type(d), type)) {
                  body = d;
                  break;
               }
            }
         }
      }

      if (body == NULL)
         parse_error(CURRENT_LOC, "subprogram %s cannot be instantiated until "
                     "its body has been analysed", istr(tree_ident(decl)));
      else
         tree_set_ref(inst, body);
   }

   if (decl != NULL && body != NULL)
      instantiate_subprogram(inst, decl, body);
   else {
      // Create a dummy subprogram type to avoid later errors
      type_t type = type_new(kind == T_FUNC_INST ? T_FUNC : T_PROC);
      if (kind == T_FUNC_INST)
         type_set_result(type, type_new(T_NONE));
      tree_set_type(inst, type);
   }

   if (peek() == tGENERIC)
      p_generic_map_aspect(inst, inst);

   consume(tSEMI);

   tree_set_loc(inst, CURRENT_LOC);
   sem_check(inst, nametab);

   hash_t *map = get_generic_map(nametab);
   if (map != NULL)
      tree_rewrite(inst, NULL, rewrite_generic_refs_cb,
                   rewrite_generic_types_cb, map);

   mangle_func(nametab, inst);
   insert_name(nametab, inst, NULL);
   return inst;
}

static void p_variable_declaration(tree_t parent)
{
   // [ shared ] variable identifier_list : subtype_indication
   //   [ := expression ] ;

   BEGIN("variable declaration");

   const bool shared = optional(tSHARED);

   consume(tVARIABLE);

   LOCAL_IDENT_LIST ids = p_identifier_list();

   for (ident_list_t *it = ids; it != NULL; it = it->next)
      hide_name(nametab, it->ident);

   consume(tCOLON);

   type_t type = p_subtype_indication();

   tree_t init = NULL;
   if (optional(tASSIGN)) {
      init = p_conditional_expression();
      solve_types(nametab, init, type);
   }

   consume(tSEMI);

   for (ident_list_t *it = ids; it != NULL; it = it->next) {
      tree_t t = tree_new(T_VAR_DECL);
      tree_set_loc(t, &(it->loc));
      tree_set_ident(t, it->ident);
      tree_set_type(t, type);

      if (init != NULL)
         tree_set_value(t, init);

      if (shared)
         tree_set_flag(t, TREE_F_SHARED);

      tree_add_decl(parent, t);
      insert_name(nametab, t, it->ident);
      sem_check(t, nametab);
   }
}

static tree_flags_t p_signal_kind(void)
{
   // register | bus

   switch (peek()) {
   case tBUS:
      consume(tBUS);
      return TREE_F_BUS;
   case tREGISTER:
      consume(tREGISTER);
      return TREE_F_REGISTER;
   default:
      return 0;
   }
}

static void p_signal_declaration(tree_t parent)
{
   // signal identifier_list : subtype_indication [ signal_kind ]
   //   [ := expression ] ;

   BEGIN("signal declaration");

   consume(tSIGNAL);

   LOCAL_IDENT_LIST ids = p_identifier_list();

   for (ident_list_t *it = ids; it != NULL; it = it->next)
      hide_name(nametab, it->ident);

   consume(tCOLON);

   type_t type = p_subtype_indication();
   tree_flags_t flags = p_signal_kind();

   tree_t init = NULL;
   if (optional(tASSIGN)) {
      init = p_conditional_expression();
      solve_known_subtype(nametab, init, type);
   }

   consume(tSEMI);

   for (ident_list_t *it = ids; it != NULL; it = it->next) {
      tree_t t = tree_new(T_SIGNAL_DECL);
      tree_set_loc(t, &(it->loc));
      tree_set_ident(t, it->ident);
      tree_set_type(t, type);
      tree_set_value(t, init);
      tree_set_flag(t, flags);

      tree_add_decl(parent, t);

      insert_name(nametab, t, it->ident);
      sem_check(t, nametab);
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
         type_t param = p_type_mark();
         type_add_param(type, param);
         error = error || type_is_none(param);
      } while (optional(tCOMMA));
   }

   if (optional(tRETURN)) {
      type_t ret = p_type_mark();
      type_set_result(type, ret);
      error = error || type_is_none(ret);
   }

   consume(tRSQUARE);

   return error ? type_new(T_NONE) : type;
}

static void p_alias_declaration(tree_t parent)
{
   // alias alias_designator [ : subtype_indication ] is name [ signature ] ;

   BEGIN("alias declaration");

   tree_t t = tree_new(T_ALIAS);

   consume(tALIAS);

   ident_t id = p_designator();
   tree_set_ident(t, id);

   hide_name(nametab, id);

   bool has_subtype_indication = false;
   if (optional(tCOLON)) {
      tree_set_type(t, p_subtype_indication());
      has_subtype_indication = true;
   }
   consume(tIS);

   tree_t value = p_name(N_SUBPROGRAM | N_TYPE);
   tree_set_value(t, value);

   if (peek() == tLPAREN) {
      parse_error(tree_loc(value), "name cannot be indexed or sliced");
      tree_set_type(t, type_new(T_NONE));
      drop_tokens_until(tRPAREN);
   }

   const tree_kind_t value_kind = tree_kind(value);

   if (peek() == tLSQUARE) {
      type_t type = p_signature();
      if (has_subtype_indication) {
         parse_error(CURRENT_LOC, "alias declaration may not contain both a "
                     "signature and a subtype indication");
         type = type_new(T_NONE);
      }
      else if (value_kind != T_REF && value_kind != T_PROT_REF) {
         if (!type_is_none((type = solve_types(nametab, value, NULL)))) {
            parse_error(tree_loc(value), "invalid name in subprogram alias");
            type = type_new(T_NONE);
         }
      }
      else {
         ident_t id = tree_ident(value);
         type_set_ident(type, ident_rfrom(id, '.') ?: id);
         solve_types(nametab, value, type);
      }
      tree_set_type(t, type);
   }
   else
      solve_types(nametab, value, NULL);

   const bool type_alias =
      value_kind == T_REF
      && tree_has_ref(value)
      && is_type_decl(tree_ref(value));

   if (type_alias && has_subtype_indication)
      parse_error(CURRENT_LOC, "non-object alias may not have "
                  "subtype indication");

   consume(tSEMI);

   tree_set_loc(t, CURRENT_LOC);
   insert_name(nametab, t, NULL);
   sem_check(t, nametab);

   tree_add_decl(parent, t);

   if (type_alias) {
      // LRM 08 section 6.6.3 an implicit alias declaration exists for
      // each enumeration literal, unit, and predefined operators

      type_t type = tree_type(value);

      switch (type_kind(type)) {
      case T_ENUM:
         {
            const int nlits = type_enum_literals(type);
            for (int i = 0; i < nlits; i++) {
               tree_t lit = type_enum_literal(type, i);

               tree_t a = tree_new(T_ALIAS);
               tree_set_loc(a, CURRENT_LOC);
               tree_set_ident(a, tree_ident(lit));
               tree_set_value(a, make_ref(lit));
               tree_set_type(a, type);
               tree_set_flag(a, TREE_F_PREDEFINED);

               insert_name(nametab, a, NULL);
               tree_add_decl(parent, a);
            }
         }
         break;

      case T_PHYSICAL:
         {
            const int nunits = type_units(type);
            for (int i = 0; i < nunits; i++) {
               tree_t unit = type_unit(type, i);

               tree_t a = tree_new(T_ALIAS);
               tree_set_loc(a, CURRENT_LOC);
               tree_set_ident(a, tree_ident(unit));
               tree_set_value(a, make_ref(unit));
               tree_set_type(a, type);
               tree_set_flag(a, TREE_F_PREDEFINED);

               insert_name(nametab, a, NULL);
               tree_add_decl(parent, a);
            }
         }
      default:
         break;
      }

      walk_predefs(nametab, tree_ident(value), add_predef_alias, parent);
   }
}

static void p_file_open_information(tree_t *mode, tree_t *name)
{
   // [ open expression ] is file_logical_name

   BEGIN("file open information");

   if (optional(tOPEN)) {
      *mode = p_expression();
      solve_types(nametab, *mode, std_type(NULL, STD_FILE_OPEN_KIND));
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
      solve_types(nametab, *name, std_type(NULL, STD_STRING));

      if (*mode == NULL) {
         tree_t decl = search_decls(find_std(nametab), mode_name, 0);
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

   for (ident_list_t *it = ids; it != NULL; it = it->next)
      hide_name(nametab, it->ident);

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
      tree_set_loc(t, &(it->loc));

      tree_add_decl(parent, t);

      insert_name(nametab, t, it->ident);
      sem_check(t, nametab);
   }
}

static void p_disconnection_specification(tree_t container)
{
   // disconnect guarded_signal_specification after time_expression ;

   BEGIN("disconnection specification");

   consume(tDISCONNECT);

   LOCAL_IDENT_LIST ids = p_identifier_list();

   consume(tCOLON);

   type_t type = p_type_mark();

   consume(tAFTER);

   tree_t delay = p_expression();
   solve_types(nametab, delay, std_type(NULL, STD_TIME));

   consume(tSEMI);

   for (ident_list_t *it = ids; it != NULL; it = it->next) {
      tree_t d = tree_new(T_DISCONNECT);
      tree_set_loc(d, &(it->loc));
      tree_set_ident(d, it->ident);
      tree_set_type(d, type);
      tree_set_delay(d, delay);
      tree_set_ref(d, resolve_name(nametab, CURRENT_LOC, it->ident));

      tree_add_decl(container, d);
      sem_check(d, nametab);
   }
}

static void p_entity_class_entry_list(tree_t group)
{
   // entity_class [ <> ] { , entity_class [ <> ] }

   BEGIN("entity class entry list");

   do {
      p_entity_class();
      optional(tBOX);
   } while (optional(tCOMMA));
}

static tree_t p_group_template_declaration(void)
{
   // group identifier is ( entity_class_entry_list ) ;

   BEGIN("group template declaration");

   consume(tGROUP);

   tree_t g = tree_new(T_GROUP_TEMPLATE);
   tree_set_ident(g, p_identifier());

   consume(tIS);
   consume(tLPAREN);
   p_entity_class_entry_list(g);
   consume(tRPAREN);
   consume(tSEMI);

   tree_set_loc(g, CURRENT_LOC);
   insert_name(nametab, g, NULL);

   return g;
}

static void p_group_constituent_list(tree_t group)
{
   // group_constituent_list ::= group_constituent { , group_constituent }

   BEGIN("group constituent list");

   do {
      (void)p_name(0);   // Do nothing with groups currently
   } while (optional(tCOMMA));
}

static tree_t p_group_declaration(void)
{
   // group identifier : group_template_name ( group_constituent_list ) ;

   BEGIN("group declaration");

   consume(tGROUP);

   tree_t g = tree_new(T_GROUP);
   tree_set_ident(g, p_identifier());

   consume(tCOLON);

   ident_t template_name = p_identifier();
   tree_t template = resolve_name(nametab, CURRENT_LOC, template_name);
   if (template != NULL && tree_kind(template) != T_GROUP_TEMPLATE) {
      parse_error(CURRENT_LOC, "%s does not name a group template",
                  istr(template_name));
      template = NULL;
   }

   tree_set_ref(g, template);

   consume(tLPAREN);
   p_group_constituent_list(g);
   consume(tRPAREN);
   consume(tSEMI);

   tree_set_loc(g, CURRENT_LOC);
   insert_name(nametab, g, NULL);

   return g;
}

static void p_protected_type_body_declarative_item(tree_t body)
{
   // subprogram_declaration | subprogram_body | type_declaration
   //   | subtype_declaration | constant_declaration | variable_declaration
   //   | file_declaration | alias_declaration | attribute_declaration
   //   | attribute_specification | use_clause | group_template_declaration
   //   | group_declaration | 2008: subprogram_instantiation_declaration

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
      p_alias_declaration(body);
      break;

   case tFUNCTION:
   case tPROCEDURE:
   case tIMPURE:
   case tPURE:
      if (peek_nth(3) == tIS && peek_nth(4) == tNEW)
         tree_add_decl(body, p_subprogram_instantiation_declaration());
      else {
         tree_t spec = p_subprogram_specification();
         tree_set_flag(spec, TREE_F_PROTECTED);
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

   case tGROUP:
      if (peek_nth(3) == tIS)
         tree_add_decl(body, p_group_template_declaration());
      else
         tree_add_decl(body, p_group_declaration());
      break;

   default:
      expect(tATTRIBUTE, tTYPE, tSUBTYPE, tCONSTANT, tFUNCTION, tPROCEDURE,
             tIMPURE, tPURE, tALIAS, tVARIABLE, tUSE, tFILE, tGROUP);
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

   tree_t decl = resolve_name(nametab, CURRENT_LOC, id);

   if (decl != NULL && tree_kind(decl) != T_PROT_DECL) {
      parse_error(CURRENT_LOC, "object %s is not a protected type "
                  "declaration", istr(id));
      decl = NULL;
   }

   tree_t body = tree_new(T_PROT_BODY);
   tree_set_ident(body, id);
   tree_set_loc(body, CURRENT_LOC);
   tree_set_primary(body, decl);

   insert_name(nametab, body, NULL);

   push_scope(nametab);

   if (decl != NULL) {
      type_t type = tree_type(decl);
      assert(type_is_protected(type));
      tree_set_type(body, type);
      insert_decls(nametab, decl);
   }
   else
      tree_set_type(body, type_new(T_NONE));

   scope_set_prefix(nametab, id);
   scope_set_container(nametab, body);

   p_protected_type_body_declarative_part(body);

   pop_scope(nametab);

   consume(tEND);
   consume(tPROTECTED);
   consume(tBODY);

   p_trailing_label(id);

   tree_set_loc(body, CURRENT_LOC);
   sem_check(body, nametab);
   return body;
}

static tree_t p_package_instantiation_declaration(tree_t unit)
{
   // 2008: package identifier is new name [ generic_map_aspect ] ;

   consume(tPACKAGE);

   ident_t id = p_identifier();

   consume(tIS);
   consume(tNEW);

   require_std(STD_08, "package instantiation declarations");

   ident_t unit_name = p_selected_identifier();
   tree_t pack = resolve_name(nametab, CURRENT_LOC, unit_name);

   if (pack != NULL && !is_uninstantiated_package(pack)) {
      parse_error(CURRENT_LOC, "unit %s is not an uninstantiated package",
                  istr(unit_name));
      pack = NULL;
   }

   tree_t new;
   if (unit != NULL) {
      // Package instantiation declaration as primary unit
      assert(tree_kind(unit) == T_DESIGN_UNIT);
      tree_change_kind(unit, T_PACK_INST);
      new = unit;

      ident_t qual = ident_prefix(lib_name(lib_work()), id, '.');
      tree_set_ident(new, qual);
   }
   else {
      new = tree_new(T_PACK_INST);
      tree_set_ident(new, id);
   }

   tree_t body = NULL;
   if (pack != NULL) {
      if (package_needs_body(pack) && (body = body_of(pack)) == NULL)
         parse_error(CURRENT_LOC, "package %s cannot be instantiated until "
                     "its body has been analysed", istr(unit_name));

      instantiate_package(new, pack, body);
   }

   tree_set_ref(new, pack);

   if (peek() == tGENERIC)
      p_generic_map_aspect(new, new);

   consume(tSEMI);

   tree_set_loc(new, CURRENT_LOC);
   insert_name(nametab, new, NULL);

   sem_check(new, nametab);

   hash_t *map = get_generic_map(nametab);
   if (map != NULL)
      tree_rewrite(new, NULL, rewrite_generic_refs_cb,
                   rewrite_generic_types_cb, map);

   return new;
}

static tree_t p_element_array_mode_view_indication(void)
{
   // view ( name )

   BEGIN("element array mode view indication");

   consume(tVIEW);
   consume(tLPAREN);

   tree_t name = p_name(0);
   solve_types(nametab, name, NULL);

   consume(tRPAREN);

   return name;
}

static tree_t p_element_record_mode_view_indication(void)
{
   // view name

   BEGIN("element record mode view indication");

   consume(tVIEW);

   tree_t name = p_name(0);
   solve_types(nametab, name, NULL);

   return name;
}

static port_mode_t p_element_mode_view_indication(tree_t *name)
{
   // element_record_mode_view_indication | element_array_mode_view_indication

   BEGIN("element mode view indication");

   if (peek_nth(2) == tLPAREN) {
      *name = p_element_array_mode_view_indication();
      return PORT_ARRAY_VIEW;
   }
   else {
      *name = p_element_record_mode_view_indication();
      return PORT_RECORD_VIEW;
   }
}

static void p_mode_view_element_declaration(type_t view, type_t of)
{
   // record_element_list : element_mode_indication ;

   BEGIN("mode view element declaration");

   LOCAL_IDENT_LIST ids = p_identifier_list();

   consume(tCOLON);

   tree_t name = NULL;

   port_mode_t mode;
   if (peek() == tVIEW) {
      // View name must be looked up in global scope
      pop_scope(nametab);
      mode = p_element_mode_view_indication(&name);
      push_scope_for_fields(nametab, of);
   }
   else
      mode = p_mode();

   for (ident_list_t *it = ids; it != NULL; it = it->next) {
      tree_t f = tree_new(T_VIEW_ELEMENT);
      tree_set_ident(f, it->ident);
      tree_set_loc(f, &(it->loc));
      tree_set_subkind(f, mode);
      tree_set_value(f, name);

      type_add_field(view, f);

      solve_types(nametab, f, NULL);
   }

   consume(tSEMI);
}

static tree_t p_mode_view_declaration(void)
{
   // view identifier of subtype_indication is
   //   { mode_view_element_definition } end view [ mode_view_simple_name ] ;

   BEGIN("mode view declaration");

   consume(tVIEW);

   tree_t view = tree_new(T_VIEW_DECL);

   ident_t id = p_identifier();
   tree_set_ident(view, id);

   consume(tOF);

   type_t of = p_subtype_indication();

   type_t type = type_new(T_VIEW);
   type_set_ident(type, id);
   type_set_designated(type, of);

   tree_set_type(view, type);

   consume(tIS);

   if (type_is_record(of)) {
      push_scope_for_fields(nametab, of);

      while (not_at_token(tEND))
         p_mode_view_element_declaration(type, of);

      pop_scope(nametab);

      consume(tEND);
   }
   else {
      parse_error(CURRENT_LOC, "subtype indication of a mode view declaration "
                  "must denote a record type");
      drop_tokens_until(tEND);
   }

   consume(tVIEW);

   p_trailing_label(id);

   consume(tSEMI);

   tree_set_loc(view, CURRENT_LOC);
   insert_name(nametab, view, NULL);
   sem_check(view, nametab);
   return view;
}

static void p_entity_declarative_item(tree_t entity)
{
   // subprogram_declaration | subprogram_body | type_declaration
   //   | subtype_declaration | constant_declaration | signal_declaration
   //   | shared_variable_declaration | file_declaration | alias_declaration
   //   | attribute_declaration | attribute_specification
   //   | disconnection_specification | use_clause | group_template_declaration
   //   | group_declaration | 2008: subprogram_instantiation_declaration
   //   | 2019: mode_view_declaration

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
      p_alias_declaration(entity);
      break;

   case tFUNCTION:
   case tPROCEDURE:
   case tIMPURE:
   case tPURE:
      if (peek_nth(3) == tIS && peek_nth(4) == tNEW)
         tree_add_decl(entity, p_subprogram_instantiation_declaration());
      else {
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

   case tDISCONNECT:
      p_disconnection_specification(entity);
      break;

   case tGROUP:
      if (peek_nth(3) == tIS)
         tree_add_decl(entity, p_group_template_declaration());
      else
         tree_add_decl(entity, p_group_declaration());
      break;

   case tSHARED:
      p_variable_declaration(entity);
      break;

   case tSIGNAL:
      p_signal_declaration(entity);
      break;

   case tVIEW:
      tree_add_decl(entity, p_mode_view_declaration());
      break;

   default:
      expect(tATTRIBUTE, tTYPE, tSUBTYPE, tCONSTANT, tFUNCTION, tPROCEDURE,
             tIMPURE, tPURE, tALIAS, tUSE, tDISCONNECT, tGROUP, tSHARED,
             tSIGNAL, STD(19, tVIEW));
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
   //   | group_declaration | 2008: subprogram_instantiation_declaration

   BEGIN("subprogram declarative item");

   switch (peek()) {
   case tVARIABLE:
      p_variable_declaration(sub);
      break;

   case tTYPE:
      p_type_declaration(sub);
      break;

   case tALIAS:
      p_alias_declaration(sub);
      break;

   case tCONSTANT:
      p_constant_declaration(sub);
      break;

   case tFUNCTION:
   case tPROCEDURE:
   case tIMPURE:
   case tPURE:
      if (peek_nth(3) == tIS && peek_nth(4) == tNEW)
         tree_add_decl(sub, p_subprogram_instantiation_declaration());
      else {
         tree_t spec = p_subprogram_specification();
         tree_set_flag(spec, tree_flags(sub) & TREE_F_PROTECTED);
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

   case tGROUP:
      if (peek_nth(3) == tIS)
         tree_add_decl(sub, p_group_template_declaration());
      else
         tree_add_decl(sub, p_group_declaration());
      break;

   default:
      expect(tVARIABLE, tTYPE, tALIAS, tCONSTANT, tFUNCTION, tPROCEDURE,
             tIMPURE, tPURE, tATTRIBUTE, tSUBTYPE, tUSE, tFILE, tGROUP);
   }
}

static void p_subprogram_declarative_part(tree_t sub)
{
   // { subprogram_declarative_item }

   BEGIN("subprogram declarative part");

   while (not_at_token(tBEGIN))
      p_subprogram_declarative_item(sub);
}

static void p_sequence_of_statements(tree_t parent)
{
   // { sequential_statement }

   BEGIN("sequence of statements");

   while (not_at_token(tEND, tELSE, tELSIF, tWHEN))
      tree_add_stmt(parent, p_sequential_statement());
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

   insert_name(nametab, spec, NULL);

   push_scope(nametab);
   scope_set_subprogram(nametab, spec);

   insert_generics(nametab, spec);
   insert_ports(nametab, spec);

   sem_check(spec, nametab);

   if (tree_flags(spec) & TREE_F_KNOWS_SUBTYPE) {
      // LRM 19 section 4.2.1: an implicit subtype declaration is
      // created as the first declarative item when the function
      // includes a return identifier

      type_t sub = type_result(tree_type(spec));
      assert(type_kind(sub) == T_SUBTYPE);

      tree_t d = tree_new(T_SUBTYPE_DECL);
      tree_set_ident(d, type_ident(sub));
      tree_set_type(d, sub);
      tree_set_loc(d, CURRENT_LOC);

      insert_name(nametab, d, NULL);

      tree_add_decl(spec, d);
   }

   p_subprogram_declarative_part(spec);

   consume(tBEGIN);

   p_sequence_of_statements(spec);

   consume(tEND);

   pop_scope(nametab);

   if (scan(tFUNCTION, tPROCEDURE))
      consume(kind == T_FUNC_BODY ? tFUNCTION : tPROCEDURE);

   p_trailing_label(tree_ident(spec));
   consume(tSEMI);

   tree_set_loc(spec, CURRENT_LOC);
   return spec;
}

static tree_t p_subprogram_declaration(tree_t spec)
{
   // subprogram_specification ;

   EXTEND("subprogram declaration");

   insert_name(nametab, spec, NULL);

   consume(tSEMI);

   tree_set_loc(spec, CURRENT_LOC);
   sem_check(spec, nametab);
   return spec;
}

static void p_sensitivity_list(tree_t proc)
{
   // name { , name }

   BEGIN("sensitivity list");

   do {
      tree_t name = p_name(0);
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
   //   | group_declaration | 2008: subprogram_instantiation_declaration

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
      if (peek_nth(3) == tIS && peek_nth(4) == tNEW)
         tree_add_decl(proc, p_subprogram_instantiation_declaration());
      else {
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
      p_alias_declaration(proc);
      break;

   case tFILE:
      p_file_declaration(proc);
      break;

   case tGROUP:
      if (peek_nth(3) == tIS)
         tree_add_decl(proc, p_group_template_declaration());
      else
         tree_add_decl(proc, p_group_declaration());
      break;

   default:
      expect(tVARIABLE, tTYPE, tSUBTYPE, tCONSTANT, tFUNCTION, tPROCEDURE,
             tIMPURE, tPURE, tATTRIBUTE, tUSE, tALIAS, tFILE, tGROUP);
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

   p_sequence_of_statements(proc);
}

static void p_process_sensitivity_list(tree_t proc)
{
   // 2008: all | sensitivity_list

   BEGIN("process sensitivity list");

   if (peek() == tALL) {
      consume(tALL);

      tree_t all = tree_new(T_ALL);
      tree_set_loc(all, CURRENT_LOC);
      tree_add_trigger(proc, all);
   }
   else
      p_sensitivity_list(proc);
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
      if (standard() < STD_08)
         p_sensitivity_list(t);
      else
         p_process_sensitivity_list(t);
      consume(tRPAREN);
   }

   optional(tIS);

   if (label == NULL) {
      tree_set_ident(t, get_implicit_label(t, nametab));
      tree_set_flag(t, TREE_F_SYNTHETIC_NAME);
   }
   else {
      tree_set_loc(t, CURRENT_LOC);
      tree_set_ident(t, label);
      insert_name(nametab, t, label);
   }

   push_scope(nametab);
   scope_set_container(nametab, t);
   scope_set_prefix(nametab, tree_ident(t));

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

   sem_check(t, nametab);
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

   case tID:
      return p_concurrent_procedure_call_statement(label, NULL);

   case tPOSTPONED:
      if (peek_nth(2) == tASSERT)
         return p_concurrent_assertion_statement(label);
      else if (peek_nth(2) == tPROCESS)
         return p_process_statement(label);
      else
         return p_concurrent_procedure_call_statement(label, NULL);

   default:
      expect(tASSERT, tPROCESS, tPOSTPONED);
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

   push_scope(nametab);

   tree_set_loc(unit, CURRENT_LOC);
   insert_name(nametab, unit, id);

   push_scope(nametab);

   ident_t qual = ident_prefix(lib_name(lib_work()), id, '.');
   scope_set_prefix(nametab, qual);

   p_entity_header(unit);
   p_entity_declarative_part(unit);

   if (optional(tBEGIN))
      p_entity_statement_part(unit);

   consume(tEND);
   optional(tENTITY);
   p_trailing_label(id);
   consume(tSEMI);

   tree_set_loc(unit, CURRENT_LOC);
   sem_check(unit, nametab);

   pop_scope(nametab);
   pop_scope(nametab);

   tree_set_ident(unit, qual);
}

static tree_t p_component_declaration(void)
{
   // component identifier [ is ] [ generic_clause ] [ port_clause ]
   //   end component [ simple_name ] ;

   // 2019:
   // component identifier [ is ] [ generic_clause ] [ port_clause ]
   //   end [ component ] [ simple_name ] ;

   BEGIN("component declaration");

   tree_t c = tree_new(T_COMPONENT);

   consume(tCOMPONENT);
   tree_set_ident(c, p_identifier());
   optional(tIS);

   push_scope(nametab);

   if (peek() == tGENERIC) {
      p_generic_clause(c);
      insert_generics(nametab, c);
   }

   if (peek() == tPORT) {
      p_port_clause(c);
      insert_ports(nametab, c);
   }

   pop_scope(nametab);

   consume(tEND);
   if (peek() != tCOMPONENT)
      require_std(STD_19, "optional end component");
   else
      consume(tCOMPONENT);
   p_trailing_label(tree_ident(c));
   consume(tSEMI);

   tree_set_loc(c, CURRENT_LOC);
   insert_name(nametab, c, NULL);
   sem_check(c, nametab);
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
   //   | 2008: package_instantiation_declaration
   //   | 2008: package_declaration | 2019: mode_view_declaration
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
      if (peek_nth(3) == tIS && peek_nth(4) == tNEW)
         tree_add_decl(pack, p_subprogram_instantiation_declaration());
      else {
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
      p_alias_declaration(pack);
      break;

   case tUSE:
      p_use_clause(pack, tree_add_decl);
      break;

   case tDISCONNECT:
      p_disconnection_specification(pack);
      break;

   case tGROUP:
      if (peek_nth(3) == tIS)
         tree_add_decl(pack, p_group_template_declaration());
      else
         tree_add_decl(pack, p_group_declaration());
      break;

   case tPACKAGE:
      if (peek_nth(4) == tNEW)
         tree_add_decl(pack, p_package_instantiation_declaration(NULL));
      else {
         require_std(STD_08, "nested package declarations");
         tree_add_decl(pack, p_package_declaration(NULL));
      }
      break;

   case tVIEW:
      tree_add_decl(pack, p_mode_view_declaration());
      break;

   default:
      expect(tTYPE, tFUNCTION, tPROCEDURE, tIMPURE, tPURE, tSUBTYPE, tSIGNAL,
             tATTRIBUTE, tCONSTANT, tCOMPONENT, tFILE, tSHARED, tALIAS, tUSE,
             tDISCONNECT, tGROUP, tPACKAGE, STD(19, tVIEW));
   }
}

static void p_package_declarative_part(tree_t pack)
{
   // { package_declarative_item }

   BEGIN("package declarative part");

   while (not_at_token(tEND))
      p_package_declarative_item(pack);
}

static void p_package_header(tree_t unit)
{
   // 2008: [ generic_clause [ generic_map_aspect ; ] ]

   BEGIN("package header");

   if (peek() == tGENERIC) {
      p_generic_clause(unit);

      if (peek() == tGENERIC) {
         p_generic_map_aspect(unit, unit);
         consume(tSEMI);
      }

      insert_generics(nametab, unit);
   }
}

static tree_t p_package_declaration(tree_t unit)
{
   // package identifier is package_declarative_part end [ package ]
   //   [ simple_name ] ;
   //
   // 2008: package identifier is package_header package_declarative_part
   //   end [ package ] [ simple_name ] ;

   BEGIN("package declaration");

   consume(tPACKAGE);

   ident_t name = p_identifier(), qual = name;

   tree_t pack;
   if (unit != NULL) {
      // Package declaration as primary unit
      assert(tree_kind(unit) == T_DESIGN_UNIT);
      tree_change_kind(unit, T_PACKAGE);
      pack = unit;


      qual = ident_prefix(lib_name(lib_work()), name, '.');
      scope_set_prefix(nametab, qual);
   }
   else {
      pack = tree_new(T_PACKAGE);
      scope_set_prefix(nametab, name);
   }

   tree_set_ident(pack, name);
   tree_set_loc(pack, CURRENT_LOC);

   push_scope(nametab);
   insert_name(nametab, pack, NULL);

   consume(tIS);

   push_scope(nametab);
   if (standard() >= STD_08)
      p_package_header(pack);
   p_package_declarative_part(pack);

   if (bootstrapping)
      declare_additional_standard_operators(pack);

   pop_scope(nametab);

   consume(tEND);
   optional(tPACKAGE);
   p_trailing_label(name);
   consume(tSEMI);

   tree_set_loc(pack, CURRENT_LOC);
   sem_check(pack, nametab);

   tree_set_ident(pack, qual);

   pop_scope(nametab);
   return pack;
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
         ident_list_push(&result, well_known(W_ALL), last_loc);
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

static tree_t p_binding_indication(tree_t comp)
{
   // [ use entity_aspect ] [ generic_map_aspect ] [ port_map_aspect ]

   BEGIN("binding indication");

   tree_t bind = NULL, unit = NULL;
   if (optional(tUSE)) {
      if ((bind = p_entity_aspect())) {
         unit = find_binding(bind);
         tree_set_ref(bind, unit);

         if (unit != NULL) unit = primary_unit_of(unit);
      }
   }
   else
      bind = tree_new(T_BINDING);

   if (comp) {
      insert_generics(nametab, comp);
      insert_ports(nametab, comp);
   }

   if (peek() == tGENERIC) {
      assert(bind != NULL);   // XXX: check for open here
      p_generic_map_aspect(bind, unit);
   }

   if (peek() == tPORT) {
      assert(bind != NULL);   // XXX: check for open here
      p_port_map_aspect(bind, unit);
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

   tree_t comp = resolve_name(nametab, CURRENT_LOC, comp_name);
   if (comp != NULL && tree_kind(comp) != T_COMPONENT) {
      parse_error(CURRENT_LOC, "%s does not name a component", istr(comp_name));
      comp = NULL;
   }

   push_scope(nametab);

   tree_t bind = p_binding_indication(comp);
   consume(tSEMI);

   if (ids != NULL) {
      for (ident_list_t *it = ids; it != NULL; it = it->next) {
         tree_t t = tree_new(T_SPEC);
         tree_set_loc(t, &(it->loc));
         tree_set_ident(t, it->ident);
         tree_set_ident2(t, comp_name);
         tree_set_value(t, bind);
         tree_set_ref(t, comp);

         const spec_kind_t kind =
            it->ident == well_known(W_ALL) ? SPEC_ALL : SPEC_EXACT;

         tree_add_decl(parent, t);
         insert_spec(nametab, t, kind, it->ident, 1);
         sem_check(t, nametab);
      }
   }
   else {
      // Instantiation list was "others"
      tree_t t = tree_new(T_SPEC);
      tree_set_loc(t, CURRENT_LOC);
      tree_set_ident2(t, comp_name);
      tree_set_value(t, bind);
      tree_set_ref(t, comp);

      tree_add_decl(parent, t);
      insert_spec(nametab, t, SPEC_OTHERS, NULL, 1);
      sem_check(t, nametab);
   }

   pop_scope(nametab);
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

   case tGROUP:
      if (peek_nth(3) == tIS)
         tree_add_decl(unit, p_group_template_declaration());
      else
         tree_add_decl(unit, p_group_declaration());
      break;

   default:
      expect(tUSE, tATTRIBUTE, tGROUP);
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

   tree_t comp = resolve_name(nametab, CURRENT_LOC, comp_name);
   if (comp != NULL && tree_kind(comp) != T_COMPONENT) {
      parse_error(CURRENT_LOC, "%s does not name a component", istr(comp_name));
      comp = NULL;
   }

   push_scope(nametab);

   // TODO: should be optional
   tree_t bind = p_binding_indication(comp);
   consume(tSEMI);

   tree_t bcfg = NULL;
   if (peek() == tFOR) {
      tree_t of = tree_has_ref(bind) ? tree_ref(bind) : NULL;
      bcfg = p_block_configuration(of);
   }

   if (ids != NULL) {
      for (ident_list_t *it = ids; it != NULL; it = it->next) {
         tree_t t = tree_new(T_SPEC);
         tree_set_loc(t, &(it->loc));
         tree_set_ident(t, it->ident);
         tree_set_ident2(t, comp_name);
         tree_set_value(t, bind);
         tree_set_ref(t, comp);
         if (bcfg != NULL) tree_add_decl(t, bcfg);

         const spec_kind_t kind =
            it->ident == well_known(W_ALL) ? SPEC_ALL : SPEC_EXACT;

         tree_add_decl(unit, t);
         sem_check(t, nametab);
         insert_spec(nametab, t, kind, it->ident, 1);
      }
   }
   else {
      // Instantiation list was "others"
      tree_t t = tree_new(T_SPEC);
      tree_set_loc(t, CURRENT_LOC);
      tree_set_ident2(t, comp_name);
      tree_set_value(t, bind);
      tree_set_ref(t, comp);
      if (bcfg != NULL) tree_add_decl(t, bcfg);

      tree_add_decl(unit, t);
      sem_check(t, nametab);
      insert_spec(nametab, t, SPEC_OTHERS, NULL, 1);
   }

   pop_scope(nametab);

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
      tree_add_decl(unit, p_block_configuration(NULL));
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
      p_discrete_range(NULL);
   }
   else {
      p_expression();
   }
}

static void p_block_specification(tree_t b)
{
   // label | label [ ( index_specification ) ]

   BEGIN("block specification");

   ident_t id = p_identifier();
   tree_set_ident(b, id);

   if (optional(tLPAREN)) {
      p_index_specification();    // XXX: not used
      consume(tRPAREN);
   }
}

static tree_t p_block_configuration(tree_t of)
{
   // for block_specification { use_clause } { configuration_item } end for ;

   BEGIN("block configuration");

   consume(tFOR);

   tree_t b = tree_new(T_BLOCK_CONFIG);
   p_block_specification(b);

   push_scope(nametab);

   tree_t sub = NULL;
   if (of != NULL) {
      switch (tree_kind(of)) {
      case T_ENTITY:
         {
            ident_t qual = ident_prefix(tree_ident(of), tree_ident(b), '-');
            if ((sub = lib_get_qualified(qual)) == NULL)
               parse_error(CURRENT_LOC, "cannot find architecture %s of "
                           "entity %s", istr(tree_ident(b)),
                           istr(tree_ident(of)));
         }
         break;
      case T_ARCH:
         {
            ident_t expect = ident_rfrom(tree_ident(of), '-');
            if (tree_ident(b) != expect)
               parse_error(CURRENT_LOC, "block specification label %s does not "
                           "match architecture name %s", istr(tree_ident(b)),
                           istr(expect));
            else
               sub = of;
         }
         break;
      default:
         fatal_trace("unexpected unit type %s in block configuration",
                     tree_kind_str(tree_kind(of)));
         break;
      }
   }
   else
      sub = resolve_name(nametab, CURRENT_LOC, tree_ident(b));

   if (sub != NULL && !is_implicit_block(sub)) {
      parse_error(CURRENT_LOC, "%s is not a block that can be configured",
                  istr(tree_ident(b)));
      sub = NULL;
   }

   if (sub != NULL) {
      tree_set_ref(b, sub);

      if (tree_kind(sub) == T_IF_GENERATE)
         sub = tree_cond(sub, 0);

      insert_names_for_config(nametab, sub);
   }
   else
      suppress_errors(nametab);

   while (not_at_token(tEND))
      p_configuration_item(b);

   if (sub != NULL) {
      const int nstmts = tree_stmts(sub);
      for (int i = 0; i < nstmts; i++) {
         tree_t s = tree_stmt(sub, i);
         if (tree_kind(s) == T_INSTANCE)
            query_spec(nametab, s);
      }

   }

   pop_scope(nametab);

   consume(tEND);
   consume(tFOR);
   consume(tSEMI);

   tree_set_loc(b, CURRENT_LOC);
   sem_check(b, nametab);
   return b;
}

static ident_t p_entity_name(tree_t *entity)
{
   // name

   BEGIN("entity name");

   ident_t ename = p_selected_identifier();

   ident_t qual = ename;
   if (ident_runtil(ename, '.') == ename)
      qual = ident_prefix(lib_name(lib_work()), ename, '.');

   *entity = resolve_name(nametab, CURRENT_LOC, qual);

   if (*entity != NULL && tree_kind(*entity) != T_ENTITY) {
      diag_t *d = diag_new(DIAG_ERROR, CURRENT_LOC);
      diag_printf(d, "%s%s is not an entity",
                  is_design_unit(*entity) ? "design unit " : "", istr(ename));
      diag_hint(d, tree_loc(*entity), "%s is a %s", istr(ename),
                class_str(class_of(*entity)));
      diag_emit(d);
      *entity = NULL;
      return ename;
   }

   return ename;
}

static void p_configuration_declaration(tree_t unit)
{
   // configuration identifier of name is configuration_declarative_part
   //   block_configuration end [ configuration ] [ simple_name ] ;

   BEGIN("configuration declaration");

   consume(tCONFIGURATION);

   tree_change_kind(unit, T_CONFIGURATION);

   ident_t id = p_identifier();
   tree_set_ident(unit, id);

   push_scope(nametab);

   consume(tOF);

   tree_t of = NULL;
   ident_t ename = p_entity_name(&of);
   tree_set_ident2(unit, ename);

   if (of != NULL) {
      tree_set_primary(unit, of);
      insert_name(nametab, of, ename);
      insert_decls(nametab, of);
   }

   consume(tIS);

   while (not_at_token(tFOR))
      p_configuration_declarative_part(unit);

   tree_add_decl(unit, p_block_configuration(of));

   consume(tEND);
   optional(tCONFIGURATION);
   p_trailing_label(id);
   consume(tSEMI);

   pop_scope(nametab);

   tree_set_loc(unit, CURRENT_LOC);
   sem_check(unit, nametab);

   tree_set_ident(unit, ident_prefix(lib_name(lib_work()), id, '.'));
}

static void p_context_declaration(tree_t unit)
{
   // 2008: context identifier is context_clause end [ context ]
   //       [ context_simple_name ] ;

   BEGIN("context declaration");

   consume(tCONTEXT);

   tree_change_kind(unit, T_CONTEXT);

   ident_t id = p_identifier();
   tree_set_ident(unit, ident_prefix(lib_name(lib_work()), id, '.'));

   consume(tIS);

   push_scope(nametab);

   // LRM 08 section 13.1 forbids preceeding context clause
   if (tree_contexts(unit) != 3)     // Implicit WORK and STD
      parse_error(tree_loc(tree_context(unit, 3)), "context clause preceeding "
                  "context declaration must be empty");

   p_context_clause(unit);

   consume(tEND);
   optional(tCONTEXT);
   p_trailing_label(id);
   consume(tSEMI);

   tree_set_loc(unit, CURRENT_LOC);
   sem_check(unit, nametab);

   pop_scope(nametab);
}

static void p_primary_unit(tree_t unit)
{
   // entity_declaration | configuration_declaration | package_declaration
   //   | 2008: package_instantiation_declaration

   BEGIN("primary unit");

   switch (peek()) {
   case tENTITY:
      p_entity_declaration(unit);
      break;

   case tPACKAGE:
      if (standard() >= STD_08 && peek_nth(4) == tNEW)
         p_package_instantiation_declaration(unit);
      else
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
   // subprogram_declaration | subprogram_body
   //   | 2008: package_instantiation_declaration | type_declaration
   //   | subtype_declaration | constant_declaration | signal_declaration
   //   | shared_variable_declaration | file_declaration | alias_declaration
   //   | component_declaration | attribute_declaration
   //   | attribute_specification | configuration_specification
   //   | disconnection_specification | use_clause | group_template_declaration
   //   | group_declaration | 2008: subprogram_instantiation_declaration
   //   | 2008: psl_clock_declaration | 2008: package_declaration
   //   | 2019: mode_view_declaration

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
      if (peek_nth(3) == tIS && peek_nth(4) == tNEW)
         tree_add_decl(parent, p_subprogram_instantiation_declaration());
      else {
         tree_t spec = p_subprogram_specification();
         if (peek() == tSEMI)
            tree_add_decl(parent, p_subprogram_declaration(spec));
         else
            tree_add_decl(parent, p_subprogram_body(spec));
      }
      break;

   case tALIAS:
      p_alias_declaration(parent);
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

   case tDISCONNECT:
      p_disconnection_specification(parent);
      break;

   case tGROUP:
      if (peek_nth(3) == tIS)
         tree_add_decl(parent, p_group_template_declaration());
      else
         tree_add_decl(parent, p_group_declaration());
      break;

   case tPACKAGE:
      if (peek_nth(4) == tNEW)
         tree_add_decl(parent, p_package_instantiation_declaration(NULL));
      else if (peek_nth(2) == tBODY) {
         require_std(STD_08, "nested package declarations");
         tree_add_decl(parent, p_package_body(NULL));
      }
      else {
         require_std(STD_08, "nested package declarations");
         tree_add_decl(parent, p_package_declaration(NULL));
      }
      break;

   case tSTARTPSL:
      consume(tSTARTPSL);
      tree_add_decl(parent, p_psl_declaration());
      break;

   case tDEFAULT:
   case tSEQUENCE:
   case tPROPERTY:
      tree_add_decl(parent, p_psl_declaration());
      break;

   case tVIEW:
      tree_add_decl(parent, p_mode_view_declaration());
      break;

   default:
      expect(tSIGNAL, tTYPE, tSUBTYPE, tFILE, tCONSTANT, tFUNCTION, tIMPURE,
             tPURE, tPROCEDURE, tALIAS, tATTRIBUTE, tFOR, tCOMPONENT, tUSE,
             tSHARED, tDISCONNECT, tGROUP, STD(08, tPACKAGE), STD(19, tVIEW));
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
         return p_name(0);
   }
   else
      return name;
}

static tree_t p_simple_variable_assignment(ident_t label, tree_t name)
{
   // [ label : ] target := expression ;
   // 2019: [ label : ] target := conditional_or_unaffected_expression ;

   EXTEND("simple variable assignment");

   tree_t target = p_target(name);

   consume(tASSIGN);

   tree_t value = p_conditional_or_unaffected_expression(STD_08);

   type_t target_type = solve_target(nametab, target, value);
   solve_known_subtype(nametab, value, target_type);

   tree_t t = tree_new(T_VAR_ASSIGN);
   tree_set_target(t, target);
   tree_set_value(t, value);

   consume(tSEMI);

   tree_set_loc(t, CURRENT_LOC);
   ensure_labelled(t, label);

   sem_check(t, nametab);
   return t;
}

static void p_selected_expressions(tree_t stmt, tree_t target)
{
   // { expression when choices , } expression when choices

   BEGIN("selected waveforms");

   type_t with_type = tree_type(tree_value(stmt));

   do {
      tree_t expr = p_expression();

      type_t constraint;
      if (tree_has_type(target))
         constraint = tree_type(target);
      else
         constraint = solve_target(nametab, target, expr);

      solve_known_subtype(nametab, expr, constraint);

      tree_t a = tree_new(T_VAR_ASSIGN);
      tree_set_target(a, target);
      tree_set_value(a, expr);

      sem_check(a, nametab);

      consume(tWHEN);

      tree_t alt = tree_new(T_ALTERNATIVE);
      tree_add_stmt(alt, a);

      p_choices(alt, with_type);

      tree_set_loc(alt, CURRENT_LOC);
      tree_add_stmt(stmt, alt);
   } while (optional(tCOMMA));
}

static tree_t p_selected_variable_assignment(ident_t label)
{
   // with expression select [ ? ] target := selected_expressions ;

   BEGIN("selected variable assignment");

   require_std(STD_08, "selected variable assignment");

   consume(tWITH);

   tree_t value = p_expression();
   solve_types(nametab, value, NULL);

   consume(tSELECT);

   tree_kind_t kind = T_SELECT;
   if (optional(tQUESTION))
      kind = T_MATCH_SELECT;

   tree_t t = tree_new(kind);
   tree_set_value(t, value);

   tree_t target = p_target(NULL);

   // This is the easiest place to disambiguate variable and signal
   // assignment without a deep lookahead
   switch (one_of(tASSIGN, tLE)) {
   case tLE:
      p_selected_waveforms(t, target, NULL);
      break;

   case tASSIGN:
   default:
      p_selected_expressions(t, target);
      break;
   }

   consume(tSEMI);

   tree_set_loc(t, CURRENT_LOC);
   ensure_labelled(t, label);

   sem_check(t, nametab);
   return t;
}

static tree_t p_variable_assignment_statement(ident_t label, tree_t name)
{
   // [ label : ] simple_variable_assignment
   //   | 2008: [ label : ] conditional_variable_assignment
   //   | 2008: [ label : ] selected_variable_assignment

   EXTEND("variable assignment statement");

   if (name == NULL && peek() == tWITH)
      return p_selected_variable_assignment(label);
   else
      return p_simple_variable_assignment(label, name);
}

static tree_t p_waveform_element(tree_t target)
{
   // expression [ after expression ] | null [ after expression ]

   BEGIN("waveform element");

   tree_t w = tree_new(T_WAVEFORM);

   if (!optional(tNULL)) {
      tree_t value = p_expression();
      tree_set_value(w, value);

      type_t constraint;
      if (tree_has_type(target))
         constraint = tree_type(target);
      else
         constraint = solve_target(nametab, target, value);

      solve_known_subtype(nametab, value, constraint);
   }
   else if (!tree_has_type(target))
      solve_types(nametab, target, NULL);

   if (optional(tAFTER)) {
      tree_t delay = p_expression();
      tree_set_delay(w, delay);
      solve_types(nametab, delay, std_type(NULL, STD_TIME));
   }

   tree_set_loc(w, CURRENT_LOC);

   return w;
}

static void p_waveform(tree_t stmt, tree_t target)
{
   // waveform_element { , waveform_element } | unaffected

   BEGIN("waveform");

   if (optional(tUNAFFECTED)) {
      solve_types(nametab, target, NULL);
      return;
   }

   tree_add_waveform(stmt, p_waveform_element(target));

   while (optional(tCOMMA))
      tree_add_waveform(stmt, p_waveform_element(target));

   tree_set_loc(stmt, CURRENT_LOC);
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

static port_mode_t p_force_mode(void)
{
   // in | out

   BEGIN("force mode");

   switch (peek()) {
   case tIN: consume(tIN); return PORT_IN;
   case tOUT: consume(tOUT); return PORT_OUT;
   default: return PORT_INVALID;
   }
}

static tree_t p_simple_force_assignment(ident_t label, tree_t target)
{
   // target <= force [ force_mode ] expression ;

   EXTEND("simple force assignment");

   consume(tFORCE);

   require_std(STD_08, "simple force assignments");

   type_t target_type;
   if (tree_kind(target) == T_AGGREGATE) {
      parse_error(CURRENT_LOC, "target of a simple force assignment may "
                  "not be an aggregate");
      target_type = type_new(T_NONE);
   }
   else
      target_type = solve_types(nametab, target, NULL);

   tree_set_type(target, target_type);

   tree_t t = tree_new(T_FORCE);
   tree_set_target(t, target);
   tree_set_subkind(t, p_force_mode());

   tree_t expr = p_expression();
   solve_types(nametab, expr, target_type);

   tree_set_value(t, expr);

   consume(tSEMI);

   set_label_and_loc(t, label, CURRENT_LOC);
   sem_check(t, nametab);
   return t;
}

static tree_t p_simple_release_assignment(ident_t label, tree_t target)
{
   // target <= release [ force_mode ] ;

   EXTEND("simple force assignment");

   consume(tRELEASE);

   require_std(STD_08, "simple release assignments");

   type_t target_type;
   if (tree_kind(target) == T_AGGREGATE) {
      parse_error(CURRENT_LOC, "target of a simple release assignment may "
                  "not be an aggregate");
      target_type = type_new(T_NONE);
   }
   else
      target_type = solve_types(nametab, target, NULL);

   tree_set_type(target, target_type);

   tree_t t = tree_new(T_RELEASE);
   tree_set_target(t, target);
   tree_set_subkind(t, p_force_mode());

   consume(tSEMI);

   set_label_and_loc(t, label, CURRENT_LOC);
   sem_check(t, nametab);
   return t;
}

static tree_t p_signal_assignment_statement(ident_t label, tree_t name)
{
   // [ label : ] target <= [ delay_mechanism ] waveform ;

   EXTEND("signal assignment statement");

   tree_t target = p_target(name);

   consume(tLE);

   switch (peek()) {
   case tFORCE: return p_simple_force_assignment(label, target);
   case tRELEASE: return p_simple_release_assignment(label, target);
   default: break;
   }


   tree_t t = tree_new(T_SIGNAL_ASSIGN);
   tree_set_target(t, target);

   tree_t reject = p_delay_mechanism();

   p_waveform(t, target);

   if (peek() == tWHEN) {
      require_std(STD_08, "conditional signal assignment statements");

      tree_t stmt = tree_new(T_COND_ASSIGN);
      tree_set_target(stmt, target);

      p_conditional_waveforms(stmt, target, t);

      const int nconds = tree_conds(stmt);
      for (int i = 0; i < nconds; i++) {
         tree_t c = tree_cond(stmt, i);
         assert(tree_stmts(c) == 1);
         set_delay_mechanism(tree_stmt(c, 0), reject);
      }

      t = stmt;
   }
   else
      set_delay_mechanism(t, reject);

   consume(tSEMI);

   set_label_and_loc(t, label, CURRENT_LOC);
   sem_check(t, nametab);
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

   tree_set_value(wait, p_condition());
}

static void p_timeout_clause(tree_t wait)
{
   // for expression

   BEGIN("timeout clause");

   consume(tFOR);

   tree_t delay = p_expression();
   tree_set_delay(wait, delay);
   solve_types(nametab, delay, std_type(NULL, STD_TIME));
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
   sem_check(t, nametab);
   return t;
}

static tree_t p_assertion_statement(ident_t label)
{
   // [ label : ] assertion ;

   EXTEND("assertion statement");

   tree_t t = p_assertion();
   consume(tSEMI);

   set_label_and_loc(t, label, CURRENT_LOC);
   sem_check(t, nametab);
   return t;
}

static tree_t p_report_statement(ident_t label)
{
   // [ label : ] report expression [ severity expression ] ;

   EXTEND("report statement");

   tree_t t = tree_new(T_ASSERT);

   consume(tREPORT);

   tree_t m = p_expression();
   tree_set_message(t, m);
   solve_types(nametab, m, std_type(NULL, STD_STRING));

   tree_t s;
   if (optional(tSEVERITY))
      s = p_expression();
   else {
      tree_t std = find_std(nametab);
      s = make_ref(search_decls(std, ident_new("NOTE"), 0));
   }

   tree_set_severity(t, s);
   solve_types(nametab, s, std_type(NULL, STD_SEVERITY_LEVEL));

   consume(tSEMI);

   set_label_and_loc(t, label, CURRENT_LOC);
   sem_check(t, nametab);
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

   tree_t c0 = tree_new(T_COND_STMT);
   tree_set_value(c0, p_condition());
   tree_add_cond(t, c0);

   consume(tTHEN);

   p_sequence_of_statements(c0);

   tree_set_loc(c0, CURRENT_LOC);

   while (optional(tELSIF)) {
      tree_t c = tree_new(T_COND_STMT);
      tree_set_value(c, p_condition());
      tree_add_cond(t, c);

      consume(tTHEN);

      p_sequence_of_statements(c);

      tree_set_loc(c, CURRENT_LOC);
   }

   if (optional(tELSE)) {
      tree_t c = tree_new(T_COND_STMT);
      tree_add_cond(t, c);

      p_sequence_of_statements(c);

      tree_set_loc(c, CURRENT_LOC);
   }

   consume(tEND);
   consume(tIF);
   p_trailing_label(label);
   consume(tSEMI);

   tree_set_loc(t, CURRENT_LOC);
   sem_check(t, nametab);
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

static void p_parameter_specification(tree_t loop, tree_kind_t pkind)
{
   // identifier in discrete_range

   BEGIN("paremeter specification");

   ident_t id = p_identifier();

   consume(tIN);

   tree_t r = p_constrained_discrete_range(NULL);
   tree_add_range(loop, r);

   type_t base = tree_type(r);

   tree_t constraint = tree_new(T_CONSTRAINT);
   tree_set_subkind(constraint, C_RANGE);
   tree_add_range(constraint, r);

   type_t sub = type_new(T_SUBTYPE);
   type_set_base(sub, base);
   type_add_constraint(sub, constraint);

   tree_t param = tree_new(pkind);
   tree_set_ident(param, id);
   tree_set_type(param, sub);
   tree_set_loc(param, CURRENT_LOC);
   tree_set_class(param, C_CONSTANT);
   tree_set_subkind(param, PORT_IN);

   tree_add_decl(loop, param);

   insert_name(nametab, param, NULL);
}

static tree_t p_iteration_scheme(void)
{
   // while condition | for parameter_specification

   BEGIN("iteration scheme");

   if (optional(tWHILE)) {
      tree_t t = tree_new(T_WHILE);
      tree_set_value(t, p_condition());
      return t;
   }
   else if (optional(tFOR)) {
      tree_t t = tree_new(T_FOR);
      p_parameter_specification(t, T_PARAM_DECL);
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

   scope_set_container(nametab, t);
   set_label_and_loc(t, label, CURRENT_LOC);

   if (label != NULL)
      insert_name(nametab, t, NULL);

   sem_check(t, nametab);

   p_sequence_of_statements(t);

   consume(tEND);
   consume(tLOOP);
   p_trailing_label(label);
   consume(tSEMI);

   pop_scope(nametab);

   tree_set_loc(t, CURRENT_LOC);
   return t;
}

static tree_t p_return_statement(ident_t label)
{
   // [ label : ] return [ expression ] ;
   // 2019: [ label : ] return [ when condition ] ;
   // 2019: [ label : ] return conditional_or_unaffected_expression ;

   EXTEND("return statement");

   consume(tRETURN);

   tree_t stmt = NULL;

   if (optional(tWHEN)) {
      require_std(STD_19, "conditional return statement");

      stmt = tree_new(T_COND_RETURN);
      tree_set_value(stmt, p_condition());
   }
   else {
      stmt = tree_new(T_RETURN);

      if (peek() != tSEMI) {
         type_t return_type = NULL;
         tree_t subprog = find_enclosing(nametab, S_SUBPROGRAM);
         if (subprog != NULL && tree_kind(subprog) == T_FUNC_BODY)
            return_type = type_result(tree_type(subprog));

         tree_t value = p_conditional_or_unaffected_expression(STD_19);
         solve_types(nametab, value, return_type);
         tree_set_value(stmt, value);
      }
   }

   consume(tSEMI);

   set_label_and_loc(stmt, label, CURRENT_LOC);
   sem_check(stmt, nametab);
   return stmt;
}

static tree_t p_exit_statement(ident_t label)
{
   // [ label : ] exit [ label ] [ when condition ] ;

   EXTEND("exit statement");

   consume(tEXIT);

   tree_t t = tree_new(T_EXIT);

   if (peek() == tID) {
      ident_t id = p_identifier();
      tree_set_ident2(t, id);

      tree_t loop = resolve_name(nametab, CURRENT_LOC, id);
      if (loop != NULL && !is_loop_stmt(loop))
         parse_error(CURRENT_LOC, "%s is not a loop statement", istr(id));
   }
   else {
      tree_t loop = find_enclosing(nametab, S_LOOP);
      if (loop == NULL)
         parse_error(CURRENT_LOC, "cannot use exit statement outside loop");
      else
         tree_set_ident2(t, tree_ident(loop));
   }

   if (optional(tWHEN))
      tree_set_value(t, p_condition());

   consume(tSEMI);

   set_label_and_loc(t, label, CURRENT_LOC);
   sem_check(t, nametab);
   return t;
}

static tree_t p_next_statement(ident_t label)
{
   // [ label : ] next [ label ] [ when condition ] ;

   EXTEND("next statement");

   consume(tNEXT);

   tree_t t = tree_new(T_NEXT);

   if (peek() == tID) {
      ident_t id = p_identifier();
      tree_set_ident2(t, id);

      tree_t loop = resolve_name(nametab, CURRENT_LOC, id);
      if (loop != NULL && !is_loop_stmt(loop))
         parse_error(CURRENT_LOC, "%s is not a loop statement", istr(id));
   }
   else {
      tree_t loop = find_enclosing(nametab, S_LOOP);
      if (loop == NULL)
         parse_error(CURRENT_LOC, "cannot use next statement outside loop");
      else
         tree_set_ident2(t, tree_ident(loop));
   }

   if (optional(tWHEN))
      tree_set_value(t, p_condition());

   consume(tSEMI);

   set_label_and_loc(t, label, CURRENT_LOC);
   sem_check(t, nametab);
   return t;
}

static tree_t p_procedure_call_statement(ident_t label, tree_t name)
{
   // [ label : ] procedure_call ;

   EXTEND("procedure call statement");

   tree_t call = NULL;

   switch (tree_kind(name)) {
   case T_REF:
      call = tree_new(T_PCALL);
      tree_set_ident2(call, tree_ident(name));
      break;

   case T_PROT_REF:
      call = tree_new(T_PROT_PCALL);
      tree_set_ident2(call, tree_ident(name));
      tree_set_name(call, tree_value(name));
      break;

   default:
      // Only print an error if name is a valid expression
      if (!type_is_none(solve_types(nametab, name, NULL)))
         parse_error(CURRENT_LOC, "expected procedure name");

      call = tree_new(T_PCALL);
      tree_set_ident2(call, error_marker());
      set_label_and_loc(call, label, CURRENT_LOC);
      drop_tokens_until(tSEMI);
      return call;
   }

   if (optional(tLPAREN)) {
      p_actual_parameter_part(call);
      consume(tRPAREN);
   }

   consume(tSEMI);

   set_label_and_loc(call, label, CURRENT_LOC);

   solve_types(nametab, call, NULL);
   sem_check(call, nametab);
   return call;
}

static tree_t p_case_statement_alternative(type_t type)
{
   // when choices => sequence_of_statements

   BEGIN("case statement alternative");

   consume(tWHEN);

   tree_t alt = tree_new(T_ALTERNATIVE);

   p_choices(alt, type);

   consume(tASSOC);

   p_sequence_of_statements(alt);

   tree_set_loc(alt, CURRENT_LOC);
   return alt;
}

static tree_t p_case_statement(ident_t label)
{
   // [ label : ] case [?] expression is case_statement_alternative
   //   { case_statement_alternative } end case [?] [ label ] ;

   EXTEND("case statement");

   consume(tCASE);

   tree_kind_t kind = T_CASE;
   if (optional(tQUESTION)) {
      require_std(STD_08, "matching case statements");
      kind = T_MATCH_CASE;
   }

   tree_t t = tree_new(kind);

   tree_t value = p_expression();
   tree_set_value(t, value);

   type_t type = solve_types(nametab, value, NULL);

   consume(tIS);

   do {
      tree_add_stmt(t, p_case_statement_alternative(type));
   } while (peek() == tWHEN);

   consume(tEND);
   consume(tCASE);

   if (kind == T_MATCH_CASE)
      consume(tQUESTION);

   p_trailing_label(label);
   consume(tSEMI);

   tree_set_loc(t, CURRENT_LOC);
   sem_check(t, nametab);
   return t;
}

static void p_sequential_block_declarative_part(tree_t block)
{
   // { process_declarative_item }

   BEGIN("sequential block declarative part");

   while (not_at_token(tBEGIN))
      p_process_declarative_item(block);
}

static void p_sequential_block_statement_part(tree_t block)
{
   // { sequential_statement }

   BEGIN("sequential block statement part");

   p_sequence_of_statements(block);
}

static tree_t p_sequential_block_statement(ident_t label)
{
   // [ label : ] block  [ is ] sequential_block_declarative_part
   //   begin sequential_block_statement_part end [ block ] [ label ] ;

   BEGIN("sequential block statement");

   consume(tBLOCK);
   optional(tIS);

   require_std(STD_19, "sequential block statements");

   push_scope(nametab);

   tree_t t = tree_new(T_SEQUENCE);

   scope_set_container(nametab, t);
   set_label_and_loc(t, label, CURRENT_LOC);

   p_sequential_block_declarative_part(t);

   consume(tBEGIN);

   p_sequential_block_statement_part(t);

   consume(tEND);
   optional(tBLOCK);

   p_trailing_label(label);
   consume(tSEMI);

   tree_set_loc(t, CURRENT_LOC);
   sem_check(t, nametab);
   pop_scope(nametab);

   return t;
}

static tree_t p_sequential_statement(void)
{
   // wait_statement | assertion_statement | report_statement
   //   | signal_assignment_statement | variable_assignment_statement
   //   | procedure_call_statement | if_statement | case_statement
   //   | loop_statement | next_statement | exit_statement | return_statement
   //   | null_statement | 2019: sequential_block_statement

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

   case tWITH:
      return p_variable_assignment_statement(label, NULL);

   case tID:
   case tLTLT:
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

   case tBLOCK:
      return p_sequential_block_statement(label);

   default:
      expect(tWAIT, tID, tASSERT, tREPORT, tIF, tNULL, tRETURN, tCASE, tWHILE,
             tFOR, tLOOP, tEXIT, tNEXT, tWITH, tLTLT, tLPAREN, tBLOCK);
      drop_tokens_until(tSEMI);
      return tree_new(T_NULL);
   }

   tree_t name = p_name(N_SUBPROGRAM);

   switch (peek()) {
   case tASSIGN:
      return p_variable_assignment_statement(label, name);

   case tLE:
      return p_signal_assignment_statement(label, name);

   case tSEMI:
   case tLPAREN:
      return p_procedure_call_statement(label, name);

   default:
      expect(tASSIGN, tLE, tSEMI);
      drop_tokens_until(tSEMI);
      return tree_new(T_NULL);
   }
}

static tree_t p_instantiated_unit(tree_t name)
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

   if (name != NULL) {
      if (tree_kind(name) == T_REF) {
         tree_set_ident2(t, tree_ident(name));
         if (tree_has_ref(name))
            tree_set_ref(t, tree_ref(name));
      }
      else {
         parse_error(tree_loc(name), "invalid instantiated unit name");
         tree_set_ident2(t, error_marker());
      }
   }
   else
      tree_set_ident2(t, p_selected_identifier());

   if ((tree_class(t) == C_ENTITY) && optional(tLPAREN)) {
      tree_set_ident2(t, ident_prefix(tree_ident2(t), p_identifier(), '-'));
      consume(tRPAREN);
   }

   tree_set_loc(t, CURRENT_LOC);
   return t;
}

static tree_t p_component_instantiation_statement(ident_t label, tree_t name)
{
   // label : instantiated_unit [ generic_map_aspect ] [ port_map_aspect ] ;

   EXTEND("component instantiation statement");

   tree_t t = p_instantiated_unit(name);
   tree_set_ident(t, label);

   tree_t ref = find_binding(t);
   tree_set_ref(t, ref);

   tree_t entity = ref ? primary_unit_of(ref) : NULL;

   tree_t spec = query_spec(nametab, t);
   if (spec != NULL)
      tree_set_spec(t, spec);

   if (label != NULL)
      insert_name(nametab, t, NULL);

   push_scope(nametab);

   if (peek() == tGENERIC)
      p_generic_map_aspect(t, entity);

   if (peek() == tPORT)
      p_port_map_aspect(t, entity);

   consume(tSEMI);

   tree_set_loc(t, CURRENT_LOC);

   if (label == NULL){
      parse_error(CURRENT_LOC, "component instantiation statement must "
                  "have a label");
      tree_set_ident(t, error_marker());
   }

   sem_check(t, nametab);
   pop_scope(nametab);

   return t;
}

static void p_options(tree_t *reject, tree_t *guard)
{
   // [ guarded ] [ delay_mechanism ]

   BEGIN("options");

   if (optional(tGUARDED)) {
      tree_t decl = NULL;
      name_mask_t mask = query_name(nametab, ident_new("GUARD"), &decl);
      if ((mask & N_OBJECT) && decl != NULL) {
         *guard = make_ref(decl);
         tree_set_loc(*guard, CURRENT_LOC);
      }
      else
         parse_error(CURRENT_LOC, "guarded assignment has no visible "
                     "guard signal");
   }

   *reject = p_delay_mechanism();
}

static void p_conditional_waveforms(tree_t stmt, tree_t target, tree_t s0)
{
   // { waveform when condition else } waveform [ when condition ]

   BEGIN("conditional waveforms");

   for (;;) {
      tree_t c = tree_new(T_COND_STMT);

      tree_t a = s0;
      if (a == NULL) {
         a = tree_new(T_SIGNAL_ASSIGN);
         tree_set_target(a, target);
         p_waveform(a, target);
      }
      else {
         s0 = NULL;
         tree_set_loc(a, CURRENT_LOC);
      }
      tree_add_stmt(c, a);
      tree_add_cond(stmt, c);

      if (optional(tWHEN)) {
         tree_t when = p_condition();
         tree_set_value(c, when);
         tree_set_loc(c, tree_loc(when));
         solve_types(nametab, when, std_type(NULL, STD_BOOLEAN));

         if (!optional(tELSE))
            break;
      }
      else
         break;
   }
}

static tree_t p_conditional_signal_assignment(tree_t name)
{
   // target <= options conditional_waveforms ;

   BEGIN("conditional signal assignment");

   tree_t conc = tree_new(T_CONCURRENT);
   tree_t stmt = tree_new(T_COND_ASSIGN);
   tree_add_stmt(conc, stmt);

   tree_t target = p_target(name);
   tree_set_target(stmt, target);

   consume(tLE);

   tree_t reject = NULL, guard = NULL;
   p_options(&reject, &guard);

   if (guard != NULL)
      tree_set_guard(conc, guard);

   p_conditional_waveforms(stmt, target, NULL);

   const int nconds = tree_conds(stmt);
   for (int i = 0; i < nconds; i++) {
      tree_t c = tree_cond(stmt, i);
      assert(tree_stmts(c) == 1);
      set_delay_mechanism(tree_stmt(c, 0), reject);
   }

   consume(tSEMI);

   tree_set_loc(stmt, CURRENT_LOC);
   tree_set_loc(conc, CURRENT_LOC);
   return conc;
}

static void p_selected_waveforms(tree_t stmt, tree_t target, tree_t reject)
{
   // { waveform when choices , } waveform when choices

   BEGIN("selected waveforms");

   type_t with_type = tree_type(tree_value(stmt));

   do {
      tree_t a = tree_new(T_SIGNAL_ASSIGN);
      tree_set_target(a, target);
      if (reject != NULL)
         tree_set_reject(a, reject);

      p_waveform(a, target);

      sem_check(a, nametab);

      consume(tWHEN);

      tree_t alt = tree_new(T_ALTERNATIVE);
      tree_add_stmt(alt, a);

      p_choices(alt, with_type);

      tree_set_loc(alt, CURRENT_LOC);
      tree_add_stmt(stmt, alt);
   } while (optional(tCOMMA));
}

static tree_t p_selected_signal_assignment(void)
{
   // with expression select target <= options selected_waveforms ;

   BEGIN("selected signal assignment");

   consume(tWITH);

   tree_t value = p_expression();
   solve_types(nametab, value, NULL);

   consume(tSELECT);

   tree_t stmt;
   if (optional(tQUESTION)) {
      require_std(STD_08, "matching select statements");
      stmt = tree_new(T_MATCH_SELECT);
   }
   else
      stmt = tree_new(T_SELECT);

   tree_set_value(stmt, value);

   tree_t conc = tree_new(T_CONCURRENT);
   tree_add_stmt(conc, stmt);

   tree_t target = p_target(NULL);

   consume(tLE);

   tree_t reject = NULL, guard = NULL;
   p_options(&reject, &guard);

   if (guard != NULL)
      tree_set_guard(conc, guard);

   p_selected_waveforms(stmt, target, reject);
   consume(tSEMI);

   tree_set_loc(stmt, CURRENT_LOC);
   tree_set_loc(conc, CURRENT_LOC);
   return conc;
}

static tree_t p_concurrent_signal_assignment_statement(ident_t label,
                                                       tree_t name)
{
   // [ label : ] [ postponed ] conditional_signal_assignment
   //   | [ label : ] [ postponed ] selected_signal_assignment

   EXTEND("concurrent signal assignment statement");

   const bool postponed = name == NULL && optional(tPOSTPONED);

   tree_t t;
   if (peek() == tWITH) {
      assert(name == NULL);
      t = p_selected_signal_assignment();
   }
   else
      t = p_conditional_signal_assignment(name);

   tree_set_loc(t, CURRENT_LOC);
   ensure_labelled(t, label);

   if (postponed)
      tree_set_flag(t, TREE_F_POSTPONED);

   if (label) insert_name(nametab, t, NULL);
   sem_check(t, nametab);
   return t;
}

static tree_t p_concurrent_procedure_call_statement(ident_t label, tree_t name)
{
   // [ label : ] [ postponed ] procedure_call ;

   EXTEND("concurrent procedure call statement");

   const bool postponed = name == NULL && optional(tPOSTPONED);

   tree_t call = NULL;
   if (name == NULL) {
      call = tree_new(T_PCALL);
      tree_set_ident2(call, p_identifier());
   }
   else if (tree_kind(name) == T_PROT_REF) {
      call = tree_new(T_PROT_PCALL);
      tree_set_ident2(call, tree_ident(name));
      tree_set_name(call, tree_value(name));
   }
   else {
      call = tree_new(T_PCALL);
      tree_set_ident2(call, tree_ident(name));
   }

   if (optional(tLPAREN)) {
      p_actual_parameter_part(call);
      consume(tRPAREN);
   }

   consume(tSEMI);

   tree_set_loc(call, CURRENT_LOC);

   solve_types(nametab, call, NULL);

   tree_t conc = tree_new(T_CONCURRENT);
   tree_add_stmt(conc, call);

   if (postponed)
      tree_set_flag(conc, TREE_F_POSTPONED);

   tree_set_loc(conc, CURRENT_LOC);
   ensure_labelled(conc, label);
   tree_set_ident(call, tree_ident(conc));

   if (label) insert_name(nametab, conc, NULL);
   sem_check(conc, nametab);
   return conc;
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

static void p_block_header(tree_t block)
{
   // [ generic_clause [ generic_map_aspect ; ] ]
   //   [ port_clause [ port_map_aspect ; ] ]

   if (peek() == tGENERIC) {
      p_generic_clause(block);

      if (peek() == tGENERIC) {
         p_generic_map_aspect(block, block);
         consume(tSEMI);
      }

      insert_generics(nametab, block);
   }

   if (peek() == tPORT) {
      p_port_clause(block);

      if (peek() == tPORT) {
         p_port_map_aspect(block, block);
         consume(tSEMI);
      }

      insert_ports(nametab, block);
   }
}

static tree_t p_block_statement(ident_t label)
{
   // label : block [ ( expression ) ] [ is ] block_header
   //   block_declarative_part begin block_statement_part end block [ label ] ;

   EXTEND("block statement");

   tree_t b = tree_new(T_BLOCK);
   tree_set_ident(b, label);

   consume(tBLOCK);

   if (label == NULL)
      parse_error(CURRENT_LOC, "block statement must have a label");
   else {
      tree_set_loc(b, CURRENT_LOC);
      insert_name(nametab, b, NULL);
   }

   push_scope(nametab);
   scope_set_prefix(nametab, label ?: error_marker());
   scope_set_container(nametab, b);

   if (peek() == tLPAREN) {
      consume(tLPAREN);

      tree_t expr = p_expression();
      solve_condition(nametab, &expr, std_type(NULL, STD_BOOLEAN));

      make_implicit_guard_signal(b, expr);

      consume(tRPAREN);
   }

   optional(tIS);
   p_block_header(b);
   p_block_declarative_part(b);
   consume(tBEGIN);
   p_block_statement_part(b);
   consume(tEND);
   consume(tBLOCK);
   p_trailing_label(label);
   consume(tSEMI);

   pop_scope(nametab);

   tree_set_loc(b, CURRENT_LOC);
   sem_check(b, nametab);
   return b;
}

static void p_generate_statement_body(tree_t container, ident_t alt_label)
{
   // [ block_declarative_part begin ] { concurrent_statement }
   //   [ end [ alternative_label ] ; ]

   BEGIN("generate statement body");

   if (scan(tSIGNAL, tTYPE, tSUBTYPE, tFILE, tCONSTANT, tFUNCTION, tIMPURE,
            tPURE, tALIAS, tATTRIBUTE, tBEGIN, tPROCEDURE, tFOR, tCOMPONENT,
            tUSE, tSHARED)) {
      while (not_at_token(tBEGIN))
         p_block_declarative_item(container);
      consume(tBEGIN);
   }

   while (not_at_token(tEND, tELSIF, tELSE, tWHEN))
      tree_add_stmt(container, p_concurrent_statement());

   if (peek() == tEND && (peek_nth(2) == tID || peek_nth(2) == tSEMI)) {
      consume(tEND);
      p_trailing_label(alt_label);
      consume(tSEMI);
   }
}

static tree_t p_for_generate_statement(ident_t label)
{
   // for generate_parameter_specification generate generate_statement_body
   //   end generate [ generate_label ] ;

   EXTEND("for generate statement");

   consume(tFOR);

   push_scope(nametab);
   scope_set_prefix(nametab, label);

   tree_t g = tree_new(T_FOR_GENERATE);
   tree_set_ident(g, label);

   scope_set_container(nametab, g);

   p_parameter_specification(g, T_GENERIC_DECL);

   consume(tGENERATE);

   p_generate_statement_body(g, NULL);

   consume(tEND);
   consume(tGENERATE);
   p_trailing_label(label);
   consume(tSEMI);

   pop_scope(nametab);

   if (label == NULL)
      parse_error(CURRENT_LOC, "generate statement must have a label");

   tree_set_loc(g, CURRENT_LOC);
   sem_check(g, nametab);
   return g;
}

static tree_t p_if_generate_statement(ident_t label)
{
   // if [ alternative_label : ] condition generate generate_statement_body
   //   { elsif [ alternative_label : ] condition generate
   //     generate_statement_body }
   //   [ else [ alternative_label : ] generate generate_statement_body ]
   //   end generate [ generate_label ] ;

   EXTEND("if generate statement");

   consume(tIF);

   tree_t g = tree_new(T_IF_GENERATE);
   tree_set_ident(g, label);

   ident_t alt_label = NULL;
   if (peek() == tID && peek_nth(2) == tCOLON) {
      require_std(STD_08, "alternative labels");

      alt_label = p_identifier();
      consume(tCOLON);
   }

   push_scope(nametab);
   scope_set_container(nametab, g);
   scope_set_prefix(nametab, alt_label ?: label);

   tree_t c0 = tree_new(T_COND_STMT);
   tree_set_ident(c0, alt_label ?: label);
   tree_set_value(c0, p_condition());

   tree_add_cond(g, c0);

   consume(tGENERATE);

   p_generate_statement_body(c0, alt_label);

   pop_scope(nametab);

   tree_set_loc(c0, CURRENT_LOC);

   while (optional(tELSIF)) {
      require_std(STD_08, "elsif in generate statements");

      ident_t alt_label = NULL;
      if (peek() == tID && peek_nth(2) == tCOLON) {
         alt_label = p_identifier();
         consume(tCOLON);
      }

      push_scope(nametab);
      scope_set_prefix(nametab, alt_label ?: label);

      tree_t c = tree_new(T_COND_STMT);
      tree_set_ident(c, alt_label ?: label);
      tree_set_value(c, p_condition());

      consume(tGENERATE);

      p_generate_statement_body(c, alt_label);

      pop_scope(nametab);

      tree_set_loc(c, CURRENT_LOC);
      tree_add_cond(g, c);
   }

   if (optional(tELSE)) {
      require_std(STD_08, "else in generate statements");

      ident_t alt_label = label;
      if (peek() == tID && peek_nth(2) == tCOLON) {
         alt_label = p_identifier();
         consume(tCOLON);
      }

      push_scope(nametab);
      scope_set_prefix(nametab, alt_label ?: label);

      tree_t c = tree_new(T_COND_STMT);
      tree_set_ident(c, alt_label ?: label);

      consume(tGENERATE);

      p_generate_statement_body(c, alt_label);

      pop_scope(nametab);

      tree_set_loc(c, CURRENT_LOC);
      tree_add_cond(g, c);
   }

   consume(tEND);
   consume(tGENERATE);
   p_trailing_label(label);
   consume(tSEMI);

   if (label == NULL)
      parse_error(CURRENT_LOC, "generate statement must have a label");

   tree_set_loc(g, CURRENT_LOC);
   sem_check(g, nametab);
   return g;
}

static tree_t p_case_generate_alternative(type_t type)
{
   // when [ alternative_label : ] choices => generate_statement_body

   BEGIN("case generate alternative");

   consume(tWHEN);

   ident_t alt_label = NULL;
   if (peek() == tID && peek_nth(2) == tCOLON) {
      alt_label = p_identifier();
      consume(tCOLON);
   }

   tree_t alt = tree_new(T_ALTERNATIVE);
   tree_set_ident(alt, alt_label);
   p_choices(alt, type);

   consume(tASSOC);

   push_scope(nametab);
   scope_set_prefix(nametab, alt_label);

   p_generate_statement_body(alt, alt_label);

   tree_set_loc(alt, CURRENT_LOC);
   pop_scope(nametab);

   return alt;
}

static tree_t p_case_generate_statement(ident_t label)
{
   // case expression generate case_generate_alternative
   //   { case_generate_alternative } end generate [ generate_label ] ;

   EXTEND("case generate statement");

   consume(tCASE);

   require_std(STD_08, "case generate statements");

   tree_t g = tree_new(T_CASE_GENERATE);
   tree_set_ident(g, label);

   tree_t value = p_expression();
   tree_set_value(g, value);

   type_t type = solve_types(nametab, value, NULL);

   consume(tGENERATE);

   do {
      tree_add_stmt(g, p_case_generate_alternative(type));
   } while (peek() == tWHEN);

   consume(tEND);
   consume(tGENERATE);

   p_trailing_label(label);
   consume(tSEMI);

   if (label == NULL)
      parse_error(CURRENT_LOC, "generate statement must have a label");

   tree_set_loc(g, CURRENT_LOC);
   sem_check(g, nametab);
   return g;
}

static tree_t p_generate_statement(ident_t label)
{
   // for_generate_statement | if_generate_statement | case_generate_statement

   EXTEND("generate statement");

   switch (peek()) {
   case tFOR:
      return p_for_generate_statement(label);
   case tIF:
      return p_if_generate_statement(label);
   case tCASE:
      return p_case_generate_statement(label);
   default:
      expect(tFOR, tIF, tCASE);
      drop_tokens_until(tSEMI);
      return ensure_labelled(tree_new(T_BLOCK), label);
   }
}

static psl_node_t p_psl_value_range(void)
{
   //   Value
   // | finite_Range

   BEGIN("PSL Value Range");

   // TODO: Enforce Numeric or Boolean on "l" and "r"

   scan_as_vhdl();

   tree_t l = p_expression();
   tree_t tgt = l;

   if (optional(tTO)) {
      tree_t range = tree_new(T_RANGE);
      tree_t r = p_expression();

      tree_set_left(range, l);
      tree_set_right(range, r);
      tree_set_subkind(range, RANGE_TO);
      tgt = range;
   }

   psl_node_t p = psl_new(P_HDL_EXPR);
   psl_set_tree(p, tgt);

   scan_as_psl();

   return p;
}

static psl_node_t p_psl_value_set(void)
{
   //   { Value_Range { , Value_Range } }
   // | boolean

   psl_node_t p = psl_new(P_VALUE_SET);

   token_t tok = one_of(tBOOLEAN, tLBRACE);
   if (tok == tBOOLEAN) {
      psl_set_subkind(p, PSL_VALUE_SET_BOOLEAN);
      return p;
   }

   psl_set_subkind(p, PSL_VALUE_SET_EXPLICIT);

   psl_add_operand(p, p_psl_value_range());
   while (optional(tCOMMA))
      psl_add_operand(p, p_psl_value_range());

   consume(tRBRACE);
   return p;
}

static psl_node_t p_psl_or_hdl_expression(void)
{
   // HDL_Expression | PSL_Expression | Built_In_Function_Call
   //    | Union_Expression

   BEGIN("PSL or HDL expression");

   tree_t expr = p_psl_condition();

   psl_node_t p = psl_new(P_HDL_EXPR);
   psl_set_tree(p, expr);
   psl_set_loc(p, tree_loc(expr));
   psl_set_type(p, PSL_TYPE_BOOLEAN);

   if (optional(tUNION)) {
      psl_node_t new = psl_new(P_UNION);
      psl_add_operand(new, p);
      psl_add_operand(new, p_psl_or_hdl_expression());
      return new;
   }

   return p;
}

static psl_node_t p_psl_clock_expression(void)
{
   tree_t expr = p_expression();
   solve_types(nametab, expr, std_type(NULL, STD_BOOLEAN));

   psl_node_t p = psl_new(P_CLOCK_DECL);
   psl_set_tree(p, expr);

   return p;
}

static psl_node_t p_psl_clock_declaration(tree_t parent)
{
   // default clock is Clock_Expression ;

   assert(tree_kind(parent) == T_PSL);

   BEGIN("PSL clock declaration");

   scan_as_psl();

   consume(tDEFAULT);
   consume(tCLOCK);

   consume(tIS);

   scan_as_vhdl();

   psl_node_t p = p_psl_clock_expression();

   tree_set_ident(parent, well_known(W_DEFAULT_CLOCK));
   consume(tSEMI);

   psl_set_loc(p, CURRENT_LOC);
   psl_check(p);

   return p;
}

static tree_t p_psl_builtin_function_call(void)
{
   // prev (Any_Type [ , Number [ , Clock_Expression ]] )
   //  | next ( Any_Type )
   //  | stable ( Any_Type [ , Clock_Expression ] )
   //  | rose ( Bit [ , Clock_Expression ] )
   //  | fell ( Bit [ , Clock_Expression ] )
   //  | ended ( Sequence [ , Clock_Expression ])
   //  | nondet ( Value_Set )
   //  | nondet_vector ( Number, Value_Set)

   BEGIN("PSL Built-in Function call");

   token_t tok = one_of(tNEXT, tPREV, tSTABLE, tROSE, tFELL, tENDED,
                        tNONDET, tNONDETV);

   psl_node_t p = psl_new(P_BUILTIN_FUNC);

   // Parse the function call
   consume(tLPAREN);

   switch (tok) {
   case tNEXT:
   case tPREV:
   case tSTABLE:
   case tROSE:
   case tFELL:
   {
      psl_node_t p1 = p_psl_or_hdl_expression();
      // TODO: Enfore "bit" for "rose" and "fell"
      psl_add_operand(p, p1);

      if (tok == tNEXT)
         break;

      if (tok == tPREV && optional(tCOMMA)) {
         psl_node_t p2 = p_psl_or_hdl_expression();
         // TODO: Enforce numeric value on p2
         psl_add_operand(p, p2);
      }

      if (optional(tCOMMA))
         psl_set_clock(p, p_psl_clock_expression());

      break;
   }

   case tENDED:
   {
      // TODO: Enforce sequence on p
      psl_add_operand(p, p_psl_sequence());

      if (optional(tCOMMA))
         psl_set_clock(p, p_psl_clock_expression());

      break;
   }
   case tNONDETV:
   {
      psl_node_t p1 = p_psl_or_hdl_expression();
      // TODO: Enforce number on p1
      psl_add_operand(p, p1);
   }

   case tNONDET:
   {
      psl_node_t p2 = p_psl_value_set();
      psl_add_operand(p, p2);
      break;
   }
   }

   consume(tRPAREN);

   // Determine function kind and return type
   unsigned kind = 0;
   type_t rvt = type_new(T_NONE);
   switch (tok) {
   case tNEXT:
      kind = PSL_BUILTIN_NEXT;
      break;
   case tPREV:
      kind = PSL_BUILTIN_PREV;
      break;
   case tSTABLE:
      kind = PSL_BUILTIN_STABLE;
      rvt = std_type(NULL, STD_BOOLEAN);
      break;
   case tROSE:
      kind = PSL_BUILTIN_ROSE;
      rvt = std_type(NULL, STD_BOOLEAN);
      break;
   case tFELL:
      kind = PSL_BUILTIN_FELL;
      rvt = std_type(NULL, STD_BOOLEAN);
      break;
   case tENDED:
      kind = PSL_BUILTIN_ENDED;
      rvt = std_type(NULL, STD_BOOLEAN);
      break;
   case tNONDET:
      kind = PSL_BUILTIN_NONDET;
      break;
   case tNONDETV:
      kind = PSL_BUILTIN_NONDET_VECTOR;
      rvt = std_type(NULL, STD_BIT_VECTOR);
      break;
   }
   psl_set_subkind(p, kind);

   tree_t t_psl = tree_new(T_PSL);
   tree_set_psl(t_psl, p);

   tree_t fcall = tree_new(T_FCALL);
   tree_set_ref(fcall, t_psl);
   tree_set_type(fcall, rvt);

   return fcall;
}

static psl_node_t p_psl_parameter_definition(void)
{
   // PSL_Identifier [ Index_Range ] in Value_Set

   BEGIN("PSL Parameter definition");

   psl_node_t p = psl_new(P_PARAM);
   tree_t param = tree_new(T_PARAM);
   ident_t name = p_identifier();
   tree_set_ident(param, name);
   insert_name(nametab, param, NULL);

   if (optional(tLSQUARE)) {

      scan_as_vhdl();

      // TODO: Enforce left and right to be numeric
      tree_t range = tree_new(T_RANGE);
      tree_set_left(range, p_expression());
      consume(tTO);
      tree_set_subkind(range, RANGE_TO);
      tree_set_right(range, p_expression());

      scan_as_psl();

      tree_set_value(param, range);
      psl_set_tree(p, param);

      consume(tRSQUARE);
   }

   consume(tIN);

   psl_set_value(p, p_psl_value_set());

   return p;
}

static void p_psl_parameters_definition(psl_node_t p)
{
   // Parameter_Definition {, Parameter_Definition }

   psl_add_operand(p, p_psl_parameter_definition());

   while (optional(tCOMMA))
      psl_add_operand(p, p_psl_parameter_definition());
}

static psl_node_t p_psl_parametrized_sere(void)
{
   // for Parameters_Definition : And_Or_SERE_OP { SERE }

   BEGIN("PSL Parametrized SERE");

   consume(tFOR);

   psl_node_t p = psl_new(P_SERE);
   p_psl_parameters_definition(p);

   consume(tCOLON);

   switch (one_of(tAMP, tDBLAMP, tBAR)) {
   case tAMP:
      psl_set_subkind(p, PSL_SERE_PARAM_NEQ_AND);
      break;
   case tDBLAMP:
      psl_set_subkind(p, PSL_SERE_PARAM_EQU_AND);
      break;
   case tBAR:
      psl_set_subkind(p, PSL_SERE_PARAM_OR);
      break;
   }

   // Last operand is always SERE that is parametrized
   consume(tLBRACE);
   psl_add_operand(p, p_psl_sere());
   consume(tRBRACE);

   return p;
}

static psl_node_t p_psl_compound_sere(void)
{
   //       Repeated_SERE
   //     | Braced_SERE
   //     | Clocked_SERE
   //     | Compound_SERE | Compound_SERE
   //     | Compound_SERE & Compound_SERE
   //     | Compound_SERE && Compound_SERE
   //     | Compound_SERE within Compound_SERE
   //     | Parameterized_SERE

   BEGIN("PSL Compound SERE");

   psl_node_t p;
   if (peek() == tFOR)
      p = p_psl_parametrized_sere();
   else
      p = p_psl_sequence();

   token_t prev = 0;
   while (scan(tBAR, tAMP, tDBLAMP, tWITHIN)) {
      token_t tok = one_of(tBAR, tAMP, tDBLAMP, tWITHIN);

      // Recurse to new SERE
      if (tok != prev) {
         psl_node_t new = psl_new(P_SERE);
         psl_add_operand(new, p);
         p = new;
      }

      switch (tok) {
      case tBAR:
         psl_set_subkind(p, PSL_SERE_OR);
         break;
      case tAMP:
         psl_set_subkind(p, PSL_SERE_NEQ_AND);
         break;
      case tDBLAMP:
         psl_set_subkind(p, PSL_SERE_EQU_AND);
         break;
      case tWITHIN:
         psl_set_subkind(p, PSL_SERE_WITHIN);
         break;
      }

      psl_add_operand(p, p_psl_compound_sere());
   }

   return p;
}

static psl_node_t p_psl_sere(void)
{
   // Boolean | Boolean Proc_Block | Sequence | SERE ; SERE | SERE : SERE
   //  | Compound_SERE

   BEGIN("PSL SERE");

   psl_node_t p = p_psl_compound_sere();
   psl_set_loc(p, CURRENT_LOC);

   token_t prev = 0;
   while (scan(tSEMI, tCOLON)) {
      token_t tok = one_of(tSEMI, tCOLON);

      // Recurse to new SERE
      if (tok != prev) {
         psl_node_t new = psl_new(P_SERE);
         psl_add_operand(new, p);
         p = new;
      }

      if (tok == tSEMI)
         psl_set_subkind(p, PSL_SERE_CONCAT);
      else
         psl_set_subkind(p, PSL_SERE_FUSION);

      psl_add_operand(p, p_psl_compound_sere());

      prev = tok;
   };

   return p;
}

static psl_node_t p_psl_braced_or_clocked_sere(void)
{
   // { SERE }
   // { SERE } @ Clock_Expression

   BEGIN("PSL Braced or Clocked SERE");

   consume(tLBRACE);
   psl_node_t p = p_psl_sere();
   consume(tRBRACE);

   if (optional(tAT))
      psl_set_clock(p, p_psl_clock_expression());

   return p;
}

static tree_t p_psl_count(void)
{
   //   Number
   // | Range

   BEGIN("PSL Count");

   tree_t range = tree_new(T_RANGE);

   scan_as_vhdl();

   tree_set_subkind(range, RANGE_TO);

   tree_t l = p_expression();
   solve_types(nametab, l, std_type(NULL, STD_INTEGER));
   tree_set_left(range, l);

   if (optional(tTO)) {
      tree_t r = p_expression();
      solve_types(nametab, r, std_type(NULL, STD_INTEGER));
      tree_set_right(range, r);
   }

   scan_as_psl();

   return range;
}


static psl_node_t p_psl_repeat_scheme(void)
{
   //   [* [ Count ] ]
   // | [+]
   // | [= Count ]
   // | [-> [ positive_Count ] ]

   BEGIN("PSL Repeat Scheme");

   psl_node_t rpt = psl_new(P_REPEAT);

   const token_t tok = one_of(tPLUSRPT, tTIMESRPT, tGOTORPT, tARROWRPT);
   switch (tok) {
   case tPLUSRPT:
      psl_set_subkind(rpt, PSL_PLUS_REPEAT);
      break;

   case tTIMESRPT:
      psl_set_subkind(rpt, PSL_TIMES_REPEAT);
      psl_set_tree(rpt, p_psl_count());
      consume(tRSQUARE);
      break;

   case tGOTORPT:
      psl_set_subkind(rpt, PSL_GOTO_REPEAT);
      psl_set_tree(rpt, p_psl_count());
      consume(tRSQUARE);
      break;

   case tARROWRPT:
      psl_set_subkind(rpt, PSL_ARROW_REPEAT);
      psl_set_tree(rpt, p_psl_count());
      consume(tRSQUARE);
      break;
   }

   return rpt;
}

static tree_t p_psl_proc_block(void)
{
   // TODO: PSL LRM does not define "[[" and "]]" as token, thus we keep it as two
   // consecutive square brace tokens (possibly split by space). However, all examples
   // and grammar description in the PSL LRM never puts spae between these two.
   consume(tLSQUARE);
   consume(tLSQUARE);

   tree_t b = tree_new(T_BLOCK);
   tree_set_loc(b, CURRENT_LOC);

   scan_as_vhdl();

   while (peek() != tRSQUARE && peek_nth(2) != tRSQUARE) {

      if (scan(tSIGNAL, tTYPE, tSUBTYPE, tFILE, tCONSTANT, tFUNCTION, tIMPURE,
               tPURE, tPROCEDURE, tALIAS, tATTRIBUTE, tFOR, tCOMPONENT, tUSE,
               tSHARED, tDISCONNECT, tGROUP, tPACKAGE))
         p_block_declarative_item(b);
      else
         tree_add_stmt(b, p_sequential_statement());
   }

   scan_as_psl();

   consume(tRSQUARE);
   consume(tRSQUARE);

   return b;
}

static type_t p_psl_subtype_indication(void)
{
   BEGIN("PSL subtype indication");

   // Handle ambiguity in "p_subtype_indication". Two consecutive IDs
   // may indicate resolution function, or only "type_mark" followed by
   // actual parameter name.
   token_t third = peek_nth(3);
   if (peek() == tID && peek_nth(1) == tID &&
       (third == tSEMI || third == tCOMMA || third == tRPAREN))
      return p_type_mark();
   else
      return p_subtype_indication();
}

static type_t p_psl_param_spec(psl_node_t node, psl_type_t *psl_type, class_t *class)
{
   //    const
   //    | [const | mutable] Value_Parameter
   //    | sequence
   //    | property
   //
   //    Value_Parameter ::=
   //       HDL_Type
   //     | PSL_Type_Class
   //
   //    HDL_Type ::=
   //       hdltype HDL_VARIABLE_TYPE
   //
   //    PSL_Type_Class ::=
   //       boolean | bit | bitvector | numeric | string

   BEGIN("PSL Parameter specification");

   *class = C_SIGNAL;

   switch (peek()) {
   case tPROPERTY:
      consume(tPROPERTY);
      *psl_type = PSL_TYPE_PROPERTY;
      break;

   case tSEQUENCE:
      consume(tSEQUENCE);
      *psl_type = PSL_TYPE_SEQUENCE;
      break;

   case tCONST:
      *class = C_CONSTANT;
      // Handle PSL 1.1 "const" only
      if (peek_nth(2) == tID) {
         consume(tCONST);
         *psl_type = PSL_TYPE_NUMERIC;
         return std_type(NULL, STD_INTEGER);
      }
   case tMUTABLE:
      one_of(tCONST, tMUTABLE);
   case tHDLTYPE:
   case tBOOLEAN:
   case tBIT:
   case tBITVECTOR:
   case tNUMERIC:
   case tSTRINGK:
      switch (one_of(tHDLTYPE, tBOOLEAN, tBIT, tBITVECTOR, tNUMERIC, tSTRINGK)) {
      case tHDLTYPE:
         *psl_type = PSL_TYPE_HDLTYPE;
         scan_as_vhdl();
         type_t t = p_psl_subtype_indication();
         scan_as_psl();
         return t;
      case tBOOLEAN:
         *psl_type = PSL_TYPE_BOOLEAN;
         return std_type(NULL, STD_BOOLEAN);
      case tBIT:
         *psl_type = PSL_TYPE_BIT;
         return std_type(NULL, STD_BIT);
         break;
      case tBITVECTOR:
         *psl_type = PSL_TYPE_BITVECTOR;
         return std_type(NULL, STD_BIT_VECTOR);
         break;
      case tNUMERIC:
         *psl_type = PSL_TYPE_NUMERIC;
         return std_type(NULL, STD_INTEGER);
         break;
      case tSTRINGK:
         *psl_type = PSL_TYPE_STRING;
         return std_type(NULL, STD_STRING);
      }
   }

  return type_new(T_NONE);
}

static void p_psl_formal_parameter(psl_node_t node)
{
   // Param_Spec PSL_Identifier { , PSL_Identifier }

   BEGIN("PSL Formal parameter");

   psl_type_t psl_type = PSL_TYPE_NUMERIC;
   class_t class;
   type_t type = p_psl_param_spec(node, &psl_type, &class);

   do {
      tree_t p = tree_new(T_PARAM_DECL);
      tree_set_ident(p, p_identifier());
      tree_set_loc(p, CURRENT_LOC);
      tree_set_class(p, class);
      tree_set_subkind(p, psl_type);
      tree_set_type(p, type);
      psl_add_port(node, p);
      insert_name(nametab, p, NULL);
   } while (optional(tCOMMA));
}

static void p_psl_formal_parameter_list(psl_node_t node)
{
   // Formal_Parameter { ; Formal_Parameter }

   BEGIN("PSL Formal parameter list");

   p_psl_formal_parameter(node);

   while (optional(tSEMI))
      p_psl_formal_parameter(node);
}

static void p_psl_actual_parameter(psl_node_t node, bool seq)
{
   // Actual_Parameter ::=
   //    AnyType | Sequence | Property
   // sequence_Actual_Parameter ::=
   //    AnyType | Sequence

   BEGIN("PSL Actual parameter");

   // "Sequence" includes "Any_Type" parsing (HDL or PSL expression)
   // "Property" includes "Sequence" parsing
   psl_add_operand(node, (seq) ? p_psl_sequence() : p_psl_property());
}

static void p_psl_actual_parameter_list(psl_node_t node, bool seq)
{

   // sequence_Actual_Parameter { , sequence_Actual_Parameter }
   // Actual_Parameter { , Actual_Parameter }

   BEGIN("PSL Actual parameter list");

   p_psl_actual_parameter(node, seq);

   while (optional(tCOMMA))
      p_psl_actual_parameter(node, seq);
}

static psl_node_t p_psl_sequence(void)
{
   // Sequence_Instance | Repeated_SERE | Braced_SERE | Clocked_SERE
   //
   // Repeated_SERE :=
   //      Boolean [* [ Count ] ]
   //    | Sequence [* [ Count ] ]
   //    | [* [ Count ] ]
   //    | Boolean [+]
   //    | Sequence [+]
   //    | [+]
   //    | Boolean [= Count ]
   //    | Boolean [-> [ positive_Count ] ]
   //    | Boolean Proc_Block
   //    | Sequence Proc_Block
   //
   // Sequence_Instance ::=
   //    sequence_Name [ ( sequence_Actual_Parameter_List ) ]

   BEGIN("PSL Sequence");

   psl_node_t p;
   tree_t decl;
   ident_t name;

   switch (peek()) {
   case tLBRACE:
      p = p_psl_braced_or_clocked_sere();
      break;

   case tPLUSRPT:
   case tTIMESRPT:
      p = psl_new(P_SERE);
      break;

   default:
      name = peek_ident();

      // Check for Sequence_Instance
      if (name && (query_name(nametab, name, &decl) & N_PSL)) {
         tree_t t_psl = tree_ref(p_name(N_PSL));
         assert (tree_has_ident(t_psl));
         assert (tree_has_psl(t_psl));

         psl_node_t s_decl = tree_psl(t_psl);

         if (psl_kind(s_decl) != P_SEQUENCE_DECL)
            parse_error(CURRENT_LOC, "invalid PSL sequence instance: %s",
                                       istr(name));

         // Here we are surely parsing Sequence_Instance
         p = psl_new(P_SEQUENCE_INST);
         psl_set_ref(p, s_decl);

         if (optional(tLPAREN)) {
            p_psl_actual_parameter_list(p, true);
            consume(tRPAREN);
         }

         psl_check(p);
         break;
      }

      p = p_psl_or_hdl_expression();

      // [= and [-> are only allowed after boolean -> no need to recurse
      // or create new SERE
      if (scan(tGOTORPT, tARROWRPT)) {
         psl_node_t new = psl_new(P_SERE);
         psl_add_operand(new, p);
         p = new;
         psl_set_repeat(p, p_psl_repeat_scheme());
      }

      break;
   }

   // According to 6.1.2.3 of IEEE Std 1850-2010, multiple consecutive
   // repeatitions shall be treated as if braces were present. Recurse
   // and place the so-far parsed SERE as operand of new SERE. Similarly,
   // this is valid also for Proc_Block
   while (scan(tPLUSRPT, tTIMESRPT) ||
          (peek() == tLSQUARE && peek_nth(2) == tLSQUARE)) {

      if (psl_kind(p) == P_HDL_EXPR ||
          psl_has_repeat(p) ||
          (peek() == tLSQUARE && peek_nth(2) == tLSQUARE)) {
         psl_node_t new = psl_new(P_SERE);
         psl_add_operand(new, p);
         p = new;
      }

      if (scan(tPLUSRPT, tTIMESRPT))
         psl_set_repeat(p, p_psl_repeat_scheme());
      else
         psl_add_decl(p, p_psl_proc_block());
   }

   return p;
}

static psl_node_t p_psl_fl_property(void)
{
   // Boolean | ( FL_Property ) | FL_Property @ Clock_Expression
   //   | always FL_Property
   //   | never FL_Property
   //   | eventually! FL_Property
   //   | next FL_Property | next [ Number ] FL_Property
   //   | next! FL_Property | next! [ Number ] FL_Property
   //   | next_event ( Boolean ) ( FL_Property )
   //   | next_event ( Boolean ) ( FL_Property )
   //   | next_event! ( Boolean ) [ Number ] ( FL_Property )
   //   | next_event! ( Boolean ) [ Number ] ( FL_Property )
   //   | FL_Property -> FL_Property
   //   | FL_Property <-> FL_Property
   //   | Sequence [ ! ]
   //   | FL_Property until! FL_Property
   //   | FL_Property until!_ FL_Property
   //   | FL_Property until FL_Property
   //   | FL_Property until_ FL_Property

   BEGIN("FL property");

   psl_node_t p = NULL;
   const token_t tok = peek();
   switch (tok) {
   case tALWAYS:
      {
         consume(tALWAYS);

         p = psl_new(P_ALWAYS);
         psl_set_value(p, p_psl_fl_property());
      }
      break;

   case tNEVER:
      {
         consume(tNEVER);

         p = psl_new(P_NEVER);
         psl_set_value(p, p_psl_fl_property());
      }
      break;

   case tEVENTUALLY:
      {
         consume(tEVENTUALLY);

         p = psl_new(P_EVENTUALLY);
         psl_set_value(p, p_psl_fl_property());
      }
      break;

   case tNEXT:
   case tNEXT1:
      {
         consume(tok);

         p = psl_new(P_NEXT);

         if (tok == tNEXT1)
            psl_set_flag(p, PSL_F_STRONG);

         if (optional(tLSQUARE)) {
            scan_as_vhdl();

            type_t std_int = std_type(NULL, STD_INTEGER);

            tree_t expr = p_expression();
            solve_types(nametab, expr, std_int);
            sem_check(expr, nametab);

            psl_set_delay(p, expr);

            scan_as_psl();
            consume(tRSQUARE);
         }

         psl_set_value(p, p_psl_fl_property());
      }
      break;

   case tNEXTA:
   case tNEXTA1:
      {
         consume(tok);

         p = psl_new(P_NEXT_A);

         if (tok == tNEXTA1)
            psl_set_flag(p, PSL_F_STRONG);

         consume(tLSQUARE);

         scan_as_vhdl();
         psl_set_delay(p, p_discrete_range(NULL));
         scan_as_psl();

         consume(tRSQUARE);

         psl_set_value(p, p_psl_fl_property());
      }
      break;

   case tNEXTE:
   case tNEXTE1:
      {
         consume(tok);

         p = psl_new(P_NEXT_E);

         if (tok == tNEXTE1)
            psl_set_flag(p, PSL_F_STRONG);

         consume(tLSQUARE);

         scan_as_vhdl();
         psl_set_delay(p, p_discrete_range(NULL));
         scan_as_psl();

         consume(tRSQUARE);

         (void)p_psl_fl_property();
      }
      break;

   case tNEXTEVENT:
   case tNEXTEVENT1:
      {
         consume(tok);

         p = psl_new(P_NEXT_EVENT);

         if (tok == tNEXTEVENT1)
            psl_set_flag(p, PSL_F_STRONG);

         consume(tLPAREN);
         (void)p_psl_fl_property();
         consume(tRPAREN);

         if (optional(tLSQUARE)) {
            scan_as_vhdl();

            type_t std_int = std_type(NULL, STD_INTEGER);

            tree_t expr = p_expression();
            solve_types(nametab, expr, std_int);
            sem_check(expr, nametab);

            psl_set_delay(p, expr);
            scan_as_psl();

            consume(tRSQUARE);
         }

         consume(tLPAREN);
         psl_set_value(p, p_psl_fl_property());
         consume(tRPAREN);
      }
      break;

   case tLPAREN:
      {
         const look_params_t lookp = {
            .lookfn   = is_psl_infix_op,
            .stop     = { tRPAREN },
            .abort    = tSEMI,
            .nest_in  = tLPAREN,
            .nest_out = tRPAREN,
            .depth    = 1
         };

         scan_as_vhdl();
         const bool is_psl = look_for(&lookp);
         scan_as_psl();

         if (is_psl) {
            consume(tok);
            p = p_psl_fl_property();
            consume(tRPAREN);
         }
         else
            p = p_psl_or_hdl_expression();
      }
      break;

   default:
      p = p_psl_sequence();
      break;
   }

   psl_set_loc(p, CURRENT_LOC);

   const token_t infix = peek();
   switch (infix) {
   case tIFIMPL:
      {
         consume(tIFIMPL);

         psl_node_t impl = psl_new(P_IMPLICATION);
         psl_set_subkind(impl, PSL_IMPL_IF);
         psl_add_operand(impl, p);
         psl_add_operand(impl, p_psl_fl_property());
         psl_set_loc(impl, CURRENT_LOC);

         return impl;
      }

   case tUNTIL:
   case tUNTIL_:
   case tUNTIL1:
   case tUNTIL_1:
      {
         consume(infix);

         psl_flags_t flags = 0;
         if (infix == tUNTIL1 || infix == tUNTIL_1)
            flags |= PSL_F_STRONG;
         if (infix == tUNTIL_ || infix == tUNTIL_1)
            flags |= PSL_F_INCLUSIVE;

         psl_node_t until = psl_new(P_UNTIL);
         psl_set_flag(until, flags);
         psl_add_operand(until, p);
         psl_add_operand(until, p_psl_fl_property());
         psl_set_loc(until, CURRENT_LOC);

         return until;
      }

   default:
      return p;
   }
}

static void p_psl_report(psl_node_t p)
{
   consume(tREPORT);

   tree_t m = p_expression();
   psl_set_message(p, m);
   solve_types(nametab, m, std_type(NULL, STD_STRING));
}

static psl_node_t p_psl_property(void)
{
   // Replicator Property | FL_Property | OBE_Property

   BEGIN("property");

   return p_psl_fl_property();
}

static psl_node_t p_psl_assert_directive(void)
{
   // assert Property [ report String ] ;

   BEGIN("assert directive");

   consume(tASSERT);

   psl_node_t p = p_psl_property();
   if (!psl_has_clock(p))
      psl_set_clock(p, find_default_clock(nametab));

   psl_node_t a = psl_new(P_ASSERT);
   psl_set_value(a, p);

   if (peek() == tREPORT)
      p_psl_report(a);

   consume(tSEMI);

   psl_set_loc(a, CURRENT_LOC);
   return a;
}

static psl_node_t p_psl_assume_directive(void)
{
   // assume Property ;
   // assume_guarantee Property [ report String ] ;

   BEGIN("assume directive");

   token_t tok = one_of(tASSUME, tASSUMEG);

   psl_node_t a = psl_new(P_ASSUME);
   if (tok == tASSUME)
      psl_set_subkind(a, PSL_NO_GUARANTEE);
   else
      psl_set_subkind(a, PSL_GUARANTEE);

   psl_node_t p = p_psl_property();
   if (!psl_has_clock(p))
      psl_set_clock(p, find_default_clock(nametab));
   psl_set_value(a, p);

   if (peek() == tREPORT && tok == tASSUMEG)
      p_psl_report(a);

   consume(tSEMI);

   psl_set_loc(a, CURRENT_LOC);
   return a;
}

static psl_node_t p_psl_restrict_directive(void)
{
   // restrict Sequence  ;
   // restrict_guarantee Sequence [ report String ] ;

   BEGIN("restrict directive");

   token_t tok = peek();
   assert(tok == tRESTRICT || tok == tRESTRICTG);
   consume(tok);

   psl_node_t a = psl_new(P_RESTRICT);
   if (tok == tRESTRICT)
      psl_set_subkind(a, PSL_NO_GUARANTEE);
   else
      psl_set_subkind(a, PSL_GUARANTEE);

   psl_node_t p = p_psl_sequence();
   if (!psl_has_clock(p))
      psl_set_clock(p, find_default_clock(nametab));
   psl_set_value(a, p);

   if (peek() == tREPORT && tok == tRESTRICTG)
      p_psl_report(a);

   consume(tSEMI);

   psl_set_loc(a, CURRENT_LOC);
   return a;
}

static psl_node_t p_psl_fairness(void)
{
   // fairness Boolean ;
   // strong fairness Boolean , Boolean ;

   BEGIN("fairness statement");

   psl_flags_t flags = 0;
   if (optional(tSTRONG))
      flags |= PSL_F_STRONG;

   consume(tFAIRNESS);

   psl_node_t a = psl_new(P_FAIRNESS);
   psl_set_flag(a, flags);

   tree_t e1 = p_expression();
   solve_psl_condition(nametab, e1);

   psl_node_t p1 = psl_new(P_HDL_EXPR);
   psl_set_tree(p1, e1);

   psl_add_operand(a, p1);

   if (flags & PSL_F_STRONG) {
      consume(tCOMMA);

      tree_t e2 = p_expression();
      solve_psl_condition(nametab, e2);

      psl_node_t p2 = psl_new(P_HDL_EXPR);
      psl_set_tree(p2, e2);

      psl_add_operand(a, p2);
   }

   consume(tSEMI);

   psl_set_loc(a, CURRENT_LOC);
   return a;
}

static psl_node_t p_psl_cover_directive(void)
{
   // cover Sequence [ report String ] ;

   BEGIN("cover directive");

   consume(tCOVER);

   psl_node_t p = p_psl_sequence();
   if (!psl_has_clock(p))
      psl_set_clock(p, find_default_clock(nametab));

   psl_node_t a = psl_new(P_COVER);
   psl_set_value(a, p);

   if (peek() == tREPORT)
      p_psl_report(a);

   consume(tSEMI);

   psl_set_loc(a, CURRENT_LOC);
   return a;
}

static psl_node_t p_psl_verification_directive(void)
{
   // Assert_Directive | Assume_Directive | Restrict_Directive
   //   | Restrict!_Directive | Cover_Directive | Fairness_Statement

   BEGIN("verification directive");

   switch (peek()) {
   case tASSERT:
      return p_psl_assert_directive();
   case tASSUME:
   case tASSUMEG:
      return p_psl_assume_directive();
   case tRESTRICT:
   case tRESTRICTG:
      return p_psl_restrict_directive();
   case tFAIRNESS:
   case tSTRONG:
      return p_psl_fairness();
   case tCOVER:
      return p_psl_cover_directive();
   default:
      one_of(tASSERT, tASSUME, tCOVER);
      return NULL;
   }
}

static tree_t p_psl_directive(void)
{
   // [ Label : ] Verification_Directive

   BEGIN("PSL directive");

   tree_t t = tree_new(T_PSL);

   scan_as_psl();

   ident_t label = NULL;
   if (peek() == tID) {
      label = p_identifier();
      consume(tCOLON);
   }

   // Verification directive can contain Proc_Block with
   // local declarations -> Push scope
   push_scope(nametab);

   psl_node_t p = p_psl_verification_directive();
   tree_set_psl(t, p);

   pop_scope(nametab);

   scan_as_vhdl();

   tree_set_loc(t, CURRENT_LOC);
   ensure_labelled(t, label);

   if (label)
     insert_name(nametab, t, NULL);

   psl_check(p);
   return t;
}

static psl_node_t p_psl_property_declaration(tree_t t)
{
   // property PSL_Identifier [ ( Formal_Parameter_List ) ] is Property ;

   BEGIN("PSL property declaration");

   scan_as_psl();

   psl_node_t decl = psl_new(P_PROPERTY_DECL);
   consume(tPROPERTY);

   ident_t ident = p_identifier();
   psl_set_ident(decl, ident);
   tree_set_ident(t, ident);
   insert_name(nametab, t, NULL);

   push_scope(nametab);
   scope_set_container(nametab, t);

   if (optional(tLPAREN)) {
      p_psl_formal_parameter_list(decl);
      consume(tRPAREN);
   }

   consume(tIS);

   psl_node_t prop = p_psl_property();
   psl_set_value(decl, prop);

   consume(tSEMI);

   psl_set_loc(decl, CURRENT_LOC);
   psl_check(decl);

   pop_scope(nametab);

   scan_as_vhdl();

   return decl;
}

static psl_node_t p_psl_sequence_declaration(tree_t t)
{
   // sequence PSL_Identifier [ ( Formal_Parameter_List ) ] is Sequence ;

   BEGIN("PSL sequence declaration");

   scan_as_psl();

   psl_node_t decl = psl_new(P_SEQUENCE_DECL);
   consume(tSEQUENCE);

   ident_t ident = p_identifier();
   psl_set_ident(decl, ident);
   tree_set_ident(t, ident);
   insert_name(nametab, t, NULL);

   push_scope(nametab);
   scope_set_container(nametab, t);

   if (optional(tLPAREN)) {
      p_psl_formal_parameter_list(decl);
      consume(tRPAREN);
   }

   consume(tIS);

   psl_node_t prop = p_psl_sequence();
   psl_set_value(decl, prop);

   consume(tSEMI);

   psl_set_loc(decl, CURRENT_LOC);
   psl_check(decl);

   pop_scope(nametab);

   scan_as_vhdl();

   return decl;
}

static tree_t p_psl_declaration(void)
{
   //      Property_Declaration
   //    | Sequence_Declaration
   //    | Clock_Declaration

   BEGIN("PSL declaration");

   token_t tok = peek();
   tree_t t = tree_new(T_PSL);
   psl_node_t p;

   insert_names_for_psl(nametab);

   switch (tok) {
   case tPROPERTY:
      p = p_psl_property_declaration(t);
      break;
   case tSEQUENCE:
      p = p_psl_sequence_declaration(t);
      break;
   case tDEFAULT:
      p = p_psl_clock_declaration(t);
      insert_name(nametab, t, NULL);
      break;
   default:
      one_of(tPROPERTY, tSEQUENCE, tDEFAULT);
      return NULL;
   }

   tree_set_psl(t, p);
   tree_set_loc(t, CURRENT_LOC);

   return t;
}

static tree_t p_psl_or_concurrent_assert(ident_t label)
{
   // Handle the ambiguity between a PSL assertion and a normal VHDL
   // concurrent assertion statement.
   //
   //  assert condition [ report expression ] [ severity expression ] ;
   //   | assert Property [ report String ] ;

   scan_as_psl();

   const look_params_t lookp = {
      .look     = { tIFIMPL, tALWAYS, tNEVER },
      .stop     = { tREPORT, tSEVERITY },
      .abort    = tSEMI,
      .nest_in  = tLPAREN,
      .nest_out = tRPAREN,
   };

   if (look_for(&lookp)) {
      psl_node_t a = p_psl_assert_directive();

      tree_t s = tree_new(T_PSL);
      tree_set_psl(s, a);
      tree_set_ident(s, label);

      scan_as_vhdl();

      tree_set_loc(s, CURRENT_LOC);
      ensure_labelled(s, label);

      insert_name(nametab, s, NULL);
      sem_check(s, nametab);
      return s;
   }
   else {
      scan_as_vhdl();
      return p_concurrent_assertion_statement(label);
   }
}

static tree_t p_concurrent_statement(void)
{
   // block_statement | process_statement | concurrent_procedure_call_statement
   //   | concurrent_assertion_statement
   //   | concurrent_signal_assignment_statement
   //   | component_instantiation_statement | generate_statement
   //   | 2008: psl_directive

   BEGIN("concurrent statement");

   ident_t label = NULL;
   if ((peek() == tID) && (peek_nth(2) == tCOLON)) {
      label = p_identifier();
      consume(tCOLON);
   }

   if (peek() == tID || (peek() == tPOSTPONED && peek_nth(2) == tID)) {
      const token_t p2 = peek_nth(2);
      if ((label != NULL && p2 == tSEMI) || p2 == tGENERIC || p2 == tPORT)
         return p_component_instantiation_statement(label, NULL);
      else {
         const bool postponed = optional(tPOSTPONED);
         tree_t name = p_name(N_SUBPROGRAM), conc;
         if (peek() == tLE)
            conc = p_concurrent_signal_assignment_statement(label, name);
         else if (scan(tGENERIC, tPORT))
            return p_component_instantiation_statement(label, name);
         else {
            switch (tree_kind(name)) {
            case T_REF:
               if (tree_has_ref(name)) {
                  tree_t decl = tree_ref(name);
                  if (tree_kind(decl) == T_COMPONENT)
                     return p_component_instantiation_statement(label, name);
               }
               // Fall-through
            case T_PROT_REF:
               conc = p_concurrent_procedure_call_statement(label, name);
               break;
            default:
               parse_error(CURRENT_LOC, "expected concurrent statement");
               drop_tokens_until(tSEMI);
               conc = tree_new(T_CONCURRENT);
            }
         }

         if (postponed)
            tree_set_flag(conc, TREE_F_POSTPONED);
         return conc;
      }
   }
   else {
      switch (peek()) {
      case tPROCESS:
         return p_process_statement(label);

      case tCOMPONENT:
      case tENTITY:
      case tCONFIGURATION:
         return p_component_instantiation_statement(label, NULL);

      case tWITH:
         return p_concurrent_signal_assignment_statement(label, NULL);

      case tASSERT:
         if (standard() >= STD_08)
            return p_psl_or_concurrent_assert(label);
         else
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
      case tCASE:
         return p_generate_statement(label);

      case tLPAREN:
         return p_concurrent_signal_assignment_statement(label, NULL);

      case tSTARTPSL:
         consume(tSTARTPSL);

         if (scan(tDEFAULT, tSEQUENCE, tPROPERTY))
            return p_psl_declaration();
         else
            return p_psl_directive();

      default:
         expect(tPROCESS, tPOSTPONED, tCOMPONENT, tENTITY, tCONFIGURATION,
                tWITH, tASSERT, tBLOCK, tIF, tFOR, tCASE);
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

   tree_t e = NULL;
   ident_t entity_name = p_entity_name(&e);
   tree_set_ident2(unit, entity_name);

   consume(tIS);

   if (e != NULL) {
      tree_set_primary(unit, e);
      insert_names_from_context(nametab, e);
   }

   push_scope(nametab);

   if (entity_name != arch_name && e != NULL)
      insert_name(nametab, e, entity_name);

   ident_t ename = ident_prefix(lib_name(lib_work()), entity_name, '.');
   ident_t qual = ident_prefix(ename, arch_name, '-');
   scope_set_prefix(nametab, qual);

   tree_set_loc(unit, CURRENT_LOC);
   insert_name(nametab, unit, NULL);

   push_scope(nametab);

   if (e != NULL) {
      insert_generics(nametab, e);
      insert_ports(nametab, e);
      insert_decls(nametab, e);
   }

   continue_proc_labelling_from(e, nametab);

   p_architecture_declarative_part(unit);

   consume(tBEGIN);

   p_architecture_statement_part(unit);

   consume(tEND);
   optional(tARCHITECTURE);
   p_trailing_label(arch_name);
   consume(tSEMI);

   tree_set_loc(unit, CURRENT_LOC);

   sem_check(unit, nametab);

   // Set the architecture name to the fully qualified identifier
   tree_set_ident(unit, qual);

   pop_scope(nametab);
   pop_scope(nametab);
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
   //         | package_declaration

   BEGIN("package body declarative item");

   switch (peek()) {
   case tFUNCTION:
   case tPROCEDURE:
   case tIMPURE:
   case tPURE:
      if (peek_nth(3) == tIS && peek_nth(4) == tNEW)
         tree_add_decl(parent, p_subprogram_instantiation_declaration());
      else {
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
      if (peek_nth(3) == tOF)
         p_attribute_specification(parent, tree_add_decl);
      else
         tree_add_decl(parent, p_attribute_declaration());

      if (standard() < STD_08)
         parse_error(tree_loc(tree_decl(parent, tree_decls(parent) - 1)),
                     "package body may not contain attribute declarations or"
                     " specifications in VHDL-%s", standard_text(standard()));
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
      p_alias_declaration(parent);
      break;

   case tUSE:
      p_use_clause(parent, tree_add_decl);
      break;

   case tGROUP:
      if (peek_nth(3) == tIS)
         tree_add_decl(parent, p_group_template_declaration());
      else
         tree_add_decl(parent, p_group_declaration());
      break;

   case tPACKAGE:
      if (peek_nth(4) == tNEW)
         tree_add_decl(parent, p_package_instantiation_declaration(NULL));
      else if (peek_nth(2) == tBODY) {
         require_std(STD_08, "nested package declarations");
         tree_add_decl(parent, p_package_body(NULL));
      }
      else {
         require_std(STD_08, "nested package declarations");
         tree_add_decl(parent, p_package_declaration(NULL));
      }
      break;

   default:
      expect(tFUNCTION, tPROCEDURE, tSHARED, tIMPURE, tPURE, tATTRIBUTE, tTYPE,
             tCONSTANT, tSUBTYPE, tFILE, tALIAS, tUSE, tGROUP,
             STD(08, tPACKAGE));
   }
}

static void p_package_body_declarative_part(tree_t unit)
{
   // { package_body_declarative_item }

   BEGIN("package body declarative part");

   while (not_at_token(tEND))
      p_package_body_declarative_item(unit);
}

static tree_t p_package_body(tree_t unit)
{
   // package body simple_name is package_body_declarative_part
   //   end [ package body ] [ simple_name ] ;

   BEGIN("package body");

   consume(tPACKAGE);
   consume(tBODY);

   ident_t name = p_identifier(), qual = name;

   tree_t body;
   if (unit != NULL) {
      // Package body as primary unit
      assert(tree_kind(unit) == T_DESIGN_UNIT);
      tree_change_kind(unit, T_PACK_BODY);
      body = unit;

      qual = ident_prefix(lib_name(lib_work()), name, '.');
   }
   else
      body = tree_new(T_PACK_BODY);

   tree_t pack = resolve_name(nametab, CURRENT_LOC, qual);
   if (pack != NULL && tree_kind(pack) != T_PACKAGE) {
      parse_error(CURRENT_LOC, "unit %s is not a package", istr(qual));
      pack = NULL;
   }
   else if (pack != NULL) {
      tree_set_primary(body, pack);
      insert_names_from_context(nametab, pack);
   }

   push_scope(nametab);

   tree_set_ident(body, ident_prefix(qual, ident_new("body"), '-'));
   tree_set_loc(body, CURRENT_LOC);

   scope_set_prefix(nametab, qual);
   insert_name(nametab, body, name);

   consume(tIS);

   push_scope(nametab);

   if (pack != NULL) {
      insert_generics(nametab, pack);
      insert_decls(nametab, pack);
   }

   p_package_body_declarative_part(body);

   pop_scope(nametab);
   pop_scope(nametab);

   consume(tEND);

   if (optional(tPACKAGE))
      consume(tBODY);

   p_trailing_label(name);
   consume(tSEMI);

   tree_set_loc(body, CURRENT_LOC);
   sem_check(body, nametab);

   return body;
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
      expect(tENTITY, tCONFIGURATION, tARCHITECTURE, tPACKAGE,
             STD(08, tCONTEXT));
   }

   if (bootstrapping && unit != find_std(nametab))
      parse_error(tree_loc(unit), "--bootstrap must only be used with "
                  "STANDARD package");
}

static tree_t p_design_unit(void)
{
   BEGIN("design unit");

   push_scope(nametab);

   tree_t unit = tree_new(T_DESIGN_UNIT);
   scope_set_container(nametab, unit);

   ident_t std_i = well_known(W_STD);

   tree_t std = tree_new(T_LIBRARY);
   tree_set_ident(std, std_i);
   tree_set_ident2(std, std_i);
   tree_add_context(unit, std);
   insert_name(nametab, std, std_i);

   ident_t work_name = lib_name(lib_work());
   tree_t work = tree_new(T_LIBRARY);
   tree_set_ident(work, work_name);
   tree_set_ident2(work, work_name);
   tree_add_context(unit, work);
   insert_name(nametab, work, well_known(W_WORK));
   insert_name(nametab, work, NULL);

   // The std.standard package is implicit unless we are bootstrapping
   if (!bootstrapping) {
      lib_t lstd = lib_require(std_i);
      ident_t standard_i = well_known(W_STD_STANDARD);
      tree_t std_pkg = lib_get(lstd, standard_i);
      if (std_pkg == NULL)
         fatal("cannot find %s package", istr(standard_i));

      tree_t u = tree_new(T_USE);
      tree_set_ident(u, standard_i);
      tree_set_ident2(u, well_known(W_ALL));
      tree_set_ref(u, std_pkg);

      tree_add_context(unit, u);
      insert_names_from_use(nametab, u);
   }

   p_context_clause(unit);
   p_library_unit(unit);

   pop_scope(nametab);

   return unit;
}

static void flush_pragmas(tree_t unit)
{
   // Make sure pragmas always appear in off/on pairs
   tree_t coverage_off = NULL, synthesis_off = NULL, translate_off = NULL;

   for (int i = 0; i < pragmas.count; i++) {
      tree_t p = pragmas.items[i];
      tree_add_pragma(unit, p);

      switch (tree_subkind(p)) {
      case PRAGMA_SYNTHESIS_ON: synthesis_off = NULL; break;
      case PRAGMA_SYNTHESIS_OFF: synthesis_off = p; break;
      case PRAGMA_COVERAGE_ON: coverage_off = NULL; break;
      case PRAGMA_COVERAGE_OFF: coverage_off = p; break;
      case PRAGMA_TRANSLATE_ON: translate_off = NULL; break;
      case PRAGMA_TRANSLATE_OFF: translate_off = p; break;
      }
   }

   if (coverage_off != NULL)
      warn_at(tree_loc(coverage_off), "no matching $bold$coverage on$$ "
              "directive seen before end of design unit");

   if (synthesis_off != NULL)
      warn_at(tree_loc(synthesis_off), "no matching $bold$synthesis "
              "translate_on$$ directive seen before end of design unit");

   if (translate_off != NULL)
      warn_at(tree_loc(translate_off), "no matching $bold$pragma "
              "translate_on$$ directive seen before end of design unit");

   ACLEAR(pragmas);
}

tree_t parse(void)
{
   n_correct = RECOVER_THRESH;

   if (peek() == tEOF)
      return NULL;

   make_new_arena();
   nametab = nametab_new();

   tree_t unit = p_design_unit();

   nametab_finish(nametab);
   nametab = NULL;

   flush_pragmas(unit);

   if (tree_kind(unit) == T_DESIGN_UNIT)
      return NULL;

   return unit;
}

void reset_vhdl_parser(void)
{
   bootstrapping = opt_get_int(OPT_BOOTSTRAP);

   if (tokenq == NULL) {
      tokenq_sz = 128;
      tokenq = xmalloc_array(tokenq_sz, sizeof(tokenq_t));
   }

   tokenq_head = tokenq_tail = 0;
}
