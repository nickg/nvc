//
//  Copyright (C) 2024-2025  Nick Gasson
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
#include "diag.h"
#include "ident.h"
#include "lib.h"
#include "object.h"
#include "scan.h"
#include "vlog/vlog-node.h"
#include "vlog/vlog-phase.h"
#include "vlog/vlog-symtab.h"
#include "vlog/vlog-util.h"

#include <ctype.h>
#include <string.h>
#include <stdlib.h>
#include <assert.h>

#define RECOVER_THRESH 5
#define TRACE_PARSE    0
#define TRACE_RECOVERY 0
#define TOKENQ_SIZE    8

typedef struct {
   token_t  token;
   yylval_t lval;
   loc_t    loc;
} tokenq_t;

typedef struct {
   loc_t       start_loc;
   loc_t       last_loc;
   const char *hint_str;
   int         n_correct;
   tokenq_t    tokenq[TOKENQ_SIZE];
   int         tokenq_head;
   int         tokenq_tail;
   yylval_t    last_lval;
   token_t     opt_hist[8];
   int         nopt_hist;
#if TRACE_PARSE
   int         depth;
#endif
} parse_state_t;

typedef struct {
   const char *old_hint;
   loc_t       old_start_loc;
} rule_state_t;

static parse_state_t    state;
static vlog_kind_t      param_kind;
static vlog_net_kind_t  implicit_kind;
static vlog_symtab_t   *symtab;

extern loc_t yylloc;

#define scan(...) _scan(1, __VA_ARGS__, -1)
#define expect(...) _expect(1, __VA_ARGS__, -1)
#define one_of(...) _one_of(1, __VA_ARGS__, -1)
#define not_at_token(...) ((peek() != tEOF) && !_scan(1, __VA_ARGS__, -1))
#define peek() peek_nth(1)

#define parse_error(loc, ...) do {              \
      if (state.n_correct >= RECOVER_THRESH)    \
         error_at((loc), __VA_ARGS__);          \
   } while (0)

#if TRACE_PARSE
static void _push_state(const rule_state_t *s);
#else
#define _push_state(s)
#endif

#define EXTEND(s)                                                      \
   __attribute__((cleanup(_pop_state), unused))                        \
   const rule_state_t _state = { state.hint_str, state.start_loc };    \
   state.hint_str = s;                                                 \
   _push_state(&_state);

#define BEGIN_WITH_HEAD(s, t)                            \
   EXTEND(s);                                            \
   state.start_loc = (t) ? *vlog_loc(t) : LOC_INVALID;   \

#define BEGIN(s)  BEGIN_WITH_HEAD(s, NULL)

#define CURRENT_LOC _diff_loc(&state.start_loc, &state.last_loc)

static vlog_node_t p_statement_or_null(void);
static vlog_node_t p_expression(void);
static vlog_node_t p_constant_expression(void);
static vlog_node_t p_data_type(void);
static void p_list_of_variable_decl_assignments(vlog_node_t parent,
                                                vlog_node_t datatype);
static vlog_node_t p_variable_lvalue(void);
static vlog_node_t p_select(ident_t id);
static vlog_net_kind_t p_net_type(void);
static void p_module_or_generate_item(vlog_node_t mod);
static void p_block_item_declaration(vlog_node_t parent);

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
      const token_t token = processed_yylex();

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

static void drop_tokens_until(token_t tok)
{
   token_t next = tEOF;
   do {
      free_token(tok, &state.last_lval);
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

static vlog_node_t peek_reference(void)
{
   assert(peek() == tID);
   ident_t id = ident_new(state.tokenq[state.tokenq_tail].lval.str);
   return vlog_symtab_query(symtab, id);
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

static void set_timescale(const char *unit_value, const char *unit_name,
                          const char *prec_value, const char *prec_name,
                          const loc_t *loc)
{
   // See IEEE 1800-2017 section 22.7 for rules

   struct {
      const char *value;
      const char *name;
      int64_t     parsed;
   } args[] = {
      { unit_value, unit_name },
      { prec_value, prec_name },
   };

   for (int i = 0; i < ARRAY_LEN(args); i++) {
      static const struct {
         const char *name;
         int64_t value;
      } valid_units[] = {
         { "s", INT64_C(60000000000000) },
         { "ms", 1000000000000 },
         { "us", 1000000000 },
         { "ns", 1000000 },
         { "ps", 1000 },
         { "fs", 1 },
      };

      bool name_valid = false;
      for (int j = 0; j < ARRAY_LEN(valid_units); j++) {
         if (strcmp(valid_units[j].name, args[i].name) == 0) {
            args[i].parsed = valid_units[j].value;
            name_valid = true;
            break;
         }
      }

      if (!name_valid)
         error_at(loc, "invalid time unit name '%s'", args[i].name);

      const int scale = atoi(args[i].value);
      if (scale != 1 && scale != 10 && scale != 100) {
         diag_t *d = diag_new(DIAG_ERROR, loc);
         diag_printf(d, "invalid order of magnitude in `timescale directive");
         diag_hint(d, NULL, "the valid values are 1, 10, and 100");
         diag_emit(d);
      }

      args[i].parsed *= scale;
   }

   // TODO: do something with parsed scale/precision
}

static ident_t error_marker(void)
{
   return well_known(W_ERROR);
}

static vlog_node_t dummy_expression(void)
{
   vlog_node_t v = vlog_new(V_NUMBER);
   vlog_set_number(v, number_new("1'b0"));
   return v;
}

static vlog_node_t logic_type(void)
{
   vlog_node_t v = vlog_new(V_DATA_TYPE);
   vlog_set_subkind(v, DT_LOGIC);
   return v;
}

static ident_t p_identifier(void)
{
   if (consume(tID)) {
      ident_t id = ident_new(state.last_lval.str);
      free(state.last_lval.str);
      return id;
   }
   else
      return error_marker();
}

static void p_external_identifier(ident_t *id, ident_t *ext)
{
   if (consume(tID)) {
      *id = ident_new(state.last_lval.str);
      for (char *p = state.last_lval.str; *p; p++)
         *p = toupper_iso88591(*p);
      *ext = ident_new(state.last_lval.str);
      free(state.last_lval.str);
   }
   else
      *id = *ext = error_marker();
}

static ident_t p_system_tf_identifier(void)
{
   if (consume(tSYSTASK)) {
      ident_t id = ident_new(state.last_lval.str);
      free(state.last_lval.str);
      return id;
   }
   else
      return error_marker();
}

static void p_attr_spec(void)
{
   // attr_name [ = constant_expression ]

   BEGIN("attribute specification");

   (void)p_identifier();

   if (optional(tEQ))
      (void)p_constant_expression();
}

static void p_attribute_instance(void)
{
   // (* attr_spec { , attr_spec } *)

   BEGIN("attribute instance");

   consume(tATTRBEGIN);

   do {
      p_attr_spec();
   } while (optional(tCOMMA));

   consume(tATTREND);
}

static vlog_node_t p_unsigned_number(void)
{
   // decimal_digit { _ | decimal_digit }

   BEGIN("unsigned number");

   consume(tUNSIGNED);

   vlog_node_t v = vlog_new(V_NUMBER);
   vlog_set_loc(v, CURRENT_LOC);
   vlog_set_number(v, number_new(state.last_lval.str));
   free(state.last_lval.str);
   return v;
}

static vlog_node_t p_integral_number(void)
{
   // decimal_number | octal_number | binary_number | hex_number

   BEGIN("integral number");

   if (peek() == tUNSIGNED)
      return p_unsigned_number();
   else {
      consume(tNUMBER);

      vlog_node_t v = vlog_new(V_NUMBER);
      vlog_set_loc(v, CURRENT_LOC);
      vlog_set_number(v, number_new(state.last_lval.str));
      free(state.last_lval.str);
      return v;
   }
}

static vlog_node_t p_real_number(void)
{
   // fixed_point_number
   //   | unsigned_number [ . unsigned_number ] exp [ sign ] unsigned_number

   BEGIN("real number");

   consume(tREAL);

   vlog_node_t v = vlog_new(V_REAL);
   vlog_set_dval(v, state.last_lval.real);
   vlog_set_loc(v, CURRENT_LOC);
   return v;
}

static vlog_node_t p_number(void)
{
   // integral_number | real_number

   BEGIN("number");

   if (peek() == tREAL)
      return p_real_number();
   else
      return p_integral_number();
}

static vlog_node_t p_string_literal(void)
{
   // " { Any_ASCII_Characters } "

   BEGIN("string literal");

   consume(tSTRING);

   vlog_node_t v = vlog_new(V_STRING);
   vlog_set_loc(v, CURRENT_LOC);
   vlog_set_number(v, number_new(state.last_lval.str));

   return v;
}

static vlog_node_t p_primary_literal(void)
{
   // number | time_literal | unbased_unsized_literal | string_literal

   BEGIN("primary literal");

   switch (peek()) {
   case tNUMBER:
   case tUNSIGNED:
   case tREAL:
      return p_number();
   case tSTRING:
      return p_string_literal();
   default:
      one_of(tNUMBER, tUNSIGNED, tREAL, tSTRING);
      return dummy_expression();
   }
}

static vlog_node_t p_constant_select(ident_t id)
{
   // [ { . member_identifier constant_bit_select } . member_identifier ]
   //    constant_bit_select [ [ constant_part_select_range ] ]

   EXTEND("constant select");

   // Checked for constant-ness later
   return p_select(id);
}

static vlog_node_t p_constant_expression(void)
{
   // constant_primary | unary_operator { attribute_instance } constant_primary
   //   | constant_expression binary_operator { attribute_instance }
   //       constant_expression
   //   | constant_expression ? { attribute_instance }
   //       constant_expression : constant_expression

   BEGIN("constant expression");

   // Checked for constant-ness later
   return p_expression();
}

static void p_constant_range(vlog_node_t *left, vlog_node_t *right)
{
   // constant_expression : constant_expression

   BEGIN("constant range");

   *left = p_constant_expression();

   consume(tCOLON);

   *right = p_constant_expression();
}

static vlog_node_t p_constant_range_expression(void)
{
   // constant_expression | constant_part_select_range

   BEGIN("constant range expression");

   return p_constant_expression();
}

static vlog_node_t p_constant_mintypmax_expression(void)
{
   // constant_expression
   //   | constant_expression : constant_expression : constant_expression

   return p_constant_expression();
}

static vlog_node_t p_packed_dimension(void)
{
   // [ constant_range ] | unsized_dimension

   BEGIN("packed dimension");

   consume(tLSQUARE);

   vlog_node_t left, right;
   p_constant_range(&left, &right);

   consume(tRSQUARE);

   vlog_node_t v = vlog_new(V_DIMENSION);
   vlog_set_subkind(v, V_DIM_PACKED);
   vlog_set_left(v, left);
   vlog_set_right(v, right);
   vlog_set_loc(v, CURRENT_LOC);

   return v;
}

static vlog_node_t p_unpacked_dimension(void)
{
   // [ constant_range ] | [ constant_expression ]

   BEGIN("unpacked dimension");

   consume(tLSQUARE);

   vlog_node_t left, right;
   p_constant_range(&left, &right);

   consume(tRSQUARE);

   vlog_node_t v = vlog_new(V_DIMENSION);
   vlog_set_subkind(v, V_DIM_UNPACKED);
   vlog_set_left(v, left);
   vlog_set_right(v, right);
   vlog_set_loc(v, CURRENT_LOC);

   return v;
}

static vlog_node_t p_data_type_or_void(void)
{
   // data_type | void

   BEGIN("data type or void");

   if (optional(tVOID))
      return NULL;
   else
      return p_data_type();
}

static void p_struct_union_member(vlog_node_t v)
{
   // { attribute_instance } [ random_qualifier ] data_type_or_void
   //    list_of_variable_decl_assignments ;

   BEGIN("struct or union member");

   while (peek() == tATTRBEGIN)
      p_attribute_instance();

   vlog_node_t dt = p_data_type_or_void();
   p_list_of_variable_decl_assignments(v, dt);

   consume(tSEMI);
}

static void p_enum_name_declaration(vlog_node_t parent)
{
   // enum_identifier [ [ integral_number [ : integral_number ] ] ]
   //   [ = constant_expression ]

   BEGIN("enum name declaration");

   vlog_node_t v = vlog_new(V_ENUM_NAME);
   vlog_set_ident(v, p_identifier());
   vlog_set_type(v, parent);

   vlog_add_decl(parent, v);

   vlog_set_loc(v, CURRENT_LOC);
   vlog_symtab_put(symtab, v);
}

static vlog_node_t p_integer_atom_type(void)
{
   // byte | shortint | int | longint | integer | time

   BEGIN("integer atom type");

   data_type_t dt = DT_BYTE;
   switch (one_of(tBYTE, tSHORTINT, tSVINT, tLONGINT, tINTEGER, tTIME)) {
   case tBYTE:     dt = DT_BYTE; break;
   case tSHORTINT: dt = DT_SHORTINT; break;
   case tSVINT:    dt = DT_INT; break;
   case tLONGINT:  dt = DT_LONGINT; break;
   case tINTEGER:  dt = DT_INTEGER; break;
   case tTIME:     dt = DT_TIME; break;
   }

   vlog_node_t v = vlog_new(V_DATA_TYPE);
   vlog_set_subkind(v, dt);
   vlog_set_loc(v, &state.last_loc);
   return v;
}

static vlog_node_t p_integer_vector_type(void)
{
   //  bit | logic | reg

   BEGIN("integer vector type");

   data_type_t dt = DT_LOGIC;
   switch (one_of(tBIT, tLOGIC, tREG)) {
   case tBIT:    dt = DT_BIT; break;
   case tLOGIC:
   case tREG:    dt = DT_LOGIC; break;
   }

   vlog_node_t v = vlog_new(V_DATA_TYPE);
   vlog_set_subkind(v, dt);
   vlog_set_loc(v, &state.last_loc);
   return v;
}

static vlog_node_t p_non_integer_type(void)
{
   // shortreal | real | realtime

   BEGIN("non-integer type");

   data_type_t dt = DT_REAL;
   switch (one_of(tSVREAL, tSHORTREAL, tREALTIME)) {
   case tSVREAL:    dt = DT_REAL; break;
   case tSHORTREAL: dt = DT_SHORTREAL; break;
   case tREALTIME:  dt = DT_REALTIME; break;
   }

   vlog_node_t v = vlog_new(V_DATA_TYPE);
   vlog_set_subkind(v, dt);
   vlog_set_loc(v, &state.last_loc);
   return v;
}

static vlog_node_t p_struct_union(void)
{
   // struct | union [ tagged ]

   BEGIN("struct or union");

   switch (one_of(tSTRUCT, tUNION)) {
   case tUNION:
      {
         vlog_node_t v = vlog_new(V_UNION_DECL);
         optional(tTAGGED);
         return v;
      }
   case tSTRUCT:
   default:
      return vlog_new(V_STRUCT_DECL);
   }
}

static vlog_node_t p_data_type(void)
{
   // integer_vector_type [ signing ] { packed_dimension }
   //   | integer_atom_type [ signing ] | non_integer_type
   //   | struct_union [ packed [ signing ] ]
   //       { struct_union_member { struct_union_member } } { packed_dimension }
   //   | enum [ enum_base_type ] { enum_name_declaration
   //       { , enum_name_declaration } } { packed_dimension }
   //   | string | chandle
   //   | virtual [ interface ] interface_identifier
   //       [ parameter_value_assignment ] [ . modport_identifier ]
   //   | [ class_scope | package_scope ] type_identifier { packed_dimension }
   //   | class_type | event | ps_covergroup_identifier | type_reference

   BEGIN("data type");

   switch (peek()) {
   case tBIT:
   case tLOGIC:
   case tREG:
      {
         vlog_node_t v = p_integer_vector_type();

         while (peek() == tLSQUARE)
            vlog_add_range(v, p_packed_dimension());

         return v;
      }

   case tBYTE:
   case tSHORTINT:
   case tSVINT:
   case tLONGINT:
   case tINTEGER:
   case tTIME:
      return p_integer_atom_type();

   case tSVREAL:
   case tREALTIME:
   case tSHORTREAL:
      return p_non_integer_type();

   case tSTRUCT:
   case tUNION:
      {
         vlog_node_t v = p_struct_union();

         (void)optional(tPACKED);

         consume(tLBRACE);

         vlog_symtab_push(symtab, v);

         do {
            p_struct_union_member(v);
         } while (not_at_token(tRBRACE));

         vlog_symtab_pop(symtab);

         consume(tRBRACE);

         if (peek() == tLSQUARE)
            vlog_add_range(v, p_packed_dimension());

         vlog_set_loc(v, CURRENT_LOC);
         return v;
      }

   case tENUM:
      {
         consume(tENUM);

         vlog_node_t v = vlog_new(V_ENUM_DECL);

         consume(tLBRACE);

         do {
            p_enum_name_declaration(v);
         } while (optional(tCOMMA));

         consume(tRBRACE);

         while (peek() == tLSQUARE)
            vlog_add_range(v, p_packed_dimension());

         vlog_set_loc(v, CURRENT_LOC);
         return v;
      }

   case tEVENT:
      {
         consume(tEVENT);

         vlog_node_t v = vlog_new(V_DATA_TYPE);
         vlog_set_subkind(v, DT_EVENT);
         vlog_set_loc(v, &state.last_loc);

         return v;
      }

   case tID:
      {
         ident_t id = p_identifier();
         vlog_node_t dt = vlog_symtab_query(symtab, id);
         if (dt == NULL)
            should_not_reach_here();   // Guarded by peek_reference() call
         else if (!is_data_type(dt)) {
            diag_t *d = diag_new(DIAG_ERROR, &state.last_loc);
            diag_printf(d, "'%s' is not a data type", istr(id));
            diag_hint(d, vlog_loc(dt), "'%s' declared here", istr(id));
            diag_emit(d);

            return logic_type();
         }
         else
            return dt;
      }

   default:
      one_of(tBIT, tLOGIC, tREG, tBYTE, tSHORTINT, tSVINT, tLONGINT, tINTEGER,
             tTIME, tSVREAL, tREALTIME, tSHORTREAL, tSTRUCT, tUNION, tENUM,
             tEVENT, tID);
      return logic_type();
   }
}

static vlog_node_t p_implicit_data_type(void)
{
   // [ signing ] { packed_dimension }

   BEGIN("implicit data type");

   vlog_node_t v = vlog_new(V_DATA_TYPE);
   vlog_set_subkind(v, DT_LOGIC);

   while (peek() == tLSQUARE)
      vlog_add_range(v, p_packed_dimension());

   vlog_set_loc(v, CURRENT_LOC);
   return v;
}

static vlog_node_t p_data_type_or_implicit(void)
{
   // data_type | implicit_data_type

   BEGIN("data type or implicit");

   switch (peek()) {
   case tID:
      if (peek_nth(2) == tID)
         return p_data_type();
      else
         return p_implicit_data_type();
      break;
   case tREG:
   case tSTRUCT:
   case tUNION:
   case tENUM:
   case tSVINT:
   case tINTEGER:
   case tSVREAL:
   case tSHORTREAL:
   case tREALTIME:
   case tTIME:
   case tLOGIC:
   case tBIT:
   case tEVENT:
      return p_data_type();
   default:
      return p_implicit_data_type();
   }
}

static void p_net_port_type(vlog_net_kind_t *kind, vlog_node_t *dt)
{
   // [ net_type ] data_type_or_implicit | net_type_identifier
   //   | interconnect implicit_data_type

   BEGIN("net port type");

   if (scan(tSUPPLY0, tSUPPLY1, tTRI, tTRIAND, tTRIOR, tTRIREG,
            tTRI0, tTRI1, tUWIRE, tWIRE, tWAND, tWOR))
      *kind = p_net_type();
   else
      *kind = V_NET_WIRE;

   *dt = p_data_type_or_implicit();
}

static vlog_node_t p_var_data_type(void)
{
   // data_type | var data_type_or_implicit

   BEGIN("var data type");

   return p_data_type();
}

static vlog_node_t p_variable_port_type(void)
{
   // var_data_type

   BEGIN("variable port type");

   return p_var_data_type();
}

static void p_list_of_port_identifiers(vlog_node_t mod, v_port_kind_t kind,
                                       vlog_node_t datatype)
{
   // port_identifier { unpacked_dimension }
   //    { , port_identifier { unpacked_dimension } }

   BEGIN("list of port identifiers");

   do {
      ident_t id, ext;
      p_external_identifier(&id, &ext);

      vlog_node_t v = vlog_new(V_PORT_DECL);
      vlog_set_subkind(v, kind);
      vlog_set_ident(v, id);
      vlog_set_ident2(v, ext);
      vlog_set_type(v, datatype);
      vlog_set_loc(v, &state.last_loc);

      vlog_add_decl(mod, v);
      vlog_symtab_put(symtab, v);

      vlog_node_t ref = vlog_new(V_REF);
      vlog_set_loc(ref, CURRENT_LOC);
      vlog_set_ident(ref, id);
      vlog_set_ref(ref, v);

      vlog_add_port(mod, ref);
   } while (optional(tCOMMA));
}

static void p_list_of_variable_port_identifiers(vlog_node_t mod,
                                                v_port_kind_t kind,
                                                vlog_node_t datatype)
{
   // port_identifier { variable_dimension } [ = constant_expression ]
   //   { , port_identifier { variable_dimension } [ = constant_expression ] }

   BEGIN("list of variable port identifiers");

   do {
      ident_t id, ext;
      p_external_identifier(&id, &ext);

      vlog_node_t v = vlog_new(V_PORT_DECL);
      vlog_set_subkind(v, kind);
      vlog_set_ident(v, id);
      vlog_set_ident2(v, ext);
      vlog_set_type(v, datatype);
      vlog_set_loc(v, &state.last_loc);

      vlog_add_decl(mod, v);
      vlog_symtab_put(symtab, v);

      vlog_node_t reg = vlog_new(V_VAR_DECL);
      vlog_set_loc(reg, vlog_loc(v));
      vlog_set_ident(reg, id);
      vlog_set_type(reg, datatype);

      vlog_add_decl(mod, reg);
      vlog_symtab_put(symtab, reg);

      vlog_node_t ref = vlog_new(V_REF);
      vlog_set_loc(ref, CURRENT_LOC);
      vlog_set_ident(ref, id);
      vlog_set_ref(ref, v);

      vlog_add_port(mod, ref);
   } while (optional(tCOMMA));
}

static void p_inout_declaration(vlog_node_t mod)
{
   // inout net_port_type list_of_port_identifiers

   BEGIN("inout declaration");

   consume(tINOUT);

   vlog_node_t dt;
   vlog_net_kind_t kind;
   p_net_port_type(&kind, &dt);

   p_list_of_port_identifiers(mod, V_PORT_INOUT, dt);
}

static void p_input_declaration(vlog_node_t mod)
{
   // input net_port_type list_of_port_identifiers
   //   | input variable_port_type list_of_variable_identifiers

   BEGIN("input declaration");

   consume(tINPUT);

   vlog_node_t dt;
   vlog_net_kind_t kind;
   switch (peek()) {
   case tREG:
      dt = p_variable_port_type();
      p_list_of_variable_port_identifiers(mod, V_PORT_INPUT, dt);
      break;
   default:
      p_net_port_type(&kind, &dt);
      p_list_of_port_identifiers(mod, V_PORT_INPUT, dt);
      break;
   }
}

static void p_output_declaration(vlog_node_t mod)
{
   // output net_port_type list_of_port_identifiers
   //   | output variable_port_type list_of_variable_port_identifiers

   BEGIN("output declaration");

   consume(tOUTPUT);

   switch (peek()) {
   case tREG:
      {
         vlog_node_t dt = p_variable_port_type();
         p_list_of_variable_port_identifiers(mod, V_PORT_OUTPUT, dt);
      }
      break;
   default:
      {
         vlog_node_t dt;
         vlog_net_kind_t kind;
         p_net_port_type(&kind, &dt);
         p_list_of_port_identifiers(mod, V_PORT_OUTPUT, dt);
      }
      break;
   }
}

static void p_port_declaration(vlog_node_t mod)
{
   // { attribute_instance } inout_declaration
   //   | { attribute_instance } input_declaration
   //   | { attribute_instance } output_declaration
   //   | { attribute_instance } ref_declaration
   //   | { attribute_instance } interface_port_declaration

   BEGIN("port declaration");

   switch (peek()) {
   case tINOUT: p_inout_declaration(mod); break;
   case tINPUT: p_input_declaration(mod); break;
   case tOUTPUT: p_output_declaration(mod); break;
   default: should_not_reach_here();
   }
}

static vlog_node_t p_net_port_header(v_port_kind_t *dir, bool *isreg)
{
   // [ port_direction ] net_port_type

   BEGIN("net port header");

   if (optional(tINPUT)) {
      *dir = V_PORT_INPUT;
      *isreg = (peek() == tREG);
   }
   else if (optional(tINOUT)) {
      *dir = V_PORT_INOUT;
      *isreg = false;
   }
   else if (optional(tOUTPUT)) {
      *dir = V_PORT_OUTPUT;
      *isreg = (peek() == tREG);
   }

   vlog_net_kind_t kind;
   vlog_node_t dt;
   p_net_port_type(&kind, &dt);
   return dt;
}

static void p_part_select_range(vlog_node_t ps)
{
   // constant_expression : constant_expression
   //   | expression +: constant_expression
   //   | expression -: constant_expression

   BEGIN("part select range");

   vlog_range_kind_t kind = V_RANGE_CONST;
   switch (one_of(tCOLON, tINDEXPOS, tINDEXNEG)) {
   case tINDEXPOS: kind = V_RANGE_POS; break;
   case tINDEXNEG: kind = V_RANGE_NEG; break;
   }

   vlog_set_subkind(ps, kind);
   vlog_set_right(ps, p_constant_expression());
}

static vlog_node_t p_select(ident_t id)
{
   // [ { . member_identifier bit_select } . member_identifier ]
   //    { [ expression ] } [ [ part_select_range ] ]

   EXTEND("select");

    if (optional(tLSQUARE)) {
      vlog_node_t bs = NULL;
      do {
         vlog_node_t expr = p_expression();
         if (scan(tCOLON, tINDEXPOS, tINDEXNEG)) {
            vlog_node_t ps = vlog_new(V_PART_SELECT);
            vlog_set_left(ps, expr);

            p_part_select_range(ps);

            if (bs == NULL) {
               vlog_node_t ref = vlog_new(V_REF);
               vlog_set_ident(ref, id);
               vlog_set_loc(ref, CURRENT_LOC);

               vlog_symtab_lookup(symtab, ref);

               vlog_set_value(ps, ref);
            }
            else
               vlog_set_value(ps, bs);

            consume(tRSQUARE);

            vlog_set_loc(ps, CURRENT_LOC);
            return ps;
         }

         if (bs == NULL) {
            bs = vlog_new(V_BIT_SELECT);
            vlog_set_ident(bs, id);
            vlog_set_loc(bs, CURRENT_LOC);

            vlog_symtab_lookup(symtab, bs);
         }

         vlog_add_param(bs, expr);

         consume(tRSQUARE);
      } while (optional(tLSQUARE));

      assert(bs != NULL);
      return bs;
   }
   else {
      vlog_node_t v = vlog_new(V_REF);
      vlog_set_ident(v, id);
      vlog_set_loc(v, CURRENT_LOC);

      vlog_symtab_lookup(symtab, v);
      return v;
   }
}

static void p_list_of_arguments(vlog_node_t call)
{
   // [ expression ] { , [ expression ] } { , . identifier ( [ expression ] ) }
   //    | . identifier ( [ expression ] ) { , . identifier ( [ expression ] ) }

   BEGIN("list of arguments");

   do {
      if (peek() == tCOMMA) {
         vlog_node_t v = vlog_new(V_EMPTY);
         vlog_set_loc(v, &state.last_loc);
         vlog_add_param(call, v);
      }
      else
         vlog_add_param(call, p_expression());
   } while (optional(tCOMMA));
}

static vlog_node_t p_system_tf_call(vlog_kind_t kind)
{
   // system_tf_identifier [ ( list_of_arguments ) ]

   BEGIN("system task or function call");

   vlog_node_t v = vlog_new(kind);
   vlog_set_ident(v, p_system_tf_identifier());

   if (optional(tLPAREN)) {
      p_list_of_arguments(v);
      consume(tRPAREN);
   }

   vlog_set_loc(v, CURRENT_LOC);
   return v;
}

static vlog_node_t p_tf_call(vlog_kind_t kind)
{
   // ps_or_hierarchical_tf_identifier { attribute_instance }
   //    [ ( list_of_arguments ) ]

   BEGIN("task or function call");

   vlog_node_t v = vlog_new(kind);
   vlog_set_ident(v, p_identifier());

   if (optional(tLPAREN)) {
      p_list_of_arguments(v);
      consume(tRPAREN);
   }

   vlog_set_loc(v, CURRENT_LOC);
   vlog_symtab_lookup(symtab, v);
   return v;
}

static vlog_node_t p_subroutine_call(vlog_kind_t kind)
{
   // tf_call | system_tf_call | method_call | [ std:: ] randomize_call

   BEGIN("subroutine call");

   if (peek() == tSYSTASK)
      return p_system_tf_call(kind);
   else
      return p_tf_call(kind);
}

static vlog_node_t p_mintypmax_expression(void)
{
   // expression | expression : expression : expression

   BEGIN("mintypmax expression");

   return p_expression();
}

static vlog_node_t p_concatenation(vlog_node_t head)
{
   // { expression { , expression } }

   BEGIN_WITH_HEAD("concatenation", head);

   if (head == NULL) {
      consume(tLBRACE);
      head = p_expression();
   }

   vlog_node_t v = vlog_new(V_CONCAT);
   vlog_add_param(v, head);

   while (optional(tCOMMA))
      vlog_add_param(v, p_expression());

   consume(tRBRACE);

   vlog_set_loc(v, CURRENT_LOC);
   return v;
}

static vlog_node_t p_multiple_concatenation(vlog_node_t head)
{
   // { expression concatenation }

   BEGIN_WITH_HEAD("multiple concatenation", head);

   vlog_node_t v = p_concatenation(NULL);
   vlog_set_value(v, head);

   consume(tRBRACE);

   vlog_set_loc(v, CURRENT_LOC);
   return v;
}

static vlog_node_t p_primary(void)
{
   // primary_literal | empty_queue
   // | [ class_qualifier | package_scope ] hierarchical_identifier select
   // | concatenation [ [ range_expression ] ]
   // | multiple_concatenation [ [ range_expression ] ]
   // | function_subroutine_call
   // | let_expression | ( mintypmax_expression ) | cast
   // | assignment_pattern_expression | streaming_concatenation
   // | sequence_method_call | this | $ | null

   BEGIN("primary");

   switch (peek()) {
   case tID:
      if (peek_nth(2) == tLPAREN)
         return p_subroutine_call(V_USER_FCALL);
      else
         return p_select(p_identifier());
   case tSTRING:
   case tNUMBER:
   case tUNSIGNED:
   case tREAL:
      return p_primary_literal();
   case tSYSTASK:
      return p_subroutine_call(V_SYS_FCALL);
   case tLPAREN:
      {
         consume(tLPAREN);
         vlog_node_t expr = p_mintypmax_expression();
         consume(tRPAREN);
         return expr;
      }
   case tLBRACE:
      {
         consume(tLBRACE);

         vlog_node_t head = p_expression();
         if (peek() == tLBRACE)
            return p_multiple_concatenation(head);
         else
            return p_concatenation(head);
      }
   default:
      one_of(tID, tSTRING, tNUMBER, tUNSIGNED, tREAL, tSYSTASK, tLPAREN,
             tLBRACE);
      return p_select(error_marker());
   }
}

static vlog_binary_t p_binary_operator(void)
{
   // + | - | * | / | % | == | != | === | !== | ==? | !=? | && | ||
   //  | ** | < | <= | > | >= | & | | | ^ | ^~ | ~^ | >> | <<
   //  | >>> | <<< | -> | <->

   BEGIN("binary operator");

   switch (one_of(tBAR, tPLUS, tAMP, tCASEEQ, tCASENEQ, tLOGOR,
                  tLOGEQ, tLOGNEQ, tDBLAMP, tSHIFTLL, tSHIFTRL,
                  tSHIFTLA, tSHIFTRA, tLT, tGT, tLE, tGE, tMINUS,
                  tTIMES, tOVER, tPERCENT, tPOWER, tCARET, tTILDECARET)) {
   case tBAR:        return V_BINARY_OR;
   case tAMP:        return V_BINARY_AND;
   case tCASEEQ:     return V_BINARY_CASE_EQ;
   case tCASENEQ:    return V_BINARY_CASE_NEQ;
   case tLOGEQ:      return V_BINARY_LOG_EQ;
   case tLOGNEQ:     return V_BINARY_LOG_NEQ;
   case tLOGOR:      return V_BINARY_LOG_OR;
   case tDBLAMP:     return V_BINARY_LOG_AND;
   case tSHIFTLL:    return V_BINARY_SHIFT_LL;
   case tSHIFTRL:    return V_BINARY_SHIFT_RL;
   case tSHIFTLA:    return V_BINARY_SHIFT_LA;
   case tSHIFTRA:    return V_BINARY_SHIFT_RA;
   case tLT:         return V_BINARY_LT;
   case tGT:         return V_BINARY_GT;
   case tLE:         return V_BINARY_LEQ;
   case tGE:         return V_BINARY_GEQ;
   case tMINUS:      return V_BINARY_MINUS;
   case tTIMES:      return V_BINARY_TIMES;
   case tOVER:       return V_BINARY_DIVIDE;
   case tPERCENT:    return V_BINARY_MOD;
   case tPOWER:      return V_BINARY_EXP;
   case tCARET:      return V_BINARY_XOR;
   case tTILDECARET: return V_BINARY_XNOR;
   case tPLUS:
   default:          return V_BINARY_PLUS;
   }
}

static vlog_unary_t p_unary_operator(void)
{
   // + | - | ! | ~ | & | ~& | | | ~| | ^ | ~^ | ^~

   BEGIN("unary operator");

   switch (one_of(tMINUS, tPLUS, tTILDE, tBANG, tAMP, tBAR, tCARET)) {
   case tMINUS: return V_UNARY_NEG;
   case tTILDE: return V_UNARY_BITNEG;
   case tBANG: return V_UNARY_NOT;
   case tAMP: return V_UNARY_AND;
   case tBAR: return V_UNARY_OR;
   case tCARET: return V_UNARY_XOR;
   case tPLUS:
   default: return V_UNARY_IDENTITY;
   }
}

static vlog_incdec_t p_inc_or_dec_operator(void)
{
   // ++ | --

   BEGIN("inc or dec operator");

   switch (one_of(tPLUSPLUS, tMINUSMINUS)) {
   case tMINUSMINUS: return V_INCDEC_MINUS;
   case tPLUSPLUS:
   default: return V_INCDEC_PLUS;
   }
}

static vlog_node_t p_inc_or_dec_expression(vlog_node_t head)
{
   // inc_or_dec_operator { attribute_instance } variable_lvalue
   //   | variable_lvalue { attribute_instance } inc_or_dec_operator

   BEGIN_WITH_HEAD("inc or dec expression", head);

   vlog_node_t v = vlog_new(head ? V_POSTFIX : V_PREFIX);
   vlog_set_subkind(v, p_inc_or_dec_operator());
   vlog_set_target(v, head ?: p_variable_lvalue());

   vlog_set_loc(v, CURRENT_LOC);
   return v;
}

static vlog_node_t p_nonbinary_expression(void)
{
   // primary | unary_operator { attribute_instance } primary
   //   | inc_or_dec_expression | ( operator_assignment )
   //   | conditional_expression | inside_expression | tagged_union_expression

   switch (peek()) {
   case tID:
   case tSTRING:
   case tNUMBER:
   case tUNSIGNED:
   case tREAL:
   case tSYSTASK:
   case tLPAREN:
   case tLBRACE:
      return p_primary();
   case tMINUS:
   case tPLUS:
   case tTILDE:
   case tBANG:
   case tAMP:
   case tBAR:
   case tCARET:
      {
         vlog_node_t v = vlog_new(V_UNARY);
         vlog_set_subkind(v, p_unary_operator());
         vlog_set_value(v, p_primary());
         vlog_set_loc(v, CURRENT_LOC);
         return v;
      }
   case tPLUSPLUS:
   case tMINUSMINUS:
      return p_inc_or_dec_expression(NULL);
   default:
      one_of(tID, tSTRING, tNUMBER, tUNSIGNED, tREAL, tSYSTASK, tLPAREN,
             tLBRACE, tMINUS, tTILDE, tBANG, tAMP, tBAR, tCARET, tPLUSPLUS,
             tMINUSMINUS);
      return p_select(error_marker());
   }
}

static vlog_node_t p_conditional_expression(vlog_node_t head)
{
   // cond_predicate ? { attribute_instance } expression : expression

   BEGIN_WITH_HEAD("conditional expression", head);

   vlog_node_t v = vlog_new(V_COND_EXPR);
   vlog_set_value(v, head);

   consume(tQUESTION);

   while (peek() == tATTRBEGIN)
      p_attribute_instance();

   vlog_set_left(v, p_expression());

   consume(tCOLON);

   vlog_set_right(v, p_expression());

   vlog_set_loc(v, CURRENT_LOC);
   return v;
}

static bool peek_binary_operator(int *prec)
{
   // See LRM 1800-2017 section 11.3.2 for operator precedence table

   switch (peek()) {
   case tPOWER:      *prec = 12; return true;
   case tTIMES:
   case tOVER:
   case tPERCENT:    *prec = 11; return true;
   case tPLUS:
   case tMINUS:      *prec = 10; return true;
   case tSHIFTLL:
   case tSHIFTRL:
   case tSHIFTLA:
   case tSHIFTRA:    *prec = 9;  return true;
   case tLT:
   case tGT:
   case tLE:
   case tGE:         *prec = 8;  return true;
   case tCASEEQ:
   case tCASENEQ:
   case tLOGEQ:
   case tLOGNEQ:     *prec = 7;  return true;
   case tAMP:        *prec = 6;  return true;
   case tTILDECARET:
   case tCARET:      *prec = 5;  return true;
   case tBAR:        *prec = 4;  return true;
   case tDBLAMP:     *prec = 3;  return true;
   case tLOGOR:      *prec = 2;  return true;
   case tQUESTION:   *prec = 1;  return true;
   default:
      return false;
   }
}

static vlog_node_t p_binary_expression(vlog_node_t lhs, int min_prec)
{
   // Precedence climbing method, see
   //    https://en.wikipedia.org/wiki/Operator-precedence_parser

   int prec1;
   while (peek_binary_operator(&prec1) && prec1 >= min_prec) {
      if (peek() == tQUESTION)
         lhs = p_conditional_expression(lhs);
      else {
         vlog_node_t v = vlog_new(V_BINARY);
         vlog_set_subkind(v, p_binary_operator());
         vlog_set_left(v, lhs);

         vlog_node_t rhs = p_nonbinary_expression();

         int prec2;
         while (peek_binary_operator(&prec2) && prec2 > prec1)
            rhs = p_binary_expression(rhs, prec1 + (prec2 > prec1));

         vlog_set_right(v, rhs);
         vlog_set_loc(v, CURRENT_LOC);
         lhs = v;
      }
   }

   return lhs;
}

static vlog_node_t p_expression(void)
{
   // primary | unary_operator { attribute_instance } primary
   //   | inc_or_dec_expression | ( operator_assignment )
   //   | expression binary_operator { attribute_instance } expression
   //   | conditional_expression | inside_expression | tagged_union_expression

   BEGIN("expression");

   vlog_node_t head = p_nonbinary_expression();
   return p_binary_expression(head, 0);
}

static void p_event_expression(vlog_node_t ctrl)
{
   // [ edge_identifier ] expression [ iff expression ]
   //   | sequence_instance [ iff expression ]
   //   | event_expression or event_expression
   //   | event_expression , event_expression
   //   | ( event_expression )

   BEGIN("event expression");

   do {
      if (optional(tLPAREN)) {
         p_event_expression(ctrl);
         consume(tRPAREN);
      }
      else {
         vlog_node_t v = vlog_new(V_EVENT);

         if (optional(tPOSEDGE))
            vlog_set_subkind(v, V_EVENT_POSEDGE);
         else if (optional(tNEGEDGE))
            vlog_set_subkind(v, V_EVENT_NEGEDGE);
         else
            vlog_set_subkind(v, V_EVENT_LEVEL);

         vlog_set_value(v, p_expression());
         vlog_set_loc(v, CURRENT_LOC);

         vlog_add_param(ctrl, v);
      }
   } while (optional(tOR) || optional(tCOMMA));
}

static vlog_node_t p_cond_predicate(void)
{
   // expression_or_cond_pattern { &&& expression_or_cond_pattern }

   BEGIN("cond predicate");

   return p_expression();
}

static vlog_node_t p_event_control(void)
{
   // @ hierarchical_event_identifier | @ ( event_expression )
   //    | @* | @ (*) | @ ps_or_hierarchical_sequence_identifier

   BEGIN("event control");

   consume(tAT);

   vlog_node_t v = vlog_new(V_EVENT_CONTROL);

   switch (one_of(tLPAREN, tTIMES, tPARENSTAR)) {
   case tLPAREN:
      p_event_expression(v);
      consume(tRPAREN);
      break;
   case tTIMES:
   case tPARENSTAR:
      break;
   }

   vlog_set_loc(v, CURRENT_LOC);
   return v;
}

static vlog_node_t p_delay_value(void)
{
   // unsigned_number | real_number | ps_identifier | time_literal | 1step

   BEGIN("delay value");

   return p_unsigned_number();
}

static vlog_node_t p_delay_control(void)
{
   // # delay_value | # ( mintypmax_expression )

   BEGIN("delay control");

   consume(tHASH);

   vlog_node_t v = vlog_new(V_DELAY_CONTROL);
   vlog_set_value(v, p_delay_value());
   vlog_set_loc(v, CURRENT_LOC);
   return v;
}

static vlog_node_t p_delay_or_event_control(void)
{
   // delay_control | event_control | repeat ( expression ) event_control

   BEGIN("delay or event control");

   return p_delay_control();
}

static vlog_node_t p_variable_lvalue(void)
{
   // [ implicit_class_handle . | package_scope ]
   //      hierarchical_variable_identifier select
   //   | { variable_lvalue { , variable_lvalue } }
   //   | [ assignment_pattern_expression_type ]
   //      assignment_pattern_variable_lvalue
   //   | streaming_concatenation

   BEGIN("variable lvalue");

   ident_t id = p_identifier();
   vlog_node_t v = p_select(id);

   vlog_set_loc(v, CURRENT_LOC);
   return v;
}

static vlog_node_t p_net_lvalue(void)
{
   // ps_or_hierarchical_net_identifier constant_select
   //   | { net_lvalue { , net_lvalue } }
   //   | [ assignment_pattern_expression_type ] assignment_pattern_net_lvalue

   BEGIN("net lvalue");

   if (optional(tLBRACE)) {
      vlog_node_t v = vlog_new(V_CONCAT);

      do {
         vlog_add_param(v, p_net_lvalue());
      } while (optional(tCOMMA));

      consume(tRBRACE);

      vlog_set_loc(v, CURRENT_LOC);
      return v;
   }
   else {
      ident_t id = p_identifier();
      vlog_node_t v = p_constant_select(id);

      vlog_set_loc(v, CURRENT_LOC);
      return v;
   }
}

static vlog_node_t p_operator_assignment(vlog_node_t lhs)
{
   // variable_lvalue assignment_operator expression

   BEGIN_WITH_HEAD("operator assignment", lhs);

   consume(tEQ);

   vlog_node_t v = vlog_new(V_BASSIGN);
   vlog_set_target(v, lhs);

   vlog_set_value(v, p_expression());

   vlog_set_loc(v, CURRENT_LOC);
   return v;
}

static vlog_node_t p_blocking_assignment(vlog_node_t lhs)
{
   // variable_lvalue = delay_or_event_control expression
   //   | nonrange_variable_lvalue = dynamic_array_new
   //   | [ implicit_class_handle . | class_scope | package_scope ]
   //         hierarchical_variable_identifier select != class_new
   //   | operator_assignment

   BEGIN_WITH_HEAD("blocking assignment", lhs);

   switch (peek_nth(2)) {
   case tHASH:
   case tAT:
      {
         consume(tEQ);

         vlog_node_t v = vlog_new(V_BASSIGN);
         vlog_set_target(v, lhs);
         vlog_set_delay(v, p_delay_or_event_control());
         vlog_set_value(v, p_expression());

         vlog_set_loc(v, CURRENT_LOC);
         return v;
      }
   default:
      return p_operator_assignment(lhs);
   }
}

static vlog_node_t p_nonblocking_assignment(vlog_node_t lhs)
{
   // variable_lvalue <= [ delay_or_event_control ] expression

   BEGIN_WITH_HEAD("non-blocking assignment", lhs);

   consume(tLE);

   vlog_node_t v = vlog_new(V_NBASSIGN);
   vlog_set_target(v, lhs);

   if (scan(tHASH, tAT))
      vlog_set_delay(v, p_delay_or_event_control());

   vlog_set_value(v, p_expression());

   vlog_set_loc(v, CURRENT_LOC);
   return v;
}

static vlog_node_t p_procedural_timing_control(void)
{
   // delay_control | event_control | cycle_delay

   BEGIN("procedural timing control");

   switch (peek()) {
   case tAT:
      return p_event_control();
   case tHASH:
      return p_delay_control();
   default:
      should_not_reach_here();
   }
}

static vlog_node_t p_procedural_timing_control_statement(void)
{
   // procedural_timing_control statement_or_null

   BEGIN("procedural timing control statement");

   vlog_node_t v = vlog_new(V_TIMING);
   vlog_set_value(v, p_procedural_timing_control());

   vlog_node_t s = p_statement_or_null();
   if (s != NULL)
      vlog_add_stmt(v, s);

   vlog_set_loc(v, CURRENT_LOC);
   return v;
}

static vlog_node_t p_seq_block(void)
{
   // begin [ : block_identifier ] { block_item_declaration }
   //   { statement_or_null } end [ : block_identifier ]

   BEGIN("sequential block");

   consume(tBEGIN);

   vlog_node_t v = vlog_new(V_BLOCK);

   if (optional(tCOLON))
      vlog_set_ident(v, p_identifier());

   while (not_at_token(tEND)) {
      vlog_node_t s = p_statement_or_null();
      if (s != NULL)
         vlog_add_stmt(v, s);
   }

   consume(tEND);

   vlog_set_loc(v, CURRENT_LOC);
   return v;
}

static vlog_node_t p_par_block(void)
{
   // fork [ : block_identifier ] { block_item_declaration }
   //   { statement_or_null } join_keyword [ : block_identifier ]

   BEGIN("parallel block");

   consume(tFORK);

   vlog_node_t v = vlog_new(V_FORK);
   vlog_set_loc(v, CURRENT_LOC);

   vlog_symtab_push(symtab, v);

   if (optional(tCOLON))
      vlog_set_ident(v, p_identifier());

   while (not_at_token(tJOIN)) {
      vlog_node_t s = p_statement_or_null();
      if (s != NULL)
         vlog_add_stmt(v, s);
   }

   vlog_symtab_pop(symtab);

   consume(tJOIN);

   vlog_set_loc(v, CURRENT_LOC);
   return v;
}

static vlog_node_t p_subroutine_call_statement(void)
{
   // subroutine_call ; | void ' ( function_subroutine_call ) ;

   BEGIN("subroutine call statement");

   vlog_node_t v = p_subroutine_call(V_SYS_TCALL);

   consume(tSEMI);

   vlog_set_loc(v, CURRENT_LOC);
   return v;
}

static vlog_node_t p_conditional_statement(void)
{
   // [ unique_priority ] if ( cond_predicate ) statement_or_null
   //     { else if ( cond_predicate ) statement_or_null }
   //     [ else statement_or_null ]

   BEGIN("conditional statement");

   vlog_node_t v = vlog_new(V_IF);

   consume(tIF);
   consume(tLPAREN);

   vlog_node_t c0 = vlog_new(V_COND);
   vlog_set_value(c0, p_cond_predicate());
   vlog_add_cond(v, c0);

   consume(tRPAREN);

   vlog_node_t s0 = p_statement_or_null();
   if (s0 != NULL)
      vlog_add_stmt(c0, s0);

   bool stop = false;
   while (!stop && optional(tELSE)) {
      vlog_node_t c = vlog_new(V_COND);
      vlog_add_cond(v, c);

      if (optional(tIF)) {
         consume(tLPAREN);
         vlog_set_value(c, p_cond_predicate());
         consume(tRPAREN);
      }
      else
         stop = true;

      vlog_node_t s = p_statement_or_null();
      if (s != NULL)
         vlog_add_stmt(c, s);
   }

   vlog_set_loc(v, CURRENT_LOC);
   return v;
}

static vlog_node_t p_variable_assignment(vlog_kind_t kind)
{
   // variable_lvalue = expression

   BEGIN("variable assignment");

   vlog_node_t v = vlog_new(kind);
   vlog_set_target(v, p_variable_lvalue());

   consume(tEQ);

   vlog_set_value(v, p_expression());

   vlog_set_loc(v, CURRENT_LOC);
   return v;
}

static void p_list_of_variable_assignments(vlog_node_t parent)
{
   // variable_assignment { , variable_assignment }

   BEGIN("list of variable assignments");

   do {
      vlog_node_t v = p_variable_assignment(V_BASSIGN);
      vlog_add_stmt(parent, v);
   } while (optional(tCOMMA));
}

static void p_for_variable_declaration(vlog_node_t parent)
{
   // [ var ] data_type variable_identifier = expression
   //   { , variable_identifier = expression }

   BEGIN("for variable declaration");

   optional(tVAR);

   vlog_node_t dt = p_data_type();

   do {
      vlog_node_t v = vlog_new(V_VAR_DECL);
      vlog_set_ident(v, p_identifier());
      vlog_set_type(v, dt);

      consume(tEQ);

      vlog_set_value(v, p_expression());

      vlog_set_loc(v, CURRENT_LOC);
      vlog_add_decl(parent, v);

      vlog_symtab_put(symtab, v);
   } while (optional(tCOMMA));
}

static vlog_node_t p_for_initialization(void)
{
   // list_of_variable_assignments
   //   | for_variable_declaration { , for_variable_declaration }

   BEGIN("for initialization");

   vlog_node_t v = vlog_new(V_FOR_INIT);

   if (scan(tREG, tSTRUCT, tUNION, tENUM, tSVINT, tINTEGER, tSVREAL,
            tSHORTREAL, tREALTIME, tLOGIC, tVAR)) {
      do {
         p_for_variable_declaration(v);
      } while (optional(tCOMMA));
   }
   else
      p_list_of_variable_assignments(v);

   vlog_set_loc(v, CURRENT_LOC);
   return v;
}

static vlog_node_t p_for_step(void)
{
   // operator_assignment | inc_or_dec_expression | function_subroutine_call

   BEGIN("for step");

   vlog_node_t v = vlog_new(V_FOR_STEP);

   switch (peek()) {
   case tPLUSPLUS:
   case tMINUSMINUS:
      vlog_add_stmt(v, p_inc_or_dec_expression(NULL));
      break;
   default:
      {
         vlog_node_t head = p_variable_lvalue();

         switch (peek()) {
         case tPLUSPLUS:
         case tMINUSMINUS:
            vlog_add_stmt(v, p_inc_or_dec_expression(head));
            break;
         default:
            vlog_add_stmt(v, p_operator_assignment(head));
            break;
         }
      }
      break;
   }

   vlog_set_loc(v, CURRENT_LOC);
   return v;
}

static vlog_node_t p_loop_statement(void)
{
   // forever statement_or_null
   //   | repeat ( expression ) statement_or_null
   //   | while ( expression ) statement_or_null
   //   | for ( [ for_initialization ] ; [ expression ] ; [ for_step ] )
   //       statement_or_null
   //   | do statement_or_null while ( expression ) ;
   //   | foreach ( ps_or_hierarchical_array_identifier [ loop_variables ] )
   //       statement

   BEGIN("loop statement");

   switch (one_of(tFOREVER, tWHILE, tREPEAT, tDO, tFOR)) {
   case tFOREVER:
      {
         vlog_node_t v = vlog_new(V_FOREVER);

         vlog_node_t s = p_statement_or_null();
         if (s != NULL)
            vlog_add_stmt(v, s);

         vlog_set_loc(v, CURRENT_LOC);
         return v;
      }

   case tWHILE:
      {
         vlog_node_t v = vlog_new(V_WHILE);

         consume(tLPAREN);
         vlog_set_value(v, p_expression());
         consume(tRPAREN);

         vlog_node_t s = p_statement_or_null();
         if (s != NULL)
            vlog_add_stmt(v, s);

         vlog_set_loc(v, CURRENT_LOC);
         return v;
      }

   case tREPEAT:
      {
         vlog_node_t v = vlog_new(V_REPEAT);

         consume(tLPAREN);
         vlog_set_value(v, p_expression());
         consume(tRPAREN);

         vlog_node_t s = p_statement_or_null();
         if (s != NULL)
            vlog_add_stmt(v, s);

         vlog_set_loc(v, CURRENT_LOC);
         return v;
      }

   case tDO:
      {
         vlog_node_t v = vlog_new(V_DO_WHILE);

         vlog_node_t s = p_statement_or_null();
         if (s != NULL)
            vlog_add_stmt(v, s);

         consume(tWHILE);

         consume(tLPAREN);
         vlog_set_value(v, p_expression());
         consume(tRPAREN);
         consume(tSEMI);

         vlog_set_loc(v, CURRENT_LOC);
         return v;
      }

   case tFOR:
      {
         vlog_node_t v = vlog_new(V_FOR_LOOP);

         vlog_symtab_push(symtab, v);

         consume(tLPAREN);

         if (not_at_token(tSEMI))
            vlog_set_left(v, p_for_initialization());
         else
            vlog_set_left(v, vlog_new(V_FOR_INIT));

         consume(tSEMI);

         if (not_at_token(tSEMI))
            vlog_set_value(v, p_expression());

         consume(tSEMI);

         if (not_at_token(tRPAREN))
            vlog_set_right(v, p_for_step());
         else
            vlog_set_right(v, vlog_new(V_FOR_STEP));

         consume(tRPAREN);

         vlog_node_t s = p_statement_or_null();
         if (s != NULL)
            vlog_add_stmt(v, s);

         vlog_symtab_pop(symtab);

         vlog_set_loc(v, CURRENT_LOC);
         return v;
      }

   default:
      should_not_reach_here();
   }
}

static vlog_node_t p_wait_statement(void)
{
   // wait ( expression ) statement_or_null
   //   | wait fork ;
   //   | wait_order ( hierarchical_identifier { , hierarchical_identifier } )
   //       action_block

   BEGIN("wait statement");

   consume(tWAIT);

   vlog_node_t v = vlog_new(V_WAIT);

   consume(tLPAREN);

   vlog_set_value(v, p_expression());

   consume(tRPAREN);

   vlog_node_t s = p_statement_or_null();
   if (s != NULL)
      vlog_add_stmt(v, s);

   vlog_set_loc(v, CURRENT_LOC);
   return v;
}

static vlog_node_t p_case_item(void)
{
   // case_item_expression { , case_item_expression } : statement_or_null
   //   | default [ : ] statement_or_null

   BEGIN("case item");

   vlog_node_t v = vlog_new(V_CASE_ITEM);

   if (optional(tDEFAULT))
      optional(tCOLON);
   else {
      do {
         vlog_add_param(v, p_expression());
      } while (optional(tCOMMA));

      consume(tCOLON);
   }

   vlog_set_loc(v, CURRENT_LOC);

   vlog_node_t s = p_statement_or_null();
   if (s != NULL)
      vlog_add_stmt(v, s);

   return v;
}

static vlog_node_t p_case_statement(void)
{
   // [ unique_priority ] case_keyword ( case_expression )
   //        case_item { case_item } endcase
   //   | [ unique_priority ] case_keyword ( case_expression ) matches
   //        case_pattern_item { case_pattern_item } endcase
   //   | [ unique_priority ] case ( case_expression ) inside case_inside_item
   //        { case_inside_item } endcase

   BEGIN("case statement");

   vlog_case_kind_t kind = V_CASE_NORMAL;
   switch (one_of(tCASE, tCASEX, tCASEZ)) {
   case tCASEX: kind = V_CASE_X; break;
   case tCASEZ: kind = V_CASE_Z; break;
   }

   vlog_node_t v = vlog_new(V_CASE);
   vlog_set_subkind(v, kind);

   consume(tLPAREN);

   vlog_set_value(v, p_expression());

   consume(tRPAREN);

   do {
      vlog_add_stmt(v, p_case_item());
   } while (not_at_token(tENDCASE));

   consume(tENDCASE);

   vlog_set_loc(v, CURRENT_LOC);
   return v;
}

static vlog_node_t p_event_trigger(void)
{
   // -> hierarchical_event_identifier ;
   //   | ->> [ delay_or_event_control ] hierarchical_event_identifier ;

   BEGIN("event trigger");

   consume(tIFIMPL);

   vlog_node_t v = vlog_new(V_EVENT_TRIGGER);
   vlog_set_ident(v, p_identifier());

   consume(tSEMI);

   vlog_set_loc(v, CURRENT_LOC);
   return v;
}

static vlog_node_t p_statement_item(void)
{
   // blocking_assignment ; | nonblocking_assignment ;
   //   | procedural_continuous_assignment ; | case_statement
   //   | conditional_statement | inc_or_dec_expression ;
   //   | subroutine_call_statement | disable_statement
   //   | event_trigger | loop_statement | jump_statement
   //   | par_block | procedural_timing_control_statement
   //   | seq_block | wait_statement | procedural_assertion_statement
   //   | clocking_drive ; | randsequence_statement | randcase_statement
   //   | expect_property_statement

   BEGIN("statement item");

   switch (peek()) {
   case tID:
      {
         vlog_node_t lhs = p_variable_lvalue(), v = NULL;

         if (peek() == tLE)
            v = p_nonblocking_assignment(lhs);
         else
            v = p_blocking_assignment(lhs);

         consume(tSEMI);
         return v;
      }
   case tAT:
   case tHASH:
      return p_procedural_timing_control_statement();
   case tBEGIN:
      return p_seq_block();
   case tFORK:
      return p_par_block();
   case tSYSTASK:
      return p_subroutine_call_statement();
   case tIF:
      return p_conditional_statement();
   case tFOREVER:
   case tWHILE:
   case tREPEAT:
   case tDO:
   case tFOR:
      return p_loop_statement();
   case tWAIT:
      return p_wait_statement();
   case tCASE:
   case tCASEX:
   case tCASEZ:
      return p_case_statement();
   case tIFIMPL:
      return p_event_trigger();
   default:
      one_of(tID, tAT, tHASH, tBEGIN, tFORK, tSYSTASK, tIF, tFOREVER, tWHILE,
             tREPEAT, tDO, tFOR, tWAIT, tCASE, tCASEX, tCASEZ, tIFIMPL);
      drop_tokens_until(tSEMI);
      return NULL;
   }
}

static vlog_node_t p_statement(void)
{
   // [ block_identifier : ] { attribute_instance } statement_item

   BEGIN("statement");

   while (peek() == tATTRBEGIN)
      p_attribute_instance();

   vlog_node_t s = p_statement_item();
   if (s == NULL)
      return vlog_new(V_BLOCK);

   return s;
}

static vlog_node_t p_statement_or_null(void)
{
   // statement | { attribute_instance } ;

   BEGIN("statement or null");

   if (optional(tSEMI))
      return NULL;
   else
      return p_statement();
}

static vlog_node_t p_always_construct(void)
{
   // always_keyword statement

   BEGIN("always construct");

   vlog_node_t v = vlog_new(V_ALWAYS);

   switch (one_of(tALWAYS, tALWAYSCOMB, tALWAYSFF, tALWAYSLATCH)) {
   case tALWAYSCOMB:  vlog_set_subkind(v, V_ALWAYS_COMB);  break;
   case tALWAYSFF:    vlog_set_subkind(v, V_ALWAYS_FF);    break;
   case tALWAYSLATCH: vlog_set_subkind(v, V_ALWAYS_LATCH); break;
   default:           vlog_set_subkind(v, V_ALWAYS_PLAIN); break;
   }

   vlog_set_ident(v, ident_uniq("__always#line%d", yylloc.first_line));
   vlog_add_stmt(v, p_statement());

   vlog_set_loc(v, CURRENT_LOC);
   return v;
}

static vlog_node_t p_initial_construct(void)
{
   // initial statement_or_null

   BEGIN("initial construct");

   consume(tINITIAL);

   vlog_node_t v = vlog_new(V_INITIAL);
   vlog_set_ident(v, ident_uniq("__initial#line%d", yylloc.first_line));

   vlog_node_t s = p_statement_or_null();
   if (s != NULL)
      vlog_add_stmt(v, s);

   vlog_set_loc(v, CURRENT_LOC);
   return v;
}

static vlog_node_t p_net_assignment(void)
{
   // net_lvalue = expression

   BEGIN("net assignment");

   vlog_symtab_set_implicit(symtab, implicit_kind);

   vlog_node_t v = vlog_new(V_ASSIGN);
   vlog_set_target(v, p_net_lvalue());
   vlog_set_ident(v, ident_uniq("__assign#line%d", state.last_loc.first_line));

   vlog_symtab_set_implicit(symtab, V_NET_NONE);

   consume(tEQ);

   vlog_set_value(v, p_expression());

   vlog_set_loc(v, CURRENT_LOC);
   return v;
}

static void p_list_of_net_assignments(vlog_node_t mod)
{
   // net_assignment { , net_assignment }

   BEGIN("list of net assignments");

   do {
      vlog_add_stmt(mod, p_net_assignment());
   } while (optional(tCOMMA));
}

static void p_continuous_assign(vlog_node_t mod)
{
   // assign [ drive_strength ] [ delay3 ] list_of_net_assignments ;
   //   | assign [ delay_control ] list_of_variable_assignments ;

   BEGIN("continuous assignment");

   consume(tASSIGN);

   p_list_of_net_assignments(mod);

   consume(tSEMI);
}

static vlog_net_kind_t p_net_type(void)
{
   // supply0 | supply1 | tri | triand | trior | trireg | tri0
   //   | tri1 | uwire | wire | wand | wor

   BEGIN("net type");

   switch (one_of(tWIRE, tUWIRE, tSUPPLY0, tSUPPLY1, tTRI, tTRIAND, tTRIOR)) {
   case tSUPPLY0: return V_NET_SUPPLY0;
   case tSUPPLY1: return V_NET_SUPPLY1;
   case tTRI:     return V_NET_TRI;
   case tTRIAND:  return V_NET_TRIAND;
   case tTRIOR:   return V_NET_TRIOR;
   case tUWIRE:   return V_NET_UWIRE;
   case tWIRE:
   default:       return V_NET_WIRE;
   }
}

static vlog_node_t p_net_decl_assignment(vlog_net_kind_t kind,
                                         vlog_node_t datatype)
{
   // net_identifier { unpacked_dimension } [ = expression ]

   BEGIN("net declaration assignment");

   vlog_node_t v = vlog_new(V_NET_DECL);
   vlog_set_subkind(v, kind);
   vlog_set_type(v, datatype);
   vlog_set_ident(v, p_identifier());

   while (peek() == tLSQUARE)
      vlog_add_range(v, p_unpacked_dimension());

   if (optional(tEQ))
      vlog_set_value(v, p_expression());

   vlog_set_loc(v, CURRENT_LOC);
   vlog_symtab_put(symtab, v);
   return v;
}

static void p_list_of_net_decl_assignments(vlog_node_t mod,
                                           vlog_net_kind_t kind,
                                           vlog_node_t datatype)
{
   // net_decl_assignment { , net_decl_assignment }

   BEGIN("list of net declaration assignments");

   do {
      vlog_node_t v = p_net_decl_assignment(kind, datatype);
      vlog_add_decl(mod, v);
   } while (optional(tCOMMA));
}

static void p_net_declaration(vlog_node_t mod)
{
   // net_type [ drive_strength | charge_strength ] [ vectored | scalared ]
   //     data_type_or_implicit [ delay3 ] list_of_net_decl_assignments ;
   //  | net_type_identifier [ delay_control ] list_of_net_decl_assignments ;
   //  | interconnect implicit_data_type [ # delay_value ] net_identifier
   //     { unpacked_dimension } [ , net_identifier { unpacked_dimension } ] ;

   BEGIN("net declaration");

   const vlog_net_kind_t kind = p_net_type();

   vlog_node_t dt = p_data_type_or_implicit();
   p_list_of_net_decl_assignments(mod, kind, dt);

   consume(tSEMI);
}

static vlog_node_t p_variable_dimension(void)
{
   // unsized_dimension | unpacked_dimension | associative_dimension
   //   | queue_dimension

   BEGIN("variable dimension");

   return p_unpacked_dimension();
}

static vlog_node_t p_variable_decl_assignment(vlog_node_t datatype)
{
   // variable_identifier { variable_dimension } [ = expression ]
   //   | dynamic_array_variable_identifier unsized_dimension
   //       { variable_dimension } [ = dynamic_array_new ]
   //   | class_variable_identifier [ = class_new ]

   BEGIN("variable declaration assignment");

   vlog_node_t v = vlog_new(V_VAR_DECL);
   vlog_set_ident(v, p_identifier());
   vlog_set_type(v, datatype);

   while (peek() == tLSQUARE)
      vlog_add_range(v, p_variable_dimension());

   if (optional(tEQ))
      vlog_set_value(v, p_expression());

   vlog_set_loc(v, CURRENT_LOC);
   vlog_symtab_put(symtab, v);
   return v;
}

static void p_list_of_variable_decl_assignments(vlog_node_t parent,
                                                vlog_node_t datatype)
{
   // variable_decl_assignment { , variable_decl_assignment }

   BEGIN("list of variable declaration assignments");

   do {
      vlog_add_decl(parent, p_variable_decl_assignment(datatype));
   } while (optional(tCOMMA));
}

static vlog_node_t p_type_declaration(void)
{
   // typedef data_type type_identifier { variable_dimension } ;
   //   | typedef interface_instance_identifier constant_bit_select .
   //       type_identifier type_identifier ;
   //   | typedef [ enum | struct | union | class | interface class ]
   //       type_identifier ;

   BEGIN("type declaration");

   consume(tTYPEDEF);

   vlog_node_t v = vlog_new(V_TYPE_DECL);
   vlog_set_type(v, p_data_type());
   vlog_set_ident(v, p_identifier());

   consume(tSEMI);

   vlog_set_loc(v, CURRENT_LOC);
   vlog_symtab_put(symtab, v);
   return v;
}

static void p_data_declaration(vlog_node_t mod)
{
   // [ const ] [ var ] [ lifetime ] data_type_or_implicit
   //     list_of_variable_decl_assignments ;
   //  | type_declaration | package_import_declaration | net_type_declaration

   BEGIN("data declaration");

   switch (peek()) {
   case tTYPEDEF:
      vlog_add_decl(mod, p_type_declaration());
      break;

   default:
      {
         vlog_node_t dt = p_data_type_or_implicit();
         p_list_of_variable_decl_assignments(mod, dt);

         consume(tSEMI);
      }
   }
}

static v_port_kind_t p_port_direction(void)
{
   // input | output | inout | ref

   BEGIN("port direction");

   switch (one_of(tINPUT, tOUTPUT, tINOUT)) {
   case tINPUT:  return V_PORT_INPUT;
   case tOUTPUT: return V_PORT_OUTPUT;
   case tINOUT:  return V_PORT_INOUT;
   default:      return V_PORT_INPUT;
   }
}

static v_port_kind_t p_tf_port_direction(void)
{
   // port_direction | const ref

   BEGIN("task or function port direction");

   return p_port_direction();
}

static vlog_node_t p_tf_port_item(void)
{
   // { attribute_instance } [ tf_port_direction ] [ var ]
   //    data_type_or_implicit [ port_identifier { variable_dimension }
   //    [ = expression ] ]

   BEGIN("task or function port item");

   vlog_node_t v = vlog_new(V_PORT_DECL);

   if (scan(tINPUT, tOUTPUT))
      vlog_set_subkind(v, p_tf_port_direction());
   else
      vlog_set_subkind(v, V_PORT_INPUT);

   vlog_set_type(v, p_data_type_or_implicit());

   if (peek() == tID) {
      vlog_set_ident(v, p_identifier());

      if (optional(tEQ))
         vlog_set_value(v, p_expression());
   }

   vlog_set_loc(v, CURRENT_LOC);
   return v;
}

static void p_tf_port_list(vlog_node_t tf)
{
   // tf_port_item { , tf_port_item }

   BEGIN("task or function port list");

   do {
      vlog_node_t v = p_tf_port_item();
      vlog_add_port(tf, v);
      vlog_symtab_put(symtab, v);
   } while (optional(tCOMMA));
}

static void p_list_of_tf_variable_identifiers(vlog_node_t tf,
                                              v_port_kind_t kind,
                                              vlog_node_t dt)
{
   // port_identifier { variable_dimension } [ = expression ]
   //    { , port_identifier { variable_dimension } [ = expression ] }

   BEGIN("list of task or function variable identifiers");

   do {
      vlog_node_t v = vlog_new(V_PORT_DECL);
      vlog_set_subkind(v, kind);
      vlog_set_type(v, dt);
      vlog_set_ident(v, p_identifier());
      vlog_set_loc(v, &state.last_loc);

      if (optional(tEQ))
         vlog_set_value(v, p_expression());

      vlog_add_port(tf, v);
      vlog_symtab_put(symtab, v);
   } while (optional(tCOMMA));
}

static void p_tf_port_declaration(vlog_node_t tf)
{
   // { attribute_instance } tf_port_direction [ var ] data_type_or_implicit
   //    list_of_tf_variable_identifiers ;

   BEGIN("task or function port declaration");

   v_port_kind_t kind = p_tf_port_direction();

   optional(tVAR);

   vlog_node_t dt = p_data_type_or_implicit();

   p_list_of_tf_variable_identifiers(tf, kind, dt);

   consume(tSEMI);
}

static void p_tf_item_declaration(vlog_node_t tf)
{
   // block_item_declaration | tf_port_declaration

   BEGIN("task or function item declaration");

   switch (peek()) {
   case tINPUT:
   case tOUTPUT:
   case tCONST:
      p_tf_port_declaration(tf);
      break;
   default:
      p_block_item_declaration(tf);
      break;
   }
}

static void p_task_body_declaration(vlog_node_t task)
{
   // [ interface_identifier . | class_scope ] task_identifier ;
   //    { tf_item_declaration } { statement_or_null }
   //    endtask [ : task_identifier ]
   // | [ interface_identifier . | class_scope ] task_identifier
   //    ( [ tf_port_list ] ) ; { block_item_declaration }
   //    { statement_or_null } endtask [ : task_identifier ]

   BEGIN("task body declaration");

   ident_t id = p_identifier();
   vlog_set_ident(task, id);
   vlog_set_loc(task, &state.last_loc);

   vlog_symtab_put(symtab, task);

   vlog_symtab_push(symtab, task);

   if (optional(tLPAREN)) {
      p_tf_port_list(task);
      consume(tRPAREN);

      consume(tSEMI);

      while (scan(tREG, tSTRUCT, tUNION, tTYPEDEF, tENUM, tSVINT, tINTEGER,
                  tSVREAL, tSHORTREAL, tREALTIME, tBIT, tLOGIC))
         p_block_item_declaration(task);
   }
   else {
      consume(tSEMI);

      while (scan(tREG, tSTRUCT, tUNION, tTYPEDEF, tENUM, tSVINT, tINTEGER,
                  tSVREAL, tSHORTREAL, tREALTIME, tBIT, tLOGIC, tINPUT,
                  tOUTPUT))
         p_tf_item_declaration(task);
   }

   while (not_at_token(tENDTASK)) {
      vlog_node_t s = p_statement_or_null();
      if (s != NULL)
         vlog_add_stmt(task, s);
   }

   consume(tENDTASK);

   vlog_symtab_pop(symtab);
}

static void p_lifetime(void)
{
   // static | automatic

   BEGIN("lifetime");

   one_of(tAUTOMATIC);
}

static vlog_node_t p_task_declaration(void)
{
   // task [ dynamic_override_specifiers ] [ lifetime ] task_body_declaration

   BEGIN("task declaration");

   vlog_node_t v = vlog_new(V_TASK_DECL);

   consume(tTASK);

   if (scan(tAUTOMATIC))
      p_lifetime();

   p_task_body_declaration(v);

   vlog_set_loc(v, CURRENT_LOC);
   return v;
}

static vlog_node_t p_function_data_type_or_implicit(void)
{
   // data_type_or_void | implicit_data_type

   BEGIN("function data type or implicit");

   if (scan(tREG, tSTRUCT, tUNION, tENUM, tSVINT, tINTEGER, tSVREAL,
            tSHORTREAL, tREALTIME, tLOGIC, tBIT, tEVENT))
      return p_data_type_or_void();
   else
      return p_implicit_data_type();
}

static void p_function_body_declaration(vlog_node_t func)
{
   // function_data_type_or_implicit [ interface_identifier . | class_scope ]
   //    function_identifier ; { tf_item_declaration }
   //    { function_statement_or_null } endfunction [ : function_identifier ]
   // | function_data_type_or_implicit [ interface_identifier . | class_scope ]
   //    function_identifier ( [ tf_port_list ] ) ; { block_item_declaration }
   //    { function_statement_or_null } endfunction [ : function_identifier ]

   BEGIN("function body declaration");

   vlog_set_type(func, p_function_data_type_or_implicit());

   ident_t id = p_identifier();
   vlog_set_ident(func, id);
   vlog_set_loc(func, &state.last_loc);

   vlog_symtab_put(symtab, func);

   vlog_symtab_push(symtab, func);

   if (optional(tLPAREN)) {
      p_tf_port_list(func);
      consume(tRPAREN);

      consume(tSEMI);

      while (scan(tREG, tSTRUCT, tUNION, tTYPEDEF, tENUM, tSVINT, tINTEGER,
                  tSVREAL, tSHORTREAL, tREALTIME, tBIT, tLOGIC))
         p_block_item_declaration(func);
   }
   else {
      consume(tSEMI);

      while (scan(tREG, tSTRUCT, tUNION, tTYPEDEF, tENUM, tSVINT, tINTEGER,
                  tSVREAL, tSHORTREAL, tREALTIME, tBIT, tLOGIC, tINPUT,
                  tOUTPUT))
         p_tf_item_declaration(func);
   }

   while (not_at_token(tENDFUNCTION)) {
      vlog_node_t s = p_statement_or_null();
      if (s != NULL)
         vlog_add_stmt(func, s);
   }

   consume(tENDFUNCTION);

   vlog_symtab_pop(symtab);
}

static vlog_node_t p_function_declaration(void)
{
   // function [ lifetime ] function_body_declaration

   BEGIN("function declaration");

   vlog_node_t v = vlog_new(V_FUNC_DECL);

   consume(tFUNCTION);

   if (scan(tAUTOMATIC))
      p_lifetime();

   p_function_body_declaration(v);

   vlog_set_loc(v, CURRENT_LOC);
   return v;
}

static vlog_node_t p_constant_param_expression(void)
{
   // mintypmax_expression | data_type | $

   BEGIN("constant parameter expression");

   return p_mintypmax_expression();
}

static vlog_node_t p_param_assignment(vlog_node_t datatype, vlog_kind_t kind)
{
   // parameter_identifier { unpacked_dimension }
   //   [ = constant_param_expression ]

   BEGIN("parameter assignment");

   vlog_node_t v = vlog_new(kind);
   vlog_set_ident(v, p_identifier());
   vlog_set_type(v, datatype);

   if (optional(tEQ))
      vlog_set_value(v, p_constant_param_expression());

   vlog_set_loc(v, CURRENT_LOC);
   return v;
}

static void p_list_of_param_assignments(vlog_node_t parent,
                                        vlog_node_t datatype,
                                        vlog_kind_t kind)
{
   // param_assignment { , param_assignment }

   BEGIN("list of parameter assignments");

   do {
      vlog_node_t v = p_param_assignment(datatype, kind);
      vlog_symtab_put(symtab, v);
      vlog_add_decl(parent, v);
   } while (peek_nth(2) == tID && optional(tCOMMA));
}

static void p_parameter_declaration(vlog_node_t mod)
{
   // parameter data_type_or_implicit list_of_param_assignments

   BEGIN("parameter declaration");

   consume(tPARAMETER);

   vlog_node_t dt = p_data_type_or_implicit();
   p_list_of_param_assignments(mod, dt, param_kind);
}

static void p_local_parameter_declaration(vlog_node_t mod)
{
   // localparam data_type_or_implicit list_of_param_assignments

   BEGIN("local parameter declaration");

   consume(tLOCALPARAM);

   vlog_node_t dt = p_data_type_or_implicit();
   p_list_of_param_assignments(mod, dt, V_LOCALPARAM);
}

static void p_block_item_declaration(vlog_node_t parent)
{
   // { attribute_instance } data_declaration
   //   | { attribute_instance } local_parameter_declaration ;
   //   | { attribute_instance } parameter_declaration ;
   //   | { attribute_instance } overload_declaration
   //   | { attribute_instance } let_declaration

   BEGIN("block item declaration");

   switch (peek()) {
   case tREG:
   case tSTRUCT:
   case tUNION:
   case tTYPEDEF:
   case tENUM:
   case tSVINT:
   case tINTEGER:
   case tSVREAL:
   case tSHORTREAL:
   case tREALTIME:
   case tBIT:
   case tLOGIC:
      p_data_declaration(parent);
      break;
   default:
      should_not_reach_here();
   }
}

static void p_package_or_generate_item_declaration(vlog_node_t mod)
{
   // net_declaration | data_declaration | task_declaration
   //   | function_declaration | checker_declaration | dpi_import_export
   //   | extern_constraint_declaration | class_declaration
   //   | class_constructor_declaration | local_parameter_declaration ;
   //   | parameter_declaration ; | covergroup_declaration
   //   | overload_declaration | assertion_item_declaration | ;

   BEGIN("package or generate item declaration");

   switch (peek()) {
   case tWIRE:
   case tUWIRE:
   case tSUPPLY0:
   case tSUPPLY1:
   case tTRI:
   case tTRIAND:
   case tTRIOR:
      p_net_declaration(mod);
      break;
   case tREG:
   case tSTRUCT:
   case tUNION:
   case tTYPEDEF:
   case tENUM:
   case tSVINT:
   case tINTEGER:
   case tSVREAL:
   case tSHORTREAL:
   case tREALTIME:
   case tTIME:
   case tEVENT:
   case tID:
      p_data_declaration(mod);
      break;
   case tTASK:
      vlog_add_decl(mod, p_task_declaration());
      break;
   case tFUNCTION:
      vlog_add_decl(mod, p_function_declaration());
      break;
   case tLOCALPARAM:
      p_local_parameter_declaration(mod);
      consume(tSEMI);
      break;
   case tPARAMETER:
      p_parameter_declaration(mod);
      consume(tSEMI);
      break;
   default:
      one_of(tWIRE, tUWIRE, tSUPPLY0, tSUPPLY1, tREG, tSTRUCT, tUNION, tTYPEDEF,
             tENUM, tSVINT, tINTEGER, tSVREAL, tSHORTREAL, tREALTIME, tTIME,
             tEVENT, tTASK, tFUNCTION, tLOCALPARAM, tPARAMETER);
      drop_tokens_until(tSEMI);
      break;
   }
}

static void p_module_or_generate_item_declaration(vlog_node_t mod)
{
   // package_or_generate_item_declaration | genvar_declaration
   //   | clocking_declaration | default clocking clocking_identifier ;
   //   | default disable iff expression_or_dist ;

   BEGIN("module or generate item declaration");

   p_package_or_generate_item_declaration(mod);
}

static void p_generate_item(vlog_node_t parent)
{
   // module_or_generate_item | interface_or_generate_item
   //   | checker_or_generate_item

   BEGIN("generate item");

   p_module_or_generate_item(parent);
}

static void p_generate_block(vlog_node_t parent)
{
   // generate_item
   //   | [ generate_block_identifier : ] begin [ : generate_block_identifier ]
   //         { generate_item } end [ : generate_block_identifier ]

   BEGIN("generate item");

   if (scan(tID, tBEGIN)) {
      vlog_node_t b = vlog_new(V_BLOCK);

      if (peek() == tID) {
         vlog_set_ident(b, p_identifier());
         consume(tCOLON);
      }

      consume(tBEGIN);

      if (optional(tCOLON)) {
         ident_t name = p_identifier();
         if (vlog_has_ident(b))    // 1800-2023 section 9.3.5
            parse_error(&state.last_loc, "cannot specify both a label and a "
                        "name for the same block");
         else
            vlog_set_ident(b, name);
      }

      vlog_symtab_push(symtab, b);

      while (not_at_token(tEND))
         p_generate_item(b);

      vlog_symtab_pop(symtab);

      consume(tEND);

      if (optional(tCOLON)) {
         ident_t name = p_identifier();
         if (!vlog_has_ident(b))
            parse_error(&state.last_loc, "block does not have a label");
         else if (name != vlog_ident(b))
            parse_error(&state.last_loc, "'%s' does not match label '%s'",
                        istr(name), istr(vlog_ident(b)));
      }

      vlog_set_loc(b, CURRENT_LOC);
      vlog_add_stmt(parent, b);
   }
   else
      p_generate_item(parent);
}

static vlog_node_t p_if_generate_construct(void)
{
   // if ( constant_expression ) generate_block [ else generate_block ]

   BEGIN("if generate construct");

   vlog_node_t v = vlog_new(V_IF_GENERATE);

   consume(tIF);
   consume(tLPAREN);

   vlog_node_t c0 = vlog_new(V_COND);
   vlog_set_value(c0, p_constant_expression());

   consume(tRPAREN);

   vlog_set_loc(c0, CURRENT_LOC);

   p_generate_block(c0);

   vlog_add_cond(v, c0);

   if (optional(tELSE)) {
      vlog_node_t c1 = vlog_new(V_COND);
      vlog_set_loc(c1, &state.last_loc);

      p_generate_block(c1);

      vlog_add_cond(v, c1);
   }

   vlog_set_loc(v, CURRENT_LOC);
   return v;
}

static vlog_node_t p_conditional_generate_construct(void)
{
   // if_generate_construct | case_generate_construct

   BEGIN("conditional generate construct");

   switch (peek()) {
   case tIF:
      return p_if_generate_construct();
   default:
      should_not_reach_here();
   }
}

static void p_module_common_item(vlog_node_t mod)
{
   // module_or_generate_item_declaration
   //   | interface_instantiation | program_instantiation
   //   | assertion_item | bind_directive | continuous_assign
   //   | net_alias | initial_construct | final_construct
   //   | always_construct | loop_generate_construct
   //   | conditional_generate_construct | elaboration_system_task

   BEGIN("module common item");

   switch (peek()) {
   case tALWAYS:
   case tALWAYSCOMB:
   case tALWAYSFF:
   case tALWAYSLATCH:
      vlog_add_stmt(mod, p_always_construct());
      break;
   case tINITIAL:
      vlog_add_stmt(mod, p_initial_construct());
      break;
   case tWIRE:
   case tUWIRE:
   case tSUPPLY0:
   case tSUPPLY1:
   case tTRI:
   case tTRIAND:
   case tTRIOR:
   case tREG:
   case tSTRUCT:
   case tUNION:
   case tTYPEDEF:
   case tENUM:
   case tSVINT:
   case tINTEGER:
   case tSVREAL:
   case tSHORTREAL:
   case tREALTIME:
   case tTIME:
   case tTASK:
   case tFUNCTION:
   case tLOCALPARAM:
   case tPARAMETER:
   case tEVENT:
   case tID:
      p_module_or_generate_item_declaration(mod);
      break;
   case tASSIGN:
      p_continuous_assign(mod);
      break;
   case tIF:
      vlog_add_stmt(mod, p_conditional_generate_construct());
      break;
   default:
      one_of(tALWAYS, tALWAYSCOMB, tALWAYSFF, tALWAYSLATCH, tWIRE, tUWIRE,
             tSUPPLY0, tSUPPLY1, tTRI, tTRIAND, tTRIOR, tREG, tSTRUCT, tUNION,
             tTYPEDEF, tENUM, tSVINT, tINTEGER, tSVREAL, tSHORTREAL, tREALTIME,
             tTIME, tTASK, tFUNCTION, tPARAMETER, tLOCALPARAM, tASSIGN, tIF,
             tEVENT);
      drop_tokens_until(tSEMI);
   }
}

static vlog_strength_t p_strength0(void)
{
   // supply0 | strong0 | pull0 | weak0

   BEGIN("strength0");

   switch (one_of(tSUPPLY0)) {
   default:
   case tSUPPLY0: return V_STRENGTH_SUPPLY;
   }
}

static vlog_strength_t p_strength1(void)
{
   // supply1 | strong1 | pull1 | weak1

   BEGIN("strength1");

   switch (one_of(tSUPPLY1)) {
   default:
   case tSUPPLY1: return V_STRENGTH_SUPPLY;
   }
}

static vlog_node_t p_pulldown_strength(void)
{
   // ( strength0 , strength1 ) | ( strength1 , strength0 ) | ( strength0 )

   BEGIN("pulldown strength");

   consume(tLPAREN);

   vlog_strength_t s0, s1;
   switch (peek()) {
   case tSUPPLY1:
      s1 = p_strength1();
      consume(tCOMMA);
      s0 = p_strength0();
      break;
   default:
      s0 = s1 = p_strength0();
      if (optional(tCOMMA))
         s1 = p_strength1();
      break;
   }

   consume(tRPAREN);

   vlog_node_t v = vlog_new(V_STRENGTH);
   vlog_set_subkind(v, MAKE_STRENGTH(s0, s1));
   vlog_set_loc(v, CURRENT_LOC);
   return v;
}

static vlog_node_t p_pullup_strength(void)
{
   // ( strength0 , strength1 ) | ( strength1 , strength0 ) | ( strength1 )

   BEGIN("pullup strength");

   consume(tLPAREN);

   vlog_strength_t s0, s1;
   switch (peek()) {
   case tSUPPLY0:
      s0 = p_strength0();
      consume(tCOMMA);
      s1 = p_strength1();
      break;
   default:
      s1 = s0 = p_strength1();
      if (optional(tCOMMA))
         s0 = p_strength0();
      break;
   }

   consume(tRPAREN);

   vlog_node_t v = vlog_new(V_STRENGTH);
   vlog_set_subkind(v, MAKE_STRENGTH(s0, s1));
   vlog_set_loc(v, CURRENT_LOC);
   return v;
}

static vlog_node_t p_pull_gate_instance(vlog_gate_kind_t kind, vlog_node_t st)
{
   // [ name_of_instance ] ( output_terminal )

   BEGIN("pull gate instance");

   vlog_node_t v = vlog_new(V_GATE_INST);
   vlog_set_subkind(v, kind);
   vlog_add_param(v, st);

   if (peek() == tID) {
      vlog_set_ident(v, p_identifier());
      vlog_set_loc(v, &state.last_loc);
      vlog_symtab_put(symtab, v);
   }
   else
      vlog_set_ident(v, ident_uniq("#gate"));

   consume(tLPAREN);

   vlog_symtab_set_implicit(symtab, implicit_kind);

   vlog_set_target(v, p_net_lvalue());

   vlog_symtab_set_implicit(symtab, V_NET_NONE);

   consume(tRPAREN);

   vlog_set_loc(v, CURRENT_LOC);
   return v;
}

static vlog_node_t p_n_terminal_gate_instance(vlog_gate_kind_t kind)
{
   // [ name_of_instance ] ( output_terminal , input_terminal
   //     { , input_terminal } )

   BEGIN("N-terminal gate instance");

   vlog_node_t v = vlog_new(V_GATE_INST);
   vlog_set_subkind(v, kind);

   if (peek() == tID) {
      vlog_set_ident(v, p_identifier());
      vlog_set_loc(v, &state.last_loc);
      vlog_symtab_put(symtab, v);
   }
   else
      vlog_set_ident(v, ident_uniq("#gate"));

   consume(tLPAREN);

   vlog_symtab_set_implicit(symtab, implicit_kind);

   vlog_set_target(v, p_net_lvalue());

   consume(tCOMMA);

   do {
      vlog_add_param(v, p_expression());
   } while (optional(tCOMMA));

   vlog_symtab_set_implicit(symtab, V_NET_NONE);

   consume(tRPAREN);

   vlog_set_loc(v, CURRENT_LOC);
   return v;
}

static void p_gate_instantiation(vlog_node_t mod)
{
   // cmos_switchtype [ delay3 ] cmos_switch_instance
   //     { , cmos_switch_instance } ;
   //  | enable_gatetype [ drive_strength ] [ delay3 ]
   //     enable_gate_instance { , enable_gate_instance } ;
   //  | mos_switchtype [ delay3 ] mos_switch_instance
   //     { , mos_switch_instance } ;
   //  | n_input_gatetype [ drive_strength ] [ delay2 ] n_input_gate_instance
   //     { , n_input_gate_instance } ;
   //  | n_output_gatetype [ drive_strength ] [ delay2 ] n_output_gate_instance
   //     { , n_output_gate_instance } ;
   //  | pass_en_switchtype [ delay2 ] pass_enable_switch_instance
   //     { , pass_enable_switch_instance } ;
   //  | pass_switchtype pass_switch_instance { , pass_switch_instance } ;
   //  | pulldown [ pulldown_strength ] pull_gate_instance
   //     { , pull_gate_instance } ;
   //  | pullup [ pullup_strength ] pull_gate_instance
   //     { , pull_gate_instance } ;

   BEGIN("gate instantiation");

   switch (one_of(tPULLDOWN, tPULLUP, tAND, tNAND, tOR, tNOR, tXOR, tXNOR,
                  tNOT, tBUF)) {
   case tPULLDOWN:
      {
         vlog_node_t st;
         if (peek() == tLPAREN && peek_nth(2) != tID)
            st = p_pulldown_strength();
         else {
            st = vlog_new(V_STRENGTH);
            vlog_set_subkind(st, ST_PULLUP);
         }

         do {
            vlog_node_t g = p_pull_gate_instance(V_GATE_PULLDOWN, st);
            vlog_add_stmt(mod, g);
         } while (optional(tCOMMA));
      }
      break;

   case tPULLUP:
      {
         vlog_node_t st;
         if (peek() == tLPAREN && peek_nth(2) != tID)
            st = p_pullup_strength();
         else {
            st = vlog_new(V_STRENGTH);
            vlog_set_subkind(st, ST_PULLUP);
         }

         do {
            vlog_node_t g = p_pull_gate_instance(V_GATE_PULLUP, st);
            vlog_add_stmt(mod, g);
         } while (optional(tCOMMA));
      }
      break;

   case tAND:
      do {
         vlog_add_stmt(mod, p_n_terminal_gate_instance(V_GATE_AND));
      } while (optional(tCOMMA));
      break;

   case tNAND:
      do {
         vlog_add_stmt(mod, p_n_terminal_gate_instance(V_GATE_NAND));
      } while (optional(tCOMMA));
      break;

   case tOR:
      do {
         vlog_add_stmt(mod, p_n_terminal_gate_instance(V_GATE_OR));
      } while (optional(tCOMMA));
      break;

   case tNOR:
      do {
         vlog_add_stmt(mod, p_n_terminal_gate_instance(V_GATE_NOR));
      } while (optional(tCOMMA));
      break;

   case tXOR:
      do {
         vlog_add_stmt(mod, p_n_terminal_gate_instance(V_GATE_XOR));
      } while (optional(tCOMMA));
      break;

   case tXNOR:
      do {
         vlog_add_stmt(mod, p_n_terminal_gate_instance(V_GATE_XNOR));
      } while (optional(tCOMMA));
      break;

   case tNOT:
      do {
         vlog_add_stmt(mod, p_n_terminal_gate_instance(V_GATE_NOT));
      } while (optional(tCOMMA));
      break;

   case tBUF:
      do {
         vlog_add_stmt(mod, p_n_terminal_gate_instance(V_GATE_BUF));
      } while (optional(tCOMMA));
      break;

   default:
      break;
   }

   consume(tSEMI);
}

static vlog_node_t p_module_path_expression(void)
{
   // module_path_primary
   //   | unary_module_path_operator { attribute_instance } module_path_primary
   //   | module_path_expression binary_module_path_operator
   //      { attribute_instance } module_path_expression
   //   | module_path_conditional_expression

   BEGIN("module path expression");

   // TODO: sem should check valid subset
   return p_expression();
}

static void p_path_delay_expression(void)
{
   // constant_expression
   //   | constant_expression : constant_expression : constant_expression

   BEGIN("path delay expression");

   (void)p_constant_expression();
}

static void p_list_of_path_delay_expressions(void)
{
   // path_delay_expression { , path_delay_expression }

   BEGIN("list of path delay expressions");

   do {
      p_path_delay_expression();
   } while (optional(tCOMMA));
}

static void p_path_delay_value(void)
{
   // list_of_path_delay_expressions | ( list_of_path_delay_expressions )

   BEGIN("path delay value");

   if (optional(tLPAREN)) {
      p_list_of_path_delay_expressions();
      consume(tRPAREN);
   }
   else
      p_list_of_path_delay_expressions();
}

static vlog_node_t p_specify_terminal_descriptor(void)
{
   // identifier [ [ constant_range_expression ] ]

   BEGIN("specify terminal descriptor");

   vlog_node_t v = vlog_new(V_REF);
   vlog_set_ident(v, p_identifier());
   vlog_set_loc(v, CURRENT_LOC);

   if (optional(tLSQUARE)) {
      (void)p_constant_range_expression();
      consume(tRSQUARE);
   }

   return v;
}

static void p_list_of_path_inputs(vlog_node_t v, vlog_node_t head)
{
   // specify_input_terminal_descriptor { , specify_input_terminal_descriptor }

   BEGIN_WITH_HEAD("list of path inputs", head);

   while (optional(tCOMMA))
      (void)p_specify_terminal_descriptor();
}

static void p_list_of_path_outputs(vlog_node_t v)
{
   // specify_output_terminal_descriptor
   //     { , specify_output_terminal_descriptor }

   BEGIN("list of path outputs");

   do {
      (void)p_specify_terminal_descriptor();
   } while (optional(tCOMMA));
}

static void p_polarity_operator(void)
{
   // + | -

   BEGIN("polarity operator");

   (void)one_of(tPLUS, tMINUS);
}

static vlog_node_t p_parallel_path_description(vlog_node_t head)
{
   // ( specify_input_terminal_descriptor [ polarity_operator ]
   //     => specify_output_terminal_descriptor )

   EXTEND("parallel path description");

   if (scan(tPLUS, tMINUS))
      (void)p_polarity_operator();

   consume(tASSOC);

   (void)p_specify_terminal_descriptor();

   consume(tRPAREN);
   return NULL;
}

static vlog_node_t p_full_path_description(vlog_node_t head)
{
   // ( list_of_path_inputs [ polarity_operator ] *> list_of_path_outputs )

   EXTEND("full path description");

   p_list_of_path_inputs(NULL, head);

   if (scan(tPLUS, tMINUS))
      (void)p_polarity_operator();

   consume(tTIMESGT);

   p_list_of_path_outputs(NULL);

   consume(tRPAREN);
   return NULL;
}

static vlog_node_t p_simple_path_declaration(void)
{
   // parallel_path_description = path_delay_value
   //   | full_path_description = path_delay_value

   BEGIN("simple path declaration");

   // Parse up to the first terminal descriptor to determine which
   // production to use

   consume(tLPAREN);

   vlog_node_t head = p_specify_terminal_descriptor();

   if (scan(tCOMMA, tTIMESGT) || peek_nth(2) == tTIMESGT)
      (void)p_full_path_description(head);
   else
      (void)p_parallel_path_description(head);

   consume(tEQ);

   (void)p_path_delay_value();

   return NULL;
}

static void p_edge_identifier(void)
{
   // posedge | negedge | edge

   BEGIN("edge identifier");

   one_of(tPOSEDGE, tNEGEDGE, tEDGE);
}

static vlog_node_t p_parallel_edge_sensitive_path_description(vlog_node_t head)
{
   // ( [ edge_identifier ] specify_input_terminal_descriptor
   //     [ polarity_operator ] => ( specify_output_terminal_descriptor
   //     [ polarity_operator ] : data_source_expression ) )

   EXTEND("parallel edge sensitive path description");

   if (scan(tPLUS, tMINUS))
      (void)p_polarity_operator();

   consume(tASSOC);

   consume(tLPAREN);

   (void)p_specify_terminal_descriptor();

   if (scan(tPLUS, tMINUS)) {
      (void)p_polarity_operator();
      consume(tCOLON);
   }
   else if (scan(tINDEXPOS, tINDEXNEG))
      consume(peek());  // Lexing ambiguity with +: and -:
   else
      consume(tCOLON);

   (void)p_expression();

   consume(tRPAREN);
   consume(tRPAREN);
   return NULL;
}

static vlog_node_t p_full_edge_sensitive_path_description(vlog_node_t head)
{
   // ( [ edge_identifier ] list_of_path_inputs [ polarity_operator ] *>
   //     ( list_of_path_outputs [ polarity_operator ]
   //     : data_source_expression ) )

   EXTEND("full edge sensitive path description");

   p_list_of_path_inputs(NULL, head);

   if (scan(tPLUS, tMINUS))
      (void)p_polarity_operator();

   consume(tTIMESGT);

   consume(tLPAREN);

   p_list_of_path_outputs(NULL);

   if (scan(tPLUS, tMINUS)) {
      (void)p_polarity_operator();
      consume(tCOLON);
   }
   else if (scan(tINDEXPOS, tINDEXNEG))
      consume(peek());  // Lexing ambiguity with +: and -:
   else
      consume(tCOLON);

   (void)p_expression();

   consume(tRPAREN);
   consume(tRPAREN);
   return NULL;
}

static vlog_node_t p_edge_sensitive_path_declaration(void)
{
   // parallel_edge_sensitive_path_description = path_delay_value
   //   | full_edge_sensitive_path_description = path_delay_value

   BEGIN("edge sensitive path declaration");

   // Parse up to the first terminal descriptor to determine which
   // production to use

   consume(tLPAREN);

   if (scan(tEDGE, tPOSEDGE, tNEGEDGE))
      p_edge_identifier();

   vlog_node_t head = p_specify_terminal_descriptor();

   if (scan(tCOMMA, tTIMESGT) || peek_nth(2) == tTIMESGT)
      (void)p_full_edge_sensitive_path_description(head);
   else
      (void)p_parallel_edge_sensitive_path_description(head);

   consume(tEQ);

   (void)p_path_delay_value();

   return NULL;
}

static vlog_node_t p_state_dependent_path_declaration(void)
{
   // if ( module_path_expression ) simple_path_declaration
   //   | if ( module_path_expression ) edge_sensitive_path_declaration
   //   | ifnone simple_path_declaration

   BEGIN("state dependent path declaration");

   switch (one_of(tIF, tIFNONE)) {
   case tIF:
      consume(tLPAREN);
      (void)p_module_path_expression();
      consume(tRPAREN);
      break;
   case tIFNONE:
      break;
   }

   if (peek_nth(2) == tID)
      (void)p_simple_path_declaration();
   else {
      // This is invalid for ifnone according to the grammar but is
      // accepted by some simulators and seen in the wild
      (void)p_edge_sensitive_path_declaration();
   }

   return NULL;
}

static vlog_node_t p_path_declaration(void)
{
   // simple_path_declaration ;
   //  | edge_sensitive_path_declaration ;
   //  | state_dependent_path_declaration ;

   BEGIN("path declaration");

   switch (peek()) {
   case tIF:
   case tIFNONE:
      (void)p_state_dependent_path_declaration();
      break;
   case tLPAREN:
      switch (peek_nth(2)) {
      case tEDGE:
      case tNEGEDGE:
      case tPOSEDGE:
         (void)p_edge_sensitive_path_declaration();
         break;
      default:
         (void)p_simple_path_declaration();
         break;
      }
      break;
   default:
      one_of(tIF, tIFNONE);
   }

   consume(tSEMI);
   return NULL;
}

static void p_timing_check_event_control(void)
{
   // posedge | negedge | edge | edge_control_specifier

   BEGIN("timing check event control");

   one_of(tPOSEDGE, tNEGEDGE, tEDGE);
}

static void p_scalar_timing_check_condition(void)
{
   //    expression
   // | ~ expression
   // | expression == scalar_constant
   // | expression === scalar_constant
   // | expression != scalar_constant
   // | expression !== scalar_constant

   BEGIN("scalar timing check condition")

   p_expression();
}

static void p_timing_check_condition(void)
{
   //     scalar_timing_check_condition
   // | ( scalar_timing_check_condition )

   BEGIN("timing check condition");

   if (optional(tLPAREN)) {
      p_scalar_timing_check_condition();
      consume(tRPAREN);
   }
   else
      p_scalar_timing_check_condition();
}

static vlog_node_t p_timing_check_event(void)
{
   // [ timing_check_event_control ] specify_terminal_descriptor
   //    [ &&& timing_check_condition ]

   BEGIN("timing check event");

   if (scan(tEDGE, tPOSEDGE, tNEGEDGE))
      p_timing_check_event_control();

   (void)p_specify_terminal_descriptor();

   if (optional(tTRPLAMP))
      p_timing_check_condition();

   return NULL;
}

static vlog_node_t p_controlled_timing_check_event(void)
{
   // timing_check_event_control specify_terminal_descriptor
   //    [ &&& timing_check_condition ]

   BEGIN("controlled timing check event");

   p_timing_check_event_control();

   (void)p_specify_terminal_descriptor();

   if (optional(tTRPLAMP))
      p_timing_check_condition();

   return NULL;
}

static vlog_node_t p_setup_or_hold_timing_check(void)
{
   // $setup ( data_event , reference_event , timing_check_limit
   //   [ , [ notifier ] ] ) ;
   //
   // $hold ( reference_event , data_event , timing_check_limit
   //   [ , [ notifier ] ] ) ;

   BEGIN("setup/hold timing check");

   one_of(tDLRSETUP, tDLRHOLD);
   consume(tLPAREN);

   (void)p_timing_check_event();

   consume(tCOMMA);

   (void)p_timing_check_event();

   consume(tCOMMA);

   (void)p_expression();

   if (optional(tCOMMA)) {
      if (peek() == tID)
         p_identifier();
   }

   consume(tRPAREN);
   consume(tSEMI);

   return NULL;
}

static vlog_node_t p_recovery_or_removal_timing_check(void)
{
   // $recovery ( reference_event , data_event , timing_check_limit
   //   [ , [ notifier ] ] ) ;
   //
   // $removal ( reference_event , data_event , timing_check_limit
   //   [ , [ notifier ] ] ) ;

   BEGIN("recovery/removal timing check");

   one_of(tDLRRECOVERY, tDLRREMOVAL);
   consume(tLPAREN);

   (void)p_timing_check_event();

   consume(tCOMMA);

   (void)p_timing_check_event();

   consume(tCOMMA);

   (void)p_expression();

   if (optional(tCOMMA)) {
      if (peek() == tID)
         p_identifier();
   }

   consume(tRPAREN);
   consume(tSEMI);

   return NULL;
}

static vlog_node_t p_width_timing_check(void)
{
   // $width ( controlled_reference_event , timing_check_limit , threshold
   //   [ , [ notifier ] ] ) ;

   BEGIN("width timing check");

   consume(tDLRWIDTH);
   consume(tLPAREN);

   (void)p_controlled_timing_check_event();

   consume(tCOMMA);

   (void)p_expression();

   consume(tCOMMA);

   (void)p_constant_expression();

   if (optional(tCOMMA)) {
      if (peek() == tID)
         p_identifier();
   }

   consume(tRPAREN);
   consume(tSEMI);

   return NULL;
}

static vlog_node_t p_delayed_data_or_reference(void)
{
   // terminal_identifier
   //   | terminal_identifier [ constant_mintypmax_expression ]

   BEGIN("delayed data/reference");

   p_identifier();

   return NULL;
}

static vlog_node_t p_setuphold_or_recrem_timing_check(void)
{
   // $setuphold ( reference_event , data_event , timing_check_limit ,
   //    timing_check_limit [ , [ notifier ] [ , [ timestamp_condition ]
   //    [ , [ timecheck_condition ] [ , [ delayed_reference ]
   //    [ , [ delayed_data ] ] ] ] ] ] ) ;

   BEGIN("setuphold/recrem timing check");

   one_of(tDLRSETUPHOLD, tDLRRECREM);
   consume(tLPAREN);

   (void)p_timing_check_event();

   consume(tCOMMA);

   (void)p_timing_check_event();

   consume(tCOMMA);

   (void)p_expression();

   consume(tCOMMA);

   (void)p_expression();

   if (optional(tCOMMA)) {
      if (peek() == tID)
         p_identifier(); // notifier

      if (optional(tCOMMA)) {
         if (not_at_token(tCOMMA, tRPAREN))
            (void)p_mintypmax_expression();  // timestamp_condition

         if (optional(tCOMMA)) {
            if (not_at_token(tCOMMA, tRPAREN))
               (void)p_mintypmax_expression();  // timecheck_condition

            if (optional(tCOMMA)) {
               if (not_at_token(tCOMMA, tRPAREN))
                  p_delayed_data_or_reference();  // delayed_reference

               if (optional(tCOMMA)) {
                  if (not_at_token(tCOMMA, tRPAREN))
                     p_delayed_data_or_reference();  // delayed_data
               }
            }
         }
      }
   }

   consume(tRPAREN);
   consume(tSEMI);

   return NULL;
}

static vlog_node_t p_system_timing_check(void)
{
   // $setup_timing_check | $hold_timing_check | $setuphold_timing_check
   //   | $recovery_timing_check | $removal_timing_check | $recrem_timing_check
   //   | $skew_timing_check | $timeskew_timing_check | $fullskew_timing_check
   //   | $period_timing_check | $width_timing_check | $nochange_timing_check

   BEGIN("system timing check");

   switch (peek()) {
   case tDLRSETUP:
   case tDLRHOLD:
      return p_setup_or_hold_timing_check();
   case tDLRRECOVERY:
   case tDLRREMOVAL:
      return p_recovery_or_removal_timing_check();
   case tDLRWIDTH:
      return p_width_timing_check();
   case tDLRSETUPHOLD:
   case tDLRRECREM:
      return p_setuphold_or_recrem_timing_check();
   default:
      should_not_reach_here();
   }
}

static vlog_node_t p_specparam_assignment(void)
{
   // specparam_identifier = constant_mintypmax_expression
   //    | pulse_control_specparam

   BEGIN("specparam assignment");

   vlog_node_t v = vlog_new(V_SPECPARAM);
   vlog_set_ident(v, p_identifier());

   consume(tEQ);

   vlog_set_value(v, p_constant_mintypmax_expression());

   vlog_set_loc(v, CURRENT_LOC);
   vlog_symtab_put(symtab, v);
   return v;
}

static void p_list_of_specparam_assignments(vlog_node_t parent)
{
   // specparam_assignment { , specparam_assignment }

   BEGIN("list of specparam assignments");

   do {
      vlog_add_decl(parent, p_specparam_assignment());
   } while (optional(tCOMMA));
}

static void p_specparam_declaration(vlog_node_t parent)
{
   // specparam [ packed_dimension ] list_of_specparam_assignments ;

   BEGIN("specparam declaration");

   consume(tSPECPARAM);

   if (peek() == tLSQUARE)
      (void)p_packed_dimension();

   p_list_of_specparam_assignments(parent);

   consume(tSEMI);
}

static void p_specify_item(vlog_node_t parent)
{
   // specparam_declaration | pulsestyle_declaration | showcancelled_declaration
   //   | path_declaration | system_timing_check

   BEGIN("specify item");

   switch (peek()) {
   case tSPECPARAM:
      p_specparam_declaration(parent);
      break;
   case tLPAREN:
   case tIF:
   case tIFNONE:
      (void)p_path_declaration();
      break;
   case tDLRSETUP:
   case tDLRHOLD:
   case tDLRRECOVERY:
   case tDLRREMOVAL:
   case tDLRSETUPHOLD:
   case tDLRRECREM:
   case tDLRWIDTH:
      (void)p_system_timing_check();
      break;
   default:
      one_of(tSPECPARAM, tLPAREN, tIF, tIFNONE, tDLRSETUP, tDLRHOLD,
             tDLRRECOVERY, tDLRREMOVAL, tDLRSETUPHOLD, tDLRRECREM,
             tDLRWIDTH);
   }
}

static vlog_node_t p_specify_block(void)
{
   // specify { specify_item } endspecify

   BEGIN("specify block");

   consume(tSPECIFY);

   vlog_node_t v = vlog_new(V_SPECIFY);
   vlog_set_loc(v, CURRENT_LOC);

   vlog_symtab_push(symtab, v);

   while (not_at_token(tENDSPECIFY))
      p_specify_item(v);

   vlog_symtab_pop(symtab);

   consume(tENDSPECIFY);

   vlog_set_loc(v, CURRENT_LOC);
   return v;
}

static vlog_node_t p_ordered_port_connection(void)
{
   // { attribute_instance } [ expression ]

   BEGIN("ordered port connection");

   vlog_node_t v = vlog_new(V_PORT_CONN);

   if (not_at_token(tCOMMA, tRPAREN))
      vlog_set_value(v, p_expression());

   vlog_set_loc(v, CURRENT_LOC);
   return v;
}

static vlog_node_t p_named_port_connection(void)
{
   // { attribute_instance } . port_identifier [ ( [ expression ] ) ]
   //    | { attribute_instance } .*

   BEGIN("named port connection");

   vlog_node_t v = vlog_new(V_PORT_CONN);

   consume(tDOT);

   vlog_set_ident(v, p_identifier());

   if (optional(tLPAREN)) {
      vlog_set_value(v, p_expression());
      consume(tRPAREN);
   }

   vlog_set_loc(v, CURRENT_LOC);
   return v;
}

static void p_list_of_port_connections(vlog_node_t inst)
{
   // ordered_port_connection { , ordered_port_connection }
   //   | named_port_connection { , named_port_connection }

   BEGIN("list of port connections");

   vlog_symtab_set_implicit(symtab, implicit_kind);

   do {
      if (peek() == tDOT)
         vlog_add_param(inst, p_named_port_connection());
      else
         vlog_add_param(inst, p_ordered_port_connection());
   } while (optional(tCOMMA));

   vlog_symtab_set_implicit(symtab, V_NET_NONE);
}

static vlog_node_t p_hierarchical_instance(void)
{
   // name_of_instance ( [ list_of_port_connections ] )

   BEGIN("hierarchical instance");

   vlog_node_t v = vlog_new(V_MOD_INST);
   vlog_set_ident(v, p_identifier());

   consume(tLPAREN);

   if (peek() != tRPAREN)
      p_list_of_port_connections(v);

   consume(tRPAREN);

   vlog_set_loc(v, CURRENT_LOC);
   vlog_symtab_put(symtab, v);
   return v;
}

static vlog_node_t p_udp_instance(void)
{
   // [ name_of_instance ] ( output_terminal , input_terminal
   //   { , input_terminal } )

   BEGIN("udp instance");

   vlog_node_t v = vlog_new(V_MOD_INST);
   if (peek() == tID)
      vlog_set_ident(v, p_identifier());
   else
      vlog_set_ident(v, ident_uniq("$unnamed"));

   consume(tLPAREN);

   p_list_of_port_connections(v);

   consume(tRPAREN);

   vlog_set_loc(v, CURRENT_LOC);
   return v;
}

static vlog_node_t p_param_expression(void)
{
   // mintypmax_expression | data_type | $

   BEGIN("param expression");

   return p_expression();   // TODO
}

static vlog_node_t p_named_parameter_assignment(void)
{
   // . parameter_identifier ( [ param_expression ] )

   BEGIN("named parameter assignment");

   consume(tDOT);

   vlog_node_t v = vlog_new(V_PARAM_ASSIGN);
   vlog_set_ident(v, p_identifier());

   consume(tLPAREN);

   if (peek() != tRPAREN)
      vlog_set_value(v, p_param_expression());

   consume(tRPAREN);

   vlog_set_loc(v, CURRENT_LOC);
   return v;
}

static vlog_node_t p_ordered_parameter_assignment(void)
{
   // param_expression

   BEGIN("ordered parameter assignment");

   vlog_node_t v = vlog_new(V_PARAM_ASSIGN);
   vlog_set_value(v, p_param_expression());

   vlog_set_loc(v, CURRENT_LOC);
   return v;
}

static void p_list_of_parameter_assignments(vlog_node_t inst)
{
   // ordered_parameter_assignment { , ordered_parameter_assignment }
   //   | named_parameter_assignment { , named_parameter_assignment }

   BEGIN("list of parameter assignments");

   do {
      if (peek() == tDOT)
         vlog_add_param(inst, p_named_parameter_assignment());
      else
         vlog_add_param(inst, p_ordered_parameter_assignment());
   } while (optional(tCOMMA));
}

static void p_parameter_value_assignment(vlog_node_t inst)
{
   // # ( [ list_of_parameter_assignments ] )

   BEGIN("parameter value assignment");

   consume(tHASH);
   consume(tLPAREN);

   if (peek() != tRPAREN)
      p_list_of_parameter_assignments(inst);

   consume(tRPAREN);
}

static void p_module_or_udp_instantiation(vlog_node_t mod)
{
   // module_identifier [ parameter_value_assignment ] hierarchical_instance
   //   { , hierarchical_instance } ;
   //
   // udp_identifier [ drive_strength ] [ delay2 ] udp_instance
   //   { , udp_instance } ;

   BEGIN("module instantiation");

   vlog_node_t v = vlog_new(V_INST_LIST);
   vlog_set_ident(v, p_identifier());

   if (peek() == tHASH)
      p_parameter_value_assignment(v);

   do {
      if (peek() == tLPAREN)
         vlog_add_stmt(v, p_udp_instance());
      else
         vlog_add_stmt(v, p_hierarchical_instance());
   } while (optional(tCOMMA));

   consume(tSEMI);

   vlog_set_loc(v, CURRENT_LOC);

   vlog_add_stmt(mod, v);
}

static void p_module_or_generate_item(vlog_node_t mod)
{
   // { attribute_instance } parameter_override
   //   | { attribute_instance } gate_instantiation
   //   | { attribute_instance } udp_instantiation
   //   | { attribute_instance } module_instantiation
   //   | { attribute_instance } module_common_item

   BEGIN("module or generate item");

   while (peek() == tATTRBEGIN)
      p_attribute_instance();

   switch (peek()) {
   case tALWAYS:
   case tALWAYSCOMB:
   case tALWAYSFF:
   case tALWAYSLATCH:
   case tWIRE:
   case tUWIRE:
   case tSUPPLY0:
   case tSUPPLY1:
   case tTRI:
   case tTRIAND:
   case tTRIOR:
   case tREG:
   case tSTRUCT:
   case tUNION:
   case tASSIGN:
   case tINITIAL:
   case tTYPEDEF:
   case tENUM:
   case tSVINT:
   case tINTEGER:
   case tSVREAL:
   case tSHORTREAL:
   case tREALTIME:
   case tTIME:
   case tTASK:
   case tFUNCTION:
   case tLOCALPARAM:
   case tPARAMETER:
   case tIF:
   case tEVENT:
      p_module_common_item(mod);
      break;
   case tPULLDOWN:
   case tPULLUP:
   case tAND:
   case tNAND:
   case tOR:
   case tNOR:
   case tXOR:
   case tXNOR:
   case tNOT:
   case tBUF:
      p_gate_instantiation(mod);
      break;
   case tID:
      {
         vlog_node_t ref = peek_reference();
         if (ref == NULL)
            p_module_or_udp_instantiation(mod);
         else
            p_module_common_item(mod);
      }
      break;
   default:
      one_of(tALWAYS, tALWAYSCOMB, tALWAYSFF, tALWAYSLATCH, tWIRE, tUWIRE,
             tSUPPLY0, tSUPPLY1, tTRI, tTRIAND, tTRIOR, tREG, tSTRUCT, tUNION,
             tASSIGN, tINITIAL, tTYPEDEF, tENUM, tSVINT, tINTEGER, tSVREAL,
             tSHORTREAL, tREALTIME, tTIME, tTASK, tFUNCTION, tLOCALPARAM,
             tPARAMETER, tIF, tPULLDOWN, tPULLUP, tID, tAND, tNAND, tOR, tNOR,
             tXOR, tXNOR, tNOT, tBUF, tEVENT);
      drop_tokens_until(tSEMI);
   }
}

static void p_generate_region(vlog_node_t mod)
{
   // generate { generate_item } endgenerate

   BEGIN("generate region");

   // Has no real meaning in System Verilog

   // TODO: generate regions do not nest so check mod is V_MODULE

   consume(tGENERATE);

   while (not_at_token(tENDGENERATE))
      p_generate_item(mod);

   consume(tENDGENERATE);
}

static void p_non_port_module_item(vlog_node_t mod)
{
   // generate_region | module_or_generate_item | specify_block
   //   | { attribute_instance } specparam_declaration | program_declaration
   //   | module_declaration | interface_declaration | timeunits_declaration

   BEGIN("non-port module item");

   switch (peek()) {
   case tALWAYS:
   case tALWAYSCOMB:
   case tALWAYSFF:
   case tALWAYSLATCH:
   case tWIRE:
   case tUWIRE:
   case tSUPPLY0:
   case tSUPPLY1:
   case tTRI:
   case tTRIAND:
   case tTRIOR:
   case tREG:
   case tSTRUCT:
   case tUNION:
   case tASSIGN:
   case tINITIAL:
   case tPULLDOWN:
   case tPULLUP:
   case tID:
   case tATTRBEGIN:
   case tAND:
   case tNAND:
   case tOR:
   case tNOR:
   case tXOR:
   case tXNOR:
   case tNOT:
   case tBUF:
   case tTYPEDEF:
   case tENUM:
   case tSVINT:
   case tINTEGER:
   case tSVREAL:
   case tSHORTREAL:
   case tREALTIME:
   case tTIME:
   case tTASK:
   case tFUNCTION:
   case tLOCALPARAM:
   case tPARAMETER:
   case tEVENT:
   case tIF:
      p_module_or_generate_item(mod);
      break;
   case tSPECIFY:
      vlog_add_stmt(mod, p_specify_block());
      break;
   case tGENERATE:
      p_generate_region(mod);
      break;
   default:
      one_of(tALWAYS, tALWAYSCOMB, tALWAYSFF, tALWAYSLATCH, tWIRE, tUWIRE,
             tSUPPLY0, tSUPPLY1, tTRI, tTRIAND, tTRIOR, tREG, tSTRUCT, tUNION,
             tASSIGN, tPULLDOWN, tPULLUP, tID, tATTRBEGIN, tAND, tNAND, tOR,
             tNOR, tXOR, tXNOR, tNOT, tBUF, tTYPEDEF, tENUM, tSVINT, tINTEGER,
             tSVREAL,  tSHORTREAL, tREALTIME, tTIME, tTASK, tFUNCTION,
             tLOCALPARAM, tPARAMETER, tEVENT, tIF, tSPECIFY, tGENERATE);
      drop_tokens_until(tSEMI);
   }
}

static void p_module_item(vlog_node_t mod)
{
   // port_declaration ; | non_port_module_item

   BEGIN("module item");

   if (scan(tINOUT, tINPUT, tOUTPUT)) {
      p_port_declaration(mod);
      consume(tSEMI);
   }
   else
      p_non_port_module_item(mod);
}

static void p_ansi_port_declaration(vlog_node_t mod, v_port_kind_t *kind,
                                    bool *isreg)
{
   // [ net_port_header | interface_port_header ] port_identifier
   //     { unpacked_dimension } [ = constant_expression ]
   // | [ variable_port_header ] port_identifier { variable_dimension }
   //     [ = constant_expression ]
   // | [ port_direction ] . port_identifier ( [ expression ] )

   BEGIN("ANSI port declaration");

   vlog_node_t dt;
   if (peek() != tID)
      dt = p_net_port_header(kind, isreg);
   else
      dt = logic_type();

   ident_t id, ext;
   p_external_identifier(&id, &ext);

   vlog_node_t v = vlog_new(V_PORT_DECL);
   vlog_set_subkind(v, *kind);
   vlog_set_ident(v, id);
   vlog_set_ident2(v, ext);
   vlog_set_type(v, dt);
   vlog_set_loc(v, &state.last_loc);

   vlog_add_decl(mod, v);
   vlog_symtab_put(symtab, v);

   if (*isreg) {
      vlog_node_t reg = vlog_new(V_VAR_DECL);
      vlog_set_loc(reg, CURRENT_LOC);
      vlog_set_ident(reg, id);
      vlog_set_type(reg, dt);

      vlog_add_decl(mod, reg);
      vlog_symtab_put(symtab, reg);
   }

   vlog_node_t ref = vlog_new(V_REF);
   vlog_set_loc(ref, CURRENT_LOC);
   vlog_set_ident(ref, id);
   vlog_set_ref(ref, v);

   vlog_add_port(mod, ref);
}

static void p_list_of_port_declarations(vlog_node_t mod)
{
   // ( [ { attribute_instance } ansi_port_declaration
   //   { , { attribute_instance } ansi_port_declaration } ] )

   BEGIN("list of port declarations");

   consume(tLPAREN);

   if (peek() != tRPAREN) {
      v_port_kind_t kind = V_PORT_INPUT;
      bool isreg = false;
      do {
         while (peek() == tATTRBEGIN)
            p_attribute_instance();

         p_ansi_port_declaration(mod, &kind, &isreg);
      } while (optional(tCOMMA));
   }

   consume(tRPAREN);
}

static void p_parameter_port_declaration(vlog_node_t mod)
{
   // parameter_declaration
   //    | local_parameter_declaration
   //    | data_type list_of_param_assignments
   //    | type list_of_type_assignments

   BEGIN("parameter port declaration");

   switch (peek()) {
   case tPARAMETER:
      p_parameter_declaration(mod);
      break;
   case tLOCALPARAM:
      p_local_parameter_declaration(mod);
      break;
   default:
      // TODO: Add parsing of "type" declarations example #(type T = bit)
      {
         vlog_node_t datatype = p_data_type();
         p_list_of_param_assignments(mod, datatype, V_PARAM_DECL);
      }
      break;
   }
}

static void p_parameter_port_list(vlog_node_t mod)
{
   // # ( list_of_param_assignments { , parameter_port_declaration } )
   //    | # ( parameter_port_declaration { , parameter_port_declaration } )
   //    | # ( )

   BEGIN("parameter port list");

   consume(tHASH);
   consume(tLPAREN);

   if (peek() != tRPAREN) {
      do {
         if (peek() == tID)
            p_list_of_param_assignments(mod, NULL, V_PARAM_DECL);
         else
            p_parameter_port_declaration(mod);
      } while(optional(tCOMMA));
   }

   consume(tRPAREN);
}

static void p_module_ansi_header(vlog_node_t mod)
{
   // { attribute_instance } module_keyword [ lifetime ] module_identifier
   //    { package_import_declaration } [ parameter_port_list ]
   ///   [ list_of_port_declarations ] ;

   EXTEND("module ANSI header");

   if (peek() == tHASH) {
      p_parameter_port_list(mod);
      param_kind = V_LOCALPARAM;
   }

   if (peek() == tLPAREN)
      p_list_of_port_declarations(mod);

   consume(tSEMI);

   vlog_set_loc(mod, CURRENT_LOC);
}

static vlog_node_t p_port_reference(void)
{
   // port_identifier constant_select

   BEGIN("port reference");

   vlog_node_t v = vlog_new(V_REF);
   vlog_set_ident(v, p_identifier());
   vlog_set_loc(v, CURRENT_LOC);
   return v;
}

static vlog_node_t p_port_expression(void)
{
   // port_reference | { port_reference { , port_reference } }

   BEGIN("port expression");

   return p_port_reference();
}

static vlog_node_t p_port(void)
{
   // [ port_expression ] | . port_identifier ( [ port_expression ] )

   BEGIN("port");

   return p_port_expression();
}

static void p_list_of_ports(vlog_node_t mod)
{
   // ( port { , port } )

   BEGIN("list of ports");

   consume(tLPAREN);

   do {
      p_port();
   } while (optional(tCOMMA));

   consume(tRPAREN);
}

static void p_module_nonansi_header(vlog_node_t mod)
{
   // { attribute_instance } module_keyword [ lifetime ] module_identifier
   //    { package_import_declaration } [ parameter_port_list ] list_of_ports ;

   EXTEND("module non-ANSI header");

   p_list_of_ports(mod);

   consume(tSEMI);

   vlog_set_loc(mod, CURRENT_LOC);
}

static vlog_node_t p_module_declaration(void)
{
   // module_nonansi_header [ timeunits_declaration ] { module_item }
   //      endmodule [ : module_identifier ]
   //   | module_ansi_header [ timeunits_declaration ] { non_port_module_item }
   //      endmodule [ : module_identifier ]
   //   | { attribute_instance } module_keyword [ lifetime ] module_identifier
   //      ( .* ) ; [ timeunits_declaration ] { module_item } endmodule
   //      [ : module_identifier ]
   //   | extern module_nonansi_header
   //   | extern module_ansi_header

   BEGIN("module declaration");

   vlog_node_t mod = vlog_new(V_MODULE);

   while (peek() == tATTRBEGIN)
      p_attribute_instance();

   consume(tMODULE);

   ident_t id, ext;
   p_external_identifier(&id, &ext);
   vlog_set_ident2(mod, id);

   vlog_set_loc(mod, &state.last_loc);

   ident_t qual = ident_prefix(lib_name(lib_work()), ext, '.');
   vlog_set_ident(mod, qual);

   vlog_symtab_push(symtab, mod);

   if (peek() == tLPAREN && peek_nth(2) == tID)
      p_module_nonansi_header(mod);
   else
      p_module_ansi_header(mod);

   while (not_at_token(tENDMODULE))
      p_module_item(mod);

   consume(tENDMODULE);

   vlog_symtab_pop(symtab);
   return mod;
}

static void p_udp_port_list(vlog_node_t udp)
{
   // output_port_identifier , input_port_identifier { , input_port_identifier }

   BEGIN("UDP port list");

   vlog_node_t oref = vlog_new(V_REF);
   vlog_set_ident(oref, p_identifier());
   vlog_set_loc(oref, &state.last_loc);

   vlog_add_port(udp, oref);

   consume(tCOMMA);

   vlog_node_t iref = vlog_new(V_REF);
   vlog_set_ident(iref, p_identifier());
   vlog_set_loc(iref, &state.last_loc);

   vlog_add_port(udp, iref);

   while (optional(tCOMMA)) {
      vlog_node_t iref = vlog_new(V_REF);
      vlog_set_ident(iref, p_identifier());
      vlog_set_loc(iref, &state.last_loc);

      vlog_add_port(udp, iref);
   }
}

static vlog_node_t p_udp_nonansi_declaration(void)
{
   // { attribute_instance } primitive udp_identifier ( udp_port_list ) ;

   BEGIN("UDP non-ANSI declaration");

   vlog_node_t udp = vlog_new(V_PRIMITIVE);

   consume(tPRIMITIVE);

   ident_t id, ext;
   p_external_identifier(&id, &ext);
   vlog_set_ident2(udp, id);

   ident_t qual = ident_prefix(lib_name(lib_work()), ext, '.');
   vlog_set_ident(udp, qual);

   consume(tLPAREN);

   p_udp_port_list(udp);

   consume(tRPAREN);
   consume(tSEMI);

   vlog_set_loc(udp, CURRENT_LOC);
   return udp;
}

static void p_list_of_udp_port_identifiers(vlog_node_t udp, v_port_kind_t kind)
{
   // port_identifier { , port_identifier }

   BEGIN("list of UDP port identifiers");

   do {
      ident_t id, ext;
      p_external_identifier(&id, &ext);

      vlog_node_t p = vlog_new(V_PORT_DECL);
      vlog_set_subkind(p, kind);
      vlog_set_ident(p, id);
      vlog_set_ident2(p, ext);
      vlog_set_type(p, logic_type());
      vlog_set_loc(p, &state.last_loc);

      vlog_add_decl(udp, p);
      vlog_symtab_put(symtab, p);
   } while (optional(tCOMMA));
}

static void p_udp_output_declaration(vlog_node_t udp, bool *has_reg)
{
   // { attribute_instance } output port_identifier
   //    | { attribute_instance } output reg port_identifier
   //         [ = constant_expression ]

   BEGIN("UDP output declaration");

   consume(tOUTPUT);

   const bool isreg = optional(tREG);

   ident_t id, ext;
   p_external_identifier(&id, &ext);

   vlog_node_t logic = logic_type();

   vlog_node_t v = vlog_new(V_PORT_DECL);
   vlog_set_subkind(v, V_PORT_OUTPUT);
   vlog_set_ident(v, id);
   vlog_set_ident2(v, ext);
   vlog_set_type(v, logic);
   vlog_set_loc(v, &state.last_loc);

   vlog_add_decl(udp, v);
   vlog_symtab_put(symtab, v);

   if (isreg) {
      vlog_node_t reg = vlog_new(V_VAR_DECL);
      vlog_set_loc(reg, &state.last_loc);
      vlog_set_ident(reg, id);
      vlog_set_type(reg, logic);

      vlog_add_decl(udp, reg);
      vlog_symtab_put(symtab, reg);

      *has_reg = true;
   }
}

static void p_udp_input_declaration(vlog_node_t udp)
{
   // { attribute_instance } input list_of_udp_port_identifiers

   BEGIN("UDP input declaration");

   consume(tINPUT);

   p_list_of_udp_port_identifiers(udp, V_PORT_INPUT);
}

static vlog_node_t p_udp_reg_declaration(void)
{
   // { attribute_instance } reg variable_identifier

   BEGIN("UDP reg declaration");

   consume(tREG);

   ident_t id = p_identifier();

   vlog_node_t reg = vlog_new(V_VAR_DECL);
   vlog_set_loc(reg, &state.last_loc);
   vlog_set_ident(reg, id);
   vlog_set_type(reg, logic_type());

   return reg;
}

static void p_udp_port_declaration(vlog_node_t udp, bool *has_reg)
{
   // udp_output_declaration ; | udp_input_declaration ; | udp_reg_declaration ;

   BEGIN("UDP port declaration");

   switch (peek()) {
   case tOUTPUT:
      p_udp_output_declaration(udp, has_reg);
      break;
   case tINPUT:
      p_udp_input_declaration(udp);
      break;
   case tREG:
      {
         vlog_node_t v = p_udp_reg_declaration();
         vlog_add_decl(udp, v);
         vlog_symtab_put(symtab, v);
         *has_reg = true;
      }
      break;
   default:
      one_of(tOUTPUT, tINPUT, tREG);
      break;
   }

   consume(tSEMI);
}

static void p_udp_declaration_port_list(vlog_node_t udp, bool *has_reg)
{
   // udp_output_declaration , udp_input_declaration { , udp_input_declaration }

   BEGIN("UDP declaration port list");

   p_udp_output_declaration(udp, has_reg);

   consume(tCOMMA);

   do {
      p_udp_input_declaration(udp);
   } while (optional(tCOMMA));

   const int ndecls = vlog_decls(udp);
   for (int i = 0; i < ndecls; i++) {
      vlog_node_t p = vlog_decl(udp, i);
      if (vlog_kind(p) != V_PORT_DECL)
         continue;

      vlog_node_t ref = vlog_new(V_REF);
      vlog_set_loc(ref, vlog_loc(p));
      vlog_set_ident(ref, vlog_ident(p));
      vlog_set_ref(ref, p);

      vlog_add_port(udp, ref);
   }
}

static vlog_node_t p_output_symbol(void)
{
   // 0 | 1 | x | X

   BEGIN("output symbol");

   vlog_node_t v = vlog_new(V_UDP_LEVEL);
   vlog_set_subkind(v, V_UDP_SYMBOL_OUTPUT);

   if (consume(tUDPLEVEL)) {
      switch (state.last_lval.i64) {
      case '0':
      case '1':
      case 'x':
      case 'X':
         vlog_set_ival(v, state.last_lval.i64);
         break;
      default:
         parse_error(&state.last_loc, "'%c' is not a valid output symbol",
                  (char)state.last_lval.i64);
         break;
      }
   }

   vlog_set_loc(v, CURRENT_LOC);
   return v;
}

static vlog_node_t p_level_symbol(void)
{
   // 0 | 1 | x | X | ? | b | B

   BEGIN("level symbol");

   consume(tUDPLEVEL);

   vlog_node_t v = vlog_new(V_UDP_LEVEL);
   vlog_set_subkind(v, V_UDP_SYMBOL_INPUT);
   vlog_set_ival(v, state.last_lval.i64);

   vlog_set_loc(v, CURRENT_LOC);
   return v;
}

static vlog_node_t p_next_state(void)
{
   // output_symbol | -

   BEGIN("next state");

   switch (peek()) {
   case tMINUS:
      consume(tMINUS);
      break;
   case tUDPLEVEL:
      return p_output_symbol();
   default:
      one_of(tUDPLEVEL, tMINUS);
   }

   vlog_node_t v = vlog_new(V_UDP_LEVEL);
   vlog_set_subkind(v, V_UDP_SYMBOL_OUTPUT);
   vlog_set_ival(v, '-');

   vlog_set_loc(v, CURRENT_LOC);
   return v;
}

static vlog_node_t p_edge_symbol(void)
{
   // r | R | f | F | p | P | n | N | *

   BEGIN("edge symbol");

   consume(tUDPEDGE);

   char left, right;
   switch (state.last_lval.i64) {
   case 'r': case 'R': left = '0'; right = '1'; break;
   case 'f': case 'F': left = '1'; right = '0'; break;
   case 'p': case 'P': left = 'p'; right = 'p'; break;
   case 'n': case 'N': left = 'n'; right = 'N'; break;
   case '*':           left = '?'; right = '?'; break;
   }

   vlog_node_t lsym = vlog_new(V_UDP_LEVEL);
   vlog_set_ival(lsym, left);

   vlog_node_t rsym = vlog_new(V_UDP_LEVEL);
   vlog_set_ival(rsym, right);

   vlog_node_t v = vlog_new(V_UDP_EDGE);
   vlog_set_subkind(v, V_UDP_SYMBOL_INPUT);
   vlog_set_left(v, lsym);
   vlog_set_right(v, rsym);

   vlog_set_loc(v, CURRENT_LOC);
   return v;
}

static void p_level_input_list(vlog_node_t entry)
{
   // level_symbol { level_symbol }

   BEGIN("level input list");

   do {
      vlog_add_param(entry, p_level_symbol());
   } while (not_at_token(tCOLON));
}

static vlog_node_t p_edge_indicator(void)
{
   // ( level_symbol level_symbol ) | edge_symbol

   BEGIN("edge indicator");

   switch (peek()) {
   case tUDPEDGE:
      return p_edge_symbol();
   case tLPAREN:
      {
         consume(tLPAREN);

         vlog_node_t v = vlog_new(V_UDP_EDGE);
         vlog_set_left(v, p_level_symbol());
         vlog_set_right(v, p_level_symbol());

         consume(tRPAREN);

         vlog_set_loc(v, CURRENT_LOC);
         return v;
      }
      break;
   default:
      should_not_reach_here();
   }
}

static void p_seq_input_list(vlog_node_t entry)
{
   // level_input_list | edge_input_list

   BEGIN("sequential input list");

   bool have_edge = false;
   do {
      switch (peek()) {
      case tUDPEDGE:
      case tLPAREN:
         vlog_add_param(entry, p_edge_indicator());
         if (have_edge)
            parse_error(&state.last_loc, "a sequential input list may have at "
                        "most one edge indicator");
         have_edge = true;
         break;

      case tUDPLEVEL:
         vlog_add_param(entry, p_level_symbol());
         break;

      default:
         one_of(tUDPEDGE, tUDPIND, tUDPLEVEL);
         break;
      }
   } while (not_at_token(tCOLON));
}

static vlog_node_t p_combinational_entry(void)
{
   // level_input_list : output_symbol ;

   BEGIN("combinational entry");

   vlog_node_t v = vlog_new(V_UDP_ENTRY);
   p_level_input_list(v);

   consume(tCOLON);

   vlog_add_param(v, p_output_symbol());

   consume(tSEMI);

   vlog_set_loc(v, CURRENT_LOC);
   return v;
}

static vlog_node_t p_combinational_body(void)
{
   // table combinational_entry { combinational_entry } endtable

   BEGIN("combinational UDP body");

   consume(tTABLE);

   scan_as_udp();

   vlog_node_t v = vlog_new(V_UDP_TABLE);
   vlog_set_subkind(v, V_UDP_COMB);
   vlog_set_ident(v, ident_new("combinational"));

   do {
      vlog_add_param(v, p_combinational_entry());
   } while (not_at_token(tENDTABLE));

   scan_as_verilog();

   consume(tENDTABLE);

   vlog_set_loc(v, CURRENT_LOC);
   return v;
}

static vlog_node_t p_sequential_entry(void)
{
   // seq_input_list : current_state : next_state ;

   BEGIN("sequential entry");

   vlog_node_t v = vlog_new(V_UDP_ENTRY);
   p_seq_input_list(v);

   consume(tCOLON);

   vlog_add_param(v, p_level_symbol());

   consume(tCOLON);

   vlog_add_param(v, p_next_state());

   consume(tSEMI);

   vlog_set_loc(v, CURRENT_LOC);
   return v;
}

static vlog_node_t p_udp_initial_statement(void)
{
   // initial output_port_identifier = init_val ;

   BEGIN("UDP initial statement");

   consume(tINITIAL);

   vlog_node_t ref = vlog_new(V_REF);
   vlog_set_ident(ref, p_identifier());
   vlog_set_loc(ref, &state.last_loc);

   vlog_symtab_lookup(symtab, ref);

   consume(tEQ);

   vlog_node_t v = vlog_new(V_BASSIGN);
   vlog_set_target(v, ref);
   vlog_set_value(v, p_integral_number());

   consume(tSEMI);

   vlog_set_loc(v, CURRENT_LOC);
   return v;
}

static vlog_node_t p_sequential_body(void)
{
   // [ udp_initial_statement ] table sequential_entry
   //     { sequential_entry } endtable

   BEGIN("sequential UDP body");

   vlog_node_t v = vlog_new(V_UDP_TABLE);
   vlog_set_subkind(v, V_UDP_SEQ);
   vlog_set_ident(v, ident_new("sequential"));

   if (peek() == tINITIAL)
      vlog_add_stmt(v, p_udp_initial_statement());

   consume(tTABLE);

   scan_as_udp();

   do {
      vlog_add_param(v, p_sequential_entry());
   } while (not_at_token(tENDTABLE));

   scan_as_verilog();

   consume(tENDTABLE);

   vlog_set_loc(v, CURRENT_LOC);
   return v;
}

static vlog_node_t p_udp_body(bool has_reg)
{
   // combinational_body | sequential_body

   BEGIN("UDP body");

   if (has_reg)
      return p_sequential_body();
   else
      return p_combinational_body();
}

static vlog_node_t p_udp_ansi_declaration(bool *has_reg)
{
   // { attribute_instance } primitive udp_identifier
   //    ( udp_declaration_port_list ) ;

   BEGIN("UDP ANSI declaration");

   while (peek() == tATTRBEGIN)
      p_attribute_instance();

   vlog_node_t udp = vlog_new(V_PRIMITIVE);

   consume(tPRIMITIVE);

   ident_t id, ext;
   p_external_identifier(&id, &ext);
   vlog_set_ident2(udp, id);

   ident_t qual = ident_prefix(lib_name(lib_work()), ext, '.');
   vlog_set_ident(udp, qual);

   vlog_symtab_push(symtab, udp);

   consume(tLPAREN);

   p_udp_declaration_port_list(udp, has_reg);

   consume(tRPAREN);
   consume(tSEMI);

   vlog_set_loc(udp, CURRENT_LOC);
   return udp;
}

static vlog_node_t p_udp_declaration(void)
{
   // udp_nonansi_declaration udp_port_declaration { udp_port_declaration }
   //        udp_body endprimitive [ : udp_identifier ]
   //   | udp_ansi_declaration udp_body endprimitive [ : udp_identifier ]
   //   | extern udp_nonansi_declaration
   //   | extern udp_ansi_declaration
   //   | { attribute_instance } primitive udp_identifier ( .* ) ;
   //        { udp_port_declaration } udp_body endprimitive [ : udp_identifier ]

   BEGIN("UDP declaration");

   bool has_reg = false;
   vlog_node_t udp;
   if (peek_nth(4) == tID) {
      udp = p_udp_nonansi_declaration();

      vlog_symtab_push(symtab, udp);

      do {
         p_udp_port_declaration(udp, &has_reg);
      } while (not_at_token(tTABLE, tINITIAL));

      const int nports = vlog_ports(udp);
      for (int i = 0; i < nports; i++)
         vlog_symtab_lookup(symtab, vlog_port(udp, i));
   }
   else
      udp = p_udp_ansi_declaration(&has_reg);

   vlog_add_stmt(udp, p_udp_body(has_reg));

   vlog_symtab_pop(symtab);

   consume(tENDPRIMITIVE);

   return udp;
}

static vlog_node_t p_description(void)
{
   // module_declaration | udp_declaration | interface_declaration
   //   | program_declaration | package_declaration
   //   | { attribute_instance } package_item
   //   | { attribute_instance } bind_directive
   //   | config_declaration

   BEGIN("description");

   switch (peek()) {
   case tMODULE:
      return p_module_declaration();
   case tPRIMITIVE:
      return p_udp_declaration();
   default:
      expect(tPRIMITIVE, tMODULE);
      return NULL;
   }
}

static void p_timescale_compiler_directive(void)
{
   // `timescale time_unit / time_precision

   BEGIN("timescale compiler directive");

   consume(tTIMESCALE);

   consume(tUNSIGNED);
   char *unit_value LOCAL = state.last_lval.str;

   consume(tID);
   char *unit_name LOCAL = state.last_lval.str;

   consume(tOVER);

   consume(tUNSIGNED);
   char *prec_value LOCAL = state.last_lval.str;

   consume(tID);
   char *prec_name LOCAL = state.last_lval.str;

   set_timescale(unit_value, unit_name, prec_value, prec_name, CURRENT_LOC);
}

static void p_defaultnettype_compiler_directive(void)
{
   // `default_nettype wire | tri | tri0 | tri1 | wand | triand | wor
   //    | trior | trireg | uwire | none

   BEGIN("default_nettype directive");

   consume(tDEFNETTYPE);

   switch (one_of(tWIRE, tTRI, tTRI0, tTRI1, tWAND, tTRIAND, tWOR, tTRIOR,
                  tTRIREG, tUWIRE, tNONE)) {
   case tWIRE: implicit_kind = V_NET_WIRE; break;
   case tNONE: implicit_kind = V_NET_NONE; break;
   }
}

static void p_keywords_directive(void)
{
   // `begin_keywords "version_specifier"

   BEGIN("keywords directive");

   consume(tBEGINKEYWORDS);

   consume(tSTRING);
   free(state.last_lval.str);
}

static void p_endkeywords_directive(void)
{
   // `end_keywords

   BEGIN("endkeywords directive");

   consume(tENDKEYWORDS);
}

static void p_resetall_directive(void)
{
   // `resetall

   BEGIN("resetall directive");

   consume(tRESETALL);

   implicit_kind = V_NET_WIRE;
}

static void p_directive_list(void)
{
   BEGIN("directive list");

   for (;;) {
      switch (peek()) {
      case tDEFNETTYPE:
         p_defaultnettype_compiler_directive();
         break;
      case tTIMESCALE:
         p_timescale_compiler_directive();
         break;
      case tBEGINKEYWORDS:
         p_keywords_directive();
         break;
      case tENDKEYWORDS:
         p_endkeywords_directive();
         break;
      case tRESETALL:
         p_resetall_directive();
         break;
      default:
         return;
      }
   }
}

static vlog_node_t end_of_file(void)
{
   vlog_symtab_pop(symtab);
   vlog_symtab_free(symtab);
   symtab = NULL;
   return NULL;
}

vlog_node_t vlog_parse(void)
{
   state.n_correct = RECOVER_THRESH;
   param_kind = V_PARAM_DECL;

   scan_as_verilog();

   if (symtab == NULL) {
      symtab = vlog_symtab_new();
      vlog_symtab_push(symtab, NULL);   // Compilation unit scope
      vlog_symtab_poison(symtab, error_marker());
   }

   if (peek() == tEOF)
      return end_of_file();

   p_directive_list();

   make_new_arena();

   if (peek() == tEOF)
      return end_of_file();

   return p_description();
}

void reset_verilog_parser(void)
{
   state.tokenq_head = state.tokenq_tail = 0;
   implicit_kind = V_NET_WIRE;

   if (symtab != NULL) {
      vlog_symtab_free(symtab);
      symtab = NULL;
   }
}
