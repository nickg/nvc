//
//  Copyright (C) 2024  Nick Gasson
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

static parse_state_t state;

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

   consume(tUNSIGNED);
   free(state.last_lval.str);

   consume(tDOT);

   consume(tUNSIGNED);
   free(state.last_lval.str);

   vlog_node_t v = vlog_new(V_NUMBER);
   vlog_set_loc(v, CURRENT_LOC);
   return v;
}

static vlog_node_t p_number(void)
{
   // integral_number | real_number

   BEGIN("number");

   if (peek_nth(2) == tDOT)
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

   state.last_lval.str[strlen(state.last_lval.str) - 1] = '\0';
   vlog_set_text(v, state.last_lval.str + 1);
   free(state.last_lval.str);

   return v;
}

static vlog_node_t p_primary_literal(void)
{
   // number | time_literal | unbased_unsized_literal | string_literal

   BEGIN("primary literal");

   switch (peek()) {
   case tNUMBER:
   case tUNSIGNED:
      return p_number();
   case tSTRING:
      return p_string_literal();
   default:
      one_of(tNUMBER, tUNSIGNED, tSTRING);
      return dummy_expression();
   }
}

static vlog_node_t p_constant_bit_select(ident_t id)
{
   // { [ constant_expression ] }

   EXTEND("constant bit select");

   if (peek() == tLSQUARE) {
      vlog_node_t v = vlog_new(V_BIT_SELECT);
      vlog_set_ident(v, id);

      while (optional(tLSQUARE)) {
         vlog_add_param(v, p_constant_expression());
         consume(tRSQUARE);
      }

      vlog_set_loc(v, CURRENT_LOC);
      return v;
   }
   else {
      vlog_node_t v = vlog_new(V_REF);
      vlog_set_ident(v, id);
      vlog_set_loc(v, CURRENT_LOC);
      return v;
   }
}

static vlog_node_t p_constant_select(ident_t id)
{
   // [ { . member_identifier constant_bit_select } . member_identifier ]
   //    constant_bit_select [ [ constant_part_select_range ] ]

   EXTEND("constant select");

   return p_constant_bit_select(id);
}

static vlog_node_t p_constant_primary(void)
{
   // primary_literal | ps_parameter_identifier constant_select
   //   | specparam_identifier [ [ constant_range_expression ] ]
   //   | genvar_identifier | formal_port_identifier constant_select
   //   | [ package_scope | class_scope ] enum_identifier
   //   | constant_concatenation [ [ constant_range_expression ] ]
   //   | constant_multiple_concatenation [ [ constant_range_expression ] ]
   //   | constant_function_call | constant_let_expression
   //   | ( constant_mintypmax_expression )
   //   | constant_cast | constant_assignment_pattern_expression
   //   | type_reference

   BEGIN("constant primary");

   switch (peek()) {
   case tNUMBER:
   case tUNSIGNED:
   case tSTRING:
      return p_primary_literal();
   case tID:
      return p_constant_select(p_identifier());
   default:
      one_of(tNUMBER, tUNSIGNED, tSTRING, tID);
      return dummy_expression();
   }
}

static vlog_node_t p_constant_expression(void)
{
   // constant_primary | unary_operator { attribute_instance } constant_primary
   //   | constant_expression binary_operator { attribute_instance }
   //       constant_expression
   //   | constant_expression ? { attribute_instance }
   //       constant_expression : constant_expression

   BEGIN("constant expression");

   return p_constant_primary();
}

static void p_constant_range(vlog_node_t *left, vlog_node_t *right)
{
   // constant_expression : constant_expression

   BEGIN("constant range");

   *left = p_constant_expression();

   consume(tCOLON);

   *right = p_constant_expression();
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

static void p_data_type(vlog_node_t *packed)
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

   consume(tREG);    // TODO

   if (peek() == tLSQUARE)
      *packed = p_packed_dimension();
}

static void p_implicit_data_type(vlog_node_t *packed)
{
   // [ signing ] { packed_dimension }

   BEGIN("implicit data type");

   if (peek() == tLSQUARE)
      *packed = p_packed_dimension();
}

static void p_data_type_or_implicit(bool *isreg, vlog_node_t *packed)
{
   // data_type | implicit_data_type

   BEGIN("data type or implicit");

   if (scan(tREG)) {
      p_data_type(packed);
      *isreg = true;
   }
   else
      p_implicit_data_type(packed);
}

static void p_net_port_type(bool *isreg, vlog_node_t *packed)
{
   // [ net_type ] data_type_or_implicit | net_type_identifier
   //   | interconnect implicit_data_type

   BEGIN("net port type");

   // TODO: rewrite this to store datatype as node

   p_data_type_or_implicit(isreg, packed);
}

static void p_list_of_port_identifiers(vlog_node_t mod, v_port_kind_t kind,
                                       bool isreg, vlog_node_t packed)
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
      vlog_set_loc(v, &state.last_loc);
      if (packed != NULL)
         vlog_add_range(v, packed);

      vlog_add_decl(mod, v);

      if (isreg) {
         vlog_node_t reg = vlog_new(V_VAR_DECL);
         vlog_set_loc(reg, vlog_loc(v));
         vlog_set_ident(reg, id);

         vlog_add_decl(mod, reg);
      }

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

   bool isreg = false;
   vlog_node_t packed = NULL;
   p_net_port_type(&isreg, &packed);

   p_list_of_port_identifiers(mod, V_PORT_INOUT, isreg, packed);
}

static void p_input_declaration(vlog_node_t mod)
{
   // input net_port_type list_of_port_identifiers
   //   | input variable_port_type list_of_variable_identifiers

   BEGIN("input declaration");

   consume(tINPUT);

   bool isreg = false;
   vlog_node_t packed = NULL;
   p_net_port_type(&isreg, &packed);

   p_list_of_port_identifiers(mod, V_PORT_INPUT, isreg, packed);
}

static void p_output_declaration(vlog_node_t mod)
{
   // output net_port_type list_of_port_identifiers
   //   | output variable_port_type list_of_variable_port_identifiers

   BEGIN("output declaration");

   consume(tOUTPUT);

   bool isreg = false;
   vlog_node_t packed = NULL;
   p_net_port_type(&isreg, &packed);

   p_list_of_port_identifiers(mod, V_PORT_OUTPUT, isreg, packed);
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

static void p_net_port_header(v_port_kind_t *kind, bool *isreg,
                              vlog_node_t *packed)
{
   // [ port_direction ] net_port_type

   BEGIN("net port header");

   if (optional(tINPUT)) {
      *kind = V_PORT_INPUT;
      *isreg = false;
      *packed = NULL;
   }
   else if (optional(tINOUT)) {
      *kind = V_PORT_INOUT;
      *isreg = false;
      *packed = NULL;
   }
   else if (optional(tOUTPUT)) {
      *kind = V_PORT_OUTPUT;
      *isreg = false;
      *packed = NULL;
   }

   p_net_port_type(isreg, packed);
}

static vlog_node_t p_bit_select(ident_t id)
{
   // { [ expression ] }

   EXTEND("bit select");

   if (peek() == tLSQUARE) {
      vlog_node_t v = vlog_new(V_BIT_SELECT);
      vlog_set_ident(v, id);

      while (optional(tLSQUARE)) {
         vlog_add_param(v, p_expression());
         consume(tRSQUARE);
      }

      vlog_set_loc(v, CURRENT_LOC);
      return v;
   }
   else {
      vlog_node_t v = vlog_new(V_REF);
      vlog_set_ident(v, id);
      vlog_set_loc(v, CURRENT_LOC);
      return v;
   }
}


static vlog_node_t p_select(ident_t id)
{
   // [ { . member_identifier bit_select } . member_identifier ]
   //    bit_select [ [ part_select_range ] ]

   EXTEND("select");

   return p_bit_select(id);
}

static void p_list_of_arguments(vlog_node_t call)
{
   // [ expression ] { , [ expression ] } { , . identifier ( [ expression ] ) }
   //    | . identifier ( [ expression ] ) { , . identifier ( [ expression ] ) }

   BEGIN("list of arguments");

   do {
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

static vlog_node_t p_subroutine_call(vlog_kind_t kind)
{
   // tf_call | system_tf_call | method_call | [ std:: ] randomize_call

   BEGIN("subroutine call");

   return p_system_tf_call(kind);
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
      return p_select(p_identifier());
   case tSTRING:
   case tNUMBER:
   case tUNSIGNED:
      return p_primary_literal();
   case tSYSTASK:
      return p_subroutine_call(V_SYSFUNC);
   default:
      one_of(tID, tSTRING, tNUMBER, tUNSIGNED, tSYSTASK);
      return p_select(error_marker());
   }
}

static vlog_binary_t p_binary_operator(void)
{
   // + | - | * | / | % | == | != | === | !== | ==? | !=? | && | ||
   //  | ** | < | <= | > | >= | & | | | ^ | ^~ | ~^ | >> | <<
   //  | >>> | <<< | -> | <->

   BEGIN("binary operator");

   switch (one_of(tBAR, tPLUS, tAMP, tCASEEQ, tCASENEQ)) {
   case tBAR: return V_BINARY_OR;
   case tAMP: return V_BINARY_AND;
   case tCASEEQ: return V_BINARY_CASE_EQ;
   case tCASENEQ: return V_BINARY_CASE_NEQ;
   case tPLUS:
   default: return V_BINARY_PLUS;
   }
}
static vlog_unary_t p_unary_operator(void)
{
   // + | - | ! | ~ | & | ~& | | | ~| | ^ | ~^ | ^~

   BEGIN("unary operator");

   switch (one_of(tMINUS, tPLUS, tTILDE, tBANG)) {
   case tMINUS: return V_UNARY_NEG;
   case tTILDE: return V_UNARY_BITNEG;
   case tBANG: return V_UNARY_NOT;
   case tPLUS:
   default: return V_UNARY_IDENTITY;
   }
}

static vlog_node_t p_expression(void)
{
   // primary | unary_operator { attribute_instance } primary
   //   | inc_or_dec_expression | ( operator_assignment )
   //   | expression binary_operator { attribute_instance } expression
   //   | conditional_expression | inside_expression | tagged_union_expression

   BEGIN("expression");

   vlog_node_t head;
   switch (peek()) {
   case tID:
   case tSTRING:
   case tNUMBER:
   case tUNSIGNED:
   case tSYSTASK:
      head = p_primary();
      break;
   case tMINUS:
   case tTILDE:
   case tBANG:
      head = vlog_new(V_UNARY);
      vlog_set_subkind(head, p_unary_operator());
      vlog_set_value(head, p_primary());
      vlog_set_loc(head, CURRENT_LOC);
      break;
   default:
      one_of(tID, tSTRING, tNUMBER, tUNSIGNED, tMINUS, tTILDE, tBANG, tSYSTASK);
      return p_select(error_marker());
   }

   if (scan(tBAR, tPLUS, tAMP, tCASEEQ, tCASENEQ)) {
      vlog_node_t v = vlog_new(V_BINARY);
      vlog_set_subkind(v, p_binary_operator());
      vlog_set_left(v, head);
      vlog_set_right(v, p_expression());

      vlog_set_loc(v, CURRENT_LOC);
      return v;
   }
   else
      return head;
}

static vlog_node_t p_event_expression(void)
{
   // [ edge_identifier ] expression [ iff expression ]
   //   | sequence_instance [ iff expression ]
   //   | event_expression or event_expression
   //   | event_expression , event_expression
   //   | ( event_expression )

   BEGIN("event expression");

   vlog_node_t v = vlog_new(V_EVENT);

   if (optional(tPOSEDGE))
      vlog_set_subkind(v, V_EVENT_POSEDGE);
   else if (optional(tNEGEDGE))
      vlog_set_subkind(v, V_EVENT_NEGEDGE);
   else
      vlog_set_subkind(v, V_EVENT_LEVEL);

   vlog_set_value(v, p_expression());

   vlog_set_loc(v, CURRENT_LOC);
   return v;
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
   consume(tLPAREN);

   vlog_node_t v = p_event_expression();

   consume(tRPAREN);

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

   ident_t id = p_identifier();
   vlog_node_t v = p_constant_select(id);

   vlog_set_loc(v, CURRENT_LOC);
   return v;
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

   vlog_node_t v = vlog_new(V_SEQ_BLOCK);

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

static vlog_node_t p_subroutine_call_statement(void)
{
   // subroutine_call ; | void ' ( function_subroutine_call ) ;

   BEGIN("subroutine call statement");

   vlog_node_t v = p_subroutine_call(V_SYSTASK);

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

   switch (one_of(tFOREVER)) {
   case tFOREVER:
      {
         vlog_node_t v = vlog_new(V_FOREVER);

         vlog_node_t s = p_statement_or_null();
         if (s != NULL)
            vlog_add_stmt(v, s);

         vlog_set_loc(v, CURRENT_LOC);
         return v;
      }

   default:
      should_not_reach_here();
   }
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
   case tSYSTASK:
      return p_subroutine_call_statement();
   case tIF:
      return p_conditional_statement();
   case tFOREVER:
      return p_loop_statement();
   default:
      one_of(tID, tAT, tHASH, tBEGIN, tSYSTASK, tIF, tFOREVER);
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
      return vlog_new(V_SEQ_BLOCK);

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

   consume(tALWAYS);

   vlog_node_t v = vlog_new(V_ALWAYS);

   char *name LOCAL = xasprintf("__always#line%d", yylloc.first_line);
   vlog_set_ident(v, ident_uniq(name));

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

   char *name LOCAL = xasprintf("__initial#line%d", yylloc.first_line);
   vlog_set_ident(v, ident_uniq(name));

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

   vlog_node_t v = vlog_new(V_ASSIGN);
   vlog_set_target(v, p_net_lvalue());

   char *name LOCAL = xasprintf("__assign#line%d", state.last_loc.first_line);
   vlog_set_ident(v, ident_uniq(name));

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

   switch (one_of(tWIRE, tSUPPLY0, tSUPPLY1)) {
   case tSUPPLY0: return V_NET_SUPPLY0;
   case tSUPPLY1: return V_NET_SUPPLY1;
   case tWIRE:
   default: return V_NET_WIRE;
   }
}

static vlog_node_t p_net_decl_assignment(vlog_net_kind_t kind, bool isreg,
                                         vlog_node_t packed)
{
   // net_identifier { unpacked_dimension } [ = expression ]

   BEGIN("net declaration assignment");

   vlog_node_t v = vlog_new(V_NET_DECL);
   vlog_set_subkind(v, kind);
   vlog_set_ident(v, p_identifier());

   if (packed != NULL)
      vlog_add_range(v, packed);

   vlog_set_loc(v, CURRENT_LOC);
   return v;
}

static void p_list_of_net_decl_assignments(vlog_node_t mod,
                                           vlog_net_kind_t kind, bool isreg,
                                           vlog_node_t packed)
{
   // net_decl_assignment { , net_decl_assignment }

   BEGIN("list of net declaration assignments");

   do {
      vlog_node_t v = p_net_decl_assignment(kind, isreg, packed);
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

   bool isreg = false;
   vlog_node_t packed = NULL;
   p_data_type_or_implicit(&isreg, &packed);
   assert(!isreg);

   p_list_of_net_decl_assignments(mod, kind, isreg, packed);

   consume(tSEMI);
}

static vlog_node_t p_variable_decl_assignment(vlog_node_t packed)
{
   // variable_identifier { variable_dimension } [ = expression ]
   //   | dynamic_array_variable_identifier unsized_dimension
   //       { variable_dimension } [ = dynamic_array_new ]
   //   | class_variable_identifier [ = class_new ]

   BEGIN("variable declaration assignment");

   vlog_node_t v = vlog_new(V_VAR_DECL);
   vlog_set_ident(v, p_identifier());

   if (packed != NULL)
      vlog_add_range(v, packed);

   vlog_set_loc(v, CURRENT_LOC);
   return v;
}

static void p_list_of_variable_decl_assignments(vlog_node_t mod,
                                                vlog_node_t packed)
{
   // variable_decl_assignment { , variable_decl_assignment }

   BEGIN("list of variable declaration assignments");

   do {
      vlog_add_decl(mod, p_variable_decl_assignment(packed));
   } while (optional(tCOMMA));
}

static void p_data_declaration(vlog_node_t mod)
{
   // [ const ] [ var ] [ lifetime ] data_type_or_implicit
   //     list_of_variable_decl_assignments ;
   //  | type_declaration | package_import_declaration | net_type_declaration

   BEGIN("data declaration");

   bool isreg = false;
   vlog_node_t packed = NULL;
   p_data_type_or_implicit(&isreg, &packed);
   assert(isreg);

   p_list_of_variable_decl_assignments(mod, packed);

   consume(tSEMI);
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
   case tSUPPLY0:
   case tSUPPLY1:
      p_net_declaration(mod);
      break;
   case tREG:
      p_data_declaration(mod);
      break;
   default:
      one_of(tWIRE, tREG, tSUPPLY0, tSUPPLY1);
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
      vlog_add_stmt(mod, p_always_construct());
      break;
   case tINITIAL:
      vlog_add_stmt(mod, p_initial_construct());
      break;
   case tWIRE:
   case tSUPPLY0:
   case tSUPPLY1:
   case tREG:
      p_module_or_generate_item_declaration(mod);
      break;
   case tASSIGN:
      p_continuous_assign(mod);
      break;
   default:
      one_of(tALWAYS, tINITIAL, tWIRE, tSUPPLY0, tSUPPLY1, tREG, tASSIGN);
   }
}

static vlog_node_t p_strength0(void)
{
   // supply0 | strong0 | pull0 | weak0

   BEGIN("strength0");

   vlog_strength_t kind;
   switch (one_of(tSUPPLY0)) {
   case tSUPPLY0:
   default:
      kind = V_STRENGTH_SUPPLY0;
      break;
   }

   vlog_node_t v = vlog_new(V_STRENGTH);
   vlog_set_subkind(v, kind);
   vlog_set_loc(v, CURRENT_LOC);
   return v;
}

static vlog_node_t p_strength1(void)
{
   // supply1 | strong1 | pull1 | weak1

   BEGIN("strength1");

   vlog_strength_t kind;
   switch (one_of(tSUPPLY1)) {
   case tSUPPLY1:
   default:
      kind = V_STRENGTH_SUPPLY1;
      break;
   }

   vlog_node_t v = vlog_new(V_STRENGTH);
   vlog_set_subkind(v, kind);
   vlog_set_loc(v, CURRENT_LOC);
   return v;
}

static void p_pulldown_strength(vlog_node_t *p0, vlog_node_t *p1)
{
   // ( strength0 , strength1 ) | ( strength1 , strength0 ) | ( strength0 )

   BEGIN("pulldown strength");

   consume(tLPAREN);

   switch (peek()) {
   case tSUPPLY1:
      *p0 = p_strength1();
      consume(tCOMMA);
      *p1 = p_strength0();
      break;
   default:
      *p0 = p_strength0();
      if (optional(tCOMMA))
         *p1 = p_strength1();
      break;
   }

   consume(tRPAREN);
}

static void p_pullup_strength(vlog_node_t *p0, vlog_node_t *p1)
{
   // ( strength0 , strength1 ) | ( strength1 , strength0 ) | ( strength1 )

   BEGIN("pullup strength");

   consume(tLPAREN);

   switch (peek()) {
   case tSUPPLY0:
      *p0 = p_strength0();
      consume(tCOMMA);
      *p1 = p_strength1();
      break;
   default:
      *p0 = p_strength1();
      if (optional(tCOMMA))
         *p1 = p_strength0();
      break;
   }

   consume(tRPAREN);
}

static vlog_node_t p_pull_gate_instance(vlog_gate_kind_t kind, vlog_node_t p0,
                                        vlog_node_t p1)
{
   // [ name_of_instance ] ( output_terminal )

   BEGIN("pull gate instance");

   vlog_node_t v = vlog_new(V_GATE_INST);
   vlog_set_subkind(v, kind);

   if (p0 != NULL)
      vlog_add_param(v, p0);

   if (p1 != NULL)
      vlog_add_param(v, p1);

   if (peek() == tID)
      vlog_set_ident(v, p_identifier());

   consume(tLPAREN);

   vlog_set_target(v, p_net_lvalue());

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

   if (peek() == tID)
      vlog_set_ident(v, p_identifier());

   consume(tLPAREN);

   vlog_set_target(v, p_net_lvalue());

   consume(tCOMMA);

   do {
      vlog_add_param(v, p_net_lvalue());
   } while (optional(tCOMMA));

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
         vlog_node_t p0 = NULL, p1 = NULL;
         if (peek() == tLPAREN && peek_nth(2) != tID)
            p_pulldown_strength(&p0, &p1);

         do {
            vlog_node_t g = p_pull_gate_instance(V_GATE_PULLDOWN, p0, p1);
            vlog_add_stmt(mod, g);
         } while (optional(tCOMMA));
      }
      break;

   case tPULLUP:
      {
         vlog_node_t p0 = NULL, p1 = NULL;
         if (peek() == tLPAREN && peek_nth(2) != tID)
            p_pullup_strength(&p0, &p1);

         do {
            vlog_node_t g = p_pull_gate_instance(V_GATE_PULLUP, p0, p1);
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

static void p_specify_terminal_descriptor(void)
{
   // identifier [ [ constant_range_expression ] ]

   BEGIN("specify terminal descriptor");

   p_identifier();
}

static void p_parallel_path_description(void)
{
   // ( specify_input_terminal_descriptor [ polarity_operator ] =>
   //     specify_output_terminal_descriptor )

   BEGIN("parallel path description");

   consume(tLPAREN);

   p_specify_terminal_descriptor();

   consume(tASSOC);

   p_specify_terminal_descriptor();

   consume(tRPAREN);
}

static void p_simple_path_declaration(void)
{
   // parallel_path_description = path_delay_value
   //   | full_path_description = path_delay_value

   BEGIN("simple path declaration");

   p_parallel_path_description();

   consume(tEQ);

   p_path_delay_value();
}

static void p_path_declaration(void)
{
   // simple_path_declaration ; | edge_sensitive_path_declaration ;
   //   | state_dependent_path_declaration ;

   BEGIN("path declaration");

   p_simple_path_declaration();

   consume(tSEMI);
}

static void p_specify_item(void)
{
   // specparam_declaration | pulsestyle_declaration | showcancelled_declaration
   //   | path_declaration | system_timing_check

   BEGIN("specify item");

   switch (peek()) {
   case tLPAREN:
      p_path_declaration();
      break;
   default:
      one_of(tLPAREN);
   }
}

static vlog_node_t p_specify_block(void)
{
   // specify { specify_item } endspecify

   BEGIN("specify block");

   consume(tSPECIFY);

   vlog_node_t v = vlog_new(V_SPECIFY);

   while (not_at_token(tENDSPECIFY))
      p_specify_item();

   consume(tENDSPECIFY);

   vlog_set_loc(v, CURRENT_LOC);
   return v;
}

static vlog_node_t p_ordered_port_connection(void)
{
   // { attribute_instance } [ expression ]

   BEGIN("ordered port connection");

   return p_expression();
}

static void p_list_of_port_connections(vlog_node_t inst)
{
   // ordered_port_connection { , ordered_port_connection }
   //   | named_port_connection { , named_port_connection }

   BEGIN("list of port connections");

   do {
      vlog_add_param(inst, p_ordered_port_connection());
   } while (optional(tCOMMA));
}

static vlog_node_t p_hierarchical_instance(ident_t module_id)
{
   // name_of_instance ( [ list_of_port_connections ] )

   BEGIN("hierarchical instance");

   vlog_node_t v = vlog_new(V_MOD_INST);
   vlog_set_ident(v, p_identifier());
   vlog_set_ident2(v, module_id);

   consume(tLPAREN);

   if (peek() != tRPAREN)
      p_list_of_port_connections(v);

   consume(tRPAREN);

   vlog_set_loc(v, CURRENT_LOC);
   return v;
}

static void p_module_instantiation(vlog_node_t mod)
{
   // module_identifier [ parameter_value_assignment ] hierarchical_instance
   //   { , hierarchical_instance } ;

   BEGIN("module instantiation");

   ident_t module_id = p_identifier();

   do {
      vlog_add_stmt(mod, p_hierarchical_instance(module_id));
   } while (optional(tCOMMA));

   consume(tSEMI);
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
   case tWIRE:
   case tSUPPLY0:
   case tSUPPLY1:
   case tREG:
   case tASSIGN:
   case tINITIAL:
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
      p_module_instantiation(mod);
      break;
   default:
      one_of(tALWAYS, tWIRE, tSUPPLY0, tSUPPLY1, tREG, tASSIGN, tPULLDOWN,
             tPULLUP, tID, tAND, tNAND, tOR, tNOR, tXOR, tXNOR, tNOT, tBUF);
   }
}

static void p_non_port_module_item(vlog_node_t mod)
{
   // generate_region | module_or_generate_item | specify_block
   //   | { attribute_instance } specparam_declaration | program_declaration
   //   | module_declaration | interface_declaration | timeunits_declaration

   BEGIN("non-port module item");

   switch (peek()) {
   case tALWAYS:
   case tWIRE:
   case tSUPPLY0:
   case tSUPPLY1:
   case tREG:
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
      p_module_or_generate_item(mod);
      break;
   case tSPECIFY:
      vlog_add_stmt(mod, p_specify_block());
      break;
   default:
      one_of(tALWAYS, tWIRE, tSUPPLY0, tSUPPLY1, tREG, tASSIGN, tPULLDOWN,
             tPULLUP, tSPECIFY, tID, tATTRBEGIN, tAND, tNAND, tOR, tNOR,
             tXOR, tXNOR, tNOT, tBUF);
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
                                    bool *isreg, vlog_node_t *packed)
{
   // [ net_port_header | interface_port_header ] port_identifier
   //     { unpacked_dimension } [ = constant_expression ]
   // | [ variable_port_header ] port_identifier { variable_dimension }
   //     [ = constant_expression ]
   // | [ port_direction ] . port_identifier ( [ expression ] )

   BEGIN("ANSI port declaration");

   if (peek() != tID)
      p_net_port_header(kind, isreg, packed);

   ident_t id, ext;
   p_external_identifier(&id, &ext);

   vlog_node_t v = vlog_new(V_PORT_DECL);
   vlog_set_subkind(v, *kind);
   vlog_set_ident(v, id);
   vlog_set_ident2(v, ext);
   vlog_set_loc(v, &state.last_loc);

   if (*packed != NULL)
      vlog_add_range(v, *packed);

   vlog_add_decl(mod, v);

   if (*isreg) {
      vlog_node_t reg = vlog_new(V_VAR_DECL);
      vlog_set_loc(reg, CURRENT_LOC);
      vlog_set_ident(reg, id);

      vlog_add_decl(mod, reg);
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
      vlog_node_t packed = NULL;
      bool isreg = false;
      do {
         p_ansi_port_declaration(mod, &kind, &isreg, &packed);
      } while (optional(tCOMMA));
   }

   consume(tRPAREN);
}

static void p_module_ansi_header(vlog_node_t mod)
{
   // { attribute_instance } module_keyword [ lifetime ] module_identifier
   //    { package_import_declaration } [ parameter_port_list ]
   ///   [ list_of_port_declarations ] ;

   EXTEND("module ANSI header");

   if (peek() == tLPAREN)
      p_list_of_port_declarations(mod);

   consume(tSEMI);

   vlog_set_loc(mod, CURRENT_LOC);
}

static vlog_node_t p_port_reference(void)
{
   // port_identifier constant_select

   BEGIN("port reference");

   return p_constant_select(p_identifier());
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

   ident_t qual = ident_prefix(lib_name(lib_work()), ext, '.');
   vlog_set_ident(mod, qual);

   if (peek_nth(2) == tID)
      p_module_nonansi_header(mod);
   else
      p_module_ansi_header(mod);

   while (not_at_token(tENDMODULE))
      p_module_item(mod);

   consume(tENDMODULE);

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
      vlog_set_loc(p, &state.last_loc);

      vlog_add_decl(udp, p);
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

   vlog_node_t v = vlog_new(V_PORT_DECL);
   vlog_set_subkind(v, V_PORT_OUTPUT);
   vlog_set_ident(v, id);
   vlog_set_ident2(v, ext);
   vlog_set_loc(v, &state.last_loc);

   vlog_add_decl(udp, v);

   if (isreg) {
      vlog_node_t reg = vlog_new(V_VAR_DECL);
      vlog_set_loc(reg, &state.last_loc);
      vlog_set_ident(reg, id);

      vlog_add_decl(udp, reg);

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
      vlog_add_decl(udp, p_udp_reg_declaration());
      *has_reg = true;
      break;
   default:
      one_of(tOUTPUT, tINPUT, tREG);
      break;
   }

   consume(tSEMI);
}

static char p_output_symbol(void)
{
   // 0 | 1 | x | X

   BEGIN("output symbol");

   if (consume(tUDPLEVEL)) {
      switch (state.last_lval.i64) {
      case '0':
      case '1':
      case 'x':
      case 'X':
         return (char)state.last_lval.i64;
      default:
         parse_error(&state.last_loc, "'%c' is not a valid output symbol",
                  (char)state.last_lval.i64);
         break;
      }
   }

   return 'x';
}

static char p_level_symbol(void)
{
   // 0 | 1 | x | X | ? | b | B

   BEGIN("level symbol");

   if (consume(tUDPLEVEL))
      return (char)state.last_lval.i64;
   else
      return 'x';
}

static char p_next_state(void)
{
   // output_symbol | -

   BEGIN("next state");

   switch (peek()) {
   case tMINUS:
      consume(tMINUS);
      return '-';
   case tUDPLEVEL:
      return p_output_symbol();
   default:
      one_of(tUDPLEVEL, tMINUS);
      return '-';
   }
}

static char p_edge_symbol(void)
{
   // r | R | f | F | p | P | n | N | *

   BEGIN("edge symbol");

   if (consume(tUDPEDGE))
      return (char)state.last_lval.i64;
   else
      return '*';
}

static void p_level_input_list(text_buf_t *tb)
{
   // level_symbol { level_symbol }

   BEGIN("level input list");

   do {
      tb_append(tb, p_level_symbol());
   } while (not_at_token(tCOLON));
}

static void p_edge_indicator(text_buf_t *tb)
{
   // ( level_symbol level_symbol ) | edge_symbol

   BEGIN("edge indicator");

   switch (peek()) {
   case tUDPEDGE:
      tb_append(tb, p_edge_symbol());
      break;
   case tUDPIND:
      consume(tUDPIND);
      tb_cat(tb, state.last_lval.str);
      free(state.last_lval.str);
      break;
   default:
      should_not_reach_here();
   }
}

static void p_seq_input_list(text_buf_t *tb)
{
   // level_input_list | edge_input_list

   BEGIN("sequential input list");

   bool have_edge = false;
   do {
      switch (peek()) {
      case tUDPEDGE:
      case tUDPIND:
         p_edge_indicator(tb);
         if (have_edge)
            parse_error(&state.last_loc, "a sequential input list may have at "
                        "most one edge indicator");
         have_edge = true;
         break;

      case tUDPLEVEL:
         tb_append(tb, p_level_symbol());
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

   LOCAL_TEXT_BUF tb = tb_new();
   p_level_input_list(tb);

   consume(tCOLON);
   tb_append(tb, ':');

   tb_append(tb, p_output_symbol());

   vlog_node_t v = vlog_new(V_UDP_ENTRY);
   vlog_set_text(v, tb_get(tb));

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

   LOCAL_TEXT_BUF tb = tb_new();
   p_seq_input_list(tb);

   consume(tCOLON);
   tb_append(tb, ':');

   tb_append(tb, p_level_symbol());

   consume(tCOLON);
   tb_append(tb, ':');

   tb_append(tb, p_next_state());

   vlog_node_t v = vlog_new(V_UDP_ENTRY);
   vlog_set_text(v, tb_get(tb));

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

      do {
         p_udp_port_declaration(udp, &has_reg);
      } while (not_at_token(tTABLE, tINITIAL));
   }
   else
      abort();  // TODO

   vlog_add_stmt(udp, p_udp_body(has_reg));

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

static void p_directive_list(void)
{
   BEGIN("directive list");

   for (;;) {
      switch (peek()) {
      case tTIMESCALE:
         p_timescale_compiler_directive();
         break;
      default:
         return;
      }
   }
}

vlog_node_t vlog_parse(void)
{
   state.n_correct = RECOVER_THRESH;

   scan_as_verilog();

   if (peek() == tEOF)
      return NULL;

   make_new_arena();

   p_directive_list();

   return p_description();
}

void reset_verilog_parser(void)
{
   state.tokenq_head = state.tokenq_tail = 0;
}
