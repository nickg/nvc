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
#include "scan.h"
#include "sdf/sdf-phase.h"
#include "sdf/sdf-util.h"

#include <string.h>
#include <stdlib.h>
#include <assert.h>
#include <stdio.h>
#include <math.h>

// Lexer symbols
extern yylval_t yylval;
extern loc_t yylloc;

// Global state, currently parsed file
static sdf_file_t *sdf_file = NULL;

static parse_state_t state;

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

#define BEGIN(s)                                         \
   EXTEND(s);                                            \
   state.start_loc = LOC_INVALID;                        \

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

static ident_t error_marker(void)
{
   return well_known(W_ERROR);
}

///////////////////////////////////////////////////////////////////////////////
// VLOG copy-paste ends here!
///////////////////////////////////////////////////////////////////////////////

#define PARSE_MIN_DELAYS      (!!(sdf_file->min_max_spec & S_F_MIN_VALUES))
#define PARSE_TYP_DELAYS      (!!(sdf_file->min_max_spec & S_F_TYP_VALUES))
#define PARSE_MAX_DELAYS      (!!(sdf_file->min_max_spec & S_F_MAX_VALUES))

static inline bool is_next_tok_signed_number(void)
{
   return scan(tINT, tREAL, tPLUS, tMINUS);
}

#define EPSILON 0.0000000000001

static bool doubles_equal(double a, double b)
{
   if (fabs(a - b) < EPSILON)
      return true;
   return false;
}

static ident_t p_identifier(void)
{
   // identifier ::=
   //       character { character }

   BEGIN("identifier");

   ident_t id;
   if (consume(tID)) {
      id = ident_new(state.last_lval.str);
      free(state.last_lval.str);
   }
   else
      id = error_marker();

   return id;
}

static ident_t p_qstring(void)
{
   // qstring ::=
   //       " { any_character } "

   BEGIN("qstring");

   // qstring identifier is stored including the surrounding
   // apostrophes.
   consume(tSTRING);
   ident_t str = ident_new(state.last_lval.str);
   free(state.last_lval.str);

   return str;
}

static ident_t p_hierarchical_identifier(void)
{
   // hierarchical_identifier ::=
   //          identifier { hchar identifier }

   BEGIN("hierarchical identifier");

   scan_as_sdf_expr();

   ident_t id = p_identifier();

   // Hierarchical identifier is purposefully converted to '.' separator.
   // This-way, "ident_walk_selected" can be used to walk the hierarchy.
   // To dump the identifier, dumper walks the ID and replaces '.' by
   // actual hierarchy separator to get to the original source!
   while (scan(tDOT, tOVER)) {
      int tok = peek();
      consume(tok);

      if (tok != sdf_file->hchar)
         warn_at(&state.last_loc, "Used hierarchy separator: %c "
                  "but hierarchy separator defined in SDF header is: %c",
                  tok, sdf_file->hchar);

      id = ident_prefix(id, p_identifier(), '.');
   }

   scan_as_sdf();

   return id;
}

static int64_t p_integer(void)
{
   // integer ::=
   //       decimal_digit { decimal_digit }

   BEGIN("integer")

   consume(tINT);

   return yylval.i64;
}

static double p_real_number(void)
{
   // real_number ::=
   //       integer
   //     | integer [ . integer ]
   //     | integer [ . integer ] e [ sign ] integer

   BEGIN("real number");

   switch (one_of(tINT, tREAL)) {
   case tINT:
      return (double)yylval.i64;
   case tREAL:
      return yylval.real;
   default:
      return 0.0;
   }
}

static double p_signed_real_number(void)
{
   // signed_real_number ::=
   //       [ sign ] real_number

   BEGIN("signed real number");

   switch (peek()) {
   case tMINUS:
      consume(tMINUS);
      return -p_real_number();
   case tPLUS:
      consume(tPLUS);
      // Fall-through
   default:
      return p_real_number();
   }
}

static void p_scalar_constant(void)
{
   // scalar_constant ::=
   //       0     // logical zero
   //     | 'b0
   //     | 'B0
   //     | 1'b0
   //     | 1'B0
   //     | 1     // logical one
   //     | 'b1
   //     | 'B1
   //     | 1'b1
   //     | 1'B1

   BEGIN("scalar constant");

   int64_t val = 0;

   switch (one_of(tINT, tSCALARZERO, tSCALARONE)) {
   case tINT:
      val = yylval.i64;
      if (val != 1 && val != 0)
         parse_error(&state.last_loc, "scalar_constant shall be one of: "
                                       "1'b0, 1'B0, 'b0, 'B0, 0, "
                                       "1'b1, 1'B1, 'b1, 'B1, 1");
      break;
   case tSCALARONE:
      val = 1;
      break;
   case tSCALARZERO:
      val = 0;
      break;
   }
}

static void p_scalar_node(void)
{
   // scalar_node ::=
   //       scalar_port
   //     | scalar_net
   // scalar_port ::=
   //       hierarchical_identifier
   //     | hierarchical_identifier [ integer ]
   // scalar_net ::=
   //       hierarchical_identifier
   //     | hierarchical_identifier [ integer ]

   BEGIN("scalar node");

   p_hierarchical_identifier();

   if (optional(tLSQUARE)) {
      p_integer();
      consume(tRSQUARE);
   }
}

static void p_port_instance(void)
{
   // port_instance ::=
   //       port
   //     | hierarchical_identifier hchar port
   // port ::=
   //       scalar_port
   //     | bus_port
   // scalar_port ::=
   //       hierarchical_identifier
   //     | hierarchical_identifier [ integer ]
   // bus_port ::=
   //       hierarchical_identifier [ integer : integer ]
   // hierarchical_identifier ::=
   //       identifier { hchar identifier }
   //
   // Note:
   //    "port_instance" is equivalent to "port" because "hierarchical_identifier hchar port"
   //    production can completeyl dervied from "port" by:
   //         port -> scalar_port -> hierarchical_identifier -> identifier { hchar identifier }
   //
   //         Therefore "hierarchical_identifier hchar port" is givan as:
   //             <----hierarchical_identifier---->       <--port-->
   //             {identifier { hchar identifier }} hchar identifier

   BEGIN("port instance");

   p_hierarchical_identifier();

   if (optional(tLSQUARE)) {
      p_integer();

      if (optional(tCOLON))
         p_integer();

      consume(tRSQUARE);
   }
}

static void p_port_or_scalar_constant(void)
{
   if (scan(tINT, tSCALARONE, tSCALARZERO))
      return p_scalar_constant();

   return p_port_instance();
}

static void p_port_edge(void)
{
   // port_edge ::=
   //       ( edge_identifier port_instance )

   BEGIN("port edge");

   consume(tLPAREN);

   // TODO: Implement other edge identifier types!
   one_of(tPOSEDGE, tNEGEDGE);

   p_port_instance();

   consume(tRPAREN);
}

static void p_port_spec(void)
{
   // port_spec ::=
   //          port_instance
   //        | port_edge

   BEGIN("port spec");

   if (scan(tLPAREN))
      return p_port_edge();

   return p_port_instance();
}

static inline sdf_binary_expr_kind_t p_binary_operator_optional(void)
{
   // binary_operator ::=
   //         +   // arithmetic sum
   //       | -   // arithmetic difference
   //       | *   // arithmetic product
   //       | /   // arithmetic quotient
   //       | %   // modulus
   //       | ==  // logical equality
   //       | !=  // logical inequality
   //       | === // case equality
   //       | !== // case inequality
   //       | &&  // logical AND
   //       | ||  // logical OR
   //       | <   // relational
   //       | <=  // relational
   //       | >   // relational
   //       | >=  // relational
   //       | &   // bit-wise binary AND
   //       | |   // bit-wise binary inclusive OR
   //       | ^   // bit-wise binary exclusive OR
   //       | ^~  // bit-wise binary equivalence
   //       | ~^  // bit-wise binary equivalence (alternative)
   //       | >>  // right shift
   //       | <<  // left shift

   BEGIN("binary operator")

   sdf_binary_expr_kind_t rv = S_BINARY_EXPR_NONE;

   int tok = peek();
   switch (tok) {
   case tPLUS:
      rv = S_BINARY_EXPR_PLUS;
      break;
   case tMINUS:
      rv = S_BINARY_EXPR_MINUS;
      break;
   case tTIMES:
      rv = S_BINARY_EXPR_MULT;
      break;
   case tOVER:
      rv = S_BINARY_EXPR_DIV;
      break;
   case tLOGEQ:
      rv = S_BINARY_EXPR_LOGEQ;
      break;
   case tLOGNEQ:
      rv = S_BINARY_EXPR_LOGNEQ;
      break;
   case tCASEEQ:
      rv = S_BINARY_EXPR_CASEEQ;
      break;
   case tCASENEQ:
      rv = S_BINARY_EXPR_CASENEQ;
      break;
   case tDBLAMP:
      rv = S_BINARY_EXPR_LOGAND;
      break;
   case tLOGOR:
      rv = S_BINARY_EXPR_LOGOR;
      break;
   case tLT:
      rv = S_BINARY_EXPR_LT;
      break;
   case tGT:
      rv = S_BINARY_EXPR_GT;
      break;
   case tLE:
      rv = S_BINARY_EXPR_LTEQ;
      break;
   case tGE:
      rv = S_BINARY_EXPR_GTEQ;
      break;
   case tAMP:
      rv = S_BINARY_EXPR_BITAND;
      break;
   case tBAR:
      rv = S_BINARY_EXPR_BITOR;
      break;
   case tCARET:
      rv = S_BINARY_EXPR_BITXOR;
      break;
   case tPERCENT:
      rv = S_BINARY_EXPR_MOD;
      break;
   case tTILDECARET:
      rv = S_BINARY_EXPR_BITXNOR;
      break;
   case tLTLT:
      rv = S_BINARY_EXPR_SHLEFT;
      break;
   case tGTGT:
      rv = S_BINARY_EXPR_SHRIGHT;
      break;
   }

   if (rv != S_BINARY_EXPR_NONE)
      consume(tok);

   return rv;
}

static sdf_unary_expr_kind_t p_unary_operator_optional(void)
{
   // unary_operator ::=
   //          +  // arithmetic identity
   //        | -  // arithmetic negation
   //        | !  // logical negation
   //        | ~  // bit-wise unary negation
   //        | &  // reduction unary AND
   //        | ~& // reduction unary NAND
   //        | |  // reduction unary OR
   //        | ~| // reduction unary NOR
   //        | ^  // reduction unary XOR
   //        | ^~ // reduction unary XNOR
   //        | ~^ // reduction unary XNOR (alternative)

   BEGIN("unary operator");

   sdf_unary_expr_kind_t rv = S_UNARY_EXPR_NONE;

   int tok = peek();
   switch (tok) {
   case tPLUS:
      rv = S_UNARY_EXPR_PLUS;
      break;
   case tMINUS:
      rv = S_UNARY_EXPR_MINUS;
      break;
   case tBANG:
      rv = S_UNARY_EXPR_LOGNOT;
      break;
   case tTILDE:
      rv = S_UNARY_EXPR_BITNOT;
      break;
   case tCARET:
      rv = S_UNARY_EXPR_XOR;
      break;
   case tBAR:
      rv = S_UNARY_EXPR_OR;
      break;
   case tAMP:
      rv = S_UNARY_EXPR_AND;
      break;
   case tTILDEAMP:
      rv = S_UNARY_EXPR_NAND;
      break;
   case tTILDEBAR:
      rv = S_UNARY_EXPR_NOR;
      break;
   case tTILDECARET:
      rv = S_UNARY_EXPR_XNOR;
      break;
   }

   if (rv != S_UNARY_EXPR_NONE)
      consume(tok);

   return rv;
}

static void p_simple_expression(void)
{
   // simple_expression ::=
   //          ( simple_expression )
   //        | unary_operator ( simple_expression )
   //        | port
   //        | unary_operator port
   //        | scalar_constant
   //        | unary_operator scalar_constant
   //        | simple_expression ? simple_expression : simple_expression
   //        | { simple_expression [ concat_expression ] }
   //        | { simple_expression { simple_expression [ concat_expression ] } }

   BEGIN("simple expression");

   if (optional(tLPAREN)) {
      p_simple_expression();
      consume(tRPAREN);
      return;
   }

   // unary_operator
   sdf_unary_expr_kind_t unary_kind = S_UNARY_EXPR_NONE;
   if ((unary_kind = p_unary_operator_optional()) != S_UNARY_EXPR_NONE) {
      if (optional(tLPAREN)) {
         p_simple_expression();
         consume(tRPAREN);
      }
      else
         p_port_or_scalar_constant();

      return;
   }

   // simple_expression
   if (optional(tLBRACE)) {
      p_simple_expression();

      while (optional(tCOMMA))
         p_simple_expression();

      if (optional(tRBRACE)) {
         p_simple_expression();

         while (optional(tCOMMA))
            p_simple_expression();

         consume(tRBRACE);
      }
      consume(tRBRACE);
   }

   p_port_or_scalar_constant();

   // simple_expression ? simple_expression : simple_expression
   if (optional(tQUESTION)) {
      p_simple_expression();
      consume(tCOLON);
      p_simple_expression();
   }
 }

static void p_conditional_port_expr(void)
{
   // conditional_port_expr ::=
   //          simple_expression
   //      | ( conditional_port_expr )
   //      | unary_operator ( conditional_port_expr )
   //      | conditional_port_expr binary_operator conditional_port_expr

   BEGIN("conditional port expression");

   sdf_unary_expr_kind_t unary_kind = S_UNARY_EXPR_NONE;

   if (optional(tLPAREN)) {
      p_conditional_port_expr();
      consume(tRPAREN);
   }
   // unary_operator
   else if ((unary_kind = p_unary_operator_optional()) != S_UNARY_EXPR_NONE) {
      p_conditional_port_expr();
   }
   // When we consume unary expression and open parenthesis, we get only to
   // "conditional_port_expr" and "simple_expression".
   // "conditional port_expr" is taken care-of by recursion,
   // only "simple_expression" remain
   else
      p_simple_expression();

   // binary_operator
   sdf_binary_expr_kind_t binary_kind = S_BINARY_EXPR_NONE;
   if ((binary_kind = p_binary_operator_optional()) != S_BINARY_EXPR_NONE) {
      p_conditional_port_expr();
   }
}

static void p_timing_check_condition(void)
{
   // timing_check_condition ::=
   //          scalar_node
   //        | inversion_operator scalar_node
   //        | scalar_node equality_operator scalar_constant

   BEGIN("timing check condition");

   // inversion_operator
   if (scan(tBANG, tTILDE)) {
      int tok = peek();
      consume(tok);

      p_scalar_node();

      return;
   }

   p_scalar_node();

   // equality_operator
   if (scan(tLOGEQ, tLOGNEQ, tCASEEQ, tCASENEQ)) {
      int tok = peek();
      consume(tok);

      p_scalar_constant();
   }
}

static void p_neg_pair(void)
{
   // neg_pair ::=
   //       ( negedge signed_real_number [ signed_real_number ] )
   //       ( posedge signed_real_number [ signed_real_number ] )

   BEGIN("neg pair");

   consume (tLPAREN);
   consume (tNEGEDGE);

   p_signed_real_number();

   if (is_next_tok_signed_number())
      p_signed_real_number();

   consume (tRPAREN);

   consume (tLPAREN);
   consume (tPOSEDGE);

   p_signed_real_number();

   if (is_next_tok_signed_number())
      p_signed_real_number();

   consume (tRPAREN);
}

static void p_pos_pair(void)
{
   // pos_pair ::=
   //       ( posedge signed_real_number [ signed_real_number ] )
   //       ( negedge signed_real_number [ signed_real_number ] )

   BEGIN("pos pair");

   consume (tLPAREN);
   consume (tPOSEDGE);

   p_signed_real_number();

   if (is_next_tok_signed_number())
      p_signed_real_number();

   consume (tRPAREN);

   consume (tLPAREN);
   consume (tNEGEDGE);

   p_signed_real_number();

   if (is_next_tok_signed_number())
      p_signed_real_number();

   consume (tRPAREN);
}

static void p_edge_list(void)
{
   // edge_list ::=
   //       pos_pair { pos_pair }
   //     | neg_pair { neg_pair }

   BEGIN("edge list")

   p_pos_pair();
   while (peek_nth(2) == tPOSEDGE)
      p_pos_pair();

   p_neg_pair();
   while (peek_nth(2) == tNEGEDGE)
      p_neg_pair();
}

static ident_t p_cell_instance(void)
{
   // cell_instance ::=
   //       ( INSTANCE [ hierarchical_identifier ] )
   //     | ( INSTANCE * )

   BEGIN("cell instance");

   consume(tLPAREN);
   consume(tINSTANCE);

   ident_t hier = NULL;
   if (scan(tTIMES, tID)) {
      if (optional(tTIMES))
         hier = ident_new("*");
      else
         hier = p_hierarchical_identifier();
   }

   consume(tRPAREN);

   return hier;
}

static void p_real_number_or_tripple(void)
{
   // real_number ::=
   //       integer
   //     | integer [ . integer ]
   //     | integer [ . integer ] e [ sign ] integer
   // triple ::=
   //       real_number   : [ real_number ] : [ real_number ]
   //   | [ real_number ] :   real_number   : [ real_number ]
   //   | [ real_number ] : [ real_number ] :   real_number

   // If parsing tripple one following scenarios can occur:
   //    :
   //    (tINT|tREAL):
   //
   // Detect if parsing tripple or only real_number

   bool is_tripple = false;
   if (scan(tCOLON) || peek_nth(2) == tCOLON)
      is_tripple = true;

   if (is_tripple) {
      bool number_present = false;

      if (not_at_token(tCOLON)) {
         p_real_number();
         number_present = true;
      }

      consume(tCOLON);

      if (not_at_token(tCOLON)) {
         p_real_number();
         number_present = true;
      }

      consume(tCOLON);

      if (not_at_token(tRPAREN)) {
         p_real_number();
         number_present = true;
      }

      if (!number_present)
         parse_error(&state.last_loc,
                     "'tripple' shall have at least one number specified");

      return;
   }

   p_real_number();
}

static void p_value(void)
{
   // value ::=
   //       ( [ real_number ] )
   //     | ( [triple] )

   BEGIN("value");

   consume(tLPAREN);

   if (not_at_token(tRPAREN))
      p_real_number_or_tripple();

   consume(tRPAREN);
}

static void p_signed_real_number_or_rtripple(void)
{
   // signed_real_number ::=
   //       [ sign ] real_number
   // rtriple ::=
   //       signed_real_number   : [ signed_real_number ] : [ signed_real_number ]
   //   | [ signed_real_number ] :   signed_real_number   : [ signed_real_number ]
   //   | [ signed_real_number ] : [ signed_real_number ] :   signed_real_number

   // If parsing rtripple one following scenarios can occur:
   //    :
   //    (tINT|tREAL):
   //    (tPLUS|tMINUS)(tINT|tREAL):
   //
   // Detect if parsing rtripple or only signed_real_number

   bool is_rtripple = false;
   if (scan(tCOLON) || peek_nth(2) == tCOLON || peek_nth(3) == tCOLON)
      is_rtripple = true;

   if (is_rtripple) {
      bool number_present = false;

      if (not_at_token(tCOLON)) {
         p_signed_real_number();
         number_present = true;
      }

      consume(tCOLON);

      if (not_at_token(tCOLON)) {
         p_signed_real_number();
         number_present = true;
      }

      consume(tCOLON);

      if (not_at_token(tRPAREN)) {
         p_signed_real_number();
         number_present = true;
      }

      if (!number_present)
         parse_error(&state.last_loc,
                     "'rtripple' shall have at least one number specified");

      return;
   }

   p_signed_real_number();
}

static void p_rvalue(void)
{
   // rvalue ::=
   //       ( [ signed_real_number ] )
   //     | ( [ rtriple ] )

   BEGIN("rvalue");

   consume(tLPAREN);

   if (not_at_token(tRPAREN))
      p_signed_real_number_or_rtripple();

   consume(tRPAREN);
}

static void p_name(void)
{
   // name ::=
   //       ( NAME [ qstring ] )

   BEGIN("name");

   consume(tLPAREN);
   consume(tNAME);

   if (scan(tSTRING))
      p_qstring();

   consume(tRPAREN);
}

static void p_path_constraint(void)
{
   // path_constraint ::=
   //       ( PATHCONSTRAINT [ name ] port_instance port_instance { port_instance }
   //                        rvalue rvalue )

   BEGIN("path constraint");

   consume(tLPAREN);
   consume(tPATHCONSTR);

   if (peek_nth(2) == tNAME)
      p_name();

   p_port_instance();
   p_port_instance();

   while (scan(tID))
      p_port_instance();

   p_rvalue();
   p_rvalue();

   consume(tRPAREN);
}

static void p_exception(void)
{
   // exception ::=
   //       ( EXCEPTION cell_instance { cell_instance } )

   BEGIN("exception");

   consume(tLPAREN);
   consume(tEXCEPTION);

   p_cell_instance();

   while (peek_nth(2) == tINSTANCE)
      p_cell_instance();

   consume(tRPAREN);
}

static void p_period_constraint(void)
{
   // period_constraint ::=
   //       ( PERIODCONSTRAINT port_instance value [ exception ] )

   BEGIN("period constraint");

   consume(tLPAREN);
   consume(tPERIODCONSTR);

   p_port_instance();
   p_value();

   if (peek_nth(2) == tEXCEPTION)
      p_exception();

   consume(tRPAREN);
}

static void p_constraint_path(void)
{
   // constraint_path ::=
   //       ( port_instance port_instance )

   BEGIN("constraint path");

   consume(tLPAREN);

   p_port_instance();
   p_port_instance();

   consume(tRPAREN);
}

static void p_sum_constraint(void)
{
   // sum_constraint ::=
   //       ( SUM constraint_path constraint_path { constraint_path } rvalue [ rvalue ] )

   BEGIN("sum constraint");

   consume(tLPAREN);
   consume(tSUM);

   p_constraint_path();
   p_constraint_path();

   // Both next follow-up tokens start with tLPAREN.
   // Decide based on second next token
   while (peek_nth(2) == tID)
      p_constraint_path();

   p_rvalue();
   if (scan(tLPAREN))
      p_rvalue();

   consume(tRPAREN);
}

static void p_diff_constraint(void)
{
   // diff_constraint ::=
   //       ( DIFF constraint_path constraint_path value [ value ] )

   BEGIN("diff constraint");

   consume(tLPAREN);
   consume(tDIFF);

   p_constraint_path();
   p_constraint_path();

   p_value();
   if (scan(tLPAREN))
      p_value();

   consume(tRPAREN);
}

static void p_skew_constraint(void)
{
   // skew_constraint ::=
   //       ( SKEWCONSTRAINT port_spec value )

   BEGIN("skew constraint");

   consume(tLPAREN);
   consume(tSKEWCONSTR);

   p_port_spec();
   p_value();

   consume(tRPAREN);
}

static void p_arrival_env(void)
{
   // arrival_env ::=
   //       ( ARRIVAL [ port_edge ] port_instance rvalue rvalue rvalue rvalue )

   BEGIN("arrival env");

   consume(tLPAREN);
   consume(tARRIVAL);

   if (scan(tLPAREN))
      p_port_edge();
   p_port_instance();

   for (int i = 0; i < 4; i++)
      p_rvalue();

   consume(tRPAREN);
}

static void p_departure_env(void)
{
   // departure_env ::=
   //       ( DEPARTURE [ port_edge ] port_instance rvalue rvalue rvalue rvalue )

   BEGIN("departure env");

   consume(tLPAREN);
   consume(tDEPARTURE);

   if (scan(tLPAREN))
      p_port_edge();

   p_port_instance();

   for (int i = 0; i < 4; i++)
      p_rvalue();

   consume(tRPAREN);
}

static void p_slack_env(void)
{
   // slack_env ::=
   //       ( SLACK port_instance rvalue rvalue rvalue rvalue [ real_number ] )

   BEGIN("slack env");

   consume(tLPAREN);
   consume(tSLACK);

   p_port_instance();

   for (int i = 0; i < 4; i++)
      p_rvalue();

   if (scan(tREAL, tINT))
      p_real_number();

   consume(tRPAREN);
}

static void p_waveform_env(void)
{
   // waveform_env ::=
   //       ( WAVEFORM port_instance real_number edge_list )

   BEGIN("waveform env");

   consume(tLPAREN);
   consume(tWAVEFORM);

   p_port_instance();
   p_real_number();

   // sdf_value of S_TENV_KIND_WAVEFORM
   //    0     - real_number
   //    1 - X - Edge pairs

   p_edge_list();

   consume(tRPAREN);
}

static void p_te_spec(void)
{
   // te_spec ::=
   //       ( TIMINGENV te_def { te_def } )

   BEGIN("te spec");

   consume(tLPAREN);
   consume(tTIMINGENV);

   do {
      switch (peek_nth(2)) {
      case tPATHCONSTR:
         p_path_constraint();
         break;
      case tPERIODCONSTR:
         p_period_constraint();
         break;
      case tSUM:
         p_sum_constraint();
         break;
      case tDIFF:
         p_diff_constraint();
         break;
      case tSKEWCONSTR:
         p_skew_constraint();
         break;
      case tARRIVAL:
         p_arrival_env();
         break;
      case tDEPARTURE:
         p_departure_env();
         break;
      case tSLACK:
         p_slack_env();
         break;
      case tWAVEFORM:
         p_waveform_env();
         break;
      default:
         consume(tLPAREN);
         one_of(tPATHCONSTR, tPERIODCONSTR, tSUM, tDIFF, tSKEWCONSTR,
                tARRIVAL, tDEPARTURE, tSLACK, tWAVEFORM);
      }
   } while (scan(tLPAREN));

   consume(tRPAREN);
}

static void p_delval(void)
{
   // delval ::=
   //       rvalue
   //     | ( rvalue rvalue )
   //     | ( rvalue rvalue rvalue )

   BEGIN("delval");

   // Multiple rvalues in delval
   if (peek_nth(2) == tLPAREN) {
      consume(tLPAREN);

      if (scan(tLPAREN))
         p_rvalue();
      if (scan(tLPAREN))
         p_rvalue();
      if (scan(tLPAREN))
         p_rvalue();

      consume(tRPAREN);
   }
   // Single rvalue in delval
   else
      p_rvalue();
}

static void p_delval_list(void)
{
   // delval_list ::=
   //          delval
   //        | delval delval
   //        | delval delval delval
   //        | delval delval delval delval [ delval ] [ delval ]
   //        | delval delval delval delval delval delval delval
   //          [ delval ] [ delval ] [ delval ] [ delval ] [ delval ]

   BEGIN("delval list");

   while (scan(tLPAREN))
      p_delval();

   //   if (sdf_values(delay) > 12)
   //   parse_error(&state.last_loc,
   //               "'delval_list' shall have at most 12 'delval' entries");
}

static void p_lbl_def(sdf_flags_t flag)
{
   // lbl_def ::=
   //          ( identifier delval_list )

   BEGIN("lbl def");

   consume(tLPAREN);

   p_identifier();

   // Reuse delay for current label. Delvals are both "value" for these
   p_delval_list();

   consume(tRPAREN);
}

static void p_lbl_type(void)
{
   // lbl_type ::=
   //       ( INCREMENT lbl_def { lbl_def } )
   //     | ( ABSOLUTE lbl_def { lbl_def } )

   BEGIN("lbl type");

   consume(tLPAREN);

   sdf_flags_t flag;
   if (one_of(tABSOLUTE, tINCREMENT) == tABSOLUTE)
      flag = S_F_VALUE_ABSOLUTE;
   else
      flag = S_F_VALUE_INCREMENT;

   while (scan(tLPAREN))
      p_lbl_def(flag);

   consume(tRPAREN);
}

static void p_lbl_spec(void)
{
   // bl_spec ::=
   //       ( LABEL lbl_type { lbl_type } )

   BEGIN("lbl spec");

   consume(tLPAREN);
   consume(tLABEL);

   do
      p_lbl_type();
   while (scan(tLPAREN));

   consume(tRPAREN);
}

static void p_port_tchk(void)
{
   // port_tchk ::=
   //          port_spec
   //       | ( COND [ qstring ] timing_check_condition port_spec )

   BEGIN("port tcheck");

   if (peek_nth(2) == tSDFCOND) {
      consume(tLPAREN);
      consume(tSDFCOND);

      if (scan(tSTRING))
         p_qstring();

      p_timing_check_condition();
      p_port_spec();

      consume(tRPAREN);

      return;
   }

   return p_port_spec();
}

static void p_nochange_timing_check(void)
{
   // nochange_timing_check ::=
   //          ( NOCHANGE port_tchk port_tchk rvalue rvalue )

   BEGIN("nochange timing check");

   consume(tLPAREN);
   consume(tNOCHANGE);

   p_port_tchk();
   p_port_tchk();
   p_rvalue();
   p_rvalue();

   consume(tRPAREN);
}

static void p_bidirectskew_timing_check(void)
{
   // bidirectskew_timing_check ::=
   //       ( BIDIRECTSKEW port_tchk port_tchk value value )

   BEGIN("bidirectskew timing check");

   consume(tLPAREN);
   consume(tBIDIRSKEW);

   p_port_tchk();
   p_port_tchk();
   p_value();
   p_value();

   consume(tRPAREN);
}

static void p_skew_timing_check(void)
{
   // skew_timing_check ::=
   //       ( SKEW port_tchk port_tchk rvalue )

   BEGIN("skew timing check");

   consume(tLPAREN);
   consume(tSKEW);

   p_port_tchk();
   p_port_tchk();
   p_rvalue();

   consume(tRPAREN);
}

#define DEFINE_1P1V_TCHK(func_name, msg, tok, subkind)                        \
      static void func_name(void)                                             \
      {                                                                       \
         /* width_timing_check ::=                      */                    \
         /*          ( WIDTH port_tchk value )          */                    \
         /* period_timing_check ::=                     */                    \
         /*          ( PERIOD port_tchk value )         */                    \
                                                                              \
         BEGIN(msg);                                                          \
                                                                              \
         consume(tLPAREN);                                                    \
         consume(tok);                                                        \
                                                                              \
         p_port_tchk();                                                       \
         p_value();                                                           \
                                                                              \
         consume(tRPAREN);                                                    \
      }

DEFINE_1P1V_TCHK(p_width_timing_check, "width timing check", tWIDTH, S_TCHECK_WIDTH);
DEFINE_1P1V_TCHK(p_period_timing_check, "period timing check", tPERIOD, S_TCHECK_PERIOD);

static void p_scond_or_ccond(sdf_cond_kind_t kind)
{
   // scond ::=
   //       ( SCOND [ qstring ] timing_check_condition )
   // ccond ::=
   //       ( CCOND [ qstring ] timing_check_condition )

   BEGIN("scond or ccond");

   consume(tLPAREN);
   if (kind == S_COND_CCOND)
      consume(tCCOND);
   else
      consume(tSCOND);

   if (scan(tSTRING))
      p_qstring();

   p_timing_check_condition();

   consume(tRPAREN);

   return;
}

#define DEFINE_2P2RV_TCHK(func_name, msg, tok, subkind)                       \
      static void func_name(void)                                             \
      {                                                                       \
         /* setuphold_timing_check ::=                                 */     \
         /*          ( SETUPHOLD port_tchk port_tchk rvalue rvalue )   */     \
         /*        | ( SETUPHOLD port_spec port_spec rvalue rvalue     */     \
         /*                      [ scond ] [ ccond ] )                 */     \
                                                                              \
         BEGIN(msg);                                                          \
                                                                              \
         consume(tLPAREN);                                                    \
         consume(tok);                                                        \
                                                                              \
         bool cond_seen = false;                                              \
         if (peek_nth(2) == tSDFCOND)                                         \
            cond_seen = true;                                                 \
         p_port_tchk();                                                       \
                                                                              \
         if (peek_nth(2) == tSDFCOND)                                         \
            cond_seen = true;                                                 \
         p_port_tchk();                                                       \
                                                                              \
         p_rvalue();                                                          \
         p_rvalue();                                                          \
                                                                              \
         if (!cond_seen && (peek_nth(2) == tSCOND))                           \
            p_scond_or_ccond(S_COND_SCOND);                                   \
         if (!cond_seen && (peek_nth(2) == tCCOND))                           \
            p_scond_or_ccond(S_COND_CCOND);                                   \
                                                                              \
         consume(tRPAREN);                                                    \
      }

DEFINE_2P2RV_TCHK(p_setuphold_timing_check, "setuphold timing check", tSETUPHOLD, S_TCHECK_SETUPHOLD);
DEFINE_2P2RV_TCHK(p_recrem_timing_check,    "recrem timing check",    tRECREM,    S_TCHECK_RECREM);

#define DEFINE_2P1V_TCHK(func_name, msg, tok, subkind)                        \
      static void func_name(void)                                             \
      {                                                                       \
         /* setup_timing_check ::=                           */               \
         /*          ( SETUP port_tchk port_tchk value )     */               \
         /* hold_timing_check ::=                            */               \
         /*          ( HOLD port_tchk port_tchk value )      */               \
         /* recovery_timing_check ::=                        */               \
         /*          ( RECOVERY port_tchk port_tchk value )  */               \
         /* removal_timing_check ::=                         */               \
         /*          ( REMOVAL port_tchk port_tchk value )   */               \
                                                                              \
         BEGIN(msg);                                                          \
                                                                              \
         consume(tLPAREN);                                                    \
         consume(tok);                                                        \
                                                                              \
         p_port_tchk();                                                       \
         p_port_tchk();                                                       \
         p_value();                                                           \
                                                                              \
         consume(tRPAREN);                                                    \
      }

DEFINE_2P1V_TCHK(p_setup_timing_check,    "setup timing check",   tSETUP,     S_TCHECK_SETUP);
DEFINE_2P1V_TCHK(p_hold_timing_check,     "hold timing check",    tHOLD,      S_TCHECK_HOLD);
DEFINE_2P1V_TCHK(p_recovery_timing_check, "recovery timing check",tRECOVERY,  S_TCHECK_RECOVERY);
DEFINE_2P1V_TCHK(p_removal_timing_check,  "removal timing check", tREMOVAL,   S_TCHECK_REMOVAL);

static void p_tc_spec(void)
{
   // tc_spec ::=
   //       ( TIMINGCHECK tchk_def { tchk_def } )

   BEGIN("tc spec");

   consume(tLPAREN);
   consume(tTIMINGCHECK);

   do {
      switch (peek_nth(2)) {
      case tSETUP:
         p_setup_timing_check();
         break;
      case tHOLD:
         p_hold_timing_check();
         break;
      case tRECOVERY:
         p_recovery_timing_check();
         break;
      case tREMOVAL:
         p_removal_timing_check();
         break;
      case tSETUPHOLD:
         p_setuphold_timing_check();
         break;
      case tRECREM:
         p_recrem_timing_check();
         break;
      case tWIDTH:
         p_width_timing_check();
         break;
      case tPERIOD:
         p_period_timing_check();
         break;
      case tSKEW:
         p_skew_timing_check();
         break;
      case tBIDIRSKEW:
         p_bidirectskew_timing_check();
         break;
      case tNOCHANGE:
         p_nochange_timing_check();
         break;
      default:
         consume(tLPAREN);
         one_of(tSETUP, tHOLD, tSETUPHOLD, tRECREM, tWIDTH,
                tPERIOD, tSKEW, tBIDIRSKEW, tNOCHANGE);
      };
   } while (scan(tLPAREN));

   consume(tRPAREN);
}

static void p_pathpulsepercent_deltype(void)
{
   // pathpulsepercent_deltype ::=
   //       ( PATHPULSEPERCENT [ input_output_path ] value [ value ] )

   BEGIN("pathpulsepercent deltype");

   consume(tLPAREN);
   consume(tPATHPULSEP);

   if (scan(tID)) {
      p_port_instance();
      p_port_instance();
   }

   p_value();
   if (scan(tLPAREN))
      p_value();

   consume(tRPAREN);
}

static void p_pathpulse_deltype(void)
{
   // pathpulse_deltype ::=
   //       ( PATHPULSE [ input_output_path ] value [ value ] )

   BEGIN("pathpulse deltype");

   consume(tLPAREN);
   consume(tPATHPULSE);

   if (scan(tID)) {
      p_port_instance();
      p_port_instance();
   }

   p_value();
   if (scan(tLPAREN))
      p_value();

   consume(tRPAREN);
}

static void p_retain_def(void)
{
   // retain_def ::=
   //       ( RETAIN retval_list )
   // retval_list ::=
   //       delval
   //     | delval delval
   //     | delval delval delval

   BEGIN("retain def");

   consume(tLPAREN);
   consume(tRETAIN);

   while (scan(tLPAREN))
      p_delval();

   //   if (sdf_values(retain) > 3)
   //   parse_error(&state.last_loc,
   //               "'retval_list' shall have at most 3 'delval' entries");

   consume(tRPAREN);

   return;
}

static void p_iopath_def(void)
{
   // iopath_def ::=
   //       ( IOPATH port_spec port_instance { retain_def } delval_list )

   BEGIN("iopath def");

   consume(tLPAREN);
   consume(tIOPATH);

   p_port_spec();
   p_port_instance();

   if (peek_nth(2) == tRETAIN)
      p_retain_def();

   p_delval_list();

   consume(tRPAREN);
}

static void p_condelse_def(void)
{
   // condelse_def ::=
   //       ( CONDELSE iopath_def )

   BEGIN("condelse def");

   consume(tLPAREN);
   consume(tSDFCONDELSE);

   p_iopath_def();

   consume(tRPAREN);
}

static void p_cond_def(void)
{
   // cond_def ::=
   //       ( COND [ qstring ] conditional_port_expr iopath_def )

   BEGIN("cond def");

   consume(tLPAREN);
   consume(tSDFCOND);

   if (scan(tSTRING))
      p_qstring();

   p_conditional_port_expr();

   p_iopath_def();

   consume(tRPAREN);
}

static void p_port_def(void)
{
   // port_def ::=
   //       ( PORT port_instance delval_list )

   BEGIN("port def");

   consume(tLPAREN);
   consume(tPORT);

   p_port_instance();

   p_delval_list();

   consume(tRPAREN);
}

static void p_interconnect_def(void)
{
   // interconnect_def ::=
   //       ( INTERCONNECT port_instance port_instance delval_list )

   BEGIN("interconnect def");

   consume(tLPAREN);
   consume(tINTERCONNECT);

   p_port_instance();
   p_port_instance();

   p_delval_list();

   consume(tRPAREN);
}

static void p_netdelay_def(void)
{
   // netdelay_def ::=
   //       ( NETDELAY net_spec delval_list )

   BEGIN("netdelay def");

   consume(tLPAREN);
   consume(tNETDELAY);

   p_port_spec();

   p_delval_list();

   consume(tRPAREN);
}

static void p_device_def(void)
{
   // device_def ::=
   //       ( DEVICE [ port_instance ] delval_list )

   BEGIN("device def");

   consume(tLPAREN);
   consume(tDEVICE);

   if (scan(tID))
      p_port_instance();

   p_delval_list();

   consume(tRPAREN);
}

static void p_deldef_list(sdf_flags_t flag)
{
   // del_def { del_def }

   BEGIN("deldef list");

   int tok = peek_nth(2);
   while (tok == tIOPATH || tok == tSDFCOND      || tok == tSDFCONDELSE ||
          tok == tPORT   || tok == tINTERCONNECT || tok == tNETDELAY ||
          tok == tDEVICE) {

      switch (tok) {
      case tIOPATH:
         p_iopath_def();
         break;
      case tSDFCOND:
         p_cond_def();
         break;
      case tSDFCONDELSE:
         p_condelse_def();
         break;
      case tPORT:
         p_port_def();
         break;
      case tINTERCONNECT:
         p_interconnect_def();
         break;
      case tNETDELAY:
         p_netdelay_def();
         break;
      default: // tDEVICE
         p_device_def();
      }

      tok = peek_nth(2);
   };
}

static void p_increment_deltype(void)
{
   // increment_deltype ::=
   //       ( INCREMENT del_def { del_def } )

   BEGIN("increment deltype")

   consume(tLPAREN);
   consume(tINCREMENT);

   p_deldef_list(S_F_VALUE_INCREMENT);

   consume(tRPAREN);
}

static void p_absolute_deltype(void)
{
   // absolute_deltype ::=
   //       ( ABSOLUTE del_def { del_def } )

   BEGIN("absolute deltype");

   consume(tLPAREN);
   consume(tABSOLUTE);

   p_deldef_list(S_F_VALUE_ABSOLUTE);

   consume(tRPAREN);
}

static void p_del_spec(void)
{
   // del_spec ::=
   //       ( DELAY deltype { deltype } )

   BEGIN("del spec");

   consume(tLPAREN);
   consume(tDELAY);

   do {
      switch (peek_nth(2)) {
      case tABSOLUTE:
         p_absolute_deltype();
         break;
      case tINCREMENT:
         p_increment_deltype();
         break;
      case tPATHPULSE:
         p_pathpulse_deltype();
         break;
      case tPATHPULSEP:
         p_pathpulsepercent_deltype();
         break;
      default:
         consume(tLPAREN);
         one_of(tABSOLUTE, tINCREMENT, tPATHPULSE, tPATHPULSEP);
      }
   } while (scan(tLPAREN));

   consume(tRPAREN);
}

static void p_cell(void)
{
   // cell ::=
   //       ( CELL celltype cell_instance { timing_spec } )
   // celltype ::=
   //       ( CELLTYPE qstring )
   // cell_instance ::=
   //       ( INSTANCE [ hierarchical_identifier ] )
   //     | ( INSTANCE * )

   BEGIN("cell");

   consume(tLPAREN);
   consume(tCELL);

   // celltype
   consume(tLPAREN);
   consume(tCELLTYPE);

   p_qstring();

   consume(tRPAREN);

   // cell_instance
   p_cell_instance();

   // { timing_spec }
   int tok = peek_nth(2);
   while (tok == tDELAY || tok == tTIMINGCHECK ||
          tok == tLABEL || tok == tTIMINGENV) {

      switch (tok) {
      case tDELAY:
         p_del_spec();
         break;
      case tTIMINGCHECK:
         p_tc_spec();
         break;
      case tLABEL:
         p_lbl_spec();
         break;
      default: // tTIMINGENV
         p_te_spec();
      }

      tok = peek_nth(2);
   }

   consume(tRPAREN);
}

static void p_timescale(void)
{
   // time_scale ::=
   //       ( TIMESCALE timescale_number timescale_unit )
   // timescale_number ::=
   //       1 | 10 | 100 | 1.0 | 10.0 | 100.0
   // timescale_unit ::=
   //       s | ms | us | ns | ps | fs

   BEGIN("timescale");

   consume(tLPAREN);
   consume(tTIMESCALE);

   double mult_fact = p_real_number();

   // This accepts also timescale_number like 1.000000
   if (!doubles_equal(mult_fact, 1.0)   &&
         !doubles_equal(mult_fact, 10.0)  &&
         !doubles_equal(mult_fact, 100.0))
         parse_error(&state.last_loc, "Invalid timescale_number: %.1f. "
                     "Allowed values are: 1, 10, 100, 1.0, 10.0, 100.0",
                     mult_fact);

   ident_t timescale_unit = p_identifier();
   const char *ustr = istr(timescale_unit);
   double unit_fact = 1;

   if (!strcmp(ustr, "s"))
      unit_fact = 1E15;
   else if (!strcmp(ustr, "ms"))
      unit_fact = 1E12;
   else if (!strcmp(ustr, "us"))
      unit_fact = 1E9;
   else if (!strcmp(ustr, "ns"))
      unit_fact = 1E6;
   else if (!strcmp(ustr, "ps"))
      unit_fact = 1E3;
   else if (strcmp(ustr, "fs"))
      parse_error(&state.last_loc, "Invalid timescale_unit: %s. "
                  "Allowed values are: s, ms, us, ps, fs.", ustr);

   // Multiplication factor for real numbers in the SDF file
   // to convert to 1 fs
   sdf_file->unit_mult = mult_fact * unit_fact;

   consume(tRPAREN);
}

static void p_temperature(void)
{
   // temperature ::=
   //       ( TEMPERATURE rtriple )
   //     | ( TEMPERATURE signed_real_number )

   BEGIN("temperature");

   consume(tLPAREN);
   consume(tTEMPERATURE);

   p_signed_real_number_or_rtripple();

   consume(tRPAREN);
}

static void p_voltage(void)
{
   // voltage ::=
   //       ( VOLTAGE rtriple )
   //     | ( VOLTAGE signed_real_number )

   BEGIN("voltage");

   consume(tLPAREN);
   consume(tVOLTAGE);

   p_signed_real_number_or_rtripple();

   consume(tRPAREN);
}

static void p_hierarchy_divider(void)
{
   // hierarchy_divider ::=
   //       ( DIVIDER hchar )

   BEGIN("hierarchy divider");

   consume(tLPAREN);
   consume(tDIVIDER);

   char hchar = one_of(tDOT, tOVER);
   if (hchar == -1)
      parse_error(&state.last_loc, "Invalid SDF Hierarchy separator: %c. "
                  "SDF hierarchy separator shall be one of: '/' , '.' ",
                  hchar);

   sdf_file->hchar = hchar;

   if (hchar == tDOT)
      sdf_file->hchar_other = tOVER;
   else
      sdf_file->hchar_other = tDOT;

   consume(tRPAREN);
}

static void p_qstring_header_entry(int header_entry_token,
                                   sdf_header_item_kind_t header_item_kind,
                                   const char *info)
{
   // design_name ::=
   //       ( DESIGN qstring )
   // date ::=
   //       ( DATE qstring )
   // vendor ::=
   //       ( VENDOR qstring )
   // program_name ::=
   //       ( PROGRAM qstring )
   // program_version ::=
   //       ( VERSION qstring )
   // process ::=
   //       ( PROCESS qstring )

   BEGIN(info);

   consume(tLPAREN);
   consume(header_entry_token);

   p_qstring();

   consume(tRPAREN);
}

static void p_sdf_version(void)
{
   // sdf_version ::=
   //       ( SDFVERSION qstring )

   BEGIN("sdf version");

   consume(tLPAREN);
   consume(tSDFVERSION);

   ident_t id = p_qstring();

   sdf_file->std = SDF_STD_4_0;

   if (ident_glob(id, "*1.0*", -1))
      sdf_file->std = SDF_STD_1_0;
   else if (ident_glob(id, "*2.0*", -1))
      sdf_file->std = SDF_STD_2_0;
   else if (ident_glob(id, "*2.1*", -1))
      sdf_file->std = SDF_STD_2_1;
   else if (ident_glob(id, "*3.0*", -1))
      sdf_file->std = SDF_STD_3_0;
   else if (ident_glob(id, "*4.0*", -1))
      sdf_file->std = SDF_STD_4_0;
   else
      parse_error(&state.last_loc, "Invalid SDF version: %s. "
                  "SDF version shall contain one of: "
                  "\"1.0\", \"2.0\", \"2.1\", \"3.0\" or \"4.0\".",
                  istr(id));

   consume(tRPAREN);
}

static void p_sdf_header(void)
{
   // sdf_header ::=
   //       sdf_version [ design_name ] [ date ] [ vendor ] [ program_name ]
   //                   [ program_version ] [hierarchy_divider ] [ voltage ]
   //                   [ process ] [temperature ] [ time_scale ]

   BEGIN("sdf header");

   p_sdf_version();

   if (peek_nth(2) == tDESIGN)
      p_qstring_header_entry(tDESIGN, S_HEADER_DESIGN, "design_name");

   if (peek_nth(2) == tDATE)
      p_qstring_header_entry(tDATE, S_HEADER_DATE, "date");

   if (peek_nth(2) == tVENDOR)
      p_qstring_header_entry(tVENDOR, S_HEADER_VENDOR, "vendor");

   if (peek_nth(2) == tPROGRAM)
      p_qstring_header_entry(tPROGRAM, S_HEADER_PROGRAM, "program");

   if (peek_nth(2) == tVERSION)
      p_qstring_header_entry(tVERSION, S_HEADER_VERSION, "program_version");

   if (peek_nth(2) == tDIVIDER)
      p_hierarchy_divider();

   if (peek_nth(2) == tVOLTAGE)
      p_voltage();

   if (peek_nth(2) == tPROCESS)
      p_qstring_header_entry(tPROCESS,  S_HEADER_PROCESS, "process");

   if (peek_nth(2) == tTEMPERATURE)
      p_temperature();

   // Default time unit is 1 ns
   // See section 5.2.11 of IEEE 1497-2001
   sdf_file->unit_mult = 1E6;

   if (peek_nth(2) == tTIMESCALE)
      p_timescale();
}

static sdf_file_t *p_delay_file(const char *file, sdf_flags_t min_max_spec)
{
   // delay_file ::=
   //       ( DELAYFILE sdf_header cell { cell } )

   BEGIN("delay file");

   consume(tLPAREN);
   consume(tDELAYFILE);

   sdf_file = sdf_file_new(16384, 256);

   sdf_file->hchar = '.';
   sdf_file->min_max_spec = min_max_spec;

   p_sdf_header();

   while (peek_nth(2) == tCELL)
      p_cell();

   consume(tRPAREN);

   return sdf_file;
}

///////////////////////////////////////////////////////////////////////////////
// Public API
///////////////////////////////////////////////////////////////////////////////

sdf_file_t *sdf_parse(const char *file, sdf_flags_t min_max_spec)
{
   scan_as_sdf();

   reset_sdf_parser();

   if (peek() == tEOF)
      return NULL;

   return p_delay_file(file, min_max_spec);
}

void reset_sdf_parser(void)
{
   state.tokenq_head = state.tokenq_tail = 0;
}
