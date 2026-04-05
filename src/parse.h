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

#ifndef _PARSE_H
#define _PARSE_H

#include "prim.h"
#include "scan.h"

#define RECOVER_THRESH 5
#define TRACE_PARSE    0
#define TRACE_RECOVERY 0
#define TOKENQ_SIZE    8

typedef struct {
   token_t  token;
   yylval_t lval;
   loc_t    loc;
} tokenq_t;

typedef token_t (*lex_fn_t)(void);
typedef void (*error_fn_t)(void);

typedef struct {
   lex_fn_t    lex_fn;
   error_fn_t  error_fn;
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
   const char    *old_hint;
   loc_t          old_start_loc;
   parse_state_t *owner;
} rule_state_t;

#define scan(...) parse_scan(&state, __VA_ARGS__, -1)
#define expect(...) parse_expect(&state, __VA_ARGS__, -1)
#define one_of(...) parse_one_of(&state, __VA_ARGS__, -1)
#define not_at_token(...) parse_not_at_token(&state, __VA_ARGS__, -1)
#define peek() peek_nth(1)
#define peek_nth(n) parse_peek_nth(&state, (n))
#define optional(tok) parse_optional(&state, (tok))
#define consume(tok) parse_consume(&state, (tok))

#define CURRENT_LOC parse_current_loc(&state)

#define parse_error(loc, ...) do {              \
      if (state.n_correct >= RECOVER_THRESH)    \
         error_at((loc), __VA_ARGS__);          \
   } while (0)

// TODO: use defer when available
#define EXTEND(s)                                                      \
   __attribute__((cleanup(__parse_pop_state), unused))                 \
   const rule_state_t _state = __parse_push_state(&state, (s))

#if TRACE_PARSE
void parse_trace_in(parse_state_t *state);
void parse_trace_out(parse_state_t *state);
#else
#define parse_trace_in(state)
#define parse_trace_out(state)
#endif

token_t parse_peek_nth(parse_state_t *state, int n);
void drop_token(parse_state_t *state);
void drop_tokens_until(parse_state_t *state, token_t tok);
void parse_vexpect(parse_state_t *state, va_list ap);
void parse_expect(parse_state_t *state, ...);
bool parse_optional(parse_state_t *state, token_t tok);
bool parse_scan(parse_state_t *state, ...);
bool parse_not_at_token(parse_state_t *state, ...);
bool parse_consume(parse_state_t *state, token_t tok);
token_t parse_one_of(parse_state_t *state, ...);
const loc_t *parse_current_loc(parse_state_t *state);

__attribute__((always_inline))
static inline rule_state_t __parse_push_state(parse_state_t *state,
                                              const char *hint)
{
   const rule_state_t _state = {
      state->hint_str,
      state->start_loc,
      state,  // TODO: remove with defer
   };
   state->hint_str = hint;

   parse_trace_in(state);

   return _state;
}

__attribute__((always_inline))
static inline void __parse_pop_state(const rule_state_t *rule)
{
   parse_trace_out(rule->owner);

   rule->owner->hint_str = rule->old_hint;
   if (rule->old_start_loc.first_line != LINE_INVALID)
      rule->owner->start_loc = rule->old_start_loc;
}

#endif  // _PARSE_H
