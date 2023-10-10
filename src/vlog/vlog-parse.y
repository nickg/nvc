// -*- mode: bison; c-basic-offset: 3 -*-
//
//  Copyright (C) 2022-2023  Nick Gasson
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

%{
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

#define YYLTYPE loc_t
#define YYSTYPE yylval_t
#define YYTOKENTYPE token_t

#define YYEMPTY -2
#define YYEOF tEOF
#define YYerror -3
#define YYUNDEF tERROR

#define yylex processed_yylex

typedef struct _node_list node_list_t;

struct _node_list {
   node_list_t *next;
   node_list_t *tail;
   vlog_node_t  value;
};

static void yyerror(const char *s);

static void node_list_append(node_list_t **l, vlog_node_t v);
static void node_list_concat(node_list_t **a, node_list_t *b);
static node_list_t *node_list_single(vlog_node_t v);
static void node_list_free(node_list_t *l);

#define YYLLOC_DEFAULT(Current, Rhs, N) {                            \
   if (N) {                                                          \
      (Current) = get_loc(YYRHSLOC(Rhs, 1).first_line,               \
                          YYRHSLOC(Rhs, 1).first_column,             \
                          YYRHSLOC(Rhs, N).first_line +              \
                          YYRHSLOC(Rhs, N).line_delta,               \
                          YYRHSLOC(Rhs, N).first_column +            \
                          YYRHSLOC(Rhs, N).column_delta,             \
                          YYRHSLOC(Rhs, N).file_ref);                \
   }                                                                 \
   else {                                                            \
      (Current) = YYRHSLOC(Rhs, 0);                                  \
   }                                                                 \
 }

static vlog_node_t root;

static void add_port(vlog_node_t module, vlog_node_t port)
{
   if (vlog_kind(port) == V_PORT_DECL) {
      // Verilog-2001 style port declaration
      vlog_node_t ref = vlog_new(V_REF);
      vlog_set_loc(ref, vlog_loc(port));
      vlog_set_ident(ref, vlog_ident(port));
      vlog_set_ref(ref, port);

      vlog_add_port(module, ref);
      vlog_add_decl(module, port);
   }
   else {
      assert(vlog_kind(port) == V_REF);
      vlog_add_port(module, port);
   }
}

static bool is_decl(vlog_node_t v)
{
   switch (vlog_kind(v)) {
   case V_PORT_DECL:
   case V_NET_DECL:
   case V_VAR_DECL:
      return true;
   default:
      return false;
   }
}
%}

%type   <vlog>          module_declaration primary expression
%type   <vlog>          always_construct statement statement_or_null
%type   <vlog>          procedural_timing_control_statement
%type   <vlog>          lvalue event_control event_expression
%type   <vlog>          nonblocking_assignment delay_or_event_control
%type   <vlog>          port_declaration port_reference blocking_assignment
%type   <vlog>          port initial_construct net_assignment
%type   <vlog>          seq_block system_task_enable string number
%type   <vlog>          decimal_number conditional_statement variable_type
%type   <vlog>          delay_control delay_value
%type   <ident>         identifier hierarchical_identifier
%type   <list>          module_item_list module_port_list_opt module_item
%type   <list>          list_of_port_declarations module_item_list_opt
%type   <list>          list_of_ports list_of_statements list_of_expressions
%type   <list>          net_declaration list_of_net_identifiers
%type   <list>          module_or_generate_item_declaration
%type   <list>          module_or_generate_item continuous_assign
%type   <list>          list_of_net_assignments reg_declaration
%type   <list>          list_of_variable_identifiers list_of_statements_opt
%type   <pair>          external_identifier
%type   <kind>          net_type

%token  <str>           tID 200 "identifier"
%token  <str>           tSYSTASK 383 "system task identifier"
%token  <str>           tSTRING 229 "string"
%token  <str>           tUNSIGNED 359 "unsigned number"
%token                  tMODULE 350 "module"
%token                  tENDMODULE 351 "endmodule"
%token                  tINPUT 352 "input"
%token                  tOUTPUT 353 "output"
%token                  tREG 354 "reg"
%token                  tALWAYS 336 "always"
%token                  tINITIAL 357 "initial"
%token                  tPOSEDGE 355 "posedge"
%token                  tNEGEDGE 356 "negedge"
%token                  tBEGIN 211 "begin"
%token                  tEND 203 "end"
%token                  tLE 294 "<="
%token                  tWIRE 358 "wire"
%token                  tASSIGN 227 "assign"
%token                  tIF 234 "if"
%token                  tELSE 255 "else"
%token                  tEOF 0 "end of file"

%left                   '|'
%left                   '&'

%precedence "then"
%precedence tELSE

%define parse.error verbose
%expect 0

%%

description:    { root = vlog_new(V_MODULE); } module_declaration { YYACCEPT; }
        |       tEOF { root = NULL; }
        ;

module_declaration:
                tMODULE external_identifier module_port_list_opt ';'
                module_item_list_opt tENDMODULE
                {
                   ident_t qual = ident_prefix(lib_name(lib_work()),
                                               $2.right, '.');

                   $$ = root;
                   vlog_set_ident($$, qual);
                   vlog_set_ident2($$, $2.left);
                   vlog_set_loc($$, &@$);

                   for (node_list_t *it = $3; it; it = it->next)
                      add_port($$, it->value);
                   node_list_free($3);

                   for (node_list_t *it = $5; it; it = it->next) {
                      if (is_decl(it->value))
                         vlog_add_decl($$, it->value);
                      else
                         vlog_add_stmt($$, it->value);
                   }
                   node_list_free($5);
                }
        ;

module_port_list_opt:
                '(' list_of_port_declarations ')' { $$ = $2; }
        |       '(' list_of_ports ')' { $$ = $2; }
        |       /* empty */ { $$ = NULL; }
        ;

list_of_ports:  list_of_ports ',' port
                {
                   $$ = $1;
                   node_list_append(&$$, $3);
                }
        |       port { $$ = node_list_single($1); }
        ;

port:           port_reference { $$ = $1; }
        ;

port_reference: identifier
                {
                   $$ = vlog_new(V_REF);
                   vlog_set_loc($$, &@$);
                   vlog_set_ident($$, $1);
                }
        ;

list_of_port_declarations:
                list_of_port_declarations ',' external_identifier
                {
                   vlog_node_t p = vlog_new(V_PORT_DECL);
                   vlog_set_loc(p, &@3);
                   vlog_set_ident(p, $3.left);
                   vlog_set_ident2(p, $3.right);
                   vlog_set_subkind(p, vlog_subkind($1->tail->value));

                   $$ = $1;
                   node_list_append(&$$, p);
                }
        |       list_of_port_declarations ',' port_declaration
                {
                   $$ = $1;
                   node_list_append(&$$, $3);
                }
        |       port_declaration { $$ = node_list_single($1); }
        ;

port_declaration:
                tINPUT external_identifier
                {
                   $$ = vlog_new(V_PORT_DECL);
                   vlog_set_loc($$, &@$);
                   vlog_set_ident($$, $2.left);
                   vlog_set_ident2($$, $2.right);
                   vlog_set_subkind($$, V_PORT_INPUT);
                }
        |       tOUTPUT external_identifier
                {
                   $$ = vlog_new(V_PORT_DECL);
                   vlog_set_loc($$, &@$);
                   vlog_set_ident($$, $2.left);
                   vlog_set_ident2($$, $2.right);
                   vlog_set_subkind($$, V_PORT_OUTPUT);
                }
        |       tOUTPUT tREG external_identifier
                {
                   $$ = vlog_new(V_PORT_DECL);
                   vlog_set_loc($$, &@$);
                   vlog_set_ident($$, $3.left);
                   vlog_set_ident2($$, $3.right);
                   vlog_set_subkind($$, V_PORT_OUTPUT_REG);
                }
        ;

module_item_list_opt:
                module_item_list
        |       /* empty */ { $$ = NULL; }
        ;

module_item_list:
                module_item_list module_item
                {
                   $$ = $1;
                   node_list_concat(&$$, $2);
                }
        |       module_item
        ;

module_item:
                module_or_generate_item
        |       port_declaration ';'
                {
                   $$ = NULL;
                   node_list_append(&$$, $1);
                }
        ;

module_or_generate_item:
                module_or_generate_item_declaration
        |       always_construct { $$ = node_list_single($1); }
        |       initial_construct { $$ = node_list_single($1); }
        |       continuous_assign
        ;

module_or_generate_item_declaration:
                net_declaration
        |       reg_declaration
        ;

net_declaration:
                net_type list_of_net_identifiers ';'
                {
                   $$ = $2;
                }
        |       net_type '[' expression ':' expression ']'
                list_of_net_identifiers ';'
                {
                   vlog_node_t r = vlog_new(V_DIMENSION);
                   vlog_set_loc(r, &@$);
                   vlog_set_subkind(r, V_DIM_PACKED);
                   vlog_set_left(r, $3);
                   vlog_set_right(r, $5);

                   for (node_list_t *it = $7; it; it = it->next) {
                      vlog_add_range(it->value, r);
                      vlog_set_datatype(it->value, DT_LOGIC);
                   }

                   $$ = $7;
                }
        ;

net_type:       tWIRE { $$ = V_NET_WIRE; }
        ;

list_of_net_identifiers:
                list_of_net_identifiers ',' identifier
                {
                   vlog_node_t v = vlog_new(V_NET_DECL);
                   vlog_set_loc(v, &@3);
                   vlog_set_ident(v, $3);

                   $$ = $1;
                   node_list_append(&$$, v);
                }
        |       identifier
                {
                   vlog_node_t v = vlog_new(V_NET_DECL);
                   vlog_set_loc(v, &@$);
                   vlog_set_ident(v, $1);

                   $$ = NULL;
                   node_list_append(&$$, v);
                }
        ;

reg_declaration:
                tREG list_of_variable_identifiers ';'
                {
                   $$ = $2;
                }
        |       tREG '[' expression ':' expression ']'
                list_of_variable_identifiers ';'
                {
                   vlog_node_t r = vlog_new(V_DIMENSION);
                   vlog_set_loc(r, &@$);
                   vlog_set_subkind(r, V_DIM_PACKED);
                   vlog_set_left(r, $3);
                   vlog_set_right(r, $5);

                   for (node_list_t *it = $7; it; it = it->next)
                      vlog_add_range(it->value, r);

                   $$ = $7;
                }
        ;

list_of_variable_identifiers:
                list_of_variable_identifiers ',' variable_type
                {
                   $$ = $1;
                   node_list_append(&$$, $3);
                }
        |       variable_type
                {
                   $$ = NULL;
                   node_list_append(&$$, $1);
                }
        ;

variable_type:  identifier
                {
                   $$ = vlog_new(V_VAR_DECL);
                   vlog_set_loc($$, &@$);
                   vlog_set_ident($$, $1);
                }
        ;

continuous_assign:
                tASSIGN list_of_net_assignments ';' { $$ = $2; }
        ;

list_of_net_assignments:
                net_assignment { $$ = node_list_single($1); }
        |       list_of_net_assignments ',' net_assignment
                {
                   $$ = $1;
                   node_list_append(&$$, $3);
                }
        ;

net_assignment: lvalue '=' expression
                {
                   $$ = vlog_new(V_ASSIGN);
                   vlog_set_loc($$, &@$);
                   vlog_set_target($$, $1);
                   vlog_set_value($$, $3);

                   char *name LOCAL =
                      xasprintf("__assign#line%d", @$.first_line);
                   vlog_set_ident($$, ident_uniq(name));
                }
        ;

always_construct:
                tALWAYS statement
                {
                   $$ = vlog_new(V_ALWAYS);
                   vlog_set_loc($$, &@$);
                   vlog_add_stmt($$, $2);

                   char *name LOCAL =
                      xasprintf("__always#line%d", @$.first_line);
                   vlog_set_ident($$, ident_uniq(name));
                }
        ;

initial_construct:
                tINITIAL statement
                {
                   $$ = vlog_new(V_INITIAL);
                   vlog_set_loc($$, &@$);
                   vlog_add_stmt($$, $2);

                   char *name LOCAL =
                      xasprintf("__initial#line%d", @$.first_line);
                   vlog_set_ident($$, ident_uniq(name));
                }
        ;

statement:      procedural_timing_control_statement
        |       nonblocking_assignment ';'
        |       blocking_assignment ';'
        |       seq_block
        |       system_task_enable
        |       conditional_statement
        ;

statement_or_null:
                statement
        |       ';' { $$ = NULL; }
        ;

list_of_statements_opt:
                list_of_statements
        |       /* Empty */
                { $$ = NULL; }
        ;

list_of_statements:
                list_of_statements statement
                {
                   $$ = $1;
                   node_list_append(&$$, $2);
                }
        |       statement
                {
                   $$ = NULL;
                   node_list_append(&$$, $1);
                }
        ;

conditional_statement:
                tIF '(' expression ')' statement_or_null %prec "then"
                {
                   vlog_node_t c = vlog_new(V_COND);
                   vlog_set_loc(c, &@3);
                   vlog_set_value(c, $3);
                   if ($5 != NULL)
                      vlog_add_stmt(c, $5);

                   $$ = vlog_new(V_IF);
                   vlog_set_loc($$, &@$);
                   vlog_add_cond($$, c);
                }
        |       tIF '(' expression ')' statement_or_null tELSE statement_or_null
                {
                   vlog_node_t c1 = vlog_new(V_COND);
                   vlog_set_loc(c1, &@3);
                   vlog_set_value(c1, $3);
                   if ($5 != NULL)
                      vlog_add_stmt(c1, $5);

                   vlog_node_t c2 = vlog_new(V_COND);
                   vlog_set_loc(c2, &@6);
                   if ($7 != NULL)
                      vlog_add_stmt(c2, $7);

                   $$ = vlog_new(V_IF);
                   vlog_set_loc($$, &@$);
                   vlog_add_cond($$, c1);
                   vlog_add_cond($$, c2);
                }
        ;

system_task_enable:
                tSYSTASK ';'
                {
                   $$ = vlog_new(V_SYSTASK);
                   vlog_set_loc($$, &@$);
                   vlog_set_ident($$, ident_new($1));

                   free($1);
                }
        |       tSYSTASK '(' list_of_expressions ')' ';'
                {
                   $$ = vlog_new(V_SYSTASK);
                   vlog_set_loc($$, &@$);
                   vlog_set_ident($$, ident_new($1));

                   for (node_list_t *it = $3; it; it = it->next)
                      vlog_add_param($$, it->value);
                   node_list_free($3);

                   free($1);
                }
        ;

seq_block:      tBEGIN list_of_statements_opt tEND
                {
                   $$ = vlog_new(V_SEQ_BLOCK);
                   vlog_set_loc($$, &@$);

                   for (node_list_t *it = $2; it; it = it->next)
                      vlog_add_stmt($$, it->value);
                   node_list_free($2);
                }
        |       tBEGIN ':' identifier list_of_statements_opt tEND
                {
                   $$ = vlog_new(V_SEQ_BLOCK);
                   vlog_set_loc($$, &@$);
                   vlog_set_ident($$, $3);

                   for (node_list_t *it = $4; it; it = it->next)
                      vlog_add_stmt($$, it->value);
                   node_list_free($4);
                }
        ;

nonblocking_assignment:
                lvalue tLE expression
                {
                   $$ = vlog_new(V_NBASSIGN);
                   vlog_set_loc($$, &@$);
                   vlog_set_target($$, $1);
                   vlog_set_value($$, $3);
                }
        ;

blocking_assignment:
                lvalue '=' expression
                {
                   $$ = vlog_new(V_BASSIGN);
                   vlog_set_loc($$, &@$);
                   vlog_set_subkind($$, V_ASSIGN_EQUALS);
                   vlog_set_target($$, $1);
                   vlog_set_value($$, $3);
                }
        ;

procedural_timing_control_statement:
                delay_or_event_control statement_or_null
                {
                   $$ = vlog_new(V_TIMING);
                   vlog_set_loc($$, &@$);
                   vlog_set_value($$, $1);
                   if ($2 != NULL)
                      vlog_add_stmt($$, $2);
                }
        ;

delay_or_event_control:
                delay_control
        |       event_control
        ;

delay_control:  '#' delay_value
                {
                   $$ = vlog_new(V_DELAY_CONTROL);
                   vlog_set_loc($$, &@$);
                   vlog_set_value($$, $2);
                }
        ;

delay_value:    decimal_number
        ;

event_control: '@' '(' event_expression ')' { $$ = $3; }

event_expression:
                expression
                {
                   $$ = vlog_new(V_EVENT);
                   vlog_set_loc($$, &@$);
                   vlog_set_subkind($$, V_EVENT_LEVEL);
                   vlog_set_value($$, $1);
                }
        |       tPOSEDGE expression
                {
                   $$ = vlog_new(V_EVENT);
                   vlog_set_loc($$, &@$);
                   vlog_set_subkind($$, V_EVENT_POSEDGE);
                   vlog_set_value($$, $2);
                }
        |       tNEGEDGE expression
                {
                   $$ = vlog_new(V_EVENT);
                   vlog_set_loc($$, &@$);
                   vlog_set_subkind($$, V_EVENT_NEGEDGE);
                   vlog_set_value($$, $2);
                }
        ;

lvalue:         hierarchical_identifier
                {
                   $$ = vlog_new(V_REF);
                   vlog_set_loc($$, &@$);
                   vlog_set_ident($$, $1);
                }
        ;

expression:     primary
        |       string
        |       expression '|' expression
                {
                   $$ = vlog_new(V_BINARY);
                   vlog_set_loc($$, &@$);
                   vlog_set_subkind($$, V_BINARY_OR);
                   vlog_set_left($$, $1);
                   vlog_set_right($$, $3);
                }
        |       expression '&' expression
                {
                   $$ = vlog_new(V_BINARY);
                   vlog_set_loc($$, &@$);
                   vlog_set_subkind($$, V_BINARY_AND);
                   vlog_set_left($$, $1);
                   vlog_set_right($$, $3);
                }
        ;

list_of_expressions:
                list_of_expressions ',' expression
                {
                   $$ = $1;
                   node_list_append(&$$, $3);
                }
        |       expression
                {
                   $$ = NULL;
                   node_list_append(&$$, $1);
                }
        ;

string:         tSTRING
                {
                   $$ = vlog_new(V_STRING);
                   vlog_set_loc($$, &@$);
                   $1[strlen($1) - 1] = '\0';
                   vlog_set_text($$, $1 + 1);
                   free($1);
                }
        ;

primary:        hierarchical_identifier
                {
                   $$ = vlog_new(V_REF);
                   vlog_set_ident($$, $1);
                   vlog_set_loc($$, &@$);
                }
        |       number
        ;

number:         decimal_number
        ;

decimal_number: tUNSIGNED
                {
                   $$ = vlog_new(V_NUMBER);
                   vlog_set_loc($$, &@$);
                   vlog_set_number($$, number_new($1));
                   free($1);
                }
        ;

hierarchical_identifier:
                identifier
        ;

identifier:     tID
                {
                   $$ = ident_new($1);
                   free($1);
                }
        ;

external_identifier:
                tID
                {
                   $$.left = ident_new($1);
                   for (char *p = $1; *p; p++)
                      *p = toupper_iso88591(*p);
                   $$.right = ident_new($1);
                   free($1);
                }
        ;

%%

static void yyerror(const char *s)
{
   error_at(&yylloc, "%s", s);
}

static void node_list_concat(node_list_t **a, node_list_t *b)
{
   if (*a == NULL)
      *a = b;
   else if (b != NULL) {
      assert((*a)->tail->next == NULL);
      (*a)->tail->next = b;
      (*a)->tail = b->tail;
   }
}

static void node_list_free(node_list_t *l)
{
   while (l != NULL) {
      node_list_t *next = l->next;
      free(l);
      l = next;
   }
}

static void node_list_append(node_list_t **l, vlog_node_t v)
{
   node_list_t *new = xmalloc(sizeof(node_list_t));
   new->next  = NULL;
   new->value = v;
   new->tail  = new;

   node_list_concat(l, new);
}

static node_list_t *node_list_single(vlog_node_t v)
{
   node_list_t *new = xmalloc(sizeof(node_list_t));
   new->next  = NULL;
   new->value = v;
   new->tail  = new;

   return new;
}

vlog_node_t vlog_parse(void)
{
   make_new_arena();
   root = NULL;

   scan_as_verilog();

   if (yyparse())
      return NULL;
   else
      return root;
}

void reset_verilog_parser(void)
{
}
