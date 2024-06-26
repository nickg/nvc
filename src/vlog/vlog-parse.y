// -*- mode: bison; c-basic-offset: 3 -*-
//
//  Copyright (C) 2022-2024  Nick Gasson
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

static void set_timescale(const char *unit_value, const char *unit_name,
                          const char *prec_value, const char *prec_name,
                          const loc_t *loc);
static void clone_port(node_list_t **list, vlog_node_t p);

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
   switch (vlog_kind(port)) {
   case V_PORT_DECL:
      {
         // Verilog-2001 style port declaration
         vlog_node_t ref = vlog_new(V_REF);
         vlog_set_loc(ref, vlog_loc(port));
         vlog_set_ident(ref, vlog_ident(port));
         vlog_set_ref(ref, port);

         vlog_add_port(module, ref);
         vlog_add_decl(module, port);
      }
      break;
   case V_REF:
      vlog_add_port(module, port);
      break;
   case V_NET_DECL:
   case V_VAR_DECL:
      vlog_add_decl(module, port);
      break;
   default:
      fatal_trace("unexpected %s in port list", vlog_kind_str(vlog_kind(port)));
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

static vlog_node_t make_strength(vlog_strength_t value, const loc_t *loc)
{
   vlog_node_t s = vlog_new(V_STRENGTH);
   vlog_set_loc(s, loc);
   vlog_set_subkind(s, value);
   return s;
}

%}

%type   <vlog>          module_declaration primary expression
%type   <vlog>          always_construct statement statement_or_null
%type   <vlog>          procedural_timing_control_statement
%type   <vlog>          lvalue event_control event_expression
%type   <vlog>          nonblocking_assignment delay_or_event_control
%type   <vlog>          port_reference blocking_assignment range_opt
%type   <vlog>          port initial_construct net_assignment
%type   <vlog>          seq_block system_task_enable string number
%type   <vlog>          decimal_number conditional_statement variable_type
%type   <vlog>          delay_control delay_value strength0 strength1
%type   <vlog>          pull_gate_instance port_identifier module_instance
%type   <vlog>          system_function_call loop_statement specify_block
%type   <vlog>          real_number
%type   <ident>         identifier hierarchical_identifier
%type   <list>          module_item_list module_port_list_opt module_item
%type   <list>          list_of_port_declarations module_item_list_opt
%type   <list>          list_of_ports list_of_statements list_of_expressions
%type   <list>          net_declaration list_of_net_identifiers
%type   <list>          module_or_generate_item_declaration
%type   <list>          module_or_generate_item continuous_assign
%type   <list>          list_of_net_assignments reg_declaration
%type   <list>          list_of_variable_identifiers list_of_statements_opt
%type   <list>          gate_instantiation pulldown_strength pullup_strength
%type   <list>          port_declaration port_declaration_head
%type   <list>          module_instantiation list_of_port_connections
%type   <list>          list_of_port_connections_opt specify_item_list_opt
%type   <list>          specify_item_list
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
%token                  tINOUT 231 "inout"
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
%token                  tASSOC 248 "=>"
%token                  tELSE 255 "else"
%token                  tTIMESCALE 399 "`timescale"
%token                  tSUPPLY0 400 "supply0"
%token                  tSUPPLY1 401 "supply1"
%token                  tPULLDOWN 402 "pulldown"
%token                  tPULLUP 403 "pullup"
%token                  tCASEEQ 404 "==="
%token                  tCASENEQ 405 "!=="
%token                  tLOGEQ 406 "=="
%token                  tLOGNEQ 407 "!="
%token                  tATTRBEGIN 408 "(*"
%token                  tATTREND 409 "*)"
%token  <str>           tNUMBER 410 "number"
%token                  tFOREVER 411 "forever"
%token                  tSPECIFY 414 "specify"
%token                  tENDSPECIFY 415 "endspecify"

%token                  tEOF 0 "end of file"

%left                   '|'
%left                   '&'
%left                   tCASEEQ tCASENEQ tLOGEQ tLOGNEQ
%left                   '+' '-'
%left                   '*' '/' '%'
%left                   '~' '!'

%precedence "then"
%precedence tELSE

%define parse.error verbose
%expect 0

%%

source_text:    directive_list description
        ;

directive_list: directive_list timescale_compiler_directive
        |       /* empty */
                ;

timescale_compiler_directive:
                tTIMESCALE tUNSIGNED tID '/' tUNSIGNED tID
                {
                   set_timescale($2, $3, $5, $6, &@$);
                }
        ;

description:    { root = vlog_new(V_MODULE); } module_declaration { YYACCEPT; }
        |       tEOF { root = NULL; }
        ;

attr_spec:      tATTRBEGIN identifier tATTREND
        ;

attr_list_opt:  attr_list_opt attr_spec
        |       /* empty */
        ;

module_declaration:
                attr_list_opt tMODULE external_identifier
                module_port_list_opt ';' module_item_list_opt tENDMODULE
                {
                   ident_t qual = ident_prefix(lib_name(lib_work()),
                                               $3.right, '.');

                   $$ = root;
                   vlog_set_ident($$, qual);
                   vlog_set_ident2($$, $3.left);
                   vlog_set_loc($$, &@$);

                   for (node_list_t *it = $4; it; it = it->next)
                      add_port($$, it->value);
                   node_list_free($4);

                   for (node_list_t *it = $6; it; it = it->next) {
                      if (is_decl(it->value))
                         vlog_add_decl($$, it->value);
                      else
                         vlog_add_stmt($$, it->value);
                   }
                   node_list_free($6);
                }
        ;

module_port_list_opt:
                '(' list_of_port_declarations ')' { $$ = $2; }
        |       '(' list_of_ports ')' { $$ = $2; }
        |       '(' ')' { $$ = NULL; }
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

port_identifier:
                external_identifier
                {
                   $$ = vlog_new(V_PORT_DECL);
                   vlog_set_loc($$, &@1);
                   vlog_set_ident($$, $1.left);
                   vlog_set_ident2($$, $1.right);
                }
        ;

list_of_port_declarations:
                list_of_port_declarations ',' port_declaration_head
                {
                   $$ = $1;
                   node_list_concat(&$$, $3);
                }
        |       list_of_port_declarations ',' port_identifier
                {
                   $$ = $1;
                   clone_port(&$$, $3);
                }
        |       port_declaration_head { $$ = $1; }
        ;

range_opt:      '[' expression ':' expression ']'
                {
                   $$ = vlog_new(V_DIMENSION);
                   vlog_set_loc($$, &@$);
                   vlog_set_subkind($$, V_DIM_PACKED);
                   vlog_set_left($$, $2);
                   vlog_set_right($$, $4);
                }
        |       /* empty */ { $$ = NULL; }
        ;

port_declaration_head:
                tINPUT range_opt port_identifier
                {
                   vlog_set_subkind($3, V_PORT_INPUT);
                   if ($2 != NULL)
                      vlog_add_range($3, $2);

                   $$ = node_list_single($3);
                }
        |       tOUTPUT range_opt port_identifier
                {
                   vlog_set_subkind($3, V_PORT_OUTPUT);
                   if ($2 != NULL)
                      vlog_add_range($3, $2);

                   $$ = node_list_single($3);
                }
        |       tINOUT range_opt port_identifier
                {
                   vlog_set_subkind($3, V_PORT_INOUT);
                   if ($2 != NULL)
                      vlog_add_range($3, $2);

                   $$ = node_list_single($3);
                }
        |       tOUTPUT tREG range_opt port_identifier
                {
                   vlog_set_subkind($4, V_PORT_OUTPUT);
                   if ($3 != NULL)
                      vlog_add_range($4, $3);

                   $$ = NULL;
                   node_list_append(&$$, $4);

                   vlog_node_t reg = vlog_new(V_VAR_DECL);
                   vlog_set_loc(reg, &@$);
                   vlog_set_ident(reg, vlog_ident($4));

                   node_list_append(&$$, reg);
                }
        ;

port_declaration:
                port_declaration ',' port_identifier
                {
                   $$ = $1;
                   clone_port(&$$, $3);
                }
        |       port_declaration_head
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
        |       specify_block { $$ = node_list_single($1); }
        ;

module_or_generate_item:
                attr_list_opt module_or_generate_item_declaration { $$ = $2; }
        |       attr_list_opt always_construct { $$ = node_list_single($2); }
        |       attr_list_opt initial_construct { $$ = node_list_single($2); }
        |       attr_list_opt continuous_assign { $$ = $2; }
        |       attr_list_opt gate_instantiation { $$ = $2; }
        |       attr_list_opt module_instantiation { $$ = $2; }
        ;

module_instantiation:
                identifier module_instance ';'
                {
                   vlog_set_ident2($2, $1);
                   $$ = node_list_single($2);
                }
        ;

module_instance:
                identifier list_of_port_connections_opt
                {
                   $$ = vlog_new(V_MOD_INST);
                   vlog_set_loc($$, &@$);
                   vlog_set_ident($$, $1);

                   for (node_list_t *it = $2; it; it = it->next)
                      vlog_add_param($$, it->value);

                   node_list_free($2);
                }
        ;

list_of_port_connections_opt:
                '(' list_of_port_connections ')' { $$ = $2; }
        |       /* empty */ { $$ = NULL; }
        ;

list_of_port_connections:
                list_of_port_connections ',' port_reference
                {
                   $$ = $1;
                   node_list_append(&$$, $3);
                }
        |       port_reference { $$ = node_list_single($1); }
        ;

module_or_generate_item_declaration:
                net_declaration
        |       reg_declaration
        ;

specify_block:  tSPECIFY specify_item_list_opt tENDSPECIFY
                {
                   $$ = vlog_new(V_SPECIFY);
                   vlog_set_loc($$, &@$);
                }
        ;

specify_item_list_opt:
                specify_item_list
        |       /* empty */ { $$ = NULL; }
        ;

specify_item_list:
                specify_item_list specify_item
                {
                   $$ = NULL;
                }
        |       specify_item { $$ = NULL; }
        ;

specify_item:   path_declaration
        ;

path_declaration:
                simple_path_declaration ';'
        ;

simple_path_declaration:
                parallel_path_description '=' path_delay_value
        ;

parallel_path_description:
                '(' identifier tASSOC identifier ')'
        ;

path_delay_value:
                list_of_path_delay_expressions
        |       '(' list_of_path_delay_expressions ')'
        ;

list_of_path_delay_expressions:
                path_delay_expression
        ;

list_of_path_delay_expressions:
                path_delay_expression ',' path_delay_expression
        ;

path_delay_expression:
                constant_expression
        ;

constant_expression:
                constant_primary
        ;

constant_primary:
                number
        ;

net_declaration:
                net_type list_of_net_identifiers ';'
                {
                   for (node_list_t *it = $2; it; it = it->next)
                      vlog_set_subkind(it->value, $1);

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
                      vlog_set_subkind(it->value, $1);
                   }

                   $$ = $7;
                }
        ;

net_type:       tWIRE { $$ = V_NET_WIRE; }
        |       tSUPPLY0 { $$ = V_NET_SUPPLY0; }
        |       tSUPPLY1 { $$ = V_NET_SUPPLY1; }
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
        |       loop_statement
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

system_function_call:
                tSYSTASK
                {
                   $$ = vlog_new(V_SYSFUNC);
                   vlog_set_loc($$, &@$);
                   vlog_set_ident($$, ident_new($1));

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
        |       lvalue tLE delay_or_event_control expression
                {
                   $$ = vlog_new(V_NBASSIGN);
                   vlog_set_loc($$, &@$);
                   vlog_set_target($$, $1);
                   vlog_set_delay($$, $3);
                   vlog_set_value($$, $4);
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
        |       lvalue '=' delay_or_event_control expression
                {
                   $$ = vlog_new(V_BASSIGN);
                   vlog_set_loc($$, &@$);
                   vlog_set_subkind($$, V_ASSIGN_EQUALS);
                   vlog_set_target($$, $1);
                   vlog_set_delay($$, $3);
                   vlog_set_value($$, $4);
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

loop_statement: tFOREVER statement
                {
                   $$ = vlog_new(V_FOREVER);
                   vlog_set_loc($$, &@$);
                   vlog_add_stmt($$, $2);
                }
        ;

lvalue:         hierarchical_identifier
                {
                   $$ = vlog_new(V_REF);
                   vlog_set_loc($$, &@$);
                   vlog_set_ident($$, $1);
                }
        |       hierarchical_identifier '[' expression ']'
                {
                   $$ = vlog_new(V_BIT_SELECT);
                   vlog_set_ident($$, $1);
                   vlog_add_param($$, $3);
                   vlog_set_loc($$, &@$);
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
        |       expression '+' expression
                {
                   $$ = vlog_new(V_BINARY);
                   vlog_set_loc($$, &@$);
                   vlog_set_subkind($$, V_BINARY_PLUS);
                   vlog_set_left($$, $1);
                   vlog_set_right($$, $3);
                }
        |       expression '-' expression
                {
                   $$ = vlog_new(V_BINARY);
                   vlog_set_loc($$, &@$);
                   vlog_set_subkind($$, V_BINARY_MINUS);
                   vlog_set_left($$, $1);
                   vlog_set_right($$, $3);
                }
        |       expression tCASEEQ expression
                {
                   $$ = vlog_new(V_BINARY);
                   vlog_set_loc($$, &@$);
                   vlog_set_subkind($$, V_BINARY_CASE_EQ);
                   vlog_set_left($$, $1);
                   vlog_set_right($$, $3);
                }
        |       expression tCASENEQ expression
                {
                   $$ = vlog_new(V_BINARY);
                   vlog_set_loc($$, &@$);
                   vlog_set_subkind($$, V_BINARY_CASE_NEQ);
                   vlog_set_left($$, $1);
                   vlog_set_right($$, $3);
                }
        |       '~' expression
                {
                   $$ = vlog_new(V_UNARY);
                   vlog_set_loc($$, &@$);
                   vlog_set_subkind($$, V_UNARY_BITNEG);
                   vlog_set_value($$, $2);
                }
        |       '!' expression
                {
                   $$ = vlog_new(V_UNARY);
                   vlog_set_loc($$, &@$);
                   vlog_set_subkind($$, V_UNARY_NOT);
                   vlog_set_value($$, $2);
                }
        |       '-' expression
                {
                   $$ = vlog_new(V_UNARY);
                   vlog_set_loc($$, &@$);
                   vlog_set_subkind($$, V_UNARY_NEG);
                   vlog_set_value($$, $2);
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
        |       hierarchical_identifier '[' expression ']'
                {
                   $$ = vlog_new(V_BIT_SELECT);
                   vlog_set_ident($$, $1);
                   vlog_add_param($$, $3);
                   vlog_set_loc($$, &@$);
                }
        |       number
        |       system_function_call
        ;

number:         decimal_number
        |       real_number
        |       tNUMBER
                {
                   $$ = vlog_new(V_NUMBER);
                   vlog_set_loc($$, &@$);
                   vlog_set_number($$, number_new($1));
                   free($1);
                }
        ;

decimal_number: tUNSIGNED
                {
                   $$ = vlog_new(V_NUMBER);
                   vlog_set_loc($$, &@$);
                   vlog_set_number($$, number_new($1));
                   free($1);
                }
        ;

real_number:    tUNSIGNED '.' tUNSIGNED
                {
                   $$ = vlog_new(V_NUMBER);
                   vlog_set_loc($$, &@$);
                   free($1);
                   free($3);
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

gate_instantiation:
                tPULLDOWN pulldown_strength pull_gate_instance ';'
                {
                   vlog_set_subkind($3, V_GATE_PULLDOWN);

                   for (node_list_t *it = $2; it; it = it->next)
                      vlog_add_param($3, it->value);
                   node_list_free($2);

                   $$ = NULL;
                   node_list_append(&$$, $3);
                }
        |       tPULLDOWN pull_gate_instance ';'
                {
                   vlog_set_subkind($2, V_GATE_PULLDOWN);

                   $$ = NULL;
                   node_list_append(&$$, $2);
                }
        |       tPULLUP pullup_strength pull_gate_instance ';'
                {
                   vlog_set_subkind($3, V_GATE_PULLUP);

                   for (node_list_t *it = $2; it; it = it->next)
                      vlog_add_param($3, it->value);
                   node_list_free($2);

                   $$ = NULL;
                   node_list_append(&$$, $3);
                }
        |       tPULLUP pull_gate_instance ';'
                {
                   vlog_set_subkind($2, V_GATE_PULLUP);

                   $$ = NULL;
                   node_list_append(&$$, $2);
                }
        ;

pull_gate_instance:
                identifier '(' lvalue ')'
                {
                   $$ = vlog_new(V_GATE_INST);
                   vlog_set_loc($$, &@$);
                   vlog_set_ident($$, $1);
                   vlog_set_target($$, $3);
                }
        |       '(' lvalue ')'
                {
                   $$ = vlog_new(V_GATE_INST);
                   vlog_set_loc($$, &@$);
                   vlog_set_target($$, $2);
                }
        ;

pulldown_strength:
                '(' strength0 ',' strength1 ')'
                {
                   $$ = NULL;
                   node_list_append(&$$, $2);
                   node_list_append(&$$, $4);
                }
        |       '(' strength1 ',' strength0 ')'
                {
                   $$ = NULL;
                   node_list_append(&$$, $2);
                   node_list_append(&$$, $4);
                }
        |       '(' strength0 ')'
                {
                   $$ = NULL;
                   node_list_append(&$$, $2);
                }
        ;

pullup_strength:
                '(' strength0 ',' strength1 ')'
                {
                   $$ = NULL;
                   node_list_append(&$$, $2);
                   node_list_append(&$$, $4);
                }
        |       '(' strength1 ',' strength0 ')'
                {
                   $$ = NULL;
                   node_list_append(&$$, $2);
                   node_list_append(&$$, $4);
                }
        |       '(' strength1 ')'
                {
                   $$ = NULL;
                   node_list_append(&$$, $2);
                }
        ;

strength0:      tSUPPLY0 { $$ = make_strength(V_STRENGTH_SUPPLY0, &@$); }
        ;

strength1:      tSUPPLY1 { $$ = make_strength(V_STRENGTH_SUPPLY1, &@$); }
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
         diag_printf(d, "invalid order of magniture in `timescale directive");
         diag_hint(d, NULL, "the valid values are 1, 10, and 100");
         diag_emit(d);
      }

      args[i].parsed *= scale;
   }

   // TODO: do something with parsed scale/precision
}

static void clone_port(node_list_t **list, vlog_node_t p)
{
   node_list_t *last = NULL;
   for (node_list_t *it = *list; it != NULL; it = it->next) {
      if (vlog_kind(it->value) == V_PORT_DECL)
         last = it;
   }

   vlog_set_subkind(p, vlog_subkind(last->value));

   node_list_append(list, p);

   if (last->next && vlog_kind(last->next->value) == V_VAR_DECL) {
      vlog_node_t reg = vlog_new(V_VAR_DECL);
      vlog_set_loc(reg, vlog_loc(p));
      vlog_set_ident(reg, vlog_ident(p));

      node_list_append(list, reg);
   }
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
